//! ADA-106: Modification Commands (4 stories).
//!
//! Provides store (N1/N2), update (A1), and delete (E1) commands, plus
//! inverted-list maintenance on modification.

use std::collections::HashMap;

use crate::descriptor::DescriptorSet;
use crate::storage::{AdabasFile, InvertedList, Isn};
use crate::AdabasError;

// ── StoreCommand ───────────────────────────────────────────────────

/// Store (N1/N2) command: add a new record to an ADABAS file.
#[derive(Debug, Clone)]
pub struct StoreCommand {
    /// File number to store into.
    pub file_number: u16,
    /// User-supplied ISN for N2 (None for N1 = system-assigned).
    pub user_isn: Option<Isn>,
    /// The record data to store.
    pub record_data: Vec<u8>,
    /// Field values for descriptor index maintenance.
    pub field_values: HashMap<String, Vec<u8>>,
}

impl StoreCommand {
    /// Create an N1 command (system-assigned ISN).
    pub fn n1(file_number: u16, record_data: Vec<u8>) -> Self {
        Self {
            file_number,
            user_isn: None,
            record_data,
            field_values: HashMap::new(),
        }
    }

    /// Create an N2 command (user-supplied ISN).
    pub fn n2(file_number: u16, isn: Isn, record_data: Vec<u8>) -> Self {
        Self {
            file_number,
            user_isn: Some(isn),
            record_data,
            field_values: HashMap::new(),
        }
    }

    /// Set field values for descriptor maintenance.
    pub fn with_field_values(mut self, values: HashMap<String, Vec<u8>>) -> Self {
        self.field_values = values;
        self
    }

    /// Execute the store command, returning the assigned ISN.
    pub fn execute(
        &self,
        file: &mut AdabasFile,
        descriptors: &DescriptorSet,
    ) -> Result<Isn, AdabasError> {
        let isn = match self.user_isn {
            Some(user_isn) => user_isn,
            None => file.allocate_isn(),
        };

        file.store_record(isn, self.record_data.clone())?;

        // Update inverted lists for descriptors.
        let field_refs: Vec<(&str, &[u8])> = self
            .field_values
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_slice()))
            .collect();
        descriptors.update_inverted_lists(
            isn,
            &field_refs,
            &mut file.associator.inverted_lists,
        );

        Ok(isn)
    }
}

// ── UpdateCommand ──────────────────────────────────────────────────

/// Update (A1) command: modify an existing record by ISN.
#[derive(Debug, Clone)]
pub struct UpdateCommand {
    /// File number.
    pub file_number: u16,
    /// ISN of the record to update.
    pub isn: Isn,
    /// New record data.
    pub record_data: Vec<u8>,
    /// Old field values (for removing old index entries).
    pub old_field_values: HashMap<String, Vec<u8>>,
    /// New field values (for adding new index entries).
    pub new_field_values: HashMap<String, Vec<u8>>,
}

impl UpdateCommand {
    /// Create an A1 command.
    pub fn new(file_number: u16, isn: Isn, record_data: Vec<u8>) -> Self {
        Self {
            file_number,
            isn,
            record_data,
            old_field_values: HashMap::new(),
            new_field_values: HashMap::new(),
        }
    }

    /// Set old field values (to remove from index).
    pub fn with_old_field_values(mut self, values: HashMap<String, Vec<u8>>) -> Self {
        self.old_field_values = values;
        self
    }

    /// Set new field values (to add to index).
    pub fn with_new_field_values(mut self, values: HashMap<String, Vec<u8>>) -> Self {
        self.new_field_values = values;
        self
    }

    /// Execute the update command.
    pub fn execute(
        &self,
        file: &mut AdabasFile,
        descriptors: &DescriptorSet,
    ) -> Result<(), AdabasError> {
        // Remove old index entries.
        let old_refs: Vec<(&str, &[u8])> = self
            .old_field_values
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_slice()))
            .collect();
        descriptors.remove_from_inverted_lists(
            self.isn,
            &old_refs,
            &mut file.associator.inverted_lists,
        );

        // Update the data record.
        file.update_record(self.isn, self.record_data.clone())?;

        // Add new index entries.
        let new_refs: Vec<(&str, &[u8])> = self
            .new_field_values
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_slice()))
            .collect();
        descriptors.update_inverted_lists(
            self.isn,
            &new_refs,
            &mut file.associator.inverted_lists,
        );

        Ok(())
    }
}

// ── DeleteCommand ──────────────────────────────────────────────────

/// Delete (E1) command: remove a record by ISN.
#[derive(Debug, Clone)]
pub struct DeleteCommand {
    /// File number.
    pub file_number: u16,
    /// ISN of the record to delete.
    pub isn: Isn,
    /// Field values of the record being deleted (for index cleanup).
    pub field_values: HashMap<String, Vec<u8>>,
}

impl DeleteCommand {
    /// Create an E1 command.
    pub fn new(file_number: u16, isn: Isn) -> Self {
        Self {
            file_number,
            isn,
            field_values: HashMap::new(),
        }
    }

    /// Set field values for descriptor cleanup.
    pub fn with_field_values(mut self, values: HashMap<String, Vec<u8>>) -> Self {
        self.field_values = values;
        self
    }

    /// Execute the delete command.
    pub fn execute(
        &self,
        file: &mut AdabasFile,
        descriptors: &DescriptorSet,
    ) -> Result<Vec<u8>, AdabasError> {
        // Remove index entries.
        let field_refs: Vec<(&str, &[u8])> = self
            .field_values
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_slice()))
            .collect();
        descriptors.remove_from_inverted_lists(
            self.isn,
            &field_refs,
            &mut file.associator.inverted_lists,
        );

        // Delete the physical record.
        file.delete_record(self.isn)
    }
}

// ── UpdateDescriptors ──────────────────────────────────────────────

/// Utility for maintaining inverted lists when records are modified.
#[derive(Debug)]
pub struct UpdateDescriptors;

impl UpdateDescriptors {
    /// Add descriptor entries for a stored record.
    pub fn on_store(
        isn: Isn,
        field_values: &[(&str, &[u8])],
        descriptors: &DescriptorSet,
        inverted_lists: &mut HashMap<String, InvertedList>,
    ) {
        descriptors.update_inverted_lists(isn, field_values, inverted_lists);
    }

    /// Remove descriptor entries for a deleted record.
    pub fn on_delete(
        isn: Isn,
        field_values: &[(&str, &[u8])],
        descriptors: &DescriptorSet,
        inverted_lists: &mut HashMap<String, InvertedList>,
    ) {
        descriptors.remove_from_inverted_lists(isn, field_values, inverted_lists);
    }

    /// Update descriptor entries: remove old, add new.
    pub fn on_update(
        isn: Isn,
        old_values: &[(&str, &[u8])],
        new_values: &[(&str, &[u8])],
        descriptors: &DescriptorSet,
        inverted_lists: &mut HashMap<String, InvertedList>,
    ) {
        descriptors.remove_from_inverted_lists(isn, old_values, inverted_lists);
        descriptors.update_inverted_lists(isn, new_values, inverted_lists);
    }
}

// ── Tests ──────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::descriptor::Descriptor;

    #[test]
    fn store_command_n1() {
        let mut file = AdabasFile::new(1, "TEST");
        let descriptors = DescriptorSet::new();
        let cmd = StoreCommand::n1(1, b"record1".to_vec());
        let isn = cmd.execute(&mut file, &descriptors).unwrap();
        assert_eq!(isn, 1);
        assert_eq!(file.read_record(isn).unwrap(), b"record1");
    }

    #[test]
    fn store_command_n2() {
        let mut file = AdabasFile::new(1, "TEST");
        let descriptors = DescriptorSet::new();
        let cmd = StoreCommand::n2(1, 100, b"record100".to_vec());
        let isn = cmd.execute(&mut file, &descriptors).unwrap();
        assert_eq!(isn, 100);
        assert_eq!(file.read_record(100).unwrap(), b"record100");
    }

    #[test]
    fn store_with_descriptor_updates_index() {
        let mut file = AdabasFile::new(1, "TEST");
        let mut descriptors = DescriptorSet::new();
        descriptors.descriptors.push(Descriptor::new("AA"));

        let mut values = HashMap::new();
        values.insert("AA".to_string(), b"SMITH".to_vec());
        let cmd = StoreCommand::n1(1, b"rec".to_vec()).with_field_values(values);
        let isn = cmd.execute(&mut file, &descriptors).unwrap();

        let search_result = file.associator.search("AA", "SMITH");
        assert_eq!(search_result, vec![isn]);
    }

    #[test]
    fn update_command() {
        let mut file = AdabasFile::new(1, "TEST");
        let descriptors = DescriptorSet::new();
        let store = StoreCommand::n1(1, b"old".to_vec());
        let isn = store.execute(&mut file, &descriptors).unwrap();

        let update = UpdateCommand::new(1, isn, b"new".to_vec());
        update.execute(&mut file, &descriptors).unwrap();
        assert_eq!(file.read_record(isn).unwrap(), b"new");
    }

    #[test]
    fn update_command_with_index() {
        let mut file = AdabasFile::new(1, "TEST");
        let mut descriptors = DescriptorSet::new();
        descriptors.descriptors.push(Descriptor::new("AA"));

        let mut old_values = HashMap::new();
        old_values.insert("AA".to_string(), b"SMITH".to_vec());
        let store = StoreCommand::n1(1, b"rec".to_vec())
            .with_field_values(old_values.clone());
        let isn = store.execute(&mut file, &descriptors).unwrap();

        let mut new_values = HashMap::new();
        new_values.insert("AA".to_string(), b"JONES".to_vec());
        let update = UpdateCommand::new(1, isn, b"rec_updated".to_vec())
            .with_old_field_values(old_values)
            .with_new_field_values(new_values);
        update.execute(&mut file, &descriptors).unwrap();

        assert!(file.associator.search("AA", "SMITH").is_empty());
        assert_eq!(file.associator.search("AA", "JONES"), vec![isn]);
    }

    #[test]
    fn delete_command() {
        let mut file = AdabasFile::new(1, "TEST");
        let descriptors = DescriptorSet::new();
        let store = StoreCommand::n1(1, b"doomed".to_vec());
        let isn = store.execute(&mut file, &descriptors).unwrap();

        let delete = DeleteCommand::new(1, isn);
        let removed = delete.execute(&mut file, &descriptors).unwrap();
        assert_eq!(removed, b"doomed");
        assert!(file.read_record(isn).is_err());
    }

    #[test]
    fn delete_nonexistent() {
        let mut file = AdabasFile::new(1, "TEST");
        let descriptors = DescriptorSet::new();
        let delete = DeleteCommand::new(1, 999);
        assert!(delete.execute(&mut file, &descriptors).is_err());
    }

    #[test]
    fn update_descriptors_on_store() {
        let mut descriptors = DescriptorSet::new();
        descriptors.descriptors.push(Descriptor::new("AA"));
        let mut lists = HashMap::new();
        let values: Vec<(&str, &[u8])> = vec![("AA", b"TEST")];
        UpdateDescriptors::on_store(1, &values, &descriptors, &mut lists);
        assert_eq!(
            lists.get("AA").unwrap().search("TEST"),
            vec![1]
        );
    }

    #[test]
    fn update_descriptors_on_update() {
        let mut descriptors = DescriptorSet::new();
        descriptors.descriptors.push(Descriptor::new("AA"));
        let mut lists = HashMap::new();
        let old: Vec<(&str, &[u8])> = vec![("AA", b"OLD")];
        UpdateDescriptors::on_store(1, &old, &descriptors, &mut lists);

        let new: Vec<(&str, &[u8])> = vec![("AA", b"NEW")];
        UpdateDescriptors::on_update(1, &old, &new, &descriptors, &mut lists);
        assert!(lists.get("AA").unwrap().search("OLD").is_empty());
        assert_eq!(lists.get("AA").unwrap().search("NEW"), vec![1]);
    }
}
