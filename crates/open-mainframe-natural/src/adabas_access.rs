// SPDX-License-Identifier: Apache-2.0
//! NAT-104: ADABAS Database Access for Natural.
//!
//! Provides DDM (Data Definition Module) mapping, and simulated in-memory
//! implementations of all ADABAS operations: READ, FIND, HISTOGRAM, GET,
//! STORE, UPDATE, DELETE, END TRANSACTION, and BACKOUT TRANSACTION.

use std::collections::HashMap;

use crate::data_model::NaturalValue;

// ---------------------------------------------------------------------------
// DDM — Data Definition Module
// ---------------------------------------------------------------------------

/// Map Natural field names to ADABAS short names and file numbers.
#[derive(Debug, Clone)]
pub struct Ddm {
    pub name: String,
    pub file_number: u16,
    pub fields: Vec<DdmField>,
}

/// A single field mapping in a DDM.
#[derive(Debug, Clone)]
pub struct DdmField {
    /// Natural (long) name
    pub natural_name: String,
    /// ADABAS short name (2 characters)
    pub short_name: String,
    /// ADABAS format/length (e.g., "A20", "P8")
    pub format: String,
    /// Is this a descriptor (searchable)?
    pub is_descriptor: bool,
    /// Level in the FDT hierarchy
    pub level: u8,
}

impl Ddm {
    pub fn new(name: &str, file_number: u16) -> Self {
        Self {
            name: name.to_string(),
            file_number,
            fields: Vec::new(),
        }
    }

    pub fn add_field(&mut self, natural_name: &str, short_name: &str, format: &str, is_descriptor: bool, level: u8) {
        self.fields.push(DdmField {
            natural_name: natural_name.to_string(),
            short_name: short_name.to_string(),
            format: format.to_string(),
            is_descriptor,
            level,
        });
    }

    pub fn get_field(&self, natural_name: &str) -> Option<&DdmField> {
        self.fields.iter().find(|f| f.natural_name == natural_name)
    }

    pub fn descriptors(&self) -> Vec<&DdmField> {
        self.fields.iter().filter(|f| f.is_descriptor).collect()
    }
}

// ---------------------------------------------------------------------------
// ADABAS record (in-memory)
// ---------------------------------------------------------------------------

/// An in-memory ADABAS record.
#[derive(Debug, Clone)]
pub struct AdabasRecord {
    pub isn: u64,
    pub fields: HashMap<String, NaturalValue>,
}

impl AdabasRecord {
    pub fn new(isn: u64) -> Self {
        Self { isn, fields: HashMap::new() }
    }

    pub fn set_field(&mut self, name: &str, value: NaturalValue) {
        self.fields.insert(name.to_string(), value);
    }

    pub fn get_field(&self, name: &str) -> NaturalValue {
        self.fields.get(name).cloned().unwrap_or(NaturalValue::Null)
    }
}

// ---------------------------------------------------------------------------
// In-memory ADABAS file
// ---------------------------------------------------------------------------

/// Simulated ADABAS file with in-memory record storage.
#[derive(Debug, Clone)]
pub struct AdabasFile {
    pub ddm: Ddm,
    records: Vec<AdabasRecord>,
    next_isn: u64,
    /// Current record cursor for UPDATE/DELETE
    current_isn: Option<u64>,
    /// Transaction buffer: records to be committed
    tx_buffer: Vec<AdabasRecord>,
    /// Records deleted in current transaction
    tx_deletes: Vec<u64>,
}

impl AdabasFile {
    pub fn new(ddm: Ddm) -> Self {
        Self {
            ddm,
            records: Vec::new(),
            next_isn: 1,
            current_isn: None,
            tx_buffer: Vec::new(),
            tx_deletes: Vec::new(),
        }
    }

    /// STORE: add a new record (N1 command).
    pub fn store(&mut self, fields: HashMap<String, NaturalValue>) -> Result<u64, AdabasError> {
        let isn = self.next_isn;
        self.next_isn += 1;
        let mut record = AdabasRecord::new(isn);
        record.fields = fields;
        self.tx_buffer.push(record);
        Ok(isn)
    }

    /// GET: random access by ISN.
    pub fn get(&mut self, isn: u64) -> Result<&AdabasRecord, AdabasError> {
        self.current_isn = Some(isn);
        self.records.iter()
            .find(|r| r.isn == isn)
            .ok_or(AdabasError::RecordNotFound(isn))
    }

    /// READ: sequential read by descriptor, optionally from a starting value.
    pub fn read(
        &self,
        descriptor: Option<&str>,
        starting_from: Option<&NaturalValue>,
        ending_at: Option<&NaturalValue>,
    ) -> Vec<&AdabasRecord> {
        let mut results: Vec<&AdabasRecord> = self.records.iter().collect();

        if let Some(desc) = descriptor {
            // Sort by descriptor
            results.sort_by(|a, b| {
                let av = a.get_field(desc).to_display_string();
                let bv = b.get_field(desc).to_display_string();
                av.cmp(&bv)
            });

            // Filter by starting/ending
            if let Some(start) = starting_from {
                let start_s = start.to_display_string();
                results.retain(|r| r.get_field(desc).to_display_string() >= start_s);
            }
            if let Some(end) = ending_at {
                let end_s = end.to_display_string();
                results.retain(|r| r.get_field(desc).to_display_string() <= end_s);
            }
        }

        results
    }

    /// FIND: search via descriptor (inverted list).
    pub fn find(&self, field: &str, value: &NaturalValue) -> Vec<&AdabasRecord> {
        let search_val = value.to_display_string();
        self.records.iter()
            .filter(|r| r.get_field(field).to_display_string() == search_val)
            .collect()
    }

    /// FIND with condition expression (simplified).
    pub fn find_with_condition<F>(&self, predicate: F) -> Vec<&AdabasRecord>
    where
        F: Fn(&AdabasRecord) -> bool,
    {
        self.records.iter().filter(|r| predicate(r)).collect()
    }

    /// HISTOGRAM: return unique values and their counts for a descriptor.
    pub fn histogram(&self, field: &str) -> Vec<(NaturalValue, usize)> {
        let mut counts: HashMap<String, (NaturalValue, usize)> = HashMap::new();
        for record in &self.records {
            let val = record.get_field(field);
            let key = val.to_display_string();
            let entry = counts.entry(key).or_insert((val, 0));
            entry.1 += 1;
        }
        let mut result: Vec<(NaturalValue, usize)> = counts.into_values().collect();
        result.sort_by(|a, b| a.0.to_display_string().cmp(&b.0.to_display_string()));
        result
    }

    /// UPDATE: modify the current record (A1 command).
    pub fn update(&mut self, fields: HashMap<String, NaturalValue>) -> Result<(), AdabasError> {
        let isn = self.current_isn.ok_or(AdabasError::NoCurrentRecord)?;
        let record = self.records.iter_mut()
            .find(|r| r.isn == isn)
            .ok_or(AdabasError::RecordNotFound(isn))?;
        for (name, value) in fields {
            record.set_field(&name, value);
        }
        Ok(())
    }

    /// DELETE: remove the current record (E1 command).
    pub fn delete(&mut self) -> Result<(), AdabasError> {
        let isn = self.current_isn.ok_or(AdabasError::NoCurrentRecord)?;
        self.tx_deletes.push(isn);
        self.current_isn = None;
        Ok(())
    }

    /// END TRANSACTION: commit buffered changes (ET command).
    pub fn end_transaction(&mut self) {
        // Apply stores
        for record in self.tx_buffer.drain(..) {
            self.records.push(record);
        }
        // Apply deletes
        for isn in self.tx_deletes.drain(..) {
            self.records.retain(|r| r.isn != isn);
        }
        self.current_isn = None;
    }

    /// BACKOUT TRANSACTION: discard buffered changes (BT command).
    pub fn backout_transaction(&mut self) {
        self.tx_buffer.clear();
        self.tx_deletes.clear();
        self.current_isn = None;
    }

    /// Set the current record (for UPDATE/DELETE context).
    pub fn set_current(&mut self, isn: u64) {
        self.current_isn = Some(isn);
    }

    /// Number of records (committed).
    pub fn record_count(&self) -> usize {
        self.records.len()
    }
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, thiserror::Error)]
pub enum AdabasError {
    #[error("record not found: ISN {0}")]
    RecordNotFound(u64),
    #[error("no current record (GET/READ required before UPDATE/DELETE)")]
    NoCurrentRecord,
    #[error("field not found in DDM: {0}")]
    FieldNotFound(String),
    #[error("duplicate descriptor value: {0}")]
    DuplicateDescriptor(String),
    #[error("file not open: {0}")]
    FileNotOpen(u16),
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_ddm() -> Ddm {
        let mut ddm = Ddm::new("EMPLOYEES", 1);
        ddm.add_field("NAME", "AA", "A20", true, 1);
        ddm.add_field("DEPT", "AB", "A3", true, 1);
        ddm.add_field("SALARY", "AC", "P8", false, 1);
        ddm.add_field("CITY", "AD", "A15", true, 1);
        ddm
    }

    fn sample_file() -> AdabasFile {
        let mut file = AdabasFile::new(sample_ddm());
        // Pre-commit some records directly
        file.records.push(AdabasRecord {
            isn: 1,
            fields: [
                ("NAME".into(), NaturalValue::Alpha("Smith".into())),
                ("DEPT".into(), NaturalValue::Alpha("D01".into())),
                ("SALARY".into(), NaturalValue::Packed("50000".into())),
                ("CITY".into(), NaturalValue::Alpha("New York".into())),
            ].into_iter().collect(),
        });
        file.records.push(AdabasRecord {
            isn: 2,
            fields: [
                ("NAME".into(), NaturalValue::Alpha("Jones".into())),
                ("DEPT".into(), NaturalValue::Alpha("D02".into())),
                ("SALARY".into(), NaturalValue::Packed("60000".into())),
                ("CITY".into(), NaturalValue::Alpha("London".into())),
            ].into_iter().collect(),
        });
        file.records.push(AdabasRecord {
            isn: 3,
            fields: [
                ("NAME".into(), NaturalValue::Alpha("Taylor".into())),
                ("DEPT".into(), NaturalValue::Alpha("D01".into())),
                ("SALARY".into(), NaturalValue::Packed("55000".into())),
                ("CITY".into(), NaturalValue::Alpha("New York".into())),
            ].into_iter().collect(),
        });
        file.next_isn = 4;
        file
    }

    #[test]
    fn test_ddm_creation() {
        let ddm = sample_ddm();
        assert_eq!(ddm.name, "EMPLOYEES");
        assert_eq!(ddm.file_number, 1);
        assert_eq!(ddm.fields.len(), 4);
    }

    #[test]
    fn test_ddm_descriptors() {
        let ddm = sample_ddm();
        let descs = ddm.descriptors();
        assert_eq!(descs.len(), 3); // NAME, DEPT, CITY
    }

    #[test]
    fn test_ddm_get_field() {
        let ddm = sample_ddm();
        let f = ddm.get_field("NAME").unwrap();
        assert_eq!(f.short_name, "AA");
        assert!(f.is_descriptor);
    }

    #[test]
    fn test_store_and_commit() {
        let mut file = AdabasFile::new(sample_ddm());
        let mut fields = HashMap::new();
        fields.insert("NAME".into(), NaturalValue::Alpha("Test".into()));
        fields.insert("DEPT".into(), NaturalValue::Alpha("D01".into()));
        let isn = file.store(fields).unwrap();
        assert_eq!(isn, 1);
        assert_eq!(file.record_count(), 0); // not yet committed

        file.end_transaction();
        assert_eq!(file.record_count(), 1);
    }

    #[test]
    fn test_store_and_backout() {
        let mut file = AdabasFile::new(sample_ddm());
        let mut fields = HashMap::new();
        fields.insert("NAME".into(), NaturalValue::Alpha("Test".into()));
        file.store(fields).unwrap();

        file.backout_transaction();
        assert_eq!(file.record_count(), 0);
    }

    #[test]
    fn test_get_by_isn() {
        let mut file = sample_file();
        let record = file.get(2).unwrap();
        assert_eq!(record.get_field("NAME").to_display_string(), "Jones");
    }

    #[test]
    fn test_get_not_found() {
        let mut file = sample_file();
        assert!(file.get(999).is_err());
    }

    #[test]
    fn test_read_all() {
        let file = sample_file();
        let results = file.read(None, None, None);
        assert_eq!(results.len(), 3);
    }

    #[test]
    fn test_read_by_descriptor() {
        let file = sample_file();
        let results = file.read(Some("NAME"), None, None);
        // Should be sorted by NAME
        assert_eq!(results[0].get_field("NAME").to_display_string(), "Jones");
        assert_eq!(results[1].get_field("NAME").to_display_string(), "Smith");
        assert_eq!(results[2].get_field("NAME").to_display_string(), "Taylor");
    }

    #[test]
    fn test_read_starting_from() {
        let file = sample_file();
        let results = file.read(
            Some("NAME"),
            Some(&NaturalValue::Alpha("Smith".into())),
            None,
        );
        assert_eq!(results.len(), 2); // Smith and Taylor
    }

    #[test]
    fn test_read_range() {
        let file = sample_file();
        let results = file.read(
            Some("NAME"),
            Some(&NaturalValue::Alpha("Jones".into())),
            Some(&NaturalValue::Alpha("Smith".into())),
        );
        assert_eq!(results.len(), 2); // Jones and Smith
    }

    #[test]
    fn test_find_by_descriptor() {
        let file = sample_file();
        let results = file.find("DEPT", &NaturalValue::Alpha("D01".into()));
        assert_eq!(results.len(), 2); // Smith and Taylor
    }

    #[test]
    fn test_find_with_condition() {
        let file = sample_file();
        let results = file.find_with_condition(|r| {
            r.get_field("SALARY").to_f64() > 55000.0
        });
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].get_field("NAME").to_display_string(), "Jones");
    }

    #[test]
    fn test_find_no_matches() {
        let file = sample_file();
        let results = file.find("DEPT", &NaturalValue::Alpha("D99".into()));
        assert!(results.is_empty());
    }

    #[test]
    fn test_histogram() {
        let file = sample_file();
        let hist = file.histogram("DEPT");
        assert_eq!(hist.len(), 2);
        // D01 has count 2, D02 has count 1
        let d01 = hist.iter().find(|(v, _)| v.to_display_string() == "D01").unwrap();
        assert_eq!(d01.1, 2);
        let d02 = hist.iter().find(|(v, _)| v.to_display_string() == "D02").unwrap();
        assert_eq!(d02.1, 1);
    }

    #[test]
    fn test_histogram_city() {
        let file = sample_file();
        let hist = file.histogram("CITY");
        let ny = hist.iter().find(|(v, _)| v.to_display_string() == "New York").unwrap();
        assert_eq!(ny.1, 2);
    }

    #[test]
    fn test_update_record() {
        let mut file = sample_file();
        file.get(1).unwrap(); // set current
        let mut updates = HashMap::new();
        updates.insert("SALARY".into(), NaturalValue::Packed("55000".into()));
        file.update(updates).unwrap();

        let record = file.get(1).unwrap();
        assert_eq!(record.get_field("SALARY").to_display_string(), "55000");
    }

    #[test]
    fn test_update_no_current() {
        let mut file = sample_file();
        let updates = HashMap::new();
        assert!(file.update(updates).is_err());
    }

    #[test]
    fn test_delete_record() {
        let mut file = sample_file();
        file.get(2).unwrap(); // set current
        file.delete().unwrap();
        file.end_transaction();
        assert_eq!(file.record_count(), 2);
    }

    #[test]
    fn test_delete_no_current() {
        let mut file = sample_file();
        assert!(file.delete().is_err());
    }

    #[test]
    fn test_end_transaction() {
        let mut file = sample_file();
        let mut fields = HashMap::new();
        fields.insert("NAME".into(), NaturalValue::Alpha("NewPerson".into()));
        file.store(fields).unwrap();

        file.get(1).unwrap();
        file.delete().unwrap();

        file.end_transaction();
        assert_eq!(file.record_count(), 3); // 3 - 1 + 1 = 3
    }

    #[test]
    fn test_backout_transaction() {
        let mut file = sample_file();
        let mut fields = HashMap::new();
        fields.insert("NAME".into(), NaturalValue::Alpha("NewPerson".into()));
        file.store(fields).unwrap();

        file.get(1).unwrap();
        file.delete().unwrap();

        file.backout_transaction();
        assert_eq!(file.record_count(), 3); // unchanged
    }

    #[test]
    fn test_record_field_access() {
        let mut record = AdabasRecord::new(1);
        record.set_field("NAME", NaturalValue::Alpha("Test".into()));
        assert_eq!(record.get_field("NAME").to_display_string(), "Test");
        assert_eq!(record.get_field("MISSING"), NaturalValue::Null);
    }

    #[test]
    fn test_multiple_stores() {
        let mut file = AdabasFile::new(sample_ddm());
        for i in 0..5 {
            let mut fields = HashMap::new();
            fields.insert("NAME".into(), NaturalValue::Alpha(format!("Person{i}")));
            file.store(fields).unwrap();
        }
        file.end_transaction();
        assert_eq!(file.record_count(), 5);
    }

    #[test]
    fn test_set_current() {
        let mut file = sample_file();
        file.set_current(2);
        let mut updates = HashMap::new();
        updates.insert("NAME".into(), NaturalValue::Alpha("Updated".into()));
        file.update(updates).unwrap();
        let rec = file.get(2).unwrap();
        assert_eq!(rec.get_field("NAME").to_display_string(), "Updated");
    }
}
