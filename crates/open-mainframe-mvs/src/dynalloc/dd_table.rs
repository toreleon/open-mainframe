//! DD Table — maps DDnames to dataset references within an address space.

use std::collections::HashMap;

use super::types::{DatasetStatus, DdEntry, Disposition};
use crate::error::{MvsError, Result};

/// DD table managing DDname-to-dataset mappings for an address space.
#[derive(Debug, Default)]
pub struct DdTable {
    entries: HashMap<String, DdEntry>,
    next_dd_num: u32,
}

impl DdTable {
    /// Create an empty DD table.
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            next_dd_num: 1,
        }
    }

    /// Allocate a DDname mapping to a dataset.
    ///
    /// If `ddname` is `None`, a system-generated name (SYSnnnnn) is assigned.
    /// Returns the actual DDname used.
    pub fn allocate(
        &mut self,
        ddname: Option<&str>,
        dsname: &str,
        status: DatasetStatus,
        disposition: Disposition,
    ) -> Result<String> {
        let actual_ddname = match ddname {
            Some(name) => {
                validate_ddname(name)?;
                name.to_uppercase()
            }
            None => self.generate_ddname(),
        };

        validate_dsname(dsname)?;

        let entry = DdEntry {
            ddname: actual_ddname.clone(),
            dsname: dsname.to_uppercase(),
            status,
            disposition,
            concatenation: Vec::new(),
        };

        self.entries.insert(actual_ddname.clone(), entry);
        Ok(actual_ddname)
    }

    /// Unallocate (free) a DDname.
    pub fn unallocate(&mut self, ddname: &str) -> Result<()> {
        let upper = ddname.to_uppercase();
        if self.entries.remove(&upper).is_some() {
            Ok(())
        } else {
            Err(MvsError::DdnameNotFound {
                name: upper,
            })
        }
    }

    /// Concatenate multiple datasets under one DDname.
    ///
    /// The first dataset becomes the primary; the rest are concatenated.
    pub fn concatenate(
        &mut self,
        ddname: Option<&str>,
        dsnames: &[&str],
        status: DatasetStatus,
        disposition: Disposition,
    ) -> Result<String> {
        if dsnames.is_empty() {
            return Err(MvsError::DynallocFailed {
                verb: 0x03,
                error_code: 0x0438,
                info_code: 0x0001,
            });
        }

        for dsn in dsnames {
            validate_dsname(dsn)?;
        }

        let actual_ddname = match ddname {
            Some(name) => {
                validate_ddname(name)?;
                name.to_uppercase()
            }
            None => self.generate_ddname(),
        };

        let primary = dsnames[0].to_uppercase();
        let concat: Vec<String> = dsnames[1..].iter().map(|s| s.to_uppercase()).collect();

        let entry = DdEntry {
            ddname: actual_ddname.clone(),
            dsname: primary,
            status,
            disposition,
            concatenation: concat,
        };

        self.entries.insert(actual_ddname.clone(), entry);
        Ok(actual_ddname)
    }

    /// Deconcatenate a DDname, breaking it into individual DD entries.
    ///
    /// The primary keeps the original DDname; concatenated datasets get generated names.
    /// Returns the list of DDnames created.
    pub fn deconcatenate(&mut self, ddname: &str) -> Result<Vec<String>> {
        let upper = ddname.to_uppercase();
        let entry = self
            .entries
            .remove(&upper)
            .ok_or_else(|| MvsError::DdnameNotFound { name: upper.clone() })?;

        let mut result_ddnames = vec![upper.clone()];

        // Re-insert primary without concatenation
        let primary = DdEntry {
            ddname: upper.clone(),
            dsname: entry.dsname,
            status: entry.status,
            disposition: entry.disposition,
            concatenation: Vec::new(),
        };
        self.entries.insert(upper, primary);

        // Create individual entries for each concatenated dataset
        for dsname in entry.concatenation {
            let new_ddname = self.generate_ddname();
            let new_entry = DdEntry {
                ddname: new_ddname.clone(),
                dsname,
                status: entry.status,
                disposition: entry.disposition,
                concatenation: Vec::new(),
            };
            self.entries.insert(new_ddname.clone(), new_entry);
            result_ddnames.push(new_ddname);
        }

        Ok(result_ddnames)
    }

    /// Look up a DD entry by DDname.
    pub fn lookup(&self, ddname: &str) -> Option<&DdEntry> {
        self.entries.get(&ddname.to_uppercase())
    }

    /// List all current DD allocations.
    pub fn list(&self) -> Vec<&DdEntry> {
        let mut entries: Vec<&DdEntry> = self.entries.values().collect();
        entries.sort_by(|a, b| a.ddname.cmp(&b.ddname));
        entries
    }

    /// Return the number of active DD entries.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Check if the DD table is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Generate a system DDname (SYSnnnnn format).
    fn generate_ddname(&mut self) -> String {
        let name = format!("SYS{:05}", self.next_dd_num);
        self.next_dd_num += 1;
        name
    }
}

/// Validate a DDname (1-8 alphanumeric characters, starting with letter or national).
fn validate_ddname(name: &str) -> Result<()> {
    if name.is_empty() || name.len() > 8 {
        return Err(MvsError::InvalidDdname {
            name: name.to_string(),
            reason: "must be 1-8 characters".to_string(),
        });
    }
    let first = name.chars().next().unwrap();
    if !first.is_ascii_alphabetic() && first != '@' && first != '#' && first != '$' {
        return Err(MvsError::InvalidDdname {
            name: name.to_string(),
            reason: "must start with a letter or national character (@#$)".to_string(),
        });
    }
    for ch in name.chars() {
        if !ch.is_ascii_alphanumeric() && ch != '@' && ch != '#' && ch != '$' {
            return Err(MvsError::InvalidDdname {
                name: name.to_string(),
                reason: format!("invalid character '{ch}'"),
            });
        }
    }
    Ok(())
}

/// Validate a dataset name (basic validation: 1-44 characters, valid qualifiers).
fn validate_dsname(name: &str) -> Result<()> {
    if name.is_empty() || name.len() > 44 {
        return Err(MvsError::InvalidDsname {
            name: name.to_string(),
            reason: "must be 1-44 characters".to_string(),
        });
    }
    for qualifier in name.split('.') {
        if qualifier.is_empty() || qualifier.len() > 8 {
            return Err(MvsError::InvalidDsname {
                name: name.to_string(),
                reason: format!("qualifier '{}' must be 1-8 characters", qualifier),
            });
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn allocate_with_explicit_ddname() {
        let mut table = DdTable::new();
        let ddname = table
            .allocate(Some("SYSUT1"), "MY.DATASET", DatasetStatus::Shr, Disposition::Keep)
            .unwrap();
        assert_eq!(ddname, "SYSUT1");
        let entry = table.lookup("SYSUT1").unwrap();
        assert_eq!(entry.dsname, "MY.DATASET");
        assert_eq!(entry.status, DatasetStatus::Shr);
    }

    #[test]
    fn allocate_with_generated_ddname() {
        let mut table = DdTable::new();
        let ddname = table
            .allocate(None, "MY.DATASET", DatasetStatus::Old, Disposition::Keep)
            .unwrap();
        assert!(ddname.starts_with("SYS"));
        assert!(table.lookup(&ddname).is_some());
    }

    #[test]
    fn unallocate_existing() {
        let mut table = DdTable::new();
        table
            .allocate(Some("DD01"), "MY.DATA", DatasetStatus::Shr, Disposition::Keep)
            .unwrap();
        assert!(table.unallocate("DD01").is_ok());
        assert!(table.lookup("DD01").is_none());
    }

    #[test]
    fn unallocate_nonexistent_returns_error() {
        let mut table = DdTable::new();
        let err = table.unallocate("BOGUS").unwrap_err();
        assert!(matches!(err, MvsError::DdnameNotFound { .. }));
    }

    #[test]
    fn concatenate_multiple_datasets() {
        let mut table = DdTable::new();
        let ddname = table
            .concatenate(
                Some("STEPLIB"),
                &["MY.LOADLIB", "SYS1.LINKLIB", "SYS1.LPALIB"],
                DatasetStatus::Shr,
                Disposition::Keep,
            )
            .unwrap();
        assert_eq!(ddname, "STEPLIB");
        let entry = table.lookup("STEPLIB").unwrap();
        assert_eq!(entry.dsname, "MY.LOADLIB");
        assert_eq!(entry.concatenation.len(), 2);
        assert_eq!(entry.concatenation[0], "SYS1.LINKLIB");
        assert_eq!(entry.concatenation[1], "SYS1.LPALIB");
    }

    #[test]
    fn deconcatenate_breaks_into_individual_entries() {
        let mut table = DdTable::new();
        table
            .concatenate(
                Some("SYSIN"),
                &["DS.A", "DS.B", "DS.C"],
                DatasetStatus::Shr,
                Disposition::Keep,
            )
            .unwrap();
        let ddnames = table.deconcatenate("SYSIN").unwrap();
        assert_eq!(ddnames.len(), 3);
        assert_eq!(ddnames[0], "SYSIN");

        // Primary should have no concatenation now
        let primary = table.lookup("SYSIN").unwrap();
        assert!(primary.concatenation.is_empty());
        assert_eq!(primary.dsname, "DS.A");

        // Each concat should be its own entry
        let second = table.lookup(&ddnames[1]).unwrap();
        assert_eq!(second.dsname, "DS.B");
    }

    #[test]
    fn list_returns_all_entries_sorted() {
        let mut table = DdTable::new();
        table
            .allocate(Some("ZZZ"), "DS.Z", DatasetStatus::Shr, Disposition::Keep)
            .unwrap();
        table
            .allocate(Some("AAA"), "DS.A", DatasetStatus::Shr, Disposition::Keep)
            .unwrap();
        let list = table.list();
        assert_eq!(list.len(), 2);
        assert_eq!(list[0].ddname, "AAA");
        assert_eq!(list[1].ddname, "ZZZ");
    }

    #[test]
    fn concurrent_dd_table_access() {
        use std::sync::Arc;
        use tokio::sync::RwLock;

        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            let table = Arc::new(RwLock::new(DdTable::new()));
            let t1 = table.clone();
            let t2 = table.clone();

            let h1 = tokio::spawn(async move {
                let mut t = t1.write().await;
                t.allocate(Some("DD01"), "DS.ONE", DatasetStatus::Shr, Disposition::Keep)
                    .unwrap();
            });
            let h2 = tokio::spawn(async move {
                let mut t = t2.write().await;
                t.allocate(Some("DD02"), "DS.TWO", DatasetStatus::Shr, Disposition::Keep)
                    .unwrap();
            });

            h1.await.unwrap();
            h2.await.unwrap();

            let t = table.read().await;
            assert_eq!(t.len(), 2);
        });
    }

    #[test]
    fn invalid_ddname_too_long() {
        let mut table = DdTable::new();
        let err = table
            .allocate(Some("TOOLONGDD"), "MY.DATA", DatasetStatus::Shr, Disposition::Keep)
            .unwrap_err();
        assert!(matches!(err, MvsError::InvalidDdname { .. }));
    }

    #[test]
    fn invalid_dsname_too_long() {
        let mut table = DdTable::new();
        let long_name = "A".repeat(45);
        let err = table
            .allocate(Some("DD01"), &long_name, DatasetStatus::Shr, Disposition::Keep)
            .unwrap_err();
        assert!(matches!(err, MvsError::InvalidDsname { .. }));
    }

    #[test]
    fn case_insensitive_lookup() {
        let mut table = DdTable::new();
        table
            .allocate(Some("sysut1"), "my.data", DatasetStatus::Shr, Disposition::Keep)
            .unwrap();
        assert!(table.lookup("SYSUT1").is_some());
        assert!(table.lookup("sysut1").is_some());
    }
}
