//! FOC-110: Mainframe Integration (4 stories).
//!
//! FILEDEF (logical-to-physical file mapping), DYNAM ALLOCATE (dynamic
//! dataset allocation), and TSO/CICS environment interfaces.

use std::collections::HashMap;
use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum FileDefError {
    #[error("FILEDEF not found: {0}")]
    NotFound(String),
    #[error("duplicate FILEDEF: {0}")]
    Duplicate(String),
    #[error("DYNAM allocation failed: {0}")]
    DynamError(String),
    #[error("environment error: {0}")]
    EnvironmentError(String),
}

// ---------------------------------------------------------------------------
// FILEDEF
// ---------------------------------------------------------------------------

/// A FILEDEF entry mapping a logical filename to a physical path.
#[derive(Debug, Clone, PartialEq)]
pub struct FileDefEntry {
    pub logical_name: String,
    pub physical_path: String,
    pub disposition: FileDisposition,
    pub record_format: RecordFormat,
    pub record_length: usize,
    pub block_size: usize,
    pub properties: HashMap<String, String>,
}

/// File disposition (like JCL DISP).
#[derive(Debug, Clone, PartialEq)]
pub enum FileDisposition {
    Old,
    New,
    Shr,
    Mod,
}

/// Record format.
#[derive(Debug, Clone, PartialEq)]
pub enum RecordFormat {
    Fixed,
    Variable,
    Undefined,
}

impl FileDefEntry {
    pub fn new(logical: &str, physical: &str) -> Self {
        Self {
            logical_name: logical.to_string(),
            physical_path: physical.to_string(),
            disposition: FileDisposition::Old,
            record_format: RecordFormat::Fixed,
            record_length: 80,
            block_size: 0,
            properties: HashMap::new(),
        }
    }

    pub fn with_disposition(mut self, disp: FileDisposition) -> Self {
        self.disposition = disp;
        self
    }

    pub fn with_record_format(mut self, fmt: RecordFormat) -> Self {
        self.record_format = fmt;
        self
    }

    pub fn with_record_length(mut self, len: usize) -> Self {
        self.record_length = len;
        self
    }

    pub fn with_block_size(mut self, size: usize) -> Self {
        self.block_size = size;
        self
    }
}

// ---------------------------------------------------------------------------
// FileDefRegistry
// ---------------------------------------------------------------------------

/// Registry managing active FILEDEF mappings.
pub struct FileDefRegistry {
    entries: HashMap<String, FileDefEntry>,
}

impl FileDefRegistry {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    /// Register a new FILEDEF.
    pub fn define(&mut self, entry: FileDefEntry) -> Result<(), FileDefError> {
        let name = entry.logical_name.to_uppercase();
        if self.entries.contains_key(&name) {
            return Err(FileDefError::Duplicate(name));
        }
        self.entries.insert(name, entry);
        Ok(())
    }

    /// Look up a FILEDEF by logical name.
    pub fn lookup(&self, logical_name: &str) -> Option<&FileDefEntry> {
        self.entries.get(&logical_name.to_uppercase())
    }

    /// Remove a FILEDEF.
    pub fn undefine(&mut self, logical_name: &str) -> Result<(), FileDefError> {
        let name = logical_name.to_uppercase();
        if self.entries.remove(&name).is_none() {
            return Err(FileDefError::NotFound(name));
        }
        Ok(())
    }

    /// Replace (overwrite) a FILEDEF.
    pub fn redefine(&mut self, entry: FileDefEntry) {
        let name = entry.logical_name.to_uppercase();
        self.entries.insert(name, entry);
    }

    /// List all active FILEDEFs.
    pub fn list(&self) -> Vec<&FileDefEntry> {
        self.entries.values().collect()
    }

    /// Number of active FILEDEFs.
    pub fn count(&self) -> usize {
        self.entries.len()
    }

    /// Clear all FILEDEFs.
    pub fn clear(&mut self) {
        self.entries.clear();
    }
}

impl Default for FileDefRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// DYNAM Allocation
// ---------------------------------------------------------------------------

/// Simulated dynamic dataset allocation.
#[derive(Debug, Clone)]
pub struct DynamAllocation {
    pub ddname: String,
    pub dsname: String,
    pub disposition: FileDisposition,
    pub space_primary: usize,
    pub space_secondary: usize,
    pub unit: String,
    pub allocated: bool,
}

impl DynamAllocation {
    pub fn new(ddname: &str, dsname: &str) -> Self {
        Self {
            ddname: ddname.to_string(),
            dsname: dsname.to_string(),
            disposition: FileDisposition::New,
            space_primary: 10,
            space_secondary: 5,
            unit: "SYSDA".to_string(),
            allocated: false,
        }
    }

    /// Simulate allocation.
    pub fn allocate(&mut self) -> Result<(), FileDefError> {
        if self.dsname.is_empty() {
            return Err(FileDefError::DynamError("empty DSNAME".to_string()));
        }
        self.allocated = true;
        Ok(())
    }

    /// Simulate deallocation.
    pub fn deallocate(&mut self) -> Result<(), FileDefError> {
        if !self.allocated {
            return Err(FileDefError::DynamError(format!(
                "{} not allocated",
                self.ddname
            )));
        }
        self.allocated = false;
        Ok(())
    }

    pub fn is_allocated(&self) -> bool {
        self.allocated
    }
}

// ---------------------------------------------------------------------------
// TSO Interface
// ---------------------------------------------------------------------------

/// TSO environment detection and basic operations.
#[derive(Debug, Clone)]
pub struct TsoInterface {
    pub userid: String,
    pub prefix: String,
    pub active: bool,
    pub region_size: usize,
}

impl TsoInterface {
    pub fn new() -> Self {
        Self {
            userid: "SYSUSER".to_string(),
            prefix: "SYSUSER".to_string(),
            active: false,
            region_size: 4096,
        }
    }

    /// Detect if running under TSO.
    pub fn detect(&mut self) -> bool {
        // Simulation: always return true for testing
        self.active = true;
        self.active
    }

    /// Get current TSO userid.
    pub fn get_userid(&self) -> &str {
        &self.userid
    }

    /// Get current DSN prefix.
    pub fn get_prefix(&self) -> &str {
        &self.prefix
    }

    /// Check if TSO is active.
    pub fn is_active(&self) -> bool {
        self.active
    }

    /// Simulate a TSO command.
    pub fn execute_command(&self, _command: &str) -> Result<String, FileDefError> {
        if !self.active {
            return Err(FileDefError::EnvironmentError(
                "TSO not active".to_string(),
            ));
        }
        Ok("OK".to_string())
    }
}

impl Default for TsoInterface {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// CICS Interface
// ---------------------------------------------------------------------------

/// CICS environment detection and basic operations.
#[derive(Debug, Clone)]
pub struct CicsInterface {
    pub applid: String,
    pub sysid: String,
    pub active: bool,
    pub transaction_id: Option<String>,
}

impl CicsInterface {
    pub fn new() -> Self {
        Self {
            applid: "CICSAPP1".to_string(),
            sysid: "CI01".to_string(),
            active: false,
            transaction_id: None,
        }
    }

    /// Detect if running under CICS.
    pub fn detect(&mut self) -> bool {
        self.active = true;
        self.active
    }

    /// Get CICS application ID.
    pub fn get_applid(&self) -> &str {
        &self.applid
    }

    /// Get CICS system ID.
    pub fn get_sysid(&self) -> &str {
        &self.sysid
    }

    /// Check if CICS is active.
    pub fn is_active(&self) -> bool {
        self.active
    }

    /// Set current transaction ID.
    pub fn set_transaction_id(&mut self, txid: &str) {
        self.transaction_id = Some(txid.to_string());
    }

    /// Get current transaction ID.
    pub fn get_transaction_id(&self) -> Option<&str> {
        self.transaction_id.as_deref()
    }

    /// Simulate a CICS command.
    pub fn execute_command(&self, _command: &str) -> Result<String, FileDefError> {
        if !self.active {
            return Err(FileDefError::EnvironmentError(
                "CICS not active".to_string(),
            ));
        }
        Ok("OK".to_string())
    }
}

impl Default for CicsInterface {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // --- FILEDEF tests ---

    #[test]
    fn test_filedef_entry_new() {
        let entry = FileDefEntry::new("INFILE", "/data/input.dat");
        assert_eq!(entry.logical_name, "INFILE");
        assert_eq!(entry.physical_path, "/data/input.dat");
        assert_eq!(entry.disposition, FileDisposition::Old);
        assert_eq!(entry.record_format, RecordFormat::Fixed);
        assert_eq!(entry.record_length, 80);
    }

    #[test]
    fn test_filedef_builder() {
        let entry = FileDefEntry::new("OUTFILE", "/data/out.dat")
            .with_disposition(FileDisposition::New)
            .with_record_format(RecordFormat::Variable)
            .with_record_length(132)
            .with_block_size(27998);
        assert_eq!(entry.disposition, FileDisposition::New);
        assert_eq!(entry.record_format, RecordFormat::Variable);
        assert_eq!(entry.record_length, 132);
        assert_eq!(entry.block_size, 27998);
    }

    #[test]
    fn test_registry_define_lookup() {
        let mut reg = FileDefRegistry::new();
        reg.define(FileDefEntry::new("INPUT", "/data/in.dat")).unwrap();
        let entry = reg.lookup("INPUT").unwrap();
        assert_eq!(entry.physical_path, "/data/in.dat");
    }

    #[test]
    fn test_registry_case_insensitive() {
        let mut reg = FileDefRegistry::new();
        reg.define(FileDefEntry::new("MYFILE", "/data/f.dat")).unwrap();
        assert!(reg.lookup("myfile").is_some());
        assert!(reg.lookup("MyFile").is_some());
    }

    #[test]
    fn test_registry_duplicate() {
        let mut reg = FileDefRegistry::new();
        reg.define(FileDefEntry::new("INPUT", "/data/a.dat")).unwrap();
        assert!(reg.define(FileDefEntry::new("INPUT", "/data/b.dat")).is_err());
    }

    #[test]
    fn test_registry_undefine() {
        let mut reg = FileDefRegistry::new();
        reg.define(FileDefEntry::new("INPUT", "/data/in.dat")).unwrap();
        reg.undefine("INPUT").unwrap();
        assert!(reg.lookup("INPUT").is_none());
    }

    #[test]
    fn test_registry_undefine_not_found() {
        let mut reg = FileDefRegistry::new();
        assert!(reg.undefine("NONEXIST").is_err());
    }

    #[test]
    fn test_registry_redefine() {
        let mut reg = FileDefRegistry::new();
        reg.define(FileDefEntry::new("INPUT", "/data/old.dat")).unwrap();
        reg.redefine(FileDefEntry::new("INPUT", "/data/new.dat"));
        assert_eq!(reg.lookup("INPUT").unwrap().physical_path, "/data/new.dat");
    }

    #[test]
    fn test_registry_list_count() {
        let mut reg = FileDefRegistry::new();
        reg.define(FileDefEntry::new("A", "/a")).unwrap();
        reg.define(FileDefEntry::new("B", "/b")).unwrap();
        assert_eq!(reg.count(), 2);
        assert_eq!(reg.list().len(), 2);
    }

    #[test]
    fn test_registry_clear() {
        let mut reg = FileDefRegistry::new();
        reg.define(FileDefEntry::new("A", "/a")).unwrap();
        reg.clear();
        assert_eq!(reg.count(), 0);
    }

    // --- DYNAM tests ---

    #[test]
    fn test_dynam_allocate() {
        let mut alloc = DynamAllocation::new("INPUT", "USER.DATA.SET");
        assert!(!alloc.is_allocated());
        alloc.allocate().unwrap();
        assert!(alloc.is_allocated());
    }

    #[test]
    fn test_dynam_deallocate() {
        let mut alloc = DynamAllocation::new("INPUT", "USER.DATA.SET");
        alloc.allocate().unwrap();
        alloc.deallocate().unwrap();
        assert!(!alloc.is_allocated());
    }

    #[test]
    fn test_dynam_deallocate_not_allocated() {
        let mut alloc = DynamAllocation::new("INPUT", "USER.DATA.SET");
        assert!(alloc.deallocate().is_err());
    }

    #[test]
    fn test_dynam_allocate_empty_dsname() {
        let mut alloc = DynamAllocation::new("INPUT", "");
        assert!(alloc.allocate().is_err());
    }

    #[test]
    fn test_dynam_fields() {
        let alloc = DynamAllocation::new("DD01", "MY.DATASET");
        assert_eq!(alloc.ddname, "DD01");
        assert_eq!(alloc.dsname, "MY.DATASET");
        assert_eq!(alloc.unit, "SYSDA");
        assert_eq!(alloc.space_primary, 10);
    }

    // --- TSO Interface tests ---

    #[test]
    fn test_tso_detect() {
        let mut tso = TsoInterface::new();
        assert!(!tso.is_active());
        tso.detect();
        assert!(tso.is_active());
    }

    #[test]
    fn test_tso_userid() {
        let tso = TsoInterface::new();
        assert_eq!(tso.get_userid(), "SYSUSER");
    }

    #[test]
    fn test_tso_prefix() {
        let tso = TsoInterface::new();
        assert_eq!(tso.get_prefix(), "SYSUSER");
    }

    #[test]
    fn test_tso_command_active() {
        let mut tso = TsoInterface::new();
        tso.detect();
        let result = tso.execute_command("LISTDS 'MY.DATASET'");
        assert!(result.is_ok());
    }

    #[test]
    fn test_tso_command_inactive() {
        let tso = TsoInterface::new();
        assert!(tso.execute_command("LISTDS").is_err());
    }

    // --- CICS Interface tests ---

    #[test]
    fn test_cics_detect() {
        let mut cics = CicsInterface::new();
        assert!(!cics.is_active());
        cics.detect();
        assert!(cics.is_active());
    }

    #[test]
    fn test_cics_applid() {
        let cics = CicsInterface::new();
        assert_eq!(cics.get_applid(), "CICSAPP1");
    }

    #[test]
    fn test_cics_sysid() {
        let cics = CicsInterface::new();
        assert_eq!(cics.get_sysid(), "CI01");
    }

    #[test]
    fn test_cics_transaction_id() {
        let mut cics = CicsInterface::new();
        assert!(cics.get_transaction_id().is_none());
        cics.set_transaction_id("FOCP");
        assert_eq!(cics.get_transaction_id(), Some("FOCP"));
    }

    #[test]
    fn test_cics_command_active() {
        let mut cics = CicsInterface::new();
        cics.detect();
        assert!(cics.execute_command("INQUIRE FILE").is_ok());
    }

    #[test]
    fn test_cics_command_inactive() {
        let cics = CicsInterface::new();
        assert!(cics.execute_command("INQUIRE FILE").is_err());
    }

    // --- File disposition & format tests ---

    #[test]
    fn test_dispositions() {
        assert_ne!(FileDisposition::Old, FileDisposition::New);
        assert_ne!(FileDisposition::Shr, FileDisposition::Mod);
    }

    #[test]
    fn test_record_formats() {
        assert_ne!(RecordFormat::Fixed, RecordFormat::Variable);
        assert_ne!(RecordFormat::Variable, RecordFormat::Undefined);
    }
}
