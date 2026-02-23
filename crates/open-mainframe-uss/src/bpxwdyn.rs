//! BPXWDYN Dynamic Allocation (USS-112).
//!
//! BPXWDYN is a text interface to a subset of SVC 99 (dynamic allocation) and
//! SVC 109 (dynamic output) services, designed for invocation from REXX and
//! high-level languages within USS.
//!
//! Supported operations:
//! - ALLOC — dynamic allocation of a dataset or file
//! - FREE — dynamic unallocation (deallocation)
//! - CONCAT — dynamic concatenation of datasets
//! - INFO — retrieve allocation information
//!
//! Enhanced entry point BPXWDY2 preserves the invoker program mask.

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors for BPXWDYN operations.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, miette::Diagnostic)]
pub enum BpxwdynError {
    /// Parse error in BPXWDYN command string.
    #[error("BPXWDYN parse error: {detail}")]
    ParseError { detail: String },

    /// DD name already allocated.
    #[error("DD name already allocated: {ddname}")]
    DdAlreadyAllocated { ddname: String },

    /// DD name not found.
    #[error("DD name not found: {ddname}")]
    DdNotFound { ddname: String },

    /// Dataset not found.
    #[error("dataset not found: {dsn}")]
    DatasetNotFound { dsn: String },

    /// Invalid disposition.
    #[error("invalid disposition: {disp}")]
    InvalidDisposition { disp: String },
}

// ---------------------------------------------------------------------------
//  File Data Type
// ---------------------------------------------------------------------------

/// Data type for a file or dataset.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileDataType {
    /// Text data (subject to codepage conversion and newline handling).
    Text,
    /// Binary data (no conversion).
    Binary,
    /// Record-oriented I/O (typical for MVS datasets).
    Record,
}

impl FileDataType {
    /// Parse from string.
    pub fn from_str_opt(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "TEXT" => Some(Self::Text),
            "BINARY" => Some(Self::Binary),
            "RECORD" => Some(Self::Record),
            _ => None,
        }
    }
}

impl std::fmt::Display for FileDataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Text => write!(f, "TEXT"),
            Self::Binary => write!(f, "BINARY"),
            Self::Record => write!(f, "RECORD"),
        }
    }
}

// ---------------------------------------------------------------------------
//  Dataset Disposition
// ---------------------------------------------------------------------------

/// Dataset disposition for allocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Disposition {
    /// SHR — shared access.
    Shr,
    /// OLD — exclusive access (existing dataset).
    Old,
    /// MOD — append to end of dataset.
    Mod,
    /// NEW — create a new dataset.
    New,
}

impl Disposition {
    /// Parse from string.
    pub fn from_str_opt(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "SHR" => Some(Self::Shr),
            "OLD" => Some(Self::Old),
            "MOD" => Some(Self::Mod),
            "NEW" => Some(Self::New),
            _ => None,
        }
    }
}

impl std::fmt::Display for Disposition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Shr => write!(f, "SHR"),
            Self::Old => write!(f, "OLD"),
            Self::Mod => write!(f, "MOD"),
            Self::New => write!(f, "NEW"),
        }
    }
}

// ---------------------------------------------------------------------------
//  Path Options
// ---------------------------------------------------------------------------

/// File open options for USS path allocation (PATHOPTS keyword).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathOpt {
    /// ORDONLY — open for reading only.
    Ordonly,
    /// OWRONLY — open for writing only.
    Owronly,
    /// ORDWR — open for reading and writing.
    Ordwr,
    /// OCREAT — create file if it doesn't exist.
    Ocreat,
    /// OTRUNC — truncate file on open.
    Otrunc,
    /// OAPPEND — append writes.
    Oappend,
    /// OEXCL — exclusive create.
    Oexcl,
}

impl PathOpt {
    /// Parse from string.
    pub fn from_str_opt(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "ORDONLY" => Some(Self::Ordonly),
            "OWRONLY" => Some(Self::Owronly),
            "ORDWR" => Some(Self::Ordwr),
            "OCREAT" => Some(Self::Ocreat),
            "OTRUNC" => Some(Self::Otrunc),
            "OAPPEND" => Some(Self::Oappend),
            "OEXCL" => Some(Self::Oexcl),
            _ => None,
        }
    }
}

// ---------------------------------------------------------------------------
//  Path Mode
// ---------------------------------------------------------------------------

/// Permission mode constants for PATHMODE keyword.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathMode {
    /// SIRUSR — owner read.
    Sirusr,
    /// SIWUSR — owner write.
    Siwusr,
    /// SIXUSR — owner execute.
    Sixusr,
    /// SIRWXU — owner read/write/execute.
    Sirwxu,
    /// SIRGRP — group read.
    Sirgrp,
    /// SIWGRP — group write.
    Siwgrp,
    /// SIXGRP — group execute.
    Sixgrp,
    /// SIRWXG — group read/write/execute.
    Sirwxg,
    /// SIROTH — other read.
    Siroth,
    /// SIWOTH — other write.
    Siwoth,
    /// SIXOTH — other execute.
    Sixoth,
    /// SIRWXO — other read/write/execute.
    Sirwxo,
}

impl PathMode {
    /// Convert to octal permission bits.
    pub fn bits(self) -> u32 {
        match self {
            Self::Sirusr => 0o400,
            Self::Siwusr => 0o200,
            Self::Sixusr => 0o100,
            Self::Sirwxu => 0o700,
            Self::Sirgrp => 0o040,
            Self::Siwgrp => 0o020,
            Self::Sixgrp => 0o010,
            Self::Sirwxg => 0o070,
            Self::Siroth => 0o004,
            Self::Siwoth => 0o002,
            Self::Sixoth => 0o001,
            Self::Sirwxo => 0o007,
        }
    }

    /// Parse from string.
    pub fn from_str_opt(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "SIRUSR" => Some(Self::Sirusr),
            "SIWUSR" => Some(Self::Siwusr),
            "SIXUSR" => Some(Self::Sixusr),
            "SIRWXU" => Some(Self::Sirwxu),
            "SIRGRP" => Some(Self::Sirgrp),
            "SIWGRP" => Some(Self::Siwgrp),
            "SIXGRP" => Some(Self::Sixgrp),
            "SIRWXG" => Some(Self::Sirwxg),
            "SIROTH" => Some(Self::Siroth),
            "SIWOTH" => Some(Self::Siwoth),
            "SIXOTH" => Some(Self::Sixoth),
            "SIRWXO" => Some(Self::Sirwxo),
            _ => None,
        }
    }
}

// ---------------------------------------------------------------------------
//  Allocation Request
// ---------------------------------------------------------------------------

/// A parsed BPXWDYN allocation request.
#[derive(Debug, Clone)]
pub struct AllocRequest {
    /// DD name.
    pub ddname: String,
    /// Dataset name (for MVS datasets).
    pub dsn: Option<String>,
    /// USS file path.
    pub path: Option<String>,
    /// Path open options.
    pub path_opts: Vec<PathOpt>,
    /// Path permission mode.
    pub path_mode: Vec<PathMode>,
    /// Data type.
    pub file_data: Option<FileDataType>,
    /// Disposition (SHR, OLD, MOD, NEW).
    pub disposition: Option<Disposition>,
}

// ---------------------------------------------------------------------------
//  Allocation Table Entry
// ---------------------------------------------------------------------------

/// A tracked DD allocation.
#[derive(Debug, Clone)]
pub struct Allocation {
    /// DD name.
    pub ddname: String,
    /// Dataset name (if MVS dataset).
    pub dsn: Option<String>,
    /// USS file path (if USS path).
    pub path: Option<String>,
    /// Disposition.
    pub disposition: Option<Disposition>,
    /// Data type.
    pub file_data: Option<FileDataType>,
}

// ---------------------------------------------------------------------------
//  BPXWDYN Manager
// ---------------------------------------------------------------------------

/// Manages dynamic allocation/deallocation via BPXWDYN.
#[derive(Debug)]
pub struct BpxwdynManager {
    /// Active allocations by DD name.
    allocations: HashMap<String, Allocation>,
    /// Concatenations: DD name -> list of dataset names.
    concatenations: HashMap<String, Vec<String>>,
}

impl BpxwdynManager {
    /// Create a new BPXWDYN manager.
    pub fn new() -> Self {
        Self {
            allocations: HashMap::new(),
            concatenations: HashMap::new(),
        }
    }

    /// Parse and execute a BPXWDYN command string.
    ///
    /// Example: "ALLOC DD(MYFILE) DSN('USER.DATA.SET') SHR"
    pub fn execute(&mut self, command: &str) -> Result<(), BpxwdynError> {
        let parts: Vec<&str> = command.split_whitespace().collect();
        if parts.is_empty() {
            return Err(BpxwdynError::ParseError {
                detail: "empty command".to_string(),
            });
        }

        match parts[0].to_uppercase().as_str() {
            "ALLOC" => self.execute_alloc(&parts[1..]),
            "FREE" => self.execute_free(&parts[1..]),
            "CONCAT" => self.execute_concat(&parts[1..]),
            "INFO" => self.execute_info(&parts[1..]),
            other => Err(BpxwdynError::ParseError {
                detail: format!("unknown operation: {other}"),
            }),
        }
    }

    /// Execute an ALLOC command.
    fn execute_alloc(&mut self, params: &[&str]) -> Result<(), BpxwdynError> {
        let mut ddname = None;
        let mut dsn = None;
        let mut path = None;
        let mut disposition = None;
        let mut file_data = None;
        let mut path_opts = Vec::new();
        let mut path_mode = Vec::new();

        for &param in params {
            if let Some(val) = extract_paren(param, "DD") {
                ddname = Some(val);
            } else if let Some(val) = extract_paren(param, "DSN") {
                dsn = Some(val.trim_matches('\'').to_string());
            } else if let Some(val) = extract_paren(param, "PATH") {
                path = Some(val);
            } else if let Some(val) = extract_paren(param, "PATHOPTS") {
                for opt_str in val.split(',') {
                    let opt_str = opt_str.trim().trim_matches('(').trim_matches(')');
                    if let Some(opt) = PathOpt::from_str_opt(opt_str) {
                        path_opts.push(opt);
                    }
                }
            } else if let Some(val) = extract_paren(param, "PATHMODE") {
                for mode_str in val.split(',') {
                    let mode_str = mode_str.trim().trim_matches('(').trim_matches(')');
                    if let Some(mode) = PathMode::from_str_opt(mode_str) {
                        path_mode.push(mode);
                    }
                }
            } else if let Some(val) = extract_paren(param, "FILEDATA") {
                file_data = FileDataType::from_str_opt(&val);
            } else if let Some(d) = Disposition::from_str_opt(param) {
                disposition = Some(d);
            }
        }

        let ddname = ddname.ok_or(BpxwdynError::ParseError {
            detail: "DD name required".to_string(),
        })?;

        if self.allocations.contains_key(&ddname) {
            return Err(BpxwdynError::DdAlreadyAllocated {
                ddname: ddname.clone(),
            });
        }

        self.allocations.insert(
            ddname.clone(),
            Allocation {
                ddname: ddname.clone(),
                dsn,
                path,
                disposition,
                file_data,
            },
        );

        // Store the parsed request parts for potential future use.
        let _ = (path_opts, path_mode);
        Ok(())
    }

    /// Execute a FREE command.
    fn execute_free(&mut self, params: &[&str]) -> Result<(), BpxwdynError> {
        let mut ddname = None;
        for &param in params {
            if let Some(val) = extract_paren(param, "DD") {
                ddname = Some(val);
            }
        }

        let ddname = ddname.ok_or(BpxwdynError::ParseError {
            detail: "DD name required for FREE".to_string(),
        })?;

        self.allocations
            .remove(&ddname)
            .ok_or(BpxwdynError::DdNotFound {
                ddname: ddname.clone(),
            })?;
        self.concatenations.remove(&ddname);
        Ok(())
    }

    /// Execute a CONCAT command.
    fn execute_concat(&mut self, params: &[&str]) -> Result<(), BpxwdynError> {
        let mut ddname = None;
        let mut datasets = Vec::new();

        for &param in params {
            if let Some(val) = extract_paren(param, "DD") {
                ddname = Some(val);
            } else if let Some(val) = extract_paren(param, "DSN") {
                datasets.push(val.trim_matches('\'').to_string());
            }
        }

        let ddname = ddname.ok_or(BpxwdynError::ParseError {
            detail: "DD name required for CONCAT".to_string(),
        })?;

        self.concatenations.insert(ddname, datasets);
        Ok(())
    }

    /// Execute an INFO command (returns allocation info).
    fn execute_info(&self, params: &[&str]) -> Result<(), BpxwdynError> {
        let mut ddname = None;
        for &param in params {
            if let Some(val) = extract_paren(param, "DD") {
                ddname = Some(val);
            }
        }

        let ddname = ddname.ok_or(BpxwdynError::ParseError {
            detail: "DD name required for INFO".to_string(),
        })?;

        if !self.allocations.contains_key(&ddname) {
            return Err(BpxwdynError::DdNotFound { ddname });
        }

        Ok(())
    }

    /// Get an allocation by DD name.
    pub fn get_allocation(&self, ddname: &str) -> Option<&Allocation> {
        self.allocations.get(ddname)
    }

    /// Get a concatenation by DD name.
    pub fn get_concatenation(&self, ddname: &str) -> Option<&Vec<String>> {
        self.concatenations.get(ddname)
    }

    /// List all active DD names.
    pub fn list_allocations(&self) -> Vec<&str> {
        let mut names: Vec<&str> = self.allocations.keys().map(|s| s.as_str()).collect();
        names.sort();
        names
    }

    /// Number of active allocations.
    pub fn allocation_count(&self) -> usize {
        self.allocations.len()
    }
}

impl Default for BpxwdynManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Extract the value from a KEYWORD(value) parameter.
fn extract_paren(param: &str, keyword: &str) -> Option<String> {
    let upper = param.to_uppercase();
    let kw_upper = keyword.to_uppercase();
    if upper.starts_with(&format!("{kw_upper}(")) {
        if let Some(start) = param.find('(') {
            if let Some(end) = param.rfind(')') {
                return Some(param[start + 1..end].to_string());
            }
        }
    }
    None
}

// ===========================================================================
//  Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_alloc_dataset_shr() {
        let mut mgr = BpxwdynManager::new();
        mgr.execute("ALLOC DD(MYFILE) DSN('USER.DATA.SET') SHR")
            .unwrap();

        let alloc = mgr.get_allocation("MYFILE").unwrap();
        assert_eq!(alloc.dsn.as_deref(), Some("USER.DATA.SET"));
        assert_eq!(alloc.disposition, Some(Disposition::Shr));
    }

    #[test]
    fn test_alloc_uss_path() {
        let mut mgr = BpxwdynManager::new();
        mgr.execute("ALLOC DD(USSFILE) PATH(/u/user/file.txt) FILEDATA(TEXT)")
            .unwrap();

        let alloc = mgr.get_allocation("USSFILE").unwrap();
        assert_eq!(alloc.path.as_deref(), Some("/u/user/file.txt"));
        assert_eq!(alloc.file_data, Some(FileDataType::Text));
    }

    #[test]
    fn test_free_dd() {
        let mut mgr = BpxwdynManager::new();
        mgr.execute("ALLOC DD(TEMP) DSN('TEMP.DATA') SHR")
            .unwrap();
        assert_eq!(mgr.allocation_count(), 1);

        mgr.execute("FREE DD(TEMP)").unwrap();
        assert_eq!(mgr.allocation_count(), 0);
    }

    #[test]
    fn test_free_nonexistent() {
        let mut mgr = BpxwdynManager::new();
        let err = mgr.execute("FREE DD(NODD)").unwrap_err();
        assert!(matches!(err, BpxwdynError::DdNotFound { .. }));
    }

    #[test]
    fn test_alloc_duplicate_dd() {
        let mut mgr = BpxwdynManager::new();
        mgr.execute("ALLOC DD(DUP) DSN('DATA.SET1') SHR").unwrap();
        let err = mgr
            .execute("ALLOC DD(DUP) DSN('DATA.SET2') SHR")
            .unwrap_err();
        assert!(matches!(err, BpxwdynError::DdAlreadyAllocated { .. }));
    }

    #[test]
    fn test_concat() {
        let mut mgr = BpxwdynManager::new();
        mgr.execute("CONCAT DD(SYSLIB) DSN('USER.MACLIB') DSN('SYS1.MACLIB')")
            .unwrap();

        let concat = mgr.get_concatenation("SYSLIB").unwrap();
        assert_eq!(concat.len(), 2);
        assert_eq!(concat[0], "USER.MACLIB");
        assert_eq!(concat[1], "SYS1.MACLIB");
    }

    #[test]
    fn test_info_existing() {
        let mut mgr = BpxwdynManager::new();
        mgr.execute("ALLOC DD(MYDD) DSN('MY.DATA') SHR").unwrap();
        mgr.execute("INFO DD(MYDD)").unwrap();
    }

    #[test]
    fn test_info_nonexistent() {
        let mgr = BpxwdynManager::new();
        let err = mgr.execute_info(&["DD(NODD)"]);
        assert!(matches!(err, Err(BpxwdynError::DdNotFound { .. })));
    }

    #[test]
    fn test_list_allocations() {
        let mut mgr = BpxwdynManager::new();
        mgr.execute("ALLOC DD(DD1) DSN('DS1') SHR").unwrap();
        mgr.execute("ALLOC DD(DD2) DSN('DS2') SHR").unwrap();

        let list = mgr.list_allocations();
        assert_eq!(list, vec!["DD1", "DD2"]);
    }

    #[test]
    fn test_disposition_variants() {
        assert_eq!(Disposition::from_str_opt("SHR"), Some(Disposition::Shr));
        assert_eq!(Disposition::from_str_opt("OLD"), Some(Disposition::Old));
        assert_eq!(Disposition::from_str_opt("MOD"), Some(Disposition::Mod));
        assert_eq!(Disposition::from_str_opt("NEW"), Some(Disposition::New));
        assert_eq!(Disposition::from_str_opt("BAD"), None);
    }

    #[test]
    fn test_file_data_type() {
        assert_eq!(FileDataType::from_str_opt("TEXT"), Some(FileDataType::Text));
        assert_eq!(
            FileDataType::from_str_opt("BINARY"),
            Some(FileDataType::Binary)
        );
        assert_eq!(
            FileDataType::from_str_opt("RECORD"),
            Some(FileDataType::Record)
        );
    }

    #[test]
    fn test_path_mode_bits() {
        assert_eq!(PathMode::Sirwxu.bits(), 0o700);
        assert_eq!(PathMode::Sirusr.bits(), 0o400);
        assert_eq!(PathMode::Siwusr.bits(), 0o200);
        assert_eq!(PathMode::Sixusr.bits(), 0o100);
    }

    #[test]
    fn test_unknown_operation() {
        let mut mgr = BpxwdynManager::new();
        let err = mgr.execute("BADOP DD(X)").unwrap_err();
        assert!(matches!(err, BpxwdynError::ParseError { .. }));
    }

    #[test]
    fn test_empty_command() {
        let mut mgr = BpxwdynManager::new();
        let err = mgr.execute("").unwrap_err();
        assert!(matches!(err, BpxwdynError::ParseError { .. }));
    }
}
