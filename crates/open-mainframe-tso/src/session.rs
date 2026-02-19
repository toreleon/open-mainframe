//! TSO session management — profile, DD allocation table, ALTLIB search paths.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

// ---------------------------------------------------------------------------
// Allocation entry
// ---------------------------------------------------------------------------

/// Disposition for an allocated dataset.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AllocDisp {
    /// Existing dataset, shared access.
    Shr,
    /// Existing dataset, exclusive access.
    Old,
    /// New dataset.
    New,
    /// Append mode (extend or create).
    Mod,
}

impl std::fmt::Display for AllocDisp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AllocDisp::Shr => write!(f, "SHR"),
            AllocDisp::Old => write!(f, "OLD"),
            AllocDisp::New => write!(f, "NEW"),
            AllocDisp::Mod => write!(f, "MOD"),
        }
    }
}

/// DCB attributes for an allocated dataset.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DcbAttrs {
    pub lrecl: Option<u32>,
    pub blksize: Option<u32>,
    pub recfm: Option<String>,
    pub dsorg: Option<String>,
}

/// An allocated DD entry in the TSO session.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AllocEntry {
    /// DD name.
    pub dd_name: String,
    /// Dataset name (fully qualified).
    pub dsn: String,
    /// Disposition.
    pub disp: AllocDisp,
    /// DCB attributes.
    pub dcb: DcbAttrs,
    /// Resolved file system path.
    pub path: PathBuf,
    /// Whether REUSE was specified (free existing before alloc).
    pub reuse: bool,
}

// ---------------------------------------------------------------------------
// ALTLIB search path
// ---------------------------------------------------------------------------

/// ALTLIB library level.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AltlibLevel {
    /// User-level library.
    User,
    /// Application-level library.
    Application,
    /// System-level library.
    System,
}

/// ALTLIB library type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AltlibType {
    /// EXEC (REXX/CLIST).
    Exec,
    /// CLIST.
    Clist,
}

/// An entry in the ALTLIB search path.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AltlibEntry {
    pub level: AltlibLevel,
    pub lib_type: AltlibType,
    pub dsn: String,
}

// ---------------------------------------------------------------------------
// Profile
// ---------------------------------------------------------------------------

/// TSO user profile settings.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TsoProfile {
    /// Dataset name prefix (prepended to unqualified names).
    pub prefix: Option<String>,
    /// Show message IDs.
    pub msgid: bool,
    /// Display WTP (Write-To-Programmer) messages.
    pub wtpmsg: bool,
    /// Default output line size.
    pub line_size: u32,
    /// Number of output lines before pause.
    pub page_size: u32,
    /// Prompt character.
    pub prompt: String,
    /// Intercom mode.
    pub intercom: bool,
    /// Recovery mode.
    pub recover: bool,
}

impl Default for TsoProfile {
    fn default() -> Self {
        Self {
            prefix: None,
            msgid: true,
            wtpmsg: true,
            line_size: 80,
            page_size: 24,
            prompt: "READY".to_string(),
            intercom: false,
            recover: true,
        }
    }
}

// ---------------------------------------------------------------------------
// Session
// ---------------------------------------------------------------------------

/// A TSO session containing allocations, profile, and search paths.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TsoSession {
    /// User ID.
    pub userid: String,
    /// User profile settings.
    pub profile: TsoProfile,
    /// DD allocations (keyed by DD name).
    allocations: HashMap<String, AllocEntry>,
    /// ALTLIB search paths.
    altlibs: Vec<AltlibEntry>,
    /// Base directory for dataset resolution.
    pub dataset_dir: PathBuf,
}

impl TsoSession {
    /// Create a new TSO session for the given user.
    pub fn new(userid: &str, dataset_dir: PathBuf) -> Self {
        Self {
            userid: userid.to_ascii_uppercase(),
            profile: TsoProfile {
                prefix: Some(userid.to_ascii_uppercase()),
                ..TsoProfile::default()
            },
            allocations: HashMap::new(),
            altlibs: Vec::new(),
            dataset_dir,
        }
    }

    // ----- Allocation management -----

    /// Allocate a dataset to a DD name.
    pub fn allocate(&mut self, entry: AllocEntry) -> crate::Result<()> {
        let dd = entry.dd_name.to_ascii_uppercase();
        if self.allocations.contains_key(&dd) && !entry.reuse {
            return Err(crate::TsoError::DdAlreadyAllocated(dd));
        }
        self.allocations.insert(dd, entry);
        Ok(())
    }

    /// Free (deallocate) a DD name.
    pub fn free(&mut self, dd_name: &str) -> crate::Result<AllocEntry> {
        let dd = dd_name.to_ascii_uppercase();
        self.allocations
            .remove(&dd)
            .ok_or(crate::TsoError::DdNotAllocated(dd))
    }

    /// Get an allocation by DD name.
    pub fn get_alloc(&self, dd_name: &str) -> Option<&AllocEntry> {
        self.allocations.get(&dd_name.to_ascii_uppercase())
    }

    /// List all allocations.
    pub fn list_allocs(&self) -> Vec<&AllocEntry> {
        let mut entries: Vec<&AllocEntry> = self.allocations.values().collect();
        entries.sort_by(|a, b| a.dd_name.cmp(&b.dd_name));
        entries
    }

    /// Number of active allocations.
    pub fn alloc_count(&self) -> usize {
        self.allocations.len()
    }

    // ----- ALTLIB management -----

    /// Activate an ALTLIB entry.
    pub fn altlib_activate(&mut self, entry: AltlibEntry) {
        // Remove existing entry at the same level+type
        self.altlibs.retain(|e| !(e.level == entry.level && e.lib_type == entry.lib_type));
        self.altlibs.push(entry);
    }

    /// Deactivate ALTLIB at the given level and type.
    pub fn altlib_deactivate(&mut self, level: &AltlibLevel, lib_type: &AltlibType) -> bool {
        let before = self.altlibs.len();
        self.altlibs.retain(|e| !(&e.level == level && &e.lib_type == lib_type));
        self.altlibs.len() < before
    }

    /// List active ALTLIB entries.
    pub fn altlib_list(&self) -> &[AltlibEntry] {
        &self.altlibs
    }

    // ----- Dataset name resolution -----

    /// Qualify a dataset name using the current prefix.
    ///
    /// If the name is already quoted (starts with `'`), the quotes are stripped
    /// and the name is used as-is. Otherwise, the profile prefix is prepended.
    pub fn qualify_dsn(&self, raw: &str) -> String {
        let raw = raw.trim();
        if raw.starts_with('\'') && raw.ends_with('\'') {
            // Fully qualified — strip quotes
            raw[1..raw.len() - 1].to_ascii_uppercase()
        } else if let Some(ref prefix) = self.profile.prefix {
            format!("{}.{}", prefix, raw.to_ascii_uppercase())
        } else {
            raw.to_ascii_uppercase()
        }
    }

    /// Resolve a dataset name to a file path.
    pub fn resolve_dsn(&self, dsn: &str) -> PathBuf {
        let mut path = self.dataset_dir.clone();
        for part in dsn.split('.') {
            path.push(part);
        }
        path
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn test_session() -> TsoSession {
        TsoSession::new("USER01", PathBuf::from("/data"))
    }

    #[test]
    fn test_new_session() {
        let s = test_session();
        assert_eq!(s.userid, "USER01");
        assert_eq!(s.profile.prefix, Some("USER01".to_string()));
        assert!(s.profile.msgid);
    }

    #[test]
    fn test_allocate_and_free() {
        let mut s = test_session();
        let entry = AllocEntry {
            dd_name: "INFILE".to_string(),
            dsn: "PROD.DATA".to_string(),
            disp: AllocDisp::Shr,
            dcb: DcbAttrs::default(),
            path: PathBuf::from("/data/PROD/DATA"),
            reuse: false,
        };
        s.allocate(entry).unwrap();
        assert_eq!(s.alloc_count(), 1);
        assert!(s.get_alloc("INFILE").is_some());

        let freed = s.free("INFILE").unwrap();
        assert_eq!(freed.dsn, "PROD.DATA");
        assert_eq!(s.alloc_count(), 0);
    }

    #[test]
    fn test_allocate_duplicate_fails() {
        let mut s = test_session();
        let entry = AllocEntry {
            dd_name: "DD1".to_string(),
            dsn: "A.B".to_string(),
            disp: AllocDisp::Shr,
            dcb: DcbAttrs::default(),
            path: PathBuf::from("/data/A/B"),
            reuse: false,
        };
        s.allocate(entry.clone()).unwrap();
        assert!(s.allocate(entry).is_err());
    }

    #[test]
    fn test_allocate_reuse() {
        let mut s = test_session();
        let entry1 = AllocEntry {
            dd_name: "DD1".to_string(),
            dsn: "A.B".to_string(),
            disp: AllocDisp::Shr,
            dcb: DcbAttrs::default(),
            path: PathBuf::from("/data/A/B"),
            reuse: false,
        };
        s.allocate(entry1).unwrap();

        let entry2 = AllocEntry {
            dd_name: "DD1".to_string(),
            dsn: "C.D".to_string(),
            disp: AllocDisp::Old,
            dcb: DcbAttrs::default(),
            path: PathBuf::from("/data/C/D"),
            reuse: true,
        };
        s.allocate(entry2).unwrap();

        let alloc = s.get_alloc("DD1").unwrap();
        assert_eq!(alloc.dsn, "C.D");
    }

    #[test]
    fn test_free_not_allocated() {
        let mut s = test_session();
        assert!(s.free("NODD").is_err());
    }

    #[test]
    fn test_qualify_dsn_with_prefix() {
        let s = test_session();
        assert_eq!(s.qualify_dsn("MY.DATA"), "USER01.MY.DATA");
    }

    #[test]
    fn test_qualify_dsn_fully_qualified() {
        let s = test_session();
        assert_eq!(s.qualify_dsn("'SYS1.PARMLIB'"), "SYS1.PARMLIB");
    }

    #[test]
    fn test_qualify_dsn_no_prefix() {
        let mut s = test_session();
        s.profile.prefix = None;
        assert_eq!(s.qualify_dsn("MY.DATA"), "MY.DATA");
    }

    #[test]
    fn test_resolve_dsn() {
        let s = test_session();
        assert_eq!(s.resolve_dsn("SYS1.PARMLIB"), PathBuf::from("/data/SYS1/PARMLIB"));
    }

    #[test]
    fn test_list_allocs_sorted() {
        let mut s = test_session();
        for dd in ["ZDD", "ADD", "MDD"] {
            s.allocate(AllocEntry {
                dd_name: dd.to_string(),
                dsn: format!("{dd}.DATA"),
                disp: AllocDisp::Shr,
                dcb: DcbAttrs::default(),
                path: PathBuf::from(format!("/data/{dd}")),
                reuse: false,
            })
            .unwrap();
        }
        let allocs = s.list_allocs();
        assert_eq!(allocs[0].dd_name, "ADD");
        assert_eq!(allocs[1].dd_name, "MDD");
        assert_eq!(allocs[2].dd_name, "ZDD");
    }

    #[test]
    fn test_altlib_activate_deactivate() {
        let mut s = test_session();
        s.altlib_activate(AltlibEntry {
            level: AltlibLevel::Application,
            lib_type: AltlibType::Exec,
            dsn: "USER01.MY.EXEC".to_string(),
        });
        assert_eq!(s.altlib_list().len(), 1);

        s.altlib_deactivate(&AltlibLevel::Application, &AltlibType::Exec);
        assert_eq!(s.altlib_list().len(), 0);
    }

    #[test]
    fn test_altlib_replace_same_level() {
        let mut s = test_session();
        s.altlib_activate(AltlibEntry {
            level: AltlibLevel::Application,
            lib_type: AltlibType::Exec,
            dsn: "OLD.EXEC".to_string(),
        });
        s.altlib_activate(AltlibEntry {
            level: AltlibLevel::Application,
            lib_type: AltlibType::Exec,
            dsn: "NEW.EXEC".to_string(),
        });
        assert_eq!(s.altlib_list().len(), 1);
        assert_eq!(s.altlib_list()[0].dsn, "NEW.EXEC");
    }
}
