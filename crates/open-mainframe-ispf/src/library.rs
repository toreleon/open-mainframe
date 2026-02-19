//! ISPF Library Management (LM) Services — programmatic dataset and member access.
//!
//! Implements the ISPF LM service API:
//! - **LMINIT/LMFREE** — Initialize and release a data ID for a dataset.
//! - **LMOPEN/LMCLOSE** — Open and close a dataset for record I/O.
//! - **LMGET/LMPUT** — Read and write records.
//! - **LMMFIND** — Locate a PDS member and optionally retrieve statistics.
//! - **LMMADD/LMMDEL/LMMREP** — Add, delete, and replace PDS members.
//! - **LMMLIST** — Enumerate PDS members.
//! - **LMMSTATS** — Set or retrieve member ISPF statistics.
//! - **LIBDEF** — Override ISPF library concatenations (ISPPLIB, ISPMLIB, etc.).

use std::collections::{BTreeMap, HashMap};

// ---------------------------------------------------------------------------
//  Return codes
// ---------------------------------------------------------------------------

/// ISPF service return codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LmRc {
    /// 0 — Normal completion.
    Ok,
    /// 4 — Warning (e.g., empty dataset, no more members).
    Warning,
    /// 8 — End of dataset / member not found.
    EndOrNotFound,
    /// 12 — Dataset not open / invalid data ID / invalid state.
    InvalidState,
    /// 20 — Severe error.
    Severe,
}

impl LmRc {
    /// Numeric return code value.
    pub fn code(self) -> u32 {
        match self {
            Self::Ok => 0,
            Self::Warning => 4,
            Self::EndOrNotFound => 8,
            Self::InvalidState => 12,
            Self::Severe => 20,
        }
    }
}

impl std::fmt::Display for LmRc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.code())
    }
}

// ---------------------------------------------------------------------------
//  Open mode
// ---------------------------------------------------------------------------

/// Mode for LMOPEN.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpenMode {
    /// INPUT — read only.
    Input,
    /// OUTPUT — write (new members / replace).
    Output,
    /// UPDATE — read and write existing records.
    Update,
}

// ---------------------------------------------------------------------------
//  ISPF member statistics
// ---------------------------------------------------------------------------

/// ISPF member statistics (LMMSTATS / LMMFIND STATS(YES)).
#[derive(Debug, Clone, Default)]
pub struct MemberStats {
    /// Version number (ZLVERS).
    pub version: u8,
    /// Modification level (ZLMOD).
    pub modification: u8,
    /// Last modified date (ZLMDATE) — YYYY/MM/DD.
    pub mod_date: String,
    /// Last modified time (ZLMTIME) — HH:MM:SS.
    pub mod_time: String,
    /// User ID of last modifier (ZLUSER).
    pub user: String,
    /// Current number of lines (ZLCNORC).
    pub current_lines: u32,
    /// Initial number of lines (ZLINORC).
    pub initial_lines: u32,
    /// Number of modified lines (ZLMNORC).
    pub modified_lines: u32,
    /// Creation date (ZLCDATE).
    pub create_date: String,
}

// ---------------------------------------------------------------------------
//  PDS Member
// ---------------------------------------------------------------------------

/// A PDS member with optional ISPF statistics and data.
#[derive(Debug, Clone)]
pub struct PdsMember {
    /// Member name (up to 8 characters, uppercase).
    pub name: String,
    /// ISPF statistics (if available).
    pub stats: Option<MemberStats>,
    /// Member data (lines/records).
    pub data: Vec<String>,
}

// ---------------------------------------------------------------------------
//  Dataset handle
// ---------------------------------------------------------------------------

/// Internal dataset state tracked by the library manager.
#[derive(Debug, Clone)]
struct DatasetHandle {
    /// Dataset name (fully qualified).
    dsname: String,
    /// Members (for PDS — name → member).
    members: BTreeMap<String, PdsMember>,
    /// Sequential data (for sequential datasets).
    seq_data: Vec<String>,
    /// Whether this is a PDS (true) or sequential (false).
    is_pds: bool,
    /// Current open mode, if opened.
    open_mode: Option<OpenMode>,
    /// Read cursor (for LMGET).
    read_cursor: usize,
    /// Current member for read/write (LMMFIND sets this).
    current_member: Option<String>,
    /// Member list cursor (for LMMLIST).
    member_list_cursor: usize,
    /// Sorted member names (cached for LMMLIST).
    member_list_cache: Vec<String>,
}

// ---------------------------------------------------------------------------
//  LIBDEF entry
// ---------------------------------------------------------------------------

/// LIBDEF library type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LibdefType {
    /// ISPPLIB — panel library.
    Ispplib,
    /// ISPMLIB — message library.
    Ispmlib,
    /// ISPSLIB — skeleton library.
    Ispslib,
    /// ISPTLIB — table input library.
    Isptlib,
    /// ISPLLIB — load library.
    Ispllib,
    /// ISPTABL — table output library.
    Isptabl,
}

impl std::fmt::Display for LibdefType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ispplib => write!(f, "ISPPLIB"),
            Self::Ispmlib => write!(f, "ISPMLIB"),
            Self::Ispslib => write!(f, "ISPSLIB"),
            Self::Isptlib => write!(f, "ISPTLIB"),
            Self::Ispllib => write!(f, "ISPLLIB"),
            Self::Isptabl => write!(f, "ISPTABL"),
        }
    }
}

/// A LIBDEF entry — stacked library override.
#[derive(Debug, Clone)]
pub struct LibdefEntry {
    /// Library type being overridden.
    pub lib_type: LibdefType,
    /// Dataset name(s) in the override concatenation.
    pub datasets: Vec<String>,
}

// ---------------------------------------------------------------------------
//  Library Manager
// ---------------------------------------------------------------------------

/// ISPF Library Manager — manages data IDs, dataset I/O, member operations, and LIBDEF.
#[derive(Debug, Clone)]
pub struct LibraryManager {
    /// Data ID → dataset handle.
    handles: HashMap<String, DatasetHandle>,
    /// Next data ID number.
    next_id: u32,
    /// LIBDEF stack per library type (most recent first).
    libdefs: HashMap<LibdefType, Vec<LibdefEntry>>,
}

impl Default for LibraryManager {
    fn default() -> Self {
        Self::new()
    }
}

impl LibraryManager {
    /// Create a new library manager.
    pub fn new() -> Self {
        Self {
            handles: HashMap::new(),
            next_id: 1,
            libdefs: HashMap::new(),
        }
    }

    // -------------------------------------------------------------------
    //  LMINIT / LMFREE
    // -------------------------------------------------------------------

    /// LMINIT — Initialize a data ID for a dataset.
    ///
    /// Returns the assigned data ID string.
    pub fn lminit(&mut self, dsname: &str, is_pds: bool) -> (LmRc, String) {
        let id = format!("LM{:04}", self.next_id);
        self.next_id += 1;

        let handle = DatasetHandle {
            dsname: dsname.to_uppercase(),
            members: BTreeMap::new(),
            seq_data: Vec::new(),
            is_pds,
            open_mode: None,
            read_cursor: 0,
            current_member: None,
            member_list_cursor: 0,
            member_list_cache: Vec::new(),
        };
        self.handles.insert(id.clone(), handle);
        (LmRc::Ok, id)
    }

    /// LMFREE — Release a data ID.
    pub fn lmfree(&mut self, data_id: &str) -> LmRc {
        if self.handles.remove(data_id).is_some() {
            LmRc::Ok
        } else {
            LmRc::InvalidState
        }
    }

    // -------------------------------------------------------------------
    //  LMOPEN / LMCLOSE
    // -------------------------------------------------------------------

    /// LMOPEN — Open a dataset for I/O.
    pub fn lmopen(&mut self, data_id: &str, mode: OpenMode) -> LmRc {
        let Some(handle) = self.handles.get_mut(data_id) else {
            return LmRc::InvalidState;
        };
        if handle.open_mode.is_some() {
            return LmRc::InvalidState; // Already open.
        }
        handle.open_mode = Some(mode);
        handle.read_cursor = 0;
        LmRc::Ok
    }

    /// LMCLOSE — Close a dataset.
    pub fn lmclose(&mut self, data_id: &str) -> LmRc {
        let Some(handle) = self.handles.get_mut(data_id) else {
            return LmRc::InvalidState;
        };
        if handle.open_mode.is_none() {
            return LmRc::InvalidState; // Not open.
        }
        handle.open_mode = None;
        handle.current_member = None;
        handle.read_cursor = 0;
        LmRc::Ok
    }

    // -------------------------------------------------------------------
    //  LMGET / LMPUT
    // -------------------------------------------------------------------

    /// LMGET — Read the next record from the dataset.
    ///
    /// For a PDS, reads from the current member (set by LMMFIND).
    /// For a sequential dataset, reads sequentially.
    ///
    /// Returns `(LmRc, Option<record>)`.
    pub fn lmget(&mut self, data_id: &str) -> (LmRc, Option<String>) {
        let Some(handle) = self.handles.get_mut(data_id) else {
            return (LmRc::InvalidState, None);
        };
        if !matches!(handle.open_mode, Some(OpenMode::Input | OpenMode::Update)) {
            return (LmRc::InvalidState, None);
        }

        let data = if handle.is_pds {
            match &handle.current_member {
                Some(name) => match handle.members.get(name) {
                    Some(m) => &m.data,
                    None => return (LmRc::EndOrNotFound, None),
                },
                None => return (LmRc::InvalidState, None),
            }
        } else {
            &handle.seq_data
        };

        if handle.read_cursor >= data.len() {
            return (LmRc::EndOrNotFound, None);
        }

        let record = data[handle.read_cursor].clone();
        handle.read_cursor += 1;
        (LmRc::Ok, Some(record))
    }

    /// LMPUT — Write a record to the dataset.
    ///
    /// For a PDS, writes to the current member.
    /// For a sequential dataset, appends to the data.
    pub fn lmput(&mut self, data_id: &str, record: &str) -> LmRc {
        let Some(handle) = self.handles.get_mut(data_id) else {
            return LmRc::InvalidState;
        };
        if !matches!(handle.open_mode, Some(OpenMode::Output | OpenMode::Update)) {
            return LmRc::InvalidState;
        }

        if handle.is_pds {
            match &handle.current_member {
                Some(name) => {
                    let name = name.clone();
                    let member = handle.members.entry(name).or_insert_with(|| PdsMember {
                        name: handle.current_member.clone().unwrap_or_default(),
                        stats: None,
                        data: Vec::new(),
                    });
                    member.data.push(record.to_string());
                }
                None => return LmRc::InvalidState,
            }
        } else {
            handle.seq_data.push(record.to_string());
        }

        LmRc::Ok
    }

    // -------------------------------------------------------------------
    //  Member operations
    // -------------------------------------------------------------------

    /// LMMFIND — Locate a member and optionally retrieve statistics.
    ///
    /// Sets the current member for subsequent LMGET/LMPUT.
    pub fn lmmfind(
        &mut self,
        data_id: &str,
        member: &str,
        stats: bool,
    ) -> (LmRc, Option<MemberStats>) {
        let Some(handle) = self.handles.get_mut(data_id) else {
            return (LmRc::InvalidState, None);
        };
        if !handle.is_pds {
            return (LmRc::InvalidState, None);
        }

        let member_name = member.to_uppercase();
        if let Some(m) = handle.members.get(&member_name) {
            handle.current_member = Some(member_name);
            handle.read_cursor = 0;
            let s = if stats { m.stats.clone() } else { None };
            (LmRc::Ok, s)
        } else {
            (LmRc::EndOrNotFound, None)
        }
    }

    /// LMMADD — Add a new member to the PDS.
    pub fn lmmadd(&mut self, data_id: &str, member: &str) -> LmRc {
        let Some(handle) = self.handles.get_mut(data_id) else {
            return LmRc::InvalidState;
        };
        if !handle.is_pds || !matches!(handle.open_mode, Some(OpenMode::Output)) {
            return LmRc::InvalidState;
        }
        let name = member.to_uppercase();
        if handle.members.contains_key(&name) {
            return LmRc::Warning; // Member already exists.
        }
        handle.members.insert(
            name.clone(),
            PdsMember {
                name,
                stats: None,
                data: Vec::new(),
            },
        );
        LmRc::Ok
    }

    /// LMMDEL — Delete a member from the PDS.
    pub fn lmmdel(&mut self, data_id: &str, member: &str) -> LmRc {
        let Some(handle) = self.handles.get_mut(data_id) else {
            return LmRc::InvalidState;
        };
        if !handle.is_pds || !matches!(handle.open_mode, Some(OpenMode::Output | OpenMode::Update))
        {
            return LmRc::InvalidState;
        }
        let name = member.to_uppercase();
        if handle.members.remove(&name).is_some() {
            LmRc::Ok
        } else {
            LmRc::EndOrNotFound
        }
    }

    /// LMMREP — Replace a member in the PDS (sets as current member for LMPUT).
    pub fn lmmrep(&mut self, data_id: &str, member: &str) -> LmRc {
        let Some(handle) = self.handles.get_mut(data_id) else {
            return LmRc::InvalidState;
        };
        if !handle.is_pds || !matches!(handle.open_mode, Some(OpenMode::Output | OpenMode::Update))
        {
            return LmRc::InvalidState;
        }
        let name = member.to_uppercase();
        // Clear existing data if member exists, or create new.
        let m = handle.members.entry(name.clone()).or_insert_with(|| PdsMember {
            name: name.clone(),
            stats: None,
            data: Vec::new(),
        });
        m.data.clear();
        handle.current_member = Some(name);
        LmRc::Ok
    }

    /// LMMLIST — List PDS members one at a time.
    ///
    /// Returns the next member name in alphabetical order.
    /// Returns `EndOrNotFound` when all members have been listed.
    pub fn lmmlist(&mut self, data_id: &str) -> (LmRc, Option<String>) {
        let Some(handle) = self.handles.get_mut(data_id) else {
            return (LmRc::InvalidState, None);
        };
        if !handle.is_pds {
            return (LmRc::InvalidState, None);
        }

        // Rebuild cache if needed.
        if handle.member_list_cache.is_empty() || handle.member_list_cursor == 0 {
            handle.member_list_cache = handle.members.keys().cloned().collect();
            // BTreeMap keys are already sorted.
        }

        if handle.member_list_cursor >= handle.member_list_cache.len() {
            handle.member_list_cursor = 0; // Reset for next cycle.
            return (LmRc::EndOrNotFound, None);
        }

        let name = handle.member_list_cache[handle.member_list_cursor].clone();
        handle.member_list_cursor += 1;
        (LmRc::Ok, Some(name))
    }

    /// LMMSTATS — Set ISPF statistics for a member.
    pub fn lmmstats(&mut self, data_id: &str, member: &str, stats: MemberStats) -> LmRc {
        let Some(handle) = self.handles.get_mut(data_id) else {
            return LmRc::InvalidState;
        };
        if !handle.is_pds {
            return LmRc::InvalidState;
        }
        let name = member.to_uppercase();
        if let Some(m) = handle.members.get_mut(&name) {
            m.stats = Some(stats);
            LmRc::Ok
        } else {
            LmRc::EndOrNotFound
        }
    }

    // -------------------------------------------------------------------
    //  LIBDEF
    // -------------------------------------------------------------------

    /// LIBDEF — Override an ISPF library concatenation.
    ///
    /// If `stack` is true, pushes the override onto the stack. Otherwise replaces.
    pub fn libdef(
        &mut self,
        lib_type: LibdefType,
        datasets: Vec<String>,
        stack: bool,
    ) -> LmRc {
        let entry = LibdefEntry {
            lib_type,
            datasets: datasets.into_iter().map(|d| d.to_uppercase()).collect(),
        };

        let stack_vec = self.libdefs.entry(lib_type).or_default();
        if stack {
            stack_vec.insert(0, entry); // Push to front (most recent first).
        } else {
            stack_vec.clear();
            stack_vec.push(entry);
        }
        LmRc::Ok
    }

    /// Remove the most recent LIBDEF for a library type.
    pub fn libdef_remove(&mut self, lib_type: LibdefType) -> LmRc {
        if let Some(stack) = self.libdefs.get_mut(&lib_type) {
            if !stack.is_empty() {
                stack.remove(0);
                return LmRc::Ok;
            }
        }
        LmRc::Warning
    }

    /// Get the current LIBDEF search order for a library type.
    pub fn libdef_search(&self, lib_type: LibdefType) -> Vec<&str> {
        self.libdefs
            .get(&lib_type)
            .map(|stack| {
                stack
                    .iter()
                    .flat_map(|e| e.datasets.iter().map(|s| s.as_str()))
                    .collect()
            })
            .unwrap_or_default()
    }

    // -------------------------------------------------------------------
    //  Query helpers
    // -------------------------------------------------------------------

    /// Find an existing data ID by dataset name. Returns the first matching
    /// (non-open) handle, or `None` if no handle exists for `dsname`.
    pub fn find_data_id(&self, dsname: &str) -> Option<String> {
        let upper = dsname.to_uppercase();
        self.handles
            .iter()
            .find(|(_, h)| h.dsname == upper)
            .map(|(id, _)| id.clone())
    }

    /// Get the dataset name for a data ID.
    pub fn dataset_name(&self, data_id: &str) -> Option<&str> {
        self.handles.get(data_id).map(|h| h.dsname.as_str())
    }

    /// Check if a data ID is currently open.
    pub fn is_open(&self, data_id: &str) -> bool {
        self.handles
            .get(data_id)
            .map(|h| h.open_mode.is_some())
            .unwrap_or(false)
    }

    /// Get the number of members in a PDS data ID.
    pub fn member_count(&self, data_id: &str) -> Option<usize> {
        self.handles
            .get(data_id)
            .filter(|h| h.is_pds)
            .map(|h| h.members.len())
    }

    /// Seed a PDS member with data (for testing / pre-population).
    pub fn seed_member(&mut self, data_id: &str, name: &str, data: Vec<String>) -> LmRc {
        let Some(handle) = self.handles.get_mut(data_id) else {
            return LmRc::InvalidState;
        };
        if !handle.is_pds {
            return LmRc::InvalidState;
        }
        let name = name.to_uppercase();
        handle.members.insert(
            name.clone(),
            PdsMember {
                name,
                stats: None,
                data,
            },
        );
        LmRc::Ok
    }

    /// Seed sequential data (for testing / pre-population).
    pub fn seed_sequential(&mut self, data_id: &str, data: Vec<String>) -> LmRc {
        let Some(handle) = self.handles.get_mut(data_id) else {
            return LmRc::InvalidState;
        };
        if handle.is_pds {
            return LmRc::InvalidState;
        }
        handle.seq_data = data;
        LmRc::Ok
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // ─── T108.1: Dataset Open/Close and Record I/O ───

    #[test]
    fn test_lminit_lmfree() {
        let mut lm = LibraryManager::new();
        let (rc, id) = lm.lminit("SYS1.PARMLIB", true);
        assert_eq!(rc, LmRc::Ok);
        assert!(id.starts_with("LM"));

        assert_eq!(lm.dataset_name(&id), Some("SYS1.PARMLIB"));
        assert_eq!(lm.lmfree(&id), LmRc::Ok);
        assert_eq!(lm.dataset_name(&id), None);
    }

    #[test]
    fn test_lmfree_invalid() {
        let mut lm = LibraryManager::new();
        assert_eq!(lm.lmfree("NOSUCH"), LmRc::InvalidState);
    }

    #[test]
    fn test_lmopen_lmclose() {
        let mut lm = LibraryManager::new();
        let (_, id) = lm.lminit("MY.DATASET", false);

        assert_eq!(lm.lmopen(&id, OpenMode::Input), LmRc::Ok);
        assert!(lm.is_open(&id));

        // Double open fails.
        assert_eq!(lm.lmopen(&id, OpenMode::Input), LmRc::InvalidState);

        assert_eq!(lm.lmclose(&id), LmRc::Ok);
        assert!(!lm.is_open(&id));
    }

    #[test]
    fn test_lmclose_not_open() {
        let mut lm = LibraryManager::new();
        let (_, id) = lm.lminit("MY.DATASET", false);
        assert_eq!(lm.lmclose(&id), LmRc::InvalidState);
    }

    #[test]
    fn test_lmget_sequential() {
        let mut lm = LibraryManager::new();
        let (_, id) = lm.lminit("MY.SEQ", false);
        lm.seed_sequential(&id, vec!["LINE1".into(), "LINE2".into(), "LINE3".into()]);

        lm.lmopen(&id, OpenMode::Input);

        let (rc, rec) = lm.lmget(&id);
        assert_eq!(rc, LmRc::Ok);
        assert_eq!(rec.as_deref(), Some("LINE1"));

        let (rc, rec) = lm.lmget(&id);
        assert_eq!(rc, LmRc::Ok);
        assert_eq!(rec.as_deref(), Some("LINE2"));

        let (rc, rec) = lm.lmget(&id);
        assert_eq!(rc, LmRc::Ok);
        assert_eq!(rec.as_deref(), Some("LINE3"));

        let (rc, _) = lm.lmget(&id);
        assert_eq!(rc, LmRc::EndOrNotFound);
    }

    #[test]
    fn test_lmget_pds_member() {
        let mut lm = LibraryManager::new();
        let (_, id) = lm.lminit("SYS1.PARMLIB", true);
        lm.seed_member(&id, "IEASYS00", vec!["CLK=00".into(), "CON=00".into()]);

        lm.lmopen(&id, OpenMode::Input);

        // Must LMMFIND first.
        let (rc, _) = lm.lmget(&id);
        assert_eq!(rc, LmRc::InvalidState); // No current member.

        let (rc, _) = lm.lmmfind(&id, "IEASYS00", false);
        assert_eq!(rc, LmRc::Ok);

        let (rc, rec) = lm.lmget(&id);
        assert_eq!(rc, LmRc::Ok);
        assert_eq!(rec.as_deref(), Some("CLK=00"));

        let (rc, rec) = lm.lmget(&id);
        assert_eq!(rc, LmRc::Ok);
        assert_eq!(rec.as_deref(), Some("CON=00"));

        let (rc, _) = lm.lmget(&id);
        assert_eq!(rc, LmRc::EndOrNotFound);
    }

    #[test]
    fn test_lmput_sequential() {
        let mut lm = LibraryManager::new();
        let (_, id) = lm.lminit("MY.OUTPUT", false);
        lm.lmopen(&id, OpenMode::Output);

        assert_eq!(lm.lmput(&id, "RECORD 1"), LmRc::Ok);
        assert_eq!(lm.lmput(&id, "RECORD 2"), LmRc::Ok);

        lm.lmclose(&id);
        lm.lmopen(&id, OpenMode::Input);

        let (rc, rec) = lm.lmget(&id);
        assert_eq!(rc, LmRc::Ok);
        assert_eq!(rec.as_deref(), Some("RECORD 1"));
    }

    #[test]
    fn test_lmput_not_open_for_output() {
        let mut lm = LibraryManager::new();
        let (_, id) = lm.lminit("MY.SEQ", false);
        lm.lmopen(&id, OpenMode::Input);
        assert_eq!(lm.lmput(&id, "FAIL"), LmRc::InvalidState);
    }

    // ─── T108.2: Member Management and LIBDEF ───

    #[test]
    fn test_lmmfind_with_stats() {
        let mut lm = LibraryManager::new();
        let (_, id) = lm.lminit("MY.PDS", true);
        lm.seed_member(&id, "MYMBR", vec!["DATA".into()]);

        let stats = MemberStats {
            version: 1,
            modification: 5,
            mod_date: "2024/01/15".into(),
            mod_time: "10:30:00".into(),
            user: "USER01".into(),
            ..Default::default()
        };
        lm.lmmstats(&id, "MYMBR", stats);

        lm.lmopen(&id, OpenMode::Input);
        let (rc, st) = lm.lmmfind(&id, "MYMBR", true);
        assert_eq!(rc, LmRc::Ok);
        let st = st.unwrap();
        assert_eq!(st.version, 1);
        assert_eq!(st.modification, 5);
        assert_eq!(st.user, "USER01");
    }

    #[test]
    fn test_lmmfind_not_found() {
        let mut lm = LibraryManager::new();
        let (_, id) = lm.lminit("MY.PDS", true);
        lm.lmopen(&id, OpenMode::Input);

        let (rc, _) = lm.lmmfind(&id, "NOSUCH", false);
        assert_eq!(rc, LmRc::EndOrNotFound);
    }

    #[test]
    fn test_lmmadd_and_lmmdel() {
        let mut lm = LibraryManager::new();
        let (_, id) = lm.lminit("MY.PDS", true);
        lm.lmopen(&id, OpenMode::Output);

        assert_eq!(lm.lmmadd(&id, "NEWMBR"), LmRc::Ok);
        assert_eq!(lm.member_count(&id), Some(1));

        // Duplicate add returns warning.
        assert_eq!(lm.lmmadd(&id, "NEWMBR"), LmRc::Warning);

        assert_eq!(lm.lmmdel(&id, "NEWMBR"), LmRc::Ok);
        assert_eq!(lm.member_count(&id), Some(0));
    }

    #[test]
    fn test_lmmdel_not_found() {
        let mut lm = LibraryManager::new();
        let (_, id) = lm.lminit("MY.PDS", true);
        lm.lmopen(&id, OpenMode::Output);
        assert_eq!(lm.lmmdel(&id, "NOSUCH"), LmRc::EndOrNotFound);
    }

    #[test]
    fn test_lmmrep_and_write() {
        let mut lm = LibraryManager::new();
        let (_, id) = lm.lminit("MY.PDS", true);
        lm.seed_member(&id, "OLDMBR", vec!["OLD LINE 1".into(), "OLD LINE 2".into()]);

        lm.lmopen(&id, OpenMode::Output);
        assert_eq!(lm.lmmrep(&id, "OLDMBR"), LmRc::Ok);
        lm.lmput(&id, "NEW LINE 1");
        lm.lmclose(&id);

        lm.lmopen(&id, OpenMode::Input);
        lm.lmmfind(&id, "OLDMBR", false);
        let (_, rec) = lm.lmget(&id);
        assert_eq!(rec.as_deref(), Some("NEW LINE 1"));

        // Only 1 line now (old data cleared, 1 new line written).
        let (rc, _) = lm.lmget(&id);
        assert_eq!(rc, LmRc::EndOrNotFound);
    }

    #[test]
    fn test_lmmlist() {
        let mut lm = LibraryManager::new();
        let (_, id) = lm.lminit("MY.PDS", true);
        lm.seed_member(&id, "ALPHA", vec![]);
        lm.seed_member(&id, "CHARLIE", vec![]);
        lm.seed_member(&id, "BRAVO", vec![]);

        // Members should be listed in alphabetical order.
        let (rc, name) = lm.lmmlist(&id);
        assert_eq!(rc, LmRc::Ok);
        assert_eq!(name.as_deref(), Some("ALPHA"));

        let (rc, name) = lm.lmmlist(&id);
        assert_eq!(rc, LmRc::Ok);
        assert_eq!(name.as_deref(), Some("BRAVO"));

        let (rc, name) = lm.lmmlist(&id);
        assert_eq!(rc, LmRc::Ok);
        assert_eq!(name.as_deref(), Some("CHARLIE"));

        // End of list.
        let (rc, _) = lm.lmmlist(&id);
        assert_eq!(rc, LmRc::EndOrNotFound);
    }

    #[test]
    fn test_lmmstats() {
        let mut lm = LibraryManager::new();
        let (_, id) = lm.lminit("MY.PDS", true);
        lm.seed_member(&id, "MBR1", vec![]);

        let stats = MemberStats {
            version: 2,
            modification: 10,
            mod_date: "2024/06/01".into(),
            mod_time: "14:00:00".into(),
            user: "ADMIN".into(),
            current_lines: 100,
            initial_lines: 50,
            modified_lines: 50,
            create_date: "2024/01/01".into(),
        };
        assert_eq!(lm.lmmstats(&id, "MBR1", stats), LmRc::Ok);
        assert_eq!(lm.lmmstats(&id, "NOSUCH", MemberStats::default()), LmRc::EndOrNotFound);
    }

    // ─── LIBDEF ───

    #[test]
    fn test_libdef_stack() {
        let mut lm = LibraryManager::new();

        lm.libdef(
            LibdefType::Ispplib,
            vec!["SYS1.PANELS".into()],
            false,
        );
        assert_eq!(lm.libdef_search(LibdefType::Ispplib), vec!["SYS1.PANELS"]);

        // Stack another.
        lm.libdef(
            LibdefType::Ispplib,
            vec!["USER01.PANELS".into()],
            true,
        );
        assert_eq!(
            lm.libdef_search(LibdefType::Ispplib),
            vec!["USER01.PANELS", "SYS1.PANELS"]
        );
    }

    #[test]
    fn test_libdef_replace() {
        let mut lm = LibraryManager::new();
        lm.libdef(LibdefType::Ispmlib, vec!["OLD.MSGS".into()], false);
        lm.libdef(LibdefType::Ispmlib, vec!["NEW.MSGS".into()], false);

        assert_eq!(lm.libdef_search(LibdefType::Ispmlib), vec!["NEW.MSGS"]);
    }

    #[test]
    fn test_libdef_remove() {
        let mut lm = LibraryManager::new();
        lm.libdef(LibdefType::Ispslib, vec!["A.SKELS".into()], false);
        lm.libdef(LibdefType::Ispslib, vec!["B.SKELS".into()], true);

        assert_eq!(lm.libdef_remove(LibdefType::Ispslib), LmRc::Ok);
        assert_eq!(lm.libdef_search(LibdefType::Ispslib), vec!["A.SKELS"]);
    }

    #[test]
    fn test_libdef_remove_empty() {
        let mut lm = LibraryManager::new();
        assert_eq!(lm.libdef_remove(LibdefType::Ispllib), LmRc::Warning);
    }

    #[test]
    fn test_libdef_type_display() {
        assert_eq!(LibdefType::Ispplib.to_string(), "ISPPLIB");
        assert_eq!(LibdefType::Ispmlib.to_string(), "ISPMLIB");
        assert_eq!(LibdefType::Ispslib.to_string(), "ISPSLIB");
        assert_eq!(LibdefType::Isptlib.to_string(), "ISPTLIB");
        assert_eq!(LibdefType::Ispllib.to_string(), "ISPLLIB");
        assert_eq!(LibdefType::Isptabl.to_string(), "ISPTABL");
    }

    #[test]
    fn test_lm_rc_codes() {
        assert_eq!(LmRc::Ok.code(), 0);
        assert_eq!(LmRc::Warning.code(), 4);
        assert_eq!(LmRc::EndOrNotFound.code(), 8);
        assert_eq!(LmRc::InvalidState.code(), 12);
        assert_eq!(LmRc::Severe.code(), 20);
    }

    #[test]
    fn test_full_lifecycle() {
        let mut lm = LibraryManager::new();

        // LMINIT
        let (rc, id) = lm.lminit("SYS1.PARMLIB", true);
        assert_eq!(rc, LmRc::Ok);

        // Seed test data.
        lm.seed_member(&id, "IEASYS00", vec!["CLK=00".into(), "CON=00".into()]);

        // LMOPEN INPUT
        assert_eq!(lm.lmopen(&id, OpenMode::Input), LmRc::Ok);

        // LMMFIND
        let (rc, _) = lm.lmmfind(&id, "IEASYS00", false);
        assert_eq!(rc, LmRc::Ok);

        // LMGET records
        let (rc, rec) = lm.lmget(&id);
        assert_eq!(rc, LmRc::Ok);
        assert_eq!(rec.as_deref(), Some("CLK=00"));

        let (rc, rec) = lm.lmget(&id);
        assert_eq!(rc, LmRc::Ok);
        assert_eq!(rec.as_deref(), Some("CON=00"));

        // End of data
        let (rc, _) = lm.lmget(&id);
        assert_eq!(rc, LmRc::EndOrNotFound);

        // LMCLOSE
        assert_eq!(lm.lmclose(&id), LmRc::Ok);

        // LMFREE
        assert_eq!(lm.lmfree(&id), LmRc::Ok);
    }

    #[test]
    fn test_seed_sequential_on_pds_fails() {
        let mut lm = LibraryManager::new();
        let (_, id) = lm.lminit("MY.PDS", true);
        assert_eq!(
            lm.seed_sequential(&id, vec!["DATA".into()]),
            LmRc::InvalidState
        );
    }

    #[test]
    fn test_seed_member_on_seq_fails() {
        let mut lm = LibraryManager::new();
        let (_, id) = lm.lminit("MY.SEQ", false);
        assert_eq!(
            lm.seed_member(&id, "MBR", vec!["DATA".into()]),
            LmRc::InvalidState
        );
    }
}
