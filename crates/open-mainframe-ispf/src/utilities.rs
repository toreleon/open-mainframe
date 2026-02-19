//! ISPF Utilities (Option 3.x) — dataset and member management utilities.
//!
//! Implements the core ISPF 3.x utility functions:
//! - **3.1 Library Utility** — PDS member list with browse/edit/delete/rename/copy
//! - **3.2 Dataset Utility** — allocate, rename, delete datasets
//! - **3.3 Move/Copy** — copy or move members/datasets
//! - **3.4 Dataset List (DSLIST)** — list datasets matching a pattern
//! - **3.12 SuperC** — line-by-line dataset comparison
//! - **3.14 Search-For** — string search across datasets

use crate::library::{LibraryManager, LmRc, MemberStats, OpenMode};

// ---------------------------------------------------------------------------
//  Return codes
// ---------------------------------------------------------------------------

/// Return code from utility operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UtilRc {
    /// Operation completed normally.
    Ok,
    /// Warning (e.g., partial result, member already exists).
    Warning,
    /// Not found / end of data.
    NotFound,
    /// Error (invalid input, permission, etc.).
    Error,
}

// ---------------------------------------------------------------------------
//  3.1 — Library Utility
// ---------------------------------------------------------------------------

/// A row in the member list (Option 3.1).
#[derive(Debug, Clone)]
pub struct MemberListEntry {
    pub name: String,
    pub stats: Option<MemberStats>,
}

/// Line command for member list rows.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemberCmd {
    /// E — Edit the member.
    Edit,
    /// V — View (browse read-only).
    View,
    /// B — Browse.
    Browse,
    /// D — Delete the member.
    Delete,
    /// R — Rename the member.
    Rename,
    /// S — Select (same as Edit).
    Select,
    /// C — Copy member to another PDS.
    Copy,
    /// P — Print the member.
    Print,
}

/// Parse a single-character member line command.
pub fn parse_member_cmd(cmd: &str) -> Option<MemberCmd> {
    match cmd.trim().to_uppercase().as_str() {
        "E" => Some(MemberCmd::Edit),
        "V" => Some(MemberCmd::View),
        "B" => Some(MemberCmd::Browse),
        "D" => Some(MemberCmd::Delete),
        "R" => Some(MemberCmd::Rename),
        "S" => Some(MemberCmd::Select),
        "C" => Some(MemberCmd::Copy),
        "P" => Some(MemberCmd::Print),
        _ => None,
    }
}

/// Obtain a data ID for `dsname`, reusing an existing handle if available.
fn obtain_id(lib: &mut LibraryManager, dsname: &str, is_pds: bool) -> (LmRc, String) {
    if let Some(id) = lib.find_data_id(dsname) {
        (LmRc::Ok, id)
    } else {
        lib.lminit(dsname, is_pds)
    }
}

/// List members of a PDS with optional ISPF statistics (Option 3.1).
pub fn library_list(lib: &mut LibraryManager, dsname: &str) -> (UtilRc, Vec<MemberListEntry>) {
    let (rc, data_id) = obtain_id(lib, dsname, true);
    if rc != LmRc::Ok {
        return (UtilRc::Error, Vec::new());
    }

    if lib.lmopen(&data_id, OpenMode::Input) != LmRc::Ok {
        return (UtilRc::Error, Vec::new());
    }

    let mut members = Vec::new();
    loop {
        let (rc, name) = lib.lmmlist(&data_id);
        match rc {
            LmRc::Ok => {
                let name = name.unwrap_or_default();
                let (_, stats) = lib.lmmfind(&data_id, &name, true);
                members.push(MemberListEntry { name, stats });
            }
            _ => break,
        }
    }

    lib.lmclose(&data_id);

    if members.is_empty() {
        (UtilRc::Warning, members)
    } else {
        (UtilRc::Ok, members)
    }
}

/// Execute a member line command (Option 3.1).
pub fn library_member_action(
    lib: &mut LibraryManager,
    dsname: &str,
    member: &str,
    cmd: MemberCmd,
    rename_to: Option<&str>,
) -> UtilRc {
    match cmd {
        MemberCmd::Delete => {
            let (rc, data_id) = obtain_id(lib, dsname, true);
            if rc != LmRc::Ok {
                return UtilRc::Error;
            }
            if lib.lmopen(&data_id, OpenMode::Update) != LmRc::Ok {
                return UtilRc::Error;
            }
            let del_rc = lib.lmmdel(&data_id, member);
            lib.lmclose(&data_id);
            if del_rc == LmRc::Ok {
                UtilRc::Ok
            } else {
                UtilRc::NotFound
            }
        }
        MemberCmd::Rename => {
            let new_name = match rename_to {
                Some(n) => n,
                None => return UtilRc::Error,
            };
            let (rc, data_id) = obtain_id(lib, dsname, true);
            if rc != LmRc::Ok {
                return UtilRc::Error;
            }
            if lib.lmopen(&data_id, OpenMode::Update) != LmRc::Ok {
                return UtilRc::Error;
            }
            // Read member data, add under new name, delete old.
            let (find_rc, _) = lib.lmmfind(&data_id, member, false);
            if find_rc != LmRc::Ok {
                lib.lmclose(&data_id);
                return UtilRc::NotFound;
            }
            // Collect data.
            let mut lines = Vec::new();
            loop {
                let (rc, line) = lib.lmget(&data_id);
                if rc != LmRc::Ok {
                    break;
                }
                if let Some(l) = line {
                    lines.push(l);
                }
            }
            // Seed new member and delete old.
            lib.seed_member(&data_id, new_name, lines);
            lib.lmmdel(&data_id, member);
            lib.lmclose(&data_id);
            UtilRc::Ok
        }
        // Edit, View, Browse, Select, Copy, Print return Ok to indicate the
        // action is recognized; actual editing/viewing is handled by the
        // caller (Editor or panel display).
        MemberCmd::Edit
        | MemberCmd::View
        | MemberCmd::Browse
        | MemberCmd::Select
        | MemberCmd::Copy
        | MemberCmd::Print => UtilRc::Ok,
    }
}

// ---------------------------------------------------------------------------
//  3.2 — Dataset Utility
// ---------------------------------------------------------------------------

/// Action for Dataset Utility (3.2).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DatasetAction {
    /// A — Allocate a new dataset.
    Allocate,
    /// D — Delete a dataset.
    Delete,
    /// R — Rename a dataset.
    Rename,
    /// I — Information (display attributes).
    Info,
}

/// Simple allocation parameters.
#[derive(Debug, Clone)]
pub struct AllocParams {
    pub dsname: String,
    pub is_pds: bool,
    pub lrecl: u32,
    pub recfm: String,
    pub blksize: u32,
}

impl Default for AllocParams {
    fn default() -> Self {
        Self {
            dsname: String::new(),
            is_pds: false,
            lrecl: 80,
            recfm: "FB".to_string(),
            blksize: 27920,
        }
    }
}

/// Dataset information returned by Info action.
#[derive(Debug, Clone)]
pub struct DatasetInfo {
    pub dsname: String,
    pub is_pds: bool,
    pub member_count: Option<usize>,
    pub is_open: bool,
}

/// Execute a Dataset Utility (3.2) action.
pub fn dataset_action(
    lib: &mut LibraryManager,
    action: DatasetAction,
    dsname: &str,
    alloc_params: Option<&AllocParams>,
    rename_to: Option<&str>,
) -> (UtilRc, Option<DatasetInfo>) {
    match action {
        DatasetAction::Allocate => {
            let params = match alloc_params {
                Some(p) => p,
                None => return (UtilRc::Error, None),
            };
            let (rc, _data_id) = lib.lminit(&params.dsname, params.is_pds);
            if rc == LmRc::Ok {
                (UtilRc::Ok, None)
            } else {
                (UtilRc::Error, None)
            }
        }
        DatasetAction::Delete => {
            let (rc, data_id) = obtain_id(lib, dsname, false);
            if rc != LmRc::Ok {
                return (UtilRc::NotFound, None);
            }
            lib.lmfree(&data_id);
            (UtilRc::Ok, None)
        }
        DatasetAction::Rename => {
            let new_name = match rename_to {
                Some(n) => n,
                None => return (UtilRc::Error, None),
            };
            let (rc, old_id) = obtain_id(lib, dsname, false);
            if rc != LmRc::Ok {
                return (UtilRc::NotFound, None);
            }
            if lib.lmopen(&old_id, OpenMode::Input) != LmRc::Ok {
                return (UtilRc::Error, None);
            }
            let mut lines = Vec::new();
            loop {
                let (rc, line) = lib.lmget(&old_id);
                if rc != LmRc::Ok {
                    break;
                }
                if let Some(l) = line {
                    lines.push(l);
                }
            }
            lib.lmclose(&old_id);
            lib.lmfree(&old_id);

            let (rc2, new_id) = lib.lminit(new_name, false);
            if rc2 != LmRc::Ok {
                return (UtilRc::Error, None);
            }
            if lib.lmopen(&new_id, OpenMode::Output) != LmRc::Ok {
                return (UtilRc::Error, None);
            }
            lib.seed_sequential(&new_id, lines);
            lib.lmclose(&new_id);
            (UtilRc::Ok, None)
        }
        DatasetAction::Info => {
            let (rc, data_id) = obtain_id(lib, dsname, false);
            if rc != LmRc::Ok {
                return (UtilRc::NotFound, None);
            }
            let info = DatasetInfo {
                dsname: dsname.to_string(),
                is_pds: lib.member_count(&data_id).is_some(),
                member_count: lib.member_count(&data_id),
                is_open: lib.is_open(&data_id),
            };
            (UtilRc::Ok, Some(info))
        }
    }
}

// ---------------------------------------------------------------------------
//  3.3 — Move/Copy Utility
// ---------------------------------------------------------------------------

/// Mode for Move/Copy utility.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TransferMode {
    Copy,
    Move,
}

/// Copy or move members between two PDS datasets (Option 3.3).
pub fn move_copy_members(
    lib: &mut LibraryManager,
    source_ds: &str,
    target_ds: &str,
    members: &[&str],
    mode: TransferMode,
    replace: bool,
) -> (UtilRc, usize) {
    let (rc_s, src_id) = obtain_id(lib, source_ds, true);
    if rc_s != LmRc::Ok {
        return (UtilRc::Error, 0);
    }
    let (rc_t, tgt_id) = obtain_id(lib, target_ds, true);
    if rc_t != LmRc::Ok {
        return (UtilRc::Error, 0);
    }

    if lib.lmopen(&src_id, OpenMode::Input) != LmRc::Ok {
        return (UtilRc::Error, 0);
    }
    if lib.lmopen(&tgt_id, OpenMode::Update) != LmRc::Ok {
        lib.lmclose(&src_id);
        return (UtilRc::Error, 0);
    }

    let mut count = 0usize;
    for &mbr in members {
        let (find_rc, _) = lib.lmmfind(&src_id, mbr, false);
        if find_rc != LmRc::Ok {
            continue;
        }
        // Read member data.
        let mut data = Vec::new();
        loop {
            let (rc, line) = lib.lmget(&src_id);
            if rc != LmRc::Ok {
                break;
            }
            if let Some(l) = line {
                data.push(l);
            }
        }
        // Check if target already has this member.
        let (tgt_find, _) = lib.lmmfind(&tgt_id, mbr, false);
        if tgt_find == LmRc::Ok && !replace {
            continue; // Skip — no replace flag.
        }
        if tgt_find == LmRc::Ok {
            lib.lmmdel(&tgt_id, mbr);
        }
        lib.seed_member(&tgt_id, mbr, data);
        count += 1;

        // If move, delete from source.
        if mode == TransferMode::Move {
            lib.lmmdel(&src_id, mbr);
        }
    }

    lib.lmclose(&src_id);
    lib.lmclose(&tgt_id);

    if count > 0 {
        (UtilRc::Ok, count)
    } else {
        (UtilRc::Warning, 0)
    }
}

// ---------------------------------------------------------------------------
//  3.4 — Dataset List (DSLIST)
// ---------------------------------------------------------------------------

/// A row in the DSLIST output (Option 3.4).
#[derive(Debug, Clone)]
pub struct DslistEntry {
    pub dsname: String,
    pub volume: String,
    pub device: String,
    pub dsorg: String,
    pub recfm: String,
    pub lrecl: u32,
}

/// Line command on a DSLIST row.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DslistCmd {
    /// E — Edit.
    Edit,
    /// B — Browse.
    Browse,
    /// D — Delete.
    Delete,
    /// I — Information.
    Info,
    /// M — Member list.
    MemberList,
    /// R — Rename.
    Rename,
    /// C — Catalog.
    Catalog,
    /// U — Uncatalog.
    Uncatalog,
}

/// Parse a DSLIST line command.
pub fn parse_dslist_cmd(cmd: &str) -> Option<DslistCmd> {
    match cmd.trim().to_uppercase().as_str() {
        "E" => Some(DslistCmd::Edit),
        "B" => Some(DslistCmd::Browse),
        "D" => Some(DslistCmd::Delete),
        "I" => Some(DslistCmd::Info),
        "M" => Some(DslistCmd::MemberList),
        "R" => Some(DslistCmd::Rename),
        "C" => Some(DslistCmd::Catalog),
        "U" => Some(DslistCmd::Uncatalog),
        _ => None,
    }
}

/// Match a dataset name against a pattern with `*` and `**` wildcards.
///
/// - `*` matches a single qualifier (no dots).
/// - `**` matches zero or more qualifiers.
fn dsname_matches(pattern: &str, dsname: &str) -> bool {
    let pat_parts: Vec<&str> = pattern.split('.').collect();
    let ds_parts: Vec<&str> = dsname.split('.').collect();
    match_parts(&pat_parts, &ds_parts)
}

fn match_parts(pat: &[&str], ds: &[&str]) -> bool {
    if pat.is_empty() {
        return ds.is_empty();
    }
    if pat[0] == "**" {
        // Match zero or more qualifiers.
        for skip in 0..=ds.len() {
            if match_parts(&pat[1..], &ds[skip..]) {
                return true;
            }
        }
        return false;
    }
    if ds.is_empty() {
        return false;
    }
    if pat[0] == "*" || pat[0].eq_ignore_ascii_case(ds[0]) {
        return match_parts(&pat[1..], &ds[1..]);
    }
    false
}

/// List datasets matching a pattern (Option 3.4).
///
/// `known_datasets` should be the list of all cataloged dataset names;
/// the function filters those matching `pattern`.
pub fn dslist(
    pattern: &str,
    known_datasets: &[&str],
) -> Vec<DslistEntry> {
    let pat_upper = pattern.to_uppercase();
    let mut result = Vec::new();
    for &ds in known_datasets {
        let ds_upper = ds.to_uppercase();
        if dsname_matches(&pat_upper, &ds_upper) {
            result.push(DslistEntry {
                dsname: ds.to_string(),
                volume: "WORK01".to_string(),
                device: "3390".to_string(),
                dsorg: "PO".to_string(),
                recfm: "FB".to_string(),
                lrecl: 80,
            });
        }
    }
    result
}

// ---------------------------------------------------------------------------
//  3.12 — SuperC (Compare)
// ---------------------------------------------------------------------------

/// A difference chunk from SuperC comparison.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiffKind {
    /// Lines only in the old dataset.
    Deleted,
    /// Lines only in the new dataset.
    Inserted,
    /// Lines changed between old and new.
    Changed,
}

/// A single difference in a SuperC listing.
#[derive(Debug, Clone)]
pub struct DiffEntry {
    pub kind: DiffKind,
    pub old_start: usize,
    pub old_count: usize,
    pub new_start: usize,
    pub new_count: usize,
    pub old_lines: Vec<String>,
    pub new_lines: Vec<String>,
}

/// Compare two datasets line-by-line (Option 3.12 — SuperC).
///
/// Uses a simple LCS-based diff to produce a list of difference entries.
pub fn superc_compare(old: &[String], new: &[String]) -> Vec<DiffEntry> {
    // Build LCS table.
    let m = old.len();
    let n = new.len();
    let mut dp = vec![vec![0u32; n + 1]; m + 1];
    for i in (0..m).rev() {
        for j in (0..n).rev() {
            if old[i] == new[j] {
                dp[i][j] = dp[i + 1][j + 1] + 1;
            } else {
                dp[i][j] = dp[i + 1][j].max(dp[i][j + 1]);
            }
        }
    }

    // Walk the DP table to extract diffs.
    let mut diffs = Vec::new();
    let mut i = 0;
    let mut j = 0;
    while i < m || j < n {
        if i < m && j < n && old[i] == new[j] {
            i += 1;
            j += 1;
        } else {
            let old_start = i;
            let new_start = j;
            let mut del_lines = Vec::new();
            let mut ins_lines = Vec::new();

            // Consume differing lines.
            while i < m && (j >= n || dp[i + 1][j] >= dp[i][j]) && old[i] != *new.get(j).unwrap_or(&String::new()) {
                del_lines.push(old[i].clone());
                i += 1;
            }
            while j < n && (i >= m || dp[i][j + 1] >= dp[i][j]) && new[j] != *old.get(i).unwrap_or(&String::new()) {
                ins_lines.push(new[j].clone());
                j += 1;
            }

            let kind = if !del_lines.is_empty() && !ins_lines.is_empty() {
                DiffKind::Changed
            } else if !del_lines.is_empty() {
                DiffKind::Deleted
            } else {
                DiffKind::Inserted
            };

            diffs.push(DiffEntry {
                kind,
                old_start: old_start + 1,
                old_count: del_lines.len(),
                new_start: new_start + 1,
                new_count: ins_lines.len(),
                old_lines: del_lines,
                new_lines: ins_lines,
            });
        }
    }
    diffs
}

/// Format SuperC comparison output as a printable listing.
pub fn superc_listing(old_name: &str, new_name: &str, diffs: &[DiffEntry]) -> Vec<String> {
    let mut out = Vec::new();
    out.push("ISRSUPC - MVS/PDF FILE/LINE/WORD/BYTE COMPARE UTILITY".to_string());
    out.push(format!("OLD: {old_name}"));
    out.push(format!("NEW: {new_name}"));
    out.push(String::new());

    if diffs.is_empty() {
        out.push("THE FILES ARE IDENTICAL.".to_string());
        return out;
    }

    out.push(format!("{} DIFFERENCE(S) FOUND.", diffs.len()));
    out.push(String::new());

    for (idx, d) in diffs.iter().enumerate() {
        out.push(format!("--- DIFFERENCE {} ---", idx + 1));
        match d.kind {
            DiffKind::Deleted => {
                out.push(format!(
                    "OLD LINE(S) {}-{} DELETED",
                    d.old_start,
                    d.old_start + d.old_count - 1
                ));
                for l in &d.old_lines {
                    out.push(format!("< {l}"));
                }
            }
            DiffKind::Inserted => {
                out.push(format!(
                    "NEW LINE(S) {}-{} INSERTED",
                    d.new_start,
                    d.new_start + d.new_count - 1
                ));
                for l in &d.new_lines {
                    out.push(format!("> {l}"));
                }
            }
            DiffKind::Changed => {
                out.push(format!(
                    "CHANGED OLD {}-{} NEW {}-{}",
                    d.old_start,
                    d.old_start + d.old_count - 1,
                    d.new_start,
                    d.new_start + d.new_count - 1
                ));
                for l in &d.old_lines {
                    out.push(format!("< {l}"));
                }
                for l in &d.new_lines {
                    out.push(format!("> {l}"));
                }
            }
        }
        out.push(String::new());
    }
    out
}

// ---------------------------------------------------------------------------
//  3.14 — Search-For (SRCHFOR)
// ---------------------------------------------------------------------------

/// A single search hit from Search-For.
#[derive(Debug, Clone)]
pub struct SearchHit {
    pub dsname: String,
    pub member: Option<String>,
    pub line_num: usize,
    pub line_text: String,
}

/// Search for a string across multiple datasets/members (Option 3.14).
pub fn search_for(
    lib: &mut LibraryManager,
    datasets: &[&str],
    search_string: &str,
    case_sensitive: bool,
) -> Vec<SearchHit> {
    let mut hits = Vec::new();
    let needle = if case_sensitive {
        search_string.to_string()
    } else {
        search_string.to_uppercase()
    };

    for &dsname in datasets {
        let (rc, data_id) = obtain_id(lib, dsname, true);
        if rc != LmRc::Ok {
            // Try as sequential.
            search_sequential(lib, dsname, &needle, case_sensitive, &mut hits);
            continue;
        }
        if lib.lmopen(&data_id, OpenMode::Input) != LmRc::Ok {
            continue;
        }

        // Try to enumerate members.
        let mut member_names = Vec::new();
        loop {
            let (rc, name) = lib.lmmlist(&data_id);
            if rc != LmRc::Ok {
                break;
            }
            if let Some(n) = name {
                member_names.push(n);
            }
        }

        if member_names.is_empty() {
            // Sequential dataset — read records.
            let mut line_num = 0usize;
            loop {
                let (rc, line) = lib.lmget(&data_id);
                if rc != LmRc::Ok {
                    break;
                }
                if let Some(l) = line {
                    line_num += 1;
                    let hay = if case_sensitive { l.clone() } else { l.to_uppercase() };
                    if hay.contains(&needle) {
                        hits.push(SearchHit {
                            dsname: dsname.to_string(),
                            member: None,
                            line_num,
                            line_text: l,
                        });
                    }
                }
            }
        } else {
            // PDS — search each member.
            for mbr in &member_names {
                let (find_rc, _) = lib.lmmfind(&data_id, mbr, false);
                if find_rc != LmRc::Ok {
                    continue;
                }
                let mut line_num = 0usize;
                loop {
                    let (rc, line) = lib.lmget(&data_id);
                    if rc != LmRc::Ok {
                        break;
                    }
                    if let Some(l) = line {
                        line_num += 1;
                        let hay = if case_sensitive { l.clone() } else { l.to_uppercase() };
                        if hay.contains(&needle) {
                            hits.push(SearchHit {
                                dsname: dsname.to_string(),
                                member: Some(mbr.clone()),
                                line_num,
                                line_text: l,
                            });
                        }
                    }
                }
            }
        }

        lib.lmclose(&data_id);
    }
    hits
}

fn search_sequential(
    lib: &mut LibraryManager,
    dsname: &str,
    needle: &str,
    case_sensitive: bool,
    hits: &mut Vec<SearchHit>,
) {
    let (rc, data_id) = obtain_id(lib, dsname, false);
    if rc != LmRc::Ok {
        return;
    }
    if lib.lmopen(&data_id, OpenMode::Input) != LmRc::Ok {
        return;
    }
    let mut line_num = 0usize;
    loop {
        let (rc, line) = lib.lmget(&data_id);
        if rc != LmRc::Ok {
            break;
        }
        if let Some(l) = line {
            line_num += 1;
            let hay = if case_sensitive { l.clone() } else { l.to_uppercase() };
            if hay.contains(needle) {
                hits.push(SearchHit {
                    dsname: dsname.to_string(),
                    member: None,
                    line_num,
                    line_text: l,
                });
            }
        }
    }
    lib.lmclose(&data_id);
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -- Helpers --

    fn make_lib_with_pds(dsname: &str, members: &[(&str, &[&str])]) -> (LibraryManager, String) {
        let mut lib = LibraryManager::new();
        let (_, data_id) = lib.lminit(dsname, true);
        lib.lmopen(&data_id, OpenMode::Output);
        for (name, lines) in members {
            let data: Vec<String> = lines.iter().map(|s| s.to_string()).collect();
            lib.seed_member(&data_id, name, data);
        }
        lib.lmclose(&data_id);
        // Do NOT lmfree — keep the handle alive so utility functions can find it.
        (lib, dsname.to_string())
    }

    // -- 3.1 Library Utility --

    #[test]
    fn test_library_list() {
        let (mut lib, dsname) = make_lib_with_pds("USER.SRC", &[
            ("MOD1", &["LINE A"]),
            ("MOD2", &["LINE B", "LINE C"]),
        ]);
        let (rc, members) = library_list(&mut lib, &dsname);
        assert_eq!(rc, UtilRc::Ok);
        assert_eq!(members.len(), 2);
        let names: Vec<&str> = members.iter().map(|m| m.name.as_str()).collect();
        assert!(names.contains(&"MOD1"));
        assert!(names.contains(&"MOD2"));
    }

    #[test]
    fn test_library_list_empty_pds() {
        let mut lib = LibraryManager::new();
        let (_, data_id) = lib.lminit("EMPTY.PDS", true);
        lib.lmopen(&data_id, OpenMode::Output);
        lib.lmclose(&data_id);
        lib.lmfree(&data_id);

        let (rc, members) = library_list(&mut lib, "EMPTY.PDS");
        assert_eq!(rc, UtilRc::Warning);
        assert!(members.is_empty());
    }

    #[test]
    fn test_member_cmd_delete() {
        let (mut lib, dsname) = make_lib_with_pds("TEST.PDS", &[
            ("DELME", &["DATA"]),
        ]);
        let rc = library_member_action(&mut lib, &dsname, "DELME", MemberCmd::Delete, None);
        assert_eq!(rc, UtilRc::Ok);
    }

    #[test]
    fn test_member_cmd_rename() {
        let (mut lib, dsname) = make_lib_with_pds("TEST.PDS", &[
            ("OLDNAME", &["CONTENT"]),
        ]);
        let rc = library_member_action(
            &mut lib, &dsname, "OLDNAME", MemberCmd::Rename, Some("NEWNAME"),
        );
        assert_eq!(rc, UtilRc::Ok);
    }

    #[test]
    fn test_member_cmd_parse() {
        assert_eq!(parse_member_cmd("E"), Some(MemberCmd::Edit));
        assert_eq!(parse_member_cmd("v"), Some(MemberCmd::View));
        assert_eq!(parse_member_cmd("d"), Some(MemberCmd::Delete));
        assert_eq!(parse_member_cmd("Z"), None);
    }

    // -- 3.2 Dataset Utility --

    #[test]
    fn test_dataset_allocate() {
        let mut lib = LibraryManager::new();
        let params = AllocParams {
            dsname: "NEW.DATA".to_string(),
            is_pds: false,
            ..Default::default()
        };
        let (rc, _) = dataset_action(
            &mut lib, DatasetAction::Allocate, "", Some(&params), None,
        );
        assert_eq!(rc, UtilRc::Ok);
    }

    #[test]
    fn test_dataset_info() {
        let (mut lib, dsname) = make_lib_with_pds("INFO.PDS", &[
            ("MBR1", &["LINE1"]),
        ]);
        let (rc, info) = dataset_action(
            &mut lib, DatasetAction::Info, &dsname, None, None,
        );
        assert_eq!(rc, UtilRc::Ok);
        let info = info.unwrap();
        assert_eq!(info.dsname, "INFO.PDS");
    }

    // -- 3.3 Move/Copy --

    #[test]
    fn test_copy_members() {
        let (mut lib, _) = make_lib_with_pds("SRC.PDS", &[
            ("A", &["LINE1"]),
            ("B", &["LINE2"]),
        ]);
        // Create target (keep handle alive).
        let (_, tgt_id) = lib.lminit("TGT.PDS", true);
        lib.lmopen(&tgt_id, OpenMode::Output);
        lib.lmclose(&tgt_id);

        let (rc, count) = move_copy_members(
            &mut lib, "SRC.PDS", "TGT.PDS", &["A", "B"], TransferMode::Copy, false,
        );
        assert_eq!(rc, UtilRc::Ok);
        assert_eq!(count, 2);
    }

    #[test]
    fn test_move_members() {
        let (mut lib, _) = make_lib_with_pds("SRC.PDS", &[
            ("X", &["DATA"]),
        ]);
        let (_, tgt_id) = lib.lminit("TGT.PDS", true);
        lib.lmopen(&tgt_id, OpenMode::Output);
        lib.lmclose(&tgt_id);

        let (rc, count) = move_copy_members(
            &mut lib, "SRC.PDS", "TGT.PDS", &["X"], TransferMode::Move, false,
        );
        assert_eq!(rc, UtilRc::Ok);
        assert_eq!(count, 1);
    }

    #[test]
    fn test_copy_no_replace() {
        let (mut lib, _) = make_lib_with_pds("SRC.PDS", &[
            ("DUP", &["SRC DATA"]),
        ]);
        let (_, tgt_id) = lib.lminit("TGT.PDS", true);
        lib.lmopen(&tgt_id, OpenMode::Output);
        lib.seed_member(&tgt_id, "DUP", vec!["TGT DATA".to_string()]);
        lib.lmclose(&tgt_id);

        let (rc, count) = move_copy_members(
            &mut lib, "SRC.PDS", "TGT.PDS", &["DUP"], TransferMode::Copy, false,
        );
        assert_eq!(rc, UtilRc::Warning);
        assert_eq!(count, 0);
    }

    #[test]
    fn test_copy_with_replace() {
        let (mut lib, _) = make_lib_with_pds("SRC.PDS", &[
            ("DUP", &["SRC DATA"]),
        ]);
        let (_, tgt_id) = lib.lminit("TGT.PDS", true);
        lib.lmopen(&tgt_id, OpenMode::Output);
        lib.seed_member(&tgt_id, "DUP", vec!["TGT DATA".to_string()]);
        lib.lmclose(&tgt_id);

        let (rc, count) = move_copy_members(
            &mut lib, "SRC.PDS", "TGT.PDS", &["DUP"], TransferMode::Copy, true,
        );
        assert_eq!(rc, UtilRc::Ok);
        assert_eq!(count, 1);
    }

    // -- 3.4 DSLIST --

    #[test]
    fn test_dslist_wildcard() {
        let datasets = &[
            "USER01.SRC.COBOL",
            "USER01.SRC.JCL",
            "USER01.LOAD",
            "USER02.SRC.COBOL",
        ];
        let result = dslist("USER01.**", datasets);
        assert_eq!(result.len(), 3);
    }

    #[test]
    fn test_dslist_single_wildcard() {
        let datasets = &[
            "USER01.SRC.COBOL",
            "USER01.SRC.JCL",
            "USER01.LOAD",
        ];
        let result = dslist("USER01.SRC.*", datasets);
        assert_eq!(result.len(), 2);
    }

    #[test]
    fn test_dslist_exact() {
        let datasets = &["MY.DATASET"];
        let result = dslist("MY.DATASET", datasets);
        assert_eq!(result.len(), 1);
    }

    #[test]
    fn test_dslist_no_match() {
        let datasets = &["A.B.C"];
        let result = dslist("X.**", datasets);
        assert_eq!(result.len(), 0);
    }

    #[test]
    fn test_dslist_cmd_parse() {
        assert_eq!(parse_dslist_cmd("E"), Some(DslistCmd::Edit));
        assert_eq!(parse_dslist_cmd("m"), Some(DslistCmd::MemberList));
        assert_eq!(parse_dslist_cmd("Z"), None);
    }

    // -- 3.12 SuperC --

    #[test]
    fn test_superc_identical() {
        let a: Vec<String> = vec!["LINE1".into(), "LINE2".into()];
        let b = a.clone();
        let diffs = superc_compare(&a, &b);
        assert!(diffs.is_empty());
    }

    #[test]
    fn test_superc_insert() {
        let old: Vec<String> = vec!["A".into(), "C".into()];
        let new: Vec<String> = vec!["A".into(), "B".into(), "C".into()];
        let diffs = superc_compare(&old, &new);
        assert_eq!(diffs.len(), 1);
        assert_eq!(diffs[0].kind, DiffKind::Inserted);
        assert_eq!(diffs[0].new_lines, vec!["B".to_string()]);
    }

    #[test]
    fn test_superc_delete() {
        let old: Vec<String> = vec!["A".into(), "B".into(), "C".into()];
        let new: Vec<String> = vec!["A".into(), "C".into()];
        let diffs = superc_compare(&old, &new);
        assert_eq!(diffs.len(), 1);
        assert_eq!(diffs[0].kind, DiffKind::Deleted);
        assert_eq!(diffs[0].old_lines, vec!["B".to_string()]);
    }

    #[test]
    fn test_superc_changed() {
        let old: Vec<String> = vec!["A".into(), "B".into(), "C".into()];
        let new: Vec<String> = vec!["A".into(), "X".into(), "C".into()];
        let diffs = superc_compare(&old, &new);
        assert_eq!(diffs.len(), 1);
        assert_eq!(diffs[0].kind, DiffKind::Changed);
    }

    #[test]
    fn test_superc_listing_identical() {
        let listing = superc_listing("OLD.DS", "NEW.DS", &[]);
        assert!(listing.iter().any(|l| l.contains("IDENTICAL")));
    }

    #[test]
    fn test_superc_listing_with_diffs() {
        let old: Vec<String> = vec!["A".into(), "B".into()];
        let new: Vec<String> = vec!["A".into(), "C".into()];
        let diffs = superc_compare(&old, &new);
        let listing = superc_listing("OLD.DS", "NEW.DS", &diffs);
        assert!(listing.iter().any(|l| l.contains("DIFFERENCE")));
    }

    // -- 3.14 Search-For --

    #[test]
    fn test_search_for_pds() {
        let (mut lib, _) = make_lib_with_pds("SEARCH.PDS", &[
            ("MOD1", &["HELLO WORLD", "FOO BAR"]),
            ("MOD2", &["BAZ WORLD", "QUX"]),
        ]);
        let hits = search_for(&mut lib, &["SEARCH.PDS"], "WORLD", false);
        assert_eq!(hits.len(), 2);
        assert!(hits.iter().any(|h| h.member.as_deref() == Some("MOD1")));
        assert!(hits.iter().any(|h| h.member.as_deref() == Some("MOD2")));
    }

    #[test]
    fn test_search_for_case_insensitive() {
        let (mut lib, _) = make_lib_with_pds("CI.PDS", &[
            ("M1", &["Hello World"]),
        ]);
        let hits = search_for(&mut lib, &["CI.PDS"], "hello", false);
        assert_eq!(hits.len(), 1);
    }

    #[test]
    fn test_search_for_no_match() {
        let (mut lib, _) = make_lib_with_pds("NM.PDS", &[
            ("M1", &["ABC"]),
        ]);
        let hits = search_for(&mut lib, &["NM.PDS"], "XYZ", false);
        assert!(hits.is_empty());
    }

    #[test]
    fn test_search_for_sequential() {
        let mut lib = LibraryManager::new();
        let (_, data_id) = lib.lminit("SEQ.DATA", false);
        lib.lmopen(&data_id, OpenMode::Output);
        lib.seed_sequential(&data_id, vec![
            "FIRST LINE".to_string(),
            "NEEDLE HERE".to_string(),
            "THIRD LINE".to_string(),
        ]);
        lib.lmclose(&data_id);
        // Keep handle alive.

        let hits = search_for(&mut lib, &["SEQ.DATA"], "NEEDLE", true);
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].line_num, 2);
        assert!(hits[0].member.is_none());
    }

    // -- Pattern matching --

    #[test]
    fn test_dsname_matches_double_star() {
        assert!(dsname_matches("USER01.**", "USER01.SRC.COBOL"));
        assert!(dsname_matches("USER01.**", "USER01.LOAD"));
        assert!(!dsname_matches("USER01.**", "USER02.SRC"));
    }

    #[test]
    fn test_dsname_matches_single_star() {
        assert!(dsname_matches("A.*.C", "A.B.C"));
        assert!(!dsname_matches("A.*.C", "A.B.D"));
        assert!(!dsname_matches("A.*.C", "A.B.X.C"));
    }
}
