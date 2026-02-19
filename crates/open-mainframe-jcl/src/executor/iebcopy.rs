//! IEBCOPY — PDS/PDSE Library Copy, Merge, and Compress utility.
//!
//! Supports:
//! - **COPY** — copy all members from INDD PDS to OUTDD PDS
//! - **SELECT** — select specific members with rename and replace options
//! - **EXCLUDE** — copy all members except those listed
//! - **Compress in place** — when INDD == OUTDD, reclaims space
//!
//! ## Control Statement Syntax
//!
//! ```text
//! COPY OUTDD=outdd,INDD=indd
//! SELECT MEMBER=((name1,,R),(name2,newname))
//! EXCLUDE MEMBER=(name1,name2)
//! ```

use std::collections::HashMap;
use std::path::PathBuf;

use open_mainframe_dataset::pds::Pds;

use crate::error::JclError;
use super::StepResult;
use super::utility::{
    read_sysin_statements, UtilityOutput,
    RC_ERROR, RC_CTRL_ERROR,
};

/// A parsed IEBCOPY control statement.
#[derive(Debug, Clone)]
enum IebcopyStmt {
    /// COPY OUTDD=...,INDD=...
    Copy { outdd: String, indd: String },
    /// SELECT MEMBER=((name,newname,R),...)
    Select { members: Vec<SelectEntry> },
    /// EXCLUDE MEMBER=(name1,name2,...)
    Exclude { members: Vec<String> },
}

/// An entry in a SELECT MEMBER list.
#[derive(Debug, Clone)]
struct SelectEntry {
    /// Source member name.
    name: String,
    /// Optional new name for the member in the output PDS.
    new_name: Option<String>,
    /// If true, replace existing member in output PDS.
    replace: bool,
}

/// IEBCOPY utility implementation.
pub struct Iebcopy;

impl super::utility::UtilityProgram for Iebcopy {
    fn execute(
        &self,
        step_name: Option<&str>,
        dd_files: &HashMap<String, PathBuf>,
        _parm: Option<&str>,
    ) -> Result<StepResult, JclError> {
        let mut out = UtilityOutput::new("IEBCOPY");

        // Parse SYSIN (optional — if absent, use default COPY OUTDD=SYSUT2,INDD=SYSUT1)
        let stmts = match read_sysin_statements(dd_files) {
            Ok(lines) => parse_iebcopy_stmts(&lines, &mut out),
            Err(_) => {
                // No SYSIN — default copy from SYSUT1 to SYSUT2
                vec![IebcopyStmt::Copy {
                    outdd: "SYSUT2".to_string(),
                    indd: "SYSUT1".to_string(),
                }]
            }
        };

        if stmts.is_empty() && out.rc() >= RC_ERROR {
            return Ok(out.into_step_result(step_name));
        }

        let mut current_copy: Option<(&str, &str)> = None;

        for (idx, stmt) in stmts.iter().enumerate() {
            match stmt {
                IebcopyStmt::Copy { outdd, indd } => {
                    current_copy = Some((outdd.as_str(), indd.as_str()));

                    let indd_path = match dd_files.get(indd.as_str()) {
                        Some(p) => p,
                        None => {
                            out.error(&format!("DD {indd} NOT ALLOCATED"));
                            continue;
                        }
                    };
                    let outdd_path = match dd_files.get(outdd.as_str()) {
                        Some(p) => p,
                        None => {
                            out.error(&format!("DD {outdd} NOT ALLOCATED"));
                            continue;
                        }
                    };

                    // Compress in place
                    if indd_path == outdd_path {
                        out.info(&format!("COMPRESS IN PLACE - {indd}"));
                        out.info("COMPRESS COMPLETE - NO ACTION NEEDED");
                        continue;
                    }

                    // If next statement is SELECT or EXCLUDE, don't do full copy —
                    // let the SELECT/EXCLUDE handle member selection
                    let next_is_filter = stmts.get(idx + 1).is_some_and(|s| {
                        matches!(s, IebcopyStmt::Select { .. } | IebcopyStmt::Exclude { .. })
                    });
                    if next_is_filter {
                        continue;
                    }

                    // Full copy: all members from INDD to OUTDD
                    let in_pds = match Pds::open(indd_path) {
                        Ok(p) => p,
                        Err(e) => {
                            out.error(&format!("CANNOT OPEN INPUT PDS {indd}: {e}"));
                            continue;
                        }
                    };
                    let mut out_pds = match open_or_create_pds(outdd_path) {
                        Ok(p) => p,
                        Err(e) => {
                            out.error(&format!("CANNOT OPEN OUTPUT PDS {outdd}: {e}"));
                            continue;
                        }
                    };

                    let members: Vec<String> = in_pds
                        .list_members()
                        .iter()
                        .filter(|m| m.alias_of.is_none())
                        .map(|m| m.name.clone())
                        .collect();

                    let mut copied = 0u32;
                    for name in &members {
                        match copy_member(&in_pds, &mut out_pds, name, name, false) {
                            CopyResult::Copied => {
                                copied += 1;
                            }
                            CopyResult::Skipped => {
                                out.warn(&format!("MEMBER {name} ALREADY EXISTS - NOT REPLACED"));
                            }
                            CopyResult::Error(e) => {
                                out.error(&format!("COPY MEMBER {name} FAILED: {e}"));
                            }
                        }
                    }
                    out.info(&format!("{copied} MEMBERS COPIED FROM {indd} TO {outdd}"));
                }

                IebcopyStmt::Select { members } => {
                    let (outdd, indd) = match current_copy {
                        Some(pair) => pair,
                        None => {
                            out.error("SELECT WITHOUT PRECEDING COPY STATEMENT");
                            continue;
                        }
                    };

                    let indd_path = match dd_files.get(indd) {
                        Some(p) => p,
                        None => {
                            out.error(&format!("DD {indd} NOT ALLOCATED"));
                            continue;
                        }
                    };
                    let outdd_path = match dd_files.get(outdd) {
                        Some(p) => p,
                        None => {
                            out.error(&format!("DD {outdd} NOT ALLOCATED"));
                            continue;
                        }
                    };

                    let in_pds = match Pds::open(indd_path) {
                        Ok(p) => p,
                        Err(e) => {
                            out.error(&format!("CANNOT OPEN INPUT PDS: {e}"));
                            continue;
                        }
                    };
                    let mut out_pds = match open_or_create_pds(outdd_path) {
                        Ok(p) => p,
                        Err(e) => {
                            out.error(&format!("CANNOT OPEN OUTPUT PDS: {e}"));
                            continue;
                        }
                    };

                    for entry in members {
                        let target = entry.new_name.as_deref().unwrap_or(&entry.name);
                        match copy_member(&in_pds, &mut out_pds, &entry.name, target, entry.replace) {
                            CopyResult::Copied => {
                                if entry.new_name.is_some() {
                                    out.info(&format!(
                                        "MEMBER {} COPIED AS {target}",
                                        entry.name
                                    ));
                                } else {
                                    out.info(&format!("MEMBER {} COPIED", entry.name));
                                }
                            }
                            CopyResult::Skipped => {
                                out.warn(&format!(
                                    "MEMBER {target} ALREADY EXISTS - NOT REPLACED"
                                ));
                            }
                            CopyResult::Error(e) => {
                                out.error(&format!("MEMBER {} FAILED: {e}", entry.name));
                            }
                        }
                    }
                }

                IebcopyStmt::Exclude { members } => {
                    let (outdd, indd) = match current_copy {
                        Some(pair) => pair,
                        None => {
                            out.error("EXCLUDE WITHOUT PRECEDING COPY STATEMENT");
                            continue;
                        }
                    };

                    let indd_path = match dd_files.get(indd) {
                        Some(p) => p,
                        None => {
                            out.error(&format!("DD {indd} NOT ALLOCATED"));
                            continue;
                        }
                    };
                    let outdd_path = match dd_files.get(outdd) {
                        Some(p) => p,
                        None => {
                            out.error(&format!("DD {outdd} NOT ALLOCATED"));
                            continue;
                        }
                    };

                    let in_pds = match Pds::open(indd_path) {
                        Ok(p) => p,
                        Err(e) => {
                            out.error(&format!("CANNOT OPEN INPUT PDS: {e}"));
                            continue;
                        }
                    };
                    let mut out_pds = match open_or_create_pds(outdd_path) {
                        Ok(p) => p,
                        Err(e) => {
                            out.error(&format!("CANNOT OPEN OUTPUT PDS: {e}"));
                            continue;
                        }
                    };

                    let exclude_set: Vec<String> = members.iter().map(|m| m.to_ascii_uppercase()).collect();
                    let all_members: Vec<String> = in_pds
                        .list_members()
                        .iter()
                        .filter(|m| m.alias_of.is_none())
                        .map(|m| m.name.clone())
                        .collect();

                    let mut copied = 0u32;
                    for name in &all_members {
                        if exclude_set.contains(&name.to_ascii_uppercase()) {
                            continue;
                        }
                        match copy_member(&in_pds, &mut out_pds, name, name, false) {
                            CopyResult::Copied => copied += 1,
                            CopyResult::Skipped => {
                                out.warn(&format!("MEMBER {name} ALREADY EXISTS"));
                            }
                            CopyResult::Error(e) => {
                                out.error(&format!("MEMBER {name} FAILED: {e}"));
                            }
                        }
                    }
                    out.info(&format!(
                        "{copied} MEMBERS COPIED ({} EXCLUDED)",
                        exclude_set.len()
                    ));
                }
            }
        }

        Ok(out.into_step_result(step_name))
    }

    fn name(&self) -> &str {
        "IEBCOPY"
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

enum CopyResult {
    Copied,
    Skipped,
    Error(String),
}

fn copy_member(
    src: &Pds,
    dst: &mut Pds,
    src_name: &str,
    dst_name: &str,
    replace: bool,
) -> CopyResult {
    // Check if target exists
    if dst.has_member(dst_name) && !replace {
        return CopyResult::Skipped;
    }

    let data = match src.read_member(src_name) {
        Ok(d) => d,
        Err(e) => return CopyResult::Error(format!("READ {src_name}: {e}")),
    };

    if dst.has_member(dst_name) {
        // Replace
        match dst.update_member(dst_name, &data) {
            Ok(()) => CopyResult::Copied,
            Err(e) => CopyResult::Error(format!("WRITE {dst_name}: {e}")),
        }
    } else {
        match dst.add_member(dst_name, &data) {
            Ok(()) => CopyResult::Copied,
            Err(e) => CopyResult::Error(format!("ADD {dst_name}: {e}")),
        }
    }
}

fn open_or_create_pds(path: &std::path::Path) -> Result<Pds, String> {
    if path.exists() {
        Pds::open(path).map_err(|e| e.to_string())
    } else {
        Pds::create(path).map_err(|e| e.to_string())
    }
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

fn parse_iebcopy_stmts(lines: &[String], out: &mut UtilityOutput) -> Vec<IebcopyStmt> {
    let mut stmts = Vec::new();

    for line in lines {
        let line = line.trim();
        if line.starts_with("COPY") {
            if let Some(stmt) = parse_copy_stmt(line) {
                stmts.push(stmt);
            } else {
                out.set_rc(RC_CTRL_ERROR);
                out.error(&format!("INVALID COPY STATEMENT: {line}"));
            }
        } else if line.starts_with("SELECT") {
            if let Some(stmt) = parse_select_stmt(line) {
                stmts.push(stmt);
            } else {
                out.set_rc(RC_CTRL_ERROR);
                out.error(&format!("INVALID SELECT STATEMENT: {line}"));
            }
        } else if line.starts_with("EXCLUDE") {
            if let Some(stmt) = parse_exclude_stmt(line) {
                stmts.push(stmt);
            } else {
                out.set_rc(RC_CTRL_ERROR);
                out.error(&format!("INVALID EXCLUDE STATEMENT: {line}"));
            }
        }
    }

    stmts
}

/// Parse `COPY OUTDD=outdd,INDD=indd`
fn parse_copy_stmt(s: &str) -> Option<IebcopyStmt> {
    let rest = s.strip_prefix("COPY")?.trim();
    let kv = parse_kv(rest);
    let outdd = kv.get("OUTDD")?.clone();
    let indd = kv.get("INDD")?.clone();
    Some(IebcopyStmt::Copy { outdd, indd })
}

/// Parse `SELECT MEMBER=((name1,,R),(name2,newname))` or `SELECT MEMBER=(name1,name2)`
fn parse_select_stmt(s: &str) -> Option<IebcopyStmt> {
    let rest = s.strip_prefix("SELECT")?.trim();
    let member_str = rest.strip_prefix("MEMBER=")?;

    let entries = parse_member_list(member_str)?;
    Some(IebcopyStmt::Select { members: entries })
}

/// Parse `EXCLUDE MEMBER=(name1,name2)`
fn parse_exclude_stmt(s: &str) -> Option<IebcopyStmt> {
    let rest = s.strip_prefix("EXCLUDE")?.trim();
    let member_str = rest.strip_prefix("MEMBER=")?;

    // Simple list: (name1,name2,...) or name1
    let inner = member_str
        .trim_start_matches('(')
        .trim_end_matches(')');
    let names: Vec<String> = inner
        .split(',')
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();

    if names.is_empty() {
        return None;
    }
    Some(IebcopyStmt::Exclude { members: names })
}

/// Parse SELECT member list. Supports:
/// - `(name1,name2)` — simple list
/// - `((name1,,R),(name2,newname))` — with rename and replace
fn parse_member_list(s: &str) -> Option<Vec<SelectEntry>> {
    let s = s.trim();
    let mut entries = Vec::new();

    // Check for nested parens (complex format)
    if s.starts_with("((") {
        let inner = &s[1..s.len() - 1]; // strip outer parens
        // Split on ),( boundaries
        let parts = split_paren_entries(inner);
        for part in parts {
            let fields: Vec<&str> = part.split(',').collect();
            let name = fields.first()?.trim().to_string();
            if name.is_empty() {
                continue;
            }
            let new_name = fields
                .get(1)
                .map(|s| s.trim())
                .filter(|s| !s.is_empty())
                .map(|s| s.to_string());
            let replace = fields
                .get(2)
                .map(|s| s.trim().eq_ignore_ascii_case("R"))
                .unwrap_or(false);
            entries.push(SelectEntry {
                name,
                new_name,
                replace,
            });
        }
    } else {
        // Simple format: (name1,name2) or single name
        let inner = s.trim_start_matches('(').trim_end_matches(')');
        for name in inner.split(',') {
            let name = name.trim().to_string();
            if !name.is_empty() {
                entries.push(SelectEntry {
                    name,
                    new_name: None,
                    replace: false,
                });
            }
        }
    }

    if entries.is_empty() {
        None
    } else {
        Some(entries)
    }
}

/// Split `(A,,R),(B,NEWB)` into `["A,,R", "B,NEWB"]`
fn split_paren_entries(s: &str) -> Vec<&str> {
    let mut entries = Vec::new();
    let mut depth = 0;
    let mut start = 0;

    for (i, c) in s.char_indices() {
        match c {
            '(' => {
                if depth == 0 {
                    start = i + 1;
                }
                depth += 1;
            }
            ')' => {
                depth -= 1;
                if depth == 0 {
                    entries.push(&s[start..i]);
                }
            }
            _ => {}
        }
    }
    entries
}

/// Parse `KEY=VALUE,KEY=VALUE,...`
fn parse_kv(s: &str) -> HashMap<String, String> {
    let mut map = HashMap::new();
    for part in s.split(',') {
        if let Some((k, v)) = part.split_once('=') {
            map.insert(k.trim().to_string(), v.trim().to_string());
        }
    }
    map
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::executor::utility::{UtilityProgram, RC_WARNING};
    use std::fs;

    fn create_test_pds(dir: &std::path::Path, members: &[(&str, &str)]) -> PathBuf {
        let pds_path = dir.to_path_buf();
        let mut pds = Pds::create(&pds_path).unwrap();
        for (name, content) in members {
            pds.add_member(name, content.as_bytes()).unwrap();
        }
        pds_path
    }

    #[test]
    fn test_copy_all_members() {
        let temp = std::env::temp_dir().join("iebcopy_copy_all");
        let _ = fs::remove_dir_all(&temp);
        fs::create_dir_all(&temp).unwrap();

        let in_path = create_test_pds(
            &temp.join("in_pds"),
            &[("MEM1", "DATA1"), ("MEM2", "DATA2"), ("MEM3", "DATA3")],
        );

        let out_path = temp.join("out_pds");

        let sysin_path = temp.join("SYSIN");
        fs::write(&sysin_path, " COPY OUTDD=OUT,INDD=IN\n").unwrap();

        let mut dd_files = HashMap::new();
        dd_files.insert("IN".to_string(), in_path);
        dd_files.insert("OUT".to_string(), out_path.clone());
        dd_files.insert("SYSIN".to_string(), sysin_path);

        let util = Iebcopy;
        let result = util.execute(Some("COPY"), &dd_files, None).unwrap();

        assert!(result.success, "stdout: {}", result.stdout);
        assert!(result.stdout.contains("3 MEMBERS COPIED"));

        // Verify output PDS
        let out_pds = Pds::open(&out_path).unwrap();
        assert_eq!(out_pds.member_count(), 3);
        assert_eq!(out_pds.read_member("MEM1").unwrap(), b"DATA1");

        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn test_select_with_rename() {
        let temp = std::env::temp_dir().join("iebcopy_select");
        let _ = fs::remove_dir_all(&temp);
        fs::create_dir_all(&temp).unwrap();

        let in_path = create_test_pds(
            &temp.join("in_pds"),
            &[("MEM1", "D1"), ("MEM2", "D2"), ("MEM3", "D3")],
        );
        let out_path = temp.join("out_pds");

        let sysin_path = temp.join("SYSIN");
        fs::write(
            &sysin_path,
            " COPY OUTDD=OUT,INDD=IN\n SELECT MEMBER=((MEM1,,R),(MEM2,NEWMEM))\n",
        )
        .unwrap();

        let mut dd_files = HashMap::new();
        dd_files.insert("IN".to_string(), in_path);
        dd_files.insert("OUT".to_string(), out_path.clone());
        dd_files.insert("SYSIN".to_string(), sysin_path);

        let util = Iebcopy;
        let result = util.execute(Some("SEL"), &dd_files, None).unwrap();

        assert!(result.success, "stdout: {}", result.stdout);
        assert!(result.stdout.contains("MEMBER MEM1 COPIED"));
        assert!(result.stdout.contains("MEMBER MEM2 COPIED AS NEWMEM"));

        let out_pds = Pds::open(&out_path).unwrap();
        assert!(out_pds.has_member("MEM1"));
        assert!(out_pds.has_member("NEWMEM"));
        assert!(!out_pds.has_member("MEM2")); // copied as NEWMEM
        assert!(!out_pds.has_member("MEM3")); // not selected

        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn test_exclude() {
        let temp = std::env::temp_dir().join("iebcopy_exclude");
        let _ = fs::remove_dir_all(&temp);
        fs::create_dir_all(&temp).unwrap();

        let in_path = create_test_pds(
            &temp.join("in_pds"),
            &[("MEM1", "D1"), ("MEM2", "D2"), ("MEM3", "D3")],
        );
        let out_path = temp.join("out_pds");

        let sysin_path = temp.join("SYSIN");
        fs::write(
            &sysin_path,
            " COPY OUTDD=OUT,INDD=IN\n EXCLUDE MEMBER=(MEM2)\n",
        )
        .unwrap();

        let mut dd_files = HashMap::new();
        dd_files.insert("IN".to_string(), in_path);
        dd_files.insert("OUT".to_string(), out_path.clone());
        dd_files.insert("SYSIN".to_string(), sysin_path);

        let util = Iebcopy;
        let result = util.execute(Some("EXCL"), &dd_files, None).unwrap();

        assert!(result.success, "stdout: {}", result.stdout);
        assert!(result.stdout.contains("2 MEMBERS COPIED (1 EXCLUDED)"));

        let out_pds = Pds::open(&out_path).unwrap();
        assert!(out_pds.has_member("MEM1"));
        assert!(!out_pds.has_member("MEM2"));
        assert!(out_pds.has_member("MEM3"));

        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn test_compress_in_place() {
        let temp = std::env::temp_dir().join("iebcopy_compress");
        let _ = fs::remove_dir_all(&temp);
        fs::create_dir_all(&temp).unwrap();

        let pds_path = create_test_pds(
            &temp.join("pds"),
            &[("MEM1", "DATA")],
        );

        let sysin_path = temp.join("SYSIN");
        fs::write(&sysin_path, " COPY OUTDD=DD1,INDD=DD1\n").unwrap();

        let mut dd_files = HashMap::new();
        dd_files.insert("DD1".to_string(), pds_path);
        dd_files.insert("SYSIN".to_string(), sysin_path);

        let util = Iebcopy;
        let result = util.execute(Some("COMP"), &dd_files, None).unwrap();

        assert!(result.success);
        assert!(result.stdout.contains("COMPRESS IN PLACE"));

        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn test_no_sysin_default_copy() {
        let temp = std::env::temp_dir().join("iebcopy_nosysin");
        let _ = fs::remove_dir_all(&temp);
        fs::create_dir_all(&temp).unwrap();

        let in_path = create_test_pds(
            &temp.join("sysut1"),
            &[("MEM1", "DATA")],
        );
        let out_path = temp.join("sysut2");

        let mut dd_files = HashMap::new();
        dd_files.insert("SYSUT1".to_string(), in_path);
        dd_files.insert("SYSUT2".to_string(), out_path.clone());

        let util = Iebcopy;
        let result = util.execute(Some("DEF"), &dd_files, None).unwrap();

        assert!(result.success, "stdout: {}", result.stdout);

        let out_pds = Pds::open(&out_path).unwrap();
        assert_eq!(out_pds.member_count(), 1);

        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn test_skip_existing_without_replace() {
        let temp = std::env::temp_dir().join("iebcopy_norepl");
        let _ = fs::remove_dir_all(&temp);
        fs::create_dir_all(&temp).unwrap();

        let in_path = create_test_pds(
            &temp.join("in_pds"),
            &[("MEM1", "NEW_DATA")],
        );
        let out_path = create_test_pds(
            &temp.join("out_pds"),
            &[("MEM1", "OLD_DATA")],
        );

        let sysin_path = temp.join("SYSIN");
        fs::write(&sysin_path, " COPY OUTDD=OUT,INDD=IN\n").unwrap();

        let mut dd_files = HashMap::new();
        dd_files.insert("IN".to_string(), in_path);
        dd_files.insert("OUT".to_string(), out_path.clone());
        dd_files.insert("SYSIN".to_string(), sysin_path);

        let util = Iebcopy;
        let result = util.execute(Some("NOREPL"), &dd_files, None).unwrap();

        // RC should be 4 (warning) due to skipped member
        assert_eq!(result.return_code, RC_WARNING);
        assert!(result.stdout.contains("NOT REPLACED"));

        // Original data preserved
        let out_pds = Pds::open(&out_path).unwrap();
        assert_eq!(out_pds.read_member("MEM1").unwrap(), b"OLD_DATA");

        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn test_select_with_replace() {
        let temp = std::env::temp_dir().join("iebcopy_repl");
        let _ = fs::remove_dir_all(&temp);
        fs::create_dir_all(&temp).unwrap();

        let in_path = create_test_pds(
            &temp.join("in_pds"),
            &[("MEM1", "NEW_DATA")],
        );
        let out_path = create_test_pds(
            &temp.join("out_pds"),
            &[("MEM1", "OLD_DATA")],
        );

        let sysin_path = temp.join("SYSIN");
        fs::write(&sysin_path, " COPY OUTDD=OUT,INDD=IN\n SELECT MEMBER=((MEM1,,R))\n").unwrap();

        let mut dd_files = HashMap::new();
        dd_files.insert("IN".to_string(), in_path);
        dd_files.insert("OUT".to_string(), out_path.clone());
        dd_files.insert("SYSIN".to_string(), sysin_path);

        let util = Iebcopy;
        let result = util.execute(Some("REPL"), &dd_files, None).unwrap();

        assert!(result.success, "stdout: {}", result.stdout);

        let out_pds = Pds::open(&out_path).unwrap();
        assert_eq!(out_pds.read_member("MEM1").unwrap(), b"NEW_DATA");

        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn test_parse_select_simple() {
        let entries = parse_member_list("(MEM1,MEM2,MEM3)").unwrap();
        assert_eq!(entries.len(), 3);
        assert_eq!(entries[0].name, "MEM1");
        assert!(!entries[0].replace);
        assert!(entries[0].new_name.is_none());
    }

    #[test]
    fn test_parse_select_complex() {
        let entries = parse_member_list("((MEM1,,R),(MEM2,NEWNAME))").unwrap();
        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0].name, "MEM1");
        assert!(entries[0].replace);
        assert_eq!(entries[1].name, "MEM2");
        assert_eq!(entries[1].new_name.as_deref(), Some("NEWNAME"));
    }
}
