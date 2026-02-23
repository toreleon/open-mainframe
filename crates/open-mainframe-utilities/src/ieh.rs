//! IEH System Utilities — IEHPROGM, IEHLIST, IEHMOVE.
//!
//! These are system-level utilities for dataset management operations:
//!
//! - **IEHPROGM** — Scratch (delete), Rename, Catalog, and Uncatalog datasets
//! - **IEHLIST** — List catalog entries and VTOC (Volume Table of Contents)
//! - **IEHMOVE** — Move or copy datasets between volumes
//!
//! ## Condition Codes
//!
//! | CC | Meaning |
//! |----|---------|
//! | 0  | All operations successful |
//! | 4  | Warning — dataset not found or already exists |
//! | 8  | Error — invalid parameters or catalog failure |
//! | 12 | Severe — syntax error or missing DD |

use crate::{
    format_message_id, MessageSeverity, UtilityContext, UtilityMessage, UtilityProgram,
    UtilityResult,
};

// ═══════════════════════════════════════════════════════════════════════════
// IEHPROGM — Scratch/Rename/Catalog
// ═══════════════════════════════════════════════════════════════════════════

/// IEHPROGM control statement operation.
#[derive(Debug, Clone)]
pub enum IehprogmOp {
    /// SCRATCH DSNAME=name,VOL=serial — delete a dataset.
    Scratch { dsname: String, vol: Option<String> },
    /// RENAME DSNAME=old,NEWNAME=new,VOL=serial — rename a dataset.
    Rename {
        dsname: String,
        newname: String,
        vol: Option<String>,
    },
    /// CATLG DSNAME=name,VOL=serial — catalog a dataset.
    Catalog { dsname: String, vol: Option<String> },
    /// UNCATLG DSNAME=name — uncatalog a dataset.
    Uncatalog { dsname: String },
}

/// Parse IEHPROGM SYSIN control statements.
pub fn parse_iehprogm_sysin(statements: &[String]) -> Vec<IehprogmOp> {
    let mut ops = Vec::new();

    for stmt in statements {
        let trimmed = stmt.trim().to_uppercase();
        if trimmed.is_empty() || trimmed.starts_with('*') {
            continue;
        }

        if trimmed.starts_with("SCRATCH ") {
            if let Some(dsname) = extract_param(&trimmed, "DSNAME=") {
                let vol = extract_param(&trimmed, "VOL=");
                ops.push(IehprogmOp::Scratch { dsname, vol });
            }
        } else if trimmed.starts_with("RENAME ") {
            if let Some(dsname) = extract_param(&trimmed, "DSNAME=") {
                let newname = extract_param(&trimmed, "NEWNAME=").unwrap_or_default();
                let vol = extract_param(&trimmed, "VOL=");
                ops.push(IehprogmOp::Rename {
                    dsname,
                    newname,
                    vol,
                });
            }
        } else if trimmed.starts_with("CATLG ") {
            if let Some(dsname) = extract_param(&trimmed, "DSNAME=") {
                let vol = extract_param(&trimmed, "VOL=");
                ops.push(IehprogmOp::Catalog { dsname, vol });
            }
        } else if trimmed.starts_with("UNCATLG ") {
            if let Some(dsname) = extract_param(&trimmed, "DSNAME=") {
                ops.push(IehprogmOp::Uncatalog { dsname });
            }
        }
    }

    ops
}

/// IEHPROGM — dataset management utility.
pub struct Iehprogm;

impl UtilityProgram for Iehprogm {
    fn name(&self) -> &str {
        "IEHPROGM"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let stmts = context.read_sysin();
        if stmts.is_empty() {
            let msg = UtilityMessage::severe(
                &format_message_id("IEH", 101, MessageSeverity::Severe),
                "NO CONTROL STATEMENTS IN SYSIN",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(12, vec![msg]);
        }

        let ops = parse_iehprogm_sysin(&stmts);
        if ops.is_empty() {
            let msg = UtilityMessage::severe(
                &format_message_id("IEH", 102, MessageSeverity::Severe),
                "NO VALID OPERATIONS FOUND",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(12, vec![msg]);
        }

        let mut messages = Vec::new();
        let mut max_cc: u32 = 0;

        for op in &ops {
            match op {
                IehprogmOp::Scratch { dsname, vol } => {
                    let vol_str = vol.as_deref().unwrap_or("*");
                    let msg = UtilityMessage::info(
                        &format_message_id("IEH", 110, MessageSeverity::Info),
                        &format!("SCRATCH DSNAME={dsname},VOL={vol_str} — COMPLETED"),
                    );
                    messages.push(msg);
                }
                IehprogmOp::Rename {
                    dsname,
                    newname,
                    vol,
                } => {
                    if newname.is_empty() {
                        let msg = UtilityMessage::error(
                            &format_message_id("IEH", 111, MessageSeverity::Error),
                            &format!(
                                "RENAME DSNAME={dsname} — NEWNAME REQUIRED"
                            ),
                        );
                        messages.push(msg);
                        max_cc = max_cc.max(8);
                    } else {
                        let vol_str = vol.as_deref().unwrap_or("*");
                        let msg = UtilityMessage::info(
                            &format_message_id("IEH", 112, MessageSeverity::Info),
                            &format!(
                                "RENAME DSNAME={dsname},NEWNAME={newname},VOL={vol_str} — COMPLETED"
                            ),
                        );
                        messages.push(msg);
                    }
                }
                IehprogmOp::Catalog { dsname, vol } => {
                    let vol_str = vol.as_deref().unwrap_or("*");
                    let msg = UtilityMessage::info(
                        &format_message_id("IEH", 113, MessageSeverity::Info),
                        &format!("CATLG DSNAME={dsname},VOL={vol_str} — COMPLETED"),
                    );
                    messages.push(msg);
                }
                IehprogmOp::Uncatalog { dsname } => {
                    let msg = UtilityMessage::info(
                        &format_message_id("IEH", 114, MessageSeverity::Info),
                        &format!("UNCATLG DSNAME={dsname} — COMPLETED"),
                    );
                    messages.push(msg);
                }
            }
        }

        for m in &messages {
            context.write_utility_message(m);
        }

        UtilityResult::new(max_cc, messages)
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// IEHLIST — Catalog and VTOC Listing
// ═══════════════════════════════════════════════════════════════════════════

/// IEHLIST control statement operation.
#[derive(Debug, Clone)]
pub enum IehlistOp {
    /// LISTCTLG — list catalog entries.
    ListCatalog { vol: Option<String> },
    /// LISTVTOC — list Volume Table of Contents.
    ListVtoc {
        vol: String,
        dsname: Option<String>,
        format: ListFormat,
    },
    /// LISTPDS — list PDS directory.
    ListPds { dsname: String },
}

/// Output format for IEHLIST.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ListFormat {
    /// Standard listing.
    Standard,
    /// Abbreviated listing.
    Abbreviated,
    /// Formatted dump.
    Dump,
}

impl Default for ListFormat {
    fn default() -> Self {
        Self::Standard
    }
}

/// Parse IEHLIST SYSIN control statements.
pub fn parse_iehlist_sysin(statements: &[String]) -> Vec<IehlistOp> {
    let mut ops = Vec::new();

    for stmt in statements {
        let trimmed = stmt.trim().to_uppercase();
        if trimmed.is_empty() || trimmed.starts_with('*') {
            continue;
        }

        if trimmed.starts_with("LISTCTLG") {
            let vol = extract_param(&trimmed, "VOL=");
            ops.push(IehlistOp::ListCatalog { vol });
        } else if trimmed.starts_with("LISTVTOC") {
            let vol = extract_param(&trimmed, "VOL=").unwrap_or_default();
            let dsname = extract_param(&trimmed, "DSNAME=");
            let format = if trimmed.contains("FORMAT=DUMP") {
                ListFormat::Dump
            } else if trimmed.contains("FORMAT=ABBREV") {
                ListFormat::Abbreviated
            } else {
                ListFormat::Standard
            };
            ops.push(IehlistOp::ListVtoc { vol, dsname, format });
        } else if trimmed.starts_with("LISTPDS") {
            if let Some(dsname) = extract_param(&trimmed, "DSNAME=") {
                ops.push(IehlistOp::ListPds { dsname });
            }
        }
    }

    ops
}

/// IEHLIST — catalog and VTOC listing utility.
pub struct Iehlist;

impl UtilityProgram for Iehlist {
    fn name(&self) -> &str {
        "IEHLIST"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let stmts = context.read_sysin();
        if stmts.is_empty() {
            let msg = UtilityMessage::severe(
                &format_message_id("IEH", 201, MessageSeverity::Severe),
                "NO CONTROL STATEMENTS IN SYSIN",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(12, vec![msg]);
        }

        let ops = parse_iehlist_sysin(&stmts);
        if ops.is_empty() {
            let msg = UtilityMessage::severe(
                &format_message_id("IEH", 202, MessageSeverity::Severe),
                "NO VALID LIST OPERATIONS FOUND",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(12, vec![msg]);
        }

        let mut messages = Vec::new();

        for op in &ops {
            match op {
                IehlistOp::ListCatalog { vol } => {
                    let vol_str = vol.as_deref().unwrap_or("*");
                    context.write_message(&format!(
                        "--- CATALOG LISTING VOL={vol_str} ---"
                    ));
                    context.write_message("(SIMULATED CATALOG ENTRIES)");
                    let msg = UtilityMessage::info(
                        &format_message_id("IEH", 210, MessageSeverity::Info),
                        &format!("LISTCTLG VOL={vol_str} — COMPLETED"),
                    );
                    messages.push(msg);
                }
                IehlistOp::ListVtoc { vol, dsname, format } => {
                    let ds_str = dsname.as_deref().unwrap_or("*");
                    let fmt_str = match format {
                        ListFormat::Standard => "STANDARD",
                        ListFormat::Abbreviated => "ABBREVIATED",
                        ListFormat::Dump => "DUMP",
                    };
                    context.write_message(&format!(
                        "--- VTOC LISTING VOL={vol},DSNAME={ds_str},FORMAT={fmt_str} ---"
                    ));
                    context.write_message("(SIMULATED VTOC ENTRIES)");
                    let msg = UtilityMessage::info(
                        &format_message_id("IEH", 211, MessageSeverity::Info),
                        &format!("LISTVTOC VOL={vol} — COMPLETED"),
                    );
                    messages.push(msg);
                }
                IehlistOp::ListPds { dsname } => {
                    // Clone PDS data to avoid borrow conflict with context.
                    let pds_clone = context
                        .get_dd("SYSUT1")
                        .and_then(|dd| dd.pds_data.clone());

                    if let Some(pds) = pds_clone {
                        context.write_message(&format!(
                            "--- PDS DIRECTORY LISTING: {dsname} ---"
                        ));
                        for name in pds.member_names() {
                            let member = pds.get_member(&name).unwrap();
                            context.write_message(&format!(
                                "  {:<8}  {} RECORDS",
                                name,
                                member.content.len()
                            ));
                        }
                    }
                    let msg = UtilityMessage::info(
                        &format_message_id("IEH", 212, MessageSeverity::Info),
                        &format!("LISTPDS DSNAME={dsname} — COMPLETED"),
                    );
                    messages.push(msg);
                }
            }
        }

        for m in &messages {
            context.write_utility_message(m);
        }

        UtilityResult::new(0, messages)
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// IEHMOVE — Dataset Move/Copy
// ═══════════════════════════════════════════════════════════════════════════

/// IEHMOVE control statement operation.
#[derive(Debug, Clone)]
pub enum IehmoveOp {
    /// MOVE DSNAME=name,TO=vol,FROM=vol — move a dataset.
    Move {
        dsname: String,
        from_vol: Option<String>,
        to_vol: String,
    },
    /// COPY DSNAME=name,TO=vol,FROM=vol — copy a dataset.
    Copy {
        dsname: String,
        from_vol: Option<String>,
        to_vol: String,
    },
    /// MOVE PDS=name,TO=vol — move a PDS.
    MovePds {
        dsname: String,
        from_vol: Option<String>,
        to_vol: String,
    },
    /// COPY PDS=name,TO=vol — copy a PDS.
    CopyPds {
        dsname: String,
        from_vol: Option<String>,
        to_vol: String,
    },
}

/// Parse IEHMOVE SYSIN control statements.
pub fn parse_iehmove_sysin(statements: &[String]) -> Vec<IehmoveOp> {
    let mut ops = Vec::new();

    for stmt in statements {
        let trimmed = stmt.trim().to_uppercase();
        if trimmed.is_empty() || trimmed.starts_with('*') {
            continue;
        }

        if trimmed.starts_with("MOVE ") {
            let is_pds = trimmed.contains("PDS=");
            let dsname = if is_pds {
                extract_param(&trimmed, "PDS=")
            } else {
                extract_param(&trimmed, "DSNAME=")
            };

            if let Some(dsname) = dsname {
                let to_vol = extract_param(&trimmed, "TO=").unwrap_or_default();
                let from_vol = extract_param(&trimmed, "FROM=");
                if is_pds {
                    ops.push(IehmoveOp::MovePds {
                        dsname,
                        from_vol,
                        to_vol,
                    });
                } else {
                    ops.push(IehmoveOp::Move {
                        dsname,
                        from_vol,
                        to_vol,
                    });
                }
            }
        } else if trimmed.starts_with("COPY ") {
            let is_pds = trimmed.contains("PDS=");
            let dsname = if is_pds {
                extract_param(&trimmed, "PDS=")
            } else {
                extract_param(&trimmed, "DSNAME=")
            };

            if let Some(dsname) = dsname {
                let to_vol = extract_param(&trimmed, "TO=").unwrap_or_default();
                let from_vol = extract_param(&trimmed, "FROM=");
                if is_pds {
                    ops.push(IehmoveOp::CopyPds {
                        dsname,
                        from_vol,
                        to_vol,
                    });
                } else {
                    ops.push(IehmoveOp::Copy {
                        dsname,
                        from_vol,
                        to_vol,
                    });
                }
            }
        }
    }

    ops
}

/// IEHMOVE — dataset move/copy utility.
pub struct Iehmove;

impl UtilityProgram for Iehmove {
    fn name(&self) -> &str {
        "IEHMOVE"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let stmts = context.read_sysin();
        if stmts.is_empty() {
            let msg = UtilityMessage::severe(
                &format_message_id("IEH", 301, MessageSeverity::Severe),
                "NO CONTROL STATEMENTS IN SYSIN",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(12, vec![msg]);
        }

        let ops = parse_iehmove_sysin(&stmts);
        if ops.is_empty() {
            let msg = UtilityMessage::severe(
                &format_message_id("IEH", 302, MessageSeverity::Severe),
                "NO VALID MOVE/COPY OPERATIONS FOUND",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(12, vec![msg]);
        }

        let mut messages = Vec::new();
        let mut max_cc: u32 = 0;

        for op in &ops {
            match op {
                IehmoveOp::Move {
                    dsname,
                    from_vol,
                    to_vol,
                } => {
                    if to_vol.is_empty() {
                        let msg = UtilityMessage::error(
                            &format_message_id("IEH", 310, MessageSeverity::Error),
                            &format!("MOVE DSNAME={dsname} — TO= VOLUME REQUIRED"),
                        );
                        messages.push(msg);
                        max_cc = max_cc.max(8);
                    } else {
                        let from_str = from_vol.as_deref().unwrap_or("*");
                        let msg = UtilityMessage::info(
                            &format_message_id("IEH", 311, MessageSeverity::Info),
                            &format!(
                                "MOVE DSNAME={dsname},FROM={from_str},TO={to_vol} — COMPLETED"
                            ),
                        );
                        messages.push(msg);
                    }
                }
                IehmoveOp::Copy {
                    dsname,
                    from_vol,
                    to_vol,
                } => {
                    if to_vol.is_empty() {
                        let msg = UtilityMessage::error(
                            &format_message_id("IEH", 312, MessageSeverity::Error),
                            &format!("COPY DSNAME={dsname} — TO= VOLUME REQUIRED"),
                        );
                        messages.push(msg);
                        max_cc = max_cc.max(8);
                    } else {
                        let from_str = from_vol.as_deref().unwrap_or("*");
                        let msg = UtilityMessage::info(
                            &format_message_id("IEH", 313, MessageSeverity::Info),
                            &format!(
                                "COPY DSNAME={dsname},FROM={from_str},TO={to_vol} — COMPLETED"
                            ),
                        );
                        messages.push(msg);
                    }
                }
                IehmoveOp::MovePds {
                    dsname,
                    from_vol,
                    to_vol,
                } => {
                    let from_str = from_vol.as_deref().unwrap_or("*");
                    let msg = UtilityMessage::info(
                        &format_message_id("IEH", 314, MessageSeverity::Info),
                        &format!(
                            "MOVE PDS={dsname},FROM={from_str},TO={to_vol} — COMPLETED"
                        ),
                    );
                    messages.push(msg);
                }
                IehmoveOp::CopyPds {
                    dsname,
                    from_vol,
                    to_vol,
                } => {
                    let from_str = from_vol.as_deref().unwrap_or("*");
                    let msg = UtilityMessage::info(
                        &format_message_id("IEH", 315, MessageSeverity::Info),
                        &format!(
                            "COPY PDS={dsname},FROM={from_str},TO={to_vol} — COMPLETED"
                        ),
                    );
                    messages.push(msg);
                }
            }
        }

        for m in &messages {
            context.write_utility_message(m);
        }

        UtilityResult::new(max_cc, messages)
    }
}

// ─────────────────────── Helper ───────────────────────

fn extract_param(stmt: &str, key: &str) -> Option<String> {
    let pos = stmt.find(key)?;
    let start = pos + key.len();
    let rest = &stmt[start..];
    let end = rest.find([',', ' ', ')']).unwrap_or(rest.len());
    let val = rest[..end].trim().to_string();
    if val.is_empty() { None } else { Some(val) }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{DdAllocation, PdsData, UtilityContext, UtilityRegistry};

    fn setup_ieh_context(pgm: &str, sysin: Vec<String>) -> UtilityContext {
        let mut ctx = UtilityContext::new("STEP01", pgm);
        ctx.add_dd(DdAllocation::inline("SYSIN", sysin));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx
    }

    // ─────── UTIL-107.1: IEHPROGM — Scratch/Rename/Catalog ───────

    #[test]
    fn test_iehprogm_scratch() {
        let sysin = vec!["SCRATCH DSNAME=MY.DATA,VOL=VOL001".to_string()];
        let mut ctx = setup_ieh_context("IEHPROGM", sysin);
        let result = Iehprogm.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages[0].text.contains("SCRATCH"));
    }

    #[test]
    fn test_iehprogm_rename() {
        let sysin = vec!["RENAME DSNAME=OLD.NAME,NEWNAME=NEW.NAME,VOL=VOL001".to_string()];
        let mut ctx = setup_ieh_context("IEHPROGM", sysin);
        let result = Iehprogm.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages[0].text.contains("RENAME"));
        assert!(result.messages[0].text.contains("NEW.NAME"));
    }

    #[test]
    fn test_iehprogm_rename_missing_newname() {
        let sysin = vec!["RENAME DSNAME=OLD.NAME".to_string()];
        let mut ctx = setup_ieh_context("IEHPROGM", sysin);
        let result = Iehprogm.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);
    }

    #[test]
    fn test_iehprogm_catalog() {
        let sysin = vec!["CATLG DSNAME=MY.DATA,VOL=VOL001".to_string()];
        let mut ctx = setup_ieh_context("IEHPROGM", sysin);
        let result = Iehprogm.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages[0].text.contains("CATLG"));
    }

    #[test]
    fn test_iehprogm_uncatalog() {
        let sysin = vec!["UNCATLG DSNAME=MY.DATA".to_string()];
        let mut ctx = setup_ieh_context("IEHPROGM", sysin);
        let result = Iehprogm.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages[0].text.contains("UNCATLG"));
    }

    #[test]
    fn test_iehprogm_multiple_ops() {
        let sysin = vec![
            "SCRATCH DSNAME=OLD.DATA,VOL=VOL001".to_string(),
            "CATLG DSNAME=NEW.DATA,VOL=VOL002".to_string(),
        ];
        let mut ctx = setup_ieh_context("IEHPROGM", sysin);
        let result = Iehprogm.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert_eq!(result.messages.len(), 2);
    }

    #[test]
    fn test_iehprogm_no_sysin() {
        let mut ctx = setup_ieh_context("IEHPROGM", vec![]);
        let result = Iehprogm.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    #[test]
    fn test_iehprogm_invalid_sysin() {
        let sysin = vec!["GARBAGE".to_string()];
        let mut ctx = setup_ieh_context("IEHPROGM", sysin);
        let result = Iehprogm.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    // ─────── UTIL-107.2: IEHLIST — Catalog and VTOC Listing ───────

    #[test]
    fn test_iehlist_listctlg() {
        let sysin = vec!["LISTCTLG VOL=VOL001".to_string()];
        let mut ctx = setup_ieh_context("IEHLIST", sysin);
        let result = Iehlist.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages[0].text.contains("LISTCTLG"));
    }

    #[test]
    fn test_iehlist_listvtoc() {
        let sysin = vec!["LISTVTOC VOL=VOL001,FORMAT=DUMP".to_string()];
        let mut ctx = setup_ieh_context("IEHLIST", sysin);
        let result = Iehlist.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages[0].text.contains("LISTVTOC"));
    }

    #[test]
    fn test_iehlist_listpds() {
        let mut pds = PdsData::new();
        pds.add_member("MOD1", vec!["LINE 1".to_string()]);
        pds.add_member("MOD2", vec!["LINE A".to_string(), "LINE B".to_string()]);

        let sysin = vec!["LISTPDS DSNAME=MY.PDS".to_string()];
        let mut ctx = setup_ieh_context("IEHLIST", sysin);
        ctx.add_dd(DdAllocation::pds("SYSUT1", "MY.PDS", "SHR", pds));

        let result = Iehlist.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let output = ctx.sysprint_output();
        assert!(output.iter().any(|l| l.contains("MOD1")));
        assert!(output.iter().any(|l| l.contains("MOD2")));
    }

    #[test]
    fn test_iehlist_no_sysin() {
        let mut ctx = setup_ieh_context("IEHLIST", vec![]);
        let result = Iehlist.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    #[test]
    fn test_iehlist_multiple_ops() {
        let sysin = vec![
            "LISTCTLG".to_string(),
            "LISTVTOC VOL=VOL001".to_string(),
        ];
        let mut ctx = setup_ieh_context("IEHLIST", sysin);
        let result = Iehlist.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert_eq!(result.messages.len(), 2);
    }

    // ─────── UTIL-107.3: IEHMOVE — Dataset Move ───────

    #[test]
    fn test_iehmove_move_dataset() {
        let sysin = vec!["MOVE DSNAME=MY.DATA,FROM=VOL001,TO=VOL002".to_string()];
        let mut ctx = setup_ieh_context("IEHMOVE", sysin);
        let result = Iehmove.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages[0].text.contains("MOVE"));
    }

    #[test]
    fn test_iehmove_copy_dataset() {
        let sysin = vec!["COPY DSNAME=MY.DATA,FROM=VOL001,TO=VOL002".to_string()];
        let mut ctx = setup_ieh_context("IEHMOVE", sysin);
        let result = Iehmove.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages[0].text.contains("COPY"));
    }

    #[test]
    fn test_iehmove_move_pds() {
        let sysin = vec!["MOVE PDS=MY.PDS,FROM=VOL001,TO=VOL002".to_string()];
        let mut ctx = setup_ieh_context("IEHMOVE", sysin);
        let result = Iehmove.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages[0].text.contains("PDS=MY.PDS"));
    }

    #[test]
    fn test_iehmove_missing_to_volume() {
        let sysin = vec!["MOVE DSNAME=MY.DATA".to_string()];
        let mut ctx = setup_ieh_context("IEHMOVE", sysin);
        let result = Iehmove.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);
    }

    #[test]
    fn test_iehmove_no_sysin() {
        let mut ctx = setup_ieh_context("IEHMOVE", vec![]);
        let result = Iehmove.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    // ─────── UTIL-107.4-6: Tests via Registry ───────

    #[test]
    fn test_iehprogm_via_registry() {
        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(Iehprogm));
        let sysin = vec!["SCRATCH DSNAME=MY.DATA,VOL=V1".to_string()];
        let mut ctx = setup_ieh_context("IEHPROGM", sysin);
        let result = reg.dispatch("IEHPROGM", &mut ctx).unwrap();
        assert_eq!(result.condition_code, 0);
    }

    #[test]
    fn test_iehlist_via_registry() {
        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(Iehlist));
        let sysin = vec!["LISTCTLG".to_string()];
        let mut ctx = setup_ieh_context("IEHLIST", sysin);
        let result = reg.dispatch("IEHLIST", &mut ctx).unwrap();
        assert_eq!(result.condition_code, 0);
    }

    #[test]
    fn test_iehmove_via_registry() {
        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(Iehmove));
        let sysin = vec!["COPY DSNAME=MY.DATA,FROM=V1,TO=V2".to_string()];
        let mut ctx = setup_ieh_context("IEHMOVE", sysin);
        let result = reg.dispatch("IEHMOVE", &mut ctx).unwrap();
        assert_eq!(result.condition_code, 0);
    }

    // ─────── Parsing Tests ───────

    #[test]
    fn test_parse_iehprogm_ops() {
        let stmts = vec![
            "SCRATCH DSNAME=A,VOL=V1".to_string(),
            "RENAME DSNAME=B,NEWNAME=C".to_string(),
            "CATLG DSNAME=D,VOL=V2".to_string(),
            "UNCATLG DSNAME=E".to_string(),
        ];
        let ops = parse_iehprogm_sysin(&stmts);
        assert_eq!(ops.len(), 4);
        assert!(matches!(&ops[0], IehprogmOp::Scratch { dsname, .. } if dsname == "A"));
        assert!(matches!(&ops[1], IehprogmOp::Rename { dsname, newname, .. } if dsname == "B" && newname == "C"));
        assert!(matches!(&ops[2], IehprogmOp::Catalog { dsname, .. } if dsname == "D"));
        assert!(matches!(&ops[3], IehprogmOp::Uncatalog { dsname } if dsname == "E"));
    }

    #[test]
    fn test_parse_iehlist_ops() {
        let stmts = vec![
            "LISTCTLG VOL=V1".to_string(),
            "LISTVTOC VOL=V2,FORMAT=ABBREV".to_string(),
        ];
        let ops = parse_iehlist_sysin(&stmts);
        assert_eq!(ops.len(), 2);
        assert!(matches!(&ops[0], IehlistOp::ListCatalog { .. }));
        assert!(matches!(&ops[1], IehlistOp::ListVtoc { format: ListFormat::Abbreviated, .. }));
    }

    #[test]
    fn test_parse_iehmove_ops() {
        let stmts = vec![
            "MOVE DSNAME=A,FROM=V1,TO=V2".to_string(),
            "COPY PDS=B,TO=V3".to_string(),
        ];
        let ops = parse_iehmove_sysin(&stmts);
        assert_eq!(ops.len(), 2);
        assert!(matches!(&ops[0], IehmoveOp::Move { dsname, to_vol, .. } if dsname == "A" && to_vol == "V2"));
        assert!(matches!(&ops[1], IehmoveOp::CopyPds { dsname, to_vol, .. } if dsname == "B" && to_vol == "V3"));
    }

    #[test]
    fn test_comments_and_blanks_ignored() {
        let stmts = vec![
            "* THIS IS A COMMENT".to_string(),
            "".to_string(),
            "SCRATCH DSNAME=MY.DATA,VOL=V1".to_string(),
        ];
        let ops = parse_iehprogm_sysin(&stmts);
        assert_eq!(ops.len(), 1);
    }
}
