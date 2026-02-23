//! IEBUPDTE — PDS Update utility.
//!
//! Updates PDS members using inline control statements. IEBUPDTE reads
//! SYSIN input containing `./ ADD`, `./ REPL`, and `./ CHANGE` commands
//! interspersed with data records to create, replace, or modify PDS members.
//!
//! ## Control Statement Syntax
//!
//! ```text
//! ./ ADD    NAME=member[,LIST=ALL]     Create a new member
//! ./ REPL   NAME=member[,LIST=ALL]     Replace an existing member
//! ./ CHANGE NAME=member                Modify records by sequence number
//! ./ NUMBER NEW1=seqno,INCR=incr       Renumber records
//! ./ DELETE SEQ1=start,SEQ2=end        Delete records by sequence range
//! ./ ENDUP                             End of input
//! ```
//!
//! ## Condition Codes
//!
//! | CC | Meaning |
//! |----|---------|
//! | 0  | All operations successful |
//! | 4  | Warning — member already exists for ADD |
//! | 8  | Error — member not found for REPL/CHANGE |
//! | 12 | Severe — invalid syntax or missing DD |

use crate::{
    format_message_id, MessageSeverity, PdsData, UtilityContext, UtilityMessage, UtilityProgram,
    UtilityResult,
};

// ─────────────────────── Control Statement Parsing ───────────────────────

/// Control statement prefix for IEBUPDTE.
const CMD_PREFIX: &str = "./";

/// Parsed IEBUPDTE operation.
#[derive(Debug, Clone)]
pub enum IebupdteOp {
    /// `./ ADD NAME=member` — create a new member with following data.
    Add {
        name: String,
        data: Vec<String>,
    },
    /// `./ REPL NAME=member` — replace an existing member.
    Repl {
        name: String,
        data: Vec<String>,
    },
    /// `./ CHANGE NAME=member` — modify records in an existing member.
    Change {
        name: String,
        modifications: Vec<ChangeAction>,
    },
}

/// A modification action within a CHANGE operation.
#[derive(Debug, Clone)]
pub enum ChangeAction {
    /// Insert or replace a record at a specific sequence number.
    InsertRecord {
        seq_number: u32,
        data: String,
    },
    /// Delete records in a sequence range.
    Delete {
        seq1: u32,
        seq2: u32,
    },
    /// Renumber records starting at new1 with increment incr.
    Number {
        new1: u32,
        incr: u32,
    },
}

/// Parse IEBUPDTE SYSIN control statements into operations.
pub fn parse_iebupdte_sysin(statements: &[String]) -> Vec<IebupdteOp> {
    let mut ops = Vec::new();
    let mut i = 0;

    while i < statements.len() {
        let trimmed = statements[i].trim();

        let Some(rest) = trimmed.strip_prefix(CMD_PREFIX) else {
            i += 1;
            continue;
        };

        let cmd = rest.trim().to_uppercase();

        if cmd.starts_with("ENDUP") {
            break;
        }

        if cmd.starts_with("ADD ") || cmd == "ADD" {
            let name = extract_name(&cmd).unwrap_or_default();
            i += 1;
            let data = collect_data_records(&statements[i..]);
            let count = data.len();
            ops.push(IebupdteOp::Add { name, data });
            i += count;
        } else if cmd.starts_with("REPL ") || cmd == "REPL" {
            let name = extract_name(&cmd).unwrap_or_default();
            i += 1;
            let data = collect_data_records(&statements[i..]);
            let count = data.len();
            ops.push(IebupdteOp::Repl { name, data });
            i += count;
        } else if cmd.starts_with("CHANGE ") || cmd == "CHANGE" {
            let name = extract_name(&cmd).unwrap_or_default();
            i += 1;
            let (mods, consumed) = collect_change_actions(&statements[i..]);
            ops.push(IebupdteOp::Change {
                name,
                modifications: mods,
            });
            i += consumed;
        } else {
            i += 1;
        }
    }

    ops
}

fn extract_name(cmd: &str) -> Option<String> {
    let pos = cmd.find("NAME=")?;
    let start = pos + 5;
    let rest = &cmd[start..];
    let end = rest.find([',', ' ']).unwrap_or(rest.len());
    let name = rest[..end].trim().to_string();
    if name.is_empty() {
        None
    } else {
        Some(name)
    }
}

/// Collect data records until the next `./ ` command or end of input.
fn collect_data_records(statements: &[String]) -> Vec<String> {
    let mut data = Vec::new();
    for stmt in statements {
        let trimmed = stmt.trim();
        if trimmed.starts_with(CMD_PREFIX) {
            break;
        }
        data.push(stmt.clone());
    }
    data
}

/// Collect CHANGE actions (NUMBER, DELETE, data records with seq numbers).
fn collect_change_actions(statements: &[String]) -> (Vec<ChangeAction>, usize) {
    let mut actions = Vec::new();
    let mut consumed = 0;

    for stmt in statements {
        let trimmed = stmt.trim();

        if let Some(rest) = trimmed.strip_prefix(CMD_PREFIX) {
            let cmd = rest.trim().to_uppercase();

            if cmd.starts_with("NUMBER ") {
                if let Some(action) = parse_number_cmd(&cmd) {
                    actions.push(action);
                }
                consumed += 1;
            } else if cmd.starts_with("DELETE ") {
                if let Some(action) = parse_delete_cmd(&cmd) {
                    actions.push(action);
                }
                consumed += 1;
            } else {
                // Next operation — stop collecting.
                break;
            }
        } else {
            // Data record with implicit sequence number.
            // In IEBUPDTE CHANGE mode, data records replace at their
            // sequence number position. We use line position as seq.
            actions.push(ChangeAction::InsertRecord {
                seq_number: (consumed as u32 + 1) * 100,
                data: stmt.clone(),
            });
            consumed += 1;
        }
    }

    (actions, consumed)
}

fn parse_number_cmd(cmd: &str) -> Option<ChangeAction> {
    let new1 = extract_numeric_param(cmd, "NEW1=")?;
    let incr = extract_numeric_param(cmd, "INCR=").unwrap_or(10);
    Some(ChangeAction::Number { new1, incr })
}

fn parse_delete_cmd(cmd: &str) -> Option<ChangeAction> {
    let seq1 = extract_numeric_param(cmd, "SEQ1=")?;
    let seq2 = extract_numeric_param(cmd, "SEQ2=").unwrap_or(seq1);
    Some(ChangeAction::Delete { seq1, seq2 })
}

fn extract_numeric_param(stmt: &str, key: &str) -> Option<u32> {
    let pos = stmt.find(key)?;
    let start = pos + key.len();
    let rest = &stmt[start..];
    let end = rest.find([',', ' ']).unwrap_or(rest.len());
    rest[..end].parse().ok()
}

// ─────────────────────── IEBUPDTE Implementation ───────────────────────

/// IEBUPDTE — PDS update utility.
pub struct Iebupdte;

impl UtilityProgram for Iebupdte {
    fn name(&self) -> &str {
        "IEBUPDTE"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let stmts = context.read_sysin();
        if stmts.is_empty() {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 401, MessageSeverity::Severe),
                "NO CONTROL STATEMENTS IN SYSIN",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(12, vec![msg]);
        }

        let ops = parse_iebupdte_sysin(&stmts);
        if ops.is_empty() {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 402, MessageSeverity::Severe),
                "NO VALID OPERATIONS FOUND",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(12, vec![msg]);
        }

        // Get or create target PDS on SYSUT2.
        let out_dd = context.get_dd_mut("SYSUT2");
        let Some(out_dd) = out_dd else {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 403, MessageSeverity::Severe),
                "SYSUT2 DD NOT ALLOCATED",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(12, vec![msg]);
        };

        if out_dd.pds_data.is_none() {
            out_dd.pds_data = Some(PdsData::new());
        }
        let pds = out_dd.pds_data.as_mut().unwrap();

        let mut max_cc: u32 = 0;
        let mut all_messages = Vec::new();

        for op in &ops {
            match op {
                IebupdteOp::Add { name, data } => {
                    if pds.has_member(name) {
                        let msg = UtilityMessage::warning(
                            &format_message_id("IEB", 410, MessageSeverity::Warning),
                            &format!("MEMBER {name} ALREADY EXISTS — NOT ADDED"),
                        );
                        all_messages.push(msg);
                        max_cc = max_cc.max(4);
                    } else {
                        pds.add_member(name, data.clone());
                        let msg = UtilityMessage::info(
                            &format_message_id("IEB", 411, MessageSeverity::Info),
                            &format!(
                                "MEMBER {name} ADDED ({} RECORDS)",
                                data.len()
                            ),
                        );
                        all_messages.push(msg);
                    }
                }
                IebupdteOp::Repl { name, data } => {
                    if !pds.has_member(name) {
                        let msg = UtilityMessage::error(
                            &format_message_id("IEB", 412, MessageSeverity::Error),
                            &format!("MEMBER {name} NOT FOUND — CANNOT REPLACE"),
                        );
                        all_messages.push(msg);
                        max_cc = max_cc.max(8);
                    } else {
                        // Replace: delete old and add new.
                        pds.members.remove(&name.to_uppercase());
                        pds.add_member(name, data.clone());
                        let msg = UtilityMessage::info(
                            &format_message_id("IEB", 413, MessageSeverity::Info),
                            &format!(
                                "MEMBER {name} REPLACED ({} RECORDS)",
                                data.len()
                            ),
                        );
                        all_messages.push(msg);
                    }
                }
                IebupdteOp::Change { name, modifications } => {
                    if !pds.has_member(name) {
                        let msg = UtilityMessage::error(
                            &format_message_id("IEB", 414, MessageSeverity::Error),
                            &format!("MEMBER {name} NOT FOUND — CANNOT CHANGE"),
                        );
                        all_messages.push(msg);
                        max_cc = max_cc.max(8);
                    } else {
                        let member = pds.members.get_mut(&name.to_uppercase()).unwrap();
                        apply_changes(&mut member.content, modifications);
                        let msg = UtilityMessage::info(
                            &format_message_id("IEB", 415, MessageSeverity::Info),
                            &format!(
                                "MEMBER {name} CHANGED ({} MODIFICATIONS APPLIED)",
                                modifications.len()
                            ),
                        );
                        all_messages.push(msg);
                    }
                }
            }
        }

        // Write messages to SYSPRINT.
        for msg in &all_messages {
            context.write_utility_message(msg);
        }

        UtilityResult::new(max_cc, all_messages)
    }
}

/// Apply CHANGE modifications to a member's content.
fn apply_changes(content: &mut Vec<String>, modifications: &[ChangeAction]) {
    for action in modifications {
        match action {
            ChangeAction::Delete { seq1, seq2 } => {
                // Delete records in the sequence range.
                // Sequence numbers are 1-based, mapped to 0-based indices.
                // We use seq/100 as record index for simplicity.
                let start = (*seq1 as usize).saturating_sub(1);
                let end = (*seq2 as usize).min(content.len());
                if start < content.len() {
                    let drain_end = end.min(content.len());
                    content.drain(start..drain_end);
                }
            }
            ChangeAction::Number { new1, incr } => {
                // Renumber is a metadata operation; in our model we just
                // track that it was requested. The actual sequence numbers
                // are implicit (line position).
                let _ = (new1, incr);
            }
            ChangeAction::InsertRecord { seq_number, data } => {
                // Insert/replace at the sequence number position.
                let idx = (*seq_number as usize / 100).saturating_sub(1);
                if idx < content.len() {
                    content[idx] = data.clone();
                } else {
                    content.push(data.clone());
                }
            }
        }
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{DdAllocation, PdsData, UtilityRegistry};

    fn setup_updte_context(pds: PdsData, sysin: Vec<String>) -> UtilityContext {
        let mut ctx = UtilityContext::new("STEP01", "IEBUPDTE");
        ctx.add_dd(DdAllocation::pds("SYSUT2", "MY.PDS", "OLD", pds));
        ctx.add_dd(DdAllocation::inline("SYSIN", sysin));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx
    }

    // ─────── UTIL-103.1: ADD Operation ───────

    #[test]
    fn test_add_new_member() {
        let pds = PdsData::new();
        let sysin = vec![
            "./ ADD NAME=MODA".to_string(),
            "LINE 1".to_string(),
            "LINE 2".to_string(),
            "./ ENDUP".to_string(),
        ];

        let mut ctx = setup_updte_context(pds, sysin);
        let result = Iebupdte.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        let pds = out.pds_data.as_ref().unwrap();
        assert!(pds.has_member("MODA"));
        let member = pds.get_member("MODA").unwrap();
        assert_eq!(member.content.len(), 2);
        assert_eq!(member.content[0], "LINE 1");
    }

    #[test]
    fn test_add_multiple_members() {
        let pds = PdsData::new();
        let sysin = vec![
            "./ ADD NAME=MOD1".to_string(),
            "DATA1".to_string(),
            "./ ADD NAME=MOD2".to_string(),
            "DATA2A".to_string(),
            "DATA2B".to_string(),
            "./ ADD NAME=MOD3".to_string(),
            "DATA3".to_string(),
            "./ ENDUP".to_string(),
        ];

        let mut ctx = setup_updte_context(pds, sysin);
        let result = Iebupdte.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        let pds = out.pds_data.as_ref().unwrap();
        assert_eq!(pds.member_count(), 3);
        assert!(pds.has_member("MOD1"));
        assert!(pds.has_member("MOD2"));
        assert!(pds.has_member("MOD3"));
        assert_eq!(pds.get_member("MOD2").unwrap().content.len(), 2);
    }

    #[test]
    fn test_add_existing_member_warns() {
        let mut pds = PdsData::new();
        pds.add_member("EXISTING", vec!["OLD DATA".to_string()]);

        let sysin = vec![
            "./ ADD NAME=EXISTING".to_string(),
            "NEW DATA".to_string(),
            "./ ENDUP".to_string(),
        ];

        let mut ctx = setup_updte_context(pds, sysin);
        let result = Iebupdte.execute(&mut ctx);
        assert_eq!(result.condition_code, 4);

        // Original member should be unchanged.
        let out = ctx.get_dd("SYSUT2").unwrap();
        let pds = out.pds_data.as_ref().unwrap();
        assert_eq!(
            pds.get_member("EXISTING").unwrap().content[0],
            "OLD DATA"
        );
    }

    #[test]
    fn test_add_empty_member() {
        let pds = PdsData::new();
        let sysin = vec![
            "./ ADD NAME=EMPTY".to_string(),
            "./ ENDUP".to_string(),
        ];

        let mut ctx = setup_updte_context(pds, sysin);
        let result = Iebupdte.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        let pds = out.pds_data.as_ref().unwrap();
        assert!(pds.has_member("EMPTY"));
        assert!(pds.get_member("EMPTY").unwrap().content.is_empty());
    }

    // ─────── UTIL-103.2: REPL Operation ───────

    #[test]
    fn test_repl_existing_member() {
        let mut pds = PdsData::new();
        pds.add_member("MODA", vec!["OLD LINE 1".to_string(), "OLD LINE 2".to_string()]);

        let sysin = vec![
            "./ REPL NAME=MODA".to_string(),
            "NEW LINE 1".to_string(),
            "NEW LINE 2".to_string(),
            "NEW LINE 3".to_string(),
            "./ ENDUP".to_string(),
        ];

        let mut ctx = setup_updte_context(pds, sysin);
        let result = Iebupdte.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        let pds = out.pds_data.as_ref().unwrap();
        let member = pds.get_member("MODA").unwrap();
        assert_eq!(member.content.len(), 3);
        assert_eq!(member.content[0], "NEW LINE 1");
    }

    #[test]
    fn test_repl_nonexistent_member_errors() {
        let pds = PdsData::new();
        let sysin = vec![
            "./ REPL NAME=NOSUCH".to_string(),
            "DATA".to_string(),
            "./ ENDUP".to_string(),
        ];

        let mut ctx = setup_updte_context(pds, sysin);
        let result = Iebupdte.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);
    }

    // ─────── UTIL-103.3: CHANGE Operation ───────

    #[test]
    fn test_change_insert_record() {
        let mut pds = PdsData::new();
        pds.add_member(
            "MODA",
            vec![
                "LINE 1".to_string(),
                "LINE 2".to_string(),
                "LINE 3".to_string(),
            ],
        );

        let sysin = vec![
            "./ CHANGE NAME=MODA".to_string(),
            "CHANGED LINE 2".to_string(), // seq_number=100 → replaces index 0
            "./ ENDUP".to_string(),
        ];

        let mut ctx = setup_updte_context(pds, sysin);
        let result = Iebupdte.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        let member = out.pds_data.as_ref().unwrap().get_member("MODA").unwrap();
        assert_eq!(member.content[0], "CHANGED LINE 2");
        assert_eq!(member.content[1], "LINE 2"); // unchanged
    }

    #[test]
    fn test_change_delete_records() {
        let mut pds = PdsData::new();
        pds.add_member(
            "MODA",
            vec![
                "LINE 1".to_string(),
                "LINE 2".to_string(),
                "LINE 3".to_string(),
                "LINE 4".to_string(),
                "LINE 5".to_string(),
            ],
        );

        let sysin = vec![
            "./ CHANGE NAME=MODA".to_string(),
            "./ DELETE SEQ1=2,SEQ2=4".to_string(),
            "./ ENDUP".to_string(),
        ];

        let mut ctx = setup_updte_context(pds, sysin);
        let result = Iebupdte.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        let member = out.pds_data.as_ref().unwrap().get_member("MODA").unwrap();
        // Records 2-4 (indices 1-3) deleted, leaving records 1 and 5.
        assert_eq!(member.content.len(), 2);
        assert_eq!(member.content[0], "LINE 1");
        assert_eq!(member.content[1], "LINE 5");
    }

    #[test]
    fn test_change_nonexistent_member_errors() {
        let pds = PdsData::new();
        let sysin = vec![
            "./ CHANGE NAME=NOSUCH".to_string(),
            "./ DELETE SEQ1=1,SEQ2=1".to_string(),
            "./ ENDUP".to_string(),
        ];

        let mut ctx = setup_updte_context(pds, sysin);
        let result = Iebupdte.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);
    }

    #[test]
    fn test_change_number_command_accepted() {
        let mut pds = PdsData::new();
        pds.add_member("MODA", vec!["LINE 1".to_string()]);

        let sysin = vec![
            "./ CHANGE NAME=MODA".to_string(),
            "./ NUMBER NEW1=100,INCR=10".to_string(),
            "./ ENDUP".to_string(),
        ];

        let mut ctx = setup_updte_context(pds, sysin);
        let result = Iebupdte.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
    }

    // ─────── UTIL-103.4: IEBUPDTE Tests ───────

    #[test]
    fn test_no_sysin_cc12() {
        let mut ctx = UtilityContext::new("STEP01", "IEBUPDTE");
        ctx.add_dd(DdAllocation::pds(
            "SYSUT2",
            "MY.PDS",
            "OLD",
            PdsData::new(),
        ));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebupdte.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    #[test]
    fn test_invalid_sysin_cc12() {
        let sysin = vec!["GARBAGE".to_string()];
        let mut ctx = setup_updte_context(PdsData::new(), sysin);
        let result = Iebupdte.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    #[test]
    fn test_missing_sysut2_cc12() {
        let mut ctx = UtilityContext::new("STEP01", "IEBUPDTE");
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec!["./ ADD NAME=X".to_string(), "D".to_string(), "./ ENDUP".to_string()],
        ));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebupdte.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    #[test]
    fn test_via_registry() {
        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(Iebupdte));

        let sysin = vec![
            "./ ADD NAME=TEST".to_string(),
            "DATA".to_string(),
            "./ ENDUP".to_string(),
        ];
        let mut ctx = setup_updte_context(PdsData::new(), sysin);

        let result = reg.dispatch("IEBUPDTE", &mut ctx).unwrap();
        assert_eq!(result.condition_code, 0);
    }

    #[test]
    fn test_mixed_operations() {
        let mut pds = PdsData::new();
        pds.add_member("OLD", vec!["ORIGINAL".to_string()]);

        let sysin = vec![
            "./ ADD NAME=NEW1".to_string(),
            "NEW DATA 1".to_string(),
            "./ ADD NAME=NEW2".to_string(),
            "NEW DATA 2".to_string(),
            "./ REPL NAME=OLD".to_string(),
            "REPLACED".to_string(),
            "./ ENDUP".to_string(),
        ];

        let mut ctx = setup_updte_context(pds, sysin);
        let result = Iebupdte.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        let pds = out.pds_data.as_ref().unwrap();
        assert_eq!(pds.member_count(), 3);
        assert_eq!(
            pds.get_member("OLD").unwrap().content[0],
            "REPLACED"
        );
    }

    #[test]
    fn test_endup_stops_processing() {
        let pds = PdsData::new();
        let sysin = vec![
            "./ ADD NAME=BEFORE".to_string(),
            "DATA".to_string(),
            "./ ENDUP".to_string(),
            "./ ADD NAME=AFTER".to_string(),
            "SHOULD NOT APPEAR".to_string(),
        ];

        let mut ctx = setup_updte_context(pds, sysin);
        Iebupdte.execute(&mut ctx);

        let out = ctx.get_dd("SYSUT2").unwrap();
        let pds = out.pds_data.as_ref().unwrap();
        assert!(pds.has_member("BEFORE"));
        assert!(!pds.has_member("AFTER"));
    }

    #[test]
    fn test_sysprint_messages() {
        let sysin = vec![
            "./ ADD NAME=TEST".to_string(),
            "DATA".to_string(),
            "./ ENDUP".to_string(),
        ];
        let mut ctx = setup_updte_context(PdsData::new(), sysin);
        Iebupdte.execute(&mut ctx);

        let output = ctx.sysprint_output();
        assert!(!output.is_empty());
        assert!(output.iter().any(|l| l.contains("MEMBER TEST ADDED")));
    }

    // ─── Parsing tests ───

    #[test]
    fn test_parse_add() {
        let stmts = vec![
            "./ ADD NAME=MODA".to_string(),
            "LINE1".to_string(),
            "./ ENDUP".to_string(),
        ];
        let ops = parse_iebupdte_sysin(&stmts);
        assert_eq!(ops.len(), 1);
        if let IebupdteOp::Add { name, data } = &ops[0] {
            assert_eq!(name, "MODA");
            assert_eq!(data, &["LINE1"]);
        } else {
            panic!("Expected Add");
        }
    }

    #[test]
    fn test_parse_repl() {
        let stmts = vec![
            "./ REPL NAME=MODB".to_string(),
            "NEW".to_string(),
            "./ ENDUP".to_string(),
        ];
        let ops = parse_iebupdte_sysin(&stmts);
        assert_eq!(ops.len(), 1);
        assert!(matches!(&ops[0], IebupdteOp::Repl { name, .. } if name == "MODB"));
    }

    #[test]
    fn test_parse_change_with_delete() {
        let stmts = vec![
            "./ CHANGE NAME=MODC".to_string(),
            "./ DELETE SEQ1=5,SEQ2=10".to_string(),
            "./ ENDUP".to_string(),
        ];
        let ops = parse_iebupdte_sysin(&stmts);
        assert_eq!(ops.len(), 1);
        if let IebupdteOp::Change { name, modifications } = &ops[0] {
            assert_eq!(name, "MODC");
            assert_eq!(modifications.len(), 1);
            assert!(matches!(&modifications[0], ChangeAction::Delete { seq1: 5, seq2: 10 }));
        } else {
            panic!("Expected Change");
        }
    }
}
