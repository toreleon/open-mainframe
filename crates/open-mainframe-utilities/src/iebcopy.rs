//! IEBCOPY — PDS Copy, Compress, and Merge utility.
//!
//! Implements the z/OS IEBCOPY utility for copying, compressing, and merging
//! Partitioned Data Sets (PDS). IEBCOPY is the most commonly used PDS
//! maintenance utility on z/OS.
//!
//! ## Control Statement Syntax
//!
//! ```text
//! COPY OUTDD=ddname,INDD=(dd1,dd2,...) [,LIST=YES|NO]
//! SELECT MEMBER=(name1,name2,...)
//! EXCLUDE MEMBER=(name1,name2,...)
//! ```
//!
//! ## Condition Codes
//!
//! | CC | Meaning |
//! |----|---------|
//! | 0  | Operation completed successfully |
//! | 4  | Warning — some members skipped (already exist, REPLACE not specified) |
//! | 8  | Error — member not found or I/O error |
//! | 12 | Severe — SYSIN/DD error or invalid control statement |

use std::collections::HashSet;

use crate::{
    format_message_id, MessageSeverity, PdsData, PdsMemberData, UnloadData,
    UtilityContext, UtilityMessage, UtilityProgram, UtilityResult,
};

// ─────────────────────── Unload Format Constants ───────────────────────

/// Header marker for the start of the unload format.
const UNLOAD_HEADER: &str = "IEBCOPY UNLOAD";
/// Marker for the start of a member in unload format.
const MEMBER_HEADER_PREFIX: &str = "./ ADD NAME=";
/// Marker for end of member data.
const MEMBER_END: &str = "./ ENDUP";

// ─────────────────────── Control Statement Parsing ───────────────────────

/// A parsed IEBCOPY control statement operation.
#[derive(Debug, Clone)]
pub enum IebcopyOp {
    /// COPY operation with INDD and OUTDD.
    Copy {
        outdd: String,
        indd: Vec<String>,
        replace: bool,
    },
    /// SELECT specific members.
    Select { members: Vec<String> },
    /// EXCLUDE specific members.
    Exclude { members: Vec<String> },
}

/// Parse IEBCOPY SYSIN control statements.
///
/// Returns a list of parsed operations.
pub fn parse_control_statements(statements: &[String]) -> Vec<IebcopyOp> {
    let mut ops = Vec::new();

    for stmt in statements {
        let trimmed = stmt.trim().to_uppercase();
        if trimmed.is_empty() || trimmed.starts_with("/*") {
            continue;
        }

        if trimmed.starts_with("COPY ") || trimmed.starts_with("COPY,") {
            if let Some(op) = parse_copy_stmt(&trimmed) {
                ops.push(op);
            }
        } else if trimmed.starts_with("SELECT ") {
            if let Some(op) = parse_select_stmt(&trimmed) {
                ops.push(op);
            }
        } else if trimmed.starts_with("EXCLUDE ") {
            if let Some(op) = parse_exclude_stmt(&trimmed) {
                ops.push(op);
            }
        }
    }

    ops
}

fn parse_copy_stmt(stmt: &str) -> Option<IebcopyOp> {
    let outdd = extract_keyword(stmt, "OUTDD=")?;
    let indd_str = extract_keyword(stmt, "INDD=")?;

    let indd: Vec<String> = if indd_str.starts_with('(') && indd_str.ends_with(')') {
        indd_str[1..indd_str.len() - 1]
            .split(',')
            .map(|s| s.trim().to_string())
            .collect()
    } else {
        vec![indd_str]
    };

    // Check for REPLACE option anywhere in the statement.
    let replace = stmt.contains(",REPLACE") || stmt.contains(" REPLACE");

    Some(IebcopyOp::Copy {
        outdd,
        indd,
        replace,
    })
}

fn parse_select_stmt(stmt: &str) -> Option<IebcopyOp> {
    let members_str = extract_keyword(stmt, "MEMBER=")?;
    let members = parse_member_list(&members_str);
    Some(IebcopyOp::Select { members })
}

fn parse_exclude_stmt(stmt: &str) -> Option<IebcopyOp> {
    let members_str = extract_keyword(stmt, "MEMBER=")?;
    let members = parse_member_list(&members_str);
    Some(IebcopyOp::Exclude { members })
}

fn extract_keyword(stmt: &str, keyword: &str) -> Option<String> {
    let pos = stmt.find(keyword)?;
    let start = pos + keyword.len();
    let rest = &stmt[start..];

    if rest.starts_with('(') {
        // Parenthesized value — find matching close paren.
        let end = rest.find(')')?;
        Some(rest[..=end].to_string())
    } else {
        // Comma or space delimited.
        let end = rest.find([',', ' ']).unwrap_or(rest.len());
        Some(rest[..end].to_string())
    }
}

fn parse_member_list(s: &str) -> Vec<String> {
    let inner = if s.starts_with('(') && s.ends_with(')') {
        &s[1..s.len() - 1]
    } else {
        s
    };

    inner
        .split(',')
        .map(|m| m.trim().to_string())
        .filter(|m| !m.is_empty())
        .collect()
}

// ─────────────────────── IEBCOPY Implementation ───────────────────────

/// IEBCOPY — PDS Copy, Compress, and Merge utility.
pub struct Iebcopy;

impl UtilityProgram for Iebcopy {
    fn name(&self) -> &str {
        "IEBCOPY"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let stmts = context.read_sysin();
        if stmts.is_empty() {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 1001, MessageSeverity::Severe),
                "NO CONTROL STATEMENTS IN SYSIN",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(12, vec![msg]);
        }

        let ops = parse_control_statements(&stmts);
        if ops.is_empty() {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 1002, MessageSeverity::Severe),
                "NO VALID CONTROL STATEMENTS FOUND",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(12, vec![msg]);
        }

        let mut max_cc: u32 = 0;
        let mut all_messages = Vec::new();

        // Collect all SELECT/EXCLUDE before processing COPY.
        let mut select_members: Option<HashSet<String>> = None;
        let mut exclude_members: Option<HashSet<String>> = None;

        for op in &ops {
            match op {
                IebcopyOp::Select { members } => {
                    select_members =
                        Some(members.iter().map(|m| m.to_uppercase()).collect());
                }
                IebcopyOp::Exclude { members } => {
                    exclude_members =
                        Some(members.iter().map(|m| m.to_uppercase()).collect());
                }
                IebcopyOp::Copy { .. } => {}
            }
        }

        // Process COPY operations.
        for op in &ops {
            if let IebcopyOp::Copy {
                outdd,
                indd,
                replace,
            } = op
            {
                let result = self.execute_copy(
                    context,
                    outdd,
                    indd,
                    *replace,
                    select_members.as_ref(),
                    exclude_members.as_ref(),
                );
                max_cc = max_cc.max(result.condition_code);
                all_messages.extend(result.messages);
            }
        }

        // Write all messages to SYSPRINT.
        for msg in &all_messages {
            context.write_utility_message(msg);
        }

        UtilityResult::new(max_cc, all_messages)
    }
}

impl Iebcopy {
    fn execute_copy(
        &self,
        context: &mut UtilityContext,
        outdd: &str,
        indd_list: &[String],
        replace: bool,
        select: Option<&HashSet<String>>,
        exclude: Option<&HashSet<String>>,
    ) -> UtilityResult {
        let mut messages = Vec::new();
        let mut max_cc: u32 = 0;

        // Check if this is a compress operation (same INDD and OUTDD).
        if indd_list.len() == 1 && indd_list[0] == outdd {
            return self.execute_compress(context, outdd);
        }

        // Check for load from sequential (unload format → PDS).
        if indd_list.len() == 1 {
            if let Some(in_dd) = context.get_dd(&indd_list[0]) {
                if in_dd.dsorg.as_deref() == Some("PS") && in_dd.unload_data.is_some() {
                    return self.execute_load(context, &indd_list[0], outdd);
                }
            }
        }

        // Check for unload to sequential (PDS → sequential).
        if let Some(out_dd) = context.get_dd(outdd) {
            if out_dd.dsorg.as_deref() == Some("PS") {
                return self.execute_unload(context, &indd_list[0], outdd);
            }
        }

        // Collect members from all input PDS datasets.
        let mut source_members: Vec<(String, PdsMemberData)> = Vec::new();

        for in_name in indd_list {
            let Some(in_dd) = context.get_dd(in_name) else {
                let msg = UtilityMessage::severe(
                    &format_message_id("IEB", 1003, MessageSeverity::Severe),
                    &format!("DD {in_name} NOT ALLOCATED"),
                );
                messages.push(msg);
                return UtilityResult::new(12, messages);
            };

            let Some(ref pds) = in_dd.pds_data else {
                let msg = UtilityMessage::severe(
                    &format_message_id("IEB", 1004, MessageSeverity::Severe),
                    &format!("DD {in_name} IS NOT A PDS"),
                );
                messages.push(msg);
                return UtilityResult::new(12, messages);
            };

            for member in pds.members.values().filter(|m| !m.deleted) {
                source_members.push((in_name.clone(), member.clone()));
            }
        }

        // Apply SELECT/EXCLUDE filters.
        let filtered: Vec<(String, PdsMemberData)> = source_members
            .into_iter()
            .filter(|(_, m)| {
                let name = m.name.to_uppercase();
                if let Some(sel) = select {
                    return sel.contains(&name);
                }
                if let Some(exc) = exclude {
                    return !exc.contains(&name);
                }
                true
            })
            .collect();

        // Get or create output PDS.
        let out_dd = context.get_dd_mut(outdd);
        let Some(out_dd) = out_dd else {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 1005, MessageSeverity::Severe),
                &format!("DD {outdd} NOT ALLOCATED"),
            );
            messages.push(msg);
            return UtilityResult::new(12, messages);
        };

        if out_dd.pds_data.is_none() {
            out_dd.pds_data = Some(PdsData::new());
        }

        let out_pds = out_dd.pds_data.as_mut().unwrap();

        let mut copied = 0u32;
        let mut skipped = 0u32;

        for (_source_dd, member) in &filtered {
            if out_pds.has_member(&member.name) {
                if replace {
                    out_pds
                        .members
                        .insert(member.name.clone(), member.clone());
                    copied += 1;
                } else {
                    let msg = UtilityMessage::warning(
                        &format_message_id("IEB", 1014, MessageSeverity::Warning),
                        &format!(
                            "MEMBER {} ALREADY EXISTS IN OUTPUT - NOT REPLACED",
                            member.name
                        ),
                    );
                    messages.push(msg);
                    skipped += 1;
                    max_cc = max_cc.max(4);
                }
            } else {
                out_pds
                    .members
                    .insert(member.name.clone(), member.clone());
                copied += 1;
            }
        }

        let summary = UtilityMessage::info(
            &format_message_id("IEB", 1013, MessageSeverity::Info),
            &format!("{copied} MEMBERS COPIED, {skipped} MEMBERS NOT REPLACED"),
        );
        messages.push(summary);

        UtilityResult::new(max_cc, messages)
    }

    fn execute_compress(&self, context: &mut UtilityContext, ddname: &str) -> UtilityResult {
        let mut messages = Vec::new();

        let Some(dd) = context.get_dd_mut(ddname) else {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 1003, MessageSeverity::Severe),
                &format!("DD {ddname} NOT ALLOCATED"),
            );
            messages.push(msg);
            return UtilityResult::new(12, messages);
        };

        let Some(ref mut pds) = dd.pds_data else {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 1004, MessageSeverity::Severe),
                &format!("DD {ddname} IS NOT A PDS"),
            );
            messages.push(msg);
            return UtilityResult::new(12, messages);
        };

        let deleted_count = pds.deleted_count();
        let before_count = pds.members.len();
        pds.compress();
        let after_count = pds.members.len();

        let msg = UtilityMessage::info(
            &format_message_id("IEB", 1017, MessageSeverity::Info),
            &format!(
                "COMPRESS SUCCESSFUL: {deleted_count} DELETED MEMBERS REMOVED, \
                 {after_count} MEMBERS REMAINING (WAS {before_count})"
            ),
        );
        messages.push(msg);

        UtilityResult::success_with(messages)
    }

    fn execute_unload(
        &self,
        context: &mut UtilityContext,
        indd: &str,
        outdd: &str,
    ) -> UtilityResult {
        let mut messages = Vec::new();

        // Read source PDS.
        let in_dd = context.get_dd(indd);
        let Some(in_dd) = in_dd else {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 1003, MessageSeverity::Severe),
                &format!("DD {indd} NOT ALLOCATED"),
            );
            messages.push(msg);
            return UtilityResult::new(12, messages);
        };

        let Some(ref pds) = in_dd.pds_data else {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 1004, MessageSeverity::Severe),
                &format!("DD {indd} IS NOT A PDS"),
            );
            messages.push(msg);
            return UtilityResult::new(12, messages);
        };

        // Build unload records.
        let mut records = Vec::new();
        records.push(UNLOAD_HEADER.to_string());

        let mut member_count = 0u32;
        for member in pds.members.values().filter(|m| !m.deleted) {
            records.push(format!("{}{}", MEMBER_HEADER_PREFIX, member.name));
            for line in &member.content {
                records.push(line.clone());
            }
            member_count += 1;
        }
        records.push(MEMBER_END.to_string());

        // Store in output DD.
        let out_dd = context.get_dd_mut(outdd);
        let Some(out_dd) = out_dd else {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 1005, MessageSeverity::Severe),
                &format!("DD {outdd} NOT ALLOCATED"),
            );
            messages.push(msg);
            return UtilityResult::new(12, messages);
        };

        out_dd.unload_data = Some(UnloadData { records });

        let msg = UtilityMessage::info(
            &format_message_id("IEB", 1018, MessageSeverity::Info),
            &format!("{member_count} MEMBERS UNLOADED TO SEQUENTIAL"),
        );
        messages.push(msg);

        UtilityResult::success_with(messages)
    }

    fn execute_load(
        &self,
        context: &mut UtilityContext,
        indd: &str,
        outdd: &str,
    ) -> UtilityResult {
        let mut messages = Vec::new();

        // Read unload data from input.
        let in_dd = context.get_dd(indd);
        let Some(in_dd) = in_dd else {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 1003, MessageSeverity::Severe),
                &format!("DD {indd} NOT ALLOCATED"),
            );
            messages.push(msg);
            return UtilityResult::new(12, messages);
        };

        let Some(ref unload) = in_dd.unload_data else {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 1019, MessageSeverity::Severe),
                "INPUT IS NOT IN UNLOAD FORMAT",
            );
            messages.push(msg);
            return UtilityResult::new(12, messages);
        };

        // Parse unload records into PDS.
        let mut pds = PdsData::new();
        let mut current_member: Option<String> = None;
        let mut current_content: Vec<String> = Vec::new();
        let mut member_count = 0u32;

        for record in &unload.records {
            if record == UNLOAD_HEADER || record == MEMBER_END {
                // Flush current member if any.
                if let Some(name) = current_member.take() {
                    pds.add_member(&name, std::mem::take(&mut current_content));
                    member_count += 1;
                }
                continue;
            }

            if let Some(name) = record.strip_prefix(MEMBER_HEADER_PREFIX) {
                // Flush previous member.
                if let Some(prev_name) = current_member.take() {
                    pds.add_member(&prev_name, std::mem::take(&mut current_content));
                    member_count += 1;
                }
                current_member = Some(name.trim().to_string());
                current_content.clear();
            } else if current_member.is_some() {
                current_content.push(record.clone());
            }
        }

        // Flush last member.
        if let Some(name) = current_member.take() {
            pds.add_member(&name, current_content);
            member_count += 1;
        }

        // Store in output DD.
        let out_dd = context.get_dd_mut(outdd);
        let Some(out_dd) = out_dd else {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 1005, MessageSeverity::Severe),
                &format!("DD {outdd} NOT ALLOCATED"),
            );
            messages.push(msg);
            return UtilityResult::new(12, messages);
        };

        out_dd.pds_data = Some(pds);

        let msg = UtilityMessage::info(
            &format_message_id("IEB", 1020, MessageSeverity::Info),
            &format!("{member_count} MEMBERS LOADED FROM SEQUENTIAL"),
        );
        messages.push(msg);

        UtilityResult::success_with(messages)
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::DdAllocation;

    /// Create a test PDS with numbered members.
    fn make_test_pds(member_names: &[&str]) -> PdsData {
        let mut pds = PdsData::new();
        for name in member_names {
            pds.add_member(name, vec![format!("* Content of {name}")]);
        }
        pds
    }

    fn make_large_pds(count: usize) -> PdsData {
        let mut pds = PdsData::new();
        for i in 1..=count {
            let name = format!("MEM{i:04}");
            pds.add_member(&name, vec![format!("DATA FOR {name}")]);
        }
        pds
    }

    fn setup_copy_context(
        indd: &str,
        outdd: &str,
        in_pds: PdsData,
        out_pds: Option<PdsData>,
        sysin: Vec<String>,
    ) -> UtilityContext {
        let mut ctx = UtilityContext::new("STEP01", "IEBCOPY");
        ctx.add_dd(DdAllocation::pds(
            indd,
            &format!("IN.{indd}"),
            "SHR",
            in_pds,
        ));
        let out = out_pds.unwrap_or_default();
        ctx.add_dd(DdAllocation::pds(outdd, &format!("OUT.{outdd}"), "OLD", out));
        ctx.add_dd(DdAllocation::inline("SYSIN", sysin));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx
    }

    // ─────── UTIL-100.1: Basic Member Copy (SELECT/EXCLUDE) ───────

    #[test]
    fn test_copy_all_members() {
        let in_pds = make_test_pds(&["A", "B", "C"]);
        let mut ctx = setup_copy_context(
            "IN1",
            "OUT1",
            in_pds,
            None,
            vec![" COPY OUTDD=OUT1,INDD=IN1".to_string()],
        );

        let result = Iebcopy.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("OUT1").unwrap();
        let pds = out.pds_data.as_ref().unwrap();
        assert_eq!(pds.member_count(), 3);
        assert!(pds.has_member("A"));
        assert!(pds.has_member("B"));
        assert!(pds.has_member("C"));
    }

    #[test]
    fn test_copy_select_members() {
        let in_pds = make_test_pds(&["A", "B", "C", "D", "E"]);
        let mut ctx = setup_copy_context(
            "IN1",
            "OUT1",
            in_pds,
            None,
            vec![
                " COPY OUTDD=OUT1,INDD=IN1".to_string(),
                " SELECT MEMBER=(A,B,C)".to_string(),
            ],
        );

        let result = Iebcopy.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("OUT1").unwrap();
        let pds = out.pds_data.as_ref().unwrap();
        assert_eq!(pds.member_count(), 3);
        assert!(pds.has_member("A"));
        assert!(pds.has_member("B"));
        assert!(pds.has_member("C"));
        assert!(!pds.has_member("D"));
        assert!(!pds.has_member("E"));
    }

    #[test]
    fn test_copy_exclude_members() {
        let in_pds = make_test_pds(&["A", "B", "C", "D", "E"]);
        let mut ctx = setup_copy_context(
            "IN1",
            "OUT1",
            in_pds,
            None,
            vec![
                " COPY OUTDD=OUT1,INDD=IN1".to_string(),
                " EXCLUDE MEMBER=(D,E)".to_string(),
            ],
        );

        let result = Iebcopy.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("OUT1").unwrap();
        let pds = out.pds_data.as_ref().unwrap();
        assert_eq!(pds.member_count(), 3);
        assert!(pds.has_member("A"));
        assert!(pds.has_member("B"));
        assert!(pds.has_member("C"));
        assert!(!pds.has_member("D"));
    }

    #[test]
    fn test_copy_no_select_no_exclude_copies_all() {
        let in_pds = make_test_pds(&["MOD1", "MOD2", "MOD3"]);
        let mut ctx = setup_copy_context(
            "INDD",
            "OUTDD",
            in_pds,
            None,
            vec![" COPY OUTDD=OUTDD,INDD=INDD".to_string()],
        );

        let result = Iebcopy.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("OUTDD").unwrap();
        assert_eq!(out.pds_data.as_ref().unwrap().member_count(), 3);
    }

    // ─────── UTIL-100.2: Copy with REPLACE Option ───────

    #[test]
    fn test_copy_replace_overwrites() {
        let in_pds = make_test_pds(&["A"]);
        let mut out_pds = PdsData::new();
        out_pds.add_member("A", vec!["OLD CONTENT".to_string()]);

        let mut ctx = setup_copy_context(
            "IN1",
            "OUT1",
            in_pds,
            Some(out_pds),
            vec![" COPY OUTDD=OUT1,INDD=IN1,REPLACE".to_string()],
        );

        let result = Iebcopy.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("OUT1").unwrap();
        let member = out.pds_data.as_ref().unwrap().get_member("A").unwrap();
        assert_eq!(member.content[0], "* Content of A");
    }

    #[test]
    fn test_copy_no_replace_skips_existing() {
        let in_pds = make_test_pds(&["A", "B"]);
        let mut out_pds = PdsData::new();
        out_pds.add_member("A", vec!["EXISTING".to_string()]);

        let mut ctx = setup_copy_context(
            "IN1",
            "OUT1",
            in_pds,
            Some(out_pds),
            vec![" COPY OUTDD=OUT1,INDD=IN1".to_string()],
        );

        let result = Iebcopy.execute(&mut ctx);
        assert_eq!(result.condition_code, 4, "Should have CC=4 warning");

        let out = ctx.get_dd("OUT1").unwrap();
        let pds = out.pds_data.as_ref().unwrap();
        // A should keep old content.
        assert_eq!(
            pds.get_member("A").unwrap().content[0],
            "EXISTING"
        );
        // B should be copied.
        assert!(pds.has_member("B"));
    }

    #[test]
    fn test_copy_replace_last_input_wins() {
        // When merging, with REPLACE, last input's version wins.
        let mut in1 = PdsData::new();
        in1.add_member("A", vec!["FROM IN1".to_string()]);
        let mut in2 = PdsData::new();
        in2.add_member("A", vec!["FROM IN2".to_string()]);

        let mut ctx = UtilityContext::new("STEP01", "IEBCOPY");
        ctx.add_dd(DdAllocation::pds("IN1", "INPUT1.PDS", "SHR", in1));
        ctx.add_dd(DdAllocation::pds("IN2", "INPUT2.PDS", "SHR", in2));
        ctx.add_dd(DdAllocation::pds(
            "OUT1",
            "OUTPUT.PDS",
            "OLD",
            PdsData::new(),
        ));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![" COPY OUTDD=OUT1,INDD=(IN1,IN2),REPLACE".to_string()],
        ));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebcopy.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("OUT1").unwrap();
        let member = out.pds_data.as_ref().unwrap().get_member("A").unwrap();
        assert_eq!(member.content[0], "FROM IN2");
    }

    // ─────── UTIL-100.3: PDS Compress In-Place ───────

    #[test]
    fn test_compress_in_place() {
        let mut pds = PdsData::new();
        pds.add_member("KEEP1", vec!["DATA1".to_string()]);
        pds.add_member("DEL1", vec!["OLD".to_string()]);
        pds.add_member("DEL2", vec!["OLD".to_string()]);
        pds.add_member("KEEP2", vec!["DATA2".to_string()]);
        pds.delete_member("DEL1");
        pds.delete_member("DEL2");

        assert_eq!(pds.deleted_count(), 2);

        let mut ctx = UtilityContext::new("STEP01", "IEBCOPY");
        ctx.add_dd(DdAllocation::pds("MYPDS", "MY.PDS", "OLD", pds));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![" COPY OUTDD=MYPDS,INDD=MYPDS".to_string()],
        ));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebcopy.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let dd = ctx.get_dd("MYPDS").unwrap();
        let pds = dd.pds_data.as_ref().unwrap();
        assert_eq!(pds.member_count(), 2);
        assert_eq!(pds.deleted_count(), 0);
        assert!(pds.has_member("KEEP1"));
        assert!(pds.has_member("KEEP2"));
    }

    #[test]
    fn test_compress_no_deleted() {
        let pds = make_test_pds(&["A", "B", "C"]);

        let mut ctx = UtilityContext::new("STEP01", "IEBCOPY");
        ctx.add_dd(DdAllocation::pds("MYPDS", "MY.PDS", "OLD", pds));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![" COPY OUTDD=MYPDS,INDD=MYPDS".to_string()],
        ));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebcopy.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let dd = ctx.get_dd("MYPDS").unwrap();
        assert_eq!(dd.pds_data.as_ref().unwrap().member_count(), 3);
    }

    // ─────── UTIL-100.4: Merge Multiple Input PDS ───────

    #[test]
    fn test_merge_multiple_pds() {
        let mut ctx = UtilityContext::new("STEP01", "IEBCOPY");

        let in1 = make_test_pds(&["A", "B"]);
        let in2 = make_test_pds(&["C", "D"]);
        let in3 = make_test_pds(&["E"]);

        ctx.add_dd(DdAllocation::pds("IN1", "PDS1", "SHR", in1));
        ctx.add_dd(DdAllocation::pds("IN2", "PDS2", "SHR", in2));
        ctx.add_dd(DdAllocation::pds("IN3", "PDS3", "SHR", in3));
        ctx.add_dd(DdAllocation::pds(
            "OUT1",
            "OUTPUT.PDS",
            "OLD",
            PdsData::new(),
        ));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![" COPY OUTDD=OUT1,INDD=(IN1,IN2,IN3)".to_string()],
        ));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebcopy.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("OUT1").unwrap();
        let pds = out.pds_data.as_ref().unwrap();
        assert_eq!(pds.member_count(), 5);
        assert!(pds.has_member("A"));
        assert!(pds.has_member("C"));
        assert!(pds.has_member("E"));
    }

    #[test]
    fn test_merge_duplicate_with_replace() {
        let mut ctx = UtilityContext::new("STEP01", "IEBCOPY");

        let mut in1 = PdsData::new();
        in1.add_member("COMMON", vec!["VERSION1".to_string()]);
        in1.add_member("ONLY1", vec!["DATA".to_string()]);

        let mut in2 = PdsData::new();
        in2.add_member("COMMON", vec!["VERSION2".to_string()]);
        in2.add_member("ONLY2", vec!["DATA".to_string()]);

        ctx.add_dd(DdAllocation::pds("IN1", "PDS1", "SHR", in1));
        ctx.add_dd(DdAllocation::pds("IN2", "PDS2", "SHR", in2));
        ctx.add_dd(DdAllocation::pds(
            "OUT1",
            "OUTPUT.PDS",
            "OLD",
            PdsData::new(),
        ));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![" COPY OUTDD=OUT1,INDD=(IN1,IN2),REPLACE".to_string()],
        ));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebcopy.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("OUT1").unwrap();
        let pds = out.pds_data.as_ref().unwrap();
        assert_eq!(pds.member_count(), 3);
        // Last input (IN2) version wins.
        assert_eq!(
            pds.get_member("COMMON").unwrap().content[0],
            "VERSION2"
        );
    }

    // ─────── UTIL-100.5: Load/Unload (Flat File Transport) ───────

    #[test]
    fn test_unload_pds_to_sequential() {
        let in_pds = make_test_pds(&["MOD1", "MOD2"]);

        let mut ctx = UtilityContext::new("STEP01", "IEBCOPY");
        ctx.add_dd(DdAllocation::pds("IN1", "MY.PDS", "SHR", in_pds));
        ctx.add_dd(DdAllocation::sequential("OUT1", "MY.UNLOAD", "NEW"));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![" COPY OUTDD=OUT1,INDD=IN1".to_string()],
        ));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebcopy.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("OUT1").unwrap();
        let unload = out.unload_data.as_ref().unwrap();
        assert!(!unload.records.is_empty());
        assert_eq!(unload.records[0], UNLOAD_HEADER);
        assert!(unload.records.iter().any(|r| r.contains("MOD1")));
        assert!(unload.records.iter().any(|r| r.contains("MOD2")));
    }

    #[test]
    fn test_load_from_sequential() {
        // First, create an unload.
        let in_pds = make_test_pds(&["A", "B", "C"]);

        let mut unload_ctx = UtilityContext::new("UNLOAD", "IEBCOPY");
        unload_ctx.add_dd(DdAllocation::pds("IN1", "MY.PDS", "SHR", in_pds));
        unload_ctx.add_dd(DdAllocation::sequential("UNLD", "MY.UNLOAD", "NEW"));
        unload_ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![" COPY OUTDD=UNLD,INDD=IN1".to_string()],
        ));
        unload_ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let r = Iebcopy.execute(&mut unload_ctx);
        assert_eq!(r.condition_code, 0);

        // Get the unload data.
        let unload = unload_ctx
            .get_dd("UNLD")
            .unwrap()
            .unload_data
            .clone()
            .unwrap();

        // Now load it back.
        let mut load_dd = DdAllocation::sequential("SEQIN", "MY.UNLOAD", "SHR");
        load_dd.unload_data = Some(unload);

        let mut load_ctx = UtilityContext::new("LOAD", "IEBCOPY");
        load_ctx.add_dd(load_dd);
        load_ctx.add_dd(DdAllocation::pds(
            "PDSOUT",
            "MY.NEWPDS",
            "NEW",
            PdsData::new(),
        ));
        load_ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![" COPY OUTDD=PDSOUT,INDD=SEQIN".to_string()],
        ));
        load_ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let r2 = Iebcopy.execute(&mut load_ctx);
        assert_eq!(r2.condition_code, 0);

        let out = load_ctx.get_dd("PDSOUT").unwrap();
        let pds = out.pds_data.as_ref().unwrap();
        assert_eq!(pds.member_count(), 3);
        assert!(pds.has_member("A"));
        assert!(pds.has_member("B"));
        assert!(pds.has_member("C"));

        // Verify content was preserved.
        assert_eq!(pds.get_member("A").unwrap().content[0], "* Content of A");
    }

    // ─────── UTIL-100.6: IEBCOPY Integration Tests ───────

    #[test]
    fn test_copy_50_members_select_5() {
        let in_pds = make_large_pds(50);
        assert_eq!(in_pds.member_count(), 50);

        let mut ctx = setup_copy_context(
            "IN1",
            "OUT1",
            in_pds,
            None,
            vec![
                " COPY OUTDD=OUT1,INDD=IN1".to_string(),
                " SELECT MEMBER=(MEM0001,MEM0010,MEM0020,MEM0030,MEM0050)".to_string(),
            ],
        );

        let result = Iebcopy.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("OUT1").unwrap();
        let pds = out.pds_data.as_ref().unwrap();
        assert_eq!(pds.member_count(), 5);
        assert!(pds.has_member("MEM0001"));
        assert!(pds.has_member("MEM0050"));
        assert!(!pds.has_member("MEM0002"));
    }

    #[test]
    fn test_compress_with_10_deleted() {
        let mut pds = make_large_pds(20);
        // Delete 10 members.
        for i in 1..=10 {
            pds.delete_member(&format!("MEM{i:04}"));
        }
        assert_eq!(pds.deleted_count(), 10);
        assert_eq!(pds.member_count(), 10);

        let mut ctx = UtilityContext::new("STEP01", "IEBCOPY");
        ctx.add_dd(DdAllocation::pds("PDS1", "MY.PDS", "OLD", pds));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![" COPY OUTDD=PDS1,INDD=PDS1".to_string()],
        ));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebcopy.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let dd = ctx.get_dd("PDS1").unwrap();
        let pds = dd.pds_data.as_ref().unwrap();
        assert_eq!(pds.member_count(), 10);
        assert_eq!(pds.deleted_count(), 0);
        assert!(pds.has_member("MEM0011"));
        assert!(!pds.has_member("MEM0001"));
    }

    #[test]
    fn test_no_sysin_returns_cc12() {
        let mut ctx = UtilityContext::new("STEP01", "IEBCOPY");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebcopy.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    #[test]
    fn test_invalid_sysin_returns_cc12() {
        let mut ctx = UtilityContext::new("STEP01", "IEBCOPY");
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec!["GARBAGE STATEMENT".to_string()],
        ));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebcopy.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    #[test]
    fn test_missing_input_dd_returns_cc12() {
        let mut ctx = UtilityContext::new("STEP01", "IEBCOPY");
        ctx.add_dd(DdAllocation::pds(
            "OUT1",
            "OUTPUT.PDS",
            "OLD",
            PdsData::new(),
        ));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![" COPY OUTDD=OUT1,INDD=NOSUCH".to_string()],
        ));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebcopy.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    // ─── Control statement parsing tests ───

    #[test]
    fn test_parse_copy_basic() {
        let stmts = vec![" COPY OUTDD=OUT,INDD=IN1".to_string()];
        let ops = parse_control_statements(&stmts);
        assert_eq!(ops.len(), 1);
        if let IebcopyOp::Copy { outdd, indd, replace } = &ops[0] {
            assert_eq!(outdd, "OUT");
            assert_eq!(indd, &["IN1"]);
            assert!(!replace);
        } else {
            panic!("Expected Copy op");
        }
    }

    #[test]
    fn test_parse_copy_multiple_indd() {
        let stmts = vec![" COPY OUTDD=OUT,INDD=(IN1,IN2,IN3)".to_string()];
        let ops = parse_control_statements(&stmts);
        if let IebcopyOp::Copy { indd, .. } = &ops[0] {
            assert_eq!(indd, &["IN1", "IN2", "IN3"]);
        } else {
            panic!("Expected Copy op");
        }
    }

    #[test]
    fn test_parse_copy_with_replace() {
        let stmts = vec![" COPY OUTDD=OUT,INDD=IN1,REPLACE".to_string()];
        let ops = parse_control_statements(&stmts);
        if let IebcopyOp::Copy { replace, .. } = &ops[0] {
            assert!(replace);
        } else {
            panic!("Expected Copy op");
        }
    }

    #[test]
    fn test_parse_select() {
        let stmts = vec![" SELECT MEMBER=(A,B,C)".to_string()];
        let ops = parse_control_statements(&stmts);
        assert_eq!(ops.len(), 1);
        if let IebcopyOp::Select { members } = &ops[0] {
            assert_eq!(members, &["A", "B", "C"]);
        } else {
            panic!("Expected Select op");
        }
    }

    #[test]
    fn test_parse_exclude() {
        let stmts = vec![" EXCLUDE MEMBER=(X,Y)".to_string()];
        let ops = parse_control_statements(&stmts);
        if let IebcopyOp::Exclude { members } = &ops[0] {
            assert_eq!(members, &["X", "Y"]);
        } else {
            panic!("Expected Exclude op");
        }
    }

    #[test]
    fn test_parse_comments_and_blanks_ignored() {
        let stmts = vec![
            "/* COMMENT".to_string(),
            "".to_string(),
            " COPY OUTDD=OUT,INDD=IN".to_string(),
        ];
        let ops = parse_control_statements(&stmts);
        assert_eq!(ops.len(), 1);
    }

    #[test]
    fn test_full_iebcopy_via_registry() {
        use crate::UtilityRegistry;

        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(Iebcopy));

        let in_pds = make_test_pds(&["MOD1", "MOD2"]);
        let mut ctx = setup_copy_context(
            "SYSUT1",
            "SYSUT2",
            in_pds,
            None,
            vec![" COPY OUTDD=SYSUT2,INDD=SYSUT1".to_string()],
        );

        let result = reg.dispatch("IEBCOPY", &mut ctx).unwrap();
        assert_eq!(result.condition_code, 0);
    }

    #[test]
    fn test_member_content_preserved() {
        let mut in_pds = PdsData::new();
        in_pds.add_member(
            "PAYROLL",
            vec![
                "       IDENTIFICATION DIVISION.".to_string(),
                "       PROGRAM-ID. PAYROLL.".to_string(),
                "       DATA DIVISION.".to_string(),
            ],
        );

        let mut ctx = setup_copy_context(
            "IN1",
            "OUT1",
            in_pds,
            None,
            vec![" COPY OUTDD=OUT1,INDD=IN1".to_string()],
        );

        Iebcopy.execute(&mut ctx);

        let out = ctx.get_dd("OUT1").unwrap();
        let member = out.pds_data.as_ref().unwrap().get_member("PAYROLL").unwrap();
        assert_eq!(member.content.len(), 3);
        assert!(member.content[0].contains("IDENTIFICATION"));
    }
}
