//! # AMASPZAP — Superzap Utility
//!
//! Patches (zaps) load modules or datasets at specific byte offsets.
//!
//! ## Control Statements
//!
//! - `NAME member ddname` — Select a PDS member to patch
//! - `CCHHR cc hh r` — Select a disk address (cylinder/head/record)
//! - `VERIFY offset data` — Verify bytes at offset match expected hex data
//! - `REP offset data` — Replace bytes at offset with new hex data
//! - `DUMP ALL` — Hex dump entire content
//! - `DUMP offset length` — Hex dump a region
//!
//! ## Safety
//!
//! A VERIFY must succeed before a REP at the same or overlapping offset.
//! If VERIFY fails, the subsequent REP is skipped and CC=8 is set.

use crate::{
    format_message_id, MessageSeverity, UtilityContext, UtilityMessage, UtilityProgram,
    UtilityResult,
};

// ─────────────────────── Addressing Mode ───────────────────────

/// How AMASPZAP locates the data to patch.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AddressMode {
    /// Select a PDS member by name from a DD.
    Name {
        member: String,
        ddname: String,
    },
    /// Select by disk address (cylinder, head, record).
    Cchhr {
        cylinder: u16,
        head: u16,
        record: u8,
    },
}

// ─────────────────────── Control Statements ───────────────────────

/// A parsed AMASPZAP control statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ZapStatement {
    /// NAME member ddname — select target.
    Name { member: String, ddname: String },
    /// CCHHR cc hh r — select by disk address.
    Cchhr { cylinder: u16, head: u16, record: u8 },
    /// VERIFY offset hex_data — verify bytes match.
    Verify { offset: usize, data: Vec<u8> },
    /// REP offset hex_data — replace bytes.
    Rep { offset: usize, data: Vec<u8> },
    /// DUMP ALL — dump entire content.
    DumpAll,
    /// DUMP offset length — dump a region.
    DumpRegion { offset: usize, length: usize },
}

/// Parse a hex string into bytes.
///
/// Accepts hex digits with optional spaces (e.g., "4040 C1C2" or "4040C1C2").
fn parse_hex(s: &str) -> Option<Vec<u8>> {
    let clean: String = s.chars().filter(|c| !c.is_whitespace()).collect();
    if clean.len() % 2 != 0 {
        return None;
    }
    let mut bytes = Vec::with_capacity(clean.len() / 2);
    for i in (0..clean.len()).step_by(2) {
        let byte = u8::from_str_radix(&clean[i..i + 2], 16).ok()?;
        bytes.push(byte);
    }
    Some(bytes)
}

/// Format a byte slice as a hex string (e.g., `[0x41, 0x42]` → `"4142"`).
fn bytes_to_hex(bytes: &[u8]) -> String {
    use std::fmt::Write;
    bytes.iter().fold(String::with_capacity(bytes.len() * 2), |mut s, b| {
        let _ = write!(s, "{b:02X}");
        s
    })
}

/// Parse an offset value (decimal or hex with 0x prefix).
fn parse_offset(s: &str) -> Option<usize> {
    if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
        usize::from_str_radix(hex, 16).ok()
    } else {
        s.parse::<usize>().ok()
    }
}

/// Parse AMASPZAP control statements from SYSIN.
pub fn parse_zap_sysin(lines: &[String]) -> Vec<ZapStatement> {
    let mut stmts = Vec::new();

    for line in lines {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('*') {
            continue;
        }

        let upper = trimmed.to_uppercase();
        let parts: Vec<&str> = trimmed.split_whitespace().collect();

        if upper.starts_with("NAME") && parts.len() >= 3 {
            stmts.push(ZapStatement::Name {
                member: parts[1].to_uppercase(),
                ddname: parts[2].to_uppercase(),
            });
        } else if upper.starts_with("CCHHR") && parts.len() >= 4 {
            let cylinder = parts[1].parse::<u16>().unwrap_or(0);
            let head = parts[2].parse::<u16>().unwrap_or(0);
            let record = parts[3].parse::<u8>().unwrap_or(1);
            stmts.push(ZapStatement::Cchhr {
                cylinder,
                head,
                record,
            });
        } else if upper.starts_with("VERIFY") && parts.len() >= 3 {
            if let Some(offset) = parse_offset(parts[1]) {
                let hex_str = parts[2..].join("");
                if let Some(data) = parse_hex(&hex_str) {
                    stmts.push(ZapStatement::Verify { offset, data });
                }
            }
        } else if upper.starts_with("REP") && parts.len() >= 3 {
            if let Some(offset) = parse_offset(parts[1]) {
                let hex_str = parts[2..].join("");
                if let Some(data) = parse_hex(&hex_str) {
                    stmts.push(ZapStatement::Rep { offset, data });
                }
            }
        } else if upper.starts_with("DUMP") {
            if parts.len() >= 2 && parts[1].eq_ignore_ascii_case("ALL") {
                stmts.push(ZapStatement::DumpAll);
            } else if parts.len() >= 3 {
                if let (Some(offset), Some(length)) =
                    (parse_offset(parts[1]), parse_offset(parts[2]))
                {
                    stmts.push(ZapStatement::DumpRegion { offset, length });
                }
            } else {
                // DUMP with no args → dump all
                stmts.push(ZapStatement::DumpAll);
            }
        }
    }

    stmts
}

// ─────────────────────── Hex Dump Formatting ───────────────────────

/// Format a hex dump of a byte slice, similar to IBM AMASPZAP output.
///
/// Format: `OFFSET  HEX-BYTES                           *EBCDIC*`
fn format_hex_dump(data: &[u8], base_offset: usize) -> Vec<String> {
    let mut lines = Vec::new();
    for chunk_start in (0..data.len()).step_by(16) {
        let chunk_end = (chunk_start + 16).min(data.len());
        let chunk = &data[chunk_start..chunk_end];

        let mut hex_part = String::with_capacity(48);
        for (i, byte) in chunk.iter().enumerate() {
            if i > 0 && i % 4 == 0 {
                hex_part.push(' ');
            }
            hex_part.push_str(&format!("{byte:02X}"));
        }

        let char_part: String = chunk
            .iter()
            .map(|&b| {
                if (0x20..=0x7E).contains(&b) {
                    b as char
                } else {
                    '.'
                }
            })
            .collect();

        lines.push(format!(
            " {:06X}  {:<39} *{}*",
            base_offset + chunk_start,
            hex_part,
            char_part
        ));
    }
    lines
}

// ─────────────────────── AMASPZAP Execution ───────────────────────

/// Execute AMASPZAP statements against a byte buffer.
///
/// Returns (condition_code, messages, modified_buffer).
fn execute_zap(
    stmts: &[ZapStatement],
    buffer: &mut [u8],
    context: &mut UtilityContext,
) -> (u32, Vec<UtilityMessage>) {
    let mut cc: u32 = 0;
    let mut messages = Vec::new();
    let mut verify_passed = false;

    for stmt in stmts {
        match stmt {
            ZapStatement::Name { member, ddname } => {
                let msg = UtilityMessage::info(
                    &format_message_id("AMA", 1, MessageSeverity::Info),
                    &format!("NAME {member} {ddname}"),
                );
                context.write_utility_message(&msg);
                messages.push(msg);
            }
            ZapStatement::Cchhr {
                cylinder,
                head,
                record,
            } => {
                let msg = UtilityMessage::info(
                    &format_message_id("AMA", 2, MessageSeverity::Info),
                    &format!("CCHHR {cylinder:04X} {head:04X} {record:02X}"),
                );
                context.write_utility_message(&msg);
                messages.push(msg);
            }
            ZapStatement::Verify { offset, data } => {
                if *offset + data.len() > buffer.len() {
                    let msg = UtilityMessage::error(
                        &format_message_id("AMA", 10, MessageSeverity::Error),
                        &format!(
                            "VERIFY OFFSET {offset:06X} LENGTH {} EXCEEDS DATA SIZE {}",
                            data.len(),
                            buffer.len()
                        ),
                    );
                    context.write_utility_message(&msg);
                    messages.push(msg);
                    cc = cc.max(8);
                    verify_passed = false;
                    continue;
                }

                let actual = &buffer[*offset..*offset + data.len()];
                if actual == data.as_slice() {
                    let msg = UtilityMessage::info(
                        &format_message_id("AMA", 11, MessageSeverity::Info),
                        &format!("VERIFY AT OFFSET {:06X} SUCCESSFUL", offset),
                    );
                    context.write_utility_message(&msg);
                    messages.push(msg);
                    verify_passed = true;
                } else {
                    let expected_hex = bytes_to_hex(data);
                    let actual_hex = bytes_to_hex(actual);
                    let msg = UtilityMessage::error(
                        &format_message_id("AMA", 12, MessageSeverity::Error),
                        &format!(
                            "VERIFY FAILED AT OFFSET {:06X} EXPECTED={expected_hex} ACTUAL={actual_hex}",
                            offset
                        ),
                    );
                    context.write_utility_message(&msg);
                    messages.push(msg);
                    cc = cc.max(8);
                    verify_passed = false;
                }
            }
            ZapStatement::Rep { offset, data } => {
                if !verify_passed {
                    let msg = UtilityMessage::error(
                        &format_message_id("AMA", 20, MessageSeverity::Error),
                        &format!(
                            "REP AT OFFSET {:06X} SKIPPED — VERIFY NOT SATISFIED",
                            offset
                        ),
                    );
                    context.write_utility_message(&msg);
                    messages.push(msg);
                    cc = cc.max(8);
                    continue;
                }

                if *offset + data.len() > buffer.len() {
                    let msg = UtilityMessage::error(
                        &format_message_id("AMA", 21, MessageSeverity::Error),
                        &format!(
                            "REP OFFSET {offset:06X} LENGTH {} EXCEEDS DATA SIZE {}",
                            data.len(),
                            buffer.len()
                        ),
                    );
                    context.write_utility_message(&msg);
                    messages.push(msg);
                    cc = cc.max(8);
                    continue;
                }

                buffer[*offset..*offset + data.len()].copy_from_slice(data);
                let hex = bytes_to_hex(data);
                let msg = UtilityMessage::info(
                    &format_message_id("AMA", 22, MessageSeverity::Info),
                    &format!(
                        "REP AT OFFSET {:06X} LENGTH {} DATA={hex}",
                        offset,
                        data.len()
                    ),
                );
                context.write_utility_message(&msg);
                messages.push(msg);
                // Reset verify — next REP needs its own VERIFY
                verify_passed = false;
            }
            ZapStatement::DumpAll => {
                let dump_lines = format_hex_dump(buffer, 0);
                for line in &dump_lines {
                    context.write_message(line);
                }
                let msg = UtilityMessage::info(
                    &format_message_id("AMA", 30, MessageSeverity::Info),
                    &format!("DUMP ALL — {} BYTES", buffer.len()),
                );
                context.write_utility_message(&msg);
                messages.push(msg);
            }
            ZapStatement::DumpRegion { offset, length } => {
                if *offset >= buffer.len() {
                    let msg = UtilityMessage::warning(
                        &format_message_id("AMA", 31, MessageSeverity::Warning),
                        &format!("DUMP OFFSET {:06X} BEYOND END OF DATA", offset),
                    );
                    context.write_utility_message(&msg);
                    messages.push(msg);
                    cc = cc.max(4);
                    continue;
                }
                let end = (*offset + *length).min(buffer.len());
                let dump_lines = format_hex_dump(&buffer[*offset..end], *offset);
                for line in &dump_lines {
                    context.write_message(line);
                }
                let msg = UtilityMessage::info(
                    &format_message_id("AMA", 32, MessageSeverity::Info),
                    &format!(
                        "DUMP OFFSET {:06X} LENGTH {} BYTES",
                        offset,
                        end - *offset
                    ),
                );
                context.write_utility_message(&msg);
                messages.push(msg);
            }
        }
    }

    (cc, messages)
}

// ─────────────────────── UtilityProgram impl ───────────────────────

/// AMASPZAP (Superzap) — patch utility.
pub struct Amaspzap;

impl UtilityProgram for Amaspzap {
    fn name(&self) -> &str {
        "AMASPZAP"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let sysin = context.read_sysin();
        if sysin.is_empty() {
            let msg = UtilityMessage::error(
                &format_message_id("AMA", 99, MessageSeverity::Error),
                "SYSIN IS EMPTY — NO CONTROL STATEMENTS",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(12, vec![msg]);
        }

        let stmts = parse_zap_sysin(&sysin);
        if stmts.is_empty() {
            let msg = UtilityMessage::error(
                &format_message_id("AMA", 98, MessageSeverity::Error),
                "NO VALID CONTROL STATEMENTS FOUND",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(12, vec![msg]);
        }

        // Resolve addressing mode from first statement(s).
        // Look for NAME to identify a PDS member as the target.
        let mut buffer = resolve_target_buffer(&stmts, context);

        // Filter out addressing statements for execution.
        let exec_stmts: Vec<&ZapStatement> = stmts
            .iter()
            .filter(|s| !matches!(s, ZapStatement::Name { .. } | ZapStatement::Cchhr { .. }))
            .collect();

        // If we only have addressing statements, just acknowledge them.
        if exec_stmts.is_empty() {
            let msg = UtilityMessage::info(
                &format_message_id("AMA", 3, MessageSeverity::Info),
                "TARGET ADDRESSED — NO OPERATIONS SPECIFIED",
            );
            context.write_utility_message(&msg);
            return UtilityResult::success_with(vec![msg]);
        }

        let (cc, messages) = execute_zap(&stmts, &mut buffer, context);

        // Write modified buffer back to PDS member if NAME was used.
        write_back_buffer(&stmts, &buffer, context);

        UtilityResult::new(cc, messages)
    }
}

/// Resolve the target byte buffer based on addressing statements.
fn resolve_target_buffer(stmts: &[ZapStatement], context: &mut UtilityContext) -> Vec<u8> {
    for stmt in stmts {
        if let ZapStatement::Name { member, ddname } = stmt {
            // Try to read member content from PDS on the named DD.
            if let Some(dd) = context.get_dd(ddname) {
                if let Some(pds) = &dd.pds_data {
                    if let Some(m) = pds.get_member(member) {
                        // Concatenate member lines into a byte buffer.
                        let mut buf = Vec::new();
                        for line in &m.content {
                            buf.extend_from_slice(line.as_bytes());
                        }
                        return buf;
                    }
                }
            }
            // Fall through to SYSUT1.
        }
    }

    // Default: read from SYSUT1 inline data as a byte buffer.
    if let Some(dd) = context.get_dd("SYSUT1") {
        if let Some(lines) = &dd.inline_data {
            let mut buf = Vec::new();
            for line in lines {
                buf.extend_from_slice(line.as_bytes());
            }
            return buf;
        }
    }

    Vec::new()
}

/// Write modified buffer back to the PDS member if NAME addressing was used.
fn write_back_buffer(stmts: &[ZapStatement], buffer: &[u8], context: &mut UtilityContext) {
    for stmt in stmts {
        if let ZapStatement::Name { member, ddname } = stmt {
            if let Some(dd) = context.get_dd_mut(ddname) {
                if let Some(pds) = dd.pds_data.as_mut() {
                    // Convert buffer back to string lines.
                    let content = String::from_utf8_lossy(buffer).to_string();
                    let lines: Vec<String> = if content.is_empty() {
                        Vec::new()
                    } else {
                        vec![content]
                    };
                    pds.add_member(member, lines);
                    return;
                }
            }
        }
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{DdAllocation, PdsData, UtilityContext};

    // ─── UTIL-108.1: VERIFY Control Statement ───

    #[test]
    fn test_verify_success() {
        let mut ctx = UtilityContext::new("STEP01", "AMASPZAP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSUT1",
            vec!["ABCDEF".to_string()],
        ));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec!["VERIFY 0 414243".to_string()], // "ABC" in hex
        ));

        let result = Amaspzap.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(
            result.messages.iter().any(|m| m.text.contains("SUCCESSFUL")),
            "Expected VERIFY SUCCESSFUL message"
        );
    }

    #[test]
    fn test_verify_failure() {
        let mut ctx = UtilityContext::new("STEP01", "AMASPZAP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSUT1",
            vec!["ABCDEF".to_string()],
        ));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec!["VERIFY 0 FF00FF".to_string()],
        ));

        let result = Amaspzap.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);
        assert!(
            result.messages.iter().any(|m| m.text.contains("FAILED")),
            "Expected VERIFY FAILED message"
        );
    }

    #[test]
    fn test_verify_out_of_bounds() {
        let mut ctx = UtilityContext::new("STEP01", "AMASPZAP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline("SYSUT1", vec!["AB".to_string()]));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec!["VERIFY 100 4142".to_string()],
        ));

        let result = Amaspzap.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);
        assert!(result
            .messages
            .iter()
            .any(|m| m.text.contains("EXCEEDS")));
    }

    // ─── UTIL-108.2: REP Control Statement ───

    #[test]
    fn test_rep_after_verify() {
        let mut ctx = UtilityContext::new("STEP01", "AMASPZAP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSUT1",
            vec!["ABCDEF".to_string()],
        ));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![
                "VERIFY 0 414243".to_string(), // Verify "ABC"
                "REP 0 585958".to_string(),     // Replace with "XYX"
            ],
        ));

        let result = Amaspzap.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages.iter().any(|m| m.text.contains("REP AT")));
    }

    #[test]
    fn test_rep_without_verify_is_rejected() {
        let mut ctx = UtilityContext::new("STEP01", "AMASPZAP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSUT1",
            vec!["ABCDEF".to_string()],
        ));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec!["REP 0 585958".to_string()], // REP without VERIFY
        ));

        let result = Amaspzap.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);
        assert!(result
            .messages
            .iter()
            .any(|m| m.text.contains("VERIFY NOT SATISFIED")));
    }

    // ─── UTIL-108.3: DUMP Control Statement ───

    #[test]
    fn test_dump_all() {
        let mut ctx = UtilityContext::new("STEP01", "AMASPZAP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSUT1",
            vec!["Hello World!".to_string()],
        ));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec!["DUMP ALL".to_string()],
        ));

        let result = Amaspzap.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let output = ctx.sysprint_output();
        // Should have hex dump line(s) + the DUMP message
        assert!(output.iter().any(|l| l.contains("000000")));
        assert!(result.messages.iter().any(|m| m.text.contains("DUMP ALL")));
    }

    #[test]
    fn test_dump_region() {
        let mut ctx = UtilityContext::new("STEP01", "AMASPZAP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSUT1",
            vec!["ABCDEFGHIJKLMNOP".to_string()],
        ));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec!["DUMP 4 8".to_string()],
        ));

        let result = Amaspzap.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result
            .messages
            .iter()
            .any(|m| m.text.contains("DUMP OFFSET")));
    }

    #[test]
    fn test_dump_beyond_end() {
        let mut ctx = UtilityContext::new("STEP01", "AMASPZAP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline("SYSUT1", vec!["AB".to_string()]));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec!["DUMP 999 10".to_string()],
        ));

        let result = Amaspzap.execute(&mut ctx);
        assert_eq!(result.condition_code, 4);
        assert!(result
            .messages
            .iter()
            .any(|m| m.text.contains("BEYOND END")));
    }

    // ─── UTIL-108.4: NAME/CCHHR Addressing Modes ───

    #[test]
    fn test_name_addressing() {
        let mut ctx = UtilityContext::new("STEP01", "AMASPZAP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let mut pds = PdsData::new();
        pds.add_member("MYMEMBER", vec!["TESTDATA".to_string()]);
        ctx.add_dd(DdAllocation::pds("SYSLIB", "MY.LOADLIB", "OLD", pds));

        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![
                "NAME MYMEMBER SYSLIB".to_string(),
                "VERIFY 0 54455354".to_string(), // "TEST"
                "REP 0 5A415050".to_string(),     // "ZAPP"
            ],
        ));

        let result = Amaspzap.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        // Verify the member was modified
        let dd = ctx.get_dd("SYSLIB").unwrap();
        let pds = dd.pds_data.as_ref().unwrap();
        let member = pds.get_member("MYMEMBER").unwrap();
        assert!(member.content[0].starts_with("ZAPP"));
    }

    #[test]
    fn test_cchhr_addressing() {
        let mut ctx = UtilityContext::new("STEP01", "AMASPZAP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSUT1",
            vec!["DATA".to_string()],
        ));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![
                "CCHHR 0 0 1".to_string(),
                "DUMP ALL".to_string(),
            ],
        ));

        let result = Amaspzap.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result
            .messages
            .iter()
            .any(|m| m.text.contains("CCHHR")));
    }

    // ─── UTIL-108.5: Safety Checks ───

    #[test]
    fn test_verify_before_rep_safety() {
        let mut ctx = UtilityContext::new("STEP01", "AMASPZAP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSUT1",
            vec!["ABCDEFGH".to_string()],
        ));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![
                // First pair: VERIFY + REP → OK
                "VERIFY 0 41424344".to_string(), // "ABCD"
                "REP 0 58585858".to_string(),     // "XXXX"
                // Second REP without new VERIFY → rejected
                "REP 4 59595959".to_string(), // Should fail
            ],
        ));

        let result = Amaspzap.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);
        // First REP should succeed, second should be rejected
        assert!(result.messages.iter().any(|m| m.text.contains("REP AT OFFSET 000000")));
        assert!(result
            .messages
            .iter()
            .any(|m| m.text.contains("VERIFY NOT SATISFIED")));
    }

    #[test]
    fn test_failed_verify_blocks_rep() {
        let mut ctx = UtilityContext::new("STEP01", "AMASPZAP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSUT1",
            vec!["ABCDEFGH".to_string()],
        ));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![
                "VERIFY 0 FFFFFFFF".to_string(), // Will fail
                "REP 0 58585858".to_string(),      // Should be skipped
            ],
        ));

        let result = Amaspzap.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);
        assert!(result.messages.iter().any(|m| m.text.contains("FAILED")));
        assert!(result
            .messages
            .iter()
            .any(|m| m.text.contains("VERIFY NOT SATISFIED")));
    }

    #[test]
    fn test_multiple_verify_rep_pairs() {
        let mut ctx = UtilityContext::new("STEP01", "AMASPZAP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSUT1",
            vec!["AABBCCDD".to_string()],
        ));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![
                "VERIFY 0 4141".to_string(), // "AA"
                "REP 0 5858".to_string(),     // "XX"
                "VERIFY 2 4242".to_string(), // "BB"
                "REP 2 5959".to_string(),     // "YY"
            ],
        ));

        let result = Amaspzap.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        // Both REPs should succeed
        let rep_msgs: Vec<_> = result
            .messages
            .iter()
            .filter(|m| m.text.starts_with("REP AT"))
            .collect();
        assert_eq!(rep_msgs.len(), 2);
    }

    // ─── UTIL-108.6: Integration Tests ───

    #[test]
    fn test_empty_sysin() {
        let mut ctx = UtilityContext::new("STEP01", "AMASPZAP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline("SYSIN", vec![]));

        let result = Amaspzap.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    #[test]
    fn test_comment_lines_skipped() {
        let mut ctx = UtilityContext::new("STEP01", "AMASPZAP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSUT1",
            vec!["ABCD".to_string()],
        ));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![
                "* THIS IS A COMMENT".to_string(),
                "DUMP ALL".to_string(),
            ],
        ));

        let result = Amaspzap.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
    }

    #[test]
    fn test_hex_offset_parsing() {
        let mut ctx = UtilityContext::new("STEP01", "AMASPZAP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        // Create a 32-byte buffer
        ctx.add_dd(DdAllocation::inline(
            "SYSUT1",
            vec!["ABCDEFGHIJKLMNOPQRSTUVWXYZ012345".to_string()],
        ));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec!["VERIFY 0x10 51525354".to_string()], // offset 16, "QRST"
        ));

        let result = Amaspzap.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result
            .messages
            .iter()
            .any(|m| m.text.contains("SUCCESSFUL")));
    }

    #[test]
    fn test_registry_dispatch() {
        let reg = crate::UtilityRegistry::with_builtins();
        assert!(reg.is_registered("AMASPZAP"));

        let mut ctx = UtilityContext::new("STEP01", "AMASPZAP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSUT1",
            vec!["TEST".to_string()],
        ));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec!["DUMP ALL".to_string()],
        ));

        let result = reg.dispatch("AMASPZAP", &mut ctx).unwrap();
        assert_eq!(result.condition_code, 0);
    }

    // ─── Parsing tests ───

    #[test]
    fn test_parse_hex() {
        assert_eq!(parse_hex("4142"), Some(vec![0x41, 0x42]));
        assert_eq!(parse_hex("41 42"), Some(vec![0x41, 0x42]));
        assert_eq!(parse_hex("FF00FF"), Some(vec![0xFF, 0x00, 0xFF]));
        assert_eq!(parse_hex("G1"), None); // invalid hex
        assert_eq!(parse_hex("4"), None); // odd length
    }

    #[test]
    fn test_parse_offset() {
        assert_eq!(parse_offset("0"), Some(0));
        assert_eq!(parse_offset("100"), Some(100));
        assert_eq!(parse_offset("0x10"), Some(16));
        assert_eq!(parse_offset("0XFF"), Some(255));
    }

    #[test]
    fn test_parse_zap_sysin() {
        let lines = vec![
            "* Comment".to_string(),
            "NAME MYMOD SYSLIB".to_string(),
            "VERIFY 0 41424344".to_string(),
            "REP 0 58585858".to_string(),
            "DUMP ALL".to_string(),
        ];
        let stmts = parse_zap_sysin(&lines);
        assert_eq!(stmts.len(), 4);
        assert!(matches!(
            &stmts[0],
            ZapStatement::Name { member, ddname }
            if member == "MYMOD" && ddname == "SYSLIB"
        ));
        assert!(matches!(
            &stmts[1],
            ZapStatement::Verify { offset: 0, .. }
        ));
        assert!(matches!(&stmts[2], ZapStatement::Rep { offset: 0, .. }));
        assert!(matches!(&stmts[3], ZapStatement::DumpAll));
    }

    #[test]
    fn test_hex_dump_format() {
        let data = b"Hello, World!";
        let lines = format_hex_dump(data, 0);
        assert_eq!(lines.len(), 1);
        assert!(lines[0].contains("000000"));
        assert!(lines[0].contains("48656C6C"));
        assert!(lines[0].contains("*Hello, World!*"));
    }
}
