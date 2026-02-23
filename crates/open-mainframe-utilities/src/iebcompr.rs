//! IEBCOMPR — Dataset Comparison utility.
//!
//! Compares two datasets (sequential or PDS) record by record and reports
//! any differences found. IEBCOMPR is the standard z/OS utility for
//! verifying that two copies of a dataset are identical.
//!
//! ## Control Statement Syntax
//!
//! ```text
//! COMPARE TYPORG=PS     (sequential comparison)
//! COMPARE TYPORG=PO     (PDS member-by-member comparison)
//! ```
//!
//! ## Condition Codes
//!
//! | CC | Meaning |
//! |----|---------|
//! | 0  | Datasets are identical |
//! | 8  | Differences found |
//! | 12 | Severe — missing DD or invalid syntax |
//! | 16 | Unrecoverable error |

use crate::{
    format_message_id, MessageSeverity, UtilityContext, UtilityMessage, UtilityProgram,
    UtilityResult,
};

// ─────────────────────── Control Statement Parsing ───────────────────────

/// Dataset organization for comparison.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompareTyporg {
    /// Sequential (PS) comparison.
    Sequential,
    /// Partitioned (PO) member-by-member comparison.
    Partitioned,
}

/// Parsed IEBCOMPR control statement.
#[derive(Debug, Clone)]
pub struct CompareConfig {
    /// Dataset organization to compare.
    pub typorg: CompareTyporg,
}

/// Parse IEBCOMPR SYSIN control statements.
pub fn parse_compare_sysin(statements: &[String]) -> Option<CompareConfig> {
    for stmt in statements {
        let trimmed = stmt.trim().to_uppercase();
        if trimmed.is_empty() || trimmed.starts_with("/*") {
            continue;
        }

        if trimmed.starts_with("COMPARE ") {
            if let Some(typorg) = extract_typorg(&trimmed) {
                return Some(CompareConfig { typorg });
            }
        }
    }

    // Default to sequential if no COMPARE statement.
    Some(CompareConfig {
        typorg: CompareTyporg::Sequential,
    })
}

fn extract_typorg(stmt: &str) -> Option<CompareTyporg> {
    if let Some(pos) = stmt.find("TYPORG=") {
        let start = pos + 7;
        let rest = &stmt[start..];
        let end = rest.find([',', ' ']).unwrap_or(rest.len());
        let val = &rest[..end];
        match val {
            "PS" => Some(CompareTyporg::Sequential),
            "PO" => Some(CompareTyporg::Partitioned),
            _ => None,
        }
    } else {
        Some(CompareTyporg::Sequential)
    }
}

// ─────────────────────── Comparison Result ───────────────────────

/// A single record mismatch found during comparison.
#[derive(Debug, Clone)]
pub struct Mismatch {
    /// Record number (1-based) where the mismatch occurred.
    pub record_number: usize,
    /// Member name (for PDS comparisons).
    pub member_name: Option<String>,
    /// Description of the mismatch.
    pub description: String,
}

// ─────────────────────── IEBCOMPR Implementation ───────────────────────

/// IEBCOMPR — Dataset comparison utility.
pub struct Iebcompr;

impl UtilityProgram for Iebcompr {
    fn name(&self) -> &str {
        "IEBCOMPR"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let stmts = context.read_sysin();
        let config = match parse_compare_sysin(&stmts) {
            Some(c) => c,
            None => {
                let msg = UtilityMessage::severe(
                    &format_message_id("IEB", 501, MessageSeverity::Severe),
                    "INVALID COMPARE CONTROL STATEMENT",
                );
                context.write_utility_message(&msg);
                return UtilityResult::new(12, vec![msg]);
            }
        };

        match config.typorg {
            CompareTyporg::Sequential => self.compare_sequential(context),
            CompareTyporg::Partitioned => self.compare_partitioned(context),
        }
    }
}

impl Iebcompr {
    fn compare_sequential(&self, context: &mut UtilityContext) -> UtilityResult {
        let mut messages = Vec::new();

        // Read SYSUT1 (first dataset).
        let data1 = match context.open_input("SYSUT1") {
            Ok(d) => d,
            Err(_) => {
                let msg = UtilityMessage::severe(
                    &format_message_id("IEB", 502, MessageSeverity::Severe),
                    "SYSUT1 DD NOT ALLOCATED",
                );
                messages.push(msg);
                return UtilityResult::new(12, messages);
            }
        };

        // Read SYSUT2 (second dataset).
        let data2 = match context.open_input("SYSUT2") {
            Ok(d) => d,
            Err(_) => {
                let msg = UtilityMessage::severe(
                    &format_message_id("IEB", 503, MessageSeverity::Severe),
                    "SYSUT2 DD NOT ALLOCATED",
                );
                messages.push(msg);
                return UtilityResult::new(12, messages);
            }
        };

        let mut mismatches = Vec::new();

        // Compare record by record.
        let max_len = data1.len().max(data2.len());
        for i in 0..max_len {
            match (data1.get(i), data2.get(i)) {
                (Some(r1), Some(r2)) => {
                    if r1 != r2 {
                        mismatches.push(Mismatch {
                            record_number: i + 1,
                            member_name: None,
                            description: format!(
                                "RECORD {} MISMATCH",
                                i + 1
                            ),
                        });
                    }
                }
                (Some(_), None) => {
                    mismatches.push(Mismatch {
                        record_number: i + 1,
                        member_name: None,
                        description: format!(
                            "RECORD {} EXISTS IN SYSUT1 BUT NOT SYSUT2",
                            i + 1
                        ),
                    });
                }
                (None, Some(_)) => {
                    mismatches.push(Mismatch {
                        record_number: i + 1,
                        member_name: None,
                        description: format!(
                            "RECORD {} EXISTS IN SYSUT2 BUT NOT SYSUT1",
                            i + 1
                        ),
                    });
                }
                (None, None) => unreachable!(),
            }
        }

        self.format_results(context, &mismatches, &mut messages, data1.len(), data2.len())
    }

    fn compare_partitioned(&self, context: &mut UtilityContext) -> UtilityResult {
        let mut messages = Vec::new();

        // Get PDS data from SYSUT1.
        let sysut1 = context.get_dd("SYSUT1");
        let Some(sysut1) = sysut1 else {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 502, MessageSeverity::Severe),
                "SYSUT1 DD NOT ALLOCATED",
            );
            messages.push(msg);
            return UtilityResult::new(12, messages);
        };
        let Some(ref pds1) = sysut1.pds_data else {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 504, MessageSeverity::Severe),
                "SYSUT1 IS NOT A PDS",
            );
            messages.push(msg);
            return UtilityResult::new(12, messages);
        };
        let pds1 = pds1.clone();

        // Get PDS data from SYSUT2.
        let sysut2 = context.get_dd("SYSUT2");
        let Some(sysut2) = sysut2 else {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 503, MessageSeverity::Severe),
                "SYSUT2 DD NOT ALLOCATED",
            );
            messages.push(msg);
            return UtilityResult::new(12, messages);
        };
        let Some(ref pds2) = sysut2.pds_data else {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 505, MessageSeverity::Severe),
                "SYSUT2 IS NOT A PDS",
            );
            messages.push(msg);
            return UtilityResult::new(12, messages);
        };
        let pds2 = pds2.clone();

        let mut mismatches = Vec::new();

        // Collect all unique member names from both PDS datasets.
        let mut all_names: Vec<String> = pds1.member_names();
        for name in pds2.member_names() {
            if !all_names.contains(&name) {
                all_names.push(name);
            }
        }
        all_names.sort();

        for name in &all_names {
            let m1 = pds1.get_member(name);
            let m2 = pds2.get_member(name);

            match (m1, m2) {
                (Some(mem1), Some(mem2)) => {
                    // Compare member content record by record.
                    let max_len = mem1.content.len().max(mem2.content.len());
                    for i in 0..max_len {
                        match (mem1.content.get(i), mem2.content.get(i)) {
                            (Some(r1), Some(r2)) if r1 != r2 => {
                                mismatches.push(Mismatch {
                                    record_number: i + 1,
                                    member_name: Some(name.clone()),
                                    description: format!(
                                        "MEMBER {} RECORD {} MISMATCH",
                                        name,
                                        i + 1
                                    ),
                                });
                            }
                            (Some(_), None) => {
                                mismatches.push(Mismatch {
                                    record_number: i + 1,
                                    member_name: Some(name.clone()),
                                    description: format!(
                                        "MEMBER {} RECORD {} IN SYSUT1 ONLY",
                                        name,
                                        i + 1
                                    ),
                                });
                            }
                            (None, Some(_)) => {
                                mismatches.push(Mismatch {
                                    record_number: i + 1,
                                    member_name: Some(name.clone()),
                                    description: format!(
                                        "MEMBER {} RECORD {} IN SYSUT2 ONLY",
                                        name,
                                        i + 1
                                    ),
                                });
                            }
                            _ => {}
                        }
                    }
                }
                (Some(_), None) => {
                    mismatches.push(Mismatch {
                        record_number: 0,
                        member_name: Some(name.clone()),
                        description: format!(
                            "MEMBER {} EXISTS IN SYSUT1 BUT NOT SYSUT2",
                            name
                        ),
                    });
                }
                (None, Some(_)) => {
                    mismatches.push(Mismatch {
                        record_number: 0,
                        member_name: Some(name.clone()),
                        description: format!(
                            "MEMBER {} EXISTS IN SYSUT2 BUT NOT SYSUT1",
                            name
                        ),
                    });
                }
                (None, None) => unreachable!(),
            }
        }

        let total_members = all_names.len();
        self.format_pds_results(context, &mismatches, &mut messages, total_members)
    }

    fn format_results(
        &self,
        context: &mut UtilityContext,
        mismatches: &[Mismatch],
        messages: &mut Vec<UtilityMessage>,
        rec_count1: usize,
        rec_count2: usize,
    ) -> UtilityResult {
        if mismatches.is_empty() {
            let msg = UtilityMessage::info(
                &format_message_id("IEB", 510, MessageSeverity::Info),
                &format!(
                    "DATASETS ARE IDENTICAL ({} RECORDS COMPARED)",
                    rec_count1
                ),
            );
            messages.push(msg.clone());
            context.write_utility_message(&msg);
            UtilityResult::success_with(messages.clone())
        } else {
            // Report each mismatch.
            for mm in mismatches {
                let msg = UtilityMessage::error(
                    &format_message_id("IEB", 511, MessageSeverity::Error),
                    &mm.description,
                );
                messages.push(msg.clone());
                context.write_utility_message(&msg);
            }

            let summary = UtilityMessage::error(
                &format_message_id("IEB", 512, MessageSeverity::Error),
                &format!(
                    "{} DIFFERENCES FOUND (SYSUT1={} RECORDS, SYSUT2={} RECORDS)",
                    mismatches.len(),
                    rec_count1,
                    rec_count2
                ),
            );
            messages.push(summary.clone());
            context.write_utility_message(&summary);

            UtilityResult::new(8, messages.clone())
        }
    }

    fn format_pds_results(
        &self,
        context: &mut UtilityContext,
        mismatches: &[Mismatch],
        messages: &mut Vec<UtilityMessage>,
        total_members: usize,
    ) -> UtilityResult {
        if mismatches.is_empty() {
            let msg = UtilityMessage::info(
                &format_message_id("IEB", 520, MessageSeverity::Info),
                &format!(
                    "PDS DATASETS ARE IDENTICAL ({total_members} MEMBERS COMPARED)"
                ),
            );
            messages.push(msg.clone());
            context.write_utility_message(&msg);
            UtilityResult::success_with(messages.clone())
        } else {
            for mm in mismatches {
                let msg = UtilityMessage::error(
                    &format_message_id("IEB", 521, MessageSeverity::Error),
                    &mm.description,
                );
                messages.push(msg.clone());
                context.write_utility_message(&msg);
            }

            let summary = UtilityMessage::error(
                &format_message_id("IEB", 522, MessageSeverity::Error),
                &format!(
                    "{} DIFFERENCES FOUND IN {total_members} MEMBERS COMPARED",
                    mismatches.len()
                ),
            );
            messages.push(summary.clone());
            context.write_utility_message(&summary);

            UtilityResult::new(8, messages.clone())
        }
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{DdAllocation, PdsData, UtilityRegistry};

    fn setup_seq_compare(
        data1: Vec<String>,
        data2: Vec<String>,
        sysin: Vec<String>,
    ) -> UtilityContext {
        let mut ctx = UtilityContext::new("STEP01", "IEBCOMPR");
        ctx.add_dd(DdAllocation::inline("SYSUT1", data1));
        ctx.add_dd(DdAllocation::inline("SYSUT2", data2));
        ctx.add_dd(DdAllocation::inline("SYSIN", sysin));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx
    }

    // ─────── UTIL-102.1: Sequential Dataset Comparison ───────

    #[test]
    fn test_identical_sequential() {
        let data = vec!["REC1".to_string(), "REC2".to_string(), "REC3".to_string()];
        let mut ctx = setup_seq_compare(
            data.clone(),
            data,
            vec![" COMPARE TYPORG=PS".to_string()],
        );

        let result = Iebcompr.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
    }

    #[test]
    fn test_different_sequential() {
        let data1 = vec!["REC1".to_string(), "REC2".to_string()];
        let data2 = vec!["REC1".to_string(), "REC2X".to_string()];
        let mut ctx = setup_seq_compare(
            data1,
            data2,
            vec![" COMPARE TYPORG=PS".to_string()],
        );

        let result = Iebcompr.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);
    }

    #[test]
    fn test_different_lengths_sequential() {
        let data1 = vec!["REC1".to_string(), "REC2".to_string(), "REC3".to_string()];
        let data2 = vec!["REC1".to_string(), "REC2".to_string()];
        let mut ctx = setup_seq_compare(
            data1,
            data2,
            vec![" COMPARE TYPORG=PS".to_string()],
        );

        let result = Iebcompr.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);
    }

    #[test]
    fn test_empty_sequential_identical() {
        let mut ctx = setup_seq_compare(
            vec![],
            vec![],
            vec![" COMPARE TYPORG=PS".to_string()],
        );

        let result = Iebcompr.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
    }

    #[test]
    fn test_default_typorg_is_sequential() {
        let data = vec!["REC1".to_string()];
        let mut ctx = setup_seq_compare(data.clone(), data, vec![]);

        let result = Iebcompr.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
    }

    // ─────── UTIL-102.2: PDS Member Comparison ───────

    #[test]
    fn test_identical_pds() {
        let mut pds1 = PdsData::new();
        pds1.add_member("MOD1", vec!["CODE1".to_string()]);
        pds1.add_member("MOD2", vec!["CODE2".to_string()]);

        let mut pds2 = PdsData::new();
        pds2.add_member("MOD1", vec!["CODE1".to_string()]);
        pds2.add_member("MOD2", vec!["CODE2".to_string()]);

        let mut ctx = UtilityContext::new("STEP01", "IEBCOMPR");
        ctx.add_dd(DdAllocation::pds("SYSUT1", "PDS1", "SHR", pds1));
        ctx.add_dd(DdAllocation::pds("SYSUT2", "PDS2", "SHR", pds2));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![" COMPARE TYPORG=PO".to_string()],
        ));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebcompr.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
    }

    #[test]
    fn test_pds_member_content_differs() {
        let mut pds1 = PdsData::new();
        pds1.add_member("MOD1", vec!["VERSION1".to_string()]);

        let mut pds2 = PdsData::new();
        pds2.add_member("MOD1", vec!["VERSION2".to_string()]);

        let mut ctx = UtilityContext::new("STEP01", "IEBCOMPR");
        ctx.add_dd(DdAllocation::pds("SYSUT1", "PDS1", "SHR", pds1));
        ctx.add_dd(DdAllocation::pds("SYSUT2", "PDS2", "SHR", pds2));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![" COMPARE TYPORG=PO".to_string()],
        ));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebcompr.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);
    }

    #[test]
    fn test_pds_missing_member_in_sysut2() {
        let mut pds1 = PdsData::new();
        pds1.add_member("MOD1", vec!["CODE".to_string()]);
        pds1.add_member("MOD2", vec!["CODE".to_string()]);

        let mut pds2 = PdsData::new();
        pds2.add_member("MOD1", vec!["CODE".to_string()]);
        // MOD2 missing.

        let mut ctx = UtilityContext::new("STEP01", "IEBCOMPR");
        ctx.add_dd(DdAllocation::pds("SYSUT1", "PDS1", "SHR", pds1));
        ctx.add_dd(DdAllocation::pds("SYSUT2", "PDS2", "SHR", pds2));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![" COMPARE TYPORG=PO".to_string()],
        ));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebcompr.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);
    }

    #[test]
    fn test_pds_extra_member_in_sysut2() {
        let mut pds1 = PdsData::new();
        pds1.add_member("MOD1", vec!["CODE".to_string()]);

        let mut pds2 = PdsData::new();
        pds2.add_member("MOD1", vec!["CODE".to_string()]);
        pds2.add_member("MOD2", vec!["EXTRA".to_string()]);

        let mut ctx = UtilityContext::new("STEP01", "IEBCOMPR");
        ctx.add_dd(DdAllocation::pds("SYSUT1", "PDS1", "SHR", pds1));
        ctx.add_dd(DdAllocation::pds("SYSUT2", "PDS2", "SHR", pds2));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![" COMPARE TYPORG=PO".to_string()],
        ));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebcompr.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);
    }

    // ─────── UTIL-102.3: Mismatch Reporting ───────

    #[test]
    fn test_sequential_mismatch_report() {
        let data1 = vec!["REC1".to_string(), "DIFF".to_string(), "REC3".to_string()];
        let data2 = vec!["REC1".to_string(), "XXXX".to_string(), "REC3".to_string()];
        let mut ctx = setup_seq_compare(
            data1,
            data2,
            vec![" COMPARE TYPORG=PS".to_string()],
        );

        let result = Iebcompr.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);

        let output = ctx.sysprint_output();
        assert!(output.iter().any(|l| l.contains("RECORD 2")));
        assert!(output.iter().any(|l| l.contains("1 DIFFERENCES")));
    }

    #[test]
    fn test_pds_mismatch_report_includes_member() {
        let mut pds1 = PdsData::new();
        pds1.add_member("BAD", vec!["V1".to_string()]);

        let mut pds2 = PdsData::new();
        pds2.add_member("BAD", vec!["V2".to_string()]);

        let mut ctx = UtilityContext::new("STEP01", "IEBCOMPR");
        ctx.add_dd(DdAllocation::pds("SYSUT1", "PDS1", "SHR", pds1));
        ctx.add_dd(DdAllocation::pds("SYSUT2", "PDS2", "SHR", pds2));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![" COMPARE TYPORG=PO".to_string()],
        ));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebcompr.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);

        let output = ctx.sysprint_output();
        assert!(output.iter().any(|l| l.contains("MEMBER BAD")));
    }

    #[test]
    fn test_identical_report() {
        let data = vec!["REC1".to_string()];
        let mut ctx = setup_seq_compare(
            data.clone(),
            data,
            vec![" COMPARE TYPORG=PS".to_string()],
        );

        Iebcompr.execute(&mut ctx);

        let output = ctx.sysprint_output();
        assert!(output.iter().any(|l| l.contains("IDENTICAL")));
    }

    // ─────── UTIL-102.4: IEBCOMPR Tests ───────

    #[test]
    fn test_missing_sysut1_cc12() {
        let mut ctx = UtilityContext::new("STEP01", "IEBCOMPR");
        ctx.add_dd(DdAllocation::inline("SYSUT2", vec![]));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebcompr.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    #[test]
    fn test_missing_sysut2_cc12() {
        let mut ctx = UtilityContext::new("STEP01", "IEBCOMPR");
        ctx.add_dd(DdAllocation::inline("SYSUT1", vec![]));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebcompr.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    #[test]
    fn test_via_registry() {
        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(Iebcompr));

        let data = vec!["REC1".to_string()];
        let mut ctx = setup_seq_compare(data.clone(), data, vec![]);

        let result = reg.dispatch("IEBCOMPR", &mut ctx).unwrap();
        assert_eq!(result.condition_code, 0);
    }

    #[test]
    fn test_parse_typorg_ps() {
        let config =
            parse_compare_sysin(&[" COMPARE TYPORG=PS".to_string()]).unwrap();
        assert_eq!(config.typorg, CompareTyporg::Sequential);
    }

    #[test]
    fn test_parse_typorg_po() {
        let config =
            parse_compare_sysin(&[" COMPARE TYPORG=PO".to_string()]).unwrap();
        assert_eq!(config.typorg, CompareTyporg::Partitioned);
    }

    #[test]
    fn test_large_sequential_identical() {
        let data: Vec<String> = (1..=1000)
            .map(|i| format!("RECORD {:06}", i))
            .collect();
        let mut ctx = setup_seq_compare(data.clone(), data, vec![]);

        let result = Iebcompr.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
    }

    #[test]
    fn test_large_sequential_one_diff() {
        let data1: Vec<String> = (1..=100).map(|i| format!("REC{:04}", i)).collect();
        let mut data2 = data1.clone();
        data2[49] = "CHANGED!".to_string();

        let mut ctx = setup_seq_compare(data1, data2, vec![]);

        let result = Iebcompr.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);

        let output = ctx.sysprint_output();
        assert!(output.iter().any(|l| l.contains("RECORD 50")));
    }

    #[test]
    fn test_pds_not_allocated_as_pds() {
        let mut ctx = UtilityContext::new("STEP01", "IEBCOMPR");
        ctx.add_dd(DdAllocation::inline("SYSUT1", vec![]));
        ctx.add_dd(DdAllocation::inline("SYSUT2", vec![]));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![" COMPARE TYPORG=PO".to_string()],
        ));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebcompr.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }
}
