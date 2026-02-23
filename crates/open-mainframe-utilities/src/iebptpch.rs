//! IEBPTPCH — Print/Punch utility.
//!
//! Prints or punches sequential or PDS datasets. IEBPTPCH reads SYSIN
//! control statements specifying the operation (PRINT or PUNCH) and
//! optional field selection/formatting.
//!
//! ## Control Statement Syntax
//!
//! ```text
//! PRINT TYPORG=PS|PO,MAXFLDS=n,MAXNAME=n
//! PUNCH TYPORG=PS|PO,MAXFLDS=n
//! RECORD FIELD=(length,input_pos,conversion,output_pos)
//! MEMBER NAME=member
//! ```
//!
//! ## Condition Codes
//!
//! | CC | Meaning |
//! |----|---------|
//! | 0  | All operations successful |
//! | 4  | Warning — empty input or member not found |
//! | 8  | Error — invalid parameters or missing DD |
//! | 12 | Severe — syntax error |

use crate::{
    format_message_id, MessageSeverity, UtilityContext, UtilityMessage, UtilityProgram,
    UtilityResult,
};

// ─────────────────────── Control Statement Parsing ───────────────────────

/// IEBPTPCH operation mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PtpchMode {
    /// PRINT — format records for printing (line output).
    Print,
    /// PUNCH — format records for card punch output.
    Punch,
}

/// Dataset organization for IEBPTPCH.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PtpchTyporg {
    /// PS — physical sequential.
    Sequential,
    /// PO — partitioned (PDS).
    Partitioned,
}

impl Default for PtpchTyporg {
    fn default() -> Self {
        Self::Sequential
    }
}

/// A field selection/conversion spec for RECORD FIELD=.
#[derive(Debug, Clone)]
pub struct PtpchField {
    /// Number of bytes to extract from input.
    pub length: usize,
    /// Starting position in input record (1-based).
    pub input_pos: usize,
    /// Conversion code: empty or "XE" for hex.
    pub conversion: Option<String>,
    /// Starting position in output record (1-based).
    pub output_pos: usize,
}

/// Parsed IEBPTPCH configuration.
#[derive(Debug, Clone)]
pub struct PtpchConfig {
    /// Operation mode (PRINT or PUNCH).
    pub mode: PtpchMode,
    /// Dataset organization.
    pub typorg: PtpchTyporg,
    /// Maximum number of FIELD parameters (MAXFLDS).
    pub maxflds: usize,
    /// Maximum number of NAME parameters for members (MAXNAME).
    pub maxname: usize,
    /// RECORD FIELD= specifications.
    pub fields: Vec<PtpchField>,
    /// MEMBER NAME= — specific members to process (PDS only).
    pub members: Vec<String>,
    /// Maximum number of lines per page (for PRINT).
    pub maxline: usize,
    /// Whether to stop after first member not found.
    pub stopaft: Option<usize>,
}

impl Default for PtpchConfig {
    fn default() -> Self {
        Self {
            mode: PtpchMode::Print,
            typorg: PtpchTyporg::Sequential,
            maxflds: 0,
            maxname: 0,
            fields: Vec::new(),
            members: Vec::new(),
            maxline: 60,
            stopaft: None,
        }
    }
}

/// Parse IEBPTPCH SYSIN control statements into configuration.
pub fn parse_ptpch_sysin(statements: &[String]) -> Option<PtpchConfig> {
    let mut config = PtpchConfig::default();
    let mut mode_found = false;

    for stmt in statements {
        let trimmed = stmt.trim().to_uppercase();
        if trimmed.is_empty() || trimmed.starts_with('*') {
            continue;
        }

        if trimmed.starts_with("PRINT") {
            config.mode = PtpchMode::Print;
            mode_found = true;
            parse_print_punch_params(&trimmed, &mut config);
        } else if trimmed.starts_with("PUNCH") {
            config.mode = PtpchMode::Punch;
            mode_found = true;
            parse_print_punch_params(&trimmed, &mut config);
        } else if trimmed.starts_with("RECORD") {
            parse_record_params(&trimmed, &mut config);
        } else if trimmed.starts_with("MEMBER") {
            if let Some(name) = extract_param(&trimmed, "NAME=") {
                config.members.push(name);
            }
        }
    }

    if mode_found {
        Some(config)
    } else {
        None
    }
}

fn parse_print_punch_params(stmt: &str, config: &mut PtpchConfig) {
    if let Some(typorg) = extract_param(stmt, "TYPORG=") {
        config.typorg = match typorg.as_str() {
            "PO" => PtpchTyporg::Partitioned,
            _ => PtpchTyporg::Sequential,
        };
    }
    if let Some(maxflds) = extract_param(stmt, "MAXFLDS=") {
        config.maxflds = maxflds.parse().unwrap_or(0);
    }
    if let Some(maxname) = extract_param(stmt, "MAXNAME=") {
        config.maxname = maxname.parse().unwrap_or(0);
    }
    if let Some(maxline) = extract_param(stmt, "MAXLINE=") {
        config.maxline = maxline.parse().unwrap_or(60);
    }
    if let Some(stopaft) = extract_param(stmt, "STOPAFT=") {
        config.stopaft = stopaft.parse().ok();
    }
}

fn parse_record_params(stmt: &str, config: &mut PtpchConfig) {
    // Parse FIELD=(length,input_pos,conversion,output_pos) or FIELD=(length,input_pos,,output_pos)
    if let Some(field_start) = stmt.find("FIELD=(") {
        let rest = &stmt[field_start + 7..];
        if let Some(end) = rest.find(')') {
            let params: Vec<&str> = rest[..end].split(',').collect();
            if params.len() >= 2 {
                let length = params[0].trim().parse().unwrap_or(0);
                let input_pos = params[1].trim().parse().unwrap_or(1);
                let conversion = if params.len() > 2 && !params[2].trim().is_empty() {
                    Some(params[2].trim().to_string())
                } else {
                    None
                };
                let output_pos = if params.len() > 3 {
                    params[3].trim().parse().unwrap_or(1)
                } else {
                    1
                };

                config.fields.push(PtpchField {
                    length,
                    input_pos,
                    conversion,
                    output_pos,
                });
            }
        }
    }
}

fn extract_param(stmt: &str, key: &str) -> Option<String> {
    let pos = stmt.find(key)?;
    let start = pos + key.len();
    let rest = &stmt[start..];
    let end = rest.find([',', ' ', ')']).unwrap_or(rest.len());
    let val = rest[..end].trim().to_string();
    if val.is_empty() { None } else { Some(val) }
}

// ─────────────────────── IEBPTPCH Implementation ───────────────────────

/// IEBPTPCH — print/punch utility.
pub struct Iebptpch;

impl UtilityProgram for Iebptpch {
    fn name(&self) -> &str {
        "IEBPTPCH"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let stmts = context.read_sysin();
        if stmts.is_empty() {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 501, MessageSeverity::Severe),
                "NO CONTROL STATEMENTS IN SYSIN",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(12, vec![msg]);
        }

        let config = match parse_ptpch_sysin(&stmts) {
            Some(c) => c,
            None => {
                let msg = UtilityMessage::severe(
                    &format_message_id("IEB", 502, MessageSeverity::Severe),
                    "NO PRINT OR PUNCH STATEMENT FOUND",
                );
                context.write_utility_message(&msg);
                return UtilityResult::new(12, vec![msg]);
            }
        };

        match config.typorg {
            PtpchTyporg::Sequential => execute_sequential(context, &config),
            PtpchTyporg::Partitioned => execute_partitioned(context, &config),
        }
    }
}

fn execute_sequential(context: &mut UtilityContext, config: &PtpchConfig) -> UtilityResult {
    let input = match context.open_input("SYSUT1") {
        Ok(data) => data,
        Err(_) => {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 503, MessageSeverity::Severe),
                "SYSUT1 DD NOT ALLOCATED",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(8, vec![msg]);
        }
    };

    let mut messages = Vec::new();

    if input.is_empty() {
        let msg = UtilityMessage::warning(
            &format_message_id("IEB", 510, MessageSeverity::Warning),
            "INPUT DATASET IS EMPTY",
        );
        messages.push(msg);
    }

    let mode_str = match config.mode {
        PtpchMode::Print => "PRINT",
        PtpchMode::Punch => "PUNCH",
    };

    // Process records
    let mut output_count = 0;
    let limit = config.stopaft.unwrap_or(usize::MAX);

    for record in &input {
        if output_count >= limit {
            break;
        }

        let formatted = format_record(record, &config.fields, config.mode);
        if let Some(out_dd) = context.get_dd_mut("SYSUT2") {
            out_dd.output.push(formatted);
        }
        output_count += 1;
    }

    let msg = UtilityMessage::info(
        &format_message_id("IEB", 511, MessageSeverity::Info),
        &format!("{mode_str} COMPLETE — {output_count} RECORDS PROCESSED"),
    );
    messages.push(msg);

    for m in &messages {
        context.write_utility_message(m);
    }

    let cc = if messages.iter().any(|m| m.severity == MessageSeverity::Warning) {
        4
    } else {
        0
    };

    UtilityResult::new(cc, messages)
}

fn execute_partitioned(context: &mut UtilityContext, config: &PtpchConfig) -> UtilityResult {
    let sysut1 = context.get_dd("SYSUT1");
    let pds = match sysut1.and_then(|dd| dd.pds_data.as_ref()) {
        Some(p) => p.clone(),
        None => {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 504, MessageSeverity::Severe),
                "SYSUT1 IS NOT ALLOCATED AS A PDS",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(8, vec![msg]);
        }
    };

    let mut messages = Vec::new();
    let mut max_cc: u32 = 0;

    let mode_str = match config.mode {
        PtpchMode::Print => "PRINT",
        PtpchMode::Punch => "PUNCH",
    };

    // Determine which members to process
    let members_to_process: Vec<String> = if config.members.is_empty() {
        pds.member_names()
    } else {
        config.members.clone()
    };

    let mut total_records = 0;

    for member_name in &members_to_process {
        match pds.get_member(member_name) {
            Some(member) => {
                // Output member header
                if let Some(out_dd) = context.get_dd_mut("SYSUT2") {
                    out_dd.output.push(format!("MEMBER NAME  {member_name}"));
                }

                // Output member records
                for record in &member.content {
                    let formatted = format_record(record, &config.fields, config.mode);
                    if let Some(out_dd) = context.get_dd_mut("SYSUT2") {
                        out_dd.output.push(formatted);
                    }
                    total_records += 1;
                }
            }
            None => {
                let msg = UtilityMessage::warning(
                    &format_message_id("IEB", 512, MessageSeverity::Warning),
                    &format!("MEMBER {member_name} NOT FOUND"),
                );
                messages.push(msg);
                max_cc = max_cc.max(4);
            }
        }
    }

    let msg = UtilityMessage::info(
        &format_message_id("IEB", 513, MessageSeverity::Info),
        &format!(
            "{mode_str} COMPLETE — {} MEMBERS, {total_records} RECORDS",
            members_to_process.len()
        ),
    );
    messages.push(msg);

    for m in &messages {
        context.write_utility_message(m);
    }

    UtilityResult::new(max_cc, messages)
}

/// Format a record for output, applying FIELD selection and conversion.
fn format_record(record: &str, fields: &[PtpchField], mode: PtpchMode) -> String {
    if fields.is_empty() {
        // No FIELD specs — output the full record.
        match mode {
            PtpchMode::Print => record.to_string(),
            PtpchMode::Punch => {
                // Punch mode truncates to 80 columns.
                if record.len() > 80 {
                    record[..80].to_string()
                } else {
                    record.to_string()
                }
            }
        }
    } else {
        // Apply FIELD selections to build output record.
        let input_bytes = record.as_bytes();
        let mut output = [b' '; 132]; // Max print line

        for field in fields {
            let src_start = field.input_pos.saturating_sub(1);
            let src_end = (src_start + field.length).min(input_bytes.len());
            let dst_start = field.output_pos.saturating_sub(1);

            if src_start < input_bytes.len() {
                let src_data = &input_bytes[src_start..src_end];

                let converted = match field.conversion.as_deref() {
                    Some("XE") => {
                        // Hex expansion: each byte → 2 hex chars.
                        let mut hex = Vec::new();
                        for byte in src_data {
                            hex.push(HEX_CHARS[(byte >> 4) as usize]);
                            hex.push(HEX_CHARS[(byte & 0x0F) as usize]);
                        }
                        hex
                    }
                    _ => src_data.to_vec(),
                };

                let dst_end = (dst_start + converted.len()).min(output.len());
                let copy_len = dst_end - dst_start;
                output[dst_start..dst_end].copy_from_slice(&converted[..copy_len]);
            }
        }

        // Trim trailing spaces
        let end = output.iter().rposition(|&b| b != b' ').map_or(0, |p| p + 1);
        String::from_utf8_lossy(&output[..end]).to_string()
    }
}

const HEX_CHARS: [u8; 16] = *b"0123456789ABCDEF";

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{DdAllocation, PdsData, UtilityContext};

    fn setup_ptpch_context_seq(
        input: Vec<String>,
        sysin: Vec<String>,
    ) -> UtilityContext {
        let mut ctx = UtilityContext::new("STEP01", "IEBPTPCH");
        ctx.add_dd(DdAllocation::inline("SYSUT1", input));
        ctx.add_dd(DdAllocation::output("SYSUT2"));
        ctx.add_dd(DdAllocation::inline("SYSIN", sysin));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx
    }

    fn setup_ptpch_context_pds(
        pds: PdsData,
        sysin: Vec<String>,
    ) -> UtilityContext {
        let mut ctx = UtilityContext::new("STEP01", "IEBPTPCH");
        ctx.add_dd(DdAllocation::pds("SYSUT1", "MY.PDS", "SHR", pds));
        ctx.add_dd(DdAllocation::output("SYSUT2"));
        ctx.add_dd(DdAllocation::inline("SYSIN", sysin));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx
    }

    // ─────── UTIL-105.1: PRINT Operation ───────

    #[test]
    fn test_print_sequential() {
        let input = vec![
            "RECORD ONE".to_string(),
            "RECORD TWO".to_string(),
            "RECORD THREE".to_string(),
        ];
        let sysin = vec!["PRINT TYPORG=PS".to_string()];
        let mut ctx = setup_ptpch_context_seq(input, sysin);
        let result = Iebptpch.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        assert_eq!(out.output.len(), 3);
        assert_eq!(out.output[0], "RECORD ONE");
    }

    #[test]
    fn test_print_pds_all_members() {
        let mut pds = PdsData::new();
        pds.add_member("MOD1", vec!["LINE A".to_string()]);
        pds.add_member("MOD2", vec!["LINE B".to_string(), "LINE C".to_string()]);

        let sysin = vec!["PRINT TYPORG=PO".to_string()];
        let mut ctx = setup_ptpch_context_pds(pds, sysin);
        let result = Iebptpch.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        // 2 member headers + 3 data lines
        assert_eq!(out.output.len(), 5);
    }

    #[test]
    fn test_print_pds_specific_member() {
        let mut pds = PdsData::new();
        pds.add_member("MOD1", vec!["LINE A".to_string()]);
        pds.add_member("MOD2", vec!["LINE B".to_string()]);

        let sysin = vec![
            "PRINT TYPORG=PO,MAXNAME=1".to_string(),
            "MEMBER NAME=MOD1".to_string(),
        ];
        let mut ctx = setup_ptpch_context_pds(pds, sysin);
        let result = Iebptpch.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        // 1 member header + 1 data line
        assert_eq!(out.output.len(), 2);
        assert!(out.output[0].contains("MOD1"));
    }

    #[test]
    fn test_print_empty_input_warns() {
        let sysin = vec!["PRINT TYPORG=PS".to_string()];
        let mut ctx = setup_ptpch_context_seq(vec![], sysin);
        let result = Iebptpch.execute(&mut ctx);
        assert_eq!(result.condition_code, 4);
    }

    #[test]
    fn test_print_member_not_found_warns() {
        let pds = PdsData::new();
        let sysin = vec![
            "PRINT TYPORG=PO".to_string(),
            "MEMBER NAME=NOSUCH".to_string(),
        ];
        let mut ctx = setup_ptpch_context_pds(pds, sysin);
        let result = Iebptpch.execute(&mut ctx);
        assert_eq!(result.condition_code, 4);
    }

    // ─────── UTIL-105.2: PUNCH Operation ───────

    #[test]
    fn test_punch_sequential() {
        let input = vec!["DATA1".to_string(), "DATA2".to_string()];
        let sysin = vec!["PUNCH TYPORG=PS".to_string()];
        let mut ctx = setup_ptpch_context_seq(input, sysin);
        let result = Iebptpch.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        assert_eq!(out.output.len(), 2);
    }

    #[test]
    fn test_punch_truncates_80() {
        let long_line = "A".repeat(120);
        let sysin = vec!["PUNCH TYPORG=PS".to_string()];
        let mut ctx = setup_ptpch_context_seq(vec![long_line], sysin);
        Iebptpch.execute(&mut ctx);

        let out = ctx.get_dd("SYSUT2").unwrap();
        assert_eq!(out.output[0].len(), 80);
    }

    // ─────── UTIL-105.3: Field Selection and Formatting ───────

    #[test]
    fn test_field_selection() {
        let input = vec!["ABCDEFGHIJ".to_string()];
        let sysin = vec![
            "PRINT TYPORG=PS,MAXFLDS=1".to_string(),
            "RECORD FIELD=(3,4,,1)".to_string(),
        ];
        let mut ctx = setup_ptpch_context_seq(input, sysin);
        let result = Iebptpch.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        // Extract 3 bytes starting at input position 4 → "DEF"
        assert_eq!(out.output[0], "DEF");
    }

    #[test]
    fn test_field_hex_conversion() {
        let input = vec!["AB".to_string()];
        let sysin = vec![
            "PRINT TYPORG=PS,MAXFLDS=1".to_string(),
            "RECORD FIELD=(2,1,XE,1)".to_string(),
        ];
        let mut ctx = setup_ptpch_context_seq(input, sysin);
        Iebptpch.execute(&mut ctx);

        let out = ctx.get_dd("SYSUT2").unwrap();
        // 'A' = 0x41, 'B' = 0x42
        assert_eq!(out.output[0], "4142");
    }

    #[test]
    fn test_multiple_fields() {
        let input = vec!["ABCDEFGHIJ".to_string()];
        let sysin = vec![
            "PRINT TYPORG=PS,MAXFLDS=2".to_string(),
            "RECORD FIELD=(3,1,,1)".to_string(),
            "RECORD FIELD=(3,8,,5)".to_string(),
        ];
        let mut ctx = setup_ptpch_context_seq(input, sysin);
        Iebptpch.execute(&mut ctx);

        let out = ctx.get_dd("SYSUT2").unwrap();
        // Field 1: pos 1-3 → "ABC" at output pos 1
        // Field 2: pos 8-10 → "HIJ" at output pos 5
        assert_eq!(&out.output[0][0..3], "ABC");
        assert_eq!(&out.output[0][4..7], "HIJ");
    }

    // ─────── UTIL-105.4: IEBPTPCH Tests ───────

    #[test]
    fn test_no_sysin_cc12() {
        let mut ctx = UtilityContext::new("STEP01", "IEBPTPCH");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        let result = Iebptpch.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    #[test]
    fn test_invalid_sysin_cc12() {
        let sysin = vec!["GARBAGE".to_string()];
        let mut ctx = setup_ptpch_context_seq(vec![], sysin);
        let result = Iebptpch.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    #[test]
    fn test_parse_print_params() {
        let stmts = vec!["PRINT TYPORG=PO,MAXFLDS=3,MAXNAME=2,MAXLINE=80".to_string()];
        let config = parse_ptpch_sysin(&stmts).unwrap();
        assert_eq!(config.mode, PtpchMode::Print);
        assert_eq!(config.typorg, PtpchTyporg::Partitioned);
        assert_eq!(config.maxflds, 3);
        assert_eq!(config.maxname, 2);
        assert_eq!(config.maxline, 80);
    }

    #[test]
    fn test_parse_punch_params() {
        let stmts = vec!["PUNCH TYPORG=PS".to_string()];
        let config = parse_ptpch_sysin(&stmts).unwrap();
        assert_eq!(config.mode, PtpchMode::Punch);
        assert_eq!(config.typorg, PtpchTyporg::Sequential);
    }

    #[test]
    fn test_parse_record_field() {
        let stmts = vec![
            "PRINT TYPORG=PS,MAXFLDS=1".to_string(),
            "RECORD FIELD=(10,5,XE,1)".to_string(),
        ];
        let config = parse_ptpch_sysin(&stmts).unwrap();
        assert_eq!(config.fields.len(), 1);
        assert_eq!(config.fields[0].length, 10);
        assert_eq!(config.fields[0].input_pos, 5);
        assert_eq!(config.fields[0].conversion, Some("XE".to_string()));
        assert_eq!(config.fields[0].output_pos, 1);
    }

    #[test]
    fn test_parse_member_names() {
        let stmts = vec![
            "PRINT TYPORG=PO,MAXNAME=2".to_string(),
            "MEMBER NAME=MOD1".to_string(),
            "MEMBER NAME=MOD2".to_string(),
        ];
        let config = parse_ptpch_sysin(&stmts).unwrap();
        assert_eq!(config.members, vec!["MOD1", "MOD2"]);
    }

    #[test]
    fn test_sysprint_messages() {
        let input = vec!["DATA".to_string()];
        let sysin = vec!["PRINT TYPORG=PS".to_string()];
        let mut ctx = setup_ptpch_context_seq(input, sysin);
        Iebptpch.execute(&mut ctx);

        let output = ctx.sysprint_output();
        assert!(!output.is_empty());
        assert!(output.iter().any(|l| l.contains("PRINT COMPLETE")));
    }

    #[test]
    fn test_stopaft() {
        let input = vec![
            "LINE 1".to_string(),
            "LINE 2".to_string(),
            "LINE 3".to_string(),
            "LINE 4".to_string(),
            "LINE 5".to_string(),
        ];
        let sysin = vec!["PRINT TYPORG=PS,STOPAFT=2".to_string()];
        let mut ctx = setup_ptpch_context_seq(input, sysin);
        Iebptpch.execute(&mut ctx);

        let out = ctx.get_dd("SYSUT2").unwrap();
        assert_eq!(out.output.len(), 2);
    }

    #[test]
    fn test_via_registry() {
        use crate::UtilityRegistry;
        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(Iebptpch));

        let input = vec!["DATA".to_string()];
        let sysin = vec!["PRINT TYPORG=PS".to_string()];
        let mut ctx = setup_ptpch_context_seq(input, sysin);

        let result = reg.dispatch("IEBPTPCH", &mut ctx).unwrap();
        assert_eq!(result.condition_code, 0);
    }
}
