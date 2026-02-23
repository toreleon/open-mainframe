//! IEBGENER — Sequential copy with reformatting and member creation.
//!
//! IEBGENER copies sequential datasets (SYSUT1 → SYSUT2) with optional
//! field reformatting via GENERATE/RECORD control statements, and can
//! create PDS members from sequential input via the MEMBER statement.
//!
//! ## Control Statement Syntax
//!
//! ```text
//! GENERATE MAXFLDS=n,MAXLITS=n
//! RECORD FIELD=(length,input-pos,,output-pos)[,FIELD=(...)]
//! RECORD FIELD=(length,'literal',,output-pos)
//! MEMBER NAME=membername
//! ```
//!
//! ## Condition Codes
//!
//! | CC | Meaning |
//! |----|---------|
//! | 0  | Copy completed successfully |
//! | 4  | Warning (e.g., truncation) |
//! | 8  | Error in control statements |
//! | 12 | Severe — missing DD or invalid syntax |

use crate::{
    format_message_id, MessageSeverity, PdsData, UtilityContext, UtilityMessage, UtilityProgram,
    UtilityResult,
};

// ─────────────────────── Control Statement Types ───────────────────────

/// GENERATE statement parameters.
#[derive(Debug, Clone)]
pub struct GenerateParams {
    /// Maximum number of field specifications.
    pub max_flds: usize,
    /// Maximum number of literal characters.
    pub max_lits: usize,
}

impl Default for GenerateParams {
    fn default() -> Self {
        Self {
            max_flds: 1,
            max_lits: 0,
        }
    }
}

/// A single FIELD specification in a RECORD statement.
#[derive(Debug, Clone)]
pub struct FieldSpec {
    /// Field length in bytes.
    pub length: usize,
    /// Source: either an input byte position (1-based) or a literal string.
    pub source: FieldSource,
    /// Output position (1-based).
    pub output_pos: usize,
}

/// Source of data for a FIELD specification.
#[derive(Debug, Clone)]
pub enum FieldSource {
    /// Copy from input record at the given 1-based byte position.
    InputPosition(usize),
    /// Insert a literal string.
    Literal(String),
}

/// MEMBER statement to create a PDS member.
#[derive(Debug, Clone)]
pub struct MemberSpec {
    /// Member name (up to 8 chars, uppercase).
    pub name: String,
}

/// Parsed IEBGENER control statements.
#[derive(Debug, Clone, Default)]
pub struct IebgenerConfig {
    /// GENERATE parameters (if specified).
    pub generate: Option<GenerateParams>,
    /// RECORD field specifications.
    pub fields: Vec<FieldSpec>,
    /// MEMBER statement (if creating a PDS member).
    pub member: Option<MemberSpec>,
}

/// Parse IEBGENER SYSIN control statements.
pub fn parse_iebgener_sysin(statements: &[String]) -> IebgenerConfig {
    let mut config = IebgenerConfig::default();

    for stmt in statements {
        let trimmed = stmt.trim().to_uppercase();
        if trimmed.is_empty() || trimmed.starts_with("/*") {
            continue;
        }

        // We need the original for literal extraction (case-sensitive).
        let orig_trimmed = stmt.trim();

        if trimmed.starts_with("GENERATE ") {
            config.generate = Some(parse_generate(&trimmed));
        } else if trimmed.starts_with("RECORD ") {
            config.fields.extend(parse_record(orig_trimmed));
        } else if trimmed.starts_with("MEMBER ") {
            config.member = parse_member(&trimmed);
        }
    }

    config
}

fn parse_generate(stmt: &str) -> GenerateParams {
    let mut params = GenerateParams::default();

    if let Some(val) = extract_numeric_param(stmt, "MAXFLDS=") {
        params.max_flds = val;
    }
    if let Some(val) = extract_numeric_param(stmt, "MAXLITS=") {
        params.max_lits = val;
    }

    params
}

fn extract_numeric_param(stmt: &str, key: &str) -> Option<usize> {
    let pos = stmt.find(key)?;
    let start = pos + key.len();
    let rest = &stmt[start..];
    let end = rest.find([',', ' ', ')']).unwrap_or(rest.len());
    rest[..end].parse().ok()
}

fn parse_record(stmt: &str) -> Vec<FieldSpec> {
    let mut fields = Vec::new();
    let upper = stmt.to_uppercase();

    // Find all FIELD=(...) specifications.
    let mut search_start = 0;
    while let Some(pos) = upper[search_start..].find("FIELD=(") {
        let abs_pos = search_start + pos;
        let paren_start = abs_pos + 6; // position of '('
        if let Some(paren_end) = find_matching_paren(&stmt[paren_start..]) {
            let inner = &stmt[paren_start + 1..paren_start + paren_end];
            if let Some(field) = parse_field_inner(inner) {
                fields.push(field);
            }
            search_start = paren_start + paren_end + 1;
        } else {
            break;
        }
    }

    fields
}

fn find_matching_paren(s: &str) -> Option<usize> {
    let mut depth = 0;
    let mut in_quote = false;
    for (i, c) in s.chars().enumerate() {
        if c == '\'' {
            in_quote = !in_quote;
        } else if !in_quote {
            if c == '(' {
                depth += 1;
            } else if c == ')' {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
        }
    }
    None
}

fn parse_field_inner(inner: &str) -> Option<FieldSpec> {
    // Format: length,source,,output-pos
    // source can be: a number (input position) or 'literal'
    let parts = split_field_parts(inner);
    if parts.len() < 4 {
        return None;
    }

    let length: usize = parts[0].trim().parse().ok()?;
    let source_str = parts[1].trim();
    // parts[2] is unused (conversion)
    let output_pos: usize = parts[3].trim().parse().ok()?;

    let source = if source_str.starts_with('\'') && source_str.ends_with('\'') {
        let lit = &source_str[1..source_str.len() - 1];
        FieldSource::Literal(lit.to_string())
    } else {
        let pos: usize = source_str.parse().ok()?;
        FieldSource::InputPosition(pos)
    };

    Some(FieldSpec {
        length,
        source,
        output_pos,
    })
}

/// Split field inner content by commas, respecting quoted strings.
fn split_field_parts(s: &str) -> Vec<String> {
    let mut parts = Vec::new();
    let mut current = String::new();
    let mut in_quote = false;

    for c in s.chars() {
        if c == '\'' {
            in_quote = !in_quote;
            current.push(c);
        } else if c == ',' && !in_quote {
            parts.push(current.clone());
            current.clear();
        } else {
            current.push(c);
        }
    }
    parts.push(current);
    parts
}

fn parse_member(stmt: &str) -> Option<MemberSpec> {
    let pos = stmt.find("NAME=")?;
    let start = pos + 5;
    let rest = &stmt[start..];
    let end = rest.find([',', ' ']).unwrap_or(rest.len());
    let name = rest[..end].trim().to_string();
    if name.is_empty() {
        return None;
    }
    Some(MemberSpec { name })
}

// ─────────────────────── Field Reformatting ───────────────────────

/// Apply RECORD FIELD= reformatting to a single input record.
///
/// The output record is initialized with spaces and then populated
/// by field specifications.
fn reformat_record(input: &str, fields: &[FieldSpec], output_len: usize) -> String {
    let mut output = vec![b' '; output_len];

    for field in fields {
        let data: Vec<u8> = match &field.source {
            FieldSource::InputPosition(pos) => {
                // 1-based position.
                let start = pos.saturating_sub(1);
                let end = (start + field.length).min(input.len());
                if start < input.len() {
                    input.as_bytes()[start..end].to_vec()
                } else {
                    vec![b' '; field.length]
                }
            }
            FieldSource::Literal(lit) => {
                let mut bytes = lit.as_bytes().to_vec();
                bytes.resize(field.length, b' ');
                bytes
            }
        };

        // Place data at output position (1-based).
        let out_start = field.output_pos.saturating_sub(1);
        for (i, &byte) in data.iter().enumerate() {
            let pos = out_start + i;
            if pos < output.len() {
                output[pos] = byte;
            }
        }
    }

    String::from_utf8_lossy(&output).to_string()
}

/// Calculate the required output record length from field specs.
fn calc_output_len(fields: &[FieldSpec]) -> usize {
    fields
        .iter()
        .map(|f| f.output_pos.saturating_sub(1) + f.length)
        .max()
        .unwrap_or(80)
}

// ─────────────────────── IEBGENER Implementation ───────────────────────

/// IEBGENER — Sequential copy with optional reformatting.
pub struct Iebgener;

impl UtilityProgram for Iebgener {
    fn name(&self) -> &str {
        "IEBGENER"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let mut messages = Vec::new();

        // Read input (SYSUT1).
        let input = match context.open_input("SYSUT1") {
            Ok(data) => data,
            Err(_) => {
                let msg = UtilityMessage::severe(
                    &format_message_id("IEB", 301, MessageSeverity::Severe),
                    "SYSUT1 DD NOT ALLOCATED",
                );
                messages.push(msg);
                return UtilityResult::new(12, messages);
            }
        };

        // Parse SYSIN control statements (optional for plain copy).
        let stmts = context.read_sysin();
        let config = parse_iebgener_sysin(&stmts);

        // Validate GENERATE limits if specified.
        if let Some(ref gen) = config.generate {
            if config.fields.len() > gen.max_flds {
                let msg = UtilityMessage::error(
                    &format_message_id("IEB", 302, MessageSeverity::Error),
                    &format!(
                        "NUMBER OF FIELDS ({}) EXCEEDS MAXFLDS ({})",
                        config.fields.len(),
                        gen.max_flds
                    ),
                );
                messages.push(msg);
                return UtilityResult::new(8, messages);
            }
        }

        // Process records.
        let output_records: Vec<String> = if config.fields.is_empty() {
            // Simple copy — no reformatting.
            input.clone()
        } else {
            // Apply field reformatting.
            let out_len = calc_output_len(&config.fields);
            input
                .iter()
                .map(|rec| reformat_record(rec, &config.fields, out_len))
                .collect()
        };

        // Check if creating a PDS member.
        if let Some(ref member_spec) = config.member {
            // Output to SYSUT2 PDS as a named member.
            let out_dd = context.get_dd_mut("SYSUT2");
            let Some(out_dd) = out_dd else {
                let msg = UtilityMessage::severe(
                    &format_message_id("IEB", 303, MessageSeverity::Severe),
                    "SYSUT2 DD NOT ALLOCATED",
                );
                messages.push(msg);
                return UtilityResult::new(12, messages);
            };

            if out_dd.pds_data.is_none() {
                out_dd.pds_data = Some(PdsData::new());
            }
            let pds = out_dd.pds_data.as_mut().unwrap();
            pds.add_member(&member_spec.name, output_records.clone());

            let msg = UtilityMessage::info(
                &format_message_id("IEB", 313, MessageSeverity::Info),
                &format!(
                    "MEMBER {} CREATED, {} RECORDS",
                    member_spec.name,
                    output_records.len()
                ),
            );
            messages.push(msg);
        } else {
            // Copy to sequential output (SYSUT2).
            let out_dd = context.get_dd_mut("SYSUT2");
            let Some(out_dd) = out_dd else {
                let msg = UtilityMessage::severe(
                    &format_message_id("IEB", 303, MessageSeverity::Severe),
                    "SYSUT2 DD NOT ALLOCATED",
                );
                messages.push(msg);
                return UtilityResult::new(12, messages);
            };

            for rec in &output_records {
                out_dd.output.push(rec.clone());
            }

            let msg = UtilityMessage::info(
                &format_message_id("IEB", 310, MessageSeverity::Info),
                &format!("{} RECORDS COPIED", output_records.len()),
            );
            messages.push(msg);
        }

        // Write messages to SYSPRINT.
        for msg in &messages {
            context.write_utility_message(msg);
        }

        UtilityResult::success_with(messages)
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::DdAllocation;

    fn setup_gener_context(
        input: Vec<String>,
        sysin: Vec<String>,
    ) -> UtilityContext {
        let mut ctx = UtilityContext::new("STEP01", "IEBGENER");
        ctx.add_dd(DdAllocation::inline("SYSUT1", input));
        ctx.add_dd(DdAllocation::output("SYSUT2"));
        ctx.add_dd(DdAllocation::inline("SYSIN", sysin));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx
    }

    // ─────── Basic copy (no reformatting) ───────

    #[test]
    fn test_simple_copy() {
        let input = vec![
            "RECORD ONE".to_string(),
            "RECORD TWO".to_string(),
            "RECORD THREE".to_string(),
        ];
        let mut ctx = setup_gener_context(input.clone(), vec![]);

        let result = Iebgener.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        assert_eq!(out.output, input);
    }

    #[test]
    fn test_simple_copy_empty_sysin() {
        let input = vec!["DATA".to_string()];
        let mut ctx = setup_gener_context(input.clone(), vec![]);

        let result = Iebgener.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert_eq!(ctx.get_dd("SYSUT2").unwrap().output, input);
    }

    #[test]
    fn test_missing_sysut1_cc12() {
        let mut ctx = UtilityContext::new("STEP01", "IEBGENER");
        ctx.add_dd(DdAllocation::output("SYSUT2"));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebgener.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    #[test]
    fn test_missing_sysut2_cc12() {
        let mut ctx = UtilityContext::new("STEP01", "IEBGENER");
        ctx.add_dd(DdAllocation::inline("SYSUT1", vec!["DATA".to_string()]));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebgener.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    // ─────── UTIL-101.1: GENERATE Statement ───────

    #[test]
    fn test_generate_maxflds_maxlits() {
        let stmts = vec!["GENERATE MAXFLDS=10,MAXLITS=50".to_string()];
        let config = parse_iebgener_sysin(&stmts);
        let gen = config.generate.unwrap();
        assert_eq!(gen.max_flds, 10);
        assert_eq!(gen.max_lits, 50);
    }

    #[test]
    fn test_generate_defaults() {
        let gen = GenerateParams::default();
        assert_eq!(gen.max_flds, 1);
        assert_eq!(gen.max_lits, 0);
    }

    #[test]
    fn test_exceed_maxflds_returns_cc8() {
        // MAXFLDS=1 but 2 FIELD specs → CC=8.
        let input = vec!["ABCDEFGHIJKLMNOP".to_string()];
        let sysin = vec![
            " GENERATE MAXFLDS=1,MAXLITS=10".to_string(),
            " RECORD FIELD=(5,1,,1),FIELD=(5,6,,6)".to_string(),
        ];

        let mut ctx = setup_gener_context(input, sysin);
        let result = Iebgener.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);
    }

    // ─────── UTIL-101.2: RECORD Statement (Field Rearrangement) ───────

    #[test]
    fn test_field_rearrangement() {
        // Input: positions 1-20 and 21-30.
        // Output: input 1-20 → output 1-20, input 21-30 → output 21-30.
        //                   pos: 1---------10---------20---------30
        let input = vec!["AAAAAAAAAABBBBBBBBBBCCCCCCCCCC".to_string()];
        let sysin = vec![
            "GENERATE MAXFLDS=10,MAXLITS=50".to_string(),
            "RECORD FIELD=(20,1,,1),FIELD=(10,21,,21)".to_string(),
        ];

        let mut ctx = setup_gener_context(input, sysin);
        let result = Iebgener.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        assert_eq!(out.output.len(), 1);
        let rec = &out.output[0];
        // First 20 chars should be from positions 1-20 of input (AAAA...BBBB...).
        assert_eq!(&rec[0..20], "AAAAAAAAAABBBBBBBBBB");
        // Positions 21-30 should be from positions 21-30 of input (CCCCCCCCCC).
        assert_eq!(&rec[20..30], "CCCCCCCCCC");
    }

    #[test]
    fn test_field_with_literal() {
        let input = vec!["SOMEDATA".to_string()];
        let sysin = vec![
            "GENERATE MAXFLDS=10,MAXLITS=50".to_string(),
            "RECORD FIELD=(8,1,,1),FIELD=(5,'CONST',,11)".to_string(),
        ];

        let mut ctx = setup_gener_context(input, sysin);
        let result = Iebgener.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        let rec = &out.output[0];
        // Position 1-8: input data.
        assert_eq!(&rec[0..8], "SOMEDATA");
        // Position 11-15: literal CONST.
        assert_eq!(&rec[10..15], "CONST");
    }

    #[test]
    fn test_multiple_records_reformatted() {
        // Swap positions 1-10 and 11-20 in each record.
        let input = vec![
            "AAAAAAAAAA1111111111".to_string(),
            "BBBBBBBBBB2222222222".to_string(),
        ];
        let sysin = vec![
            "GENERATE MAXFLDS=2".to_string(),
            "RECORD FIELD=(10,11,,1),FIELD=(10,1,,11)".to_string(),
        ];

        let mut ctx = setup_gener_context(input, sysin);
        let result = Iebgener.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        assert_eq!(out.output.len(), 2);
        // Records should have fields swapped.
        assert_eq!(&out.output[0][0..10], "1111111111");
        assert_eq!(&out.output[0][10..20], "AAAAAAAAAA");
        assert_eq!(&out.output[1][0..10], "2222222222");
        assert_eq!(&out.output[1][10..20], "BBBBBBBBBB");
    }

    #[test]
    fn test_three_field_reformatting() {
        // AC: Given reformatting with 3 FIELD specifications.
        // Input positions:  1234567890123456789...
        //                   ABCDEFGHIJKLMNOPQRS...
        let input = vec!["ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789".to_string()];
        let sysin = vec![
            " GENERATE MAXFLDS=10,MAXLITS=50".to_string(),
            " RECORD FIELD=(5,1,,1),FIELD=(5,11,,6),FIELD=(3,'***',,11)".to_string(),
        ];

        let mut ctx = setup_gener_context(input, sysin);
        let result = Iebgener.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        let rec = &out.output[0];
        assert_eq!(&rec[0..5], "ABCDE");   // Field 1: input pos 1-5
        assert_eq!(&rec[5..10], "KLMNO");  // Field 2: input pos 11-15
        assert_eq!(&rec[10..13], "***");    // Field 3: literal
    }

    // ─────── UTIL-101.3: MEMBER Statement (PDS Member Creation) ───────

    #[test]
    fn test_member_creation_from_sequential() {
        let input = vec![
            "       IDENTIFICATION DIVISION.".to_string(),
            "       PROGRAM-ID. MEMA.".to_string(),
        ];
        let sysin = vec!["MEMBER NAME=MEMA".to_string()];

        let mut ctx = UtilityContext::new("STEP01", "IEBGENER");
        ctx.add_dd(DdAllocation::inline("SYSUT1", input.clone()));
        ctx.add_dd(DdAllocation::pds(
            "SYSUT2",
            "MY.PDS",
            "OLD",
            PdsData::new(),
        ));
        ctx.add_dd(DdAllocation::inline("SYSIN", sysin));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebgener.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        let pds = out.pds_data.as_ref().unwrap();
        assert!(pds.has_member("MEMA"));
        let member = pds.get_member("MEMA").unwrap();
        assert_eq!(member.content.len(), 2);
        assert!(member.content[0].contains("IDENTIFICATION"));
    }

    #[test]
    fn test_member_creation_initializes_pds() {
        // SYSUT2 doesn't have PDS data yet — should be created.
        let input = vec!["DATA".to_string()];
        let sysin = vec!["MEMBER NAME=NEWMOD".to_string()];

        let mut ctx = UtilityContext::new("STEP01", "IEBGENER");
        ctx.add_dd(DdAllocation::inline("SYSUT1", input));
        ctx.add_dd(DdAllocation::output("SYSUT2"));
        ctx.add_dd(DdAllocation::inline("SYSIN", sysin));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebgener.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        let pds = out.pds_data.as_ref().unwrap();
        assert!(pds.has_member("NEWMOD"));
    }

    #[test]
    fn test_member_creation_with_reformatting() {
        let input = vec!["ABCDEFGHIJ".to_string()];
        let sysin = vec![
            "GENERATE MAXFLDS=10".to_string(),
            "RECORD FIELD=(5,1,,1),FIELD=(5,6,,6)".to_string(),
            "MEMBER NAME=REFORMD".to_string(),
        ];

        let mut ctx = UtilityContext::new("STEP01", "IEBGENER");
        ctx.add_dd(DdAllocation::inline("SYSUT1", input));
        ctx.add_dd(DdAllocation::pds(
            "SYSUT2",
            "MY.PDS",
            "OLD",
            PdsData::new(),
        ));
        ctx.add_dd(DdAllocation::inline("SYSIN", sysin));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = Iebgener.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let pds = ctx.get_dd("SYSUT2").unwrap().pds_data.as_ref().unwrap();
        let member = pds.get_member("REFORMD").unwrap();
        assert_eq!(&member.content[0][0..5], "ABCDE");
        assert_eq!(&member.content[0][5..10], "FGHIJ");
    }

    // ─────── UTIL-101.4: IEBGENER Enhancement Tests ───────

    #[test]
    fn test_via_registry() {
        use crate::UtilityRegistry;

        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(Iebgener));

        let mut ctx = setup_gener_context(
            vec!["DATA LINE".to_string()],
            vec![],
        );

        let result = reg.dispatch("IEBGENER", &mut ctx).unwrap();
        assert_eq!(result.condition_code, 0);
    }

    #[test]
    fn test_empty_input_copies_nothing() {
        let mut ctx = setup_gener_context(vec![], vec![]);
        let result = Iebgener.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(ctx.get_dd("SYSUT2").unwrap().output.is_empty());
    }

    #[test]
    fn test_sysprint_messages() {
        let mut ctx = setup_gener_context(
            vec!["DATA".to_string()],
            vec![],
        );
        Iebgener.execute(&mut ctx);

        let output = ctx.sysprint_output();
        assert!(!output.is_empty());
        assert!(output[0].contains("IEB0310I"));
    }

    // ─── Control statement parsing tests ───

    #[test]
    fn test_parse_generate() {
        let config = parse_iebgener_sysin(&["GENERATE MAXFLDS=5,MAXLITS=20".to_string()]);
        let gen = config.generate.unwrap();
        assert_eq!(gen.max_flds, 5);
        assert_eq!(gen.max_lits, 20);
    }

    #[test]
    fn test_parse_record_single_field() {
        let config = parse_iebgener_sysin(&["RECORD FIELD=(20,1,,1)".to_string()]);
        assert_eq!(config.fields.len(), 1);
        assert_eq!(config.fields[0].length, 20);
        assert_eq!(config.fields[0].output_pos, 1);
        assert!(matches!(config.fields[0].source, FieldSource::InputPosition(1)));
    }

    #[test]
    fn test_parse_record_multiple_fields() {
        let config = parse_iebgener_sysin(&[
            "RECORD FIELD=(20,1,,1),FIELD=(10,30,,21)".to_string()
        ]);
        assert_eq!(config.fields.len(), 2);
    }

    #[test]
    fn test_parse_record_with_literal() {
        let config = parse_iebgener_sysin(&[
            "RECORD FIELD=(5,'CONST',,41)".to_string()
        ]);
        assert_eq!(config.fields.len(), 1);
        if let FieldSource::Literal(ref lit) = config.fields[0].source {
            assert_eq!(lit, "CONST");
        } else {
            panic!("Expected Literal source");
        }
    }

    #[test]
    fn test_parse_member() {
        let config = parse_iebgener_sysin(&["MEMBER NAME=MEMA".to_string()]);
        assert_eq!(config.member.unwrap().name, "MEMA");
    }

    #[test]
    fn test_parse_comments_and_blanks() {
        let config = parse_iebgener_sysin(&[
            "/* COMMENT".to_string(),
            "".to_string(),
            "MEMBER NAME=TEST".to_string(),
        ]);
        assert!(config.member.is_some());
    }

    #[test]
    fn test_reformat_preserves_spaces() {
        // Output record should be padded with spaces.
        let fields = vec![FieldSpec {
            length: 3,
            source: FieldSource::InputPosition(1),
            output_pos: 1,
        }];
        let result = reformat_record("ABC", &fields, 10);
        assert_eq!(result.len(), 10);
        assert_eq!(&result[0..3], "ABC");
        assert_eq!(&result[3..], "       ");
    }
}
