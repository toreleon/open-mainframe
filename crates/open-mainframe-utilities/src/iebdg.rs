//! IEBDG — Test Data Generator utility.
//!
//! Generates test data based on field definitions and pattern specifications.
//! IEBDG creates records with controlled content for testing purposes.
//!
//! ## Control Statement Syntax
//!
//! ```text
//! DSD OUTPUT=(ddname)
//! FD NAME=field,LENGTH=n,STARTLOC=n,FORMAT=AN|ZD|BI,ACTION=WAVE|ROLL|RANDOM|FX
//! CREATE QUANTITY=n,NAME=(field1,field2,...),FILL=char
//! END
//! ```
//!
//! ## Condition Codes
//!
//! | CC | Meaning |
//! |----|---------|
//! | 0  | All records generated successfully |
//! | 4  | Warning — zero quantity or no fields |
//! | 8  | Error — missing DD or invalid parameters |
//! | 12 | Severe — syntax error |

use crate::{
    format_message_id, MessageSeverity, UtilityContext, UtilityMessage, UtilityProgram,
    UtilityResult,
};

// ─────────────────────── Control Statement Parsing ───────────────────────

/// Field format for IEBDG.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldFormat {
    /// AN — alphanumeric.
    Alphanumeric,
    /// ZD — zoned decimal.
    ZonedDecimal,
    /// BI — binary.
    Binary,
    /// PD — packed decimal.
    PackedDecimal,
}

impl Default for FieldFormat {
    fn default() -> Self {
        Self::Alphanumeric
    }
}

/// Pattern action for generating field data.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatternAction {
    /// WAVE — cycle through a pattern string, resetting each record.
    Wave,
    /// ROLL — cycle through a pattern string, continuing across records.
    Roll,
    /// RANDOM — generate pseudo-random content.
    Random,
    /// FX — fixed value (same in every record).
    Fixed,
}

impl Default for PatternAction {
    fn default() -> Self {
        Self::Wave
    }
}

/// A field definition (FD statement).
#[derive(Debug, Clone)]
pub struct FieldDef {
    /// Field name.
    pub name: String,
    /// Length in bytes.
    pub length: usize,
    /// Start location in record (1-based).
    pub start_loc: usize,
    /// Field format.
    pub format: FieldFormat,
    /// Pattern action.
    pub action: PatternAction,
    /// Pattern characters (for WAVE/ROLL/FX).
    pub pattern: Option<Vec<u8>>,
}

impl FieldDef {
    /// Create a default field definition.
    pub fn new(name: &str, length: usize, start_loc: usize) -> Self {
        Self {
            name: name.to_uppercase(),
            length,
            start_loc,
            format: FieldFormat::default(),
            action: PatternAction::default(),
            pattern: None,
        }
    }
}

/// A CREATE statement — defines how many records to generate.
#[derive(Debug, Clone)]
pub struct CreateSpec {
    /// Number of records to generate.
    pub quantity: usize,
    /// Field names to include.
    pub field_names: Vec<String>,
    /// Fill character for unused positions.
    pub fill: u8,
}

/// Parsed IEBDG configuration.
#[derive(Debug, Clone)]
pub struct IebdgConfig {
    /// Output DDname (from DSD statement).
    pub output_dd: String,
    /// Field definitions.
    pub fields: Vec<FieldDef>,
    /// CREATE specifications.
    pub creates: Vec<CreateSpec>,
}

/// Parse IEBDG SYSIN control statements into configuration.
pub fn parse_iebdg_sysin(statements: &[String]) -> Option<IebdgConfig> {
    let mut output_dd = String::new();
    let mut fields = Vec::new();
    let mut creates = Vec::new();

    for stmt in statements {
        let trimmed = stmt.trim().to_uppercase();
        if trimmed.is_empty() || trimmed.starts_with('*') {
            continue;
        }

        if trimmed.starts_with("DSD ") || trimmed == "DSD" {
            if let Some(out) = extract_param(&trimmed, "OUTPUT=(") {
                let out = out.trim_end_matches(')');
                output_dd = out.to_string();
            } else if let Some(out) = extract_param(&trimmed, "OUTPUT=") {
                output_dd = out.to_string();
            }
        } else if trimmed.starts_with("FD ") || trimmed == "FD" {
            if let Some(fd) = parse_fd(&trimmed) {
                fields.push(fd);
            }
        } else if trimmed.starts_with("CREATE ") || trimmed == "CREATE" {
            creates.push(parse_create(&trimmed, &fields));
        } else if trimmed.starts_with("END") {
            break;
        }
    }

    if output_dd.is_empty() && !creates.is_empty() {
        // Default to SYSUT2 if no DSD specified.
        output_dd = "SYSUT2".to_string();
    }

    if output_dd.is_empty() {
        return None;
    }

    Some(IebdgConfig {
        output_dd,
        fields,
        creates,
    })
}

fn parse_fd(stmt: &str) -> Option<FieldDef> {
    let name = extract_param(stmt, "NAME=")?;
    let length = extract_param(stmt, "LENGTH=")
        .and_then(|s| s.parse().ok())
        .unwrap_or(1);
    let start_loc = extract_param(stmt, "STARTLOC=")
        .and_then(|s| s.parse().ok())
        .unwrap_or(1);

    let format = match extract_param(stmt, "FORMAT=").as_deref() {
        Some("ZD") => FieldFormat::ZonedDecimal,
        Some("BI") => FieldFormat::Binary,
        Some("PD") => FieldFormat::PackedDecimal,
        _ => FieldFormat::Alphanumeric,
    };

    let action = match extract_param(stmt, "ACTION=").as_deref() {
        Some("WAVE") => PatternAction::Wave,
        Some("ROLL") => PatternAction::Roll,
        Some("RANDOM") => PatternAction::Random,
        Some("FX") | Some("FIXED") => PatternAction::Fixed,
        _ => PatternAction::Wave,
    };

    let pattern = extract_param(stmt, "PICTURE=")
        .or_else(|| extract_param(stmt, "FROMLOC="))
        .map(|s| {
            // Remove enclosing quotes if present.
            let s = s.trim_matches('\'').trim_matches('"');
            s.as_bytes().to_vec()
        });

    Some(FieldDef {
        name,
        length,
        start_loc,
        format,
        action,
        pattern,
    })
}

fn parse_create(stmt: &str, all_fields: &[FieldDef]) -> CreateSpec {
    let quantity = extract_param(stmt, "QUANTITY=")
        .and_then(|s| s.parse().ok())
        .unwrap_or(1);

    let field_names = if let Some(paren_pos) = stmt.find("NAME=(") {
        // Extract everything between ( and )
        let rest = &stmt[paren_pos + 6..];
        let end = rest.find(')').unwrap_or(rest.len());
        rest[..end]
            .split(',')
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .collect()
    } else if let Some(name) = extract_param(stmt, "NAME=") {
        vec![name]
    } else {
        // Use all defined fields by default.
        all_fields.iter().map(|f| f.name.clone()).collect()
    };

    let fill = extract_param(stmt, "FILL=")
        .and_then(|s| {
            let s = s.trim_matches('\'').trim_matches('"');
            s.bytes().next()
        })
        .unwrap_or(b' ');

    CreateSpec {
        quantity,
        field_names,
        fill,
    }
}

fn extract_param(stmt: &str, key: &str) -> Option<String> {
    let pos = stmt.find(key)?;
    let start = pos + key.len();
    let rest = &stmt[start..];
    let end = rest.find([',', ' ']).unwrap_or(rest.len());
    // For parenthesized values, find the closing paren.
    let end = if rest.starts_with('(') {
        rest.find(')').map_or(end, |p| p + 1)
    } else {
        end
    };
    let val = rest[..end].trim_matches(|c| c == '(' || c == ')').trim().to_string();
    if val.is_empty() { None } else { Some(val) }
}

// ─────────────────────── IEBDG Implementation ───────────────────────

/// IEBDG — test data generator utility.
pub struct Iebdg;

impl UtilityProgram for Iebdg {
    fn name(&self) -> &str {
        "IEBDG"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let stmts = context.read_sysin();
        if stmts.is_empty() {
            let msg = UtilityMessage::severe(
                &format_message_id("IEB", 601, MessageSeverity::Severe),
                "NO CONTROL STATEMENTS IN SYSIN",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(12, vec![msg]);
        }

        let config = match parse_iebdg_sysin(&stmts) {
            Some(c) => c,
            None => {
                let msg = UtilityMessage::severe(
                    &format_message_id("IEB", 602, MessageSeverity::Severe),
                    "NO DSD/CREATE STATEMENTS FOUND",
                );
                context.write_utility_message(&msg);
                return UtilityResult::new(12, vec![msg]);
            }
        };

        if !context.has_dd(&config.output_dd) {
            let msg = UtilityMessage::error(
                &format_message_id("IEB", 603, MessageSeverity::Error),
                &format!("{} DD NOT ALLOCATED", config.output_dd),
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(8, vec![msg]);
        }

        let mut messages = Vec::new();
        let mut total_records = 0;
        let mut roll_positions: Vec<usize> = vec![0; config.fields.len()];
        let mut rng_state: u64 = 12345; // Simple deterministic seed.

        for create in &config.creates {
            let active_fields: Vec<&FieldDef> = create
                .field_names
                .iter()
                .filter_map(|name| config.fields.iter().find(|f| f.name == *name))
                .collect();

            if active_fields.is_empty() {
                let msg = UtilityMessage::warning(
                    &format_message_id("IEB", 610, MessageSeverity::Warning),
                    "NO MATCHING FIELD DEFINITIONS FOR CREATE",
                );
                messages.push(msg);
                continue;
            }

            // Determine record length from field positions.
            let record_len = active_fields
                .iter()
                .map(|f| f.start_loc - 1 + f.length)
                .max()
                .unwrap_or(80);

            for rec_idx in 0..create.quantity {
                let mut record = vec![create.fill; record_len];

                for field in &active_fields {
                    let start = field.start_loc.saturating_sub(1);
                    let end = (start + field.length).min(record.len());
                    let field_len = end - start;

                    let data = generate_field_data(
                        field,
                        field_len,
                        rec_idx,
                        &mut roll_positions,
                        &config.fields,
                        &mut rng_state,
                    );

                    let copy_len = data.len().min(field_len);
                    record[start..start + copy_len].copy_from_slice(&data[..copy_len]);
                }

                let line = String::from_utf8_lossy(&record).to_string();
                if let Some(out_dd) = context.get_dd_mut(&config.output_dd) {
                    out_dd.output.push(line);
                }
                total_records += 1;
            }
        }

        let msg = UtilityMessage::info(
            &format_message_id("IEB", 611, MessageSeverity::Info),
            &format!("IEBDG COMPLETE — {total_records} RECORDS GENERATED"),
        );
        messages.push(msg);

        for m in &messages {
            context.write_utility_message(m);
        }

        let cc = if messages
            .iter()
            .any(|m| m.severity == MessageSeverity::Warning)
        {
            4
        } else {
            0
        };

        UtilityResult::new(cc, messages)
    }
}

fn generate_field_data(
    field: &FieldDef,
    length: usize,
    record_index: usize,
    roll_positions: &mut [usize],
    all_fields: &[FieldDef],
    rng_state: &mut u64,
) -> Vec<u8> {
    let pattern = field.pattern.as_deref().unwrap_or(b"ABCDEFGHIJ");

    match field.action {
        PatternAction::Wave => {
            // Cycle through pattern, reset each record.
            let mut data = Vec::with_capacity(length);
            for i in 0..length {
                data.push(pattern[i % pattern.len()]);
            }
            data
        }
        PatternAction::Roll => {
            // Continue cycling across records.
            let field_idx = all_fields
                .iter()
                .position(|f| f.name == field.name)
                .unwrap_or(0);
            let offset = roll_positions.get(field_idx).copied().unwrap_or(0);
            let mut data = Vec::with_capacity(length);
            for i in 0..length {
                data.push(pattern[(offset + i) % pattern.len()]);
            }
            if let Some(pos) = roll_positions.get_mut(field_idx) {
                *pos = (offset + length) % pattern.len();
            }
            data
        }
        PatternAction::Random => {
            // Simple LCG pseudo-random.
            let mut data = Vec::with_capacity(length);
            for _ in 0..length {
                *rng_state = rng_state.wrapping_mul(6364136223846793005).wrapping_add(1);
                match field.format {
                    FieldFormat::Alphanumeric => {
                        let ch = b'A' + ((*rng_state >> 32) as u8 % 26);
                        data.push(ch);
                    }
                    FieldFormat::ZonedDecimal => {
                        let ch = b'0' + ((*rng_state >> 32) as u8 % 10);
                        data.push(ch);
                    }
                    _ => {
                        data.push((*rng_state >> 32) as u8);
                    }
                }
            }
            data
        }
        PatternAction::Fixed => {
            // Same value every record.
            let _ = record_index;
            let mut data = Vec::with_capacity(length);
            for i in 0..length {
                data.push(pattern[i % pattern.len()]);
            }
            data
        }
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{DdAllocation, UtilityContext};

    fn setup_iebdg_context(sysin: Vec<String>) -> UtilityContext {
        let mut ctx = UtilityContext::new("STEP01", "IEBDG");
        ctx.add_dd(DdAllocation::output("SYSUT2"));
        ctx.add_dd(DdAllocation::inline("SYSIN", sysin));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx
    }

    // ─────── UTIL-106.1: DSD/FD/CREATE Statements ───────

    #[test]
    fn test_basic_generation() {
        let sysin = vec![
            "DSD OUTPUT=(SYSUT2)".to_string(),
            "FD NAME=FLD1,LENGTH=5,STARTLOC=1".to_string(),
            "CREATE QUANTITY=3,NAME=(FLD1)".to_string(),
            "END".to_string(),
        ];
        let mut ctx = setup_iebdg_context(sysin);
        let result = Iebdg.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        assert_eq!(out.output.len(), 3);
        assert_eq!(out.output[0].len(), 5);
    }

    #[test]
    fn test_multiple_fields() {
        let sysin = vec![
            "DSD OUTPUT=(SYSUT2)".to_string(),
            "FD NAME=FLD1,LENGTH=3,STARTLOC=1,PICTURE='ABC'".to_string(),
            "FD NAME=FLD2,LENGTH=3,STARTLOC=4,PICTURE='XYZ'".to_string(),
            "CREATE QUANTITY=2,NAME=(FLD1,FLD2)".to_string(),
            "END".to_string(),
        ];
        let mut ctx = setup_iebdg_context(sysin);
        let result = Iebdg.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);

        let out = ctx.get_dd("SYSUT2").unwrap();
        assert_eq!(out.output.len(), 2);
        assert_eq!(&out.output[0][0..3], "ABC");
        assert_eq!(&out.output[0][3..6], "XYZ");
    }

    #[test]
    fn test_fill_character() {
        let sysin = vec![
            "DSD OUTPUT=(SYSUT2)".to_string(),
            "FD NAME=FLD1,LENGTH=3,STARTLOC=1,PICTURE='AB'".to_string(),
            "CREATE QUANTITY=1,NAME=(FLD1),FILL='*'".to_string(),
            "END".to_string(),
        ];
        let mut ctx = setup_iebdg_context(sysin);
        Iebdg.execute(&mut ctx);

        let out = ctx.get_dd("SYSUT2").unwrap();
        // Field is 3 bytes, pattern is "AB" → "ABA" (wave), no fill in field
        // But record length = 3, so no fill visible here.
        assert_eq!(out.output[0].len(), 3);
    }

    #[test]
    fn test_default_output_dd() {
        let sysin = vec![
            "FD NAME=F1,LENGTH=5,STARTLOC=1".to_string(),
            "CREATE QUANTITY=1,NAME=(F1)".to_string(),
            "END".to_string(),
        ];
        let mut ctx = setup_iebdg_context(sysin);
        let result = Iebdg.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
    }

    // ─────── UTIL-106.2: Pattern Generation (WAVE, ROLL, RANDOM) ───────

    #[test]
    fn test_wave_pattern() {
        let sysin = vec![
            "DSD OUTPUT=(SYSUT2)".to_string(),
            "FD NAME=FLD1,LENGTH=6,STARTLOC=1,ACTION=WAVE,PICTURE='AB'".to_string(),
            "CREATE QUANTITY=2,NAME=(FLD1)".to_string(),
            "END".to_string(),
        ];
        let mut ctx = setup_iebdg_context(sysin);
        Iebdg.execute(&mut ctx);

        let out = ctx.get_dd("SYSUT2").unwrap();
        // WAVE: "ABABAB" each record
        assert_eq!(out.output[0], "ABABAB");
        assert_eq!(out.output[1], "ABABAB");
    }

    #[test]
    fn test_roll_pattern() {
        let sysin = vec![
            "DSD OUTPUT=(SYSUT2)".to_string(),
            "FD NAME=FLD1,LENGTH=3,STARTLOC=1,ACTION=ROLL,PICTURE='ABCDE'".to_string(),
            "CREATE QUANTITY=3,NAME=(FLD1)".to_string(),
            "END".to_string(),
        ];
        let mut ctx = setup_iebdg_context(sysin);
        Iebdg.execute(&mut ctx);

        let out = ctx.get_dd("SYSUT2").unwrap();
        // ROLL: continues across records
        // Record 1: ABC (offset 0→3)
        // Record 2: DEA (offset 3→6, wrapping)
        // Record 3: BCD (offset 1→4)
        assert_eq!(out.output[0], "ABC");
        assert_eq!(out.output[1], "DEA");
        assert_eq!(out.output[2], "BCD");
    }

    #[test]
    fn test_fixed_pattern() {
        let sysin = vec![
            "DSD OUTPUT=(SYSUT2)".to_string(),
            "FD NAME=FLD1,LENGTH=4,STARTLOC=1,ACTION=FX,PICTURE='TEST'".to_string(),
            "CREATE QUANTITY=3,NAME=(FLD1)".to_string(),
            "END".to_string(),
        ];
        let mut ctx = setup_iebdg_context(sysin);
        Iebdg.execute(&mut ctx);

        let out = ctx.get_dd("SYSUT2").unwrap();
        assert_eq!(out.output[0], "TEST");
        assert_eq!(out.output[1], "TEST");
        assert_eq!(out.output[2], "TEST");
    }

    #[test]
    fn test_random_pattern_alphanumeric() {
        let sysin = vec![
            "DSD OUTPUT=(SYSUT2)".to_string(),
            "FD NAME=FLD1,LENGTH=10,STARTLOC=1,FORMAT=AN,ACTION=RANDOM".to_string(),
            "CREATE QUANTITY=3,NAME=(FLD1)".to_string(),
            "END".to_string(),
        ];
        let mut ctx = setup_iebdg_context(sysin);
        Iebdg.execute(&mut ctx);

        let out = ctx.get_dd("SYSUT2").unwrap();
        assert_eq!(out.output.len(), 3);
        // All chars should be A-Z
        for record in &out.output {
            assert!(record.chars().all(|c| c.is_ascii_uppercase()));
        }
    }

    #[test]
    fn test_random_pattern_zoned_decimal() {
        let sysin = vec![
            "DSD OUTPUT=(SYSUT2)".to_string(),
            "FD NAME=FLD1,LENGTH=8,STARTLOC=1,FORMAT=ZD,ACTION=RANDOM".to_string(),
            "CREATE QUANTITY=3,NAME=(FLD1)".to_string(),
            "END".to_string(),
        ];
        let mut ctx = setup_iebdg_context(sysin);
        Iebdg.execute(&mut ctx);

        let out = ctx.get_dd("SYSUT2").unwrap();
        assert_eq!(out.output.len(), 3);
        // All chars should be digits
        for record in &out.output {
            assert!(record.chars().all(|c| c.is_ascii_digit()));
        }
    }

    // ─────── UTIL-106.3: Field Types and Lengths ───────

    #[test]
    fn test_varying_field_lengths() {
        let sysin = vec![
            "DSD OUTPUT=(SYSUT2)".to_string(),
            "FD NAME=SHORT,LENGTH=2,STARTLOC=1,PICTURE='AB'".to_string(),
            "FD NAME=LONG,LENGTH=10,STARTLOC=3,PICTURE='1234567890'".to_string(),
            "CREATE QUANTITY=1,NAME=(SHORT,LONG)".to_string(),
            "END".to_string(),
        ];
        let mut ctx = setup_iebdg_context(sysin);
        Iebdg.execute(&mut ctx);

        let out = ctx.get_dd("SYSUT2").unwrap();
        assert_eq!(out.output[0].len(), 12); // 2 + 10
        assert_eq!(&out.output[0][0..2], "AB");
        assert_eq!(&out.output[0][2..12], "1234567890");
    }

    #[test]
    fn test_multiple_creates() {
        let sysin = vec![
            "DSD OUTPUT=(SYSUT2)".to_string(),
            "FD NAME=F1,LENGTH=3,STARTLOC=1,PICTURE='AAA'".to_string(),
            "FD NAME=F2,LENGTH=3,STARTLOC=1,PICTURE='BBB'".to_string(),
            "CREATE QUANTITY=2,NAME=(F1)".to_string(),
            "CREATE QUANTITY=3,NAME=(F2)".to_string(),
            "END".to_string(),
        ];
        let mut ctx = setup_iebdg_context(sysin);
        Iebdg.execute(&mut ctx);

        let out = ctx.get_dd("SYSUT2").unwrap();
        assert_eq!(out.output.len(), 5); // 2 + 3
        assert_eq!(out.output[0], "AAA");
        assert_eq!(out.output[2], "BBB");
    }

    // ─────── UTIL-106.4: IEBDG Tests ───────

    #[test]
    fn test_no_sysin_cc12() {
        let mut ctx = UtilityContext::new("STEP01", "IEBDG");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        let result = Iebdg.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    #[test]
    fn test_invalid_sysin_cc12() {
        let sysin = vec!["GARBAGE".to_string()];
        let mut ctx = setup_iebdg_context(sysin);
        let result = Iebdg.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    #[test]
    fn test_missing_output_dd_cc8() {
        let sysin = vec![
            "DSD OUTPUT=(OUTDD)".to_string(),
            "FD NAME=F1,LENGTH=5,STARTLOC=1".to_string(),
            "CREATE QUANTITY=1,NAME=(F1)".to_string(),
            "END".to_string(),
        ];
        // No OUTDD allocated.
        let mut ctx = UtilityContext::new("STEP01", "IEBDG");
        ctx.add_dd(DdAllocation::inline("SYSIN", sysin));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        let result = Iebdg.execute(&mut ctx);
        assert_eq!(result.condition_code, 8);
    }

    #[test]
    fn test_parse_fd() {
        let stmts = vec![
            "DSD OUTPUT=(SYSUT2)".to_string(),
            "FD NAME=FIELD1,LENGTH=10,STARTLOC=5,FORMAT=ZD,ACTION=ROLL".to_string(),
            "CREATE QUANTITY=1,NAME=(FIELD1)".to_string(),
            "END".to_string(),
        ];
        let config = parse_iebdg_sysin(&stmts).unwrap();
        assert_eq!(config.output_dd, "SYSUT2");
        assert_eq!(config.fields.len(), 1);
        assert_eq!(config.fields[0].name, "FIELD1");
        assert_eq!(config.fields[0].length, 10);
        assert_eq!(config.fields[0].start_loc, 5);
        assert_eq!(config.fields[0].format, FieldFormat::ZonedDecimal);
        assert_eq!(config.fields[0].action, PatternAction::Roll);
    }

    #[test]
    fn test_sysprint_messages() {
        let sysin = vec![
            "DSD OUTPUT=(SYSUT2)".to_string(),
            "FD NAME=F1,LENGTH=5,STARTLOC=1".to_string(),
            "CREATE QUANTITY=2,NAME=(F1)".to_string(),
            "END".to_string(),
        ];
        let mut ctx = setup_iebdg_context(sysin);
        Iebdg.execute(&mut ctx);

        let output = ctx.sysprint_output();
        assert!(!output.is_empty());
        assert!(output.iter().any(|l| l.contains("2 RECORDS GENERATED")));
    }

    #[test]
    fn test_via_registry() {
        use crate::UtilityRegistry;
        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(Iebdg));

        let sysin = vec![
            "DSD OUTPUT=(SYSUT2)".to_string(),
            "FD NAME=F1,LENGTH=3,STARTLOC=1".to_string(),
            "CREATE QUANTITY=1,NAME=(F1)".to_string(),
            "END".to_string(),
        ];
        let mut ctx = setup_iebdg_context(sysin);

        let result = reg.dispatch("IEBDG", &mut ctx).unwrap();
        assert_eq!(result.condition_code, 0);
    }

    #[test]
    fn test_end_stops_processing() {
        let sysin = vec![
            "DSD OUTPUT=(SYSUT2)".to_string(),
            "FD NAME=F1,LENGTH=3,STARTLOC=1".to_string(),
            "CREATE QUANTITY=2,NAME=(F1)".to_string(),
            "END".to_string(),
            "CREATE QUANTITY=10,NAME=(F1)".to_string(), // Should be ignored.
        ];
        let mut ctx = setup_iebdg_context(sysin);
        Iebdg.execute(&mut ctx);

        let out = ctx.get_dd("SYSUT2").unwrap();
        assert_eq!(out.output.len(), 2); // Only first CREATE executed.
    }
}
