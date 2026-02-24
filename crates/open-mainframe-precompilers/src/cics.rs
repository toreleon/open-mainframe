//! # CICS COBOL Precompiler
//!
//! Source-to-source transformer for COBOL programs with embedded EXEC CICS.
//!
//! Transforms `EXEC CICS ... END-EXEC` blocks into `CALL 'DFHEI1'` statements,
//! generates DFHEIBLK and DFHCOMMAREA, and supports BMS map references.
//!
//! Uses the scanner from `open-mainframe-cics::preprocess` for block extraction,
//! then applies precompiler-specific transformation logic.

use std::collections::HashMap;

// Re-export upstream preprocessing API for consumers who need the full CICS preprocessor.
pub use open_mainframe_cics::preprocess::{
    CicsPreprocessor, CicsScanner, CicsBlock,
    CicsCommandType as UpstreamCicsCommandType,
    CicsOption as UpstreamCicsOption,
    CicsCommand as UpstreamCicsCommand,
    PreprocessResult as UpstreamPreprocessResult,
};

// ─────────────────────── Errors ───────────────────────

/// CICS precompiler errors.
#[derive(Debug, thiserror::Error)]
pub enum CicsPrecompileError {
    /// An EXEC CICS block was not terminated.
    #[error("unterminated EXEC CICS block at line {line}")]
    UnterminatedExecCics { line: usize },

    /// Unknown CICS command.
    #[error("unrecognized CICS command at line {line}: {command}")]
    UnrecognizedCommand { line: usize, command: String },

    /// Upstream CICS error.
    #[error("CICS scanner error: {0}")]
    CicsError(#[from] open_mainframe_cics::CicsError),
}

// ─────────────────────── EXEC CICS Parser ───────────────────────

/// A CICS command option.
#[derive(Debug, Clone)]
pub struct CicsOption {
    /// Option name (e.g., "MAP", "MAPSET", "ERASE").
    pub name: String,
    /// Option value, if any (e.g., "'MENU01'").
    pub value: Option<String>,
}

/// A parsed EXEC CICS block.
#[derive(Debug, Clone)]
pub struct ExecCicsBlock {
    /// Source line number.
    pub line: usize,
    /// CICS command name (e.g., "SEND MAP", "READ FILE").
    pub command: String,
    /// Command options.
    pub options: Vec<CicsOption>,
    /// Raw text.
    pub raw_text: String,
}

/// Recognized CICS commands.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CicsCommand {
    SendMap,
    SendText,
    ReceiveMap,
    ReadFile,
    WriteFile,
    RewriteFile,
    DeleteFile,
    StartBrowse,
    ReadNext,
    EndBrowse,
    Link,
    Xctl,
    Return,
    Abend,
    Assign,
    Address,
    Handle,
    Ignore,
    Syncpoint,
    Start,
    Cancel,
    Retrieve,
    Enq,
    Deq,
    Asktime,
    Formattime,
    WriteQ,
    ReadQ,
    DeleteQ,
    GetMain,
    FreeMain,
    Other(String),
}

impl CicsCommand {
    fn from_str(s: &str) -> Self {
        let upper = s.trim().to_uppercase();
        match upper.as_str() {
            "SEND MAP" => Self::SendMap,
            "SEND TEXT" | "SEND" => Self::SendText,
            "RECEIVE MAP" | "RECEIVE" => Self::ReceiveMap,
            "READ" | "READ FILE" => Self::ReadFile,
            "WRITE" | "WRITE FILE" => Self::WriteFile,
            "REWRITE" | "REWRITE FILE" => Self::RewriteFile,
            "DELETE" | "DELETE FILE" => Self::DeleteFile,
            "STARTBR" | "STARTBROWSE" => Self::StartBrowse,
            "READNEXT" => Self::ReadNext,
            "ENDBR" | "ENDBROWSE" => Self::EndBrowse,
            "LINK" => Self::Link,
            "XCTL" => Self::Xctl,
            "RETURN" => Self::Return,
            "ABEND" => Self::Abend,
            "ASSIGN" => Self::Assign,
            "ADDRESS" => Self::Address,
            "HANDLE" | "HANDLE CONDITION" | "HANDLE AID" => Self::Handle,
            "IGNORE" | "IGNORE CONDITION" => Self::Ignore,
            "SYNCPOINT" => Self::Syncpoint,
            "START" => Self::Start,
            "CANCEL" => Self::Cancel,
            "RETRIEVE" => Self::Retrieve,
            "ENQ" => Self::Enq,
            "DEQ" => Self::Deq,
            "ASKTIME" => Self::Asktime,
            "FORMATTIME" => Self::Formattime,
            "WRITEQ TD" | "WRITEQ TS" | "WRITEQ" => Self::WriteQ,
            "READQ TD" | "READQ TS" | "READQ" => Self::ReadQ,
            "DELETEQ TD" | "DELETEQ TS" | "DELETEQ" => Self::DeleteQ,
            "GETMAIN" => Self::GetMain,
            "FREEMAIN" => Self::FreeMain,
            _ => Self::Other(upper),
        }
    }

    /// Get the CICS command code for DFHEI1 CALL.
    pub fn command_code(&self) -> u16 {
        match self {
            Self::SendMap => 0x0804,
            Self::SendText => 0x0800,
            Self::ReceiveMap => 0x0A04,
            Self::ReadFile => 0x0602,
            Self::WriteFile => 0x0604,
            Self::RewriteFile => 0x0606,
            Self::DeleteFile => 0x0608,
            Self::StartBrowse => 0x060A,
            Self::ReadNext => 0x060C,
            Self::EndBrowse => 0x060E,
            Self::Link => 0x0E02,
            Self::Xctl => 0x0E04,
            Self::Return => 0x0E06,
            Self::Abend => 0x0E08,
            Self::Assign => 0x1002,
            Self::Address => 0x1004,
            Self::Handle => 0x0202,
            Self::Ignore => 0x0204,
            Self::Syncpoint => 0x1202,
            Self::Start => 0x0C02,
            Self::Cancel => 0x0C04,
            Self::Retrieve => 0x0C06,
            Self::Enq => 0x1402,
            Self::Deq => 0x1404,
            Self::Asktime => 0x1602,
            Self::Formattime => 0x1604,
            Self::WriteQ => 0x0A02,
            Self::ReadQ => 0x0A04,
            Self::DeleteQ => 0x0A06,
            Self::GetMain => 0x1802,
            Self::FreeMain => 0x1804,
            Self::Other(_) => 0xFFFF,
        }
    }
}

/// Parse all EXEC CICS blocks from COBOL source.
///
/// Delegates to the upstream `CicsScanner` from `open-mainframe-cics` for block
/// extraction, then parses each block with precompiler-specific logic.
pub fn parse_exec_cics(source: &str) -> Result<Vec<ExecCicsBlock>, CicsPrecompileError> {
    let mut scanner = CicsScanner::new();
    let raw_blocks = scanner.scan(source)?;

    let mut blocks = Vec::new();
    for raw in &raw_blocks {
        let (command, options) = parse_cics_command(&raw.text);
        blocks.push(ExecCicsBlock {
            line: raw.start_line,
            command,
            options,
            raw_text: raw.text.clone(),
        });
    }

    Ok(blocks)
}

fn parse_cics_command(text: &str) -> (String, Vec<CicsOption>) {
    let upper = text.trim().to_uppercase();
    let tokens: Vec<&str> = upper.split_whitespace().collect();
    let mut options = Vec::new();

    if tokens.is_empty() {
        return (String::new(), options);
    }

    // Determine command name (1 or 2 words).
    let (command_name, option_start) = if tokens.len() >= 2 {
        let second_bare = tokens[1].split('(').next().unwrap_or(tokens[1]);
        let two_word = format!("{} {}", tokens[0], second_bare);
        match two_word.as_str() {
            "SEND MAP" | "SEND TEXT" | "RECEIVE MAP" | "READ FILE" | "WRITE FILE"
            | "REWRITE FILE" | "DELETE FILE" | "HANDLE CONDITION" | "HANDLE AID"
            | "IGNORE CONDITION" | "WRITEQ TD" | "WRITEQ TS" | "READQ TD" | "READQ TS"
            | "DELETEQ TD" | "DELETEQ TS" => {
                if tokens[1].contains('(') {
                    (two_word, 1)
                } else {
                    (two_word, 2)
                }
            }
            _ => (tokens[0].to_string(), 1),
        }
    } else {
        (tokens[0].to_string(), 1)
    };

    // Parse options.
    let text_parts: Vec<&str> = text.split_whitespace().collect();
    let mut idx = option_start;
    while idx < text_parts.len() {
        let part = text_parts[idx].to_uppercase();
        if let Some(eq_pos) = part.find('(') {
            let name = part[..eq_pos].to_string();
            let value_part = &part[eq_pos + 1..];
            let value = if let Some(stripped) = value_part.strip_suffix(')') {
                stripped.to_string()
            } else {
                value_part.to_string()
            };
            options.push(CicsOption {
                name,
                value: Some(value),
            });
        } else {
            options.push(CicsOption {
                name: part,
                value: None,
            });
        }
        idx += 1;
    }

    (command_name, options)
}

// ─────────────────────── Transformation ───────────────────────

/// A transformed CALL statement replacing an EXEC CICS block.
#[derive(Debug, Clone)]
pub struct CicsTransformedCall {
    /// Original source line.
    pub original_line: usize,
    /// Generated COBOL CALL statement.
    pub call_text: String,
    /// CICS command code.
    pub command_code: u16,
}

/// Transform EXEC CICS blocks into CALL 'DFHEI1' statements.
pub fn transform_cics_blocks(blocks: &[ExecCicsBlock]) -> Vec<CicsTransformedCall> {
    blocks
        .iter()
        .map(|block| {
            let cmd = CicsCommand::from_str(&block.command);
            let code = cmd.command_code();

            let option_list: Vec<String> = block
                .options
                .iter()
                .map(|opt| match &opt.value {
                    Some(v) => format!("{}({})", opt.name, v),
                    None => opt.name.clone(),
                })
                .collect();

            let options_str = if option_list.is_empty() {
                String::new()
            } else {
                format!(
                    "\n           {}",
                    option_list.join("\n           ")
                )
            };

            CicsTransformedCall {
                original_line: block.line,
                call_text: format!(
                    "           CALL 'DFHEI1' USING DFHEIBLK\n           X'{code:04X}'{options_str}"
                ),
                command_code: code,
            }
        })
        .collect()
}

// ─────────────────────── DFHEIBLK / DFHCOMMAREA ───────────────────────

/// Generate the DFHEIBLK copybook (EIB -- Execute Interface Block).
pub fn generate_dfheiblk() -> String {
    [
        "       01  DFHEIBLK.",
        "           05  EIBTIME   PIC S9(7) COMP-3.",
        "           05  EIBDATE   PIC S9(7) COMP-3.",
        "           05  EIBTRNID  PIC X(4).",
        "           05  EIBTASKN  PIC S9(7) COMP-3.",
        "           05  EIBTRMID  PIC X(4).",
        "           05  EIBCPOSN  PIC S9(4) COMP.",
        "           05  EIBCALEN  PIC S9(4) COMP.",
        "           05  EIBAID    PIC X.",
        "           05  EIBFN     PIC X(2).",
        "           05  EIBRCODE  PIC X(6).",
        "           05  EIBDS     PIC X(8).",
        "           05  EIBREQID  PIC X(8).",
        "           05  EIBRSRCE  PIC X(8).",
        "           05  EIBSYNC   PIC X.",
        "           05  EIBFREE   PIC X.",
        "           05  EIBRECV   PIC X.",
        "           05  EIBATT    PIC X.",
        "           05  EIBEOC    PIC X.",
        "           05  EIBCONF   PIC X.",
        "           05  EIBERR    PIC X.",
        "           05  EIBERRCD  PIC X(4).",
        "           05  EIBNODAT  PIC X.",
        "           05  EIBRESP   PIC S9(8) COMP.",
        "           05  EIBRESP2  PIC S9(8) COMP.",
    ]
    .join("\n")
}

/// Generate the DFHCOMMAREA definition.
pub fn generate_dfhcommarea(length: u32) -> String {
    format!("       01  DFHCOMMAREA  PIC X({length}).")
}

/// Generate PROCEDURE DIVISION USING clause.
pub fn generate_procedure_division_using() -> &'static str {
    "       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA."
}

// ─────────────────────── BMS Map Support ───────────────────────

/// A BMS symbolic map field.
#[derive(Debug, Clone)]
pub struct BmsField {
    /// Field name.
    pub name: String,
    /// Data length.
    pub length: u32,
    /// Whether input-enabled.
    pub input: bool,
    /// Whether output-enabled.
    pub output: bool,
}

/// A BMS symbolic map definition.
#[derive(Debug, Clone)]
pub struct BmsSymbolicMap {
    /// Map name.
    pub map_name: String,
    /// Mapset name.
    pub mapset_name: String,
    /// Fields in the map.
    pub fields: Vec<BmsField>,
}

impl BmsSymbolicMap {
    /// Generate the COBOL copybook for this symbolic map.
    pub fn generate_copybook(&self) -> String {
        let mut lines = Vec::new();
        let struct_name = format!("{}I", self.map_name);
        lines.push(format!("       01  {struct_name}."));

        for field in &self.fields {
            let field_name_l = format!("{}L", field.name);
            let field_name_a = format!("{}A", field.name);
            let field_name_i = format!("{}I", field.name);

            lines.push(format!(
                "           05  {:<12} PIC S9(4) COMP.",
                field_name_l
            ));
            lines.push(format!("           05  {:<12} PIC X.", field_name_a));
            lines.push(format!(
                "           05  {:<12} PIC X({}).",
                field_name_i, field.length
            ));
        }

        // Output map.
        let out_name = format!("{}O", self.map_name);
        lines.push(format!("       01  {out_name}."));
        for field in &self.fields {
            let field_name_l = format!("{}L", field.name);
            let field_name_a = format!("{}A", field.name);
            let field_name_o = format!("{}O", field.name);

            lines.push(format!(
                "           05  {:<12} PIC S9(4) COMP.",
                field_name_l
            ));
            lines.push(format!("           05  {:<12} PIC X.", field_name_a));
            lines.push(format!(
                "           05  {:<12} PIC X({}).",
                field_name_o, field.length
            ));
        }

        lines.join("\n")
    }
}

// ─────────────────────── Integrated DB2+CICS ───────────────────────

/// Precompilation mode for combined DB2+CICS programs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrecompileMode {
    /// CICS only.
    CicsOnly,
    /// DB2 only.
    Db2Only,
    /// Integrated: CICS first, then DB2.
    Integrated,
}

/// Check if source contains both EXEC SQL and EXEC CICS.
pub fn detect_mode(source: &str) -> PrecompileMode {
    let upper = source.to_uppercase();
    let has_sql = upper.contains("EXEC SQL");
    let has_cics = upper.contains("EXEC CICS");

    match (has_sql, has_cics) {
        (true, true) => PrecompileMode::Integrated,
        (false, true) => PrecompileMode::CicsOnly,
        (true, false) => PrecompileMode::Db2Only,
        (false, false) => PrecompileMode::CicsOnly, // Default.
    }
}

// ─────────────────────── Full CICS Precompilation ───────────────────────

/// Result of a CICS precompilation.
#[derive(Debug)]
pub struct CicsPrecompileResult {
    /// Transformed COBOL source.
    pub transformed_source: String,
    /// Parsed CICS blocks.
    pub cics_blocks: Vec<ExecCicsBlock>,
    /// Whether DFHEIBLK was inserted.
    pub dfheiblk_inserted: bool,
    /// Line number mappings.
    pub line_map: HashMap<usize, usize>,
}

/// Perform CICS precompilation on COBOL source.
pub fn precompile_cics(source: &str) -> Result<CicsPrecompileResult, CicsPrecompileError> {
    let blocks = parse_exec_cics(source)?;
    let calls = transform_cics_blocks(&blocks);

    let lines: Vec<&str> = source.lines().collect();
    let mut output_lines = Vec::new();
    let mut line_map = HashMap::new();
    let mut skip_until_end_exec = false;
    let mut current_call_idx = 0;
    let mut dfheiblk_inserted = false;

    for line in lines.iter() {
        let trimmed = line.trim().to_uppercase();

        if skip_until_end_exec {
            if trimmed.contains("END-EXEC") {
                skip_until_end_exec = false;
                if current_call_idx < calls.len() {
                    let call = &calls[current_call_idx];
                    line_map.insert(call.original_line, output_lines.len() + 1);
                    for call_line in call.call_text.lines() {
                        output_lines.push(call_line.to_string());
                    }
                    current_call_idx += 1;
                }
            }
            continue;
        }

        if trimmed.contains("EXEC CICS") && !trimmed.starts_with('*') {
            if trimmed.contains("END-EXEC") {
                if current_call_idx < calls.len() {
                    let call = &calls[current_call_idx];
                    line_map.insert(call.original_line, output_lines.len() + 1);
                    for call_line in call.call_text.lines() {
                        output_lines.push(call_line.to_string());
                    }
                    current_call_idx += 1;
                }
            } else {
                skip_until_end_exec = true;
            }
            continue;
        }

        // Insert DFHEIBLK before PROCEDURE DIVISION.
        if trimmed.contains("LINKAGE SECTION") && !dfheiblk_inserted {
            output_lines.push(line.to_string());
            for eib_line in generate_dfheiblk().lines() {
                output_lines.push(eib_line.to_string());
            }
            dfheiblk_inserted = true;
            continue;
        }

        output_lines.push(line.to_string());
    }

    Ok(CicsPrecompileResult {
        transformed_source: output_lines.join("\n"),
        cics_blocks: blocks,
        dfheiblk_inserted,
        line_map,
    })
}

// ─────────────────────── Tests ───────────────────────
// NOTE: The upstream CicsScanner enforces COBOL column layout (cols 1-6 = seq,
// col 7 = indicator, cols 8-72 = code). Test data must use proper COBOL
// formatting with a 7-character prefix and lines within 72 columns.

#[cfg(test)]
mod tests {
    use super::*;

    // ─── SYS-113.1: EXEC CICS Parser ───

    #[test]
    fn test_parse_send_map() {
        let source = concat!(
            "       EXEC CICS\n",
            "         SEND MAP('MENU01')\n",
            "         MAPSET('MENUSET') ERASE\n",
            "       END-EXEC."
        );
        let blocks = parse_exec_cics(source).unwrap();
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].command, "SEND MAP");
    }

    #[test]
    fn test_parse_read_file() {
        let source = concat!(
            "       EXEC CICS\n",
            "         READ FILE('EMPFILE')\n",
            "         INTO(WS-RECORD)\n",
            "         RIDFLD(WS-KEY)\n",
            "       END-EXEC."
        );
        let blocks = parse_exec_cics(source).unwrap();
        assert_eq!(blocks.len(), 1);
        assert!(blocks[0].command.contains("READ"));
    }

    #[test]
    fn test_parse_multiple_commands() {
        let source = concat!(
            "       EXEC CICS\n",
            "         SEND MAP('M1')\n",
            "         MAPSET('MS1')\n",
            "       END-EXEC.\n",
            "       DISPLAY 'HELLO'.\n",
            "       EXEC CICS RETURN END-EXEC."
        );
        let blocks = parse_exec_cics(source).unwrap();
        assert_eq!(blocks.len(), 2);
    }

    #[test]
    fn test_parse_multi_line_cics() {
        let source = r#"       EXEC CICS
           SEND MAP('MENU01')
           MAPSET('MENUSET')
           ERASE
       END-EXEC."#;
        let blocks = parse_exec_cics(source).unwrap();
        assert_eq!(blocks.len(), 1);
    }

    #[test]
    fn test_parse_all_command_types() {
        let commands = vec![
            ("       EXEC CICS\n         LINK PROGRAM('SUBPGM')\n       END-EXEC.", "LINK"),
            ("       EXEC CICS\n         XCTL PROGRAM('NEXT')\n       END-EXEC.", "XCTL"),
            ("       EXEC CICS RETURN END-EXEC.", "RETURN"),
            ("       EXEC CICS\n         ABEND ABCODE('ABRT')\n       END-EXEC.", "ABEND"),
            ("       EXEC CICS SYNCPOINT END-EXEC.", "SYNCPOINT"),
            ("       EXEC CICS ASKTIME END-EXEC.", "ASKTIME"),
        ];
        for (cmd_src, label) in &commands {
            let blocks = parse_exec_cics(cmd_src).unwrap();
            assert_eq!(blocks.len(), 1, "Failed for: {label}");
        }
    }

    // ─── SYS-113.2: CICS Command Transformation ───

    #[test]
    fn test_transform_send_map() {
        let source = concat!(
            "       EXEC CICS\n",
            "         SEND MAP('MENU01')\n",
            "         MAPSET('MENUSET') ERASE\n",
            "       END-EXEC."
        );
        let blocks = parse_exec_cics(source).unwrap();
        let calls = transform_cics_blocks(&blocks);
        assert_eq!(calls.len(), 1);
        assert!(calls[0].call_text.contains("CALL 'DFHEI1'"));
        assert!(calls[0].call_text.contains("DFHEIBLK"));
    }

    #[test]
    fn test_transform_read_file() {
        let source = concat!(
            "       EXEC CICS\n",
            "         READ FILE('EMPFILE')\n",
            "         INTO(WS-RECORD)\n",
            "         RIDFLD(WS-KEY)\n",
            "       END-EXEC."
        );
        let blocks = parse_exec_cics(source).unwrap();
        let calls = transform_cics_blocks(&blocks);
        assert!(calls[0].call_text.contains("CALL 'DFHEI1'"));
        assert_eq!(calls[0].command_code, CicsCommand::ReadFile.command_code());
    }

    #[test]
    fn test_command_codes() {
        assert_eq!(CicsCommand::SendMap.command_code(), 0x0804);
        assert_eq!(CicsCommand::ReadFile.command_code(), 0x0602);
        assert_eq!(CicsCommand::Link.command_code(), 0x0E02);
        assert_eq!(CicsCommand::Return.command_code(), 0x0E06);
    }

    // ─── SYS-113.3: DFHEIBLK and DFHCOMMAREA ───

    #[test]
    fn test_dfheiblk_generation() {
        let eib = generate_dfheiblk();
        assert!(eib.contains("DFHEIBLK"));
        assert!(eib.contains("EIBTIME"));
        assert!(eib.contains("EIBDATE"));
        assert!(eib.contains("EIBTRNID"));
        assert!(eib.contains("EIBRSRCE"));
        assert!(eib.contains("EIBRESP"));
        assert!(eib.contains("EIBRESP2"));
    }

    #[test]
    fn test_dfhcommarea_generation() {
        let comm = generate_dfhcommarea(200);
        assert!(comm.contains("DFHCOMMAREA"));
        assert!(comm.contains("PIC X(200)"));
    }

    #[test]
    fn test_procedure_division_using() {
        let proc = generate_procedure_division_using();
        assert!(proc.contains("PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA"));
    }

    // ─── SYS-113.4: BMS Map Support ───

    #[test]
    fn test_bms_symbolic_map() {
        let map = BmsSymbolicMap {
            map_name: "MENU01".to_string(),
            mapset_name: "MENUSET".to_string(),
            fields: vec![
                BmsField {
                    name: "EMPNO".to_string(),
                    length: 6,
                    input: true,
                    output: true,
                },
                BmsField {
                    name: "ENAME".to_string(),
                    length: 30,
                    input: false,
                    output: true,
                },
            ],
        };

        let copybook = map.generate_copybook();
        assert!(copybook.contains("MENU01I"));
        assert!(copybook.contains("MENU01O"));
        assert!(copybook.contains("EMPNOL"));
        assert!(copybook.contains("EMPNOA"));
        assert!(copybook.contains("EMPNOI"));
    }

    #[test]
    fn test_receive_map_references_symbolic() {
        let source = concat!(
            "       EXEC CICS\n",
            "         RECEIVE MAP('MENU01')\n",
            "         MAPSET('MENUSET')\n",
            "         INTO(WS-MENU-MAP)\n",
            "       END-EXEC."
        );
        let blocks = parse_exec_cics(source).unwrap();
        assert_eq!(blocks.len(), 1);
        assert!(blocks[0].command.contains("RECEIVE"));
    }

    // ─── SYS-113.5: Integrated DB2+CICS ───

    #[test]
    fn test_detect_cics_only() {
        let source = "       EXEC CICS SEND MAP('M1') END-EXEC";
        assert_eq!(detect_mode(source), PrecompileMode::CicsOnly);
    }

    #[test]
    fn test_detect_db2_only() {
        let source = "       EXEC SQL SELECT X FROM T END-EXEC";
        assert_eq!(detect_mode(source), PrecompileMode::Db2Only);
    }

    #[test]
    fn test_detect_integrated() {
        let source = r#"       EXEC CICS SEND MAP('M1') MAPSET('MS') END-EXEC
       EXEC SQL SELECT X FROM T END-EXEC"#;
        assert_eq!(detect_mode(source), PrecompileMode::Integrated);
    }

    // ─── SYS-113.6: CICS Precompiler Integration Tests ───

    #[test]
    fn test_full_cics_precompilation() {
        let source = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSPGM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MAP    PIC X(100).
       LINKAGE SECTION.
       01  DFHCOMMAREA PIC X(200).
       PROCEDURE DIVISION.
       EXEC CICS
         SEND MAP('MENU01')
         MAPSET('MENUSET') ERASE
       END-EXEC.
       EXEC CICS RETURN END-EXEC.
       STOP RUN."#;

        let result = precompile_cics(source).unwrap();

        assert!(result.transformed_source.contains("CALL 'DFHEI1'"));
        assert!(result.transformed_source.contains("DFHEIBLK"));
        assert!(result.dfheiblk_inserted);
        assert!(result.transformed_source.contains("STOP RUN"));
        assert_eq!(result.cics_blocks.len(), 2);
    }

    #[test]
    fn test_cics_with_file_operations() {
        let source = r#"       EXEC CICS
         READ FILE('EMPFILE')
         INTO(WS-REC) RIDFLD(WS-KEY)
       END-EXEC.
       EXEC CICS
         REWRITE FILE('EMPFILE')
         FROM(WS-REC)
       END-EXEC.
       EXEC CICS
         DELETE FILE('EMPFILE')
         RIDFLD(WS-KEY)
       END-EXEC."#;
        let blocks = parse_exec_cics(source).unwrap();
        assert_eq!(blocks.len(), 3);
        let calls = transform_cics_blocks(&blocks);
        for call in &calls {
            assert!(call.call_text.contains("CALL 'DFHEI1'"));
        }
    }
}
