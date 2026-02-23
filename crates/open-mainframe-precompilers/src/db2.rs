//! # DB2 COBOL Precompiler
//!
//! Source-to-source transformer for COBOL programs with embedded EXEC SQL.
//!
//! Transforms `EXEC SQL ... END-EXEC` blocks into `CALL 'DSNHLI'` statements,
//! maps host variables, generates SQLCA, and produces a DBRM for DB2 BIND.

use std::collections::HashMap;

// ─────────────────────── Errors ───────────────────────

/// Precompiler errors.
#[derive(Debug, thiserror::Error)]
pub enum Db2PrecompileError {
    /// An EXEC SQL block was not terminated.
    #[error("unterminated EXEC SQL block at line {line}")]
    UnterminatedExecSql { line: usize },

    /// Unknown SQL statement.
    #[error("unrecognized SQL statement at line {line}: {statement}")]
    UnrecognizedSql { line: usize, statement: String },

    /// Host variable not found.
    #[error("host variable not found: {name}")]
    HostVariableNotFound { name: String },
}

// ─────────────────────── Host Variables ───────────────────────

/// SQL data type mapping for host variables.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SqlType {
    Integer,
    Smallint,
    Decimal { precision: u8, scale: u8 },
    Char(u32),
    Varchar(u32),
    Date,
    Timestamp,
}

/// A COBOL host variable mapped to SQL.
#[derive(Debug, Clone)]
pub struct HostVariable {
    /// COBOL variable name.
    pub cobol_name: String,
    /// Mapped SQL type.
    pub sql_type: SqlType,
    /// Indicator variable name, if any.
    pub indicator: Option<String>,
    /// Whether it's an array (OCCURS).
    pub is_array: bool,
    /// Array size if applicable.
    pub array_size: u32,
}

/// Map a COBOL PIC to SQL type.
pub fn pic_to_sql_type(pic: &str) -> SqlType {
    let upper = pic.to_uppercase().replace(' ', "");

    if upper.starts_with("9") || upper.starts_with("S9") {
        // Check for decimal point.
        if upper.contains('V') {
            let parts: Vec<&str> = upper.split('V').collect();
            let int_digits = count_digits(parts[0]);
            let dec_digits = if parts.len() > 1 {
                count_digits(parts[1])
            } else {
                0
            };
            return SqlType::Decimal {
                precision: (int_digits + dec_digits) as u8,
                scale: dec_digits as u8,
            };
        }
        let digits = count_digits(&upper);
        if digits <= 4 {
            SqlType::Smallint
        } else {
            SqlType::Integer
        }
    } else if upper.starts_with("X(") || upper.starts_with("X") {
        let len = extract_length(&upper).unwrap_or(1);
        SqlType::Char(len)
    } else {
        // Default: treat as CHAR.
        let len = extract_length(&upper).unwrap_or(1);
        SqlType::Char(len)
    }
}

fn count_digits(s: &str) -> u32 {
    // Handle patterns like 9(6) or 999.
    if let Some(len) = extract_length(s) {
        len
    } else {
        s.chars().filter(|c| *c == '9').count() as u32
    }
}

fn extract_length(s: &str) -> Option<u32> {
    let start = s.find('(')?;
    let end = s.find(')')?;
    s[start + 1..end].parse().ok()
}

// ─────────────────────── EXEC SQL Parser ───────────────────────

/// A parsed EXEC SQL block.
#[derive(Debug, Clone)]
pub struct ExecSqlBlock {
    /// Source line number.
    pub line: usize,
    /// The raw SQL text (between EXEC SQL and END-EXEC).
    pub sql_text: String,
    /// Parsed statement type.
    pub stmt_type: SqlStatementType,
    /// Host variables referenced (prefixed with ':' in SQL).
    pub host_variables: Vec<String>,
    /// Indicator variables (the part after ':var:ind').
    pub indicators: Vec<(String, String)>,
}

/// SQL statement classification.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SqlStatementType {
    Select,
    Insert,
    Update,
    Delete,
    DeclareCursor(String),
    Open(String),
    Fetch(String),
    Close(String),
    Prepare(String),
    Execute(String),
    Describe(String),
    IncludeSqlca,
    WheneverSqlerror(String),
    WheneverNotFound(String),
    Commit,
    Rollback,
    Other,
}

/// Parse all EXEC SQL blocks from COBOL source.
pub fn parse_exec_sql(source: &str) -> Result<Vec<ExecSqlBlock>, Db2PrecompileError> {
    let mut blocks = Vec::new();
    let lines: Vec<&str> = source.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        let trimmed = lines[i].trim().to_uppercase();
        if trimmed.contains("EXEC SQL") && !trimmed.starts_with('*') {
            let start_line = i + 1; // 1-based.
            let mut sql_parts = Vec::new();

            // Extract the part after EXEC SQL on the same line.
            if let Some(pos) = trimmed.find("EXEC SQL") {
                let after = &lines[i].trim()[pos + 8..].trim().to_string();
                if let Some(end_pos) = after.to_uppercase().find("END-EXEC") {
                    sql_parts.push(after[..end_pos].trim().to_string());
                    let sql_text = sql_parts.join(" ").trim().to_string();
                    let (stmt_type, host_vars, indicators) = classify_sql(&sql_text);
                    blocks.push(ExecSqlBlock {
                        line: start_line,
                        sql_text,
                        stmt_type,
                        host_variables: host_vars,
                        indicators,
                    });
                    i += 1;
                    continue;
                }
                if !after.is_empty() {
                    sql_parts.push(after.clone());
                }
            }

            // Multi-line: scan until END-EXEC.
            i += 1;
            let mut found_end = false;
            while i < lines.len() {
                let line_trimmed = lines[i].trim();
                if line_trimmed.to_uppercase().contains("END-EXEC") {
                    if let Some(pos) = line_trimmed.to_uppercase().find("END-EXEC") {
                        let before = &line_trimmed[..pos];
                        if !before.trim().is_empty() {
                            sql_parts.push(before.trim().to_string());
                        }
                    }
                    found_end = true;
                    break;
                }
                // Skip comment lines (column 7 = *).
                if !line_trimmed.starts_with('*') {
                    sql_parts.push(line_trimmed.to_string());
                }
                i += 1;
            }

            if !found_end {
                return Err(Db2PrecompileError::UnterminatedExecSql { line: start_line });
            }

            let sql_text = sql_parts.join(" ").trim().to_string();
            let (stmt_type, host_vars, indicators) = classify_sql(&sql_text);
            blocks.push(ExecSqlBlock {
                line: start_line,
                sql_text,
                stmt_type,
                host_variables: host_vars,
                indicators,
            });
        }
        i += 1;
    }

    Ok(blocks)
}

fn classify_sql(sql: &str) -> (SqlStatementType, Vec<String>, Vec<(String, String)>) {
    let upper = sql.trim().to_uppercase();
    let mut host_vars = Vec::new();
    let mut indicators = Vec::new();

    // Extract host variables (prefixed with ':').
    let mut chars = sql.chars().peekable();
    while let Some(c) = chars.next() {
        if c == ':' {
            let mut var_name = String::new();
            for ch in chars.by_ref() {
                if ch.is_alphanumeric() || ch == '-' || ch == '_' {
                    var_name.push(ch);
                } else if ch == ':' && !var_name.is_empty() {
                    // Indicator variable follows.
                    let mut ind_name = String::new();
                    for ich in chars.by_ref() {
                        if ich.is_alphanumeric() || ich == '-' || ich == '_' {
                            ind_name.push(ich);
                        } else {
                            break;
                        }
                    }
                    if !ind_name.is_empty() {
                        indicators.push((var_name.to_uppercase(), ind_name.to_uppercase()));
                    }
                    break;
                } else {
                    break;
                }
            }
            if !var_name.is_empty() {
                host_vars.push(var_name.to_uppercase());
            }
        }
    }

    let stmt = if upper.starts_with("SELECT") {
        SqlStatementType::Select
    } else if upper.starts_with("INSERT") {
        SqlStatementType::Insert
    } else if upper.starts_with("UPDATE") {
        SqlStatementType::Update
    } else if upper.starts_with("DELETE") {
        SqlStatementType::Delete
    } else if upper.starts_with("DECLARE") && upper.contains("CURSOR") {
        let name = extract_word_after(&upper, "DECLARE");
        SqlStatementType::DeclareCursor(name)
    } else if upper.starts_with("OPEN") {
        SqlStatementType::Open(extract_first_word_after_keyword(&upper, "OPEN"))
    } else if upper.starts_with("FETCH") {
        SqlStatementType::Fetch(extract_first_word_after_keyword(&upper, "FETCH"))
    } else if upper.starts_with("CLOSE") {
        SqlStatementType::Close(extract_first_word_after_keyword(&upper, "CLOSE"))
    } else if upper.starts_with("PREPARE") {
        SqlStatementType::Prepare(extract_first_word_after_keyword(&upper, "PREPARE"))
    } else if upper.starts_with("EXECUTE") {
        SqlStatementType::Execute(extract_first_word_after_keyword(&upper, "EXECUTE"))
    } else if upper.starts_with("DESCRIBE") {
        SqlStatementType::Describe(extract_first_word_after_keyword(&upper, "DESCRIBE"))
    } else if upper.starts_with("INCLUDE") && upper.contains("SQLCA") {
        SqlStatementType::IncludeSqlca
    } else if upper.starts_with("WHENEVER") && upper.contains("SQLERROR") {
        let target = extract_goto_target(&upper);
        SqlStatementType::WheneverSqlerror(target)
    } else if upper.starts_with("WHENEVER") && upper.contains("NOT FOUND") {
        let target = extract_goto_target(&upper);
        SqlStatementType::WheneverNotFound(target)
    } else if upper.starts_with("COMMIT") {
        SqlStatementType::Commit
    } else if upper.starts_with("ROLLBACK") {
        SqlStatementType::Rollback
    } else {
        SqlStatementType::Other
    };

    (stmt, host_vars, indicators)
}

fn extract_word_after(s: &str, keyword: &str) -> String {
    if let Some(pos) = s.find(keyword) {
        let after = s[pos + keyword.len()..].trim();
        after
            .split_whitespace()
            .next()
            .unwrap_or("")
            .to_string()
    } else {
        String::new()
    }
}

fn extract_first_word_after_keyword(s: &str, keyword: &str) -> String {
    if let Some(pos) = s.find(keyword) {
        let after = s[pos + keyword.len()..].trim();
        after
            .split_whitespace()
            .next()
            .unwrap_or("")
            .to_string()
    } else {
        String::new()
    }
}

fn extract_goto_target(s: &str) -> String {
    // WHENEVER SQLERROR GO TO err-handler
    if let Some(pos) = s.find("GO TO") {
        let after = s[pos + 5..].trim();
        after
            .split_whitespace()
            .next()
            .unwrap_or("")
            .trim_start_matches(':')
            .to_string()
    } else if let Some(pos) = s.find("GOTO") {
        let after = s[pos + 4..].trim();
        after
            .split_whitespace()
            .next()
            .unwrap_or("")
            .trim_start_matches(':')
            .to_string()
    } else {
        String::new()
    }
}

// ─────────────────────── Transformation ───────────────────────

/// A transformed CALL statement replacing an EXEC SQL block.
#[derive(Debug, Clone)]
pub struct TransformedCall {
    /// Original source line.
    pub original_line: usize,
    /// Generated COBOL CALL statement.
    pub call_text: String,
    /// Statement ID for DBRM reference.
    pub stmt_id: u32,
}

/// Transform EXEC SQL blocks into CALL 'DSNHLI' statements.
pub fn transform_sql_blocks(blocks: &[ExecSqlBlock]) -> Vec<TransformedCall> {
    let mut calls = Vec::new();
    let mut stmt_id = 1u32;

    for block in blocks {
        let call = match &block.stmt_type {
            SqlStatementType::IncludeSqlca => TransformedCall {
                original_line: block.line,
                call_text: generate_sqlca_include(),
                stmt_id: 0,
            },
            SqlStatementType::WheneverSqlerror(target) => TransformedCall {
                original_line: block.line,
                call_text: format!(
                    "      * WHENEVER SQLERROR GO TO {target}\n      * (error handler set)"
                ),
                stmt_id: 0,
            },
            SqlStatementType::WheneverNotFound(target) => TransformedCall {
                original_line: block.line,
                call_text: format!(
                    "      * WHENEVER NOT FOUND GO TO {target}\n      * (not-found handler set)"
                ),
                stmt_id: 0,
            },
            _ => {
                let host_var_list = if block.host_variables.is_empty() {
                    String::new()
                } else {
                    format!(
                        "\n           {}",
                        block
                            .host_variables
                            .iter()
                            .map(|v| format!(":{v}"))
                            .collect::<Vec<_>>()
                            .join(" ")
                    )
                };
                let id = stmt_id;
                stmt_id += 1;
                TransformedCall {
                    original_line: block.line,
                    call_text: format!(
                        "           CALL 'DSNHLI' USING SQLCA\n           STMT-ID-{id}{host_var_list}"
                    ),
                    stmt_id: id,
                }
            }
        };
        calls.push(call);
    }

    calls
}

// ─────────────────────── SQLCA Generation ───────────────────────

/// Generate the SQLCA copybook insertion.
pub fn generate_sqlca_include() -> String {
    [
        "       01  SQLCA.",
        "           05  SQLCAID    PIC X(8)  VALUE 'SQLCA'.",
        "           05  SQLCABC   PIC S9(9) COMP VALUE 136.",
        "           05  SQLCODE   PIC S9(9) COMP.",
        "           05  SQLERRM.",
        "               10  SQLERRML PIC S9(4) COMP.",
        "               10  SQLERRMC PIC X(70).",
        "           05  SQLERRP   PIC X(8).",
        "           05  SQLERRD   PIC S9(9) COMP OCCURS 6.",
        "           05  SQLWARN.",
        "               10  SQLWARN0 PIC X.",
        "               10  SQLWARN1 PIC X.",
        "               10  SQLWARN2 PIC X.",
        "               10  SQLWARN3 PIC X.",
        "               10  SQLWARN4 PIC X.",
        "               10  SQLWARN5 PIC X.",
        "               10  SQLWARN6 PIC X.",
        "               10  SQLWARN7 PIC X.",
        "           05  SQLSTATE  PIC X(5).",
    ]
    .join("\n")
}

/// Generate WHENEVER SQLERROR check (to insert after each SQL CALL).
pub fn generate_sqlerror_check(goto_label: &str) -> String {
    format!(
        "           IF SQLCODE < 0\n              GO TO {goto_label}\n           END-IF"
    )
}

// ─────────────────────── DBRM Generation ───────────────────────

/// A DBRM (Database Request Module) for DB2 BIND.
#[derive(Debug, Clone)]
pub struct Dbrm {
    /// Program name.
    pub program_name: String,
    /// SQL statements with parameter markers and metadata.
    pub statements: Vec<DbrmStatement>,
}

/// A SQL statement in the DBRM.
#[derive(Debug, Clone)]
pub struct DbrmStatement {
    /// Statement ID.
    pub stmt_id: u32,
    /// SQL text with host variables replaced by parameter markers.
    pub sql_with_markers: String,
    /// Host variable metadata.
    pub host_variables: Vec<DbrmHostVar>,
}

/// Host variable metadata in the DBRM.
#[derive(Debug, Clone)]
pub struct DbrmHostVar {
    /// Variable name.
    pub name: String,
    /// SQL type mapping.
    pub sql_type: SqlType,
    /// Position in the parameter marker list (1-based).
    pub position: u32,
}

/// Generate a DBRM from parsed SQL blocks and host variable mappings.
pub fn generate_dbrm(
    program_name: &str,
    blocks: &[ExecSqlBlock],
    host_var_map: &HashMap<String, HostVariable>,
) -> Dbrm {
    let mut statements = Vec::new();
    let mut stmt_id = 1u32;

    for block in blocks {
        // Skip non-SQL statements.
        match &block.stmt_type {
            SqlStatementType::IncludeSqlca
            | SqlStatementType::WheneverSqlerror(_)
            | SqlStatementType::WheneverNotFound(_) => continue,
            _ => {}
        }

        // Replace host variables with parameter markers.
        let mut sql = block.sql_text.clone();
        let mut dbrm_vars = Vec::new();
        let mut pos = 1u32;

        for var_name in &block.host_variables {
            let marker = "?";
            sql = sql.replace(&format!(":{}", var_name.to_lowercase()), marker);
            sql = sql.replace(&format!(":{var_name}"), marker);

            let sql_type = host_var_map
                .get(var_name)
                .map(|hv| hv.sql_type.clone())
                .unwrap_or(SqlType::Char(80));

            dbrm_vars.push(DbrmHostVar {
                name: var_name.clone(),
                sql_type,
                position: pos,
            });
            pos += 1;
        }

        statements.push(DbrmStatement {
            stmt_id,
            sql_with_markers: sql,
            host_variables: dbrm_vars,
        });
        stmt_id += 1;
    }

    Dbrm {
        program_name: program_name.to_uppercase(),
        statements,
    }
}

/// Serialize a DBRM to a binary representation.
pub fn serialize_dbrm(dbrm: &Dbrm) -> Vec<u8> {
    let mut data = Vec::new();

    // Header: "DBRM" magic + program name (8 bytes, padded).
    data.extend_from_slice(b"DBRM");
    let mut name_bytes = [b' '; 8];
    for (i, b) in dbrm.program_name.as_bytes().iter().take(8).enumerate() {
        name_bytes[i] = *b;
    }
    data.extend_from_slice(&name_bytes);

    // Statement count (4 bytes big-endian).
    data.extend_from_slice(&(dbrm.statements.len() as u32).to_be_bytes());

    // Each statement.
    for stmt in &dbrm.statements {
        data.extend_from_slice(&stmt.stmt_id.to_be_bytes());
        let sql_bytes = stmt.sql_with_markers.as_bytes();
        data.extend_from_slice(&(sql_bytes.len() as u32).to_be_bytes());
        data.extend_from_slice(sql_bytes);
        data.extend_from_slice(&(stmt.host_variables.len() as u32).to_be_bytes());
        for hv in &stmt.host_variables {
            let mut hv_name = [b' '; 32];
            for (i, b) in hv.name.as_bytes().iter().take(32).enumerate() {
                hv_name[i] = *b;
            }
            data.extend_from_slice(&hv_name);
            data.extend_from_slice(&hv.position.to_be_bytes());
        }
    }

    data
}

// ─────────────────────── Full Precompilation ───────────────────────

/// Result of a full DB2 precompilation.
#[derive(Debug)]
pub struct Db2PrecompileResult {
    /// Transformed COBOL source.
    pub transformed_source: String,
    /// Generated DBRM.
    pub dbrm: Dbrm,
    /// Parsed SQL blocks.
    pub sql_blocks: Vec<ExecSqlBlock>,
    /// Line number mapping (original -> transformed).
    pub line_map: HashMap<usize, usize>,
}

/// Perform full DB2 precompilation on COBOL source.
pub fn precompile_db2(
    source: &str,
    program_name: &str,
    host_var_map: &HashMap<String, HostVariable>,
) -> Result<Db2PrecompileResult, Db2PrecompileError> {
    let blocks = parse_exec_sql(source)?;
    let calls = transform_sql_blocks(&blocks);
    let dbrm = generate_dbrm(program_name, &blocks, host_var_map);

    // Build transformed source by replacing EXEC SQL blocks with CALL statements.
    let lines: Vec<&str> = source.lines().collect();
    let mut output_lines = Vec::new();
    let mut line_map = HashMap::new();
    let mut skip_until_end_exec = false;
    let mut current_call_idx = 0;

    for line in lines.iter() {
        let trimmed = line.trim().to_uppercase();

        if skip_until_end_exec {
            if trimmed.contains("END-EXEC") {
                skip_until_end_exec = false;
                // Insert the call statement.
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

        if trimmed.contains("EXEC SQL") && !trimmed.starts_with('*') {
            // Check if END-EXEC is on the same line.
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

        output_lines.push(line.to_string());
    }

    Ok(Db2PrecompileResult {
        transformed_source: output_lines.join("\n"),
        dbrm,
        sql_blocks: blocks,
        line_map,
    })
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ─── SYS-112.1: EXEC SQL Parser ───

    #[test]
    fn test_parse_single_line_exec_sql() {
        let source = "       EXEC SQL SELECT ENAME INTO :WS-NAME FROM EMP WHERE EMPNO = :WS-ID END-EXEC";
        let blocks = parse_exec_sql(source).unwrap();
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].stmt_type, SqlStatementType::Select);
        assert!(blocks[0].host_variables.contains(&"WS-NAME".to_string()));
        assert!(blocks[0].host_variables.contains(&"WS-ID".to_string()));
    }

    #[test]
    fn test_parse_multi_line_exec_sql() {
        let source = "\
       EXEC SQL
           SELECT ENAME, SALARY
           INTO :WS-NAME, :WS-SALARY
           FROM EMP
           WHERE EMPNO = :WS-ID
       END-EXEC";
        let blocks = parse_exec_sql(source).unwrap();
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].host_variables.len(), 3);
    }

    #[test]
    fn test_parse_multiple_blocks() {
        let source = "\
       EXEC SQL INCLUDE SQLCA END-EXEC
       MOVE 100 TO WS-ID.
       EXEC SQL SELECT ENAME INTO :WS-NAME FROM EMP WHERE EMPNO = :WS-ID END-EXEC
       EXEC SQL INSERT INTO LOG VALUES(:WS-MSG) END-EXEC";
        let blocks = parse_exec_sql(source).unwrap();
        assert_eq!(blocks.len(), 3);
        assert_eq!(blocks[0].stmt_type, SqlStatementType::IncludeSqlca);
        assert_eq!(blocks[1].stmt_type, SqlStatementType::Select);
        assert_eq!(blocks[2].stmt_type, SqlStatementType::Insert);
    }

    #[test]
    fn test_parse_indicator_variable() {
        let source =
            "       EXEC SQL SELECT ENAME INTO :WS-NAME:WS-NAME-IND FROM EMP END-EXEC";
        let blocks = parse_exec_sql(source).unwrap();
        assert_eq!(blocks[0].indicators.len(), 1);
        assert_eq!(blocks[0].indicators[0].0, "WS-NAME");
        assert_eq!(blocks[0].indicators[0].1, "WS-NAME-IND");
    }

    // ─── SYS-112.2: Host Variable Mapping ───

    #[test]
    fn test_pic_to_integer() {
        assert_eq!(pic_to_sql_type("9(6)"), SqlType::Integer);
        assert_eq!(pic_to_sql_type("S9(9)"), SqlType::Integer);
    }

    #[test]
    fn test_pic_to_smallint() {
        assert_eq!(pic_to_sql_type("9(4)"), SqlType::Smallint);
        assert_eq!(pic_to_sql_type("99"), SqlType::Smallint);
    }

    #[test]
    fn test_pic_to_decimal() {
        assert_eq!(
            pic_to_sql_type("S9(5)V9(2)"),
            SqlType::Decimal {
                precision: 7,
                scale: 2
            }
        );
    }

    #[test]
    fn test_pic_to_char() {
        assert_eq!(pic_to_sql_type("X(30)"), SqlType::Char(30));
    }

    // ─── SYS-112.3: Static SQL Transformation ───

    #[test]
    fn test_transform_select() {
        let source =
            "       EXEC SQL SELECT ENAME INTO :WS-NAME FROM EMP WHERE EMPNO = :WS-ID END-EXEC";
        let blocks = parse_exec_sql(source).unwrap();
        let calls = transform_sql_blocks(&blocks);
        assert_eq!(calls.len(), 1);
        assert!(calls[0].call_text.contains("CALL 'DSNHLI'"));
        assert!(calls[0].call_text.contains("SQLCA"));
        assert_eq!(calls[0].stmt_id, 1);
    }

    #[test]
    fn test_transform_cursor_operations() {
        let source = "\
       EXEC SQL DECLARE C1 CURSOR FOR SELECT ENAME FROM EMP END-EXEC
       EXEC SQL OPEN C1 END-EXEC
       EXEC SQL FETCH C1 INTO :WS-NAME END-EXEC
       EXEC SQL CLOSE C1 END-EXEC";
        let blocks = parse_exec_sql(source).unwrap();
        assert_eq!(blocks[0].stmt_type, SqlStatementType::DeclareCursor("C1".to_string()));
        assert_eq!(blocks[1].stmt_type, SqlStatementType::Open("C1".to_string()));
        assert_eq!(blocks[2].stmt_type, SqlStatementType::Fetch("C1".to_string()));
        assert_eq!(blocks[3].stmt_type, SqlStatementType::Close("C1".to_string()));

        let calls = transform_sql_blocks(&blocks);
        assert_eq!(calls.len(), 4);
        for call in &calls {
            assert!(call.call_text.contains("CALL 'DSNHLI'"));
        }
    }

    // ─── SYS-112.4: Dynamic SQL Support ───

    #[test]
    fn test_dynamic_sql_prepare() {
        let source = "       EXEC SQL PREPARE STMT1 FROM :WS-SQL-TEXT END-EXEC";
        let blocks = parse_exec_sql(source).unwrap();
        assert_eq!(
            blocks[0].stmt_type,
            SqlStatementType::Prepare("STMT1".to_string())
        );
    }

    #[test]
    fn test_dynamic_sql_execute() {
        let source =
            "       EXEC SQL EXECUTE STMT1 USING :WS-PARAM1, :WS-PARAM2 END-EXEC";
        let blocks = parse_exec_sql(source).unwrap();
        assert_eq!(
            blocks[0].stmt_type,
            SqlStatementType::Execute("STMT1".to_string())
        );
        assert_eq!(blocks[0].host_variables.len(), 2);
    }

    // ─── SYS-112.5: SQLCA Generation ───

    #[test]
    fn test_sqlca_include() {
        let sqlca = generate_sqlca_include();
        assert!(sqlca.contains("SQLCODE"));
        assert!(sqlca.contains("SQLERRM"));
        assert!(sqlca.contains("SQLWARN"));
        assert!(sqlca.contains("SQLSTATE"));
        assert!(sqlca.contains("SQLCAID"));
    }

    #[test]
    fn test_whenever_sqlerror() {
        let source = "       EXEC SQL WHENEVER SQLERROR GO TO ERR-HANDLER END-EXEC";
        let blocks = parse_exec_sql(source).unwrap();
        assert_eq!(
            blocks[0].stmt_type,
            SqlStatementType::WheneverSqlerror("ERR-HANDLER".to_string())
        );
    }

    #[test]
    fn test_sqlerror_check_generation() {
        let check = generate_sqlerror_check("ERR-HANDLER");
        assert!(check.contains("SQLCODE < 0"));
        assert!(check.contains("GO TO ERR-HANDLER"));
    }

    // ─── SYS-112.6: DBRM Generation ───

    #[test]
    fn test_dbrm_generation() {
        let source = "\
       EXEC SQL SELECT ENAME INTO :WS-NAME FROM EMP WHERE EMPNO = :WS-ID END-EXEC
       EXEC SQL INSERT INTO LOG (MSG) VALUES (:WS-MSG) END-EXEC
       EXEC SQL UPDATE EMP SET SALARY = :WS-SAL WHERE EMPNO = :WS-ID END-EXEC
       EXEC SQL DELETE FROM TEMP WHERE ID = :WS-TMP END-EXEC
       EXEC SQL COMMIT END-EXEC";

        let blocks = parse_exec_sql(source).unwrap();
        let mut hv_map = HashMap::new();
        hv_map.insert(
            "WS-NAME".to_string(),
            HostVariable {
                cobol_name: "WS-NAME".to_string(),
                sql_type: SqlType::Char(30),
                indicator: None,
                is_array: false,
                array_size: 0,
            },
        );

        let dbrm = generate_dbrm("TESTPGM", &blocks, &hv_map);
        assert_eq!(dbrm.program_name, "TESTPGM");
        assert_eq!(dbrm.statements.len(), 5);
    }

    #[test]
    fn test_dbrm_serialization() {
        let dbrm = Dbrm {
            program_name: "MYPROG".to_string(),
            statements: vec![DbrmStatement {
                stmt_id: 1,
                sql_with_markers: "SELECT ENAME FROM EMP WHERE EMPNO = ?".to_string(),
                host_variables: vec![DbrmHostVar {
                    name: "WS-ID".to_string(),
                    sql_type: SqlType::Integer,
                    position: 1,
                }],
            }],
        };

        let data = serialize_dbrm(&dbrm);
        assert!(data.len() > 16);
        assert_eq!(&data[0..4], b"DBRM");
        assert_eq!(&data[4..10], b"MYPROG");
    }

    // ─── SYS-112.7: Line Number Preservation ───

    #[test]
    fn test_line_number_tracking() {
        let source = "\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       EXEC SQL INCLUDE SQLCA END-EXEC
       PROCEDURE DIVISION.
       EXEC SQL SELECT X INTO :WS-X FROM T END-EXEC
       STOP RUN.";

        let blocks = parse_exec_sql(source).unwrap();
        assert_eq!(blocks[0].line, 5); // INCLUDE SQLCA
        assert_eq!(blocks[1].line, 7); // SELECT

        let calls = transform_sql_blocks(&blocks);
        assert_eq!(calls[0].original_line, 5);
        assert_eq!(calls[1].original_line, 7);
    }

    // ─── SYS-112.8: DB2 Precompiler Integration Tests ───

    #[test]
    fn test_full_precompilation() {
        let source = "\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPRPT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       EXEC SQL INCLUDE SQLCA END-EXEC
       01  WS-NAME    PIC X(30).
       01  WS-ID      PIC 9(6).
       PROCEDURE DIVISION.
       EXEC SQL
           SELECT ENAME INTO :WS-NAME
           FROM EMP
           WHERE EMPNO = :WS-ID
       END-EXEC
       DISPLAY WS-NAME.
       STOP RUN.";

        let mut hv_map = HashMap::new();
        hv_map.insert(
            "WS-NAME".to_string(),
            HostVariable {
                cobol_name: "WS-NAME".to_string(),
                sql_type: SqlType::Char(30),
                indicator: None,
                is_array: false,
                array_size: 0,
            },
        );
        hv_map.insert(
            "WS-ID".to_string(),
            HostVariable {
                cobol_name: "WS-ID".to_string(),
                sql_type: SqlType::Integer,
                indicator: None,
                is_array: false,
                array_size: 0,
            },
        );

        let result = precompile_db2(source, "EMPRPT", &hv_map).unwrap();

        // SQLCA should be in the output.
        assert!(result.transformed_source.contains("SQLCODE"));

        // CALL should be in the output.
        assert!(result.transformed_source.contains("CALL 'DSNHLI'"));

        // Non-SQL lines preserved.
        assert!(result.transformed_source.contains("DISPLAY WS-NAME"));
        assert!(result.transformed_source.contains("STOP RUN"));

        // DBRM has the SELECT statement.
        assert_eq!(result.dbrm.statements.len(), 1);
        assert_eq!(result.dbrm.program_name, "EMPRPT");
    }
}
