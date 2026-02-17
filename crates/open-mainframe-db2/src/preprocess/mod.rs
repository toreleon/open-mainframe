//! SQL preprocessing for COBOL programs.
//!
//! This module handles extraction and processing of EXEC SQL statements
//! from COBOL source code.

mod dbrm;
mod scanner;
mod sqlca;

pub use dbrm::{Dbrm, DbrmStatement, DbrmHostVariable};
pub use scanner::{SqlBlock, SqlScanner};
pub use sqlca::{generate_sqlca_copybook, generate_sqlcode_conditions, SqlCode};

use crate::Db2Result;

/// Result of preprocessing a COBOL source file.
#[derive(Debug)]
pub struct PreprocessResult {
    /// Pure COBOL source with EXEC SQL replaced by CALL statements
    pub cobol_source: String,
    /// Extracted SQL statements for DBRM
    pub sql_statements: Vec<SqlStatement>,
    /// Host variables found in SQL
    pub host_variables: Vec<HostVariable>,
}

/// A SQL statement extracted from COBOL source.
#[derive(Debug, Clone)]
pub struct SqlStatement {
    /// Statement number (1-based)
    pub number: usize,
    /// The SQL text (without EXEC SQL / END-EXEC)
    pub sql: String,
    /// Starting line in original source
    pub start_line: usize,
    /// Ending line in original source
    pub end_line: usize,
    /// Statement type
    pub stmt_type: SqlStatementType,
}

/// Types of SQL statements.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SqlStatementType {
    /// SELECT ... INTO
    SelectInto,
    /// INSERT
    Insert,
    /// UPDATE
    Update,
    /// DELETE
    Delete,
    /// DECLARE CURSOR
    DeclareCursor,
    /// OPEN cursor
    Open,
    /// FETCH cursor
    Fetch,
    /// CLOSE cursor
    Close,
    /// COMMIT
    Commit,
    /// ROLLBACK
    Rollback,
    /// INCLUDE (copybook)
    Include,
    /// WHENEVER (error handling)
    Whenever,
    /// PREPARE — compile dynamic SQL into a named statement
    Prepare,
    /// EXECUTE — run a previously prepared statement
    Execute,
    /// EXECUTE IMMEDIATE — one-shot dynamic SQL
    ExecuteImmediate,
    /// DESCRIBE — populate SQLDA with result column info
    Describe,
    /// Other/unknown
    Other,
}

impl SqlStatementType {
    /// Determine statement type from SQL text.
    pub fn from_sql(sql: &str) -> Self {
        let upper = sql.trim().to_uppercase();
        let first_word = upper.split_whitespace().next().unwrap_or("");

        match first_word {
            "SELECT" => {
                if upper.contains(" INTO ") && !upper.contains(" INTO TABLE") {
                    SqlStatementType::SelectInto
                } else {
                    SqlStatementType::Other
                }
            }
            "INSERT" => SqlStatementType::Insert,
            "UPDATE" => SqlStatementType::Update,
            "DELETE" => SqlStatementType::Delete,
            "DECLARE" => SqlStatementType::DeclareCursor,
            "OPEN" => SqlStatementType::Open,
            "FETCH" => SqlStatementType::Fetch,
            "CLOSE" => SqlStatementType::Close,
            "COMMIT" => SqlStatementType::Commit,
            "ROLLBACK" => SqlStatementType::Rollback,
            "INCLUDE" => SqlStatementType::Include,
            "WHENEVER" => SqlStatementType::Whenever,
            "PREPARE" => SqlStatementType::Prepare,
            "EXECUTE" => {
                // EXECUTE IMMEDIATE vs plain EXECUTE
                let words: Vec<&str> = upper.split_whitespace().collect();
                if words.len() > 1 && words[1] == "IMMEDIATE" {
                    SqlStatementType::ExecuteImmediate
                } else {
                    SqlStatementType::Execute
                }
            }
            "DESCRIBE" => SqlStatementType::Describe,
            _ => SqlStatementType::Other,
        }
    }
}

/// A host variable used in SQL.
#[derive(Debug, Clone)]
pub struct HostVariable {
    /// Variable name (without colon prefix)
    pub name: String,
    /// Optional indicator variable name
    pub indicator: Option<String>,
    /// Statement number where used
    pub statement_number: usize,
    /// Whether used as input or output
    pub usage: HostVariableUsage,
}

/// How a host variable is used.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HostVariableUsage {
    /// Input parameter (in WHERE, VALUES, SET)
    Input,
    /// Output parameter (in INTO clause)
    Output,
    /// Both input and output
    Both,
}

/// Action to take when a WHENEVER condition is triggered.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WheneverAction {
    /// No action — processing continues normally.
    Continue,
    /// Branch to the specified COBOL paragraph/label.
    GoTo(String),
}

/// Tracks the current WHENEVER settings for all three conditions.
///
/// Each condition slot is `None` (no WHENEVER issued yet) or a
/// `WheneverAction`.  State persists until overridden by another
/// WHENEVER for the same condition.
#[derive(Debug, Clone, Default)]
pub struct WheneverState {
    /// Action for SQLCODE < 0.
    pub sqlerror: Option<WheneverAction>,
    /// Action for SQLCODE = 100.
    pub not_found: Option<WheneverAction>,
    /// Action for SQLWARN0 = 'W'.
    pub sqlwarning: Option<WheneverAction>,
}

impl WheneverState {
    /// Parse a WHENEVER SQL text and update the appropriate slot.
    ///
    /// Expected forms:
    /// - `WHENEVER SQLERROR CONTINUE`
    /// - `WHENEVER SQLERROR GO TO <label>`
    /// - `WHENEVER NOT FOUND GO TO <label>`
    /// - `WHENEVER SQLWARNING CONTINUE`
    pub fn apply(&mut self, sql: &str) {
        let upper = sql.trim().to_uppercase();
        let words: Vec<&str> = upper.split_whitespace().collect();

        // words[0] = "WHENEVER"
        if words.len() < 3 {
            return;
        }

        // Determine which condition and where the action starts
        let (condition, action_start) = if words[1] == "NOT" && words.get(2) == Some(&"FOUND") {
            ("NOT_FOUND", 3)
        } else if words[1] == "SQLERROR" {
            ("SQLERROR", 2)
        } else if words[1] == "SQLWARNING" {
            ("SQLWARNING", 2)
        } else {
            return;
        };

        // Parse action
        let action = if words.get(action_start) == Some(&"CONTINUE") {
            WheneverAction::Continue
        } else if words.get(action_start) == Some(&"GO")
            && words.get(action_start + 1) == Some(&"TO")
        {
            if let Some(&label) = words.get(action_start + 2) {
                WheneverAction::GoTo(label.to_string())
            } else {
                return;
            }
        } else if words.get(action_start) == Some(&"GOTO") {
            if let Some(&label) = words.get(action_start + 1) {
                WheneverAction::GoTo(label.to_string())
            } else {
                return;
            }
        } else {
            return;
        };

        match condition {
            "SQLERROR" => self.sqlerror = Some(action),
            "NOT_FOUND" => self.not_found = Some(action),
            "SQLWARNING" => self.sqlwarning = Some(action),
            _ => {}
        }
    }

    /// Generate COBOL IF checks based on current state.
    ///
    /// Returns empty string if all conditions are Continue or unset.
    pub fn generate_checks(&self) -> String {
        let mut checks = Vec::new();

        if let Some(WheneverAction::GoTo(ref label)) = self.sqlerror {
            checks.push(format!(
                "           IF SQLCODE < 0 GO TO {label}"
            ));
        }

        if let Some(WheneverAction::GoTo(ref label)) = self.not_found {
            checks.push(format!(
                "           IF SQLCODE = 100 GO TO {label}"
            ));
        }

        if let Some(WheneverAction::GoTo(ref label)) = self.sqlwarning {
            checks.push(format!(
                "           IF SQLWARN0 = 'W' GO TO {label}"
            ));
        }

        checks.join("\n")
    }
}

/// SQL preprocessor for COBOL programs.
pub struct SqlPreprocessor {
    scanner: SqlScanner,
    statements: Vec<SqlStatement>,
    host_variables: Vec<HostVariable>,
    statement_counter: usize,
    /// Current WHENEVER state (persists across EXEC SQL blocks).
    whenever_state: WheneverState,
}

impl SqlPreprocessor {
    /// Create a new preprocessor.
    pub fn new() -> Self {
        Self {
            scanner: SqlScanner::new(),
            statements: Vec::new(),
            host_variables: Vec::new(),
            statement_counter: 0,
            whenever_state: WheneverState::default(),
        }
    }

    /// Get the current WHENEVER state.
    pub fn whenever_state(&self) -> &WheneverState {
        &self.whenever_state
    }

    /// Process COBOL source and extract SQL.
    pub fn process(&mut self, source: &str) -> Db2Result<PreprocessResult> {
        // Scan for EXEC SQL blocks
        let blocks = self.scanner.scan(source)?;

        // First pass (forward): collect statement types and compute WHENEVER
        // state snapshots so that the reverse-order replacement pass can
        // emit the correct IF checks for each block.
        let mut whenever_snapshots: Vec<WheneverState> = Vec::with_capacity(blocks.len());
        let mut forward_state = WheneverState::default();

        for block in &blocks {
            let stmt_type = SqlStatementType::from_sql(&block.sql);

            if stmt_type == SqlStatementType::Whenever {
                // Update state from this WHENEVER declaration
                forward_state.apply(&block.sql);
            }

            // Snapshot the state *after* processing this block
            whenever_snapshots.push(forward_state.clone());
        }

        // Record final WHENEVER state for external callers
        self.whenever_state = forward_state;

        // Second pass (reverse): replace blocks with CALL statements
        let mut cobol_lines: Vec<String> = source.lines().map(|s| s.to_string()).collect();

        for (idx, block) in blocks.iter().enumerate().rev() {
            self.statement_counter += 1;
            let stmt_num = self.statement_counter;

            // Determine statement type
            let stmt_type = SqlStatementType::from_sql(&block.sql);

            // Extract host variables
            let vars = self.extract_host_variables(&block.sql, stmt_num, stmt_type);
            self.host_variables.extend(vars);

            // Create SQL statement record
            let statement = SqlStatement {
                number: stmt_num,
                sql: block.sql.clone(),
                start_line: block.start_line,
                end_line: block.end_line,
                stmt_type,
            };
            self.statements.push(statement);

            // Generate replacement code
            if stmt_type == SqlStatementType::Whenever {
                // WHENEVER is a preprocessor directive — it produces no
                // runtime CALL, just a comment showing the declaration.
                let replacement = format!(
                    "      * WHENEVER: {}",
                    block.sql.trim()
                );
                self.replace_block(&mut cobol_lines, block, &replacement);
            } else {
                // Generate COBOL CALL replacement
                let mut call_code = self.generate_call(stmt_num, stmt_type, &block.sql);

                // Append WHENEVER IF checks using the snapshot for this block
                let checks = whenever_snapshots[idx].generate_checks();
                if !checks.is_empty() {
                    call_code.push('\n');
                    call_code.push_str(&checks);
                }

                self.replace_block(&mut cobol_lines, block, &call_code);
            }
        }

        // Reverse statements to get correct order
        self.statements.reverse();

        Ok(PreprocessResult {
            cobol_source: cobol_lines.join("\n"),
            sql_statements: self.statements.clone(),
            host_variables: self.host_variables.clone(),
        })
    }

    /// Extract host variables from SQL.
    fn extract_host_variables(
        &self,
        sql: &str,
        stmt_num: usize,
        stmt_type: SqlStatementType,
    ) -> Vec<HostVariable> {
        let mut vars = Vec::new();
        let mut chars = sql.chars().peekable();
        let mut in_string = false;
        let mut current_pos = 0;

        // Find INTO and FROM/WHERE positions to determine output vs input
        let upper_sql = sql.to_uppercase();
        let into_pos = upper_sql.find(" INTO ");
        let from_pos = upper_sql.find(" FROM ");

        // For SELECT INTO, variables between INTO and FROM are outputs
        // Variables after WHERE are inputs
        let into_end = if let (Some(into), Some(from)) = (into_pos, from_pos) {
            if from > into {
                Some(from)
            } else {
                None
            }
        } else {
            None
        };

        while let Some(c) = chars.next() {
            current_pos += c.len_utf8();

            if c == '\'' {
                in_string = !in_string;
                continue;
            }

            if in_string {
                continue;
            }

            if c == ':' {
                let var_start_pos = current_pos - 1;
                // Found host variable
                let mut name = String::new();
                while let Some(&nc) = chars.peek() {
                    if nc.is_alphanumeric() || nc == '-' || nc == '_' {
                        let ch = chars.next().unwrap();
                        current_pos += ch.len_utf8();
                        name.push(ch);
                    } else {
                        break;
                    }
                }

                if !name.is_empty() {
                    // Check for indicator variable
                    let indicator = if chars.peek() == Some(&':') {
                        chars.next(); // consume ':'
                        current_pos += 1;
                        let mut ind_name = String::new();
                        while let Some(&nc) = chars.peek() {
                            if nc.is_alphanumeric() || nc == '-' || nc == '_' {
                                let ch = chars.next().unwrap();
                                current_pos += ch.len_utf8();
                                ind_name.push(ch);
                            } else {
                                break;
                            }
                        }
                        if !ind_name.is_empty() {
                            Some(ind_name)
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    // Determine usage based on position and statement type
                    let usage = match stmt_type {
                        SqlStatementType::SelectInto | SqlStatementType::Fetch => {
                            // Variables between INTO and FROM are outputs
                            // Variables after WHERE are inputs
                            if let Some(into) = into_pos {
                                if let Some(into_end_pos) = into_end {
                                    // Between INTO and FROM = output
                                    if var_start_pos > into && var_start_pos < into_end_pos {
                                        HostVariableUsage::Output
                                    } else {
                                    // After FROM or in WHERE clause = input
                                    HostVariableUsage::Input
                                }
                                } else if var_start_pos > into {
                                    // No FROM, so everything after INTO is output
                                    HostVariableUsage::Output
                                } else {
                                    HostVariableUsage::Input
                                }
                            } else {
                                HostVariableUsage::Output
                            }
                        }
                        SqlStatementType::Insert
                        | SqlStatementType::Update
                        | SqlStatementType::Delete => HostVariableUsage::Input,
                        _ => HostVariableUsage::Input,
                    };

                    vars.push(HostVariable {
                        name,
                        indicator,
                        statement_number: stmt_num,
                        usage,
                    });
                }
            }
        }

        vars
    }

    /// Generate COBOL CALL statement to replace EXEC SQL.
    fn generate_call(&self, stmt_num: usize, stmt_type: SqlStatementType, _sql: &str) -> String {
        let call_name = match stmt_type {
            SqlStatementType::SelectInto => "SQLSELECT",
            SqlStatementType::Insert => "SQLINSERT",
            SqlStatementType::Update => "SQLUPDATE",
            SqlStatementType::Delete => "SQLDELETE",
            SqlStatementType::Open => "SQLOPEN",
            SqlStatementType::Fetch => "SQLFETCH",
            SqlStatementType::Close => "SQLCLOSE",
            SqlStatementType::Commit => "SQLCOMMIT",
            SqlStatementType::Rollback => "SQLROLLBK",
            SqlStatementType::Prepare => "SQLPREP",
            SqlStatementType::Execute => "SQLEXECP",
            SqlStatementType::ExecuteImmediate => "SQLEXECI",
            SqlStatementType::Describe => "SQLDESCR",
            _ => "SQLEXEC",
        };

        format!(
            "           CALL \"{call_name}\" USING SQL-STMT-{stmt_num:03}\n\
             {pad}SQLCA",
            pad = "                                    "
        )
    }

    /// Replace EXEC SQL block with generated CALL.
    fn replace_block(&self, lines: &mut Vec<String>, block: &SqlBlock, replacement: &str) {
        // Remove original lines
        let start = block.start_line.saturating_sub(1);
        let end = block.end_line.min(lines.len());

        // Remove the block lines
        for _ in start..end {
            if start < lines.len() {
                lines.remove(start);
            }
        }

        // Insert replacement at start position
        for (i, line) in replacement.lines().enumerate() {
            lines.insert(start + i, line.to_string());
        }
    }
}

impl Default for SqlPreprocessor {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_statement_type_detection() {
        assert_eq!(
            SqlStatementType::from_sql("SELECT NAME INTO :WS-NAME FROM T"),
            SqlStatementType::SelectInto
        );
        assert_eq!(
            SqlStatementType::from_sql("INSERT INTO T VALUES (:V1)"),
            SqlStatementType::Insert
        );
        assert_eq!(
            SqlStatementType::from_sql("UPDATE T SET X = :V WHERE Y = :W"),
            SqlStatementType::Update
        );
        assert_eq!(
            SqlStatementType::from_sql("DELETE FROM T WHERE X = :V"),
            SqlStatementType::Delete
        );
        assert_eq!(
            SqlStatementType::from_sql("DECLARE C1 CURSOR FOR SELECT * FROM T"),
            SqlStatementType::DeclareCursor
        );
        assert_eq!(
            SqlStatementType::from_sql("OPEN C1"),
            SqlStatementType::Open
        );
        assert_eq!(
            SqlStatementType::from_sql("FETCH C1 INTO :V1"),
            SqlStatementType::Fetch
        );
        assert_eq!(
            SqlStatementType::from_sql("CLOSE C1"),
            SqlStatementType::Close
        );
        assert_eq!(
            SqlStatementType::from_sql("COMMIT"),
            SqlStatementType::Commit
        );
        assert_eq!(
            SqlStatementType::from_sql("ROLLBACK"),
            SqlStatementType::Rollback
        );
    }

    #[test]
    fn test_host_variable_extraction() {
        let preprocessor = SqlPreprocessor::new();
        let sql = "SELECT NAME INTO :WS-NAME FROM CUSTOMER WHERE ID = :WS-ID";
        let vars = preprocessor.extract_host_variables(sql, 1, SqlStatementType::SelectInto);

        assert_eq!(vars.len(), 2);
        assert_eq!(vars[0].name, "WS-NAME");
        assert_eq!(vars[0].usage, HostVariableUsage::Output);
        assert_eq!(vars[1].name, "WS-ID");
        assert_eq!(vars[1].usage, HostVariableUsage::Input);
    }

    #[test]
    fn test_host_variable_with_indicator() {
        let preprocessor = SqlPreprocessor::new();
        let sql = "SELECT NAME INTO :WS-NAME:WS-NAME-IND FROM T";
        let vars = preprocessor.extract_host_variables(sql, 1, SqlStatementType::SelectInto);

        assert_eq!(vars.len(), 1);
        assert_eq!(vars[0].name, "WS-NAME");
        assert_eq!(vars[0].indicator, Some("WS-NAME-IND".to_string()));
    }

    #[test]
    fn test_dynamic_sql_type_detection() {
        assert_eq!(
            SqlStatementType::from_sql("PREPARE STMT1 FROM :SQL-TEXT"),
            SqlStatementType::Prepare
        );
        assert_eq!(
            SqlStatementType::from_sql("EXECUTE STMT1 USING :DEPT-NO"),
            SqlStatementType::Execute
        );
        assert_eq!(
            SqlStatementType::from_sql("EXECUTE IMMEDIATE :DDL-TEXT"),
            SqlStatementType::ExecuteImmediate
        );
        assert_eq!(
            SqlStatementType::from_sql("DESCRIBE STMT1 INTO :SQLDA"),
            SqlStatementType::Describe
        );
    }

    #[test]
    fn test_prepare_host_variable_extraction() {
        let preprocessor = SqlPreprocessor::new();
        let sql = "PREPARE S1 FROM :SQL-VAR";
        let vars = preprocessor.extract_host_variables(sql, 1, SqlStatementType::Prepare);
        assert_eq!(vars.len(), 1);
        assert_eq!(vars[0].name, "SQL-VAR");
    }

    #[test]
    fn test_execute_using_extraction() {
        let preprocessor = SqlPreprocessor::new();
        let sql = "EXECUTE STMT1 USING :DEPT-NO, :EMP-NAME";
        let vars = preprocessor.extract_host_variables(sql, 1, SqlStatementType::Execute);
        assert_eq!(vars.len(), 2);
        assert_eq!(vars[0].name, "DEPT-NO");
        assert_eq!(vars[1].name, "EMP-NAME");
    }

    #[test]
    fn test_execute_immediate_extraction() {
        let preprocessor = SqlPreprocessor::new();
        let sql = "EXECUTE IMMEDIATE :DDL-TEXT";
        let vars = preprocessor.extract_host_variables(sql, 1, SqlStatementType::ExecuteImmediate);
        assert_eq!(vars.len(), 1);
        assert_eq!(vars[0].name, "DDL-TEXT");
    }

    #[test]
    fn test_prepare_generates_sqlprep_call() {
        let source = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
           EXEC SQL
             PREPARE STMT1 FROM :SQL-TEXT
           END-EXEC.
           STOP RUN."#;

        let mut preprocessor = SqlPreprocessor::new();
        let result = preprocessor.process(source).unwrap();

        assert_eq!(result.sql_statements.len(), 1);
        assert_eq!(result.sql_statements[0].stmt_type, SqlStatementType::Prepare);
        assert!(result.cobol_source.contains("CALL \"SQLPREP\""));
    }

    #[test]
    fn test_execute_immediate_generates_sqlexeci_call() {
        let source = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
           EXEC SQL
             EXECUTE IMMEDIATE :DDL-TEXT
           END-EXEC.
           STOP RUN."#;

        let mut preprocessor = SqlPreprocessor::new();
        let result = preprocessor.process(source).unwrap();

        assert_eq!(result.sql_statements.len(), 1);
        assert_eq!(result.sql_statements[0].stmt_type, SqlStatementType::ExecuteImmediate);
        assert!(result.cobol_source.contains("CALL \"SQLEXECI\""));
    }

    #[test]
    fn test_preprocessor_basic() {
        let source = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC X(30).
       PROCEDURE DIVISION.
           EXEC SQL
             SELECT NAME INTO :WS-NAME
             FROM CUSTOMER
           END-EXEC.
           STOP RUN."#;

        let mut preprocessor = SqlPreprocessor::new();
        let result = preprocessor.process(source).unwrap();

        assert_eq!(result.sql_statements.len(), 1);
        assert_eq!(result.sql_statements[0].stmt_type, SqlStatementType::SelectInto);
        assert!(result.cobol_source.contains("CALL \"SQLSELECT\""));
    }

    // --- WHENEVER Tests (Epic 301) ---

    #[test]
    fn test_whenever_state_sqlerror_goto() {
        let mut state = WheneverState::default();
        state.apply("WHENEVER SQLERROR GO TO ERR-PARA");
        assert_eq!(state.sqlerror, Some(WheneverAction::GoTo("ERR-PARA".to_string())));
    }

    #[test]
    fn test_whenever_state_not_found_goto() {
        let mut state = WheneverState::default();
        state.apply("WHENEVER NOT FOUND GO TO EOF-PARA");
        assert_eq!(state.not_found, Some(WheneverAction::GoTo("EOF-PARA".to_string())));
    }

    #[test]
    fn test_whenever_state_sqlwarning_continue() {
        let mut state = WheneverState::default();
        state.apply("WHENEVER SQLWARNING CONTINUE");
        assert_eq!(state.sqlwarning, Some(WheneverAction::Continue));
    }

    #[test]
    fn test_whenever_state_goto_single_word() {
        let mut state = WheneverState::default();
        state.apply("WHENEVER SQLERROR GOTO ERRPARA");
        assert_eq!(state.sqlerror, Some(WheneverAction::GoTo("ERRPARA".to_string())));
    }

    #[test]
    fn test_whenever_override() {
        let mut state = WheneverState::default();
        state.apply("WHENEVER SQLERROR GO TO ERR1");
        assert_eq!(state.sqlerror, Some(WheneverAction::GoTo("ERR1".to_string())));

        state.apply("WHENEVER SQLERROR GO TO ERR2");
        assert_eq!(state.sqlerror, Some(WheneverAction::GoTo("ERR2".to_string())));
    }

    #[test]
    fn test_whenever_generate_checks_sqlerror() {
        let mut state = WheneverState::default();
        state.sqlerror = Some(WheneverAction::GoTo("ERR-PARA".to_string()));
        let checks = state.generate_checks();
        assert!(checks.contains("IF SQLCODE < 0 GO TO ERR-PARA"));
    }

    #[test]
    fn test_whenever_generate_checks_not_found() {
        let mut state = WheneverState::default();
        state.not_found = Some(WheneverAction::GoTo("EOF-PARA".to_string()));
        let checks = state.generate_checks();
        assert!(checks.contains("IF SQLCODE = 100 GO TO EOF-PARA"));
    }

    #[test]
    fn test_whenever_generate_checks_continue_produces_nothing() {
        let mut state = WheneverState::default();
        state.sqlerror = Some(WheneverAction::Continue);
        state.not_found = Some(WheneverAction::Continue);
        let checks = state.generate_checks();
        assert!(checks.is_empty());
    }

    #[test]
    fn test_whenever_in_preprocessor_emits_if_checks() {
        let source = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
           EXEC SQL
             WHENEVER SQLERROR GO TO ERR-PARA
           END-EXEC.
           EXEC SQL
             SELECT NAME INTO :WS-NAME FROM T
           END-EXEC.
           STOP RUN."#;

        let mut preprocessor = SqlPreprocessor::new();
        let result = preprocessor.process(source).unwrap();

        // The SELECT should have an IF SQLCODE < 0 check after the CALL
        assert!(result.cobol_source.contains("CALL \"SQLSELECT\""));
        assert!(result.cobol_source.contains("IF SQLCODE < 0 GO TO ERR-PARA"));
    }

    #[test]
    fn test_whenever_scope_overrides() {
        let source = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
           EXEC SQL
             WHENEVER SQLERROR GO TO ERR1
           END-EXEC.
           EXEC SQL
             INSERT INTO T VALUES (:V1)
           END-EXEC.
           EXEC SQL
             WHENEVER SQLERROR GO TO ERR2
           END-EXEC.
           EXEC SQL
             DELETE FROM T WHERE X = :V2
           END-EXEC.
           STOP RUN."#;

        let mut preprocessor = SqlPreprocessor::new();
        let result = preprocessor.process(source).unwrap();

        let lines: Vec<&str> = result.cobol_source.lines().collect();

        // Find INSERT CALL and verify it jumps to ERR1
        let insert_pos = lines.iter().position(|l| l.contains("SQLINSERT")).unwrap();
        let insert_check = lines[insert_pos + 2]; // SQLCA line, then IF check
        assert!(insert_check.contains("GO TO ERR1"), "INSERT should jump to ERR1, got: {}", insert_check);

        // Find DELETE CALL and verify it jumps to ERR2
        let delete_pos = lines.iter().position(|l| l.contains("SQLDELETE")).unwrap();
        let delete_check = lines[delete_pos + 2];
        assert!(delete_check.contains("GO TO ERR2"), "DELETE should jump to ERR2, got: {}", delete_check);
    }

    #[test]
    fn test_whenever_continue_disables_check() {
        let source = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
           EXEC SQL
             WHENEVER NOT FOUND GO TO EOF-PARA
           END-EXEC.
           EXEC SQL
             FETCH C1 INTO :V1
           END-EXEC.
           EXEC SQL
             WHENEVER NOT FOUND CONTINUE
           END-EXEC.
           EXEC SQL
             FETCH C1 INTO :V2
           END-EXEC.
           STOP RUN."#;

        let mut preprocessor = SqlPreprocessor::new();
        let result = preprocessor.process(source).unwrap();

        let src = &result.cobol_source;

        // First FETCH should have NOT FOUND check
        let first_fetch = src.find("SQLFETCH").unwrap();
        let after_first = &src[first_fetch..];
        let second_fetch_offset = after_first[1..].find("SQLFETCH").unwrap() + 1;

        // Text between first SQLFETCH and second SQLFETCH should contain the check
        let between = &after_first[..second_fetch_offset];
        assert!(between.contains("IF SQLCODE = 100 GO TO EOF-PARA"));

        // Text after second SQLFETCH should NOT contain the check
        let after_second = &after_first[second_fetch_offset..];
        assert!(!after_second.contains("IF SQLCODE = 100 GO TO EOF-PARA"));
    }

    #[test]
    fn test_whenever_no_call_for_whenever_itself() {
        let source = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
           EXEC SQL
             WHENEVER SQLERROR GO TO ERR-PARA
           END-EXEC.
           STOP RUN."#;

        let mut preprocessor = SqlPreprocessor::new();
        let result = preprocessor.process(source).unwrap();

        // WHENEVER should NOT generate a runtime CALL
        assert!(!result.cobol_source.contains("CALL"));
        // It should have a comment showing the directive
        assert!(result.cobol_source.contains("* WHENEVER"));
    }
}
