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

/// SQL preprocessor for COBOL programs.
pub struct SqlPreprocessor {
    scanner: SqlScanner,
    statements: Vec<SqlStatement>,
    host_variables: Vec<HostVariable>,
    statement_counter: usize,
}

impl SqlPreprocessor {
    /// Create a new preprocessor.
    pub fn new() -> Self {
        Self {
            scanner: SqlScanner::new(),
            statements: Vec::new(),
            host_variables: Vec::new(),
            statement_counter: 0,
        }
    }

    /// Process COBOL source and extract SQL.
    pub fn process(&mut self, source: &str) -> Db2Result<PreprocessResult> {
        // Scan for EXEC SQL blocks
        let blocks = self.scanner.scan(source)?;

        // Process each block
        let mut cobol_lines: Vec<String> = source.lines().map(|s| s.to_string()).collect();

        for block in blocks.iter().rev() {
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

            // Generate COBOL CALL replacement
            let call_code = self.generate_call(stmt_num, stmt_type, &block.sql);

            // Replace EXEC SQL block with CALL
            self.replace_block(&mut cobol_lines, block, &call_code);
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
}
