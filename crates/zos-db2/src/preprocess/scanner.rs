//! EXEC SQL block scanner for COBOL source.
//!
//! Extracts EXEC SQL ... END-EXEC blocks from COBOL source code,
//! handling continuation lines and comments.

use crate::{Db2Error, Db2Result};

/// A scanned SQL block from COBOL source.
#[derive(Debug, Clone)]
pub struct SqlBlock {
    /// The SQL text (without EXEC SQL / END-EXEC markers)
    pub sql: String,
    /// Starting line number (1-based)
    pub start_line: usize,
    /// Ending line number (1-based)
    pub end_line: usize,
    /// Column where EXEC SQL started
    pub start_column: usize,
}

/// Scanner for EXEC SQL blocks in COBOL source.
pub struct SqlScanner {
    /// Current line being processed
    current_line: usize,
    /// Whether we're inside an EXEC SQL block
    in_sql_block: bool,
    /// Accumulated SQL text
    sql_buffer: String,
    /// Start line of current block
    block_start_line: usize,
    /// Start column of current block
    block_start_column: usize,
}

impl SqlScanner {
    /// Create a new scanner.
    pub fn new() -> Self {
        Self {
            current_line: 0,
            in_sql_block: false,
            sql_buffer: String::new(),
            block_start_line: 0,
            block_start_column: 0,
        }
    }

    /// Scan COBOL source for EXEC SQL blocks.
    pub fn scan(&mut self, source: &str) -> Db2Result<Vec<SqlBlock>> {
        let mut blocks = Vec::new();
        self.reset();

        for (line_num, line) in source.lines().enumerate() {
            self.current_line = line_num + 1;

            // Skip sequence number area (columns 1-6) and check indicator (column 7)
            let content = if line.len() > 6 {
                let indicator = line.chars().nth(6).unwrap_or(' ');

                // Skip comment lines
                if indicator == '*' || indicator == '/' {
                    if self.in_sql_block {
                        // Comments inside SQL block are ignored
                    }
                    continue;
                }

                // Handle continuation lines
                if indicator == '-' && self.in_sql_block {
                    // Continuation: append content starting from column 12
                    if line.len() > 11 {
                        &line[11..]
                    } else {
                        ""
                    }
                } else {
                    // Normal line: columns 8-72 (Area A + B)
                    let end = line.len().min(72);
                    if line.len() > 7 {
                        &line[7..end]
                    } else {
                        ""
                    }
                }
            } else {
                line
            };

            // Process the content
            if let Some(block) = self.process_line(content)? {
                blocks.push(block);
            }
        }

        // Check for unclosed EXEC SQL block
        if self.in_sql_block {
            return Err(Db2Error::SyntaxError {
                line: self.block_start_line,
                message: "Unclosed EXEC SQL block (missing END-EXEC)".to_string(),
            });
        }

        Ok(blocks)
    }

    /// Reset scanner state.
    fn reset(&mut self) {
        self.current_line = 0;
        self.in_sql_block = false;
        self.sql_buffer.clear();
        self.block_start_line = 0;
        self.block_start_column = 0;
    }

    /// Process a single line of content.
    fn process_line(&mut self, content: &str) -> Db2Result<Option<SqlBlock>> {
        let upper = content.to_uppercase();

        if !self.in_sql_block {
            // Look for EXEC SQL
            if let Some(pos) = upper.find("EXEC SQL") {
                self.in_sql_block = true;
                self.block_start_line = self.current_line;
                self.block_start_column = pos + 8; // Position after "EXEC SQL"

                // Get content after EXEC SQL
                let after_exec = &content[pos + 8..];

                // Check if END-EXEC is on the same line
                if let Some(end_pos) = after_exec.to_uppercase().find("END-EXEC") {
                    let sql = after_exec[..end_pos].trim().to_string();
                    self.in_sql_block = false;

                    return Ok(Some(SqlBlock {
                        sql,
                        start_line: self.block_start_line,
                        end_line: self.current_line,
                        start_column: self.block_start_column,
                    }));
                } else {
                    // SQL continues on next lines
                    self.sql_buffer = after_exec.trim().to_string();
                }
            }
        } else {
            // Inside SQL block, look for END-EXEC
            if let Some(end_pos) = upper.find("END-EXEC") {
                // Add content before END-EXEC
                let before_end = content[..end_pos].trim();
                if !before_end.is_empty() {
                    if !self.sql_buffer.is_empty() {
                        self.sql_buffer.push(' ');
                    }
                    self.sql_buffer.push_str(before_end);
                }

                let block = SqlBlock {
                    sql: self.normalize_sql(&self.sql_buffer),
                    start_line: self.block_start_line,
                    end_line: self.current_line,
                    start_column: self.block_start_column,
                };

                self.in_sql_block = false;
                self.sql_buffer.clear();

                return Ok(Some(block));
            } else {
                // Accumulate SQL content
                let trimmed = content.trim();
                if !trimmed.is_empty() {
                    if !self.sql_buffer.is_empty() {
                        self.sql_buffer.push(' ');
                    }
                    self.sql_buffer.push_str(trimmed);
                }
            }
        }

        Ok(None)
    }

    /// Normalize SQL text (collapse whitespace, etc.)
    fn normalize_sql(&self, sql: &str) -> String {
        // Collapse multiple spaces to single space
        let mut result = String::with_capacity(sql.len());
        let mut last_was_space = false;

        for c in sql.chars() {
            if c.is_whitespace() {
                if !last_was_space {
                    result.push(' ');
                    last_was_space = true;
                }
            } else {
                result.push(c);
                last_was_space = false;
            }
        }

        result.trim().to_string()
    }
}

impl Default for SqlScanner {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_line_exec_sql() {
        let source = "       EXEC SQL SELECT * FROM T END-EXEC.";
        let mut scanner = SqlScanner::new();
        let blocks = scanner.scan(source).unwrap();

        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].sql, "SELECT * FROM T");
        assert_eq!(blocks[0].start_line, 1);
        assert_eq!(blocks[0].end_line, 1);
    }

    #[test]
    fn test_multi_line_exec_sql() {
        let source = r#"       EXEC SQL
           SELECT NAME, ADDRESS
           INTO :WS-NAME, :WS-ADDR
           FROM CUSTOMER
           WHERE ID = :WS-ID
       END-EXEC."#;

        let mut scanner = SqlScanner::new();
        let blocks = scanner.scan(source).unwrap();

        assert_eq!(blocks.len(), 1);
        assert!(blocks[0].sql.contains("SELECT NAME"));
        assert!(blocks[0].sql.contains("INTO :WS-NAME"));
        assert!(blocks[0].sql.contains("WHERE ID = :WS-ID"));
        assert_eq!(blocks[0].start_line, 1);
        assert_eq!(blocks[0].end_line, 6);
    }

    #[test]
    fn test_multiple_exec_sql_blocks() {
        let source = r#"       EXEC SQL SELECT A INTO :V1 FROM T1 END-EXEC.
       MOVE V1 TO V2.
       EXEC SQL INSERT INTO T2 VALUES (:V2) END-EXEC."#;

        let mut scanner = SqlScanner::new();
        let blocks = scanner.scan(source).unwrap();

        assert_eq!(blocks.len(), 2);
        assert!(blocks[0].sql.contains("SELECT A"));
        assert!(blocks[1].sql.contains("INSERT INTO T2"));
    }

    #[test]
    fn test_skip_comment_lines() {
        let source = r#"       EXEC SQL
      *    This is a comment
           SELECT NAME FROM T
       END-EXEC."#;

        let mut scanner = SqlScanner::new();
        let blocks = scanner.scan(source).unwrap();

        assert_eq!(blocks.len(), 1);
        assert!(blocks[0].sql.contains("SELECT NAME FROM T"));
        assert!(!blocks[0].sql.contains("comment"));
    }

    #[test]
    fn test_unclosed_exec_sql() {
        let source = r#"       EXEC SQL
           SELECT * FROM T"#;

        let mut scanner = SqlScanner::new();
        let result = scanner.scan(source);

        assert!(result.is_err());
        if let Err(Db2Error::SyntaxError { line, message }) = result {
            assert_eq!(line, 1);
            assert!(message.contains("Unclosed"));
        }
    }

    #[test]
    fn test_exec_sql_with_into_clause() {
        let source = "       EXEC SQL SELECT NAME INTO :WS-NAME FROM CUST END-EXEC.";

        let mut scanner = SqlScanner::new();
        let blocks = scanner.scan(source).unwrap();

        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].sql, "SELECT NAME INTO :WS-NAME FROM CUST");
    }

    #[test]
    fn test_declare_cursor() {
        let source = r#"       EXEC SQL
           DECLARE C1 CURSOR FOR
           SELECT * FROM CUSTOMER
           WHERE STATUS = 'A'
       END-EXEC."#;

        let mut scanner = SqlScanner::new();
        let blocks = scanner.scan(source).unwrap();

        assert_eq!(blocks.len(), 1);
        assert!(blocks[0].sql.contains("DECLARE C1 CURSOR"));
        assert!(blocks[0].sql.contains("SELECT * FROM CUSTOMER"));
    }

    #[test]
    fn test_whitespace_normalization() {
        let source = r#"       EXEC SQL
           SELECT    A,    B
           FROM      T
       END-EXEC."#;

        let mut scanner = SqlScanner::new();
        let blocks = scanner.scan(source).unwrap();

        assert_eq!(blocks.len(), 1);
        // Multiple spaces should be collapsed
        assert!(!blocks[0].sql.contains("    "));
    }

    #[test]
    fn test_include_sqlca() {
        let source = "       EXEC SQL INCLUDE SQLCA END-EXEC.";

        let mut scanner = SqlScanner::new();
        let blocks = scanner.scan(source).unwrap();

        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].sql, "INCLUDE SQLCA");
    }
}
