//! JCL lexer.
//!
//! JCL has a column-based format:
//! - Columns 1-2: // for JCL statements, /* for comments, //* for comment statements
//! - Column 3: blank for continuation, name for new statement
//! - Columns 4-71: statement content
//! - Column 72: continuation marker (non-blank)
//! - Columns 73-80: sequence number (optional)

pub mod scanner;
pub mod token;

pub use scanner::tokenize_operands;
pub use token::{JclStatement, Token};

use crate::error::JclError;
use open_mainframe_lang_core::PreprocessedSource;

/// JCL lexer.
pub struct Lexer {
    /// Preprocessed source (normalized line endings + line index).
    preprocessed: PreprocessedSource,
    /// Input lines (slices into the normalized text).
    lines: Vec<String>,
    /// Byte offset of each line in the normalized source.
    line_offsets: Vec<u32>,
    /// Current line index.
    current_line: usize,
    /// In inline data mode.
    in_inline_data: bool,
    /// Inline data delimiter.
    inline_delimiter: Option<String>,
}

impl Lexer {
    /// Create a new lexer for the given JCL source.
    pub fn new(source: &str) -> Self {
        let preprocessed = PreprocessedSource::new(source);
        let lines: Vec<String> = preprocessed.text.lines().map(|l| l.to_string()).collect();
        let line_offsets: Vec<u32> = (0..lines.len())
            .map(|i| preprocessed.line_index.line_start(i).unwrap_or(0))
            .collect();
        Self {
            preprocessed,
            lines,
            line_offsets,
            current_line: 0,
            in_inline_data: false,
            inline_delimiter: None,
        }
    }

    /// Get the preprocessed (normalized) source text.
    pub fn source(&self) -> &str {
        &self.preprocessed.text
    }

    /// Get the byte offset of the start of the given line index.
    fn line_byte_offset(&self, line_idx: usize) -> u32 {
        self.line_offsets.get(line_idx).copied().unwrap_or(self.preprocessed.text.len() as u32)
    }

    /// Get the byte offset of the end of the given line index (exclusive, includes line content).
    fn line_byte_end(&self, line_idx: usize) -> u32 {
        self.line_byte_offset(line_idx) + self.lines.get(line_idx).map(|l| l.len() as u32).unwrap_or(0)
    }

    /// Parse all JCL statements from the source.
    pub fn parse_statements(&mut self) -> Result<Vec<JclStatement>, JclError> {
        let mut statements: Vec<JclStatement> = Vec::new();

        while self.current_line < self.lines.len() {
            let line = &self.lines[self.current_line];

            // Check for inline data end and collect inline data lines
            if self.in_inline_data {
                if self.is_inline_data_end(line) {
                    // Explicit delimiter (/* or custom DLM=) — consume it
                    self.in_inline_data = false;
                    self.inline_delimiter = None;
                    self.current_line += 1;
                    continue;
                }
                // In z/OS JCL, DD * inline data is also terminated by any
                // JCL statement (line starting with //) unless a custom
                // DLM= delimiter was specified.
                if self.inline_delimiter.is_none() && line.starts_with("//") {
                    self.in_inline_data = false;
                    // Fall through to parse this line as a JCL statement
                } else {
                    // Collect inline data line and attach to previous DD statement
                    if let Some(prev_stmt) = statements.last_mut() {
                        prev_stmt.inline_data.push(line.to_string());
                    }
                    self.current_line += 1;
                    continue;
                }
            }

            // Skip empty lines
            if line.trim().is_empty() {
                self.current_line += 1;
                continue;
            }

            // Check line type
            if line.starts_with("//*") {
                // Comment line
                self.current_line += 1;
                continue;
            }

            if line.starts_with("/*") {
                // End of JCL or inline data end
                self.current_line += 1;
                continue;
            }

            if !line.starts_with("//") {
                // Not a JCL line, could be inline data
                self.current_line += 1;
                continue;
            }

            // Parse JCL statement
            if let Some(stmt) = self.parse_statement()? {
                // Check if this starts inline data
                // Inline data is indicated by:
                // - DD * (just asterisk)
                // - DD *,... (asterisk with params like DLM=)
                // NOT by SYSOUT=* which means output class
                if stmt.operation.eq_ignore_ascii_case("DD") {
                    let trimmed = stmt.operands.trim();
                    let is_inline =
                        trimmed == "*" || trimmed.starts_with("*,") || trimmed.starts_with("* ");
                    if is_inline {
                        self.in_inline_data = true;
                        // Check for DLM= parameter
                        let operands_upper = stmt.operands.to_uppercase();
                        if let Some(dlm_pos) = operands_upper.find("DLM=") {
                            let dlm_start = dlm_pos + 4;
                            let dlm_end = stmt.operands[dlm_start..]
                                .find([',', ' ', ')'])
                                .map(|p| dlm_start + p)
                                .unwrap_or(stmt.operands.len());
                            self.inline_delimiter =
                                Some(stmt.operands[dlm_start..dlm_end].to_string());
                        }
                    }
                }
                statements.push(stmt);
            }
        }

        Ok(statements)
    }

    /// Parse a single JCL statement (may span multiple lines due to continuation).
    fn parse_statement(&mut self) -> Result<Option<JclStatement>, JclError> {
        let start_line_idx = self.current_line;
        let start_line = self.current_line as u32 + 1;
        let byte_offset = self.line_byte_offset(start_line_idx);
        let first_line = self.lines[self.current_line].clone();
        self.current_line += 1;

        // Must start with //
        if !first_line.starts_with("//") {
            return Ok(None);
        }

        // Strip columns 72-80 (continuation marker + sequence numbers).
        // JCL columns 1-71 are the statement content.
        let effective_line = if first_line.len() > 71 { &first_line[..71] } else { &first_line };

        // Get content after //
        let content = &effective_line[2..];

        // Check for null statement
        if content.trim().is_empty() {
            let byte_end = self.line_byte_end(start_line_idx);
            return Ok(Some(JclStatement {
                name: None,
                operation: "NULL".to_string(),
                operands: String::new(),
                line: start_line,
                byte_offset,
                inline_data: Vec::new(),
                byte_end,
            }));
        }

        // Parse name field (starts at column 3, i.e., index 0 of content)
        let (name, rest) = Self::parse_name_field(content);

        // Parse operation and operands
        let trimmed = rest.trim_start();
        if trimmed.is_empty() {
            return Err(JclError::ParseError {
                message: format!("Missing operation on line {}", start_line),
            });
        }

        // Split into operation and operands
        let (operation, operands) = Self::split_operation_operands(trimmed);

        // Handle continuation lines
        let mut full_operands = operands.to_string();
        let mut last_line_idx = start_line_idx;
        while self.is_continued(&full_operands) && self.current_line < self.lines.len() {
            let cont_line = &self.lines[self.current_line];
            if !cont_line.starts_with("//") || cont_line.starts_with("//*") {
                break;
            }

            // Strip columns 72-80 from continuation lines too.
            let cont_effective = if cont_line.len() > 71 { &cont_line[..71] } else { cont_line };
            let cont_content = &cont_effective[2..];
            // Continuation has blank in column 3 (name field)
            if !cont_content.starts_with(' ') {
                break;
            }

            // Remove trailing comma from previous and append continuation.
            // Strip JCL comment field from the continuation content:
            // after a comma at paren-depth-0, a blank starts a comment.
            let cont_trimmed = Self::strip_operand_comment(cont_content.trim());
            if full_operands.ends_with(',') {
                full_operands.pop();
            }
            full_operands.push(',');
            full_operands.push_str(cont_trimmed);
            last_line_idx = self.current_line;
            self.current_line += 1;
        }

        let byte_end = self.line_byte_end(last_line_idx);

        Ok(Some(JclStatement {
            name,
            operation: operation.to_uppercase(),
            operands: full_operands,
            line: start_line,
            byte_offset,
            byte_end,
            inline_data: Vec::new(),
        }))
    }

    /// Parse the name field from JCL content.
    fn parse_name_field(content: &str) -> (Option<String>, &str) {
        // Name starts at column 3 (index 0) and is 1-8 characters
        // If column 3 is blank, there's no name
        if content.is_empty() || content.starts_with(' ') {
            return (None, content.trim_start());
        }

        // Find end of name (first space or end of valid name chars).
        // Allow dots for DD override names like STEPNAME.DDNAME (max 8+1+8 = 17).
        let name_end = content
            .find(|c: char| !c.is_ascii_alphanumeric() && c != '@' && c != '#' && c != '$' && c != '.')
            .unwrap_or(content.len())
            .min(17); // Max 17 characters (STEPNAME.DDNAME)

        let name = &content[..name_end];
        let rest = &content[name_end..];

        (Some(name.to_string()), rest)
    }

    /// Split operation from operands.
    fn split_operation_operands(content: &str) -> (&str, &str) {
        let op_end = content
            .find(|c: char| c.is_whitespace())
            .unwrap_or(content.len());

        let operation = &content[..op_end];
        let operands = content[op_end..].trim_start().trim_end();

        (operation, operands)
    }

    /// Check if a line is continued (ends with comma, or has continuation char in col 72).
    fn is_continued(&self, operands: &str) -> bool {
        operands.trim_end().ends_with(',')
    }

    /// Strip JCL comment field from operand text on a continuation line.
    ///
    /// In z/OS JCL, after the operand field on a statement line, a blank
    /// followed by any text is a comment. The operand field ends when
    /// parentheses are balanced and a comma followed by a blank is found.
    fn strip_operand_comment(operand: &str) -> &str {
        let mut depth: i32 = 0;
        let mut in_quote = false;
        let bytes = operand.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            let c = bytes[i] as char;
            if in_quote {
                if c == '\'' {
                    if i + 1 < bytes.len() && bytes[i + 1] == b'\'' {
                        i += 2; // skip escaped quote
                        continue;
                    }
                    in_quote = false;
                }
                i += 1;
                continue;
            }
            match c {
                '\'' => in_quote = true,
                '(' => depth += 1,
                ')' => {
                    depth -= 1;
                    if depth < 0 {
                        depth = 0;
                    }
                }
                ',' if depth == 0 => {
                    // Comma at top level — if next char is blank, rest is comment
                    if i + 1 < bytes.len() && bytes[i + 1] == b' ' {
                        return &operand[..=i]; // include the comma
                    }
                }
                _ => {}
            }
            i += 1;
        }
        operand
    }

    /// Check if line ends inline data.
    fn is_inline_data_end(&self, line: &str) -> bool {
        if let Some(ref delim) = self.inline_delimiter {
            line.starts_with(delim)
        } else {
            line.starts_with("/*")
        }
    }

    /// Get all lines of inline data following current position.
    pub fn collect_inline_data(&mut self, delimiter: Option<&str>) -> Vec<String> {
        let mut data = Vec::new();
        let end_marker = delimiter.unwrap_or("/*");

        while self.current_line < self.lines.len() {
            let line = &self.lines[self.current_line];
            if line.starts_with(end_marker) {
                self.current_line += 1;
                break;
            }
            data.push(line.to_string());
            self.current_line += 1;
        }

        data
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_job_statement() {
        let jcl = "//TESTJOB  JOB (ACCT),'PROGRAMMER',CLASS=A";
        let mut lexer = Lexer::new(jcl);
        let statements = lexer.parse_statements().unwrap();

        assert_eq!(statements.len(), 1);
        assert_eq!(statements[0].name, Some("TESTJOB".to_string()));
        assert_eq!(statements[0].operation, "JOB");
    }

    #[test]
    fn test_parse_exec_statement() {
        let jcl = "//STEP1    EXEC PGM=MYPROG,PARM='TEST'";
        let mut lexer = Lexer::new(jcl);
        let statements = lexer.parse_statements().unwrap();

        assert_eq!(statements.len(), 1);
        assert_eq!(statements[0].name, Some("STEP1".to_string()));
        assert_eq!(statements[0].operation, "EXEC");
    }

    #[test]
    fn test_parse_dd_statement() {
        let jcl = "//INPUT    DD DSN=MY.DATA.SET,DISP=SHR";
        let mut lexer = Lexer::new(jcl);
        let statements = lexer.parse_statements().unwrap();

        assert_eq!(statements.len(), 1);
        assert_eq!(statements[0].name, Some("INPUT".to_string()));
        assert_eq!(statements[0].operation, "DD");
    }

    #[test]
    fn test_parse_multiple_statements() {
        let jcl = r#"//MYJOB    JOB (ACCT),CLASS=A
//STEP1    EXEC PGM=HELLO
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//"#;
        let mut lexer = Lexer::new(jcl);
        let statements = lexer.parse_statements().unwrap();

        assert_eq!(statements.len(), 5); // JOB, EXEC, DD, DD, NULL
    }

    #[test]
    fn test_skip_comments() {
        let jcl = r#"//* This is a comment
//MYJOB    JOB CLASS=A
//* Another comment
//STEP1    EXEC PGM=TEST"#;
        let mut lexer = Lexer::new(jcl);
        let statements = lexer.parse_statements().unwrap();

        // Should only have JOB and EXEC, not comments
        assert_eq!(statements.len(), 2);
        assert_eq!(statements[0].operation, "JOB");
        assert_eq!(statements[1].operation, "EXEC");
    }

    #[test]
    fn test_crlf_line_endings_produce_correct_offsets() {
        // This was the original bug: \r\n caused byte offset drift
        let jcl = "//JOB1    JOB CLASS=A\r\n//STEP1   EXEC PGM=SORT\r\n//DD1     DD *\r\n";
        let mut lexer = Lexer::new(jcl);
        let statements = lexer.parse_statements().unwrap();

        assert_eq!(statements.len(), 3);
        assert_eq!(statements[0].operation, "JOB");
        assert_eq!(statements[1].operation, "EXEC");
        assert_eq!(statements[2].operation, "DD");

        // After normalization, line offsets should be based on \n only.
        // Line 0: "//JOB1    JOB CLASS=A" (21 chars) + \n = offset 0
        // Line 1: "//STEP1   EXEC PGM=SORT" (23 chars) starts at offset 22
        // Line 2: "//DD1     DD *" (14 chars) starts at offset 46
        assert_eq!(statements[0].byte_offset, 0);
        assert_eq!(statements[1].byte_offset, 22);
        assert_eq!(statements[2].byte_offset, 46);

        // Verify the source text can be sliced at these offsets
        let src = lexer.source();
        assert!(src[statements[1].byte_offset as usize..].starts_with("//STEP1"));
        assert!(src[statements[2].byte_offset as usize..].starts_with("//DD1"));
    }

    #[test]
    fn test_sequence_number_stripping() {
        // JCL with sequence numbers in columns 73-80
        let jcl = "//STEP1    EXEC PGM=IEFBR14                                            00000100";
        let mut lexer = Lexer::new(jcl);
        let statements = lexer.parse_statements().unwrap();

        assert_eq!(statements.len(), 1);
        assert_eq!(statements[0].operation, "EXEC");
        // Operands should NOT contain the sequence number
        assert!(!statements[0].operands.contains("00000100"));
        assert!(statements[0].operands.contains("PGM=IEFBR14"));
    }

    #[test]
    fn test_sequence_number_stripping_continuation() {
        // Multi-line JCL with sequence numbers on both lines
        let jcl = "//STEP1    EXEC PGM=IEFBR14,                                           00000100\n//             REGION=4M                                                00000200";
        let mut lexer = Lexer::new(jcl);
        let statements = lexer.parse_statements().unwrap();

        assert_eq!(statements.len(), 1);
        assert_eq!(statements[0].operation, "EXEC");
        assert!(statements[0].operands.contains("PGM=IEFBR14"));
        assert!(statements[0].operands.contains("REGION=4M"));
        assert!(!statements[0].operands.contains("00000100"));
        assert!(!statements[0].operands.contains("00000200"));
    }

    #[test]
    fn test_dot_in_name_field() {
        // DD override with STEPNAME.DDNAME format
        let jcl = "//PRC001.FILEIN DD DSN=MY.DATA.SET,DISP=SHR";
        let mut lexer = Lexer::new(jcl);
        let statements = lexer.parse_statements().unwrap();

        assert_eq!(statements.len(), 1);
        assert_eq!(statements[0].name, Some("PRC001.FILEIN".to_string()));
        assert_eq!(statements[0].operation, "DD");
    }

    #[test]
    fn test_mixed_line_endings() {
        // Mix of \n and \r\n
        let jcl = "//JOB1 JOB CLASS=A\n//STEP1 EXEC PGM=TEST\r\n//DD1 DD SYSOUT=*\n";
        let mut lexer = Lexer::new(jcl);
        let statements = lexer.parse_statements().unwrap();

        assert_eq!(statements.len(), 3);
        // All offsets should still be accurate after normalization
        let src = lexer.source();
        for stmt in &statements {
            let slice = &src[stmt.byte_offset as usize..stmt.byte_end as usize];
            assert!(slice.starts_with("//"));
        }
    }
}
