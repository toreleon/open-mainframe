//! JCL lexer.
//!
//! JCL has a column-based format:
//! - Columns 1-2: // for JCL statements, /* for comments, //* for comment statements
//! - Column 3: blank for continuation, name for new statement
//! - Columns 4-71: statement content
//! - Column 72: continuation marker (non-blank)
//! - Columns 73-80: sequence number (optional)

use crate::error::JclError;

/// Token types for JCL.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    /// JOB statement.
    Job,
    /// EXEC statement.
    Exec,
    /// DD statement.
    Dd,
    /// SET statement.
    Set,
    /// IF statement.
    If,
    /// THEN keyword.
    Then,
    /// ELSE keyword.
    Else,
    /// ENDIF statement.
    Endif,
    /// INCLUDE statement.
    Include,
    /// JCLLIB statement.
    Jcllib,
    /// PROC statement.
    Proc,
    /// PEND statement.
    Pend,
    /// OUTPUT statement.
    Output,
    /// NULL statement (just //).
    Null,

    /// Identifier (name, keyword).
    Ident(String),
    /// String literal (in quotes).
    String(String),
    /// Numeric literal.
    Number(i64),

    /// = sign.
    Equals,
    /// , comma.
    Comma,
    /// ( open paren.
    LParen,
    /// ) close paren.
    RParen,
    /// . period.
    Period,
    /// * asterisk.
    Asterisk,
    /// & ampersand (symbolic parameter).
    Ampersand,

    /// Start of inline data.
    InlineDataStart,
    /// Inline data line.
    InlineData(String),
    /// End of inline data (/*).
    InlineDataEnd,

    /// Comment (//*, or /* ... */).
    Comment(String),

    /// End of statement.
    EndStatement,
    /// End of JCL.
    Eof,
}

/// JCL statement as parsed from source.
#[derive(Debug, Clone)]
pub struct JclStatement {
    /// The name field (columns 3-10 of first line).
    pub name: Option<String>,
    /// The operation (JOB, EXEC, DD, etc.).
    pub operation: String,
    /// The operands as a string (to be parsed further).
    pub operands: String,
    /// Line number in source.
    pub line: u32,
}

/// JCL lexer.
pub struct Lexer<'a> {
    /// Input lines.
    lines: Vec<&'a str>,
    /// Current line index.
    current_line: usize,
    /// In inline data mode.
    in_inline_data: bool,
    /// Inline data delimiter.
    inline_delimiter: Option<String>,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given JCL source.
    pub fn new(source: &'a str) -> Self {
        let lines: Vec<&str> = source.lines().collect();
        Self {
            lines,
            current_line: 0,
            in_inline_data: false,
            inline_delimiter: None,
        }
    }

    /// Parse all JCL statements from the source.
    pub fn parse_statements(&mut self) -> Result<Vec<JclStatement>, JclError> {
        let mut statements = Vec::new();

        while self.current_line < self.lines.len() {
            let line = self.lines[self.current_line];

            // Check for inline data end
            if self.in_inline_data {
                if self.is_inline_data_end(line) {
                    self.in_inline_data = false;
                    self.inline_delimiter = None;
                    self.current_line += 1;
                    continue;
                }
                // Skip inline data lines for now (they'll be collected by parser)
                self.current_line += 1;
                continue;
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
        let start_line = self.current_line as u32 + 1;
        let first_line = self.lines[self.current_line];
        self.current_line += 1;

        // Must start with //
        if !first_line.starts_with("//") {
            return Ok(None);
        }

        // Get content after //
        let content = &first_line[2..];

        // Check for null statement
        if content.trim().is_empty() {
            return Ok(Some(JclStatement {
                name: None,
                operation: "NULL".to_string(),
                operands: String::new(),
                line: start_line,
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
        while self.is_continued(&full_operands) && self.current_line < self.lines.len() {
            let cont_line = self.lines[self.current_line];
            if !cont_line.starts_with("//") || cont_line.starts_with("//*") {
                break;
            }

            let cont_content = &cont_line[2..];
            // Continuation has blank in column 3 (name field)
            if !cont_content.starts_with(' ') {
                break;
            }

            // Remove trailing comma from previous and append continuation
            if full_operands.ends_with(',') {
                full_operands.pop();
            }
            full_operands.push(',');
            full_operands.push_str(cont_content.trim());
            self.current_line += 1;
        }

        Ok(Some(JclStatement {
            name,
            operation: operation.to_uppercase(),
            operands: full_operands,
            line: start_line,
        }))
    }

    /// Parse the name field from JCL content.
    fn parse_name_field(content: &str) -> (Option<String>, &str) {
        // Name starts at column 3 (index 0) and is 1-8 characters
        // If column 3 is blank, there's no name
        if content.is_empty() || content.starts_with(' ') {
            return (None, content.trim_start());
        }

        // Find end of name (first space or end of valid name chars)
        let name_end = content
            .find(|c: char| !c.is_ascii_alphanumeric() && c != '@' && c != '#' && c != '$')
            .unwrap_or(content.len())
            .min(8); // Max 8 characters

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
        let operands = content[op_end..].trim_start();

        // Remove sequence number (columns 73-80) if present
        // Limit operand length to avoid sequence numbers
        let max_len = 71usize.saturating_sub(op_end);
        let operands = if operands.len() > max_len {
            operands[..max_len].trim_end()
        } else {
            operands.trim_end()
        };

        (operation, operands)
    }

    /// Check if a line is continued (ends with comma, or has continuation char in col 72).
    fn is_continued(&self, operands: &str) -> bool {
        operands.trim_end().ends_with(',')
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
            let line = self.lines[self.current_line];
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

/// Tokenize operands string for further parsing.
pub fn tokenize_operands(operands: &str) -> Result<Vec<Token>, JclError> {
    let mut tokens = Vec::new();
    let mut chars = operands.chars().peekable();

    while let Some(&c) = chars.peek() {
        match c {
            ' ' | '\t' => {
                chars.next();
            }
            '=' => {
                chars.next();
                tokens.push(Token::Equals);
            }
            ',' => {
                chars.next();
                tokens.push(Token::Comma);
            }
            '(' => {
                chars.next();
                tokens.push(Token::LParen);
            }
            ')' => {
                chars.next();
                tokens.push(Token::RParen);
            }
            '.' => {
                chars.next();
                tokens.push(Token::Period);
            }
            '*' => {
                chars.next();
                tokens.push(Token::Asterisk);
            }
            '&' => {
                chars.next();
                // Collect symbolic parameter name
                let mut name = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphanumeric() || c == '@' || c == '#' || c == '$' {
                        name.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Ampersand);
                if !name.is_empty() {
                    tokens.push(Token::Ident(name));
                }
            }
            '\'' => {
                chars.next();
                let mut s = String::new();
                loop {
                    match chars.next() {
                        Some('\'') => {
                            if chars.peek() == Some(&'\'') {
                                // Escaped quote
                                s.push('\'');
                                chars.next();
                            } else {
                                break;
                            }
                        }
                        Some(c) => s.push(c),
                        None => {
                            return Err(JclError::ParseError {
                                message: "Unterminated string".to_string(),
                            });
                        }
                    }
                }
                tokens.push(Token::String(s));
            }
            '0'..='9' => {
                let mut num = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() {
                        num.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Number(num.parse().unwrap_or(0)));
            }
            _ if c.is_ascii_alphabetic() || c == '@' || c == '#' || c == '$' => {
                let mut ident = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphanumeric() || c == '@' || c == '#' || c == '$' || c == '-' {
                        ident.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Ident(ident.to_uppercase()));
            }
            _ => {
                chars.next();
            }
        }
    }

    Ok(tokens)
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
    fn test_tokenize_operands() {
        let tokens = tokenize_operands("PGM=MYPROG,PARM='TEST'").unwrap();

        assert!(matches!(tokens[0], Token::Ident(ref s) if s == "PGM"));
        assert!(matches!(tokens[1], Token::Equals));
        assert!(matches!(tokens[2], Token::Ident(ref s) if s == "MYPROG"));
        assert!(matches!(tokens[3], Token::Comma));
        assert!(matches!(tokens[4], Token::Ident(ref s) if s == "PARM"));
        assert!(matches!(tokens[5], Token::Equals));
        assert!(matches!(tokens[6], Token::String(ref s) if s == "TEST"));
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
}
