//! EXEC CICS block scanner.
//!
//! Scans COBOL source to find EXEC CICS ... END-EXEC blocks.

use crate::CicsResult;

/// An EXEC CICS block found in source.
#[derive(Debug, Clone)]
pub struct CicsBlock {
    /// The CICS command text (without EXEC CICS / END-EXEC)
    pub text: String,
    /// Starting line number (1-based)
    pub start_line: usize,
    /// Ending line number (1-based)
    pub end_line: usize,
}

/// Scanner for EXEC CICS blocks.
pub struct CicsScanner {
    /// Current state
    in_exec_cics: bool,
    /// Accumulated text
    current_text: String,
    /// Start line of current block
    current_start: usize,
}

impl CicsScanner {
    /// Create a new scanner.
    pub fn new() -> Self {
        Self {
            in_exec_cics: false,
            current_text: String::new(),
            current_start: 0,
        }
    }

    /// Scan source for EXEC CICS blocks.
    pub fn scan(&mut self, source: &str) -> CicsResult<Vec<CicsBlock>> {
        let mut blocks = Vec::new();

        for (line_num, line) in source.lines().enumerate() {
            let line_number = line_num + 1;

            // Skip comment lines (column 7 is *)
            if line.len() > 6 && line.chars().nth(6) == Some('*') {
                continue;
            }

            // Get the code portion (columns 8-72 in standard COBOL)
            let code = if line.len() > 7 {
                &line[7..std::cmp::min(line.len(), 72)]
            } else {
                ""
            };

            let upper = code.to_uppercase();

            if self.in_exec_cics {
                // Look for END-EXEC
                if let Some(pos) = upper.find("END-EXEC") {
                    // Get text before END-EXEC
                    let before = &code[..pos];
                    self.current_text.push_str(before);

                    // Create block
                    blocks.push(CicsBlock {
                        text: self.normalize_text(&self.current_text),
                        start_line: self.current_start,
                        end_line: line_number,
                    });

                    self.in_exec_cics = false;
                    self.current_text.clear();
                } else {
                    // Continuation line
                    self.current_text.push(' ');
                    self.current_text.push_str(code.trim());
                }
            } else {
                // Look for EXEC CICS
                if let Some(pos) = upper.find("EXEC CICS") {
                    self.in_exec_cics = true;
                    self.current_start = line_number;

                    // Get text after EXEC CICS
                    let after_pos = pos + 9; // Length of "EXEC CICS"
                    if after_pos < code.len() {
                        let after = &code[after_pos..];
                        let after_upper = after.to_uppercase();

                        // Check if END-EXEC is on same line
                        if let Some(end_pos) = after_upper.find("END-EXEC") {
                            let cmd_text = &after[..end_pos];
                            blocks.push(CicsBlock {
                                text: self.normalize_text(cmd_text),
                                start_line: line_number,
                                end_line: line_number,
                            });
                            self.in_exec_cics = false;
                        } else {
                            self.current_text = after.trim().to_string();
                        }
                    }
                }
            }
        }

        Ok(blocks)
    }

    /// Normalize command text (collapse whitespace, trim).
    fn normalize_text(&self, text: &str) -> String {
        text.split_whitespace().collect::<Vec<&str>>().join(" ")
    }
}

impl Default for CicsScanner {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_line_exec_cics() {
        let mut scanner = CicsScanner::new();
        let source = "       EXEC CICS RETURN END-EXEC.";

        let blocks = scanner.scan(source).unwrap();

        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].text, "RETURN");
    }

    #[test]
    fn test_multi_line_exec_cics() {
        let mut scanner = CicsScanner::new();
        let source = r#"       EXEC CICS
             LINK PROGRAM('SUBPROG')
                  COMMAREA(WS-DATA)
           END-EXEC."#;

        let blocks = scanner.scan(source).unwrap();

        assert_eq!(blocks.len(), 1);
        assert!(blocks[0].text.contains("LINK"));
        assert!(blocks[0].text.contains("PROGRAM"));
        assert!(blocks[0].text.contains("COMMAREA"));
    }

    #[test]
    fn test_multiple_exec_cics_blocks() {
        let mut scanner = CicsScanner::new();
        let source = r#"       EXEC CICS LINK PROGRAM('A') END-EXEC.
           MOVE 'X' TO WS-VAR.
           EXEC CICS RETURN END-EXEC."#;

        let blocks = scanner.scan(source).unwrap();

        assert_eq!(blocks.len(), 2);
        assert!(blocks[0].text.contains("LINK"));
        assert!(blocks[1].text.contains("RETURN"));
    }

    #[test]
    fn test_skip_comment_lines() {
        let mut scanner = CicsScanner::new();
        let source = r#"      *EXEC CICS LINK PROGRAM('A') END-EXEC.
       EXEC CICS RETURN END-EXEC."#;

        let blocks = scanner.scan(source).unwrap();

        assert_eq!(blocks.len(), 1);
        assert!(blocks[0].text.contains("RETURN"));
    }

    #[test]
    fn test_read_command() {
        let mut scanner = CicsScanner::new();
        let source = r#"       EXEC CICS
             READ FILE('CUSTFILE')
                  INTO(WS-RECORD)
                  RIDFLD(WS-KEY)
           END-EXEC."#;

        let blocks = scanner.scan(source).unwrap();

        assert_eq!(blocks.len(), 1);
        assert!(blocks[0].text.contains("READ"));
        assert!(blocks[0].text.contains("FILE"));
        assert!(blocks[0].text.contains("INTO"));
        assert!(blocks[0].text.contains("RIDFLD"));
    }

    #[test]
    fn test_handle_condition() {
        let mut scanner = CicsScanner::new();
        let source = "       EXEC CICS HANDLE CONDITION NOTFND(NOT-FOUND-PARA) END-EXEC.";

        let blocks = scanner.scan(source).unwrap();

        assert_eq!(blocks.len(), 1);
        assert!(blocks[0].text.contains("HANDLE CONDITION"));
        assert!(blocks[0].text.contains("NOTFND"));
    }

    #[test]
    fn test_whitespace_normalization() {
        let mut scanner = CicsScanner::new();
        let source = r#"       EXEC CICS
             LINK    PROGRAM('SUBPROG')
                     COMMAREA(WS-DATA)
           END-EXEC."#;

        let blocks = scanner.scan(source).unwrap();

        assert_eq!(blocks.len(), 1);
        // Whitespace should be normalized
        assert!(!blocks[0].text.contains("    "));
    }
}
