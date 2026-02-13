//! CICS command preprocessing for COBOL programs.
//!
//! This module handles extraction and processing of EXEC CICS commands
//! from COBOL source code.

mod scanner;

pub use scanner::{CicsBlock, CicsScanner};

use crate::CicsResult;

/// Result of preprocessing a COBOL source file.
#[derive(Debug)]
pub struct PreprocessResult {
    /// Pure COBOL source with EXEC CICS replaced by runtime calls
    pub cobol_source: String,
    /// Extracted CICS commands
    pub commands: Vec<CicsCommand>,
}

/// A CICS command extracted from COBOL source.
#[derive(Debug, Clone)]
pub struct CicsCommand {
    /// Command number (1-based)
    pub number: usize,
    /// The command text (without EXEC CICS / END-EXEC)
    pub text: String,
    /// Command type
    pub command_type: CicsCommandType,
    /// Command options
    pub options: Vec<CicsOption>,
    /// Starting line in original source
    pub start_line: usize,
    /// Ending line in original source
    pub end_line: usize,
}

/// Types of CICS commands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CicsCommandType {
    /// LINK - call subprogram with return
    Link,
    /// XCTL - transfer control without return
    Xctl,
    /// RETURN - end program or transaction
    Return,
    /// READ - read file record
    Read,
    /// WRITE - write file record
    Write,
    /// REWRITE - update file record
    Rewrite,
    /// DELETE - delete file record
    Delete,
    /// SEND - send data to terminal
    Send,
    /// RECEIVE - receive data from terminal
    Receive,
    /// SEND MAP - send BMS map
    SendMap,
    /// RECEIVE MAP - receive BMS map
    ReceiveMap,
    /// GETMAIN - allocate storage
    Getmain,
    /// FREEMAIN - release storage
    Freemain,
    /// HANDLE CONDITION - set up exception handler
    HandleCondition,
    /// HANDLE ABEND - set up abend handler
    HandleAbend,
    /// HANDLE AID - set up attention key handler
    HandleAid,
    /// IGNORE CONDITION - ignore exception
    IgnoreCondition,
    /// START - start transaction
    Start,
    /// RETRIEVE - retrieve started data
    Retrieve,
    /// CANCEL - cancel started transaction
    Cancel,
    /// DELAY - suspend execution
    Delay,
    /// ASKTIME - get current time
    Asktime,
    /// FORMATTIME - format timestamp
    Formattime,
    /// ASSIGN - get system values
    Assign,
    /// ADDRESS - get address
    Address,
    /// READQ TS - read temporary storage
    ReadqTs,
    /// WRITEQ TS - write temporary storage
    WriteqTs,
    /// DELETEQ TS - delete temporary storage
    DeleteqTs,
    /// ENQ - enqueue resource
    Enq,
    /// DEQ - dequeue resource
    Deq,
    /// SYNCPOINT - commit changes
    Syncpoint,
    /// ABEND - abnormal end
    Abend,
    /// Other/unknown command
    Other,
}

impl CicsCommandType {
    /// Determine command type from command text.
    pub fn from_text(text: &str) -> Self {
        let upper = text.trim().to_uppercase();
        let words: Vec<&str> = upper.split_whitespace().collect();

        if words.is_empty() {
            return CicsCommandType::Other;
        }

        match words[0] {
            "LINK" => CicsCommandType::Link,
            "XCTL" => CicsCommandType::Xctl,
            "RETURN" => CicsCommandType::Return,
            "READ" => CicsCommandType::Read,
            "WRITE" => CicsCommandType::Write,
            "REWRITE" => CicsCommandType::Rewrite,
            "DELETE" => CicsCommandType::Delete,
            "SEND" => {
                if words.len() > 1 && words[1].starts_with("MAP") {
                    CicsCommandType::SendMap
                } else {
                    CicsCommandType::Send
                }
            }
            "RECEIVE" => {
                if words.len() > 1 && words[1].starts_with("MAP") {
                    CicsCommandType::ReceiveMap
                } else {
                    CicsCommandType::Receive
                }
            }
            "GETMAIN" => CicsCommandType::Getmain,
            "FREEMAIN" => CicsCommandType::Freemain,
            "HANDLE" => {
                if words.len() > 1 {
                    match words[1] {
                        "CONDITION" => CicsCommandType::HandleCondition,
                        "ABEND" => CicsCommandType::HandleAbend,
                        "AID" => CicsCommandType::HandleAid,
                        _ => CicsCommandType::Other,
                    }
                } else {
                    CicsCommandType::Other
                }
            }
            "IGNORE" => {
                if words.len() > 1 && words[1] == "CONDITION" {
                    CicsCommandType::IgnoreCondition
                } else {
                    CicsCommandType::Other
                }
            }
            "START" => CicsCommandType::Start,
            "RETRIEVE" => CicsCommandType::Retrieve,
            "CANCEL" => CicsCommandType::Cancel,
            "DELAY" => CicsCommandType::Delay,
            "ASKTIME" => CicsCommandType::Asktime,
            "FORMATTIME" => CicsCommandType::Formattime,
            "ASSIGN" => CicsCommandType::Assign,
            "ADDRESS" => CicsCommandType::Address,
            "READQ" => {
                if words.len() > 1 && words[1] == "TS" {
                    CicsCommandType::ReadqTs
                } else {
                    CicsCommandType::Other
                }
            }
            "WRITEQ" => {
                if words.len() > 1 && words[1] == "TS" {
                    CicsCommandType::WriteqTs
                } else {
                    CicsCommandType::Other
                }
            }
            "DELETEQ" => {
                if words.len() > 1 && words[1] == "TS" {
                    CicsCommandType::DeleteqTs
                } else {
                    CicsCommandType::Other
                }
            }
            "ENQ" => CicsCommandType::Enq,
            "DEQ" => CicsCommandType::Deq,
            "SYNCPOINT" => CicsCommandType::Syncpoint,
            "ABEND" => CicsCommandType::Abend,
            _ => CicsCommandType::Other,
        }
    }
}

/// A CICS command option.
#[derive(Debug, Clone)]
pub struct CicsOption {
    /// Option name (e.g., PROGRAM, COMMAREA, FILE)
    pub name: String,
    /// Option value (may be a literal or variable reference)
    pub value: Option<String>,
}

/// CICS preprocessor for COBOL programs.
pub struct CicsPreprocessor {
    scanner: CicsScanner,
    commands: Vec<CicsCommand>,
    command_counter: usize,
}

impl CicsPreprocessor {
    /// Create a new preprocessor.
    pub fn new() -> Self {
        Self {
            scanner: CicsScanner::new(),
            commands: Vec::new(),
            command_counter: 0,
        }
    }

    /// Process COBOL source and extract CICS commands.
    pub fn process(&mut self, source: &str) -> CicsResult<PreprocessResult> {
        // Scan for EXEC CICS blocks
        let blocks = self.scanner.scan(source)?;

        // Process each block
        let mut cobol_lines: Vec<String> = source.lines().map(|s| s.to_string()).collect();

        for block in blocks.iter().rev() {
            self.command_counter += 1;
            let cmd_num = self.command_counter;

            // Parse command
            let command_type = CicsCommandType::from_text(&block.text);
            let options = self.parse_options(&block.text);

            // Create command record
            let command = CicsCommand {
                number: cmd_num,
                text: block.text.clone(),
                command_type,
                options,
                start_line: block.start_line,
                end_line: block.end_line,
            };
            self.commands.push(command);

            // Generate COBOL CALL replacement
            let call_code = self.generate_call(cmd_num, command_type);

            // Replace EXEC CICS block with CALL
            self.replace_block(&mut cobol_lines, block, &call_code);
        }

        // Reverse to get correct order
        self.commands.reverse();

        Ok(PreprocessResult {
            cobol_source: cobol_lines.join("\n"),
            commands: self.commands.clone(),
        })
    }

    /// Parse options from command text.
    fn parse_options(&self, text: &str) -> Vec<CicsOption> {
        let mut options = Vec::new();
        let mut chars = text.chars().peekable();
        let mut in_parens = 0;
        let mut current_name = String::new();
        let mut current_value = String::new();
        let mut parsing_value = false;

        // Skip command name(s)
        while let Some(&c) = chars.peek() {
            if c.is_whitespace() {
                chars.next();
                // Skip to first option keyword
                break;
            }
            chars.next();
        }

        // Skip additional whitespace and command modifiers
        while let Some(&c) = chars.peek() {
            if c.is_whitespace() {
                chars.next();
            } else {
                break;
            }
        }

        for c in chars {
            match c {
                '(' => {
                    in_parens += 1;
                    if in_parens == 1 {
                        parsing_value = true;
                        continue;
                    }
                    current_value.push(c);
                }
                ')' => {
                    in_parens -= 1;
                    if in_parens == 0 {
                        // End of option value
                        options.push(CicsOption {
                            name: current_name.trim().to_uppercase(),
                            value: if current_value.trim().is_empty() {
                                None
                            } else {
                                Some(current_value.trim().to_string())
                            },
                        });
                        current_name.clear();
                        current_value.clear();
                        parsing_value = false;
                        continue;
                    }
                    current_value.push(c);
                }
                ' ' | '\n' | '\t' => {
                    if in_parens > 0 {
                        current_value.push(c);
                    } else if !current_name.is_empty() && !parsing_value {
                        // Option without value
                        options.push(CicsOption {
                            name: current_name.trim().to_uppercase(),
                            value: None,
                        });
                        current_name.clear();
                    }
                }
                _ => {
                    if parsing_value {
                        current_value.push(c);
                    } else {
                        current_name.push(c);
                    }
                }
            }
        }

        // Handle trailing option without value
        if !current_name.is_empty() {
            options.push(CicsOption {
                name: current_name.trim().to_uppercase(),
                value: None,
            });
        }

        options
    }

    /// Generate COBOL CALL statement to replace EXEC CICS.
    fn generate_call(&self, cmd_num: usize, cmd_type: CicsCommandType) -> String {
        let call_name = match cmd_type {
            CicsCommandType::Link => "CICSLINK",
            CicsCommandType::Xctl => "CICSXCTL",
            CicsCommandType::Return => "CICSRETN",
            CicsCommandType::Read => "CICSREAD",
            CicsCommandType::Write => "CICSWRIT",
            CicsCommandType::Rewrite => "CICSRWRT",
            CicsCommandType::Delete => "CICSDELT",
            CicsCommandType::SendMap => "CICSSMAP",
            CicsCommandType::ReceiveMap => "CICSRMAP",
            CicsCommandType::Send => "CICSSEND",
            CicsCommandType::Receive => "CICSRECV",
            CicsCommandType::Getmain => "CICSGMN",
            CicsCommandType::Freemain => "CICSFMN",
            CicsCommandType::HandleCondition => "CICSHCND",
            CicsCommandType::HandleAbend => "CICSHABN",
            CicsCommandType::HandleAid => "CICSHAID",
            _ => "CICSEXEC",
        };

        format!(
            "           CALL \"{call_name}\" USING CICS-CMD-{cmd_num:03}\n\
             {pad}DFHEIBLK",
            pad = "                                    "
        )
    }

    /// Replace EXEC CICS block with generated CALL.
    fn replace_block(&self, lines: &mut Vec<String>, block: &CicsBlock, replacement: &str) {
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

impl Default for CicsPreprocessor {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_command_type_detection() {
        assert_eq!(
            CicsCommandType::from_text("LINK PROGRAM('SUB')"),
            CicsCommandType::Link
        );
        assert_eq!(
            CicsCommandType::from_text("XCTL PROGRAM('NEXT')"),
            CicsCommandType::Xctl
        );
        assert_eq!(
            CicsCommandType::from_text("RETURN"),
            CicsCommandType::Return
        );
        assert_eq!(
            CicsCommandType::from_text("READ FILE('CUSTFILE')"),
            CicsCommandType::Read
        );
        assert_eq!(
            CicsCommandType::from_text("WRITE FILE('CUSTFILE')"),
            CicsCommandType::Write
        );
        assert_eq!(
            CicsCommandType::from_text("SEND MAP('MAP1')"),
            CicsCommandType::SendMap
        );
        assert_eq!(
            CicsCommandType::from_text("RECEIVE MAP('MAP1')"),
            CicsCommandType::ReceiveMap
        );
        assert_eq!(
            CicsCommandType::from_text("HANDLE CONDITION NOTFND(LABEL)"),
            CicsCommandType::HandleCondition
        );
    }

    #[test]
    fn test_option_parsing() {
        let preprocessor = CicsPreprocessor::new();
        let options = preprocessor.parse_options("LINK PROGRAM('SUBPROG') COMMAREA(WS-DATA)");

        assert!(options.iter().any(|o| o.name == "PROGRAM" && o.value == Some("'SUBPROG'".to_string())));
        assert!(options.iter().any(|o| o.name == "COMMAREA" && o.value == Some("WS-DATA".to_string())));
    }

    #[test]
    fn test_option_without_value() {
        let preprocessor = CicsPreprocessor::new();
        let options = preprocessor.parse_options("RETURN TRANSID('MENU') IMMEDIATE");

        assert!(options.iter().any(|o| o.name == "TRANSID"));
        assert!(options.iter().any(|o| o.name == "IMMEDIATE" && o.value.is_none()));
    }

    #[test]
    fn test_preprocessor_basic() {
        let source = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
           EXEC CICS
             LINK PROGRAM('SUBPROG')
                  COMMAREA(WS-DATA)
           END-EXEC.
           STOP RUN."#;

        let mut preprocessor = CicsPreprocessor::new();
        let result = preprocessor.process(source).unwrap();

        assert_eq!(result.commands.len(), 1);
        assert_eq!(result.commands[0].command_type, CicsCommandType::Link);
        assert!(result.cobol_source.contains("CALL \"CICSLINK\""));
    }
}
