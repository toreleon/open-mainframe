//! TSO service routines — PUTLINE, GETLINE, PUTGET, STACK, and I/O management.
//!
//! These are the programmatic interfaces TSO commands and programs use for
//! terminal I/O.  They abstract over the actual terminal, enabling batch
//! (IKJEFT01) and interactive use with the same API.

use serde::{Deserialize, Serialize};

/// Terminal I/O provider for TSO service routines.
///
/// Implementations handle either interactive terminal I/O or batch I/O
/// (SYSTSIN/SYSTSPRT).
pub trait TsoIo {
    /// Write a line to the terminal (PUTLINE equivalent).
    fn putline(&mut self, line: &str);

    /// Read a line from the terminal (GETLINE equivalent).
    /// Returns `None` at end-of-input.
    fn getline(&mut self) -> Option<String>;

    /// Write a prompt and read the response (PUTGET equivalent).
    fn putget(&mut self, prompt: &str) -> Option<String> {
        self.putline(prompt);
        self.getline()
    }
}

/// In-memory I/O provider for testing and batch execution.
///
/// Input lines are consumed from a queue; output lines are collected in a
/// buffer.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct MemoryIo {
    /// Lines available for GETLINE (front = next).
    pub input: Vec<String>,
    /// Collected PUTLINE output.
    pub output: Vec<String>,
    /// Input stack (pushed lines consumed before normal input).
    pub stack: Vec<String>,
    /// Index into `input`.
    input_pos: usize,
}

impl MemoryIo {
    /// Create a new `MemoryIo` with the given input lines.
    pub fn new(input: Vec<String>) -> Self {
        Self {
            input,
            output: Vec::new(),
            stack: Vec::new(),
            input_pos: 0,
        }
    }

    /// Create an empty `MemoryIo` (no input).
    pub fn empty() -> Self {
        Self::default()
    }

    /// Push lines onto the input stack (STACK equivalent).
    ///
    /// Stacked lines are consumed by GETLINE before normal input.
    pub fn stack_input(&mut self, lines: &[String]) {
        // Prepend to stack (LIFO — last stacked line read first)
        let mut new_stack = lines.to_vec();
        new_stack.append(&mut self.stack);
        self.stack = new_stack;
    }

    /// Get all output lines collected so far.
    pub fn drain_output(&mut self) -> Vec<String> {
        std::mem::take(&mut self.output)
    }
}

impl TsoIo for MemoryIo {
    fn putline(&mut self, line: &str) {
        self.output.push(line.to_string());
    }

    fn getline(&mut self) -> Option<String> {
        // Stack lines take priority over normal input
        if !self.stack.is_empty() {
            return Some(self.stack.remove(0));
        }
        if self.input_pos < self.input.len() {
            let line = self.input[self.input_pos].clone();
            self.input_pos += 1;
            Some(line)
        } else {
            None
        }
    }
}

// ---------------------------------------------------------------------------
// IKJPARS — command parsing service
// ---------------------------------------------------------------------------

/// Parameter type for IKJPARS Parse Control Entry (PCE).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PceParam {
    /// Positional parameter.
    Positional {
        name: String,
        required: bool,
    },
    /// Keyword parameter with value: `KEYWORD(value)`.
    Keyword {
        name: String,
        required: bool,
    },
    /// Boolean flag: present or absent.
    Flag {
        name: String,
    },
}

/// Parse Control Entry — defines the syntax of a TSO command.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ParseControlEntry {
    /// Command name.
    pub command: String,
    /// Parameter definitions (order matters for positionals).
    pub params: Vec<PceParam>,
}

impl ParseControlEntry {
    pub fn new(command: &str) -> Self {
        Self {
            command: command.to_ascii_uppercase(),
            params: Vec::new(),
        }
    }

    /// Add a positional parameter definition.
    pub fn positional(mut self, name: &str, required: bool) -> Self {
        self.params.push(PceParam::Positional {
            name: name.to_ascii_uppercase(),
            required,
        });
        self
    }

    /// Add a keyword parameter definition.
    pub fn keyword(mut self, name: &str, required: bool) -> Self {
        self.params.push(PceParam::Keyword {
            name: name.to_ascii_uppercase(),
            required,
        });
        self
    }

    /// Add a flag definition.
    pub fn flag(mut self, name: &str) -> Self {
        self.params.push(PceParam::Flag {
            name: name.to_ascii_uppercase(),
        });
        self
    }
}

/// Parse Descriptor List — result of IKJPARS parsing.
#[derive(Debug, Clone, Default)]
pub struct ParseDescriptorList {
    /// Positional parameter values (in PCE order).
    pub positionals: Vec<Option<String>>,
    /// Keyword parameter values (by name).
    pub keywords: std::collections::HashMap<String, String>,
    /// Flags that were present.
    pub flags: Vec<String>,
}

/// Parse command operands according to a PCE definition.
///
/// This is the IKJPARS equivalent — validates parameters against the PCE
/// and produces a PDL.
pub fn ikjpars(
    pce: &ParseControlEntry,
    cmd: &crate::ParsedCommand,
) -> Result<ParseDescriptorList, String> {
    let mut pdl = ParseDescriptorList::default();
    let mut pos_idx = 0;

    for param in &pce.params {
        match param {
            PceParam::Positional { name, required } => {
                let val = cmd.positional.get(pos_idx).cloned();
                if *required && val.is_none() {
                    return Err(format!("IKJ56702I MISSING {name} OPERAND"));
                }
                pdl.positionals.push(val);
                pos_idx += 1;
            }
            PceParam::Keyword { name, required } => {
                let val = cmd.keyword(name).map(|s| s.to_string());
                if *required && val.is_none() {
                    return Err(format!("IKJ56702I MISSING {name} KEYWORD"));
                }
                if let Some(v) = val {
                    pdl.keywords.insert(name.clone(), v);
                }
            }
            PceParam::Flag { name } => {
                if cmd.has_flag(name) {
                    pdl.flags.push(name.clone());
                }
            }
        }
    }

    Ok(pdl)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_memory_io_putline_getline() {
        let mut io = MemoryIo::new(vec!["hello".to_string(), "world".to_string()]);
        assert_eq!(io.getline(), Some("hello".to_string()));
        io.putline("response");
        assert_eq!(io.getline(), Some("world".to_string()));
        assert_eq!(io.getline(), None);
        assert_eq!(io.output, vec!["response".to_string()]);
    }

    #[test]
    fn test_memory_io_putget() {
        let mut io = MemoryIo::new(vec!["yes".to_string()]);
        let resp = io.putget("Confirm? ");
        assert_eq!(resp, Some("yes".to_string()));
        assert_eq!(io.output, vec!["Confirm? ".to_string()]);
    }

    #[test]
    fn test_memory_io_stack() {
        let mut io = MemoryIo::new(vec!["normal".to_string()]);
        io.stack_input(&["stacked1".to_string(), "stacked2".to_string()]);
        // Stacked lines consumed first
        assert_eq!(io.getline(), Some("stacked1".to_string()));
        assert_eq!(io.getline(), Some("stacked2".to_string()));
        // Then normal input
        assert_eq!(io.getline(), Some("normal".to_string()));
        assert_eq!(io.getline(), None);
    }

    #[test]
    fn test_memory_io_drain() {
        let mut io = MemoryIo::empty();
        io.putline("line1");
        io.putline("line2");
        let drained = io.drain_output();
        assert_eq!(drained, vec!["line1", "line2"]);
        assert!(io.output.is_empty());
    }

    #[test]
    fn test_ikjpars_full() {
        use crate::parser::parse_command;

        let pce = ParseControlEntry::new("ALLOC")
            .keyword("DA", true)
            .keyword("FILE", false)
            .flag("SHR")
            .flag("NEW");

        let cmd = parse_command("ALLOC DA('PROD.DATA') FILE(INFILE) SHR");
        let pdl = ikjpars(&pce, &cmd).unwrap();
        assert_eq!(pdl.keywords.get("DA"), Some(&"PROD.DATA".to_string()));
        assert_eq!(pdl.keywords.get("FILE"), Some(&"INFILE".to_string()));
        assert!(pdl.flags.contains(&"SHR".to_string()));
        assert!(!pdl.flags.contains(&"NEW".to_string()));
    }

    #[test]
    fn test_ikjpars_missing_required() {
        use crate::parser::parse_command;

        let pce = ParseControlEntry::new("TEST")
            .positional("DSNAME", true);

        let cmd = parse_command("TEST");
        let result = ikjpars(&pce, &cmd);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("MISSING DSNAME"));
    }

    #[test]
    fn test_ikjpars_optional() {
        use crate::parser::parse_command;

        let pce = ParseControlEntry::new("TEST")
            .positional("DSNAME", false)
            .keyword("VOL", false);

        let cmd = parse_command("TEST");
        let pdl = ikjpars(&pce, &cmd).unwrap();
        assert_eq!(pdl.positionals[0], None);
        assert!(!pdl.keywords.contains_key("VOL"));
    }

    #[test]
    fn test_ikjpars_positional() {
        use crate::parser::parse_command;

        let pce = ParseControlEntry::new("LISTDS")
            .positional("DSNAME", true)
            .flag("MEMBERS")
            .flag("STATUS");

        let cmd = parse_command("LISTDS 'SYS1.PARMLIB' MEMBERS STATUS");
        let pdl = ikjpars(&pce, &cmd).unwrap();
        assert_eq!(pdl.positionals[0], Some("SYS1.PARMLIB".to_string()));
        assert!(pdl.flags.contains(&"MEMBERS".to_string()));
        assert!(pdl.flags.contains(&"STATUS".to_string()));
    }
}
