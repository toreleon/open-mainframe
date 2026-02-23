//! # CLIST I/O and Error Handling (CL-103)
//!
//! Terminal output (WRITE/WRITENR), terminal input (READ/READDVAL/TERMIN),
//! sequential file I/O (OPENFILE/GETFILE/PUTFILE/CLOSFILE),
//! ERROR/ATTN routine handling, and CONTROL statement options.

use std::collections::HashMap;

// ─────────────────────── Errors ───────────────────────

/// I/O error.
#[derive(Debug, thiserror::Error)]
pub enum IoError {
    #[error("file not open: {0}")]
    FileNotOpen(String),
    #[error("file already open: {0}")]
    FileAlreadyOpen(String),
    #[error("end of file: {0}")]
    EndOfFile(String),
    #[error("no input available")]
    NoInput,
    #[error("invalid file mode: {0}")]
    InvalidMode(String),
    #[error("write error: {0}")]
    WriteError(String),
}

// ─────────────────────── CL-103.6: CONTROL Statement ───────────────────────

/// CONTROL statement options.
#[derive(Debug, Clone)]
pub struct ControlOptions {
    /// LIST / NOLIST — display each statement before execution
    pub list: bool,
    /// CONLIST / NOCONLIST — display after variable substitution
    pub conlist: bool,
    /// SYMLIST / NOSYMLIST — display symbolic variable resolution
    pub symlist: bool,
    /// MSG / NOMSG — display TSO messages
    pub msg: bool,
    /// PROMPT / NOPROMPT — allow terminal prompts
    pub prompt: bool,
    /// ASIS / CAPS — case handling for input
    pub asis: bool,
    /// MAIN — declare this as the main CLIST
    pub main: bool,
    /// FLUSH / NOFLUSH — flush input stack on error
    pub flush: bool,
    /// NOEND — prevent END subcommand from terminating CLIST
    pub noend: bool,
}

impl Default for ControlOptions {
    fn default() -> Self {
        Self {
            list: false,
            conlist: false,
            symlist: false,
            msg: true,
            prompt: true,
            asis: false,
            main: false,
            flush: true,
            noend: false,
        }
    }
}

// ─────────────────────── CL-103.4/5: Error/Attn Routines ───────────────────────

/// Error routine definition.
#[derive(Debug, Clone, Default)]
pub struct ErrorRoutine {
    /// Whether the error routine is active.
    pub active: bool,
    /// Action: body of statements to execute on error.
    pub body: Vec<String>,
}

/// Attention routine definition.
#[derive(Debug, Clone, Default)]
pub struct AttnRoutine {
    /// Whether the attn routine is active.
    pub active: bool,
    /// Action.
    pub body: Vec<String>,
}

// ─────────────────────── File I/O ───────────────────────

/// File access mode.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FileMode {
    Input,
    Output,
    Update,
}

impl FileMode {
    pub fn parse_str(s: &str) -> Self {
        match s.to_uppercase().as_str() {
            "OUTPUT" => Self::Output,
            "UPDATE" => Self::Update,
            _ => Self::Input,
        }
    }
}

/// An open CLIST file.
#[derive(Debug, Clone)]
pub struct ClistFile {
    /// File identifier (variable name).
    pub name: String,
    /// Access mode.
    pub mode: FileMode,
    /// Lines read from file.
    lines: Vec<String>,
    /// Current read position.
    read_pos: usize,
    /// Lines written to file.
    written: Vec<String>,
}

impl ClistFile {
    fn new(name: &str, mode: FileMode) -> Self {
        Self {
            name: name.to_uppercase(),
            mode,
            lines: Vec::new(),
            read_pos: 0,
            written: Vec::new(),
        }
    }

    /// Provide simulated file content.
    pub fn set_content(&mut self, content: &str) {
        self.lines = content.lines().map(|l| l.to_string()).collect();
        self.read_pos = 0;
    }

    /// Read next line.
    fn get_line(&mut self) -> Option<String> {
        if self.read_pos < self.lines.len() {
            let line = self.lines[self.read_pos].clone();
            self.read_pos += 1;
            Some(line)
        } else {
            None
        }
    }

    /// Write a line.
    fn put_line(&mut self, data: &str) {
        self.written.push(data.to_string());
    }

    /// Get written lines.
    pub fn written_lines(&self) -> &[String] {
        &self.written
    }
}

// ─────────────────────── I/O Manager ───────────────────────

/// I/O manager for CLIST file and terminal operations.
#[derive(Debug, Default)]
pub struct IoManager {
    /// Open files.
    files: HashMap<String, ClistFile>,
    /// Simulated terminal input queue.
    input_queue: Vec<String>,
    /// Terminal output.
    output: Vec<String>,
}

impl IoManager {
    pub fn new() -> Self {
        Self::default()
    }

    // ─── CL-103.1/2: Terminal I/O ───

    /// Push simulated input.
    pub fn push_input(&mut self, input: &str) {
        self.input_queue.push(input.to_string());
    }

    /// Read a line from terminal.
    pub fn read_line(&mut self) -> Result<String, IoError> {
        if self.input_queue.is_empty() {
            Err(IoError::NoInput)
        } else {
            Ok(self.input_queue.remove(0))
        }
    }

    /// Write to terminal.
    pub fn write_line(&mut self, text: &str) {
        self.output.push(text.to_string());
    }

    /// Get terminal output.
    pub fn output(&self) -> &[String] {
        &self.output
    }

    // ─── CL-103.3: File I/O ───

    /// Open a file.
    pub fn open_file(&mut self, name: &str, mode: &str) -> Result<(), IoError> {
        let upper = name.to_uppercase();
        if self.files.contains_key(&upper) {
            return Err(IoError::FileAlreadyOpen(upper));
        }
        let file_mode = FileMode::parse_str(mode);
        self.files.insert(upper.clone(), ClistFile::new(&upper, file_mode));
        Ok(())
    }

    /// Read next line from file (GETFILE).
    pub fn get_file(&mut self, name: &str) -> Result<String, IoError> {
        let upper = name.to_uppercase();
        let file = self.files.get_mut(&upper)
            .ok_or_else(|| IoError::FileNotOpen(upper.clone()))?;
        file.get_line().ok_or(IoError::EndOfFile(upper))
    }

    /// Write line to file (PUTFILE).
    pub fn put_file(&mut self, name: &str, data: &str) -> Result<(), IoError> {
        let upper = name.to_uppercase();
        let file = self.files.get_mut(&upper)
            .ok_or_else(|| IoError::FileNotOpen(upper.clone()))?;
        if file.mode == FileMode::Input {
            return Err(IoError::WriteError("file open for INPUT".to_string()));
        }
        file.put_line(data);
        Ok(())
    }

    /// Close a file.
    pub fn close_file(&mut self, name: &str) -> Result<(), IoError> {
        let upper = name.to_uppercase();
        self.files.remove(&upper)
            .ok_or(IoError::FileNotOpen(upper))?;
        Ok(())
    }

    /// Get an open file (for testing/content injection).
    pub fn get_open_file(&mut self, name: &str) -> Option<&mut ClistFile> {
        self.files.get_mut(&name.to_uppercase())
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ─── CL-103.1: Terminal Output ───

    #[test]
    fn test_write_output() {
        let mut io = IoManager::new();
        io.write_line("Hello, World!");
        assert_eq!(io.output(), &["Hello, World!"]);
    }

    #[test]
    fn test_write_multiple() {
        let mut io = IoManager::new();
        io.write_line("Line 1");
        io.write_line("Line 2");
        assert_eq!(io.output().len(), 2);
    }

    // ─── CL-103.2: Terminal Input ───

    #[test]
    fn test_read_input() {
        let mut io = IoManager::new();
        io.push_input("user input");
        let result = io.read_line().unwrap();
        assert_eq!(result, "user input");
    }

    #[test]
    fn test_read_no_input() {
        let mut io = IoManager::new();
        assert!(io.read_line().is_err());
    }

    #[test]
    fn test_read_multiple() {
        let mut io = IoManager::new();
        io.push_input("first");
        io.push_input("second");
        assert_eq!(io.read_line().unwrap(), "first");
        assert_eq!(io.read_line().unwrap(), "second");
    }

    // ─── CL-103.3: File I/O ───

    #[test]
    fn test_open_close_file() {
        let mut io = IoManager::new();
        io.open_file("TEST", "INPUT").unwrap();
        io.close_file("TEST").unwrap();
    }

    #[test]
    fn test_file_already_open() {
        let mut io = IoManager::new();
        io.open_file("TEST", "INPUT").unwrap();
        assert!(io.open_file("TEST", "INPUT").is_err());
    }

    #[test]
    fn test_getfile() {
        let mut io = IoManager::new();
        io.open_file("MYFILE", "INPUT").unwrap();
        {
            let file = io.get_open_file("MYFILE").unwrap();
            file.set_content("line1\nline2\nline3");
        }
        assert_eq!(io.get_file("MYFILE").unwrap(), "line1");
        assert_eq!(io.get_file("MYFILE").unwrap(), "line2");
        assert_eq!(io.get_file("MYFILE").unwrap(), "line3");
        assert!(io.get_file("MYFILE").is_err()); // EOF
    }

    #[test]
    fn test_putfile() {
        let mut io = IoManager::new();
        io.open_file("OUTFILE", "OUTPUT").unwrap();
        io.put_file("OUTFILE", "data line").unwrap();
        let file = io.get_open_file("OUTFILE").unwrap();
        assert_eq!(file.written_lines(), &["data line"]);
    }

    #[test]
    fn test_putfile_input_mode() {
        let mut io = IoManager::new();
        io.open_file("INFILE", "INPUT").unwrap();
        assert!(io.put_file("INFILE", "data").is_err());
    }

    #[test]
    fn test_close_not_open() {
        let mut io = IoManager::new();
        assert!(io.close_file("NOSUCH").is_err());
    }

    // ─── CL-103.4: ERROR Routine ───

    #[test]
    fn test_error_routine_default() {
        let routine = ErrorRoutine::default();
        assert!(!routine.active);
    }

    // ─── CL-103.5: ATTN Routine ───

    #[test]
    fn test_attn_routine_default() {
        let routine = AttnRoutine::default();
        assert!(!routine.active);
    }

    // ─── CL-103.6: CONTROL Statement ───

    #[test]
    fn test_control_defaults() {
        let control = ControlOptions::default();
        assert!(!control.list);
        assert!(!control.conlist);
        assert!(!control.symlist);
        assert!(control.msg);
        assert!(control.prompt);
        assert!(!control.asis);
        assert!(!control.main);
        assert!(control.flush);
    }

    #[test]
    fn test_file_mode_parse() {
        assert_eq!(FileMode::parse_str("INPUT"), FileMode::Input);
        assert_eq!(FileMode::parse_str("OUTPUT"), FileMode::Output);
        assert_eq!(FileMode::parse_str("UPDATE"), FileMode::Update);
        assert_eq!(FileMode::parse_str("unknown"), FileMode::Input);
    }
}
