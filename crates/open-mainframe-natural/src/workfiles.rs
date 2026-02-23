// SPDX-License-Identifier: Apache-2.0
//! NAT-109 (partial): Work File I/O for Natural.
//!
//! Provides WRITE WORK FILE / READ WORK FILE support with up to 32 work
//! files, ON ERROR runtime error trapping, and STACK INPUT processing.

use std::collections::HashMap;

use crate::data_model::NaturalValue;

// ---------------------------------------------------------------------------
// Work file
// ---------------------------------------------------------------------------

/// An in-memory work file.
#[derive(Debug, Clone)]
pub struct WorkFile {
    pub file_number: u8,
    pub records: Vec<Vec<NaturalValue>>,
    pub position: usize,
    pub is_open: bool,
}

impl WorkFile {
    pub fn new(file_number: u8) -> Self {
        Self {
            file_number,
            records: Vec::new(),
            position: 0,
            is_open: true,
        }
    }

    /// Write a record to the work file.
    pub fn write_record(&mut self, fields: Vec<NaturalValue>) {
        self.records.push(fields);
    }

    /// Read the next record from the work file.
    pub fn read_record(&mut self) -> Option<Vec<NaturalValue>> {
        if self.position < self.records.len() {
            let record = self.records[self.position].clone();
            self.position += 1;
            Some(record)
        } else {
            None
        }
    }

    /// Reset read position to the beginning.
    pub fn rewind(&mut self) {
        self.position = 0;
    }

    /// Check if at end of file.
    pub fn is_eof(&self) -> bool {
        self.position >= self.records.len()
    }

    /// Get record count.
    pub fn record_count(&self) -> usize {
        self.records.len()
    }

    /// Clear all records.
    pub fn clear(&mut self) {
        self.records.clear();
        self.position = 0;
    }

    /// Close the work file.
    pub fn close(&mut self) {
        self.is_open = false;
    }
}

// ---------------------------------------------------------------------------
// Work file manager
// ---------------------------------------------------------------------------

/// Maximum number of work files.
pub const MAX_WORK_FILES: u8 = 32;

/// Manages up to 32 work files.
#[derive(Debug, Clone)]
pub struct WorkFileManager {
    files: HashMap<u8, WorkFile>,
}

impl WorkFileManager {
    pub fn new() -> Self {
        Self { files: HashMap::new() }
    }

    /// Get or create a work file.
    pub fn get_or_create(&mut self, file_number: u8) -> Result<&mut WorkFile, WorkFileError> {
        if !(1..=MAX_WORK_FILES).contains(&file_number) {
            return Err(WorkFileError::InvalidFileNumber(file_number));
        }
        Ok(self.files.entry(file_number).or_insert_with(|| WorkFile::new(file_number)))
    }

    /// Write a record to a work file.
    pub fn write_record(&mut self, file_number: u8, fields: Vec<NaturalValue>) -> Result<(), WorkFileError> {
        let wf = self.get_or_create(file_number)?;
        if !wf.is_open {
            return Err(WorkFileError::FileClosed(file_number));
        }
        wf.write_record(fields);
        Ok(())
    }

    /// Read a record from a work file.
    pub fn read_record(&mut self, file_number: u8) -> Result<Option<Vec<NaturalValue>>, WorkFileError> {
        let wf = self.get_or_create(file_number)?;
        if !wf.is_open {
            return Err(WorkFileError::FileClosed(file_number));
        }
        Ok(wf.read_record())
    }

    /// Close a work file.
    pub fn close(&mut self, file_number: u8) -> Result<(), WorkFileError> {
        if let Some(wf) = self.files.get_mut(&file_number) {
            wf.close();
            Ok(())
        } else {
            Err(WorkFileError::FileNotOpen(file_number))
        }
    }

    /// Rewind a work file.
    pub fn rewind(&mut self, file_number: u8) -> Result<(), WorkFileError> {
        if let Some(wf) = self.files.get_mut(&file_number) {
            wf.rewind();
            Ok(())
        } else {
            Err(WorkFileError::FileNotOpen(file_number))
        }
    }

    /// Check if a work file is at EOF.
    pub fn is_eof(&self, file_number: u8) -> bool {
        self.files.get(&file_number).map_or(true, |wf| wf.is_eof())
    }
}

impl Default for WorkFileManager {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// ON ERROR handler
// ---------------------------------------------------------------------------

/// Captured error information for ON ERROR processing.
#[derive(Debug, Clone)]
pub struct NaturalError {
    pub error_nr: u32,
    pub error_line: usize,
    pub error_message: String,
    pub program_name: String,
}

impl NaturalError {
    pub fn new(error_nr: u32, error_line: usize, message: &str, program: &str) -> Self {
        Self {
            error_nr,
            error_line,
            error_message: message.to_string(),
            program_name: program.to_string(),
        }
    }
}

/// Error handler registry.
#[derive(Debug, Clone, Default)]
pub struct ErrorHandler {
    pub active: bool,
    pub last_error: Option<NaturalError>,
}

impl ErrorHandler {
    pub fn new() -> Self {
        Self { active: false, last_error: None }
    }

    /// Activate ON ERROR handling.
    pub fn activate(&mut self) {
        self.active = true;
    }

    /// Deactivate error handling.
    pub fn deactivate(&mut self) {
        self.active = false;
    }

    /// Trap a runtime error.
    pub fn trap(&mut self, error: NaturalError) -> bool {
        if self.active {
            self.last_error = Some(error);
            true
        } else {
            false
        }
    }

    /// Get the last error number.
    pub fn error_nr(&self) -> u32 {
        self.last_error.as_ref().map_or(0, |e| e.error_nr)
    }

    /// Get the last error line.
    pub fn error_line(&self) -> usize {
        self.last_error.as_ref().map_or(0, |e| e.error_line)
    }

    /// Clear the last error.
    pub fn clear(&mut self) {
        self.last_error = None;
    }
}

// ---------------------------------------------------------------------------
// STACK INPUT processing
// ---------------------------------------------------------------------------

/// Process data from the stack as INPUT values.
pub fn process_stack_input(stack: &mut Vec<NaturalValue>, var_count: usize) -> Vec<NaturalValue> {
    let mut values = Vec::new();
    for _ in 0..var_count {
        if stack.is_empty() {
            values.push(NaturalValue::Null);
        } else {
            values.push(stack.remove(0));
        }
    }
    values
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, thiserror::Error)]
pub enum WorkFileError {
    #[error("invalid work file number: {0} (must be 1-32)")]
    InvalidFileNumber(u8),
    #[error("work file {0} is closed")]
    FileClosed(u8),
    #[error("work file {0} is not open")]
    FileNotOpen(u8),
    #[error("I/O error on work file {file_number}: {message}")]
    IoError { file_number: u8, message: String },
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_work_file_write_read() {
        let mut wf = WorkFile::new(1);
        wf.write_record(vec![NaturalValue::Alpha("Record1".into()), NaturalValue::Integer(100)]);
        wf.write_record(vec![NaturalValue::Alpha("Record2".into()), NaturalValue::Integer(200)]);

        let rec1 = wf.read_record().unwrap();
        assert_eq!(rec1[0].to_display_string(), "Record1");
        assert_eq!(rec1[1].to_i64(), 100);

        let rec2 = wf.read_record().unwrap();
        assert_eq!(rec2[0].to_display_string(), "Record2");
    }

    #[test]
    fn test_work_file_eof() {
        let mut wf = WorkFile::new(1);
        wf.write_record(vec![NaturalValue::Integer(1)]);
        assert!(!wf.is_eof());
        wf.read_record();
        assert!(wf.is_eof());
        assert!(wf.read_record().is_none());
    }

    #[test]
    fn test_work_file_rewind() {
        let mut wf = WorkFile::new(1);
        wf.write_record(vec![NaturalValue::Integer(1)]);
        wf.write_record(vec![NaturalValue::Integer(2)]);
        wf.read_record();
        wf.read_record();
        assert!(wf.is_eof());
        wf.rewind();
        assert!(!wf.is_eof());
        let rec = wf.read_record().unwrap();
        assert_eq!(rec[0].to_i64(), 1);
    }

    #[test]
    fn test_work_file_clear() {
        let mut wf = WorkFile::new(1);
        wf.write_record(vec![NaturalValue::Integer(1)]);
        wf.clear();
        assert_eq!(wf.record_count(), 0);
        assert!(wf.is_eof());
    }

    #[test]
    fn test_work_file_close() {
        let mut wf = WorkFile::new(1);
        assert!(wf.is_open);
        wf.close();
        assert!(!wf.is_open);
    }

    #[test]
    fn test_manager_write_read() {
        let mut mgr = WorkFileManager::new();
        mgr.write_record(1, vec![NaturalValue::Alpha("test".into())]).unwrap();
        let rec = mgr.read_record(1).unwrap().unwrap();
        assert_eq!(rec[0].to_display_string(), "test");
    }

    #[test]
    fn test_manager_invalid_file_number() {
        let mut mgr = WorkFileManager::new();
        assert!(mgr.write_record(0, vec![]).is_err());
        assert!(mgr.write_record(33, vec![]).is_err());
    }

    #[test]
    fn test_manager_multiple_files() {
        let mut mgr = WorkFileManager::new();
        mgr.write_record(1, vec![NaturalValue::Alpha("file1".into())]).unwrap();
        mgr.write_record(2, vec![NaturalValue::Alpha("file2".into())]).unwrap();

        let rec1 = mgr.read_record(1).unwrap().unwrap();
        assert_eq!(rec1[0].to_display_string(), "file1");

        let rec2 = mgr.read_record(2).unwrap().unwrap();
        assert_eq!(rec2[0].to_display_string(), "file2");
    }

    #[test]
    fn test_manager_eof() {
        let mgr = WorkFileManager::new();
        assert!(mgr.is_eof(1));
    }

    #[test]
    fn test_manager_rewind() {
        let mut mgr = WorkFileManager::new();
        mgr.write_record(1, vec![NaturalValue::Integer(42)]).unwrap();
        mgr.read_record(1).unwrap();
        mgr.rewind(1).unwrap();
        let rec = mgr.read_record(1).unwrap().unwrap();
        assert_eq!(rec[0].to_i64(), 42);
    }

    #[test]
    fn test_manager_close() {
        let mut mgr = WorkFileManager::new();
        mgr.write_record(1, vec![NaturalValue::Integer(1)]).unwrap();
        mgr.close(1).unwrap();
        assert!(mgr.write_record(1, vec![]).is_err());
    }

    #[test]
    fn test_manager_close_not_open() {
        let mut mgr = WorkFileManager::new();
        assert!(mgr.close(5).is_err());
    }

    #[test]
    fn test_error_handler_inactive() {
        let mut handler = ErrorHandler::new();
        let err = NaturalError::new(100, 10, "Test error", "PROG1");
        assert!(!handler.trap(err));
    }

    #[test]
    fn test_error_handler_active() {
        let mut handler = ErrorHandler::new();
        handler.activate();
        let err = NaturalError::new(3009, 25, "File not found", "PROG1");
        assert!(handler.trap(err));
        assert_eq!(handler.error_nr(), 3009);
        assert_eq!(handler.error_line(), 25);
    }

    #[test]
    fn test_error_handler_clear() {
        let mut handler = ErrorHandler::new();
        handler.activate();
        handler.trap(NaturalError::new(100, 1, "err", "P"));
        handler.clear();
        assert_eq!(handler.error_nr(), 0);
    }

    #[test]
    fn test_error_handler_deactivate() {
        let mut handler = ErrorHandler::new();
        handler.activate();
        handler.deactivate();
        let err = NaturalError::new(100, 1, "err", "P");
        assert!(!handler.trap(err));
    }

    #[test]
    fn test_stack_input_processing() {
        let mut stack = vec![
            NaturalValue::Alpha("val1".into()),
            NaturalValue::Alpha("val2".into()),
            NaturalValue::Alpha("val3".into()),
        ];
        let values = process_stack_input(&mut stack, 2);
        assert_eq!(values.len(), 2);
        assert_eq!(values[0].to_display_string(), "val1");
        assert_eq!(values[1].to_display_string(), "val2");
        assert_eq!(stack.len(), 1); // one left
    }

    #[test]
    fn test_stack_input_empty() {
        let mut stack: Vec<NaturalValue> = Vec::new();
        let values = process_stack_input(&mut stack, 2);
        assert_eq!(values.len(), 2);
        assert_eq!(values[0], NaturalValue::Null);
        assert_eq!(values[1], NaturalValue::Null);
    }

    #[test]
    fn test_work_file_record_count() {
        let mut wf = WorkFile::new(1);
        assert_eq!(wf.record_count(), 0);
        wf.write_record(vec![NaturalValue::Integer(1)]);
        wf.write_record(vec![NaturalValue::Integer(2)]);
        assert_eq!(wf.record_count(), 2);
    }

    #[test]
    fn test_work_file_multiple_fields() {
        let mut wf = WorkFile::new(1);
        wf.write_record(vec![
            NaturalValue::Alpha("Name".into()),
            NaturalValue::Integer(42),
            NaturalValue::Logical(true),
        ]);
        let rec = wf.read_record().unwrap();
        assert_eq!(rec.len(), 3);
        assert_eq!(rec[2], NaturalValue::Logical(true));
    }

    #[test]
    fn test_natural_error_creation() {
        let err = NaturalError::new(3009, 100, "Record not found", "MYPROG");
        assert_eq!(err.error_nr, 3009);
        assert_eq!(err.error_line, 100);
        assert_eq!(err.error_message, "Record not found");
        assert_eq!(err.program_name, "MYPROG");
    }

    #[test]
    fn test_manager_file_32() {
        let mut mgr = WorkFileManager::new();
        mgr.write_record(32, vec![NaturalValue::Alpha("last".into())]).unwrap();
        let rec = mgr.read_record(32).unwrap().unwrap();
        assert_eq!(rec[0].to_display_string(), "last");
    }
}
