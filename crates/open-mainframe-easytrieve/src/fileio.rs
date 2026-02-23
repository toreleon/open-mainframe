//! EZ-102: File Processing for Easytrieve Plus.
//!
//! Handles FILE definitions with record layouts (LRECL, RECFM),
//! field definitions within records, and GET/PUT sequential access.

use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

/// Errors during file processing.
#[derive(Debug, Error, Diagnostic)]
pub enum FileError {
    /// File not defined.
    #[error("file '{name}' not defined")]
    FileNotDefined {
        /// File name.
        name: String,
    },
    /// Field not defined in file.
    #[error("field '{field}' not defined in file '{file}'")]
    FieldNotDefined {
        /// Field name.
        field: String,
        /// File name.
        file: String,
    },
    /// Record length mismatch.
    #[error("record length mismatch: expected {expected}, got {got}")]
    RecordLengthMismatch {
        /// Expected length.
        expected: usize,
        /// Actual length.
        got: usize,
    },
    /// End of file reached.
    #[error("end of file reached for '{name}'")]
    EndOfFile {
        /// File name.
        name: String,
    },
    /// Invalid field position.
    #[error("field '{field}' at position {position} with length {length} exceeds record length {lrecl}")]
    InvalidFieldPosition {
        /// Field name.
        field: String,
        /// Start position.
        position: usize,
        /// Field length.
        length: usize,
        /// Record length.
        lrecl: usize,
    },
}

// ---------------------------------------------------------------------------
// Record format
// ---------------------------------------------------------------------------

/// Record format specification (RECFM).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecordFormat {
    /// Fixed-length records.
    Fixed,
    /// Fixed-length blocked records.
    FixedBlocked,
    /// Variable-length records.
    Variable,
    /// Variable-length blocked records.
    VariableBlocked,
}

impl RecordFormat {
    /// Parse a RECFM string (F, FB, V, VB).
    pub fn parse(s: &str) -> Self {
        match s.to_uppercase().as_str() {
            "F" => Self::Fixed,
            "FB" => Self::FixedBlocked,
            "V" => Self::Variable,
            "VB" => Self::VariableBlocked,
            _ => Self::Fixed,
        }
    }
}

// ---------------------------------------------------------------------------
// Field definition
// ---------------------------------------------------------------------------

/// Field definition within a record layout.
///
/// Describes position, length, and type of a field within an Easytrieve record.
#[derive(Debug, Clone)]
pub struct EzFieldDef {
    /// Field name.
    pub name: String,
    /// Start position within record (1-based).
    pub position: usize,
    /// Field length in bytes.
    pub length: usize,
    /// Data type (A=alpha, N=numeric, P=packed, B=binary).
    pub data_type: String,
    /// Optional heading text for reports.
    pub heading: Option<String>,
}

impl EzFieldDef {
    /// Create a new field definition.
    pub fn new(name: &str, position: usize, length: usize, data_type: &str) -> Self {
        Self {
            name: name.to_string(),
            position,
            length,
            data_type: data_type.to_string(),
            heading: None,
        }
    }

    /// Set the heading text for report output.
    pub fn with_heading(mut self, heading: &str) -> Self {
        self.heading = Some(heading.to_string());
        self
    }
}

// ---------------------------------------------------------------------------
// File definition
// ---------------------------------------------------------------------------

/// Easytrieve file definition with record layout.
///
/// Represents a FILE declaration with its LRECL, RECFM, and field definitions.
#[derive(Debug, Clone)]
pub struct EzFile {
    /// File identifier name (DD name).
    pub name: String,
    /// Logical record length.
    pub lrecl: usize,
    /// Record format.
    pub recfm: RecordFormat,
    /// Field definitions within the record, keyed by name.
    pub fields: HashMap<String, EzFieldDef>,
    /// Field names in definition order.
    pub field_order: Vec<String>,
}

impl EzFile {
    /// Create a new file definition.
    pub fn new(name: &str, lrecl: usize, recfm: RecordFormat) -> Self {
        Self {
            name: name.to_string(),
            lrecl,
            recfm,
            fields: HashMap::new(),
            field_order: Vec::new(),
        }
    }

    /// Add a field definition to this file.
    pub fn add_field(&mut self, field: EzFieldDef) -> Result<(), FileError> {
        // Validate field fits within record
        if field.position + field.length - 1 > self.lrecl {
            return Err(FileError::InvalidFieldPosition {
                field: field.name.clone(),
                position: field.position,
                length: field.length,
                lrecl: self.lrecl,
            });
        }
        self.field_order.push(field.name.clone());
        self.fields.insert(field.name.clone(), field);
        Ok(())
    }

    /// Get a field definition by name.
    pub fn get_field(&self, name: &str) -> Option<&EzFieldDef> {
        self.fields.get(name)
    }
}

// ---------------------------------------------------------------------------
// Record buffer
// ---------------------------------------------------------------------------

/// Record buffer with field-level access.
///
/// Provides named access to fields within a fixed-length record buffer.
#[derive(Debug, Clone)]
pub struct EzRecord {
    /// Raw record data.
    pub data: Vec<u8>,
    /// Record length.
    pub length: usize,
}

impl EzRecord {
    /// Create a new empty record buffer of the given length.
    pub fn new(length: usize) -> Self {
        Self {
            data: vec![b' '; length],
            length,
        }
    }

    /// Create a record from raw data.
    pub fn from_data(data: Vec<u8>) -> Self {
        let length = data.len();
        Self { data, length }
    }

    /// Get a field value as a string, using the field definition.
    pub fn get_field(&self, field: &EzFieldDef) -> Result<String, FileError> {
        let start = field.position - 1; // Convert to 0-based
        let end = start + field.length;
        if end > self.data.len() {
            return Err(FileError::RecordLengthMismatch {
                expected: end,
                got: self.data.len(),
            });
        }
        let bytes = &self.data[start..end];
        Ok(String::from_utf8_lossy(bytes).trim_end().to_string())
    }

    /// Set a field value within the record buffer.
    pub fn set_field(&mut self, field: &EzFieldDef, value: &str) -> Result<(), FileError> {
        let start = field.position - 1;
        let end = start + field.length;
        if end > self.data.len() {
            return Err(FileError::RecordLengthMismatch {
                expected: end,
                got: self.data.len(),
            });
        }
        // Pad or truncate value to field length
        let padded = if value.len() >= field.length {
            &value[..field.length]
        } else {
            // Pad with spaces for alpha, zeros for numeric
            let pad_char = if field.data_type == "N" || field.data_type == "P" {
                b'0'
            } else {
                b' '
            };
            let mut buf = vec![pad_char; field.length];
            // Right-justify numbers, left-justify alpha
            if field.data_type == "N" || field.data_type == "P" {
                let offset = field.length - value.len();
                buf[offset..].copy_from_slice(value.as_bytes());
            } else {
                buf[..value.len()].copy_from_slice(value.as_bytes());
            }
            self.data[start..end].copy_from_slice(&buf);
            return Ok(());
        };
        self.data[start..end].copy_from_slice(padded.as_bytes());
        Ok(())
    }

    /// Get the entire record as a string.
    pub fn as_string(&self) -> String {
        String::from_utf8_lossy(&self.data).to_string()
    }
}

// ---------------------------------------------------------------------------
// File processor
// ---------------------------------------------------------------------------

/// File processor for sequential GET/PUT operations.
///
/// Manages a set of records for reading (GET) and writing (PUT).
#[derive(Debug)]
pub struct FileProcessor {
    /// File definition.
    pub file: EzFile,
    /// Input records for GET.
    input_records: Vec<EzRecord>,
    /// Output records from PUT.
    output_records: Vec<EzRecord>,
    /// Current read position.
    read_pos: usize,
}

impl FileProcessor {
    /// Create a new file processor.
    pub fn new(file: EzFile) -> Self {
        Self {
            file,
            input_records: Vec::new(),
            output_records: Vec::new(),
            read_pos: 0,
        }
    }

    /// Load input records for GET processing.
    pub fn load_records(&mut self, records: Vec<EzRecord>) {
        self.input_records = records;
        self.read_pos = 0;
    }

    /// GET: Read the next record sequentially.
    pub fn get(&mut self) -> Result<&EzRecord, FileError> {
        if self.read_pos >= self.input_records.len() {
            return Err(FileError::EndOfFile {
                name: self.file.name.clone(),
            });
        }
        let record = &self.input_records[self.read_pos];
        self.read_pos += 1;
        Ok(record)
    }

    /// PUT: Write a record to output.
    pub fn put(&mut self, record: EzRecord) {
        self.output_records.push(record);
    }

    /// Get all output records.
    pub fn get_output(&self) -> &[EzRecord] {
        &self.output_records
    }

    /// Reset read position to beginning.
    pub fn rewind(&mut self) {
        self.read_pos = 0;
    }

    /// Check if there are more records to read.
    pub fn has_more(&self) -> bool {
        self.read_pos < self.input_records.len()
    }

    /// Get the number of input records.
    pub fn record_count(&self) -> usize {
        self.input_records.len()
    }

    /// Get the number of output records.
    pub fn output_count(&self) -> usize {
        self.output_records.len()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_file() -> EzFile {
        let mut file = EzFile::new("TESTFILE", 80, RecordFormat::Fixed);
        file.add_field(EzFieldDef::new("NAME", 1, 20, "A")).unwrap();
        file.add_field(EzFieldDef::new("DEPT", 21, 10, "A")).unwrap();
        file.add_field(EzFieldDef::new("SALARY", 31, 8, "N")).unwrap();
        file
    }

    #[test]
    fn test_file_creation() {
        let file = sample_file();
        assert_eq!(file.name, "TESTFILE");
        assert_eq!(file.lrecl, 80);
        assert_eq!(file.fields.len(), 3);
        assert_eq!(file.field_order, vec!["NAME", "DEPT", "SALARY"]);
    }

    #[test]
    fn test_field_position_validation() {
        let mut file = EzFile::new("TEST", 20, RecordFormat::Fixed);
        let result = file.add_field(EzFieldDef::new("TOOLONG", 15, 10, "A"));
        assert!(result.is_err());
    }

    #[test]
    fn test_record_field_access() {
        let file = sample_file();
        let mut record = EzRecord::new(80);

        let name_field = file.get_field("NAME").unwrap();
        record.set_field(name_field, "JOHN DOE").unwrap();

        let salary_field = file.get_field("SALARY").unwrap();
        record.set_field(salary_field, "50000").unwrap();

        assert_eq!(record.get_field(name_field).unwrap(), "JOHN DOE");
        assert_eq!(record.get_field(salary_field).unwrap(), "00050000");
    }

    #[test]
    fn test_record_from_data() {
        let mut data = vec![b' '; 80];
        data[0..8].copy_from_slice(b"JANE DOE");
        let record = EzRecord::from_data(data);
        assert_eq!(record.length, 80);
    }

    #[test]
    fn test_file_processor_get_put() {
        let file = sample_file();
        let mut processor = FileProcessor::new(file);

        // Load some records
        let mut rec1 = EzRecord::new(80);
        rec1.data[0..4].copy_from_slice(b"REC1");
        let mut rec2 = EzRecord::new(80);
        rec2.data[0..4].copy_from_slice(b"REC2");

        processor.load_records(vec![rec1, rec2]);
        assert_eq!(processor.record_count(), 2);
        assert!(processor.has_more());

        // GET records
        let r1 = processor.get().unwrap();
        assert_eq!(&r1.data[0..4], b"REC1");

        let r2 = processor.get().unwrap();
        assert_eq!(&r2.data[0..4], b"REC2");

        // End of file
        assert!(!processor.has_more());
        assert!(processor.get().is_err());
    }

    #[test]
    fn test_file_processor_put() {
        let file = EzFile::new("OUTFILE", 80, RecordFormat::Fixed);
        let mut processor = FileProcessor::new(file);

        let rec = EzRecord::new(80);
        processor.put(rec);

        assert_eq!(processor.output_count(), 1);
        assert_eq!(processor.get_output().len(), 1);
    }

    #[test]
    fn test_file_processor_rewind() {
        let file = EzFile::new("TEST", 40, RecordFormat::Fixed);
        let mut processor = FileProcessor::new(file);
        processor.load_records(vec![EzRecord::new(40), EzRecord::new(40)]);

        let _ = processor.get().unwrap();
        let _ = processor.get().unwrap();
        assert!(!processor.has_more());

        processor.rewind();
        assert!(processor.has_more());
        let _ = processor.get().unwrap();
    }

    #[test]
    fn test_record_format_parsing() {
        assert_eq!(RecordFormat::parse("F"), RecordFormat::Fixed);
        assert_eq!(RecordFormat::parse("FB"), RecordFormat::FixedBlocked);
        assert_eq!(RecordFormat::parse("V"), RecordFormat::Variable);
        assert_eq!(RecordFormat::parse("VB"), RecordFormat::VariableBlocked);
        assert_eq!(RecordFormat::parse("X"), RecordFormat::Fixed); // default
    }

    #[test]
    fn test_field_with_heading() {
        let field = EzFieldDef::new("AMOUNT", 1, 10, "N").with_heading("Total Amount");
        assert_eq!(field.heading.as_deref(), Some("Total Amount"));
    }
}
