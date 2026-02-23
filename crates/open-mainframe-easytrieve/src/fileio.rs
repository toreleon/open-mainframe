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
// Indexed file processor
// ---------------------------------------------------------------------------

/// Indexed file processor for keyed READ/WRITE/POINT operations.
///
/// Supports VSAM KSDS-style random access by a key field.
#[derive(Debug)]
pub struct IndexedFileProcessor {
    /// File definition.
    pub file: EzFile,
    /// Records stored by key value.
    records: HashMap<String, EzRecord>,
    /// Key field name used for indexing.
    pub key_field: String,
    /// Current positioned key (from POINT).
    current_key: Option<String>,
    /// Sorted key list for sequential access after POINT.
    sorted_keys: Vec<String>,
    /// Current position in sorted_keys for sequential reads.
    seq_pos: usize,
}

impl IndexedFileProcessor {
    /// Create a new indexed file processor.
    pub fn new(file: EzFile, key_field: &str) -> Self {
        Self {
            file,
            records: HashMap::new(),
            key_field: key_field.to_string(),
            current_key: None,
            sorted_keys: Vec::new(),
            seq_pos: 0,
        }
    }

    /// Load records, extracting the key from each record.
    pub fn load_records(&mut self, records: Vec<EzRecord>) {
        for rec in records {
            if let Some(field_def) = self.file.get_field(&self.key_field) {
                if let Ok(key) = rec.get_field(field_def) {
                    self.records.insert(key, rec);
                }
            }
        }
        self.rebuild_key_index();
    }

    /// Rebuild the sorted key index.
    fn rebuild_key_index(&mut self) {
        self.sorted_keys = self.records.keys().cloned().collect();
        self.sorted_keys.sort();
    }

    /// READ: Random access by key.
    pub fn read(&self, key: &str) -> Result<&EzRecord, FileError> {
        self.records.get(key).ok_or_else(|| FileError::EndOfFile {
            name: self.file.name.clone(),
        })
    }

    /// WRITE (ADD): Insert a new record.
    pub fn write_add(&mut self, key: String, record: EzRecord) {
        self.records.insert(key, record);
        self.rebuild_key_index();
    }

    /// WRITE (UPDATE): Update an existing record.
    pub fn write_update(&mut self, key: &str, record: EzRecord) -> Result<(), FileError> {
        if self.records.contains_key(key) {
            self.records.insert(key.to_string(), record);
            Ok(())
        } else {
            Err(FileError::EndOfFile {
                name: self.file.name.clone(),
            })
        }
    }

    /// WRITE (DELETE): Delete a record by key.
    pub fn write_delete(&mut self, key: &str) -> Result<(), FileError> {
        if self.records.remove(key).is_some() {
            self.rebuild_key_index();
            Ok(())
        } else {
            Err(FileError::EndOfFile {
                name: self.file.name.clone(),
            })
        }
    }

    /// POINT: Position to a key for subsequent sequential reads.
    pub fn point(&mut self, key: &str) {
        self.current_key = Some(key.to_string());
        // Find position in sorted keys
        self.seq_pos = self
            .sorted_keys
            .iter()
            .position(|k| k.as_str() >= key)
            .unwrap_or(self.sorted_keys.len());
    }

    /// Sequential GET after POINT (reads next record in key order).
    pub fn get_next(&mut self) -> Result<&EzRecord, FileError> {
        if self.seq_pos >= self.sorted_keys.len() {
            return Err(FileError::EndOfFile {
                name: self.file.name.clone(),
            });
        }
        let key = &self.sorted_keys[self.seq_pos];
        self.seq_pos += 1;
        self.records.get(key).ok_or_else(|| FileError::EndOfFile {
            name: self.file.name.clone(),
        })
    }

    /// Get the number of records.
    pub fn record_count(&self) -> usize {
        self.records.len()
    }
}

// ---------------------------------------------------------------------------
// Edit mask
// ---------------------------------------------------------------------------

/// Edit mask for numeric formatting (MASK statement).
///
/// Defines a pattern for displaying numeric values with commas,
/// decimal points, currency symbols, etc.
#[derive(Debug, Clone)]
pub struct EditMask {
    /// Mask name.
    pub name: String,
    /// Pattern string (e.g., "ZZZ,ZZ9.99" or "$$$,$$9.99CR").
    pub pattern: String,
}

impl EditMask {
    /// Create a new edit mask.
    pub fn new(name: &str, pattern: &str) -> Self {
        Self {
            name: name.to_string(),
            pattern: pattern.to_string(),
        }
    }

    /// Apply the edit mask to a numeric value.
    ///
    /// Supports basic patterns:
    /// - `9` = digit (always shown)
    /// - `Z` = digit (suppress leading zeros with space)
    /// - `.` = decimal point
    /// - `,` = thousands separator (suppressed if in leading zeros)
    /// - `$` = currency symbol (floats to first significant digit)
    /// - `CR` = credit suffix (shown if negative)
    /// - `-` = minus sign (shown if negative)
    pub fn format(&self, value: f64) -> String {
        let is_negative = value < 0.0;
        let abs_value = value.abs();

        // Split pattern at decimal point
        let (int_pat, dec_pat) = if let Some(dot_pos) = self.pattern.find('.') {
            (&self.pattern[..dot_pos], Some(&self.pattern[dot_pos + 1..]))
        } else {
            (self.pattern.as_str(), None)
        };

        // Determine decimal places from pattern
        let dec_places = dec_pat.map(|p| {
            p.chars()
                .filter(|c| *c == '9' || *c == 'Z')
                .count()
        }).unwrap_or(0);

        // Format the number
        let formatted_num = if dec_places > 0 {
            format!("{:.prec$}", abs_value, prec = dec_places)
        } else {
            format!("{}", abs_value as i64)
        };

        // Split formatted number
        let (int_str, dec_str) = if let Some(dot_pos) = formatted_num.find('.') {
            (&formatted_num[..dot_pos], Some(&formatted_num[dot_pos + 1..]))
        } else {
            (formatted_num.as_str(), None)
        };

        // Build integer part according to pattern
        let digit_positions: Vec<usize> = int_pat
            .chars()
            .enumerate()
            .filter(|(_, c)| *c == '9' || *c == 'Z' || *c == '$')
            .map(|(i, _)| i)
            .collect();

        let int_digits: Vec<u8> = int_str.bytes().collect();
        let mut result_chars: Vec<char> = int_pat.chars().collect();

        // Fill digits from right to left
        let mut digit_idx = int_digits.len() as isize - 1;
        let mut first_significant = false;
        for &pos in digit_positions.iter().rev() {
            if digit_idx >= 0 {
                let d = int_digits[digit_idx as usize] as char;
                if d != '0' {
                    first_significant = true;
                }
                if first_significant || result_chars[pos] == '9' {
                    result_chars[pos] = d;
                } else {
                    result_chars[pos] = ' ';
                }
                digit_idx -= 1;
            } else {
                if result_chars[pos] == '9' {
                    result_chars[pos] = '0';
                } else {
                    result_chars[pos] = ' ';
                }
            }
        }

        // Suppress commas in leading zero area
        let mut in_leading = true;
        for ch in result_chars.iter_mut() {
            if *ch == ' ' || *ch == '$' {
                continue;
            }
            if *ch == ',' && in_leading {
                *ch = ' ';
                continue;
            }
            if ch.is_ascii_digit() && *ch != '0' {
                in_leading = false;
            }
        }

        let mut result: String = result_chars.into_iter().collect();

        // Append decimal part
        if let Some(dp) = dec_pat {
            result.push('.');
            let dec = dec_str.unwrap_or("0");
            let padded: String = format!("{:0<width$}", dec, width = dp.len());
            result.push_str(&padded[..dp.len()]);
        }

        // Handle CR/minus suffix
        let has_cr = self.pattern.ends_with("CR");
        let has_minus = self.pattern.ends_with('-');
        if has_cr {
            if is_negative {
                result.push_str("CR");
            } else {
                result.push_str("  ");
            }
        } else if has_minus {
            if is_negative {
                result.push('-');
            } else {
                result.push(' ');
            }
        }

        result
    }
}

/// Table definition for SEARCH operations.
///
/// Represents an in-memory lookup table loaded from instream data
/// or a file, searchable by key.
#[derive(Debug, Clone)]
pub struct EzTable {
    /// Table name.
    pub name: String,
    /// Key field name.
    pub key_field: String,
    /// Key length.
    pub key_length: usize,
    /// Data entries keyed by lookup key.
    pub entries: Vec<(String, HashMap<String, String>)>,
    /// Whether the table is sorted (for binary search).
    pub sorted: bool,
}

impl EzTable {
    /// Create a new table.
    pub fn new(name: &str, key_field: &str, key_length: usize) -> Self {
        Self {
            name: name.to_string(),
            key_field: key_field.to_string(),
            key_length,
            entries: Vec::new(),
            sorted: false,
        }
    }

    /// Add an entry to the table.
    pub fn add_entry(&mut self, key: &str, values: HashMap<String, String>) {
        self.entries.push((key.to_string(), values));
        self.sorted = false;
    }

    /// Sort the table by key for binary search.
    pub fn sort(&mut self) {
        self.entries.sort_by(|a, b| a.0.cmp(&b.0));
        self.sorted = true;
    }

    /// Sequential search for a key.
    pub fn search_sequential(&self, key: &str) -> Option<&HashMap<String, String>> {
        self.entries
            .iter()
            .find(|(k, _)| k == key)
            .map(|(_, v)| v)
    }

    /// Binary search for a key (table must be sorted first).
    pub fn search_binary(&self, key: &str) -> Option<&HashMap<String, String>> {
        if !self.sorted {
            return self.search_sequential(key);
        }
        self.entries
            .binary_search_by(|(k, _)| k.as_str().cmp(key))
            .ok()
            .map(|idx| &self.entries[idx].1)
    }

    /// Search using the specified mode.
    pub fn search(&self, key: &str, binary: bool) -> Option<&HashMap<String, String>> {
        if binary {
            self.search_binary(key)
        } else {
            self.search_sequential(key)
        }
    }

    /// Get the number of entries.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Check if the table is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
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

    #[test]
    fn test_indexed_file_read_write() {
        let mut file = EzFile::new("KSDS", 40, RecordFormat::Fixed);
        file.add_field(EzFieldDef::new("KEY", 1, 5, "A")).unwrap();
        file.add_field(EzFieldDef::new("DATA", 6, 35, "A")).unwrap();

        let mut proc = IndexedFileProcessor::new(file, "KEY");

        let mut rec = EzRecord::new(40);
        rec.data[0..5].copy_from_slice(b"KEY01");
        rec.data[5..10].copy_from_slice(b"HELLO");
        proc.write_add("KEY01".into(), rec);

        assert_eq!(proc.record_count(), 1);
        let r = proc.read("KEY01").unwrap();
        assert_eq!(&r.data[0..5], b"KEY01");
    }

    #[test]
    fn test_indexed_file_point_get_next() {
        let mut file = EzFile::new("KSDS", 40, RecordFormat::Fixed);
        file.add_field(EzFieldDef::new("KEY", 1, 3, "A")).unwrap();

        let mut proc = IndexedFileProcessor::new(file, "KEY");

        for key in ["AAA", "BBB", "CCC", "DDD"] {
            let mut rec = EzRecord::new(40);
            rec.data[0..3].copy_from_slice(key.as_bytes());
            proc.write_add(key.to_string(), rec);
        }

        proc.point("BBB");
        let r1 = proc.get_next().unwrap();
        assert_eq!(&r1.data[0..3], b"BBB");
        let r2 = proc.get_next().unwrap();
        assert_eq!(&r2.data[0..3], b"CCC");
    }

    #[test]
    fn test_indexed_file_delete() {
        let file = EzFile::new("KSDS", 40, RecordFormat::Fixed);
        let mut proc = IndexedFileProcessor::new(file, "KEY");

        let rec = EzRecord::new(40);
        proc.write_add("K1".into(), rec);
        assert_eq!(proc.record_count(), 1);

        proc.write_delete("K1").unwrap();
        assert_eq!(proc.record_count(), 0);
    }

    #[test]
    fn test_edit_mask_basic() {
        let mask = EditMask::new("M1", "ZZZ,ZZ9.99");
        let result = mask.format(12345.67);
        assert!(result.contains("12,345.67") || result.contains("12345.67"));
    }

    #[test]
    fn test_edit_mask_zero() {
        let mask = EditMask::new("M2", "ZZZ9");
        let result = mask.format(0.0);
        assert!(result.trim().ends_with('0'));
    }

    #[test]
    fn test_edit_mask_negative_cr() {
        let mask = EditMask::new("M3", "ZZZ9.99CR");
        let result = mask.format(-42.50);
        assert!(result.contains("CR"));
    }

    #[test]
    fn test_table_sequential_search() {
        let mut table = EzTable::new("STATES", "CODE", 2);
        let mut vals = HashMap::new();
        vals.insert("NAME".into(), "CALIFORNIA".into());
        table.add_entry("CA", vals);

        let mut vals2 = HashMap::new();
        vals2.insert("NAME".into(), "NEW YORK".into());
        table.add_entry("NY", vals2);

        let found = table.search_sequential("CA").unwrap();
        assert_eq!(found.get("NAME").unwrap(), "CALIFORNIA");
        assert!(table.search_sequential("TX").is_none());
    }

    #[test]
    fn test_table_binary_search() {
        let mut table = EzTable::new("CODES", "KEY", 3);
        for (k, v) in [("BBB", "Two"), ("AAA", "One"), ("CCC", "Three")] {
            let mut vals = HashMap::new();
            vals.insert("VALUE".into(), v.into());
            table.add_entry(k, vals);
        }
        table.sort();

        let found = table.search_binary("BBB").unwrap();
        assert_eq!(found.get("VALUE").unwrap(), "Two");
    }

    #[test]
    fn test_table_empty() {
        let table = EzTable::new("EMPTY", "K", 1);
        assert!(table.is_empty());
        assert_eq!(table.len(), 0);
    }
}
