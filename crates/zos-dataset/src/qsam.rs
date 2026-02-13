//! QSAM (Queued Sequential Access Method) implementation.
//!
//! Provides sequential file access compatible with IBM mainframe QSAM.
//! Supports READ, WRITE, OPEN, and CLOSE operations for sequential datasets.

use std::fs::{File, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Read, Write};

use crate::error::DatasetError;
use crate::types::{DatasetRef, RecordFormat};

/// Open mode for QSAM files.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpenMode {
    /// Open for reading.
    Input,
    /// Open for writing (create/truncate).
    Output,
    /// Open for appending.
    Extend,
    /// Open for reading and writing.
    InputOutput,
}

/// A sequential dataset reader.
pub struct QsamReader {
    /// The dataset reference.
    dataset: DatasetRef,
    /// Buffered reader.
    reader: BufReader<File>,
    /// Current record number (1-indexed).
    record_number: u64,
    /// End of file flag.
    eof: bool,
    /// Buffer for current record.
    record_buffer: Vec<u8>,
}

impl QsamReader {
    /// Open a dataset for reading.
    pub fn open(dataset: DatasetRef) -> Result<Self, DatasetError> {
        let path = dataset
            .path
            .as_ref()
            .ok_or_else(|| DatasetError::NotFound {
                name: dataset.dsn.clone(),
            })?;

        let file = File::open(path).map_err(|e| DatasetError::IoError {
            message: format!("Failed to open {}: {}", path.display(), e),
        })?;

        let lrecl = dataset.attributes.lrecl as usize;
        let reader = BufReader::with_capacity(dataset.attributes.blksize as usize, file);

        Ok(Self {
            dataset,
            reader,
            record_number: 0,
            eof: false,
            record_buffer: vec![0u8; lrecl],
        })
    }

    /// Read the next record.
    pub fn read(&mut self) -> Result<Option<&[u8]>, DatasetError> {
        if self.eof {
            return Ok(None);
        }

        let lrecl = self.dataset.attributes.lrecl as usize;

        match self.dataset.attributes.recfm {
            RecordFormat::Fixed | RecordFormat::FixedBlocked => self.read_fixed_record(lrecl),
            RecordFormat::Variable | RecordFormat::VariableBlocked => self.read_variable_record(),
            RecordFormat::Undefined => self.read_line_record(),
            _ => self.read_line_record(),
        }
    }

    /// Read a fixed-length record.
    fn read_fixed_record(&mut self, lrecl: usize) -> Result<Option<&[u8]>, DatasetError> {
        self.record_buffer.resize(lrecl, 0);

        match self.reader.read_exact(&mut self.record_buffer) {
            Ok(()) => {
                self.record_number += 1;
                Ok(Some(&self.record_buffer))
            }
            Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                self.eof = true;
                Ok(None)
            }
            Err(e) => Err(DatasetError::IoError {
                message: format!("Read error at record {}: {}", self.record_number + 1, e),
            }),
        }
    }

    /// Read a variable-length record (with RDW).
    fn read_variable_record(&mut self) -> Result<Option<&[u8]>, DatasetError> {
        // Read Record Descriptor Word (4 bytes: 2-byte length + 2 reserved)
        let mut rdw = [0u8; 4];
        match self.reader.read_exact(&mut rdw) {
            Ok(()) => {}
            Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                self.eof = true;
                return Ok(None);
            }
            Err(e) => {
                return Err(DatasetError::IoError {
                    message: format!("Failed to read RDW: {}", e),
                });
            }
        }

        // RDW length is big-endian, includes the RDW itself
        let length = ((rdw[0] as usize) << 8) | (rdw[1] as usize);
        if length < 4 {
            return Err(DatasetError::InvalidRecordFormat(format!(
                "Invalid RDW length: {}",
                length
            )));
        }

        let data_len = length - 4;
        self.record_buffer.resize(data_len, 0);

        self.reader
            .read_exact(&mut self.record_buffer)
            .map_err(|e| DatasetError::IoError {
                message: format!("Failed to read record data: {}", e),
            })?;

        self.record_number += 1;
        Ok(Some(&self.record_buffer))
    }

    /// Read a line-based record (for text files).
    fn read_line_record(&mut self) -> Result<Option<&[u8]>, DatasetError> {
        let mut line = String::new();
        match self.reader.read_line(&mut line) {
            Ok(0) => {
                self.eof = true;
                Ok(None)
            }
            Ok(_) => {
                // Remove trailing newline
                let trimmed = line.trim_end_matches(['\n', '\r']);
                self.record_buffer.clear();
                self.record_buffer.extend_from_slice(trimmed.as_bytes());

                // Pad to LRECL if fixed format
                let lrecl = self.dataset.attributes.lrecl as usize;
                if !self.dataset.attributes.recfm.is_variable() && self.record_buffer.len() < lrecl
                {
                    self.record_buffer.resize(lrecl, b' ');
                }

                self.record_number += 1;
                Ok(Some(&self.record_buffer))
            }
            Err(e) => Err(DatasetError::IoError {
                message: format!("Read error: {}", e),
            }),
        }
    }

    /// Get current record number.
    pub fn record_number(&self) -> u64 {
        self.record_number
    }

    /// Check if at end of file.
    pub fn is_eof(&self) -> bool {
        self.eof
    }

    /// Get dataset reference.
    pub fn dataset(&self) -> &DatasetRef {
        &self.dataset
    }
}

/// A sequential dataset writer.
pub struct QsamWriter {
    /// The dataset reference.
    dataset: DatasetRef,
    /// Buffered writer.
    writer: BufWriter<File>,
    /// Current record number (1-indexed).
    record_number: u64,
}

impl QsamWriter {
    /// Open a dataset for writing.
    pub fn open(dataset: DatasetRef, mode: OpenMode) -> Result<Self, DatasetError> {
        let path = dataset
            .path
            .as_ref()
            .ok_or_else(|| DatasetError::NotFound {
                name: dataset.dsn.clone(),
            })?;

        // Create parent directories if needed
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| DatasetError::IoError {
                message: format!("Failed to create directory: {}", e),
            })?;
        }

        let file = match mode {
            OpenMode::Output => OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(path),
            OpenMode::Extend => OpenOptions::new().create(true).append(true).open(path),
            OpenMode::InputOutput => OpenOptions::new()
                .read(true)
                .write(true)
                .create(true)
                .truncate(false)
                .open(path),
            OpenMode::Input => {
                return Err(DatasetError::IoError {
                    message: "Cannot open writer in Input mode".to_string(),
                });
            }
        }
        .map_err(|e| DatasetError::IoError {
            message: format!("Failed to open {}: {}", path.display(), e),
        })?;

        let writer = BufWriter::with_capacity(dataset.attributes.blksize as usize, file);

        Ok(Self {
            dataset,
            writer,
            record_number: 0,
        })
    }

    /// Write a record.
    pub fn write(&mut self, data: &[u8]) -> Result<(), DatasetError> {
        let lrecl = self.dataset.attributes.lrecl as usize;

        match self.dataset.attributes.recfm {
            RecordFormat::Fixed | RecordFormat::FixedBlocked => {
                self.write_fixed_record(data, lrecl)
            }
            RecordFormat::Variable | RecordFormat::VariableBlocked => {
                self.write_variable_record(data)
            }
            _ => self.write_line_record(data),
        }
    }

    /// Write a fixed-length record.
    fn write_fixed_record(&mut self, data: &[u8], lrecl: usize) -> Result<(), DatasetError> {
        let mut record = vec![b' '; lrecl];
        let copy_len = data.len().min(lrecl);
        record[..copy_len].copy_from_slice(&data[..copy_len]);

        self.writer
            .write_all(&record)
            .map_err(|e| DatasetError::IoError {
                message: format!("Write error at record {}: {}", self.record_number + 1, e),
            })?;

        self.record_number += 1;
        Ok(())
    }

    /// Write a variable-length record (with RDW).
    fn write_variable_record(&mut self, data: &[u8]) -> Result<(), DatasetError> {
        let length = data.len() + 4; // Include RDW size
        if length > 65535 {
            return Err(DatasetError::InvalidRecordFormat(format!(
                "Record too long: {} bytes",
                data.len()
            )));
        }

        // Write RDW (big-endian length + 2 reserved bytes)
        let rdw = [((length >> 8) & 0xFF) as u8, (length & 0xFF) as u8, 0, 0];
        self.writer
            .write_all(&rdw)
            .map_err(|e| DatasetError::IoError {
                message: format!("Failed to write RDW: {}", e),
            })?;

        self.writer
            .write_all(data)
            .map_err(|e| DatasetError::IoError {
                message: format!("Failed to write record: {}", e),
            })?;

        self.record_number += 1;
        Ok(())
    }

    /// Write a line-based record.
    fn write_line_record(&mut self, data: &[u8]) -> Result<(), DatasetError> {
        self.writer
            .write_all(data)
            .map_err(|e| DatasetError::IoError {
                message: format!("Write error: {}", e),
            })?;

        self.writer
            .write_all(b"\n")
            .map_err(|e| DatasetError::IoError {
                message: format!("Write error: {}", e),
            })?;

        self.record_number += 1;
        Ok(())
    }

    /// Flush buffered data to disk.
    pub fn flush(&mut self) -> Result<(), DatasetError> {
        self.writer.flush().map_err(|e| DatasetError::IoError {
            message: format!("Flush error: {}", e),
        })
    }

    /// Get current record number.
    pub fn record_number(&self) -> u64 {
        self.record_number
    }

    /// Get dataset reference.
    pub fn dataset(&self) -> &DatasetRef {
        &self.dataset
    }
}

impl Drop for QsamWriter {
    fn drop(&mut self) {
        let _ = self.flush();
    }
}

/// Read all records from a sequential dataset.
pub fn read_all_records(dataset: DatasetRef) -> Result<Vec<Vec<u8>>, DatasetError> {
    let mut reader = QsamReader::open(dataset)?;
    let mut records = Vec::new();

    while let Some(record) = reader.read()? {
        records.push(record.to_vec());
    }

    Ok(records)
}

/// Write records to a sequential dataset.
pub fn write_records(dataset: DatasetRef, records: &[&[u8]]) -> Result<u64, DatasetError> {
    let mut writer = QsamWriter::open(dataset, OpenMode::Output)?;

    for record in records {
        writer.write(record)?;
    }

    writer.flush()?;
    Ok(writer.record_number())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::DatasetAttributes;
    use std::path::PathBuf;

    fn test_dataset(path: &str) -> DatasetRef {
        DatasetRef {
            dsn: "TEST.DATASET".to_string(),
            member: None,
            disp: Default::default(),
            attributes: DatasetAttributes::default(),
            path: Some(PathBuf::from(path)),
        }
    }

    #[test]
    fn test_write_and_read_fixed() {
        let temp_dir = std::env::temp_dir();
        let path = temp_dir.join("test_qsam_fixed.dat");

        // Write
        {
            let dataset = test_dataset(path.to_str().unwrap());
            let mut writer = QsamWriter::open(dataset, OpenMode::Output).unwrap();
            writer.write(b"RECORD ONE").unwrap();
            writer.write(b"RECORD TWO").unwrap();
            writer.write(b"RECORD THREE").unwrap();
            writer.flush().unwrap();
            assert_eq!(writer.record_number(), 3);
        }

        // Read
        {
            let dataset = test_dataset(path.to_str().unwrap());
            let mut reader = QsamReader::open(dataset).unwrap();

            let rec1 = reader.read().unwrap().unwrap();
            assert!(rec1.starts_with(b"RECORD ONE"));

            let rec2 = reader.read().unwrap().unwrap();
            assert!(rec2.starts_with(b"RECORD TWO"));

            let rec3 = reader.read().unwrap().unwrap();
            assert!(rec3.starts_with(b"RECORD THREE"));

            assert!(reader.read().unwrap().is_none());
            assert!(reader.is_eof());
        }

        // Cleanup
        std::fs::remove_file(path).ok();
    }

    #[test]
    fn test_write_and_read_variable() {
        let temp_dir = std::env::temp_dir();
        let path = temp_dir.join("test_qsam_variable.dat");

        // Write
        {
            let mut dataset = test_dataset(path.to_str().unwrap());
            dataset.attributes.recfm = RecordFormat::Variable;
            let mut writer = QsamWriter::open(dataset, OpenMode::Output).unwrap();
            writer.write(b"SHORT").unwrap();
            writer.write(b"A MUCH LONGER RECORD").unwrap();
            writer.flush().unwrap();
        }

        // Read
        {
            let mut dataset = test_dataset(path.to_str().unwrap());
            dataset.attributes.recfm = RecordFormat::Variable;
            let mut reader = QsamReader::open(dataset).unwrap();

            let rec1 = reader.read().unwrap().unwrap();
            assert_eq!(rec1, b"SHORT");

            let rec2 = reader.read().unwrap().unwrap();
            assert_eq!(rec2, b"A MUCH LONGER RECORD");
        }

        // Cleanup
        std::fs::remove_file(path).ok();
    }

    #[test]
    fn test_open_mode_extend() {
        let temp_dir = std::env::temp_dir();
        let path = temp_dir.join("test_qsam_extend.dat");

        // Initial write
        {
            let dataset = test_dataset(path.to_str().unwrap());
            let mut writer = QsamWriter::open(dataset, OpenMode::Output).unwrap();
            writer.write(b"FIRST").unwrap();
        }

        // Extend
        {
            let dataset = test_dataset(path.to_str().unwrap());
            let mut writer = QsamWriter::open(dataset, OpenMode::Extend).unwrap();
            writer.write(b"SECOND").unwrap();
        }

        // Verify
        {
            let dataset = test_dataset(path.to_str().unwrap());
            let records = read_all_records(dataset).unwrap();
            assert_eq!(records.len(), 2);
        }

        // Cleanup
        std::fs::remove_file(path).ok();
    }
}
