//! SMF Record Writer.
//!
//! Provides an in-memory writer that collects SMF records and serialises them
//! in the standard z/OS binary format.  Also supports writing to a log file.
//!
//! On real z/OS, SMF records are written via the SMFWTM / SMFEWTM macros to
//! the SMF recording dataset (SYS1.MANx).  This emulation writes to an
//! in-memory buffer or an on-disk file.
//!
//! ## SMF-102 enhancements
//! - SMFWTM — standard record write (with NOTYPE suppression)
//! - SMFEWTM — extended write with subsystem identification
//! - Buffer management with configurable flush intervals and max size
//! - User record types (128-255) support

use crate::config::SmfPrmConfig;
use crate::exits::{SmfExitAction, SmfExitRegistry};
use crate::record::{SmfRecord, SmfRecordType};

// ---------------------------------------------------------------------------
//  Writer config
// ---------------------------------------------------------------------------

/// Configuration for the SMF writer.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SmfWriterConfig {
    /// System ID to stamp into every record header.
    pub system_id: String,
    /// Maximum record size (bytes).  z/OS default is 32 760.
    pub max_record_size: usize,
    /// Record types to collect (empty = all).
    pub enabled_types: Vec<SmfRecordType>,
    /// Whether to validate records before writing.
    pub validate: bool,
    /// Maximum buffer size in bytes before auto-flush.
    pub max_buffer_size: usize,
}

impl Default for SmfWriterConfig {
    fn default() -> Self {
        Self {
            system_id: "SYS1".to_string(),
            max_record_size: 32760,
            enabled_types: Vec::new(),
            validate: true,
            max_buffer_size: 65536,
        }
    }
}

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors produced by the SMF writer.
#[derive(Debug, thiserror::Error)]
pub enum SmfWriterError {
    /// Record exceeds the maximum allowed size.
    #[error("SMF record size {size} exceeds maximum {max}")]
    RecordTooLarge { size: usize, max: usize },

    /// Record type is not enabled in the configuration.
    #[error("SMF record type {0} is not enabled")]
    TypeNotEnabled(u8),

    /// Record type is suppressed by NOTYPE configuration.
    #[error("SMF record type {0} is suppressed (NOTYPE)")]
    TypeSuppressed(u8),

    /// Record was suppressed by an exit.
    #[error("SMF record suppressed by exit")]
    SuppressedByExit,

    /// I/O error while writing to a file target.
    #[error("SMF I/O error: {0}")]
    Io(#[from] std::io::Error),
}

// ---------------------------------------------------------------------------
//  Self-defining section triplet
// ---------------------------------------------------------------------------

/// A self-defining section triplet (offset / length / number).
///
/// Many SMF record types use triplets to describe variable-length sections
/// within the record.
#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub struct SmfTriplet {
    /// Offset from start of the record to the section.
    pub offset: u32,
    /// Length of each entry.
    pub entry_length: u16,
    /// Number of entries.
    pub entry_count: u16,
}

impl SmfTriplet {
    /// Create a new triplet.
    pub fn new(offset: u32, entry_length: u16, entry_count: u16) -> Self {
        Self {
            offset,
            entry_length,
            entry_count,
        }
    }

    /// Serialize the triplet to 8 bytes (big-endian).
    pub fn to_bytes(&self) -> [u8; 8] {
        let mut buf = [0u8; 8];
        buf[0] = (self.offset >> 24) as u8;
        buf[1] = (self.offset >> 16) as u8;
        buf[2] = (self.offset >> 8) as u8;
        buf[3] = self.offset as u8;
        buf[4] = (self.entry_length >> 8) as u8;
        buf[5] = self.entry_length as u8;
        buf[6] = (self.entry_count >> 8) as u8;
        buf[7] = self.entry_count as u8;
        buf
    }

    /// Deserialize from 8 bytes.
    pub fn from_bytes(bytes: &[u8; 8]) -> Self {
        let offset =
            (bytes[0] as u32) << 24 | (bytes[1] as u32) << 16 | (bytes[2] as u32) << 8 | bytes[3] as u32;
        let entry_length = (bytes[4] as u16) << 8 | bytes[5] as u16;
        let entry_count = (bytes[6] as u16) << 8 | bytes[7] as u16;
        Self {
            offset,
            entry_length,
            entry_count,
        }
    }
}

// ---------------------------------------------------------------------------
//  SMF Writer
// ---------------------------------------------------------------------------

/// An SMF record writer.
///
/// Collects serialised SMF records in memory.  Records can later be retrieved
/// as raw bytes or drained as a list.
#[derive(Debug)]
pub struct SmfWriter {
    config: SmfWriterConfig,
    /// Collected raw records (each entry is a serialised record).
    records: Vec<Vec<u8>>,
    /// Sequence counter.
    sequence: u64,
    /// Total bytes written.
    total_bytes: u64,
    /// Count by record type (indexed by type code).
    type_counts: [u64; 256],
    /// Current buffer size in bytes.
    buffer_bytes: usize,
    /// Flushed records (moved from buffer on flush).
    flushed_records: Vec<Vec<u8>>,
    /// Total flush count.
    flush_count: u64,
}

impl SmfWriter {
    /// Create a new writer with the given configuration.
    pub fn new(config: SmfWriterConfig) -> Self {
        Self {
            config,
            records: Vec::new(),
            sequence: 0,
            total_bytes: 0,
            type_counts: [0; 256],
            buffer_bytes: 0,
            flushed_records: Vec::new(),
            flush_count: 0,
        }
    }

    /// Create a writer with default configuration.
    pub fn with_defaults() -> Self {
        Self::new(SmfWriterConfig::default())
    }

    /// SMFWTM — write an SMF record.
    ///
    /// If `prm_config` is provided, respects TYPE/NOTYPE suppression.
    /// Records in the NOTYPE list are silently dropped.
    pub fn smfwtm(
        &mut self,
        record: &SmfRecord,
        prm_config: Option<&SmfPrmConfig>,
    ) -> Result<u64, SmfWriterError> {
        // Check NOTYPE suppression.
        if let Some(prm) = prm_config {
            if !prm.is_type_active(record.header.record_type) {
                return Err(SmfWriterError::TypeSuppressed(record.header.record_type));
            }
        }
        self.write(record)
    }

    /// SMFEWTM — extended write with subsystem identification.
    pub fn smfewtm(
        &mut self,
        record: &SmfRecord,
        subsystem_id: &str,
    ) -> Result<u64, SmfWriterError> {
        let mut rec = record.clone();
        rec.header.subsystem_id = subsystem_id.to_string();
        self.write(&rec)
    }

    /// Write an SMF record through the exit pipeline.
    pub fn write_with_exits(
        &mut self,
        record: &SmfRecord,
        exits: &SmfExitRegistry,
    ) -> Result<u64, SmfWriterError> {
        let mut rec = record.clone();
        let action = exits.process(&mut rec);
        match action {
            SmfExitAction::Suppress => Err(SmfWriterError::SuppressedByExit),
            _ => self.write(&rec),
        }
    }

    /// Write an SMF record.
    pub fn write(&mut self, record: &SmfRecord) -> Result<u64, SmfWriterError> {
        if self.config.validate {
            self.validate(record)?;
        }

        let mut rec = record.clone();
        // Stamp system ID from config.
        rec.header.system_id = self.config.system_id.clone();

        let bytes = rec.to_bytes();
        let bytes_len = bytes.len();
        self.sequence += 1;
        self.total_bytes += bytes_len as u64;
        self.type_counts[rec.header.record_type as usize] += 1;
        self.buffer_bytes += bytes_len;
        self.records.push(bytes);

        // Auto-flush if buffer exceeds max size.
        if self.buffer_bytes >= self.config.max_buffer_size {
            self.flush();
        }

        Ok(self.sequence)
    }

    /// Validate a record against the writer configuration.
    fn validate(&self, record: &SmfRecord) -> Result<(), SmfWriterError> {
        let size = record.length();
        if size > self.config.max_record_size {
            return Err(SmfWriterError::RecordTooLarge {
                size,
                max: self.config.max_record_size,
            });
        }

        if !self.config.enabled_types.is_empty() {
            let rtype = record.header.record_type;
            let enabled = self.config.enabled_types.iter().any(|t| t.code() == rtype);
            if !enabled {
                return Err(SmfWriterError::TypeNotEnabled(rtype));
            }
        }

        Ok(())
    }

    /// Write a record to a file in addition to the in-memory buffer.
    pub fn write_to_file(
        &mut self,
        record: &SmfRecord,
        path: &std::path::Path,
    ) -> Result<u64, SmfWriterError> {
        use std::io::Write;
        let seq = self.write(record)?;
        let bytes = record.to_bytes();
        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)?;
        // Write 4-byte length prefix followed by the record.
        let len = bytes.len() as u32;
        file.write_all(&len.to_be_bytes())?;
        file.write_all(&bytes)?;
        Ok(seq)
    }

    /// Flush the current buffer (move records to flushed storage).
    pub fn flush(&mut self) {
        self.flushed_records.append(&mut self.records);
        self.buffer_bytes = 0;
        self.flush_count += 1;
    }

    /// Number of records in the current buffer.
    pub fn record_count(&self) -> usize {
        self.records.len()
    }

    /// Total records including flushed.
    pub fn total_record_count(&self) -> usize {
        self.records.len() + self.flushed_records.len()
    }

    /// Total bytes written.
    pub fn total_bytes(&self) -> u64 {
        self.total_bytes
    }

    /// Current sequence number.
    pub fn sequence(&self) -> u64 {
        self.sequence
    }

    /// Get the count for a specific record type.
    pub fn count_for_type(&self, record_type: u8) -> u64 {
        self.type_counts[record_type as usize]
    }

    /// Current buffer size in bytes.
    pub fn buffer_bytes(&self) -> usize {
        self.buffer_bytes
    }

    /// Number of flushes performed.
    pub fn flush_count(&self) -> u64 {
        self.flush_count
    }

    /// Drain all collected records (returns raw bytes).
    pub fn drain(&mut self) -> Vec<Vec<u8>> {
        std::mem::take(&mut self.records)
    }

    /// Drain all records including flushed.
    pub fn drain_all(&mut self) -> Vec<Vec<u8>> {
        let mut all = std::mem::take(&mut self.flushed_records);
        all.append(&mut self.records);
        self.buffer_bytes = 0;
        all
    }

    /// Get a reference to all collected records.
    pub fn records(&self) -> &[Vec<u8>] {
        &self.records
    }

    /// Clear all collected records and reset counters.
    pub fn reset(&mut self) {
        self.records.clear();
        self.flushed_records.clear();
        self.sequence = 0;
        self.total_bytes = 0;
        self.type_counts = [0; 256];
        self.buffer_bytes = 0;
        self.flush_count = 0;
    }

    /// Get the writer configuration.
    pub fn config(&self) -> &SmfWriterConfig {
        &self.config
    }

    /// Concatenate all records into a single byte buffer.
    /// Each record is prefixed with a 4-byte big-endian length.
    pub fn to_dataset(&self) -> Vec<u8> {
        let mut buf = Vec::new();
        for rec in &self.flushed_records {
            let len = rec.len() as u32;
            buf.extend_from_slice(&len.to_be_bytes());
            buf.extend_from_slice(rec);
        }
        for rec in &self.records {
            let len = rec.len() as u32;
            buf.extend_from_slice(&len.to_be_bytes());
            buf.extend_from_slice(rec);
        }
        buf
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::record::{SmfType4, SmfType5, SmfType30, SmfSubtype30};

    #[test]
    fn test_writer_defaults() {
        let w = SmfWriter::with_defaults();
        assert_eq!(w.record_count(), 0);
        assert_eq!(w.total_bytes(), 0);
        assert_eq!(w.sequence(), 0);
    }

    #[test]
    fn test_write_type4() {
        let mut w = SmfWriter::with_defaults();
        let step = SmfType4 {
            job_name: "MYJOB".to_string(),
            step_name: "STEP1".to_string(),
            program_name: "IEFBR14".to_string(),
            step_number: 1,
            completion_code: 0,
            ..Default::default()
        };
        let seq = w.write(&step.to_record()).unwrap();
        assert_eq!(seq, 1);
        assert_eq!(w.record_count(), 1);
        assert_eq!(w.count_for_type(4), 1);
    }

    #[test]
    fn test_write_type5() {
        let mut w = SmfWriter::with_defaults();
        let job = SmfType5 {
            job_name: "MYJOB".to_string(),
            job_id: "JOB00001".to_string(),
            step_count: 3,
            ..Default::default()
        };
        let seq = w.write(&job.to_record()).unwrap();
        assert_eq!(seq, 1);
        assert_eq!(w.count_for_type(5), 1);
    }

    #[test]
    fn test_write_type30() {
        let mut w = SmfWriter::with_defaults();
        let work = SmfType30 {
            subtype: SmfSubtype30::JobStart,
            job_name: "BATCH01".to_string(),
            job_id: "JOB00100".to_string(),
            service_class: "PRODBTCH".to_string(),
            ..Default::default()
        };
        let seq = w.write(&work.to_record()).unwrap();
        assert_eq!(seq, 1);
        assert_eq!(w.count_for_type(30), 1);
    }

    #[test]
    fn test_sequence_numbering() {
        let mut w = SmfWriter::with_defaults();
        let r1 = SmfRecord::new(4, vec![0; 10]);
        let r2 = SmfRecord::new(5, vec![0; 10]);
        let r3 = SmfRecord::new(30, vec![0; 10]);
        assert_eq!(w.write(&r1).unwrap(), 1);
        assert_eq!(w.write(&r2).unwrap(), 2);
        assert_eq!(w.write(&r3).unwrap(), 3);
        assert_eq!(w.sequence(), 3);
    }

    #[test]
    fn test_record_too_large() {
        let config = SmfWriterConfig {
            max_record_size: 50,
            ..Default::default()
        };
        let mut w = SmfWriter::new(config);
        let rec = SmfRecord::new(4, vec![0; 100]);
        let err = w.write(&rec).unwrap_err();
        assert!(matches!(err, SmfWriterError::RecordTooLarge { .. }));
    }

    #[test]
    fn test_type_not_enabled() {
        let config = SmfWriterConfig {
            enabled_types: vec![SmfRecordType::Type4],
            ..Default::default()
        };
        let mut w = SmfWriter::new(config);
        let rec = SmfRecord::new(5, vec![0; 10]);
        let err = w.write(&rec).unwrap_err();
        assert!(matches!(err, SmfWriterError::TypeNotEnabled(5)));
    }

    #[test]
    fn test_enabled_types_allows_matching() {
        let config = SmfWriterConfig {
            enabled_types: vec![SmfRecordType::Type4, SmfRecordType::Type30],
            ..Default::default()
        };
        let mut w = SmfWriter::new(config);
        let rec = SmfRecord::new(4, vec![0; 10]);
        assert!(w.write(&rec).is_ok());
    }

    #[test]
    fn test_drain() {
        let mut w = SmfWriter::with_defaults();
        w.write(&SmfRecord::new(4, vec![1, 2])).unwrap();
        w.write(&SmfRecord::new(5, vec![3, 4])).unwrap();
        let drained = w.drain();
        assert_eq!(drained.len(), 2);
        assert_eq!(w.record_count(), 0);
    }

    #[test]
    fn test_reset() {
        let mut w = SmfWriter::with_defaults();
        w.write(&SmfRecord::new(4, vec![1])).unwrap();
        w.write(&SmfRecord::new(4, vec![2])).unwrap();
        assert_eq!(w.record_count(), 2);
        w.reset();
        assert_eq!(w.record_count(), 0);
        assert_eq!(w.sequence(), 0);
        assert_eq!(w.total_bytes(), 0);
        assert_eq!(w.count_for_type(4), 0);
    }

    #[test]
    fn test_system_id_stamped() {
        let config = SmfWriterConfig {
            system_id: "TST1".to_string(),
            ..Default::default()
        };
        let mut w = SmfWriter::new(config);
        let rec = SmfRecord::new(4, vec![0; 10]);
        w.write(&rec).unwrap();
        // The serialised record should have TST1 in the header.
        let bytes = &w.records()[0];
        let sid = std::str::from_utf8(&bytes[14..18]).unwrap();
        assert_eq!(sid, "TST1");
    }

    #[test]
    fn test_to_dataset() {
        let mut w = SmfWriter::with_defaults();
        w.write(&SmfRecord::new(4, vec![0xAA])).unwrap();
        w.write(&SmfRecord::new(5, vec![0xBB])).unwrap();
        let ds = w.to_dataset();
        // Each record: 4-byte len prefix + record bytes.
        assert!(!ds.is_empty());
        // First 4 bytes should be the length of the first record.
        let len1 = u32::from_be_bytes([ds[0], ds[1], ds[2], ds[3]]) as usize;
        assert_eq!(len1, 19); // 18 header + 1 data byte
    }

    #[test]
    fn test_triplet_roundtrip() {
        let triplet = SmfTriplet::new(128, 64, 3);
        let bytes = triplet.to_bytes();
        let restored = SmfTriplet::from_bytes(&bytes);
        assert_eq!(restored.offset, 128);
        assert_eq!(restored.entry_length, 64);
        assert_eq!(restored.entry_count, 3);
    }

    #[test]
    fn test_triplet_zero() {
        let triplet = SmfTriplet::new(0, 0, 0);
        let bytes = triplet.to_bytes();
        assert_eq!(bytes, [0; 8]);
    }

    #[test]
    fn test_validation_disabled() {
        let config = SmfWriterConfig {
            max_record_size: 10,
            validate: false,
            ..Default::default()
        };
        let mut w = SmfWriter::new(config);
        // Would fail validation but validation is off.
        let rec = SmfRecord::new(4, vec![0; 100]);
        assert!(w.write(&rec).is_ok());
    }

    #[test]
    fn test_multiple_type_counts() {
        let mut w = SmfWriter::with_defaults();
        w.write(&SmfRecord::new(4, vec![0])).unwrap();
        w.write(&SmfRecord::new(4, vec![0])).unwrap();
        w.write(&SmfRecord::new(5, vec![0])).unwrap();
        w.write(&SmfRecord::new(30, vec![0])).unwrap();
        w.write(&SmfRecord::new(30, vec![0])).unwrap();
        w.write(&SmfRecord::new(30, vec![0])).unwrap();
        assert_eq!(w.count_for_type(4), 2);
        assert_eq!(w.count_for_type(5), 1);
        assert_eq!(w.count_for_type(30), 3);
        assert_eq!(w.count_for_type(80), 0);
    }

    // --- SMF-102 tests ---

    #[test]
    fn test_smfwtm_basic() {
        let mut w = SmfWriter::with_defaults();
        let rec = SmfRecord::new(30, vec![0; 20]);
        let seq = w.smfwtm(&rec, None).unwrap();
        assert_eq!(seq, 1);
    }

    #[test]
    fn test_smfwtm_notype_suppression() {
        let prm = SmfPrmConfig::parse("NOTYPE(0:29)").unwrap();
        let mut w = SmfWriter::with_defaults();
        let rec = SmfRecord::new(4, vec![0; 10]);
        let err = w.smfwtm(&rec, Some(&prm)).unwrap_err();
        assert!(matches!(err, SmfWriterError::TypeSuppressed(4)));
    }

    #[test]
    fn test_smfwtm_type_active() {
        let prm = SmfPrmConfig::parse("TYPE(30,80)").unwrap();
        let mut w = SmfWriter::with_defaults();
        let rec = SmfRecord::new(30, vec![0; 10]);
        assert!(w.smfwtm(&rec, Some(&prm)).is_ok());
    }

    #[test]
    fn test_smfewtm_sets_subsystem() {
        let mut w = SmfWriter::with_defaults();
        let rec = SmfRecord::new(110, vec![0; 20]);
        let seq = w.smfewtm(&rec, "DB2A").unwrap();
        assert_eq!(seq, 1);
    }

    #[test]
    fn test_buffer_auto_flush() {
        let config = SmfWriterConfig {
            max_buffer_size: 100,
            ..Default::default()
        };
        let mut w = SmfWriter::new(config);
        // Write records until auto-flush triggers.
        for _ in 0..10 {
            w.write(&SmfRecord::new(4, vec![0; 20])).unwrap();
        }
        assert!(w.flush_count() > 0);
        assert!(w.total_record_count() >= 10);
    }

    #[test]
    fn test_manual_flush() {
        let mut w = SmfWriter::with_defaults();
        w.write(&SmfRecord::new(4, vec![0; 10])).unwrap();
        w.write(&SmfRecord::new(5, vec![0; 10])).unwrap();
        assert_eq!(w.record_count(), 2);
        w.flush();
        assert_eq!(w.record_count(), 0);
        assert_eq!(w.buffer_bytes(), 0);
        assert_eq!(w.flush_count(), 1);
        assert_eq!(w.total_record_count(), 2);
    }

    #[test]
    fn test_drain_all_includes_flushed() {
        let mut w = SmfWriter::with_defaults();
        w.write(&SmfRecord::new(4, vec![0; 10])).unwrap();
        w.flush();
        w.write(&SmfRecord::new(5, vec![0; 10])).unwrap();
        let all = w.drain_all();
        assert_eq!(all.len(), 2);
    }

    #[test]
    fn test_user_record_types() {
        let mut w = SmfWriter::with_defaults();
        // User types 128-255 should work.
        let rec = SmfRecord::new(200, vec![0; 50]);
        let seq = w.write(&rec).unwrap();
        assert_eq!(seq, 1);
        assert_eq!(w.count_for_type(200), 1);
    }

    #[test]
    fn test_write_with_exits() {
        use crate::exits::{Iefu84Exit, SmfExitRegistry};
        let mut w = SmfWriter::with_defaults();
        let mut exits = SmfExitRegistry::new();
        exits.register(Box::new(Iefu84Exit::new("PROD")), vec![]);

        let rec = SmfRecord::new(30, vec![0; 10]);
        let seq = w.write_with_exits(&rec, &exits).unwrap();
        assert_eq!(seq, 1);
    }

    #[test]
    fn test_write_with_exits_suppressed() {
        use crate::exits::{Iefu83Exit, SmfExitRegistry};
        let mut w = SmfWriter::with_defaults();
        let mut exits = SmfExitRegistry::new();
        exits.register(
            Box::new(Iefu83Exit::new(Box::new(|_| true))),
            vec![],
        );

        let rec = SmfRecord::new(30, vec![0; 10]);
        let err = w.write_with_exits(&rec, &exits).unwrap_err();
        assert!(matches!(err, SmfWriterError::SuppressedByExit));
    }

    #[test]
    fn test_to_dataset_includes_flushed() {
        let mut w = SmfWriter::with_defaults();
        w.write(&SmfRecord::new(4, vec![0xAA])).unwrap();
        w.flush();
        w.write(&SmfRecord::new(5, vec![0xBB])).unwrap();
        let ds = w.to_dataset();
        assert!(!ds.is_empty());
        // Should contain both records.
        // Parse: first record.
        let len1 = u32::from_be_bytes([ds[0], ds[1], ds[2], ds[3]]) as usize;
        assert_eq!(len1, 19);
    }
}
