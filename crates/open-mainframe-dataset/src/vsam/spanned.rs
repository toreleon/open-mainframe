//! Spanned record support for VSAM datasets.
//!
//! Implements records that span multiple Control Intervals (CIs) when
//! the record size exceeds the CI size. Each segment carries a descriptor
//! word (SDW) linking segments together.
//!
//! # Record Formats
//!
//! - **Variable**: Records must fit within a single CI.
//! - **VariableSpanned (VS)**: Records may span multiple CIs.
//! - **VariableBlockedSpanned (VBS)**: Blocked spanned records.
//!
//! # Segment Layout
//!
//! Each segment has a 16-byte header:
//! ```text
//! Bytes 0–7:   Next segment offset (0 if last)
//! Bytes 8–9:   Segment number (0-based)
//! Bytes 10–11: Total segments
//! Bytes 12–15: Data length in this segment
//! Bytes 16+:   Record data
//! ```
//!
//! # Example
//!
//! ```
//! use open_mainframe_dataset::types::RecordFormat;
//! use open_mainframe_dataset::vsam::spanned::SpannedRecordManager;
//!
//! let mgr = SpannedRecordManager::new(4096, RecordFormat::VariableSpanned);
//! let data = vec![0xABu8; 10000]; // Larger than 4096 CI
//! let segments = mgr.split_record(&data).unwrap();
//! assert_eq!(segments.len(), 3); // Needs 3 CIs
//! let reassembled = mgr.assemble_record(&segments).unwrap();
//! assert_eq!(reassembled, data);
//! ```

use crate::error::DatasetError;
use crate::types::RecordFormat;

/// Segment descriptor word (SDW) — 16-byte header for each segment.
pub const SDW_SIZE: usize = 16;

/// Returns true if the given record format allows spanned records.
pub fn allows_spanning(fmt: RecordFormat) -> bool {
    matches!(
        fmt,
        RecordFormat::VariableSpanned | RecordFormat::VariableBlockedSpanned
    )
}

/// A single segment of a spanned record.
#[derive(Debug, Clone)]
pub struct Segment {
    /// Offset to the next segment (0 if this is the last).
    pub next_offset: u64,
    /// Segment number (0-based).
    pub segment_number: u16,
    /// Total number of segments for this record.
    pub total_segments: u16,
    /// Length of data in this segment (excluding SDW).
    pub data_length: u32,
    /// The actual record data for this segment.
    pub data: Vec<u8>,
}

impl Segment {
    /// Serialize the segment to bytes (SDW + data).
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut buf = Vec::with_capacity(SDW_SIZE + self.data.len());
        buf.extend_from_slice(&self.next_offset.to_le_bytes()); // 8 bytes
        buf.extend_from_slice(&self.segment_number.to_le_bytes()); // 2 bytes
        buf.extend_from_slice(&self.total_segments.to_le_bytes()); // 2 bytes
        buf.extend_from_slice(&self.data_length.to_le_bytes()); // 4 bytes
        buf.extend_from_slice(&self.data);
        buf
    }

    /// Parse a segment from bytes.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, DatasetError> {
        if bytes.len() < SDW_SIZE {
            return Err(DatasetError::InvalidParameter(format!(
                "Segment too short: {} bytes (need at least {})",
                bytes.len(),
                SDW_SIZE
            )));
        }

        let next_offset = u64::from_le_bytes(bytes[0..8].try_into().unwrap());
        let segment_number = u16::from_le_bytes(bytes[8..10].try_into().unwrap());
        let total_segments = u16::from_le_bytes(bytes[10..12].try_into().unwrap());
        let data_length = u32::from_le_bytes(bytes[12..16].try_into().unwrap());

        let data_end = SDW_SIZE + data_length as usize;
        if bytes.len() < data_end {
            return Err(DatasetError::InvalidParameter(format!(
                "Segment data truncated: have {} bytes, need {}",
                bytes.len(),
                data_end
            )));
        }

        Ok(Self {
            next_offset,
            segment_number,
            total_segments,
            data_length,
            data: bytes[SDW_SIZE..data_end].to_vec(),
        })
    }

    /// Returns true if this is the last segment.
    pub fn is_last(&self) -> bool {
        self.next_offset == 0
    }

    /// Returns true if this is the first segment.
    pub fn is_first(&self) -> bool {
        self.segment_number == 0
    }
}

/// Manager for spanned record operations.
pub struct SpannedRecordManager {
    /// Control interval size in bytes.
    ci_size: usize,
    /// Record format.
    format: RecordFormat,
}

impl SpannedRecordManager {
    /// Create a new spanned record manager.
    pub fn new(ci_size: usize, format: RecordFormat) -> Self {
        Self { ci_size, format }
    }

    /// Maximum data bytes per segment (CI size minus SDW header).
    pub fn max_data_per_segment(&self) -> usize {
        self.ci_size.saturating_sub(SDW_SIZE)
    }

    /// Calculate how many segments a record of the given size requires.
    pub fn segments_needed(&self, record_len: usize) -> usize {
        let max_per_seg = self.max_data_per_segment();
        if max_per_seg == 0 {
            return 0;
        }
        (record_len + max_per_seg - 1) / max_per_seg
    }

    /// Validate whether a record can be stored with the current format.
    ///
    /// For Variable format, the record must fit in a single CI.
    /// For VariableSpanned/VariableBlockedSpanned, spanned records are allowed.
    pub fn validate_record(&self, record_len: usize) -> Result<(), DatasetError> {
        if record_len == 0 {
            return Err(DatasetError::InvalidParameter(
                "Record length must be > 0".to_string(),
            ));
        }

        let max_per_seg = self.max_data_per_segment();
        if max_per_seg == 0 {
            return Err(DatasetError::InvalidParameter(
                "CI size too small for SDW header".to_string(),
            ));
        }

        if !allows_spanning(self.format) && record_len > max_per_seg {
            return Err(DatasetError::RecordTooLong {
                actual: record_len,
                max: max_per_seg,
            });
        }

        Ok(())
    }

    /// Split a record into segments that each fit within a CI.
    ///
    /// Returns an error if the record format doesn't allow spanning and the
    /// record exceeds the CI size.
    pub fn split_record(&self, data: &[u8]) -> Result<Vec<Segment>, DatasetError> {
        self.validate_record(data.len())?;

        let max_per_seg = self.max_data_per_segment();
        let total = self.segments_needed(data.len());

        let mut segments = Vec::with_capacity(total);
        let mut offset = 0;

        for i in 0..total {
            let end = (offset + max_per_seg).min(data.len());
            let chunk = &data[offset..end];

            segments.push(Segment {
                next_offset: 0, // Filled in by the caller with actual file offsets
                segment_number: i as u16,
                total_segments: total as u16,
                data_length: chunk.len() as u32,
                data: chunk.to_vec(),
            });

            offset = end;
        }

        Ok(segments)
    }

    /// Reassemble a complete record from its segments.
    ///
    /// Segments must be in order (by segment_number).
    pub fn assemble_record(&self, segments: &[Segment]) -> Result<Vec<u8>, DatasetError> {
        if segments.is_empty() {
            return Err(DatasetError::InvalidParameter(
                "No segments to assemble".to_string(),
            ));
        }

        // Verify segment ordering and consistency
        let expected_total = segments[0].total_segments;
        if segments.len() != expected_total as usize {
            return Err(DatasetError::InvalidParameter(format!(
                "Expected {} segments, got {}",
                expected_total,
                segments.len()
            )));
        }

        let mut result = Vec::new();
        for (i, seg) in segments.iter().enumerate() {
            if seg.segment_number != i as u16 {
                return Err(DatasetError::InvalidParameter(format!(
                    "Segment {} out of order (expected {})",
                    seg.segment_number, i
                )));
            }
            result.extend_from_slice(&seg.data);
        }

        Ok(result)
    }

    /// Returns the record format.
    pub fn format(&self) -> RecordFormat {
        self.format
    }

    /// Returns the CI size.
    pub fn ci_size(&self) -> usize {
        self.ci_size
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Story 605.1: Record that fits in one CI produces one segment.
    #[test]
    fn test_single_segment_record() {
        let mgr = SpannedRecordManager::new(4096, RecordFormat::VariableSpanned);
        let data = vec![0xABu8; 100];
        let segments = mgr.split_record(&data).unwrap();

        assert_eq!(segments.len(), 1);
        assert_eq!(segments[0].segment_number, 0);
        assert_eq!(segments[0].total_segments, 1);
        assert_eq!(segments[0].data_length, 100);
        assert_eq!(segments[0].data, data);
    }

    /// Story 605.1: 10KB record spans 3 CIs at 4096 CI size.
    #[test]
    fn test_spanned_record_three_segments() {
        let mgr = SpannedRecordManager::new(4096, RecordFormat::VariableSpanned);
        // max_data_per_segment = 4096 - 16 = 4080
        let data = vec![0xCDu8; 10000];
        let segments = mgr.split_record(&data).unwrap();

        assert_eq!(segments.len(), 3);
        assert_eq!(segments[0].data_length, 4080);
        assert_eq!(segments[1].data_length, 4080);
        assert_eq!(segments[2].data_length, 1840);

        for seg in &segments {
            assert_eq!(seg.total_segments, 3);
        }
    }

    /// Story 605.1: Reassemble spanned record produces original data.
    #[test]
    fn test_assemble_spanned_record() {
        let mgr = SpannedRecordManager::new(4096, RecordFormat::VariableSpanned);
        let data: Vec<u8> = (0..10000).map(|i| (i % 256) as u8).collect();
        let segments = mgr.split_record(&data).unwrap();
        let reassembled = mgr.assemble_record(&segments).unwrap();
        assert_eq!(reassembled, data);
    }

    /// Story 605.2: Variable format rejects records larger than CI.
    #[test]
    fn test_v_format_rejects_large_record() {
        let mgr = SpannedRecordManager::new(4096, RecordFormat::Variable);
        let data = vec![0u8; 10000];
        let result = mgr.split_record(&data);
        assert!(result.is_err());
    }

    /// Story 605.2: Variable format accepts records that fit.
    #[test]
    fn test_v_format_accepts_small_record() {
        let mgr = SpannedRecordManager::new(4096, RecordFormat::Variable);
        let data = vec![0u8; 1000];
        let result = mgr.split_record(&data);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 1);
    }

    /// Story 605.2: VariableSpanned format accepts large records.
    #[test]
    fn test_vs_format_accepts_large_record() {
        let mgr = SpannedRecordManager::new(4096, RecordFormat::VariableSpanned);
        let data = vec![0u8; 10000];
        assert!(mgr.split_record(&data).is_ok());
    }

    /// Story 605.2: VariableBlockedSpanned format also allows spanning.
    #[test]
    fn test_vbs_format_allows_spanning() {
        let mgr = SpannedRecordManager::new(4096, RecordFormat::VariableBlockedSpanned);
        let data = vec![0u8; 20000];
        let segments = mgr.split_record(&data).unwrap();
        assert_eq!(segments.len(), 5); // 20000 / 4080 = 4.9 → 5
    }

    /// Story 605.1: Segment serialization roundtrip.
    #[test]
    fn test_segment_serialization() {
        let seg = Segment {
            next_offset: 8192,
            segment_number: 1,
            total_segments: 3,
            data_length: 100,
            data: vec![0xFFu8; 100],
        };

        let bytes = seg.to_bytes();
        assert_eq!(bytes.len(), SDW_SIZE + 100);

        let parsed = Segment::from_bytes(&bytes).unwrap();
        assert_eq!(parsed.next_offset, 8192);
        assert_eq!(parsed.segment_number, 1);
        assert_eq!(parsed.total_segments, 3);
        assert_eq!(parsed.data_length, 100);
        assert_eq!(parsed.data, vec![0xFFu8; 100]);
    }

    /// Story 605.1: segments_needed calculation.
    #[test]
    fn test_segments_needed() {
        let mgr = SpannedRecordManager::new(4096, RecordFormat::VariableSpanned);
        assert_eq!(mgr.segments_needed(100), 1);
        assert_eq!(mgr.segments_needed(4080), 1);
        assert_eq!(mgr.segments_needed(4081), 2);
        assert_eq!(mgr.segments_needed(10000), 3);
        assert_eq!(mgr.segments_needed(0), 0);
    }

    /// Story 605.1: Exact CI boundary record.
    #[test]
    fn test_exact_ci_boundary() {
        let mgr = SpannedRecordManager::new(4096, RecordFormat::VariableSpanned);
        let data = vec![0xABu8; 4080];
        let segments = mgr.split_record(&data).unwrap();
        assert_eq!(segments.len(), 1);
        assert_eq!(segments[0].data_length, 4080);
    }

    /// Story 605.2: allows_spanning check.
    #[test]
    fn test_allows_spanning() {
        assert!(!allows_spanning(RecordFormat::Fixed));
        assert!(!allows_spanning(RecordFormat::Variable));
        assert!(allows_spanning(RecordFormat::VariableSpanned));
        assert!(allows_spanning(RecordFormat::VariableBlockedSpanned));
    }

    /// Story 605.1: Empty segments assembly fails.
    #[test]
    fn test_assemble_empty_segments() {
        let mgr = SpannedRecordManager::new(4096, RecordFormat::VariableSpanned);
        let result = mgr.assemble_record(&[]);
        assert!(result.is_err());
    }

    /// Story 605.1: Out-of-order segments are detected.
    #[test]
    fn test_assemble_out_of_order() {
        let mgr = SpannedRecordManager::new(4096, RecordFormat::VariableSpanned);
        let segments = vec![
            Segment {
                next_offset: 0,
                segment_number: 1, // Should be 0
                total_segments: 2,
                data_length: 10,
                data: vec![0u8; 10],
            },
            Segment {
                next_offset: 0,
                segment_number: 0,
                total_segments: 2,
                data_length: 10,
                data: vec![0u8; 10],
            },
        ];
        let result = mgr.assemble_record(&segments);
        assert!(result.is_err());
    }

    /// Story 605.2: Zero-length record is rejected.
    #[test]
    fn test_zero_length_record_rejected() {
        let mgr = SpannedRecordManager::new(4096, RecordFormat::VariableSpanned);
        let result = mgr.validate_record(0);
        assert!(result.is_err());
    }
}
