//! SMF Dump Utilities (IFASMFDP equivalent).
//!
//! Extracts and filters SMF records from datasets for analysis and reporting:
//! - Type filtering (include/exclude record types)
//! - Date/time range filtering
//! - Job name and system ID filtering
//! - Output formatting (binary or human-readable report)

use crate::record::{read_padded, SmfHeader, SmfRecord, SmfRecordError};

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors from dump operations.
#[derive(Debug, thiserror::Error)]
pub enum SmfDumpError {
    /// Record parse error.
    #[error("record error: {0}")]
    RecordError(#[from] SmfRecordError),

    /// I/O error.
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    /// No records match the filter criteria.
    #[error("no records match the filter criteria")]
    NoMatchingRecords,
}

// ---------------------------------------------------------------------------
//  Output format
// ---------------------------------------------------------------------------

/// Output format for dump results.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum DumpOutputFormat {
    /// Binary output (raw SMF records).
    #[default]
    Binary,
    /// Human-readable report.
    Report,
}

// ---------------------------------------------------------------------------
//  Dump filter
// ---------------------------------------------------------------------------

/// Filter criteria for the dump utility.
#[derive(Debug, Clone, Default)]
pub struct DumpFilter {
    /// Include only these record types (empty = all).
    pub include_types: Vec<u8>,
    /// Exclude these record types.
    pub exclude_types: Vec<u8>,
    /// Start time filter (hundredths of seconds since midnight). None = no filter.
    pub start_time: Option<u32>,
    /// End time filter. None = no filter.
    pub end_time: Option<u32>,
    /// Date filter (packed date). None = no filter.
    pub date: Option<u32>,
    /// Job name pattern (supports trailing '*' wildcard). None = no filter.
    pub job_name_pattern: Option<String>,
    /// System ID filter. None = no filter.
    pub system_id: Option<String>,
}

impl DumpFilter {
    /// Check if a record's header matches the filter.
    pub fn matches_header(&self, header: &SmfHeader) -> bool {
        // Type filtering.
        if !self.include_types.is_empty()
            && !self.include_types.contains(&header.record_type)
        {
            return false;
        }
        if self.exclude_types.contains(&header.record_type) {
            return false;
        }

        // Time filtering.
        if let Some(start) = self.start_time {
            if header.time < start {
                return false;
            }
        }
        if let Some(end) = self.end_time {
            if header.time > end {
                return false;
            }
        }

        // Date filtering.
        if let Some(date) = self.date {
            if header.date != date {
                return false;
            }
        }

        // System ID filtering.
        if let Some(ref sid) = self.system_id {
            if header.system_id != *sid {
                return false;
            }
        }

        true
    }

    /// Check if a record matches including data-level checks (job name).
    pub fn matches(&self, record: &SmfRecord) -> bool {
        if !self.matches_header(&record.header) {
            return false;
        }

        // Job name pattern matching (check in record data).
        if let Some(ref pattern) = self.job_name_pattern {
            let job_name = extract_job_name(record);
            if !matches_pattern(&job_name, pattern) {
                return false;
            }
        }

        true
    }
}

/// Extract job name from common record types.
fn extract_job_name(record: &SmfRecord) -> String {
    // For Type 4, 5, 30: job name is in the first 8 bytes of data.
    // For Type 30: job name is at offset 2 (after 2-byte subtype).
    let offset = match record.header.record_type {
        30 => 2,
        4 | 5 => 0,
        _ => return String::new(),
    };

    read_padded(&record.data, offset, 8)
}

/// Simple pattern matching with trailing '*' wildcard.
fn matches_pattern(value: &str, pattern: &str) -> bool {
    if let Some(prefix) = pattern.strip_suffix('*') {
        value.starts_with(prefix)
    } else {
        value == pattern
    }
}

// ---------------------------------------------------------------------------
//  IFASMFDP — Dump program
// ---------------------------------------------------------------------------

/// IFASMFDP — SMF dump program.
///
/// Reads a buffer of SMF records, applies filters, and produces output.
#[derive(Debug)]
pub struct SmfDumpProgram {
    filter: DumpFilter,
    output_format: DumpOutputFormat,
}

impl SmfDumpProgram {
    /// Create a new dump program with filter and format.
    pub fn new(filter: DumpFilter, output_format: DumpOutputFormat) -> Self {
        Self {
            filter,
            output_format,
        }
    }

    /// Dump records from raw dataset bytes (4-byte length prefix per record).
    pub fn dump_from_bytes(&self, dataset: &[u8]) -> Result<Vec<SmfRecord>, SmfDumpError> {
        let records = parse_dataset(dataset)?;
        let filtered: Vec<SmfRecord> = records
            .into_iter()
            .filter(|r| self.filter.matches(r))
            .collect();
        Ok(filtered)
    }

    /// Dump records from a pre-parsed list.
    pub fn dump_records(&self, records: &[SmfRecord]) -> Vec<SmfRecord> {
        records
            .iter()
            .filter(|r| self.filter.matches(r))
            .cloned()
            .collect()
    }

    /// Generate a human-readable report from records.
    pub fn format_report(&self, records: &[SmfRecord]) -> String {
        let mut report = String::new();
        report.push_str("SMF DUMP REPORT\n");
        report.push_str(&format!("Total records: {}\n", records.len()));
        report.push_str("--------------------------------------------\n");

        for (i, rec) in records.iter().enumerate() {
            report.push_str(&format!(
                "Record {}: Type={}, Length={}, SID={}, Time={}, Subtype={}\n",
                i + 1,
                rec.header.record_type,
                rec.header.rdw_length,
                rec.header.system_id,
                rec.header.time,
                rec.header.subtype,
            ));
        }

        report.push_str("--------------------------------------------\n");
        report.push_str("END OF REPORT\n");
        report
    }

    /// Get the output format.
    pub fn output_format(&self) -> DumpOutputFormat {
        self.output_format
    }

    /// Produce binary output (concatenated records with length prefixes).
    pub fn to_binary_output(&self, records: &[SmfRecord]) -> Vec<u8> {
        let mut output = Vec::new();
        for rec in records {
            let bytes = rec.to_bytes();
            let len = bytes.len() as u32;
            output.extend_from_slice(&len.to_be_bytes());
            output.extend_from_slice(&bytes);
        }
        output
    }
}

/// Parse a dataset (4-byte length prefix per record) into SmfRecords.
pub fn parse_dataset(data: &[u8]) -> Result<Vec<SmfRecord>, SmfDumpError> {
    let mut records = Vec::new();
    let mut offset = 0;

    while offset + 4 <= data.len() {
        let len = u32::from_be_bytes([
            data[offset],
            data[offset + 1],
            data[offset + 2],
            data[offset + 3],
        ]) as usize;
        offset += 4;

        if offset + len > data.len() {
            break;
        }

        let record = SmfRecord::from_bytes(&data[offset..offset + len])?;
        records.push(record);
        offset += len;
    }

    Ok(records)
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn make_test_records() -> Vec<SmfRecord> {
        let mut records = Vec::new();

        let mut r30 = SmfRecord::new(30, vec![0; 80]);
        r30.header.system_id = "SYS1".to_string();
        r30.header.set_time(10, 0, 0, 0);
        records.push(r30);

        let mut r80 = SmfRecord::new(80, vec![0; 40]);
        r80.header.system_id = "SYS1".to_string();
        r80.header.set_time(14, 0, 0, 0);
        records.push(r80);

        let mut r4 = SmfRecord::new(4, vec![0; 50]);
        r4.header.system_id = "SYS2".to_string();
        r4.header.set_time(16, 0, 0, 0);
        records.push(r4);

        records
    }

    #[test]
    fn test_filter_by_type() {
        let filter = DumpFilter {
            include_types: vec![30, 80],
            ..Default::default()
        };
        let records = make_test_records();
        let dump = SmfDumpProgram::new(filter, DumpOutputFormat::Binary);
        let result = dump.dump_records(&records);
        assert_eq!(result.len(), 2);
    }

    #[test]
    fn test_filter_exclude_type() {
        let filter = DumpFilter {
            exclude_types: vec![4],
            ..Default::default()
        };
        let records = make_test_records();
        let dump = SmfDumpProgram::new(filter, DumpOutputFormat::Binary);
        let result = dump.dump_records(&records);
        assert_eq!(result.len(), 2);
    }

    #[test]
    fn test_filter_by_time_range() {
        let filter = DumpFilter {
            start_time: Some(8 * 360000),  // 08:00
            end_time: Some(15 * 360000),   // 15:00
            ..Default::default()
        };
        let records = make_test_records();
        let dump = SmfDumpProgram::new(filter, DumpOutputFormat::Binary);
        let result = dump.dump_records(&records);
        assert_eq!(result.len(), 2); // 10:00 and 14:00 match; 16:00 excluded.
    }

    #[test]
    fn test_filter_by_system_id() {
        let filter = DumpFilter {
            system_id: Some("SYS1".to_string()),
            ..Default::default()
        };
        let records = make_test_records();
        let dump = SmfDumpProgram::new(filter, DumpOutputFormat::Binary);
        let result = dump.dump_records(&records);
        assert_eq!(result.len(), 2); // SYS1 only
    }

    #[test]
    fn test_filter_by_jobname_wildcard() {
        use crate::record::extend_padded;

        let mut data = Vec::new();
        // Type 30 has subtype (2 bytes) then job name (8 bytes).
        data.push(0); data.push(1); // subtype
        extend_padded(&mut data, "PAYROLL", 8);
        data.extend_from_slice(&[0; 70]); // padding

        let rec = SmfRecord::new(30, data);
        let filter = DumpFilter {
            job_name_pattern: Some("PAY*".to_string()),
            ..Default::default()
        };
        assert!(filter.matches(&rec));

        let filter_no_match = DumpFilter {
            job_name_pattern: Some("ABC*".to_string()),
            ..Default::default()
        };
        assert!(!filter_no_match.matches(&rec));
    }

    #[test]
    fn test_format_report() {
        let records = make_test_records();
        let dump = SmfDumpProgram::new(DumpFilter::default(), DumpOutputFormat::Report);
        let report = dump.format_report(&records);
        assert!(report.contains("SMF DUMP REPORT"));
        assert!(report.contains("Total records: 3"));
        assert!(report.contains("Type=30"));
        assert!(report.contains("Type=80"));
        assert!(report.contains("END OF REPORT"));
    }

    #[test]
    fn test_binary_output_roundtrip() {
        let records = make_test_records();
        let dump = SmfDumpProgram::new(DumpFilter::default(), DumpOutputFormat::Binary);
        let binary = dump.to_binary_output(&records);
        let parsed = parse_dataset(&binary).unwrap();
        assert_eq!(parsed.len(), 3);
        assert_eq!(parsed[0].header.record_type, 30);
        assert_eq!(parsed[1].header.record_type, 80);
        assert_eq!(parsed[2].header.record_type, 4);
    }

    #[test]
    fn test_parse_empty_dataset() {
        let parsed = parse_dataset(&[]).unwrap();
        assert!(parsed.is_empty());
    }

    #[test]
    fn test_no_filter_passes_all() {
        let filter = DumpFilter::default();
        let records = make_test_records();
        let dump = SmfDumpProgram::new(filter, DumpOutputFormat::Binary);
        let result = dump.dump_records(&records);
        assert_eq!(result.len(), 3);
    }

    #[test]
    fn test_matches_pattern_exact() {
        assert!(matches_pattern("MYJOB", "MYJOB"));
        assert!(!matches_pattern("MYJOB", "OTHER"));
    }

    #[test]
    fn test_matches_pattern_wildcard() {
        assert!(matches_pattern("PAYROLL", "PAY*"));
        assert!(matches_pattern("PAY", "PAY*"));
        assert!(!matches_pattern("BILLING", "PAY*"));
    }

    #[test]
    fn test_dump_from_bytes() {
        let rec = SmfRecord::new(30, vec![0; 20]);
        let bytes = rec.to_bytes();
        let mut dataset = Vec::new();
        let len = bytes.len() as u32;
        dataset.extend_from_slice(&len.to_be_bytes());
        dataset.extend_from_slice(&bytes);

        let dump = SmfDumpProgram::new(DumpFilter::default(), DumpOutputFormat::Binary);
        let result = dump.dump_from_bytes(&dataset).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].header.record_type, 30);
    }
}
