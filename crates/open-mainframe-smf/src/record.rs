//! SMF Record Format and Types.
//!
//! SMF records have a standard header followed by type-specific data.
//! Common record types:
//! - Type 4: Step termination
//! - Type 5: Job termination
//! - Type 30: Common address space work (subtype 1-5)

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors from SMF record operations.
#[derive(Debug, thiserror::Error)]
pub enum SmfRecordError {
    /// Not enough bytes for header deserialization.
    #[error("insufficient data: expected at least {expected} bytes, got {actual}")]
    InsufficientData { expected: usize, actual: usize },

    /// Record type not found in the registry.
    #[error("unregistered record type {0}")]
    UnregisteredType(u8),
}

// ---------------------------------------------------------------------------
//  SMF Header
// ---------------------------------------------------------------------------

/// Standard SMF record header.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SmfHeader {
    /// Record length (including header).
    pub rdw_length: u16,
    /// Segment descriptor (0 for single-segment records).
    pub rdw_segment: u16,
    /// System indicator flags.
    pub flag: u8,
    /// Record type (0-255).
    pub record_type: u8,
    /// Time of day in hundredths of a second since midnight.
    pub time: u32,
    /// Date (packed: 0CYYDDDF, C=century, YY=year, DDD=day of year, F=sign).
    pub date: u32,
    /// System ID (SID) — 4-character system name.
    pub system_id: String,
    /// Subsystem ID.
    pub subsystem_id: String,
    /// Subtype (for records with subtypes).
    pub subtype: u16,
}

impl SmfHeader {
    /// Standard header serialized size in bytes.
    pub const HEADER_SIZE: usize = 18;

    /// Create a new header for a given record type.
    pub fn new(record_type: u8) -> Self {
        Self {
            rdw_length: 0,
            rdw_segment: 0,
            flag: 0,
            record_type,
            time: 0,
            date: 0,
            system_id: "SYS1".to_string(),
            subsystem_id: String::new(),
            subtype: 0,
        }
    }

    /// Deserialize a header from bytes.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, SmfRecordError> {
        if bytes.len() < Self::HEADER_SIZE {
            return Err(SmfRecordError::InsufficientData {
                expected: Self::HEADER_SIZE,
                actual: bytes.len(),
            });
        }
        let rdw_length = u16::from_be_bytes([bytes[0], bytes[1]]);
        let rdw_segment = u16::from_be_bytes([bytes[2], bytes[3]]);
        let flag = bytes[4];
        let record_type = bytes[5];
        let time = u32::from_be_bytes([bytes[6], bytes[7], bytes[8], bytes[9]]);
        let date = u32::from_be_bytes([bytes[10], bytes[11], bytes[12], bytes[13]]);
        let system_id = String::from_utf8_lossy(&bytes[14..18]).trim_end().to_string();
        Ok(Self {
            rdw_length,
            rdw_segment,
            flag,
            record_type,
            time,
            date,
            system_id,
            subsystem_id: String::new(),
            subtype: 0,
        })
    }

    /// Set date fields from components.
    pub fn set_date(&mut self, year: u16, day_of_year: u16) {
        let century = if year >= 2000 { 1 } else { 0 };
        let yy = (year % 100) as u32;
        let ddd = day_of_year as u32;
        // Packed format: 0CYYDDDF
        self.date = (century << 24) | (yy << 16) | (ddd << 4) | 0x0F;
    }

    /// Set time from hours, minutes, seconds.
    pub fn set_time(&mut self, hours: u8, minutes: u8, seconds: u8, hundredths: u8) {
        self.time = (hours as u32) * 360000
            + (minutes as u32) * 6000
            + (seconds as u32) * 100
            + (hundredths as u32);
    }

    /// Serialize the header to bytes (18 bytes for standard header).
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(28);

        // RDW: 4 bytes.
        bytes.push((self.rdw_length >> 8) as u8);
        bytes.push(self.rdw_length as u8);
        bytes.push((self.rdw_segment >> 8) as u8);
        bytes.push(self.rdw_segment as u8);

        // Flag and record type.
        bytes.push(self.flag);
        bytes.push(self.record_type);

        // Time: 4 bytes.
        bytes.push((self.time >> 24) as u8);
        bytes.push((self.time >> 16) as u8);
        bytes.push((self.time >> 8) as u8);
        bytes.push(self.time as u8);

        // Date: 4 bytes.
        bytes.push((self.date >> 24) as u8);
        bytes.push((self.date >> 16) as u8);
        bytes.push((self.date >> 8) as u8);
        bytes.push(self.date as u8);

        // System ID: 4 bytes (padded).
        let sid_bytes: Vec<u8> = self.system_id.bytes().chain(std::iter::repeat(b' ')).take(4).collect();
        bytes.extend_from_slice(&sid_bytes);

        bytes
    }
}

// ---------------------------------------------------------------------------
//  Record types
// ---------------------------------------------------------------------------

/// Well-known SMF record types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum SmfRecordType {
    /// Type 4: Step termination.
    Type4,
    /// Type 5: Job termination.
    Type5,
    /// Type 14: Input dataset activity.
    Type14,
    /// Type 15: Output dataset activity.
    Type15,
    /// Type 17: Dataset scratch (delete).
    Type17,
    /// Type 18: Dataset rename.
    Type18,
    /// Type 30: Common address space work.
    Type30,
    /// Type 42: SMS statistics.
    Type42,
    /// Type 70: RMF CPU activity.
    Type70,
    /// Type 71: Paging activity.
    Type71,
    /// Type 72: Workload activity.
    Type72,
    /// Type 74: Device activity.
    Type74,
    /// Type 80: RACF processing.
    Type80,
    /// Type 89: Usage data.
    Type89,
    /// Type 92: File system activity.
    Type92,
    /// Type 100: DB2 accounting.
    Type100,
    /// Type 101: DB2 performance.
    Type101,
    /// Type 110: CICS transaction.
    Type110,
    /// Type 115: MQ accounting.
    Type115,
    /// Type 116: DB2/MQ statistics.
    Type116,
    /// Type 119: TCP/IP connections.
    Type119,
    /// User-defined type.
    User(u8),
}

impl SmfRecordType {
    /// Get the numeric type code.
    pub fn code(&self) -> u8 {
        match self {
            SmfRecordType::Type4 => 4,
            SmfRecordType::Type5 => 5,
            SmfRecordType::Type14 => 14,
            SmfRecordType::Type15 => 15,
            SmfRecordType::Type17 => 17,
            SmfRecordType::Type18 => 18,
            SmfRecordType::Type30 => 30,
            SmfRecordType::Type42 => 42,
            SmfRecordType::Type70 => 70,
            SmfRecordType::Type71 => 71,
            SmfRecordType::Type72 => 72,
            SmfRecordType::Type74 => 74,
            SmfRecordType::Type80 => 80,
            SmfRecordType::Type89 => 89,
            SmfRecordType::Type92 => 92,
            SmfRecordType::Type100 => 100,
            SmfRecordType::Type101 => 101,
            SmfRecordType::Type110 => 110,
            SmfRecordType::Type115 => 115,
            SmfRecordType::Type116 => 116,
            SmfRecordType::Type119 => 119,
            SmfRecordType::User(n) => *n,
        }
    }

    /// Construct from a numeric code.
    pub fn from_code(code: u8) -> Self {
        match code {
            4 => SmfRecordType::Type4,
            5 => SmfRecordType::Type5,
            14 => SmfRecordType::Type14,
            15 => SmfRecordType::Type15,
            17 => SmfRecordType::Type17,
            18 => SmfRecordType::Type18,
            30 => SmfRecordType::Type30,
            42 => SmfRecordType::Type42,
            70 => SmfRecordType::Type70,
            71 => SmfRecordType::Type71,
            72 => SmfRecordType::Type72,
            74 => SmfRecordType::Type74,
            80 => SmfRecordType::Type80,
            89 => SmfRecordType::Type89,
            92 => SmfRecordType::Type92,
            100 => SmfRecordType::Type100,
            101 => SmfRecordType::Type101,
            110 => SmfRecordType::Type110,
            115 => SmfRecordType::Type115,
            116 => SmfRecordType::Type116,
            119 => SmfRecordType::Type119,
            n => SmfRecordType::User(n),
        }
    }
}

// ---------------------------------------------------------------------------
//  SMF Record (generic)
// ---------------------------------------------------------------------------

/// A generic SMF record.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SmfRecord {
    /// Record header.
    pub header: SmfHeader,
    /// Record-specific data.
    pub data: Vec<u8>,
}

impl SmfRecord {
    /// Create a new record with the given type and data.
    pub fn new(record_type: u8, data: Vec<u8>) -> Self {
        let mut header = SmfHeader::new(record_type);
        header.rdw_length = (SmfHeader::HEADER_SIZE + data.len()) as u16;
        Self { header, data }
    }

    /// Deserialize a record from bytes.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, SmfRecordError> {
        let header = SmfHeader::from_bytes(bytes)?;
        let data = bytes[SmfHeader::HEADER_SIZE..].to_vec();
        Ok(Self { header, data })
    }

    /// Serialize the entire record to bytes.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = self.header.to_bytes();
        bytes.extend_from_slice(&self.data);
        bytes
    }

    /// Total record length.
    pub fn length(&self) -> usize {
        self.header.to_bytes().len() + self.data.len()
    }
}

// ---------------------------------------------------------------------------
//  Record Type Registry
// ---------------------------------------------------------------------------

/// A parser function that takes raw record data and returns a description string.
pub type RecordParserFn = Box<dyn Fn(&SmfRecord) -> String + Send + Sync>;

/// Registry mapping record type codes to their parsers.
pub struct SmfRecordRegistry {
    parsers: HashMap<u8, RecordParserFn>,
}

impl std::fmt::Debug for SmfRecordRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SmfRecordRegistry")
            .field("registered_types", &self.parsers.keys().collect::<Vec<_>>())
            .finish()
    }
}

impl Default for SmfRecordRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl SmfRecordRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self {
            parsers: HashMap::new(),
        }
    }

    /// Register a parser for a record type.
    pub fn register_type(&mut self, type_code: u8, parser: RecordParserFn) {
        self.parsers.insert(type_code, parser);
    }

    /// Parse a record using the registered parser.
    pub fn parse(&self, record: &SmfRecord) -> Result<String, SmfRecordError> {
        let type_code = record.header.record_type;
        let parser = self
            .parsers
            .get(&type_code)
            .ok_or(SmfRecordError::UnregisteredType(type_code))?;
        Ok(parser(record))
    }

    /// Check if a type is registered.
    pub fn is_registered(&self, type_code: u8) -> bool {
        self.parsers.contains_key(&type_code)
    }

    /// Get all registered type codes.
    pub fn registered_types(&self) -> Vec<u8> {
        let mut types: Vec<u8> = self.parsers.keys().copied().collect();
        types.sort();
        types
    }
}

// ---------------------------------------------------------------------------
//  Type 4: Step Termination
// ---------------------------------------------------------------------------

/// SMF Type 4 record — step termination.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct SmfType4 {
    /// Job name.
    pub job_name: String,
    /// Step name.
    pub step_name: String,
    /// Program name.
    pub program_name: String,
    /// Step number.
    pub step_number: u16,
    /// Completion code.
    pub completion_code: u16,
    /// Abend code (0 if no abend).
    pub abend_code: u16,
    /// CPU time used (in hundredths of a second).
    pub cpu_time: u32,
    /// Elapsed time (in hundredths of a second).
    pub elapsed_time: u32,
    /// Region size requested (KB).
    pub region_size: u32,
    /// EXCP count (I/O operations).
    pub excp_count: u32,
}


impl SmfType4 {
    /// Convert to a generic SMF record.
    pub fn to_record(&self) -> SmfRecord {
        let mut data = Vec::new();
        // Job name (8 bytes).
        extend_padded(&mut data, &self.job_name, 8);
        // Step name (8 bytes).
        extend_padded(&mut data, &self.step_name, 8);
        // Program name (8 bytes).
        extend_padded(&mut data, &self.program_name, 8);
        // Step number (2 bytes).
        data.push((self.step_number >> 8) as u8);
        data.push(self.step_number as u8);
        // Completion code (2 bytes).
        data.push((self.completion_code >> 8) as u8);
        data.push(self.completion_code as u8);
        // Abend code (2 bytes).
        data.push((self.abend_code >> 8) as u8);
        data.push(self.abend_code as u8);
        // CPU time (4 bytes).
        push_u32(&mut data, self.cpu_time);
        // Elapsed time (4 bytes).
        push_u32(&mut data, self.elapsed_time);
        // Region size (4 bytes).
        push_u32(&mut data, self.region_size);
        // EXCP count (4 bytes).
        push_u32(&mut data, self.excp_count);
        SmfRecord::new(4, data)
    }
}

// ---------------------------------------------------------------------------
//  Type 5: Job Termination
// ---------------------------------------------------------------------------

/// SMF Type 5 record — job termination.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct SmfType5 {
    /// Job name.
    pub job_name: String,
    /// Job ID (e.g., "JOB12345").
    pub job_id: String,
    /// User ID.
    pub user_id: String,
    /// Job class.
    pub job_class: String,
    /// Number of steps.
    pub step_count: u16,
    /// Maximum completion code across all steps.
    pub max_completion_code: u16,
    /// Total CPU time (hundredths of a second).
    pub total_cpu_time: u32,
    /// Total elapsed time (hundredths of a second).
    pub total_elapsed_time: u32,
    /// Reader start time.
    pub reader_start_time: u32,
    /// Job end time.
    pub job_end_time: u32,
}


impl SmfType5 {
    /// Convert to a generic SMF record.
    pub fn to_record(&self) -> SmfRecord {
        let mut data = Vec::new();
        extend_padded(&mut data, &self.job_name, 8);
        extend_padded(&mut data, &self.job_id, 8);
        extend_padded(&mut data, &self.user_id, 8);
        extend_padded(&mut data, &self.job_class, 1);
        // Padding to align.
        data.push(0);
        // Step count (2 bytes).
        data.push((self.step_count >> 8) as u8);
        data.push(self.step_count as u8);
        // Max completion code (2 bytes).
        data.push((self.max_completion_code >> 8) as u8);
        data.push(self.max_completion_code as u8);
        // CPU time (4 bytes).
        push_u32(&mut data, self.total_cpu_time);
        // Elapsed time (4 bytes).
        push_u32(&mut data, self.total_elapsed_time);
        // Reader start time (4 bytes).
        push_u32(&mut data, self.reader_start_time);
        // Job end time (4 bytes).
        push_u32(&mut data, self.job_end_time);
        SmfRecord::new(5, data)
    }
}

// ---------------------------------------------------------------------------
//  Type 30: Common Address Space Work
// ---------------------------------------------------------------------------

/// SMF Type 30 subtypes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum SmfSubtype30 {
    /// Subtype 1: Job start.
    JobStart,
    /// Subtype 2: Interval (periodic).
    Interval,
    /// Subtype 3: Step end / last step.
    StepEnd,
    /// Subtype 4: Step termination.
    StepTermination,
    /// Subtype 5: Job end.
    JobEnd,
}

impl SmfSubtype30 {
    /// Get the numeric subtype code.
    pub fn code(&self) -> u16 {
        match self {
            SmfSubtype30::JobStart => 1,
            SmfSubtype30::Interval => 2,
            SmfSubtype30::StepEnd => 3,
            SmfSubtype30::StepTermination => 4,
            SmfSubtype30::JobEnd => 5,
        }
    }
}

/// SMF Type 30 record — common address space work.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SmfType30 {
    /// Subtype.
    pub subtype: SmfSubtype30,
    /// Job name.
    pub job_name: String,
    /// Job ID.
    pub job_id: String,
    /// Step name.
    pub step_name: String,
    /// Program name.
    pub program_name: String,
    /// User ID.
    pub user_id: String,
    /// Accounting info.
    pub accounting: String,
    /// Service class.
    pub service_class: String,
    /// CPU time (microseconds).
    pub cpu_time_us: u64,
    /// SRB time (microseconds).
    pub srb_time_us: u64,
    /// I/O count.
    pub io_count: u64,
    /// Region size above (pages).
    pub region_above: u32,
    /// Region size below (pages).
    pub region_below: u32,
    /// Completion code.
    pub completion_code: u16,
    /// Performance group.
    pub performance_group: u16,
}

impl Default for SmfType30 {
    fn default() -> Self {
        Self {
            subtype: SmfSubtype30::JobEnd,
            job_name: String::new(),
            job_id: String::new(),
            step_name: String::new(),
            program_name: String::new(),
            user_id: String::new(),
            accounting: String::new(),
            service_class: String::new(),
            cpu_time_us: 0,
            srb_time_us: 0,
            io_count: 0,
            region_above: 0,
            region_below: 0,
            completion_code: 0,
            performance_group: 0,
        }
    }
}

impl SmfType30 {
    /// Convert to a generic SMF record.
    pub fn to_record(&self) -> SmfRecord {
        let mut data = Vec::new();

        // Subtype (2 bytes).
        let st = self.subtype.code();
        data.push((st >> 8) as u8);
        data.push(st as u8);

        extend_padded(&mut data, &self.job_name, 8);
        extend_padded(&mut data, &self.job_id, 8);
        extend_padded(&mut data, &self.step_name, 8);
        extend_padded(&mut data, &self.program_name, 8);
        extend_padded(&mut data, &self.user_id, 8);
        extend_padded(&mut data, &self.accounting, 16);
        extend_padded(&mut data, &self.service_class, 8);

        // CPU time (8 bytes).
        push_u64(&mut data, self.cpu_time_us);
        // SRB time (8 bytes).
        push_u64(&mut data, self.srb_time_us);
        // I/O count (8 bytes).
        push_u64(&mut data, self.io_count);
        // Region above (4 bytes).
        push_u32(&mut data, self.region_above);
        // Region below (4 bytes).
        push_u32(&mut data, self.region_below);
        // Completion code (2 bytes).
        data.push((self.completion_code >> 8) as u8);
        data.push(self.completion_code as u8);
        // Performance group (2 bytes).
        data.push((self.performance_group >> 8) as u8);
        data.push(self.performance_group as u8);

        let mut record = SmfRecord::new(30, data);
        record.header.subtype = self.subtype.code();
        record
    }
}

// ---------------------------------------------------------------------------
//  Helpers (public for use by other modules)
// ---------------------------------------------------------------------------

pub(crate) fn extend_padded(buf: &mut Vec<u8>, s: &str, len: usize) {
    let bytes: Vec<u8> = s.bytes().chain(std::iter::repeat(b' ')).take(len).collect();
    buf.extend_from_slice(&bytes);
}

pub(crate) fn read_padded(data: &[u8], offset: usize, len: usize) -> String {
    if offset + len > data.len() {
        return String::new();
    }
    String::from_utf8_lossy(&data[offset..offset + len])
        .trim_end()
        .to_string()
}

pub(crate) fn push_u16(buf: &mut Vec<u8>, val: u16) {
    buf.push((val >> 8) as u8);
    buf.push(val as u8);
}

pub(crate) fn push_u32(buf: &mut Vec<u8>, val: u32) {
    buf.push((val >> 24) as u8);
    buf.push((val >> 16) as u8);
    buf.push((val >> 8) as u8);
    buf.push(val as u8);
}

pub(crate) fn push_u64(buf: &mut Vec<u8>, val: u64) {
    buf.push((val >> 56) as u8);
    buf.push((val >> 48) as u8);
    buf.push((val >> 40) as u8);
    buf.push((val >> 32) as u8);
    buf.push((val >> 24) as u8);
    buf.push((val >> 16) as u8);
    buf.push((val >> 8) as u8);
    buf.push(val as u8);
}

pub(crate) fn read_u16(data: &[u8], offset: usize) -> u16 {
    if offset + 2 > data.len() {
        return 0;
    }
    u16::from_be_bytes([data[offset], data[offset + 1]])
}

#[allow(dead_code)]
pub(crate) fn read_u32(data: &[u8], offset: usize) -> u32 {
    if offset + 4 > data.len() {
        return 0;
    }
    u32::from_be_bytes([data[offset], data[offset + 1], data[offset + 2], data[offset + 3]])
}

pub(crate) fn read_u64(data: &[u8], offset: usize) -> u64 {
    if offset + 8 > data.len() {
        return 0;
    }
    u64::from_be_bytes([
        data[offset],
        data[offset + 1],
        data[offset + 2],
        data[offset + 3],
        data[offset + 4],
        data[offset + 5],
        data[offset + 6],
        data[offset + 7],
    ])
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_smf_header_new() {
        let hdr = SmfHeader::new(30);
        assert_eq!(hdr.record_type, 30);
        assert_eq!(hdr.system_id, "SYS1");
    }

    #[test]
    fn test_smf_header_set_date() {
        let mut hdr = SmfHeader::new(4);
        hdr.set_date(2024, 150);
        assert_ne!(hdr.date, 0);
        // Century = 1, YY = 24, DDD = 150
        let century = (hdr.date >> 24) & 0xFF;
        assert_eq!(century, 1);
    }

    #[test]
    fn test_smf_header_set_time() {
        let mut hdr = SmfHeader::new(4);
        hdr.set_time(14, 30, 0, 0);
        // 14*360000 + 30*6000 = 5040000 + 180000 = 5220000
        assert_eq!(hdr.time, 5220000);
    }

    #[test]
    fn test_smf_header_to_bytes() {
        let hdr = SmfHeader::new(4);
        let bytes = hdr.to_bytes();
        assert_eq!(bytes.len(), 18);
        assert_eq!(bytes[5], 4); // record type
    }

    #[test]
    fn test_smf_header_roundtrip() {
        let mut hdr = SmfHeader::new(30);
        hdr.rdw_length = 100;
        hdr.flag = 0x42;
        hdr.set_date(2024, 150);
        hdr.set_time(14, 30, 0, 0);
        let bytes = hdr.to_bytes();
        let restored = SmfHeader::from_bytes(&bytes).unwrap();
        assert_eq!(restored.record_type, 30);
        assert_eq!(restored.rdw_length, 100);
        assert_eq!(restored.flag, 0x42);
        assert_eq!(restored.time, hdr.time);
        assert_eq!(restored.date, hdr.date);
        assert_eq!(restored.system_id, "SYS1");
    }

    #[test]
    fn test_smf_header_from_bytes_insufficient() {
        let err = SmfHeader::from_bytes(&[0u8; 10]).unwrap_err();
        assert!(matches!(err, SmfRecordError::InsufficientData { .. }));
    }

    #[test]
    fn test_smf_record_new() {
        let rec = SmfRecord::new(4, vec![1, 2, 3, 4]);
        assert_eq!(rec.header.record_type, 4);
        assert_eq!(rec.data.len(), 4);
    }

    #[test]
    fn test_smf_record_to_bytes() {
        let rec = SmfRecord::new(5, vec![0xAA, 0xBB]);
        let bytes = rec.to_bytes();
        assert_eq!(bytes.len(), 20); // 18 header + 2 data
    }

    #[test]
    fn test_smf_record_roundtrip() {
        let rec = SmfRecord::new(42, vec![0xDE, 0xAD, 0xBE, 0xEF]);
        let bytes = rec.to_bytes();
        let restored = SmfRecord::from_bytes(&bytes).unwrap();
        assert_eq!(restored.header.record_type, 42);
        assert_eq!(restored.data, vec![0xDE, 0xAD, 0xBE, 0xEF]);
    }

    #[test]
    fn test_record_type_registry() {
        let mut registry = SmfRecordRegistry::new();
        registry.register_type(
            30,
            Box::new(|rec| format!("Type30(len={})", rec.data.len())),
        );
        assert!(registry.is_registered(30));
        assert!(!registry.is_registered(80));

        let rec = SmfRecord::new(30, vec![0; 10]);
        let result = registry.parse(&rec).unwrap();
        assert_eq!(result, "Type30(len=10)");

        let rec2 = SmfRecord::new(80, vec![0; 5]);
        assert!(registry.parse(&rec2).is_err());
    }

    #[test]
    fn test_record_type_registry_types() {
        let mut registry = SmfRecordRegistry::new();
        registry.register_type(30, Box::new(|_| "t30".to_string()));
        registry.register_type(80, Box::new(|_| "t80".to_string()));
        let types = registry.registered_types();
        assert_eq!(types, vec![30, 80]);
    }

    #[test]
    fn test_record_type_from_code() {
        assert_eq!(SmfRecordType::from_code(4), SmfRecordType::Type4);
        assert_eq!(SmfRecordType::from_code(30), SmfRecordType::Type30);
        assert_eq!(SmfRecordType::from_code(80), SmfRecordType::Type80);
        assert_eq!(SmfRecordType::from_code(200), SmfRecordType::User(200));
    }

    #[test]
    fn test_type4_to_record() {
        let step = SmfType4 {
            job_name: "MYJOB".to_string(),
            step_name: "STEP1".to_string(),
            program_name: "IEFBR14".to_string(),
            step_number: 1,
            completion_code: 0,
            abend_code: 0,
            cpu_time: 500,
            elapsed_time: 1000,
            region_size: 4096,
            excp_count: 100,
        };
        let rec = step.to_record();
        assert_eq!(rec.header.record_type, 4);
        assert!(!rec.data.is_empty());
    }

    #[test]
    fn test_type5_to_record() {
        let job = SmfType5 {
            job_name: "MYJOB".to_string(),
            job_id: "JOB00001".to_string(),
            user_id: "USER01".to_string(),
            job_class: "A".to_string(),
            step_count: 3,
            max_completion_code: 4,
            total_cpu_time: 1500,
            total_elapsed_time: 5000,
            reader_start_time: 100,
            job_end_time: 5100,
        };
        let rec = job.to_record();
        assert_eq!(rec.header.record_type, 5);
        assert!(!rec.data.is_empty());
    }

    #[test]
    fn test_type30_to_record() {
        let work = SmfType30 {
            subtype: SmfSubtype30::JobEnd,
            job_name: "MYJOB".to_string(),
            job_id: "JOB00001".to_string(),
            step_name: "STEP1".to_string(),
            program_name: "PROG1".to_string(),
            user_id: "USER01".to_string(),
            service_class: "ONLINE".to_string(),
            cpu_time_us: 1_000_000,
            io_count: 500,
            ..Default::default()
        };
        let rec = work.to_record();
        assert_eq!(rec.header.record_type, 30);
        assert_eq!(rec.header.subtype, 5);
    }

    #[test]
    fn test_record_type_codes() {
        assert_eq!(SmfRecordType::Type4.code(), 4);
        assert_eq!(SmfRecordType::Type5.code(), 5);
        assert_eq!(SmfRecordType::Type30.code(), 30);
        assert_eq!(SmfRecordType::User(200).code(), 200);
    }

    #[test]
    fn test_subtype30_codes() {
        assert_eq!(SmfSubtype30::JobStart.code(), 1);
        assert_eq!(SmfSubtype30::JobEnd.code(), 5);
    }

    #[test]
    fn test_helper_read_padded() {
        let data = b"HELLO   WORLD   ";
        assert_eq!(read_padded(data, 0, 8), "HELLO");
        assert_eq!(read_padded(data, 8, 8), "WORLD");
    }

    #[test]
    fn test_helper_u16_roundtrip() {
        let mut buf = Vec::new();
        push_u16(&mut buf, 0x1234);
        assert_eq!(read_u16(&buf, 0), 0x1234);
    }

    #[test]
    fn test_helper_u32_roundtrip() {
        let mut buf = Vec::new();
        push_u32(&mut buf, 0xDEADBEEF);
        assert_eq!(read_u32(&buf, 0), 0xDEADBEEF);
    }

    #[test]
    fn test_helper_u64_roundtrip() {
        let mut buf = Vec::new();
        push_u64(&mut buf, 0x0102030405060708);
        assert_eq!(read_u64(&buf, 0), 0x0102030405060708);
    }
}
