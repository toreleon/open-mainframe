//! IMS/DB hierarchical database support for OpenMainframe.
//!
//! This crate provides IMS (Information Management System) support including:
//! - DBD (Database Definition) parsing
//! - PSB (Program Specification Block) parsing
//! - DL/I (Data Language/I) call interface
//! - Runtime support for hierarchical data access
//!
//! # Example
//!
//! ```ignore
//! use open_mainframe_ims::dli::{DliCall, DliRuntime};
//!
//! let mut runtime = DliRuntime::new()?;
//! runtime.schedule_psb("MYPSB")?;
//!
//! // Get Unique call
//! let result = runtime.gu(
//!     "CUSTOMER",
//!     Some("CUSTNO  = 12345"),
//!     &mut buffer,
//! )?;
//! ```

pub mod dbd;
pub mod dli;
pub mod persist;
pub mod preprocess;
pub mod psb;
pub mod runtime;
pub mod schema;

use thiserror::Error;

/// Errors that can occur during IMS operations.
#[derive(Error, Debug)]
pub enum ImsError {
    /// DBD parse error
    #[error("DBD parse error at line {line}: {message}")]
    DbdParseError { line: usize, message: String },

    /// PSB parse error
    #[error("PSB parse error at line {line}: {message}")]
    PsbParseError { line: usize, message: String },

    /// Database not found
    #[error("Database not found: {0}")]
    DatabaseNotFound(String),

    /// Segment not found
    #[error("Segment not found: {0}")]
    SegmentNotFound(String),

    /// PCB not found
    #[error("PCB not found: {0}")]
    PcbNotFound(String),

    /// Invalid SSA
    #[error("Invalid SSA: {0}")]
    InvalidSsa(String),

    /// DL/I call error
    #[error("DL/I error: status {status}")]
    DliError { status: StatusCode },

    /// Database I/O error
    #[error("Database I/O error: {0}")]
    IoError(String),

    /// Connection error
    #[error("Connection error: {0}")]
    ConnectionError(String),
}

/// Result type for IMS operations.
pub type ImsResult<T> = Result<T, ImsError>;

/// Status code category for classification.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StatusCategory {
    /// Successful completion
    Success,
    /// Informational — not an error, e.g. GA, GK
    Informational,
    /// Not-found — segment/record end conditions
    NotFound,
    /// Program error — logic error in the application
    ProgramError,
    /// I/O or system error
    SystemError,
    /// Message queue status
    MessageQueue,
}

/// IMS DL/I status codes.
///
/// Covers database, system service, GSAM, and message queue operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StatusCode {
    // --- Success ---
    /// Successful completion (spaces)
    Ok,

    // --- Database Get informational ---
    /// Move to next database record of a different type (hierarchical ascent)
    GA,
    /// End of database (no more records)
    GB,
    /// Segment occurrence not found but a different type returned
    GD,
    /// Segment not found
    GE,
    /// Segment type obtained differs from SSA (informational, GN path)
    GK,

    // --- Database Get / navigation errors ---
    /// Sequence error (GNP without parentage)
    GP,

    // --- Insert status ---
    /// Duplicate key on insert
    II,
    /// Segment inserted but twin chain interrupted (informational)
    IX,

    // --- Replace / Delete status ---
    /// No prior get hold for replace/delete
    DJ,
    /// Delete rule violation
    DX,
    /// Key field changed on replace
    RX,

    // --- Application / protocol errors (A-class) ---
    /// Function code not valid for PCB type
    AA,
    /// Invalid call — unable to accept
    AB,
    /// Invalid change to key field
    AC,
    /// Invalid function code
    AD,
    /// Invalid field length or data in SSA
    AF,
    /// Function not allowed for this PCB processing option
    AH,
    /// Insufficient buffer length
    AJ,
    /// Invalid segment name in SSA
    AK,
    /// Data exceeds segment length
    AL,
    /// Missing or invalid I/O area
    AM,
    /// Invalid PCB address or PCB not open
    AO,
    /// PSB not scheduled
    AP,
    /// Invalid PCB
    AI,

    // --- Batch / GSAM status ---
    /// GSAM: physical end of data set
    BA,

    // --- Dead-letter / MFS errors ---
    /// Data not available for field
    DA,

    // --- Logical relationship errors ---
    /// Segment not accessible via this path
    FD,
    /// Segment not defined in the DBD for this logical view
    FR,

    // --- Scheduling / PSB errors ---
    /// Invalid SSA or SSA not allowed
    NI,

    // --- Message queue status ---
    /// No more messages in queue (I/O PCB)
    QC,
    /// No more segments in current message (I/O PCB)
    QD,

    // --- Transaction / sync errors ---
    /// Transaction code not valid
    TG,

    // --- Other ---
    /// Lock / deadlock condition
    XD,

    /// Unknown/unrecognised two-character status
    Unknown(char, char),
}

impl StatusCode {
    /// Create from two-character status.
    pub fn from_chars(c1: char, c2: char) -> Self {
        match (c1, c2) {
            (' ', ' ') => StatusCode::Ok,
            // G-class: database get
            ('G', 'A') => StatusCode::GA,
            ('G', 'B') => StatusCode::GB,
            ('G', 'D') => StatusCode::GD,
            ('G', 'E') => StatusCode::GE,
            ('G', 'K') => StatusCode::GK,
            ('G', 'P') => StatusCode::GP,
            // I-class: insert
            ('I', 'I') => StatusCode::II,
            ('I', 'X') => StatusCode::IX,
            // D-class: delete
            ('D', 'J') => StatusCode::DJ,
            ('D', 'X') => StatusCode::DX,
            // R-class: replace
            ('R', 'X') => StatusCode::RX,
            // A-class: application/protocol errors
            ('A', 'A') => StatusCode::AA,
            ('A', 'B') => StatusCode::AB,
            ('A', 'C') => StatusCode::AC,
            ('A', 'D') => StatusCode::AD,
            ('A', 'F') => StatusCode::AF,
            ('A', 'H') => StatusCode::AH,
            ('A', 'I') => StatusCode::AI,
            ('A', 'J') => StatusCode::AJ,
            ('A', 'K') => StatusCode::AK,
            ('A', 'L') => StatusCode::AL,
            ('A', 'M') => StatusCode::AM,
            ('A', 'O') => StatusCode::AO,
            ('A', 'P') => StatusCode::AP,
            // B-class: batch/GSAM
            ('B', 'A') => StatusCode::BA,
            // D-class: data
            ('D', 'A') => StatusCode::DA,
            // F-class: logical relationship
            ('F', 'D') => StatusCode::FD,
            ('F', 'R') => StatusCode::FR,
            // N-class: SSA
            ('N', 'I') => StatusCode::NI,
            // Q-class: message queue
            ('Q', 'C') => StatusCode::QC,
            ('Q', 'D') => StatusCode::QD,
            // T-class: transaction
            ('T', 'G') => StatusCode::TG,
            // X-class: lock/deadlock
            ('X', 'D') => StatusCode::XD,
            _ => StatusCode::Unknown(c1, c2),
        }
    }

    /// Convert to two-character string.
    pub fn as_str_pair(&self) -> String {
        match self {
            StatusCode::Ok => "  ".to_string(),
            StatusCode::GA => "GA".to_string(),
            StatusCode::GB => "GB".to_string(),
            StatusCode::GD => "GD".to_string(),
            StatusCode::GE => "GE".to_string(),
            StatusCode::GK => "GK".to_string(),
            StatusCode::GP => "GP".to_string(),
            StatusCode::II => "II".to_string(),
            StatusCode::IX => "IX".to_string(),
            StatusCode::DJ => "DJ".to_string(),
            StatusCode::DX => "DX".to_string(),
            StatusCode::RX => "RX".to_string(),
            StatusCode::AA => "AA".to_string(),
            StatusCode::AB => "AB".to_string(),
            StatusCode::AC => "AC".to_string(),
            StatusCode::AD => "AD".to_string(),
            StatusCode::AF => "AF".to_string(),
            StatusCode::AH => "AH".to_string(),
            StatusCode::AI => "AI".to_string(),
            StatusCode::AJ => "AJ".to_string(),
            StatusCode::AK => "AK".to_string(),
            StatusCode::AL => "AL".to_string(),
            StatusCode::AM => "AM".to_string(),
            StatusCode::AO => "AO".to_string(),
            StatusCode::AP => "AP".to_string(),
            StatusCode::BA => "BA".to_string(),
            StatusCode::DA => "DA".to_string(),
            StatusCode::FD => "FD".to_string(),
            StatusCode::FR => "FR".to_string(),
            StatusCode::NI => "NI".to_string(),
            StatusCode::QC => "QC".to_string(),
            StatusCode::QD => "QD".to_string(),
            StatusCode::TG => "TG".to_string(),
            StatusCode::XD => "XD".to_string(),
            StatusCode::Unknown(c1, c2) => format!("{}{}", c1, c2),
        }
    }

    /// Check if status indicates success.
    pub fn is_ok(&self) -> bool {
        matches!(self, StatusCode::Ok)
    }

    /// Check if status indicates not found / end-of-data.
    pub fn is_not_found(&self) -> bool {
        matches!(
            self,
            StatusCode::GE | StatusCode::GB | StatusCode::GK | StatusCode::GD
        )
    }

    /// Check if status indicates an error (program or system).
    pub fn is_error(&self) -> bool {
        matches!(
            self,
            StatusCode::II
                | StatusCode::DJ
                | StatusCode::DX
                | StatusCode::RX
                | StatusCode::AA
                | StatusCode::AB
                | StatusCode::AC
                | StatusCode::AD
                | StatusCode::AF
                | StatusCode::AH
                | StatusCode::AI
                | StatusCode::AJ
                | StatusCode::AK
                | StatusCode::AL
                | StatusCode::AM
                | StatusCode::AO
                | StatusCode::AP
                | StatusCode::BA
                | StatusCode::FD
                | StatusCode::FR
                | StatusCode::NI
                | StatusCode::TG
                | StatusCode::XD
        )
    }

    /// Check if status is informational (not an error, not success).
    pub fn is_informational(&self) -> bool {
        matches!(self, StatusCode::GA | StatusCode::IX)
    }

    /// Human-readable description of this status code.
    pub fn description(&self) -> &'static str {
        match self {
            StatusCode::Ok => "Successful completion",
            StatusCode::GA => "Hierarchical ascent — different segment type returned",
            StatusCode::GB => "End of database — no more segments",
            StatusCode::GD => "Segment not found but different type returned",
            StatusCode::GE => "Segment not found",
            StatusCode::GK => "Different segment type at this level",
            StatusCode::GP => "Parentage not established for GNP",
            StatusCode::II => "Duplicate key on insert",
            StatusCode::IX => "Segment inserted, twin chain interrupted",
            StatusCode::DJ => "No prior get hold for replace/delete",
            StatusCode::DX => "Delete rule violation",
            StatusCode::RX => "Key field changed on replace",
            StatusCode::AA => "Function code not valid for PCB type",
            StatusCode::AB => "Unable to accept call",
            StatusCode::AC => "Invalid change to key field",
            StatusCode::AD => "Invalid function code",
            StatusCode::AF => "Invalid field length or data in SSA",
            StatusCode::AH => "Function not allowed for PCB processing option",
            StatusCode::AI => "Invalid PCB address",
            StatusCode::AJ => "Insufficient buffer length",
            StatusCode::AK => "Invalid segment name in SSA",
            StatusCode::AL => "Data exceeds segment length",
            StatusCode::AM => "Missing or invalid I/O area",
            StatusCode::AO => "PCB not open or invalid PCB address",
            StatusCode::AP => "PSB not scheduled",
            StatusCode::BA => "GSAM physical end of data set",
            StatusCode::DA => "Data not available for field",
            StatusCode::FD => "Segment not accessible via this path",
            StatusCode::FR => "Segment not defined in DBD for this logical view",
            StatusCode::NI => "Invalid SSA or SSA not allowed",
            StatusCode::QC => "No more messages in queue",
            StatusCode::QD => "No more segments in current message",
            StatusCode::TG => "Transaction code not valid",
            StatusCode::XD => "Deadlock or lock timeout",
            StatusCode::Unknown(_, _) => "Unknown status code",
        }
    }

    /// Classification category for this status code.
    pub fn category(&self) -> StatusCategory {
        match self {
            StatusCode::Ok => StatusCategory::Success,
            StatusCode::GA | StatusCode::GK | StatusCode::IX => StatusCategory::Informational,
            StatusCode::GB | StatusCode::GD | StatusCode::GE => StatusCategory::NotFound,
            StatusCode::GP
            | StatusCode::II
            | StatusCode::DJ
            | StatusCode::DX
            | StatusCode::RX
            | StatusCode::AA
            | StatusCode::AB
            | StatusCode::AC
            | StatusCode::AD
            | StatusCode::AF
            | StatusCode::AH
            | StatusCode::AI
            | StatusCode::AJ
            | StatusCode::AL
            | StatusCode::AM
            | StatusCode::AO
            | StatusCode::AP
            | StatusCode::NI
            | StatusCode::TG => StatusCategory::ProgramError,
            StatusCode::AK | StatusCode::BA | StatusCode::DA | StatusCode::FD | StatusCode::FR | StatusCode::XD => {
                StatusCategory::SystemError
            }
            StatusCode::QC | StatusCode::QD => StatusCategory::MessageQueue,
            StatusCode::Unknown(_, _) => StatusCategory::SystemError,
        }
    }

    /// Total number of named status codes defined (excluding Unknown).
    pub fn defined_count() -> usize {
        33
    }
}

impl Default for StatusCode {
    fn default() -> Self {
        StatusCode::Ok
    }
}

impl std::fmt::Display for StatusCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str_pair())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_status_code_from_chars() {
        assert_eq!(StatusCode::from_chars(' ', ' '), StatusCode::Ok);
        assert_eq!(StatusCode::from_chars('G', 'E'), StatusCode::GE);
        assert_eq!(StatusCode::from_chars('G', 'B'), StatusCode::GB);
        assert_eq!(StatusCode::from_chars('I', 'I'), StatusCode::II);
    }

    #[test]
    fn test_status_code_to_string() {
        assert_eq!(StatusCode::Ok.as_str_pair(), "  ");
        assert_eq!(StatusCode::GE.as_str_pair(), "GE");
        assert_eq!(StatusCode::II.as_str_pair(), "II");
    }

    #[test]
    fn test_status_code_checks() {
        assert!(StatusCode::Ok.is_ok());
        assert!(!StatusCode::GE.is_ok());

        assert!(StatusCode::GE.is_not_found());
        assert!(StatusCode::GB.is_not_found());
        assert!(!StatusCode::Ok.is_not_found());

        assert!(StatusCode::II.is_error());
        assert!(StatusCode::DJ.is_error());
        assert!(!StatusCode::Ok.is_error());
    }

    // --- Epic 405: Extended status codes ---

    #[test]
    fn test_defined_count_at_least_30() {
        assert!(
            StatusCode::defined_count() >= 30,
            "Expected at least 30 status codes, got {}",
            StatusCode::defined_count()
        );
    }

    #[test]
    fn test_new_status_codes_from_chars() {
        assert_eq!(StatusCode::from_chars('A', 'A'), StatusCode::AA);
        assert_eq!(StatusCode::from_chars('A', 'B'), StatusCode::AB);
        assert_eq!(StatusCode::from_chars('A', 'C'), StatusCode::AC);
        assert_eq!(StatusCode::from_chars('A', 'F'), StatusCode::AF);
        assert_eq!(StatusCode::from_chars('A', 'H'), StatusCode::AH);
        assert_eq!(StatusCode::from_chars('A', 'J'), StatusCode::AJ);
        assert_eq!(StatusCode::from_chars('A', 'L'), StatusCode::AL);
        assert_eq!(StatusCode::from_chars('A', 'M'), StatusCode::AM);
        assert_eq!(StatusCode::from_chars('A', 'O'), StatusCode::AO);
        assert_eq!(StatusCode::from_chars('B', 'A'), StatusCode::BA);
        assert_eq!(StatusCode::from_chars('D', 'A'), StatusCode::DA);
        assert_eq!(StatusCode::from_chars('D', 'X'), StatusCode::DX);
        assert_eq!(StatusCode::from_chars('F', 'D'), StatusCode::FD);
        assert_eq!(StatusCode::from_chars('F', 'R'), StatusCode::FR);
        assert_eq!(StatusCode::from_chars('G', 'A'), StatusCode::GA);
        assert_eq!(StatusCode::from_chars('G', 'D'), StatusCode::GD);
        assert_eq!(StatusCode::from_chars('G', 'K'), StatusCode::GK);
        assert_eq!(StatusCode::from_chars('G', 'P'), StatusCode::GP);
        assert_eq!(StatusCode::from_chars('I', 'X'), StatusCode::IX);
        assert_eq!(StatusCode::from_chars('N', 'I'), StatusCode::NI);
        assert_eq!(StatusCode::from_chars('T', 'G'), StatusCode::TG);
        assert_eq!(StatusCode::from_chars('X', 'D'), StatusCode::XD);
    }

    #[test]
    fn test_roundtrip_all_named_codes() {
        let codes = [
            StatusCode::Ok,
            StatusCode::GA,
            StatusCode::GB,
            StatusCode::GD,
            StatusCode::GE,
            StatusCode::GK,
            StatusCode::GP,
            StatusCode::II,
            StatusCode::IX,
            StatusCode::DJ,
            StatusCode::DX,
            StatusCode::RX,
            StatusCode::AA,
            StatusCode::AB,
            StatusCode::AC,
            StatusCode::AD,
            StatusCode::AF,
            StatusCode::AH,
            StatusCode::AI,
            StatusCode::AJ,
            StatusCode::AK,
            StatusCode::AL,
            StatusCode::AM,
            StatusCode::AO,
            StatusCode::AP,
            StatusCode::BA,
            StatusCode::DA,
            StatusCode::FD,
            StatusCode::FR,
            StatusCode::NI,
            StatusCode::QC,
            StatusCode::QD,
            StatusCode::TG,
            StatusCode::XD,
        ];

        for code in &codes {
            let pair = code.as_str_pair();
            let chars: Vec<char> = pair.chars().collect();
            let round_tripped = StatusCode::from_chars(chars[0], chars[1]);
            assert_eq!(*code, round_tripped, "Roundtrip failed for {}", pair);
        }
    }

    #[test]
    fn test_description_not_empty() {
        let codes = [
            StatusCode::Ok, StatusCode::GA, StatusCode::GB, StatusCode::GE,
            StatusCode::II, StatusCode::DJ, StatusCode::AD, StatusCode::AP,
            StatusCode::BA, StatusCode::QC, StatusCode::XD,
        ];
        for code in &codes {
            assert!(
                !code.description().is_empty(),
                "Description for {} should not be empty",
                code.as_str_pair()
            );
        }
    }

    #[test]
    fn test_category_classification() {
        assert_eq!(StatusCode::Ok.category(), StatusCategory::Success);
        assert_eq!(StatusCode::GA.category(), StatusCategory::Informational);
        assert_eq!(StatusCode::GE.category(), StatusCategory::NotFound);
        assert_eq!(StatusCode::GB.category(), StatusCategory::NotFound);
        assert_eq!(StatusCode::II.category(), StatusCategory::ProgramError);
        assert_eq!(StatusCode::AD.category(), StatusCategory::ProgramError);
        assert_eq!(StatusCode::AK.category(), StatusCategory::SystemError);
        assert_eq!(StatusCode::XD.category(), StatusCategory::SystemError);
        assert_eq!(StatusCode::QC.category(), StatusCategory::MessageQueue);
        assert_eq!(StatusCode::QD.category(), StatusCategory::MessageQueue);
    }

    #[test]
    fn test_is_informational() {
        assert!(StatusCode::GA.is_informational());
        assert!(StatusCode::IX.is_informational());
        assert!(!StatusCode::Ok.is_informational());
        assert!(!StatusCode::GE.is_informational());
    }

    #[test]
    fn test_unknown_status_code() {
        let unk = StatusCode::from_chars('Z', 'Z');
        assert!(matches!(unk, StatusCode::Unknown('Z', 'Z')));
        assert_eq!(unk.as_str_pair(), "ZZ");
        assert_eq!(unk.description(), "Unknown status code");
    }
}
