//! IMS/DB hierarchical database support for zOS-clone.
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
//! use zos_ims::dli::{DliCall, DliRuntime};
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
pub mod preprocess;
pub mod psb;
pub mod runtime;

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

/// IMS DL/I status codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StatusCode {
    /// Successful completion (spaces)
    Ok,
    /// Segment not found
    GE,
    /// End of database
    GB,
    /// Segment not present (for GN after insert)
    GA,
    /// Sequence error
    GP,
    /// No more segments at this level
    GK,
    /// Duplicate key
    II,
    /// No prior get hold for replace
    DJ,
    /// Key field changed on replace
    RX,
    /// PSB not scheduled
    AP,
    /// Invalid function code
    AD,
    /// Invalid PCB
    AI,
    /// Segment I/O error
    AK,
    /// Unknown status
    Unknown(char, char),
}

impl StatusCode {
    /// Create from two-character status.
    pub fn from_chars(c1: char, c2: char) -> Self {
        match (c1, c2) {
            (' ', ' ') => StatusCode::Ok,
            ('G', 'E') => StatusCode::GE,
            ('G', 'B') => StatusCode::GB,
            ('G', 'A') => StatusCode::GA,
            ('G', 'P') => StatusCode::GP,
            ('G', 'K') => StatusCode::GK,
            ('I', 'I') => StatusCode::II,
            ('D', 'J') => StatusCode::DJ,
            ('R', 'X') => StatusCode::RX,
            ('A', 'P') => StatusCode::AP,
            ('A', 'D') => StatusCode::AD,
            ('A', 'I') => StatusCode::AI,
            ('A', 'K') => StatusCode::AK,
            _ => StatusCode::Unknown(c1, c2),
        }
    }

    /// Convert to two-character string.
    pub fn to_string(&self) -> String {
        match self {
            StatusCode::Ok => "  ".to_string(),
            StatusCode::GE => "GE".to_string(),
            StatusCode::GB => "GB".to_string(),
            StatusCode::GA => "GA".to_string(),
            StatusCode::GP => "GP".to_string(),
            StatusCode::GK => "GK".to_string(),
            StatusCode::II => "II".to_string(),
            StatusCode::DJ => "DJ".to_string(),
            StatusCode::RX => "RX".to_string(),
            StatusCode::AP => "AP".to_string(),
            StatusCode::AD => "AD".to_string(),
            StatusCode::AI => "AI".to_string(),
            StatusCode::AK => "AK".to_string(),
            StatusCode::Unknown(c1, c2) => format!("{}{}", c1, c2),
        }
    }

    /// Check if status indicates success.
    pub fn is_ok(&self) -> bool {
        matches!(self, StatusCode::Ok)
    }

    /// Check if status indicates not found.
    pub fn is_not_found(&self) -> bool {
        matches!(self, StatusCode::GE | StatusCode::GB | StatusCode::GK)
    }

    /// Check if status indicates an error.
    pub fn is_error(&self) -> bool {
        matches!(
            self,
            StatusCode::II
                | StatusCode::DJ
                | StatusCode::RX
                | StatusCode::AP
                | StatusCode::AD
                | StatusCode::AI
                | StatusCode::AK
        )
    }
}

impl Default for StatusCode {
    fn default() -> Self {
        StatusCode::Ok
    }
}

impl std::fmt::Display for StatusCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
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
        assert_eq!(StatusCode::Ok.to_string(), "  ");
        assert_eq!(StatusCode::GE.to_string(), "GE");
        assert_eq!(StatusCode::II.to_string(), "II");
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
}
