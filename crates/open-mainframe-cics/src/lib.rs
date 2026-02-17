//! CICS transaction processing for OpenMainframe.
//!
//! This crate provides CICS support by:
//! - Preprocessing EXEC CICS commands in COBOL source
//! - Runtime library for executing CICS commands
//! - BMS map support for terminal I/O
//! - File operations (VSAM integration)
//!
//! # Example
//!
//! ```ignore
//! use open_mainframe_cics::preprocess::CicsPreprocessor;
//!
//! let source = r#"
//!     EXEC CICS
//!       LINK PROGRAM('SUBPROG')
//!            COMMAREA(WS-DATA)
//!     END-EXEC.
//! "#;
//!
//! let mut preprocessor = CicsPreprocessor::new();
//! let result = preprocessor.process(source)?;
//! ```

pub mod bms;
pub mod channels;
pub mod interval;
pub mod preprocess;
pub mod queues;
pub mod runtime;
pub mod sync;
pub mod terminal;
pub mod time;

use thiserror::Error;

/// Errors that can occur during CICS operations.
#[derive(Error, Debug)]
pub enum CicsError {
    /// Command syntax error
    #[error("CICS command syntax error at line {line}: {message}")]
    SyntaxError { line: usize, message: String },

    /// Unknown command
    #[error("Unknown CICS command: {0}")]
    UnknownCommand(String),

    /// Program not found
    #[error("Program not found: {0}")]
    ProgramNotFound(String),

    /// File not found
    #[error("File not found: {0}")]
    FileNotFound(String),

    /// Record not found
    #[error("Record not found")]
    RecordNotFound,

    /// Duplicate record
    #[error("Duplicate record")]
    DuplicateRecord,

    /// Invalid request
    #[error("Invalid request: {0}")]
    InvalidRequest(String),

    /// File operation error with response code
    #[error("File error: {0:?}")]
    FileError(CicsResponse),

    /// IO error
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

/// Result type for CICS operations.
pub type CicsResult<T> = Result<T, CicsError>;

/// CICS response codes (EIBRESP values).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum CicsResponse {
    /// Normal completion
    Normal = 0,
    /// ERROR condition
    Error = 1,
    /// RDATT - read attention
    Rdatt = 2,
    /// WRBRK - write break
    Wrbrk = 3,
    /// EOF - end of file
    Eof = 4,
    /// EODS - end of data set
    Eods = 5,
    /// EOC - end of chain
    Eoc = 6,
    /// INBFMH - inbound FMH
    Inbfmh = 7,
    /// ENDINPT - end of input
    Endinpt = 8,
    /// NONVAL - non-valid data
    Nonval = 9,
    /// NOSTART - not started
    Nostart = 10,
    /// TERMIDERR - terminal ID error
    Termiderr = 11,
    /// FILENOTFOUND - file not found
    Filenotfound = 12,
    /// NOTFND - record not found
    Notfnd = 13,
    /// DUPREC - duplicate record
    Duprec = 14,
    /// DUPKEY - duplicate key
    Dupkey = 15,
    /// INVREQ - invalid request
    Invreq = 16,
    /// IOERR - I/O error
    Ioerr = 17,
    /// NOSPACE - no space
    Nospace = 18,
    /// NOTOPEN - file not open
    Notopen = 19,
    /// ENDFILE - end of file
    Endfile = 20,
    /// ILLOGIC - illogical error
    Illogic = 21,
    /// LENGERR - length error
    Lengerr = 22,
    /// QZERO - queue zero
    Qzero = 23,
    /// SIGNAL - signal received
    Signal = 24,
    /// QBUSY - queue busy
    Qbusy = 25,
    /// ITEMERR - item error
    Itemerr = 26,
    /// PGMIDERR - program ID error
    Pgmiderr = 27,
    /// TRANSIDERR - transaction ID error
    Transiderr = 28,
    /// ENDDATA - end of data
    Enddata = 29,
    /// INVTSREQ - invalid TS request
    Invtsreq = 30,
    /// EXPIRED - timeout expired
    Expired = 31,
    /// MAPFAIL - map failure
    Mapfail = 36,
    /// INVMPSZ - invalid map size
    Invmpsz = 38,
    /// OVERFLOW - overflow
    Overflow = 40,
    /// INVLDC - invalid LDC
    Invldc = 49,
    /// NOSTG - no storage
    Nostg = 42,
    /// JIDERR - journal ID error
    Jiderr = 43,
    /// QIDERR - queue ID error
    Qiderr = 44,
    /// NOJBUFSP - no journal buffer space
    Nojbufsp = 45,
    /// DISABLED - disabled
    Disabled = 84,
    /// NOTAUTH - not authorized
    Notauth = 70,
}

impl CicsResponse {
    /// Convert from u32.
    pub fn from_u32(value: u32) -> Option<Self> {
        match value {
            0 => Some(CicsResponse::Normal),
            1 => Some(CicsResponse::Error),
            13 => Some(CicsResponse::Notfnd),
            14 => Some(CicsResponse::Duprec),
            16 => Some(CicsResponse::Invreq),
            27 => Some(CicsResponse::Pgmiderr),
            _ => None,
        }
    }

    /// Get the condition name.
    pub fn condition_name(&self) -> &'static str {
        match self {
            CicsResponse::Normal => "NORMAL",
            CicsResponse::Error => "ERROR",
            CicsResponse::Rdatt => "RDATT",
            CicsResponse::Wrbrk => "WRBRK",
            CicsResponse::Eof => "EOF",
            CicsResponse::Eods => "EODS",
            CicsResponse::Eoc => "EOC",
            CicsResponse::Inbfmh => "INBFMH",
            CicsResponse::Endinpt => "ENDINPT",
            CicsResponse::Nonval => "NONVAL",
            CicsResponse::Nostart => "NOSTART",
            CicsResponse::Termiderr => "TERMIDERR",
            CicsResponse::Filenotfound => "FILENOTFOUND",
            CicsResponse::Notfnd => "NOTFND",
            CicsResponse::Duprec => "DUPREC",
            CicsResponse::Dupkey => "DUPKEY",
            CicsResponse::Invreq => "INVREQ",
            CicsResponse::Ioerr => "IOERR",
            CicsResponse::Nospace => "NOSPACE",
            CicsResponse::Notopen => "NOTOPEN",
            CicsResponse::Endfile => "ENDFILE",
            CicsResponse::Illogic => "ILLOGIC",
            CicsResponse::Lengerr => "LENGERR",
            CicsResponse::Qzero => "QZERO",
            CicsResponse::Signal => "SIGNAL",
            CicsResponse::Qbusy => "QBUSY",
            CicsResponse::Itemerr => "ITEMERR",
            CicsResponse::Pgmiderr => "PGMIDERR",
            CicsResponse::Transiderr => "TRANSIDERR",
            CicsResponse::Enddata => "ENDDATA",
            CicsResponse::Invtsreq => "INVTSREQ",
            CicsResponse::Expired => "EXPIRED",
            CicsResponse::Mapfail => "MAPFAIL",
            CicsResponse::Invmpsz => "INVMPSZ",
            CicsResponse::Overflow => "OVERFLOW",
            CicsResponse::Invldc => "INVLDC",
            CicsResponse::Nostg => "NOSTG",
            CicsResponse::Jiderr => "JIDERR",
            CicsResponse::Qiderr => "QIDERR",
            CicsResponse::Nojbufsp => "NOJBUFSP",
            CicsResponse::Disabled => "DISABLED",
            CicsResponse::Notauth => "NOTAUTH",
        }
    }
}
