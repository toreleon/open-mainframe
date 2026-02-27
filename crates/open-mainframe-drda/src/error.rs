//! DRDA server error types.

use thiserror::Error;

/// Errors that can occur during DRDA protocol handling.
#[derive(Error, Debug)]
pub enum DrdaError {
    /// I/O error on the TCP connection.
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    /// Invalid DSS frame received.
    #[error("Invalid DSS frame: {0}")]
    InvalidDss(String),

    /// Invalid DDM object received.
    #[error("Invalid DDM object: {0}")]
    InvalidDdm(String),

    /// Unexpected code point received.
    #[error("Unexpected code point 0x{0:04X} in state {1}")]
    UnexpectedCodePoint(u16, String),

    /// Authentication failure.
    #[error("Authentication failed for user '{0}'")]
    AuthFailed(String),

    /// Unknown database name.
    #[error("Unknown database '{0}'")]
    UnknownDatabase(String),

    /// Connection closed by client.
    #[error("Connection closed")]
    ConnectionClosed,

    /// Protocol error.
    #[error("Protocol error: {0}")]
    Protocol(String),
}

/// Result type for DRDA operations.
pub type DrdaResult<T> = Result<T, DrdaError>;
