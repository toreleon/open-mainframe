//! Error types for TUI sessions.

use thiserror::Error;

/// Errors that can occur during an interactive CICS session.
#[derive(Debug, Error)]
pub enum SessionError {
    /// Terminal initialization failed.
    #[error("Terminal initialization failed: {0}")]
    TerminalInit(#[from] std::io::Error),

    /// Program not found for execution or XCTL.
    #[error("Program not found: {name}")]
    ProgramNotFound { name: String },

    /// COBOL execution error.
    #[error("COBOL execution error in {program}: {message}")]
    Execution { program: String, message: String },

    /// CICS error during command processing.
    #[error("CICS error: {0}")]
    Cics(String),

    /// Session interrupted by user (Ctrl+C).
    #[error("Session interrupted by user")]
    Interrupted,

    /// Transaction ID not found in mapping.
    #[error("TRANSID not found: {0}")]
    TransidNotFound(String),

    /// Maximum XCTL chain depth exceeded.
    #[error("Maximum XCTL chain depth ({0}) exceeded")]
    XctlDepthExceeded(usize),
}
