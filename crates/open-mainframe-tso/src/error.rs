//! TSO error types.

use thiserror::Error;

/// TSO-specific error type.
#[derive(Debug, Error)]
pub enum TsoError {
    /// Command not recognized.
    #[error("IKJ56500I COMMAND {0} NOT FOUND")]
    CommandNotFound(String),

    /// Missing required operand.
    #[error("IKJ56702I MISSING {0} OPERAND")]
    MissingOperand(String),

    /// Invalid operand value.
    #[error("IKJ56703I INVALID OPERAND - {0}")]
    InvalidOperand(String),

    /// Dataset not found.
    #[error("IKJ56704I DATA SET {0} NOT FOUND")]
    DatasetNotFound(String),

    /// DD name not allocated.
    #[error("IKJ56861I FILE {0} NOT ALLOCATED")]
    DdNotAllocated(String),

    /// DD name already allocated.
    #[error("IKJ56862I FILE {0} ALREADY ALLOCATED")]
    DdAlreadyAllocated(String),

    /// I/O error.
    #[error("IKJ56870I I/O ERROR - {0}")]
    IoError(String),

    /// Generic execution error.
    #[error("IKJ56500I {0}")]
    ExecutionError(String),
}

/// Convenience result type.
pub type Result<T> = std::result::Result<T, TsoError>;
