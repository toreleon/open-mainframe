//! Utility program error types.

use miette::Diagnostic;
use thiserror::Error;

/// Errors returned by utility program operations.
#[derive(Debug, Error, Diagnostic)]
pub enum UtilityError {
    /// Program not found in the utility registry.
    #[error("S806 ABEND: program '{name}' not found")]
    #[diagnostic(code(utility::program_not_found))]
    ProgramNotFound { name: String },

    /// Required DD statement is missing.
    #[error("DD statement '{ddname}' required but not allocated")]
    #[diagnostic(code(utility::dd_not_found))]
    DdNotFound { ddname: String },

    /// Invalid control statement in SYSIN.
    #[error("invalid control statement: {detail}")]
    #[diagnostic(code(utility::invalid_control_stmt))]
    InvalidControlStatement { detail: String },

    /// I/O error during utility processing.
    #[error("I/O error on DD '{ddname}': {detail}")]
    #[diagnostic(code(utility::io_error))]
    IoError { ddname: String, detail: String },

    /// Utility-specific processing error.
    #[error("{program}: {detail}")]
    #[diagnostic(code(utility::processing_error))]
    ProcessingError { program: String, detail: String },
}
