//! Error types for dataset operations.

use miette::Diagnostic;
use thiserror::Error;

/// Errors that can occur during dataset operations.
#[derive(Debug, Error, Diagnostic)]
pub enum DatasetError {
    /// Dataset not found.
    #[error("Dataset not found: {name}")]
    #[diagnostic(
        code(dataset::not_found),
        help("Check that the dataset exists and is cataloged")
    )]
    NotFound {
        /// Name of the dataset.
        name: String,
    },

    /// I/O error during dataset operations.
    #[error("I/O error: {message}")]
    #[diagnostic(code(dataset::io_error))]
    IoError {
        /// Description of the I/O error.
        message: String,
    },

    /// Invalid record format.
    #[error("Invalid record format: {0}")]
    #[diagnostic(code(dataset::invalid_record_format))]
    InvalidRecordFormat(String),

    /// Dataset already exists.
    #[error("Dataset already exists: {name}")]
    #[diagnostic(code(dataset::already_exists))]
    AlreadyExists {
        /// Name of the dataset.
        name: String,
    },

    /// Invalid dataset name.
    #[error("Invalid dataset name: {name}")]
    #[diagnostic(
        code(dataset::invalid_name),
        help("Dataset names must be 1-44 characters, qualifiers 1-8 characters")
    )]
    InvalidName {
        /// The invalid name.
        name: String,
    },

    /// Record too long.
    #[error("Record length {actual} exceeds maximum {max}")]
    #[diagnostic(code(dataset::record_too_long))]
    RecordTooLong {
        /// Actual record length.
        actual: usize,
        /// Maximum allowed length.
        max: usize,
    },

    /// End of file reached unexpectedly.
    #[error("Unexpected end of file at record {record_number}")]
    #[diagnostic(code(dataset::unexpected_eof))]
    UnexpectedEof {
        /// Record number where EOF occurred.
        record_number: u64,
    },

    /// Catalog error.
    #[error("Catalog error: {message}")]
    #[diagnostic(code(dataset::catalog_error))]
    CatalogError {
        /// Description of the catalog error.
        message: String,
    },

    /// Dataset is in use.
    #[error("Dataset {name} is in use")]
    #[diagnostic(
        code(dataset::in_use),
        help("Wait for other jobs to complete or use DISP=SHR")
    )]
    InUse {
        /// Name of the dataset.
        name: String,
    },

    /// Permission denied.
    #[error("Permission denied: {name}")]
    #[diagnostic(code(dataset::permission_denied))]
    PermissionDenied {
        /// Name of the dataset.
        name: String,
    },
}

impl From<std::io::Error> for DatasetError {
    fn from(err: std::io::Error) -> Self {
        DatasetError::IoError {
            message: err.to_string(),
        }
    }
}
