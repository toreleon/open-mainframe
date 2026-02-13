//! DB2 SQL preprocessing and runtime for zOS-clone.
//!
//! This crate provides DB2 SQL support by:
//! - Preprocessing EXEC SQL statements in COBOL source
//! - Translating DB2 SQL to PostgreSQL dialect
//! - Runtime library for executing SQL operations
//! - DCLGEN utility for generating COBOL copybooks
//!
//! # Example
//!
//! ```ignore
//! use zos_db2::preprocess::SqlPreprocessor;
//!
//! let source = r#"
//!     EXEC SQL
//!       SELECT NAME INTO :WS-NAME
//!       FROM CUSTOMER
//!       WHERE CUSTNO = :WS-CUSTNO
//!     END-EXEC.
//! "#;
//!
//! let mut preprocessor = SqlPreprocessor::new();
//! let result = preprocessor.process(source)?;
//! ```

pub mod preprocess;

use thiserror::Error;

/// Errors that can occur during DB2 operations.
#[derive(Error, Debug)]
pub enum Db2Error {
    /// SQL syntax error
    #[error("SQL syntax error at line {line}: {message}")]
    SyntaxError { line: usize, message: String },

    /// Invalid host variable
    #[error("Invalid host variable '{name}': {reason}")]
    InvalidHostVariable { name: String, reason: String },

    /// Unsupported SQL feature
    #[error("Unsupported SQL feature: {0}")]
    UnsupportedFeature(String),

    /// IO error
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

/// Result type for DB2 operations.
pub type Db2Result<T> = Result<T, Db2Error>;
