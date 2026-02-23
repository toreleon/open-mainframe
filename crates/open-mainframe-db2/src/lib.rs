//! DB2 SQL preprocessing and runtime for OpenMainframe.
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
//! use open_mainframe_db2::preprocess::SqlPreprocessor;
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

pub mod bind;
pub mod ops;
pub mod preprocess;
pub mod runtime;
pub mod utilities;

pub use runtime::{
    cobol_format_decimal, cobol_format_value, cobol_pad_string, map_row_to_host_variables,
    resolve_input_with_indicators, set_output_indicators, sql_value_to_display, Cursor,
    CursorManager, CursorOptions, CursorState, Db2Connection, Db2ConnectionConfig, Db2Runtime,
    Db2Type, DescribeColumn, ExecutorMode, FetchDirection, PreparedDynamic, RuntimeHostVariable,
    RuntimeStatement, ScrollSensitivity, Sqlca, SqlcaBuilder, SqlExecutor, SqlRow, SqlTranslator,
    SqlValue, TransactionConfig, TransactionManager, TransactionState, TypeMapping,
};

pub use utilities::{
    BindAction, BindOptions, Binder, BoundPackage, ColumnInfo, Dclgen, DclgenLanguage,
    DclgenOptions, IsolationLevel, ReleaseOption, TableInfo, ValidateOption,
};

// SYS-114: BIND & Package Management re-exports
pub use bind::{
    BindError, BindPackage, BindPlan, BindResult, CatalogAction, CatalogExplain,
    CatalogIsolation, CatalogPackListEntry, CatalogPackage, CatalogPlan, CatalogValidate,
    Db2Catalog, Dbrm as BindDbrm, FreeCommand, FreeTarget, PackageBindOptions, PackageRef,
    RebindCommand, RebindTarget,
};

// SYS-115: Operational Utilities re-exports
pub use ops::{
    DataFormat, DclgenOutput, DclgenUtil, Dsntep2, Dsntep2Result, LoadUtility, OpsColumnDef,
    OpsError, OpsResult, OpsTable, OpsTableDef, UnloadUtility,
};

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
