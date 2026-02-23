#![forbid(unsafe_code)]
//! Easytrieve Plus language support for z/OS report generation.
//!
//! This crate provides:
//!
//! - **Parser** (EZ-100) — lexer and parser for Easytrieve fixed-column source
//! - **Interpreter** (EZ-101) — execution engine for Easytrieve programs
//! - **File I/O** (EZ-102) — file definitions, record layouts, GET/PUT processing
//! - **Report** (EZ-103) — report generator with headings, detail lines, control breaks
//! - **SQL** (EZ-104) — embedded SQL support with pluggable bridge
//! - **Sort** (EZ-105) — SORT, MATCH, and control break utilities
//! - **Macros** (EZ-106) — MACRO/END-MACRO, COPY, and external CALL support

pub mod fileio;
pub mod interpreter;
pub mod macros;
pub mod parser;
pub mod report;
pub mod sort;
pub mod sql;

pub use fileio::{EzFieldDef, EzFile, EzRecord, FileProcessor};
pub use interpreter::{EzInterpreter, EzValue, EzVariable};
pub use macros::{EzCopy, EzExternalCall, EzMacro};
pub use parser::{EzParser, EzProgram, EzStatement, EzToken};
pub use report::{ControlBreak, PageControl, ReportDef, ReportFormatter, SummaryLine};
pub use sort::{EzControlBreak, EzMatch, EzSort};
pub use sql::{EzSqlBlock, EzSqlResult, MockSqlBridge, SqlBridge};
