#![forbid(unsafe_code)]
//! Software AG Natural 4GL — parser, interpreter, ADABAS/SQL database access,
//! reporting, maps, and security.
//!
//! This crate provides:
//!
//! - **Parser** (NAT-100) — lexer and parser for Natural source text
//! - **Data Model** (NAT-101) — 11 data types, DEFINE DATA, arrays, groups
//! - **Interpreter** (NAT-102) — execution engine with control flow and call stack
//! - **Manipulation** (NAT-103) — COMPUTE, MOVE, COMPRESS, SEPARATE, EXAMINE, SORT
//! - **ADABAS Access** (NAT-104) — DDM-based database operations (simulated)
//! - **SQL Access** (NAT-105) — SELECT, INSERT, UPDATE, DELETE (simulated)
//! - **Output** (NAT-106) — DISPLAY, WRITE, PRINT, control breaks, page formatting
//! - **Maps** (NAT-107) — Interactive I/O, INPUT USING MAP, PF key handling
//! - **Functions** (NAT-108) — 25+ built-in functions
//! - **System Variables** (NAT-108) — 70+ system variables
//! - **Work Files** (NAT-109) — work file I/O, ON ERROR, STACK INPUT
//! - **Environment** (NAT-110) — library management, security, EntireX RPC

pub mod parser;
pub mod data_model;
pub mod interpreter;
pub mod manipulation;
pub mod adabas_access;
pub mod sql_access;
pub mod output;
pub mod maps;
pub mod functions;
pub mod sysvars;
pub mod workfiles;
pub mod environment;

// Re-exports for convenience
pub use parser::{parse_natural, Lexer, Parser, Program, Statement, Token};
pub use data_model::{NaturalType, NaturalValue, TypeSpec, Variable, VariablePool};
pub use interpreter::{NaturalInterpreter, NaturalObject, ObjectType};
pub use manipulation::{compress, examine, separate, sort_records, SortDirection};
pub use adabas_access::{AdabasFile, AdabasRecord, Ddm};
pub use sql_access::{InMemorySql, SqlConnection, SqlCondition};
pub use output::{Alignment, ColumnDef, ControlBreak, ReportEngine};
pub use maps::{MapDefinition, MapField, PfKey, TerminalSimulator};
pub use functions::eval_builtin;
pub use sysvars::{all_system_variables, get_system_variable};
pub use workfiles::{WorkFile, WorkFileManager, ErrorHandler};
pub use environment::{
    EntireXBroker, LibraryManager, NaturalLibrary, NaturalSecurity,
    SecurityProfile, StoredObject,
};
