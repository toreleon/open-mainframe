#![forbid(unsafe_code)]
//! Information Builders FOCUS — multi-dialect 4GL implementation.
//!
//! This crate provides:
//!
//! - **Parser** (FOC-100) — multi-dialect language parser for TABLE, GRAPH, MODIFY, Dialogue Manager, SQL
//! - **MFD** (FOC-101) — Master File Descriptor parser and metadata
//! - **Table Engine** (FOC-102) — TABLE request engine with PRINT, SUM, BY, ACROSS
//! - **Graph Engine** (FOC-103) — GRAPH engine producing text-mode charts
//! - **Modify Engine** (FOC-104) — MODIFY/MAINTAIN batch and interactive processing
//! - **Dialogue** (FOC-105) — Dialogue Manager with amper variables and control flow
//! - **Functions** (FOC-106) — built-in function library (character, date, numeric, conversion)
//! - **Adapters** (FOC-107) — data adapters for FOCUS native, sequential, VSAM, DB2, IMS
//! - **Output** (FOC-108) — output formatting (text, HTML, HOLD file)
//! - **Joins** (FOC-109) — joins and multi-source operations (JOIN, COMBINE, MATCH FILE)
//! - **Filedef** (FOC-110) — mainframe integration (FILEDEF, DYNAM, TSO/CICS interfaces)

pub mod adapters;
pub mod dialogue;
pub mod filedef;
pub mod functions;
pub mod graph_engine;
pub mod joins;
pub mod mfd;
pub mod modify_engine;
pub mod output;
pub mod parser;
pub mod table_engine;

pub use adapters::{
    AdapterRegistry, DataAdapter, Db2Adapter, FocusNativeAdapter, ImsAdapter, SequentialAdapter,
    VsamAdapter,
};
pub use dialogue::{AmperVariable, DialogueInterpreter};
pub use filedef::{CicsInterface, DynamAllocation, FileDefEntry, FileDefRegistry, TsoInterface};
pub use functions::FunctionRegistry;
pub use graph_engine::{ChartFormat, ChartType, GraphEngine, GraphOutput};
pub use joins::{CombineOp, JoinDefinition, JoinEngine, JoinType, MatchFileOp};
pub use mfd::{AccessFile, FieldDef, FocusDataType, MfdParser, Segment};
pub use modify_engine::{
    FixformField, MatchAction, ModifyEngine, ModifyRequest, TransactionLog, ValidationRule,
};
pub use output::{HoldFormatter, HtmlFormatter, OutputFormatter, TextFormatter};
pub use parser::{
    DialogueCmd, FocusLexer, FocusToken, GraphRequest, MaintainRequest, SqlPassthrough,
    TableRequest,
};
pub use table_engine::{ReportOutput, ReportRow, TableEngine};
