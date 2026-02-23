#![forbid(unsafe_code)]
//! z/OS COBOL Precompilers — DB2 and CICS source-to-source transformation.
//!
//! This crate provides:
//!
//! - **DB2 Precompiler** — transforms `EXEC SQL ... END-EXEC` into `CALL 'DSNHLI'`
//! - **CICS Precompiler** — transforms `EXEC CICS ... END-EXEC` into `CALL 'DFHEI1'`
//! - **Integrated mode** — combined DB2+CICS precompilation in a single pass

pub mod cics;
pub mod db2;

pub use cics::{
    BmsField, BmsSymbolicMap, CicsCommand, CicsOption, CicsPrecompileError,
    CicsPrecompileResult, CicsTransformedCall, ExecCicsBlock, PrecompileMode,
};
pub use db2::{
    Db2PrecompileError, Db2PrecompileResult, Dbrm, DbrmHostVar, DbrmStatement,
    ExecSqlBlock, HostVariable, SqlStatementType, SqlType, TransformedCall,
};
