//! JCL (Job Control Language) interpreter for OpenMainframe.
//!
//! This crate provides parsing and execution of JCL, the language
//! used to control batch job execution on IBM mainframes.
//!
//! # Example
//!
//! ```ignore
//! use open_mainframe_jcl::{parse, JobExecutor};
//!
//! let jcl = r#"
//! //MYJOB    JOB (ACCT),'PROGRAMMER',CLASS=A
//! //STEP1    EXEC PGM=HELLO
//! //SYSOUT   DD SYSOUT=*
//! //"#;
//!
//! let job = parse(jcl).unwrap();
//! let mut executor = JobExecutor::new();
//! let result = executor.execute(&job).unwrap();
//! ```

pub mod ast;
pub mod error;
pub mod executor;
pub mod lexer;
pub mod parser;
pub mod procedure;

pub use ast::*;
pub use error::{JclError, SourceLocation};
pub use executor::{run, run_with_config, ExecutionConfig, JobExecutor, JobResult, StepResult};
// Standard IBM utilities are provided by the `open-mainframe-utilities` crate.
pub use open_mainframe_utilities;
pub use lexer::{tokenize_operands, JclStatement, Lexer, Token};
pub use open_mainframe_lang_core::{AstNode, Span};
pub use parser::{parse, Parser};
pub use procedure::{
    FilesystemProcLib, InMemoryProcLib, ProcedureExpander, ProcedureLibrary,
};
