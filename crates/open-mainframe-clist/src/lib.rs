#![forbid(unsafe_code)]
//! z/OS CLIST (Command List) scripting language interpreter.
//!
//! This crate provides:
//!
//! - **Lexer & Parser** — Source line processing, tokenization, AST generation
//! - **Interpreter Core** — Variable substitution, control flow, subprocedures
//! - **Built-in Functions** — &EVAL, &SUBSTR, &LENGTH, &SYSINDEX, &DATATYPE, etc.
//! - **I/O & Error Handling** — WRITE/READ, file I/O, ERROR/ATTN routines, CONTROL
//! - **TSO/ISPF Integration** — TSO command dispatch, ISPEXEC, ISREDIT, LISTDSI

pub mod functions;
pub mod interpreter;
pub mod io;
pub mod parser;
pub mod tso_bridge;

pub use functions::{BuiltinFunction, evaluate_builtin};
pub use interpreter::{
    ClistInterpreter, ControlVariable, ExecResult, InterpreterError, SubprocDef,
    VariablePool,
};
pub use io::{
    ControlOptions, ClistFile, FileMode, IoError, IoManager, ErrorRoutine,
    AttnRoutine,
};
pub use parser::{
    ClistAst, ClistStatement, ClistToken, ClistTokenKind, Expression, ExprOp,
    ParseError, parse_clist, tokenize_clist,
};
pub use tso_bridge::{
    DatasetAttributes, TsoEnvironment, ListdsiResult,
};
