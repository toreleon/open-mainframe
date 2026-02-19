//! Enterprise PL/I — lexer, parser, type system, interpreter, and built-in functions.
//!
//! This crate implements a PL/I compiler front-end for the OpenMainframe project.
//! PL/I is notable for having **no reserved words** — any keyword can also be
//! used as an identifier, requiring context-sensitive parsing.

pub mod lexer;
pub mod parser;

pub use lexer::{is_keyword, string_value, Lexer, LexerError, Token, TokenKind};
pub use parser::{
    AllocateStmt, AssignmentStmt, BeginBlock, BinOp, CallStmt, CloseStmt, DataType, DeclareItem,
    DeclareStmt, Dimension, DisplayStmt, DoControl, DoStmt, Expr, FreeStmt, GetStmt, GoToStmt,
    IfStmt, IoMode, IterateStmt, LeaveStmt, OnCondition, OnStmt, OpenStmt, ParseError, Parser,
    PreprocessorDirective, ProcedureStmt, Program, PutStmt, ReadStmt, ReturnStmt, RevertStmt,
    SelectStmt, SignalStmt, Statement, StorageClass, UnaryOp, WhenClause, WriteStmt,
};
