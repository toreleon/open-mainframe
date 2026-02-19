//! REXX language support — lexer, parser, interpreter, and AST for TSO/E REXX.
//!
//! This crate provides:
//!
//! - **Lexer** — tokenizes REXX source (strings, comments, operators, symbols)
//! - **AST** — abstract syntax tree types for all REXX instructions
//! - **Parser** — builds AST from tokens with expression precedence
//! - **Interpreter** — executes REXX programs with variable pools and decimal arithmetic
//! - **Value** — REXX values with arbitrary-precision decimal arithmetic

pub mod ast;
pub mod builtins;
pub mod interpreter;
pub mod lexer;
pub mod parse_template;
pub mod parser;
pub mod token;
pub mod value;

pub use ast::{BinOp, Clause, ClauseBody, DoControl, Expr, Program, UnaryOp};
pub use interpreter::{interpret, ExecResult, InterpError};
pub use lexer::{is_rexx, lex, LexError};
pub use parser::{parse, ParseError};
pub use token::{Span, Token, TokenKind};
pub use value::{NumericForm, NumericSettings, RexxValue};
