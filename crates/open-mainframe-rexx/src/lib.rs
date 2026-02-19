//! REXX language support — lexer, parser, and AST for TSO/E REXX.
//!
//! This crate provides:
//!
//! - **Lexer** — tokenizes REXX source (strings, comments, operators, symbols)
//! - **AST** — abstract syntax tree types for all REXX instructions
//! - **Parser** — builds AST from tokens with expression precedence

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;

pub use ast::{BinOp, Clause, ClauseBody, DoControl, Expr, Program, UnaryOp};
pub use lexer::{is_rexx, lex, LexError};
pub use parser::{parse, ParseError};
pub use token::{Span, Token, TokenKind};
