//! HLASM — High Level Assembler.
//!
//! This crate provides:
//!
//! - **Lexer** — HLASM fixed-format source parser and operand tokenizer
//! - Column-based parsing (label, opcode, operands, remarks)
//! - Continuation handling (column 72)
//! - Operand tokenization (registers, symbols, literals, self-defining terms)
//! - **Symbol Table & Expression Evaluator** — two-pass symbol resolution and expression evaluation
//! - Attribute references: L' (length), T' (type), S' (scale), I' (integer), D' (defined)

pub mod lexer;
pub mod symbol;

pub use lexer::{
    parse_source, parse_source_line, tokenize_operands, InstructionLine, LexerError, SourceLine,
    Token,
};
pub use symbol::{eval_expr_str, eval_expression, ExprError, Symbol, SymbolTable};
