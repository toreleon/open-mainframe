//! HLASM — High Level Assembler.
//!
//! This crate provides:
//!
//! - **Lexer** — HLASM fixed-format source parser and operand tokenizer
//! - Column-based parsing (label, opcode, operands, remarks)
//! - Continuation handling (column 72)
//! - Operand tokenization (registers, symbols, literals, self-defining terms)

pub mod lexer;

pub use lexer::{
    parse_source, parse_source_line, tokenize_operands, InstructionLine, LexerError, SourceLine,
    Token,
};
