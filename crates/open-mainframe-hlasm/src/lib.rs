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
//! - **Machine Instruction Encoding** — z/Architecture instruction formats and encoding
//! - 200+ instructions across RR, RX, RXY, RS, RSY, RI, RIL, SI, SIY, SIL, SS, S, E formats
//! - Extended branch mnemonics (B, BE, BNE, J, JE, etc.)
//! - **Macro Language** — MACRO/MEND, parameter substitution, &SYSNDX, MNOTE, MEXIT, COPY
//! - **Object Code Generation** — OBJ format (ESD/TXT/RLD/END), GOFF basics, AMODE/RMODE

pub mod instruction;
pub mod lexer;
pub mod macros;
pub mod object;
pub mod symbol;

pub use instruction::{
    encode_instruction, EncodeError, InsnCatalog, InsnDef, InsnFormat, InsnOperands,
};
pub use lexer::{
    parse_source, parse_source_line, tokenize_operands, InstructionLine, LexerError, SourceLine,
    Token,
};
pub use macros::{CopyLibrary, MacroDef, MacroEngine, MacroError, MacroParam, Mnote, SystemVars};
pub use object::{Amode, EsdItem, EsdType, ObjectModule, Rmode, RldEntry, TxtRecord};
pub use symbol::{eval_expr_str, eval_expression, ExprError, Symbol, SymbolTable};
