//! COBOL compiler for OpenMainframe.
//!
//! This crate provides the COBOL compiler, including:
//! - Lexer: Tokenizes COBOL source code
//! - Parser: Builds an Abstract Syntax Tree (AST)
//! - Semantic Analyzer: Type checking and symbol resolution
//! - Code Generator: LLVM IR generation
//!
//! # Architecture
//!
//! The compiler follows a traditional pipeline:
//! 1. Source files are loaded and preprocessed (copybook resolution)
//! 2. The lexer produces a token stream
//! 3. The parser builds an AST
//! 4. Semantic analysis validates the AST
//! 5. Code generation produces LLVM IR
//! 6. LLVM compiles to native code

// Macro definitions must come first so they're visible to all subsequent modules.
#[macro_use]
mod macros;

pub mod ast;
#[cfg(feature = "llvm")]
pub mod codegen;
pub mod error;
pub mod intrinsics;
pub mod lexer;
pub mod parser;
pub mod semantic;
pub mod xml_json;

pub use ast::*;
pub use error::CobolError;
pub use lexer::{
    apply_replacements, parse_cbl_process, parse_replacing_clause, scan, ArithMode,
    CompilerOptionError, CompilerOptions, ConditionalProcessor, CopybookConfig, CopybookResolver,
    FileId, Indicator, IntDate, Keyword, Location, NSymbol, NumProc, Preprocessor,
    ReplaceProcessor, Replacement, SourceFile, SourceFormat, SourceFormatChange, SourceFormatKind,
    SourceLine, SourceManager, Span, Token, TokenKind, TruncMode, max_precision_digits,
    should_truncate_to_pic,
};
pub use parser::Parser;
pub use semantic::{
    analyze, CobolType, Diagnostic, SemanticAnalyzer, SemanticResult, Severity, Symbol, SymbolKind,
    SymbolTable, TypeCategory,
};
pub use xml_json::{
    json_generate, json_generate_value, json_parse, xml_generate, CobolField, FieldType,
    JsonGenerateOptions, JsonParser, JsonValue, XmlEvent, XmlGenerateOptions, XmlJsonError,
    XmlJsonResult, XmlParser,
};
