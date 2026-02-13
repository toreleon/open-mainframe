//! COBOL lexical analysis (tokenization).
//!
//! This module provides the lexer for COBOL source code. It handles:
//! - Fixed and free format source files
//! - EBCDIC and ASCII encoding
//! - Copybook (COPY statement) resolution
//! - All COBOL token types
//!
//! # Example
//!
//! ```
//! use zos_cobol::lexer::{SourceFile, SourceFormat, FileId, scan};
//!
//! let source = SourceFile::from_text(
//!     FileId::MAIN,
//!     "IDENTIFICATION DIVISION.".to_string(),
//!     SourceFormat::Free,
//! );
//! let (tokens, errors) = scan(&source);
//! assert!(errors.is_empty());
//! ```

pub mod copybook;
pub mod keywords;
pub mod preprocessor;
pub mod scanner;
pub mod source;
pub mod span;
pub mod token;

// Re-exports
pub use copybook::{
    apply_replacements, parse_replacing_clause, CopybookConfig, CopybookResolver, Replacement,
};
pub use preprocessor::Preprocessor;
pub use keywords::{is_keyword, lookup_keyword};
pub use scanner::scan;
pub use source::{Indicator, SourceFile, SourceFormat, SourceLine, SourceManager};
pub use span::{offset_to_line_col, FileId, Location, Span};
pub use token::{Keyword, Token, TokenKind};
