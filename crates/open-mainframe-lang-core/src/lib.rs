//! Shared types and traits for OpenMainframe language compilers.
//!
//! This crate provides the foundational building blocks that all language
//! compiler crates in the OpenMainframe workspace share:
//!
//! - **Source location tracking**: [`Span`], [`FileId`], [`Location`]
//! - **Diagnostics**: [`Diagnostic`], [`Severity`]
//! - **Compiler pipeline traits**: [`AstNode`], [`Lexer`], [`Parse`]
//!
//! # Design Principles
//!
//! - **Zero dependencies**: This crate has no external dependencies. It contains
//!   only plain Rust types and traits. Language crates add `miette`/`thiserror`
//!   on top for rich error rendering.
//! - **Shared, not prescriptive**: The traits define minimal contracts. Each
//!   language crate implements them according to its own needs.
//! - **Backward compatible**: Types moved here from `open-mainframe-cobol` are
//!   re-exported there for compatibility.

mod diagnostic;
mod span;
mod traits;

pub use diagnostic::{Diagnostic, Severity};
pub use span::{offset_to_line_col, FileId, Location, Span};
pub use traits::{AstNode, Lexer, Parse};
