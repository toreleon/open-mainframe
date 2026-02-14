//! Shared traits for language compiler pipelines.
//!
//! These traits define the common interface that all language crates implement.
//! They enable generic tooling (assessment, LSP, etc.) to operate across
//! different languages without knowing language-specific details.

use crate::span::Span;

/// Trait for AST nodes that carry source location information.
///
/// Every AST root node should implement this trait so that generic tooling
/// can access the source span of any language's AST.
///
/// # Example
///
/// ```
/// use open_mainframe_lang_core::{AstNode, Span};
///
/// struct MyProgram {
///     name: String,
///     span: Span,
/// }
///
/// impl AstNode for MyProgram {
///     fn span(&self) -> Span {
///         self.span
///     }
/// }
/// ```
pub trait AstNode {
    /// Returns the source span covering this AST node.
    fn span(&self) -> Span;
}

/// Common interface for language-specific lexers.
///
/// Implementations tokenize source text into a language-specific token stream.
/// The lexer should recover from errors where possible, returning both
/// valid tokens and any errors encountered.
pub trait Lexer {
    /// The token type produced by this lexer.
    type Token;
    /// The error type produced by this lexer.
    type Error;

    /// Tokenize the given source text.
    ///
    /// Returns a tuple of `(tokens, errors)`. The lexer should attempt to
    /// recover from errors and produce as many valid tokens as possible.
    fn tokenize(&mut self, source: &str) -> (Vec<Self::Token>, Vec<Self::Error>);
}

/// Common interface for language-specific parsers.
///
/// Implementations consume a token stream and produce an AST.
/// The parser should recover from errors where possible, returning both
/// a (potentially partial) AST and any errors encountered.
pub trait Parse {
    /// The AST root type produced by this parser.
    type Ast: AstNode;
    /// The error type produced by this parser.
    type Error;

    /// Parse the token stream into an AST.
    ///
    /// Returns `(Option<Ast>, Vec<Error>)`. The AST is `None` only if
    /// parsing fails catastrophically. Otherwise, a partial AST is returned
    /// alongside accumulated errors.
    fn parse(&mut self) -> (Option<Self::Ast>, Vec<Self::Error>);
}
