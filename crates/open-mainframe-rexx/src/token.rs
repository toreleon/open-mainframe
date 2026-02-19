//! REXX token types.

use serde::{Deserialize, Serialize};

/// Source location for a token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Span {
    /// Line number (1-based).
    pub line: u32,
    /// Column number (1-based).
    pub col: u32,
}

/// A REXX token.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

/// REXX token types.
///
/// REXX has *no reserved words* — `SAY`, `DO`, `IF`, etc. are identified
/// contextually by the parser.  The lexer emits them all as `Symbol`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TokenKind {
    // -- Literals --
    /// Integer or decimal number (e.g., `42`, `3.14`, `1E10`).
    Number(String),
    /// Quoted string: `'hello'` or `"hello"`.
    StringLit(String),
    /// Hex string: `'FF00'x`.
    HexString(String),
    /// Binary string: `'1010'b`.
    BinString(String),

    // -- Identifiers / symbols --
    /// An identifier or keyword (uppercased).
    /// REXX has no reserved words; the parser disambiguates.
    Symbol(String),

    // -- Operators --
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `%` (integer division)
    Percent,
    /// `//` (remainder)
    SlashSlash,
    /// `**` (power)
    StarStar,
    /// `||` (concatenation)
    Concat,
    /// `\` (not — also ¬ on EBCDIC)
    Not,
    /// `=`
    Eq,
    /// `\=` or `¬=` (not equal)
    Ne,
    /// `>`
    Gt,
    /// `<`
    Lt,
    /// `>=`
    Ge,
    /// `<=`
    Le,
    /// `>>` (strictly greater)
    StrictGt,
    /// `<<` (strictly less)
    StrictLt,
    /// `>>=` (strictly greater or equal)
    StrictGe,
    /// `<<=` (strictly less or equal)
    StrictLe,
    /// `\>>` (strictly not greater)
    StrictNgt,
    /// `\<<` (strictly not less)
    StrictNlt,
    /// `&` (and)
    And,
    /// `|` (or)
    Or,
    /// `&&` (xor)
    Xor,

    // -- Delimiters --
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `,`
    Comma,
    /// `;`
    Semicolon,
    /// `:` (label terminator)
    Colon,
    /// `.` (stem delimiter in compound variables)
    Dot,

    // -- Special --
    /// End of line (implicit clause terminator).
    Eol,
    /// End of source.
    Eof,
}

impl TokenKind {
    /// Check if this is an operator token.
    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Percent
                | TokenKind::SlashSlash
                | TokenKind::StarStar
                | TokenKind::Concat
                | TokenKind::Not
                | TokenKind::Eq
                | TokenKind::Ne
                | TokenKind::Gt
                | TokenKind::Lt
                | TokenKind::Ge
                | TokenKind::Le
                | TokenKind::StrictGt
                | TokenKind::StrictLt
                | TokenKind::StrictGe
                | TokenKind::StrictLe
                | TokenKind::StrictNgt
                | TokenKind::StrictNlt
                | TokenKind::And
                | TokenKind::Or
                | TokenKind::Xor
        )
    }
}
