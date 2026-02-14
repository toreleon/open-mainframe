//! Token types for JCL lexical analysis.

/// Token types for JCL.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    /// JOB statement.
    Job,
    /// EXEC statement.
    Exec,
    /// DD statement.
    Dd,
    /// SET statement.
    Set,
    /// IF statement.
    If,
    /// THEN keyword.
    Then,
    /// ELSE keyword.
    Else,
    /// ENDIF statement.
    Endif,
    /// INCLUDE statement.
    Include,
    /// JCLLIB statement.
    Jcllib,
    /// PROC statement.
    Proc,
    /// PEND statement.
    Pend,
    /// OUTPUT statement.
    Output,
    /// NULL statement (just //).
    Null,

    /// Identifier (name, keyword).
    Ident(String),
    /// String literal (in quotes).
    String(String),
    /// Numeric literal.
    Number(i64),

    /// = sign.
    Equals,
    /// , comma.
    Comma,
    /// ( open paren.
    LParen,
    /// ) close paren.
    RParen,
    /// . period.
    Period,
    /// * asterisk.
    Asterisk,
    /// & ampersand (symbolic parameter).
    Ampersand,

    /// Start of inline data.
    InlineDataStart,
    /// Inline data line.
    InlineData(String),
    /// End of inline data (/*).
    InlineDataEnd,

    /// Comment (//*, or /* ... */).
    Comment(String),

    /// End of statement.
    EndStatement,
    /// End of JCL.
    Eof,
}

/// JCL statement as parsed from source.
#[derive(Debug, Clone)]
pub struct JclStatement {
    /// The name field (columns 3-10 of first line).
    pub name: Option<String>,
    /// The operation (JOB, EXEC, DD, etc.).
    pub operation: String,
    /// The operands as a string (to be parsed further).
    pub operands: String,
    /// Line number in source.
    pub line: u32,
    /// Byte offset of the start of this statement in the source.
    pub byte_offset: u32,
    /// Byte offset of the end of this statement in the source (exclusive).
    pub byte_end: u32,
}
