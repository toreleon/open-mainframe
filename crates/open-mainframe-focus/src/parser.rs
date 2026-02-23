//! FOC-100: Multi-Dialect Language Parser (6 stories).
//!
//! Lexer and parser for FOCUS source text covering TABLE, GRAPH, MODIFY,
//! MAINTAIN, Dialogue Manager, and SQL passthrough dialects.

use std::fmt;
use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("unexpected token: expected {expected}, found {found}")]
    UnexpectedToken { expected: String, found: String },
    #[error("unexpected end of input")]
    UnexpectedEof,
    #[error("unknown keyword: {0}")]
    UnknownKeyword(String),
    #[error("invalid expression: {0}")]
    InvalidExpression(String),
}

// ---------------------------------------------------------------------------
// Tokens
// ---------------------------------------------------------------------------

/// Token types produced by the FOCUS lexer.
#[derive(Debug, Clone, PartialEq)]
pub enum FocusToken {
    // Keywords — TABLE dialect
    Table,
    File,
    Print,
    Sum,
    Count,
    Avg,
    Max,
    Min,
    Pct,
    First,
    Last,
    By,
    Across,
    Where,
    End,
    Define,
    Compute,
    Heading,
    Footing,
    Subfoot,
    On,

    // Keywords — GRAPH dialect
    Graph,
    Type,

    // Keywords — MODIFY / MAINTAIN dialect
    Modify,
    Maintain,
    Fixform,
    Match,
    Nomatch,
    Update,
    Include,
    Commit,
    Rollback,
    Crtform,
    Validate,

    // Keywords — Dialogue Manager
    DmSet,
    DmIf,
    DmGoto,
    DmRun,
    DmInclude,
    DmRepeat,
    DmUntil,
    DmType,
    DmRead,
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,

    // Keywords — SQL
    Sql,
    Select,

    // Literals and identifiers
    Ident(String),
    StringLit(String),
    NumberLit(f64),
    AmperVar(String),
    GlobalAmperVar(String),

    // Punctuation
    Dot,
    Comma,
    Equals,
    LParen,
    RParen,
    Slash,
    Star,
    Plus,
    Minus,
    Semicolon,

    // Special
    Eof,
}

impl fmt::Display for FocusToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FocusToken::Ident(s) => write!(f, "IDENT({s})"),
            FocusToken::StringLit(s) => write!(f, "STRING({s})"),
            FocusToken::NumberLit(n) => write!(f, "NUMBER({n})"),
            FocusToken::AmperVar(v) => write!(f, "&{v}"),
            FocusToken::GlobalAmperVar(v) => write!(f, "&&{v}"),
            FocusToken::Eof => write!(f, "EOF"),
            other => write!(f, "{other:?}"),
        }
    }
}

// ---------------------------------------------------------------------------
// Lexer
// ---------------------------------------------------------------------------

/// Tokenises FOCUS source text.
pub struct FocusLexer {
    chars: Vec<char>,
    pos: usize,
}

impl FocusLexer {
    pub fn new(src: &str) -> Self {
        Self {
            chars: src.chars().collect(),
            pos: 0,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<FocusToken>, ParseError> {
        let mut tokens = Vec::new();
        loop {
            let tok = self.next_token()?;
            if tok == FocusToken::Eof {
                tokens.push(tok);
                break;
            }
            tokens.push(tok);
        }
        Ok(tokens)
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.chars.get(self.pos).copied();
        if ch.is_some() {
            self.pos += 1;
        }
        ch
    }

    fn skip_whitespace_and_comments(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.advance();
            } else if ch == '-' && self.lookahead_is_comment() {
                // Skip comment line (-- to end of line)
                while let Some(c) = self.advance() {
                    if c == '\n' {
                        break;
                    }
                }
            } else {
                break;
            }
        }
    }

    fn lookahead_is_comment(&self) -> bool {
        // Check for "--" that is NOT a DM command (which starts with "-" then keyword)
        if self.pos + 1 < self.chars.len() && self.chars[self.pos + 1] == '-' {
            // check next non-dash char
            let after = self.pos + 2;
            if after >= self.chars.len() {
                return true;
            }
            // Actual comment: "-- text"
            return self.chars[after] == ' ' || self.chars[after] == '\n';
        }
        false
    }

    fn next_token(&mut self) -> Result<FocusToken, ParseError> {
        self.skip_whitespace_and_comments();
        let ch = match self.peek() {
            Some(c) => c,
            None => return Ok(FocusToken::Eof),
        };

        // Dialogue Manager commands start with '-'
        if ch == '-' && self.pos + 1 < self.chars.len() && self.chars[self.pos + 1].is_alphabetic()
        {
            self.advance(); // consume '-'
            let word = self.read_ident();
            return Ok(match word.to_uppercase().as_str() {
                "SET" => FocusToken::DmSet,
                "IF" => FocusToken::DmIf,
                "GOTO" => FocusToken::DmGoto,
                "RUN" => FocusToken::DmRun,
                "INCLUDE" => FocusToken::DmInclude,
                "REPEAT" => FocusToken::DmRepeat,
                "UNTIL" => FocusToken::DmUntil,
                "TYPE" => FocusToken::DmType,
                "READ" => FocusToken::DmRead,
                _ => FocusToken::Ident(format!("-{word}")),
            });
        }

        // Amper variables
        if ch == '&' {
            self.advance();
            if self.peek() == Some('&') {
                self.advance();
                let name = self.read_ident();
                return Ok(FocusToken::GlobalAmperVar(name));
            }
            let name = self.read_ident();
            return Ok(FocusToken::AmperVar(name));
        }

        // String literals
        if ch == '\'' {
            self.advance();
            let mut s = String::new();
            loop {
                match self.advance() {
                    Some('\'') => {
                        if self.peek() == Some('\'') {
                            self.advance();
                            s.push('\'');
                        } else {
                            break;
                        }
                    }
                    Some(c) => s.push(c),
                    None => return Err(ParseError::UnexpectedEof),
                }
            }
            return Ok(FocusToken::StringLit(s));
        }

        // Numbers
        if ch.is_ascii_digit() {
            return Ok(self.read_number());
        }

        // Identifiers / keywords
        if ch.is_alphabetic() || ch == '_' {
            let word = self.read_ident();
            return Ok(self.keyword_or_ident(&word));
        }

        // Single-char punctuation
        self.advance();
        Ok(match ch {
            '.' => FocusToken::Dot,
            ',' => FocusToken::Comma,
            '=' => FocusToken::Equals,
            '(' => FocusToken::LParen,
            ')' => FocusToken::RParen,
            '/' => FocusToken::Slash,
            '*' => FocusToken::Star,
            '+' => FocusToken::Plus,
            '-' => FocusToken::Minus,
            ';' => FocusToken::Semicolon,
            _ => FocusToken::Ident(ch.to_string()),
        })
    }

    fn read_ident(&mut self) -> String {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' || c == '.' {
                s.push(c);
                self.advance();
            } else {
                break;
            }
        }
        s
    }

    fn read_number(&mut self) -> FocusToken {
        let mut s = String::new();
        let mut has_dot = false;
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                s.push(c);
                self.advance();
            } else if c == '.' && !has_dot {
                has_dot = true;
                s.push(c);
                self.advance();
            } else {
                break;
            }
        }
        FocusToken::NumberLit(s.parse::<f64>().unwrap_or(0.0))
    }

    fn keyword_or_ident(&self, word: &str) -> FocusToken {
        match word.to_uppercase().as_str() {
            "TABLE" => FocusToken::Table,
            "FILE" => FocusToken::File,
            "PRINT" => FocusToken::Print,
            "SUM" => FocusToken::Sum,
            "COUNT" => FocusToken::Count,
            "AVG" => FocusToken::Avg,
            "MAX" => FocusToken::Max,
            "MIN" => FocusToken::Min,
            "PCT" => FocusToken::Pct,
            "FIRST" => FocusToken::First,
            "LAST" => FocusToken::Last,
            "BY" => FocusToken::By,
            "ACROSS" => FocusToken::Across,
            "WHERE" => FocusToken::Where,
            "END" => FocusToken::End,
            "DEFINE" => FocusToken::Define,
            "COMPUTE" => FocusToken::Compute,
            "HEADING" => FocusToken::Heading,
            "FOOTING" => FocusToken::Footing,
            "SUBFOOT" => FocusToken::Subfoot,
            "ON" => FocusToken::On,
            "GRAPH" => FocusToken::Graph,
            "TYPE" => FocusToken::Type,
            "MODIFY" => FocusToken::Modify,
            "MAINTAIN" => FocusToken::Maintain,
            "FIXFORM" => FocusToken::Fixform,
            "MATCH" => FocusToken::Match,
            "NOMATCH" => FocusToken::Nomatch,
            "UPDATE" => FocusToken::Update,
            "INCLUDE" => FocusToken::Include,
            "COMMIT" => FocusToken::Commit,
            "ROLLBACK" => FocusToken::Rollback,
            "CRTFORM" => FocusToken::Crtform,
            "VALIDATE" => FocusToken::Validate,
            "SQL" => FocusToken::Sql,
            "SELECT" => FocusToken::Select,
            "EQ" => FocusToken::Eq,
            "NE" => FocusToken::Ne,
            "GT" => FocusToken::Gt,
            "LT" => FocusToken::Lt,
            "GE" => FocusToken::Ge,
            "LE" => FocusToken::Le,
            _ => FocusToken::Ident(word.to_string()),
        }
    }
}

// ---------------------------------------------------------------------------
// AST types
// ---------------------------------------------------------------------------

/// Comparison operators in WHERE clauses.
#[derive(Debug, Clone, PartialEq)]
pub enum CompOp {
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
}

/// Expression node for WHERE clauses and computed fields.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Field(String),
    Literal(f64),
    StringLiteral(String),
    Var(String),
    BinOp {
        left: Box<Expr>,
        op: char,
        right: Box<Expr>,
    },
    FunctionCall {
        name: String,
        args: Vec<Expr>,
    },
}

/// A WHERE condition.
#[derive(Debug, Clone, PartialEq)]
pub struct WhereClause {
    pub field: String,
    pub op: CompOp,
    pub value: Expr,
}

/// Computed field definition.
#[derive(Debug, Clone, PartialEq)]
pub struct ComputedField {
    pub name: String,
    pub expr: Expr,
}

/// TABLE request AST.
#[derive(Debug, Clone, PartialEq)]
pub struct TableRequest {
    pub file: String,
    pub verb: TableVerb,
    pub fields: Vec<String>,
    pub by_dims: Vec<String>,
    pub across_dims: Vec<String>,
    pub where_clauses: Vec<WhereClause>,
    pub computes: Vec<ComputedField>,
    pub heading: Option<String>,
    pub footing: Option<String>,
    pub subfoot: Option<String>,
}

/// TABLE verb — PRINT, SUM, COUNT, AVG, MAX, MIN, etc.
#[derive(Debug, Clone, PartialEq)]
pub enum TableVerb {
    Print,
    Sum,
    Count,
    Avg,
    Max,
    Min,
    Pct,
    First,
    Last,
}

/// GRAPH request AST.
#[derive(Debug, Clone, PartialEq)]
pub struct GraphRequest {
    pub file: String,
    pub fields: Vec<String>,
    pub by_dims: Vec<String>,
    pub graph_type: String,
}

/// MODIFY request AST (batch).
#[derive(Debug, Clone, PartialEq)]
pub struct FocusModifyRequest {
    pub file: String,
    pub fixform_fields: Vec<FixformSpec>,
    pub match_key: Option<String>,
    pub on_match: Option<MatchOp>,
    pub on_nomatch: Option<MatchOp>,
}

/// FIXFORM field specification.
#[derive(Debug, Clone, PartialEq)]
pub struct FixformSpec {
    pub name: String,
    pub start: usize,
    pub length: usize,
}

/// What to do ON MATCH / ON NOMATCH.
#[derive(Debug, Clone, PartialEq)]
pub enum MatchOp {
    Update,
    Include,
    Reject,
}

/// MAINTAIN request AST (interactive).
#[derive(Debug, Clone, PartialEq)]
pub struct MaintainRequest {
    pub file: String,
    pub form_name: Option<String>,
    pub fields: Vec<String>,
}

/// Dialogue Manager command AST.
#[derive(Debug, Clone, PartialEq)]
pub enum DialogueCmd {
    Set { var: String, value: Expr },
    If { var: String, op: CompOp, value: Expr, label: String },
    Goto { label: String },
    Run { procedure: String },
    Include { file: String },
    Repeat,
    Until { var: String, op: CompOp, value: Expr },
    TypeMsg { message: String },
    Read { var: String },
}

/// SQL passthrough AST.
#[derive(Debug, Clone, PartialEq)]
pub struct SqlPassthrough {
    pub sql_text: String,
}

/// Top-level FOCUS statement.
#[derive(Debug, Clone, PartialEq)]
pub enum FocusStatement {
    Table(TableRequest),
    Graph(GraphRequest),
    Modify(FocusModifyRequest),
    Maintain(MaintainRequest),
    Dialogue(DialogueCmd),
    Sql(SqlPassthrough),
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

/// Recursive-descent parser for FOCUS multi-dialect source.
pub struct FocusParser {
    tokens: Vec<FocusToken>,
    pos: usize,
}

impl FocusParser {
    pub fn new(tokens: Vec<FocusToken>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn parse_all(&mut self) -> Result<Vec<FocusStatement>, ParseError> {
        let mut stmts = Vec::new();
        while !self.at_eof() {
            stmts.push(self.parse_statement()?);
        }
        Ok(stmts)
    }

    fn peek(&self) -> &FocusToken {
        self.tokens.get(self.pos).unwrap_or(&FocusToken::Eof)
    }

    fn advance(&mut self) -> FocusToken {
        let tok = self.tokens.get(self.pos).cloned().unwrap_or(FocusToken::Eof);
        self.pos += 1;
        tok
    }

    fn at_eof(&self) -> bool {
        matches!(self.peek(), FocusToken::Eof)
    }

    fn expect(&mut self, expected: &FocusToken) -> Result<(), ParseError> {
        let tok = self.advance();
        if std::mem::discriminant(&tok) == std::mem::discriminant(expected) {
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: format!("{expected:?}"),
                found: format!("{tok:?}"),
            })
        }
    }

    fn expect_ident(&mut self) -> Result<String, ParseError> {
        match self.advance() {
            FocusToken::Ident(s) => Ok(s),
            other => Err(ParseError::UnexpectedToken {
                expected: "identifier".into(),
                found: format!("{other:?}"),
            }),
        }
    }

    fn parse_statement(&mut self) -> Result<FocusStatement, ParseError> {
        match self.peek().clone() {
            FocusToken::Table => self.parse_table(),
            FocusToken::Graph => self.parse_graph(),
            FocusToken::Modify => self.parse_modify(),
            FocusToken::Maintain => self.parse_maintain(),
            FocusToken::Sql => self.parse_sql(),
            FocusToken::DmSet
            | FocusToken::DmIf
            | FocusToken::DmGoto
            | FocusToken::DmRun
            | FocusToken::DmInclude
            | FocusToken::DmRepeat
            | FocusToken::DmUntil
            | FocusToken::DmType
            | FocusToken::DmRead => self.parse_dialogue(),
            other => Err(ParseError::UnknownKeyword(format!("{other:?}"))),
        }
    }

    // ----- TABLE parsing -----
    fn parse_table(&mut self) -> Result<FocusStatement, ParseError> {
        self.advance(); // TABLE
        self.expect(&FocusToken::File)?;
        let file = self.expect_ident()?;

        let verb = match self.peek() {
            FocusToken::Print => { self.advance(); TableVerb::Print }
            FocusToken::Sum => { self.advance(); TableVerb::Sum }
            FocusToken::Count => { self.advance(); TableVerb::Count }
            FocusToken::Avg => { self.advance(); TableVerb::Avg }
            FocusToken::Max => { self.advance(); TableVerb::Max }
            FocusToken::Min => { self.advance(); TableVerb::Min }
            FocusToken::Pct => { self.advance(); TableVerb::Pct }
            FocusToken::First => { self.advance(); TableVerb::First }
            FocusToken::Last => { self.advance(); TableVerb::Last }
            _ => TableVerb::Print,
        };

        let mut fields = Vec::new();
        let mut by_dims = Vec::new();
        let mut across_dims = Vec::new();
        let mut where_clauses = Vec::new();
        let mut computes = Vec::new();
        let mut heading = None;
        let mut footing = None;
        let mut subfoot = None;

        loop {
            match self.peek() {
                FocusToken::End | FocusToken::Eof => {
                    if matches!(self.peek(), FocusToken::End) {
                        self.advance();
                    }
                    break;
                }
                FocusToken::By => {
                    self.advance();
                    by_dims.push(self.expect_ident()?);
                }
                FocusToken::Across => {
                    self.advance();
                    across_dims.push(self.expect_ident()?);
                }
                FocusToken::Where => {
                    self.advance();
                    where_clauses.push(self.parse_where_clause()?);
                }
                FocusToken::Compute => {
                    self.advance();
                    computes.push(self.parse_computed_field()?);
                }
                FocusToken::Heading => {
                    self.advance();
                    heading = Some(self.parse_string_value()?);
                }
                FocusToken::Footing => {
                    self.advance();
                    footing = Some(self.parse_string_value()?);
                }
                FocusToken::Subfoot => {
                    self.advance();
                    subfoot = Some(self.parse_string_value()?);
                }
                FocusToken::Ident(_) => {
                    let name = self.expect_ident()?;
                    fields.push(name);
                }
                _ => {
                    self.advance();
                }
            }
        }

        Ok(FocusStatement::Table(TableRequest {
            file,
            verb,
            fields,
            by_dims,
            across_dims,
            where_clauses,
            computes,
            heading,
            footing,
            subfoot,
        }))
    }

    // ----- GRAPH parsing -----
    fn parse_graph(&mut self) -> Result<FocusStatement, ParseError> {
        self.advance(); // GRAPH
        self.expect(&FocusToken::File)?;
        let file = self.expect_ident()?;
        self.expect(&FocusToken::Sum)?;

        let mut fields = Vec::new();
        let mut by_dims = Vec::new();
        let mut graph_type = "BAR".to_string();

        loop {
            match self.peek() {
                FocusToken::End | FocusToken::Eof => {
                    if matches!(self.peek(), FocusToken::End) {
                        self.advance();
                    }
                    break;
                }
                FocusToken::By => {
                    self.advance();
                    by_dims.push(self.expect_ident()?);
                }
                FocusToken::Type => {
                    self.advance();
                    graph_type = self.expect_ident()?;
                }
                FocusToken::Ident(_) => {
                    fields.push(self.expect_ident()?);
                }
                _ => {
                    self.advance();
                }
            }
        }

        Ok(FocusStatement::Graph(GraphRequest {
            file,
            fields,
            by_dims,
            graph_type,
        }))
    }

    // ----- MODIFY parsing -----
    fn parse_modify(&mut self) -> Result<FocusStatement, ParseError> {
        self.advance(); // MODIFY
        self.expect(&FocusToken::File)?;
        let file = self.expect_ident()?;

        let mut fixform_fields = Vec::new();
        let mut match_key = None;
        let mut on_match = None;
        let mut on_nomatch = None;

        loop {
            match self.peek() {
                FocusToken::End | FocusToken::Eof => {
                    if matches!(self.peek(), FocusToken::End) {
                        self.advance();
                    }
                    break;
                }
                FocusToken::Fixform => {
                    self.advance();
                    fixform_fields.push(self.parse_fixform_spec()?);
                }
                FocusToken::Match => {
                    self.advance();
                    match_key = Some(self.expect_ident()?);
                }
                FocusToken::On => {
                    self.advance();
                    match self.peek() {
                        FocusToken::Match => {
                            self.advance();
                            on_match = Some(self.parse_match_op()?);
                        }
                        FocusToken::Nomatch => {
                            self.advance();
                            on_nomatch = Some(self.parse_match_op()?);
                        }
                        _ => {
                            self.advance();
                        }
                    }
                }
                _ => {
                    self.advance();
                }
            }
        }

        Ok(FocusStatement::Modify(FocusModifyRequest {
            file,
            fixform_fields,
            match_key,
            on_match,
            on_nomatch,
        }))
    }

    fn parse_fixform_spec(&mut self) -> Result<FixformSpec, ParseError> {
        let name = self.expect_ident()?;
        // Expect start position and length as numbers
        let start = match self.advance() {
            FocusToken::NumberLit(n) => n as usize,
            _ => 1,
        };
        let length = match self.advance() {
            FocusToken::NumberLit(n) => n as usize,
            _ => 10,
        };
        Ok(FixformSpec {
            name,
            start,
            length,
        })
    }

    fn parse_match_op(&mut self) -> Result<MatchOp, ParseError> {
        match self.peek() {
            FocusToken::Update => {
                self.advance();
                Ok(MatchOp::Update)
            }
            FocusToken::Include => {
                self.advance();
                Ok(MatchOp::Include)
            }
            _ => Ok(MatchOp::Reject),
        }
    }

    // ----- MAINTAIN parsing -----
    fn parse_maintain(&mut self) -> Result<FocusStatement, ParseError> {
        self.advance(); // MAINTAIN
        self.expect(&FocusToken::File)?;
        let file = self.expect_ident()?;

        let mut form_name = None;
        let mut fields = Vec::new();

        loop {
            match self.peek() {
                FocusToken::End | FocusToken::Eof => {
                    if matches!(self.peek(), FocusToken::End) {
                        self.advance();
                    }
                    break;
                }
                FocusToken::Crtform => {
                    self.advance();
                    form_name = Some(self.expect_ident()?);
                }
                FocusToken::Ident(_) => {
                    fields.push(self.expect_ident()?);
                }
                _ => {
                    self.advance();
                }
            }
        }

        Ok(FocusStatement::Maintain(MaintainRequest {
            file,
            form_name,
            fields,
        }))
    }

    // ----- SQL passthrough -----
    fn parse_sql(&mut self) -> Result<FocusStatement, ParseError> {
        self.advance(); // SQL
        let mut sql_text = String::new();
        loop {
            match self.peek() {
                FocusToken::End | FocusToken::Eof => {
                    if matches!(self.peek(), FocusToken::End) {
                        self.advance();
                    }
                    break;
                }
                _ => {
                    let tok = self.advance();
                    if !sql_text.is_empty() {
                        sql_text.push(' ');
                    }
                    sql_text.push_str(&format!("{tok}"));
                }
            }
        }
        Ok(FocusStatement::Sql(SqlPassthrough { sql_text }))
    }

    // ----- Dialogue Manager parsing -----
    fn parse_dialogue(&mut self) -> Result<FocusStatement, ParseError> {
        let tok = self.advance();
        let cmd = match tok {
            FocusToken::DmSet => {
                let var_tok = self.advance();
                let var = match var_tok {
                    FocusToken::AmperVar(v) | FocusToken::GlobalAmperVar(v) => v,
                    _ => {
                        return Err(ParseError::UnexpectedToken {
                            expected: "amper variable".into(),
                            found: format!("{var_tok:?}"),
                        })
                    }
                };
                self.expect(&FocusToken::Equals)?;
                let value = self.parse_expr()?;
                DialogueCmd::Set { var, value }
            }
            FocusToken::DmIf => {
                let var_tok = self.advance();
                let var = match var_tok {
                    FocusToken::AmperVar(v) | FocusToken::GlobalAmperVar(v) => v,
                    _ => {
                        return Err(ParseError::UnexpectedToken {
                            expected: "amper variable".into(),
                            found: format!("{var_tok:?}"),
                        })
                    }
                };
                let op = self.parse_comp_op()?;
                let value = self.parse_expr()?;
                // expect GOTO label
                self.expect(&FocusToken::DmGoto)?;
                let label = self.expect_ident()?;
                DialogueCmd::If {
                    var,
                    op,
                    value,
                    label,
                }
            }
            FocusToken::DmGoto => {
                let label = self.expect_ident()?;
                DialogueCmd::Goto { label }
            }
            FocusToken::DmRun => {
                let procedure = self.expect_ident()?;
                DialogueCmd::Run { procedure }
            }
            FocusToken::DmInclude => {
                let file = self.expect_ident()?;
                DialogueCmd::Include { file }
            }
            FocusToken::DmRepeat => DialogueCmd::Repeat,
            FocusToken::DmUntil => {
                let var_tok = self.advance();
                let var = match var_tok {
                    FocusToken::AmperVar(v) | FocusToken::GlobalAmperVar(v) => v,
                    _ => {
                        return Err(ParseError::UnexpectedToken {
                            expected: "amper variable".into(),
                            found: format!("{var_tok:?}"),
                        })
                    }
                };
                let op = self.parse_comp_op()?;
                let value = self.parse_expr()?;
                DialogueCmd::Until { var, op, value }
            }
            FocusToken::DmType => {
                let msg = self.parse_string_value()?;
                DialogueCmd::TypeMsg { message: msg }
            }
            FocusToken::DmRead => {
                let var_tok = self.advance();
                let var = match var_tok {
                    FocusToken::AmperVar(v) | FocusToken::GlobalAmperVar(v) => v,
                    _ => {
                        return Err(ParseError::UnexpectedToken {
                            expected: "amper variable".into(),
                            found: format!("{var_tok:?}"),
                        })
                    }
                };
                DialogueCmd::Read { var }
            }
            _ => {
                return Err(ParseError::UnknownKeyword(format!("{tok:?}")));
            }
        };
        Ok(FocusStatement::Dialogue(cmd))
    }

    fn parse_comp_op(&mut self) -> Result<CompOp, ParseError> {
        let tok = self.advance();
        match tok {
            FocusToken::Eq => Ok(CompOp::Eq),
            FocusToken::Ne => Ok(CompOp::Ne),
            FocusToken::Gt => Ok(CompOp::Gt),
            FocusToken::Lt => Ok(CompOp::Lt),
            FocusToken::Ge => Ok(CompOp::Ge),
            FocusToken::Le => Ok(CompOp::Le),
            FocusToken::Equals => Ok(CompOp::Eq),
            _ => Err(ParseError::UnexpectedToken {
                expected: "comparison operator".into(),
                found: format!("{tok:?}"),
            }),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        let left = self.parse_primary()?;
        // Check for binary op
        match self.peek() {
            FocusToken::Plus | FocusToken::Minus | FocusToken::Star | FocusToken::Slash => {
                let op = match self.advance() {
                    FocusToken::Plus => '+',
                    FocusToken::Minus => '-',
                    FocusToken::Star => '*',
                    FocusToken::Slash => '/',
                    _ => unreachable!(),
                };
                let right = self.parse_primary()?;
                Ok(Expr::BinOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                })
            }
            _ => Ok(left),
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.peek().clone() {
            FocusToken::NumberLit(n) => {
                self.advance();
                Ok(Expr::Literal(n))
            }
            FocusToken::StringLit(s) => {
                self.advance();
                Ok(Expr::StringLiteral(s))
            }
            FocusToken::AmperVar(v) | FocusToken::GlobalAmperVar(v) => {
                self.advance();
                Ok(Expr::Var(v))
            }
            FocusToken::Ident(name) => {
                self.advance();
                if self.peek() == &FocusToken::LParen {
                    self.advance(); // (
                    let mut args = Vec::new();
                    while self.peek() != &FocusToken::RParen {
                        args.push(self.parse_expr()?);
                        if self.peek() == &FocusToken::Comma {
                            self.advance();
                        }
                    }
                    self.advance(); // )
                    Ok(Expr::FunctionCall { name, args })
                } else {
                    Ok(Expr::Field(name))
                }
            }
            FocusToken::LParen => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(&FocusToken::RParen)?;
                Ok(expr)
            }
            _ => Err(ParseError::InvalidExpression(format!(
                "unexpected {:?}",
                self.peek()
            ))),
        }
    }

    fn parse_where_clause(&mut self) -> Result<WhereClause, ParseError> {
        let field = self.expect_ident()?;
        let op = self.parse_comp_op()?;
        let value = self.parse_expr()?;
        Ok(WhereClause { field, op, value })
    }

    fn parse_computed_field(&mut self) -> Result<ComputedField, ParseError> {
        let name = self.expect_ident()?;
        self.expect(&FocusToken::Equals)?;
        let expr = self.parse_expr()?;
        Ok(ComputedField { name, expr })
    }

    fn parse_string_value(&mut self) -> Result<String, ParseError> {
        match self.advance() {
            FocusToken::StringLit(s) => Ok(s),
            FocusToken::Ident(s) => Ok(s),
            other => Err(ParseError::UnexpectedToken {
                expected: "string literal or identifier".into(),
                found: format!("{other:?}"),
            }),
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(src: &str) -> Vec<FocusToken> {
        FocusLexer::new(src).tokenize().unwrap()
    }

    fn parse(src: &str) -> Vec<FocusStatement> {
        let tokens = lex(src);
        FocusParser::new(tokens).parse_all().unwrap()
    }

    // --- Lexer tests ---

    #[test]
    fn test_lex_table_keywords() {
        let tokens = lex("TABLE FILE EMPLOYEE PRINT NAME SALARY END");
        assert!(matches!(tokens[0], FocusToken::Table));
        assert!(matches!(tokens[1], FocusToken::File));
        assert!(matches!(tokens[2], FocusToken::Ident(ref s) if s == "EMPLOYEE"));
        assert!(matches!(tokens[3], FocusToken::Print));
        assert!(matches!(tokens[6], FocusToken::End));
    }

    #[test]
    fn test_lex_number_literal() {
        let tokens = lex("42 3.14");
        assert!(matches!(tokens[0], FocusToken::NumberLit(n) if (n - 42.0).abs() < f64::EPSILON));
        assert!(matches!(tokens[1], FocusToken::NumberLit(n) if (n - 3.14).abs() < 0.001));
    }

    #[test]
    fn test_lex_string_literal() {
        let tokens = lex("'Hello World'");
        assert!(matches!(tokens[0], FocusToken::StringLit(ref s) if s == "Hello World"));
    }

    #[test]
    fn test_lex_amper_variables() {
        let tokens = lex("&VAR1 &&GLOBAL");
        assert!(matches!(tokens[0], FocusToken::AmperVar(ref v) if v == "VAR1"));
        assert!(matches!(tokens[1], FocusToken::GlobalAmperVar(ref v) if v == "GLOBAL"));
    }

    #[test]
    fn test_lex_dm_commands() {
        let tokens = lex("-SET -IF -GOTO -RUN -INCLUDE -REPEAT -UNTIL -TYPE -READ");
        assert!(matches!(tokens[0], FocusToken::DmSet));
        assert!(matches!(tokens[1], FocusToken::DmIf));
        assert!(matches!(tokens[2], FocusToken::DmGoto));
        assert!(matches!(tokens[3], FocusToken::DmRun));
        assert!(matches!(tokens[4], FocusToken::DmInclude));
        assert!(matches!(tokens[5], FocusToken::DmRepeat));
        assert!(matches!(tokens[6], FocusToken::DmUntil));
        assert!(matches!(tokens[7], FocusToken::DmType));
        assert!(matches!(tokens[8], FocusToken::DmRead));
    }

    #[test]
    fn test_lex_comparison_operators() {
        let tokens = lex("EQ NE GT LT GE LE");
        assert!(matches!(tokens[0], FocusToken::Eq));
        assert!(matches!(tokens[1], FocusToken::Ne));
        assert!(matches!(tokens[2], FocusToken::Gt));
        assert!(matches!(tokens[3], FocusToken::Lt));
        assert!(matches!(tokens[4], FocusToken::Ge));
        assert!(matches!(tokens[5], FocusToken::Le));
    }

    #[test]
    fn test_lex_punctuation() {
        let tokens = lex(".,=()/*+-;");
        assert!(matches!(tokens[0], FocusToken::Dot));
        assert!(matches!(tokens[1], FocusToken::Comma));
        assert!(matches!(tokens[2], FocusToken::Equals));
        assert!(matches!(tokens[3], FocusToken::LParen));
        assert!(matches!(tokens[4], FocusToken::RParen));
        assert!(matches!(tokens[5], FocusToken::Slash));
        assert!(matches!(tokens[6], FocusToken::Star));
        assert!(matches!(tokens[7], FocusToken::Plus));
        assert!(matches!(tokens[8], FocusToken::Minus));
        assert!(matches!(tokens[9], FocusToken::Semicolon));
    }

    // --- TABLE parser tests ---

    #[test]
    fn test_parse_simple_table_print() {
        let stmts = parse("TABLE FILE EMPLOYEE PRINT NAME SALARY END");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            FocusStatement::Table(t) => {
                assert_eq!(t.file, "EMPLOYEE");
                assert_eq!(t.verb, TableVerb::Print);
                assert_eq!(t.fields, vec!["NAME", "SALARY"]);
            }
            _ => panic!("expected TABLE"),
        }
    }

    #[test]
    fn test_parse_table_sum_by() {
        let stmts = parse("TABLE FILE SALES SUM REVENUE BY REGION END");
        match &stmts[0] {
            FocusStatement::Table(t) => {
                assert_eq!(t.verb, TableVerb::Sum);
                assert_eq!(t.fields, vec!["REVENUE"]);
                assert_eq!(t.by_dims, vec!["REGION"]);
            }
            _ => panic!("expected TABLE"),
        }
    }

    #[test]
    fn test_parse_table_across() {
        let stmts = parse("TABLE FILE SALES SUM REVENUE BY REGION ACROSS YEAR END");
        match &stmts[0] {
            FocusStatement::Table(t) => {
                assert_eq!(t.across_dims, vec!["YEAR"]);
            }
            _ => panic!("expected TABLE"),
        }
    }

    #[test]
    fn test_parse_table_where() {
        let stmts = parse("TABLE FILE EMPLOYEE PRINT NAME WHERE SALARY GT 50000 END");
        match &stmts[0] {
            FocusStatement::Table(t) => {
                assert_eq!(t.where_clauses.len(), 1);
                assert_eq!(t.where_clauses[0].field, "SALARY");
                assert_eq!(t.where_clauses[0].op, CompOp::Gt);
            }
            _ => panic!("expected TABLE"),
        }
    }

    #[test]
    fn test_parse_table_heading_footing() {
        let stmts = parse("TABLE FILE EMP PRINT NAME HEADING 'Report Title' FOOTING 'Page End' END");
        match &stmts[0] {
            FocusStatement::Table(t) => {
                assert_eq!(t.heading.as_deref(), Some("Report Title"));
                assert_eq!(t.footing.as_deref(), Some("Page End"));
            }
            _ => panic!("expected TABLE"),
        }
    }

    #[test]
    fn test_parse_table_compute() {
        let stmts = parse("TABLE FILE EMP PRINT NAME COMPUTE BONUS = SALARY * 10 END");
        match &stmts[0] {
            FocusStatement::Table(t) => {
                assert_eq!(t.computes.len(), 1);
                assert_eq!(t.computes[0].name, "BONUS");
            }
            _ => panic!("expected TABLE"),
        }
    }

    // --- GRAPH parser tests ---

    #[test]
    fn test_parse_graph() {
        let stmts = parse("GRAPH FILE SALES SUM REVENUE BY REGION TYPE BAR END");
        match &stmts[0] {
            FocusStatement::Graph(g) => {
                assert_eq!(g.file, "SALES");
                assert_eq!(g.fields, vec!["REVENUE"]);
                assert_eq!(g.by_dims, vec!["REGION"]);
                assert_eq!(g.graph_type, "BAR");
            }
            _ => panic!("expected GRAPH"),
        }
    }

    #[test]
    fn test_parse_graph_pie() {
        let stmts = parse("GRAPH FILE BUDGET SUM AMOUNT BY DEPT TYPE PIE END");
        match &stmts[0] {
            FocusStatement::Graph(g) => {
                assert_eq!(g.graph_type, "PIE");
            }
            _ => panic!("expected GRAPH"),
        }
    }

    // --- MODIFY parser tests ---

    #[test]
    fn test_parse_modify() {
        let stmts = parse("MODIFY FILE EMPLOYEE FIXFORM EMPID 1 5 MATCH EMPID ON MATCH UPDATE ON NOMATCH INCLUDE END");
        match &stmts[0] {
            FocusStatement::Modify(m) => {
                assert_eq!(m.file, "EMPLOYEE");
                assert_eq!(m.fixform_fields.len(), 1);
                assert_eq!(m.fixform_fields[0].name, "EMPID");
                assert_eq!(m.fixform_fields[0].start, 1);
                assert_eq!(m.fixform_fields[0].length, 5);
                assert_eq!(m.match_key, Some("EMPID".to_string()));
                assert_eq!(m.on_match, Some(MatchOp::Update));
                assert_eq!(m.on_nomatch, Some(MatchOp::Include));
            }
            _ => panic!("expected MODIFY"),
        }
    }

    // --- MAINTAIN parser tests ---

    #[test]
    fn test_parse_maintain() {
        let stmts = parse("MAINTAIN FILE EMPLOYEE CRTFORM EMPFORM NAME SALARY END");
        match &stmts[0] {
            FocusStatement::Maintain(m) => {
                assert_eq!(m.file, "EMPLOYEE");
                assert_eq!(m.form_name, Some("EMPFORM".to_string()));
                assert_eq!(m.fields, vec!["NAME", "SALARY"]);
            }
            _ => panic!("expected MAINTAIN"),
        }
    }

    // --- SQL parser tests ---

    #[test]
    fn test_parse_sql() {
        let stmts = parse("SQL SELECT NAME SALARY END");
        match &stmts[0] {
            FocusStatement::Sql(s) => {
                assert!(s.sql_text.contains("Select"));
                assert!(s.sql_text.contains("NAME"));
            }
            _ => panic!("expected SQL"),
        }
    }

    // --- Dialogue Manager parser tests ---

    #[test]
    fn test_parse_dm_set() {
        let stmts = parse("-SET &COUNT = 10");
        match &stmts[0] {
            FocusStatement::Dialogue(DialogueCmd::Set { var, value }) => {
                assert_eq!(var, "COUNT");
                assert_eq!(*value, Expr::Literal(10.0));
            }
            _ => panic!("expected DM SET"),
        }
    }

    #[test]
    fn test_parse_dm_if_goto() {
        let stmts = parse("-IF &X EQ 5 -GOTO DONE");
        match &stmts[0] {
            FocusStatement::Dialogue(DialogueCmd::If {
                var,
                op,
                value,
                label,
            }) => {
                assert_eq!(var, "X");
                assert_eq!(*op, CompOp::Eq);
                assert_eq!(*value, Expr::Literal(5.0));
                assert_eq!(label, "DONE");
            }
            _ => panic!("expected DM IF"),
        }
    }

    #[test]
    fn test_parse_dm_goto() {
        let stmts = parse("-GOTO LOOP_START");
        match &stmts[0] {
            FocusStatement::Dialogue(DialogueCmd::Goto { label }) => {
                assert_eq!(label, "LOOP_START");
            }
            _ => panic!("expected DM GOTO"),
        }
    }

    #[test]
    fn test_parse_dm_run() {
        let stmts = parse("-RUN report.fex");
        match &stmts[0] {
            FocusStatement::Dialogue(DialogueCmd::Run { procedure }) => {
                assert_eq!(procedure, "report.fex");
            }
            _ => panic!("expected DM RUN"),
        }
    }

    #[test]
    fn test_parse_dm_include() {
        let stmts = parse("-INCLUDE common.fex");
        match &stmts[0] {
            FocusStatement::Dialogue(DialogueCmd::Include { file }) => {
                assert_eq!(file, "common.fex");
            }
            _ => panic!("expected DM INCLUDE"),
        }
    }

    #[test]
    fn test_parse_dm_type() {
        let stmts = parse("-TYPE 'Hello User'");
        match &stmts[0] {
            FocusStatement::Dialogue(DialogueCmd::TypeMsg { message }) => {
                assert_eq!(message, "Hello User");
            }
            _ => panic!("expected DM TYPE"),
        }
    }

    #[test]
    fn test_parse_dm_read() {
        let stmts = parse("-READ &INPUT");
        match &stmts[0] {
            FocusStatement::Dialogue(DialogueCmd::Read { var }) => {
                assert_eq!(var, "INPUT");
            }
            _ => panic!("expected DM READ"),
        }
    }

    #[test]
    fn test_parse_dm_repeat_until() {
        let stmts = parse("-REPEAT\n-UNTIL &COUNTER GE 10");
        assert_eq!(stmts.len(), 2);
        assert!(matches!(stmts[0], FocusStatement::Dialogue(DialogueCmd::Repeat)));
        match &stmts[1] {
            FocusStatement::Dialogue(DialogueCmd::Until { var, op, .. }) => {
                assert_eq!(var, "COUNTER");
                assert_eq!(*op, CompOp::Ge);
            }
            _ => panic!("expected DM UNTIL"),
        }
    }

    // --- Expression tests ---

    #[test]
    fn test_parse_expression_binop() {
        let stmts = parse("TABLE FILE EMP PRINT NAME COMPUTE TAX = SALARY * 0.3 END");
        match &stmts[0] {
            FocusStatement::Table(t) => {
                assert_eq!(t.computes[0].name, "TAX");
                match &t.computes[0].expr {
                    Expr::BinOp { op, .. } => assert_eq!(*op, '*'),
                    _ => panic!("expected BinOp"),
                }
            }
            _ => panic!("expected TABLE"),
        }
    }

    #[test]
    fn test_parse_multiple_statements() {
        let stmts = parse("-SET &X = 1\nTABLE FILE EMP PRINT NAME END");
        assert_eq!(stmts.len(), 2);
        assert!(matches!(stmts[0], FocusStatement::Dialogue(_)));
        assert!(matches!(stmts[1], FocusStatement::Table(_)));
    }
}
