// SPDX-License-Identifier: Apache-2.0
//! NAT-100: Lexer & Parser for Software AG Natural 4GL.
//!
//! Provides tokenization and AST construction for Natural programs,
//! supporting 130+ keywords, expression parsing with operator precedence,
//! and all major statement categories.

use std::fmt;

// ---------------------------------------------------------------------------
// Tokens
// ---------------------------------------------------------------------------

/// All Natural keywords and token types.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals & identifiers
    Identifier(String),
    StringLiteral(String),
    IntegerLiteral(i64),
    DecimalLiteral(String),
    HashVariable(String), // #VAR
    SystemVariable(String), // *DATX etc.

    // Punctuation / operators
    LeftParen,
    RightParen,
    Equals,
    NotEquals,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Plus,
    Minus,
    Star,
    Slash,
    Comma,
    Period,
    Colon,
    Apostrophe,

    // Control flow keywords
    If,
    Then,
    Else,
    EndIf,
    Decide,
    For,
    On,
    Every,
    First,
    Value,
    Values,
    When,
    None,
    Any,
    All,
    EndDecide,
    Repeat,
    Until,
    While,
    EndRepeat,
    EndFor,
    Escape,
    Top,
    Bottom,
    Routine,
    Perform,
    EndPerform,

    // Data definition
    Define,
    Data,
    Local,
    Global,
    Parameter,
    Using,
    EndDefine,
    Init,
    Const,
    Redefine,

    // Database access (ADABAS)
    Read,
    By,
    StartingFrom,
    Ending,
    EndRead,
    Find,
    With,
    Sorted,
    Where,
    EndFind,
    Histogram,
    EndHistogram,
    Get,
    Store,
    Update,
    Delete,
    End,
    Transaction,
    Backout,
    InFileNumber,

    // I/O
    Display,
    Write,
    Print,
    Input,
    Reinput,
    Mark,
    Map,
    Newpage,

    // Data manipulation
    Compute,
    Move,
    Edited,
    ByName,
    Compress,
    Into,
    Leaving,
    Space,
    Separate,
    Examine,
    Replace,
    Full,
    Giving,
    Number,
    Position,
    Length,
    Sort,

    // Program flow
    Callnat,
    Fetch,
    Return,
    Stack,
    OnError,
    EndError,

    // Logical
    And,
    Or,
    Not,

    // Report
    At,
    Break,
    Of,
    Page,

    // SQL
    Select,
    Insert,
    Commit,
    Rollback,

    // Work file
    Work,
    File,

    // Built-in functions (used as identifiers mostly)
    Substr,
    Abs,
    Sign,
    Frac,
    Int,
    Mod,
    Sqrt,
    Val,
    Trim,
    Upper,
    Lower,
    Translate,
    IsFunc,

    // Environment
    Logon,
    Catalog,
    Stow,

    // Additional keywords
    Add,
    Subtract,
    Multiply,
    Divide,
    Reset,
    Stop,
    Terminate,
    Include,
    MoveAll,
    Limit,
    Accept,
    Reject,
    Set,
    Key,
    Window,
    Run,
    Call,

    // OO keywords
    Class,
    Method,
    Property,
    Create,
    Object,
    Send,
    Interface,
    EndClass,
    EndMethod,
    EndInterface,

    // Special
    True,
    False,
    To,
    From,
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Identifier(s) => write!(f, "{s}"),
            Token::StringLiteral(s) => write!(f, "'{s}'"),
            Token::IntegerLiteral(n) => write!(f, "{n}"),
            Token::DecimalLiteral(s) => write!(f, "{s}"),
            Token::HashVariable(s) => write!(f, "#{s}"),
            Token::SystemVariable(s) => write!(f, "*{s}"),
            _ => write!(f, "{self:?}"),
        }
    }
}

// ---------------------------------------------------------------------------
// Lexer
// ---------------------------------------------------------------------------

/// Lexer error.
#[derive(Debug, Clone, thiserror::Error)]
pub enum LexError {
    #[error("unexpected character '{0}' at position {1}")]
    UnexpectedChar(char, usize),
    #[error("unterminated string literal at position {0}")]
    UnterminatedString(usize),
}

/// Tokenize Natural source text into a list of tokens.
pub struct Lexer<'a> {
    input: &'a str,
    chars: Vec<char>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            chars: input.chars().collect(),
            pos: 0,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        while self.pos < self.chars.len() {
            self.skip_whitespace();
            if self.pos >= self.chars.len() {
                break;
            }
            let ch = self.chars[self.pos];
            match ch {
                '(' => { tokens.push(Token::LeftParen); self.pos += 1; }
                ')' => { tokens.push(Token::RightParen); self.pos += 1; }
                '+' => { tokens.push(Token::Plus); self.pos += 1; }
                '-' => {
                    // Check if this is a negative number literal
                    if self.pos + 1 < self.chars.len() && self.chars[self.pos + 1].is_ascii_digit() {
                        // Only treat as negative number if previous token is an operator or start
                        let is_unary = tokens.is_empty() || matches!(
                            tokens.last(),
                            Some(Token::LeftParen | Token::Equals | Token::NotEquals
                                | Token::LessThan | Token::LessEqual | Token::GreaterThan
                                | Token::GreaterEqual | Token::Plus | Token::Minus
                                | Token::Star | Token::Slash | Token::Comma)
                        );
                        if is_unary {
                            self.pos += 1;
                            let num_tok = self.read_number()?;
                            match num_tok {
                                Token::IntegerLiteral(n) => tokens.push(Token::IntegerLiteral(-n)),
                                Token::DecimalLiteral(s) => tokens.push(Token::DecimalLiteral(format!("-{s}"))),
                                _ => unreachable!(),
                            }
                        } else {
                            tokens.push(Token::Minus);
                            self.pos += 1;
                        }
                    } else {
                        tokens.push(Token::Minus);
                        self.pos += 1;
                    }
                }
                '/' => {
                    if self.pos + 1 < self.chars.len() && self.chars[self.pos + 1] == '*' {
                        // Comment — skip to end of line
                        while self.pos < self.chars.len() && self.chars[self.pos] != '\n' {
                            self.pos += 1;
                        }
                    } else {
                        tokens.push(Token::Slash);
                        self.pos += 1;
                    }
                }
                ',' => { tokens.push(Token::Comma); self.pos += 1; }
                '.' => { tokens.push(Token::Period); self.pos += 1; }
                ':' => { tokens.push(Token::Colon); self.pos += 1; }
                '=' => { tokens.push(Token::Equals); self.pos += 1; }
                '<' => {
                    if self.peek_next() == Some('>') {
                        tokens.push(Token::NotEquals);
                        self.pos += 2;
                    } else if self.peek_next() == Some('=') {
                        tokens.push(Token::LessEqual);
                        self.pos += 2;
                    } else {
                        tokens.push(Token::LessThan);
                        self.pos += 1;
                    }
                }
                '>' => {
                    if self.peek_next() == Some('=') {
                        tokens.push(Token::GreaterEqual);
                        self.pos += 2;
                    } else {
                        tokens.push(Token::GreaterThan);
                        self.pos += 1;
                    }
                }
                '\'' => {
                    tokens.push(self.read_string()?);
                }
                '#' => {
                    self.pos += 1;
                    let name = self.read_identifier_text();
                    tokens.push(Token::HashVariable(name));
                }
                '*' => {
                    if self.pos + 1 < self.chars.len()
                        && (self.chars[self.pos + 1].is_alphabetic() || self.chars[self.pos + 1] == '_')
                    {
                        self.pos += 1;
                        let name = self.read_identifier_text();
                        tokens.push(Token::SystemVariable(name.to_uppercase()));
                    } else {
                        tokens.push(Token::Star);
                        self.pos += 1;
                    }
                }
                _ if ch.is_ascii_digit() => {
                    tokens.push(self.read_number()?);
                }
                _ if ch.is_alphabetic() || ch == '_' => {
                    let word = self.read_identifier_text();
                    tokens.push(Self::keyword_or_ident(&word));
                }
                '\n' | '\r' => { self.pos += 1; }
                _ => {
                    return Err(LexError::UnexpectedChar(ch, self.pos));
                }
            }
        }
        let _ = self.input; // keep borrow alive
        Ok(tokens)
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.chars.len() && (self.chars[self.pos] == ' ' || self.chars[self.pos] == '\t') {
            self.pos += 1;
        }
    }

    fn peek_next(&self) -> Option<char> {
        self.chars.get(self.pos + 1).copied()
    }

    fn read_identifier_text(&mut self) -> String {
        let start = self.pos;
        while self.pos < self.chars.len()
            && (self.chars[self.pos].is_alphanumeric() || self.chars[self.pos] == '_' || self.chars[self.pos] == '-')
        {
            self.pos += 1;
        }
        self.chars[start..self.pos].iter().collect()
    }

    fn read_string(&mut self) -> Result<Token, LexError> {
        let start = self.pos;
        self.pos += 1; // skip opening '
        let mut s = String::new();
        loop {
            if self.pos >= self.chars.len() {
                return Err(LexError::UnterminatedString(start));
            }
            if self.chars[self.pos] == '\'' {
                self.pos += 1;
                // Double apostrophe is escape
                if self.pos < self.chars.len() && self.chars[self.pos] == '\'' {
                    s.push('\'');
                    self.pos += 1;
                } else {
                    break;
                }
            } else {
                s.push(self.chars[self.pos]);
                self.pos += 1;
            }
        }
        Ok(Token::StringLiteral(s))
    }

    fn read_number(&mut self) -> Result<Token, LexError> {
        let start = self.pos;
        while self.pos < self.chars.len() && self.chars[self.pos].is_ascii_digit() {
            self.pos += 1;
        }
        if self.pos < self.chars.len() && self.chars[self.pos] == '.' {
            self.pos += 1;
            while self.pos < self.chars.len() && self.chars[self.pos].is_ascii_digit() {
                self.pos += 1;
            }
            let text: String = self.chars[start..self.pos].iter().collect();
            Ok(Token::DecimalLiteral(text))
        } else {
            let text: String = self.chars[start..self.pos].iter().collect();
            Ok(Token::IntegerLiteral(text.parse::<i64>().unwrap_or(0)))
        }
    }

    fn keyword_or_ident(word: &str) -> Token {
        match word.to_uppercase().as_str() {
            "IF" => Token::If,
            "THEN" => Token::Then,
            "ELSE" => Token::Else,
            "END-IF" => Token::EndIf,
            "DECIDE" => Token::Decide,
            "FOR" => Token::For,
            "ON" => Token::On,
            "EVERY" => Token::Every,
            "FIRST" => Token::First,
            "VALUE" => Token::Value,
            "VALUES" => Token::Values,
            "WHEN" => Token::When,
            "NONE" => Token::None,
            "ANY" => Token::Any,
            "ALL" => Token::All,
            "END-DECIDE" => Token::EndDecide,
            "REPEAT" => Token::Repeat,
            "UNTIL" => Token::Until,
            "WHILE" => Token::While,
            "END-REPEAT" => Token::EndRepeat,
            "END-FOR" => Token::EndFor,
            "ESCAPE" => Token::Escape,
            "TOP" => Token::Top,
            "BOTTOM" => Token::Bottom,
            "ROUTINE" => Token::Routine,
            "PERFORM" => Token::Perform,
            "END-PERFORM" => Token::EndPerform,
            "DEFINE" => Token::Define,
            "DATA" => Token::Data,
            "LOCAL" => Token::Local,
            "GLOBAL" => Token::Global,
            "PARAMETER" => Token::Parameter,
            "USING" => Token::Using,
            "END-DEFINE" => Token::EndDefine,
            "INIT" => Token::Init,
            "CONST" => Token::Const,
            "REDEFINE" => Token::Redefine,
            "READ" => Token::Read,
            "BY" => Token::By,
            "ENDING" => Token::Ending,
            "END-READ" => Token::EndRead,
            "FIND" => Token::Find,
            "WITH" => Token::With,
            "SORTED" => Token::Sorted,
            "WHERE" => Token::Where,
            "END-FIND" => Token::EndFind,
            "HISTOGRAM" => Token::Histogram,
            "END-HISTOGRAM" => Token::EndHistogram,
            "GET" => Token::Get,
            "STORE" => Token::Store,
            "UPDATE" => Token::Update,
            "DELETE" => Token::Delete,
            "END" => Token::End,
            "TRANSACTION" => Token::Transaction,
            "BACKOUT" => Token::Backout,
            "DISPLAY" => Token::Display,
            "WRITE" => Token::Write,
            "PRINT" => Token::Print,
            "INPUT" => Token::Input,
            "REINPUT" => Token::Reinput,
            "MARK" => Token::Mark,
            "MAP" => Token::Map,
            "NEWPAGE" => Token::Newpage,
            "COMPUTE" => Token::Compute,
            "MOVE" => Token::Move,
            "EDITED" => Token::Edited,
            "COMPRESS" => Token::Compress,
            "INTO" => Token::Into,
            "LEAVING" => Token::Leaving,
            "SPACE" => Token::Space,
            "SEPARATE" => Token::Separate,
            "EXAMINE" => Token::Examine,
            "REPLACE" => Token::Replace,
            "FULL" => Token::Full,
            "GIVING" => Token::Giving,
            "NUMBER" => Token::Number,
            "POSITION" => Token::Position,
            "LENGTH" => Token::Length,
            "SORT" => Token::Sort,
            "CALLNAT" => Token::Callnat,
            "FETCH" => Token::Fetch,
            "RETURN" => Token::Return,
            "STACK" => Token::Stack,
            "END-ERROR" => Token::EndError,
            "AND" => Token::And,
            "OR" => Token::Or,
            "NOT" => Token::Not,
            "AT" => Token::At,
            "BREAK" => Token::Break,
            "OF" => Token::Of,
            "PAGE" => Token::Page,
            "SELECT" => Token::Select,
            "INSERT" => Token::Insert,
            "COMMIT" => Token::Commit,
            "ROLLBACK" => Token::Rollback,
            "WORK" => Token::Work,
            "FILE" => Token::File,
            "SUBSTR" => Token::Substr,
            "ABS" => Token::Abs,
            "SIGN" => Token::Sign,
            "FRAC" => Token::Frac,
            "INT" => Token::Int,
            "MOD" => Token::Mod,
            "SQRT" => Token::Sqrt,
            "VAL" => Token::Val,
            "TRIM" => Token::Trim,
            "UPPER" => Token::Upper,
            "LOWER" => Token::Lower,
            "TRANSLATE" => Token::Translate,
            "IS" => Token::IsFunc,
            "LOGON" => Token::Logon,
            "CATALOG" => Token::Catalog,
            "STOW" => Token::Stow,
            "ADD" => Token::Add,
            "SUBTRACT" => Token::Subtract,
            "MULTIPLY" => Token::Multiply,
            "DIVIDE" => Token::Divide,
            "RESET" => Token::Reset,
            "STOP" => Token::Stop,
            "TERMINATE" => Token::Terminate,
            "INCLUDE" => Token::Include,
            "LIMIT" => Token::Limit,
            "ACCEPT" => Token::Accept,
            "REJECT" => Token::Reject,
            "SET" => Token::Set,
            "KEY" => Token::Key,
            "WINDOW" => Token::Window,
            "RUN" => Token::Run,
            "CALL" => Token::Call,
            "CLASS" => Token::Class,
            "METHOD" => Token::Method,
            "PROPERTY" => Token::Property,
            "CREATE" => Token::Create,
            "OBJECT" => Token::Object,
            "SEND" => Token::Send,
            "INTERFACE" => Token::Interface,
            "END-CLASS" => Token::EndClass,
            "END-METHOD" => Token::EndMethod,
            "END-INTERFACE" => Token::EndInterface,
            "TRUE" => Token::True,
            "FALSE" => Token::False,
            "TO" => Token::To,
            "FROM" => Token::From,
            _ => Token::Identifier(word.to_uppercase()),
        }
    }
}

// ---------------------------------------------------------------------------
// AST
// ---------------------------------------------------------------------------

/// A parsed Natural program.
#[derive(Debug, Clone)]
pub struct Program {
    pub name: String,
    pub define_data: Option<DefineData>,
    pub statements: Vec<Statement>,
}

/// DEFINE DATA block.
#[derive(Debug, Clone)]
pub struct DefineData {
    pub sections: Vec<DataSection>,
}

/// LOCAL / GLOBAL / PARAMETER section.
#[derive(Debug, Clone)]
pub struct DataSection {
    pub kind: DataSectionKind,
    pub variables: Vec<VarDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataSectionKind {
    Local,
    Global,
    Parameter,
}

/// Variable declaration inside DEFINE DATA.
#[derive(Debug, Clone)]
pub struct VarDecl {
    pub level: u8,
    pub name: String,
    pub data_type: Option<String>,
    pub array_dims: Vec<(i32, i32)>,
    pub init_value: Option<Expr>,
}

/// Expression node.
#[derive(Debug, Clone)]
pub enum Expr {
    IntLit(i64),
    DecLit(String),
    StrLit(String),
    BoolLit(bool),
    Var(String),
    SysVar(String),
    BinOp { op: BinOp, left: Box<Expr>, right: Box<Expr> },
    UnaryMinus(Box<Expr>),
    FuncCall { name: String, args: Vec<Expr> },
    Not(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add, Sub, Mul, Div, Mod,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or,
}

/// All statement types in the AST.
#[derive(Debug, Clone)]
pub enum Statement {
    DefineDataStmt(DefineData),
    IfStmt { condition: Expr, then_body: Vec<Statement>, else_body: Vec<Statement> },
    DecideFor { conditions: Vec<(Expr, Vec<Statement>)>, none_body: Vec<Statement> },
    DecideOn { expr: Expr, values: Vec<(Vec<Expr>, Vec<Statement>)>, none_body: Vec<Statement> },
    ForLoop { var: String, from: Expr, to: Expr, step: Option<Expr>, body: Vec<Statement> },
    RepeatLoop { body: Vec<Statement>, until: Option<Expr> },
    EscapeStmt(EscapeKind),
    PerformStmt { name: String },
    ReadStmt { view: String, by: Option<String>, starting: Option<Expr>, ending: Option<Expr>, body: Vec<Statement> },
    FindStmt { view: String, with: Option<Expr>, sorted_by: Option<String>, body: Vec<Statement> },
    HistogramStmt { view: String, field: String, body: Vec<Statement> },
    GetStmt { view: String, isn: Expr },
    StoreStmt { view: String },
    UpdateStmt,
    DeleteStmt,
    EndTransaction,
    BackoutTransaction,
    DisplayStmt { items: Vec<Expr> },
    WriteStmt { items: Vec<Expr> },
    PrintStmt { items: Vec<Expr> },
    InputStmt { prompt: Option<String>, vars: Vec<String>, map_name: Option<String> },
    ReinputStmt { message: String, mark_field: Option<String> },
    ComputeStmt { target: String, expr: Expr },
    MoveStmt { source: Expr, target: String },
    MoveByNameStmt { source: String, target: String },
    CompressStmt { sources: Vec<Expr>, into: String, leaving_space: bool },
    SeparateStmt { source: Expr, into: Vec<String>, delimiter: Option<String> },
    ExamineStmt { target: String, pattern: Expr, replace_with: Option<Expr>, giving_number: Option<String>, giving_position: Option<String>, giving_length: Option<String> },
    SortStmt { body: Vec<Statement>, by: Vec<(String, SortOrder)> },
    CallnatStmt { subprogram: String, args: Vec<Expr> },
    FetchStmt { program: String },
    ReturnStmt,
    StackStmt { position: StackPosition, data: Vec<Expr> },
    OnErrorBlock { body: Vec<Statement> },
    AtBreak { field: String, body: Vec<Statement> },
    AtTopOfPage { body: Vec<Statement> },
    AtEndOfPage { body: Vec<Statement> },
    Newpage,
    SelectStmt { columns: Vec<String>, into: Vec<String>, from: String, condition: Option<Expr> },
    InsertStmt { table: String, columns: Vec<String>, values: Vec<Expr> },
    SqlUpdate { table: String, set: Vec<(String, Expr)>, condition: Option<Expr> },
    SqlDelete { table: String, condition: Option<Expr> },
    CommitStmt,
    RollbackStmt,
    ReadWorkFile { file_num: u8, vars: Vec<String> },
    WriteWorkFile { file_num: u8, items: Vec<Expr> },
    InlineSubroutine { name: String, body: Vec<Statement> },
    // Additional statements
    AddStmt { target: String, expr: Expr },
    SubtractStmt { target: String, expr: Expr },
    MultiplyStmt { target: String, expr: Expr },
    DivideStmt { target: String, expr: Expr, giving: Option<String> },
    ResetStmt { vars: Vec<String> },
    StopStmt,
    TerminateStmt,
    IncludeStmt { copycode: String },
    MoveAllStmt { source: Expr, target: String },
    LimitStmt { count: Expr },
    AcceptStmt,
    RejectStmt,
    SetKeyStmt { key: String, action: Option<String> },
    SetWindowStmt { window: Option<String> },
    DefineWindowStmt { name: String, size: Option<(Expr, Expr)>, position: Option<(Expr, Expr)> },
    CallExternalStmt { program: String, args: Vec<Expr> },
    RunStmt { program: String },
    // OO statements
    DefineClassStmt { name: String, methods: Vec<Statement>, properties: Vec<(String, String)> },
    CreateObjectStmt { class: String, target: String },
    SendMethodStmt { object: String, method: String, args: Vec<Expr> },
    DefineInterfaceStmt { name: String, methods: Vec<String> },
    MethodStmt { name: String, body: Vec<Statement> },
    PropertyStmt { name: String, data_type: String },
}

#[derive(Debug, Clone, PartialEq)]
pub enum EscapeKind {
    Top,
    Bottom,
    Routine,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SortOrder {
    Ascending,
    Descending,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StackPosition {
    Top,
    Bottom,
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

/// Parser error.
#[derive(Debug, Clone, thiserror::Error)]
pub enum ParseError {
    #[error("unexpected token: {0:?}")]
    UnexpectedToken(Token),
    #[error("unexpected end of input")]
    UnexpectedEof,
    #[error("expected {expected}, found {found:?}")]
    Expected { expected: String, found: Token },
    #[error("lex error: {0}")]
    LexError(#[from] LexError),
}

/// Parse Natural source into a `Program`.
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn parse_program(&mut self, name: &str) -> Result<Program, ParseError> {
        let mut define_data = None;
        let mut statements = Vec::new();

        while self.pos < self.tokens.len() {
            if self.check(&Token::Eof) {
                break;
            }
            if self.check(&Token::Define) && self.check_ahead(1, &Token::Data) {
                define_data = Some(self.parse_define_data()?);
            } else {
                let stmt = self.parse_statement()?;
                statements.push(stmt);
            }
        }

        Ok(Program {
            name: name.to_string(),
            define_data,
            statements,
        })
    }

    fn parse_define_data(&mut self) -> Result<DefineData, ParseError> {
        self.expect(&Token::Define)?;
        self.expect(&Token::Data)?;
        let mut sections = Vec::new();

        while !self.check(&Token::EndDefine) && self.pos < self.tokens.len() {
            let kind = match self.peek() {
                Some(Token::Local) => { self.advance(); DataSectionKind::Local }
                Some(Token::Global) => { self.advance(); DataSectionKind::Global }
                Some(Token::Parameter) => { self.advance(); DataSectionKind::Parameter }
                _ => DataSectionKind::Local,
            };
            let mut variables = Vec::new();
            while self.pos < self.tokens.len()
                && !self.check(&Token::EndDefine)
                && !self.check(&Token::Local)
                && !self.check(&Token::Global)
                && !self.check(&Token::Parameter)
            {
                let var = self.parse_var_decl()?;
                variables.push(var);
            }
            sections.push(DataSection { kind, variables });
        }
        self.expect(&Token::EndDefine)?;
        Ok(DefineData { sections })
    }

    fn parse_var_decl(&mut self) -> Result<VarDecl, ParseError> {
        let level = if let Some(Token::IntegerLiteral(n)) = self.peek() {
            let l = n as u8;
            self.advance();
            l
        } else {
            1
        };

        let name = self.expect_identifier()?;

        let mut data_type = None;
        let mut array_dims = Vec::new();
        let init_value = None;

        // optional (A10) or (P7.2) or (I4) etc.
        if self.check(&Token::LeftParen) {
            self.advance(); // consume (
            // Check for array dims like 1:10
            if self.is_array_start() {
                array_dims = self.parse_array_dims()?;
                self.expect_token(&Token::RightParen)?;
                // Might still have type
                if self.check(&Token::LeftParen) {
                    self.advance();
                    data_type = Some(self.parse_type_spec()?);
                    self.expect_token(&Token::RightParen)?;
                }
            } else {
                data_type = Some(self.parse_type_spec()?);
                self.expect_token(&Token::RightParen)?;
            }
        }

        Ok(VarDecl { level, name, data_type, array_dims, init_value })
    }

    fn is_array_start(&self) -> bool {
        // Look for pattern: number : or *
        if let Some(Token::IntegerLiteral(_)) = self.peek() {
            if let Some(Token::Colon) = self.peek_at(1) {
                return true;
            }
        }
        if let Some(Token::Star) = self.peek() {
            return true;
        }
        false
    }

    fn parse_array_dims(&mut self) -> Result<Vec<(i32, i32)>, ParseError> {
        let mut dims = Vec::new();
        loop {
            let lower = if let Some(Token::Star) = self.peek() {
                self.advance();
                0
            } else if let Some(Token::IntegerLiteral(n)) = self.peek() {
                let v = n as i32;
                self.advance();
                if self.check(&Token::Colon) {
                    self.advance();
                    let upper = match self.peek() {
                        Some(Token::IntegerLiteral(u)) => { let u2 = u as i32; self.advance(); u2 }
                        Some(Token::Star) => { self.advance(); -1 }
                        _ => v,
                    };
                    dims.push((v, upper));
                    if self.check(&Token::Comma) { self.advance(); continue; }
                    break;
                }
                v
            } else {
                break;
            };
            dims.push((1, lower));
            if self.check(&Token::Comma) { self.advance(); continue; }
            break;
        }
        Ok(dims)
    }

    fn parse_type_spec(&mut self) -> Result<String, ParseError> {
        let mut spec = String::new();
        // Read type letter + optional length
        while self.pos < self.tokens.len() && !self.check(&Token::RightParen) {
            match self.peek() {
                Some(Token::Identifier(s)) => { spec.push_str(&s); self.advance(); }
                Some(Token::IntegerLiteral(n)) => { spec.push_str(&n.to_string()); self.advance(); }
                Some(Token::Period) => { spec.push('.'); self.advance(); }
                Some(Token::DecimalLiteral(s)) => { spec.push_str(&s); self.advance(); }
                _ => break,
            }
        }
        Ok(spec)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        let tok = self.peek().ok_or(ParseError::UnexpectedEof)?;
        match tok {
            Token::If => self.parse_if(),
            Token::Decide => self.parse_decide(),
            Token::For => self.parse_for(),
            Token::Repeat => self.parse_repeat(),
            Token::Escape => self.parse_escape(),
            Token::Perform => self.parse_perform(),
            Token::Read => {
                // Could be READ WORK FILE or READ view
                if self.check_ahead(1, &Token::Work) {
                    self.parse_read_work_file()
                } else {
                    self.parse_read()
                }
            }
            Token::Find => self.parse_find(),
            Token::Histogram => self.parse_histogram(),
            Token::Get => self.parse_get(),
            Token::Store => self.parse_store(),
            Token::Update => { self.advance(); Ok(Statement::UpdateStmt) }
            Token::Delete => { self.advance(); Ok(Statement::DeleteStmt) }
            Token::End => {
                self.advance();
                if self.check(&Token::Transaction) { self.advance(); }
                Ok(Statement::EndTransaction)
            }
            Token::Backout => {
                self.advance();
                if self.check(&Token::Transaction) { self.advance(); }
                Ok(Statement::BackoutTransaction)
            }
            Token::Display => self.parse_display(),
            Token::Write => {
                if self.check_ahead(1, &Token::Work) {
                    self.parse_write_work_file()
                } else {
                    self.parse_write()
                }
            }
            Token::Print => self.parse_print(),
            Token::Input => self.parse_input(),
            Token::Reinput => self.parse_reinput(),
            Token::Compute => self.parse_compute(),
            Token::Move => self.parse_move(),
            Token::Compress => self.parse_compress(),
            Token::Separate => self.parse_separate(),
            Token::Examine => self.parse_examine(),
            Token::Sort => self.parse_sort(),
            Token::Callnat => self.parse_callnat(),
            Token::Fetch => self.parse_fetch(),
            Token::Return => { self.advance(); Ok(Statement::ReturnStmt) }
            Token::Stack => self.parse_stack(),
            Token::At => self.parse_at_block(),
            Token::Newpage => { self.advance(); Ok(Statement::Newpage) }
            Token::Select => self.parse_select(),
            Token::Insert => self.parse_insert(),
            Token::Commit => { self.advance(); Ok(Statement::CommitStmt) }
            Token::Rollback => { self.advance(); Ok(Statement::RollbackStmt) }
            Token::Define => self.parse_define_block(),
            Token::Add => self.parse_add(),
            Token::Subtract => self.parse_subtract(),
            Token::Multiply => self.parse_multiply(),
            Token::Divide => self.parse_divide(),
            Token::Reset => self.parse_reset(),
            Token::Stop => { self.advance(); Ok(Statement::StopStmt) }
            Token::Terminate => { self.advance(); Ok(Statement::TerminateStmt) }
            Token::Include => self.parse_include(),
            Token::Accept => { self.advance(); Ok(Statement::AcceptStmt) }
            Token::Reject => { self.advance(); Ok(Statement::RejectStmt) }
            Token::Set => self.parse_set(),
            Token::Call => self.parse_call_external(),
            Token::Run => self.parse_run(),
            Token::Create => self.parse_create_object(),
            Token::Send => self.parse_send_method(),
            Token::OnError => self.parse_on_error(),
            Token::Identifier(ref s) if s == "ON" => self.parse_on_error(),
            _ => {
                // Try compute-style assignment: #VAR = expr
                if let Token::HashVariable(_) = tok {
                    self.parse_assignment()
                } else {
                    let t = tok.clone();
                    self.advance();
                    Err(ParseError::UnexpectedToken(t))
                }
            }
        }
    }

    fn parse_if(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // IF
        let condition = self.parse_expression()?;
        if self.check(&Token::Then) { self.advance(); }
        let mut then_body = Vec::new();
        while self.pos < self.tokens.len() && !self.check(&Token::Else) && !self.check(&Token::EndIf) {
            then_body.push(self.parse_statement()?);
        }
        let mut else_body = Vec::new();
        if self.check(&Token::Else) {
            self.advance();
            while self.pos < self.tokens.len() && !self.check(&Token::EndIf) {
                else_body.push(self.parse_statement()?);
            }
        }
        self.expect(&Token::EndIf)?;
        Ok(Statement::IfStmt { condition, then_body, else_body })
    }

    fn parse_decide(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // DECIDE
        if self.check(&Token::For) {
            self.advance();
            if self.check(&Token::First) || self.check(&Token::Every) {
                self.advance();
            }
            // DECIDE FOR FIRST/EVERY CONDITION
            if self.check(&Token::Identifier(String::new())) || matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "CONDITION") {
                self.advance(); // CONDITION
            }
            let mut conditions = Vec::new();
            let mut none_body = Vec::new();
            while self.pos < self.tokens.len() && !self.check(&Token::EndDecide) {
                if self.check(&Token::When) {
                    self.advance();
                    if self.check(&Token::None) {
                        self.advance();
                        while self.pos < self.tokens.len() && !self.check(&Token::EndDecide) {
                            none_body.push(self.parse_statement()?);
                        }
                    } else {
                        let cond = self.parse_expression()?;
                        let mut body = Vec::new();
                        while self.pos < self.tokens.len()
                            && !self.check(&Token::When)
                            && !self.check(&Token::EndDecide)
                        {
                            body.push(self.parse_statement()?);
                        }
                        conditions.push((cond, body));
                    }
                } else {
                    break;
                }
            }
            self.expect(&Token::EndDecide)?;
            Ok(Statement::DecideFor { conditions, none_body })
        } else if self.check(&Token::On) {
            self.advance();
            if self.check(&Token::First) || self.check(&Token::Every) { self.advance(); }
            if self.check(&Token::Value) || self.check(&Token::Values) { self.advance(); }
            if self.check(&Token::Of) { self.advance(); }
            let expr = self.parse_expression()?;
            let mut value_cases = Vec::new();
            let mut none_body = Vec::new();
            while self.pos < self.tokens.len() && !self.check(&Token::EndDecide) {
                if self.check(&Token::When) {
                    self.advance();
                    if self.check(&Token::None) {
                        self.advance();
                        while self.pos < self.tokens.len() && !self.check(&Token::EndDecide) {
                            none_body.push(self.parse_statement()?);
                        }
                    } else {
                        let mut vals = Vec::new();
                        loop {
                            vals.push(self.parse_expression()?);
                            if self.check(&Token::Comma) { self.advance(); } else { break; }
                        }
                        let mut body = Vec::new();
                        while self.pos < self.tokens.len()
                            && !self.check(&Token::When)
                            && !self.check(&Token::EndDecide)
                        {
                            body.push(self.parse_statement()?);
                        }
                        value_cases.push((vals, body));
                    }
                } else {
                    break;
                }
            }
            self.expect(&Token::EndDecide)?;
            Ok(Statement::DecideOn { expr, values: value_cases, none_body })
        } else {
            Err(ParseError::Expected { expected: "FOR or ON".into(), found: self.peek().unwrap_or(Token::Eof) })
        }
    }

    fn parse_for(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // FOR
        let var = self.expect_identifier()?;
        self.expect(&Token::Equals)?;
        let from = self.parse_expression()?;
        self.expect(&Token::To)?;
        let to = self.parse_expression()?;
        let step = if self.check(&Token::Identifier(String::new())) || matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "STEP") {
            if matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "STEP") {
                self.advance();
                Some(self.parse_expression()?)
            } else {
                None
            }
        } else {
            None
        };
        let mut body = Vec::new();
        while self.pos < self.tokens.len() && !self.check(&Token::EndFor) {
            body.push(self.parse_statement()?);
        }
        self.expect(&Token::EndFor)?;
        Ok(Statement::ForLoop { var, from, to, step, body })
    }

    fn parse_repeat(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // REPEAT
        let mut body = Vec::new();
        let mut until = None;
        while self.pos < self.tokens.len() && !self.check(&Token::EndRepeat) {
            if self.check(&Token::Until) {
                self.advance();
                until = Some(self.parse_expression()?);
            } else {
                body.push(self.parse_statement()?);
            }
        }
        self.expect(&Token::EndRepeat)?;
        Ok(Statement::RepeatLoop { body, until })
    }

    fn parse_escape(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // ESCAPE
        let kind = if self.check(&Token::Top) { self.advance(); EscapeKind::Top }
            else if self.check(&Token::Bottom) { self.advance(); EscapeKind::Bottom }
            else if self.check(&Token::Routine) { self.advance(); EscapeKind::Routine }
            else { EscapeKind::Bottom };
        Ok(Statement::EscapeStmt(kind))
    }

    fn parse_perform(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // PERFORM
        let name = self.expect_identifier()?;
        Ok(Statement::PerformStmt { name })
    }

    fn parse_read(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // READ
        let view = self.expect_identifier()?;
        let mut by = None;
        let mut starting = None;
        let mut ending = None;
        if self.check(&Token::By) {
            self.advance();
            by = Some(self.expect_identifier()?);
        }
        if matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "STARTING") {
            self.advance();
            if self.check(&Token::From) { self.advance(); }
            starting = Some(self.parse_expression()?);
        }
        if self.check(&Token::Ending) {
            self.advance();
            if self.check(&Token::At) { self.advance(); }
            ending = Some(self.parse_expression()?);
        }
        let mut body = Vec::new();
        while self.pos < self.tokens.len() && !self.check(&Token::EndRead) {
            body.push(self.parse_statement()?);
        }
        self.expect(&Token::EndRead)?;
        Ok(Statement::ReadStmt { view, by, starting, ending, body })
    }

    fn parse_find(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // FIND
        let view = self.expect_identifier()?;
        let mut with = None;
        let mut sorted_by = None;
        if self.check(&Token::With) {
            self.advance();
            with = Some(self.parse_expression()?);
        }
        if self.check(&Token::Sorted) {
            self.advance();
            if self.check(&Token::By) { self.advance(); }
            sorted_by = Some(self.expect_identifier()?);
        }
        let mut body = Vec::new();
        while self.pos < self.tokens.len() && !self.check(&Token::EndFind) {
            body.push(self.parse_statement()?);
        }
        self.expect(&Token::EndFind)?;
        Ok(Statement::FindStmt { view, with, sorted_by, body })
    }

    fn parse_histogram(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // HISTOGRAM
        let view = self.expect_identifier()?;
        let field = self.expect_identifier()?;
        let mut body = Vec::new();
        while self.pos < self.tokens.len() && !self.check(&Token::EndHistogram) {
            body.push(self.parse_statement()?);
        }
        self.expect(&Token::EndHistogram)?;
        Ok(Statement::HistogramStmt { view, field, body })
    }

    fn parse_get(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // GET
        let view = self.expect_identifier()?;
        let isn = self.parse_expression()?;
        Ok(Statement::GetStmt { view, isn })
    }

    fn parse_store(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // STORE
        let view = self.expect_identifier()?;
        Ok(Statement::StoreStmt { view })
    }

    fn parse_display(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // DISPLAY
        let items = self.parse_expr_list()?;
        Ok(Statement::DisplayStmt { items })
    }

    fn parse_write(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // WRITE
        let items = self.parse_expr_list()?;
        Ok(Statement::WriteStmt { items })
    }

    fn parse_print(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // PRINT
        let items = self.parse_expr_list()?;
        Ok(Statement::PrintStmt { items })
    }

    fn parse_input(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // INPUT
        if self.check(&Token::Using) {
            self.advance();
            self.expect(&Token::Map)?;
            let map_name = match self.peek() {
                Some(Token::StringLiteral(s)) => { let n = s; self.advance(); n }
                _ => self.expect_identifier()?,
            };
            return Ok(Statement::InputStmt { prompt: None, vars: Vec::new(), map_name: Some(map_name) });
        }
        let mut prompt = None;
        let mut vars = Vec::new();
        if let Some(Token::StringLiteral(s)) = self.peek() {
            prompt = Some(s);
            self.advance();
        }
        while let Some(Token::HashVariable(v)) = self.peek() {
            vars.push(format!("#{v}"));
            self.advance();
        }
        Ok(Statement::InputStmt { prompt, vars, map_name: None })
    }

    fn parse_reinput(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // REINPUT
        let message = match self.peek() {
            Some(Token::StringLiteral(s)) => { let m = s; self.advance(); m }
            _ => String::new(),
        };
        let mut mark_field = None;
        if self.check(&Token::Mark) {
            self.advance();
            if let Some(Token::HashVariable(v)) = self.peek() {
                mark_field = Some(format!("#{v}"));
                self.advance();
            } else if let Some(Token::Star) = self.peek() {
                self.advance();
                if let Some(Token::HashVariable(v)) = self.peek() {
                    mark_field = Some(format!("*#{v}"));
                    self.advance();
                }
            }
        }
        Ok(Statement::ReinputStmt { message, mark_field })
    }

    fn parse_compute(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // COMPUTE
        let target = self.expect_identifier()?;
        self.expect(&Token::Equals)?;
        let expr = self.parse_expression()?;
        Ok(Statement::ComputeStmt { target, expr })
    }

    fn parse_assignment(&mut self) -> Result<Statement, ParseError> {
        let target = self.expect_identifier()?;
        self.expect(&Token::Equals)?;
        let expr = self.parse_expression()?;
        Ok(Statement::ComputeStmt { target, expr })
    }

    fn parse_move(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // MOVE
        if self.check(&Token::By) || matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "BY") {
            // MOVE BY NAME
            self.advance(); // BY
            if matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "NAME") { self.advance(); }
            let source = self.expect_identifier()?;
            self.expect(&Token::To)?;
            let target = self.expect_identifier()?;
            return Ok(Statement::MoveByNameStmt { source, target });
        }
        let source = self.parse_expression()?;
        self.expect(&Token::To)?;
        let target = self.expect_identifier()?;
        Ok(Statement::MoveStmt { source, target })
    }

    fn parse_compress(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // COMPRESS
        let mut sources = Vec::new();
        while self.pos < self.tokens.len() && !self.check(&Token::Into) {
            sources.push(self.parse_expression()?);
        }
        self.expect(&Token::Into)?;
        let into = self.expect_identifier()?;
        let leaving_space = if self.check(&Token::Leaving) {
            self.advance();
            if self.check(&Token::Space) { self.advance(); }
            true
        } else {
            false
        };
        Ok(Statement::CompressStmt { sources, into, leaving_space })
    }

    fn parse_separate(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // SEPARATE
        let source = self.parse_expression()?;
        self.expect(&Token::Into)?;
        let mut into = Vec::new();
        while self.pos < self.tokens.len() {
            if let Some(Token::HashVariable(v)) = self.peek() {
                into.push(format!("#{v}"));
                self.advance();
            } else {
                break;
            }
        }
        let delimiter = if self.check(&Token::With) {
            self.advance();
            if matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "DELIMITER") { self.advance(); }
            match self.peek() {
                Some(Token::StringLiteral(s)) => { let d = s; self.advance(); Some(d) }
                _ => None,
            }
        } else {
            None
        };
        Ok(Statement::SeparateStmt { source, into, delimiter })
    }

    fn parse_examine(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // EXAMINE
        let target = self.expect_identifier()?;
        if self.check(&Token::For) { self.advance(); }
        let pattern = self.parse_expression()?;
        let mut replace_with = None;
        let mut giving_number = None;
        let mut giving_position = None;
        let mut giving_length = None;

        while self.pos < self.tokens.len() {
            if self.check(&Token::Replace) {
                self.advance();
                if self.check(&Token::With) { self.advance(); }
                replace_with = Some(self.parse_expression()?);
            } else if self.check(&Token::Giving) {
                self.advance();
                if self.check(&Token::Number) {
                    self.advance();
                    giving_number = Some(self.expect_identifier()?);
                } else if self.check(&Token::Position) {
                    self.advance();
                    giving_position = Some(self.expect_identifier()?);
                } else if self.check(&Token::Length) {
                    self.advance();
                    giving_length = Some(self.expect_identifier()?);
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        Ok(Statement::ExamineStmt { target, pattern, replace_with, giving_number, giving_position, giving_length })
    }

    fn parse_sort(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // SORT
        if self.check(&Token::By) { self.advance(); }
        let mut sort_by = Vec::new();
        while self.pos < self.tokens.len() {
            if let Some(Token::Identifier(_)) = self.peek() {
                let field = self.expect_identifier()?;
                let order = if matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "DESCENDING" || s == "DESC") {
                    self.advance();
                    SortOrder::Descending
                } else {
                    if matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "ASCENDING" || s == "ASC") { self.advance(); }
                    SortOrder::Ascending
                };
                sort_by.push((field, order));
            } else {
                break;
            }
        }
        Ok(Statement::SortStmt { body: Vec::new(), by: sort_by })
    }

    fn parse_callnat(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // CALLNAT
        let subprogram = match self.peek() {
            Some(Token::StringLiteral(s)) => { let n = s; self.advance(); n }
            _ => self.expect_identifier()?,
        };
        let args = self.parse_expr_list()?;
        Ok(Statement::CallnatStmt { subprogram, args })
    }

    fn parse_fetch(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // FETCH
        if self.check(&Token::Return) { self.advance(); }
        let program = match self.peek() {
            Some(Token::StringLiteral(s)) => { let n = s; self.advance(); n }
            _ => self.expect_identifier()?,
        };
        Ok(Statement::FetchStmt { program })
    }

    fn parse_stack(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // STACK
        let position = if self.check(&Token::Top) { self.advance(); StackPosition::Top }
            else if self.check(&Token::Bottom) { self.advance(); StackPosition::Bottom }
            else { StackPosition::Top };
        if self.check(&Token::Data) { self.advance(); }
        let data = self.parse_expr_list()?;
        Ok(Statement::StackStmt { position, data })
    }

    fn parse_on_error(&mut self) -> Result<Statement, ParseError> {
        // ON ERROR
        if self.check(&Token::OnError) { self.advance(); }
        else {
            self.advance(); // ON
            if matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "ERROR") {
                self.advance();
            }
        }
        let mut body = Vec::new();
        while self.pos < self.tokens.len() && !self.check(&Token::EndError) {
            body.push(self.parse_statement()?);
        }
        self.expect(&Token::EndError)?;
        Ok(Statement::OnErrorBlock { body })
    }

    fn parse_at_block(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // AT
        if self.check(&Token::Break) {
            self.advance();
            if self.check(&Token::Of) { self.advance(); }
            let field = self.expect_identifier()?;
            let mut body = Vec::new();
            while self.pos < self.tokens.len() && !self.check_end_block() {
                body.push(self.parse_statement()?);
            }
            self.skip_end_block();
            Ok(Statement::AtBreak { field, body })
        } else if self.check(&Token::Top) {
            self.advance();
            if self.check(&Token::Of) { self.advance(); }
            if self.check(&Token::Page) { self.advance(); }
            let mut body = Vec::new();
            while self.pos < self.tokens.len() && !self.check_end_block() {
                body.push(self.parse_statement()?);
            }
            self.skip_end_block();
            Ok(Statement::AtTopOfPage { body })
        } else if self.check(&Token::End) {
            self.advance();
            if self.check(&Token::Of) { self.advance(); }
            if self.check(&Token::Page) { self.advance(); }
            let mut body = Vec::new();
            while self.pos < self.tokens.len() && !self.check_end_block() {
                body.push(self.parse_statement()?);
            }
            self.skip_end_block();
            Ok(Statement::AtEndOfPage { body })
        } else {
            Err(ParseError::Expected { expected: "BREAK, TOP, or END".into(), found: self.peek().unwrap_or(Token::Eof) })
        }
    }

    fn check_end_block(&self) -> bool {
        matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "END-BREAK" || s == "END-TOPPAGE" || s == "END-ENDPAGE")
            || (self.check(&Token::EndFind) || self.check(&Token::EndRead) || self.check(&Token::EndRepeat)
                || self.check(&Token::EndFor) || self.check(&Token::EndIf))
    }

    fn skip_end_block(&mut self) {
        if matches!(self.peek(), Some(Token::Identifier(_))) {
            self.advance();
        }
    }

    fn parse_select(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // SELECT
        let mut columns = Vec::new();
        while self.pos < self.tokens.len() && !self.check(&Token::Into) && !self.check(&Token::From) {
            columns.push(self.expect_identifier()?);
            if self.check(&Token::Comma) { self.advance(); }
        }
        let mut into_vars = Vec::new();
        if self.check(&Token::Into) {
            self.advance();
            while self.pos < self.tokens.len() && !self.check(&Token::From) {
                into_vars.push(self.expect_identifier()?);
                if self.check(&Token::Comma) { self.advance(); }
            }
        }
        self.expect(&Token::From)?;
        let table = self.expect_identifier()?;
        let condition = if self.check(&Token::Where) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };
        Ok(Statement::SelectStmt { columns, into: into_vars, from: table, condition })
    }

    fn parse_insert(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // INSERT
        if self.check(&Token::Into) { self.advance(); }
        let table = self.expect_identifier()?;
        let mut columns = Vec::new();
        if self.check(&Token::LeftParen) {
            self.advance();
            while !self.check(&Token::RightParen) && self.pos < self.tokens.len() {
                columns.push(self.expect_identifier()?);
                if self.check(&Token::Comma) { self.advance(); }
            }
            self.expect_token(&Token::RightParen)?;
        }
        let mut values = Vec::new();
        if self.check(&Token::Values) || matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "VALUES") {
            self.advance();
            if self.check(&Token::LeftParen) {
                self.advance();
                while !self.check(&Token::RightParen) && self.pos < self.tokens.len() {
                    values.push(self.parse_expression()?);
                    if self.check(&Token::Comma) { self.advance(); }
                }
                self.expect_token(&Token::RightParen)?;
            }
        }
        Ok(Statement::InsertStmt { table, columns, values })
    }

    fn parse_read_work_file(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // READ
        self.expect(&Token::Work)?;
        self.expect(&Token::File)?;
        let file_num = match self.peek() {
            Some(Token::IntegerLiteral(n)) => { let f = n as u8; self.advance(); f }
            _ => 1,
        };
        let mut vars = Vec::new();
        while let Some(Token::HashVariable(v)) = self.peek() {
            vars.push(format!("#{v}"));
            self.advance();
        }
        Ok(Statement::ReadWorkFile { file_num, vars })
    }

    fn parse_write_work_file(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // WRITE
        self.expect(&Token::Work)?;
        self.expect(&Token::File)?;
        let file_num = match self.peek() {
            Some(Token::IntegerLiteral(n)) => { let f = n as u8; self.advance(); f }
            _ => 1,
        };
        let items = self.parse_expr_list()?;
        Ok(Statement::WriteWorkFile { file_num, items })
    }

    // -----------------------------------------------------------------------
    // Additional statement parsers
    // -----------------------------------------------------------------------

    fn parse_define_block(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // DEFINE
        if self.check(&Token::Identifier(String::new())) || matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "SUBROUTINE") {
            if matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "SUBROUTINE") {
                self.advance();
                let name = self.expect_identifier()?;
                let mut body = Vec::new();
                while self.pos < self.tokens.len() && !matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "END-SUBROUTINE") {
                    body.push(self.parse_statement()?);
                }
                if matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "END-SUBROUTINE") {
                    self.advance();
                }
                return Ok(Statement::InlineSubroutine { name, body });
            }
        }
        if self.check(&Token::Window) {
            return self.parse_define_window_inner();
        }
        if self.check(&Token::Class) {
            return self.parse_define_class();
        }
        Err(ParseError::Expected { expected: "SUBROUTINE, CLASS, or WINDOW after DEFINE".into(), found: self.peek().unwrap_or(Token::Eof) })
    }

    fn parse_define_class(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // CLASS
        let name = self.expect_identifier()?;
        let mut methods = Vec::new();
        let mut properties = Vec::new();
        while self.pos < self.tokens.len() && !self.check(&Token::EndClass) {
            if self.check(&Token::Method) {
                self.advance();
                let method_name = self.expect_identifier()?;
                let mut body = Vec::new();
                while self.pos < self.tokens.len() && !self.check(&Token::EndMethod) {
                    body.push(self.parse_statement()?);
                }
                if self.check(&Token::EndMethod) { self.advance(); }
                methods.push(Statement::MethodStmt { name: method_name, body });
            } else if self.check(&Token::Property) {
                self.advance();
                let prop_name = self.expect_identifier()?;
                let dtype = if self.check(&Token::LeftParen) {
                    self.advance();
                    let t = self.parse_type_spec()?;
                    self.expect_token(&Token::RightParen)?;
                    t
                } else {
                    String::new()
                };
                properties.push((prop_name, dtype));
            } else {
                self.advance(); // skip unrecognized tokens in class body
            }
        }
        if self.check(&Token::EndClass) { self.advance(); }
        Ok(Statement::DefineClassStmt { name, methods, properties })
    }

    fn parse_define_window_inner(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // WINDOW
        let name = self.expect_identifier()?;
        let mut size = None;
        let mut position = None;
        if matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "SIZE") {
            self.advance();
            let rows = self.parse_expression()?;
            if self.check(&Token::Star) { self.advance(); }
            let cols = self.parse_expression()?;
            size = Some((rows, cols));
        }
        if matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "POSITION") {
            self.advance();
            let row = self.parse_expression()?;
            if self.check(&Token::Slash) { self.advance(); }
            let col = self.parse_expression()?;
            position = Some((row, col));
        }
        Ok(Statement::DefineWindowStmt { name, size, position })
    }

    fn parse_add(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // ADD
        let expr = self.parse_expression()?;
        self.expect(&Token::To)?;
        let target = self.expect_identifier()?;
        Ok(Statement::AddStmt { target, expr })
    }

    fn parse_subtract(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // SUBTRACT
        let expr = self.parse_expression()?;
        self.expect(&Token::From)?;
        let target = self.expect_identifier()?;
        Ok(Statement::SubtractStmt { target, expr })
    }

    fn parse_multiply(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // MULTIPLY
        let expr = self.parse_expression()?;
        if self.check(&Token::By) { self.advance(); }
        let target = self.expect_identifier()?;
        Ok(Statement::MultiplyStmt { target, expr })
    }

    fn parse_divide(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // DIVIDE
        let expr = self.parse_expression()?;
        self.expect(&Token::Into)?;
        let target = self.expect_identifier()?;
        let giving = if self.check(&Token::Giving) {
            self.advance();
            Some(self.expect_identifier()?)
        } else {
            None
        };
        Ok(Statement::DivideStmt { target, expr, giving })
    }

    fn parse_reset(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // RESET
        let mut vars = Vec::new();
        while self.pos < self.tokens.len() {
            match self.peek() {
                Some(Token::HashVariable(v)) => { vars.push(format!("#{v}")); self.advance(); }
                Some(Token::Identifier(_)) => { vars.push(self.expect_identifier()?); }
                _ => break,
            }
        }
        Ok(Statement::ResetStmt { vars })
    }

    fn parse_include(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // INCLUDE
        let copycode = match self.peek() {
            Some(Token::StringLiteral(s)) => { let n = s; self.advance(); n }
            _ => self.expect_identifier()?,
        };
        Ok(Statement::IncludeStmt { copycode })
    }

    fn parse_set(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // SET
        if self.check(&Token::Key) {
            self.advance();
            let key = self.expect_identifier()?;
            let action = if self.check(&Token::Equals) {
                self.advance();
                match self.peek() {
                    Some(Token::StringLiteral(s)) => { let a = s; self.advance(); Some(a) }
                    _ => Some(self.expect_identifier()?),
                }
            } else {
                None
            };
            Ok(Statement::SetKeyStmt { key, action })
        } else if self.check(&Token::Window) {
            self.advance();
            let window = if matches!(self.peek(), Some(Token::Identifier(_))) || matches!(self.peek(), Some(Token::StringLiteral(_))) {
                match self.peek() {
                    Some(Token::StringLiteral(s)) => { let n = s; self.advance(); Some(n) }
                    _ => Some(self.expect_identifier()?),
                }
            } else {
                None
            };
            Ok(Statement::SetWindowStmt { window })
        } else {
            // SET GLOBALS or SET CONTROL or SET TIME — skip the rest
            while self.pos < self.tokens.len() {
                match self.peek() {
                    Some(Token::If | Token::For | Token::Repeat | Token::Read | Token::Find
                        | Token::Display | Token::Write | Token::Compute | Token::Move
                        | Token::EndIf | Token::EndFor | Token::EndRepeat) => break,
                    None => break,
                    _ => { self.advance(); }
                }
            }
            Ok(Statement::SetKeyStmt { key: "GLOBALS".into(), action: None })
        }
    }

    fn parse_call_external(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // CALL
        let program = match self.peek() {
            Some(Token::StringLiteral(s)) => { let n = s; self.advance(); n }
            _ => self.expect_identifier()?,
        };
        let args = self.parse_expr_list()?;
        Ok(Statement::CallExternalStmt { program, args })
    }

    fn parse_run(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // RUN
        let program = match self.peek() {
            Some(Token::StringLiteral(s)) => { let n = s; self.advance(); n }
            _ => self.expect_identifier()?,
        };
        Ok(Statement::RunStmt { program })
    }

    fn parse_create_object(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // CREATE
        if self.check(&Token::Object) { self.advance(); }
        let target = self.expect_identifier()?;
        if self.check(&Token::Of) { self.advance(); }
        if matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "CLASS") { self.advance(); }
        let class = match self.peek() {
            Some(Token::StringLiteral(s)) => { let n = s; self.advance(); n }
            _ => self.expect_identifier()?,
        };
        Ok(Statement::CreateObjectStmt { class, target })
    }

    fn parse_send_method(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // SEND
        if self.check(&Token::Method) { self.advance(); }
        let method = match self.peek() {
            Some(Token::StringLiteral(s)) => { let n = s; self.advance(); n }
            _ => self.expect_identifier()?,
        };
        self.expect(&Token::To)?;
        let object = self.expect_identifier()?;
        let args = self.parse_expr_list()?;
        Ok(Statement::SendMethodStmt { object, method, args })
    }

    // -----------------------------------------------------------------------
    // Expression parser with precedence climbing
    // -----------------------------------------------------------------------

    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_and_expr()?;
        while self.check(&Token::Or) {
            self.advance();
            let right = self.parse_and_expr()?;
            left = Expr::BinOp { op: BinOp::Or, left: Box::new(left), right: Box::new(right) };
        }
        Ok(left)
    }

    fn parse_and_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_comparison()?;
        while self.check(&Token::And) {
            self.advance();
            let right = self.parse_comparison()?;
            left = Expr::BinOp { op: BinOp::And, left: Box::new(left), right: Box::new(right) };
        }
        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let left = self.parse_add_sub()?;
        let op = match self.peek() {
            Some(Token::Equals) => Some(BinOp::Eq),
            Some(Token::NotEquals) => Some(BinOp::Ne),
            Some(Token::LessThan) => Some(BinOp::Lt),
            Some(Token::LessEqual) => Some(BinOp::Le),
            Some(Token::GreaterThan) => Some(BinOp::Gt),
            Some(Token::GreaterEqual) => Some(BinOp::Ge),
            _ => None,
        };
        if let Some(op) = op {
            self.advance();
            let right = self.parse_add_sub()?;
            Ok(Expr::BinOp { op, left: Box::new(left), right: Box::new(right) })
        } else {
            Ok(left)
        }
    }

    fn parse_add_sub(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_mul_div()?;
        loop {
            match self.peek() {
                Some(Token::Plus) => { self.advance(); let right = self.parse_mul_div()?; left = Expr::BinOp { op: BinOp::Add, left: Box::new(left), right: Box::new(right) }; }
                Some(Token::Minus) => { self.advance(); let right = self.parse_mul_div()?; left = Expr::BinOp { op: BinOp::Sub, left: Box::new(left), right: Box::new(right) }; }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_mul_div(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_unary()?;
        loop {
            match self.peek() {
                Some(Token::Star) => { self.advance(); let right = self.parse_unary()?; left = Expr::BinOp { op: BinOp::Mul, left: Box::new(left), right: Box::new(right) }; }
                Some(Token::Slash) => { self.advance(); let right = self.parse_unary()?; left = Expr::BinOp { op: BinOp::Div, left: Box::new(left), right: Box::new(right) }; }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if self.check(&Token::Not) {
            self.advance();
            let e = self.parse_primary()?;
            return Ok(Expr::Not(Box::new(e)));
        }
        if self.check(&Token::Minus) {
            self.advance();
            let e = self.parse_primary()?;
            return Ok(Expr::UnaryMinus(Box::new(e)));
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            Some(Token::IntegerLiteral(n)) => { self.advance(); Ok(Expr::IntLit(n)) }
            Some(Token::DecimalLiteral(s)) => { self.advance(); Ok(Expr::DecLit(s)) }
            Some(Token::StringLiteral(s)) => { self.advance(); Ok(Expr::StrLit(s)) }
            Some(Token::True) => { self.advance(); Ok(Expr::BoolLit(true)) }
            Some(Token::False) => { self.advance(); Ok(Expr::BoolLit(false)) }
            Some(Token::HashVariable(v)) => { let name = format!("#{v}"); self.advance(); Ok(Expr::Var(name)) }
            Some(Token::SystemVariable(v)) => { let name = format!("*{v}"); self.advance(); Ok(Expr::SysVar(name)) }
            Some(Token::Identifier(s)) => {
                let name = s;
                self.advance();
                // Check for function call
                if self.check(&Token::LeftParen) {
                    self.advance();
                    let mut args = Vec::new();
                    while !self.check(&Token::RightParen) && self.pos < self.tokens.len() {
                        args.push(self.parse_expression()?);
                        if self.check(&Token::Comma) { self.advance(); }
                    }
                    self.expect_token(&Token::RightParen)?;
                    Ok(Expr::FuncCall { name, args })
                } else {
                    Ok(Expr::Var(name))
                }
            }
            Some(Token::LeftParen) => {
                self.advance();
                let e = self.parse_expression()?;
                self.expect_token(&Token::RightParen)?;
                Ok(e)
            }
            _ => Err(ParseError::UnexpectedEof),
        }
    }

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    fn parse_expr_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut items = Vec::new();
        while self.pos < self.tokens.len() {
            // Stop at statement-ending and block-boundary tokens
            match self.peek() {
                Some(Token::EndIf | Token::EndFor | Token::EndRepeat | Token::EndDecide
                    | Token::EndRead | Token::EndFind | Token::EndHistogram
                    | Token::EndPerform | Token::EndDefine | Token::EndError
                    | Token::EndClass | Token::EndMethod | Token::EndInterface
                    | Token::Eof) => break,
                // Block-boundary keywords
                Some(Token::Else | Token::When | Token::Until | Token::None) => break,
                // Also stop at next statement keywords
                Some(Token::If | Token::Decide | Token::For | Token::Repeat
                    | Token::Read | Token::Find | Token::Histogram | Token::Get
                    | Token::Store | Token::Update | Token::Delete | Token::End
                    | Token::Display | Token::Write | Token::Print | Token::Input
                    | Token::Compute | Token::Move | Token::Compress | Token::Separate
                    | Token::Examine | Token::Sort | Token::Callnat | Token::Fetch
                    | Token::Return | Token::Stack | Token::At | Token::Newpage
                    | Token::Select | Token::Insert | Token::Commit | Token::Rollback
                    | Token::Escape | Token::Perform | Token::Backout | Token::Reinput
                    | Token::Add | Token::Subtract | Token::Multiply | Token::Divide
                    | Token::Reset | Token::Stop | Token::Terminate | Token::Include
                    | Token::Accept | Token::Reject | Token::Set | Token::Call
                    | Token::Run | Token::Create | Token::Send | Token::Define) => break,
                None => break,
                _ => {}
            }
            items.push(self.parse_expression()?);
        }
        Ok(items)
    }

    fn peek(&self) -> Option<Token> {
        self.tokens.get(self.pos).cloned()
    }

    fn peek_at(&self, offset: usize) -> Option<Token> {
        self.tokens.get(self.pos + offset).cloned()
    }

    fn check(&self, expected: &Token) -> bool {
        match (self.peek(), expected) {
            (Some(ref a), b) => std::mem::discriminant(a) == std::mem::discriminant(b),
            _ => false,
        }
    }

    fn check_ahead(&self, offset: usize, expected: &Token) -> bool {
        match (self.peek_at(offset), expected) {
            (Some(ref a), b) => std::mem::discriminant(a) == std::mem::discriminant(b),
            _ => false,
        }
    }

    #[allow(dead_code)]
    fn check_end_break(&self) -> bool {
        matches!(self.peek(), Some(Token::Identifier(ref s)) if s == "END-BREAK")
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    fn expect(&mut self, expected: &Token) -> Result<(), ParseError> {
        if self.check(expected) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::Expected {
                expected: format!("{expected:?}"),
                found: self.peek().unwrap_or(Token::Eof),
            })
        }
    }

    fn expect_token(&mut self, expected: &Token) -> Result<(), ParseError> {
        self.expect(expected)
    }

    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        match self.peek() {
            Some(Token::Identifier(s)) => { self.advance(); Ok(s) }
            Some(Token::HashVariable(v)) => { self.advance(); Ok(format!("#{v}")) }
            Some(other) => Err(ParseError::Expected { expected: "identifier".into(), found: other }),
            None => Err(ParseError::UnexpectedEof),
        }
    }
}

/// Convenience: lex + parse in one call.
pub fn parse_natural(source: &str, name: &str) -> Result<Program, ParseError> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize()?;
    let mut parser = Parser::new(tokens);
    parser.parse_program(name)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_simple_tokens() {
        let mut lexer = Lexer::new("IF #X = 10");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0], Token::If);
        assert!(matches!(tokens[1], Token::HashVariable(ref v) if v == "X"));
        assert_eq!(tokens[2], Token::Equals);
        assert!(matches!(tokens[3], Token::IntegerLiteral(10)));
    }

    #[test]
    fn test_lex_string_literal() {
        let mut lexer = Lexer::new("'hello world'");
        let tokens = lexer.tokenize().unwrap();
        assert!(matches!(tokens[0], Token::StringLiteral(ref s) if s == "hello world"));
    }

    #[test]
    fn test_lex_escaped_apostrophe() {
        let mut lexer = Lexer::new("'it''s'");
        let tokens = lexer.tokenize().unwrap();
        assert!(matches!(tokens[0], Token::StringLiteral(ref s) if s == "it's"));
    }

    #[test]
    fn test_lex_decimal() {
        let mut lexer = Lexer::new("3.14");
        let tokens = lexer.tokenize().unwrap();
        assert!(matches!(tokens[0], Token::DecimalLiteral(ref s) if s == "3.14"));
    }

    #[test]
    fn test_lex_system_variable() {
        let mut lexer = Lexer::new("*DATX");
        let tokens = lexer.tokenize().unwrap();
        assert!(matches!(tokens[0], Token::SystemVariable(ref s) if s == "DATX"));
    }

    #[test]
    fn test_lex_operators() {
        let mut lexer = Lexer::new("< > <= >= <> = + - * /");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0], Token::LessThan);
        assert_eq!(tokens[1], Token::GreaterThan);
        assert_eq!(tokens[2], Token::LessEqual);
        assert_eq!(tokens[3], Token::GreaterEqual);
        assert_eq!(tokens[4], Token::NotEquals);
        assert_eq!(tokens[5], Token::Equals);
    }

    #[test]
    fn test_lex_comment() {
        let mut lexer = Lexer::new("IF /* this is a comment\n#X");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0], Token::If);
    }

    #[test]
    fn test_lex_keywords() {
        let mut lexer = Lexer::new("DEFINE DATA LOCAL END-DEFINE");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0], Token::Define);
        assert_eq!(tokens[1], Token::Data);
        assert_eq!(tokens[2], Token::Local);
        assert_eq!(tokens[3], Token::EndDefine);
    }

    #[test]
    fn test_parse_if_stmt() {
        let src = "IF #X = 1\n  COMPUTE #Y = 2\nEND-IF";
        let prog = parse_natural(src, "TEST").unwrap();
        assert_eq!(prog.statements.len(), 1);
        assert!(matches!(prog.statements[0], Statement::IfStmt { .. }));
    }

    #[test]
    fn test_parse_for_loop() {
        let src = "FOR #I = 1 TO 10\n  DISPLAY #I\nEND-FOR";
        let prog = parse_natural(src, "TEST").unwrap();
        assert!(matches!(prog.statements[0], Statement::ForLoop { .. }));
    }

    #[test]
    fn test_parse_repeat_until() {
        let src = "REPEAT\n  COMPUTE #X = #X + 1\n  UNTIL #X = 10\nEND-REPEAT";
        let prog = parse_natural(src, "TEST").unwrap();
        assert!(matches!(prog.statements[0], Statement::RepeatLoop { .. }));
    }

    #[test]
    fn test_parse_escape() {
        let src = "ESCAPE TOP";
        let prog = parse_natural(src, "TEST").unwrap();
        assert!(matches!(prog.statements[0], Statement::EscapeStmt(EscapeKind::Top)));
    }

    #[test]
    fn test_parse_perform() {
        let src = "PERFORM CALC-TOTAL";
        let prog = parse_natural(src, "TEST").unwrap();
        assert!(matches!(prog.statements[0], Statement::PerformStmt { ref name } if name == "CALC-TOTAL"));
    }

    #[test]
    fn test_parse_read() {
        let src = "READ EMPLOYEES BY NAME\n  DISPLAY #NAME\nEND-READ";
        let prog = parse_natural(src, "TEST").unwrap();
        assert!(matches!(prog.statements[0], Statement::ReadStmt { .. }));
    }

    #[test]
    fn test_parse_find() {
        let src = "FIND EMPLOYEES WITH #DEPT = 'D01'\n  DISPLAY #NAME\nEND-FIND";
        let prog = parse_natural(src, "TEST").unwrap();
        assert!(matches!(prog.statements[0], Statement::FindStmt { .. }));
    }

    #[test]
    fn test_parse_store() {
        let src = "STORE EMPLOYEES";
        let prog = parse_natural(src, "TEST").unwrap();
        assert!(matches!(prog.statements[0], Statement::StoreStmt { .. }));
    }

    #[test]
    fn test_parse_end_transaction() {
        let src = "END TRANSACTION";
        let prog = parse_natural(src, "TEST").unwrap();
        assert!(matches!(prog.statements[0], Statement::EndTransaction));
    }

    #[test]
    fn test_parse_display() {
        let src = "DISPLAY 'Hello' #X";
        let prog = parse_natural(src, "TEST").unwrap();
        assert!(matches!(prog.statements[0], Statement::DisplayStmt { .. }));
    }

    #[test]
    fn test_parse_compute() {
        let src = "COMPUTE #TOTAL = #BASE * 1.1 + #BONUS";
        let prog = parse_natural(src, "TEST").unwrap();
        assert!(matches!(prog.statements[0], Statement::ComputeStmt { .. }));
    }

    #[test]
    fn test_parse_move() {
        let src = "MOVE 'Hello' TO #GREETING";
        let prog = parse_natural(src, "TEST").unwrap();
        assert!(matches!(prog.statements[0], Statement::MoveStmt { .. }));
    }

    #[test]
    fn test_parse_compress() {
        let src = "COMPRESS #FIRST #LAST INTO #FULL LEAVING SPACE";
        let prog = parse_natural(src, "TEST").unwrap();
        if let Statement::CompressStmt { leaving_space, .. } = &prog.statements[0] {
            assert!(leaving_space);
        } else {
            panic!("Expected CompressStmt");
        }
    }

    #[test]
    fn test_parse_callnat() {
        let src = "CALLNAT 'CALC-TAX' #AMOUNT #RATE";
        let prog = parse_natural(src, "TEST").unwrap();
        if let Statement::CallnatStmt { subprogram, args } = &prog.statements[0] {
            assert_eq!(subprogram, "CALC-TAX");
            assert_eq!(args.len(), 2);
        } else {
            panic!("Expected CallnatStmt");
        }
    }

    #[test]
    fn test_parse_fetch() {
        let src = "FETCH 'MAINMENU'";
        let prog = parse_natural(src, "TEST").unwrap();
        if let Statement::FetchStmt { program } = &prog.statements[0] {
            assert_eq!(program, "MAINMENU");
        } else {
            panic!("Expected FetchStmt");
        }
    }

    #[test]
    fn test_parse_stack() {
        let src = "STACK TOP DATA 'val1' 'val2'";
        let prog = parse_natural(src, "TEST").unwrap();
        if let Statement::StackStmt { position, data } = &prog.statements[0] {
            assert_eq!(*position, StackPosition::Top);
            assert_eq!(data.len(), 2);
        } else {
            panic!("Expected StackStmt");
        }
    }

    #[test]
    fn test_parse_expression_precedence() {
        let src = "COMPUTE #R = 1 + 2 * 3";
        let prog = parse_natural(src, "TEST").unwrap();
        if let Statement::ComputeStmt { expr, .. } = &prog.statements[0] {
            // Should parse as 1 + (2 * 3) due to precedence
            assert!(matches!(expr, Expr::BinOp { op: BinOp::Add, .. }));
        } else {
            panic!("Expected ComputeStmt");
        }
    }

    #[test]
    fn test_parse_decide_on() {
        let src = "DECIDE ON FIRST VALUE OF #STATUS\n  WHEN 'A'\n    DISPLAY 'Active'\n  WHEN 'I'\n    DISPLAY 'Inactive'\n  WHEN NONE\n    DISPLAY 'Unknown'\nEND-DECIDE";
        let prog = parse_natural(src, "TEST").unwrap();
        if let Statement::DecideOn { values, none_body, .. } = &prog.statements[0] {
            assert_eq!(values.len(), 2);
            assert_eq!(none_body.len(), 1);
        } else {
            panic!("Expected DecideOn");
        }
    }

    #[test]
    fn test_parse_histogram() {
        let src = "HISTOGRAM EMPLOYEES DEPT\n  DISPLAY DEPT *COUNTER\nEND-HISTOGRAM";
        let prog = parse_natural(src, "TEST").unwrap();
        assert!(matches!(prog.statements[0], Statement::HistogramStmt { .. }));
    }

    #[test]
    fn test_parse_separate() {
        let src = "SEPARATE #FULLNAME INTO #FIRST #LAST WITH DELIMITER ','";
        let prog = parse_natural(src, "TEST").unwrap();
        if let Statement::SeparateStmt { into, delimiter, .. } = &prog.statements[0] {
            assert_eq!(into.len(), 2);
            assert_eq!(delimiter.as_deref(), Some(","));
        } else {
            panic!("Expected SeparateStmt");
        }
    }

    #[test]
    fn test_parse_input_using_map() {
        let src = "INPUT USING MAP 'EMPL-MAP'";
        let prog = parse_natural(src, "TEST").unwrap();
        if let Statement::InputStmt { map_name, .. } = &prog.statements[0] {
            assert_eq!(map_name.as_deref(), Some("EMPL-MAP"));
        } else {
            panic!("Expected InputStmt");
        }
    }

    #[test]
    fn test_parse_select() {
        let src = "SELECT NAME DEPT INTO #NAME #DEPT FROM EMPLOYEES WHERE #DEPT = 'D01'";
        let prog = parse_natural(src, "TEST").unwrap();
        if let Statement::SelectStmt { columns, from, condition, .. } = &prog.statements[0] {
            assert_eq!(columns.len(), 2);
            assert_eq!(from, "EMPLOYEES");
            assert!(condition.is_some());
        } else {
            panic!("Expected SelectStmt");
        }
    }

    #[test]
    fn test_parse_nested_if() {
        let src = "IF #A = 1\n  IF #B = 2\n    DISPLAY 'nested'\n  END-IF\nEND-IF";
        let prog = parse_natural(src, "TEST").unwrap();
        if let Statement::IfStmt { then_body, .. } = &prog.statements[0] {
            assert!(matches!(then_body[0], Statement::IfStmt { .. }));
        } else {
            panic!("Expected nested IfStmt");
        }
    }

    #[test]
    fn test_parse_add() {
        let src = "ADD 5 TO #TOTAL";
        let prog = parse_natural(src, "TEST").unwrap();
        assert!(matches!(prog.statements[0], Statement::AddStmt { .. }));
    }

    #[test]
    fn test_parse_subtract() {
        let src = "SUBTRACT 3 FROM #TOTAL";
        let prog = parse_natural(src, "TEST").unwrap();
        assert!(matches!(prog.statements[0], Statement::SubtractStmt { .. }));
    }

    #[test]
    fn test_parse_multiply() {
        let src = "MULTIPLY 10 BY #A";
        let prog = parse_natural(src, "TEST").unwrap();
        assert!(matches!(prog.statements[0], Statement::MultiplyStmt { .. }));
    }

    #[test]
    fn test_parse_divide() {
        let src = "DIVIDE 100 INTO #RESULT";
        let prog = parse_natural(src, "TEST").unwrap();
        assert!(matches!(prog.statements[0], Statement::DivideStmt { .. }));
    }

    #[test]
    fn test_parse_reset() {
        let src = "RESET #A #B #C";
        let prog = parse_natural(src, "TEST").unwrap();
        if let Statement::ResetStmt { vars } = &prog.statements[0] {
            assert_eq!(vars.len(), 3);
        } else {
            panic!("Expected ResetStmt");
        }
    }

    #[test]
    fn test_parse_stop() {
        let src = "STOP";
        let prog = parse_natural(src, "TEST").unwrap();
        assert!(matches!(prog.statements[0], Statement::StopStmt));
    }

    #[test]
    fn test_parse_terminate() {
        let src = "TERMINATE";
        let prog = parse_natural(src, "TEST").unwrap();
        assert!(matches!(prog.statements[0], Statement::TerminateStmt { .. }));
    }

    #[test]
    fn test_parse_include() {
        let src = "INCLUDE COPYCODE1";
        let prog = parse_natural(src, "TEST").unwrap();
        if let Statement::IncludeStmt { copycode } = &prog.statements[0] {
            assert_eq!(copycode, "COPYCODE1");
        } else {
            panic!("Expected IncludeStmt");
        }
    }
}
