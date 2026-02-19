//! PL/I Parser — builds an AST from a token stream.
//!
//! Handles context-sensitive keyword resolution, DECLARE with full attribute
//! syntax, control flow (IF/DO/SELECT), I/O (GET/PUT/READ/WRITE), ON
//! conditions, and preprocessor directives.

use serde::{Deserialize, Serialize};

use crate::lexer::{is_keyword, Token, TokenKind};

// ---------------------------------------------------------------------------
//  AST nodes
// ---------------------------------------------------------------------------

/// A complete PL/I program (compilation unit).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Program {
    /// Top-level statements (procedures, blocks, declarations).
    pub statements: Vec<Statement>,
}

/// A PL/I statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Statement {
    /// PROCEDURE statement.
    Procedure(ProcedureStmt),
    /// BEGIN block.
    BeginBlock(BeginBlock),
    /// DECLARE / DCL statement.
    Declare(DeclareStmt),
    /// Assignment statement.
    Assignment(AssignmentStmt),
    /// IF / THEN / ELSE statement.
    If(IfStmt),
    /// DO block (iterative, while, until, simple).
    Do(DoStmt),
    /// SELECT / WHEN / OTHERWISE / END.
    Select(SelectStmt),
    /// CALL statement.
    Call(CallStmt),
    /// RETURN statement.
    Return(ReturnStmt),
    /// GO TO statement.
    GoTo(GoToStmt),
    /// LEAVE statement (exit loop).
    Leave(LeaveStmt),
    /// ITERATE statement (next iteration).
    Iterate(IterateStmt),
    /// PUT statement (stream I/O output).
    Put(PutStmt),
    /// GET statement (stream I/O input).
    Get(GetStmt),
    /// OPEN statement.
    Open(OpenStmt),
    /// CLOSE statement.
    Close(CloseStmt),
    /// READ statement (record I/O).
    Read(ReadStmt),
    /// WRITE statement (record I/O).
    Write(WriteStmt),
    /// ON statement (exception handling).
    On(OnStmt),
    /// SIGNAL statement.
    Signal(SignalStmt),
    /// REVERT statement.
    Revert(RevertStmt),
    /// ALLOCATE statement.
    Allocate(AllocateStmt),
    /// FREE statement.
    Free(FreeStmt),
    /// DISPLAY statement.
    Display(DisplayStmt),
    /// STOP statement.
    Stop,
    /// EXIT statement.
    Exit,
    /// END statement.
    End(Option<String>),
    /// Null statement (just a semicolon).
    Null,
    /// Labeled statement (label: stmt).
    Labeled(String, Box<Statement>),
    /// Preprocessor directive (%IF, %INCLUDE, etc.).
    Preprocessor(PreprocessorDirective),
}

// ---------------------------------------------------------------------------
//  PROCEDURE
// ---------------------------------------------------------------------------

/// PROCEDURE / PROC declaration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcedureStmt {
    /// Procedure label name.
    pub name: String,
    /// Parameter names.
    pub params: Vec<String>,
    /// OPTIONS (e.g., MAIN, REENTRANT).
    pub options: Vec<String>,
    /// RETURNS type (if any).
    pub returns: Option<DataType>,
    /// Body statements.
    pub body: Vec<Statement>,
}

/// BEGIN ... END block.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BeginBlock {
    /// Optional label.
    pub label: Option<String>,
    /// Body statements.
    pub body: Vec<Statement>,
}

// ---------------------------------------------------------------------------
//  DECLARE
// ---------------------------------------------------------------------------

/// A DECLARE (DCL) statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeclareStmt {
    /// Declared items.
    pub items: Vec<DeclareItem>,
}

/// A single item within a DECLARE statement.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DeclareItem {
    /// Level number (1-15, 0 if scalar).
    pub level: u8,
    /// Variable name.
    pub name: String,
    /// Data type and attributes.
    pub data_type: Option<DataType>,
    /// Storage class.
    pub storage: Option<StorageClass>,
    /// Dimension (array bounds).
    pub dimension: Option<Vec<Dimension>>,
    /// INITIAL / INIT value.
    pub initial: Option<Expr>,
    /// LIKE reference.
    pub like: Option<String>,
    /// BASED(pointer) reference.
    pub based: Option<String>,
    /// DEFINED(target) reference.
    pub defined: Option<String>,
    /// ALIGNED / UNALIGNED.
    pub aligned: Option<bool>,
}


/// Array dimension.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dimension {
    /// Lower bound.
    pub lower: Expr,
    /// Upper bound.
    pub upper: Expr,
}

/// PL/I data types.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DataType {
    /// FIXED DECIMAL(precision, scale).
    FixedDecimal(u8, u8),
    /// FIXED BINARY(precision, scale).
    FixedBinary(u8, u8),
    /// FLOAT DECIMAL(precision).
    FloatDecimal(u8),
    /// FLOAT BINARY(precision).
    FloatBinary(u8),
    /// CHARACTER(length).
    Character(u32),
    /// CHARACTER(length) VARYING.
    CharacterVarying(u32),
    /// CHARACTER(*).
    CharacterStar,
    /// BIT(length).
    Bit(u32),
    /// BIT(length) VARYING.
    BitVarying(u32),
    /// GRAPHIC(length).
    Graphic(u32),
    /// WIDECHAR(length).
    Widechar(u32),
    /// POINTER.
    Pointer,
    /// OFFSET.
    Offset,
    /// HANDLE(structure-name).
    Handle(String),
    /// AREA(size).
    Area(u32),
    /// LABEL.
    Label,
    /// ENTRY.
    Entry,
    /// FILE.
    File,
    /// FORMAT.
    Format,
    /// TASK.
    Task,
    /// EVENT.
    Event,
    /// PICTURE 'spec'.
    Picture(String),
    /// ORDINAL (V5+).
    Ordinal(String),
    /// UNION.
    Union,
    /// Structure (group — has members at deeper levels).
    Structure,
}

/// Storage classes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum StorageClass {
    Automatic,
    Static,
    Controlled,
    Based,
    Defined,
    Parameter,
}

// ---------------------------------------------------------------------------
//  Expressions
// ---------------------------------------------------------------------------

/// PL/I expression.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expr {
    /// Integer literal.
    IntLit(i64),
    /// Decimal literal.
    DecLit(String),
    /// Float literal.
    FloatLit(String),
    /// String literal.
    StringLit(String),
    /// Bit literal.
    BitLit(String),
    /// Hex literal.
    HexLit(String),
    /// Variable / identifier reference.
    Ident(String),
    /// Binary operation.
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    /// Unary operation.
    UnaryOp(UnaryOp, Box<Expr>),
    /// Function call / built-in.
    FuncCall(String, Vec<Expr>),
    /// Array subscript / structure qualification.
    Subscript(Box<Expr>, Vec<Expr>),
    /// Pointer qualification (expr->field).
    PtrQual(Box<Expr>, String),
    /// Parenthesized expression.
    Paren(Box<Expr>),
}

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Power,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
    Concat,
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    Neg,
    Pos,
    Not,
}

// ---------------------------------------------------------------------------
//  Control flow
// ---------------------------------------------------------------------------

/// Assignment statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AssignmentStmt {
    pub target: Expr,
    pub value: Expr,
}

/// IF / THEN / ELSE.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_stmt: Box<Statement>,
    pub else_stmt: Option<Box<Statement>>,
}

/// DO block.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DoStmt {
    /// Optional label.
    pub label: Option<String>,
    /// Loop control (None = simple DO).
    pub control: Option<DoControl>,
    /// Body statements.
    pub body: Vec<Statement>,
}

/// DO loop control variants.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DoControl {
    /// DO WHILE(condition).
    While(Expr),
    /// DO UNTIL(condition).
    Until(Expr),
    /// DO var = start TO end [BY step] [WHILE(cond)].
    Iterative {
        var: String,
        from: Expr,
        to: Expr,
        by: Option<Expr>,
        while_cond: Option<Expr>,
    },
    /// DO REPEAT(expr).
    Repeat(Expr),
}

/// SELECT statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SelectStmt {
    /// SELECT expression (None = SELECT without expression).
    pub expr: Option<Expr>,
    /// WHEN clauses.
    pub when_clauses: Vec<WhenClause>,
    /// OTHERWISE clause.
    pub otherwise: Option<Vec<Statement>>,
}

/// A WHEN clause in a SELECT.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WhenClause {
    /// Match values (can be multiple).
    pub values: Vec<Expr>,
    /// Action statement(s).
    pub body: Vec<Statement>,
}

/// CALL statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CallStmt {
    pub name: String,
    pub args: Vec<Expr>,
}

/// RETURN statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
}

/// GO TO statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GoToStmt {
    pub label: String,
}

/// LEAVE statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LeaveStmt {
    pub label: Option<String>,
}

/// ITERATE statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IterateStmt {
    pub label: Option<String>,
}

// ---------------------------------------------------------------------------
//  I/O
// ---------------------------------------------------------------------------

/// I/O mode for GET/PUT.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum IoMode {
    /// LIST-directed.
    List,
    /// EDIT-directed with format items.
    Edit,
    /// DATA-directed.
    Data,
}

/// PUT statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PutStmt {
    /// Optional file reference.
    pub file: Option<String>,
    /// I/O mode.
    pub mode: IoMode,
    /// Expressions to output.
    pub items: Vec<Expr>,
    /// SKIP(n).
    pub skip: Option<Expr>,
    /// PAGE.
    pub page: bool,
    /// LINE(n).
    pub line: Option<Expr>,
}

/// GET statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetStmt {
    /// Optional file reference.
    pub file: Option<String>,
    /// I/O mode.
    pub mode: IoMode,
    /// Target variables.
    pub items: Vec<Expr>,
}

/// OPEN statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OpenStmt {
    pub file: String,
    pub attributes: Vec<String>,
}

/// CLOSE statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CloseStmt {
    pub file: String,
}

/// READ statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReadStmt {
    pub file: String,
    pub into: Option<String>,
    pub key: Option<Expr>,
}

/// WRITE statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WriteStmt {
    pub file: String,
    pub from: Option<String>,
    pub keyfrom: Option<Expr>,
}

// ---------------------------------------------------------------------------
//  Exception handling
// ---------------------------------------------------------------------------

/// ON statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OnStmt {
    /// ON condition name.
    pub condition: OnCondition,
    /// Action (SNAP, SYSTEM, or a statement).
    pub action: Box<Statement>,
}

/// ON conditions.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OnCondition {
    Conversion,
    Endfile(Option<String>),
    Endpage(Option<String>),
    Error,
    Finish,
    Fixedoverflow,
    Overflow,
    Underflow,
    Zerodivide,
    Size,
    Stringrange,
    Stringsize,
    Subscriptrange,
    Invalidop,
    Key(Option<String>),
    Record(Option<String>),
    Transmit(Option<String>),
    Undefinedfile(Option<String>),
    Name(Option<String>),
    Area,
    Attention,
    Storage,
    /// User-defined condition: CONDITION(name).
    Condition(String),
}

/// SIGNAL statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SignalStmt {
    pub condition: OnCondition,
}

/// REVERT statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RevertStmt {
    pub condition: OnCondition,
}

// ---------------------------------------------------------------------------
//  Storage management
// ---------------------------------------------------------------------------

/// ALLOCATE statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AllocateStmt {
    pub name: String,
    pub set: Option<String>,
}

/// FREE statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FreeStmt {
    pub name: String,
}

/// DISPLAY statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DisplayStmt {
    pub expr: Expr,
}

// ---------------------------------------------------------------------------
//  Preprocessor
// ---------------------------------------------------------------------------

/// Preprocessor directives.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PreprocessorDirective {
    /// %DECLARE / %DCL.
    Declare(String, String),
    /// %assignment.
    Assign(String, Expr),
    /// %IF ... %THEN.
    If {
        condition: Expr,
        then_body: Vec<Statement>,
        else_body: Option<Vec<Statement>>,
    },
    /// %INCLUDE member.
    Include(String),
    /// %XINCLUDE member (include once).
    XInclude(String),
    /// %NOTE message.
    Note(String),
    /// %ACTIVATE / %DEACTIVATE.
    Activate(String, bool),
    /// %PROCEDURE.
    Procedure {
        name: String,
        params: Vec<String>,
        body: Vec<Statement>,
    },
}

// ---------------------------------------------------------------------------
//  Parser errors
// ---------------------------------------------------------------------------

/// Errors from the PL/I parser.
#[derive(Debug, Clone, thiserror::Error)]
pub enum ParseError {
    /// Unexpected token.
    #[error("unexpected {found:?} at line {line}:{col}, expected {expected}")]
    Unexpected {
        expected: String,
        found: TokenKind,
        line: usize,
        col: usize,
    },

    /// Unexpected end of input.
    #[error("unexpected end of input, expected {expected}")]
    UnexpectedEof { expected: String },
}

// ---------------------------------------------------------------------------
//  Parser
// ---------------------------------------------------------------------------

/// PL/I parser — builds AST from token stream.
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    eof_sentinel: Token,
}

impl Parser {
    /// Create a new parser from a token stream.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            eof_sentinel: Token {
                kind: TokenKind::Eof,
                text: String::new(),
                line: 0,
                col: 0,
            },
        }
    }

    /// Parse into a Program.
    pub fn parse(tokens: Vec<Token>) -> Result<Program, ParseError> {
        let mut parser = Self::new(tokens);
        let mut stmts = Vec::new();
        while !parser.at_eof() {
            stmts.push(parser.parse_statement()?);
        }
        Ok(Program { statements: stmts })
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&self.eof_sentinel)
    }

    fn advance(&mut self) -> &Token {
        let tok_idx = self.pos;
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
        self.tokens.get(tok_idx).unwrap_or(&self.eof_sentinel)
    }

    fn at_eof(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        let tok = self.peek().clone();
        if tok.kind == kind {
            self.advance();
            Ok(tok)
        } else if tok.kind == TokenKind::Eof {
            Err(ParseError::UnexpectedEof {
                expected: format!("{:?}", kind),
            })
        } else {
            Err(ParseError::Unexpected {
                expected: format!("{:?}", kind),
                found: tok.kind,
                line: tok.line,
                col: tok.col,
            })
        }
    }

    fn expect_ident(&mut self) -> Result<String, ParseError> {
        let tok = self.expect(TokenKind::Ident)?;
        Ok(tok.text)
    }

    fn check_keyword(&self, kw: &str) -> bool {
        self.peek().kind == TokenKind::Ident && is_keyword(&self.peek().text, kw)
    }

    fn eat_keyword(&mut self, kw: &str) -> bool {
        if self.check_keyword(kw) {
            self.advance();
            true
        } else {
            false
        }
    }

    // ─────── Statement parsing ───────

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        // Check for preprocessor.
        if self.peek().kind == TokenKind::Percent {
            return self.parse_preprocessor();
        }

        // Check for null statement.
        if self.peek().kind == TokenKind::Semi {
            self.advance();
            return Ok(Statement::Null);
        }

        // Check for label: stmt.
        if self.peek().kind == TokenKind::Ident
            && self.pos + 1 < self.tokens.len()
            && self.tokens[self.pos + 1].kind == TokenKind::Colon
        {
            let label = self.peek().text.clone();
            self.advance(); // label
            self.advance(); // :

            // Check for PROC/PROCEDURE.
            if self.check_keyword("PROC") || self.check_keyword("PROCEDURE") {
                return self.parse_procedure(label);
            }

            let inner = self.parse_statement()?;
            return Ok(Statement::Labeled(label, Box::new(inner)));
        }

        // Keyword-driven statements.
        if self.peek().kind == TokenKind::Ident {
            let kw = self.peek().text.to_uppercase();
            match kw.as_str() {
                "DECLARE" | "DCL" => return self.parse_declare(),
                "IF" => return self.parse_if(),
                "DO" => return self.parse_do(None),
                "SELECT" => return self.parse_select(),
                "CALL" => return self.parse_call(),
                "RETURN" => return self.parse_return(),
                "GO" | "GOTO" => return self.parse_goto(),
                "LEAVE" => return self.parse_leave(),
                "ITERATE" => return self.parse_iterate(),
                "PUT" => return self.parse_put(),
                "GET" => return self.parse_get(),
                "OPEN" => return self.parse_open(),
                "CLOSE" => return self.parse_close(),
                "READ" => return self.parse_read(),
                "WRITE" => return self.parse_write(),
                "ON" => return self.parse_on(),
                "SIGNAL" => return self.parse_signal(),
                "REVERT" => return self.parse_revert(),
                "ALLOCATE" => return self.parse_allocate(),
                "FREE" => return self.parse_free(),
                "DISPLAY" => return self.parse_display(),
                "BEGIN" => return self.parse_begin(None),
                "END" => return self.parse_end(),
                "STOP" => {
                    self.advance();
                    self.expect(TokenKind::Semi)?;
                    return Ok(Statement::Stop);
                }
                "EXIT" => {
                    self.advance();
                    self.expect(TokenKind::Semi)?;
                    return Ok(Statement::Exit);
                }
                _ => {}
            }
        }

        // Default: assignment statement.
        self.parse_assignment()
    }

    fn parse_procedure(&mut self, name: String) -> Result<Statement, ParseError> {
        self.advance(); // PROC/PROCEDURE

        let mut params = Vec::new();
        if self.peek().kind == TokenKind::LParen {
            self.advance();
            while self.peek().kind != TokenKind::RParen && !self.at_eof() {
                params.push(self.expect_ident()?);
                if self.peek().kind == TokenKind::Comma {
                    self.advance();
                }
            }
            self.expect(TokenKind::RParen)?;
        }

        let mut options = Vec::new();
        if self.check_keyword("OPTIONS") {
            self.advance();
            self.expect(TokenKind::LParen)?;
            while self.peek().kind != TokenKind::RParen && !self.at_eof() {
                options.push(self.expect_ident()?);
                if self.peek().kind == TokenKind::Comma {
                    self.advance();
                }
            }
            self.expect(TokenKind::RParen)?;
        }

        let mut returns = None;
        if self.check_keyword("RETURNS") {
            self.advance();
            self.expect(TokenKind::LParen)?;
            returns = Some(self.parse_data_type()?);
            self.expect(TokenKind::RParen)?;
        }

        self.expect(TokenKind::Semi)?;

        let mut body = Vec::new();
        while !self.check_keyword("END") && !self.at_eof() {
            body.push(self.parse_statement()?);
        }

        // END [name];
        self.eat_keyword("END");
        if self.peek().kind == TokenKind::Ident {
            self.advance(); // optional name
        }
        self.expect(TokenKind::Semi)?;

        Ok(Statement::Procedure(ProcedureStmt {
            name,
            params,
            options,
            returns,
            body,
        }))
    }

    fn parse_begin(&mut self, label: Option<String>) -> Result<Statement, ParseError> {
        self.advance(); // BEGIN
        self.expect(TokenKind::Semi)?;

        let mut body = Vec::new();
        while !self.check_keyword("END") && !self.at_eof() {
            body.push(self.parse_statement()?);
        }
        self.eat_keyword("END");
        if self.peek().kind == TokenKind::Ident {
            self.advance();
        }
        self.expect(TokenKind::Semi)?;

        Ok(Statement::BeginBlock(BeginBlock { label, body }))
    }

    fn parse_end(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // END
        let name = if self.peek().kind == TokenKind::Ident {
            Some(self.advance().text.clone())
        } else {
            None
        };
        self.expect(TokenKind::Semi)?;
        Ok(Statement::End(name))
    }

    // ─────── DECLARE ───────

    fn parse_declare(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // DECLARE/DCL
        let mut items = Vec::new();

        loop {
            items.push(self.parse_declare_item()?);
            if self.peek().kind == TokenKind::Comma {
                self.advance();
            } else {
                break;
            }
        }
        self.expect(TokenKind::Semi)?;

        Ok(Statement::Declare(DeclareStmt { items }))
    }

    fn parse_declare_item(&mut self) -> Result<DeclareItem, ParseError> {
        let mut item = DeclareItem::default();

        // Optional level number.
        if self.peek().kind == TokenKind::IntLit {
            let level: u8 = self.peek().text.parse().unwrap_or(0);
            if (1..=15).contains(&level) {
                item.level = level;
                self.advance();
            }
        }

        // Variable name.
        item.name = self.expect_ident()?;

        // Parse attributes until we hit a comma, semicolon, or end.
        while self.peek().kind == TokenKind::Ident || self.peek().kind == TokenKind::LParen {
            if self.peek().kind != TokenKind::Ident {
                break;
            }
            let attr_upper = self.peek().text.to_uppercase();
            match attr_upper.as_str() {
                "FIXED" | "FLOAT" | "CHAR" | "CHARACTER" | "BIT" | "GRAPHIC"
                | "WIDECHAR" | "POINTER" | "PTR" | "OFFSET" | "HANDLE" | "AREA"
                | "LABEL" | "ENTRY" | "FILE" | "FORMAT" | "TASK" | "EVENT"
                | "PICTURE" | "PIC" | "ORDINAL" | "UNION" | "DECIMAL" | "DEC"
                | "BINARY" | "BIN" => {
                    item.data_type = Some(self.parse_data_type()?);
                }
                "VARYING" | "VAR" => {
                    self.advance();
                    // Upgrade current type to varying.
                    if let Some(ref mut dt) = item.data_type {
                        *dt = match dt {
                            DataType::Character(n) => DataType::CharacterVarying(*n),
                            DataType::Bit(n) => DataType::BitVarying(*n),
                            _ => dt.clone(),
                        };
                    }
                }
                "AUTOMATIC" | "AUTO" => {
                    self.advance();
                    item.storage = Some(StorageClass::Automatic);
                }
                "STATIC" => {
                    self.advance();
                    item.storage = Some(StorageClass::Static);
                }
                "CONTROLLED" | "CTL" => {
                    self.advance();
                    item.storage = Some(StorageClass::Controlled);
                }
                "BASED" => {
                    self.advance();
                    if self.peek().kind == TokenKind::LParen {
                        self.advance();
                        item.based = Some(self.expect_ident()?);
                        self.expect(TokenKind::RParen)?;
                    }
                    item.storage = Some(StorageClass::Based);
                }
                "DEFINED" | "DEF" => {
                    self.advance();
                    if self.peek().kind == TokenKind::LParen {
                        self.advance();
                        item.defined = Some(self.expect_ident()?);
                        self.expect(TokenKind::RParen)?;
                    }
                    item.storage = Some(StorageClass::Defined);
                }
                "PARAMETER" | "PARM" => {
                    self.advance();
                    item.storage = Some(StorageClass::Parameter);
                }
                "DIMENSION" | "DIM" => {
                    self.advance();
                    item.dimension = Some(self.parse_dimensions()?);
                }
                "INITIAL" | "INIT" => {
                    self.advance();
                    self.expect(TokenKind::LParen)?;
                    item.initial = Some(self.parse_expr()?);
                    self.expect(TokenKind::RParen)?;
                }
                "LIKE" => {
                    self.advance();
                    item.like = Some(self.expect_ident()?);
                }
                "ALIGNED" => {
                    self.advance();
                    item.aligned = Some(true);
                }
                "UNALIGNED" => {
                    self.advance();
                    item.aligned = Some(false);
                }
                _ => break,
            }
        }

        // Check for dimension after name without keyword.
        if item.dimension.is_none() && self.peek().kind == TokenKind::LParen && item.data_type.is_none() {
            // Could be dimension; skip for now — parser is simplified.
        }

        Ok(item)
    }

    fn parse_data_type(&mut self) -> Result<DataType, ParseError> {
        let kw = self.peek().text.to_uppercase();
        self.advance();

        match kw.as_str() {
            "FIXED" => {
                let base = if self.check_keyword("DECIMAL") || self.check_keyword("DEC") {
                    self.advance();
                    "DEC"
                } else if self.check_keyword("BINARY") || self.check_keyword("BIN") {
                    self.advance();
                    "BIN"
                } else {
                    "DEC" // default
                };
                let (p, q) = self.parse_precision()?;
                if base == "BIN" {
                    Ok(DataType::FixedBinary(p, q))
                } else {
                    Ok(DataType::FixedDecimal(p, q))
                }
            }
            "DECIMAL" | "DEC" => {
                let (p, q) = self.parse_precision()?;
                Ok(DataType::FixedDecimal(p, q))
            }
            "BINARY" | "BIN" => {
                let (p, q) = self.parse_precision()?;
                Ok(DataType::FixedBinary(p, q))
            }
            "FLOAT" => {
                let base = if self.check_keyword("DECIMAL") || self.check_keyword("DEC") {
                    self.advance();
                    "DEC"
                } else if self.check_keyword("BINARY") || self.check_keyword("BIN") {
                    self.advance();
                    "BIN"
                } else {
                    "DEC"
                };
                let p = self.parse_single_precision()?;
                if base == "BIN" {
                    Ok(DataType::FloatBinary(p))
                } else {
                    Ok(DataType::FloatDecimal(p))
                }
            }
            "CHAR" | "CHARACTER" => {
                let n = self.parse_length()?;
                if self.check_keyword("VARYING") || self.check_keyword("VAR") {
                    self.advance();
                    Ok(DataType::CharacterVarying(n))
                } else {
                    Ok(DataType::Character(n))
                }
            }
            "BIT" => {
                let n = self.parse_length()?;
                if self.check_keyword("VARYING") || self.check_keyword("VAR") {
                    self.advance();
                    Ok(DataType::BitVarying(n))
                } else {
                    Ok(DataType::Bit(n))
                }
            }
            "GRAPHIC" => {
                let n = self.parse_length()?;
                Ok(DataType::Graphic(n))
            }
            "WIDECHAR" => {
                let n = self.parse_length()?;
                Ok(DataType::Widechar(n))
            }
            "POINTER" | "PTR" => Ok(DataType::Pointer),
            "OFFSET" => Ok(DataType::Offset),
            "HANDLE" => {
                self.expect(TokenKind::LParen)?;
                let name = self.expect_ident()?;
                self.expect(TokenKind::RParen)?;
                Ok(DataType::Handle(name))
            }
            "AREA" => {
                let n = self.parse_length()?;
                Ok(DataType::Area(n))
            }
            "LABEL" => Ok(DataType::Label),
            "ENTRY" => Ok(DataType::Entry),
            "FILE" => Ok(DataType::File),
            "FORMAT" => Ok(DataType::Format),
            "TASK" => Ok(DataType::Task),
            "EVENT" => Ok(DataType::Event),
            "PICTURE" | "PIC" => {
                let spec = if self.peek().kind == TokenKind::StringLit {
                    let s = self.advance().text.clone();
                    s
                } else {
                    String::new()
                };
                Ok(DataType::Picture(spec))
            }
            "ORDINAL" => {
                let name = if self.peek().kind == TokenKind::Ident {
                    self.advance().text.clone()
                } else {
                    String::new()
                };
                Ok(DataType::Ordinal(name))
            }
            "UNION" => Ok(DataType::Union),
            _ => Ok(DataType::FixedDecimal(15, 0)), // fallback
        }
    }

    fn parse_precision(&mut self) -> Result<(u8, u8), ParseError> {
        if self.peek().kind == TokenKind::LParen {
            self.advance();
            let p = self.parse_int_value()?;
            let q = if self.peek().kind == TokenKind::Comma {
                self.advance();
                self.parse_int_value()?
            } else {
                0
            };
            self.expect(TokenKind::RParen)?;
            Ok((p as u8, q as u8))
        } else {
            Ok((15, 0))
        }
    }

    fn parse_single_precision(&mut self) -> Result<u8, ParseError> {
        if self.peek().kind == TokenKind::LParen {
            self.advance();
            let p = self.parse_int_value()?;
            self.expect(TokenKind::RParen)?;
            Ok(p as u8)
        } else {
            Ok(6)
        }
    }

    fn parse_length(&mut self) -> Result<u32, ParseError> {
        if self.peek().kind == TokenKind::LParen {
            self.advance();
            if self.peek().kind == TokenKind::Star {
                self.advance();
                self.expect(TokenKind::RParen)?;
                return Ok(0); // * = adjustable
            }
            let n = self.parse_int_value()?;
            self.expect(TokenKind::RParen)?;
            Ok(n as u32)
        } else {
            Ok(1)
        }
    }

    fn parse_dimensions(&mut self) -> Result<Vec<Dimension>, ParseError> {
        self.expect(TokenKind::LParen)?;
        let mut dims = Vec::new();
        loop {
            let lower = self.parse_expr()?;
            let upper = if self.peek().kind == TokenKind::Colon {
                self.advance();
                self.parse_expr()?
            } else {
                lower.clone()
            };
            dims.push(Dimension {
                lower: Expr::IntLit(1),
                upper,
            });
            if self.peek().kind == TokenKind::Comma {
                self.advance();
            } else {
                break;
            }
        }
        self.expect(TokenKind::RParen)?;
        Ok(dims)
    }

    fn parse_int_value(&mut self) -> Result<i64, ParseError> {
        let tok = self.expect(TokenKind::IntLit)?;
        Ok(tok.text.parse::<i64>().unwrap_or(0))
    }

    // ─────── Control flow ───────

    fn parse_if(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // IF
        let condition = self.parse_expr()?;
        if !self.eat_keyword("THEN") {
            return Err(ParseError::Unexpected {
                expected: "THEN".to_string(),
                found: self.peek().kind.clone(),
                line: self.peek().line,
                col: self.peek().col,
            });
        }
        let then_stmt = Box::new(self.parse_statement()?);
        let else_stmt = if self.check_keyword("ELSE") {
            self.advance();
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };
        Ok(Statement::If(IfStmt {
            condition,
            then_stmt,
            else_stmt,
        }))
    }

    fn parse_do(&mut self, label: Option<String>) -> Result<Statement, ParseError> {
        self.advance(); // DO

        let control = if self.check_keyword("WHILE") {
            self.advance();
            self.expect(TokenKind::LParen)?;
            let cond = self.parse_expr()?;
            self.expect(TokenKind::RParen)?;
            Some(DoControl::While(cond))
        } else if self.check_keyword("UNTIL") {
            self.advance();
            self.expect(TokenKind::LParen)?;
            let cond = self.parse_expr()?;
            self.expect(TokenKind::RParen)?;
            Some(DoControl::Until(cond))
        } else if self.peek().kind == TokenKind::Ident
            && self.pos + 1 < self.tokens.len()
            && self.tokens[self.pos + 1].kind == TokenKind::Eq
            && !self.check_keyword("END")
        {
            let var = self.expect_ident()?;
            self.expect(TokenKind::Eq)?;
            let from = self.parse_expr()?;
            if !self.eat_keyword("TO") {
                return Err(ParseError::Unexpected {
                    expected: "TO".to_string(),
                    found: self.peek().kind.clone(),
                    line: self.peek().line,
                    col: self.peek().col,
                });
            }
            let to = self.parse_expr()?;
            let by = if self.check_keyword("BY") {
                self.advance();
                Some(self.parse_expr()?)
            } else {
                None
            };
            let while_cond = if self.check_keyword("WHILE") {
                self.advance();
                self.expect(TokenKind::LParen)?;
                let c = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                Some(c)
            } else {
                None
            };
            Some(DoControl::Iterative {
                var,
                from,
                to,
                by,
                while_cond,
            })
        } else {
            None
        };

        self.expect(TokenKind::Semi)?;

        let mut body = Vec::new();
        while !self.check_keyword("END") && !self.at_eof() {
            body.push(self.parse_statement()?);
        }
        self.eat_keyword("END");
        if self.peek().kind == TokenKind::Ident {
            self.advance();
        }
        self.expect(TokenKind::Semi)?;

        Ok(Statement::Do(DoStmt {
            label,
            control,
            body,
        }))
    }

    fn parse_select(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // SELECT
        let expr = if self.peek().kind == TokenKind::LParen {
            self.advance();
            let e = self.parse_expr()?;
            self.expect(TokenKind::RParen)?;
            Some(e)
        } else {
            None
        };
        self.expect(TokenKind::Semi)?;

        let mut when_clauses = Vec::new();
        let mut otherwise = None;

        while !self.check_keyword("END") && !self.at_eof() {
            if self.check_keyword("WHEN") {
                self.advance();
                self.expect(TokenKind::LParen)?;
                let mut values = Vec::new();
                loop {
                    values.push(self.parse_expr()?);
                    if self.peek().kind == TokenKind::Comma {
                        self.advance();
                    } else {
                        break;
                    }
                }
                self.expect(TokenKind::RParen)?;
                let stmt = self.parse_statement()?;
                when_clauses.push(WhenClause {
                    values,
                    body: vec![stmt],
                });
            } else if self.check_keyword("OTHERWISE") || self.check_keyword("OTHER") {
                self.advance();
                let stmt = self.parse_statement()?;
                otherwise = Some(vec![stmt]);
            } else {
                break;
            }
        }

        self.eat_keyword("END");
        self.expect(TokenKind::Semi)?;

        Ok(Statement::Select(SelectStmt {
            expr,
            when_clauses,
            otherwise,
        }))
    }

    fn parse_call(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // CALL
        let name = self.expect_ident()?;
        let mut args = Vec::new();
        if self.peek().kind == TokenKind::LParen {
            self.advance();
            while self.peek().kind != TokenKind::RParen && !self.at_eof() {
                args.push(self.parse_expr()?);
                if self.peek().kind == TokenKind::Comma {
                    self.advance();
                }
            }
            self.expect(TokenKind::RParen)?;
        }
        self.expect(TokenKind::Semi)?;
        Ok(Statement::Call(CallStmt { name, args }))
    }

    fn parse_return(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // RETURN
        let value = if self.peek().kind == TokenKind::LParen {
            self.advance();
            let e = self.parse_expr()?;
            self.expect(TokenKind::RParen)?;
            Some(e)
        } else if self.peek().kind != TokenKind::Semi {
            Some(self.parse_expr()?)
        } else {
            None
        };
        self.expect(TokenKind::Semi)?;
        Ok(Statement::Return(ReturnStmt { value }))
    }

    fn parse_goto(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // GO or GOTO
        if self.check_keyword("TO") {
            self.advance();
        }
        let label = self.expect_ident()?;
        self.expect(TokenKind::Semi)?;
        Ok(Statement::GoTo(GoToStmt { label }))
    }

    fn parse_leave(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // LEAVE
        let label = if self.peek().kind == TokenKind::Ident && !self.at_eof() && self.peek().kind != TokenKind::Semi {
            Some(self.expect_ident()?)
        } else {
            None
        };
        self.expect(TokenKind::Semi)?;
        Ok(Statement::Leave(LeaveStmt { label }))
    }

    fn parse_iterate(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // ITERATE
        let label = if self.peek().kind == TokenKind::Ident {
            Some(self.expect_ident()?)
        } else {
            None
        };
        self.expect(TokenKind::Semi)?;
        Ok(Statement::Iterate(IterateStmt { label }))
    }

    // ─────── I/O ───────

    fn parse_put(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // PUT
        let mut file = None;
        let mut mode = IoMode::List;
        let mut items = Vec::new();
        let mut skip = None;
        let mut page = false;
        let mut line_num = None;

        if self.check_keyword("FILE") {
            self.advance();
            self.expect(TokenKind::LParen)?;
            file = Some(self.expect_ident()?);
            self.expect(TokenKind::RParen)?;
        }

        if self.check_keyword("SKIP") {
            self.advance();
            skip = if self.peek().kind == TokenKind::LParen {
                self.advance();
                let e = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                Some(e)
            } else {
                Some(Expr::IntLit(1))
            };
        }

        if self.check_keyword("PAGE") {
            self.advance();
            page = true;
        }

        if self.check_keyword("LINE") {
            self.advance();
            self.expect(TokenKind::LParen)?;
            line_num = Some(self.parse_expr()?);
            self.expect(TokenKind::RParen)?;
        }

        if self.check_keyword("LIST") {
            self.advance();
            mode = IoMode::List;
            self.expect(TokenKind::LParen)?;
            while self.peek().kind != TokenKind::RParen && !self.at_eof() {
                items.push(self.parse_expr()?);
                if self.peek().kind == TokenKind::Comma {
                    self.advance();
                }
            }
            self.expect(TokenKind::RParen)?;
        } else if self.check_keyword("EDIT") {
            self.advance();
            mode = IoMode::Edit;
            // Simplified: just consume until semicolon.
            self.expect(TokenKind::LParen)?;
            while self.peek().kind != TokenKind::RParen && !self.at_eof() {
                items.push(self.parse_expr()?);
                if self.peek().kind == TokenKind::Comma {
                    self.advance();
                }
            }
            self.expect(TokenKind::RParen)?;
        } else if self.check_keyword("DATA") {
            self.advance();
            mode = IoMode::Data;
        }

        // Possible trailing SKIP after items.
        if self.check_keyword("SKIP") {
            self.advance();
            skip = if self.peek().kind == TokenKind::LParen {
                self.advance();
                let e = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                Some(e)
            } else {
                Some(Expr::IntLit(1))
            };
        }

        self.expect(TokenKind::Semi)?;

        Ok(Statement::Put(PutStmt {
            file,
            mode,
            items,
            skip,
            page,
            line: line_num,
        }))
    }

    fn parse_get(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // GET
        let mut file = None;
        let mut mode = IoMode::List;
        let mut items = Vec::new();

        if self.check_keyword("FILE") {
            self.advance();
            self.expect(TokenKind::LParen)?;
            file = Some(self.expect_ident()?);
            self.expect(TokenKind::RParen)?;
        }

        if self.check_keyword("LIST") {
            self.advance();
            mode = IoMode::List;
            self.expect(TokenKind::LParen)?;
            while self.peek().kind != TokenKind::RParen && !self.at_eof() {
                items.push(self.parse_expr()?);
                if self.peek().kind == TokenKind::Comma {
                    self.advance();
                }
            }
            self.expect(TokenKind::RParen)?;
        } else if self.check_keyword("EDIT") {
            self.advance();
            mode = IoMode::Edit;
            self.expect(TokenKind::LParen)?;
            while self.peek().kind != TokenKind::RParen && !self.at_eof() {
                items.push(self.parse_expr()?);
                if self.peek().kind == TokenKind::Comma {
                    self.advance();
                }
            }
            self.expect(TokenKind::RParen)?;
        } else if self.check_keyword("DATA") {
            self.advance();
            mode = IoMode::Data;
        }

        self.expect(TokenKind::Semi)?;

        Ok(Statement::Get(GetStmt { file, mode, items }))
    }

    fn parse_open(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // OPEN
        if self.check_keyword("FILE") {
            self.advance();
        }
        self.expect(TokenKind::LParen)?;
        let file = self.expect_ident()?;
        self.expect(TokenKind::RParen)?;
        let mut attributes = Vec::new();
        while self.peek().kind == TokenKind::Ident && self.peek().kind != TokenKind::Semi {
            attributes.push(self.expect_ident()?);
        }
        self.expect(TokenKind::Semi)?;
        Ok(Statement::Open(OpenStmt { file, attributes }))
    }

    fn parse_close(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // CLOSE
        if self.check_keyword("FILE") {
            self.advance();
        }
        self.expect(TokenKind::LParen)?;
        let file = self.expect_ident()?;
        self.expect(TokenKind::RParen)?;
        self.expect(TokenKind::Semi)?;
        Ok(Statement::Close(CloseStmt { file }))
    }

    fn parse_read(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // READ
        if self.check_keyword("FILE") {
            self.advance();
        }
        self.expect(TokenKind::LParen)?;
        let file = self.expect_ident()?;
        self.expect(TokenKind::RParen)?;
        let mut into = None;
        let mut key = None;
        if self.check_keyword("INTO") {
            self.advance();
            self.expect(TokenKind::LParen)?;
            into = Some(self.expect_ident()?);
            self.expect(TokenKind::RParen)?;
        }
        if self.check_keyword("KEY") {
            self.advance();
            self.expect(TokenKind::LParen)?;
            key = Some(self.parse_expr()?);
            self.expect(TokenKind::RParen)?;
        }
        self.expect(TokenKind::Semi)?;
        Ok(Statement::Read(ReadStmt { file, into, key }))
    }

    fn parse_write(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // WRITE
        if self.check_keyword("FILE") {
            self.advance();
        }
        self.expect(TokenKind::LParen)?;
        let file = self.expect_ident()?;
        self.expect(TokenKind::RParen)?;
        let mut from = None;
        let mut keyfrom = None;
        if self.check_keyword("FROM") {
            self.advance();
            self.expect(TokenKind::LParen)?;
            from = Some(self.expect_ident()?);
            self.expect(TokenKind::RParen)?;
        }
        if self.check_keyword("KEYFROM") {
            self.advance();
            self.expect(TokenKind::LParen)?;
            keyfrom = Some(self.parse_expr()?);
            self.expect(TokenKind::RParen)?;
        }
        self.expect(TokenKind::Semi)?;
        Ok(Statement::Write(WriteStmt { file, from, keyfrom }))
    }

    // ─────── Exception handling ───────

    fn parse_on(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // ON
        let condition = self.parse_on_condition()?;
        let action = Box::new(self.parse_statement()?);
        Ok(Statement::On(OnStmt { condition, action }))
    }

    fn parse_signal(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // SIGNAL
        let condition = self.parse_on_condition()?;
        self.expect(TokenKind::Semi)?;
        Ok(Statement::Signal(SignalStmt { condition }))
    }

    fn parse_revert(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // REVERT
        let condition = self.parse_on_condition()?;
        self.expect(TokenKind::Semi)?;
        Ok(Statement::Revert(RevertStmt { condition }))
    }

    fn parse_on_condition(&mut self) -> Result<OnCondition, ParseError> {
        let name = self.expect_ident()?;
        let upper = name.to_uppercase();

        let file_arg = |parser: &mut Self| -> Result<Option<String>, ParseError> {
            if parser.peek().kind == TokenKind::LParen {
                parser.advance();
                let f = parser.expect_ident()?;
                parser.expect(TokenKind::RParen)?;
                Ok(Some(f))
            } else {
                Ok(None)
            }
        };

        match upper.as_str() {
            "CONVERSION" => Ok(OnCondition::Conversion),
            "ENDFILE" => Ok(OnCondition::Endfile(file_arg(self)?)),
            "ENDPAGE" => Ok(OnCondition::Endpage(file_arg(self)?)),
            "ERROR" => Ok(OnCondition::Error),
            "FINISH" => Ok(OnCondition::Finish),
            "FIXEDOVERFLOW" => Ok(OnCondition::Fixedoverflow),
            "OVERFLOW" => Ok(OnCondition::Overflow),
            "UNDERFLOW" => Ok(OnCondition::Underflow),
            "ZERODIVIDE" => Ok(OnCondition::Zerodivide),
            "SIZE" => Ok(OnCondition::Size),
            "STRINGRANGE" => Ok(OnCondition::Stringrange),
            "STRINGSIZE" => Ok(OnCondition::Stringsize),
            "SUBSCRIPTRANGE" => Ok(OnCondition::Subscriptrange),
            "INVALIDOP" => Ok(OnCondition::Invalidop),
            "KEY" => Ok(OnCondition::Key(file_arg(self)?)),
            "RECORD" => Ok(OnCondition::Record(file_arg(self)?)),
            "TRANSMIT" => Ok(OnCondition::Transmit(file_arg(self)?)),
            "UNDEFINEDFILE" => Ok(OnCondition::Undefinedfile(file_arg(self)?)),
            "NAME" => Ok(OnCondition::Name(file_arg(self)?)),
            "AREA" => Ok(OnCondition::Area),
            "ATTENTION" => Ok(OnCondition::Attention),
            "STORAGE" => Ok(OnCondition::Storage),
            "CONDITION" => {
                self.expect(TokenKind::LParen)?;
                let cname = self.expect_ident()?;
                self.expect(TokenKind::RParen)?;
                Ok(OnCondition::Condition(cname))
            }
            _ => Ok(OnCondition::Condition(name)),
        }
    }

    // ─────── Storage management ───────

    fn parse_allocate(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // ALLOCATE
        let name = self.expect_ident()?;
        let set = if self.check_keyword("SET") {
            self.advance();
            self.expect(TokenKind::LParen)?;
            let s = self.expect_ident()?;
            self.expect(TokenKind::RParen)?;
            Some(s)
        } else {
            None
        };
        self.expect(TokenKind::Semi)?;
        Ok(Statement::Allocate(AllocateStmt { name, set }))
    }

    fn parse_free(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // FREE
        let name = self.expect_ident()?;
        self.expect(TokenKind::Semi)?;
        Ok(Statement::Free(FreeStmt { name }))
    }

    fn parse_display(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // DISPLAY
        self.expect(TokenKind::LParen)?;
        let expr = self.parse_expr()?;
        self.expect(TokenKind::RParen)?;
        self.expect(TokenKind::Semi)?;
        Ok(Statement::Display(DisplayStmt { expr }))
    }

    fn parse_assignment(&mut self) -> Result<Statement, ParseError> {
        // Parse assignment target without consuming `=` as comparison.
        let target = self.parse_assignment_target()?;
        self.expect(TokenKind::Eq)?;
        let value = self.parse_expr()?;
        self.expect(TokenKind::Semi)?;
        Ok(Statement::Assignment(AssignmentStmt { target, value }))
    }

    /// Parse an assignment target: identifier with optional subscripts, qualifications, or
    /// pointer dereferences — but NOT full expressions (avoids consuming `=` as comparison).
    fn parse_assignment_target(&mut self) -> Result<Expr, ParseError> {
        let tok = self.peek().clone();
        match tok.kind {
            TokenKind::Ident => {
                let name = tok.text.clone();
                self.advance();
                let mut expr = Expr::Ident(name.clone());
                // Allow subscript / function call parens.
                if self.peek().kind == TokenKind::LParen {
                    self.advance();
                    let mut args = Vec::new();
                    while self.peek().kind != TokenKind::RParen && !self.at_eof() {
                        args.push(self.parse_expr()?);
                        if self.peek().kind == TokenKind::Comma {
                            self.advance();
                        }
                    }
                    self.expect(TokenKind::RParen)?;
                    expr = Expr::Subscript(Box::new(expr), args);
                }
                // Allow pointer qualification chains.
                while self.peek().kind == TokenKind::Arrow {
                    self.advance();
                    let field = self.expect_ident()?;
                    expr = Expr::PtrQual(Box::new(expr), field);
                }
                // Allow dot qualification.
                while self.peek().kind == TokenKind::Dot {
                    self.advance();
                    let field = self.expect_ident()?;
                    expr = Expr::PtrQual(Box::new(expr), field);
                }
                Ok(expr)
            }
            _ => Err(ParseError::Unexpected {
                expected: "assignment target".to_string(),
                found: tok.kind,
                line: tok.line,
                col: tok.col,
            }),
        }
    }

    // ─────── Preprocessor ───────

    fn parse_preprocessor(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // %
        let kw = self.expect_ident()?;
        let upper = kw.to_uppercase();

        match upper.as_str() {
            "INCLUDE" | "XINCLUDE" => {
                let member = self.expect_ident()?;
                self.expect(TokenKind::Semi)?;
                if upper == "XINCLUDE" {
                    Ok(Statement::Preprocessor(PreprocessorDirective::XInclude(member)))
                } else {
                    Ok(Statement::Preprocessor(PreprocessorDirective::Include(member)))
                }
            }
            "NOTE" => {
                let msg = if self.peek().kind == TokenKind::StringLit {
                    self.advance().text.clone()
                } else {
                    String::new()
                };
                self.expect(TokenKind::Semi)?;
                Ok(Statement::Preprocessor(PreprocessorDirective::Note(msg)))
            }
            _ => {
                // Skip to semicolon for unrecognized directives.
                while self.peek().kind != TokenKind::Semi && !self.at_eof() {
                    self.advance();
                }
                if self.peek().kind == TokenKind::Semi {
                    self.advance();
                }
                Ok(Statement::Null)
            }
        }
    }

    // ─────── Expression parsing (simplified) ───────

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_or_expr()
    }

    fn parse_or_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_and_expr()?;
        while self.peek().kind == TokenKind::Or {
            self.advance();
            let right = self.parse_and_expr()?;
            left = Expr::BinOp(Box::new(left), BinOp::Or, Box::new(right));
        }
        Ok(left)
    }

    fn parse_and_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_compare_expr()?;
        while self.peek().kind == TokenKind::And {
            self.advance();
            let right = self.parse_compare_expr()?;
            left = Expr::BinOp(Box::new(left), BinOp::And, Box::new(right));
        }
        Ok(left)
    }

    fn parse_compare_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_concat_expr()?;
        loop {
            let op = match self.peek().kind {
                TokenKind::Eq => BinOp::Eq,
                TokenKind::Ne => BinOp::Ne,
                TokenKind::Lt => BinOp::Lt,
                TokenKind::Gt => BinOp::Gt,
                TokenKind::Le => BinOp::Le,
                TokenKind::Ge => BinOp::Ge,
                _ => break,
            };
            self.advance();
            let right = self.parse_concat_expr()?;
            left = Expr::BinOp(Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    fn parse_concat_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_add_expr()?;
        while self.peek().kind == TokenKind::Concat {
            self.advance();
            let right = self.parse_add_expr()?;
            left = Expr::BinOp(Box::new(left), BinOp::Concat, Box::new(right));
        }
        Ok(left)
    }

    fn parse_add_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_mul_expr()?;
        loop {
            let op = match self.peek().kind {
                TokenKind::Plus => BinOp::Add,
                TokenKind::Minus => BinOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_mul_expr()?;
            left = Expr::BinOp(Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    fn parse_mul_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_power_expr()?;
        loop {
            let op = match self.peek().kind {
                TokenKind::Star => BinOp::Mul,
                TokenKind::Slash => BinOp::Div,
                _ => break,
            };
            self.advance();
            let right = self.parse_power_expr()?;
            left = Expr::BinOp(Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    fn parse_power_expr(&mut self) -> Result<Expr, ParseError> {
        let left = self.parse_unary_expr()?;
        if self.peek().kind == TokenKind::Power {
            self.advance();
            let right = self.parse_unary_expr()?;
            Ok(Expr::BinOp(Box::new(left), BinOp::Power, Box::new(right)))
        } else {
            Ok(left)
        }
    }

    fn parse_unary_expr(&mut self) -> Result<Expr, ParseError> {
        match self.peek().kind {
            TokenKind::Minus => {
                self.advance();
                let e = self.parse_primary()?;
                Ok(Expr::UnaryOp(UnaryOp::Neg, Box::new(e)))
            }
            TokenKind::Plus => {
                self.advance();
                let e = self.parse_primary()?;
                Ok(Expr::UnaryOp(UnaryOp::Pos, Box::new(e)))
            }
            TokenKind::Not => {
                self.advance();
                let e = self.parse_primary()?;
                Ok(Expr::UnaryOp(UnaryOp::Not, Box::new(e)))
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let tok = self.peek().clone();
        match tok.kind {
            TokenKind::IntLit => {
                self.advance();
                Ok(Expr::IntLit(tok.text.parse::<i64>().unwrap_or(0)))
            }
            TokenKind::DecLit => {
                self.advance();
                Ok(Expr::DecLit(tok.text))
            }
            TokenKind::FloatLit => {
                self.advance();
                Ok(Expr::FloatLit(tok.text))
            }
            TokenKind::StringLit => {
                self.advance();
                Ok(Expr::StringLit(tok.text))
            }
            TokenKind::BitLit => {
                self.advance();
                Ok(Expr::BitLit(tok.text))
            }
            TokenKind::HexLit => {
                self.advance();
                Ok(Expr::HexLit(tok.text))
            }
            TokenKind::Ident => {
                let name = tok.text.clone();
                self.advance();
                // Check for function call or subscript.
                if self.peek().kind == TokenKind::LParen {
                    self.advance();
                    let mut args = Vec::new();
                    while self.peek().kind != TokenKind::RParen && !self.at_eof() {
                        args.push(self.parse_expr()?);
                        if self.peek().kind == TokenKind::Comma {
                            self.advance();
                        }
                    }
                    self.expect(TokenKind::RParen)?;
                    Ok(Expr::FuncCall(name, args))
                } else {
                    Ok(Expr::Ident(name))
                }
            }
            TokenKind::LParen => {
                self.advance();
                let e = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                Ok(Expr::Paren(Box::new(e)))
            }
            _ => Err(ParseError::Unexpected {
                expected: "expression".to_string(),
                found: tok.kind,
                line: tok.line,
                col: tok.col,
            }),
        }
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn parse(src: &str) -> Program {
        let tokens = Lexer::tokenize(src).unwrap();
        Parser::parse(tokens).unwrap()
    }

    // ─────── P100.1: Lexer+Parser integration ───────

    #[test]
    fn test_parse_declare_fixed_decimal() {
        let prog = parse("DCL X FIXED DECIMAL(7,2);");
        assert_eq!(prog.statements.len(), 1);
        if let Statement::Declare(dcl) = &prog.statements[0] {
            assert_eq!(dcl.items.len(), 1);
            assert_eq!(dcl.items[0].name, "X");
            assert!(matches!(dcl.items[0].data_type, Some(DataType::FixedDecimal(7, 2))));
        } else {
            panic!("Expected Declare");
        }
    }

    // ─────── P100.2: DECLARE with full attributes ───────

    #[test]
    fn test_parse_declare_structure() {
        let prog = parse(
            "DCL 1 REC, 2 NAME CHAR(30) VARYING, 2 AGE FIXED BIN(31);",
        );
        if let Statement::Declare(dcl) = &prog.statements[0] {
            assert_eq!(dcl.items.len(), 3);
            assert_eq!(dcl.items[0].level, 1);
            assert_eq!(dcl.items[0].name, "REC");
            assert_eq!(dcl.items[1].level, 2);
            assert_eq!(dcl.items[1].name, "NAME");
            assert!(matches!(dcl.items[1].data_type, Some(DataType::CharacterVarying(30))));
            assert_eq!(dcl.items[2].level, 2);
            assert!(matches!(dcl.items[2].data_type, Some(DataType::FixedBinary(31, 0))));
        } else {
            panic!("Expected Declare");
        }
    }

    #[test]
    fn test_parse_declare_pointer_and_based() {
        let prog = parse("DCL PTR POINTER, AREA_X AREA(1000);");
        if let Statement::Declare(dcl) = &prog.statements[0] {
            assert_eq!(dcl.items.len(), 2);
            assert!(matches!(dcl.items[0].data_type, Some(DataType::Pointer)));
            assert!(matches!(dcl.items[1].data_type, Some(DataType::Area(1000))));
        } else {
            panic!("Expected Declare");
        }
    }

    #[test]
    fn test_parse_declare_char_varying() {
        let prog = parse("DCL MSG CHAR(100) VAR;");
        if let Statement::Declare(dcl) = &prog.statements[0] {
            assert!(matches!(dcl.items[0].data_type, Some(DataType::CharacterVarying(100))));
        } else {
            panic!("Expected Declare");
        }
    }

    #[test]
    fn test_parse_declare_static_initial() {
        let prog = parse("DCL COUNT FIXED BIN(31) STATIC INIT(0);");
        if let Statement::Declare(dcl) = &prog.statements[0] {
            assert_eq!(dcl.items[0].storage, Some(StorageClass::Static));
            assert!(dcl.items[0].initial.is_some());
        } else {
            panic!("Expected Declare");
        }
    }

    // ─────── P100.3: Control Flow ───────

    #[test]
    fn test_parse_if_then_else() {
        let prog = parse("IF X > 0 THEN Y = 1; ELSE Y = 2;");
        if let Statement::If(if_stmt) = &prog.statements[0] {
            assert!(if_stmt.else_stmt.is_some());
        } else {
            panic!("Expected If");
        }
    }

    #[test]
    fn test_parse_do_iterative() {
        let prog = parse("DO I = 1 TO 100 BY 2; X = X + I; END;");
        if let Statement::Do(do_stmt) = &prog.statements[0] {
            assert!(matches!(do_stmt.control, Some(DoControl::Iterative { .. })));
            assert_eq!(do_stmt.body.len(), 1);
        } else {
            panic!("Expected Do");
        }
    }

    #[test]
    fn test_parse_do_while() {
        let prog = parse("DO WHILE (X > 0); X = X - 1; END;");
        if let Statement::Do(do_stmt) = &prog.statements[0] {
            assert!(matches!(do_stmt.control, Some(DoControl::While(_))));
        } else {
            panic!("Expected Do");
        }
    }

    #[test]
    fn test_parse_select() {
        let prog = parse("SELECT (CODE); WHEN (1) CALL PROC_A; WHEN (2,3) CALL PROC_B; OTHERWISE CALL PROC_ERR; END;");
        if let Statement::Select(sel) = &prog.statements[0] {
            assert!(sel.expr.is_some());
            assert_eq!(sel.when_clauses.len(), 2);
            assert_eq!(sel.when_clauses[1].values.len(), 2); // WHEN(2,3)
            assert!(sel.otherwise.is_some());
        } else {
            panic!("Expected Select");
        }
    }

    #[test]
    fn test_parse_call() {
        let prog = parse("CALL MYPROC(A, B, C);");
        if let Statement::Call(call) = &prog.statements[0] {
            assert_eq!(call.name, "MYPROC");
            assert_eq!(call.args.len(), 3);
        } else {
            panic!("Expected Call");
        }
    }

    #[test]
    fn test_parse_return() {
        let prog = parse("RETURN(X + 1);");
        if let Statement::Return(ret) = &prog.statements[0] {
            assert!(ret.value.is_some());
        } else {
            panic!("Expected Return");
        }
    }

    #[test]
    fn test_parse_goto() {
        let prog = parse("GO TO DONE;");
        if let Statement::GoTo(gt) = &prog.statements[0] {
            assert_eq!(gt.label, "DONE");
        } else {
            panic!("Expected GoTo");
        }
    }

    // ─────── I/O ───────

    #[test]
    fn test_parse_put_list() {
        let prog = parse("PUT LIST('Hello, World!');");
        if let Statement::Put(put) = &prog.statements[0] {
            assert!(matches!(put.mode, IoMode::List));
            assert_eq!(put.items.len(), 1);
        } else {
            panic!("Expected Put");
        }
    }

    #[test]
    fn test_parse_put_skip() {
        let prog = parse("PUT SKIP LIST(X, Y);");
        if let Statement::Put(put) = &prog.statements[0] {
            assert!(put.skip.is_some());
            assert_eq!(put.items.len(), 2);
        } else {
            panic!("Expected Put");
        }
    }

    // ─────── Exception handling ───────

    #[test]
    fn test_parse_on_endfile() {
        let prog = parse("ON ENDFILE(INFILE) EOF_FLAG = '1'B;");
        if let Statement::On(on) = &prog.statements[0] {
            assert!(matches!(on.condition, OnCondition::Endfile(Some(_))));
        } else {
            panic!("Expected On");
        }
    }

    #[test]
    fn test_parse_signal() {
        let prog = parse("SIGNAL ERROR;");
        if let Statement::Signal(sig) = &prog.statements[0] {
            assert!(matches!(sig.condition, OnCondition::Error));
        } else {
            panic!("Expected Signal");
        }
    }

    // ─────── Procedure ───────

    #[test]
    fn test_parse_procedure() {
        let prog = parse("HELLO: PROC OPTIONS(MAIN); PUT LIST('Hi'); END HELLO;");
        if let Statement::Procedure(proc) = &prog.statements[0] {
            assert_eq!(proc.name, "HELLO");
            assert!(proc.options.contains(&"MAIN".to_string()));
            assert_eq!(proc.body.len(), 1);
        } else {
            panic!("Expected Procedure");
        }
    }

    // ─────── Preprocessor ───────

    #[test]
    fn test_parse_include() {
        let prog = parse("%INCLUDE COPYBOOK;");
        if let Statement::Preprocessor(PreprocessorDirective::Include(member)) = &prog.statements[0] {
            assert_eq!(member, "COPYBOOK");
        } else {
            panic!("Expected Include");
        }
    }

    // ─────── Expressions ───────

    #[test]
    fn test_parse_arithmetic_expr() {
        let prog = parse("X = A + B * C;");
        // Should parse as X = A + (B * C) due to precedence.
        assert!(matches!(prog.statements[0], Statement::Assignment(_)));
    }

    #[test]
    fn test_parse_concatenation() {
        let prog = parse("X = A || B;");
        if let Statement::Assignment(a) = &prog.statements[0] {
            assert!(matches!(a.value, Expr::BinOp(_, BinOp::Concat, _)));
        } else {
            panic!("Expected Assignment");
        }
    }

    #[test]
    fn test_parse_exponentiation() {
        let prog = parse("X = A ** 2;");
        if let Statement::Assignment(a) = &prog.statements[0] {
            assert!(matches!(a.value, Expr::BinOp(_, BinOp::Power, _)));
        } else {
            panic!("Expected Assignment");
        }
    }

    #[test]
    fn test_parse_func_call_expr() {
        let prog = parse("X = SUBSTR(S, 1, 5);");
        if let Statement::Assignment(a) = &prog.statements[0] {
            assert!(matches!(a.value, Expr::FuncCall(_, _)));
        } else {
            panic!("Expected Assignment");
        }
    }
}
