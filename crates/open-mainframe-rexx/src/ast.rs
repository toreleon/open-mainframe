//! REXX abstract syntax tree.
//!
//! REXX programs are sequences of clauses.  Each clause is either a label,
//! an instruction, a command, or an assignment.

use serde::{Deserialize, Serialize};

/// A REXX program — a sequence of clauses.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Program {
    pub clauses: Vec<Clause>,
}

/// A single REXX clause.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Clause {
    /// Source line where this clause begins.
    pub line: u32,
    /// The clause body.
    pub body: ClauseBody,
}

/// The body of a clause.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ClauseBody {
    /// Label definition: `name:`
    Label(String),
    /// Variable assignment: `var = expr`
    Assignment { var: String, expr: Expr },
    /// SAY instruction: `SAY expr`
    Say(Expr),
    /// IF/THEN/ELSE instruction.
    If {
        cond: Expr,
        then_clause: Box<Clause>,
        else_clause: Option<Box<Clause>>,
    },
    /// DO block (simple, iterative, while, until, forever).
    Do {
        control: DoControl,
        body: Vec<Clause>,
    },
    /// SELECT/WHEN/OTHERWISE/END.
    Select {
        whens: Vec<(Expr, Vec<Clause>)>,
        otherwise: Option<Vec<Clause>>,
    },
    /// CALL instruction: `CALL name args...`
    Call { name: String, args: Vec<Expr> },
    /// RETURN instruction: `RETURN [expr]`
    Return(Option<Expr>),
    /// EXIT instruction: `EXIT [expr]`
    Exit(Option<Expr>),
    /// PARSE instruction: `PARSE [UPPER] source template`
    Parse {
        upper: bool,
        source: ParseSource,
        template: String,
    },
    /// ARG instruction (shorthand for `PARSE UPPER ARG template`).
    Arg(String),
    /// PULL instruction (shorthand for `PARSE UPPER PULL template`).
    Pull(String),
    /// PUSH instruction: push to stack.
    Push(Expr),
    /// QUEUE instruction: queue to stack (FIFO).
    Queue(Expr),
    /// DROP instruction: drop variables.
    Drop(Vec<String>),
    /// SIGNAL instruction.
    Signal(SignalTarget),
    /// ITERATE instruction (in DO loop).
    Iterate(Option<String>),
    /// LEAVE instruction (in DO loop).
    Leave(Option<String>),
    /// NOP instruction.
    Nop,
    /// TRACE instruction.
    Trace(String),
    /// ADDRESS instruction.
    Address {
        environment: Option<String>,
        command: Option<Expr>,
    },
    /// PROCEDURE instruction (with optional EXPOSE list).
    Procedure { expose: Vec<String> },
    /// NUMERIC instruction (DIGITS, FUZZ, FORM).
    Numeric { setting: String, value: Expr },
    /// Host command (unrecognized instruction → sent to ADDRESS).
    Command(Expr),
}

/// DO loop control.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DoControl {
    /// `DO; ... END` (simple block).
    Simple,
    /// `DO FOREVER; ... END`
    Forever,
    /// `DO count; ... END`
    Count(Expr),
    /// `DO var = from TO to [BY step]; ... END`
    Iterative {
        var: String,
        from: Expr,
        to: Expr,
        by: Option<Expr>,
    },
    /// `DO WHILE cond; ... END`
    While(Expr),
    /// `DO UNTIL cond; ... END`
    Until(Expr),
}

/// PARSE source.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ParseSource {
    Arg,
    Pull,
    Var(String),
    Value(Expr),
    External,
    Source,
    Version,
    Linein,
}

/// SIGNAL target.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SignalTarget {
    /// `SIGNAL label`
    Label(String),
    /// `SIGNAL ON condition [NAME trap]`
    On { condition: String, name: Option<String> },
    /// `SIGNAL OFF condition`
    Off(String),
}

/// A REXX expression.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expr {
    /// A literal string.
    StringLit(String),
    /// A number literal.
    Number(String),
    /// A variable reference.
    Variable(String),
    /// Hex string literal.
    HexLit(String),
    /// Binary string literal.
    BinLit(String),
    /// Binary operation.
    BinOp {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
    /// Unary prefix operation.
    UnaryOp {
        op: UnaryOp,
        operand: Box<Expr>,
    },
    /// Function call: `name(args)`
    FunctionCall {
        name: String,
        args: Vec<Expr>,
    },
    /// Concatenation (abuttal — no operator, just adjacency).
    Abuttal(Vec<Expr>),
}

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    IntDiv,  // %
    Mod,     // //
    Power,   // **
    Concat,  // ||
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
    StrictEq,
    StrictNe,
    StrictGt,
    StrictLt,
    StrictGe,
    StrictLe,
    And,
    Or,
    Xor,
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    /// Unary minus.
    Neg,
    /// Unary plus.
    Pos,
    /// Logical NOT.
    Not,
}
