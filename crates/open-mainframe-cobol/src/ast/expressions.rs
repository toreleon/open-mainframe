//! Expression and condition types for the COBOL AST.

use crate::lexer::Span;

/// An expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    /// Literal value.
    Literal(Literal),
    /// Variable reference.
    Variable(QualifiedName),
    /// Reference modification.
    RefMod(RefMod),
    /// Function call.
    Function(FunctionCall),
    /// Binary operation.
    Binary(Box<BinaryExpr>),
    /// Unary operation.
    Unary(Box<UnaryExpr>),
    /// Parenthesized expression.
    Paren(Box<Expression>),
    /// LENGTH OF data-item.
    LengthOf(LengthOf),
    /// ADDRESS OF data-item.
    AddressOf(AddressOf),
}

impl Expression {
    /// Get the span of this expression.
    pub fn span(&self) -> Span {
        match self {
            Expression::Literal(l) => l.span,
            Expression::Variable(v) => v.span,
            Expression::RefMod(r) => r.span,
            Expression::Function(f) => f.span,
            Expression::Binary(b) => b.span,
            Expression::Unary(u) => u.span,
            Expression::Paren(e) => e.span(),
            Expression::LengthOf(l) => l.span,
            Expression::AddressOf(a) => a.span,
        }
    }
}

/// LENGTH OF expression.
#[derive(Debug, Clone, PartialEq)]
pub struct LengthOf {
    /// Data item to get length of.
    pub item: QualifiedName,
    /// Source span.
    pub span: Span,
}

/// ADDRESS OF expression.
#[derive(Debug, Clone, PartialEq)]
pub struct AddressOf {
    /// Data item to get address of.
    pub item: QualifiedName,
    /// Source span.
    pub span: Span,
}

/// A literal value.
#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    /// The kind of literal.
    pub kind: LiteralKind,
    /// Source span.
    pub span: Span,
}

/// Kind of literal.
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    /// Integer.
    Integer(i64),
    /// Decimal (stored as string to preserve precision).
    Decimal(String),
    /// String.
    String(String),
    /// Hex string.
    Hex(String),
    /// Figurative constant.
    Figurative(FigurativeConstant),
    /// ALL followed by a literal (the literal is repeated).
    AllOf(Box<Literal>),
}

/// Figurative constants.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FigurativeConstant {
    /// ZERO/ZEROS/ZEROES.
    Zero,
    /// SPACE/SPACES.
    Space,
    /// HIGH-VALUE/HIGH-VALUES.
    HighValue,
    /// LOW-VALUE/LOW-VALUES.
    LowValue,
    /// QUOTE/QUOTES.
    Quote,
    /// ALL followed by a literal.
    All,
}

/// Qualified name (with OF qualification).
#[derive(Debug, Clone, PartialEq)]
pub struct QualifiedName {
    /// The base name.
    pub name: String,
    /// Qualifiers (in reverse order: innermost first).
    pub qualifiers: Vec<String>,
    /// Subscripts.
    pub subscripts: Vec<Expression>,
    /// Reference modification (start:length).
    pub refmod: Option<(Box<Expression>, Option<Box<Expression>>)>,
    /// Source span.
    pub span: Span,
}

impl QualifiedName {
    /// Create a simple unqualified name.
    pub fn simple(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: name.into(),
            qualifiers: Vec::new(),
            subscripts: Vec::new(),
            refmod: None,
            span,
        }
    }
}

/// Reference modification.
#[derive(Debug, Clone, PartialEq)]
pub struct RefMod {
    /// Variable being modified.
    pub variable: QualifiedName,
    /// Start position.
    pub start: Box<Expression>,
    /// Length (optional).
    pub length: Option<Box<Expression>>,
    /// Source span.
    pub span: Span,
}

/// Function call.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    /// Function name.
    pub name: String,
    /// Arguments.
    pub arguments: Vec<Expression>,
    /// Source span.
    pub span: Span,
}

/// Binary expression.
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    /// Left operand.
    pub left: Expression,
    /// Operator.
    pub op: BinaryOp,
    /// Right operand.
    pub right: Expression,
    /// Source span.
    pub span: Span,
}

/// Binary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// Addition.
    Add,
    /// Subtraction.
    Subtract,
    /// Multiplication.
    Multiply,
    /// Division.
    Divide,
    /// Exponentiation.
    Power,
}

/// Unary expression.
#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    /// Operator.
    pub op: UnaryOp,
    /// Operand.
    pub operand: Expression,
    /// Source span.
    pub span: Span,
}

/// Unary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// Unary plus.
    Plus,
    /// Unary minus.
    Minus,
}

/// A condition (boolean expression).
#[derive(Debug, Clone, PartialEq)]
pub enum Condition {
    /// Comparison.
    Comparison(Box<Comparison>),
    /// Class condition.
    Class(Box<ClassCondition>),
    /// Sign condition.
    Sign(Box<SignCondition>),
    /// Condition name (level 88).
    ConditionName(QualifiedName),
    /// Negation.
    Not(Box<Condition>),
    /// Conjunction.
    And(Box<Condition>, Box<Condition>),
    /// Disjunction.
    Or(Box<Condition>, Box<Condition>),
    /// Parenthesized.
    Paren(Box<Condition>),
}

/// Comparison condition.
#[derive(Debug, Clone, PartialEq)]
pub struct Comparison {
    /// Left operand.
    pub left: Expression,
    /// Comparison operator.
    pub op: ComparisonOp,
    /// Right operand.
    pub right: Expression,
    /// Source span.
    pub span: Span,
}

/// Comparison operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparisonOp {
    /// Equal.
    Equal,
    /// Not equal.
    NotEqual,
    /// Greater than.
    GreaterThan,
    /// Greater than or equal.
    GreaterOrEqual,
    /// Less than.
    LessThan,
    /// Less than or equal.
    LessOrEqual,
}

/// Class condition (NUMERIC, ALPHABETIC, etc.).
#[derive(Debug, Clone, PartialEq)]
pub struct ClassCondition {
    /// Variable being tested.
    pub operand: Expression,
    /// Class type.
    pub class: ClassType,
    /// Negated (NOT).
    pub negated: bool,
    /// Source span.
    pub span: Span,
}

/// Class type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClassType {
    /// NUMERIC.
    Numeric,
    /// ALPHABETIC.
    Alphabetic,
    /// ALPHABETIC-LOWER.
    AlphabeticLower,
    /// ALPHABETIC-UPPER.
    AlphabeticUpper,
}

/// Sign condition.
#[derive(Debug, Clone, PartialEq)]
pub struct SignCondition {
    /// Variable being tested.
    pub operand: Expression,
    /// Sign type.
    pub sign: SignType,
    /// Negated (NOT).
    pub negated: bool,
    /// Source span.
    pub span: Span,
}

/// Sign type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SignType {
    /// POSITIVE.
    Positive,
    /// NEGATIVE.
    Negative,
    /// ZERO.
    Zero,
}
