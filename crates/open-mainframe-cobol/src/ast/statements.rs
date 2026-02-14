//! Statement types for the COBOL AST.

use crate::lexer::Span;
use super::expressions::*;
use super::data::OpenMode;

// ============================================================================
// STATEMENTS
// ============================================================================

/// A COBOL statement.
///
/// Generated from the master statement table in `macros.rs`.
/// To add a new statement variant, add one line there -- the enum variant
/// and `span()` arm are generated automatically.
macro_rules! gen_statement_enum {
    ( $($variant:ident($stype:ident)),* $(,)? ) => {
        #[derive(Debug, Clone, PartialEq)]
        #[non_exhaustive]
        pub enum Statement {
            $(
                #[doc = concat!(stringify!($variant), " statement.")]
                $variant($stype),
            )*
        }

        impl Statement {
            /// Get the span of this statement.
            pub fn span(&self) -> Span {
                match self {
                    $(Statement::$variant(s) => s.span,)*
                }
            }
        }
    };
}
for_all_statement_variants!(gen_statement_enum);

/// MOVE statement.
#[derive(Debug, Clone, PartialEq)]
pub struct MoveStatement {
    /// Source expression.
    pub from: Expression,
    /// Target variables.
    pub to: Vec<QualifiedName>,
    /// CORRESPONDING flag.
    pub corresponding: bool,
    /// Source span.
    pub span: Span,
}

/// COMPUTE statement.
#[derive(Debug, Clone, PartialEq)]
pub struct ComputeStatement {
    /// Target variables.
    pub targets: Vec<ComputeTarget>,
    /// Expression to compute.
    pub expression: Expression,
    /// ON SIZE ERROR handler.
    pub on_size_error: Option<Vec<Statement>>,
    /// NOT ON SIZE ERROR handler.
    pub not_on_size_error: Option<Vec<Statement>>,
    /// Source span.
    pub span: Span,
}

/// Target for COMPUTE statement.
#[derive(Debug, Clone, PartialEq)]
pub struct ComputeTarget {
    /// Variable name.
    pub name: QualifiedName,
    /// ROUNDED flag.
    pub rounded: bool,
}

/// ADD statement.
#[derive(Debug, Clone, PartialEq)]
pub struct AddStatement {
    /// Values to add.
    pub operands: Vec<Expression>,
    /// Targets (TO clause).
    pub to: Vec<AddTarget>,
    /// GIVING clause targets.
    pub giving: Vec<ComputeTarget>,
    /// ON SIZE ERROR handler.
    pub on_size_error: Option<Vec<Statement>>,
    /// NOT ON SIZE ERROR handler.
    pub not_on_size_error: Option<Vec<Statement>>,
    /// Source span.
    pub span: Span,
}

/// Target for ADD statement.
#[derive(Debug, Clone, PartialEq)]
pub struct AddTarget {
    /// Variable name.
    pub name: QualifiedName,
    /// ROUNDED flag.
    pub rounded: bool,
}

/// SUBTRACT statement.
#[derive(Debug, Clone, PartialEq)]
pub struct SubtractStatement {
    /// Values to subtract.
    pub operands: Vec<Expression>,
    /// Targets (FROM clause).
    pub from: Vec<AddTarget>,
    /// GIVING clause targets.
    pub giving: Vec<ComputeTarget>,
    /// ON SIZE ERROR handler.
    pub on_size_error: Option<Vec<Statement>>,
    /// NOT ON SIZE ERROR handler.
    pub not_on_size_error: Option<Vec<Statement>>,
    /// Source span.
    pub span: Span,
}

/// MULTIPLY statement.
#[derive(Debug, Clone, PartialEq)]
pub struct MultiplyStatement {
    /// First operand.
    pub operand: Expression,
    /// Second operand (BY clause).
    pub by: Expression,
    /// Targets.
    pub giving: Vec<ComputeTarget>,
    /// ON SIZE ERROR handler.
    pub on_size_error: Option<Vec<Statement>>,
    /// NOT ON SIZE ERROR handler.
    pub not_on_size_error: Option<Vec<Statement>>,
    /// Source span.
    pub span: Span,
}

/// DIVIDE statement.
#[derive(Debug, Clone, PartialEq)]
pub struct DivideStatement {
    /// Dividend or divisor.
    pub operand: Expression,
    /// The other operand (INTO or BY clause).
    pub into_or_by: Expression,
    /// Whether INTO (true) or BY (false) form.
    pub is_into: bool,
    /// GIVING clause targets.
    pub giving: Vec<ComputeTarget>,
    /// REMAINDER target.
    pub remainder: Option<QualifiedName>,
    /// ON SIZE ERROR handler.
    pub on_size_error: Option<Vec<Statement>>,
    /// NOT ON SIZE ERROR handler.
    pub not_on_size_error: Option<Vec<Statement>>,
    /// Source span.
    pub span: Span,
}

/// IF statement.
#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    /// Condition.
    pub condition: Condition,
    /// Then branch statements.
    pub then_branch: Vec<Statement>,
    /// Else branch statements (optional).
    pub else_branch: Option<Vec<Statement>>,
    /// Source span.
    pub span: Span,
}

/// EVALUATE statement.
#[derive(Debug, Clone, PartialEq)]
pub struct EvaluateStatement {
    /// Subjects (the values being evaluated).
    pub subjects: Vec<Expression>,
    /// WHEN clauses.
    pub when_clauses: Vec<WhenClause>,
    /// WHEN OTHER clause.
    pub when_other: Option<Vec<Statement>>,
    /// Source span.
    pub span: Span,
}

/// WHEN clause in EVALUATE.
#[derive(Debug, Clone, PartialEq)]
pub struct WhenClause {
    /// Conditions for each subject.
    pub conditions: Vec<WhenCondition>,
    /// Statements to execute.
    pub statements: Vec<Statement>,
    /// Source span.
    pub span: Span,
}

/// A single condition in a WHEN clause.
#[derive(Debug, Clone, PartialEq)]
pub enum WhenCondition {
    /// ANY value.
    Any,
    /// TRUE.
    True,
    /// FALSE.
    False,
    /// Specific value or expression.
    Value(Expression),
    /// Range (THROUGH/THRU).
    Range { from: Expression, to: Expression },
    /// Condition.
    Condition(Condition),
}

/// PERFORM statement.
#[derive(Debug, Clone, PartialEq)]
pub struct PerformStatement {
    /// Target paragraph/section.
    pub target: Option<PerformTarget>,
    /// THRU clause target.
    pub thru: Option<String>,
    /// Inline statements (for inline PERFORM).
    pub inline: Option<Vec<Statement>>,
    /// TIMES clause.
    pub times: Option<Expression>,
    /// UNTIL clause.
    pub until: Option<Condition>,
    /// VARYING clause.
    pub varying: Option<PerformVarying>,
    /// TEST BEFORE or TEST AFTER.
    pub test_before: bool,
    /// Source span.
    pub span: Span,
}

/// Target for PERFORM statement.
#[derive(Debug, Clone, PartialEq)]
pub struct PerformTarget {
    /// Paragraph or section name.
    pub name: String,
    /// Source span.
    pub span: Span,
}

/// VARYING clause for PERFORM.
#[derive(Debug, Clone, PartialEq)]
pub struct PerformVarying {
    /// Loop variable.
    pub variable: QualifiedName,
    /// FROM value.
    pub from: Expression,
    /// BY value.
    pub by: Expression,
    /// UNTIL condition.
    pub until: Condition,
    /// AFTER clauses.
    pub after: Vec<PerformAfter>,
    /// Source span.
    pub span: Span,
}

/// AFTER clause in PERFORM VARYING.
#[derive(Debug, Clone, PartialEq)]
pub struct PerformAfter {
    /// Loop variable.
    pub variable: QualifiedName,
    /// FROM value.
    pub from: Expression,
    /// BY value.
    pub by: Expression,
    /// UNTIL condition.
    pub until: Condition,
    /// Source span.
    pub span: Span,
}

/// CALL statement.
#[derive(Debug, Clone, PartialEq)]
pub struct CallStatement {
    /// Program to call (literal or identifier).
    pub program: Expression,
    /// USING clause parameters.
    pub using: Vec<CallParameter>,
    /// RETURNING clause.
    pub returning: Option<QualifiedName>,
    /// ON EXCEPTION handler.
    pub on_exception: Option<Vec<Statement>>,
    /// NOT ON EXCEPTION handler.
    pub not_on_exception: Option<Vec<Statement>>,
    /// Source span.
    pub span: Span,
}

/// Parameter for CALL statement.
#[derive(Debug, Clone, PartialEq)]
pub struct CallParameter {
    /// Parameter expression.
    pub value: Expression,
    /// BY REFERENCE, BY CONTENT, or BY VALUE.
    pub mode: super::ParameterMode,
    /// Source span.
    pub span: Span,
}

/// DISPLAY statement.
#[derive(Debug, Clone, PartialEq)]
pub struct DisplayStatement {
    /// Items to display.
    pub items: Vec<Expression>,
    /// UPON clause target.
    pub upon: Option<String>,
    /// WITH NO ADVANCING.
    pub no_advancing: bool,
    /// Source span.
    pub span: Span,
}

/// ACCEPT statement.
#[derive(Debug, Clone, PartialEq)]
pub struct AcceptStatement {
    /// Target variable.
    pub target: QualifiedName,
    /// FROM clause.
    pub from: Option<AcceptFrom>,
    /// Source span.
    pub span: Span,
}

/// FROM clause for ACCEPT.
#[derive(Debug, Clone, PartialEq)]
pub enum AcceptFrom {
    /// CONSOLE or standard input.
    Console,
    /// DATE.
    Date,
    /// DAY.
    Day,
    /// DAY-OF-WEEK.
    DayOfWeek,
    /// TIME.
    Time,
    /// Device name.
    Device(String),
}

/// OPEN statement.
#[derive(Debug, Clone, PartialEq)]
pub struct OpenStatement {
    /// Files to open.
    pub files: Vec<OpenFile>,
    /// Source span.
    pub span: Span,
}

/// File in OPEN statement.
#[derive(Debug, Clone, PartialEq)]
pub struct OpenFile {
    /// File name.
    pub name: String,
    /// Open mode.
    pub mode: OpenMode,
}

/// CLOSE statement.
#[derive(Debug, Clone, PartialEq)]
pub struct CloseStatement {
    /// Files to close.
    pub files: Vec<String>,
    /// Source span.
    pub span: Span,
}

/// READ statement.
#[derive(Debug, Clone, PartialEq)]
pub struct ReadStatement {
    /// File name.
    pub file: String,
    /// INTO clause target.
    pub into: Option<QualifiedName>,
    /// NEXT RECORD clause.
    pub next: bool,
    /// AT END handler.
    pub at_end: Option<Vec<Statement>>,
    /// NOT AT END handler.
    pub not_at_end: Option<Vec<Statement>>,
    /// INVALID KEY handler.
    pub invalid_key: Option<Vec<Statement>>,
    /// NOT INVALID KEY handler.
    pub not_invalid_key: Option<Vec<Statement>>,
    /// Source span.
    pub span: Span,
}

/// WRITE statement.
#[derive(Debug, Clone, PartialEq)]
pub struct WriteStatement {
    /// Record name.
    pub record: QualifiedName,
    /// FROM clause source.
    pub from: Option<QualifiedName>,
    /// ADVANCING clause.
    pub advancing: Option<WriteAdvancing>,
    /// INVALID KEY handler.
    pub invalid_key: Option<Vec<Statement>>,
    /// NOT INVALID KEY handler.
    pub not_invalid_key: Option<Vec<Statement>>,
    /// AT END-OF-PAGE handler.
    pub at_eop: Option<Vec<Statement>>,
    /// NOT AT END-OF-PAGE handler.
    pub not_at_eop: Option<Vec<Statement>>,
    /// Source span.
    pub span: Span,
}

/// ADVANCING clause for WRITE.
#[derive(Debug, Clone, PartialEq)]
pub enum WriteAdvancing {
    /// BEFORE/AFTER ADVANCING n LINES.
    Lines { count: Expression, before: bool },
    /// BEFORE/AFTER ADVANCING PAGE.
    Page { before: bool },
}

/// STOP RUN statement.
#[derive(Debug, Clone, PartialEq)]
pub struct StopRunStatement {
    /// Optional return code.
    pub return_code: Option<Expression>,
    /// Source span.
    pub span: Span,
}

/// GOBACK statement.
#[derive(Debug, Clone, PartialEq)]
pub struct GoBackStatement {
    /// Optional RETURNING expression.
    pub returning: Option<Expression>,
    /// Source span.
    pub span: Span,
}

/// EXIT statement.
#[derive(Debug, Clone, PartialEq)]
pub struct ExitStatement {
    /// EXIT PROGRAM.
    pub program: bool,
    /// EXIT PERFORM (cycle).
    pub perform_cycle: bool,
    /// Source span.
    pub span: Span,
}

/// GO TO statement.
#[derive(Debug, Clone, PartialEq)]
pub struct GoToStatement {
    /// Target(s).
    pub targets: Vec<String>,
    /// DEPENDING ON variable (for GO TO DEPENDING).
    pub depending: Option<QualifiedName>,
    /// Source span.
    pub span: Span,
}

/// INITIALIZE statement.
#[derive(Debug, Clone, PartialEq)]
pub struct InitializeStatement {
    /// Variables to initialize.
    pub variables: Vec<QualifiedName>,
    /// REPLACING clauses.
    pub replacing: Vec<InitializeReplacing>,
    /// Source span.
    pub span: Span,
}

/// REPLACING clause for INITIALIZE.
#[derive(Debug, Clone, PartialEq)]
pub struct InitializeReplacing {
    /// Category to replace.
    pub category: InitializeCategory,
    /// Replacement value.
    pub value: Expression,
}

/// Category for INITIALIZE REPLACING.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InitializeCategory {
    /// ALPHABETIC.
    Alphabetic,
    /// ALPHANUMERIC.
    Alphanumeric,
    /// NUMERIC.
    Numeric,
    /// ALPHANUMERIC-EDITED.
    AlphanumericEdited,
    /// NUMERIC-EDITED.
    NumericEdited,
}

/// INSPECT statement.
#[derive(Debug, Clone, PartialEq)]
pub struct InspectStatement {
    /// Target variable.
    pub target: QualifiedName,
    /// TALLYING clause.
    pub tallying: Option<InspectTallying>,
    /// REPLACING clause.
    pub replacing: Option<InspectReplacing>,
    /// CONVERTING clause.
    pub converting: Option<InspectConverting>,
    /// Source span.
    pub span: Span,
}

/// TALLYING clause for INSPECT.
#[derive(Debug, Clone, PartialEq)]
pub struct InspectTallying {
    /// Counter variable.
    pub counter: QualifiedName,
    /// FOR clauses.
    pub for_clauses: Vec<InspectFor>,
}

/// FOR clause in INSPECT TALLYING.
#[derive(Debug, Clone, PartialEq)]
pub struct InspectFor {
    /// CHARACTERS, ALL, or LEADING.
    pub mode: InspectMode,
    /// Pattern to count/match.
    pub pattern: Option<Expression>,
    /// BEFORE/AFTER clause.
    pub delimiters: Vec<InspectDelimiter>,
}

/// INSPECT mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InspectMode {
    /// CHARACTERS.
    Characters,
    /// ALL.
    All,
    /// LEADING.
    Leading,
    /// FIRST.
    First,
}

/// BEFORE/AFTER delimiter for INSPECT.
#[derive(Debug, Clone, PartialEq)]
pub struct InspectDelimiter {
    /// BEFORE (true) or AFTER (false).
    pub before: bool,
    /// INITIAL keyword present.
    pub initial: bool,
    /// Delimiter value.
    pub value: Expression,
}

/// REPLACING clause for INSPECT.
#[derive(Debug, Clone, PartialEq)]
pub struct InspectReplacing {
    /// Replacement rules.
    pub rules: Vec<InspectReplacingRule>,
}

/// A replacement rule in INSPECT REPLACING.
#[derive(Debug, Clone, PartialEq)]
pub struct InspectReplacingRule {
    /// Mode (CHARACTERS, ALL, LEADING, FIRST).
    pub mode: InspectMode,
    /// Pattern to replace (None for CHARACTERS).
    pub pattern: Option<Expression>,
    /// Replacement value.
    pub by: Expression,
    /// BEFORE/AFTER clause.
    pub delimiters: Vec<InspectDelimiter>,
}

/// CONVERTING clause for INSPECT.
#[derive(Debug, Clone, PartialEq)]
pub struct InspectConverting {
    /// From characters.
    pub from: Expression,
    /// To characters.
    pub to: Expression,
    /// BEFORE/AFTER clause.
    pub delimiters: Vec<InspectDelimiter>,
}

/// STRING statement.
#[derive(Debug, Clone, PartialEq)]
pub struct StringStatement {
    /// Source items.
    pub sources: Vec<StringSource>,
    /// Target variable.
    pub into: QualifiedName,
    /// POINTER variable.
    pub pointer: Option<QualifiedName>,
    /// ON OVERFLOW handler.
    pub on_overflow: Option<Vec<Statement>>,
    /// NOT ON OVERFLOW handler.
    pub not_on_overflow: Option<Vec<Statement>>,
    /// Source span.
    pub span: Span,
}

/// Source item for STRING.
#[derive(Debug, Clone, PartialEq)]
pub struct StringSource {
    /// Source value.
    pub value: Expression,
    /// Delimiter.
    pub delimited_by: StringDelimiter,
}

/// Delimiter for STRING.
#[derive(Debug, Clone, PartialEq)]
pub enum StringDelimiter {
    /// SIZE (entire field).
    Size,
    /// Specific delimiter value.
    Value(Expression),
}

/// UNSTRING statement.
#[derive(Debug, Clone, PartialEq)]
pub struct UnstringStatement {
    /// Source string.
    pub source: QualifiedName,
    /// Delimiters.
    pub delimiters: Vec<UnstringDelimiter>,
    /// Target fields.
    pub into: Vec<UnstringTarget>,
    /// POINTER variable.
    pub pointer: Option<QualifiedName>,
    /// TALLYING variable.
    pub tallying: Option<QualifiedName>,
    /// ON OVERFLOW handler.
    pub on_overflow: Option<Vec<Statement>>,
    /// NOT ON OVERFLOW handler.
    pub not_on_overflow: Option<Vec<Statement>>,
    /// Source span.
    pub span: Span,
}

/// Delimiter for UNSTRING.
#[derive(Debug, Clone, PartialEq)]
pub struct UnstringDelimiter {
    /// ALL keyword present.
    pub all: bool,
    /// Delimiter value.
    pub value: Expression,
}

/// Target field for UNSTRING.
#[derive(Debug, Clone, PartialEq)]
pub struct UnstringTarget {
    /// Target variable.
    pub name: QualifiedName,
    /// DELIMITER IN variable.
    pub delimiter_in: Option<QualifiedName>,
    /// COUNT IN variable.
    pub count_in: Option<QualifiedName>,
}

/// SET statement.
#[derive(Debug, Clone, PartialEq)]
pub struct SetStatement {
    /// SET mode.
    pub mode: SetMode,
    /// Source span.
    pub span: Span,
}

/// SET mode variants.
#[derive(Debug, Clone, PartialEq)]
pub enum SetMode {
    /// SET index TO value.
    IndexTo {
        targets: Vec<QualifiedName>,
        value: Expression,
    },
    /// SET index UP/DOWN BY value.
    IndexUpDown {
        targets: Vec<QualifiedName>,
        up: bool,
        value: Expression,
    },
    /// SET condition TO TRUE/FALSE.
    ConditionTo { target: QualifiedName, value: bool },
    /// SET pointer TO ADDRESS OF.
    AddressOf {
        target: QualifiedName,
        source: QualifiedName,
    },
}

/// CANCEL statement.
#[derive(Debug, Clone, PartialEq)]
pub struct CancelStatement {
    /// Program names to cancel.
    pub programs: Vec<Expression>,
    /// Source span.
    pub span: Span,
}

/// SORT statement.
#[derive(Debug, Clone, PartialEq)]
pub struct SortStatement {
    /// Sort file/work area name.
    pub file: String,
    /// Sort keys (ascending/descending + field).
    pub keys: Vec<SortKey>,
    /// INPUT PROCEDURE paragraph/section.
    pub input_procedure: Option<String>,
    /// OUTPUT PROCEDURE paragraph/section.
    pub output_procedure: Option<String>,
    /// USING file names.
    pub using: Vec<String>,
    /// GIVING file names.
    pub giving: Vec<String>,
    /// Source span.
    pub span: Span,
}

/// A sort key.
#[derive(Debug, Clone, PartialEq)]
pub struct SortKey {
    /// Field name.
    pub field: QualifiedName,
    /// Ascending (true) or descending (false).
    pub ascending: bool,
}

/// MERGE statement.
#[derive(Debug, Clone, PartialEq)]
pub struct MergeStatement {
    /// Merge file name.
    pub file: String,
    /// Merge keys.
    pub keys: Vec<SortKey>,
    /// USING file names.
    pub using: Vec<String>,
    /// GIVING file names.
    pub giving: Vec<String>,
    /// OUTPUT PROCEDURE.
    pub output_procedure: Option<String>,
    /// Source span.
    pub span: Span,
}

/// RELEASE statement.
#[derive(Debug, Clone, PartialEq)]
pub struct ReleaseStatement {
    /// Record name.
    pub record: QualifiedName,
    /// FROM clause source.
    pub from: Option<Expression>,
    /// Source span.
    pub span: Span,
}

/// RETURN statement (file I/O).
#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    /// File name.
    pub file: String,
    /// INTO clause target.
    pub into: Option<QualifiedName>,
    /// AT END handler.
    pub at_end: Option<Vec<Statement>>,
    /// NOT AT END handler.
    pub not_at_end: Option<Vec<Statement>>,
    /// Source span.
    pub span: Span,
}

/// SEARCH statement.
#[derive(Debug, Clone, PartialEq)]
pub struct SearchStatement {
    /// Table to search.
    pub table: QualifiedName,
    /// VARYING clause.
    pub varying: Option<QualifiedName>,
    /// SEARCH ALL (binary search).
    pub all: bool,
    /// AT END handler.
    pub at_end: Option<Vec<Statement>>,
    /// WHEN clauses.
    pub when_clauses: Vec<SearchWhen>,
    /// Source span.
    pub span: Span,
}

/// WHEN clause for SEARCH.
#[derive(Debug, Clone, PartialEq)]
pub struct SearchWhen {
    /// Condition.
    pub condition: Condition,
    /// Statements to execute.
    pub statements: Vec<Statement>,
    /// Source span.
    pub span: Span,
}

/// CONTINUE statement.
#[derive(Debug, Clone, PartialEq)]
pub struct ContinueStatement {
    /// Source span.
    pub span: Span,
}

/// EXEC CICS statement.
///
/// Represents embedded CICS commands like SEND MAP, RECEIVE MAP, RETURN, etc.
#[derive(Debug, Clone, PartialEq)]
pub struct ExecCicsStatement {
    /// The CICS command (e.g., "SEND", "RECEIVE", "RETURN", "XCTL").
    pub command: String,
    /// Command options as key-value pairs.
    pub options: Vec<CicsOption>,
    /// Source span.
    pub span: Span,
}

/// CICS command option.
#[derive(Debug, Clone, PartialEq)]
pub struct CicsOption {
    /// Option name (e.g., "MAP", "MAPSET", "FROM", "ERASE").
    pub name: String,
    /// Option value (if present).
    pub value: Option<Expression>,
}

/// EXEC SQL statement.
///
/// Represents embedded SQL commands.
#[derive(Debug, Clone, PartialEq)]
pub struct ExecSqlStatement {
    /// The SQL statement text.
    pub sql: String,
    /// Source span.
    pub span: Span,
}
