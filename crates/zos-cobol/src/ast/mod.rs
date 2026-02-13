//! Abstract Syntax Tree (AST) definitions for COBOL programs.
//!
//! This module defines the complete AST structure for COBOL programs,
//! including all four divisions and their various sections and statements.
//!
//! All AST nodes include a `span` field for source location tracking.

use crate::lexer::Span;

/// A complete COBOL program.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    /// The IDENTIFICATION DIVISION.
    pub identification: IdentificationDivision,
    /// The ENVIRONMENT DIVISION (optional).
    pub environment: Option<EnvironmentDivision>,
    /// The DATA DIVISION (optional).
    pub data: Option<DataDivision>,
    /// The PROCEDURE DIVISION (optional for copybooks).
    pub procedure: Option<ProcedureDivision>,
    /// Source span for the entire program.
    pub span: Span,
}

// ============================================================================
// IDENTIFICATION DIVISION
// ============================================================================

/// The IDENTIFICATION DIVISION of a COBOL program.
#[derive(Debug, Clone, PartialEq)]
pub struct IdentificationDivision {
    /// The PROGRAM-ID (required).
    pub program_id: ProgramId,
    /// AUTHOR paragraph (optional).
    pub author: Option<String>,
    /// INSTALLATION paragraph (optional).
    pub installation: Option<String>,
    /// DATE-WRITTEN paragraph (optional).
    pub date_written: Option<String>,
    /// DATE-COMPILED paragraph (optional).
    pub date_compiled: Option<String>,
    /// SECURITY paragraph (optional).
    pub security: Option<String>,
    /// Source span.
    pub span: Span,
}

/// The PROGRAM-ID paragraph.
#[derive(Debug, Clone, PartialEq)]
pub struct ProgramId {
    /// The program name.
    pub name: String,
    /// Whether the program is COMMON.
    pub is_common: bool,
    /// Whether the program is INITIAL.
    pub is_initial: bool,
    /// Source span.
    pub span: Span,
}

// ============================================================================
// ENVIRONMENT DIVISION
// ============================================================================

/// The ENVIRONMENT DIVISION of a COBOL program.
#[derive(Debug, Clone, PartialEq)]
pub struct EnvironmentDivision {
    /// CONFIGURATION SECTION (optional).
    pub configuration: Option<ConfigurationSection>,
    /// INPUT-OUTPUT SECTION (optional).
    pub input_output: Option<InputOutputSection>,
    /// Source span.
    pub span: Span,
}

/// The CONFIGURATION SECTION.
#[derive(Debug, Clone, PartialEq)]
pub struct ConfigurationSection {
    /// SOURCE-COMPUTER paragraph.
    pub source_computer: Option<String>,
    /// OBJECT-COMPUTER paragraph.
    pub object_computer: Option<String>,
    /// SPECIAL-NAMES paragraph entries.
    pub special_names: Vec<SpecialName>,
    /// Source span.
    pub span: Span,
}

/// A SPECIAL-NAMES entry.
#[derive(Debug, Clone, PartialEq)]
pub struct SpecialName {
    /// The system name.
    pub system_name: String,
    /// The user-defined name.
    pub user_name: String,
    /// Source span.
    pub span: Span,
}

/// The INPUT-OUTPUT SECTION.
#[derive(Debug, Clone, PartialEq)]
pub struct InputOutputSection {
    /// FILE-CONTROL entries.
    pub file_control: Vec<FileControlEntry>,
    /// Source span.
    pub span: Span,
}

/// A FILE-CONTROL entry (SELECT statement).
#[derive(Debug, Clone, PartialEq)]
pub struct FileControlEntry {
    /// The file name.
    pub file_name: String,
    /// The ASSIGN TO clause value.
    pub assign_to: String,
    /// File organization.
    pub organization: FileOrganization,
    /// Access mode.
    pub access_mode: AccessMode,
    /// Record key (for indexed files).
    pub record_key: Option<QualifiedName>,
    /// File status variable.
    pub file_status: Option<QualifiedName>,
    /// Source span.
    pub span: Span,
}

/// File organization type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FileOrganization {
    /// Sequential file (default).
    #[default]
    Sequential,
    /// Indexed file.
    Indexed,
    /// Relative file.
    Relative,
    /// Line sequential (extension).
    LineSequential,
}

/// File access mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum AccessMode {
    /// Sequential access (default).
    #[default]
    Sequential,
    /// Random access.
    Random,
    /// Dynamic access.
    Dynamic,
}

// ============================================================================
// DATA DIVISION
// ============================================================================

/// The DATA DIVISION of a COBOL program.
#[derive(Debug, Clone, PartialEq)]
pub struct DataDivision {
    /// FILE SECTION entries.
    pub file_section: Vec<FileDescription>,
    /// WORKING-STORAGE SECTION entries.
    pub working_storage: Vec<DataItem>,
    /// LOCAL-STORAGE SECTION entries.
    pub local_storage: Vec<DataItem>,
    /// LINKAGE SECTION entries.
    pub linkage: Vec<DataItem>,
    /// Source span.
    pub span: Span,
}

/// A file description (FD or SD).
#[derive(Debug, Clone, PartialEq)]
pub struct FileDescription {
    /// The file name.
    pub name: String,
    /// Whether this is a sort file (SD vs FD).
    pub is_sort_file: bool,
    /// Record descriptions.
    pub records: Vec<DataItem>,
    /// Record contains clause.
    pub record_contains: Option<RecordContains>,
    /// Block contains clause.
    pub block_contains: Option<BlockContains>,
    /// Source span.
    pub span: Span,
}

/// RECORD CONTAINS clause.
#[derive(Debug, Clone, PartialEq)]
pub struct RecordContains {
    /// Minimum record size.
    pub min: u32,
    /// Maximum record size (if different from min).
    pub max: Option<u32>,
    /// Source span.
    pub span: Span,
}

/// BLOCK CONTAINS clause.
#[derive(Debug, Clone, PartialEq)]
pub struct BlockContains {
    /// Number of records or characters.
    pub size: u32,
    /// Whether size is in records (true) or characters (false).
    pub records: bool,
    /// Source span.
    pub span: Span,
}

/// A data item (variable declaration).
#[derive(Debug, Clone, PartialEq)]
pub struct DataItem {
    /// Level number (01-49, 66, 77, 88).
    pub level: u8,
    /// Item name (or FILLER).
    pub name: DataItemName,
    /// PICTURE clause.
    pub picture: Option<PictureClause>,
    /// USAGE clause.
    pub usage: Option<Usage>,
    /// VALUE clause.
    pub value: Option<Literal>,
    /// OCCURS clause.
    pub occurs: Option<OccursClause>,
    /// REDEFINES clause.
    pub redefines: Option<QualifiedName>,
    /// SIGN clause.
    pub sign: Option<SignClause>,
    /// JUSTIFIED clause.
    pub justified: bool,
    /// BLANK WHEN ZERO clause.
    pub blank_when_zero: bool,
    /// Subordinate items (for group items).
    pub children: Vec<DataItem>,
    /// Condition names (level 88 items).
    pub condition_values: Vec<ConditionValue>,
    /// Source span.
    pub span: Span,
}

/// Data item name.
#[derive(Debug, Clone, PartialEq)]
pub enum DataItemName {
    /// A named data item.
    Named(String),
    /// FILLER (unnamed data item).
    Filler,
}

impl DataItemName {
    /// Get the name as a string, or None for FILLER.
    pub fn as_str(&self) -> Option<&str> {
        match self {
            DataItemName::Named(s) => Some(s),
            DataItemName::Filler => None,
        }
    }
}

/// PICTURE clause.
#[derive(Debug, Clone, PartialEq)]
pub struct PictureClause {
    /// The picture string.
    pub picture: String,
    /// Parsed category.
    pub category: PictureCategory,
    /// Total size in bytes.
    pub size: u32,
    /// Decimal positions (for numeric).
    pub decimal_positions: u32,
    /// Source span.
    pub span: Span,
}

/// Picture category (numeric, alphabetic, alphanumeric, etc.).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PictureCategory {
    /// Alphabetic (A).
    Alphabetic,
    /// Alphanumeric (X).
    Alphanumeric,
    /// Alphanumeric edited.
    AlphanumericEdited,
    /// Numeric (9).
    Numeric,
    /// Numeric edited.
    NumericEdited,
}

/// USAGE clause.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Usage {
    /// DISPLAY (default).
    Display,
    /// BINARY / COMP / COMP-4.
    Binary,
    /// COMP-1 (single precision float).
    Comp1,
    /// COMP-2 (double precision float).
    Comp2,
    /// COMP-3 / PACKED-DECIMAL.
    PackedDecimal,
    /// COMP-5 (native binary).
    Comp5,
    /// POINTER.
    Pointer,
    /// INDEX.
    Index,
}

/// OCCURS clause.
#[derive(Debug, Clone, PartialEq)]
pub struct OccursClause {
    /// Fixed number of occurrences (or minimum for DEPENDING ON).
    pub times: u32,
    /// Maximum occurrences (for DEPENDING ON).
    pub max_times: Option<u32>,
    /// DEPENDING ON variable.
    pub depending_on: Option<QualifiedName>,
    /// INDEXED BY names.
    pub indexed_by: Vec<String>,
    /// KEY clause.
    pub keys: Vec<OccursKey>,
    /// Source span.
    pub span: Span,
}

/// OCCURS KEY clause.
#[derive(Debug, Clone, PartialEq)]
pub struct OccursKey {
    /// Key name.
    pub name: QualifiedName,
    /// Whether ASCENDING (true) or DESCENDING (false).
    pub ascending: bool,
    /// Source span.
    pub span: Span,
}

/// SIGN clause.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SignClause {
    /// LEADING or TRAILING.
    pub leading: bool,
    /// SEPARATE CHARACTER.
    pub separate: bool,
}

/// Condition value (level 88 item).
#[derive(Debug, Clone, PartialEq)]
pub struct ConditionValue {
    /// The condition name.
    pub name: String,
    /// The value(s).
    pub values: Vec<ConditionValueEntry>,
    /// Source span.
    pub span: Span,
}

/// A single condition value entry.
#[derive(Debug, Clone, PartialEq)]
pub enum ConditionValueEntry {
    /// Single value.
    Single(Literal),
    /// Range (THROUGH/THRU).
    Range { from: Literal, to: Literal },
}

// ============================================================================
// PROCEDURE DIVISION
// ============================================================================

/// The PROCEDURE DIVISION of a COBOL program.
#[derive(Debug, Clone, PartialEq)]
pub struct ProcedureDivision {
    /// USING clause parameters.
    pub using: Vec<UsingParameter>,
    /// RETURNING clause.
    pub returning: Option<QualifiedName>,
    /// Sections and paragraphs.
    pub body: ProcedureBody,
    /// Source span.
    pub span: Span,
}

/// USING clause parameter.
#[derive(Debug, Clone, PartialEq)]
pub struct UsingParameter {
    /// Parameter name.
    pub name: QualifiedName,
    /// BY REFERENCE, BY CONTENT, or BY VALUE.
    pub mode: ParameterMode,
    /// Source span.
    pub span: Span,
}

/// Parameter passing mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ParameterMode {
    /// BY REFERENCE (default).
    #[default]
    Reference,
    /// BY CONTENT.
    Content,
    /// BY VALUE.
    Value,
}

/// The body of the PROCEDURE DIVISION.
#[derive(Debug, Clone, PartialEq)]
pub enum ProcedureBody {
    /// Structured form: sections containing paragraphs.
    Sections(Vec<Section>),
    /// Flat form: just paragraphs (no sections).
    Paragraphs(Vec<Paragraph>),
    /// Inline: just statements (no paragraphs).
    Statements(Vec<Statement>),
}

/// A section in the PROCEDURE DIVISION.
#[derive(Debug, Clone, PartialEq)]
pub struct Section {
    /// Section name.
    pub name: String,
    /// Paragraphs in this section.
    pub paragraphs: Vec<Paragraph>,
    /// Source span.
    pub span: Span,
}

/// A paragraph in the PROCEDURE DIVISION.
#[derive(Debug, Clone, PartialEq)]
pub struct Paragraph {
    /// Paragraph name.
    pub name: String,
    /// Statements in this paragraph.
    pub statements: Vec<Statement>,
    /// Source span.
    pub span: Span,
}

// ============================================================================
// STATEMENTS
// ============================================================================

/// A COBOL statement.
#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum Statement {
    /// MOVE statement.
    Move(MoveStatement),
    /// COMPUTE statement.
    Compute(ComputeStatement),
    /// ADD statement.
    Add(AddStatement),
    /// SUBTRACT statement.
    Subtract(SubtractStatement),
    /// MULTIPLY statement.
    Multiply(MultiplyStatement),
    /// DIVIDE statement.
    Divide(DivideStatement),
    /// IF statement.
    If(IfStatement),
    /// EVALUATE statement.
    Evaluate(EvaluateStatement),
    /// PERFORM statement.
    Perform(PerformStatement),
    /// CALL statement.
    Call(CallStatement),
    /// DISPLAY statement.
    Display(DisplayStatement),
    /// ACCEPT statement.
    Accept(AcceptStatement),
    /// OPEN statement.
    Open(OpenStatement),
    /// CLOSE statement.
    Close(CloseStatement),
    /// READ statement.
    Read(ReadStatement),
    /// WRITE statement.
    Write(WriteStatement),
    /// STOP RUN statement.
    StopRun(StopRunStatement),
    /// EXIT statement.
    Exit(ExitStatement),
    /// GO TO statement.
    GoTo(GoToStatement),
    /// INITIALIZE statement.
    Initialize(InitializeStatement),
    /// INSPECT statement.
    Inspect(InspectStatement),
    /// STRING statement.
    String(StringStatement),
    /// UNSTRING statement.
    Unstring(UnstringStatement),
    /// SET statement.
    Set(SetStatement),
    /// SEARCH statement.
    Search(SearchStatement),
    /// CONTINUE statement.
    Continue(ContinueStatement),
    /// EXEC CICS statement.
    ExecCics(ExecCicsStatement),
    /// EXEC SQL statement.
    ExecSql(ExecSqlStatement),
}

impl Statement {
    /// Get the span of this statement.
    pub fn span(&self) -> Span {
        match self {
            Statement::Move(s) => s.span,
            Statement::Compute(s) => s.span,
            Statement::Add(s) => s.span,
            Statement::Subtract(s) => s.span,
            Statement::Multiply(s) => s.span,
            Statement::Divide(s) => s.span,
            Statement::If(s) => s.span,
            Statement::Evaluate(s) => s.span,
            Statement::Perform(s) => s.span,
            Statement::Call(s) => s.span,
            Statement::Display(s) => s.span,
            Statement::Accept(s) => s.span,
            Statement::Open(s) => s.span,
            Statement::Close(s) => s.span,
            Statement::Read(s) => s.span,
            Statement::Write(s) => s.span,
            Statement::StopRun(s) => s.span,
            Statement::Exit(s) => s.span,
            Statement::GoTo(s) => s.span,
            Statement::Initialize(s) => s.span,
            Statement::Inspect(s) => s.span,
            Statement::String(s) => s.span,
            Statement::Unstring(s) => s.span,
            Statement::Set(s) => s.span,
            Statement::Search(s) => s.span,
            Statement::Continue(s) => s.span,
            Statement::ExecCics(s) => s.span,
            Statement::ExecSql(s) => s.span,
        }
    }
}

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
    pub mode: ParameterMode,
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

/// File open mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpenMode {
    /// INPUT.
    Input,
    /// OUTPUT.
    Output,
    /// I-O.
    InputOutput,
    /// EXTEND.
    Extend,
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

// ============================================================================
// EXPRESSIONS AND CONDITIONS
// ============================================================================

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
