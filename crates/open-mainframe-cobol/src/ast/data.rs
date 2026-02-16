//! Data Division types for the COBOL AST.

use crate::lexer::Span;
use super::expressions::*;

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
    /// CODE-SET clause.
    pub code_set: Option<String>,
    /// LINAGE clause.
    pub linage: Option<LinageClause>,
    /// Source span.
    pub span: Span,
}

/// LINAGE clause for logical page control.
#[derive(Debug, Clone, PartialEq)]
pub struct LinageClause {
    /// Number of lines in the page body.
    pub lines: Expression,
    /// FOOTING AT line number.
    pub footing: Option<Expression>,
    /// LINES AT TOP.
    pub top: Option<Expression>,
    /// LINES AT BOTTOM.
    pub bottom: Option<Expression>,
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
    /// EXTERNAL clause (01 level only).
    pub external: bool,
    /// GLOBAL clause (01 level only).
    pub global: bool,
    /// SYNCHRONIZED clause.
    pub synchronized: Option<SyncDirection>,
    /// RENAMES clause (level 66): from_name THRU to_name.
    pub renames: Option<(QualifiedName, Option<QualifiedName>)>,
    /// DYNAMIC LENGTH clause (IBM COBOL v6.2+).
    pub dynamic_length: bool,
    /// GROUP-USAGE clause (NATIONAL).
    pub group_usage: Option<GroupUsage>,
    /// Subordinate items (for group items).
    pub children: Vec<DataItem>,
    /// Condition names (level 88 items).
    pub condition_values: Vec<ConditionValue>,
    /// Source span.
    pub span: Span,
}

/// GROUP-USAGE clause.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GroupUsage {
    /// GROUP-USAGE NATIONAL.
    National,
    /// GROUP-USAGE UTF-8.
    Utf8,
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
    /// UTF-8 (U) — IBM COBOL v6.2+.
    Utf8,
    /// National (N) — UTF-16 / DBCS.
    National,
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
    /// FUNCTION-POINTER.
    FunctionPointer,
    /// PROCEDURE-POINTER.
    ProcedurePointer,
    /// NATIONAL.
    National,
    /// UTF-8 — IBM COBOL v6.2+ for PIC U items.
    Utf8,
    /// DISPLAY-1 — DBCS (Double-Byte Character Set).
    Display1,
}

/// SYNCHRONIZED direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyncDirection {
    /// SYNCHRONIZED (unspecified).
    Default,
    /// SYNCHRONIZED LEFT.
    Left,
    /// SYNCHRONIZED RIGHT.
    Right,
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
