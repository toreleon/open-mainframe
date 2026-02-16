//! Abstract Syntax Tree (AST) definitions for COBOL programs.
//!
//! This module defines the complete AST structure for COBOL programs,
//! including all four divisions and their various sections and statements.
//!
//! All AST nodes include a `span` field for source location tracking.

use crate::lexer::Span;
use open_mainframe_lang_core::AstNode;

pub mod data;
pub mod expressions;
pub mod statements;

pub use data::*;
pub use expressions::*;
pub use statements::*;

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
    /// Contained (nested) programs.
    pub contained_programs: Vec<Program>,
    /// Source span for the entire program.
    pub span: Span,
}

impl AstNode for Program {
    fn span(&self) -> Span {
        self.span
    }
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
    /// REPOSITORY paragraph.
    pub repository: Option<RepositoryParagraph>,
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

/// REPOSITORY paragraph.
#[derive(Debug, Clone, PartialEq)]
pub struct RepositoryParagraph {
    /// FUNCTION ALL INTRINSIC is active.
    pub function_all_intrinsic: bool,
    /// Individual function names available without FUNCTION keyword.
    pub functions: Vec<String>,
    /// Source span.
    pub span: Span,
}

/// The INPUT-OUTPUT SECTION.
#[derive(Debug, Clone, PartialEq)]
pub struct InputOutputSection {
    /// FILE-CONTROL entries.
    pub file_control: Vec<FileControlEntry>,
    /// I-O-CONTROL paragraph.
    pub io_control: Option<IoControlParagraph>,
    /// Source span.
    pub span: Span,
}

/// I-O-CONTROL paragraph.
#[derive(Debug, Clone, PartialEq)]
pub struct IoControlParagraph {
    /// SAME RECORD AREA clauses: groups of files sharing a record area.
    pub same_record_areas: Vec<Vec<String>>,
    /// APPLY WRITE-ONLY file names.
    pub apply_write_only: Vec<String>,
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
    /// Alternate record keys.
    pub alternate_keys: Vec<QualifiedName>,
    /// File status variable.
    pub file_status: Option<QualifiedName>,
    /// LOCK MODE clause.
    pub lock_mode: Option<LockMode>,
    /// RESERVE clause (number of areas).
    pub reserve: Option<u32>,
    /// PADDING CHARACTER.
    pub padding_character: Option<String>,
    /// Source span.
    pub span: Span,
}

/// LOCK MODE clause.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LockMode {
    /// LOCK MODE IS MANUAL.
    Manual,
    /// LOCK MODE IS AUTOMATIC.
    Automatic,
    /// LOCK MODE IS EXCLUSIVE.
    Exclusive,
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
// PROCEDURE DIVISION
// ============================================================================

/// The PROCEDURE DIVISION of a COBOL program.
#[derive(Debug, Clone, PartialEq)]
pub struct ProcedureDivision {
    /// USING clause parameters.
    pub using: Vec<UsingParameter>,
    /// RETURNING clause.
    pub returning: Option<QualifiedName>,
    /// DECLARATIVES section (optional).
    pub declaratives: Vec<DeclarativeSection>,
    /// Sections and paragraphs.
    pub body: ProcedureBody,
    /// Source span.
    pub span: Span,
}

/// A declarative section (USE AFTER ERROR/EXCEPTION).
#[derive(Debug, Clone, PartialEq)]
pub struct DeclarativeSection {
    /// Section name.
    pub name: String,
    /// USE clause.
    pub use_clause: UseClause,
    /// Paragraphs in this declarative section.
    pub paragraphs: Vec<Paragraph>,
    /// Source span.
    pub span: Span,
}

/// USE AFTER ERROR/EXCEPTION clause.
#[derive(Debug, Clone, PartialEq)]
pub struct UseClause {
    /// Target file or open mode.
    pub target: UseTarget,
    /// Source span.
    pub span: Span,
}

/// Target of a USE clause.
#[derive(Debug, Clone, PartialEq)]
pub enum UseTarget {
    /// A specific file name.
    File(String),
    /// All INPUT files.
    Input,
    /// All OUTPUT files.
    Output,
    /// All I-O files.
    InputOutput,
    /// All EXTEND files.
    Extend,
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
