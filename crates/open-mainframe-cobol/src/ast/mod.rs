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
