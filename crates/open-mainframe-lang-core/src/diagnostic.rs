//! Diagnostic types for language compiler error/warning reporting.
//!
//! These types provide a uniform way for all language crates to report
//! errors, warnings, and informational messages with source location context.

use std::fmt;

use crate::span::Span;

/// Diagnostic severity level.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    /// Error — prevents successful compilation/execution.
    Error,
    /// Warning — compilation continues but something looks suspicious.
    Warning,
    /// Informational — not a problem, but worth noting.
    Info,
}

/// A diagnostic message from the compiler or interpreter.
///
/// Diagnostics are produced during semantic analysis, type checking, or
/// other validation phases. Each diagnostic carries a source location,
/// severity, and human-readable message.
///
/// # Example
///
/// ```
/// use open_mainframe_lang_core::{Diagnostic, Severity, Span};
///
/// let d = Diagnostic::error("E001", "Undefined variable 'WS-NAME'", Span::main(100, 107))
///     .with_suggestion("Did you mean 'WS-FNAME'?");
///
/// assert_eq!(d.severity, Severity::Error);
/// assert_eq!(d.code, "E001");
/// assert!(d.suggestion.is_some());
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    /// Severity of the diagnostic.
    pub severity: Severity,
    /// Error code (e.g., "E001", "W003", "COBOL-E001", "JCL-W001").
    pub code: String,
    /// Human-readable message describing the issue.
    pub message: String,
    /// Source location where the issue was found.
    pub span: Span,
    /// Optional suggestion for how to fix the issue.
    pub suggestion: Option<String>,
}

impl Diagnostic {
    /// Create a new error diagnostic.
    pub fn error(code: impl Into<String>, message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Error,
            code: code.into(),
            message: message.into(),
            span,
            suggestion: None,
        }
    }

    /// Create a new warning diagnostic.
    pub fn warning(code: impl Into<String>, message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Warning,
            code: code.into(),
            message: message.into(),
            span,
            suggestion: None,
        }
    }

    /// Create a new info diagnostic.
    pub fn info(code: impl Into<String>, message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Info,
            code: code.into(),
            message: message.into(),
            span,
            suggestion: None,
        }
    }

    /// Add a suggestion to this diagnostic.
    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestion = Some(suggestion.into());
        self
    }

    /// Returns `true` if this diagnostic is an error.
    pub fn is_error(&self) -> bool {
        self.severity == Severity::Error
    }

    /// Returns `true` if this diagnostic is a warning.
    pub fn is_warning(&self) -> bool {
        self.severity == Severity::Warning
    }
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Severity::Error => write!(f, "error"),
            Severity::Warning => write!(f, "warning"),
            Severity::Info => write!(f, "info"),
        }
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}[{}]: {}", self.severity, self.code, self.message)?;
        if let Some(ref suggestion) = self.suggestion {
            write!(f, " ({})", suggestion)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Span;

    #[test]
    fn test_diagnostic_error() {
        let d = Diagnostic::error("E001", "Something went wrong", Span::main(0, 10));
        assert_eq!(d.severity, Severity::Error);
        assert_eq!(d.code, "E001");
        assert_eq!(d.message, "Something went wrong");
        assert!(d.is_error());
        assert!(!d.is_warning());
        assert!(d.suggestion.is_none());
    }

    #[test]
    fn test_diagnostic_warning() {
        let d = Diagnostic::warning("W001", "Unused variable", Span::main(5, 15));
        assert_eq!(d.severity, Severity::Warning);
        assert!(d.is_warning());
        assert!(!d.is_error());
    }

    #[test]
    fn test_diagnostic_info() {
        let d = Diagnostic::info("I001", "Consider using COMP-5", Span::main(0, 5));
        assert_eq!(d.severity, Severity::Info);
        assert!(!d.is_error());
        assert!(!d.is_warning());
    }

    #[test]
    fn test_diagnostic_with_suggestion() {
        let d = Diagnostic::error("E002", "Unknown keyword", Span::main(0, 5))
            .with_suggestion("Did you mean 'MOVE'?");
        assert_eq!(d.suggestion.as_deref(), Some("Did you mean 'MOVE'?"));
    }

    #[test]
    fn test_severity_display() {
        assert_eq!(format!("{}", Severity::Error), "error");
        assert_eq!(format!("{}", Severity::Warning), "warning");
        assert_eq!(format!("{}", Severity::Info), "info");
    }

    #[test]
    fn test_diagnostic_display() {
        let d = Diagnostic::error("E001", "Undefined variable", Span::main(0, 10));
        assert_eq!(format!("{}", d), "error[E001]: Undefined variable");
    }

    #[test]
    fn test_diagnostic_display_with_suggestion() {
        let d = Diagnostic::warning("W001", "Unused variable", Span::main(0, 5))
            .with_suggestion("Remove or prefix with _");
        assert_eq!(
            format!("{}", d),
            "warning[W001]: Unused variable (Remove or prefix with _)"
        );
    }

    #[test]
    fn test_diagnostic_partial_eq() {
        let d1 = Diagnostic::error("E001", "Test", Span::main(0, 5));
        let d2 = Diagnostic::error("E001", "Test", Span::main(0, 5));
        assert_eq!(d1, d2);
    }
}
