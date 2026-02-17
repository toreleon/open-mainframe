//! Migration assessment tool for mainframe COBOL programs.
//!
//! This crate provides tools to analyze COBOL source code and generate
//! migration assessment reports including:
//! - Code complexity metrics
//! - Feature compatibility analysis
//! - Migration effort estimation
//! - Recommendations for modernization
//!
//! # Example
//!
//! ```ignore
//! use open_mainframe_assess::{Analyzer, Report};
//!
//! let analyzer = Analyzer::new();
//! let report = analyzer.analyze_file("CUSTMAST.cbl")?;
//! println!("{}", report.summary());
//! ```

pub mod analyzer;
pub mod ast_analyzer;
pub mod callgraph;
pub mod cics_inventory;
pub mod metrics;
pub mod report;
pub mod compatibility;
pub mod scanner;
pub mod sql_analysis;

pub use analyzer::{Analyzer, AnalysisResult};
pub use ast_analyzer::AstAnalyzer;
pub use callgraph::{CallGraph, CallEdge, CallType};
pub use cics_inventory::{CicsInventory, CicsCommand, CicsCategory, SupportStatus};
pub use sql_analysis::{SqlAnalysis, SqlStatement, SqlComplexity};
pub use metrics::{CodeMetrics, ComplexityLevel};
pub use report::{Report, ReportFormat};
pub use compatibility::{CompatibilityChecker, CompatibilityIssue};
pub use scanner::{Scanner, ScanConfig, ScanResult};

use thiserror::Error;

/// Errors that can occur during assessment.
#[derive(Error, Debug)]
pub enum AssessError {
    /// File not found
    #[error("File not found: {0}")]
    FileNotFound(String),

    /// Parse error
    #[error("Parse error at line {line}: {message}")]
    ParseError { line: usize, message: String },

    /// IO error
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    /// Analysis error
    #[error("Analysis error: {0}")]
    AnalysisError(String),
}

/// Result type for assessment operations.
pub type AssessResult<T> = Result<T, AssessError>;

/// Migration complexity rating.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize)]
pub enum MigrationComplexity {
    /// Simple migration with minimal changes
    Low,
    /// Moderate complexity requiring some refactoring
    Medium,
    /// Complex migration requiring significant work
    High,
    /// Very complex, may require redesign
    VeryHigh,
}

impl MigrationComplexity {
    /// Get description.
    pub fn description(&self) -> &'static str {
        match self {
            MigrationComplexity::Low => "Simple migration with minimal changes",
            MigrationComplexity::Medium => "Moderate complexity, some refactoring needed",
            MigrationComplexity::High => "Complex migration, significant work required",
            MigrationComplexity::VeryHigh => "Very complex, may require redesign",
        }
    }

    /// Get estimated effort multiplier.
    pub fn effort_multiplier(&self) -> f64 {
        match self {
            MigrationComplexity::Low => 1.0,
            MigrationComplexity::Medium => 2.0,
            MigrationComplexity::High => 4.0,
            MigrationComplexity::VeryHigh => 8.0,
        }
    }
}

/// Feature category for compatibility checking.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize)]
pub enum FeatureCategory {
    /// Core COBOL language features
    CoreLanguage,
    /// File handling (VSAM, sequential, etc.)
    FileHandling,
    /// Database access (DB2, IMS)
    Database,
    /// Transaction processing (CICS)
    Transaction,
    /// Batch processing (JCL, utilities)
    Batch,
    /// Interoperability (CALL, subprograms)
    Interoperability,
    /// Platform-specific features
    PlatformSpecific,
}

impl FeatureCategory {
    /// Get display name.
    pub fn name(&self) -> &'static str {
        match self {
            FeatureCategory::CoreLanguage => "Core Language",
            FeatureCategory::FileHandling => "File Handling",
            FeatureCategory::Database => "Database",
            FeatureCategory::Transaction => "Transaction Processing",
            FeatureCategory::Batch => "Batch Processing",
            FeatureCategory::Interoperability => "Interoperability",
            FeatureCategory::PlatformSpecific => "Platform Specific",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_migration_complexity() {
        assert!(MigrationComplexity::Low < MigrationComplexity::Medium);
        assert!(MigrationComplexity::Medium < MigrationComplexity::High);
        assert_eq!(MigrationComplexity::Low.effort_multiplier(), 1.0);
        assert_eq!(MigrationComplexity::VeryHigh.effort_multiplier(), 8.0);
    }

    #[test]
    fn test_feature_category() {
        assert_eq!(FeatureCategory::CoreLanguage.name(), "Core Language");
        assert_eq!(FeatureCategory::Database.name(), "Database");
    }
}
