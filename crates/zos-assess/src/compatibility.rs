//! Compatibility checking for migration.
//!
//! Identifies compatibility issues and provides recommendations.

use crate::FeatureCategory;

/// Severity of a compatibility issue.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize)]
pub enum Severity {
    /// Informational - no action required
    Info,
    /// Warning - may need attention
    Warning,
    /// High - likely needs changes
    High,
    /// Critical - must be addressed
    Critical,
}

impl Severity {
    /// Get display name.
    pub fn name(&self) -> &'static str {
        match self {
            Severity::Info => "Info",
            Severity::Warning => "Warning",
            Severity::High => "High",
            Severity::Critical => "Critical",
        }
    }
}

/// A compatibility issue found in the code.
#[derive(Debug, Clone, serde::Serialize)]
pub struct CompatibilityIssue {
    /// Issue code
    pub code: String,
    /// Issue description
    pub description: String,
    /// Severity
    pub severity: Severity,
    /// Category
    pub category: FeatureCategory,
    /// Line number (if applicable)
    pub line: Option<usize>,
    /// Recommendation
    pub recommendation: String,
}

impl CompatibilityIssue {
    /// Create a new issue.
    pub fn new(
        code: &str,
        description: &str,
        severity: Severity,
        category: FeatureCategory,
    ) -> Self {
        Self {
            code: code.to_string(),
            description: description.to_string(),
            severity,
            category,
            line: None,
            recommendation: String::new(),
        }
    }

    /// Set line number.
    pub fn at_line(mut self, line: usize) -> Self {
        self.line = Some(line);
        self
    }

    /// Set recommendation.
    pub fn with_recommendation(mut self, rec: &str) -> Self {
        self.recommendation = rec.to_string();
        self
    }
}

/// Compatibility checker rules.
pub struct CompatibilityRule {
    /// Rule code
    pub code: String,
    /// Pattern to match
    pub pattern: String,
    /// Issue description
    pub description: String,
    /// Severity level
    pub severity: Severity,
    /// Feature category
    pub category: FeatureCategory,
    /// Recommendation
    pub recommendation: String,
}

/// Compatibility checker for COBOL code.
pub struct CompatibilityChecker {
    rules: Vec<CompatibilityRule>,
}

impl CompatibilityChecker {
    /// Create a new compatibility checker.
    pub fn new() -> Self {
        Self {
            rules: Self::default_rules(),
        }
    }

    fn default_rules() -> Vec<CompatibilityRule> {
        vec![
            // Platform-specific features
            CompatibilityRule {
                code: "PLAT001".to_string(),
                pattern: "EXEC DLI".to_string(),
                description: "IMS/DL1 database calls detected".to_string(),
                severity: Severity::Critical,
                category: FeatureCategory::Database,
                recommendation: "IMS must be replaced with a supported database system".to_string(),
            },
            CompatibilityRule {
                code: "PLAT002".to_string(),
                pattern: "UPON CONSOLE".to_string(),
                description: "Console display may behave differently".to_string(),
                severity: Severity::Warning,
                category: FeatureCategory::PlatformSpecific,
                recommendation: "Review console output handling".to_string(),
            },
            CompatibilityRule {
                code: "PLAT003".to_string(),
                pattern: "ACCEPT FROM DATE".to_string(),
                description: "System date format may differ".to_string(),
                severity: Severity::Info,
                category: FeatureCategory::PlatformSpecific,
                recommendation: "Verify date format compatibility".to_string(),
            },
            CompatibilityRule {
                code: "PLAT004".to_string(),
                pattern: "ACCEPT FROM TIME".to_string(),
                description: "System time format may differ".to_string(),
                severity: Severity::Info,
                category: FeatureCategory::PlatformSpecific,
                recommendation: "Verify time format compatibility".to_string(),
            },
            // File handling
            CompatibilityRule {
                code: "FILE001".to_string(),
                pattern: "ORGANIZATION IS RELATIVE".to_string(),
                description: "Relative file organization detected".to_string(),
                severity: Severity::High,
                category: FeatureCategory::FileHandling,
                recommendation: "Relative files need special handling or conversion".to_string(),
            },
            CompatibilityRule {
                code: "FILE002".to_string(),
                pattern: "ASSIGN TO EXTERNAL".to_string(),
                description: "External file assignment".to_string(),
                severity: Severity::Warning,
                category: FeatureCategory::FileHandling,
                recommendation: "External assignments need environment variable mapping".to_string(),
            },
            // Deprecated features
            CompatibilityRule {
                code: "DEPR001".to_string(),
                pattern: "ALTER ".to_string(),
                description: "ALTER statement is deprecated and not recommended".to_string(),
                severity: Severity::High,
                category: FeatureCategory::CoreLanguage,
                recommendation: "Replace ALTER with structured control flow".to_string(),
            },
            CompatibilityRule {
                code: "DEPR002".to_string(),
                pattern: "GO TO DEPENDING".to_string(),
                description: "GO TO DEPENDING is discouraged".to_string(),
                severity: Severity::Warning,
                category: FeatureCategory::CoreLanguage,
                recommendation: "Consider using EVALUATE statement instead".to_string(),
            },
            CompatibilityRule {
                code: "DEPR003".to_string(),
                pattern: "ENTRY ".to_string(),
                description: "ENTRY statement for multiple entry points".to_string(),
                severity: Severity::Warning,
                category: FeatureCategory::Interoperability,
                recommendation: "Consider splitting into separate programs".to_string(),
            },
            // Interoperability
            CompatibilityRule {
                code: "CALL001".to_string(),
                pattern: "CALL USING BY CONTENT LENGTH".to_string(),
                description: "BY CONTENT LENGTH may have different behavior".to_string(),
                severity: Severity::Warning,
                category: FeatureCategory::Interoperability,
                recommendation: "Verify length calculation compatibility".to_string(),
            },
            // Performance
            CompatibilityRule {
                code: "PERF001".to_string(),
                pattern: "SEARCH ALL".to_string(),
                description: "Binary search requires sorted data".to_string(),
                severity: Severity::Info,
                category: FeatureCategory::CoreLanguage,
                recommendation: "Ensure data is properly sorted before SEARCH ALL".to_string(),
            },
        ]
    }

    /// Check source code for compatibility issues.
    pub fn check(&self, source: &str) -> Vec<CompatibilityIssue> {
        let mut issues = Vec::new();

        for (line_num, line) in source.lines().enumerate() {
            let upper = line.to_uppercase();

            for rule in &self.rules {
                if upper.contains(&rule.pattern) {
                    let issue = CompatibilityIssue::new(
                        &rule.code,
                        &rule.description,
                        rule.severity,
                        rule.category,
                    )
                    .at_line(line_num + 1)
                    .with_recommendation(&rule.recommendation);

                    issues.push(issue);
                }
            }
        }

        issues
    }

    /// Get all rules.
    pub fn rules(&self) -> &[CompatibilityRule] {
        &self.rules
    }

    /// Add a custom rule.
    pub fn add_rule(
        &mut self,
        code: &str,
        pattern: &str,
        description: &str,
        severity: Severity,
        category: FeatureCategory,
        recommendation: &str,
    ) {
        self.rules.push(CompatibilityRule {
            code: code.to_string(),
            pattern: pattern.to_string(),
            description: description.to_string(),
            severity,
            category,
            recommendation: recommendation.to_string(),
        });
    }
}

impl Default for CompatibilityChecker {
    fn default() -> Self {
        Self::new()
    }
}

/// Supported COBOL features.
pub struct FeatureSupport {
    /// Feature name
    pub feature: String,
    /// Is fully supported
    pub supported: bool,
    /// Support level (0-100)
    pub support_level: u8,
    /// Notes
    pub notes: String,
}

/// Get feature support information.
pub fn get_feature_support() -> Vec<FeatureSupport> {
    vec![
        FeatureSupport {
            feature: "Sequential Files".to_string(),
            supported: true,
            support_level: 100,
            notes: "Full support via zos-dataset".to_string(),
        },
        FeatureSupport {
            feature: "VSAM KSDS".to_string(),
            supported: true,
            support_level: 90,
            notes: "Emulated via zos-dataset VSAM module".to_string(),
        },
        FeatureSupport {
            feature: "VSAM ESDS".to_string(),
            supported: true,
            support_level: 90,
            notes: "Emulated via zos-dataset VSAM module".to_string(),
        },
        FeatureSupport {
            feature: "VSAM RRDS".to_string(),
            supported: true,
            support_level: 80,
            notes: "Emulated with some limitations".to_string(),
        },
        FeatureSupport {
            feature: "DB2 SQL".to_string(),
            supported: true,
            support_level: 85,
            notes: "Translated to PostgreSQL via zos-db2".to_string(),
        },
        FeatureSupport {
            feature: "CICS Commands".to_string(),
            supported: true,
            support_level: 75,
            notes: "Core commands supported via zos-cics".to_string(),
        },
        FeatureSupport {
            feature: "BMS Maps".to_string(),
            supported: true,
            support_level: 70,
            notes: "Basic BMS support via zos-cics bms module".to_string(),
        },
        FeatureSupport {
            feature: "IMS/DL1".to_string(),
            supported: false,
            support_level: 0,
            notes: "Not supported - requires database migration".to_string(),
        },
        FeatureSupport {
            feature: "JCL".to_string(),
            supported: true,
            support_level: 80,
            notes: "Core JCL supported via zos-jcl".to_string(),
        },
        FeatureSupport {
            feature: "SORT Utility".to_string(),
            supported: true,
            support_level: 90,
            notes: "Full SORT support via zos-sort".to_string(),
        },
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compatibility_checker() {
        let checker = CompatibilityChecker::new();

        let source = r#"
           DISPLAY "HELLO" UPON CONSOLE.
           ALTER PARA-1 TO PROCEED TO PARA-2.
"#;

        let issues = checker.check(source);

        assert!(!issues.is_empty());
        assert!(issues.iter().any(|i| i.code == "PLAT002"));
        assert!(issues.iter().any(|i| i.code == "DEPR001"));
    }

    #[test]
    fn test_severity_ordering() {
        assert!(Severity::Info < Severity::Warning);
        assert!(Severity::Warning < Severity::High);
        assert!(Severity::High < Severity::Critical);
    }

    #[test]
    fn test_ims_detection() {
        let checker = CompatibilityChecker::new();

        let source = "           EXEC DLI GU USING PCB1 END-EXEC.";
        let issues = checker.check(source);

        assert!(issues.iter().any(|i| i.code == "PLAT001"));
        assert!(issues.iter().any(|i| i.severity == Severity::Critical));
    }

    #[test]
    fn test_custom_rule() {
        let mut checker = CompatibilityChecker::new();

        checker.add_rule(
            "CUST001",
            "MY-CUSTOM-PATTERN",
            "Custom pattern detected",
            Severity::Warning,
            FeatureCategory::CoreLanguage,
            "Review custom pattern usage",
        );

        let source = "           MOVE MY-CUSTOM-PATTERN TO WS-VAR.";
        let issues = checker.check(source);

        assert!(issues.iter().any(|i| i.code == "CUST001"));
    }

    #[test]
    fn test_feature_support() {
        let features = get_feature_support();

        assert!(!features.is_empty());

        // Check that DB2 is supported
        let db2 = features.iter().find(|f| f.feature == "DB2 SQL");
        assert!(db2.is_some());
        assert!(db2.unwrap().supported);

        // Check that IMS is not supported
        let ims = features.iter().find(|f| f.feature == "IMS/DL1");
        assert!(ims.is_some());
        assert!(!ims.unwrap().supported);
    }

    #[test]
    fn test_issue_builder() {
        let issue = CompatibilityIssue::new(
            "TEST001",
            "Test issue",
            Severity::Warning,
            FeatureCategory::CoreLanguage,
        )
        .at_line(42)
        .with_recommendation("Fix the issue");

        assert_eq!(issue.code, "TEST001");
        assert_eq!(issue.line, Some(42));
        assert_eq!(issue.recommendation, "Fix the issue");
    }
}
