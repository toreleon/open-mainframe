//! Source code analyzer.
//!
//! Analyzes COBOL source code to extract metrics and identify features.

use crate::compatibility::{CompatibilityChecker, CompatibilityIssue};
use crate::metrics::CodeMetrics;
use crate::{AssessResult, FeatureCategory, MigrationComplexity};
use std::collections::{HashMap, HashSet};

/// Analysis result for a source file.
#[derive(Debug, Clone, serde::Serialize)]
pub struct AnalysisResult {
    /// Source file name
    pub file_name: String,
    /// Program ID
    pub program_id: Option<String>,
    /// Code metrics
    pub metrics: CodeMetrics,
    /// Features detected
    pub features: HashSet<Feature>,
    /// Compatibility issues
    pub issues: Vec<CompatibilityIssue>,
    /// Overall migration complexity
    pub complexity: MigrationComplexity,
    /// Recommendations
    pub recommendations: Vec<String>,
}

impl AnalysisResult {
    /// Get summary text.
    pub fn summary(&self) -> String {
        let mut summary = String::new();

        summary.push_str(&format!("File: {}\n", self.file_name));
        if let Some(ref id) = self.program_id {
            summary.push_str(&format!("Program: {}\n", id));
        }
        summary.push_str(&format!("\nComplexity: {:?}\n", self.complexity));
        summary.push_str(&format!("Lines of Code: {}\n", self.metrics.total_lines));
        summary.push_str(&format!("Executable Statements: {}\n", self.metrics.executable_statements));
        summary.push_str(&format!("Cyclomatic Complexity: {}\n", self.metrics.cyclomatic_complexity));

        if !self.issues.is_empty() {
            summary.push_str(&format!("\nCompatibility Issues: {}\n", self.issues.len()));
        }

        summary
    }
}

/// A detected feature in the source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize)]
pub struct Feature {
    /// Feature name
    pub name: String,
    /// Feature category
    pub category: FeatureCategory,
    /// Number of occurrences
    pub count: usize,
    /// Lines where feature appears
    pub lines: Vec<usize>,
}

impl Feature {
    /// Create a new feature.
    pub fn new(name: &str, category: FeatureCategory) -> Self {
        Self {
            name: name.to_string(),
            category,
            count: 0,
            lines: Vec::new(),
        }
    }

    /// Add an occurrence.
    pub fn add_occurrence(&mut self, line: usize) {
        self.count += 1;
        self.lines.push(line);
    }
}

/// Source code analyzer.
pub struct Analyzer {
    /// Compatibility checker
    compatibility: CompatibilityChecker,
    /// Feature patterns
    feature_patterns: Vec<FeaturePattern>,
}

struct FeaturePattern {
    name: String,
    category: FeatureCategory,
    patterns: Vec<String>,
}

impl Analyzer {
    /// Create a new analyzer.
    pub fn new() -> Self {
        Self {
            compatibility: CompatibilityChecker::new(),
            feature_patterns: Self::default_patterns(),
        }
    }

    fn default_patterns() -> Vec<FeaturePattern> {
        vec![
            // File handling
            FeaturePattern {
                name: "VSAM".to_string(),
                category: FeatureCategory::FileHandling,
                patterns: vec!["SELECT".to_string(), "ORGANIZATION IS INDEXED".to_string()],
            },
            FeaturePattern {
                name: "Sequential Files".to_string(),
                category: FeatureCategory::FileHandling,
                patterns: vec!["ORGANIZATION IS SEQUENTIAL".to_string()],
            },
            // Database
            FeaturePattern {
                name: "DB2".to_string(),
                category: FeatureCategory::Database,
                patterns: vec!["EXEC SQL".to_string()],
            },
            FeaturePattern {
                name: "IMS".to_string(),
                category: FeatureCategory::Database,
                patterns: vec!["EXEC DLI".to_string()],
            },
            // Transaction
            FeaturePattern {
                name: "CICS".to_string(),
                category: FeatureCategory::Transaction,
                patterns: vec!["EXEC CICS".to_string()],
            },
            // Interoperability
            FeaturePattern {
                name: "Subprogram Calls".to_string(),
                category: FeatureCategory::Interoperability,
                patterns: vec!["CALL ".to_string()],
            },
            FeaturePattern {
                name: "COPY Statements".to_string(),
                category: FeatureCategory::Interoperability,
                patterns: vec!["COPY ".to_string()],
            },
            // Platform specific
            FeaturePattern {
                name: "DISPLAY".to_string(),
                category: FeatureCategory::CoreLanguage,
                patterns: vec!["DISPLAY ".to_string()],
            },
            FeaturePattern {
                name: "ACCEPT".to_string(),
                category: FeatureCategory::CoreLanguage,
                patterns: vec!["ACCEPT ".to_string()],
            },
            FeaturePattern {
                name: "STRING/UNSTRING".to_string(),
                category: FeatureCategory::CoreLanguage,
                patterns: vec!["STRING ".to_string(), "UNSTRING ".to_string()],
            },
            FeaturePattern {
                name: "INSPECT".to_string(),
                category: FeatureCategory::CoreLanguage,
                patterns: vec!["INSPECT ".to_string()],
            },
            FeaturePattern {
                name: "COMPUTE".to_string(),
                category: FeatureCategory::CoreLanguage,
                patterns: vec!["COMPUTE ".to_string()],
            },
        ]
    }

    /// Analyze source code.
    pub fn analyze(&self, source: &str, file_name: &str) -> AssessResult<AnalysisResult> {
        let lines: Vec<&str> = source.lines().collect();

        // Extract program ID
        let program_id = self.extract_program_id(&lines);

        // Calculate metrics
        let metrics = self.calculate_metrics(&lines);

        // Detect features
        let features = self.detect_features(&lines);

        // Check compatibility
        let issues = self.compatibility.check(source);

        // Calculate complexity
        let complexity = self.calculate_complexity(&metrics, &features, &issues);

        // Generate recommendations
        let recommendations = self.generate_recommendations(&features, &issues);

        Ok(AnalysisResult {
            file_name: file_name.to_string(),
            program_id,
            metrics,
            features,
            issues,
            complexity,
            recommendations,
        })
    }

    fn extract_program_id(&self, lines: &[&str]) -> Option<String> {
        for line in lines {
            let upper = line.to_uppercase();
            if upper.contains("PROGRAM-ID") {
                // Extract the program name
                if let Some(pos) = upper.find("PROGRAM-ID") {
                    let rest = &line[pos + 10..];
                    let name: String = rest
                        .trim()
                        .trim_start_matches('.')
                        .trim()
                        .chars()
                        .take_while(|c| c.is_alphanumeric() || *c == '-')
                        .collect();
                    if !name.is_empty() {
                        return Some(name);
                    }
                }
            }
        }
        None
    }

    fn calculate_metrics(&self, lines: &[&str]) -> CodeMetrics {
        let mut metrics = CodeMetrics::default();

        metrics.total_lines = lines.len();

        let mut in_procedure = false;
        let mut _current_para = None;

        for (line_num, line) in lines.iter().enumerate() {
            let trimmed = line.trim();
            let upper = trimmed.to_uppercase();

            // Skip empty lines and comments
            if trimmed.is_empty() {
                metrics.blank_lines += 1;
                continue;
            }

            if trimmed.starts_with('*') || (line.len() > 6 && line.chars().nth(6) == Some('*')) {
                metrics.comment_lines += 1;
                continue;
            }

            metrics.code_lines += 1;

            // Track divisions
            if upper.contains("IDENTIFICATION DIVISION") {
                metrics.has_identification = true;
            } else if upper.contains("ENVIRONMENT DIVISION") {
                metrics.has_environment = true;
            } else if upper.contains("DATA DIVISION") {
                metrics.has_data = true;
            } else if upper.contains("PROCEDURE DIVISION") {
                metrics.has_procedure = true;
                in_procedure = true;
            }

            if in_procedure {
                // Count executable statements
                if self.is_executable_statement(&upper) {
                    metrics.executable_statements += 1;
                }

                // Track paragraphs for complexity
                if self.is_paragraph_header(trimmed) {
                    metrics.paragraph_count += 1;
                    _current_para = Some(line_num);
                }

                // Count decision points for cyclomatic complexity
                if upper.contains(" IF ") || upper.starts_with("IF ") {
                    metrics.cyclomatic_complexity += 1;
                }
                if upper.contains("EVALUATE ") {
                    metrics.cyclomatic_complexity += 1;
                }
                if upper.contains(" WHEN ") {
                    metrics.cyclomatic_complexity += 1;
                }
                if upper.contains("PERFORM ") && upper.contains(" UNTIL ") {
                    metrics.cyclomatic_complexity += 1;
                }
                if upper.contains("PERFORM ") && upper.contains(" VARYING ") {
                    metrics.cyclomatic_complexity += 1;
                }
            }

            // Count data items
            if !in_procedure && (upper.contains(" PIC ") || upper.contains(" PICTURE ")) {
                metrics.data_items += 1;
            }
        }

        // Base cyclomatic complexity is 1
        metrics.cyclomatic_complexity += 1;

        metrics
    }

    fn is_executable_statement(&self, upper: &str) -> bool {
        let statements = [
            "MOVE ", "ADD ", "SUBTRACT ", "MULTIPLY ", "DIVIDE ", "COMPUTE ",
            "IF ", "EVALUATE ", "PERFORM ", "CALL ", "GO TO ", "STOP RUN",
            "READ ", "WRITE ", "REWRITE ", "DELETE ", "OPEN ", "CLOSE ",
            "DISPLAY ", "ACCEPT ", "STRING ", "UNSTRING ", "INSPECT ",
            "SEARCH ", "SET ", "INITIALIZE ", "EXEC ",
        ];

        statements.iter().any(|s| upper.contains(s) || upper.starts_with(s))
    }

    fn is_paragraph_header(&self, line: &str) -> bool {
        // A paragraph header ends with a period and has no leading spaces after column 8
        let trimmed = line.trim();
        if trimmed.ends_with('.') {
            let name = trimmed.trim_end_matches('.');
            let upper = name.to_uppercase();

            // Exclude COBOL scope terminators
            if upper.starts_with("END-") {
                return false;
            }

            // Must be a valid paragraph name (starts with letter, contains only alphanum and hyphen)
            !name.is_empty()
                && name.chars().next().map(|c| c.is_alphabetic()).unwrap_or(false)
                && name.chars().all(|c| c.is_alphanumeric() || c == '-')
        } else {
            false
        }
    }

    fn detect_features(&self, lines: &[&str]) -> HashSet<Feature> {
        let mut features: HashMap<String, Feature> = HashMap::new();

        for (line_num, line) in lines.iter().enumerate() {
            let upper = line.to_uppercase();

            for pattern in &self.feature_patterns {
                for p in &pattern.patterns {
                    if upper.contains(p) {
                        let feature = features
                            .entry(pattern.name.clone())
                            .or_insert_with(|| Feature::new(&pattern.name, pattern.category));
                        feature.add_occurrence(line_num + 1);
                        break;
                    }
                }
            }
        }

        features.into_values().collect()
    }

    fn calculate_complexity(
        &self,
        metrics: &CodeMetrics,
        features: &HashSet<Feature>,
        issues: &[CompatibilityIssue],
    ) -> MigrationComplexity {
        let mut score = 0;

        // Base complexity from code size
        if metrics.total_lines > 5000 {
            score += 3;
        } else if metrics.total_lines > 2000 {
            score += 2;
        } else if metrics.total_lines > 500 {
            score += 1;
        }

        // Cyclomatic complexity
        if metrics.cyclomatic_complexity > 50 {
            score += 3;
        } else if metrics.cyclomatic_complexity > 20 {
            score += 2;
        } else if metrics.cyclomatic_complexity > 10 {
            score += 1;
        }

        // Feature complexity
        for feature in features {
            match feature.category {
                FeatureCategory::Database => score += 2,
                FeatureCategory::Transaction => score += 2,
                FeatureCategory::PlatformSpecific => score += 3,
                FeatureCategory::FileHandling => score += 1,
                _ => {}
            }
        }

        // Issue severity
        let critical_issues = issues.iter().filter(|i| i.severity == crate::compatibility::Severity::Critical).count();
        let high_issues = issues.iter().filter(|i| i.severity == crate::compatibility::Severity::High).count();

        score += critical_issues * 3;
        score += high_issues * 2;

        match score {
            0..=3 => MigrationComplexity::Low,
            4..=7 => MigrationComplexity::Medium,
            8..=12 => MigrationComplexity::High,
            _ => MigrationComplexity::VeryHigh,
        }
    }

    fn generate_recommendations(
        &self,
        features: &HashSet<Feature>,
        issues: &[CompatibilityIssue],
    ) -> Vec<String> {
        let mut recommendations = Vec::new();

        // Feature-based recommendations
        for feature in features {
            match feature.category {
                FeatureCategory::Database => {
                    if feature.name == "DB2" {
                        recommendations.push(
                            "Consider using PostgreSQL with the zos-db2 compatibility layer".to_string()
                        );
                    }
                }
                FeatureCategory::Transaction => {
                    if feature.name == "CICS" {
                        recommendations.push(
                            "CICS commands can be emulated using the zos-cics runtime".to_string()
                        );
                    }
                }
                FeatureCategory::FileHandling => {
                    if feature.name == "VSAM" {
                        recommendations.push(
                            "VSAM files can be migrated to the zos-dataset VSAM emulation".to_string()
                        );
                    }
                }
                _ => {}
            }
        }

        // Issue-based recommendations
        for issue in issues {
            if !issue.recommendation.is_empty() && !recommendations.contains(&issue.recommendation) {
                recommendations.push(issue.recommendation.clone());
            }
        }

        recommendations
    }
}

impl Default for Analyzer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE_COBOL: &str = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTINQ.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CUSTOMER-ID    PIC X(10).
       01  WS-CUSTOMER-NAME  PIC X(30).
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "CUSTOMER INQUIRY".
           ACCEPT WS-CUSTOMER-ID.
           PERFORM READ-CUSTOMER.
           STOP RUN.
       READ-CUSTOMER.
           IF WS-CUSTOMER-ID = SPACES
               DISPLAY "INVALID ID"
           ELSE
               DISPLAY WS-CUSTOMER-NAME
           END-IF.
"#;

    #[test]
    fn test_analyze_basic() {
        let analyzer = Analyzer::new();
        let result = analyzer.analyze(SAMPLE_COBOL, "CUSTINQ.cbl").unwrap();

        assert_eq!(result.file_name, "CUSTINQ.cbl");
        assert_eq!(result.program_id, Some("CUSTINQ".to_string()));
        assert!(result.metrics.has_identification);
        assert!(result.metrics.has_procedure);
    }

    #[test]
    fn test_metrics_calculation() {
        let analyzer = Analyzer::new();
        let result = analyzer.analyze(SAMPLE_COBOL, "test.cbl").unwrap();

        assert!(result.metrics.total_lines > 0);
        assert!(result.metrics.executable_statements > 0);
        assert!(result.metrics.cyclomatic_complexity >= 1);
        assert_eq!(result.metrics.paragraph_count, 2);
    }

    #[test]
    fn test_feature_detection() {
        let source_with_db2 = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB2PROG.
       PROCEDURE DIVISION.
           EXEC SQL
               SELECT NAME INTO :WS-NAME
               FROM CUSTOMERS
               WHERE ID = :WS-ID
           END-EXEC.
"#;

        let analyzer = Analyzer::new();
        let result = analyzer.analyze(source_with_db2, "test.cbl").unwrap();

        assert!(result.features.iter().any(|f| f.name == "DB2"));
    }

    #[test]
    fn test_cics_detection() {
        let source_with_cics = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSPROG.
       PROCEDURE DIVISION.
           EXEC CICS
               SEND MAP('CUSTMAP')
               MAPSET('CUSTSET')
           END-EXEC.
           EXEC CICS RETURN END-EXEC.
"#;

        let analyzer = Analyzer::new();
        let result = analyzer.analyze(source_with_cics, "test.cbl").unwrap();

        assert!(result.features.iter().any(|f| f.name == "CICS"));
        assert!(result.features.iter().find(|f| f.name == "CICS").map(|f| f.count).unwrap_or(0) >= 2);
    }

    #[test]
    fn test_complexity_rating() {
        let analyzer = Analyzer::new();
        let result = analyzer.analyze(SAMPLE_COBOL, "test.cbl").unwrap();

        // Simple program should have low complexity
        assert_eq!(result.complexity, MigrationComplexity::Low);
    }

    #[test]
    fn test_recommendations() {
        let source_with_db2 = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB2PROG.
       PROCEDURE DIVISION.
           EXEC SQL SELECT * FROM TABLE END-EXEC.
"#;

        let analyzer = Analyzer::new();
        let result = analyzer.analyze(source_with_db2, "test.cbl").unwrap();

        assert!(result.recommendations.iter().any(|r| r.contains("PostgreSQL")));
    }
}
