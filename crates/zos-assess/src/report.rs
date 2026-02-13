//! Migration assessment report generation.
//!
//! Generates reports in various formats (text, HTML, JSON).

use crate::analyzer::AnalysisResult;
use crate::compatibility::{get_feature_support, Severity};
use crate::MigrationComplexity;
use serde::Serialize;

/// Report output format.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReportFormat {
    /// Plain text
    Text,
    /// Markdown
    Markdown,
    /// JSON
    Json,
    /// HTML
    Html,
}

/// Migration assessment report.
#[derive(Debug, Clone, Serialize)]
pub struct Report {
    /// Report title
    pub title: String,
    /// Analysis results
    pub results: Vec<AnalysisResult>,
    /// Overall complexity
    pub overall_complexity: MigrationComplexity,
    /// Total lines analyzed
    pub total_lines: usize,
    /// Total issues found
    pub total_issues: usize,
    /// Critical issues
    pub critical_issues: usize,
    /// Estimated migration effort (hours)
    pub estimated_effort_hours: f64,
}

impl Report {
    /// Create a new report from analysis results.
    pub fn new(title: &str, results: Vec<AnalysisResult>) -> Self {
        let total_lines: usize = results.iter().map(|r| r.metrics.total_lines).sum();
        let total_issues: usize = results.iter().map(|r| r.issues.len()).sum();
        let critical_issues: usize = results
            .iter()
            .flat_map(|r| r.issues.iter())
            .filter(|i| i.severity == Severity::Critical)
            .count();

        let overall_complexity = Self::calculate_overall_complexity(&results);
        let estimated_effort_hours = Self::estimate_effort(&results, overall_complexity);

        Self {
            title: title.to_string(),
            results,
            overall_complexity,
            total_lines,
            total_issues,
            critical_issues,
            estimated_effort_hours,
        }
    }

    fn calculate_overall_complexity(results: &[AnalysisResult]) -> MigrationComplexity {
        if results.is_empty() {
            return MigrationComplexity::Low;
        }

        // Use the highest complexity found
        results
            .iter()
            .map(|r| r.complexity)
            .max()
            .unwrap_or(MigrationComplexity::Low)
    }

    fn estimate_effort(results: &[AnalysisResult], overall: MigrationComplexity) -> f64 {
        // Base: 1 hour per 100 lines
        let base_hours: f64 = results.iter().map(|r| r.metrics.code_lines as f64 / 100.0).sum();

        // Apply complexity multiplier
        base_hours * overall.effort_multiplier()
    }

    /// Generate report in specified format.
    pub fn generate(&self, format: ReportFormat) -> String {
        match format {
            ReportFormat::Text => self.generate_text(),
            ReportFormat::Markdown => self.generate_markdown(),
            ReportFormat::Json => self.generate_json(),
            ReportFormat::Html => self.generate_html(),
        }
    }

    fn generate_text(&self) -> String {
        let mut output = String::new();

        // Header
        output.push_str(&format!("{}\n", self.title));
        output.push_str(&"=".repeat(self.title.len()));
        output.push_str("\n\n");

        // Executive Summary
        output.push_str("EXECUTIVE SUMMARY\n");
        output.push_str("-----------------\n\n");
        output.push_str(&format!("Files Analyzed:      {}\n", self.results.len()));
        output.push_str(&format!("Total Lines:         {}\n", self.total_lines));
        output.push_str(&format!("Overall Complexity:  {:?}\n", self.overall_complexity));
        output.push_str(&format!("Total Issues:        {}\n", self.total_issues));
        output.push_str(&format!("Critical Issues:     {}\n", self.critical_issues));
        output.push_str(&format!("Estimated Effort:    {:.1} hours\n", self.estimated_effort_hours));
        output.push_str("\n");

        // Complexity Description
        output.push_str(&format!("Assessment: {}\n\n", self.overall_complexity.description()));

        // Per-file details
        if !self.results.is_empty() {
            output.push_str("FILE DETAILS\n");
            output.push_str("------------\n\n");

            for result in &self.results {
                output.push_str(&format!("File: {}\n", result.file_name));
                if let Some(ref id) = result.program_id {
                    output.push_str(&format!("  Program ID: {}\n", id));
                }
                output.push_str(&format!("  Lines: {}\n", result.metrics.total_lines));
                output.push_str(&format!("  Complexity: {:?}\n", result.complexity));
                output.push_str(&format!("  Issues: {}\n", result.issues.len()));
                output.push('\n');
            }
        }

        // Issues Summary
        if self.total_issues > 0 {
            output.push_str("ISSUES\n");
            output.push_str("------\n\n");

            for result in &self.results {
                for issue in &result.issues {
                    output.push_str(&format!(
                        "[{}] {} - {}\n",
                        issue.severity.name(),
                        issue.code,
                        issue.description
                    ));
                    if let Some(line) = issue.line {
                        output.push_str(&format!("     File: {}, Line: {}\n", result.file_name, line));
                    }
                    if !issue.recommendation.is_empty() {
                        output.push_str(&format!("     Recommendation: {}\n", issue.recommendation));
                    }
                    output.push('\n');
                }
            }
        }

        // Recommendations
        let all_recommendations: Vec<_> = self
            .results
            .iter()
            .flat_map(|r| r.recommendations.iter())
            .collect();

        if !all_recommendations.is_empty() {
            output.push_str("RECOMMENDATIONS\n");
            output.push_str("---------------\n\n");

            let mut seen = std::collections::HashSet::new();
            for rec in all_recommendations {
                if seen.insert(rec) {
                    output.push_str(&format!("- {}\n", rec));
                }
            }
            output.push('\n');
        }

        // Feature Support
        output.push_str("FEATURE SUPPORT\n");
        output.push_str("---------------\n\n");

        for feature in get_feature_support() {
            let status = if feature.supported { "Yes" } else { "No" };
            output.push_str(&format!(
                "{:<20} {:>3}  {}%  {}\n",
                feature.feature, status, feature.support_level, feature.notes
            ));
        }

        output
    }

    fn generate_markdown(&self) -> String {
        let mut output = String::new();

        // Header
        output.push_str(&format!("# {}\n\n", self.title));

        // Executive Summary
        output.push_str("## Executive Summary\n\n");
        output.push_str("| Metric | Value |\n");
        output.push_str("|--------|-------|\n");
        output.push_str(&format!("| Files Analyzed | {} |\n", self.results.len()));
        output.push_str(&format!("| Total Lines | {} |\n", self.total_lines));
        output.push_str(&format!("| Overall Complexity | {:?} |\n", self.overall_complexity));
        output.push_str(&format!("| Total Issues | {} |\n", self.total_issues));
        output.push_str(&format!("| Critical Issues | {} |\n", self.critical_issues));
        output.push_str(&format!("| Estimated Effort | {:.1} hours |\n\n", self.estimated_effort_hours));

        output.push_str(&format!("> **Assessment:** {}\n\n", self.overall_complexity.description()));

        // Per-file details
        if !self.results.is_empty() {
            output.push_str("## File Details\n\n");
            output.push_str("| File | Program | Lines | Complexity | Issues |\n");
            output.push_str("|------|---------|-------|------------|--------|\n");

            for result in &self.results {
                output.push_str(&format!(
                    "| {} | {} | {} | {:?} | {} |\n",
                    result.file_name,
                    result.program_id.as_deref().unwrap_or("-"),
                    result.metrics.total_lines,
                    result.complexity,
                    result.issues.len()
                ));
            }
            output.push('\n');
        }

        // Issues
        if self.total_issues > 0 {
            output.push_str("## Issues\n\n");

            for result in &self.results {
                if !result.issues.is_empty() {
                    output.push_str(&format!("### {}\n\n", result.file_name));

                    for issue in &result.issues {
                        let icon = match issue.severity {
                            Severity::Critical => "🔴",
                            Severity::High => "🟠",
                            Severity::Warning => "🟡",
                            Severity::Info => "🔵",
                        };

                        output.push_str(&format!(
                            "- {} **{}**: {}\n",
                            icon, issue.code, issue.description
                        ));

                        if !issue.recommendation.is_empty() {
                            output.push_str(&format!("  - *Recommendation:* {}\n", issue.recommendation));
                        }
                    }
                    output.push('\n');
                }
            }
        }

        output
    }

    fn generate_json(&self) -> String {
        serde_json::to_string_pretty(self).unwrap_or_else(|_| "{}".to_string())
    }

    fn generate_html(&self) -> String {
        let mut output = String::new();

        output.push_str("<!DOCTYPE html>\n<html>\n<head>\n");
        output.push_str(&format!("<title>{}</title>\n", self.title));
        output.push_str("<style>\n");
        output.push_str("body { font-family: Arial, sans-serif; margin: 40px; }\n");
        output.push_str("h1 { color: #333; }\n");
        output.push_str("table { border-collapse: collapse; width: 100%; margin: 20px 0; }\n");
        output.push_str("th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }\n");
        output.push_str("th { background-color: #4CAF50; color: white; }\n");
        output.push_str(".critical { color: #dc3545; }\n");
        output.push_str(".high { color: #fd7e14; }\n");
        output.push_str(".warning { color: #ffc107; }\n");
        output.push_str(".info { color: #17a2b8; }\n");
        output.push_str("</style>\n");
        output.push_str("</head>\n<body>\n");

        output.push_str(&format!("<h1>{}</h1>\n", self.title));

        // Summary
        output.push_str("<h2>Executive Summary</h2>\n");
        output.push_str("<table>\n");
        output.push_str(&format!("<tr><td>Files Analyzed</td><td>{}</td></tr>\n", self.results.len()));
        output.push_str(&format!("<tr><td>Total Lines</td><td>{}</td></tr>\n", self.total_lines));
        output.push_str(&format!("<tr><td>Overall Complexity</td><td>{:?}</td></tr>\n", self.overall_complexity));
        output.push_str(&format!("<tr><td>Total Issues</td><td>{}</td></tr>\n", self.total_issues));
        output.push_str(&format!("<tr><td>Critical Issues</td><td>{}</td></tr>\n", self.critical_issues));
        output.push_str(&format!("<tr><td>Estimated Effort</td><td>{:.1} hours</td></tr>\n", self.estimated_effort_hours));
        output.push_str("</table>\n");

        output.push_str(&format!("<p><strong>Assessment:</strong> {}</p>\n", self.overall_complexity.description()));

        output.push_str("</body>\n</html>");

        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analyzer::Analyzer;

    fn create_sample_results() -> Vec<AnalysisResult> {
        let analyzer = Analyzer::new();

        let source1 = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG1.
       PROCEDURE DIVISION.
           DISPLAY "HELLO".
           STOP RUN.
"#;

        let source2 = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG2.
       PROCEDURE DIVISION.
           EXEC SQL SELECT * FROM TABLE END-EXEC.
           STOP RUN.
"#;

        vec![
            analyzer.analyze(source1, "PROG1.cbl").unwrap(),
            analyzer.analyze(source2, "PROG2.cbl").unwrap(),
        ]
    }

    #[test]
    fn test_report_creation() {
        let results = create_sample_results();
        let report = Report::new("Test Report", results);

        assert_eq!(report.results.len(), 2);
        assert!(report.total_lines > 0);
    }

    #[test]
    fn test_text_report() {
        let results = create_sample_results();
        let report = Report::new("Test Report", results);

        let text = report.generate(ReportFormat::Text);

        assert!(text.contains("Test Report"));
        assert!(text.contains("EXECUTIVE SUMMARY"));
        assert!(text.contains("Files Analyzed"));
    }

    #[test]
    fn test_markdown_report() {
        let results = create_sample_results();
        let report = Report::new("Test Report", results);

        let markdown = report.generate(ReportFormat::Markdown);

        assert!(markdown.contains("# Test Report"));
        assert!(markdown.contains("## Executive Summary"));
        assert!(markdown.contains("|"));
    }

    #[test]
    fn test_json_report() {
        let results = create_sample_results();
        let report = Report::new("Test Report", results);

        let json = report.generate(ReportFormat::Json);

        assert!(json.starts_with('{'));
        assert!(json.contains("\"title\""));
        assert!(json.contains("Test Report"));
    }

    #[test]
    fn test_html_report() {
        let results = create_sample_results();
        let report = Report::new("Test Report", results);

        let html = report.generate(ReportFormat::Html);

        assert!(html.contains("<!DOCTYPE html>"));
        assert!(html.contains("<title>Test Report</title>"));
        assert!(html.contains("<h1>Test Report</h1>"));
    }

    #[test]
    fn test_empty_report() {
        let report = Report::new("Empty Report", vec![]);

        assert_eq!(report.results.len(), 0);
        assert_eq!(report.total_lines, 0);
        assert_eq!(report.overall_complexity, MigrationComplexity::Low);
    }
}
