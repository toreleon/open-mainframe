//! Code metrics calculation.
//!
//! Provides various code metrics for COBOL programs.

/// Code complexity level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ComplexityLevel {
    /// Low complexity (cyclomatic < 10)
    Low,
    /// Moderate complexity (10-20)
    Moderate,
    /// High complexity (20-50)
    High,
    /// Very high complexity (50+)
    VeryHigh,
}

impl ComplexityLevel {
    /// Determine level from cyclomatic complexity.
    pub fn from_cyclomatic(cc: usize) -> Self {
        match cc {
            0..=10 => ComplexityLevel::Low,
            11..=20 => ComplexityLevel::Moderate,
            21..=50 => ComplexityLevel::High,
            _ => ComplexityLevel::VeryHigh,
        }
    }

    /// Get description.
    pub fn description(&self) -> &'static str {
        match self {
            ComplexityLevel::Low => "Low complexity, easy to understand and maintain",
            ComplexityLevel::Moderate => "Moderate complexity, requires careful review",
            ComplexityLevel::High => "High complexity, consider refactoring",
            ComplexityLevel::VeryHigh => "Very high complexity, refactoring recommended",
        }
    }
}

/// Code metrics for a COBOL program.
#[derive(Debug, Clone, Default, serde::Serialize)]
pub struct CodeMetrics {
    /// Total lines including blanks and comments
    pub total_lines: usize,
    /// Lines of code (excluding blanks and comments)
    pub code_lines: usize,
    /// Blank lines
    pub blank_lines: usize,
    /// Comment lines
    pub comment_lines: usize,
    /// Number of executable statements
    pub executable_statements: usize,
    /// Cyclomatic complexity
    pub cyclomatic_complexity: usize,
    /// Number of paragraphs/sections
    pub paragraph_count: usize,
    /// Number of data items defined
    pub data_items: usize,
    /// Has IDENTIFICATION DIVISION
    pub has_identification: bool,
    /// Has ENVIRONMENT DIVISION
    pub has_environment: bool,
    /// Has DATA DIVISION
    pub has_data: bool,
    /// Has PROCEDURE DIVISION
    pub has_procedure: bool,
}

impl CodeMetrics {
    /// Get complexity level.
    pub fn complexity_level(&self) -> ComplexityLevel {
        ComplexityLevel::from_cyclomatic(self.cyclomatic_complexity)
    }

    /// Calculate comment ratio (comments / total code).
    pub fn comment_ratio(&self) -> f64 {
        if self.code_lines == 0 {
            0.0
        } else {
            self.comment_lines as f64 / self.code_lines as f64
        }
    }

    /// Calculate statements per paragraph.
    pub fn statements_per_paragraph(&self) -> f64 {
        if self.paragraph_count == 0 {
            self.executable_statements as f64
        } else {
            self.executable_statements as f64 / self.paragraph_count as f64
        }
    }

    /// Check if program has all required divisions.
    pub fn is_complete(&self) -> bool {
        self.has_identification && self.has_procedure
    }

    /// Generate metrics summary.
    pub fn summary(&self) -> String {
        let mut summary = String::new();

        summary.push_str("Code Metrics Summary\n");
        summary.push_str("====================\n\n");

        summary.push_str(&format!("Total Lines:           {}\n", self.total_lines));
        summary.push_str(&format!("Code Lines:            {}\n", self.code_lines));
        summary.push_str(&format!("Comment Lines:         {}\n", self.comment_lines));
        summary.push_str(&format!("Blank Lines:           {}\n", self.blank_lines));
        summary.push_str(&format!("Comment Ratio:         {:.1}%\n", self.comment_ratio() * 100.0));
        summary.push('\n');

        summary.push_str(&format!("Executable Statements: {}\n", self.executable_statements));
        summary.push_str(&format!("Paragraphs:            {}\n", self.paragraph_count));
        summary.push_str(&format!("Statements/Paragraph:  {:.1}\n", self.statements_per_paragraph()));
        summary.push_str(&format!("Data Items:            {}\n", self.data_items));
        summary.push('\n');

        summary.push_str(&format!("Cyclomatic Complexity: {}\n", self.cyclomatic_complexity));
        summary.push_str(&format!("Complexity Level:      {:?}\n", self.complexity_level()));
        summary.push_str(&format!("                       {}\n", self.complexity_level().description()));

        summary
    }
}

/// Maintainability index calculation.
pub fn calculate_maintainability_index(metrics: &CodeMetrics) -> f64 {
    // Simplified Maintainability Index
    // Based on: 171 - 5.2 * ln(HV) - 0.23 * CC - 16.2 * ln(LOC)
    // Where HV = Halstead Volume (simplified as LOC * 2)
    //       CC = Cyclomatic Complexity
    //       LOC = Lines of Code

    let loc = metrics.code_lines.max(1) as f64;
    let cc = metrics.cyclomatic_complexity as f64;
    let hv = loc * 2.0; // Simplified Halstead Volume

    let mi = 171.0 - 5.2 * hv.ln() - 0.23 * cc - 16.2 * loc.ln();

    // Normalize to 0-100 scale
    mi.clamp(0.0, 100.0)
}

/// Technical debt estimation in hours.
pub fn estimate_technical_debt(metrics: &CodeMetrics) -> f64 {
    let base_hours = metrics.code_lines as f64 * 0.01; // 1 hour per 100 lines base

    // Add complexity penalty
    let complexity_penalty = match metrics.complexity_level() {
        ComplexityLevel::Low => 0.0,
        ComplexityLevel::Moderate => base_hours * 0.2,
        ComplexityLevel::High => base_hours * 0.5,
        ComplexityLevel::VeryHigh => base_hours * 1.0,
    };

    // Add documentation penalty (low comment ratio)
    let doc_penalty = if metrics.comment_ratio() < 0.1 {
        base_hours * 0.2
    } else {
        0.0
    };

    base_hours + complexity_penalty + doc_penalty
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_complexity_level() {
        assert_eq!(ComplexityLevel::from_cyclomatic(5), ComplexityLevel::Low);
        assert_eq!(ComplexityLevel::from_cyclomatic(15), ComplexityLevel::Moderate);
        assert_eq!(ComplexityLevel::from_cyclomatic(30), ComplexityLevel::High);
        assert_eq!(ComplexityLevel::from_cyclomatic(60), ComplexityLevel::VeryHigh);
    }

    #[test]
    fn test_code_metrics() {
        let metrics = CodeMetrics {
            total_lines: 100,
            code_lines: 80,
            blank_lines: 10,
            comment_lines: 10,
            executable_statements: 50,
            cyclomatic_complexity: 8,
            paragraph_count: 5,
            data_items: 20,
            has_identification: true,
            has_environment: true,
            has_data: true,
            has_procedure: true,
        };

        assert_eq!(metrics.complexity_level(), ComplexityLevel::Low);
        assert!((metrics.comment_ratio() - 0.125).abs() < 0.01);
        assert!((metrics.statements_per_paragraph() - 10.0).abs() < 0.01);
        assert!(metrics.is_complete());
    }

    #[test]
    fn test_maintainability_index() {
        let metrics = CodeMetrics {
            code_lines: 100,
            cyclomatic_complexity: 10,
            ..Default::default()
        };

        let mi = calculate_maintainability_index(&metrics);
        assert!(mi > 0.0);
        assert!(mi <= 100.0);
    }

    #[test]
    fn test_technical_debt() {
        let simple_metrics = CodeMetrics {
            code_lines: 100,
            cyclomatic_complexity: 5,
            comment_lines: 20,
            ..Default::default()
        };

        let complex_metrics = CodeMetrics {
            code_lines: 100,
            cyclomatic_complexity: 60,
            comment_lines: 0,
            ..Default::default()
        };

        let simple_debt = estimate_technical_debt(&simple_metrics);
        let complex_debt = estimate_technical_debt(&complex_metrics);

        assert!(complex_debt > simple_debt);
    }

    #[test]
    fn test_metrics_summary() {
        let metrics = CodeMetrics {
            total_lines: 100,
            code_lines: 80,
            comment_lines: 10,
            cyclomatic_complexity: 15,
            ..Default::default()
        };

        let summary = metrics.summary();
        assert!(summary.contains("Total Lines"));
        assert!(summary.contains("Cyclomatic Complexity"));
    }
}
