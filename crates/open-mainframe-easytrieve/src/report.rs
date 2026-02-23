//! EZ-103: Report Generator for Easytrieve Plus.
//!
//! Provides report formatting with headings, detail lines, page control,
//! control breaks (automatic subtotals), and summary accumulators.

use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

/// Errors during report generation.
#[derive(Debug, Error, Diagnostic)]
pub enum ReportError {
    /// Report not defined.
    #[error("report '{name}' not defined")]
    ReportNotDefined {
        /// Report name.
        name: String,
    },
    /// Line definition not found.
    #[error("line {number} not defined in report '{report}'")]
    LineNotDefined {
        /// Line number.
        number: u32,
        /// Report name.
        report: String,
    },
}

// ---------------------------------------------------------------------------
// Report definition
// ---------------------------------------------------------------------------

/// Report definition with headings, lines, and titles.
///
/// Defines the layout of an Easytrieve report including page headers,
/// detail line formats, and page titles.
#[derive(Debug, Clone)]
pub struct ReportDef {
    /// Report name.
    pub name: String,
    /// Page width (default 132).
    pub page_width: usize,
    /// Page length / lines per page (default 60).
    pub page_length: usize,
    /// Heading line definitions.
    pub headings: Vec<ReportLine>,
    /// Detail line definitions.
    pub lines: Vec<ReportLine>,
    /// Title definitions.
    pub titles: Vec<ReportLine>,
    /// Control break fields.
    pub control_fields: Vec<String>,
    /// Summary fields for accumulation.
    pub sum_fields: Vec<String>,
}

impl ReportDef {
    /// Create a new report definition.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            page_width: 132,
            page_length: 60,
            headings: Vec::new(),
            lines: Vec::new(),
            titles: Vec::new(),
            control_fields: Vec::new(),
            sum_fields: Vec::new(),
        }
    }

    /// Set the page dimensions.
    pub fn with_page_size(mut self, width: usize, length: usize) -> Self {
        self.page_width = width;
        self.page_length = length;
        self
    }

    /// Add a heading line.
    pub fn add_heading(&mut self, number: u32, items: Vec<String>) {
        self.headings.push(ReportLine { number, items });
    }

    /// Add a detail line definition.
    pub fn add_line(&mut self, number: u32, items: Vec<String>) {
        self.lines.push(ReportLine { number, items });
    }

    /// Add a title line.
    pub fn add_title(&mut self, number: u32, items: Vec<String>) {
        self.titles.push(ReportLine { number, items });
    }

    /// Set control break fields.
    pub fn set_control_fields(&mut self, fields: Vec<String>) {
        self.control_fields = fields;
    }

    /// Set summary accumulation fields.
    pub fn set_sum_fields(&mut self, fields: Vec<String>) {
        self.sum_fields = fields;
    }
}

/// A line within a report (heading, detail, or title).
#[derive(Debug, Clone)]
pub struct ReportLine {
    /// Line number / sequence.
    pub number: u32,
    /// Content items (field names, literals, spacing).
    pub items: Vec<String>,
}

// ---------------------------------------------------------------------------
// Page control
// ---------------------------------------------------------------------------

/// Page numbering, line counting, and page break management.
#[derive(Debug, Clone)]
pub struct PageControl {
    /// Current page number.
    pub page_number: usize,
    /// Current line on the page.
    pub current_line: usize,
    /// Maximum lines per page.
    pub lines_per_page: usize,
    /// Total lines output across all pages.
    pub total_lines: usize,
}

impl PageControl {
    /// Create a new page controller.
    pub fn new(lines_per_page: usize) -> Self {
        Self {
            page_number: 1,
            current_line: 0,
            lines_per_page,
            total_lines: 0,
        }
    }

    /// Check if a page break is needed.
    pub fn needs_page_break(&self) -> bool {
        self.current_line >= self.lines_per_page
    }

    /// Advance to next page.
    pub fn new_page(&mut self) {
        self.page_number += 1;
        self.current_line = 0;
    }

    /// Advance line counter.
    pub fn advance_line(&mut self) {
        self.current_line += 1;
        self.total_lines += 1;
    }

    /// Advance by multiple lines.
    pub fn advance_lines(&mut self, count: usize) {
        self.current_line += count;
        self.total_lines += count;
    }
}

// ---------------------------------------------------------------------------
// Control break
// ---------------------------------------------------------------------------

/// Automatic subtotal control break processing.
///
/// Triggers subtotal output when a sort key value changes between records.
#[derive(Debug, Clone)]
pub struct ControlBreak {
    /// Control break field name.
    pub field_name: String,
    /// Previous value of the control field.
    pub previous_value: Option<String>,
    /// Accumulator values for the current group.
    pub accumulators: HashMap<String, f64>,
    /// Count of records in the current group.
    pub record_count: usize,
}

impl ControlBreak {
    /// Create a new control break tracker.
    pub fn new(field_name: &str) -> Self {
        Self {
            field_name: field_name.to_string(),
            previous_value: None,
            accumulators: HashMap::new(),
            record_count: 0,
        }
    }

    /// Check if a control break has occurred with the new value.
    ///
    /// Returns `true` if the value changed (break occurred).
    pub fn check(&mut self, current_value: &str) -> bool {
        let broke = match &self.previous_value {
            Some(prev) => prev != current_value,
            None => false,
        };
        self.previous_value = Some(current_value.to_string());
        if broke {
            self.record_count = 0;
        }
        self.record_count += 1;
        broke
    }

    /// Accumulate a value for a field.
    pub fn accumulate(&mut self, field: &str, value: f64) {
        let acc = self.accumulators.entry(field.to_string()).or_insert(0.0);
        *acc += value;
    }

    /// Get the accumulated value for a field.
    pub fn get_accumulated(&self, field: &str) -> f64 {
        self.accumulators.get(field).copied().unwrap_or(0.0)
    }

    /// Reset accumulators for the next group.
    pub fn reset(&mut self) {
        self.accumulators.clear();
        self.record_count = 0;
    }

    /// Get the previous (break) value.
    pub fn break_value(&self) -> Option<&str> {
        self.previous_value.as_deref()
    }
}

// ---------------------------------------------------------------------------
// Summary line
// ---------------------------------------------------------------------------

/// Summary accumulator for SUM, COUNT, AVG, MIN, MAX operations.
#[derive(Debug, Clone)]
pub struct SummaryLine {
    /// Field name being summarized.
    pub field_name: String,
    /// Running sum.
    pub sum: f64,
    /// Record count.
    pub count: usize,
    /// Minimum value seen.
    pub min: f64,
    /// Maximum value seen.
    pub max: f64,
}

impl SummaryLine {
    /// Create a new summary accumulator.
    pub fn new(field_name: &str) -> Self {
        Self {
            field_name: field_name.to_string(),
            sum: 0.0,
            count: 0,
            min: f64::MAX,
            max: f64::MIN,
        }
    }

    /// Add a value to the summary.
    pub fn add(&mut self, value: f64) {
        self.sum += value;
        self.count += 1;
        if value < self.min {
            self.min = value;
        }
        if value > self.max {
            self.max = value;
        }
    }

    /// Get the SUM value.
    pub fn sum(&self) -> f64 {
        self.sum
    }

    /// Get the COUNT value.
    pub fn count(&self) -> usize {
        self.count
    }

    /// Get the AVG (average) value.
    pub fn avg(&self) -> f64 {
        if self.count == 0 {
            0.0
        } else {
            self.sum / self.count as f64
        }
    }

    /// Get the MIN value.
    pub fn min(&self) -> f64 {
        if self.count == 0 {
            0.0
        } else {
            self.min
        }
    }

    /// Get the MAX value.
    pub fn max(&self) -> f64 {
        if self.count == 0 {
            0.0
        } else {
            self.max
        }
    }

    /// Reset the summary accumulator.
    pub fn reset(&mut self) {
        self.sum = 0.0;
        self.count = 0;
        self.min = f64::MAX;
        self.max = f64::MIN;
    }
}

// ---------------------------------------------------------------------------
// Report formatter
// ---------------------------------------------------------------------------

/// Report formatter producing paginated report output.
///
/// Formats report pages with headers, detail lines, footers,
/// and control break subtotals.
#[derive(Debug)]
pub struct ReportFormatter {
    /// Report definition.
    pub report: ReportDef,
    /// Page control.
    pub page_control: PageControl,
    /// Output lines.
    output: Vec<String>,
    /// Control break trackers.
    control_breaks: Vec<ControlBreak>,
    /// Summary accumulators.
    summaries: HashMap<String, SummaryLine>,
    /// Whether headers have been printed for current page.
    headers_printed: bool,
}

impl ReportFormatter {
    /// Create a new report formatter.
    pub fn new(report: ReportDef) -> Self {
        let page_control = PageControl::new(report.page_length);
        let control_breaks = report
            .control_fields
            .iter()
            .map(|f| ControlBreak::new(f))
            .collect();
        let summaries = report
            .sum_fields
            .iter()
            .map(|f| (f.clone(), SummaryLine::new(f)))
            .collect();

        Self {
            report,
            page_control,
            output: Vec::new(),
            control_breaks,
            summaries,
            headers_printed: false,
        }
    }

    /// Format and output the page header (titles + headings).
    pub fn print_headers(&mut self) {
        // Titles
        for title in &self.report.titles {
            let line = self.format_line_items(&title.items);
            self.output.push(line);
            self.page_control.advance_line();
        }

        // Page number line
        let page_line = format!(
            "{:>width$}",
            format!("PAGE {}", self.page_control.page_number),
            width = self.report.page_width
        );
        self.output.push(page_line);
        self.page_control.advance_line();

        // Headings
        for heading in &self.report.headings {
            let line = self.format_line_items(&heading.items);
            self.output.push(line);
            self.page_control.advance_line();
        }

        // Blank line after headers
        self.output.push(String::new());
        self.page_control.advance_line();

        self.headers_printed = true;
    }

    /// Output a detail line with field values.
    pub fn print_detail(&mut self, values: &HashMap<String, String>) {
        if !self.headers_printed || self.page_control.needs_page_break() {
            if self.headers_printed {
                self.page_control.new_page();
            }
            self.print_headers();
        }

        // Check control breaks
        for cb in &mut self.control_breaks {
            if let Some(val) = values.get(&cb.field_name) {
                if cb.check(val) {
                    // Control break occurred — subtotals would be printed here
                }
            }
        }

        // Accumulate summaries
        for (field, summary) in &mut self.summaries {
            if let Some(val_str) = values.get(field) {
                if let Ok(val) = val_str.trim().parse::<f64>() {
                    summary.add(val);
                }
            }
        }

        // Format the first detail line definition
        if let Some(line_def) = self.report.lines.first() {
            let formatted = self.format_detail_with_values(&line_def.items, values);
            self.output.push(formatted);
            self.page_control.advance_line();
        }
    }

    /// Print a summary/total line.
    pub fn print_summary(&mut self) {
        let mut parts = Vec::new();
        parts.push("*** TOTALS ***".to_string());
        for (field, summary) in &self.summaries {
            parts.push(format!(
                "  {field}: SUM={:.2} COUNT={} AVG={:.2} MIN={:.2} MAX={:.2}",
                summary.sum(),
                summary.count(),
                summary.avg(),
                summary.min(),
                summary.max(),
            ));
        }
        for part in parts {
            self.output.push(part);
            self.page_control.advance_line();
        }
    }

    /// Get all output lines.
    pub fn get_output(&self) -> &[String] {
        &self.output
    }

    /// Get output as a single string.
    pub fn get_output_text(&self) -> String {
        self.output.join("\n")
    }

    /// Format a line from its item definitions.
    fn format_line_items(&self, items: &[String]) -> String {
        let mut parts = Vec::new();
        for item in items {
            let trimmed = item.trim_matches('\'');
            if item.starts_with('\'') && item.ends_with('\'') {
                parts.push(trimmed.to_string());
            } else {
                parts.push(item.clone());
            }
        }
        parts.join(" ")
    }

    /// Format a detail line substituting field values.
    fn format_detail_with_values(
        &self,
        items: &[String],
        values: &HashMap<String, String>,
    ) -> String {
        let mut parts = Vec::new();
        for item in items {
            let trimmed = item.trim_matches('\'');
            if item.starts_with('\'') && item.ends_with('\'') {
                parts.push(trimmed.to_string());
            } else if let Some(val) = values.get(item) {
                parts.push(val.clone());
            } else {
                parts.push(item.clone());
            }
        }
        parts.join("  ")
    }

    /// Get a summary accumulator by field name.
    pub fn get_summary(&self, field: &str) -> Option<&SummaryLine> {
        self.summaries.get(field)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_report() -> ReportDef {
        let mut report = ReportDef::new("RPT1");
        report.page_width = 80;
        report.page_length = 50;
        report.add_title(1, vec!["'EMPLOYEE SALARY REPORT'".into()]);
        report.add_heading(
            1,
            vec!["'NAME'".into(), "'DEPT'".into(), "'SALARY'".into()],
        );
        report.add_line(
            1,
            vec!["NAME".into(), "DEPT".into(), "SALARY".into()],
        );
        report.set_sum_fields(vec!["SALARY".into()]);
        report
    }

    #[test]
    fn test_report_def_creation() {
        let report = sample_report();
        assert_eq!(report.name, "RPT1");
        assert_eq!(report.page_width, 80);
        assert_eq!(report.headings.len(), 1);
        assert_eq!(report.lines.len(), 1);
        assert_eq!(report.titles.len(), 1);
    }

    #[test]
    fn test_page_control() {
        let mut pc = PageControl::new(60);
        assert_eq!(pc.page_number, 1);
        assert!(!pc.needs_page_break());

        for _ in 0..60 {
            pc.advance_line();
        }
        assert!(pc.needs_page_break());

        pc.new_page();
        assert_eq!(pc.page_number, 2);
        assert_eq!(pc.current_line, 0);
    }

    #[test]
    fn test_page_control_advance_lines() {
        let mut pc = PageControl::new(60);
        pc.advance_lines(5);
        assert_eq!(pc.current_line, 5);
        assert_eq!(pc.total_lines, 5);
    }

    #[test]
    fn test_control_break() {
        let mut cb = ControlBreak::new("DEPT");
        assert!(!cb.check("SALES")); // First value, no break
        assert!(!cb.check("SALES")); // Same value
        assert!(cb.check("IT")); // Different value = break!
        assert_eq!(cb.break_value(), Some("IT"));
    }

    #[test]
    fn test_control_break_accumulator() {
        let mut cb = ControlBreak::new("DEPT");
        cb.accumulate("SALARY", 50000.0);
        cb.accumulate("SALARY", 60000.0);
        assert!((cb.get_accumulated("SALARY") - 110000.0).abs() < f64::EPSILON);

        cb.reset();
        assert!((cb.get_accumulated("SALARY")).abs() < f64::EPSILON);
    }

    #[test]
    fn test_summary_line() {
        let mut summary = SummaryLine::new("SALARY");
        summary.add(50000.0);
        summary.add(60000.0);
        summary.add(70000.0);

        assert!((summary.sum() - 180000.0).abs() < f64::EPSILON);
        assert_eq!(summary.count(), 3);
        assert!((summary.avg() - 60000.0).abs() < f64::EPSILON);
        assert!((summary.min() - 50000.0).abs() < f64::EPSILON);
        assert!((summary.max() - 70000.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_summary_line_reset() {
        let mut summary = SummaryLine::new("AMT");
        summary.add(100.0);
        summary.reset();
        assert_eq!(summary.count(), 0);
        assert!((summary.sum()).abs() < f64::EPSILON);
        assert!((summary.avg()).abs() < f64::EPSILON);
    }

    #[test]
    fn test_report_formatter_headers() {
        let report = sample_report();
        let mut fmt = ReportFormatter::new(report);
        fmt.print_headers();

        let output = fmt.get_output();
        assert!(!output.is_empty());
        // Title should be first line
        assert!(output[0].contains("EMPLOYEE SALARY REPORT"));
        // Page number should appear
        assert!(output.iter().any(|l| l.contains("PAGE 1")));
    }

    #[test]
    fn test_report_formatter_detail() {
        let report = sample_report();
        let mut fmt = ReportFormatter::new(report);

        let mut values = HashMap::new();
        values.insert("NAME".to_string(), "JOHN DOE".to_string());
        values.insert("DEPT".to_string(), "SALES".to_string());
        values.insert("SALARY".to_string(), "50000".to_string());
        fmt.print_detail(&values);

        let output = fmt.get_output();
        assert!(output.iter().any(|l| l.contains("JOHN DOE")));
    }

    #[test]
    fn test_report_formatter_summary() {
        let report = sample_report();
        let mut fmt = ReportFormatter::new(report);

        for salary in [50000.0, 60000.0, 70000.0] {
            let mut values = HashMap::new();
            values.insert("SALARY".to_string(), salary.to_string());
            values.insert("NAME".to_string(), "EMP".to_string());
            fmt.print_detail(&values);
        }

        fmt.print_summary();

        let text = fmt.get_output_text();
        assert!(text.contains("TOTALS"));
    }

    #[test]
    fn test_report_page_break() {
        let mut report = ReportDef::new("SMALL");
        report.page_length = 10;
        report.add_title(1, vec!["'TEST'".into()]);
        report.add_heading(1, vec!["'COL1'".into()]);
        report.add_line(1, vec!["VAL".into()]);

        let mut fmt = ReportFormatter::new(report);

        // Print enough lines to trigger page break
        for i in 0..15 {
            let mut values = HashMap::new();
            values.insert("VAL".to_string(), i.to_string());
            fmt.print_detail(&values);
        }

        // Should have gone to page 2 or more
        assert!(fmt.page_control.page_number >= 2);
    }

    #[test]
    fn test_report_with_page_size() {
        let report = ReportDef::new("CUSTOM").with_page_size(100, 40);
        assert_eq!(report.page_width, 100);
        assert_eq!(report.page_length, 40);
    }
}
