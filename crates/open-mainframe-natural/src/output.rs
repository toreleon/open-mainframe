// SPDX-License-Identifier: Apache-2.0
//! NAT-106: Output & Reporting for Natural.
//!
//! Provides DISPLAY (columnar), WRITE (free-form), PRINT output,
//! AT BREAK OF control break processing, AT TOP OF PAGE / AT END OF PAGE
//! headers and footers, NEWPAGE, and a `ReportEngine` with page width,
//! line count, and column formatting.

use crate::data_model::NaturalValue;

// ---------------------------------------------------------------------------
// Column definition
// ---------------------------------------------------------------------------

/// A column in a DISPLAY statement.
#[derive(Debug, Clone)]
pub struct ColumnDef {
    pub header: String,
    pub width: usize,
    pub alignment: Alignment,
}

/// Text alignment within a column.
#[derive(Debug, Clone, PartialEq)]
pub enum Alignment {
    Left,
    Right,
    Center,
}

impl ColumnDef {
    pub fn new(header: &str, width: usize, alignment: Alignment) -> Self {
        Self {
            header: header.to_string(),
            width,
            alignment,
        }
    }

    /// Format a value according to this column definition.
    pub fn format_value(&self, value: &str) -> String {
        let truncated = if value.len() > self.width {
            &value[..self.width]
        } else {
            value
        };

        match self.alignment {
            Alignment::Left => format!("{:<width$}", truncated, width = self.width),
            Alignment::Right => format!("{:>width$}", truncated, width = self.width),
            Alignment::Center => {
                let padding = self.width.saturating_sub(truncated.len());
                let left_pad = padding / 2;
                let right_pad = padding - left_pad;
                format!("{}{}{}", " ".repeat(left_pad), truncated, " ".repeat(right_pad))
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Control break
// ---------------------------------------------------------------------------

/// Tracking state for control break processing.
#[derive(Debug, Clone)]
pub struct ControlBreak {
    pub field_name: String,
    pub last_value: Option<NaturalValue>,
    pub subtotals: Vec<(String, f64)>,
    pub count: usize,
}

impl ControlBreak {
    pub fn new(field_name: &str) -> Self {
        Self {
            field_name: field_name.to_string(),
            last_value: None,
            subtotals: Vec::new(),
            count: 0,
        }
    }

    /// Check if the field value has changed (break occurred).
    pub fn check(&mut self, current_value: &NaturalValue) -> bool {
        let changed = match &self.last_value {
            Some(last) => last.to_display_string() != current_value.to_display_string(),
            None => false,
        };
        self.last_value = Some(current_value.clone());
        self.count += 1;
        changed
    }

    /// Add to a subtotal accumulator.
    pub fn accumulate(&mut self, field: &str, value: f64) {
        if let Some(entry) = self.subtotals.iter_mut().find(|(f, _)| f == field) {
            entry.1 += value;
        } else {
            self.subtotals.push((field.to_string(), value));
        }
    }

    /// Get a subtotal and reset it.
    pub fn take_subtotal(&mut self, field: &str) -> f64 {
        if let Some(entry) = self.subtotals.iter_mut().find(|(f, _)| f == field) {
            let val = entry.1;
            entry.1 = 0.0;
            val
        } else {
            0.0
        }
    }

    /// Reset all subtotals.
    pub fn reset(&mut self) {
        for entry in &mut self.subtotals {
            entry.1 = 0.0;
        }
        self.count = 0;
    }
}

// ---------------------------------------------------------------------------
// Report Engine
// ---------------------------------------------------------------------------

/// Report generation engine with page formatting.
#[derive(Debug)]
pub struct ReportEngine {
    pub page_width: usize,
    pub page_length: usize,
    pub current_line: usize,
    pub current_page: usize,
    pub columns: Vec<ColumnDef>,
    pub output: Vec<String>,
    pub top_of_page: Vec<String>,
    pub end_of_page: Vec<String>,
    headers_printed: bool,
}

impl ReportEngine {
    pub fn new(page_width: usize, page_length: usize) -> Self {
        Self {
            page_width,
            page_length,
            current_line: 0,
            current_page: 0,
            columns: Vec::new(),
            output: Vec::new(),
            top_of_page: Vec::new(),
            end_of_page: Vec::new(),
            headers_printed: false,
        }
    }

    /// Define columns for DISPLAY output.
    pub fn set_columns(&mut self, columns: Vec<ColumnDef>) {
        self.columns = columns;
        self.headers_printed = false;
    }

    /// Set top-of-page lines.
    pub fn set_top_of_page(&mut self, lines: Vec<String>) {
        self.top_of_page = lines;
    }

    /// Set end-of-page lines.
    pub fn set_end_of_page(&mut self, lines: Vec<String>) {
        self.end_of_page = lines;
    }

    /// Start a new page.
    pub fn new_page(&mut self) {
        if self.current_page > 0 {
            // Emit end-of-page
            for line in &self.end_of_page.clone() {
                self.emit_raw(line.clone());
            }
            self.output.push("\x0C".to_string()); // form feed
        }
        self.current_page += 1;
        self.current_line = 0;
        self.headers_printed = false;

        // Emit top-of-page
        for line in &self.top_of_page.clone() {
            let formatted = line.replace("*PAGE-NUMBER*", &self.current_page.to_string());
            self.emit_raw(formatted);
        }
    }

    /// DISPLAY: columnar output with automatic headers.
    pub fn display(&mut self, values: &[NaturalValue]) {
        if !self.headers_printed && !self.columns.is_empty() {
            self.print_column_headers();
        }

        // Check for page overflow
        if self.page_length > 0 && self.current_line >= self.page_length {
            self.new_page();
            if !self.headers_printed && !self.columns.is_empty() {
                self.print_column_headers();
            }
        }

        let mut parts = Vec::new();
        for (i, val) in values.iter().enumerate() {
            let display = val.to_display_string();
            if let Some(col) = self.columns.get(i) {
                parts.push(col.format_value(&display));
            } else {
                parts.push(display);
            }
        }
        let line = parts.join(" ");
        self.emit(line);
    }

    /// WRITE: free-form output.
    pub fn write_line(&mut self, text: &str) {
        if self.page_length > 0 && self.current_line >= self.page_length {
            self.new_page();
        }
        self.emit(text.to_string());
    }

    /// PRINT: same as write_line (alias behavior).
    pub fn print_line(&mut self, text: &str) {
        self.write_line(text);
    }

    fn print_column_headers(&mut self) {
        let header_line: String = self.columns.iter()
            .map(|c| c.format_value(&c.header))
            .collect::<Vec<_>>()
            .join(" ");
        self.emit(header_line);

        let sep_line: String = self.columns.iter()
            .map(|c| "-".repeat(c.width))
            .collect::<Vec<_>>()
            .join(" ");
        self.emit(sep_line);

        self.headers_printed = true;
    }

    fn emit(&mut self, line: String) {
        self.output.push(line);
        self.current_line += 1;
    }

    fn emit_raw(&mut self, line: String) {
        self.output.push(line);
        self.current_line += 1;
    }

    /// Get all output lines.
    pub fn get_output(&self) -> &[String] {
        &self.output
    }

    /// Get total line count.
    pub fn line_count(&self) -> usize {
        self.output.len()
    }
}

impl Default for ReportEngine {
    fn default() -> Self {
        Self::new(132, 60)
    }
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, thiserror::Error)]
pub enum OutputError {
    #[error("invalid column index: {0}")]
    InvalidColumn(usize),
    #[error("page overflow")]
    PageOverflow,
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_column_format_left() {
        let col = ColumnDef::new("NAME", 10, Alignment::Left);
        assert_eq!(col.format_value("Alice"), "Alice     ");
    }

    #[test]
    fn test_column_format_right() {
        let col = ColumnDef::new("SALARY", 10, Alignment::Right);
        assert_eq!(col.format_value("50000"), "     50000");
    }

    #[test]
    fn test_column_format_center() {
        let col = ColumnDef::new("DEPT", 10, Alignment::Center);
        let result = col.format_value("D01");
        assert_eq!(result.len(), 10);
        assert!(result.contains("D01"));
    }

    #[test]
    fn test_column_format_truncate() {
        let col = ColumnDef::new("X", 5, Alignment::Left);
        assert_eq!(col.format_value("ABCDEFGHIJ"), "ABCDE");
    }

    #[test]
    fn test_display_with_headers() {
        let mut engine = ReportEngine::new(80, 60);
        engine.set_columns(vec![
            ColumnDef::new("NAME", 15, Alignment::Left),
            ColumnDef::new("DEPT", 5, Alignment::Left),
        ]);
        engine.display(&[
            NaturalValue::Alpha("Smith".into()),
            NaturalValue::Alpha("D01".into()),
        ]);
        // Should have header + separator + data line
        assert_eq!(engine.line_count(), 3);
        assert!(engine.output[0].contains("NAME"));
        assert!(engine.output[1].contains("---"));
        assert!(engine.output[2].contains("Smith"));
    }

    #[test]
    fn test_display_no_columns() {
        let mut engine = ReportEngine::new(80, 60);
        engine.display(&[NaturalValue::Alpha("hello".into())]);
        assert_eq!(engine.line_count(), 1);
        assert_eq!(engine.output[0], "hello");
    }

    #[test]
    fn test_write_line() {
        let mut engine = ReportEngine::new(80, 60);
        engine.write_line("Free-form text");
        assert_eq!(engine.output[0], "Free-form text");
    }

    #[test]
    fn test_print_line() {
        let mut engine = ReportEngine::new(80, 60);
        engine.print_line("Print output");
        assert_eq!(engine.output[0], "Print output");
    }

    #[test]
    fn test_new_page() {
        let mut engine = ReportEngine::new(80, 3);
        engine.set_top_of_page(vec!["--- Page *PAGE-NUMBER* ---".into()]);
        engine.new_page();
        assert_eq!(engine.current_page, 1);
        assert!(engine.output[0].contains("Page 1"));
    }

    #[test]
    fn test_page_overflow() {
        let mut engine = ReportEngine::new(80, 3);
        engine.write_line("line 1");
        engine.write_line("line 2");
        engine.write_line("line 3");
        // Next line should trigger new page
        engine.write_line("line 4");
        assert!(engine.current_page > 0);
    }

    #[test]
    fn test_end_of_page() {
        let mut engine = ReportEngine::new(80, 60);
        engine.set_end_of_page(vec!["--- End ---".into()]);
        engine.new_page(); // page 1
        engine.new_page(); // page 2 triggers end-of-page for page 1
        let has_end = engine.output.iter().any(|l| l.contains("End"));
        assert!(has_end);
    }

    #[test]
    fn test_control_break_check() {
        let mut cb = ControlBreak::new("DEPT");
        assert!(!cb.check(&NaturalValue::Alpha("D01".into()))); // First time
        assert!(!cb.check(&NaturalValue::Alpha("D01".into()))); // Same
        assert!(cb.check(&NaturalValue::Alpha("D02".into())));  // Changed!
    }

    #[test]
    fn test_control_break_subtotal() {
        let mut cb = ControlBreak::new("DEPT");
        cb.accumulate("SALARY", 50000.0);
        cb.accumulate("SALARY", 55000.0);
        let total = cb.take_subtotal("SALARY");
        assert_eq!(total, 105000.0);
        // After take, should be reset
        assert_eq!(cb.take_subtotal("SALARY"), 0.0);
    }

    #[test]
    fn test_control_break_count() {
        let mut cb = ControlBreak::new("DEPT");
        cb.check(&NaturalValue::Alpha("D01".into()));
        cb.check(&NaturalValue::Alpha("D01".into()));
        cb.check(&NaturalValue::Alpha("D02".into()));
        assert_eq!(cb.count, 3);
    }

    #[test]
    fn test_control_break_reset() {
        let mut cb = ControlBreak::new("DEPT");
        cb.accumulate("SALARY", 100.0);
        cb.count = 5;
        cb.reset();
        assert_eq!(cb.count, 0);
        assert_eq!(cb.take_subtotal("SALARY"), 0.0);
    }

    #[test]
    fn test_report_default() {
        let engine = ReportEngine::default();
        assert_eq!(engine.page_width, 132);
        assert_eq!(engine.page_length, 60);
    }

    #[test]
    fn test_display_multiple_rows() {
        let mut engine = ReportEngine::new(80, 60);
        engine.set_columns(vec![
            ColumnDef::new("NAME", 10, Alignment::Left),
        ]);
        engine.display(&[NaturalValue::Alpha("Alice".into())]);
        engine.display(&[NaturalValue::Alpha("Bob".into())]);
        engine.display(&[NaturalValue::Alpha("Charlie".into())]);
        // 2 header lines + 3 data lines
        assert_eq!(engine.line_count(), 5);
    }

    #[test]
    fn test_top_of_page_substitution() {
        let mut engine = ReportEngine::new(80, 60);
        engine.set_top_of_page(vec!["Page *PAGE-NUMBER* of Report".into()]);
        engine.new_page();
        assert!(engine.output[0].contains("Page 1"));
    }

    #[test]
    fn test_control_break_multiple_subtotals() {
        let mut cb = ControlBreak::new("DEPT");
        cb.accumulate("SALARY", 100.0);
        cb.accumulate("BONUS", 10.0);
        cb.accumulate("SALARY", 200.0);
        cb.accumulate("BONUS", 20.0);
        assert_eq!(cb.take_subtotal("SALARY"), 300.0);
        assert_eq!(cb.take_subtotal("BONUS"), 30.0);
    }
}
