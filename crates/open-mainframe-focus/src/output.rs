//! FOC-108: Output Formatting (5 stories).
//!
//! OutputFormatter trait with TextFormatter, HtmlFormatter, and HoldFormatter
//! implementations for rendering report output in various formats.

use thiserror::Error;

use crate::table_engine::{ReportOutput, ReportRow, RowType};

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum OutputError {
    #[error("formatting error: {0}")]
    FormatError(String),
    #[error("no data to format")]
    NoData,
}

// ---------------------------------------------------------------------------
// StyleSheet
// ---------------------------------------------------------------------------

/// Style specifications for output formatting.
#[derive(Debug, Clone)]
pub struct StyleSheet {
    pub heading_style: StyleDef,
    pub column_style: StyleDef,
    pub footing_style: StyleDef,
    pub detail_style: StyleDef,
    pub total_style: StyleDef,
}

/// A single style definition.
#[derive(Debug, Clone)]
pub struct StyleDef {
    pub font: FontStyle,
    pub color: Option<String>,
    pub alignment: Alignment,
}

/// Font styles.
#[derive(Debug, Clone, PartialEq)]
pub enum FontStyle {
    Normal,
    Bold,
    Italic,
    BoldItalic,
}

/// Text alignment.
#[derive(Debug, Clone, PartialEq)]
pub enum Alignment {
    Left,
    Center,
    Right,
}

impl Default for StyleSheet {
    fn default() -> Self {
        Self {
            heading_style: StyleDef {
                font: FontStyle::Bold,
                color: None,
                alignment: Alignment::Center,
            },
            column_style: StyleDef {
                font: FontStyle::Bold,
                color: None,
                alignment: Alignment::Left,
            },
            footing_style: StyleDef {
                font: FontStyle::Normal,
                color: None,
                alignment: Alignment::Center,
            },
            detail_style: StyleDef {
                font: FontStyle::Normal,
                color: None,
                alignment: Alignment::Left,
            },
            total_style: StyleDef {
                font: FontStyle::Bold,
                color: None,
                alignment: Alignment::Left,
            },
        }
    }
}

// ---------------------------------------------------------------------------
// OutputFormatter trait
// ---------------------------------------------------------------------------

/// Common interface for output formatting.
pub trait OutputFormatter {
    fn format(&self, report: &ReportOutput) -> Result<String, OutputError>;
    fn format_name(&self) -> &str;
}

// ---------------------------------------------------------------------------
// TextFormatter
// ---------------------------------------------------------------------------

/// Plain-text report formatter with column alignment.
pub struct TextFormatter {
    pub column_width: usize,
    pub separator: char,
    pub style: StyleSheet,
}

impl TextFormatter {
    pub fn new() -> Self {
        Self {
            column_width: 15,
            separator: ' ',
            style: StyleSheet::default(),
        }
    }

    pub fn with_column_width(mut self, width: usize) -> Self {
        self.column_width = width;
        self
    }

    fn format_row(&self, row: &ReportRow, column_order: &[String]) -> String {
        let mut parts = Vec::new();
        for col in column_order {
            let val = row
                .cells
                .iter()
                .find(|(k, _)| k == col)
                .map(|(_, v)| v.as_str())
                .unwrap_or_default();
            parts.push(format!("{:<width$}", val, width = self.column_width));
        }
        parts.join(&self.separator.to_string())
    }
}

impl Default for TextFormatter {
    fn default() -> Self {
        Self::new()
    }
}

impl OutputFormatter for TextFormatter {
    fn format(&self, report: &ReportOutput) -> Result<String, OutputError> {
        let mut lines = Vec::new();

        // Heading
        if let Some(ref heading) = report.heading {
            lines.push(heading.clone());
            lines.push("=".repeat(report.columns.len() * (self.column_width + 1)));
        }

        // Column headers
        let header: Vec<String> = report
            .columns
            .iter()
            .map(|c| format!("{:<width$}", c, width = self.column_width))
            .collect();
        lines.push(header.join(&self.separator.to_string()));
        lines.push("-".repeat(report.columns.len() * (self.column_width + 1)));

        // Data rows
        for row in &report.rows {
            let prefix = match row.row_type {
                RowType::Total => "*** ",
                RowType::Subtotal => "  * ",
                _ => "    ",
            };
            let formatted = self.format_row(row, &report.columns);
            lines.push(format!("{prefix}{formatted}"));
        }

        // Footing
        if let Some(ref footing) = report.footing {
            lines.push("=".repeat(report.columns.len() * (self.column_width + 1)));
            lines.push(footing.clone());
        }

        // Subfoot
        if let Some(ref subfoot) = report.subfoot {
            lines.push(subfoot.clone());
        }

        Ok(lines.join("\n"))
    }

    fn format_name(&self) -> &str {
        "TEXT"
    }
}

// ---------------------------------------------------------------------------
// HtmlFormatter
// ---------------------------------------------------------------------------

/// HTML table formatter with inline styles.
pub struct HtmlFormatter {
    pub style: StyleSheet,
    pub css_class: Option<String>,
}

impl HtmlFormatter {
    pub fn new() -> Self {
        Self {
            style: StyleSheet::default(),
            css_class: None,
        }
    }

    pub fn with_css_class(mut self, class: &str) -> Self {
        self.css_class = Some(class.to_string());
        self
    }

    fn font_style_css(font: &FontStyle) -> &'static str {
        match font {
            FontStyle::Normal => "",
            FontStyle::Bold => "font-weight:bold;",
            FontStyle::Italic => "font-style:italic;",
            FontStyle::BoldItalic => "font-weight:bold;font-style:italic;",
        }
    }

    fn color_css(color: &Option<String>) -> String {
        match color {
            Some(c) => format!("color:{c};"),
            None => String::new(),
        }
    }
}

impl Default for HtmlFormatter {
    fn default() -> Self {
        Self::new()
    }
}

impl OutputFormatter for HtmlFormatter {
    fn format(&self, report: &ReportOutput) -> Result<String, OutputError> {
        let mut html = String::new();
        let class_attr = self
            .css_class
            .as_ref()
            .map_or(String::new(), |c| format!(" class=\"{c}\""));

        html.push_str(&format!("<table{class_attr}>\n"));

        // Heading (caption)
        if let Some(ref heading) = report.heading {
            let style = Self::font_style_css(&self.style.heading_style.font);
            let color = Self::color_css(&self.style.heading_style.color);
            html.push_str(&format!(
                "  <caption style=\"{style}{color}\">{heading}</caption>\n"
            ));
        }

        // Column headers
        html.push_str("  <thead>\n    <tr>\n");
        let col_style = Self::font_style_css(&self.style.column_style.font);
        let col_color = Self::color_css(&self.style.column_style.color);
        for col in &report.columns {
            html.push_str(&format!(
                "      <th style=\"{col_style}{col_color}\">{col}</th>\n"
            ));
        }
        html.push_str("    </tr>\n  </thead>\n");

        // Data rows
        html.push_str("  <tbody>\n");
        for row in &report.rows {
            let row_style = match row.row_type {
                RowType::Total | RowType::Subtotal => {
                    Self::font_style_css(&self.style.total_style.font)
                }
                _ => Self::font_style_css(&self.style.detail_style.font),
            };
            html.push_str(&format!("    <tr style=\"{row_style}\">\n"));
            for col in &report.columns {
                let val = row
                    .cells
                    .iter()
                    .find(|(k, _)| k == col)
                    .map(|(_, v)| Self::escape_html(&v.as_str()))
                    .unwrap_or_default();
                html.push_str(&format!("      <td>{val}</td>\n"));
            }
            html.push_str("    </tr>\n");
        }
        html.push_str("  </tbody>\n");

        // Footing
        if let Some(ref footing) = report.footing {
            let style = Self::font_style_css(&self.style.footing_style.font);
            html.push_str(&format!(
                "  <tfoot>\n    <tr>\n      <td colspan=\"{}\" style=\"{style}\">{footing}</td>\n    </tr>\n  </tfoot>\n",
                report.columns.len()
            ));
        }

        html.push_str("</table>");

        Ok(html)
    }

    fn format_name(&self) -> &str {
        "HTML"
    }
}

impl HtmlFormatter {
    fn escape_html(s: &str) -> String {
        s.replace('&', "&amp;")
            .replace('<', "&lt;")
            .replace('>', "&gt;")
            .replace('"', "&quot;")
    }
}

// ---------------------------------------------------------------------------
// HoldFormatter
// ---------------------------------------------------------------------------

/// HOLD file formatter — writes records for later processing.
pub struct HoldFormatter {
    pub delimiter: char,
    pub include_header: bool,
}

impl HoldFormatter {
    pub fn new() -> Self {
        Self {
            delimiter: '|',
            include_header: true,
        }
    }

    pub fn with_delimiter(mut self, delim: char) -> Self {
        self.delimiter = delim;
        self
    }
}

impl Default for HoldFormatter {
    fn default() -> Self {
        Self::new()
    }
}

impl OutputFormatter for HoldFormatter {
    fn format(&self, report: &ReportOutput) -> Result<String, OutputError> {
        let mut lines = Vec::new();

        if self.include_header {
            lines.push(report.columns.join(&self.delimiter.to_string()));
        }

        for row in &report.rows {
            let vals: Vec<String> = report
                .columns
                .iter()
                .map(|col| {
                    row.cells
                        .iter()
                        .find(|(k, _)| k == col)
                        .map(|(_, v)| v.as_str())
                        .unwrap_or_default()
                })
                .collect();
            lines.push(vals.join(&self.delimiter.to_string()));
        }

        Ok(lines.join("\n"))
    }

    fn format_name(&self) -> &str {
        "HOLD"
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::table_engine::CellValue;

    fn sample_report() -> ReportOutput {
        ReportOutput {
            heading: Some("Employee Report".into()),
            footing: Some("End of Report".into()),
            subfoot: None,
            columns: vec!["NAME".into(), "SALARY".into()],
            rows: vec![
                ReportRow {
                    row_type: RowType::Detail,
                    cells: vec![
                        ("NAME".into(), CellValue::Str("Alice".into())),
                        ("SALARY".into(), CellValue::Num(60000.0)),
                    ],
                },
                ReportRow {
                    row_type: RowType::Detail,
                    cells: vec![
                        ("NAME".into(), CellValue::Str("Bob".into())),
                        ("SALARY".into(), CellValue::Num(55000.0)),
                    ],
                },
                ReportRow {
                    row_type: RowType::Total,
                    cells: vec![
                        ("NAME".into(), CellValue::Str("TOTAL".into())),
                        ("SALARY".into(), CellValue::Num(115000.0)),
                    ],
                },
            ],
        }
    }

    // --- TextFormatter tests ---

    #[test]
    fn test_text_format_basic() {
        let fmt = TextFormatter::new();
        let output = fmt.format(&sample_report()).unwrap();
        assert!(output.contains("Employee Report"));
        assert!(output.contains("NAME"));
        assert!(output.contains("Alice"));
        assert!(output.contains("Bob"));
        assert!(output.contains("End of Report"));
    }

    #[test]
    fn test_text_format_heading() {
        let fmt = TextFormatter::new();
        let output = fmt.format(&sample_report()).unwrap();
        let lines: Vec<&str> = output.lines().collect();
        assert_eq!(lines[0], "Employee Report");
    }

    #[test]
    fn test_text_format_column_headers() {
        let fmt = TextFormatter::new();
        let output = fmt.format(&sample_report()).unwrap();
        assert!(output.contains("NAME"));
        assert!(output.contains("SALARY"));
    }

    #[test]
    fn test_text_format_total_prefix() {
        let fmt = TextFormatter::new();
        let output = fmt.format(&sample_report()).unwrap();
        assert!(output.contains("*** "));
    }

    #[test]
    fn test_text_format_custom_width() {
        let fmt = TextFormatter::new().with_column_width(20);
        let output = fmt.format(&sample_report()).unwrap();
        assert!(output.contains("NAME"));
    }

    #[test]
    fn test_text_format_name() {
        let fmt = TextFormatter::new();
        assert_eq!(fmt.format_name(), "TEXT");
    }

    // --- HtmlFormatter tests ---

    #[test]
    fn test_html_format_basic() {
        let fmt = HtmlFormatter::new();
        let output = fmt.format(&sample_report()).unwrap();
        assert!(output.contains("<table>"));
        assert!(output.contains("</table>"));
        assert!(output.contains("<th"));
        assert!(output.contains("<td>"));
    }

    #[test]
    fn test_html_format_caption() {
        let fmt = HtmlFormatter::new();
        let output = fmt.format(&sample_report()).unwrap();
        assert!(output.contains("<caption"));
        assert!(output.contains("Employee Report"));
    }

    #[test]
    fn test_html_format_columns() {
        let fmt = HtmlFormatter::new();
        let output = fmt.format(&sample_report()).unwrap();
        assert!(output.contains("<th"));
        assert!(output.contains("NAME"));
        assert!(output.contains("SALARY"));
    }

    #[test]
    fn test_html_format_data() {
        let fmt = HtmlFormatter::new();
        let output = fmt.format(&sample_report()).unwrap();
        assert!(output.contains("Alice"));
        assert!(output.contains("60000"));
    }

    #[test]
    fn test_html_format_footing() {
        let fmt = HtmlFormatter::new();
        let output = fmt.format(&sample_report()).unwrap();
        assert!(output.contains("<tfoot>"));
        assert!(output.contains("End of Report"));
    }

    #[test]
    fn test_html_format_css_class() {
        let fmt = HtmlFormatter::new().with_css_class("report-table");
        let output = fmt.format(&sample_report()).unwrap();
        assert!(output.contains("class=\"report-table\""));
    }

    #[test]
    fn test_html_escape() {
        let report = ReportOutput {
            heading: None,
            footing: None,
            subfoot: None,
            columns: vec!["DATA".into()],
            rows: vec![ReportRow {
                row_type: RowType::Detail,
                cells: vec![("DATA".into(), CellValue::Str("<script>alert('xss')</script>".into()))],
            }],
        };
        let fmt = HtmlFormatter::new();
        let output = fmt.format(&report).unwrap();
        assert!(output.contains("&lt;script&gt;"));
        assert!(!output.contains("<script>"));
    }

    #[test]
    fn test_html_format_name() {
        let fmt = HtmlFormatter::new();
        assert_eq!(fmt.format_name(), "HTML");
    }

    // --- HoldFormatter tests ---

    #[test]
    fn test_hold_format_basic() {
        let fmt = HoldFormatter::new();
        let output = fmt.format(&sample_report()).unwrap();
        let lines: Vec<&str> = output.lines().collect();
        assert_eq!(lines[0], "NAME|SALARY");
        assert!(lines[1].contains("Alice"));
        assert!(lines[1].contains('|'));
    }

    #[test]
    fn test_hold_format_no_header() {
        let mut fmt = HoldFormatter::new();
        fmt.include_header = false;
        let output = fmt.format(&sample_report()).unwrap();
        let lines: Vec<&str> = output.lines().collect();
        // First line should be data, not header
        assert!(lines[0].contains("Alice"));
    }

    #[test]
    fn test_hold_format_custom_delimiter() {
        let fmt = HoldFormatter::new().with_delimiter(',');
        let output = fmt.format(&sample_report()).unwrap();
        assert!(output.contains("NAME,SALARY"));
        assert!(output.contains("Alice,"));
    }

    #[test]
    fn test_hold_format_all_rows() {
        let fmt = HoldFormatter::new();
        let output = fmt.format(&sample_report()).unwrap();
        let lines: Vec<&str> = output.lines().collect();
        // 1 header + 3 data rows
        assert_eq!(lines.len(), 4);
    }

    #[test]
    fn test_hold_format_name() {
        let fmt = HoldFormatter::new();
        assert_eq!(fmt.format_name(), "HOLD");
    }

    // --- StyleSheet tests ---

    #[test]
    fn test_stylesheet_default() {
        let ss = StyleSheet::default();
        assert_eq!(ss.heading_style.font, FontStyle::Bold);
        assert_eq!(ss.heading_style.alignment, Alignment::Center);
        assert_eq!(ss.detail_style.font, FontStyle::Normal);
    }

    #[test]
    fn test_style_with_color() {
        let mut ss = StyleSheet::default();
        ss.heading_style.color = Some("red".to_string());
        assert_eq!(ss.heading_style.color, Some("red".to_string()));
    }

    #[test]
    fn test_html_style_applied() {
        let mut fmt = HtmlFormatter::new();
        fmt.style.heading_style.color = Some("blue".to_string());
        let output = fmt.format(&sample_report()).unwrap();
        assert!(output.contains("color:blue;"));
    }

    // --- No data tests ---

    #[test]
    fn test_format_empty_report() {
        let report = ReportOutput {
            heading: None,
            footing: None,
            subfoot: None,
            columns: vec!["A".into()],
            rows: Vec::new(),
        };
        let fmt = TextFormatter::new();
        let output = fmt.format(&report).unwrap();
        // Should have column header but no data rows
        assert!(output.contains('A'));
    }
}
