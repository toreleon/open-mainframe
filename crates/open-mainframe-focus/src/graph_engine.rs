//! FOC-103: GRAPH Engine (4 stories).
//!
//! Executes GRAPH requests producing text-mode charts including BAR, PIE,
//! LINE, and AREA chart types with ASCII-art rendering.

use thiserror::Error;

use crate::table_engine::DataRecord;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum GraphError {
    #[error("no data to graph")]
    NoData,
    #[error("field not found: {0}")]
    FieldNotFound(String),
    #[error("unsupported chart type: {0}")]
    UnsupportedChartType(String),
}

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// Supported chart types.
#[derive(Debug, Clone, PartialEq)]
pub enum ChartType {
    Bar,
    Pie,
    Line,
    Area,
}

impl ChartType {
    pub fn from_str_name(s: &str) -> Result<Self, GraphError> {
        match s.to_uppercase().as_str() {
            "BAR" => Ok(ChartType::Bar),
            "PIE" => Ok(ChartType::Pie),
            "LINE" => Ok(ChartType::Line),
            "AREA" => Ok(ChartType::Area),
            _ => Err(GraphError::UnsupportedChartType(s.to_string())),
        }
    }
}

/// Chart formatting options.
#[derive(Debug, Clone)]
pub struct ChartFormat {
    pub title: Option<String>,
    pub width: usize,
    pub height: usize,
    pub show_legend: bool,
    pub bar_char: char,
    pub fill_char: char,
}

impl Default for ChartFormat {
    fn default() -> Self {
        Self {
            title: None,
            width: 60,
            height: 20,
            show_legend: true,
            bar_char: '#',
            fill_char: '*',
        }
    }
}

/// The rendered graph output.
#[derive(Debug, Clone)]
pub struct GraphOutput {
    pub chart_type: ChartType,
    pub lines: Vec<String>,
    pub legend: Vec<(String, f64)>,
}

// ---------------------------------------------------------------------------
// Graph engine
// ---------------------------------------------------------------------------

/// Text-mode chart rendering engine.
pub struct GraphEngine;

impl GraphEngine {
    /// Render a chart from the given data.
    pub fn render(
        chart_type: &ChartType,
        data: &[DataRecord],
        measure_field: &str,
        dim_field: &str,
        format: &ChartFormat,
    ) -> Result<GraphOutput, GraphError> {
        if data.is_empty() {
            return Err(GraphError::NoData);
        }

        // Aggregate data by dimension
        let aggregated = Self::aggregate(data, measure_field, dim_field);
        if aggregated.is_empty() {
            return Err(GraphError::NoData);
        }

        let lines = match chart_type {
            ChartType::Bar => Self::render_bar(&aggregated, format),
            ChartType::Pie => Self::render_pie(&aggregated, format),
            ChartType::Line => Self::render_line(&aggregated, format),
            ChartType::Area => Self::render_area(&aggregated, format),
        };

        Ok(GraphOutput {
            chart_type: chart_type.clone(),
            lines,
            legend: aggregated,
        })
    }

    /// Aggregate measure values by dimension.
    fn aggregate(
        data: &[DataRecord],
        measure_field: &str,
        dim_field: &str,
    ) -> Vec<(String, f64)> {
        let mut map: Vec<(String, f64)> = Vec::new();
        for rec in data {
            let dim = rec
                .get(dim_field)
                .map_or("(unknown)".to_string(), |v| v.as_str());
            let val = rec.get(measure_field).map_or(0.0, |v| v.as_num());
            if let Some(entry) = map.iter_mut().find(|(k, _)| *k == dim) {
                entry.1 += val;
            } else {
                map.push((dim, val));
            }
        }
        map
    }

    /// Render horizontal bar chart.
    fn render_bar(data: &[(String, f64)], format: &ChartFormat) -> Vec<String> {
        let mut lines = Vec::new();
        let max_val = data.iter().map(|(_, v)| *v).fold(0.0_f64, f64::max);
        let max_label_len = data.iter().map(|(l, _)| l.len()).max().unwrap_or(5);
        let bar_width = format.width.saturating_sub(max_label_len + 15);

        if let Some(ref title) = format.title {
            lines.push(title.clone());
            lines.push("=".repeat(format.width));
        }

        for (label, val) in data {
            let bar_len = if max_val > 0.0 {
                ((val / max_val) * bar_width as f64) as usize
            } else {
                0
            };
            let bar = format.bar_char.to_string().repeat(bar_len);
            lines.push(format!(
                "{:<width$} | {:<bar_w$} {:.0}",
                label,
                bar,
                val,
                width = max_label_len,
                bar_w = bar_width,
            ));
        }

        if format.show_legend {
            lines.push(String::new());
            lines.push(format!(
                "Legend: '{}' = value unit",
                format.bar_char
            ));
        }

        lines
    }

    /// Render text-mode pie chart with percentages.
    fn render_pie(data: &[(String, f64)], format: &ChartFormat) -> Vec<String> {
        let mut lines = Vec::new();
        let total: f64 = data.iter().map(|(_, v)| *v).sum();

        if let Some(ref title) = format.title {
            lines.push(title.clone());
            lines.push("=".repeat(format.width));
        }

        let symbols = ['#', '*', '+', '=', '-', '.', '@', '%'];
        let max_label_len = data.iter().map(|(l, _)| l.len()).max().unwrap_or(5);
        let bar_width = format.width.saturating_sub(max_label_len + 20);

        for (i, (label, val)) in data.iter().enumerate() {
            let pct = if total > 0.0 {
                (val / total) * 100.0
            } else {
                0.0
            };
            let fill_len = ((pct / 100.0) * bar_width as f64) as usize;
            let sym = symbols[i % symbols.len()];
            let fill = sym.to_string().repeat(fill_len);
            lines.push(format!(
                "{:<width$} [{:<bar_w$}] {:.1}%",
                label,
                fill,
                pct,
                width = max_label_len,
                bar_w = bar_width,
            ));
        }

        // Pie total
        lines.push(format!("Total: {total:.0}"));

        lines
    }

    /// Render text-mode line chart for time series.
    fn render_line(data: &[(String, f64)], format: &ChartFormat) -> Vec<String> {
        let mut lines = Vec::new();
        let max_val = data.iter().map(|(_, v)| *v).fold(0.0_f64, f64::max);
        let min_val = data.iter().map(|(_, v)| *v).fold(f64::MAX, f64::min);
        let range = max_val - min_val;
        let chart_height = format.height;

        if let Some(ref title) = format.title {
            lines.push(title.clone());
            lines.push("=".repeat(format.width));
        }

        // Render top-down
        for row in (0..chart_height).rev() {
            let threshold = if range > 0.0 {
                min_val + (row as f64 / chart_height as f64) * range
            } else {
                min_val
            };
            let mut line = String::new();
            if row == chart_height - 1 {
                line.push_str(&format!("{:>8.0} |", max_val));
            } else if row == 0 {
                line.push_str(&format!("{:>8.0} |", min_val));
            } else {
                line.push_str("         |");
            }

            for (_label, val) in data {
                if *val >= threshold {
                    line.push_str(" * ");
                } else {
                    line.push_str("   ");
                }
            }
            lines.push(line);
        }

        // X-axis
        let mut axis = String::from("         +");
        for _ in data {
            axis.push_str("---");
        }
        lines.push(axis);

        // Labels
        let mut label_line = String::from("          ");
        for (label, _) in data {
            let short = if label.len() > 3 {
                &label[..3]
            } else {
                label
            };
            label_line.push_str(&format!("{short:>3}"));
        }
        lines.push(label_line);

        lines
    }

    /// Render text-mode area chart (filled below the line).
    fn render_area(data: &[(String, f64)], format: &ChartFormat) -> Vec<String> {
        let mut lines = Vec::new();
        let max_val = data.iter().map(|(_, v)| *v).fold(0.0_f64, f64::max);
        let min_val = data.iter().map(|(_, v)| *v).fold(f64::MAX, f64::min);
        let range = max_val - min_val;
        let chart_height = format.height;

        if let Some(ref title) = format.title {
            lines.push(title.clone());
            lines.push("=".repeat(format.width));
        }

        for row in (0..chart_height).rev() {
            let threshold = if range > 0.0 {
                min_val + (row as f64 / chart_height as f64) * range
            } else {
                min_val
            };
            let mut line = String::from("         |");
            for (_label, val) in data {
                if *val >= threshold {
                    line.push_str(&format!(" {} ", format.fill_char));
                } else {
                    line.push_str("   ");
                }
            }
            lines.push(line);
        }

        // X-axis
        let mut axis = String::from("         +");
        for _ in data {
            axis.push_str("---");
        }
        lines.push(axis);

        // Labels
        let mut label_line = String::from("          ");
        for (label, _) in data {
            let short = if label.len() > 3 {
                &label[..3]
            } else {
                label
            };
            label_line.push_str(&format!("{short:>3}"));
        }
        lines.push(label_line);

        lines
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use crate::table_engine::CellValue;

    fn sample_data() -> Vec<DataRecord> {
        vec![
            HashMap::from([
                ("REGION".into(), CellValue::Str("EAST".into())),
                ("SALES".into(), CellValue::Num(100.0)),
            ]),
            HashMap::from([
                ("REGION".into(), CellValue::Str("WEST".into())),
                ("SALES".into(), CellValue::Num(200.0)),
            ]),
            HashMap::from([
                ("REGION".into(), CellValue::Str("NORTH".into())),
                ("SALES".into(), CellValue::Num(150.0)),
            ]),
            HashMap::from([
                ("REGION".into(), CellValue::Str("SOUTH".into())),
                ("SALES".into(), CellValue::Num(80.0)),
            ]),
        ]
    }

    fn time_series_data() -> Vec<DataRecord> {
        vec![
            HashMap::from([
                ("MONTH".into(), CellValue::Str("JAN".into())),
                ("REVENUE".into(), CellValue::Num(1000.0)),
            ]),
            HashMap::from([
                ("MONTH".into(), CellValue::Str("FEB".into())),
                ("REVENUE".into(), CellValue::Num(1500.0)),
            ]),
            HashMap::from([
                ("MONTH".into(), CellValue::Str("MAR".into())),
                ("REVENUE".into(), CellValue::Num(1200.0)),
            ]),
            HashMap::from([
                ("MONTH".into(), CellValue::Str("APR".into())),
                ("REVENUE".into(), CellValue::Num(1800.0)),
            ]),
            HashMap::from([
                ("MONTH".into(), CellValue::Str("MAY".into())),
                ("REVENUE".into(), CellValue::Num(2000.0)),
            ]),
        ]
    }

    #[test]
    fn test_bar_chart() {
        let fmt = ChartFormat {
            title: Some("Sales by Region".into()),
            ..ChartFormat::default()
        };
        let output =
            GraphEngine::render(&ChartType::Bar, &sample_data(), "SALES", "REGION", &fmt).unwrap();
        assert_eq!(output.chart_type, ChartType::Bar);
        assert!(!output.lines.is_empty());
        assert!(output.lines[0].contains("Sales by Region"));
        assert_eq!(output.legend.len(), 4);
    }

    #[test]
    fn test_bar_chart_values() {
        let fmt = ChartFormat::default();
        let output =
            GraphEngine::render(&ChartType::Bar, &sample_data(), "SALES", "REGION", &fmt).unwrap();
        // Check WEST has longest bar (200 is max)
        let west = output.legend.iter().find(|(l, _)| l == "WEST").unwrap();
        assert!((west.1 - 200.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_pie_chart() {
        let fmt = ChartFormat::default();
        let output =
            GraphEngine::render(&ChartType::Pie, &sample_data(), "SALES", "REGION", &fmt).unwrap();
        assert_eq!(output.chart_type, ChartType::Pie);
        // Check percentages exist in output
        let pct_line = output.lines.iter().any(|l| l.contains('%'));
        assert!(pct_line);
    }

    #[test]
    fn test_pie_chart_total() {
        let fmt = ChartFormat::default();
        let output =
            GraphEngine::render(&ChartType::Pie, &sample_data(), "SALES", "REGION", &fmt).unwrap();
        let total_line = output.lines.iter().find(|l| l.starts_with("Total:")).unwrap();
        assert!(total_line.contains("530"));
    }

    #[test]
    fn test_line_chart() {
        let fmt = ChartFormat {
            title: Some("Revenue Trend".into()),
            height: 10,
            ..ChartFormat::default()
        };
        let output = GraphEngine::render(
            &ChartType::Line,
            &time_series_data(),
            "REVENUE",
            "MONTH",
            &fmt,
        )
        .unwrap();
        assert_eq!(output.chart_type, ChartType::Line);
        assert!(!output.lines.is_empty());
        // Should have '*' markers
        let has_markers = output.lines.iter().any(|l| l.contains('*'));
        assert!(has_markers);
    }

    #[test]
    fn test_line_chart_axis() {
        let fmt = ChartFormat {
            height: 10,
            ..ChartFormat::default()
        };
        let output = GraphEngine::render(
            &ChartType::Line,
            &time_series_data(),
            "REVENUE",
            "MONTH",
            &fmt,
        )
        .unwrap();
        // Should have x-axis with +---
        let has_axis = output.lines.iter().any(|l| l.contains('+'));
        assert!(has_axis);
    }

    #[test]
    fn test_area_chart() {
        let fmt = ChartFormat {
            height: 10,
            fill_char: '*',
            ..ChartFormat::default()
        };
        let output = GraphEngine::render(
            &ChartType::Area,
            &time_series_data(),
            "REVENUE",
            "MONTH",
            &fmt,
        )
        .unwrap();
        assert_eq!(output.chart_type, ChartType::Area);
        // Should have fill chars
        let has_fill = output.lines.iter().any(|l| l.contains('*'));
        assert!(has_fill);
    }

    #[test]
    fn test_chart_type_from_str() {
        assert_eq!(ChartType::from_str_name("BAR").unwrap(), ChartType::Bar);
        assert_eq!(ChartType::from_str_name("pie").unwrap(), ChartType::Pie);
        assert_eq!(ChartType::from_str_name("Line").unwrap(), ChartType::Line);
        assert_eq!(ChartType::from_str_name("AREA").unwrap(), ChartType::Area);
        assert!(ChartType::from_str_name("SCATTER").is_err());
    }

    #[test]
    fn test_no_data_error() {
        let fmt = ChartFormat::default();
        let result = GraphEngine::render(&ChartType::Bar, &[], "SALES", "REGION", &fmt);
        assert!(result.is_err());
    }

    #[test]
    fn test_aggregation() {
        let data = vec![
            HashMap::from([
                ("REGION".into(), CellValue::Str("EAST".into())),
                ("SALES".into(), CellValue::Num(100.0)),
            ]),
            HashMap::from([
                ("REGION".into(), CellValue::Str("EAST".into())),
                ("SALES".into(), CellValue::Num(50.0)),
            ]),
        ];
        let fmt = ChartFormat::default();
        let output =
            GraphEngine::render(&ChartType::Bar, &data, "SALES", "REGION", &fmt).unwrap();
        assert_eq!(output.legend.len(), 1);
        assert!((output.legend[0].1 - 150.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_chart_format_default() {
        let fmt = ChartFormat::default();
        assert_eq!(fmt.width, 60);
        assert_eq!(fmt.height, 20);
        assert!(fmt.show_legend);
        assert_eq!(fmt.bar_char, '#');
    }

    #[test]
    fn test_chart_with_title() {
        let fmt = ChartFormat {
            title: Some("Test Chart".into()),
            ..ChartFormat::default()
        };
        let output =
            GraphEngine::render(&ChartType::Bar, &sample_data(), "SALES", "REGION", &fmt).unwrap();
        assert_eq!(output.lines[0], "Test Chart");
    }

    #[test]
    fn test_legend_content() {
        let fmt = ChartFormat::default();
        let output =
            GraphEngine::render(&ChartType::Bar, &sample_data(), "SALES", "REGION", &fmt).unwrap();
        let regions: Vec<&str> = output.legend.iter().map(|(l, _)| l.as_str()).collect();
        assert!(regions.contains(&"EAST"));
        assert!(regions.contains(&"WEST"));
        assert!(regions.contains(&"NORTH"));
        assert!(regions.contains(&"SOUTH"));
    }
}
