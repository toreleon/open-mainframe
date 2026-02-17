//! ICETOOL — high-level data analysis utility.
//!
//! Provides simplified batch-style operators for common data analysis tasks:
//! SORT, COPY, DISPLAY, OCCUR, STATS, UNIQUE, SELECT, COUNT.

use crate::fields::{DataType, SortSpec};

/// An ICETOOL operator.
#[derive(Debug, Clone)]
pub enum IceToolOp {
    /// Sort records.
    Sort {
        sort_spec: SortSpec,
    },
    /// Copy records (no sorting).
    Copy,
    /// Display formatted report.
    Display {
        /// Column specifications: (name, position, length, data_type, optional edit mask).
        columns: Vec<DisplayColumn>,
        /// Report header text.
        header: Option<String>,
    },
    /// Occurrence/frequency analysis.
    Occur {
        /// Field to count occurrences of.
        on_field: OnField,
    },
    /// Statistical analysis.
    Stats {
        /// Field to compute statistics for.
        on_field: OnField,
    },
    /// Unique values.
    Unique {
        /// Field to find unique values of.
        on_field: OnField,
    },
    /// Select records by occurrence count.
    Select {
        /// Field to group by.
        on_field: OnField,
        /// Minimum count to include.
        min_count: Option<usize>,
        /// Maximum count to include.
        max_count: Option<usize>,
    },
    /// Count total records.
    Count,
}

/// A column in a DISPLAY report.
#[derive(Debug, Clone)]
pub struct DisplayColumn {
    /// Column header name.
    pub name: String,
    /// Position in record (1-based).
    pub position: usize,
    /// Length in bytes.
    pub length: usize,
    /// Data type.
    pub data_type: DataType,
    /// Optional EDIT mask for formatting.
    pub edit_mask: Option<String>,
}

/// A field specification for ON(...) parameter.
#[derive(Debug, Clone)]
pub struct OnField {
    /// Position (1-based).
    pub position: usize,
    /// Length in bytes.
    pub length: usize,
    /// Data type.
    pub data_type: DataType,
}

impl OnField {
    /// Create a new ON field specification.
    pub fn new(position: usize, length: usize, data_type: DataType) -> Self {
        Self { position, length, data_type }
    }

    /// Extract the field bytes from a record.
    pub fn extract<'a>(&self, record: &'a [u8]) -> Option<&'a [u8]> {
        let start = self.position.checked_sub(1)?;
        let end = start.checked_add(self.length)?;
        if end <= record.len() {
            Some(&record[start..end])
        } else {
            None
        }
    }
}

/// Result from an ICETOOL operation.
#[derive(Debug)]
pub enum IceToolResult {
    /// Sorted/copied records.
    Records(Vec<Vec<u8>>),
    /// Formatted report lines.
    Report(Vec<String>),
    /// Occurrence counts: (value_bytes, count).
    Occurrences(Vec<(Vec<u8>, usize)>),
    /// Statistics for a numeric field.
    Statistics(FieldStats),
    /// Unique values.
    UniqueValues(Vec<Vec<u8>>),
    /// Selected records.
    Selected(Vec<Vec<u8>>),
    /// Record count.
    RecordCount(usize),
}

/// Statistics for a numeric field.
#[derive(Debug, Clone)]
pub struct FieldStats {
    /// Minimum value.
    pub min: i64,
    /// Maximum value.
    pub max: i64,
    /// Sum of all values.
    pub sum: i64,
    /// Number of records.
    pub count: usize,
}

impl FieldStats {
    /// Compute average (truncated integer division).
    pub fn average(&self) -> i64 {
        if self.count == 0 {
            0
        } else {
            self.sum / self.count as i64
        }
    }
}

/// Execute an ICETOOL operation on in-memory records.
pub fn execute(op: &IceToolOp, records: &[Vec<u8>]) -> IceToolResult {
    match op {
        IceToolOp::Sort { sort_spec } => {
            let mut sorted = records.to_vec();
            sorted.sort_by(|a, b| sort_spec.compare(a, b));
            IceToolResult::Records(sorted)
        }
        IceToolOp::Copy => {
            IceToolResult::Records(records.to_vec())
        }
        IceToolOp::Count => {
            IceToolResult::RecordCount(records.len())
        }
        IceToolOp::Display { columns, header } => {
            execute_display(records, columns, header.as_deref())
        }
        IceToolOp::Occur { on_field } => {
            execute_occur(records, on_field)
        }
        IceToolOp::Stats { on_field } => {
            execute_stats(records, on_field)
        }
        IceToolOp::Unique { on_field } => {
            execute_unique(records, on_field)
        }
        IceToolOp::Select { on_field, min_count, max_count } => {
            execute_select(records, on_field, *min_count, *max_count)
        }
    }
}

/// Execute DISPLAY operator — produce formatted report.
fn execute_display(
    records: &[Vec<u8>],
    columns: &[DisplayColumn],
    header: Option<&str>,
) -> IceToolResult {
    let mut lines = Vec::new();

    // Header
    if let Some(h) = header {
        lines.push(h.to_string());
        lines.push("-".repeat(h.len().max(40)));
    }

    // Column headers
    let col_headers: Vec<String> = columns.iter().map(|c| {
        format!("{:width$}", c.name, width = c.length.max(c.name.len()))
    }).collect();
    lines.push(col_headers.join("  "));

    // Separator
    let sep: Vec<String> = columns.iter().map(|c| {
        "-".repeat(c.length.max(c.name.len()))
    }).collect();
    lines.push(sep.join("  "));

    // Data rows
    for record in records {
        let mut row_parts = Vec::new();
        for col in columns {
            let start = col.position.saturating_sub(1);
            let end = (start + col.length).min(record.len());
            let value = if start < record.len() {
                &record[start..end]
            } else {
                b""
            };

            let formatted = if let Some(ref mask) = col.edit_mask {
                let num = crate::fields::extract_numeric(value, col.data_type);
                crate::reformat::apply_edit_mask_pub(num, mask)
            } else {
                let s = String::from_utf8_lossy(value).to_string();
                format!("{:width$}", s, width = col.length.max(col.name.len()))
            };
            row_parts.push(formatted);
        }
        lines.push(row_parts.join("  "));
    }

    IceToolResult::Report(lines)
}

/// Execute OCCUR operator — count occurrences of each unique value.
fn execute_occur(records: &[Vec<u8>], on_field: &OnField) -> IceToolResult {
    let mut counts: Vec<(Vec<u8>, usize)> = Vec::new();

    for record in records {
        if let Some(value) = on_field.extract(record) {
            let val = value.to_vec();
            if let Some(entry) = counts.iter_mut().find(|(k, _)| k == &val) {
                entry.1 += 1;
            } else {
                counts.push((val, 1));
            }
        }
    }

    // Sort by value for consistent output
    counts.sort_by(|a, b| a.0.cmp(&b.0));

    IceToolResult::Occurrences(counts)
}

/// Execute STATS operator — compute min, max, sum, count.
fn execute_stats(records: &[Vec<u8>], on_field: &OnField) -> IceToolResult {
    let mut min = i64::MAX;
    let mut max = i64::MIN;
    let mut sum: i64 = 0;
    let mut count: usize = 0;

    for record in records {
        if let Some(value) = on_field.extract(record) {
            let num = crate::fields::extract_numeric(value, on_field.data_type);
            if num < min {
                min = num;
            }
            if num > max {
                max = num;
            }
            sum = sum.saturating_add(num);
            count += 1;
        }
    }

    if count == 0 {
        min = 0;
        max = 0;
    }

    IceToolResult::Statistics(FieldStats { min, max, sum, count })
}

/// Execute UNIQUE operator — find distinct values.
fn execute_unique(records: &[Vec<u8>], on_field: &OnField) -> IceToolResult {
    let mut seen: Vec<Vec<u8>> = Vec::new();

    for record in records {
        if let Some(value) = on_field.extract(record) {
            let val = value.to_vec();
            if !seen.contains(&val) {
                seen.push(val);
            }
        }
    }

    seen.sort();
    IceToolResult::UniqueValues(seen)
}

/// Execute SELECT operator — select records by occurrence count.
fn execute_select(
    records: &[Vec<u8>],
    on_field: &OnField,
    min_count: Option<usize>,
    max_count: Option<usize>,
) -> IceToolResult {
    // First, count occurrences
    let mut counts: Vec<(Vec<u8>, usize)> = Vec::new();
    for record in records {
        if let Some(value) = on_field.extract(record) {
            let val = value.to_vec();
            if let Some(entry) = counts.iter_mut().find(|(k, _)| k == &val) {
                entry.1 += 1;
            } else {
                counts.push((val, 1));
            }
        }
    }

    // Determine which values pass the count filter
    let qualifying: Vec<Vec<u8>> = counts
        .into_iter()
        .filter(|(_, c)| {
            let above_min = min_count.map_or(true, |m| *c >= m);
            let below_max = max_count.map_or(true, |m| *c <= m);
            above_min && below_max
        })
        .map(|(v, _)| v)
        .collect();

    // Select records whose field value qualifies
    let result: Vec<Vec<u8>> = records
        .iter()
        .filter(|record| {
            if let Some(value) = on_field.extract(record) {
                qualifying.iter().any(|q| q.as_slice() == value)
            } else {
                false
            }
        })
        .cloned()
        .collect();

    IceToolResult::Selected(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fields::{SortField, SortOrder};

    #[test]
    fn test_sort_operator() {
        let records = vec![b"C".to_vec(), b"A".to_vec(), b"B".to_vec()];
        let spec = SortSpec::new()
            .add_field(SortField::new(1, 1, DataType::Character, SortOrder::Ascending));

        let result = execute(&IceToolOp::Sort { sort_spec: spec }, &records);
        match result {
            IceToolResult::Records(sorted) => {
                assert_eq!(sorted, vec![b"A".to_vec(), b"B".to_vec(), b"C".to_vec()]);
            }
            _ => panic!("Expected Records"),
        }
    }

    #[test]
    fn test_copy_operator() {
        let records = vec![b"X".to_vec(), b"Y".to_vec()];
        let result = execute(&IceToolOp::Copy, &records);
        match result {
            IceToolResult::Records(copied) => {
                assert_eq!(copied.len(), 2);
                assert_eq!(copied[0], b"X");
            }
            _ => panic!("Expected Records"),
        }
    }

    #[test]
    fn test_count_operator() {
        let records = vec![b"A".to_vec(), b"B".to_vec(), b"C".to_vec()];
        let result = execute(&IceToolOp::Count, &records);
        match result {
            IceToolResult::RecordCount(n) => assert_eq!(n, 3),
            _ => panic!("Expected RecordCount"),
        }
    }

    #[test]
    fn test_occur_operator() {
        let records = vec![
            b"NY".to_vec(),
            b"CA".to_vec(),
            b"NY".to_vec(),
            b"TX".to_vec(),
            b"NY".to_vec(),
        ];

        let result = execute(
            &IceToolOp::Occur {
                on_field: OnField::new(1, 2, DataType::Character),
            },
            &records,
        );

        match result {
            IceToolResult::Occurrences(occ) => {
                assert_eq!(occ.len(), 3);
                // Sorted by value
                assert_eq!(occ[0], (b"CA".to_vec(), 1));
                assert_eq!(occ[1], (b"NY".to_vec(), 3));
                assert_eq!(occ[2], (b"TX".to_vec(), 1));
            }
            _ => panic!("Expected Occurrences"),
        }
    }

    #[test]
    fn test_stats_operator_binary() {
        // Records with 4-byte binary values at position 1
        let records: Vec<Vec<u8>> = vec![
            10i32.to_be_bytes().to_vec(),
            20i32.to_be_bytes().to_vec(),
            30i32.to_be_bytes().to_vec(),
            40i32.to_be_bytes().to_vec(),
        ];

        let result = execute(
            &IceToolOp::Stats {
                on_field: OnField::new(1, 4, DataType::Binary),
            },
            &records,
        );

        match result {
            IceToolResult::Statistics(stats) => {
                assert_eq!(stats.min, 10);
                assert_eq!(stats.max, 40);
                assert_eq!(stats.sum, 100);
                assert_eq!(stats.count, 4);
                assert_eq!(stats.average(), 25);
            }
            _ => panic!("Expected Statistics"),
        }
    }

    #[test]
    fn test_stats_empty() {
        let result = execute(
            &IceToolOp::Stats {
                on_field: OnField::new(1, 4, DataType::Binary),
            },
            &[],
        );

        match result {
            IceToolResult::Statistics(stats) => {
                assert_eq!(stats.min, 0);
                assert_eq!(stats.max, 0);
                assert_eq!(stats.count, 0);
                assert_eq!(stats.average(), 0);
            }
            _ => panic!("Expected Statistics"),
        }
    }

    #[test]
    fn test_unique_operator() {
        let records = vec![
            b"NY".to_vec(),
            b"CA".to_vec(),
            b"NY".to_vec(),
            b"TX".to_vec(),
            b"CA".to_vec(),
        ];

        let result = execute(
            &IceToolOp::Unique {
                on_field: OnField::new(1, 2, DataType::Character),
            },
            &records,
        );

        match result {
            IceToolResult::UniqueValues(vals) => {
                assert_eq!(vals.len(), 3);
                assert_eq!(vals[0], b"CA");
                assert_eq!(vals[1], b"NY");
                assert_eq!(vals[2], b"TX");
            }
            _ => panic!("Expected UniqueValues"),
        }
    }

    #[test]
    fn test_select_min_count() {
        let records = vec![
            b"NY100".to_vec(),
            b"CA200".to_vec(),
            b"NY300".to_vec(),
            b"TX400".to_vec(),
            b"NY500".to_vec(),
        ];

        let result = execute(
            &IceToolOp::Select {
                on_field: OnField::new(1, 2, DataType::Character),
                min_count: Some(2),
                max_count: None,
            },
            &records,
        );

        match result {
            IceToolResult::Selected(selected) => {
                // Only NY records (count=3 >= 2)
                assert_eq!(selected.len(), 3);
                assert!(selected.iter().all(|r| &r[0..2] == b"NY"));
            }
            _ => panic!("Expected Selected"),
        }
    }

    #[test]
    fn test_select_max_count() {
        let records = vec![
            b"NY".to_vec(),
            b"CA".to_vec(),
            b"NY".to_vec(),
            b"TX".to_vec(),
        ];

        let result = execute(
            &IceToolOp::Select {
                on_field: OnField::new(1, 2, DataType::Character),
                min_count: None,
                max_count: Some(1),
            },
            &records,
        );

        match result {
            IceToolResult::Selected(selected) => {
                // Only CA and TX (count=1 each)
                assert_eq!(selected.len(), 2);
            }
            _ => panic!("Expected Selected"),
        }
    }

    #[test]
    fn test_display_report() {
        let records = vec![
            b"JOHNDOE".to_vec(),
            b"JANEROE".to_vec(),
        ];

        let result = execute(
            &IceToolOp::Display {
                columns: vec![
                    DisplayColumn {
                        name: "FIRST".to_string(),
                        position: 1,
                        length: 4,
                        data_type: DataType::Character,
                        edit_mask: None,
                    },
                    DisplayColumn {
                        name: "LAST".to_string(),
                        position: 5,
                        length: 3,
                        data_type: DataType::Character,
                        edit_mask: None,
                    },
                ],
                header: Some("Name Report".to_string()),
            },
            &records,
        );

        match result {
            IceToolResult::Report(lines) => {
                assert!(lines.len() >= 4); // header, separator, col headers, col sep, 2 data rows
                assert_eq!(lines[0], "Name Report");
                assert!(lines[2].contains("FIRST"));
                assert!(lines[2].contains("LAST"));
            }
            _ => panic!("Expected Report"),
        }
    }
}
