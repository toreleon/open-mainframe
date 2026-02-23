// SPDX-License-Identifier: Apache-2.0
//! NAT-103: Data Manipulation statements for Natural.
//!
//! Provides decimal arithmetic (COMPUTE), string operations (COMPRESS,
//! SEPARATE, EXAMINE), data movement (MOVE, MOVE BY NAME, MOVE EDITED),
//! and in-memory SORT.

use crate::data_model::NaturalValue;

// ---------------------------------------------------------------------------
// Decimal arithmetic helpers
// ---------------------------------------------------------------------------

/// Evaluate a decimal arithmetic expression as f64.
/// Supports +, -, *, / with standard precedence.
pub fn compute_arithmetic(left: f64, op: char, right: f64) -> Result<f64, ManipulationError> {
    match op {
        '+' => Ok(left + right),
        '-' => Ok(left - right),
        '*' => Ok(left * right),
        '/' => {
            if right == 0.0 {
                Err(ManipulationError::DivisionByZero)
            } else {
                Ok(left / right)
            }
        }
        _ => Err(ManipulationError::InvalidOperator(op)),
    }
}

/// Round a decimal to a given number of decimal places.
pub fn round_decimal(value: f64, places: u32) -> f64 {
    let factor = 10_f64.powi(places as i32);
    (value * factor).round() / factor
}

// ---------------------------------------------------------------------------
// MOVE operations
// ---------------------------------------------------------------------------

/// Move a value, performing basic type conversion.
pub fn move_value(source: &NaturalValue, target_type: Option<char>) -> NaturalValue {
    match target_type {
        Some('A') | Some('U') => NaturalValue::Alpha(source.to_display_string()),
        Some('I') => NaturalValue::Integer(source.to_i64()),
        Some('F') => NaturalValue::Float(source.to_f64()),
        Some('P') | Some('N') => NaturalValue::Packed(format!("{}", source.to_f64())),
        Some('L') => {
            let b = match source {
                NaturalValue::Logical(b) => *b,
                NaturalValue::Alpha(s) => s == "TRUE" || s == "Y",
                NaturalValue::Integer(i) => *i != 0,
                _ => false,
            };
            NaturalValue::Logical(b)
        }
        _ => source.clone(),
    }
}

/// MOVE EDITED: apply an edit mask to a numeric value.
pub fn move_edited(source: &NaturalValue, mask: &str) -> String {
    let num_str = source.to_display_string();
    let digits: Vec<char> = num_str.chars().filter(|c| c.is_ascii_digit()).collect();

    // Count digit positions in mask for right-alignment
    let mask_chars: Vec<char> = mask.chars().collect();
    let digit_positions = mask_chars.iter().filter(|c| **c == '9' || **c == 'Z').count();
    let pad_count = digit_positions.saturating_sub(digits.len());

    let mut result = String::new();
    let mut d_idx = 0;
    let mut pos_idx = 0; // counts digit positions seen

    for ch in &mask_chars {
        match ch {
            '9' | 'Z' => {
                if pos_idx < pad_count {
                    // Padding position
                    if *ch == '9' {
                        result.push('0');
                    } else {
                        result.push(' ');
                    }
                } else if d_idx < digits.len() {
                    result.push(digits[d_idx]);
                    d_idx += 1;
                } else if *ch == '9' {
                    result.push('0');
                } else {
                    result.push(' ');
                }
                pos_idx += 1;
            }
            _ => result.push(*ch),
        }
    }
    result
}

// ---------------------------------------------------------------------------
// COMPRESS
// ---------------------------------------------------------------------------

/// Compress multiple string values into one, optionally leaving spaces.
pub fn compress(sources: &[String], leaving_space: bool) -> String {
    let trimmed: Vec<&str> = sources.iter().map(|s| s.trim_end()).collect();
    if leaving_space {
        trimmed.join(" ")
    } else {
        trimmed.concat()
    }
}

// ---------------------------------------------------------------------------
// SEPARATE
// ---------------------------------------------------------------------------

/// Separate a string into parts by a delimiter.
pub fn separate(source: &str, delimiter: &str, max_parts: usize) -> Vec<String> {
    let parts: Vec<&str> = source.splitn(max_parts, delimiter).collect();
    parts.into_iter().map(|s| s.to_string()).collect()
}

// ---------------------------------------------------------------------------
// EXAMINE
// ---------------------------------------------------------------------------

/// Result of an EXAMINE operation.
#[derive(Debug, Clone)]
pub struct ExamineResult {
    pub modified: String,
    pub count: usize,
    pub position: Option<usize>,
    pub length: Option<usize>,
}

/// Examine a string: scan for pattern, optionally replace, return count/position/length.
pub fn examine(source: &str, pattern: &str, replace_with: Option<&str>) -> ExamineResult {
    let count = source.matches(pattern).count();
    let position = source.find(pattern);
    let length = position.map(|_| pattern.len());

    let modified = if let Some(repl) = replace_with {
        source.replace(pattern, repl)
    } else {
        source.to_string()
    };

    ExamineResult {
        modified,
        count,
        position,
        length,
    }
}

/// EXAMINE with FULL option — exact match only.
pub fn examine_full(source: &str, pattern: &str) -> bool {
    source == pattern
}

// ---------------------------------------------------------------------------
// SORT
// ---------------------------------------------------------------------------

/// Sort a vector of records (represented as key-value maps) by the given fields.
pub fn sort_records(
    records: &mut [Vec<(String, NaturalValue)>],
    sort_keys: &[(String, SortDirection)],
) {
    records.sort_by(|a, b| {
        for (field, dir) in sort_keys {
            let av = a.iter().find(|(k, _)| k == field).map(|(_, v)| v);
            let bv = b.iter().find(|(k, _)| k == field).map(|(_, v)| v);
            let cmp = match (av, bv) {
                (Some(NaturalValue::Alpha(sa)), Some(NaturalValue::Alpha(sb))) => sa.cmp(sb),
                (Some(NaturalValue::Integer(ia)), Some(NaturalValue::Integer(ib))) => ia.cmp(ib),
                (Some(a_val), Some(b_val)) => a_val.to_f64().partial_cmp(&b_val.to_f64()).unwrap_or(std::cmp::Ordering::Equal),
                _ => std::cmp::Ordering::Equal,
            };
            let cmp = match dir {
                SortDirection::Ascending => cmp,
                SortDirection::Descending => cmp.reverse(),
            };
            if cmp != std::cmp::Ordering::Equal {
                return cmp;
            }
        }
        std::cmp::Ordering::Equal
    });
}

/// Sort direction.
#[derive(Debug, Clone, PartialEq)]
pub enum SortDirection {
    Ascending,
    Descending,
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, thiserror::Error)]
pub enum ManipulationError {
    #[error("division by zero")]
    DivisionByZero,
    #[error("invalid operator: {0}")]
    InvalidOperator(char),
    #[error("type conversion error: {0}")]
    TypeConversion(String),
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compute_add() {
        assert_eq!(compute_arithmetic(10.0, '+', 5.0).unwrap(), 15.0);
    }

    #[test]
    fn test_compute_subtract() {
        assert_eq!(compute_arithmetic(10.0, '-', 3.0).unwrap(), 7.0);
    }

    #[test]
    fn test_compute_multiply() {
        assert_eq!(compute_arithmetic(4.0, '*', 2.5).unwrap(), 10.0);
    }

    #[test]
    fn test_compute_divide() {
        assert_eq!(compute_arithmetic(10.0, '/', 4.0).unwrap(), 2.5);
    }

    #[test]
    fn test_compute_divide_by_zero() {
        assert!(compute_arithmetic(10.0, '/', 0.0).is_err());
    }

    #[test]
    fn test_round_decimal() {
        assert_eq!(round_decimal(3.14159, 2), 3.14);
        assert_eq!(round_decimal(3.145, 2), 3.15);
        assert_eq!(round_decimal(3.14159, 0), 3.0);
    }

    #[test]
    fn test_move_value_to_alpha() {
        let v = move_value(&NaturalValue::Integer(42), Some('A'));
        assert_eq!(v.to_display_string(), "42");
    }

    #[test]
    fn test_move_value_to_integer() {
        let v = move_value(&NaturalValue::Alpha("123".into()), Some('I'));
        assert_eq!(v.to_i64(), 123);
    }

    #[test]
    fn test_move_value_to_float() {
        let v = move_value(&NaturalValue::Integer(42), Some('F'));
        assert_eq!(v.to_f64(), 42.0);
    }

    #[test]
    fn test_move_value_to_logical() {
        let v = move_value(&NaturalValue::Alpha("TRUE".into()), Some('L'));
        assert_eq!(v, NaturalValue::Logical(true));

        let v = move_value(&NaturalValue::Alpha("FALSE".into()), Some('L'));
        assert_eq!(v, NaturalValue::Logical(false));
    }

    #[test]
    fn test_move_edited() {
        let result = move_edited(&NaturalValue::Integer(12345), "99,999");
        assert_eq!(result, "12,345");
    }

    #[test]
    fn test_move_edited_z_mask() {
        let result = move_edited(&NaturalValue::Integer(42), "ZZZ9");
        assert_eq!(result, "  42");
    }

    #[test]
    fn test_compress_leaving_space() {
        let result = compress(&["John".into(), "Doe".into()], true);
        assert_eq!(result, "John Doe");
    }

    #[test]
    fn test_compress_no_space() {
        let result = compress(&["AB".into(), "CD".into()], false);
        assert_eq!(result, "ABCD");
    }

    #[test]
    fn test_compress_trims_trailing() {
        let result = compress(&["Hello  ".into(), "World ".into()], true);
        assert_eq!(result, "Hello World");
    }

    #[test]
    fn test_separate_comma() {
        let parts = separate("a,b,c", ",", 10);
        assert_eq!(parts, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_separate_space() {
        let parts = separate("John Doe Smith", " ", 3);
        assert_eq!(parts, vec!["John", "Doe", "Smith"]);
    }

    #[test]
    fn test_separate_max_parts() {
        let parts = separate("a,b,c,d", ",", 2);
        assert_eq!(parts, vec!["a", "b,c,d"]);
    }

    #[test]
    fn test_examine_count() {
        let result = examine("ABCABC", "A", None);
        assert_eq!(result.count, 2);
        assert_eq!(result.position, Some(0));
    }

    #[test]
    fn test_examine_replace() {
        let result = examine("hello world", "world", Some("earth"));
        assert_eq!(result.modified, "hello earth");
        assert_eq!(result.count, 1);
    }

    #[test]
    fn test_examine_no_match() {
        let result = examine("hello", "xyz", None);
        assert_eq!(result.count, 0);
        assert!(result.position.is_none());
    }

    #[test]
    fn test_examine_multiple_replace() {
        let result = examine("aXbXc", "X", Some("Y"));
        assert_eq!(result.modified, "aYbYc");
        assert_eq!(result.count, 2);
    }

    #[test]
    fn test_examine_full_match() {
        assert!(examine_full("HELLO", "HELLO"));
        assert!(!examine_full("HELLO", "HELL"));
    }

    #[test]
    fn test_sort_ascending() {
        let mut records = vec![
            vec![("NAME".into(), NaturalValue::Alpha("Charlie".into()))],
            vec![("NAME".into(), NaturalValue::Alpha("Alice".into()))],
            vec![("NAME".into(), NaturalValue::Alpha("Bob".into()))],
        ];
        sort_records(&mut records, &[("NAME".into(), SortDirection::Ascending)]);
        assert_eq!(records[0][0].1.to_display_string(), "Alice");
        assert_eq!(records[1][0].1.to_display_string(), "Bob");
        assert_eq!(records[2][0].1.to_display_string(), "Charlie");
    }

    #[test]
    fn test_sort_descending() {
        let mut records = vec![
            vec![("AGE".into(), NaturalValue::Integer(25))],
            vec![("AGE".into(), NaturalValue::Integer(30))],
            vec![("AGE".into(), NaturalValue::Integer(20))],
        ];
        sort_records(&mut records, &[("AGE".into(), SortDirection::Descending)]);
        assert_eq!(records[0][0].1.to_i64(), 30);
        assert_eq!(records[1][0].1.to_i64(), 25);
        assert_eq!(records[2][0].1.to_i64(), 20);
    }

    #[test]
    fn test_sort_multiple_keys() {
        let mut records = vec![
            vec![("DEPT".into(), NaturalValue::Alpha("B".into())), ("NAME".into(), NaturalValue::Alpha("Zoe".into()))],
            vec![("DEPT".into(), NaturalValue::Alpha("A".into())), ("NAME".into(), NaturalValue::Alpha("Bob".into()))],
            vec![("DEPT".into(), NaturalValue::Alpha("A".into())), ("NAME".into(), NaturalValue::Alpha("Ada".into()))],
        ];
        sort_records(&mut records, &[
            ("DEPT".into(), SortDirection::Ascending),
            ("NAME".into(), SortDirection::Ascending),
        ]);
        assert_eq!(records[0][1].1.to_display_string(), "Ada");
        assert_eq!(records[1][1].1.to_display_string(), "Bob");
        assert_eq!(records[2][1].1.to_display_string(), "Zoe");
    }

    #[test]
    fn test_invalid_operator() {
        assert!(compute_arithmetic(1.0, '%', 2.0).is_err());
    }
}
