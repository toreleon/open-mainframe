//! Parser for DFSORT control statements.

use crate::error::SortError;
use crate::fields::{DataType, SortField, SortOrder, SortSpec};
use crate::filter::{BooleanLogic, CompareOp, Condition, FilterSpec, FilterType};
use crate::reformat::{OutrecField, OutrecSpec};

/// Parsed control statements.
#[derive(Debug, Default)]
pub struct ControlStatements {
    /// SORT specification.
    pub sort: Option<SortSpec>,
    /// INCLUDE filter.
    pub include: Option<FilterSpec>,
    /// OMIT filter.
    pub omit: Option<FilterSpec>,
    /// INREC specification.
    pub inrec: Option<OutrecSpec>,
    /// OUTREC specification.
    pub outrec: Option<OutrecSpec>,
    /// SUM fields (positions).
    pub sum_fields: Option<Vec<(usize, usize, DataType)>>,
    /// OPTION COPY mode.
    pub copy_mode: bool,
}

/// Parse DFSORT control statements.
pub fn parse_control_statements(input: &str) -> Result<ControlStatements, SortError> {
    let mut result = ControlStatements::default();
    let mut current_line = 0;

    // Join continuation lines and process
    let normalized = normalize_continuations(input);

    for line in normalized.lines() {
        current_line += 1;
        let line = line.trim();

        // Skip empty lines and comments
        if line.is_empty() || line.starts_with('*') {
            continue;
        }

        // Parse statement
        if let Some(rest) = line.strip_prefix("SORT") {
            let rest = rest.trim();
            if let Some(fields_str) = rest.strip_prefix("FIELDS=") {
                result.sort = Some(parse_sort_fields(fields_str.trim(), current_line)?);
            }
        } else if let Some(rest) = line.strip_prefix("INCLUDE") {
            let rest = rest.trim();
            if let Some(cond_str) = rest.strip_prefix("COND=") {
                result.include = Some(parse_filter(cond_str.trim(), FilterType::Include, current_line)?);
            }
        } else if let Some(rest) = line.strip_prefix("OMIT") {
            let rest = rest.trim();
            if let Some(cond_str) = rest.strip_prefix("COND=") {
                result.omit = Some(parse_filter(cond_str.trim(), FilterType::Omit, current_line)?);
            }
        } else if let Some(rest) = line.strip_prefix("OUTREC") {
            let rest = rest.trim();
            if let Some(fields_str) = rest.strip_prefix("FIELDS=") {
                result.outrec = Some(parse_outrec_fields(fields_str.trim(), current_line)?);
            }
        } else if let Some(rest) = line.strip_prefix("INREC") {
            let rest = rest.trim();
            if let Some(fields_str) = rest.strip_prefix("FIELDS=") {
                result.inrec = Some(parse_outrec_fields(fields_str.trim(), current_line)?);
            }
        } else if let Some(rest) = line.strip_prefix("SUM") {
            let rest = rest.trim();
            if rest.starts_with("FIELDS=NONE") {
                result.sum_fields = Some(Vec::new()); // NONE = remove duplicates
            } else if let Some(fields_str) = rest.strip_prefix("FIELDS=") {
                result.sum_fields = Some(parse_sum_fields(fields_str.trim(), current_line)?);
            }
        } else if line.starts_with("OPTION") && line.contains("COPY") {
            result.copy_mode = true;
        }
    }

    Ok(result)
}

/// Check whether a trimmed line starts with a DFSORT statement keyword,
/// which means it is a new statement rather than a continuation.
fn is_statement_keyword(trimmed: &str) -> bool {
    let upper = trimmed.to_uppercase();
    upper.starts_with("SORT ")
        || upper.starts_with("INCLUDE ")
        || upper.starts_with("OMIT ")
        || upper.starts_with("INREC ")
        || upper.starts_with("OUTREC ")
        || upper.starts_with("SUM ")
        || upper.starts_with("OPTION ")
        || upper.starts_with("MERGE ")
}

/// Normalize continuation lines (lines starting with spaces are continuations).
///
/// In DFSORT control cards, lines starting with a space are normally
/// continuations of the previous statement. However, if the trimmed
/// content starts with a known statement keyword (SORT, INCLUDE, etc.)
/// it is a new statement, not a continuation.
fn normalize_continuations(input: &str) -> String {
    let mut result = String::new();
    let mut current_statement = String::new();

    for line in input.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('*') {
            // Blank / comment — flush current and pass through
            if !current_statement.is_empty() {
                result.push_str(&current_statement);
                result.push('\n');
                current_statement.clear();
            }
            result.push_str(trimmed);
            result.push('\n');
            continue;
        }

        let is_indented = line.starts_with(' ') || line.starts_with('\t');

        if is_indented && !is_statement_keyword(trimmed) {
            // Continuation — append to current statement
            current_statement.push_str(trimmed);
        } else {
            // New statement (either not indented, or starts with a keyword)
            if !current_statement.is_empty() {
                result.push_str(&current_statement);
                result.push('\n');
            }
            current_statement = trimmed.to_string();
        }
    }

    if !current_statement.is_empty() {
        result.push_str(&current_statement);
    }

    result
}

/// Parse SORT FIELDS specification.
fn parse_sort_fields(input: &str, line: usize) -> Result<SortSpec, SortError> {
    let input = input.trim_start_matches('(').trim_end_matches(')');
    let mut spec = SortSpec::new();

    // Split by commas, handling groups of 4 values
    let parts: Vec<&str> = input.split(',').map(|s| s.trim()).collect();

    let mut i = 0;
    while i + 3 < parts.len() {
        let position: usize = parts[i].parse().map_err(|_| SortError::ParseError {
            line,
            message: format!("Invalid position: {}", parts[i]),
        })?;

        let length: usize = parts[i + 1].parse().map_err(|_| SortError::ParseError {
            line,
            message: format!("Invalid length: {}", parts[i + 1]),
        })?;

        let data_type = DataType::from_code(parts[i + 2]).ok_or_else(|| SortError::ParseError {
            line,
            message: format!("Invalid data type: {}", parts[i + 2]),
        })?;

        let order = SortOrder::from_code(parts[i + 3]).ok_or_else(|| SortError::ParseError {
            line,
            message: format!("Invalid sort order: {}", parts[i + 3]),
        })?;

        spec = spec.add_field(SortField::new(position, length, data_type, order));
        i += 4;
    }

    if spec.fields.is_empty() {
        return Err(SortError::ParseError {
            line,
            message: "No valid sort fields found".to_string(),
        });
    }

    Ok(spec)
}

/// Parse INCLUDE/OMIT condition, supporting compound conditions with AND/OR.
///
/// Formats:
/// - Simple:   `(pos,len,type,op,value)`
/// - Compound: `(pos,len,type,op,value,AND,pos,len,type,op,value)`
fn parse_filter(input: &str, filter_type: FilterType, line: usize) -> Result<FilterSpec, SortError> {
    let input = input.trim_start_matches('(').trim_end_matches(')');

    // Split respecting quoted literals (C'...' may contain commas in theory)
    let parts: Vec<&str> = split_filter_parts(input);

    if parts.len() < 5 {
        return Err(SortError::ParseError {
            line,
            message: "Incomplete condition specification".to_string(),
        });
    }

    let mut conditions = Vec::new();
    let mut logic = None;
    let mut i = 0;

    while i + 4 < parts.len() {
        let position: usize = parts[i].parse().map_err(|_| SortError::ParseError {
            line,
            message: format!("Invalid position: {}", parts[i]),
        })?;

        let length: usize = parts[i + 1].parse().map_err(|_| SortError::ParseError {
            line,
            message: format!("Invalid length: {}", parts[i + 1]),
        })?;

        let data_type = DataType::from_code(parts[i + 2]).ok_or_else(|| SortError::ParseError {
            line,
            message: format!("Invalid data type: {}", parts[i + 2]),
        })?;

        let op = CompareOp::from_code(parts[i + 3]).ok_or_else(|| SortError::ParseError {
            line,
            message: format!("Invalid comparison operator: {}", parts[i + 3]),
        })?;

        let value = parse_condition_value(parts[i + 4], line)?;

        conditions.push(Condition {
            position,
            length,
            data_type,
            op,
            value,
        });
        i += 5;

        // Check for AND/OR between conditions
        if i < parts.len() {
            let connector = parts[i].to_uppercase();
            match connector.as_str() {
                "AND" => { logic = Some(BooleanLogic::And); i += 1; }
                "OR"  => { logic = Some(BooleanLogic::Or);  i += 1; }
                _ => break,
            }
        }
    }

    if conditions.is_empty() {
        return Err(SortError::ParseError {
            line,
            message: "No valid conditions found".to_string(),
        });
    }

    Ok(FilterSpec {
        filter_type,
        conditions,
        logic,
    })
}

/// Split filter condition parts by comma, respecting quoted literals.
fn split_filter_parts(input: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut start = 0;
    let mut in_quote = false;

    for (i, c) in input.char_indices() {
        match c {
            '\'' => in_quote = !in_quote,
            ',' if !in_quote => {
                parts.push(input[start..i].trim());
                start = i + 1;
            }
            _ => {}
        }
    }
    if start < input.len() {
        parts.push(input[start..].trim());
    }
    parts
}

/// Parse a condition value (C'literal' or numeric).
fn parse_condition_value(input: &str, line: usize) -> Result<Vec<u8>, SortError> {
    let input = input.trim();

    if let Some(lit) = input.strip_prefix("C'").and_then(|s| s.strip_suffix('\'')) {
        // Character literal
        Ok(lit.as_bytes().to_vec())
    } else if let Some(lit) = input.strip_prefix("X'").and_then(|s| s.strip_suffix('\'')) {
        // Hex literal
        parse_hex_literal(lit, line)
    } else {
        // Numeric - convert to bytes
        let num: i64 = input.parse().map_err(|_| SortError::ParseError {
            line,
            message: format!("Invalid value: {}", input),
        })?;
        Ok(num.to_be_bytes().to_vec())
    }
}

/// Parse hex literal.
fn parse_hex_literal(input: &str, line: usize) -> Result<Vec<u8>, SortError> {
    let mut result = Vec::new();
    let chars: Vec<char> = input.chars().collect();

    for i in (0..chars.len()).step_by(2) {
        let hex_str: String = chars[i..].iter().take(2).collect();
        let byte = u8::from_str_radix(&hex_str, 16).map_err(|_| SortError::ParseError {
            line,
            message: format!("Invalid hex: {}", hex_str),
        })?;
        result.push(byte);
    }

    Ok(result)
}

/// Parse OUTREC/INREC FIELDS specification.
fn parse_outrec_fields(input: &str, line: usize) -> Result<OutrecSpec, SortError> {
    let input = input.trim_start_matches('(').trim_end_matches(')');
    let mut spec = OutrecSpec::new();

    // Parse comma-separated fields
    // Format: pos,len or pos:len or 'literal' or numeric
    let mut current_pos = 1usize; // Output position tracking
    let parts: Vec<&str> = split_outrec_parts(input);

    let mut i = 0;
    while i < parts.len() {
        let part = parts[i].trim();

        if part.starts_with("C'") {
            // Literal
            if let Some(lit) = part.strip_prefix("C'").and_then(|s| s.strip_suffix('\'')) {
                spec.fields.push(OutrecField::Literal(lit.as_bytes().to_vec()));
                current_pos += lit.len();
            }
            i += 1;
        } else if part.contains(':') {
            // Output position: source position, length
            // e.g., "1:10,5" means output pos 1, source pos 10, len 5
            let col_parts: Vec<&str> = part.split(':').collect();
            if col_parts.len() >= 2 {
                let _out_pos: usize = col_parts[0].parse().unwrap_or(current_pos);
                let src_pos: usize = col_parts[1].parse().map_err(|_| SortError::ParseError {
                    line,
                    message: format!("Invalid position: {}", col_parts[1]),
                })?;

                // Get length from next part
                if i + 1 < parts.len() {
                    let len: usize = parts[i + 1].parse().map_err(|_| SortError::ParseError {
                        line,
                        message: format!("Invalid length: {}", parts[i + 1]),
                    })?;
                    spec.fields.push(OutrecField::Field { position: src_pos, length: len });
                    current_pos += len;
                    i += 2;
                    continue;
                }
            }
            i += 1;
        } else if let Ok(pos) = part.parse::<usize>() {
            // Position - get length from next part
            if i + 1 < parts.len() {
                let len: usize = parts[i + 1].parse().map_err(|_| SortError::ParseError {
                    line,
                    message: format!("Invalid length: {}", parts[i + 1]),
                })?;
                spec.fields.push(OutrecField::Field { position: pos, length: len });
                current_pos += len;
                i += 2;
            } else {
                i += 1;
            }
        } else {
            i += 1;
        }
    }

    Ok(spec)
}

/// Split OUTREC parts handling literals with commas.
fn split_outrec_parts(input: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut start = 0;
    let mut in_quote = false;

    for (i, c) in input.char_indices() {
        match c {
            '\'' => in_quote = !in_quote,
            ',' if !in_quote => {
                parts.push(&input[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }

    if start < input.len() {
        parts.push(&input[start..]);
    }

    parts
}

/// Parse SUM FIELDS specification.
fn parse_sum_fields(input: &str, line: usize) -> Result<Vec<(usize, usize, DataType)>, SortError> {
    let input = input.trim_start_matches('(').trim_end_matches(')');
    let mut fields = Vec::new();

    let parts: Vec<&str> = input.split(',').map(|s| s.trim()).collect();

    let mut i = 0;
    while i + 2 < parts.len() {
        let position: usize = parts[i].parse().map_err(|_| SortError::ParseError {
            line,
            message: format!("Invalid position: {}", parts[i]),
        })?;

        let length: usize = parts[i + 1].parse().map_err(|_| SortError::ParseError {
            line,
            message: format!("Invalid length: {}", parts[i + 1]),
        })?;

        let data_type = DataType::from_code(parts[i + 2]).ok_or_else(|| SortError::ParseError {
            line,
            message: format!("Invalid data type: {}", parts[i + 2]),
        })?;

        fields.push((position, length, data_type));
        i += 3;
    }

    Ok(fields)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_sort_fields() {
        let input = "SORT FIELDS=(1,10,CH,A,15,5,ZD,D)";
        let result = parse_control_statements(input).unwrap();

        let sort = result.sort.unwrap();
        assert_eq!(sort.fields.len(), 2);

        assert_eq!(sort.fields[0].position, 1);
        assert_eq!(sort.fields[0].length, 10);
        assert_eq!(sort.fields[0].data_type, DataType::Character);
        assert_eq!(sort.fields[0].order, SortOrder::Ascending);

        assert_eq!(sort.fields[1].position, 15);
        assert_eq!(sort.fields[1].length, 5);
        assert_eq!(sort.fields[1].data_type, DataType::ZonedDecimal);
        assert_eq!(sort.fields[1].order, SortOrder::Descending);
    }

    #[test]
    fn test_parse_include_condition() {
        let input = "INCLUDE COND=(10,2,CH,EQ,C'NY')";
        let result = parse_control_statements(input).unwrap();

        let include = result.include.unwrap();
        assert_eq!(include.conditions.len(), 1);

        let cond = &include.conditions[0];
        assert_eq!(cond.position, 10);
        assert_eq!(cond.length, 2);
        assert_eq!(cond.op, CompareOp::Eq);
        assert_eq!(cond.value, b"NY".to_vec());
    }

    #[test]
    fn test_parse_outrec() {
        let input = "OUTREC FIELDS=(1,10,25,15)";
        let result = parse_control_statements(input).unwrap();

        let outrec = result.outrec.unwrap();
        assert_eq!(outrec.fields.len(), 2);
    }

    #[test]
    fn test_parse_copy_mode() {
        let input = "OPTION COPY";
        let result = parse_control_statements(input).unwrap();
        assert!(result.copy_mode);
    }

    #[test]
    fn test_parse_sum_fields_none() {
        let input = "SUM FIELDS=NONE";
        let result = parse_control_statements(input).unwrap();
        assert!(result.sum_fields.is_some());
        assert!(result.sum_fields.unwrap().is_empty());
    }

    #[test]
    fn test_invalid_sort_fields() {
        let input = "SORT FIELDS=(1,10,XX,A)";
        let result = parse_control_statements(input);
        assert!(result.is_err());
    }

    /// Regression: multi-line control cards where both SORT and INCLUDE are
    /// indented (as in typical SYSIN inline data) must be parsed as separate
    /// statements, not concatenated into one.
    #[test]
    fn test_multiline_sort_and_include() {
        let input = " SORT FIELDS=(1,10,CH,A)\n INCLUDE COND=(20,2,CH,EQ,C'NY')\n";
        let result = parse_control_statements(input).unwrap();

        let sort = result.sort.unwrap();
        assert_eq!(sort.fields.len(), 1);
        assert_eq!(sort.fields[0].position, 1);
        assert_eq!(sort.fields[0].length, 10);
        assert_eq!(sort.fields[0].order, SortOrder::Ascending);

        let include = result.include.unwrap();
        assert_eq!(include.conditions.len(), 1);
        assert_eq!(include.conditions[0].position, 20);
        assert_eq!(include.conditions[0].length, 2);
        assert_eq!(include.conditions[0].op, CompareOp::Eq);
    }
}
