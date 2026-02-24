//! INCLUDE/OMIT record filtering.

use crate::fields::DataType;

use open_mainframe_encoding::decimal::{
    unpack_to_i64,
    unzone_to_i64,
};

/// Type of filter operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FilterType {
    /// Include matching records.
    Include,
    /// Omit (exclude) matching records.
    Omit,
}

/// Comparison operators for conditions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompareOp {
    /// Equal.
    Eq,
    /// Not equal.
    Ne,
    /// Greater than.
    Gt,
    /// Greater than or equal.
    Ge,
    /// Less than.
    Lt,
    /// Less than or equal.
    Le,
}

impl CompareOp {
    /// Parse comparison operator from DFSORT code.
    pub fn from_code(code: &str) -> Option<Self> {
        match code.to_uppercase().as_str() {
            "EQ" => Some(CompareOp::Eq),
            "NE" => Some(CompareOp::Ne),
            "GT" => Some(CompareOp::Gt),
            "GE" => Some(CompareOp::Ge),
            "LT" => Some(CompareOp::Lt),
            "LE" => Some(CompareOp::Le),
            _ => None,
        }
    }

    /// Returns the DFSORT code.
    pub fn code(&self) -> &'static str {
        match self {
            CompareOp::Eq => "EQ",
            CompareOp::Ne => "NE",
            CompareOp::Gt => "GT",
            CompareOp::Ge => "GE",
            CompareOp::Lt => "LT",
            CompareOp::Le => "LE",
        }
    }
}

/// Boolean logic for combining conditions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BooleanLogic {
    /// All conditions must match (AND).
    And,
    /// Any condition must match (OR).
    Or,
}

/// A single filter condition.
#[derive(Debug, Clone)]
pub struct Condition {
    /// Starting position (1-based).
    pub position: usize,
    /// Length in bytes.
    pub length: usize,
    /// Data type for comparison.
    pub data_type: DataType,
    /// Comparison operator.
    pub op: CompareOp,
    /// Value to compare against.
    pub value: Vec<u8>,
}

impl Condition {
    /// Evaluates the condition against a record.
    pub fn evaluate(&self, record: &[u8]) -> bool {
        let start = self.position.saturating_sub(1);
        let end = start + self.length;

        if end > record.len() {
            return false;
        }

        let field = &record[start..end];
        let cmp = compare_values(field, &self.value, self.data_type);

        match self.op {
            CompareOp::Eq => cmp == std::cmp::Ordering::Equal,
            CompareOp::Ne => cmp != std::cmp::Ordering::Equal,
            CompareOp::Gt => cmp == std::cmp::Ordering::Greater,
            CompareOp::Ge => cmp != std::cmp::Ordering::Less,
            CompareOp::Lt => cmp == std::cmp::Ordering::Less,
            CompareOp::Le => cmp != std::cmp::Ordering::Greater,
        }
    }
}

/// Compare two values according to data type.
fn compare_values(a: &[u8], b: &[u8], data_type: DataType) -> std::cmp::Ordering {
    match data_type {
        DataType::Character => a.cmp(b),
        DataType::ZonedDecimal => {
            let a_val = parse_zoned_decimal(a);
            let b_val = parse_zoned_decimal(b);
            a_val.cmp(&b_val)
        }
        DataType::PackedDecimal => {
            let a_val = parse_packed_decimal(a);
            let b_val = parse_packed_decimal(b);
            a_val.cmp(&b_val)
        }
        DataType::Binary | DataType::FixedPoint => {
            let a_val = parse_binary(a);
            let b_val = parse_binary(b);
            a_val.cmp(&b_val)
        }
    }
}

/// Parse zoned decimal.
fn parse_zoned_decimal(data: &[u8]) -> i64 {
    unzone_to_i64(data)
}

/// Parse packed decimal.
fn parse_packed_decimal(data: &[u8]) -> i64 {
    unpack_to_i64(data)
}

/// Parse binary.
fn parse_binary(data: &[u8]) -> i64 {
    if data.is_empty() {
        return 0;
    }

    let negative = data[0] & 0x80 != 0;
    let mut value: i64 = 0;

    for &byte in data {
        value = (value << 8) | (byte as i64);
    }

    if negative {
        let bits = data.len() * 8;
        let mask = !((1i64 << bits) - 1);
        value |= mask;
    }

    value
}

/// Complete filter specification.
#[derive(Debug, Clone)]
pub struct FilterSpec {
    /// Filter type (include or omit).
    pub filter_type: FilterType,
    /// Conditions to evaluate.
    pub conditions: Vec<Condition>,
    /// Logic for combining multiple conditions.
    pub logic: Option<BooleanLogic>,
}

impl FilterSpec {
    /// Evaluates the filter against a record.
    /// Returns true if the record should be included in output.
    pub fn should_include(&self, record: &[u8]) -> bool {
        let match_result = if self.conditions.len() == 1 {
            self.conditions[0].evaluate(record)
        } else {
            match self.logic.unwrap_or(BooleanLogic::And) {
                BooleanLogic::And => self.conditions.iter().all(|c| c.evaluate(record)),
                BooleanLogic::Or => self.conditions.iter().any(|c| c.evaluate(record)),
            }
        };

        match self.filter_type {
            FilterType::Include => match_result,
            FilterType::Omit => !match_result,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compare_op_from_code() {
        assert_eq!(CompareOp::from_code("EQ"), Some(CompareOp::Eq));
        assert_eq!(CompareOp::from_code("eq"), Some(CompareOp::Eq));
        assert_eq!(CompareOp::from_code("NE"), Some(CompareOp::Ne));
        assert_eq!(CompareOp::from_code("GT"), Some(CompareOp::Gt));
        assert_eq!(CompareOp::from_code("XX"), None);
    }

    #[test]
    fn test_condition_eq() {
        let cond = Condition {
            position: 1,
            length: 2,
            data_type: DataType::Character,
            op: CompareOp::Eq,
            value: b"NY".to_vec(),
        };

        assert!(cond.evaluate(b"NY12345"));
        assert!(!cond.evaluate(b"CA12345"));
    }

    #[test]
    fn test_condition_gt() {
        let cond = Condition {
            position: 1,
            length: 2,
            data_type: DataType::Character,
            op: CompareOp::Gt,
            value: b"CA".to_vec(),
        };

        assert!(cond.evaluate(b"NY12345"));
        assert!(!cond.evaluate(b"CA12345"));
        assert!(!cond.evaluate(b"AA12345"));
    }

    #[test]
    fn test_filter_include() {
        let filter = FilterSpec {
            filter_type: FilterType::Include,
            conditions: vec![Condition {
                position: 1,
                length: 2,
                data_type: DataType::Character,
                op: CompareOp::Eq,
                value: b"NY".to_vec(),
            }],
            logic: None,
        };

        assert!(filter.should_include(b"NY12345"));
        assert!(!filter.should_include(b"CA12345"));
    }

    #[test]
    fn test_filter_omit() {
        let filter = FilterSpec {
            filter_type: FilterType::Omit,
            conditions: vec![Condition {
                position: 1,
                length: 1,
                data_type: DataType::Character,
                op: CompareOp::Eq,
                value: b"X".to_vec(),
            }],
            logic: None,
        };

        assert!(!filter.should_include(b"X12345"));
        assert!(filter.should_include(b"A12345"));
    }

    #[test]
    fn test_filter_multiple_and() {
        let filter = FilterSpec {
            filter_type: FilterType::Include,
            conditions: vec![
                Condition {
                    position: 1,
                    length: 1,
                    data_type: DataType::Character,
                    op: CompareOp::Eq,
                    value: b"A".to_vec(),
                },
                Condition {
                    position: 2,
                    length: 1,
                    data_type: DataType::Character,
                    op: CompareOp::Eq,
                    value: b"B".to_vec(),
                },
            ],
            logic: Some(BooleanLogic::And),
        };

        assert!(filter.should_include(b"AB123"));
        assert!(!filter.should_include(b"AC123"));
        assert!(!filter.should_include(b"BB123"));
    }

    #[test]
    fn test_filter_multiple_or() {
        let filter = FilterSpec {
            filter_type: FilterType::Include,
            conditions: vec![
                Condition {
                    position: 1,
                    length: 2,
                    data_type: DataType::Character,
                    op: CompareOp::Eq,
                    value: b"NY".to_vec(),
                },
                Condition {
                    position: 1,
                    length: 2,
                    data_type: DataType::Character,
                    op: CompareOp::Eq,
                    value: b"CA".to_vec(),
                },
            ],
            logic: Some(BooleanLogic::Or),
        };

        assert!(filter.should_include(b"NY123"));
        assert!(filter.should_include(b"CA123"));
        assert!(!filter.should_include(b"TX123"));
    }
}
