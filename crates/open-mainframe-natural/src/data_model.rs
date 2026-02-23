// SPDX-License-Identifier: Apache-2.0
//! NAT-101: Data Model & DEFINE DATA for Natural.
//!
//! Implements the 11 Natural data types (A, B, C, D, F, I, L, N, P, T, U),
//! variable declarations with levels, array support (1–3 dimensions),
//! dynamic variables, and group hierarchies.

use std::collections::HashMap;
use std::fmt;

// ---------------------------------------------------------------------------
// Data Types
// ---------------------------------------------------------------------------

/// Natural data-type codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NaturalType {
    /// Alpha (fixed-length string)
    Alpha,
    /// Binary
    Binary,
    /// Attribute control
    Control,
    /// Date (internal integer YYYYMMDD)
    Date,
    /// Floating point
    Float,
    /// Integer (I1/I2/I4)
    Integer,
    /// Logical (TRUE/FALSE)
    Logical,
    /// Numeric unpacked
    Numeric,
    /// Packed decimal
    Packed,
    /// Time (internal integer)
    Time,
    /// Unicode (fixed-length)
    Unicode,
}

impl fmt::Display for NaturalType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let code = match self {
            NaturalType::Alpha => "A",
            NaturalType::Binary => "B",
            NaturalType::Control => "C",
            NaturalType::Date => "D",
            NaturalType::Float => "F",
            NaturalType::Integer => "I",
            NaturalType::Logical => "L",
            NaturalType::Numeric => "N",
            NaturalType::Packed => "P",
            NaturalType::Time => "T",
            NaturalType::Unicode => "U",
        };
        write!(f, "{code}")
    }
}

/// Parse a type code letter into a `NaturalType`.
pub fn parse_type_code(ch: char) -> Option<NaturalType> {
    match ch.to_ascii_uppercase() {
        'A' => Some(NaturalType::Alpha),
        'B' => Some(NaturalType::Binary),
        'C' => Some(NaturalType::Control),
        'D' => Some(NaturalType::Date),
        'F' => Some(NaturalType::Float),
        'I' => Some(NaturalType::Integer),
        'L' => Some(NaturalType::Logical),
        'N' => Some(NaturalType::Numeric),
        'P' => Some(NaturalType::Packed),
        'T' => Some(NaturalType::Time),
        'U' => Some(NaturalType::Unicode),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// NaturalValue
// ---------------------------------------------------------------------------

/// Boxed value for each Natural data type.
#[derive(Debug, Clone, PartialEq)]
pub enum NaturalValue {
    Alpha(String),
    Binary(Vec<u8>),
    Control(u8),
    Date(i32),      // YYYYMMDD
    Float(f64),
    Integer(i64),
    Logical(bool),
    Numeric(String), // unpacked BCD as string repr
    Packed(String),  // packed BCD as string repr
    Time(i64),       // seconds since midnight
    Unicode(String),
    /// Null / not set.
    Null,
}

impl NaturalValue {
    /// Return the type code of this value.
    pub fn type_code(&self) -> Option<NaturalType> {
        match self {
            NaturalValue::Alpha(_) => Some(NaturalType::Alpha),
            NaturalValue::Binary(_) => Some(NaturalType::Binary),
            NaturalValue::Control(_) => Some(NaturalType::Control),
            NaturalValue::Date(_) => Some(NaturalType::Date),
            NaturalValue::Float(_) => Some(NaturalType::Float),
            NaturalValue::Integer(_) => Some(NaturalType::Integer),
            NaturalValue::Logical(_) => Some(NaturalType::Logical),
            NaturalValue::Numeric(_) => Some(NaturalType::Numeric),
            NaturalValue::Packed(_) => Some(NaturalType::Packed),
            NaturalValue::Time(_) => Some(NaturalType::Time),
            NaturalValue::Unicode(_) => Some(NaturalType::Unicode),
            NaturalValue::Null => None,
        }
    }

    /// Convert to display string.
    pub fn to_display_string(&self) -> String {
        match self {
            NaturalValue::Alpha(s) | NaturalValue::Unicode(s) => s.clone(),
            NaturalValue::Binary(b) => format!("{b:X?}"),
            NaturalValue::Control(c) => format!("{c}"),
            NaturalValue::Date(d) => format!("{d}"),
            NaturalValue::Float(f) => format!("{f}"),
            NaturalValue::Integer(i) => format!("{i}"),
            NaturalValue::Logical(b) => if *b { "TRUE".into() } else { "FALSE".into() },
            NaturalValue::Numeric(s) | NaturalValue::Packed(s) => s.clone(),
            NaturalValue::Time(t) => format!("{t}"),
            NaturalValue::Null => String::new(),
        }
    }

    /// Try to interpret value as f64 for arithmetic.
    pub fn to_f64(&self) -> f64 {
        match self {
            NaturalValue::Integer(i) => *i as f64,
            NaturalValue::Float(f) => *f,
            NaturalValue::Numeric(s) | NaturalValue::Packed(s) => s.parse::<f64>().unwrap_or(0.0),
            NaturalValue::Alpha(s) | NaturalValue::Unicode(s) => s.trim().parse::<f64>().unwrap_or(0.0),
            NaturalValue::Date(d) => *d as f64,
            NaturalValue::Time(t) => *t as f64,
            NaturalValue::Logical(b) => if *b { 1.0 } else { 0.0 },
            _ => 0.0,
        }
    }

    /// Try to interpret value as i64.
    pub fn to_i64(&self) -> i64 {
        match self {
            NaturalValue::Integer(i) => *i,
            NaturalValue::Float(f) => *f as i64,
            NaturalValue::Numeric(s) | NaturalValue::Packed(s) => s.parse::<i64>().unwrap_or(0),
            NaturalValue::Alpha(s) | NaturalValue::Unicode(s) => s.trim().parse::<i64>().unwrap_or(0),
            NaturalValue::Date(d) => *d as i64,
            NaturalValue::Time(t) => *t,
            NaturalValue::Logical(b) => if *b { 1 } else { 0 },
            _ => 0,
        }
    }

    /// Get string content.
    pub fn as_str(&self) -> &str {
        match self {
            NaturalValue::Alpha(s) | NaturalValue::Unicode(s) => s.as_str(),
            NaturalValue::Numeric(s) | NaturalValue::Packed(s) => s.as_str(),
            _ => "",
        }
    }

    /// Default value for a given type.
    pub fn default_for(ty: NaturalType) -> Self {
        match ty {
            NaturalType::Alpha => NaturalValue::Alpha(String::new()),
            NaturalType::Binary => NaturalValue::Binary(Vec::new()),
            NaturalType::Control => NaturalValue::Control(0),
            NaturalType::Date => NaturalValue::Date(0),
            NaturalType::Float => NaturalValue::Float(0.0),
            NaturalType::Integer => NaturalValue::Integer(0),
            NaturalType::Logical => NaturalValue::Logical(false),
            NaturalType::Numeric => NaturalValue::Numeric("0".into()),
            NaturalType::Packed => NaturalValue::Packed("0".into()),
            NaturalType::Time => NaturalValue::Time(0),
            NaturalType::Unicode => NaturalValue::Unicode(String::new()),
        }
    }
}

impl fmt::Display for NaturalValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_display_string())
    }
}

// ---------------------------------------------------------------------------
// Type Specification
// ---------------------------------------------------------------------------

/// Parsed type specification such as A10, P7.2, I4.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeSpec {
    pub base_type: NaturalType,
    pub length: u32,
    pub precision: u32,
    pub is_dynamic: bool, // A DYNAMIC, U DYNAMIC
}

impl TypeSpec {
    pub fn parse(spec: &str) -> Option<Self> {
        let spec = spec.trim();
        if spec.is_empty() {
            return None;
        }
        let first_char = spec.chars().next()?;
        let base_type = parse_type_code(first_char)?;
        let rest = &spec[1..];

        // Check for DYNAMIC
        let is_dynamic = rest.contains("DYNAMIC") || rest.contains("dynamic");
        let numeric_part = rest
            .replace("DYNAMIC", "")
            .replace("dynamic", "")
            .trim()
            .to_string();

        if numeric_part.is_empty() {
            let default_len = match base_type {
                NaturalType::Integer => 4,
                NaturalType::Float => 8,
                NaturalType::Date | NaturalType::Time => 4,
                NaturalType::Logical => 1,
                _ => 1,
            };
            return Some(TypeSpec { base_type, length: default_len, precision: 0, is_dynamic });
        }

        if let Some((len_str, prec_str)) = numeric_part.split_once('.') {
            let length = len_str.parse().unwrap_or(1);
            let precision = prec_str.parse().unwrap_or(0);
            Some(TypeSpec { base_type, length, precision, is_dynamic })
        } else {
            let length = numeric_part.parse().unwrap_or(1);
            Some(TypeSpec { base_type, length, precision: 0, is_dynamic })
        }
    }
}

// ---------------------------------------------------------------------------
// Variable definition
// ---------------------------------------------------------------------------

/// A single variable in the variable pool.
#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub level: u8,
    pub type_spec: Option<TypeSpec>,
    pub array_dims: Vec<ArrayDim>,
    pub value: NaturalValue,
    /// For arrays: flat storage keyed by index tuple.
    pub array_values: HashMap<Vec<i32>, NaturalValue>,
    /// Whether this is a group (has subordinate fields).
    pub is_group: bool,
    /// Names of children in group hierarchy.
    pub children: Vec<String>,
}

/// Array dimension bounds.
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayDim {
    pub lower: i32,
    pub upper: i32, // -1 = unbounded (*)
}

impl Variable {
    /// Create a new scalar variable.
    pub fn new_scalar(name: &str, level: u8, type_spec: Option<TypeSpec>) -> Self {
        let value = type_spec
            .as_ref()
            .map(|ts| NaturalValue::default_for(ts.base_type))
            .unwrap_or(NaturalValue::Null);
        Self {
            name: name.to_string(),
            level,
            type_spec,
            array_dims: Vec::new(),
            value,
            array_values: HashMap::new(),
            is_group: false,
            children: Vec::new(),
        }
    }

    /// Create a new array variable.
    pub fn new_array(name: &str, level: u8, type_spec: Option<TypeSpec>, dims: Vec<ArrayDim>) -> Self {
        let value = type_spec
            .as_ref()
            .map(|ts| NaturalValue::default_for(ts.base_type))
            .unwrap_or(NaturalValue::Null);
        Self {
            name: name.to_string(),
            level,
            type_spec,
            array_dims: dims,
            value,
            array_values: HashMap::new(),
            is_group: false,
            children: Vec::new(),
        }
    }

    /// Create a group variable.
    pub fn new_group(name: &str, level: u8) -> Self {
        Self {
            name: name.to_string(),
            level,
            type_spec: None,
            array_dims: Vec::new(),
            value: NaturalValue::Null,
            array_values: HashMap::new(),
            is_group: true,
            children: Vec::new(),
        }
    }

    /// Is this an array?
    pub fn is_array(&self) -> bool {
        !self.array_dims.is_empty()
    }

    /// Is this a dynamic variable?
    pub fn is_dynamic(&self) -> bool {
        self.type_spec.as_ref().is_some_and(|ts| ts.is_dynamic)
    }

    /// Get array element.
    pub fn get_element(&self, indices: &[i32]) -> Result<&NaturalValue, DataModelError> {
        self.check_bounds(indices)?;
        Ok(self.array_values.get(indices).unwrap_or(&self.value))
    }

    /// Set array element.
    pub fn set_element(&mut self, indices: &[i32], value: NaturalValue) -> Result<(), DataModelError> {
        self.check_bounds(indices)?;
        self.array_values.insert(indices.to_vec(), value);
        Ok(())
    }

    fn check_bounds(&self, indices: &[i32]) -> Result<(), DataModelError> {
        if indices.len() != self.array_dims.len() {
            return Err(DataModelError::DimensionMismatch {
                expected: self.array_dims.len(),
                got: indices.len(),
            });
        }
        for (i, (idx, dim)) in indices.iter().zip(self.array_dims.iter()).enumerate() {
            if *idx < dim.lower || (dim.upper >= 0 && *idx > dim.upper) {
                return Err(DataModelError::IndexOutOfBounds {
                    dimension: i,
                    index: *idx,
                    lower: dim.lower,
                    upper: dim.upper,
                });
            }
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Variable Pool
// ---------------------------------------------------------------------------

/// Collection of variables for a program execution.
#[derive(Debug, Clone, Default)]
pub struct VariablePool {
    vars: HashMap<String, Variable>,
}

impl VariablePool {
    pub fn new() -> Self {
        Self { vars: HashMap::new() }
    }

    pub fn define(&mut self, var: Variable) {
        self.vars.insert(var.name.clone(), var);
    }

    pub fn get(&self, name: &str) -> Option<&Variable> {
        self.vars.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Variable> {
        self.vars.get_mut(name)
    }

    pub fn get_value(&self, name: &str) -> NaturalValue {
        self.vars.get(name).map(|v| v.value.clone()).unwrap_or(NaturalValue::Null)
    }

    pub fn set_value(&mut self, name: &str, value: NaturalValue) {
        if let Some(var) = self.vars.get_mut(name) {
            // Dynamic variables grow automatically
            if var.is_dynamic() {
                var.value = value;
            } else {
                // Truncate alpha to fixed length
                if let (NaturalValue::Alpha(ref s), Some(ts)) = (&value, &var.type_spec) {
                    if ts.base_type == NaturalType::Alpha && !ts.is_dynamic {
                        let truncated: String = s.chars().take(ts.length as usize).collect();
                        var.value = NaturalValue::Alpha(truncated);
                        return;
                    }
                }
                var.value = value;
            }
        } else {
            // Auto-define variable (Natural allows implicit #-variable creation)
            let mut var = Variable::new_scalar(name, 1, None);
            var.value = value;
            self.vars.insert(name.to_string(), var);
        }
    }

    pub fn names(&self) -> Vec<String> {
        self.vars.keys().cloned().collect()
    }

    /// Move by name: copy all fields with matching names from source group.
    pub fn move_by_name(&mut self, source_group: &str, target_group: &str) {
        let source_children: Vec<String> = self.vars.get(source_group)
            .map(|g| g.children.clone())
            .unwrap_or_default();
        let target_children: Vec<String> = self.vars.get(target_group)
            .map(|g| g.children.clone())
            .unwrap_or_default();

        for src_name in &source_children {
            // Strip group prefix for matching
            let field_name = src_name.rsplit('.').next().unwrap_or(src_name);
            for tgt_name in &target_children {
                let tgt_field = tgt_name.rsplit('.').next().unwrap_or(tgt_name);
                if field_name == tgt_field {
                    if let Some(val) = self.vars.get(src_name).map(|v| v.value.clone()) {
                        if let Some(tgt) = self.vars.get_mut(tgt_name) {
                            tgt.value = val;
                        }
                    }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// DEFINE DATA parser (processes AST VarDecl nodes)
// ---------------------------------------------------------------------------

/// Parse DEFINE DATA sections from AST variable declarations.
pub fn build_variable_pool(sections: &[crate::parser::DataSection]) -> Result<VariablePool, DataModelError> {
    let mut pool = VariablePool::new();
    let mut group_stack: Vec<(u8, String)> = Vec::new();

    for section in sections {
        for decl in &section.variables {
            // Pop groups with level >= this one
            while let Some((lvl, _)) = group_stack.last() {
                if *lvl >= decl.level {
                    group_stack.pop();
                } else {
                    break;
                }
            }

            let ts = decl.data_type.as_ref().and_then(|s| TypeSpec::parse(s));
            let dims: Vec<ArrayDim> = decl.array_dims.iter().map(|(l, u)| ArrayDim { lower: *l, upper: *u }).collect();

            let var = if ts.is_none() && dims.is_empty() && decl.data_type.is_none() {
                // Group variable
                let mut g = Variable::new_group(&decl.name, decl.level);
                // Register as child of parent group
                if let Some((_, parent_name)) = group_stack.last() {
                    if let Some(parent) = pool.get_mut(parent_name) {
                        parent.children.push(decl.name.clone());
                    }
                }
                group_stack.push((decl.level, decl.name.clone()));
                g.level = decl.level;
                g
            } else if dims.is_empty() {
                let mut v = Variable::new_scalar(&decl.name, decl.level, ts);
                // Register as child
                if let Some((_, parent_name)) = group_stack.last() {
                    if let Some(parent) = pool.get_mut(parent_name) {
                        parent.children.push(decl.name.clone());
                    }
                }
                v.level = decl.level;
                v
            } else {
                let mut v = Variable::new_array(&decl.name, decl.level, ts, dims);
                if let Some((_, parent_name)) = group_stack.last() {
                    if let Some(parent) = pool.get_mut(parent_name) {
                        parent.children.push(decl.name.clone());
                    }
                }
                v.level = decl.level;
                v
            };

            pool.define(var);
        }
    }

    Ok(pool)
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, thiserror::Error)]
pub enum DataModelError {
    #[error("dimension mismatch: expected {expected}, got {got}")]
    DimensionMismatch { expected: usize, got: usize },

    #[error("index out of bounds in dimension {dimension}: index {index} not in [{lower}..{upper}]")]
    IndexOutOfBounds { dimension: usize, index: i32, lower: i32, upper: i32 },

    #[error("unknown type code: {0}")]
    UnknownType(String),

    #[error("variable not found: {0}")]
    VariableNotFound(String),
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_type_alpha() {
        let ts = TypeSpec::parse("A10").unwrap();
        assert_eq!(ts.base_type, NaturalType::Alpha);
        assert_eq!(ts.length, 10);
        assert_eq!(ts.precision, 0);
        assert!(!ts.is_dynamic);
    }

    #[test]
    fn test_parse_type_packed() {
        let ts = TypeSpec::parse("P7.2").unwrap();
        assert_eq!(ts.base_type, NaturalType::Packed);
        assert_eq!(ts.length, 7);
        assert_eq!(ts.precision, 2);
    }

    #[test]
    fn test_parse_type_integer() {
        let ts = TypeSpec::parse("I4").unwrap();
        assert_eq!(ts.base_type, NaturalType::Integer);
        assert_eq!(ts.length, 4);
    }

    #[test]
    fn test_parse_type_float() {
        let ts = TypeSpec::parse("F8").unwrap();
        assert_eq!(ts.base_type, NaturalType::Float);
        assert_eq!(ts.length, 8);
    }

    #[test]
    fn test_parse_type_dynamic() {
        let ts = TypeSpec::parse("ADYNAMIC").unwrap();
        assert_eq!(ts.base_type, NaturalType::Alpha);
        assert!(ts.is_dynamic);
    }

    #[test]
    fn test_all_type_codes() {
        for (ch, expected) in [
            ('A', NaturalType::Alpha), ('B', NaturalType::Binary),
            ('C', NaturalType::Control), ('D', NaturalType::Date),
            ('F', NaturalType::Float), ('I', NaturalType::Integer),
            ('L', NaturalType::Logical), ('N', NaturalType::Numeric),
            ('P', NaturalType::Packed), ('T', NaturalType::Time),
            ('U', NaturalType::Unicode),
        ] {
            assert_eq!(parse_type_code(ch).unwrap(), expected);
        }
        assert!(parse_type_code('Z').is_none());
    }

    #[test]
    fn test_natural_value_defaults() {
        let v = NaturalValue::default_for(NaturalType::Alpha);
        assert_eq!(v, NaturalValue::Alpha(String::new()));

        let v = NaturalValue::default_for(NaturalType::Integer);
        assert_eq!(v, NaturalValue::Integer(0));

        let v = NaturalValue::default_for(NaturalType::Logical);
        assert_eq!(v, NaturalValue::Logical(false));
    }

    #[test]
    fn test_natural_value_conversions() {
        assert_eq!(NaturalValue::Integer(42).to_f64(), 42.0);
        assert_eq!(NaturalValue::Float(3.14).to_i64(), 3);
        assert_eq!(NaturalValue::Packed("100".into()).to_f64(), 100.0);
        assert_eq!(NaturalValue::Alpha("hello".into()).as_str(), "hello");
    }

    #[test]
    fn test_natural_value_display() {
        assert_eq!(NaturalValue::Alpha("test".into()).to_display_string(), "test");
        assert_eq!(NaturalValue::Integer(42).to_display_string(), "42");
        assert_eq!(NaturalValue::Logical(true).to_display_string(), "TRUE");
        assert_eq!(NaturalValue::Null.to_display_string(), "");
    }

    #[test]
    fn test_variable_scalar() {
        let v = Variable::new_scalar("#NAME", 1, Some(TypeSpec::parse("A20").unwrap()));
        assert!(!v.is_array());
        assert!(!v.is_dynamic());
        assert_eq!(v.value, NaturalValue::Alpha(String::new()));
    }

    #[test]
    fn test_variable_array_1d() {
        let mut v = Variable::new_array(
            "#SCORES", 1,
            Some(TypeSpec::parse("I4").unwrap()),
            vec![ArrayDim { lower: 1, upper: 10 }],
        );
        assert!(v.is_array());
        v.set_element(&[1], NaturalValue::Integer(100)).unwrap();
        v.set_element(&[10], NaturalValue::Integer(200)).unwrap();
        assert_eq!(v.get_element(&[1]).unwrap(), &NaturalValue::Integer(100));
        assert_eq!(v.get_element(&[10]).unwrap(), &NaturalValue::Integer(200));
    }

    #[test]
    fn test_variable_array_bounds_check() {
        let v = Variable::new_array(
            "#ARR", 1,
            Some(TypeSpec::parse("I4").unwrap()),
            vec![ArrayDim { lower: 1, upper: 5 }],
        );
        assert!(v.get_element(&[0]).is_err());
        assert!(v.get_element(&[6]).is_err());
        assert!(v.get_element(&[1, 1]).is_err()); // wrong dims
    }

    #[test]
    fn test_variable_array_2d() {
        let mut v = Variable::new_array(
            "#MATRIX", 1,
            Some(TypeSpec::parse("I4").unwrap()),
            vec![
                ArrayDim { lower: 1, upper: 3 },
                ArrayDim { lower: 1, upper: 3 },
            ],
        );
        v.set_element(&[2, 3], NaturalValue::Integer(99)).unwrap();
        assert_eq!(v.get_element(&[2, 3]).unwrap(), &NaturalValue::Integer(99));
    }

    #[test]
    fn test_variable_array_3d() {
        let mut v = Variable::new_array(
            "#CUBE", 1,
            Some(TypeSpec::parse("I4").unwrap()),
            vec![
                ArrayDim { lower: 1, upper: 2 },
                ArrayDim { lower: 1, upper: 2 },
                ArrayDim { lower: 1, upper: 2 },
            ],
        );
        v.set_element(&[1, 2, 1], NaturalValue::Integer(7)).unwrap();
        assert_eq!(v.get_element(&[1, 2, 1]).unwrap(), &NaturalValue::Integer(7));
    }

    #[test]
    fn test_dynamic_variable() {
        let ts = TypeSpec { base_type: NaturalType::Alpha, length: 0, precision: 0, is_dynamic: true };
        let v = Variable::new_scalar("#DYN", 1, Some(ts));
        assert!(v.is_dynamic());
    }

    #[test]
    fn test_variable_pool_basic() {
        let mut pool = VariablePool::new();
        pool.define(Variable::new_scalar("#X", 1, Some(TypeSpec::parse("I4").unwrap())));
        pool.set_value("#X", NaturalValue::Integer(42));
        assert_eq!(pool.get_value("#X"), NaturalValue::Integer(42));
    }

    #[test]
    fn test_variable_pool_alpha_truncation() {
        let mut pool = VariablePool::new();
        pool.define(Variable::new_scalar("#S", 1, Some(TypeSpec::parse("A5").unwrap())));
        pool.set_value("#S", NaturalValue::Alpha("ABCDEFGH".into()));
        assert_eq!(pool.get_value("#S"), NaturalValue::Alpha("ABCDE".into()));
    }

    #[test]
    fn test_group_variable() {
        let mut g = Variable::new_group("#PERSON", 1);
        assert!(g.is_group);
        g.children.push("#PERSON.NAME".to_string());
        g.children.push("#PERSON.AGE".to_string());
        assert_eq!(g.children.len(), 2);
    }

    #[test]
    fn test_move_by_name() {
        let mut pool = VariablePool::new();
        let mut src_group = Variable::new_group("SRC", 1);
        src_group.children.push("SRC.NAME".into());
        src_group.children.push("SRC.AGE".into());
        pool.define(src_group);
        pool.define(Variable::new_scalar("SRC.NAME", 2, Some(TypeSpec::parse("A20").unwrap())));
        pool.define(Variable::new_scalar("SRC.AGE", 2, Some(TypeSpec::parse("I4").unwrap())));

        let mut tgt_group = Variable::new_group("TGT", 1);
        tgt_group.children.push("TGT.NAME".into());
        tgt_group.children.push("TGT.AGE".into());
        pool.define(tgt_group);
        pool.define(Variable::new_scalar("TGT.NAME", 2, Some(TypeSpec::parse("A20").unwrap())));
        pool.define(Variable::new_scalar("TGT.AGE", 2, Some(TypeSpec::parse("I4").unwrap())));

        pool.set_value("SRC.NAME", NaturalValue::Alpha("Alice".into()));
        pool.set_value("SRC.AGE", NaturalValue::Integer(30));

        pool.move_by_name("SRC", "TGT");

        assert_eq!(pool.get_value("TGT.NAME"), NaturalValue::Alpha("Alice".into()));
        assert_eq!(pool.get_value("TGT.AGE"), NaturalValue::Integer(30));
    }

    #[test]
    fn test_type_spec_display() {
        assert_eq!(format!("{}", NaturalType::Alpha), "A");
        assert_eq!(format!("{}", NaturalType::Packed), "P");
        assert_eq!(format!("{}", NaturalType::Integer), "I");
    }

    #[test]
    fn test_value_type_code() {
        assert_eq!(NaturalValue::Alpha("x".into()).type_code(), Some(NaturalType::Alpha));
        assert_eq!(NaturalValue::Integer(0).type_code(), Some(NaturalType::Integer));
        assert_eq!(NaturalValue::Null.type_code(), None);
    }

    #[test]
    fn test_parse_type_numeric() {
        let ts = TypeSpec::parse("N5.2").unwrap();
        assert_eq!(ts.base_type, NaturalType::Numeric);
        assert_eq!(ts.length, 5);
        assert_eq!(ts.precision, 2);
    }

    #[test]
    fn test_parse_type_logical() {
        let ts = TypeSpec::parse("L").unwrap();
        assert_eq!(ts.base_type, NaturalType::Logical);
        assert_eq!(ts.length, 1);
    }

    #[test]
    fn test_parse_type_date() {
        let ts = TypeSpec::parse("D").unwrap();
        assert_eq!(ts.base_type, NaturalType::Date);
    }

    #[test]
    fn test_parse_type_time() {
        let ts = TypeSpec::parse("T").unwrap();
        assert_eq!(ts.base_type, NaturalType::Time);
    }

    #[test]
    fn test_parse_type_unicode() {
        let ts = TypeSpec::parse("U20").unwrap();
        assert_eq!(ts.base_type, NaturalType::Unicode);
        assert_eq!(ts.length, 20);
    }

    #[test]
    fn test_parse_type_binary() {
        let ts = TypeSpec::parse("B4").unwrap();
        assert_eq!(ts.base_type, NaturalType::Binary);
        assert_eq!(ts.length, 4);
    }

    #[test]
    fn test_parse_type_control() {
        let ts = TypeSpec::parse("C").unwrap();
        assert_eq!(ts.base_type, NaturalType::Control);
    }
}
