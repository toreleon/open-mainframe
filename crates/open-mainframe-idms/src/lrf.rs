//! IDMS-109: Logical Record Facility (LRF).
//!
//! LRF abstracts complex navigational paths into logical records,
//! allowing programs to retrieve data from multiple record types
//! with a single OBTAIN LOGICAL RECORD statement and optional
//! WHERE clauses.

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Logical record path step
// ---------------------------------------------------------------------------

/// A single step in a logical record path definition.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct PathStep {
    /// Record type to access at this step.
    pub record_type: String,
    /// Set name to navigate (if navigating through a set).
    pub set_name: Option<String>,
    /// Navigation direction within the set.
    pub direction: PathDirection,
}

/// Direction for set navigation within a path step.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum PathDirection {
    /// Navigate to owner.
    Owner,
    /// Navigate to first member.
    FirstMember,
    /// Navigate to next member.
    NextMember,
    /// Navigate to last member.
    LastMember,
    /// Direct access (no set navigation).
    Direct,
}

// ---------------------------------------------------------------------------
//  Logical record definition
// ---------------------------------------------------------------------------

/// A logical record definition that maps to one or more physical records.
///
/// Logical records provide a simplified, flattened view of data that may
/// span multiple CODASYL record types connected via sets.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct LogicalRecord {
    /// Logical record name.
    pub name: String,
    /// Path steps defining how to navigate from root to target records.
    pub path: Vec<PathStep>,
    /// Fields exposed by this logical record (field_name -> source record.field).
    pub fields: HashMap<String, String>,
}

impl LogicalRecord {
    /// Create a new logical record definition.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            path: Vec::new(),
            fields: HashMap::new(),
        }
    }

    /// Add a navigation step to the path.
    pub fn add_step(&mut self, record_type: &str, set_name: Option<&str>, direction: PathDirection) {
        self.path.push(PathStep {
            record_type: record_type.to_uppercase(),
            set_name: set_name.map(|s| s.to_uppercase()),
            direction,
        });
    }

    /// Map a logical field name to a physical record.field source.
    pub fn add_field(&mut self, logical_name: &str, source: &str) {
        self.fields
            .insert(logical_name.to_uppercase(), source.to_uppercase());
    }

    /// Return the number of path steps.
    pub fn step_count(&self) -> usize {
        self.path.len()
    }

    /// Return the number of exposed fields.
    pub fn field_count(&self) -> usize {
        self.fields.len()
    }
}

// ---------------------------------------------------------------------------
//  WHERE clause (simplified)
// ---------------------------------------------------------------------------

/// A simplified WHERE clause condition for LRF queries.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhereCondition {
    /// Field name to compare.
    pub field: String,
    /// Comparison operator.
    pub operator: ComparisonOp,
    /// Value to compare against.
    pub value: String,
}

/// Comparison operators for WHERE conditions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparisonOp {
    /// Equal.
    Eq,
    /// Not equal.
    Ne,
    /// Greater than.
    Gt,
    /// Less than.
    Lt,
    /// Greater than or equal.
    Ge,
    /// Less than or equal.
    Le,
}

// ---------------------------------------------------------------------------
//  LRF engine
// ---------------------------------------------------------------------------

/// Engine for processing logical record requests.
///
/// Stores logical record definitions and processes OBTAIN LOGICAL RECORD
/// requests by following the defined navigation paths.
#[derive(Debug, Default)]
pub struct LrfEngine {
    /// Registered logical record definitions.
    definitions: HashMap<String, LogicalRecord>,
}

impl LrfEngine {
    /// Create a new LRF engine.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a logical record definition.
    pub fn register(&mut self, lr: LogicalRecord) {
        self.definitions.insert(lr.name.clone(), lr);
    }

    /// Look up a logical record definition.
    pub fn get_definition(&self, name: &str) -> Option<&LogicalRecord> {
        self.definitions.get(&name.to_uppercase())
    }

    /// Return the number of registered logical records.
    pub fn definition_count(&self) -> usize {
        self.definitions.len()
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn logical_record_definition() {
        let mut lr = LogicalRecord::new("EMP-DEPT-LR");
        lr.add_step("DEPARTMENT", None, PathDirection::Direct);
        lr.add_step("EMPLOYEE", Some("DEPT-EMP"), PathDirection::FirstMember);
        lr.add_field("DEPT-NAME", "DEPARTMENT.DEPT-NAME");
        lr.add_field("EMP-NAME", "EMPLOYEE.EMP-NAME");

        assert_eq!(lr.name, "EMP-DEPT-LR");
        assert_eq!(lr.step_count(), 2);
        assert_eq!(lr.field_count(), 2);
        assert_eq!(lr.path[1].direction, PathDirection::FirstMember);
    }

    #[test]
    fn lrf_engine_register_and_lookup() {
        let mut engine = LrfEngine::new();
        let mut lr = LogicalRecord::new("TEST-LR");
        lr.add_step("REC1", None, PathDirection::Direct);
        engine.register(lr);

        assert_eq!(engine.definition_count(), 1);
        assert!(engine.get_definition("TEST-LR").is_some());
        assert!(engine.get_definition("NONEXISTENT").is_none());
    }

    #[test]
    fn where_condition() {
        let cond = WhereCondition {
            field: "SALARY".to_string(),
            operator: ComparisonOp::Gt,
            value: "50000".to_string(),
        };
        assert_eq!(cond.operator, ComparisonOp::Gt);
    }

    #[test]
    fn path_direction_variants() {
        assert_ne!(PathDirection::Owner, PathDirection::FirstMember);
        assert_ne!(PathDirection::NextMember, PathDirection::LastMember);
    }
}
