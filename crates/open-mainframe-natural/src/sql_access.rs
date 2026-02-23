// SPDX-License-Identifier: Apache-2.0
//! NAT-105: SQL Database Access for Natural.
//!
//! Provides simulated SQL operations: SELECT (single-row and cursor),
//! INSERT, UPDATE, DELETE, COMMIT, and ROLLBACK, with a pluggable
//! `SqlConnection` trait for abstraction.

use std::collections::HashMap;

use crate::data_model::NaturalValue;

// ---------------------------------------------------------------------------
// SqlConnection trait
// ---------------------------------------------------------------------------

/// Abstraction over a SQL database connection.
pub trait SqlConnection {
    /// Execute a SELECT returning at most one row.
    fn select_single(
        &self,
        table: &str,
        columns: &[String],
        condition: Option<&SqlCondition>,
    ) -> Result<Option<Vec<NaturalValue>>, SqlError>;

    /// Execute a SELECT returning multiple rows.
    fn select_cursor(
        &self,
        table: &str,
        columns: &[String],
        condition: Option<&SqlCondition>,
    ) -> Result<Vec<Vec<NaturalValue>>, SqlError>;

    /// Execute an INSERT.
    fn insert(
        &mut self,
        table: &str,
        columns: &[String],
        values: &[NaturalValue],
    ) -> Result<usize, SqlError>;

    /// Execute an UPDATE.
    fn update(
        &mut self,
        table: &str,
        set: &[(String, NaturalValue)],
        condition: Option<&SqlCondition>,
    ) -> Result<usize, SqlError>;

    /// Execute a DELETE.
    fn delete(
        &mut self,
        table: &str,
        condition: Option<&SqlCondition>,
    ) -> Result<usize, SqlError>;

    /// COMMIT the current transaction.
    fn commit(&mut self) -> Result<(), SqlError>;

    /// ROLLBACK the current transaction.
    fn rollback(&mut self) -> Result<(), SqlError>;
}

// ---------------------------------------------------------------------------
// SQL condition (simplified)
// ---------------------------------------------------------------------------

/// A simplified SQL condition for WHERE clauses.
#[derive(Debug, Clone)]
pub enum SqlCondition {
    Equals(String, NaturalValue),
    NotEquals(String, NaturalValue),
    LessThan(String, NaturalValue),
    GreaterThan(String, NaturalValue),
    And(Box<SqlCondition>, Box<SqlCondition>),
    Or(Box<SqlCondition>, Box<SqlCondition>),
}

impl SqlCondition {
    pub fn matches(&self, row: &HashMap<String, NaturalValue>) -> bool {
        match self {
            SqlCondition::Equals(col, val) => {
                row.get(col).map_or(false, |v| v.to_display_string() == val.to_display_string())
            }
            SqlCondition::NotEquals(col, val) => {
                row.get(col).map_or(true, |v| v.to_display_string() != val.to_display_string())
            }
            SqlCondition::LessThan(col, val) => {
                row.get(col).map_or(false, |v| v.to_f64() < val.to_f64())
            }
            SqlCondition::GreaterThan(col, val) => {
                row.get(col).map_or(false, |v| v.to_f64() > val.to_f64())
            }
            SqlCondition::And(a, b) => a.matches(row) && b.matches(row),
            SqlCondition::Or(a, b) => a.matches(row) || b.matches(row),
        }
    }
}

// ---------------------------------------------------------------------------
// In-memory SQL implementation
// ---------------------------------------------------------------------------

/// An in-memory table for SQL simulation.
#[derive(Debug, Clone)]
struct MemTable {
    #[allow(dead_code)]
    columns: Vec<String>,
    rows: Vec<HashMap<String, NaturalValue>>,
}

/// In-memory SQL connection for testing.
#[derive(Debug, Clone)]
pub struct InMemorySql {
    tables: HashMap<String, MemTable>,
    /// Snapshot for rollback
    snapshot: Option<HashMap<String, MemTable>>,
}

impl InMemorySql {
    pub fn new() -> Self {
        Self {
            tables: HashMap::new(),
            snapshot: None,
        }
    }

    /// Create a table with given column names.
    pub fn create_table(&mut self, name: &str, columns: &[&str]) {
        self.tables.insert(name.to_string(), MemTable {
            columns: columns.iter().map(|s| s.to_string()).collect(),
            rows: Vec::new(),
        });
    }

    /// Insert a row directly (bypasses transaction).
    pub fn insert_row(&mut self, table: &str, row: HashMap<String, NaturalValue>) {
        if let Some(tbl) = self.tables.get_mut(table) {
            tbl.rows.push(row);
        }
    }

    /// Get the number of rows in a table.
    pub fn row_count(&self, table: &str) -> usize {
        self.tables.get(table).map_or(0, |t| t.rows.len())
    }

    fn save_snapshot(&mut self) {
        if self.snapshot.is_none() {
            self.snapshot = Some(self.tables.clone());
        }
    }
}

impl Default for InMemorySql {
    fn default() -> Self {
        Self::new()
    }
}

impl SqlConnection for InMemorySql {
    fn select_single(
        &self,
        table: &str,
        columns: &[String],
        condition: Option<&SqlCondition>,
    ) -> Result<Option<Vec<NaturalValue>>, SqlError> {
        let tbl = self.tables.get(table).ok_or(SqlError::TableNotFound(table.to_string()))?;
        for row in &tbl.rows {
            if condition.as_ref().map_or(true, |c| c.matches(row)) {
                let vals: Vec<NaturalValue> = columns.iter()
                    .map(|c| row.get(c).cloned().unwrap_or(NaturalValue::Null))
                    .collect();
                return Ok(Some(vals));
            }
        }
        Ok(None)
    }

    fn select_cursor(
        &self,
        table: &str,
        columns: &[String],
        condition: Option<&SqlCondition>,
    ) -> Result<Vec<Vec<NaturalValue>>, SqlError> {
        let tbl = self.tables.get(table).ok_or(SqlError::TableNotFound(table.to_string()))?;
        let mut results = Vec::new();
        for row in &tbl.rows {
            if condition.as_ref().map_or(true, |c| c.matches(row)) {
                let vals: Vec<NaturalValue> = columns.iter()
                    .map(|c| row.get(c).cloned().unwrap_or(NaturalValue::Null))
                    .collect();
                results.push(vals);
            }
        }
        Ok(results)
    }

    fn insert(
        &mut self,
        table: &str,
        columns: &[String],
        values: &[NaturalValue],
    ) -> Result<usize, SqlError> {
        self.save_snapshot();
        let tbl = self.tables.get_mut(table).ok_or(SqlError::TableNotFound(table.to_string()))?;
        let mut row = HashMap::new();
        for (col, val) in columns.iter().zip(values.iter()) {
            row.insert(col.clone(), val.clone());
        }
        tbl.rows.push(row);
        Ok(1)
    }

    fn update(
        &mut self,
        table: &str,
        set: &[(String, NaturalValue)],
        condition: Option<&SqlCondition>,
    ) -> Result<usize, SqlError> {
        self.save_snapshot();
        let tbl = self.tables.get_mut(table).ok_or(SqlError::TableNotFound(table.to_string()))?;
        let mut count = 0;
        for row in &mut tbl.rows {
            if condition.as_ref().map_or(true, |c| c.matches(row)) {
                for (col, val) in set {
                    row.insert(col.clone(), val.clone());
                }
                count += 1;
            }
        }
        Ok(count)
    }

    fn delete(
        &mut self,
        table: &str,
        condition: Option<&SqlCondition>,
    ) -> Result<usize, SqlError> {
        self.save_snapshot();
        let tbl = self.tables.get_mut(table).ok_or(SqlError::TableNotFound(table.to_string()))?;
        let before = tbl.rows.len();
        tbl.rows.retain(|row| !condition.as_ref().map_or(true, |c| c.matches(row)));
        Ok(before - tbl.rows.len())
    }

    fn commit(&mut self) -> Result<(), SqlError> {
        self.snapshot = None;
        Ok(())
    }

    fn rollback(&mut self) -> Result<(), SqlError> {
        if let Some(snap) = self.snapshot.take() {
            self.tables = snap;
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, thiserror::Error)]
pub enum SqlError {
    #[error("table not found: {0}")]
    TableNotFound(String),
    #[error("column not found: {0}")]
    ColumnNotFound(String),
    #[error("SQL execution error: {0}")]
    ExecutionError(String),
    #[error("type mismatch in SQL: {0}")]
    TypeMismatch(String),
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn setup_db() -> InMemorySql {
        let mut db = InMemorySql::new();
        db.create_table("EMPLOYEES", &["ID", "NAME", "DEPT", "SALARY"]);

        let mut row1 = HashMap::new();
        row1.insert("ID".into(), NaturalValue::Integer(1));
        row1.insert("NAME".into(), NaturalValue::Alpha("Smith".into()));
        row1.insert("DEPT".into(), NaturalValue::Alpha("D01".into()));
        row1.insert("SALARY".into(), NaturalValue::Integer(50000));
        db.insert_row("EMPLOYEES", row1);

        let mut row2 = HashMap::new();
        row2.insert("ID".into(), NaturalValue::Integer(2));
        row2.insert("NAME".into(), NaturalValue::Alpha("Jones".into()));
        row2.insert("DEPT".into(), NaturalValue::Alpha("D02".into()));
        row2.insert("SALARY".into(), NaturalValue::Integer(60000));
        db.insert_row("EMPLOYEES", row2);

        let mut row3 = HashMap::new();
        row3.insert("ID".into(), NaturalValue::Integer(3));
        row3.insert("NAME".into(), NaturalValue::Alpha("Taylor".into()));
        row3.insert("DEPT".into(), NaturalValue::Alpha("D01".into()));
        row3.insert("SALARY".into(), NaturalValue::Integer(55000));
        db.insert_row("EMPLOYEES", row3);

        db
    }

    #[test]
    fn test_select_single() {
        let db = setup_db();
        let result = db.select_single(
            "EMPLOYEES",
            &["NAME".into(), "SALARY".into()],
            Some(&SqlCondition::Equals("ID".into(), NaturalValue::Integer(1))),
        ).unwrap();
        assert!(result.is_some());
        let vals = result.unwrap();
        assert_eq!(vals[0].to_display_string(), "Smith");
        assert_eq!(vals[1].to_i64(), 50000);
    }

    #[test]
    fn test_select_single_not_found() {
        let db = setup_db();
        let result = db.select_single(
            "EMPLOYEES",
            &["NAME".into()],
            Some(&SqlCondition::Equals("ID".into(), NaturalValue::Integer(999))),
        ).unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn test_select_cursor() {
        let db = setup_db();
        let result = db.select_cursor(
            "EMPLOYEES",
            &["NAME".into()],
            Some(&SqlCondition::Equals("DEPT".into(), NaturalValue::Alpha("D01".into()))),
        ).unwrap();
        assert_eq!(result.len(), 2);
    }

    #[test]
    fn test_select_all() {
        let db = setup_db();
        let result = db.select_cursor(
            "EMPLOYEES",
            &["NAME".into()],
            None,
        ).unwrap();
        assert_eq!(result.len(), 3);
    }

    #[test]
    fn test_insert() {
        let mut db = setup_db();
        let count = db.insert(
            "EMPLOYEES",
            &["ID".into(), "NAME".into(), "DEPT".into(), "SALARY".into()],
            &[NaturalValue::Integer(4), NaturalValue::Alpha("Brown".into()),
              NaturalValue::Alpha("D03".into()), NaturalValue::Integer(45000)],
        ).unwrap();
        assert_eq!(count, 1);
        assert_eq!(db.row_count("EMPLOYEES"), 4);
    }

    #[test]
    fn test_update() {
        let mut db = setup_db();
        let count = db.update(
            "EMPLOYEES",
            &[("SALARY".into(), NaturalValue::Integer(70000))],
            Some(&SqlCondition::Equals("ID".into(), NaturalValue::Integer(1))),
        ).unwrap();
        assert_eq!(count, 1);

        let result = db.select_single(
            "EMPLOYEES",
            &["SALARY".into()],
            Some(&SqlCondition::Equals("ID".into(), NaturalValue::Integer(1))),
        ).unwrap().unwrap();
        assert_eq!(result[0].to_i64(), 70000);
    }

    #[test]
    fn test_update_multiple() {
        let mut db = setup_db();
        let count = db.update(
            "EMPLOYEES",
            &[("SALARY".into(), NaturalValue::Integer(99999))],
            Some(&SqlCondition::Equals("DEPT".into(), NaturalValue::Alpha("D01".into()))),
        ).unwrap();
        assert_eq!(count, 2);
    }

    #[test]
    fn test_delete() {
        let mut db = setup_db();
        let count = db.delete(
            "EMPLOYEES",
            Some(&SqlCondition::Equals("ID".into(), NaturalValue::Integer(2))),
        ).unwrap();
        assert_eq!(count, 1);
        assert_eq!(db.row_count("EMPLOYEES"), 2);
    }

    #[test]
    fn test_delete_multiple() {
        let mut db = setup_db();
        let count = db.delete(
            "EMPLOYEES",
            Some(&SqlCondition::Equals("DEPT".into(), NaturalValue::Alpha("D01".into()))),
        ).unwrap();
        assert_eq!(count, 2);
        assert_eq!(db.row_count("EMPLOYEES"), 1);
    }

    #[test]
    fn test_commit() {
        let mut db = setup_db();
        db.insert(
            "EMPLOYEES",
            &["ID".into(), "NAME".into()],
            &[NaturalValue::Integer(4), NaturalValue::Alpha("New".into())],
        ).unwrap();
        db.commit().unwrap();
        assert_eq!(db.row_count("EMPLOYEES"), 4);
    }

    #[test]
    fn test_rollback() {
        let mut db = setup_db();
        let initial = db.row_count("EMPLOYEES");
        db.insert(
            "EMPLOYEES",
            &["ID".into(), "NAME".into()],
            &[NaturalValue::Integer(4), NaturalValue::Alpha("New".into())],
        ).unwrap();
        db.rollback().unwrap();
        assert_eq!(db.row_count("EMPLOYEES"), initial);
    }

    #[test]
    fn test_table_not_found() {
        let db = setup_db();
        let result = db.select_single("NONEXISTENT", &["X".into()], None);
        assert!(result.is_err());
    }

    #[test]
    fn test_condition_and() {
        let db = setup_db();
        let cond = SqlCondition::And(
            Box::new(SqlCondition::Equals("DEPT".into(), NaturalValue::Alpha("D01".into()))),
            Box::new(SqlCondition::GreaterThan("SALARY".into(), NaturalValue::Integer(52000))),
        );
        let result = db.select_cursor(
            "EMPLOYEES",
            &["NAME".into()],
            Some(&cond),
        ).unwrap();
        assert_eq!(result.len(), 1); // Taylor (D01, 55000)
    }

    #[test]
    fn test_condition_or() {
        let db = setup_db();
        let cond = SqlCondition::Or(
            Box::new(SqlCondition::Equals("ID".into(), NaturalValue::Integer(1))),
            Box::new(SqlCondition::Equals("ID".into(), NaturalValue::Integer(3))),
        );
        let result = db.select_cursor(
            "EMPLOYEES",
            &["NAME".into()],
            Some(&cond),
        ).unwrap();
        assert_eq!(result.len(), 2);
    }

    #[test]
    fn test_condition_not_equals() {
        let db = setup_db();
        let result = db.select_cursor(
            "EMPLOYEES",
            &["NAME".into()],
            Some(&SqlCondition::NotEquals("DEPT".into(), NaturalValue::Alpha("D01".into()))),
        ).unwrap();
        assert_eq!(result.len(), 1); // Only Jones
    }

    #[test]
    fn test_condition_less_than() {
        let db = setup_db();
        let result = db.select_cursor(
            "EMPLOYEES",
            &["NAME".into()],
            Some(&SqlCondition::LessThan("SALARY".into(), NaturalValue::Integer(55000))),
        ).unwrap();
        assert_eq!(result.len(), 1); // Smith
    }

    #[test]
    fn test_create_table() {
        let mut db = InMemorySql::new();
        db.create_table("TEST", &["A", "B"]);
        assert_eq!(db.row_count("TEST"), 0);
    }

    #[test]
    fn test_insert_into_new_table() {
        let mut db = InMemorySql::new();
        db.create_table("ITEMS", &["ID", "LABEL"]);
        db.insert(
            "ITEMS",
            &["ID".into(), "LABEL".into()],
            &[NaturalValue::Integer(1), NaturalValue::Alpha("Widget".into())],
        ).unwrap();
        assert_eq!(db.row_count("ITEMS"), 1);
    }
}
