//! EZ-104: SQL/Database Integration for Easytrieve Plus.
//!
//! Provides embedded SQL support (SQL ... END-SQL blocks), result set
//! handling, and a pluggable [`SqlBridge`] trait for database execution.

use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

/// Errors during SQL processing.
#[derive(Debug, Error, Diagnostic)]
pub enum SqlError {
    /// SQL execution failed.
    #[error("SQL execution error: {message}")]
    ExecutionError {
        /// Error message.
        message: String,
    },
    /// Column not found in result set.
    #[error("column '{column}' not found in result set")]
    ColumnNotFound {
        /// Column name.
        column: String,
    },
    /// No result set available.
    #[error("no result set available")]
    NoResultSet,
    /// SQL bridge not configured.
    #[error("SQL bridge not configured")]
    NoBridge,
}

// ---------------------------------------------------------------------------
// SQL block
// ---------------------------------------------------------------------------

/// An embedded SQL block (SQL ... END-SQL).
///
/// Captures the SQL statement text between SQL and END-SQL keywords.
#[derive(Debug, Clone)]
pub struct EzSqlBlock {
    /// The SQL statement text.
    pub sql_text: String,
    /// Host variable names referenced in the SQL (prefixed with colon).
    pub host_variables: Vec<String>,
}

impl EzSqlBlock {
    /// Create a new SQL block from statement text.
    pub fn new(sql_text: &str) -> Self {
        let host_variables = Self::extract_host_variables(sql_text);
        Self {
            sql_text: sql_text.to_string(),
            host_variables,
        }
    }

    /// Extract host variable names (tokens prefixed with ':').
    fn extract_host_variables(sql: &str) -> Vec<String> {
        let mut vars = Vec::new();
        let mut chars = sql.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == ':' {
                let mut name = String::new();
                while let Some(&next) = chars.peek() {
                    if next.is_ascii_alphanumeric() || next == '_' || next == '-' {
                        name.push(next);
                        chars.next();
                    } else {
                        break;
                    }
                }
                if !name.is_empty() {
                    vars.push(name);
                }
            }
        }

        vars
    }

    /// Get the SQL text with host variables replaced by '?' placeholders.
    pub fn parameterized_sql(&self) -> String {
        let mut result = self.sql_text.clone();
        for var in &self.host_variables {
            let pattern = format!(":{var}");
            result = result.replace(&pattern, "?");
        }
        result
    }
}

// ---------------------------------------------------------------------------
// SQL result
// ---------------------------------------------------------------------------

/// Result set from SQL execution.
///
/// Contains rows of column-value pairs.
#[derive(Debug, Clone)]
pub struct EzSqlResult {
    /// Column names in order.
    pub columns: Vec<String>,
    /// Rows of data (each row is a map of column name -> value).
    pub rows: Vec<HashMap<String, String>>,
    /// Number of rows affected (for UPDATE/DELETE/INSERT).
    pub rows_affected: usize,
    /// SQL return code (SQLCODE).
    pub sqlcode: i32,
}

impl EzSqlResult {
    /// Create an empty result set.
    pub fn empty() -> Self {
        Self {
            columns: Vec::new(),
            rows: Vec::new(),
            rows_affected: 0,
            sqlcode: 0,
        }
    }

    /// Create a result set with columns.
    pub fn with_columns(columns: Vec<String>) -> Self {
        Self {
            columns,
            rows: Vec::new(),
            rows_affected: 0,
            sqlcode: 0,
        }
    }

    /// Add a row to the result set.
    pub fn add_row(&mut self, row: HashMap<String, String>) {
        self.rows.push(row);
    }

    /// Get a column value from a specific row.
    pub fn get_value(&self, row: usize, column: &str) -> Result<&str, SqlError> {
        let row_data = self
            .rows
            .get(row)
            .ok_or(SqlError::NoResultSet)?;
        row_data
            .get(column)
            .map(|s| s.as_str())
            .ok_or_else(|| SqlError::ColumnNotFound {
                column: column.to_string(),
            })
    }

    /// Get the number of rows in the result set.
    pub fn row_count(&self) -> usize {
        self.rows.len()
    }

    /// Check if the result set is empty.
    pub fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }

    /// Check if SQL execution was successful (SQLCODE = 0 or 100).
    pub fn is_ok(&self) -> bool {
        self.sqlcode == 0 || self.sqlcode == 100
    }
}

// ---------------------------------------------------------------------------
// SQL bridge trait
// ---------------------------------------------------------------------------

/// Abstraction for SQL execution backends.
///
/// Implement this trait to connect Easytrieve SQL blocks to a real database.
pub trait SqlBridge {
    /// Execute a SQL statement and return the result.
    fn execute(
        &mut self,
        sql: &str,
        params: &[String],
    ) -> Result<EzSqlResult, SqlError>;

    /// Execute a SQL SELECT and return rows.
    fn query(
        &mut self,
        sql: &str,
        params: &[String],
    ) -> Result<EzSqlResult, SqlError>;

    /// Commit the current transaction.
    fn commit(&mut self) -> Result<(), SqlError>;

    /// Roll back the current transaction.
    fn rollback(&mut self) -> Result<(), SqlError>;
}

// ---------------------------------------------------------------------------
// Mock SQL bridge
// ---------------------------------------------------------------------------

/// Mock SQL bridge for testing.
///
/// Stores executed SQL and returns pre-configured results.
#[derive(Debug, Default)]
pub struct MockSqlBridge {
    /// SQL statements that were executed.
    pub executed: Vec<String>,
    /// Pre-configured results to return (keyed by SQL text fragment).
    pub results: HashMap<String, EzSqlResult>,
    /// Whether commit was called.
    pub committed: bool,
    /// Whether rollback was called.
    pub rolled_back: bool,
}

impl MockSqlBridge {
    /// Create a new mock bridge.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a pre-configured result for a SQL fragment.
    pub fn add_result(&mut self, sql_fragment: &str, result: EzSqlResult) {
        self.results.insert(sql_fragment.to_string(), result);
    }
}

impl SqlBridge for MockSqlBridge {
    fn execute(
        &mut self,
        sql: &str,
        _params: &[String],
    ) -> Result<EzSqlResult, SqlError> {
        self.executed.push(sql.to_string());
        // Check for matching result
        for (fragment, result) in &self.results {
            if sql.contains(fragment.as_str()) {
                return Ok(result.clone());
            }
        }
        let mut result = EzSqlResult::empty();
        result.rows_affected = 1;
        Ok(result)
    }

    fn query(
        &mut self,
        sql: &str,
        _params: &[String],
    ) -> Result<EzSqlResult, SqlError> {
        self.executed.push(sql.to_string());
        for (fragment, result) in &self.results {
            if sql.contains(fragment.as_str()) {
                return Ok(result.clone());
            }
        }
        Ok(EzSqlResult::empty())
    }

    fn commit(&mut self) -> Result<(), SqlError> {
        self.committed = true;
        Ok(())
    }

    fn rollback(&mut self) -> Result<(), SqlError> {
        self.rolled_back = true;
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sql_block_creation() {
        let block = EzSqlBlock::new("SELECT * FROM EMPLOYEES WHERE DEPT = :DEPT-CODE");
        assert_eq!(block.host_variables, vec!["DEPT-CODE"]);
    }

    #[test]
    fn test_sql_block_parameterized() {
        let block = EzSqlBlock::new("SELECT * FROM EMP WHERE ID = :EMP-ID AND NAME = :EMP-NAME");
        let param_sql = block.parameterized_sql();
        assert_eq!(param_sql, "SELECT * FROM EMP WHERE ID = ? AND NAME = ?");
    }

    #[test]
    fn test_sql_block_no_host_vars() {
        let block = EzSqlBlock::new("SELECT COUNT(*) FROM EMPLOYEES");
        assert!(block.host_variables.is_empty());
    }

    #[test]
    fn test_sql_result_creation() {
        let mut result = EzSqlResult::with_columns(vec![
            "ID".into(),
            "NAME".into(),
            "SALARY".into(),
        ]);

        let mut row = HashMap::new();
        row.insert("ID".into(), "1".into());
        row.insert("NAME".into(), "JOHN".into());
        row.insert("SALARY".into(), "50000".into());
        result.add_row(row);

        assert_eq!(result.row_count(), 1);
        assert!(!result.is_empty());
        assert_eq!(result.get_value(0, "NAME").unwrap(), "JOHN");
    }

    #[test]
    fn test_sql_result_column_not_found() {
        let result = EzSqlResult::with_columns(vec!["ID".into()]);
        assert!(result.get_value(0, "NAME").is_err());
    }

    #[test]
    fn test_sql_result_is_ok() {
        let mut result = EzSqlResult::empty();
        assert!(result.is_ok()); // SQLCODE 0

        result.sqlcode = 100; // Normal end of data
        assert!(result.is_ok());

        result.sqlcode = -803; // Duplicate key
        assert!(!result.is_ok());
    }

    #[test]
    fn test_mock_bridge_execute() {
        let mut bridge = MockSqlBridge::new();
        let result = bridge
            .execute("INSERT INTO EMP VALUES (1, 'JOHN')", &[])
            .unwrap();
        assert_eq!(result.rows_affected, 1);
        assert_eq!(bridge.executed.len(), 1);
    }

    #[test]
    fn test_mock_bridge_query_with_result() {
        let mut bridge = MockSqlBridge::new();

        let mut expected = EzSqlResult::with_columns(vec!["NAME".into()]);
        let mut row = HashMap::new();
        row.insert("NAME".into(), "ALICE".into());
        expected.add_row(row);

        bridge.add_result("SELECT", expected);

        let result = bridge.query("SELECT * FROM EMP", &[]).unwrap();
        assert_eq!(result.row_count(), 1);
        assert_eq!(result.get_value(0, "NAME").unwrap(), "ALICE");
    }

    #[test]
    fn test_mock_bridge_commit_rollback() {
        let mut bridge = MockSqlBridge::new();
        assert!(!bridge.committed);
        assert!(!bridge.rolled_back);

        bridge.commit().unwrap();
        assert!(bridge.committed);

        bridge.rollback().unwrap();
        assert!(bridge.rolled_back);
    }

    #[test]
    fn test_sql_result_empty() {
        let result = EzSqlResult::empty();
        assert!(result.is_empty());
        assert_eq!(result.row_count(), 0);
        assert!(result.is_ok());
    }
}
