//! IDMS-108: SQL Option (5 stories).
//!
//! Provides SQL access to CODASYL data.  The SQL option allows programs
//! to use standard SQL SELECT/INSERT/UPDATE/DELETE statements against
//! IDMS records and sets, with an engine that translates SQL into
//! navigational DML operations internally.

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  SQL statement types
// ---------------------------------------------------------------------------

/// Parsed SQL statement targeting IDMS data.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SqlStatement {
    /// SELECT columns FROM table [WHERE conditions].
    Select {
        /// Column names (* for all).
        columns: Vec<String>,
        /// Table (record type) name.
        table: String,
        /// Optional WHERE clause conditions.
        where_clause: Option<String>,
    },
    /// INSERT INTO table (columns) VALUES (values).
    Insert {
        /// Table (record type) name.
        table: String,
        /// Column-value pairs.
        values: Vec<(String, String)>,
    },
    /// UPDATE table SET column=value [WHERE conditions].
    Update {
        /// Table (record type) name.
        table: String,
        /// Column-value pairs to update.
        assignments: Vec<(String, String)>,
        /// Optional WHERE clause.
        where_clause: Option<String>,
    },
    /// DELETE FROM table [WHERE conditions].
    Delete {
        /// Table (record type) name.
        table: String,
        /// Optional WHERE clause.
        where_clause: Option<String>,
    },
}

// ---------------------------------------------------------------------------
//  SQL parser
// ---------------------------------------------------------------------------

/// Parser for SQL statements targeting IDMS tables.
///
/// Supports a simplified SQL syntax for SELECT, INSERT, UPDATE, and DELETE.
#[derive(Debug)]
pub struct IdmsSqlParser;

impl IdmsSqlParser {
    /// Parse a SQL statement string.
    pub fn parse(input: &str) -> Result<SqlStatement, SqlError> {
        let trimmed = input.trim().trim_end_matches(';');
        let tokens: Vec<&str> = trimmed.split_whitespace().collect();
        if tokens.is_empty() {
            return Err(SqlError::EmptyStatement);
        }

        match tokens[0].to_uppercase().as_str() {
            "SELECT" => Self::parse_select(&tokens),
            "INSERT" => Self::parse_insert(&tokens),
            "UPDATE" => Self::parse_update(&tokens),
            "DELETE" => Self::parse_delete(&tokens),
            _ => Err(SqlError::UnsupportedStatement(tokens[0].to_string())),
        }
    }

    fn parse_select(tokens: &[&str]) -> Result<SqlStatement, SqlError> {
        // SELECT col1,col2 FROM table [WHERE ...]
        let from_pos = tokens
            .iter()
            .position(|t| t.eq_ignore_ascii_case("FROM"))
            .ok_or(SqlError::MissingKeyword("FROM".into()))?;

        let col_tokens = &tokens[1..from_pos];
        let columns: Vec<String> = col_tokens
            .iter()
            .flat_map(|t| t.split(','))
            .filter(|s| !s.is_empty())
            .map(|s| s.to_uppercase())
            .collect();

        if from_pos + 1 >= tokens.len() {
            return Err(SqlError::MissingTableName);
        }
        let table = tokens[from_pos + 1].to_uppercase();

        let where_clause = Self::extract_where(tokens, from_pos + 2);

        Ok(SqlStatement::Select {
            columns,
            table,
            where_clause,
        })
    }

    fn parse_insert(tokens: &[&str]) -> Result<SqlStatement, SqlError> {
        // INSERT INTO table (col1,col2) VALUES (val1,val2)
        if tokens.len() < 3 || !tokens[1].eq_ignore_ascii_case("INTO") {
            return Err(SqlError::MissingKeyword("INTO".into()));
        }
        let table = tokens[2].to_uppercase();

        let values_pos = tokens
            .iter()
            .position(|t| t.eq_ignore_ascii_case("VALUES"))
            .ok_or(SqlError::MissingKeyword("VALUES".into()))?;

        // Extract column names from between table and VALUES.
        let col_str: String = tokens[3..values_pos].join(" ");
        let cols = Self::extract_paren_list(&col_str);

        // Extract values from after VALUES.
        let val_str: String = tokens[values_pos + 1..].join(" ");
        let vals = Self::extract_paren_list(&val_str);

        if cols.len() != vals.len() {
            return Err(SqlError::ColumnValueMismatch);
        }

        let values: Vec<(String, String)> = cols
            .into_iter()
            .zip(vals)
            .map(|(c, v)| (c.to_uppercase(), v))
            .collect();

        Ok(SqlStatement::Insert { table, values })
    }

    fn parse_update(tokens: &[&str]) -> Result<SqlStatement, SqlError> {
        // UPDATE table SET col1=val1,col2=val2 [WHERE ...]
        if tokens.len() < 4 {
            return Err(SqlError::MissingKeyword("SET".into()));
        }
        let table = tokens[1].to_uppercase();

        if !tokens[2].eq_ignore_ascii_case("SET") {
            return Err(SqlError::MissingKeyword("SET".into()));
        }

        let where_pos = tokens
            .iter()
            .position(|t| t.eq_ignore_ascii_case("WHERE"));
        let set_end = where_pos.unwrap_or(tokens.len());

        let set_str: String = tokens[3..set_end].join(" ");
        let assignments: Vec<(String, String)> = set_str
            .split(',')
            .filter_map(|part| {
                let mut split = part.splitn(2, '=');
                let col = split.next()?.trim().to_uppercase();
                let val = split.next()?.trim().to_string();
                if col.is_empty() {
                    None
                } else {
                    Some((col, val))
                }
            })
            .collect();

        let where_clause = where_pos.map(|wp| {
            tokens[wp + 1..]
                .join(" ")
                .to_uppercase()
        });

        Ok(SqlStatement::Update {
            table,
            assignments,
            where_clause,
        })
    }

    fn parse_delete(tokens: &[&str]) -> Result<SqlStatement, SqlError> {
        // DELETE FROM table [WHERE ...]
        if tokens.len() < 3 || !tokens[1].eq_ignore_ascii_case("FROM") {
            return Err(SqlError::MissingKeyword("FROM".into()));
        }
        let table = tokens[2].to_uppercase();
        let where_clause = Self::extract_where(tokens, 3);
        Ok(SqlStatement::Delete {
            table,
            where_clause,
        })
    }

    fn extract_where(tokens: &[&str], start: usize) -> Option<String> {
        if start < tokens.len() && tokens[start].eq_ignore_ascii_case("WHERE") {
            Some(
                tokens[start + 1..]
                    .join(" ")
                    .to_uppercase(),
            )
        } else {
            None
        }
    }

    fn extract_paren_list(s: &str) -> Vec<String> {
        let s = s.trim();
        let inner = s
            .strip_prefix('(')
            .and_then(|r| r.strip_suffix(')'))
            .unwrap_or(s);
        inner
            .split(',')
            .map(|p| p.trim().trim_matches('\'').to_string())
            .collect()
    }
}

// ---------------------------------------------------------------------------
//  SQL view
// ---------------------------------------------------------------------------

/// A SQL VIEW over CODASYL data.
///
/// Represents a named query that can be used in place of a table name.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SqlView {
    /// View name.
    pub name: String,
    /// Underlying SELECT statement (as text).
    pub query: String,
    /// Column names exposed by the view.
    pub columns: Vec<String>,
}

impl SqlView {
    /// Create a new SQL view.
    pub fn new(name: &str, query: &str, columns: Vec<String>) -> Self {
        Self {
            name: name.to_uppercase(),
            query: query.to_string(),
            columns,
        }
    }
}

// ---------------------------------------------------------------------------
//  SQL engine
// ---------------------------------------------------------------------------

/// Executes SQL statements by translating them to DML operations.
///
/// This is a simplified engine that demonstrates the SQL-to-DML mapping.
#[derive(Debug, Default)]
pub struct IdmsSqlEngine {
    /// Registered views.
    views: HashMap<String, SqlView>,
    /// Execution log for testing/auditing.
    pub execution_log: Vec<String>,
}

impl IdmsSqlEngine {
    /// Create a new SQL engine.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a SQL view.
    pub fn register_view(&mut self, view: SqlView) {
        self.views.insert(view.name.clone(), view);
    }

    /// Look up a registered view.
    pub fn get_view(&self, name: &str) -> Option<&SqlView> {
        self.views.get(&name.to_uppercase())
    }

    /// Execute a SQL statement (simplified: logs the DML translation).
    pub fn execute(&mut self, stmt: &SqlStatement) -> Result<SqlResult, SqlError> {
        match stmt {
            SqlStatement::Select {
                columns,
                table,
                where_clause,
            } => {
                let dml = format!(
                    "FIND/GET {table} fields={cols}{where_part}",
                    cols = columns.join(","),
                    where_part = where_clause
                        .as_ref()
                        .map(|w| format!(" WHERE {w}"))
                        .unwrap_or_default(),
                );
                self.execution_log.push(dml);
                Ok(SqlResult {
                    rows_affected: 0,
                    message: format!("SELECT from {table}"),
                })
            }
            SqlStatement::Insert { table, values } => {
                let dml = format!(
                    "STORE {table} fields={}",
                    values
                        .iter()
                        .map(|(c, v)| format!("{c}={v}"))
                        .collect::<Vec<_>>()
                        .join(","),
                );
                self.execution_log.push(dml);
                Ok(SqlResult {
                    rows_affected: 1,
                    message: format!("INSERT into {table}"),
                })
            }
            SqlStatement::Update {
                table,
                assignments,
                ..
            } => {
                let dml = format!(
                    "MODIFY {table} set={}",
                    assignments
                        .iter()
                        .map(|(c, v)| format!("{c}={v}"))
                        .collect::<Vec<_>>()
                        .join(","),
                );
                self.execution_log.push(dml);
                Ok(SqlResult {
                    rows_affected: 1,
                    message: format!("UPDATE {table}"),
                })
            }
            SqlStatement::Delete { table, .. } => {
                let dml = format!("ERASE {table}");
                self.execution_log.push(dml);
                Ok(SqlResult {
                    rows_affected: 1,
                    message: format!("DELETE from {table}"),
                })
            }
        }
    }
}

/// Result of a SQL execution.
#[derive(Debug, Clone)]
pub struct SqlResult {
    /// Number of rows affected.
    pub rows_affected: usize,
    /// Human-readable message.
    pub message: String,
}

// ---------------------------------------------------------------------------
//  SQL DDL statements
// ---------------------------------------------------------------------------

/// A SQL DDL statement for IDMS.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SqlDdl {
    /// CREATE TABLE name (columns...).
    CreateTable {
        /// Table name.
        name: String,
        /// Column definitions (name, type_str).
        columns: Vec<(String, String)>,
    },
    /// CREATE INDEX name ON table (columns...).
    CreateIndex {
        /// Index name.
        name: String,
        /// Target table.
        table: String,
        /// Indexed column names.
        columns: Vec<String>,
    },
    /// DROP TABLE name.
    DropTable {
        /// Table name to drop.
        name: String,
    },
}

// ---------------------------------------------------------------------------
//  SQL cursor
// ---------------------------------------------------------------------------

/// A SQL cursor for multi-row retrieval.
#[derive(Debug, Clone)]
pub struct SqlCursor {
    /// Cursor name.
    pub name: String,
    /// The query this cursor is declared for.
    pub query: String,
    /// Whether the cursor is currently open.
    pub open: bool,
    /// Current position in the result set (0-based).
    pub position: usize,
}

impl SqlCursor {
    /// Declare a new cursor.
    pub fn declare(name: &str, query: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            query: query.to_string(),
            open: false,
            position: 0,
        }
    }

    /// Open the cursor.
    pub fn open(&mut self) {
        self.open = true;
        self.position = 0;
    }

    /// Fetch next row, returning the current position.
    pub fn fetch(&mut self) -> Option<usize> {
        if !self.open {
            return None;
        }
        let pos = self.position;
        self.position += 1;
        Some(pos)
    }

    /// Close the cursor.
    pub fn close(&mut self) {
        self.open = false;
        self.position = 0;
    }
}

// ---------------------------------------------------------------------------
//  Catalog tables
// ---------------------------------------------------------------------------

/// Represents an IDMS system catalog table entry.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CatalogEntry {
    /// Table/object name.
    pub name: String,
    /// Object type (TABLE, INDEX, VIEW, etc.).
    pub object_type: String,
    /// Schema that owns this object.
    pub schema_name: String,
}

impl CatalogEntry {
    /// Create a new catalog entry.
    pub fn new(name: &str, object_type: &str, schema_name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            object_type: object_type.to_uppercase(),
            schema_name: schema_name.to_uppercase(),
        }
    }
}

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors from SQL parsing or execution.
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum SqlError {
    /// Empty SQL statement.
    #[error("empty SQL statement")]
    EmptyStatement,
    /// Missing required keyword.
    #[error("missing keyword: {0}")]
    MissingKeyword(String),
    /// Missing table name.
    #[error("missing table name")]
    MissingTableName,
    /// Column/value count mismatch in INSERT.
    #[error("column count does not match value count")]
    ColumnValueMismatch,
    /// Unsupported SQL statement type.
    #[error("unsupported statement: {0}")]
    UnsupportedStatement(String),
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_select() {
        let stmt = IdmsSqlParser::parse("SELECT EMP-ID, EMP-NAME FROM EMPLOYEE").unwrap();
        match stmt {
            SqlStatement::Select {
                columns,
                table,
                where_clause,
            } => {
                assert_eq!(columns, vec!["EMP-ID", "EMP-NAME"]);
                assert_eq!(table, "EMPLOYEE");
                assert!(where_clause.is_none());
            }
            _ => panic!("expected SELECT"),
        }
    }

    #[test]
    fn parse_select_star() {
        let stmt = IdmsSqlParser::parse("SELECT * FROM DEPARTMENT").unwrap();
        match stmt {
            SqlStatement::Select { columns, table, .. } => {
                assert_eq!(columns, vec!["*"]);
                assert_eq!(table, "DEPARTMENT");
            }
            _ => panic!("expected SELECT"),
        }
    }

    #[test]
    fn parse_select_with_where() {
        let stmt =
            IdmsSqlParser::parse("SELECT EMP-ID FROM EMPLOYEE WHERE DEPT-ID = 10").unwrap();
        match stmt {
            SqlStatement::Select { where_clause, .. } => {
                assert!(where_clause.is_some());
                assert!(where_clause.unwrap().contains("DEPT-ID"));
            }
            _ => panic!("expected SELECT"),
        }
    }

    #[test]
    fn parse_insert() {
        let stmt = IdmsSqlParser::parse(
            "INSERT INTO EMPLOYEE (EMP-ID, EMP-NAME) VALUES (100, 'SMITH')",
        )
        .unwrap();
        match stmt {
            SqlStatement::Insert { table, values } => {
                assert_eq!(table, "EMPLOYEE");
                assert_eq!(values.len(), 2);
                assert_eq!(values[0].0, "EMP-ID");
            }
            _ => panic!("expected INSERT"),
        }
    }

    #[test]
    fn parse_update() {
        let stmt =
            IdmsSqlParser::parse("UPDATE EMPLOYEE SET SALARY=50000 WHERE EMP-ID = 100")
                .unwrap();
        match stmt {
            SqlStatement::Update {
                table,
                assignments,
                where_clause,
            } => {
                assert_eq!(table, "EMPLOYEE");
                assert_eq!(assignments.len(), 1);
                assert_eq!(assignments[0].0, "SALARY");
                assert!(where_clause.is_some());
            }
            _ => panic!("expected UPDATE"),
        }
    }

    #[test]
    fn parse_delete() {
        let stmt = IdmsSqlParser::parse("DELETE FROM EMPLOYEE WHERE EMP-ID = 100").unwrap();
        match stmt {
            SqlStatement::Delete {
                table,
                where_clause,
            } => {
                assert_eq!(table, "EMPLOYEE");
                assert!(where_clause.is_some());
            }
            _ => panic!("expected DELETE"),
        }
    }

    #[test]
    fn parse_empty_error() {
        assert!(IdmsSqlParser::parse("").is_err());
    }

    #[test]
    fn parse_unsupported() {
        assert!(IdmsSqlParser::parse("CREATE TABLE FOO (A INT)").is_err());
    }

    #[test]
    fn sql_engine_execute_select() {
        let mut engine = IdmsSqlEngine::new();
        let stmt = IdmsSqlParser::parse("SELECT * FROM EMPLOYEE").unwrap();
        let result = engine.execute(&stmt).unwrap();
        assert!(result.message.contains("SELECT"));
        assert_eq!(engine.execution_log.len(), 1);
    }

    #[test]
    fn sql_engine_execute_insert() {
        let mut engine = IdmsSqlEngine::new();
        let stmt = IdmsSqlParser::parse(
            "INSERT INTO EMPLOYEE (EMP-ID) VALUES (1)",
        )
        .unwrap();
        let result = engine.execute(&stmt).unwrap();
        assert_eq!(result.rows_affected, 1);
    }

    #[test]
    fn sql_view_registration() {
        let mut engine = IdmsSqlEngine::new();
        let view = SqlView::new(
            "EMP_VIEW",
            "SELECT EMP-ID, EMP-NAME FROM EMPLOYEE",
            vec!["EMP-ID".into(), "EMP-NAME".into()],
        );
        engine.register_view(view);
        assert!(engine.get_view("EMP_VIEW").is_some());
        assert!(engine.get_view("NONEXISTENT").is_none());
    }
}
