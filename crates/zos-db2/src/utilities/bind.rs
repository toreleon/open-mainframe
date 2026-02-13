//! BIND utility for creating prepared statements from DBRMs.
//!
//! Processes DBRM files and creates execution plans in PostgreSQL.

use crate::preprocess::Dbrm;
use crate::Db2Result;
use std::path::Path;

/// Bind options.
#[derive(Debug, Clone)]
pub struct BindOptions {
    /// Package name (default: derived from DBRM name)
    pub package: Option<String>,
    /// Collection ID
    pub collection: String,
    /// Owner/qualifier
    pub owner: String,
    /// Isolation level
    pub isolation: IsolationLevel,
    /// Action on existing package
    pub action: BindAction,
    /// Validate at bind time or runtime
    pub validate: ValidateOption,
    /// Release cursor resources
    pub release: ReleaseOption,
}

impl Default for BindOptions {
    fn default() -> Self {
        Self {
            package: None,
            collection: "NULLID".to_string(),
            owner: String::new(),
            isolation: IsolationLevel::CursorStability,
            action: BindAction::Replace,
            validate: ValidateOption::Bind,
            release: ReleaseOption::Commit,
        }
    }
}

/// SQL isolation level.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IsolationLevel {
    /// Read uncommitted (UR)
    UncommittedRead,
    /// Read committed (CS)
    CursorStability,
    /// Repeatable read (RS)
    ReadStability,
    /// Serializable (RR)
    RepeatableRead,
}

impl IsolationLevel {
    /// Convert to PostgreSQL isolation level.
    pub fn to_postgres(&self) -> &'static str {
        match self {
            IsolationLevel::UncommittedRead => "READ UNCOMMITTED",
            IsolationLevel::CursorStability => "READ COMMITTED",
            IsolationLevel::ReadStability => "REPEATABLE READ",
            IsolationLevel::RepeatableRead => "SERIALIZABLE",
        }
    }
}

/// Action on existing package.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindAction {
    /// Add new package (fail if exists)
    Add,
    /// Replace existing package
    Replace,
}

/// Validation timing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValidateOption {
    /// Validate at bind time
    Bind,
    /// Validate at runtime
    Run,
}

/// Cursor release option.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReleaseOption {
    /// Release at commit
    Commit,
    /// Release when connection closes
    Deallocate,
}

/// Bound package information.
#[derive(Debug, Clone)]
pub struct BoundPackage {
    /// Package name
    pub name: String,
    /// Collection ID
    pub collection: String,
    /// Number of statements
    pub statement_count: usize,
    /// Bind timestamp
    pub bound_at: String,
    /// Prepared statement names
    pub prepared_statements: Vec<String>,
}

/// BIND utility.
pub struct Binder {
    options: BindOptions,
    #[allow(dead_code)]
    mock_mode: bool,
}

impl Binder {
    /// Create a new binder.
    pub fn new() -> Self {
        Self {
            options: BindOptions::default(),
            mock_mode: true,
        }
    }

    /// Create with options.
    pub fn with_options(options: BindOptions) -> Self {
        Self {
            options,
            mock_mode: true,
        }
    }

    /// Bind a DBRM to create prepared statements.
    pub fn bind(&self, dbrm: &Dbrm) -> Db2Result<BoundPackage> {
        let package_name = self
            .options
            .package
            .clone()
            .unwrap_or_else(|| dbrm.program_name.clone());

        // Generate prepared statement names
        let mut prepared_statements = Vec::new();

        for (i, _stmt) in dbrm.statements.iter().enumerate() {
            let stmt_name = format!(
                "{}_{:04}",
                package_name.to_uppercase(),
                i + 1
            );
            prepared_statements.push(stmt_name);
        }

        // In mock mode, just return the package info
        let package = BoundPackage {
            name: package_name,
            collection: self.options.collection.clone(),
            statement_count: dbrm.statements.len(),
            bound_at: chrono_stub(),
            prepared_statements,
        };

        Ok(package)
    }

    /// Bind from a DBRM file.
    pub fn bind_file(&self, dbrm_path: &Path) -> Db2Result<BoundPackage> {
        let dbrm = Dbrm::read_from_file(dbrm_path)?;
        self.bind(&dbrm)
    }

    /// Generate PostgreSQL PREPARE statements.
    pub fn generate_prepares(&self, dbrm: &Dbrm) -> Db2Result<Vec<String>> {
        let package_name = self
            .options
            .package
            .clone()
            .unwrap_or_else(|| dbrm.program_name.clone());

        let mut prepares = Vec::new();

        for (i, stmt) in dbrm.statements.iter().enumerate() {
            let stmt_name = format!(
                "{}_{:04}",
                package_name.to_uppercase(),
                i + 1
            );

            // Generate PREPARE statement
            let prepare = format!(
                "PREPARE {} AS {}",
                stmt_name,
                stmt.sql
            );
            prepares.push(prepare);
        }

        Ok(prepares)
    }

    /// Generate free (deallocate) statements.
    pub fn generate_frees(&self, package: &BoundPackage) -> Vec<String> {
        package
            .prepared_statements
            .iter()
            .map(|name| format!("DEALLOCATE {}", name))
            .collect()
    }
}

impl Default for Binder {
    fn default() -> Self {
        Self::new()
    }
}

/// Stub for timestamp (avoids chrono dependency).
fn chrono_stub() -> String {
    "2026-02-13 12:00:00".to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::preprocess::{SqlStatement, SqlStatementType};

    fn create_test_dbrm() -> Dbrm {
        let mut dbrm = Dbrm::new("TESTPROG");
        let statements = vec![
            SqlStatement {
                number: 1,
                sql: "SELECT NAME INTO :WS-NAME FROM CUSTOMER WHERE ID = :WS-ID".to_string(),
                start_line: 10,
                end_line: 12,
                stmt_type: SqlStatementType::SelectInto,
            },
            SqlStatement {
                number: 2,
                sql: "INSERT INTO CUSTOMER (ID, NAME) VALUES (:WS-ID, :WS-NAME)".to_string(),
                start_line: 15,
                end_line: 15,
                stmt_type: SqlStatementType::Insert,
            },
        ];
        dbrm.add_statements(&statements, &[]);
        dbrm
    }

    #[test]
    fn test_bind_dbrm() {
        let binder = Binder::new();
        let dbrm = create_test_dbrm();

        let package = binder.bind(&dbrm).unwrap();

        assert_eq!(package.name, "TESTPROG");
        assert_eq!(package.statement_count, 2);
        assert_eq!(package.prepared_statements.len(), 2);
        assert_eq!(package.prepared_statements[0], "TESTPROG_0001");
        assert_eq!(package.prepared_statements[1], "TESTPROG_0002");
    }

    #[test]
    fn test_custom_package_name() {
        let options = BindOptions {
            package: Some("MYPKG".to_string()),
            ..Default::default()
        };
        let binder = Binder::with_options(options);
        let dbrm = create_test_dbrm();

        let package = binder.bind(&dbrm).unwrap();

        assert_eq!(package.name, "MYPKG");
        assert!(package.prepared_statements[0].starts_with("MYPKG_"));
    }

    #[test]
    fn test_generate_prepares() {
        let binder = Binder::new();
        let dbrm = create_test_dbrm();

        let prepares = binder.generate_prepares(&dbrm).unwrap();

        assert_eq!(prepares.len(), 2);
        assert!(prepares[0].starts_with("PREPARE TESTPROG_0001 AS"));
        assert!(prepares[0].contains("SELECT NAME"));
    }

    #[test]
    fn test_generate_frees() {
        let binder = Binder::new();
        let dbrm = create_test_dbrm();

        let package = binder.bind(&dbrm).unwrap();
        let frees = binder.generate_frees(&package);

        assert_eq!(frees.len(), 2);
        assert_eq!(frees[0], "DEALLOCATE TESTPROG_0001");
    }

    #[test]
    fn test_isolation_level_conversion() {
        assert_eq!(IsolationLevel::UncommittedRead.to_postgres(), "READ UNCOMMITTED");
        assert_eq!(IsolationLevel::CursorStability.to_postgres(), "READ COMMITTED");
        assert_eq!(IsolationLevel::ReadStability.to_postgres(), "REPEATABLE READ");
        assert_eq!(IsolationLevel::RepeatableRead.to_postgres(), "SERIALIZABLE");
    }

    #[test]
    fn test_collection_id() {
        let options = BindOptions {
            collection: "MYLIB".to_string(),
            ..Default::default()
        };
        let binder = Binder::with_options(options);
        let dbrm = create_test_dbrm();

        let package = binder.bind(&dbrm).unwrap();

        assert_eq!(package.collection, "MYLIB");
    }
}
