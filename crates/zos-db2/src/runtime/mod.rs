//! DB2 runtime for SQL execution.
//!
//! This module provides runtime support for executing SQL statements
//! against a PostgreSQL database backend.

mod connection;
mod cursor;
mod executor;
mod sqlca;
mod transaction;
mod translate;
mod types;

pub use connection::{Db2Connection, Db2ConnectionConfig};
pub use cursor::{Cursor, CursorManager, CursorOptions, CursorState};
pub use executor::{RuntimeHostVariable, RuntimeStatement, SqlExecutor, SqlRow, SqlValue};
pub use sqlca::{Sqlca, SqlcaBuilder};
pub use transaction::{TransactionConfig, TransactionManager, TransactionState};
pub use translate::SqlTranslator;
pub use types::{picture_to_db2_type, Db2Type, TypeMapping};

use crate::Db2Result;

/// DB2 runtime context for a program.
pub struct Db2Runtime {
    /// Connection configuration
    config: Db2ConnectionConfig,
    /// Current SQLCA
    sqlca: Sqlca,
    /// SQL translator
    translator: SqlTranslator,
}

impl Db2Runtime {
    /// Create a new runtime with the given configuration.
    pub fn new(config: Db2ConnectionConfig) -> Self {
        Self {
            config,
            sqlca: Sqlca::new(),
            translator: SqlTranslator::new(),
        }
    }

    /// Create a runtime from environment variables.
    pub fn from_env() -> Db2Result<Self> {
        let config = Db2ConnectionConfig::from_env()?;
        Ok(Self::new(config))
    }

    /// Get the current SQLCA.
    pub fn sqlca(&self) -> &Sqlca {
        &self.sqlca
    }

    /// Get a mutable reference to SQLCA.
    pub fn sqlca_mut(&mut self) -> &mut Sqlca {
        &mut self.sqlca
    }

    /// Get the connection configuration.
    pub fn config(&self) -> &Db2ConnectionConfig {
        &self.config
    }

    /// Translate DB2 SQL to PostgreSQL.
    pub fn translate(&self, sql: &str) -> String {
        self.translator.translate(sql)
    }

    /// Execute SQL and update SQLCA (mock implementation without actual connection).
    pub fn execute_sql(&mut self, sql: &str) -> Db2Result<()> {
        // Reset SQLCA
        self.sqlca.reset();

        // Translate SQL
        let _pg_sql = self.translate(sql);

        // Without the postgres feature, we just simulate success
        #[cfg(not(feature = "postgres"))]
        {
            self.sqlca.set_success();
        }

        #[cfg(feature = "postgres")]
        {
            // Actual PostgreSQL execution would go here
            self.sqlca.set_success();
        }

        Ok(())
    }
}

impl Default for Db2Runtime {
    fn default() -> Self {
        Self::new(Db2ConnectionConfig::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_runtime_creation() {
        let runtime = Db2Runtime::default();
        assert_eq!(runtime.sqlca().sqlcode(), 0);
    }

    #[test]
    fn test_sql_translation() {
        let runtime = Db2Runtime::default();
        let sql = "SELECT * FROM T FETCH FIRST 10 ROWS ONLY";
        let pg_sql = runtime.translate(sql);
        assert!(pg_sql.contains("LIMIT 10"));
    }
}
