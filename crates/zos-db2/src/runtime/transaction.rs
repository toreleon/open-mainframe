//! Transaction management for DB2 operations.
//!
//! Provides COMMIT, ROLLBACK, and transaction control.

use crate::runtime::{CursorManager, Sqlca};
use crate::Db2Result;

/// Transaction state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TransactionState {
    /// No active transaction
    Inactive,
    /// Transaction in progress
    Active,
    /// Transaction committed
    Committed,
    /// Transaction rolled back
    RolledBack,
}

/// Transaction configuration.
#[derive(Debug, Clone)]
pub struct TransactionConfig {
    /// Auto-commit after each SQL statement
    pub auto_commit: bool,
    /// Implicit commit on normal program end
    pub implicit_commit: bool,
    /// Implicit rollback on error/abend
    pub implicit_rollback: bool,
}

impl Default for TransactionConfig {
    fn default() -> Self {
        Self {
            auto_commit: false,
            implicit_commit: true,
            implicit_rollback: true,
        }
    }
}

/// Transaction manager for coordinating DB2 operations.
pub struct TransactionManager {
    /// Current transaction state
    state: TransactionState,
    /// Transaction configuration
    config: TransactionConfig,
    /// Current SQLCA
    sqlca: Sqlca,
    /// Mock mode (no actual database connection)
    mock_mode: bool,
    /// Number of statements in current transaction
    statement_count: usize,
}

impl TransactionManager {
    /// Create a new transaction manager.
    pub fn new() -> Self {
        Self {
            state: TransactionState::Inactive,
            config: TransactionConfig::default(),
            sqlca: Sqlca::new(),
            mock_mode: true,
            statement_count: 0,
        }
    }

    /// Create with custom configuration.
    pub fn with_config(config: TransactionConfig) -> Self {
        Self {
            state: TransactionState::Inactive,
            config,
            sqlca: Sqlca::new(),
            mock_mode: true,
            statement_count: 0,
        }
    }

    /// Get current transaction state.
    pub fn state(&self) -> TransactionState {
        self.state
    }

    /// Get the current SQLCA.
    pub fn sqlca(&self) -> &Sqlca {
        &self.sqlca
    }

    /// Get mutable SQLCA.
    pub fn sqlca_mut(&mut self) -> &mut Sqlca {
        &mut self.sqlca
    }

    /// Get configuration.
    pub fn config(&self) -> &TransactionConfig {
        &self.config
    }

    /// Set configuration.
    pub fn set_config(&mut self, config: TransactionConfig) {
        self.config = config;
    }

    /// Begin a transaction (implicit - called on first SQL statement).
    pub fn begin(&mut self) -> Db2Result<()> {
        if self.state == TransactionState::Inactive {
            self.state = TransactionState::Active;
            self.statement_count = 0;
            self.sqlca.reset();
            self.sqlca.set_success();
        }
        Ok(())
    }

    /// Record that a SQL statement was executed.
    pub fn record_statement(&mut self) {
        if self.state == TransactionState::Inactive {
            self.state = TransactionState::Active;
        }
        self.statement_count += 1;
    }

    /// Commit the current transaction.
    pub fn commit(&mut self, cursor_manager: Option<&mut CursorManager>) -> Db2Result<()> {
        self.sqlca.reset();

        if self.state != TransactionState::Active {
            // No active transaction - still success in DB2
            self.sqlca.set_success();
            return Ok(());
        }

        // In mock mode, just update state
        if self.mock_mode {
            self.state = TransactionState::Committed;
            self.sqlca.set_success();

            // Notify cursor manager
            if let Some(cm) = cursor_manager {
                cm.on_commit();
            }

            // Reset for next transaction
            self.state = TransactionState::Inactive;
            self.statement_count = 0;
            return Ok(());
        }

        #[cfg(feature = "postgres")]
        {
            // Actual PostgreSQL COMMIT would go here
            self.state = TransactionState::Committed;
            self.sqlca.set_success();

            if let Some(cm) = cursor_manager {
                cm.on_commit();
            }

            self.state = TransactionState::Inactive;
            self.statement_count = 0;
        }

        Ok(())
    }

    /// Rollback the current transaction.
    pub fn rollback(&mut self, cursor_manager: Option<&mut CursorManager>) -> Db2Result<()> {
        self.sqlca.reset();

        if self.state != TransactionState::Active {
            // No active transaction - still success in DB2
            self.sqlca.set_success();
            return Ok(());
        }

        // In mock mode, just update state
        if self.mock_mode {
            self.state = TransactionState::RolledBack;
            self.sqlca.set_success();

            // Notify cursor manager - rollback closes all cursors
            if let Some(cm) = cursor_manager {
                cm.on_rollback();
            }

            // Reset for next transaction
            self.state = TransactionState::Inactive;
            self.statement_count = 0;
            return Ok(());
        }

        #[cfg(feature = "postgres")]
        {
            // Actual PostgreSQL ROLLBACK would go here
            self.state = TransactionState::RolledBack;
            self.sqlca.set_success();

            if let Some(cm) = cursor_manager {
                cm.on_rollback();
            }

            self.state = TransactionState::Inactive;
            self.statement_count = 0;
        }

        Ok(())
    }

    /// Handle implicit commit (on normal program end).
    pub fn on_program_end(
        &mut self,
        cursor_manager: Option<&mut CursorManager>,
    ) -> Db2Result<()> {
        if self.config.implicit_commit && self.state == TransactionState::Active {
            self.commit(cursor_manager)?;
        }
        Ok(())
    }

    /// Handle implicit rollback (on error/abend).
    pub fn on_program_error(
        &mut self,
        cursor_manager: Option<&mut CursorManager>,
    ) -> Db2Result<()> {
        if self.config.implicit_rollback && self.state == TransactionState::Active {
            self.rollback(cursor_manager)?;
        }
        Ok(())
    }

    /// Check if auto-commit should happen after statement.
    pub fn should_auto_commit(&self) -> bool {
        self.config.auto_commit && self.state == TransactionState::Active
    }

    /// Get number of statements in current transaction.
    pub fn statement_count(&self) -> usize {
        self.statement_count
    }
}

impl Default for TransactionManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transaction_begin() {
        let mut tm = TransactionManager::new();
        assert_eq!(tm.state(), TransactionState::Inactive);

        tm.begin().unwrap();
        assert_eq!(tm.state(), TransactionState::Active);
    }

    #[test]
    fn test_commit() {
        let mut tm = TransactionManager::new();
        tm.begin().unwrap();
        tm.record_statement();

        tm.commit(None).unwrap();

        assert!(tm.sqlca().is_success());
        assert_eq!(tm.state(), TransactionState::Inactive);
        assert_eq!(tm.statement_count(), 0);
    }

    #[test]
    fn test_rollback() {
        let mut tm = TransactionManager::new();
        tm.begin().unwrap();
        tm.record_statement();

        tm.rollback(None).unwrap();

        assert!(tm.sqlca().is_success());
        assert_eq!(tm.state(), TransactionState::Inactive);
    }

    #[test]
    fn test_commit_no_transaction() {
        let mut tm = TransactionManager::new();

        // Commit without active transaction should succeed (DB2 behavior)
        tm.commit(None).unwrap();
        assert!(tm.sqlca().is_success());
    }

    #[test]
    fn test_rollback_no_transaction() {
        let mut tm = TransactionManager::new();

        // Rollback without active transaction should succeed (DB2 behavior)
        tm.rollback(None).unwrap();
        assert!(tm.sqlca().is_success());
    }

    #[test]
    fn test_implicit_commit() {
        let mut tm = TransactionManager::new();
        assert!(tm.config().implicit_commit);

        tm.begin().unwrap();
        tm.record_statement();

        tm.on_program_end(None).unwrap();

        // Should have committed
        assert_eq!(tm.state(), TransactionState::Inactive);
    }

    #[test]
    fn test_implicit_rollback() {
        let mut tm = TransactionManager::new();
        assert!(tm.config().implicit_rollback);

        tm.begin().unwrap();
        tm.record_statement();

        tm.on_program_error(None).unwrap();

        // Should have rolled back
        assert_eq!(tm.state(), TransactionState::Inactive);
    }

    #[test]
    fn test_auto_commit() {
        let config = TransactionConfig {
            auto_commit: true,
            ..Default::default()
        };
        let mut tm = TransactionManager::with_config(config);

        tm.begin().unwrap();
        assert!(tm.should_auto_commit());
    }

    #[test]
    fn test_statement_count() {
        let mut tm = TransactionManager::new();
        tm.begin().unwrap();

        tm.record_statement();
        assert_eq!(tm.statement_count(), 1);

        tm.record_statement();
        assert_eq!(tm.statement_count(), 2);

        tm.commit(None).unwrap();
        assert_eq!(tm.statement_count(), 0);
    }

    #[test]
    fn test_implicit_commit_disabled() {
        let config = TransactionConfig {
            implicit_commit: false,
            ..Default::default()
        };
        let mut tm = TransactionManager::with_config(config);

        tm.begin().unwrap();
        tm.record_statement();

        tm.on_program_end(None).unwrap();

        // Should still be active (no implicit commit)
        assert_eq!(tm.state(), TransactionState::Active);
    }
}
