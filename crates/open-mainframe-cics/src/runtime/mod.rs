//! CICS runtime for command execution.
//!
//! This module provides runtime support for executing CICS commands.

mod commands;
pub mod dispatcher;
pub mod eib;
mod files;

pub use commands::{CicsRuntime, ProgramRegistry, ProgramResult};
pub use dispatcher::{CicsDispatcher, CommandParamBlock, DispatchError, DispatchResult};
pub use eib::Eib;
pub use files::{
    CicsFile, FctEntry, FileManager, FileMode, FileRecord, PersistentFileManager,
    VsamAccessMethod,
};

use crate::channels::ChannelManager;


/// COMMAREA (Communication Area) for passing data between programs.
#[derive(Debug, Clone, Default)]
pub struct Commarea {
    /// Data bytes
    data: Vec<u8>,
    /// Maximum length
    max_length: usize,
}

impl Commarea {
    /// Create a new COMMAREA with specified max length.
    pub fn new(max_length: usize) -> Self {
        Self {
            data: vec![0; max_length],
            max_length,
        }
    }

    /// Create from existing data.
    pub fn from_data(data: Vec<u8>) -> Self {
        let max_length = data.len();
        Self { data, max_length }
    }

    /// Get data as bytes.
    pub fn data(&self) -> &[u8] {
        &self.data
    }

    /// Get mutable data.
    pub fn data_mut(&mut self) -> &mut [u8] {
        &mut self.data
    }

    /// Get length.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Set data from string.
    pub fn set_string(&mut self, s: &str) {
        let bytes = s.as_bytes();
        let copy_len = std::cmp::min(bytes.len(), self.max_length);
        self.data[..copy_len].copy_from_slice(&bytes[..copy_len]);
        // Pad with spaces
        for byte in &mut self.data[copy_len..] {
            *byte = b' ';
        }
    }

    /// Get data as string.
    pub fn as_string(&self) -> String {
        String::from_utf8_lossy(&self.data).trim_end().to_string()
    }
}

/// Condition handler registration.
#[derive(Debug, Clone)]
pub struct ConditionHandler {
    /// Condition name
    pub condition: String,
    /// Label/paragraph to branch to
    pub label: String,
}

/// Transaction context.
pub struct TransactionContext {
    /// Transaction ID
    pub transaction_id: String,
    /// Terminal ID
    pub terminal_id: Option<String>,
    /// User ID
    pub user_id: Option<String>,
    /// Start time
    pub start_time: u64,
    /// COMMAREA
    pub commarea: Option<Commarea>,
    /// Channel manager for channel/container support.
    pub channels: ChannelManager,
    /// Condition handlers
    pub handlers: Vec<ConditionHandler>,
    /// Abend handler label
    pub abend_handler: Option<String>,
}

impl TransactionContext {
    /// Create a new transaction context.
    pub fn new(transaction_id: &str) -> Self {
        Self {
            transaction_id: transaction_id.to_string(),
            terminal_id: None,
            user_id: None,
            start_time: 0,
            commarea: None,
            channels: ChannelManager::new(),
            handlers: Vec::new(),
            abend_handler: None,
        }
    }

    /// Set terminal ID.
    pub fn with_terminal(mut self, terminal_id: &str) -> Self {
        self.terminal_id = Some(terminal_id.to_string());
        self
    }

    /// Set user ID.
    pub fn with_user(mut self, user_id: &str) -> Self {
        self.user_id = Some(user_id.to_string());
        self
    }

    /// Set COMMAREA.
    pub fn with_commarea(mut self, commarea: Commarea) -> Self {
        self.commarea = Some(commarea);
        self
    }

    /// Register a condition handler.
    pub fn handle_condition(&mut self, condition: &str, label: &str) {
        // Remove existing handler for this condition
        self.handlers.retain(|h| h.condition != condition.to_uppercase());

        self.handlers.push(ConditionHandler {
            condition: condition.to_uppercase(),
            label: label.to_string(),
        });
    }

    /// Register abend handler.
    pub fn handle_abend(&mut self, label: &str) {
        self.abend_handler = Some(label.to_string());
    }

    /// Get handler for a condition.
    pub fn get_handler(&self, condition: &str) -> Option<&str> {
        self.handlers
            .iter()
            .find(|h| h.condition == condition.to_uppercase())
            .map(|h| h.label.as_str())
    }

    /// Ignore a condition.
    pub fn ignore_condition(&mut self, condition: &str) {
        self.handlers.retain(|h| h.condition != condition.to_uppercase());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_commarea_new() {
        let ca = Commarea::new(100);
        assert_eq!(ca.len(), 100);
        assert!(ca.data().iter().all(|&b| b == 0));
    }

    #[test]
    fn test_commarea_string() {
        let mut ca = Commarea::new(20);
        ca.set_string("Hello");
        assert_eq!(&ca.data()[..5], b"Hello");
        assert_eq!(ca.as_string(), "Hello");
    }

    #[test]
    fn test_transaction_context() {
        let ctx = TransactionContext::new("MENU")
            .with_terminal("T001")
            .with_user("USER1");

        assert_eq!(ctx.transaction_id, "MENU");
        assert_eq!(ctx.terminal_id, Some("T001".to_string()));
        assert_eq!(ctx.user_id, Some("USER1".to_string()));
    }

    #[test]
    fn test_condition_handler() {
        let mut ctx = TransactionContext::new("TEST");
        ctx.handle_condition("NOTFND", "NOT-FOUND-PARA");
        ctx.handle_condition("ERROR", "ERROR-PARA");

        assert_eq!(ctx.get_handler("NOTFND"), Some("NOT-FOUND-PARA"));
        assert_eq!(ctx.get_handler("ERROR"), Some("ERROR-PARA"));
        assert_eq!(ctx.get_handler("OTHER"), None);
    }

    #[test]
    fn test_ignore_condition() {
        let mut ctx = TransactionContext::new("TEST");
        ctx.handle_condition("NOTFND", "LABEL1");
        ctx.ignore_condition("NOTFND");

        assert_eq!(ctx.get_handler("NOTFND"), None);
    }
}
