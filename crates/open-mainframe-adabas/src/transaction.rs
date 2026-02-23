//! ADA-107: Transaction Management (5 stories).
//!
//! Provides ET (end transaction/commit), BT (backout transaction/rollback),
//! hold queues, command-level protection, and transaction logging.

use std::collections::{HashMap, HashSet};

use crate::storage::Isn;
use crate::AdabasError;

// ── TransactionId ──────────────────────────────────────────────────

/// Unique transaction identifier.
pub type TransactionId = u64;

// ── TransactionState ───────────────────────────────────────────────

/// The current state of a transaction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TransactionState {
    /// Transaction is active (in progress).
    Active,
    /// Transaction has been committed (ET).
    Committed,
    /// Transaction has been rolled back (BT).
    RolledBack,
}

// ── TransactionLogEntry ────────────────────────────────────────────

/// An entry in the transaction log recording a before/after image.
#[derive(Debug, Clone)]
pub struct TransactionLogEntry {
    /// The file number.
    pub file_number: u16,
    /// The ISN affected.
    pub isn: Isn,
    /// The operation type.
    pub operation: LogOperation,
    /// Before-image (record data before modification; None for inserts).
    pub before_image: Option<Vec<u8>>,
    /// After-image (record data after modification; None for deletes).
    pub after_image: Option<Vec<u8>>,
}

/// The type of operation logged.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogOperation {
    /// A new record was inserted.
    Insert,
    /// An existing record was updated.
    Update,
    /// A record was deleted.
    Delete,
}

// ── TransactionLog ─────────────────────────────────────────────────

/// Transaction log: before/after images for rollback support.
#[derive(Debug, Clone)]
pub struct TransactionLog {
    /// Entries in chronological order.
    entries: Vec<TransactionLogEntry>,
    /// The transaction this log belongs to.
    pub transaction_id: TransactionId,
}

impl TransactionLog {
    /// Create a new transaction log.
    pub fn new(transaction_id: TransactionId) -> Self {
        Self {
            entries: Vec::new(),
            transaction_id,
        }
    }

    /// Log an insert operation.
    pub fn log_insert(&mut self, file_number: u16, isn: Isn, after_image: Vec<u8>) {
        self.entries.push(TransactionLogEntry {
            file_number,
            isn,
            operation: LogOperation::Insert,
            before_image: None,
            after_image: Some(after_image),
        });
    }

    /// Log an update operation.
    pub fn log_update(
        &mut self,
        file_number: u16,
        isn: Isn,
        before_image: Vec<u8>,
        after_image: Vec<u8>,
    ) {
        self.entries.push(TransactionLogEntry {
            file_number,
            isn,
            operation: LogOperation::Update,
            before_image: Some(before_image),
            after_image: Some(after_image),
        });
    }

    /// Log a delete operation.
    pub fn log_delete(&mut self, file_number: u16, isn: Isn, before_image: Vec<u8>) {
        self.entries.push(TransactionLogEntry {
            file_number,
            isn,
            operation: LogOperation::Delete,
            before_image: Some(before_image),
            after_image: None,
        });
    }

    /// Return the entries in reverse order (for rollback).
    pub fn entries_for_rollback(&self) -> impl Iterator<Item = &TransactionLogEntry> {
        self.entries.iter().rev()
    }

    /// Return the number of log entries.
    pub fn entry_count(&self) -> usize {
        self.entries.len()
    }

    /// Return all log entries.
    pub fn entries(&self) -> &[TransactionLogEntry] {
        &self.entries
    }
}

// ── HoldQueue ──────────────────────────────────────────────────────

/// Records held for update during a transaction.
#[derive(Debug, Clone)]
pub struct HoldQueue {
    /// Set of (file_number, ISN) pairs currently held.
    held: HashSet<(u16, Isn)>,
    /// Maximum number of records that can be held.
    max_holds: usize,
}

impl HoldQueue {
    /// Create a new hold queue with a maximum capacity.
    pub fn new(max_holds: usize) -> Self {
        Self {
            held: HashSet::new(),
            max_holds,
        }
    }

    /// Place a hold on a record.
    pub fn hold(&mut self, file_number: u16, isn: Isn) -> Result<(), AdabasError> {
        if self.held.len() >= self.max_holds {
            return Err(AdabasError::HoldQueueFull {
                max: self.max_holds,
            });
        }
        self.held.insert((file_number, isn));
        Ok(())
    }

    /// Release a hold on a record.
    pub fn release(&mut self, file_number: u16, isn: Isn) {
        self.held.remove(&(file_number, isn));
    }

    /// Check whether a record is currently held.
    pub fn is_held(&self, file_number: u16, isn: Isn) -> bool {
        self.held.contains(&(file_number, isn))
    }

    /// Release all holds (on ET or BT).
    pub fn release_all(&mut self) {
        self.held.clear();
    }

    /// Return the number of records currently held.
    pub fn held_count(&self) -> usize {
        self.held.len()
    }
}

// ── ClpNumber ──────────────────────────────────────────────────────

/// Command-level protection number tracking.
///
/// Each user session receives a CLP number; ADABAS checks this number
/// to ensure that update sequences are valid (prevents stale reads
/// from being used for updates).
#[derive(Debug, Clone)]
pub struct ClpNumber {
    /// Current CLP values per session.
    sessions: HashMap<u64, u64>,
}

impl ClpNumber {
    /// Create a new CLP tracker.
    pub fn new() -> Self {
        Self {
            sessions: HashMap::new(),
        }
    }

    /// Get the current CLP for a session.
    pub fn get(&self, session_id: u64) -> u64 {
        self.sessions.get(&session_id).copied().unwrap_or(0)
    }

    /// Increment and return the new CLP for a session.
    pub fn increment(&mut self, session_id: u64) -> u64 {
        let entry = self.sessions.entry(session_id).or_insert(0);
        *entry += 1;
        *entry
    }

    /// Remove a session's CLP tracking.
    pub fn remove_session(&mut self, session_id: u64) {
        self.sessions.remove(&session_id);
    }
}

impl Default for ClpNumber {
    fn default() -> Self {
        Self::new()
    }
}

// ── TransactionManager ─────────────────────────────────────────────

/// Manages transactions with ET (commit) and BT (rollback) support.
#[derive(Debug)]
pub struct TransactionManager {
    /// Next transaction ID.
    next_id: TransactionId,
    /// Active transaction logs, keyed by transaction ID.
    active_logs: HashMap<TransactionId, TransactionLog>,
    /// Hold queue for the current session.
    pub hold_queue: HoldQueue,
    /// CLP number tracking.
    pub clp: ClpNumber,
    /// States of transactions.
    states: HashMap<TransactionId, TransactionState>,
}

impl TransactionManager {
    /// Create a new transaction manager.
    pub fn new(max_holds: usize) -> Self {
        Self {
            next_id: 1,
            active_logs: HashMap::new(),
            hold_queue: HoldQueue::new(max_holds),
            clp: ClpNumber::new(),
            states: HashMap::new(),
        }
    }

    /// Begin a new transaction, returning the transaction ID.
    pub fn begin(&mut self) -> TransactionId {
        let id = self.next_id;
        self.next_id += 1;
        self.active_logs.insert(id, TransactionLog::new(id));
        self.states.insert(id, TransactionState::Active);
        id
    }

    /// Get the transaction log for an active transaction.
    pub fn log(&mut self, txn_id: TransactionId) -> Result<&mut TransactionLog, AdabasError> {
        self.active_logs
            .get_mut(&txn_id)
            .ok_or(AdabasError::TransactionNotFound { id: txn_id })
    }

    /// ET (End Transaction): commit the transaction.
    pub fn commit(&mut self, txn_id: TransactionId) -> Result<TransactionLog, AdabasError> {
        let log = self
            .active_logs
            .remove(&txn_id)
            .ok_or(AdabasError::TransactionNotFound { id: txn_id })?;
        self.states.insert(txn_id, TransactionState::Committed);
        self.hold_queue.release_all();
        Ok(log)
    }

    /// BT (Backout Transaction): rollback the transaction, returning the log
    /// entries in reverse order for undo processing.
    pub fn rollback(&mut self, txn_id: TransactionId) -> Result<TransactionLog, AdabasError> {
        let log = self
            .active_logs
            .remove(&txn_id)
            .ok_or(AdabasError::TransactionNotFound { id: txn_id })?;
        self.states.insert(txn_id, TransactionState::RolledBack);
        self.hold_queue.release_all();
        Ok(log)
    }

    /// Get the state of a transaction.
    pub fn state(&self, txn_id: TransactionId) -> Option<TransactionState> {
        self.states.get(&txn_id).copied()
    }

    /// Whether a transaction is currently active.
    pub fn is_active(&self, txn_id: TransactionId) -> bool {
        self.states.get(&txn_id) == Some(&TransactionState::Active)
    }
}

// ── Tests ──────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn transaction_begin_commit() {
        let mut mgr = TransactionManager::new(100);
        let txn = mgr.begin();
        assert!(mgr.is_active(txn));

        mgr.log(txn).unwrap().log_insert(1, 1, b"data".to_vec());
        let log = mgr.commit(txn).unwrap();
        assert_eq!(log.entry_count(), 1);
        assert_eq!(mgr.state(txn), Some(TransactionState::Committed));
        assert!(!mgr.is_active(txn));
    }

    #[test]
    fn transaction_rollback() {
        let mut mgr = TransactionManager::new(100);
        let txn = mgr.begin();
        mgr.log(txn).unwrap().log_insert(1, 1, b"data".to_vec());
        mgr.log(txn)
            .unwrap()
            .log_update(1, 2, b"old".to_vec(), b"new".to_vec());

        let log = mgr.rollback(txn).unwrap();
        assert_eq!(log.entry_count(), 2);
        assert_eq!(mgr.state(txn), Some(TransactionState::RolledBack));
    }

    #[test]
    fn transaction_log_entries_for_rollback() {
        let mut log = TransactionLog::new(1);
        log.log_insert(1, 1, b"a".to_vec());
        log.log_insert(1, 2, b"b".to_vec());
        log.log_delete(1, 3, b"c".to_vec());

        let rollback: Vec<_> = log.entries_for_rollback().collect();
        assert_eq!(rollback.len(), 3);
        assert_eq!(rollback[0].operation, LogOperation::Delete);
        assert_eq!(rollback[1].isn, 2);
        assert_eq!(rollback[2].isn, 1);
    }

    #[test]
    fn hold_queue_basic() {
        let mut hq = HoldQueue::new(10);
        hq.hold(1, 42).unwrap();
        assert!(hq.is_held(1, 42));
        assert!(!hq.is_held(1, 43));
        assert_eq!(hq.held_count(), 1);
        hq.release(1, 42);
        assert!(!hq.is_held(1, 42));
    }

    #[test]
    fn hold_queue_full() {
        let mut hq = HoldQueue::new(2);
        hq.hold(1, 1).unwrap();
        hq.hold(1, 2).unwrap();
        let err = hq.hold(1, 3);
        assert!(err.is_err());
    }

    #[test]
    fn hold_queue_release_all() {
        let mut hq = HoldQueue::new(10);
        hq.hold(1, 1).unwrap();
        hq.hold(1, 2).unwrap();
        hq.release_all();
        assert_eq!(hq.held_count(), 0);
    }

    #[test]
    fn clp_number_tracking() {
        let mut clp = ClpNumber::new();
        assert_eq!(clp.get(1), 0);
        assert_eq!(clp.increment(1), 1);
        assert_eq!(clp.increment(1), 2);
        assert_eq!(clp.get(1), 2);
        clp.remove_session(1);
        assert_eq!(clp.get(1), 0);
    }

    #[test]
    fn transaction_not_found() {
        let mut mgr = TransactionManager::new(100);
        assert!(mgr.commit(999).is_err());
        assert!(mgr.rollback(999).is_err());
    }

    #[test]
    fn transaction_log_operations() {
        let mut log = TransactionLog::new(1);
        log.log_insert(1, 1, b"new".to_vec());
        log.log_update(1, 2, b"old".to_vec(), b"updated".to_vec());
        log.log_delete(1, 3, b"removed".to_vec());
        assert_eq!(log.entry_count(), 3);

        let entries = log.entries();
        assert_eq!(entries[0].operation, LogOperation::Insert);
        assert!(entries[0].before_image.is_none());
        assert_eq!(entries[0].after_image.as_deref(), Some(b"new".as_slice()));

        assert_eq!(entries[1].operation, LogOperation::Update);
        assert_eq!(
            entries[1].before_image.as_deref(),
            Some(b"old".as_slice())
        );
        assert_eq!(
            entries[1].after_image.as_deref(),
            Some(b"updated".as_slice())
        );

        assert_eq!(entries[2].operation, LogOperation::Delete);
        assert_eq!(
            entries[2].before_image.as_deref(),
            Some(b"removed".as_slice())
        );
        assert!(entries[2].after_image.is_none());
    }

    #[test]
    fn hold_releases_on_commit() {
        let mut mgr = TransactionManager::new(100);
        let txn = mgr.begin();
        mgr.hold_queue.hold(1, 10).unwrap();
        assert!(mgr.hold_queue.is_held(1, 10));
        mgr.commit(txn).unwrap();
        assert!(!mgr.hold_queue.is_held(1, 10));
    }

    #[test]
    fn hold_releases_on_rollback() {
        let mut mgr = TransactionManager::new(100);
        let txn = mgr.begin();
        mgr.hold_queue.hold(1, 10).unwrap();
        mgr.rollback(txn).unwrap();
        assert!(!mgr.hold_queue.is_held(1, 10));
    }
}
