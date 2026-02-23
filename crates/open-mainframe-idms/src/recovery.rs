//! IDMS-109: Recovery & Operations (5 stories).
//!
//! Provides journaling, rollback, and restart capabilities for IDMS.
//! The journal records before/after images and checkpoint records.
//! Rollback uses the journal to undo incomplete transactions.
//! Warm start replays the journal after a failure.  Cold start
//! reloads the database from a backup.

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Journal record types
// ---------------------------------------------------------------------------

/// Type of journal record.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum JournalRecordType {
    /// Before-image of a record (before modification).
    BeforeImage,
    /// After-image of a record (after modification).
    AfterImage,
    /// Checkpoint (transaction boundary marker).
    Checkpoint,
    /// Begin transaction marker.
    BeginTransaction,
    /// Commit transaction marker.
    CommitTransaction,
    /// Abort transaction marker.
    AbortTransaction,
}

/// A single journal record.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct JournalRecord {
    /// Sequence number within the journal.
    pub sequence: u64,
    /// Record type.
    pub record_type: JournalRecordType,
    /// Transaction ID.
    pub transaction_id: u64,
    /// Database key of the affected record (if applicable).
    pub dbkey: Option<u64>,
    /// Data payload (before or after image bytes, represented as string).
    pub data: Option<String>,
    /// Timestamp (simplified as u64 counter).
    pub timestamp: u64,
}

// ---------------------------------------------------------------------------
//  Journal manager
// ---------------------------------------------------------------------------

/// Manages the IDMS journal (log) for recovery purposes.
///
/// Records before/after images and transaction boundaries.
#[derive(Debug, Default)]
pub struct JournalManager {
    /// All journal records in order.
    records: Vec<JournalRecord>,
    /// Next sequence number.
    next_sequence: u64,
    /// Current timestamp counter.
    timestamp: u64,
}

impl JournalManager {
    /// Create a new journal manager.
    pub fn new() -> Self {
        Self {
            records: Vec::new(),
            next_sequence: 1,
            timestamp: 1,
        }
    }

    /// Write a journal record.
    pub fn write(
        &mut self,
        record_type: JournalRecordType,
        transaction_id: u64,
        dbkey: Option<u64>,
        data: Option<String>,
    ) -> u64 {
        let seq = self.next_sequence;
        self.next_sequence += 1;
        let ts = self.timestamp;
        self.timestamp += 1;
        self.records.push(JournalRecord {
            sequence: seq,
            record_type,
            transaction_id,
            dbkey,
            data,
            timestamp: ts,
        });
        seq
    }

    /// Write a before-image.
    pub fn write_before_image(
        &mut self,
        transaction_id: u64,
        dbkey: u64,
        data: &str,
    ) -> u64 {
        self.write(
            JournalRecordType::BeforeImage,
            transaction_id,
            Some(dbkey),
            Some(data.to_string()),
        )
    }

    /// Write an after-image.
    pub fn write_after_image(
        &mut self,
        transaction_id: u64,
        dbkey: u64,
        data: &str,
    ) -> u64 {
        self.write(
            JournalRecordType::AfterImage,
            transaction_id,
            Some(dbkey),
            Some(data.to_string()),
        )
    }

    /// Write a checkpoint record.
    pub fn write_checkpoint(&mut self, transaction_id: u64) -> u64 {
        self.write(JournalRecordType::Checkpoint, transaction_id, None, None)
    }

    /// Write a begin-transaction record.
    pub fn begin_transaction(&mut self, transaction_id: u64) -> u64 {
        self.write(
            JournalRecordType::BeginTransaction,
            transaction_id,
            None,
            None,
        )
    }

    /// Write a commit-transaction record.
    pub fn commit_transaction(&mut self, transaction_id: u64) -> u64 {
        self.write(
            JournalRecordType::CommitTransaction,
            transaction_id,
            None,
            None,
        )
    }

    /// Write an abort-transaction record.
    pub fn abort_transaction(&mut self, transaction_id: u64) -> u64 {
        self.write(
            JournalRecordType::AbortTransaction,
            transaction_id,
            None,
            None,
        )
    }

    /// Return all journal records for a given transaction.
    pub fn records_for_transaction(&self, transaction_id: u64) -> Vec<&JournalRecord> {
        self.records
            .iter()
            .filter(|r| r.transaction_id == transaction_id)
            .collect()
    }

    /// Return the total number of journal records.
    pub fn record_count(&self) -> usize {
        self.records.len()
    }

    /// Return all records (for recovery processing).
    pub fn all_records(&self) -> &[JournalRecord] {
        &self.records
    }
}

// ---------------------------------------------------------------------------
//  Rollback manager
// ---------------------------------------------------------------------------

/// Manages transaction rollback using journal before-images.
///
/// When a transaction must be rolled back, the rollback manager reads
/// the journal backwards and restores before-images.
#[derive(Debug, Default)]
pub struct RollbackManager {
    /// Records that were rolled back (dbkey -> restored data).
    pub rolled_back: HashMap<u64, String>,
}

impl RollbackManager {
    /// Create a new rollback manager.
    pub fn new() -> Self {
        Self::default()
    }

    /// Perform rollback for a transaction using the journal.
    ///
    /// Returns the number of records restored.
    pub fn rollback(
        &mut self,
        journal: &JournalManager,
        transaction_id: u64,
    ) -> usize {
        let records = journal.records_for_transaction(transaction_id);
        let mut count = 0;

        // Process in reverse order (most recent first).
        for rec in records.iter().rev() {
            if rec.record_type == JournalRecordType::BeforeImage {
                if let (Some(dbkey), Some(data)) = (rec.dbkey, rec.data.as_ref()) {
                    self.rolled_back.insert(dbkey, data.clone());
                    count += 1;
                }
            }
        }
        count
    }

    /// Check if a specific dbkey was rolled back.
    pub fn was_rolled_back(&self, dbkey: u64) -> bool {
        self.rolled_back.contains_key(&dbkey)
    }

    /// Get the restored data for a rolled-back record.
    pub fn restored_data(&self, dbkey: u64) -> Option<&str> {
        self.rolled_back.get(&dbkey).map(String::as_str)
    }
}

// ---------------------------------------------------------------------------
//  Warm start
// ---------------------------------------------------------------------------

/// Warm start recovery -- replays the journal from the last checkpoint
/// to restore committed transactions and roll back uncommitted ones.
#[derive(Debug, Default)]
pub struct WarmStart {
    /// Transactions that were committed (should be redone).
    pub committed: Vec<u64>,
    /// Transactions that were uncommitted (should be undone).
    pub uncommitted: Vec<u64>,
}

impl WarmStart {
    /// Create a new warm start processor.
    pub fn new() -> Self {
        Self::default()
    }

    /// Analyze the journal and determine which transactions need redo/undo.
    pub fn analyze(&mut self, journal: &JournalManager) {
        let mut began = std::collections::HashSet::new();
        let mut committed = std::collections::HashSet::new();

        for rec in journal.all_records() {
            match rec.record_type {
                JournalRecordType::BeginTransaction => {
                    began.insert(rec.transaction_id);
                }
                JournalRecordType::CommitTransaction => {
                    committed.insert(rec.transaction_id);
                }
                _ => {}
            }
        }

        self.committed = committed.iter().copied().collect();
        self.committed.sort();
        self.uncommitted = began
            .difference(&committed)
            .copied()
            .collect();
        self.uncommitted.sort();
    }

    /// Return whether a transaction needs redo (was committed).
    pub fn needs_redo(&self, transaction_id: u64) -> bool {
        self.committed.contains(&transaction_id)
    }

    /// Return whether a transaction needs undo (was not committed).
    pub fn needs_undo(&self, transaction_id: u64) -> bool {
        self.uncommitted.contains(&transaction_id)
    }
}

// ---------------------------------------------------------------------------
//  Cold start
// ---------------------------------------------------------------------------

/// Cold start -- full database reload from backup.
///
/// Used when the journal is insufficient for warm start recovery.
#[derive(Debug, Default)]
pub struct ColdStart {
    /// Backup source identifier.
    pub backup_source: Option<String>,
    /// Whether the cold start completed.
    pub completed: bool,
    /// Number of records reloaded.
    pub records_reloaded: u64,
}

impl ColdStart {
    /// Create a new cold start processor.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the backup source.
    pub fn set_backup_source(&mut self, source: &str) {
        self.backup_source = Some(source.to_string());
    }

    /// Simulate a cold start reload.
    pub fn execute(&mut self, record_count: u64) -> Result<(), RecoveryError> {
        if self.backup_source.is_none() {
            return Err(RecoveryError::NoBackupSource);
        }
        self.records_reloaded = record_count;
        self.completed = true;
        Ok(())
    }
}

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors from recovery operations.
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum RecoveryError {
    /// No backup source configured for cold start.
    #[error("no backup source configured")]
    NoBackupSource,
    /// Journal corrupted or unreadable.
    #[error("journal error: {0}")]
    JournalError(String),
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn journal_write_and_read() {
        let mut jm = JournalManager::new();
        jm.begin_transaction(1);
        jm.write_before_image(1, 100, "old-data");
        jm.write_after_image(1, 100, "new-data");
        jm.commit_transaction(1);

        assert_eq!(jm.record_count(), 4);
        let tx_records = jm.records_for_transaction(1);
        assert_eq!(tx_records.len(), 4);
    }

    #[test]
    fn journal_checkpoint() {
        let mut jm = JournalManager::new();
        let seq = jm.write_checkpoint(0);
        assert_eq!(seq, 1);
        assert_eq!(jm.record_count(), 1);
    }

    #[test]
    fn rollback_restores_before_images() {
        let mut jm = JournalManager::new();
        jm.begin_transaction(1);
        jm.write_before_image(1, 100, "original-100");
        jm.write_after_image(1, 100, "modified-100");
        jm.write_before_image(1, 200, "original-200");
        jm.write_after_image(1, 200, "modified-200");

        let mut rm = RollbackManager::new();
        let count = rm.rollback(&jm, 1);
        assert_eq!(count, 2);
        assert!(rm.was_rolled_back(100));
        assert_eq!(rm.restored_data(100), Some("original-100"));
        assert_eq!(rm.restored_data(200), Some("original-200"));
    }

    #[test]
    fn rollback_no_records() {
        let jm = JournalManager::new();
        let mut rm = RollbackManager::new();
        let count = rm.rollback(&jm, 999);
        assert_eq!(count, 0);
    }

    #[test]
    fn warm_start_analysis() {
        let mut jm = JournalManager::new();
        // Transaction 1: committed.
        jm.begin_transaction(1);
        jm.write_before_image(1, 100, "data");
        jm.commit_transaction(1);

        // Transaction 2: uncommitted (in-flight).
        jm.begin_transaction(2);
        jm.write_before_image(2, 200, "data");
        // No commit for tx 2.

        let mut ws = WarmStart::new();
        ws.analyze(&jm);

        assert!(ws.needs_redo(1));
        assert!(!ws.needs_undo(1));
        assert!(ws.needs_undo(2));
        assert!(!ws.needs_redo(2));
    }

    #[test]
    fn cold_start_success() {
        let mut cs = ColdStart::new();
        cs.set_backup_source("BACKUP.DATASET");
        cs.execute(1000).unwrap();
        assert!(cs.completed);
        assert_eq!(cs.records_reloaded, 1000);
    }

    #[test]
    fn cold_start_no_source() {
        let mut cs = ColdStart::new();
        let err = cs.execute(100).unwrap_err();
        assert!(matches!(err, RecoveryError::NoBackupSource));
    }

    #[test]
    fn journal_abort_transaction() {
        let mut jm = JournalManager::new();
        jm.begin_transaction(1);
        jm.abort_transaction(1);
        assert_eq!(jm.record_count(), 2);
        let recs = jm.records_for_transaction(1);
        assert_eq!(recs[1].record_type, JournalRecordType::AbortTransaction);
    }
}
