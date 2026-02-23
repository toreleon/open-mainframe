//! Transient Data (TD) Queue implementation.
//!
//! TD queues are used for asynchronous processing. Records are written
//! to a queue and processed later by another transaction.
//!
//! The Destination Control Table (DCT) defines queue properties such as
//! destination type, trigger level, associated transaction, and backing
//! file path for extrapartition queues.

use std::collections::{HashMap, VecDeque};

/// Type of transient data destination.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum TdDestType {
    /// Intrapartition - internal queue within CICS
    #[default]
    Intrapartition,
    /// Extrapartition - external file or device
    Extrapartition,
}

/// Destination Control Table (DCT) entry.
///
/// Defines the properties of a TD queue as configured by the system
/// administrator. In IBM CICS, the DCT is a system table that controls
/// TD queue behavior.
#[derive(Debug, Clone)]
pub struct DctEntry {
    /// Queue name (up to 4 characters)
    pub queue_name: String,
    /// Destination type (intra or extra)
    pub dest_type: TdDestType,
    /// Trigger level: fire associated transaction after this many records
    pub trigger_level: Option<u32>,
    /// Transaction ID to trigger
    pub trigger_transid: Option<String>,
    /// Backing file path for extrapartition queues
    pub dataset_name: Option<String>,
    /// Whether the queue is currently enabled
    pub enabled: bool,
}

impl DctEntry {
    /// Create a new DCT entry for an intrapartition queue.
    pub fn intra(queue_name: &str) -> Self {
        Self {
            queue_name: queue_name.to_string(),
            dest_type: TdDestType::Intrapartition,
            trigger_level: None,
            trigger_transid: None,
            dataset_name: None,
            enabled: true,
        }
    }

    /// Create a new DCT entry for an extrapartition queue.
    pub fn extra(queue_name: &str, dataset_name: &str) -> Self {
        Self {
            queue_name: queue_name.to_string(),
            dest_type: TdDestType::Extrapartition,
            trigger_level: None,
            trigger_transid: None,
            dataset_name: Some(dataset_name.to_string()),
            enabled: true,
        }
    }

    /// Set trigger level and associated transaction.
    pub fn with_trigger(mut self, level: u32, transid: &str) -> Self {
        self.trigger_level = Some(level);
        self.trigger_transid = Some(transid.to_string());
        self
    }

    /// Convert DCT entry into a TdQueue.
    pub fn to_queue(&self) -> TdQueue {
        let mut queue = TdQueue::new(&self.queue_name, self.dest_type);
        if let Some(level) = self.trigger_level {
            if let Some(ref transid) = self.trigger_transid {
                queue = queue.with_trigger(level, transid);
            }
        }
        if let Some(ref dsn) = self.dataset_name {
            queue.dataset_name = Some(dsn.clone());
        }
        queue.enabled = self.enabled;
        queue
    }
}

/// A Transient Data queue.
#[derive(Debug, Clone)]
pub struct TdQueue {
    /// Queue name (up to 4 characters in CICS)
    pub name: String,
    /// Destination type
    pub dest_type: TdDestType,
    /// Trigger level (number of records before triggering)
    pub trigger_level: Option<u32>,
    /// Transaction to trigger
    pub trigger_transid: Option<String>,
    /// Backing file path for extrapartition queues
    pub dataset_name: Option<String>,
    /// Whether the queue is enabled
    pub enabled: bool,
    /// Records in the queue (for intrapartition)
    records: VecDeque<Vec<u8>>,
    /// Record count (for trigger checking)
    record_count: u32,
    /// Whether trigger has fired
    trigger_fired: bool,
}

impl TdQueue {
    /// Create a new TD queue.
    pub fn new(name: &str, dest_type: TdDestType) -> Self {
        Self {
            name: name.to_string(),
            dest_type,
            trigger_level: None,
            trigger_transid: None,
            dataset_name: None,
            enabled: true,
            records: VecDeque::new(),
            record_count: 0,
            trigger_fired: false,
        }
    }

    /// Create with trigger.
    pub fn with_trigger(mut self, level: u32, transid: &str) -> Self {
        self.trigger_level = Some(level);
        self.trigger_transid = Some(transid.to_string());
        self
    }

    /// Write a record to the queue.
    pub fn write(&mut self, data: Vec<u8>) -> TdResult<()> {
        if !self.enabled {
            return Err(TdError::QueueDisabled);
        }
        self.records.push_back(data);
        self.record_count += 1;
        Ok(())
    }

    /// Flush extrapartition records to the backing file.
    ///
    /// For extrapartition queues, this writes all buffered records to the
    /// dataset file. Records are appended line-by-line.
    pub fn flush_to_file(&mut self) -> TdResult<()> {
        if self.dest_type != TdDestType::Extrapartition {
            return Ok(());
        }
        let path = self.dataset_name.as_ref().ok_or(TdError::WriteError)?;
        use std::io::Write;
        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .map_err(|_| TdError::WriteError)?;
        while let Some(record) = self.records.pop_front() {
            file.write_all(&record).map_err(|_| TdError::WriteError)?;
            file.write_all(b"\n").map_err(|_| TdError::WriteError)?;
        }
        Ok(())
    }

    /// Read a record from the queue (destructive read).
    pub fn read(&mut self) -> TdResult<Vec<u8>> {
        if !self.enabled {
            return Err(TdError::QueueDisabled);
        }
        self.records.pop_front().ok_or(TdError::QueueEmpty)
    }

    /// Check if trigger should fire.
    pub fn should_trigger(&self) -> bool {
        if self.trigger_fired {
            return false;
        }
        match self.trigger_level {
            Some(level) => self.record_count >= level,
            None => false,
        }
    }

    /// Get triggered transaction ID.
    pub fn get_trigger_transid(&self) -> Option<&str> {
        self.trigger_transid.as_deref()
    }

    /// Mark trigger as fired.
    pub fn mark_triggered(&mut self) {
        self.trigger_fired = true;
    }

    /// Reset trigger.
    pub fn reset_trigger(&mut self) {
        self.trigger_fired = false;
        self.record_count = 0;
    }

    /// Get number of records in queue.
    pub fn num_records(&self) -> usize {
        self.records.len()
    }

    /// Check if queue is empty.
    pub fn is_empty(&self) -> bool {
        self.records.is_empty()
    }
}

/// Transient Data Queue Manager.
#[derive(Debug, Default)]
pub struct TdQueueManager {
    /// Queues indexed by name
    queues: HashMap<String, TdQueue>,
    /// Triggered transactions waiting to run
    pending_triggers: Vec<String>,
}

impl TdQueueManager {
    /// Create a new TD queue manager.
    pub fn new() -> Self {
        Self {
            queues: HashMap::new(),
            pending_triggers: Vec::new(),
        }
    }

    /// Load queues from a set of DCT entries.
    ///
    /// Each DCT entry defines one TD queue with its properties.
    pub fn load_dct(&mut self, entries: &[DctEntry]) {
        for entry in entries {
            self.queues.insert(entry.queue_name.clone(), entry.to_queue());
        }
    }

    /// Define a TD queue.
    pub fn define_queue(&mut self, queue: TdQueue) {
        self.queues.insert(queue.name.clone(), queue);
    }

    /// Write to a TD queue.
    pub fn writeq(&mut self, queue_name: &str, data: Vec<u8>) -> TdResult<()> {
        let queue = self.queues.get_mut(queue_name).ok_or(TdError::QueueNotFound)?;
        queue.write(data)?;

        // For extrapartition queues, flush to backing file
        if queue.dest_type == TdDestType::Extrapartition && queue.dataset_name.is_some() {
            queue.flush_to_file()?;
        }

        // Check for trigger
        if queue.should_trigger() {
            if let Some(transid) = queue.get_trigger_transid() {
                self.pending_triggers.push(transid.to_string());
                queue.mark_triggered();
            }
        }

        Ok(())
    }

    /// Read from a TD queue.
    pub fn readq(&mut self, queue_name: &str) -> TdResult<Vec<u8>> {
        let queue = self.queues.get_mut(queue_name).ok_or(TdError::QueueNotFound)?;
        queue.read()
    }

    /// Get pending triggered transactions.
    pub fn get_pending_triggers(&mut self) -> Vec<String> {
        std::mem::take(&mut self.pending_triggers)
    }

    /// Check if queue exists.
    pub fn queue_exists(&self, queue_name: &str) -> bool {
        self.queues.contains_key(queue_name)
    }

    /// Get queue info.
    pub fn get_queue(&self, queue_name: &str) -> Option<&TdQueue> {
        self.queues.get(queue_name)
    }

    /// Delete a queue.
    pub fn delete_queue(&mut self, queue_name: &str) -> TdResult<()> {
        self.queues.remove(queue_name).map(|_| ()).ok_or(TdError::QueueNotFound)
    }
}

/// TD Queue errors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TdError {
    /// Queue not found (QIDERR)
    QueueNotFound,
    /// Queue is empty (QZERO)
    QueueEmpty,
    /// Queue is disabled
    QueueDisabled,
    /// Write error
    WriteError,
    /// Read error
    ReadError,
    /// Queue is full
    QueueFull,
}

impl std::fmt::Display for TdError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TdError::QueueNotFound => write!(f, "Queue not found (QIDERR)"),
            TdError::QueueEmpty => write!(f, "Queue is empty (QZERO)"),
            TdError::QueueDisabled => write!(f, "Queue is disabled"),
            TdError::WriteError => write!(f, "Write error"),
            TdError::ReadError => write!(f, "Read error"),
            TdError::QueueFull => write!(f, "Queue is full"),
        }
    }
}

impl std::error::Error for TdError {}

/// Result type for TD operations.
pub type TdResult<T> = Result<T, TdError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_td_queue_basic() {
        let mut queue = TdQueue::new("TDQ1", TdDestType::Intrapartition);

        // Write records
        queue.write(b"Record 1".to_vec()).unwrap();
        queue.write(b"Record 2".to_vec()).unwrap();

        assert_eq!(queue.num_records(), 2);

        // Read records (FIFO)
        let rec1 = queue.read().unwrap();
        assert_eq!(rec1, b"Record 1");

        let rec2 = queue.read().unwrap();
        assert_eq!(rec2, b"Record 2");

        assert!(queue.is_empty());
    }

    #[test]
    fn test_td_queue_trigger() {
        let queue = TdQueue::new("TDQ2", TdDestType::Intrapartition)
            .with_trigger(3, "TRIG");

        let mut mgr = TdQueueManager::new();
        mgr.define_queue(queue);

        // Write 2 records - no trigger yet
        mgr.writeq("TDQ2", b"Rec1".to_vec()).unwrap();
        mgr.writeq("TDQ2", b"Rec2".to_vec()).unwrap();
        assert!(mgr.get_pending_triggers().is_empty());

        // Write 3rd record - trigger fires
        mgr.writeq("TDQ2", b"Rec3".to_vec()).unwrap();
        let triggers = mgr.get_pending_triggers();
        assert_eq!(triggers.len(), 1);
        assert_eq!(triggers[0], "TRIG");
    }

    #[test]
    fn test_td_queue_manager() {
        let mut mgr = TdQueueManager::new();

        // Define queue
        let queue = TdQueue::new("MYQUEUE", TdDestType::Intrapartition);
        mgr.define_queue(queue);

        // Write and read
        mgr.writeq("MYQUEUE", b"Test data".to_vec()).unwrap();
        let data = mgr.readq("MYQUEUE").unwrap();
        assert_eq!(data, b"Test data");
    }

    #[test]
    fn test_td_queue_empty() {
        let mut mgr = TdQueueManager::new();
        mgr.define_queue(TdQueue::new("EMPTY", TdDestType::Intrapartition));

        let result = mgr.readq("EMPTY");
        assert!(matches!(result, Err(TdError::QueueEmpty)));
    }

    #[test]
    fn test_td_queue_not_found() {
        let mut mgr = TdQueueManager::new();
        let result = mgr.writeq("NOQUEUE", b"data".to_vec());
        assert!(matches!(result, Err(TdError::QueueNotFound)));
    }

    // === Story 201.2: DCT Configuration ===

    #[test]
    fn test_dct_entry_intra() {
        let entry = DctEntry::intra("CSSL")
            .with_trigger(100, "CSSLRPT");
        assert_eq!(entry.queue_name, "CSSL");
        assert_eq!(entry.dest_type, TdDestType::Intrapartition);
        assert_eq!(entry.trigger_level, Some(100));
        assert_eq!(entry.trigger_transid.as_deref(), Some("CSSLRPT"));
        assert!(entry.enabled);
    }

    #[test]
    fn test_dct_entry_extra() {
        let entry = DctEntry::extra("EXTQ", "/data/output.dat");
        assert_eq!(entry.queue_name, "EXTQ");
        assert_eq!(entry.dest_type, TdDestType::Extrapartition);
        assert_eq!(entry.dataset_name.as_deref(), Some("/data/output.dat"));
    }

    #[test]
    fn test_dct_to_queue() {
        let entry = DctEntry::intra("CSSL")
            .with_trigger(100, "CSSLRPT");
        let queue = entry.to_queue();
        assert_eq!(queue.name, "CSSL");
        assert_eq!(queue.trigger_level, Some(100));
        assert_eq!(queue.trigger_transid.as_deref(), Some("CSSLRPT"));
    }

    #[test]
    fn test_load_dct() {
        let entries = vec![
            DctEntry::intra("CSSL").with_trigger(100, "CSSLRPT"),
            DctEntry::extra("EXTQ", "/data/output.dat"),
        ];

        let mut mgr = TdQueueManager::new();
        mgr.load_dct(&entries);

        assert!(mgr.queue_exists("CSSL"));
        assert!(mgr.queue_exists("EXTQ"));

        let cssl = mgr.get_queue("CSSL").unwrap();
        assert_eq!(cssl.trigger_level, Some(100));

        let extq = mgr.get_queue("EXTQ").unwrap();
        assert_eq!(extq.dest_type, TdDestType::Extrapartition);
        assert_eq!(extq.dataset_name.as_deref(), Some("/data/output.dat"));
    }

    #[test]
    fn test_dct_trigger_fires_at_level() {
        let entries = vec![
            DctEntry::intra("CSSL").with_trigger(3, "CSSLRPT"),
        ];

        let mut mgr = TdQueueManager::new();
        mgr.load_dct(&entries);

        // Write 2 records — no trigger
        mgr.writeq("CSSL", b"rec1".to_vec()).unwrap();
        mgr.writeq("CSSL", b"rec2".to_vec()).unwrap();
        assert!(mgr.get_pending_triggers().is_empty());

        // Write 3rd record — trigger fires
        mgr.writeq("CSSL", b"rec3".to_vec()).unwrap();
        let triggers = mgr.get_pending_triggers();
        assert_eq!(triggers.len(), 1);
        assert_eq!(triggers[0], "CSSLRPT");
    }

    #[test]
    fn test_extrapartition_writeq() {
        use std::io::Read;

        // Write to a temp file
        let dir = std::env::temp_dir();
        let path = dir.join("test_td_extra.dat");
        let path_str = path.to_string_lossy().to_string();

        // Clean up any previous run
        let _ = std::fs::remove_file(&path);

        let entries = vec![DctEntry::extra("EXTQ", &path_str)];
        let mut mgr = TdQueueManager::new();
        mgr.load_dct(&entries);

        mgr.writeq("EXTQ", b"Line 1".to_vec()).unwrap();
        mgr.writeq("EXTQ", b"Line 2".to_vec()).unwrap();

        // Read back from file
        let mut contents = String::new();
        std::fs::File::open(&path)
            .unwrap()
            .read_to_string(&mut contents)
            .unwrap();
        assert!(contents.contains("Line 1"));
        assert!(contents.contains("Line 2"));

        // Clean up
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn test_disabled_queue_rejects_operations() {
        let mut entry = DctEntry::intra("DISQ");
        entry.enabled = false;
        let mut mgr = TdQueueManager::new();
        mgr.load_dct(&[entry]);

        let result = mgr.writeq("DISQ", b"data".to_vec());
        assert!(matches!(result, Err(TdError::QueueDisabled)));
    }
}
