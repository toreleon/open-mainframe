//! IMS-TM108: Fast Path.
//!
//! Fast Path provides expedited message handling (EMH) for
//! high-volume, low-overhead transactions. It uses IFP (IMS Fast Path)
//! regions and can be paired with DEDBs (Data Entry Databases) for
//! fast, direct data access.

use crate::{ImsError, ImsResult, StatusCode};

// ---------------------------------------------------------------------------
// EMH Queue
// ---------------------------------------------------------------------------

/// Priority level for EMH messages.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum EmhPriority {
    /// Normal priority.
    Normal,
    /// High priority -- bypasses normal queue ordering.
    High,
}

/// An entry in the Expedited Message Handler queue.
#[derive(Debug, Clone)]
pub struct EmhEntry {
    /// Transaction code.
    pub trancode: String,
    /// Message data.
    pub data: Vec<u8>,
    /// Priority.
    pub priority: EmhPriority,
    /// Sequence number (for FIFO within same priority).
    pub sequence: u64,
}

/// Expedited Message Handler queue.
///
/// The EMH queue gives Fast Path transactions priority access,
/// bypassing the normal IMS message queue.
#[derive(Debug)]
pub struct EmhQueue {
    /// Queued entries.
    entries: Vec<EmhEntry>,
    /// Next sequence number.
    next_seq: u64,
}

impl EmhQueue {
    /// Create a new empty EMH queue.
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            next_seq: 1,
        }
    }

    /// Enqueue a message.
    pub fn enqueue(&mut self, trancode: &str, data: Vec<u8>, priority: EmhPriority) {
        self.entries.push(EmhEntry {
            trancode: trancode.to_string(),
            data,
            priority,
            sequence: self.next_seq,
        });
        self.next_seq += 1;
        // Sort: high priority first, then by sequence
        self.entries.sort_by(|a, b| {
            b.priority
                .cmp(&a.priority)
                .then(a.sequence.cmp(&b.sequence))
        });
    }

    /// Dequeue the next message.
    pub fn dequeue(&mut self) -> Option<EmhEntry> {
        if self.entries.is_empty() {
            None
        } else {
            Some(self.entries.remove(0))
        }
    }

    /// Return the number of queued messages.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Check if the queue is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Peek at the next entry without removing it.
    pub fn peek(&self) -> Option<&EmhEntry> {
        self.entries.first()
    }
}

impl Default for EmhQueue {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// IFP Region
// ---------------------------------------------------------------------------

/// State of an IFP region.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IfpState {
    /// Region is stopped.
    Stopped,
    /// Region is waiting for EMH messages.
    Waiting,
    /// Region is processing a message.
    Processing,
}

/// IMS Fast Path region.
///
/// IFP regions process messages from the EMH queue. They are optimised
/// for short, high-frequency transactions.
#[derive(Debug)]
pub struct IfpRegion {
    /// Region identifier.
    pub id: String,
    /// Current state.
    state: IfpState,
    /// Number of transactions processed.
    processed_count: u64,
}

impl IfpRegion {
    /// Create a new IFP region.
    pub fn new(id: &str) -> Self {
        Self {
            id: id.to_string(),
            state: IfpState::Stopped,
            processed_count: 0,
        }
    }

    /// Start the region.
    pub fn start(&mut self) {
        self.state = IfpState::Waiting;
    }

    /// Stop the region.
    pub fn stop(&mut self) {
        self.state = IfpState::Stopped;
    }

    /// Process the next message from an EMH queue.
    ///
    /// Returns the processed entry, or `None` if the queue is empty.
    pub fn process_next(&mut self, queue: &mut EmhQueue) -> ImsResult<Option<EmhEntry>> {
        if self.state == IfpState::Stopped {
            return Err(ImsError::DliError {
                status: StatusCode::AD,
            });
        }
        match queue.dequeue() {
            Some(entry) => {
                self.state = IfpState::Processing;
                self.processed_count += 1;
                self.state = IfpState::Waiting;
                Ok(Some(entry))
            }
            None => Ok(None),
        }
    }

    /// Return the current state.
    pub fn state(&self) -> IfpState {
        self.state
    }

    /// Return the number of processed transactions.
    pub fn processed_count(&self) -> u64 {
        self.processed_count
    }
}

// ---------------------------------------------------------------------------
// DEDB Integration
// ---------------------------------------------------------------------------

/// A reference to a DEDB (Data Entry Database) for Fast Path access.
///
/// In a full implementation this would connect to the actual DEDB
/// subsystem. Here we model the metadata needed for Fast Path routing.
#[derive(Debug, Clone)]
pub struct DedbIntegration {
    /// DEDB name.
    pub dedb_name: String,
    /// Area names within the DEDB.
    pub areas: Vec<String>,
    /// Whether the DEDB is online.
    pub online: bool,
}

impl DedbIntegration {
    /// Create a new DEDB reference.
    pub fn new(dedb_name: &str) -> Self {
        Self {
            dedb_name: dedb_name.to_string(),
            areas: Vec::new(),
            online: false,
        }
    }

    /// Add an area to the DEDB.
    pub fn add_area(&mut self, area: &str) {
        self.areas.push(area.to_string());
    }

    /// Bring the DEDB online.
    pub fn bring_online(&mut self) {
        self.online = true;
    }

    /// Take the DEDB offline.
    pub fn take_offline(&mut self) {
        self.online = false;
    }

    /// Check if the DEDB contains a specific area.
    pub fn has_area(&self, area: &str) -> bool {
        self.areas.iter().any(|a| a == area)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // --- EMH Queue ---

    #[test]
    fn test_emh_queue_new() {
        let q = EmhQueue::new();
        assert!(q.is_empty());
        assert_eq!(q.len(), 0);
    }

    #[test]
    fn test_emh_queue_default() {
        let q = EmhQueue::default();
        assert!(q.is_empty());
    }

    #[test]
    fn test_emh_enqueue_dequeue() {
        let mut q = EmhQueue::new();
        q.enqueue("T1", b"data1".to_vec(), EmhPriority::Normal);
        q.enqueue("T2", b"data2".to_vec(), EmhPriority::Normal);
        assert_eq!(q.len(), 2);

        let e = q.dequeue().unwrap();
        assert_eq!(e.trancode, "T1");
        assert_eq!(q.len(), 1);
    }

    #[test]
    fn test_emh_priority_ordering() {
        let mut q = EmhQueue::new();
        q.enqueue("LOW", b"lo".to_vec(), EmhPriority::Normal);
        q.enqueue("HIGH", b"hi".to_vec(), EmhPriority::High);

        let e = q.dequeue().unwrap();
        assert_eq!(e.trancode, "HIGH");
    }

    #[test]
    fn test_emh_fifo_within_priority() {
        let mut q = EmhQueue::new();
        q.enqueue("FIRST", b"1".to_vec(), EmhPriority::High);
        q.enqueue("SECOND", b"2".to_vec(), EmhPriority::High);

        let e = q.dequeue().unwrap();
        assert_eq!(e.trancode, "FIRST");
    }

    #[test]
    fn test_emh_dequeue_empty() {
        let mut q = EmhQueue::new();
        assert!(q.dequeue().is_none());
    }

    #[test]
    fn test_emh_peek() {
        let mut q = EmhQueue::new();
        q.enqueue("T1", b"d".to_vec(), EmhPriority::Normal);
        assert_eq!(q.peek().unwrap().trancode, "T1");
        assert_eq!(q.len(), 1); // Not removed
    }

    // --- IFP Region ---

    #[test]
    fn test_ifp_region_new() {
        let r = IfpRegion::new("IFP1");
        assert_eq!(r.id, "IFP1");
        assert_eq!(r.state(), IfpState::Stopped);
        assert_eq!(r.processed_count(), 0);
    }

    #[test]
    fn test_ifp_region_start_stop() {
        let mut r = IfpRegion::new("IFP1");
        r.start();
        assert_eq!(r.state(), IfpState::Waiting);
        r.stop();
        assert_eq!(r.state(), IfpState::Stopped);
    }

    #[test]
    fn test_ifp_process_next() {
        let mut r = IfpRegion::new("IFP1");
        let mut q = EmhQueue::new();
        q.enqueue("T1", b"payload".to_vec(), EmhPriority::Normal);

        r.start();
        let entry = r.process_next(&mut q).unwrap().unwrap();
        assert_eq!(entry.trancode, "T1");
        assert_eq!(r.processed_count(), 1);
        assert!(q.is_empty());
    }

    #[test]
    fn test_ifp_process_empty_queue() {
        let mut r = IfpRegion::new("IFP1");
        let mut q = EmhQueue::new();
        r.start();
        assert!(r.process_next(&mut q).unwrap().is_none());
    }

    #[test]
    fn test_ifp_process_while_stopped() {
        let mut r = IfpRegion::new("IFP1");
        let mut q = EmhQueue::new();
        q.enqueue("T1", b"d".to_vec(), EmhPriority::Normal);
        assert!(r.process_next(&mut q).is_err());
    }

    // --- DEDB Integration ---

    #[test]
    fn test_dedb_new() {
        let dedb = DedbIntegration::new("DEDB1");
        assert_eq!(dedb.dedb_name, "DEDB1");
        assert!(dedb.areas.is_empty());
        assert!(!dedb.online);
    }

    #[test]
    fn test_dedb_areas() {
        let mut dedb = DedbIntegration::new("DEDB1");
        dedb.add_area("AREA1");
        dedb.add_area("AREA2");
        assert!(dedb.has_area("AREA1"));
        assert!(!dedb.has_area("AREA3"));
    }

    #[test]
    fn test_dedb_online_offline() {
        let mut dedb = DedbIntegration::new("DEDB1");
        dedb.bring_online();
        assert!(dedb.online);
        dedb.take_offline();
        assert!(!dedb.online);
    }
}
