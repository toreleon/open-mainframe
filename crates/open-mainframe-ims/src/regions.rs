//! IMS-TM104: Region Model & Scheduling.
//!
//! Models IMS region types (MPP, BMP, IFP, JBP, JMP), transaction
//! definitions, and the scheduling infrastructure that matches
//! transactions to regions by class and priority.

use crate::{ImsError, ImsResult, StatusCode};
use std::collections::BinaryHeap;
use std::cmp::Ordering;

// ---------------------------------------------------------------------------
// Region types
// ---------------------------------------------------------------------------

/// IMS region type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RegionType {
    /// Message Processing Program -- online transaction processing.
    MPP,
    /// Batch Message Processing -- batch + DL/I + message queue access.
    BMP,
    /// IMS Fast Path -- expedited message handler.
    IFP,
    /// Java Batch Processing.
    JBP,
    /// Java Message Processing.
    JMP,
}

impl std::fmt::Display for RegionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegionType::MPP => write!(f, "MPP"),
            RegionType::BMP => write!(f, "BMP"),
            RegionType::IFP => write!(f, "IFP"),
            RegionType::JBP => write!(f, "JBP"),
            RegionType::JMP => write!(f, "JMP"),
        }
    }
}

/// State of an IMS region.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegionState {
    /// Region is not started.
    Stopped,
    /// Region is started and waiting for work.
    Idle,
    /// Region is actively processing a transaction.
    Active,
    /// Region is being shut down.
    Stopping,
}

// ---------------------------------------------------------------------------
// MPP Region
// ---------------------------------------------------------------------------

/// Message Processing Program region.
///
/// MPP regions process online transactions from the IMS message queue.
/// Each MPP is assigned one or more scheduling classes and a priority
/// within those classes.
#[derive(Debug, Clone)]
pub struct MppRegion {
    /// Region identifier.
    pub id: String,
    /// Scheduling classes this region can serve (1-999).
    pub classes: Vec<u16>,
    /// Maximum number of parallel instances.
    pub max_parallel: u16,
    /// Scheduling priority (higher = higher priority).
    pub scheduling_priority: u16,
    /// Current state.
    state: RegionState,
    /// Number of currently active instances.
    active_count: u16,
}

impl MppRegion {
    /// Create a new MPP region.
    pub fn new(id: &str, classes: Vec<u16>, max_parallel: u16, scheduling_priority: u16) -> Self {
        Self {
            id: id.to_string(),
            classes,
            max_parallel,
            scheduling_priority,
            state: RegionState::Stopped,
            active_count: 0,
        }
    }

    /// Start the region.
    pub fn start(&mut self) {
        self.state = RegionState::Idle;
    }

    /// Stop the region.
    pub fn stop(&mut self) {
        self.state = RegionState::Stopping;
        if self.active_count == 0 {
            self.state = RegionState::Stopped;
        }
    }

    /// Check whether this region can service the given class.
    pub fn serves_class(&self, class: u16) -> bool {
        self.classes.contains(&class)
    }

    /// Check if the region can accept another transaction.
    pub fn can_schedule(&self) -> bool {
        self.state == RegionState::Idle || (self.state == RegionState::Active && self.active_count < self.max_parallel)
    }

    /// Mark one instance as active.
    pub fn activate(&mut self) -> ImsResult<()> {
        if !self.can_schedule() {
            return Err(ImsError::DliError {
                status: StatusCode::AD,
            });
        }
        self.active_count += 1;
        self.state = RegionState::Active;
        Ok(())
    }

    /// Mark one instance as complete.
    pub fn deactivate(&mut self) {
        if self.active_count > 0 {
            self.active_count -= 1;
        }
        if self.active_count == 0 && self.state == RegionState::Active {
            self.state = RegionState::Idle;
        }
        if self.active_count == 0 && self.state == RegionState::Stopping {
            self.state = RegionState::Stopped;
        }
    }

    /// Return current state.
    pub fn state(&self) -> RegionState {
        self.state
    }

    /// Return active instance count.
    pub fn active_count(&self) -> u16 {
        self.active_count
    }
}

// ---------------------------------------------------------------------------
// BMP Region
// ---------------------------------------------------------------------------

/// Batch Message Processing region.
///
/// BMP regions run batch programs that can also access the IMS message
/// queue and perform DL/I calls.
#[derive(Debug, Clone)]
pub struct BmpRegion {
    /// Region identifier.
    pub id: String,
    /// PSB name this BMP will use.
    pub psb_name: String,
    /// Whether this is a message-driven BMP (has access to msg queue).
    pub message_driven: bool,
    /// Current state.
    state: RegionState,
}

impl BmpRegion {
    /// Create a new BMP region.
    pub fn new(id: &str, psb_name: &str, message_driven: bool) -> Self {
        Self {
            id: id.to_string(),
            psb_name: psb_name.to_string(),
            message_driven,
            state: RegionState::Stopped,
        }
    }

    /// Start the BMP.
    pub fn start(&mut self) {
        self.state = RegionState::Active;
    }

    /// Stop the BMP.
    pub fn stop(&mut self) {
        self.state = RegionState::Stopped;
    }

    /// Return current state.
    pub fn state(&self) -> RegionState {
        self.state
    }
}

// ---------------------------------------------------------------------------
// Transaction Definition
// ---------------------------------------------------------------------------

/// Scheduling type for a transaction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SchedulingType {
    /// Parallel scheduling -- multiple instances can run concurrently.
    Parallel,
    /// Serial scheduling -- only one instance at a time.
    Serial,
}

/// Transaction definition (TRANSACT macro equivalent).
#[derive(Debug, Clone)]
pub struct TransactionDef {
    /// Transaction code (1-8 characters).
    pub trancode: String,
    /// Scheduling class.
    pub class: u16,
    /// Normal scheduling priority (1-14).
    pub priority: u8,
    /// Scheduling type.
    pub scheduling_type: SchedulingType,
    /// Whether this is a conversational transaction.
    pub conversational: bool,
    /// SPA size (0 if not conversational).
    pub spa_size: usize,
    /// PSB name.
    pub psb_name: String,
}

impl TransactionDef {
    /// Create a new transaction definition.
    pub fn new(trancode: &str, class: u16, priority: u8, psb_name: &str) -> Self {
        Self {
            trancode: trancode.to_string(),
            class,
            priority,
            scheduling_type: SchedulingType::Parallel,
            conversational: false,
            spa_size: 0,
            psb_name: psb_name.to_string(),
        }
    }

    /// Builder: set scheduling type.
    pub fn with_scheduling_type(mut self, st: SchedulingType) -> Self {
        self.scheduling_type = st;
        self
    }

    /// Builder: mark as conversational with a given SPA size.
    pub fn with_conversational(mut self, spa_size: usize) -> Self {
        self.conversational = true;
        self.spa_size = spa_size;
        self
    }
}

// ---------------------------------------------------------------------------
// Program Scheduling Parameters
// ---------------------------------------------------------------------------

/// Parameters that describe the program that services a transaction.
#[derive(Debug, Clone)]
pub struct ProgramSchedulingParams {
    /// PSB name.
    pub psb_name: String,
    /// Program language (COBOL, PL/I, Java, etc.).
    pub language: String,
    /// Required region type.
    pub region_type: RegionType,
}

impl ProgramSchedulingParams {
    /// Create new scheduling parameters.
    pub fn new(psb_name: &str, language: &str, region_type: RegionType) -> Self {
        Self {
            psb_name: psb_name.to_string(),
            language: language.to_string(),
            region_type,
        }
    }
}

// ---------------------------------------------------------------------------
// Transaction Scheduler
// ---------------------------------------------------------------------------

/// A queued transaction awaiting scheduling.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct QueuedTransaction {
    /// Transaction code.
    pub trancode: String,
    /// Class of the transaction.
    pub class: u16,
    /// Priority (higher = scheduled first).
    pub priority: u8,
    /// Sequence number for FIFO within the same priority.
    pub sequence: u64,
}

impl Ord for QueuedTransaction {
    fn cmp(&self, other: &Self) -> Ordering {
        // Higher priority first, then lower sequence (older = first).
        self.priority
            .cmp(&other.priority)
            .then(other.sequence.cmp(&self.sequence))
    }
}

impl PartialOrd for QueuedTransaction {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Queue-based transaction scheduler.
///
/// Enqueues transactions and dispatches them to regions that match the
/// transaction's scheduling class.
#[derive(Debug)]
pub struct TransactionScheduler {
    /// Priority queue of pending transactions.
    queue: BinaryHeap<QueuedTransaction>,
    /// Monotonically increasing sequence counter.
    next_seq: u64,
}

impl TransactionScheduler {
    /// Create a new scheduler.
    pub fn new() -> Self {
        Self {
            queue: BinaryHeap::new(),
            next_seq: 1,
        }
    }

    /// Enqueue a transaction for scheduling.
    pub fn enqueue(&mut self, trancode: &str, class: u16, priority: u8) {
        let qt = QueuedTransaction {
            trancode: trancode.to_string(),
            class,
            priority,
            sequence: self.next_seq,
        };
        self.next_seq += 1;
        self.queue.push(qt);
    }

    /// Dequeue the highest-priority transaction that matches any of the
    /// given classes.
    ///
    /// Returns `None` if no matching transaction is queued.
    pub fn dequeue_for_classes(&mut self, classes: &[u16]) -> Option<QueuedTransaction> {
        // We need to find the best matching item. Drain into a vec,
        // find the match, rebuild the heap.
        let mut items: Vec<QueuedTransaction> = Vec::new();
        let mut found: Option<QueuedTransaction> = None;

        while let Some(item) = self.queue.pop() {
            if found.is_none() && classes.contains(&item.class) {
                found = Some(item);
            } else {
                items.push(item);
            }
        }

        for item in items {
            self.queue.push(item);
        }

        found
    }

    /// Return the number of queued transactions.
    pub fn pending_count(&self) -> usize {
        self.queue.len()
    }

    /// Check if the queue is empty.
    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }
}

impl Default for TransactionScheduler {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // --- Region lifecycle ---

    #[test]
    fn test_mpp_region_new() {
        let region = MppRegion::new("MPP1", vec![1, 2], 4, 10);
        assert_eq!(region.id, "MPP1");
        assert_eq!(region.state(), RegionState::Stopped);
        assert_eq!(region.active_count(), 0);
    }

    #[test]
    fn test_mpp_region_start_stop() {
        let mut region = MppRegion::new("MPP1", vec![1], 2, 5);
        region.start();
        assert_eq!(region.state(), RegionState::Idle);
        region.stop();
        assert_eq!(region.state(), RegionState::Stopped);
    }

    #[test]
    fn test_mpp_region_activate_deactivate() {
        let mut region = MppRegion::new("MPP1", vec![1], 2, 5);
        region.start();
        region.activate().unwrap();
        assert_eq!(region.state(), RegionState::Active);
        assert_eq!(region.active_count(), 1);
        region.deactivate();
        assert_eq!(region.state(), RegionState::Idle);
        assert_eq!(region.active_count(), 0);
    }

    #[test]
    fn test_mpp_region_max_parallel() {
        let mut region = MppRegion::new("MPP1", vec![1], 2, 5);
        region.start();
        region.activate().unwrap();
        region.activate().unwrap();
        // At capacity
        assert!(region.activate().is_err());
    }

    #[test]
    fn test_mpp_serves_class() {
        let region = MppRegion::new("MPP1", vec![1, 3, 5], 2, 5);
        assert!(region.serves_class(1));
        assert!(region.serves_class(3));
        assert!(!region.serves_class(2));
    }

    #[test]
    fn test_mpp_stop_while_active() {
        let mut region = MppRegion::new("MPP1", vec![1], 2, 5);
        region.start();
        region.activate().unwrap();
        region.stop();
        assert_eq!(region.state(), RegionState::Stopping);
        region.deactivate();
        assert_eq!(region.state(), RegionState::Stopped);
    }

    #[test]
    fn test_bmp_region() {
        let mut bmp = BmpRegion::new("BMP1", "BMPPSB", true);
        assert_eq!(bmp.state(), RegionState::Stopped);
        assert!(bmp.message_driven);
        bmp.start();
        assert_eq!(bmp.state(), RegionState::Active);
        bmp.stop();
        assert_eq!(bmp.state(), RegionState::Stopped);
    }

    // --- Transaction definition ---

    #[test]
    fn test_transaction_def() {
        let tdef = TransactionDef::new("TRAN01", 1, 7, "MYPSB");
        assert_eq!(tdef.trancode, "TRAN01");
        assert_eq!(tdef.class, 1);
        assert_eq!(tdef.priority, 7);
        assert!(!tdef.conversational);
    }

    #[test]
    fn test_transaction_def_conversational() {
        let tdef = TransactionDef::new("CONV01", 2, 5, "CPSB")
            .with_conversational(256);
        assert!(tdef.conversational);
        assert_eq!(tdef.spa_size, 256);
    }

    #[test]
    fn test_transaction_def_serial() {
        let tdef = TransactionDef::new("SER01", 1, 3, "PSB1")
            .with_scheduling_type(SchedulingType::Serial);
        assert_eq!(tdef.scheduling_type, SchedulingType::Serial);
    }

    // --- Program scheduling params ---

    #[test]
    fn test_program_scheduling_params() {
        let psp = ProgramSchedulingParams::new("MYPSB", "COBOL", RegionType::MPP);
        assert_eq!(psp.psb_name, "MYPSB");
        assert_eq!(psp.language, "COBOL");
        assert_eq!(psp.region_type, RegionType::MPP);
    }

    // --- Scheduler ---

    #[test]
    fn test_scheduler_new() {
        let sched = TransactionScheduler::new();
        assert!(sched.is_empty());
        assert_eq!(sched.pending_count(), 0);
    }

    #[test]
    fn test_scheduler_default() {
        let sched = TransactionScheduler::default();
        assert!(sched.is_empty());
    }

    #[test]
    fn test_scheduler_enqueue_dequeue() {
        let mut sched = TransactionScheduler::new();
        sched.enqueue("T1", 1, 5);
        sched.enqueue("T2", 1, 10);
        assert_eq!(sched.pending_count(), 2);

        // Higher priority first
        let t = sched.dequeue_for_classes(&[1]).unwrap();
        assert_eq!(t.trancode, "T2");
        let t = sched.dequeue_for_classes(&[1]).unwrap();
        assert_eq!(t.trancode, "T1");
    }

    #[test]
    fn test_scheduler_fifo_same_priority() {
        let mut sched = TransactionScheduler::new();
        sched.enqueue("FIRST", 1, 5);
        sched.enqueue("SECOND", 1, 5);
        let t = sched.dequeue_for_classes(&[1]).unwrap();
        assert_eq!(t.trancode, "FIRST");
    }

    #[test]
    fn test_scheduler_class_filtering() {
        let mut sched = TransactionScheduler::new();
        sched.enqueue("CL1", 1, 10);
        sched.enqueue("CL2", 2, 10);
        sched.enqueue("CL3", 3, 10);

        // Only match class 2
        let t = sched.dequeue_for_classes(&[2]).unwrap();
        assert_eq!(t.trancode, "CL2");
        assert_eq!(sched.pending_count(), 2);
    }

    #[test]
    fn test_scheduler_no_match() {
        let mut sched = TransactionScheduler::new();
        sched.enqueue("T1", 1, 5);
        assert!(sched.dequeue_for_classes(&[99]).is_none());
        assert_eq!(sched.pending_count(), 1);
    }

    #[test]
    fn test_scheduler_empty_dequeue() {
        let mut sched = TransactionScheduler::new();
        assert!(sched.dequeue_for_classes(&[1]).is_none());
    }

    #[test]
    fn test_region_type_display() {
        assert_eq!(RegionType::MPP.to_string(), "MPP");
        assert_eq!(RegionType::BMP.to_string(), "BMP");
        assert_eq!(RegionType::IFP.to_string(), "IFP");
        assert_eq!(RegionType::JBP.to_string(), "JBP");
        assert_eq!(RegionType::JMP.to_string(), "JMP");
    }
}
