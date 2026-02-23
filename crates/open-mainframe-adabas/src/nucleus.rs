//! ADA-108: Nucleus & Logging (6 stories).
//!
//! Provides the main database engine orchestrator (`AdabasNucleus`),
//! command queue, protection log (write-ahead log), work pool (buffer
//! management), and nucleus configuration parameters.

use std::collections::{HashMap, VecDeque};

use crate::acb::{Acb, AcbCommand, AcbResult};
use crate::descriptor::DescriptorSet;
use crate::fdt::Fdt;
use crate::modify::{DeleteCommand, StoreCommand, UpdateCommand};
use crate::storage::{AdabasFile, Isn};
use crate::transaction::{TransactionId, TransactionManager};
use crate::AdabasError;

// ── NucleusParams ──────────────────────────────────────────────────

/// Configuration parameters for the ADABAS nucleus.
#[derive(Debug, Clone)]
pub struct NucleusParams {
    /// Maximum number of files the nucleus can manage.
    pub max_files: u16,
    /// Buffer pool size in bytes.
    pub buffer_size: usize,
    /// Maximum number of concurrent users.
    pub max_users: u16,
    /// Maximum records held per transaction.
    pub max_holds: usize,
    /// Protection log enabled.
    pub protection_log_enabled: bool,
    /// Command queue capacity.
    pub command_queue_size: usize,
}

impl NucleusParams {
    /// Create default nucleus parameters.
    pub fn new() -> Self {
        Self {
            max_files: 5000,
            buffer_size: 1_048_576, // 1 MB
            max_users: 100,
            max_holds: 1000,
            protection_log_enabled: true,
            command_queue_size: 256,
        }
    }
}

impl Default for NucleusParams {
    fn default() -> Self {
        Self::new()
    }
}

// ── ProtectionLog ──────────────────────────────────────────────────

/// Write-ahead log for database recovery.
#[derive(Debug, Clone)]
pub struct ProtectionLog {
    /// Log entries (serialized transaction log segments).
    entries: Vec<ProtectionLogEntry>,
    /// Whether the log is active.
    pub active: bool,
}

/// A single entry in the protection log.
#[derive(Debug, Clone)]
pub struct ProtectionLogEntry {
    /// Sequence number.
    pub sequence: u64,
    /// Transaction ID.
    pub transaction_id: TransactionId,
    /// The operation type.
    pub operation: String,
    /// File number.
    pub file_number: u16,
    /// ISN affected.
    pub isn: Isn,
    /// Before-image data.
    pub before_image: Option<Vec<u8>>,
    /// After-image data.
    pub after_image: Option<Vec<u8>>,
}

impl ProtectionLog {
    /// Create a new protection log.
    pub fn new(active: bool) -> Self {
        Self {
            entries: Vec::new(),
            active,
        }
    }

    /// Write an entry to the protection log.
    pub fn write(
        &mut self,
        txn_id: TransactionId,
        operation: &str,
        file_number: u16,
        isn: Isn,
        before: Option<Vec<u8>>,
        after: Option<Vec<u8>>,
    ) {
        if !self.active {
            return;
        }
        let seq = self.entries.len() as u64 + 1;
        self.entries.push(ProtectionLogEntry {
            sequence: seq,
            transaction_id: txn_id,
            operation: operation.to_string(),
            file_number,
            isn,
            before_image: before,
            after_image: after,
        });
    }

    /// Return all log entries.
    pub fn entries(&self) -> &[ProtectionLogEntry] {
        &self.entries
    }

    /// Return the number of entries.
    pub fn entry_count(&self) -> usize {
        self.entries.len()
    }

    /// Clear the log (after checkpoint).
    pub fn clear(&mut self) {
        self.entries.clear();
    }
}

// ── CommandLog ─────────────────────────────────────────────────────

/// Command log (CLOG): audit trail of all executed commands.
#[derive(Debug, Clone)]
pub struct CommandLog {
    /// Log entries.
    entries: Vec<CommandLogEntry>,
    /// Whether the log is active.
    pub active: bool,
}

/// A single command log entry.
#[derive(Debug, Clone)]
pub struct CommandLogEntry {
    /// Sequence number.
    pub sequence: u64,
    /// Command code that was executed.
    pub command_code: String,
    /// File number.
    pub file_number: u16,
    /// ISN involved.
    pub isn: Isn,
    /// Response code returned.
    pub response_code: u16,
}

impl CommandLog {
    /// Create a new command log.
    pub fn new(active: bool) -> Self {
        Self {
            entries: Vec::new(),
            active,
        }
    }

    /// Record a command execution.
    pub fn record(
        &mut self,
        command_code: &str,
        file_number: u16,
        isn: Isn,
        response_code: u16,
    ) {
        if !self.active {
            return;
        }
        let seq = self.entries.len() as u64 + 1;
        self.entries.push(CommandLogEntry {
            sequence: seq,
            command_code: command_code.to_string(),
            file_number,
            isn,
            response_code,
        });
    }

    /// Return all log entries.
    pub fn entries(&self) -> &[CommandLogEntry] {
        &self.entries
    }

    /// Return the number of entries.
    pub fn entry_count(&self) -> usize {
        self.entries.len()
    }

    /// Clear the log.
    pub fn clear(&mut self) {
        self.entries.clear();
    }
}

// ── CommandQueue ───────────────────────────────────────────────────

/// Queue for incoming ACB commands.
#[derive(Debug)]
pub struct CommandQueue {
    queue: VecDeque<Acb>,
    capacity: usize,
}

impl CommandQueue {
    /// Create a new command queue with the given capacity.
    pub fn new(capacity: usize) -> Self {
        Self {
            queue: VecDeque::with_capacity(capacity),
            capacity,
        }
    }

    /// Enqueue an ACB command.
    pub fn enqueue(&mut self, acb: Acb) -> Result<(), AdabasError> {
        if self.queue.len() >= self.capacity {
            return Err(AdabasError::CommandQueueFull {
                capacity: self.capacity,
            });
        }
        self.queue.push_back(acb);
        Ok(())
    }

    /// Dequeue the next ACB command.
    pub fn dequeue(&mut self) -> Option<Acb> {
        self.queue.pop_front()
    }

    /// Return the number of pending commands.
    pub fn pending_count(&self) -> usize {
        self.queue.len()
    }

    /// Whether the queue is empty.
    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }
}

// ── WorkPool ───────────────────────────────────────────────────────

/// Buffer management: work areas for the nucleus.
#[derive(Debug)]
pub struct WorkPool {
    /// Named work areas.
    areas: HashMap<String, Vec<u8>>,
    /// Total buffer size limit.
    max_size: usize,
    /// Current total allocated.
    current_size: usize,
}

impl WorkPool {
    /// Create a new work pool.
    pub fn new(max_size: usize) -> Self {
        Self {
            areas: HashMap::new(),
            max_size,
            current_size: 0,
        }
    }

    /// Allocate a work area.
    pub fn allocate(&mut self, name: impl Into<String>, size: usize) -> Result<(), AdabasError> {
        if self.current_size + size > self.max_size {
            return Err(AdabasError::WorkPoolExhausted {
                requested: size,
                available: self.max_size.saturating_sub(self.current_size),
            });
        }
        let name = name.into();
        self.areas.insert(name, vec![0u8; size]);
        self.current_size += size;
        Ok(())
    }

    /// Get a work area by name.
    pub fn get(&self, name: &str) -> Option<&[u8]> {
        self.areas.get(name).map(|v| v.as_slice())
    }

    /// Get a mutable work area by name.
    pub fn get_mut(&mut self, name: &str) -> Option<&mut [u8]> {
        self.areas.get_mut(name).map(|v| v.as_mut_slice())
    }

    /// Release a work area.
    pub fn release(&mut self, name: &str) -> bool {
        if let Some(area) = self.areas.remove(name) {
            self.current_size = self.current_size.saturating_sub(area.len());
            true
        } else {
            false
        }
    }

    /// Return the available buffer space.
    pub fn available(&self) -> usize {
        self.max_size.saturating_sub(self.current_size)
    }
}

// ── AdabasNucleus ──────────────────────────────────────────────────

/// Main database engine orchestrator.
#[derive(Debug)]
pub struct AdabasNucleus {
    /// Nucleus configuration.
    pub params: NucleusParams,
    /// Database files.
    files: HashMap<u16, AdabasFile>,
    /// Field Definition Tables per file.
    fdts: HashMap<u16, Fdt>,
    /// Descriptor sets per file.
    descriptor_sets: HashMap<u16, DescriptorSet>,
    /// Transaction manager.
    pub transactions: TransactionManager,
    /// Protection log.
    pub protection_log: ProtectionLog,
    /// Command queue.
    pub command_queue: CommandQueue,
    /// Command log (audit trail).
    pub command_log: CommandLog,
    /// Work pool.
    pub work_pool: WorkPool,
    /// Whether the nucleus is running.
    running: bool,
}

impl AdabasNucleus {
    /// Create a new ADABAS nucleus with the given parameters.
    pub fn new(params: NucleusParams) -> Self {
        let plog_active = params.protection_log_enabled;
        let cq_size = params.command_queue_size;
        let buf_size = params.buffer_size;
        let max_holds = params.max_holds;
        Self {
            params,
            files: HashMap::new(),
            fdts: HashMap::new(),
            descriptor_sets: HashMap::new(),
            transactions: TransactionManager::new(max_holds),
            protection_log: ProtectionLog::new(plog_active),
            command_queue: CommandQueue::new(cq_size),
            command_log: CommandLog::new(true),
            work_pool: WorkPool::new(buf_size),
            running: false,
        }
    }

    /// Start the nucleus.
    pub fn start(&mut self) {
        self.running = true;
    }

    /// Stop the nucleus.
    pub fn stop(&mut self) {
        self.running = false;
    }

    /// Whether the nucleus is running.
    pub fn is_running(&self) -> bool {
        self.running
    }

    /// Define a file in the database.
    pub fn define_file(
        &mut self,
        file_number: u16,
        name: &str,
        fdt: Fdt,
        descriptors: DescriptorSet,
    ) -> Result<(), AdabasError> {
        if self.files.len() >= self.params.max_files as usize {
            return Err(AdabasError::MaxFilesExceeded {
                max: self.params.max_files,
            });
        }
        self.files
            .insert(file_number, AdabasFile::new(file_number, name));
        self.fdts.insert(file_number, fdt);
        self.descriptor_sets.insert(file_number, descriptors);
        Ok(())
    }

    /// Get a reference to a file.
    pub fn file(&self, file_number: u16) -> Result<&AdabasFile, AdabasError> {
        self.files
            .get(&file_number)
            .ok_or(AdabasError::FileNotFound {
                file_number,
            })
    }

    /// Get a mutable reference to a file.
    pub fn file_mut(&mut self, file_number: u16) -> Result<&mut AdabasFile, AdabasError> {
        self.files
            .get_mut(&file_number)
            .ok_or(AdabasError::FileNotFound {
                file_number,
            })
    }

    /// Get the FDT for a file.
    pub fn fdt(&self, file_number: u16) -> Option<&Fdt> {
        self.fdts.get(&file_number)
    }

    /// Get the descriptor set for a file.
    pub fn descriptors(&self, file_number: u16) -> Option<&DescriptorSet> {
        self.descriptor_sets.get(&file_number)
    }

    /// Execute an ACB command against the nucleus.
    pub fn execute_acb(&mut self, acb: &Acb) -> AcbResult {
        match acb.command_code {
            AcbCommand::L1 => self.execute_read_by_isn(acb),
            AcbCommand::N1 => self.execute_store(acb, false),
            AcbCommand::N2 => self.execute_store(acb, true),
            AcbCommand::A1 => self.execute_update(acb),
            AcbCommand::E1 => self.execute_delete(acb),
            AcbCommand::Et => self.execute_et(acb),
            AcbCommand::Bt => self.execute_bt(acb),
            AcbCommand::Op => AcbResult::success(),
            AcbCommand::Cl => AcbResult::success(),
            AcbCommand::Lf => AcbResult::success(), // LF: FDT data returned via record buffer
            AcbCommand::Rc => AcbResult::success(), // RC: release resources
            AcbCommand::Hi => self.execute_hi(acb),
            AcbCommand::Ri => self.execute_ri(acb),
            AcbCommand::Re => AcbResult::success(), // RE: ET data (simplified)
            _ => AcbResult::error(22), // Invalid command for this context
        }
    }

    fn execute_read_by_isn(&self, acb: &Acb) -> AcbResult {
        match self.files.get(&acb.file_number) {
            Some(file) => match file.read_record(acb.isn) {
                Ok(data) => AcbResult::success()
                    .with_isn(acb.isn)
                    .with_record(data.to_vec()),
                Err(_) => AcbResult::error(113), // ISN not found
            },
            None => AcbResult::error(17), // File not found
        }
    }

    fn execute_store(&mut self, acb: &Acb, user_isn: bool) -> AcbResult {
        let descriptors = self
            .descriptor_sets
            .get(&acb.file_number)
            .cloned()
            .unwrap_or_default();
        let file = match self.files.get_mut(&acb.file_number) {
            Some(f) => f,
            None => return AcbResult::error(17),
        };

        let cmd = if user_isn {
            StoreCommand::n2(acb.file_number, acb.isn, acb.record_buffer.clone())
        } else {
            StoreCommand::n1(acb.file_number, acb.record_buffer.clone())
        };

        match cmd.execute(file, &descriptors) {
            Ok(isn) => AcbResult::success().with_isn(isn),
            Err(_) => AcbResult::error(113),
        }
    }

    fn execute_update(&mut self, acb: &Acb) -> AcbResult {
        let descriptors = self
            .descriptor_sets
            .get(&acb.file_number)
            .cloned()
            .unwrap_or_default();
        let file = match self.files.get_mut(&acb.file_number) {
            Some(f) => f,
            None => return AcbResult::error(17),
        };

        let cmd = UpdateCommand::new(acb.file_number, acb.isn, acb.record_buffer.clone());
        match cmd.execute(file, &descriptors) {
            Ok(()) => AcbResult::success().with_isn(acb.isn),
            Err(_) => AcbResult::error(113),
        }
    }

    fn execute_delete(&mut self, acb: &Acb) -> AcbResult {
        let descriptors = self
            .descriptor_sets
            .get(&acb.file_number)
            .cloned()
            .unwrap_or_default();
        let file = match self.files.get_mut(&acb.file_number) {
            Some(f) => f,
            None => return AcbResult::error(17),
        };

        let cmd = DeleteCommand::new(acb.file_number, acb.isn);
        match cmd.execute(file, &descriptors) {
            Ok(_) => AcbResult::success().with_isn(acb.isn),
            Err(_) => AcbResult::error(113),
        }
    }

    fn execute_et(&mut self, acb: &Acb) -> AcbResult {
        if acb.command_id == 0 {
            return AcbResult::success();
        }
        match self.transactions.commit(acb.command_id as TransactionId) {
            Ok(_) => AcbResult::success(),
            Err(_) => AcbResult::error(9), // Transaction error
        }
    }

    fn execute_hi(&mut self, acb: &Acb) -> AcbResult {
        match self
            .transactions
            .hold_queue
            .hold(acb.file_number, acb.isn)
        {
            Ok(()) => AcbResult::success().with_isn(acb.isn),
            Err(_) => AcbResult::error(145), // Hold queue overflow
        }
    }

    fn execute_ri(&mut self, acb: &Acb) -> AcbResult {
        self.transactions
            .hold_queue
            .release(acb.file_number, acb.isn);
        AcbResult::success().with_isn(acb.isn)
    }

    fn execute_bt(&mut self, acb: &Acb) -> AcbResult {
        if acb.command_id == 0 {
            return AcbResult::success();
        }
        match self.transactions.rollback(acb.command_id as TransactionId) {
            Ok(_) => AcbResult::success(),
            Err(_) => AcbResult::error(9),
        }
    }
}

// ── Tests ──────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fdt::{FieldDef, FieldType};

    fn setup_nucleus() -> AdabasNucleus {
        let mut nucleus = AdabasNucleus::new(NucleusParams::new());
        nucleus.start();

        let mut fdt = Fdt::new();
        fdt.add_field(FieldDef::new("AA", 1, FieldType::Alpha, 20))
            .unwrap();
        fdt.add_field(FieldDef::new("AB", 1, FieldType::Numeric, 8))
            .unwrap();

        nucleus
            .define_file(1, "EMPLOYEES", fdt, DescriptorSet::new())
            .unwrap();
        nucleus
    }

    #[test]
    fn nucleus_start_stop() {
        let mut n = AdabasNucleus::new(NucleusParams::new());
        assert!(!n.is_running());
        n.start();
        assert!(n.is_running());
        n.stop();
        assert!(!n.is_running());
    }

    #[test]
    fn nucleus_define_file() {
        let nucleus = setup_nucleus();
        assert!(nucleus.file(1).is_ok());
        assert!(nucleus.file(99).is_err());
        assert!(nucleus.fdt(1).is_some());
    }

    #[test]
    fn nucleus_store_and_read() {
        let mut nucleus = setup_nucleus();
        let store_acb = Acb::new(AcbCommand::N1, 1)
            .with_record_buffer(b"SMITH".to_vec());
        let result = nucleus.execute_acb(&store_acb);
        assert_eq!(result.response_code, 0);
        let isn = result.isn;

        let read_acb = Acb::new(AcbCommand::L1, 1).with_isn(isn);
        let result = nucleus.execute_acb(&read_acb);
        assert_eq!(result.response_code, 0);
        assert_eq!(result.record_buffer, b"SMITH");
    }

    #[test]
    fn nucleus_update() {
        let mut nucleus = setup_nucleus();
        let store = Acb::new(AcbCommand::N1, 1)
            .with_record_buffer(b"OLD".to_vec());
        let isn = nucleus.execute_acb(&store).isn;

        let update = Acb::new(AcbCommand::A1, 1)
            .with_isn(isn)
            .with_record_buffer(b"NEW".to_vec());
        let result = nucleus.execute_acb(&update);
        assert_eq!(result.response_code, 0);

        let read = Acb::new(AcbCommand::L1, 1).with_isn(isn);
        let result = nucleus.execute_acb(&read);
        assert_eq!(result.record_buffer, b"NEW");
    }

    #[test]
    fn nucleus_delete() {
        let mut nucleus = setup_nucleus();
        let store = Acb::new(AcbCommand::N1, 1)
            .with_record_buffer(b"GONE".to_vec());
        let isn = nucleus.execute_acb(&store).isn;

        let delete = Acb::new(AcbCommand::E1, 1).with_isn(isn);
        let result = nucleus.execute_acb(&delete);
        assert_eq!(result.response_code, 0);

        let read = Acb::new(AcbCommand::L1, 1).with_isn(isn);
        let result = nucleus.execute_acb(&read);
        assert_ne!(result.response_code, 0);
    }

    #[test]
    fn nucleus_file_not_found() {
        let mut nucleus = setup_nucleus();
        let acb = Acb::new(AcbCommand::L1, 99).with_isn(1);
        let result = nucleus.execute_acb(&acb);
        assert_eq!(result.response_code, 17);
    }

    #[test]
    fn command_queue_operations() {
        let mut cq = CommandQueue::new(2);
        assert!(cq.is_empty());
        cq.enqueue(Acb::new(AcbCommand::L1, 1)).unwrap();
        cq.enqueue(Acb::new(AcbCommand::L2, 1)).unwrap();
        assert_eq!(cq.pending_count(), 2);
        assert!(cq.enqueue(Acb::new(AcbCommand::L3, 1)).is_err());

        let cmd = cq.dequeue().unwrap();
        assert_eq!(cmd.command_code, AcbCommand::L1);
        assert_eq!(cq.pending_count(), 1);
    }

    #[test]
    fn protection_log_write() {
        let mut plog = ProtectionLog::new(true);
        plog.write(1, "INSERT", 1, 42, None, Some(b"data".to_vec()));
        assert_eq!(plog.entry_count(), 1);
        let entry = &plog.entries()[0];
        assert_eq!(entry.sequence, 1);
        assert_eq!(entry.transaction_id, 1);
        assert_eq!(entry.isn, 42);
    }

    #[test]
    fn protection_log_inactive() {
        let mut plog = ProtectionLog::new(false);
        plog.write(1, "INSERT", 1, 42, None, Some(b"data".to_vec()));
        assert_eq!(plog.entry_count(), 0);
    }

    #[test]
    fn protection_log_clear() {
        let mut plog = ProtectionLog::new(true);
        plog.write(1, "INSERT", 1, 1, None, Some(vec![1]));
        plog.clear();
        assert_eq!(plog.entry_count(), 0);
    }

    #[test]
    fn work_pool_operations() {
        let mut wp = WorkPool::new(100);
        wp.allocate("A", 30).unwrap();
        wp.allocate("B", 30).unwrap();
        assert_eq!(wp.available(), 40);
        assert!(wp.get("A").is_some());
        assert!(wp.get("C").is_none());

        assert!(wp.allocate("C", 50).is_err());
        assert!(wp.release("A"));
        assert_eq!(wp.available(), 70);
    }

    #[test]
    fn nucleus_params_default() {
        let p = NucleusParams::new();
        assert_eq!(p.max_files, 5000);
        assert!(p.protection_log_enabled);
    }

    #[test]
    fn nucleus_op_cl() {
        let mut nucleus = setup_nucleus();
        let op = Acb::new(AcbCommand::Op, 0);
        assert_eq!(nucleus.execute_acb(&op).response_code, 0);
        let cl = Acb::new(AcbCommand::Cl, 0);
        assert_eq!(nucleus.execute_acb(&cl).response_code, 0);
    }
}
