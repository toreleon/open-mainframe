//! IMS Runtime support.
//!
//! Provides database access and transaction management for IMS programs.
//! Uses an in-memory hierarchical store that can be persisted to PostgreSQL.

use std::collections::HashMap;
use std::collections::VecDeque;

use crate::dbd::DatabaseDefinition;
use crate::dli::store::{HierarchicalStore, QualOp};
use crate::dli::{DliCall, DliFunction, DliProcessor, DliResult, Ssa, SsaOperator};
use crate::psb::{PcbType, ProgramCommBlock, ProgramSpecBlock};
use crate::{ImsError, ImsResult, StatusCode};

/// A queued message for IMS message processing (MPP-style).
#[derive(Debug, Clone)]
pub struct ImsMessage {
    /// Originating logical terminal name
    pub lterm: String,
    /// Transaction code
    pub tran_code: String,
    /// Message segments (multi-segment messages)
    pub segments: Vec<Vec<u8>>,
    /// User ID of message originator
    pub user_id: String,
}

impl ImsMessage {
    /// Create a new single-segment message.
    pub fn new(lterm: &str, tran_code: &str, data: Vec<u8>) -> Self {
        Self {
            lterm: lterm.to_string(),
            tran_code: tran_code.to_string(),
            segments: vec![data],
            user_id: String::new(),
        }
    }

    /// Create a new message with a user ID.
    pub fn with_user_id(mut self, user_id: &str) -> Self {
        self.user_id = user_id.to_string();
        self
    }

    /// Add a segment to a multi-segment message.
    pub fn add_segment(&mut self, data: Vec<u8>) {
        self.segments.push(data);
    }
}

/// IMS Runtime for executing DL/I calls.
pub struct ImsRuntime {
    /// Loaded databases
    databases: HashMap<String, HierarchicalStore>,
    /// Database definitions
    dbds: HashMap<String, DatabaseDefinition>,
    /// Scheduled PSB
    psb: Option<ProgramSpecBlock>,
    /// DL/I processors for each PCB
    processors: Vec<DliProcessor>,
    /// Current position in each database
    positions: HashMap<String, DatabasePosition>,
    /// Snapshots of databases at last commit point (for ROLB)
    snapshots: HashMap<String, HierarchicalStore>,
    /// Checkpoint IDs recorded by CHKP calls
    checkpoint_ids: Vec<Vec<u8>>,
    /// Log records written by LOG calls
    log_records: Vec<Vec<u8>>,
    /// Runtime statistics
    stats: RuntimeStats,
    /// Input message queue (for GU to I/O PCB)
    input_queue: VecDeque<ImsMessage>,
    /// Output message queue (from ISRT to I/O PCB)
    output_queue: Vec<ImsMessage>,
    /// Current input message being processed
    current_input: Option<ImsMessage>,
    /// Index into current input message segments (for multi-segment GN)
    current_input_seg_index: usize,
}

/// Runtime statistics for STAT call.
#[derive(Debug, Clone, Default)]
pub struct RuntimeStats {
    /// Total number of DL/I calls executed.
    pub total_calls: u64,
    /// Number of GU calls.
    pub gu_calls: u64,
    /// Number of GN/GNP calls.
    pub gn_calls: u64,
    /// Number of ISRT calls.
    pub isrt_calls: u64,
    /// Number of DLET calls.
    pub dlet_calls: u64,
    /// Number of REPL calls.
    pub repl_calls: u64,
    /// Number of CHKP/SYNC calls.
    pub commit_calls: u64,
    /// Number of ROLB calls.
    pub rolb_calls: u64,
}

/// Current position in a database for GN/GNP calls.
#[derive(Debug, Clone, Default)]
pub struct DatabasePosition {
    /// Current segment record ID
    pub current_id: u64,
    /// Parent ID for GNP calls
    pub parent_id: u64,
    /// Path of segment names to current position
    pub path: Vec<String>,
}

impl ImsRuntime {
    /// Create a new IMS runtime.
    pub fn new() -> Self {
        Self {
            databases: HashMap::new(),
            dbds: HashMap::new(),
            psb: None,
            processors: Vec::new(),
            positions: HashMap::new(),
            snapshots: HashMap::new(),
            checkpoint_ids: Vec::new(),
            log_records: Vec::new(),
            stats: RuntimeStats::default(),
            input_queue: VecDeque::new(),
            output_queue: Vec::new(),
            current_input: None,
            current_input_seg_index: 0,
        }
    }

    /// Load a database definition.
    pub fn load_dbd(&mut self, dbd: DatabaseDefinition) {
        let name = dbd.name.clone();
        self.databases
            .entry(name.clone())
            .or_insert_with(|| HierarchicalStore::new(&name));
        self.dbds.insert(name, dbd);
    }

    /// Get or create a database store.
    pub fn get_store(&mut self, dbname: &str) -> &mut HierarchicalStore {
        self.databases
            .entry(dbname.to_uppercase())
            .or_insert_with(|| HierarchicalStore::new(dbname))
    }

    /// Schedule a PSB for execution.
    pub fn schedule_psb(&mut self, psb: ProgramSpecBlock) -> ImsResult<()> {
        let pcb_count = psb.pcb_count();
        self.psb = Some(psb);
        self.processors = (0..pcb_count).map(|_| DliProcessor::new()).collect();
        Ok(())
    }

    /// Get the scheduled PSB.
    pub fn get_psb(&self) -> Option<&ProgramSpecBlock> {
        self.psb.as_ref()
    }

    /// Get a mutable PCB by index.
    pub fn get_pcb_mut(&mut self, index: usize) -> ImsResult<&mut ProgramCommBlock> {
        self.psb
            .as_mut()
            .and_then(|p| p.pcbs.get_mut(index))
            .ok_or(ImsError::PcbNotFound(format!("PCB index {}", index)))
    }

    /// Execute a DL/I call.
    pub fn execute(&mut self, call: &DliCall) -> ImsResult<DliResult> {
        let psb = self
            .psb
            .as_mut()
            .ok_or(ImsError::DliError { status: StatusCode::AP })?;

        let pcb = psb
            .pcbs
            .get_mut(call.pcb_index)
            .ok_or(ImsError::PcbNotFound(format!("PCB index {}", call.pcb_index)))?;

        // Track statistics
        self.stats.total_calls += 1;
        match call.function {
            DliFunction::GU | DliFunction::GHU => self.stats.gu_calls += 1,
            DliFunction::GN | DliFunction::GNP | DliFunction::GHN | DliFunction::GHNP => {
                self.stats.gn_calls += 1
            }
            DliFunction::ISRT => self.stats.isrt_calls += 1,
            DliFunction::DLET => self.stats.dlet_calls += 1,
            DliFunction::REPL => self.stats.repl_calls += 1,
            _ => {}
        }

        // System service calls are dispatched separately
        if call.function.is_system_service() {
            return self.execute_system_service(call);
        }

        // I/O PCB message operations (GU/ISRT to I/O PCB)
        if pcb.pcb_type == PcbType::Io {
            return self.execute_io_pcb_call(call);
        }

        let dbname = pcb.dbname.clone();
        let store = self
            .databases
            .entry(dbname.clone())
            .or_insert_with(|| HierarchicalStore::new(&dbname));

        let position = self.positions.entry(dbname.clone()).or_default();

        match call.function {
            DliFunction::GU => execute_gu(store, call, pcb, position),
            DliFunction::GN => execute_gn(store, call, pcb, position),
            DliFunction::GNP => execute_gnp(store, call, pcb, position),
            DliFunction::GHU => execute_ghu(store, call, pcb, position),
            DliFunction::GHN => execute_ghn(store, call, pcb, position),
            DliFunction::GHNP => execute_ghnp(store, call, pcb, position),
            DliFunction::ISRT => execute_isrt(store, call, pcb, position),
            DliFunction::DLET => execute_dlet(store, call, pcb, position),
            DliFunction::REPL => execute_repl(store, call, pcb, position),
            _ => {
                pcb.set_status(StatusCode::AD);
                Ok(DliResult::error(StatusCode::AD))
            }
        }
    }

    /// Execute GU (Get Unique) - convenience method.
    pub fn gu(&mut self, pcb_index: usize, ssas: Vec<Ssa>) -> ImsResult<DliResult> {
        let call = DliCall::new(DliFunction::GU, pcb_index).with_ssas(ssas);
        self.execute(&call)
    }

    /// Execute GN (Get Next) - convenience method.
    pub fn gn(&mut self, pcb_index: usize, ssas: Vec<Ssa>) -> ImsResult<DliResult> {
        let call = DliCall::new(DliFunction::GN, pcb_index).with_ssas(ssas);
        self.execute(&call)
    }

    /// Execute GNP (Get Next within Parent) - convenience method.
    pub fn gnp(&mut self, pcb_index: usize, ssas: Vec<Ssa>) -> ImsResult<DliResult> {
        let call = DliCall::new(DliFunction::GNP, pcb_index).with_ssas(ssas);
        self.execute(&call)
    }

    /// Execute ISRT (Insert) - convenience method.
    pub fn isrt(&mut self, pcb_index: usize, ssas: Vec<Ssa>, data: Vec<u8>) -> ImsResult<DliResult> {
        let call = DliCall::new(DliFunction::ISRT, pcb_index)
            .with_ssas(ssas)
            .with_io_area(data);
        self.execute(&call)
    }

    // -----------------------------------------------------------------------
    // Message Queue Operations (Epic 402)
    // -----------------------------------------------------------------------

    /// Queue an input message for processing.
    ///
    /// In a real IMS system, messages arrive from terminals via IMS/TM.
    /// This method allows test code and the runtime to enqueue messages
    /// that will be delivered via GU to the I/O PCB.
    pub fn enqueue_input(&mut self, msg: ImsMessage) {
        self.input_queue.push_back(msg);
    }

    /// Get output messages (responses written via ISRT to I/O PCB).
    pub fn output_messages(&self) -> &[ImsMessage] {
        &self.output_queue
    }

    /// Drain and return all output messages.
    pub fn take_output_messages(&mut self) -> Vec<ImsMessage> {
        std::mem::take(&mut self.output_queue)
    }

    /// Execute a DL/I call against the I/O PCB (GU reads input, ISRT writes output).
    fn execute_io_pcb_call(&mut self, call: &DliCall) -> ImsResult<DliResult> {
        match call.function {
            DliFunction::GU => {
                // GU to I/O PCB: dequeue next input message
                if let Some(msg) = self.input_queue.pop_front() {
                    let data = msg.segments.first().cloned().unwrap_or_default();

                    // Update I/O PCB fields
                    if let Some(ref mut psb) = self.psb {
                        if let Some(io_pcb) = psb.pcbs.get_mut(call.pcb_index) {
                            io_pcb.lterm_name = msg.lterm.clone();
                            io_pcb.user_id = msg.user_id.clone();
                            io_pcb.input_msg_seq += 1;
                            io_pcb.set_status(StatusCode::Ok);
                        }
                    }

                    self.current_input = Some(msg);
                    self.current_input_seg_index = 1; // first segment already delivered

                    Ok(DliResult::ok(data, ""))
                } else {
                    // No message available — QC status
                    if let Some(ref mut psb) = self.psb {
                        if let Some(io_pcb) = psb.pcbs.get_mut(call.pcb_index) {
                            io_pcb.set_status(StatusCode::QC);
                        }
                    }
                    Ok(DliResult::error(StatusCode::QC))
                }
            }
            DliFunction::GN => {
                // GN to I/O PCB: get next segment of current input message
                if let Some(ref msg) = self.current_input {
                    if self.current_input_seg_index < msg.segments.len() {
                        let data = msg.segments[self.current_input_seg_index].clone();
                        self.current_input_seg_index += 1;

                        if let Some(ref mut psb) = self.psb {
                            if let Some(io_pcb) = psb.pcbs.get_mut(call.pcb_index) {
                                io_pcb.set_status(StatusCode::Ok);
                            }
                        }

                        Ok(DliResult::ok(data, ""))
                    } else {
                        // No more segments in this message
                        if let Some(ref mut psb) = self.psb {
                            if let Some(io_pcb) = psb.pcbs.get_mut(call.pcb_index) {
                                io_pcb.set_status(StatusCode::QD);
                            }
                        }
                        Ok(DliResult::error(StatusCode::QD))
                    }
                } else {
                    // No current message
                    if let Some(ref mut psb) = self.psb {
                        if let Some(io_pcb) = psb.pcbs.get_mut(call.pcb_index) {
                            io_pcb.set_status(StatusCode::QC);
                        }
                    }
                    Ok(DliResult::error(StatusCode::QC))
                }
            }
            DliFunction::ISRT => {
                // ISRT to I/O PCB: queue response message to originating terminal
                let lterm = self
                    .current_input
                    .as_ref()
                    .map(|m| m.lterm.clone())
                    .unwrap_or_default();
                let tran_code = self
                    .current_input
                    .as_ref()
                    .map(|m| m.tran_code.clone())
                    .unwrap_or_default();

                let response = ImsMessage::new(&lterm, &tran_code, call.io_area.clone());
                self.output_queue.push(response);

                if let Some(ref mut psb) = self.psb {
                    if let Some(io_pcb) = psb.pcbs.get_mut(call.pcb_index) {
                        io_pcb.set_status(StatusCode::Ok);
                    }
                }

                Ok(DliResult::ok(vec![], ""))
            }
            _ => {
                // Other functions not supported on I/O PCB
                if let Some(ref mut psb) = self.psb {
                    if let Some(io_pcb) = psb.pcbs.get_mut(call.pcb_index) {
                        io_pcb.set_status(StatusCode::AD);
                    }
                }
                Ok(DliResult::error(StatusCode::AD))
            }
        }
    }

    // -----------------------------------------------------------------------
    // System Service Calls (Epic 401)
    // -----------------------------------------------------------------------

    /// Execute a system service call (CHKP, SYNC, ROLB, LOG, STAT).
    fn execute_system_service(&mut self, call: &DliCall) -> ImsResult<DliResult> {
        match call.function {
            DliFunction::CHKP => self.execute_chkp(call),
            DliFunction::SYNC => self.execute_sync(),
            DliFunction::ROLB => self.execute_rolb(),
            DliFunction::LOG => self.execute_log(call),
            DliFunction::STAT => self.execute_stat(),
            _ => Ok(DliResult::error(StatusCode::AD)),
        }
    }

    /// Execute CHKP (Checkpoint).
    ///
    /// Commits all database changes and records a checkpoint ID.
    /// After CHKP, all changes are permanent and cannot be rolled back.
    fn execute_chkp(&mut self, call: &DliCall) -> ImsResult<DliResult> {
        self.stats.commit_calls += 1;

        // Record checkpoint ID from I/O area
        if !call.io_area.is_empty() {
            self.checkpoint_ids.push(call.io_area.clone());
        }

        // Take a snapshot of all databases — this IS the commit point
        self.snapshots.clear();
        for (name, store) in &self.databases {
            self.snapshots.insert(name.clone(), store.snapshot());
        }

        // Reset positions
        self.positions.clear();

        // Set I/O PCB status to success
        if let Some(ref mut psb) = self.psb {
            if let Some(io_pcb) = psb.pcbs.first_mut() {
                io_pcb.set_status(StatusCode::Ok);
            }
        }

        Ok(DliResult::ok(vec![], ""))
    }

    /// Execute SYNC (Synchronization point).
    ///
    /// Same as CHKP but without a checkpoint ID.
    fn execute_sync(&mut self) -> ImsResult<DliResult> {
        self.stats.commit_calls += 1;

        // Take a snapshot — commit point
        self.snapshots.clear();
        for (name, store) in &self.databases {
            self.snapshots.insert(name.clone(), store.snapshot());
        }

        // Reset positions
        self.positions.clear();

        // Set I/O PCB status
        if let Some(ref mut psb) = self.psb {
            if let Some(io_pcb) = psb.pcbs.first_mut() {
                io_pcb.set_status(StatusCode::Ok);
            }
        }

        Ok(DliResult::ok(vec![], ""))
    }

    /// Execute ROLB (Rollback).
    ///
    /// Undoes all changes since the last commit point (CHKP or SYNC).
    /// If no commit point exists, undoes all changes since PSB schedule.
    fn execute_rolb(&mut self) -> ImsResult<DliResult> {
        self.stats.rolb_calls += 1;

        // Restore databases from snapshots
        if !self.snapshots.is_empty() {
            for (name, snapshot) in &self.snapshots {
                if let Some(store) = self.databases.get_mut(name) {
                    store.restore_from(snapshot);
                }
            }
        } else {
            // No snapshot — clear all databases to their initial empty state
            for store in self.databases.values_mut() {
                let name = store.name.clone();
                *store = HierarchicalStore::new(&name);
            }
        }

        // Reset positions
        self.positions.clear();

        // Set I/O PCB status
        if let Some(ref mut psb) = self.psb {
            if let Some(io_pcb) = psb.pcbs.first_mut() {
                io_pcb.set_status(StatusCode::Ok);
            }
        }

        Ok(DliResult::ok(vec![], ""))
    }

    /// Execute LOG (Write log record).
    ///
    /// Writes the I/O area data to the IMS log.
    fn execute_log(&mut self, call: &DliCall) -> ImsResult<DliResult> {
        self.log_records.push(call.io_area.clone());

        // Set I/O PCB status
        if let Some(ref mut psb) = self.psb {
            if let Some(io_pcb) = psb.pcbs.first_mut() {
                io_pcb.set_status(StatusCode::Ok);
            }
        }

        Ok(DliResult::ok(vec![], ""))
    }

    /// Execute STAT (Get statistics).
    ///
    /// Returns runtime statistics in the I/O area.
    fn execute_stat(&mut self) -> ImsResult<DliResult> {
        // Format statistics as a byte array
        let stats_str = format!(
            "CALLS={},GU={},GN={},ISRT={},DLET={},REPL={},COMMIT={},ROLB={}",
            self.stats.total_calls,
            self.stats.gu_calls,
            self.stats.gn_calls,
            self.stats.isrt_calls,
            self.stats.dlet_calls,
            self.stats.repl_calls,
            self.stats.commit_calls,
            self.stats.rolb_calls,
        );

        // Set I/O PCB status
        if let Some(ref mut psb) = self.psb {
            if let Some(io_pcb) = psb.pcbs.first_mut() {
                io_pcb.set_status(StatusCode::Ok);
            }
        }

        Ok(DliResult::ok(stats_str.into_bytes(), ""))
    }

    /// Get checkpoint IDs recorded by CHKP calls.
    pub fn checkpoint_ids(&self) -> &[Vec<u8>] {
        &self.checkpoint_ids
    }

    /// Get log records written by LOG calls.
    pub fn log_records(&self) -> &[Vec<u8>] {
        &self.log_records
    }

    /// Get runtime statistics.
    pub fn stats(&self) -> &RuntimeStats {
        &self.stats
    }

    /// Schedule a PSB for execution with I/O PCB at index 0.
    ///
    /// Per AD-3.0-02, the I/O PCB is automatically created at index 0.
    /// Existing DB PCBs shift to index 1+.
    pub fn schedule_psb_with_io_pcb(&mut self, mut psb: ProgramSpecBlock) -> ImsResult<()> {
        // Create I/O PCB and insert at position 0
        let io_pcb = ProgramCommBlock::new_io();
        psb.pcbs.insert(0, io_pcb);

        // Update PCB positions
        for (i, pcb) in psb.pcbs.iter_mut().enumerate() {
            pcb.position = i;
        }

        let pcb_count = psb.pcb_count();
        self.psb = Some(psb);
        self.processors = (0..pcb_count).map(|_| DliProcessor::new()).collect();

        // Take initial snapshot as commit point
        self.snapshots.clear();
        for (name, store) in &self.databases {
            self.snapshots.insert(name.clone(), store.snapshot());
        }

        Ok(())
    }

    /// Terminate PSB.
    pub fn terminate(&mut self) {
        self.psb = None;
        self.processors.clear();
        self.positions.clear();
        self.snapshots.clear();
    }

    /// Reset all positions.
    pub fn reset_positions(&mut self) {
        self.positions.clear();
    }
}

impl Default for ImsRuntime {
    fn default() -> Self {
        Self::new()
    }
}

// Helper functions for DL/I operations

fn convert_ssa_op(op: SsaOperator) -> QualOp {
    match op {
        SsaOperator::Eq => QualOp::Eq,
        SsaOperator::Ne => QualOp::Ne,
        SsaOperator::Gt => QualOp::Gt,
        SsaOperator::Ge => QualOp::Ge,
        SsaOperator::Lt => QualOp::Lt,
        SsaOperator::Le => QualOp::Le,
    }
}

/// Execute GU (Get Unique) call.
fn execute_gu(
    store: &mut HierarchicalStore,
    call: &DliCall,
    pcb: &mut ProgramCommBlock,
    position: &mut DatabasePosition,
) -> ImsResult<DliResult> {
    // Reset position for GU
    position.current_id = 0;
    position.parent_id = 0;
    position.path.clear();

    if call.ssas.is_empty() {
        pcb.set_status(StatusCode::AD);
        return Ok(DliResult::error(StatusCode::AD));
    }

    // Navigate through SSAs
    let mut current_parent = 0u64;

    for (i, ssa) in call.ssas.iter().enumerate() {
        let is_last = i == call.ssas.len() - 1;

        // Check segment accessibility
        if !pcb.is_segment_accessible(&ssa.segment_name) {
            pcb.set_status(StatusCode::GE);
            return Ok(DliResult::not_found());
        }

        // Find segment
        let record = if current_parent == 0 {
            // Looking for root
            if let Some(ref qual) = ssa.qualification {
                store.find_root_qualified(
                    &ssa.segment_name,
                    &qual.field_name,
                    &qual.value,
                    convert_ssa_op(qual.operator),
                )
            } else {
                store.find_root(&ssa.segment_name)
            }
        } else {
            // Looking for child
            if let Some(ref qual) = ssa.qualification {
                store.find_child_qualified(
                    current_parent,
                    &ssa.segment_name,
                    &qual.field_name,
                    &qual.value,
                    convert_ssa_op(qual.operator),
                )
            } else {
                store.find_first_child(current_parent, &ssa.segment_name)
            }
        };

        match record {
            Some(rec) => {
                position.path.push(rec.segment_name.clone());
                current_parent = rec.id;

                if is_last {
                    // Found target segment
                    position.current_id = rec.id;
                    position.parent_id = rec.parent_id;
                    pcb.set_status(StatusCode::Ok);
                    return Ok(DliResult::ok(rec.data.clone(), &rec.segment_name));
                }
            }
            None => {
                pcb.set_status(StatusCode::GE);
                return Ok(DliResult::not_found());
            }
        }
    }

    pcb.set_status(StatusCode::GE);
    Ok(DliResult::not_found())
}

/// Execute GN (Get Next) call.
fn execute_gn(
    store: &mut HierarchicalStore,
    call: &DliCall,
    pcb: &mut ProgramCommBlock,
    position: &mut DatabasePosition,
) -> ImsResult<DliResult> {
    let target_segment = call.ssas.last().map(|s| s.segment_name.as_str());

    // If no current position, start from beginning
    if position.current_id == 0 {
        // Find first matching segment
        let record = if let Some(seg_name) = target_segment {
            if !pcb.is_segment_accessible(seg_name) {
                pcb.set_status(StatusCode::GE);
                return Ok(DliResult::not_found());
            }

            // Check qualification
            if let Some(ssa) = call.ssas.last() {
                if let Some(ref qual) = ssa.qualification {
                    store.find_root_qualified(
                        seg_name,
                        &qual.field_name,
                        &qual.value,
                        convert_ssa_op(qual.operator),
                    )
                } else {
                    store.find_root(seg_name)
                }
            } else {
                store.find_root(seg_name)
            }
        } else {
            // No segment specified, get first root
            store.first_root()
        };

        match record {
            Some(rec) => {
                position.current_id = rec.id;
                position.parent_id = rec.parent_id;
                position.path = vec![rec.segment_name.clone()];
                pcb.set_status(StatusCode::Ok);
                return Ok(DliResult::ok(rec.data.clone(), &rec.segment_name));
            }
            None => {
                pcb.set_status(StatusCode::GE);
                return Ok(DliResult::not_found());
            }
        }
    }

    // Get next from current position
    let record = store.get_next(position.current_id, target_segment);

    // Check qualification if present
    let record = if let Some(ssa) = call.ssas.last() {
        if let Some(ref qual) = ssa.qualification {
            // Need to keep searching until we find a match or hit end
            let mut current = record;
            while let Some(rec) = current {
                if rec.get_key(&qual.field_name)
                    .map(|v| convert_ssa_op(qual.operator).matches(v, &qual.value))
                    .unwrap_or(false)
                {
                    break;
                }
                current = store.get_next(rec.id, target_segment);
            }
            current
        } else {
            record
        }
    } else {
        record
    };

    match record {
        Some(rec) => {
            // Check accessibility
            if !pcb.is_segment_accessible(&rec.segment_name) {
                // Skip and continue
                position.current_id = rec.id;
                return execute_gn(store, call, pcb, position);
            }

            position.current_id = rec.id;
            position.parent_id = rec.parent_id;
            position.path.push(rec.segment_name.clone());
            pcb.set_status(StatusCode::Ok);
            Ok(DliResult::ok(rec.data.clone(), &rec.segment_name))
        }
        None => {
            pcb.set_status(StatusCode::GB);
            Ok(DliResult::end_of_db())
        }
    }
}

/// Execute GNP (Get Next within Parent) call.
fn execute_gnp(
    store: &mut HierarchicalStore,
    call: &DliCall,
    pcb: &mut ProgramCommBlock,
    position: &mut DatabasePosition,
) -> ImsResult<DliResult> {
    // Must have established parentage
    if position.current_id == 0 {
        pcb.set_status(StatusCode::GP);
        return Ok(DliResult::error(StatusCode::GP));
    }

    let target_segment = call.ssas.last().map(|s| s.segment_name.as_str());

    // Determine parent - if current is a root segment (parent_id == 0),
    // then current IS the parent for GNP purposes
    let (parent_id, search_from) = if position.parent_id == 0 {
        // Current is root, search from it as parent
        (position.current_id, position.current_id)
    } else {
        // Current is not root, use its parent
        (position.parent_id, position.current_id)
    };

    // If we're AT the parent (not a child), get the first matching child
    let record = if search_from == parent_id {
        // We're positioned at the parent, find first child
        if let Some(seg_name) = target_segment {
            store.find_first_child(parent_id, seg_name)
        } else {
            store.find_children(parent_id, None).into_iter().next()
        }
    } else {
        // We're positioned at a child, get next within parent
        store.get_next_within_parent(search_from, parent_id, target_segment)
    };

    match record {
        Some(rec) => {
            if !pcb.is_segment_accessible(&rec.segment_name) {
                position.current_id = rec.id;
                return execute_gnp(store, call, pcb, position);
            }

            position.current_id = rec.id;
            // Keep parent_id pointing to the boundary parent
            if position.parent_id == 0 {
                position.parent_id = parent_id;
            }
            pcb.set_status(StatusCode::Ok);
            Ok(DliResult::ok(rec.data.clone(), &rec.segment_name))
        }
        None => {
            pcb.set_status(StatusCode::GE);
            Ok(DliResult::not_found())
        }
    }
}

/// Execute GHU (Get Hold Unique) call.
fn execute_ghu(
    store: &mut HierarchicalStore,
    call: &DliCall,
    pcb: &mut ProgramCommBlock,
    position: &mut DatabasePosition,
) -> ImsResult<DliResult> {
    // Same as GU but marks for hold
    let result = execute_gu(store, call, pcb, position)?;

    // Position already set, so we're "holding" the current record
    Ok(result)
}

/// Execute GHN (Get Hold Next) call.
fn execute_ghn(
    store: &mut HierarchicalStore,
    call: &DliCall,
    pcb: &mut ProgramCommBlock,
    position: &mut DatabasePosition,
) -> ImsResult<DliResult> {
    execute_gn(store, call, pcb, position)
}

/// Execute GHNP (Get Hold Next within Parent) call.
fn execute_ghnp(
    store: &mut HierarchicalStore,
    call: &DliCall,
    pcb: &mut ProgramCommBlock,
    position: &mut DatabasePosition,
) -> ImsResult<DliResult> {
    execute_gnp(store, call, pcb, position)
}

/// Execute ISRT (Insert) call.
fn execute_isrt(
    store: &mut HierarchicalStore,
    call: &DliCall,
    pcb: &mut ProgramCommBlock,
    position: &mut DatabasePosition,
) -> ImsResult<DliResult> {
    if call.ssas.is_empty() {
        pcb.set_status(StatusCode::AD);
        return Ok(DliResult::error(StatusCode::AD));
    }

    let last_ssa = call.ssas.last().unwrap();

    // Check permission
    if !pcb.can_operate(&last_ssa.segment_name, crate::psb::Operation::Insert) {
        pcb.set_status(StatusCode::AD);
        return Ok(DliResult::error(StatusCode::AD));
    }

    // Determine parent
    let parent_id = if call.ssas.len() == 1 {
        // Root insert
        0
    } else {
        // Navigate to parent
        let mut current_parent = 0u64;

        for ssa in call.ssas.iter().take(call.ssas.len() - 1) {
            let record = if current_parent == 0 {
                if let Some(ref qual) = ssa.qualification {
                    store.find_root_qualified(
                        &ssa.segment_name,
                        &qual.field_name,
                        &qual.value,
                        convert_ssa_op(qual.operator),
                    )
                } else {
                    store.find_root(&ssa.segment_name)
                }
            } else {
                if let Some(ref qual) = ssa.qualification {
                    store.find_child_qualified(
                        current_parent,
                        &ssa.segment_name,
                        &qual.field_name,
                        &qual.value,
                        convert_ssa_op(qual.operator),
                    )
                } else {
                    store.find_first_child(current_parent, &ssa.segment_name)
                }
            };

            match record {
                Some(rec) => current_parent = rec.id,
                None => {
                    pcb.set_status(StatusCode::GE);
                    return Ok(DliResult::not_found());
                }
            }
        }

        current_parent
    };

    // Build keys from qualification
    let mut keys = HashMap::new();
    if let Some(ref qual) = last_ssa.qualification {
        keys.insert(qual.field_name.to_uppercase(), qual.value.clone());
    }

    // Check for duplicate key
    if !keys.is_empty() {
        if let Some((field, value)) = keys.iter().next() {
            if store.duplicate_key_exists(&last_ssa.segment_name, parent_id, field, value) {
                pcb.set_status(StatusCode::II);
                return Ok(DliResult::error(StatusCode::II));
            }
        }
    }

    // Insert
    let id = store.insert_with_keys(parent_id, &last_ssa.segment_name, call.io_area.clone(), keys);

    match id {
        Some(id) => {
            position.current_id = id;
            position.parent_id = parent_id;
            pcb.set_status(StatusCode::Ok);
            Ok(DliResult::ok(vec![], &last_ssa.segment_name))
        }
        None => {
            pcb.set_status(StatusCode::GE);
            Ok(DliResult::not_found())
        }
    }
}

/// Execute DLET (Delete) call.
fn execute_dlet(
    store: &mut HierarchicalStore,
    _call: &DliCall,
    pcb: &mut ProgramCommBlock,
    position: &mut DatabasePosition,
) -> ImsResult<DliResult> {
    // Must have prior Get Hold
    if position.current_id == 0 {
        pcb.set_status(StatusCode::DJ);
        return Ok(DliResult::error(StatusCode::DJ));
    }

    // Get segment name for permission check
    let segment_name = store
        .get(position.current_id)
        .map(|r| r.segment_name.clone())
        .unwrap_or_default();

    if !pcb.can_operate(&segment_name, crate::psb::Operation::Delete) {
        pcb.set_status(StatusCode::AD);
        return Ok(DliResult::error(StatusCode::AD));
    }

    // Delete (cascades to children)
    if store.delete(position.current_id) {
        position.current_id = 0;
        pcb.set_status(StatusCode::Ok);
        Ok(DliResult::ok(vec![], &segment_name))
    } else {
        pcb.set_status(StatusCode::AK);
        Ok(DliResult::error(StatusCode::AK))
    }
}

/// Execute REPL (Replace) call.
fn execute_repl(
    store: &mut HierarchicalStore,
    call: &DliCall,
    pcb: &mut ProgramCommBlock,
    position: &mut DatabasePosition,
) -> ImsResult<DliResult> {
    // Must have prior Get Hold
    if position.current_id == 0 {
        pcb.set_status(StatusCode::DJ);
        return Ok(DliResult::error(StatusCode::DJ));
    }

    // Get segment info
    let (segment_name, old_keys) = store
        .get(position.current_id)
        .map(|r| (r.segment_name.clone(), r.keys.clone()))
        .unwrap_or_default();

    if !pcb.can_operate(&segment_name, crate::psb::Operation::Replace) {
        pcb.set_status(StatusCode::AD);
        return Ok(DliResult::error(StatusCode::AD));
    }

    // Build new keys from I/O area if needed
    // For now, preserve old keys (key change check would compare)
    let new_keys = old_keys.clone();

    // Check key change (simplified - in real impl, would parse segment to get key field)
    if let Some((key_field, _)) = old_keys.iter().next() {
        if store.key_changed(position.current_id, &new_keys, key_field) {
            pcb.set_status(StatusCode::RX);
            return Ok(DliResult::error(StatusCode::RX));
        }
    }

    // Update
    if store.update_with_keys(position.current_id, call.io_area.clone(), new_keys) {
        pcb.set_status(StatusCode::Ok);
        Ok(DliResult::ok(vec![], &segment_name))
    } else {
        pcb.set_status(StatusCode::AK);
        Ok(DliResult::error(StatusCode::AK))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dli::Ssa;
    use crate::psb::{ProcessingOptions, SensitiveSegment};

    fn create_test_runtime() -> ImsRuntime {
        let mut runtime = ImsRuntime::new();

        // Create PSB (without I/O PCB for backward compatibility)
        let mut psb = ProgramSpecBlock::new("TESTPSB", crate::psb::PsbLanguage::Cobol);
        let mut pcb = ProgramCommBlock::new_db("TESTDB", ProcessingOptions::from_str("A"), 50);
        pcb.add_senseg(SensitiveSegment::new("CUSTOMER", "", ProcessingOptions::from_str("A")));
        pcb.add_senseg(SensitiveSegment::new("ORDER", "CUSTOMER", ProcessingOptions::from_str("A")));
        pcb.add_senseg(SensitiveSegment::new("ITEM", "ORDER", ProcessingOptions::from_str("A")));
        psb.add_pcb(pcb);

        runtime.schedule_psb(psb).unwrap();

        // Load test data
        let store = runtime.get_store("TESTDB");

        let cust1 = store.insert_root("CUSTOMER", b"Customer One".to_vec());
        store.get_mut(cust1).unwrap().keys.insert("CUSTNO".to_string(), b"C001".to_vec());

        let ord1 = store.insert_child(cust1, "ORDER", b"Order 1-1".to_vec()).unwrap();
        store.get_mut(ord1).unwrap().keys.insert("ORDNO".to_string(), b"O001".to_vec());

        let item1 = store.insert_child(ord1, "ITEM", b"Item 1-1-1".to_vec()).unwrap();
        store.get_mut(item1).unwrap().keys.insert("ITEMNO".to_string(), b"I001".to_vec());

        store.insert_child(ord1, "ITEM", b"Item 1-1-2".to_vec()).unwrap();

        let ord2 = store.insert_child(cust1, "ORDER", b"Order 1-2".to_vec()).unwrap();
        store.get_mut(ord2).unwrap().keys.insert("ORDNO".to_string(), b"O002".to_vec());

        let cust2 = store.insert_root("CUSTOMER", b"Customer Two".to_vec());
        store.get_mut(cust2).unwrap().keys.insert("CUSTNO".to_string(), b"C002".to_vec());

        runtime
    }

    #[test]
    fn test_gu_unqualified() {
        let mut runtime = create_test_runtime();

        let result = runtime.gu(0, vec![Ssa::unqualified("CUSTOMER")]).unwrap();
        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Customer One");
    }

    #[test]
    fn test_gu_qualified() {
        let mut runtime = create_test_runtime();

        let ssa = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C002");
        let result = runtime.gu(0, vec![ssa]).unwrap();
        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Customer Two");
    }

    #[test]
    fn test_gu_path() {
        let mut runtime = create_test_runtime();

        let result = runtime.gu(0, vec![
            Ssa::unqualified("CUSTOMER"),
            Ssa::unqualified("ORDER"),
        ]).unwrap();

        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Order 1-1");
    }

    #[test]
    fn test_gu_not_found() {
        let mut runtime = create_test_runtime();

        let ssa = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C999");
        let result = runtime.gu(0, vec![ssa]).unwrap();
        assert!(result.is_not_found());
    }

    #[test]
    fn test_gn_sequential() {
        let mut runtime = create_test_runtime();

        // First GN starts at beginning
        let result = runtime.gn(0, vec![Ssa::unqualified("CUSTOMER")]).unwrap();
        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Customer One");

        // Next GN gets second customer
        let result = runtime.gn(0, vec![Ssa::unqualified("CUSTOMER")]).unwrap();
        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Customer Two");

        // Next GN hits end
        let result = runtime.gn(0, vec![Ssa::unqualified("CUSTOMER")]).unwrap();
        assert_eq!(result.status, StatusCode::GB);
    }

    #[test]
    fn test_gnp() {
        let mut runtime = create_test_runtime();

        // First establish position at CUSTOMER
        runtime.gu(0, vec![Ssa::unqualified("CUSTOMER")]).unwrap();

        // GNP to get children
        let result = runtime.gnp(0, vec![Ssa::unqualified("ORDER")]).unwrap();
        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Order 1-1");

        // Get second order
        let result = runtime.gnp(0, vec![Ssa::unqualified("ORDER")]).unwrap();
        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Order 1-2");

        // No more orders under this customer
        let result = runtime.gnp(0, vec![Ssa::unqualified("ORDER")]).unwrap();
        assert_eq!(result.status, StatusCode::GE);
    }

    #[test]
    fn test_isrt() {
        let mut runtime = create_test_runtime();

        // Insert new customer
        let ssa = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C003");
        let result = runtime.isrt(0, vec![ssa], b"Customer Three".to_vec()).unwrap();
        assert!(result.is_ok());

        // Verify it exists
        let ssa = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C003");
        let result = runtime.gu(0, vec![ssa]).unwrap();
        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Customer Three");
    }

    #[test]
    fn test_isrt_duplicate() {
        let mut runtime = create_test_runtime();

        // Try to insert duplicate
        let ssa = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C001");
        let result = runtime.isrt(0, vec![ssa], b"Duplicate".to_vec()).unwrap();
        assert_eq!(result.status, StatusCode::II);
    }

    #[test]
    fn test_dlet() {
        let mut runtime = create_test_runtime();

        // Get hold
        let call = DliCall::new(DliFunction::GHU, 0)
            .with_ssa(Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C002"));
        runtime.execute(&call).unwrap();

        // Delete
        let call = DliCall::new(DliFunction::DLET, 0);
        let result = runtime.execute(&call).unwrap();
        assert!(result.is_ok());

        // Verify gone
        let ssa = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C002");
        let result = runtime.gu(0, vec![ssa]).unwrap();
        assert!(result.is_not_found());
    }

    #[test]
    fn test_dlet_no_hold() {
        let mut runtime = create_test_runtime();

        // Try delete without hold
        let call = DliCall::new(DliFunction::DLET, 0);
        let result = runtime.execute(&call).unwrap();
        assert_eq!(result.status, StatusCode::DJ);
    }

    #[test]
    fn test_repl() {
        let mut runtime = create_test_runtime();

        // Get hold
        let call = DliCall::new(DliFunction::GHU, 0)
            .with_ssa(Ssa::unqualified("CUSTOMER"));
        runtime.execute(&call).unwrap();

        // Replace
        let call = DliCall::new(DliFunction::REPL, 0)
            .with_io_area(b"Updated Customer One".to_vec());
        let result = runtime.execute(&call).unwrap();
        assert!(result.is_ok());

        // Verify updated
        let result = runtime.gu(0, vec![Ssa::unqualified("CUSTOMER")]).unwrap();
        assert_eq!(result.segment_data.unwrap(), b"Updated Customer One");
    }

    #[test]
    fn test_repl_no_hold() {
        let mut runtime = create_test_runtime();

        // Try replace without hold
        let call = DliCall::new(DliFunction::REPL, 0)
            .with_io_area(b"Update".to_vec());
        let result = runtime.execute(&call).unwrap();
        assert_eq!(result.status, StatusCode::DJ);
    }

    #[test]
    fn test_terminate() {
        let mut runtime = create_test_runtime();

        // Do some operations
        runtime.gu(0, vec![Ssa::unqualified("CUSTOMER")]).unwrap();

        // Terminate
        runtime.terminate();

        assert!(runtime.get_psb().is_none());
    }

    // -----------------------------------------------------------------------
    // Helper for system service call tests (uses I/O PCB at index 0)
    // -----------------------------------------------------------------------

    fn create_io_pcb_runtime() -> ImsRuntime {
        let mut runtime = ImsRuntime::new();

        // Create PSB with I/O PCB
        let mut psb = ProgramSpecBlock::new("TESTPSB", crate::psb::PsbLanguage::Cobol);
        let mut pcb = ProgramCommBlock::new_db("TESTDB", ProcessingOptions::from_str("A"), 50);
        pcb.add_senseg(SensitiveSegment::new("CUSTOMER", "", ProcessingOptions::from_str("A")));
        pcb.add_senseg(SensitiveSegment::new("ORDER", "CUSTOMER", ProcessingOptions::from_str("A")));
        psb.add_pcb(pcb);

        runtime.schedule_psb_with_io_pcb(psb).unwrap();

        // Load test data (PCB is now at index 1 due to I/O PCB at 0)
        let store = runtime.get_store("TESTDB");
        let cust1 = store.insert_root("CUSTOMER", b"Customer One".to_vec());
        store.get_mut(cust1).unwrap().keys.insert("CUSTNO".to_string(), b"C001".to_vec());

        runtime
    }

    // --- Story 401.1: CHKP Tests ---

    #[test]
    fn test_chkp_commits_and_records_id() {
        let mut runtime = create_io_pcb_runtime();

        // Insert a new customer (DB PCB is at index 1)
        let ssa = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C002");
        runtime.isrt(1, vec![ssa], b"Customer Two".to_vec()).unwrap();

        // Issue CHKP with checkpoint ID
        let chkp_call = DliCall::new(DliFunction::CHKP, 0)
            .with_io_area(b"CHKP001".to_vec());
        let result = runtime.execute(&chkp_call).unwrap();
        assert!(result.is_ok());

        // Verify checkpoint ID was recorded
        assert_eq!(runtime.checkpoint_ids().len(), 1);
        assert_eq!(runtime.checkpoint_ids()[0], b"CHKP001");
    }

    #[test]
    fn test_chkp_preserves_data_after_rolb() {
        let mut runtime = create_io_pcb_runtime();

        // Insert 2 customers
        let ssa1 = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C002");
        runtime.isrt(1, vec![ssa1], b"Customer Two".to_vec()).unwrap();

        let ssa2 = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C003");
        runtime.isrt(1, vec![ssa2], b"Customer Three".to_vec()).unwrap();

        // CHKP — commit these inserts
        let chkp_call = DliCall::new(DliFunction::CHKP, 0)
            .with_io_area(b"CP01".to_vec());
        runtime.execute(&chkp_call).unwrap();

        // Insert another customer after checkpoint
        let ssa3 = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C004");
        runtime.isrt(1, vec![ssa3], b"Customer Four".to_vec()).unwrap();

        // ROLB — only C004 should be undone
        let rolb_call = DliCall::new(DliFunction::ROLB, 0);
        runtime.execute(&rolb_call).unwrap();

        // C002 and C003 should still exist (committed before ROLB)
        let ssa_check2 = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C002");
        let result = runtime.gu(1, vec![ssa_check2]).unwrap();
        assert!(result.is_ok(), "C002 should exist after ROLB (was committed)");

        // C004 should be gone
        let ssa_check4 = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C004");
        let result = runtime.gu(1, vec![ssa_check4]).unwrap();
        assert!(result.is_not_found(), "C004 should be gone after ROLB");
    }

    // --- Story 401.2: ROLB Tests ---

    #[test]
    fn test_rolb_undoes_inserts() {
        let mut runtime = create_io_pcb_runtime();

        // Commit the initial test data (C001) so ROLB restores to this state
        let chkp_call = DliCall::new(DliFunction::CHKP, 0).with_io_area(b"INIT".to_vec());
        runtime.execute(&chkp_call).unwrap();

        let committed_count = runtime.get_store("TESTDB").len();
        assert_eq!(committed_count, 1);

        // Insert 3 new customers (uncommitted)
        for i in 2..=4 {
            let key = format!("C00{}", i);
            let data = format!("Customer {}", i);
            let ssa = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, key.as_bytes());
            runtime.isrt(1, vec![ssa], data.into_bytes()).unwrap();
        }

        // Verify inserts happened
        assert_eq!(runtime.get_store("TESTDB").len(), committed_count + 3);

        // ROLB — undo all 3 inserts, restore to last CHKP
        let rolb_call = DliCall::new(DliFunction::ROLB, 0);
        let result = runtime.execute(&rolb_call).unwrap();
        assert!(result.is_ok());

        // Verify database is back to committed state (1 customer)
        assert_eq!(runtime.get_store("TESTDB").len(), committed_count);
    }

    #[test]
    fn test_rolb_after_chkp_preserves_committed() {
        let mut runtime = create_io_pcb_runtime();

        // Insert C002
        let ssa = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C002");
        runtime.isrt(1, vec![ssa], b"Customer Two".to_vec()).unwrap();

        // CHKP
        let chkp_call = DliCall::new(DliFunction::CHKP, 0).with_io_area(b"CP1".to_vec());
        runtime.execute(&chkp_call).unwrap();

        // Insert C003 and C004 after checkpoint
        let ssa3 = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C003");
        runtime.isrt(1, vec![ssa3], b"Customer Three".to_vec()).unwrap();

        let ssa4 = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C004");
        runtime.isrt(1, vec![ssa4], b"Customer Four".to_vec()).unwrap();

        // ROLB — only C003 and C004 should be undone
        let rolb_call = DliCall::new(DliFunction::ROLB, 0);
        runtime.execute(&rolb_call).unwrap();

        // C001 and C002 should still exist
        let result = runtime.gu(1, vec![Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C001")]).unwrap();
        assert!(result.is_ok());
        let result = runtime.gu(1, vec![Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C002")]).unwrap();
        assert!(result.is_ok());

        // C003 should be gone
        let result = runtime.gu(1, vec![Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C003")]).unwrap();
        assert!(result.is_not_found());
    }

    // --- Story 401.3: SYNC, LOG, STAT Tests ---

    #[test]
    fn test_sync_commits_like_chkp() {
        let mut runtime = create_io_pcb_runtime();

        // Insert a customer
        let ssa = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C002");
        runtime.isrt(1, vec![ssa], b"Customer Two".to_vec()).unwrap();

        // SYNC — commit without checkpoint ID
        let sync_call = DliCall::new(DliFunction::SYNC, 0);
        let result = runtime.execute(&sync_call).unwrap();
        assert!(result.is_ok());

        // No checkpoint ID recorded
        assert!(runtime.checkpoint_ids().is_empty());

        // Insert C003 after SYNC
        let ssa3 = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C003");
        runtime.isrt(1, vec![ssa3], b"Customer Three".to_vec()).unwrap();

        // ROLB — only C003 undone
        let rolb_call = DliCall::new(DliFunction::ROLB, 0);
        runtime.execute(&rolb_call).unwrap();

        // C002 should still exist (committed by SYNC)
        let result = runtime.gu(1, vec![Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C002")]).unwrap();
        assert!(result.is_ok());

        // C003 should be gone
        let result = runtime.gu(1, vec![Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C003")]).unwrap();
        assert!(result.is_not_found());
    }

    #[test]
    fn test_log_writes_record() {
        let mut runtime = create_io_pcb_runtime();

        let log_call = DliCall::new(DliFunction::LOG, 0)
            .with_io_area(b"Application log message".to_vec());
        let result = runtime.execute(&log_call).unwrap();
        assert!(result.is_ok());

        assert_eq!(runtime.log_records().len(), 1);
        assert_eq!(runtime.log_records()[0], b"Application log message");
    }

    #[test]
    fn test_stat_returns_statistics() {
        let mut runtime = create_io_pcb_runtime();

        // Do some operations
        runtime.gu(1, vec![Ssa::unqualified("CUSTOMER")]).unwrap();
        runtime.gn(1, vec![Ssa::unqualified("CUSTOMER")]).unwrap();

        let ssa = Ssa::qualified("CUSTOMER", "CUSTNO", crate::dli::SsaOperator::Eq, b"C002");
        runtime.isrt(1, vec![ssa], b"New Customer".to_vec()).unwrap();

        // Get statistics
        let stat_call = DliCall::new(DliFunction::STAT, 0);
        let result = runtime.execute(&stat_call).unwrap();
        assert!(result.is_ok());

        // Verify stats are populated
        let stats = runtime.stats();
        assert!(stats.total_calls >= 4); // GU + GN + ISRT + STAT
        assert_eq!(stats.gu_calls, 1);
        assert_eq!(stats.gn_calls, 1);
        assert_eq!(stats.isrt_calls, 1);
    }

    #[test]
    fn test_dli_function_system_service() {
        assert!(DliFunction::CHKP.is_system_service());
        assert!(DliFunction::SYNC.is_system_service());
        assert!(DliFunction::ROLB.is_system_service());
        assert!(DliFunction::LOG.is_system_service());
        assert!(DliFunction::STAT.is_system_service());
        assert!(!DliFunction::GU.is_system_service());
        assert!(!DliFunction::ISRT.is_system_service());
    }

    #[test]
    fn test_dli_function_parse_new_variants() {
        assert_eq!(DliFunction::from_str("CHKP"), Some(DliFunction::CHKP));
        assert_eq!(DliFunction::from_str("SYNC"), Some(DliFunction::SYNC));
        assert_eq!(DliFunction::from_str("ROLB"), Some(DliFunction::ROLB));
        assert_eq!(DliFunction::from_str("LOG"), Some(DliFunction::LOG));
        assert_eq!(DliFunction::from_str("STAT"), Some(DliFunction::STAT));
    }

    #[test]
    fn test_io_pcb_at_index_zero() {
        let runtime = create_io_pcb_runtime();
        let psb = runtime.get_psb().unwrap();

        // I/O PCB should be at index 0
        assert_eq!(psb.pcbs[0].pcb_type, crate::psb::PcbType::Io);
        // DB PCB should be at index 1
        assert_eq!(psb.pcbs[1].pcb_type, crate::psb::PcbType::Db);
        assert_eq!(psb.pcbs[1].dbname, "TESTDB");
    }

    #[test]
    fn test_multiple_chkp_ids() {
        let mut runtime = create_io_pcb_runtime();

        // Issue multiple checkpoints
        for i in 1..=3 {
            let id = format!("CHKP{:03}", i);
            let chkp_call = DliCall::new(DliFunction::CHKP, 0)
                .with_io_area(id.into_bytes());
            runtime.execute(&chkp_call).unwrap();
        }

        assert_eq!(runtime.checkpoint_ids().len(), 3);
    }

    // -----------------------------------------------------------------------
    // Epic 402: Message Queue Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_gu_io_pcb_reads_input_message() {
        let mut runtime = create_io_pcb_runtime();

        // Enqueue a message
        let msg = super::ImsMessage::new("TERM01", "INQY", b"Hello from terminal".to_vec())
            .with_user_id("USER01");
        runtime.enqueue_input(msg);

        // GU to I/O PCB (index 0)
        let gu_call = DliCall::new(DliFunction::GU, 0);
        let result = runtime.execute(&gu_call).unwrap();
        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Hello from terminal");

        // Verify I/O PCB fields were updated
        let psb = runtime.get_psb().unwrap();
        let io_pcb = &psb.pcbs[0];
        assert_eq!(io_pcb.lterm_name, "TERM01");
        assert_eq!(io_pcb.user_id, "USER01");
        assert_eq!(io_pcb.input_msg_seq, 1);
    }

    #[test]
    fn test_gu_io_pcb_no_message_returns_qc() {
        let mut runtime = create_io_pcb_runtime();

        // GU to I/O PCB with no messages queued
        let gu_call = DliCall::new(DliFunction::GU, 0);
        let result = runtime.execute(&gu_call).unwrap();
        assert_eq!(result.status, StatusCode::QC);
    }

    #[test]
    fn test_isrt_io_pcb_writes_output_message() {
        let mut runtime = create_io_pcb_runtime();

        // Enqueue input first (establishes originating terminal)
        let msg = super::ImsMessage::new("TERM01", "INQY", b"Request".to_vec());
        runtime.enqueue_input(msg);

        let gu_call = DliCall::new(DliFunction::GU, 0);
        runtime.execute(&gu_call).unwrap();

        // ISRT to I/O PCB — send response
        let isrt_call = DliCall::new(DliFunction::ISRT, 0)
            .with_io_area(b"Response data".to_vec());
        let result = runtime.execute(&isrt_call).unwrap();
        assert!(result.is_ok());

        // Verify output was queued
        let output = runtime.output_messages();
        assert_eq!(output.len(), 1);
        assert_eq!(output[0].lterm, "TERM01");
        assert_eq!(output[0].segments[0], b"Response data");
    }

    #[test]
    fn test_multi_segment_input_message() {
        let mut runtime = create_io_pcb_runtime();

        // Create multi-segment message
        let mut msg = super::ImsMessage::new("TERM01", "TXN1", b"Segment 1".to_vec());
        msg.add_segment(b"Segment 2".to_vec());
        msg.add_segment(b"Segment 3".to_vec());
        runtime.enqueue_input(msg);

        // GU gets first segment
        let gu_call = DliCall::new(DliFunction::GU, 0);
        let result = runtime.execute(&gu_call).unwrap();
        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Segment 1");

        // GN gets second segment
        let gn_call = DliCall::new(DliFunction::GN, 0);
        let result = runtime.execute(&gn_call).unwrap();
        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Segment 2");

        // GN gets third segment
        let result = runtime.execute(&gn_call).unwrap();
        assert!(result.is_ok());
        assert_eq!(result.segment_data.unwrap(), b"Segment 3");

        // GN returns QD — no more segments
        let result = runtime.execute(&gn_call).unwrap();
        assert_eq!(result.status, StatusCode::QD);
    }

    #[test]
    fn test_multiple_messages_in_queue() {
        let mut runtime = create_io_pcb_runtime();

        // Enqueue two messages
        runtime.enqueue_input(super::ImsMessage::new("TERM01", "T1", b"Msg1".to_vec()));
        runtime.enqueue_input(super::ImsMessage::new("TERM02", "T2", b"Msg2".to_vec()));

        // First GU gets first message
        let gu_call = DliCall::new(DliFunction::GU, 0);
        let result = runtime.execute(&gu_call).unwrap();
        assert_eq!(result.segment_data.unwrap(), b"Msg1");

        // Second GU gets second message
        let result = runtime.execute(&gu_call).unwrap();
        assert_eq!(result.segment_data.unwrap(), b"Msg2");

        // I/O PCB lterm should reflect the last message
        let psb = runtime.get_psb().unwrap();
        assert_eq!(psb.pcbs[0].lterm_name, "TERM02");
    }

    #[test]
    fn test_take_output_messages() {
        let mut runtime = create_io_pcb_runtime();

        runtime.enqueue_input(super::ImsMessage::new("TERM01", "T1", b"Req".to_vec()));
        let gu_call = DliCall::new(DliFunction::GU, 0);
        runtime.execute(&gu_call).unwrap();

        // Send two responses
        let isrt = DliCall::new(DliFunction::ISRT, 0).with_io_area(b"Resp1".to_vec());
        runtime.execute(&isrt).unwrap();
        let isrt = DliCall::new(DliFunction::ISRT, 0).with_io_area(b"Resp2".to_vec());
        runtime.execute(&isrt).unwrap();

        // Drain output
        let output = runtime.take_output_messages();
        assert_eq!(output.len(), 2);
        assert!(runtime.output_messages().is_empty());
    }

    #[test]
    fn test_io_pcb_rejects_unsupported_functions() {
        let mut runtime = create_io_pcb_runtime();

        // DLET on I/O PCB should return AD
        let dlet_call = DliCall::new(DliFunction::DLET, 0);
        let result = runtime.execute(&dlet_call).unwrap();
        assert_eq!(result.status, StatusCode::AD);
    }

    #[test]
    fn test_store_snapshot_and_restore() {
        let mut store = HierarchicalStore::new("TEST");
        store.insert_root("SEG1", b"data1".to_vec());

        // Take snapshot
        let snapshot = store.snapshot();
        assert_eq!(snapshot.len(), 1);

        // Add more data
        store.insert_root("SEG2", b"data2".to_vec());
        assert_eq!(store.len(), 2);

        // Restore
        store.restore_from(&snapshot);
        assert_eq!(store.len(), 1);
    }
}
