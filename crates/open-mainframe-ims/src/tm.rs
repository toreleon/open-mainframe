//! IMS Transaction Manager (TM) support.
//!
//! Implements IMS-TM100 through IMS-TM103:
//! - **Alt PCB** (Alternate Program Communication Block) for message switching
//! - **Conversational transactions** with Scratch Pad Area (SPA) lifecycle
//! - **System service calls**: ROLL, ROLS, SETS, SETU, XRST for rollback & restart
//! - **Query calls**: INIT, INQY, AUTH for environment introspection

use crate::{ImsError, ImsResult, StatusCode};

// ---------------------------------------------------------------------------
// IMS-TM100: Alternate PCB
// ---------------------------------------------------------------------------

/// State of an alternate PCB destination.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AltPcbState {
    /// PCB is available for use.
    Ready,
    /// A CHNG call has been issued; destination is set.
    Changed,
    /// Messages have been inserted.
    Inserted,
    /// Messages have been purged / sent.
    Purged,
}

/// Alternate Program Communication Block.
///
/// Used by IMS-TM applications to send output messages to alternate
/// destinations (terminals, transactions, or logical terminals).
#[derive(Debug, Clone)]
pub struct AltPcb {
    /// Logical terminal or transaction destination name (up to 8 chars).
    pub destination: String,
    /// Current PCB status code.
    pub status: StatusCode,
    /// Internal state.
    state: AltPcbState,
    /// Queued message segments waiting to be purged / sent.
    message_segments: Vec<Vec<u8>>,
}

impl AltPcb {
    /// Create a new alternate PCB with no destination.
    pub fn new() -> Self {
        Self {
            destination: String::new(),
            status: StatusCode::Ok,
            state: AltPcbState::Ready,
            message_segments: Vec::new(),
        }
    }

    /// CHNG call -- set or change the destination.
    ///
    /// The destination name must be 1-8 characters.
    pub fn chng(&mut self, destination: &str) -> ImsResult<()> {
        if destination.is_empty() || destination.len() > 8 {
            self.status = StatusCode::AD;
            return Err(ImsError::DliError {
                status: StatusCode::AD,
            });
        }
        self.destination = destination.to_string();
        self.state = AltPcbState::Changed;
        self.status = StatusCode::Ok;
        Ok(())
    }

    /// ISRT call -- insert a message segment into the output queue.
    ///
    /// A destination must have been set via [`chng`](Self::chng) first.
    pub fn isrt(&mut self, segment: Vec<u8>) -> ImsResult<()> {
        if self.destination.is_empty() {
            self.status = StatusCode::AD;
            return Err(ImsError::DliError {
                status: StatusCode::AD,
            });
        }
        self.message_segments.push(segment);
        self.state = AltPcbState::Inserted;
        self.status = StatusCode::Ok;
        Ok(())
    }

    /// PURG call -- purge (send) the queued message segments.
    ///
    /// Returns the segments that were flushed.
    pub fn purg(&mut self) -> ImsResult<Vec<Vec<u8>>> {
        if self.message_segments.is_empty() {
            // Nothing to purge is still OK
            self.status = StatusCode::Ok;
            return Ok(Vec::new());
        }
        let segments = std::mem::take(&mut self.message_segments);
        self.state = AltPcbState::Purged;
        self.status = StatusCode::Ok;
        Ok(segments)
    }

    /// SETO call -- set processing options on the PCB.
    ///
    /// In a full implementation this would modify formatting / MFS options.
    /// Here we accept the option string and acknowledge.
    pub fn seto(&mut self, _options: &str) -> ImsResult<()> {
        self.status = StatusCode::Ok;
        Ok(())
    }

    /// Return the current PCB state.
    pub fn state(&self) -> AltPcbState {
        self.state
    }

    /// Return the number of queued (un-purged) message segments.
    pub fn queued_segments(&self) -> usize {
        self.message_segments.len()
    }

    /// Reset the PCB to its initial state.
    pub fn reset(&mut self) {
        self.destination.clear();
        self.status = StatusCode::Ok;
        self.state = AltPcbState::Ready;
        self.message_segments.clear();
    }
}

impl Default for AltPcb {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// IMS-TM101: Conversational Transactions
// ---------------------------------------------------------------------------

/// Scratch Pad Area -- persistent storage across conversational turns.
///
/// The SPA is written to the message queue between interactions so the
/// application can resume where it left off.
#[derive(Debug, Clone)]
pub struct ScratchPadArea {
    /// Maximum SPA size in bytes.
    pub max_size: usize,
    /// Current SPA data.
    data: Vec<u8>,
    /// Transaction code that owns this SPA.
    pub trancode: String,
}

impl ScratchPadArea {
    /// Create a new SPA with the given maximum size and owning trancode.
    pub fn new(trancode: &str, max_size: usize) -> Self {
        Self {
            max_size,
            data: vec![0u8; max_size],
            trancode: trancode.to_string(),
        }
    }

    /// Read the current SPA data.
    pub fn read(&self) -> &[u8] {
        &self.data
    }

    /// Write data into the SPA.
    ///
    /// Returns an error if `data` exceeds [`max_size`](Self::max_size).
    pub fn write(&mut self, data: &[u8]) -> ImsResult<()> {
        if data.len() > self.max_size {
            return Err(ImsError::DliError {
                status: StatusCode::AL,
            });
        }
        self.data[..data.len()].copy_from_slice(data);
        // Zero-fill the rest
        for byte in &mut self.data[data.len()..] {
            *byte = 0;
        }
        Ok(())
    }

    /// Clear the SPA (zero-fill).
    pub fn clear(&mut self) {
        self.data.fill(0);
    }

    /// Check whether the SPA is empty (all zeros).
    pub fn is_empty(&self) -> bool {
        self.data.iter().all(|&b| b == 0)
    }
}

/// State of a conversational transaction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConversationState {
    /// Initial state -- waiting for first input.
    Initial,
    /// Active -- mid-conversation.
    Active,
    /// Ended -- conversation was explicitly ended by the application.
    Ended,
}

/// A conversational IMS transaction.
///
/// Conversational transactions maintain state across message exchanges via
/// a [`ScratchPadArea`].
#[derive(Debug, Clone)]
pub struct ConversationalTransaction {
    /// Transaction code.
    pub trancode: String,
    /// Scratch Pad Area.
    pub spa: ScratchPadArea,
    /// Current conversation state.
    state: ConversationState,
    /// Number of turns completed.
    turn_count: u32,
}

impl ConversationalTransaction {
    /// Create a new conversational transaction.
    pub fn new(trancode: &str, spa_size: usize) -> Self {
        Self {
            trancode: trancode.to_string(),
            spa: ScratchPadArea::new(trancode, spa_size),
            state: ConversationState::Initial,
            turn_count: 0,
        }
    }

    /// Begin a new turn of the conversation.
    pub fn begin_turn(&mut self) -> ImsResult<()> {
        if self.state == ConversationState::Ended {
            return Err(ImsError::DliError {
                status: StatusCode::AD,
            });
        }
        self.state = ConversationState::Active;
        self.turn_count += 1;
        Ok(())
    }

    /// End the current turn (SPA is saved for next iteration).
    pub fn end_turn(&mut self) {
        // SPA is implicitly persisted; state remains Active
    }

    /// End the conversation entirely (clear SPA).
    pub fn end_conversation(&mut self) {
        self.spa.clear();
        self.state = ConversationState::Ended;
    }

    /// Return the current conversation state.
    pub fn state(&self) -> ConversationState {
        self.state
    }

    /// Return the number of completed turns.
    pub fn turn_count(&self) -> u32 {
        self.turn_count
    }
}

// ---------------------------------------------------------------------------
// IMS-TM102: System Service Calls
// ---------------------------------------------------------------------------

/// Type of rollback operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RollbackType {
    /// ROLL -- abnormal termination with rollback, message re-queued.
    Roll,
    /// ROLS -- rollback to a savepoint or to the last commit point.
    Rols,
}

/// A savepoint token returned by SETS / SETU.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Savepoint {
    /// Token identifying this savepoint (up to 8 bytes).
    pub token: Vec<u8>,
    /// Sequence number within the unit of work.
    pub sequence: u32,
}

/// Manages savepoints and rollback within a unit of work.
#[derive(Debug, Clone)]
pub struct SavepointManager {
    /// Active savepoints in order.
    savepoints: Vec<Savepoint>,
    /// Next sequence number.
    next_seq: u32,
}

impl SavepointManager {
    /// Create a new savepoint manager.
    pub fn new() -> Self {
        Self {
            savepoints: Vec::new(),
            next_seq: 1,
        }
    }

    /// SETS call -- set an intermediate backout point (non-persistent).
    pub fn sets(&mut self, token: &[u8]) -> ImsResult<Savepoint> {
        if token.len() > 8 {
            return Err(ImsError::DliError {
                status: StatusCode::AD,
            });
        }
        let sp = Savepoint {
            token: token.to_vec(),
            sequence: self.next_seq,
        };
        self.next_seq += 1;
        self.savepoints.push(sp.clone());
        Ok(sp)
    }

    /// SETU call -- set an unconditional intermediate backout point.
    ///
    /// Same as SETS but does not return an error if unsupported.
    pub fn setu(&mut self, token: &[u8]) -> ImsResult<Savepoint> {
        self.sets(token)
    }

    /// ROLS call -- rollback to a specific savepoint.
    ///
    /// All savepoints after the target are discarded.
    pub fn rols(&mut self, token: &[u8]) -> ImsResult<()> {
        if let Some(pos) = self
            .savepoints
            .iter()
            .position(|sp| sp.token == token)
        {
            self.savepoints.truncate(pos);
            Ok(())
        } else {
            Err(ImsError::DliError {
                status: StatusCode::AD,
            })
        }
    }

    /// ROLL call -- discard all savepoints (full rollback).
    pub fn roll(&mut self) {
        self.savepoints.clear();
        self.next_seq = 1;
    }

    /// Return the current number of active savepoints.
    pub fn savepoint_count(&self) -> usize {
        self.savepoints.len()
    }

    /// Return a reference to all active savepoints.
    pub fn savepoints(&self) -> &[Savepoint] {
        &self.savepoints
    }
}

impl Default for SavepointManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Extended restart context for XRST call.
#[derive(Debug, Clone)]
pub struct ExtendedRestart {
    /// Checkpoint ID to restart from.
    pub checkpoint_id: String,
    /// I/O area length for log records.
    pub io_area_length: usize,
    /// Whether the restart was successful.
    pub restart_ok: bool,
}

impl ExtendedRestart {
    /// Issue an XRST call -- attempt to restart from a checkpoint.
    ///
    /// If `checkpoint_id` is empty a normal (cold) start is assumed.
    pub fn xrst(checkpoint_id: &str, io_area_length: usize) -> ImsResult<Self> {
        if io_area_length == 0 {
            return Err(ImsError::DliError {
                status: StatusCode::AD,
            });
        }
        Ok(Self {
            checkpoint_id: checkpoint_id.to_string(),
            io_area_length,
            restart_ok: true,
        })
    }

    /// Check whether this is a cold start (no checkpoint ID).
    pub fn is_cold_start(&self) -> bool {
        self.checkpoint_id.is_empty()
    }
}

// ---------------------------------------------------------------------------
// IMS-TM103: Query / Environment Calls
// ---------------------------------------------------------------------------

/// Information returned by the INIT call.
#[derive(Debug, Clone)]
pub struct InitInfo {
    /// Whether the region supports conversational transactions.
    pub conversational: bool,
    /// The IMS system ID.
    pub system_id: String,
    /// The IMS release level.
    pub release: String,
}

/// Execute the INIT call -- initialize and retrieve environment info.
pub fn init_call() -> ImsResult<InitInfo> {
    Ok(InitInfo {
        conversational: true,
        system_id: "IMS1".to_string(),
        release: "15.3.0".to_string(),
    })
}

/// Area code for the INQY call.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InqyArea {
    /// Query the environment (ENVIRON).
    Environ,
    /// Query the program name (PROGRAM).
    Program,
    /// Query the transaction name (TRAN).
    Transaction,
    /// Query the region type (REGION).
    Region,
    /// Query the IMS ID (IMSID).
    ImsId,
}

/// Information returned by the INQY call.
#[derive(Debug, Clone)]
pub struct InqyInfo {
    /// The area that was queried.
    pub area: InqyArea,
    /// Resulting data as a human-readable string.
    pub data: String,
}

/// Execute the INQY call -- query system information.
pub fn inqy_call(area: InqyArea) -> ImsResult<InqyInfo> {
    let data = match area {
        InqyArea::Environ => "IMS/TM V15".to_string(),
        InqyArea::Program => "DFSSAM01".to_string(),
        InqyArea::Transaction => "SAMTRAN".to_string(),
        InqyArea::Region => "MPP".to_string(),
        InqyArea::ImsId => "IMS1".to_string(),
    };
    Ok(InqyInfo { area, data })
}

/// Result of the AUTH call.
#[derive(Debug, Clone)]
pub struct AuthInfo {
    /// Whether the user is authorized.
    pub authorized: bool,
    /// User ID that was checked.
    pub user_id: String,
    /// Resource that was checked.
    pub resource: String,
}

/// Execute the AUTH call -- check security authorization.
pub fn auth_call(user_id: &str, resource: &str) -> ImsResult<AuthInfo> {
    if user_id.is_empty() {
        return Err(ImsError::DliError {
            status: StatusCode::AD,
        });
    }
    Ok(AuthInfo {
        authorized: true,
        user_id: user_id.to_string(),
        resource: resource.to_string(),
    })
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // =======================================================================
    // IMS-TM100: Alt PCB tests
    // =======================================================================

    #[test]
    fn test_alt_pcb_new() {
        let pcb = AltPcb::new();
        assert!(pcb.destination.is_empty());
        assert_eq!(pcb.state(), AltPcbState::Ready);
        assert_eq!(pcb.queued_segments(), 0);
    }

    #[test]
    fn test_alt_pcb_chng() {
        let mut pcb = AltPcb::new();
        pcb.chng("TERM001").unwrap();
        assert_eq!(pcb.destination, "TERM001");
        assert_eq!(pcb.state(), AltPcbState::Changed);
    }

    #[test]
    fn test_alt_pcb_chng_empty_destination() {
        let mut pcb = AltPcb::new();
        assert!(pcb.chng("").is_err());
    }

    #[test]
    fn test_alt_pcb_chng_too_long() {
        let mut pcb = AltPcb::new();
        assert!(pcb.chng("TOOLONGNAME").is_err());
    }

    #[test]
    fn test_alt_pcb_isrt() {
        let mut pcb = AltPcb::new();
        pcb.chng("TERM001").unwrap();
        pcb.isrt(b"Hello".to_vec()).unwrap();
        assert_eq!(pcb.queued_segments(), 1);
        assert_eq!(pcb.state(), AltPcbState::Inserted);
    }

    #[test]
    fn test_alt_pcb_isrt_no_destination() {
        let mut pcb = AltPcb::new();
        assert!(pcb.isrt(b"Hello".to_vec()).is_err());
    }

    #[test]
    fn test_alt_pcb_purg() {
        let mut pcb = AltPcb::new();
        pcb.chng("DEST1").unwrap();
        pcb.isrt(b"seg1".to_vec()).unwrap();
        pcb.isrt(b"seg2".to_vec()).unwrap();
        let segments = pcb.purg().unwrap();
        assert_eq!(segments.len(), 2);
        assert_eq!(segments[0], b"seg1");
        assert_eq!(segments[1], b"seg2");
        assert_eq!(pcb.queued_segments(), 0);
        assert_eq!(pcb.state(), AltPcbState::Purged);
    }

    #[test]
    fn test_alt_pcb_purg_empty() {
        let mut pcb = AltPcb::new();
        let segments = pcb.purg().unwrap();
        assert!(segments.is_empty());
    }

    #[test]
    fn test_alt_pcb_seto() {
        let mut pcb = AltPcb::new();
        pcb.seto("NOFORMAT").unwrap();
        assert_eq!(pcb.status, StatusCode::Ok);
    }

    #[test]
    fn test_alt_pcb_reset() {
        let mut pcb = AltPcb::new();
        pcb.chng("DEST").unwrap();
        pcb.isrt(b"data".to_vec()).unwrap();
        pcb.reset();
        assert!(pcb.destination.is_empty());
        assert_eq!(pcb.state(), AltPcbState::Ready);
        assert_eq!(pcb.queued_segments(), 0);
    }

    #[test]
    fn test_alt_pcb_default() {
        let pcb = AltPcb::default();
        assert_eq!(pcb.state(), AltPcbState::Ready);
    }

    #[test]
    fn test_alt_pcb_multi_message_flow() {
        let mut pcb = AltPcb::new();
        pcb.chng("LTERM01").unwrap();
        pcb.isrt(b"line1".to_vec()).unwrap();
        pcb.isrt(b"line2".to_vec()).unwrap();
        let batch1 = pcb.purg().unwrap();
        assert_eq!(batch1.len(), 2);

        // Second message to a different destination
        pcb.chng("LTERM02").unwrap();
        pcb.isrt(b"reply".to_vec()).unwrap();
        let batch2 = pcb.purg().unwrap();
        assert_eq!(batch2.len(), 1);
        assert_eq!(pcb.destination, "LTERM02");
    }

    // =======================================================================
    // IMS-TM101: Conversational Transaction tests
    // =======================================================================

    #[test]
    fn test_spa_new() {
        let spa = ScratchPadArea::new("TRAN1", 256);
        assert_eq!(spa.max_size, 256);
        assert!(spa.is_empty());
        assert_eq!(spa.trancode, "TRAN1");
    }

    #[test]
    fn test_spa_write_and_read() {
        let mut spa = ScratchPadArea::new("TRAN1", 64);
        spa.write(b"Hello SPA").unwrap();
        assert_eq!(&spa.read()[..9], b"Hello SPA");
        assert!(!spa.is_empty());
    }

    #[test]
    fn test_spa_write_too_large() {
        let mut spa = ScratchPadArea::new("TRAN1", 4);
        assert!(spa.write(b"12345").is_err());
    }

    #[test]
    fn test_spa_clear() {
        let mut spa = ScratchPadArea::new("TRAN1", 64);
        spa.write(b"data").unwrap();
        spa.clear();
        assert!(spa.is_empty());
    }

    #[test]
    fn test_spa_zero_fill() {
        let mut spa = ScratchPadArea::new("T", 8);
        spa.write(b"AB").unwrap();
        assert_eq!(spa.read(), &[b'A', b'B', 0, 0, 0, 0, 0, 0]);
    }

    #[test]
    fn test_conversation_new() {
        let conv = ConversationalTransaction::new("CONV1", 128);
        assert_eq!(conv.state(), ConversationState::Initial);
        assert_eq!(conv.turn_count(), 0);
    }

    #[test]
    fn test_conversation_begin_turn() {
        let mut conv = ConversationalTransaction::new("CONV1", 128);
        conv.begin_turn().unwrap();
        assert_eq!(conv.state(), ConversationState::Active);
        assert_eq!(conv.turn_count(), 1);
    }

    #[test]
    fn test_conversation_multiple_turns() {
        let mut conv = ConversationalTransaction::new("CONV1", 128);
        conv.begin_turn().unwrap();
        conv.end_turn();
        conv.begin_turn().unwrap();
        assert_eq!(conv.turn_count(), 2);
    }

    #[test]
    fn test_conversation_end() {
        let mut conv = ConversationalTransaction::new("CONV1", 128);
        conv.begin_turn().unwrap();
        conv.end_conversation();
        assert_eq!(conv.state(), ConversationState::Ended);
        assert!(conv.spa.is_empty());
    }

    #[test]
    fn test_conversation_begin_after_end() {
        let mut conv = ConversationalTransaction::new("CONV1", 128);
        conv.begin_turn().unwrap();
        conv.end_conversation();
        assert!(conv.begin_turn().is_err());
    }

    #[test]
    fn test_conversation_spa_persists() {
        let mut conv = ConversationalTransaction::new("CONV1", 64);
        conv.begin_turn().unwrap();
        conv.spa.write(b"state1").unwrap();
        conv.end_turn();
        // SPA data persists into next turn
        conv.begin_turn().unwrap();
        assert_eq!(&conv.spa.read()[..6], b"state1");
    }

    // =======================================================================
    // IMS-TM102: System Service Calls tests
    // =======================================================================

    #[test]
    fn test_savepoint_manager_new() {
        let mgr = SavepointManager::new();
        assert_eq!(mgr.savepoint_count(), 0);
    }

    #[test]
    fn test_savepoint_manager_default() {
        let mgr = SavepointManager::default();
        assert_eq!(mgr.savepoint_count(), 0);
    }

    #[test]
    fn test_sets_call() {
        let mut mgr = SavepointManager::new();
        let sp = mgr.sets(b"SP1").unwrap();
        assert_eq!(sp.token, b"SP1");
        assert_eq!(sp.sequence, 1);
        assert_eq!(mgr.savepoint_count(), 1);
    }

    #[test]
    fn test_setu_call() {
        let mut mgr = SavepointManager::new();
        let sp = mgr.setu(b"SPU").unwrap();
        assert_eq!(sp.token, b"SPU");
        assert_eq!(mgr.savepoint_count(), 1);
    }

    #[test]
    fn test_sets_token_too_long() {
        let mut mgr = SavepointManager::new();
        assert!(mgr.sets(b"TOOLONGTOKEN").is_err());
    }

    #[test]
    fn test_rols_to_savepoint() {
        let mut mgr = SavepointManager::new();
        mgr.sets(b"SP1").unwrap();
        mgr.sets(b"SP2").unwrap();
        mgr.sets(b"SP3").unwrap();
        mgr.rols(b"SP2").unwrap();
        // SP2 and SP3 are discarded; only SP1 remains
        assert_eq!(mgr.savepoint_count(), 1);
    }

    #[test]
    fn test_rols_unknown_token() {
        let mut mgr = SavepointManager::new();
        mgr.sets(b"SP1").unwrap();
        assert!(mgr.rols(b"NOPE").is_err());
    }

    #[test]
    fn test_roll_clears_all() {
        let mut mgr = SavepointManager::new();
        mgr.sets(b"SP1").unwrap();
        mgr.sets(b"SP2").unwrap();
        mgr.roll();
        assert_eq!(mgr.savepoint_count(), 0);
    }

    #[test]
    fn test_savepoints_ref() {
        let mut mgr = SavepointManager::new();
        mgr.sets(b"A").unwrap();
        mgr.sets(b"B").unwrap();
        let sps = mgr.savepoints();
        assert_eq!(sps.len(), 2);
        assert_eq!(sps[0].token, b"A");
        assert_eq!(sps[1].token, b"B");
    }

    #[test]
    fn test_xrst_cold_start() {
        let xrst = ExtendedRestart::xrst("", 4096).unwrap();
        assert!(xrst.is_cold_start());
        assert!(xrst.restart_ok);
    }

    #[test]
    fn test_xrst_warm_restart() {
        let xrst = ExtendedRestart::xrst("CHKP0001", 4096).unwrap();
        assert!(!xrst.is_cold_start());
        assert_eq!(xrst.checkpoint_id, "CHKP0001");
    }

    #[test]
    fn test_xrst_zero_io_area() {
        assert!(ExtendedRestart::xrst("C1", 0).is_err());
    }

    #[test]
    fn test_rollback_type_values() {
        assert_ne!(RollbackType::Roll, RollbackType::Rols);
    }

    // =======================================================================
    // IMS-TM103: Query / Environment Calls tests
    // =======================================================================

    #[test]
    fn test_init_call() {
        let info = init_call().unwrap();
        assert!(info.conversational);
        assert!(!info.system_id.is_empty());
        assert!(!info.release.is_empty());
    }

    #[test]
    fn test_inqy_environ() {
        let info = inqy_call(InqyArea::Environ).unwrap();
        assert_eq!(info.area, InqyArea::Environ);
        assert!(!info.data.is_empty());
    }

    #[test]
    fn test_inqy_program() {
        let info = inqy_call(InqyArea::Program).unwrap();
        assert_eq!(info.area, InqyArea::Program);
    }

    #[test]
    fn test_inqy_transaction() {
        let info = inqy_call(InqyArea::Transaction).unwrap();
        assert_eq!(info.area, InqyArea::Transaction);
    }

    #[test]
    fn test_inqy_region() {
        let info = inqy_call(InqyArea::Region).unwrap();
        assert_eq!(info.area, InqyArea::Region);
    }

    #[test]
    fn test_inqy_imsid() {
        let info = inqy_call(InqyArea::ImsId).unwrap();
        assert_eq!(info.area, InqyArea::ImsId);
        assert_eq!(info.data, "IMS1");
    }

    #[test]
    fn test_auth_call_success() {
        let info = auth_call("USER01", "TRAN01").unwrap();
        assert!(info.authorized);
        assert_eq!(info.user_id, "USER01");
        assert_eq!(info.resource, "TRAN01");
    }

    #[test]
    fn test_auth_call_empty_user() {
        assert!(auth_call("", "RESOURCE").is_err());
    }
}
