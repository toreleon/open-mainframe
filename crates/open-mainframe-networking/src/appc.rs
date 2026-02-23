//! APPC / LU 6.2 / CPI-C (NET-102).
//!
//! Provides program-to-program communication via APPC conversations
//! and the portable CPI-C (Common Programming Interface for Communications) API.

use std::collections::HashMap;

use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum AppcError {
    #[error("conversation {0} not found")]
    ConversationNotFound(u64),
    #[error("invalid state for operation: expected {expected:?}, got {actual:?}")]
    InvalidState {
        expected: ConversationState,
        actual: ConversationState,
    },
    #[error("partner not confirmed")]
    PartnerNotConfirmed,
    #[error("conversation already deallocated")]
    AlreadyDeallocated,
    #[error("ALLOCATE failed: {0}")]
    AllocateFailed(String),
    #[error("CPI-C error: {0}")]
    CpiCError(String),
}

// ---------------------------------------------------------------------------
// Conversation State Machine
// ---------------------------------------------------------------------------

/// APPC conversation states.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConversationState {
    Reset,
    Send,
    Receive,
    Confirm,
    ConfirmSend,
    ConfirmDealloc,
    Deallocated,
}

/// Sync level for conversation coordination.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyncLevel {
    None,
    Confirm,
    Syncpoint,
}

/// Deallocate type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeallocateType {
    Flush,
    SyncLevel,
    Abend,
}

// ---------------------------------------------------------------------------
// NET-102.1 — ALLOCATE (Start Conversation)
// ---------------------------------------------------------------------------

/// An APPC conversation between two transaction programs.
#[derive(Debug)]
pub struct Conversation {
    pub id: u64,
    pub tp_name: String,
    pub partner_lu: String,
    pub state: ConversationState,
    pub sync_level: SyncLevel,
    recv_queue: Vec<Vec<u8>>,
    confirmed: bool,
}

impl Conversation {
    /// Current conversation state.
    pub fn state(&self) -> ConversationState {
        self.state
    }
}

/// Manages APPC conversations.
#[derive(Debug)]
pub struct AppcManager {
    conversations: HashMap<u64, Conversation>,
    next_id: u64,
}

impl Default for AppcManager {
    fn default() -> Self {
        Self::new()
    }
}

impl AppcManager {
    pub fn new() -> Self {
        Self {
            conversations: HashMap::new(),
            next_id: 1,
        }
    }

    /// ALLOCATE — start a new APPC conversation.
    pub fn allocate(
        &mut self,
        tp_name: impl Into<String>,
        partner_lu: impl Into<String>,
        sync_level: SyncLevel,
    ) -> Result<u64, AppcError> {
        let id = self.next_id;
        self.next_id += 1;
        let conv = Conversation {
            id,
            tp_name: tp_name.into(),
            partner_lu: partner_lu.into(),
            state: ConversationState::Send,
            sync_level,
            recv_queue: Vec::new(),
            confirmed: false,
        };
        self.conversations.insert(id, conv);
        Ok(id)
    }

    // NET-102.2 — SEND_DATA / RECEIVE ------------------------------------

    /// SEND_DATA — queue data for the conversation partner.
    pub fn send_data(&mut self, conv_id: u64, data: Vec<u8>) -> Result<(), AppcError> {
        let conv = self
            .conversations
            .get_mut(&conv_id)
            .ok_or(AppcError::ConversationNotFound(conv_id))?;
        if conv.state != ConversationState::Send {
            return Err(AppcError::InvalidState {
                expected: ConversationState::Send,
                actual: conv.state,
            });
        }
        // In simulation, data goes directly to receive queue (loopback).
        conv.recv_queue.push(data);
        Ok(())
    }

    /// Switch conversation to receive state.
    pub fn prepare_to_receive(&mut self, conv_id: u64) -> Result<(), AppcError> {
        let conv = self
            .conversations
            .get_mut(&conv_id)
            .ok_or(AppcError::ConversationNotFound(conv_id))?;
        if conv.state != ConversationState::Send {
            return Err(AppcError::InvalidState {
                expected: ConversationState::Send,
                actual: conv.state,
            });
        }
        conv.state = ConversationState::Receive;
        Ok(())
    }

    /// RECEIVE — get data from the conversation partner.
    pub fn receive(&mut self, conv_id: u64) -> Result<Option<Vec<u8>>, AppcError> {
        let conv = self
            .conversations
            .get_mut(&conv_id)
            .ok_or(AppcError::ConversationNotFound(conv_id))?;
        if conv.state != ConversationState::Receive {
            return Err(AppcError::InvalidState {
                expected: ConversationState::Receive,
                actual: conv.state,
            });
        }
        Ok(conv.recv_queue.pop())
    }

    // NET-102.3 — CONFIRM / CONFIRMED ------------------------------------

    /// CONFIRM — request synchronisation from the partner.
    pub fn confirm(&mut self, conv_id: u64) -> Result<(), AppcError> {
        let conv = self
            .conversations
            .get_mut(&conv_id)
            .ok_or(AppcError::ConversationNotFound(conv_id))?;
        if conv.state != ConversationState::Send {
            return Err(AppcError::InvalidState {
                expected: ConversationState::Send,
                actual: conv.state,
            });
        }
        conv.state = ConversationState::Confirm;
        Ok(())
    }

    /// CONFIRMED — partner acknowledges confirmation.
    pub fn confirmed(&mut self, conv_id: u64) -> Result<(), AppcError> {
        let conv = self
            .conversations
            .get_mut(&conv_id)
            .ok_or(AppcError::ConversationNotFound(conv_id))?;
        if conv.state != ConversationState::Confirm {
            return Err(AppcError::InvalidState {
                expected: ConversationState::Confirm,
                actual: conv.state,
            });
        }
        conv.confirmed = true;
        conv.state = ConversationState::Send;
        Ok(())
    }

    // NET-102.4 — DEALLOCATE ---------------------------------------------

    /// DEALLOCATE — end a conversation.
    pub fn deallocate(
        &mut self,
        conv_id: u64,
        dealloc_type: DeallocateType,
    ) -> Result<(), AppcError> {
        let conv = self
            .conversations
            .get_mut(&conv_id)
            .ok_or(AppcError::ConversationNotFound(conv_id))?;
        if conv.state == ConversationState::Deallocated {
            return Err(AppcError::AlreadyDeallocated);
        }
        match dealloc_type {
            DeallocateType::SyncLevel if conv.sync_level == SyncLevel::Confirm => {
                if !conv.confirmed {
                    return Err(AppcError::PartnerNotConfirmed);
                }
            }
            _ => {}
        }
        conv.state = ConversationState::Deallocated;
        Ok(())
    }

    /// Get a reference to a conversation.
    pub fn conversation(&self, conv_id: u64) -> Option<&Conversation> {
        self.conversations.get(&conv_id)
    }

    /// Number of active (non-deallocated) conversations.
    pub fn active_count(&self) -> usize {
        self.conversations
            .values()
            .filter(|c| c.state != ConversationState::Deallocated)
            .count()
    }
}

// ---------------------------------------------------------------------------
// NET-102.5 — CPI-C Interface
// ---------------------------------------------------------------------------

/// CPI-C (Common Programming Interface for Communications) —
/// a portable API mapping to underlying APPC operations.
#[derive(Debug)]
pub struct CpiC {
    manager: AppcManager,
}

impl Default for CpiC {
    fn default() -> Self {
        Self::new()
    }
}

impl CpiC {
    pub fn new() -> Self {
        Self {
            manager: AppcManager::new(),
        }
    }

    /// CMINIT + CMALLC — initialise and allocate a conversation.
    pub fn cm_init_and_alloc(
        &mut self,
        sym_dest_name: &str,
        tp_name: &str,
    ) -> Result<u64, AppcError> {
        self.manager
            .allocate(tp_name, sym_dest_name, SyncLevel::Confirm)
    }

    /// CMSEND — send data on a conversation.
    pub fn cm_send(&mut self, conv_id: u64, data: Vec<u8>) -> Result<(), AppcError> {
        self.manager.send_data(conv_id, data)
    }

    /// CMRCV — receive data from a conversation.
    pub fn cm_receive(&mut self, conv_id: u64) -> Result<Option<Vec<u8>>, AppcError> {
        self.manager.prepare_to_receive(conv_id)?;
        self.manager.receive(conv_id)
    }

    /// CMDEAL — deallocate a conversation.
    pub fn cm_deallocate(&mut self, conv_id: u64) -> Result<(), AppcError> {
        self.manager.deallocate(conv_id, DeallocateType::Flush)
    }

    /// Get a reference to the underlying APPC manager.
    pub fn appc_manager(&self) -> &AppcManager {
        &self.manager
    }
}

// ---------------------------------------------------------------------------
// Tests — NET-102.6
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn allocate_conversation() {
        let mut mgr = AppcManager::new();
        let id = mgr
            .allocate("WORKER", "REMOTE.LU", SyncLevel::Confirm)
            .unwrap();
        let conv = mgr.conversation(id).unwrap();
        assert_eq!(conv.tp_name, "WORKER");
        assert_eq!(conv.partner_lu, "REMOTE.LU");
        assert_eq!(conv.state, ConversationState::Send);
    }

    #[test]
    fn send_and_receive_data() {
        let mut mgr = AppcManager::new();
        let id = mgr
            .allocate("WORKER", "REMOTE.LU", SyncLevel::None)
            .unwrap();
        mgr.send_data(id, b"Hello APPC".to_vec()).unwrap();
        mgr.prepare_to_receive(id).unwrap();
        let data = mgr.receive(id).unwrap().unwrap();
        assert_eq!(data, b"Hello APPC");
    }

    #[test]
    fn send_in_wrong_state_errors() {
        let mut mgr = AppcManager::new();
        let id = mgr
            .allocate("WORKER", "REMOTE.LU", SyncLevel::None)
            .unwrap();
        mgr.prepare_to_receive(id).unwrap();
        assert!(mgr.send_data(id, b"data".to_vec()).is_err());
    }

    #[test]
    fn confirm_and_confirmed() {
        let mut mgr = AppcManager::new();
        let id = mgr
            .allocate("WORKER", "REMOTE.LU", SyncLevel::Confirm)
            .unwrap();
        mgr.confirm(id).unwrap();
        assert_eq!(
            mgr.conversation(id).unwrap().state,
            ConversationState::Confirm
        );
        mgr.confirmed(id).unwrap();
        assert_eq!(
            mgr.conversation(id).unwrap().state,
            ConversationState::Send
        );
    }

    #[test]
    fn deallocate_flush() {
        let mut mgr = AppcManager::new();
        let id = mgr
            .allocate("WORKER", "REMOTE.LU", SyncLevel::None)
            .unwrap();
        mgr.deallocate(id, DeallocateType::Flush).unwrap();
        assert_eq!(
            mgr.conversation(id).unwrap().state,
            ConversationState::Deallocated
        );
    }

    #[test]
    fn deallocate_sync_level_requires_confirm() {
        let mut mgr = AppcManager::new();
        let id = mgr
            .allocate("WORKER", "REMOTE.LU", SyncLevel::Confirm)
            .unwrap();
        assert!(mgr.deallocate(id, DeallocateType::SyncLevel).is_err());
    }

    #[test]
    fn deallocate_sync_level_after_confirm() {
        let mut mgr = AppcManager::new();
        let id = mgr
            .allocate("WORKER", "REMOTE.LU", SyncLevel::Confirm)
            .unwrap();
        mgr.confirm(id).unwrap();
        mgr.confirmed(id).unwrap();
        mgr.deallocate(id, DeallocateType::SyncLevel).unwrap();
    }

    #[test]
    fn double_deallocate_errors() {
        let mut mgr = AppcManager::new();
        let id = mgr
            .allocate("WORKER", "REMOTE.LU", SyncLevel::None)
            .unwrap();
        mgr.deallocate(id, DeallocateType::Flush).unwrap();
        assert!(mgr.deallocate(id, DeallocateType::Flush).is_err());
    }

    #[test]
    fn active_count_tracks_conversations() {
        let mut mgr = AppcManager::new();
        let id1 = mgr
            .allocate("TP1", "LU1", SyncLevel::None)
            .unwrap();
        let _id2 = mgr
            .allocate("TP2", "LU2", SyncLevel::None)
            .unwrap();
        assert_eq!(mgr.active_count(), 2);
        mgr.deallocate(id1, DeallocateType::Flush).unwrap();
        assert_eq!(mgr.active_count(), 1);
    }

    // CPI-C tests
    #[test]
    fn cpic_init_send_receive_dealloc() {
        let mut cpic = CpiC::new();
        let id = cpic.cm_init_and_alloc("PARTNER.LU", "MYTP").unwrap();
        cpic.cm_send(id, b"CPI-C data".to_vec()).unwrap();
        let data = cpic.cm_receive(id).unwrap().unwrap();
        assert_eq!(data, b"CPI-C data");
        cpic.cm_deallocate(id).unwrap();
        assert_eq!(
            cpic.appc_manager().conversation(id).unwrap().state,
            ConversationState::Deallocated
        );
    }

    #[test]
    fn cpic_multiple_sends() {
        let mut cpic = CpiC::new();
        let id = cpic.cm_init_and_alloc("PARTNER.LU", "MYTP").unwrap();
        cpic.cm_send(id, b"msg1".to_vec()).unwrap();
        cpic.cm_send(id, b"msg2".to_vec()).unwrap();
        // Receive returns last-in (from the Vec pop)
        let data = cpic.cm_receive(id).unwrap().unwrap();
        assert_eq!(data, b"msg2");
    }
}
