//! IMS-TM106: Open Transaction Manager Access (OTMA).
//!
//! OTMA provides a high-performance, high-throughput interface to IMS
//! through z/OS Cross-system Coupling Facility (XCF) groups. It uses
//! transaction pipes (TPIPEs) and message prefixes to route and manage
//! messages between IMS and external clients.

use crate::{ImsError, ImsResult, StatusCode};

// ---------------------------------------------------------------------------
// XCF Group
// ---------------------------------------------------------------------------

/// An XCF (Cross-system Coupling Facility) group for OTMA.
///
/// Members of the same XCF group can exchange messages. IMS and its OTMA
/// clients must belong to the same group.
#[derive(Debug, Clone)]
pub struct XcfGroup {
    /// Group name (up to 8 characters).
    pub name: String,
    /// Member names currently joined.
    members: Vec<String>,
}

impl XcfGroup {
    /// Create a new XCF group.
    pub fn new(name: &str) -> ImsResult<Self> {
        if name.is_empty() || name.len() > 8 {
            return Err(ImsError::DliError {
                status: StatusCode::AD,
            });
        }
        Ok(Self {
            name: name.to_string(),
            members: Vec::new(),
        })
    }

    /// Join a member to the group.
    ///
    /// Returns an error if the member name is invalid or already joined.
    pub fn join(&mut self, member: &str) -> ImsResult<()> {
        if member.is_empty() || member.len() > 8 {
            return Err(ImsError::DliError {
                status: StatusCode::AD,
            });
        }
        if self.members.iter().any(|m| m == member) {
            return Err(ImsError::ConnectionError(format!(
                "Member {} already in group {}",
                member, self.name
            )));
        }
        self.members.push(member.to_string());
        Ok(())
    }

    /// Remove a member from the group.
    pub fn leave(&mut self, member: &str) -> ImsResult<()> {
        if let Some(pos) = self.members.iter().position(|m| m == member) {
            self.members.remove(pos);
            Ok(())
        } else {
            Err(ImsError::ConnectionError(format!(
                "Member {} not found in group {}",
                member, self.name
            )))
        }
    }

    /// Return the list of current members.
    pub fn members(&self) -> &[String] {
        &self.members
    }

    /// Check whether a member is in the group.
    pub fn is_member(&self, member: &str) -> bool {
        self.members.iter().any(|m| m == member)
    }
}

// ---------------------------------------------------------------------------
// TPIPE (Transaction Pipe)
// ---------------------------------------------------------------------------

/// Synchronisation level for OTMA messages.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyncLevel {
    /// No confirmation required.
    None,
    /// Confirm receipt.
    Confirm,
    /// Sync point (two-phase commit).
    Syncpt,
}

/// State of a TPIPE.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TpipeState {
    /// Pipe is open and ready.
    Open,
    /// Pipe is closed.
    Closed,
}

/// A Transaction Pipe.
///
/// A TPIPE represents a named, logical connection between an OTMA client
/// and IMS. Each TPIPE can carry transactions at a specified sync level.
#[derive(Debug, Clone)]
pub struct Tpipe {
    /// Pipe name (up to 8 characters).
    pub name: String,
    /// Current state.
    state: TpipeState,
    /// Synchronisation level.
    pub sync_level: SyncLevel,
    /// Number of messages sent through this pipe.
    message_count: u64,
}

impl Tpipe {
    /// Create a new TPIPE.
    pub fn new(name: &str, sync_level: SyncLevel) -> ImsResult<Self> {
        if name.is_empty() || name.len() > 8 {
            return Err(ImsError::DliError {
                status: StatusCode::AD,
            });
        }
        Ok(Self {
            name: name.to_string(),
            state: TpipeState::Closed,
            sync_level,
            message_count: 0,
        })
    }

    /// Open the pipe.
    pub fn open(&mut self) {
        self.state = TpipeState::Open;
    }

    /// Close the pipe.
    pub fn close(&mut self) {
        self.state = TpipeState::Closed;
    }

    /// Return the current state.
    pub fn state(&self) -> TpipeState {
        self.state
    }

    /// Record a message being sent.
    pub fn record_message(&mut self) {
        self.message_count += 1;
    }

    /// Return the total message count.
    pub fn message_count(&self) -> u64 {
        self.message_count
    }
}

// ---------------------------------------------------------------------------
// OTMA Message Prefix
// ---------------------------------------------------------------------------

/// OTMA message prefix -- metadata attached to every OTMA message.
#[derive(Debug, Clone)]
pub struct OtmaMessagePrefix {
    /// Client ID (originating member).
    pub client_id: String,
    /// TPIPE name through which the message flows.
    pub tpipe_name: String,
    /// Synchronisation level.
    pub sync_level: SyncLevel,
    /// Transaction code (for input messages).
    pub trancode: Option<String>,
    /// Sequence number.
    pub sequence: u32,
}

impl OtmaMessagePrefix {
    /// Create a new OTMA message prefix.
    pub fn new(client_id: &str, tpipe_name: &str, sync_level: SyncLevel) -> Self {
        Self {
            client_id: client_id.to_string(),
            tpipe_name: tpipe_name.to_string(),
            sync_level,
            trancode: None,
            sequence: 0,
        }
    }

    /// Set the transaction code.
    pub fn with_trancode(mut self, trancode: &str) -> Self {
        self.trancode = Some(trancode.to_string());
        self
    }

    /// Set the sequence number.
    pub fn with_sequence(mut self, seq: u32) -> Self {
        self.sequence = seq;
        self
    }
}

// ---------------------------------------------------------------------------
// OTMA Exits
// ---------------------------------------------------------------------------

/// OTMA user exit type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OtmaExit {
    /// DFSYPRX0 -- pre-routing exit (determines destination).
    Prerouting,
    /// DFSYDRU0 -- destination resolution user exit.
    DestinationResolution,
}

impl OtmaExit {
    /// Return the exit module name.
    pub fn module_name(&self) -> &'static str {
        match self {
            OtmaExit::Prerouting => "DFSYPRX0",
            OtmaExit::DestinationResolution => "DFSYDRU0",
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // --- XCF Group ---

    #[test]
    fn test_xcf_group_new() {
        let group = XcfGroup::new("GRP1").unwrap();
        assert_eq!(group.name, "GRP1");
        assert!(group.members().is_empty());
    }

    #[test]
    fn test_xcf_group_invalid_name() {
        assert!(XcfGroup::new("").is_err());
        assert!(XcfGroup::new("TOOLONGNAME").is_err());
    }

    #[test]
    fn test_xcf_join_leave() {
        let mut group = XcfGroup::new("GRP1").unwrap();
        group.join("IMS1").unwrap();
        group.join("CLIENT1").unwrap();
        assert_eq!(group.members().len(), 2);
        assert!(group.is_member("IMS1"));

        group.leave("IMS1").unwrap();
        assert!(!group.is_member("IMS1"));
        assert_eq!(group.members().len(), 1);
    }

    #[test]
    fn test_xcf_duplicate_join() {
        let mut group = XcfGroup::new("GRP1").unwrap();
        group.join("MEM1").unwrap();
        assert!(group.join("MEM1").is_err());
    }

    #[test]
    fn test_xcf_leave_nonexistent() {
        let mut group = XcfGroup::new("GRP1").unwrap();
        assert!(group.leave("NOPE").is_err());
    }

    #[test]
    fn test_xcf_join_invalid_name() {
        let mut group = XcfGroup::new("GRP1").unwrap();
        assert!(group.join("").is_err());
        assert!(group.join("TOOLONGMM").is_err());
    }

    // --- TPIPE ---

    #[test]
    fn test_tpipe_new() {
        let tpipe = Tpipe::new("TP01", SyncLevel::Confirm).unwrap();
        assert_eq!(tpipe.name, "TP01");
        assert_eq!(tpipe.state(), TpipeState::Closed);
        assert_eq!(tpipe.sync_level, SyncLevel::Confirm);
        assert_eq!(tpipe.message_count(), 0);
    }

    #[test]
    fn test_tpipe_invalid_name() {
        assert!(Tpipe::new("", SyncLevel::None).is_err());
        assert!(Tpipe::new("TOOLONGNAME", SyncLevel::None).is_err());
    }

    #[test]
    fn test_tpipe_open_close() {
        let mut tpipe = Tpipe::new("TP01", SyncLevel::None).unwrap();
        tpipe.open();
        assert_eq!(tpipe.state(), TpipeState::Open);
        tpipe.close();
        assert_eq!(tpipe.state(), TpipeState::Closed);
    }

    #[test]
    fn test_tpipe_message_count() {
        let mut tpipe = Tpipe::new("TP01", SyncLevel::None).unwrap();
        tpipe.record_message();
        tpipe.record_message();
        assert_eq!(tpipe.message_count(), 2);
    }

    // --- Message prefix ---

    #[test]
    fn test_otma_prefix() {
        let prefix = OtmaMessagePrefix::new("CLIENT1", "TP01", SyncLevel::Syncpt)
            .with_trancode("TRAN1")
            .with_sequence(42);
        assert_eq!(prefix.client_id, "CLIENT1");
        assert_eq!(prefix.tpipe_name, "TP01");
        assert_eq!(prefix.sync_level, SyncLevel::Syncpt);
        assert_eq!(prefix.trancode.as_deref(), Some("TRAN1"));
        assert_eq!(prefix.sequence, 42);
    }

    #[test]
    fn test_otma_prefix_no_trancode() {
        let prefix = OtmaMessagePrefix::new("C1", "TP", SyncLevel::None);
        assert!(prefix.trancode.is_none());
    }

    // --- Exits ---

    #[test]
    fn test_otma_exit_names() {
        assert_eq!(OtmaExit::Prerouting.module_name(), "DFSYPRX0");
        assert_eq!(
            OtmaExit::DestinationResolution.module_name(),
            "DFSYDRU0"
        );
    }

    #[test]
    fn test_sync_level_values() {
        assert_ne!(SyncLevel::None, SyncLevel::Confirm);
        assert_ne!(SyncLevel::Confirm, SyncLevel::Syncpt);
    }
}
