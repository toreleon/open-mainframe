//! VTAM Application Interface (NET-100).
//!
//! Provides ACB/RPL/NIB abstractions for SNA session management,
//! enabling CICS and IMS terminal connectivity.

use std::collections::HashMap;

use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum VtamError {
    #[error("ACB '{0}' is already open")]
    AcbAlreadyOpen(String),
    #[error("ACB '{0}' is not open")]
    AcbNotOpen(String),
    #[error("session '{0}' not found")]
    SessionNotFound(String),
    #[error("session '{0}' is not active")]
    SessionNotActive(String),
    #[error("SETLOGON not enabled for ACB '{0}'")]
    SetlogonNotEnabled(String),
    #[error("no pending logon requests")]
    NoPendingLogon,
    #[error("receive buffer is empty")]
    ReceiveBufferEmpty,
}

// ---------------------------------------------------------------------------
// ACB — Application Control Block (NET-100.1)
// ---------------------------------------------------------------------------

/// MACRF — macro format options for ACB.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AcbMacrf {
    Logon,
    Nlogon,
}

/// AUTH — authorization level for ACB.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AcbAuth {
    /// Primary application — can own sessions.
    Primary,
    /// ACQ application — can acquire sessions.
    Acq,
}

/// The VTAM Application Control Block.
#[derive(Debug)]
pub struct Acb {
    pub applid: String,
    pub macrf: AcbMacrf,
    pub auth: AcbAuth,
    state: AcbState,
    logon_enabled: bool,
    sessions: HashMap<String, Session>,
    pending_logons: Vec<String>,
    next_session_id: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AcbState {
    Closed,
    Open,
}

impl Acb {
    /// Create a new ACB for the given APPLID.
    pub fn new(applid: impl Into<String>, macrf: AcbMacrf, auth: AcbAuth) -> Self {
        Self {
            applid: applid.into(),
            macrf,
            auth,
            state: AcbState::Closed,
            logon_enabled: false,
            sessions: HashMap::new(),
            pending_logons: Vec::new(),
            next_session_id: 1,
        }
    }

    /// OPEN ACB — register the application with VTAM.
    pub fn open(&mut self) -> Result<(), VtamError> {
        if self.state == AcbState::Open {
            return Err(VtamError::AcbAlreadyOpen(self.applid.clone()));
        }
        self.state = AcbState::Open;
        Ok(())
    }

    /// CLOSE ACB — deregister and terminate all sessions.
    pub fn close(&mut self) -> Result<(), VtamError> {
        if self.state == AcbState::Closed {
            return Err(VtamError::AcbNotOpen(self.applid.clone()));
        }
        self.sessions.clear();
        self.pending_logons.clear();
        self.logon_enabled = false;
        self.state = AcbState::Closed;
        Ok(())
    }

    /// Returns `true` when the ACB is open.
    pub fn is_open(&self) -> bool {
        self.state == AcbState::Open
    }

    // NET-100.2 — SETLOGON -----------------------------------------------

    /// SETLOGON OPTCD=START — accept incoming session requests.
    pub fn setlogon_start(&mut self) -> Result<(), VtamError> {
        if self.state != AcbState::Open {
            return Err(VtamError::AcbNotOpen(self.applid.clone()));
        }
        self.logon_enabled = true;
        Ok(())
    }

    /// SETLOGON OPTCD=STOP — stop accepting incoming session requests.
    pub fn setlogon_stop(&mut self) -> Result<(), VtamError> {
        if self.state != AcbState::Open {
            return Err(VtamError::AcbNotOpen(self.applid.clone()));
        }
        self.logon_enabled = false;
        Ok(())
    }

    /// Simulate a terminal requesting logon to this APPLID.
    pub fn queue_logon(&mut self, lu_name: impl Into<String>) -> Result<(), VtamError> {
        if !self.logon_enabled {
            return Err(VtamError::SetlogonNotEnabled(self.applid.clone()));
        }
        self.pending_logons.push(lu_name.into());
        Ok(())
    }

    /// Accept a pending logon and create a session (OPNDST-equivalent).
    pub fn accept_session(&mut self, lu_type: LuType) -> Result<String, VtamError> {
        let lu_name = self
            .pending_logons
            .pop()
            .ok_or(VtamError::NoPendingLogon)?;
        let session_id = format!("S{:04}", self.next_session_id);
        self.next_session_id += 1;
        let session = Session {
            id: session_id.clone(),
            lu_name,
            lu_type,
            state: SessionState::Active,
            recv_buffer: Vec::new(),
        };
        self.sessions.insert(session_id.clone(), session);
        Ok(session_id)
    }

    // NET-100.3 — SEND / RECEIVE ------------------------------------------

    /// SEND RPL — transmit data on a session.
    pub fn send(&mut self, session_id: &str, data: &[u8]) -> Result<(), VtamError> {
        let session = self
            .sessions
            .get_mut(session_id)
            .ok_or_else(|| VtamError::SessionNotFound(session_id.to_string()))?;
        if session.state != SessionState::Active {
            return Err(VtamError::SessionNotActive(session_id.to_string()));
        }
        // In a real implementation data would go over the wire;
        // here we loop it into the receive buffer for testing.
        session.recv_buffer.extend_from_slice(data);
        Ok(())
    }

    /// RECEIVE RPL — read data from a session.
    pub fn receive(&mut self, session_id: &str) -> Result<Vec<u8>, VtamError> {
        let session = self
            .sessions
            .get_mut(session_id)
            .ok_or_else(|| VtamError::SessionNotFound(session_id.to_string()))?;
        if session.state != SessionState::Active {
            return Err(VtamError::SessionNotActive(session_id.to_string()));
        }
        if session.recv_buffer.is_empty() {
            return Err(VtamError::ReceiveBufferEmpty);
        }
        Ok(std::mem::take(&mut session.recv_buffer))
    }

    // NET-100.4 — CLSDST --------------------------------------------------

    /// CLSDST — terminate an individual session and free resources.
    pub fn close_session(&mut self, session_id: &str) -> Result<(), VtamError> {
        let session = self
            .sessions
            .get_mut(session_id)
            .ok_or_else(|| VtamError::SessionNotFound(session_id.to_string()))?;
        session.state = SessionState::Closed;
        self.sessions.remove(session_id);
        Ok(())
    }

    /// Number of active sessions.
    pub fn active_session_count(&self) -> usize {
        self.sessions
            .values()
            .filter(|s| s.state == SessionState::Active)
            .count()
    }

    /// Returns a reference to a session by id.
    pub fn session(&self, session_id: &str) -> Option<&Session> {
        self.sessions.get(session_id)
    }
}

// ---------------------------------------------------------------------------
// SNA LU Types (shared with NET-101)
// ---------------------------------------------------------------------------

/// SNA Logical Unit types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LuType {
    /// LU 0 — unformatted data exchange.
    Lu0,
    /// LU 1 — SCS printer.
    Lu1,
    /// LU 2 — 3270 terminal.
    Lu2,
    /// LU 3 — DSC printer.
    Lu3,
    /// LU 6.2 — APPC program-to-program.
    Lu62,
}

// ---------------------------------------------------------------------------
// Session
// ---------------------------------------------------------------------------

/// State of an SNA session.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SessionState {
    Pending,
    Active,
    Closed,
}

/// An SNA session between an application and a logical unit.
#[derive(Debug)]
pub struct Session {
    pub id: String,
    pub lu_name: String,
    pub lu_type: LuType,
    pub state: SessionState,
    recv_buffer: Vec<u8>,
}

// ---------------------------------------------------------------------------
// Tests — NET-100.5
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn acb_open_close() {
        let mut acb = Acb::new("CICSAPPL", AcbMacrf::Logon, AcbAuth::Primary);
        assert!(!acb.is_open());
        acb.open().unwrap();
        assert!(acb.is_open());
        acb.close().unwrap();
        assert!(!acb.is_open());
    }

    #[test]
    fn acb_double_open_errors() {
        let mut acb = Acb::new("CICSAPPL", AcbMacrf::Logon, AcbAuth::Primary);
        acb.open().unwrap();
        assert!(acb.open().is_err());
    }

    #[test]
    fn acb_close_when_closed_errors() {
        let mut acb = Acb::new("CICSAPPL", AcbMacrf::Logon, AcbAuth::Primary);
        assert!(acb.close().is_err());
    }

    #[test]
    fn setlogon_and_session_initiation() {
        let mut acb = Acb::new("CICSAPPL", AcbMacrf::Logon, AcbAuth::Primary);
        acb.open().unwrap();
        acb.setlogon_start().unwrap();
        acb.queue_logon("TERM0001").unwrap();
        let sid = acb.accept_session(LuType::Lu2).unwrap();
        assert_eq!(acb.active_session_count(), 1);
        let session = acb.session(&sid).unwrap();
        assert_eq!(session.lu_name, "TERM0001");
        assert_eq!(session.lu_type, LuType::Lu2);
    }

    #[test]
    fn logon_without_setlogon_errors() {
        let mut acb = Acb::new("CICSAPPL", AcbMacrf::Logon, AcbAuth::Primary);
        acb.open().unwrap();
        assert!(acb.queue_logon("TERM0001").is_err());
    }

    #[test]
    fn send_receive_data() {
        let mut acb = Acb::new("CICSAPPL", AcbMacrf::Logon, AcbAuth::Primary);
        acb.open().unwrap();
        acb.setlogon_start().unwrap();
        acb.queue_logon("TERM0001").unwrap();
        let sid = acb.accept_session(LuType::Lu2).unwrap();

        acb.send(&sid, b"Hello terminal").unwrap();
        let data = acb.receive(&sid).unwrap();
        assert_eq!(data, b"Hello terminal");
    }

    #[test]
    fn receive_empty_buffer_errors() {
        let mut acb = Acb::new("CICSAPPL", AcbMacrf::Logon, AcbAuth::Primary);
        acb.open().unwrap();
        acb.setlogon_start().unwrap();
        acb.queue_logon("TERM0001").unwrap();
        let sid = acb.accept_session(LuType::Lu2).unwrap();
        assert!(acb.receive(&sid).is_err());
    }

    #[test]
    fn clsdst_terminates_session() {
        let mut acb = Acb::new("CICSAPPL", AcbMacrf::Logon, AcbAuth::Primary);
        acb.open().unwrap();
        acb.setlogon_start().unwrap();
        acb.queue_logon("TERM0001").unwrap();
        let sid = acb.accept_session(LuType::Lu2).unwrap();
        assert_eq!(acb.active_session_count(), 1);
        acb.close_session(&sid).unwrap();
        assert_eq!(acb.active_session_count(), 0);
    }

    #[test]
    fn close_acb_terminates_all_sessions() {
        let mut acb = Acb::new("CICSAPPL", AcbMacrf::Logon, AcbAuth::Primary);
        acb.open().unwrap();
        acb.setlogon_start().unwrap();
        acb.queue_logon("TERM0001").unwrap();
        acb.queue_logon("TERM0002").unwrap();
        acb.accept_session(LuType::Lu2).unwrap();
        acb.accept_session(LuType::Lu2).unwrap();
        assert_eq!(acb.active_session_count(), 2);
        acb.close().unwrap();
        assert_eq!(acb.active_session_count(), 0);
    }

    #[test]
    fn send_to_nonexistent_session_errors() {
        let mut acb = Acb::new("CICSAPPL", AcbMacrf::Logon, AcbAuth::Primary);
        acb.open().unwrap();
        assert!(acb.send("NOSESSION", b"data").is_err());
    }

    #[test]
    fn multiple_sessions_independent() {
        let mut acb = Acb::new("IMSAPPL", AcbMacrf::Logon, AcbAuth::Primary);
        acb.open().unwrap();
        acb.setlogon_start().unwrap();
        acb.queue_logon("TERM0001").unwrap();
        acb.queue_logon("TERM0002").unwrap();
        let s1 = acb.accept_session(LuType::Lu2).unwrap();
        let s2 = acb.accept_session(LuType::Lu0).unwrap();
        acb.send(&s1, b"data-for-1").unwrap();
        acb.send(&s2, b"data-for-2").unwrap();
        assert_eq!(acb.receive(&s1).unwrap(), b"data-for-1");
        assert_eq!(acb.receive(&s2).unwrap(), b"data-for-2");
    }
}
