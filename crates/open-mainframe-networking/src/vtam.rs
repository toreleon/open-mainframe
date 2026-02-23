//! VTAM Application Interface (NET-100).
//!
//! Provides ACB/RPL/NIB/EXLST abstractions for SNA session management,
//! enabling CICS and IMS terminal connectivity. Includes MODCB/SHOWCB/TESTCB
//! for runtime control block inspection and APPL definition parsing.

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
    #[error("RPL '{0}' not found")]
    RplNotFound(String),
    #[error("invalid APPL definition: {0}")]
    InvalidApplDef(String),
    #[error("EXLST exit '{0}' not registered")]
    ExitNotRegistered(String),
}

// ---------------------------------------------------------------------------
// RPL — Request Parameter List (NET-100.1b)
// ---------------------------------------------------------------------------

/// RPL operation type — the I/O verb this RPL describes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RplOperation {
    Send,
    Receive,
    OpenDst,
    CloseDst,
    SetLogon,
    ResetSr,
}

/// RPL return code from a completed operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RplReturnCode {
    Ok,
    Error(u32),
    Pending,
}

/// Request Parameter List — describes each VTAM I/O request.
#[derive(Debug, Clone)]
pub struct Rpl {
    pub id: String,
    pub operation: RplOperation,
    pub session_id: Option<String>,
    pub data_length: u32,
    pub return_code: RplReturnCode,
    pub post_flag: bool,
}

impl Rpl {
    pub fn new(id: impl Into<String>, operation: RplOperation) -> Self {
        Self {
            id: id.into(),
            operation,
            session_id: None,
            data_length: 0,
            return_code: RplReturnCode::Pending,
            post_flag: false,
        }
    }

    /// Mark RPL as completed successfully.
    pub fn complete(&mut self, data_length: u32) {
        self.return_code = RplReturnCode::Ok;
        self.data_length = data_length;
        self.post_flag = true;
    }

    /// Mark RPL as completed with error.
    pub fn fail(&mut self, code: u32) {
        self.return_code = RplReturnCode::Error(code);
        self.post_flag = true;
    }

    /// Check if the RPL has completed (posted).
    pub fn is_complete(&self) -> bool {
        self.post_flag
    }
}

// ---------------------------------------------------------------------------
// NIB — Node Initialization Block (NET-100.1c)
// ---------------------------------------------------------------------------

/// NIB processing mode for session establishment.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NibProcMode {
    /// Application will explicitly accept logon (OPNDST OPTCD=ACCEPT).
    Accept,
    /// Application acquires the session (OPNDST OPTCD=ACQUIRE).
    Acquire,
}

/// Node Initialization Block — parameters for initiating/accepting sessions.
#[derive(Debug, Clone)]
pub struct Nib {
    pub name: String,
    pub lu_type: LuType,
    pub proc_mode: NibProcMode,
    pub logmode: String,
    pub bind_data: Option<Vec<u8>>,
}

impl Nib {
    pub fn new(name: impl Into<String>, lu_type: LuType) -> Self {
        Self {
            name: name.into(),
            lu_type,
            proc_mode: NibProcMode::Accept,
            logmode: String::new(),
            bind_data: None,
        }
    }

    /// Set the LOGMODE entry name for session parameters.
    pub fn set_logmode(&mut self, logmode: impl Into<String>) {
        self.logmode = logmode.into();
    }
}

// ---------------------------------------------------------------------------
// EXLST — Exit List (NET-100.1d)
// ---------------------------------------------------------------------------

/// Exit types that VTAM can invoke asynchronously.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExitType {
    /// LOGON exit — terminal requests session.
    Logon,
    /// LOSTERM exit — session lost unexpectedly.
    Losterm,
    /// SCIP exit — session control input pending.
    Scip,
    /// DFASY exit — deferred asynchronous event.
    Dfasy,
    /// TPEND exit — VTAM is shutting down.
    Tpend,
    /// RELREQ exit — release requested.
    Relreq,
}

/// A registered exit entry in an EXLST.
#[derive(Debug, Clone)]
pub struct ExitEntry {
    pub exit_type: ExitType,
    pub active: bool,
    pub invocation_count: u64,
}

/// Exit List — a set of exit routines registered for asynchronous events.
#[derive(Debug, Default)]
pub struct Exlst {
    exits: HashMap<ExitType, ExitEntry>,
}

impl Exlst {
    pub fn new() -> Self {
        Self {
            exits: HashMap::new(),
        }
    }

    /// Register an exit routine.
    pub fn register(&mut self, exit_type: ExitType) {
        self.exits.insert(
            exit_type,
            ExitEntry {
                exit_type,
                active: true,
                invocation_count: 0,
            },
        );
    }

    /// Deactivate an exit.
    pub fn deactivate(&mut self, exit_type: ExitType) -> Result<(), VtamError> {
        let entry = self
            .exits
            .get_mut(&exit_type)
            .ok_or_else(|| VtamError::ExitNotRegistered(format!("{:?}", exit_type)))?;
        entry.active = false;
        Ok(())
    }

    /// Drive (simulate invoking) an exit.
    pub fn drive(&mut self, exit_type: ExitType) -> Result<bool, VtamError> {
        let entry = self
            .exits
            .get_mut(&exit_type)
            .ok_or_else(|| VtamError::ExitNotRegistered(format!("{:?}", exit_type)))?;
        if !entry.active {
            return Ok(false);
        }
        entry.invocation_count += 1;
        Ok(true)
    }

    /// Check if an exit is registered and active.
    pub fn is_active(&self, exit_type: ExitType) -> bool {
        self.exits
            .get(&exit_type)
            .map(|e| e.active)
            .unwrap_or(false)
    }

    /// Get the invocation count for an exit.
    pub fn invocation_count(&self, exit_type: ExitType) -> u64 {
        self.exits
            .get(&exit_type)
            .map(|e| e.invocation_count)
            .unwrap_or(0)
    }
}

// ---------------------------------------------------------------------------
// MODCB / SHOWCB / TESTCB (NET-100.1e)
// ---------------------------------------------------------------------------

/// Fields that can be inspected/modified on an ACB via MODCB/SHOWCB/TESTCB.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AcbField {
    Applid,
    Macrf,
    Auth,
    IsOpen,
    LogonEnabled,
    SessionCount,
}

/// SHOWCB — inspect an ACB field at runtime.
pub fn showcb(acb: &Acb, field: AcbField) -> String {
    match field {
        AcbField::Applid => acb.applid.clone(),
        AcbField::Macrf => format!("{:?}", acb.macrf),
        AcbField::Auth => format!("{:?}", acb.auth),
        AcbField::IsOpen => acb.is_open().to_string(),
        AcbField::LogonEnabled => acb.logon_enabled.to_string(),
        AcbField::SessionCount => acb.active_session_count().to_string(),
    }
}

/// TESTCB — test an ACB field against an expected value. Returns `true` if matched.
pub fn testcb(acb: &Acb, field: AcbField, expected: &str) -> bool {
    showcb(acb, field) == expected
}

// ---------------------------------------------------------------------------
// APPL Definition (NET-100.1f)
// ---------------------------------------------------------------------------

/// An APPL definition parsed from SYS1.VTAMLST.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ApplDef {
    pub name: String,
    pub auth: AcbAuth,
    pub parsess: u32,
    pub sonscip: bool,
    pub modetab: String,
}

impl ApplDef {
    /// Parse an APPL definition line, e.g.:
    /// `CICSAPPL APPL AUTH=(ACQ) PARSESS=100 SONSCIP=YES MODETAB=ISTINCLM`
    pub fn parse(line: &str) -> Result<Self, VtamError> {
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() < 2 || parts[1].to_uppercase() != "APPL" {
            return Err(VtamError::InvalidApplDef(
                "expected '<name> APPL ...'".to_string(),
            ));
        }
        let name = parts[0].to_string();
        let mut auth = AcbAuth::Primary;
        let mut parsess = 0u32;
        let mut sonscip = false;
        let mut modetab = String::new();

        for part in &parts[2..] {
            let upper = part.to_uppercase();
            if upper.starts_with("AUTH=") {
                let val = &upper[5..];
                if val.contains("ACQ") {
                    auth = AcbAuth::Acq;
                }
            } else if upper.starts_with("PARSESS=") {
                parsess = upper[8..]
                    .parse()
                    .map_err(|_| VtamError::InvalidApplDef("bad PARSESS".to_string()))?;
            } else if upper.starts_with("SONSCIP=") {
                sonscip = &upper[8..] == "YES";
            } else if upper.starts_with("MODETAB=") {
                modetab = part[8..].to_string();
            }
        }

        Ok(ApplDef {
            name,
            auth,
            parsess,
            sonscip,
            modetab,
        })
    }
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

    // RPL tests
    #[test]
    fn rpl_create_and_complete() {
        let mut rpl = Rpl::new("RPL001", RplOperation::Send);
        assert!(!rpl.is_complete());
        assert_eq!(rpl.return_code, RplReturnCode::Pending);
        rpl.complete(256);
        assert!(rpl.is_complete());
        assert_eq!(rpl.data_length, 256);
        assert_eq!(rpl.return_code, RplReturnCode::Ok);
    }

    #[test]
    fn rpl_fail() {
        let mut rpl = Rpl::new("RPL002", RplOperation::Receive);
        rpl.fail(8);
        assert!(rpl.is_complete());
        assert_eq!(rpl.return_code, RplReturnCode::Error(8));
    }

    // NIB tests
    #[test]
    fn nib_create_and_set_logmode() {
        let mut nib = Nib::new("TERM0001", LuType::Lu2);
        assert_eq!(nib.proc_mode, NibProcMode::Accept);
        nib.set_logmode("SNX32705");
        assert_eq!(nib.logmode, "SNX32705");
    }

    // EXLST tests
    #[test]
    fn exlst_register_and_drive() {
        let mut exlst = Exlst::new();
        exlst.register(ExitType::Logon);
        exlst.register(ExitType::Losterm);
        assert!(exlst.is_active(ExitType::Logon));
        assert!(exlst.drive(ExitType::Logon).unwrap());
        assert_eq!(exlst.invocation_count(ExitType::Logon), 1);
        exlst.drive(ExitType::Logon).unwrap();
        assert_eq!(exlst.invocation_count(ExitType::Logon), 2);
    }

    #[test]
    fn exlst_deactivate() {
        let mut exlst = Exlst::new();
        exlst.register(ExitType::Logon);
        exlst.deactivate(ExitType::Logon).unwrap();
        assert!(!exlst.is_active(ExitType::Logon));
        assert!(!exlst.drive(ExitType::Logon).unwrap());
    }

    #[test]
    fn exlst_unregistered_exit_errors() {
        let mut exlst = Exlst::new();
        assert!(exlst.drive(ExitType::Tpend).is_err());
    }

    // SHOWCB / TESTCB tests
    #[test]
    fn showcb_acb_fields() {
        let mut acb = Acb::new("CICSAPPL", AcbMacrf::Logon, AcbAuth::Primary);
        assert_eq!(showcb(&acb, AcbField::Applid), "CICSAPPL");
        assert_eq!(showcb(&acb, AcbField::IsOpen), "false");
        acb.open().unwrap();
        assert_eq!(showcb(&acb, AcbField::IsOpen), "true");
    }

    #[test]
    fn testcb_matches() {
        let acb = Acb::new("CICSAPPL", AcbMacrf::Logon, AcbAuth::Primary);
        assert!(testcb(&acb, AcbField::Applid, "CICSAPPL"));
        assert!(!testcb(&acb, AcbField::Applid, "IMSAPPL"));
    }

    // APPL definition tests
    #[test]
    fn appl_def_parse() {
        let def =
            ApplDef::parse("CICSAPPL APPL AUTH=(ACQ) PARSESS=100 SONSCIP=YES MODETAB=ISTINCLM")
                .unwrap();
        assert_eq!(def.name, "CICSAPPL");
        assert_eq!(def.auth, AcbAuth::Acq);
        assert_eq!(def.parsess, 100);
        assert!(def.sonscip);
        assert_eq!(def.modetab, "ISTINCLM");
    }

    #[test]
    fn appl_def_parse_minimal() {
        let def = ApplDef::parse("MYAPPL APPL").unwrap();
        assert_eq!(def.name, "MYAPPL");
        assert_eq!(def.auth, AcbAuth::Primary);
        assert_eq!(def.parsess, 0);
    }

    #[test]
    fn appl_def_parse_invalid() {
        assert!(ApplDef::parse("NOTANAPPL").is_err());
        assert!(ApplDef::parse("NAME NOTAPPL").is_err());
    }
}
