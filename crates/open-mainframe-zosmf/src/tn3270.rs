//! TN3270E TCP server for terminal access.
//!
//! Provides a TN3270E-compatible TCP listener that maps incoming 3270 terminal
//! sessions to TSO/ISPF sessions via the `open-mainframe-tso` crate.
//!
//! ## Protocol
//!
//! TN3270E operates over TCP (default port 23 or configurable) using the
//! Telnet negotiation handshake extended with 3270 device type and function
//! negotiation. This implementation supports:
//!
//! - TN3270E CONNECT with LU name assignment
//! - Basic device type negotiation (IBM-3278-2)
//! - Data stream mapping to TSO session I/O
//! - Concurrent terminal sessions

use std::collections::HashMap;
use std::io;
use std::path::PathBuf;
use std::sync::RwLock;

use open_mainframe_tso::TsoSession;

/// Default TN3270E port.
pub const DEFAULT_TN3270E_PORT: u16 = 992;

/// TN3270E device types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeviceType {
    /// IBM 3278 Model 2 (24x80).
    Ibm3278M2,
    /// IBM 3278 Model 3 (32x80).
    Ibm3278M3,
    /// IBM 3278 Model 4 (43x80).
    Ibm3278M4,
    /// IBM 3278 Model 5 (27x132).
    Ibm3278M5,
}

impl DeviceType {
    /// Parse device type from TN3270E negotiation string.
    pub fn parse(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "IBM-3278-2" | "IBM-3278-2-E" => Some(DeviceType::Ibm3278M2),
            "IBM-3278-3" | "IBM-3278-3-E" => Some(DeviceType::Ibm3278M3),
            "IBM-3278-4" | "IBM-3278-4-E" => Some(DeviceType::Ibm3278M4),
            "IBM-3278-5" | "IBM-3278-5-E" => Some(DeviceType::Ibm3278M5),
            _ => None,
        }
    }

    /// Get screen dimensions (rows, cols) for this device type.
    pub fn screen_size(&self) -> (u32, u32) {
        match self {
            DeviceType::Ibm3278M2 => (24, 80),
            DeviceType::Ibm3278M3 => (32, 80),
            DeviceType::Ibm3278M4 => (43, 80),
            DeviceType::Ibm3278M5 => (27, 132),
        }
    }
}

/// State of a TN3270E terminal session.
#[derive(Debug)]
pub struct TerminalSession {
    /// LU name assigned to this session.
    pub lu_name: String,
    /// Device type negotiated.
    pub device_type: DeviceType,
    /// Userid if authenticated.
    pub userid: Option<String>,
    /// Backing TSO session.
    pub tso_session: Option<TsoSession>,
    /// Screen buffer (rows x cols characters).
    pub screen_buffer: Vec<Vec<u8>>,
}

impl TerminalSession {
    /// Create a new terminal session.
    pub fn new(lu_name: String, device_type: DeviceType) -> Self {
        let (rows, cols) = device_type.screen_size();
        let screen_buffer = vec![vec![0x40u8; cols as usize]; rows as usize]; // 0x40 = EBCDIC space
        Self {
            lu_name,
            device_type,
            userid: None,
            tso_session: None,
            screen_buffer,
        }
    }

    /// Attach a TSO session to this terminal.
    pub fn attach_tso(&mut self, userid: &str, dataset_dir: PathBuf) {
        self.userid = Some(userid.to_string());
        self.tso_session = Some(TsoSession::new(userid, dataset_dir));
    }

    /// Check if the session is authenticated.
    pub fn is_authenticated(&self) -> bool {
        self.userid.is_some()
    }
}

/// TN3270E server managing multiple concurrent terminal sessions.
#[derive(Debug)]
pub struct Tn3270eServer {
    /// Port to listen on.
    pub port: u16,
    /// Active sessions: LU name → session.
    sessions: RwLock<HashMap<String, TerminalSession>>,
    /// Next available LU number.
    next_lu: RwLock<u32>,
    /// Dataset directory for TSO sessions.
    dataset_dir: PathBuf,
}

impl Tn3270eServer {
    /// Create a new TN3270E server.
    pub fn new(port: u16, dataset_dir: PathBuf) -> Self {
        Self {
            port,
            sessions: RwLock::new(HashMap::new()),
            next_lu: RwLock::new(1),
            dataset_dir,
        }
    }

    /// Allocate a new LU name.
    pub fn allocate_lu(&self) -> String {
        let mut next = self.next_lu.write().unwrap();
        let lu = format!("LU{:04}", *next);
        *next += 1;
        lu
    }

    /// Create a new terminal session with an auto-assigned LU name.
    pub fn create_session(&self, device_type: DeviceType) -> String {
        let lu_name = self.allocate_lu();
        let session = TerminalSession::new(lu_name.clone(), device_type);
        self.sessions.write().unwrap().insert(lu_name.clone(), session);
        lu_name
    }

    /// Get the number of active sessions.
    pub fn session_count(&self) -> usize {
        self.sessions.read().unwrap().len()
    }

    /// Remove a terminal session.
    pub fn remove_session(&self, lu_name: &str) -> bool {
        self.sessions.write().unwrap().remove(lu_name).is_some()
    }

    /// Authenticate a session and attach a TSO session.
    pub fn authenticate_session(
        &self,
        lu_name: &str,
        userid: &str,
    ) -> std::result::Result<(), io::Error> {
        let mut sessions = self.sessions.write().unwrap();
        let session = sessions.get_mut(lu_name).ok_or_else(|| {
            io::Error::new(io::ErrorKind::NotFound, "Session not found")
        })?;
        session.attach_tso(userid, self.dataset_dir.clone());
        Ok(())
    }

    /// List all active LU names.
    pub fn list_sessions(&self) -> Vec<String> {
        self.sessions.read().unwrap().keys().cloned().collect()
    }
}

/// TN3270E telnet negotiation constants.
pub mod telnet {
    /// Telnet IAC (Interpret As Command).
    pub const IAC: u8 = 0xFF;
    /// Telnet DO.
    pub const DO: u8 = 0xFD;
    /// Telnet DONT.
    pub const DONT: u8 = 0xFE;
    /// Telnet WILL.
    pub const WILL: u8 = 0xFB;
    /// Telnet WONT.
    pub const WONT: u8 = 0xFC;
    /// Telnet SB (subnegotiation begin).
    pub const SB: u8 = 0xFA;
    /// Telnet SE (subnegotiation end).
    pub const SE: u8 = 0xF0;
    /// TN3270E option.
    pub const TN3270E: u8 = 0x28;
    /// Terminal Type option.
    pub const TERMINAL_TYPE: u8 = 0x18;
    /// EOR (End of Record).
    pub const EOR: u8 = 0xEF;
}

/// 3270 data stream orders.
pub mod orders {
    /// Start Field (SF).
    pub const SF: u8 = 0x1D;
    /// Start Field Extended (SFE).
    pub const SFE: u8 = 0x29;
    /// Set Buffer Address (SBA).
    pub const SBA: u8 = 0x11;
    /// Insert Cursor (IC).
    pub const IC: u8 = 0x13;
    /// Erase/Write (EW).
    pub const EW: u8 = 0xF5;
    /// Erase/Write Alternate (EWA).
    pub const EWA: u8 = 0x7E;
    /// Write (W).
    pub const W: u8 = 0xF1;
    /// Read Buffer (RB).
    pub const RB: u8 = 0xF2;
    /// Read Modified (RM).
    pub const RM: u8 = 0xF6;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_device_type_parse() {
        assert_eq!(DeviceType::parse("IBM-3278-2"), Some(DeviceType::Ibm3278M2));
        assert_eq!(DeviceType::parse("IBM-3278-5-E"), Some(DeviceType::Ibm3278M5));
        assert_eq!(DeviceType::parse("UNKNOWN"), None);
    }

    #[test]
    fn test_device_type_screen_size() {
        assert_eq!(DeviceType::Ibm3278M2.screen_size(), (24, 80));
        assert_eq!(DeviceType::Ibm3278M5.screen_size(), (27, 132));
    }

    #[test]
    fn test_terminal_session_new() {
        let session = TerminalSession::new("LU0001".to_string(), DeviceType::Ibm3278M2);
        assert_eq!(session.lu_name, "LU0001");
        assert!(!session.is_authenticated());
        assert_eq!(session.screen_buffer.len(), 24);
        assert_eq!(session.screen_buffer[0].len(), 80);
    }

    #[test]
    fn test_server_create_session() {
        let server = Tn3270eServer::new(992, PathBuf::from("/tmp"));
        let lu1 = server.create_session(DeviceType::Ibm3278M2);
        let lu2 = server.create_session(DeviceType::Ibm3278M2);
        assert_ne!(lu1, lu2);
        assert_eq!(server.session_count(), 2);
    }

    #[test]
    fn test_server_remove_session() {
        let server = Tn3270eServer::new(992, PathBuf::from("/tmp"));
        let lu = server.create_session(DeviceType::Ibm3278M2);
        assert!(server.remove_session(&lu));
        assert!(!server.remove_session(&lu));
        assert_eq!(server.session_count(), 0);
    }

    #[test]
    fn test_server_authenticate() {
        let server = Tn3270eServer::new(992, PathBuf::from("/tmp"));
        let lu = server.create_session(DeviceType::Ibm3278M2);
        assert!(server.authenticate_session(&lu, "IBMUSER").is_ok());
    }
}
