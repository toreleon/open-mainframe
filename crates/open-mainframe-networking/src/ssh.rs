//! SSH Server & TN3270E Enhancement (NET-107).
//!
//! Provides SSH server with password/public-key authentication, USS shell
//! access, enhanced TN3270E server with multi-session support, and
//! 3270 data stream processing.

use std::collections::HashMap;

use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum SshError {
    #[error("authentication failed for user '{0}'")]
    AuthFailed(String),
    #[error("no shell available for user '{0}'")]
    NoShell(String),
    #[error("session '{0}' not found")]
    SessionNotFound(String),
    #[error("server not started")]
    NotStarted,
}

#[derive(Debug, Error)]
pub enum Tn3270Error {
    #[error("TN3270E negotiation failed: {0}")]
    NegotiationFailed(String),
    #[error("session '{0}' not found")]
    SessionNotFound(String),
    #[error("invalid structured field: 0x{0:02X}")]
    InvalidStructuredField(u8),
    #[error("device type not supported: {0}")]
    UnsupportedDeviceType(String),
}

// ---------------------------------------------------------------------------
// NET-107.1 — SSH Server Authentication
// ---------------------------------------------------------------------------

/// SSH authentication method.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SshAuthMethod {
    Password,
    PublicKey,
}

/// An SSH public key entry.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AuthorizedKey {
    pub key_type: String,
    pub key_data: String,
    pub comment: String,
}

/// SSH server configuration and state.
#[derive(Debug)]
pub struct SshServer {
    pub port: u16,
    pub host_key_type: String,
    running: bool,
    /// user -> password
    credentials: HashMap<String, String>,
    /// user -> authorized keys
    authorized_keys: HashMap<String, Vec<AuthorizedKey>>,
    sessions: HashMap<String, SshSession>,
    next_session_id: u64,
}

/// An SSH session.
#[derive(Debug)]
pub struct SshSession {
    pub id: String,
    pub username: String,
    pub auth_method: SshAuthMethod,
    pub shell: Option<String>,
    pub env: HashMap<String, String>,
    active: bool,
}

impl Default for SshServer {
    fn default() -> Self {
        Self::new(22)
    }
}

impl SshServer {
    pub fn new(port: u16) -> Self {
        Self {
            port,
            host_key_type: "ssh-ed25519".to_string(),
            running: false,
            credentials: HashMap::new(),
            authorized_keys: HashMap::new(),
            sessions: HashMap::new(),
            next_session_id: 1,
        }
    }

    /// Start the SSH server.
    pub fn start(&mut self) {
        self.running = true;
    }

    /// Add a user with password authentication.
    pub fn add_user(&mut self, username: impl Into<String>, password: impl Into<String>) {
        self.credentials.insert(username.into(), password.into());
    }

    /// Add an authorized key for public-key authentication.
    pub fn add_authorized_key(&mut self, username: impl Into<String>, key: AuthorizedKey) {
        self.authorized_keys
            .entry(username.into())
            .or_default()
            .push(key);
    }

    /// Authenticate with password (RACF validation simulation).
    pub fn auth_password(
        &mut self,
        username: &str,
        password: &str,
    ) -> Result<String, SshError> {
        if !self.running {
            return Err(SshError::NotStarted);
        }
        let expected = self
            .credentials
            .get(username)
            .ok_or_else(|| SshError::AuthFailed(username.to_string()))?;
        if password != expected {
            return Err(SshError::AuthFailed(username.to_string()));
        }
        Ok(self.create_session(username, SshAuthMethod::Password))
    }

    /// Authenticate with public key.
    pub fn auth_public_key(
        &mut self,
        username: &str,
        key_data: &str,
    ) -> Result<String, SshError> {
        if !self.running {
            return Err(SshError::NotStarted);
        }
        let keys = self
            .authorized_keys
            .get(username)
            .ok_or_else(|| SshError::AuthFailed(username.to_string()))?;
        if !keys.iter().any(|k| k.key_data == key_data) {
            return Err(SshError::AuthFailed(username.to_string()));
        }
        Ok(self.create_session(username, SshAuthMethod::PublicKey))
    }

    fn create_session(&mut self, username: &str, method: SshAuthMethod) -> String {
        let id = format!("SSH-{:04}", self.next_session_id);
        self.next_session_id += 1;
        self.sessions.insert(
            id.clone(),
            SshSession {
                id: id.clone(),
                username: username.to_string(),
                auth_method: method,
                shell: None,
                env: HashMap::new(),
                active: true,
            },
        );
        id
    }

    // NET-107.2 — SSH Shell Access ----------------------------------------

    /// Request a shell for an SSH session.
    pub fn request_shell(&mut self, session_id: &str) -> Result<(), SshError> {
        let session = self
            .sessions
            .get_mut(session_id)
            .ok_or_else(|| SshError::SessionNotFound(session_id.to_string()))?;
        session.shell = Some("/bin/sh".to_string());
        session.env.insert("HOME".to_string(), format!("/u/{}", session.username));
        session.env.insert("USER".to_string(), session.username.clone());
        session.env.insert("SHELL".to_string(), "/bin/sh".to_string());
        Ok(())
    }

    /// Get a reference to a session.
    pub fn session(&self, session_id: &str) -> Option<&SshSession> {
        self.sessions.get(session_id)
    }

    /// Close an SSH session.
    pub fn close_session(&mut self, session_id: &str) -> Result<(), SshError> {
        let session = self
            .sessions
            .get_mut(session_id)
            .ok_or_else(|| SshError::SessionNotFound(session_id.to_string()))?;
        session.active = false;
        Ok(())
    }

    /// Number of active sessions.
    pub fn active_sessions(&self) -> usize {
        self.sessions.values().filter(|s| s.active).count()
    }
}

// ---------------------------------------------------------------------------
// NET-107.3 — TN3270E Server Enhancement
// ---------------------------------------------------------------------------

/// TN3270E device type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tn3270DeviceType {
    /// IBM-3278-2 (24x80)
    Ibm3278Mod2,
    /// IBM-3278-3 (32x80)
    Ibm3278Mod3,
    /// IBM-3278-4 (43x80)
    Ibm3278Mod4,
    /// IBM-3278-5 (27x132)
    Ibm3278Mod5,
    /// IBM-Dynamic
    Dynamic,
}

impl Tn3270DeviceType {
    pub fn parse(s: &str) -> Result<Self, Tn3270Error> {
        match s {
            "IBM-3278-2" | "IBM-3278-2-E" => Ok(Self::Ibm3278Mod2),
            "IBM-3278-3" | "IBM-3278-3-E" => Ok(Self::Ibm3278Mod3),
            "IBM-3278-4" | "IBM-3278-4-E" => Ok(Self::Ibm3278Mod4),
            "IBM-3278-5" | "IBM-3278-5-E" => Ok(Self::Ibm3278Mod5),
            "IBM-DYNAMIC" => Ok(Self::Dynamic),
            _ => Err(Tn3270Error::UnsupportedDeviceType(s.to_string())),
        }
    }

    pub fn screen_size(&self) -> (u16, u16) {
        match self {
            Self::Ibm3278Mod2 => (24, 80),
            Self::Ibm3278Mod3 => (32, 80),
            Self::Ibm3278Mod4 => (43, 80),
            Self::Ibm3278Mod5 => (27, 132),
            Self::Dynamic => (24, 80),
        }
    }
}

/// Target application for a TN3270E session.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tn3270Target {
    Tso,
    Cics(String),
    Ims(String),
}

/// A TN3270E session.
#[derive(Debug)]
pub struct Tn3270Session {
    pub id: String,
    pub device_type: Tn3270DeviceType,
    pub lu_name: String,
    pub target: Tn3270Target,
    pub active: bool,
}

/// Enhanced TN3270E server supporting multiple sessions per client.
#[derive(Debug)]
pub struct Tn3270Server {
    pub port: u16,
    sessions: HashMap<String, Tn3270Session>,
    next_session_id: u64,
}

impl Default for Tn3270Server {
    fn default() -> Self {
        Self::new(23)
    }
}

impl Tn3270Server {
    pub fn new(port: u16) -> Self {
        Self {
            port,
            sessions: HashMap::new(),
            next_session_id: 1,
        }
    }

    /// Negotiate and establish a TN3270E session.
    pub fn negotiate_session(
        &mut self,
        device_type_str: &str,
        lu_name: impl Into<String>,
        target: Tn3270Target,
    ) -> Result<String, Tn3270Error> {
        let device_type = Tn3270DeviceType::parse(device_type_str)?;
        let id = format!("TN-{:04}", self.next_session_id);
        self.next_session_id += 1;
        self.sessions.insert(
            id.clone(),
            Tn3270Session {
                id: id.clone(),
                device_type,
                lu_name: lu_name.into(),
                target,
                active: true,
            },
        );
        Ok(id)
    }

    /// Get a session reference.
    pub fn session(&self, id: &str) -> Option<&Tn3270Session> {
        self.sessions.get(id)
    }

    /// Close a session.
    pub fn close_session(&mut self, id: &str) -> Result<(), Tn3270Error> {
        let session = self
            .sessions
            .get_mut(id)
            .ok_or_else(|| Tn3270Error::SessionNotFound(id.to_string()))?;
        session.active = false;
        Ok(())
    }

    /// Number of active sessions.
    pub fn active_sessions(&self) -> usize {
        self.sessions.values().filter(|s| s.active).count()
    }
}

// ---------------------------------------------------------------------------
// NET-107.4 — TN3270E Data Stream Processing
// ---------------------------------------------------------------------------

/// Structured field types for 3270 data streams.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StructuredFieldType {
    QueryReply,
    ReadPartition,
    SetReplyMode,
    Outbound3270DS,
    Inbound3270DS,
}

impl StructuredFieldType {
    pub fn from_byte(byte: u8) -> Result<Self, Tn3270Error> {
        match byte {
            0x88 => Ok(Self::QueryReply),
            0x01 => Ok(Self::ReadPartition),
            0x09 => Ok(Self::SetReplyMode),
            0x40 => Ok(Self::Outbound3270DS),
            0x41 => Ok(Self::Inbound3270DS),
            _ => Err(Tn3270Error::InvalidStructuredField(byte)),
        }
    }
}

/// Query Reply data parsed from terminal capabilities.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QueryReply {
    pub usable_area: Option<UsableArea>,
    pub color_supported: bool,
    pub highlighting_supported: bool,
}

/// Usable area dimensions from Query Reply.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UsableArea {
    pub rows: u16,
    pub cols: u16,
    pub alternate_rows: u16,
    pub alternate_cols: u16,
}

/// Parse a Query Reply structured field from raw bytes.
pub fn parse_query_reply(data: &[u8]) -> QueryReply {
    let mut qr = QueryReply {
        usable_area: None,
        color_supported: false,
        highlighting_supported: false,
    };

    if data.len() >= 4 {
        qr.usable_area = Some(UsableArea {
            rows: if !data.is_empty() { u16::from(data[0]) } else { 24 },
            cols: if data.len() > 1 { u16::from(data[1]) } else { 80 },
            alternate_rows: if data.len() > 2 { u16::from(data[2]) } else { 24 },
            alternate_cols: if data.len() > 3 { u16::from(data[3]) } else { 80 },
        });
    }

    if data.len() > 4 {
        qr.color_supported = data[4] & 0x01 != 0;
        qr.highlighting_supported = data[4] & 0x02 != 0;
    }

    qr
}

// ---------------------------------------------------------------------------
// Tests — NET-107.5
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ssh_password_auth() {
        let mut server = SshServer::new(22);
        server.start();
        server.add_user("jsmith", "password123");
        let session_id = server.auth_password("jsmith", "password123").unwrap();
        assert!(session_id.starts_with("SSH-"));
        let session = server.session(&session_id).unwrap();
        assert_eq!(session.auth_method, SshAuthMethod::Password);
    }

    #[test]
    fn ssh_password_auth_failed() {
        let mut server = SshServer::new(22);
        server.start();
        server.add_user("jsmith", "password123");
        assert!(server.auth_password("jsmith", "wrong").is_err());
    }

    #[test]
    fn ssh_pubkey_auth() {
        let mut server = SshServer::new(22);
        server.start();
        server.add_authorized_key(
            "jsmith",
            AuthorizedKey {
                key_type: "ssh-ed25519".to_string(),
                key_data: "AAAAC3NzaC1lZDI1NTE5AAAAIExampleKey".to_string(),
                comment: "jsmith@workstation".to_string(),
            },
        );
        let sid = server
            .auth_public_key("jsmith", "AAAAC3NzaC1lZDI1NTE5AAAAIExampleKey")
            .unwrap();
        let session = server.session(&sid).unwrap();
        assert_eq!(session.auth_method, SshAuthMethod::PublicKey);
    }

    #[test]
    fn ssh_pubkey_auth_wrong_key() {
        let mut server = SshServer::new(22);
        server.start();
        server.add_authorized_key(
            "jsmith",
            AuthorizedKey {
                key_type: "ssh-ed25519".to_string(),
                key_data: "correct_key".to_string(),
                comment: "".to_string(),
            },
        );
        assert!(server.auth_public_key("jsmith", "wrong_key").is_err());
    }

    #[test]
    fn ssh_shell_access() {
        let mut server = SshServer::new(22);
        server.start();
        server.add_user("jsmith", "pass");
        let sid = server.auth_password("jsmith", "pass").unwrap();
        server.request_shell(&sid).unwrap();
        let session = server.session(&sid).unwrap();
        assert_eq!(session.shell, Some("/bin/sh".to_string()));
        assert_eq!(session.env.get("HOME"), Some(&"/u/jsmith".to_string()));
        assert_eq!(session.env.get("USER"), Some(&"jsmith".to_string()));
    }

    #[test]
    fn ssh_close_session() {
        let mut server = SshServer::new(22);
        server.start();
        server.add_user("jsmith", "pass");
        let sid = server.auth_password("jsmith", "pass").unwrap();
        assert_eq!(server.active_sessions(), 1);
        server.close_session(&sid).unwrap();
        assert_eq!(server.active_sessions(), 0);
    }

    #[test]
    fn ssh_server_not_started() {
        let mut server = SshServer::new(22);
        server.add_user("jsmith", "pass");
        assert!(server.auth_password("jsmith", "pass").is_err());
    }

    #[test]
    fn tn3270e_negotiate_session() {
        let mut server = Tn3270Server::new(23);
        let id = server
            .negotiate_session("IBM-3278-2", "LU0001", Tn3270Target::Tso)
            .unwrap();
        let session = server.session(&id).unwrap();
        assert_eq!(session.device_type, Tn3270DeviceType::Ibm3278Mod2);
        assert_eq!(session.target, Tn3270Target::Tso);
    }

    #[test]
    fn tn3270e_multiple_sessions() {
        let mut server = Tn3270Server::new(23);
        let id1 = server
            .negotiate_session("IBM-3278-2", "LU0001", Tn3270Target::Tso)
            .unwrap();
        let id2 = server
            .negotiate_session("IBM-3278-5", "LU0002", Tn3270Target::Cics("CICSPROD".to_string()))
            .unwrap();
        assert_ne!(id1, id2);
        assert_eq!(server.active_sessions(), 2);
        let s1 = server.session(&id1).unwrap();
        let s2 = server.session(&id2).unwrap();
        assert_eq!(s1.device_type.screen_size(), (24, 80));
        assert_eq!(s2.device_type.screen_size(), (27, 132));
    }

    #[test]
    fn tn3270e_unsupported_device_type() {
        let mut server = Tn3270Server::new(23);
        assert!(server
            .negotiate_session("IBM-UNKNOWN", "LU0001", Tn3270Target::Tso)
            .is_err());
    }

    #[test]
    fn tn3270e_close_session() {
        let mut server = Tn3270Server::new(23);
        let id = server
            .negotiate_session("IBM-3278-2", "LU0001", Tn3270Target::Tso)
            .unwrap();
        server.close_session(&id).unwrap();
        assert_eq!(server.active_sessions(), 0);
    }

    #[test]
    fn structured_field_parsing() {
        assert_eq!(
            StructuredFieldType::from_byte(0x88).unwrap(),
            StructuredFieldType::QueryReply
        );
        assert_eq!(
            StructuredFieldType::from_byte(0x01).unwrap(),
            StructuredFieldType::ReadPartition
        );
        assert!(StructuredFieldType::from_byte(0xFF).is_err());
    }

    #[test]
    fn query_reply_parsing() {
        let data = [24u8, 80, 43, 80, 0x03];
        let qr = parse_query_reply(&data);
        let area = qr.usable_area.unwrap();
        assert_eq!(area.rows, 24);
        assert_eq!(area.cols, 80);
        assert_eq!(area.alternate_rows, 43);
        assert_eq!(area.alternate_cols, 80);
        assert!(qr.color_supported);
        assert!(qr.highlighting_supported);
    }

    #[test]
    fn device_type_screen_sizes() {
        assert_eq!(Tn3270DeviceType::Ibm3278Mod2.screen_size(), (24, 80));
        assert_eq!(Tn3270DeviceType::Ibm3278Mod3.screen_size(), (32, 80));
        assert_eq!(Tn3270DeviceType::Ibm3278Mod4.screen_size(), (43, 80));
        assert_eq!(Tn3270DeviceType::Ibm3278Mod5.screen_size(), (27, 132));
    }
}
