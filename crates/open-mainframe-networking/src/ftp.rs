//! FTP Client/Server (NET-106).
//!
//! Provides FTP server core (authentication, commands), USS file transfer,
//! MVS dataset transfer, JES2 job submission, and an FTP client.

use std::collections::HashMap;

use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum FtpError {
    #[error("authentication failed for user '{0}'")]
    AuthFailed(String),
    #[error("not authenticated")]
    NotAuthenticated,
    #[error("file not found: {0}")]
    FileNotFound(String),
    #[error("invalid command: {0}")]
    InvalidCommand(String),
    #[error("transfer failed: {0}")]
    TransferFailed(String),
    #[error("not connected")]
    NotConnected,
    #[error("JES2 submission failed: {0}")]
    JesSubmitFailed(String),
    #[error("dataset not found: {0}")]
    DatasetNotFound(String),
}

// ---------------------------------------------------------------------------
// FTP Reply Codes
// ---------------------------------------------------------------------------

/// Standard FTP reply codes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FtpReply {
    pub code: u16,
    pub message: String,
}

impl FtpReply {
    pub fn new(code: u16, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
        }
    }

    pub fn is_success(&self) -> bool {
        (200..300).contains(&self.code)
    }

    pub fn is_positive_completion(&self) -> bool {
        self.code >= 200 && self.code < 300
    }
}

// ---------------------------------------------------------------------------
// NET-106.1 — FTP Server Core
// ---------------------------------------------------------------------------

/// Transfer mode for FTP.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TransferMode {
    Ascii,
    Binary,
}

/// File type for SITE command.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SiteFileType {
    /// Unix file system.
    Uss,
    /// Sequential dataset.
    Seq,
    /// JES spool.
    Jes,
}

/// FTP session state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FtpSessionState {
    Connected,
    UserProvided,
    Authenticated,
    Disconnected,
}

/// An FTP server instance.
#[derive(Debug)]
pub struct FtpServer {
    pub port: u16,
    pub hostname: String,
    sessions: HashMap<u64, FtpSession>,
    /// Simulated user credentials (user -> password).
    credentials: HashMap<String, String>,
    /// Simulated USS filesystem (path -> content).
    uss_files: HashMap<String, Vec<u8>>,
    /// Simulated MVS datasets (dsname -> content).
    mvs_datasets: HashMap<String, Vec<u8>>,
    next_session_id: u64,
    next_job_number: u32,
}

/// An individual FTP session.
#[derive(Debug)]
pub struct FtpSession {
    pub id: u64,
    pub username: String,
    state: FtpSessionState,
    pub transfer_mode: TransferMode,
    pub site_filetype: SiteFileType,
    pub current_dir: String,
}

impl Default for FtpServer {
    fn default() -> Self {
        Self::new(21, "localhost")
    }
}

impl FtpServer {
    pub fn new(port: u16, hostname: impl Into<String>) -> Self {
        Self {
            port,
            hostname: hostname.into(),
            sessions: HashMap::new(),
            credentials: HashMap::new(),
            uss_files: HashMap::new(),
            mvs_datasets: HashMap::new(),
            next_session_id: 1,
            next_job_number: 1000,
        }
    }

    /// Register a user for authentication.
    pub fn add_user(&mut self, username: impl Into<String>, password: impl Into<String>) {
        self.credentials.insert(username.into(), password.into());
    }

    /// Add a simulated USS file.
    pub fn add_uss_file(&mut self, path: impl Into<String>, content: Vec<u8>) {
        self.uss_files.insert(path.into(), content);
    }

    /// Add a simulated MVS dataset.
    pub fn add_mvs_dataset(&mut self, dsname: impl Into<String>, content: Vec<u8>) {
        self.mvs_datasets.insert(dsname.into(), content);
    }

    /// Accept a new FTP connection.
    pub fn accept_connection(&mut self) -> (u64, FtpReply) {
        let id = self.next_session_id;
        self.next_session_id += 1;
        self.sessions.insert(
            id,
            FtpSession {
                id,
                username: String::new(),
                state: FtpSessionState::Connected,
                transfer_mode: TransferMode::Ascii,
                site_filetype: SiteFileType::Uss,
                current_dir: "/".to_string(),
            },
        );
        (
            id,
            FtpReply::new(220, format!("{} FTP server ready", self.hostname)),
        )
    }

    /// Process USER command.
    pub fn cmd_user(&mut self, session_id: u64, username: &str) -> Result<FtpReply, FtpError> {
        let session = self
            .sessions
            .get_mut(&session_id)
            .ok_or(FtpError::NotConnected)?;
        session.username = username.to_string();
        session.state = FtpSessionState::UserProvided;
        Ok(FtpReply::new(331, "User name okay, need password"))
    }

    /// Process PASS command.
    pub fn cmd_pass(&mut self, session_id: u64, password: &str) -> Result<FtpReply, FtpError> {
        let session = self
            .sessions
            .get_mut(&session_id)
            .ok_or(FtpError::NotConnected)?;
        if session.state != FtpSessionState::UserProvided {
            return Ok(FtpReply::new(503, "Login with USER first"));
        }
        let expected = self
            .credentials
            .get(&session.username)
            .ok_or_else(|| FtpError::AuthFailed(session.username.clone()))?
            .clone();
        if password != expected {
            return Err(FtpError::AuthFailed(session.username.clone()));
        }
        session.state = FtpSessionState::Authenticated;
        Ok(FtpReply::new(230, "User logged in, proceed"))
    }

    // NET-106.2 — USS File Transfer --------------------------------------

    /// GET a USS file.
    pub fn cmd_retr_uss(
        &self,
        session_id: u64,
        path: &str,
    ) -> Result<(FtpReply, Vec<u8>), FtpError> {
        let session = self
            .sessions
            .get(&session_id)
            .ok_or(FtpError::NotConnected)?;
        if session.state != FtpSessionState::Authenticated {
            return Err(FtpError::NotAuthenticated);
        }
        let content = self
            .uss_files
            .get(path)
            .ok_or_else(|| FtpError::FileNotFound(path.to_string()))?
            .clone();
        Ok((
            FtpReply::new(226, "Transfer complete"),
            content,
        ))
    }

    /// PUT a USS file.
    pub fn cmd_stor_uss(
        &mut self,
        session_id: u64,
        path: &str,
        data: Vec<u8>,
    ) -> Result<FtpReply, FtpError> {
        let session = self
            .sessions
            .get(&session_id)
            .ok_or(FtpError::NotConnected)?;
        if session.state != FtpSessionState::Authenticated {
            return Err(FtpError::NotAuthenticated);
        }
        self.uss_files.insert(path.to_string(), data);
        Ok(FtpReply::new(226, "Transfer complete"))
    }

    // NET-106.3 — MVS Dataset Transfer -----------------------------------

    /// GET an MVS dataset.
    pub fn cmd_retr_mvs(
        &self,
        session_id: u64,
        dsname: &str,
    ) -> Result<(FtpReply, Vec<u8>), FtpError> {
        let session = self
            .sessions
            .get(&session_id)
            .ok_or(FtpError::NotConnected)?;
        if session.state != FtpSessionState::Authenticated {
            return Err(FtpError::NotAuthenticated);
        }
        let content = self
            .mvs_datasets
            .get(dsname)
            .ok_or_else(|| FtpError::DatasetNotFound(dsname.to_string()))?
            .clone();
        Ok((
            FtpReply::new(226, "Transfer complete"),
            content,
        ))
    }

    /// SITE FILETYPE command.
    pub fn cmd_site_filetype(
        &mut self,
        session_id: u64,
        filetype: SiteFileType,
    ) -> Result<FtpReply, FtpError> {
        let session = self
            .sessions
            .get_mut(&session_id)
            .ok_or(FtpError::NotConnected)?;
        if session.state != FtpSessionState::Authenticated {
            return Err(FtpError::NotAuthenticated);
        }
        session.site_filetype = filetype;
        let name = match filetype {
            SiteFileType::Uss => "USS",
            SiteFileType::Seq => "SEQ",
            SiteFileType::Jes => "JES",
        };
        Ok(FtpReply::new(200, format!("SITE FILETYPE={}", name)))
    }

    // NET-106.4 — JES2 Job Submission via FTP ----------------------------

    /// PUT JCL for JES2 submission.
    pub fn cmd_stor_jes(
        &mut self,
        session_id: u64,
        jcl: &str,
    ) -> Result<FtpReply, FtpError> {
        let session = self
            .sessions
            .get(&session_id)
            .ok_or(FtpError::NotConnected)?;
        if session.state != FtpSessionState::Authenticated {
            return Err(FtpError::NotAuthenticated);
        }
        if session.site_filetype != SiteFileType::Jes {
            return Err(FtpError::JesSubmitFailed(
                "SITE FILETYPE=JES not set".to_string(),
            ));
        }
        if !jcl.starts_with("//") {
            return Err(FtpError::JesSubmitFailed("invalid JCL".to_string()));
        }
        let job_num = self.next_job_number;
        self.next_job_number += 1;
        Ok(FtpReply::new(
            250,
            format!("It is known to JES as JOB{:05}", job_num),
        ))
    }

    /// Get the file type set for a session.
    pub fn session_filetype(&self, session_id: u64) -> Option<SiteFileType> {
        self.sessions.get(&session_id).map(|s| s.site_filetype)
    }

    /// QUIT command.
    pub fn cmd_quit(&mut self, session_id: u64) -> FtpReply {
        if let Some(session) = self.sessions.get_mut(&session_id) {
            session.state = FtpSessionState::Disconnected;
        }
        FtpReply::new(221, "Goodbye")
    }
}

// ---------------------------------------------------------------------------
// NET-106.5 — FTP Client
// ---------------------------------------------------------------------------

/// A simple FTP client for outbound transfers.
#[derive(Debug)]
pub struct FtpClient {
    pub hostname: String,
    connected: bool,
    authenticated: bool,
    pub username: String,
    transfer_mode: TransferMode,
    /// Simulated remote listing.
    remote_files: Vec<String>,
    /// Simulated downloaded files.
    downloaded: HashMap<String, Vec<u8>>,
}

impl FtpClient {
    pub fn new() -> Self {
        Self {
            hostname: String::new(),
            connected: false,
            authenticated: false,
            username: String::new(),
            transfer_mode: TransferMode::Binary,
            remote_files: Vec::new(),
            downloaded: HashMap::new(),
        }
    }

    /// Connect to a remote FTP server.
    pub fn connect(&mut self, hostname: &str) -> Result<FtpReply, FtpError> {
        self.hostname = hostname.to_string();
        self.connected = true;
        Ok(FtpReply::new(220, format!("{} FTP server ready", hostname)))
    }

    /// Login to the server.
    pub fn login(&mut self, username: &str, password: &str) -> Result<FtpReply, FtpError> {
        if !self.connected {
            return Err(FtpError::NotConnected);
        }
        // Simulated: accept any credentials
        let _ = password;
        self.username = username.to_string();
        self.authenticated = true;
        Ok(FtpReply::new(230, "User logged in"))
    }

    /// List remote files.
    pub fn ls(&self) -> Result<Vec<String>, FtpError> {
        if !self.authenticated {
            return Err(FtpError::NotAuthenticated);
        }
        Ok(self.remote_files.clone())
    }

    /// Simulate adding a remote file for listing.
    pub fn add_remote_file(&mut self, name: impl Into<String>) {
        self.remote_files.push(name.into());
    }

    /// GET a remote file (simulated).
    pub fn get(&mut self, filename: &str, content: Vec<u8>) -> Result<FtpReply, FtpError> {
        if !self.authenticated {
            return Err(FtpError::NotAuthenticated);
        }
        self.downloaded.insert(filename.to_string(), content);
        Ok(FtpReply::new(226, "Transfer complete"))
    }

    /// PUT a local file to remote (simulated).
    pub fn put(&self, _filename: &str, _data: &[u8]) -> Result<FtpReply, FtpError> {
        if !self.authenticated {
            return Err(FtpError::NotAuthenticated);
        }
        Ok(FtpReply::new(226, "Transfer complete"))
    }

    /// Set transfer mode.
    pub fn set_mode(&mut self, mode: TransferMode) -> FtpReply {
        self.transfer_mode = mode;
        FtpReply::new(200, "Mode set")
    }

    /// QUIT.
    pub fn quit(&mut self) -> FtpReply {
        self.connected = false;
        self.authenticated = false;
        FtpReply::new(221, "Goodbye")
    }

    /// Is the client connected?
    pub fn is_connected(&self) -> bool {
        self.connected
    }
}

impl Default for FtpClient {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Tests — NET-106.6
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn setup_server() -> FtpServer {
        let mut server = FtpServer::new(21, "mainframe.example.com");
        server.add_user("jsmith", "secret123");
        server.add_uss_file("/u/jsmith/data.txt", b"Hello USS".to_vec());
        server.add_mvs_dataset("MY.DATA.SET", b"Hello MVS".to_vec());
        server
    }

    fn authenticate(server: &mut FtpServer) -> u64 {
        let (id, _) = server.accept_connection();
        server.cmd_user(id, "jsmith").unwrap();
        server.cmd_pass(id, "secret123").unwrap();
        id
    }

    #[test]
    fn server_accept_connection() {
        let mut server = setup_server();
        let (_, reply) = server.accept_connection();
        assert_eq!(reply.code, 220);
    }

    #[test]
    fn server_user_pass_authentication() {
        let mut server = setup_server();
        let (id, _) = server.accept_connection();
        let reply = server.cmd_user(id, "jsmith").unwrap();
        assert_eq!(reply.code, 331);
        let reply = server.cmd_pass(id, "secret123").unwrap();
        assert_eq!(reply.code, 230);
    }

    #[test]
    fn server_auth_wrong_password() {
        let mut server = setup_server();
        let (id, _) = server.accept_connection();
        server.cmd_user(id, "jsmith").unwrap();
        assert!(server.cmd_pass(id, "wrong").is_err());
    }

    #[test]
    fn get_uss_file() {
        let mut server = setup_server();
        let id = authenticate(&mut server);
        let (reply, data) = server.cmd_retr_uss(id, "/u/jsmith/data.txt").unwrap();
        assert_eq!(reply.code, 226);
        assert_eq!(data, b"Hello USS");
    }

    #[test]
    fn get_uss_file_not_found() {
        let mut server = setup_server();
        let id = authenticate(&mut server);
        assert!(server.cmd_retr_uss(id, "/u/jsmith/missing.txt").is_err());
    }

    #[test]
    fn put_uss_file() {
        let mut server = setup_server();
        let id = authenticate(&mut server);
        let reply = server
            .cmd_stor_uss(id, "/u/jsmith/new.txt", b"New file".to_vec())
            .unwrap();
        assert_eq!(reply.code, 226);
        let (_, data) = server.cmd_retr_uss(id, "/u/jsmith/new.txt").unwrap();
        assert_eq!(data, b"New file");
    }

    #[test]
    fn get_mvs_dataset() {
        let mut server = setup_server();
        let id = authenticate(&mut server);
        let (reply, data) = server.cmd_retr_mvs(id, "MY.DATA.SET").unwrap();
        assert_eq!(reply.code, 226);
        assert_eq!(data, b"Hello MVS");
    }

    #[test]
    fn site_filetype_jes() {
        let mut server = setup_server();
        let id = authenticate(&mut server);
        let reply = server.cmd_site_filetype(id, SiteFileType::Jes).unwrap();
        assert_eq!(reply.code, 200);
        assert_eq!(server.session_filetype(id), Some(SiteFileType::Jes));
    }

    #[test]
    fn jes2_job_submission() {
        let mut server = setup_server();
        let id = authenticate(&mut server);
        server.cmd_site_filetype(id, SiteFileType::Jes).unwrap();
        let reply = server
            .cmd_stor_jes(id, "//MYJOB JOB CLASS=A\n//STEP1 EXEC PGM=IEFBR14\n")
            .unwrap();
        assert_eq!(reply.code, 250);
        assert!(reply.message.contains("JOB"));
    }

    #[test]
    fn jes2_without_site_filetype() {
        let mut server = setup_server();
        let id = authenticate(&mut server);
        assert!(server.cmd_stor_jes(id, "//JOB1").is_err());
    }

    #[test]
    fn jes2_invalid_jcl() {
        let mut server = setup_server();
        let id = authenticate(&mut server);
        server.cmd_site_filetype(id, SiteFileType::Jes).unwrap();
        assert!(server.cmd_stor_jes(id, "not JCL").is_err());
    }

    #[test]
    fn ftp_client_connect_login() {
        let mut client = FtpClient::new();
        let reply = client.connect("mainframe.example.com").unwrap();
        assert_eq!(reply.code, 220);
        let reply = client.login("jsmith", "secret").unwrap();
        assert_eq!(reply.code, 230);
    }

    #[test]
    fn ftp_client_ls() {
        let mut client = FtpClient::new();
        client.connect("host").unwrap();
        client.login("user", "pass").unwrap();
        client.add_remote_file("file1.txt");
        client.add_remote_file("file2.dat");
        let files = client.ls().unwrap();
        assert_eq!(files.len(), 2);
    }

    #[test]
    fn ftp_client_get_put() {
        let mut client = FtpClient::new();
        client.connect("host").unwrap();
        client.login("user", "pass").unwrap();
        let reply = client.get("remote.txt", b"content".to_vec()).unwrap();
        assert_eq!(reply.code, 226);
        let reply = client.put("local.txt", b"upload").unwrap();
        assert_eq!(reply.code, 226);
    }

    #[test]
    fn ftp_client_not_connected() {
        let client = FtpClient::new();
        assert!(client.ls().is_err());
    }

    #[test]
    fn ftp_quit() {
        let mut server = setup_server();
        let id = authenticate(&mut server);
        let reply = server.cmd_quit(id);
        assert_eq!(reply.code, 221);
    }

    #[test]
    fn ftp_reply_success_check() {
        assert!(FtpReply::new(226, "ok").is_success());
        assert!(!FtpReply::new(550, "error").is_success());
    }
}
