#![forbid(unsafe_code)]
//! z/OS Networking: VTAM, SNA, TCP/IP, FTP, SSH.
//!
//! This crate provides:
//!
//! - **VTAM** — Application Control Block (ACB), sessions, SEND/RECEIVE
//! - **SNA** — LU types (0/1/2/3/6.2), BIND parameters, 3270 data streams
//! - **APPC** — LU 6.2 conversations, CPI-C portable API
//! - **TCP/IP** — TCPIP.PROFILE parsing, stack configuration
//! - **Sockets** — POSIX sockets compatibility layer
//! - **AT-TLS** — Policy Agent, TLS handshake, RACF keyring, client certs
//! - **FTP** — FTP server/client, USS/MVS transfers, JES2 submission
//! - **SSH/TN3270E** — SSH server, TN3270E multi-session terminal access
//! - **Sysplex** — Static/Dynamic VIPA, Sysplex Distributor, WLM routing
//! - **Security** — IP filtering, IPSec Security Associations

pub mod appc;
pub mod ftp;
pub mod security;
pub mod sna;
pub mod sockets;
pub mod ssh;
pub mod sysplex;
pub mod tcpip;
pub mod tls;
pub mod vtam;

// Re-exports: VTAM (NET-100)
pub use vtam::{Acb, AcbAuth, AcbMacrf, LuType, Session, SessionState, VtamError};

// Re-exports: SNA (NET-101)
pub use sna::{
    BindParameters, Command3270, Lu0Session, Lu1PrinterSession, Lu2Session, ScsControlCode,
    SnaError,
};

// Re-exports: APPC (NET-102)
pub use appc::{
    AppcError, AppcManager, Conversation, ConversationState, CpiC, DeallocateType, SyncLevel,
};

// Re-exports: TCP/IP (NET-103)
pub use tcpip::{
    AutologEntry, DeviceDefinition, DeviceType, HomeAddress, LinkDefinition, PortReservation,
    ResolverConfig, RouteEntry, TcpConfig, TcpIpError, TcpIpProfile, TransportProtocol, UdpConfig,
};

// Re-exports: Sockets (NET-104)
pub use sockets::{
    SocketError, SocketRuntime, AF_INET, AF_INET6, SOCK_DGRAM, SOCK_STREAM, SOL_SOCKET,
    SO_KEEPALIVE, SO_RCVBUF, SO_REUSEADDR, SO_SNDBUF,
};

// Re-exports: AT-TLS (NET-105)
pub use tls::{
    Certificate, CipherStrength, CipherSuite, Direction, HandshakeResult, HandshakeRole, Keyring,
    KeyringStore, PolicyAgent, TlsError, TlsRule, TlsVersion,
};

// Re-exports: FTP (NET-106)
pub use ftp::{
    FtpClient, FtpError, FtpReply, FtpServer, SiteFileType, TransferMode,
};

// Re-exports: SSH/TN3270E (NET-107)
pub use ssh::{
    AuthorizedKey, SshAuthMethod, SshError, SshServer, SshSession,
    Tn3270DeviceType, Tn3270Error, Tn3270Server, Tn3270Session, Tn3270Target,
    StructuredFieldType, QueryReply, UsableArea,
};

// Re-exports: Sysplex (NET-108)
pub use sysplex::{
    DistributorTarget, DynamicVipa, StaticVipa, SysplexDistributor, SysplexNetError,
    VipaManager, VipaState,
};

// Re-exports: Security (NET-109)
pub use security::{
    DefenseManager, FilterAction, IkeAuthMethod, IpFilterRule, IpNetwork, IpsecAlgorithm,
    IpsecMode, ProtocolMatch, SecurityAssociation, SecurityError,
};
