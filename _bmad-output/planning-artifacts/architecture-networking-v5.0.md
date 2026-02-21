---
version: 'v5.0'
planningGroup: 'PG-16'
technology: 'Networking & TCP/IP'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-networking-v5.0.md'
---

# Architecture: Networking & TCP/IP

## 1. Crate Strategy

**New crate:** `open-mainframe-networking`

Rationale: Networking spans SNA (VTAM/APPC) and TCP/IP stacks with 10 epics. It is a foundational subsystem with its own protocol implementations, configuration model, and security layer (AT-TLS). A separate crate avoids polluting the existing crates with protocol-level code.

## 2. Module Layout

```
crates/open-mainframe-networking/src/
├── lib.rs
├── vtam/
│   ├── mod.rs          # VTAM application interface
│   ├── acb.rs          # ACB (Application Control Block)
│   ├── rpl.rs          # RPL (Request Parameter List)
│   └── sessions.rs     # LU session management
├── appc/
│   ├── mod.rs          # APPC/LU 6.2 conversation model
│   ├── cpic.rs         # CPI-C interface
│   └── transaction.rs  # Sync-point and conversation state
├── tcpip/
│   ├── mod.rs          # TCP/IP stack configuration
│   ├── profile.rs      # TCPIP.PROFILE parser
│   ├── sockets.rs      # POSIX sockets compatibility layer
│   └── resolver.rs     # DNS resolution
├── tls/
│   ├── mod.rs          # AT-TLS engine
│   ├── policy.rs       # Policy Agent configuration
│   └── keyring.rs      # RACF keyring integration
├── services/
│   ├── mod.rs          # Application services registry
│   ├── ftp.rs          # FTP client/server
│   ├── ssh.rs          # SSH server (USS integration)
│   └── tn3270e.rs      # Enhanced TN3270E server
├── sysplex/
│   ├── mod.rs          # Sysplex networking
│   ├── vipa.rs         # VIPA and Dynamic VIPA
│   ├── distributor.rs  # Sysplex Distributor
│   └── wlm_route.rs    # WLM-based connection routing
└── security/
    ├── mod.rs          # IP filtering
    └── ipsec.rs        # IPSec policy
```

## 3. Key Types

```rust
/// VTAM Application Control Block
pub struct Acb {
    pub applid: String,
    pub macrf: AcbMacrf,
    pub auth: AcbAuth,
    pub sessions: Vec<Session>,
}

/// SNA Session
pub struct Session {
    pub lu_name: String,
    pub lu_type: LuType,
    pub state: SessionState,
    pub bind_parameters: BindParameters,
}

pub enum LuType {
    Lu0, Lu1, Lu2, Lu3, Lu62,
}

/// APPC Conversation
pub struct Conversation {
    pub id: u64,
    pub tp_name: String,
    pub partner_lu: String,
    pub state: ConversationState,
    pub sync_level: SyncLevel,
}

pub enum ConversationState {
    Reset, Send, Receive, Confirm, Deallocated,
}

/// TCP/IP Profile configuration
pub struct TcpIpProfile {
    pub hostname: String,
    pub ports: Vec<PortReservation>,
    pub tcp_config: TcpConfig,
    pub udp_config: UdpConfig,
    pub autolog: Vec<AutologEntry>,
}

/// AT-TLS Policy
pub struct AtTlsPolicy {
    pub rules: Vec<TlsRule>,
    pub keyring: String,
    pub min_tls_version: TlsVersion,
}

/// VIPA configuration
pub struct VipaConfig {
    pub static_vipas: Vec<IpAddr>,
    pub dynamic_vipas: Vec<DynamicVipa>,
    pub distributor: Option<SysplexDistributor>,
}
```

## 4. Design Decisions

### DD-5.0-NET-01: Tokio as TCP/IP Runtime
**Decision:** All TCP/IP operations use Tokio's async runtime. The POSIX sockets compatibility layer wraps Tokio primitives to provide C-sockets semantics. This avoids reimplementing a TCP/IP stack.

### DD-5.0-NET-02: SNA as Simulation Layer
**Decision:** VTAM/SNA is implemented as a simulation layer (no real SNA networking). VTAM sessions manage logical connections between CICS/IMS and terminal emulators. APPC conversations are local-only.

### DD-5.0-NET-03: AT-TLS via rustls
**Decision:** AT-TLS is implemented using the `rustls` crate. Policy Agent rules select TLS parameters per connection. RACF keyring integration loads certificates from the RACF security store.
