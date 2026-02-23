# Gap Analysis: Networking (VTAM / TCP-IP)

## Official Specification Summary

z/OS Communications Server is the integrated networking component of z/OS, providing both SNA (Systems Network Architecture) and TCP/IP protocol stacks in a single product. It enables z/OS systems to communicate with terminals, other mainframes, distributed systems, and the internet. The Communications Server encompasses VTAM (the SNA access method), the TCP/IP stack, and a range of application services (TN3270, FTP, SSH, NFS, DNS).

Networking is classified as **Core** — every z/OS system requires Communications Server:
- **VTAM (Virtual Telecommunications Access Method)**: SNA session manager — ACB/RPL/NIB control blocks, LOGON/LOGOFF, LU types (0/1/2/3/6.2), APPCCMD macro for LU 6.2, APPC transaction programs
- **SNA (Systems Network Architecture)**: Subarea networking (PU/LU/SSCP), APPN (Advanced Peer-to-Peer Networking), HPR (High Performance Routing), DLUR/DLUS for dependent LU support
- **TCP/IP stack**: z/OS Communications Server TCP/IP, TCPIP.DATA/PROFILE configuration, PORT reservations, C and assembler sockets API, CINET (multiple stacks)
- **AT-TLS (Application Transparent TLS)**: Policy Agent-driven TLS encryption transparent to applications
- **Enterprise Extender (EE)**: SNA over IP — HPR traffic encapsulated in UDP datagrams, replacing SDLC/NCP hardware
- **Application services**: TN3270E server, FTP/FTPS, SSH/SFTP (OpenSSH), NFS server/client, DNS (BIND), SNMP
- **Sysplex networking**: VIPA (Virtual IP Address), Dynamic VIPA, Sysplex Distributor for workload balancing
- **Security**: IPSec, AT-TLS, RACF keyrings for certificates, IP filtering/firewall, IDS (Intrusion Detection Services)

Key documentation:
- **z/OS Communications Server: SNA Programming** (SC27-3674) — ACB, RPL, NIB, VTAM macros
- **z/OS Communications Server: SNA Programmer's LU 6.2 Guide** (SC31-6444) — APPCCMD macro, LU 6.2 sessions
- **z/OS Communications Server: IP Configuration Guide** (SC27-3650) — TCP/IP stack, AT-TLS, VIPA, Policy Agent
- **z/OS Communications Server: IP Sockets Application Programming Interface Guide** (SC27-3660) — C and assembler sockets
- **z/OS Communications Server: IP Configuration Reference** (SC27-3651) — TCPIP PROFILE statements
- **IBM Redbook: Enterprise Extender Implementation Guide** (SG24-7359)
- **IBM Redbook: z/OS Communications Server TCP/IP Implementation Volume 3** (SG24-7800) — VIPA, Sysplex Distributor

## Key Features & Capabilities

### 1. VTAM (Virtual Telecommunications Access Method)

| Feature | Description |
|---------|-------------|
| ACB (Access Control Block) | VTAM application's identity — opened via OPEN ACB macro; ACBNAME = VTAM application name |
| RPL (Request Parameter List) | Describes each I/O request (SEND, RECEIVE, OPNDST, CLSDST, etc.) |
| NIB (Node Initialization Block) | Parameters for initiating/accepting sessions |
| EXLST (Exit List) | List of exit routines for asynchronous events (LOGON, LOSTERM, etc.) |
| OPEN / CLOSE | Open/close a VTAM ACB (register/deregister with VTAM) |
| OPNDST / CLSDST | Establish/terminate a session with an LU |
| SEND / RECEIVE | Send/receive data on an established session |
| LOGON exit | Asynchronous notification when an LU requests logon |
| LOSTERM exit | Notification when a session is lost |
| MODCB / SHOWCB / TESTCB | Modify, show, test control block fields at runtime |
| SETLOGON | Control whether application accepts logon requests |
| REQSESS / TERMSESS | Request/terminate session for APPC |
| APPL definition | Define VTAM application in SYS1.VTAMLST |
| PU/LU definitions | Define physical/logical units in VTAM configuration |

### 2. SNA LU Types

| LU Type | Description | Common Usage |
|---------|-------------|-------------|
| LU 0 | Unformatted data stream | Custom protocols, session managers |
| LU 1 | SCS (SNA Character String) data stream | Line-mode printers |
| LU 2 | 3270 data stream | 3270 terminal sessions (CICS, TSO, IMS) |
| LU 3 | 3270 printer data stream | 3270 printers |
| LU 6.2 | APPC (Advanced Program-to-Program Communication) | Distributed transactions, CICS ISC, MQ, DRDA |

### 3. APPC / LU 6.2

| Feature | Description |
|---------|-------------|
| APPCCMD macro | Primary macro for LU 6.2 operations |
| Conversation types | Basic (low-level) and Mapped (high-level) |
| ALLOCATE | Start a conversation with partner TP |
| SEND_DATA | Send data on a conversation |
| RECEIVE_AND_WAIT | Receive data from partner |
| DEALLOCATE | End a conversation |
| CONFIRM / CONFIRMED | Sync-point coordination |
| CPI-C | Common Programming Interface for Communications (portable API) |
| TP (Transaction Program) | Application program participating in APPC conversation |
| Mode | Session properties (LOGMODE entry) |
| CNOS | Change Number of Sessions negotiation |

### 4. APPN / HPR / Enterprise Extender

| Feature | Description |
|---------|-------------|
| APPN | Advanced Peer-to-Peer Networking — flat, dynamic SNA topology |
| HPR (High Performance Routing) | Reliable, fast-path SNA routing with automatic rerouting |
| Enterprise Extender (EE) | HPR over UDP/IP — SNA applications over IP network |
| DLUR (Dependent LU Requester) | Allow dependent LUs to use APPN network |
| DLUS (Dependent LU Server) | Serve dependent LU requests in APPN |
| Connection network | EE views IP network as APPN connection network |
| UDP transport | EE uses UDP (port 12000-12004) for HPR NLPs |
| Priority ports | UDP ports 12000-12004 mapped to SNA transmission priorities |

### 5. TCP/IP Stack

| Feature | Description |
|---------|-------------|
| TCPIP PROFILE | Main TCP/IP configuration (PORT, TCPCONFIG, UDPCONFIG, etc.) |
| TCPIP.DATA | Resolver configuration (hostname, domain, nameserver) |
| PORT statement | Reserve ports for specific applications |
| AUTOLOG | Auto-start applications when TCP/IP initializes |
| C sockets API | Standard BSD/POSIX sockets for C programs |
| Assembler sockets API | EZASMI/EZASOKET macro interface |
| REXX sockets | SOCKET() function for REXX programs |
| CINET | Common INET — support for multiple TCP/IP stacks |
| IPv4 / IPv6 | Dual-stack support |
| MAXFILEPROC | Per-process socket limit (BPXPRMxx) |

### 6. AT-TLS (Application Transparent TLS)

| Feature | Description |
|---------|-------------|
| Policy Agent | Daemon that applies AT-TLS rules based on connection attributes |
| TTLSRule | Match criteria (local/remote address, port, jobname) |
| TTLSGroupAction | TLS protocol settings (TLS versions, cipher suites) |
| TTLSEnvironmentAction | Certificate/keyring configuration |
| HandshakeRole | Client or server role |
| RACF keyrings | Certificate storage for TLS (SAF keyring, PKCS#11 token) |
| System SSL | IBM's TLS implementation underlying AT-TLS |
| Transparent encryption | Application unaware of TLS — Policy Agent intercepts |
| TLS 1.0/1.1/1.2/1.3 | Progressive TLS version support |

### 7. Application Services

| Service | Description |
|---------|-------------|
| TN3270E server | Telnet server for 3270 terminal access (LU 2 over TCP/IP) |
| FTP server/client | File transfer with MVS dataset, USS file, JES spool support |
| FTPS | FTP over TLS |
| SSH (OpenSSH) | Secure shell — sshd, sftp, scp |
| NFS server | Serve MVS datasets and USS files via NFS protocol |
| NFS client | Mount remote NFS shares |
| DNS (BIND) | Domain Name System server on z/OS |
| SNMP agent | SNMP v1/v2c/v3 with MIB support |
| SMTP | Simple Mail Transfer Protocol |
| HTTP server (IBM IHS) | IBM HTTP Server for z/OS (Apache-based) |
| z/OSMF | z/OS Management Facility (REST APIs over HTTPS) |

### 8. Sysplex Networking

| Feature | Description |
|---------|-------------|
| Static VIPA | Virtual IP address tied to TCP/IP stack, surviving NIC failures |
| Dynamic VIPA (DVIPA) | VIPA that can move between systems on failure |
| Sysplex Distributor | WLM-aware load balancing across sysplex members |
| Distributed DVIPA | Sysplex Distributor routes to server instances by WLM weight |
| Dynamic XCF | Automatic cross-system coupling links within sysplex |
| Connection routing | Target-system routing via WLM policy |
| Nondisruptive port sharing | Multiple stacks serving the same port |

### 9. Security & Policy

| Feature | Description |
|---------|-------------|
| IPSec | IP Security — IKE daemon, ESP/AH protocols |
| IP filtering | Packet filtering rules (firewall) |
| IDS (Intrusion Detection Services) | Detect network attacks |
| Policy Agent | Applies QoS, AT-TLS, IPSec, IP filter policies |
| RACF certificates | RACDCERT for certificate management |
| RACF keyrings | Store CA certs, server certs, client certs for TLS |
| SAF interface | Security Access Facility for authentication |

## Current OpenMainframe Status

### What IS Implemented

#### 1. TN3270E Protocol — Full Implementation
**File:** `crates/open-mainframe-tui/src/tn3270.rs` (588 lines)
- Complete TN3270E telnet negotiation (DO/WILL/WONT/DONT for BINARY, EOR, TN3270E)
- 3270 data stream parsing: SBA, SF, SFE, IC, PT, RA, EUA orders
- Buffer address encoding/decoding (6-bit 3270 encoding)
- AID (Attention Identifier) codes: Enter, PF1-PF24, PA1-PA3, Clear
- Read Modified response generation
- TcpStream-based connection (std::net)
- Connection config struct (host, port, LU name, device type)
- 16 unit tests

#### 2. HTTP Server — Deployment Infrastructure
**File:** `crates/open-mainframe-deploy/src/server.rs` (360 lines)
- Async HTTP server using `tokio::net::TcpListener`
- Dual-port design: app server + metrics server
- Health (`/health`) and readiness (`/ready`) endpoints
- Prometheus metrics (`/metrics`) endpoint
- Graceful shutdown coordination
- 7 async tests

#### 3. CICS APPLID — VTAM Application Name
**File:** `crates/open-mainframe-cics/src/runtime/commands.rs:101-106`
- `applid: String` field (VTAM application name, default "OMCICS01")
- `netname: String` field
- ASSIGN APPLID command returns the VTAM APPLID
- `set_applid()` method for configuration
- Data structure only — no VTAM protocol or ACB operations

### What Is NOT Implemented

No z/OS Communications Server-specific networking exists. All missing components are listed in the gap table below.

## Gap Details

### VTAM / SNA

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| ACB (Access Control Block) | VTAM application identity and registration | None | **Missing** |
| RPL (Request Parameter List) | I/O request descriptor | None | **Missing** |
| NIB (Node Initialization Block) | Session initiation parameters | None | **Missing** |
| EXLST (Exit List) | Async event exits (LOGON, LOSTERM) | None | **Missing** |
| OPEN / CLOSE ACB | Register/deregister with VTAM | None | **Missing** |
| OPNDST / CLSDST | Establish/terminate SNA session | None | **Missing** |
| SEND / RECEIVE | Send/receive data on session | None | **Missing** |
| SETLOGON | Accept/reject logon requests | None | **Missing** |
| MODCB / SHOWCB / TESTCB | Runtime control block manipulation | None | **Missing** |
| APPL definition | VTAM application config in SYS1.VTAMLST | APPLID string in CICS | **Partial** |
| PU / LU definitions | Physical/logical unit config | None | **Missing** |
| SNA LU 0 | Unformatted data stream | None | **Missing** |
| SNA LU 1 | SCS printer data stream | None | **Missing** |
| SNA LU 2 | 3270 terminal data stream | TN3270E (TCP/IP equivalent) | **Partial** |
| SNA LU 3 | 3270 printer stream | None | **Missing** |
| SNA LU 6.2 / APPC | Program-to-program communication | None | **Missing** |
| APPCCMD macro | LU 6.2 verb interface | None | **Missing** |
| CPI-C | Portable APPC API | None | **Missing** |
| CNOS negotiation | Session limit management | None | **Missing** |

### APPN / HPR / Enterprise Extender

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| APPN | Advanced Peer-to-Peer Networking | None | **Missing** |
| HPR | High Performance Routing | None | **Missing** |
| Enterprise Extender | HPR over UDP/IP | None | **Missing** |
| DLUR / DLUS | Dependent LU support in APPN | None | **Missing** |
| Connection network | IP as APPN connection network | None | **Missing** |
| SNA topology | Network node, end node, LEN node | None | **Missing** |

### TCP/IP Stack

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| TCPIP PROFILE | Main TCP/IP configuration | None | **Missing** |
| TCPIP.DATA | Resolver configuration | None | **Missing** |
| PORT statement | Port reservation | None | **Missing** |
| AUTOLOG | Auto-start applications | None | **Missing** |
| C sockets API | BSD/POSIX sockets | tokio async sockets (different API) | **Partial** |
| Assembler sockets (EZASMI) | z/OS-specific socket macro interface | None | **Missing** |
| REXX sockets | SOCKET() function | None | **Missing** |
| CINET | Multiple TCP/IP stacks | None | **Missing** |
| IPv4 support | Full TCP/UDP over IPv4 | tokio TcpListener (partial) | **Partial** |
| IPv6 support | Full TCP/UDP over IPv6 | None | **Missing** |

### AT-TLS

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Policy Agent configuration | TTLSRule, TTLSGroupAction, TTLSEnvironmentAction | None | **Missing** |
| Transparent TLS encryption | Application-unaware TLS | None | **Missing** |
| System SSL integration | IBM TLS implementation | None | **Missing** |
| RACF keyring integration | Certificate management | None | **Missing** |
| TLS 1.2 / 1.3 support | Modern TLS versions | None | **Missing** |

### Application Services

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| TN3270E server | 3270 terminal access over TCP/IP | TN3270E client (tn3270.rs) | **Partial** |
| FTP server/client | MVS dataset and USS file transfer | None | **Missing** |
| FTPS (FTP over TLS) | Secure file transfer | None | **Missing** |
| SSH (OpenSSH) | Secure shell, sftp, scp | None | **Missing** |
| NFS server | Serve MVS/USS files via NFS | None | **Missing** |
| NFS client | Mount remote NFS shares | None | **Missing** |
| DNS (BIND) | Name server | None | **Missing** |
| SNMP agent | Network management | None | **Missing** |
| SMTP | Email | None | **Missing** |
| HTTP server (IHS) | Web server | Health endpoint only (deploy) | **Partial** |

### Sysplex Networking

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Static VIPA | Virtual IP surviving NIC failure | None | **Missing** |
| Dynamic VIPA | Moveable VIP for application failover | None | **Missing** |
| Sysplex Distributor | WLM-aware load balancing | None | **Missing** |
| Dynamic XCF | Automatic sysplex links | None | **Missing** |
| Connection routing | WLM-based target system selection | None | **Missing** |

### Security & Policy

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| IPSec | IP-layer encryption (IKE, ESP, AH) | None | **Missing** |
| IP filtering | Packet filtering/firewall rules | None | **Missing** |
| IDS | Intrusion Detection Services | None | **Missing** |
| Policy Agent | QoS, AT-TLS, IPSec policy daemon | None | **Missing** |
| RACF certificate management | RACDCERT for TLS certificates | None | **Missing** |

## Proposed Epic Structure

### NET-100: VTAM Application Interface
**Scope:** Implement VTAM ACB, RPL, NIB, EXLST control block abstractions. OPEN/CLOSE ACB for application registration. OPNDST/CLSDST for session management. SEND/RECEIVE for data exchange. SETLOGON for logon acceptance. MODCB/SHOWCB/TESTCB for runtime manipulation. APPL definition parsing.
**Complexity:** XL
**Rationale:** Full SNA access method implementation. ACB/RPL/NIB are complex control blocks with many fields. Session lifecycle (bind/unbind) involves multi-step protocol exchanges.

### NET-101: SNA LU Type Support
**Scope:** Implement LU type 0 (unformatted), LU 1 (SCS printer), LU 2 (3270 data stream — extend existing TN3270E), LU 3 (3270 printer), LU 6.2 (APPC). Each LU type has distinct data stream formats and session protocols.
**Complexity:** L
**Rationale:** LU 2 partially exists (TN3270E). LU 0 is simple. LU 6.2 is the main complexity — full conversation model.

### NET-102: APPC / LU 6.2 / CPI-C
**Scope:** Implement APPCCMD macro interface for LU 6.2 conversations — ALLOCATE, SEND_DATA, RECEIVE_AND_WAIT, DEALLOCATE, CONFIRM/CONFIRMED. Basic and mapped conversation types. CPI-C portable API. TP (Transaction Program) registration. CNOS session limit negotiation. Mode definitions.
**Complexity:** XL
**Rationale:** Full distributed transaction protocol. Conversation state machine with sync-point coordination. Used by CICS ISC, MQ channels, DRDA. Critical for subsystem interoperability.

### NET-103: APPN / HPR / Enterprise Extender
**Scope:** Implement APPN topology (network node, end node, LEN node), HPR (reliable routing with automatic path switching), Enterprise Extender (HPR encapsulated in UDP/IP). DLUR/DLUS for dependent LU support. Connection network abstraction. SNA topology database.
**Complexity:** L
**Rationale:** HPR routing and EE encapsulation are well-defined protocols. Main complexity is topology management and path selection. Lower priority as most new work uses TCP/IP directly.

### NET-104: TCP/IP Stack Configuration
**Scope:** Implement TCPIP PROFILE parsing (PORT, TCPCONFIG, UDPCONFIG, IPCONFIG statements). TCPIP.DATA resolver configuration. AUTOLOG application auto-start. CINET multi-stack support. Port reservation and management. Integrate with existing tokio TCP infrastructure.
**Complexity:** M
**Rationale:** Configuration parsing plus integration with existing async networking. The TCP/IP stack itself is provided by the host OS — main work is z/OS-specific configuration and policy layer.

### NET-105: Sockets API Compatibility Layer
**Scope:** Implement z/OS-specific socket APIs — EZASMI assembler macro interface, EZASOKET call interface, REXX SOCKET() function. Extend existing tokio sockets with z/OS-specific options (AT-TLS awareness, CINET stack selection). IPv6 support.
**Complexity:** M
**Rationale:** API compatibility layer over existing Rust/tokio networking. EZASMI/EZASOKET are z/OS-specific calling conventions. REXX sockets integrate with Batch 1 (REXX).

### NET-106: AT-TLS & Security
**Scope:** Implement AT-TLS Policy Agent configuration — TTLSRule matching (address, port, jobname), TTLSGroupAction (TLS versions, cipher suites), TTLSEnvironmentAction (certificates, keyrings). Transparent TLS interception. RACF keyring integration for certificate storage. IP filtering rules. IPSec configuration.
**Complexity:** L
**Rationale:** Policy-driven TLS is unique to z/OS. Must integrate with RACF (Batch 8) for keyrings. TLS implementation can delegate to Rust native-tls or rustls.

### NET-107: Application Services (FTP/SSH/NFS)
**Scope:** Implement FTP server/client with MVS dataset and JES spool support, FTPS (TLS). SSH server (sshd) with RACF authentication. NFS server for MVS datasets and USS files. DNS resolver integration. SNMP agent with z/OS MIBs.
**Complexity:** XL
**Rationale:** Multiple independent protocols. FTP with MVS dataset transfer is complex (record-oriented I/O). SSH with RACF is a significant security integration. NFS serving MVS datasets bridges two I/O models.

### NET-108: TN3270E Server Enhancement
**Scope:** Extend existing TN3270E client implementation to include TN3270E server functionality — accept incoming 3270 sessions, manage multiple concurrent terminals, integrate with CICS/TSO/IMS for application routing, support LU names and device pools.
**Complexity:** M
**Rationale:** Client protocol logic exists (tn3270.rs). Server mode inverts the flow — accept connections, manage terminal pools, route to applications. Builds on existing 3270 data stream code.

### NET-109: Sysplex Networking
**Scope:** Implement VIPA (static and dynamic), Sysplex Distributor for WLM-aware load balancing, Dynamic XCF links, connection routing. Map to Kubernetes service abstractions (ClusterIP, load balancer) for cloud-native deployment.
**Complexity:** L
**Rationale:** High-availability networking. Can leverage Kubernetes Services/Ingress for cloud-native equivalent. Main work is the configuration model and WLM integration.

## Dependencies

| Epic | Depends On |
|------|-----------|
| NET-100 | None (foundational VTAM interface) |
| NET-101 | NET-100 (LU types use VTAM session management) |
| NET-102 | NET-100, NET-101 (APPC builds on LU 6.2 sessions) |
| NET-103 | NET-100 (APPN/HPR extends VTAM topology), NET-104 (EE needs TCP/IP) |
| NET-104 | None (TCP/IP config is independent) |
| NET-105 | NET-104 (socket APIs use TCP/IP stack) |
| NET-106 | NET-104 (AT-TLS applies to TCP connections), Batch 8 RACF (keyrings) |
| NET-107 | NET-104 (FTP/SSH/NFS run over TCP/IP), NET-106 (TLS for FTPS/SSH), Batch 19 DFSMS (dataset access for FTP/NFS) |
| NET-108 | Existing TN3270E client (tn3270.rs), `open-mainframe-cics`, `open-mainframe-tui` |
| NET-109 | NET-104 (VIPA is IP-layer), Batch 17 WLM (sysplex distributor uses WLM) |

### Cross-Batch Dependencies

| Batch | Relationship |
|-------|-------------|
| Batch 1 — REXX | REXX SOCKET() function |
| Batch 8 — RACF | Certificate management (RACDCERT), keyrings, AT-TLS authentication |
| Batch 9 — TSO/ISPF | TSO VTAM logon |
| Batch 10 — IBM MQ | MQ channels use TCP/IP or LU 6.2 |
| Batch 11 — JES2 | NJE over TCP/IP or SNA |
| Batch 13 — IMS TM | IMS Connect (TCP/IP gateway), OTMA |
| Batch 17 — WLM | Sysplex Distributor uses WLM for routing decisions |
| Batch 18 — USS | USS sockets API, inetd, SSH/FTP daemons |

## Complexity Estimate

| Epic | Complexity | Rationale |
|------|-----------|-----------|
| NET-100 | XL | Full VTAM access method — ACB/RPL/NIB, session lifecycle |
| NET-101 | L | Multiple LU types, LU 2 partially exists |
| NET-102 | XL | APPC conversation protocol with sync-point, used by CICS/MQ/DRDA |
| NET-103 | L | APPN topology and HPR routing, EE encapsulation |
| NET-104 | M | Configuration parsing + integration with existing async networking |
| NET-105 | M | API compatibility layers (EZASMI, EZASOKET, REXX SOCKET) |
| NET-106 | L | AT-TLS policy engine, RACF keyring integration, IP filtering |
| NET-107 | XL | Multiple protocols (FTP, SSH, NFS, DNS, SNMP) with z/OS integration |
| NET-108 | M | Server mode for existing TN3270E protocol |
| NET-109 | L | VIPA, Sysplex Distributor, maps to K8s services |

**Overall Complexity: XL** — 10 proposed epics (3×M, 4×L, 3×XL). z/OS Communications Server spans two complete protocol stacks (SNA and TCP/IP) with extensive application services and sysplex-level high availability. The SNA/VTAM side is architecturally unique; the TCP/IP side overlaps with standard networking but has z/OS-specific configuration and security layers.

## Feature Count Summary

- **Total features analyzed:** 95+
- **Present:** 2 (TN3270E client protocol, async HTTP health server)
- **Partial:** 5 (CICS APPLID, tokio TCP/IPv4, LU 2 via TN3270E, basic HTTP endpoint, C-equivalent sockets via std::net)
- **Missing:** 88+ (VTAM, SNA, APPC, APPN/HPR/EE, TCP/IP config, AT-TLS, FTP, SSH, NFS, DNS, SNMP, sysplex networking, IPSec)

## Reference Documentation

- [z/OS Communications Server: SNA Programming (SC27-3674)](https://www.ibm.com/docs/en/zos/2.5.0?topic=communications-server-sna-programming)
- [z/OS Communications Server: SNA Programmer's LU 6.2 Guide (SC31-6444)](https://www.ibm.com/docs/en/SSLTBW_2.2.0/pdf/f1a2e910.pdf)
- [z/OS Communications Server: IP Configuration Guide (SC27-3650)](https://www.ibm.com/docs/en/zos/2.5.0?topic=communications-server-ip-configuration-guide)
- [z/OS Communications Server: IP Sockets API Guide (SC27-3660)](https://www.ibm.com/docs/en/zos/2.5.0?topic=communications-server-ip-sockets-application-programming-interface-guide)
- [z/OS Communications Server 3.1 New Function Summary (GC27-3664)](https://www.ibm.com/docs/en/SSLTBW_3.1.0/pdf/halg001_v3r1.pdf)
- [IBM Redbook: Enterprise Extender Implementation Guide (SG24-7359)](https://www.redbooks.ibm.com/abstracts/sg247359.html)
- [IBM Redbook: z/OS Communications Server TCP/IP Volume 3 (SG24-7800)](https://www.redbooks.ibm.com/redbooks/pdfs/sg247800.pdf)
- [Introduction to z/OS Communications Server (2024)](https://www.ibm.com/support/pages/system/files/inline-files/2024-04-10-CS_Intro.pdf)
- [z/OS Basic Skills — Sysplex Distributor](https://www.ibm.com/docs/en/zos-basic-skills?topic=sysplex-distributor)
- [Enterprise Extender — Wikipedia](https://en.wikipedia.org/wiki/Enterprise_Extender)

## Implementation Status

Review performed against `crates/open-mainframe-networking/` (10 modules, 155 tests).

### VTAM / SNA (NET-100, NET-101)

| Feature | Status | Notes |
|---------|--------|-------|
| ACB (Access Control Block) | YES | `vtam.rs` — Acb struct with applid, macrf, auth, state |
| RPL (Request Parameter List) | YES (now implemented) | `vtam.rs` — Rpl struct with operation, return_code, post_flag |
| NIB (Node Initialization Block) | YES (now implemented) | `vtam.rs` — Nib struct with lu_type, proc_mode, logmode |
| EXLST (Exit List) | YES (now implemented) | `vtam.rs` — Exlst with register/deactivate/drive for Logon, Losterm, Tpend, etc. |
| OPEN / CLOSE ACB | YES | `vtam.rs` — Acb::open() / Acb::close() |
| OPNDST / CLSDST | YES | `vtam.rs` — accept_session() / close_session() |
| SEND / RECEIVE | YES | `vtam.rs` — send() / receive() with session buffers |
| SETLOGON | YES | `vtam.rs` — setlogon_start() / setlogon_stop() |
| MODCB / SHOWCB / TESTCB | YES (now implemented) | `vtam.rs` — showcb() / testcb() functions with AcbField enum |
| APPL definition | YES (now implemented) | `vtam.rs` — ApplDef::parse() for SYS1.VTAMLST entries |
| PU / LU definitions | GAP | Low priority; hardware-level SNA topology not modeled |
| SNA LU 0 | YES | `sna.rs` — Lu0Session for raw byte exchange |
| SNA LU 1 | YES | `sna.rs` — Lu1PrinterSession with SCS control codes |
| SNA LU 2 | YES | `sna.rs` — Lu2Session with full 3270 command set |
| SNA LU 3 | YES (now implemented) | `sna.rs` — Lu3PrinterSession with 3270 printer data stream |
| BIND parameters | YES | `sna.rs` — BindParameters with negotiate() |

### APPC / LU 6.2 / CPI-C (NET-102)

| Feature | Status | Notes |
|---------|--------|-------|
| APPCCMD macro (ALLOCATE) | YES | `appc.rs` — AppcManager::allocate() |
| SEND_DATA | YES | `appc.rs` — AppcManager::send_data() |
| RECEIVE_AND_WAIT | YES | `appc.rs` — AppcManager::receive() with prepare_to_receive() |
| DEALLOCATE | YES | `appc.rs` — AppcManager::deallocate() with Flush/SyncLevel/Abend |
| CONFIRM / CONFIRMED | YES | `appc.rs` — confirm() / confirmed() with state machine |
| CPI-C | YES | `appc.rs` — CpiC struct with cm_init_and_alloc, cm_send, cm_receive, cm_deallocate |
| Conversation state machine | YES | `appc.rs` — ConversationState enum (Reset/Send/Receive/Confirm/Deallocated) |
| CNOS negotiation | YES (now implemented) | `appc.rs` — cnos_negotiate() with ModeDef |
| TP registration | YES (now implemented) | `appc.rs` — TpRegistry with TpDefinition, TpType |
| Mode definitions | YES (now implemented) | `appc.rs` — ModeDef struct with max_sessions, max_ru_size, pacing |
| Conversation types | YES (now implemented) | `appc.rs` — ConversationType::Basic / Mapped |

### APPN / HPR / Enterprise Extender (NET-103)

| Feature | Status | Notes |
|---------|--------|-------|
| APPN | GAP | Full APPN topology not modeled (low priority for emulation) |
| HPR | GAP | High Performance Routing not implemented |
| Enterprise Extender | GAP | HPR-over-UDP not implemented |
| DLUR / DLUS | GAP | Dependent LU support not modeled |
| Connection network | GAP | IP-as-APPN abstraction not modeled |
| SNA topology | GAP | Network/end/LEN node types not modeled |

### TCP/IP Stack (NET-104)

| Feature | Status | Notes |
|---------|--------|-------|
| TCPIP PROFILE | YES | `tcpip.rs` — TcpIpProfile::parse() with PORT, TCPCONFIG, AUTOLOG, etc. |
| TCPIP.DATA | YES (now implemented) | `tcpip.rs` — TcpIpData::parse() for resolver config |
| PORT statement | YES | `tcpip.rs` — PortReservation with protocol and daemon_name |
| AUTOLOG | YES | `tcpip.rs` — AutologEntry with instance_count |
| C sockets API | YES | `sockets.rs` — Full POSIX socket API (socket/bind/listen/accept/connect/send/recv) |
| Assembler sockets (EZASMI) | GAP | z/OS-specific macro interface; low priority |
| REXX sockets | GAP | Depends on REXX integration (Batch 1) |
| CINET | YES (now implemented) | `tcpip.rs` — CinetConfig with multi-stack support |
| IPv4 support | YES | `sockets.rs` — AF_INET, full TCP/UDP |
| IPv6 support | YES | `sockets.rs` — AF_INET6, IPv6 socket creation and binding |

### AT-TLS (NET-105)

| Feature | Status | Notes |
|---------|--------|-------|
| Policy Agent configuration | YES | `tls.rs` — PolicyAgent with TlsRule matching |
| TTLSRule / TTLSGroupAction / TTLSEnvironmentAction | YES | `tls.rs` — TlsRule with direction, version range, cipher suites |
| Transparent TLS encryption | YES | `tls.rs` — perform_handshake() simulates AT-TLS interception |
| System SSL integration | YES | `tls.rs` — TLS version negotiation (1.0-1.3) |
| RACF keyring integration | YES | `tls.rs` — Keyring, KeyringStore, Certificate structs |
| TLS 1.2 / 1.3 support | YES | `tls.rs` — TlsVersion enum with negotiation |
| Client certificate auth | YES | `tls.rs` — HandshakeRole::ServerWithClientAuth, validate_client_cert() |

### Application Services (NET-106, NET-107)

| Feature | Status | Notes |
|---------|--------|-------|
| TN3270E server | YES | `ssh.rs` — Tn3270Server with multi-session, device types, structured fields |
| FTP server/client | YES | `ftp.rs` — FtpServer with auth, USS/MVS transfer, JES2 submission; FtpClient |
| FTPS (FTP over TLS) | GAP | TLS layer exists but not integrated into FTP |
| SSH (OpenSSH) | YES | `ssh.rs` — SshServer with password/pubkey auth, shell access |
| NFS server | GAP | Not implemented |
| NFS client | GAP | Not implemented |
| DNS (BIND) | GAP | Not implemented (resolver config exists in TCPIP.DATA) |
| SNMP agent | GAP | Not implemented |
| SMTP | GAP | Not implemented |
| HTTP server (IHS) | GAP | Health endpoint exists in deploy crate, not in networking |

### Sysplex Networking (NET-108)

| Feature | Status | Notes |
|---------|--------|-------|
| Static VIPA | YES | `sysplex.rs` — StaticVipa with VipaManager |
| Dynamic VIPA | YES | `sysplex.rs` — DynamicVipa with failover() |
| Sysplex Distributor | YES | `sysplex.rs` — SysplexDistributor with WLM-weighted routing |
| Dynamic XCF | GAP | Cross-system coupling not modeled |
| Connection routing | YES | `sysplex.rs` — route_connection() with WLM weight scoring |

### Security & Policy (NET-109)

| Feature | Status | Notes |
|---------|--------|-------|
| IPSec | YES | `security.rs` — SecurityAssociation with IKE auth, ESP/AH algorithms |
| IP filtering | YES | `security.rs` — IpFilterRule with CIDR matching, permit/deny/ipsec |
| IDS (Intrusion Detection) | GAP | Not implemented |
| Policy Agent | YES | `tls.rs` — PolicyAgent for AT-TLS (security policy partially in DefenseManager) |
| RACF certificate management | YES | `tls.rs` — Keyring/KeyringStore/Certificate with CA validation |
| Defense Manager | YES | `security.rs` — DefenseManager with rule loading, dynamic updates, SA management |

### Summary

| Category | Total Features | Implemented | Newly Implemented | GAP |
|----------|---------------|-------------|-------------------|-----|
| VTAM/SNA | 16 | 15 | 5 (RPL, NIB, EXLST, MODCB/SHOWCB/TESTCB, APPL def, LU 3) | 1 (PU/LU defs) |
| APPC/LU 6.2 | 11 | 11 | 4 (CNOS, TP registry, Mode defs, ConversationType) | 0 |
| APPN/HPR/EE | 6 | 0 | 0 | 6 |
| TCP/IP Stack | 10 | 8 | 2 (TCPIP.DATA, CINET) | 2 (EZASMI, REXX sockets) |
| AT-TLS | 7 | 7 | 0 | 0 |
| Application Services | 10 | 4 | 0 | 6 (FTPS, NFS, DNS, SNMP, SMTP, IHS) |
| Sysplex Networking | 5 | 4 | 0 | 1 (Dynamic XCF) |
| Security & Policy | 5 | 4 | 0 | 1 (IDS) |
| **Totals** | **70** | **53** | **11** | **17** |

**Test count:** 155 passing (was 132 before this review, +23 new tests)

**Key gaps remaining:**
- APPN/HPR/Enterprise Extender: Full SNA topology/routing protocol stack (6 features) -- low priority for emulation; real-world z/OS networking is shifting to TCP/IP
- NFS/DNS/SNMP/SMTP: Protocol servers that can be added incrementally
- EZASMI/REXX sockets: z/OS-specific socket calling conventions dependent on assembler/REXX runtime
- Dynamic XCF / IDS: Advanced sysplex and security features
