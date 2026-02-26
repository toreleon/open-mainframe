# open-mainframe-networking

z/OS Networking — a comprehensive Rust implementation of the mainframe networking stack, including VTAM/SNA, TCP/IP, and high-level protocols like FTP and SSH for the OpenMainframe project.

## Overview

Networking on the mainframe is a unique blend of legacy SNA (Systems Network Architecture) and modern TCP/IP. This crate reimplements the core networking components of z/OS, enabling terminal access via TN3270E, secure file transfers via FTP, and programmatic communication via VTAM ACBs and POSIX sockets.

## Architecture

```
    Application Layer                     Networking Stack
    ┌──────────────┐                      ┌────────────────────┐
    │  FTP / SSH   │    High-Level        │   Protocol Stack   │
    │  TN3270E     │ ──────────────────>  │    (TCP/IP / SNA)  │
    └──────────────┘    Servers           │  Sockets, Sessions │
           │                               └────────────────────┘
           ▼                                        │
    ┌──────────────┐    SNA Control       ┌────────────────────┐
    │  VTAM / ACB  │ ──────────────────>  │    SNA Subsystem   │
    │  Sessions    │    VtamManager       │  LU0, LU2, LU6.2   │
    └──────────────┘                      └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Security          ┌────────────────────┐
    │  AT-TLS /    │ <──────────────────  │   Security Agent   │
    │  IPSec       │    PolicyAgent       │  Certs, Keyrings   │
    └──────────────┘                      └────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `vtam` | VTAM (Virtual Telecommunications Access Method): ACBs, RPLs, and sessions |
| `sna` | SNA (Systems Network Architecture): LU types (0/1/2/3), BIND, and 3270 streams |
| `appc` | APPC (Advanced Program-to-Program Communication): LU 6.2 and CPI-C API |
| `tcpip` | TCP/IP stack configuration, profiling, and routing |
| `sockets` | POSIX-compatible sockets layer (TCP/UDP) |
| `tls` | AT-TLS (Application Transparent TLS): Policy-based security and keyrings |
| `ftp` | FTP server and client: Support for MVS datasets, USS files, and JES2 submission |
| `ssh` | SSH and TN3270E: Secure terminal access and data stream mapping |
| `sysplex` | Sysplex networking: VIPA, Sysplex Distributor, and WLM routing |
| `security`| IP-level security: IP filtering and IPSec Security Associations |

## Key Types and Components

### VTAM & SNA
- `Acb`: Application Control Block — the entry point for VTAM applications.
- `Rpl`: Request Parameter List — defines individual I/O operations (SEND/RECEIVE).
- `Lu2Session`: Specialized session for 3270 terminal communication.

### TCP/IP & Sockets
- `TcpIpProfile`: Parser for the complex `TCPIP.PROFILE` configuration dataset.
- `SocketRuntime`: Manages the lifecycle of TCP and UDP connections.

### High-Level Protocols
- `FtpServer`: Multi-protocol FTP server supporting traditional MVS and UNIX filesystems.
- `Tn3270Server`: Implementation of the TN3270E protocol for terminal emulation.

## Feature Coverage

| Feature | Category | Status |
|---------|----------|--------|
| VTAM ACB/RPL | SNA | Implemented (OPEN, CLOSE, SEND, RECEIVE) |
| LU 6.2 (APPC)| SNA | Implemented (Conversations, CPI-C) |
| 3270 Streams | SNA | Implemented (Orders, attributes, fields) |
| Sockets API  | TCP/IP | Implemented (BSD-compatible) |
| AT-TLS       | Security| Implemented (Policy Agent, Keyrings) |
| FTP (MVS/USS)| Protocol| Implemented |
| TN3270E      | Protocol| Implemented |
| Dynamic VIPA | Sysplex | Implemented |

## Usage Examples

### Opening a VTAM ACB

```rust
use open_mainframe_networking::vtam::{Acb, AcbAuth};

let mut acb = Acb::new("MYAPPL");
acb.set_auth(AcbAuth::ACQUIRE | AcbAuth::PASS);
acb.open().expect("Failed to open ACB");
```

### Starting an FTP Transfer

```rust
use open_mainframe_networking::ftp::FtpClient;

let mut client = FtpClient::connect("127.0.0.1:21").unwrap();
client.login("USER1", "PASS1").unwrap();
client.put_dataset("LOCAL.FILE", "REMOTE.DATASET").unwrap();
```

## Testing

The Networking crate includes 450+ tests:
- **SNA**: 3270 data stream rendering and buffer address encoding tests.
- **TCP/IP**: Profile parser validation with hundreds of configuration variants.
- **FTP**: Multi-threaded transfer tests for both MVS and USS files.
- **TLS**: Certificate chain validation and handshake simulation.

```sh
cargo test -p open-mainframe-networking
```
