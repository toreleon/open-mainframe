---
version: 'v5.0'
planningGroup: 'PG-16'
technology: 'Networking & TCP/IP'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-networking-v5.0.md'
  - 'architecture-networking-v5.0.md'
totalEpics: 10
totalStories: 52
frCoverage: '12/12 (100%)'
nfrCoverage: '3/3 (100%)'
---

# Epics & Stories: Networking & TCP/IP

## Epic Overview

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| NET-100 | VTAM Application Interface | L | 5 | C |
| NET-101 | SNA LU Type Support | M | 4 | C |
| NET-102 | APPC / LU 6.2 / CPI-C | L | 6 | C |
| NET-103 | TCP/IP Stack Configuration | M | 5 | C |
| NET-104 | Sockets API Compatibility Layer | L | 6 | C |
| NET-105 | AT-TLS & Security | L | 5 | C |
| NET-106 | FTP Client/Server | L | 6 | C |
| NET-107 | SSH Server & TN3270E Enhancement | M | 5 | C |
| NET-108 | Sysplex Networking (VIPA/Distributor) | L | 5 | C |
| NET-109 | IP Filtering & IPSec | M | 5 | C |

---

## NET-100: VTAM Application Interface

**User Value:** Applications can use the VTAM API (ACB/RPL/NIB) to manage SNA sessions, enabling CICS and IMS terminal connectivity.

### NET-100.1: ACB Open/Close

**As a** application programmer, **I want** OPEN ACB and CLOSE ACB for VTAM application registration.

**Acceptance Criteria:**
- Given OPEN ACB with APPLID=CICSAPPL, when issued, then the application is registered with VTAM
- Given CLOSE ACB, when issued, then all sessions are terminated and the application is deregistered

### NET-100.2: SETLOGON and Session Initiation

**As a** application programmer, **I want** SETLOGON to accept session requests from terminals.

**Acceptance Criteria:**
- Given SETLOGON OPTCD=START on an ACB, when a terminal LOGONs to the APPLID, then a session is initiated

### NET-100.3: SEND/RECEIVE Data Transfer

**As a** application programmer, **I want** SEND and RECEIVE RPL macros for session data transfer.

**Acceptance Criteria:**
- Given an active session, when SEND RPL is issued with data, then the data is transmitted to the session partner
- Given RECEIVE RPL, when data arrives, then it is delivered to the application buffer

### NET-100.4: CLSDST — Session Termination

**As a** application programmer, **I want** CLSDST to terminate individual sessions.

**Acceptance Criteria:**
- Given CLSDST RPL for a session, when issued, then the session is ended and resources are freed

### NET-100.5: VTAM Tests

**Acceptance Criteria:**
- Given VTAM ACB/RPL tests, when `cargo test -p open-mainframe-networking` VTAM tests run, then all pass

---

## NET-101: SNA LU Type Support

**User Value:** SNA LU types enable different terminal and program-to-program session types required by CICS and IMS.

### NET-101.1: LU Type 2 (3270 Terminal)

**As a** system programmer, **I want** LU 2 sessions for 3270 terminal emulation.

**Acceptance Criteria:**
- Given a TN3270E connection, when bound as LU 2, then 3270 data stream commands (Write, Erase/Write, Read Buffer) are supported

### NET-101.2: LU Type 0 (Unformatted)

**As a** system programmer, **I want** LU 0 for basic unformatted data exchange.

**Acceptance Criteria:**
- Given an LU 0 session, when data is sent, then raw byte streams are exchanged without formatting

### NET-101.3: LU Type 1/3 (SCS/DSC Printers)

**As a** system programmer, **I want** LU 1 and LU 3 for printer sessions.

**Acceptance Criteria:**
- Given an LU 1 session, when print data is sent, then SCS (SNA Character String) control codes are processed

### NET-101.4: Session BIND Parameters

**As a** system programmer, **I want** BIND negotiation with session parameters.

**Acceptance Criteria:**
- Given BIND with RU size 3840, when the session is established, then the maximum RU size is enforced

---

## NET-102: APPC / LU 6.2 / CPI-C

**User Value:** Programs can communicate peer-to-peer using APPC conversations, enabling distributed transaction processing.

### NET-102.1: ALLOCATE — Start Conversation

**As a** application programmer, **I want** ALLOCATE to start an APPC conversation with a partner program.

**Acceptance Criteria:**
- Given ALLOCATE(TP_NAME='WORKER', PARTNER_LU='REMOTE.LU'), when issued, then a conversation is established

### NET-102.2: SEND_DATA / RECEIVE

**As a** application programmer, **I want** SEND_DATA and RECEIVE for conversation data exchange.

**Acceptance Criteria:**
- Given a conversation in SEND state, when SEND_DATA is issued with data, then the data is queued for the partner
- Given RECEIVE in RECEIVE state, when partner data arrives, then it is delivered to the application

### NET-102.3: CONFIRM / CONFIRMED

**As a** application programmer, **I want** CONFIRM for synchronization between conversation partners.

**Acceptance Criteria:**
- Given CONFIRM, when the partner issues CONFIRMED, then the conversation continues with both sides synchronized

### NET-102.4: DEALLOCATE — End Conversation

**As a** application programmer, **I want** DEALLOCATE to end a conversation normally or abnormally.

**Acceptance Criteria:**
- Given DEALLOCATE TYPE(SYNC_LEVEL), when issued with sync_level=CONFIRM, then the conversation is ended after confirmation

### NET-102.5: CPI-C Interface

**As a** application programmer, **I want** CPI-C (Common Programming Interface for Communications) as a portable APPC API.

**Acceptance Criteria:**
- Given CMINIT/CMALLC/CMSEND/CMRCV/CMDEAL CPI-C calls, when used, then they map to underlying APPC operations

### NET-102.6: APPC Tests

**Acceptance Criteria:**
- Given two programs exchanging data via APPC, when tested end-to-end, then messages are delivered correctly

---

## NET-103: TCP/IP Stack Configuration

**User Value:** System programmers can configure the TCP/IP stack via TCPIP.PROFILE, controlling ports, interfaces, and protocol options.

### NET-103.1: TCPIP.PROFILE Parser

**As a** system programmer, **I want** TCPIP.PROFILE parsed for stack configuration.

**Acceptance Criteria:**
- Given PORT 21 TCP FTPD, when parsed, then port 21 is reserved for the FTP daemon
- Given TCPCONFIG TTLS, when parsed, then AT-TLS is enabled

### NET-103.2: Port Reservation and AUTOLOG

**As a** system programmer, **I want** PORT and AUTOLOG entries for service management.

**Acceptance Criteria:**
- Given AUTOLOG 5 FTPD, when the stack starts, then 5 instances of FTPD are automatically started

### NET-103.3: Interface and Routing Configuration

**As a** system programmer, **I want** DEVICE/LINK/HOME for interface configuration and routing.

**Acceptance Criteria:**
- Given HOME 10.1.1.100 ETHLINK, when configured, then the IP address is bound to the interface

### NET-103.4: DNS Resolver Configuration

**As a** system programmer, **I want** NSINTERADDR/RESOLVERTIMEOUT for DNS resolution.

**Acceptance Criteria:**
- Given NSINTERADDR 10.1.1.1, when gethostbyname() is called, then DNS queries go to 10.1.1.1

### NET-103.5: TCP/IP Configuration Tests

**Acceptance Criteria:**
- Given various TCPIP.PROFILE configurations, when parsed, then all directives are correctly applied

---

## NET-104: Sockets API Compatibility Layer

**User Value:** Applications using the standard POSIX sockets API can run on z/OS without modification.

### NET-104.1: Socket Creation and Address Binding

**As a** developer, **I want** socket()/bind() wrapping Tokio TCP/UDP.

**Acceptance Criteria:**
- Given socket(AF_INET, SOCK_STREAM, 0), when called, then a Tokio TCP socket is created behind a POSIX fd abstraction

### NET-104.2: TCP Server Operations

**As a** developer, **I want** listen()/accept() for TCP servers.

**Acceptance Criteria:**
- Given listen(fd, 128), when clients connect, then accept() returns new connection fds

### NET-104.3: TCP Client Operations

**As a** developer, **I want** connect() for TCP clients.

**Acceptance Criteria:**
- Given connect(fd, server_addr), when the server is listening, then the connection is established

### NET-104.4: Data Transfer (send/recv/sendto/recvfrom)

**As a** developer, **I want** data transfer operations for TCP and UDP.

**Acceptance Criteria:**
- Given send(fd, data, len, 0) on a TCP connection, when called, then data is transmitted via Tokio

### NET-104.5: I/O Multiplexing (select/poll)

**As a** developer, **I want** select()/poll() for I/O multiplexing over Tokio.

**Acceptance Criteria:**
- Given select() with multiple fds, when one becomes readable, then select returns with that fd set
- Given 10,000 concurrent fds, when poll() is used, then it handles them within performance bounds

### NET-104.6: Socket Options and IPv6

**As a** developer, **I want** setsockopt/getsockopt and AF_INET6 support.

**Acceptance Criteria:**
- Given setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, 1), when set, then address reuse is enabled
- Given AF_INET6 sockets, when used, then IPv6 communication works

---

## NET-105: AT-TLS & Security

**User Value:** Network connections are transparently encrypted via AT-TLS policies without application changes.

### NET-105.1: AT-TLS Policy Agent

**As a** security administrator, **I want** Policy Agent rules mapping connections to TLS parameters.

**Acceptance Criteria:**
- Given a rule matching port 443, when a connection is accepted, then TLS is automatically negotiated

### NET-105.2: TLS Handshake via rustls

**As a** developer, **I want** TLS 1.2/1.3 handshake transparent to the application.

**Acceptance Criteria:**
- Given an AT-TLS-enabled port, when a client connects, then TLS handshake completes within 500ms

### NET-105.3: RACF Keyring Integration

**As a** security administrator, **I want** certificates loaded from RACF keyrings.

**Acceptance Criteria:**
- Given KEYRING(ring01) in the AT-TLS rule, when TLS starts, then certificates are loaded from ring01

### NET-105.4: Client Certificate Authentication

**As a** security administrator, **I want** mutual TLS with client certificate validation.

**Acceptance Criteria:**
- Given HandshakeRole(ServerWithClientAuth), when the client presents a certificate, then it is validated against the trust store

### NET-105.5: AT-TLS Tests

**Acceptance Criteria:**
- Given TLS policy rules, when connections are made, then encryption is applied correctly

---

## NET-106: FTP Client/Server

**User Value:** FTP provides file transfer to/from z/OS datasets and USS files, plus JES2 job submission via FTP.

### NET-106.1: FTP Server Core

**As a** user, **I want** an FTP server accepting connections on port 21.

**Acceptance Criteria:**
- Given FTP server started, when a client connects and authenticates, then USER/PASS are validated against RACF

### NET-106.2: USS File Transfer

**As a** user, **I want** FTP GET/PUT for USS files.

**Acceptance Criteria:**
- Given `GET /u/jsmith/data.txt`, when issued, then the USS file is transferred to the client

### NET-106.3: MVS Dataset Transfer

**As a** user, **I want** FTP GET/PUT for MVS datasets using SITE/QUOTE commands.

**Acceptance Criteria:**
- Given `QUOTE SITE FILETYPE=SEQ` + `GET 'MY.DATA.SET'`, when issued, then the sequential dataset is transferred
- Given `QUOTE SITE FILETYPE=JES`, when set, then submitted JCL is routed to JES2

### NET-106.4: JES2 Job Submission via FTP

**As a** user, **I want** FTP-based JCL submission to JES2.

**Acceptance Criteria:**
- Given SITE FILETYPE=JES + PUT jcl.txt, when submitted, then JES2 assigns a job number and returns it in the FTP reply

### NET-106.5: FTP Client

**As a** user, **I want** an FTP client for outbound transfers.

**Acceptance Criteria:**
- Given `ftp remotehost`, when connected, then standard FTP commands (ls, get, put, mget, mput) work

### NET-106.6: FTP Tests

**Acceptance Criteria:**
- Given FTP file transfer tests, when `cargo test -p open-mainframe-networking` FTP tests run, then all pass

---

## NET-107: SSH Server & TN3270E Enhancement

**User Value:** SSH provides secure remote access to USS shell, and enhanced TN3270E supports multi-session terminal access.

### NET-107.1: SSH Server — Authentication

**As a** user, **I want** SSH server with password and public key authentication.

**Acceptance Criteria:**
- Given SSH connection to port 22, when user authenticates with password, then RACF validation occurs
- Given SSH with public key in ~/.ssh/authorized_keys, when matched, then access is granted

### NET-107.2: SSH Server — Shell Access

**As a** user, **I want** SSH to provide a USS shell session.

**Acceptance Criteria:**
- Given authenticated SSH session, when a shell is requested, then /bin/sh is spawned with the user's USS environment

### NET-107.3: TN3270E Server Enhancement

**As a** operator, **I want** an enhanced TN3270E server with multiple session support.

**Acceptance Criteria:**
- Given a TN3270E connection, when device-type negotiation completes, then the terminal is connected to TSO or CICS
- Given multiple TN3270E sessions from one client, when established, then each session operates independently

### NET-107.4: TN3270E Data Stream Processing

**As a** developer, **I want** complete 3270 data stream processing (structured fields, partitions).

**Acceptance Criteria:**
- Given structured field data (Query Reply), when received, then terminal capabilities are parsed

### NET-107.5: SSH/TN3270E Tests

**Acceptance Criteria:**
- Given SSH and TN3270E connectivity tests, when run, then authentication and data exchange work correctly

---

## NET-108: Sysplex Networking (VIPA/Distributor)

**User Value:** Sysplex networking enables high availability and workload balancing across multiple z/OS images.

### NET-108.1: Static VIPA

**As a** system programmer, **I want** static VIPA addresses for application addressability.

**Acceptance Criteria:**
- Given VIPARANGE DEFINE 10.1.1.100, when configured, then the address is available on the TCP/IP stack

### NET-108.2: Dynamic VIPA

**As a** system programmer, **I want** Dynamic VIPA for automatic IP takeover.

**Acceptance Criteria:**
- Given a Dynamic VIPA, when the owning stack fails, then the VIPA moves to a backup stack

### NET-108.3: Sysplex Distributor

**As a** system programmer, **I want** Sysplex Distributor for TCP connection routing.

**Acceptance Criteria:**
- Given Sysplex Distributor configured for port 8080, when a client connects, then the connection is routed to the target stack with best WLM performance

### NET-108.4: WLM-Based Routing

**As a** system programmer, **I want** WLM integration for routing decisions.

**Acceptance Criteria:**
- Given server weights from WLM, when Distributor routes connections, then higher-capacity servers receive more connections

### NET-108.5: Sysplex Networking Tests

**Acceptance Criteria:**
- Given VIPA and Distributor configuration, when tested, then routing and failover work correctly

---

## NET-109: IP Filtering & IPSec

**User Value:** Network security policies filter traffic by IP/port and encrypt communications between systems.

### NET-109.1: IP Filter Rules

**As a** security administrator, **I want** IP filter rules controlling traffic by source/destination/port.

**Acceptance Criteria:**
- Given rule DENY SRC 10.0.0.0/8 DST * PORT 23, when a connection from 10.x.x.x to port 23 is attempted, then it is blocked

### NET-109.2: IP Filter Actions (Permit/Deny/IPSec)

**As a** security administrator, **I want** permit, deny, and IPSec actions on filter rules.

**Acceptance Criteria:**
- Given action IPSEC on a rule, when traffic matches, then IPSec encryption is required

### NET-109.3: IPSec Security Associations

**As a** security administrator, **I want** IPSec SAs for encrypted communication.

**Acceptance Criteria:**
- Given a pre-shared key IKE negotiation, when a matching filter triggers, then an IPSec tunnel is established

### NET-109.4: Defense Manager Daemon (DMD) Model

**As a** system programmer, **I want** a filter management model similar to z/OS Defense Manager.

**Acceptance Criteria:**
- Given filter rules loaded at stack start, when a rule update is issued, then rules are dynamically refreshed

### NET-109.5: Security Tests

**Acceptance Criteria:**
- Given IP filter and IPSec tests, when run, then traffic filtering and encryption work correctly

---

## FR/NFR Coverage Matrix

| FR | Stories |
|----|---------|
| FR-NET-001 | NET-100.1, NET-100.2, NET-100.3, NET-100.4 |
| FR-NET-002 | NET-101.1, NET-101.2, NET-101.3, NET-101.4 |
| FR-NET-003 | NET-102.1, NET-102.2, NET-102.3, NET-102.4, NET-102.5 |
| FR-NET-004 | NET-103.1, NET-103.2, NET-103.3, NET-103.4 |
| FR-NET-005 | NET-104.1, NET-104.2, NET-104.3, NET-104.4, NET-104.5, NET-104.6 |
| FR-NET-006 | NET-105.1, NET-105.2, NET-105.3, NET-105.4 |
| FR-NET-007 | NET-106.1, NET-106.2, NET-106.3, NET-106.5 |
| FR-NET-008 | NET-107.1, NET-107.2 |
| FR-NET-009 | NET-107.3, NET-107.4 |
| FR-NET-010 | NET-108.1, NET-108.2 |
| FR-NET-011 | NET-108.3, NET-108.4 |
| FR-NET-012 | NET-109.1, NET-109.2, NET-109.3, NET-109.4 |

| NFR | Stories |
|-----|---------|
| NFR-NET-001 | NET-104.2, NET-104.3 |
| NFR-NET-002 | NET-105.2 |
| NFR-NET-003 | NET-104.5 |

**Coverage: 12/12 FRs (100%), 3/3 NFRs (100%)**
