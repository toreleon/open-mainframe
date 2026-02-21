---
version: 'v5.0'
planningGroup: 'PG-16'
technology: 'Networking & TCP/IP'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'gap-analysis/batch-20-networking.md'
  - 'zos-complete-inventory.md (AREA-20)'
  - 'gap-analysis/batch-22-priority-matrix.md'
epicIds: ['NET-100', 'NET-101', 'NET-102', 'NET-103', 'NET-104', 'NET-105', 'NET-106', 'NET-107', 'NET-108', 'NET-109']
sysEpicIds: []
totalFRs: 12
totalNFRs: 3
---

# PRD: Networking & TCP/IP

## 1. Problem Statement

OpenMainframe has partial TCP/IP (async HTTP, TN3270E client) but no VTAM/SNA stack, no AT-TLS, no sockets compatibility layer, no Sysplex networking, and no FTP/SSH/NFS services. z/OS networking spans two protocol stacks (SNA and TCP/IP) that modern mainframe applications depend on.

## 2. User Personas

- **Network Administrator** — configures VTAM, TCP/IP profiles, AT-TLS policies, Sysplex Distributor
- **Application Developer** — uses sockets API, APPC conversations, TN3270E for terminal I/O
- **System Programmer** — manages listeners, VIPA addresses, IP filtering, application services

## 3. Functional Requirements

| ID | Requirement |
|----|-------------|
| FR-NET-001 | Implement VTAM ACB/RPL/NIB application interface for SNA session management |
| FR-NET-002 | Implement SNA LU type 2 (3270) and LU type 6.2 (APPC) session support |
| FR-NET-003 | Implement APPC/CPI-C conversation model: ALLOCATE, SEND_DATA, RECEIVE, DEALLOCATE |
| FR-NET-004 | Implement TCP/IP stack configuration via TCPIP.PROFILE parsing (PORT, PORTRANGE, AUTOLOG, TCPCONFIG, UDPCONFIG) |
| FR-NET-005 | Implement POSIX-compatible C sockets API (socket, bind, listen, accept, connect, send, recv, select, poll) as a compatibility layer over Tokio |
| FR-NET-006 | Implement AT-TLS (Application Transparent TLS) with Policy Agent integration and RACF keyring support |
| FR-NET-007 | Implement FTP client/server with MVS dataset access and JES2 job submission via FTP |
| FR-NET-008 | Implement SSH server (OpenSSH-based) for secure remote access to USS shell |
| FR-NET-009 | Enhance TN3270E server with data stream conversion and multi-session support |
| FR-NET-010 | Implement VIPA (Virtual IP Address) and Dynamic VIPA for Sysplex networking |
| FR-NET-011 | Implement Sysplex Distributor with WLM integration for workload-balanced TCP connections |
| FR-NET-012 | Implement IP filtering and basic IPSec policy enforcement |

## 4. Non-Functional Requirements

| ID | Requirement |
|----|-------------|
| NFR-NET-001 | TCP connection establishment < 10ms for localhost, < 100ms for remote |
| NFR-NET-002 | AT-TLS handshake completes within 500ms including certificate validation |
| NFR-NET-003 | Sockets API supports at least 10,000 concurrent file descriptors via poll/epoll |

## 5. Scope

**MVP:** TCP/IP configuration, sockets compatibility, AT-TLS, FTP, TN3270E enhancement
**Full:** VTAM/SNA, APPC, SSH, Sysplex networking, IP filtering
**Deferred:** APPN/HPR routing, NFS server, SNMP agent, Enterprise Extender

## 6. Dependencies

- `open-mainframe-tui` — existing TN3270E client infrastructure
- `open-mainframe-uss` (PG-15) — sockets API integration, SSH/FTP daemons
- `open-mainframe-racf` — keyrings, certificates, AT-TLS policy
- PG-12 WLM — Sysplex Distributor uses WLM for connection routing
