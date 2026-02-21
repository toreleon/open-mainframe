---
version: 'v5.0'
planningGroup: 'PG-17'
technology: 'SMF (System Management Facilities)'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'gap-analysis/batch-14-smf.md'
  - 'zos-complete-inventory.md (AREA-14)'
  - 'gap-analysis/batch-22-priority-matrix.md'
epicIds: ['SMF-100', 'SMF-101', 'SMF-102', 'SMF-103', 'SMF-104', 'SMF-105', 'SMF-106', 'SMF-107', 'SMF-108', 'SMF-109', 'SMF-110']
sysEpicIds: []
totalFRs: 12
totalNFRs: 4
---

# PRD: SMF (System Management Facilities)

## 1. Problem Statement

OpenMainframe has basic Prometheus metrics but no SMF record infrastructure. z/OS SMF provides 130+ record types for job accounting, dataset activity, security auditing, and performance monitoring. Without SMF, the system lacks the accounting, auditing, and capacity planning capabilities required by enterprise customers.

## 2. User Personas

- **System Programmer** — configures SMFPRMxx, manages SMF datasets, writes exits
- **Capacity Planner** — analyzes SMF Type 70-79 performance records for capacity planning
- **Security Auditor** — reviews SMF Type 80 records for RACF events
- **Operations** — monitors job accounting via Type 30 records
- **DevOps Engineer** — bridges SMF to modern observability (Prometheus, OpenTelemetry)

## 3. Functional Requirements

| ID | Requirement |
|----|-------------|
| FR-SMF-001 | Implement SMF record header (standard header, self-defining sections, triplets) conforming to SMFRECORDHEADER mapping |
| FR-SMF-002 | Implement binary serialization/deserialization for all SMF record types |
| FR-SMF-003 | Implement SMFPRMxx PARMLIB configuration: TYPE/NOTYPE, EXITS, INTERVAL, RECORDING, SYS/SUBSYS |
| FR-SMF-004 | Implement SMF writing APIs: SMFWTM (write record macro), SMFEWTM (extended write), buffer management |
| FR-SMF-005 | Implement SMF exit framework: IEFU83 (record filtering), IEFU84 (subsystem identification), IEFU85 (dump analysis), IFASMFEX (dynamic registration) |
| FR-SMF-006 | Implement Type 30 job accounting records (subtypes 1-5: initiation, interval, step termination, job termination, redirected output) |
| FR-SMF-007 | Implement Type 14/15/17/18 dataset activity records (open, close, scratch, rename) |
| FR-SMF-008 | Implement Type 80 RACF security audit records (command, access, event logging) |
| FR-SMF-009 | Implement Type 70-79 performance records (CPU, paging, workload, I/O, storage) |
| FR-SMF-010 | Implement Type 100-120 subsystem records (DB2, CICS, MQ, TCP/IP) |
| FR-SMF-011 | Implement SMF dump utilities: IFASMFDP (dump), IFASMFDL (log stream), filtering by type/date/time/jobname |
| FR-SMF-012 | Implement bidirectional Prometheus/OpenTelemetry bridge for modern observability |

## 4. Non-Functional Requirements

| ID | Requirement |
|----|-------------|
| NFR-SMF-001 | SMF record write latency < 100 microseconds (buffered) |
| NFR-SMF-002 | SMF dump utility processes 1GB of records in < 10 seconds |
| NFR-SMF-003 | User record types (128-255) supported for custom application recording |
| NFR-SMF-004 | SMF records compatible with existing SMF analysis tools via binary format fidelity |

## 5. Scope

**MVP:** Record infrastructure, SMFPRMxx, writing API, Type 30 job accounting, dump utility
**Full:** All record types, exit framework, security audit, performance, subsystem records, observability bridge
**Deferred:** Real-time SMF streaming, Logstream integration

## 6. Dependencies

- `open-mainframe-jcl` — job accounting fields for Type 30
- `open-mainframe-dataset` — dataset activity for Types 14/15/17/18
- `open-mainframe-racf` — security events for Type 80
- `open-mainframe-deploy` — Prometheus/OpenTelemetry metrics infrastructure
- PG-12 WLM — performance records reference WLM service classes
