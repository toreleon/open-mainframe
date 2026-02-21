---
version: 'v5.0'
planningGroup: 'PG-10'
technology: 'System Commands & Console'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'zos-complete-inventory.md (AREA-2)'
epicIds: ['SYS-102', 'SYS-103']
sysEpicIds: ['SYS-102', 'SYS-103']
---

# PRD: System Commands & Console

## 1. Problem Statement

z/OS operators and developers interact with the system via MVS system commands (DISPLAY, START, STOP, CANCEL, REPLY, VARY) and SDSF (System Display and Search Facility). The DISPLAY command family has 20+ subcommands for viewing active jobs, storage, devices, and subsystem status. SDSF is the most-used operator/developer tool after ISPF, providing live job status, output browsing, and system log viewing. OpenMainframe has z/OSMF console API stubs but no actual system command processor, no SDSF, and no console infrastructure.

## 2. Functional Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| FR-CMD-001 | DISPLAY A,L shall list active address spaces with status | AREA-2 |
| FR-CMD-002 | DISPLAY J,jobname shall show job details (step, RC, CPU) | AREA-2 |
| FR-CMD-003 | DISPLAY T shall show system date and time | AREA-2 |
| FR-CMD-004 | DISPLAY M shall show storage usage | AREA-2 |
| FR-CMD-005 | START/STOP/MODIFY shall control address space lifecycle | AREA-2 |
| FR-CMD-006 | CANCEL shall terminate jobs/address spaces | AREA-2 |
| FR-CMD-007 | REPLY shall respond to WTOR messages | AREA-2 |
| FR-CMD-008 | SDSF DA panel shall display active jobs with filtering | AREA-2 |
| FR-CMD-009 | SDSF ST panel shall show job status with metrics | AREA-2 |
| FR-CMD-010 | SDSF O/H panels shall browse output/held queues | AREA-2 |
| FR-CMD-011 | SDSF LOG panel shall display system message log | AREA-2 |
| FR-CMD-012 | SDSF REXX API (ISFEXEC) shall enable programmatic access | AREA-2 |
| FR-CMD-013 | JES2 operator commands ($D, $S, $P, $C, $A, $T) shall be routed correctly | AREA-2 |

## 3. Non-Functional Requirements

| ID | Requirement | Threshold |
|----|-------------|-----------|
| NFR-CMD-001 | SDSF DA panel render time for 1000 jobs | < 500ms |
| NFR-CMD-002 | No unsafe code | `#![forbid(unsafe_code)]` |
| NFR-CMD-003 | Error diagnostics | thiserror + miette |
| NFR-CMD-004 | Command output format matches IBM conventions | IEE prefix for MVS messages |

## 4. Dependencies

| Dependency | Direction | Description |
|------------|-----------|-------------|
| open-mainframe-jcl | CMD → JCL | JES2 job info, spool data |
| open-mainframe-tso | CMD → TSO | TSO session context |
| open-mainframe-ispf | CMD → ISPF | SDSF panel rendering |
| open-mainframe-mvs | CMD → MVS | WTO/WTOR integration, task management |
