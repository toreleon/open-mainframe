---
version: 'v5.0'
planningGroup: 'PG-11'
technology: 'Program Management & Binder'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'zos-complete-inventory.md (AREA-3)'
epicIds: ['SYS-104', 'SYS-105']
sysEpicIds: ['SYS-104', 'SYS-105']
---

# PRD: Program Management & Binder

## 1. Problem Statement

Every compiled z/OS program (COBOL, PL/I, HLASM, C) must pass through the Binder (IEWBLINK) to produce an executable load module or program object. Programs are loaded via LOAD/LINK/XCTL/ATTACH macros and found through a search order: STEPLIB → JOBLIB → LPA → LNKLST. OpenMainframe has no binder, no object module parser, and no program search infrastructure. Without these, `EXEC PGM=` in JCL cannot load real compiled programs.

## 2. Functional Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| FR-PGM-001 | Binder shall combine object modules into load modules via INCLUDE/ENTRY/NAME | AREA-3 |
| FR-PGM-002 | External symbol resolution shall link cross-module references | AREA-3 |
| FR-PGM-003 | Relocation engine shall adjust address constants for load point | AREA-3 |
| FR-PGM-004 | LOAD macro shall load module into virtual storage | AREA-3 |
| FR-PGM-005 | LINK macro shall load and branch to program entry point | AREA-3 |
| FR-PGM-006 | XCTL macro shall transfer control without return | AREA-3 |
| FR-PGM-007 | ATTACH macro shall create subtask for program execution | AREA-3 |
| FR-PGM-008 | Program search order (STEPLIB → JOBLIB → LPA → LNKLST) | AREA-3 |
| FR-PGM-009 | Object module format parser (ESD/TXT/RLD/END records) | AREA-3 |
| FR-PGM-010 | AMODE/RMODE attributes shall be respected | AREA-3 |
| FR-PGM-011 | APF authorization shall control supervisor-state access | AREA-3 |
| FR-PGM-012 | ALIAS entry points shall be supported | AREA-3 |

## 3. Non-Functional Requirements

| ID | Requirement | Threshold |
|----|-------------|-----------|
| NFR-PGM-001 | Load module parse time for 1MB module | < 50ms |
| NFR-PGM-002 | No unsafe code | `#![forbid(unsafe_code)]` |
| NFR-PGM-003 | Error diagnostics | thiserror + miette |

## 4. Dependencies

| Dependency | Direction | Description |
|------------|-----------|-------------|
| open-mainframe-dataset | PGM → DS | PDS/PDSE load library access |
| open-mainframe-jcl | JCL → PGM | EXEC PGM= dispatches to program fetch |
| open-mainframe-mvs | PGM → MVS | Task (TCB) management for ATTACH |
| open-mainframe-racf | PGM → RACF | APF authorization checks |
