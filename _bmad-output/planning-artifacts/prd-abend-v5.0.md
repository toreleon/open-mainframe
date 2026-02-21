---
version: 'v5.0'
planningGroup: 'PG-7'
technology: 'ABEND Framework & Debugging'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'zos-complete-inventory.md (AREA-11)'
  - 'architecture-mvs-services-v5.0.md (SYS-100/SYS-101)'
  - 'gap-analysis/batch-12-le.md (LE109)'
epicIds: ['SYS-111']
sysEpicIds: ['SYS-111']
---

# PRD: ABEND Framework & Debugging

## 1. Problem Statement

z/OS programs terminate abnormally via ABEND (ABnormal END) codes — system codes (S0C1, S0C4, S0C7, S013, S322, S806, S913, SB37, etc.) and user codes (U0000-U4095). These codes are the primary diagnostic mechanism for batch job failures. When an ABEND occurs, z/OS generates dumps (SYSABEND, SYSUDUMP, SYSMDUMP) containing register contents, storage contents, and diagnostic information.

OpenMainframe has partial ABEND support: CICS HANDLE ABEND, basic AbendCode types planned in SYS-100/SYS-101, and LE condition handling. However, there is no comprehensive ABEND code registry, no dump DD support (SYSABEND/SYSUDUMP/SYSMDUMP), no diagnostic message catalog for ABEND codes, and no SNAP dump capability. Real z/OS job streams depend on ABEND codes for error handling (COND= and IF/THEN), operator notification, and automated recovery.

## 2. User Personas

| Persona | Description | Key Needs |
|---------|-------------|-----------|
| Batch Developer | Writes and debugs JCL job streams | Meaningful ABEND codes, dump output, COND= integration |
| Systems Programmer | Diagnoses system-level failures | System ABEND codes (S0Cx), SDWA contents, dump analysis |
| Application Programmer | Handles errors in COBOL/PL/I/Assembler | User ABEND codes, SNAP dumps, recovery routines |
| Operator | Monitors batch job execution | ABEND notification, reason codes in job log |

## 3. Scope

### MVP (SYS-111)
- Comprehensive ABEND code registry (system + user codes)
- SYSABEND/SYSUDUMP/SYSMDUMP DD support in JCL executor
- Formatted dump generation (registers, PSW, storage areas)
- ABEND-to-message mapping (diagnostic messages)
- SNAP dump (application-generated diagnostic dumps)
- Cross-subsystem ABEND propagation (JCL ↔ LE ↔ MVS)

### Deferred
- IPCS (Interactive Problem Control System) — dump analysis tool
- GTF/CTRACE (Generalized Trace Facility) — system tracing
- SVC dump (SDUMP) — full system dump
- Stand-alone dump utility

## 4. Functional Requirements

| ID | Requirement | Acceptance Criteria | Source |
|----|-------------|---------------------|--------|
| FR-ABN-001 | System ABEND codes shall be classified with standard z/OS meanings | Given S0C7 ABEND, when reported, then the message identifies it as "DATA EXCEPTION" | AREA-11 |
| FR-ABN-002 | User ABEND codes (U0000-U4095) shall be supported | Given ABEND USER=1234, when issued, then the completion code is U1234 | AREA-11 |
| FR-ABN-003 | ABEND codes shall include reason codes for additional diagnostics | Given S0C4 with REASON=0004, when reported, then both code and reason are shown | AREA-11 |
| FR-ABN-004 | SYSUDUMP DD shall generate a formatted user dump | Given SYSUDUMP DD in JCL, when ABEND occurs, then registers, PSW, user storage, and traceback are written | Convention |
| FR-ABN-005 | SYSABEND DD shall generate a formatted dump with system areas | Given SYSABEND DD in JCL, when ABEND occurs, then dump includes nucleus areas and system control blocks in addition to user areas | Convention |
| FR-ABN-006 | SYSMDUMP DD shall generate an unformatted machine-readable dump | Given SYSMDUMP DD in JCL, when ABEND occurs, then a binary dump suitable for IPCS analysis is written | Convention |
| FR-ABN-007 | SNAP macro shall generate a point-in-time dump without termination | Given SNAP STORAGE=(FROM,TO),DCB=SNAPDCB, when issued, then the specified storage range is dumped and execution continues | AREA-11 |
| FR-ABN-008 | ABEND codes shall propagate to JCL step completion code | Given an ABEND in a step, when the step completes, then the ABEND code is available for COND= and IF/THEN processing | JCL integration |
| FR-ABN-009 | Common system ABENDs shall produce diagnostic messages in the job log | Given S806 ABEND, when reported, then "MODULE NOT FOUND" message appears in job log with module name | Convention |
| FR-ABN-010 | ESTAE/ESPIE integration shall allow recovery from ABENDs | Given ESTAE set before ABEND, when ABEND occurs, then ESTAE exit receives SDWA with full diagnostic information | SYS-101 |
| FR-ABN-011 | LE condition handling shall translate ABEND codes to LE conditions | Given S0C7 in a LE-enabled program, when the ABEND occurs, then LE CEE3xxx condition is raised before ABEND | LE integration |
| FR-ABN-012 | ABEND dump priority shall follow z/OS conventions | Given both SYSUDUMP and SYSABEND in JCL, when ABEND occurs, then SYSUDUMP takes priority (first found is used) | Convention |

## 5. Non-Functional Requirements

| ID | Requirement | Threshold |
|----|-------------|-----------|
| NFR-ABN-001 | ABEND code registry covers all commonly-encountered system ABENDs | At least 30 system ABEND codes documented |
| NFR-ABN-002 | Dump generation performance | Dump completes within 1 second for typical program storage |
| NFR-ABN-003 | No unsafe code | `#![forbid(unsafe_code)]` |
| NFR-ABN-004 | Error diagnostics | thiserror + miette per crate convention |
| NFR-ABN-005 | ABEND message format matches IBM conventions | Messages use IEA, IEC, IGD, CSV prefixes per subsystem |

## 6. Dependencies

| Dependency | Direction | Description |
|------------|-----------|-------------|
| open-mainframe-mvs | ABN → MVS | AbendCode, Sdwa, Tcb types defined in MVS crate (SYS-100) |
| open-mainframe-jcl | JCL → ABN | JCL executor recognizes dump DDs and triggers dump generation |
| open-mainframe-le | LE → ABN | LE condition handling chains to ABEND framework |
| open-mainframe-cics | CICS → ABN | HANDLE ABEND references ABEND code system |
