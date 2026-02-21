---
version: 'v5.0'
planningGroup: 'PG-7'
technology: 'ABEND Framework & Debugging'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-abend-v5.0.md'
  - 'architecture-abend-v5.0.md'
totalEpics: 1
totalStories: 8
frCoverage: '12/12 (100%)'
nfrCoverage: '5/5 (100%)'
---

# Epics & Stories: ABEND Framework & Debugging

## Epic Overview

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| SYS-111 | ABEND Code Framework & Dump Support | M | 8 | A |

---

## SYS-111: ABEND Code Framework & Dump Support

**User Value:** ABEND codes are a meaningful diagnostic system — system ABENDs (S0C1, S0C4, S0C7, S013, S322, S806, S913, SB37) and user ABENDs (U0000-U4095) are properly classified, formatted, and produce diagnostic dumps per JCL DD specifications.

### SYS-111.1: System ABEND Code Registry

**As a** systems programmer, **I want** a comprehensive registry of system ABEND codes with descriptions, **so that** ABEND diagnostics are meaningful.

**Acceptance Criteria:**
- Given S0C1 (operation exception), when looked up, then the registry returns "OPERATION EXCEPTION" with common causes and recovery hints
- Given S0C4 (protection exception), when looked up, then the registry returns "PROTECTION EXCEPTION (INVALID STORAGE REFERENCE)"
- Given S0C7 (data exception), when looked up, then the registry returns "DATA EXCEPTION (INVALID DECIMAL DATA)"
- Given S013 (DCB error), when looked up, then the description includes "CONFLICTING DCB PARAMETERS"
- Given S322 (time limit), S222 (cancel), S806 (module not found), S913 (RACF), SB37/SD37/SE37 (space), when looked up, then all return appropriate descriptions
- Given at least 30 system ABEND codes registered, when the registry is queried, then all return valid entries
- Given an unknown system code (e.g., SFFF), when looked up, then a generic "UNKNOWN SYSTEM ABEND" is returned

### SYS-111.2: User ABEND Code Support

**As a** application programmer, **I want** user ABEND codes (U0000-U4095) with optional reason codes, **so that** applications can signal specific error conditions.

**Acceptance Criteria:**
- Given `ABEND USER=1234`, when issued by a program, then the task terminates with completion code U1234
- Given `ABEND USER=1234,REASON=0008`, when issued, then both user code and reason code are recorded
- Given `ABEND USER=1234,DUMP=NO`, when issued, then the ABEND occurs without generating a dump
- Given user code validation, when code > 4095 is specified, then an error is returned

### SYS-111.3: ABEND-to-JCL Step Completion Integration

**As a** JCL developer, **I want** ABEND codes propagated to JCL step completion, **so that** COND= and IF/THEN logic can act on ABENDs.

**Acceptance Criteria:**
- Given a step that ABENDs with S0C7, when subsequent steps check `IF (STEP1.ABEND)`, then the condition is true
- Given a step that ABENDs with S0C7, when `COND=(0,NE)` is on the next step, then the next step is bypassed (ABEND sets high CC)
- Given a step with an ABEND, when the job log is written, then the ABEND code appears as `SYSTEM=0C7` or `USER=1234`
- Given ABEND in a step without ESTAE recovery, when the step ends, then the step CC shows the ABEND code

### SYS-111.4: SYSUDUMP — Formatted User Dump

**As a** application programmer, **I want** SYSUDUMP DD to generate a formatted dump on ABEND, **so that** I can diagnose application failures.

**Acceptance Criteria:**
- Given `//SYSUDUMP DD SYSOUT=*` in JCL, when ABEND occurs, then a formatted dump is written to the SYSUDUMP DD
- Given the dump output, when examined, then it contains:
  - ABEND code and reason code
  - General purpose registers (R0-R15)
  - PSW (Program Status Word) at time of ABEND
  - Traceback (call stack with module names and addresses)
  - User program storage (working storage, save areas)
- Given no SYSUDUMP DD in JCL, when ABEND occurs, then no dump is produced (ABEND still recorded)

### SYS-111.5: SYSABEND — Extended Dump with System Areas

**As a** systems programmer, **I want** SYSABEND DD to include system areas in the dump, **so that** system-level failures can be diagnosed.

**Acceptance Criteria:**
- Given `//SYSABEND DD SYSOUT=*` in JCL, when ABEND occurs, then the dump includes everything in SYSUDUMP plus:
  - Task control block (TCB) contents
  - Recovery routine chain (ESTAE entries)
  - System trace entries (if available)
- Given both SYSUDUMP and SYSABEND in JCL, when ABEND occurs, then SYSUDUMP takes priority (only one dump produced)

### SYS-111.6: SYSMDUMP — Unformatted Machine Dump

**As a** systems programmer, **I want** SYSMDUMP DD to produce a binary dump, **so that** dumps can be analyzed with IPCS or other tools.

**Acceptance Criteria:**
- Given `//SYSMDUMP DD DSN=MY.DUMP,DISP=(NEW,CATLG)` in JCL, when ABEND occurs, then a binary dump dataset is created
- Given the binary dump, when read, then it contains a structured header (dump title, date, time, ABEND code) followed by raw storage contents
- Given SYSMDUMP priority, when only SYSMDUMP DD exists, then the unformatted dump is produced

### SYS-111.7: SNAP Dump (Non-Terminating)

**As a** application programmer, **I want** SNAP to capture a point-in-time dump during execution without terminating, **so that** I can debug intermittent issues.

**Acceptance Criteria:**
- Given `SNAP DCB=SNAPDCB,STORAGE=(FROM,TO),ID=1`, when issued, then the specified storage range is written to the SNAP dataset and execution continues
- Given SNAP PDATA=(REGS,SA), when issued, then registers and save areas are included in the SNAP output
- Given multiple SNAP calls with different IDs, when the dataset is examined, then each SNAP is identifiable by its ID
- Given SNAP DCB not opened, when SNAP is issued, then an error is returned (DCB must be opened first)

### SYS-111.8: ABEND Diagnostic Messages and Integration Tests

**As a** developer, **I want** ABEND diagnostic messages following IBM conventions and comprehensive tests, **so that** ABEND reporting matches z/OS standards.

**Acceptance Criteria:**
- Given S0C7 ABEND, when the job log is written, then message `IEA995I SYMPTOM DUMP` appears with the ABEND code, PSW, and failing module
- Given S806 ABEND (module not found), when reported, then message `CSV003I MODULE modulename NOT FOUND` appears
- Given a JCL job with 3 steps where step 2 ABENDs, when executed, then:
  - Step 1 runs successfully (CC=0)
  - Step 2 ABENDs with dump (if dump DD present)
  - Step 3 is evaluated based on COND= logic
- Given `cargo test -p open-mainframe-mvs` ABEND tests, then all pass

---

## FR/NFR Coverage Matrix

| FR | Stories |
|----|---------|
| FR-ABN-001 | SYS-111.1 |
| FR-ABN-002 | SYS-111.2 |
| FR-ABN-003 | SYS-111.2 |
| FR-ABN-004 | SYS-111.4 |
| FR-ABN-005 | SYS-111.5 |
| FR-ABN-006 | SYS-111.6 |
| FR-ABN-007 | SYS-111.7 |
| FR-ABN-008 | SYS-111.3 |
| FR-ABN-009 | SYS-111.8 |
| FR-ABN-010 | SYS-111.3 (references SYS-101) |
| FR-ABN-011 | SYS-111.3 (LE integration) |
| FR-ABN-012 | SYS-111.5 |

| NFR | Stories |
|-----|---------|
| NFR-ABN-001 | SYS-111.1 |
| NFR-ABN-002 | SYS-111.4, SYS-111.5, SYS-111.6 |
| NFR-ABN-003 | SYS-111.1 |
| NFR-ABN-004 | SYS-111.1 |
| NFR-ABN-005 | SYS-111.8 |

**Coverage: 12/12 FRs (100%), 5/5 NFRs (100%)**
