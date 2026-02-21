---
version: 'v5.0'
planningGroup: 'PG-11'
technology: 'Program Management & Binder'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-pgmmgmt-v5.0.md'
  - 'architecture-pgmmgmt-v5.0.md'
totalEpics: 2
totalStories: 16
frCoverage: '12/12 (100%)'
nfrCoverage: '3/3 (100%)'
---

# Epics & Stories: Program Management & Binder

## Epic Overview

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| SYS-104 | Binder & Program Execution (LOAD/LINK/XCTL/ATTACH) | XL | 10 | B |
| SYS-105 | Object & Load Module Format Parser | L | 6 | B |

---

## SYS-104: Binder & Program Execution

**User Value:** Compiled programs can be linked into executables and loaded for execution via JCL `EXEC PGM=`, matching how z/OS loads and runs programs.

### SYS-104.1: Program Search Order

**As a** JCL developer, **I want** programs found via STEPLIB → JOBLIB → LPA → LNKLST, **so that** `EXEC PGM=` resolves correctly.

**Acceptance Criteria:**
- Given program MYPROG in STEPLIB, when `EXEC PGM=MYPROG` is processed, then STEPLIB is searched first
- Given program not in STEPLIB but in LNKLST, when searched, then LNKLST is used as fallback
- Given program not found anywhere, when searched, then S806 ABEND occurs

### SYS-104.2: LOAD Macro (SVC 8)

**As a** developer, **I want** LOAD to bring a module into virtual storage, **so that** programs can be loaded on demand.

**Acceptance Criteria:**
- Given `LOAD EP=SUBPROG`, when issued, then the module is loaded and entry point address is returned in R0
- Given module already loaded, when LOAD is issued again, then use count is incremented (no reload)

### SYS-104.3: DELETE Macro (SVC 9)

**As a** developer, **I want** DELETE to release a loaded module.

**Acceptance Criteria:**
- Given loaded module SUBPROG with use count 1, when DELETE is issued, then the module is freed
- Given use count > 1, when DELETE is issued, then use count is decremented but module stays

### SYS-104.4: LINK Macro (SVC 6)

**As a** developer, **I want** LINK to load a program and branch to its entry point with return.

**Acceptance Criteria:**
- Given `LINK EP=SUBPROG,PARAM=parmlist`, when issued, then SUBPROG is loaded, receives the parameter list, executes, and returns to the caller
- Given LINK return, when SUBPROG returns with RC=4, then R15 contains 4

### SYS-104.5: XCTL Macro (SVC 7)

**As a** developer, **I want** XCTL to transfer control without return, **so that** chain-of-programs can execute.

**Acceptance Criteria:**
- Given `XCTL EP=NEXTPROG`, when issued, then control transfers to NEXTPROG; the caller's storage is freed
- Given XCTL, when NEXTPROG ends, then it returns to the caller's caller (not the issuer of XCTL)

### SYS-104.6: ATTACH Macro (SVC 42)

**As a** developer, **I want** ATTACH to create a subtask running a program, **so that** parallel execution is supported.

**Acceptance Criteria:**
- Given `ATTACH EP=WORKER,ECB=ecbaddr`, when issued, then a new TCB is created running WORKER
- Given the WORKER program completes, when ECB is posted, then the caller can WAIT on it

### SYS-104.7: Binder Core — Symbol Resolution

**As a** systems programmer, **I want** the binder to resolve external references across object modules.

**Acceptance Criteria:**
- Given object module A with ER (External Reference) to FUNC1, when module B provides SD (Section Definition) FUNC1, then the reference is resolved
- Given unresolved external reference, when binder link-edits, then error IEW2456E is reported

### SYS-104.8: Binder — Relocation Engine

**As a** systems programmer, **I want** address constants relocated for the load point.

**Acceptance Criteria:**
- Given RLD entry for V-type address constant, when the module is bound with base address 0x1000, then the address constant is adjusted by 0x1000
- Given A-type address constant, when relocated, then the CSECT origin is added

### SYS-104.9: APF Authorization

**As a** security administrator, **I want** APF to control which libraries produce authorized programs.

**Acceptance Criteria:**
- Given library SYS1.LINKLIB in the APF list, when a program from it is loaded, then it can execute authorized system services
- Given a non-APF library, when a program attempts authorized services, then S047 ABEND occurs

### SYS-104.10: Program Management Tests

**Acceptance Criteria:**
- Given full program lifecycle (compile→bind→LOAD→LINK→DELETE), when tested, then all steps succeed
- Given `cargo test -p open-mainframe-mvs` program management tests, then all pass

---

## SYS-105: Object & Load Module Format Parser

**User Value:** Real z/OS object decks and load modules can be parsed, enabling interoperability with existing z/OS program libraries.

### SYS-105.1: Object Module Parser (ESD/TXT/RLD/END)

**As a** developer, **I want** object modules parsed into structured data.

**Acceptance Criteria:**
- Given an 80-byte card-image object deck, when parsed, then ESD (columns 2-72), TXT, RLD, and END records are extracted
- Given ESD record, when parsed, then SD/LD/ER/WX entries are identified with names, addresses, and lengths

### SYS-105.2: Text Record Processing

**As a** developer, **I want** TXT records assembled into code segments.

**Acceptance Criteria:**
- Given TXT records for CSECT MAIN at various offsets, when assembled, then a contiguous byte array represents the CSECT

### SYS-105.3: Relocation Dictionary Processing

**As a** developer, **I want** RLD records parsed for address constant relocation.

**Acceptance Criteria:**
- Given RLD entry specifying 4-byte A-con at offset 0x100 in CSECT MAIN, when processed, then the relocation is recorded for the binder

### SYS-105.4: Load Module Format Parser

**As a** developer, **I want** existing load modules parsed from PDS load libraries.

**Acceptance Criteria:**
- Given a load module in a PDS, when parsed, then CESD (Composite ESD), text, and relocation data are extracted
- Given AMODE=31, RMODE=ANY attributes in the directory entry, when parsed, then they are available

### SYS-105.5: ALIAS Entry Points

**As a** developer, **I want** ALIAS entries in load modules, **so that** a module is callable by multiple names.

**Acceptance Criteria:**
- Given module MAIN with ALIAS ALTNAME, when `EXEC PGM=ALTNAME` is used, then MAIN is loaded via its ALIAS

### SYS-105.6: Object/Load Module Tests

**Acceptance Criteria:**
- Given test object modules with known ESD/TXT/RLD content, when parsed, then all records match expected values
- Given `cargo test` object module tests, then all pass

---

## FR/NFR Coverage Matrix

| FR | Stories |
|----|---------|
| FR-PGM-001 | SYS-104.7 |
| FR-PGM-002 | SYS-104.7 |
| FR-PGM-003 | SYS-104.8 |
| FR-PGM-004 | SYS-104.2 |
| FR-PGM-005 | SYS-104.4 |
| FR-PGM-006 | SYS-104.5 |
| FR-PGM-007 | SYS-104.6 |
| FR-PGM-008 | SYS-104.1 |
| FR-PGM-009 | SYS-105.1 |
| FR-PGM-010 | SYS-105.4 |
| FR-PGM-011 | SYS-104.9 |
| FR-PGM-012 | SYS-105.5 |

**Coverage: 12/12 FRs (100%), 3/3 NFRs (100%)**
