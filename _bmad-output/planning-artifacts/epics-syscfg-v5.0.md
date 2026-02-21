---
version: 'v5.0'
planningGroup: 'PG-22'
technology: 'System Initialization & PARMLIB Configuration'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-syscfg-v5.0.md'
  - 'architecture-syscfg-v5.0.md'
totalEpics: 4
totalStories: 24
frCoverage: '12/12 (100%)'
nfrCoverage: '3/3 (100%)'
---

# Epics & Stories: System Initialization & PARMLIB Configuration

## Epic Overview

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| SYS-117 | PARMLIB Framework & Core Members | L | 8 | D |
| SYS-118 | System Symbol Substitution | M | 4 | D |
| SYS-119 | Subsystem Configuration Members | M | 7 | D |
| SYS-120 | Initialization Sequence & Operator Commands | L | 5 | D |

---

## SYS-117: PARMLIB Framework & Core Members

**User Value:** OpenMainframe reads z/OS-compatible PARMLIB members, enabling system administrators to configure the system using familiar mainframe conventions.

### SYS-117.1: PARMLIB Concatenation & Member Discovery

**As a** system administrator, **I want** a PARMLIB concatenation that searches multiple directories for configuration members, **so that** I can organize configuration like z/OS SYS1.PARMLIB.

**Acceptance Criteria:**
- Given a PARMLIB concatenation of 3 directories, when member IEASYS00 is requested, then the first directory containing it is used
- Given a member name and suffix (e.g., "IEASYS" + "00"), when looked up, then the file `IEASYS00` in the PARMLIB path is read
- Given a member that doesn't exist in any PARMLIB dataset, when requested, then a descriptive error is returned

### SYS-117.2: Member Parser Registry

**As a** subsystem developer, **I want** to register custom PARMLIB member parsers, **so that** my crate's configuration members are handled correctly.

**Acceptance Criteria:**
- Given a parser registered for "SMFPRM", when SMFPRMxx is read, then the registered parser processes it
- Given no parser registered for a member, when the member is read, then the raw content is returned as a string

### SYS-117.3: IEASYSxx Parser — Master Parameters

**As a** system administrator, **I want** IEASYSxx parsed for master system parameters, **so that** subsystem suffixes, memory sizes, and system identity are configured.

**Acceptance Criteria:**
- Given `SYSPLEX=PLEX1,LNKLST=00,SMF=00,MAXUSER=500`, when parsed, then SystemParameters fields are populated
- Given continuation lines (comma at end), when parsed, then multi-line parameter lists are assembled
- Given comments (/*...*/ or inline), when parsed, then they are ignored

### SYS-117.4: LNKLSTxx Parser — Linklist Configuration

**As a** system administrator, **I want** LNKLSTxx parsed to define the linklist library concatenation.

**Acceptance Criteria:**
- Given `SYS1.LINKLIB,SYS1.MIGLIB,USER.LOADLIB`, when parsed, then an ordered list of LinklistEntry structs is produced
- Given volume serial specifications, when parsed, then the volume is associated with the entry

### SYS-117.5: PROGxx Parser — APF & Dynamic Updates

**As a** system administrator, **I want** PROGxx parsed for APF authorized libraries, linklist updates, and LPA additions.

**Acceptance Criteria:**
- Given `APF ADD DSNAME(MY.AUTH.LIB) SMS`, when parsed, then an ApfEntry with sms_managed=true is added
- Given `LNKLST ADD NAME(LNKLST00) DSNAME(NEW.LIB)`, when parsed, then a dynamic linklist addition is recorded

### SYS-117.6: CONSOLxx Parser — Console Definitions

**As a** system administrator, **I want** CONSOLxx parsed to define console names, authorities, and routing.

**Acceptance Criteria:**
- Given `CONSOLE DEVNUM(0009) NAME(MASTER) AUTH(MASTER) ROUTCODE(ALL)`, when parsed, then a ConsoleDefinition with Master authority is created
- Given multiple CONSOLE statements, when parsed, then all consoles are registered

### SYS-117.7: COMMNDxx Parser — IPL Commands

**As a** system administrator, **I want** COMMNDxx parsed to define commands issued automatically during initialization.

**Acceptance Criteria:**
- Given `COM='START JES2,PARM=WARM'`, when parsed, then an IplCommand with AfterNip phase is created
- Given `COM='V NET,ACT,ID=VTAMAPPL'`, when parsed, then the VTAM activation command is registered

### SYS-117.8: PARMLIB Framework Tests

**Acceptance Criteria:**
- Given sample PARMLIB members for all supported types, when parsed, then correct data structures are produced
- Given `cargo test -p open-mainframe-parmlib` framework tests, then all pass

---

## SYS-118: System Symbol Substitution

**User Value:** System symbols enable single-source PARMLIB configuration across multiple systems in a sysplex, with symbols like &SYSNAME. resolved at read time.

### SYS-118.1: IEASYMxx Parser — Symbol Definitions

**As a** system administrator, **I want** IEASYMxx parsed to define system symbols.

**Acceptance Criteria:**
- Given `SYSDEF SYMDEF(&SYSNAME.='SYS1')`, when parsed, then the symbol table contains SYSNAME=SYS1
- Given `SYSDEF SYMDEF(&SYSPLEX.='PLEX1')`, when parsed, then SYSPLEX=PLEX1 is in the symbol table
- Given up to 200 symbol definitions, when parsed, then all are stored

### SYS-118.2: Symbol Substitution Engine

**As a** system administrator, **I want** &symbol. references in PARMLIB members replaced with defined values.

**Acceptance Criteria:**
- Given text `DSN=&SYSNAME..LINKLIB` and symbol SYSNAME=SYS1, when substituted, then the result is `DSN=SYS1.LINKLIB`
- Given nested symbols (symbol value contains another symbol reference), when substituted, then recursive resolution occurs (up to depth limit)
- Given an undefined symbol, when encountered, then a warning is issued and the symbol reference is preserved

### SYS-118.3: Static System Symbols

**As a** system administrator, **I want** built-in system symbols (&SYSNAME., &SYSPLEX., &SYSDATE., &SYSTIME., &SYSCLONE.) automatically available.

**Acceptance Criteria:**
- Given no IEASYMxx override for &SYSDATE., when the symbol is referenced, then the current date is substituted
- Given an IEASYMxx override for &SYSNAME., when the symbol is referenced, then the override value is used

### SYS-118.4: Symbol Substitution Tests

**Acceptance Criteria:**
- Given PARMLIB members with various symbol references, when substituted, then correct values are produced
- Given edge cases (undefined symbols, recursive, escaped ampersands), when processed, then correct behavior results

---

## SYS-119: Subsystem Configuration Members

**User Value:** Subsystem-specific PARMLIB members provide z/OS-compatible configuration for TSO, allocation, and other services.

### SYS-119.1: IKJTSOxx Parser — TSO Parameters

**As a** system administrator, **I want** IKJTSOxx parsed for TSO/E configuration.

**Acceptance Criteria:**
- Given `AUTHCMD NAMES(LISTDS LISTCAT RECEIVE TRANSMIT)`, when parsed, then authorized commands list is populated
- Given `AUTHPGM NAMES(IKJEFT01 IRXJCL)`, when parsed, then authorized programs list is populated
- Given `LOGONPROC(IKJACCNT)`, when parsed, then the default logon procedure is set

### SYS-119.2: ALLOCxx Parser — Allocation Defaults

**As a** system administrator, **I want** ALLOCxx parsed for default dataset allocation parameters.

**Acceptance Criteria:**
- Given `UNIT(SYSALLDA) VOLUME(VOL001)`, when parsed, then default unit and volume are set
- Given SMS class defaults, when parsed, then STORCLAS, MGMTCLAS, DATACLAS are set

### SYS-119.3: SMFPRMxx Delegation Interface

**As a** SMF subsystem developer, **I want** the PARMLIB framework to delegate SMFPRMxx parsing to my registered parser.

**Acceptance Criteria:**
- Given SMF crate registers SmfPrmParser, when SMFPRMxx is read during init, then the SMF parser receives the raw member content
- Given the parsed result, when returned to the framework, then it is stored in the initialization result for the SMF subsystem to retrieve

### SYS-119.4: BPXPRMxx Delegation Interface

**As a** USS subsystem developer, **I want** BPXPRMxx parsing delegated to the USS crate.

**Acceptance Criteria:**
- Given USS crate registers BpxPrmParser, when BPXPRMxx is read during init, then the USS parser processes it
- Given BPXPRMxx settings (MAXPROCSYS, MAXFILEPROC, ROOT filesystem), when parsed, then USS configuration is returned

### SYS-119.5: JES2PARMxx Delegation Interface

**As a** JES2 subsystem developer, **I want** JES2 initialization parameters delegated to the JES2 crate.

**Acceptance Criteria:**
- Given JES2 crate registers Jes2ParmParser, when JES2PARMxx is read, then JES2 parameters (SPOOLDEF, PROCLIB, etc.) are parsed by the JES2 crate

### SYS-119.6: ICHPRMxx (RACF) Delegation Interface

**As a** RACF subsystem developer, **I want** RACF parameters delegated to the security crate.

**Acceptance Criteria:**
- Given the security crate registers IchPrmParser, when ICHPRMxx is read, then RACF initialization parameters are parsed by the security crate

### SYS-119.7: Subsystem Member Tests

**Acceptance Criteria:**
- Given all subsystem delegation interfaces, when tested with sample members, then correct data flows from PARMLIB framework to subsystem parsers
- Given `cargo test -p open-mainframe-parmlib` subsystem delegation tests, then all pass

---

## SYS-120: Initialization Sequence & Operator Commands

**User Value:** OpenMainframe performs an orderly startup sequence reading PARMLIB members in the correct z/OS order, and supports runtime reconfiguration via operator commands.

### SYS-120.1: Initialization Orchestrator

**As a** system administrator, **I want** a startup sequence that reads PARMLIB members in the correct order.

**Acceptance Criteria:**
- Given startup is initiated, when the orchestrator runs, then: (1) PARMLIB concatenation is established, (2) IEASYMxx is read, (3) IEASYSxx is read with symbol substitution, (4) subsidiary members are read per IEASYSxx suffixes, (5) COMMNDxx commands are queued
- Given an error in a critical member (IEASYSxx), when encountered, then initialization fails with a clear error message
- Given an error in a non-critical member, when encountered, then a warning is issued and initialization continues with defaults

### SYS-120.2: SET Command — Switch PARMLIB Suffixes

**As an** operator, **I want** `SET xxx=yy` to switch to a different PARMLIB member suffix at runtime.

**Acceptance Criteria:**
- Given `SET SMF=01`, when issued, then SMFPRMxx member SMFPRM01 is read and the SMF subsystem is reconfigured
- Given `SET IKJTSO=01`, when issued, then IKJTSOxx member IKJTSO01 is read and TSO parameters are updated

### SYS-120.3: SETPROG Command — APF/Linklist Updates

**As an** operator, **I want** SETPROG to modify APF, linklist, and LPA at runtime without IPL.

**Acceptance Criteria:**
- Given `SETPROG APF,ADD,DSNAME=MY.NEW.AUTH,SMS`, when issued, then the dataset is added to the APF list
- Given `SETPROG LNKLST,ADD,NAME=LNKLST00,DSNAME=MY.NEW.LIB`, when issued, then the dataset is added to the active linklist

### SYS-120.4: DISPLAY PARMLIB Command

**As an** operator, **I want** `D PARMLIB` to display the current PARMLIB concatenation and active suffixes.

**Acceptance Criteria:**
- Given `D PARMLIB`, when issued, then the PARMLIB concatenation order and active member suffixes are displayed

### SYS-120.5: Initialization & Operator Command Tests

**Acceptance Criteria:**
- Given a complete PARMLIB directory with all supported members, when the full initialization sequence runs, then all members are parsed and subsystems are configured
- Given SET and SETPROG commands, when tested, then runtime reconfiguration works correctly
- Given `cargo test -p open-mainframe-parmlib` integration tests, then all pass

---

## Dependency Graph

```
SYS-118 (Symbols) → SYS-117 (Framework) → SYS-119 (Subsystem Members)
                                         → SYS-120 (Init Sequence)
```

SYS-118 symbol substitution must be available before SYS-117 can parse members with symbol references. SYS-117 framework is prerequisite for SYS-119 and SYS-120.

## FR/NFR Coverage Matrix

| FR | Stories |
|----|---------|
| FR-CFG-001 | SYS-117.1 |
| FR-CFG-002 | SYS-117.3 |
| FR-CFG-003 | SYS-118.1, SYS-118.2, SYS-118.3 |
| FR-CFG-004 | SYS-117.4 |
| FR-CFG-005 | SYS-117.5 |
| FR-CFG-006 | SYS-117.6 |
| FR-CFG-007 | SYS-117.7 |
| FR-CFG-008 | SYS-119.1 |
| FR-CFG-009 | SYS-119.2 |
| FR-CFG-010 | SYS-119.3, SYS-119.4, SYS-119.5, SYS-119.6 |
| FR-CFG-011 | SYS-120.1 |
| FR-CFG-012 | SYS-120.2, SYS-120.3, SYS-120.4 |

| NFR | Stories |
|-----|---------|
| NFR-CFG-001 | SYS-117.8, SYS-118.4, SYS-119.7, SYS-120.5 |
| NFR-CFG-002 | SYS-117.2 |
| NFR-CFG-003 | SYS-117.3 |

**Coverage: 12/12 FRs (100%), 3/3 NFRs (100%)**
