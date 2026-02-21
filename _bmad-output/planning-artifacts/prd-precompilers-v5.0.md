---
version: 'v5.0'
planningGroup: 'PG-18'
technology: 'COBOL Precompilers (DB2 + CICS)'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'zos-complete-inventory.md (AREA-12)'
  - 'gap-analysis/batch-22-priority-matrix.md'
epicIds: []
sysEpicIds: ['SYS-112', 'SYS-113']
totalFRs: 10
totalNFRs: 3
---

# PRD: COBOL Precompilers (DB2 + CICS)

## 1. Problem Statement

OpenMainframe has a COBOL compiler and separate DB2/CICS subsystems, but no precompiler/coprocessor step that transforms EXEC SQL and EXEC CICS statements into standard COBOL CALLs. Without precompilers, COBOL programs cannot embed SQL or CICS commands — the primary way mainframe applications access databases and transaction services.

## 2. User Personas

- **COBOL Application Developer** — writes COBOL programs with embedded EXEC SQL and EXEC CICS statements
- **DBA** — manages DB2 BIND operations and DBRM libraries
- **CICS System Programmer** — manages CICS program definitions and BMS maps

## 3. Functional Requirements

| ID | Requirement |
|----|-------------|
| FR-PRE-001 | Parse EXEC SQL ... END-EXEC blocks in COBOL source and replace with CALL statements to DSNHLI (DB2 interface module) |
| FR-PRE-002 | Generate DBRM (Database Request Module) containing extracted SQL statements with host variable references |
| FR-PRE-003 | Support static SQL: SELECT, INSERT, UPDATE, DELETE, DECLARE CURSOR, OPEN, FETCH, CLOSE |
| FR-PRE-004 | Support dynamic SQL: PREPARE, DESCRIBE, EXECUTE, EXECUTE IMMEDIATE |
| FR-PRE-005 | Map COBOL host variables (01-level, group items, OCCURS) to SQL parameter markers |
| FR-PRE-006 | Generate SQLCA (SQL Communication Area) and SQLCODE/SQLSTATE processing |
| FR-PRE-007 | Parse EXEC CICS ... END-EXEC blocks in COBOL source and replace with CALL statements to DFHEI1 (CICS stub) |
| FR-PRE-008 | Support EXEC CICS commands: SEND, RECEIVE, SEND MAP, RECEIVE MAP, READ, WRITE, REWRITE, DELETE, STARTBR, READNEXT, READPREV, ENDBR, LINK, XCTL, RETURN, GETMAIN, FREEMAIN, ASKTIME, FORMATTIME, ENQ, DEQ, START, RETRIEVE, WRITEQ TS, READQ TS, WRITEQ TD, READQ TD |
| FR-PRE-009 | Generate DFHEIBLK (EIB) and DFHCOMMAREA copybooks for CICS programs |
| FR-PRE-010 | Support integrated CICS-DB2 precompilation (both EXEC SQL and EXEC CICS in same source) |

## 4. Non-Functional Requirements

| ID | Requirement |
|----|-------------|
| NFR-PRE-001 | Precompilation of a 10,000-line COBOL program completes in < 5 seconds |
| NFR-PRE-002 | Generated COBOL output preserves line number correspondence for debugging |
| NFR-PRE-003 | DBRM binary format compatible with DB2 BIND utility |

## 5. Scope

**MVP:** DB2 static SQL precompiler, CICS basic command precompiler, DBRM generation
**Full:** Dynamic SQL, all CICS commands, integrated DB2+CICS precompilation
**Deferred:** DB2 coprocessor mode (single-pass), INCLUDE SQLCA/SQLDA from syslib

## 6. Dependencies

- `open-mainframe-cobol` — COBOL source parsing, AST access
- `open-mainframe-db2` — DB2 SQL execution, BIND support
- `open-mainframe-cics` — CICS command execution, EIB
- `open-mainframe-dataset` — DBRM library (PDS) storage
