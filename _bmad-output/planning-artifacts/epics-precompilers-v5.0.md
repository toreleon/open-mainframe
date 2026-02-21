---
version: 'v5.0'
planningGroup: 'PG-18'
technology: 'COBOL Precompilers (DB2 + CICS)'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-precompilers-v5.0.md'
  - 'architecture-precompilers-v5.0.md'
totalEpics: 2
totalStories: 14
frCoverage: '10/10 (100%)'
nfrCoverage: '3/3 (100%)'
---

# Epics & Stories: COBOL Precompilers (DB2 + CICS)

## Epic Overview

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| SYS-112 | DB2 COBOL Precompiler | L | 8 | C |
| SYS-113 | CICS COBOL Precompiler | L | 6 | C |

---

## SYS-112: DB2 COBOL Precompiler

**User Value:** COBOL programs with embedded EXEC SQL statements are transformed into compilable COBOL with DB2 runtime calls, enabling database-driven applications.

### SYS-112.1: EXEC SQL Parser

**As a** COBOL developer, **I want** EXEC SQL ... END-EXEC blocks parsed from COBOL source, **so that** embedded SQL is recognized.

**Acceptance Criteria:**
- Given `EXEC SQL SELECT ENAME INTO :WS-NAME FROM EMP WHERE EMPNO = :WS-ID END-EXEC`, when parsed, then the SQL statement and host variables are extracted
- Given nested EXEC SQL blocks and COBOL comments, when parsed, then only valid EXEC SQL blocks are processed

### SYS-112.2: Host Variable Mapping

**As a** COBOL developer, **I want** COBOL data names mapped to SQL host variables, **so that** data flows between COBOL and DB2.

**Acceptance Criteria:**
- Given `01 WS-EMPLOYEE-ID PIC 9(6)`, when referenced as `:WS-EMPLOYEE-ID` in SQL, then it maps to SQL INTEGER
- Given a group item with OCCURS, when referenced, then array host variable binding is generated
- Given `:WS-NAME:WS-NAME-IND` (with indicator), when parsed, then the null indicator variable is mapped

### SYS-112.3: Static SQL Transformation

**As a** COBOL developer, **I want** static SQL (SELECT/INSERT/UPDATE/DELETE) transformed to CALL DSNHLI.

**Acceptance Criteria:**
- Given `EXEC SQL SELECT ... END-EXEC`, when transformed, then it becomes `CALL 'DSNHLI' USING SQLCA, stmt-id, host-vars...`
- Given DECLARE CURSOR / OPEN / FETCH / CLOSE, when transformed, then each generates the appropriate CALL with cursor reference

### SYS-112.4: Dynamic SQL Support

**As a** COBOL developer, **I want** PREPARE/DESCRIBE/EXECUTE for dynamic SQL.

**Acceptance Criteria:**
- Given `EXEC SQL PREPARE STMT1 FROM :WS-SQL-TEXT END-EXEC`, when transformed, then a CALL for dynamic prepare is generated
- Given `EXEC SQL EXECUTE STMT1 USING :WS-PARAM1, :WS-PARAM2 END-EXEC`, when transformed, then the execute CALL passes the parameters

### SYS-112.5: SQLCA Generation

**As a** COBOL developer, **I want** SQLCA (SQL Communication Area) automatically included, **so that** SQLCODE/SQLSTATE are available.

**Acceptance Criteria:**
- Given EXEC SQL INCLUDE SQLCA END-EXEC, when processed, then the SQLCA copybook (SQLCODE, SQLERRM, SQLWARN, SQLSTATE) is inserted
- Given EXEC SQL WHENEVER SQLERROR GO TO :ERR-HANDLER END-EXEC, when processed, then an IF SQLCODE < 0 GO TO is generated after each SQL CALL

### SYS-112.6: DBRM Generation

**As a** DBA, **I want** a DBRM generated for DB2 BIND, **so that** SQL access plans can be created.

**Acceptance Criteria:**
- Given a precompiled program with 5 SQL statements, when the DBRM is generated, then it contains all 5 statements with parameter markers and host variable metadata
- Given the DBRM, when stored in a PDS library, then DB2 BIND PACKAGE can process it

### SYS-112.7: Line Number Preservation

**As a** COBOL developer, **I want** generated output to preserve source line correspondence, **so that** compile errors map back to original source.

**Acceptance Criteria:**
- Given EXEC SQL on line 150 of the original source, when transformed, then the generated CALL is marked as originating from line 150

### SYS-112.8: DB2 Precompiler Tests

**Acceptance Criteria:**
- Given COBOL source with various EXEC SQL patterns (static, dynamic, cursors), when precompiled, then correct CALL statements and DBRM are produced
- Given `cargo test -p open-mainframe-cobol` DB2 precompiler tests, then all pass

---

## SYS-113: CICS COBOL Precompiler

**User Value:** COBOL programs with embedded EXEC CICS statements are transformed into compilable COBOL with CICS runtime calls, enabling online transaction programs.

### SYS-113.1: EXEC CICS Parser

**As a** COBOL developer, **I want** EXEC CICS ... END-EXEC blocks parsed from COBOL source.

**Acceptance Criteria:**
- Given `EXEC CICS SEND MAP('MENU01') MAPSET('MENUSET') ERASE END-EXEC`, when parsed, then command=SEND MAP, options MAP/MAPSET/ERASE are extracted
- Given all 30+ supported EXEC CICS commands, when parsed, then each is correctly recognized

### SYS-113.2: CICS Command Transformation

**As a** COBOL developer, **I want** EXEC CICS transformed to CALL DFHEI1 with encoded command parameters.

**Acceptance Criteria:**
- Given EXEC CICS SEND MAP, when transformed, then it becomes `CALL 'DFHEI1' USING DFHEIBLK, command-code, option-list...`
- Given EXEC CICS READ FILE('EMPFILE') INTO(WS-RECORD) RIDFLD(WS-KEY), when transformed, then the CALL passes file name, buffer address, and key

### SYS-113.3: DFHEIBLK and DFHCOMMAREA Generation

**As a** COBOL developer, **I want** DFHEIBLK (EIB block) and DFHCOMMAREA copybooks auto-generated.

**Acceptance Criteria:**
- Given a CICS program, when precompiled, then DFHEIBLK is inserted in LINKAGE SECTION with EIBTIME, EIBDATE, EIBTRNID, EIBRSRCE, etc.
- Given PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA, when generated, then the program can receive the COMMAREA from CICS

### SYS-113.4: BMS Map Support

**As a** COBOL developer, **I want** SEND MAP/RECEIVE MAP commands to reference symbolic BMS maps.

**Acceptance Criteria:**
- Given `EXEC CICS RECEIVE MAP('MENU01') MAPSET('MENUSET') INTO(WS-MENU-MAP) END-EXEC`, when transformed, then the CALL references the symbolic map copybook for data transfer
- Given a symbolic map generated from BMS macros, when referenced by RECEIVE MAP, then field data is mapped to the COBOL group item

### SYS-113.5: Integrated DB2+CICS Precompilation

**As a** COBOL developer, **I want** both EXEC SQL and EXEC CICS in the same program, **so that** CICS transactions can access DB2.

**Acceptance Criteria:**
- Given a program with both EXEC SQL SELECT and EXEC CICS SEND MAP, when precompiled in integrated mode, then both are correctly transformed in a single pass
- Given CICS precompilation runs first, when followed by DB2 precompilation, then the combined output is valid COBOL

### SYS-113.6: CICS Precompiler Tests

**Acceptance Criteria:**
- Given COBOL source with various EXEC CICS patterns, when precompiled, then correct CALL statements are produced
- Given `cargo test -p open-mainframe-cobol` CICS precompiler tests, then all pass

---

## FR/NFR Coverage Matrix

| FR | Stories |
|----|---------|
| FR-PRE-001 | SYS-112.1, SYS-112.3 |
| FR-PRE-002 | SYS-112.6 |
| FR-PRE-003 | SYS-112.3 |
| FR-PRE-004 | SYS-112.4 |
| FR-PRE-005 | SYS-112.2 |
| FR-PRE-006 | SYS-112.5 |
| FR-PRE-007 | SYS-113.1, SYS-113.2 |
| FR-PRE-008 | SYS-113.2 |
| FR-PRE-009 | SYS-113.3 |
| FR-PRE-010 | SYS-113.5 |

| NFR | Stories |
|-----|---------|
| NFR-PRE-001 | SYS-112.8, SYS-113.6 |
| NFR-PRE-002 | SYS-112.7 |
| NFR-PRE-003 | SYS-112.6 |

**Coverage: 10/10 FRs (100%), 3/3 NFRs (100%)**
