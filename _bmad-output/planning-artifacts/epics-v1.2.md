---
version: 'v1.2'
baseVersion: 'v1.1'
date: '2026-02-13'
status: 'draft'
---

# Epics - zOS-clone v1.2: Enterprise Features

## Epic Overview

| Epic | Name | Stories | Complexity | Phase |
|------|------|---------|------------|-------|
| 21 | SQL Preprocessor | 7 | L | DB2 Core |
| 22 | DB2 Runtime | 8 | L | DB2 Core |
| 23 | Cursor Operations | 6 | M | DB2 Complete |
| 24 | DB2 Utilities | 6 | M | DB2 Complete |
| 25 | CICS Command Processor | 8 | L | CICS Foundation |
| 26 | BMS Maps | 7 | M | CICS Foundation |
| 27 | CICS Terminal Interface | 8 | M | CICS Terminal |
| 28 | Migration Assessment | 7 | M | Assessment |

**Total: 8 Epics, ~57 Stories**

---

## Epic 21: SQL Preprocessor

**Goal:** Extract EXEC SQL statements from COBOL and generate runtime calls.

**Crate:** `zos-db2/preprocess`
**FRs:** FR-v1.2-001 to FR-v1.2-005

### Story 21.1: EXEC SQL Scanner

As a **COBOL developer**,
I want **EXEC SQL blocks identified in my source**,
So that **they can be processed separately from COBOL code**.

**Acceptance Criteria:**

**Given** COBOL source with EXEC SQL ... END-EXEC blocks
**When** preprocessor scans the file
**Then** each EXEC SQL block is extracted with line numbers

**Given** EXEC SQL spanning multiple lines
**When** scanned
**Then** continuation lines are properly joined

**Complexity:** M
**Supports:** FR-v1.2-001

---

### Story 21.2: SQL Statement Parser

As a **COBOL developer**,
I want **SQL statements parsed into structured form**,
So that **they can be validated and transformed**.

**Acceptance Criteria:**

**Given** SELECT statement with INTO clause
**When** parsed
**Then** columns, table, conditions, and host variables identified

**Given** INSERT, UPDATE, DELETE statements
**When** parsed
**Then** table, columns, values, and conditions captured

**Complexity:** L
**Supports:** FR-v1.2-001

---

### Story 21.3: Host Variable Detection

As a **COBOL developer**,
I want **host variables (:VAR-NAME) recognized**,
So that **data can flow between COBOL and SQL**.

**Acceptance Criteria:**

**Given** host variable in SQL (e.g., :WS-CUSTNO)
**When** parsed
**Then** variable name extracted and mapped to COBOL definition

**Given** indicator variable (:VAR :IND-VAR)
**When** parsed
**Then** both variable and indicator captured

**Complexity:** M
**Supports:** FR-v1.2-004

---

### Story 21.4: COBOL Call Generation

As a **compiler**,
I want **EXEC SQL replaced with runtime calls**,
So that **generated COBOL can be compiled normally**.

**Acceptance Criteria:**

**Given** EXEC SQL SELECT ... END-EXEC
**When** preprocessor runs
**Then** replaced with CALL "SQLEXEC" USING ...

**Given** multiple host variables
**When** call generated
**Then** all variables passed in correct order

**Complexity:** M
**Supports:** FR-v1.2-001, FR-v1.2-002

---

### Story 21.5: SQLCA Generation

As a **COBOL developer**,
I want **SQLCA copybook generated**,
So that **I can check SQL return codes**.

**Acceptance Criteria:**

**Given** program uses EXEC SQL
**When** preprocessor runs
**Then** SQLCA copybook added to WORKING-STORAGE

**Given** SQLCODE field
**When** after SQL execution
**Then** contains 0 (success), 100 (not found), or negative (error)

**Complexity:** S
**Supports:** FR-v1.2-003

---

### Story 21.6: DBRM Output

As a **DBA**,
I want **Database Request Module generated**,
So that **SQL can be bound to the database**.

**Acceptance Criteria:**

**Given** COBOL program with SQL
**When** preprocessed
**Then** DBRM file created with SQL statements

**Given** DBRM file
**When** BIND executed
**Then** access paths created in PostgreSQL

**Complexity:** M
**Supports:** FR-v1.2-022

---

### Story 21.7: Preprocessor CLI

As a **developer**,
I want **command-line preprocessor invocation**,
So that **I can preprocess SQL independently**.

**Acceptance Criteria:**

**Given** `zos-clone db2 preprocess program.cbl`
**When** executed
**Then** outputs program.cob (pure COBOL) and program.dbrm

**Given** --output option
**When** specified
**Then** uses custom output paths

**Complexity:** S
**Supports:** FR-v1.2-001

---

## Epic 22: DB2 Runtime

**Goal:** Execute SQL against PostgreSQL at runtime.

**Crate:** `zos-db2/runtime`
**FRs:** FR-v1.2-006 to FR-v1.2-010, FR-v1.2-019 to FR-v1.2-021

### Story 22.1: PostgreSQL Connection

As a **runtime**,
I want **connection to PostgreSQL established**,
So that **SQL can be executed**.

**Acceptance Criteria:**

**Given** connection string in environment or config
**When** first SQL executed
**Then** connection established and pooled

**Given** connection failure
**When** occurs
**Then** SQLCODE set to -911 (connection error)

**Complexity:** M
**Supports:** FR-v1.2-019, FR-v1.2-020

---

### Story 22.2: Connection Pool

As a **operations engineer**,
I want **connection pooling**,
So that **multiple programs can share connections efficiently**.

**Acceptance Criteria:**

**Given** pool configuration (min, max connections)
**When** connections requested
**Then** reused from pool when available

**Given** pool exhausted
**When** new connection requested
**Then** waits or fails based on config

**Complexity:** M
**Supports:** FR-v1.2-020

---

### Story 22.3: SQL Translation

As a **runtime**,
I want **DB2 SQL translated to PostgreSQL**,
So that **standard PostgreSQL can execute it**.

**Acceptance Criteria:**

**Given** DB2-specific syntax (FETCH FIRST n ROWS ONLY)
**When** translated
**Then** PostgreSQL equivalent used (LIMIT n)

**Given** DB2 functions (VALUE, SUBSTR)
**When** translated
**Then** PostgreSQL equivalents used (COALESCE, SUBSTRING)

**Complexity:** L
**Supports:** FR-v1.2-019

---

### Story 22.4: SELECT INTO Execution

As a **COBOL developer**,
I want **SELECT INTO executed**,
So that **single rows are retrieved into host variables**.

**Acceptance Criteria:**

**Given** SELECT ... INTO :HOST-VAR
**When** executed
**Then** result copied to COBOL variable

**Given** no rows found
**When** SELECT INTO executed
**Then** SQLCODE = 100

**Given** multiple rows
**When** SELECT INTO executed
**Then** SQLCODE = -811 (too many rows)

**Complexity:** M
**Supports:** FR-v1.2-006

---

### Story 22.5: INSERT Execution

As a **COBOL developer**,
I want **INSERT executed**,
So that **new rows are added**.

**Acceptance Criteria:**

**Given** INSERT INTO table VALUES (:V1, :V2)
**When** executed
**Then** row added and SQLCODE = 0

**Given** duplicate key
**When** INSERT executed
**Then** SQLCODE = -803

**Complexity:** M
**Supports:** FR-v1.2-007

---

### Story 22.6: UPDATE Execution

As a **COBOL developer**,
I want **UPDATE executed**,
So that **existing rows are modified**.

**Acceptance Criteria:**

**Given** UPDATE table SET col = :VAR WHERE key = :KEY
**When** executed
**Then** rows updated and SQLERRD(3) contains count

**Given** no matching rows
**When** UPDATE executed
**Then** SQLCODE = 100

**Complexity:** M
**Supports:** FR-v1.2-008

---

### Story 22.7: DELETE Execution

As a **COBOL developer**,
I want **DELETE executed**,
So that **rows are removed**.

**Acceptance Criteria:**

**Given** DELETE FROM table WHERE key = :KEY
**When** executed
**Then** rows deleted and SQLERRD(3) contains count

**Complexity:** M
**Supports:** FR-v1.2-009

---

### Story 22.8: Data Type Mapping

As a **runtime**,
I want **DB2 data types mapped to PostgreSQL and COBOL**,
So that **data converts correctly**.

**Acceptance Criteria:**

**Given** DB2 DECIMAL(10,2)
**When** mapped
**Then** PostgreSQL NUMERIC(10,2), COBOL PIC 9(8)V99

**Given** DB2 VARCHAR(100)
**When** mapped
**Then** PostgreSQL VARCHAR(100), COBOL PIC X(100)

**Complexity:** M
**Supports:** FR-v1.2-019

---

## Epic 23: Cursor Operations

**Goal:** Implement multi-row result set processing.

**Crate:** `zos-db2/runtime`
**FRs:** FR-v1.2-011 to FR-v1.2-015

### Story 23.1: DECLARE CURSOR

As a **COBOL developer**,
I want **cursors declared**,
So that **I can process multiple rows**.

**Acceptance Criteria:**

**Given** DECLARE cursor-name CURSOR FOR SELECT ...
**When** parsed
**Then** cursor definition stored for later use

**Complexity:** M
**Supports:** FR-v1.2-011

---

### Story 23.2: OPEN CURSOR

As a **COBOL developer**,
I want **cursors opened**,
So that **query executes and results are ready**.

**Acceptance Criteria:**

**Given** OPEN cursor-name
**When** executed
**Then** query runs and cursor positioned before first row

**Given** cursor already open
**When** OPEN attempted
**Then** SQLCODE = -502

**Complexity:** M
**Supports:** FR-v1.2-012

---

### Story 23.3: FETCH CURSOR

As a **COBOL developer**,
I want **rows fetched from cursor**,
So that **I can process each row**.

**Acceptance Criteria:**

**Given** FETCH cursor-name INTO :V1, :V2
**When** executed
**Then** next row copied to host variables

**Given** no more rows
**When** FETCH executed
**Then** SQLCODE = 100

**Complexity:** M
**Supports:** FR-v1.2-013, FR-v1.2-010

---

### Story 23.4: CLOSE CURSOR

As a **COBOL developer**,
I want **cursors closed**,
So that **resources are released**.

**Acceptance Criteria:**

**Given** CLOSE cursor-name
**When** executed
**Then** cursor resources freed

**Given** cursor not open
**When** CLOSE attempted
**Then** SQLCODE = -501

**Complexity:** S
**Supports:** FR-v1.2-014

---

### Story 23.5: WITH HOLD Cursors

As a **COBOL developer**,
I want **WITH HOLD cursors**,
So that **cursors survive COMMIT**.

**Acceptance Criteria:**

**Given** DECLARE cursor WITH HOLD
**When** COMMIT executed
**Then** cursor remains open and positioned

**Complexity:** M
**Supports:** FR-v1.2-015

---

### Story 23.6: Positioned UPDATE/DELETE

As a **COBOL developer**,
I want **UPDATE/DELETE WHERE CURRENT OF cursor**,
So that **I can modify the fetched row**.

**Acceptance Criteria:**

**Given** UPDATE table SET ... WHERE CURRENT OF cursor
**When** executed
**Then** last fetched row updated

**Complexity:** M
**Supports:** FR-v1.2-008, FR-v1.2-009

---

## Epic 24: DB2 Utilities

**Goal:** Support DB2 utilities and JCL integration.

**Crate:** `zos-db2/utilities`, `zos-jcl`
**FRs:** FR-v1.2-016 to FR-v1.2-018, FR-v1.2-021, FR-v1.2-022

### Story 24.1: COMMIT/ROLLBACK

As a **COBOL developer**,
I want **transaction control**,
So that **I can commit or rollback changes**.

**Acceptance Criteria:**

**Given** EXEC SQL COMMIT END-EXEC
**When** executed
**Then** all changes made permanent

**Given** EXEC SQL ROLLBACK END-EXEC
**When** executed
**Then** all changes since last COMMIT undone

**Complexity:** M
**Supports:** FR-v1.2-016, FR-v1.2-017

---

### Story 24.2: Implicit COMMIT

As a **DBA**,
I want **configurable commit behavior**,
So that **programs commit appropriately**.

**Acceptance Criteria:**

**Given** program ends normally
**When** implicit commit enabled
**Then** COMMIT executed automatically

**Given** program abends
**When** occurs
**Then** ROLLBACK executed automatically

**Complexity:** S
**Supports:** FR-v1.2-018

---

### Story 24.3: DCLGEN Utility

As a **COBOL developer**,
I want **DCLGEN to create copybooks**,
So that **my COBOL matches the database schema**.

**Acceptance Criteria:**

**Given** `zos-clone db2 dclgen TABLENAME`
**When** executed
**Then** COBOL copybook generated matching table columns

**Given** PostgreSQL table with various types
**When** DCLGEN runs
**Then** appropriate COBOL PIC clauses generated

**Complexity:** M
**Supports:** FR-v1.2-021

---

### Story 24.4: BIND Utility

As a **DBA**,
I want **BIND to create access paths**,
So that **SQL is optimized for execution**.

**Acceptance Criteria:**

**Given** `zos-clone db2 bind program.dbrm`
**When** executed
**Then** prepared statements created in PostgreSQL

**Complexity:** M
**Supports:** FR-v1.2-022

---

### Story 24.5: JCL DB2 Step Recognition

As a **JCL interpreter**,
I want **DB2 utility steps recognized**,
So that **batch DB2 jobs run correctly**.

**Acceptance Criteria:**

**Given** EXEC PGM=IKJEFT01 with SYSTSIN containing DSN
**When** JCL executed
**Then** DB2 commands processed

**Given** RUN PROGRAM(pgmname)
**When** in DSN session
**Then** COBOL program with SQL executed

**Complexity:** M
**Supports:** FR-v1.2-022

---

### Story 24.6: DB2 Error Handling

As a **COBOL developer**,
I want **detailed SQL errors**,
So that **I can diagnose problems**.

**Acceptance Criteria:**

**Given** SQL error occurs
**When** SQLCA examined
**Then** SQLERRMC contains error message

**Given** WHENEVER SQLERROR
**When** error occurs
**Then** specified paragraph executed

**Complexity:** M
**Supports:** FR-v1.2-003

---

## Epic 25: CICS Command Processor

**Goal:** Process EXEC CICS commands at runtime.

**Crate:** `zos-cics`
**FRs:** FR-v1.2-023 to FR-v1.2-039

### Story 25.1: EXEC CICS Scanner

As a **COBOL developer**,
I want **EXEC CICS blocks identified**,
So that **they can be processed**.

**Acceptance Criteria:**

**Given** COBOL with EXEC CICS ... END-EXEC
**When** preprocessed
**Then** commands extracted and runtime calls generated

**Complexity:** M
**Supports:** FR-v1.2-023

---

### Story 25.2: Command Parser

As a **preprocessor**,
I want **CICS commands parsed**,
So that **options are identified**.

**Acceptance Criteria:**

**Given** EXEC CICS LINK PROGRAM('SUBPROG') COMMAREA(WS-DATA)
**When** parsed
**Then** command=LINK, program=SUBPROG, commarea=WS-DATA

**Complexity:** M
**Supports:** FR-v1.2-023 to FR-v1.2-039

---

### Story 25.3: LINK Command

As a **CICS developer**,
I want **EXEC CICS LINK**,
So that **I can call subprograms with return**.

**Acceptance Criteria:**

**Given** LINK PROGRAM('SUB') COMMAREA(DATA)
**When** executed
**Then** SUB runs, control returns to caller

**Given** program not found
**When** LINK executed
**Then** PGMIDERR condition raised

**Complexity:** M
**Supports:** FR-v1.2-023

---

### Story 25.4: XCTL Command

As a **CICS developer**,
I want **EXEC CICS XCTL**,
So that **I can transfer control without return**.

**Acceptance Criteria:**

**Given** XCTL PROGRAM('NEXT') COMMAREA(DATA)
**When** executed
**Then** NEXT runs, current program ends

**Complexity:** M
**Supports:** FR-v1.2-024

---

### Story 25.5: RETURN Command

As a **CICS developer**,
I want **EXEC CICS RETURN**,
So that **I can end the program or transaction**.

**Acceptance Criteria:**

**Given** RETURN without options
**When** executed
**Then** program ends, control to caller or CICS

**Given** RETURN TRANSID('NEXT')
**When** executed
**Then** next user input runs NEXT transaction

**Complexity:** M
**Supports:** FR-v1.2-025, FR-v1.2-026

---

### Story 25.6: File READ Command

As a **CICS developer**,
I want **EXEC CICS READ**,
So that **I can retrieve VSAM records**.

**Acceptance Criteria:**

**Given** READ FILE('CUSTFILE') INTO(DATA) RIDFLD(KEY)
**When** executed
**Then** record read into DATA

**Given** key not found
**When** READ executed
**Then** NOTFND condition raised

**Complexity:** M
**Supports:** FR-v1.2-031

---

### Story 25.7: File WRITE/REWRITE/DELETE

As a **CICS developer**,
I want **file modification commands**,
So that **I can update VSAM files**.

**Acceptance Criteria:**

**Given** WRITE FILE('F') FROM(DATA) RIDFLD(KEY)
**When** executed
**Then** new record added

**Given** REWRITE FILE('F') FROM(DATA)
**When** executed after READ UPDATE
**Then** current record modified

**Given** DELETE FILE('F')
**When** executed after READ UPDATE
**Then** current record removed

**Complexity:** M
**Supports:** FR-v1.2-032 to FR-v1.2-034

---

### Story 25.8: HANDLE CONDITION

As a **CICS developer**,
I want **exception handling**,
So that **I can handle errors gracefully**.

**Acceptance Criteria:**

**Given** HANDLE CONDITION NOTFND(NOT-FOUND-PARA)
**When** NOTFND occurs
**Then** NOT-FOUND-PARA executed

**Given** RESP(WS-RESP) option
**When** command executed
**Then** response code in WS-RESP, no exception raised

**Complexity:** M
**Supports:** FR-v1.2-037 to FR-v1.2-039

---

## Epic 26: BMS Maps

**Goal:** Compile BMS map definitions and generate copybooks.

**Crate:** `zos-bms`
**FRs:** FR-v1.2-027 to FR-v1.2-030

### Story 26.1: BMS Macro Parser

As a **BMS compiler**,
I want **DFHMSD/DFHMDI/DFHMDF parsed**,
So that **screen layouts are understood**.

**Acceptance Criteria:**

**Given** BMS macro source
**When** parsed
**Then** mapset, maps, and fields extracted

**Complexity:** M
**Supports:** FR-v1.2-029

---

### Story 26.2: COBOL Copybook Generation

As a **CICS developer**,
I want **copybooks generated from BMS**,
So that **my COBOL can reference map fields**.

**Acceptance Criteria:**

**Given** BMS map with fields
**When** compiled
**Then** input and output copybooks created

**Given** field with ATTRB=NUM
**When** copybook generated
**Then** appropriate PIC clause used

**Complexity:** M
**Supports:** FR-v1.2-029

---

### Story 26.3: Screen Layout Storage

As a **BMS runtime**,
I want **screen layouts stored**,
So that **SEND/RECEIVE can use them**.

**Acceptance Criteria:**

**Given** compiled mapset
**When** loaded at runtime
**Then** field positions and attributes available

**Complexity:** M
**Supports:** FR-v1.2-029, FR-v1.2-030

---

### Story 26.4: SEND MAP Command

As a **CICS developer**,
I want **EXEC CICS SEND MAP**,
So that **screens display to user**.

**Acceptance Criteria:**

**Given** SEND MAP('MAP1') MAPSET('SET1') FROM(DATA)
**When** executed
**Then** screen rendered with data values

**Given** ERASE option
**When** specified
**Then** screen cleared before display

**Complexity:** M
**Supports:** FR-v1.2-027

---

### Story 26.5: RECEIVE MAP Command

As a **CICS developer**,
I want **EXEC CICS RECEIVE MAP**,
So that **user input is captured**.

**Acceptance Criteria:**

**Given** RECEIVE MAP('MAP1') INTO(DATA)
**When** user submits
**Then** input fields copied to DATA

**Given** modified data tag (MDT)
**When** field changed
**Then** only modified fields transmitted

**Complexity:** M
**Supports:** FR-v1.2-028

---

### Story 26.6: Field Attributes

As a **BMS compiler**,
I want **field attributes supported**,
So that **display characteristics work**.

**Acceptance Criteria:**

**Given** ATTRB=(UNPROT,BRT,IC)
**When** rendered
**Then** field is unprotected, bright, with cursor

**Given** ATTRB=ASKIP
**When** rendered
**Then** field is auto-skip (protected)

**Complexity:** M
**Supports:** FR-v1.2-029

---

### Story 26.7: BMS CLI

As a **developer**,
I want **BMS compilation command**,
So that **I can compile maps independently**.

**Acceptance Criteria:**

**Given** `zos-clone bms compile mapset.bms`
**When** executed
**Then** copybooks and layout files created

**Complexity:** S
**Supports:** FR-v1.2-029

---

## Epic 27: CICS Terminal Interface

**Goal:** Provide terminal emulation for BMS screens.

**Crate:** `zos-bms/render`, `zos-cics`
**FRs:** FR-v1.2-040 to FR-v1.2-042

### Story 27.1: Console Mode Renderer

As a **developer**,
I want **screens in terminal**,
So that **I can test CICS programs locally**.

**Acceptance Criteria:**

**Given** SEND MAP executed
**When** in console mode
**Then** ANSI-formatted screen displayed

**Given** field attributes
**When** rendered
**Then** colors and cursor position correct

**Complexity:** M
**Supports:** FR-v1.2-040

---

### Story 27.2: Console Input Handler

As a **developer**,
I want **keyboard input captured**,
So that **RECEIVE MAP works in console**.

**Acceptance Criteria:**

**Given** console displaying map
**When** user types and presses Enter
**Then** input available for RECEIVE MAP

**Given** function keys (PF1-24)
**When** pressed
**Then** EIBAID set appropriately

**Complexity:** M
**Supports:** FR-v1.2-040

---

### Story 27.3: HTML Renderer

As a **developer**,
I want **screens as HTML**,
So that **web browsers can access CICS**.

**Acceptance Criteria:**

**Given** SEND MAP executed
**When** in web mode
**Then** HTML form generated with fields

**Given** field attributes
**When** rendered
**Then** CSS classes applied appropriately

**Complexity:** M
**Supports:** FR-v1.2-041

---

### Story 27.4: Web Server

As a **developer**,
I want **embedded HTTP server**,
So that **browsers can connect**.

**Acceptance Criteria:**

**Given** `zos-clone cics web --port 3270`
**When** browser connects
**Then** transaction menu displayed

**Given** transaction submitted
**When** SEND MAP executed
**Then** HTML response sent

**Complexity:** M
**Supports:** FR-v1.2-041

---

### Story 27.5: REST API Mode

As a **integration developer**,
I want **transactions as REST endpoints**,
So that **modern apps can call CICS**.

**Acceptance Criteria:**

**Given** `POST /api/txn/{transid}` with JSON body
**When** received
**Then** COMMAREA populated, transaction executed

**Given** transaction completes
**When** response sent
**Then** output COMMAREA as JSON

**Complexity:** M
**Supports:** FR-v1.2-042

---

### Story 27.6: EIB Management

As a **CICS runtime**,
I want **EIB populated correctly**,
So that **programs can check state**.

**Acceptance Criteria:**

**Given** transaction starts
**When** EIB examined
**Then** EIBTRNID, EIBTIME, EIBDATE set correctly

**Given** SEND MAP executed
**When** RECEIVE MAP follows
**Then** EIBCPOSN shows cursor position

**Complexity:** M
**Supports:** FR-v1.2-039

---

### Story 27.7: GETMAIN/FREEMAIN

As a **CICS developer**,
I want **dynamic storage allocation**,
So that **I can request memory at runtime**.

**Acceptance Criteria:**

**Given** GETMAIN SET(PTR) LENGTH(1000)
**When** executed
**Then** 1000 bytes allocated, PTR set to address

**Given** FREEMAIN DATA(PTR)
**When** executed
**Then** memory released

**Complexity:** M
**Supports:** FR-v1.2-035, FR-v1.2-036

---

### Story 27.8: CICS CLI

As a **developer**,
I want **CICS management commands**,
So that **I can control the environment**.

**Acceptance Criteria:**

**Given** `zos-clone cics start`
**When** executed
**Then** CICS runtime initialized

**Given** `zos-clone cics run TRAN`
**When** executed
**Then** transaction executed in console mode

**Complexity:** M
**Supports:** FR-v1.2-040 to FR-v1.2-042

---

## Epic 28: Migration Assessment

**Goal:** Analyze mainframe codebases for compatibility.

**Crate:** `zos-migrate`
**FRs:** FR-v1.2-051 to FR-v1.2-057

### Story 28.1: Codebase Scanner

As a **migration analyst**,
I want **all source files discovered**,
So that **I know the scope of migration**.

**Acceptance Criteria:**

**Given** `zos-clone migrate scan /path/to/code`
**When** executed
**Then** all COBOL, JCL, copybook files listed

**Given** various encodings
**When** scanned
**Then** EBCDIC and ASCII detected

**Complexity:** M
**Supports:** FR-v1.2-051

---

### Story 28.2: Feature Detector

As a **migration analyst**,
I want **language features identified**,
So that **I know what's used in the code**.

**Acceptance Criteria:**

**Given** COBOL source
**When** analyzed
**Then** COBOL version, divisions, features reported

**Given** EXEC SQL or EXEC CICS
**When** detected
**Then** subsystem usage flagged

**Complexity:** M
**Supports:** FR-v1.2-052

---

### Story 28.3: Compatibility Analyzer

As a **migration analyst**,
I want **unsupported features flagged**,
So that **I know what needs remediation**.

**Acceptance Criteria:**

**Given** unsupported COBOL construct
**When** analyzed
**Then** flagged with severity and suggestion

**Given** partially supported feature
**When** analyzed
**Then** limitations documented

**Complexity:** M
**Supports:** FR-v1.2-052

---

### Story 28.4: Dependency Mapper

As a **migration analyst**,
I want **program dependencies shown**,
So that **I can plan migration order**.

**Acceptance Criteria:**

**Given** COPY statements
**When** analyzed
**Then** copybook dependencies mapped

**Given** CALL statements
**When** analyzed
**Then** subprogram dependencies mapped

**Complexity:** M
**Supports:** FR-v1.2-053

---

### Story 28.5: Complexity Scoring

As a **migration analyst**,
I want **complexity scores**,
So that **I can prioritize effort**.

**Acceptance Criteria:**

**Given** program analyzed
**When** scored
**Then** complexity (low/medium/high) based on features

**Given** codebase
**When** summarized
**Then** total lines, programs, complexity distribution

**Complexity:** M
**Supports:** FR-v1.2-055

---

### Story 28.6: Remediation Suggestions

As a **migration analyst**,
I want **fix suggestions**,
So that **developers know how to address issues**.

**Acceptance Criteria:**

**Given** flagged incompatibility
**When** suggestion generated
**Then** specific remediation steps provided

**Given** common patterns
**When** detected
**Then** automated fix suggestions offered

**Complexity:** M
**Supports:** FR-v1.2-056, FR-v1.2-057

---

### Story 28.7: Report Generator

As a **migration analyst**,
I want **formatted reports**,
So that **stakeholders can review**.

**Acceptance Criteria:**

**Given** `zos-clone migrate report --format html`
**When** executed
**Then** HTML report with charts and details

**Given** `--format json`
**When** specified
**Then** machine-readable output

**Complexity:** M
**Supports:** FR-v1.2-054

---

## Requirements Traceability

### FR to Story Mapping

| Requirement | Story | Status |
|-------------|-------|--------|
| FR-v1.2-001 | 21.1, 21.4 | Planned |
| FR-v1.2-002 | 21.4 | Planned |
| FR-v1.2-003 | 21.5, 24.6 | Planned |
| FR-v1.2-004 | 21.3 | Planned |
| FR-v1.2-005 | 23.1 | Planned |
| FR-v1.2-006 | 22.4 | Planned |
| FR-v1.2-007 | 22.5 | Planned |
| FR-v1.2-008 | 22.6, 23.6 | Planned |
| FR-v1.2-009 | 22.7, 23.6 | Planned |
| FR-v1.2-010 | 23.3 | Planned |
| FR-v1.2-011 | 23.1 | Planned |
| FR-v1.2-012 | 23.2 | Planned |
| FR-v1.2-013 | 23.3 | Planned |
| FR-v1.2-014 | 23.4 | Planned |
| FR-v1.2-015 | 23.5 | Planned |
| FR-v1.2-016 | 24.1 | Planned |
| FR-v1.2-017 | 24.1 | Planned |
| FR-v1.2-018 | 24.2 | Planned |
| FR-v1.2-019 | 22.3 | Planned |
| FR-v1.2-020 | 22.1, 22.2 | Planned |
| FR-v1.2-021 | 24.3 | Planned |
| FR-v1.2-022 | 21.6, 24.4 | Planned |
| FR-v1.2-023 | 25.1, 25.3 | Planned |
| FR-v1.2-024 | 25.4 | Planned |
| FR-v1.2-025 | 25.5 | Planned |
| FR-v1.2-026 | 25.5 | Planned |
| FR-v1.2-027 | 26.4 | Planned |
| FR-v1.2-028 | 26.5 | Planned |
| FR-v1.2-029 | 26.1, 26.2 | Planned |
| FR-v1.2-030 | 26.3 | Planned |
| FR-v1.2-031 | 25.6 | Planned |
| FR-v1.2-032 | 25.7 | Planned |
| FR-v1.2-033 | 25.7 | Planned |
| FR-v1.2-034 | 25.7 | Planned |
| FR-v1.2-035 | 27.7 | Planned |
| FR-v1.2-036 | 27.7 | Planned |
| FR-v1.2-037 | 25.8 | Planned |
| FR-v1.2-038 | 25.8 | Planned |
| FR-v1.2-039 | 25.8, 27.6 | Planned |
| FR-v1.2-040 | 27.1, 27.2 | Planned |
| FR-v1.2-041 | 27.3, 27.4 | Planned |
| FR-v1.2-042 | 27.5 | Planned |
| FR-v1.2-051 | 28.1 | Planned |
| FR-v1.2-052 | 28.2, 28.3 | Planned |
| FR-v1.2-053 | 28.4 | Planned |
| FR-v1.2-054 | 28.7 | Planned |
| FR-v1.2-055 | 28.5 | Planned |
| FR-v1.2-056 | 28.6 | Planned |
| FR-v1.2-057 | 28.6 | Planned |
