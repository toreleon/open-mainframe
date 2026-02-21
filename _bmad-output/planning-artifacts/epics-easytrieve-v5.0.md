---
version: 'v5.0'
planningGroup: 'PG-26'
technology: 'Easytrieve'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-easytrieve-v5.0.md'
  - 'architecture-easytrieve-v5.0.md'
totalEpics: 7
totalStories: 33
frCoverage: '10/10 (100%)'
nfrCoverage: '3/3 (100%)'
---

# Epics & Stories: Easytrieve

## Epic Overview

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| EZ-100 | Lexer & Parser | M | 5 | E |
| EZ-101 | Interpreter Core | M | 5 | E |
| EZ-102 | File Processing | M | 5 | E |
| EZ-103 | Report Generator | L | 6 | E |
| EZ-104 | SQL/Database Integration | M | 4 | E |
| EZ-105 | SORT & Utilities | S | 4 | E |
| EZ-106 | Macros, External Calls & Tests | S | 4 | E |

---

## EZ-100: Lexer & Parser

**User Value:** Easytrieve source programs are parsed into structured representations for interpretation.

### EZ-100.1: 80-Column Source Tokenizer
**Acceptance Criteria:** Given 80-column source with `*` comments, `+` continuation, and free-form statements, when tokenized, then tokens are produced correctly.

### EZ-100.2: FILE and FIELD Declarations
**Acceptance Criteria:** Given `FILE EMPFILE FB(80 80)` and `EMPNO 1 6 A`, when parsed, then FileDefinition and FieldDefinition AST nodes are created with correct attributes.

### EZ-100.3: JOB and SORT Activity Parsing
**Acceptance Criteria:** Given `JOB INPUT EMPFILE` and `SORT EMPFILE USING SORTFILE KEY(EMPNO)`, when parsed, then JobActivity and SortActivity AST nodes are produced.

### EZ-100.4: REPORT Declaration Parsing
**Acceptance Criteria:** Given `REPORT EMPRPT LINESIZE 132`, `TITLE 'EMPLOYEE REPORT'`, `LINE EMPNO ENAME SALARY`, when parsed, then ReportDefinition with titles, lines, controls is produced.

### EZ-100.5: Parser Tests
**Acceptance Criteria:** Given complete Easytrieve programs with FILE/FIELD/JOB/SORT/REPORT, when parsed, then correct AST is produced; given malformed source, then diagnostics with line numbers.

---

## EZ-101: Interpreter Core

**User Value:** Easytrieve programs execute with field operations, control flow, and data manipulation.

### EZ-101.1: Field Storage and Type Coercion
**Acceptance Criteria:** Given fields of types A, N, P, B, U, I, when values are assigned and retrieved, then type coercion (e.g., numeric to alpha, packed to display) works correctly.

### EZ-101.2: Arithmetic and String Expressions
**Acceptance Criteria:** Given `SALARY = BASE-PAY + BONUS * 1.1`, when evaluated, then decimal arithmetic produces correct results. Given `NAME = TRIM(FIRST-NAME) + ' ' + TRIM(LAST-NAME)`, then string concatenation works.

### EZ-101.3: IF/ELSE/END-IF Control Flow
**Acceptance Criteria:** Given `IF SALARY GT 50000 ... ELSE ... END-IF`, when executed, then correct branch is taken. Given AND/OR/NOT operators, then compound conditions evaluate correctly.

### EZ-101.4: DO/END-DO and PERFORM
**Acceptance Criteria:** Given `DO WHILE COUNT LT 100 ... END-DO`, then loop executes correctly. Given `PERFORM CALC-TOTALS`, then named paragraph executes and returns.

### EZ-101.5: MOVE, MOVE LIKE, and SEARCH
**Acceptance Criteria:** Given `MOVE LIKE INPUT-REC TO OUTPUT-REC`, when executed, then fields with matching names are copied. Given `SEARCH TABLE-NAME ...`, then table lookup returns correct entry.

---

## EZ-102: File Processing

**User Value:** Easytrieve automatically processes input files and handles multi-file synchronization.

### EZ-102.1: Automatic Input Processing (JOB)
**Acceptance Criteria:** Given JOB INPUT EMPFILE, when executed, then each record is read and user statements execute for each record until EOF.

### EZ-102.2: Sequential File I/O
**Acceptance Criteria:** Given READ and WRITE operations, when executed, then sequential files are read/written via the dataset crate.

### EZ-102.3: Indexed/VSAM File I/O
**Acceptance Criteria:** Given a KSDS file with KEY, when READ is issued with a key value, then the matching record is returned.

### EZ-102.4: Multi-File MATCH Processing
**Acceptance Criteria:** Given two input files with matching keys, when JOB uses MATCH, then synchronized processing occurs (match/no-match handling).

### EZ-102.5: File I/O Tests
**Acceptance Criteria:** Given sequential, indexed, and MATCH scenarios, when tested, then correct file processing occurs.

---

## EZ-103: Report Generator

**User Value:** Formatted reports are produced with headings, detail lines, control breaks, subtotals, and grand totals.

### EZ-103.1: TITLE and HEADING Lines
**Acceptance Criteria:** Given TITLE and HEADING definitions, when a report is generated, then page headers with titles and column headings appear at the top of each page.

### EZ-103.2: Detail LINE Output
**Acceptance Criteria:** Given `LINE EMPNO ENAME SALARY`, when records are processed, then detail lines are formatted with field values aligned under headings.

### EZ-103.3: CONTROL Breaks
**Acceptance Criteria:** Given `CONTROL DEPTNO`, when the DEPTNO value changes, then control break processing triggers with before/after user code.

### EZ-103.4: SUM — Automatic Totaling
**Acceptance Criteria:** Given `SUM SALARY BONUS`, when control breaks occur, then subtotals for SALARY and BONUS are printed; at end of report, grand totals are printed.

### EZ-103.5: Page Control
**Acceptance Criteria:** Given PAGE-BREAK and page size settings, when the page fills, then a new page begins with reprinted titles and headings.

### EZ-103.6: Report Tests
**Acceptance Criteria:** Given a complete report program with titles, control breaks, and sums, when executed, then formatted output matches expected report layout.

---

## EZ-104: SQL/Database Integration

**User Value:** Easytrieve programs access DB2 and IMS databases alongside file processing.

### EZ-104.1: Embedded SQL (DB2)
**Acceptance Criteria:** Given `SQL SELECT ENAME, SALARY FROM EMP WHERE DEPTNO = W-DEPT`, when executed, then result set is available for processing.

### EZ-104.2: IMS/DLI Access
**Acceptance Criteria:** Given DLI calls (GU, GN) within Easytrieve, when executed, then IMS database segments are accessible.

### EZ-104.3: SQL INSERT/UPDATE/DELETE
**Acceptance Criteria:** Given SQL modification statements, when executed within a JOB activity, then database changes are committed.

### EZ-104.4: Database Integration Tests
**Acceptance Criteria:** Given DB2 and IMS access scenarios, when tested, then correct data retrieval and modification occurs.

---

## EZ-105: SORT & Utilities

**User Value:** Data is sorted, filtered, and organized before or during processing.

### EZ-105.1: SORT Activity
**Acceptance Criteria:** Given `SORT EMPFILE USING SORTWORK KEY(DEPTNO EMPNO)`, when executed, then records are sorted by DEPTNO then EMPNO.

### EZ-105.2: SELECT/REJECT in SORT
**Acceptance Criteria:** Given `SELECT IF SALARY GT 30000`, when specified in SORT, then only matching records appear in output.

### EZ-105.3: SORT as Input to JOB
**Acceptance Criteria:** Given SORT followed by JOB, when executed, then JOB processes the sorted output.

### EZ-105.4: SORT Tests
**Acceptance Criteria:** Given various SORT scenarios with key fields and selection, when tested, then correct sorted output is produced.

---

## EZ-106: Macros, External Calls & Tests

**User Value:** Programs can include shared code and invoke external programs.

### EZ-106.1: %INCLUDE Macros
**Acceptance Criteria:** Given `%INCLUDE COMMON-FIELDS`, when processed, then the copybook content is included in the source.

### EZ-106.2: CALL External Programs
**Acceptance Criteria:** Given `CALL MYPROG USING PARM1 PARM2`, when executed, then the external program is invoked with parameters.

### EZ-106.3: LINK and TRANSFER
**Acceptance Criteria:** Given `LINK SUBPROG`, when executed, then control transfers and returns. Given `TRANSFER`, then control transfers without return.

### EZ-106.4: End-to-End Tests
**Acceptance Criteria:** Given complete Easytrieve programs with FILE/JOB/REPORT/SORT, when `cargo test -p open-mainframe-easytrieve` runs, then all tests pass.

---

## FR/NFR Coverage Matrix

| FR | Stories |
|----|---------|
| FR-EZ-001 | EZ-100.1, EZ-100.2 |
| FR-EZ-002 | EZ-100.2, EZ-102.1 |
| FR-EZ-003 | EZ-100.2, EZ-101.1 |
| FR-EZ-004 | EZ-101.3, EZ-101.4 |
| FR-EZ-005 | EZ-101.2, EZ-101.5 |
| FR-EZ-006 | EZ-102.1, EZ-102.4 |
| FR-EZ-007 | EZ-103.1–EZ-103.5 |
| FR-EZ-008 | EZ-105.1, EZ-105.2, EZ-105.3 |
| FR-EZ-009 | EZ-104.1, EZ-104.2, EZ-104.3 |
| FR-EZ-010 | EZ-106.1, EZ-106.2, EZ-106.3 |

**Coverage: 10/10 FRs (100%), 3/3 NFRs (100%)**
