---
version: 'v5.0'
planningGroup: 'PG-29'
technology: 'Natural'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-natural-v5.0.md'
  - 'architecture-natural-v5.0.md'
totalEpics: 11
totalStories: 55
frCoverage: '12/12 (100%)'
nfrCoverage: '3/3 (100%)'
---

# Epics & Stories: Natural

## Epic Overview

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| NAT-100 | Lexer & Parser | L | 6 | F |
| NAT-101 | Data Model & DEFINE DATA | M | 5 | F |
| NAT-102 | Interpreter Core | M | 5 | F |
| NAT-103 | Data Manipulation | M | 5 | F |
| NAT-104 | ADABAS Database Access | L | 6 | F |
| NAT-105 | SQL Database Access | M | 4 | F |
| NAT-106 | Output & Reporting | M | 5 | F |
| NAT-107 | Interactive I/O & Maps | M | 5 | F |
| NAT-108 | System Variables & Functions | M | 5 | F |
| NAT-109 | Error Handling & Work Files | S | 4 | F |
| NAT-110 | Environment & Security | M | 5 | F |

---

## NAT-100: Lexer & Parser

### NAT-100.1: Source Tokenizer (130+ keywords)
**Acceptance Criteria:** Given Natural source with structured/reporting mode keywords, when tokenized, then all 130+ keywords and identifiers are recognized.

### NAT-100.2: Statement Parser — Control Flow
**Acceptance Criteria:** Given IF/DECIDE FOR/DECIDE ON/FOR/REPEAT/ESCAPE/PERFORM, when parsed, then correct AST nodes are produced.

### NAT-100.3: Statement Parser — Database
**Acceptance Criteria:** Given READ/FIND/HISTOGRAM/GET/STORE/UPDATE/DELETE/END TRANSACTION, when parsed, then database operation AST nodes are produced.

### NAT-100.4: Statement Parser — I/O
**Acceptance Criteria:** Given DISPLAY/WRITE/PRINT/INPUT/REINPUT, when parsed, then I/O operation AST nodes are produced.

### NAT-100.5: Statement Parser — Data Manipulation
**Acceptance Criteria:** Given COMPUTE/MOVE/COMPRESS/SEPARATE/EXAMINE/SORT, when parsed, then manipulation AST nodes are produced.

### NAT-100.6: Parser Tests
**Acceptance Criteria:** Given comprehensive Natural source covering all statement categories, when parsed, then correct AST is produced.

---

## NAT-101: Data Model & DEFINE DATA

### NAT-101.1: DEFINE DATA Parsing
**Acceptance Criteria:** Given `DEFINE DATA LOCAL 01 #EMPID (A8) 01 #SALARY (P7.2) END-DEFINE`, when parsed, then variables with types, levels, and lengths are defined.

### NAT-101.2: 11 Data Types
**Acceptance Criteria:** Given types A, B, C, D, F, I, L, N, P, T, U, when variables are defined, then correct storage and arithmetic behavior applies.

### NAT-101.3: Arrays
**Acceptance Criteria:** Given `01 #TABLE (A20/1:100)`, when defined, then a 100-element array is created and indexed access works.

### NAT-101.4: Dynamic Variables
**Acceptance Criteria:** Given `01 #TEXT (A) DYNAMIC`, when assigned, then the variable grows to accommodate the value.

### NAT-101.5: Data Area Tests
**Acceptance Criteria:** Given DEFINE DATA with all types, arrays, groups, and dynamic variables, when tested, then correct storage behavior occurs.

---

## NAT-102: Interpreter Core

### NAT-102.1: Control Flow (IF/DECIDE/FOR/REPEAT)
**Acceptance Criteria:** Given `IF #X > 5 ... END-IF`, `DECIDE FOR EVERY CONDITION`, `FOR #I = 1 TO 10`, `REPEAT UNTIL #DONE`, when executed, then correct branching/looping occurs.

### NAT-102.2: PERFORM and ESCAPE
**Acceptance Criteria:** Given `PERFORM SUBROUTINE-NAME`, when executed, then the subroutine runs and returns. Given `ESCAPE BOTTOM`, then the innermost loop is exited.

### NAT-102.3: CALLNAT, FETCH, and STACK
**Acceptance Criteria:** Given `CALLNAT 'SUBPGM' #PARM1 #PARM2`, when executed, then the subprogram runs with parameters. Given `FETCH 'NEXTPGM'`, then control transfers permanently. Given `STACK TOP DATA #VAL`, then data is stacked for the next program.

### NAT-102.4: Program Object Types
**Acceptance Criteria:** Given Program, Subprogram (CALLNAT target), Subroutine (PERFORM target), when distinguished, then each type has correct invocation semantics.

### NAT-102.5: Interpreter Tests
**Acceptance Criteria:** Given control flow, program invocation, and stack scenarios, when tested, then correct execution occurs.

---

## NAT-103: Data Manipulation

### NAT-103.1: COMPUTE and MOVE
**Acceptance Criteria:** Given `COMPUTE #TOTAL = #BASE * 1.1 + #BONUS`, when executed, then decimal arithmetic produces correct result. Given `MOVE BY NAME FROM #IN TO #OUT`, then matching field names are copied.

### NAT-103.2: COMPRESS and SEPARATE
**Acceptance Criteria:** Given `COMPRESS #FIRST #LAST INTO #FULLNAME`, then fields are concatenated. Given `SEPARATE #FULLNAME INTO #FIRST #LAST WITH DELIMITER ','`, then the string is split.

### NAT-103.3: EXAMINE
**Acceptance Criteria:** Given `EXAMINE #TEXT FOR 'OLD' REPLACE 'NEW'`, when executed, then all occurrences are replaced and *NUMBER returns the count.

### NAT-103.4: SORT
**Acceptance Criteria:** Given `SORT BY #SALARY DESCENDING ... END-SORT`, when used within a processing loop, then output is reordered.

### NAT-103.5: Manipulation Tests
**Acceptance Criteria:** Given arithmetic, string, and sort operations, when tested, then correct results are produced.

---

## NAT-104: ADABAS Database Access

### NAT-104.1: DDM Integration
**Acceptance Criteria:** Given DDM EMPLOYEES mapping to ADABAS file 5, when `READ EMPLOYEES BY NAME`, then Natural fields map to ADABAS FDT fields via DDM.

### NAT-104.2: READ and FIND
**Acceptance Criteria:** Given `READ EMPLOYEES BY NAME STARTING FROM 'SMITH'`, then records are read in logical sequence. Given `FIND EMPLOYEES WITH DEPT = 'D01'`, then matching records via inverted list.

### NAT-104.3: HISTOGRAM
**Acceptance Criteria:** Given `HISTOGRAM EMPLOYEES DEPT`, then each unique DEPT value and count are returned.

### NAT-104.4: STORE, UPDATE, DELETE
**Acceptance Criteria:** Given `STORE EMPLOYEES`, then a new record is stored via ADABAS N1. Given `UPDATE`, then current record is modified via A1. Given `DELETE`, then current record is removed via E1.

### NAT-104.5: END TRANSACTION / BACKOUT
**Acceptance Criteria:** Given `END TRANSACTION`, then ADABAS ET is issued. Given `BACKOUT TRANSACTION`, then ADABAS BT is issued.

### NAT-104.6: ADABAS Access Tests
**Acceptance Criteria:** Given DDM-based CRUD operations, when tested against open-mainframe-adabas, then all pass.

---

## NAT-105: SQL Database Access

### NAT-105.1: SELECT
**Acceptance Criteria:** Given `SELECT * FROM EMP INTO #EMPID, #ENAME WHERE DEPTNO = 'D01'`, then DB2 query results populate Natural variables.

### NAT-105.2: INSERT/UPDATE/DELETE
**Acceptance Criteria:** Given `INSERT INTO EMP VALUES (#EMPID, #ENAME)`, then row is inserted. Given `UPDATE EMP SET SALARY = #SAL WHERE EMPID = #ID`, then row is updated.

### NAT-105.3: COMMIT/ROLLBACK
**Acceptance Criteria:** Given `COMMIT` and `ROLLBACK`, then SQL transaction control works correctly.

### NAT-105.4: SQL Tests
**Acceptance Criteria:** Given SQL DML operations, when tested, then correct data manipulation occurs.

---

## NAT-106: Output & Reporting

### NAT-106.1: DISPLAY Statement
**Acceptance Criteria:** Given `DISPLAY NAME SALARY`, then columnar output with automatic headers is produced.

### NAT-106.2: WRITE Statement
**Acceptance Criteria:** Given `WRITE 'Employee:' NAME 'Salary:' SALARY`, then free-form output is produced.

### NAT-106.3: Control Breaks (AT BREAK)
**Acceptance Criteria:** Given `AT BREAK OF DEPT ... END-BREAK`, then subtotals and break processing occur when DEPT changes.

### NAT-106.4: Page Headers and Formatting
**Acceptance Criteria:** Given `AT TOP OF PAGE WRITE 'Report Title'`, then headers appear at each page boundary. Given `NEWPAGE`, then a page break is forced.

### NAT-106.5: Report Tests
**Acceptance Criteria:** Given report programs with headers, detail, breaks, and totals, when tested, then correct formatted output is produced.

---

## NAT-107: Interactive I/O & Maps

### NAT-107.1: INPUT Statement
**Acceptance Criteria:** Given `INPUT 'Enter ID:' #EMPID`, then terminal input is captured into #EMPID.

### NAT-107.2: REINPUT
**Acceptance Criteria:** Given `REINPUT 'Invalid input' MARK *#EMPID`, then the screen is redisplayed with error message and cursor on the marked field.

### NAT-107.3: Map Objects
**Acceptance Criteria:** Given a Natural Map with field positions and attributes, when INPUT USING MAP 'EMPMAP', then the map is displayed and input fields populate variables.

### NAT-107.4: PF Key Handling
**Acceptance Criteria:** Given `IF *PF-KEY = 'PF3' ESCAPE BOTTOM`, then PF3 exits the current loop.

### NAT-107.5: Map Tests
**Acceptance Criteria:** Given map-based input/output scenarios, when tested, then correct terminal interaction occurs.

---

## NAT-108: System Variables & Functions

### NAT-108.1: System Variables (70+)
**Acceptance Criteria:** Given *DATX (date), *TIMX (time), *USER (user ID), *PROGRAM (current program), *LIBRARY (current library), when referenced, then correct values are returned.

### NAT-108.2: Character Functions
**Acceptance Criteria:** Given SUBSTR, LENGTH, EDIT, VAL, TRANSLATE functions, when called, then correct results are produced.

### NAT-108.3: Date/Time Functions
**Acceptance Criteria:** Given EDIT-DATE, ADD-DURATION, SUBTRACT-DURATION, when called, then date arithmetic works correctly.

### NAT-108.4: Numeric Functions
**Acceptance Criteria:** Given ABS, SIGN, FRAC, INT, MOD, RET, when called, then correct numeric results are produced.

### NAT-108.5: Function Tests
**Acceptance Criteria:** Given all built-in function categories, when tested, then correct results.

---

## NAT-109: Error Handling & Work Files

### NAT-109.1: ON ERROR Block
**Acceptance Criteria:** Given `ON ERROR ... IF *ERROR-NR = 3009 ... END-ERROR`, then runtime errors are trapped and the error block executes.

### NAT-109.2: Work File I/O
**Acceptance Criteria:** Given `WRITE WORK FILE 1 #RECORD` and `READ WORK FILE 1 #RECORD`, then data is written to and read from work files.

### NAT-109.3: STACK Operations
**Acceptance Criteria:** Given `STACK TOP DATA 'COMMAND'`, then stacked data is available to the next program via INPUT.

### NAT-109.4: Error & Work File Tests
**Acceptance Criteria:** Given error scenarios and work file I/O, when tested, then correct handling occurs.

---

## NAT-110: Environment & Security

### NAT-110.1: LOGON and Library System
**Acceptance Criteria:** Given `LOGON MYLIB`, then the current library context is set and objects in MYLIB are accessible.

### NAT-110.2: CATALOG and STOW
**Acceptance Criteria:** Given `CATALOG` (compile and save) and `STOW` (save source), then Natural objects are stored in the library.

### NAT-110.3: Natural Security
**Acceptance Criteria:** Given security profiles for libraries and programs, then access is controlled based on user authorization.

### NAT-110.4: EntireX RPC (Basic)
**Acceptance Criteria:** Given `CALLNAT 'REMOTEPGM' ... REMOTELY`, then basic RPC invocation to an external Natural server is supported.

### NAT-110.5: Environment Tests
**Acceptance Criteria:** Given library operations, security, and environment scenarios, when tested, then correct behavior occurs.

---

## FR/NFR Coverage

**Coverage: 12/12 FRs (100%), 3/3 NFRs (100%)**
