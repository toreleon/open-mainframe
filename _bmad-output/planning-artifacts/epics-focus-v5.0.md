---
version: 'v5.0'
planningGroup: 'PG-30'
technology: 'FOCUS'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-focus-v5.0.md'
  - 'architecture-focus-v5.0.md'
totalEpics: 11
totalStories: 55
frCoverage: '12/12 (100%)'
nfrCoverage: '3/3 (100%)'
---

# Epics & Stories: FOCUS

## Epic Overview

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| FOC-100 | Multi-Dialect Language Parser | L | 6 | F |
| FOC-101 | Master File Descriptor (MFD) | M | 5 | F |
| FOC-102 | TABLE Request Engine | L | 6 | F |
| FOC-103 | GRAPH Engine | M | 4 | F |
| FOC-104 | MODIFY/MAINTAIN Engine | M | 5 | F |
| FOC-105 | Dialogue Manager | M | 5 | F |
| FOC-106 | Built-in Functions (150+) | M | 5 | F |
| FOC-107 | Data Adapters | L | 6 | F |
| FOC-108 | Output Formatting | M | 5 | F |
| FOC-109 | Joins & Multi-Source | M | 4 | F |
| FOC-110 | Mainframe Integration | M | 4 | F |

---

## FOC-100: Multi-Dialect Language Parser

### FOC-100.1: TABLE Request Parser
**Acceptance Criteria:** Given `TABLE FILE EMPLOYEE PRINT ENAME SALARY BY DEPT END`, when parsed, then a TABLE request AST with PRINT fields, BY dimension, and source file is produced.

### FOC-100.2: GRAPH Request Parser
**Acceptance Criteria:** Given `GRAPH FILE SALES SUM REVENUE BY REGION TYPE BAR END`, when parsed, then a GRAPH request AST with aggregation and chart type is produced.

### FOC-100.3: MODIFY Request Parser
**Acceptance Criteria:** Given `MODIFY FILE EMPLOYEE FIXFORM ... END`, when parsed, then a MODIFY request with transaction logic is produced.

### FOC-100.4: Dialogue Manager Parser
**Acceptance Criteria:** Given `-SET &DEPT = 'D01'`, `-IF &DEPT EQ 'D01' GOTO PROCESS`, `-RUN report.fex`, when parsed, then Dialogue Manager commands are recognized.

### FOC-100.5: SQL Dialect Parser
**Acceptance Criteria:** Given `SQL SELECT * FROM EMP WHERE DEPT = 'D01' END`, when parsed, then SQL passthrough is recognized.

### FOC-100.6: Parser Tests
**Acceptance Criteria:** Given all FOCUS dialects, when parsed, then correct AST structures are produced.

---

## FOC-101: Master File Descriptor (MFD)

### FOC-101.1: MFD Parsing
**Acceptance Criteria:** Given `FILENAME=EMPLOYEE, SUFFIX=FOC ... FIELDNAME=EMPID, ALIAS=EMP_ID, USAGE=A8, INDEX=I`, when parsed, then field metadata with physical mapping is created.

### FOC-101.2: Segment Hierarchy
**Acceptance Criteria:** Given multi-segment MFD (parent-child), when parsed, then hierarchical relationships are established.

### FOC-101.3: Access File
**Acceptance Criteria:** Given Access File mapping MFD to a physical data source (e.g., DB2 table), when loaded, then the adapter knows how to translate operations.

### FOC-101.4: Data Types and Formats
**Acceptance Criteria:** Given FOCUS data types (A, An, I, F, D, P, YMD, etc.), when defined, then correct storage and display formatting applies.

### FOC-101.5: MFD Tests
**Acceptance Criteria:** Given MFD definitions with various structures, when tested, then correct metadata is produced.

---

## FOC-102: TABLE Request Engine

### FOC-102.1: PRINT and SUM Aggregation
**Acceptance Criteria:** Given `TABLE FILE EMP PRINT ENAME SALARY SUM SALARY END`, then detail lines and total are produced.

### FOC-102.2: BY and ACROSS Dimensions
**Acceptance Criteria:** Given `TABLE FILE SALES SUM REVENUE BY REGION ACROSS QUARTER END`, then a cross-tabulation matrix is produced with regions as rows and quarters as columns.

### FOC-102.3: WHERE Filtering
**Acceptance Criteria:** Given `WHERE SALARY GT 50000`, then only matching records appear in output.

### FOC-102.4: DEFINE FILE and COMPUTE
**Acceptance Criteria:** Given `DEFINE FILE EMP BONUS/D8.2 = SALARY * 0.1; END`, then virtual field BONUS is computed for each record.

### FOC-102.5: HEADING, FOOTING, SUBFOOT
**Acceptance Criteria:** Given heading/footing specifications, then page headers and footers appear in output.

### FOC-102.6: TABLE Tests
**Acceptance Criteria:** Given TABLE requests with various features, when tested, then correct report output is produced.

---

## FOC-103: GRAPH Engine

### FOC-103.1: BAR and PIE Charts
**Acceptance Criteria:** Given `GRAPH ... TYPE BAR`, when executed, then a text-mode bar chart is produced. Given `TYPE PIE`, then a pie chart representation.

### FOC-103.2: LINE and AREA Charts
**Acceptance Criteria:** Given time-series data with `TYPE LINE`, then a line chart is produced.

### FOC-103.3: Chart Formatting
**Acceptance Criteria:** Given color, title, and legend specifications, when applied, then the chart includes formatting.

### FOC-103.4: Graph Tests
**Acceptance Criteria:** Given various chart types, when tested, then correct graphical output is produced.

---

## FOC-104: MODIFY/MAINTAIN Engine

### FOC-104.1: MODIFY — Batch Transaction
**Acceptance Criteria:** Given `MODIFY FILE EMPLOYEE FIXFORM ... MATCH EMPID ON MATCH UPDATE ... ON NOMATCH INCLUDE END`, then batch update/insert logic executes.

### FOC-104.2: MAINTAIN — Interactive Transaction
**Acceptance Criteria:** Given MAINTAIN with screen-based input, then interactive data entry/update is supported.

### FOC-104.3: Transaction Integrity
**Acceptance Criteria:** Given COMMIT/ROLLBACK in MODIFY, then transaction boundaries are enforced.

### FOC-104.4: Validation Rules
**Acceptance Criteria:** Given VALIDATE specifications, then input data is checked before modification.

### FOC-104.5: Modify/Maintain Tests
**Acceptance Criteria:** Given batch and interactive transaction scenarios, when tested, then correct data changes occur.

---

## FOC-105: Dialogue Manager

### FOC-105.1: Amper Variables
**Acceptance Criteria:** Given `-SET &DEPT = 'D01'`, then local amper variable is set. Given `&&GLOBAL = 'VALUE'`, then global variable is set.

### FOC-105.2: Control Flow (-IF, -GOTO, -LOOP)
**Acceptance Criteria:** Given `-IF &X EQ 1 GOTO LABEL1;`, then conditional branching works. Given `-REPEAT ... -UNTIL`, then looping works.

### FOC-105.3: -RUN and -INCLUDE
**Acceptance Criteria:** Given `-RUN myreport.fex`, then the specified FOCUS procedure is executed. Given `-INCLUDE common.fex`, then the file is included.

### FOC-105.4: System Variables
**Acceptance Criteria:** Given &DATE, &TIME, &USER, &FOCFOC, when referenced, then correct system values are returned.

### FOC-105.5: Dialogue Manager Tests
**Acceptance Criteria:** Given Dialogue Manager procedures with variables, control flow, and RUN, when tested, then correct execution occurs.

---

## FOC-106: Built-in Functions (150+)

### FOC-106.1: Character Functions
**Acceptance Criteria:** Given SUBSTR, TRIM, EDIT, PARONE, ARGLEN, CHAR, when called, then correct string results.

### FOC-106.2: Date/Time Functions
**Acceptance Criteria:** Given DATECVT, DATEDIF, DATEADD, TODAY, when called, then correct date arithmetic.

### FOC-106.3: Numeric Functions
**Acceptance Criteria:** Given ABS, INT, MOD, ROUND, MAX, MIN, when called, then correct numeric results.

### FOC-106.4: Conversion Functions
**Acceptance Criteria:** Given ATODBL, DBLTOS, CHKFMT, CTRAN, when called, then type conversion works.

### FOC-106.5: Function Tests
**Acceptance Criteria:** Given all function categories, when tested, then correct results.

---

## FOC-107: Data Adapters

### FOC-107.1: FOCUS Native (.FOC) Adapter
**Acceptance Criteria:** Given a .FOC file with MFD, when TABLE requests execute, then data is read/written in FOCUS native format.

### FOC-107.2: Sequential File Adapter
**Acceptance Criteria:** Given a sequential dataset with fixed-format records, when accessed via MFD, then records are read/written.

### FOC-107.3: VSAM Adapter
**Acceptance Criteria:** Given a VSAM KSDS with MFD, when accessed, then keyed and sequential access works.

### FOC-107.4: DB2 Adapter
**Acceptance Criteria:** Given DB2 tables mapped via Access File, when TABLE/MODIFY requests execute, then SQL is generated and executed.

### FOC-107.5: IMS/DLI Adapter
**Acceptance Criteria:** Given IMS database via Access File, when accessed, then DL/I calls retrieve segments.

### FOC-107.6: Adapter Tests
**Acceptance Criteria:** Given multiple data source scenarios, when tested, then all adapters produce correct results.

---

## FOC-108: Output Formatting

### FOC-108.1: Plain Text Report
**Acceptance Criteria:** Given TABLE output, then formatted text with aligned columns, headers, and totals is produced.

### FOC-108.2: StyleSheet Application
**Acceptance Criteria:** Given `STYLESHEET ... TYPE=HEADING, FONT=BOLD ... END`, then styled output is produced.

### FOC-108.3: HTML Output
**Acceptance Criteria:** Given `ON TABLE SET STYLE *` or HTML target, then HTML table output is generated.

### FOC-108.4: HOLD File Generation
**Acceptance Criteria:** Given `ON TABLE HOLD`, then output is written to a HOLD file for subsequent processing.

### FOC-108.5: Output Tests
**Acceptance Criteria:** Given text, HTML, and HOLD output scenarios, when tested, then correct formatted output is produced.

---

## FOC-109: Joins & Multi-Source

### FOC-109.1: JOIN
**Acceptance Criteria:** Given `JOIN FILE1 IN FILE2 AS JOINNAME WHERE F1.KEY EQ F2.KEY END`, then files are joined and accessible in TABLE requests.

### FOC-109.2: COMBINE
**Acceptance Criteria:** Given `COMBINE ... END`, then multiple report requests are combined into a single output.

### FOC-109.3: MATCH FILE
**Acceptance Criteria:** Given MATCH FILE with OLD-FILE/NEW-FILE, then records are matched and differences identified.

### FOC-109.4: Join Tests
**Acceptance Criteria:** Given join, combine, and match scenarios, when tested, then correct multi-source results.

---

## FOC-110: Mainframe Integration

### FOC-110.1: FILEDEF
**Acceptance Criteria:** Given `FILEDEF EMPFILE DISK /data/employees.dat`, then the logical filename maps to a physical file.

### FOC-110.2: DYNAM ALLOCATE
**Acceptance Criteria:** Given `DYNAM ALLOC FILE MYDD DA MY.DATASET SHR`, then dynamic dataset allocation occurs.

### FOC-110.3: TSO/CICS Interface
**Acceptance Criteria:** Given FOCUS running under TSO, then TSO commands can be issued. Given CICS environment, then FOCUS transactions execute within CICS.

### FOC-110.4: Integration Tests
**Acceptance Criteria:** Given FILEDEF, DYNAM, and environment scenarios, when tested, then correct mainframe integration occurs.

---

## FR/NFR Coverage

**Coverage: 12/12 FRs (100%), 3/3 NFRs (100%)**
