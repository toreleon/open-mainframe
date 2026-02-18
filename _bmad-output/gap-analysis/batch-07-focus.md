# Gap Analysis: FOCUS (Information Builders / OpenText 4GL)

## Official Specification Summary

FOCUS is a fourth-generation programming language (4GL) developed by Information Builders Inc. (acquired by OpenText in 2021), originally released in 1975 for data handling, reporting, and analysis on IBM mainframes. FOCUS evolved from RAMIS, created by Gerald C. Cohen at Mathematica Products Group. In 1997, the web-enabled version **WebFOCUS** was introduced.

FOCUS is classified as **Common** (enterprise 4GL) on mainframes:
- Non-procedural, English-like language for ad-hoc queries, reports, graphs, and data maintenance
- Operates through multiple integrated "dialects": TABLE (reporting), GRAPH (charting), MODIFY/MAINTAIN (transactions), SQL, and Dialogue Manager (flow control)
- Master File Descriptor (MFD) metadata layer abstracts physical data sources
- Supports 20+ data adapters (DB2, Oracle, VSAM, IMS, ADABAS, flat files, etc.)
- Runs on z/OS (MVS/TSO/CICS), UNIX, Windows — cross-platform portability
- Stored procedures called FOCEXECs
- 150+ built-in functions across character, date/time, numeric, conversion, and I/O categories
- Dialogue Manager provides procedural control flow with amper variables (&/&&)
- StyleSheet system for formatted output (HTML, PDF, Excel, CSV)
- Competes with SAS in reporting; focuses on data import/export flexibility and end-user reporting

Key documentation:
- **FOCUS for Mainframe v7.6** — latest mainframe release documentation
- **FOCUS Technical Library** — ECL (Electronic Content Library) at ecl.informationbuilders.com
- **WebFOCUS 8.x** — current web-enabled version documentation
- **Creating Reports** — TABLE/PRINT/GRAPH reference
- **Developing Applications** — Dialogue Manager reference
- **Using Functions** — built-in function reference

## Key Features & Capabilities

### 1. Data Types

| Format | Name | Description |
|--------|------|-------------|
| An | Alphanumeric | Fixed-length character (n = 1–4096 bytes) |
| AnV | Variable Alpha | Variable-length alphanumeric |
| I | Integer | Binary integer (4 bytes) |
| D | Double | Double-precision decimal (8 bytes, 16 digits) |
| Dn.m | Date | Date format (various display formats: YYMD, DMY, MDY, etc.) |
| F | Float | Floating-point (8 bytes) |
| P | Packed | Packed decimal (BCD) |
| In.m | Time | Date-time format (nanosecond precision) |
| TX | Text | Large text blob |
| Sm.n | Smart Date | Intelligent date with granularity (year, quarter, month, day) |

### 2. TABLE Request (Report Language)

The TABLE dialect is the core of FOCUS reporting:

```focus
TABLE FILE EMPLOYEE
PRINT LAST_NAME FIRST_NAME SALARY
BY DEPARTMENT
WHERE SALARY GT 50000
ON TABLE SET PAGE NOPAGE
ON TABLE SET STYLE *
GRID=OFF, $
END
END
```

#### TABLE Verbs
| Verb | Purpose |
|------|---------|
| PRINT | Display individual field values (detail lines) |
| SUM | Aggregate (sum) numeric fields |
| COUNT | Count records |
| AVG | Average numeric fields |
| MAX / MIN | Maximum / minimum values |
| PCT / CPCT | Percentage / cumulative percentage |
| FIRST / LAST | First / last value in group |
| LIST | List unique values |

#### TABLE Qualifiers
| Qualifier | Purpose |
|-----------|---------|
| BY | Sort/group by field (vertical) |
| ACROSS | Cross-tabulate by field (horizontal) |
| WHERE | Filter conditions (field OP value) |
| IF | Conditional inclusion |
| ON TABLE SET | Set report-level options |
| ON TABLE HOLD | Save results to temporary file |
| ON TABLE PCHOLD | Save to PC download format |
| HEADING | Report heading text |
| FOOTING | Report footing text |
| SUBHEAD | Section subheading |
| SUBFOOT | Section subfooting |
| PAGE-BREAK | Force page break on value change |

#### DEFINE / COMPUTE
| Feature | Purpose |
|---------|---------|
| DEFINE FILE | Create virtual (calculated) fields on a data source |
| COMPUTE | Calculate values within a report request |
| IF ... THEN ... ELSE | Conditional computation |
| DECODE | Value lookup/translation (DECODE field value1 result1 ...) |

### 3. GRAPH Request

```focus
GRAPH FILE SALES
SUM REVENUE
BY REGION
ON GRAPH SET GRAPHTYPE BAR
ON GRAPH SET LOOKGRAPH VBAR
END
```

- Produces charts from same syntax as TABLE (replace TABLE with GRAPH)
- Chart types: BAR, VBAR, HBAR, PIE, LINE, AREA, SCATTER, etc.
- Can overlay multiple data series
- Output to terminal, PostScript, HTML, or image formats

### 4. MODIFY / MAINTAIN (Transaction Processing)

```focus
MODIFY FILE EMPLOYEE
  MATCH EMPLOYEE_ID
  ON MATCH UPDATE SALARY
  ON NOMATCH REJECT
  DATA ON TRAN_FILE
END
```

| Statement | Purpose |
|-----------|---------|
| MODIFY | Begin transaction processing block |
| MATCH | Specify key field(s) for matching |
| ON MATCH UPDATE | Update matching records |
| ON MATCH DELETE | Delete matching records |
| ON NOMATCH INCLUDE | Add new records |
| ON NOMATCH REJECT | Reject non-matching transactions |
| VALIDATE | Field validation rules |
| COMPUTE | Calculate values during transaction |
| TYPE | Display messages during processing |
| GOTO / PERFORM | Flow control within MODIFY |
| CASE | Multi-way branching |

**MAINTAIN** is an enhanced interactive version of MODIFY with screen-based data entry:
- MAINTAIN FILE, MAINTAIN STACK
- User-defined screens via WINFORM
- Scrollable data displays
- PF-key handlers
- Integrated with Dialogue Manager

### 5. Master File Descriptor (MFD)

The Master File defines metadata for any data source:

```focus
FILENAME=EMPLOYEE, SUFFIX=FOC,
  SEGMENT=EMPSEG, SEGTYPE=S0,
    FIELDNAME=EMPLOYEE_ID, ALIAS=EMP_ID, FORMAT=A9, INDEX=I, $
    FIELDNAME=LAST_NAME, ALIAS=LNAME, FORMAT=A25, $
    FIELDNAME=FIRST_NAME, ALIAS=FNAME, FORMAT=A20, $
    FIELDNAME=SALARY, ALIAS=SAL, FORMAT=D12.2, $
    FIELDNAME=DEPARTMENT, ALIAS=DEPT, FORMAT=A10, INDEX=I, $
    FIELDNAME=HIRE_DATE, ALIAS=HDATE, FORMAT=YYMD, $
```

| MFD Component | Purpose |
|---------------|---------|
| FILENAME | Logical file name |
| SUFFIX | File type (FOC=FOCUS, FIX=fixed, COM=comma-delimited, etc.) |
| SEGMENT | Segment definition (hierarchical data model) |
| SEGTYPE | Segment type (S0–S9, KU=key unique, KM=key multiple, etc.) |
| FIELDNAME | Field definition |
| ALIAS | Physical field name/column mapping |
| FORMAT | Data type and length |
| INDEX | Index flag (I=indexed) |
| UNIQUE | Unique index |
| MISSING | Allow missing/null values |
| TITLE | Display title for reports |
| DESCRIPTION | Field description |
| WITHIN | Parent segment reference |

### 6. Access File (ACF)

The Access File specifies physical storage location:

```
SEGNAME=EMPSEG,
  DATASET=USER.EMPLOYEE.DATA,
  TABLENAME=EMPLOYEE,      (for SQL)
  CONNECTION=DB2PROD,       (for adapters)
```

### 7. Dialogue Manager (~29 commands)

| Command | Purpose |
|---------|---------|
| -SET | Assign value to amper variable |
| -DEFAULT / -DEFAULTS / -DEFAULTH | Set default variable values (visible/hidden) |
| -IF | Conditional branching |
| -GOTO | Transfer to label |
| -label | Define branch target |
| -REPEAT | Loop construct (with -UNTIL) |
| -RUN | Execute stacked FOCUS requests |
| -INCLUDE | Include and execute external FOCEXEC |
| -EXIT | Terminate procedure normally |
| -QUIT | Force immediate exit |
| -TYPE | Display messages to terminal |
| -READ | Read from external sequential file |
| -WRITE | Write to external sequential file |
| -CLOSE | Close external file |
| -PROMPT | Display message and read user input |
| -CRTCLEAR | Clear terminal screen |
| -CRTFORM | Create interactive form for input |
| -WINDOW | Execute window files |
| -REMOTE | Pass execution to server |
| -PASS | Issue and control passwords |
| -MVS | Execute z/OS operating system command |
| -TSO | Execute TSO command |
| -CMS | Execute CMS command |
| -MVS RUN / -TSO RUN / -CMS RUN | Load user-written external functions |
| -* | Comment line |
| -? | Display variable values (debugging) |

#### Amper Variables
| Type | Syntax | Scope |
|------|--------|-------|
| Local | &variable | Current procedure only |
| Global | &&variable | Entire session |
| System | &DATE, &TIME, &FOCFEXNAME, &LINES, etc. | Automatic |
| Statistical | &RECORDS, &LINES, &TOT, etc. | Set by last request |

Variable suffixes: `.EXIST` (test existence), `.LENGTH` (get length), `.EVAL` (force evaluation)

### 8. Joins and Multi-File Processing

| Feature | Purpose |
|---------|---------|
| JOIN | Join two data sources on matching keys |
| DEFINE FILE ... END | Virtual field definitions |
| COMBINE | Combine records from multiple files |
| MATCH FILE | Match and merge files |
| HOLD / SAVE | Save intermediate results for chaining |

```focus
JOIN EMPLOYEE_ID IN EMPLOYEE TO EMPLOYEE_ID IN PAYROLL
TABLE FILE EMPLOYEE
PRINT LAST_NAME SALARY BONUS
END
```

### 9. HOLD / SAVE (Intermediate Storage)

| Statement | Purpose |
|-----------|---------|
| ON TABLE HOLD | Save report output to FOCUS HOLD file |
| ON TABLE HOLD AS name | Save to named HOLD file |
| ON TABLE HOLD FORMAT fmt | Save in specific format (FOCUS, ALPHA, CSV, etc.) |
| ON TABLE PCHOLD | Save in PC download format |
| SAVE | Save to permanent FOCUS file |

Supported HOLD formats: FOCUS, ALPHA, COM (comma), TAB, COBOL, DB2, LOTUS, EXCEL, XML, HTML, PDF, DFIX, etc.

### 10. Built-in Functions (~150+)

#### Character Functions (~30+)
ARGLEN, ASIS, BITSON, BITVAL, BYTVAL, CHKFMT, CTRAN, CTRFLD, DCTRAN, DSTRIP, DSUBSTR, EDIT, GETTOK, JPTRANS, LCWORD, LCWORD2, LJUST, LOCASE, OVRLAY, PARAG, POSIT, REVERSE, RJUST, SOUNDEX, SPELLNM, SQUEEZ, STRIP, STRREP, SUBSTR, TRIM, UPCASE

#### Character Functions for AnV Fields (~6)
LENV, LOCASV, POSITV, SUBSTV, TRIMV, UPCASV

#### MAINTAIN-Specific Character Functions (~12)
CHAR2INT, INT2CHAR, LENGTH, LOWER, MASK, NLSCHR, SELECTS, STRAN, STRCMP, STRICMP, STRNCMP, TRIMLEN

#### Data Source / Decoding Functions (~5)
DB_LOOKUP, DECODE, FIND, LAST, LOOKUP

#### Standard Date/Time Functions (~22)
DATEADD, DATECVT, DATEDIF, DATEMOV, DATETRAN, HADD, HCNVRT, HDATE, HDIFF, HDTTM, HEXTR, HGETC, HMASK, HHMMSS, HINPUT, HMIDNT, HNAME, HPART, HSETPT, HTIME, TIMETOTS, TODAY

#### Legacy Date Functions (~12)
AYM, AYMD, CHGDAT, DA, DMY, MDY, YMD, DOWK, DOWKL, DT, GREGDT, JULDAT

#### MAINTAIN-Specific Date Functions (~12)
ADD, DAY, JULIAN, MONTH, QUARTER, SETMDY, SUB, WEEKDAY, YEAR, Initial_HHMMSS, Initial_TODAY, TODAY2

#### Format Conversion Functions (~10)
ATODBL, FTOA, HEXBYT, ITONUM, ITOPACK, ITOZ, PCKOUT, PTOA, UFMT, XTPACK

#### Numeric / Math Functions (~25)
ABS, BAR, CHKPCK, DMOD, FMOD, IMOD, EXP, EXPN, FMLINFO, FMLLIST, FMLFOR, FMLCAP, INT, LOG, MAX, MIN, MIRR, NORMSDST, NORMSINV, PRDNOR, PRDUNI, RDNORM, RDUNIF, SQRT, XIRR

#### System Functions (~8)
FEXERR, FINDMEM, GETCOOKI, GETHEADR, GETPDS, GETUSER, MVSDYNAM, SLEEP

#### I/O Functions (~2)
PUTDDREC, CLSDDREC

### 11. StyleSheet System

```focus
ON TABLE SET STYLE *
TYPE=REPORT, FONT='ARIAL', SIZE=10, $
TYPE=HEADING, JUSTIFY=CENTER, SIZE=14, STYLE=BOLD, $
TYPE=DATA, COLUMN=SALARY, COLOR=RED, WHEN=SALARY GT 100000, $
END
```

- Controls output appearance for HTML, PDF, Excel
- Conditional formatting (WHEN clause)
- Type targets: REPORT, HEADING, FOOTING, DATA, COLUMN, TABHEADING
- Properties: FONT, SIZE, COLOR, BACKCOLOR, STYLE, JUSTIFY, etc.
- COMPOUND REPORT — multiple reports in single output document

### 12. Data Adapters

| Adapter | Data Source |
|---------|------------|
| FOCUS Native | FOCUS database files (.FOC) |
| DB2 | IBM DB2 (z/OS and LUW) |
| Oracle | Oracle RDBMS |
| SQL Server | Microsoft SQL Server |
| VSAM | z/OS VSAM files (KSDS/ESDS/RRDS) |
| IMS | IMS/DB hierarchical database |
| ADABAS | Software AG ADABAS |
| Sequential | z/OS flat files (QSAM) |
| Fixed | Fixed-length record files |
| Delimited | CSV, TAB, and other delimited files |
| IDMS | Broadcom IDMS database |
| Teradata | Teradata data warehouse |
| Hadoop/HDFS | Big data sources |
| REST/SOAP | Web services |

### 13. Mainframe Operating Environments

| Environment | Interface |
|-------------|-----------|
| MVS/TSO | Interactive via TSO — FILEDEF, DYNAM ALLOCATE |
| MVS Batch | JCL-driven batch execution via FOCJCL |
| CICS | Online transaction processing — FOCUS/CICS interface |
| CMS (VM) | VM/CMS interactive environment |

Key mainframe commands:
- `FILEDEF` — Define logical file to physical dataset (equivalent of JCL DD)
- `DYNAM ALLOCATE` / `DYNAM FREE` — Dynamic dataset allocation
- `-MVS` / `-TSO` — Issue OS commands from Dialogue Manager
- `FOCUS ONLINE` — Interactive command mode

### 14. FOCEXEC (Stored Procedures)

- FOCUS procedures are stored in PDS members as FOCEXECs
- Invoked via EX command: `EX MYREPORT`
- Can contain TABLE, MODIFY, MAINTAIN, Dialogue Manager, and SQL
- Parameterized via amper variables
- Nested invocation via -INCLUDE
- Executed by Reporting Server in WebFOCUS environment

### 15. SQL Support

```focus
SQL DB2 SELECT EMPLOYEE_ID, LAST_NAME, SALARY
FROM EMPLOYEE
WHERE DEPARTMENT = 'IT'
ORDER BY SALARY DESC ;
END
```

- Pass-through SQL to any connected SQL database
- SQL can be embedded within Dialogue Manager procedures
- Supports DB2, Oracle, SQL Server, and other RDBMS via adapters
- Result sets can feed into TABLE requests

## Current OpenMainframe Status

**No FOCUS implementation exists.** A comprehensive search across all crates returned zero matches for "FOCUS" (as 4GL language), "WebFOCUS", "FOCEXEC", "Information Builders", "Master File Descriptor", or "Dialogue Manager" in any source code context. FOCUS is listed only in the RALPH-PROMPT.md gap analysis roadmap as Batch 7.

## Gap Details

| Feature | Official FOCUS | OpenMainframe | Gap |
|---------|---------------|---------------|-----|
| TABLE request language (PRINT/SUM/COUNT/AVG/MAX/MIN) | Full | None | **Missing** |
| GRAPH request language (BAR/PIE/LINE/AREA/SCATTER) | Full | None | **Missing** |
| MODIFY transaction processing (MATCH/ON MATCH/ON NOMATCH) | Full | None | **Missing** |
| MAINTAIN interactive data entry (WINFORM, screens) | Full | None | **Missing** |
| Master File Descriptor (MFD) metadata layer | Full | None | **Missing** |
| Access File (ACF) physical data mapping | Full | None | **Missing** |
| FOCUS native database engine (.FOC files) | Full | None | **Missing** |
| Dialogue Manager (~29 commands) | Full | None | **Missing** |
| Amper variables (&/&& local/global, system, statistical) | Full | None | **Missing** |
| Data types (~10: A, I, D, F, P, Dn.m, In.m, TX, Sm.n, AnV) | Full | None | **Missing** |
| 150+ built-in functions (char, date, math, conversion) | Full | None | **Missing** |
| DEFINE FILE / COMPUTE — virtual fields | Full | None | **Missing** |
| JOIN / COMBINE / MATCH FILE — multi-source operations | Full | None | **Missing** |
| HOLD / SAVE — intermediate result storage | Full | None | **Missing** |
| StyleSheet — formatted output (HTML, PDF, Excel, CSV) | Full | None | **Missing** |
| COMPOUND REPORT — multi-report documents | Full | None | **Missing** |
| FOCEXEC stored procedures | Full | None | **Missing** |
| SQL pass-through (DB2, Oracle, etc.) | Full | None | **Missing** |
| 14+ data adapters (DB2, VSAM, IMS, ADABAS, flat files) | Full | None (partial via existing crates) | **Missing** |
| FILEDEF / DYNAM ALLOCATE — dataset access | Full | None | **Missing** |
| MVS/TSO interactive environment | Full | None (no TSO) | **Missing** |
| CICS interface | Full | Partial (CICS crate exists) | **Partial** |
| WebFOCUS (web-based reporting/dashboards) | Full | None | **Missing** |
| DECODE / DB_LOOKUP — data lookup functions | Full | None | **Missing** |
| ON ERROR handling | Full | None | **Missing** |
| Report formatting (HEADING/FOOTING/BY/ACROSS/PAGE-BREAK) | Full | None | **Missing** |

## Proposed Epic Structure

### Epic F100: FOCUS Language Parser
- **F100.1**: Source format — FOCUS request syntax (TABLE, GRAPH, MODIFY, SQL blocks)
- **F100.2**: Tokenizer — keywords (TABLE/PRINT/SUM/BY/WHERE/ON/HOLD/END), operators, literals, amper variables
- **F100.3**: TABLE request parser — verbs, qualifiers, expressions, DEFINE/COMPUTE
- **F100.4**: MODIFY/MAINTAIN parser — MATCH, ON MATCH/NOMATCH, CASE, COMPUTE
- **F100.5**: Dialogue Manager parser — -SET, -IF, -GOTO, -REPEAT, -INCLUDE, -RUN
- **Complexity**: M (Medium — English-like syntax, but multiple dialect grammars)

### Epic F101: Master File Descriptor (MFD) & Access File
- **F101.1**: MFD parser — FILENAME, SEGMENT, FIELDNAME, FORMAT, ALIAS, INDEX definitions
- **F101.2**: Data type system — A, An, AnV, I, D, Dn.m, F, P, In.m, TX, Sm.n
- **F101.3**: Segment/hierarchy model — parent-child segment relationships (S0–S9)
- **F101.4**: Access File parser — DATASET, TABLENAME, CONNECTION mapping
- **F101.5**: SUFFIX handlers — FOC, FIX, COM, TAB, COBOL file format recognition
- **Complexity**: M (Medium)

### Epic F102: TABLE Request Engine
- **F102.1**: PRINT verb — detail record output
- **F102.2**: Aggregation — SUM, COUNT, AVG, MAX, MIN, PCT, CPCT, FIRST, LAST
- **F102.3**: BY / ACROSS — sorting, grouping, cross-tabulation
- **F102.4**: WHERE / IF — record filtering and conditional inclusion
- **F102.5**: DEFINE FILE / COMPUTE — virtual field computation
- **F102.6**: ON TABLE SET — report-level options
- **F102.7**: HEADING / FOOTING / SUBHEAD / SUBFOOT — report decoration
- **F102.8**: PAGE-BREAK — page control on value change
- **Complexity**: L (Large — feature-rich report engine)

### Epic F103: GRAPH Request Engine
- **F103.1**: GRAPH dialect — reuse TABLE syntax with chart output
- **F103.2**: Chart types — BAR, VBAR, HBAR, PIE, LINE, AREA, SCATTER
- **F103.3**: Graph options — ON GRAPH SET parameters
- **F103.4**: Output targets — terminal, PostScript, HTML, image
- **Complexity**: M (Medium — leverages TABLE engine)

### Epic F104: MODIFY / MAINTAIN Transaction Engine
- **F104.1**: MODIFY — batch transaction processing, MATCH/ON MATCH/ON NOMATCH
- **F104.2**: Data validation — VALIDATE rules
- **F104.3**: MAINTAIN — interactive data entry with WINFORM screens
- **F104.4**: Flow control within MODIFY — GOTO, PERFORM, CASE, COMPUTE
- **F104.5**: Transaction file processing — DATA ON file
- **Complexity**: M (Medium)

### Epic F105: Dialogue Manager
- **F105.1**: Amper variables — local (&), global (&&), system, statistical
- **F105.2**: -SET / -DEFAULT — variable assignment
- **F105.3**: -IF / -GOTO / -label — conditional branching
- **F105.4**: -REPEAT — loop construct
- **F105.5**: -RUN — stacked request execution
- **F105.6**: -INCLUDE — external procedure invocation
- **F105.7**: -READ / -WRITE / -CLOSE — external file I/O
- **F105.8**: -TYPE / -PROMPT / -CRTFORM — terminal interaction
- **F105.9**: -MVS / -TSO / -CMS — OS command execution
- **F105.10**: Variable suffixes — .EXIST, .LENGTH, .EVAL
- **Complexity**: M (Medium)

### Epic F106: Built-in Functions
- **F106.1**: Character functions (~30: SUBSTR, POSIT, TRIM, EDIT, CTRAN, UPCASE, LOCASE, etc.)
- **F106.2**: Date/time functions (~34: DATECVT, DATEADD, DATEDIF, HDATE, TODAY, etc.)
- **F106.3**: Numeric/math functions (~25: ABS, SQRT, LOG, EXP, MAX, MIN, MOD, etc.)
- **F106.4**: Format conversion functions (~10: ATODBL, FTOA, PTOA, ITOPACK, etc.)
- **F106.5**: Data source functions (~5: DECODE, DB_LOOKUP, FIND, LAST, LOOKUP)
- **F106.6**: System/I/O functions (~10: GETUSER, GETPDS, MVSDYNAM, SLEEP, PUTDDREC)
- **F106.7**: MAINTAIN-specific functions (~24: LENGTH, LOWER, STRCMP, DAY, MONTH, YEAR, etc.)
- **Complexity**: L (Large — 150+ functions across 7 categories)

### Epic F107: Data Adapters
- **F107.1**: FOCUS native file adapter (.FOC — hierarchical FOCUS database)
- **F107.2**: Sequential/fixed file adapter (via existing dataset crate)
- **F107.3**: VSAM adapter (via existing dataset crate)
- **F107.4**: DB2 adapter (via existing DB2 crate)
- **F107.5**: IMS/DB adapter (via existing IMS crate)
- **F107.6**: SQL pass-through — arbitrary SQL to connected databases
- **F107.7**: Delimited file adapters (CSV, TAB)
- **Complexity**: M (Medium — leverages existing crates heavily)

### Epic F108: Output Formatting & HOLD
- **F108.1**: StyleSheet system — TYPE, FONT, SIZE, COLOR, JUSTIFY, WHEN conditions
- **F108.2**: HTML output — styled reports
- **F108.3**: PDF output — page-formatted reports
- **F108.4**: Excel/CSV output — tabular exports
- **F108.5**: HOLD / SAVE — intermediate result files in multiple formats
- **F108.6**: COMPOUND REPORT — multi-report documents
- **Complexity**: M (Medium)

### Epic F109: Joins & Multi-Source Operations
- **F109.1**: JOIN — cross-file joins on matching keys
- **F109.2**: COMBINE — combine records from multiple sources
- **F109.3**: MATCH FILE — match and merge operations
- **F109.4**: DEFINE FILE — virtual field definitions spanning sources
- **Complexity**: S (Small)

### Epic F110: Mainframe Environment Integration
- **F110.1**: FILEDEF — logical-to-physical file mapping
- **F110.2**: DYNAM ALLOCATE / FREE — dynamic dataset allocation
- **F110.3**: -MVS / -TSO command execution
- **F110.4**: CICS interface — FOCUS/CICS online processing
- **F110.5**: Batch JCL execution — FOCJCL procedure
- **Complexity**: M (Medium — depends on TSO/JCL being available)

## Dependencies

| Dependency | Crate / Component | Reason |
|------------|-------------------|--------|
| open-mainframe-encoding | EBCDIC support | FOCUS on mainframe uses EBCDIC |
| open-mainframe-dataset | VSAM/QSAM access | Data adapters for VSAM, sequential files |
| open-mainframe-db2 | DB2 adapter | SQL pass-through to DB2 |
| open-mainframe-ims | IMS/DB adapter | IMS data access |
| open-mainframe-sort | Sort processing | BY clause, SORT operations |
| open-mainframe-tui | 3270 terminal | MAINTAIN screens, CRTFORM |
| open-mainframe-cics | CICS integration | FOCUS/CICS online interface |
| open-mainframe-jcl | Batch execution | FOCJCL batch processing |
| TSO (not implemented, Batch 9) | Interactive env | MVS/TSO interactive mode |

**Integration note**: FOCUS's data adapter model maps well to existing OpenMainframe crates. The DB2, VSAM, IMS, QSAM, and DFSORT crates can serve as adapter backends, reducing the scope of F107 significantly.

## Complexity Estimate

| Epic | Complexity | Rationale |
|------|-----------|-----------|
| F100 (Parser) | M | Multiple dialect grammars, but English-like |
| F101 (MFD/ACF) | M | Metadata layer with segment hierarchy |
| F102 (TABLE Engine) | L | Feature-rich report engine, aggregation, formatting |
| F103 (GRAPH Engine) | M | Extends TABLE with chart output |
| F104 (MODIFY/MAINTAIN) | M | Transaction processing + interactive screens |
| F105 (Dialogue Manager) | M | Procedural control flow, variable management |
| F106 (Functions) | L | 150+ built-in functions |
| F107 (Data Adapters) | M | Leverages existing crates |
| F108 (Output/HOLD) | M | StyleSheet, multi-format output |
| F109 (Joins/Multi-Source) | S | JOIN, COMBINE, MATCH |
| F110 (Mainframe Env) | M | FILEDEF, DYNAM, TSO/CICS integration |

**Total estimated effort**: 11 epics, overall L (Large) complexity. FOCUS is a substantial 4GL with a report engine, graph engine, transaction engine, Dialogue Manager procedural layer, 150+ functions, and a multi-adapter data access model. However, its non-procedural nature and the availability of existing OpenMainframe data access crates mitigate the effort.

**Recommendation**: FOCUS is lower priority than core languages (REXX, HLASM, PL/I) and core subsystems (RACF, TSO, MQ). Implement after the core subsystem infrastructure exists. The TABLE engine (F102) and data adapters (F107) would provide the most value early, enabling report migration from mainframe FOCUS to the open platform.

## Reference Documentation

- [FOCUS Language Overview (v7.6)](https://ecl.informationbuilders.com/focus/topic/shell_76/FOCUS_OverviewOperEnv/source/topic11.htm)
- [FOCUS for Mainframe Overview (v7.3, PDF)](https://ecl.informationbuilders.com/focus/topic/shell_73/Overview73.pdf)
- [FOCUS for Mainframe New Features (v7.3, PDF)](https://ecl.informationbuilders.com/focus/topic/shell_73/s390snf73.pdf)
- [FOCUS Technical Library — ECL Portal](https://ecl.informationbuilders.com/focus/index.jsp)
- [FOCUS Using Functions — Categories (v7.6)](https://ecl.informationbuilders.com/focus/topic/shell_76/FOCUS_UsingFunctions/source/topic10.htm)
- [FOCUS Dialogue Manager Quick Reference (v7.6)](https://ecl.informationbuilders.com/focus/topic/shell_76/FOCUS_DevelopingApps/source/topic54.htm)
- [WebFOCUS 8.0 — Using Dialogue Manager](https://infocenter.informationbuilders.com/wf80/topic/pubdocs/App_Studio/Ref_Guide/source/topic80.htm)
- [WebFOCUS — Customizing Procedures with Variables](https://infocenter.informationbuilders.com/wf80/topic/pubdocs/reporting/DevelopingAppsWithWFLanguage/source/custom_proc_var.htm)
- [FOCUS — Wikipedia](https://en.wikipedia.org/wiki/FOCUS)
- [FOCUS Programming Language Resources](https://programminglanguages.info/language/focus/)
- [FOCUS Community Home Page](https://members.tripod.com/focus_site/intro.html)
