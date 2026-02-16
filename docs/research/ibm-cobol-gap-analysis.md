# IBM Enterprise COBOL for z/OS v6.4 — Gap Analysis

**Date:** 2026-02-16
**Reference:** IBM Enterprise COBOL for z/OS 6.4 Language Reference (SC27-8713-03)
**Source:** [Language Reference PDF](https://publibfp.dhe.ibm.com/epubs/pdf/igy6lr40.pdf) | [Programming Guide PDF](https://publibfp.dhe.ibm.com/epubs/pdf/igy6pg40.pdf) | [IBM Docs Online](https://www.ibm.com/docs/en/cobol-zos/6.4.0)

This document identifies features present in the official IBM Enterprise COBOL for z/OS v6.4 specification that are **not yet implemented** in the OpenMainframe COBOL compiler.

---

## 1. Missing Statements

IBM Enterprise COBOL v6.4 defines **44 Procedure Division statements**. OpenMainframe implements 33 of these (via parse dispatch plus EXEC CICS/SQL). The following **11 statements** are missing:

| # | Statement | Description | Priority |
|---|-----------|-------------|----------|
| 1 | **ALLOCATE** | Obtains dynamic storage for a based data item or an unrestricted storage area | Medium |
| 2 | **FREE** | Releases storage previously obtained by ALLOCATE | Medium |
| 3 | **ALTER** | Changes the target of a GO TO statement (obsolete but in spec) | Low |
| 4 | **ENTRY** | Establishes an alternate entry point into a called program | Medium |
| 5 | **INVOKE** | Invokes a method on an object (OO COBOL / Java interop) | Medium |
| 6 | **JSON GENERATE** | Converts COBOL data structures to JSON text | Critical |
| 7 | **JSON PARSE** | Parses JSON text and populates COBOL data items | Critical |
| 8 | **XML GENERATE** | Converts COBOL data structures to XML text | Critical |
| 9 | **XML PARSE** | Parses XML documents with event-based processing callbacks | Critical |
| 10 | **STOP literal** | STOP with a literal argument (displays and pauses; distinct from STOP RUN) | Low |
| 11 | **EXIT METHOD / EXIT FUNCTION** | Exit from a method or user-defined function (OO COBOL) | Medium |

**Notes:**
- Keywords for ALLOCATE, FREE, ALTER, ENTRY, INVOKE, JSON, XML, GENERATE, PARSE exist in the lexer but **no parse functions or AST nodes** are wired into the statement dispatch table (`for_parse_dispatch!`).
- Runtime utility code for XML GENERATE/PARSE and JSON GENERATE/PARSE exists in `crates/open-mainframe-cobol/src/xml_json/` but these are **not connected as COBOL statements** — they are standalone library functions, not compiler-parsed statement forms.
- STOP RUN is implemented but `STOP literal` (display a literal and pause) is not.

---

## 2. Missing Data Division Features

### 2.1 Missing USAGE Types

| USAGE Clause | Description | Priority |
|---|---|---|
| **DISPLAY-1** | DBCS (Double-Byte Character Set) items for Japanese/Chinese data | Medium |
| **UTF-8** (PIC U) | UTF-8 encoded data items — new in COBOL 6.3/6.4 | High |
| **OBJECT REFERENCE** | Reference to a Java or COBOL class instance (OO COBOL) | Medium |

**Currently implemented:** DISPLAY, BINARY (COMP/COMP-4), COMP-1, COMP-2, COMP-3 (PACKED-DECIMAL), COMP-5, POINTER, INDEX, FUNCTION-POINTER, PROCEDURE-POINTER, NATIONAL.

### 2.2 Missing Data Description Clauses

| Clause | Description | Priority |
|---|---|---|
| **GROUP-USAGE NATIONAL** | Declares an entire group as national (UTF-16) | Medium |
| **CODE-SET** | Specifies the character encoding for a file (FD-level) | Medium |
| **APPLY WRITE-ONLY** | Optimizes buffer usage for sequential output files | Low |
| **SAME AS** | Defines a data item with the same description as another (COBOL 2014) | Low |
| **DYNAMIC LENGTH** | Dynamic-length data items — new in COBOL 6.2+ | High |
| **VOLATILE** | Prevents compiler optimization of data item accesses | Low |
| **PROPERTY** | OO COBOL property clause for class data | Low |
| **TYPE** | User-defined type reference (TYPEDEF/STRONG) — parsing exists but semantics may be incomplete | Medium |

### 2.3 Missing Sections

| Section | Description | Priority |
|---|---|---|
| **REPORT SECTION** | Defines report descriptions (RD) for the Report Writer module | Low |
| **COMMUNICATION SECTION** | Communication descriptions (CD) — obsolete, removed in 2002 standard | Low |
| **SCREEN SECTION** | Screen definitions — not part of IBM Enterprise COBOL (Micro Focus extension) | N/A |

### 2.4 Level Number Semantics

| Feature | Status | Priority |
|---|---|---|
| **Level 01-49** | Implemented | -- |
| **Level 66 (RENAMES)** | Implemented | -- |
| **Level 77** | Needs verification — likely parsed but may lack semantic completeness | Low |
| **Level 88 (Condition names)** | Implemented | -- |

---

## 3. Missing Environment Division Features

### 3.1 Missing SPECIAL-NAMES Options

| Feature | Description | Priority |
|---|---|---|
| **ALPHABET clause** (full) | Define custom alphabets for SORT/MERGE collating sequences; keyword exists but full semantics (NATIVE, STANDARD-1, STANDARD-2, EBCDIC, user-defined) may be incomplete | High |
| **CLASS clause** (full) | Define user-defined class conditions (e.g., CLASS VALID-CHAR IS "A" THRU "Z") | High |
| **CURRENCY SIGN** (multi) | Multiple CURRENCY SIGN specifications with PICTURE SYMBOL phrase | Medium |
| **DECIMAL-POINT IS COMMA** | Swap period and comma in numeric literals — keyword exists, semantic completeness needs verification | Medium |
| **ENVIRONMENT-NAME / ENVIRONMENT-VALUE** | System-defined mnemonic names | Low |
| **SYMBOLIC CHARACTERS** (full) | Named character references; keyword exists but full semantics unclear | Medium |
| **XML-SCHEMA clause** | Associates XML schema with a file for XML PARSE validation | Medium |
| **UPSI switches** | User-Programmable Status Indicators (UPSI-0 through UPSI-7) | Low |

### 3.2 Missing REPOSITORY Paragraph

| Feature | Description | Priority |
|---|---|---|
| **REPOSITORY paragraph** | Declares class names and interfaces for OO COBOL / Java interop | Medium |
| **FUNCTION ALL INTRINSIC** | Allows calling intrinsic functions without FUNCTION keyword | High |
| **FUNCTION prototype** | User-defined function declarations (COBOL 2014) | Medium |

### 3.3 Missing I-O-CONTROL Paragraph

| Feature | Description | Priority |
|---|---|---|
| **I-O-CONTROL paragraph** | Controls file I/O buffering and resource sharing | Medium |
| **RERUN clause** | Checkpoint/restart specification | Low |
| **SAME RECORD AREA** | Share record area between files | Medium |
| **SAME SORT AREA** | Share sort work area | Low |
| **SAME SORT-MERGE AREA** | Share sort-merge work area | Low |
| **MULTIPLE FILE TAPE** | Multiple files on tape (obsolete) | Low |
| **APPLY WRITE-ONLY** | Can also appear in I-O-CONTROL | Low |

### 3.4 Missing File Control Clauses

| Feature | Description | Priority |
|---|---|---|
| **LOCK MODE** | Record locking semantics (AUTOMATIC, MANUAL, EXCLUSIVE) | Medium |
| **SHARING clause** | File sharing mode (ALL, NO OTHER, READ ONLY) | Medium |
| **PASSWORD clause** | File access password (IBM extension, largely obsolete) | Low |
| **PADDING CHARACTER** | Padding for sequential files | Low |
| **RECORD DELIMITER** | Record delimiter for line-sequential files | Low |
| **RESERVE clause** | Number of I/O buffers to reserve | Low |

---

## 4. Missing Intrinsic Functions

IBM Enterprise COBOL v6.4 defines **82 intrinsic functions**. OpenMainframe implements **64**. The following **18 functions** are missing:

| # | Function | Category | Description | Priority |
|---|----------|----------|-------------|----------|
| 1 | **CONCATENATE** | String | Join strings together | See note |
| 2 | **SUBSTITUTE** | String | Replace substrings | See note |
| 3 | **FORMATTED-CURRENT-DATE** | DateTime | Current date/time in ISO 8601 format | High |
| 4 | **FORMATTED-DATE** | DateTime | Date from integer in ISO 8601 format | High |
| 5 | **FORMATTED-DATETIME** | DateTime | Date-time from integers in ISO 8601 format | High |
| 6 | **FORMATTED-TIME** | DateTime | Time from seconds in ISO 8601 format | High |
| 7 | **INTEGER-OF-FORMATTED-DATE** | DateTime | Integer from ISO 8601 date string | High |
| 8 | **SECONDS-FROM-FORMATTED-TIME** | DateTime | Seconds from ISO 8601 time string | High |
| 9 | **TEST-FORMATTED-DATETIME** | DateTime | Validate ISO 8601 date/time string | High |
| 10 | **ULENGTH** | String/UTF-8 | Length of UTF-8 string in characters | Medium |
| 11 | **UPOS** | String/UTF-8 | Character position in UTF-8 string | Medium |
| 12 | **USUBSTR** | String/UTF-8 | Substring of UTF-8 string | Medium |
| 13 | **USUPPLEMENTARY** | String/UTF-8 | Test for supplementary UTF-8 characters | Low |
| 14 | **UVALID** | String/UTF-8 | Validate UTF-8 encoding | Medium |
| 15 | **UWIDTH** | String/UTF-8 | Width of UTF-8 character | Low |
| 16 | **UTF8STRING** | String/UTF-8 | Convert to UTF-8 string (noted in some refs) | Medium |

**Notes:**
- CONCATENATE and SUBSTITUTE are registered in the `INTRINSIC_FUNCTIONS` array but are **IBM extensions** not in the official v6.4 function list (they appear in some COBOL implementations but IBM uses STRING/UNSTRING instead). They are listed here for cross-reference but are NOT missing from IBM's spec — the project actually has *extra* functions.
- The 7 ISO 8601 date/time functions (FORMATTED-*) are a significant gap — they are part of COBOL 2002/2014 standards adopted by IBM in v6.x.
- The U* UTF-8 functions are tied to the PIC U (UTF-8) data type support.

**Adjusted count:** 14 genuinely missing functions (excluding CONCATENATE and SUBSTITUTE which are extras, not gaps).

---

## 5. Missing Language Features (Broad)

### 5.1 Nested Programs / Contained Programs

| Feature | Status | Priority |
|---|---|---|
| **Nested program definitions** | Not implemented — no support for PROGRAM-ID within a program | High |
| **COMMON clause** | Keyword exists but nested program semantics missing | High |
| **INITIAL clause** | Keyword exists but initialization semantics for nested programs missing | Medium |
| **END PROGRAM** | Keyword exists but multi-program-per-source-file not supported | High |

### 5.2 Object-Oriented COBOL

| Feature | Description | Priority |
|---|---|---|
| **CLASS-ID** | Class definition paragraph | Low |
| **FACTORY / OBJECT paragraphs** | Factory and instance object sections | Low |
| **METHOD-ID / END METHOD** | Method definitions within a class | Low |
| **INVOKE statement** | Keyword exists, no parse dispatch | Medium |
| **OBJECT REFERENCE usage** | Type exists in semantic layer only | Low |
| **Java interoperability** | Calling Java methods from COBOL | Low |
| **REPOSITORY paragraph** | Class/interface registration | Low |

### 5.3 Report Writer

| Feature | Description | Priority |
|---|---|---|
| **REPORT SECTION** | RD (Report Description) entries | Low |
| **INITIATE statement** | Begin report processing | Low |
| **GENERATE statement** | Produce report detail/summary lines (keyword exists, no statement) | Low |
| **TERMINATE statement** | End report processing | Low |
| **USE BEFORE REPORTING** | Declarative for report events | Low |
| **Report groups** | PAGE, DETAIL, CONTROL HEADING/FOOTING, etc. | Low |

### 5.4 Declaratives (USE Statements)

| Feature | Description | Priority |
|---|---|---|
| **DECLARATIVES section** | Section at start of Procedure Division for declarative procedures | High |
| **USE AFTER ERROR/EXCEPTION** | I/O error handling declaratives | High |
| **USE BEFORE REPORTING** | Report Writer declaratives | Low |
| **USE FOR DEBUGGING** | Debug declaratives (obsolete in standard, still in IBM spec) | Low |

### 5.5 Conditional Compilation

| Feature | Description | Priority |
|---|---|---|
| **>>DEFINE directive** | Define compile-time variables | High |
| **>>IF / >>ELSE / >>END-IF** | Conditional source inclusion | High |
| **>>EVALUATE / >>WHEN / >>END-EVALUATE** | Multi-branch conditional compilation | Medium |
| **>>SET directive** | Set compiler option at source level | Medium |

### 5.6 COPY / REPLACE Completeness

| Feature | Description | Priority |
|---|---|---|
| **COPY statement** | Implemented with basic REPLACING | -- |
| **REPLACE statement** | Standalone REPLACE (not just COPY REPLACING) — not implemented | High |
| **Partial-word replacement** | `REPLACING ==:PREFIX:== BY ==NEW-PREFIX:==` syntax — not implemented | High |
| **COPY REPLACING with LEADING/TRAILING** | Partial-word matching on word boundaries | High |

### 5.7 Reference Modification

| Feature | Description | Priority |
|---|---|---|
| **Basic reference modification** | `identifier(start:length)` — needs verification of implementation status | High |
| **Function reference modification** | `FUNCTION UPPER-CASE(name)(1:5)` — likely not supported | Medium |

### 5.8 Qualification (IN/OF)

| Feature | Description | Priority |
|---|---|---|
| **OF/IN qualification** | `field-name OF group-name` for disambiguation | High |
| **Multi-level qualification** | `field IN group-1 IN group-2` chains | High |
| **FILE qualification** | `field IN file-name` for file records | Medium |

### 5.9 XML and JSON Statement Conformance

| Feature | Description | Priority |
|---|---|---|
| **XML GENERATE** (statement) | Full statement with NAME, TYPE, NAMESPACE, ENCODING phrases | Critical |
| **XML PARSE** (statement) | Event-driven parsing with PROCESSING PROCEDURE | Critical |
| **JSON GENERATE** (statement) | Full statement with NAME, SUPPRESS, BOOLEAN phrases | Critical |
| **JSON PARSE** (statement) | Parsing with NAME, SUPPRESS, BOOLEAN phrases | Critical |
| **XML-CODE / XML-EVENT / XML-TEXT** | Special registers for XML PARSE processing | Critical |
| **JSON-CODE / JSON-STATUS** | Special registers for JSON processing | Critical |

**Note:** Runtime library functions for XML/JSON exist in `xml_json/` module but they are NOT wired as COBOL statements with proper AST nodes, parse dispatch, and special register handling.

### 5.10 Other Missing Features

| Feature | Description | Priority |
|---|---|---|
| **User-defined functions** | FUNCTION-ID paragraph, RETURNING clause (COBOL 2014) | Medium |
| **Function prototypes** | Declare external function signatures (new in 6.4) | Medium |
| **SAME AREA** | Share I/O buffers between files | Low |
| **MULTIPLE FILE TAPE** | Multiple files on tape volume | Low |
| **Segmentation (sections 01-99)** | Fixed/independent segments (obsolete) | Low |
| **Debug module** | USE FOR DEBUGGING, WITH DEBUGGING MODE | Low |
| **LINAGE clause** | Logical page control for WRITE ADVANCING | Medium |
| **Dynamic-length items** | DYNAMIC LENGTH clause (new in 6.2+) | High |

---

## 6. Missing Compiler Directives

### 6.1 Compiler-Directing Statements

| Statement | Description | Priority |
|---|---|---|
| **COPY** | Implemented (with basic REPLACING) | -- |
| **REPLACE** | Standalone text replacement — NOT implemented | High |
| **BASIS** | Specify a basis source member for patching with INSERT/DELETE | Low |
| **INSERT** | Insert source lines into a BASIS member | Low |
| **DELETE (compiler)** | Delete source lines from a BASIS member | Low |
| **SKIP1/SKIP2/SKIP3** | Source listing spacing control | Low |
| **EJECT** | Force page break in source listing | Low |
| **TITLE** | Set source listing title | Low |
| **CBL / PROCESS** | Specify compiler options in source | Medium |
| **SERVICE LABEL / RELOAD** | Service-related directives (IBM-specific) | Low |
| **READY TRACE / RESET TRACE** | Execution tracing (IBM extension) | Low |
| **ENTER** | Language interface (obsolete) | Low |

### 6.2 Conditional Compilation Directives

| Directive | Description | Priority |
|---|---|---|
| **>>DEFINE** | Define compile-time literal variables | High |
| **>>IF / >>ELSE / >>END-IF** | Conditional source inclusion | High |
| **>>EVALUATE / >>WHEN / >>END-EVALUATE** | Multi-branch source selection | Medium |
| **>>SET** | Set compiler options inline | Medium |
| **>>TURN** | Control compiler checking messages | Low |
| **>>CALLINTERFACE** | Specify calling convention | Low |

### 6.3 CBL/PROCESS Options (Partial List of Key Options)

IBM Enterprise COBOL v6.4 supports 90+ compiler options. Key ones that affect language semantics:

| Option | Description | Priority |
|---|---|---|
| **ARITH(COMPAT/EXTEND)** | Arithmetic precision mode (18 vs 31 digits) | High |
| **CODEPAGE** | EBCDIC code page selection | High |
| **INTDATE(ANSI/LILIAN)** | Integer date format | Medium |
| **NUMPROC(NOPFD/PFD/MIG)** | Numeric processing mode | Medium |
| **TRUNC(STD/OPT/BIN)** | Binary field truncation behavior | High |
| **NSYMBOL(NATIONAL/DBCS)** | N"..." literal interpretation | Medium |
| **SQLCCSID** | SQL CCSID behavior | Medium |
| **AFP(VOLATILE/NOVOLATILE)** | Advanced Function Printing | Low |

---

## 7. Priority Matrix

### Critical (Blocks Real-World Programs)

These features are used in the majority of production COBOL programs and their absence blocks compilation or execution of common code:

1. **JSON GENERATE / JSON PARSE statements** — Modern COBOL programs increasingly use JSON for API integration
2. **XML GENERATE / XML PARSE statements** — Core enterprise integration feature, very common
3. **REPLACE statement** — Widely used in enterprise copybook management
4. **COPY REPLACING with LEADING/TRAILING** — Standard copybook parameterization technique
5. **Declaratives / USE AFTER ERROR** — Standard I/O error handling mechanism
6. **Nested programs** — Extremely common in enterprise COBOL
7. **Conditional compilation (>>DEFINE, >>IF)** — Standard practice in multi-environment deployments

### High (Commonly Used)

Features used regularly in production code; their absence limits but doesn't block many programs:

8. **FORMATTED-* date/time functions** (7 functions) — ISO 8601 date handling
9. **Reference modification** — If not already complete, this is fundamental
10. **OF/IN qualification** — Essential for data structure disambiguation
11. **ALPHABET clause** (full semantics) — Needed for SORT/MERGE collating sequences
12. **CLASS condition** (user-defined) — Common data validation technique
13. **PIC U / UTF-8 data items** — Increasingly needed for modern Unicode handling
14. **Dynamic-length items** — New but rapidly adopted feature
15. **FUNCTION ALL INTRINSIC** — Very common in modern COBOL programs
16. **ARITH/TRUNC compiler options** — Affect numeric semantics, critical for correctness
17. **I-O-CONTROL paragraph** — Buffer management for file I/O

### Medium (Occasionally Needed)

Features used in specific domains or less common patterns:

18. **ALLOCATE / FREE statements** — Dynamic memory management
19. **ENTRY statement** — Multiple entry points
20. **INVOKE statement** — OO COBOL / Java interop
21. **OBJECT REFERENCE usage** — OO COBOL
22. **DISPLAY-1 (DBCS)** — East Asian character support
23. **CODE-SET clause** — File encoding specification
24. **LOCK MODE / SHARING** — Multi-user file access
25. **LINAGE clause** — Print file page control
26. **CBL/PROCESS statement** — Inline compiler options
27. **GROUP-USAGE NATIONAL** — National group items
28. **SAME RECORD AREA** — File buffer sharing
29. **User-defined functions** — COBOL 2014 feature
30. **U* UTF-8 intrinsic functions** (6 functions) — UTF-8 string operations

### Low (Rarely Used / Obsolete)

Features that are obsolete, rarely used, or edge cases:

31. **ALTER statement** — Obsolete (modifies GO TO targets)
32. **Report Writer** (INITIATE, GENERATE, TERMINATE) — Rarely used in modern code
33. **Communication Section** — Removed from COBOL 2002 standard
34. **BASIS / INSERT / DELETE** — Rarely used source management directives
35. **SKIP / EJECT / TITLE** — Listing control only
36. **Debug module** (USE FOR DEBUGGING) — Obsolete, replaced by debuggers
37. **Segmentation** — Obsolete memory management
38. **MULTIPLE FILE TAPE** — Obsolete tape handling
39. **UPSI switches** — Legacy indicator handling
40. **SAME SORT AREA** — Rarely specified
41. **RERUN clause** — Checkpoint/restart (rarely used)
42. **APPLY WRITE-ONLY** — Buffer optimization hint
43. **OO COBOL** (full CLASS-ID, FACTORY, OBJECT, METHOD) — Very rarely used in practice
44. **READY TRACE / RESET TRACE** — Legacy debugging
45. **ENTER statement** — Obsolete language interface
46. **VOLATILE clause** — Rarely needed

---

## Summary Statistics

| Category | IBM v6.4 Total | Implemented | Missing | Coverage |
|---|---|---|---|---|
| Procedure Division Statements | 44 | 33 | 11 | 75% |
| Intrinsic Functions | 82 | 64 (+2 extras) | 14 | 78% |
| Data Division USAGE types | ~14 | 11 | 3 | 79% |
| Environment Division features | ~25 | ~10 | ~15 | 40% |
| Compiler Directives | ~18 | 1 (COPY) | ~17 | 6% |
| Conditional Compilation | 6 | 0 | 6 | 0% |
| Language Features (broad) | -- | -- | 30+ items | -- |

**Overall assessment:** The core COBOL language (statements, intrinsics, data types) is well-covered at ~75-80%. The largest gaps are in:
1. **Compiler infrastructure** (directives, conditional compilation, REPLACE) — 0-6% coverage
2. **Environment Division** features — ~40% coverage
3. **Modern IBM extensions** (JSON/XML statements, PIC U, dynamic length) — these are the highest-impact gaps for real-world program compatibility
