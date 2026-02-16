---
version: 'v2.0'
baseVersion: 'v1.6'
date: '2026-02-16'
status: 'draft'
---

# Epics - OpenMainframe v2.0: IBM COBOL v6.4 Conformance

## Epic Overview

| Epic | Name | Stories | Complexity | Phase |
|------|------|---------|------------|-------|
| 60 | REPLACE Statement & Pseudo-Text Matching | 4 | L | 1 - Compiler Infrastructure |
| 61 | Conditional Compilation Directives | 5 | L | 1 - Compiler Infrastructure |
| 62 | CBL/PROCESS & Compiler Options | 4 | M | 1 - Compiler Infrastructure |
| 63 | JSON GENERATE & JSON PARSE Statements | 5 | L | 2 - Core Statements |
| 64 | XML GENERATE & XML PARSE Statements | 5 | XL | 2 - Core Statements |
| 65 | ALLOCATE & FREE Statements | 3 | M | 2 - Core Statements |
| 66 | ENTRY, ALTER & INVOKE Statements | 4 | M | 2 - Core Statements |
| 67 | PIC U / UTF-8 Data Type | 5 | XL | 3 - Data Division |
| 68 | DISPLAY-1, DYNAMIC LENGTH & Data Clauses | 4 | L | 3 - Data Division |
| 69 | Nested Programs | 5 | XL | 4 - Language Features |
| 70 | Declaratives Section | 4 | L | 4 - Language Features |
| 71 | COPY REPLACING LEADING/TRAILING | 3 | M | 4 - Language Features |
| 72 | OF/IN Qualification & Reference Modification | 4 | L | 4 - Language Features |
| 73 | Environment Division Completeness | 5 | L | 5 - Environment & Conformance |
| 74 | ISO 8601 Date/Time Intrinsic Functions | 3 | M | 5 - Environment & Conformance |
| 75 | UTF-8 Intrinsic Functions | 3 | M | 5 - Environment & Conformance |
| 76 | Conformance Test Suite | 4 | L | 5 - Environment & Conformance |

**Total: 17 Epics, 71 Stories**

---

## Requirements Coverage Map

| Requirement | Epic(s) |
|-------------|---------|
| FR-v2.0-001 REPLACE Statement | 60 |
| FR-v2.0-002 Conditional Compilation >>DEFINE/>>IF | 61 |
| FR-v2.0-003 Conditional Compilation >>EVALUATE | 61 |
| FR-v2.0-004 Conditional Compilation >>SET | 61 |
| FR-v2.0-005 CBL/PROCESS Statement | 62 |
| FR-v2.0-006 ARITH and TRUNC Options | 62 |
| FR-v2.0-007 JSON GENERATE | 63 |
| FR-v2.0-008 JSON PARSE | 63 |
| FR-v2.0-009 XML GENERATE | 64 |
| FR-v2.0-010 XML PARSE | 64 |
| FR-v2.0-011 ALLOCATE | 65 |
| FR-v2.0-012 FREE | 65 |
| FR-v2.0-013 ENTRY | 66 |
| FR-v2.0-014 ALTER | 66 |
| FR-v2.0-015 INVOKE | 66 |
| FR-v2.0-016 STOP Literal | 66 |
| FR-v2.0-017 PIC U / UTF-8 | 67 |
| FR-v2.0-018 DISPLAY-1 / DBCS | 68 |
| FR-v2.0-019 DYNAMIC LENGTH | 68 |
| FR-v2.0-020 GROUP-USAGE NATIONAL | 68 |
| FR-v2.0-021 CODE-SET | 68 |
| FR-v2.0-022 LINAGE | 68 |
| FR-v2.0-023 I-O-CONTROL | 73 |
| FR-v2.0-024 REPOSITORY | 73 |
| FR-v2.0-025 Full SPECIAL-NAMES | 73 |
| FR-v2.0-026 File Control Extensions | 73 |
| FR-v2.0-027 ISO 8601 Functions | 74 |
| FR-v2.0-028 UTF-8 Functions | 75 |
| FR-v2.0-029 Declaratives | 70 |
| FR-v2.0-030 Nested Programs | 69 |
| FR-v2.0-031 COPY REPLACING LEADING/TRAILING | 71 |
| FR-v2.0-032 REPLACE with Pseudo-Text | 60 |
| FR-v2.0-033 OF/IN Qualification | 72 |
| FR-v2.0-034 Reference Modification | 72 |
| FR-v2.0-035 XML/JSON Special Registers | 63, 64 |
| NFR-v2.0-001 Backward Compatibility | 76 |
| NFR-v2.0-002 Test Coverage | All |
| NFR-v2.0-003 Compilation Performance | 76 |
| NFR-v2.0-004 Conformance Test Suite | 76 |

---

## Phase 1: Compiler Infrastructure

---

## Epic 60: REPLACE Statement & Pseudo-Text Matching

**Goal:** Implement the REPLACE compiler-directing statement with full pseudo-text matching, enabling source-level text substitution independent of COPY.

**Crate:** `open-mainframe-cobol`
**FRs:** FR-v2.0-001, FR-v2.0-032

### Story 60.1: Pseudo-Text Matching Engine

As a **COBOL developer**,
I want **the compiler to match pseudo-text delimited by `==` markers**,
So that **REPLACE and COPY REPLACING can use pseudo-text operands**.

**Acceptance Criteria:**

**Given** pseudo-text `==:PREFIX:-FIELD==`
**When** matched against source token `:PREFIX:-FIELD`
**Then** the token is identified as a match

**Given** pseudo-text with multiple tokens `==MOVE A TO==`
**When** matched against source containing `MOVE A TO`
**Then** the multi-token sequence is matched

**Given** empty pseudo-text `====`
**When** used as replacement
**Then** the matched text is deleted (replaced with nothing)

**Complexity:** M
**Supports:** FR-v2.0-001, FR-v2.0-032

---

### Story 60.2: REPLACE Statement Parser

As a **COBOL developer**,
I want **the preprocessor to parse REPLACE statements**,
So that **text substitutions are applied to subsequent source lines**.

**Acceptance Criteria:**

**Given** `REPLACE ==FOO== BY ==BAR==.`
**When** preprocessor processes subsequent source
**Then** all occurrences of `FOO` are replaced with `BAR`

**Given** `REPLACE ==A== BY ==B== ==C== BY ==D==.`
**When** processed
**Then** both replacement pairs are applied

**Given** `REPLACE OFF.`
**When** processed
**Then** all active replacements are deactivated

**Complexity:** M
**Supports:** FR-v2.0-001

---

### Story 60.3: REPLACE Preprocessor Pass Integration

As a **COBOL developer**,
I want **REPLACE processing to occur after COPY expansion**,
So that **the IBM-specified processing order is followed**.

**Acceptance Criteria:**

**Given** a source with COPY followed by REPLACE
**When** preprocessed
**Then** COPY is expanded first, then REPLACE applied to the expanded text

**Given** a REPLACE active when a COPY is encountered
**When** the COPY is expanded
**Then** REPLACE applies to the expanded copybook text

**Complexity:** M
**Supports:** FR-v2.0-001

---

### Story 60.4: REPLACE Statement Tests

As a **developer**,
I want **comprehensive tests for REPLACE**,
So that **edge cases and IBM compatibility are validated**.

**Acceptance Criteria:**

**Given** REPLACE with partial word match
**When** `REPLACE ==:TAG:== BY ==WS-TAG==.` and source contains `:TAG:-NAME`
**Then** result is `WS-TAG-NAME`

**Given** multiple active REPLACE statements
**When** a second REPLACE is encountered
**Then** it replaces (not supplements) the first

**Given** nested COPY with REPLACE active
**When** processed
**Then** both COPY REPLACING and REPLACE apply correctly

**Complexity:** M
**Supports:** FR-v2.0-001, NFR-v2.0-002

---

## Epic 61: Conditional Compilation Directives

**Goal:** Implement `>>DEFINE`, `>>IF`, `>>ELSE`, `>>END-IF`, `>>EVALUATE`, `>>WHEN`, `>>END-EVALUATE`, and `>>SET` directives for conditional source inclusion.

**Crate:** `open-mainframe-cobol`
**FRs:** FR-v2.0-002, FR-v2.0-003, FR-v2.0-004

### Story 61.1: Compile-Time Variable Store

As a **COBOL developer**,
I want **`>>DEFINE` to create compile-time variables**,
So that **conditional compilation can use them**.

**Acceptance Criteria:**

**Given** `>>DEFINE MY-VAR AS 'PROD'`
**When** the variable store is queried for MY-VAR
**Then** it returns the literal value 'PROD'

**Given** `>>DEFINE MY-FLAG AS PARAMETER`
**When** no external value is provided
**Then** the variable is defined but has no value (used for `>>IF DEFINED`)

**Given** `>>DEFINE MY-VAR AS 'TEST'` after a previous definition
**When** re-defined
**Then** the variable takes the new value

**Complexity:** S
**Supports:** FR-v2.0-002

---

### Story 61.2: >>IF / >>ELSE / >>END-IF Directives

As a **COBOL developer**,
I want **conditional source inclusion with >>IF**,
So that **different code paths compile for different environments**.

**Acceptance Criteria:**

**Given** `>>IF MY-VAR = 'PROD'` with MY-VAR defined as 'PROD'
**When** preprocessed
**Then** lines between >>IF and >>ELSE (or >>END-IF) are included

**Given** `>>IF MY-VAR = 'PROD'` with MY-VAR defined as 'TEST'
**When** preprocessed
**Then** lines in the >>ELSE branch are included (if present)

**Given** `>>IF DEFINED MY-FLAG`
**When** MY-FLAG is defined
**Then** the block is included

**Given** nested `>>IF` blocks
**When** preprocessed
**Then** nesting is handled correctly up to arbitrary depth

**Complexity:** L
**Supports:** FR-v2.0-002

---

### Story 61.3: >>EVALUATE / >>WHEN / >>END-EVALUATE Directives

As a **COBOL developer**,
I want **multi-branch conditional compilation**,
So that **I can select code blocks based on compile-time variables**.

**Acceptance Criteria:**

**Given** `>>EVALUATE MY-VAR >>WHEN 'PROD' ... >>WHEN 'TEST' ... >>WHEN OTHER ... >>END-EVALUATE`
**When** MY-VAR is 'PROD'
**Then** only the PROD branch lines are included

**Given** no matching >>WHEN value
**When** >>WHEN OTHER exists
**Then** the OTHER branch is included

**Complexity:** M
**Supports:** FR-v2.0-003

---

### Story 61.4: >>SET Directive

As a **COBOL developer**,
I want **`>>SET` to change compiler options inline**,
So that **I can switch source format or options mid-file**.

**Acceptance Criteria:**

**Given** `>>SET SOURCEFORMAT"FREE"`
**When** processed
**Then** subsequent lines are lexed in free format

**Given** `>>SET SOURCEFORMAT"FIXED"`
**When** processed
**Then** subsequent lines are lexed in fixed format (columns 1-6 sequence, 7 indicator, 8-72 code)

**Complexity:** M
**Supports:** FR-v2.0-004

---

### Story 61.5: Conditional Compilation Integration and Tests

As a **developer**,
I want **the conditional compilation pass to integrate correctly with the preprocessor pipeline**,
So that **conditionals are evaluated before COPY expansion**.

**Acceptance Criteria:**

**Given** `>>IF` that excludes a COPY statement
**When** preprocessed
**Then** the COPY is not expanded

**Given** `>>DEFINE` inside a conditional block that is excluded
**When** preprocessed
**Then** the >>DEFINE does not take effect

**Given** a source using >>IF, COPY, and REPLACE together
**When** fully preprocessed
**Then** the correct pass ordering is maintained (conditionals → COPY → REPLACE)

**Complexity:** M
**Supports:** FR-v2.0-002, NFR-v2.0-002

---

## Epic 62: CBL/PROCESS & Compiler Options

**Goal:** Parse CBL/PROCESS statements and implement compiler option semantics that affect language behavior.

**Crate:** `open-mainframe-cobol`
**FRs:** FR-v2.0-005, FR-v2.0-006

### Story 62.1: CompilerOptions Struct

As a **developer**,
I want **a `CompilerOptions` struct that holds all compiler option values**,
So that **options can be set from CBL/PROCESS and referenced throughout compilation**.

**Acceptance Criteria:**

**Given** `CompilerOptions::default()`
**When** created
**Then** ARITH is COMPAT, TRUNC is STD (IBM defaults)

**Given** the struct
**When** inspected
**Then** it includes fields for ARITH, TRUNC, CODEPAGE, INTDATE, NUMPROC, NSYMBOL

**Complexity:** S
**Supports:** FR-v2.0-006

---

### Story 62.2: CBL/PROCESS Statement Parsing

As a **COBOL developer**,
I want **the compiler to parse CBL and PROCESS statements on the first line(s)**,
So that **I can specify compiler options in source code**.

**Acceptance Criteria:**

**Given** `CBL ARITH(EXTEND),TRUNC(BIN)` on line 1
**When** preprocessed
**Then** CompilerOptions has arith=Extend, trunc=Bin

**Given** `PROCESS CODEPAGE(1140),NUMPROC(PFD)`
**When** preprocessed
**Then** CompilerOptions has codepage=1140, numproc=Pfd

**Given** no CBL/PROCESS statement
**When** preprocessed
**Then** default compiler options are used

**Complexity:** M
**Supports:** FR-v2.0-005

---

### Story 62.3: ARITH Option Semantics

As a **COBOL developer**,
I want **ARITH(EXTEND) to support 31-digit precision**,
So that **large numeric computations work correctly**.

**Acceptance Criteria:**

**Given** ARITH(EXTEND) is active
**When** a COMPUTE uses numeric fields with PIC 9(31)
**Then** full 31-digit precision is maintained

**Given** ARITH(COMPAT) is active (default)
**When** numeric precision exceeds 18 digits
**Then** intermediate results are limited to 18 digits

**Complexity:** M
**Supports:** FR-v2.0-006

---

### Story 62.4: TRUNC Option Semantics

As a **COBOL developer**,
I want **TRUNC(BIN) to preserve full binary field range**,
So that **BINARY/COMP fields are not truncated to PIC size**.

**Acceptance Criteria:**

**Given** `01 WS-NUM PIC 9(4) COMP.` with TRUNC(STD)
**When** value exceeds 9999
**Then** value is truncated to PIC size (4 digits)

**Given** the same field with TRUNC(BIN)
**When** value exceeds 9999
**Then** full halfword range (0-65535) is available

**Complexity:** M
**Supports:** FR-v2.0-006

---

## Phase 2: Core Missing Statements

---

## Epic 63: JSON GENERATE & JSON PARSE Statements

**Goal:** Wire the existing JSON utility code as proper COBOL statements with AST nodes, parser dispatch, semantic analysis, and special register support.

**Crate:** `open-mainframe-cobol`, `open-mainframe-runtime`
**FRs:** FR-v2.0-007, FR-v2.0-008, FR-v2.0-035

### Story 63.1: JSON Statement AST Nodes

As a **developer**,
I want **`JsonGenerateStatement` and `JsonParseStatement` AST structs**,
So that **the parser can represent JSON statements in the syntax tree**.

**Acceptance Criteria:**

**Given** `JsonGenerateStatement` struct
**When** created
**Then** it has fields for: receiver, source, count_in, name_phrases, suppress_phrases, on_exception, not_on_exception, end_json

**Given** `JsonParseStatement` struct
**When** created
**Then** it has fields for: source, target, name_phrases, suppress_phrases, on_exception, not_on_exception, end_json

**Complexity:** M
**Supports:** FR-v2.0-007, FR-v2.0-008

---

### Story 63.2: JSON Statement Parser

As a **COBOL developer**,
I want **`JSON GENERATE` and `JSON PARSE` to be parsed**,
So that **JSON operations are expressed as COBOL statements**.

**Acceptance Criteria:**

**Given** `JSON GENERATE WS-JSON FROM WS-REC COUNT IN WS-LEN ON EXCEPTION DISPLAY 'ERROR' END-JSON`
**When** parsed
**Then** a valid `JsonGenerateStatement` AST node is produced

**Given** `JSON PARSE WS-JSON INTO WS-REC WITH DETAIL NAME WS-FIELD IS 'renamed'`
**When** parsed
**Then** a valid `JsonParseStatement` AST node is produced with name phrase

**Complexity:** L
**Supports:** FR-v2.0-007, FR-v2.0-008

---

### Story 63.3: JSON Special Registers

As a **COBOL developer**,
I want **JSON-CODE and JSON-STATUS special registers**,
So that **I can check the result of JSON operations**.

**Acceptance Criteria:**

**Given** a successful JSON GENERATE
**When** execution completes
**Then** JSON-CODE is 0

**Given** a JSON PARSE with invalid JSON
**When** execution completes
**Then** JSON-CODE is non-zero and ON EXCEPTION is taken

**Given** a reference to JSON-CODE in COBOL source
**When** the semantic analyzer resolves it
**Then** it is recognized as a special register (not a user-defined variable)

**Complexity:** M
**Supports:** FR-v2.0-035

---

### Story 63.4: JSON Statement Runtime Execution

As a **COBOL developer**,
I want **JSON GENERATE and JSON PARSE to execute at runtime**,
So that **COBOL programs can exchange JSON data**.

**Acceptance Criteria:**

**Given** `JSON GENERATE WS-JSON FROM WS-RECORD`
**When** executed with WS-RECORD containing `{NAME: "ALICE", AGE: 30}`
**Then** WS-JSON contains valid JSON text `{"NAME":"ALICE","AGE":30}`

**Given** `JSON PARSE WS-JSON INTO WS-RECORD`
**When** executed with valid JSON
**Then** WS-RECORD fields are populated from the JSON values

**Complexity:** L
**Supports:** FR-v2.0-007, FR-v2.0-008

---

### Story 63.5: JSON Statement Macro Registration

As a **developer**,
I want **JSON statements registered in the macro dispatch tables**,
So that **the existing code generation machinery handles them**.

**Acceptance Criteria:**

**Given** `for_all_statement_variants!` macro
**When** `JsonGenerate(JsonGenerateStatement)` and `JsonParse(JsonParseStatement)` are added
**Then** the Statement enum includes the new variants

**Given** `for_parse_dispatch!` macro
**When** `Json => parse_json_statement` is added
**Then** JSON keyword triggers the correct parse function

**Complexity:** S
**Supports:** FR-v2.0-007, FR-v2.0-008

---

## Epic 64: XML GENERATE & XML PARSE Statements

**Goal:** Wire the existing XML utility code as proper COBOL statements with event-driven processing for XML PARSE.

**Crate:** `open-mainframe-cobol`, `open-mainframe-runtime`
**FRs:** FR-v2.0-009, FR-v2.0-010, FR-v2.0-035

### Story 64.1: XML Statement AST Nodes

As a **developer**,
I want **`XmlGenerateStatement` and `XmlParseStatement` AST structs**,
So that **the parser can represent XML statements in the syntax tree**.

**Acceptance Criteria:**

**Given** `XmlGenerateStatement` struct
**When** created
**Then** it has fields for: receiver, source, count_in, name_phrases, type_phrases, namespace, encoding, on_exception, not_on_exception, end_xml

**Given** `XmlParseStatement` struct
**When** created
**Then** it has fields for: source, processing_procedure (paragraph name), encoding, validating, on_exception, not_on_exception, end_xml

**Complexity:** M
**Supports:** FR-v2.0-009, FR-v2.0-010

---

### Story 64.2: XML Statement Parser

As a **COBOL developer**,
I want **`XML GENERATE` and `XML PARSE` to be parsed**,
So that **XML operations are expressed as COBOL statements**.

**Acceptance Criteria:**

**Given** `XML GENERATE WS-XML FROM WS-REC COUNT IN WS-LEN ENCODING 1208 END-XML`
**When** parsed
**Then** a valid `XmlGenerateStatement` AST node is produced

**Given** `XML PARSE WS-XML PROCESSING PROCEDURE XML-HANDLER ON EXCEPTION DISPLAY 'ERR' END-XML`
**When** parsed
**Then** a valid `XmlParseStatement` AST node with processing_procedure set to "XML-HANDLER"

**Complexity:** L
**Supports:** FR-v2.0-009, FR-v2.0-010

---

### Story 64.3: XML Special Registers

As a **COBOL developer**,
I want **XML-CODE, XML-EVENT, XML-TEXT, and XML-NTEXT special registers**,
So that **I can process XML events in a PROCESSING PROCEDURE**.

**Acceptance Criteria:**

**Given** XML PARSE executing with event "START-OF-ELEMENT"
**When** the processing procedure is invoked
**Then** XML-EVENT is "START-OF-ELEMENT" and XML-TEXT is the element name

**Given** XML PARSE completing successfully
**When** done
**Then** XML-CODE is 0

**Given** XML PARSE encountering malformed XML
**When** the error occurs
**Then** XML-CODE is non-zero

**Complexity:** M
**Supports:** FR-v2.0-035

---

### Story 64.4: XML PARSE Event-Driven Runtime

As a **COBOL developer**,
I want **XML PARSE to invoke my PROCESSING PROCEDURE for each XML event**,
So that **I can handle XML content with custom logic**.

**Acceptance Criteria:**

**Given** `XML PARSE WS-XML PROCESSING PROCEDURE MY-HANDLER`
**When** executed with `<ROOT><ITEM>value</ITEM></ROOT>`
**Then** MY-HANDLER is called with events: START-OF-DOCUMENT, START-OF-ELEMENT(ROOT), START-OF-ELEMENT(ITEM), CONTENT-CHARACTERS(value), END-OF-ELEMENT(ITEM), END-OF-ELEMENT(ROOT), END-OF-DOCUMENT

**Given** MY-HANDLER sets XML-CODE to -1
**When** the handler returns
**Then** XML PARSE terminates and ON EXCEPTION is taken

**Complexity:** XL
**Supports:** FR-v2.0-010

---

### Story 64.5: XML Statement Macro Registration and Tests

As a **developer**,
I want **XML statements registered in the macro dispatch tables with comprehensive tests**,
So that **XML processing matches IBM behavior**.

**Acceptance Criteria:**

**Given** `for_all_statement_variants!` macro
**When** `XmlGenerate(XmlGenerateStatement)` and `XmlParse(XmlParseStatement)` are added
**Then** the Statement enum includes the new variants

**Given** an XML document with namespaces and attributes
**When** parsed via XML PARSE
**Then** events correctly report namespace-qualified names

**Complexity:** M
**Supports:** FR-v2.0-009, FR-v2.0-010, NFR-v2.0-002

---

## Epic 65: ALLOCATE & FREE Statements

**Goal:** Add dynamic memory allocation statements.

**Crate:** `open-mainframe-cobol`, `open-mainframe-runtime`
**FRs:** FR-v2.0-011, FR-v2.0-012

### Story 65.1: ALLOCATE/FREE AST Nodes and Parser

As a **COBOL developer**,
I want **ALLOCATE and FREE statements parsed**,
So that **I can dynamically allocate and release storage**.

**Acceptance Criteria:**

**Given** `ALLOCATE WS-PTR CHARACTERS 100 RETURNING WS-ADDR`
**When** parsed
**Then** an AllocateStatement node with characters=100 and returning=WS-ADDR is produced

**Given** `ALLOCATE MY-RECORD`
**When** parsed
**Then** an AllocateStatement node for a based data item is produced

**Given** `FREE WS-PTR`
**When** parsed
**Then** a FreeStatement node is produced

**Complexity:** M
**Supports:** FR-v2.0-011, FR-v2.0-012

---

### Story 65.2: ALLOCATE/FREE Runtime Execution

As a **COBOL developer**,
I want **ALLOCATE to obtain storage and FREE to release it**,
So that **dynamic memory management works at runtime**.

**Acceptance Criteria:**

**Given** `ALLOCATE WS-PTR CHARACTERS 100`
**When** executed
**Then** 100 bytes of storage are allocated and WS-PTR points to it

**Given** `FREE WS-PTR` after ALLOCATE
**When** executed
**Then** the storage is released and WS-PTR is set to NULL

**Given** `FREE` on a NULL pointer
**When** executed
**Then** no error occurs (no-op)

**Complexity:** M
**Supports:** FR-v2.0-011, FR-v2.0-012

---

### Story 65.3: ALLOCATE/FREE Macro Registration and Tests

As a **developer**,
I want **ALLOCATE and FREE registered in dispatch macros with tests**,
So that **the statements are fully integrated**.

**Acceptance Criteria:**

**Given** `for_parse_dispatch!` macro
**When** `Allocate => parse_allocate_statement` and `Free => parse_free_statement` are added
**Then** the keywords dispatch correctly

**Given** multiple ALLOCATE/FREE cycles
**When** tested
**Then** no memory leaks occur

**Complexity:** S
**Supports:** FR-v2.0-011, FR-v2.0-012, NFR-v2.0-002

---

## Epic 66: ENTRY, ALTER, INVOKE & STOP Literal

**Goal:** Add remaining missing Procedure Division statements.

**Crate:** `open-mainframe-cobol`, `open-mainframe-runtime`
**FRs:** FR-v2.0-013, FR-v2.0-014, FR-v2.0-015, FR-v2.0-016

### Story 66.1: ENTRY Statement

As a **COBOL developer**,
I want **ENTRY to establish alternate entry points**,
So that **a program can be called at different entry points**.

**Acceptance Criteria:**

**Given** `ENTRY 'ALTENTRY' USING WS-PARM.`
**When** parsed
**Then** an EntryStatement with literal='ALTENTRY' and using=[WS-PARM] is produced

**Given** `CALL 'ALTENTRY' USING MY-DATA`
**When** executed at runtime
**Then** execution begins at the ENTRY point in the called program

**Complexity:** M
**Supports:** FR-v2.0-013

---

### Story 66.2: ALTER Statement

As a **COBOL developer**,
I want **ALTER to change GO TO targets**,
So that **legacy programs using ALTER compile correctly**.

**Acceptance Criteria:**

**Given** `ALTER PARA-1 TO PROCEED TO PARA-2.`
**When** parsed
**Then** an AlterStatement with source=PARA-1, target=PARA-2 is produced

**Given** the ALTER at runtime
**When** the GO TO in PARA-1 executes
**Then** control transfers to PARA-2 instead of the original target

**Given** the ALTER statement
**When** semantic analysis runs
**Then** a warning diagnostic is emitted: "ALTER is an obsolete feature"

**Complexity:** M
**Supports:** FR-v2.0-014

---

### Story 66.3: INVOKE Statement Parser

As a **COBOL developer**,
I want **INVOKE to be parsed**,
So that **OO COBOL programs with method invocations can be compiled**.

**Acceptance Criteria:**

**Given** `INVOKE MY-OBJ 'methodName' USING WS-ARG RETURNING WS-RESULT`
**When** parsed
**Then** an InvokeStatement with object, method, using, returning is produced

**Given** `INVOKE SELF 'init'`
**When** parsed
**Then** an InvokeStatement with object=SELF is produced

**Complexity:** M
**Supports:** FR-v2.0-015

---

### Story 66.4: STOP Literal and Macro Registration

As a **COBOL developer**,
I want **STOP with a literal argument to display and pause**,
So that **checkpoint-style programs work correctly**.

**Acceptance Criteria:**

**Given** `STOP 'CHECKPOINT 1'.`
**When** parsed
**Then** a StopRunStatement with literal='CHECKPOINT 1' and is_run=false is produced

**Given** STOP literal at runtime
**When** executed
**Then** the literal is displayed and execution pauses

**Given** all new statements (ENTRY, ALTER, INVOKE)
**When** added to `for_all_statement_variants!` and `for_parse_dispatch!`
**Then** keywords dispatch correctly

**Complexity:** S
**Supports:** FR-v2.0-016

---

## Phase 3: Data Division Enhancements

---

## Epic 67: PIC U / UTF-8 Data Type

**Goal:** Add UTF-8 data item support with Usage::Utf8, PIC U picture clause, and related semantics.

**Crate:** `open-mainframe-cobol`, `open-mainframe-runtime`
**FRs:** FR-v2.0-017

### Story 67.1: Usage::Utf8 Enum Variant and PIC U Parsing

As a **COBOL developer**,
I want **`PIC U` accepted as a picture clause and `USAGE UTF-8` as a usage clause**,
So that **I can declare UTF-8 data items**.

**Acceptance Criteria:**

**Given** `01 WS-UTF8 PIC U(100) USAGE UTF-8.`
**When** parsed
**Then** Usage::Utf8 is set and picture category is Utf8 with size 100

**Given** `01 WS-UTF8 PIC UUUU.`
**When** parsed
**Then** picture size is 4 characters

**Complexity:** M
**Supports:** FR-v2.0-017

---

### Story 67.2: UTF-8 Data Layout Computation

As a **developer**,
I want **UTF-8 data items allocated with correct byte sizes**,
So that **storage is sufficient for worst-case UTF-8 encoding**.

**Acceptance Criteria:**

**Given** `PIC U(100)`
**When** data layout is computed
**Then** 400 bytes are allocated (4 bytes per character maximum)

**Given** a UTF-8 item at runtime
**When** actual content is ASCII only
**Then** only bytes needed are used, remaining is space-filled

**Complexity:** M
**Supports:** FR-v2.0-017

---

### Story 67.3: UTF-8 MOVE Semantics

As a **COBOL developer**,
I want **MOVE to/from UTF-8 items to handle encoding correctly**,
So that **character conversions are transparent**.

**Acceptance Criteria:**

**Given** `MOVE WS-ALPHA TO WS-UTF8`
**When** WS-ALPHA is PIC X (EBCDIC/ASCII)
**Then** characters are converted to UTF-8 encoding

**Given** `MOVE WS-UTF8 TO WS-ALPHA`
**When** WS-UTF8 contains multi-byte characters
**Then** characters that map to single-byte are converted; others produce substitution characters

**Given** `MOVE WS-NATIONAL TO WS-UTF8`
**When** WS-NATIONAL is PIC N (UTF-16)
**Then** UTF-16 to UTF-8 conversion is performed

**Complexity:** L
**Supports:** FR-v2.0-017

---

### Story 67.4: UTF-8 STRING/UNSTRING Operations

As a **COBOL developer**,
I want **STRING and UNSTRING to operate on UTF-8 character boundaries**,
So that **multi-byte characters are not split**.

**Acceptance Criteria:**

**Given** `STRING WS-UTF8-A DELIMITED SPACE INTO WS-UTF8-B`
**When** WS-UTF8-A contains multi-byte characters
**Then** concatenation respects character boundaries

**Given** `UNSTRING WS-UTF8 DELIMITED ',' INTO WS-A WS-B`
**When** WS-UTF8 contains UTF-8 text with commas
**Then** fields are split on character boundaries, not byte boundaries

**Complexity:** L
**Supports:** FR-v2.0-017

---

### Story 67.5: UTF-8 Semantic Analysis and Tests

As a **developer**,
I want **semantic analysis to validate UTF-8 data item usage**,
So that **type errors are caught at compile time**.

**Acceptance Criteria:**

**Given** `MOVE WS-COMP3 TO WS-UTF8`
**When** semantic analysis runs
**Then** a type error is reported (numeric to UTF-8)

**Given** `INSPECT WS-UTF8 TALLYING WS-COUNT FOR ALL 'X'`
**When** analyzed
**Then** INSPECT operates on UTF-8 character boundaries

**Complexity:** M
**Supports:** FR-v2.0-017, NFR-v2.0-002

---

## Epic 68: DISPLAY-1, DYNAMIC LENGTH & Data Clauses

**Goal:** Add remaining data division enhancements.

**Crate:** `open-mainframe-cobol`
**FRs:** FR-v2.0-018, FR-v2.0-019, FR-v2.0-020, FR-v2.0-021, FR-v2.0-022

### Story 68.1: DISPLAY-1 / DBCS Data Type

As a **COBOL developer**,
I want **USAGE DISPLAY-1 and PIC G for DBCS data items**,
So that **double-byte character data can be used**.

**Acceptance Criteria:**

**Given** `01 WS-DBCS PIC G(10) USAGE DISPLAY-1.`
**When** parsed
**Then** Usage::Display1 is set with 20 bytes allocated (2 per character)

**Complexity:** M
**Supports:** FR-v2.0-018

---

### Story 68.2: DYNAMIC LENGTH Clause

As a **COBOL developer**,
I want **the DYNAMIC LENGTH clause for variable-length data items**,
So that **data items can grow and shrink at runtime**.

**Acceptance Criteria:**

**Given** `01 WS-DYN PIC X DYNAMIC LENGTH.`
**When** parsed
**Then** the data item is marked as dynamic-length

**Given** `MOVE 'HELLO WORLD' TO WS-DYN`
**When** executed
**Then** WS-DYN length becomes 11

**Given** `FUNCTION LENGTH(WS-DYN)`
**When** called
**Then** returns the current dynamic length, not a fixed size

**Complexity:** L
**Supports:** FR-v2.0-019

---

### Story 68.3: GROUP-USAGE NATIONAL and CODE-SET

As a **COBOL developer**,
I want **GROUP-USAGE NATIONAL and CODE-SET clauses parsed**,
So that **national group items and file encodings are supported**.

**Acceptance Criteria:**

**Given** `01 WS-NAT-GRP GROUP-USAGE NATIONAL. 05 WS-A PIC N(10). 05 WS-B PIC N(5).`
**When** parsed
**Then** the group is marked as national with all subordinate items treated as national

**Given** `FD MY-FILE CODE-SET IS ASCII.`
**When** parsed
**Then** the file descriptor has code_set=ASCII

**Complexity:** M
**Supports:** FR-v2.0-020, FR-v2.0-021

---

### Story 68.4: LINAGE Clause

As a **COBOL developer**,
I want **the LINAGE clause for logical page control**,
So that **print files can have page headers and footers**.

**Acceptance Criteria:**

**Given** `FD PRINT-FILE LINAGE IS 60 LINES WITH FOOTING AT 55 LINES AT TOP 3 LINES AT BOTTOM 3.`
**When** parsed
**Then** linage settings are stored on the FD

**Given** `WRITE PRINT-REC AFTER ADVANCING PAGE`
**When** executed with LINAGE active
**Then** page break logic uses LINAGE values

**Complexity:** M
**Supports:** FR-v2.0-022

---

## Phase 4: Language Features

---

## Epic 69: Nested Programs

**Goal:** Support contained programs with COMMON, INITIAL, GLOBAL, and END PROGRAM.

**Crate:** `open-mainframe-cobol`, `open-mainframe-runtime`
**FRs:** FR-v2.0-030

### Story 69.1: Nested Program AST Representation

As a **developer**,
I want **the Program AST node to support contained programs**,
So that **nested programs are represented in the syntax tree**.

**Acceptance Criteria:**

**Given** a source with PROGRAM-ID OUTER containing PROGRAM-ID INNER
**When** parsed
**Then** `outer_program.contained_programs` has one entry with program_id="INNER"

**Given** `PROGRAM-ID. INNER COMMON INITIAL.`
**When** parsed
**Then** the inner program has is_common=true, is_initial=true

**Complexity:** L
**Supports:** FR-v2.0-030

---

### Story 69.2: Nested Program Parser

As a **COBOL developer**,
I want **multiple PROGRAM-ID/END PROGRAM pairs in a single source file**,
So that **nested programs compile correctly**.

**Acceptance Criteria:**

**Given** source with outer program containing two inner programs
**When** parsed
**Then** `outer_program.contained_programs.len()` is 2

**Given** deeply nested programs (3 levels)
**When** parsed
**Then** the nesting hierarchy is correctly represented

**Given** `END PROGRAM OUTER.` at the end
**When** parsed
**Then** the program-id matches the opening PROGRAM-ID

**Complexity:** L
**Supports:** FR-v2.0-030

---

### Story 69.3: GLOBAL Data Visibility

As a **COBOL developer**,
I want **GLOBAL data items visible to contained programs**,
So that **outer program data can be shared**.

**Acceptance Criteria:**

**Given** `01 WS-SHARED PIC X(10) GLOBAL.` in outer program
**When** the inner program references WS-SHARED
**Then** semantic analysis resolves it to the outer program's data item

**Given** a non-GLOBAL item in the outer program
**When** referenced from an inner program
**Then** semantic analysis reports "undefined identifier"

**Complexity:** L
**Supports:** FR-v2.0-030

---

### Story 69.4: COMMON Program Calling

As a **COBOL developer**,
I want **COMMON programs callable from sibling contained programs**,
So that **shared utility programs can be reused**.

**Acceptance Criteria:**

**Given** PROGRAM-ID UTIL COMMON inside OUTER, and PROGRAM-ID MAIN inside OUTER
**When** MAIN calls UTIL
**Then** the call resolves to the sibling COMMON program

**Given** a non-COMMON contained program
**When** a sibling attempts to CALL it
**Then** semantic analysis reports an error

**Complexity:** M
**Supports:** FR-v2.0-030

---

### Story 69.5: Nested Program Runtime Execution

As a **developer**,
I want **nested programs to execute with correct scoping and INITIAL semantics**,
So that **runtime behavior matches IBM COBOL**.

**Acceptance Criteria:**

**Given** CALL to a contained INITIAL program
**When** called a second time
**Then** WORKING-STORAGE is reinitialized to VALUE clauses

**Given** CALL to a non-INITIAL contained program
**When** called a second time
**Then** WORKING-STORAGE retains values from the previous call

**Given** GLOBAL file descriptors
**When** the inner program performs I/O
**Then** the file is shared with the outer program

**Complexity:** L
**Supports:** FR-v2.0-030

---

## Epic 70: Declaratives Section

**Goal:** Implement DECLARATIVES with USE AFTER ERROR/EXCEPTION for I/O error handling.

**Crate:** `open-mainframe-cobol`, `open-mainframe-runtime`
**FRs:** FR-v2.0-029

### Story 70.1: Declaratives AST and Parser

As a **COBOL developer**,
I want **the DECLARATIVES section parsed**,
So that **USE AFTER ERROR procedures are recognized**.

**Acceptance Criteria:**

**Given** `DECLARATIVES. FILE-ERR SECTION. USE AFTER STANDARD ERROR PROCEDURE ON MY-FILE. ... END DECLARATIVES.`
**When** parsed
**Then** procedure_division.declaratives contains one entry with use_clause targeting MY-FILE

**Given** `USE AFTER STANDARD ERROR PROCEDURE ON INPUT`
**When** parsed
**Then** the use clause applies to all INPUT files

**Complexity:** L
**Supports:** FR-v2.0-029

---

### Story 70.2: Declarative Runtime Registration

As a **developer**,
I want **declarative procedures registered in the runtime**,
So that **file I/O errors trigger the correct handler**.

**Acceptance Criteria:**

**Given** a declarative for MY-FILE
**When** runtime starts
**Then** a mapping from "MY-FILE" to the declarative section is established

**Given** a declarative for INPUT
**When** runtime starts
**Then** all files opened for INPUT are mapped to this declarative

**Complexity:** M
**Supports:** FR-v2.0-029

---

### Story 70.3: Declarative Invocation on I/O Error

As a **COBOL developer**,
I want **my USE AFTER ERROR procedure executed when a file I/O error occurs**,
So that **I can handle errors programmatically**.

**Acceptance Criteria:**

**Given** READ MY-FILE with a declarative registered
**When** a file status error occurs (e.g., end-of-file)
**Then** the declarative section's paragraphs execute before control returns

**Given** no declarative registered for a file
**When** an I/O error occurs
**Then** default error behavior applies (FILE STATUS set, no declarative invoked)

**Complexity:** L
**Supports:** FR-v2.0-029

---

### Story 70.4: Declaratives Tests

As a **developer**,
I want **comprehensive tests for declaratives**,
So that **error handling matches IBM behavior**.

**Acceptance Criteria:**

**Given** multiple declaratives for different files
**When** errors occur on each file
**Then** the correct declarative is invoked for each

**Given** a declarative that modifies FILE STATUS
**When** control returns to the I/O statement
**Then** the modified FILE STATUS is visible

**Complexity:** M
**Supports:** FR-v2.0-029, NFR-v2.0-002

---

## Epic 71: COPY REPLACING LEADING/TRAILING

**Goal:** Add partial-word replacement to COPY REPLACING.

**Crate:** `open-mainframe-cobol`
**FRs:** FR-v2.0-031

### Story 71.1: LEADING/TRAILING Keyword Parsing in REPLACING

As a **COBOL developer**,
I want **COPY REPLACING with LEADING and TRAILING keywords**,
So that **partial-word replacement works in copybooks**.

**Acceptance Criteria:**

**Given** `COPY MYBOOK REPLACING LEADING ==:TAG:== BY ==WS-==.`
**When** parsed
**Then** a replacement rule with mode=Leading is created

**Given** `COPY MYBOOK REPLACING TRAILING ==-OLD== BY ==-NEW==.`
**When** parsed
**Then** a replacement rule with mode=Trailing is created

**Complexity:** M
**Supports:** FR-v2.0-031

---

### Story 71.2: Partial-Word Matching Engine

As a **COBOL developer**,
I want **LEADING replacement to match prefixes and TRAILING to match suffixes**,
So that **copybook parameterization works correctly**.

**Acceptance Criteria:**

**Given** LEADING replacement of `:TAG:` with `WS-`
**When** copybook contains `:TAG:-FIELD-A` and `:TAG:-FIELD-B`
**Then** results are `WS-FIELD-A` and `WS-FIELD-B`

**Given** TRAILING replacement of `-OLD` with `-NEW`
**When** copybook contains `RECORD-OLD` and `FIELD-OLD`
**Then** results are `RECORD-NEW` and `FIELD-NEW`

**Given** non-matching tokens
**When** LEADING/TRAILING replacement is active
**Then** non-matching tokens are unchanged

**Complexity:** M
**Supports:** FR-v2.0-031

---

### Story 71.3: COPY REPLACING LEADING/TRAILING Tests

As a **developer**,
I want **tests verifying partial-word replacement in COPY**,
So that **edge cases are covered**.

**Acceptance Criteria:**

**Given** LEADING replacement applied to a token that exactly matches the pattern
**When** the full token is the prefix
**Then** the entire token is replaced

**Given** LEADING and standard REPLACING combined
**When** processed
**Then** both replacement types apply correctly

**Complexity:** S
**Supports:** FR-v2.0-031, NFR-v2.0-002

---

## Epic 72: OF/IN Qualification & Reference Modification

**Goal:** Implement data name qualification and complete reference modification support.

**Crate:** `open-mainframe-cobol`
**FRs:** FR-v2.0-033, FR-v2.0-034

### Story 72.1: OF/IN Qualification Parser

As a **COBOL developer**,
I want **`field-name OF group-name` qualification parsed**,
So that **ambiguous data names can be disambiguated**.

**Acceptance Criteria:**

**Given** `MOVE FIELD-A OF GROUP-1 TO WS-TARGET`
**When** parsed
**Then** the identifier node has qualifications=["GROUP-1"]

**Given** `MOVE FIELD-A IN GROUP-1 IN RECORD-1 TO WS-TARGET`
**When** parsed
**Then** the identifier node has qualifications=["GROUP-1", "RECORD-1"]

**Complexity:** M
**Supports:** FR-v2.0-033

---

### Story 72.2: Qualified Name Resolution

As a **developer**,
I want **the semantic analyzer to resolve qualified names**,
So that **the correct data item is identified**.

**Acceptance Criteria:**

**Given** two fields named FIELD-A in different groups
**When** `FIELD-A OF GROUP-1` is referenced
**Then** the field in GROUP-1 is resolved

**Given** an ambiguous reference without qualification
**When** multiple definitions exist
**Then** semantic analysis reports "ambiguous reference" with suggestions

**Given** `FIELD-A IN FILE-NAME`
**When** FILE-NAME is an FD
**Then** the field in that file's record area is resolved

**Complexity:** L
**Supports:** FR-v2.0-033

---

### Story 72.3: Reference Modification Parser

As a **COBOL developer**,
I want **reference modification `identifier(start:length)` fully supported**,
So that **substring operations work on any data item**.

**Acceptance Criteria:**

**Given** `MOVE WS-NAME(1:5) TO WS-SHORT`
**When** parsed
**Then** the identifier has ref_mod with start=1, length=5

**Given** `MOVE WS-NAME(WS-POS:)` (length omitted)
**When** parsed
**Then** length defaults to remaining characters from start

**Complexity:** M
**Supports:** FR-v2.0-034

---

### Story 72.4: Function Reference Modification

As a **COBOL developer**,
I want **reference modification on function results**,
So that **I can take substrings of function return values**.

**Acceptance Criteria:**

**Given** `MOVE FUNCTION UPPER-CASE(WS-NAME)(1:5) TO WS-SHORT`
**When** parsed
**Then** the function call has ref_mod applied to its result

**Given** `FUNCTION CURRENT-DATE(5:2)`
**When** evaluated
**Then** returns 2 characters starting at position 5 of the current date

**Complexity:** M
**Supports:** FR-v2.0-034

---

## Phase 5: Environment Division & Conformance

---

## Epic 73: Environment Division Completeness

**Goal:** Implement I-O-CONTROL, REPOSITORY, full SPECIAL-NAMES, and file control extensions.

**Crate:** `open-mainframe-cobol`
**FRs:** FR-v2.0-023, FR-v2.0-024, FR-v2.0-025, FR-v2.0-026

### Story 73.1: I-O-CONTROL Paragraph

As a **COBOL developer**,
I want **the I-O-CONTROL paragraph parsed**,
So that **SAME RECORD AREA and APPLY WRITE-ONLY are supported**.

**Acceptance Criteria:**

**Given** `I-O-CONTROL. SAME RECORD AREA FOR FILE-A FILE-B.`
**When** parsed
**Then** IoControlParagraph has same_record_areas containing [["FILE-A", "FILE-B"]]

**Given** `APPLY WRITE-ONLY ON FILE-C.`
**When** parsed
**Then** apply_write_only contains ["FILE-C"]

**Complexity:** M
**Supports:** FR-v2.0-023

---

### Story 73.2: REPOSITORY Paragraph and FUNCTION ALL INTRINSIC

As a **COBOL developer**,
I want **the REPOSITORY paragraph parsed with FUNCTION ALL INTRINSIC**,
So that **I can call intrinsic functions without the FUNCTION keyword**.

**Acceptance Criteria:**

**Given** `REPOSITORY. FUNCTION ALL INTRINSIC.`
**When** parsed
**Then** repository.function_all_intrinsic is true

**Given** FUNCTION ALL INTRINSIC is active
**When** `MOVE UPPER-CASE(WS-NAME) TO WS-UPPER` appears (no FUNCTION keyword)
**Then** semantic analysis resolves UPPER-CASE as an intrinsic function call

**Given** `REPOSITORY. FUNCTION TRIM UPPER-CASE.`
**When** parsed
**Then** only TRIM and UPPER-CASE can be called without FUNCTION keyword

**Complexity:** L
**Supports:** FR-v2.0-024

---

### Story 73.3: Full SPECIAL-NAMES — ALPHABET and CLASS

As a **COBOL developer**,
I want **ALPHABET and CLASS clauses in SPECIAL-NAMES**,
So that **custom collating sequences and user-defined class conditions work**.

**Acceptance Criteria:**

**Given** `ALPHABET MY-ALPHA IS STANDARD-1.`
**When** parsed
**Then** an AlphabetClause with type=Standard1 is stored

**Given** `CLASS VALID-CHAR IS 'A' THRU 'Z' '0' THRU '9' '-'.`
**When** parsed
**Then** a ClassClause with ranges is stored

**Given** `IF WS-FIELD IS VALID-CHAR`
**When** evaluated
**Then** returns true only if all characters match the class definition

**Complexity:** L
**Supports:** FR-v2.0-025

---

### Story 73.4: SPECIAL-NAMES — CURRENCY, DECIMAL-POINT, SYMBOLIC CHARACTERS

As a **COBOL developer**,
I want **CURRENCY SIGN, DECIMAL-POINT IS COMMA, and SYMBOLIC CHARACTERS**,
So that **locale-specific formatting works**.

**Acceptance Criteria:**

**Given** `CURRENCY SIGN IS '€' WITH PICTURE SYMBOL '€'.`
**When** parsed
**Then** the currency sign is '€' for PIC editing

**Given** `DECIMAL-POINT IS COMMA.`
**When** active
**Then** numeric literals use comma as decimal separator and period as thousands separator

**Given** `SYMBOLIC CHARACTERS MY-TAB IS 10.`
**When** WS-FIELD contains MY-TAB
**Then** it references the character at ordinal position 10

**Complexity:** M
**Supports:** FR-v2.0-025

---

### Story 73.5: File Control Extensions

As a **COBOL developer**,
I want **LOCK MODE, SHARING, RESERVE, PADDING CHARACTER, and RECORD DELIMITER parsed**,
So that **file access properties are fully configurable**.

**Acceptance Criteria:**

**Given** `SELECT MY-FILE ASSIGN TO MYDD LOCK MODE IS AUTOMATIC SHARING WITH ALL OTHER RESERVE 3.`
**When** parsed
**Then** file control entry has lock_mode=Automatic, sharing=AllOther, reserve=3

**Given** `PADDING CHARACTER IS '*'.`
**When** parsed
**Then** padding_character is '*'

**Complexity:** M
**Supports:** FR-v2.0-026

---

## Epic 74: ISO 8601 Date/Time Intrinsic Functions

**Goal:** Implement the 7 FORMATTED-* date/time functions and SECONDS-FROM-FORMATTED-TIME.

**Crate:** `open-mainframe-cobol`
**FRs:** FR-v2.0-027

### Story 74.1: FORMATTED-CURRENT-DATE and FORMATTED-DATE/TIME Functions

As a **COBOL developer**,
I want **FORMATTED-CURRENT-DATE, FORMATTED-DATE, and FORMATTED-TIME functions**,
So that **I can generate ISO 8601 date/time strings**.

**Acceptance Criteria:**

**Given** `FUNCTION FORMATTED-CURRENT-DATE("YYYY-MM-DDThh:mm:ss")`
**When** evaluated
**Then** returns current date/time in ISO 8601 format

**Given** `FUNCTION FORMATTED-DATE("YYYY-MM-DD", 150000)`
**When** evaluated
**Then** returns the date corresponding to integer 150000

**Given** `FUNCTION FORMATTED-TIME("hh:mm:ss", 43200)`
**When** evaluated
**Then** returns "12:00:00"

**Complexity:** M
**Supports:** FR-v2.0-027

---

### Story 74.2: FORMATTED-DATETIME, INTEGER-OF-FORMATTED-DATE, SECONDS-FROM-FORMATTED-TIME

As a **COBOL developer**,
I want **the remaining ISO 8601 conversion functions**,
So that **I can convert between integers and ISO 8601 strings**.

**Acceptance Criteria:**

**Given** `FUNCTION FORMATTED-DATETIME("YYYY-MM-DDThh:mm:ss", 150000, 43200)`
**When** evaluated
**Then** returns combined date and time in ISO 8601

**Given** `FUNCTION INTEGER-OF-FORMATTED-DATE("YYYY-MM-DD", "2026-02-16")`
**When** evaluated
**Then** returns the integer date value

**Given** `FUNCTION SECONDS-FROM-FORMATTED-TIME("hh:mm:ss", "12:30:45")`
**When** evaluated
**Then** returns 45045

**Complexity:** M
**Supports:** FR-v2.0-027

---

### Story 74.3: TEST-FORMATTED-DATETIME and Registration

As a **COBOL developer**,
I want **TEST-FORMATTED-DATETIME for validation and all functions registered**,
So that **ISO 8601 strings can be validated**.

**Acceptance Criteria:**

**Given** `FUNCTION TEST-FORMATTED-DATETIME("YYYY-MM-DD", "2026-02-30")`
**When** evaluated
**Then** returns non-zero (invalid date)

**Given** `FUNCTION TEST-FORMATTED-DATETIME("YYYY-MM-DD", "2026-02-16")`
**When** evaluated
**Then** returns 0 (valid)

**Given** all 7 functions + SECONDS-FROM-FORMATTED-TIME
**When** added to INTRINSIC_FUNCTIONS array
**Then** they are callable in COBOL programs

**Complexity:** M
**Supports:** FR-v2.0-027, NFR-v2.0-002

---

## Epic 75: UTF-8 Intrinsic Functions

**Goal:** Implement the 6 U* UTF-8 string functions.

**Crate:** `open-mainframe-cobol`
**FRs:** FR-v2.0-028
**Dependencies:** Epic 67 (PIC U support)

### Story 75.1: ULENGTH, UPOS, USUBSTR

As a **COBOL developer**,
I want **ULENGTH, UPOS, and USUBSTR for UTF-8 string operations**,
So that **I can work with UTF-8 data at the character level**.

**Acceptance Criteria:**

**Given** `FUNCTION ULENGTH(WS-UTF8)` with WS-UTF8 containing "Héllo" (6 bytes, 5 chars)
**When** evaluated
**Then** returns 5

**Given** `FUNCTION UPOS(WS-UTF8, 3)` with "Héllo"
**When** evaluated
**Then** returns the byte offset of the 3rd character

**Given** `FUNCTION USUBSTR(WS-UTF8, 2, 3)` with "Héllo"
**When** evaluated
**Then** returns "éll"

**Complexity:** M
**Supports:** FR-v2.0-028

---

### Story 75.2: UVALID, UWIDTH, USUPPLEMENTARY

As a **COBOL developer**,
I want **UVALID, UWIDTH, and USUPPLEMENTARY for UTF-8 validation**,
So that **I can verify and measure UTF-8 data**.

**Acceptance Criteria:**

**Given** `FUNCTION UVALID(WS-UTF8)` with valid UTF-8
**When** evaluated
**Then** returns 0

**Given** `FUNCTION UVALID(WS-UTF8)` with invalid bytes at position 5
**When** evaluated
**Then** returns 5 (position of first invalid byte)

**Given** `FUNCTION UWIDTH(WS-UTF8, 1)` with a CJK character at position 1
**When** evaluated
**Then** returns 2 (double-width character)

**Given** `FUNCTION USUPPLEMENTARY(WS-UTF8, 3)` with emoji at position 3
**When** evaluated
**Then** returns 1 (supplementary plane character)

**Complexity:** M
**Supports:** FR-v2.0-028

---

### Story 75.3: UTF-8 Function Registration and Tests

As a **developer**,
I want **all U* functions registered in INTRINSIC_FUNCTIONS with tests**,
So that **UTF-8 operations are fully integrated**.

**Acceptance Criteria:**

**Given** all 6 U* functions
**When** added to the INTRINSIC_FUNCTIONS array
**Then** they are callable with `FUNCTION ULENGTH(...)` syntax

**Given** edge cases (empty string, ASCII-only, 4-byte characters)
**When** tested
**Then** all functions return correct results

**Complexity:** S
**Supports:** FR-v2.0-028, NFR-v2.0-002

---

## Epic 76: Conformance Test Suite

**Goal:** Create a systematic test suite validating IBM COBOL v6.4 compatibility across all implemented features.

**Crate:** workspace-level tests
**FRs:** NFR-v2.0-001, NFR-v2.0-003, NFR-v2.0-004

### Story 76.1: Statement Conformance Tests

As a **developer**,
I want **conformance tests for all 44 Procedure Division statements**,
So that **each statement matches IBM COBOL v6.4 behavior**.

**Acceptance Criteria:**

**Given** a test directory `tests/conformance/statements/`
**When** tests are run
**Then** each of the 44 statements (33 existing + 11 new) has at least one conformance test

**Given** each test
**When** examined
**Then** it verifies parser output, semantic analysis, and runtime execution

**Complexity:** L
**Supports:** NFR-v2.0-004

---

### Story 76.2: Data Division Conformance Tests

As a **developer**,
I want **conformance tests for all data types, USAGE clauses, and data description clauses**,
So that **data handling matches IBM COBOL v6.4**.

**Acceptance Criteria:**

**Given** a test directory `tests/conformance/data/`
**When** tests are run
**Then** each USAGE type, PIC category, and data clause has conformance tests

**Given** MOVE between different USAGE types
**When** tested
**Then** conversion semantics match IBM COBOL

**Complexity:** L
**Supports:** NFR-v2.0-004

---

### Story 76.3: Backward Compatibility Regression Tests

As a **developer**,
I want **regression tests confirming existing programs compile and run identically**,
So that **v2.0 changes don't break anything**.

**Acceptance Criteria:**

**Given** all existing test programs
**When** compiled and executed with v2.0
**Then** output is identical to v1.6

**Given** `cargo test --workspace`
**When** run
**Then** zero test failures

**Complexity:** M
**Supports:** NFR-v2.0-001

---

### Story 76.4: Performance Benchmark

As a **developer**,
I want **compilation performance benchmarks**,
So that **we can verify < 5% regression**.

**Acceptance Criteria:**

**Given** a set of representative COBOL programs
**When** compilation is benchmarked before and after v2.0
**Then** compilation time regression is < 5%

**Given** the preprocessor pipeline (4 passes vs 1)
**When** benchmarked
**Then** the additional passes add < 10% to preprocessing time

**Complexity:** M
**Supports:** NFR-v2.0-003

---

## Implementation Order

### Phase 1: Compiler Infrastructure (Epics 60-62)
1. Epic 60: REPLACE Statement
2. Epic 61: Conditional Compilation
3. Epic 62: CBL/PROCESS & Options

### Phase 2: Core Missing Statements (Epics 63-66)
4. Epic 63: JSON Statements
5. Epic 64: XML Statements
6. Epic 65: ALLOCATE/FREE
7. Epic 66: ENTRY/ALTER/INVOKE

### Phase 3: Data Division Enhancements (Epics 67-68)
8. Epic 67: PIC U / UTF-8
9. Epic 68: DISPLAY-1/DYNAMIC LENGTH/Data Clauses

### Phase 4: Language Features (Epics 69-72)
10. Epic 69: Nested Programs
11. Epic 70: Declaratives
12. Epic 71: COPY REPLACING LEADING/TRAILING
13. Epic 72: Qualification & Reference Modification

### Phase 5: Environment & Conformance (Epics 73-76)
14. Epic 73: Environment Division
15. Epic 74: ISO 8601 Functions
16. Epic 75: UTF-8 Functions
17. Epic 76: Conformance Test Suite
