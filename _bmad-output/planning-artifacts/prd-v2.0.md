---
version: 'v2.0'
baseVersion: 'v1.6'
date: '2026-02-16'
status: 'draft'
---

# Product Requirements Document - v2.0: IBM COBOL v6.4 Conformance

_Addendum to the base PRD. All existing requirements remain in effect._

## Overview

v2.0 targets full conformance with IBM Enterprise COBOL for z/OS v6.4 (Language Reference SC27-8713-03). The gap analysis (`docs/research/ibm-cobol-gap-analysis.md`) identified significant gaps in compiler directives, environment division features, and modern IBM extensions. This version closes those gaps to enable compilation and execution of real-world enterprise COBOL programs without modification.

## Problem Statement

1. **Compiler directives** have 0-6% coverage — REPLACE, conditional compilation (`>>DEFINE`, `>>IF`), and `CBL/PROCESS` are entirely missing
2. **Environment Division** features are at ~40% coverage — I-O-CONTROL, REPOSITORY, full SPECIAL-NAMES clauses, and file control extensions are absent
3. **Procedure Division statements** are at 75% — 11 statements missing including critical JSON GENERATE/PARSE and XML GENERATE/PARSE
4. **Intrinsic functions** are at 78% — 14 functions missing including the ISO 8601 FORMATTED-* family
5. **Core language features** like nested programs, declaratives (USE AFTER ERROR), REPLACE statement, and COPY REPLACING with LEADING/TRAILING are not implemented
6. **Modern data types** — PIC U (UTF-8) and DYNAMIC LENGTH items from COBOL 6.2+ are absent

These gaps prevent compilation of the majority of production enterprise COBOL programs that rely on conditional compilation, JSON/XML integration, nested program structures, or declarative error handling.

## Functional Requirements

### Compiler Infrastructure

#### FR-v2.0-001: REPLACE Statement

The compiler shall support the REPLACE statement for source text replacement independent of COPY.

**Acceptance:** `REPLACE ==:TAG:== BY ==WS-TAG==.` applied to subsequent source lines produces correct token substitution. `REPLACE OFF.` deactivates replacements.

#### FR-v2.0-002: Conditional Compilation — >>DEFINE and >>IF

The compiler shall support `>>DEFINE`, `>>IF`, `>>ELSE`, `>>END-IF` conditional compilation directives.

**Acceptance:** A source file with `>>DEFINE ENV AS 'PROD'` followed by `>>IF ENV = 'PROD'` conditionally includes/excludes source lines correctly.

#### FR-v2.0-003: Conditional Compilation — >>EVALUATE

The compiler shall support `>>EVALUATE`, `>>WHEN`, `>>END-EVALUATE` multi-branch conditional compilation.

**Acceptance:** Source with `>>EVALUATE ENV >>WHEN 'PROD' ... >>WHEN 'TEST' ... >>END-EVALUATE` selects the correct branch.

#### FR-v2.0-004: Conditional Compilation — >>SET

The compiler shall support the `>>SET` directive to set compiler options inline.

**Acceptance:** `>>SET SOURCEFORMAT"FREE"` changes source format processing for subsequent lines.

#### FR-v2.0-005: CBL/PROCESS Statement

The compiler shall parse CBL and PROCESS statements to set compiler options from source.

**Acceptance:** `CBL ARITH(EXTEND),TRUNC(BIN)` on the first line sets the corresponding compiler options.

#### FR-v2.0-006: Compiler Option Semantics — ARITH and TRUNC

The compiler shall implement ARITH(COMPAT/EXTEND) for arithmetic precision and TRUNC(STD/OPT/BIN) for binary field truncation.

**Acceptance:** With `ARITH(EXTEND)`, numeric precision supports up to 31 digits. With `TRUNC(BIN)`, BINARY fields are not truncated to PIC size.

### Missing Statements

#### FR-v2.0-007: JSON GENERATE Statement

The compiler shall parse and execute `JSON GENERATE` with NAME, SUPPRESS, and COUNT IN phrases.

**Acceptance:** `JSON GENERATE WS-JSON FROM WS-RECORD COUNT IN WS-LEN` produces valid JSON text from a COBOL group item. JSON-CODE special register is set to 0 on success.

#### FR-v2.0-008: JSON PARSE Statement

The compiler shall parse and execute `JSON PARSE` with NAME and SUPPRESS phrases.

**Acceptance:** `JSON PARSE WS-JSON INTO WS-RECORD` populates COBOL data items from JSON text. JSON-CODE and JSON-STATUS special registers reflect parse results.

#### FR-v2.0-009: XML GENERATE Statement

The compiler shall parse and execute `XML GENERATE` with NAME, TYPE, NAMESPACE, ENCODING, and COUNT IN phrases.

**Acceptance:** `XML GENERATE WS-XML FROM WS-RECORD COUNT IN WS-LEN` produces valid XML from a COBOL group item. XML-CODE special register is set to 0 on success.

#### FR-v2.0-010: XML PARSE Statement

The compiler shall parse and execute `XML PARSE` with PROCESSING PROCEDURE, ENCODING, and VALIDATING phrases.

**Acceptance:** `XML PARSE WS-XML PROCESSING PROCEDURE XML-HANDLER` invokes the handler paragraph for each XML event with XML-EVENT, XML-TEXT, XML-CODE special registers set correctly.

#### FR-v2.0-011: ALLOCATE Statement

The compiler shall parse and execute `ALLOCATE` for dynamic storage allocation.

**Acceptance:** `ALLOCATE WS-PTR CHARACTERS 100` obtains storage and sets the pointer. `ALLOCATE MY-RECORD` allocates storage for a based data item.

#### FR-v2.0-012: FREE Statement

The compiler shall parse and execute `FREE` to release dynamically allocated storage.

**Acceptance:** `FREE WS-PTR` releases storage previously obtained by ALLOCATE.

#### FR-v2.0-013: ENTRY Statement

The compiler shall parse and execute `ENTRY` to establish alternate entry points.

**Acceptance:** `ENTRY 'ALTENTRY' USING WS-PARM` creates an alternate entry point that can be invoked via CALL 'ALTENTRY'.

#### FR-v2.0-014: ALTER Statement

The compiler shall parse and execute `ALTER` to change GO TO targets.

**Acceptance:** `ALTER PARA-1 TO PROCEED TO PARA-2` modifies the target of the GO TO in PARA-1. A warning diagnostic is emitted noting this is an obsolete feature.

#### FR-v2.0-015: INVOKE Statement

The compiler shall parse `INVOKE` for method invocation on objects.

**Acceptance:** `INVOKE MY-OBJ 'methodName' USING WS-ARG RETURNING WS-RESULT` parses without error and produces a valid AST node.

#### FR-v2.0-016: STOP Literal

The compiler shall support `STOP literal` (distinct from STOP RUN).

**Acceptance:** `STOP 'CHECKPOINT REACHED'` displays the literal and pauses execution.

### Data Division Enhancements

#### FR-v2.0-017: PIC U / UTF-8 Data Items

The compiler shall support USAGE UTF-8 and PIC U picture clause for UTF-8 encoded data items.

**Acceptance:** `01 WS-UTF8 PIC U(100) USAGE UTF-8.` is parsed and allocated. MOVE, STRING, UNSTRING operate on UTF-8 character boundaries.

#### FR-v2.0-018: DISPLAY-1 / DBCS Data Items

The compiler shall support USAGE DISPLAY-1 for DBCS (Double-Byte Character Set) data items.

**Acceptance:** `01 WS-DBCS PIC G(10) USAGE DISPLAY-1.` is parsed and allocated with 2 bytes per character.

#### FR-v2.0-019: DYNAMIC LENGTH Items

The compiler shall support the DYNAMIC LENGTH clause for variable-length data items.

**Acceptance:** `01 WS-DYN PIC X DYNAMIC LENGTH.` is parsed and allocates a dynamically-sized data item. LENGTH OF returns the current length.

#### FR-v2.0-020: GROUP-USAGE NATIONAL

The compiler shall support GROUP-USAGE NATIONAL on group items.

**Acceptance:** `01 WS-NAT-GRP GROUP-USAGE NATIONAL.` treats the entire group as national (UTF-16) data.

#### FR-v2.0-021: CODE-SET Clause

The compiler shall support the CODE-SET clause on FD entries.

**Acceptance:** `FD MY-FILE CODE-SET IS ASCII.` specifies the character encoding for file data.

#### FR-v2.0-022: LINAGE Clause

The compiler shall support the LINAGE clause for logical page control.

**Acceptance:** `FD PRINT-FILE LINAGE IS 60 LINES WITH FOOTING AT 55 LINES AT TOP 3 LINES AT BOTTOM 3.` controls page formatting for WRITE ADVANCING.

### Environment Division Completeness

#### FR-v2.0-023: I-O-CONTROL Paragraph

The compiler shall parse the I-O-CONTROL paragraph with SAME RECORD AREA, APPLY WRITE-ONLY, and RERUN clauses.

**Acceptance:** `I-O-CONTROL. SAME RECORD AREA FOR FILE-A FILE-B.` shares the record area between files.

#### FR-v2.0-024: REPOSITORY Paragraph

The compiler shall parse the REPOSITORY paragraph with FUNCTION ALL INTRINSIC and function prototype declarations.

**Acceptance:** `REPOSITORY. FUNCTION ALL INTRINSIC.` allows intrinsic functions to be called without the FUNCTION keyword.

#### FR-v2.0-025: Full SPECIAL-NAMES

The compiler shall support complete SPECIAL-NAMES options: ALPHABET (NATIVE, STANDARD-1, STANDARD-2, EBCDIC, user-defined), CLASS (user-defined conditions), CURRENCY SIGN with PICTURE SYMBOL, DECIMAL-POINT IS COMMA, and SYMBOLIC CHARACTERS.

**Acceptance:** `SPECIAL-NAMES. ALPHABET MY-ALPHA IS STANDARD-1. CLASS VALID-CHAR IS 'A' THRU 'Z' '0' THRU '9'. DECIMAL-POINT IS COMMA.` All clauses parse and affect program behavior.

#### FR-v2.0-026: File Control Extensions

The compiler shall support LOCK MODE, SHARING, RESERVE, PADDING CHARACTER, and RECORD DELIMITER clauses in file control entries.

**Acceptance:** `SELECT MY-FILE ASSIGN TO MYDD LOCK MODE IS AUTOMATIC SHARING WITH ALL OTHER.` parses and sets file access properties.

### Missing Intrinsic Functions

#### FR-v2.0-027: ISO 8601 Date/Time Functions

The compiler shall implement 7 FORMATTED-* intrinsic functions: FORMATTED-CURRENT-DATE, FORMATTED-DATE, FORMATTED-DATETIME, FORMATTED-TIME, INTEGER-OF-FORMATTED-DATE, SECONDS-FROM-FORMATTED-TIME, TEST-FORMATTED-DATETIME.

**Acceptance:** `FUNCTION FORMATTED-CURRENT-DATE("YYYY-MM-DDThh:mm:ss")` returns the current date/time in ISO 8601 format.

#### FR-v2.0-028: UTF-8 Intrinsic Functions

The compiler shall implement 6 U* intrinsic functions: ULENGTH, UPOS, USUBSTR, UVALID, UWIDTH, USUPPLEMENTARY.

**Acceptance:** `FUNCTION ULENGTH(WS-UTF8)` returns the number of UTF-8 characters (not bytes) in the data item.

### Language Feature Gaps

#### FR-v2.0-029: Declaratives Section

The compiler shall support the DECLARATIVES section with USE AFTER STANDARD ERROR/EXCEPTION procedures.

**Acceptance:** `DECLARATIVES. FILE-ERROR SECTION. USE AFTER STANDARD ERROR PROCEDURE ON MY-FILE. ... END DECLARATIVES.` The error handler paragraph executes when a file I/O error occurs on MY-FILE.

#### FR-v2.0-030: Nested Programs

The compiler shall support nested (contained) programs with COMMON and INITIAL clauses, GLOBAL data visibility, and END PROGRAM markers.

**Acceptance:** A source file containing `PROGRAM-ID. OUTER. ... PROGRAM-ID. INNER COMMON. ... END PROGRAM INNER. END PROGRAM OUTER.` compiles and the outer program can CALL the inner program. GLOBAL data items are visible to contained programs.

#### FR-v2.0-031: COPY REPLACING with LEADING/TRAILING

The COPY statement shall support REPLACING with LEADING and TRAILING partial-word matching.

**Acceptance:** `COPY MYBOOK REPLACING LEADING ==:PREFIX:== BY ==WS-==.` replaces `:PREFIX:-FIELD` with `WS-FIELD` in the copybook text.

#### FR-v2.0-032: REPLACE Statement with Pseudo-Text

The REPLACE statement shall support `==pseudo-text-1== BY ==pseudo-text-2==` syntax.

**Acceptance:** `REPLACE ==SECTION-A== BY ==SECTION-B==.` replaces all occurrences in subsequent source. `REPLACE OFF.` deactivates.

#### FR-v2.0-033: OF/IN Qualification

The compiler shall support data name qualification with OF and IN keywords, including multi-level chains and FILE qualification.

**Acceptance:** `MOVE FIELD-A OF GROUP-1 OF RECORD-1 TO WS-TARGET` resolves the correct data item when FIELD-A exists in multiple groups.

#### FR-v2.0-034: Reference Modification Completeness

The compiler shall support reference modification on identifiers and function results.

**Acceptance:** `MOVE WS-NAME(1:5) TO WS-SHORT` and `MOVE FUNCTION UPPER-CASE(WS-NAME)(1:5) TO WS-SHORT` both compile and execute correctly.

### Modern IBM Extensions

#### FR-v2.0-035: XML/JSON Special Registers

The runtime shall provide XML-CODE, XML-EVENT, XML-TEXT, XML-NTEXT, JSON-CODE, and JSON-STATUS special registers.

**Acceptance:** During XML PARSE processing, XML-EVENT contains the event type (e.g., "START-OF-ELEMENT"), XML-TEXT contains the element text, and XML-CODE is 0 for success.

## Non-Functional Requirements

### NFR-v2.0-001: Backward Compatibility

All existing COBOL programs that compile and run under v1.6 shall continue to compile and run identically under v2.0.

**Acceptance:** `cargo test --workspace` passes with zero failures after all v2.0 changes.

### NFR-v2.0-002: Test Coverage

Each new feature shall include: (a) parser tests verifying AST construction, (b) semantic analysis tests verifying type checking and validation, (c) runtime/interpreter tests verifying execution behavior.

**Acceptance:** Each FR-v2.0-NNN has at least 3 test cases covering happy path, error path, and edge cases.

### NFR-v2.0-003: Compilation Performance

v2.0 changes shall not degrade compilation time for existing programs by more than 5%.

**Acceptance:** Benchmark compilation of representative COBOL programs shows < 5% regression.

### NFR-v2.0-004: Conformance Test Suite

A conformance test suite shall validate IBM COBOL v6.4 compatibility across all implemented features.

**Acceptance:** Test suite in `tests/conformance/` covers each Procedure Division statement, each Data Division clause, each Environment Division feature, and each intrinsic function with IBM-compatible behavior.

## Out of Scope

The following features are classified as Low priority per the gap analysis and deferred to future versions:

- **Object-Oriented COBOL** — CLASS-ID, FACTORY, OBJECT, METHOD-ID, Java interoperability
- **Report Writer** — REPORT SECTION, INITIATE, GENERATE, TERMINATE, USE BEFORE REPORTING
- **Communication Section** — Removed from COBOL 2002 standard, obsolete
- **Debug Module** — USE FOR DEBUGGING, WITH DEBUGGING MODE (replaced by modern debuggers)
- **Segmentation** — Fixed/independent segments (obsolete memory management)
- **BASIS/INSERT/DELETE** — Rarely used source management directives
- **SKIP/EJECT/TITLE** — Listing control directives (cosmetic only)
- **READY TRACE / RESET TRACE** — Legacy execution tracing
- **ENTER** — Obsolete language interface statement
- **VOLATILE clause** — Rarely needed optimization hint
- **UPSI switches** — Legacy indicator handling
- **MULTIPLE FILE TAPE** — Obsolete tape handling
- **SAME SORT AREA / SAME SORT-MERGE AREA** — Rarely specified
- **RERUN clause** — Checkpoint/restart (rarely used in modern environments)
