# Runtime Crate — Product Requirements

## Overview

The `open-mainframe-runtime` crate provides a runtime execution environment for compiled COBOL programs on the OpenMainframe platform. It includes a tree-walking interpreter that executes a simplified COBOL AST, decimal arithmetic with IBM-compatible overflow detection, COBOL console I/O, string manipulation (STRING, UNSTRING, INSPECT), and a COBOL value type system.

## Current State Assessment

- **Lines of code:** ~4,100
- **Test count:** 42 (all passing)
- **Maturity:** Moderate (arithmetic, strings), Prototype (interpreter, external calls)
- **Files:** 7 Rust source files (lib, decimal, error, interpreter, io, string, value)

### What Works Well

**Arithmetic (decimal.rs):**
- IBM COBOL-compatible decimal arithmetic via `rust_decimal`
- 18-digit precision (IBM standard)
- ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE with ON SIZE ERROR detection
- POWER with positive and negative exponents
- Rounding modes: Truncate (default), Round (ties away from zero)
- Remainder computation for DIVIDE ... REMAINDER

**Value System (value.rs):**
- CobolValue enum: Alphanumeric(String), Numeric(NumericValue), Group(Vec<u8>)
- Full arithmetic operations on NumericValue
- Type coercion between alphanumeric and numeric
- Display string formatting with decimal places

**String Operations (string.rs):**
- STRING statement with delimiter handling and pointer tracking
- UNSTRING with multiple delimiters, ALL keyword, COUNT, DELIMITER capture
- INSPECT TALLYING (CHARACTERS, ALL, LEADING, FIRST with BEFORE/AFTER)
- INSPECT REPLACING (ALL, LEADING, FIRST with BEFORE/AFTER)
- INSPECT CONVERTING (character-by-character mapping)

**Console I/O (io.rs):**
- DISPLAY to Console, Sysout, Syserr with NO ADVANCING support
- ACCEPT from Console, Date (YYYYMMDD), Day (YYDDD), Day-of-Week (1-7), Time (HHMMSSCC)
- Configurable input/output streams for testing

**Interpreter (interpreter.rs):**
- Statement execution: MOVE, COMPUTE, ADD, SUBTRACT, MULTIPLY, DIVIDE, IF, EVALUATE, PERFORM, PERFORM INLINE, GO TO, INITIALIZE, SET, STRING, UNSTRING, INSPECT (all three forms)
- File I/O: OPEN, CLOSE, READ, WRITE (simulated in-memory)
- EXEC CICS command dispatch via callback handler
- JSON GENERATE and JSON PARSE (simplified)
- XML GENERATE and XML PARSE (simplified)
- ALLOCATE/FREE for dynamic memory
- SEARCH/SEARCH ALL for table lookups
- Group item decomposition/composition with OCCURS/REDEFINES/FILLER
- Reference modification support
- 4 intrinsic functions: UPPER-CASE, LOWER-CASE, CURRENT-DATE, LENGTH

### What Does NOT Work

- GO TO uses PERFORM semantics instead of true unstructured jump
- External CALL targets not resolved (logs and continues)
- SEARCH ALL uses linear search instead of binary search
- Limited intrinsic functions (4 of 50+ IBM standard functions)
- No SORT/MERGE verb runtime support
- No COPY/REPLACING runtime support (compile-time feature)
- No reference modification validation (bounds checking)
- No COBOL 88-level condition name evaluation at runtime
- No PERFORM VARYING with UNTIL/AFTER
- No true paragraph/section flow control (THRU, EXIT PARAGRAPH, EXIT SECTION)
- No ON SIZE ERROR handler propagation for nested arithmetic
- No numeric editing (PIC ZZ,ZZ9.99, PIC $$,$$$,$$9.99)
- No USAGE COMP/COMP-3/COMP-5 binary storage formats
- No file status codes (2-byte status field)
- No sequential, indexed, or relative file organization
- No Language Environment callable services (CEExxxx)
- No condition handling framework (CEEHDLR/CEESGL equivalent)
- Date/time calculations are approximate (not calendar-accurate)
- JSON/XML parsing is manually implemented (not spec-compliant)
- No SPECIAL-NAMES paragraph support
- No DEBUGGING mode
- No program-level RETURN-CODE management

## Functional Requirements

### FR-v3.0-500: External CALL Resolution
Implement external program CALL with BY REFERENCE and BY CONTENT parameter passing. Resolve called programs via a registry or dynamic loading mechanism. Support CANCEL to unload.
- **Priority:** CRITICAL
- **Gap Type:** Incomplete implementation (logs and continues)
- **IBM Reference:** Enterprise COBOL Programming Guide — CALL statement. External CALL is fundamental to modular COBOL programming. Nearly all production systems use CALL for inter-program communication.

### FR-v3.0-501: True GO TO and Paragraph Flow Control
Implement unstructured GO TO, PERFORM ... THRU, EXIT PARAGRAPH, EXIT SECTION, and ALTER statement for legacy code support. Enable true paragraph-level flow control instead of current PERFORM-like semantics.
- **Priority:** CRITICAL
- **Gap Type:** Incomplete implementation
- **IBM Reference:** Enterprise COBOL Language Reference — GO TO, PERFORM THRU. GO TO is heavily used in legacy COBOL. PERFORM THRU is the most common paragraph execution pattern.

### FR-v3.0-502: Complete Intrinsic Functions
Expand from 4 to cover the 50+ standard IBM COBOL intrinsic functions including FUNCTION NUMVAL, NUMVAL-C, TRIM, REVERSE, ORD, MAX, MIN, INTEGER, MOD, REM, RANDOM, WHEN-COMPILED, INTEGER-OF-DATE, DATE-OF-INTEGER, etc.
- **Priority:** CRITICAL
- **Gap Type:** Incomplete implementation (4 of 50+)
- **IBM Reference:** Enterprise COBOL Language Reference — Intrinsic Functions. IBM Enterprise COBOL 6.4 supports 50+ intrinsic functions. Many are used in production code for date arithmetic, string manipulation, and numeric conversion.

### FR-v3.0-503: Numeric Editing and Display Formats
Implement numeric editing per PIC clause patterns: Z (zero suppress), * (check protect), $ (floating currency), CR/DB (credit/debit), +/- (sign), B (blank insertion), 0 (zero insertion), , (comma insertion), . (decimal point).
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** Enterprise COBOL Language Reference — PICTURE clause editing. Numeric editing is fundamental to COBOL report generation and display output.

### FR-v3.0-504: Binary Storage Formats (COMP/COMP-3/COMP-5)
Implement USAGE COMP (binary), COMP-3 (packed decimal), COMP-5 (native binary) storage representations. Support conversion between storage formats and display format.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** Enterprise COBOL Language Reference — USAGE clause. COMP-3 is ubiquitous in production COBOL (packed decimal storage). COMP is standard for binary integers. These affect how data is stored in memory and files.

### FR-v3.0-505: File I/O with Status Codes and Organizations
Implement proper file handling with file status codes (00, 10, 23, 35, etc.), sequential/indexed/relative file organizations, SELECT ... ASSIGN, and file status field updates. Integrate with the dataset crate for actual file access.
- **Priority:** MAJOR
- **Gap Type:** Incomplete implementation (simulated in-memory only)
- **IBM Reference:** Enterprise COBOL Programming Guide — File Handling. Real COBOL programs rely on file status codes for error handling. Indexed files (VSAM KSDS) are the most common file organization.

### FR-v3.0-506: SORT and MERGE Verb Runtime
Implement the SORT verb with USING/GIVING/INPUT PROCEDURE/OUTPUT PROCEDURE and ASCENDING/DESCENDING KEY. Integrate with the open-mainframe-sort crate.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** Enterprise COBOL Language Reference — SORT statement. SORT is one of the most frequently used COBOL features in batch processing.

### FR-v3.0-507: PERFORM VARYING with AFTER
Implement PERFORM VARYING identifier FROM value BY value UNTIL condition with AFTER clause for nested loops. Support both in-line and out-of-line PERFORM VARYING.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** Enterprise COBOL Language Reference — PERFORM VARYING. Used in virtually all COBOL programs that process tables or arrays.

### FR-v3.0-508: 88-Level Condition Names
Implement runtime evaluation of 88-level condition names (TRUE/FALSE testing, SET TO TRUE, VALUE clause with ranges and lists).
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** Enterprise COBOL Language Reference — Condition Names. 88-levels are a fundamental COBOL feature used extensively for state and flag management.

### FR-v3.0-509: Language Environment Callable Services
Implement a subset of LE callable services covering date/time (CEEDAYS, CEEDATE, CEESECS), storage (CEEGTST, CEECZST), and condition handling (CEEHDLR, CEESGL). These are accessed via CALL 'CEExxxx'.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** z/OS Language Environment Programming Reference — Callable Services. LE callable services provide standardized date/time, math, and condition handling across languages.

### FR-v3.0-510: RETURN-CODE Special Register
Implement the RETURN-CODE special register for inter-program communication. Set on STOP RUN and GOBACK. Accessible in calling program after CALL returns.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** Enterprise COBOL Language Reference — RETURN-CODE Special Register. Used extensively for program-to-program status communication.

### FR-v3.0-511: Accurate Date/Time Functions
Replace approximate date/time calculations with calendar-accurate implementations. Handle leap years, leap seconds, Lilian day numbers, and COBOL date intrinsic functions.
- **Priority:** MINOR
- **Gap Type:** Incomplete implementation
- **IBM Reference:** Enterprise COBOL Language Reference — Date/Time intrinsic functions. Accurate date arithmetic is critical for financial and business applications.

### FR-v3.0-512: SEARCH ALL Binary Search
Fix SEARCH ALL to use proper binary search algorithm instead of linear scan. The table must be sorted for SEARCH ALL.
- **Priority:** MINOR
- **Gap Type:** Incorrect implementation
- **IBM Reference:** Enterprise COBOL Language Reference — SEARCH ALL. SEARCH ALL requires ascending key ordering and uses binary search for O(log n) performance.
