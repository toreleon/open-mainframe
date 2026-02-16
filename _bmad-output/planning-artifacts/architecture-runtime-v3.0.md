# Runtime Crate — Architecture Decisions

## AD-3.0-01: External CALL via Program Registry

**Context:** The interpreter logs a message and continues when an external CALL target is not found. Real COBOL uses CALL to invoke subprograms, which is fundamental to modular programming. The called program shares LINKAGE SECTION data with the caller via BY REFERENCE / BY CONTENT.

**Decision:** Implement a `ProgramRegistry` that maps program names to `SimpleProgram` instances (or compiled function pointers in future). External CALL resolves the target from the registry, creates a new `Environment` for the called program (sharing LINKAGE SECTION data via Arc<Mutex<>> for BY REFERENCE), executes it, and returns control to the caller. BY CONTENT creates a copy. The registry supports dynamic registration and CANCEL (unloading).

**Consequences:**
- Programs must be loaded/registered before they can be called
- BY REFERENCE requires shared mutable access to data items
- Recursive CALL is supported (each invocation gets its own environment)
- CANCEL removes the program, freeing resources
- RETURN-CODE flows from called to calling program via environment

## AD-3.0-02: True GO TO via Continuation-Based Control Flow

**Context:** The current GO TO implementation uses PERFORM-like semantics (function call and return). Real COBOL GO TO performs an unstructured jump — control does not return to the GO TO location. PERFORM THRU executes a range of paragraphs sequentially. EXIT PARAGRAPH/SECTION terminates the current scope.

**Decision:** Replace the statement execution loop with a continuation-based model. Each statement returns a `ControlFlow` enum: `Continue`, `GoTo(label)`, `PerformReturn`, `StopRun(code)`, `ExitParagraph`, `ExitSection`. When `GoTo(label)` is returned, the executor looks up the target paragraph and transfers control there. For PERFORM THRU, the executor iterates paragraphs from the start to the end label. ALTER changes the GO TO target dynamically (tracked in the environment).

**Consequences:**
- Major refactor of `execute_statements` and `execute_statement_impl`
- GO TO across section boundaries requires global paragraph index
- PERFORM THRU needs an ordered paragraph list
- ALTER requires mutable GO TO targets (HashMap<String, String>)
- Fall-through from one paragraph to the next is the default behavior

## AD-3.0-03: Intrinsic Functions via Function Registry

**Context:** Only 4 intrinsic functions are implemented (UPPER-CASE, LOWER-CASE, CURRENT-DATE, LENGTH). IBM Enterprise COBOL 6.4 supports 50+ intrinsic functions covering date arithmetic, string manipulation, numeric conversion, and math.

**Decision:** Create a `FunctionRegistry` HashMap mapping function names to evaluation closures. Each intrinsic function is a standalone closure that takes `Vec<CobolValue>` and returns `CobolValue`. Functions are grouped by category: string (TRIM, REVERSE, ORD, CHAR), numeric (NUMVAL, NUMVAL-C, MOD, REM, INTEGER, MAX, MIN, RANDOM), date (INTEGER-OF-DATE, DATE-OF-INTEGER, WHEN-COMPILED, DATE-TO-YYYYMMDD), and misc (LENGTH, BYTE-LENGTH). The registry is populated at interpreter construction.

**Consequences:**
- Adding new intrinsic functions is mechanical (one closure per function)
- Date functions need the accurate calendar library (see AD-3.0-06)
- NUMVAL/NUMVAL-C must handle edited numeric strings
- Some functions require access to environment state (WHEN-COMPILED)
- Functions can be tested independently of the interpreter

## AD-3.0-04: Binary Storage Formats

**Context:** COBOL programs use USAGE COMP (pure binary), COMP-3 (packed decimal — each digit in 4 bits, sign in last nibble), and COMP-5 (native binary — platform endianness) for efficient storage. The current `CobolValue` stores all numerics as `rust_decimal::Decimal`, losing the underlying storage representation.

**Decision:** Extend `NumericValue` with a `storage_format` field: `Display` (default), `PackedDecimal`, `Binary`, `NativeBinary`. When reading from or writing to byte buffers (group items, file records), convert between the storage format and the internal `Decimal` representation. Packed decimal uses the standard IBM format: each byte holds two BCD digits, the last nibble is the sign (C=positive, D=negative, F=unsigned). Binary uses big-endian 2/4/8 byte format based on PIC size.

**Consequences:**
- Group item decomposition/composition must respect USAGE clauses
- File I/O reads/writes binary data correctly
- COMP-3 conversion is critical for data interchange with real z/OS files
- Performance improvement for numeric comparisons on binary formats
- Must handle halfword/fullword/doubleword alignment for COMP items

## AD-3.0-05: File I/O via Dataset Crate Integration

**Context:** The interpreter simulates file I/O with in-memory vectors. Real COBOL files use sequential, indexed (VSAM KSDS), and relative (VSAM RRDS) organizations with 2-byte file status codes. The `open-mainframe-dataset` crate provides VSAM support.

**Decision:** Create a `FileHandler` trait with implementations for `MemoryFile` (current behavior, for tests), `SequentialFile` (line-based or fixed-record), and `VsamFile` (using the dataset crate). The SELECT clause maps to a `FileDescriptor` containing organization, access mode, record format, and file status field. After each I/O operation, the file status field in the environment is updated with the standard 2-byte status code (00=success, 10=end-of-file, 23=not found, etc.).

**Consequences:**
- File status codes enable standard COBOL error handling patterns
- VSAM integration requires the dataset crate as a dependency
- Sequential files support both fixed-length and variable-length records
- Indexed files support primary and alternate keys
- START, READ NEXT, READ PREVIOUS for indexed file navigation

## AD-3.0-06: Calendar-Accurate Date Library

**Context:** The current date/time functions use approximate calculations that don't properly handle leap years, century transitions, or Lilian day numbers (days since October 15, 1582). COBOL date intrinsic functions and LE callable services depend on accurate calendar arithmetic.

**Decision:** Implement a small calendar library (or use the `chrono` crate) for date arithmetic. Support Lilian day numbers (INTEGER-OF-DATE), Gregorian date construction (DATE-OF-INTEGER), day-of-year (INTEGER-OF-DAY), and year-day construction (DAY-OF-INTEGER). Handle leap year rules correctly (divisible by 4, except centuries, except 400-year centuries). Use this library for both intrinsic functions and LE callable services (CEEDAYS, CEEDATE).

**Consequences:**
- Accurate date arithmetic for financial calculations
- Shared between intrinsic functions and LE callable services
- Lilian day numbers provide a common base for date manipulation
- Must handle date ranges from 1582 to 9999
- ACCEPT FROM DATE/DAY become accurate
