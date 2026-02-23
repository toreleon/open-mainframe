# Gap Analysis: PL/I (Programming Language One)

## Official Specification Summary

PL/I (Programming Language One) is a general-purpose procedural programming language developed by IBM in the mid-1960s. It was designed to unify features from FORTRAN (scientific computing), COBOL (business data processing), and ALGOL (structured programming) into a single language. IBM Enterprise PL/I for z/OS (currently Version 6.1) is a production-grade compiler that generates optimized code for z/Architecture.

PL/I is classified as **Common** on mainframes — while not as ubiquitous as COBOL, it has a significant presence in:
- Insurance companies (policy administration systems)
- Government agencies (federal and state systems)
- Banking (core banking platforms, especially in Europe)
- System utilities and infrastructure programs
- Mixed-language applications (PL/I calling COBOL or assembler)

PL/I programs can use EXEC CICS, EXEC SQL (DB2), and EXEC DLI (IMS) just like COBOL, making it a first-class citizen in the mainframe application ecosystem. The Language Environment (LE) provides a common runtime for PL/I, COBOL, C/C++, and Fortran programs to interoperate.

Key documentation:
- **Enterprise PL/I for z/OS Language Reference** (SC31-5716)
- **Enterprise PL/I for z/OS Programming Guide** (GI13-4536)

## Key Features & Capabilities

### 1. Data Types

#### Arithmetic Data Types
| Type | Description |
|------|-------------|
| FIXED DECIMAL(p,q) | Packed decimal, precision p (1-31), scale q. Stored as packed BCD. |
| FIXED BINARY(p,q) | Binary integer, precision p (1-63). Maps to halfword/fullword/doubleword. |
| FLOAT DECIMAL(p) | Floating-point, p significant digits. Maps to short/long/extended HFP or BFP. |
| FLOAT BINARY(p) | Floating-point, p binary digits. |
| COMPLEX | Complex number (real + imaginary parts). Any arithmetic base. |
| PICTURE | Numeric editing (like COBOL PIC 9). Drifting signs, insertion chars. |

#### String Data Types
| Type | Description |
|------|-------------|
| CHARACTER(n) | Fixed-length character string |
| CHARACTER(n) VARYING | Variable-length character string (stored with 2-byte length prefix) |
| CHARACTER(*) | Adjustable-length (for parameters) |
| BIT(n) | Fixed-length bit string |
| BIT(n) VARYING | Variable-length bit string |
| GRAPHIC(n) | DBCS (double-byte) character string |
| GRAPHIC(n) VARYING | Variable-length DBCS string |
| WIDECHAR(n) | UTF-16 character string (Enterprise PL/I V5+) |

#### Pointer and Locator Types
| Type | Description |
|------|-------------|
| POINTER | Address of any data item |
| OFFSET | Offset within an AREA (relocatable pointer) |
| HANDLE | Typed pointer to a specific structure type (V5+) |
| AREA(n) | Storage area for BASED allocation (contains OFFSET-addressable data) |

#### Other Data Types
| Type | Description |
|------|-------------|
| LABEL | Label variable (target of GO TO) |
| ENTRY | Entry point variable (for CALL through function pointer) |
| FILE | File variable |
| FORMAT | Format variable (for PUT/GET EDIT) |
| TASK | Task variable (for multitasking) |
| EVENT | Event variable (for async operations) |
| ORDINAL | User-defined enumeration type (V5+) |
| TYPE | Type alias (DEFINE ALIAS, DEFINE STRUCTURE) (V5+) |
| UNION | Overlaid storage (like COBOL REDEFINES) (V5+) |

### 2. Data Attributes and Storage Classes

#### Storage Classes
| Attribute | Description |
|-----------|-------------|
| AUTOMATIC | Stack-allocated (default for internal procedures) |
| STATIC | Persistent across calls |
| CONTROLLED | Stack of allocations (ALLOCATE/FREE) — explicit lifetime |
| BASED | Pointer-based allocation (ALLOCATE/FREE with POINTER) |
| DEFINED | Alias for existing storage (like C union or overlay) |
| PARAMETER | Procedure parameter |

#### Other Attributes
| Attribute | Description |
|-----------|-------------|
| ALIGNED / UNALIGNED | Storage alignment |
| INITIAL | Initialization value |
| DIMENSION(bounds) | Array dimensions (up to 15) |
| LIKE | Copy structure layout from another structure |
| CONNECTED / NONCONNECTED | Array storage layout |
| REFER | Self-defining structures (adjustable extents) |

### 3. Statements (~80+ statement types)

#### Declaration Statements
| Statement | Purpose |
|-----------|---------|
| DECLARE (DCL) | Declare variables with attributes |
| DEFAULT (DFT) | Set default attributes |
| DEFINE ALIAS | Create type alias |
| DEFINE ORDINAL | Create enumeration type |
| DEFINE STRUCTURE | Create named structure type |

#### Assignment and Expression
| Statement | Purpose |
|-----------|---------|
| Assignment (=) | Assign value |
| ALLOCATE | Allocate CONTROLLED or BASED storage |
| FREE | Free CONTROLLED or BASED storage |

#### Control Flow
| Statement | Purpose |
|-----------|---------|
| IF / THEN / ELSE | Conditional |
| DO / END | Loop (DO WHILE, DO UNTIL, DO var=start TO end BY step, DO REPEAT) |
| SELECT / WHEN / OTHERWISE / END | Multi-way branch |
| GO TO | Transfer control |
| CALL | Call procedure |
| RETURN | Return from procedure/function |
| STOP | Terminate program |
| EXIT | Exit from task |
| LEAVE | Exit loop |
| ITERATE | Next iteration of loop |

#### I/O Statements (Stream)
| Statement | Purpose |
|-----------|---------|
| GET LIST | Free-format input |
| GET EDIT | Formatted input (with format items) |
| GET DATA | Name-directed input |
| PUT LIST | Free-format output |
| PUT EDIT | Formatted output |
| PUT DATA | Name-directed output |
| PUT PAGE / PUT LINE / PUT SKIP | Output formatting |
| DISPLAY | Display message to operator |

#### I/O Statements (Record)
| Statement | Purpose |
|-----------|---------|
| READ | Read record from file |
| WRITE | Write record to file |
| REWRITE | Update record in place |
| DELETE | Delete record |
| LOCATE | Locate-mode output (no copy) |
| OPEN | Open file |
| CLOSE | Close file |

#### Exception Handling
| Statement | Purpose |
|-----------|---------|
| ON condition action | Establish condition handler |
| SIGNAL condition | Raise a condition |
| REVERT condition | Remove condition handler |
| RESIGNAL | Pass condition to next handler |

#### Preprocessor Statements
| Statement | Purpose |
|-----------|---------|
| %DECLARE | Declare preprocessor variable |
| %assignment | Assign preprocessor variable |
| %IF / %THEN / %ELSE | Conditional compilation |
| %DO / %END | Preprocessor loop |
| %INCLUDE | Include source member |
| %ACTIVATE / %DEACTIVATE | Control preprocessor replacement |
| %PROCEDURE / %END | Preprocessor procedure (macro) |
| %NOTE | Generate compiler message |
| %PRINT / %NOPRINT | Control listing |
| %PUSH / %POP | Save/restore preprocessor state |
| %XINCLUDE | Include once (guard) |

#### Other Statements
| Statement | Purpose |
|-----------|---------|
| BEGIN / END | Begin block (scoping) |
| PROCEDURE / END | Procedure definition |
| FETCH | Dynamically load a program |
| RELEASE | Release dynamically loaded program |
| ATTACH | Create subtask (multitasking) |
| WAIT | Wait for event(s) |
| DELAY | Wait for time interval |
| FLUSH | Flush output buffers |
| FORMAT | Define format for EDIT I/O |

### 4. ON-Conditions (Exception Handling)

PL/I has one of the most comprehensive exception handling systems of any language:

#### Computational Conditions
| Condition | Triggered By |
|-----------|-------------|
| CONVERSION | Invalid character-to-arithmetic conversion |
| FIXEDOVERFLOW | Fixed-point arithmetic overflow |
| OVERFLOW | Floating-point overflow |
| UNDERFLOW | Floating-point underflow |
| ZERODIVIDE | Division by zero |
| SIZE | Assignment truncation (enabled by prefix) |
| STRINGRANGE | Substring out of bounds |
| STRINGSIZE | String truncation on assignment |
| SUBSCRIPTRANGE | Array subscript out of bounds |
| INVALIDOP | Invalid floating-point operation |

#### I/O Conditions
| Condition | Triggered By |
|-----------|-------------|
| ENDFILE(file) | End of file reached |
| ENDPAGE(file) | End of page on print file |
| KEY(file) | Keyed I/O error |
| RECORD(file) | Record I/O error |
| TRANSMIT(file) | I/O transmission error |
| UNDEFINEDFILE(file) | File cannot be opened |
| NAME(file) | Invalid name in data-directed I/O |

#### Program Control Conditions
| Condition | Triggered By |
|-----------|-------------|
| ERROR | Any unhandled condition |
| FINISH | Program termination |
| AREA | Area overflow |
| ATTENTION | External interrupt |
| CONDITION(name) | User-defined named condition |
| STORAGE | Storage allocation failure |

### 5. Built-in Functions (~200+ functions)

#### Arithmetic
ABS, ADD, BINARY, CEIL, COMPLEX, CONJG, DECIMAL, DIVIDE, FIXED, FLOAT, FLOOR, IMAG, MAX, MIN, MOD, MULTIPLY, PRECISION, REAL, REMAINDER, ROUND, SIGN, TRUNC

#### Mathematical
ACOS, ASIN, ATAN, ATAND, ATANH, COS, COSD, COSH, ERF, ERFC, EXP, GAMMA, LOG, LOG10, LOG2, LOGGAMMA, SIN, SIND, SINH, SQRT, TAN, TAND, TANH

#### String
BIT, BOOL, CHAR, COLLATE, COPY, GRAPHIC, HEX, HIGH, INDEX, LENGTH, LOW, MAXLENGTH, MPSTR, REPEAT, REVERSE, SEARCH, SEARCHR, SUBSTR, TRANSLATE, TRIM, LTRIM, RTRIM, UNSPEC, VERIFY, VERIFYR, WHIGH, WLOW

#### Conversion
BINARY, BIT, CHAR, DECIMAL, FIXED, FLOAT, GRAPHIC, HEX, HEXIMAGE, IEEE, UNSPEC, WIDECHAR

#### Array/Structure
ALLOCATION, CURRENTSIZE, DIMENSION, HBOUND, LBOUND, POLY, PROD, SIZE, STORAGE, SUM

#### Date/Time
DATE, DATETIME, DAYS, DAYSTODATE, DAYSTOSECS, JULIANDATE, SECS, SECSTODATE, SECSTODAYS, TIME, TIMESTAMP, VALIDDATE, WEEKDAY, Y4DATE, Y4JULIAN, Y4YEAR

#### Storage Management
ADDR, ALLOCATE, BINARYVALUE, ENTRYADDR, NULL, OFFSET, POINTER, POINTERADD, POINTERDIFF, POINTERVALUE, SYSNULL

#### Condition Handling
DATAFIELD, ONCHAR, ONCODE, ONCONDCOND, ONCONDID, ONCOUNT, ONFILE, ONGSOURCE, ONKEY, ONLOC, ONSOURCE, ONSUBCODE, ONWCHAR, ONWSOURCE, PLIRETC, PLIRETV

#### System
ADDR, COMPILETIME, COUNTER, CURRENTSIZE, EMPTY, ENTRYADDR, INDICATORS, LINENO, MEMCONVERT, MEMCU12, MEMCU14, MEMCU21, MEMCU24, MEMCU41, MEMCU42, MEMINDEX, MEMSEARCH, MEMSEARCHR, MEMVERIFY, MEMVERIFYR, PACKAGENAME, PLIRETC, PLIRETV, PLISRTA, PLISRTB, PLISRTC, PLISRTD, PROCEDURENAME, SOURCEFILE, SOURCELINE, STACKADDR, STRING, SYSTEM, THREADID

### 6. I/O Model

#### Stream I/O
- **GET LIST/PUT LIST**: Free-format, space-delimited
- **GET EDIT/PUT EDIT**: Formatted with format items: A(w), F(w,d), E(w,d), B(w), P'picture', COLUMN(n), LINE(n), PAGE, SKIP(n), X(n), R(format-var)
- **GET DATA/PUT DATA**: Self-describing name=value format

#### Record I/O
- **Sequential**: READ/WRITE/REWRITE with SEQUENTIAL/BUFFERED
- **Keyed**: READ/WRITE/REWRITE/DELETE with KEY/KEYTO/KEYFROM
- **Regional**: REGIONAL(1), REGIONAL(2), REGIONAL(3)
- **VSAM**: KSDS/ESDS/RRDS access through record I/O
- **LOCATE mode**: Zero-copy output using SET(pointer)

#### File Attributes
STREAM/RECORD, INPUT/OUTPUT/UPDATE, SEQUENTIAL/DIRECT/TRANSIENT, BUFFERED/UNBUFFERED, PRINT, KEYED, ENVIRONMENT(options)

### 7. Preprocessor

The PL/I preprocessor is a compile-time programming facility:
- **%DECLARE / %DCL**: Declare CHARACTER or FIXED preprocessor variables
- **%IF / %THEN / %ELSE / %DO / %END**: Conditional compilation
- **%INCLUDE**: Source inclusion (like COBOL COPY)
- **%PROCEDURE / %RETURN / %END**: Preprocessor macros (functions)
- **%ACTIVATE / %DEACTIVATE**: Enable/disable identifier replacement
- **%NOTE**: Generate compile-time messages
- **Preprocessor expressions**: Full string and arithmetic evaluation at compile time

### 8. Structures and Arrays

```pli
DCL 1 EMPLOYEE,
      2 NAME,
        3 FIRST CHAR(20),
        3 LAST CHAR(30),
      2 SALARY FIXED DEC(9,2),
      2 HIRE_DATE CHAR(10),
      2 DEPARTMENT FIXED BIN(15);

DCL TABLE(100) LIKE EMPLOYEE;     /* Array of structures */
DCL MATRIX(10,10) FLOAT DEC(15);  /* 2D array */
```

- **Structure levels** (1-15): Like COBOL group/elementary items
- **LIKE**: Copy structure definition
- **DEFINED**: Overlay one variable on another
- **UNION**: Shared storage (like C union)
- **REFER**: Self-defining structures with adjustable extents
- **Arrays**: Up to 15 dimensions, adjustable bounds with *

### 9. CICS/DB2/IMS Integration

PL/I has full parity with COBOL for subsystem access:

- **EXEC CICS**: Same commands as COBOL (SEND MAP, RECEIVE MAP, READ, WRITE, LINK, XCTL, etc.)
- **EXEC SQL**: Same DB2 SQL as COBOL (SELECT, INSERT, UPDATE, DELETE, DECLARE CURSOR, FETCH, etc.)
- **EXEC DLI**: IMS DL/I access (GU, GN, GNP, ISRT, DLET, REPL, etc.)
- **Build process**: DB2 precompiler → CICS translator → PL/I compiler → Binder

### 10. Multitasking

PL/I has built-in multitasking primitives (unique among mainframe languages):
- **ATTACH**: Create a subtask
- **WAIT**: Wait for task/event completion
- **EVENT**: Event variable for synchronization
- **PRIORITY**: Set task priority
- **COMPLETION**: Check event status
- **STATUS**: Check task status

## Current OpenMainframe Status

**No PL/I implementation exists.** The only reference is `MapLanguage::Pli` in `crates/open-mainframe-cics/src/bms/parser.rs:160` — an enum variant for BMS map language detection. No PL/I lexer, parser, compiler, or runtime exists.

However, several existing subsystems could be reused:
- `open-mainframe-encoding` — EBCDIC, packed decimal, DBCS (PL/I uses the same encodings)
- `open-mainframe-cics` — EXEC CICS preprocessing and runtime (PL/I uses the same CICS API)
- `open-mainframe-db2` — EXEC SQL preprocessing and runtime (PL/I uses the same SQL)
- `open-mainframe-ims` — EXEC DLI preprocessing and runtime
- `open-mainframe-dataset` — VSAM, QSAM, PDS file access
- `open-mainframe-runtime` — Decimal arithmetic, date/time (partially reusable)

## Gap Details

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| PL/I Lexer | Full — free-form, keywords, operators, string/numeric literals | None | **Missing** |
| PL/I Parser | Full — declarations, statements, expressions, ON-units | None | **Missing** |
| Arithmetic data types (FIXED DEC/BIN, FLOAT DEC/BIN, COMPLEX) | Full | None (COBOL runtime has decimal) | **Missing** |
| String data types (CHAR, CHAR VARYING, BIT, GRAPHIC, WIDECHAR) | Full | None | **Missing** |
| Pointer types (POINTER, OFFSET, HANDLE, AREA) | Full | None | **Missing** |
| Special types (LABEL, ENTRY, FILE, FORMAT, TASK, EVENT) | Full | None | **Missing** |
| User-defined types (DEFINE ALIAS, ORDINAL, STRUCTURE, UNION) | Full (V5+) | None | **Missing** |
| Storage classes (AUTOMATIC, STATIC, CONTROLLED, BASED, DEFINED) | Full | None | **Missing** |
| Structures and arrays (up to 15 dims, LIKE, REFER, UNION) | Full | None | **Missing** |
| ~80 statement types | Full | None | **Missing** |
| ~200 built-in functions | Full | None | **Missing** |
| ~25 ON-conditions (exception handling) | Full | None | **Missing** |
| Stream I/O (GET/PUT LIST/EDIT/DATA) | Full | None | **Missing** |
| Record I/O (READ/WRITE/REWRITE/DELETE, VSAM) | Full | None | **Missing** |
| Preprocessor (%IF, %INCLUDE, %PROCEDURE, etc.) | Full | None | **Missing** |
| EXEC CICS integration | Full | Preprocessor exists for COBOL | **Missing** (PL/I syntax variant) |
| EXEC SQL integration | Full | Preprocessor exists for COBOL | **Missing** (PL/I syntax variant) |
| EXEC DLI integration | Full | Preprocessor exists for COBOL | **Missing** (PL/I syntax variant) |
| Multitasking (ATTACH, WAIT, EVENT) | Full | None | **Missing** |
| FETCH/RELEASE (dynamic loading) | Full | None | **Missing** |
| Code generation | Full — native z/Architecture + LLVM (V5+) | None | **Missing** |
| Format items for EDIT I/O | Full — A, F, E, B, P, COLUMN, LINE, etc. | None | **Missing** |
| PICTURE data type (numeric editing) | Full | COBOL has PIC, not reusable | **Missing** |

## Proposed Epic Structure

### Epic P100: PL/I Lexer and Parser
- **P100.1**: Lexer — free-form source, keywords (non-reserved in PL/I!), string literals (single/double quote), numeric literals, operators, comments (/* */)
- **P100.2**: Parser — DECLARE statements with full attribute syntax, structure levels
- **P100.3**: Parser — control flow (IF, DO, SELECT, GO TO, CALL, RETURN)
- **P100.4**: Parser — I/O statements (GET, PUT, OPEN, CLOSE, READ, WRITE)
- **P100.5**: Parser — ON/SIGNAL/REVERT exception handling
- **P100.6**: Parser — preprocessor (%IF, %INCLUDE, %DECLARE, %PROCEDURE)
- **Complexity**: XL (Extra Large — PL/I has no reserved words, making parsing very context-sensitive)

### Epic P101: PL/I Type System
- **P101.1**: Arithmetic types — FIXED DECIMAL, FIXED BINARY, FLOAT DECIMAL, FLOAT BINARY, COMPLEX
- **P101.2**: String types — CHARACTER (fixed/varying), BIT (fixed/varying), GRAPHIC, WIDECHAR
- **P101.3**: Pointer types — POINTER, OFFSET, AREA, HANDLE
- **P101.4**: Special types — LABEL, ENTRY, FILE, FORMAT
- **P101.5**: User-defined types — DEFINE ALIAS, ORDINAL, STRUCTURE, UNION
- **P101.6**: Implicit type conversion rules (PL/I has extensive automatic conversions)
- **Complexity**: XL (Extra Large — PL/I's type system is far more complex than COBOL's)

### Epic P102: PL/I Interpreter Core
- **P102.1**: Variable storage — AUTOMATIC, STATIC scoping
- **P102.2**: Expression evaluator — arithmetic, string, comparison, concatenation
- **P102.3**: Assignment — with automatic type conversion
- **P102.4**: Control flow — IF/THEN/ELSE, DO loops (WHILE, UNTIL, REPEAT, iterative), SELECT/WHEN
- **P102.5**: Procedures — CALL, RETURN, recursion, ENTRY attribute, RETURNS attribute
- **P102.6**: BEGIN blocks and scoping
- **Complexity**: XL (Extra Large)

### Epic P103: PL/I Storage Management
- **P103.1**: CONTROLLED storage — ALLOCATE/FREE stack
- **P103.2**: BASED storage — ALLOCATE/FREE with pointer, SET option
- **P103.3**: DEFINED overlays — POSITION attribute
- **P103.4**: AREA and OFFSET — area-based allocation
- **P103.5**: REFER — self-defining structures with adjustable extents
- **P103.6**: FETCH/RELEASE — dynamic program loading
- **Complexity**: L (Large)

### Epic P104: PL/I Built-in Functions
- **P104.1**: Arithmetic functions (~25 functions: ABS, MOD, CEIL, FLOOR, MAX, MIN, SIGN, etc.)
- **P104.2**: Mathematical functions (~25 functions: SIN, COS, TAN, SQRT, LOG, EXP, etc.)
- **P104.3**: String functions (~30 functions: INDEX, SUBSTR, LENGTH, VERIFY, TRANSLATE, TRIM, etc.)
- **P104.4**: Conversion functions (~15 functions: BINARY, DECIMAL, FIXED, FLOAT, CHAR, BIT, HEX, etc.)
- **P104.5**: Date/time functions (~15 functions: DATE, DATETIME, DAYS, SECS, TIMESTAMP, etc.)
- **P104.6**: Array/structure functions (~10 functions: HBOUND, LBOUND, DIMENSION, SUM, PROD, etc.)
- **P104.7**: Storage/system functions (~30 functions: ADDR, NULL, ALLOCATION, PLIRETC, etc.)
- **Complexity**: L (Large)

### Epic P105: PL/I Exception Handling
- **P105.1**: ON-unit establishment — ON condition action
- **P105.2**: Computational conditions — CONVERSION, FIXEDOVERFLOW, OVERFLOW, UNDERFLOW, ZERODIVIDE, SIZE
- **P105.3**: I/O conditions — ENDFILE, ENDPAGE, KEY, RECORD, TRANSMIT, UNDEFINEDFILE
- **P105.4**: Program conditions — ERROR, FINISH, AREA, STORAGE, CONDITION(name)
- **P105.5**: SIGNAL and REVERT statements
- **P105.6**: Condition built-in functions — ONCODE, ONLOC, ONCHAR, ONSOURCE, ONFILE, ONKEY
- **P105.7**: Condition prefixes — (SIZE): stmt, (STRINGRANGE): stmt, (SUBSCRIPTRANGE): stmt
- **Complexity**: L (Large)

### Epic P106: PL/I Stream I/O
- **P106.1**: GET LIST / PUT LIST — free-format I/O
- **P106.2**: GET EDIT / PUT EDIT — formatted I/O with format items (A, F, E, B, P, COLUMN, LINE, X, SKIP)
- **P106.3**: GET DATA / PUT DATA — name-directed I/O
- **P106.4**: OPEN/CLOSE with file attributes (STREAM, PRINT, INPUT, OUTPUT)
- **P106.5**: DISPLAY statement (operator console)
- **Complexity**: M (Medium)

### Epic P107: PL/I Record I/O
- **P107.1**: Sequential READ/WRITE (CONSECUTIVE)
- **P107.2**: Keyed READ/WRITE/REWRITE/DELETE (INDEXED)
- **P107.3**: LOCATE-mode I/O (zero-copy with SET pointer)
- **P107.4**: VSAM integration via open-mainframe-dataset
- **P107.5**: QSAM/BSAM integration
- **P107.6**: ENVIRONMENT options
- **Complexity**: L (Large)

### Epic P108: PL/I Preprocessor
- **P108.1**: %DECLARE, %assignment (CHARACTER, FIXED)
- **P108.2**: %IF / %THEN / %ELSE / %DO / %END
- **P108.3**: %INCLUDE — source member inclusion
- **P108.4**: %PROCEDURE / %RETURN / %END — preprocessor macros
- **P108.5**: %ACTIVATE / %DEACTIVATE — identifier replacement control
- **P108.6**: %NOTE, %PRINT/%NOPRINT, %PUSH/%POP
- **Complexity**: M (Medium)

### Epic P109: PL/I CICS/DB2/IMS Integration
- **P109.1**: EXEC CICS preprocessing for PL/I (adapt existing CICS preprocessor)
- **P109.2**: EXEC SQL preprocessing for PL/I (adapt existing DB2 preprocessor)
- **P109.3**: EXEC DLI preprocessing for PL/I
- **P109.4**: Host variable binding for PL/I data types
- **P109.5**: SQLCA/EIB/DIB structure generation for PL/I
- **Complexity**: M (Medium — leverages existing preprocessors)

### Epic P110: PL/I Structures and Arrays
- **P110.1**: Structure declarations (levels 1-15)
- **P110.2**: Arrays — up to 15 dimensions, adjustable bounds (*)
- **P110.3**: LIKE attribute — structure type copying
- **P110.4**: UNION — overlaid storage
- **P110.5**: REFER — self-defining structures
- **P110.6**: BY NAME assignment (structure field matching)
- **Complexity**: L (Large)

### Epic P111: PL/I PICTURE Type and Numeric Editing
- **P111.1**: Character PICTURE — A, X, 9 picture characters
- **P111.2**: Numeric PICTURE — 9, V, Z, *, +, -, $, B, /, . characters
- **P111.3**: Drifting characters (-, +, $, currency)
- **P111.4**: Insertion characters (B, /, comma)
- **P111.5**: Exponent specification
- **Complexity**: M (Medium)

## Dependencies

| Dependency | Crate | Reason |
|------------|-------|--------|
| open-mainframe-lang-core | Shared traits | Span, Diagnostic, AstNode traits for lexer/parser |
| open-mainframe-encoding | EBCDIC/DBCS | PL/I CHAR and GRAPHIC types use EBCDIC/DBCS |
| open-mainframe-encoding | Packed decimal | FIXED DECIMAL maps to packed decimal |
| open-mainframe-encoding | Floating point | FLOAT maps to HFP/BFP/DFP |
| open-mainframe-cics | EXEC CICS | PL/I CICS programs use same API as COBOL |
| open-mainframe-db2 | EXEC SQL | PL/I DB2 programs use same SQL |
| open-mainframe-ims | EXEC DLI | PL/I IMS programs use same DL/I |
| open-mainframe-dataset | File I/O | VSAM, QSAM, PDS access for record I/O |
| open-mainframe-runtime | Shared runtime | Decimal arithmetic, date/time (partially reusable) |

**Key advantage**: PL/I uses the same CICS, DB2, and IMS APIs as COBOL. The existing preprocessors could be adapted (they need to handle PL/I syntax for host variable declarations, but the EXEC commands themselves are identical).

## Complexity Estimate

| Epic | Complexity | Rationale |
|------|-----------|-----------|
| P100 (Lexer/Parser) | XL | PL/I has no reserved words — any keyword can be a variable name. Context-sensitive parsing is very challenging. |
| P101 (Type System) | XL | 15+ base types, automatic conversions between all types, PICTURE, COMPLEX, adjustable bounds |
| P102 (Interpreter) | XL | Full expression evaluator with all type conversions, recursive procedures, scoping |
| P103 (Storage Mgmt) | L | CONTROLLED/BASED/DEFINED/AREA — unique to PL/I, complex lifetime semantics |
| P104 (Built-in Functions) | L | ~200 functions across many categories |
| P105 (Exception Handling) | L | Most comprehensive exception system of any mainframe language |
| P106 (Stream I/O) | M | Well-defined GET/PUT with format items |
| P107 (Record I/O) | L | Multiple access methods, LOCATE mode, VSAM integration |
| P108 (Preprocessor) | M | Compile-time programming language, but simpler than the main language |
| P109 (CICS/DB2/IMS) | M | Adapts existing preprocessors — lower effort than building from scratch |
| P110 (Structures/Arrays) | L | Multi-dimensional arrays with adjustable bounds, REFER |
| P111 (PICTURE) | M | Similar to COBOL PIC but with additional features |

**Total estimated effort**: 12 epics, overall XXL complexity (comparable to HLASM — PL/I's lack of reserved words and extensive type coercion rules make it exceptionally hard to parse correctly)

## Reference Documentation

- [Enterprise PL/I for z/OS 6.1 Language Reference (SC31-5716)](https://www.ibm.com/docs/en/SSY2V3_6.1/pdf/lrm.pdf)
- [Enterprise PL/I for z/OS 5.3 Language Reference (SC27-8940)](https://www.ibm.com/docs/en/SSY2V3_5.3.0/lr/lrm.pdf)
- [Enterprise PL/I for z/OS 5.3 Programming Guide (GI13-4536)](https://www.ibm.com/docs/en/SSY2V3_5.3.0/com.ibm.ent.pl1.zos.doc/pg.pdf)
- [IBM Enterprise PL/I Product Page](https://www.ibm.com/products/pli-compiler-zos)
- [Enterprise PL/I Documentation Library](https://www.ibm.com/support/pages/enterprise-pli-zos-documentation-library)
- [PL/I — Wikipedia](https://en.wikipedia.org/wiki/PL/I)
- [PL/I Preprocessor — Wikipedia](https://en.wikipedia.org/wiki/PL/I_preprocessor)
- [PL/I Language Features — Try MTS](https://try-mts.com/pli-language-features-1/)

## Implementation Status

> Reviewed 2026-02-23 against crate `open-mainframe-pli` (`crates/open-mainframe-pli/src/`).
> The gap analysis above was written when no PL/I implementation existed. A substantial implementation now exists.

| # | Feature | Status | Notes |
|---|---------|--------|-------|
| 1 | PL/I Lexer | YES | Full free-form lexer in `lexer.rs` (556 lines). Tokenizes keywords (non-reserved), identifiers with `#@$`, string/bit/hex literals (suffix and prefix forms), all operators (`** -> || ^= <= >=`), comments (`/* */`), preprocessor (`%`), line/col tracking. 30+ unit tests. |
| 2 | PL/I Parser | YES | Full parser in `parser.rs` (2250 lines). DECLARE with levels 1-15, full attribute syntax (data types, storage classes, LIKE, BASED, DEFINED, DIMENSION, INITIAL, ALIGNED). Control flow (IF/DO/SELECT/GO TO/CALL/RETURN/LEAVE/ITERATE). I/O (PUT/GET LIST/EDIT/DATA, OPEN/CLOSE, READ/WRITE). ON/SIGNAL/REVERT. Preprocessor (%INCLUDE/%XINCLUDE/%NOTE). 25+ unit tests. |
| 3 | Arithmetic data types (FIXED DEC/BIN, FLOAT DEC/BIN) | YES | `types.rs` implements FIXED DECIMAL(p,q), FIXED BINARY(p,q), FLOAT DECIMAL(p), FLOAT BINARY(p) with runtime values, storage size calculations, and arithmetic result type rules. COMPLEX not yet implemented. |
| 4 | String data types (CHAR, CHAR VARYING, BIT, GRAPHIC, WIDECHAR) | YES | CHARACTER(n), CHARACTER(n) VARYING, CHARACTER(*), BIT(n), BIT(n) VARYING, GRAPHIC(n), WIDECHAR(n) all implemented in parser, type system, and runtime values. |
| 5 | Pointer types (POINTER, OFFSET, HANDLE, AREA) | YES | POINTER, OFFSET, HANDLE(name), AREA(n) all have DataType, PliType, PliValue, and parser support. Pointer qualification (`->`) supported in expressions. |
| 6 | Special types (LABEL, ENTRY, FILE, FORMAT, TASK, EVENT) | YES (partial) | LABEL, ENTRY, FILE, FORMAT all have full type/value support. TASK and EVENT have parser/type variants but map to FixedBinary in interpreter. |
| 7 | User-defined types (DEFINE ALIAS, ORDINAL, STRUCTURE, UNION) | YES (partial) | ORDINAL and UNION have DataType/PliType variants. DEFINE ALIAS/DEFINE STRUCTURE not in parser. |
| 8 | Storage classes (AUTOMATIC, STATIC, CONTROLLED, BASED, DEFINED) | YES | All 6 storage classes (AUTOMATIC, STATIC, CONTROLLED, BASED, DEFINED, PARAMETER) parsed and stored. BASED(pointer) and DEFINED(target) references captured. |
| 9 | Structures and arrays (up to 15 dims, LIKE, REFER, UNION) | YES (partial) | Structure levels 1-15, LIKE attribute, DIMENSION parsing implemented. Structure/Union PliType with members. REFER and BY NAME not implemented. |
| 10 | ~80 statement types | YES (partial) | ~28 statement types implemented: PROCEDURE, BEGIN, DECLARE, Assignment, IF/THEN/ELSE, DO (simple/WHILE/UNTIL/iterative/REPEAT), SELECT/WHEN/OTHERWISE, CALL, RETURN, GO TO, LEAVE, ITERATE, PUT, GET, OPEN, CLOSE, READ, WRITE, ON, SIGNAL, REVERT, ALLOCATE, FREE, DISPLAY, STOP, EXIT, labeled statements, preprocessor directives. Missing: FETCH, RELEASE, ATTACH, WAIT, DELAY, FLUSH, FORMAT, DELETE, LOCATE, REWRITE. |
| 11 | ~200 built-in functions | YES (partial) | ~85 built-in functions in `builtins.rs` (1315 lines): Arithmetic (ABS, MOD, CEIL, FLOOR, ROUND, TRUNC, MAX, MIN, SIGN, MULTIPLY, DIVIDE, REM), Math (SQRT, SIN/SIND, COS/COSD, TAN/TAND, ASIN/ASIND, ACOS/ACOSD, ATAN/ATAND, ATANH, LOG, LOG2, LOG10, EXP, ERF, ERFC), String (LENGTH, SUBSTR, INDEX, VERIFY, TRANSLATE, TRIM, LTRIM, RTRIM, LEFT, RIGHT, CENTER, COPY, REPEAT, REVERSE, SEARCH/SEARCHR, TALLY, LOWERCASE, UPPERCASE, MAXLENGTH, BIT, BOOL, COLLATE), Conversion (BINARY, DECIMAL, FIXED, FLOAT, CHAR, HEX, UNSPEC, HIGH, LOW, BYTE, RANK), DateTime (DATETIME, DATE, TIME, DAYS, DAYSTODATE, SECS, SECSTODAYS, WEEKDAY, Y4DATE), Storage (NULL, SYSNULL, ADDR, SIZE/STORAGE), Array (HBOUND, LBOUND, DIM, SUM, PROD), Misc (PLIRETV, ONCODE, ONLOC, DATAFIELD). |
| 12 | ~25 ON-conditions (exception handling) | YES | Full implementation in `exceptions.rs` (742 lines). All 22 conditions (AREA, ATTENTION, CONVERSION, ENDFILE, ENDPAGE, ERROR, FINISH, FIXEDOVERFLOW, INVALIDOP, KEY, NAME, OVERFLOW, RECORD, SIZE, STORAGE, STRINGRANGE, STRINGSIZE, SUBSCRIPTRANGE, TRANSMIT, UNDEFINEDFILE, UNDERFLOW, ZERODIVIDE) plus user-defined CONDITION(name). ON-unit stack with scope management, REVERT, SIGNAL, condition codes, enabled/disabled conditions (SIZE/STRINGRANGE/STRINGSIZE/SUBSCRIPTRANGE disabled by default), condition inquiry functions (ONCODE, ONLOC, ONCHAR, ONSOURCE, ONFILE, ONKEY, ONCOUNT). 25+ tests. |
| 13 | Stream I/O (GET/PUT LIST/EDIT/DATA) | YES (partial) | PUT LIST/EDIT/DATA and GET LIST/EDIT/DATA parsed with FILE, SKIP, PAGE, LINE options. PUT LIST executes in interpreter (writes to output buffer). EDIT format items not processed. |
| 14 | Record I/O (READ/WRITE/REWRITE/DELETE, VSAM) | YES (partial) | READ FILE(f) INTO(v) KEY(k) and WRITE FILE(f) FROM(v) KEYFROM(k) parsed. OPEN/CLOSE with attributes. Interpreter stubs only -- no actual file I/O. REWRITE, DELETE, LOCATE not in parser. |
| 15 | Preprocessor (%IF, %INCLUDE, %PROCEDURE, etc.) | YES (partial) | %INCLUDE, %XINCLUDE, %NOTE parsed and represented in AST. %IF/%THEN/%ELSE, %DECLARE, %assignment, %PROCEDURE, %ACTIVATE/%DEACTIVATE have AST node types but parser currently skips unrecognized directives. |
| 16 | EXEC CICS integration | GAP | Not implemented. Would need PL/I syntax variant adapter for existing CICS preprocessor. |
| 17 | EXEC SQL integration | GAP | Not implemented. Would need PL/I syntax variant adapter for existing DB2 preprocessor. |
| 18 | EXEC DLI integration | GAP | Not implemented. Would need PL/I syntax variant adapter for existing IMS preprocessor. |
| 19 | Multitasking (ATTACH, WAIT, EVENT) | GAP | TASK and EVENT types exist but ATTACH/WAIT/DELAY/PRIORITY/COMPLETION/STATUS statements not implemented. |
| 20 | FETCH/RELEASE (dynamic loading) | GAP | Not implemented. |
| 21 | Code generation | GAP | Interpreter-based execution only; no native z/Architecture or LLVM code generation. |
| 22 | Format items for EDIT I/O | GAP | Parser accepts EDIT mode but format items (A, F, E, B, P, COLUMN, LINE, X, SKIP, R) not parsed or evaluated. |
| 23 | PICTURE data type (numeric editing) | YES (partial) | DataType::Picture(spec) and PliType::Picture(spec) exist with storage_size calculation. No numeric editing runtime (drifting signs, insertion chars, exponent spec). |
| 24 | Interpreter core | YES | Full interpreter in `interpreter.rs` (1384 lines). Expression evaluation with type coercion, control flow (IF/DO/SELECT), procedures with recursion and scoped variables, assignment with automatic type conversion, built-in function dispatch. 30+ tests including factorial, fibonacci, nested loops. |
| 25 | Type system and conversions | YES | Comprehensive type system in `types.rs` (990 lines). PliType with 20 variants, PliValue with 13 variants, TypeCategory classification, can_convert_to rules, convert_value engine (arithmetic-to-arithmetic, arithmetic-to-string, string-to-arithmetic, bit conversions, graphic conversions), arithmetic_result_type, concat_result_type, comparison_common_type. 30+ tests. |

### Summary

The `open-mainframe-pli` crate contains a substantial PL/I implementation across 7 source files (~7,530 lines total):

- **lexer.rs** (825 lines) -- Complete free-form lexer with context-sensitive keyword handling
- **parser.rs** (2,250 lines) -- Full recursive-descent parser producing typed AST
- **types.rs** (989 lines) -- Complete type system with 20 types, runtime values, conversion engine
- **interpreter.rs** (1,383 lines) -- Working interpreter with expression evaluation, control flow, procedures
- **builtins.rs** (1,314 lines) -- ~85 built-in functions across 7 categories
- **exceptions.rs** (741 lines) -- Full ON-condition system with all 22+ conditions
- **lib.rs** (28 lines) -- Public re-exports

**Coverage**: 17 of 25 features are fully or partially implemented. The main remaining gaps are EXEC CICS/SQL/DLI integration (which can leverage existing preprocessors with PL/I syntax adapters), multitasking, FETCH/RELEASE, code generation, EDIT format items, and PICTURE numeric editing runtime.
