# Gap Analysis: Natural (Software AG 4GL)

## Official Specification Summary

Natural is a proprietary fourth-generation programming language (4GL) developed by Software AG, first released in 1979 as the application development complement to the ADABAS database. Natural is widely deployed in European enterprises (government, banking, insurance) and powers mission-critical mainframe applications. Software AG's "Adabas & Natural 2050+" agenda guarantees support and development beyond 2050.

Natural is classified as **Common** (enterprise 4GL) on mainframes:
- Tightly coupled with ADABAS (inverted-list database) via DDM (Data Definition Module) logical views
- Supports both batch and online (CICS, Com-plete) transaction processing
- Two programming modes: **Structured Mode** (enforced top-down, no GOTO) and **Reporting Mode** (quick ad-hoc, relaxed syntax)
- 130+ statements across 14 functional categories
- ~70+ system variables, ~25 system functions
- 11 data types including date (D) and time (T) native types
- 12+ program object types (Program, Subprogram, Subroutine, Helproutine, Map, Copycode, Data Areas, Class, etc.)
- Built-in Map Editor for 3270 screen design
- Natural Security for library/application-level access control
- Natural RPC via EntireX for distributed computing

Key documentation:
- **Natural for Mainframes 9.1.1** — primary reference (documentation.softwareag.com)
- **Natural Statements Reference** — complete DML/DDL statement set
- **Natural System Variables / System Functions** — runtime introspection
- **Natural Programming Guide** — coding conventions, modes, object types

## Key Features & Capabilities

### 1. Data Types (11 types)

| Type Code | Name | Description |
|-----------|------|-------------|
| A | Alphanumeric | Fixed-length character strings (1–253 bytes, or dynamic up to 1GB) |
| B | Binary | Binary data (fixed or dynamic length) |
| C | Attribute Control | Screen attribute control variable (color, intensity, etc.) |
| D | Date | Native date type with date arithmetic support |
| F | Floating Point | IEEE 754 floating point (4 or 8 bytes) |
| I | Integer | Binary integer (1, 2, or 4 bytes) |
| L | Logical | Boolean (TRUE/FALSE) |
| N | Numeric | Unpacked (zoned) decimal |
| P | Packed Numeric | Packed decimal (BCD) — most common for business math |
| T | Time | Native timestamp (date + time combined) |
| U | Unicode | Unicode character strings (UTF-16) |

### 2. Program Object Types (~12 types)

| Type | Name | Purpose |
|------|------|---------|
| P | Program | Main executable — entry point, can FETCH other programs |
| N | Subprogram | Called via CALLNAT — own data area, parameter passing only |
| S | Subroutine | External subroutine — PERFORM from calling program |
| H | Helproutine | Online help text — invoked by PF-key or automatic help |
| M | Map | 3270 screen layout — designed in Map Editor |
| C | Copycode | Source include — inserted at compile time via INCLUDE |
| G | Global Data Area (GDA) | Shared data definitions across programs in a session |
| L | Local Data Area (LDA) | Pre-defined data layout for a single program |
| A | Parameter Data Area (PDA) | Parameter definitions for CALLNAT interfaces |
| T | Text | Documentation/help text objects |
| Class | Class | OO programming — DEFINE CLASS (Natural v6.1+) |
| Interface | Interface | OO interface definition |
| Adapter | Adapter | Integration adapter objects |

### 3. Statements — Database Access & Update (~25 DML statements)

#### ADABAS DML Statements
| Statement | Purpose |
|-----------|---------|
| READ | Sequential read through database (by ISN or descriptor) |
| READLOB | Read large object (LOB) data |
| FIND | Search with selection criteria (WHERE/WITH clause) |
| HISTOGRAM | Read descriptor value distribution (counts per value) |
| GET | Direct read by ISN (Internal Sequence Number) |
| GET SAME | Re-read current record |
| STORE | Insert new record |
| UPDATE | Modify current record |
| UPDATELOB | Update large object data |
| DELETE | Remove current record |
| END TRANSACTION | Commit (ET logic) — release held records |
| BACKOUT TRANSACTION | Rollback (BT logic) — undo changes |
| GET TRANSACTION DATA | Retrieve ET data stored with last END TRANSACTION |
| RETRY | Retry after database deadlock |
| ACCEPT/REJECT | Filter records within processing loop |
| PASSW | Supply ADABAS file password |
| LIMIT | Set maximum record count for loop |
| AT START OF DATA | Execute before first record |
| AT END OF DATA | Execute after last record |
| AT BREAK | Execute on control break (field value change) |
| BEFORE BREAK PROCESSING | Execute before break processing |
| PERFORM BREAK PROCESSING | Force break processing |

#### Natural SQL Statements (for DB2/SQL databases)
| Statement | Purpose |
|-----------|---------|
| SELECT | SQL query with INTO clause |
| INSERT | SQL insert |
| UPDATE | SQL update |
| DELETE | SQL delete |
| COMMIT | SQL commit |
| ROLLBACK | SQL rollback |
| PROCESS SQL | Execute arbitrary SQL |
| CALLDBPROC | Call stored procedure |
| MERGE | SQL merge (upsert) |
| READ RESULT SET | Read stored procedure result set |

### 4. Statements — Arithmetic & Data Movement (~12 statements)

| Statement | Purpose |
|-----------|---------|
| COMPUTE | General arithmetic/assignment (supports complex expressions) |
| ADD | Add values |
| SUBTRACT | Subtract values |
| MULTIPLY | Multiply values |
| DIVIDE | Divide values |
| MOVE | Assign/transfer data between variables |
| MOVE ALL | Fill variable with repeated pattern |
| COMPRESS | Concatenate fields into one string (with delimiters) |
| SEPARATE | Split string into multiple fields |
| EXAMINE | Search/replace within strings (EXAMINE...FOR...REPLACE) |
| EXAMINE TRANSLATE | Translate characters via translation table |
| RESET | Initialize variables to default values |

### 5. Statements — Control Flow & Loops (~8 statements)

| Statement | Purpose |
|-----------|---------|
| IF / THEN / ELSE / END-IF | Conditional branching |
| DECIDE FOR | Multi-way conditional (first true wins) |
| DECIDE ON | Multi-way conditional on single variable (CASE) |
| IF SELECTION | Check which input fields were modified |
| FOR | Iterative counting loop |
| REPEAT / UNTIL / WHILE / END-REPEAT | General loop construct |
| ESCAPE | Exit loop (ESCAPE TOP/BOTTOM/ROUTINE/MODULE) |
| SORT | Sort records in processing loop (creates new loop) |

### 6. Statements — Output & Reports (~14 statements)

| Statement | Purpose |
|-----------|---------|
| DISPLAY | Columnar output with auto-headers |
| WRITE | Free-format output |
| PRINT | Print to specific printer/destination |
| WRITE TITLE | Define page title |
| WRITE TRAILER | Define page trailer |
| AT TOP OF PAGE | Execute at start of each page |
| AT END OF PAGE | Execute at end of each page |
| FORMAT | Set output formatting parameters |
| SKIP | Skip blank lines in output |
| EJECT | Force page break |
| NEWPAGE | Conditional page break |
| SUSPEND IDENTICAL SUPPRESS | Control duplicate value suppression |
| DEFINE PRINTER | Define logical printer |
| CLOSE PRINTER | Close/release printer |

### 7. Statements — Screen/Interactive I/O (~8 statements)

| Statement | Purpose |
|-----------|---------|
| INPUT | Display map/accept terminal input |
| REINPUT | Re-display map with error message |
| DEFINE WINDOW | Define pop-up window |
| SET WINDOW | Activate/deactivate window |
| PROCESS PAGE | Natural Advanced Facilities page processing |
| PROCESS PAGE USING | Process page with specified map |
| PROCESS PAGE UPDATE | Update existing page display |
| PROCESS PAGE MODAL | Modal dialog page processing |

### 8. Statements — Programs & Routines (~9 statements)

| Statement | Purpose |
|-----------|---------|
| CALLNAT | Call subprogram (N-type) with parameters |
| FETCH | Load and execute a program (P-type) — replaces caller |
| FETCH RETURN | Load and execute, then return to caller |
| PERFORM | Call internal or external subroutine |
| DEFINE SUBROUTINE | Define inline subroutine block |
| CALL | Call external (non-Natural) program (assembler, C, etc.) |
| CALL FILE | Call program by filename |
| CALL LOOP | Initiate nested processing loop |
| RUN | Execute a Natural program (clears data) |
| PROCESS COMMAND | Issue Natural system command programmatically |

### 9. Statements — Work File I/O (~6 statements)

| Statement | Purpose |
|-----------|---------|
| READ WORK FILE | Read from sequential work file |
| WRITE WORK FILE | Write to sequential work file |
| CLOSE WORK FILE | Close work file |
| DOWNLOAD PC FILE | Transfer data to PC file |
| UPLOAD PC FILE | Transfer data from PC file |
| DEFINE WORK FILE | Define work file attributes |

### 10. Statements — Object-Oriented (~6 statements, Natural v6.1+)

| Statement | Purpose |
|-----------|---------|
| DEFINE CLASS | Define a class with methods/properties |
| CREATE OBJECT | Instantiate a class |
| SEND METHOD | Invoke a method on an object |
| INTERFACE | Define interface contract |
| METHOD | Implement a method |
| PROPERTY | Define a property accessor |

### 11. Statements — Memory Management & Other (~12 statements)

| Statement | Purpose |
|-----------|---------|
| EXPAND | Expand dynamic variable/array |
| REDUCE | Reduce dynamic variable/array |
| RESIZE | Resize dynamic variable/array |
| DEFINE DATA | Define program's data area (local, global, parameter views) |
| END | End of block |
| INCLUDE | Include copycode at compile time |
| ON ERROR | Establish error handler |
| RELEASE | Release sets/ISN lists |
| SET CONTROL | Control terminal attributes |
| SET KEY | Assign PF/PA key functions |
| SET GLOBALS | Set global session parameters |
| SET TIME | Set CPU time limit |
| STACK | Place data on command stack |
| STOP | Terminate program normally |
| TERMINATE | Terminate Natural session |
| OPEN CONVERSATION | Begin RPC conversation |
| CLOSE CONVERSATION | End RPC conversation |
| DEFINE DATA CONTEXT | Context for RPC |
| PARSE | Parse XML documents |
| REQUEST DOCUMENT | HTTP request (web services) |

### 12. Reporting Mode Only Statements

| Statement | Purpose |
|-----------|---------|
| LOOP / CLOSE LOOP | Close processing loop (structured mode uses END-READ, etc.) |
| DO / DOEND | Block delimiters |
| OBTAIN | Retrieve field values from database |
| REDEFINE | Redefine field format/structure |

### 13. System Variables (~70+)

#### Application-Related
| Variable | Content |
|----------|---------|
| *APPLIC-ID / *LIBRARY-ID | Current library name |
| *PROGRAM | Currently executing program name |
| *LEVEL | Program nesting level |
| *COUNTER(r) | Loop iteration counter for statement r |
| *NUMBER(r) | Record count from FIND/HISTOGRAM |
| *ISN(r) | Current ADABAS Internal Sequence Number |
| *LBOUND / *UBOUND | Array bounds |
| *LENGTH(field) | Runtime length of dynamic field |
| *OCCURRENCE | Number of occurrences in periodic group |
| *LINE(r) / *LINEX(r) | Current output line number |
| *PAGE-EVENT | Page overflow event flag |
| *DATA | Data available on stack (YES/NO) |
| *ERROR-NR | Error number that triggered ON ERROR |
| *ERROR-LINE | Line number where error occurred |
| *ERROR-TA | Transaction code associated with error |
| *COM | Communication area (256 bytes shared between programs) |
| *ETID | ADABAS ET-ID for transaction tracking |
| *CPU-TIME | CPU time consumed |
| *STARTUP | Startup program name |
| *STEPLIB | Steplib (chained library) |
| *SUBROUTINE | Current subroutine name |
| *TYPE | Object type (Program, Subprogram, etc.) |
| *OBJECT-TYPE | Detailed object type |
| *THIS-OBJECT | Reference to current object (OO) |

#### Date/Time Variables
| Variable | Content |
|----------|---------|
| *DATX | Current date (D format — for date arithmetic) |
| *TIMX | Current time (T format — date + time) |
| *DAT4I | Date YYYY-MM-DD |
| *DAT4U | Date MM/DD/YYYY |
| *DAT4E | Date DD/MM/YYYY |
| *DATE | Date in session date format |
| *TIME | Current time HH:II:SS |
| *TIMESTMP | Timestamp (26-digit packed) |

#### Screen/I/O Variables
| Variable | Content |
|----------|---------|
| *PF-KEY | Last PF/PA key pressed |
| *PF-NAME | Name assigned to PF key |
| *CURS-COL | Cursor column position |
| *CURS-LINE | Cursor line position |
| *CURS-FIELD | Field name at cursor position |
| *WINDOW-LS | Window line size |
| *WINDOW-PS | Window page size |
| *WINDOW-POS | Window position |

#### Environment Variables
| Variable | Content |
|----------|---------|
| *USER | Current Natural Security user ID |
| *INIT-USER | Operating system user ID |
| *INIT-ID | Terminal ID |
| *INIT-PROGRAM | Startup program |
| *DEVICE | Device type |
| *HARDWARE | Hardware platform identifier |
| *LANGUAGE | Session language code |
| *OPSYS / *OS | Operating system |
| *NATVERS | Natural version |
| *TPSYS | TP monitor (CICS, Com-plete, etc.) |
| *GROUP | User group (Natural Security) |
| *NET-USER | Network user ID |
| *SCREEN-IO | Screen I/O active flag |
| *BROWSER-IO | Browser I/O active flag |
| *CODEPAGE | Active code page |
| *MACHINE-CLASS | Machine class (mainframe, UNIX, Windows) |
| *LOCALE | Locale setting |
| *HOSTNAME | Host name |
| *SERVER-TYPE | Server type identifier |

### 14. System Functions (~25 functions)

#### Mathematical Functions
| Function | Purpose |
|----------|---------|
| ABS(x) | Absolute value |
| ATN(x) | Arc tangent |
| COS(x) | Cosine |
| EXP(x) | Exponential (e^x) |
| FRAC(x) | Fractional part |
| INT(x) | Integer part |
| LOG(x) | Natural logarithm |
| SGN(x) | Sign (-1, 0, +1) |
| SIN(x) | Sine |
| SQRT(x) | Square root |
| TAN(x) | Tangent |
| VAL(x) | Extract numeric value from alphanumeric |

#### Aggregation Functions (within loops)
| Function | Purpose |
|----------|---------|
| AVER(r)(field) | Average |
| COUNT(r) | Record count |
| MAX(r)(field) | Maximum value |
| MIN(r)(field) | Minimum value |
| NAVER(r)(field) | Average excluding nulls |
| NCOUNT(r)(field) | Count excluding nulls |
| NMIN(r)(field) | Minimum excluding nulls |
| OLD(r)(field) | Previous value |
| SUM(r)(field) | Sum |
| TOTAL(r)(field) | Running total |
| SORTKEY(field) | Locale-sensitive sort key |

### 15. DDM (Data Definition Module) / ADABAS Integration

Natural accesses ADABAS through DDMs — logical views of physical database files:

```natural
DEFINE DATA LOCAL
  1 EMPLOYEE VIEW OF EMPLOYEES
    2 PERSONNEL-ID (A8)
    2 FIRST-NAME   (A20)
    2 NAME         (A25)
    2 SALARY       (P7.2)
    2 DEPT         (A6)
END-DEFINE
```

Key DDM concepts:
- **File Number**: Each DDM maps to an ADABAS file number
- **Field Mapping**: ADABAS 2-character names mapped to descriptive names (up to 32 chars)
- **Descriptors**: Fields indexed for searching — DE (descriptor), SPR (superdescriptor), SUB (subdescriptor), PHO (phonetic), HYP (hyperdescriptor)
- **Periodic Groups**: Repeating groups within a record (1:N within a row)
- **Multiple-Value Fields**: Multi-valued fields within a record (arrays in a column)
- **ISN**: Internal Sequence Number — ADABAS record identifier
- **ET/BT Logic**: END TRANSACTION (commit) / BACKOUT TRANSACTION (rollback)
- **SYSDDM Utility**: Maintains DDM definitions

### 16. Natural Map Editor

The Map Editor provides WYSIWYG 3270 screen design:
- Visual layout of input/output fields
- Automatic field attribute control (color, intensity, protection)
- Field validation rules (format, range, mandatory)
- Multiple pages per map
- Delimiter characters for field boundaries
- Help text association
- Map testing within editor
- Map objects stored as M-type in library

### 17. Natural Security

Library and application-level access control:
- Library-level protection (LOGON required)
- Program-level protection (execute restrictions)
- User profiles with group membership
- Command restrictions per user/group
- Utility restrictions
- DDM access restrictions
- Integrated with OS-level security (RACF, ACF2, Top Secret)

### 18. Natural RPC (Remote Procedure Call)

Via EntireX middleware:
- CALLNAT across address spaces and platforms
- Conversation-based RPC (OPEN/CLOSE CONVERSATION)
- Natural as RPC server or client
- Supports Natural-to-Natural and Natural-to-non-Natural calls
- DEFINE DATA CONTEXT for conversation state

### 19. System Commands (~30+ commands)

| Command | Purpose |
|---------|---------|
| LOGON | Connect to a library |
| LOGOFF | Disconnect from Natural |
| EDIT | Open source editor for an object |
| CHECK | Syntax check a source |
| CATALOG | Compile and save a cataloged (executable) object |
| STOW | Save source + catalog in one step |
| CATALL | Catalog all objects in library |
| RUN | Execute current source |
| EXECUTE | Execute a cataloged object |
| LIST | List objects in library |
| SCAN | Search for string across objects |
| FIND | Find objects by name pattern |
| SAVE | Save source without cataloging |
| PURGE | Delete an object from library |
| RENAME | Rename an object |
| UNCATALOG | Remove cataloged object (keep source) |
| CLEAR | Clear editor buffer |
| GLOBALS | Display/set global parameters |
| SYSDDM | DDM maintenance utility |
| SYSERR | Error message maintenance |
| SYSMAIN | Object maintenance utility |
| SYSEXT | Natural Application Programming Interfaces |
| SYSPARM | Parameter maintenance |
| SYSRPC | RPC maintenance |

### 20. Error Handling

```natural
DEFINE DATA LOCAL
  1 #ERR-NUM (N4)
  1 #ERR-LINE (N4)
END-DEFINE
*
ON ERROR
  COMPUTE #ERR-NUM = *ERROR-NR
  COMPUTE #ERR-LINE = *ERROR-LINE
  WRITE 'Error' #ERR-NUM 'at line' #ERR-LINE
  ESCAPE ROUTINE
END-ERROR
```

- ON ERROR block catches runtime errors
- *ERROR-NR — error number (0–9999)
- *ERROR-LINE — source line number
- *ERROR-TA — transaction ID associated with error
- ESCAPE ROUTINE — exit error handler (resume or terminate)
- Error numbers: 0001–0999 (Natural system), 1000–8999 (application via REINPUT WITH TEXT *nnnn)

## Current OpenMainframe Status

**No Natural implementation exists.** A comprehensive search across all crates returned zero matches for "Natural" (as language), "ADABAS", "CALLNAT", "DDM" (as Data Definition Module), or "Software AG". The only matches were:
- "Natural logarithm" in COBOL intrinsic math functions (unrelated)
- "DDMMYYYY" date format strings (unrelated)
- RALPH-PROMPT.md and planning documents listing Natural as a gap

The ADABAS database (Batch 16) is also completely missing — Natural depends on it.

## Gap Details

| Feature | Official Natural | OpenMainframe | Gap |
|---------|-----------------|---------------|-----|
| Natural lexer/compiler (structured mode) | Full — 130+ statements | None | **Missing** |
| Natural lexer/compiler (reporting mode) | Full — relaxed syntax | None | **Missing** |
| 11 data types (A/B/C/D/F/I/L/N/P/T/U) | Full | None | **Missing** |
| ~12 program object types (P/N/S/H/M/C/G/L/A/T/Class) | Full | None | **Missing** |
| DEFINE DATA (local, global, parameter, views) | Full | None | **Missing** |
| ADABAS DML (~25 statements: READ/FIND/HISTOGRAM/GET/STORE/UPDATE/DELETE/ET/BT) | Full | None | **Missing** |
| SQL statements (~10: SELECT/INSERT/UPDATE/DELETE/PROCESS SQL/CALLDBPROC) | Full | None | **Missing** |
| Arithmetic & data movement (~12: COMPUTE/MOVE/COMPRESS/SEPARATE/EXAMINE) | Full | None | **Missing** |
| Control flow (~8: IF/DECIDE FOR/DECIDE ON/FOR/REPEAT/ESCAPE/SORT) | Full | None | **Missing** |
| Output/reporting (~14: DISPLAY/WRITE/PRINT/WRITE TITLE/AT TOP OF PAGE) | Full | None | **Missing** |
| Interactive I/O (~8: INPUT/REINPUT/DEFINE WINDOW/PROCESS PAGE) | Full | None | **Missing** |
| Program invocation (~9: CALLNAT/FETCH/PERFORM/DEFINE SUBROUTINE/CALL) | Full | None | **Missing** |
| Work file I/O (~6: READ/WRITE/CLOSE WORK FILE) | Full | None | **Missing** |
| Object-oriented (~6: DEFINE CLASS/CREATE OBJECT/SEND METHOD) | Full | None | **Missing** |
| Memory management (EXPAND/REDUCE/RESIZE) | Full | None | **Missing** |
| ~70+ system variables (*COUNTER, *ISN, *NUMBER, *DATX, *USER, etc.) | Full | None | **Missing** |
| ~25 system functions (ABS, SIN, COS, VAL, AVER, COUNT, MAX, MIN, SUM, OLD) | Full | None | **Missing** |
| DDM (Data Definition Module) — ADABAS file views | Full | None (no ADABAS) | **Missing** |
| Map Editor — 3270 screen design | Full | None | **Missing** |
| Natural Security — library/program/DDM access control | Full | None | **Missing** |
| Natural RPC — EntireX remote procedure calls | Full | None | **Missing** |
| ~30+ system commands (LOGON, EDIT, CATALOG, STOW, LIST, SCAN, etc.) | Full | None | **Missing** |
| ON ERROR handling (*ERROR-NR, *ERROR-LINE, ESCAPE ROUTINE) | Full | None | **Missing** |
| INCLUDE/Copycode — compile-time source inclusion | Full | None | **Missing** |
| PARSE — XML parsing | Full | None | **Missing** |
| REQUEST DOCUMENT — HTTP/web services | Full | None | **Missing** |

## Proposed Epic Structure

### Epic N100: Natural Lexer and Parser
- **N100.1**: Source format — line-based, comment styles (`*`, `**`, `/*`), continuation, level-number data definitions
- **N100.2**: Tokenizer — keywords (130+ statements), identifiers, literals, system variables (`*` prefix), operators
- **N100.3**: Statement parser — structured mode (all statement types)
- **N100.4**: Expression parser — arithmetic, string, comparison, Boolean (AND/OR/NOT), MASK/SCAN patterns
- **N100.5**: Reporting mode variant — LOOP/DOEND, OBTAIN, REDEFINE, relaxed rules
- **Complexity**: M (Medium — English-like syntax, but many statement forms)

### Epic N101: Natural Data Model
- **N101.1**: 11 data types — A, B, C, D, F, I, L, N, P, T, U with format/length specification
- **N101.2**: DEFINE DATA — local, global (GDA), parameter (PDA) data areas, level numbers
- **N101.3**: Arrays — 1D, 2D, 3D arrays, index notation, *OCCURRENCE, *LBOUND/*UBOUND
- **N101.4**: Dynamic variables — EXPAND/REDUCE/RESIZE, *LENGTH
- **N101.5**: Redefine — overlay different formats on same storage (REDEFINE in data area)
- **N101.6**: Date/Time arithmetic — D and T type operations, native date math
- **Complexity**: M (Medium)

### Epic N102: Natural Interpreter Core
- **N102.1**: Control flow — IF/ELSE/END-IF, DECIDE FOR/ON, FOR, REPEAT/WHILE/UNTIL
- **N102.2**: ESCAPE (TOP/BOTTOM/ROUTINE/MODULE) — loop/routine exit control
- **N102.3**: Program invocation — CALLNAT (subprogram), PERFORM (subroutine), FETCH (program), RUN
- **N102.4**: DEFINE SUBROUTINE / PERFORM — inline and external subroutines
- **N102.5**: INCLUDE/Copycode — compile-time source inclusion
- **N102.6**: CALL — external (non-Natural) program invocation
- **Complexity**: M (Medium)

### Epic N103: Natural Data Manipulation
- **N103.1**: Arithmetic — COMPUTE, ADD, SUBTRACT, MULTIPLY, DIVIDE
- **N103.2**: MOVE / MOVE ALL — data transfer with type conversion rules
- **N103.3**: COMPRESS — string concatenation with delimiter control
- **N103.4**: SEPARATE — string splitting
- **N103.5**: EXAMINE — search/replace with multiple options (FULL, GIVING POSITION, GIVING LENGTH, GIVING NUMBER, REPLACE)
- **N103.6**: EXAMINE TRANSLATE — character translation
- **N103.7**: RESET — variable initialization
- **N103.8**: SORT — in-memory sort of loop results
- **Complexity**: M (Medium)

### Epic N104: Natural ADABAS Database Access
- **N104.1**: DDM definitions — file number mapping, field names, descriptors, periodic groups, MU fields
- **N104.2**: READ — sequential by ISN or descriptor, WITH clause, WHERE clause
- **N104.3**: FIND — search with selection criteria, SORTED BY, multi-fetch
- **N104.4**: HISTOGRAM — descriptor value distribution
- **N104.5**: GET / GET SAME — direct ISN access
- **N104.6**: STORE / UPDATE / DELETE — record modification
- **N104.7**: END TRANSACTION / BACKOUT TRANSACTION — ET/BT logic, record locking
- **N104.8**: ACCEPT/REJECT, LIMIT, AT START/END OF DATA, AT BREAK
- **Complexity**: L (Large — requires ADABAS database implementation)

### Epic N105: Natural SQL Database Access
- **N105.1**: SELECT — SQL query with INTO clause, cursor-based
- **N105.2**: INSERT / UPDATE / DELETE — SQL DML
- **N105.3**: COMMIT / ROLLBACK — transaction control
- **N105.4**: PROCESS SQL — arbitrary SQL pass-through
- **N105.5**: CALLDBPROC / READ RESULT SET — stored procedure invocation
- **N105.6**: MERGE — SQL upsert
- **Complexity**: M (Medium — can leverage existing DB2 crate)

### Epic N106: Natural Output & Reporting
- **N106.1**: DISPLAY — columnar output with automatic headers
- **N106.2**: WRITE — free-format output with positioning
- **N106.3**: PRINT — directed output to printer/file
- **N106.4**: Page control — WRITE TITLE/TRAILER, AT TOP/END OF PAGE, SKIP, EJECT, NEWPAGE
- **N106.5**: SUSPEND IDENTICAL SUPPRESS — duplicate value suppression
- **N106.6**: DEFINE/CLOSE PRINTER — logical printer management
- **N106.7**: Control breaks — AT BREAK, BEFORE BREAK PROCESSING, PERFORM BREAK PROCESSING
- **Complexity**: M (Medium)

### Epic N107: Natural Interactive I/O & Maps
- **N107.1**: INPUT statement — display map, accept terminal input
- **N107.2**: REINPUT — redisplay with error positioning
- **N107.3**: Map objects — field layout, attributes, validation rules
- **N107.4**: DEFINE WINDOW / SET WINDOW — pop-up window management
- **N107.5**: PROCESS PAGE — Natural Advanced Facilities page processing
- **N107.6**: SET KEY — PF/PA key assignment
- **N107.7**: SET CONTROL — terminal attribute control
- **Complexity**: M (Medium — builds on existing TUI crate)

### Epic N108: Natural System Variables & Functions
- **N108.1**: Application variables — *COUNTER, *ISN, *NUMBER, *PROGRAM, *LEVEL, *DATA, *COM
- **N108.2**: Date/Time variables — *DATX, *TIMX, *DAT4I, *DATE, *TIME, *TIMESTMP
- **N108.3**: Screen/I/O variables — *PF-KEY, *CURS-COL, *CURS-LINE, *CURS-FIELD
- **N108.4**: Environment variables — *USER, *LIBRARY-ID, *DEVICE, *HARDWARE, *OPSYS, *NATVERS
- **N108.5**: Error variables — *ERROR-NR, *ERROR-LINE, *ERROR-TA
- **N108.6**: Mathematical functions — ABS, SIN, COS, TAN, ATN, LOG, EXP, SQRT, SGN, INT, FRAC, VAL
- **N108.7**: Aggregation functions — AVER, COUNT, MAX, MIN, SUM, TOTAL, OLD, NAVER, NCOUNT, NMIN, SORTKEY
- **Complexity**: M (Medium)

### Epic N109: Natural Error Handling & Work Files
- **N109.1**: ON ERROR / END-ERROR — error trap establishment
- **N109.2**: Error propagation — nested program error handling
- **N109.3**: ESCAPE ROUTINE — exit from error handler
- **N109.4**: Work file I/O — READ/WRITE/CLOSE WORK FILE, DEFINE WORK FILE
- **N109.5**: STACK — command/data stack manipulation
- **N109.6**: STOP / TERMINATE — program and session termination
- **Complexity**: S (Small)

### Epic N110: Natural System Commands & Environment
- **N110.1**: Session management — LOGON, LOGOFF, GLOBALS
- **N110.2**: Development commands — EDIT, CHECK, CATALOG, STOW, SAVE, RUN, EXECUTE
- **N110.3**: Library management — LIST, SCAN, FIND, PURGE, RENAME, UNCATALOG, CATALL
- **N110.4**: Utilities — SYSDDM, SYSERR, SYSMAIN, SYSEXT, SYSPARM
- **N110.5**: PROCESS COMMAND — programmatic command execution
- **Complexity**: M (Medium)

### Epic N111: Natural Security & RPC
- **N111.1**: Library-level protection — LOGON enforcement, allowed libraries
- **N111.2**: User/group profiles — command restrictions, utility restrictions
- **N111.3**: DDM access restrictions — read/write control per DDM
- **N111.4**: Natural RPC client — CALLNAT across address spaces via EntireX
- **N111.5**: Natural RPC server — expose CALLNAT as remote service
- **N111.6**: OPEN/CLOSE CONVERSATION, DEFINE DATA CONTEXT
- **Complexity**: M (Medium)

### Epic N112: Natural Object-Oriented & Advanced Features
- **N112.1**: DEFINE CLASS / METHOD / PROPERTY — class definitions
- **N112.2**: CREATE OBJECT / SEND METHOD — object instantiation and method calls
- **N112.3**: INTERFACE — interface definitions
- **N112.4**: PARSE — XML document parsing
- **N112.5**: REQUEST DOCUMENT — HTTP web service calls
- **Complexity**: M (Medium)

## Dependencies

| Dependency | Crate / Component | Reason |
|------------|-------------------|--------|
| open-mainframe-encoding | EBCDIC support | Natural runs in EBCDIC on mainframes |
| open-mainframe-dataset | Work file I/O | READ/WRITE WORK FILE uses sequential datasets |
| open-mainframe-tui | 3270 terminal | Map (M-type) objects display on 3270 |
| open-mainframe-db2 | SQL access | Natural SQL statements can target DB2 |
| open-mainframe-runtime | Decimal arithmetic | Packed/zoned decimal math |
| ADABAS (not implemented, Batch 16) | Database access | READ/FIND/HISTOGRAM require ADABAS |
| Natural Security (new) | Access control | Library/program-level protection |
| EntireX/RPC (new) | Distributed computing | CALLNAT across platforms |
| CICS (existing) | TP monitor | Natural for CICS online processing |

**Critical dependency**: Natural is tightly coupled with ADABAS. While Natural can operate with SQL databases (DB2), the majority of Natural applications use ADABAS DML statements. The ADABAS implementation (Batch 16) should be co-developed or precede the ADABAS DML epic (N104).

## Complexity Estimate

| Epic | Complexity | Rationale |
|------|-----------|-----------|
| N100 (Lexer/Parser) | M | English-like syntax, 130+ keywords, two modes |
| N101 (Data Model) | M | 11 types, arrays, dynamic vars, date arithmetic |
| N102 (Interpreter Core) | M | Control flow, program invocation, subroutines |
| N103 (Data Manipulation) | M | COMPUTE, COMPRESS, EXAMINE are feature-rich |
| N104 (ADABAS Access) | L | Requires ADABAS database — biggest dependency |
| N105 (SQL Access) | M | Leverages existing DB2 crate |
| N106 (Output/Reporting) | M | Auto-headers, control breaks, page formatting |
| N107 (Interactive I/O) | M | Maps, windows, PF keys — leverages TUI crate |
| N108 (System Vars/Funcs) | M | 70+ variables, 25 functions — mostly lookup |
| N109 (Error/Work Files) | S | ON ERROR, work file I/O, STACK |
| N110 (System Commands) | M | Development environment commands |
| N111 (Security/RPC) | M | Security model + EntireX RPC |
| N112 (OO/Advanced) | M | Classes, XML, HTTP — later priority |

**Total estimated effort**: 13 epics, overall L (Large) complexity. Natural is a substantial 4GL with 130+ statements, its own screen editor, security subsystem, and tight ADABAS coupling. Significantly more complex than Easytrieve or CLIST, comparable to REXX in scope.

**Recommendation**: Implement in conjunction with ADABAS (Batch 16). Core language (N100–N103, N106, N108–N109) can proceed independently; database access (N104) and interactive features (N107) require ADABAS and TUI respectively.

## Reference Documentation

- [Natural for Mainframes 9.1.1 — Documentation Overview](https://documentation.softwareag.com/natural/nat911mf/overview.htm)
- [Natural Statements Grouped by Function (v8.2.8)](https://documentation.softwareag.com/natural/nat828mf/sm/stmt_groups.htm)
- [Natural System Variables Overview (v9.1.4)](https://documentation.softwareag.com/natural/nat914unx/vari/vari-over.htm)
- [Natural System Functions Overview (v9.1.4)](https://documentation.softwareag.com/natural/nat914unx/func/func-over.htm)
- [Natural System Functions — Math (v9.1.2)](https://documentation.softwareag.com/natural/nat912win/func/func_math.htm)
- [Natural Programming Guide (v9.1.3, PDF)](https://documentation.softwareag.com/natural/nat913win/print/pg.pdf)
- [Natural System Variables (v9.3.2, PDF)](https://documentation.softwareag.com/one/9.3.2/en/webhelp/one-webhelp/natwin/pdf/vari.pdf)
- [Natural for Mainframes 8.2.7 — Documentation](https://documentation.softwareag.com/natural/nat827mf/overview.htm)
- [ADABAS — Wikipedia](https://en.wikipedia.org/wiki/ADABAS)
- [Refactor Adabas & Natural on Azure — Microsoft Architecture Center](https://learn.microsoft.com/en-us/azure/architecture/example-scenario/mainframe/refactor-adabas-aks)
- [Software AG — IBM Z Mainframe](https://www.softwareag.com/en_corporate/platform/adabas-natural/ibm-z.html)
- [Getting Started with Adabas & Natural — Medium](https://medium.com/@mohamad.razzi.my/getting-started-with-adabas-natural-part-1-6597688406ad)
- [NATURAL Essentials — Self-Study Course (PDF)](https://www.spsimpson.com/nat-u/NATURAL%20Essentials.pdf)
- [Software AG Natural Code Samples — GitHub](https://github.com/SoftwareAG/adabas-natural-code-samples)
