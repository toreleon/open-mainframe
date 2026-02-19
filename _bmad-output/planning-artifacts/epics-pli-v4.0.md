# PL/I Crate — Epics & Stories

## Epic P100: PL/I Lexer and Parser

**Goal:** Build a lexer and parser for Enterprise PL/I, handling the language's lack of reserved words and context-sensitive syntax.

**Crate:** `open-mainframe-pli`
**FRs:** FR-v4.0-P100

### Story P100.1: Lexer — Free-Form Source and Token Classification

As a **PL/I developer**,
I want **PL/I source to be tokenized correctly despite having no reserved words**,
So that **identifiers, keywords, operators, and literals are distinguished by context**.

**Acceptance Criteria:**

**Given** `DECLARE X FIXED DECIMAL(7,2);`
**When** tokenized
**Then** DECLARE is classified as keyword-in-context, X as identifier, FIXED DECIMAL as type attributes

**Given** `IF = 1; /* IF used as variable name */`
**When** tokenized
**Then** IF is classified as an identifier (assigned to), not a keyword

**Given** `'Hello''World'` (embedded quotes in PL/I strings)
**When** tokenized
**Then** a single string literal "Hello'World" is produced

**Complexity:** L

### Story P100.2: Parser — DECLARE Statements with Full Attribute Syntax

As a **PL/I developer**,
I want **DECLARE statements parsed with all attribute combinations**,
So that **the full PL/I type system is representable in the AST**.

**Acceptance Criteria:**

**Given** `DCL 1 REC, 2 NAME CHAR(30) VARYING, 2 AGE FIXED BIN(31), 2 SALARY FIXED DEC(9,2);`
**When** parsed
**Then** a structure with level numbers and typed fields is constructed

**Given** `DCL PTR POINTER, AREA_X AREA(1000), BASED_REC BASED(PTR);`
**When** parsed
**Then** pointer and area types with BASED association are represented

**Complexity:** XL

### Story P100.3: Parser — Control Flow and I/O Statements

As a **PL/I developer**,
I want **all PL/I statement types parsed**,
So that **any valid PL/I program can be represented as an AST**.

**Acceptance Criteria:**

**Given** `DO I = 1 TO 100 BY 2 WHILE (X > 0);`
**When** parsed
**Then** a DO-iterative node with control variable, bounds, increment, and WHILE condition

**Given** `SELECT (CODE); WHEN (1) CALL PROC_A; WHEN (2,3) CALL PROC_B; OTHERWISE CALL PROC_ERR; END;`
**When** parsed
**Then** a SELECT node with WHEN clauses (including multi-value) and OTHERWISE

**Given** `ON ENDFILE(INFILE) EOF_FLAG = '1'B;`
**When** parsed
**Then** an ON-unit with condition(ENDFILE), file reference, and action statement

**Complexity:** L

### Story P100.4: Parser — Preprocessor (%IF, %INCLUDE, %PROCEDURE)

As a **PL/I developer**,
I want **the PL/I preprocessor to process %IF, %INCLUDE, and %PROCEDURE**,
So that **compile-time customization and source inclusion work correctly**.

**Acceptance Criteria:**

**Given** `%IF DEBUG = 'YES' %THEN %DO; PUT SKIP LIST('Debug mode'); %END;`
**When** preprocessed with DEBUG='YES'
**Then** the PUT statement is included in the source

**Given** `%INCLUDE COPYBOOK;`
**When** preprocessed
**Then** member COPYBOOK from SYSLIB is inserted at that point

**Complexity:** M

---

## Epic P101: PL/I Type System

**Goal:** Implement PL/I's comprehensive type system including arithmetic, string, pointer, and user-defined types with automatic conversion rules.

**Crate:** `open-mainframe-pli`
**FRs:** FR-v4.0-P101

### Story P101.1: Arithmetic Types

As a **PL/I developer**,
I want **FIXED DECIMAL, FIXED BINARY, FLOAT DECIMAL, FLOAT BINARY, and COMPLEX types**,
So that **I can perform exact decimal and binary arithmetic**.

**Acceptance Criteria:**

**Given** `DCL X FIXED DEC(7,2) INIT(12345.67);`
**When** X is stored
**Then** it uses packed decimal format with precision 7 and scale 2

**Given** `DCL Y FIXED BIN(31) INIT(2147483647);`
**When** Y is stored
**Then** it uses 32-bit signed binary integer

**Given** `DCL Z FLOAT DEC(16) INIT(3.14159265358979E0);`
**When** Z is stored
**Then** it uses IEEE 754 double precision (or HFP depending on compile options)

**Complexity:** L

### Story P101.2: String Types

As a **PL/I developer**,
I want **CHARACTER, BIT, GRAPHIC, and WIDECHAR types with fixed and varying lengths**,
So that **I can manipulate text and bit data**.

**Acceptance Criteria:**

**Given** `DCL NAME CHAR(30) VARYING INIT('John');`
**When** stored
**Then** a varying-length string with current length 4 and maximum 30

**Given** `DCL FLAGS BIT(8) INIT('10110000'B);`
**When** stored
**Then** 8 bits with the specified pattern

**Complexity:** M

### Story P101.3: Pointer, Area, and Special Types

As a **PL/I developer**,
I want **POINTER, OFFSET, AREA, HANDLE, LABEL, ENTRY, and FILE types**,
So that **I can use dynamic storage and control flow constructs**.

**Acceptance Criteria:**

**Given** `DCL P POINTER; ALLOCATE REC SET(P);`
**When** executed
**Then** P points to the newly allocated REC structure

**Given** `DCL WORK_AREA AREA(10000); DCL BASED_VAR CHAR(100) BASED;`
**When** `ALLOCATE BASED_VAR IN(WORK_AREA)`
**Then** storage is allocated from WORK_AREA and BASED_VAR is located there

**Complexity:** L

### Story P101.4: Implicit Type Conversion Rules

As a **PL/I developer**,
I want **automatic type conversions between all compatible types**,
So that **mixed-type expressions evaluate correctly per PL/I rules**.

**Acceptance Criteria:**

**Given** `DCL X FIXED DEC(5); DCL Y FIXED BIN(15); X = Y;`
**When** assignment executes
**Then** Y is converted from binary to decimal automatically

**Given** `DCL S CHAR(10); DCL N FIXED DEC(5); S = N;`
**When** assignment executes
**Then** N is converted to character representation and padded/truncated to length 10

PL/I conversion rules cover: FIXED↔FLOAT, DEC↔BIN, arithmetic↔CHAR, arithmetic↔BIT, CHAR↔BIT

**Complexity:** XL

---

## Epic P102: PL/I Interpreter Core

**Goal:** Implement the PL/I expression evaluator, control flow, and procedure calling with proper scoping.

**Crate:** `open-mainframe-pli`
**FRs:** FR-v4.0-P102

### Story P102.1: Expression Evaluator with Type Coercion

As a **PL/I developer**,
I want **full expression evaluation with automatic type promotion**,
So that **mixed-type arithmetic and string operations produce correct results**.

**Acceptance Criteria:**

**Given** `X = A + B * C;` where A is FIXED DEC(5), B is FIXED BIN(15), C is FLOAT DEC(6)
**When** evaluated
**Then** operands are promoted per PL/I rules: B*C promotes to FLOAT, then A is promoted, result assigned to X with conversion

**Complexity:** L

### Story P102.2: Control Flow — IF, DO, SELECT, GO TO

As a **PL/I developer**,
I want **all PL/I control flow constructs**,
So that **I can write any PL/I program logic**.

**Acceptance Criteria:**

**Given** `DO I = 1 TO N WHILE (FLAG); ... END;`
**When** executed
**Then** loop iterates with both counter and WHILE condition checked each iteration

**Given** `SELECT (STATUS); WHEN ('A','B') CALL ACTIVE; WHEN ('I') CALL INACTIVE; OTHER CALL ERROR; END;`
**When** STATUS = 'A'
**Then** CALL ACTIVE is executed

**Complexity:** M

### Story P102.3: Procedures — CALL, RETURN, Recursion, ENTRY Attribute

As a **PL/I developer**,
I want **full procedure support with multiple entry points and recursion**,
So that **I can write modular PL/I programs**.

**Acceptance Criteria:**

**Given** `FACTORIAL: PROC(N) RETURNS(FIXED BIN(31)) RECURSIVE; IF N<=1 THEN RETURN(1); RETURN(N*FACTORIAL(N-1)); END;`
**When** `CALL FACTORIAL(10)`
**Then** returns 3628800

**Given** a procedure with `ENTRY ALTNAME(P1,P2)`
**When** `CALL ALTNAME(X,Y)` is executed
**Then** the procedure is entered at the ENTRY point with parameters P1=X, P2=Y

**Complexity:** L

---

## Epic P103: PL/I Storage Management

**Goal:** Implement CONTROLLED, BASED, DEFINED, and AREA storage classes with ALLOCATE/FREE.

**Crate:** `open-mainframe-pli`
**FRs:** FR-v4.0-P103

### Story P103.1: CONTROLLED and BASED Storage

As a **PL/I developer**,
I want **CONTROLLED (stack) and BASED (heap) storage allocation**,
So that **I can dynamically allocate and free data structures**.

**Acceptance Criteria:**

**Given** `DCL X FIXED DEC(5) CONTROLLED;` then `ALLOCATE X; X = 100; ALLOCATE X; X = 200;`
**When** `FREE X;` executes
**Then** X reverts to 100 (CONTROLLED maintains a stack of allocations)

**Given** `DCL P POINTER; DCL REC CHAR(80) BASED(P);` then `ALLOCATE REC;`
**When** `REC = 'data';` executes
**Then** the string is stored at P's address

**Complexity:** L

### Story P103.2: DEFINED Overlay and REFER

As a **PL/I developer**,
I want **DEFINED overlays and REFER for self-defining structures**,
So that **I can map different views onto the same storage**.

**Acceptance Criteria:**

**Given** `DCL A CHAR(4); DCL B FIXED BIN(31) DEFINED(A);`
**When** A is set to X'00000064'
**Then** B = 100 (overlay interpretation)

**Given** `DCL 1 VREC BASED(P), 2 LEN FIXED BIN(15), 2 DATA CHAR(N REFER(LEN));`
**When** `ALLOCATE VREC; LEN = 50;`
**Then** VREC is allocated with DATA having length 50

**Complexity:** L

---

## Epic P104: PL/I Built-in Functions (~200)

**Goal:** Implement the ~200 PL/I built-in functions across arithmetic, mathematical, string, conversion, date/time, array, and storage categories.

**Crate:** `open-mainframe-pli`
**FRs:** FR-v4.0-P104

### Story P104.1: Arithmetic and Mathematical Functions (~50)

As a **PL/I developer**,
I want **arithmetic functions (ABS, MOD, CEIL, FLOOR, etc.) and math functions (SIN, COS, SQRT, LOG, etc.)**,
So that **I can perform mathematical computations**.

**Acceptance Criteria:**

**Given** `X = SQRT(144.0);`
**When** executed
**Then** X = 12.0

**Given** `Y = MOD(17, 5);`
**When** executed
**Then** Y = 2

**Complexity:** M

### Story P104.2: String and Conversion Functions (~45)

As a **PL/I developer**,
I want **string manipulation (INDEX, SUBSTR, LENGTH, TRANSLATE, TRIM, VERIFY) and type conversion functions**,
So that **I can process and convert text data**.

**Acceptance Criteria:**

**Given** `I = INDEX('ABCDEF', 'CD');`
**When** executed
**Then** I = 3

**Given** `S = TRIM(' hello ', '1'B, '1'B);`
**When** executed
**Then** S = 'hello' (trim both leading and trailing blanks)

**Complexity:** M

### Story P104.3: Date/Time and Storage Functions (~30)

As a **PL/I developer**,
I want **date/time functions (DATETIME, DAYS, SECS) and storage functions (ADDR, NULL, ALLOCATION, SIZE)**,
So that **I can work with dates and manage storage programmatically**.

**Acceptance Criteria:**

**Given** `DT = DATETIME('YYYYMMDD');`
**When** executed
**Then** DT contains current date in ISO format

**Given** `P = ADDR(MY_STRUCT);`
**When** executed
**Then** P contains the address of MY_STRUCT

**Complexity:** M

---

## Epic P105: PL/I Exception Handling (ON-Units)

**Goal:** Implement PL/I's comprehensive exception handling system with ~25 conditions.

**Crate:** `open-mainframe-pli`
**FRs:** FR-v4.0-P105

### Story P105.1: ON-Unit Establishment and Condition Handling

As a **PL/I developer**,
I want **ON condition action statements to handle exceptions**,
So that **my programs can recover from error conditions**.

**Acceptance Criteria:**

**Given** `ON ZERODIVIDE BEGIN; PUT SKIP LIST('Division by zero!'); GOTO RECOVERY; END;`
**When** a division by zero occurs
**Then** the ON-unit executes and control transfers to RECOVERY

**Given** `ON ENDFILE(INFILE) EOF = '1'B;`
**When** the end of INFILE is reached during a READ
**Then** EOF is set to true and processing continues

Conditions: AREA, ATTENTION, CONDITION(name), CONVERSION, ENDFILE, ENDPAGE, ERROR, FINISH, FIXEDOVERFLOW, INVALIDOP, KEY, NAME, OVERFLOW, RECORD, SIZE, STORAGE, STRINGRANGE, STRINGSIZE, SUBSCRIPTRANGE, TRANSMIT, UNDEFINEDFILE, UNDERFLOW, ZERODIVIDE

**Complexity:** L

### Story P105.2: SIGNAL, REVERT, and Condition Built-in Functions

As a **PL/I developer**,
I want **SIGNAL to raise conditions and REVERT to remove ON-units**,
So that **I can programmatically trigger and manage exception handling**.

**Acceptance Criteria:**

**Given** `SIGNAL CONDITION(MY_ERROR);`
**When** executed
**Then** the ON CONDITION(MY_ERROR) handler is invoked

**Given** `REVERT ZERODIVIDE;`
**When** executed
**Then** the current ON-unit for ZERODIVIDE is removed, reverting to the enclosing scope's handler

Functions: ONCODE, ONLOC, ONCHAR, ONSOURCE, ONFILE, ONKEY, ONCOUNT

**Complexity:** M

---

## Epic P106: PL/I Stream I/O

**Goal:** Implement GET/PUT with LIST, EDIT, and DATA directed I/O.

**Crate:** `open-mainframe-pli`
**FRs:** FR-v4.0-P106

### Story P106.1: GET/PUT LIST and EDIT

As a **PL/I developer**,
I want **stream I/O with free-format (LIST) and formatted (EDIT) modes**,
So that **I can read and write data in flexible formats**.

**Acceptance Criteria:**

**Given** `PUT LIST('Name:', NAME, 'Age:', AGE);`
**When** executed
**Then** items are written in default format separated by blanks

**Given** `PUT EDIT(NAME, AGE)(A(20), F(3));`
**When** executed
**Then** NAME is written as 20-character string, AGE as 3-digit integer

**Given** `GET EDIT(CODE, AMOUNT)(A(3), F(10,2));`
**When** reading from SYSIN
**Then** CODE reads 3 characters, AMOUNT reads 10-digit decimal with 2 decimal places

Format items: A, B, C, COLUMN, E, F, LINE, P, PAGE, R, SKIP, X

**Complexity:** M

---

## Epic P107: PL/I Record I/O

**Goal:** Implement READ/WRITE/REWRITE/DELETE for sequential and keyed (VSAM) file access.

**Crate:** `open-mainframe-pli`
**FRs:** FR-v4.0-P107

### Story P107.1: Sequential and Keyed Record I/O

As a **PL/I developer**,
I want **record I/O with sequential and indexed access**,
So that **I can process VSAM, QSAM, and BSAM datasets**.

**Acceptance Criteria:**

**Given** `READ FILE(INFILE) INTO(REC);`
**When** executed
**Then** the next sequential record is read into REC

**Given** `READ FILE(KSDS) INTO(REC) KEY(SEARCH_KEY);`
**When** executed
**Then** the record with matching key is retrieved from the VSAM KSDS

**Given** `WRITE FILE(OUTFILE) FROM(REC);`
**When** executed
**Then** REC is written to the output dataset

**Complexity:** L

### Story P107.2: LOCATE-Mode I/O

As a **PL/I developer**,
I want **LOCATE-mode I/O for zero-copy buffer access**,
So that **I can process records directly in I/O buffers for performance**.

**Acceptance Criteria:**

**Given** `READ FILE(INFILE) SET(P);`
**When** executed
**Then** P points directly to the record in the I/O buffer (no data copy)

**Complexity:** M

---

## Epic P108: PL/I Preprocessor

**Goal:** Implement the PL/I compile-time preprocessor with %IF, %INCLUDE, %DECLARE, and %PROCEDURE.

**Crate:** `open-mainframe-pli`
**FRs:** FR-v4.0-P108

### Story P108.1: Preprocessor Directives and Macro Procedures

As a **PL/I developer**,
I want **compile-time preprocessing with %IF/%THEN/%ELSE, %INCLUDE, and %PROCEDURE macros**,
So that **I can conditionally compile code and reuse preprocessor logic**.

**Acceptance Criteria:**

**Given** `%DCL DEBUG CHAR; %DEBUG = 'YES';` then `%IF DEBUG = 'YES' %THEN %DO; PUT SKIP LIST('Debug'); %END;`
**When** preprocessed
**Then** the PUT statement is included in the output

**Given** `%INCLUDE SQLCA;`
**When** preprocessed
**Then** member SQLCA from SYSLIB is included

**Given** a `%PROCEDURE` macro that generates structure declarations
**When** `%ACTIVATE MYMACRO;` then `MYMACRO(PARM1);`
**Then** the macro expands and generates PL/I source code

**Complexity:** M

---

## Epic P109: PL/I CICS/DB2/IMS Integration

**Goal:** Adapt existing CICS, DB2, and IMS preprocessors for PL/I syntax.

**Crate:** `open-mainframe-pli`
**FRs:** FR-v4.0-P109

### Story P109.1: EXEC CICS and EXEC SQL for PL/I

As a **PL/I developer**,
I want **EXEC CICS and EXEC SQL embedded statements preprocessed for PL/I host variables**,
So that **I can write PL/I CICS transactions and DB2 queries**.

**Acceptance Criteria:**

**Given** `EXEC SQL SELECT NAME INTO :WS_NAME FROM EMPLOYEES WHERE ID = :WS_ID;`
**When** preprocessed for PL/I
**Then** host variable declarations use PL/I DECLARE syntax and SQLCA is a PL/I structure

**Given** `EXEC CICS RECEIVE INTO(INPUT_DATA) LENGTH(INPUT_LEN);`
**When** preprocessed for PL/I
**Then** the EIB is declared as a PL/I structure and DFHCOMMAREA uses PL/I types

**Complexity:** M

### Story P109.2: EXEC DLI for IMS

As a **PL/I developer**,
I want **EXEC DLI embedded statements preprocessed for PL/I**,
So that **I can write PL/I IMS DL/I database programs**.

**Acceptance Criteria:**

**Given** `EXEC DLI GU USING PCB(1) SEGMENT(CUSTOMER) INTO(CUST_REC) WHERE(CUSTNO = SEARCH_KEY);`
**When** preprocessed for PL/I
**Then** DIB and PCB are declared as PL/I structures

**Complexity:** M

---

## Epic P110: PL/I Structures, Arrays, and PICTURE

**Goal:** Implement multi-level structures, multi-dimensional arrays, and PICTURE data formatting.

**Crate:** `open-mainframe-pli`
**FRs:** FR-v4.0-P110

### Story P110.1: Structures and Arrays

As a **PL/I developer**,
I want **structures with up to 15 levels and arrays with up to 15 dimensions**,
So that **I can define complex data layouts matching mainframe record formats**.

**Acceptance Criteria:**

**Given** `DCL 1 EMPLOYEE, 2 NAME CHAR(30), 2 DEPT FIXED DEC(3), 2 HISTORY(10), 3 YEAR FIXED DEC(4), 3 RATING CHAR(1);`
**When** parsed and stored
**Then** a 3-level structure with a 10-element array of sub-structures is created

**Given** `DCL TABLE(10,20,5) FIXED BIN(31);`
**When** `TABLE(3,7,2) = 42;`
**Then** the 3-dimensional array element is correctly addressed

**Complexity:** L

### Story P110.2: PICTURE Data Type

As a **PL/I developer**,
I want **PICTURE types for numeric editing and character validation**,
So that **I can format numbers for display and validate input patterns**.

**Acceptance Criteria:**

**Given** `DCL SALARY PICTURE '$$$,$$9V.99';` then `SALARY = 12345.67;`
**When** displayed
**Then** output is ' $12,345.67' (with drifting dollar sign and comma insertion)

**Given** `DCL CODE PICTURE 'AAA99';` then `CODE = 'ABC12';`
**When** assigned
**Then** validated that first 3 characters are alphabetic and last 2 are numeric

**Complexity:** M

---
