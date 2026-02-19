# REXX Crate — Epics & Stories

## Epic R100: REXX Lexer and Parser

**Goal:** Build a complete lexer and parser for TSO/E REXX, handling the language's unusual syntax (no reserved words, label-based subroutines, free-form layout).

**Crate:** `open-mainframe-rexx`
**FRs:** FR-v4.0-R100

### Story R100.1: Tokenizer — Keywords, Strings, Operators, Comments

As a **mainframe developer**,
I want **REXX source code to be tokenized into structured tokens**,
So that **the parser can process any valid REXX program**.

**Acceptance Criteria:**

**Given** REXX source with `/* REXX */ SAY 'Hello World'`
**When** tokenized
**Then** tokens are: comment, keyword(SAY), string('Hello World')

**Given** a line with `x = 2 + 3 /* inline comment */ * 4`
**When** tokenized
**Then** operators (+, *, =) and numeric literals are correctly identified, comment is stripped

**Given** a continuation line ending with comma: `SAY 'part1' ||,`
**When** tokenized
**Then** the next line is treated as a continuation of the current clause

**Complexity:** M

### Story R100.2: Parser — Clause Structure and Instruction Parsing

As a **mainframe developer**,
I want **REXX clauses to be parsed into an AST**,
So that **the interpreter can execute any REXX instruction**.

**Acceptance Criteria:**

**Given** `IF x > 5 THEN SAY 'big'; ELSE SAY 'small'`
**When** parsed
**Then** an IF node with THEN and ELSE branches is constructed

**Given** `DO i = 1 TO 10 BY 2; SAY i; END`
**When** parsed
**Then** a DO-iterative node with control variable, bounds, and increment is constructed

**Given** `myLabel: PROCEDURE; RETURN x + 1`
**When** parsed
**Then** a labeled subroutine with PROCEDURE scope is recognized

**Complexity:** L

### Story R100.3: Source Format Handling and REXX Identification

As a **mainframe developer**,
I want **the parser to identify REXX source by the `/* REXX */` comment convention**,
So that **REXX programs are distinguished from CLIST and other scripts**.

**Acceptance Criteria:**

**Given** a source file starting with `/* REXX */`
**When** the parser initializes
**Then** the file is identified as REXX source

**Given** a source file without the REXX identifier comment
**When** loaded from SYSEXEC
**Then** the file is still treated as REXX (SYSEXEC implies REXX)

**Complexity:** S

---

## Epic R101: REXX Interpreter Core

**Goal:** Implement the core REXX execution engine with variable pools, expression evaluation, arbitrary precision arithmetic, and control flow.

**Crate:** `open-mainframe-rexx`
**FRs:** FR-v4.0-R101

### Story R101.1: Variable Pool — Simple, Compound/Stem, and DROP

As a **REXX programmer**,
I want **variables to support simple names and compound (stem) variables**,
So that **I can use associative arrays with default values**.

**Acceptance Criteria:**

**Given** `stem. = 0; stem.1 = 'first'; stem.key = 'value'`
**When** executed
**Then** `stem.1` returns 'first', `stem.key` returns 'value', `stem.unknown` returns '0'

**Given** `DROP x; SAY SYMBOL('x')`
**When** executed
**Then** SYMBOL returns 'LIT' indicating x is uninitialized (returns its own name uppercased)

**Complexity:** M

### Story R101.2: Expression Evaluator

As a **REXX programmer**,
I want **full expression evaluation with arithmetic, comparison, and concatenation**,
So that **I can write complex expressions in any REXX statement**.

**Acceptance Criteria:**

**Given** `SAY 3 + 4 * 2` (no precedence in REXX — left to right)
**When** evaluated
**Then** result is 14 (left-to-right: 3+4=7, 7*2=14)

**Given** `SAY 'Hello' 'World'` (blank concatenation)
**When** evaluated
**Then** result is 'Hello World'

**Given** `SAY 'AB' || 'CD'` (abuttal concatenation)
**When** evaluated
**Then** result is 'ABCD'

**Complexity:** L

### Story R101.3: Arbitrary Precision Decimal Arithmetic

As a **REXX programmer**,
I want **NUMERIC DIGITS to control decimal precision up to 999999999**,
So that **I can perform exact arithmetic for financial and scientific calculations**.

**Acceptance Criteria:**

**Given** `NUMERIC DIGITS 50; SAY 1/3`
**When** executed
**Then** result is '0.33333333333333333333333333333333333333333333333333'

**Given** `NUMERIC FORM SCIENTIFIC; SAY 1.23E+2 * 10`
**When** executed
**Then** result is '1230' with NUMERIC FORM controlling exponential notation

**Complexity:** L

### Story R101.4: Control Flow — DO/END, IF/THEN/ELSE, SELECT

As a **REXX programmer**,
I want **all DO loop variants, IF conditionals, and SELECT/WHEN/OTHERWISE**,
So that **I can write structured programs**.

**Acceptance Criteria:**

**Given** `DO WHILE x < 10; x = x + 1; END`
**When** executed with x=0
**Then** loop executes 10 times, x ends at 10

**Given** `DO i = 1 TO 5; IF i = 3 THEN ITERATE; SAY i; END`
**When** executed
**Then** outputs 1, 2, 4, 5 (skipping 3 via ITERATE)

**Given** `SELECT; WHEN x=1 THEN SAY 'one'; WHEN x=2 THEN SAY 'two'; OTHERWISE SAY 'other'; END`
**When** executed with x=2
**Then** outputs 'two'

**Complexity:** M

### Story R101.5: Subroutines — CALL/RETURN, PROCEDURE EXPOSE

As a **REXX programmer**,
I want **internal and external subroutine calls with PROCEDURE scoping**,
So that **I can write modular programs with local variables**.

**Acceptance Criteria:**

**Given** `CALL myFunc 'arg1'; EXIT; myFunc: PROCEDURE; PARSE ARG a; RETURN a || '!';`
**When** executed
**Then** RESULT contains 'arg1!'

**Given** `x = 10; CALL sub; SAY x; EXIT; sub: PROCEDURE EXPOSE x; x = x + 1; RETURN`
**When** executed
**Then** outputs 11 (x is exposed to the subroutine)

**Complexity:** M

---

## Epic R102: PARSE Instruction and Templates

**Goal:** Implement the REXX PARSE instruction with all template types — the most distinctive feature of REXX.

**Crate:** `open-mainframe-rexx`
**FRs:** FR-v4.0-R102

### Story R102.1: Word Parsing Templates

As a **REXX programmer**,
I want **PARSE to split strings by word boundaries into variables**,
So that **I can destructure input strings easily**.

**Acceptance Criteria:**

**Given** `PARSE VALUE 'John Smith 42' WITH first last age`
**When** executed
**Then** first='John', last='Smith', age='42'

**Given** `PARSE VALUE 'a b c d e' WITH x y z`
**When** executed
**Then** x='a', y='b', z='c d e' (last variable gets remainder)

**Complexity:** M

### Story R102.2: Positional and Literal Templates

As a **REXX programmer**,
I want **PARSE templates with absolute/relative positions and literal delimiters**,
So that **I can extract fields from fixed-format and delimited data**.

**Acceptance Criteria:**

**Given** `PARSE VALUE '2025-12-31' WITH year 5 '-' month 8 '-' day`
**When** executed
**Then** year='2025', month='12', day='31'

**Given** `PARSE VALUE 'key=value' WITH name '=' val`
**When** executed
**Then** name='key', val='value'

**Complexity:** L

### Story R102.3: PARSE Variants — ARG, PULL, VAR, SOURCE, VERSION

As a **REXX programmer**,
I want **all PARSE source variants**,
So that **I can parse arguments, stack input, variables, and system information**.

**Acceptance Criteria:**

**Given** `PARSE ARG first, second` with two arguments passed via CALL
**When** executed
**Then** first and second contain the respective argument strings

**Given** PUSH 'hello world' then `PARSE PULL greeting`
**When** executed
**Then** greeting='hello world' (read from data stack)

**Given** `PARSE SOURCE sys type name`
**When** executed
**Then** sys='TSO' (or equivalent), type='COMMAND'/'SUBROUTINE', name=exec name

**Complexity:** M

---

## Epic R103: Built-in Functions — String & Conversion

**Goal:** Implement all 36+ REXX string manipulation and data conversion built-in functions.

**Crate:** `open-mainframe-rexx`
**FRs:** FR-v4.0-R103

### Story R103.1: String Functions (25 functions)

As a **REXX programmer**,
I want **all string built-in functions**,
So that **I can manipulate text data without external libraries**.

**Acceptance Criteria:**

**Given** `SAY SUBSTR('Hello World', 7, 5)`
**When** executed
**Then** outputs 'World'

**Given** `SAY COPIES('AB', 3)`
**When** executed
**Then** outputs 'ABABAB'

**Given** `SAY WORDPOS('fox', 'the quick brown fox')`
**When** executed
**Then** outputs '4'

Functions: ABBREV, CENTER, CENTRE, CHANGESTR, COMPARE, COPIES, COUNTSTR, DELSTR, DELWORD, INSERT, LASTPOS, LEFT, LENGTH, OVERLAY, POS, REVERSE, RIGHT, SPACE, STRIP, SUBSTR, SUBWORD, TRANSLATE, VERIFY, WORD, WORDINDEX, WORDLENGTH, WORDPOS, WORDS

**Complexity:** M

### Story R103.2: Conversion Functions (8 functions)

As a **REXX programmer**,
I want **base conversion functions between binary, character, decimal, and hex**,
So that **I can convert between data representations**.

**Acceptance Criteria:**

**Given** `SAY C2X('AB')`
**When** executed
**Then** outputs 'C1C2' (EBCDIC hex for A and B)

**Given** `SAY X2D('FF')`
**When** executed
**Then** outputs '255'

Functions: B2X, C2D, C2X, D2C, D2X, X2B, X2C, X2D

**Complexity:** S

### Story R103.3: Bit Functions (3 functions)

As a **REXX programmer**,
I want **bitwise operation functions**,
So that **I can perform bit-level data manipulation**.

**Acceptance Criteria:**

**Given** `SAY BITAND('73'x, 'FF'x)`
**When** executed
**Then** outputs '73'x

Functions: BITAND, BITOR, BITXOR

**Complexity:** S

---

## Epic R104: Built-in Functions — Numeric, Date/Time, Information

**Goal:** Implement all numeric, date/time, and information built-in functions.

**Crate:** `open-mainframe-rexx`
**FRs:** FR-v4.0-R104

### Story R104.1: Numeric Functions (7 functions)

As a **REXX programmer**,
I want **numeric built-in functions**,
So that **I can perform mathematical operations**.

**Acceptance Criteria:**

**Given** `SAY ABS(-5)`
**When** executed
**Then** outputs '5'

**Given** `SAY FORMAT(12.3, 5, 2)`
**When** executed
**Then** outputs '   12.30' (right-justified, 2 decimal places)

Functions: ABS, FORMAT, MAX, MIN, RANDOM, SIGN, TRUNC

**Complexity:** S

### Story R104.2: DATE and TIME Functions

As a **REXX programmer**,
I want **DATE and TIME functions with all format options**,
So that **I can work with dates and times in any required format**.

**Acceptance Criteria:**

**Given** `SAY DATE('S')`
**When** executed
**Then** outputs '20260219' (sorted/ISO format)

**Given** `SAY DATE('B')` (base days since 01 Jan 0001)
**When** executed
**Then** outputs the correct base day count

**Given** `SAY TIME('L')` (long format HH:MM:SS.uuuuuu)
**When** executed
**Then** outputs current time with microseconds

DATE formats: Base, Century, Days, European, Julian, Month, Normal, Ordered, Sorted, Standard, USA, Weekday
TIME formats: Civil, Elapsed, Hours, Long, Minutes, Normal, Reset, Seconds

**Complexity:** M

### Story R104.3: Information Functions (16 functions)

As a **REXX programmer**,
I want **information built-in functions for runtime introspection**,
So that **I can query the execution environment**.

**Acceptance Criteria:**

**Given** `SAY DATATYPE('123', 'N')`
**When** executed
**Then** outputs '1' (is numeric)

**Given** `SAY SOURCELINE(1)`
**When** executed
**Then** outputs the first line of the current REXX program

Functions: ADDRESS, ARG, CONDITION, DATATYPE, DIGITS, ERRORTEXT, FORM, FUZZ, LINESIZE, QUEUED, SOURCELINE, SYMBOL, TRACE, USERID, VALUE, XRANGE

**Complexity:** M

---

## Epic R105: Data Stack and EXECIO

**Goal:** Implement the REXX data stack and EXECIO command for dataset I/O.

**Crate:** `open-mainframe-rexx`
**FRs:** FR-v4.0-R105

### Story R105.1: Data Stack — PUSH, PULL, QUEUE

As a **REXX programmer**,
I want **a LIFO/FIFO data stack for inter-clause and inter-program communication**,
So that **I can pass data between REXX programs and TSO commands**.

**Acceptance Criteria:**

**Given** `PUSH 'last'; PUSH 'first'; PULL x; SAY x`
**When** executed
**Then** outputs 'first' (LIFO — PUSH adds to top, PULL reads from top)

**Given** `QUEUE 'first'; QUEUE 'last'; PULL x; SAY x`
**When** executed
**Then** outputs 'first' (FIFO — QUEUE adds to bottom, PULL reads from top)

**Given** `SAY QUEUED()` after pushing 3 items
**When** executed
**Then** outputs '3'

**Complexity:** M

### Story R105.2: Buffer Management — MAKEBUF, DROPBUF, NEWSTACK

As a **REXX programmer**,
I want **stack buffer management for isolating stack contents**,
So that **subroutines can use the stack without affecting callers**.

**Acceptance Criteria:**

**Given** `PUSH 'a'; MAKEBUF; PUSH 'b'; DROPBUF; PULL x; SAY x`
**When** executed
**Then** outputs 'a' (DROPBUF removes items added since MAKEBUF)

**Complexity:** S

### Story R105.3: EXECIO — Dataset Read/Write

As a **REXX programmer**,
I want **EXECIO to read and write datasets via DD names**,
So that **I can process files in REXX programs**.

**Acceptance Criteria:**

**Given** `EXECIO * DISKR INDD (STEM line. FINIS`
**When** executed with a 3-record dataset allocated to INDD
**Then** line.0='3', line.1/line.2/line.3 contain the records

**Given** `EXECIO 0 DISKW OUTDD (OPEN`; then `EXECIO 1 DISKW OUTDD (STEM rec.`
**When** executed
**Then** writes rec.1 to the dataset allocated to OUTDD

**Complexity:** L

---

## Epic R106: ADDRESS Environments and Host Commands

**Goal:** Implement the ADDRESS instruction and host environment bridges for TSO, ISPF, and MVS.

**Crate:** `open-mainframe-rexx`
**FRs:** FR-v4.0-R106

### Story R106.1: ADDRESS Instruction and Environment Routing

As a **REXX programmer**,
I want **ADDRESS to route commands to different host environments**,
So that **I can interact with TSO, ISPF, and MVS from REXX**.

**Acceptance Criteria:**

**Given** `ADDRESS TSO 'LISTDS myds'`
**When** executed
**Then** the LISTDS command is routed to the TSO command processor

**Given** `ADDRESS ISPEXEC 'DISPLAY PANEL(MYPANEL)'`
**When** executed
**Then** the DISPLAY command is routed to the ISPF dialog manager

**Complexity:** M

### Story R106.2: TSO and MVS Environments

As a **REXX programmer**,
I want **TSO and MVS host command environments**,
So that **I can execute TSO commands and MVS system commands from REXX**.

**Acceptance Criteria:**

**Given** `ADDRESS TSO "ALLOCATE DA('MY.DATA') FI(INDD) SHR"`
**When** executed
**Then** the dataset is allocated to DD name INDD

**Given** `ADDRESS MVS "EXECIO * DISKR INDD (STEM line. FINIS"`
**When** executed
**Then** EXECIO is processed via the MVS environment

**Complexity:** L

---

## Epic R107: Condition Handling and Debugging

**Goal:** Implement SIGNAL/CALL condition traps and the TRACE interactive debugger.

**Crate:** `open-mainframe-rexx`
**FRs:** FR-v4.0-R107

### Story R107.1: SIGNAL ON/OFF and CALL ON/OFF

As a **REXX programmer**,
I want **condition traps via SIGNAL and CALL**,
So that **I can handle errors and exceptional conditions gracefully**.

**Acceptance Criteria:**

**Given** `SIGNAL ON SYNTAX; x = 1/0; EXIT; SYNTAX: SAY 'Error:' ERRORTEXT(RC)`
**When** executed
**Then** outputs error message for division by zero instead of terminating

**Given** `SIGNAL ON NOVALUE; SAY undeclaredVar`
**When** executed
**Then** control transfers to the NOVALUE label

Conditions: ERROR, FAILURE, HALT, NOVALUE, SYNTAX, NOTREADY, LOSTDIGITS

**Complexity:** M

### Story R107.2: TRACE Instruction and Interactive Debugging

As a **REXX programmer**,
I want **TRACE for execution tracing and interactive debugging**,
So that **I can debug REXX programs by stepping through clauses**.

**Acceptance Criteria:**

**Given** `TRACE I` (intermediates)
**When** a REXX program executes
**Then** each clause and its intermediate results are displayed

**Given** `TRACE ?R` (interactive results)
**When** a REXX program executes
**Then** execution pauses after each clause, allowing interactive commands

Trace options: A (All), C (Commands), E (Error), F (Failure), I (Intermediates), L (Labels), N (Normal), O (Off), R (Results), S (Scan)

**Complexity:** M

---

## Epic R108: INTERPRET and Advanced Features

**Goal:** Implement the INTERPRET instruction for dynamic code execution and DBCS support.

**Crate:** `open-mainframe-rexx`
**FRs:** FR-v4.0-R108

### Story R108.1: INTERPRET Instruction

As a **REXX programmer**,
I want **INTERPRET to execute dynamically constructed REXX code**,
So that **I can build and execute code at runtime**.

**Acceptance Criteria:**

**Given** `code = "SAY 'Dynamic!'"; INTERPRET code`
**When** executed
**Then** outputs 'Dynamic!'

**Given** `varname = 'x'; INTERPRET varname '= 42'`
**When** executed
**Then** variable x is set to 42

**Complexity:** M

### Story R108.2: External Function Packages

As a **REXX programmer**,
I want **external function packages to extend REXX with custom functions**,
So that **applications can provide domain-specific REXX functions**.

**Acceptance Criteria:**

**Given** a registered function package with function MYFUNC
**When** `SAY MYFUNC('arg')` is executed
**Then** the external function is called and its result is returned

**Complexity:** M

---

## Epic R109: REXX-COBOL/JCL Integration

**Goal:** Enable REXX invocation from JCL (IRXJCL, IKJEFT01) and from COBOL (IRXEXEC).

**Crate:** `open-mainframe-rexx`
**FRs:** FR-v4.0-R109

### Story R109.1: IRXJCL — Batch REXX Execution

As a **batch programmer**,
I want **EXEC PGM=IRXJCL to execute REXX programs from JCL**,
So that **I can run REXX scripts as batch job steps**.

**Acceptance Criteria:**

**Given** `//STEP1 EXEC PGM=IRXJCL,PARM='MYEXEC'` with SYSEXEC pointing to a PDS containing MYEXEC
**When** the JCL step executes
**Then** the REXX exec runs with SYSTSIN as input and SYSTSPRT as output

**Given** the REXX exec ends with `EXIT 8`
**When** the step completes
**Then** the step return code is 8

**Complexity:** M

### Story R109.2: IRXEXEC — Call REXX from COBOL/Assembler

As a **application developer**,
I want **IRXEXEC API to invoke REXX from compiled programs**,
So that **COBOL programs can call REXX scripts dynamically**.

**Acceptance Criteria:**

**Given** a COBOL program calling IRXEXEC with exec name and arguments
**When** the call completes
**Then** the REXX exec's return value is available to the COBOL program

**Complexity:** M

---
