---
version: 'v5.0'
planningGroup: 'PG-21'
technology: 'CLIST (Command List)'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-clist-v5.0.md'
  - 'architecture-clist-v5.0.md'
totalEpics: 5
totalStories: 27
frCoverage: '10/10 (100%)'
nfrCoverage: '3/3 (100%)'
---

# Epics & Stories: CLIST (Command List)

## Epic Overview

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| CL-100 | CLIST Lexer & Parser | M | 5 | D |
| CL-101 | CLIST Interpreter Core | M | 6 | D |
| CL-102 | Built-in Functions | S | 4 | D |
| CL-103 | I/O and Error Handling | M | 6 | D |
| CL-104 | TSO/ISPF Integration | L | 6 | D |

---

## CL-100: CLIST Lexer & Parser

**User Value:** CLIST source scripts are parsed into structured representations, enabling interpretation and validation of legacy CLIST programs.

### CL-100.1: Source Line Processing

**As a** CLIST developer, **I want** source lines parsed with continuation (`+`/`-`) and label recognition, **so that** multi-line CLIST statements are assembled correctly.

**Acceptance Criteria:**
- Given a line ending with `+`, when the next line follows, then leading blanks are stripped and the lines are joined
- Given a line ending with `-`, when the next line follows, then leading blanks are preserved and the lines are joined
- Given `LOOP1: SET &X = 1`, when parsed, then label `LOOP1` is registered at the current line index

### CL-100.2: Tokenizer

**As a** CLIST developer, **I want** CLIST source tokenized into statements, `&`-variables, literals, operators, and labels, **so that** the parser can construct AST nodes.

**Acceptance Criteria:**
- Given `SET &MYVAR = &EVAL(1 + 2)`, when tokenized, then tokens are: Statement(Set), Variable("MYVAR"), Operator(Eq), Function("EVAL"), Number(1), Operator(Plus), Number(2)
- Given `'quoted string'`, when tokenized, then a single Literal token is produced preserving internal spaces

### CL-100.3: Statement Parser

**As a** CLIST developer, **I want** all ~25 CLIST statement types parsed into AST nodes.

**Acceptance Criteria:**
- Given `IF &X = 1 THEN WRITE YES`, when parsed, then an If AST node with condition, then-branch is produced
- Given `DO WHILE &X < 10 ... END`, when parsed, then a Do AST node with WhileCondition is produced
- Given `SELECT WHEN(&X = 1) ... OTHERWISE ... END`, when parsed, then a Select AST node with When branches and Otherwise is produced

### CL-100.4: Expression Parser

**As a** CLIST developer, **I want** arithmetic, string, and comparison expressions parsed with correct operator precedence.

**Acceptance Criteria:**
- Given `&A + &B * &C`, when parsed, then multiplication binds tighter than addition
- Given `&STR1 = &STR2`, when in a condition context, then string comparison is performed
- Given compound conditions `&A > 1 AND &B < 10`, when parsed, then AND/OR boolean operators are recognized

### CL-100.5: Parser Tests

**Acceptance Criteria:**
- Given CLIST sources exercising all 25 statement types, continuations, labels, and expressions, when parsed, then correct AST structures are produced
- Given malformed CLIST, when parsed, then diagnostic errors include line numbers and descriptions

---

## CL-101: CLIST Interpreter Core

**User Value:** CLIST scripts execute with variable substitution, control flow, and subprocedure support, enabling automation of TSO tasks.

### CL-101.1: Variable Pool & Substitution

**As a** CLIST developer, **I want** `&`-prefixed variables stored, substituted, and resolved (including nested `&&` deferred substitution), **so that** CLIST variable semantics work correctly.

**Acceptance Criteria:**
- Given `SET &X = HELLO` followed by `WRITE &X`, when executed, then `HELLO` is output
- Given `SET &NAME = X` and `SET &&NAME = 42`, when `&X` is referenced, then `42` is returned (deferred substitution)
- Given a variable name of 252 characters, when set, then it is stored and retrievable

### CL-101.2: System Control Variables

**As a** CLIST developer, **I want** ~37 system control variables (&LASTCC, &MAXCC, &SYSDATE, &SYSTIME, &SYSUID, etc.) available as read-only values.

**Acceptance Criteria:**
- Given `WRITE &SYSDATE`, when executed, then the current date in MM/DD/YY format is output
- Given `WRITE &SYSUID`, when executed, then the current user ID is output
- Given `SET &LASTCC = 5`, when attempted, then an error is raised (read-only)

### CL-101.3: SET Statement

**As a** CLIST developer, **I want** SET to assign values with implicit string/numeric handling.

**Acceptance Criteria:**
- Given `SET &X = HELLO WORLD`, when executed, then `&X` contains `HELLO WORLD`
- Given `SET &X = &EVAL(10 + 20)`, when executed, then `&X` contains `30`
- Given `SET &X = &SUBSTR(1:3,&Y)`, when &Y is `ABCDEF`, then `&X` contains `ABC`

### CL-101.4: Control Flow

**As a** CLIST developer, **I want** IF/THEN/ELSE, DO/WHILE/UNTIL/END, SELECT/WHEN/OTHERWISE/END, and GOTO, **so that** branching and looping work correctly.

**Acceptance Criteria:**
- Given `IF &X > 5 THEN WRITE BIG ELSE WRITE SMALL`, when &X is 10, then `BIG` is output
- Given `DO WHILE &I < 5 ... SET &I = &EVAL(&I + 1) ... END`, when &I starts at 0, then the loop executes 5 times
- Given `GOTO SKIP1`, when SKIP1 is a defined label, then execution jumps to that line

### CL-101.5: EXIT and Nesting

**As a** CLIST developer, **I want** EXIT to terminate with an optional return code, and nested CLIST invocation to work correctly.

**Acceptance Criteria:**
- Given `EXIT CODE(4)`, when executed, then the CLIST terminates and &LASTCC in the caller is set to 4
- Given nested CLIST execution via EXEC, when the inner CLIST exits, then control returns to the outer CLIST

### CL-101.6: Subprocedures

**As a** CLIST developer, **I want** SYSCALL, SYSREF, GLOBAL, and NGLOBAL for internal subprocedures and inter-CLIST variable sharing.

**Acceptance Criteria:**
- Given `SYSCALL MYSUB PARM1`, when MYSUB is a labeled subprocedure, then it executes with parameters
- Given `SYSREF P1` inside a subprocedure, when P1 is modified, then the caller's variable is updated (pass by reference)
- Given `GLOBAL VAR1 VAR2`, when a nested CLIST reads &VAR1, then it accesses the parent's variable

---

## CL-102: Built-in Functions

**User Value:** CLIST scripts can perform arithmetic, string manipulation, and system queries using built-in functions.

### CL-102.1: &EVAL — Arithmetic Evaluation

**As a** CLIST developer, **I want** `&EVAL(expr)` to evaluate arithmetic expressions (integer and decimal).

**Acceptance Criteria:**
- Given `&EVAL(10 + 20 * 3)`, when evaluated, then `70` is returned
- Given `&EVAL(100 / 3)`, when evaluated, then integer division returns `33`
- Given `&EVAL(&X + 1)` where &X is `42`, when evaluated, then `43` is returned

### CL-102.2: String Functions

**As a** CLIST developer, **I want** &SUBSTR, &LENGTH, &SYSINDEX, &SYSCAPS, and &SYSLC for string operations.

**Acceptance Criteria:**
- Given `&SUBSTR(3:5,ABCDEFGH)`, when evaluated, then `CDE` is returned
- Given `&LENGTH(HELLO)`, when evaluated, then `5` is returned
- Given `&SYSINDEX(CD,ABCDEF)`, when evaluated, then `3` is returned
- Given `&SYSCAPS(hello)`, when evaluated, then `HELLO` is returned
- Given `&SYSLC(HELLO)`, when evaluated, then `hello` is returned

### CL-102.3: Data Functions

**As a** CLIST developer, **I want** &DATATYPE, &STR, &NRSTR, and &SYSNSUB for type checking and substitution control.

**Acceptance Criteria:**
- Given `&DATATYPE(123)`, when evaluated, then `NUM` is returned
- Given `&DATATYPE(ABC)`, when evaluated, then `CHAR` is returned
- Given `&STR(&MYVAR)`, when evaluated, then the literal text `&MYVAR` is returned (no substitution)

### CL-102.4: System Functions & Tests

**As a** CLIST developer, **I want** &SYSDSN for dataset availability checking.

**Acceptance Criteria:**
- Given `&SYSDSN('EXIST.DATASET')`, when the dataset exists, then `OK` is returned
- Given `&SYSDSN('NOEXIST.DATASET')`, when the dataset does not exist, then `DATASET NOT FOUND` is returned
- Given function tests covering all ~15 built-in functions, when `cargo test -p open-mainframe-clist` runs, then all pass

---

## CL-103: I/O and Error Handling

**User Value:** CLIST scripts interact with terminals and files, and handle errors gracefully, enabling robust automation.

### CL-103.1: Terminal Output — WRITE and WRITENR

**As a** CLIST developer, **I want** WRITE (with newline) and WRITENR (no newline) to output to the terminal.

**Acceptance Criteria:**
- Given `WRITE HELLO &NAME`, when &NAME is `SMITH`, then `HELLO SMITH\n` is output
- Given `WRITENR ENTER VALUE:`, when executed, then `ENTER VALUE:` is output without newline

### CL-103.2: Terminal Input — READ, READDVAL, TERMIN

**As a** CLIST developer, **I want** READ, READDVAL, and TERMIN to capture terminal input.

**Acceptance Criteria:**
- Given `READ`, when the user types `HELLO`, then `&SYSDVAL` contains `HELLO`
- Given `READDVAL &A &B &C`, when the user types `X Y Z`, then &A=X, &B=Y, &C=Z
- Given `TERMIN`, when the user types a command, then it is executed as a TSO command

### CL-103.3: File I/O

**As a** CLIST developer, **I want** OPENFILE, GETFILE, PUTFILE, CLOSFILE for sequential file operations.

**Acceptance Criteria:**
- Given `OPENFILE MYFILE INPUT` after allocation, when GETFILE MYFILE is called, then the next record is placed in &MYFILE
- Given `OPENFILE OUTFILE OUTPUT` followed by `SET &OUTFILE = DATA LINE` and `PUTFILE OUTFILE`, when executed, then the record is written
- Given `CLOSFILE MYFILE`, when called, then the file is closed and resources freed

### CL-103.4: ERROR Routine

**As a** CLIST developer, **I want** ERROR routines to trap non-zero return codes automatically.

**Acceptance Criteria:**
- Given `ERROR DO ... RETURN END`, when a TSO command returns RC=12, then the ERROR block executes and &LASTCC is 12
- Given RETURN within the ERROR block, when executed, then execution resumes after the failing statement

### CL-103.5: ATTN Routine

**As a** CLIST developer, **I want** ATTN routines to handle terminal attention interrupts.

**Acceptance Criteria:**
- Given `ATTN DO ... EXIT CODE(8) END`, when attention is signaled, then the ATTN block executes

### CL-103.6: CONTROL Statement

**As a** CLIST developer, **I want** CONTROL with 13 options to govern execution behavior.

**Acceptance Criteria:**
- Given `CONTROL LIST CONLIST MSG`, when set, then TSO commands and CLIST statements are displayed before execution
- Given `CONTROL NOLIST NOMSG`, when set, then commands and messages are suppressed
- Given `CONTROL ASIS`, when set, then input case is preserved (not uppercased)
- Given `CONTROL MAIN`, when set, then the CLIST is declared as main-level and cannot be stacked

---

## CL-104: TSO/ISPF Integration

**User Value:** CLIST scripts seamlessly execute TSO commands and ISPF services, enabling panel-driven applications and system automation.

### CL-104.1: TSO Command Dispatch

**As a** CLIST developer, **I want** unrecognized statements routed to the TSO command environment.

**Acceptance Criteria:**
- Given `ALLOC FILE(X) DA('MY.DATA') SHR`, when executed, then the ALLOCATE command is dispatched to TSO
- Given `DELETE 'MY.TEMP.DATA'`, when executed, then the TSO DELETE command is dispatched
- Given a TSO command that returns RC=8, when executed, then &LASTCC is set to 8

### CL-104.2: ISPEXEC — ISPF Dialog Manager

**As a** CLIST developer, **I want** ISPEXEC prefix to invoke ISPF Dialog Manager services.

**Acceptance Criteria:**
- Given `ISPEXEC DISPLAY PANEL(MYPANEL)`, when executed, then the ISPF Display service is invoked
- Given `ISPEXEC TBOPEN MYTABLE`, when executed, then the ISPF Table Open service is invoked
- Given ISPF shared variable pool, when CLIST variables match ISPF variable names, then values are exchanged

### CL-104.3: ISREDIT — ISPF Edit Macros

**As a** CLIST developer, **I want** ISREDIT prefix to invoke ISPF Edit macro services.

**Acceptance Criteria:**
- Given `ISREDIT FIND 'SEARCH-TEXT'`, when executed, then the ISPF Edit Find service is invoked
- Given `ISREDIT CHANGE 'OLD' 'NEW' ALL`, when executed, then all occurrences are replaced

### CL-104.4: LISTDSI — Dataset Information

**As a** CLIST developer, **I want** LISTDSI to retrieve dataset attributes.

**Acceptance Criteria:**
- Given `LISTDSI 'MY.DATASET'`, when executed, then &SYSDSORG, &SYSRECFM, &SYSLRECL, &SYSBLKSIZE, &SYSUNITS, &SYSPRIMARY, &SYSSECONDS, &SYSEXTENTS, &SYSVOLUME are populated

### CL-104.5: Nested CLIST Execution

**As a** CLIST developer, **I want** the EXEC command to invoke another CLIST, **so that** CLISTs can compose functionality.

**Acceptance Criteria:**
- Given `EXEC 'MY.CLIST.LIB(SUBCLIST)' 'PARM1 PARM2'`, when executed, then the named CLIST runs with parameters
- Given GLOBAL variables set in the parent, when the child CLIST accesses them, then shared values are visible
- Given the child CLIST exits with RC=4, when control returns to the parent, then &LASTCC is 4

### CL-104.6: PROC Statement

**As a** CLIST developer, **I want** PROC to define positional and keyword parameters.

**Acceptance Criteria:**
- Given `PROC 2 NAME DEPT TITLE(ENGINEER) LEVEL(5)`, when the CLIST is invoked with `EXEC 'LIB(MYCLIST)' 'SMITH SALES'`, then &NAME=SMITH, &DEPT=SALES, &TITLE=ENGINEER, &LEVEL=5
- Given fewer positional parameters than defined, when PROMPT is active, then the user is prompted for missing values

---

## Dependency Graph

```
CL-100 (Lexer/Parser) → CL-101 (Interpreter Core) → CL-102 (Functions)
                                                   → CL-103 (I/O & Error)
                                                   → CL-104 (TSO/ISPF)
```

CL-100 is prerequisite for all others. CL-101 is prerequisite for CL-102, CL-103, CL-104. CL-102/103/104 are independent of each other.

## FR/NFR Coverage Matrix

| FR | Stories |
|----|---------|
| FR-CLIST-001 | CL-100.1, CL-100.2 |
| FR-CLIST-002 | CL-100.3 |
| FR-CLIST-003 | CL-101.1 |
| FR-CLIST-004 | CL-101.2 |
| FR-CLIST-005 | CL-102.1, CL-102.2, CL-102.3, CL-102.4 |
| FR-CLIST-006 | CL-103.1, CL-103.2 |
| FR-CLIST-007 | CL-103.3 |
| FR-CLIST-008 | CL-103.4, CL-103.5 |
| FR-CLIST-009 | CL-101.6, CL-104.5 |
| FR-CLIST-010 | CL-104.1, CL-104.2, CL-104.3, CL-104.4 |

| NFR | Stories |
|-----|---------|
| NFR-CLIST-001 | CL-100.5, CL-102.4 |
| NFR-CLIST-002 | CL-100.5 |
| NFR-CLIST-003 | CL-101.4 |

**Coverage: 10/10 FRs (100%), 3/3 NFRs (100%)**
