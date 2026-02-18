# Gap Analysis: CLIST (Command List)

## Official Specification Summary

CLIST (Command List, pronounced "C-list") is a procedural interpreted scripting language for TSO/E on z/OS. It originated in OS/360 Release 20 (late 1960s) and was the original TSO scripting language before REXX was introduced in TSO/E Version 2. While REXX has largely superseded CLIST for new development, CLIST remains present in many production mainframe environments.

CLIST is classified as **Core** (legacy) on mainframes:
- Built into every z/OS system as part of TSO/E
- Thousands of existing CLISTs in production at many shops
- Used for TSO command automation, ISPF panel driving, and batch submission
- Simpler than REXX but less powerful — primarily for sequential command execution
- Can only execute within a TSO/E address space (unlike REXX which can run in batch, CICS, etc.)

Key documentation:
- **z/OS TSO/E CLISTs** (SA32-0978) — primary reference
- **z/OS TSO/E Programming Guide** (SA32-0981) — programming context

## Key Features & Capabilities

### 1. CLIST Statements (~25 statements)

| Statement | Purpose |
|-----------|---------|
| **PROC** | Define CLIST parameters (positional and keyword) |
| **SET** | Assign value to a variable (arithmetic or string) |
| **READ** | Read input from terminal into a variable |
| **READDVAL** | Read multiple values from terminal into variables |
| **WRITE** | Write output to terminal (with newline) |
| **WRITENR** | Write output to terminal (no newline — prompting) |
| **IF / THEN / ELSE** | Conditional branching |
| **DO / WHILE / UNTIL / END** | Loop constructs (DO-WHILE, DO-UNTIL, iterative DO) |
| **SELECT / WHEN / OTHERWISE / END** | Multi-way conditional (added in TSO/E V2) |
| **GOTO** | Unconditional branch to label |
| **EXIT** | Exit CLIST with optional return code |
| **END** | End CLIST execution |
| **RETURN** | Return from ERROR/ATTN routine |
| **ERROR** | Establish error-handling routine (triggered on non-zero RC) |
| **ATTN** | Establish attention-handling routine (triggered by PA1/BREAK) |
| **CONTROL** | Set processing options (LIST, NOLIST, CONLIST, SYMLIST, MSG, NOMSG, PROMPT, NOPROMPT, FLUSH, NOFLUSH, MAIN, NOEND, ASIS, CAPS) |
| **OPENFILE** | Open a file for INPUT, OUTPUT, or UPDATE |
| **GETFILE** | Read a record from an opened file |
| **PUTFILE** | Write a record to an opened file |
| **CLOSFILE** | Close an opened file |
| **DATA / ENDDATA** | Pass data block to a command (e.g., subcommands to EDIT) |
| **DATA PROMPT / ENDDATA** | Pass data in response to command prompts |
| **TERMIN** | Read input from terminal as command or subcommand |
| **GLOBAL** | Share variables between CLIST and nested CLISTs |
| **NGLOBAL** | Declare numeric global variables |
| **SYSCALL** | Call an internal subprocedure |
| **SYSREF** | Declare variables passed by reference to subprocedure |
| **LISTDSI** | Retrieve dataset allocation information |

### 2. Variables

#### Symbolic Variables
- Prefixed with `&` (e.g., `&MYVAR`)
- No explicit typing — all values are character strings
- Arithmetic via `&EVAL()` function
- Nesting: `&&` for deferred substitution
- Maximum 252 characters per variable name

#### System Control Variables (~37 variables)

| Variable | Content |
|----------|---------|
| &LASTCC | Return code from last command |
| &MAXCC | Maximum return code |
| &SYSDATE | Current date (MM/DD/YY) |
| &SYSSDATE | Current date (YY/MM/DD) |
| &SYSJDATE | Current Julian date (YY.DDD) |
| &SYSTIME | Current time (HH:MM:SS) |
| &SYSSTIME | Current time (HH:MM) |
| &SYSUID | Current user ID |
| &SYSPROC | Current procedure library name |
| &SYSPCMD | Previous command name |
| &SYSSCMD | Previous subcommand name |
| &SYSNEST | Nesting level (YES/NO) |
| &SYSENV | Environment (FORE/BACK) |
| &SYSISPF | ISPF active (ACTIVE/NOT ACTIVE) |
| &SYSLTERM | Terminal line count |
| &SYSWTERM | Terminal width |
| &SYSCPU | CPU time used |
| &SYSSRV | Service units used |
| &SYSRACF | RACF status |
| &SYSLRACF | RACF level |
| &SYSTSOE | TSO/E version level |
| &SYSICMD | Initial command name |
| &SYSHSM | HSM status |
| &SYSDLM | End-of-data delimiter |
| &SYSDVAL | Input data from READ |
| &SYSNAME | System name |
| &SYSPROMPT | Prompt setting |
| &SYSSCAN | Scan substitution limit |

### 3. Built-in Functions (~15 functions)

| Function | Purpose |
|----------|---------|
| &EVAL(expr) | Evaluate arithmetic expression |
| &DATATYPE(expr) | Return data type (NUM or CHAR) |
| &LENGTH(expr) | Return byte length of expression |
| &SUBSTR(str,start[,len]) | Extract substring |
| &STR(text) | Define character data (prevents variable substitution within) |
| &NRSTR(text) | Preserve string integrity (no rescan) |
| &SYSDSN(dsname[(member)]) | Check dataset existence/availability |
| &SYSINDEX(needle,haystack[,start]) | Find string position within another string |
| &SYSCAPS(str) | Convert to uppercase |
| &SYSLC(str) | Convert to lowercase |
| &SYSNSUB(n,str) | Control substitution depth |

### 4. CONTROL Statement Options

The CONTROL statement governs CLIST execution behavior:

| Option | Effect |
|--------|--------|
| LIST / NOLIST | Display/suppress TSO commands before execution |
| CONLIST / NOCONLIST | Display/suppress CLIST statements before execution |
| SYMLIST / NOSYMLIST | Display/suppress symbolic substitution |
| MSG / NOMSG | Display/suppress command messages |
| PROMPT / NOPROMPT | Allow/suppress prompting for missing operands |
| FLUSH / NOFLUSH | Flush/preserve input stack on error |
| MAIN | Declare CLIST as main-level (cannot be stacked) |
| NOEND | Prevent END subcommand from terminating CLIST |
| ASIS / CAPS | Preserve case / uppercase all input |

### 5. File I/O Model

CLIST file I/O uses TSO dataset allocation:
```clist
ALLOC FILE(MYFILE) DA('USER.DATA') SHR
OPENFILE MYFILE INPUT
GETFILE MYFILE
/* &MYFILE now contains the record */
SET &LINE = &MYFILE
CLOSFILE MYFILE
FREE FILE(MYFILE)
```

- Files must be pre-allocated via TSO ALLOCATE
- Variable with same name as DD receives each record
- Sequential read/write only (no random access)
- One record at a time

### 6. Error and Attention Handling

```clist
ERROR DO
  SET &RC = &LASTCC
  IF &RC > 4 THEN WRITE ERROR: RC = &RC
  RETURN
END
ATTN DO
  WRITE ATTENTION RECEIVED
  EXIT CODE(8)
END
```

- ERROR routine triggers on non-zero return codes
- ATTN routine triggers on terminal attention (PA1)
- RETURN resumes execution after error handler

### 7. Subprocedures (TSO/E V2+)

```clist
PROC 0
SYSCALL MYSUB PARM1 PARM2
EXIT
MYSUB: PROC 2 P1 P2
  SYSREF P1
  SET &P1 = &P1 + 1
  RETURN
END
```

- SYSCALL invokes an internal label as a subprocedure
- SYSREF passes variables by reference
- GLOBAL/NGLOBAL share variables across CLIST nesting levels

### 8. ISPF Integration

CLISTs commonly drive ISPF services:
```clist
ISPEXEC DISPLAY PANEL(MYPANEL)
ISPEXEC BROWSE DATASET('MY.DATA')
ISPEXEC EDIT DATASET('MY.SOURCE')
ISPEXEC TBOPEN MYTABLE
```

- ISPEXEC prefix invokes ISPF Dialog Manager services
- ISREDIT prefix invokes ISPF Edit macro services
- CLIST variables map to ISPF dialog variables

### 9. Coding Rules

- One statement per line
- Continuation: `+` (strip leading blanks) or `-` (preserve leading blanks)
- Comments: not natively supported (convention: use WRITE or /* via DATA)
- Labels: followed by colon (e.g., `LOOP1:`)
- Uppercase by default (CONTROL ASIS for mixed case)
- Maximum line length: 32,756 characters (logical)

## Current OpenMainframe Status

**No CLIST implementation exists.** A grep for "clist" across all crates returned zero matches. There is no CLIST parser, interpreter, or TSO command environment.

CLIST's implementation depends heavily on TSO/E, which is also not implemented. Without TSO, CLIST has limited utility since it can only execute within a TSO address space.

## Gap Details

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| CLIST parser/interpreter | Full — ~25 statements | None | **Missing** |
| Symbolic variables (&-prefix, substitution) | Full | None | **Missing** |
| ~37 system control variables | Full | None | **Missing** |
| ~15 built-in functions | Full | None | **Missing** |
| PROC statement (parameter passing) | Full | None | **Missing** |
| SET (assignment, arithmetic via &EVAL) | Full | None | **Missing** |
| Control flow (IF, DO WHILE/UNTIL, SELECT/WHEN, GOTO) | Full | None | **Missing** |
| Terminal I/O (READ, WRITE, WRITENR, READDVAL, TERMIN) | Full | None | **Missing** |
| File I/O (OPENFILE, GETFILE, PUTFILE, CLOSFILE) | Full | None | **Missing** |
| CONTROL statement (13 options) | Full | None | **Missing** |
| ERROR/ATTN handling | Full | None | **Missing** |
| DATA/ENDDATA sequences | Full | None | **Missing** |
| Subprocedures (SYSCALL, SYSREF, GLOBAL/NGLOBAL) | Full | None | **Missing** |
| ISPF integration (ISPEXEC, ISREDIT) | Full | None (no ISPF) | **Missing** |
| LISTDSI (dataset info retrieval) | Full | None | **Missing** |
| TSO command execution | Full | None (no TSO) | **Missing** |
| Nested CLIST invocation (EXEC) | Full | None | **Missing** |

## Proposed Epic Structure

### Epic C100: CLIST Lexer and Parser
- **C100.1**: Source format — line-based, `+`/`-` continuation, label syntax
- **C100.2**: Tokenizer — statements, `&`-variables, literals, operators
- **C100.3**: Statement parser — all ~25 statement types
- **C100.4**: Expression parser — arithmetic, string, comparison, compound conditions
- **Complexity**: M (Medium — CLIST has a simpler syntax than REXX or PL/I)

### Epic C101: CLIST Interpreter Core
- **C101.1**: Variable pool — symbolic variables, `&`-prefix substitution, nested substitution
- **C101.2**: System control variables (~37 read-only variables)
- **C101.3**: SET statement — assignment with implicit string/numeric handling
- **C101.4**: Control flow — IF/THEN/ELSE, DO/WHILE/UNTIL/END, SELECT/WHEN/OTHERWISE
- **C101.5**: GOTO and label management
- **C101.6**: Subprocedures — SYSCALL, SYSREF, GLOBAL/NGLOBAL
- **Complexity**: M (Medium)

### Epic C102: CLIST Built-in Functions
- **C102.1**: &EVAL — arithmetic expression evaluation
- **C102.2**: String functions — &SUBSTR, &LENGTH, &SYSINDEX, &SYSCAPS, &SYSLC
- **C102.3**: Data functions — &DATATYPE, &STR, &NRSTR, &SYSNSUB
- **C102.4**: System functions — &SYSDSN (dataset availability check)
- **Complexity**: S (Small — only ~15 functions)

### Epic C103: CLIST I/O and Error Handling
- **C103.1**: Terminal I/O — READ, READDVAL, WRITE, WRITENR, TERMIN
- **C103.2**: File I/O — OPENFILE, GETFILE, PUTFILE, CLOSFILE (via dataset crate)
- **C103.3**: ERROR routine — automatic error trapping, RETURN
- **C103.4**: ATTN routine — attention interrupt handling
- **C103.5**: CONTROL statement — all 13 options
- **C103.6**: DATA/ENDDATA — data block passing to commands
- **Complexity**: M (Medium)

### Epic C104: CLIST TSO/ISPF Integration
- **C104.1**: TSO command dispatch — route commands to TSO environment
- **C104.2**: ISPEXEC — bridge to ISPF Dialog Manager services
- **C104.3**: ISREDIT — bridge to ISPF Edit services
- **C104.4**: LISTDSI — dataset information retrieval
- **C104.5**: Nested CLIST execution (EXEC command)
- **C104.6**: PROC statement — positional/keyword parameter handling
- **Complexity**: L (Large — depends on TSO/ISPF being implemented)

## Dependencies

| Dependency | Crate | Reason |
|------------|-------|--------|
| open-mainframe-lang-core | Shared traits | Span, Diagnostic types |
| open-mainframe-dataset | File I/O | OPENFILE/GETFILE/PUTFILE use TSO-allocated datasets |
| TSO (not implemented) | Command environment | CLISTs execute TSO commands — requires TSO |
| ISPF (not implemented) | Dialog services | ISPEXEC/ISREDIT for panel-driven applications |

**Critical dependency**: CLIST can only run within a TSO address space. Without TSO/E implementation, CLIST is non-functional. This makes CLIST lower priority than REXX (which can also run in batch) unless TSO is implemented first.

## Complexity Estimate

| Epic | Complexity | Rationale |
|------|-----------|-----------|
| C100 (Lexer/Parser) | M | Line-based format, ~25 keywords, simpler than REXX |
| C101 (Interpreter) | M | String-centric variables, basic control flow |
| C102 (Built-in Functions) | S | Only ~15 functions, mostly string operations |
| C103 (I/O & Error Handling) | M | File I/O, error/attention routines |
| C104 (TSO/ISPF Integration) | L | Requires TSO and ISPF to exist |

**Total estimated effort**: 5 epics, overall M complexity (CLIST is significantly simpler than REXX, but depends entirely on TSO/ISPF infrastructure that doesn't exist yet)

**Recommendation**: Implement CLIST after TSO/ISPF (Batch 9), or co-develop with TSO as CLIST is its primary scripting interface.

## Reference Documentation

- [z/OS 3.2 TSO/E CLISTs (SA32-0978)](https://www.ibm.com/docs/en/SSLTBW_3.2.0/pdf/ikjb800_v3r2.pdf)
- [z/OS 2.5 Overview of CLIST Statements](https://www.ibm.com/docs/en/zos/2.5.0?topic=conventions-overview-clist-statements)
- [The CLIST Language on z/OS](https://www.ibm.com/docs/en/zos-basic-skills?topic=zos-clist-language)
- [z/OS 2.4 TSO/E Programming Guide (SA32-0981)](https://www.ibm.com/docs/en/SSLTBW_2.4.0/pdf/ikjb600_v2r4.pdf)
- [TSO Extensions CLISTs Implementation and Reference (SC28-1304)](http://bitsavers.trailing-edge.com/pdf/ibm/370/TSO_Extensions/SC28-1304-1_TSO_Extensions_CLISTS_Implementation_and_Reference_Dec85.pdf)
- [CLIST — Wikipedia](https://en.wikipedia.org/wiki/CLIST)
- [TSO CLIST Programming in z/OS (training material)](https://www.trainersfriend.com/A650-FirstSection.pdf)
