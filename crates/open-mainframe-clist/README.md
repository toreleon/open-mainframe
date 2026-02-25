# open-mainframe-clist

z/OS **CLIST (Command List)** scripting language interpreter — a complete lexer,
parser, and runtime for the TSO/E CLIST language used to automate tasks under
TSO on IBM mainframes.

## Overview

This crate implements a full CLIST scripting environment comprising five
cooperating modules:

| Module | Lines | Responsibility |
|---|---|---|
| `parser.rs` | 1 647 | Source-line processing, tokenization, AST generation |
| `interpreter.rs` | 1 062 | Variable pool, control flow, statement execution |
| `functions.rs` | 455 | 11 built-in functions (`&EVAL`, `&SUBSTR`, etc.) |
| `io.rs` | 393 | Terminal I/O, sequential file I/O, CONTROL options |
| `tso_bridge.rs` | 308 | TSO command dispatch, ISPEXEC, ISREDIT, LISTDSI |

**Total**: ~3 865 lines of Rust (including tests).

## Architecture

```
                  ┌────────────────────────┐
                  │     CLIST Source        │
                  └──────────┬─────────────┘
                             │
                 ┌───────────▼────────────┐
                 │  Source Line Processor  │
                 │  (+/- continuations)    │
                 └───────────┬────────────┘
                             │
                 ┌───────────▼────────────┐
                 │      Tokenizer         │
                 │  (11 token kinds)      │
                 └───────────┬────────────┘
                             │
                 ┌───────────▼────────────┐
                 │    Statement Parser    │
                 │  (~30 statement types) │
                 └───────────┬────────────┘
                             │
                 ┌───────────▼────────────┐
                 │       ClistAst         │
                 └───────────┬────────────┘
                             │
          ┌──────────────────▼──────────────────┐
          │         ClistInterpreter            │
          │                                     │
          │  VariablePool ─── System Variables  │
          │  Label Map    ─── Control Options   │
          │  Error/Attn Handlers                │
          │                                     │
          ├────────┬──────────┬─────────────────┤
          │ IoMgr  │BuiltinFn│  TsoEnvironment │
          └────────┴──────────┴─────────────────┘
```

## Key Types and Traits

### Parser (`parser.rs`)

| Type | Description |
|---|---|
| `ClistAst` | Top-level AST: `Vec<ClistStatement>` + line map |
| `ClistStatement` | Enum with ~30 variants (SET, IF, DO, SELECT, GOTO, EXIT, WRITE, READ, OPENFILE, GETFILE, PUTFILE, CLOSFILE, ERROR, ATTN, CONTROL, PROC, SYSCALL, GLOBAL, NGLOBAL, SYSREF, EXEC, LISTDSI, ISPEXEC, ISREDIT, TsoCommand, RETURN, DATA, TERMIN, Label) |
| `Expression` | Recursive enum: `Number`, `StringLit`, `Variable`, `BinaryOp`, `UnaryOp`, `FunctionCall` |
| `ExprOp` | 15 operators: Add, Subtract, Multiply, Divide, Modulo, Equal, NotEqual, LessThan, GreaterThan, LessOrEqual, GreaterOrEqual, And, Or, Not, Concat |
| `ClistToken` / `ClistTokenKind` | Token with line/col position; 11 kinds (Keyword, Variable, StringLiteral, Number, Operator, LeftParen, RightParen, Comma, Label, Identifier, EndOfLine) |
| `DoCondition` | `While` / `Until` |
| `ErrorAction` | `Off` / `Body(Vec<ClistStatement>)` / `Return` |
| `ParseError` | Syntax, UnexpectedEnd, UnknownStatement |

### Interpreter (`interpreter.rs`)

| Type | Description |
|---|---|
| `ClistInterpreter` | Main runtime: variables, system vars, I/O, control options, TSO bridge, label map, error/attn handlers, nesting depth, output buffer |
| `VariablePool` | Local `HashMap` + shared `Arc<Mutex<HashMap>>` globals; case-insensitive |
| `ControlVariable` | Named system variable with string value |
| `SubprocDef` | Subprocedure definition (label + body) |
| `InterpreterError` | UndefinedVariable, TypeError, LabelNotFound, DivisionByZero, Parse, Io, Exit, MaxIterations, General |
| `ExecResult<T>` | `Result<T, InterpreterError>` |

### Built-in Functions (`functions.rs`)

| Type | Description |
|---|---|
| `BuiltinFunction` | Enum of 11 function identifiers |
| `FunctionError` | Unknown, ArgCount, InvalidArg, Arithmetic |

### I/O (`io.rs`)

| Type | Description |
|---|---|
| `IoManager` | Terminal input queue + output buffer + open file registry |
| `ClistFile` | Open file with name, mode, read position, written lines |
| `FileMode` | `Input` / `Output` / `Update` |
| `ControlOptions` | 9 flags: list, conlist, symlist, msg, prompt, asis, main, flush, noend |
| `ErrorRoutine` / `AttnRoutine` | Handler definitions with active flag and body |
| `IoError` | FileNotOpen, FileAlreadyOpen, EndOfFile, NoInput, InvalidMode, WriteError |

### TSO Bridge (`tso_bridge.rs`)

| Type | Description |
|---|---|
| `TsoEnvironment` (trait) | `execute_command`, `ispexec`, `isredit`, `listdsi`, `get_ispf_variable`, `set_ispf_variable` |
| `MockTsoEnvironment` | Testing double: registered datasets, ISPF variable pool, command history |
| `DatasetAttributes` | 11 fields: dsorg, recfm, lrecl, blksize, volume, primary, secondary, units, members, creation_date, expiration_date |
| `ListdsiResult` | return_code, reason_code, optional attributes |

## Implementation Details

### Source Line Processing (CL-100.1)

`process_source_lines` handles CLIST continuation conventions:

- **`+` at end of line**: join with next line, strip leading blanks
- **`-` at end of line**: join with next line, preserve leading blanks
- Blank and comment lines are skipped during continuation

Returns a vector of `(line_number, logical_line)` tuples.

### Tokenizer (CL-100.2)

`tokenize_line` produces tokens from a single logical line:

- `/* ... */` comments are stripped
- Single-quoted strings with `''` escape for embedded quotes
- `&name` and `&&name` variable references (uppercased)
- Operators: `+`, `-`, `*`, `/`, `=`, `<`, `>`, `<=`, `>=`, `<>`
- Parentheses, commas, colons (label separators)
- Numeric literals (i64)
- Keywords recognized via `is_keyword()` (~50 keywords including statement names, comparison operators, CONTROL options)
- Bare identifiers for dataset names, TSO commands, etc.

### Expression Parser (CL-100.4)

Recursive descent with operator precedence:

1. **Comparison/logical** (lowest): `EQ`, `NE`, `LT`, `GT`, `LE`, `GE`, `AND`, `OR`, `=`, `<>`, `<`, `>`, `<=`, `>=`
2. **Additive**: `+`, `-`
3. **Multiplicative**: `*`, `/`
4. **Parenthesized** expressions
5. **Function calls**: `&FUNC(arg1, arg2, ...)`
6. **Atoms**: numbers, string literals, variable references

Scanning proceeds right-to-left at each precedence level to achieve correct
left-to-right associativity.

### Interpreter Execution Model (CL-101)

The interpreter uses a **program counter (PC)** approach for top-level
statements, enabling GOTO label resolution:

```
execute(source)
  → parse_clist(source) → ClistAst
  → build label map: HashMap<String, usize>
  → execute_top_level: pc-based loop
      → execute_statement → StmtResult { Continue, Goto, Exit, Return }
      → on Goto: look up label → reset pc
      → on error: invoke error_handler if set
```

**DO loops** are executed with a separate iteration counter capped at
`max_iterations` (default 100,000) to prevent runaway scripts.

**Block execution** (`execute_block`) propagates Goto/Exit/Return upward
through nested IF, DO, and SELECT bodies.

### Variable Resolution (CL-101.1-2)

Resolution order:
1. **User variables** — local `HashMap` or shared globals (via `Arc<Mutex>`)
2. **System control variables** — 40+ predefined variables
3. **Empty string** — undefined variables resolve to `""` (CLIST convention)

Variables are case-insensitive (uppercased internally). The `GLOBAL` statement
moves a variable from local to shared pool; `NGLOBAL` moves it back.

### Variable Substitution

`substitute_variables` scans for `&name` patterns in string values and
replaces them with resolved values. This occurs during expression evaluation
for `StringLit` nodes.

### Built-in Functions (CL-102)

| Function | Args | Description |
|---|---|---|
| `&EVAL(expr)` | 1 | Recursive arithmetic evaluator with `+`, `-`, `*`, `/`, parentheses; integer (i64) precision |
| `&SUBSTR(pos, string)` | 2 | Substring extraction; `start:end` (1-based) or `start` only |
| `&LENGTH(string)` | 1 | String length |
| `&SYSINDEX(needle, haystack [,start])` | 2-3 | Find substring position (1-based), 0 if not found |
| `&SYSCAPS(string)` | 1 | Uppercase conversion |
| `&SYSLC(string)` | 1 | Lowercase conversion |
| `&DATATYPE(value)` | 1 | Returns `"NUM"` or `"CHAR"` |
| `&STR(text)` | 0-1 | Return without substitution (passthrough) |
| `&NRSTR(text)` | 0-1 | Suppress all substitution (passthrough) |
| `&SYSNSUB(text)` | 0-1 | Returns `"1"` if `&` present, `"0"` otherwise |
| `&SYSDSN(dsname)` | 1 | Dataset existence check; returns `"OK"` or `"MISSING DATASET NAME"` |

### System Control Variables (CL-101.2)

40+ predefined variables initialized at interpreter startup:

| Category | Variables |
|---|---|
| Return codes | `LASTCC`, `MAXCC` |
| Date/time | `SYSDATE`, `SYSSDATE`, `SYSJDATE`, `SYSTIME`, `SYSSTIME` |
| User/system | `SYSUID`, `SYSPREF`, `SYSENV`, `SYSNODE`, `SYSNAME`, `SYSTERMID`, `SYSJOBID` |
| Command tracking | `SYSICMD`, `SYSPCMD`, `SYSSCMD`, `SYSNEST` |
| TSO/ISPF status | `SYSISPF`, `SYSTSOE`, `SYSPROMPT`, `SYSRACF`, `SYSLRACF`, `SYSHSM`, `SYSJES` |
| Terminal info | `SYSLTERM` (24 rows), `SYSWTERM` (80 cols) |
| LISTDSI results | `SYSDSORG`, `SYSRECFM`, `SYSLRECL`, `SYSBLKSIZE`, `SYSVOLUME`, `SYSPRIMARY`, `SYSSECONDS`, `SYSUNITS`, `SYSMEMBERS` |
| Miscellaneous | `SYSSCAN`, `SYSDLM`, `SYSDVAL`, `SYSCPU`, `SYSSRV`, `SYSMVS`, `SYSAPPCLU`, `SYSPROC` |

### CONTROL Options (CL-103.6)

| Option | Default | Description |
|---|---|---|
| `LIST` / `NOLIST` | off | Display each statement before execution |
| `CONLIST` / `NOCONLIST` | off | Display after variable substitution |
| `SYMLIST` / `NOSYMLIST` | off | Display symbolic variable resolution |
| `MSG` / `NOMSG` | on | Display TSO messages |
| `PROMPT` / `NOPROMPT` | on | Allow terminal prompts |
| `ASIS` / `CAPS` | caps | Case handling for input |
| `MAIN` | off | Declare as main CLIST |
| `FLUSH` / `NOFLUSH` | on | Flush input stack on error |
| `NOEND` | off | Prevent END from terminating CLIST |

### TSO/ISPF Integration (CL-104)

The `TsoEnvironment` trait provides an abstraction layer for:

- **TSO command dispatch** — unrecognized statements are sent to `execute_command`
- **ISPEXEC** — ISPF Dialog Manager service invocation
- **ISREDIT** — ISPF Edit macro commands
- **LISTDSI** — dataset attribute retrieval; populates 9 `SYS*` variables on success (RC=0) or sets RC=16 on failure
- **ISPF shared variables** — get/set via `get_ispf_variable` / `set_ispf_variable`

`MockTsoEnvironment` provides a testing double with a dataset registry, ISPF
variable pool, and command history recording.

## Syntax / Feature Coverage

### Statement Types

| Statement | Status | Notes |
|---|---|---|
| `SET &var = expr` | Implemented | Full expression support |
| `IF ... THEN ... ELSE` | Implemented | Inline and block forms |
| `DO` / `DO WHILE` / `DO UNTIL` | Implemented | With max-iteration safety |
| `SELECT` / `WHEN` / `OTHERWISE` | Implemented | Multi-branch selection |
| `GOTO label` | Implemented | PC-based label resolution |
| `EXIT [code]` | Implemented | Optional return code |
| `RETURN [code]` | Implemented | Subprocedure return |
| `WRITE expr` | Implemented | Terminal output with newline |
| `WRITENR expr` | Implemented | Terminal output without newline |
| `READ &var` | Implemented | Terminal input; defaults to `&SYSDVAL` |
| `READDVAL &var1 &var2 ...` | Implemented | Multi-variable input split |
| `TERMIN` | Implemented | Raw terminal read |
| `OPENFILE &file [mode]` | Implemented | INPUT / OUTPUT / UPDATE |
| `GETFILE &file` | Implemented | Read next line from file |
| `PUTFILE &file` | Implemented | Write line to file |
| `CLOSFILE &file` | Implemented | Close file |
| `ERROR DO ... END` / `OFF` / `RETURN` | Implemented | Error handler registration |
| `ATTN DO ... END` / `OFF` / `RETURN` | Implemented | Attention handler registration |
| `CONTROL options` | Implemented | 9 option flags |
| `PROC n [keywords]` | Implemented | Positional count + keyword defaults |
| `SYSCALL label (args)` | Implemented | Internal subprocedure call |
| `GLOBAL &vars` | Implemented | Shared variable declaration |
| `NGLOBAL &vars` | Implemented | Return variable to local scope |
| `SYSREF &vars` | Implemented | Pass-by-reference declaration |
| `EXEC 'name' args` | Implemented | Nested CLIST invocation (stub) |
| `LISTDSI 'dsname'` | Implemented | Via TsoEnvironment trait |
| `ISPEXEC service` | Implemented | Via TsoEnvironment trait |
| `ISREDIT command` | Implemented | Via TsoEnvironment trait |
| `DATA ... ENDDATA` | Implemented | Raw data block capture |
| `label:` definitions | Implemented | GOTO targets |
| Unrecognized statements | Implemented | Forwarded as TSO commands |

### Expression Operators

| Operator | Symbol | Keyword | Status |
|---|---|---|---|
| Addition | `+` | — | Implemented |
| Subtraction | `-` | — | Implemented |
| Multiplication | `*` | — | Implemented |
| Division | `/` | — | Implemented |
| Modulo | `//` | — | Implemented |
| Equal | `=` | `EQ` | Implemented |
| Not Equal | `<>` | `NE` | Implemented |
| Less Than | `<` | `LT` | Implemented |
| Greater Than | `>` | `GT` | Implemented |
| Less or Equal | `<=` | `LE` | Implemented |
| Greater or Equal | `>=` | `GE` | Implemented |
| Logical AND | — | `AND` | Implemented |
| Logical OR | — | `OR` | Implemented |
| Logical NOT | — | `NOT` | Implemented |
| Concatenation | `\|\|` | — | Implemented |

## Usage Examples

### Basic Script Execution

```rust
use open_mainframe_clist::ClistInterpreter;

let mut interp = ClistInterpreter::new();
let source = r#"
PROC 0
CONTROL NOLIST NOMSG
SET &SUM = 0
SET &I = 1
LOOP: IF &I GT 10 THEN GOTO DONE
  SET &SUM = &SUM + &I
  SET &I = &I + 1
  GOTO LOOP
DONE: WRITE &SUM
EXIT 0
"#;
let rc = interp.execute(source).unwrap();
assert_eq!(rc, 0);
assert_eq!(interp.output(), &["55"]);
```

### With TSO Environment

```rust
use open_mainframe_clist::{ClistInterpreter, DatasetAttributes};
use open_mainframe_clist::tso_bridge::MockTsoEnvironment;

let mut interp = ClistInterpreter::new();
let mut mock = MockTsoEnvironment::new();
mock.add_dataset("SYS1.MACLIB", DatasetAttributes {
    dsorg: "PO".to_string(),
    recfm: "FB".to_string(),
    lrecl: 80,
    blksize: 27920,
    volume: "SYSRES".to_string(),
    ..Default::default()
});
interp.set_tso(Box::new(mock));

let rc = interp.execute(r#"
PROC 0
LISTDSI 'SYS1.MACLIB'
WRITE 'Dataset found'
EXIT 0
"#).unwrap();
assert_eq!(rc, 0);
```

### Shared Global Variables

```rust
use open_mainframe_clist::ClistInterpreter;
use std::sync::{Arc, Mutex};
use std::collections::HashMap;

let globals = Arc::new(Mutex::new(HashMap::new()));
let mut interp1 = ClistInterpreter::with_globals(globals.clone());
let mut interp2 = ClistInterpreter::with_globals(globals);

interp1.execute("GLOBAL &SHARED\nSET &SHARED = 'hello'").unwrap();
interp2.execute("GLOBAL &SHARED\nWRITE &SHARED").unwrap();
assert_eq!(interp2.output(), &["hello"]);
```

## Dependencies

| Crate | Purpose |
|---|---|
| `thiserror` | Derive `Error` for `ParseError`, `InterpreterError`, `FunctionError`, `IoError` |
| `miette` | Diagnostic error reporting |

No other crate dependencies. The CLIST interpreter is fully self-contained.

## Testing

Each module includes its own `#[cfg(test)]` section:

- **`parser.rs`** — 20 tests: source line processing, tokenizer, statement parser, expression parser, full CLIST parse, error/file handlers, DATA blocks
- **`interpreter.rs`** — 16 tests: variable pool, system variables, SET, IF, DO WHILE/UNTIL, GOTO, EXIT, GLOBAL, SELECT/WHEN, CONTROL, WRITE, arithmetic loop
- **`functions.rs`** — 17 tests: &EVAL (simple, addition, multiplication, precedence, division, div-by-zero), &SUBSTR, &LENGTH, &SYSINDEX, &SYSCAPS, &SYSLC, &DATATYPE, &STR, &SYSDSN, unknown function
- **`io.rs`** — 14 tests: terminal read/write, file open/close/get/put, file mode enforcement, error/attn routine defaults, CONTROL defaults
- **`tso_bridge.rs`** — 10 tests: TSO command dispatch, ISPEXEC, ISREDIT, LISTDSI success/failure, nested EXEC, PROC, mock environment, ISPF variables, dataset attributes, full CLIST integration with TSO

```sh
cargo test -p open-mainframe-clist
```

## Limitations and Future Work

- **Nested EXEC** is stubbed — logs the invocation but does not load external CLIST source
- **SYSCALL** subprocedure dispatch logs the call but does not execute the label body
- **&SYSDSN** always returns `"OK"` for non-empty names (no real dataset catalog)
- **&STR / &NRSTR / &SYSNSUB** substitution suppression is approximate since substitution occurs before the function sees its arguments
- **Arithmetic** uses `i64` only; no floating-point or packed-decimal support
- **DO loop nesting** inside DO does not maintain independent depth counters for the inner/outer loops within the parser
- **LISTDSI** only sets dataset-level system variables; member-level attributes and SMS information are not populated
- **No line-number diagnostics** on interpreter errors (only parser errors carry line numbers)
