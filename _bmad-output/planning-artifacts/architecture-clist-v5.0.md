---
version: 'v5.0'
planningGroup: 'PG-21'
technology: 'CLIST (Command List)'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-clist-v5.0.md'
---

# Architecture: CLIST (Command List)

## 1. Crate Strategy

**New crate:** `open-mainframe-clist`

Rationale: CLIST is a distinct scripting language with its own parser, interpreter, variable pool, and execution model. While it integrates with TSO/ISPF, it has no structural dependency on the REXX crate. A dedicated crate keeps the language boundary clean.

## 2. Module Layout

```
crates/open-mainframe-clist/src/
├── lib.rs             # Crate root, public API
├── parser/
│   ├── mod.rs         # Parser entry point
│   ├── lexer.rs       # Line-based tokenizer (statements, &-variables, operators)
│   ├── ast.rs         # AST node definitions
│   └── expr.rs        # Expression parser (arithmetic, string, comparison)
├── interpreter/
│   ├── mod.rs         # Interpreter loop — statement dispatch
│   ├── variables.rs   # Variable pool (&-prefix, substitution, system vars)
│   ├── control.rs     # IF, DO, SELECT, GOTO, EXIT control flow
│   ├── subproc.rs     # SYSCALL, SYSREF, GLOBAL/NGLOBAL
│   └── error.rs       # ERROR/ATTN routine management
├── io/
│   ├── mod.rs         # I/O dispatch
│   ├── terminal.rs    # READ, READDVAL, WRITE, WRITENR, TERMIN
│   └── file.rs        # OPENFILE, GETFILE, PUTFILE, CLOSFILE
├── functions.rs       # Built-in functions (&EVAL, &SUBSTR, &LENGTH, etc.)
├── control_stmt.rs    # CONTROL statement (13 options)
├── tso_bridge.rs      # TSO command dispatch, LISTDSI
└── ispf_bridge.rs     # ISPEXEC, ISREDIT dispatch
```

## 3. Key Types

```rust
/// CLIST source token
pub enum Token {
    Statement(StatementKind),
    Variable(String),        // &-prefixed variable name
    Literal(String),         // Quoted or unquoted string
    Number(i64),
    Operator(Op),
    Label(String),           // Colon-terminated label
    Continuation(ContinuationKind), // + or -
    Eof,
}

pub enum ContinuationKind {
    StripBlanks,   // +
    PreserveBlanks, // -
}

/// AST node for a CLIST statement
pub enum Statement {
    Proc(ProcStmt),
    Set(SetStmt),
    Read(ReadStmt),
    ReadDval(ReadDvalStmt),
    Write(WriteStmt),
    WriteNr(WriteStmt),
    If(IfStmt),
    Do(DoStmt),
    Select(SelectStmt),
    Goto(String),
    Exit(Option<Expr>),
    End,
    Return,
    Error(Vec<Statement>),
    Attn(Vec<Statement>),
    Control(Vec<ControlOption>),
    OpenFile(OpenFileStmt),
    GetFile(String),
    PutFile(String),
    ClosFile(String),
    Data(DataStmt),
    Termin(String),
    Global(Vec<String>),
    NGlobal(Vec<String>),
    SysCall(String, Vec<Expr>),
    SysRef(Vec<String>),
    ListDsi(ListDsiStmt),
    TsoCommand(String),       // Unrecognized → TSO dispatch
    IspExec(String),          // ISPEXEC prefix
    IsrEdit(String),          // ISREDIT prefix
}

/// Variable pool — all values are strings
pub struct VariablePool {
    user_vars: HashMap<String, String>,
    system_vars: HashMap<String, SystemVar>,
    global_vars: Arc<Mutex<HashMap<String, String>>>,
}

pub enum SystemVar {
    ReadOnly(fn(&ClistContext) -> String),
}

/// CLIST execution context
pub struct ClistContext {
    pub source: Vec<ClistLine>,
    pub variables: VariablePool,
    pub pc: usize,               // Program counter (line index)
    pub labels: HashMap<String, usize>,
    pub call_stack: Vec<CallFrame>,
    pub error_handler: Option<usize>,
    pub attn_handler: Option<usize>,
    pub control: ControlOptions,
    pub last_cc: i32,
    pub max_cc: i32,
    pub open_files: HashMap<String, OpenFile>,
    pub nesting_level: u32,
}

pub struct ControlOptions {
    pub list: bool,
    pub conlist: bool,
    pub symlist: bool,
    pub msg: bool,
    pub prompt: bool,
    pub flush: bool,
    pub main: bool,
    pub noend: bool,
    pub asis: bool,
}

/// Trait for TSO command dispatch (dependency injection)
pub trait TsoEnvironment: Send + Sync {
    fn execute_command(&mut self, command: &str) -> Result<i32, ClistError>;
    fn allocate_file(&mut self, ddname: &str, dsname: &str) -> Result<(), ClistError>;
    fn free_file(&mut self, ddname: &str) -> Result<(), ClistError>;
}

/// Trait for ISPF service dispatch (dependency injection)
pub trait IspfServices: Send + Sync {
    fn ispexec(&mut self, service: &str, vars: &mut VariablePool) -> Result<i32, ClistError>;
    fn isredit(&mut self, macro_cmd: &str, vars: &mut VariablePool) -> Result<i32, ClistError>;
}
```

## 4. Design Decisions

### DD-5.0-CLIST-01: Line-Based Interpreter
**Decision:** CLIST is implemented as a line-based interpreter (not AST-walking). The parser produces a flat list of parsed statements indexed by line number, and the interpreter advances a program counter. This matches real CLIST semantics where GOTO targets labels by line position.

### DD-5.0-CLIST-02: TSO/ISPF via Trait Injection
**Decision:** TSO command dispatch and ISPF services are abstracted behind traits (`TsoEnvironment`, `IspfServices`). This allows the CLIST interpreter to function with mock TSO/ISPF for testing, and to connect to real implementations when available. Without a TSO implementation, CLISTs can still be parsed and partially executed (pure computation without TSO commands).

### DD-5.0-CLIST-03: String-Centric Variable Model
**Decision:** All CLIST variables are stored as strings (matching real CLIST semantics). Numeric operations are performed by converting to i64/f64 for evaluation via `&EVAL()` and converting back to string. No type system beyond the implicit string model.

### DD-5.0-CLIST-04: Shared Global Variable Pool
**Decision:** GLOBAL/NGLOBAL variables are shared via `Arc<Mutex<HashMap>>` across nested CLIST invocations. Each nested CLIST gets its own local `VariablePool` but shares the global pool with its parent.
