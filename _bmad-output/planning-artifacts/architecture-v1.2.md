---
version: 'v1.2'
baseVersion: 'v1.1'
date: '2026-02-13'
status: 'draft'
---

# Architecture - zOS-clone v1.2: Enterprise Features

## Overview

v1.2 introduces two major subsystems: DB2 SQL support with PostgreSQL backend and CICS transaction processing. Both integrate with the existing COBOL compiler via preprocessor extensions.

---

## System Architecture

### High-Level Component Diagram

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           zOS-clone v1.2                                 │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌──────────────────────────────────────────────────────────────────┐   │
│  │                    Source Processing Layer                        │   │
│  │  ┌───────────────┐  ┌───────────────┐  ┌───────────────┐        │   │
│  │  │ COBOL Lexer   │  │ SQL Preproc   │  │ CICS Preproc  │        │   │
│  │  │   (v1.0)      │  │   (NEW)       │  │    (NEW)      │        │   │
│  │  └───────────────┘  └───────────────┘  └───────────────┘        │   │
│  └──────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  ┌──────────────────────────────────────────────────────────────────┐   │
│  │                     Compilation Layer                             │   │
│  │  ┌───────────────┐  ┌───────────────┐  ┌───────────────┐        │   │
│  │  │ COBOL Parser  │  │ Semantic      │  │ LLVM Codegen  │        │   │
│  │  │   (v1.0)      │  │ Analyzer      │  │    (v1.0)     │        │   │
│  │  └───────────────┘  └───────────────┘  └───────────────┘        │   │
│  └──────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  ┌──────────────────────────────────────────────────────────────────┐   │
│  │                      Runtime Layer                                │   │
│  │  ┌───────────────┐  ┌───────────────┐  ┌───────────────┐        │   │
│  │  │ COBOL Runtime │  │ DB2 Runtime   │  │ CICS Runtime  │        │   │
│  │  │   (v1.0)      │  │    (NEW)      │  │    (NEW)      │        │   │
│  │  └───────────────┘  └───────────────┘  └───────────────┘        │   │
│  └──────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  ┌──────────────────────────────────────────────────────────────────┐   │
│  │                      Data Layer                                   │   │
│  │  ┌───────────────┐  ┌───────────────┐  ┌───────────────┐        │   │
│  │  │ VSAM Files    │  │ PostgreSQL    │  │ BMS Screens   │        │   │
│  │  │   (v1.1)      │  │    (NEW)      │  │    (NEW)      │        │   │
│  │  └───────────────┘  └───────────────┘  └───────────────┘        │   │
│  └──────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## DB2 Subsystem Architecture

### Component Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                      zos-db2 Crate                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │                 SQL Preprocessor                         │    │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐      │    │
│  │  │ SQL Scanner │  │ SQL Parser  │  │ Code Gen    │      │    │
│  │  │ (EXEC SQL)  │  │             │  │ (COBOL)     │      │    │
│  │  └─────────────┘  └─────────────┘  └─────────────┘      │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │                  SQL Translator                          │    │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐      │    │
│  │  │ DB2 AST     │  │ Transform   │  │ PostgreSQL  │      │    │
│  │  │             │  │             │  │ SQL Gen     │      │    │
│  │  └─────────────┘  └─────────────┘  └─────────────┘      │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │                   DB2 Runtime                            │    │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐      │    │
│  │  │ Connection  │  │ Statement   │  │ SQLCA       │      │    │
│  │  │ Pool        │  │ Cache       │  │ Manager     │      │    │
│  │  └─────────────┘  └─────────────┘  └─────────────┘      │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                      PostgreSQL                                  │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐              │
│  │ Tables      │  │ Indexes     │  │ Functions   │              │
│  │ (DB2 schema)│  │             │  │ (DB2 compat)│              │
│  └─────────────┘  └─────────────┘  └─────────────┘              │
└─────────────────────────────────────────────────────────────────┘
```

### SQL Preprocessing Flow

```
Input: COBOL + EXEC SQL
┌─────────────────────────────────────────────────────────────┐
│ IDENTIFICATION DIVISION.                                     │
│ PROGRAM-ID. CUSTINQ.                                        │
│ DATA DIVISION.                                              │
│ WORKING-STORAGE SECTION.                                    │
│ 01 WS-CUSTNO    PIC X(10).                                  │
│ 01 WS-NAME      PIC X(30).                                  │
│     EXEC SQL                                                │
│       SELECT NAME INTO :WS-NAME                             │
│       FROM CUSTOMER                                         │
│       WHERE CUSTNO = :WS-CUSTNO                             │
│     END-EXEC.                                               │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼ SQL Preprocessor
┌─────────────────────────────────────────────────────────────┐
│ IDENTIFICATION DIVISION.                                     │
│ PROGRAM-ID. CUSTINQ.                                        │
│ DATA DIVISION.                                              │
│ WORKING-STORAGE SECTION.                                    │
│ 01 WS-CUSTNO    PIC X(10).                                  │
│ 01 WS-NAME      PIC X(30).                                  │
│     COPY SQLCA.                                             │
│     CALL "SQLEXEC" USING SQL-STMT-001                       │
│                          WS-CUSTNO                          │
│                          WS-NAME                            │
│                          SQLCA.                             │
└─────────────────────────────────────────────────────────────┘
Output: Pure COBOL + DBRM
```

### DB2 to PostgreSQL Translation

| DB2 Syntax | PostgreSQL Equivalent |
|------------|----------------------|
| `VARCHAR(n) FOR BIT DATA` | `BYTEA` |
| `DECIMAL(p,s)` | `NUMERIC(p,s)` |
| `TIMESTAMP` | `TIMESTAMP` |
| `CURRENT TIMESTAMP` | `CURRENT_TIMESTAMP` |
| `FETCH FIRST n ROWS ONLY` | `LIMIT n` |
| `SUBSTR(str, pos, len)` | `SUBSTRING(str FROM pos FOR len)` |
| `CONCAT(a, b)` | `a \|\| b` |
| `VALUE(a, b)` | `COALESCE(a, b)` |
| `LOCATE(a, b)` | `POSITION(a IN b)` |

### SQLCA Structure

```rust
/// SQL Communication Area
pub struct Sqlca {
    /// SQL return code
    pub sqlcode: i32,
    /// Error message length
    pub sqlerrml: i16,
    /// Error message text
    pub sqlerrmc: String,
    /// Error position info
    pub sqlerrd: [i32; 6],
    /// Warning flags
    pub sqlwarn: [char; 11],
    /// SQL state (ANSI)
    pub sqlstate: String,
}

impl Sqlca {
    /// Success
    pub const SUCCESS: i32 = 0;
    /// Not found
    pub const NOT_FOUND: i32 = 100;
    /// Duplicate key
    pub const DUPLICATE_KEY: i32 = -803;
    /// Null value
    pub const NULL_VALUE: i32 = -305;
}
```

---

## CICS Subsystem Architecture

### Component Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                       zos-cics Crate                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │                 CICS Preprocessor                        │    │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐      │    │
│  │  │ Command     │  │ Command     │  │ COBOL Code  │      │    │
│  │  │ Scanner     │  │ Parser      │  │ Generator   │      │    │
│  │  └─────────────┘  └─────────────┘  └─────────────┘      │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │                 Command Processor                        │    │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐      │    │
│  │  │ Program     │  │ File        │  │ Terminal    │      │    │
│  │  │ Control     │  │ Control     │  │ Control     │      │    │
│  │  └─────────────┘  └─────────────┘  └─────────────┘      │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │                  Task Manager                            │    │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐      │    │
│  │  │ Task        │  │ Storage     │  │ Exception   │      │    │
│  │  │ Control     │  │ Manager     │  │ Handler     │      │    │
│  │  └─────────────┘  └─────────────┘  └─────────────┘      │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### CICS Command Categories

```rust
/// CICS command types
pub enum CicsCommand {
    // Program Control
    Link { program: String, commarea: Option<Vec<u8>> },
    Xctl { program: String, commarea: Option<Vec<u8>> },
    Return { transid: Option<String>, commarea: Option<Vec<u8>> },

    // Terminal Control
    Send { map: String, mapset: String, data: Vec<u8> },
    Receive { map: String, mapset: String },
    SendText { text: String },

    // File Control
    Read { file: String, ridfld: Vec<u8>, into: Vec<u8> },
    Write { file: String, ridfld: Vec<u8>, from: Vec<u8> },
    Rewrite { file: String, from: Vec<u8> },
    Delete { file: String, ridfld: Vec<u8> },

    // Browse
    StartBr { file: String, ridfld: Vec<u8> },
    ReadNext { file: String, into: Vec<u8> },
    ReadPrev { file: String, into: Vec<u8> },
    EndBr { file: String },

    // Storage
    Getmain { length: usize, set: *mut u8 },
    Freemain { data: *mut u8 },

    // Abend
    Abend { abcode: String },
}
```

### EIBLK (Execute Interface Block)

```rust
/// CICS Execute Interface Block
pub struct Eib {
    /// Transaction ID
    pub eibtrnid: [u8; 4],
    /// Task number
    pub eibtaskn: u32,
    /// Time (HHMMSS)
    pub eibtime: u32,
    /// Date (YYYYDDD)
    pub eibdate: u32,
    /// Terminal ID
    pub eibtrmid: [u8; 4],
    /// Response code
    pub eibresp: i32,
    /// Response code 2
    pub eibresp2: i32,
    /// Current program
    pub eibprog: [u8; 8],
    /// Cursor position
    pub eibcposn: i16,
    /// Communication area length
    pub eibcalen: i16,
    /// Attention identifier
    pub eibaid: u8,
}
```

### BMS Map Structure

```
┌─────────────────────────────────────────────────────────────────┐
│                       zos-bms Crate                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │                  BMS Compiler                            │    │
│  │  Input: DFHMSD/DFHMDI/DFHMDF macros                      │    │
│  │  Output: COBOL copybook + screen layout                  │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │                  Screen Renderer                         │    │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐      │    │
│  │  │ Console     │  │ HTML        │  │ JSON        │      │    │
│  │  │ (ANSI)      │  │ (Web)       │  │ (REST)      │      │    │
│  │  └─────────────┘  └─────────────┘  └─────────────┘      │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### BMS Map Definition Example

```
Input: BMS Macro
─────────────────────────────────────
CUSTMAP  DFHMSD TYPE=MAP,LANG=COBOL,MODE=INOUT
CUSTINQ  DFHMDI SIZE=(24,80),LINE=1
         DFHMDF POS=(1,30),LENGTH=20,INITIAL='CUSTOMER INQUIRY'
CUSTNO   DFHMDF POS=(5,15),LENGTH=10,ATTRB=(UNPROT,IC)
NAME     DFHMDF POS=(7,15),LENGTH=30,ATTRB=PROT
         DFHMSD TYPE=FINAL
─────────────────────────────────────
                    │
                    ▼ BMS Compiler
─────────────────────────────────────
Output: COBOL Copybook
01  CUSTINQI.
    05  CUSTNOL    PIC S9(4) COMP.
    05  CUSTNOF    PIC X.
    05  CUSTNOI    PIC X(10).
    05  NAMEL      PIC S9(4) COMP.
    05  NAMEF      PIC X.
    05  NAMEI      PIC X(30).
01  CUSTINQO REDEFINES CUSTINQI.
    05  FILLER     PIC X(3).
    05  CUSTNOO    PIC X(10).
    05  FILLER     PIC X(3).
    05  NAMEO      PIC X(30).
```

---

## Migration Assessment Architecture

### Component Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                     zos-migrate Crate                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │                  Codebase Scanner                        │    │
│  │  - File discovery (COBOL, JCL, copybooks)                │    │
│  │  - Encoding detection (EBCDIC/ASCII)                     │    │
│  │  - Size metrics                                          │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │                Compatibility Analyzer                    │    │
│  │  - Language feature detection                            │    │
│  │  - Unsupported construct flagging                        │    │
│  │  - Dependency mapping                                    │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │                  Report Generator                        │    │
│  │  - Compatibility percentage                              │    │
│  │  - Issue summary by category                             │    │
│  │  - Remediation suggestions                               │    │
│  │  - Effort estimation                                     │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Compatibility Matrix

| Feature Category | v1.0 | v1.1 | v1.2 | Notes |
|-----------------|------|------|------|-------|
| COBOL-85 | 95% | 95% | 98% | OO-COBOL in v1.2 |
| Sequential Files | 100% | 100% | 100% | Complete |
| VSAM KSDS | - | 100% | 100% | Complete |
| VSAM ESDS/RRDS | - | 100% | 100% | Complete |
| SORT/DFSORT | - | 90% | 95% | Common options |
| GDG | - | 100% | 100% | Complete |
| IDCAMS | - | 80% | 90% | Major commands |
| DB2 SQL | - | - | 80% | Common syntax |
| CICS Commands | - | - | 40% | Foundation set |

---

## File/Crate Layout

```
crates/
├── zos-db2/                    # NEW: DB2 subsystem
│   ├── src/
│   │   ├── lib.rs
│   │   ├── preprocess/         # SQL preprocessor
│   │   │   ├── mod.rs
│   │   │   ├── scanner.rs      # EXEC SQL extraction
│   │   │   ├── parser.rs       # SQL statement parsing
│   │   │   └── codegen.rs      # COBOL CALL generation
│   │   ├── translate/          # DB2 → PostgreSQL
│   │   │   ├── mod.rs
│   │   │   ├── syntax.rs       # SQL syntax transformation
│   │   │   ├── types.rs        # Data type mapping
│   │   │   └── functions.rs    # Built-in function mapping
│   │   ├── runtime/            # Execution support
│   │   │   ├── mod.rs
│   │   │   ├── connection.rs   # Connection pooling
│   │   │   ├── statement.rs    # Prepared statements
│   │   │   └── sqlca.rs        # SQLCA management
│   │   └── dclgen/             # DCLGEN utility
│   │       ├── mod.rs
│   │       └── generator.rs    # Copybook generation
│   └── Cargo.toml
│
├── zos-cics/                   # NEW: CICS subsystem
│   ├── src/
│   │   ├── lib.rs
│   │   ├── preprocess/         # CICS preprocessor
│   │   │   ├── mod.rs
│   │   │   ├── scanner.rs      # EXEC CICS extraction
│   │   │   ├── parser.rs       # Command parsing
│   │   │   └── codegen.rs      # Runtime CALL generation
│   │   ├── commands/           # Command implementations
│   │   │   ├── mod.rs
│   │   │   ├── program.rs      # LINK, XCTL, RETURN
│   │   │   ├── file.rs         # READ, WRITE, REWRITE, DELETE
│   │   │   ├── terminal.rs     # SEND, RECEIVE
│   │   │   └── storage.rs      # GETMAIN, FREEMAIN
│   │   ├── task/               # Task management
│   │   │   ├── mod.rs
│   │   │   ├── manager.rs      # Task lifecycle
│   │   │   └── eib.rs          # EIB management
│   │   └── exception/          # HANDLE CONDITION
│   │       ├── mod.rs
│   │       └── handler.rs
│   └── Cargo.toml
│
├── zos-bms/                    # NEW: BMS maps
│   ├── src/
│   │   ├── lib.rs
│   │   ├── compiler/           # BMS compiler
│   │   │   ├── mod.rs
│   │   │   ├── parser.rs       # DFHMSD/DFHMDI/DFHMDF
│   │   │   └── codegen.rs      # Copybook generation
│   │   └── render/             # Screen rendering
│   │       ├── mod.rs
│   │       ├── console.rs      # ANSI terminal
│   │       ├── html.rs         # Web browser
│   │       └── json.rs         # REST API
│   └── Cargo.toml
│
├── zos-migrate/                # NEW: Migration tools
│   ├── src/
│   │   ├── lib.rs
│   │   ├── scanner.rs          # Codebase discovery
│   │   ├── analyzer.rs         # Compatibility analysis
│   │   ├── report.rs           # Report generation
│   │   └── estimator.rs        # Effort estimation
│   └── Cargo.toml
│
├── zos-clone/                  # CLI (modified)
│   └── src/
│       └── commands/
│           ├── db2.rs          # NEW: db2 subcommand
│           ├── cics.rs         # NEW: cics subcommand
│           └── migrate.rs      # NEW: migrate subcommand
│
└── ... (existing crates unchanged)
```

---

## Integration Points

### COBOL Compilation Pipeline

```
┌───────────────┐     ┌───────────────┐     ┌───────────────┐
│ Source File   │────▶│ SQL Preproc   │────▶│ CICS Preproc  │
│ (.cbl)        │     │               │     │               │
└───────────────┘     └───────────────┘     └───────────────┘
                                                    │
                                                    ▼
┌───────────────┐     ┌───────────────┐     ┌───────────────┐
│ Executable    │◀────│ LLVM Codegen  │◀────│ COBOL Parser  │
│               │     │               │     │               │
└───────────────┘     └───────────────┘     └───────────────┘
```

### Runtime Library Linkage

```rust
// Generated COBOL calls these runtime functions
extern "C" {
    // DB2 runtime
    fn sqlexec(stmt: *const u8, params: ...) -> i32;
    fn sqlopen(cursor: *const u8) -> i32;
    fn sqlfetch(cursor: *const u8, into: ...) -> i32;
    fn sqlclose(cursor: *const u8) -> i32;

    // CICS runtime
    fn cicslink(program: *const u8, commarea: *mut u8) -> i32;
    fn cicsxctl(program: *const u8, commarea: *mut u8) -> i32;
    fn cicssend(map: *const u8, data: *const u8) -> i32;
    fn cicsrecv(map: *const u8, data: *mut u8) -> i32;
}
```

---

## Technology Choices

### DB2 Backend: PostgreSQL

**Rationale:**
- Open source, widely deployed
- SQL standard compliance
- Rich ecosystem (connection pooling, monitoring)
- Cloud-native options (RDS, Cloud SQL)

**Alternatives Considered:**
- SQLite: Too limited for enterprise workloads
- MySQL: Less SQL standard compliant
- CockroachDB: Adds complexity, overkill for most

### CICS Terminal: Multiple Modes

| Mode | Use Case | Technology |
|------|----------|------------|
| Console | Development/debugging | ANSI escape codes |
| HTML | Web-based access | Embedded HTTP server |
| REST | API integration | JSON serialization |

### Connection Pooling: deadpool-postgres

**Rationale:**
- Async-native (tokio)
- Well-maintained
- Simple configuration

---

## Security Considerations

### DB2 Security

- Connection credentials from environment or config file
- SSL/TLS for PostgreSQL connections
- No plaintext passwords in generated COBOL

### CICS Security

- Transaction authorization checks (future: RACF integration)
- Session timeout configuration
- Input validation on BMS fields

---

## Performance Targets

| Operation | Target | Measurement |
|-----------|--------|-------------|
| SQL SELECT (single row) | < 5ms | PostgreSQL query time |
| SQL SELECT (1000 rows) | < 100ms | Cursor fetch loop |
| CICS SEND MAP | < 50ms | Screen render time |
| CICS READ file | < 10ms | VSAM access time |
| BMS compile | < 1s | Mapset compilation |

---

## Testing Strategy

### DB2 Testing

1. **Unit tests:** SQL translation correctness
2. **Integration tests:** PostgreSQL execution
3. **Compatibility tests:** Output matching DB2

### CICS Testing

1. **Unit tests:** Command parsing
2. **Integration tests:** Transaction flows
3. **Screen tests:** BMS rendering verification

### Migration Tool Testing

1. **Scanner tests:** File discovery accuracy
2. **Analyzer tests:** Feature detection
3. **Report tests:** Output format validation
