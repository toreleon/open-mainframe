# OpenMainframe - Claude Code Memory

## What This Is
Rust monorepo implementing a z/OSMF-compatible REST API server with full z/OS subsystem
emulation. 41 crates covering COBOL compilation, CICS transaction processing, JCL
interpretation, VSAM datasets, RACF security, JES2 job scheduling, TSO, and more.

**Default endpoint**: `http://127.0.0.1:10443` (user: IBMUSER, password: SYS1)

## Critical Fact: On-Demand COBOL Compilation
`compile_program()` in `src/lib.rs` reads COBOL source **fresh from disk** every time
a program is loaded. There is **no compilation cache**. This means edits to `.cbl` files
take effect immediately on the next CICS program load (XCTL, LINK, RETURN TRANSID).

`find_program_source()` searches configured directories at runtime to locate programs.

## Repository Structure

```
OpenMainframe/
├── Cargo.toml                  # Workspace root (41 crate members)
├── Cargo.lock                  # Dependency lock
├── rust-toolchain.toml         # Requires Rust 1.82
├── zosmf.toml                  # z/OSMF server configuration
├── Makefile                    # Build targets
├── src/lib.rs                  # open_mainframe_lib (compile_program, setup_env, BridgeHandler)
├── src/main.rs                 # CLI entry (CICS TUI + headless modes)
├── crates/                     # 39 library crates (see below)
├── .claude/commands/           # Claude project management templates
├── .github/                    # CI workflows
├── data/                       # Test data files
├── deploy/                     # Docker, K8s configs
├── examples/                   # Example programs
├── packaging/                  # Distribution packaging
├── scripts/                    # Utility scripts
└── tests/                      # Integration tests (e2e_datasets, e2e_jobs, etc.)
```

## All 41 Crates

### Foundation
| Crate | Purpose |
|-------|---------|
| `open-mainframe-lang-core` | Shared AST, spans, diagnostics, error types |
| `open-mainframe-encoding` | EBCDIC encoding (21 code pages), decimal arithmetic |
| `open-mainframe-runtime` | Language Environment runtime for COBOL execution |

### Compilers & Languages
| Crate | Purpose |
|-------|---------|
| `open-mainframe-cobol` | Full COBOL compiler (8-pass: options→conditional→copy→replace→lex→parse→semantic→codegen) |
| `open-mainframe-jcl` | JCL interpreter (parsing, PROC expansion, execution) |
| `open-mainframe-rexx` | REXX language interpreter |
| `open-mainframe-hlasm` | HLASM assembler (lexer, parser, macro engine, object code) |
| `open-mainframe-pli` | PL/I parser/interpreter |
| `open-mainframe-clist` | CLIST scripting interpreter |
| `open-mainframe-easytrieve` | Easytrieve Plus (lexer, parser, interpreter, reports) |
| `open-mainframe-natural` | Software AG Natural 4GL |
| `open-mainframe-focus` | Information Builders FOCUS 4GL |
| `open-mainframe-precompilers` | COBOL precompilers (DB2, CICS source transformation) |

### Data & Storage
| Crate | Purpose |
|-------|---------|
| `open-mainframe-dataset` | Dataset I/O: QSAM, BSAM, PDS/PDSE, VSAM (KSDS/ESDS/RRDS), ICF catalog, GDG |
| `open-mainframe-db2` | DB2 SQL preprocessing and runtime (EXEC SQL) |
| `open-mainframe-ims` | IMS hierarchical database (DL/I) |
| `open-mainframe-idms` | IDMS CODASYL network database |
| `open-mainframe-adabas` | ADABAS inverted-list database |
| `open-mainframe-sort` | DFSORT-compatible sort/merge/copy utility |

### Subsystems
| Crate | Purpose |
|-------|---------|
| `open-mainframe-cics` | CICS transaction processor (BMS, EXEC CICS, queues, terminal I/O) |
| `open-mainframe-jes2` | JES2 job entry subsystem (queue, spool, initiators) |
| `open-mainframe-racf` | RACF security (users, groups, resource protection, SAF) |
| `open-mainframe-tso` | TSO/E command processor |
| `open-mainframe-ispf` | ISPF panels, tables, editor, dialog services |
| `open-mainframe-mq` | IBM MQ queue manager (MQI, MQSC) |

### System Services
| Crate | Purpose |
|-------|---------|
| `open-mainframe-mvs` | MVS system services (SVCs, DYNALLOC, WTO, ENQ, ESTAE) |
| `open-mainframe-wlm` | Workload Manager |
| `open-mainframe-smf` | System Management Facilities |
| `open-mainframe-uss` | Unix System Services (POSIX) |
| `open-mainframe-utilities` | z/OS utilities (IEBCOPY, IEBGENER, IEBCOMPR, IEBUPDTE, etc.) |
| `open-mainframe-syscmd` | System console commands (DISPLAY, START, STOP) |
| `open-mainframe-pgmmgmt` | Program Management (Binder, Loader) |
| `open-mainframe-networking` | VTAM, SNA, TCP/IP, FTP, SSH |
| `open-mainframe-crypto` | ICSF cryptographic services |
| `open-mainframe-parmlib` | z/OS PARMLIB system initialization config |

### API & Deployment
| Crate | Purpose |
|-------|---------|
| `open-mainframe-zosmf` | z/OSMF REST API server (Axum, 14 handler modules, 56+ endpoints) |
| `open-mainframe-assess` | Code complexity analysis and migration assessment |
| `open-mainframe-deploy` | Container deployment (K8s, Docker, Helm) |
| `open-mainframe-tui` | Interactive 3270 terminal UI |

### Binary
| Crate | Purpose |
|-------|---------|
| `open-mainframe` (bin) | CLI entry point (CICS TUI + headless modes) |

## z/OSMF REST Server (`open-mainframe-zosmf`)

### Handler Modules (14)
| Module | Base Path | Purpose |
|--------|-----------|---------|
| `info.rs` | `/zosmf/info` | Server information |
| `authenticate.rs` | `/zosmf/services/authenticate` | JWT + Basic Auth |
| `datasets.rs` | `/zosmf/restfiles/ds/*` | Dataset CRUD, PDS members |
| `jobs.rs` | `/zosmf/restjobs/jobs/*` | JCL submission, status, spool |
| `tso.rs` | `/zosmf/tsoApp/tso*` | TSO command execution |
| `console.rs` | `/zosmf/restconsoles/consoles/*` | MVS console commands |
| `files.rs` | `/zosmf/restfiles/fs/*` | USS file operations |
| `cics.rs` | `/zosmf/cicsApp/terminal` | CICS session management |
| `wlm.rs` | WLM paths | Workload management |
| `variables.rs` | Variables paths | System variables |
| `topology.rs` | Topology paths | System topology |
| `workflow.rs` | Workflow paths | Workflow provisioning |
| `provisioning.rs` | Provisioning paths | Resource provisioning |

### CICS REST Endpoints
| Method | Path | Purpose |
|--------|------|---------|
| POST | `/zosmf/cicsApp/terminal` | Start new CICS session (returns initial screen) |
| PUT | `/zosmf/cicsApp/terminal/{sessionKey}` | Send input (AID key + fields, returns next screen) |
| GET | `/zosmf/cicsApp/terminal/{sessionKey}` | Read current screen |
| DELETE | `/zosmf/cicsApp/terminal/{sessionKey}` | Terminate session |

### CICS Session Architecture
Each session gets a dedicated OS thread (because `CicsBridge` uses `Rc<RefCell<>>`):
```
HTTP Handler (async)  ──mpsc──>  Session Thread (sync)
                      <─oneshot─
```
- **Key file**: `crates/open-mainframe-zosmf/src/cics_runner.rs` — `CicsSessionRunner`
- Uses `SessionCommand`/`SessionResponse` enums over tokio channels
- `DashMap<String, CicsSession>` in `AppState` for concurrent session storage

### AppState
- `DashMap` session stores for CICS, TSO, console
- RACF, JES2, catalog subsystems (in-memory)
- Mount table maps local filesystem → PDS/USS virtual paths

## CICS Execution Engine (`open-mainframe-cics`)

### Key Components
```
src/
├── bms/                    # BMS map processing
│   ├── parser.rs           # DFHMSD/DFHMDI/DFHMDF parser
│   ├── render.rs           # 3270 data stream renderer (SBA/SF/SFE)
│   └── symbolic.rs         # COBOL copybook generator
├── runtime/
│   ├── commands.rs         # 30+ CICS operation implementations
│   ├── dispatcher.rs       # CALL-to-runtime bridge
│   ├── eib.rs              # EXEC Interface Block
│   └── files.rs            # File control
├── preprocess/
│   ├── mod.rs              # EXEC CICS → CALL transformation
│   └── scanner.rs          # EXEC CICS block scanner
├── queues/                 # TS and TD queues
├── terminal/               # Terminal manager + screen state
├── channels.rs             # Channel/Container data passing
├── interval.rs             # DELAY, START, CANCEL
└── web.rs                  # HTTP/REST services
```

### CICS Command Flow
1. `CicsPreprocessor` converts `EXEC CICS...END-EXEC` → `CALL "CICSxxx"` with parameter block
2. `CicsDispatcher` maps CALL names to `CicsRuntime` method calls
3. 30+ commands: LINK, XCTL, RETURN, SEND MAP, RECEIVE MAP, READ, WRITE, REWRITE, DELETE, STARTBR, READNEXT, etc.

### Key CICS Implementation Details

**EIBRESP error propagation**: File operations in `FileManager` (files.rs) return
`Err(CicsError::FileError(CicsResponse::Xxx))` but do NOT set `eib.eibresp`
themselves. The bridge (`bridge.rs`) must call `set_error_response(&err)` to
extract the response code and set it on the EIB before the outer `execute()`
wrapper copies it to the COBOL RESP variable.

**BMS input/output map REDEFINES**: BMS copybooks define two 01-level groups:
`xxxI` (input) and `xxxO REDEFINES xxxI` (output). In real COBOL they share
memory, so setting `USRID01I` (input map) automatically sets `USRID01O` (output
map) at the same offset. Our interpreter uses a flat variable namespace, so
`USRID01I` and `USRID01O` are separate variables. The `collect_send_map_data()`
method in `bridge.rs` handles this by checking both O and I suffixes when
resolving BMS field values for SEND MAP, preferring whichever is non-blank.

**Group variable decomposition**: After CICS READ/READNEXT/READPREV sets a group
variable (e.g., `SEC-USER-DATA`), the interpreter automatically decomposes it
into sub-fields using `decompose_group()` based on `program.group_layouts`
(built during the AST lowering pass in `lower.rs`). This is critical for browse
operations where READNEXT fills a record buffer that must be split into
individual fields.

## COBOL Compiler (`open-mainframe-cobol`)

### 8-Pass Pipeline
```
Source → Pass 0 (options) → Pass 1 (>>IF) → Pass 2 (COPY) → Pass 3 (REPLACE)
       → Lexer → Parser → Semantic Analysis → Codegen (LLVM, optional)
```

### Key Features
- 77+ intrinsic functions (numeric, string, datetime)
- JSON/XML GENERATE/PARSE (COBOL-2014)
- IBM Enterprise COBOL extensions
- Fixed and free format source
- 20,500 lines of compiler code

## Configuration (zosmf.toml)

```toml
[server]
host = "0.0.0.0"
port = 10443

[auth]
token_ttl_seconds = 28800
token_secret = "change-me-in-production"

[cics]
default_app = "CARDDEMO"
session_timeout_seconds = 1800

[cics.apps.CARDDEMO]
program = "../carddemo/app/cbl/COSGN00C.cbl"
include_paths = ["../carddemo/app/cpy", "../carddemo/app/cpy-bms"]
bms_dir = "../carddemo/app/bms"
program_dir = "../carddemo/app/cbl"
data_files = [
    "ACCTDAT=../carddemo/app/data/ASCII/acctdata.txt:11:300",
    # ... (format: DDNAME=path:key_len:record_count)
]

[cics.apps.CARDDEMO.transids]
CC00 = "COSGN00C"
# ... (transid → program name mappings)
```

## Build & Run

```bash
# Build all
cargo build --release

# Run z/OSMF server
cargo run --release -p open-mainframe-zosmf --bin zosmf-server
# or with config: cargo run --release -p open-mainframe-zosmf --bin zosmf-server -- --config zosmf.toml

# Run CLI (CICS TUI)
cargo run --release

# Tests
cargo test --workspace
cargo clippy -- -D warnings
cargo fmt --check
```

## Technology
- **Rust 1.82** (enforced via rust-toolchain.toml)
- **Async**: tokio 1 (multi-threaded runtime)
- **Web**: Axum 0.8, tower, tower-http
- **Auth**: base64, hmac, sha2 (JWT)
- **Data**: serde, serde_json, toml
- **Concurrency**: DashMap (concurrent HashMap)
- **Errors**: miette (diagnostic error reporting)
- **LLVM**: inkwell (optional, behind feature flag)
- **Memory**: bumpalo (arena allocation)
- **Decimal**: rust_decimal (COBOL COMP-3/COMP-2)

## Git
- Remote: `git@github.com:toreleon/OpenMainframe.git`
- Branch: `master`
