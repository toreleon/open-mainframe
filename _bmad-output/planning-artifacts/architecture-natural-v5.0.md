---
version: 'v5.0'
planningGroup: 'PG-29'
technology: 'Natural'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-natural-v5.0.md'
---

# Architecture: Natural

## 1. Crate Strategy

**New crate:** `open-mainframe-natural`

## 2. Module Layout

```
crates/open-mainframe-natural/src/
├── lib.rs              # Crate root
├── parser/
│   ├── mod.rs          # Parser entry
│   ├── lexer.rs        # Tokenizer (130+ keywords)
│   ├── ast.rs          # AST nodes
│   └── data_area.rs    # DEFINE DATA parser
├── interpreter/
│   ├── mod.rs          # Statement dispatch
│   ├── variables.rs    # 11 data types, arrays, dynamic
│   ├── control.rs      # IF/DECIDE/FOR/REPEAT/PERFORM/ESCAPE
│   ├── compute.rs      # COMPUTE, MOVE, COMPRESS, SEPARATE, EXAMINE
│   └── stack.rs        # STACK, FETCH, CALLNAT
├── database/
│   ├── mod.rs          # Database access dispatch
│   ├── adabas.rs       # ADABAS DML (DDM, READ, FIND, etc.)
│   └── sql.rs          # SQL DML
├── output/
│   ├── mod.rs          # Output engine
│   ├── display.rs      # DISPLAY, WRITE, PRINT
│   └── report.rs       # Control breaks, headers, page formatting
├── maps/
│   ├── mod.rs          # Map processing
│   └── terminal.rs     # INPUT, REINPUT, PF key handling
├── functions.rs        # 25+ built-in functions
├── sysvars.rs          # 70+ system variables
├── workfiles.rs        # Work file I/O
├── security.rs         # Natural Security
├── environment.rs      # LOGON, CATALOG, STOW, libraries
└── error.rs            # Error types, ON ERROR
```

## 3. Design Decisions

### DD-5.0-NAT-01: ADABAS Integration via Crate Dependency
**Decision:** Natural DML statements (READ, FIND, STORE, etc.) are translated to ADABAS ACB calls via the `open-mainframe-adabas` crate. DDM definitions map Natural field names to ADABAS FDT fields.

### DD-5.0-NAT-02: Object Library as Directory
**Decision:** Natural libraries (FUSER, FNAT) are implemented as directories. Each Natural object (Program, Subprogram, Map, etc.) is a file within the library directory, enabling standard file-system management.

### DD-5.0-NAT-03: Map Objects as Data Structures
**Decision:** Natural Maps (3270 screen definitions) are represented as data structures mapping field positions to Natural variables. Runtime MAP processing converts between 3270 data streams and Natural variable pools.
