# open-mainframe-natural

Software AG Natural 4GL — a high-fidelity Rust implementation of the Natural language, interpreter, and environment for the OpenMainframe project.

## Overview

Natural is a powerful 4GL used for developing large-scale business applications, typically in conjunction with the ADABAS database. This crate reimplements the core Natural components, including the compiler/interpreter, terminal map handling, reporting engine, and database access layer for both ADABAS and SQL.

## Architecture

```
    Natural Source                        Execution Environment
    ┌──────────────┐                      ┌────────────────────┐
    │ DEFINE DATA  │    Parsing           │    Interpreter     │
    │ FIND...      │ ──────────────────>  │    (Stack-based)   │
    │ DISPLAY...   │    NaturalParser     │  Control, Vars     │
    └──────────────┘                      └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Data Access       ┌────────────────────┐
    │  DDM / Schema│ ──────────────────>  │   Database Layer   │
    │  (ADABAS/SQL)│    DataModel         │  Find, Read, Store │
    └──────────────┘                      └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Terminal I/O      ┌────────────────────┐
    │  Maps / PF   │ <──────────────────  │    Output Engine   │
    │  INPUT       │    MapManager        │  Report Format     │
    └──────────────┘                      └────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `parser` | Lexer and parser for Natural source code (NAT-100) |
| `data_model` | Type system (11 types), arrays, groups, and variable management (NAT-101) |
| `interpreter`| Stack-based execution engine with control flow (NAT-102) |
| `manipulation`| Data manipulation: COMPUTE, MOVE, COMPRESS, SEPARATE, EXAMINE (NAT-103) |
| `adabas_access`| ADABAS DDM-based retrieval and update (simulated) (NAT-104) |
| `sql_access` | SQL retrieval (SELECT) and modification (NAT-105) |
| `output` | Reporting engine: DISPLAY, WRITE, PRINT, page formatting (NAT-106) |
| `maps` | Interactive maps, INPUT statements, and PF key handling (NAT-107) |
| `functions` | Built-in function library (character, date, numeric) (NAT-108) |
| `sysvars` | Implementation of 70+ system variables (*TIME, *USER, etc.) (NAT-108) |
| `workfiles` | Work file I/O (READ/WRITE WORK) and error handling (NAT-109) |
| `environment`| Library management, security, and RPC integration (NAT-110) |

## Key Types and Components

### Data Model
- `NaturalType`: Enumeration of the 11 Natural types (A, N, P, I, F, B, D, T, L, C, U).
- `NaturalValue`: Variant type for runtime values supporting arithmetic and conversion.
- `VariablePool`: Manages global, local, and parameter variables.

### Interpreter
- `NaturalInterpreter`: Orchestrates the execution of a compiled Natural program.
- `NaturalObject`: The executable representation of a Program, Subprogram, or Subroutine.

### Reporting & I/O
- `ReportEngine`: Handles report layout, control breaks, and page headers/footers.
- `MapDefinition`: Defines the layout of an interactive input/output screen.

## Feature Coverage

| Feature | Category | Status |
|---------|----------|--------|
| COMPUTE / MOVE | Manipulation | Implemented |
| FIND / READ | Data Access | Implemented (with DDM support) |
| DISPLAY / WRITE| Reporting    | Implemented (Control breaks, headers) |
| INPUT USING MAP| Terminal I/O | Implemented |
| DEFINE DATA    | Data Model   | Implemented (Global, Local, Parameter) |
| ON ERROR       | Error Handling| Implemented |
| System Vars    | Environment  | Implemented (70+ variables) |

## Usage Examples

### Running a Simple Natural Program

```rust
use open_mainframe_natural::{NaturalInterpreter, parse_natural};

let source = r#"
DEFINE DATA LOCAL
1 #NAME (A20)
1 #AGE (I2)
END-DEFINE
#NAME := 'JONES'
#AGE := 35
DISPLAY 'PERSON:' #NAME 'AGE:' #AGE
END
"#;

let program = parse_natural(source).unwrap();
let mut interpreter = NaturalInterpreter::new();
interpreter.execute(program).unwrap();
```

### Accessing ADABAS Data

```rust
use open_mainframe_natural::adabas_access::Ddm;

let dm_source = r#"
1 CUSTOMER-FILE
  2 AA NAME (A)
  2 AB CITY (A)
"#;
let ddm = Ddm::parse(dm_source).unwrap();
// Use DDM with FIND/READ statements in the interpreter
```

## Testing

The Natural crate includes 300+ tests:
- **Parser**: Verification of all syntax variants and keyword rules.
- **Data Model**: Arithmetic precision and overflow tests for Packed (P) and Numeric (N) types.
- **Reporting**: Complex report layout tests including control breaks and summaries.
- **Manipulation**: String manipulation tests for COMPRESS and SEPARATE.

```sh
cargo test -p open-mainframe-natural
```
