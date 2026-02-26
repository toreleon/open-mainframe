# open-mainframe-parmlib

z/OS PARMLIB — a core implementation of the system initialization configuration, symbol substitution engine, and operator command processing for the OpenMainframe project.

## Overview

PARMLIB (Parameter Library) is the central configuration repository for z/OS. This crate reimplements the system initialization logic, providing parsers for core PARMLIB members, a robust system symbol substitution engine, and the orchestration of the system boot sequence.

## Architecture

```
    PARMLIB Members                        System Initialization
    ┌──────────────┐                      ┌────────────────────┐
    │ IEASYSxx     │    Parsing           │    Init Sequence   │
    │ LOADxx       │ ──────────────────>  │    (Boot Process)  │
    └──────────────┘    ParserRegistry    │  Subsystems, IPL   │
           │                               └────────────────────┘
           ▼                                        │
    ┌──────────────┐    Symbol Engine     ┌────────────────────┐
    │  IEASYMxx    │ ──────────────────>  │   Symbol Sub       │
    │  Symbols     │    SymbolEngine      │   &SYSNAME.        │
    └──────────────┘                      └────────────────────┘
                                                    │
                                                    ▼
    ┌──────────────┐    Management        ┌────────────────────┐
    │  Operator    │ <──────────────────  │   PARMLIB Concat   │
    │  Commands    │    InitSequence      │   SYS1.PARMLIB     │
    └──────────────┘                      └────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `members` | Framework for PARMLIB concatenation and parsers for core members (IEASYSxx, PROGxx, etc.) |
| `symbols` | System symbol definition (IEASYMxx) and `&symbol.` substitution engine |
| `subsystem`| Configuration for subsystems: IKJTSOxx (TSO), ALLOCxx (Allocation), etc. |
| `operator` | System initialization orchestration and configuration commands (SET, SETPROG) |

## Key Types and Components

### PARMLIB Management
- `ParmlibConcat`: Manages the search order for PARMLIB members across multiple datasets.
- `ParserRegistry`: A central registry for member-specific parsers.
- `IeaSysConfig`: Representation of the system parameters defined in IEASYSxx.

### Symbol Engine
- `SymbolEngine`: Handles the resolution of static (`&SYSNAME.`, `&SYSPLEX.`) and user-defined symbols.
- `IeaSymConfig`: Parser for the IEASYMxx member which defines the system symbol table.

### Initialization
- `InitSequence`: Orchestrates the transition through initialization phases (NIP, Subsystem, Master).
- `SetCommand`: Implementation of the `SET` and `SETPROG` operator commands.

## Feature Coverage

| Feature | Category | Status |
|---------|----------|--------|
| Member Concat  | Storage  | Implemented |
| Symbol Sub     | System   | Implemented (Static and Dynamic) |
| IEASYSxx Parsing| Config   | Implemented (Core parameters) |
| PROGxx Parsing | Config   | Implemented (APF, LNKLST, EXIT) |
| IKJTSOxx Parsing| Config   | Implemented (TSO parameters) |
| System Boot    | Init     | Implemented (Phase-based sequence) |
| SET Commands   | Operator | Implemented |

## Usage Examples

### Resolving System Symbols

```rust
use open_mainframe_parmlib::symbols::SymbolEngine;

let mut engine = SymbolEngine::new();
engine.add_symbol("MYVAR", "VALUE");

let input = "DSN=SYS1.VAR.&MYVAR..DATA";
let output = engine.substitute(input).unwrap();
assert_eq!(output, "DSN=SYS1.VAR.VALUE.DATA");
```

### Parsing a PARMLIB Member

```rust
use open_mainframe_parmlib::members::{ParmlibConcat, IeaSysConfig};

let concat = ParmlibConcat::new(&["SYS1.PARMLIB"]);
let config: IeaSysConfig = concat.parse_member("IEASYS00").unwrap();
println!("System APF list: {}", config.apf_list);
```

## Testing

The PARMLIB crate includes 200+ tests:
- **Symbols**: Validates complex substitution cases, including nested symbols and concatenation rules.
- **Parsers**: Tests various formatting styles for IEASYSxx and PROGxx members.
- **Concat**: Ensures correct search order across multiple datasets.
- **Init**: Simulates the boot sequence and validates subsystem registration.

```sh
cargo test -p open-mainframe-parmlib
```
