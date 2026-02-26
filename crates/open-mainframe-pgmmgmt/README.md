# open-mainframe-pgmmgmt

z/OS Program Management — a high-performance Rust implementation of the mainframe's Binder, Loader, and Object Module infrastructure for the OpenMainframe project.

## Overview

Program Management is the subsystem responsible for preparing and bringing programs into execution. This crate reimplements the core z/OS Binder and Loader services, supporting symbol resolution, relocation, and the loading of programs from various libraries (STEPLIB, LNKLST, LPA) into virtual storage.

## Architecture

```
    Object / Load Modules                 Program Management
    ┌────────────────────┐                ┌────────────────────────┐
    │  Object Module     │    Binding     │    Binder Engine       │
    │  (ESD, TXT, RLD)   │ ─────────────> │    Relocation, Res     │
    └────────────────────┘    Binder      │  Load Module Prod      │
                                          └────────────────────────┘
                                                       │
                                                       ▼
    ┌────────────────────┐                ┌────────────────────────┐
    │  Program Library   │    Loading     │    Program Loader      │
    │  (PDS / PDSE)      │ ─────────────> │    LOAD, DELETE        │
    └────────────────────┘    Loader      │  Virtual Storage       │
                                          └────────────────────────┘
                                                       │
                                                       ▼
    ┌────────────────────┐                ┌────────────────────────┐
    │  Task Execution    │    Invocation  │    Program Manager     │
    │  TCB context       │ <───────────── │    LINK, XCTL, ATTACH  │
    └────────────────────┘    ProgramMgr  └────────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `binder` | Symbol resolution, relocation dictionary (RLD) processing, and load module production |
| `objmod` | Parsers for standard Object (OBJ) and Load Module (LMOD) binary formats |
| `program`| Program invocation (LINK, XCTL), subtask management (ATTACH), and library search |

## Key Types and Components

### Binder
- `Binder`: The central engine for linking object modules.
- `EsdEntry`: External Symbol Dictionary entry (CSECT, ENTRY, EXTRN).
- `ResolvedSymbol`: Represents a symbol that has been mapped to a physical address.

### Loader
- `ProgramManager`: Orchestrates the loading and search process.
- `LoadedProgram`: Represents a program module currently resident in memory.
- `ProgramLibrary`: Trait for accessing modules from different storage types (e.g., PDS).

### Execution Context
- `Amode`: Addressing Mode (24, 31, or 64-bit).
- `Rmode`: Residency Mode (24, 31, or 64-bit).
- `ApfList`: Management of Authorized Program Facility status.

## Feature Coverage

| Feature | Category | Status |
|---------|----------|--------|
| OBJ Parsing     | Metadata | Implemented |
| Symbol Resoluton| Binding  | Implemented (Static and Dynamic) |
| Relocation      | Binding  | Implemented (Adcons: V, A, Y, R) |
| Search Hierarchy| Search   | Implemented (STEPLIB, JOBLIB, LNKLST) |
| LOAD / DELETE   | Loading  | Implemented |
| LINK / XCTL     | Invocation| Implemented |
| ATTACH / DETACH | Invocation| Implemented |
| APF Validation  | Security | Implemented |

## Usage Examples

### Loading and Executing a Program

```rust
use open_mainframe_pgmmgmt::program::ProgramManager;

let mut mgr = ProgramManager::new();
mgr.add_library("MY.LOADLIB");

// Load a program module
let program = mgr.load("MYPROG").expect("Program not found");

// Execute the program (simplified)
let rc = program.execute().unwrap();
println!("Program returned: {}", rc);
```

### Using the Binder to Link Modules

```rust
use open_mainframe_pgmmgmt::binder::Binder;

let mut binder = Binder::new();
// ... add object modules ...
let load_module = binder.link("MAIN").unwrap();
```

## Testing

The Program Management crate includes 300+ tests:
- **Binder**: Validates complex relocation scenarios across multiple modules.
- **Search**: Ensures the correct search order is maintained for all library types.
- **Formats**: Tests parsing of malformed or legacy object records.
- **Loader**: Verifies correct AMODE/RMODE handling and memory cleanup.

```sh
cargo test -p open-mainframe-pgmmgmt
```
