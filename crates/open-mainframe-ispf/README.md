# open-mainframe-ispf

A high-fidelity Rust implementation of the **ISPF (Interactive System Productivity Facility)** for the OpenMainframe project — providing the full mainframe development environment including panel-driven applications, dialog services, variable pools, table services, file tailoring, and the iconic Line Editor.

## Overview

ISPF is the primary interface for mainframe developers, often referred to as the "mainframe's IDE." This crate reimplements the core ISPF components, enabling the execution of ISPF-based applications via a text-based user interface or REST API. It handles the parsing of complex panel definitions, management of hierarchical variable pools, and provides the essential services used by dialog-driven programs.

The implementation comprises:
1. **Panel Engine** — A parser and interpreter for ISPF panel definitions, supporting sections like `)ATTR`, `)BODY`, `)INIT`, `)REINIT`, and `)PROC`.
2. **Dialog Manager** — Implementation of core dialog services including `DISPLAY`, `SELECT`, `SETMSG`, and `CONTROL`.
3. **Variable Pools** — A comprehensive four-pool variable model (Function, Shared, Profile, and System) with inheritance and persistence.
4. **Table Services** — In-memory relational tables with row pointers, search arguments, and persistent storage.
5. **File Tailoring** — Skeleton processing engine with variable substitution and control statements (`)SEL`, `)DOT`, `)IM`).
6. **Library Management** — Services for dataset and member access, including member lists and statistics.
7. **Line Editor** — A robust implementation of the ISPF editor supporting primary and line commands, profiles, and undo/redo.

## Architecture

```
    User Terminal / TUI
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  Dialog Manager                        │
    │  - Service dispatcher (DISPLAY, SELECT)                │
    │  - Message processing                                  │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  Panel Engine                          │
    │  - Section-based parsing and rendering                 │
    │  - Field validation (VER checks)                       │
    │  - Attribute character mapping                         │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  Variable Pools                        │
    │  - Func / Shared / Profile / System pools              │
    │  - Variable substitution engine                        │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                 Data & Tailoring Services              │
    │  - ISPF Tables (TBADD, TBSCAN)                         │
    │  - File Tailoring (FTOPEN, FTINCL)                     │
    │  - Library Management (LMINIT, LMOPEN)                 │
    └────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Description | Lines |
|--------|-------------|------:|
| `dialog` | Dialog services: DISPLAY, SELECT, SETMSG, and variable pool management | ~1,346 |
| `editor` | Line editor engine: Support for primary and line commands, and profiles | ~1,241 |
| `panel` | Panel definition parser and interpreter: Sections, attributes, and fields | ~1,143 |
| `utilities`| Common utilities: DSLIST, Search-For, SuperC, and Member List | ~1,147 |
| `isredit` | ISREDIT macro engine and editor service interface | ~1,010 |
| `library` | Library management (LMINIT, LMOPEN) and member access services | ~986 |
| `table` | Table services: In-memory tables with search, sort, and persistence | ~775 |
| `skeleton`| File tailoring: Template processing with control statements | ~632 |

**Total**: ~8,330 lines of Rust.

## Key Types and Components

### Dialog & Panels

| Type | Description |
|------|-------------|
| `DialogManager`| Orchestrates the execution of dialog services and display events. |
| `Panel` | Parsed representation of an ISPF panel with all its sections. |
| `IspfVarPools` | Manages the four-tier hierarchy of variable storage. |
| `MessageDef` | Representation of an ISPF message (e.g., ISR00001). |

### Data Management

| Type | Description |
|------|-------------|
| `IspfTable` | An in-memory relational table with searching and sorting capabilities. |
| `LibraryManager`| Facilitates PDS and dataset access via LM services. |
| `FileTailor` | Processes skeletons into final tailored files. |

### Editor

| Type | Description |
|------|-------------|
| `Editor` | The core engine for the ISPF line-based text editor. |
| `EditProfile` | Maintains editor settings (CAPS, HEX, NUMBER, etc.). |
| `LineCommand` | Represents line-area commands (I, D, C, M, R). |

## Implementation Details

### Panel Parsing and Section Logic

Panel definitions are parsed into a structured model where sections are treated as procedural code:
- **`)INIT`**: Executed before the panel is first displayed to set initial values.
- **`)REINIT`**: Executed when a panel is redisplayed after a validation error.
- **`)PROC`**: Executed after user input to perform validation (`VER` checks) and processing.

### The Four-Pool Variable Model

Variables are resolved in a specific order:
1. **Function Pool**: Local to the current dialog.
2. **Shared Pool**: Shared between nested dialogs.
3. **Profile Pool**: Persists across user sessions (stored in JSON).
4. **System Pool**: Read-only system-defined variables (ZCMD, ZUSER, etc.).

### ISPF Table Services

Tables in ISPF are unique in that they combine relational storage with cursor-based navigation. This crate implements:
- **TBSCAN**: High-performance searching using condition blocks.
- **TBSORT**: Sorting based on one or more column keys.
- **TBTOP / TBBOTTOM**: Row pointer management.

## Feature Coverage

| Feature | Category | Status |
|---------|----------|--------|
| Panel Sections| Panels   | Implemented (BODY, ATTR, INIT, PROC, MODEL) |
| VER checks    | Panels   | Implemented (ALPHA, NUM, DSNAME, PICT, etc.)|
| Variable Pools| Dialog   | Implemented (VGET, VPUT, VERASE) |
| Dialog Svcs   | Dialog   | Implemented (DISPLAY, SELECT, SETMSG, CONTROL)|
| Table Svcs    | Tables   | Implemented (TBADD, TBPUT, TBMOD, TBSCAN, TBSORT)|
| File Tailoring| Tailoring| Implemented (FTOPEN, FTINCL, FTCLOSE) |
| Line Editor   | Editor   | Implemented (FIND, CHANGE, I, D, C, M, R) |
| Editor Macros | Editor   | Implemented (ISREDIT interface) |
| DSLIST / Util | Utilities| Implemented (DSLIST, Member List, SuperC) |

## Usage Examples

### Displaying an ISPF Panel

```rust
use open_mainframe_ispf::{DialogManager, Panel, DialogEvent};

let mut dm = DialogManager::new();
let panel_src = ")BODY\n%Command ===>_ZCMD\n%\n% User: _USER+\n)END";
let panel = Panel::parse(panel_src).unwrap();

// Set a variable in the pool
dm.set_var("USER", "IBMUSER");

// Display the panel (in a TUI environment)
if let DialogEvent::Enter = dm.display(&panel).unwrap() {
    let cmd = dm.get_var("ZCMD").unwrap();
    println!("User entered command: {}", cmd);
}
```

### Working with ISPF Tables

```rust
use open_mainframe_ispf::table::TableManager;

let mut tm = TableManager::new();
tm.tbcreate("MYTABLE", &["KEY"], &["VALUE"]).unwrap();

tm.set_var("KEY", "001");
tm.set_var("VALUE", "TEST DATA");
tm.tbadd("MYTABLE").unwrap();

// Scan for a record
tm.set_var("KEY", "001");
if tm.tbscan("MYTABLE", "KEY").is_ok() {
    println!("Found record!");
}
```

## Testing

The ISPF crate features over 400 tests:
- **Panel Tests**: Validates parsing of complex attribute definitions and validation rules.
- **Variable Tests**: Verifies pool isolation and persistence logic.
- **Table Tests**: Exhaustive testing of sort and scan edge cases.
- **Editor Tests**: Command round-trips ensuring buffer consistency after multiple edits.

```sh
cargo test -p open-mainframe-ispf
```

## Limitations and Future Work

- **DBCS Panels**: Support for Double-Byte Character Set rendering in panels is partially implemented.
- **Full Screen Macros**: Some complex ISREDIT macro interactions are still being refined.
- **Persistence**: While Profile pools persist, the current implementation uses a flat JSON file; a dataset-backed model is in design.
- **Graphics**: ISPF GDDM integration for graphical displays is not implemented.
