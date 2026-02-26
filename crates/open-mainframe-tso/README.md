# open-mainframe-tso

A comprehensive Rust implementation of **TSO/E (Time Sharing Option/Extensions)** for the OpenMainframe z/OS clone — providing interactive command processing, dataset management, session profile control, and a bridge for REXX and CLIST execution.

## Overview

TSO is the primary interactive interface for mainframe users and developers. This crate reimplements the core TSO/E environment, enabling the execution of TSO commands via a line-oriented interface. It handles command parsing, dynamic dataset allocation, and provides the essential service routines (IKJPARS, PUTLINE, GETLINE) used by TSO command processors.

The implementation comprises:
1. **Command Processor** — A central dispatcher that routes user input to the appropriate command handler (e.g., `ALLOCATE`, `LISTDS`).
2. **IKJPARS Service** — A high-fidelity implementation of the TSO parameter parsing service, supporting keyword and positional parameters.
3. **Session Management** — Maintenance of user-specific state, including the PROFILE (PREFIX, MSGID) and active dataset allocations.
4. **Dataset Management** — Direct integration with MVS `DYNALLOC` for managing datasets and PDS members from the command line.
5. **JES2 Integration** — Interaction with the batch subsystem via `SUBMIT`, `STATUS`, `CANCEL`, and `OUTPUT` commands.
6. **REXX & CLIST Bridge** — Hosting environment for executing scripts that interact with TSO services.

## Architecture

```
    User Terminal / REST API
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  TSO Command Dispatcher                │
    │  - Command line parsing                                │
    │  - Alias resolution                                    │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  TSO Service Routines                  │
    │  - IKJPARS (Parameter Parsing)                         │
    │  - PUTLINE / GETLINE (Terminal I/O)                    │
    │  - STACK (Command Stacking)                            │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  Session & Profile                     │
    │  - User PREFIX, MSGID settings                         │
    │  - Active DD Table (Allocations)                       │
    │  - ALTLIB (Alternative Libraries)                      │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                 Subsystem Connectors                   │
    │  - MVS DynAlloc (SVC 99)                               │
    │  - JES2 Interface (SUBMIT/STATUS)                      │
    │  - REXX / CLIST Interpreters                           │
    └────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Description | Lines |
|--------|-------------|------:|
| `commands` | Implementation of core TSO commands: ALLOCATE, FREE, LISTDS, LISTALC, etc. | ~891 |
| `jobs` | JES2 interaction: SUBMIT, STATUS, CANCEL, and OUTPUT processing | ~592 |
| `session` | Session state: User profile, active allocations, and session lifecycle | ~416 |
| `rexx_tso` | TSO host command environment for REXX (ADDRESS TSO) | ~487 |
| `exec` | Script execution: Support for CLIST and REXX invocation | ~446 |
| `parser` | Command line tokenizer and keyword/positional extractor | ~320 |
| `services` | TSO service routines: Implementation of IKJPARS and TSO I/O | ~323 |

**Total**: ~3,554 lines of Rust.

## Key Types and Components

### Session & Environment

| Type | Description |
|------|-------------|
| `TsoSession` | Manages the state of an interactive user session. |
| `TsoProfile` | Contains user settings like `PREFIX`, `WTPMSG`, and `MSGID`. |
| `AllocEntry` | Represents a single active dataset or DD allocation. |

### Command Processing

| Type | Description |
|------|-------------|
| `ParsedCommand`| Structured representation of a TSO command string. |
| `CommandResult`| Captures the outcome of a command, including return code and output. |
| `TsoIo` | Trait for terminal I/O, allowing TSO to run over SSH or REST. |

### Service Routines

| Type | Description |
|------|-------------|
| `ParseDescriptorList (PDL)`| Definition of expected parameters for `IKJPARS`. |
| `MemoryIo` | A TSO I/O implementation that buffers output in memory. |

## Implementation Details

### IKJPARS Parameter Parsing

The `IKJPARS` service is the heart of TSO command flexibility. This crate implements a Rust-native version that:
- Supports **Abbreviations**: e.g., `ALLOC` for `ALLOCATE`.
- Handles **Keyword Values**: e.g., `DSN('MY.DATA')`.
- Handles **Positional Parameters**: Ensuring required operands are present.
- Provides **Help Prompts**: If a required parameter is missing, the service can prompt the user (via `GETLINE`).

### TSO Command Dispatch

Commands are registered in a central registry. When a user enters a line:
1. The first word is extracted and matched against the registry (with alias support).
2. The remaining string is passed to the command handler.
3. The handler typically uses `IKJPARS` to validate its operands before calling MVS services.

### REXX/TSO Integration

REXX scripts running under TSO can issue commands using `ADDRESS TSO`. This crate provides the `RexxTsoHost` which:
- Captures `SAY` output and routes it to `PUTLINE`.
- Handles the `PULL` instruction via `GETLINE`.
- Allows scripts to perform TSO-specific actions like `LISTDSI`.

## Feature Coverage

| Feature | Category | Status |
|---------|----------|--------|
| IKJPARS         | Services | Implemented (Keyword/Positional) |
| ALLOCATE / FREE | Dataset  | Implemented (SVC 99 integration) |
| LISTDS / LISTALC| Dataset  | Implemented |
| PROFILE         | Session  | Implemented (PREFIX, MSGID, WTP) |
| SUBMIT / STATUS | JES2     | Implemented |
| EXEC (REXX)     | Scripting| Implemented |
| CALL            | Program  | Implemented |
| ALTLIB          | Session  | Implemented |

## Usage Examples

### Executing a TSO Command Programmatically

```rust
use open_mainframe_tso::{execute, TsoSession};

let mut session = TsoSession::new("IBMUSER");
let result = execute(&mut session, "ALLOCATE F(SYSUT1) DSN('PROD.DATA') SHR").unwrap();

if result.is_success() {
    println!("Allocation successful!");
}
```

### Parsing Parameters with IKJPARS

```rust
use open_mainframe_tso::services::ikjpars;

// Define a simple PDL and parse a command
let parsed = ikjpars(pdl, "MYCMD KEYWORD(VALUE)").expect("Parse failed");
```

## Testing

The TSO crate is tested for interactive consistency:
- **Parser Tests**: Validates 50+ command syntax variants.
- **Command Tests**: Verifies complex `ALLOCATE` scenarios including concatenation.
- **REXX Integration**: Round-trip tests for REXX scripts issuing TSO commands.
- **Session Tests**: Ensures `PREFIX` is correctly applied to non-quoted datasets.

```sh
cargo test -p open-mainframe-tso
```

## Limitations and Future Work

- **CLIST Support**: The CLIST interpreter is partially implemented; full support for nested `%` scripts is in progress.
- **Interactive Prompts**: While the API supports prompting, the current CLI runner is non-interactive for missing parameters.
- **Full ISPF Integration**: Integration with the `open-mainframe-ispf` crate for `ISPEXEC` commands is currently being refined.
