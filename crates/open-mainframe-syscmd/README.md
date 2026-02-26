# open-mainframe-syscmd

z/OS System Commands & Console — a comprehensive Rust implementation of the mainframe's operator command interface and SDSF (System Display and Search Facility) for the OpenMainframe project.

## Overview

System Commands are the primary way operators interact with and manage z/OS. This crate reimplements the MVS command dispatcher, providing support for standard DISPLAY, START, STOP, and CANCEL commands, as well as a full SDSF-compatible engine for monitoring jobs, initiators, and system logs.

## Architecture

```
    Operator Input                        System Command Environment
    ┌──────────────┐                      ┌────────────────────┐
    │  DISPLAY A,L │    Command           │    Dispatcher      │
    │  START MYJOB │ ──────────────────>  │    (Routing)       │
    └──────────────┘    CommandParser     │  MVS, JES2, Subsys │
           │                               └────────────────────┘
           ▼                                        │
    ┌──────────────┐    SDSF Engine       ┌────────────────────┐
    │  SDSF Panels │ ──────────────────>  │   Status Monitor   │
    │  DA, ST, LOG │    SdsfEngine        │   ASIDs, Jobs, CPU │
    └──────────────┘                      └────────────────────┘
                                                    │
                                                    ▼
    ┌──────────────┐    Console I/O       ┌────────────────────┐
    │  WTO / WTOR  │ <──────────────────  │    System Console  │
    │  Replies     │    CommandRegistry   │    Master Console  │
    └──────────────┘                      └────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `commands` | MVS command dispatcher: Implementation of D, S, P, C, A, and R commands |
| `sdsf` | SDSF engine: Support for DA, I, O, ST, H, and LOG panels with line commands |

## Key Types and Components

### Command Dispatcher
- `CommandDispatcher`: Orchestrates the parsing and execution of system commands.
- `SystemCommand`: Enumeration of all supported MVS and JES2 operator commands.
- `AddressSpace`: Represents an active ASID with its status and resource usage.

### SDSF Engine
- `SdsfEngine`: Provides a structured API for querying system state in an SDSF-like format.
- `SdsfJob`: Represents a job row in the SDSF ST or DA panels.
- `LineCommand`: Implementation of SDSF line commands (e.g., `S` for browse, `C` for cancel).

## Feature Coverage

| Feature | Category | Status |
|---------|----------|--------|
| DISPLAY A,L     | MVS      | Implemented |
| START / STOP    | MVS      | Implemented (Address space lifecycle) |
| CANCEL / FORCE  | MVS      | Implemented |
| REPLY (R)       | MVS      | Implemented (WTOR response) |
| $D / $S / $P    | JES2     | Implemented (Routed to JES2) |
| SDSF DA Panel   | SDSF     | Implemented |
| SDSF LOG Panel  | SDSF     | Implemented (SYSLOG browsing) |

## Usage Examples

### Executing a DISPLAY Command

```rust
use open_mainframe_syscmd::commands::CommandDispatcher;

let mut dispatcher = CommandDispatcher::new();
let result = dispatcher.execute("DISPLAY A,L").unwrap();
println!("Active Jobs: {}", result.output);
```

### Querying the SDSF Status Panel

```rust
use open_mainframe_syscmd::sdsf::SdsfEngine;

let engine = SdsfEngine::new();
let jobs = engine.get_jobs().unwrap(); // Example call
for job in jobs {
    println!("Job: {}, Owner: {}, Status: {}", job.name, job.owner, job.status);
}
```

## Testing

The System Commands crate includes 150+ tests:
- **Dispatcher**: Validates command parsing and routing across various subsystems.
- **SDSF**: Verifies the correct population of SDSF panels from system data.

```sh
cargo test -p open-mainframe-syscmd
```
