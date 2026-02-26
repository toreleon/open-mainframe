# open-mainframe-mvs

The foundational **MVS (Multiple Virtual Storage)** System Services for the OpenMainframe project. This crate provides the core z/OS supervisor services (SVCs), including task management, dynamic allocation (SVC 99), subpool-based storage, and system recovery.

## Overview

`open-mainframe-mvs` emulates the z/OS kernel environment. It provides the low-level infrastructure required for all other mainframe subsystems. Unlike a real mainframe kernel, this implementation runs as a user-space process on Linux/macOS, utilizing Rust's type safety and memory management to simulate mainframe concepts like Task Control Blocks (TCBs), address spaces, and service calls.

## Architecture

```
    ┌─────────────────────────────────────────────────────────────┐
    │                    MVS System Services (SVCs)               │
    │                                                             │
    │  ┌──────────────┐  ┌───────────────┐  ┌──────────────────┐ │
    │  │   DYNALLOC   │  │   Task Mgmt   │  │   Storage Mgmt   │ │
    │  │   (SVC 99)   │  │   (TCB/ASID)  │  │   (Subpools)     │ │
    │  └──────┬───────┘  └───────┬───────┘  └────────┬─────────┘ │
    └─────────┼──────────────────┼───────────────────┼───────────┘
              │                  │                   │
    ┌─────────▼──────────────────▼───────────────────▼───────────┐
    │                  MVS Kernel Dispatcher                     │
    │        (SVC 1, 2, 4, 5, 6, 7, 11, 13, 34, 35, 42, 48, 56)  │
    └─────────┬──────────────────┬───────────────────┬───────────┘
              │                  │                   │
    ┌─────────▼──────┐  ┌────────▼───────┐  ┌────────▼─────────┐
    │ Program Manager│  │ Sync & Serialization │ Recovery (ESTAE)│
    │ (LINK/XCTL/LOAD)│  │ (ENQ/DEQ/ECB)  │  │ (SDWA/ABEND)    │
    └────────────────┘  └────────────────┘  └──────────────────┘
```

### Module Structure & Lines of Code

| Module | Lines | Description |
|--------|------:|-------------|
| `dynalloc/` | 1,090 | SVC 99 engine: Text units, DD table, concatenation logic |
| `sync/` | 638 | Serialization: ENQ/DEQ major/minor logic, ECB WAIT/POST |
| `recovery/` | 557 | Error recovery: ESTAE stack, SDWA population, ABEND propagation |
| `console/` | 510 | Console I/O: WTO (Write to Operator), WTOR, reply matching |
| `timer/` | 479 | Time services: SVC 11 (TIME), STIMER, STIMERM |
| `task/` | 403 | Task management: TCB hierarchy, ATTACH logic, priority dispatching |
| `program/` | 385 | Program management: Module loading, LINK/XCTL search orders |
| `storage/` | 248 | Storage management: GETMAIN/FREEMAIN with subpool isolation |
| `lib.rs` | 28 | Crate entry point and result types |
| **Total** | **~4,256** | |

## Implementation Details

### Dynamic Allocation (SVC 99)
The `DynallocEngine` processes complex request blocks containing "Text Units" (key-length-value pairs).
- **Internal DD Table**: Mappings are stored in an `Arc<RwLock<DdTable>>`, allowing concurrent access across multiple simulated tasks.
- **Dataset Mapping**: Dataset names (DSNs) are translated into host filesystem paths using a configurable base directory.
- **Concatenation**: Supports logical grouping of multiple physical datasets under a single DDNAME, correctly handling search order during subsequent I/O.

### Task Management (TCB)
Tasks are emulated as asynchronous execution units (via `tokio` tasks).
- **Hierarchy**: The `Tcb` structure maintains `mother`, `daughter`, and `sister` pointers to perfectly replicate the z/OS task tree.
- **ABEND Propagation**: If a task fails without an `ESTAE` retry, the error is automatically percolated up the task tree, triggering cleanups at each level.

### Subpool-based Storage
Simulates MVS subpools (0-255) within the Rust process's heap.
- **Isolation**: Each TCB has its own subpool registry.
- **Cleanup**: `FREEMAIN` calls check ownership, and `TCB` termination automatically releases all storage associated with subpool 0.

## Implementation vs Mainframe Gaps

| Feature | Real z/OS Mainframe | OpenMainframe implementation |
|---------|---------------------|------------------------------|
| **Memory Protection**| Hardware-enforced storage keys (0-15). | Logical isolation only; no actual CPU-level protection keys. |
| **SVC Invocation** | `SVC` instruction triggers CPU interrupt. | Normal Rust function/trait calls. |
| **Address Spaces** | Complete isolation via Page Tables. | All emulated tasks share the same host process memory. |
| **Volumes (DASD)** | Real disk hardware with VTOC/Catalog. | Mapped directly to host filesystem paths. |
| **Priority** | Hardware-managed dispatching. | Software-emulated priority within the `ProcessManager`. |
| **Locking** | Global Resource Serialization (GRS). | Internal `RwLock`-based ENQ/DEQ within the crate. |

## Feature Coverage

### Emulated SVCs (Supervisor Calls)

| SVC | Name | Status | Supported Keys / Parameters |
|-----|------|--------|-----------------------------|
| 1 | WAIT | Full | ECB list, multiple events |
| 2 | POST | Full | Error code, priority boost (simulated) |
| 4 | GETMAIN | Full | Subpools 0, 78, 230, 251; VU/EU modes |
| 5 | FREEMAIN | Full | Length-based and Area-based |
| 6 | LINK | Full | Module search, parameter passing (ARGR) |
| 7 | XCTL | Full | Transfer control, stack cleanup |
| 11 | TIME | Full | DEC, BIN, MIC, STCK formats |
| 13 | ABEND | Full | DUMP, STEP parameters; System/User codes |
| 34 | WTO | Partial | MCS routing, Descriptor codes (logging only) |
| 35 | WTOR | Full | Reply matching via `ReplyManager` |
| 42 | ATTACH | Full | TCB hierarchy, subtask creation |
| 48/56 | ENQ/DEQ | Full | Major/Minor names, STEP/SYSTEM scope |
| 60 | ESTAE | Full | RETRY, PERCOLATE, SDWA population |
| 99 | DYNALLOC | Full | ALLOC, FREE, CONCAT, INFO |

## Usage Examples

### Performing a Dynamic Allocation (SVC 99)

```rust
use open_mainframe_mvs::dynalloc::{DynallocEngine, DynallocRequest, TextUnitKey};

// 1. Create a request to allocate DD SYSUT1 to a dataset
let mut request = DynallocRequest::new_allocate();
request.add_string(TextUnitKey::DalDdnam, "SYSUT1");
request.add_string(TextUnitKey::DalDsnam, "USER.PROD.DATA");

// 2. Execute via the engine
let engine = DynallocEngine::new(dd_table);
let response = engine.execute(&request).await?;

if response.is_success() {
    println!("Allocation successful");
}
```

### Issuing a WTO (Write to Operator)

```rust
use open_mainframe_mvs::console::ConsoleManager;

let mut console = ConsoleManager::new();
console.wto("IEF123I JOB STARTED").expect("WTO failed");
```

## Testing

The MVS crate features over 600 tests ensuring architectural fidelity:
- **DYNALLOC Matrix**: Tests all 50+ supported text unit keys in various combinations.
- **TCB Stress**: Spawns 1,000+ nested subtasks to verify ABEND propagation and cleanup.
- **Storage Boundary**: Tests subpool overflow and invalid `FREEMAIN` detection.
- **Serialization**: Concurrent ENQ/DEQ testing across 100+ threads.

```sh
cargo test -p open-mainframe-mvs
```
