# open-mainframe-ims

IMS (Information Management System) — a comprehensive Rust implementation of the hierarchical database (IMS/DB) and transaction manager (IMS/TM) for the OpenMainframe project.

## Overview

IMS is one of the most resilient and high-performance database and transaction processing systems on IBM mainframes. This crate provides the full stack required to run IMS applications, including DBD/PSB parsing, DL/I (Data Language/I) hierarchical data access, MFS (Message Format Service) screen rendering, and the IMS/TM region-based transaction processing environment.

## Architecture

```
    Application Source                    IMS Runtime Environment
    ┌──────────────┐                      ┌────────────────────┐
    │ CALL 'CBLTDLI'│    DL/I Interface    │    DLI Runtime     │
    │ GU, GN, REPL  │ ──────────────────>  │    (Hierarchical)  │
    └──────────────┘    DliRuntime        │  SSAs, PCB, Segment│
           │                               └────────────────────┘
           ▼                                        │
    ┌──────────────┐    Metadata          ┌────────────────────┐
    │  DBD / PSB   │ ──────────────────>  │   Storage Layer    │
    │  Definitions │    DbdParser         │  Hierarchical Keys │
    └──────────────┘                      └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Transaction       ┌────────────────────┐
    │ IMS/TM Region│ <──────────────────  │    MFS Subsystem   │
    │ MPP / BMP    │    TransactionDef    │  Screen Formatting │
    └──────────────┘                      └────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `dli` | DL/I engine: GU, GN, GNP, ISRT, REPL, DLET, and SSA processing |
| `dbd` | Database Definition (DBD) parser and physical hierarchical model |
| `psb` | PSB (Program Specification Block) and PCB (Program Communication Block) management |
| `tm` | Transaction Manager: AltPCBs, SPA (Scratch Pad Area), and conversational state |
| `mfs_parser` | MFS (Message Format Service) source compiler for terminal formats |
| `mfs_runtime`| MFS execution engine for 3270 screen formatting and field mapping |
| `regions` | MPP (Message Processing Program) and BMP (Batch Message Processing) region simulation |
| `otma` | Open Transaction Manager Access for asynchronous messaging |
| `connect` | IMS Connect simulation for TCP/IP and connection pooling |
| `operator` | IMS operator command processor (/DISPLAY, /START, /STOP, /CHECKPOINT) |
| `fastpath` | DEDB (Data Entry Database) and EMH (Expedited Message Handler) support |
| `codegen` | Utilities for generating AIB/DIB copybooks and DL/I call wrappers |

## Key Types and Components

### DL/I Runtime
- `DliRuntime`: The core execution engine for hierarchical database calls.
- `StatusCode`: Complete implementation of 30+ DL/I status codes (GE, GB, GA, etc.).
- `Ssa`: Segment Search Argument parser supporting qualified and unqualified searches.

### Metadata
- `DatabaseDefinition (DBD)`: Defines the physical hierarchical structure (Segments, Keys, Fields).
- `ProgramSpecificationBlock (PSB)`: Defines the application's view of one or more DBDs.

### Transaction Manager (TM)
- `TransactionScheduler`: Manages the job queue and dispatches transactions to available regions.
- `MppRegion`: Simulates an Online Message Processing Region.
- `BmpRegion`: Simulates a Batch Message Processing Region.

### MFS (Message Format Service)
- `MfsCompiler`: Compiles MFS macros (FMT, MSG, TABLE) into internal DIF/DOF formats.
- `MfsRuntime`: Maps transaction data to 3270 screen fields using compiled formats.

## Feature Coverage

| Feature | Category | Status |
|---------|----------|--------|
| DL/I Database | DB | Implemented (GU, GN, GNP, ISRT, REPL, DLET) |
| SSA Processing| DB | Implemented (Relational operators, command codes) |
| MFS Formatting| TM | Implemented (DIF/DOF, field mapping) |
| Conversational| TM | Implemented (SPA management) |
| Region Mgmt   | TM | Implemented (MPP, BMP, IFP) |
| Checkpoint    | System | Implemented (CHKP / XRST) |
| AltPCBs       | TM | Implemented |

## Usage Examples

### Performing a DL/I Get Unique (GU)

```rust
use open_mainframe_ims::dli::{DliCall, DliRuntime};

let mut runtime = DliRuntime::new().unwrap();
runtime.schedule_psb("MYPSB").unwrap();

let mut buffer = vec![0u8; 100];
// Search for a customer with a specific ID
let result = runtime.gu(
    "CUSTOMER",
    Some("CUSTNO  = 12345"),
    &mut buffer,
).unwrap();

if result.is_ok() {
    println!("Found customer segment!");
}
```

### IMS Operator Command

```rust
use open_mainframe_ims::operator::{ImsCommand, ImsCommandProcessor};

let mut processor = ImsCommandProcessor::new();
let result = processor.execute("/DISPLAY TRANSACTION ALL").unwrap();
println!("Active transactions: {}", result.output);
```

## Testing

The IMS crate includes over 300 tests:
- **DL/I**: Comprehensive verification of status code logic (GA, GB, GK, GE class).
- **MFS**: Field mapping and attribute rendering tests.
- **DBD/PSB**: Parser validation for complex hierarchical structures.
- **TM**: Multi-region scheduling and conversational state round-trips.

```sh
cargo test -p open-mainframe-ims
```
