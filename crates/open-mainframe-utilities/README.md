# open-mainframe-utilities

A high-fidelity Rust implementation of the **Standard z/OS Utility Programs** for the OpenMainframe project — providing core data management utilities (IEBCOPY, IEBGENER, IEBUPDTE), system service aids (AMASPZAP), and batch execution interfaces.

## Overview

Mainframe operations rely heavily on a suite of standard utility programs for data management, member maintenance, and system patching. This crate reimplements these iconic utilities, providing a unified `UtilityRegistry` for dispatching `EXEC PGM=` calls from JCL, standardized condition code reporting, and high-fidelity message formatting.

The implementation comprises:
1. **Dataset Utilities** — Implementation of IEBCOPY (PDS management), IEBGENER (Sequential copy), IEBUPDTE (Member maintenance), and IEBCOMPR (Comparison).
2. **System Utilities** — Implementation of IEHPROGM (Catalog/VTOC management), IEHLIST (Listing), and IEHMOVE (Data movement).
3. **Service Aids** — A complete implementation of AMASPZAP ("Superzap") for verifying and patching load modules and data records.
4. **Data Generation** — IEBDG utility for creating test datasets based on complex patterns.
5. **Batch Interfaces** — Support for standard batch entry points like IKJEFT01 (TSO), IRXJCL (REXX), and BPXBATCH (USS).
6. **Utility Framework** — A standardized context and messaging system that ensures all utilities behave and report errors like their IBM counterparts.

## Architecture

```
    JCL EXEC Statement / API Call
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  Utility Registry                      │
    │  - Program name to handler mapping                     │
    │  - Subsystem dispatch (IMS, CICS, DB2)                 │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  Utility Context                       │
    │  - DDname to Dataset mapping                           │
    │  - SYSIN / SYSPRINT capture                            │
    │  - Condition Code (CC) tracking                        │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │               Utility Program Modules                  │
    │  - IEBxxxx (Dataset Utilities)                         │
    │  - IEHxxxx (System Utilities)                          │
    │  - AMASPZAP (Patching)                                 │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                 Messaging & Reporting                  │
    │  - IBM-style message IDs (IEB1013I)                    │
    │  - Formatted listings and dumps                        │
    └────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Description | Lines |
|--------|-------------|------:|
| `iebcopy` | Partitioned dataset (PDS) management: Copy, Merge, and Compress | ~1,206 |
| `iebgener` | Sequential dataset management: Copy and record reformatting | ~788 |
| `amaspzap` | Service aid: Load module and record verification/patching (VER/REP) | ~920 |
| `iebupdte` | Source library maintenance: Member creation and modification | ~779 |
| `iebcompr` | Dataset comparison utility: Sequential and partitioned support | ~741 |
| `iebptpch` | Print and punch utility: Member formatting and hex dumping | ~720 |
| `ieh` | System utilities: IEHPROGM, IEHLIST, and IEHMOVE | ~847 |
| `iebdg` | Data generator: Pattern-based test data creation | ~767 |
| `batch` | Batch entry points: IKJEFT01, IRXJCL, and BPXBATCH | ~690 |
| `subsystems`| Subsystem utility stubs: IMS, CICS, and DB2 utility wrappers | ~1,043 |
| `lib.rs` | Utility registry and core framework types | ~1,207 |

**Total**: ~9,741 lines of Rust.

## Key Types and Components

### Framework

| Type | Description |
|------|-------------|
| `UtilityRegistry`| The central dispatcher mapping program names to implementations. |
| `UtilityContext` | Encapsulates the execution environment (DDs, PARM, message buffer). |
| `UtilityProgram` | Trait that all utility implementations must satisfy. |

### Messaging & CC

| Type | Description |
|------|-------------|
| `UtilityResult` | Captures the final condition code (0, 4, 8, 12, 16) and output. |
| `UtilityMessage`| Represents a standard IBM message (ID + Severity + Text). |
| `MessageSeverity`| Enum: `Info` (I), `Warning` (W), `Error` (E), `Severe` (S). |

### Specialized Utilities

| Type | Description |
|------|-------------|
| `Iebcopy` | Handles complex `COPY`, `SELECT`, and `EXCLUDE` control statements. |
| `Amaspzap` | Implements the `VER` (Verify), `REP` (Replace), and `DUMP` logic. |

## Implementation Details

### Standardized Messaging (IEBnnnnX)

This crate ensures that all generated output matches the expected IBM format. Every message is assigned a standard ID (e.g., `IEB1013I`) and routed to the `SYSPRINT` buffer within the `UtilityContext`. This allows JCL executors to capture and display utility logs faithfully.

### AMASPZAP (The "Superzap" Logic)

The `amaspzap` module provides a byte-perfect implementation of the verify-and-patch logic:
- **VER**: Compares the target address or record against expected data. If they don't match, the patch is rejected (CC=12).
- **REP**: Atomically replaces the target data if verification succeeds.
- **DUMP**: Produces a side-by-side hex and character dump of the target.

### IEBCOPY PDS Operations

`IEBCOPY` is the most complex utility implemented. It handles:
- **Copying**: Moving members between PDS datasets.
- **Merging**: Combining multiple input libraries into one.
- **Selection**: Using `SELECT MEMBER=(...)` or `EXCLUDE` to filter members.
- **Renaming**: Renaming members during the copy process.

## Feature Coverage

| Utility | Category | Status |
|---------|----------|--------|
| IEBCOPY | Dataset  | Implemented (Copy, Select, Exclude, Rename) |
| IEBGENER| Dataset  | Implemented (Copy, RECORD FIELD reformatting) |
| IEBUPDTE| Dataset  | Implemented (./ ADD, ./ CHANGE, ./ DELETE) |
| IEBCOMPR| Dataset  | Implemented (Record-level comparison) |
| IEBPTCPH| Dataset  | Implemented (RECORD FIELD printing) |
| AMASPZAP| Service  | Implemented (VER, REP, DUMP, NAME, ABSDUMP) |
| IEHPROGM| System   | Implemented (CATLG, UNCATLG, SCRATCH, RENAME) |
| IKJEFT01| Batch    | Implemented (TSO command processing) |
| BPXBATCH| Batch    | Implemented (USS process dispatching) |
| IEFBR14 | System   | Implemented (Standard CC=0 return) |

## Usage Examples

### Executing IEBGENER Programmatically

```rust
use open_mainframe_utilities::{UtilityRegistry, UtilityContext};

let mut registry = UtilityRegistry::with_builtins();
let mut ctx = UtilityContext::new("STEP1", "IEBGENER");

// Setup DDs (usually handled by JCL executor)
ctx.add_dd("SYSUT1", "USER.INPUT.DATA");
ctx.add_dd("SYSUT2", "USER.OUTPUT.DATA");

let result = registry.dispatch("IEBGENER", &mut ctx).unwrap();
println!("IEBGENER CC: {}", result.condition_code);
```

### Using AMASPZAP to Verify and Patch

```rust
use open_mainframe_utilities::amaspzap::{Amaspzap, ZapCommand};

let mut zap = Amaspzap::new();
zap.add_command(ZapCommand::Verify { offset: 0x10, data: vec![0x47, 0xF0] });
zap.add_command(ZapCommand::Replace { offset: 0x10, data: vec![0x07, 0xFE] });

// Execute against a module or record...
```

## Testing

The Utilities crate features an extensive suite of integration tests:
- **Round-trip Tests**: Verifies that `IEBGENER` copies data without alteration.
- **Conflict Tests**: Ensures `AMASPZAP` rejects patches when `VER` fails.
- **Control Statement Tests**: Validates 100+ variations of `IEBCOPY` control syntax.
- **Batch Tests**: Simulates `IKJEFT01` executing TSO commands via the registry.

```sh
cargo test -p open-mainframe-utilities
```

## Limitations and Future Work

- **IEBCOPY Compress**: The `COMPRESS` command is currently a stub; actual space reclamation depends on the `open-mainframe-dataset` implementation.
- **Subsystem Utilities**: Utilities like `DSNTIAD` (DB2) and `DFSRRC00` (IMS) are skeletal and require the respective subsystem crates to be fully integrated.
- **IEHMOVE**: Full support for moving whole volumes is not yet implemented.
- **SMF Recording**: Utilities do not yet write Type 14/15 SMF records upon completion.
