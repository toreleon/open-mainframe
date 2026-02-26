# open-mainframe-jes2

A core implementation of the **JES2 (Job Entry Subsystem 2)** for the OpenMainframe project. This crate manages the entire batch lifecycle, providing a priority-based job queue, spool storage, initiator control, and comprehensive operator command processing.

## Overview

JES2 is the primary work manager for z/OS. `open-mainframe-jes2` reimplements this subsystem to provide a realistic mainframe batch environment. It handles everything from the "Internal Reader" where JCL is first received, to the initiators that dispatch jobs, and finally the output processor that manages SYSOUT spooling.

## Architecture

```
    Job Submission                         JES2 Job Lifecycle
    ┌──────────────┐                      ┌────────────────────┐
    │  Internal    │    Input             │    Job Queue       │
    │  Reader      │ ──────────────────>  │    (Priority)      │
    └──────────────┘    Intrdr            │  Active, Output    │
           │                               └────────────────────┘
           ▼                                        │
    ┌──────────────┐    Spooling          ┌────────────────────┐
    │  Spool       │ ──────────────────>  │   Initiator        │
    │  Volumes     │    SpoolManager      │   (Execution)      │
    └──────────────┘                      │ Class, Priority    │
                                          └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Output            ┌────────────────────┐
    │  SYSOUT      │ <──────────────────  │  Output Processor  │
    │  Processing  │    OutputDescriptor  │  Print, Purge      │
    └──────────────┘                      └────────────────────┘
```

### Module Structure & Lines of Code

| Module | Lines | Description |
|--------|------:|-------------|
| `exit` | 1,442 | Exit framework: Support for custom processing at 20+ job lifecycle hooks |
| `output` | 796 | SYSOUT management: Class-based routing, destinations, and print grouping |
| `commands` | 715 | Operator interface: Full implementation of $ commands ($D, $S, $P, $C, $A, $T) |
| `job` | 661 | Job model: Complete state machine (INPUT -> CONVERSION -> READY -> ... -> PURGE) |
| `intrdr` | 653 | Internal Reader: Programmatic and TSO-based job submission interface |
| `initiator`| 649 | Initiator manager: Class-aware workload selectors and dispatchers |
| `jecl` | 617 | JES2 Control Language: Parser for `/*JOBPARM`, `/*OUTPUT`, `/*ROUTE` |
| `config` | 600 | Subsystem configuration: Spool volumes, class definitions, and MAS parameters |
| `queue` | 541 | Scheduling: Priority-weighted job selection and class matching |
| `checkpoint`| 379 | Persistence: State recovery for Warm/Cold starts |
| `spool` | 258 | Spool management: Binary storage for JCL source and SYSOUT datasets |
| **Total** | **~7,414** | |

## Implementation Details

### Job State Machine
Every unit of work in JES2 follows a strict transition path:
1. **INPUT**: Data read from `INTRDR` and written to `SPOOL`.
2. **CONVERSION**: JCL is expanded and validated.
3. **READY**: Job is assigned to a priority queue based on `CLASS`.
4. **RUNNING**: An initiator with a matching class list selects and runs the job.
5. **OUTPUT**: Job completes; SYSOUT is closed and made available for printing.
6. **HARDCOPY**: SYSOUT is processed by a writer.
7. **PURGE**: All spool resources are reclaimed.

### Priority Scheduling (WLM-light)
JES2 uses a priority-weighted selection algorithm (0-15).
- **Class Matching**: Initiators (e.g., `I1`) are configured with a list of classes (e.g., `ABC`). They will only select jobs whose `CLASS` is in their list.
- **Priority Aging**: Jobs that have been in the queue for a long time receive a small priority boost to prevent starvation.

### Spool Management
The `SpoolManager` implements a simulated multi-volume spool.
- **Data Sets**: Each job's SYSOUT is stored as a collection of binary records on the host disk.
- **Addressing**: Uses a simulated Track/Record (MTTR) addressing system, allowing for efficient random access during SDSF browsing.

## Implementation vs Mainframe Gaps

| Feature | Real z/OS Mainframe | OpenMainframe implementation |
|---------|---------------------|------------------------------|
| **Job ID** | `JOBnnnnn`, `STCnnnnn`, `TSUnnnnn`. | Internal numeric ID only (string representation is simplified). |
| **MAS Clustering** | Shared Spool across multiple z/OS images. | Single-member implementation only. |
| **Checkpoints** | Shared 4K records on Coupling Facility/DASD. | Local binary file storage for system state. |
| **Output Writers** | IP Printway, PSF, PSF/MVS for real printers. | Mapped to plain text files or internal message buffers. |
| **NJE** | Network Job Entry between JES nodes. | Not implemented. |
| **Security** | RACF `JESSPOOL` class protection. | Security checks are stubbed; full RACF integration is in progress. |

## Feature Coverage

### JES2 Command Support

| Command | Name | Status | Supported Parameters |
|---------|------|--------|----------------------|
| `$D` | Display | Full | `$D J`, `$D A`, `$D I`, `$D S` (Spool), `$D Q` |
| `$S` | Start | Full | Start initiators, start jobs |
| `$P` | Stop | Full | Stop initiators, purge jobs |
| `$C` | Cancel | Full | Cancel active jobs |
| `$A` | Release | Full | Release held jobs or output |
| `$T` | Modify | Full | Change job class, priority, or initiator classes |

## Usage Examples

### Submitting a Job Programmatically

```rust
use open_mainframe_jes2::Jes2;
use open_mainframe_jes2::intrdr::InternalReader;

let mut jes = Jes2::new();
let mut reader = InternalReader::new(&mut jes);

// Submit JCL stream
reader.write("//MYJOB JOB CLASS=A\n//S1 EXEC PGM=IEFBR14\n").unwrap();
let job_id = reader.close().unwrap();

println!("Submitted job ID: {}", job_id);
```

### Checking Job Status via Operator Command

```rust
use open_mainframe_jes2::commands::execute_command;

let mut jes = Jes2::new();
let response = execute_command(&mut jes, "$D J(MYJOB)").unwrap();
println!("JES2 Status: {}", response);
```

## Testing

The JES2 crate includes 250+ tests for operational stability:
- **Concurrency**: Verified multi-initiator job selection with 100+ concurrent threads.
- **State Integrity**: Ensures jobs cannot skip conversion or output phases.
- **Spool Stress**: Verifies data integrity during large (1GB+) SYSOUT writes.
- **Checkpoint Recovery**: Tests Warm Start by crashing and reloading the checkpoint file.

```sh
cargo test -p open-mainframe-jes2
```
