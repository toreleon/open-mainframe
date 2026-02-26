# open-mainframe-jcl

A high-performance **JCL (Job Control Language) interpreter** for the OpenMainframe project. This crate provides a complete job execution pipeline, from fixed-format source parsing and recursive procedure expansion to step-by-step program dispatching and dynamic allocation.

## Overview

JCL is the scripting language used to control batch processing on IBM mainframes. `open-mainframe-jcl` implements a z/OS-compatible interpreter that allows users to submit and run complex multi-step jobs. The engine handles the expansion of cataloged procedures (PROCs), symbolic parameter substitution, and the conditional execution logic (`COND` and `IF/THEN/ELSE`) that defines the mainframe batch workflow.

## Architecture

```
    JCL Source Source                     Job Execution Engine
    ┌──────────────┐                      ┌────────────────────┐
    │ //JOB        │    Parsing           │    Job Executor    │
    │ //STEP EXEC  │ ──────────────────>  │    (Lifecycle)     │
    │ //DD         │    JclParser         │  Steps, COND, CC   │
    └──────────────┘                      └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Proc Expansion    ┌────────────────────┐
    │  PROCLIB     │ ──────────────────>  │   Dataset Engine   │
    │  (Cataloged) │    ProcExpander      │   (Allocation)     │
    └──────────────┘                      │ SVC 99, DDnames    │
                                          └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Dispatch          ┌────────────────────┐
    │  Program     │ <──────────────────  │  Utility Registry  │
    │  Execution   │    JobExecutor       │  IEBCOPY, GENER    │
    └──────────────┘                      └────────────────────┘
```

### Module Structure & Lines of Code

| Module | Lines | Description |
|--------|------:|-------------|
| `parser/` | 3,809 | Recursive descent parser for all z/OS JCL statements and complex operands |
| `executor/` | 4,000 | Job state machine, step execution, condition code tracking, and utility integration |
| `procedure.rs`| 1,331 | Multi-level PROC expansion, recursive parameter substitution, and override logic |
| `ast/` | 761 | Complete AST: Job, Step, DD, IF/THEN constructs, and parameter types |
| `lexer/` | 727 | Fixed-format scanner, column-based tokenization, and column 72 continuation |
| `error.rs` | 205 | JclError enum with source location tracking and miette diagnostics |
| **Total** | **~10,872** | |

## Implementation Details

### Procedure Expansion (Symbolics)
The `ProcedureExpander` replicates the z/OS two-pass expansion model:
- **First Pass**: Identifies all `SET` statements and `PROC` headers to build a parameter map.
- **Second Pass**: Recursively replaces `&VAR.` symbols. It correctly handles the "period termination" rule (e.g., `&DSN..DATA` becomes `MYDSN.DATA`).
- **Override Logic**: Implements the complex override rules for `EXEC` parameters and `DD` statements when calling PROCs (e.g., `//STEP1.SYSUT1 DD ...` to override `SYSUT1` in `STEP1` of a PROC).

### Job Execution & Step Passing
The `JobExecutor` maintains an internal state during the life of a job:
- **Condition Code Table**: Stores the RC (Return Code) of every completed step.
- **Dataset Passing**: Implements `DISP=(PASS)`. A "Passed Dataset Table" tracks temporary files that are created in one step and deleted only when a later step specifies `DISP=(OLD,DELETE)`.
- **GDG Resolution**: Maintains a job-level map for Generation Data Groups. If `STEP1` creates `GDG(+1)`, `STEP2` referring to `GDG(+1)` will point to that same new generation, matching z/OS consistency.

## Implementation vs Mainframe Gaps

| Feature | Real z/OS Mainframe | OpenMainframe implementation |
|---------|---------------------|------------------------------|
| **Step Isolation**| Separate Address Spaces (ASIDs). | Steps run as sub-processes or tasks within the same system. |
| **SYSOUT** | JES2 Spool with routing (DEST). | Written to files in a `sysout/` directory on the host. |
| **Restart/Checkpoint** | Supports `RESTART=STEP2`. | Always executes from the first step. |
| **Resource Limits** | `REGION`, `TIME`, `MEMLIMIT` enforced by WLM. | Parsed but not currently enforced. |
| **Tape Handling** | Real tape drives and mounts. | Simulated via sequential files on disk. |
| **System Symbols** | Resolved from IEASYMxx. | Resolved via `open-mainframe-parmlib` integration. |

## Feature Coverage

### JCL Statement Support

| Statement | Status | Supported Parameters |
|-----------|--------|----------------------|
| `JOB` | Full | CLASS, MSGCLASS, TYPRUN, REGION, ADDRSPC, COND |
| `EXEC` | Full | PGM, PROC, PARM, COND, REGION, TIME |
| `DD` | Full | DSN, DISP, UNIT, VOL, SPACE, DCB, SYSOUT, SUBSYS, FREE, AMP |
| `PROC / PEND`| Full | In-stream and cataloged procedure definitions |
| `SET` | Full | Symbolic variable assignment |
| `IF / THEN / ELSE` | Full | Complex logical expressions (AND, OR, NOT) |
| `INCLUDE` | Full | Cataloged member inclusion |
| `OUTPUT` | Partial | DEST, CLASS, FORMS, WRITER |

## Usage Examples

### Executing a Multi-Step JCL

```rust
use open_mainframe_jcl::{parse, JobExecutor, ExecutionConfig};

let jcl = r#"
//MYJOB    JOB (ACCT),'OPENMAINFRAME',CLASS=A
//STEP1    EXEC PGM=IEBGENER
//SYSUT1   DD *
INPUT DATA
/*
//SYSUT2   DD DSN=&&TEMP,DISP=(NEW,PASS),SPACE=(TRK,1)
//SYSPRINT DD SYSOUT=*
//STEP2    EXEC PGM=SORT,COND=(0,NE,STEP1)
//SORTIN   DD DSN=&&TEMP,DISP=(OLD,DELETE)
//SORTOUT  DD SYSOUT=*
//"#;

let job = parse(jcl).expect("Parse error");
let mut executor = JobExecutor::new();
let result = executor.execute(&job).unwrap();

println!("Job status: {}", if result.success { "OK" } else { "FAIL" });
```

## Testing

The JCL crate is verified against a corpus of 500+ test cases:
- **Parser Matrix**: Tests 100+ variations of the `DD` statement.
- **Symbolic Stress**: Recursive PROC expansion with 10+ levels of nesting and parameter overrides.
- **Logic Tests**: Comprehensive verification of `IF/THEN/ELSE` truth tables and `COND` operator precedence.
- **GDG Tests**: Round-trip creation and retrieval of +1/0/-1 generations.

```sh
cargo test -p open-mainframe-jcl
```
