---
version: 'v5.0'
planningGroup: 'PG-1'
technology: 'MVS System Services & SVCs'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-mvs-services-v5.0.md'
  - 'architecture-mvs-services-v5.0.md'
totalEpics: 2
totalStories: 16
frCoverage: '33/33 (100%)'
nfrCoverage: '10/10 (100%)'
---

# Epics & Stories: MVS System Services & SVCs

## Epic Overview

| Epic | Title | Size | Stories | Dependencies | Phase |
|------|-------|------|---------|-------------|-------|
| SYS-100 | MVS System Services Core | L | 8 | open-mainframe-dataset, open-mainframe-racf | A |
| SYS-101 | ESTAE/ESPIE Recovery & Extended Services | L | 8 | SYS-100 | A |

## Dependency Graph

```
SYS-100 (Core Services)
    │
    ▼
SYS-101 (Recovery & Extended)
```

---

## SYS-100: MVS System Services Core

**Description:** Implement the foundational MVS system services that all z/OS subsystems depend on: dynamic allocation (SVC 99), operator messaging (WTO/WTOR), task synchronization (WAIT/POST/ECB), resource serialization (ENQ/DEQ), time services, storage management, and ABEND processing.

**User Value:** Mainframe programs can dynamically allocate datasets, write operator messages, synchronize tasks, serialize resources, and terminate abnormally — the fundamental building blocks required by TSO, JES2, CICS, and all other subsystems.

### SYS-100.1: Create Crate and Core Types

**As a** system developer, **I want** the `open-mainframe-mvs` crate scaffolded with error types and core type definitions, **so that** subsequent stories have a foundation to build on.

**Acceptance Criteria:**
- Given the workspace, when `open-mainframe-mvs` is added to `Cargo.toml` members, then `cargo check -p open-mainframe-mvs` passes
- Given the crate root, when `lib.rs` is created, then it contains `#![forbid(unsafe_code)]` and re-exports all public modules
- Given the error module, when `MvsError` is defined, then it derives `Debug, thiserror::Error, miette::Diagnostic` with variants for DYNALLOC, ENQ, ABEND, program-not-found
- Given the types, when `AbendCode`, `DatasetStatus`, `Disposition`, `TaskState` enums are defined, then they derive `Debug, Clone, PartialEq, Eq, Serialize, Deserialize`
- Given `cargo clippy -p open-mainframe-mvs -- -D warnings`, then zero warnings

**FR Coverage:** NFR-MVS-007, NFR-MVS-008

### SYS-100.2: DYNALLOC Engine — Allocate and Unallocate

**As a** TSO user, **I want** to dynamically allocate and deallocate datasets by name, **so that** I can use `ALLOCATE` and `FREE` commands without JCL DD statements.

**Acceptance Criteria:**
- Given a `DynallocRequest` with verb=Allocate and dsname text unit, when `DynallocEngine::execute()` is called, then a DD entry is created in the DD table mapping the generated DDname to the dataset
- Given a `DynallocRequest` with verb=Unallocate and ddname text unit, when `execute()` is called, then the DD entry is removed from the DD table
- Given text unit keys DALDSNAM (0x0001), DALDDNAM (0x0002), DALSTATS (0x0004), DALNDISP (0x0005), DALCDISP (0x0006), when they are provided, then they are processed per IBM semantics
- Given an invalid dsname, when `execute()` is called, then `DynallocResponse` contains appropriate error_code and info_code
- Given a request with DALRTDDN (0x0056), when allocation succeeds, then the return text unit contains the assigned DDname

**FR Coverage:** FR-MVS-001, FR-MVS-002, FR-MVS-004, FR-MVS-005, NFR-MVS-001, NFR-MVS-009

### SYS-100.3: DYNALLOC — Concatenation and DD Table

**As a** batch job developer, **I want** to concatenate multiple datasets under one DDname, **so that** program search paths and input datasets can be combined.

**Acceptance Criteria:**
- Given a `DynallocRequest` with verb=Concatenate and multiple dsnames, when `execute()` is called, then a single DD entry is created with all datasets in concatenation order
- Given a `DynallocRequest` with verb=Deconcatenate, when `execute()` is called, then the concatenation is broken into individual DD entries
- Given the `DdTable`, when `lookup(ddname)` is called, then it returns the `DdEntry` with all concatenated datasets
- Given the `DdTable`, when `list()` is called, then all current DD allocations are returned
- Given concurrent access, when two tasks allocate to the DD table simultaneously, then both allocations succeed without data corruption (RwLock test)

**FR Coverage:** FR-MVS-003, NFR-MVS-006

### SYS-100.4: WTO/WTOR — Operator Messaging

**As an** operator, **I want** programs to send messages to the console with routing codes, **so that** I can monitor system activity and respond to requests.

**Acceptance Criteria:**
- Given a message text and routing code, when `wto()` is called, then a `ConsoleMessage` is sent to the console channel with correct routing and descriptor codes
- Given multiple text lines, when `wto()` is called with `is_multiline=true`, then all lines appear as a single logical message
- Given a message text and reply ECB, when `wtor()` is called, then the message is sent and the calling task waits on the ECB until a reply is provided
- Given a `ReplyToken`, when `reply(token, text)` is called, then the ECB is posted and the reply text is available to the waiting task
- Given a message ID, when `dom(id)` is called, then the message is removed from the action message queue
- Given routing codes, when routing code bits are set per IBM conventions (bit 0 = routing code 1), then only consoles subscribed to those codes see the message

**FR Coverage:** FR-MVS-006, FR-MVS-007, FR-MVS-008, FR-MVS-009, NFR-MVS-003

### SYS-100.5: WAIT/POST/ECB — Task Synchronization

**As a** system programmer, **I want** to synchronize tasks using Event Control Blocks, **so that** I can coordinate async operations using standard z/OS synchronization patterns.

**Acceptance Criteria:**
- Given a new `Ecb`, when `ecb.value()` is read, then the wait and complete bits are both 0
- Given an unposted ECB, when `wait(&ecb)` is called from a Tokio task, then the task suspends
- Given a waiting task's ECB, when `post(&ecb, completion_code)` is called, then the complete bit is set, the completion code stored in bits 2-31, and the waiting task resumes
- Given multiple ECBs, when `wait_multiple(&ecbs)` is called, then the task suspends until at least one ECB is posted, and the index of the first posted ECB is returned
- Given the ECB layout, when bits are read, then bit 0 = wait bit, bit 1 = complete bit, bits 2-31 = completion code (matching IBM layout)

**FR Coverage:** FR-MVS-010, FR-MVS-011, FR-MVS-012, NFR-MVS-004, NFR-MVS-006

### SYS-100.6: ENQ/DEQ — Resource Serialization

**As a** system programmer, **I want** to serialize access to named resources, **so that** concurrent tasks don't corrupt shared data (datasets, catalogs, spool files).

**Acceptance Criteria:**
- Given qname (1-8 chars) and rname (1-255 chars), when `enq(qname, rname, Exclusive, scope)` is called, then no other task can obtain ENQ on the same resource until DEQ
- Given two tasks both requesting SHARED ENQ on the same resource, when both call `enq()`, then both succeed concurrently
- Given one task holding SHARED and another requesting EXCLUSIVE, when the EXCLUSIVE request is made, then it blocks until the SHARED holder calls DEQ
- Given an active ENQ, when `deq(qname, rname)` is called, then the resource is released and any waiting EXCLUSIVE request is granted (FIFO)
- Given scope=STEP, when ENQ is called, then serialization applies only within the current task's context; given scope=SYSTEM, serialization is system-wide
- Given an invalid qname (>8 chars) or rname (>255 chars), when `enq()` is called, then an error is returned

**FR Coverage:** FR-MVS-013, FR-MVS-014, FR-MVS-015, FR-MVS-016, NFR-MVS-002, NFR-MVS-006

### SYS-100.7: TIME — Time-of-Day Services

**As a** programmer, **I want** to retrieve the current time and date in z/OS-compatible formats, **so that** timestamps in SMF records, logs, and program output match real z/OS conventions.

**Acceptance Criteria:**
- Given a call to `time()`, when it returns `MvsTime`, then `tod_clock` contains a 64-bit value in STCK format (bit 51 = 1 microsecond epoch from 1900-01-01)
- Given the result, when `date_packed` is read, then it contains `0CYYDDDF` packed decimal (C=century, YY=year, DDD=Julian day, F=sign)
- Given the result, when `time_binary` is read, then it contains hundredths of seconds since midnight as a 32-bit binary value
- Given the result, when `time_packed` is read, then it contains `HHMMSSTHxxxxxx` packed decimal
- Given the result, when `date_yyyymmdd` is read, then it contains Gregorian date in packed decimal format

**FR Coverage:** FR-MVS-017, NFR-MVS-010

### SYS-100.8: GETMAIN/FREEMAIN and ABEND

**As a** programmer, **I want** to allocate/free storage and trigger abnormal termination with codes, **so that** programs can manage memory and signal errors using z/OS conventions.

**Acceptance Criteria:**
- Given a subpool number and length, when `getmain(subpool, length)` is called, then a storage block is allocated and its address returned
- Given a previously allocated address, when `freemain(address, length)` is called, then the storage is returned to the subpool
- Given a user ABEND code (0-4095), when `abend(AbendCode::User(code))` is called, then the current task is terminated with `U{code:04d}` and the ABEND code is stored in the TCB
- Given a system ABEND code, when `abend(AbendCode::System(code))` is called, then the task terminates with the system code (e.g., `S0C7`)
- Given ABEND with REASON= parameter, when `abend_with_reason(code, reason)` is called, then the reason code is stored and accessible to ESTAE exits
- Given ABEND and no ESTAE exit, when ABEND occurs, then the task terminates immediately with the completion code

**FR Coverage:** FR-MVS-018, FR-MVS-019, FR-MVS-020, FR-MVS-021, NFR-MVS-005

---

## SYS-101: ESTAE/ESPIE Recovery & Extended Services

**Description:** Implement the MVS recovery framework (ESTAE/ESPIE), interval timer services (STIMER), and program management SVCs (LINK/XCTL/LOAD/DELETE/ATTACH/DETACH). These extend the core services with error recovery, scheduling, and program lifecycle management.

**User Value:** Programs can establish recovery routines for graceful error handling, set timers for timeouts and scheduling, and dynamically load and call other programs — enabling realistic z/OS application behavior.

### SYS-101.1: ESTAE — Recovery Exit Management

**As a** system programmer, **I want** to establish recovery exits that intercept ABENDs, **so that** programs can retry operations or perform cleanup before termination.

**Acceptance Criteria:**
- Given a recovery callback, when `estae(callback)` is called, then the callback is added to the ESTAE chain on the current TCB
- Given an ESTAE chain with entries, when ABEND occurs, then the most recently established ESTAE is invoked first (LIFO order)
- Given an ESTAE callback, when it receives the SDWA, then the SDWA contains: abend_code, reason_code, failing_instruction, registers
- Given the ESTAE returns `EstaeAction::Retry { address }`, when the action is processed, then execution resumes at the specified retry point
- Given the ESTAE returns `EstaeAction::Percolate`, when the action is processed, then the next ESTAE in the chain is invoked
- Given all ESTAEs percolate, when no more exits remain, then the task terminates with the original ABEND code

**FR Coverage:** FR-MVS-030, FR-MVS-031, FR-MVS-032

### SYS-101.2: ESPIE — Program Exception Handling

**As a** programmer, **I want** to intercept program exceptions (0C1, 0C4, 0C7, etc.) before they cause ABEND, **so that** programs can handle errors gracefully.

**Acceptance Criteria:**
- Given exception types [0x01, 0x04, 0x07], when `espie(types, handler)` is called, then those interrupt codes are routed to the handler instead of ABEND
- Given an ESPIE handler, when it receives the PIE, then the PIE contains: interrupt_code, failing_instruction, psw
- Given the handler returns `EspieAction::Resume { address }`, when processed, then execution continues at the specified address
- Given the handler returns `EspieAction::Abend`, when processed, then normal ABEND processing occurs (ESTAE chain invoked)
- Given ESPIE is established for 0C7 and a data exception occurs, when the interrupt fires, then the ESPIE handler is invoked before any ESTAE

**FR Coverage:** FR-MVS-033, FR-MVS-034

### SYS-101.3: STIMER/STIMERM — Interval Timer Services

**As a** programmer, **I want** to set timers that expire after a specified duration, **so that** I can implement timeouts, delays, and scheduled actions.

**Acceptance Criteria:**
- Given a duration and `StimerMode::Wait`, when `stimer(duration, mode)` is called, then the task suspends until the timer expires
- Given a duration and `StimerMode::Exit(callback)`, when `stimer(duration, mode)` is called and the timer expires, then the callback is invoked asynchronously
- Given a pending STIMER, when `stimer_cancel()` is called, then the timer is cancelled and the remaining time is returned
- Given `RealTime { hours: 0, minutes: 0, seconds: 5, hundredths: 0 }`, when STIMER is set, then the timer expires after 5 seconds (within 100ms tolerance)
- Given multiple STIMERs via `stimerm()`, when set, then each timer operates independently

**FR Coverage:** FR-MVS-035, FR-MVS-036

### SYS-101.4: LINK — Call Program with Return

**As a** programmer, **I want** to call another program by name and receive its return code, **so that** programs can compose functionality via standard z/OS program call conventions.

**Acceptance Criteria:**
- Given a program name, when `link(name, param)` is called, then the program is located via search order (STEPLIB → JOBLIB → LNKLST), loaded, and control transfers to its entry point
- Given the called program returns, when control returns to the caller, then the return code (R15) is available
- Given the called program ABENDs, when the ABEND occurs, then it percolates to the caller's ESTAE chain
- Given a program name that doesn't exist in the search order, when `link()` is called, then `MvsError::ProgramNotFound` is returned
- Given a parameter list, when `link(name, param)` is called, then the parameter is passed to the called program via the standard parameter passing convention (R1 → parameter list)

**FR Coverage:** FR-MVS-037

### SYS-101.5: XCTL — Transfer Control Without Return

**As a** programmer, **I want** to transfer control to another program without returning to the current program, **so that** I can chain programs together efficiently.

**Acceptance Criteria:**
- Given a program name, when `xctl(name, param)` is called, then the current program's storage is freed, the target program is loaded, and control transfers to it
- Given the target program was previously loaded, when `xctl()` is called, then the existing copy is used (no redundant load)
- Given a parameter list, when `xctl(name, param)` is called, then the parameter is passed to the target program
- Given the target program doesn't exist, when `xctl()` is called, then an ABEND S806 occurs (program not found)

**FR Coverage:** FR-MVS-038

### SYS-101.6: LOAD/DELETE — Module Management

**As a** system programmer, **I want** to load program modules into storage without executing them, **so that** I can obtain entry points and manage module lifecycles.

**Acceptance Criteria:**
- Given a program name, when `load(name)` is called, then the module is loaded into storage and its entry point address and length are returned
- Given a LOADed module, when `load(name)` is called again, then the use count increments and the same entry point is returned
- Given a LOADed module with use count 2, when `delete(name)` is called once, then the use count decrements to 1 and the module remains in storage
- Given a LOADed module with use count 1, when `delete(name)` is called, then the module is removed from storage
- Given a module name not found in search order, when `load(name)` is called, then ABEND S806 occurs

**FR Coverage:** FR-MVS-039, FR-MVS-040

### SYS-101.7: ATTACH/DETACH — Subtask Management

**As a** system programmer, **I want** to create and remove subtasks within an address space, **so that** programs can perform parallel processing using z/OS multi-tasking.

**Acceptance Criteria:**
- Given a program name, when `attach(name, param)` is called, then a new TCB is created as a child of the current TCB, and the program begins execution in the new task
- Given an attached subtask, when the subtask completes, then its completion code is stored in the subtask's TCB and an ECB is posted (if provided)
- Given a completed subtask's TCB token, when `detach(token)` is called, then the subtask's TCB is removed and its resources are freed
- Given DETACH on a still-running subtask, when `detach(token)` is called, then the subtask is terminated and resources freed
- Given the parent TCB, when a subtask is attached, then the subtask inherits the parent's DD table (shared reference) and ENQ scope

**FR Coverage:** FR-MVS-041, FR-MVS-042

### SYS-101.8: Integration Tests and Program Search Order

**As a** developer, **I want** comprehensive integration tests covering cross-module flows, **so that** the MVS services work correctly when used together.

**Acceptance Criteria:**
- Given a test scenario: DYNALLOC allocates a dataset, ENQ serializes it, WTO reports success, then all three services work in sequence without errors
- Given a test scenario: ATTACH subtask → subtask issues WTO → subtask ABENDs → parent's ESTAE intercepts, then the recovery chain works across task boundaries
- Given a test scenario: LINK to program → program issues STIMER WAIT → timer expires → program returns, then timer integration works
- Given `ProgramSearchOrder` configured with STEPLIB and JOBLIB datasets, when `search(name)` is called, then STEPLIB is searched first, then JOBLIB, then LNKLST
- Given `cargo test -p open-mainframe-mvs`, then all unit and integration tests pass
- Given `cargo clippy -p open-mainframe-mvs -- -D warnings`, then zero warnings

**FR Coverage:** All remaining NFRs (NFR-MVS-006 through NFR-MVS-010)

---

## FR/NFR Coverage Matrix

### Functional Requirements

| FR | Story |
|----|-------|
| FR-MVS-001 | SYS-100.2 |
| FR-MVS-002 | SYS-100.2 |
| FR-MVS-003 | SYS-100.3 |
| FR-MVS-004 | SYS-100.2 |
| FR-MVS-005 | SYS-100.2 |
| FR-MVS-006 | SYS-100.4 |
| FR-MVS-007 | SYS-100.4 |
| FR-MVS-008 | SYS-100.4 |
| FR-MVS-009 | SYS-100.4 |
| FR-MVS-010 | SYS-100.5 |
| FR-MVS-011 | SYS-100.5 |
| FR-MVS-012 | SYS-100.5 |
| FR-MVS-013 | SYS-100.6 |
| FR-MVS-014 | SYS-100.6 |
| FR-MVS-015 | SYS-100.6 |
| FR-MVS-016 | SYS-100.6 |
| FR-MVS-017 | SYS-100.7 |
| FR-MVS-018 | SYS-100.8 |
| FR-MVS-019 | SYS-100.8 |
| FR-MVS-020 | SYS-100.8 |
| FR-MVS-021 | SYS-100.8 |
| FR-MVS-030 | SYS-101.1 |
| FR-MVS-031 | SYS-101.1 |
| FR-MVS-032 | SYS-101.1 |
| FR-MVS-033 | SYS-101.2 |
| FR-MVS-034 | SYS-101.2 |
| FR-MVS-035 | SYS-101.3 |
| FR-MVS-036 | SYS-101.3 |
| FR-MVS-037 | SYS-101.4 |
| FR-MVS-038 | SYS-101.5 |
| FR-MVS-039 | SYS-101.6 |
| FR-MVS-040 | SYS-101.6 |
| FR-MVS-041 | SYS-101.7 |
| FR-MVS-042 | SYS-101.7 |

### Non-Functional Requirements

| NFR | Stories |
|-----|---------|
| NFR-MVS-001 | SYS-100.2, SYS-100.3 |
| NFR-MVS-002 | SYS-100.6 |
| NFR-MVS-003 | SYS-100.4 |
| NFR-MVS-004 | SYS-100.5 |
| NFR-MVS-005 | SYS-100.8 |
| NFR-MVS-006 | SYS-100.3, SYS-100.5, SYS-100.6, SYS-101.8 |
| NFR-MVS-007 | SYS-100.1 |
| NFR-MVS-008 | SYS-100.1 |
| NFR-MVS-009 | SYS-100.2 |
| NFR-MVS-010 | SYS-100.7 |

**Coverage: 33/33 FRs (100%), 10/10 NFRs (100%)**
