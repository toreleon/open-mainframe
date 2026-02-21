---
version: 'v5.0'
planningGroup: 'PG-1'
technology: 'MVS System Services & SVCs'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'zos-complete-inventory.md (AREA-1)'
  - 'gap-analysis/batch-22-priority-matrix.md'
epicIds: []
sysEpicIds: ['SYS-100', 'SYS-101']
---

# PRD: MVS System Services & SVCs

## 1. Problem Statement

OpenMainframe currently has no implementation of z/OS MVS system services — the fundamental kernel-level APIs that all higher-level subsystems depend on. Programs on z/OS request services via SVC (Supervisor Call) instructions, and these services (DYNALLOC, WTO/WTOR, WAIT/POST, ENQ/DEQ, ATTACH/DETACH, ESTAE/ESPIE, GETMAIN/FREEMAIN, STIMER/TIME) are invoked by virtually every z/OS program.

Without these services:
- TSO `ALLOCATE`/`FREE` commands cannot dynamically allocate datasets
- Operator console output has no programmatic interface
- Programs cannot synchronize via standard z/OS event mechanisms
- Resource serialization beyond CICS-internal ENQ is unavailable
- Task recovery (ESTAE/ESPIE) has no system-level implementation (only LE condition handling)
- Multi-tasking within an address space is unsupported

This is the single largest foundational gap in OpenMainframe — MVS system services are the "syscall layer" of z/OS.

## 2. User Personas

| Persona | Description | Key Needs |
|---------|-------------|-----------|
| Mainframe Application Developer | Writes COBOL/PL/I/REXX programs that call MVS services | DYNALLOC for dataset allocation, WTO for messages, ENQ for serialization |
| System Programmer | Writes assembler programs using authorized SVCs | ESTAE/ESPIE for recovery, ATTACH/DETACH for subtasking, GETMAIN for storage |
| TSO/ISPF User | Uses TSO commands that invoke SVCs internally | ALLOCATE/FREE (DYNALLOC), SEND (WTO), LISTDS (ENQ for catalog) |
| Batch Job Developer | Submits JCL that triggers SVC processing | TIME for timestamps, WAIT/POST for I/O sync, ABEND for error signaling |

## 3. Scope

### MVP (SYS-100 — Phase A Foundation)
- DYNALLOC (SVC 99): Allocate/deallocate/concatenate datasets programmatically
- WTO/WTOR: Write operator messages and receive replies
- WAIT/POST: Task synchronization event control blocks (ECBs)
- ENQ/DEQ: System-level resource serialization (qname/rname)
- TIME: Time-of-day and date retrieval
- GETMAIN/FREEMAIN: Storage allocation (emulated via Rust allocator)
- ABEND (SVC 13): Abnormal task termination with user/system codes

### Growth (SYS-101 — Phase A Foundation)
- ESTAE/ESPIE: Recovery exit establishment and program exception handling
- STIMER/STIMERM: Interval timer services
- LINK/XCTL/LOAD/DELETE: Program management SVCs (load, call, transfer)
- ATTACH/DETACH: Subtask creation and removal

### Deferred
- Cross-memory services (AXRES/LXRES/ETDEF/PC/PT) — Only needed for multi-address-space emulation
- Data spaces / hiperspaces — Only for DB2 buffer pool scenarios
- CVT and full control block chain — Only for system-level diagnostic tools
- EXCP/XDAP — Hardware I/O; abstracted by host OS
- MODESET/TESTAUTH — Authorization mode changes

## 4. Functional Requirements

### SYS-100: MVS System Services Core

| ID | Requirement | Acceptance Criteria | Source |
|----|-------------|---------------------|--------|
| FR-MVS-001 | DYNALLOC shall allocate a dataset by dsname, returning a DDname | Given a valid dsname, when DYNALLOC verb 01 is invoked with dsname text unit, then a DD is created mapping to the dataset and the DDname is returned in the text unit | AREA-1 Gap #1 |
| FR-MVS-002 | DYNALLOC shall deallocate a dataset by DDname | Given an allocated DDname, when DYNALLOC verb 02 is invoked, then the DD mapping is removed and resources freed | AREA-1 Gap #1 |
| FR-MVS-003 | DYNALLOC shall support concatenation of multiple datasets to a single DDname | Given multiple dsnames, when DYNALLOC verb 03 is invoked, then datasets are concatenated in order under one DDname | AREA-1 Gap #1 |
| FR-MVS-004 | DYNALLOC shall return z/OS-compatible error reason codes (S99ERROR/S99INFO) | Given an invalid dsname, when DYNALLOC is invoked, then S99ERROR contains reason code and S99INFO contains info code matching IBM-defined values | AREA-1 Gap #1 |
| FR-MVS-005 | DYNALLOC shall support text unit keys: DALDSNAM, DALDDNAM, DALSTATS, DALNDISP, DALCDISP, DALUNIT, DALVLSER | When any of these text unit keys are provided, DYNALLOC processes them per IBM-defined semantics | AREA-1 Gap #1 |
| FR-MVS-006 | WTO shall write a message to the operator console with routing codes | Given a message text and routing code, when WTO is invoked, then the message appears in the console message log with correct routing | AREA-1 Gap #2 |
| FR-MVS-007 | WTO shall support multi-line messages (WTO with MLWTO parameter) | Given multiple text lines, when multi-line WTO is invoked, then all lines appear as a single logical message with the same message ID | AREA-1 Gap #2 |
| FR-MVS-008 | WTOR shall write a message and wait for an operator reply | Given a message text, when WTOR is invoked, then the system waits until a REPLY command is issued and returns the reply text | AREA-1 Gap #2 |
| FR-MVS-009 | DOM shall delete a previously written operator message | Given a message ID from WTO, when DOM is invoked, then the message is removed from the action message queue | AREA-1 Gap #2 |
| FR-MVS-010 | WAIT shall suspend a task until an ECB is posted | Given an unposted ECB, when WAIT is invoked, then the calling task is suspended until POST sets the ECB complete bit | AREA-1 Gap #4 |
| FR-MVS-011 | POST shall signal event completion by setting an ECB | Given a waiting task's ECB address, when POST is invoked, then the ECB complete bit is set and the waiting task becomes dispatchable | AREA-1 Gap #4 |
| FR-MVS-012 | EVENTS shall wait for any one of multiple ECBs to be posted | Given a list of ECBs, when EVENTS is invoked, then the task is suspended until at least one ECB is posted, and the completed ECB is identified | AREA-1 Gap #4 |
| FR-MVS-013 | ENQ shall enqueue on a resource identified by qname (1-8 chars) and rname (1-255 chars) | Given qname and rname, when ENQ is invoked with EXCLUSIVE scope, then no other ENQ with same qname/rname succeeds until DEQ | AREA-1 Gap #5 |
| FR-MVS-014 | ENQ shall support SHARED and EXCLUSIVE lock modes | Given a SHARED ENQ, when another task requests SHARED on same resource, then both succeed; when EXCLUSIVE is requested, the second task waits | AREA-1 Gap #5 |
| FR-MVS-015 | DEQ shall release a previously enqueued resource | Given an active ENQ, when DEQ is invoked with same qname/rname, then the resource is released and waiting tasks are dispatched | AREA-1 Gap #5 |
| FR-MVS-016 | ENQ shall support scope levels: STEP, SYSTEM | When ENQ scope=STEP, serialization applies within the address space; when scope=SYSTEM, it applies system-wide | AREA-1 Gap #5 |
| FR-MVS-017 | TIME shall return current date and time in multiple formats | When TIME is invoked, it returns: TOD clock (STCK format), date (packed decimal CCYYDDD+), time (packed decimal HHMMSSTH), and binary formats | AREA-1 Gap #6 |
| FR-MVS-018 | GETMAIN shall allocate virtual storage in a specified subpool | Given a length and subpool number, when GETMAIN is invoked, then storage of the requested size is allocated and the address returned | AREA-1 SVC 4/5/10/120 |
| FR-MVS-019 | FREEMAIN shall release previously allocated virtual storage | Given an address and length from GETMAIN, when FREEMAIN is invoked, then the storage is freed to the subpool | AREA-1 SVC 4/5/10/120 |
| FR-MVS-020 | ABEND shall terminate the current task with a user or system completion code | Given a user code (0-4095) or system code (hex), when ABEND is invoked, then the task terminates with the specified code and optional dump | AREA-1 SVC 13 |
| FR-MVS-021 | ABEND shall support REASON= parameter for extended reason codes | When ABEND is invoked with REASON=, the reason code is stored in the SDWA and available to ESTAE exits | AREA-1 SVC 13 |

### SYS-101: ESTAE/ESPIE Recovery Framework

| ID | Requirement | Acceptance Criteria | Source |
|----|-------------|---------------------|--------|
| FR-MVS-030 | ESTAE shall establish a recovery exit routine for the current task | Given a recovery routine address, when ESTAE is invoked, then subsequent ABENDs invoke the recovery routine before task termination | AREA-1 Gap #7 |
| FR-MVS-031 | ESTAE exit shall receive an SDWA (System Diagnostic Work Area) | When the ESTAE exit receives control, the SDWA contains: ABEND code, PSW, registers, failing instruction address | AREA-1 Gap #7 |
| FR-MVS-032 | ESTAE exit shall be able to RETRY (continue execution) or PERCOLATE (pass to next exit) | When SETRP RC=4 is set in ESTAE exit, execution resumes at retry address; when RC=0, the ABEND percolates to the next exit in the chain | AREA-1 Gap #7 |
| FR-MVS-033 | ESPIE shall establish an exit for program exceptions (0Cx interrupts) | Given exception types (0C1, 0C4, 0C7, etc.), when ESPIE is invoked, then those program interrupts invoke the ESPIE exit instead of ABEND | AREA-1 Gap #7 |
| FR-MVS-034 | ESPIE exit shall receive a PIE (Program Interruption Element) | When ESPIE exit gets control, the PIE contains: interrupt code, PSW, failing instruction address | AREA-1 Gap #7 |
| FR-MVS-035 | STIMER shall set an interval timer that expires after a specified duration | Given a time interval in binary or clock format, when STIMER is invoked with WAIT, the task suspends until the timer expires | AREA-1 Gap #6 |
| FR-MVS-036 | STIMER with EXIT= shall invoke an exit routine when the timer expires | Given EXIT= parameter, when the timer expires, the specified exit routine receives control asynchronously | AREA-1 Gap #6 |
| FR-MVS-037 | LINK shall load a program module and transfer control, expecting return | Given a program name, when LINK is invoked, the named program is loaded (if not resident), control transfers to its entry point, and on return, the return code is available in R15 | AREA-1 SVC 6 |
| FR-MVS-038 | XCTL shall transfer control to a program without return | Given a program name, when XCTL is invoked, the current program's storage is freed and control transfers to the named program's entry point | AREA-1 SVC 7 |
| FR-MVS-039 | LOAD shall load a program module into storage without transferring control | Given a program name, when LOAD is invoked, the module is loaded and its entry point and length are returned | AREA-1 SVC 8 |
| FR-MVS-040 | DELETE shall remove a previously LOADed module from storage | Given a program name, when DELETE is invoked, the module's use count decrements and storage is freed when count reaches zero | AREA-1 SVC 9 |
| FR-MVS-041 | ATTACH shall create a subtask under the current task | Given an entry point or program name, when ATTACH is invoked, a new TCB is created as a subtask, dispatched independently | AREA-1 Gap #3 |
| FR-MVS-042 | DETACH shall remove a subtask and free its resources | Given a subtask TCB token from ATTACH, when DETACH is invoked, the subtask is terminated and its resources freed | AREA-1 Gap #3 |

## 5. Non-Functional Requirements

| ID | Requirement | Threshold | Source |
|----|-------------|-----------|--------|
| NFR-MVS-001 | DYNALLOC processing time | < 5ms per allocation/deallocation call (excluding I/O) | Performance |
| NFR-MVS-002 | ENQ/DEQ contention resolution | Fair ordering — tasks enqueue in FIFO order when contending for same resource | Compatibility |
| NFR-MVS-003 | WTO message format compatibility | Messages formatted per z/OS conventions: IEE/IEF prefixes, routing codes 1-16, descriptor codes 1-13 | Compatibility |
| NFR-MVS-004 | ECB format compatibility | ECB layout: bit 0 = wait bit, bit 1 = complete bit, bits 2-31 = completion code (matching IBM layout) | Compatibility |
| NFR-MVS-005 | ABEND code compatibility | System ABEND codes (0Cx, Bxx, Dxx, Exx) and user ABEND codes (U0001-U4095) match IBM conventions | Compatibility |
| NFR-MVS-006 | Thread safety | All MVS services must be safe for concurrent invocation from multiple Tokio tasks | Reliability |
| NFR-MVS-007 | No unsafe code | `#![forbid(unsafe_code)]` in the crate root | Code quality |
| NFR-MVS-008 | Error diagnostics | All errors use thiserror + miette with diagnostic codes | Code quality |
| NFR-MVS-009 | DYNALLOC text unit compatibility | Text unit key values match IBM-defined numeric keys (e.g., DALDSNAM = 0x0001, DALDDNAM = 0x0002) | Compatibility |
| NFR-MVS-010 | TIME format compatibility | TOD clock format, packed decimal date/time, and 4-byte binary time must match IBM layouts | Compatibility |

## 6. Dependencies

| Dependency | Direction | Description |
|------------|-----------|-------------|
| open-mainframe-dataset | MVS → Dataset | DYNALLOC needs catalog lookup and DD allocation |
| open-mainframe-jes2 | MVS → JES2 | WTO/WTOR routes messages through JES2 console |
| open-mainframe-racf | MVS → RACF | ENQ may check RACF FACILITY class; DYNALLOC checks dataset access |
| open-mainframe-le | LE → MVS | LE condition handling (CEEHDLR) built on top of ESTAE/ESPIE |
| open-mainframe-tso | TSO → MVS | TSO ALLOCATE/FREE invoke DYNALLOC; SEND invokes WTO |
| open-mainframe-cics | CICS → MVS | CICS task management wraps ATTACH/DETACH concepts |
