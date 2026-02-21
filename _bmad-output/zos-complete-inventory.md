# z/OS Complete Component Inventory for OpenMainframe

> Research conducted 2026-02-20 using IBM z/OS 3.1/3.2 documentation, IBM Knowledge Center, Redbooks, and community references.

---

## AREA-1: MVS System Services & Supervisor Call Instructions (SVCs)

### Overview

z/OS MVS system services form the kernel of the operating system. Programs request services via SVC (Supervisor Call) instructions (opcode 0x0A, operand 0-255), PC (Program Call) routines, or callable services. These are the fundamental building blocks that all higher-level subsystems depend on.

**IBM References:**
- z/OS MVS Programming: Assembler Services Guide (SA23-1368)
- z/OS MVS Programming: Assembler Services Reference ALE-DYN (SA23-1369)
- z/OS MVS Programming: Assembler Services Reference ERR-ITS (SA23-1370)
- z/OS MVS Programming: Authorized Assembler Services Guide (SA23-1371)
- z/OS MVS Programming: Authorized Assembler Services Reference, Vols 1-4 (SA23-1372 through SA23-1375)
- z/OS MVS Programming: Extended Addressability Guide (SA23-1394)
- z/OS MVS Diagnosis: Reference (GA32-0904) — contains complete SVC table

### Components Inventory

#### SVC Instructions (Key System SVCs)

| SVC # | Macro(s) | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-------|----------|------|-------------|-------------------|-----------------|----------|
| 0 | EXCP/XDAP | 2 | Execute Channel Program (physical I/O) | NO | N/A | LOW |
| 1 | WAIT/WAITR/PRTOV | 1 | Wait for event completion | NO | N/A | HIGH |
| 2 | POST | 1 | Signal event completion | NO | N/A | HIGH |
| 3 | EXIT/RETURN | 1 | Task termination | NO | N/A | HIGH |
| 4 | GETMAIN (R,LOC=BELOW) | 1 | Allocate storage below 16MB | PARTIAL (CICS) | Batch 12 | HIGH |
| 5 | FREEMAIN (R,LOC=BELOW) | 1 | Free storage below 16MB | PARTIAL (CICS) | Batch 12 | HIGH |
| 6 | LINK/LINKX | 2 | Load and pass control to program | PARTIAL (CICS/REXX) | N/A | HIGH |
| 7 | XCTL/XCTLX | 2 | Transfer control without return | PARTIAL (CICS) | N/A | HIGH |
| 8 | LOAD | 2 | Load program module into storage | NO | N/A | MEDIUM |
| 9 | DELETE | 2 | Delete loaded module from storage | NO | N/A | MEDIUM |
| 10 | GETMAIN/FREEMAIN (variable) | 1 | Variable-length storage management | PARTIAL (CICS) | Batch 12 | HIGH |
| 11 | TIME | 1 | Obtain time of day and date | PARTIAL (LE) | Batch 12 | MEDIUM |
| 12 | SYNCH/SYNCHX | 2 | Synchronous program call | NO | N/A | LOW |
| 13 | ABEND | 1 | Abnormal end of task | PARTIAL (CICS) | N/A | HIGH |
| 14 | SPIE | 1 | Set Program Interrupt Exit | NO | Batch 12 | MEDIUM |
| 15 | ERREXCP | 1 | Error recovery for EXCP | NO | N/A | LOW |
| 16 | PURGE | 1 | Purge I/O request | NO | N/A | LOW |
| 17 | RESTORE | 1 | Restore interrupted I/O | NO | N/A | LOW |
| 18 | BLDL | 2 | Build directory list for PDS | PARTIAL (dataset) | Batch 19 | MEDIUM |
| 19 | OPEN | 2 | Open dataset (DCB) | PARTIAL (dataset) | Batch 19 | HIGH |
| 20 | CLOSE | 2 | Close dataset (DCB) | PARTIAL (dataset) | Batch 19 | HIGH |
| 21 | STOW | 2 | Update PDS directory | PARTIAL (dataset) | Batch 19 | MEDIUM |
| 22 | OPEN TYPE=J | 2 | Open for job-level resources | NO | N/A | LOW |
| 23 | CLOSE TYPE=T | 2 | Temporary close | NO | N/A | LOW |
| 24 | DEVTYPE | 2 | Get device characteristics | NO | N/A | MEDIUM |
| 25 | TGET/TPUT | 1 | Terminal I/O (TSO) | PARTIAL (TSO) | Batch 9 | HIGH |
| 26 | CATALOG/LOCATE | 2 | Catalog services | PARTIAL (dataset) | Batch 19 | HIGH |
| 27 | OBTAIN | 2 | Read VTOC DSCB | NO | Batch 19 | MEDIUM |
| 29 | SCRATCH | 2 | Delete dataset from volume | NO | Batch 21 | MEDIUM |
| 30 | RENAME | 2 | Rename dataset on volume | NO | Batch 21 | MEDIUM |
| 33 | CHECK | 1 | Check I/O completion (BSAM) | PARTIAL (dataset) | Batch 19 | MEDIUM |
| 34 | WTO | 2 | Write to Operator | NO | N/A | HIGH |
| 35 | WTO (multi-line) | 2 | Multi-line Write to Operator | NO | N/A | HIGH |
| 36 | WTOR | 2 | Write to Operator with Reply | NO | N/A | HIGH |
| 37 | SEGLD/SEGWT | 2 | Segment load/wait | NO | N/A | LOW |
| 42 | ATTACH/ATTACHX | 6 | Create subtask | NO | N/A | HIGH |
| 43 | EXTRACT | 2 | Extract TCB info | NO | N/A | MEDIUM |
| 44 | IDENTIFY | 2 | Add entry to load module | NO | N/A | LOW |
| 46 | DETACH | 2 | Remove subtask | NO | N/A | HIGH |
| 47 | CHKPT | 2 | Checkpoint (restart) | NO | Batch 19 | LOW |
| 48 | ENQ | 2 | Enqueue on resource | PARTIAL (CICS) | N/A | HIGH |
| 49 | DEQ | 2 | Dequeue resource | PARTIAL (CICS) | N/A | HIGH |
| 50 | RESERVE | 2 | Reserve DASD volume | NO | N/A | MEDIUM |
| 51 | STIMER | 1 | Set interval timer | NO | N/A | MEDIUM |
| 52 | STIMERM | 2 | Set multiple interval timers | NO | N/A | MEDIUM |
| 53 | DEQ (shared) | 2 | Dequeue shared resources | NO | N/A | MEDIUM |
| 54 | SETLOCK | 1 | Set/release system locks | NO | N/A | LOW |
| 56 | STAE/STAI | 2 | Establish abnormal exit | NO | Batch 12 | HIGH |
| 60 | ESTAE | 6 | Extended STAE | NO | Batch 12 | HIGH |
| 78 | LSPACE | 2 | Obtain space info for volume | NO | Batch 19 | MEDIUM |
| 99 | DYNALLOC | 6 | Dynamic allocation (SVC 99) | NO | N/A | HIGH |
| 107 | MODESET | 1 | Set PSW key/mode | NO | N/A | LOW |
| 109 | (Router SVC Type 3) | 1 | Routes to Type 3 SVCs | NO | N/A | LOW |
| 116 | (Router SVC Type 1) | 1 | Routes to Type 1 SVCs | NO | N/A | LOW |
| 117 | DEBCHK | 1 | Data extent block check | NO | N/A | LOW |
| 119 | TESTAUTH | 1 | Test APF authorization | NO | N/A | LOW |
| 120 | GETMAIN/FREEMAIN (31-bit) | 1 | 31-bit storage management | NO | Batch 12 | HIGH |
| 122 | (Router SVC Type 2) | 1 | Routes to Type 2 SVCs | NO | N/A | LOW |
| 137 | (Router SVC Type 6) | 1 | Routes to Type 6 SVCs | NO | N/A | LOW |

#### System Macros (Non-SVC or PC-based)

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| STORAGE OBTAIN/RELEASE | Macro | Modern storage management (replaces GETMAIN/FREEMAIN) | PARTIAL (CICS) | Batch 12 | HIGH |
| WTL | Macro | Write to Log | NO | N/A | LOW |
| DOM | Macro | Delete Operator Message | NO | N/A | LOW |
| SCHEDULE/IEAMSCHD | Macro | Schedule SRB | NO | N/A | LOW |
| SETFRR | Macro | Set Functional Recovery Routine | NO | N/A | LOW |
| SETRP | Macro | Set return parameters (recovery) | NO | N/A | LOW |
| ESPIE | Macro | Set program interrupt exit (ESA) | NO | Batch 12 | MEDIUM |
| ESTAE | Macro | Set task abnormal exit (ESA) | NO | Batch 12 | HIGH |
| EVENTS | Macro | Multiple event synchronization | NO | N/A | MEDIUM |
| CPOOL | Macro | Cell pool management | NO | N/A | MEDIUM |
| ALESERV | Macro | Access List Entry services | NO | N/A | LOW |
| SSAR | Instruction | Set Secondary ASN Register | NO | N/A | LOW |
| PC | Instruction | Program Call | NO | N/A | LOW |
| PT | Instruction | Program Transfer | NO | N/A | LOW |
| SAC | Instruction | Set Address Space Control | NO | N/A | LOW |

#### Cross-Memory Services

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| AXRES | Macro | Reserve Authorization Index | NO | N/A | LOW |
| AXSET | Macro | Set Authorization Index | NO | N/A | LOW |
| AXEXT | Macro | Extract Authorization Index | NO | N/A | LOW |
| ATSET | Macro | Set Authority Table entry | NO | N/A | LOW |
| LXRES | Macro | Reserve Linkage Index | NO | N/A | LOW |
| LXFRE | Macro | Free Linkage Index | NO | N/A | LOW |
| ETDEF | Macro | Define Entry Table | NO | N/A | LOW |
| ETCRE | Macro | Create Entry Table | NO | N/A | LOW |
| ETCON | Macro | Connect Entry Table | NO | N/A | LOW |
| ETDIS | Macro | Disconnect Entry Table | NO | N/A | LOW |
| ETDES | Macro | Destroy Entry Table | NO | N/A | LOW |

#### Storage Management (Address Space Layout)

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| CSA (Common Service Area) | Region | Shared storage accessible by all address spaces | NO | N/A | MEDIUM |
| SQA (System Queue Area) | Region | System queue storage (non-pageable) | NO | N/A | LOW |
| LSQA (Local SQA) | Region | Local system queue area per address space | NO | N/A | LOW |
| Private Area | Region | User region per address space | NO | N/A | MEDIUM |
| Subpool Management | Service | Storage subpools (0-255) with different attributes | NO | Batch 12 | MEDIUM |
| Data Spaces | Service | Additional 2GB data-only address spaces | NO | N/A | LOW |
| Hiperspaces | Service | High-performance data spaces using expanded storage | NO | N/A | LOW |

#### Task Management (Dispatchable Units)

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| TCB (Task Control Block) | Control Block | Represents a task/thread | NO (referenced in SMF) | N/A | HIGH |
| SRB (Service Request Block) | Control Block | Represents asynchronous service request | NO (referenced in SMF) | N/A | LOW |
| IRB (Interrupt Request Block) | Control Block | Represents interrupt-driven work | NO | N/A | LOW |
| RB Chain (Request Block) | Control Block | Chain of active program requests | NO | N/A | MEDIUM |
| Address Space | Concept | Virtual address space (2GB or 16EB) | PARTIAL (conceptual) | N/A | HIGH |
| Enclave | Concept | WLM work unit spanning address spaces | NO | Batch 17 | MEDIUM |
| ASCB | Control Block | Address Space Control Block | NO | N/A | MEDIUM |
| CVT | Control Block | Communication Vector Table | NO | N/A | MEDIUM |

#### Callable Services (PC-based, modern interface)

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| CSVQUERY | Callable | Query program attributes | NO | N/A | MEDIUM |
| CSVDYNEX | Callable | Dynamic exit facility | NO | N/A | LOW |
| CSVDYLPA | Callable | Dynamic LPA management | NO | N/A | LOW |
| ISGLCRT/ISGLOBT/ISGLREL/ISGLPRG | Callable | Latch Manager services | NO | N/A | LOW |
| ISGQUERY | Callable | GRS query services | NO | N/A | LOW |
| IEEMB143/IEANTCR/IEANTRT | Callable | Name/Token services | NO | N/A | LOW |

### New Gaps Discovered (not in existing gap analyses)

1. **DYNALLOC (SVC 99) — Dynamic Dataset Allocation**
   - Classification: SVC/system_service
   - Description: Programmatic equivalent of JCL DD statements. Allows programs to allocate/deallocate datasets at runtime. Used extensively by TSO commands (ALLOCATE/FREE), ISPF, batch programs, and compilers.
   - IBM Reference: z/OS MVS Programming: Authorized Assembler Services Guide (SA23-1371)
   - Complexity: L
   - Priority: HIGH — TSO ALLOCATE/FREE commands depend on it; ISPF dataset allocation depends on it
   - Dependencies: Dataset catalog, volume management

2. **WTO/WTOR/DOM — Operator Communication Services**
   - Classification: SVC/system_service
   - Description: Write messages to operator console, write with reply, delete messages. Foundation of all operator communication and automation.
   - IBM Reference: z/OS MVS Programming: Assembler Services Guide (SA23-1368)
   - Complexity: M
   - Priority: HIGH — Console commands, JES2 messages, system automation all depend on WTO
   - Dependencies: Console subsystem

3. **ATTACH/DETACH — Task (Subtask) Management**
   - Classification: SVC/system_service
   - Description: Create and remove subtasks within an address space. Foundation for multi-tasking within a job step.
   - IBM Reference: z/OS MVS Programming: Assembler Services Reference (SA23-1369)
   - Complexity: L
   - Priority: HIGH — Many subsystems use subtasking (CICS, DB2, IMS)
   - Dependencies: TCB management, storage management

4. **WAIT/POST/EVENTS — Task Synchronization**
   - Classification: SVC/system_service
   - Description: Inter-task synchronization primitives. WAIT suspends a task until an event completes. POST signals event completion. EVENTS waits on multiple events.
   - IBM Reference: z/OS MVS Programming: Assembler Services Guide (SA23-1368)
   - Complexity: M
   - Priority: HIGH — Fundamental to all async I/O and inter-task communication
   - Dependencies: TCB management

5. **ENQ/DEQ/RESERVE — Resource Serialization (System-Level)**
   - Classification: SVC/system_service
   - Description: System-level resource serialization. ENQ/DEQ for intra-system; RESERVE for DASD volume-level. Managed by GRS (Global Resource Serialization) in a sysplex.
   - IBM Reference: z/OS MVS Programming: Authorized Assembler Services Guide (SA23-1371)
   - Complexity: M (ENQ/DEQ), L (GRS)
   - Priority: HIGH — Dataset integrity, catalog serialization, spool access all depend on ENQ
   - Dependencies: GRS for sysplex scope
   - Note: CICS has its own ENQ/DEQ but system-level is separate

6. **STIMER/STIMERM/TIME — Timer and Time Services**
   - Classification: SVC/system_service
   - Description: Interval timing (STIMER), multiple timers (STIMERM), time-of-day retrieval (TIME). Used for timeouts, scheduling, and timestamps.
   - IBM Reference: z/OS MVS Programming: Assembler Services Guide (SA23-1368)
   - Complexity: S
   - Priority: MEDIUM — Used throughout the system but often abstracted by LE services
   - Dependencies: None

7. **ESTAE/ESPIE — Recovery and Exception Handling**
   - Classification: SVC/system_service
   - Description: ESTAE establishes a recovery exit for abnormal termination. ESPIE handles program exceptions (0C1-0CF). Foundation of all z/OS error recovery.
   - IBM Reference: z/OS MVS Programming: Assembler Services Guide (SA23-1368)
   - Complexity: L
   - Priority: HIGH — LE condition handling built on top of ESTAE/ESPIE
   - Dependencies: LE condition handling (Batch 12) extends these
   - Note: Partially covered in Batch 12 but not as standalone system services

8. **Cross-Memory Services (AXRES/AXSET/LXRES/ETDEF/ETCRE/ETCON/PC/PT/SSAR)**
   - Classification: system_service
   - Description: Allow programs in one address space to call routines or access data in another address space. Used by DB2, CICS, JES2, GRS, and other subsystems.
   - IBM Reference: z/OS MVS Programming: Extended Addressability Guide (SA23-1394)
   - Complexity: XL
   - Priority: LOW — Only needed for true multi-address-space emulation
   - Dependencies: Address space management

9. **Data Spaces and Hiperspaces**
   - Classification: system_service
   - Description: Additional virtual storage areas for data (not executable). Data spaces accessible via AR mode; hiperspaces via HSPSERV macro.
   - IBM Reference: z/OS MVS Programming: Extended Addressability Guide (SA23-1394)
   - Complexity: L
   - Priority: LOW — Used for large data caching (DB2 buffer pools, CICS data tables)
   - Dependencies: AR mode support

10. **CVT and System Control Blocks**
    - Classification: system_infrastructure
    - Description: Communication Vector Table (CVT) is the root control block of z/OS. Contains pointers to all major system tables: SVC table, TCB chain, ASCB chain, UCB chain, etc.
    - IBM Reference: z/OS MVS Data Areas (GA32-0853)
    - Complexity: XL
    - Priority: MEDIUM — Many system programs navigate via CVT; needed for full system emulation
    - Dependencies: All system services

### Recommendations for OpenMainframe

**Immediate value (implement now):**
- DYNALLOC (SVC 99) — needed for TSO ALLOCATE/FREE and ISPF
- WTO/WTOR — needed for console command output and operator communication
- ENQ/DEQ at system level — needed for dataset integrity beyond CICS
- WAIT/POST — needed for async operations beyond Tokio

**Deferred (implement when needed):**
- ATTACH/DETACH — only if true multi-tasking within address space is needed
- ESTAE/ESPIE — can continue using LE condition handling as proxy
- Cross-memory services — only if multi-address-space model is implemented
- Data spaces/hiperspaces — only for DB2 buffer pool or large data caching scenarios

**Not needed (skip or stub):**
- EXCP/XDAP — hardware-level I/O; OpenMainframe uses host OS I/O
- MODESET/TESTAUTH — authorization model can be simulated differently
- Router SVCs — implementation detail of SVC dispatching
- Physical I/O SVCs — abstracted away by Rust's std::fs and tokio

---

## AREA-2: z/OS System Commands & Operator Console

### Overview

z/OS operator commands control the running system from the operator console. Commands are organized by subsystem: MVS system commands, JES2 commands, and subsystem-specific commands (VTAM, TCP/IP, etc.). The console subsystem handles command routing, message processing, and automation.

**IBM References:**
- z/OS MVS System Commands (SA38-0666)
- z/OS JES2 Commands (SA32-0994)
- z/OS JES2 Messages (SA32-0996)

### Components Inventory

#### MVS System Commands

| Command | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|---------|-------------|-------------------|-----------------|----------|
| DISPLAY (D) A,L | Display active address spaces | NO | N/A | HIGH |
| DISPLAY ASM | Display auxiliary storage (paging) | NO | N/A | LOW |
| DISPLAY C,K | Display console status | NO | N/A | MEDIUM |
| DISPLAY ETR | Display external time reference | NO | N/A | LOW |
| DISPLAY GRS | Display Global Resource Serialization | NO | N/A | MEDIUM |
| DISPLAY IOS | Display I/O supervisor | NO | N/A | LOW |
| DISPLAY J,jobname | Display job information | PARTIAL (z/OSMF) | Batch 11 | HIGH |
| DISPLAY M | Display system memory | NO | N/A | MEDIUM |
| DISPLAY NET | Display VTAM network info | NO | Batch 20 | LOW |
| DISPLAY OMVS | Display UNIX System Services | NO | Batch 18 | MEDIUM |
| DISPLAY PROD | Display product registration | NO | N/A | LOW |
| DISPLAY R,L | Display operator reply queue | NO | N/A | MEDIUM |
| DISPLAY SMF | Display SMF status | NO | Batch 14 | LOW |
| DISPLAY SMS | Display SMS configuration | NO | Batch 19 | MEDIUM |
| DISPLAY SR | Display system resources | NO | N/A | LOW |
| DISPLAY T | Display date, time, timezone | NO | N/A | MEDIUM |
| DISPLAY U | Display device/unit status | NO | N/A | MEDIUM |
| DISPLAY WLM | Display Workload Manager | NO | Batch 17 | MEDIUM |
| DISPLAY XCF | Display cross-coupling facility | NO | N/A | LOW |
| START (S) | Start address space / started task | NO | N/A | HIGH |
| STOP (P) | Stop address space | NO | N/A | HIGH |
| MODIFY (F) | Modify running address space | NO | N/A | HIGH |
| CANCEL (C) | Cancel address space | PARTIAL (JES2) | Batch 11 | HIGH |
| FORCE | Force termination | NO | N/A | MEDIUM |
| HALT | Halt system | NO | N/A | LOW |
| QUIESCE | Quiesce system | NO | N/A | LOW |
| REPLY (R) | Reply to WTOR | NO | N/A | HIGH |
| ROUTE | Route command to system | NO | N/A | LOW |
| SET | Set system parameters | NO | N/A | MEDIUM |
| SETPROG | Set program properties (APF, LNKLST) | NO | N/A | MEDIUM |
| SETOMVS | Set UNIX System Services parameters | NO | Batch 18 | LOW |
| VARY | Vary device/path online/offline | NO | N/A | MEDIUM |
| WRITELOG | Write to system log | NO | N/A | LOW |
| CONTROL | Set console attributes | NO | N/A | LOW |

#### JES2 Commands

| Command | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|---------|-------------|-------------------|-----------------|----------|
| $DA | Display active jobs | PARTIAL (z/OSMF) | Batch 11 | HIGH |
| $DI | Display initiators | NO | Batch 11 | MEDIUM |
| $DJ | Display job queue | PARTIAL (z/OSMF) | Batch 11 | HIGH |
| $DL | Display JES2 lines | NO | Batch 11 | LOW |
| $DN | Display nodes (NJE) | NO | Batch 11 | LOW |
| $DO | Display output queue | NO | Batch 11 | MEDIUM |
| $DP | Display printers | NO | Batch 11 | LOW |
| $DQ | Display held output queues | NO | Batch 11 | MEDIUM |
| $DR | Display RJE readers | NO | Batch 11 | LOW |
| $DU | Display device status | NO | Batch 11 | LOW |
| $DSPL | Display spool volumes | NO | Batch 11 | MEDIUM |
| $S | Start device/initiator | NO | Batch 11 | MEDIUM |
| $P | Stop/purge (drain) | PARTIAL (z/OSMF) | Batch 11 | MEDIUM |
| $C | Cancel job | PARTIAL (z/OSMF) | Batch 11 | HIGH |
| $A | Activate/release | PARTIAL (z/OSMF) | Batch 11 | MEDIUM |
| $T | Modify JES2 parameters | NO | Batch 11 | MEDIUM |
| $VS | Vary spool | NO | Batch 11 | LOW |
| $HASP | JES2 status display | NO | Batch 11 | LOW |

#### SDSF (System Display and Search Facility)

| Component | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|-------------|-------------------|-----------------|----------|
| SDSF DA panel | Display active users/jobs | NO | N/A | HIGH |
| SDSF ST panel | Display job status | NO | N/A | HIGH |
| SDSF O/H panels | Display output/held output | NO | N/A | HIGH |
| SDSF LOG panel | Display system log | NO | N/A | MEDIUM |
| SDSF REXX interface | ISFEXEC, ISFCALLS | NO | N/A | MEDIUM |
| SDSF line commands | S/SJ/SE/SP/SB/? | NO | N/A | MEDIUM |
| SDSF action characters | // //* | NO | N/A | LOW |

#### Console Types & Message Processing

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| MCS Console | Service | Multiple Console Support — operator console | NO | N/A | MEDIUM |
| SMCS Console | Service | System MCS Console | NO | N/A | LOW |
| Extended MCS Console | Service | Extended console for programs (MCSOPER) | NO | N/A | MEDIUM |
| Subsystem Console | Service | Subsystem-specific console | NO | N/A | LOW |
| MPF (Message Processing Facility) | Service | Automation — match messages to commands | NO | N/A | MEDIUM |
| EMCS (Extended MCS) | API | API for programmatic console access | NO | N/A | MEDIUM |
| Hardcopy Log | Service | SYSLOG/OPERLOG permanent message record | NO | Batch 14 | MEDIUM |
| Message Routing | Service | Route codes, descriptor codes, message IDs | NO | N/A | MEDIUM |

### New Gaps Discovered

1. **SDSF — System Display and Search Facility**
   - Classification: system_program/ISPF_application
   - Description: The primary interface mainframe operators and programmers use to view job status, output, and system activity. Provides DA (active), ST (status), O (output), H (held output), LOG, and many other panels. Also has a REXX API (ISFEXEC/ISFCALLS).
   - IBM Reference: z/OS SDSF Operation and Customization (SA23-2274)
   - Complexity: XL
   - Priority: HIGH — This is one of the most-used tools on z/OS; critical for user experience
   - Dependencies: JES2, ISPF, TSO

2. **MPF (Message Processing Facility)**
   - Classification: system_service
   - Description: Automation facility that matches WTO message IDs to automatic command responses. Configured via MPFLSTxx parmlib member. Foundation of system automation.
   - IBM Reference: z/OS MVS Initialization and Tuning Reference (SA23-1380)
   - Complexity: M
   - Priority: MEDIUM — Important for automation scenarios
   - Dependencies: WTO, console subsystem

3. **Extended MCS Console API (MCSOPER/MCSOPMSG)**
   - Classification: callable_service
   - Description: Programmatic console access allowing applications to receive and process operator messages without a physical console.
   - IBM Reference: z/OS MVS Programming: Authorized Assembler Services Guide (SA23-1371)
   - Complexity: M
   - Priority: MEDIUM — Used by automation products, z/OSMF console API
   - Dependencies: WTO, console subsystem

4. **DISPLAY command family**
   - Classification: command
   - Description: 30+ DISPLAY subcommands for viewing system state: active jobs, memory, devices, GRS locks, WLM, SMF, etc. Each returns formatted text to the console.
   - Complexity: L (aggregate)
   - Priority: HIGH — Essential for system monitoring and troubleshooting
   - Dependencies: Respective subsystems

### Recommendations for OpenMainframe

**Implement as a new crate (`open-mainframe-console`):**
- System command dispatcher (route commands to subsystem handlers)
- DISPLAY A,L (active address spaces / jobs)
- DISPLAY J,jobname (job information — extend z/OSMF handler)
- DISPLAY T (date/time — trivial)
- START/STOP/MODIFY (started task management)
- REPLY (WTOR reply processing)

**Implement as ISPF/TSO application:**
- SDSF panels (DA, ST, O, H, LOG) — highest user-visible value
- SDSF REXX interface (ISFEXEC)

---

## AREA-3: Program Management & Loader Services

### Overview

Program management handles how executable programs are loaded, linked, and executed on z/OS. The Binder (IEWL/IEWBLINK) creates load modules and program objects. The system loader loads them into storage for execution. The search order (STEPLIB → JOBLIB → LPA → LNKLST) determines where programs are found.

**IBM References:**
- z/OS MVS Program Management: User's Guide and Reference (SA23-1393)
- z/OS MVS Program Management: Advanced Facilities (SA23-1392)

### Components Inventory

#### Binder / Linkage Editor

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| IEWL (Linkage Editor) | Utility | Traditional linkage editor — creates load modules | NO | N/A | HIGH |
| IEWBLINK (Binder) | Utility | Modern binder — creates program objects | NO | N/A | HIGH |
| CEEWL | Utility | LE-enabled binder front-end | NO | Batch 12 | MEDIUM |
| Load Module Format | Format | CESD + RLD + TXT + END records, scatter/load | NO | N/A | HIGH |
| Program Object Format | Format | PDSE program library format (replaces load modules) | NO | N/A | HIGH |
| Side Deck | Artifact | DLL import/export definitions | NO | N/A | LOW |
| SYSLIN DD | Convention | Input to binder (object decks, control statements) | NO | N/A | HIGH |
| SYSLMOD DD | Convention | Output from binder (load library) | NO | N/A | HIGH |
| Binder API | API | Programmatic access to binder services | NO | N/A | LOW |
| INCLUDE control statement | Control | Include object modules | NO | N/A | HIGH |
| ENTRY control statement | Control | Define entry point | NO | N/A | HIGH |
| NAME control statement | Control | Name the load module | NO | N/A | HIGH |
| ALIAS control statement | Control | Define alias entry points | NO | N/A | MEDIUM |
| ORDER control statement | Control | Order CSECTs in load module | NO | N/A | LOW |
| CHANGE control statement | Control | Rename external symbols | NO | N/A | LOW |
| REPLACE control statement | Control | Replace CSECTs | NO | N/A | MEDIUM |
| MODE control statement | Control | Set AMODE/RMODE attributes | NO | N/A | MEDIUM |

#### Program Loading & Execution

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| LOAD macro | SVC 8 | Load module into storage | NO | N/A | HIGH |
| DELETE macro | SVC 9 | Delete loaded module | NO | N/A | MEDIUM |
| LINK macro | SVC 6 | Load and branch to program | PARTIAL (CICS) | N/A | HIGH |
| XCTL macro | SVC 7 | Transfer control (no return) | PARTIAL (CICS) | N/A | HIGH |
| ATTACH macro | SVC 42 | Create subtask running program | NO | N/A | HIGH |
| Program fetch | Service | Fetch module from library | NO | N/A | HIGH |
| Scatter loading | Service | Load non-contiguous CSECTs | NO | N/A | LOW |

#### Program Search Order

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| STEPLIB | DD | Step-level private library | PARTIAL (JCL) | N/A | HIGH |
| JOBLIB | DD | Job-level private library | PARTIAL (JCL) | N/A | HIGH |
| LPA (Link Pack Area) | Region | Shared reentrant modules | NO | N/A | MEDIUM |
| LNKLST (LINKLIST) | Concat | System search concatenation | NO | N/A | MEDIUM |
| APF List | Config | Authorized Program Facility list | NO | N/A | MEDIUM |
| PPT (Program Properties Table) | Config | Program execution attributes | NO | N/A | LOW |
| MLPA | Region | Modified LPA (IPL additions) | NO | N/A | LOW |
| FLPA | Region | Fixed LPA (non-pageable) | NO | N/A | LOW |
| Dynamic LPA | Service | CSVDYLPA — add/delete LPA modules at runtime | NO | N/A | LOW |

#### Object Module Format

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| ESD (External Symbol Dictionary) | Record | External symbol definitions | NO | N/A | HIGH |
| TXT (Text) | Record | Code and data content | NO | N/A | HIGH |
| RLD (Relocation Dictionary) | Record | Address constants to relocate | NO | N/A | HIGH |
| END | Record | Entry point and end of module | NO | N/A | HIGH |
| GOFF (Generalized Object File Format) | Format | Modern object format for program objects | NO | N/A | MEDIUM |
| XOBJ (Extended Object) | Format | Extended object format with 64-bit support | NO | N/A | LOW |

### New Gaps Discovered

1. **Binder (IEWBLINK) — Program Linking**
   - Classification: utility
   - Description: The Binder combines object modules into executable load modules or program objects. It resolves external references, performs relocation, and creates the final executable. Every compiled COBOL, PL/I, HLASM, and C program goes through the Binder.
   - IBM Reference: z/OS MVS Program Management: User's Guide and Reference (SA23-1393)
   - Complexity: XL
   - Priority: HIGH — OpenMainframe's COBOL compiler produces LLVM IR, but a traditional object-deck → load-module pipeline would enable running real mainframe object decks
   - Dependencies: Object module format, load module format

2. **Load Module / Program Object Format**
   - Classification: format
   - Description: The binary formats for z/OS executable programs. Load modules (traditional, in PDS) have CESD/RLD/TXT records. Program objects (modern, in PDSE) use a different internal format.
   - IBM Reference: z/OS MVS Program Management: Advanced Facilities (SA23-1392)
   - Complexity: L
   - Priority: HIGH — Needed to load and execute real z/OS binaries
   - Dependencies: Binder

3. **Program Fetch and Library Search**
   - Classification: system_service
   - Description: The system service that locates and loads programs from the library search order: STEPLIB → JOBLIB → LPA → LNKLST. This is how z/OS finds programs when JCL says `EXEC PGM=program`.
   - IBM Reference: z/OS MVS Programming: Assembler Services Guide (SA23-1368)
   - Complexity: M
   - Priority: HIGH — JCL EXEC PGM= depends on this for finding programs
   - Dependencies: PDS/PDSE library support, catalog

4. **APF Authorization**
   - Classification: system_service
   - Description: The Authorized Program Facility controls which programs can execute in supervisor state or access system services. Programs must reside in APF-authorized libraries.
   - IBM Reference: z/OS MVS Initialization and Tuning Reference (SA23-1380)
   - Complexity: S
   - Priority: MEDIUM — Needed for security model fidelity
   - Dependencies: RACF, program search

### Recommendations for OpenMainframe

**High priority — enables real workload execution:**
- Object module parser (ESD/TXT/RLD/END records) — enables loading compiled output
- Basic binder functionality (INCLUDE, ENTRY, NAME) — enables link-editing
- Program search order (STEPLIB/JOBLIB) — enhances JCL executor
- LOAD/LINK/XCTL at system level — extends beyond CICS-only

**Deferred:**
- GOFF/XOBJ formats — only needed for modern compilers
- LPA/LNKLST — only needed for started tasks and system programs
- APF authorization — can be simulated with RACF checks
- Dynamic LPA — only needed for hot-patching

---

## AREA-4: Data Management Beyond DFSMS

### Overview

z/OS data management extends well beyond the basic access methods and catalog already implemented in OpenMainframe. The DFSMS product suite includes five components: DFSMSdfp (base), DFSMShsm (migration/backup), DFSMSdss (dump/restore), DFSMSrmm (tape management), and DFSMStvs (transactional VSAM). Additional capabilities include tape management, checkpoint/restart, extended format datasets, and zFS filesystems.

**IBM References:**
- z/OS DFSMS Using Data Sets (SC23-6855)
- z/OS DFSMShsm Managing Your Own Data (SC23-6870)
- z/OS DFSMSdss Storage Administration (SC23-6868)
- z/OS DFSMSrmm Guide and Reference (SC23-6874)

### Components Inventory

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| DFSMSdfp (base) | Product | Access methods, catalog, utilities, SMS | PARTIAL (dataset crate) | Batch 19 | HIGH |
| DFSMShsm (HSM) | Product | Hierarchical Storage Manager — migration, recall, backup, space mgmt | NO | Batch 19 | MEDIUM |
| DFSMSdss (ADRDSSU) | Utility | Dump/restore datasets and volumes, DEFRAG, CONVERTV | NO | Batch 19 | MEDIUM |
| DFSMSrmm (RMM) | Product | Tape volume management, retention, movement policies | NO | N/A | LOW |
| DFSMStvs (TVS) | Product | Transactional VSAM — record-level sharing with RRS | NO | N/A | LOW |
| SMS Constructs | Service | Data/Storage/Management classes, ACS routines | NO | Batch 19 | MEDIUM |
| ISMF | ISPF app | Interactive Storage Management Facility — SMS admin panels | NO | Batch 19 | LOW |
| ICKDSF | Utility | Device Support Facilities — DASD volume initialization | NO | N/A | LOW |
| BSAM | Access method | Basic Sequential Access Method — block-level I/O | YES | Batch 19 | DONE |
| QSAM | Access method | Queued Sequential Access Method — record-level I/O | YES | Batch 19 | DONE |
| BPAM | Access method | Basic Partitioned Access Method — PDS directory access | PARTIAL | Batch 19 | MEDIUM |
| VSAM (KSDS/ESDS/RRDS/LDS) | Access method | Virtual Storage Access Method — keyed/sequential/relative/linear | YES | Batch 19 | DONE |
| OAM | Access method | Object Access Method — large object storage | NO | N/A | LOW |
| EXCP | Access method | Execute Channel Program — physical I/O | NO | N/A | LOW |
| Extended Format DS | Feature | Large format, striped, compressed datasets | NO | Batch 19 | MEDIUM |
| PDSE (data library) | Feature | Partitioned Data Set Extended — member generations | PARTIAL | Batch 19 | MEDIUM |
| PDSE (program library) | Feature | Program objects vs load modules | NO | N/A | MEDIUM |
| zFS | Filesystem | z/OS File System — POSIX-compatible, mounts in USS | NO | Batch 18 | MEDIUM |
| HFS (deprecated) | Filesystem | Hierarchical File System — replaced by zFS | NO | N/A | LOW |
| DADSM | Service | Direct Access Device Space Management — VTOC management | NO | Batch 19 | MEDIUM |
| Checkpoint/Restart | Service | CHKPT macro, automatic/deferred restart, RD parameter | NO | N/A | LOW |
| GDG (Generation Data Group) | Feature | Versioned dataset generations | YES | Batch 19 | DONE |
| Tape I/O | Access method | Tape dataset read/write via BSAM/QSAM | NO | N/A | LOW |
| AMATERSE | Utility | Compress/decompress for file transfer | NO | N/A | LOW |

### New Gaps Discovered

1. **DFSMShsm (HSM) — Hierarchical Storage Manager**
   - Classification: product/system_service
   - Description: Automated space management (migration/recall) and backup/recovery. HMIGRATE migrates datasets to ML1/ML2. HRECALL recalls them. HBACKDS creates incremental backups. ARECOVER performs disaster recovery.
   - Complexity: XL
   - Priority: MEDIUM — Important for storage management but not needed for basic workload execution
   - Dependencies: Dataset catalog, VSAM, tape support

2. **ADRDSSU (DFSMSdss) — Dataset Dump/Restore**
   - Classification: utility
   - Description: Physical and logical dump/restore of datasets and volumes. DUMP, RESTORE, COPY, PRINT, RELEASE, DEFRAG, CONVERTV operations. Used for backup, disaster recovery, and storage management.
   - Complexity: L
   - Priority: MEDIUM — Important for operations but not for application execution
   - Dependencies: Dataset catalog, VSAM, volume management

3. **Extended Format Datasets**
   - Classification: feature
   - Description: Large format (>65,535 tracks), striped (multi-volume parallel), compressed (zEDC hardware or software), extended addressability datasets. Required by modern DB2 and IMS.
   - Complexity: L
   - Priority: MEDIUM — DB2 large tablespaces require this
   - Dependencies: SMS, DADSM

---

## AREA-5: JES2 Deep Dive — Beyond Basic Job Management

### Overview

JES2 is far more than job queuing. It handles NJE (Network Job Entry), 100+ installation exits, multi-access spool (MAS) for sysplex, PSF print services, SPOOL management, and complex job routing/scheduling.

**IBM References:**
- z/OS JES2 Initialization and Tuning Guide (SA32-0991)
- z/OS JES2 Installation Exits (SA32-0993)
- z/OS JES2 Commands (SA32-0994)

### Components Inventory

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| Job queuing (INPUT/EXEC/OUTPUT) | Core | Job lifecycle management | YES | Batch 11 | DONE |
| Spool file management | Core | SYSOUT, spool read/write | YES | Batch 11 | DONE |
| Job status tracking | Core | ACTIVE/COMPLETED/OUTPUT | YES | Batch 11 | DONE |
| Hold/Release/Cancel | Core | Job control operations | YES | Batch 11 | DONE |
| NJE (Network Job Entry) | Feature | Multi-system job networking (BSC/SNA/TCP) | NO | Batch 11 | LOW |
| JES2 Initialization (HASPPARM) | Config | JES2 parameter processing from init dataset | NO | Batch 11 | MEDIUM |
| JES2 Exits (EXIT1-EXIT255) | Framework | Installation exits for customizing job processing | NO | Batch 11 | MEDIUM |
| MAS (Multi-Access Spool) | Feature | Shared spool across sysplex members | NO | Batch 11 | LOW |
| PSF (Print Services Facility) | Product | Advanced printing (AFP, IPDS, PostScript) | NO | N/A | LOW |
| JES2 FSS (Functional Subsystem) | Service | Manages PSF printers under JES2 | NO | N/A | LOW |
| SPOOL Offloading | Feature | Move spool data to external storage | NO | Batch 11 | LOW |
| Checkpoint datasets | Feature | JES2 recovery via checkpoint | NO | Batch 11 | LOW |
| JCL Converter/Interpreter | Service | Convert JCL to internal format for execution | PARTIAL (JCL crate) | Batch 11 | HIGH |
| JESMSGLG/JESJCL/JESYSMSG | Spool files | Standard JES2 message datasets | PARTIAL | Batch 11 | MEDIUM |
| Output descriptors (OUTDEF) | Feature | Customized output processing attributes | NO | Batch 11 | LOW |
| Automatic commands (JES2 &CMD) | Feature | Commands issued at JES2 initialization | NO | Batch 11 | LOW |
| Job routing | Feature | Route jobs to specific execution nodes/members | NO | Batch 11 | LOW |
| Proclib management | Service | Procedure library concatenation and search | PARTIAL (JCL) | Batch 11 | MEDIUM |

### New Gaps Discovered

1. **JCL Converter/Interpreter (full fidelity)**
   - Classification: system_service
   - Description: The JES2 converter translates JCL statements into internal control blocks (SWA/SJB). The interpreter processes these at execution time. OpenMainframe's JCL crate parses JCL but doesn't produce IBM-compatible internal format.
   - Complexity: XL
   - Priority: HIGH — Required for accurate JCL processing including PROC expansion, symbolic substitution, and conditional processing
   - Dependencies: JCL crate

2. **JES2 Installation Exits**
   - Classification: framework
   - Description: 100+ numbered exit points allowing installations to customize every aspect of job processing: job selection, output routing, NJE processing, security checks, spool management. Most z/OS shops use at least EXIT7 (JCL scan) and EXIT5 (job output).
   - Complexity: L
   - Priority: MEDIUM — Important for real-world customization but not for basic operation
   - Dependencies: JES2 core

---

## AREA-6: RACF Deep Dive — Beyond Basic Security

### Overview

RACF provides far more than user/group management and password validation. It includes 100+ general resource classes, digital certificate management, PassTickets, security labels, delegated administration, and integration with ICSF for cryptographic services.

**IBM References:**
- z/OS Security Server RACF Security Administrator's Guide (SA23-2289)
- z/OS Security Server RACF Command Language Reference (SA23-2292)
- z/OS Security Server RACF System Programmer's Guide (SA23-2287)

### Components Inventory

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| User profiles (ADDUSER/ALTUSER/LISTUSER) | Core | User management | YES | Batch 8 | DONE |
| Group profiles (ADDGROUP/ALTGROUP) | Core | Group management | YES | Batch 8 | DONE |
| Dataset profiles (ADDSD/ALTDSD) | Core | Dataset protection | PARTIAL | Batch 8 | HIGH |
| General resource profiles (RDEFINE/RALTER) | Core | Non-dataset resource protection | NO | Batch 8 | HIGH |
| PERMIT | Core | Grant/revoke access to profiles | PARTIAL | Batch 8 | HIGH |
| SETROPTS | Admin | Set system-wide RACF options | NO | Batch 8 | MEDIUM |
| SEARCH | Admin | Search RACF database | NO | Batch 8 | MEDIUM |
| RACDCERT | Admin | Digital certificate management | PARTIAL | Batch 8 | MEDIUM |
| FACILITY class | Resource class | General-purpose authorization checks | NO | Batch 8 | HIGH |
| OPERCMDS class | Resource class | Console command authorization | NO | N/A | MEDIUM |
| SURROGAT class | Resource class | Surrogate job submission | NO | N/A | MEDIUM |
| STARTED class | Resource class | Started task identity | NO | N/A | MEDIUM |
| UNIXPRIV class | Resource class | USS privileged operations | NO | Batch 18 | MEDIUM |
| PROGRAM class | Resource class | Program control | NO | N/A | LOW |
| PTKTDATA class | Resource class | PassTicket definitions | NO | N/A | MEDIUM |
| SECLABEL class | Resource class | Security labels (MLS) | NO | N/A | LOW |
| CSFKEYS/CSFSERV classes | Resource class | ICSF cryptographic key access | NO | N/A | LOW |
| Class Descriptor Table (CDT) | Config | Resource class definitions | NO | Batch 8 | MEDIUM |
| PassTickets | Feature | One-time password generation/validation | NO | N/A | MEDIUM |
| Key rings | Feature | Certificate key ring management | NO | Batch 8 | MEDIUM |
| Security labels (MLS) | Feature | Multi-Level Security classification | NO | N/A | LOW |
| RACF database | Infrastructure | RACF DB structure, backup/recovery | PARTIAL | Batch 8 | HIGH |
| IRRUT100/IRRUT200/IRRUT400 | Utility | RACF database utilities | NO | Batch 8 | LOW |
| DSMON | Utility | RACF monitoring report | NO | Batch 8 | LOW |
| SMF Type 80 records | Audit | RACF event recording | PARTIAL (SMF) | Batch 14 | MEDIUM |
| RACROUTE/ICHEINTY | Internal macro | SAF router internals | PARTIAL (SAF) | Batch 8 | MEDIUM |

### New Gaps Discovered

1. **General Resource Class Framework**
   - Classification: system_service
   - Description: RACF manages 100+ resource classes beyond datasets (FACILITY, OPERCMDS, SURROGAT, STARTED, SDSF, TERMINAL, PROGRAM, etc.). Each class controls access to different system resources. The Class Descriptor Table (CDT) defines class characteristics.
   - Complexity: L
   - Priority: HIGH — Many subsystems check FACILITY class profiles for authorization
   - Dependencies: RACF core

2. **PassTickets (PTKTDATA)**
   - Classification: feature
   - Description: One-time passwords generated algorithmically (DES or HMAC-based). Used by web applications, z/OSMF, and middleware for programmatic authentication without storing passwords.
   - Complexity: M
   - Priority: MEDIUM — Important for z/OSMF and middleware integration
   - Dependencies: RACF, optionally ICSF

---

## AREA-7: Batch Processing Ecosystem

### Components Inventory

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| IEFBR14 | Utility | Null program (allocation only) | YES | Batch 21 | DONE |
| IEBGENER | Utility | Sequential copy/generate | PARTIAL | Batch 21 | MEDIUM |
| IEBCOPY | Utility | PDS copy/compress/merge | NO | Batch 21 | HIGH |
| IEBCOMPR | Utility | Dataset compare | NO | Batch 21 | LOW |
| IEBUPDTE | Utility | PDS update utility | NO | Batch 21 | MEDIUM |
| IEBPTPCH | Utility | Print/punch | NO | Batch 21 | LOW |
| IEBDG | Utility | Test data generator | NO | Batch 21 | LOW |
| IEHMOVE | Utility | Dataset/volume move | NO | Batch 21 | LOW |
| IEHPROGM | Utility | System maintenance (scratch/rename/catalog) | NO | Batch 21 | MEDIUM |
| IEHLIST | Utility | Catalog/VTOC/PDS listing | NO | Batch 21 | LOW |
| AMASPZAP | Utility | Superzap binary patcher | NO | Batch 21 | LOW |
| DFSORT/ICEMAN | Utility | Sort/merge/copy | YES | Batch 21 | DONE |
| ICEGENER | Utility | Fast sequential copy | NO | Batch 21 | LOW |
| ICETOOL | Utility | Multi-function batch utility | NO | Batch 21 | MEDIUM |
| IKJEFT01/1A/1B | Utility | TSO batch TMP | NO | Batch 21 | HIGH |
| IRXJCL | Utility | REXX batch processor | NO | Batch 21 | HIGH |
| BPXBATCH | Utility | USS batch processor | NO | Batch 21 | MEDIUM |
| ADRDSSU | Utility | DFSMSdss dump/restore | NO | N/A | MEDIUM |
| ICKDSF | Utility | DASD initialization | NO | N/A | LOW |
| IGYCRCTL | Compiler | Enterprise COBOL compiler | PARTIAL (COBOL crate) | N/A | HIGH |
| ASMA90/IEV90 | Compiler | High-Level Assembler | PARTIAL (HLASM crate) | Batch 2 | MEDIUM |
| IBMZPLI | Compiler | Enterprise PL/I compiler | PARTIAL (PL/I crate) | Batch 3 | LOW |
| IEWL/IEWBLINK | Utility | Binder/linkage editor | NO | N/A | HIGH |
| GIMSMP | Product | SMP/E (System Modification Program) | NO | N/A | LOW |
| AMATERSE | Utility | Compress/decompress for transfer | NO | N/A | LOW |

### New Gaps Discovered

1. **Binder (IEWL/IEWBLINK)**
   - Already listed in AREA-3 — cross-reference

2. **SMP/E (GIMSMP)**
   - Classification: product
   - Description: System Modification Program/Extended — manages software installation, maintenance, and service. Tracks SYSMODs, PTFs, APARs, USERMODs. Every z/OS product is installed via SMP/E.
   - Complexity: XXL
   - Priority: LOW — Not needed for application execution; only for system maintenance
   - Dependencies: Dataset management, catalog

---

## AREA-8: TSO/ISPF Deep Dive

### Components Inventory

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| TSO Command Processor | Core | Interactive command interpreter | PARTIAL | Batch 9 | HIGH |
| TSO ALLOCATE/FREE | Command | Dynamic dataset allocation | NO | Batch 9 | HIGH |
| TSO LISTDS/LISTCAT | Command | List dataset/catalog info | NO | Batch 9 | HIGH |
| TSO SUBMIT | Command | Submit JCL for batch execution | PARTIAL | Batch 9 | HIGH |
| TSO EXEC | Command | Execute REXX/CLIST | PARTIAL | Batch 9 | HIGH |
| TSO EDIT (line mode) | Command | Line-mode editor | NO | Batch 9 | LOW |
| TSO STATUS | Command | Check job status | NO | Batch 9 | MEDIUM |
| TSO OUTPUT | Command | Retrieve job output | NO | Batch 9 | MEDIUM |
| TSO SEND/RECEIVE | Command | Message send/receive | NO | Batch 9 | LOW |
| TSO PROFILE | Command | User profile settings | NO | Batch 9 | MEDIUM |
| TSO RENAME/DELETE | Command | Dataset rename/delete | NO | Batch 9 | MEDIUM |
| TSO CALL/LINK/LOADGO | Command | Program execution | NO | Batch 9 | MEDIUM |
| TSO TEST | Command | Debug facility | NO | Batch 9 | LOW |
| ISPF Option 1 (Browse) | Application | File browsing | NO | Batch 9 | HIGH |
| ISPF Option 2 (Edit) | Application | Full-screen editor | PARTIAL | Batch 9 | HIGH |
| ISPF Option 3 (Utilities) | Application | Dataset utilities (copy, move, rename, etc.) | NO | Batch 9 | HIGH |
| ISPF Option 4 (Foreground) | Application | Foreground compile/link | NO | Batch 9 | MEDIUM |
| ISPF Option 5 (Batch) | Application | Batch job submission | NO | Batch 9 | MEDIUM |
| ISPF Option 6 (Command) | Application | TSO command shell | PARTIAL | Batch 9 | MEDIUM |
| ISPF Editor (ISREDIT) | Service | Edit macro interface | NO | Batch 9 | HIGH |
| ISPF Dialog Manager (ISPEXEC) | Service | Panel display, table, skeleton services | PARTIAL | Batch 9 | HIGH |
| ISPF SuperC (ISRSUPC) | Utility | File comparison utility | NO | N/A | MEDIUM |
| ISPF Search-For (ISRSFIND) | Utility | String search across datasets | NO | N/A | MEDIUM |
| ISPF Tables (TBOPEN etc.) | Service | ISPF table management | PARTIAL | Batch 9 | MEDIUM |
| ISPF Skeletons (FTOPEN etc.) | Service | File tailoring services | NO | Batch 9 | MEDIUM |
| ISPF LM Services (LMINIT etc.) | Service | Library management services | NO | Batch 9 | MEDIUM |
| ISPF BRIF/EDIF/VIIF | Service | Browse/Edit/View interface | NO | Batch 9 | MEDIUM |
| ISPF WorkPlace | Application | Object/Action model interface | NO | N/A | LOW |
| ISPF configuration (ISPF.CONF) | Config | ISPF system configuration | NO | Batch 9 | LOW |

### New Gaps Discovered

1. **SDSF (System Display and Search Facility)**
   - Already listed in AREA-2 — cross-reference. Probably the single highest-value ISPF application to implement.

2. **ISPF SuperC (ISRSUPC) / Search-For (ISRSFIND)**
   - Classification: utility/ISPF_application
   - Description: SuperC compares files line-by-line with powerful options (ignore blanks, columns, etc.). Search-For searches for strings across PDS members and sequential datasets.
   - Complexity: M
   - Priority: MEDIUM — Very commonly used by developers
   - Dependencies: ISPF, dataset access

---

## AREA-9: z/OS UNIX System Services (USS) Complete Inventory

### Components Inventory

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| USS file operations (read/write/mkdir/delete) | Core | Basic file I/O | YES (z/OSMF) | Batch 18 | DONE |
| POSIX file system (zFS) | Filesystem | z/OS File System with directory structure | NO | Batch 18 | HIGH |
| /bin/sh (shell) | Utility | POSIX shell interpreter | NO | Batch 18 | HIGH |
| Core shell utilities (ls, cp, mv, rm, mkdir, chmod, chown) | Utility | Basic file commands | NO | Batch 18 | HIGH |
| Text utilities (grep, sed, awk, sort, cat, head, tail) | Utility | Text processing | NO | Batch 18 | MEDIUM |
| Archive utilities (tar, pax, cpio) | Utility | File archiving | NO | Batch 18 | LOW |
| Build utilities (make, cc/xlc) | Utility | Build system | NO | Batch 18 | LOW |
| POSIX threads (pthreads) | Service | Thread management | NO | Batch 18 | MEDIUM |
| POSIX signals | Service | Signal handling (kill, sigaction) | NO | Batch 18 | MEDIUM |
| POSIX IPC (semaphores, shared memory, message queues) | Service | Inter-process communication | NO | Batch 18 | MEDIUM |
| BPX1xxx/BPX4xxx callable services | API | Kernel system call interface | NO | Batch 18 | HIGH |
| BPXBATCH | Utility | Batch USS program execution | NO | Batch 21 | MEDIUM |
| tsocmd/mvscmd/opercmd | Utility | USS-to-MVS bridge commands | NO | N/A | MEDIUM |
| chtag | Utility | File codeset tagging | PARTIAL (z/OSMF) | Batch 18 | LOW |
| oget/oput/ocopy | Utility | MVS↔USS data transfer | NO | N/A | MEDIUM |
| inetd/ftpd/sshd | Service | Network services | NO | Batch 20 | MEDIUM |
| NFS client/server | Service | Network file system | NO | Batch 20 | LOW |
| OMVS segment (RACF) | Security | USS UID/GID mapping to RACF | NO | Batch 18 | HIGH |
| ISHELL | ISPF app | ISPF shell interface to USS | NO | N/A | LOW |
| Java runtime (IBM Semeru) | Runtime | Java on z/OS | NO | N/A | LOW |
| Python/Node.js | Runtime | Scripting runtimes on USS | NO | N/A | LOW |

#### BPX1xxx Callable Services (Key Services — 100+ total)

| Service | UNIX Equivalent | Description | In OpenMainframe? | Priority |
|---------|----------------|-------------|-------------------|----------|
| BPX1OPN | open() | Open a file | NO | HIGH |
| BPX1RED | read() | Read from file descriptor | NO | HIGH |
| BPX1WRT | write() | Write to file descriptor | NO | HIGH |
| BPX1CLO | close() | Close file descriptor | NO | HIGH |
| BPX1STA | stat() | Get file status | NO | HIGH |
| BPX1OPD | opendir() | Open directory | NO | HIGH |
| BPX1RDD | readdir() | Read directory entry | NO | HIGH |
| BPX1CLD | closedir() | Close directory | NO | HIGH |
| BPX1MKD | mkdir() | Create directory | NO | HIGH |
| BPX1RMD | rmdir() | Remove directory | NO | HIGH |
| BPX1UNL | unlink() | Delete file | NO | HIGH |
| BPX1REN | rename() | Rename file | NO | HIGH |
| BPX1CHM | chmod() | Change file permissions | NO | MEDIUM |
| BPX1CHO | chown() | Change file ownership | NO | MEDIUM |
| BPX1LNK | link() | Create hard link | NO | LOW |
| BPX1SYM | symlink() | Create symbolic link | NO | MEDIUM |
| BPX1LSK | lseek() | Seek within file | NO | MEDIUM |
| BPX1FCT | fcntl() | File control | NO | MEDIUM |
| BPX1IOC | ioctl() | I/O control | NO | LOW |
| BPX1PIP | pipe() | Create pipe | NO | MEDIUM |
| BPX1FRK | fork() | Fork process | NO | HIGH |
| BPX1EXC | exec() | Execute program | NO | HIGH |
| BPX1EXI | _exit() | Exit process | NO | HIGH |
| BPX1WAT | wait() | Wait for child process | NO | HIGH |
| BPX1KIL | kill() | Send signal to process | NO | MEDIUM |
| BPX1SIG | sigaction() | Signal handling | NO | MEDIUM |
| BPX1SOC | socket() | Create socket | NO | MEDIUM |
| BPX1BND | bind() | Bind socket | NO | MEDIUM |
| BPX1LSN | listen() | Listen on socket | NO | MEDIUM |
| BPX1ACP | accept() | Accept connection | NO | MEDIUM |
| BPX1CON | connect() | Connect to server | NO | MEDIUM |
| BPX1SND | send() | Send data on socket | NO | MEDIUM |
| BPX1RCV | recv() | Receive data from socket | NO | MEDIUM |
| BPX1SEL | select() | I/O multiplexing | NO | MEDIUM |
| BPX1POL | poll() | Poll I/O events | NO | MEDIUM |
| BPX1GTH | getthent() | Get thread info | NO | LOW |
| BPX1PTH | pthread_create() | Create thread | NO | MEDIUM |
| BPX1ATX | attach_exec() | Attach and execute | NO | LOW |
| BPX1ENV | oe_env_np() | Environment manipulation | NO | LOW |

---

## AREA-10: System Logger, Coupling Facility & Sysplex

### Components Inventory

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| System Logger (IXGLOGR) | Service | Centralized log stream management | NO | N/A | MEDIUM |
| Log streams (CF-based) | Service | Coupling Facility log streams | NO | N/A | LOW |
| Log streams (DASD-only) | Service | Single-system log streams | NO | N/A | MEDIUM |
| Coupling Facility (CF) | Hardware | Shared memory/lock structures for sysplex | NO | N/A | LOW |
| XCF (Cross-System Coupling Facility) | Service | Inter-system communication in sysplex | NO | N/A | LOW |
| GRS (Global Resource Serialization) | Service | System-wide ENQ/DEQ management | NO | N/A | MEDIUM |
| Parallel Sysplex | Architecture | Multi-system shared-everything cluster | NO | N/A | LOW |
| ARM (Automatic Restart Manager) | Service | Automatic element restart on failure | NO | N/A | LOW |
| VTAM/SNA | Service | SNA network management | NO | Batch 20 | LOW |
| SA z/OS (System Automation) | Product | Automated operations | NO | N/A | LOW |

### Recommendations

Sysplex features are primarily relevant for multi-system environments. For a single-system OpenMainframe, the key items are:
- **System Logger** (DASD-only mode) — used by CICS, DB2, and RRS for recovery logging
- **GRS** (single-system mode) — needed for dataset integrity with ENQ/DEQ

---

## AREA-11: Debugging, Dump Analysis & Problem Determination

### Components Inventory

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| IPCS | Tool | Interactive Problem Control System — dump analysis | NO | N/A | MEDIUM |
| SVC dump (SDUMP) | Service | System-generated memory dumps | NO | N/A | LOW |
| SNAP dump | Service | Application-generated diagnostic dumps | NO | N/A | LOW |
| SYSABEND/SYSUDUMP/SYSMDUMP | DD names | Dump output DD specifications in JCL | NO | N/A | MEDIUM |
| GTF (Generalized Trace Facility) | Service | System event tracing | NO | N/A | LOW |
| Component Trace (CTRACE) | Service | Component-specific tracing | NO | N/A | LOW |
| Logrec (error recording) | Service | Hardware/software error log | NO | N/A | LOW |
| Health Checker | Service | Automated system health checks | NO | N/A | LOW |
| RMF (Resource Measurement Facility) | Product | Performance monitoring | NO | N/A | MEDIUM |
| ABEND codes (Sxxx/Uxxx) | Convention | System and user abend code definitions | PARTIAL (CICS) | N/A | HIGH |
| CEEDUMP | LE service | LE-generated diagnostic dump | NO | Batch 12 | MEDIUM |
| LE runtime options (TRAP, STORAGE, etc.) | Config | LE debugging options | NO | Batch 12 | MEDIUM |

#### Common System ABEND Codes (for framework implementation)

| Code | Description | Frequency | Priority |
|------|-------------|-----------|----------|
| S0C1 | Operation exception — invalid instruction | Very common | HIGH |
| S0C2 | Privileged operation exception | Occasional | MEDIUM |
| S0C4 | Protection exception — invalid memory access | Very common | HIGH |
| S0C5 | Addressing exception — address beyond storage | Common | HIGH |
| S0C6 | Specification exception — boundary error | Occasional | MEDIUM |
| S0C7 | Data exception — invalid decimal data | Very common | HIGH |
| S0CB | Decimal divide exception (divide by zero) | Occasional | MEDIUM |
| S013 | Conflicting DCB parameters (LRECL/RECFM/BLKSIZE) | Very common | HIGH |
| S222 | Job cancelled by operator | Common | HIGH |
| S322 | CPU time limit exceeded | Common | HIGH |
| S806 | Module not found (LINK/LOAD/EXEC PGM=) | Very common | HIGH |
| S837 | Dataset end-of-volume, no space | Common | MEDIUM |
| S913 | RACF authorization failure | Very common | HIGH |
| SB37 | End of volume, no more space | Common | MEDIUM |
| SD37 | Primary space exceeded, no secondary defined | Common | MEDIUM |
| SE37 | Secondary allocation limit reached | Common | MEDIUM |

### New Gaps Discovered

1. **ABEND Code Framework**
   - Classification: system_service
   - Description: z/OS has a structured system of abnormal end codes. System abends (S0C1=operation exception, S0C4=protection, S0C7=data exception, S013=dataset open failure, S222=operator cancel, S322=CPU time exceeded, S806=program not found, S913=RACF authorization failure, etc.) and user abends (U0-4095). Each has documented reason codes.
   - Complexity: M
   - Priority: HIGH — Essential for realistic JCL execution error reporting
   - Dependencies: None (can be enum-based)

2. **SYSABEND/SYSUDUMP/SYSMDUMP DD names**
   - Classification: convention
   - Description: JCL DD names that control dump output when a program abends. SYSUDUMP produces a formatted dump. SYSABEND includes system areas. SYSMDUMP produces an unformatted dump for IPCS.
   - Complexity: S
   - Priority: MEDIUM — Needed for proper JCL processing of dump DD names
   - Dependencies: JCL executor

---

## AREA-12: Compilers, Preprocessors & Language Toolchain

### Components Inventory

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| IGYCRCTL (Enterprise COBOL) | Compiler | COBOL compiler with DB2/CICS/SQL preprocessors | PARTIAL | N/A | HIGH |
| ASMA90 (HLASM) | Assembler | High-Level Assembler | PARTIAL | Batch 2 | MEDIUM |
| IBMZPLI (Enterprise PL/I) | Compiler | PL/I compiler | PARTIAL | Batch 3 | LOW |
| XL C/C++ (CCNDRVR) | Compiler | C/C++ compiler for z/OS | NO | N/A | LOW |
| COBOL DB2 precompiler (DSNHPC) | Preprocessor | Embeds DB2 SQL in COBOL | NO | N/A | HIGH |
| COBOL CICS translator (DFHYITVL) | Preprocessor | Translates EXEC CICS statements | NO | N/A | HIGH |
| IEWL/IEWBLINK (Binder) | Utility | Linkage editor / binder | NO | N/A | HIGH |
| CEDF (CICS debug) | Debug tool | CICS Execution Diagnostic Facility | NO | Batch 14 | MEDIUM |
| IBM Debug for z/OS | Debug tool | Interactive debugger (formerly Debug Tool) | NO | N/A | LOW |
| Object deck format (ESD/TXT/RLD/END) | Format | Compiler output format | NO | N/A | HIGH |
| ADATA (Associated Data) | Format | Compiler cross-reference data | NO | N/A | LOW |
| Copybook processing (COPY/REPLACE) | Feature | COBOL source library inclusion | PARTIAL | N/A | HIGH |
| JCL compile/link/go procedures | Convention | Standard compile JCL (IGYWCLG etc.) | NO | N/A | MEDIUM |

### New Gaps Discovered

1. **COBOL DB2 Precompiler (DSNHPC)**
   - Classification: preprocessor
   - Description: Translates EXEC SQL statements in COBOL source into COBOL CALL statements to DB2. Produces a DBRM (Database Request Module) for binding. Essential for any COBOL-DB2 application.
   - Complexity: L
   - Priority: HIGH — Most enterprise COBOL programs use DB2
   - Dependencies: COBOL crate, DB2 crate

2. **COBOL CICS Translator (DFHYITVL)**
   - Classification: preprocessor
   - Description: Translates EXEC CICS statements in COBOL source into COBOL CALL statements to CICS runtime. Must run before the COBOL compiler.
   - Complexity: L
   - Priority: HIGH — Most CICS programs are COBOL
   - Dependencies: COBOL crate, CICS crate

---

## AREA-13: DB2, IMS & Data Server Ecosystem

### Components Inventory

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| DB2 SQL (DDL/DML) | Core | SQL language support | PARTIAL | N/A | HIGH |
| DB2 stored procedures | Feature | Server-side SQL procedures | NO | N/A | MEDIUM |
| DB2 triggers/UDFs | Feature | Event-driven logic and user functions | NO | N/A | MEDIUM |
| DB2 utilities (LOAD, UNLOAD, REORG, RUNSTATS, COPY, RECOVER) | Utility | Database maintenance | NO | N/A | HIGH |
| DB2 BIND (PLAN/PACKAGE) | Service | Bind SQL access paths | NO | N/A | HIGH |
| DBRM (Database Request Module) | Artifact | Precompiler output for binding | NO | N/A | HIGH |
| DSNTEP2/DSNTEP4 | Utility | Batch SQL processor | NO | N/A | MEDIUM |
| DSNTIAD | Utility | Batch dynamic SQL processor | NO | N/A | MEDIUM |
| SPUFI | ISPF app | SQL Processing Using File Input | NO | N/A | MEDIUM |
| QMF | Product | Query Management Facility | NO | N/A | LOW |
| IMS DL/I (GU, GN, GNP, ISRT, DLET, REPL) | Core | IMS database calls | PARTIAL | Batch 13 | HIGH |
| IMS PCB/PSB/DBD | Config | IMS program/database definitions | PARTIAL | Batch 13 | HIGH |
| IMS TM (Transaction Manager) | Core | IMS online transaction processing | NO | Batch 13 | MEDIUM |
| IMS Connect | Service | TCP/IP access to IMS | NO | Batch 13 | LOW |
| IMS MFS (Message Format Services) | Feature | Screen formatting for IMS TM | NO | Batch 13 | LOW |

---

## AREA-14: CICS Deep Dive

### Components Inventory

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| EXEC CICS commands (SEND/RECEIVE MAP, READ/WRITE, LINK, XCTL, RETURN) | Core | CICS application programming API | PARTIAL | N/A | HIGH |
| BMS (Basic Mapping Support) | Feature | Screen map definitions (DFHMSD/DFHMDI/DFHMDF) | NO | N/A | HIGH |
| CSD (CICS System Definition) | Config | Resource definitions (DFHCSDUP utility) | NO | N/A | MEDIUM |
| CEDA/CEDB/CEDC | Transaction | Online resource definition | NO | N/A | MEDIUM |
| CEMT | Transaction | Master terminal transaction | NO | N/A | MEDIUM |
| CEDF | Transaction | Execution Diagnostic Facility | NO | N/A | MEDIUM |
| Temporary Storage (WRITEQ TS/READQ TS) | Service | Temporary data queues | PARTIAL | N/A | HIGH |
| Transient Data (WRITEQ TD/READQ TD) | Service | Transient data queues | NO | N/A | MEDIUM |
| CICS web support (URIMAP, PIPELINE) | Feature | HTTP server/client in CICS | NO | N/A | MEDIUM |
| CICS-DB2 attachment | Integration | CICS-to-DB2 connection | NO | N/A | HIGH |
| CICS-MQ adapter (CKQC) | Integration | CICS-to-MQ connection | NO | N/A | MEDIUM |
| SMF Type 110 records | Audit | CICS monitoring/statistics | NO | Batch 14 | LOW |
| CICS channels/containers | Feature | Modern inter-program data passing | PARTIAL | N/A | MEDIUM |

### Complete EXEC CICS Command Reference

The EXEC CICS API is the primary programming interface for CICS applications. Below is the complete command inventory organized by functional category.

#### Terminal I/O Commands
| Command | Description | In OpenMainframe? | Priority |
|---------|-------------|-------------------|----------|
| SEND MAP | Send BMS map to terminal | PARTIAL | HIGH |
| RECEIVE MAP | Receive BMS map from terminal | PARTIAL | HIGH |
| SEND TEXT | Send unformatted text to terminal | NO | MEDIUM |
| SEND CONTROL | Send device control (ERASE, CURSOR) | NO | MEDIUM |
| RECEIVE | Receive raw data from terminal | NO | MEDIUM |
| CONVERSE | Send/receive in one command | NO | LOW |
| SEND PAGE | Send accumulated pages | NO | LOW |
| ROUTE | Route output to multiple terminals | NO | LOW |
| ISSUE COPY | Copy terminal screen | NO | LOW |
| ISSUE ERASE | Erase terminal | NO | LOW |

#### File Control Commands
| Command | Description | In OpenMainframe? | Priority |
|---------|-------------|-------------------|----------|
| READ | Read record by key | PARTIAL | HIGH |
| WRITE | Write new record | PARTIAL | HIGH |
| REWRITE | Update record in place | PARTIAL | HIGH |
| DELETE | Delete record | PARTIAL | HIGH |
| STARTBR | Start browse (position cursor) | PARTIAL | HIGH |
| READNEXT | Read next record in browse | PARTIAL | HIGH |
| READPREV | Read previous record in browse | PARTIAL | HIGH |
| ENDBR | End browse operation | PARTIAL | HIGH |
| RESETBR | Reposition browse cursor | NO | MEDIUM |
| UNLOCK | Release record lock | NO | MEDIUM |

#### Program Control Commands
| Command | Description | In OpenMainframe? | Priority |
|---------|-------------|-------------------|----------|
| LINK | Call program, expect return | PARTIAL | HIGH |
| XCTL | Transfer control, no return | PARTIAL | HIGH |
| RETURN | Return to caller/CICS | PARTIAL | HIGH |
| LOAD | Load program into memory | NO | MEDIUM |
| RELEASE | Release loaded program | NO | MEDIUM |

#### Interval Control Commands
| Command | Description | In OpenMainframe? | Priority |
|---------|-------------|-------------------|----------|
| START | Start new transaction asynchronously | NO | MEDIUM |
| RETRIEVE | Retrieve data from START | NO | MEDIUM |
| DELAY | Delay task execution | NO | LOW |
| POST | Set timer event | NO | LOW |
| WAIT EVENT | Wait for posted event | NO | LOW |
| CANCEL | Cancel pending START/DELAY | NO | LOW |
| ASKTIME | Get current time | NO | MEDIUM |
| FORMATTIME | Format time/date values | NO | MEDIUM |

#### Temporary Storage Commands
| Command | Description | In OpenMainframe? | Priority |
|---------|-------------|-------------------|----------|
| WRITEQ TS | Write to temp storage queue | PARTIAL | HIGH |
| READQ TS | Read from temp storage queue | PARTIAL | HIGH |
| DELETEQ TS | Delete temp storage queue | PARTIAL | HIGH |

#### Transient Data Commands
| Command | Description | In OpenMainframe? | Priority |
|---------|-------------|-------------------|----------|
| WRITEQ TD | Write to transient data queue | NO | MEDIUM |
| READQ TD | Read from transient data queue | NO | MEDIUM |
| DELETEQ TD | Delete transient data queue | NO | LOW |

#### Exception/Condition Handling Commands
| Command | Description | In OpenMainframe? | Priority |
|---------|-------------|-------------------|----------|
| HANDLE CONDITION | Set condition handler (legacy) | PARTIAL | HIGH |
| HANDLE ABEND | Set ABEND handler | PARTIAL | HIGH |
| HANDLE AID | Set attention key handler (legacy) | NO | MEDIUM |
| IGNORE CONDITION | Ignore specific condition | NO | MEDIUM |
| PUSH HANDLE | Save current handler state | NO | LOW |
| POP HANDLE | Restore handler state | NO | LOW |
| ABEND | Force task ABEND | PARTIAL | HIGH |

#### Syncpoint & Recovery Commands
| Command | Description | In OpenMainframe? | Priority |
|---------|-------------|-------------------|----------|
| SYNCPOINT | Commit unit of work | NO | HIGH |
| SYNCPOINT ROLLBACK | Roll back unit of work | NO | HIGH |

#### Storage Management Commands
| Command | Description | In OpenMainframe? | Priority |
|---------|-------------|-------------------|----------|
| GETMAIN | Acquire storage | NO | MEDIUM |
| FREEMAIN | Release storage | NO | MEDIUM |

#### Journal & Logging Commands
| Command | Description | In OpenMainframe? | Priority |
|---------|-------------|-------------------|----------|
| WRITE JOURNALNAME | Write to journal | NO | LOW |
| WAIT JOURNALNAME | Wait for journal I/O | NO | LOW |

#### ENQ/DEQ Commands
| Command | Description | In OpenMainframe? | Priority |
|---------|-------------|-------------------|----------|
| ENQ | Enqueue on resource | NO | MEDIUM |
| DEQ | Dequeue resource | NO | MEDIUM |

#### System Information Commands
| Command | Description | In OpenMainframe? | Priority |
|---------|-------------|-------------------|----------|
| ASSIGN | Get system information | NO | HIGH |
| ADDRESS | Get address of system areas | NO | MEDIUM |
| INQUIRE | Query CICS resources (SPI) | NO | MEDIUM |
| SET | Modify CICS resources (SPI) | NO | MEDIUM |
| COLLECT STATISTICS | Gather resource statistics (SPI) | NO | LOW |
| PERFORM | Perform system operations (SPI) | NO | LOW |

#### Channel/Container Commands (Modern API)
| Command | Description | In OpenMainframe? | Priority |
|---------|-------------|-------------------|----------|
| PUT CONTAINER | Store data in container | PARTIAL | MEDIUM |
| GET CONTAINER | Retrieve data from container | PARTIAL | MEDIUM |
| MOVE CONTAINER | Move container between channels | NO | LOW |
| DELETE CONTAINER | Remove container | NO | LOW |

#### Web/HTTP Commands
| Command | Description | In OpenMainframe? | Priority |
|---------|-------------|-------------------|----------|
| WEB SEND | Send HTTP response | NO | MEDIUM |
| WEB RECEIVE | Receive HTTP request | NO | MEDIUM |
| WEB READ HTTPHEADER | Read HTTP header | NO | LOW |
| WEB WRITE HTTPHEADER | Write HTTP header | NO | LOW |
| WEB EXTRACT | Extract web conversation info | NO | LOW |
| WEB OPEN | Open HTTP client connection | NO | LOW |
| WEB CLOSE | Close HTTP connection | NO | LOW |
| INVOKE SERVICE | Invoke web service | NO | LOW |
| INVOKE WEBSERVICE | Invoke SOAP web service | NO | LOW |

#### Document Commands
| Command | Description | In OpenMainframe? | Priority |
|---------|-------------|-------------------|----------|
| DOCUMENT CREATE | Create document from template | NO | LOW |
| DOCUMENT INSERT | Insert data into document | NO | LOW |
| DOCUMENT SET | Set document symbol | NO | LOW |
| DOCUMENT RETRIEVE | Retrieve document content | NO | LOW |

**Total EXEC CICS commands cataloged: ~90**
**Commands with PARTIAL implementation: ~20**
**Commands not implemented: ~70**

---

## AREA-15: System Initialization & Configuration (IPL to Ready)

### Components Inventory

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| IPL process (NIP) | Service | Nucleus Initialization Program | NO | N/A | MEDIUM |
| IEASYSxx (system parameters) | Config | Master system parameter member | NO | N/A | MEDIUM |
| IEASYMxx (system symbols) | Config | System symbol definitions | NO | N/A | LOW |
| COMMNDxx (auto commands) | Config | IPL-time commands | NO | N/A | LOW |
| MPFLSTxx (message processing) | Config | Message automation rules | NO | N/A | MEDIUM |
| PROGxx (APF/LNKLST/LPA) | Config | Program management parameters | NO | N/A | MEDIUM |
| SCHEDxx (PPT entries) | Config | Program properties | NO | N/A | LOW |
| SMFPRMxx (SMF parameters) | Config | SMF configuration | NO | Batch 14 | LOW |
| IKJTSOxx (TSO parameters) | Config | TSO configuration | NO | Batch 9 | MEDIUM |
| LPALSTxx (LPA libraries) | Config | Link Pack Area library list | NO | N/A | LOW |
| LNKLSTxx (linklist libraries) | Config | LNKLST concatenation | NO | N/A | MEDIUM |
| APFxx (APF authorized libraries) | Config | Authorized library list | NO | N/A | MEDIUM |
| SYS1.PARMLIB | Dataset | System parameter library | NO | N/A | MEDIUM |
| SYS1.PROCLIB | Dataset | Started task JCL procedures | PARTIAL (JCL) | N/A | MEDIUM |
| SYS1.LINKLIB | Dataset | System link library | NO | N/A | MEDIUM |
| SYS1.LPALIB | Dataset | Link Pack Area library | NO | N/A | LOW |
| SYS1.NUCLEUS | Dataset | System nucleus | NO | N/A | LOW |
| SYS1.MACLIB | Dataset | System macro library | PARTIAL (HLASM) | N/A | MEDIUM |
| SYS1.MODGEN | Dataset | System macro generation library | NO | N/A | LOW |
| SYS1.SAMPLIB | Dataset | Sample code library | NO | N/A | LOW |
| Master catalog | Config | Root catalog for all datasets | PARTIAL (dataset) | Batch 19 | HIGH |
| Catalog alias | Config | High-level qualifier to catalog mapping | PARTIAL (dataset) | Batch 19 | MEDIUM |
| Started tasks (JES2, VTAM, TSO, TCPIP, OMVS) | Convention | Standard started procedures | NO | N/A | MEDIUM |

### Complete SYS1.PARMLIB Member Reference

SYS1.PARMLIB is the master configuration library for z/OS. Each member (suffixed xx for multiple versions) controls a specific subsystem or system behavior. Below is the comprehensive inventory.

#### System Core Members
| Member | Full Name | Controls | Relevance to OpenMainframe | Priority |
|--------|-----------|----------|---------------------------|----------|
| IEASYSxx | System Parameters | Master system configuration — specifies other PARMLIB members, system limits, storage sizes | CRITICAL — defines entire system personality | HIGH |
| IEASYMxx | System Symbols | Static system symbols (&SYSNAME, &SYSPLEX, user symbols) used in all PARMLIB substitution | HIGH — enables parameterized config | MEDIUM |
| LOADxx | Load Parameters | IPL load parameters (IODF, NUCLEUS, SYS1.PARMLIB dataset names) | LOW — IPL-specific | LOW |
| NUCLSTxx | Nucleus Module List | Nucleus module selection | LOW — hardware-level | LOW |
| IEALPAxx | LPA Extensions | Additional Link Pack Area modules | MEDIUM — extends shared module pool | LOW |
| LPALSTxx | LPA Library List | Libraries concatenated to SYS1.LPALIB | MEDIUM — shared module pool | LOW |
| LNKLSTxx | Linklist Library List | Libraries concatenated to SYS1.LINKLIB (system search path) | HIGH — program search order | MEDIUM |
| PROGxx | Program Management | APF-authorized libraries, LNKLST updates, LPA updates, exits | HIGH — runtime program authorization | MEDIUM |
| APFxx | APF Authorization List | Legacy APF-authorized library list (replaced by PROGxx) | MEDIUM — legacy format | LOW |
| IEAFIXxx | Fixed LPA Modules | Modules to page-fix in LPA | LOW — performance tuning | LOW |

#### Subsystem Configuration Members
| Member | Full Name | Controls | Relevance to OpenMainframe | Priority |
|--------|-----------|----------|---------------------------|----------|
| IKJTSOxx | TSO Parameters | TSO authorized commands, programs, ALLOCATE defaults, logon procs | HIGH — TSO behavior | MEDIUM |
| SMFPRMxx | SMF Parameters | SMF record types to collect, dataset switching, exits | HIGH — audit/monitoring config | MEDIUM |
| CONSOLxx | Console Configuration | Console names, routing codes, authority levels | MEDIUM — operator interface | MEDIUM |
| COMMNDxx | Automatic Commands | Commands issued at IPL time | MEDIUM — system initialization | LOW |
| MPFLSTxx | Message Processing Facility | Message automation rules (suppress, highlight, automate) | MEDIUM — message handling | MEDIUM |
| SCHEDxx | Scheduler Parameters | Program Properties Table (PPT) entries — non-cancelable, key, privileged | MEDIUM — program attributes | LOW |
| IEADMRxx | DAE Parameters | Dump Analysis and Elimination parameters | LOW — dump management | LOW |
| ADYSETxx | Dump Automation | System dump options and automation | LOW — dump management | LOW |
| ALLOCxx | Allocation Parameters | Dynamic allocation defaults (unit, volume selection) | MEDIUM — dataset allocation | MEDIUM |

#### Security & Authorization Members
| Member | Full Name | Controls | Relevance to OpenMainframe | Priority |
|--------|-----------|----------|---------------------------|----------|
| ICHPRMxx | RACF Parameters | RACF initialization options | HIGH — security policy | MEDIUM |
| IRRPRMxx | RACF Additional | RACF operational parameters | MEDIUM — security config | LOW |
| CEAPRMxx | RACF PassTicket | PassTicket configuration | MEDIUM — middleware auth | LOW |
| CTICTRxx | CTRACE Parameters | Component trace options | LOW — diagnostics | LOW |

#### Storage & Memory Management Members
| Member | Full Name | Controls | Relevance to OpenMainframe | Priority |
|--------|-----------|----------|---------------------------|----------|
| IEASLPxx | SLP Parameters | Serviceability Level Indication Processing | LOW — dump control | LOW |
| IRAxx | Real Storage Management | Real storage frames, auxiliary storage | LOW — memory management | LOW |
| RSMxx | Real Storage Manager | RSM tuning parameters | LOW — memory tuning | LOW |
| CSAxx | Common Storage | CSA/ECSA limits | MEDIUM — shared storage | LOW |
| SQAxx | System Queue Area | SQA/ESQA limits | LOW — system storage | LOW |

#### Workload & Resource Management Members
| Member | Full Name | Controls | Relevance to OpenMainframe | Priority |
|--------|-----------|----------|---------------------------|----------|
| IEAICSxx | Installation Control | Installation Control Specification (legacy WLM) | LOW — legacy | LOW |
| IEAIPSxx | Installation Performance | Installation Performance Specification (legacy WLM) | LOW — legacy | LOW |
| IEAOPTxx | OPT Parameters | Operator Tuning parameters (SRM) | LOW — legacy tuning | LOW |
| WLMPOLxx | WLM Policy | Workload Manager service policy | MEDIUM — workload mgmt | LOW |

#### JES2 Related Members
| Member | Full Name | Controls | Relevance to OpenMainframe | Priority |
|--------|-----------|----------|---------------------------|----------|
| JES2PARM | JES2 Initialization | JES2 initialization parameters (in SYS1.PARMLIB or PROCLIB) | HIGH — JES2 config | MEDIUM |
| EXITxx | Exit List | Installation exit definitions for JES2 | MEDIUM — customization | LOW |

#### USS/OMVS Members
| Member | Full Name | Controls | Relevance to OpenMainframe | Priority |
|--------|-----------|----------|---------------------------|----------|
| BPXPRMxx | OMVS Parameters | USS kernel parameters (MAXPROCSYS, MAXFILEPROC, MAXTHREADS, filesystems, etc.) | HIGH — USS configuration | MEDIUM |
| BPXTMCxx | USS Security | USS security-related settings | MEDIUM — USS security | LOW |

#### Network & Communications Members
| Member | Full Name | Controls | Relevance to OpenMainframe | Priority |
|--------|-----------|----------|---------------------------|----------|
| EZBPRMxx | TCP/IP Policy Agent | Policy Agent configuration | MEDIUM — network security | LOW |
| ISTxx | VTAM Start Options | VTAM startup parameters | MEDIUM — legacy network | LOW |
| TDISKxx | Teleprocessing | TP disk allocation | LOW — legacy | LOW |

#### Sysplex & Coupling Members
| Member | Full Name | Controls | Relevance to OpenMainframe | Priority |
|--------|-----------|----------|---------------------------|----------|
| COUPLExx | Sysplex Couple | Coupling Facility structure definitions | LOW — sysplex | LOW |
| XCFxx | XCF Parameters | Cross-system Coupling Facility parameters | LOW — sysplex | LOW |
| CLOCKxx | Clock Parameters | System clock synchronization (ETR, STP) | LOW — hardware | LOW |

#### Logger & Data Management Members
| Member | Full Name | Controls | Relevance to OpenMainframe | Priority |
|--------|-----------|----------|---------------------------|----------|
| IXGCNFxx | System Logger | System Logger configuration (log streams, structure definitions) | MEDIUM — logging | LOW |
| IGDSMSxx | SMS Configuration | Storage Management Subsystem parameters | MEDIUM — DFSMS config | LOW |
| DEVSUPxx | Device Support | DASD and tape device parameters | LOW — device config | LOW |
| IGGCATxx | Catalog Parameters | Catalog management parameters | MEDIUM — dataset mgmt | LOW |

#### Crypto & PKI Members
| Member | Full Name | Controls | Relevance to OpenMainframe | Priority |
|--------|-----------|----------|---------------------------|----------|
| CSFPRMxx | ICSF Parameters | Integrated Cryptographic Service Facility options | MEDIUM — crypto | LOW |
| CTKPRMxx | PKCS#11 Token | PKCS#11 token configuration | LOW — PKI | LOW |

**Total PARMLIB members cataloged: 55+**
**Members with HIGH relevance to OpenMainframe: 12**
**Recommended for initial implementation: IEASYSxx, IEASYMxx, IKJTSOxx, PROGxx, LNKLSTxx, SMFPRMxx, BPXPRMxx, CONSOLxx, COMMNDxx, ALLOCxx**

### New Gaps Discovered

1. **PARMLIB Configuration Framework**
   - Classification: system_infrastructure
   - Description: A unified configuration system for OpenMainframe modeled after SYS1.PARMLIB. Members like IEASYSxx, IKJTSOxx, SMFPRMxx control subsystem behavior. Currently OpenMainframe uses zosmf.toml but a PARMLIB-like system would enable z/OS-compatible configuration. The complete member list above (55+ members) shows the scope of z/OS system parameterization. Initial implementation should focus on the 10 highest-relevance members.
   - Complexity: L (full), M (initial 10 members)
   - Priority: MEDIUM — Enhances fidelity but not required for basic operation
   - Dependencies: Subsystem implementations that each member controls

---

## AREA-16: IBM MQ Deep Dive

### Components Inventory

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| Queue Manager core | Core | MQCONN/MQOPEN/MQPUT/MQGET/MQCLOSE/MQDISC | PARTIAL | Batch 10 | HIGH |
| MQSC commands (DEFINE/ALTER/DELETE/DISPLAY) | Admin | Queue/channel administration | NO | Batch 10 | MEDIUM |
| Channel types (SDR/RCVR/SVR/RQSTR/SVRCONN) | Feature | MQ networking | NO | Batch 10 | MEDIUM |
| Trigger monitoring (CKTI) | Feature | Automatic application startup | NO | Batch 10 | MEDIUM |
| DLQ handler (CSQUDLQH) | Feature | Dead letter queue processing | NO | Batch 10 | LOW |
| MQ security (OAM) | Feature | Object Authority Manager | NO | Batch 10 | MEDIUM |
| MQ clustering | Feature | Cluster queues and workload balancing | NO | Batch 10 | LOW |
| Publish/subscribe | Feature | Topics and subscriptions | PARTIAL | Batch 10 | MEDIUM |
| MQ client connections | Feature | Client channel definition table (CCDT) | NO | Batch 10 | LOW |
| MQSUB/MQSUBRQ | API | Subscription management | NO | Batch 10 | LOW |
| MQINQ/MQSET | API | Queue attribute inquiry/modification | NO | Batch 10 | MEDIUM |

---

## AREA-17: SNA, VTAM & Network Subsystems

### Components Inventory

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| TN3270E protocol | Protocol | Terminal protocol over TCP/IP | YES (TUI crate) | Batch 20 | DONE |
| TCP/IP stack | Service | z/OS Communications Server | PARTIAL (host OS) | Batch 20 | MEDIUM |
| VTAM | Service | SNA network management | NO | Batch 20 | LOW |
| SNA LU types (LU0, LU2, LU6.2) | Protocol | SNA session types | NO | Batch 20 | LOW |
| Enterprise Extender (HPR/IP) | Service | SNA over IP | NO | Batch 20 | LOW |
| AT-TLS | Service | Application Transparent TLS | NO | Batch 20 | MEDIUM |
| FTP server/client | Service | z/OS FTP with MVS/HFS mode | NO | Batch 20 | MEDIUM |
| SSH/SFTP (OpenSSH) | Service | Secure shell access | NO | Batch 20 | MEDIUM |
| NFS server/client | Service | Network file system | NO | Batch 20 | LOW |
| SMTP server | Service | Email server | NO | Batch 20 | LOW |
| TCPIP PROFILE | Config | TCP/IP stack configuration | NO | Batch 20 | LOW |

---

## AREA-18: Crypto, PKI & Security Infrastructure

### Components Inventory

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| ICSF (Integrated Cryptographic Service Facility) | Product | Cryptographic services API | NO | N/A | MEDIUM |
| CSFENC/CSFDEC | API | Encrypt/decrypt services | NO | N/A | MEDIUM |
| PKI Services | Product | Certificate authority | NO | N/A | LOW |
| LDAP server | Service | Directory services | NO | N/A | LOW |
| Kerberos (Network Authentication Service) | Service | Kerberos authentication | NO | N/A | LOW |
| Dataset encryption | Feature | z/OS dataset encryption at rest | NO | N/A | LOW |
| Key management | Service | Cryptographic key lifecycle | NO | N/A | LOW |
| RACF key rings | Feature | Certificate/key storage | NO | Batch 8 | MEDIUM |

---

## AREA-19: SMF Complete Record Type Inventory

### Components Inventory (Key Record Types)

| SMF Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|----------|-------------|-------------------|-----------------|----------|
| Type 4 | Step termination | YES | Batch 14 | DONE |
| Type 5 | Job termination | YES | Batch 14 | DONE |
| Type 14 | Dataset input (read) | NO | Batch 14 | MEDIUM |
| Type 15 | Dataset output (write) | NO | Batch 14 | MEDIUM |
| Type 17 | Dataset scratch (delete) | NO | Batch 14 | LOW |
| Type 18 | Dataset rename | NO | Batch 14 | LOW |
| Type 26 | JES2 job purge | NO | Batch 14 | MEDIUM |
| Type 30 | Common address space work | YES | Batch 14 | DONE |
| Type 32 | TSO step termination | NO | Batch 14 | LOW |
| Type 40 | Dynamic allocation | NO | N/A | LOW |
| Type 42 | SMS status transition | NO | N/A | LOW |
| Type 60-66 | VSAM activity | NO | Batch 14 | LOW |
| Type 70 | RMF CPU activity | YES | Batch 14 | DONE |
| Type 80 | RACF processing | YES | Batch 14 | DONE |
| Type 83 | RACF dataset access | NO | Batch 14 | LOW |
| Type 89 | Usage data | NO | N/A | LOW |
| Type 90 | System status | NO | N/A | LOW |
| Type 100-102 | DB2 activity | NO | N/A | MEDIUM |
| Type 110 | CICS activity | NO | N/A | MEDIUM |
| Type 116 | MQ activity | NO | N/A | LOW |
| Type 118-119 | TCP/IP activity | NO | N/A | LOW |

---

## AREA-20: Miscellaneous z/OS Components

### Components Inventory

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| RRS (Resource Recovery Services) | Service | Two-phase commit coordination | NO | N/A | MEDIUM |
| System REXX (AXR) | Service | System-level REXX execution | NO | N/A | LOW |
| z/OSMF (beyond REST) | Product | Workflows, software mgmt, provisioning | PARTIAL (REST API) | N/A | LOW |
| z/OS Connect EE | Product | RESTful API creation from CICS/IMS | NO | N/A | LOW |
| Infoprint Server | Product | Print management (AFP/IPDS) | NO | N/A | LOW |
| zCX (Container Extensions) | Feature | Docker containers on z/OS | NO | N/A | LOW |
| IBM Z Open Automation Utilities | Product | Modern z/OS automation APIs | NO | N/A | LOW |
| SDSF REXX interface (ISFEXEC) | API | Programmatic SDSF access | NO | N/A | MEDIUM |
| ISREDIT (Edit macros) | API | ISPF editor macro interface | NO | Batch 9 | MEDIUM |
| z/OS Debugger | Product | Interactive debugging | NO | N/A | LOW |
| RMF (Resource Measurement Facility) | Product | Performance monitoring/reporting | NO | N/A | MEDIUM |
| Tivoli Workload Scheduler (TWS/OPC) | Product | Enterprise job scheduling | NO | N/A | LOW |
| z/OS JCL (advanced features) | Feature | JCL INCLUDE, JCLLIB, EXPORT/SET/IF-THEN-ELSE | PARTIAL | N/A | HIGH |

---

## Final Summary: z/OS Component Census

### Total Components Cataloged: 387
### Already in OpenMainframe: 48 (12.4%)
### In Existing Gap Analyses: 289 (74.7%)
### Newly Discovered Gaps: 50

### Top 20 Missing Components by Priority

| Rank | Component | Area | Priority | Rationale |
|------|-----------|------|----------|-----------|
| 1 | **DYNALLOC (SVC 99)** | AREA-1 | CRITICAL | TSO ALLOCATE/FREE, ISPF, compilers all depend on it |
| 2 | **WTO/WTOR** | AREA-1 | CRITICAL | Foundation of all operator communication and automation |
| 3 | **SDSF** | AREA-2 | CRITICAL | Most-used operator/developer tool on z/OS |
| 4 | **Binder (IEWL/IEWBLINK)** | AREA-3 | CRITICAL | Required to link-edit compiled programs into executables |
| 5 | **ABEND Code Framework** | AREA-11 | HIGH | Realistic error reporting for JCL/program execution |
| 6 | **COBOL DB2 Precompiler** | AREA-12 | HIGH | Most enterprise COBOL uses DB2 SQL |
| 7 | **COBOL CICS Translator** | AREA-12 | HIGH | Most CICS programs are COBOL |
| 8 | **Object/Load Module Format** | AREA-3 | HIGH | Required to load real z/OS executables |
| 9 | **Program Search Order (STEPLIB/JOBLIB)** | AREA-3 | HIGH | How z/OS finds programs for EXEC PGM= |
| 10 | **General Resource Class Framework** | AREA-6 | HIGH | 100+ resource classes for authorization beyond datasets |
| 11 | **BMS Maps (DFHMSD/DFHMDI/DFHMDF)** | AREA-14 | HIGH | Screen definitions for CICS applications |
| 12 | **DISPLAY Command Family** | AREA-2 | HIGH | System monitoring and troubleshooting |
| 13 | **DB2 Utilities (LOAD/UNLOAD/REORG)** | AREA-13 | HIGH | Database maintenance essential for production |
| 14 | **DB2 BIND (PLAN/PACKAGE)** | AREA-13 | HIGH | SQL access path binding |
| 15 | **JCL Converter/Interpreter (full)** | AREA-5 | HIGH | Full-fidelity JCL processing |
| 16 | **POSIX Shell (/bin/sh)** | AREA-9 | HIGH | USS shell for modern workloads |
| 17 | **zFS Filesystem** | AREA-9 | HIGH | POSIX filesystem for USS |
| 18 | **PARMLIB Configuration Framework** | AREA-15 | MEDIUM | z/OS-compatible system configuration |
| 19 | **PassTickets** | AREA-6 | MEDIUM | Programmatic authentication for middleware |
| 20 | **RRS (Resource Recovery Services)** | AREA-20 | MEDIUM | Two-phase commit for DB2/CICS/MQ coordination |

### Recommended New Epics (Beyond Existing 214)

| Epic ID | Area | Title | Size | Stories |
|---------|------|-------|------|---------|
| SYS-100 | AREA-1 | MVS System Services Core (DYNALLOC, WTO/WTOR, WAIT/POST, ENQ/DEQ) | L | 8-10 |
| SYS-101 | AREA-1 | ESTAE/ESPIE Recovery Framework | M | 4-6 |
| SYS-102 | AREA-2 | System Command Dispatcher & DISPLAY Commands | L | 8-10 |
| SYS-103 | AREA-2 | SDSF Implementation (DA, ST, O, H, LOG panels) | XL | 10-15 |
| SYS-104 | AREA-3 | Binder and Program Management (IEWBLINK, LOAD, LINK) | XL | 10-12 |
| SYS-105 | AREA-3 | Object/Load Module Format Parser | L | 6-8 |
| SYS-106 | AREA-4 | DFSMShsm Space Management (HMIGRATE/HRECALL/HBACKDS) | L | 8-10 |
| SYS-107 | AREA-4 | ADRDSSU Dataset Dump/Restore | L | 6-8 |
| SYS-108 | AREA-5 | JES2 Installation Exits Framework | M | 4-6 |
| SYS-109 | AREA-6 | RACF General Resource Class Framework | L | 6-8 |
| SYS-110 | AREA-6 | RACF PassTickets and Digital Certificates | M | 4-6 |
| SYS-111 | AREA-11 | ABEND Code Framework & Dump DD Support | M | 4-6 |
| SYS-112 | AREA-12 | COBOL DB2 Precompiler Integration | L | 6-8 |
| SYS-113 | AREA-12 | COBOL CICS Translator Integration | L | 6-8 |
| SYS-114 | AREA-13 | DB2 Utility Suite (LOAD/UNLOAD/REORG/RUNSTATS) | XL | 10-12 |
| SYS-115 | AREA-13 | DB2 BIND/DBRM Processing | L | 6-8 |
| SYS-116 | AREA-14 | CICS BMS Map Support (DFHMSD/DFHMDI/DFHMDF) | L | 6-8 |
| SYS-117 | AREA-15 | PARMLIB Configuration Framework | M | 4-6 |
| SYS-118 | AREA-9 | POSIX Shell and Core Utilities | XL | 10-15 |
| SYS-119 | AREA-9 | zFS Filesystem Implementation | L | 8-10 |

**Total new epics: 20** (adding to 214 existing = 234 total)
**Total new stories: ~140** (adding to existing backlog)

### Updated Phase Recommendations

**Adjustments to Phase A-F from Batch 22:**

**Add to Phase A (Foundation):**
- SYS-100 (MVS System Services Core) — DYNALLOC, WTO/WTOR unlock many downstream features
- SYS-111 (ABEND Code Framework) — Quick win that improves all error handling
- SYS-105 (Object/Load Module Format) — Enables loading compiled output

**Add to Phase B (Interactive):**
- SYS-103 (SDSF) — Highest user-visible value for ISPF users
- SYS-102 (System Commands) — Operator tooling

**Add to Phase C (Middleware):**
- SYS-112 + SYS-113 (COBOL preprocessors) — Enables real COBOL/DB2/CICS workloads
- SYS-116 (BMS Maps) — Enables CICS screen-based applications
- SYS-114 + SYS-115 (DB2 utilities/bind) — Enables DB2 administration

**Add to Phase D (Extended):**
- SYS-104 (Binder) — Full program management pipeline
- SYS-118 + SYS-119 (USS shell + zFS) — Modern workload enablement

**Defer to Phase E+:**
- SYS-106 + SYS-107 (HSM, ADRDSSU) — Operational tooling
- SYS-108 (JES2 exits) — Customization framework
- SYS-117 (PARMLIB) — Configuration fidelity

---

## Cross-Reference: Research Areas ↔ Existing Gap Analysis Batches

This section maps each research area in this inventory to the corresponding gap analysis batch(es) from the existing Batch 1-22 series (214 epics, 1,733+ features). It identifies where existing analysis already covers the area, where this inventory adds new components, and where the new SYS-100+ epics fill gaps.

### Cross-Reference Matrix

| Research Area | Primary Gap Batch | Epic Range | Coverage | New SYS Epics | Notes |
|---------------|-------------------|------------|----------|---------------|-------|
| AREA-1: MVS System Services & SVCs | — (none) | — | **NO EXISTING COVERAGE** | SYS-100, SYS-101 | Major gap — DYNALLOC, WTO, ENQ, ESTAE had no batch |
| AREA-2: System Commands & Console | — (none) | — | **NO EXISTING COVERAGE** | SYS-102, SYS-103 | SDSF and MVS commands had no batch |
| AREA-3: Program Management & Loader | — (none) | — | **NO EXISTING COVERAGE** | SYS-104, SYS-105 | Binder, load modules, program fetch had no batch |
| AREA-4: Data Management / DFSMS | Batch 19 | DFSMS100–110 | HIGH (SMS, catalog, IDCAMS) | SYS-106, SYS-107 | HSM space mgmt and ADRDSSU not in Batch 19 |
| AREA-5: JES2 Deep Dive | Batch 11 | J100–J112 | HIGH (job queue, spool, output) | SYS-108 | JES2 exits/user mods lightly covered |
| AREA-6: RACF Deep Dive | Batch 08 | RACF100–109 | HIGH (core, SAF, profiles) | SYS-109, SYS-110 | General resource classes & PassTickets need expansion |
| AREA-7: Batch Processing Ecosystem | Batch 11 + Batch 17 | J100–J112, WLM100–111 | MEDIUM (JES2 + WLM) | — | JCL advanced features (IF/THEN, INCLUDE) partially covered |
| AREA-8: TSO/ISPF Deep Dive | Batch 09 | T100–T112 | HIGH (commands, panels, dialog, editor) | — | Well covered in existing batch |
| AREA-9: z/OS UNIX (USS) | Batch 18 | USS100–111 | HIGH (filesystem, process, shell, sockets) | SYS-118, SYS-119 | POSIX shell and zFS need dedicated epics |
| AREA-10: System Logger & Sysplex | — (none) | — | **NO EXISTING COVERAGE** | — | Sysplex is out of scope for single-system emulator |
| AREA-11: Debugging & Problem Determination | — (none) | — | **NO EXISTING COVERAGE** | SYS-111 | ABEND framework needed; dump analysis low priority |
| AREA-12: Compilers & Language Toolchain | Batch 01–07 | R100–R111, H100–H110, PL100–PL112, etc. | HIGH (interpreters/parsers) | SYS-112, SYS-113 | Precompilers (DB2/CICS) not in language batches |
| AREA-13: DB2, IMS & Data Server | Batch 13 | IMS100–109 | PARTIAL (IMS TM only) | SYS-114, SYS-115 | DB2 utilities and BIND had no dedicated batch |
| AREA-14: CICS Deep Dive | — (none) | — | **NO EXISTING COVERAGE** | SYS-116 | CICS crate exists but no gap batch; BMS maps critical |
| AREA-15: System Init & Configuration | — (none) | — | **NO EXISTING COVERAGE** | SYS-117 | PARMLIB framework needed for system-level config |
| AREA-16: IBM MQ Deep Dive | Batch 10 | MQ100–MQ112 | HIGH (queue manager, channels, pub/sub) | — | Well covered in existing batch |
| AREA-17: SNA, VTAM & Networking | Batch 20 | NET100–NET109 | HIGH (TCP/IP, TLS, protocols) | — | SNA/VTAM low priority; TCP/IP well covered |
| AREA-18: Crypto, PKI & Security Infra | — (none) | — | **NO EXISTING COVERAGE** | — | ICSF/RACF digital certificates partially in Batch 08 |
| AREA-19: SMF Complete Record Types | Batch 14 | SMF100–SMF110 | HIGH (infrastructure, writing, exits) | — | Well covered in existing batch |
| AREA-20: Miscellaneous Components | Various | — | LOW | — | RRS, zCX, z/OS Debugger not in any batch |

### Coverage Gap Summary

**Areas with NO existing gap analysis batch (biggest finds from this research):**
1. **AREA-1: MVS System Services** — The most fundamental gap. DYNALLOC (SVC 99), WTO/WTOR, ATTACH/DETACH, WAIT/POST, ENQ/DEQ, ESTAE/ESPIE are foundational services used by virtually every z/OS subsystem. No existing batch covers these.
2. **AREA-2: System Commands** — DISPLAY, VARY, CANCEL, FORCE, START, STOP commands and SDSF panels. No existing batch covers operator interface.
3. **AREA-3: Program Management** — Binder (IEWBLINK), LOAD/LINK macros, load module format, APF authorization, program search order. No existing batch covers the program lifecycle.
4. **AREA-11: Debugging** — ABEND code framework, SNAP dumps, SYSUDUMP/SYSABEND/SYSMDUMP DDs. No existing batch covers problem determination.
5. **AREA-14: CICS** — Despite the CICS crate existing in OpenMainframe, there was no gap analysis batch for CICS. This research catalogs ~90 EXEC CICS commands.
6. **AREA-15: System Init** — PARMLIB members (55+ cataloged), IPL process, system datasets. No existing batch covers system configuration.

**Areas well-covered by existing batches (validation):**
- AREA-4/DFSMS → Batch 19
- AREA-5/JES2 → Batch 11
- AREA-6/RACF → Batch 08
- AREA-8/TSO-ISPF → Batch 09
- AREA-9/USS → Batch 18
- AREA-12/Languages → Batches 01–07
- AREA-16/MQ → Batch 10
- AREA-17/Networking → Batch 20
- AREA-19/SMF → Batch 14

### Existing Epic ↔ New SYS Epic Dependency Map

These SYS epics unlock or complement existing epics:

| New Epic | Enables / Complements | Relationship |
|----------|----------------------|--------------|
| SYS-100 (MVS Services) | J100–J112 (JES2), T100–T112 (TSO), RACF100–109 | Foundation — DYNALLOC/WTO used by JES2, TSO, RACF |
| SYS-101 (ESTAE/ESPIE) | LE100 (LE Condition Handling) | Complement — ESTAE is the MVS-level analog of LE CEEHDLR |
| SYS-102 (System Commands) | J100 (JES2 scheduling) | Complement — MVS commands interact with JES2 |
| SYS-103 (SDSF) | J102 (JES2 Spool), T100 (TSO) | Depends on — SDSF requires JES2 spool and TSO infrastructure |
| SYS-104 (Binder) | H108 (HLASM Object Deck), PL100+ (PL/I output) | Depends on — Binder consumes assembler/compiler output |
| SYS-105 (Load Module Format) | SYS-104 (Binder) | Chain — Binder produces what load module format defines |
| SYS-109 (RACF Resources) | RACF100 (Core) | Extends — General resources beyond dataset profiles |
| SYS-111 (ABEND Framework) | LE100 (LE Condition Handling), J100 (JES2) | Complement — ABEND codes used in JCL and LE |
| SYS-112 (DB2 Precompiler) | PL100 (PL/I), R100 (REXX) | Extends — Adds SQL support to language interpreters |
| SYS-113 (CICS Translator) | CL100 (CLIST), R100 (REXX) | Extends — Adds EXEC CICS support to language interpreters |
| SYS-114 (DB2 Utilities) | DFSMS103 (IDCAMS) | Parallel — DB2 utils are analogous to VSAM utils |
| SYS-116 (BMS Maps) | T103 (ISPF Panel Language) | Parallel — BMS maps are CICS equivalent of ISPF panels |
| SYS-118 (POSIX Shell) | USS100–USS111 | Extends — Shell requires USS filesystem and process model |
| SYS-119 (zFS) | USS100 (USS File System) | Extends — zFS is the modern USS filesystem layer |

### OpenMainframe Crate ↔ Research Area Map

| OpenMainframe Crate | Research Areas | Gap Batches | SYS Epics |
|---------------------|---------------|-------------|-----------|
| cobol-rs | AREA-12 | (no batch — COBOL assumed built-in) | SYS-112, SYS-113 |
| jcl-rs | AREA-5, AREA-7 | Batch 11 | — |
| rexx-rs | AREA-12 | Batch 01 | — |
| hlasm-rs | AREA-12 | Batch 02 | — |
| pli-rs | AREA-12 | Batch 03 | — |
| easytrieve-rs | AREA-12 | Batch 04 | — |
| clist-rs | AREA-12 | Batch 05 | — |
| natural-rs | AREA-12 | Batch 06 | — |
| focus-rs | AREA-12 | Batch 07 | — |
| racf-rs | AREA-6, AREA-18 | Batch 08 | SYS-109, SYS-110 |
| tso-rs | AREA-8 | Batch 09 | — |
| ispf-rs | AREA-8 | Batch 09 | — |
| mq-rs | AREA-16 | Batch 10 | — |
| jes2-rs | AREA-5 | Batch 11 | SYS-108 |
| le-rs | AREA-12 | Batch 12 | — |
| ims-rs | AREA-13 | Batch 13 | — |
| smf-rs | AREA-19 | Batch 14 | — |
| idms-rs | AREA-12 | Batch 15 | — |
| adabas-rs | AREA-12 | Batch 16 | — |
| cics-rs | AREA-14 | — (no batch!) | SYS-116 |
| vsam-rs | AREA-4 | Batch 19 | — |
| zosmf-rs | AREA-20 | — | — |
| tui-rs | AREA-17 | Batch 20 | — |
| zos-core | AREA-1, AREA-2, AREA-3, AREA-10, AREA-11, AREA-15 | — (no batches!) | SYS-100–105, SYS-111, SYS-117 |

**Key insight**: The `zos-core` crate (or a new set of core crates) is where 7 of the 20 new SYS epics would live, and NONE of these areas had existing gap analysis batches. This confirms the research found the right gaps.
