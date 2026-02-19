# Gap Analysis: JES2 (Job Entry Subsystem 2)

## Official Specification Summary

JES2 (Job Entry Subsystem 2) is the primary job entry subsystem for IBM z/OS, originally derived from HASP II. JES2 manages the complete lifecycle of batch jobs: receiving them into the system, converting JCL, scheduling execution, managing spool storage for input and output, and controlling output processing. JES2 is one of the most fundamental z/OS subsystems — without it, no batch jobs can run.

JES2 is classified as **Core** (essential infrastructure) on mainframes:
- Processes every batch job submitted to z/OS — the central scheduler for all batch work
- Manages spool storage (HASPACE) for job input, output (SYSOUT), and system messages
- Provides job queuing with priority-based scheduling across job classes
- Controls initiators (classic JES2-managed or WLM-managed) that run batch jobs
- Supports multi-system configurations via Multi-Access Spool (MAS)
- Network Job Entry (NJE) enables job and output routing between z/OS systems
- JECL (Job Entry Control Language) statements extend JCL with JES2-specific directives
- ~70 initialization statements configure all JES2 behavior
- Over 50 operator commands ($D, $S, $P, $A, $C, $T, etc.) for real-time control
- ~73+ IBM-defined installation exit points for customization
- Five processing phases: Input → Conversion → Execution → Output → Purge

Key documentation:
- **z/OS JES2 Introduction (SA32-0994)** — concepts and architecture
- **z/OS JES2 Commands (SA32-0990)** — operator commands
- **z/OS JES2 Initialization and Tuning Reference (SA32-0992)** — initialization statements
- **z/OS JES2 Initialization and Tuning Guide (SA32-0991)** — configuration guide
- **z/OS JES2 Installation Exits (SA32-0995)** — exit programming

## Key Features & Capabilities

### 1. Job Processing Phases

| Phase | Description |
|-------|-------------|
| **Input** | JCL read from card reader, internal reader, TSO SUBMIT, or NJE; stored on spool |
| **Conversion** | JCL scanned by converter/interpreter; JECL processed; internal text (SWA) created |
| **Execution** | Job dispatched to initiator; steps execute; SYSOUT created on spool |
| **Output** | SYSOUT datasets routed to printers, punches, or held for viewing via SDSF |
| **Purge** | Job and all spool space released after output complete or operator cancel |

### 2. JES2 Operator Commands (~50+ commands)

#### Display Commands ($D)
| Command | Purpose |
|---------|---------|
| $D JOBn | Display job status/attributes |
| $D Qjobname | Display job by name |
| $D I | Display all initiators and classes |
| $D I(n) | Display specific initiator |
| $D PRT(n) | Display printer status |
| $D PUN(n) | Display punch status |
| $D LINE(n) | Display communication line |
| $D NODE(n) | Display NJE node |
| $D SPOOL | Display spool volume status |
| $D SPOOLDEF | Display spool configuration |
| $D JOBCLASS(c) | Display job class definition |
| $D OUTCLASS(c) | Display output class definition |
| $D INITDEF | Display initiator configuration |
| $D JOBDEF | Display job defaults |
| $D OUTDEF | Display output defaults |
| $D MASDEF | Display MAS configuration |
| $D NJEDEF | Display NJE configuration |
| $D CKPTDEF | Display checkpoint configuration |
| $D CKPTSPACE | Display checkpoint space usage |
| $D EXIT(nnn) | Display exit point status |
| $D MEMBER(n) | Display MAS member |
| $D JES2 | Display JES2 address space activity |
| $D JQ | Display job queue summary |
| $D JQ,DAYS>n | Display jobs older than n days |
| $D DUPJOB | Display duplicate job names |
| $D A | Display active jobs |
| $D U | Display output queue |

#### Start Commands ($S)
| Command | Purpose |
|---------|---------|
| $S I(n) | Start initiator |
| $S PRT(n) | Start printer |
| $S PUN(n) | Start punch |
| $S RDR(n) | Start reader |
| $S LINE(n) | Start communication line |
| $S INTRDR | Start internal reader |

#### Stop/Drain Commands ($P)
| Command | Purpose |
|---------|---------|
| $P I(n) | Stop initiator after current job |
| $P PRT(n) | Stop printer after current output |
| $P PUN(n) | Stop punch |
| $P JOBn | Purge job |
| $P JES2 | Stop JES2 (orderly shutdown) |
| $P JES2,ABEND | Force JES2 termination |

#### Cancel Commands ($C)
| Command | Purpose |
|---------|---------|
| $C JOBn | Cancel current job |
| $C PRT(n) | Cancel current printer output |
| $C PUN(n) | Cancel current punch output |

#### Release/Activate Commands ($A)
| Command | Purpose |
|---------|---------|
| $A JOBn | Release job from hold |
| $A Q(class) | Release all held jobs in class |

#### Modify Commands ($T)
| Command | Purpose |
|---------|---------|
| $T I(n),C=(classes) | Change initiator job classes |
| $T JOBn,C=class | Change job class |
| $T JOBn,P=priority | Change job priority |
| $T JOBCLASS(c),params | Modify job class definition |
| $T OUTCLASS(c),params | Modify output class definition |
| $T SPOOLDEF,params | Modify spool configuration |
| $T EXIT(nnn),params | Modify exit point configuration |
| $T JECLDEF,params | Modify JECL processing options |

#### Hold Commands ($H)
| Command | Purpose |
|---------|---------|
| $H JOBn | Hold job |
| $H Q(class) | Hold all jobs in class |

#### Other Commands
| Command | Purpose |
|---------|---------|
| $E JOBn | Restart job |
| $Z JES2 | Halt JES2 (immediate) |
| $L JOBn | List job information |
| $B PRT(n) | Backspace printer |
| $F PRT(n) | Forward printer |
| $I PRT(n) | Interrupt printer |
| $ACTIVATE | Activate JES2 z/OS features |
| $JDSTATUS | Display JES2 health |
| $JDDETAILS | Display JNUM/JOES/BERT utilization |
| $VS,'cmd' | Issue MVS command via JES2 |
| $DN,Q=class | Display spool utilization by class |

### 3. JECL (Job Entry Control Language) Statements

| Statement | Purpose |
|-----------|---------|
| /*JOBPARM | Job-level parameters: SYSAFF (system affinity), PROCLIB, TIME, LINES, PAGES, CARDS, BYTES, COPIES, FORMS, LINECT |
| /*ROUTE PRINT | Route SYSOUT to specific node/user destination |
| /*ROUTE XEQ | Route job execution to specific node |
| /*OUTPUT | Output descriptor: CLASS, DEST, COPIES, FORMS, WRITER, BURST, CHARS, FCB |
| /*PRIORITY | Set job scheduling priority |
| /*SETUP | Request volume mount before execution |
| /*SIGNOFF | End remote session |
| /*SIGNON | Start remote session (RJE) |
| /*MESSAGE | Send message to operator |
| /*NETACCT | Network accounting information |
| /*NOTIFY | Request notification on job completion |
| /*XEQ | Route job to execution node (NJE) |
| /*XMIT | Transmit job/data to another node (NJE) |

### 4. Spool Management

| Feature | Description |
|---------|-------------|
| HASPACE | JES2 spool datasets on direct access volumes |
| Spool volumes | SPOOL(vvvvvv) — define spool extents on DASD volumes |
| SPOOLDEF | Global spool configuration (TGSPACE, TGSIZE, LARGEDS) |
| Track groups | Allocation unit for spool data (default 3 tracks) |
| Spool partitioning | Separate spool for different job classes/purposes |
| Checkpoint (CKPT) | Dual checkpoint datasets for recovery (hot standby) |
| Spool offload | OFFLOAD.JT/JR/ST/SR — offload/reload spool data to/from tape |
| BERT (Buffer Element Resource Table) | Tracks spool buffer allocation |
| JOE (Job Output Element) | Tracks individual SYSOUT datasets on spool |
| $HASP messages | JES2 system messages (HASP001-HASP999) |

### 5. Output Processing

| Feature | Description |
|---------|-------------|
| SYSOUT classes | A-Z, 0-9 — output classification (36 classes) |
| Output descriptors | Named output processing specifications |
| MSGCLASS | Job log/messages output class |
| Printers (PRT) | Local and remote printers managed by JES2 |
| Punches (PUN) | Card punch devices |
| PSF (Print Services Facility) | AFP advanced print processing via FSS |
| External writers | User-written output processing programs |
| FSS (Functional Subsystem) | Separate address space for output processing (e.g., PSF) |
| Forms/FCB/UCS | Paper forms, Forms Control Buffer, Universal Character Set |
| Output groups | Group SYSOUT datasets for processing together |
| OUTDISP | Output disposition: WRITE/HOLD/KEEP/PURGE |
| DEST | Destination routing for SYSOUT (node.userid) |

### 6. Initiators and Job Scheduling

| Feature | Description |
|---------|-------------|
| JES2-managed initiators | Classic model: INIT(nnnn) definitions with class assignments |
| WLM-managed initiators | Modern model: WLM starts/stops initiators based on service goals |
| Job classes | A-Z, 0-9 — job classification (36 classes + STC + TSU) |
| Class groups | Multiple classes per initiator for flexible scheduling |
| Job priority | 0-15 scheduling priority within a class |
| SYSAFF | System affinity — restrict job to specific MAS member(s) |
| Scheduling environments | WLM scheduling environments for specialized resources |
| Dependent job control (DJC) | Job dependencies via /*JOBPARM DJCKEY |
| TYPRUN=HOLD | Submit job in held state |
| TYPRUN=SCAN | Syntax check only, no execution |
| TYPRUN=COPY | Copy job to internal reader |

### 7. NJE (Network Job Entry)

| Feature | Description |
|---------|-------------|
| Nodes | NODE(nnnn) — each z/OS system is a node in the NJE network |
| NJEDEF | NJE configuration (OWNNODE, NODENUM, MAXHOPS, RESTTOL) |
| Connectivity | BSC, SNA (VTAM), TCP/IP for inter-node communication |
| NETSERV | TCP/IP NJE server address space |
| Job routing | Submit jobs for execution on remote nodes (/*ROUTE XEQ) |
| Output routing | Route SYSOUT to remote nodes (/*ROUTE PRINT) |
| Store-and-forward | Intermediate nodes relay jobs/output to final destination |
| CONNECT | Static connection definitions between nodes |
| Multi-hop | Jobs can traverse multiple nodes to reach destination |
| NJE security | RACF NODES class for controlling NJE access |

### 8. Multi-Access Spool (MAS)

| Feature | Description |
|---------|-------------|
| MAS complex | Multiple JES2 images sharing spool volumes in a sysplex |
| MASDEF | MAS configuration (OWNMEMB, XCFGRPNM) |
| MEMBER(n) | Individual MAS member definitions |
| XCF group | Cross-system Coupling Facility group for MAS communication |
| Shared spool | All MAS members access common spool datasets |
| Shared checkpoint | Coordinated checkpoint across MAS members |
| Job affinity | SYSAFF restricts job to specific MAS member(s) |
| Workload distribution | Jobs can run on any MAS member with matching class/affinity |

### 9. JES2 Initialization Statements (~70)

Key categories and statements:

#### Job Configuration
| Statement | Purpose |
|-----------|---------|
| JOBCLASS(c\|STC\|TSU) | Job class definitions (PROCLIB, OUTDISP, MSGCLASS, TIME, AUTH) |
| JOBDEF | Job processing defaults |
| INPUTDEF | Input processing options |
| OUTCLASS(v) | Output class definitions |
| OUTDEF | Output processing defaults |
| OUTPRTY(n) | Output scheduling priorities |
| PRINTDEF | Print environment configuration |

#### Spool Configuration
| Statement | Purpose |
|-----------|---------|
| SPOOL(vvvvvv) | Spool volume definitions |
| SPOOLDEF | Global spool configuration (TGSPACE, TGSIZE, LARGEDS) |
| CKPTDEF | Checkpoint dataset definitions |
| CKPTSPACE | Checkpoint space configuration |
| BUFDEF | JES2 buffer definitions |

#### Device Configuration
| Statement | Purpose |
|-----------|---------|
| INIT(nnnn) | Initiator definitions |
| INITDEF | Initiator count and defaults |
| PRT(nnnnn) | Printer definitions |
| PUN(nnnnn) | Punch definitions |
| RDR(nnnnn) | Reader definitions |
| FSS(name) | Functional subsystem definitions |

#### Network Configuration
| Statement | Purpose |
|-----------|---------|
| NJEDEF | NJE global configuration |
| NODE(nnnn) | NJE node definitions |
| CONNECT | Static NJE connections |
| LINE(nnnnn) | Communication line definitions |
| NETSERV | TCP/IP NJE server |
| NETACCT | Network account mapping |
| DESTDEF | Destination definitions |
| SOCKET(name) | TCP/IP socket definitions |
| RMT(nnnnn) | Remote workstation definitions |
| APPL(name) | SNA NJE application definitions |

#### MAS Configuration
| Statement | Purpose |
|-----------|---------|
| MASDEF | MAS global configuration |
| MEMBER(n) | MAS member definitions |

#### System Configuration
| Statement | Purpose |
|-----------|---------|
| CONDEF | Console communication environment |
| EXIT(nnn) | Exit point definitions |
| GRPDEF | Group definitions |
| INTRDR | Internal reader characteristics |
| JECLDEF | JECL processing options |
| PCEDEF | Processor control elements |
| PROCLIB(name) | Dynamic procedure library concatenations |
| REDIRECT | Redirection rules |
| SMFDEF | SMF buffer definitions |
| SRVCLASS | Service class definitions |
| SUBTDEF | Subsystem task definitions |
| POLICYLIB | Policy import concatenation |

#### Estimation / Limits
| Statement | Purpose |
|-----------|---------|
| ESTBYTE | Estimate output bytes |
| ESTLNCT | Estimate line count |
| ESTIME | Estimate execution time |
| ESTPUN | Estimate punched output |

### 10. JES2 Installation Exits (~73+ IBM-defined)

Key exits (EXIT001–EXIT073+):

| Exit | Purpose |
|------|---------|
| EXIT001 | Print/punch separator page processing |
| EXIT002 | JCL/JECL scan — job statement processing |
| EXIT003 | JCL/JECL scan — continuation |
| EXIT004 | JCL and JES2 control statement scan |
| EXIT005 | JES2 command preprocessing |
| EXIT006 | JES2 command post-processing |
| EXIT007 | JCL converter — control statement modification |
| EXIT008 | Control block (JCT) initialization |
| EXIT009-012 | NJE job/SYSOUT transmission/reception |
| EXIT013-014 | Allocation/unallocation processing |
| EXIT015-016 | Output processing (writer selection, output limit) |
| EXIT017-020 | TSO session management |
| EXIT021-024 | Spool management and offloading |
| EXIT025-030 | Scheduling and selection exits |
| EXIT031-040 | Security, accounting, and audit exits |
| EXIT041-050 | Output formatting and processing |
| EXIT051-073+ | Various specialized exit points |

Exits support up to 256 total (EXIT001–EXIT255), with IBM defining ~73+ standard exit points. Installations can use remaining numbers for custom exits.

### 11. JES2 Internal Control Blocks

| Control Block | Description |
|---------------|-------------|
| JCT (Job Control Table) | Primary job control block — one per job |
| JQE (Job Queue Element) | Queue entry for job scheduling |
| IOT (I/O Table) | Spool I/O tracking |
| PDDB (Peripheral Data Definition Block) | SYSOUT dataset definition |
| SJB (Scheduler Job Block) | Scheduler interface control |
| JOE (Job Output Element) | Individual output dataset tracking |
| BERT (Buffer Element Resource Table) | Buffer management |
| CMB (Console Message Buffer) | Operator message handling |

### 12. JES2 Start Types

| Start Type | Description |
|------------|-------------|
| Cold start | Full initialization; rebuilds all queues from scratch; spool cleared |
| Warm start | Normal restart; preserves queued jobs and spool data |
| Quick start | Minimal restart; skips optional initialization |
| Hot start | Resume after brief interruption; minimal reprocessing |

## Current OpenMainframe Status

**No JES2 implementation exists.** However, several JES2-adjacent capabilities exist in the JCL crate:

### Partial Implementations Found

1. **JOB CLASS parameter** — `crates/open-mainframe-jcl/src/ast/mod.rs:153`
   - `class: Option<char>` in `JobParams` struct
   - Parsed from JOB card but not used for scheduling

2. **MSGCLASS parameter** — `crates/open-mainframe-jcl/src/ast/mod.rs:155`
   - `msgclass: Option<char>` in `JobParams` struct
   - Parsed but not used for output routing

3. **SYSOUT DD definition** — `crates/open-mainframe-jcl/src/ast/mod.rs:583`
   - `SysoutDef` struct with `class`, `writer`, `form` fields
   - Parsed but no queue management or class-based dispatching

4. **OUTPUT statement** — `crates/open-mainframe-jcl/src/ast/mod.rs:413`
   - `OutputStatement` struct with `class`, `dest`, `copies`, `forms`, `writer`
   - Parser support exists; no JES2 output processing

5. **TYPRUN parameter** — `crates/open-mainframe-jcl/src/ast/mod.rs:177`
   - `TypeRun` enum: `Run`, `Hold`, `Scan`, `Copy`
   - Parsed but none of the special behaviors implemented

6. **SYSOUT file creation** — `crates/open-mainframe-jcl/src/executor/mod.rs:331`
   - SYSOUT DD files created as `{ddname}_{stepidx}.{class}.txt`
   - Simple file creation — no spool, no output queue, no routing

7. **Dataset locking** — `crates/open-mainframe-dataset/src/locking.rs`
   - Multi-job dataset serialization with shared/exclusive locks
   - Foundation for concurrent job execution, but not integrated with any scheduler

### What's Missing
- No job queue or scheduling infrastructure
- No spool management system
- No initiator/processor model
- No JES2 commands
- No JECL statement processing
- No NJE networking
- No installation exits framework
- No output routing or printer management
- Job execution is synchronous, single-threaded, immediate

## Gap Details

| # | Feature | Official z/OS | OpenMainframe | Gap |
|---|---------|--------------|---------------|-----|
| 1 | Job queue and scheduling engine | Priority queue with classes/affinity | None — jobs execute immediately | **Missing** |
| 2 | Spool management (HASPACE) | Multi-volume spool with track groups | None | **Missing** |
| 3 | Input processing (converter/interpreter) | JCL→internal text, JECL scan | JCL parsed to AST (partial) | **Partial** |
| 4 | Job CLASS scheduling | 36 classes (A-Z, 0-9) + STC + TSU | CLASS parsed but unused | **Partial** |
| 5 | MSGCLASS routing | Message class → output class mapping | MSGCLASS parsed but unused | **Partial** |
| 6 | SYSOUT dataset management | Spooled output with class/dest/forms | Files written to disk (no queue) | **Partial** |
| 7 | OUTPUT statement processing | Full output descriptor support | OUTPUT struct exists, no processing | **Partial** |
| 8 | TYPRUN (HOLD/SCAN/COPY) | Hold, syntax scan, copy behaviors | Enum parsed, behaviors not implemented | **Partial** |
| 9 | Initiators (JES2-managed) | INIT(nnnn) with class assignments | None | **Missing** |
| 10 | WLM-managed initiators | Dynamic initiator management | None | **Missing** |
| 11 | JES2 operator commands ($D/$S/$P/$A/$C/$T) | 50+ commands for real-time control | None | **Missing** |
| 12 | JECL statements (/*JOBPARM, /*ROUTE, etc.) | 13+ JECL statements | None — not recognized by lexer | **Missing** |
| 13 | Output processing (printers/writers) | PRT/PUN/FSS/external writers | None | **Missing** |
| 14 | PSF (Print Services Facility) | AFP advanced function printing | None | **Missing** |
| 15 | NJE (Network Job Entry) | Multi-node job/output routing | None | **Missing** |
| 16 | Multi-Access Spool (MAS) | Shared spool across sysplex members | None | **Missing** |
| 17 | Job priority scheduling | 0-15 priority within class | None | **Missing** |
| 18 | System affinity (SYSAFF) | Restrict job to specific MAS members | None | **Missing** |
| 19 | Dependent job control (DJC) | Job dependency chains | None | **Missing** |
| 20 | Scheduling environments | WLM resource-based scheduling | None | **Missing** |
| 21 | Spool offload/reload | OFFLOAD.JT/JR/ST/SR | None | **Missing** |
| 22 | Checkpoint/restart | Dual CKPT for recovery | None | **Missing** |
| 23 | Installation exits (EXIT001-073+) | 73+ customization points | None | **Missing** |
| 24 | Initialization statements (~70) | Full JES2PARM configuration | None | **Missing** |
| 25 | $HASP messages | JES2 system message handling | None | **Missing** |
| 26 | Internal control blocks (JCT/JQE/IOT/PDDB) | Job tracking data structures | None | **Missing** |
| 27 | JES2 start types (cold/warm/quick/hot) | Multiple startup modes | None | **Missing** |
| 28 | Output descriptors and groups | Named output specifications | OUTPUT struct exists (partial) | **Partial** |
| 29 | Forms/FCB/UCS management | Print formatting control | None | **Missing** |
| 30 | SDSF integration | System Display and Search Facility | None | **Missing** |
| 31 | Internal reader (INTRDR) | Submit jobs from within jobs | None | **Missing** |
| 32 | Dataset locking for multi-job | Enqueue/dequeue serialization | Locking framework exists (unused) | **Partial** |

**Total: 32 major gaps — 7 partial implementations, 25 missing**

## Proposed Epic Structure

### J100 — Job Queue & Scheduling Engine (XL)
- Job queue data structure (priority queue by class and priority)
- Job states: Input → Conversion → Ready → Running → Output → Purge
- Job class definitions (A-Z, 0-9, STC, TSU) with attributes
- Priority scheduling (0-15 within class)
- Job selection algorithm (class, priority, affinity)
- Job hold/release mechanism
- Dependent job control (DJC) scheduling
- TYPRUN=HOLD, TYPRUN=SCAN, TYPRUN=COPY behaviors
- Integration with existing JCL executor
- **Depends on**: open-mainframe-jcl (AST, executor)

### J101 — Initiator Management (L)
- JES2-managed initiator pool (INIT definitions)
- Initiator class assignments (multiple classes per initiator)
- Initiator start/stop lifecycle
- WLM-managed initiator model (goal-based start/stop)
- Concurrent job execution across multiple initiators
- Integration with dataset locking for serialization
- **Depends on**: J100, open-mainframe-dataset (locking)

### J102 — Spool Management (XL)
- Spool dataset abstraction (track groups, spool volumes)
- SYSOUT dataset storage on spool
- Spool allocation/deallocation
- Spool space monitoring and reporting
- Checkpoint mechanism (dual CKPT for recovery)
- BERT (buffer element) tracking
- Warm start recovery from spool
- Cold start with spool rebuild
- **Depends on**: J100

### J103 — Output Processing (L)
- SYSOUT class routing (A-Z, 0-9)
- Output descriptors (named output specifications)
- MSGCLASS routing for job log/messages
- DEST parameter routing
- Output groups (group SYSOUT datasets)
- Output disposition (WRITE/HOLD/KEEP/PURGE)
- OUTDISP processing
- Integration with OUTPUT statement in JCL AST
- **Depends on**: J100, J102

### J104 — Printer & Writer Management (L)
- Local printer definitions (PRT)
- Punch definitions (PUN)
- Print separator pages (banner/trailer)
- Forms/FCB/UCS support
- External writer interface
- FSS (Functional Subsystem) interface for PSF
- Print copy count and burst mode
- Printer commands ($S PRT, $P PRT, $C PRT, $B PRT, $F PRT)
- **Depends on**: J103

### J105 — JECL Statement Processing (M)
- JECL lexer (/* prefix recognition distinct from JCL //* comments)
- /*JOBPARM parsing (SYSAFF, PROCLIB, TIME, LINES, PAGES, COPIES, FORMS)
- /*ROUTE PRINT and /*ROUTE XEQ parsing
- /*OUTPUT parsing
- /*PRIORITY parsing
- /*SETUP, /*MESSAGE, /*NOTIFY parsing
- Integration with JCL parser pipeline
- **Depends on**: J100, open-mainframe-jcl (parser)

### J106 — JES2 Operator Commands (L)
- Command parser for $ prefix syntax
- $D (display) commands for all object types
- $S (start) commands for initiators, printers, readers
- $P (stop/drain) commands
- $A (release from hold) commands
- $C (cancel) commands
- $T (modify) commands for dynamic configuration
- $H (hold) commands
- $E (restart), $Z (halt), $L (list) commands
- $ACTIVATE, $JDSTATUS, $JDDETAILS
- Command authorization via RACF JESSPOOL class
- **Depends on**: J100, J101, J103, J104

### J107 — NJE (Network Job Entry) (L)
- NJE node model (NJEDEF, NODE definitions)
- NJE protocol implementation (TCP/IP transport)
- Job routing (/*ROUTE XEQ, /*XMIT)
- Output routing (/*ROUTE PRINT, DEST=node.user)
- Store-and-forward relay through intermediate nodes
- NETSERV TCP/IP server address space
- NJE security via RACF NODES class
- Multi-hop routing
- **Depends on**: J100, J102, J105

### J108 — Multi-Access Spool (MAS) (L)
- MASDEF/MEMBER configuration
- XCF group integration for MAS communication
- Shared spool across MAS members
- Shared checkpoint coordination
- System affinity (SYSAFF) enforcement
- Job selection across MAS members
- Workload distribution
- **Depends on**: J100, J102

### J109 — Installation Exits Framework (M)
- Exit point infrastructure (EXIT001-EXIT255)
- Exit registration and enablement ($T EXIT)
- Standard exit calling conventions (registers, return codes)
- Key exits: separator (EXIT001), JCL scan (EXIT002-004), command (EXIT005-006)
- Converter exits (EXIT007-008)
- NJE exits (EXIT009-012)
- Scheduling exits (EXIT025-030)
- Security/accounting exits (EXIT031-040)
- Output exits (EXIT041-050)
- **Depends on**: J100, J106

### J110 — JES2 Initialization & Configuration (M)
- JES2PARM parameter file parsing (~70 statements)
- JOBCLASS/OUTCLASS definition processing
- SPOOL/SPOOLDEF/CKPTDEF configuration
- INIT/INITDEF configuration
- PRT/PUN/RDR device configuration
- NJEDEF/NODE/LINE network configuration
- Start type handling (cold/warm/quick/hot)
- Dynamic parameter modification via $T commands
- **Depends on**: J100, J101, J102, J103

### J111 — Spool Offload & Utilities (M)
- Spool offload to external media (OFFLOAD.JT/JR/ST/SR)
- Spool reload from offloaded data
- Spool drain and cleanup
- ZAPJOB utility for damaged jobs
- $HASP message generation and handling
- Internal reader (INTRDR) support — submit jobs from within jobs
- **Depends on**: J100, J102

### J112 — SDSF Integration (M)
- SDSF (System Display and Search Facility) panel model
- Job status display (Input/Active/Output queues)
- SYSOUT browsing
- Job log viewing
- Command interface via SDSF panels
- Integration with ISPF panel services (from Batch 9)
- **Depends on**: J100, J103, TSO/ISPF subsystem (Batch 9)

## Dependencies

### Existing Crate Dependencies
| Crate | Relationship |
|-------|-------------|
| open-mainframe-jcl | JCL parsing, AST (JobParams, SysoutDef, OutputStatement, TypeRun) |
| open-mainframe-dataset | Dataset locking for multi-job serialization, DD statement allocation |
| open-mainframe-runtime | Job step execution, program invocation |
| open-mainframe-sort | DFSORT/ICETOOL invoked as job steps |
| open-mainframe-cobol | COBOL programs executed as job steps |

### Cross-Batch Dependencies
| Batch | Dependency |
|-------|------------|
| Batch 1 (REXX) | REXX EXECs invoked via JCL EXEC PGM=IKJEFT01 |
| Batch 5 (CLIST) | CLISTs invoked similarly via TSO batch |
| Batch 8 (RACF) | JESSPOOL class for JES2 command authorization; JESJOBS for job-level security |
| Batch 9 (TSO/ISPF) | TSO SUBMIT command; SDSF via ISPF; internal reader |
| Batch 10 (IBM MQ) | MQ trigger monitors started as JES2 jobs |
| Batch 12 (LE) | Language Environment for COBOL/PL/I job step execution |
| Batch 14 (SMF) | SMF Type 6/26 (JES2 accounting), Type 30 (job/step info) |

### New Crate Recommendation
- **open-mainframe-jes2** — New crate for JES2 subsystem
- Consider sub-modules: `queue`, `spool`, `output`, `nje`, `commands`, `exits`, `config`

## Complexity Estimate

| Epic | Size | Rationale |
|------|------|-----------|
| J100 — Job Queue & Scheduling | XL | Core scheduler with priority queue, class management, DJC, multi-state lifecycle |
| J101 — Initiator Management | L | Pool management, class assignment, concurrent execution coordination |
| J102 — Spool Management | XL | Spool storage abstraction, track groups, checkpoint, warm start recovery |
| J103 — Output Processing | L | SYSOUT routing, output descriptors, MSGCLASS, disposition handling |
| J104 — Printer & Writer Management | L | Device abstraction, forms/FCB, FSS interface, separator pages |
| J105 — JECL Processing | M | Lexer extension + ~13 statement parsers; integrates with existing JCL parser |
| J106 — Operator Commands | L | 50+ commands with parsing, authorization, and object targeting |
| J107 — NJE | L | Network protocol, multi-node routing, store-and-forward |
| J108 — MAS | L | Shared spool, XCF coordination, affinity, workload distribution |
| J109 — Installation Exits | M | Exit framework + ~73 exit point definitions with calling conventions |
| J110 — Initialization | M | ~70 statement parser + start type handling |
| J111 — Spool Offload & Utilities | M | Offload/reload, INTRDR, ZAPJOB, $HASP messages |
| J112 — SDSF Integration | M | Panel-based job/output browsing; depends on TSO/ISPF |

**Overall complexity: XXL** — JES2 is the batch processing backbone of z/OS. The 13 proposed epics cover the full lifecycle from job submission through output processing, plus networking (NJE), multi-system (MAS), and operational tooling (SDSF, exits, commands).

## Reference Documentation

- [z/OS JES2 Introduction (SA32-0994) — z/OS 2.4](https://www.ibm.com/docs/en/SSLTBW_2.4.0/pdf/hasa800_v2r4.pdf)
- [z/OS JES2 Commands (SA32-0990) — z/OS 2.4](https://www.ibm.com/docs/en/SSLTBW_2.4.0/pdf/hasa200_v2r4.pdf)
- [z/OS JES2 Initialization and Tuning Guide (SA32-0991) — z/OS 3.1](https://www.ibm.com/docs/en/SSLTBW_3.1.0/pdf/hasa300_v3r1.pdf)
- [z/OS JES2 Initialization and Tuning Reference (SA32-0992) — z/OS 3.1](https://www.ibm.com/docs/en/SSLTBW_3.1.0/pdf/hasa400_v3r1.pdf)
- [z/OS JES2 Installation Exits (SA32-0995) — z/OS V2R3](https://www.ibm.com/docs/en/SSLTBW_2.3.0/pdf/hasc100_v2r3.pdf)
- [JES2 IBM-defined exits (z/OS 2.5)](https://www.ibm.com/docs/en/zos/2.5.0?topic=system-jes2-defined-exits)
- [JES2 Initialization Statement Descriptions (z/OS 2.4)](https://www.ibm.com/docs/en/zos/2.4.0?topic=reference-jes2-initialization-statement-descriptions)
- [JES2 Initialization Statement Summary Tables (z/OS 2.4)](https://www.ibm.com/docs/en/zos/2.4.0?topic=reference-jes2-initialization-statement-parameter-summary-tables)
- [JES2 Command Reference — Zikipedia (IBM Redbooks)](https://ibmredbooks.github.io/zikipedia/references/jes2_command_reference/)
- [JES2 Commands Tutorial — MainframeMaster](https://www.mainframemaster.com/tutorials/jcl/jes2-commands)
- [JES2 JECL Statements — TmaxSoft OpenFrame Reference](https://docs.tmaxsoft.com/en/openframe_batch/7.1_MVS/jcl-reference-guide/chapter-jes2-job-control-statements.html)
- [JES2/JES3 JCL/JECL Differences — SHARE Conference](https://share.confex.com/share/119/webprogram/Handout/Session11710/JES2-JES3%20JCL-JECL%20Differences.pdf)
- [JES — One of the Most Important Base Elements of z/OS — TechChannel](https://techchannel.com/z-os/jes-zos-base-element/)
- [$T EXIT(nnn) Command (z/OS 3.1)](https://www.ibm.com/docs/en/zos/3.1.0?topic=section-t-exitnnn-control-jes2-installation-exit-points)
