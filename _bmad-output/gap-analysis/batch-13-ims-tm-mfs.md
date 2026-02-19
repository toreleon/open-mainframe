# Gap Analysis: IMS TM (Transaction Manager) + MFS (Message Format Service)

## Official Specification Summary

IMS Transaction Manager (IMS TM, also called IMS/DC — Data Communications) is the transaction processing component of IBM's Information Management System on z/OS. While the companion IMS DB (Database Manager) handles hierarchical database access, IMS TM manages the online transaction environment: receiving messages from terminals and clients, scheduling application programs, routing output messages, managing conversational and non-conversational transactions, and providing system administration commands.

MFS (Message Format Service) is the device-dependent formatting layer within IMS TM. MFS translates between physical device formats (3270 screens, printers, SLU-P devices) and logical message formats used by application programs, allowing a single program to handle input/output without device-specific code.

IMS TM is classified as **Core** (required for online IMS transaction processing) on mainframes:
- Processes millions of transactions per day at major installations
- Three region types: **MPP** (Message Processing Program — online), **BMP** (Batch Message Processing — batch with DB/queue access), **IFP** (IMS Fast Path — high-volume DEDB transactions)
- Message queues connect terminals/clients to application programs
- I/O PCB (Program Communication Block) handles message I/O; Alternate PCBs route to other destinations
- System service calls: CHKP (checkpoint), SYNC (sync point), ROLB (rollback), LOG, STAT
- **OTMA** (Open Transaction Manager Access): high-performance XCF-based interface for clients (e.g., IMS Connect, MQ-IMS bridge)
- **IMS Connect**: TCP/IP gateway providing external access to IMS transactions
- Conversational transactions preserve Scratch Pad Area (SPA) across interactions
- Fast Path: EMH (Expedited Message Handler) + DEDB (Data Entry Database) for sub-millisecond transactions
- /DIS, /STA, /STO, /DBR and 100+ operator commands for administration
- IMS TM is tightly coupled with IMS DB — they share the same control region

MFS provides:
- **MID** (Message Input Descriptor): maps device input fields to logical message segments
- **MOD** (Message Output Descriptor): maps logical output segments to device fields
- **DIF** (Device Input Format): defines physical device input layout
- **DOF** (Device Output Format): defines physical device output layout
- **MFS Language Utility** (DFSUPAA0): compiles MFS source into control blocks stored in IMS.FORMAT library
- Supports 3270, SLU-2, SLU-P, FIN (financial terminals), ISC (inter-system communication)

Key documentation:
- **IMS Application Programming (SC18-9698)** — DL/I calls, message processing, system services
- **IMS Application Programming APIs (SC18-9699)** — CBLTDLI, AIBTDLI, CEETDLI call interfaces
- **IMS System Administration (SC18-9718)** — commands, operations, configuration
- **IMS Messages and Codes (GC18-9712)** — DFS messages, status codes
- **IMS MFS Reference (SC18-9700)** — MFS statements, utilities, control blocks
- **IMS Operations and Automation (SC18-9714)** — operator commands, automation exits
- **IMS Connect Guide (SC18-9703)** — TCP/IP connectivity, XML/JSON support
- **IMS OTMA Guide (SC18-9717)** — OTMA protocol, TPIPEs, XCF groups

## Key Features & Capabilities

### 1. Region Types (Dependent Regions)

| Region Type | Description | Scheduling |
|-------------|-------------|------------|
| MPP (Message Processing Program) | Online transaction processing; program loaded when message arrives | IMS schedules based on transaction class/priority |
| BMP (Batch Message Processing) | Batch-oriented; can access DB and/or message queues; JCL-initiated | JCL EXEC PGM=DFSRRC00 with PROC= |
| BMP (transaction-oriented) | Like MPP but JCL-started; processes messages from a specific queue | JCL with MBR= and IN= parameters |
| IFP (IMS Fast Path) | Processes Fast Path transactions; uses EMH and DEDBs | IMS schedules; sub-millisecond response |
| JMP (Java Message Processing) | Java-based MPP equivalent | IMS schedules Java programs |
| JBP (Java Batch Processing) | Java-based BMP equivalent | JCL-initiated |

### 2. Message Processing (DL/I Calls to I/O PCB)

| DL/I Call | Target PCB | Purpose |
|-----------|-----------|---------|
| GU (Get Unique) | I/O PCB | Retrieve first segment of next input message |
| GN (Get Next) | I/O PCB | Retrieve next segment of multi-segment message |
| ISRT (Insert) | I/O PCB | Send output message segment back to originating terminal |
| GU | Alt PCB | (Not typically used — Alt PCB for output) |
| ISRT | Alt PCB | Send message to alternate destination (different terminal/transaction) |
| PURG | I/O/Alt PCB | Force output message to be sent (end of logical message) |
| CHNG | Alt PCB | Change destination of modifiable alternate PCB |
| SETO | I/O/Alt PCB | Set output descriptor (options) |
| CMD | I/O PCB | Issue IMS command from application program |
| GCMD | I/O PCB | Get response to CMD call |
| AUTH | I/O PCB | Check security authorization |
| INIT | I/O PCB | Get information about user/transaction/destination |
| INQY | I/O PCB | Query environment (ENVIRON), PCB info, program info |

### 3. System Service Calls

| DL/I Call | Purpose |
|-----------|---------|
| CHKP (basic) | Issue checkpoint; commits DB changes, releases locks |
| CHKP (symbolic) | Checkpoint with restart data (for BMP restart) |
| SYNC | Commit point (like CHKP but no restart data) |
| ROLB | Rollback to last checkpoint; re-enqueue current message |
| ROLL | Rollback and send error message to terminal |
| ROLS | Rollback to an intermediate backout point (savepoint) |
| SETS | Establish an intermediate backout point |
| SETU | Like SETS, but for unsupported options returns status rather than abend |
| LOG | Write a record to the IMS system log |
| STAT | Retrieve runtime statistics |
| PCB (SCHD) | Schedule a PSB (BMP only; MPP auto-scheduled) |
| TERM | Terminate/release PSB |
| XRST | Extended restart — recover from checkpoint (BMP) |
| ICAL | Invoke a remote service via OTMA or IMS Connect |

### 4. Conversational Transactions

| Feature | Description |
|---------|-------------|
| SPA (Scratch Pad Area) | Preserved across conversational exchanges; first segment of input message |
| SPA size | Defined in transaction definition (1–32,767 bytes) |
| Multi-step conversation | Program receives SPA on GU, processes, inserts SPA on ISRT, IMS re-schedules on next input |
| Conversation termination | Insert blank transaction code in SPA to end conversation |

### 5. Fast Path

| Feature | Description |
|---------|-------------|
| EMH (Expedited Message Handler) | Bypasses normal message queuing for ultra-fast response |
| DEDB (Data Entry Database) | Specialized DB type optimized for high-volume updates |
| IFP region | Dedicated dependent region type for Fast Path |
| FP-exclusive ISRT | ISRT to I/O PCB with FP routing |
| MSC (Multiple Systems Coupling) | Route Fast Path transactions across IMS systems |

### 6. OTMA (Open Transaction Manager Access)

| Feature | Description |
|---------|-------------|
| XCF groups | IMS joins an XCF group; clients connect via TPIPE |
| TPIPE (Transaction Pipe) | Named pipe for client-IMS communication |
| OTMA message prefix | Headers: control data, state data, security data, user data |
| Commit mode | Commit-then-send (CM0) or Send-then-commit (CM1) |
| Sync level | NONE, CONFIRM, SYNCPT |
| Resume TPIPE | Client retrieves asynchronous output |
| Super member | OTMA client with routing authority |
| OTMA exits | DFSYPRX0 (routing), DFSYDRU0 (destination resolution) |

### 7. IMS Connect

| Feature | Description |
|---------|-------------|
| TCP/IP gateway | External access to IMS transactions via TCP/IP |
| HWSSMPL0/HWSSMPL1 | Sample message exit routines |
| IRM (IMS Request Message) | Input message header with transaction code, client ID |
| RSM (IMS Response Message) | Output message header with status |
| XML/JSON adapter | Transform IMS messages to/from XML or JSON |
| SSL/TLS | Secure connections (AT-TLS or IMS Connect native) |
| ODBM (Open Database Manager) | Direct database access without transaction scheduling |
| MSC routing | Route via IMS Connect to remote IMS systems |
| Port definitions | Multiple ports, each with own configuration |
| Connection bundling | Persistent connections for performance |

### 8. IMS Commands (Operator Commands)

| Command | Purpose |
|---------|---------|
| /DISPLAY TRAN | Show transaction status, queue depth |
| /DISPLAY PGM | Show program status |
| /DISPLAY ACTIVE | Show active regions |
| /START TRAN | Enable transaction scheduling |
| /STOP TRAN | Disable transaction scheduling |
| /START PGM | Enable program scheduling |
| /STOP PGM | Disable program scheduling |
| /START REGION | Start a dependent region |
| /STOP REGION | Stop a dependent region |
| /ASSIGN | Change transaction/program attributes |
| /DBR (Database Recovery) | Recover database |
| /DBD (Database Dealloc) | Deallocate database |
| /CHANGE | Modify IMS resource attributes |
| /LOCK / /UNLOCK | Lock/unlock IMS resources |
| /LOG | Write message to IMS log |
| /BROADCAST | Send message to terminals/users |
| /TRACE | Enable/disable tracing |
| /CHECKPOINT | Force system checkpoint |
| /ERE (Emergency Restart) | Emergency restart after failure |
| /NRE (Normal Restart) | Normal restart |

Type-2 commands (OM-managed, XML-based):
- QUERY TRAN, UPDATE TRAN, CREATE TRAN, DELETE TRAN
- QUERY PGM, UPDATE PGM, CREATE PGM, DELETE PGM
- QUERY OTMADESC, UPDATE OTMADESC

### 9. MFS (Message Format Service)

#### 9.1 MFS Control Blocks

| Control Block | Purpose |
|---------------|---------|
| MID (Message Input Descriptor) | Maps device input fields → logical message segments |
| MOD (Message Output Descriptor) | Maps logical output segments → device output fields |
| DIF (Device Input Format) | Physical device input layout (3270 field definitions) |
| DOF (Device Output Format) | Physical device output layout (3270 field positions, attributes) |
| MSG (Message Descriptor) | Contains LPAGE/SEG/MFLD definitions |
| FMT (Format Descriptor) | Contains DEV/DIV/DPAGE/DFLD definitions |

#### 9.2 MFS Statement Types

| Statement | Purpose |
|-----------|---------|
| MSG | Define a message descriptor (TYPE=INPUT or TYPE=OUTPUT) |
| LPAGE | Define a logical page within MSG |
| SEG | Define a segment within LPAGE |
| MFLD | Define a message field within SEG |
| FMT | Define a format descriptor |
| DEV | Define device type within FMT (TYPE=3270-A2, 3270-An, SLU2, FIN, etc.) |
| DIV | Define division (TYPE=INOUT, INPUT, OUTPUT) |
| DPAGE | Define device page |
| DFLD | Define device field (position, length, attributes) |
| TABLE | Define translation table |
| ALPHA / EGCS | Define character translation |
| FMTEND / MSGEND | End delimiters |
| PASSWORD | Password control |
| COPY | Copy previously defined format |

#### 9.3 MFS Field Attributes (DFLD)

| Attribute | Description |
|-----------|-------------|
| POS=(line,col) | Screen position |
| LTH=n | Field length |
| ATTR=(PROT,NUM,BRT,MOD,...) | 3270 field attributes |
| EATTR=(HILIGHT,COLOR,PS,...) | Extended attributes (color, highlighting) |
| JUST=(L/R,ZERO/BLANK) | Justification and fill |
| NODISP | Non-display (password fields) |
| PEN | Light-pen detectable |
| SCA | System Control Area fields |

### 10. Additional IMS TM Features

| Feature | Description |
|---------|-------------|
| MSC (Multiple Systems Coupling) | Route transactions between IMS systems via physical/logical links |
| Shared Queues | IMS systems share message queues via coupling facility |
| APPC/IMS (LU 6.2) | Peer-to-peer program-to-program communication |
| CPI-C | Common Programming Interface for Communications |
| IMS Callable Interface (ICI) | Non-message-driven DL/I access from batch, CICS, etc. |
| DBCTL (Database Control) | IMS DB without TM; CICS accesses IMS DB via DBCTL |
| Automated Operator Interface (AOI) | Programs issue IMS commands via CMD/GCMD |
| Transaction class/priority | Scheduling controls: class (1-999), normal/limit priority, limit count |
| Program scheduling | Load balancing: parallel scheduling, serial vs parallel class |
| Extended Terminal Option (ETO) | Dynamic logon; no pre-defined terminal definitions needed |

## Current OpenMainframe Status

The `open-mainframe-ims` crate (in `crates/open-mainframe-ims/`) currently provides **full IMS DB support** and **partial IMS TM foundations**:

### Present — IMS DB (Full)
- DL/I call interface: GU, GN, GNP, GHU, GHN, GHNP, ISRT, DLET, REPL (`runtime/mod.rs:355-369`)
- DBD parsing (`dbd/` module)
- PSB parsing with SENSEG/SENFLD (`psb/mod.rs:332-420`)
- Hierarchical store with in-memory engine + PostgreSQL persistence (`dli/store/`, `persist/`)
- SSA (Segment Search Argument) with qualification operators (`dli/mod.rs`)
- Secondary index / alternate processing sequence via PROCSEQ (`psb/mod.rs:114,374`)
- 33 named status codes covering DB, system service, GSAM, and message queue classes (`lib.rs:99-196`)

### Present — IMS TM Foundations (Partial)
- **ImsMessage struct**: lterm, tran_code, segments (multi-segment), user_id (`runtime/mod.rs:17-26`)
- **I/O PCB**: `PcbType::Io`, `new_io()` with lterm_name, user_id, status_date, status_time, input_msg_seq (`psb/mod.rs:146-164,218-229`)
- **Alternate PCB type**: `PcbType::Alt` defined (`psb/mod.rs:226`)
- **Message queue operations** (via `execute_io_pcb_call`, `runtime/mod.rs:422-520`):
  - GU to I/O PCB: dequeue input message, update LTERM/user fields
  - GN to I/O PCB: get next segment of multi-segment message
  - ISRT to I/O PCB: queue output response to originating terminal
  - QC status (no messages) and QD status (no more segments) returned correctly
- **Input/output queues**: `input_queue: VecDeque<ImsMessage>`, `output_queue: Vec<ImsMessage>` (`runtime/mod.rs:150-152`)
- **System service calls** (`runtime/mod.rs:588-598`):
  - CHKP: basic checkpoint with ID, snapshot support (`runtime/mod.rs:591`)
  - SYNC: commit point (`runtime/mod.rs:592`)
  - ROLB: rollback with re-enqueue of current input message (`runtime/mod.rs:593`)
  - LOG: write record to in-memory log (`runtime/mod.rs:594`)
  - STAT: return runtime statistics (`runtime/mod.rs:595`)
- **GSAM support**: GsamDataset struct, GN (read sequential), ISRT (write sequential) (`runtime/mod.rs:522-582`)
- **EXEC DLI scanner**: ExecDliBlock struct, DliScanner, DliQualification — parses EXEC DLI blocks from COBOL source (`preprocess/mod.rs:14-48`)
- **StatusCategory::MessageQueue** for QC/QD classification (`lib.rs:92`)

### Missing — IMS TM
- Region types (MPP/BMP/IFP/JMP/JBP) — no region model or scheduling
- Alt PCB operations (ISRT to Alt PCB, CHNG, PURG)
- Conversational transactions (SPA handling)
- OTMA protocol
- IMS Connect (TCP/IP gateway)
- Fast Path (EMH, IFP scheduling)
- IMS operator commands (/DIS, /STA, /STO, etc.)
- MSC (Multiple Systems Coupling)
- Shared queues (coupling facility)
- APPC/IMS / CPI-C / LU 6.2
- ETO (Extended Terminal Option)
- Transaction class/priority scheduling
- CMD/GCMD (automated operator interface)
- INIT/INQY (environment queries)
- AUTH (security checks)
- XRST/SETS/SETU/ROLS (advanced recovery)
- EXEC DLI code generation (scanner exists but no CBLTDLI CALL generation)

### Missing — MFS (Entirely)
- MFS control blocks (MID/MOD/DIF/DOF)
- MFS source language (MSG/LPAGE/SEG/MFLD/FMT/DEV/DIV/DPAGE/DFLD)
- MFS Language Utility (compiler)
- IMS.FORMAT library
- Field attribute mapping (3270 ↔ logical message)
- Device independence layer

## Gap Details

### IMS TM — Message Processing

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| GU to I/O PCB (read input) | Dequeue message from queue | `execute_io_pcb_call` GU branch | **Present** |
| GN to I/O PCB (next segment) | Multi-segment message retrieval | `execute_io_pcb_call` GN branch | **Present** |
| ISRT to I/O PCB (send output) | Queue response to originator | `execute_io_pcb_call` ISRT branch | **Present** |
| ISRT to Alt PCB | Send message to alternate destination | PcbType::Alt exists but no Alt PCB I/O logic | **Partial** |
| PURG call | Force end-of-message to device | Not implemented | Missing |
| CHNG call | Change Alt PCB destination | Not implemented | Missing |
| SETO call | Set output options | Not implemented | Missing |
| CMD / GCMD calls | Issue/retrieve IMS commands from program | Not implemented | Missing |
| AUTH call | Check user authorization | Not implemented | Missing |
| INIT call | Get user/transaction/destination info | Not implemented | Missing |
| INQY call | Query environment, PCB info | Not implemented | Missing |
| Multi-segment output | Build multi-segment output messages | Single-segment ISRT only | **Partial** |
| I/O PCB fields | LTERM, user_id, date, time, seq# | All present in ProgramCommBlock | **Present** |

### IMS TM — System Services

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| CHKP (basic) | Checkpoint, commit, release locks | Implemented with ID and snapshots | **Present** |
| CHKP (symbolic) | Checkpoint with restart data areas | No restart data areas | **Partial** |
| SYNC | Commit point | Implemented | **Present** |
| ROLB | Rollback to last CHKP, re-enqueue message | Implemented with re-enqueue | **Present** |
| ROLL | Rollback + send error to terminal | Not implemented | Missing |
| ROLS | Rollback to intermediate backout point | Not implemented | Missing |
| SETS / SETU | Establish intermediate backout point | Not implemented | Missing |
| LOG | Write to system log | Implemented (in-memory log) | **Present** |
| STAT | Retrieve runtime statistics | Implemented (call counts) | **Present** |
| XRST | Extended restart for BMP recovery | Not implemented | Missing |
| PSB scheduling (SCHD/TERM) | Schedule/release PSB | schedule_psb() exists | **Partial** |

### IMS TM — Region Types & Scheduling

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| MPP region model | Message-driven, auto-scheduled | No region concept | Missing |
| BMP region model | JCL-started, DB+queue access | No region concept | Missing |
| IFP region (Fast Path) | EMH-driven, DEDB-optimized | No region concept | Missing |
| JMP (Java MPP) | Java message processing | No Java support | Missing |
| JBP (Java BMP) | Java batch processing | No Java support | Missing |
| Transaction class/priority | Classes 1-999, priority scheduling | Not implemented | Missing |
| Parallel scheduling | Multiple instances of same program | Not implemented | Missing |
| Limit count/priority | Dynamic priority adjustment under load | Not implemented | Missing |
| Wait-for-input (WFI) | MPP stays loaded between messages | Not implemented | Missing |

### IMS TM — Conversational Transactions

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| SPA (Scratch Pad Area) | Preserved across conversational steps | Not implemented | Missing |
| SPA as first segment | GU returns SPA then data segments | Not implemented | Missing |
| Conversation termination | Blank transaction code in SPA | Not implemented | Missing |
| SPA size definition | 1-32,767 bytes per transaction | Not implemented | Missing |

### IMS TM — OTMA

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| XCF group membership | IMS joins XCF group | Not implemented | Missing |
| TPIPE protocol | Named pipes for client connections | Not implemented | Missing |
| OTMA message prefix | Control/state/security/user data headers | Not implemented | Missing |
| Commit modes (CM0/CM1) | Commit-then-send / Send-then-commit | Not implemented | Missing |
| Sync levels | NONE, CONFIRM, SYNCPT | Not implemented | Missing |
| Resume TPIPE | Async output retrieval | Not implemented | Missing |
| OTMA exits (DFSYPRX0/DFSYDRU0) | Routing and destination resolution | Not implemented | Missing |

### IMS TM — IMS Connect

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| TCP/IP listener | Accept connections on configured ports | Not implemented | Missing |
| IRM/RSM protocol | Request/response message headers | Not implemented | Missing |
| HWSSMPL0/1 exits | Message formatting exits | Not implemented | Missing |
| XML/JSON adapter | Message transformation | Not implemented | Missing |
| SSL/TLS | Secure connections | Not implemented | Missing |
| ODBM | Direct database access | Not implemented | Missing |
| Connection management | Persistent, pooled, one-shot | Not implemented | Missing |

### IMS TM — Operator Commands

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| /DISPLAY (TRAN/PGM/ACTIVE/...) | Query status of resources | Not implemented | Missing |
| /START / /STOP | Enable/disable resources | Not implemented | Missing |
| /ASSIGN | Change resource attributes | Not implemented | Missing |
| /DBR / /DBD | Database recovery/deallocation | Not implemented | Missing |
| /CHANGE | Modify resource definitions | Not implemented | Missing |
| /BROADCAST | Send messages to terminals | Not implemented | Missing |
| /CHECKPOINT | Force system checkpoint | Not implemented | Missing |
| /TRACE | Enable tracing | Not implemented | Missing |
| Type-2 commands (XML) | QUERY/UPDATE/CREATE/DELETE via OM | Not implemented | Missing |

### IMS TM — Advanced Features

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| MSC (Multiple Systems Coupling) | Inter-IMS transaction routing | Not implemented | Missing |
| Shared queues | Coupling facility message sharing | Not implemented | Missing |
| APPC/IMS (LU 6.2) | Program-to-program communication | Not implemented | Missing |
| CPI-C | Common programming interface | Not implemented | Missing |
| ETO (Extended Terminal Option) | Dynamic terminal definitions | Not implemented | Missing |
| ICI (IMS Callable Interface) | Non-message DL/I from external programs | Not implemented | Missing |
| DBCTL | DB-only mode for CICS | Not implemented | Missing |
| Fast Path EMH | Expedited message handler | Not implemented | Missing |
| Fast Path DEDB runtime | DEDB transaction processing in IFP | Not implemented | Missing |

### MFS (Message Format Service)

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| MID (Message Input Descriptor) | Device input → logical message mapping | Not implemented | Missing |
| MOD (Message Output Descriptor) | Logical output → device format mapping | Not implemented | Missing |
| DIF (Device Input Format) | Physical device input layout | Not implemented | Missing |
| DOF (Device Output Format) | Physical device output layout | Not implemented | Missing |
| MSG statement | Message descriptor definition | Not implemented | Missing |
| FMT statement | Format descriptor definition | Not implemented | Missing |
| LPAGE / SEG / MFLD | Logical page, segment, message field | Not implemented | Missing |
| DEV / DIV / DPAGE / DFLD | Device, division, device page, device field | Not implemented | Missing |
| DFLD attributes (POS, LTH, ATTR, EATTR) | 3270 field positioning and attributes | Not implemented | Missing |
| MFS Language Utility | Compile MFS source to control blocks | Not implemented | Missing |
| IMS.FORMAT library | Store compiled MFS blocks | Not implemented | Missing |
| /FORMAT command | Invoke MFS format from program | Not implemented | Missing |
| MFS COPY | Copy/reuse format definitions | Not implemented | Missing |
| Device independence | Single program → multiple device types | Not implemented | Missing |

### EXEC DLI Preprocessing

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| EXEC DLI block scanning | Locate EXEC DLI...END-EXEC in COBOL | DliScanner in preprocess/mod.rs | **Present** |
| Parse GU/GN/ISRT/SCHD/CHKP | Extract function, PCB, segment, I/O area | ExecDliBlock struct with all fields | **Present** |
| Generate CBLTDLI CALL | Replace EXEC DLI with CALL 'CBLTDLI' | Not implemented (scanner only) | **Partial** |
| Generate DIB copybook | DL/I Interface Block working storage | Not implemented | Missing |
| Generate SSA working storage | SSA layout in WORKING-STORAGE | Not implemented | Missing |
| AIBTDLI interface | Application Interface Block calling convention | Not implemented | Missing |
| CEETDLI interface | LE-compatible DL/I call interface | Not implemented | Missing |

## Proposed Epic Structure

### IMS-TM100: Alt PCB & Extended Message Operations (M)
- ISRT to Alt PCB (route to alternate destination)
- CHNG call (change Alt PCB destination)
- PURG call (force message output)
- SETO call (set output options)
- Multi-segment output message assembly
- **Depends on**: existing I/O PCB infrastructure

### IMS-TM101: Conversational Transaction Support (M)
- SPA (Scratch Pad Area) data structure
- SPA as first segment on GU
- Conversational transaction lifecycle (insert SPA to continue, blank to end)
- SPA size configuration per transaction definition
- **Depends on**: IMS-TM100

### IMS-TM102: Advanced System Service Calls (M)
- ROLL (rollback + error message)
- ROLS (rollback to intermediate backout point)
- SETS / SETU (establish intermediate backout points)
- XRST (extended restart for BMP recovery)
- Symbolic CHKP with restart data areas
- **Depends on**: existing CHKP/SYNC/ROLB

### IMS-TM103: Environment Query Calls (S)
- INIT call (get user/transaction/destination info)
- INQY call (query environment, PCB, program name)
- AUTH call (check security authorization)
- **Depends on**: existing I/O PCB, RACF integration (batch-08)

### IMS-TM104: Region Model & Transaction Scheduling (L)
- Region type definitions (MPP, BMP, IFP)
- Transaction definition (class, priority, SPA size, scheduling type)
- Program definition (scheduling type, language, PSB name)
- Transaction-to-program routing
- Message queue depth monitoring
- Parallel scheduling support (multiple instances)
- Wait-for-input (WFI) mode
- **Depends on**: existing runtime, JES2 (batch-11) for BMP JCL

### IMS-TM105: Operator Command Framework (L)
- Command parser for /DISPLAY, /START, /STOP, /ASSIGN, /CHANGE
- Resource registry (transactions, programs, regions, databases)
- /DISPLAY TRAN/PGM/ACTIVE implementation
- /START and /STOP for TRAN/PGM/REGION
- /CHECKPOINT, /LOG, /BROADCAST
- CMD/GCMD DL/I calls (Automated Operator Interface)
- **Depends on**: IMS-TM104

### IMS-TM106: OTMA Protocol (XL)
- XCF group abstraction (simulated via channels/sockets)
- TPIPE implementation
- OTMA message prefix (control, state, security, user data sections)
- Commit modes (CM0, CM1)
- Sync levels (NONE, CONFIRM, SYNCPT)
- Resume TPIPE for async output
- OTMA routing/destination resolution exits
- **Depends on**: IMS-TM104

### IMS-TM107: IMS Connect Gateway (XL)
- TCP/IP listener with port configuration
- IRM/RSM message protocol
- Connection management (persistent, pooled, one-shot)
- Message exit framework (HWSSMPL0-style)
- XML/JSON adapter for message transformation
- SSL/TLS support
- ODBM (Open Database Manager) direct DB access
- **Depends on**: IMS-TM106, existing IMS DB

### IMS-TM108: Fast Path (EMH + IFP) (L)
- EMH (Expedited Message Handler) — bypass normal queuing
- IFP region scheduling
- Fast Path routing
- DEDB runtime integration (if DEDB implemented in IMS DB)
- **Depends on**: IMS-TM104, existing IMS DB

### IMS-TM109: MSC & Shared Queues (L)
- MSC link definitions (physical + logical)
- Transaction routing across IMS systems
- Shared message queues via coupling facility abstraction
- **Depends on**: IMS-TM104, IMS-TM106

### MFS100: MFS Source Language Parser (M)
- Parse MSG/LPAGE/SEG/MFLD statements
- Parse FMT/DEV/DIV/DPAGE/DFLD statements
- DFLD attributes (POS, LTH, ATTR, EATTR, JUST, etc.)
- COPY statement resolution
- TABLE/ALPHA/EGCS character translation
- MFS AST representation
- **Depends on**: none (standalone parser)

### MFS101: MFS Control Block Compiler (M)
- Compile parsed MFS into MID/MOD/DIF/DOF control blocks
- IMS.FORMAT library (in-memory or file-based control block store)
- /FORMAT command support
- MFS validation (field overlap detection, attribute validation)
- **Depends on**: MFS100

### MFS102: MFS Runtime Integration (L)
- Device input → MID → logical message mapping
- Logical output → MOD → device output mapping
- 3270 data stream ↔ MFS field mapping
- Integration with I/O PCB message flow
- Device independence layer (one program, multiple device types)
- **Depends on**: MFS101, open-mainframe-tui (3270 support)

### IMS-TM110: EXEC DLI Code Generation (M)
- Generate CALL 'CBLTDLI' from parsed EXEC DLI blocks
- Generate DIB (DL/I Interface Block) copybook
- Generate SSA working storage layouts
- AIBTDLI calling convention
- CEETDLI (LE-compatible) calling convention
- **Depends on**: existing EXEC DLI scanner (preprocess/mod.rs)

## Dependencies

| Epic | Depends On (Internal) | Depends On (External Crate) |
|------|----------------------|----------------------------|
| IMS-TM100 | Existing I/O PCB, ImsMessage | — |
| IMS-TM101 | IMS-TM100 | — |
| IMS-TM102 | Existing CHKP/SYNC/ROLB | — |
| IMS-TM103 | Existing I/O PCB | open-mainframe-racf (batch-08) |
| IMS-TM104 | Existing runtime | open-mainframe-jcl (BMP) |
| IMS-TM105 | IMS-TM104 | — |
| IMS-TM106 | IMS-TM104 | — |
| IMS-TM107 | IMS-TM106 | open-mainframe-tui (3270) |
| IMS-TM108 | IMS-TM104 | IMS DB (DEDB) |
| IMS-TM109 | IMS-TM104, IMS-TM106 | — |
| MFS100 | — | — |
| MFS101 | MFS100 | — |
| MFS102 | MFS101 | open-mainframe-tui (3270) |
| IMS-TM110 | EXEC DLI scanner | open-mainframe-cobol (CALL gen) |

## Complexity Estimate

| Epic | Size | Rationale |
|------|------|-----------|
| IMS-TM100 | M | Extends existing I/O PCB infrastructure; Alt PCB logic + PURG/CHNG/SETO |
| IMS-TM101 | M | SPA data structure + conversational lifecycle; well-defined protocol |
| IMS-TM102 | M | ROLL/ROLS/SETS/SETU extend existing CHKP/ROLB; XRST more complex |
| IMS-TM103 | S | Three query-type calls with straightforward semantics |
| IMS-TM104 | L | Core scheduler: region model, transaction definitions, queue routing, parallel scheduling |
| IMS-TM105 | L | 20+ commands, resource registry, command parser; significant surface area |
| IMS-TM106 | XL | Full protocol implementation: XCF simulation, TPIPE, message prefix, commit/sync modes |
| IMS-TM107 | XL | TCP/IP gateway: listener, connection management, message transforms, SSL, ODBM |
| IMS-TM108 | L | EMH bypass, IFP scheduling, DEDB integration; requires IMS DB DEDB support |
| IMS-TM109 | L | Inter-system routing + coupling facility abstraction; distributed systems complexity |
| MFS100 | M | MFS source language has ~15 statement types; moderately complex parser |
| MFS101 | M | Compile AST to control blocks; format validation; library storage |
| MFS102 | L | Runtime field mapping between 3270 data streams and logical messages; device independence |
| IMS-TM110 | M | Code generation from existing AST; three calling conventions (CBLTDLI/AIBTDLI/CEETDLI) |

**Overall**: XL — 14 epics totaling approximately 4S + 6M + 6L + 2XL. IMS TM is a major subsystem rivaling CICS in scope. The existing IMS DB and partial I/O PCB infrastructure provide a solid foundation, but the bulk of transaction management (scheduling, commands, OTMA, IMS Connect, MFS) is entirely missing.

## Reference Documentation

- IBM IMS 15 Application Programming — https://www.ibm.com/docs/en/ims/15.4?topic=programming-application
- IBM IMS 15 Application Programming APIs — https://www.ibm.com/docs/en/ims/15.4?topic=apis
- IBM IMS 15 System Administration — https://www.ibm.com/docs/en/ims/15.4?topic=administration-system
- IBM IMS 15 MFS Reference — https://www.ibm.com/docs/en/ims/15.4?topic=service-message-format
- IBM IMS 15 Operations and Automation — https://www.ibm.com/docs/en/ims/15.4?topic=automation-operations
- IBM IMS 15 OTMA Guide — https://www.ibm.com/docs/en/ims/15.4?topic=access-open-transaction-manager
- IBM IMS 15 Connect Guide — https://www.ibm.com/docs/en/ims/15.4?topic=connect-ims
- IBM IMS 15 Messages and Codes — https://www.ibm.com/docs/en/ims/15.4?topic=codes-messages
- IBM IMS 15 Database Administration — https://www.ibm.com/docs/en/ims/15.4?topic=administration-database
- Wikipedia: IMS — https://en.wikipedia.org/wiki/IBM_Information_Management_System
