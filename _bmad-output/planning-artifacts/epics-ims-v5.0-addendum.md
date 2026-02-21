---
version: 'v5.0'
planningGroup: 'PG-24'
technology: 'IMS TM & MFS'
date: '2026-02-21'
status: 'complete'
action: 'EXTEND'
parentDocument: 'epics-ims-v3.0.md'
inputDocuments:
  - 'epics-ims-v3.0.md'
  - 'batch-13-ims-tm-mfs.md'
totalNewEpics: 14
totalNewStories: 72
---

# IMS TM & MFS Addendum (v5.0)

## Overview

This addendum extends the v3.0 IMS planning (Epics 400-410, focused on IMS DB) with 14 new epics addressing IMS Transaction Manager and Message Format Service gaps. The existing v3.0 epics provide the DB foundation; these epics add the online transaction processing layer.

---

## New Epics

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| IMS-TM100 | Alt PCB & Extended Message Operations | M | 5 | E |
| IMS-TM101 | Conversational Transaction Support | M | 4 | E |
| IMS-TM102 | Advanced System Service Calls | M | 5 | E |
| IMS-TM103 | Environment Query Calls | S | 4 | E |
| IMS-TM104 | Region Model & Transaction Scheduling | L | 6 | E |
| IMS-TM105 | Operator Command Framework | L | 6 | E |
| IMS-TM106 | OTMA Protocol (XCF-based) | L | 5 | E |
| IMS-TM107 | IMS Connect Gateway (TCP/IP) | L | 6 | E |
| IMS-TM108 | Fast Path (EMH + IFP) | M | 4 | E |
| IMS-TM109 | MSC & Shared Queues | M | 4 | E |
| MFS-100 | MFS Source Language Parser | M | 5 | E |
| MFS-101 | MFS Control Block Compiler | M | 5 | E |
| MFS-102 | MFS Runtime Integration | L | 5 | E |
| IMS-TM110 | EXEC DLI Code Generation | M | 4 | E |

---

## IMS-TM100: Alt PCB & Extended Message Operations

**User Value:** IMS programs can send messages to alternate destinations (different terminals, transactions, or printers) beyond the originating terminal.

### IMS-TM100.1: ISRT to Alternate PCB

**As an** IMS developer, **I want** ISRT to an Alt PCB to send output to a destination other than the originating terminal.

**Acceptance Criteria:**
- Given an Alt PCB with destination PRINTER01, when ISRT is issued, then the message is queued for PRINTER01
- Given a multi-segment message, when multiple ISRTs are issued to the same Alt PCB, then all segments are assembled into one message

### IMS-TM100.2: CHNG — Change Alternate PCB Destination

**As an** IMS developer, **I want** CHNG to dynamically set or change the destination of a modifiable Alt PCB.

**Acceptance Criteria:**
- Given a modifiable Alt PCB, when CHNG sets destination to TERM002, then subsequent ISRTs go to TERM002
- Given CHNG with an invalid destination, when issued, then status A1 (invalid destination) is returned

### IMS-TM100.3: PURG — Force Message Output

**As an** IMS developer, **I want** PURG to force the current output message to be sent.

**Acceptance Criteria:**
- Given segments queued via ISRT to I/O PCB, when PURG is issued, then the assembled message is sent immediately
- Given PURG followed by more ISRTs, when processed, then a new output message begins

### IMS-TM100.4: SETO — Set Output Descriptor

**As an** IMS developer, **I want** SETO to set output message options (e.g., MOD name for MFS formatting).

**Acceptance Criteria:**
- Given SETO with MOD name EMPRPT01, when the output message is sent, then MFS uses EMPRPT01 for formatting

### IMS-TM100.5: Alt PCB Tests

**Acceptance Criteria:**
- Given Alt PCB operations (ISRT, CHNG, PURG, SETO) with various destinations, when tested, then all produce correct message routing

---

## IMS-TM101: Conversational Transaction Support

**User Value:** Multi-step dialogues between terminals and IMS programs maintain state across interactions via the Scratch Pad Area (SPA).

### IMS-TM101.1: SPA (Scratch Pad Area) Handling

**As an** IMS developer, **I want** the SPA preserved across conversational exchanges, **so that** state persists between terminal interactions.

**Acceptance Criteria:**
- Given a conversational transaction with SPA size 256 bytes, when the first GU is issued, then the SPA is the first segment with initial content
- Given the program modifies the SPA and issues ISRT, when the user responds, then the next GU returns the modified SPA

### IMS-TM101.2: Conversational Transaction Lifecycle

**As an** IMS developer, **I want** conversational transaction setup and termination.

**Acceptance Criteria:**
- Given a transaction defined as conversational (SPA=256), when a user enters the transaction code, then IMS creates a SPA and schedules the program
- Given the program inserts a blank transaction code in the SPA on ISRT, when processed, then the conversation is terminated

### IMS-TM101.3: Multi-Segment Conversational Messages

**As an** IMS developer, **I want** multi-segment messages in conversational mode with SPA as the first segment.

**Acceptance Criteria:**
- Given GU returning SPA followed by GN returning application data, when processed, then both segments are available
- Given ISRT of SPA followed by ISRT of response, when processed, then both are queued as a single message

### IMS-TM101.4: Conversational Tests

**Acceptance Criteria:**
- Given a multi-step conversational scenario, when tested end-to-end, then SPA state is maintained across exchanges

---

## IMS-TM102: Advanced System Service Calls

**User Value:** Programs have full transaction control with savepoints, extended restart, and rollback variants.

### IMS-TM102.1: ROLL — Rollback with Error Message

**As an** IMS developer, **I want** ROLL to undo changes and send a DFS error message to the terminal.

**Acceptance Criteria:**
- Given ROLL is issued, when executed, then changes are rolled back and DFS554I message is sent to the originating terminal

### IMS-TM102.2: ROLS — Rollback to Savepoint

**As an** IMS developer, **I want** ROLS to undo changes back to a SETS point.

**Acceptance Criteria:**
- Given SETS establishes backout point P1, followed by DB operations, when ROLS TO P1 is issued, then only operations after P1 are undone

### IMS-TM102.3: SETS/SETU — Establish Backout Points

**As an** IMS developer, **I want** SETS to establish intermediate backout points within a transaction.

**Acceptance Criteria:**
- Given SETS TOKEN('POINT1'), when issued, then a backout point is established that ROLS can target
- Given SETU with an unsupported option, when issued, then a status code is returned instead of an abend

### IMS-TM102.4: XRST — Extended Restart

**As an** IMS developer, **I want** XRST for BMP restart from a symbolic checkpoint.

**Acceptance Criteria:**
- Given a BMP that issued CHKP with ID=CKP001, when restarted with XRST CKPTID(CKP001), then processing resumes from that checkpoint

### IMS-TM102.5: Advanced Service Tests

**Acceptance Criteria:**
- Given ROLL, ROLS/SETS, and XRST scenarios, when tested, then correct rollback/restart behavior occurs

---

## IMS-TM103: Environment Query Calls

**User Value:** Programs can query the IMS runtime environment for user, transaction, and destination information.

### IMS-TM103.1: INIT Call

**As an** IMS developer, **I want** INIT to obtain information about the current user and transaction.

**Acceptance Criteria:**
- Given INIT DBQUERY, when issued, then the program receives database availability information
- Given INIT STATUS GROUPB, when issued, then status of group B databases is returned

### IMS-TM103.2: INQY Call

**As an** IMS developer, **I want** INQY to query the IMS environment.

**Acceptance Criteria:**
- Given INQY ENVIRON, when issued, then IMS version, region type, and configuration are returned
- Given INQY PROGRAM, when issued, then current program name and scheduling info are returned

### IMS-TM103.3: AUTH Call

**As an** IMS developer, **I want** AUTH to check security authorization.

**Acceptance Criteria:**
- Given AUTH for transaction EMPTRAN, when the user has authority, then a blank status is returned
- Given AUTH for transaction ADMTRAN, when the user lacks authority, then status indicates unauthorized

### IMS-TM103.4: Query Call Tests

**Acceptance Criteria:**
- Given INIT, INQY, and AUTH calls, when tested with various parameters, then correct information is returned

---

## IMS-TM104: Region Model & Transaction Scheduling

**User Value:** IMS schedules application programs in dependent regions based on transaction type, class, and priority.

### IMS-TM104.1: MPP Region Type

**As an** IMS system programmer, **I want** MPP (Message Processing Program) regions that are auto-scheduled when messages arrive.

**Acceptance Criteria:**
- Given an MPP region started, when a message for transaction EMPTRAN arrives, then the associated program is scheduled in the MPP region
- Given multiple MPP regions, when messages queue, then IMS distributes work across available regions

### IMS-TM104.2: BMP Region Type

**As an** IMS system programmer, **I want** BMP regions for batch processing with DB and queue access.

**Acceptance Criteria:**
- Given JCL `EXEC PGM=DFSRRC00,PARM=(BMP,MYPSB,,,,,,,,,,MYPGM)`, when submitted, then the BMP region starts and runs MYPGM
- Given a transaction-oriented BMP with IN=TRANCOD, when messages queue for TRANCOD, then the BMP processes them

### IMS-TM104.3: Transaction Definitions

**As an** IMS system programmer, **I want** transaction definitions specifying program, class, priority, and scheduling parameters.

**Acceptance Criteria:**
- Given transaction EMPTRAN mapped to program EMPPGM with class=1 priority=7, when defined, then IMS uses these attributes for scheduling
- Given class limits (e.g., max 3 concurrent class-1 transactions), when exceeded, then excess transactions queue

### IMS-TM104.4: Transaction Scheduling Engine

**As an** IMS system, **I want** a scheduling engine that assigns queued messages to available regions by class and priority.

**Acceptance Criteria:**
- Given 5 queued transactions across 3 classes with 2 available regions, when scheduling runs, then highest-priority messages are dispatched first
- Given parallel scheduling enabled, when multiple messages for different programs queue, then they run concurrently in separate regions

### IMS-TM104.5: Program Scheduling Parameters

**As an** IMS system programmer, **I want** program definitions with scheduling attributes (serial/parallel, RESIDENT).

**Acceptance Criteria:**
- Given program EMPPGM defined as RESIDENT, when loaded, then it stays in memory across invocations
- Given serial scheduling, when two messages for the same program queue, then they run sequentially

### IMS-TM104.6: Region & Scheduling Tests

**Acceptance Criteria:**
- Given MPP and BMP region scenarios with various scheduling parameters, when tested, then correct dispatching occurs

---

## IMS-TM105: Operator Command Framework

**User Value:** System programmers can manage the IMS system at runtime using standard operator commands.

### IMS-TM105.1: /DISPLAY Commands

**As an** IMS operator, **I want** /DISPLAY TRAN, /DISPLAY PGM, /DISPLAY ACTIVE for status queries.

**Acceptance Criteria:**
- Given `/DIS TRAN EMPTRAN`, when issued, then transaction status, queue depth, and associated program are displayed
- Given `/DIS ACTIVE REGION`, when issued, then all active dependent regions with current programs are displayed

### IMS-TM105.2: /START and /STOP Commands

**As an** IMS operator, **I want** /START and /STOP for transactions, programs, and regions.

**Acceptance Criteria:**
- Given `/STO TRAN EMPTRAN`, when issued, then new messages for EMPTRAN are queued but not scheduled
- Given `/STA TRAN EMPTRAN`, when issued, then scheduling resumes for EMPTRAN

### IMS-TM105.3: /ASSIGN and /CHANGE Commands

**As an** IMS operator, **I want** /ASSIGN and /CHANGE to modify resource attributes at runtime.

**Acceptance Criteria:**
- Given `/ASSIGN TRAN EMPTRAN CLASS 2`, when issued, then EMPTRAN's scheduling class changes to 2
- Given `/CHANGE PGM EMPPGM SCHDTYPE(PARALLEL)`, when issued, then parallel scheduling is enabled

### IMS-TM105.4: /CHECKPOINT and Recovery Commands

**As an** IMS operator, **I want** /CHECKPOINT, /ERE, and /NRE for system checkpoint and restart.

**Acceptance Criteria:**
- Given `/CHECKPOINT FREEZE`, when issued, then a system checkpoint is taken and new scheduling pauses
- Given `/ERE`, when issued after a failure, then IMS performs emergency restart

### IMS-TM105.5: CMD/GCMD — Programmatic Command Execution

**As an** IMS developer, **I want** CMD and GCMD calls to issue and retrieve IMS commands from applications.

**Acceptance Criteria:**
- Given CMD call with `/DIS TRAN ALL`, when issued from a program, then the I/O PCB receives command output on GCMD
- Given GCMD after CMD, when issued, then successive lines of command output are returned

### IMS-TM105.6: Operator Command Tests

**Acceptance Criteria:**
- Given all core operator commands, when tested, then correct state changes and output are produced

---

## IMS-TM106: OTMA Protocol (XCF-based)

**User Value:** External clients connect to IMS via high-performance XCF communication, enabling IMS Connect and MQ-IMS bridge.

### IMS-TM106.1: XCF Group Membership

**As an** IMS system, **I want** IMS to join an XCF group as a server member.

**Acceptance Criteria:**
- Given IMS configured with OTMA=Y and GROUP=IMSGRP1, when started, then IMS joins XCF group IMSGRP1

### IMS-TM106.2: TPIPE (Transaction Pipe) Management

**As an** OTMA client, **I want** named TPIPEs for bidirectional message exchange.

**Acceptance Criteria:**
- Given a client creates TPIPE "CLIENT01", when a message is sent through it, then IMS receives the message with TPIPE context
- Given output for CLIENT01, when IMS responds, then the message is routed back via the CLIENT01 TPIPE

### IMS-TM106.3: OTMA Message Prefix

**As an** OTMA client, **I want** structured message prefixes with control, state, security, and user data sections.

**Acceptance Criteria:**
- Given an OTMA message with commit mode CM1 (send-then-commit), when processed, then IMS sends the response before committing
- Given sync level CONFIRM, when the client acknowledges, then IMS commits the transaction

### IMS-TM106.4: OTMA Exits (DFSYPRX0/DFSYDRU0)

**As an** IMS system programmer, **I want** OTMA exits for routing and destination resolution.

**Acceptance Criteria:**
- Given DFSYPRX0 registered, when an OTMA message arrives, then the exit can modify routing (e.g., redirect to different transaction)
- Given DFSYDRU0, when output is routed, then the exit can modify the destination TPIPE

### IMS-TM106.5: OTMA Tests

**Acceptance Criteria:**
- Given OTMA client connection, message exchange, commit modes, and exit scenarios, when tested, then correct protocol behavior occurs

---

## IMS-TM107: IMS Connect Gateway (TCP/IP)

**User Value:** External applications access IMS transactions via TCP/IP with XML/JSON support, bridging mainframe transactions to modern architectures.

### IMS-TM107.1: TCP/IP Listener

**As an** IMS Connect administrator, **I want** IMS Connect to listen on configurable TCP ports.

**Acceptance Criteria:**
- Given port 9999 configured for IMS Connect, when a TCP client connects, then the connection is accepted
- Given multiple port definitions, when configured, then each port operates independently

### IMS-TM107.2: IRM/RSM Protocol

**As an** external client, **I want** IRM (IMS Request Message) and RSM (IMS Response Message) headers for structured communication.

**Acceptance Criteria:**
- Given IRM with transaction code EMPTRAN and data payload, when sent, then IMS schedules the transaction and returns RSM with response data
- Given RSM with status flags, when returned, then the client can determine success/failure

### IMS-TM107.3: Connection Management

**As an** IMS Connect system, **I want** persistent connection pooling and timeout management.

**Acceptance Criteria:**
- Given a client with keep-alive, when multiple requests are sent on the same connection, then they are processed sequentially
- Given a connection idle beyond timeout, when the timeout expires, then the connection is closed

### IMS-TM107.4: XML/JSON Adapter

**As an** external client, **I want** XML and JSON input/output conversion for IMS messages.

**Acceptance Criteria:**
- Given JSON input `{"tranCode":"EMPTRAN","data":{"empId":"12345"}}`, when received by IMS Connect, then the data is converted to IMS message format
- Given IMS output segments, when the response is returned, then it is formatted as JSON

### IMS-TM107.5: TLS Support

**As an** IMS Connect administrator, **I want** TLS/SSL for secure connections.

**Acceptance Criteria:**
- Given AT-TLS policy for IMS Connect port, when a client connects with TLS, then the connection is encrypted
- Given client certificate authentication, when the certificate maps to a RACF user, then the user is authenticated

### IMS-TM107.6: IMS Connect Tests

**Acceptance Criteria:**
- Given TCP connection, IRM/RSM exchange, JSON conversion, and TLS scenarios, when tested, then all pass

---

## IMS-TM108: Fast Path (EMH + IFP)

**User Value:** Ultra-high-volume transactions are processed with sub-millisecond response times via expedited message handling.

### IMS-TM108.1: EMH (Expedited Message Handler)

**As an** IMS system, **I want** EMH to bypass normal message queuing for Fast Path transactions.

**Acceptance Criteria:**
- Given a Fast Path transaction, when a message arrives, then EMH routes it directly to an IFP region without queuing

### IMS-TM108.2: IFP Region Type

**As an** IMS system programmer, **I want** IFP regions dedicated to Fast Path transactions.

**Acceptance Criteria:**
- Given an IFP region started, when a Fast Path message arrives, then the program is dispatched immediately in the IFP region
- Given the program completes, when response is ready, then EMH sends it without queue storage

### IMS-TM108.3: DEDB Integration

**As an** IMS developer, **I want** Fast Path programs to access DEDBs (Data Entry Databases).

**Acceptance Criteria:**
- Given a DEDB defined, when GU/GN/ISRT are issued from an IFP program, then DEDB-specific path is used for high performance

### IMS-TM108.4: Fast Path Tests

**Acceptance Criteria:**
- Given EMH, IFP region, and DEDB access scenarios, when tested, then correct fast-path processing occurs

---

## IMS-TM109: MSC & Shared Queues

**User Value:** Multiple IMS systems cooperate to process transactions, providing scalability and availability.

### IMS-TM109.1: MSC Physical/Logical Links

**As an** IMS system programmer, **I want** MSC links between IMS systems for transaction routing.

**Acceptance Criteria:**
- Given MSC link defined between IMS1 and IMS2, when a transaction on IMS1 targets a program on IMS2, then the message is routed via the MSC link

### IMS-TM109.2: Remote Transaction Routing

**As an** IMS system, **I want** remote destination definitions for cross-system message routing.

**Acceptance Criteria:**
- Given transaction REMTRAN defined as remote on IMS1 with home system IMS2, when entered on IMS1, then the message is forwarded to IMS2

### IMS-TM109.3: Shared Queues (Coupling Facility)

**As an** IMS system programmer, **I want** message queues shared across IMS systems via coupling facility.

**Acceptance Criteria:**
- Given shared queue enabled for transaction SHARED1, when IMS1 and IMS2 both have MPP regions, then either can process messages for SHARED1

### IMS-TM109.4: MSC & Shared Queue Tests

**Acceptance Criteria:**
- Given MSC routing and shared queue scenarios, when tested, then correct cross-system behavior occurs

---

## MFS-100: MFS Source Language Parser

**User Value:** MFS source definitions are parsed, enabling compilation of device-independent formatting control blocks.

### MFS-100.1: MSG/LPAGE/SEG/MFLD Statements

**As an** MFS developer, **I want** MSG, LPAGE, SEG, and MFLD statements parsed for message descriptors.

**Acceptance Criteria:**
- Given `MSG TYPE=INPUT,SOR=EMPFMT,NXT=EMPRSP`, when parsed, then a message descriptor with input type and SOR/NXT references is created
- Given `MFLD 'EMPNO',LTH=6,JUST=L,ATTR=NUM`, when parsed, then a message field with length=6, left-justified, numeric attribute is created

### MFS-100.2: FMT/DEV/DIV/DPAGE/DFLD Statements

**As an** MFS developer, **I want** FMT, DEV, DIV, DPAGE, and DFLD statements parsed for format descriptors.

**Acceptance Criteria:**
- Given `FMT EMPFMT`, when parsed, then a format descriptor is created
- Given `DFLD POS=(3,10),LTH=30,ATTR=(PROT,BRT)`, when parsed, then a device field at row 3, col 10, length 30 with protected/bright attributes is created

### MFS-100.3: Extended Attributes (Color, Highlighting)

**As an** MFS developer, **I want** EATTR parsing for 3270 extended attributes.

**Acceptance Criteria:**
- Given `EATTR=(HILIGHT=BLINK,COLOR=RED,PS=BASE)`, when parsed, then highlight=blink, color=red, programmed symbols=base

### MFS-100.4: COPY and TABLE Statements

**As an** MFS developer, **I want** COPY for reusing format definitions and TABLE for translation tables.

**Acceptance Criteria:**
- Given `COPY EMPFMT`, when processed, then the previously defined format is included
- Given TABLE with translation entries, when parsed, then character mapping is defined

### MFS-100.5: MFS Parser Tests

**Acceptance Criteria:**
- Given MFS source with all statement types, when parsed, then correct AST structures are produced
- Given malformed MFS, when parsed, then diagnostic errors include line numbers

---

## MFS-101: MFS Control Block Compiler

**User Value:** Parsed MFS source is compiled into binary control blocks (MID/MOD/DIF/DOF) stored in the IMS.FORMAT library.

### MFS-101.1: MID Generation (Message Input Descriptor)

**As an** MFS compiler, **I want** MSG TYPE=INPUT compiled into a MID control block.

**Acceptance Criteria:**
- Given a parsed MSG with LPAGE/SEG/MFLD definitions, when compiled, then a binary MID is produced mapping device input fields to logical message segments

### MFS-101.2: MOD Generation (Message Output Descriptor)

**As an** MFS compiler, **I want** MSG TYPE=OUTPUT compiled into a MOD control block.

**Acceptance Criteria:**
- Given a parsed MSG with output LPAGE/SEG/MFLD definitions, when compiled, then a binary MOD is produced mapping logical output segments to device fields

### MFS-101.3: DIF/DOF Generation

**As an** MFS compiler, **I want** FMT compiled into DIF (Device Input Format) and DOF (Device Output Format) control blocks.

**Acceptance Criteria:**
- Given a parsed FMT with DEV/DFLD definitions, when compiled for TYPE=INPUT, then a DIF with physical field layouts is produced
- Given TYPE=OUTPUT, when compiled, then a DOF with screen positioning and attributes is produced

### MFS-101.4: IMS.FORMAT Library Storage

**As an** MFS compiler, **I want** compiled control blocks stored in the IMS.FORMAT library (PDS).

**Acceptance Criteria:**
- Given compiled MID/MOD/DIF/DOF, when stored, then they are written as members in the IMS.FORMAT PDS
- Given an MFS compile, when the same member exists, then it is replaced

### MFS-101.5: MFS Compiler Tests

**Acceptance Criteria:**
- Given sample MFS source, when compiled and stored, then control blocks can be retrieved and match expected binary format

---

## MFS-102: MFS Runtime Integration

**User Value:** IMS runtime uses MFS control blocks to format input/output messages, providing device independence for application programs.

### MFS-102.1: Input Message Formatting (MID/DIF)

**As an** IMS runtime, **I want** incoming 3270 data formatted using DIF→MID mapping to produce logical message segments.

**Acceptance Criteria:**
- Given 3270 input with fields at specific positions, when DIF maps positions to DFLD names and MID maps DFLD to MFLD, then the logical message segment contains application data fields

### MFS-102.2: Output Message Formatting (MOD/DOF)

**As an** IMS runtime, **I want** outgoing logical messages formatted using MOD→DOF mapping to produce 3270 data streams.

**Acceptance Criteria:**
- Given application ISRT with message data, when MOD maps MFLD to DFLD and DOF maps DFLD to screen positions, then a 3270 data stream with correct positioning and attributes is produced

### MFS-102.3: Device Independence

**As an** IMS developer, **I want** the same program to work with different device types via different FMT definitions.

**Acceptance Criteria:**
- Given FMT with DEV TYPE=3270-A2 and DEV TYPE=SLU2, when the same MSG is used, then the appropriate device format is selected based on the terminal type

### MFS-102.4: MFS Bypass Mode

**As an** IMS developer, **I want** to bypass MFS for applications that handle their own formatting.

**Acceptance Criteria:**
- Given a transaction defined with no MFS (basic edit), when messages are exchanged, then raw data is passed without MFS formatting

### MFS-102.5: MFS Runtime Tests

**Acceptance Criteria:**
- Given 3270 input/output scenarios with MFS formatting, when tested, then correct device-independent message handling occurs

---

## IMS-TM110: EXEC DLI Code Generation

**User Value:** The existing EXEC DLI scanner is extended with COBOL CALL generation, completing the COBOL-to-IMS preprocessing pipeline.

### IMS-TM110.1: CBLTDLI CALL Generation

**As a** COBOL developer, **I want** EXEC DLI blocks replaced with CALL 'CBLTDLI' statements.

**Acceptance Criteria:**
- Given `EXEC DLI GU USING PCB(1) SEGMENT(CUSTOMER) INTO(WS-CUST) WHERE(CUSTNO = :WS-KEY) END-EXEC`, when transformed, then `CALL 'CBLTDLI' USING DLI-GU, PCB-001, WS-CUST, CUST-SSA` is generated
- Given the SSA, when generated in WORKING-STORAGE, then it contains the segment name and qualification

### IMS-TM110.2: DIB Copybook Generation

**As a** COBOL developer, **I want** a DIB (DL/I Interface Block) copybook generated for status checking.

**Acceptance Criteria:**
- Given an EXEC DLI program, when preprocessed, then DIBSTAT, DIBSEGM, DIBKFBL fields are generated in WORKING-STORAGE

### IMS-TM110.3: AIBTDLI/CEETDLI Support

**As a** COBOL developer, **I want** AIBTDLI and CEETDLI calling conventions as alternatives to CBLTDLI.

**Acceptance Criteria:**
- Given EXEC DLI with AIBTDLI convention, when transformed, then `CALL 'AIBTDLI' USING AIB, DLI-func, ...` is generated with AIB block
- Given CEETDLI, when transformed, then Language Environment calling conventions are used

### IMS-TM110.4: Code Generation Tests

**Acceptance Criteria:**
- Given COBOL with EXEC DLI blocks covering GU, GN, ISRT, DLET, REPL, CHKP, when preprocessed, then correct CBLTDLI/AIBTDLI CALLs are generated

---

## Dependency Graph

```
v3.0 Epics (existing):
  400 → 401 → 402 → 403 → 404 → 405 → 406 → 407 → 408 → 409 → 410

v5.0 Addendum (new):
  IMS-TM100 depends on: 402 (I/O PCB)
  IMS-TM101 depends on: IMS-TM100 (Alt PCB for SPA ISRT)
  IMS-TM102 depends on: 401 (System Services)
  IMS-TM103 depends on: 402 (I/O PCB)
  IMS-TM104 depends on: 402 (I/O PCB), IMS-TM100
  IMS-TM105 depends on: IMS-TM104 (Region Model)
  IMS-TM106 depends on: IMS-TM104 (Region Model)
  IMS-TM107 depends on: IMS-TM106 (OTMA)
  IMS-TM108 depends on: IMS-TM104 (Region Model)
  IMS-TM109 depends on: IMS-TM104 (Region Model)
  MFS-100 — independent (parser only)
  MFS-101 depends on: MFS-100
  MFS-102 depends on: MFS-101, 402 (I/O PCB)
  IMS-TM110 depends on: 400 (EXEC DLI scanner)
```
