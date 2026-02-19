# IBM MQ Crate — Epics & Stories

## Epic MQ100: MQ Core Runtime & Queue Manager

**Goal:** Implement the queue manager process model with local queues, connection management, and object handling.

**Crate:** `open-mainframe-mq`
**FRs:** FR-v4.0-MQ100

### Story MQ100.1: Queue Manager and Connection Handling

As a **MQ application developer**,
I want **MQCONN/MQCONNX/MQDISC to connect to and disconnect from a queue manager**,
So that **applications can establish sessions for messaging operations**.

**Acceptance Criteria:**

**Given** `MQCONN("QM01", &Hconn, &CompCode, &Reason)`
**When** the queue manager QM01 is running
**Then** a connection handle is returned with CompCode=MQCC_OK and Reason=MQRC_NONE

**Given** `MQDISC(&Hconn, &CompCode, &Reason)`
**When** invoked with a valid connection handle
**Then** the connection is closed and all uncommitted messages within a local UOW are backed out

**Given** `MQCONNX` with MQCNO option MQCNO_HANDLE_SHARE_BLOCK
**When** invoked
**Then** the connection handle can be shared across threads with blocking behavior

**Complexity:** L

### Story MQ100.2: Local Queue Implementation

As a **MQ application developer**,
I want **local queues with priority ordering, depth tracking, and configurable attributes**,
So that **messages can be stored and retrieved in priority order**.

**Acceptance Criteria:**

**Given** a local queue with MAXDEPTH(5000) and MAXMSGL(4194304)
**When** messages are put to the queue
**Then** messages are stored in priority order (0=lowest, 9=highest within MQMD.Priority)

**Given** the queue depth reaches MAXDEPTH
**When** another MQPUT is attempted
**Then** the put fails with MQRC_Q_FULL

**Given** a queue with DEFPSIST(MQPER_PERSISTENT)
**When** the queue manager restarts after a failure
**Then** persistent messages on the queue survive the restart

**Complexity:** L

### Story MQ100.3: Queue Types and Name Resolution

As a **MQ administrator**,
I want **alias queues, model queues, and dynamic queue creation**,
So that **I can abstract queue names and create temporary queues on demand**.

**Acceptance Criteria:**

**Given** an alias queue MYALIAS targeting base queue MYLOCAL
**When** `MQOPEN` for MYALIAS is called
**Then** the name is resolved to MYLOCAL and messages are placed on the local queue

**Given** a model queue MYMODEL with DEFTYPE(TEMPDYN)
**When** `MQOPEN` specifies the model queue name with MQOO_INPUT_AS_Q_DEF
**Then** a temporary dynamic queue is created; its name is returned in the MQOD.ObjectName field

**Given** the application closes the dynamic queue with MQCO_DELETE_PURGE
**When** the close completes
**Then** the temporary queue and all its messages are deleted

**Complexity:** M

---

## Epic MQ101: MQI Put/Get Operations

**Goal:** Implement MQPUT, MQPUT1, MQGET with full option processing, browse, and message locking.

**Crate:** `open-mainframe-mq`
**FRs:** FR-v4.0-MQ101

### Story MQ101.1: MQPUT and MQPUT1

As a **MQ application developer**,
I want **MQPUT to place messages on a queue and MQPUT1 for single-message convenience**,
So that **I can send messages to queues**.

**Acceptance Criteria:**

**Given** `MQPUT(Hconn, Hobj, &MQMD, &MQPMO, BufferLength, Buffer, &CompCode, &Reason)`
**When** invoked with a valid object handle for a local queue
**Then** the message is placed on the queue; MsgId is generated if MQMI_NONE specified

**Given** `MQPUT1(Hconn, &MQOD, &MQMD, &MQPMO, BufferLength, Buffer, &CompCode, &Reason)`
**When** invoked
**Then** the queue is opened, the message is put, and the queue is closed in a single call

**Given** MQPMO_SYNCPOINT option is set
**When** the message is put
**Then** the message is not visible to other applications until MQCMIT is called

**Complexity:** M

### Story MQ101.2: MQGET with Match and Browse

As a **MQ application developer**,
I want **MQGET to retrieve messages with matching, wait, and browse options**,
So that **I can consume or browse messages selectively**.

**Acceptance Criteria:**

**Given** `MQGET` with MQGMO_WAIT and WaitInterval=5000 on an empty queue
**When** a message arrives within 5 seconds
**Then** the message is returned; if no message arrives, MQRC_NO_MSG_AVAILABLE is returned

**Given** `MQGET` with MQMO_MATCH_CORREL_ID and a specific CorrelId
**When** invoked
**Then** only a message with the matching correlation ID is returned

**Given** `MQGET` with MQGMO_BROWSE_FIRST then subsequent MQGMO_BROWSE_NEXT calls
**When** invoked
**Then** messages are browsed without removal; the browse cursor advances through the queue

**Given** `MQGET` with MQGMO_LOCK followed by MQGET with MQGMO_MSG_UNDER_CURSOR
**When** invoked
**Then** the browsed message is locked (not available to other getters) then destructively consumed

**Complexity:** L

### Story MQ101.3: MQINQ and MQSET

As a **MQ application developer**,
I want **MQINQ to query and MQSET to modify queue attributes at runtime**,
So that **applications can dynamically inspect and adjust queue behavior**.

**Acceptance Criteria:**

**Given** `MQINQ(Hconn, Hobj, 3, Selectors, 3, IntAttrs, 0, CharAttrs, &CC, &Reason)` with selectors MQIA_CURRENT_Q_DEPTH, MQIA_MAX_Q_DEPTH, MQIA_OPEN_INPUT_COUNT
**When** invoked
**Then** the current depth, max depth, and input-open count are returned in IntAttrs

**Given** `MQSET` with selector MQIA_INHIBIT_GET and value MQQA_GET_INHIBITED
**When** invoked
**Then** subsequent MQGET calls to the queue fail with MQRC_GET_INHIBITED

**Complexity:** S

---

## Epic MQ102: MQ Data Structures

**Goal:** Implement MQMD, MQOD, MQGMO, MQPMO, and message header structures.

**Crate:** `open-mainframe-mq`
**FRs:** FR-v4.0-MQ102

### Story MQ102.1: MQMD Message Descriptor

As a **MQ application developer**,
I want **the full MQMD v1/v2 structure with all 28+ fields**,
So that **messages carry proper identity, routing, and delivery attributes**.

**Acceptance Criteria:**

**Given** an MQMD is initialized with MQMD_DEFAULT
**When** used in MQPUT
**Then** fields MsgId and CorrelId are generated (if MQMI_NONE), PutDate/PutTime are set by the queue manager

**Given** MQMD with Format=MQFMT_STRING and CodedCharSetId=500 (EBCDIC)
**When** the message is retrieved with MQGMO_CONVERT on a system using CCSID 1208 (UTF-8)
**Then** the message data is converted from EBCDIC to UTF-8

**Given** MQMD with Report=MQRO_COD (confirm on delivery)
**When** the message is successfully retrieved by a consumer
**Then** a report message is generated and sent to the ReplyToQ

**Complexity:** L

### Story MQ102.2: Object Descriptor and Option Structures

As a **MQ application developer**,
I want **MQOD, MQGMO, and MQPMO structures with all option flags**,
So that **I can control queue open, get, and put behavior**.

**Acceptance Criteria:**

**Given** MQOD with ObjectType=MQOT_Q, ObjectName='MYQUEUE', ObjectQMgrName='' (local)
**When** used in MQOPEN
**Then** the local queue MYQUEUE is opened

**Given** MQGMO with Options=MQGMO_WAIT|MQGMO_SYNCPOINT|MQGMO_FAIL_IF_QUIESCING
**When** used in MQGET
**Then** the get waits for a message, operates under syncpoint, and fails if the queue manager is shutting down

**Complexity:** M

### Story MQ102.3: Message Headers (DLH, XQH, RFH2, TM)

As a **MQ application developer**,
I want **MQDLH, MQXQH, MQRFH2, and MQTM structures**,
So that **dead-letter, transmission, properties, and trigger messages are properly formatted**.

**Acceptance Criteria:**

**Given** a message routed to the dead-letter queue
**When** placed on the DLQ
**Then** an MQDLH header is prepended containing the Reason, DestQName, DestQMgrName, and original put timestamp

**Given** a message destined for a remote queue
**When** placed on the transmission queue
**Then** an MQXQH header is prepended containing the RemoteQName and RemoteQMgrName

**Given** a message with MQRFH2 header containing message properties in `<usr>` folder
**When** retrieved by a subscriber with a selection string
**Then** the properties in the RFH2 are evaluated against the selection criteria

**Complexity:** M

---

## Epic MQ103: Remote Queuing & Channels

**Goal:** Implement the channel engine for inter-queue-manager message transfer over TCP/IP.

**Crate:** `open-mainframe-mq`
**FRs:** FR-v4.0-MQ103

### Story MQ103.1: Remote Queue Definitions and Transmission Queues

As a **MQ administrator**,
I want **remote queue definitions that route messages via transmission queues**,
So that **applications can send messages to queues on remote queue managers**.

**Acceptance Criteria:**

**Given** QREMOTE definition with RNAME('TARGET.Q'), RQMNAME('QM02'), XMITQ('QM02.XMITQ')
**When** an application opens the remote queue and puts a message
**Then** the message is placed on QM02.XMITQ with an MQXQH header for routing

**Given** no explicit XMITQ and a transmission queue named QM02 exists
**When** a message is put to a queue on QM02
**Then** the default transmission queue resolution finds QM02 as the XMITQ

**Complexity:** M

### Story MQ103.2: Channel Engine (Sender/Receiver)

As a **MQ administrator**,
I want **sender and receiver channels that transfer messages between queue managers over TCP/IP**,
So that **distributed messaging works across systems**.

**Acceptance Criteria:**

**Given** a sender channel SDR.QM01.QM02 with CONNAME('qm02host(1414)') and XMITQ('QM02.XMITQ')
**When** the channel is started
**Then** it connects to the receiver on QM02, retrieves messages from QM02.XMITQ, and transmits them

**Given** a receiver channel RCVR.QM01.QM02 on QM02
**When** it accepts a connection from QM01's sender
**Then** received messages are placed on their target local queues (per MQXQH routing)

**Given** the network connection drops during transfer
**When** the channel detects the failure
**Then** it enters RETRYING state and attempts reconnection per SHORTRTY/LONGRTY settings

**Complexity:** XL

### Story MQ103.3: Server-Connection and Client Channels

As a **MQ client application developer**,
I want **SVRCONN channels for client connections to the queue manager**,
So that **remote applications can connect via MQI client protocol**.

**Acceptance Criteria:**

**Given** a SVRCONN channel 'MY.SVRCONN' on the queue manager
**When** a client application connects with MQCONNX specifying the channel name and CONNAME
**Then** a client connection is established and MQI calls are routed over the network

**Given** SHARECNV(10) on the SVRCONN channel
**When** 10 conversations share a single TCP connection
**Then** each conversation operates independently with its own MQI handles

**Complexity:** L

---

## Epic MQ104: Syncpoint & Transaction Coordination

**Goal:** Implement local and global transaction support with persistent message recovery.

**Crate:** `open-mainframe-mq`
**FRs:** FR-v4.0-MQ104

### Story MQ104.1: Local Unit of Work

As a **MQ application developer**,
I want **MQCMIT and MQBACK for local unit-of-work management**,
So that **message puts and gets can be committed or rolled back atomically**.

**Acceptance Criteria:**

**Given** MQPUT with MQPMO_SYNCPOINT followed by MQCMIT
**When** MQCMIT completes successfully
**Then** the message becomes visible to consumers on the target queue

**Given** MQGET with MQGMO_SYNCPOINT followed by MQBACK
**When** MQBACK completes
**Then** the message is returned to the queue and is available for re-consumption

**Given** the application disconnects (MQDISC) without committing
**When** the disconnect is processed
**Then** all uncommitted operations within the local UOW are backed out

**Complexity:** L

### Story MQ104.2: Persistent Message Recovery and Logging

As a **MQ administrator**,
I want **write-ahead logging for persistent messages**,
So that **persistent messages survive queue manager restarts**.

**Acceptance Criteria:**

**Given** a persistent message is committed via MQCMIT
**When** the queue manager crashes and restarts
**Then** the message is recovered from the log and is available on the queue

**Given** an in-doubt unit of work at restart
**When** the queue manager performs recovery
**Then** in-doubt transactions are resolved (committed or backed out) based on log records

**Complexity:** L

---

## Epic MQ105: Publish/Subscribe Engine

**Goal:** Implement the topic tree, subscription management, and publication distribution.

**Crate:** `open-mainframe-mq`
**FRs:** FR-v4.0-MQ105

### Story MQ105.1: Topic Tree and Subscription Management

As a **MQ application developer**,
I want **MQSUB to create durable and non-durable subscriptions to topic strings**,
So that **applications receive messages published to matching topics**.

**Acceptance Criteria:**

**Given** `MQSUB` with TopicString='Price/Fruit/Apples' and MQSO_CREATE|MQSO_DURABLE
**When** invoked
**Then** a durable subscription is created; publications to 'Price/Fruit/Apples' are delivered to the subscription queue

**Given** a subscription with TopicString='Price/Fruit/#' (multilevel wildcard)
**When** messages are published to 'Price/Fruit/Apples' and 'Price/Fruit/Citrus/Lemons'
**Then** both publications are delivered to the subscriber

**Given** `MQSUB` with MQSO_MANAGED
**When** invoked
**Then** the queue manager creates a managed destination queue automatically

**Complexity:** L

### Story MQ105.2: Retained Publications and Selection Strings

As a **MQ application developer**,
I want **retained publications and SQL92-like selection strings for message filtering**,
So that **new subscribers receive the latest state and can filter by message properties**.

**Acceptance Criteria:**

**Given** MQPMO_RETAIN on a publish operation to topic 'Sensors/Temp/Room1'
**When** a new subscriber subscribes to 'Sensors/Temp/Room1'
**Then** the most recent retained publication is delivered immediately

**Given** a subscription with SelectionString `"Color = 'Red' AND Weight > 100"`
**When** a publication with message properties Color='Red', Weight=150 is published
**Then** the publication is delivered; a publication with Weight=50 is not

**Given** `MQSUBRQ` on an existing subscription
**When** invoked
**Then** the retained publication (if any) for the subscription topic is re-delivered

**Complexity:** L

---

## Epic MQ106: Clustering

**Goal:** Implement multi-queue-manager clustering with workload distribution and auto-defined channels.

**Crate:** `open-mainframe-mq`
**FRs:** FR-v4.0-MQ106

### Story MQ106.1: Cluster Membership and Repositories

As a **MQ administrator**,
I want **full and partial repository queue managers in a named cluster**,
So that **queue managers automatically discover each other and share queue definitions**.

**Acceptance Criteria:**

**Given** QM01 and QM02 are designated as full repositories for cluster MYCLUSTER
**When** QM03 joins the cluster via CLUSSDR/CLUSRCVR channel definitions
**Then** QM03 receives a partial repository of cluster metadata from a full repository

**Given** a cluster queue SHARED.Q is defined on QM02 and QM03
**When** QM01 opens SHARED.Q
**Then** both instances are discovered and messages are distributed between them

**Complexity:** L

### Story MQ106.2: Cluster Workload Distribution

As a **MQ administrator**,
I want **round-robin workload balancing with rank, priority, and weight controls**,
So that **messages are distributed across cluster queue instances according to policy**.

**Acceptance Criteria:**

**Given** SHARED.Q exists on QM02 (CLWLRANK=5) and QM03 (CLWLRANK=3)
**When** messages are put to SHARED.Q from QM01
**Then** all messages go to QM02 (higher rank); QM03 only receives messages if QM02 is unavailable

**Given** `SUSPEND QMGR CLUSTER(MYCLUSTER)` on QM02
**When** executed
**Then** QM02 stops receiving workload-balanced messages in MYCLUSTER

**Given** `REFRESH CLUSTER(MYCLUSTER)` on QM01
**When** executed
**Then** QM01 rebuilds its cluster metadata from the full repositories

**Complexity:** L

---

## Epic MQ107: Security

**Goal:** Implement MQ security with RACF integration, channel authentication, and TLS.

**Crate:** `open-mainframe-mq`
**FRs:** FR-v4.0-MQ107

### Story MQ107.1: RACF Resource Classes and Connection Authentication

As a **MQ administrator**,
I want **RACF-based access control for MQ resources and CONNAUTH for user authentication**,
So that **only authorized users and applications can access queues and issue commands**.

**Acceptance Criteria:**

**Given** RACF class MQQUEUE is active and profile `QM01.PAYROLL.Q` has UACC(NONE) with JSMITH permitted READ
**When** JSMITH issues MQOPEN for PAYROLL.Q with MQOO_INPUT_SHARED
**Then** access is granted (READ allows browse and get)

**Given** CONNAUTH is configured with AUTHINFO type IDPWOS and CHCKCLNT(REQUIRED)
**When** a client connects without providing credentials
**Then** the connection is rejected with MQRC_NOT_AUTHORIZED

**Complexity:** L

### Story MQ107.2: Channel Authentication and TLS

As a **MQ administrator**,
I want **CHLAUTH rules and SSL/TLS channel encryption**,
So that **channel connections are authenticated and encrypted**.

**Acceptance Criteria:**

**Given** `SET CHLAUTH('MY.SVRCONN') TYPE(BLOCKUSER) USERLIST('*MQADMIN')`
**When** an admin user connects through MY.SVRCONN
**Then** the connection is blocked

**Given** `SET CHLAUTH('MY.SVRCONN') TYPE(ADDRESSMAP) ADDRESS('10.0.1.*') MCAUSER('APPUSER')`
**When** a client connects from 10.0.1.50
**Then** the connection is accepted and runs under MCAUSER APPUSER

**Given** SSLCIPH('TLS_AES_256_GCM_SHA384') on a channel definition
**When** the channel starts
**Then** TLS 1.3 is negotiated using the specified cipher; certificates are validated from the RACF keyring

**Complexity:** L

---

## Epic MQ108: MQSC Command Engine

**Goal:** Implement the MQSC command parser and executor for administration of all MQ object types.

**Crate:** `open-mainframe-mq`
**FRs:** FR-v4.0-MQ108

### Story MQ108.1: DEFINE/ALTER/DELETE/DISPLAY Commands

As a **MQ administrator**,
I want **MQSC commands to create, modify, delete, and display MQ objects**,
So that **I can administer the queue manager from the console or batch**.

**Acceptance Criteria:**

**Given** `DEFINE QLOCAL('APP.REQUEST.Q') MAXDEPTH(10000) MAXMSGL(1048576) DEFPSIST(YES)`
**When** executed
**Then** a local queue is created with the specified attributes

**Given** `ALTER QLOCAL('APP.REQUEST.Q') MAXDEPTH(50000)`
**When** executed
**Then** only MAXDEPTH is changed; all other attributes remain unchanged

**Given** `DISPLAY QUEUE('APP.*') CURDEPTH MAXDEPTH IPPROCS OPPROCS`
**When** executed
**Then** all queues matching APP.* are listed with the requested attributes

**Given** `DELETE QLOCAL('OLD.QUEUE') PURGE`
**When** executed
**Then** the queue and all its messages are deleted

**Complexity:** L

### Story MQ108.2: Operational and Control Commands

As a **MQ administrator**,
I want **START/STOP/PING/CLEAR/RESET/RESOLVE commands**,
So that **I can control channels, clear queues, and manage runtime state**.

**Acceptance Criteria:**

**Given** `START CHANNEL('SDR.QM01.QM02')`
**When** executed
**Then** the sender channel initiates a connection to the remote receiver

**Given** `STOP CHANNEL('SDR.QM01.QM02') MODE(QUIESCE)`
**When** executed
**Then** the channel completes the current batch and stops gracefully

**Given** `PING CHANNEL('SDR.QM01.QM02')`
**When** executed
**Then** a test message is sent and echoed back; round-trip time is displayed

**Given** `CLEAR QLOCAL('TEMP.Q')`
**When** executed
**Then** all messages on the queue are deleted

**Complexity:** M

---

## Epic MQ109: PCF Programmatic Administration

**Goal:** Implement PCF (Programmable Command Format) for programmatic administration via messages.

**Crate:** `open-mainframe-mq`
**FRs:** FR-v4.0-MQ109

### Story MQ109.1: PCF Message Processing

As a **MQ automation developer**,
I want **PCF command messages processed via SYSTEM.ADMIN.COMMAND.QUEUE**,
So that **I can administer MQ programmatically from any application**.

**Acceptance Criteria:**

**Given** a PCF message with Command=MQCMD_INQUIRE_Q, parameter QName='APP.*'
**When** put to SYSTEM.ADMIN.COMMAND.QUEUE
**Then** response messages are generated on the ReplyToQ with one MQCFH+parameters per matching queue

**Given** a PCF message with Command=MQCMD_CHANGE_Q
**When** processed
**Then** the queue attributes are modified and a success response is returned

**Given** a PCF Escape message (MQCMD_ESCAPE) containing MQSC text
**When** processed
**Then** the MQSC command is parsed and executed as if issued directly

**Complexity:** M

---

## Epic MQ110: z/OS-Specific Features

**Goal:** Implement z/OS-specific MQ features: page sets, buffer pools, storage classes, and logging.

**Crate:** `open-mainframe-mq`
**FRs:** FR-v4.0-MQ110

### Story MQ110.1: Page Sets, Buffer Pools, and Storage Classes

As a **MQ system programmer**,
I want **page sets for persistent storage, buffer pools for caching, and storage classes for mapping**,
So that **MQ storage is configured for z/OS performance and capacity**.

**Acceptance Criteria:**

**Given** `DEFINE BUFFPOOL(0) BUFFERS(1000)`
**When** executed
**Then** buffer pool 0 is created with 1000 4KB buffers for caching page set data

**Given** `DEFINE PSID(01) BUFFPOOL(0)` and a formatted page set dataset
**When** the queue manager starts
**Then** page set 01 uses buffer pool 0 for I/O caching

**Given** `DEFINE STGCLASS('FAST') PSID(01)`
**When** a queue with STGCLASS('FAST') receives a message
**Then** the message is stored on page set 01

**Complexity:** L

### Story MQ110.2: Active/Archive Logging and BSDS

As a **MQ system programmer**,
I want **active and archive log management with BSDS (Bootstrap Data Set)**,
So that **MQ can recover persistent messages and transactions after failure**.

**Acceptance Criteria:**

**Given** dual active log datasets configured in the BSDS
**When** the active log fills
**Then** it is offloaded to an archive log dataset and the BSDS inventory is updated

**Given** a queue manager restart after abnormal termination
**When** recovery begins
**Then** the BSDS is read to locate log datasets, and forward recovery replays committed transactions

**Complexity:** L

### Story MQ110.3: CSQUTIL Batch Utility

As a **MQ system programmer**,
I want **CSQUTIL for batch administration (COMMAND, COPY, LOAD, EMPTY, SDEFS)**,
So that **I can administer MQ from JCL batch jobs**.

**Acceptance Criteria:**

**Given** `CSQUTIL` with `COMMAND` function and SYSIN containing `DISPLAY QLOCAL(*) CURDEPTH`
**When** the batch job runs
**Then** the MQSC commands are sent to the queue manager and responses are written to SYSPRINT

**Given** `CSQUTIL` with `COPY` function specifying queue names
**When** executed
**Then** messages are non-destructively copied from the queues to a sequential dataset

**Given** `CSQUTIL` with `LOAD` function
**When** executed
**Then** messages from the sequential dataset are loaded back onto the specified queues

**Complexity:** M

---

## Epic MQ111: Triggering & Dead-Letter Handling

**Goal:** Implement queue triggering for application auto-start and dead-letter queue processing.

**Crate:** `open-mainframe-mq`
**FRs:** FR-v4.0-MQ111

### Story MQ111.1: Trigger Types and Trigger Monitor

As a **MQ application developer**,
I want **FIRST/EVERY/DEPTH trigger types with a trigger monitor**,
So that **applications are started automatically when messages arrive**.

**Acceptance Criteria:**

**Given** a queue with TRIGGER(ON), TRIGTYPE(FIRST), INITQ('SYSTEM.DEFAULT.INITIATION.QUEUE')
**When** the first message arrives on the empty queue
**Then** a trigger message (MQTM) is placed on the initiation queue

**Given** TRIGTYPE(DEPTH) with TRIGDPTH(100)
**When** the queue depth reaches 100
**Then** a trigger fires and TRIGGER is automatically set to OFF

**Given** a trigger monitor reading the initiation queue
**When** it retrieves an MQTM message
**Then** it starts the application identified in the associated process definition (APPLICID)

**Complexity:** M

### Story MQ111.2: Dead-Letter Queue and DLQ Handler

As a **MQ administrator**,
I want **undeliverable messages routed to the dead-letter queue with a rules-based handler**,
So that **failed messages are captured and can be retried or discarded**.

**Acceptance Criteria:**

**Given** MQPUT to a full queue (MAXDEPTH reached) on a channel receiver
**When** the put fails
**Then** the message is placed on the DLQ with an MQDLH header containing MQRC_Q_FULL

**Given** a DLQ handler rules table with `REASON(MQRC_Q_FULL) ACTION(RETRY) RETRIES(3) FWDQ('ERROR.Q')`
**When** the handler processes a message with reason MQRC_Q_FULL
**Then** it retries putting to the original queue up to 3 times; if still failing, forwards to ERROR.Q

**Complexity:** M

---

## Epic MQ112: MQ Bridges

**Goal:** Implement MQ-CICS and MQ-IMS bridges for routing messages to subsystem transactions.

**Crate:** `open-mainframe-mq`
**FRs:** FR-v4.0-MQ112

### Story MQ112.1: MQ-CICS Bridge

As a **MQ application developer**,
I want **messages routed to CICS transactions via the MQ-CICS bridge**,
So that **MQ clients can invoke CICS programs without direct CICS connectivity**.

**Acceptance Criteria:**

**Given** a message with MQMD.Format=MQFMT_CICS on the CICS bridge request queue
**When** the bridge processes the message
**Then** the specified CICS transaction is invoked via DPL (Distributed Program Link) and the response is returned

**Given** the CICS transaction abends
**When** the bridge detects the failure
**Then** an error report message is generated and sent to the ReplyToQ

**Complexity:** L

### Story MQ112.2: MQ-IMS Bridge (OTMA)

As a **MQ application developer**,
I want **messages routed to IMS transactions via OTMA**,
So that **MQ clients can invoke IMS transactions without direct IMS connectivity**.

**Acceptance Criteria:**

**Given** a message on the MQ-IMS bridge queue with IMS transaction code in the MQIIH header
**When** the bridge processes the message
**Then** the IMS transaction is invoked via OTMA and the reply is placed on the ReplyToQ

**Given** the IMS transaction returns multiple segments
**When** the reply is constructed
**Then** all output segments are assembled into the reply message with proper MQIIH formatting

**Complexity:** L

---
