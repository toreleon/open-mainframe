# Gap Analysis: IBM MQ (Message Queue)

## Official Specification Summary

IBM MQ (formerly WebSphere MQ and MQSeries) is IBM's enterprise messaging middleware, providing reliable asynchronous message delivery between applications across platforms. First released in 1993, IBM MQ is the dominant messaging middleware on z/OS mainframes and is core infrastructure at most large enterprises.

IBM MQ is classified as **Core** (essential middleware) on mainframes:
- Provides assured once-and-only-once message delivery between applications
- Supports point-to-point messaging and publish/subscribe patterns
- Message Queue Interface (MQI) is the primary API — ~20 core calls used by COBOL, PL/I, C, Java, and other language applications
- Queue manager is the central server process managing queues, channels, and connections
- z/OS-specific features include queue sharing groups (QSG), coupling facility integration, and shared queues
- Bridges to CICS (MQ-CICS bridge), IMS (MQ-IMS bridge/OTMA), and batch applications
- MQSC commands for administration; PCF (Programmable Command Format) for programmatic admin
- Current version: IBM MQ 9.4 (LTS and CD releases)

Key documentation:
- **IBM MQ 9.4 Application Programming Reference (SC34-7606)** — MQI API reference
- **IBM MQ 9.4 MQSC Command Reference (SC34-7602)** — administration commands
- **IBM MQ 9.4 System Administration Guide for z/OS (SC34-7601)** — z/OS operations
- **IBM MQ 9.4 Programming Guide (SC34-7605)** — application development
- **IBM MQ 9.4 Security Reference (SC34-7608)** — security configuration

## Key Features & Capabilities

### 1. MQI (Message Queue Interface) API Calls (~20 core calls)

#### Connection Management
| API Call | Purpose |
|----------|---------|
| MQCONN | Connect to queue manager (returns connection handle) |
| MQCONNX | Extended connect with options (client/shared/fast-path bindings) |
| MQDISC | Disconnect from queue manager |

#### Queue Operations
| API Call | Purpose |
|----------|---------|
| MQOPEN | Open a queue or topic for access (returns object handle) |
| MQCLOSE | Close an object handle and release resources |
| MQPUT | Put a message on an open queue |
| MQPUT1 | Combined open-put-close for single message (convenience) |
| MQGET | Get (receive) a message from a queue |

#### Inquiry/Set
| API Call | Purpose |
|----------|---------|
| MQINQ | Inquire about object attributes (queue depth, etc.) |
| MQSET | Set object attributes dynamically |

#### Transaction Coordination
| API Call | Purpose |
|----------|---------|
| MQCMIT | Commit current unit of work (syncpoint) |
| MQBACK | Back out (rollback) current unit of work |
| MQBEGIN | Begin a unit of work (for global transactions) |

#### Subscribe/Publish
| API Call | Purpose |
|----------|---------|
| MQSUB | Register a subscription to a topic |
| MQSUBRQ | Request retained publications for a subscription |

#### Message Properties (MQ v7+)
| API Call | Purpose |
|----------|---------|
| MQCRTMH | Create a message handle for properties |
| MQDLTMH | Delete a message handle |
| MQSETMP | Set a message property |
| MQINQMP | Inquire about a message property |
| MQDLTMP | Delete a message property |

#### Callback (Asynchronous Consumption)
| API Call | Purpose |
|----------|---------|
| MQCB | Register a callback function for async message delivery |
| MQCTL | Control callback processing (START/STOP/SUSPEND/RESUME) |
| MQSTAT | Retrieve status information for async operations |

### 2. Key Data Structures

#### MQMD (Message Descriptor) — ~28 fields
| Field | Type | Purpose |
|-------|------|---------|
| StrucId | MQCHAR4 | Structure identifier ("MD  ") |
| Version | MQLONG | Version (1 or 2) |
| Report | MQLONG | Report options (COA/COD/Exception/Expiry) |
| MsgType | MQLONG | Request/Reply/Datagram/Report |
| Expiry | MQLONG | Message lifetime (tenths of seconds) |
| Feedback | MQLONG | Feedback/reason code for reports |
| Encoding | MQLONG | Numeric encoding of data |
| CodedCharSetId | MQLONG | Character set (CCSID) of data |
| Format | MQCHAR8 | Format name (MQSTR, MQHRF2, user-defined) |
| Priority | MQLONG | Message priority (0-9) |
| Persistence | MQLONG | MQPER_PERSISTENT / MQPER_NOT_PERSISTENT |
| MsgId | MQBYTE24 | Message identifier |
| CorrelId | MQBYTE24 | Correlation identifier |
| BackoutCount | MQLONG | Number of backout attempts |
| ReplyToQ | MQCHAR48 | Reply-to queue name |
| ReplyToQMgr | MQCHAR48 | Reply-to queue manager name |
| UserIdentifier | MQCHAR12 | User ID of sender |
| AccountingToken | MQBYTE32 | Accounting token |
| ApplIdentityData | MQCHAR32 | Application identity data |
| PutApplType | MQLONG | Application type (CICS/IMS/Batch/etc.) |
| PutApplName | MQCHAR28 | Application name |
| PutDate | MQCHAR8 | Date message was put (YYYYMMDD) |
| PutTime | MQCHAR8 | Time message was put (HHMMSSTH) |
| ApplOriginData | MQCHAR4 | Application origin data |
| GroupId | MQBYTE24 | Group identifier (v2) |
| MsgSeqNumber | MQLONG | Sequence number within group (v2) |
| Offset | MQLONG | Offset of data in physical message (v2) |
| MsgFlags | MQLONG | Message flags — segmentation/grouping (v2) |
| OriginalLength | MQLONG | Original data length before segmentation (v2) |

#### MQOD (Object Descriptor) — Key fields
| Field | Purpose |
|-------|---------|
| ObjectType | Queue/Topic/Process/Namelist/QMgr |
| ObjectName | Name of object to open (48 chars) |
| ObjectQMgrName | Queue manager owning the object |
| DynamicQName | Name template for dynamic queues |
| AlternateUserId | Alternate user ID for authority checks |
| RecsPresent | Number of object records (for distribution lists) |
| ObjectString | Extended topic string for pub/sub |
| SelectionString | Selector for subscription filtering |
| ResObjectString | Resolved topic string returned |

#### MQGMO (Get Message Options) — Key options
| Option | Purpose |
|--------|---------|
| MQGMO_WAIT | Wait for message (with WaitInterval) |
| MQGMO_NO_WAIT | Return immediately if no message |
| MQGMO_SYNCPOINT | Get under syncpoint control |
| MQGMO_BROWSE_FIRST | Browse first message (non-destructive) |
| MQGMO_BROWSE_NEXT | Browse next message |
| MQGMO_MSG_UNDER_CURSOR | Get browsed message destructively |
| MQGMO_LOCK | Lock message for exclusive access |
| MQGMO_UNLOCK | Unlock previously locked message |
| MQGMO_ACCEPT_TRUNCATED_MSG | Accept message truncation |
| MQGMO_SET_SIGNAL | Set signal for message arrival (z/OS) |
| MQGMO_CONVERT | Convert message data on get |
| MQGMO_LOGICAL_ORDER | Get messages in logical group order |
| MQGMO_COMPLETE_MSG | Get only complete (non-segmented) messages |
| MQGMO_ALL_MSGS_AVAILABLE | Wait for all group segments |
| MatchOptions | Match by MsgId/CorrelId/GroupId |

#### MQPMO (Put Message Options) — Key options
| Option | Purpose |
|--------|---------|
| MQPMO_SYNCPOINT | Put under syncpoint control |
| MQPMO_NO_SYNCPOINT | Put outside syncpoint |
| MQPMO_NEW_MSG_ID | Generate new MsgId |
| MQPMO_NEW_CORREL_ID | Generate new CorrelId |
| MQPMO_LOGICAL_ORDER | Maintain group/segment ordering |
| MQPMO_SET_IDENTITY_CONTEXT | Set identity context fields |
| MQPMO_SET_ALL_CONTEXT | Set all context fields |
| MQPMO_ALTERNATE_USER_AUTHORITY | Use alternate user for auth |
| MQPMO_RESOLVE_LOCAL_Q | Return resolved queue name |
| MQPMO_WARN_IF_NO_SUBS_MATCHED | Warn if pub has no subscribers |
| ResolvedQName | Output: resolved target queue name |
| ResolvedQMgrName | Output: resolved queue manager name |

### 3. Queue Types

| Type | Description |
|------|-------------|
| Local queue (QLOCAL) | Messages stored on local queue manager |
| Remote queue (QREMOTE) | Definition pointing to queue on another queue manager |
| Alias queue (QALIAS) | Alternate name for another queue or topic |
| Model queue (QMODEL) | Template for creating dynamic queues at MQOPEN time |
| Cluster queue | Local queue advertised to cluster members |
| Shared queue (z/OS) | Queue stored in coupling facility, accessible by all QSG members |
| Transmission queue (XMITQ) | Staging area for messages destined for remote queue managers |
| Dead-letter queue (DLQ) | Undeliverable messages routed here with MQDLH header |
| Initiation queue | Receives trigger messages to start applications |
| Command queue | SYSTEM.COMMAND.INPUT — receives MQSC/PCF commands |
| Reply-to queue | Application-defined queue for receiving replies |
| Dynamic queue | Created at runtime from a model queue template |

### 4. Channel Types

#### Message Channels (unidirectional, inter-QMgr)
| Type | Keyword | Purpose |
|------|---------|---------|
| Sender | SDR | Initiates connection, sends from transmission queue |
| Receiver | RCVR | Accepts connection, puts messages to destination queue |
| Server | SVR | Sends (can be triggered or started by requester) |
| Requester | RQSTR | Requests server to start sending |
| Cluster-sender | CLUSSDR | Sends cluster info and messages to full repository |
| Cluster-receiver | CLUSRCVR | Receives messages and cluster info from cluster members |

#### MQI Channels (bidirectional, client-to-QMgr)
| Type | Keyword | Purpose |
|------|---------|---------|
| Server-connection | SVRCONN | Queue manager end of client connection |
| Client-connection | CLNTCONN | Client end of client connection |

#### AMQP Channel
| Type | Keyword | Purpose |
|------|---------|---------|
| AMQP | AMQP | Accepts AMQP 1.0 client connections |

#### Channel Features
- **Transmission protocols**: TCP/IP, LU 6.2 (SNA), NetBIOS, SPX
- **SSL/TLS support**: CipherSpecs, certificate authentication, OCSP/CRL checking
- **Channel exits**: Security exit, message exit, send exit, receive exit, channel auto-definition exit
- **Channel authentication records (CHLAUTH)**: IP address mapping/blocking, user mapping, SSL DN mapping, QMgr name mapping
- **Message sequence numbering**: Ensures delivery order and detects gaps
- **Heartbeating**: Configurable heartbeat interval for idle channels
- **Compression**: Message data and header compression (ZLIBFAST/ZLIBHIGH)

### 5. Triggering

| Trigger Type | Description |
|--------------|-------------|
| FIRST | Trigger when first message arrives on empty queue |
| EVERY | Trigger on every message arrival |
| DEPTH | Trigger when queue depth reaches configured threshold |
| NONE | Triggering disabled |

Triggering components:
- **Trigger monitor** (CKTI on z/OS CICS, CSQQTRMN for batch): Monitors initiation queue
- **Initiation queue**: Receives trigger messages (MQTM structure)
- **Process definition**: Specifies application to start when triggered
- **Trigger message (MQTM)**: Contains queue name, process name, trigger data, application type

### 6. Dead-Letter Queue (DLQ) Handling

- Every queue manager should have a DLQ defined (SYSTEM.DEAD.LETTER.QUEUE)
- Undeliverable messages get a **MQDLH** (Dead Letter Header) prepended
- DLQ header contains: reason code, destination queue/QMgr, put date/time, format
- **DLQ handler**: Rule-based utility that processes DLQ messages
  - z/OS: **CSQUDLQH** utility
  - Distributed: **runmqdlq** command
- Rules table specifies actions: RETRY, FORWARD, DISCARD based on reason/queue/format

### 7. Publish/Subscribe

| Feature | Description |
|---------|-------------|
| Topic trees | Hierarchical topic namespace using '/' delimiters (e.g., `Price/Fruit/Apples`) |
| Topic objects | Administrative definition mapped to topic string node |
| Durable subscriptions | Persist across subscriber disconnects; accumulate messages |
| Non-durable subscriptions | Removed when subscriber disconnects |
| Managed subscriptions | Queue manager creates/deletes subscription queue automatically |
| Unmanaged subscriptions | Application specifies existing queue for publications |
| Retained publications | Last publication on topic kept for new subscribers |
| Selection strings | SQL92-like filters on message properties |
| Proxy subscriptions | Propagated across cluster for distributed pub/sub |
| TREELIFE | Queue manager attribute controlling dynamic topic node lifetime |

System queues for pub/sub:
- `SYSTEM.DURABLE.SUBSCRIBER.QUEUE` — stores durable subscription messages
- `SYSTEM.RETAINED.PUB.QUEUE` — stores retained publications

### 8. Clustering

| Feature | Description |
|---------|-------------|
| Full repository | Queue manager holding complete cluster state (2 recommended) |
| Partial repository | Queue manager holding subset of cluster state (most members) |
| Cluster channels | CLUSSDR/CLUSRCVR auto-defined between members as needed |
| Cluster queues | Local queues advertised to all cluster members |
| Workload balancing | Round-robin or custom (via cluster workload exit) |
| Cluster workload exit | User exit for custom routing decisions |
| Overlapping clusters | Queue manager can belong to multiple clusters |
| REFRESH CLUSTER | Rebuild cluster state from full repositories |
| SUSPEND/RESUME QMGR | Temporarily remove QMgr from cluster workload |

### 9. z/OS-Specific Features

#### Queue Sharing Groups (QSG)
| Feature | Description |
|---------|-------------|
| Shared queues | Messages stored in coupling facility, accessible by all QSG members |
| Coupling facility structures | CF list structures hold shared queue messages |
| CSQ_ADMIN | Administration CF structure (coordination, UOW recovery) |
| Application structures | User-defined CF structures for shared queues |
| SMDS (Shared Message Data Sets) | Offload large messages from CF entries |
| Db2 offload | Alternative large message storage (older, slower than SMDS) |
| Peer channel recovery | Failed shared channels restart on another QSG member |
| Intra-group queuing (IGQ) | Fast message transfer between QSG members without channels |
| Group listener | Single VIPA address for client connections to any QSG member |

#### z/OS Batch Utilities
| Utility | Purpose |
|---------|---------|
| CSQUTIL | Multi-function batch utility: issue commands, copy/load queues, migrate |
| CSQUDLQH | Dead-letter queue handler (rule-based processing) |
| CSQJU003 | Change log inventory (BSDS) |
| CSQJU004 | Print log data from BSDS |
| CSQ0UTIL | Message security policy utility (AMS) |
| CSQ5PQSG | QSG verification and repair |

#### z/OS Storage & Logging
| Feature | Description |
|---------|-------------|
| Page sets | Storage for persistent/non-persistent messages (00-99) |
| Buffer pools | In-memory cache for page set data (0-99) |
| Storage classes | Map queues to page sets |
| Active logs | Circular log datasets for recovery |
| Archive logs | Long-term log retention |
| BSDS | Bootstrap Data Set — log inventory |

#### z/OS Commands (CSQ operator commands)
| Command Prefix | Purpose |
|----------------|---------|
| +CSQ1 DISPLAY | Display queue manager attributes |
| +CSQ1 START QMGR | Start queue manager subsystem |
| +CSQ1 STOP QMGR | Stop queue manager subsystem |
| +CSQ1 START CHINIT | Start channel initiator |
| +CSQ1 START LISTENER | Start TCP/IP or LU 6.2 listener |

### 10. Transaction Coordination

| Feature | Description |
|---------|-------------|
| Local UOW | Single queue manager syncpoint (MQCMIT/MQBACK) |
| Global UOW | MQBEGIN starts XA-coordinated transaction |
| CICS coordination | MQ participates in CICS unit of work via RRS |
| IMS coordination | MQ participates in IMS sync points |
| Batch coordination | RRS (Resource Recovery Services) for z/OS batch |
| Two-phase commit | Full 2PC support via RRS on z/OS |
| In-doubt resolution | RESOLVE INDOUBT command for stuck threads |

### 11. Security

#### z/OS Security (RACF Integration)
| RACF Class | Purpose |
|------------|---------|
| MQCONN | Connection authority |
| MQADMIN/MXADMIN | Administration command authority |
| MQQUEUE/MXQUEUE | Queue access control |
| MQPROC/MXPROC | Process definition access |
| MQNLIST/MXNLIST | Namelist access |
| MQTOPIC/MXTOPIC | Topic access (pub/sub) |
| MQCMDS | Command authority |

MQ-specific access levels: NONE, CONNECT, BROWSE, INPUT, OUTPUT, INQUIRE, SET, PASSALL, PASSID, SETALL, SETID, CONTROL, ALTUSR, ALLMQI, DSP, CTL, CHG, CLR, CRT, DLT

#### Channel Security
| Feature | Description |
|---------|-------------|
| SSL/TLS | Channel encryption using CipherSpecs |
| CHLAUTH | Channel authentication records (IP/user/DN/QMgr rules) |
| CONNAUTH | Connection authentication (user ID/password via OS or LDAP) |
| Certificate auth | Mutual TLS authentication via SSLPEER |
| Security exits | Custom authentication via user exit programs |

#### Advanced Message Security (AMS)
| Feature | Description |
|---------|-------------|
| Message signing | Digital signature on message content |
| Message encryption | Encrypt message data per security policy |
| Security policies | Per-queue policies for sign/encrypt requirements |

### 12. MQSC Commands (~100+ commands)

#### DEFINE Commands (16+)
AUTHINFO, BUFFPOOL, CFSTRUCT, CHANNEL, COMMINFO, LISTENER, LOG, MAXSMSGS, NAMELIST, PROCESS, PSID, QLOCAL/QREMOTE/QMODEL/QALIAS, SERVICE, STGCLASS, SUB, TOPIC

#### ALTER Commands (17+)
AUTHINFO, BUFFPOOL, CFSTRUCT, CHANNEL, COMMINFO, LISTENER, NAMELIST, PROCESS, PSID, QMGR, QLOCAL/QREMOTE/QMODEL/QALIAS, SECURITY, SERVICE, SMDS, STGCLASS, SUB, TOPIC, TRACE

#### DISPLAY Commands (35+)
APSTATUS, ARCHIVE, AUTHINFO, AUTHREC, AUTHSERV, CFSTATUS, CFSTRUCT, CHANNEL, CHINIT, CHLAUTH, CHSTATUS, CLUSQMGR, CMDSERV, COMMINFO, CONN, ENTAUTH, GROUP, LISTENER, LOG, LSSTATUS, MAXSMSGS, NAMELIST, POLICY, PROCESS, PUBSUB, QMGR, QMSTATUS, QSTATUS, QUEUE, SBSTATUS, SECURITY, SERVICE, SMDS, SMDSCONN, STGCLASS, SUB, SVSTATUS, SYSTEM, TCLUSTER, THREAD, TOPIC, TPSTATUS, TRACE, USAGE

#### DELETE Commands (16+)
AUTHINFO, AUTHREC, BUFFPOOL, CFSTRUCT, CHANNEL, COMMINFO, LISTENER, NAMELIST, POLICY, PROCESS, PSID, QLOCAL/QREMOTE/QMODEL/QALIAS, SERVICE, STGCLASS, SUB, TOPIC

#### START/STOP Commands (8+8)
START: CHANNEL, CHINIT, CMDSERV, LISTENER, QMGR, SERVICE, SMDSCONN, TRACE
STOP: CHANNEL, CHINIT, CMDSERV, CONN, LISTENER, QMGR, SERVICE, SMDSCONN, TRACE

#### Other Commands
| Command | Purpose |
|---------|---------|
| CLEAR QLOCAL | Remove all messages from a local queue |
| CLEAR TOPICSTR | Clear retained publication for topic |
| PING CHANNEL | Test channel connectivity |
| PING QMGR | Test queue manager responsiveness |
| RESET CHANNEL | Reset message sequence number |
| RESET CFSTRUCT | Modify CF structure status |
| RESET CLUSTER | Special cluster operations |
| RESET QMGR | Backup/recovery operations |
| RESET QSTATS | Report/reset queue performance data (z/OS) |
| RESOLVE CHANNEL | Commit/backout in-doubt messages |
| RESOLVE INDOUBT | Resolve in-doubt threads (z/OS) |
| REFRESH CLUSTER | Rebuild cluster state |
| REFRESH QMGR | Special queue manager refresh |
| REFRESH SECURITY | Refresh security cache |
| SUSPEND/RESUME QMGR | Control cluster availability |
| SET ARCHIVE | Change archive settings (z/OS) |
| SET AUTHREC | Configure authority records |
| SET CHLAUTH | Create/modify channel authentication records |
| SET LOG | Log management |
| SET POLICY | Configure security policies |
| SET SYSTEM | Change system parameters (z/OS) |
| ARCHIVE LOG | Backup active log (z/OS) |
| BACKUP CFSTRUCT | Backup CF structure (z/OS) |
| MOVE QLOCAL | Move messages between queues (z/OS) |
| RECOVER BSDS | Reestablish dual BSDS (z/OS) |
| RECOVER CFSTRUCT | Recover CF structure (z/OS) |
| RVERIFY SECURITY | Force user reverification (z/OS) |

### 13. PCF (Programmable Command Format)

| Feature | Description |
|---------|-------------|
| PCF messages | Self-describing MQ messages for programmatic administration |
| Command server | Processes PCF commands from SYSTEM.COMMAND.INPUT queue |
| PCF commands | Mirror MQSC: MQCMD_INQUIRE_Q, MQCMD_CREATE_Q, MQCMD_CHANGE_Q, etc. |
| PCF response | Returned to reply-to queue with object attributes |
| Escape PCF | PCF wrapper containing MQSC command text |
| MQAI (Admin Interface) | Simplified API using data bags for PCF programming |
| Remote admin | PCF commands can target any queue manager reachable via channels |

### 14. MQ Bridges

| Bridge | Description |
|--------|-------------|
| MQ-CICS bridge | Route MQ messages to CICS transactions (DPL/3270 bridge) |
| MQ-IMS bridge | Route MQ messages to IMS transactions via OTMA |
| MQ-CICS DPL bridge | Invoke CICS programs as distributed program links |
| HTTP bridge | REST/HTTP access to MQ queues (mqweb server) |
| AMQP channel | Accept AMQP 1.0 client connections |

## Current OpenMainframe Status

**No IBM MQ implementation exists in the codebase.**

Comprehensive search results:
- Zero MQ API calls (MQCONN, MQOPEN, MQPUT, MQGET, etc.) in source code
- Zero MQ data structures (MQMD, MQOD, MQGMO, MQPMO) in source code
- Zero MQ-related Cargo dependencies
- No `open-mainframe-mq` crate directory
- No MQ channel, queue manager, or messaging abstractions

**False positives identified and excluded:**
- CICS `channels.rs` — CICS inter-program channels, not MQ channels
- IMS `runtime/mod.rs` — IMS transaction message queues, not IBM MQ queues
- `target/` directory — Rust compiler hash artifacts containing "mq" substring

**Related existing infrastructure:**
- CICS crate supports channels/containers (EXEC CICS GET CONTAINER / PUT CONTAINER) — could eventually bridge to MQ-CICS bridge
- IMS crate has DL/I and basic message processing — could bridge to MQ-IMS/OTMA
- RACF gap analysis (Batch 8) identified MQCONN, MQADMIN, MQQUEUE, MQPROC resource classes
- Encoding crate handles EBCDIC/CCSID conversion needed for MQ message data

## Gap Details

| # | Feature | Official z/OS | OpenMainframe | Gap |
|---|---------|--------------|---------------|-----|
| 1 | MQI core API (MQCONN/MQDISC/MQOPEN/MQCLOSE/MQPUT/MQGET) | 8 core calls | None | **Missing** |
| 2 | MQI extended API (MQPUT1/MQINQ/MQSET/MQCMIT/MQBACK/MQBEGIN) | 6 additional calls | None | **Missing** |
| 3 | MQI pub/sub API (MQSUB/MQSUBRQ) | 2 calls | None | **Missing** |
| 4 | MQI message properties (MQCRTMH/MQDLTMH/MQSETMP/MQINQMP/MQDLTMP) | 5 calls | None | **Missing** |
| 5 | MQI async callback (MQCB/MQCTL/MQSTAT) | 3 calls | None | **Missing** |
| 6 | MQMD (Message Descriptor) | 28+ fields, 2 versions | None | **Missing** |
| 7 | MQOD (Object Descriptor) | 15+ fields | None | **Missing** |
| 8 | MQGMO (Get Message Options) | 20+ option flags | None | **Missing** |
| 9 | MQPMO (Put Message Options) | 15+ option flags | None | **Missing** |
| 10 | Queue types (local/remote/alias/model/dynamic) | 5 core types | None | **Missing** |
| 11 | Transmission queues and remote routing | XMITQ + channel resolution | None | **Missing** |
| 12 | Dead-letter queue and MQDLH header | DLQ + DLH structure | None | **Missing** |
| 13 | Triggering (FIRST/EVERY/DEPTH) | 3 trigger types + monitor | None | **Missing** |
| 14 | Channel types (SDR/RCVR/SVR/RQSTR/SVRCONN/CLNTCONN/CLUSSDR/CLUSRCVR/AMQP) | 9 types | None | **Missing** |
| 15 | Channel communication protocols (TCP, LU 6.2) | 2+ protocols | None | **Missing** |
| 16 | Channel exits (security/message/send/receive) | 4+ exit types | None | **Missing** |
| 17 | SSL/TLS channel encryption | CipherSpecs + certificates | None | **Missing** |
| 18 | Channel authentication records (CHLAUTH) | 5 rule types | None | **Missing** |
| 19 | Connection authentication (CONNAUTH) | IDPWOS/IDPWLDAP | None | **Missing** |
| 20 | RACF security integration | 7+ resource classes | None | **Missing** |
| 21 | Publish/subscribe (topics, subscriptions, retained) | Full pub/sub engine | None | **Missing** |
| 22 | Selection strings (SQL92-like filters) | Message property selectors | None | **Missing** |
| 23 | Clustering (full/partial repos, workload balancing) | Multi-QMgr clusters | None | **Missing** |
| 24 | Queue sharing groups (z/OS) | CF structures + shared queues | None | **Missing** |
| 25 | Coupling facility integration | CF list structures + SMDS | None | **Missing** |
| 26 | Intra-group queuing (IGQ) | Fast QSG-internal transfer | None | **Missing** |
| 27 | MQSC command engine | 100+ commands | None | **Missing** |
| 28 | PCF command processing | Programmatic admin | None | **Missing** |
| 29 | CSQUTIL/CSQUDLQH batch utilities | z/OS batch admin | None | **Missing** |
| 30 | Transaction coordination (syncpoint, RRS, 2PC) | MQCMIT/MQBACK/MQBEGIN | None | **Missing** |
| 31 | MQ-CICS bridge | Route messages to CICS transactions | None | **Missing** |
| 32 | MQ-IMS bridge (OTMA) | Route messages to IMS transactions | None | **Missing** |
| 33 | Message grouping and segmentation | GroupId/MsgSeqNumber/Offset | None | **Missing** |
| 34 | Data conversion (MQGMO_CONVERT) | Cross-platform encoding conversion | None | **Missing** |
| 35 | Advanced Message Security (AMS) | Sign/encrypt per policy | None | **Missing** |

**Total: 35 major gaps, 0 partial implementations, 0 present**

## Proposed Epic Structure

### MQ100 — MQ Core Runtime & Queue Manager (XL)
- Queue manager process model (connection handling, object management)
- Local queue implementation (in-memory + persistent storage)
- Queue attributes (MAXDEPTH, MAXMSGL, GET/PUT enabled, CURDEPTH tracking)
- Connection handle management (MQCONN/MQCONNX/MQDISC)
- Object handle management (MQOPEN/MQCLOSE)
- Queue name resolution (local, alias → base queue)
- Dynamic queue creation from model queues
- Message priority ordering
- Message persistence (in-memory vs disk-backed)
- **Depends on**: open-mainframe-runtime, open-mainframe-encoding

### MQ101 — MQI Put/Get Operations (L)
- MQPUT — put message with MQPMO options
- MQPUT1 — open-put-close convenience
- MQGET — get message with MQGMO options (wait/no-wait, browse, match)
- MQINQ — inquire queue/QMgr attributes
- MQSET — set queue attributes dynamically
- Message truncation and buffer management
- Browse cursor (BROWSE_FIRST/BROWSE_NEXT/MSG_UNDER_CURSOR)
- Message locking (MQGMO_LOCK/UNLOCK)
- Signal mechanism (z/OS-specific MQGMO_SET_SIGNAL)
- **Depends on**: MQ100

### MQ102 — MQMD & Data Structures (L)
- MQMD v1 and v2 implementation (all 28+ fields)
- MQOD (Object Descriptor) with name resolution
- MQGMO (Get Message Options) with all option flags
- MQPMO (Put Message Options) with all option flags
- MQDLH (Dead Letter Header)
- MQTM/MQTMC2 (Trigger Message)
- MQXQH (Transmission Queue Header)
- MQRFH2 (Rules and Formatting Header v2)
- Message ID generation (MQMD_NEW_MSG_ID)
- Correlation ID handling
- Report options (COA/COD/Exception/Expiry)
- **Depends on**: MQ100, open-mainframe-encoding

### MQ103 — Remote Queuing & Channels (XL)
- Transmission queue management
- Remote queue definitions (QREMOTE → XMITQ routing)
- Channel engine (sender/receiver MCA logic)
- TCP/IP transport layer
- Message sequence numbering and gap detection
- Channel heartbeating
- Batch size and message compression
- Channel auto-start via triggering
- Retry logic (short/long retry counts and intervals)
- Channel status tracking (BINDING/STARTING/RUNNING/STOPPING/RETRYING)
- **Depends on**: MQ100, MQ101, MQ102

### MQ104 — Syncpoint & Transaction Coordination (L)
- Local unit of work (MQCMIT/MQBACK)
- Persistent message recovery (write-ahead logging)
- MQBEGIN for global transactions
- Syncpoint participation with CICS UOW
- Syncpoint participation with IMS sync points
- In-doubt thread tracking and resolution
- Backout count and backout threshold handling
- **Depends on**: MQ100, MQ101, open-mainframe-cics (optional)

### MQ105 — Publish/Subscribe Engine (L)
- Topic tree management (hierarchical topic namespace)
- Topic object administration (DEFINE TOPIC)
- MQSUB — create/resume subscriptions (durable/non-durable, managed/unmanaged)
- MQSUBRQ — request retained publications
- Publication distribution to matching subscriptions
- Retained publication storage
- Selection string parsing (SQL92-like WHERE clauses)
- Wildcard topics (# and + characters)
- TREELIFE management for dynamic topic cleanup
- **Depends on**: MQ100, MQ101, MQ102

### MQ106 — Clustering (L)
- Full and partial repository queue managers
- Cluster-sender (CLUSSDR) and cluster-receiver (CLUSRCVR) channels
- Auto-defined cluster-sender channels
- Cluster workload distribution (round-robin default)
- Cluster workload exit interface
- REFRESH CLUSTER / RESET CLUSTER operations
- SUSPEND/RESUME QMGR for cluster maintenance
- Overlapping cluster support
- **Depends on**: MQ100, MQ103

### MQ107 — Security (L)
- RACF integration for MQ resource classes (MQCONN, MQADMIN, MQQUEUE, etc.)
- Connection authentication (CONNAUTH — OS/LDAP user verification)
- Channel authentication records (CHLAUTH — IP/user/DN/QMgr rules)
- SSL/TLS channel encryption (CipherSpecs, certificate handling)
- Alternate user authority (MQOO_ALTERNATE_USER_AUTHORITY)
- Context authority (PASSALL/PASSID/SETALL/SETID)
- Advanced Message Security (sign/encrypt policies)
- **Depends on**: MQ100, MQ103, RACF subsystem (Batch 8)

### MQ108 — MQSC Command Engine (L)
- MQSC command parser (DEFINE/ALTER/DISPLAY/DELETE/START/STOP/etc.)
- Command server (SYSTEM.COMMAND.INPUT queue processing)
- All object type support (queues, channels, topics, listeners, etc.)
- PING CHANNEL / PING QMGR
- CLEAR QLOCAL / CLEAR TOPICSTR
- RESET/RESOLVE/REFRESH commands
- SET CHLAUTH / SET AUTHREC / SET POLICY
- z/OS-specific: DISPLAY USAGE, DISPLAY THREAD, ARCHIVE LOG
- Command response formatting
- **Depends on**: MQ100, MQ103, MQ105

### MQ109 — PCF & Programmatic Administration (M)
- PCF message structure (MQCFH header + parameter structures)
- PCF command processing (MQCMD_* constants)
- PCF response generation
- Escape PCF (MQSC wrapped in PCF)
- MQAI (MQ Administration Interface) data bag abstraction
- Remote administration via PCF over channels
- **Depends on**: MQ100, MQ108

### MQ110 — z/OS-Specific Features (XL)
- Queue sharing groups (QSG) model
- Shared queue support (coupling facility abstraction)
- CF structure management (CSQ_ADMIN + application structures)
- SMDS (Shared Message Data Sets) offloading
- Intra-group queuing (IGQ) for fast QSG-internal transfer
- Group listener for client connections
- Page sets and buffer pools (storage management)
- Storage classes (queue-to-page-set mapping)
- Active/archive logging with BSDS
- CSQUTIL batch utility functions (COMMAND, COPY, LOAD, EMPTY)
- CSQUDLQH dead-letter handler
- Channel initiator management
- Peer channel recovery
- **Depends on**: MQ100, MQ103, MQ104

### MQ111 — Triggering & Dead-Letter Handling (M)
- Trigger types (FIRST/EVERY/DEPTH/NONE)
- Trigger monitor implementation (CKTI/CSQQTRMN equivalents)
- Process definition objects
- Initiation queue and trigger message (MQTM) generation
- Dead-letter queue routing logic
- MQDLH header construction/parsing
- DLQ handler rules table processing
- Configurable retry/forward/discard actions
- **Depends on**: MQ100, MQ101, MQ102

### MQ112 — MQ Bridges (L)
- MQ-CICS bridge (route messages to CICS transactions via DPL)
- MQ-CICS 3270 bridge (screen-based transaction invocation)
- MQ-IMS bridge via OTMA interface
- Bridge monitoring and error handling
- Message format conversion between MQ and CICS/IMS conventions
- **Depends on**: MQ100, MQ101, open-mainframe-cics, open-mainframe-ims

## Dependencies

### Existing Crate Dependencies
| Crate | Relationship |
|-------|-------------|
| open-mainframe-runtime | Decimal arithmetic, string operations for message processing |
| open-mainframe-encoding | EBCDIC/CCSID conversion for MQMD.CodedCharSetId, data conversion on MQGET |
| open-mainframe-cics | MQ-CICS bridge integration, CICS transaction trigger targets |
| open-mainframe-ims | MQ-IMS bridge via OTMA, IMS transaction trigger targets |
| open-mainframe-dataset | Potential persistent queue storage using sequential/VSAM files |

### Cross-Batch Dependencies
| Batch | Dependency |
|-------|------------|
| Batch 8 (RACF) | MQCONN/MQADMIN/MQQUEUE/MQPROC RACF classes for MQ security |
| Batch 11 (JES2) | JES2 job submission via MQ trigger monitors |
| Batch 12 (LE) | Language Environment for COBOL/PL/I MQ application runtime |
| Batch 14 (SMF) | SMF Type 115/116 records for MQ accounting/statistics |

### New Crate Recommendation
- **open-mainframe-mq** — New crate for all IBM MQ functionality
- Consider sub-modules: `mqi`, `mqsc`, `channels`, `pubsub`, `clustering`, `security`, `zos`

## Complexity Estimate

| Epic | Size | Rationale |
|------|------|-----------|
| MQ100 — Core Runtime & Queue Manager | XL | Full queue manager with connection/object/queue management, persistence |
| MQ101 — Put/Get Operations | L | Complex options processing for MQPUT/MQGET with browse, lock, wait |
| MQ102 — MQMD & Data Structures | L | Many structures with dozens of fields and option flags each |
| MQ103 — Remote Queuing & Channels | XL | Full channel engine with TCP/IP transport, retries, batching |
| MQ104 — Syncpoint & Transactions | L | Write-ahead log, unit of work, 2PC coordination |
| MQ105 — Publish/Subscribe | L | Topic tree, subscription management, retained publications, selectors |
| MQ106 — Clustering | L | Distributed cluster state, workload balancing, auto-defined channels |
| MQ107 — Security | L | RACF integration, CHLAUTH, CONNAUTH, SSL/TLS, AMS |
| MQ108 — MQSC Command Engine | L | 100+ commands with full parsing and execution |
| MQ109 — PCF Administration | M | PCF message format + command mapping (builds on MQ108) |
| MQ110 — z/OS-Specific Features | XL | QSG, shared queues, CF, page sets, logging — highly complex |
| MQ111 — Triggering & DLQ | M | Well-defined patterns for trigger monitors and DLQ rules |
| MQ112 — MQ Bridges | L | CICS/IMS bridge protocols with message format conversion |

**Overall complexity: XXL** — IBM MQ is a complete middleware subsystem with its own storage, networking, administration, security, and transaction infrastructure. The 13 proposed epics span the full range from core messaging to z/OS-specific coupling facility integration.

## Reference Documentation

- [IBM MQ 9.4 Product Documentation](https://www.ibm.com/docs/en/ibm-mq/9.4)
- [IBM MQ MQI Application Programming Reference](https://www.ibm.com/docs/en/ibm-mq/9.4?topic=reference-mqi)
- [MQMD — Message Descriptor (IBM MQ 9.2)](https://www.ibm.com/docs/SSFKSJ_9.2.0/com.ibm.mq.ref.dev.doc/q097390_.htm)
- [MQSC Commands Reference (IBM MQ 9.2)](https://www.ibm.com/docs/en/ibm-mq/9.2.x?topic=reference-mqsc-commands)
- [Automating Administration Using PCF Commands (IBM MQ 9.4)](https://www.ibm.com/docs/en/ibm-mq/9.4.x?topic=administering-automating-mq-administration-using-pcf-commands)
- [Queue Sharing Groups and Shared Queues (IBM MQ 9.1)](https://www.ibm.com/docs/SSFKSJ_9.1.0/com.ibm.mq.pro.doc/q003630_.htm)
- [Managing QSGs and Shared Queues (IBM MQ 9.2)](https://www.ibm.com/docs/en/ibm-mq/9.2.x?topic=zos-managing-queue-sharing-groups-shared-queues)
- [Channel Types (IBM MQ 9.2)](https://www.ibm.com/docs/SSFKSJ_9.2.0/com.ibm.mq.explorer.doc/e_channels.htm)
- [Cluster Channels (IBM MQ 9.2)](https://www.ibm.com/docs/en/ibm-mq/9.2.0?topic=components-cluster-channels)
- [Triggering — Controlling Trigger Events (IBM MQ 9.2)](https://www.ibm.com/docs/en/ibm-mq/9.2.x?topic=triggers-controlling-trigger-events)
- [CSQUTIL Utility Program (IBM MQ 9.2)](https://www.ibm.com/docs/en/ibm-mq/9.2?topic=zos-mq-utility-program-csqutil)
- [CSQUDLQH Dead-Letter Queue Handler (IBM MQ 9.2)](https://www.ibm.com/docs/en/ibm-mq/9.2.x?topic=zos-dead-letter-queue-handler-utility-csqudlqh)
- [Connection Authentication Configuration (IBM MQ 9.1)](https://www.ibm.com/docs/SSFKSJ_9.1.0/com.ibm.mq.sec.doc/q113250_.htm)
- [CHLAUTH and CONNAUTH Interaction (IBM MQ 9.2)](https://www.ibm.com/docs/en/ibm-mq/9.2.x?topic=records-interaction-chlauth-connauth)
- [z/OS Authority Checks (IBM MQ 9.2)](https://www.ibm.com/docs/en/ibm-mq/9.2.x?topic=zos-authority-checks)
- [MQ Publish/Subscribe — Getting the Most Out of It (MQ Tech Conference)](https://www.mqtechconference.com/sessions_v2017/MQTC_2017_MQ_Pub-Sub_Getting_the_Most.pdf)
- [IBM MQ Wikipedia](https://en.wikipedia.org/wiki/IBM_MQ)
- [IBM MQ Go Library — MQMD Structure](https://github.com/ibm-messaging/mq-golang/blob/master/ibmmq/mqiMQMD.go)
