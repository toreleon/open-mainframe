# open-mainframe-mq

IBM MQ (Message Queuing) — a comprehensive Rust implementation of the mainframe's premier messaging middleware for the OpenMainframe project.

## Overview

IBM MQ is the industry standard for reliable, asynchronous messaging. This crate reimplements the core MQ components, including the Queue Manager, MQI (Message Queue Interface) operations, MQSC command engine, and advanced features like Publish/Subscribe and Channel authentication.

## Architecture

```
    Application Source                    MQ Runtime Environment
    ┌──────────────┐                      ┌────────────────────┐
    │ CALL 'MQPUT' │    MQI Interface     │   Queue Manager    │
    │ Descriptor,  │ ──────────────────>  │    (Local)         │
    │ Payload      │    MqiHandle         │  Queues, MsgStore  │
    └──────────────┘                      └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Management        ┌────────────────────┐
    │  MQSC Cmds   │ ──────────────────>  │    Channel Mgr     │
    │  DEFINE Q    │    MqscEngine        │    SDR / RCVR      │
    └──────────────┘                      │    TLS, CHLAUTH    │
                                          └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Pub/Sub           ┌────────────────────┐
    │  Topics      │ <──────────────────  │   Trigger Monitor  │
    │  Subscribers │    PubSubEngine      │   Process Defs     │
    └──────────────┘                      └────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `mqi` | MQI implementation: MQCONN, MQDISC, MQOPEN, MQCLOSE, MQPUT, MQGET, etc. |
| `core` | Queue Manager core: Local queue lifecycle and message storage |
| `structures`| Binary-compatible structures: MQMD, MQOD, MQGMO, MQPMO, MQRFH2 |
| `mqsc` | MQSC Command Engine: DEFINE, ALTER, DELETE, DISPLAY, CLEAR for all MQ objects |
| `pubsub` | Publish/Subscribe engine: Topic tree, subscriptions, and selection strings |
| `channels` | Channel management: Sender/Receiver/SVRCONN, CHLAUTH rules, and SSL/TLS |
| `triggering`| Triggering mechanism: Trigger types (FIRST/EVERY/DEPTH) and process definitions |

## Key Types and Components

### MQI Handle
- `Connection`: Represents an active connection to a queue manager (MQCONN).
- `MqiHandle`: Trait providing all core MQI operations.
- `MqError`: Comprehensive error codes corresponding to MQ reason codes (e.g., 2033 NO_MSG_AVAILABLE).

### MQ Structures
- `Mqmd`: Message Descriptor — contains message ID, correlation ID, priority, and expiry.
- `Mqrfh2`: Rules and Formatting Header — for JMS-compatible message properties.
- `Mqod`: Object Descriptor — used for opening queues or topics.

### Queue Manager
- `QueueManager`: The central coordinator for all MQ resources.
- `Queue`: Represents an individual queue with its attributes and message buffer.

## Feature Coverage

| Feature | Category | Status |
|---------|----------|--------|
| MQI API | Messaging| Implemented (CONN, DISC, OPEN, CLOSE, PUT, GET) |
| MQSC Engine | Mgmt     | Implemented (All queue types, channels, topics) |
| Pub/Sub | Messaging| Implemented (Topic tree, durable subs) |
| Triggering | System   | Implemented (Trigger monitors, process defs) |
| Dead-Letter| System   | Implemented (DLH header, DLQ handler) |
| CHLAUTH | Security | Implemented (Block user, map client, SSL/TLS) |

## Usage Examples

### Putting a Message to a Queue

```rust
use open_mainframe_mq::{QueueManager, Mqmd, MqPmo};

let mut qm = QueueManager::new("QM1");
qm.define_local_queue("MY.QUEUE").unwrap();

let mut mqmd = Mqmd::new();
let mut pmo = MqPmo::new();
let payload = b"Hello MQ!";

qm.put("MY.QUEUE", &mut mqmd, &mut pmo, payload).unwrap();
println!("Message put successfully with MsgId: {:?}", mqmd.msg_id);
```

### Executing MQSC Commands

```rust
use open_mainframe_mq::mqsc::MqscEngine;

let mut engine = MqscEngine::new(&mut qm);
let result = engine.execute("DEFINE QLOCAL(APP.DATA) REPLACE").unwrap();
println!("MQSC Result: {}", result.output);
```

## Testing

The MQ crate features 350+ tests:
- **MQI**: Unit tests for every MQI call with complex option flags.
- **MQSC**: Parser tests for all MQSC statement variants and attribute types.
- **Pub/Sub**: Topic matching and wildcard subscription tests.
- **Channels**: CHLAUTH mapping and security rule validation.

```sh
cargo test -p open-mainframe-mq
```
