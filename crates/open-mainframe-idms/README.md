# open-mainframe-idms

z/OS IDMS (Integrated Database Management System) — a robust implementation of the CODASYL network database model, transactional DC environment, and SQL accessibility for the OpenMainframe project.

## Overview

IDMS is a cornerstone database system on IBM mainframes, known for its high performance and flexible network data model. This crate reimplements the core IDMS engines, including the navigational DML (Data Manipulation Language) for set-based traversal, the IDMS-DC transaction manager for high-volume online processing, and the COBOL DML precompiler for application integration.

## Architecture

```
    Application Source                    Database Runtime
    ┌──────────────┐                      ┌────────────────────┐
    │ EXEC IDMS    │    Preprocessing     │    DML Engine      │
    │ OBTAIN NEXT  │ ──────────────────>  │    (Navigational)  │
    │ END-EXEC     │    DmlPrecompiler    │  Currency, Sets    │
    └──────────────┘                      └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Metadata          ┌────────────────────┐
    │ Schema /     │ ──────────────────>  │   Storage Manager  │
    │ Subschema    │    SchemaParser      │    (DMCL)          │
    └──────────────┘                      │ Pages, CALC, VIA   │
                                          └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Transaction       ┌────────────────────┐
    │ IDMS-DC Task │ <──────────────────  │    Lock Manager    │
    │ Queues, Maps │    TaskScheduler     │ Deadlock Detection │
    └──────────────┘                      └────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `codasyl` | Definition of the network data model: Areas, Records, and Set types |
| `schema` | Parser for Schema and Subschema DDL (Data Description Language) |
| `dml` | Navigational DML engine: FIND, GET, STORE, MODIFY, ERASE, CONNECT |
| `currency` | Management of currency indicators (Current of Record, Set, Area, Run-Unit) |
| `dc` | IDMS-DC runtime: Task scheduling, pseudo-converse, maps, and queues |
| `precompiler`| COBOL source-to-source transformation for `EXEC IDMS` statements |
| `storage` | DMCL (Device Media Control Language) and physical page management |
| `lock` | Lock manager supporting record and area level locking with deadlock detection |
| `sql_option` | SQL interface providing relational access to CODASYL data |
| `recovery` | Transaction logging, journaling, and warm/cold start orchestration |
| `ads` | ADS/Online 4GL runtime for dialog-driven application development |

## Key Types and Components

### CODASYL Model
- `CodasylSchema`: The root metadata object containing the entire database structure.
- `RecordType`: Defines the fields and keys (CALC, DIRECT, or VIA) for a database record.
- `SetType`: Defines the relationship between Owner and Member records (Chain or Pointer Array).

### DML Engine
- `DmlEngine`: Executes navigational commands and maintains the run-unit context.
- `CurrencyTable`: Tracks the current record for every entity in the subschema.
- `FindMode`: Enumerates navigation types: CALC, ANY, DUPLICATE, NEXT, PRIOR, FIRST, LAST, OWNER, WITHIN SET/AREA.

### IDMS-DC Runtime
- `DcRuntime`: Manages the online execution environment.
- `TaskScheduler`: Handles task priorities and asynchronous execution.
- `MapSupport`: Renders and processes IDMS-DC maps for terminal I/O.

### Storage & DMCL
- `PageManager`: Handles buffering and physical I/O to database files.
- `CalcRoutine`: Implements the hashing algorithm for CALC-key record placement.

## Feature Coverage

| Feature | Category | Status |
|---------|----------|--------|
| Navigational DML| Database | Implemented (FIND, OBTAIN, STORE, etc.) |
| Set Processing | Database | Implemented (NEXT, PRIOR, OWNER, VIA) |
| Schema DDL | Metadata | Implemented |
| Subschema DDL | Metadata | Implemented |
| Precompiler | Tools | Implemented |
| Task Scheduling | DC | Implemented |
| Deadlock Detect | System | Implemented |
| SQL Interface | SQL | Implemented |

## Usage Examples

### Navigational DML in COBOL (Precompiled)

```cobol
* Original source
EXEC IDMS OBTAIN NEXT CUSTOMER WITHIN TERRITORY-CUST-SET
     ON-ERROR GO TO ERROR-PARA.
END-EXEC.

* Transformed call
CALL 'IDMS' USING IDMS-BNDB-BLOCK CUSTOMER-RECORD.
```

### Using the DML Engine in Rust

```rust
use open_mainframe_idms::{DmlEngine, FindMode};

let mut engine = DmlEngine::new(subschema);
engine.bind_record("CUSTOMER", &mut buffer);

// Obtain the first customer in a set
let result = engine.find(FindMode::First, "TERRITORY-CUST-SET")?;
if result.is_ok() {
    engine.get("CUSTOMER")?;
    println!("Found customer: {}", buffer.name);
}
```

## Testing

The IDMS crate features a comprehensive test suite:
- **Codasyl**: Validates set membership rules and owner/member relationships.
- **Schema**: Tests parsing of complex schema definitions with multiple areas and sets.
- **DML**: Simulates database traversal including recursive set navigation.
- **Locking**: Tests concurrent access scenarios and deadlock detection logic.
- **DC**: Validates task scheduling and queue management.

```sh
cargo test -p open-mainframe-idms
```
