---
version: 'v5.0'
planningGroup: 'PG-27'
technology: 'IDMS'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-idms-v5.0.md'
---

# Architecture: IDMS

## 1. Crate Strategy

**New crate:** `open-mainframe-idms`

## 2. Module Layout

```
crates/open-mainframe-idms/src/
├── lib.rs              # Crate root
├── schema/
│   ├── mod.rs          # Schema/subschema management
│   ├── ddl_parser.rs   # Schema DDL parser
│   └── types.rs        # Record, Set, Area, Element types
├── dml/
│   ├── mod.rs          # DML engine dispatch
│   ├── obtain.rs       # OBTAIN/FIND/GET operations
│   ├── modify.rs       # STORE/MODIFY/ERASE
│   ├── sets.rs         # CONNECT/DISCONNECT set navigation
│   └── currency.rs     # Currency indicator system
├── storage/
│   ├── mod.rs          # Physical storage layer
│   ├── dmcl.rs         # DMCL configuration
│   ├── pages.rs        # Page-level I/O
│   ├── calc.rs         # CALC hash access
│   └── via.rs          # VIA clustered storage
├── precompile/
│   ├── mod.rs          # COBOL DML precompiler
│   └── transform.rs    # DML → CALL 'IDMS' transformation
├── dc/
│   ├── mod.rs          # IDMS-DC transaction processing
│   ├── maps.rs         # MAP/BIND MAP operations
│   └── task.rs         # Task dispatch and scratch/queue
├── sql/
│   └── mod.rs          # SQL option over network data
├── recovery/
│   ├── mod.rs          # Journal and recovery
│   └── dcmt.rs         # DCMT operator commands
└── lock.rs             # Record/area locking
```

## 3. Design Decisions

### DD-5.0-IDMS-01: Set Pointers as Graph Edges
**Decision:** CODASYL sets are implemented as directed graph edges stored alongside records. Each member record holds NEXT/PRIOR/OWNER pointer indices. This enables O(1) navigational DML without separate index structures.

### DD-5.0-IDMS-02: Currency as Runtime State
**Decision:** Currency indicators are maintained as runtime state per run-unit (connection). Four HashMap levels track current record, set, area, and run-unit currencies.

### DD-5.0-IDMS-03: Pages as File-Backed Storage
**Decision:** Areas map to files via DMCL. Records are stored in fixed-size pages within area files, supporting CALC (hash to page) and VIA (store near owner) access patterns.
