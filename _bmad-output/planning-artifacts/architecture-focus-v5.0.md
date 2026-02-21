---
version: 'v5.0'
planningGroup: 'PG-30'
technology: 'FOCUS'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-focus-v5.0.md'
---

# Architecture: FOCUS

## 1. Crate Strategy

**New crate:** `open-mainframe-focus`

## 2. Module Layout

```
crates/open-mainframe-focus/src/
├── lib.rs              # Crate root
├── parser/
│   ├── mod.rs          # Multi-dialect parser
│   ├── table.rs        # TABLE request parser
│   ├── graph.rs        # GRAPH request parser
│   ├── modify.rs       # MODIFY/MAINTAIN parser
│   ├── dialogue.rs     # Dialogue Manager parser
│   └── sql.rs          # SQL dialect parser
├── metadata/
│   ├── mod.rs          # MFD/Access File management
│   ├── mfd.rs          # Master File Descriptor
│   └── access.rs       # Access File (physical data source mapping)
├── engine/
│   ├── mod.rs          # Request engine dispatch
│   ├── table.rs        # TABLE execution (PRINT, SUM, BY, ACROSS)
│   ├── graph.rs        # GRAPH execution (BAR, PIE, LINE)
│   ├── modify.rs       # MODIFY transaction engine
│   └── maintain.rs     # MAINTAIN interactive engine
├── dialogue/
│   ├── mod.rs          # Dialogue Manager interpreter
│   ├── variables.rs    # Amper variables (local, global, system, statistical)
│   └── control.rs      # -IF, -SET, -RUN, -INCLUDE
├── functions.rs        # 150+ built-in functions
├── adapters/
│   ├── mod.rs          # Data adapter trait and registry
│   ├── focus_native.rs # .FOC file adapter
│   ├── sequential.rs   # Sequential file adapter
│   ├── vsam.rs         # VSAM adapter
│   ├── db2.rs          # DB2 SQL adapter
│   ├── ims.rs          # IMS/DLI adapter
│   └── adabas.rs       # ADABAS adapter
├── output/
│   ├── mod.rs          # Output dispatch
│   ├── text.rs         # Plain text report output
│   ├── html.rs         # HTML output with StyleSheet
│   ├── hold.rs         # HOLD file generation
│   └── stylesheet.rs   # StyleSheet processor
├── joins.rs            # JOIN, COMBINE, MATCH FILE
├── filedef.rs          # FILEDEF, DYNAM ALLOCATE
└── error.rs            # Error types
```

## 3. Design Decisions

### DD-5.0-FOC-01: Multi-Dialect Parser
**Decision:** FOCUS's TABLE, GRAPH, MODIFY, and Dialogue Manager dialects are parsed by separate sub-parsers dispatched by the top-level parser based on the initial keyword. SQL requests are delegated to the SQL sub-parser.

### DD-5.0-FOC-02: Data Adapter Trait
**Decision:** All data sources are accessed via a `DataAdapter` trait. Each adapter translates FOCUS data operations (read, write, find) into source-specific operations. This enables adding new data sources without modifying the core engine.

### DD-5.0-FOC-03: MFD as Metadata Abstraction
**Decision:** The Master File Descriptor (MFD) provides a logical schema independent of physical data sources. The Access File maps the MFD to specific physical adapters. This separation enables the same TABLE request to run against different data sources by changing only the Access File.
