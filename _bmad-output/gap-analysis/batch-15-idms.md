# Gap Analysis: IDMS (Integrated Database Management System)

## Official Specification Summary

IDMS (Integrated Database Management System) is a network/CODASYL database management system originally developed by Cullinane/Cullinet, later owned by Computer Associates (CA Technologies), and now by Broadcom. It runs on IBM z/OS mainframes and implements the CODASYL DBTG (Data Base Task Group) standard — a network data model where records are linked through owner-member sets forming a directed graph, as opposed to the hierarchical model (IMS) or relational model (DB2).

IDMS is classified as **Common** (widely deployed in enterprises) on mainframes:
- Still in active production at thousands of sites worldwide, particularly in insurance, finance, and government
- Two access modes: **Central Version** (online, shared multi-user) and **Local Mode** (batch, single-user)
- **IDMS-DC**: Built-in online transaction processing (TP) monitor, similar in function to CICS
- Navigational DML with ~15 verbs: OBTAIN, STORE, MODIFY, ERASE, CONNECT, DISCONNECT, FIND, GET, etc.
- Currency indicators automatically track database position (current of run-unit, record type, set, area)
- Schema/subschema architecture: full schema defines database; subschemas define application views
- **ADS/Online**: Built-in 4GL for rapid application development using dialogs, maps, and processes
- **SQL Option**: Provides SQL access to IDMS databases alongside navigational DML
- **LRF (Logical Record Facility)**: Abstracts complex navigation into logical record paths
- **OLQ (Online Query)**: Ad-hoc query facility for interactive data retrieval
- Records stored in AREAs (files) organized by page groups; records located by CALC (hash), VIA (cluster), DIRECT, or sequential access
- Database keys (db-keys) uniquely identify records via page:line addressing

Key documentation:
- **CA IDMS Navigational DML Programming Reference** — DML verbs, programming model
- **CA IDMS DML Reference for COBOL** — COBOL-embedded DML syntax
- **CA IDMS DML Reference for PL/I** — PL/I-embedded DML syntax
- **CA IDMS Database Administration** — schema DDL, DMCL, areas, page groups
- **CA IDMS ADS Application Design Reference** — ADS dialogs, maps, processes
- **CA IDMS SQL Reference** — SQL option DML/DDL
- **CA IDMS System Operations** — Central Version operations, startup/shutdown

## Key Features & Capabilities

### 1. CODASYL Data Model

| Concept | Description |
|---------|-------------|
| Record | Named collection of data elements (analogous to a row/entity) |
| Set | One-to-many relationship: one OWNER record, many MEMBER records (directed linked list) |
| Area | Physical storage region; contains pages; records stored within areas |
| Schema | Complete database definition: all records, sets, areas |
| Subschema | Application-specific view: subset of records, sets, fields |
| Database Key (db-key) | Unique record address = page number + line index |
| CALC | Hashing-based direct access by key value |
| VIA | Cluster members near their owner in the same area |
| DIRECT | Access by stored db-key |
| Currency | Automatic position tracking within the database graph |

### 2. Navigational DML Verbs

| Verb | Purpose |
|------|---------|
| OBTAIN | Locate record and transfer data to program (FIND + GET combined) |
| FIND | Locate record and set currency without data transfer |
| GET | Transfer data from current record to program variable storage |
| STORE | Insert a new record into the database |
| MODIFY | Update current record from program variable storage |
| ERASE | Delete current record (and optionally members) |
| CONNECT | Add current record as member of a set occurrence |
| DISCONNECT | Remove current record from a set occurrence |
| READY | Open an area for processing (usage mode: RETRIEVAL / UPDATE / EXCLUSIVE / PROTECTED) |
| FINISH | Close areas, commit or rollback changes |
| BIND | Establish run-unit, associate program with subschema |
| ACCEPT | Retrieve database information (db-key, statistics, run-unit info) |
| IF | Test set membership (IF member/owner of set) |
| RETURN | Return control to IDMS-DC |
| COMMIT | Commit transaction (alternative to FINISH) |
| ROLLBACK | Rollback transaction |

#### OBTAIN/FIND Variations

| Form | Description |
|------|-------------|
| OBTAIN CALC | Access record by CALC key (hash lookup) |
| OBTAIN WITHIN SET | Navigate to first/last/next/prior member in a set |
| OBTAIN OWNER WITHIN SET | Navigate to owner of current set occurrence |
| OBTAIN CURRENT | Retrieve the current record (of record type, set, or area) |
| OBTAIN DBKEY | Access record by stored database key |
| OBTAIN WITHIN AREA | Sequential scan through an area |
| OBTAIN USING SORT KEY | Locate member by sorted key in a sorted set |

### 3. Schema DDL

#### RECORD Definition

| Clause | Description |
|--------|-------------|
| RECORD NAME IS | Record type name (1–16 characters) |
| RECORD ID IS | Numeric identifier |
| LOCATION MODE IS CALC | Hash access using specified field(s) |
| LOCATION MODE IS VIA SET | Cluster near owner in named set |
| LOCATION MODE IS DIRECT | Direct access by db-key |
| WITHIN AREA | Which area stores this record type |
| DUPLICATES ARE [NOT ALLOWED / FIRST / LAST] | Duplicate handling for CALC keys |

#### SET Definition

| Clause | Description |
|--------|-------------|
| SET NAME IS | Set name (1–16 characters) |
| OWNER IS | Owner record type |
| MEMBER IS | Member record type |
| ORDER IS [SORTED / FIRST / LAST / NEXT / PRIOR] | Insertion order |
| MODE IS CHAIN [LINKED TO PRIOR] | Chain (linked list) implementation |
| MODE IS INDEX | B-tree index implementation |
| MANDATORY AUTOMATIC | Must belong; auto-connected on STORE |
| MANDATORY MANUAL | Must belong; explicit CONNECT required |
| OPTIONAL AUTOMATIC | May belong; auto-connected on STORE |
| OPTIONAL MANUAL | May belong; explicit CONNECT required |
| ASCENDING/DESCENDING KEY IS | Sort key for sorted sets |
| DUPLICATES ARE [NOT ALLOWED / FIRST / LAST] | Duplicate handling in sorted sets |

#### AREA Definition

| Clause | Description |
|--------|-------------|
| AREA NAME IS | Area name (1–16 characters) |
| RANGE IS page1 THRU page2 | Page range within the area |
| WITHIN FILE | Physical file mapping |
| PAGE GROUP | Grouping of pages for storage management |

### 4. Currency Indicators

| Currency Type | Description |
|---------------|-------------|
| Current of Run-Unit | Last record accessed by any DML verb |
| Current of Record Type | Last record accessed of each record type |
| Current of Set | Last record accessed within each set |
| Current of Area | Last record accessed within each area |

Currency is automatically maintained by IDMS after each DML verb. Programs navigate the database graph by moving between currency positions.

### 5. IDMS-DC (Data Communications)

| Feature | Description |
|---------|-------------|
| Task management | Concurrent task execution, priority dispatching |
| Screen management | MAP definitions, BIND MAP, MAP IN, MAP OUT |
| Program management | LINK, XCTL, DC RETURN, TRANSFER CONTROL |
| Storage management | GET STORAGE, FREE STORAGE, variable storage |
| Scratch management | Scratch records for inter-task data passing |
| Queue management | Queue records for ordered data |
| Time management | ACCEPT TIME, WAIT, POST |
| Terminal management | Physical/logical terminal definitions |
| Print management | Printer I/O, report output |
| Statistics | BIND TRANSACTION STATISTICS, ACCEPT STATISTICS |
| Abend handling | ON clause for error conditions |

#### DML Statements for IDMS-DC

| Statement | Description |
|-----------|-------------|
| BIND MAP | Associate map with program storage |
| MAP IN | Receive screen input into map record |
| MAP OUT | Send map record to screen |
| DC RETURN | Return control within task hierarchy |
| DC RETURN NEXT TASK CODE | Specify next transaction |
| TRANSFER CONTROL | Transfer to another program/system |
| LINK | Call subroutine, expect return |
| XCTL | Transfer control, no return expected |
| ATTACH | Initiate a new task |
| CHANGE PRIORITY | Modify dispatching priority |
| SNAP | Write diagnostic snapshot |
| WRITE TO LOG | Write to system log |
| ABEND | Force abnormal termination |
| ACCEPT TASK CODE | Get current task code |
| ACCEPT TERMINAL ID | Get terminal identifier |
| ACCEPT USER ID | Get authenticated user |

### 6. ADS/Online (Application Development System)

| Component | Description |
|-----------|-------------|
| Dialog | Unit of application logic: map + processes + records |
| Premap Process | Logic executed before map display |
| Response Process | Logic executed in response to user input |
| Default Response | Handles unspecified user actions |
| Declaration Module | Declares cursors and WHENEVER for SQL |
| Map | Screen definition with field layout |
| ADSC (Dialog Compiler) | Interactive tool for building/compiling dialogs |
| ADS Batch | Batch execution of ADS dialogs |
| ADS/Online runtime | Executes compiled dialogs in IDMS-DC |

### 7. SQL Option for IDMS

| Feature | Description |
|---------|-------------|
| SQL DML | SELECT, INSERT, UPDATE, DELETE on IDMS databases |
| SQL DDL | CREATE TABLE, CREATE INDEX, ALTER, DROP |
| Network access via SQL | SQL queries can navigate network-defined records |
| Stored procedures | Server-side SQL procedures |
| JDBC | Java connectivity to IDMS databases |
| SQL-defined tables | Tables defined with SQL DDL (non-network) |
| Catalog tables | SYSTEM.* catalog for metadata queries |
| Cursors | DECLARE CURSOR, FETCH, CLOSE for multi-row access |

### 8. Additional Facilities

| Facility | Description |
|----------|-------------|
| LRF (Logical Record Facility) | Abstract navigation into logical record paths with WHERE clauses |
| OLQ (Online Query) | Interactive query language for ad-hoc data retrieval |
| Culprit | Report writer facility |
| ASF (Automatic System Facility) | Table-based data access for non-programmers |
| IDD (Integrated Data Dictionary) | Central metadata repository |
| DMCL (Device Media Control Language) | Physical storage definitions, buffer pools, journal assignments |
| Journals | Write-ahead logging for recovery; before/after images |
| Warmstart/Coldstart | Recovery mechanisms |

### 9. DMCL & Physical Storage

| Concept | Description |
|---------|-------------|
| DMCL | Maps logical areas to physical files and buffers |
| Buffer pools | Configurable memory buffers for page I/O |
| Journal files | Transaction logging for recovery |
| Before images | Pre-update record state for rollback |
| After images | Post-update record state for roll-forward |
| Warmstart | Recover from journal after abnormal end |
| Coldstart | Initialize system, discard pending work |
| DCMT commands | IDMS system operator commands |

## Current OpenMainframe Status

The `open-mainframe` codebase has **no IDMS implementation whatsoever**. The CODASYL network data model, navigational DML, set-based relationships, and currency indicators are entirely absent.

### Not Applicable — No Existing Code

There are no Rust crates, modules, or partial implementations for IDMS. The search found:

1. **Zero Rust source files** with IDMS-specific code
2. **Zero CODASYL/network model** structures
3. **Zero navigational DML** verb implementations
4. **Zero currency indicator** tracking
5. **Zero set membership** management
6. **Zero IDMS-DC** transaction processing code

### Documentation References Only

IDMS is mentioned in:
- `RALPH-PROMPT.md` (line 101): Batch 15 planning
- `batch-04-easytrieve.md`: Easytrieve's IDMS file access support (10+ IDMS statements)
- `batch-07-focus.md`: FOCUS IDMS adapter reference

### Related But Different Implementations

| Existing Crate | Relationship to IDMS |
|----------------|---------------------|
| open-mainframe-db2 | Relational model (SQL); fundamentally different from CODASYL network model |
| open-mainframe-ims | Hierarchical model (DL/I); different data model but similar "navigational" concept |
| open-mainframe-dataset | File-level I/O (VSAM/QSAM); IDMS uses its own storage management |
| open-mainframe-cics | TP monitor; IDMS-DC is a competing TP monitor with different API |

## Gap Details

### CODASYL Data Model

| Feature | Official IDMS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Record type definitions | Named records with elements | Not implemented | Missing |
| Set definitions (owner/member) | One-to-many relationships | Not implemented | Missing |
| Area definitions | Physical storage regions with page ranges | Not implemented | Missing |
| Schema definitions | Complete database structure | Not implemented | Missing |
| Subschema definitions | Application-specific views | Not implemented | Missing |
| Database keys (db-key) | Page:line unique addressing | Not implemented | Missing |
| CALC access (hashing) | Direct access by key hash | Not implemented | Missing |
| VIA set access (clustering) | Members near owner | Not implemented | Missing |
| DIRECT access | Access by stored db-key | Not implemented | Missing |
| Chain mode (linked lists) | Pointer chains for set traversal | Not implemented | Missing |
| Index mode (B-tree) | Indexed set implementation | Not implemented | Missing |

### Navigational DML

| Feature | Official IDMS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| OBTAIN/FIND (CALC) | Hash-based record retrieval | Not implemented | Missing |
| OBTAIN/FIND (WITHIN SET) | Set traversal (first/last/next/prior) | Not implemented | Missing |
| OBTAIN/FIND (OWNER) | Navigate to set owner | Not implemented | Missing |
| OBTAIN/FIND (CURRENT) | Access current record | Not implemented | Missing |
| OBTAIN/FIND (DBKEY) | Direct access by db-key | Not implemented | Missing |
| OBTAIN/FIND (WITHIN AREA) | Area scan | Not implemented | Missing |
| OBTAIN/FIND (SORT KEY) | Sorted set lookup | Not implemented | Missing |
| GET | Transfer data from current record | Not implemented | Missing |
| STORE | Insert new record | Not implemented | Missing |
| MODIFY | Update current record | Not implemented | Missing |
| ERASE | Delete current record (+ optional members) | Not implemented | Missing |
| CONNECT | Add record to set | Not implemented | Missing |
| DISCONNECT | Remove record from set | Not implemented | Missing |
| READY | Open area with usage mode | Not implemented | Missing |
| FINISH | Close areas, commit | Not implemented | Missing |
| BIND RUN-UNIT | Establish connection to database | Not implemented | Missing |
| ACCEPT (db-key, statistics) | Retrieve database info | Not implemented | Missing |
| IF (set membership test) | Test if member/owner | Not implemented | Missing |
| COMMIT / ROLLBACK | Transaction control | Not implemented | Missing |

### Currency Indicators

| Feature | Official IDMS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Current of run-unit | Last record accessed globally | Not implemented | Missing |
| Current of record type | Last record of each type | Not implemented | Missing |
| Current of set | Last record in each set | Not implemented | Missing |
| Current of area | Last record in each area | Not implemented | Missing |
| Currency suppression | SUPPRESS CURRENCY options | Not implemented | Missing |

### IDMS-DC (Online Transaction Processing)

| Feature | Official IDMS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| MAP definitions | Screen layout with field mapping | Not implemented | Missing |
| BIND MAP / MAP IN / MAP OUT | Screen I/O operations | Not implemented | Missing |
| DC RETURN | Return control within task | Not implemented | Missing |
| LINK / XCTL | Program-to-program communication | Not implemented | Missing |
| TRANSFER CONTROL | Cross-system transfer | Not implemented | Missing |
| ATTACH (new task) | Task creation | Not implemented | Missing |
| Task dispatching | Priority-based scheduling | Not implemented | Missing |
| Scratch management | Temporary data records | Not implemented | Missing |
| Queue management | Ordered data queues | Not implemented | Missing |
| Terminal management | Physical/logical terminals | Not implemented | Missing |
| ACCEPT TASK CODE / TERMINAL ID | Runtime info retrieval | Not implemented | Missing |
| SNAP / WRITE TO LOG | Diagnostics | Not implemented | Missing |

### ADS/Online (4GL)

| Feature | Official IDMS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Dialog definitions | Map + processes + records | Not implemented | Missing |
| Premap/response processes | Event-driven process logic | Not implemented | Missing |
| ADSC (dialog compiler) | Interactive dialog builder | Not implemented | Missing |
| ADS Batch | Batch execution of dialogs | Not implemented | Missing |
| ADS runtime | Dialog execution engine | Not implemented | Missing |

### SQL Option

| Feature | Official IDMS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| SQL DML (SELECT/INSERT/UPDATE/DELETE) | SQL access to network databases | Not implemented | Missing |
| SQL DDL (CREATE TABLE/INDEX) | SQL-defined tables | Not implemented | Missing |
| SQL on network records | SQL queries over CODASYL data | Not implemented | Missing |
| Stored procedures | Server-side SQL logic | Not implemented | Missing |
| JDBC connectivity | Java access | Not implemented | Missing |
| Cursors | Multi-row retrieval | Not implemented | Missing |
| Catalog tables | SYSTEM.* metadata | Not implemented | Missing |

### Additional Facilities

| Feature | Official IDMS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| LRF (Logical Record Facility) | Path-based logical records | Not implemented | Missing |
| OLQ (Online Query) | Ad-hoc interactive queries | Not implemented | Missing |
| IDD (Integrated Data Dictionary) | Central metadata store | Not implemented | Missing |
| ASF (Automatic System Facility) | Table-based access for end users | Not implemented | Missing |
| DMCL | Physical storage configuration | Not implemented | Missing |
| Journal/recovery | Write-ahead log, warmstart | Not implemented | Missing |
| DCMT commands | Operator commands | Not implemented | Missing |
| DML precompiler (COBOL) | Embedded DML → COBOL CALL | Not implemented | Missing |
| DML precompiler (PL/I) | Embedded DML → PL/I CALL | Not implemented | Missing |

## Proposed Epic Structure

### IDMS100: CODASYL Data Model Core (L)
- Record type definitions with elements (PICTURE, LEVEL, REDEFINES, OCCURS)
- Set definitions (owner/member, ORDER IS, MODE IS CHAIN/INDEX)
- Set membership options (MANDATORY/OPTIONAL, AUTOMATIC/MANUAL)
- Area definitions (page ranges, file mapping)
- Database key (db-key) generation and management (page:line)
- CALC key hashing algorithm
- VIA set clustering
- In-memory storage engine for records, sets, areas
- **Depends on**: open-mainframe-encoding (EBCDIC, packed decimal)

### IDMS101: Schema & Subschema DDL Parser (M)
- Parse SCHEMA/RECORD/SET/AREA DDL statements
- Record element definitions (data types, PICTURE)
- Set clauses (ORDER, MODE, DUPLICATES, membership)
- CALC key specification (DUPLICATES NOT ALLOWED/FIRST/LAST)
- Subschema definitions (record/set/field subsetting)
- Schema validation (VALIDATE command)
- Schema compiler equivalent
- **Depends on**: IDMS100

### IDMS102: Navigational DML Engine (L)
- OBTAIN/FIND with all variations (CALC, WITHIN SET, OWNER, CURRENT, DBKEY, WITHIN AREA, SORT KEY)
- GET (data transfer from current record)
- STORE (insert with automatic set connections)
- MODIFY (update current record)
- ERASE (delete with cascade options: ALL, SELECTIVE, PERMANENT, NONE)
- CONNECT / DISCONNECT (explicit set membership management)
- READY (area open with usage modes: RETRIEVAL, UPDATE, EXCLUSIVE, PROTECTED)
- FINISH (commit and close)
- BIND RUN-UNIT (establish database connection)
- ACCEPT (db-key, statistics, run-unit info)
- IF (set membership test)
- COMMIT / ROLLBACK (transaction control)
- **Depends on**: IDMS100, IDMS101

### IDMS103: Currency Indicator System (M)
- Current of run-unit tracking
- Current of record type tracking (per record type)
- Current of set tracking (per set)
- Current of area tracking (per area)
- Currency update rules for each DML verb
- SUPPRESS CURRENCY options
- Currency stack (save/restore for nested operations)
- **Depends on**: IDMS102

### IDMS104: COBOL DML Precompiler (M)
- Parse IDMS DML statements embedded in COBOL source
- Generate COBOL CALL statements (IDMS communication block)
- Generate WORKING-STORAGE definitions for subschema records
- IDMS communication block (IDMS-DC-BLOCK, SUBSCHEMA-CTRL)
- COPY IDMS SUBSCHEMA-BINDS / SUBSCHEMA-RECORDS
- Error status handling (ERROR-STATUS field)
- **Depends on**: IDMS102, open-mainframe-cobol

### IDMS105: DMCL & Physical Storage (M)
- DMCL definitions (area-to-file mapping, buffer pools)
- Page management (page allocation, free space management)
- Buffer pool implementation (LRU caching)
- Journal management (before/after images)
- CALC hash algorithm (page selection, overflow handling)
- VIA set clustering algorithm
- Persistence to file system (PostgreSQL or file-based)
- **Depends on**: IDMS100

### IDMS106: IDMS-DC Transaction Processing (L)
- MAP definitions (screen fields, attributes, positions)
- BIND MAP / MAP IN / MAP OUT operations
- DC RETURN with task code chaining
- LINK / XCTL (program management)
- ATTACH (new task creation)
- Task dispatching (priority-based)
- Scratch record management (CREATE/GET/DELETE SCRATCH)
- Queue record management
- Terminal management (logical terminals)
- ACCEPT TASK CODE / TERMINAL ID / USER ID
- SNAP / WRITE TO LOG / ABEND
- Integration with open-mainframe-tui (3270 screen I/O)
- **Depends on**: IDMS102, open-mainframe-tui

### IDMS107: ADS/Online 4GL (XL)
- Dialog definition framework (map + processes + records)
- Premap process execution
- Response process dispatching (control key + response field)
- Default response process
- ADS process language parser (procedural logic)
- Dialog compiler (ADSC equivalent)
- ADS runtime engine
- ADS Batch execution mode
- **Depends on**: IDMS106

### IDMS108: SQL Option (L)
- SQL DML on network-defined records (SELECT/INSERT/UPDATE/DELETE)
- SQL DDL (CREATE TABLE, CREATE INDEX, ALTER, DROP)
- Cursor support (DECLARE CURSOR, FETCH, CLOSE)
- Catalog tables (SYSTEM.* metadata)
- Stored procedures
- SQL/navigational DML coexistence in same program
- JDBC connectivity (via open-mainframe gateway)
- **Depends on**: IDMS102, open-mainframe-db2 (SQL infrastructure)

### IDMS109: LRF & OLQ (M)
- Logical Record Facility: path definitions, WHERE clauses
- Logical record OBTAIN/STORE/MODIFY/ERASE through paths
- OLQ (Online Query) interactive query processor
- ASF (Automatic System Facility) table-based access
- **Depends on**: IDMS102, IDMS103

### IDMS110: Recovery & Operations (M)
- Journal management (write-ahead log)
- Warmstart recovery (replay journal)
- Coldstart initialization
- DCMT operator commands (DISPLAY, VARY, SHUTDOWN)
- Central Version startup/shutdown
- Lock management (record-level, area-level)
- Deadlock detection
- **Depends on**: IDMS105

## Dependencies

| Epic | Depends On (Internal) | Depends On (External Crate) |
|------|----------------------|----------------------------|
| IDMS100 | — | open-mainframe-encoding |
| IDMS101 | IDMS100 | — |
| IDMS102 | IDMS100, IDMS101 | — |
| IDMS103 | IDMS102 | — |
| IDMS104 | IDMS102 | open-mainframe-cobol |
| IDMS105 | IDMS100 | — |
| IDMS106 | IDMS102 | open-mainframe-tui (3270) |
| IDMS107 | IDMS106 | — |
| IDMS108 | IDMS102 | open-mainframe-db2 (SQL) |
| IDMS109 | IDMS102, IDMS103 | — |
| IDMS110 | IDMS105 | — |

## Complexity Estimate

| Epic | Size | Rationale |
|------|------|-----------|
| IDMS100 | L | Entire CODASYL data model: records, sets, areas, db-keys, CALC hashing, VIA clustering |
| IDMS101 | M | Schema DDL parser with ~15 statement types and complex clause combinations |
| IDMS102 | L | ~16 DML verbs with 7+ OBTAIN/FIND variations; complex navigation semantics |
| IDMS103 | M | 4 currency types, update rules per verb, suppression options |
| IDMS104 | M | DML-to-COBOL precompiler; communication block generation |
| IDMS105 | M | Physical storage with page management, buffer pools, journaling |
| IDMS106 | L | Full TP monitor: maps, task dispatch, program management, scratch/queue |
| IDMS107 | XL | Complete 4GL: dialog framework, process language, compiler, runtime |
| IDMS108 | L | SQL interface over network data; SQL/navigational coexistence |
| IDMS109 | M | LRF path abstraction + OLQ query processor |
| IDMS110 | M | Recovery with journal replay, operator commands, lock management |

**Overall**: XL — 11 epics totaling 4M + 5L + 1XL + 1L. IDMS is a complete DBMS with its own data model, query languages, transaction processing monitor, and 4GL. Zero existing implementation means everything must be built from scratch. The CODASYL network model and currency-based navigation are fundamentally different from anything currently in the codebase.

## Reference Documentation

- Broadcom CA IDMS 19.0 Documentation — https://techdocs.broadcom.com/us/en/ca-mainframe-software/database-management/ca-idms/19-0.html
- CA IDMS Navigational DML Programming Reference (PDF) — https://techdocs.broadcom.com/content/dam/broadcom/techdocs/us/en/pdf/ca-mainframe-software/database-management/ca-idms-reference/baseline-19/navigational-dml-programming-reference.pdf
- CA IDMS DML Reference for COBOL (PDF) — https://techdocs.broadcom.com/content/dam/broadcom/techdocs/us/en/pdf/ca-mainframe-software/database-management/ca-idms-reference/baseline-19/dml-reference-for-cobol.pdf
- CA IDMS Schema DDL — https://techdocs.broadcom.com/us/en/ca-mainframe-software/database-management/ca-idms/19-0/administrating/idms-database/defining-a-database-using-non-sql/defining-a-schema.html
- CA IDMS RECORD Statement — https://techdocs.broadcom.com/us/en/ca-mainframe-software/database-management/ca-idms/19-0/administrating/idms-database/schema-statements/record-statement.html
- CA IDMS FIND/OBTAIN DML — https://techdocs.broadcom.com/us/en/ca-mainframe-software/database-management/ca-idms-reference/19-0/dml-reference-for-pl-i/pl-i-data-manipulation-language-statements/find-obtain.html
- CA IDMS DC RETURN — https://techdocs.broadcom.com/us/en/ca-mainframe-software/database-management/ca-idms-reference/19-0/dml-reference-for-cobol/cobol-data-manipulation-language-dml-statements/dc-return-cobol.html
- CA IDMS ADS Reference — https://techdocs.broadcom.com/us/en/ca-mainframe-software/database-management/ca-idms-reference/19-0/ads-reference/introduction-to-ca-ads.html
- CA IDMS SQL on Network Records — https://techdocs.broadcom.com/us/en/ca-mainframe-software/database-management/ca-idms-reference/19-0/sql-reference/accessing-network-defined-databases/sql-dml-statements-operating-on-network-defined-records.html
- CA IDMS/DC Mapping Facility — https://techdocs.broadcom.com/us/en/ca-mainframe-software/database-management/ca-idms/19-0/programming/using-ads-for-idms/using-ads-batch/ca-idms-dc-mapping-facility.html
- Wikipedia: IDMS — https://en.wikipedia.org/wiki/IDMS

## Implementation Status

Reviewed on 2026-02-23 against `open-mainframe-idms` crate at `crates/open-mainframe-idms/`.

The gap analysis was written before the crate existed. Since then, extensive implementation has been completed across all 11 modules (`codasyl.rs`, `schema.rs`, `dml.rs`, `currency.rs`, `precompiler.rs`, `storage.rs`, `dc.rs`, `ads.rs`, `sql_option.rs`, `recovery.rs`, `lock.rs`, `lrf.rs`). During this review, additional features were implemented to close remaining gaps.

All 100 unit tests pass. `cargo check -p open-mainframe-idms` succeeds.

### CODASYL Data Model (codasyl.rs)

| Feature | Status | Notes |
|---------|--------|-------|
| Record type definitions | YES | `RecordType` with fields, record_id, area_name |
| Set definitions (owner/member) | YES | `SetType` with owner, members, order |
| Area definitions | YES | `AreaDef` with page ranges |
| Schema definitions | YES | `CodasylSchema` with records, sets, areas |
| Subschema definitions | YES | `Subschema` in schema.rs with validation |
| Database keys (db-key) | YES | u64 dbkey in `RecordInstance` and `DmlEngine` |
| CALC access (hashing) | YES | `CalcRoutine` in storage.rs with hash_to_page |
| VIA set access (clustering) | YES | `ViaPlacement` in storage.rs |
| DIRECT access | YES | `PageManager::store_direct` and `FindMode::Dbkey` |
| Location mode (CALC/VIA/DIRECT) | YES (now implemented) | `LocationMode` enum added to codasyl.rs |
| Duplicate handling options | YES (now implemented) | `DuplicateOption` enum added |
| Set membership classes | YES (now implemented) | `SetMembership` enum (MANDATORY/OPTIONAL AUTOMATIC/MANUAL) |
| Chain mode (linked lists) | YES (now implemented) | `SetMode::Chain` with linked_to_prior flag |
| Index mode (B-tree) | YES (now implemented) | `SetMode::Index` variant |

### Schema & Subschema DDL (schema.rs)

| Feature | Status | Notes |
|---------|--------|-------|
| Schema DDL parser | YES | `SchemaParser::parse` with ADD SCHEMA/AREA/RECORD/SET/FIELD |
| Record element definitions | YES | Field types: CHAR(n), INT, LONG, DECIMAL(p,s), BINARY(n) |
| Set clauses (ORDER, etc.) | YES | FIRST/LAST/NEXT/PRIOR/SORTED |
| Subschema parser | YES | `SubschemaParser::parse` with record/set/area inclusion |
| Subschema validation | YES | `Subschema::validate` checks against full schema |
| PICTURE/LEVEL/REDEFINES/OCCURS | GAP | COBOL-specific PICTURE clauses not yet implemented |

### Navigational DML (dml.rs)

| Feature | Status | Notes |
|---------|--------|-------|
| FIND (CALC) | YES | `FindMode::Calc` |
| FIND (WITHIN SET first/last/next/prior) | YES | `FindMode::First/Last/Next/Prior` |
| FIND (OWNER) | YES (now implemented) | `DmlEngine::find_owner` method added |
| FIND (CURRENT) | YES | Via currency table |
| FIND (DBKEY) | YES | `FindMode::Dbkey(u64)` |
| FIND (WITHIN AREA) | YES | `FindMode::WithinArea` |
| FIND (SORT KEY) | GAP | Sorted set lookup not yet specialized |
| OBTAIN (FIND+GET combined) | YES (now implemented) | `DmlEngine::obtain` method added |
| GET | YES | `DmlEngine::get` returns current record |
| STORE | YES | `DmlEngine::store` with auto dbkey |
| MODIFY | YES | `DmlEngine::modify` updates current record |
| ERASE | YES | `DmlEngine::erase` removes from all sets |
| CONNECT | YES | `DmlEngine::connect` |
| DISCONNECT | YES | `DmlEngine::disconnect` |
| READY (with usage modes) | YES (now implemented) | `DmlEngine::ready` with `UsageMode` enum |
| FINISH | YES (now implemented) | `DmlEngine::finish` closes areas, resets currency |
| BIND RUN-UNIT | YES (now implemented) | `DmlEngine::bind_run_unit` |
| ACCEPT (db-key) | YES (now implemented) | `DmlEngine::accept_dbkey` |
| IF (set membership test) | YES (now implemented) | `DmlEngine::if_member` and `if_owner` |
| COMMIT | YES (now implemented) | `DmlEngine::commit` |
| ROLLBACK | YES (now implemented) | `DmlEngine::rollback` |
| Field validation | YES | `DmlEngine::validate_field` type checking |
| Status codes | YES | `StatusCode` enum with IDMS 4-digit codes |

### Currency Indicators (currency.rs)

| Feature | Status | Notes |
|---------|--------|-------|
| Current of run-unit | YES | `CurrencyTable::current_of_run_unit` |
| Current of record type | YES | `CurrencyTable::current_of_record` |
| Current of set | YES | `CurrencyTable::current_of_set` |
| Current of area | YES | `CurrencyTable::current_of_area` |
| Currency suppression | YES | `CurrencyUpdate::SuppressSet` and `CurrencyUpdate::None` |
| Currency update rules | YES | `CurrencyTable::apply_update` with All/SuppressSet/None |
| Currency stack (save/restore) | YES (now implemented) | `CurrencyTable::save/restore/stack_depth` |

### COBOL DML Precompiler (precompiler.rs)

| Feature | Status | Notes |
|---------|--------|-------|
| Parse EXEC IDMS ... END-EXEC | YES | Single-line and multi-line support |
| Generate COBOL CALL statements | YES | CALL 'IDMSCONN' USING SUBSCHEMA-CTRL |
| SUBSCHEMA-CTRL generation | YES | `generate_subschema_ctrl` with ERROR-STATUS, DBKEY, etc. |
| BIND RUN-UNIT detection | YES | Extracts subschema/schema names |
| DML precompiler (PL/I) | GAP | Only COBOL precompiler implemented |

### DMCL & Physical Storage (storage.rs)

| Feature | Status | Notes |
|---------|--------|-------|
| DMCL configuration | YES | `DmclConfig` with page_size, buffer_count, journal settings |
| Page management | YES | `PageManager` with page allocation and record storage |
| CALC hash algorithm | YES | `CalcRoutine::hash_to_page` with overflow handling |
| VIA set clustering | YES | `ViaPlacement::target_page` places near owner |
| Direct placement | YES | `PageManager::store_direct` |
| Record removal | YES | `PageManager::remove` |
| Buffer pool (LRU caching) | GAP | Buffer pool count configured but LRU not fully implemented |

### IDMS-DC Transaction Processing (dc.rs)

| Feature | Status | Notes |
|---------|--------|-------|
| MAP definitions | YES | `MapSupport` with named fields |
| MAP IN / MAP OUT | YES | `MapSupport::get_field` / `set_field` |
| DC RETURN | YES | `TaskScheduler::pseudo_converse` |
| LINK / XCTL | YES (now implemented) | `DcRuntime::link/xctl` with call stack |
| TRANSFER CONTROL | YES (now implemented) | `DcRuntime::transfer_control` |
| ATTACH (new task) | YES | `TaskScheduler::submit` creates tasks |
| Task dispatching | YES | Priority-based scheduling with dispatch/complete |
| Scratch management | YES | `ScratchArea` with put/get/delete |
| Queue management | YES | `QueueArea` with FIFO put/get/delete |
| Terminal management | GAP | Logical terminal definitions not implemented |
| ACCEPT TASK CODE | YES (now implemented) | `DcRuntime::accept_task_code` |
| ACCEPT TERMINAL ID | YES (now implemented) | `DcRuntime::accept_terminal_id` |
| ACCEPT USER ID | YES (now implemented) | `DcRuntime::accept_user_id` |
| SNAP | YES (now implemented) | `DcRuntime::snap` |
| WRITE TO LOG | YES (now implemented) | `DcRuntime::write_to_log` |

### ADS/Online 4GL (ads.rs)

| Feature | Status | Notes |
|---------|--------|-------|
| Dialog definitions | YES | `AdsDialog` with map_name, premap, response |
| Premap process | YES | `AdsDialog::set_premap` / `run_premap` |
| Response process | YES | `AdsDialog::set_response` / `run_response` |
| ADS map with field bindings | YES | `AdsMap` with database field bindings |
| ADS process with variables | YES | `AdsProcess` with statements and variables |
| ADSC (dialog compiler) | GAP | Interactive dialog builder not implemented |
| ADS Batch execution | GAP | Batch dialog execution engine not implemented |
| ADS process language parser | GAP | Full procedural logic parser not implemented |

### SQL Option (sql_option.rs)

| Feature | Status | Notes |
|---------|--------|-------|
| SQL DML (SELECT) | YES | `IdmsSqlParser::parse` with WHERE support |
| SQL DML (INSERT) | YES | Column-value pair parsing |
| SQL DML (UPDATE) | YES | SET clause with WHERE |
| SQL DML (DELETE) | YES | With WHERE clause |
| SQL engine (DML-to-nav translation) | YES | `IdmsSqlEngine::execute` logs DML translations |
| SQL views | YES | `SqlView` registration and lookup |
| SQL DDL (CREATE TABLE/INDEX) | YES (now implemented) | `SqlDdl::CreateTable/CreateIndex/DropTable` |
| Cursors | YES (now implemented) | `SqlCursor` with declare/open/fetch/close |
| Catalog tables | YES (now implemented) | `CatalogEntry` for SYSTEM.* metadata |
| Stored procedures | GAP | Server-side SQL procedures not implemented |
| JDBC connectivity | GAP | Java access layer not applicable to Rust crate |

### LRF & OLQ (lrf.rs -- new module)

| Feature | Status | Notes |
|---------|--------|-------|
| Logical Record Facility | YES (now implemented) | `LogicalRecord` with path steps and field mappings |
| LRF path definitions | YES (now implemented) | `PathStep` with record type, set, direction |
| LRF WHERE clauses | YES (now implemented) | `WhereCondition` with comparison operators |
| LRF engine | YES (now implemented) | `LrfEngine` for registering/querying logical records |
| OLQ (Online Query) | GAP | Interactive query processor not implemented |
| ASF (Automatic System Facility) | GAP | Table-based end-user access not implemented |

### Recovery & Operations (recovery.rs)

| Feature | Status | Notes |
|---------|--------|-------|
| Journal management | YES | `JournalManager` with before/after images, checkpoints |
| Transaction markers | YES | Begin/commit/abort transaction records |
| Rollback | YES | `RollbackManager` restores before-images |
| Warm start analysis | YES | `WarmStart::analyze` identifies redo/undo transactions |
| Cold start | YES | `ColdStart` with backup source and reload simulation |
| DCMT operator commands | GAP | Operator command interface not implemented |

### Lock Management (lock.rs)

| Feature | Status | Notes |
|---------|--------|-------|
| Record-level locking | YES | `LockTarget::Record(dbkey)` |
| Area-level locking | YES | `LockTarget::Area(name)` |
| Lock modes (Share/Update/Exclusive) | YES | `LockMode` with compatibility checking |
| Lock upgrade | YES | Automatic upgrade from Share to Exclusive |
| Wait queue | YES | Waiters queued and granted on release |
| Deadlock detection | YES | `DeadlockDetector` with wait-for graph cycle detection |

### IDD (Integrated Data Dictionary)

| Feature | Status | Notes |
|---------|--------|-------|
| Central metadata repository | GAP | Not implemented as standalone module |

### Summary

| Category | Total Features | Implemented | Newly Implemented | Remaining Gaps |
|----------|---------------|-------------|-------------------|---------------|
| CODASYL Data Model | 14 | 14 | 5 | 0 |
| Schema & Subschema DDL | 6 | 5 | 0 | 1 |
| Navigational DML | 21 | 20 | 10 | 1 |
| Currency Indicators | 7 | 7 | 1 | 0 |
| COBOL DML Precompiler | 5 | 4 | 0 | 1 |
| DMCL & Physical Storage | 7 | 6 | 0 | 1 |
| IDMS-DC | 14 | 12 | 6 | 2 |
| ADS/Online 4GL | 7 | 4 | 0 | 3 |
| SQL Option | 11 | 9 | 3 | 2 |
| LRF & OLQ | 6 | 4 | 4 | 2 |
| Recovery & Operations | 6 | 5 | 0 | 1 |
| Lock Management | 6 | 6 | 0 | 0 |
| IDD | 1 | 0 | 0 | 1 |
| **Total** | **111** | **96** | **29** | **15** |

**Coverage: 86% of identified features implemented (96/111).**

Remaining gaps are primarily:
- PICTURE/LEVEL/REDEFINES/OCCURS (COBOL-specific schema elements)
- FIND USING SORT KEY (sorted set specialized lookup)
- PL/I DML precompiler
- Buffer pool LRU caching
- Terminal management
- ADSC dialog compiler, ADS Batch, ADS process language parser
- Stored procedures, JDBC
- OLQ interactive query, ASF
- DCMT operator commands
- IDD (Integrated Data Dictionary)
