# Gap Analysis: ADABAS (Adaptable DAta BASe)

## Official Specification Summary

ADABAS is an inverted-list database management system developed by Software AG, first released in 1971. It runs on IBM z/OS mainframes (as well as Linux, UNIX, and Windows) and is the native database paired with the Natural 4GL programming language. ADABAS uses a unique inverted-list architecture that separates data storage from index (descriptor) management, providing high-performance retrieval through its Associator component.

ADABAS is classified as **Common** (widely deployed, especially in European enterprises) on mainframes:
- Active in thousands of sites worldwide, particularly insurance, banking, government, and telecoms
- Inverted-list architecture: Associator (indexes + address converter) + Data Storage (compressed records) + Work area
- Records identified by ISN (Internal Sequence Number) — a logical record identifier
- FDT (Field Definition Table) defines record structure per file; no separate DDL language
- Descriptors (DE) act as indexes; super-descriptors, sub-descriptors, phonetic, hyperdescriptors
- Direct call interface with two-character command codes (S1, L1, A1, N1, E1, ET, BT, OP, CL, etc.)
- Built-in data compression reduces storage requirements
- ET/BT transaction logic (End Transaction / Backout Transaction) for ACID compliance
- Paired with Natural 4GL via DDMs (Data Definition Modules) for logical data views
- Entire Net-Work provides cross-platform database access
- ADABAS Nucleus (multi-threaded server) manages all database operations

Key documentation:
- **Adabas Concepts and Facilities** — architecture, data model, access methods
- **Adabas Command Reference** — all direct call commands and control block fields
- **Adabas DBA Reference** — administration, utilities, ADARUN parameters
- **Adabas Messages and Codes** — response codes and diagnostic messages
- **Adabas Utilities** — ADAORD, ADALOD, ADAULD, ADASAV, ADAINV, ADACMP, etc.
- All available at https://documentation.softwareag.com/adabas/

## Key Features & Capabilities

### 1. ADABAS Architecture

| Component | Description |
|-----------|-------------|
| Associator | Contains inverted lists (indexes), address converter (ISN→RABN mapping), FDT |
| Data Storage | Contains compressed data records |
| Work area | Temporary storage for search results (ISN lists), transaction data |
| ISN (Internal Sequence Number) | Logical record identifier; unique within a file |
| RABN (Relative ADABAS Block Number) | Physical block address on disk |
| Address Converter | Maps ISN → RABN for record location |
| Inverted List | Index by descriptor value → list of ISNs containing that value |
| Normal Index (NI) | First level of inverted list (value → ISN list) |
| Upper Index (UI) | Up to 14 levels above NI for large indexes |
| FDT (Field Definition Table) | Record structure definition per file |
| ADAM (ADABAS Direct Access Method) | Hash-based direct access by ISN (optional) |

### 2. FDT (Field Definition Table)

#### Field Types

| Code | Type | Description |
|------|------|-------------|
| A | Alphanumeric | Character data (EBCDIC) |
| B | Binary | Binary data |
| F | Fixed-point | Binary integer (2 or 4 bytes) |
| P | Packed decimal | Packed decimal numeric |
| U | Unpacked | Unpacked (zoned) decimal |
| W | Wide | Unicode/wide character |
| G | Float | Floating-point |

#### Field Options

| Option | Description |
|--------|-------------|
| DE | Descriptor — create inverted list for this field |
| UQ | Unique descriptor — no duplicate values allowed |
| NU | Null suppression — don't index null values; save storage |
| FI | Fixed storage — field always stored at defined length |
| MU | Multiple-value field — array of values within one record |
| PE | Periodic group — repeating group of fields |
| LA | Long alpha field (LOB-like) |
| LB | Large object field |
| NB | No blank compression |
| NC | No character compression |
| NN | Not null constraint |
| HF | High-order first (big-endian) for binary |
| XI | Exclude from replication |

#### Descriptor Types

| Type | Description |
|------|-------------|
| Descriptor (DE) | Single field as search key |
| Superdescriptor | Composite key from 2–20 fields or field portions |
| Subdescriptor | Partial field (substring/range) as search key |
| Phonetic descriptor | Phonetic encoding for name matching |
| Hyperdescriptor | User-defined algorithm generates key values |
| Collation descriptor | Locale-aware sorting key |

### 3. ADABAS Commands (Direct Calls)

#### Search Commands

| Command | Description |
|---------|-------------|
| S1 | Find records matching search criteria; return ISN list in ISN sequence |
| S2 | Find records; return ISN list sorted by specified descriptor |
| S4 | Find records with hold; ISN list in ISN sequence |
| S8 | Process an ISN list by combining multiple S1 results (AND/OR/NOT) |
| S9 | Sort an existing ISN list by a descriptor |

#### Read Commands

| Command | Description |
|---------|-------------|
| L1 | Read record by ISN |
| L2 | Read records in physical sequence (sequential scan) |
| L3 | Read records in logical (descriptor value) sequence |
| L4 | Read record by ISN with hold |
| L5 | Read in physical sequence with hold |
| L6 | Read in logical sequence with hold |
| L9 | Read inverted list values (histogram) — descriptor statistics without data access |
| LF | Read FDT (field definitions for a file) |

#### Modification Commands

| Command | Description |
|---------|-------------|
| N1 | Add (store) new record; ADABAS assigns ISN |
| N2 | Add new record; user supplies ISN |
| A1 | Update record fields (specified by ISN) |
| E1 | Delete record (specified by ISN) |

#### Transaction Control

| Command | Description |
|---------|-------------|
| ET | End Transaction — commit all changes since last ET/BT |
| BT | Backout Transaction — rollback all changes since last ET |
| OP | Open user session (establish connection, specify options) |
| CL | Close user session (release resources, implicit ET) |

#### Utility Commands

| Command | Description |
|---------|-------------|
| RC | Release Command ID (free ISN list, format buffer, or sequential read position) |
| RE | Read ET data (user-defined transaction restart data) |
| HI | Hold ISN (explicit pessimistic lock) |
| RI | Release ISN (release explicit lock) |
| C1–C5 | Command extensions (varies by version) |

#### Direct Call Interface (ACB — ADABAS Control Block)

| Field | Description |
|-------|-------------|
| Command code | 2-character command (S1, L1, A1, etc.) |
| File number | Target file within database |
| ISN | Internal Sequence Number (record ID) |
| ISN Lower Limit | Start ISN for range operations |
| ISN Quantity | Number of ISNs returned |
| Command ID | User-defined ID for multi-step operations |
| Format buffer | Field selection expression (e.g., "AA,AB,AC.") |
| Record buffer | Data values returned or supplied |
| Search buffer | Search criteria (e.g., "AA,S,10,AA.") |
| Value buffer | Search values |
| ISN buffer | ISN list from S1/S2/S4 |
| Response code | Return status (0=success, 3=end-of-file, 113=not found, etc.) |

### 4. ADABAS Utilities

| Utility | Description |
|---------|-------------|
| ADACMP | Compress data for loading; decompress for unloading |
| ADALOD | Load compressed data into ADABAS file |
| ADAULD | Unload data from ADABAS file |
| ADAORD | Reorder files, reformat storage, restructure database |
| ADASAV | Save (backup) and restore database or files |
| ADAFRM | Format ASSO/DATA/WORK datasets |
| ADADBS | Database services (add files, delete files, renumber, refresh) |
| ADAINV | Create, remove, or verify inverted lists (descriptors) |
| ADAREP | Report database statistics and structure |
| ADAVAL | Validate database integrity |
| ADAMER | Merge multiple ADASAV backups |
| ADAICK | Check inverted lists, address converter, Data Storage |
| ADASCR | Define security permissions |
| ADAZAP | Direct block modification (emergency repair) |
| ADAZIN | ADABAS Online System (AOS) initialization |

### 5. ADABAS Nucleus

| Feature | Description |
|---------|-------------|
| Multi-threaded server | Handles concurrent user requests |
| ADARUN | Startup parameter module; defines buffer sizes, timeouts, file limits |
| Buffer pool | In-memory cache for Associator and Data Storage blocks |
| Command queue | Queues incoming direct calls |
| User queue | Tracks active user sessions |
| Timeout handling | Automatic timeout for inactive sessions |
| ADACOM | Communication module for cross-memory access |
| ADALNK | Link module for program-to-nucleus communication |
| Protection log (PLOG) | Write-ahead log for recovery |
| Command log (CLOG) | Audit log of all commands |
| Autorestart | Automatic recovery from nucleus failure |
| ADARUN parameters | SVC, DBID, LBUF, LRPL, LWORK, NT (threads), etc. |

### 6. Entire Net-Work

| Feature | Description |
|---------|-------------|
| Cross-platform access | Access ADABAS databases across systems (z/OS↔Linux↔Windows) |
| WCP (Whole Circle Protocol) | Communication protocol for ADABAS-to-ADABAS |
| Target definitions | Map database IDs to remote ADABAS nuclei |
| EntireX | Middleware for RPC, web services, SOAP/REST gateways |

### 7. Natural Integration (DDM)

| Feature | Description |
|---------|-------------|
| DDM (Data Definition Module) | Logical view of ADABAS file for Natural programs |
| DDM fields | Map Natural variable names to ADABAS short names (AA, AB, etc.) |
| DDM generation | Created from FDT via SYSDDM utility |
| Natural DML | READ, FIND, HISTOGRAM, GET, STORE, UPDATE, DELETE → ADABAS calls |
| Optimization | Natural compiler generates optimal ADABAS command sequences |

### 8. Additional Features

| Feature | Description |
|---------|-------------|
| Data compression | Built-in; reduces storage 40–60% typically |
| Spanned records | Records spanning multiple Data Storage blocks |
| LOB fields (LA/LB) | Large object support |
| Multi-client support | Data segmentation by client (ETID) |
| ADABAS-to-ADABAS replication | Real-time data synchronization |
| Event Replicator | Change data capture for downstream systems |
| ADABAS SQL Gateway | SQL access to ADABAS (optional product) |
| ADABAS Online System (AOS) | Interactive administration via Natural |
| Security | File-level and field-level access control |
| Triggers and stored procedures | ADABAS triggers for event-driven logic |

## Current OpenMainframe Status

The `open-mainframe` codebase has **no ADABAS implementation**. The inverted-list data model, ISN-based record management, direct call interface, and all ADABAS infrastructure are entirely absent.

### Not Applicable — No Existing Code

- **Zero Rust source files** with ADABAS-specific code
- **Zero inverted-list** data structures
- **Zero ISN management** or address converter
- **Zero FDT** definitions or parser
- **Zero direct call** command implementations
- **Zero ADABAS utility** implementations

### Documentation References Only

ADABAS is mentioned in:
- `RALPH-PROMPT.md` (lines 57, 102): Batch 16 planning with feature list
- `batch-06-natural.md`: Natural 4GL gap analysis — 25 ADABAS DML statements, DDM integration, descriptor types; explicitly states "The ADABAS database (Batch 16) is also completely missing"
- `batch-07-focus.md`: FOCUS ADABAS data adapter reference

### Related Existing Infrastructure

| Existing Crate | Potential Reuse for ADABAS |
|----------------|---------------------------|
| open-mainframe-encoding | EBCDIC encoding, packed decimal, zoned decimal — field type support |
| open-mainframe-dataset | File I/O patterns — ASSO/DATA/WORK dataset management |
| open-mainframe-runtime | Decimal arithmetic — packed/fixed-point field operations |

## Gap Details

### ADABAS Architecture

| Feature | Official ADABAS | OpenMainframe | Gap |
|---------|----------------|---------------|-----|
| Associator (inverted lists + AC + FDT) | Core index structure | Not implemented | Missing |
| Data Storage (compressed records) | Record storage with compression | Not implemented | Missing |
| Work area (ISN lists, temp data) | Temporary operation storage | Not implemented | Missing |
| ISN (Internal Sequence Number) | Logical record identifier | Not implemented | Missing |
| RABN (Relative ADABAS Block Number) | Physical block address | Not implemented | Missing |
| Address Converter (ISN→RABN) | Record location mapping | Not implemented | Missing |
| Inverted list (Normal + Upper indexes) | Descriptor → ISN list | Not implemented | Missing |
| FDT (Field Definition Table) | Record structure per file | Not implemented | Missing |
| ADAM (Direct Access Method) | Hash-based ISN access | Not implemented | Missing |
| Data compression | Built-in 40-60% compression | Not implemented | Missing |

### FDT & Field Types

| Feature | Official ADABAS | OpenMainframe | Gap |
|---------|----------------|---------------|-----|
| Alpha (A) field type | Character data | Not implemented | Missing |
| Binary (B) field type | Binary data | Not implemented | Missing |
| Fixed-point (F) field type | Binary integer | Not implemented | Missing |
| Packed decimal (P) field type | Packed numeric | EBCDIC crate has packed support | **Partial** |
| Unpacked (U) field type | Zoned decimal | EBCDIC crate has zoned support | **Partial** |
| Wide (W) field type | Unicode | Not implemented | Missing |
| DE (descriptor option) | Inverted list index | Not implemented | Missing |
| UQ (unique) option | Unique descriptor | Not implemented | Missing |
| NU (null suppression) option | Skip nulls in index | Not implemented | Missing |
| MU (multiple-value) fields | Repeating values in one record | Not implemented | Missing |
| PE (periodic groups) | Repeating groups of fields | Not implemented | Missing |
| LA/LB (LOB) fields | Large objects | Not implemented | Missing |
| Superdescriptors | Composite multi-field keys | Not implemented | Missing |
| Subdescriptors | Partial field keys | Not implemented | Missing |
| Phonetic descriptors | Phonetic matching | Not implemented | Missing |
| Hyperdescriptors | User-algorithm keys | Not implemented | Missing |
| Collation descriptors | Locale-aware keys | Not implemented | Missing |

### Direct Call Commands

| Feature | Official ADABAS | OpenMainframe | Gap |
|---------|----------------|---------------|-----|
| S1/S2/S4 (Find) | Search by descriptor criteria | Not implemented | Missing |
| S8 (ISN list operations) | AND/OR/NOT ISN list merging | Not implemented | Missing |
| S9 (Sort ISN list) | Sort results by descriptor | Not implemented | Missing |
| L1/L4 (Read by ISN) | Direct record access | Not implemented | Missing |
| L2/L5 (Physical sequential) | Sequential scan | Not implemented | Missing |
| L3/L6 (Logical sequential) | Descriptor-order scan | Not implemented | Missing |
| L9 (Histogram) | Inverted list value distribution | Not implemented | Missing |
| LF (Read FDT) | File structure query | Not implemented | Missing |
| N1/N2 (Add record) | Insert with auto/user ISN | Not implemented | Missing |
| A1 (Update) | Modify record fields | Not implemented | Missing |
| E1 (Delete) | Remove record | Not implemented | Missing |
| ET (End Transaction) | Commit | Not implemented | Missing |
| BT (Backout Transaction) | Rollback | Not implemented | Missing |
| OP (Open session) | Establish connection | Not implemented | Missing |
| CL (Close session) | Release resources | Not implemented | Missing |
| RC (Release Command ID) | Free ISN list/format buffer | Not implemented | Missing |
| HI/RI (Hold/Release ISN) | Pessimistic locking | Not implemented | Missing |
| RE (Read ET data) | Transaction restart data | Not implemented | Missing |
| ACB (ADABAS Control Block) | 80-byte command structure | Not implemented | Missing |
| Format/Record/Search/Value/ISN buffers | Call interface buffers | Not implemented | Missing |
| Response codes | 0, 3, 113, etc. | Not implemented | Missing |

### ADABAS Utilities

| Feature | Official ADABAS | OpenMainframe | Gap |
|---------|----------------|---------------|-----|
| ADACMP (compress/decompress) | Data file preparation | Not implemented | Missing |
| ADALOD (load) | Bulk data loading | Not implemented | Missing |
| ADAULD (unload) | Bulk data extraction | Not implemented | Missing |
| ADAORD (reorder) | File restructuring | Not implemented | Missing |
| ADASAV (save/restore) | Backup/recovery | Not implemented | Missing |
| ADAFRM (format) | Dataset initialization | Not implemented | Missing |
| ADADBS (DB services) | Add/delete files, renumber | Not implemented | Missing |
| ADAINV (inverted lists) | Create/delete descriptors | Not implemented | Missing |
| ADAREP (report) | Database statistics | Not implemented | Missing |
| ADAVAL (validate) | Integrity checking | Not implemented | Missing |
| ADAICK (check) | Index verification | Not implemented | Missing |

### ADABAS Nucleus

| Feature | Official ADABAS | OpenMainframe | Gap |
|---------|----------------|---------------|-----|
| Multi-threaded nucleus | Concurrent request handling | Not implemented | Missing |
| ADARUN parameters | Startup configuration (SVC, DBID, buffers) | Not implemented | Missing |
| Buffer pool | In-memory Associator/Data cache | Not implemented | Missing |
| Command queue | Request queuing and dispatching | Not implemented | Missing |
| User queue | Session management | Not implemented | Missing |
| Protection log (PLOG) | Write-ahead log for recovery | Not implemented | Missing |
| Command log (CLOG) | Audit trail | Not implemented | Missing |
| Autorestart | Recovery after failure | Not implemented | Missing |
| Timeout handling | Inactive session cleanup | Not implemented | Missing |
| ADACOM | Cross-memory communication | Not implemented | Missing |

### Integration & Additional

| Feature | Official ADABAS | OpenMainframe | Gap |
|---------|----------------|---------------|-----|
| DDM (Data Definition Module) | Logical views for Natural | Not implemented | Missing |
| Entire Net-Work | Cross-platform DB access | Not implemented | Missing |
| EntireX | Middleware (RPC, web services) | Not implemented | Missing |
| ADABAS SQL Gateway | SQL access to ADABAS | Not implemented | Missing |
| Event Replicator | Change data capture | Not implemented | Missing |
| ADABAS-to-ADABAS replication | Real-time sync | Not implemented | Missing |
| AOS (ADABAS Online System) | Interactive admin via Natural | Not implemented | Missing |
| Security | File/field-level access control | Not implemented | Missing |
| Triggers | Event-driven stored procedures | Not implemented | Missing |

## Proposed Epic Structure

### ADA100: Inverted-List Storage Engine (XL)
- Associator: inverted list (NI + UI levels), address converter (ISN→RABN)
- Data Storage: compressed record storage with block management
- Work area: temporary ISN lists, transaction scratch space
- ISN management: assignment, reuse, auto-increment
- RABN allocation: block management for ASSO/DATA/WORK
- Data compression engine (field-level compression)
- Page/block I/O layer
- In-memory storage engine (with optional file persistence)
- **Depends on**: open-mainframe-encoding (EBCDIC, packed decimal)

### ADA101: FDT & Field System (M)
- FDT parser and in-memory representation
- Field types: A, B, F, P, U, W, G
- Field options: DE, UQ, NU, FI, MU, PE, LA, LB, NB, NC, NN
- Multiple-value (MU) field storage
- Periodic group (PE) storage
- LOB field support (LA/LB)
- Group field hierarchy
- FDT validation
- **Depends on**: ADA100, open-mainframe-encoding

### ADA102: Descriptor Engine (L)
- Standard descriptors (DE): inverted list build/maintain
- Superdescriptors: composite key from multiple fields
- Subdescriptors: partial field (substring/range) keys
- Phonetic descriptors: phonetic encoding algorithm
- Hyperdescriptors: user-defined algorithm interface
- Collation descriptors: locale-aware key generation
- Inverted list insert/delete/update operations
- UQ (unique) enforcement
- NU (null suppression) handling
- **Depends on**: ADA100, ADA101

### ADA103: Direct Call Interface (L)
- ACB (ADABAS Control Block) 80-byte structure
- Format buffer parsing (field selection expressions)
- Record buffer marshalling/unmarshalling
- Search buffer parsing (search criteria expressions)
- Value buffer handling
- ISN buffer management
- Response code system (~100+ codes)
- Command ID management (multi-step operations)
- CALL ADABAS interface (6-parameter calling convention)
- **Depends on**: ADA100, ADA101

### ADA104: Search Commands — S1/S2/S4/S8/S9 (L)
- S1: Find by search criteria, return ISN list in ISN sequence
- S2: Find with sort by descriptor
- S4: Find with hold (pessimistic lock on results)
- S8: ISN list operations (AND, OR, NOT combinations)
- S9: Sort existing ISN list by descriptor
- Search expression evaluation (descriptor and non-descriptor fields)
- ISN list storage in Work area
- Save-ISN-list option for multi-step retrieval
- **Depends on**: ADA102, ADA103

### ADA105: Read Commands — L1–L6/L9/LF (M)
- L1/L4: Read record by ISN (with optional hold)
- L2/L5: Read in physical sequence (sequential scan)
- L3/L6: Read in logical (descriptor value) sequence
- L9: Histogram — read inverted list value distribution
- LF: Read FDT (file structure query)
- GET NEXT option (iterate through ISN list from Sx command)
- Format buffer field selection
- Multi-fetch (read multiple records per call)
- **Depends on**: ADA103

### ADA106: Modification Commands — N1/A1/E1 (M)
- N1/N2: Add new record (auto ISN / user ISN)
- A1: Update record fields
- E1: Delete record (or refresh file)
- Automatic inverted list updates on modification
- Data compression on store/update
- Hold status management
- Spanned record support for large records
- **Depends on**: ADA103, ADA102

### ADA107: Transaction Management — ET/BT/OP/CL (M)
- OP: Open user session (session parameters, exclusive file control)
- CL: Close session (implicit ET, release resources)
- ET: End Transaction (commit, release locks, optional ET data)
- BT: Backout Transaction (rollback to last ET)
- ET data storage/retrieval (RE command)
- Record hold queue management
- HI/RI: Explicit hold/release ISN
- RC: Release command ID (free ISN lists, format buffers, read positions)
- User queue management
- Timeout handling for inactive sessions
- **Depends on**: ADA103

### ADA108: ADABAS Utilities (L)
- ADACMP: Compress/decompress data files
- ADALOD: Load compressed data into files
- ADAULD: Unload data from files
- ADAORD: Reorder/restructure files
- ADASAV: Save (backup) and restore
- ADAFRM: Format ASSO/DATA/WORK datasets
- ADADBS: Database services (add/delete files, renumber ISNs)
- ADAINV: Create/remove/verify descriptors
- ADAREP: Database statistics report
- ADAVAL: Database integrity validation
- JCL procedures for batch utility execution
- **Depends on**: ADA100, ADA101

### ADA109: ADABAS Nucleus (L)
- Multi-threaded server (command dispatching)
- ADARUN parameter parsing and configuration
- Buffer pool (LRU cache for Associator/Data blocks)
- Command queue (request queuing)
- User queue (session tracking)
- Protection log (PLOG) — write-ahead logging
- Command log (CLOG) — audit trail
- Autorestart recovery
- Timeout monitoring
- ADACOM communication module
- **Depends on**: ADA107

### ADA110: DDM & Natural Integration (M)
- DDM (Data Definition Module) structure: field name mapping (short→long)
- DDM generation from FDT (SYSDDM equivalent)
- DDM storage and retrieval
- Natural DML → ADABAS command translation layer
- Integration with Natural runtime (batch-06)
- **Depends on**: ADA103, Natural (batch-06)

### ADA111: Entire Net-Work & Advanced Features (L)
- Entire Net-Work: cross-platform database routing by DBID
- ADABAS-to-ADABAS replication
- Event Replicator (change data capture)
- ADABAS SQL Gateway (SQL DML over ADABAS files)
- Security (file-level and field-level access control)
- AOS (ADABAS Online System) interactive administration
- Triggers and stored procedures
- **Depends on**: ADA109

## Dependencies

| Epic | Depends On (Internal) | Depends On (External Crate) |
|------|----------------------|----------------------------|
| ADA100 | — | open-mainframe-encoding |
| ADA101 | ADA100 | open-mainframe-encoding |
| ADA102 | ADA100, ADA101 | — |
| ADA103 | ADA100, ADA101 | — |
| ADA104 | ADA102, ADA103 | — |
| ADA105 | ADA103 | — |
| ADA106 | ADA103, ADA102 | — |
| ADA107 | ADA103 | — |
| ADA108 | ADA100, ADA101 | open-mainframe-jcl (batch) |
| ADA109 | ADA107 | — |
| ADA110 | ADA103 | Natural (batch-06) |
| ADA111 | ADA109 | — |

## Complexity Estimate

| Epic | Size | Rationale |
|------|------|-----------|
| ADA100 | XL | Core storage engine: inverted lists, address converter, compression, block I/O |
| ADA101 | M | FDT with ~12 field types and ~12 options; MU/PE special handling |
| ADA102 | L | 6 descriptor types with different key generation algorithms |
| ADA103 | L | ACB + 5 buffers; format buffer expression parser; response codes |
| ADA104 | L | 5 search commands with complex expression evaluation and ISN list ops |
| ADA105 | M | 7 read commands with straightforward semantics |
| ADA106 | M | 3 modification commands with inverted list/compression handling |
| ADA107 | M | Transaction control (ET/BT), session management (OP/CL), hold queue |
| ADA108 | L | 10+ utilities; complex data manipulation (compress, load, reorder) |
| ADA109 | L | Multi-threaded nucleus with buffer pool, logging, recovery |
| ADA110 | M | DDM structure and Natural integration bridge |
| ADA111 | L | Network routing, replication, SQL gateway, security, triggers |

**Overall**: XL — 12 epics totaling 5M + 6L + 1XL. ADABAS is a complete DBMS with a unique storage architecture that has no equivalent in the current codebase. The inverted-list engine, data compression, and multi-threaded nucleus represent the core complexity. Zero existing implementation means everything must be built from scratch, though the encoding crate provides foundational data type support for packed decimal and zoned decimal fields.

## Reference Documentation

- Software AG ADABAS Concepts and Facilities — https://documentation.softwareag.com/adabas/ada852mfr/concepts.htm
- Software AG ADABAS Command Reference — https://documentation.softwareag.com/adabas/ada852mfr/comref/cmdover.htm
- Software AG ADABAS Commands — https://documentation.softwareag.com/adabas/ada852mfr/comref/commands.htm
- ADABAS S1/S2/S4 Commands — https://documentation.softwareag.com/adabas/ada842mfr/comref/s1s2s4.htm
- ADABAS L1/L4 Commands — https://documentation.softwareag.com/adabas/ada854mfr/comref/l1l4.htm
- ADABAS A1 Command — https://documentation.softwareag.com/adabas/ada852mfr/comref/a1.htm
- ADABAS RC Command — https://documentation.softwareag.com/adabas/ada842mfr/comref/rc.htm
- ADABAS FDT Record Structure — https://documentation.softwareag.com/adabas/ada671luw/basics/fdtrec.htm
- ADABAS Design — https://documentation.softwareag.com/adabas/ada744mfr/adamf/concepts/cfdesign.htm
- ADABAS Using Adabas — https://documentation.softwareag.com/adabas/ada814mfr/adamf/concepts/cfusing.htm
- ADABAS Utilities — https://documentation.softwareag.com/adabas/azp843/concepts/cfutil.htm
- ADAINV Utility — https://documentation.softwareag.com/adabas/ada67luw/utils/inv.htm
- Wikipedia: ADABAS — https://en.wikipedia.org/wiki/ADABAS
