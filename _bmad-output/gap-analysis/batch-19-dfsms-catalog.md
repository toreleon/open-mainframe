# Gap Analysis: DFSMS + ICF Catalogs

## Official Specification Summary

DFSMS (Data Facility Storage Management Subsystem) is the central storage management infrastructure for z/OS, comprising several functional components that automate the lifecycle of data — from allocation and cataloging through backup, migration, and eventual expiration. DFSMS encompasses the Storage Management Subsystem (SMS) policy engine, ICF Catalogs for dataset location, DFSMShsm for hierarchical storage management, DFSMSdss for data movement utilities, and DFSMSrmm for tape management.

DFSMS is classified as **Core** — every z/OS installation uses DFSMS for dataset management:
- **SMS (Storage Management Subsystem)**: Policy engine that assigns data class, storage class, management class, and storage group via Automatic Class Selection (ACS) routines
- **ICF Catalogs**: Two-component catalog system — BCS (Basic Catalog Structure) for dataset name→volume mapping and VVDS (VSAM Volume Data Set) for volume-level extent/attribute information
- **DFSMShsm**: Hierarchical Storage Manager — automatic migration (ML0→ML1→ML2), recall, backup/recovery, ABARS disaster recovery
- **DFSMSdss**: Data Set Services — DUMP/RESTORE/COPY/PRINT with concurrent copy, FlashCopy, logical/physical modes
- **DFSMSrmm**: Removable Media Manager — tape volume lifecycle, VRS (Vital Record Specifications) retention policies, scratch/private pools
- **IDCAMS**: Access Method Services utility — DEFINE, DELETE, ALTER, LISTCAT, REPRO, PRINT, VERIFY, EXAMINE, DIAGNOSE
- **GDG**: Generation Data Groups — versioned dataset families with rolloff management
- **PDSE**: Partitioned Data Set Extended — program objects, member generations (z/OS 2.1+)

Key documentation:
- **z/OS DFSMS Implementing System-Managed Storage** (SC26-7407) — SMS concepts, ACS routines, constructs
- **z/OS DFSMS Managing Catalogs** (SC23-6853) — ICF catalog structure, BCS, VVDS, maintenance
- **z/OS DFSMShsm Storage Administration** (SC23-6871) — migration, recall, backup, ABARS
- **z/OS DFSMSdss Storage Administration** (SC23-6868) — DUMP, RESTORE, COPY, FlashCopy
- **z/OS DFSMSrmm Managing and Using Removable Media** (SC23-6873) — tape management, VRS
- **z/OS DFSMS Access Method Services for Catalogs** (SC23-6854) — IDCAMS command reference
- **IBM Redbook: A Practical Guide to ICF Catalogs** (SG24-8262)

## Key Features & Capabilities

### 1. SMS (Storage Management Subsystem)

| Component | Description |
|-----------|-------------|
| Data Class | Default allocation attributes (RECFM, LRECL, BLKSIZE, space, RETPD, EXPDT) |
| Storage Class | Performance and availability goals (guaranteed space, accessibility, I/O priority) |
| Management Class | Lifecycle policies (backup frequency, migration age, retention, versions) |
| Storage Group | Pool of volumes (POOL, VIO, DUMMY, COPY, BACKUP, TAPE types) |
| ACS Routines | User-written scripts that assign classes and groups at allocation time |
| SCDS (Source Control Data Set) | Source definitions for SMS constructs |
| ACDS (Active Control Data Set) | Activated copy of SMS configuration |
| COMMDS (Communications Data Set) | Sysplex SMS config propagation |
| ISMF | Interactive Storage Management Facility — ISPF panels for managing constructs |
| z/OSMF Storage Management plugin | Modern REST API for SMS construct management (z/OS 3.2+) |

#### ACS Routine Language

| Feature | Description |
|---------|-------------|
| FILTLIST | Named filter lists for pattern matching |
| SELECT/WHEN/DO/END | Conditional logic |
| SET | Assign construct to &STESSION variable |
| &DSN | Dataset name system variable |
| &DSTYPE | Dataset type (VSAM, HFS, PDS, etc.) |
| &HLQ | High-level qualifier |
| &AESSION | Account info |
| &JOBNAME | Job name |
| WRITE | Debug output |
| Four routine types | DATACLAS, STORCLAS, MGMTCLAS, STORGRP |

### 2. ICF Catalogs

| Component | Description |
|-----------|-------------|
| Master Catalog | System-level catalog containing SYS1.* datasets and user catalog connectors |
| User Catalog | Application-level catalogs connected to master via aliases |
| BCS (Basic Catalog Structure) | KSDS containing dataset name → volume mapping, 45-byte key |
| VVDS (VSAM Volume Data Set) | ESDS on each volume with VVR/NVR records for extent/attribute data |
| VVR (VSAM Volume Record) | Describes VSAM data/index components on a volume |
| NVR (Non-VSAM Volume Record) | Describes SMS-managed non-VSAM datasets on a volume |
| VVCR (VSAM Volume Control Record) | Links VVDS to owning BCS |
| Catalog search order | Jobcat/stepcat (removed z/OS 2.3) → alias → master catalog |
| Alias | HLQ-to-user-catalog mapping in master catalog |
| Connector record | Master catalog entry pointing to user catalog |
| EXAMINE | BCS structural integrity check |
| DIAGNOSE | BCS-VVDS synchronization verification |
| VTOC synchronization | BCS + VVDS + VTOC must agree for dataset operations |

### 3. DFSMShsm (Hierarchical Storage Manager)

| Feature | Description |
|---------|-------------|
| ML0 (Level 0) | Active DASD — directly accessible datasets |
| ML1 (Migration Level 1) | Secondary DASD — compressed migrated data |
| ML2 (Migration Level 2) | Tape — further compressed migrated data |
| Automatic space management | HMIGRATE based on age/access frequency |
| Automatic secondary space mgmt | ML1→ML2 cleanup, expired dataset deletion |
| HRECALL | Recall migrated datasets to ML0 |
| HBACKUP / HRECOVER | Dataset backup and recovery |
| ABARS | Aggregate Backup and Recovery Support (disaster recovery) |
| CDS (Control Data Sets) | MCDS, BCDS, OCDS, Journal — HSM state tracking |
| Fast Subsequent Migration | Reconnect without data movement |
| Cloud ML2 | ML2 data to S3-compatible object storage via TS7700 |
| Management class integration | Backup frequency, migration age, retention from SMS management class |

### 4. DFSMSdss (Data Set Services)

| Feature | Description |
|---------|-------------|
| DUMP | Backup datasets or volumes to dump datasets |
| RESTORE | Recover from dump datasets |
| COPY | Move/copy datasets between volumes |
| PRINT | Print dataset contents |
| Logical dump | Dataset-level backup (SMS-aware, portable) |
| Physical dump | Track-level volume backup |
| Concurrent Copy | Point-in-time copy without quiescing applications |
| FlashCopy | Hardware-assisted instant copy (IBM DS8000) |
| SnapShot | Space-efficient point-in-time copy |
| INCLUDE/EXCLUDE | Dataset filtering with wildcards |
| Cross-memory API | ADRXMAIA for programmatic invocation |
| Volume operations | DEFRAG, COMPRESS, RELEASE |
| SMS conversion | Convert non-SMS to SMS-managed and vice versa |

### 5. DFSMSrmm (Removable Media Manager)

| Feature | Description |
|---------|-------------|
| VRS (Vital Record Specification) | Retention and movement policies |
| VRS types | Dataset VRS, Volume VRS, Name VRS |
| Retention methods | EXPDT, catalog status (WHILECATALOG), days since last ref, cycle, GDG |
| Scratch pool | Available tapes for new allocations |
| Private pool | Tapes with active data |
| VRSEL processing | Vital Record Selection — evaluates retention for all volumes |
| Tape library integration | ATL (Automated Tape Library) support |
| Volume lifecycle | Create → active → retained → scratch cycle |
| EDG_EXIT100 | Installation exit for new dataset retention |

### 6. IDCAMS (Access Method Services)

| Command | Description |
|---------|-------------|
| DEFINE CLUSTER | Create VSAM dataset (KSDS, ESDS, RRDS, LDS) |
| DEFINE AIX | Create alternate index |
| DEFINE PATH | Create path linking AIX to base cluster |
| DEFINE GDG | Create Generation Data Group base |
| DEFINE NONVSAM | Catalog a non-VSAM dataset |
| DEFINE ALIAS | Create HLQ alias to user catalog |
| DELETE | Remove catalog entries (with PURGE, FORCE, SCRATCH, NOSCRATCH) |
| ALTER | Modify dataset attributes (rename, add volumes, change parameters) |
| LISTCAT | Display catalog information (LEVEL, ALL, NAME, HISTORY, VOLUME) |
| REPRO | Copy data between datasets (with FROMKEY/TOKEY, FROMADDRESS/TOADDRESS, SKIP/COUNT) |
| PRINT | Display dataset contents (CHARACTER, HEX, DUMP) |
| VERIFY | Check and correct VSAM end-of-data pointers |
| EXAMINE | Check BCS structural integrity |
| DIAGNOSE | Compare BCS and VVDS for synchronization errors |
| EXPORT / IMPORT | Portable dataset copy with catalog entry |

### 7. GDG (Generation Data Groups)

| Feature | Description |
|---------|-------------|
| GDG Base | Named family with limit (1-255 generations) |
| Generation naming | DSN.GnnnnVmm format |
| Relative reference | (+1) new, (0) current, (-1) previous |
| Absolute reference | G0001V00 explicit |
| Rolloff | Oldest generation deleted/uncataloged when limit exceeded |
| SCRATCH option | Delete data on rolloff vs NOSCRATCH (uncatalog only) |
| EMPTY/NOEMPTY | Clear all generations on rolloff vs remove oldest only |
| Model DCB | Template for generation attributes |

### 8. PDSE (Partitioned Data Set Extended)

| Feature | Description |
|---------|-------------|
| Program objects | Compiled load modules with class-based structure |
| Member generations | Version history for members (z/OS 2.1+, MAXGENS parameter) |
| Extended attributes | EATTR=OPT for large format, > 65535 members |
| Directory | 15-byte key, user data extension, no TTR limit |
| Concurrent access | Multiple concurrent writers to different members |
| Load module format | AMODE/RMODE attributes, binder output |

## Current OpenMainframe Status

The open-mainframe-dataset crate (11,851 lines of code) contains significant implementations of core data access functionality, with strong VSAM, GDG, catalog, and PDS/PDSE support. However, SMS policy management, ICF catalog infrastructure, and all DFSMShsm/dss/rmm components are missing.

### What IS Implemented

#### 1. Dataset Catalog
**File:** `crates/open-mainframe-dataset/src/catalog.rs` (435 lines)
- Hierarchical dataset catalog (DSN-to-path mapping)
- Wildcard pattern matching
- PDS member lookup
- CatalogEntry struct with explicit entries
- Extension-based attribute inference
- Filesystem scanning for discovery
- Persistent catalog index (.catalog.idx)
- 13 unit tests

#### 2. GDG (Generation Data Groups)
**Files:** `crates/open-mainframe-dataset/src/gdg/mod.rs`, `gdg/base.rs` (400+ lines), `gdg/generation.rs`
- GDG base creation with configurable options (limit, scratch, empty, noempty, model_dcb)
- Generation numbering in GxxxxVyy format
- Generation limit management (1-255)
- Scratch/delete on rolloff
- Relative reference support (0, -1, -2)
- Absolute reference support (G0001V00)
- Persistent binary catalog format (GDG1 magic)

#### 3. IDCAMS Utility
**Files:** `crates/open-mainframe-dataset/src/idcams/mod.rs` (400+ lines), `idcams/commands.rs` (135+ lines), `idcams/parser.rs` (400+ lines)
- DEFINE CLUSTER (KSDS, ESDS, RRDS) with key length, record size, CI size
- DEFINE GDG with limit, scratch/noscratch
- DELETE with PURGE, FORCE options
- ALTER (rename)
- LISTCAT with LEVEL, ALL options
- PRINT with CHARACTER, HEX, SKIP, COUNT
- REPRO with FROMKEY/TOKEY, SKIP/COUNT (parsed, partial execution)
- VERIFY (VSAM integrity check)
- DEFINE AIX / DEFINE PATH — command stubs exist, not functional
- Continuation line support, comment handling

#### 4. VSAM (Full Implementation)
**Files:** `crates/open-mainframe-dataset/src/vsam/*.rs` (10 files)
- KSDS with B+ tree indexing, O(log n) lookups
- ESDS with RBA (Relative Byte Address) access
- RRDS with slot-based storage and RRN access
- LDS (Linear Data Set) — file exists
- AIX definitions — struct exists, not functional
- Cluster parameters and creation
- Free space management (file exists)
- Spanned record support (file exists)

#### 5. PDS/PDSE
**File:** `crates/open-mainframe-dataset/src/pds.rs` (500+ lines)
- Member directory (JSON-based: _pds_directory.json)
- Member add, delete, rename, read, list
- ISPF statistics (created, modified, user_id, version, lines)
- Member aliases
- Member name validation (8-char max)

#### 6. Sequential Access (QSAM/BSAM)
**Files:** `crates/open-mainframe-dataset/src/qsam.rs`, `bsam.rs`
- QSAM: All record formats (F, FB, V, VB, U, VS, VBS), read/write/extend
- BSAM: Block-level I/O, PDS directory block parsing (TTR), member enumeration

#### 7. Dataset Types & Locking
**Files:** `crates/open-mainframe-dataset/src/types.rs` (359 lines), `locking.rs`
- 7 dataset organizations (Sequential, Partitioned, Direct, IndexedSequential, VSAM variants)
- 7 record formats with validation
- Disposition specs (New/Old/Shr/Mod, Keep/Delete/Catlg/Uncatlg/Pass)
- DatasetAttributes with RECFM, LRECL, BLKSIZE
- Lock manager with shared/exclusive modes

#### 8. JCL SMS Parameters (Parse-Only)
**File:** `crates/open-mainframe-jcl/src/ast/mod.rs:315-320`
```rust
pub storclas: Option<String>,
pub dataclas: Option<String>,
pub mgmtclas: Option<String>,
```
**File:** `crates/open-mainframe-jcl/src/parser/mod.rs:1571-1585`
- Parses STORCLAS, DATACLAS, MGMTCLAS from DD statements
- No processing or enforcement — parse only

## Gap Details

### SMS Policy Engine

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Data class definitions | Named class with default allocation attributes | DATACLAS parsed in JCL only | **Partial** |
| Storage class definitions | Named class with performance/availability goals | STORCLAS parsed in JCL only | **Partial** |
| Management class definitions | Named class with backup/migration/retention policies | MGMTCLAS parsed in JCL only | **Partial** |
| Storage group definitions | Named volume pool (POOL, VIO, DUMMY, COPY, BACKUP, TAPE) | None | **Missing** |
| ACS routine interpreter | Four routine types for automatic class assignment | None | **Missing** |
| ACS FILTLIST | Pattern matching filter lists | None | **Missing** |
| ACS system variables | &DSN, &DSTYPE, &HLQ, &JOBNAME, etc. | None | **Missing** |
| SCDS (Source Control Data Set) | SMS construct source definitions | None | **Missing** |
| ACDS (Active Control Data Set) | Activated SMS configuration | None | **Missing** |
| COMMDS | Sysplex SMS config propagation | None | **Missing** |
| ISMF panels | ISPF-based SMS administration | None | **Missing** |
| SMS construct validation | Consistency checking across classes/groups | None | **Missing** |

### ICF Catalog Structure

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Master catalog | System-level catalog with SYS1.* and user catalog connectors | Simple name→path catalog | **Partial** |
| User catalogs | Application-level catalogs | None | **Missing** |
| BCS (Basic Catalog Structure) | KSDS with 45-byte key, dataset name→volume mapping | Flat file index (.catalog.idx) | **Partial** |
| VVDS | ESDS per volume with VVR/NVR records | None | **Missing** |
| VVR (VSAM Volume Record) | VSAM component extent/attribute info | None | **Missing** |
| NVR (Non-VSAM Volume Record) | SMS non-VSAM dataset info | None | **Missing** |
| VVCR (Control Record) | Links VVDS to owning BCS | None | **Missing** |
| Alias definitions | HLQ-to-user-catalog mapping | None | **Missing** |
| Connector records | Master→user catalog links | None | **Missing** |
| Catalog search order | Alias → master → user catalog chain | None | **Missing** |
| EXAMINE | BCS structural integrity check | None | **Missing** |
| DIAGNOSE | BCS-VVDS synchronization verification | None | **Missing** |
| BCS-VVDS-VTOC synchronization | Three-way consistency | None | **Missing** |
| Catalog recovery | IMPORT CONNECT, EXPORT DISCONNECT | None | **Missing** |

### IDCAMS Utility

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| DEFINE CLUSTER | Create VSAM (KSDS/ESDS/RRDS/LDS) with full parameters | KSDS/ESDS/RRDS supported | **Present** |
| DEFINE GDG | Create GDG base with limit/scratch/empty | Fully implemented | **Present** |
| DEFINE AIX | Create alternate index | Command stub only | **Partial** |
| DEFINE PATH | Create AIX-to-cluster path | Command stub only | **Partial** |
| DEFINE NONVSAM | Catalog non-VSAM dataset | None | **Missing** |
| DEFINE ALIAS | Create HLQ alias | None | **Missing** |
| DELETE | Remove catalog entries | Implemented with PURGE, FORCE | **Present** |
| ALTER | Modify attributes | Rename supported | **Partial** |
| LISTCAT | Display catalog info | LEVEL and ALL supported | **Present** |
| REPRO | Copy data between datasets | Parsed but partial execution | **Partial** |
| PRINT | Display dataset contents | CHARACTER, HEX, SKIP, COUNT | **Present** |
| VERIFY | Check VSAM end-of-data | Implemented | **Present** |
| EXAMINE | BCS integrity check | None | **Missing** |
| DIAGNOSE | BCS-VVDS sync check | None | **Missing** |
| EXPORT / IMPORT | Portable dataset copy | None | **Missing** |

### DFSMShsm

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| ML0 → ML1 migration | Automatic space management, compress to DASD | None | **Missing** |
| ML1 → ML2 migration | Secondary space management, compress to tape | None | **Missing** |
| HRECALL | Recall migrated datasets to ML0 | None | **Missing** |
| HMIGRATE | Manual migration command | None | **Missing** |
| HBACKUP | Dataset backup | None | **Missing** |
| HRECOVER | Dataset recovery from backup | None | **Missing** |
| ABARS | Aggregate backup and recovery for disaster recovery | None | **Missing** |
| CDS (Control Data Sets) | MCDS, BCDS, OCDS, Journal | None | **Missing** |
| Fast Subsequent Migration | Reconnect without data movement | None | **Missing** |
| Age-based migration | Days since last reference triggers migration | None | **Missing** |
| Automatic secondary space mgmt | ML1 cleanup, expired deletion | None | **Missing** |
| Cloud ML2 | S3-compatible object storage for ML2 | None | **Missing** |
| Management class integration | Backup freq, migration age, retention from SMS | None | **Missing** |

### DFSMSdss

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| DUMP (logical) | Dataset-level backup | None | **Missing** |
| DUMP (physical) | Track-level volume backup | None | **Missing** |
| RESTORE (logical) | Dataset-level recovery | None | **Missing** |
| RESTORE (physical) | Track-level volume recovery | None | **Missing** |
| COPY | Dataset/volume copy between volumes | None | **Missing** |
| PRINT | Dataset content display | None (IDCAMS PRINT exists) | **Partial** |
| Concurrent Copy | Point-in-time copy without quiescing | None | **Missing** |
| FlashCopy | Hardware-assisted instant copy | None | **Missing** |
| SnapShot | Space-efficient point-in-time copy | None | **Missing** |
| INCLUDE/EXCLUDE | Dataset filtering with wildcards | None | **Missing** |
| Volume DEFRAG | Reduce free-space fragmentation | None | **Missing** |
| SMS conversion | Non-SMS ↔ SMS-managed conversion | None | **Missing** |
| Cross-memory API (ADRXMAIA) | Programmatic invocation | None | **Missing** |

### DFSMSrmm

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| VRS (Vital Record Specifications) | Dataset/volume/name retention policies | None | **Missing** |
| Scratch pool management | Available tape allocation | None | **Missing** |
| Private pool management | Active data tape tracking | None | **Missing** |
| VRSEL processing | Evaluate retention for all volumes | None | **Missing** |
| Retention methods | EXPDT, WHILECATALOG, cycle, days since ref, GDG | None | **Missing** |
| Tape library integration | ATL automation | None | **Missing** |
| Volume lifecycle | Create → active → retained → scratch | None | **Missing** |
| EDG_EXIT100 | Installation exit for retention | None | **Missing** |

### GDG (Generation Data Groups)

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| GDG base creation | Named family with limit | Implemented (gdg/base.rs) | **Present** |
| Generation naming (GxxxxVyy) | Standard format | Implemented | **Present** |
| Relative references (+1, 0, -1) | JCL DSN(+n) syntax | Implemented | **Present** |
| Absolute references | G0001V00 format | Implemented | **Present** |
| Rolloff management | Oldest deleted when limit exceeded | Implemented (scratch option) | **Present** |
| EMPTY/NOEMPTY | Clear all vs oldest on rolloff | Implemented | **Present** |
| Model DCB | Template for generation attributes | Implemented | **Present** |
| Catalog integration | GDG base in ICF catalog | Flat file (not ICF) | **Partial** |

### PDSE Advanced Features

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Basic PDS operations | Member CRUD, directory | Implemented (pds.rs) | **Present** |
| ISPF statistics | Created, modified, user, version, lines | Implemented | **Present** |
| Member aliases | Alternative member names | Implemented | **Present** |
| Member generations | Version history (z/OS 2.1+ MAXGENS) | None | **Missing** |
| Program objects | Compiled load module format | None | **Missing** |
| Extended attributes (EATTR) | Large format, > 65535 members | None | **Missing** |
| Load module AMODE/RMODE | Addressing mode attributes | None | **Missing** |

### Space & Allocation Management

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Primary/secondary allocation | Space in tracks/cylinders/bytes | None (host FS) | **Missing** |
| Guaranteed space | SMS storage class attribute | None | **Missing** |
| Extended addressability | > 4 GB datasets | None | **Missing** |
| Striped datasets | Multi-volume parallel I/O | None | **Missing** |
| Compressed datasets | Hardware/software compression | None | **Missing** |
| Multi-volume datasets | Span across multiple volumes | None | **Missing** |
| VTOC (Volume Table of Contents) | Volume-level dataset directory | None | **Missing** |

## Proposed Epic Structure

### DFSMS-100: SMS Construct Data Model
**Scope:** Define Rust data structures for SMS constructs — data class (allocation defaults), storage class (performance goals), management class (lifecycle policies), storage group (volume pool with type). Construct validation and serialization. SCDS/ACDS/COMMDS configuration model.
**Complexity:** L
**Rationale:** Four construct types with many attributes each. Must model the relationships between classes and groups.

### DFSMS-101: ACS Routine Interpreter
**Scope:** Implement the ACS routine language — FILTLIST pattern matching, SELECT/WHEN/DO/END conditional logic, SET assignment, system variables (&DSN, &DSTYPE, &HLQ, &JOBNAME, etc.). Four routine types (DATACLAS, STORCLAS, MGMTCLAS, STORGRP). Wire into dataset allocation flow.
**Complexity:** L
**Rationale:** Small but complete scripting language. Must integrate with dataset allocation in the JCL executor and IDCAMS DEFINE.

### DFSMS-102: ICF Catalog Infrastructure
**Scope:** Replace the flat-file catalog with ICF catalog structure — master catalog, user catalogs, BCS (KSDS-based with 45-byte key), VVDS (ESDS per volume). Implement alias definitions, connector records, catalog search order, EXAMINE/DIAGNOSE commands. BCS-VVDS-VTOC synchronization.
**Complexity:** XL
**Rationale:** Replaces the core catalog system. BCS is a KSDS (leverages existing VSAM), but the two-component architecture with VVR/NVR records and three-way synchronization is complex.

### DFSMS-103: IDCAMS Enhancements
**Scope:** Complete the IDCAMS implementation — functional DEFINE AIX with build, DEFINE PATH, DEFINE NONVSAM, DEFINE ALIAS, ALTER with full parameter support, REPRO with full execution (FROMADDRESS/TOADDRESS), EXAMINE, DIAGNOSE, EXPORT/IMPORT. Wire AIX maintenance into base cluster updates.
**Complexity:** M
**Rationale:** Most infrastructure exists. Main work is AIX build/maintenance (secondary index construction), EXAMINE/DIAGNOSE (catalog integrity), and EXPORT/IMPORT (portable dataset copy).

### DFSMS-104: DFSMShsm — Migration & Recall
**Scope:** Implement hierarchical storage management — ML0/ML1/ML2 storage hierarchy, automatic space management (age-based migration), HMIGRATE/HRECALL commands, compression for ML1/ML2 storage, management-class-driven migration policies, CDS (control datasets) for state tracking.
**Complexity:** L
**Rationale:** Migration/recall engine with tiered storage. Can map to hot/warm/cold storage tiers or object storage in cloud-native deployment.

### DFSMS-105: DFSMShsm — Backup & ABARS
**Scope:** Implement HBACKUP/HRECOVER for dataset-level backup/recovery. ABARS (Aggregate Backup and Recovery Support) for disaster recovery — aggregate definition, ABACKUP/ARECOVER commands. Incremental backup support. Management-class-driven backup frequency.
**Complexity:** L
**Rationale:** Backup/recovery adds to the migration engine. ABARS is a separate disaster recovery subsystem with aggregate grouping.

### DFSMS-106: DFSMSdss — DUMP/RESTORE/COPY
**Scope:** Implement DFSMSdss commands — logical DUMP/RESTORE for dataset-level backup, physical DUMP/RESTORE for volume-level, COPY for dataset movement, INCLUDE/EXCLUDE filtering, concurrent copy integration. Cross-memory API (ADRXMAIA) for programmatic use.
**Complexity:** L
**Rationale:** Data movement utility with multiple modes (logical/physical, dataset/volume) and filtering. Conceptually similar to tar/rsync but with SMS awareness.

### DFSMS-107: DFSMSrmm — Tape Management
**Scope:** Implement tape volume lifecycle — VRS retention policies (dataset/volume/name types), scratch/private pool management, VRSEL retention evaluation, retention methods (EXPDT, WHILECATALOG, cycle, days-since-ref, GDG), volume state machine (create→active→retained→scratch).
**Complexity:** M
**Rationale:** Policy engine for tape media. May be lower priority in cloud-native deployment where tape is replaced by object storage.

### DFSMS-108: PDSE Program Objects & Member Generations
**Scope:** Implement PDSE-specific features — program object format (binder output, class-based structure, AMODE/RMODE), member generations (version history, MAXGENS parameter), extended attributes (EATTR=OPT, >65535 members). Extend existing pds.rs.
**Complexity:** M
**Rationale:** Program objects require understanding of z/OS binder output format. Member generations extend the existing PDS implementation with versioning.

### DFSMS-109: Space & Volume Management
**Scope:** Implement space allocation (primary/secondary in tracks/cylinders/bytes/records), guaranteed space, multi-volume datasets, extended addressability (>4 GB), striped datasets, compressed datasets. VTOC (Volume Table of Contents) data structure. Volume-level operations (DEFRAG, COMPRESS, RELEASE).
**Complexity:** L
**Rationale:** Fundamental storage allocation model. Maps physical z/OS concepts (tracks, cylinders, extents) to cloud-native storage.

### DFSMS-110: GDG-to-ICF Catalog Integration
**Scope:** Wire existing GDG implementation into ICF catalog infrastructure — GDG base as catalog entry, generation catalog entries with proper aging, rolloff via catalog operations, LISTCAT GDG-specific output. Ensure IDCAMS DEFINE GDG creates proper ICF entries.
**Complexity:** S
**Rationale:** GDG core logic exists. Main work is rewiring from flat-file catalog to ICF catalog entries and ensuring rolloff uses catalog DELETE.

## Dependencies

| Epic | Depends On |
|------|-----------|
| DFSMS-100 | None (foundational data model) |
| DFSMS-101 | DFSMS-100 (ACS assigns constructs from SMS model) |
| DFSMS-102 | `open-mainframe-dataset` VSAM (BCS is a KSDS) |
| DFSMS-103 | DFSMS-102 (EXAMINE/DIAGNOSE need ICF catalog), existing IDCAMS |
| DFSMS-104 | DFSMS-100 (migration policy from management class), DFSMS-102 (catalog entries for migrated datasets) |
| DFSMS-105 | DFSMS-104 (backup extends migration engine), DFSMS-100 (backup policy from management class) |
| DFSMS-106 | DFSMS-102 (catalog-aware DUMP/RESTORE), DFSMS-100 (SMS conversion) |
| DFSMS-107 | DFSMS-100 (management class retention policies) |
| DFSMS-108 | Existing PDS implementation (pds.rs) |
| DFSMS-109 | DFSMS-102 (VTOC/volume model tied to catalog) |
| DFSMS-110 | DFSMS-102 (ICF catalog), existing GDG |

### Cross-Batch Dependencies

| Batch | Relationship |
|-------|-------------|
| Batch 8 — RACF | Dataset profile security for SMS-managed datasets |
| Batch 9 — TSO/ISPF | ISMF panels for SMS administration |
| Batch 11 — JES2 | Spool dataset management, output class → storage class |
| Batch 14 — SMF | SMF type 14/15 (dataset activity), type 42 (DB2 datasets) |
| Batch 17 — WLM | I/O priority from storage class |
| Batch 21 — Utilities | IEBCOPY, IEBGENER use dataset access methods |

## Complexity Estimate

| Epic | Complexity | Rationale |
|------|-----------|-----------|
| DFSMS-100 | L | Four construct types with many attributes, relationships, validation |
| DFSMS-101 | L | Small scripting language with conditional logic, pattern matching |
| DFSMS-102 | XL | Core catalog replacement — BCS/VVDS/VVR/NVR, three-way sync |
| DFSMS-103 | M | Extends existing IDCAMS — AIX build, EXAMINE, DIAGNOSE |
| DFSMS-104 | L | Tiered storage engine with automatic migration policies |
| DFSMS-105 | L | Backup/recovery and ABARS disaster recovery |
| DFSMS-106 | L | Multi-mode data movement utility with filtering |
| DFSMS-107 | M | Tape policy engine (lower priority in cloud-native) |
| DFSMS-108 | M | Program objects and member versioning for PDSE |
| DFSMS-109 | L | Physical space model mapped to cloud-native storage |
| DFSMS-110 | S | Wire existing GDG into new ICF catalog |

**Overall Complexity: XL** — 11 proposed epics (1×S, 3×M, 6×L, 1×XL). DFSMS is the storage infrastructure of z/OS. While the dataset crate has strong data access primitives (VSAM, GDG, PDS, QSAM), the policy management (SMS), catalog infrastructure (ICF), and lifecycle management (hsm/dss/rmm) components are entirely missing.

## Feature Count Summary

- **Total features analyzed:** 110+
- **Present:** 18 (VSAM KSDS/ESDS/RRDS, GDG full lifecycle, IDCAMS core commands, PDS operations, QSAM/BSAM, dataset types, locking)
- **Partial:** 10 (JCL SMS parameters parse-only, catalog as flat file, IDCAMS AIX/PATH stubs, REPRO partial, ALTER limited, GDG catalog not ICF)
- **Missing:** 82+ (SMS policy engine, ACS routines, ICF catalogs, DFSMShsm, DFSMSdss, DFSMSrmm, PDSE advanced, space management)

## Reference Documentation

- [z/OS DFSMS Implementing System-Managed Storage (SC26-7407)](https://www.ibm.com/docs/en/SSLTBW_2.4.0/pdf/idai600_v2r4.pdf)
- [z/OS DFSMSdfp Storage Administration (SC23-6860)](https://www.ibm.com/docs/en/SSLTBW_2.4.0/pdf/idas200_v2r4.pdf)
- [z/OS DFSMS Managing Catalogs (SC23-6853)](https://www.ibm.com/docs/en/SSLTBW_3.1.0/pdf/idac100_v3r1.pdf)
- [z/OS DFSMShsm Storage Administration (SC23-6871)](https://www.ibm.com/docs/en/SSLTBW_3.1.0/pdf/arcf000_v3r1.pdf)
- [z/OS DFSMShsm Primer (IBM Redbook SG24-5272)](https://www.redbooks.ibm.com/redbooks/pdfs/sg245272.pdf)
- [z/OS DFSMSdss Storage Administration (SC23-6868)](https://www.ibm.com/docs/en/SSLTBW_2.4.0/pdf/adru000_v2r4.pdf)
- [z/OS DFSMSrmm Managing and Using Removable Media (SC23-6873)](https://www.ibm.com/docs/en/SSLTBW_2.4.0/pdf/idarm00_v2r4.pdf)
- [IBM Redbook: A Practical Guide to ICF Catalogs (SG24-8262)](https://www.redbooks.ibm.com/redbooks/pdfs/sg248262.pdf)
- [z/OS DFSMS Access Method Services for Catalogs (SC23-6854)](https://www.ibm.com/docs/en/zos/2.5.0?topic=dfsms-access-method-services-catalogs)
- [z/OS Catalog Structure (IBM Docs)](https://www.ibm.com/docs/en/zos/2.4.0?topic=catalogs-catalog-structure)
- [Data Facility Storage Management Subsystem — Wikipedia](https://en.wikipedia.org/wiki/Data_Facility_Storage_Management_Subsystem_(MVS))
