# open-mainframe-dataset

Mainframe-compatible dataset and file I/O for the OpenMainframe z/OS clone. This crate provides the full spectrum of z/OS data management services: sequential access methods (QSAM, BSAM), partitioned datasets (PDS/PDSE), VSAM clusters (KSDS, ESDS, RRDS, LDS) with B+ tree indexing and alternate indexes, generation data groups (GDG), integrated catalog facility (ICF), IDCAMS utility services, DFSMSdss dump/restore, DFSMShsm hierarchical storage management, DFSMSrmm tape management, and SMS storage class constructs.

## Overview

The `open-mainframe-dataset` crate is the storage foundation of the OpenMainframe project. It emulates the z/OS data management layer — the subsystem responsible for how datasets are organized, accessed, cataloged, and managed across their lifecycle.

On real z/OS, dataset I/O is mediated through access methods (QSAM for record-level sequential, BSAM for block-level, VSAM for keyed/indexed/relative), with metadata tracked in the Integrated Catalog Facility (ICF) and storage policy enforced by the Storage Management Subsystem (SMS). This crate implements all of these layers, mapping mainframe concepts to a POSIX filesystem while preserving the programming model that COBOL, PL/I, and assembler programs expect.

The crate also implements the major z/OS data management utilities: IDCAMS for VSAM and catalog administration, ADRDSSU (DFSMSdss) for dataset dump/copy/restore, DFHSM for tiered storage migration and backup, and DFSMSrmm for tape volume lifecycle management. Together, these provide a complete data management stack that other crates (COBOL runtime, JCL, JES2, CICS) depend on for dataset allocation, I/O, and catalog services.

## Architecture

```
┌────────────────────────────────────────────────────────────────────┐
│                        Public API (lib.rs)                        │
├─────────┬──────────┬──────────┬────────────────┬──────────────────┤
│  Access │  VSAM    │  Catalog │   Utilities    │  Storage Mgmt   │
│ Methods │ Cluster  │  System  │                │                 │
├─────────┼──────────┼──────────┼────────────────┼──────────────────┤
│ qsam    │ ksds     │ catalog  │ idcams/        │ sms             │
│ bsam    │ esds     │ icf      │   mod.rs       │ acs             │
│ pds     │ rrds     │ gdg/     │   parser.rs    │ space           │
│ pdse    │ lds      │ gdg_icf  │   commands.rs  │ hsm             │
│         │ btree    │          │ dss            │ rmm             │
│         │ cluster  │          │                │ locking         │
│         │ aix      │          │                │                 │
│         │ freespace│          │                │                 │
│         │ spanned  │          │                │                 │
├─────────┴──────────┴──────────┴────────────────┴──────────────────┤
│  types.rs  │  error.rs  │  open-mainframe-encoding (EBCDIC)      │
└────────────┴────────────┴─────────────────────────────────────────┘
```

### Module Structure

| Module | Lines | Description |
|--------|------:|-------------|
| `qsam` | 485 | Record-level sequential I/O (Fixed, Variable, Line formats) |
| `bsam` | 735 | Block-level I/O and BPAM PDS directory block access |
| `pds` | 955 | Partitioned Data Set member management with ISPF statistics |
| `pdse` | 597 | PDSE extensions: Program Objects, Member Generations, EATTR |
| `vsam/mod` | 46 | VSAM module root and re-exports |
| `vsam/ksds` | 1,063 | Key-Sequenced Data Set with B+ tree index |
| `vsam/esds` | 470 | Entry-Sequenced Data Set (append-only, RBA access) |
| `vsam/rrds` | 669 | Relative Record Data Set (slot-based) |
| `vsam/lds` | 416 | Linear Data Set (4 KB page I/O for DB2 tablespaces) |
| `vsam/btree` | 638 | In-memory B+ tree for KSDS indexing |
| `vsam/cluster` | 560 | VSAM cluster definition, 128-byte header, lifecycle |
| `vsam/aix` | 378 | Alternate Index support for secondary access paths |
| `vsam/freespace` | 492 | CI/CA free space management and split tracking |
| `vsam/spanned` | 429 | Spanned record support across Control Intervals |
| `catalog` | 694 | Dataset name-to-path catalog with wildcard listing |
| `icf` | 1,108 | ICF master/user catalogs, BCS, VVDS, EXAMINE, DIAGNOSE |
| `gdg/mod` | 29 | GDG module root |
| `gdg/base` | 883 | GDG base definition with binary catalog format |
| `gdg/generation` | 263 | Generation number representation (GxxxxVyy) |
| `gdg_icf` | 530 | GDG integration with ICF catalog system |
| `idcams/mod` | 1,897 | IDCAMS command execution engine with LASTCC/MAXCC |
| `idcams/parser` | 1,136 | IDCAMS control statement parser with continuations |
| `idcams/commands` | 246 | IDCAMS command type definitions (19 command variants) |
| `dss` | 1,033 | DFSMSdss DUMP/RESTORE/COPY/PRINT with filtering |
| `hsm` | 1,680 | DFSMShsm migration, recall, backup, ABARS |
| `rmm` | 591 | DFSMSrmm tape volume lifecycle and VRS policies |
| `sms` | 991 | SMS constructs: DataClass, StorageClass, ManagementClass |
| `acs` | 1,072 | ACS routine interpreter for automatic class selection |
| `space` | 835 | Space allocation, extents, VTOC, multi-volume datasets |
| `locking` | 537 | Record-level and dataset-level locking (DISP semantics) |
| `types` | 359 | Core types: RecordFormat, DatasetOrg, DatasetAttributes |
| `error` | 126 | DatasetError with 13 variants |
| **Total** | **~21,000** | |

## Key Types and Traits

### Core Data Types (`types.rs`)

- **`RecordFormat`** — Record format enum: `F` (Fixed), `FB` (Fixed Blocked), `V` (Variable), `VB` (Variable Blocked), `U` (Undefined), `VS` (Variable Spanned), `VBS` (Variable Blocked Spanned). Methods: `is_fixed()`, `is_variable()`, `is_blocked()`, `is_spanned()`.
- **`DatasetOrg`** — Dataset organization: `PS` (Physical Sequential), `PO` (Partitioned), `DA` (Direct Access), `IS` (Indexed Sequential), `ESDS`, `KSDS`, `RRDS`.
- **`DatasetAttributes`** — DCB-equivalent: `recfm`, `lrecl`, `blksize`, `dsorg`. Factory methods: `cobol_source()` (FB/80/27920), `print_file()` (VBA/133/27998).
- **`DatasetRef`** — Resolved dataset reference with path, attributes, member name, and PDS flag.
- **`Disposition`**, **`DispAction`**, **`DispSpec`** — JCL DISP parameter model (status + normal/abnormal actions).

### Access Methods

- **`QsamReader`** / **`QsamWriter`** — Record-level sequential I/O. Supports Fixed (space-padded to LRECL), Variable (4-byte RDW prefix), and Line (text with newlines) formats.
- **`BsamReader`** / **`BsamWriter`** — Block-level I/O with configurable block size.
- **`BpamDirEntry`** / **`Ttr`** — BPAM directory entry with Track-Track-Record pointers for PDS directory block access.

### Partitioned Datasets

- **`Pds`** — Partitioned Data Set with JSON-backed directory. Operations: `add_member`, `read_member`, `update_member`, `delete_member`, `rename_member`, `add_alias`.
- **`PdsMember`** / **`IspfStats`** — Directory entry with optional ISPF statistics (version, line counts, user ID).
- **`Pdse`** — Extended PDS with `ProgramObject` (AMODE/RMODE), `MemberGeneration` (MAXGENS version history), and `Eattr` (extended attributes for large members/directories).

### VSAM

- **`Ksds`** — Key-Sequenced Data Set. Uses `BPlusTree<Vec<u8>, u64>` for O(log n) key lookups. Supports `read_key`, `read_key_generic` (prefix match), `write`, `rewrite`, `delete`, and sequential access via `start`/`read_next`.
- **`Esds`** — Entry-Sequenced Data Set. Append-only with RBA (Relative Byte Address) access. Variable-length records with 4-byte length prefix.
- **`Rrds`** — Relative Record Data Set. Fixed-size slots with 1-byte status headers. Supports sparse slot allocation and sequential traversal skipping empty slots.
- **`Lds`** — Linear Data Set. 4,096-byte page I/O with auto-extending file. Used by DB2 for tablespace storage.
- **`BPlusTree<K, V>`** — Generic B+ tree with configurable order (default 100). Supports insert, get, remove, range queries, and ordered iteration.
- **`AlternateIndex`** — Secondary access path mapping alternate keys to lists of primary keys. Supports unique and non-unique keys.
- **`VsamCluster`** / **`ClusterParams`** / **`KeySpec`** — Cluster lifecycle management with 128-byte binary file headers.
- **`FreeSpaceManager`** / **`FreeSpaceConfig`** — CI/CA free space reservation and split tracking per `FREESPACE(ci_pct, ca_pct)`.
- **`SpannedRecordManager`** / **`Segment`** — Records spanning multiple Control Intervals via 16-byte Segment Descriptor Words.
- **`FileStatus`** — 14-variant VSAM file status codes (00, 10, 21, 22, 23, 35, etc.) matching IBM COBOL file status conventions.

### Catalog System

- **`Catalog`** — Simple dataset name-to-path resolver with DSN-to-filesystem mapping, wildcard listing, and persistence to `.catalog.idx`.
- **`IcfCatalogSystem`** — Full ICF implementation with master catalog, user catalogs, alias routing, BCS entries, and VVDS per-volume tracking. Includes `examine()` and `diagnose()` for integrity checking.
- **`GdgBase`** — Generation Data Group with binary catalog format, rolloff, and relative generation resolution.
- **`GdgIcfManager`** — GDG management through the ICF catalog system.

### Utilities

- **`Idcams`** — 19-command IDCAMS executor with LASTCC/MAXCC tracking, IF/THEN/SET support, and continuation line parsing.
- **`Dss`** — DFSMSdss with DUMP (RLE compression), RESTORE (rename/replace), COPY, and PRINT.
- **`Hsm`** — DFSMShsm with three-tier migration (ML0/ML1/ML2), HRECALL, HBACKUP (versioned), HRECOVER, auto-migration, and ABARS aggregate backup.
- **`Rmm`** — DFSMSrmm tape volume lifecycle: scratch/private/retained states, VRS retention policies, and VRSEL batch recycling.

### Storage Management

- **`SmsConfiguration`** / **`ActiveConfiguration`** — SMS SCDS/ACDS with `DataClass`, `StorageClass`, `ManagementClass`, `StorageGroup` constructs and validation.
- **`AcsEngine`** / **`AcsRoutine`** — ACS routine interpreter: parser, FILTLIST pattern matching (`*`/`**` wildcards), SELECT/WHEN/SET execution.
- **`SpaceAllocation`** / **`ExtentList`** / **`Vtoc`** — 3390-geometry space allocation, extent tracking (max 16), VTOC with first-fit allocation and defragmentation.
- **`LockManager`** / **`DatasetLockManager`** — Record-level and dataset-level locking with stale detection, DISP mapping, and shared/exclusive compatibility.

## Implementation Details

### Record Formats and Access Methods

QSAM implements three record handling strategies:
- **Fixed (F/FB)**: Records are exactly LRECL bytes, space-padded on write, read as fixed-size blocks.
- **Variable (V/VB)**: Each record is prefixed with a 4-byte Record Descriptor Word (RDW) containing a 2-byte big-endian length (including the RDW itself) and 2 reserved bytes. Maximum record length is 65,535 bytes.
- **Line**: Text-oriented mode where records are delimited by newlines, optionally padded to LRECL on read.

BSAM operates at the block level, reading and writing physical blocks of `blksize` bytes. The last block in a dataset may be shorter (short block). BPAM extends BSAM for PDS directory access using 256-byte directory blocks with 2-byte used-length prefixes and 12-byte entries (8-byte name + 3-byte TTR + 1-byte indicator).

### VSAM Internals

The VSAM subsystem uses a 128-byte binary header on each cluster file:

| Bytes | Field | Description |
|-------|-------|-------------|
| 0–3 | Magic | `KSDS`, `ESDS`, or `RRDS` |
| 4–5 | Version | Header version (1) |
| 8–15 | Record count | u64 little-endian |
| 16–19 | Record size | u32 little-endian |
| 20–23 | CI size | u32 little-endian |
| 24–31 | Key offset/length | KSDS only |

**KSDS** maintains an in-memory B+ tree (`BPlusTree<Vec<u8>, u64>`) mapping keys to file offsets. On `open()`, the entire file is scanned to rebuild the index. The B+ tree uses a configurable order (default 100) with leaf-only data storage and automatic splitting/rebalancing. Sequential reads collect all keys, sort them, and iterate.

**ESDS** stores variable-length records as 4-byte little-endian length prefix + data. RBAs point to the length prefix position. Records are append-only.

**RRDS** uses fixed-size slots with a 1-byte status header (`0x00` = empty, `0x01` = occupied). Slot numbers are 1-based. The file is zero-extended when writing beyond the current highest slot.

**LDS** provides 4,096-byte page I/O with a 4,096-byte metadata header page containing magic (`LDS\0`) and page count. Pages auto-extend on write.

### Free Space and Spanned Records

The `FreeSpaceManager` implements `FREESPACE(ci_pct, ca_pct)`:
- CI-level: Each Control Interval reserves `ci_pct%` of capacity for future inserts.
- CA-level: The last `ca_pct%` of CIs in each 16-CI Control Area are set to zero capacity.
- CI splits move half the records to a new CI. CA splits add a full 16-CI Control Area.

Spanned records use 16-byte Segment Descriptor Words (SDW) containing next-offset, segment number, total segments, and data length. Records exceeding a single CI are split into segments of `ci_size - 16` bytes each.

### Catalog Architecture

The ICF catalog system implements the z/OS three-level hierarchy:
1. **Master Catalog** — contains user catalog connectors and aliases
2. **User Catalogs** — contain BCS entries for datasets
3. **VVDS** (per volume) — tracks VSAM and non-VSAM records per volume

Lookup follows z/OS search order: extract HLQ → check aliases in master → search target user catalog → search master → fall back to all user catalogs. `EXAMINE` validates BCS structural integrity (cluster component references, alias targets). `DIAGNOSE` performs bidirectional BCS-VVDS synchronization checking.

### GDG Naming and Rolloff

GDG generation names follow the format `BASE.GxxxxVyy` where `xxxx` is the generation number (1–9999) and `yy` is the version (0–99). The `GdgBase` maintains a binary catalog file with a 64-byte header (magic `GDG1`) followed by variable-length generation entries. When generation count exceeds the limit, the oldest generations roll off — in scratch mode, the physical files are deleted.

### IDCAMS Command Processing

The IDCAMS engine supports 19 commands with a full parser handling continuation lines (trailing `-`), comments (`/*`, `*`), and nested parenthesized parameters. The execution engine tracks LASTCC (per-command return code) and MAXCC (session maximum). The `IF LASTCC=12 THEN SET MAXCC=0` pattern is supported for suppressing expected errors from duplicate defines.

### HSM Three-Tier Storage

DFSMShsm implements ML0 (primary DASD) → ML1 (secondary) → ML2 (archive) migration using RLE compression. Age-based auto-migration moves datasets from ML0 after `primary_age_days` (default 30) and from ML1 after `secondary_age_days` (default 90). Backup uses versioned copies with configurable retention. ABARS provides aggregate backup/recovery using pattern-matched dataset groups.

### SMS and ACS

The SMS configuration models four construct types (DataClass, StorageClass, ManagementClass, StorageGroup) with validation rules. The SCDS (Source Control Data Set) must pass validation before activation into an ACDS (Active Configuration). The ACS interpreter parses and executes routines that automatically assign SMS classes at dataset allocation time, using FILTLIST pattern matching with `*` (single qualifier) and `**` (zero or more qualifiers) wildcards.

## Feature Coverage

### Access Methods

| Feature | Status |
|---------|--------|
| QSAM sequential read/write | Implemented |
| Fixed record format (F/FB) | Implemented |
| Variable record format (V/VB) with RDW | Implemented |
| Undefined record format (U) | Implemented |
| Line/text record mode | Implemented |
| BSAM block-level I/O | Implemented |
| BPAM PDS directory blocks | Implemented |

### Partitioned Datasets

| Feature | Status |
|---------|--------|
| PDS create/open | Implemented |
| Member add/read/update/delete/rename | Implemented |
| Alias support | Implemented |
| ISPF statistics | Implemented |
| PDSE Program Objects (AMODE/RMODE) | Implemented |
| PDSE Member Generations (MAXGENS) | Implemented |
| PDSE Extended Attributes (EATTR) | Implemented |

### VSAM

| Feature | Status |
|---------|--------|
| KSDS with B+ tree index | Implemented |
| KSDS generic/prefix key read | Implemented |
| KSDS sequential access (START/READ NEXT) | Implemented |
| ESDS append and RBA access | Implemented |
| RRDS slot-based access | Implemented |
| LDS 4 KB page I/O | Implemented |
| Alternate Index (unique and non-unique) | Implemented |
| VSAM Path definitions | Implemented |
| Free space management (CI/CA splits) | Implemented |
| Spanned record support (VS/VBS) | Implemented |
| CI/CA split statistics | Implemented |
| File status codes (14 variants) | Implemented |
| VSAM record-level sharing | Stub / Not yet |

### Catalog System

| Feature | Status |
|---------|--------|
| Simple name-to-path catalog | Implemented |
| Wildcard dataset listing | Implemented |
| Catalog persistence (.catalog.idx) | Implemented |
| ICF master/user catalogs | Implemented |
| BCS entries (9 types) | Implemented |
| VVDS per-volume tracking | Implemented |
| Alias routing | Implemented |
| EXAMINE (BCS integrity) | Implemented |
| DIAGNOSE (BCS-VVDS sync) | Implemented |
| GDG base with binary catalog | Implemented |
| GDG generation rolloff | Implemented |
| GDG relative resolution (0, -1, +1) | Implemented |
| GDG via ICF catalog | Implemented |

### IDCAMS Commands

| Command | Status |
|---------|--------|
| DEFINE CLUSTER (KSDS/ESDS/RRDS) | Implemented |
| DEFINE GDG | Implemented |
| DEFINE AIX | Implemented |
| DEFINE PATH | Implemented |
| DEFINE NONVSAM | Implemented |
| DEFINE ALIAS | Implemented |
| DELETE | Implemented |
| ALTER (NEWNAME, ADDVOLUMES, FREESPACE) | Implemented |
| LISTCAT (ENTRIES, LEVEL, ALL) | Implemented |
| PRINT (HEX, CHARACTER, SKIP, COUNT) | Implemented |
| REPRO (FROMKEY, TOKEY, SKIP, COUNT) | Implemented |
| VERIFY | Implemented |
| BLDINDEX | Implemented |
| EXPORT / IMPORT | Implemented |
| EXAMINE | Implemented |
| DIAGNOSE | Implemented |
| SET MAXCC | Implemented |
| IF/THEN (LASTCC/MAXCC conditions) | Implemented |

### Storage Management

| Feature | Status |
|---------|--------|
| SMS DataClass / StorageClass / ManagementClass | Implemented |
| StorageGroup (Pool, VIO, Dummy, Tape) | Implemented |
| SCDS validation and ACDS activation | Implemented |
| ACS routine parsing and execution | Implemented |
| ACS FILTLIST pattern matching | Implemented |
| ACS SELECT/WHEN/SET | Implemented |
| Space allocation (Tracks, Cylinders, Bytes) | Implemented |
| Extent tracking (max 16) with secondary allocation | Implemented |
| VTOC with first-fit allocation | Implemented |
| VTOC defragmentation | Implemented |
| Multi-volume datasets | Implemented |
| 3390 geometry constants | Implemented |
| DFSMSdss DUMP with RLE compression | Implemented |
| DFSMSdss RESTORE with rename/replace | Implemented |
| DFSMSdss COPY with filtering | Implemented |
| DFSMShsm HMIGRATE (ML0→ML1→ML2) | Implemented |
| DFSMShsm HRECALL | Implemented |
| DFSMShsm HBACKUP (versioned) | Implemented |
| DFSMShsm HRECOVER | Implemented |
| DFSMShsm auto-migration (age-based) | Implemented |
| DFSMShsm ABARS aggregate backup | Implemented |
| DFSMSrmm tape volume lifecycle | Implemented |
| DFSMSrmm VRS retention policies | Implemented |
| DFSMSrmm VRSEL batch recycling | Implemented |

### Locking

| Feature | Status |
|---------|--------|
| Dataset-level locking (DISP semantics) | Implemented |
| Record-level locking (VSAM-style) | Implemented |
| Shared/Exclusive compatibility matrix | Implemented |
| Stale lock detection and purge | Implemented |
| Re-entrant locking (same owner) | Implemented |

## Usage Examples

### Sequential I/O with QSAM

```rust
use open_mainframe_dataset::{QsamReader, QsamWriter, OpenMode, DatasetRef, DatasetAttributes};

// Write records
let attrs = DatasetAttributes::cobol_source(); // FB/80/27920
let dataset = DatasetRef::new("MY.DATA.FILE", "/path/to/file", attrs);
let mut writer = QsamWriter::open(dataset.clone(), OpenMode::Output)?;
writer.write(b"HELLO WORLD")?;
writer.flush()?;

// Read records
let mut reader = QsamReader::open(dataset)?;
while let Some(record) = reader.read()? {
    println!("Record {}: {} bytes", reader.record_number(), record.len());
}
```

### VSAM KSDS Operations

```rust
use open_mainframe_dataset::{Ksds, ClusterParams};

// Create a KSDS cluster
let params = ClusterParams::ksds("MY.KSDS.FILE", 100, 0, 8);
let mut ksds = Ksds::from_params(params)?;
ksds.create()?;

// Write records
let record = b"KEY00001Some data here...";
ksds.write(record);

// Random read by key
let result = ksds.read_key(b"KEY00001");
if result.status.is_success() {
    println!("Found: {:?}", result.value);
}

// Sequential browse
ksds.start(b"KEY00001");
while let result = ksds.read_next() {
    if !result.status.is_success() { break; }
    println!("Record: {:?}", result.value.unwrap());
}
```

### PDS Member Management

```rust
use open_mainframe_dataset::Pds;

let mut pds = Pds::create("/path/to/my.pds")?;
pds.add_member("MAIN", b"Program source code...")?;
pds.add_alias("MAINPGM", "MAIN")?;

let content = pds.read_member("MAINPGM")?;  // Resolves alias -> MAIN
for member in pds.list_members() {
    println!("{} (alias: {:?})", member.name, member.alias_of);
}
```

### IDCAMS

```rust
use open_mainframe_dataset::Idcams;

let mut idcams = Idcams::new("/datasets");
let result = idcams.execute(r#"
    DEFINE CLUSTER -
        (NAME(MY.VSAM.FILE) -
         INDEXED -
         KEYS(8 0) -
         RECORDSIZE(100 100))
    IF LASTCC = 12 THEN SET MAXCC = 0
"#)?;
println!("RC={}, Output:\n{}", result.return_code, result.output);
```

### DFSMShsm Migration

```rust
use open_mainframe_dataset::{Hsm, HsmConfig};

let config = HsmConfig::new("/storage");
let mut hsm = Hsm::new(config)?;
hsm.register_dataset("USER.DATA.FILE", "VOL001")?;
hsm.hmigrate("USER.DATA.FILE", None)?;  // Migrate to next lower tier
hsm.hrecall("USER.DATA.FILE")?;         // Recall back to ML0
```

### SMS and ACS

```rust
use open_mainframe_dataset::{
    SmsConfiguration, DataClass, StorageClass, AcsEngine, AcsContext, AcsRoutine,
};

let mut config = SmsConfiguration::new();
config.add_data_class(DataClass::new("STANDARD").with_recfm(RecordFormat::FB).with_lrecl(80));
config.add_storage_class(StorageClass::new("FAST").with_guaranteed_space(true));
let active = config.activate()?;

let mut engine = AcsEngine::new();
// Add routines...
let ctx = AcsContext::new("USER.PROD.DATA");
let result = engine.execute(&ctx);
println!("DATACLAS={:?} STORCLAS={:?}", result.dataclas, result.storclas);
```

## Dependencies

| Dependency | Purpose |
|------------|---------|
| `open-mainframe-encoding` | EBCDIC ↔ ASCII conversion for mainframe-compatible I/O |
| `miette` | Diagnostic error reporting |
| `thiserror` | Error type derive macros |

## Testing

Run the full test suite:

```bash
cargo test -p open-mainframe-dataset
```

The crate includes approximately 200 unit tests organized by module:

- **QSAM/BSAM**: Fixed, variable, and line record read/write; short blocks; convenience functions
- **PDS/PDSE**: Member CRUD, aliases, ISPF stats, program objects, member generations
- **VSAM**: KSDS key operations, ESDS append/RBA, RRDS slot management, LDS page I/O, B+ tree (10,000-record stress test), alternate indexes, free space CI/CA splits, spanned records
- **Catalog/ICF**: Name resolution, wildcard matching, ICF search order, EXAMINE/DIAGNOSE, 20-dataset integration scenario
- **GDG**: Creation, rolloff, relative resolution, ICF integration
- **IDCAMS**: All 19 command types, IF/THEN/SET, continuation lines, 27 tests
- **DSS**: DUMP/RESTORE/COPY with filtering, serialization roundtrips
- **HSM**: Migration/recall cycles, auto-migration, RLE compression, ABARS, 30 tests
- **RMM**: Volume lifecycle, VRS policies, 1000-volume VRSEL
- **SMS/ACS**: Construct validation, ACS routine execution, FILTLIST pattern matching
- **Space/Locking**: 3390 geometry, extent allocation, VTOC defrag, lock compatibility

All tests use temporary directories and are self-contained.

## Limitations and Future Work

- **No persistent VSAM index**: The KSDS B+ tree is rebuilt in memory on each `open()` by scanning the entire file. A persistent on-disk index would improve startup for large clusters.
- **No VSAM record-level sharing (RLS)**: Cross-address-space VSAM sharing is not yet implemented.
- **KSDS delete does not reclaim space**: Deleted records leave gaps in the data file. A compaction/reorganization pass would reclaim space.
- **No VSAM LSR/GSR buffering**: Local Shared Resources and Global Shared Resources buffer pools are not implemented.
- **No SMS auto-class assignment integration**: ACS routines execute standalone but are not automatically invoked during dataset allocation.
- **GDG EMPTY/NOEMPTY partially modeled**: The NOEMPTY flag is parsed but rolloff behavior does not differentiate between EMPTY and NOEMPTY modes.
- **RMM Cycles retention is a stub**: `RetentionType::Cycles` always returns true; actual cycle-based expiration is not tracked.
- **No EXCP-level I/O**: Execute Channel Program access method is not emulated.
- **LDS does not integrate with DB2**: The Linear Data Set is functional but not wired into the DB2 crate's tablespace layer.
- **IDCAMS MODAL commands**: Only IF/THEN is supported; DO/END loops and ELSE are not implemented.
