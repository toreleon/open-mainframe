---
version: 'v1.1'
baseVersion: 'v1.0'
date: '2026-02-13'
status: 'draft'
inputDocuments: [prd-v1.1.md, architecture.md]
---

# Architecture Extension - zOS-clone v1.1: Batch Workload Ready

_This document extends the v1.0 MVP architecture with components required for v1.1 features._

## Architecture Overview

### v1.1 Component Summary

| Component | Location | New/Extended | Purpose |
|-----------|----------|--------------|---------|
| VSAM Engine | zos-dataset/vsam/ | **NEW** | B+ tree indexed file support |
| SORT Utility | zos-sort (new crate) | **NEW** | DFSORT-compatible sorting |
| GDG Manager | zos-dataset/gdg/ | **NEW** | Generation data group handling |
| IDCAMS | zos-dataset/idcams/ | **NEW** | Dataset management utility |
| Catalog Extension | zos-dataset/catalog | Extended | VSAM/GDG catalog entries |
| JCL Extension | zos-jcl | Extended | VSAM DD, GDG references |
| Package Build | CI/CD | **NEW** | apt/yum packaging |

### Dependency Graph Update

```
                    ┌─────────────────┐
                    │   zos-clone     │ (CLI binary)
                    │                 │
                    └───────┬─────────┘
                            │
        ┌───────────────────┼───────────────────┐
        │                   │                   │
        ▼                   ▼                   ▼
┌───────────────┐   ┌───────────────┐   ┌───────────────┐
│   zos-cobol   │   │    zos-jcl    │   │   zos-sort    │ ◄── NEW
│               │   │               │   │               │
└───────┬───────┘   └───────┬───────┘   └───────┬───────┘
        │                   │                   │
        └───────────────────┼───────────────────┘
                            │
                            ▼
                    ┌───────────────┐
                    │  zos-dataset  │ (extended)
                    │   ├── vsam/   │ ◄── NEW
                    │   ├── gdg/    │ ◄── NEW
                    │   └── idcams/ │ ◄── NEW
                    └───────┬───────┘
                            │
                ┌───────────┴───────────┐
                ▼                       ▼
        ┌───────────────┐       ┌───────────────┐
        │  zos-runtime  │       │ zos-encoding  │
        └───────────────┘       └───────────────┘
```

---

## New Component: VSAM Engine

### Module Structure

```
crates/zos-dataset/src/
├── vsam/
│   ├── mod.rs              # VSAM module exports
│   ├── cluster.rs          # Cluster definition and metadata
│   ├── ksds.rs             # Key-Sequenced Data Set
│   ├── esds.rs             # Entry-Sequenced Data Set
│   ├── rrds.rs             # Relative Record Data Set
│   ├── btree.rs            # B+ tree implementation
│   ├── ci.rs               # Control Interval handling
│   ├── ca.rs               # Control Area handling
│   ├── index.rs            # Index component
│   └── status.rs           # VSAM file status codes
```

### Architectural Decisions

#### ADR-v1.1-001: B+ Tree for KSDS Index

**Context:** KSDS requires efficient key-based access with ordered traversal.

**Decision:** Implement a B+ tree structure for KSDS indexes.

**Rationale:**
- B+ trees provide O(log n) lookup, O(n) sequential scan
- Leaf node linking enables efficient range queries
- Well-understood data structure with predictable performance
- Matches z/OS VSAM internal structure conceptually

**Structure:**
```rust
pub struct BPlusTree<K: Ord, V> {
    root: NodeId,
    order: usize,           // Max keys per node
    nodes: HashMap<NodeId, Node<K, V>>,
}

enum Node<K, V> {
    Internal {
        keys: Vec<K>,
        children: Vec<NodeId>,
    },
    Leaf {
        keys: Vec<K>,
        values: Vec<V>,
        next: Option<NodeId>, // Leaf linking for sequential access
    },
}
```

**Trade-offs:**
- Custom implementation vs. external crate (sled, rocksdb)
- Chose custom for: VSAM semantics control, no external dependencies, learning opportunity
- Risk: Correctness complexity - mitigated by comprehensive testing

#### ADR-v1.1-002: Control Interval/Area Abstraction

**Context:** z/OS VSAM uses CI (Control Interval) and CA (Control Area) concepts.

**Decision:** Implement simplified CI/CA abstraction for compatibility.

**Rationale:**
- Allows DEFINE CLUSTER parameters to be meaningful
- CISIZE, FREESPACE affect storage layout
- Not full z/OS fidelity, but compatible behavior

**Structure:**
```rust
pub struct ControlInterval {
    header: CIHeader,       // RDF/CIDF equivalent
    records: Vec<Record>,
    free_space: usize,
}

pub struct ControlArea {
    intervals: Vec<ControlInterval>,
    index: CAIndex,         // CA-level index splits
}
```

#### ADR-v1.1-003: VSAM File Format

**Context:** Need persistent storage format for VSAM datasets.

**Decision:** Use custom binary format with header + B+ tree serialization.

**Format:**
```
┌────────────────────────────────────────┐
│ VSAM Header (256 bytes)                │
│   Magic: "ZVSAM1"                      │
│   Type: KSDS/ESDS/RRDS                 │
│   Key offset, length                   │
│   Record size (avg/max)                │
│   CI size, CA size                     │
│   Statistics                           │
├────────────────────────────────────────┤
│ Index Component                        │
│   B+ tree nodes (for KSDS)             │
│   RBA index (for ESDS)                 │
├────────────────────────────────────────┤
│ Data Component                         │
│   Control Areas                        │
│     Control Intervals                  │
│       Records                          │
└────────────────────────────────────────┘
```

**File extension:** `.vsam` (single file containing index + data)

### VSAM API Design

```rust
// Cluster definition
pub struct VsamCluster {
    name: String,
    cluster_type: ClusterType,
    key_spec: Option<KeySpec>,      // For KSDS
    record_size: RecordSize,
    ci_size: usize,
    share_options: ShareOptions,
}

// File operations
pub trait VsamFile {
    fn open(path: &Path, mode: OpenMode) -> Result<Self>;
    fn close(&mut self) -> Result<()>;
    fn read(&mut self, key: &[u8]) -> Result<Option<Record>>;
    fn write(&mut self, record: &Record) -> Result<()>;
    fn delete(&mut self, key: &[u8]) -> Result<bool>;
    fn start(&mut self, key: &[u8], relation: KeyRelation) -> Result<()>;
    fn read_next(&mut self) -> Result<Option<Record>>;
}

// KSDS-specific
impl KsdsFile {
    fn read_key(&mut self, key: &[u8]) -> Result<Option<Record>>;
    fn read_key_generic(&mut self, partial_key: &[u8]) -> Result<Option<Record>>;
    fn update(&mut self, record: &Record) -> Result<()>;
}
```

### Integration with COBOL Runtime

COBOL VSAM verbs map to VSAM API:

| COBOL Verb | VSAM API Call |
|------------|---------------|
| OPEN | VsamFile::open() |
| CLOSE | VsamFile::close() |
| READ key | KsdsFile::read_key() |
| READ NEXT | VsamFile::read_next() |
| WRITE | VsamFile::write() |
| REWRITE | KsdsFile::update() |
| DELETE | VsamFile::delete() |
| START | VsamFile::start() |

---

## New Component: SORT Utility

### Module Structure

**Decision:** Create new crate `zos-sort` for SORT utility.

**Rationale:**
- SORT is a standalone utility, not just a library
- Complex enough to warrant separate crate
- Can be invoked via JCL or CLI independently
- Clear API boundary

```
crates/zos-sort/
├── Cargo.toml
├── src/
│   ├── lib.rs              # Library interface
│   ├── main.rs             # Standalone binary (optional)
│   ├── parser/
│   │   ├── mod.rs
│   │   ├── control.rs      # DFSORT control statement parser
│   │   └── fields.rs       # Field specification parser
│   ├── engine/
│   │   ├── mod.rs
│   │   ├── sort.rs         # Sort algorithm
│   │   ├── merge.rs        # Merge algorithm
│   │   ├── copy.rs         # Copy (no sort)
│   │   └── external.rs     # External merge sort
│   ├── ops/
│   │   ├── mod.rs
│   │   ├── include.rs      # INCLUDE/OMIT filtering
│   │   ├── inrec.rs        # Input record reformatting
│   │   ├── outrec.rs       # Output record reformatting
│   │   └── sum.rs          # Summarization
│   └── io/
│       ├── mod.rs
│       ├── reader.rs       # Input file handling
│       └── writer.rs       # Output file handling
```

### Architectural Decisions

#### ADR-v1.1-004: Sort Algorithm Strategy

**Decision:** Hybrid sort with automatic strategy selection.

**Strategies:**
1. **In-memory quicksort:** Dataset fits in memory
2. **External merge sort:** Dataset exceeds memory
3. **Parallel sort:** Multiple CPU cores available

```rust
pub enum SortStrategy {
    InMemory { threshold_mb: usize },
    External { temp_dir: PathBuf, memory_limit: usize },
    Parallel { threads: usize, chunk_size: usize },
}

impl SortEngine {
    fn select_strategy(&self, input_size: u64) -> SortStrategy {
        let available_memory = get_available_memory();
        if input_size < available_memory * 80 / 100 {
            SortStrategy::InMemory { threshold_mb: available_memory / 1024 / 1024 }
        } else {
            SortStrategy::External {
                temp_dir: self.config.temp_dir.clone(),
                memory_limit: available_memory * 50 / 100,
            }
        }
    }
}
```

#### ADR-v1.1-005: DFSORT Control Statement Parser

**Decision:** Hand-written parser for DFSORT control statements.

**Rationale:**
- DFSORT syntax is complex with many optional parameters
- Need detailed error messages for users migrating from z/OS
- Control over compatibility level

**Supported Statements:**
```
SORT FIELDS=(start,length,format,order,...)
MERGE FIELDS=(start,length,format,order,...)
INCLUDE COND=(field,op,value,...)
OMIT COND=(field,op,value,...)
INREC FIELDS=(...)
OUTREC FIELDS=(...)
SUM FIELDS=(...)
OPTION COPY
```

### SORT Integration

**JCL Integration:**
```
//SORT    EXEC PGM=SORT
//SORTIN  DD DSN=INPUT.FILE,...
//SORTOUT DD DSN=OUTPUT.FILE,...
//SYSIN   DD *
  SORT FIELDS=(1,10,CH,A)
  INCLUDE COND=(15,2,CH,EQ,C'NY')
/*
```

Maps to:
```rust
// JCL executor calls SORT engine
let sort_job = SortJob {
    input: vec![dd_sortin],
    output: dd_sortout,
    control: parse_control_statements(sysin_content)?,
};
sort_engine.execute(sort_job)?;
```

---

## New Component: GDG Manager

### Module Structure

```
crates/zos-dataset/src/
├── gdg/
│   ├── mod.rs              # GDG module exports
│   ├── base.rs             # GDG base entry
│   ├── generation.rs       # Generation management
│   ├── resolution.rs       # Relative reference resolution
│   └── rolloff.rs          # Generation limit rolloff
```

### Architectural Decisions

#### ADR-v1.1-006: GDG Catalog Extension

**Decision:** Extend existing catalog with GDG metadata.

**Structure:**
```rust
// In catalog
pub enum CatalogEntry {
    Sequential(SequentialEntry),
    Vsam(VsamEntry),
    Gdg(GdgBaseEntry),       // NEW
    GdgGeneration(GdgGen),   // NEW
}

pub struct GdgBaseEntry {
    name: String,            // e.g., "MY.GDG.BASE"
    limit: u32,              // Max generations
    scratch: bool,           // Delete when rolled off
    empty: bool,             // Allow empty GDG
    generations: Vec<GdgGen>,
}

pub struct GdgGen {
    base_name: String,
    absolute_name: String,   // e.g., "MY.GDG.BASE.G0001V00"
    generation: u32,
    version: u32,
    created: DateTime<Utc>,
    dataset_path: PathBuf,
}
```

#### ADR-v1.1-007: Relative Generation Resolution

**Decision:** Resolve relative generations at JCL parsing time.

**Resolution Rules:**
- `(+1)` - Create new generation (highest + 1)
- `(0)` - Current generation (highest existing)
- `(-1)` - Previous generation
- `G0001V00` - Absolute reference

```rust
impl GdgResolver {
    fn resolve(&self, reference: &GdgReference) -> Result<GdgGen> {
        match reference {
            GdgReference::Relative(offset) => {
                let current = self.get_current_generation()?;
                let target = (current as i32 + offset) as u32;
                if offset > 0 {
                    self.create_generation(target)
                } else {
                    self.get_generation(target)
                }
            }
            GdgReference::Absolute { gen, ver } => {
                self.get_generation_absolute(*gen, *ver)
            }
        }
    }
}
```

---

## New Component: IDCAMS

### Module Structure

```
crates/zos-dataset/src/
├── idcams/
│   ├── mod.rs              # IDCAMS module exports
│   ├── parser.rs           # Command parser
│   ├── commands/
│   │   ├── mod.rs
│   │   ├── define.rs       # DEFINE CLUSTER/GDG
│   │   ├── delete.rs       # DELETE
│   │   ├── alter.rs        # ALTER
│   │   ├── listcat.rs      # LISTCAT
│   │   ├── print.rs        # PRINT
│   │   ├── repro.rs        # REPRO
│   │   └── verify.rs       # VERIFY
│   └── executor.rs         # Command execution
```

### Architectural Decisions

#### ADR-v1.1-008: IDCAMS Command Model

**Decision:** Model IDCAMS as command pattern with parsed AST.

```rust
pub enum IdcamsCommand {
    Define(DefineCommand),
    Delete(DeleteCommand),
    Alter(AlterCommand),
    Listcat(ListcatCommand),
    Print(PrintCommand),
    Repro(ReproCommand),
    Verify(VerifyCommand),
}

pub struct DefineCommand {
    object_type: ObjectType,  // CLUSTER, GDG, etc.
    name: String,
    parameters: HashMap<String, Value>,
}

impl IdcamsExecutor {
    fn execute(&mut self, cmd: IdcamsCommand) -> Result<IdcamsResult> {
        match cmd {
            IdcamsCommand::Define(def) => self.define(def),
            IdcamsCommand::Delete(del) => self.delete(del),
            // ...
        }
    }
}
```

**CLI Integration:**
```bash
zos-clone idcams <<EOF
  DEFINE CLUSTER (NAME(MY.KSDS) -
    KEYS(10 0) -
    RECORDSIZE(100 200))
EOF
```

---

## JCL Extensions

### DD Statement Extensions

```rust
// Extended DD statement for VSAM/GDG
pub struct DdStatement {
    name: String,
    dsn: Option<DatasetName>,
    disp: Option<Disposition>,
    // New fields for v1.1:
    amp: Option<AmpParameters>,      // VSAM parameters
    gdg_ref: Option<GdgReference>,   // GDG relative reference
}

pub struct AmpParameters {
    amorg: bool,                     // VSAM organization
    bufnd: Option<u32>,              // Data buffers
    bufni: Option<u32>,              // Index buffers
}
```

### EXEC Statement Extensions

```rust
// Support for SORT and IDCAMS
pub enum ExecProgram {
    User(String),           // User program
    Sort,                   // PGM=SORT
    Idcams,                 // PGM=IDCAMS
    // Future: IEFBR14, etc.
}
```

---

## Package Distribution

### Build Infrastructure

```
.github/workflows/
├── ci.yml                  # Existing CI
├── release.yml             # Existing release
└── packages.yml            # NEW: Package building

scripts/
├── package/
│   ├── deb/
│   │   ├── control         # Debian package metadata
│   │   ├── postinst        # Post-install script
│   │   └── build.sh        # Build script
│   └── rpm/
│       ├── zos-clone.spec  # RPM spec file
│       └── build.sh        # Build script
```

### Package Contents

```
/usr/bin/
    zos-clone               # Main binary
    zos-sort                # SORT utility (optional)
/usr/share/man/man1/
    zos-clone.1             # Man page
    zos-clone-compile.1
    zos-clone-run.1
    zos-clone-idcams.1
    zos-clone-sort.1
/usr/share/bash-completion/completions/
    zos-clone               # Bash completions
/usr/share/zsh/vendor-completions/
    _zos-clone              # Zsh completions
/etc/zos-clone/
    zos-clone.toml.example  # Example config
```

---

## Testing Strategy

### VSAM Testing

| Test Category | Approach |
|---------------|----------|
| Unit tests | B+ tree operations, CI/CA handling |
| Integration tests | COBOL programs with VSAM verbs |
| Compatibility tests | Compare output with z/OS VSAM |
| Stress tests | Large datasets, concurrent access |
| Fuzz tests | Invalid key/record data |

### SORT Testing

| Test Category | Approach |
|---------------|----------|
| Unit tests | Parser, individual operations |
| Integration tests | Full sort jobs via JCL |
| Compatibility tests | Compare with DFSORT output |
| Performance tests | Large file sorting benchmarks |

### GDG Testing

| Test Category | Approach |
|---------------|----------|
| Unit tests | Resolution logic, rolloff |
| Integration tests | JCL with GDG references |
| Edge cases | Empty GDG, limit reached |

---

## Migration Path from v1.0

### No Breaking Changes

v1.1 is backward compatible with v1.0:
- All v1.0 APIs remain unchanged
- Existing JCL continues to work
- Sequential file I/O unchanged
- Configuration format unchanged

### New Dependencies

| Crate | Dependency | Reason |
|-------|------------|--------|
| zos-dataset | (internal) | VSAM/GDG modules |
| zos-sort | zos-dataset, zos-encoding | New crate |
| zos-jcl | zos-sort | SORT job execution |
| zos-clone | zos-sort | CLI integration |

---

## Performance Considerations

### VSAM Performance Targets

| Operation | Target | Implementation |
|-----------|--------|----------------|
| KSDS read (random) | <1ms | B+ tree O(log n) |
| KSDS read (sequential) | >50MB/s | Leaf node linking |
| KSDS write | <5ms | Buffered writes |
| Index rebuild | <10s/GB | Bulk load optimization |

### SORT Performance Targets

| Dataset Size | Target Time | Strategy |
|--------------|-------------|----------|
| <100MB | <5s | In-memory |
| 100MB-1GB | <60s | In-memory or external |
| 1GB-10GB | <10min | External merge |
| >10GB | Linear scaling | Parallel external merge |
