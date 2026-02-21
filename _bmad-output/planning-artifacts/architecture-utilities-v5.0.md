---
version: 'v5.0'
planningGroup: 'PG-5'
technology: 'Utilities & DFSORT'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-utilities-v5.0.md'
---

# Architecture: Utilities & DFSORT

## 1. Crate Strategy

**Extend existing crate:** `open-mainframe-sort` (DFSORT enhancements) + **New module in `open-mainframe-jcl`** (utility programs)

Rationale: Utility programs are invoked via JCL `EXEC PGM=utilname`. They are effectively "built-in programs" that the JCL runtime recognizes. Rather than a separate crate, utilities live as a `utilities` module within the JCL crate (since they are dispatched by the JCL executor). DFSORT enhancements (OUTFIL, IFTHEN, ICETOOL) extend the existing sort crate.

## 2. Module Layout

### In open-mainframe-jcl (utilities module)
```
crates/open-mainframe-jcl/src/
└── utilities/
    ├── mod.rs            # UtilityRegistry, UtilityProgram trait, dispatch
    ├── iebcopy.rs        # IEBCOPY — PDS copy/compress/merge
    ├── iebgener.rs       # IEBGENER — sequential copy with control statements
    ├── iebcompr.rs       # IEBCOMPR — dataset comparison
    ├── iebupdte.rs       # IEBUPDTE — PDS update
    ├── iebptpch.rs       # IEBPTPCH — print/punch
    ├── iebdg.rs          # IEBDG — test data generator
    ├── iehprogm.rs       # IEHPROGM — scratch/rename/catalog
    ├── iehlist.rs        # IEHLIST — catalog/VTOC listing
    ├── iehmove.rs        # IEHMOVE — dataset move
    ├── amaspzap.rs       # AMASPZAP — superzap patch
    ├── ikjeft01.rs       # IKJEFT01 — TSO batch
    ├── irxjcl.rs         # IRXJCL — REXX batch
    └── bpxbatch.rs       # BPXBATCH — USS batch
```

### In open-mainframe-sort (DFSORT enhancements)
```
crates/open-mainframe-sort/src/
├── outfil.rs             # OUTFIL statement processing (new)
├── ifthen.rs             # IFTHEN conditional processing (new)
├── symbols.rs            # DFSORT symbols and functions (new)
└── icetool.rs            # ICETOOL multi-operation utility (new)
```

## 3. Key Types

### Utility Framework

```rust
/// Trait that all utility programs implement
pub trait UtilityProgram: Send + Sync {
    /// Program name (e.g., "IEBCOPY", "IEBGENER")
    fn name(&self) -> &str;

    /// Execute the utility with the given DD allocations and control statements
    fn execute(&self, context: &mut UtilityContext) -> UtilityResult;
}

/// Context passed to each utility during execution
pub struct UtilityContext {
    pub dd_table: Arc<RwLock<DdTable>>,
    pub sysin: Option<Vec<String>>,       // Control statements from SYSIN DD
    pub sysprint: Vec<String>,            // Output messages
    pub step_name: String,
    pub program_name: String,
}

/// Utility result with condition code
pub struct UtilityResult {
    pub condition_code: u32,  // 0, 4, 8, 12, or 16
    pub messages: Vec<UtilityMessage>,
}

/// Utility message with ID and severity
pub struct UtilityMessage {
    pub id: String,           // e.g., "IEB1013I"
    pub severity: char,       // I, W, E, S
    pub text: String,
}

/// Registry of available utility programs
pub struct UtilityRegistry {
    programs: HashMap<String, Box<dyn UtilityProgram>>,
}
```

### IEBCOPY Types

```rust
pub struct IebcopyConfig {
    pub operations: Vec<CopyOperation>,
}

pub enum CopyOperation {
    Copy {
        indd: String,
        outdd: String,
        select: Option<Vec<String>>,
        exclude: Option<Vec<String>>,
        replace: bool,
    },
    Compress {
        dd: String,
    },
}
```

### ICETOOL Types

```rust
pub enum IcetoolOperation {
    Sort { from: String, to: String, using: String },
    Copy { from: String, to: String },
    Count { from: String, save: Option<String> },
    Stats { from: String, on: Vec<FieldSpec> },
    Display { from: String, list: Option<String> },
    Occur { from: String, on: Vec<FieldSpec>, list: Option<String> },
    Select { from: String, to: String, on: Vec<FieldSpec>, condition: SelectCondition },
    Unique { from: String, to: String, on: Vec<FieldSpec> },
}
```

## 4. Integration Points

```
┌──────────────────┐
│ open-mainframe-  │
│     jcl          │  EXEC PGM=IEBCOPY → UtilityRegistry.dispatch("IEBCOPY")
│                  │
│  executor.rs ────┼──► utilities/mod.rs → iebcopy.rs
│                  │                    → iebgener.rs
│                  │                    → ikjeft01.rs → open-mainframe-tso
│                  │                    → irxjcl.rs   → open-mainframe-rexx
└────────┬─────────┘
         │
         ▼
┌──────────────────┐    ┌──────────────────┐
│ open-mainframe-  │    │ open-mainframe-  │
│    dataset       │    │     sort         │
│ PDS, QSAM, VSAM │    │ DFSORT, ICETOOL  │
└──────────────────┘    └──────────────────┘
```

## 5. Design Decisions

### DD-5.0-UTIL-01: Utilities as Trait Implementations

**Decision:** Each utility implements the `UtilityProgram` trait. The JCL executor dispatches to utilities via `UtilityRegistry::dispatch(program_name)`.

**Rationale:** This mirrors how z/OS loads and executes utility programs. The trait-based approach allows easy addition of new utilities and testing via mock contexts.

### DD-5.0-UTIL-02: Control Statements via SYSIN DD

**Decision:** Utility control statements are read from the SYSIN DD as a `Vec<String>`. Each utility parses its own control statement format.

**Rationale:** Each z/OS utility has its own unique control statement syntax. A common parser would be overly complex. Instead, each utility module contains its own statement parser.

### DD-5.0-UTIL-03: DFSORT Enhancements in Sort Crate

**Decision:** OUTFIL, IFTHEN, FINDREP, and ICETOOL are added to the existing `open-mainframe-sort` crate rather than the utilities module.

**Rationale:** These are core DFSORT features, not separate utilities. DFSORT is already implemented in the sort crate; OUTFIL and IFTHEN extend its record processing pipeline.

## 6. Testing Strategy

- Each utility has unit tests with mock DD tables and datasets
- IEBCOPY tests: copy members, compress, merge, select/exclude
- IEBGENER tests: basic copy, control statement reformatting
- DFSORT/ICETOOL tests: OUTFIL splitting, IFTHEN conditions, ICETOOL operations
- IKJEFT01 tests: TSO command execution from SYSTSIN
- Integration tests: Full JCL job stream with multiple utility steps
