---
version: 'v5.0'
planningGroup: 'PG-11'
technology: 'Program Management & Binder'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-pgmmgmt-v5.0.md'
---

# Architecture: Program Management & Binder

## 1. Crate Strategy

**Extend existing crate:** `open-mainframe-mvs` (new `program/` module)

Rationale: Program management (LOAD/LINK/XCTL/ATTACH) is part of MVS system services. The SVC implementations belong in `open-mainframe-mvs`. The Binder is a utility invoked via `EXEC PGM=IEWBLINK` and can live in the utilities module of `open-mainframe-jcl`.

## 2. Module Layout

```
crates/open-mainframe-mvs/src/
├── program/
│   ├── mod.rs          # ProgramManager, program search, LOAD/LINK/XCTL/ATTACH
│   ├── search.rs       # Library search order (STEPLIB→JOBLIB→LPA→LNKLST)
│   ├── loader.rs       # Load module loader (parse and relocate)
│   ├── objmod.rs       # Object module format parser (ESD/TXT/RLD/END)
│   ├── loadmod.rs      # Load module format (CESD, text records)
│   ├── binder.rs       # Binder core (symbol resolution, relocation, output)
│   ├── apf.rs          # APF authorization list
│   └── lpa.rs          # Link Pack Area model
```

## 3. Key Types

```rust
/// Program entry in the loaded program table
pub struct LoadedProgram {
    pub name: String,
    pub entry_point: u64,
    pub amode: AddressingMode,
    pub rmode: ResidenceMode,
    pub size: usize,
    pub apf_authorized: bool,
    pub use_count: u32,
    pub aliases: Vec<String>,
}

/// Object module ESD entry
pub enum EsdType {
    SD { name: String, address: u32, length: u32 },  // Section Definition (CSECT)
    LD { name: String, address: u32, section: u16 },  // Label Definition (ENTRY)
    ER { name: String },                                // External Reference
    WX { name: String },                                // Weak External
}

/// Relocation dictionary entry
pub struct RldEntry {
    pub target_esdid: u16,
    pub position_esdid: u16,
    pub offset: u32,
    pub length: u8,    // 1, 2, 3, or 4 bytes
    pub direction: RldDirection,  // Add or Subtract
}

/// Program search order
pub struct ProgramSearchPath {
    pub steplib: Vec<String>,   // STEPLIB DD libraries
    pub joblib: Vec<String>,    // JOBLIB DD libraries
    pub lpa: Vec<String>,       // Link Pack Area modules
    pub lnklst: Vec<String>,    // LNKLST concatenation
}
```

## 4. Design Decisions

### DD-5.0-PGM-01: Object Module as Data Structure
**Decision:** Object modules are parsed into Rust data structures (ESD table, text segments, RLD entries). No assembly-level execution; the binder produces a loadable image.

### DD-5.0-PGM-02: Emulated Addressing Modes
**Decision:** AMODE/RMODE are tracked as metadata. In the Rust runtime, all addresses are native 64-bit. AMODE/RMODE affect external interface conventions only.

### DD-5.0-PGM-03: Program Search via Filesystem
**Decision:** STEPLIB/JOBLIB/LPA/LNKLST map to directories or PDS paths on the filesystem. The search order is a simple sequential path search.
