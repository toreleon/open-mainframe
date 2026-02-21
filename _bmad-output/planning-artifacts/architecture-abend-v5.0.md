---
version: 'v5.0'
planningGroup: 'PG-7'
technology: 'ABEND Framework & Debugging'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-abend-v5.0.md'
---

# Architecture: ABEND Framework & Debugging

## 1. Crate Strategy

**Extend existing crate:** `open-mainframe-mvs` (new `abend` module)

Rationale: ABEND is a core MVS service. The AbendCode, Sdwa, and Tcb types are already planned for `open-mainframe-mvs` (SYS-100/SYS-101). The ABEND framework extends these with a comprehensive code registry, dump generation, and diagnostic messaging. Dump DD handling integrates with `open-mainframe-jcl` via the existing utility/program dispatch mechanism.

## 2. Module Layout

```
crates/open-mainframe-mvs/src/
├── abend/
│   ├── mod.rs            # AbendCode, AbendInfo, ABEND macro entrypoint
│   ├── registry.rs       # System ABEND code registry with descriptions
│   ├── dump.rs           # Dump generation (SYSUDUMP, SYSABEND, SYSMDUMP)
│   ├── snap.rs           # SNAP dump generation (non-terminating)
│   └── messages.rs       # ABEND diagnostic message catalog
```

## 3. Key Types

### ABEND Code System

```rust
/// ABEND completion code
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AbendCode {
    /// System ABEND (S0C1, S0C4, S0C7, S013, S222, S322, S806, S913, etc.)
    System(u16),
    /// User ABEND (U0000-U4095)
    User(u16),
}

/// Extended ABEND information
pub struct AbendInfo {
    pub code: AbendCode,
    pub reason: u32,              // Optional reason code
    pub retry: bool,              // RETRY option on ABEND macro
    pub dump: bool,               // DUMP option (default true)
    pub step_name: String,
    pub program_name: String,
}

/// System ABEND code registry entry
pub struct SystemAbendEntry {
    pub code: u16,
    pub name: &'static str,       // e.g., "OPERATION EXCEPTION"
    pub description: &'static str,
    pub message_id: &'static str, // e.g., "IEA995I"
    pub common_causes: &'static [&'static str],
    pub recovery_hint: &'static str,
}

/// ABEND code registry (static lookup)
pub struct AbendRegistry;

impl AbendRegistry {
    /// Look up system ABEND code
    pub fn lookup_system(code: u16) -> Option<&'static SystemAbendEntry>;

    /// Format ABEND code for display
    pub fn format_code(code: &AbendCode) -> String;

    /// Get diagnostic message for ABEND
    pub fn diagnostic_message(info: &AbendInfo) -> Vec<String>;
}
```

### Dump Generation

```rust
/// Dump type (determines content and format)
pub enum DumpType {
    /// SYSUDUMP — formatted user dump (registers, user storage, traceback)
    UserDump,
    /// SYSABEND — formatted dump with system areas
    AbendDump,
    /// SYSMDUMP — unformatted machine-readable dump
    MachineDump,
}

/// Dump context — what to include in the dump
pub struct DumpContext {
    pub dump_type: DumpType,
    pub abend_info: AbendInfo,
    pub registers: [u64; 16],
    pub psw: u64,
    pub storage_ranges: Vec<StorageRange>,
    pub traceback: Vec<TraceEntry>,
    pub task_info: TaskDumpInfo,
}

/// Storage range for dump
pub struct StorageRange {
    pub label: String,
    pub address: u64,
    pub data: Vec<u8>,
}

/// Traceback entry
pub struct TraceEntry {
    pub module_name: String,
    pub entry_point: u64,
    pub return_address: u64,
    pub save_area: [u64; 16],
}

/// SNAP dump request
pub struct SnapRequest {
    pub id: u16,                  // SNAP ID for identification
    pub storage: Vec<StorageRange>,
    pub pdata: SnapPdata,         // What program data to include
}

/// SNAP PDATA options
pub struct SnapPdata {
    pub regs: bool,               // Include registers
    pub sa: bool,                 // Include save areas
    pub cb: bool,                 // Include control blocks
    pub jpa: bool,                // Include job pack area
    pub spls: bool,               // Include subpool list
}

/// Dump writer
pub trait DumpWriter: Send + Sync {
    /// Write formatted dump to output
    fn write_formatted(&self, context: &DumpContext, output: &mut dyn Write) -> Result<()>;

    /// Write unformatted dump to output
    fn write_unformatted(&self, context: &DumpContext, output: &mut dyn Write) -> Result<()>;

    /// Write SNAP dump to output
    fn write_snap(&self, request: &SnapRequest, output: &mut dyn Write) -> Result<()>;
}
```

## 4. Integration Points

```
┌──────────────────────┐
│ open-mainframe-jcl   │  Step ABEND → check SYSUDUMP/SYSABEND/SYSMDUMP DDs
│   executor.rs        │  → generate dump → propagate CC/ABEND to COND=
│                      │
│  IF (STEP1.RC > 0)   │  ← ABEND code available as step completion
└──────────┬───────────┘
           │
           ▼
┌──────────────────────┐
│ open-mainframe-mvs   │
│                      │
│   abend/mod.rs ──────┤──► registry.rs (code lookup)
│                      │──► dump.rs (dump generation)
│                      │──► snap.rs (SNAP dumps)
│                      │──► messages.rs (diagnostic messages)
│                      │
│   recovery/          │
│     estae.rs ────────┤──► SDWA receives AbendInfo
│     espie.rs ────────┤──► Program exceptions → ABEND
│                      │
│   task/tcb.rs ───────┤──► Tcb.abend_code set on ABEND
└──────────┬───────────┘
           │
           ▼
┌──────────────────────┐
│ open-mainframe-le    │  LE condition handling translates ABENDs
│   condition.rs       │  S0C7 → CEE3207S
│                      │  ABEND → CEEMSG + CEEDUMP
└──────────────────────┘
```

## 5. Design Decisions

### DD-5.0-ABN-01: ABEND Registry as Static Data

**Decision:** System ABEND codes are stored as a static registry (compile-time data) rather than runtime configuration.

**Rationale:** ABEND code meanings are defined by the z/OS architecture and never change at runtime. A static registry provides O(1) lookup and zero allocation. The registry covers the ~40 most common system ABENDs; unknown codes fall through with a generic description.

### DD-5.0-ABN-02: Dump DDs Checked in Priority Order

**Decision:** When ABEND occurs, the JCL executor checks for dump DDs in this order: SYSUDUMP → SYSABEND → SYSMDUMP. Only the first found DD receives the dump.

**Rationale:** This matches the z/OS convention where only one dump is produced per ABEND. SYSUDUMP is checked first because it produces the most useful developer-oriented output.

### DD-5.0-ABN-03: SNAP as Non-Terminating Dump

**Decision:** SNAP dumps write diagnostic information and return control to the caller (unlike ABEND dumps which occur during termination).

**Rationale:** SNAP is used for debugging during execution — programs call SNAP to capture state at specific points without terminating. The SNAP DCB must be opened before the first SNAP call and closed when done.

### DD-5.0-ABN-04: Module in open-mainframe-mvs

**Decision:** ABEND framework lives as an `abend/` module within `open-mainframe-mvs` rather than a separate crate.

**Rationale:** ABEND is intrinsically tied to MVS task management (TCB), recovery (ESTAE/ESPIE), and program management (LINK/XCTL). These are all in `open-mainframe-mvs`. A separate crate would create circular dependencies.

## 6. Testing Strategy

- Unit tests: ABEND code registry lookup, code formatting, message generation
- Dump generation: Formatted dump output for SYSUDUMP/SYSABEND with known inputs
- SNAP: Non-terminating dump with storage ranges
- Integration: JCL job with ABEND → dump DD → verify dump output
- Integration: ESTAE recovery from ABEND → verify SDWA contents
- Integration: COND= processing after ABEND step → verify step skipping
