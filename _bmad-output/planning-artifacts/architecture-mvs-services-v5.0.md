---
version: 'v5.0'
planningGroup: 'PG-1'
technology: 'MVS System Services & SVCs'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-mvs-services-v5.0.md'
  - 'zos-complete-inventory.md (AREA-1)'
---

# Architecture: MVS System Services & SVCs

## 1. Crate Strategy

**New crate:** `open-mainframe-mvs`

MVS system services form a distinct foundational layer below the existing subsystem crates. A dedicated crate provides:
- Clear dependency boundary — subsystem crates depend on mvs, not vice versa
- Separation of kernel-level abstractions from application-level subsystems
- Ability for multiple crates (TSO, JES2, CICS, RACF) to share the same service implementations

**Workspace integration:** Add to `Cargo.toml` workspace members list and create workspace dependency entry.

## 2. Module Layout

```
crates/open-mainframe-mvs/
├── Cargo.toml
└── src/
    ├── lib.rs                # #![forbid(unsafe_code)], pub re-exports, crate docs
    ├── error.rs              # MvsError enum with thiserror + miette
    │
    ├── dynalloc/
    │   ├── mod.rs            # pub use, module docs
    │   ├── types.rs          # TextUnit, TextUnitKey, S99RB, S99TUPL, Verb
    │   ├── engine.rs         # DynallocEngine — processes allocation requests
    │   └── dd_table.rs       # DdTable — maps DDnames to dataset references
    │
    ├── console/
    │   ├── mod.rs            # pub use, module docs
    │   ├── wto.rs            # wto(), wtor(), dom() — operator message services
    │   ├── message.rs        # ConsoleMessage, RoutingCode, DescriptorCode
    │   └── reply.rs          # ReplyManager — handles WTOR reply correlation
    │
    ├── sync/
    │   ├── mod.rs            # pub use, module docs
    │   ├── ecb.rs            # Ecb — Event Control Block (wait/post)
    │   ├── wait.rs           # wait(), wait_multiple() — task suspension
    │   └── enq.rs            # EnqManager — resource serialization (qname/rname)
    │
    ├── storage/
    │   ├── mod.rs            # pub use, module docs
    │   ├── getmain.rs        # getmain(), freemain() — storage allocation
    │   └── subpool.rs        # Subpool management (subpool numbers, attributes)
    │
    ├── task/
    │   ├── mod.rs            # pub use, module docs
    │   ├── tcb.rs            # Tcb — Task Control Block representation
    │   ├── abend.rs          # abend() — abnormal termination with codes
    │   ├── attach.rs         # attach(), detach() — subtask management
    │   └── dispatch.rs       # TaskDispatcher — task scheduling/dispatching
    │
    ├── recovery/
    │   ├── mod.rs            # pub use, module docs
    │   ├── estae.rs          # EstaeManager — recovery exit chain
    │   ├── espie.rs          # EspieManager — program exception exits
    │   └── sdwa.rs           # Sdwa — System Diagnostic Work Area
    │
    ├── timer/
    │   ├── mod.rs            # pub use, module docs
    │   ├── time.rs           # time() — time-of-day retrieval in z/OS formats
    │   └── stimer.rs         # stimer(), stimerm() — interval timer services
    │
    └── program/
        ├── mod.rs            # pub use, module docs
        ├── link.rs           # link(), xctl() — program call/transfer
        ├── load.rs           # load(), delete() — module loading
        └── search.rs         # ProgramSearchOrder — STEPLIB/JOBLIB/LNKLST search
```

## 3. Key Types

### Core Error Type

```rust
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum MvsError {
    #[error("DYNALLOC failed: verb={verb}, error={error_code:#06x}, info={info_code:#06x}")]
    #[diagnostic(code(mvs::dynalloc_failed))]
    DynallocFailed {
        verb: u8,
        error_code: u16,
        info_code: u16,
    },

    #[error("ENQ contention timeout on resource {qname}/{rname}")]
    #[diagnostic(code(mvs::enq_timeout))]
    EnqTimeout { qname: String, rname: String },

    #[error("ABEND {code}")]
    #[diagnostic(code(mvs::abend))]
    Abend { code: AbendCode, reason: u32 },

    #[error("ESTAE recovery failed: no recovery routine established")]
    #[diagnostic(code(mvs::no_estae))]
    NoEstaeExit,

    #[error("program '{name}' not found in search order")]
    #[diagnostic(code(mvs::program_not_found))]
    ProgramNotFound { name: String },

    #[error("invalid text unit key {key:#06x}")]
    #[diagnostic(code(mvs::invalid_text_unit))]
    InvalidTextUnit { key: u16 },
}

pub type Result<T> = std::result::Result<T, MvsError>;
```

### DYNALLOC Types

```rust
/// SVC 99 verb codes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum DynallocVerb {
    Allocate = 0x01,
    Unallocate = 0x02,
    Concatenate = 0x03,
    Deconcatenate = 0x04,
    RemoveInUse = 0x05,
    DdallocMarkNonPerm = 0x06,
    InfoRetrieval = 0x07,
}

/// Text unit keys (IBM-defined numeric values)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u16)]
pub enum TextUnitKey {
    DalDsnam = 0x0001,   // Dataset name
    DalDdnam = 0x0002,   // DD name
    DalMembr = 0x0003,   // Member name
    DalStats = 0x0004,   // Dataset status (OLD/NEW/SHR/MOD)
    DalNdisp = 0x0005,   // Normal disposition
    DalCdisp = 0x0006,   // Conditional disposition
    DalUnit  = 0x0015,   // Unit name
    DalVlser = 0x0010,   // Volume serial
    DalBlksz = 0x0030,   // Block size
    DalLrecl = 0x0042,   // Logical record length
    DalRecfm = 0x0049,   // Record format
    DalDsorg = 0x003C,   // Dataset organization
    DalSpace = 0x000A,   // Space type (TRK/CYL/BLK)
    DalPrime = 0x000B,   // Primary allocation
    DalSecnd = 0x000C,   // Secondary allocation
    DalDir   = 0x000D,   // Directory blocks (PDS)
    DalRtddn = 0x0056,   // Return DDname
    DalRtdsn = 0x0057,   // Return dataset name
    DalRtorg = 0x0059,   // Return dataset organization
}

/// A single text unit in the SVC 99 parameter list
#[derive(Debug, Clone)]
pub struct TextUnit {
    pub key: TextUnitKey,
    pub values: Vec<Vec<u8>>,
}

/// SVC 99 Request Block
#[derive(Debug, Clone)]
pub struct DynallocRequest {
    pub verb: DynallocVerb,
    pub flags: u16,
    pub text_units: Vec<TextUnit>,
}

/// SVC 99 response
#[derive(Debug, Clone)]
pub struct DynallocResponse {
    pub error_code: u16,
    pub info_code: u16,
    pub return_text_units: Vec<TextUnit>,
}

/// DD Table entry — maps a DDname to a dataset
#[derive(Debug, Clone)]
pub struct DdEntry {
    pub ddname: String,
    pub dsname: String,
    pub status: DatasetStatus,
    pub disposition: Disposition,
    pub concatenation: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DatasetStatus {
    Old,
    New,
    Shr,
    Mod,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Disposition {
    Keep,
    Delete,
    Catlg,
    Uncatlg,
    Pass,
}
```

### Console/WTO Types

```rust
/// Operator console message
#[derive(Debug, Clone)]
pub struct ConsoleMessage {
    pub id: u32,
    pub text: String,
    pub routing_codes: u16,
    pub descriptor_codes: u16,
    pub is_multiline: bool,
    pub lines: Vec<String>,
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

/// Routing codes (bits 0-15 map to routing codes 1-16)
#[derive(Debug, Clone, Copy)]
pub struct RoutingCode(pub u16);

/// Descriptor codes (bits 0-12 map to descriptor codes 1-13)
#[derive(Debug, Clone, Copy)]
pub struct DescriptorCode(pub u16);

/// WTOR reply handle
#[derive(Debug, Clone)]
pub struct ReplyToken {
    pub id: u32,
    pub ecb: Arc<Ecb>,
}
```

### Synchronization Types

```rust
/// Event Control Block — the fundamental z/OS synchronization primitive
#[derive(Debug)]
pub struct Ecb {
    /// Bit 0: wait bit, Bit 1: complete bit, Bits 2-31: completion code
    value: AtomicU32,
    notify: tokio::sync::Notify,
}

/// ENQ resource scope
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EnqScope {
    Step,    // Within current address space (task-local)
    System,  // System-wide
    Systems, // Sysplex-wide (maps to System in single-system)
}

/// ENQ lock mode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EnqMode {
    Shared,
    Exclusive,
}

/// ENQ resource identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnqResource {
    pub qname: String,  // 1-8 characters
    pub rname: String,  // 1-255 characters
}

/// ENQ manager — system-wide resource serialization
pub struct EnqManager {
    locks: DashMap<EnqResource, EnqState>,
}
```

### Task & Recovery Types

```rust
/// ABEND completion code
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AbendCode {
    System(u16),   // System ABEND (e.g., 0C7 = 0x0C7)
    User(u16),     // User ABEND (0-4095)
}

/// Task Control Block — represents a dispatchable unit of work
#[derive(Debug)]
pub struct Tcb {
    pub id: u64,
    pub parent: Option<u64>,
    pub state: TaskState,
    pub program_name: String,
    pub abend_code: Option<AbendCode>,
    pub estae_chain: Vec<EstaeEntry>,
    pub espie_table: EspieTable,
    pub dd_table: Arc<RwLock<DdTable>>,
    pub storage: SubpoolManager,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TaskState {
    Ready,
    Running,
    Waiting,
    Terminated,
}

/// ESTAE recovery chain entry
#[derive(Debug, Clone)]
pub struct EstaeEntry {
    pub handler: Arc<dyn Fn(&Sdwa) -> EstaeAction + Send + Sync>,
    pub parameter: u64,
}

#[derive(Debug, Clone, Copy)]
pub enum EstaeAction {
    Retry { address: u64 },
    Percolate,
}

/// System Diagnostic Work Area
#[derive(Debug, Clone)]
pub struct Sdwa {
    pub abend_code: AbendCode,
    pub reason_code: u32,
    pub failing_instruction: u64,
    pub registers: [u64; 16],
    pub psw: u64,
}

/// ESPIE table — maps interrupt codes to handlers
#[derive(Debug, Default)]
pub struct EspieTable {
    handlers: HashMap<u8, Arc<dyn Fn(&Pie) -> EspieAction + Send + Sync>>,
}

/// Program Interruption Element
#[derive(Debug, Clone)]
pub struct Pie {
    pub interrupt_code: u8,
    pub failing_instruction: u64,
    pub psw: u64,
}

#[derive(Debug, Clone, Copy)]
pub enum EspieAction {
    Resume { address: u64 },
    Abend,
}
```

### Timer Types

```rust
/// z/OS time formats
#[derive(Debug, Clone)]
pub struct MvsTime {
    pub tod_clock: u64,           // STCK format (bit 51 = 1 microsecond)
    pub date_packed: u32,         // 0CYYDDDF (packed decimal, Julian)
    pub time_binary: u32,         // Hundredths of seconds since midnight
    pub time_packed: u64,         // HHMMSSTHxxxxxx (packed decimal)
    pub date_yyyymmdd: u32,       // Gregorian date packed decimal
}

/// STIMER interval specification
#[derive(Debug, Clone)]
pub enum StimerInterval {
    BinaryTime(u32),              // Hundredths of seconds
    ClockTime(u64),               // TOD clock units
    RealTime { hours: u8, minutes: u8, seconds: u8, hundredths: u8 },
}

/// STIMER completion mode
#[derive(Debug, Clone)]
pub enum StimerMode {
    Wait,                          // Suspend task until timer expires
    Exit(Arc<dyn Fn() + Send + Sync>), // Invoke exit when timer expires
}
```

## 4. Integration Points

```
                    ┌─────────────────┐
                    │ open-mainframe- │
                    │     tso         │ ALLOCATE/FREE → dynalloc
                    │                 │ SEND → wto
                    └────────┬────────┘
                             │
┌──────────────┐    ┌────────▼────────┐    ┌──────────────┐
│ open-main-   │    │ open-mainframe- │    │ open-main-   │
│ frame-jes2   │◄───│     mvs         │───►│ frame-racf   │
│ console msgs │    │                 │    │ ENQ auth     │
└──────────────┘    │ dynalloc/       │    └──────────────┘
                    │ console/        │
┌──────────────┐    │ sync/           │    ┌──────────────┐
│ open-main-   │    │ storage/        │    │ open-main-   │
│ frame-cics   │◄───│ task/           │───►│ frame-le     │
│ task mgmt    │    │ recovery/       │    │ ESTAE→CEEHDLR│
└──────────────┘    │ timer/          │    └──────────────┘
                    │ program/        │
                    └────────┬────────┘
                             │
                    ┌────────▼────────┐
                    │ open-mainframe- │
                    │    dataset      │ DD→catalog mapping
                    └─────────────────┘
```

**Key integration patterns:**
- MVS crate does NOT depend on subsystem crates (TSO, JES2, CICS)
- Subsystem crates depend on MVS crate for services
- MVS depends on `open-mainframe-dataset` for catalog resolution in DYNALLOC
- MVS depends on `open-mainframe-racf` for authorization checks
- Integration uses trait objects and callbacks to avoid circular dependencies

## 5. Design Decisions

### DD-5.0-01: Tokio-based Task Abstraction

**Decision:** Map z/OS tasks (TCBs) to Tokio tasks. ECB WAIT/POST maps to `tokio::sync::Notify`. ENQ maps to `DashMap` with `tokio::sync::RwLock` entries.

**Rationale:** OpenMainframe runs on a Tokio async runtime. Rather than implementing a custom task dispatcher, we leverage Tokio's executor. A z/OS "task" (TCB) becomes a Tokio task with metadata (TCB struct). WAIT maps to `Notify::notified()`, POST maps to `Notify::notify_one()`.

### DD-5.0-02: DD Table as Shared State

**Decision:** The DD table (mapping DDnames to datasets) is per-address-space, stored as `Arc<RwLock<DdTable>>` in the TCB.

**Rationale:** On z/OS, DD allocations are scoped to the address space (job step). Multiple tasks within the same step share the DD table. Using `Arc<RwLock<_>>` allows safe concurrent access from multiple Tokio tasks representing subtasks.

### DD-5.0-03: Console as Channel

**Decision:** WTO/WTOR messages are sent via a `tokio::sync::mpsc` channel to a Console actor. WTOR replies come back via a per-request `tokio::sync::oneshot` channel.

**Rationale:** Decouples message producers (any program issuing WTO) from the console display and automation layer. The Console actor can be backed by the z/OSMF console REST API, a terminal UI, or an automated MPF rules engine.

### DD-5.0-04: ESTAE as Callback Chain

**Decision:** ESTAE recovery exits are stored as a `Vec<EstaeEntry>` on the TCB, processed in LIFO order (last established = first invoked).

**Rationale:** Matches z/OS semantics where the most recent ESTAE is invoked first. If it percolates, the next in the chain is tried. Using `Arc<dyn Fn>` trait objects allows both Rust closures and language-specific callbacks (COBOL, REXX) to serve as recovery routines.

### DD-5.0-05: ENQ Global Manager

**Decision:** A singleton `EnqManager` (behind `Arc`) manages all system-wide ENQ/DEQ operations using `DashMap` for lock-free read access.

**Rationale:** On z/OS, GRS (Global Resource Serialization) is system-wide. Since OpenMainframe is single-system, one `EnqManager` suffices. `DashMap` provides concurrent access without a global mutex, and each entry tracks shared/exclusive holders with a wait queue.

## 6. Testing Strategy

- **Unit tests:** Each module has `#[cfg(test)] mod tests` testing individual functions
- **DYNALLOC tests:** Allocate/deallocate/concatenate with mock catalog
- **ENQ tests:** Concurrent ENQ contention scenarios with multiple Tokio tasks
- **WTO tests:** Message formatting, routing code filtering, WTOR reply correlation
- **ESTAE tests:** Recovery chain invocation, retry vs percolate, nested ESTAE
- **Timer tests:** STIMER expiry, STIMER cancellation, TIME format accuracy
- **Integration tests:** `tests/` directory with scenarios exercising cross-module flows (e.g., DYNALLOC → ENQ on dataset → WTO success message)

## 7. Cargo.toml Template

```toml
[package]
name = "open-mainframe-mvs"
description = "MVS system services (SVCs) for OpenMainframe — DYNALLOC, WTO, ENQ, ESTAE, task management"
version.workspace = true
edition.workspace = true
rust-version.workspace = true
license.workspace = true

[dependencies]
miette = { workspace = true }
thiserror = { workspace = true }
serde = { workspace = true }
serde_json = { workspace = true }
tracing = { workspace = true }
tokio = { version = "1", features = ["sync", "time", "rt"] }
dashmap = "6"
chrono = { version = "0.4", features = ["serde"] }

# Internal crates
open-mainframe-dataset = { workspace = true }
open-mainframe-racf = { workspace = true }

[dev-dependencies]
tokio = { version = "1", features = ["full", "test-util"] }
```
