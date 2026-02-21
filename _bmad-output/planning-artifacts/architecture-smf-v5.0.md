---
version: 'v5.0'
planningGroup: 'PG-17'
technology: 'SMF (System Management Facilities)'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-smf-v5.0.md'
---

# Architecture: SMF (System Management Facilities)

## 1. Crate Strategy

**New crate:** `open-mainframe-smf`

Rationale: SMF is a pervasive subsystem touching all other crates (JCL, RACF, CICS, DB2, IMS, MQ, WLM). A separate crate provides the record infrastructure and APIs that all other crates call into. This avoids circular dependencies — other crates depend on `open-mainframe-smf` for writing records.

## 2. Module Layout

```
crates/open-mainframe-smf/src/
├── lib.rs
├── record/
│   ├── mod.rs          # SmfRecord, SmfHeader, self-defining sections
│   ├── header.rs       # Standard header (SMFRECORDHEADER)
│   ├── triplet.rs      # Self-defining section triplets (offset/length/count)
│   ├── serde.rs        # Binary serialization/deserialization
│   └── types.rs        # Record type constants (0-255)
├── config/
│   ├── mod.rs          # SMFPRMxx configuration
│   └── parmlib.rs      # PARMLIB parameter parser
├── writer/
│   ├── mod.rs          # SmfWriter, buffer management
│   ├── smfwtm.rs       # SMFWTM macro equivalent
│   └── buffer.rs       # Record buffering and flushing
├── exits/
│   ├── mod.rs          # Exit framework
│   ├── iefu83.rs       # Record filtering exit
│   ├── iefu84.rs       # Subsystem identification exit
│   └── dynamic.rs      # IFASMFEX dynamic registration
├── records/
│   ├── mod.rs          # Record type registry
│   ├── type30.rs       # Job accounting (subtypes 1-5)
│   ├── type14_15.rs    # Dataset open/close
│   ├── type17_18.rs    # Dataset scratch/rename
│   ├── type80.rs       # RACF security audit
│   ├── type70_79.rs    # Performance records
│   ├── type100_120.rs  # Subsystem records (DB2, CICS, MQ, TCP/IP)
│   └── user.rs         # User record types (128-255)
├── dump/
│   ├── mod.rs          # Dump utilities
│   ├── ifasmfdp.rs     # IFASMFDP dump program
│   └── filter.rs       # Record filtering (type, date, time, jobname)
└── bridge/
    ├── mod.rs          # Observability bridge
    ├── prometheus.rs   # SMF → Prometheus metrics
    └── otel.rs         # SMF → OpenTelemetry spans/metrics
```

## 3. Key Types

```rust
/// SMF record header (standard header)
pub struct SmfHeader {
    pub length: u16,           // Record length (including header)
    pub segment: u8,           // Segment descriptor
    pub flag: u8,              // System indicator flags
    pub record_type: u8,       // Record type (0-255)
    pub time: u32,             // Time of day (hundredths of seconds since midnight)
    pub date: u32,             // Date (0cyydddF packed decimal)
    pub system_id: [u8; 4],    // SMF system ID (SID)
    pub subsystem_id: [u8; 4], // Subsystem ID
    pub subtype: u16,          // Record subtype
}

/// Self-defining section triplet
pub struct SmfTriplet {
    pub offset: u32,   // Offset from record start to section data
    pub length: u16,   // Length of each section entry
    pub count: u16,    // Number of section entries
}

/// SMF record (generic container)
pub struct SmfRecord {
    pub header: SmfHeader,
    pub triplets: Vec<SmfTriplet>,
    pub data: Vec<u8>,
}

/// SMFPRMxx configuration
pub struct SmfPrmConfig {
    pub active_types: HashSet<u8>,      // TYPE(30,70:79,80)
    pub inactive_types: HashSet<u8>,    // NOTYPE(0:29)
    pub exits: Vec<SmfExit>,
    pub interval: Duration,             // Recording interval
    pub max_buffer_size: usize,
    pub recording_mode: RecordingMode,  // LOGSTREAM | DATASET
}

/// Type 30 job accounting record
pub struct Type30Record {
    pub subtype: Type30Subtype,
    pub job_name: [u8; 8],
    pub job_id: [u8; 8],
    pub step_name: [u8; 8],
    pub program_name: [u8; 8],
    pub cpu_time: Duration,
    pub elapsed_time: Duration,
    pub completion_code: u32,
    pub service_class: [u8; 8],
    pub excp_count: u32,
}

pub enum Type30Subtype {
    JobInitiation = 1,
    IntervalRecord = 2,
    StepTermination = 3,
    JobTermination = 4,
    RedirectedOutput = 5,
}

/// SMF exit trait
pub trait SmfExit: Send + Sync {
    fn process(&self, record: &mut SmfRecord) -> SmfExitAction;
}

pub enum SmfExitAction {
    Pass,     // Let the record through
    Suppress, // Drop the record
    Modify,   // Record was modified in-place
}
```

## 4. Design Decisions

### DD-5.0-SMF-01: Binary-Compatible Records
**Decision:** SMF records use the exact same binary layout as z/OS SMF records (big-endian, packed decimal dates, EBCDIC strings). This ensures compatibility with existing SMF analysis tools.

### DD-5.0-SMF-02: Exit Framework via Trait Objects
**Decision:** SMF exits are implemented as `dyn SmfExit` trait objects, registered at runtime. This mirrors z/OS's dynamic exit registration (IFASMFEX) while using Rust's trait system.

### DD-5.0-SMF-03: Buffered Writing
**Decision:** SMF writes are buffered in memory and flushed periodically or when the buffer fills. This matches z/OS behavior where SMFWTM writes to a buffer, not directly to the SMF dataset.

### DD-5.0-SMF-04: Prometheus Bridge as First-Class Feature
**Decision:** The observability bridge is bidirectional: existing Prometheus metrics can generate SMF records (for SMF-based tooling), and SMF records can expose Prometheus metrics (for modern monitoring). This leverages the existing open-mainframe-deploy infrastructure.
