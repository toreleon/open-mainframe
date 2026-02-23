#![forbid(unsafe_code)]
//! System Management Facilities (SMF).
//!
//! This crate provides:
//!
//! - **SMF Record Format** — standard header, self-defining sections, binary serde
//! - **SMFPRMxx Configuration** — TYPE/NOTYPE filtering, exits, intervals
//! - **SMF Writer** — SMFWTM/SMFEWTM record writing with buffer management
//! - **SMF Exit Framework** — IEFU83 filtering, IEFU84 subsystem ID, IFASMFEX dynamic registration
//! - **Type 30 — Job Accounting** — subtypes 1-5 for complete job lifecycle
//! - **Types 14/15/17/18 — Dataset Activity** — input, output, scratch, rename
//! - **Type 80 — Security Audit** — RACF command, access violation, logon/logoff events
//! - **Types 70-79 — Performance** — CPU, paging, workload, device activity
//! - **Types 100-120 — Subsystem** — DB2, CICS, MQ, TCP/IP records
//! - **Dump Utilities** — IFASMFDP equivalent for record extraction and filtering
//! - **Observability Bridge** — SMF <-> Prometheus metrics, SMF -> OpenTelemetry spans

pub mod record;
pub mod config;
pub mod writer;
pub mod exits;
pub mod type30;
pub mod dataset;
pub mod type80;
pub mod performance;
pub mod subsystem;
pub mod dump;
pub mod bridge;

// Re-export core record types.
pub use record::{
    SmfHeader, SmfRecord, SmfRecordError, SmfRecordRegistry, SmfRecordType,
    SmfType4, SmfType5, SmfType30, SmfSubtype30,
};

// Re-export writer types.
pub use writer::{SmfTriplet, SmfWriter, SmfWriterConfig, SmfWriterError};

// Re-export config types.
pub use config::{RecordingMode, SmfPrmConfig, SmfConfigError, SmfExitConfig};

// Re-export exit types.
pub use exits::{
    Iefu83Exit, Iefu84Exit, SmfExit, SmfExitAction, SmfExitRegistry,
};

// Re-export Type 30 job accounting.
pub use type30::{JobLifecycleCollector, Type30Record, Type30Subtype};

// Re-export dataset activity types.
pub use dataset::{
    DatasetActivityType, DatasetIoRecord, DatasetRenameRecord, DatasetScratchRecord,
};

// Re-export Type 80 security audit.
pub use type80::{
    AccessLevel, EventSeverity, SecurityEventFilter, SecurityEventType, Type80Record,
};

// Re-export performance record types.
pub use performance::{
    ServiceClassMetrics, Type70Record, Type71Record, Type72Record, Type74Record,
    VolumeDeviceMetrics,
};

// Re-export subsystem record types.
pub use subsystem::{
    CicsTransactionRecord, Db2AccountingRecord, MqStatisticsRecord, SubsystemType,
    TcpIpConnectionRecord,
};

// Re-export dump utilities.
pub use dump::{DumpFilter, DumpOutputFormat, SmfDumpError, SmfDumpProgram};

// Re-export observability bridge.
pub use bridge::{
    MetricType, OtelSpan, PrometheusMetric, PrometheusToSmf, SmfToOtel, SmfToPrometheus,
};
