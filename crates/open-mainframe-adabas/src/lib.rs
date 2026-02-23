#![forbid(unsafe_code)]
//! z/OS ADABAS (Adaptable Database System).
//!
//! This crate provides:
//!
//! - **Inverted-List Storage** — Data Storage, Associator, Address Converter
//! - **FDT & Field System** — Field Definition Table, field types, groups, MU/PE
//! - **Descriptor Engine** — standard, super, sub, phonetic, hyper descriptors
//! - **Direct Call Interface (ACB)** — ADABAS Control Block, format/record buffers
//! - **Search Commands** — S1/S2/S4/S8, search criteria with AND/OR/NOT logic
//! - **Read Commands** — L1-L6, physical/logical sequential, sorted reads
//! - **Modification Commands** — N1/N2 (store), A1 (update), E1 (delete)
//! - **Transaction Management** — ET/BT, hold queue, CLP, transaction log
//! - **Nucleus & Logging** — command queue, protection log, work pool
//! - **Utilities & DDM** — ADALOD, ADAUNI, ADASAV, Data Definition Module

pub mod acb;
pub mod descriptor;
pub mod fdt;
pub mod modify;
pub mod nucleus;
pub mod read;
pub mod search;
pub mod storage;
pub mod transaction;
pub mod utilities;

// ── Re-exports ─────────────────────────────────────────────────────

pub use acb::{Acb, AcbCommand, AcbResult, FieldRef, FormatBuffer, parse_format_buffer};
pub use descriptor::{
    Descriptor, DescriptorSet, HyperDescriptor, PhoneticDescriptor, SubDescriptor,
    SuperDescriptor,
};
pub use fdt::{Fdt, FieldDef, FieldType, GroupField, MultipleValueField};
pub use modify::{DeleteCommand, StoreCommand, UpdateCommand, UpdateDescriptors};
pub use nucleus::{
    AdabasNucleus, CommandQueue, NucleusParams, ProtectionLog, ProtectionLogEntry, WorkPool,
};
pub use read::{ReadCommand, ReadCursor, ReadOptions, ReadResult};
pub use search::{
    Isnlist, LogicalOp, SearchBuffer, SearchCommand, SearchCriteria, SearchOperator,
};
pub use storage::{
    AddressConverter, AdabasFile, AssociatorStorage, DataStorage, InvertedList, Isn, Rabn,
};
pub use transaction::{
    ClpNumber, HoldQueue, LogOperation, TransactionId, TransactionLog, TransactionLogEntry,
    TransactionManager, TransactionState,
};
pub use utilities::{
    AdalodUtility, AdasavUtility, AdauniUtility, BackupImage, Ddm, DdmEntry, LoadMode,
    SaveOperation,
};

// ── Error ──────────────────────────────────────────────────────────

/// Errors produced by the ADABAS subsystem.
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum AdabasError {
    /// ISN not found in address converter.
    #[error("ISN {isn} not found")]
    IsnNotFound {
        /// The ISN that was not found.
        isn: u64,
    },

    /// RABN not found in data storage.
    #[error("RABN {rabn} not found")]
    RabnNotFound {
        /// The RABN that was not found.
        rabn: u64,
    },

    /// Duplicate field name in FDT.
    #[error("duplicate field '{name}' in FDT")]
    DuplicateField {
        /// The duplicate field name.
        name: String,
    },

    /// Maximum occurrences exceeded for MU/PE field.
    #[error("max occurrences ({max}) exceeded for field '{field}'")]
    MaxOccurrencesExceeded {
        /// The field name.
        field: String,
        /// The maximum allowed.
        max: u16,
    },

    /// Invalid ACB command code.
    #[error("invalid command code '{code}'")]
    InvalidCommand {
        /// The invalid code.
        code: String,
    },

    /// Invalid format buffer specification.
    #[error("invalid format buffer: '{spec}'")]
    InvalidFormatBuffer {
        /// The invalid specification.
        spec: String,
    },

    /// Invalid search operator.
    #[error("invalid search operator '{op}'")]
    InvalidSearchOperator {
        /// The invalid operator.
        op: String,
    },

    /// File not found in nucleus.
    #[error("file {file_number} not found")]
    FileNotFound {
        /// The missing file number.
        file_number: u16,
    },

    /// Maximum files exceeded.
    #[error("max files ({max}) exceeded")]
    MaxFilesExceeded {
        /// The maximum allowed.
        max: u16,
    },

    /// File is not empty (for initial load).
    #[error("file {file_number} is not empty")]
    FileNotEmpty {
        /// The file number.
        file_number: u16,
    },

    /// Hold queue is full.
    #[error("hold queue full (max {max})")]
    HoldQueueFull {
        /// The maximum capacity.
        max: usize,
    },

    /// Transaction not found.
    #[error("transaction {id} not found")]
    TransactionNotFound {
        /// The transaction ID.
        id: u64,
    },

    /// Command queue is full.
    #[error("command queue full (capacity {capacity})")]
    CommandQueueFull {
        /// The queue capacity.
        capacity: usize,
    },

    /// Work pool memory exhausted.
    #[error("work pool exhausted: requested {requested} bytes, {available} available")]
    WorkPoolExhausted {
        /// Bytes requested.
        requested: usize,
        /// Bytes available.
        available: usize,
    },
}
