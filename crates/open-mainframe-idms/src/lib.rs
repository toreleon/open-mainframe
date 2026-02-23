#![forbid(unsafe_code)]
//! z/OS IDMS (Integrated Database Management System).
//!
//! This crate provides:
//!
//! - **CODASYL Data Model** -- record types, set types, and area definitions
//! - **Schema & Subschema DDL** -- parse and validate schema definitions
//! - **Navigational DML** -- FIND, GET, STORE, MODIFY, ERASE, CONNECT, DISCONNECT
//! - **Currency Indicators** -- track current position in the database
//! - **COBOL DML Precompiler** -- transform EXEC IDMS statements
//! - **DMCL & Physical Storage** -- page management, CALC/VIA placement
//! - **IDMS-DC Transaction Processing** -- task scheduling, pseudo-converse, queues
//! - **ADS/Online 4GL** -- dialog-driven application development
//! - **SQL Option** -- SQL access to CODASYL data
//! - **Recovery & Operations** -- journaling, rollback, warm/cold start
//! - **Lock Management** -- record/area locking with deadlock detection

pub mod ads;
pub mod codasyl;
pub mod currency;
pub mod dc;
pub mod dml;
pub mod lock;
pub mod lrf;
pub mod precompiler;
pub mod recovery;
pub mod schema;
pub mod sql_option;
pub mod storage;

pub use ads::{AdsDialog, AdsMap, AdsProcess};
pub use codasyl::{
    AreaDef, CodasylSchema, DuplicateOption, LocationMode, RecordType, SetMembership, SetMode,
    SetOrder, SetType,
};
pub use currency::{CurrencyTable, CurrencyUpdate};
pub use dc::{DcRuntime, IdmsDcTask, MapSupport, QueueArea, ScratchArea, TaskScheduler};
pub use dml::{DmlEngine, DmlResult, FindMode, RecordInstance, UsageMode};
pub use lock::{DeadlockDetector, LockManager, LockMode};
pub use lrf::{LogicalRecord, LrfEngine, PathDirection};
pub use precompiler::DmlPrecompiler;
pub use recovery::{ColdStart, JournalManager, RollbackManager, WarmStart};
pub use schema::{SchemaParser, Subschema, SubschemaParser};
pub use sql_option::{CatalogEntry, IdmsSqlEngine, IdmsSqlParser, SqlCursor, SqlDdl, SqlView};
pub use storage::{CalcRoutine, DmclConfig, PageManager, ViaPlacement};
