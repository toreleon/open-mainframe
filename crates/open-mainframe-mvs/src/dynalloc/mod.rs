//! Dynamic allocation (SVC 99) — allocate, deallocate, and concatenate DD entries.

pub mod dd_table;
pub mod engine;
pub mod types;

pub use dd_table::DdTable;
pub use engine::DynallocEngine;
pub use types::{
    DatasetStatus, DdEntry, Disposition, DynallocRequest, DynallocResponse, DynallocVerb, TextUnit,
    TextUnitKey,
};
