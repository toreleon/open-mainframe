//! DFSORT-compatible sort utility for OpenMainframe.
//!
//! This crate provides a sort utility compatible with IBM's DFSORT,
//! supporting SORT, MERGE, and COPY operations with record filtering
//! and reformatting capabilities.
//!
//! # Features
//!
//! - SORT with multiple key fields (CH, ZD, PD, BI, FI)
//! - INCLUDE/OMIT record filtering
//! - INREC/OUTREC record reformatting
//! - SUM for duplicate key handling
//! - MERGE of pre-sorted files
//! - COPY without sorting
//!
//! # Example
//!
//! ```ignore
//! use open_mainframe_sort::{SortEngine, SortSpec, SortField, DataType, SortOrder};
//!
//! let spec = SortSpec::new()
//!     .add_field(SortField::new(1, 10, DataType::Character, SortOrder::Ascending));
//!
//! let mut engine = SortEngine::new(spec);
//! engine.sort_file("input.dat", "output.dat")?;
//! ```

mod error;
mod fields;
mod parser;
mod engine;
mod filter;
pub mod ifthen;
pub mod outfil;
mod reformat;
pub mod joinkeys;
pub mod icetool;
pub mod symbols;

pub use error::SortError;
pub use fields::{DataType, SortField, SortOrder, SortSpec};
pub use parser::parse_control_statements;
pub use engine::SortEngine;
pub use filter::{Condition, FilterSpec, CompareOp};
pub use ifthen::{IfThenSpec, IfThenClause, WhenCondition, IfThenAction, OverlayField, OverlayData};
pub use outfil::{
    OutfilSpec, OutfilDescriptor, SplitMode, HeaderTrailerSpec, HeaderSegment,
    DateFormat, TimeFormat,
};
pub use reformat::{OutrecField, OutrecSpec, OverlaySpec, FindRepSpec, datetime};
pub use joinkeys::{JoinKeysSpec, JoinKeyField, JoinType, ReformatField as JoinReformatField};
pub use icetool::{IceToolOp, IceToolResult, FieldStats, DisplayColumn, OnField};
