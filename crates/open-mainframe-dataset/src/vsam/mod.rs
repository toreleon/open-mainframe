//! VSAM (Virtual Storage Access Method) support for OpenMainframe.
//!
//! This module provides implementations for VSAM file types:
//! - **KSDS** (Key-Sequenced Data Set): Records accessed by primary key
//! - **ESDS** (Entry-Sequenced Data Set): Records in arrival sequence
//! - **RRDS** (Relative Record Data Set): Records by relative position
//!
//! # Architecture
//!
//! VSAM files use a B+ tree index structure for efficient key-based access.
//! The implementation supports:
//! - Point lookups in O(log n) time
//! - Range queries via linked leaf nodes
//! - Efficient insertions and deletions
//!
//! # Example
//!
//! ```ignore
//! use open_mainframe_dataset::vsam::{VsamCluster, ClusterParams, VsamType};
//!
//! // Create a KSDS cluster
//! let params = ClusterParams::ksds("MY.VSAM.DATA", 100, 0, 10);
//! let mut cluster = VsamCluster::new(params)?;
//! cluster.create()?;
//! ```

mod aix;
mod btree;
mod cluster;
mod esds;
pub mod freespace;
mod ksds;
pub mod lds;
mod rrds;
pub mod spanned;

pub use aix::{AixDefinition, AlternateIndex, VsamPath};
pub use btree::{BPlusTree, DEFAULT_ORDER};
pub use cluster::{ClusterParams, KeySpec, VsamCluster, VsamType};
pub use esds::{Esds, EsdsResult};
pub use freespace::{FreeSpaceConfig, FreeSpaceManager, FreeSpaceStatistics};
pub use ksds::{FileStatus, Ksds, KsdsResult};
pub use lds::{Lds, LDS_PAGE_SIZE};
pub use rrds::{Rrds, RrdsResult};
pub use spanned::{Segment, SpannedRecordManager, SDW_SIZE};
