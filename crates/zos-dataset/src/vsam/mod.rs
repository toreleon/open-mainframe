//! VSAM (Virtual Storage Access Method) support for zOS-clone.
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
//! use zos_dataset::vsam::{VsamCluster, ClusterParams, VsamType};
//!
//! // Create a KSDS cluster
//! let params = ClusterParams::ksds("MY.VSAM.DATA", 100, 0, 10);
//! let mut cluster = VsamCluster::new(params)?;
//! cluster.create()?;
//! ```

mod btree;
mod cluster;
mod ksds;

pub use btree::{BPlusTree, DEFAULT_ORDER};
pub use cluster::{ClusterParams, KeySpec, VsamCluster, VsamType};
pub use ksds::{FileStatus, Ksds, KsdsResult};
