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
//! use zos_dataset::vsam::{BPlusTree, DEFAULT_ORDER};
//!
//! let mut tree = BPlusTree::with_order(100);
//! tree.insert(key, value);
//! if let Some(val) = tree.get(&key) {
//!     // Use value
//! }
//! ```

mod btree;

pub use btree::{BPlusTree, DEFAULT_ORDER};
