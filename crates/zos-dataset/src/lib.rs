//! Dataset and file I/O for zOS-clone.
//!
//! This crate provides mainframe-compatible file I/O operations,
//! including support for sequential (QSAM) and indexed (VSAM) datasets.
//!
//! # Example
//!
//! ```ignore
//! use zos_dataset::{Catalog, QsamReader, QsamWriter, OpenMode};
//!
//! // Look up a dataset
//! let catalog = Catalog::default();
//! let dataset = catalog.lookup("MY.DATA.FILE")?;
//!
//! // Read records
//! let mut reader = QsamReader::open(dataset)?;
//! while let Some(record) = reader.read()? {
//!     // Process record
//! }
//! ```
//!
//! # Features
//!
//! - Sequential file access (QSAM)
//! - VSAM file support (KSDS, ESDS, RRDS)
//! - Record formats: Fixed, Variable, Undefined
//! - Dataset catalog for name resolution
//! - PDS member support

pub mod catalog;
pub mod error;
pub mod qsam;
pub mod types;
pub mod vsam;

pub use catalog::{Catalog, CatalogEntry};
pub use error::DatasetError;
pub use qsam::{read_all_records, write_records, OpenMode, QsamReader, QsamWriter};
pub use types::{
    DatasetAttributes, DatasetOrg, DatasetRef, DispAction, DispSpec, Disposition, RecordFormat,
};
pub use vsam::{
    BPlusTree, ClusterParams, FileStatus, KeySpec, Ksds, KsdsResult, VsamCluster, VsamType,
    DEFAULT_ORDER,
};
