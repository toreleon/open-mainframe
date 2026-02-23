//! Dataset and file I/O for OpenMainframe.
//!
//! This crate provides mainframe-compatible file I/O operations,
//! including support for sequential (QSAM) and indexed (VSAM) datasets.
//!
//! # Example
//!
//! ```ignore
//! use open_mainframe_dataset::{Catalog, QsamReader, QsamWriter, OpenMode};
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

pub mod acs;
pub mod bsam;
pub mod catalog;
pub mod dss;
pub mod error;
pub mod gdg;
pub mod hsm;
pub mod icf;
pub mod idcams;
pub mod locking;
pub mod pds;
pub mod pdse;
pub mod qsam;
pub mod rmm;
pub mod sms;
pub mod space;
pub mod types;
pub mod vsam;

pub use bsam::{
    bpam_pds_path, bpam_read_member, parse_directory_block, read_pds_directory_blocks,
    BpamDirEntry, BsamReader, BsamWriter, Ttr, DIRECTORY_BLOCK_SIZE, TTR_SIZE,
};
pub use catalog::{Catalog, CatalogEntry};
pub use dss::{Dss, DssFilter, DumpDataset, DumpDatasetRecord, DumpHeader};
pub use error::DatasetError;
pub use gdg::{GdgBase, GdgGeneration, GdgGenerationInfo, GdgListInfo, GdgOptions, GenerationNumber};
pub use hsm::{
    Abars, AggregateGroup, BackupRecord, Hsm, HsmConfig, HsmControlDataSets, MigrationRecord,
    MigrationStatus, StorageTier,
};
pub use icf::{
    BcsEntry, BcsEntryType, CatalogIssue, CatalogLookupResult, IcfCatalog, IcfCatalogSystem, Vvds,
    VvdsRecord, VvdsRecordType,
};
pub use idcams::{Idcams, IdcamsCommand, IdcamsResult};
pub use locking::{DatasetLockManager, LockEntry, LockManager, LockMode};
pub use pds::{IspfStats, Pds, PdsMember};
pub use pdse::{Amode, Eattr, MemberGeneration, Pdse, ProgramObject, Rmode};
pub use qsam::{read_all_records, write_records, OpenMode, QsamReader, QsamWriter};
pub use rmm::{Rmm, TapeDataset, TapeVolume, VolumeState, Vrs};
pub use space::{
    DscbFormat1, Extent, ExtentList, MultiVolumeDataset, SpaceAllocation, SpaceAllocationUnit,
    SpaceError, Vtoc, BYTES_PER_CYLINDER_3390, BYTES_PER_TRACK_3390, MAX_EXTENTS,
    TRACKS_PER_CYLINDER_3390,
};
pub use sms::{
    ActiveConfiguration, DataClass, ManagementClass, SmsConfiguration, SpaceSpec, SpaceUnit,
    StorageClass, StorageGroup, StorageGroupType,
};
pub use types::{
    DatasetAttributes, DatasetOrg, DatasetRef, DispAction, DispSpec, Disposition, RecordFormat,
};
pub use vsam::{
    AixDefinition, AlternateIndex, BPlusTree, ClusterParams, Esds, EsdsResult, FileStatus,
    FreeSpaceConfig, FreeSpaceManager, FreeSpaceStatistics, KeySpec, Ksds, KsdsResult, Lds, Rrds,
    RrdsResult, Segment, SpannedRecordManager, VsamCluster, VsamPath, VsamType, DEFAULT_ORDER,
    LDS_PAGE_SIZE, SDW_SIZE,
};
