//! IDCAMS command definitions.

use crate::vsam::VsamType;

/// IDCAMS commands.
#[derive(Debug, Clone)]
pub enum IdcamsCommand {
    /// DEFINE CLUSTER - Create a VSAM cluster.
    DefineCluster {
        /// Cluster name.
        name: String,
        /// Cluster type (KSDS, ESDS, RRDS).
        cluster_type: VsamType,
        /// Key specification (length, offset) for KSDS.
        keys: Option<(u16, u16)>,
        /// Record size (average, maximum).
        recordsize: Option<(u32, u32)>,
        /// Volumes.
        volumes: Option<Vec<String>>,
    },

    /// DEFINE GDG - Create a Generation Data Group.
    DefineGdg {
        /// GDG base name.
        name: String,
        /// Maximum number of generations.
        limit: u8,
        /// Delete old generations when rolling off.
        scratch: bool,
        /// Allow empty GDG.
        empty: bool,
    },

    /// DELETE - Delete datasets and catalog entries.
    Delete {
        /// Dataset name.
        name: String,
        /// Ignore retention date.
        purge: bool,
        /// Don't fail if not found.
        force: bool,
    },

    /// DEFINE NONVSAM - Catalog a non-VSAM dataset.
    DefineNonVsam {
        /// Dataset name.
        name: String,
        /// Volume serials.
        volumes: Vec<String>,
        /// Device type (e.g. 3390).
        devt: Option<String>,
    },

    /// DEFINE ALIAS - Create an HLQ alias to a user catalog.
    DefineAlias {
        /// Alias name (typically an HLQ).
        name: String,
        /// Related user catalog.
        relate: String,
    },

    /// ALTER - Modify dataset attributes.
    Alter {
        /// Dataset name.
        name: String,
        /// New name (rename).
        newname: Option<String>,
        /// Additional volumes.
        addvolumes: Option<Vec<String>>,
        /// Freespace (ci-pct, ca-pct).
        freespace: Option<(u8, u8)>,
    },

    /// LISTCAT - List catalog entries.
    Listcat {
        /// Specific entry name.
        entry: Option<String>,
        /// Level/prefix to list.
        level: Option<String>,
        /// Show all details.
        all: bool,
    },

    /// PRINT - Display dataset contents.
    Print {
        /// Dataset name.
        dataset: String,
        /// Character-only output.
        character: bool,
        /// Hex-only output.
        hex: bool,
        /// Records to skip.
        skip: usize,
        /// Number of records to print.
        count: usize,
    },

    /// REPRO - Copy datasets.
    Repro {
        /// Input dataset.
        indataset: String,
        /// Output dataset.
        outdataset: String,
        /// Starting key (for KSDS).
        fromkey: Option<String>,
        /// Ending key (for KSDS).
        tokey: Option<String>,
        /// Number of records to skip from the start.
        skip: usize,
        /// Maximum number of records to copy (0 = all).
        count: usize,
    },

    /// VERIFY - Verify VSAM integrity.
    Verify {
        /// Dataset name.
        dataset: String,
    },

    /// DEFINE ALTERNATEINDEX - Create a VSAM alternate index.
    DefineAix {
        /// AIX name.
        name: String,
        /// Base cluster name (RELATE).
        relate: String,
        /// Key specification (length, offset) for the alternate key.
        keys: (u16, u16),
        /// Whether alternate keys must be unique.
        unique_key: bool,
    },

    /// DEFINE PATH - Connect an AIX to its base cluster.
    DefinePath {
        /// Path name.
        name: String,
        /// AIX entry name (PATHENTRY).
        pathentry: String,
    },

    /// BLDINDEX - Build an alternate index from base cluster records.
    BldIndex {
        /// Base cluster (input).
        indataset: String,
        /// AIX (output).
        outdataset: String,
    },

    /// EXPORT - Export a dataset to portable format.
    Export {
        /// Dataset to export.
        dataset: String,
        /// Output DD/file name.
        outfile: String,
    },

    /// IMPORT - Import a dataset from portable format.
    Import {
        /// Input DD/file name.
        infile: String,
        /// Target dataset name.
        outdataset: String,
    },

    /// EXAMINE - Check BCS structural integrity.
    Examine {
        /// Catalog name.
        name: String,
    },

    /// DIAGNOSE - Check BCS-VVDS synchronization.
    Diagnose {
        /// Catalog name.
        name: String,
    },
}

/// Result of executing IDCAMS commands.
#[derive(Debug, Clone)]
pub struct IdcamsResult {
    /// Output messages.
    pub output: String,
    /// Return code (0 = success, 4 = warning, 8+ = error).
    pub return_code: u32,
}

impl IdcamsResult {
    /// Check if execution was successful.
    pub fn is_success(&self) -> bool {
        self.return_code == 0
    }

    /// Check if there were warnings.
    pub fn has_warnings(&self) -> bool {
        self.return_code == 4
    }

    /// Check if there were errors.
    pub fn has_errors(&self) -> bool {
        self.return_code >= 8
    }
}
