//! Dataset types and configurations.
//!
//! This module defines the core types for IBM mainframe datasets,
//! including record formats, dataset organizations, and attributes.

use std::path::PathBuf;

/// Record format (RECFM).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RecordFormat {
    /// Fixed-length records.
    #[default]
    Fixed,
    /// Fixed-length blocked records.
    FixedBlocked,
    /// Variable-length records (4-byte RDW prefix).
    Variable,
    /// Variable-length blocked records.
    VariableBlocked,
    /// Undefined-length records.
    Undefined,
    /// Variable spanned records.
    VariableSpanned,
    /// Variable blocked spanned records.
    VariableBlockedSpanned,
}

impl RecordFormat {
    /// Parse record format from string (e.g., "FB", "VB").
    pub fn parse(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "F" => Some(RecordFormat::Fixed),
            "FB" => Some(RecordFormat::FixedBlocked),
            "V" => Some(RecordFormat::Variable),
            "VB" => Some(RecordFormat::VariableBlocked),
            "U" => Some(RecordFormat::Undefined),
            "VS" => Some(RecordFormat::VariableSpanned),
            "VBS" => Some(RecordFormat::VariableBlockedSpanned),
            _ => None,
        }
    }

    /// Check if this format uses blocking.
    pub fn is_blocked(&self) -> bool {
        matches!(
            self,
            RecordFormat::FixedBlocked
                | RecordFormat::VariableBlocked
                | RecordFormat::VariableBlockedSpanned
        )
    }

    /// Check if this format uses variable-length records.
    pub fn is_variable(&self) -> bool {
        matches!(
            self,
            RecordFormat::Variable
                | RecordFormat::VariableBlocked
                | RecordFormat::VariableSpanned
                | RecordFormat::VariableBlockedSpanned
        )
    }

    /// Get the record descriptor word (RDW) size for variable formats.
    pub fn rdw_size(&self) -> usize {
        if self.is_variable() {
            4 // 2-byte length + 2 bytes reserved
        } else {
            0
        }
    }
}

/// Dataset organization (DSORG).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum DatasetOrg {
    /// Physical sequential (PS).
    #[default]
    Sequential,
    /// Partitioned organization (PO) - PDS.
    Partitioned,
    /// Direct access (DA).
    Direct,
    /// Indexed sequential (IS) - ISAM.
    IndexedSequential,
    /// VSAM Entry-Sequenced (ESDS).
    VsamEntrySequenced,
    /// VSAM Key-Sequenced (KSDS).
    VsamKeySequenced,
    /// VSAM Relative Record (RRDS).
    VsamRelativeRecord,
}

impl DatasetOrg {
    /// Parse organization from string.
    pub fn parse(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "PS" => Some(DatasetOrg::Sequential),
            "PO" => Some(DatasetOrg::Partitioned),
            "DA" => Some(DatasetOrg::Direct),
            "IS" => Some(DatasetOrg::IndexedSequential),
            "ESDS" => Some(DatasetOrg::VsamEntrySequenced),
            "KSDS" => Some(DatasetOrg::VsamKeySequenced),
            "RRDS" => Some(DatasetOrg::VsamRelativeRecord),
            _ => None,
        }
    }
}

/// Dataset attributes (DCB - Data Control Block).
#[derive(Debug, Clone)]
pub struct DatasetAttributes {
    /// Record format.
    pub recfm: RecordFormat,
    /// Logical record length.
    pub lrecl: u32,
    /// Block size.
    pub blksize: u32,
    /// Dataset organization.
    pub dsorg: DatasetOrg,
    /// Key length (for VSAM KSDS).
    pub keylen: Option<u32>,
    /// Key offset (for VSAM KSDS).
    pub keyoff: Option<u32>,
}

impl Default for DatasetAttributes {
    fn default() -> Self {
        Self {
            recfm: RecordFormat::FixedBlocked,
            lrecl: 80,
            blksize: 800,
            dsorg: DatasetOrg::Sequential,
            keylen: None,
            keyoff: None,
        }
    }
}

impl DatasetAttributes {
    /// Create attributes for a typical COBOL source file.
    pub fn cobol_source() -> Self {
        Self {
            recfm: RecordFormat::FixedBlocked,
            lrecl: 80,
            blksize: 3200,
            dsorg: DatasetOrg::Sequential,
            keylen: None,
            keyoff: None,
        }
    }

    /// Create attributes for a typical print file.
    pub fn print_file() -> Self {
        Self {
            recfm: RecordFormat::FixedBlocked,
            lrecl: 133, // 132 + 1 for carriage control
            blksize: 1330,
            dsorg: DatasetOrg::Sequential,
            keylen: None,
            keyoff: None,
        }
    }

    /// Calculate records per block.
    pub fn records_per_block(&self) -> u32 {
        if self.recfm.is_blocked() && self.lrecl > 0 {
            self.blksize / self.lrecl
        } else {
            1
        }
    }

    /// Validate attributes consistency.
    pub fn validate(&self) -> Result<(), String> {
        if self.lrecl == 0 {
            return Err("LRECL must be greater than 0".to_string());
        }

        if self.recfm.is_blocked() && self.blksize < self.lrecl {
            return Err("BLKSIZE must be >= LRECL for blocked formats".to_string());
        }

        if self.recfm.is_blocked() && self.blksize % self.lrecl != 0 {
            return Err("BLKSIZE should be a multiple of LRECL for blocked formats".to_string());
        }

        Ok(())
    }
}

/// Dataset disposition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Disposition {
    /// New dataset being created.
    New,
    /// Existing dataset, exclusive access.
    Old,
    /// Existing dataset, shared access.
    #[default]
    Shr,
    /// Existing dataset, append mode.
    Mod,
}

/// Action to take on dataset after job step.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum DispAction {
    /// Keep the dataset.
    #[default]
    Keep,
    /// Delete the dataset.
    Delete,
    /// Catalog the dataset.
    Catlg,
    /// Uncatalog the dataset.
    Uncatlg,
    /// Pass to next step.
    Pass,
}

/// Full disposition specification (status, normal, abnormal).
#[derive(Debug, Clone, Default)]
pub struct DispSpec {
    /// Initial status.
    pub status: Disposition,
    /// Normal termination action.
    pub normal: DispAction,
    /// Abnormal termination action.
    pub abnormal: DispAction,
}

/// A dataset reference with all allocation information.
#[derive(Debug, Clone)]
pub struct DatasetRef {
    /// Dataset name (e.g., "USER.DATA.FILE").
    pub dsn: String,
    /// Member name for PDS (e.g., "MEMBER").
    pub member: Option<String>,
    /// Disposition.
    pub disp: DispSpec,
    /// Dataset attributes.
    pub attributes: DatasetAttributes,
    /// Physical file path (resolved).
    pub path: Option<PathBuf>,
}

impl DatasetRef {
    /// Create a new dataset reference.
    pub fn new(dsn: impl Into<String>) -> Self {
        Self {
            dsn: dsn.into(),
            member: None,
            disp: DispSpec::default(),
            attributes: DatasetAttributes::default(),
            path: None,
        }
    }

    /// Set member name.
    pub fn with_member(mut self, member: impl Into<String>) -> Self {
        self.member = Some(member.into());
        self
    }

    /// Set disposition.
    pub fn with_disp(mut self, disp: DispSpec) -> Self {
        self.disp = disp;
        self
    }

    /// Set attributes.
    pub fn with_attributes(mut self, attrs: DatasetAttributes) -> Self {
        self.attributes = attrs;
        self
    }

    /// Get full dataset name including member.
    pub fn full_name(&self) -> String {
        if let Some(ref member) = self.member {
            format!("{}({})", self.dsn, member)
        } else {
            self.dsn.clone()
        }
    }

    /// Parse a dataset name that may include member.
    pub fn parse(name: &str) -> Self {
        if let Some(paren_pos) = name.find('(') {
            let dsn = &name[..paren_pos];
            let member_end = name.find(')').unwrap_or(name.len());
            let member = &name[paren_pos + 1..member_end];
            Self::new(dsn).with_member(member)
        } else {
            Self::new(name)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_record_format_parse() {
        assert_eq!(RecordFormat::parse("FB"), Some(RecordFormat::FixedBlocked));
        assert_eq!(
            RecordFormat::parse("vb"),
            Some(RecordFormat::VariableBlocked)
        );
        assert_eq!(RecordFormat::parse("F"), Some(RecordFormat::Fixed));
        assert_eq!(RecordFormat::parse("XX"), None);
    }

    #[test]
    fn test_record_format_properties() {
        assert!(RecordFormat::FixedBlocked.is_blocked());
        assert!(!RecordFormat::Fixed.is_blocked());
        assert!(RecordFormat::Variable.is_variable());
        assert!(!RecordFormat::Fixed.is_variable());
        assert_eq!(RecordFormat::Variable.rdw_size(), 4);
        assert_eq!(RecordFormat::Fixed.rdw_size(), 0);
    }

    #[test]
    fn test_dataset_attributes_validation() {
        let attrs = DatasetAttributes::cobol_source();
        assert!(attrs.validate().is_ok());

        let bad_attrs = DatasetAttributes {
            lrecl: 0,
            ..Default::default()
        };
        assert!(bad_attrs.validate().is_err());
    }

    #[test]
    fn test_dataset_ref_parse() {
        let ds = DatasetRef::parse("MY.DATA.SET");
        assert_eq!(ds.dsn, "MY.DATA.SET");
        assert!(ds.member.is_none());

        let ds = DatasetRef::parse("MY.PDS(MEMBER)");
        assert_eq!(ds.dsn, "MY.PDS");
        assert_eq!(ds.member, Some("MEMBER".to_string()));
    }

    #[test]
    fn test_records_per_block() {
        let attrs = DatasetAttributes {
            recfm: RecordFormat::FixedBlocked,
            lrecl: 80,
            blksize: 800,
            ..Default::default()
        };
        assert_eq!(attrs.records_per_block(), 10);
    }
}
