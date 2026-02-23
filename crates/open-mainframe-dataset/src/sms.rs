//! # SMS (Storage Management Subsystem) Construct Data Model
//!
//! Defines data structures for SMS constructs used by DFSMS to manage
//! dataset allocation, performance, lifecycle, and storage placement.
//!
//! ## Constructs
//!
//! - **Data Class** — default allocation attributes (RECFM, LRECL, BLKSIZE, SPACE)
//! - **Storage Class** — performance and availability goals
//! - **Management Class** — lifecycle policies (backup, migration, retention)
//! - **Storage Group** — volume pool definitions (POOL, VIO, DUMMY, TAPE)
//!
//! ## Configuration Model
//!
//! - **SCDS** (Source Control Data Set) — editable configuration
//! - **ACDS** (Active Control Data Set) — activated snapshot used at runtime

use std::collections::HashMap;

use crate::types::RecordFormat;

// ─────────────────────── Space Unit ───────────────────────

/// Unit for space allocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpaceUnit {
    /// Tracks (~56KB per track on 3390).
    Tracks,
    /// Cylinders (~850KB per cylinder on 3390).
    Cylinders,
    /// Bytes (block size).
    Bytes(u32),
    /// Records.
    Records,
}

/// Space allocation specification.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpaceSpec {
    /// Allocation unit.
    pub unit: SpaceUnit,
    /// Primary quantity.
    pub primary: u32,
    /// Secondary quantity.
    pub secondary: u32,
    /// Directory blocks (for PDS).
    pub directory: u32,
    /// Release unused space on close.
    pub rlse: bool,
}

impl SpaceSpec {
    /// Create a new space spec.
    pub fn new(unit: SpaceUnit, primary: u32, secondary: u32) -> Self {
        Self {
            unit,
            primary,
            secondary,
            directory: 0,
            rlse: false,
        }
    }

    /// Create a tracks spec.
    pub fn tracks(primary: u32, secondary: u32) -> Self {
        Self::new(SpaceUnit::Tracks, primary, secondary)
    }

    /// Create a cylinders spec.
    pub fn cylinders(primary: u32, secondary: u32) -> Self {
        Self::new(SpaceUnit::Cylinders, primary, secondary)
    }
}

// ─────────────────────── Data Class ───────────────────────

/// Data Class — default allocation attributes.
///
/// When a dataset is allocated with `DATACLAS=name`, these defaults
/// are used unless explicitly overridden in JCL.
#[derive(Debug, Clone)]
pub struct DataClass {
    /// Data class name (up to 8 characters, uppercase).
    pub name: String,
    /// Description.
    pub description: String,
    /// Default record format.
    pub recfm: Option<RecordFormat>,
    /// Default logical record length.
    pub lrecl: Option<u32>,
    /// Default block size.
    pub blksize: Option<u32>,
    /// Default space allocation.
    pub space: Option<SpaceSpec>,
    /// Default dataset organization (PS, PO, DA, VS).
    pub dsorg: Option<String>,
    /// Retention period in days.
    pub retpd: Option<u32>,
    /// Average record unit (for variable-length).
    pub avgrec: Option<String>,
    /// COMPACTION (Y/N) — data compaction.
    pub compaction: bool,
}

impl DataClass {
    /// Create a new data class with the given name.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            description: String::new(),
            recfm: None,
            lrecl: None,
            blksize: None,
            space: None,
            dsorg: None,
            retpd: None,
            avgrec: None,
            compaction: false,
        }
    }

    /// Builder: set RECFM.
    pub fn with_recfm(mut self, recfm: RecordFormat) -> Self {
        self.recfm = Some(recfm);
        self
    }

    /// Builder: set LRECL.
    pub fn with_lrecl(mut self, lrecl: u32) -> Self {
        self.lrecl = Some(lrecl);
        self
    }

    /// Builder: set BLKSIZE.
    pub fn with_blksize(mut self, blksize: u32) -> Self {
        self.blksize = Some(blksize);
        self
    }

    /// Builder: set SPACE.
    pub fn with_space(mut self, space: SpaceSpec) -> Self {
        self.space = Some(space);
        self
    }

    /// Builder: set retention period in days.
    pub fn with_retpd(mut self, days: u32) -> Self {
        self.retpd = Some(days);
        self
    }
}

// ─────────────────────── Storage Class ───────────────────────

/// Accessibility level for storage class.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Accessibility {
    /// Dataset always accessible.
    Continuous,
    /// Brief outages acceptable.
    Standard,
    /// Longer outages acceptable.
    Delayed,
}

/// Storage Class — performance and availability goals.
///
/// Assigned via `STORCLAS=name` in JCL. Determines which volumes are
/// eligible for dataset placement based on performance requirements.
#[derive(Debug, Clone)]
pub struct StorageClass {
    /// Storage class name (up to 8 characters, uppercase).
    pub name: String,
    /// Description.
    pub description: String,
    /// Whether space is guaranteed at allocation time.
    pub guaranteed_space: bool,
    /// Accessibility level.
    pub accessibility: Accessibility,
    /// I/O priority (1-255, higher = higher priority).
    pub io_priority: Option<u8>,
    /// Direct millisecond response time goal.
    pub direct_ms_response: Option<u32>,
    /// Sequential millisecond response time goal.
    pub sequential_ms_response: Option<u32>,
    /// Multi-tiered storage eligible.
    pub multi_tiered: bool,
}

impl StorageClass {
    /// Create a new storage class with the given name.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            description: String::new(),
            guaranteed_space: false,
            accessibility: Accessibility::Standard,
            io_priority: None,
            direct_ms_response: None,
            sequential_ms_response: None,
            multi_tiered: false,
        }
    }

    /// Builder: set guaranteed space.
    pub fn with_guaranteed_space(mut self, guaranteed: bool) -> Self {
        self.guaranteed_space = guaranteed;
        self
    }

    /// Builder: set accessibility.
    pub fn with_accessibility(mut self, accessibility: Accessibility) -> Self {
        self.accessibility = accessibility;
        self
    }

    /// Builder: set I/O priority.
    pub fn with_io_priority(mut self, priority: u8) -> Self {
        self.io_priority = Some(priority);
        self
    }
}

// ─────────────────────── Management Class ───────────────────────

/// Backup frequency for management class.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackupFrequency {
    /// No automatic backup.
    None,
    /// Backup daily.
    Daily,
    /// Backup weekly.
    Weekly,
    /// Backup on every close.
    OnClose,
}

/// Management Class — lifecycle policies.
///
/// Controls backup, migration, and retention for datasets assigned
/// via `MGMTCLAS=name` in JCL. DFSMShsm uses these policies.
#[derive(Debug, Clone)]
pub struct ManagementClass {
    /// Management class name (up to 8 characters, uppercase).
    pub name: String,
    /// Description.
    pub description: String,
    /// Automatic backup frequency.
    pub backup_frequency: BackupFrequency,
    /// Number of backup versions to retain.
    pub versions_backup: u32,
    /// Days since last reference before migration from ML0 to ML1.
    pub migration_age: Option<u32>,
    /// Days since last reference before migration from ML1 to ML2.
    pub secondary_migration_age: Option<u32>,
    /// Retention period in days.
    pub retention: Option<u32>,
    /// Whether the dataset should expire based on date or last-reference.
    pub expire_after_days_unused: Option<u32>,
    /// Command or auto-class for migration.
    pub command_or_auto: bool,
    /// Associated storage group name (for cross-reference validation).
    pub storage_group: Option<String>,
}

impl ManagementClass {
    /// Create a new management class with the given name.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            description: String::new(),
            backup_frequency: BackupFrequency::None,
            versions_backup: 1,
            migration_age: None,
            secondary_migration_age: None,
            retention: None,
            expire_after_days_unused: None,
            command_or_auto: true,
            storage_group: None,
        }
    }

    /// Builder: set backup frequency.
    pub fn with_backup_frequency(mut self, freq: BackupFrequency) -> Self {
        self.backup_frequency = freq;
        self
    }

    /// Builder: set versions to keep.
    pub fn with_versions_backup(mut self, versions: u32) -> Self {
        self.versions_backup = versions;
        self
    }

    /// Builder: set migration age (days).
    pub fn with_migration_age(mut self, days: u32) -> Self {
        self.migration_age = Some(days);
        self
    }

    /// Builder: set retention period (days).
    pub fn with_retention(mut self, days: u32) -> Self {
        self.retention = Some(days);
        self
    }
}

// ─────────────────────── Storage Group ───────────────────────

/// Type of storage group.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StorageGroupType {
    /// POOL — volumes for permanent datasets.
    Pool,
    /// VIO — Virtual I/O for temporary datasets.
    Vio,
    /// DUMMY — placeholder that rejects all allocations.
    Dummy,
    /// TAPE — tape volume pool.
    Tape,
}

/// Volume status within a storage group.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VolumeStatus {
    /// Volume is enabled for new allocations.
    Enabled,
    /// Volume is disabled (no new allocations, existing data accessible).
    Disabled,
    /// Volume is quiesced (no new allocations, existing data accessible, draining).
    Quiesced,
}

/// A volume entry within a storage group.
#[derive(Debug, Clone)]
pub struct StorageGroupVolume {
    /// Volume serial (6 characters).
    pub volser: String,
    /// Volume status.
    pub status: VolumeStatus,
}

/// Storage Group — volume pool definition.
///
/// Determines which volumes are eligible for dataset placement.
/// Assigned by ACS STORGRP routine or JCL.
#[derive(Debug, Clone)]
pub struct StorageGroup {
    /// Storage group name (up to 8 characters, uppercase).
    pub name: String,
    /// Description.
    pub description: String,
    /// Storage group type.
    pub group_type: StorageGroupType,
    /// Volumes in this group.
    pub volumes: Vec<StorageGroupVolume>,
    /// Auto-migrate threshold (percentage) — migrate when volume exceeds this.
    pub high_threshold: Option<u8>,
    /// Stop migration threshold (percentage).
    pub low_threshold: Option<u8>,
    /// SMS-managed (should always be true for SMS groups).
    pub sms_managed: bool,
}

impl StorageGroup {
    /// Create a new storage group with the given name and type.
    pub fn new(name: &str, group_type: StorageGroupType) -> Self {
        Self {
            name: name.to_uppercase(),
            description: String::new(),
            group_type,
            volumes: Vec::new(),
            high_threshold: None,
            low_threshold: None,
            sms_managed: true,
        }
    }

    /// Builder: add a volume.
    pub fn with_volume(mut self, volser: &str, status: VolumeStatus) -> Self {
        self.volumes.push(StorageGroupVolume {
            volser: volser.to_uppercase(),
            status,
        });
        self
    }

    /// Builder: set thresholds.
    pub fn with_thresholds(mut self, high: u8, low: u8) -> Self {
        self.high_threshold = Some(high);
        self.low_threshold = Some(low);
        self
    }

    /// Count of enabled volumes.
    pub fn enabled_volume_count(&self) -> usize {
        self.volumes
            .iter()
            .filter(|v| v.status == VolumeStatus::Enabled)
            .count()
    }
}

// ─────────────────────── SCDS / ACDS ───────────────────────

/// SMS Configuration — Source Control Data Set (SCDS).
///
/// Holds the editable SMS configuration. When activated, a snapshot
/// becomes the Active Control Data Set (ACDS).
#[derive(Debug, Clone, Default)]
pub struct SmsConfiguration {
    /// Data classes indexed by name.
    pub data_classes: HashMap<String, DataClass>,
    /// Storage classes indexed by name.
    pub storage_classes: HashMap<String, StorageClass>,
    /// Management classes indexed by name.
    pub management_classes: HashMap<String, ManagementClass>,
    /// Storage groups indexed by name.
    pub storage_groups: HashMap<String, StorageGroup>,
}

/// Validation issue severity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValidationSeverity {
    /// Warning — construct may have issues.
    Warning,
    /// Error — construct is invalid and must be fixed before activation.
    Error,
}

/// A validation issue found during construct validation.
#[derive(Debug, Clone)]
pub struct ValidationIssue {
    /// Severity.
    pub severity: ValidationSeverity,
    /// Construct type (e.g., "DataClass", "ManagementClass").
    pub construct_type: String,
    /// Construct name.
    pub construct_name: String,
    /// Issue description.
    pub message: String,
}

impl SmsConfiguration {
    /// Create a new empty SMS configuration.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a data class.
    pub fn add_data_class(&mut self, dc: DataClass) {
        self.data_classes.insert(dc.name.clone(), dc);
    }

    /// Add a storage class.
    pub fn add_storage_class(&mut self, sc: StorageClass) {
        self.storage_classes.insert(sc.name.clone(), sc);
    }

    /// Add a management class.
    pub fn add_management_class(&mut self, mc: ManagementClass) {
        self.management_classes.insert(mc.name.clone(), mc);
    }

    /// Add a storage group.
    pub fn add_storage_group(&mut self, sg: StorageGroup) {
        self.storage_groups.insert(sg.name.clone(), sg);
    }

    /// Look up a data class by name.
    pub fn get_data_class(&self, name: &str) -> Option<&DataClass> {
        self.data_classes.get(&name.to_uppercase())
    }

    /// Look up a storage class by name.
    pub fn get_storage_class(&self, name: &str) -> Option<&StorageClass> {
        self.storage_classes.get(&name.to_uppercase())
    }

    /// Look up a management class by name.
    pub fn get_management_class(&self, name: &str) -> Option<&ManagementClass> {
        self.management_classes.get(&name.to_uppercase())
    }

    /// Look up a storage group by name.
    pub fn get_storage_group(&self, name: &str) -> Option<&StorageGroup> {
        self.storage_groups.get(&name.to_uppercase())
    }

    /// Validate all constructs for consistency.
    ///
    /// Returns a list of validation issues. Empty list means the
    /// configuration is valid and can be activated.
    pub fn validate(&self) -> Vec<ValidationIssue> {
        let mut issues = Vec::new();

        // Validate data classes
        for dc in self.data_classes.values() {
            // FB requires LRECL
            if dc.recfm == Some(RecordFormat::Fixed) && dc.lrecl.is_none() {
                issues.push(ValidationIssue {
                    severity: ValidationSeverity::Warning,
                    construct_type: "DataClass".to_string(),
                    construct_name: dc.name.clone(),
                    message: "RECFM=FB but LRECL not specified".to_string(),
                });
            }
            // BLKSIZE should be multiple of LRECL for FB
            if let (Some(RecordFormat::Fixed), Some(lrecl), Some(blksize)) =
                (dc.recfm, dc.lrecl, dc.blksize)
            {
                if lrecl > 0 && blksize % lrecl != 0 {
                    issues.push(ValidationIssue {
                        severity: ValidationSeverity::Warning,
                        construct_type: "DataClass".to_string(),
                        construct_name: dc.name.clone(),
                        message: format!(
                            "BLKSIZE {blksize} is not a multiple of LRECL {lrecl}"
                        ),
                    });
                }
            }
        }

        // Validate management classes — check storage group references
        for mc in self.management_classes.values() {
            if let Some(sg_name) = &mc.storage_group {
                if !self.storage_groups.contains_key(sg_name) {
                    issues.push(ValidationIssue {
                        severity: ValidationSeverity::Error,
                        construct_type: "ManagementClass".to_string(),
                        construct_name: mc.name.clone(),
                        message: format!(
                            "References non-existent storage group '{sg_name}'"
                        ),
                    });
                }
            }
        }

        // Validate storage groups — must have at least one volume for POOL type
        for sg in self.storage_groups.values() {
            if sg.group_type == StorageGroupType::Pool && sg.volumes.is_empty() {
                issues.push(ValidationIssue {
                    severity: ValidationSeverity::Warning,
                    construct_type: "StorageGroup".to_string(),
                    construct_name: sg.name.clone(),
                    message: "POOL storage group has no volumes".to_string(),
                });
            }
            // Threshold validation
            if let (Some(high), Some(low)) = (sg.high_threshold, sg.low_threshold) {
                if low >= high {
                    issues.push(ValidationIssue {
                        severity: ValidationSeverity::Error,
                        construct_type: "StorageGroup".to_string(),
                        construct_name: sg.name.clone(),
                        message: format!(
                            "Low threshold ({low}) must be less than high threshold ({high})"
                        ),
                    });
                }
            }
        }

        issues
    }

    /// Check if the configuration has errors (not just warnings).
    pub fn has_errors(&self) -> bool {
        self.validate()
            .iter()
            .any(|i| i.severity == ValidationSeverity::Error)
    }

    /// Activate this SCDS into an ACDS (returns a clone as the active snapshot).
    ///
    /// Fails if there are validation errors.
    pub fn activate(&self) -> Result<ActiveConfiguration, Vec<ValidationIssue>> {
        let issues = self.validate();
        let errors: Vec<_> = issues
            .iter()
            .filter(|i| i.severity == ValidationSeverity::Error)
            .cloned()
            .collect();
        if !errors.is_empty() {
            return Err(errors);
        }
        Ok(ActiveConfiguration {
            config: self.clone(),
        })
    }
}

/// Active Control Data Set (ACDS) — the activated SMS configuration.
///
/// Created by activating an SCDS. Used at runtime for SMS construct lookups.
#[derive(Debug, Clone)]
pub struct ActiveConfiguration {
    config: SmsConfiguration,
}

impl ActiveConfiguration {
    /// Look up a data class by name.
    pub fn get_data_class(&self, name: &str) -> Option<&DataClass> {
        self.config.get_data_class(name)
    }

    /// Look up a storage class by name.
    pub fn get_storage_class(&self, name: &str) -> Option<&StorageClass> {
        self.config.get_storage_class(name)
    }

    /// Look up a management class by name.
    pub fn get_management_class(&self, name: &str) -> Option<&ManagementClass> {
        self.config.get_management_class(name)
    }

    /// Look up a storage group by name.
    pub fn get_storage_group(&self, name: &str) -> Option<&StorageGroup> {
        self.config.get_storage_group(name)
    }

    /// Get the underlying configuration.
    pub fn config(&self) -> &SmsConfiguration {
        &self.config
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::RecordFormat;

    // ─── DFSMS-100.1: Data Class Definition ───

    #[test]
    fn test_data_class_basic() {
        let dc = DataClass::new("DCSTD80")
            .with_recfm(RecordFormat::Fixed)
            .with_lrecl(80)
            .with_blksize(27920)
            .with_space(SpaceSpec::tracks(10, 5));

        assert_eq!(dc.name, "DCSTD80");
        assert_eq!(dc.recfm, Some(RecordFormat::Fixed));
        assert_eq!(dc.lrecl, Some(80));
        assert_eq!(dc.blksize, Some(27920));
        assert!(dc.space.is_some());
        let space = dc.space.unwrap();
        assert_eq!(space.unit, SpaceUnit::Tracks);
        assert_eq!(space.primary, 10);
        assert_eq!(space.secondary, 5);
    }

    #[test]
    fn test_data_class_retention() {
        let dc = DataClass::new("DCRETAIN").with_retpd(30);
        assert_eq!(dc.retpd, Some(30));
    }

    #[test]
    fn test_data_class_uppercase() {
        let dc = DataClass::new("dcstd80");
        assert_eq!(dc.name, "DCSTD80");
    }

    #[test]
    fn test_data_class_in_scds() {
        let mut scds = SmsConfiguration::new();
        let dc = DataClass::new("DCSTD80")
            .with_recfm(RecordFormat::Fixed)
            .with_lrecl(80)
            .with_blksize(27920);
        scds.add_data_class(dc);

        let loaded = scds.get_data_class("DCSTD80").unwrap();
        assert_eq!(loaded.lrecl, Some(80));
        assert_eq!(loaded.blksize, Some(27920));
    }

    // ─── DFSMS-100.2: Storage Class Definition ───

    #[test]
    fn test_storage_class_basic() {
        let sc = StorageClass::new("SCFAST")
            .with_guaranteed_space(true)
            .with_accessibility(Accessibility::Continuous)
            .with_io_priority(200);

        assert_eq!(sc.name, "SCFAST");
        assert!(sc.guaranteed_space);
        assert_eq!(sc.accessibility, Accessibility::Continuous);
        assert_eq!(sc.io_priority, Some(200));
    }

    #[test]
    fn test_storage_class_defaults() {
        let sc = StorageClass::new("SCDEFAULT");
        assert!(!sc.guaranteed_space);
        assert_eq!(sc.accessibility, Accessibility::Standard);
        assert_eq!(sc.io_priority, None);
    }

    #[test]
    fn test_storage_class_in_scds() {
        let mut scds = SmsConfiguration::new();
        scds.add_storage_class(StorageClass::new("SCFAST").with_guaranteed_space(true));

        let sc = scds.get_storage_class("SCFAST").unwrap();
        assert!(sc.guaranteed_space);
    }

    // ─── DFSMS-100.3: Management Class Definition ───

    #[test]
    fn test_management_class_basic() {
        let mc = ManagementClass::new("MCPROD")
            .with_backup_frequency(BackupFrequency::Daily)
            .with_versions_backup(3)
            .with_migration_age(30)
            .with_retention(365);

        assert_eq!(mc.name, "MCPROD");
        assert_eq!(mc.backup_frequency, BackupFrequency::Daily);
        assert_eq!(mc.versions_backup, 3);
        assert_eq!(mc.migration_age, Some(30));
        assert_eq!(mc.retention, Some(365));
    }

    #[test]
    fn test_management_class_defaults() {
        let mc = ManagementClass::new("MCDEFAULT");
        assert_eq!(mc.backup_frequency, BackupFrequency::None);
        assert_eq!(mc.versions_backup, 1);
        assert_eq!(mc.migration_age, None);
    }

    // ─── DFSMS-100.4: Storage Group Definition ───

    #[test]
    fn test_storage_group_pool() {
        let sg = StorageGroup::new("SGPROD", StorageGroupType::Pool)
            .with_volume("VOL001", VolumeStatus::Enabled)
            .with_volume("VOL002", VolumeStatus::Enabled)
            .with_volume("VOL003", VolumeStatus::Disabled)
            .with_thresholds(85, 70);

        assert_eq!(sg.name, "SGPROD");
        assert_eq!(sg.group_type, StorageGroupType::Pool);
        assert_eq!(sg.volumes.len(), 3);
        assert_eq!(sg.enabled_volume_count(), 2);
        assert_eq!(sg.high_threshold, Some(85));
        assert_eq!(sg.low_threshold, Some(70));
    }

    #[test]
    fn test_storage_group_vio() {
        let sg = StorageGroup::new("SGVIO", StorageGroupType::Vio);
        assert_eq!(sg.group_type, StorageGroupType::Vio);
        assert!(sg.volumes.is_empty());
    }

    #[test]
    fn test_storage_group_dummy() {
        let sg = StorageGroup::new("SGDUMMY", StorageGroupType::Dummy);
        assert_eq!(sg.group_type, StorageGroupType::Dummy);
    }

    #[test]
    fn test_storage_group_tape() {
        let sg = StorageGroup::new("SGTAPE", StorageGroupType::Tape);
        assert_eq!(sg.group_type, StorageGroupType::Tape);
    }

    // ─── DFSMS-100.5: SCDS/ACDS Configuration Model ───

    #[test]
    fn test_scds_activation() {
        let mut scds = SmsConfiguration::new();
        scds.add_data_class(DataClass::new("DCSTD80").with_recfm(RecordFormat::Fixed).with_lrecl(80));
        scds.add_storage_class(StorageClass::new("SCFAST"));
        scds.add_management_class(ManagementClass::new("MCPROD"));
        scds.add_storage_group(
            StorageGroup::new("SGPROD", StorageGroupType::Pool)
                .with_volume("VOL001", VolumeStatus::Enabled),
        );

        let acds = scds.activate().unwrap();
        assert!(acds.get_data_class("DCSTD80").is_some());
        assert!(acds.get_storage_class("SCFAST").is_some());
        assert!(acds.get_management_class("MCPROD").is_some());
        assert!(acds.get_storage_group("SGPROD").is_some());
    }

    #[test]
    fn test_scds_not_activated_uses_old_acds() {
        let mut scds = SmsConfiguration::new();
        scds.add_data_class(DataClass::new("DC1").with_lrecl(80));

        let acds1 = scds.activate().unwrap();

        // Modify SCDS without re-activating
        scds.add_data_class(DataClass::new("DC2").with_lrecl(133));

        // Old ACDS doesn't have DC2
        assert!(acds1.get_data_class("DC1").is_some());
        assert!(acds1.get_data_class("DC2").is_none());

        // New activation has DC2
        let acds2 = scds.activate().unwrap();
        assert!(acds2.get_data_class("DC2").is_some());
    }

    // ─── DFSMS-100.6: Construct Validation and Cross-References ───

    #[test]
    fn test_validate_mgmtclass_bad_storgrp_ref() {
        let mut scds = SmsConfiguration::new();
        let mut mc = ManagementClass::new("MCBAD");
        mc.storage_group = Some("NONEXISTENT".to_string());
        scds.add_management_class(mc);

        let issues = scds.validate();
        assert!(!issues.is_empty());
        assert!(issues.iter().any(|i| {
            i.severity == ValidationSeverity::Error
                && i.construct_name == "MCBAD"
                && i.message.contains("NONEXISTENT")
        }));
    }

    #[test]
    fn test_validate_fb_no_lrecl_warning() {
        let mut scds = SmsConfiguration::new();
        let dc = DataClass::new("DCNOLRECL").with_recfm(RecordFormat::Fixed);
        scds.add_data_class(dc);

        let issues = scds.validate();
        assert!(issues.iter().any(|i| {
            i.severity == ValidationSeverity::Warning
                && i.construct_name == "DCNOLRECL"
                && i.message.contains("LRECL")
        }));
    }

    #[test]
    fn test_validate_blksize_not_multiple_of_lrecl() {
        let mut scds = SmsConfiguration::new();
        let dc = DataClass::new("DCBADBLK")
            .with_recfm(RecordFormat::Fixed)
            .with_lrecl(80)
            .with_blksize(1000); // Not a multiple of 80
        scds.add_data_class(dc);

        let issues = scds.validate();
        assert!(issues.iter().any(|i| {
            i.severity == ValidationSeverity::Warning
                && i.message.contains("not a multiple")
        }));
    }

    #[test]
    fn test_validate_pool_no_volumes_warning() {
        let mut scds = SmsConfiguration::new();
        scds.add_storage_group(StorageGroup::new("SGEMPTY", StorageGroupType::Pool));

        let issues = scds.validate();
        assert!(issues.iter().any(|i| {
            i.severity == ValidationSeverity::Warning
                && i.construct_name == "SGEMPTY"
                && i.message.contains("no volumes")
        }));
    }

    #[test]
    fn test_validate_threshold_order() {
        let mut scds = SmsConfiguration::new();
        scds.add_storage_group(
            StorageGroup::new("SGBADTHRESH", StorageGroupType::Pool)
                .with_volume("VOL001", VolumeStatus::Enabled)
                .with_thresholds(50, 80), // low > high
        );

        let issues = scds.validate();
        assert!(issues.iter().any(|i| {
            i.severity == ValidationSeverity::Error && i.message.contains("threshold")
        }));
    }

    #[test]
    fn test_validate_clean_config() {
        let mut scds = SmsConfiguration::new();
        scds.add_data_class(DataClass::new("DC1").with_recfm(RecordFormat::Fixed).with_lrecl(80).with_blksize(27920));
        scds.add_storage_class(StorageClass::new("SC1"));
        scds.add_management_class(ManagementClass::new("MC1"));
        scds.add_storage_group(
            StorageGroup::new("SG1", StorageGroupType::Pool)
                .with_volume("VOL001", VolumeStatus::Enabled),
        );

        let issues = scds.validate();
        assert!(issues.is_empty(), "Expected no issues, got: {issues:?}");
        assert!(!scds.has_errors());
    }

    #[test]
    fn test_activation_fails_with_errors() {
        let mut scds = SmsConfiguration::new();
        let mut mc = ManagementClass::new("MCBAD");
        mc.storage_group = Some("NONEXISTENT".to_string());
        scds.add_management_class(mc);

        let result = scds.activate();
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(!errors.is_empty());
    }

    // ─── DFSMS-100.7: SMS Construct Integration Tests ───

    #[test]
    fn test_full_configuration() {
        let mut scds = SmsConfiguration::new();

        // 5 data classes
        for i in 1..=5 {
            let dc = DataClass::new(&format!("DC{i:03}"))
                .with_recfm(RecordFormat::Fixed)
                .with_lrecl(80)
                .with_blksize(27920)
                .with_space(SpaceSpec::tracks(10, 5));
            scds.add_data_class(dc);
        }

        // 3 storage classes
        scds.add_storage_class(StorageClass::new("SCFAST").with_guaranteed_space(true).with_accessibility(Accessibility::Continuous));
        scds.add_storage_class(StorageClass::new("SCSTD"));
        scds.add_storage_class(StorageClass::new("SCARCH").with_accessibility(Accessibility::Delayed));

        // 2 management classes
        scds.add_management_class(ManagementClass::new("MCPROD").with_backup_frequency(BackupFrequency::Daily).with_versions_backup(3).with_migration_age(30));
        scds.add_management_class(ManagementClass::new("MCTEST").with_backup_frequency(BackupFrequency::Weekly).with_migration_age(7));

        // 2 storage groups
        scds.add_storage_group(
            StorageGroup::new("SGPROD", StorageGroupType::Pool)
                .with_volume("VOL001", VolumeStatus::Enabled)
                .with_volume("VOL002", VolumeStatus::Enabled)
                .with_thresholds(85, 70),
        );
        scds.add_storage_group(StorageGroup::new("SGVIO", StorageGroupType::Vio));

        // Validate
        let issues = scds.validate();
        assert!(issues.is_empty(), "Expected no issues, got: {issues:?}");

        // Activate
        let acds = scds.activate().unwrap();

        // Verify all constructs are in ACDS
        assert_eq!(acds.config().data_classes.len(), 5);
        assert_eq!(acds.config().storage_classes.len(), 3);
        assert_eq!(acds.config().management_classes.len(), 2);
        assert_eq!(acds.config().storage_groups.len(), 2);

        // Spot-check lookups
        assert_eq!(acds.get_data_class("DC001").unwrap().lrecl, Some(80));
        assert!(acds.get_storage_class("SCFAST").unwrap().guaranteed_space);
        assert_eq!(acds.get_management_class("MCPROD").unwrap().versions_backup, 3);
        assert_eq!(acds.get_storage_group("SGPROD").unwrap().enabled_volume_count(), 2);
    }

    #[test]
    fn test_case_insensitive_lookup() {
        let mut scds = SmsConfiguration::new();
        scds.add_data_class(DataClass::new("DC001"));
        scds.add_storage_class(StorageClass::new("SC001"));
        scds.add_management_class(ManagementClass::new("MC001"));
        scds.add_storage_group(StorageGroup::new("SG001", StorageGroupType::Pool));

        assert!(scds.get_data_class("dc001").is_some());
        assert!(scds.get_storage_class("sc001").is_some());
        assert!(scds.get_management_class("mc001").is_some());
        assert!(scds.get_storage_group("sg001").is_some());
    }
}
