//! # DFSMShsm — Hierarchical Storage Management
//!
//! Implements z/OS HSM storage tier management:
//!
//! - **ML0** — Primary (active) DASD storage
//! - **ML1** — Secondary (migration level 1) DASD storage
//! - **ML2** — Archive (migration level 2) storage
//! - **HMIGRATE** — Manual migration to lower tiers
//! - **HRECALL** — Recall migrated datasets to ML0
//! - **Automatic Space Management** — Age-based migration policies
//! - **Compression** — RLE compression for migrated data
//! - **CDS** — Control Data Sets (MCDS, BCDS, OCDS)
//! - **HBACKUP/HRECOVER** — Dataset-level backup and recovery
//! - **ABARS** — Aggregate Backup and Recovery Support
//! - **Auto Backup** — Management-class-driven backup frequency

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime};

use crate::error::DatasetError;

// ─────────────────────── Storage Tiers ───────────────────────

/// HSM storage tier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StorageTier {
    /// ML0: Primary active DASD.
    Ml0,
    /// ML1: Secondary migration level 1.
    Ml1,
    /// ML2: Archive migration level 2.
    Ml2,
}

impl StorageTier {
    /// Get the next lower tier, if any.
    pub fn next_lower(self) -> Option<Self> {
        match self {
            StorageTier::Ml0 => Some(StorageTier::Ml1),
            StorageTier::Ml1 => Some(StorageTier::Ml2),
            StorageTier::Ml2 => None,
        }
    }

    /// Display name.
    pub fn name(self) -> &'static str {
        match self {
            StorageTier::Ml0 => "ML0",
            StorageTier::Ml1 => "ML1",
            StorageTier::Ml2 => "ML2",
        }
    }
}

/// Migration status for a dataset.
#[derive(Debug, Clone)]
pub struct MigrationStatus {
    /// Dataset name.
    pub dsn: String,
    /// Current storage tier.
    pub tier: StorageTier,
    /// Original size in bytes.
    pub original_size: u64,
    /// Compressed size in bytes (same as original if on ML0).
    pub compressed_size: u64,
    /// Last reference time.
    pub last_referenced: SystemTime,
    /// Source volume (where it was on ML0).
    pub source_volume: String,
}

// ─────────────────────── Compression ───────────────────────

/// Simple run-length encoding for migration compression.
///
/// Format: for each run of N identical bytes B:
/// - If N <= 3: emit B repeated N times (literal)
/// - If N > 3: emit [0xFF, B, N_high, N_low] (escape + byte + 16-bit count)
///
/// The escape byte 0xFF is itself encoded as [0xFF, 0xFF, 0x00, 0x01].
pub fn rle_compress(data: &[u8]) -> Vec<u8> {
    let mut output = Vec::new();
    let mut i = 0;

    while i < data.len() {
        let byte = data[i];
        let mut run_len = 1usize;

        while i + run_len < data.len() && data[i + run_len] == byte && run_len < 65535 {
            run_len += 1;
        }

        if run_len > 3 || byte == 0xFF {
            output.push(0xFF);
            output.push(byte);
            output.push((run_len >> 8) as u8);
            output.push((run_len & 0xFF) as u8);
        } else {
            for _ in 0..run_len {
                output.push(byte);
            }
        }

        i += run_len;
    }

    output
}

/// Decompress RLE-compressed data.
pub fn rle_decompress(data: &[u8]) -> Vec<u8> {
    let mut output = Vec::new();
    let mut i = 0;

    while i < data.len() {
        if data[i] == 0xFF && i + 3 < data.len() {
            let byte = data[i + 1];
            let count = ((data[i + 2] as usize) << 8) | (data[i + 3] as usize);
            for _ in 0..count {
                output.push(byte);
            }
            i += 4;
        } else {
            output.push(data[i]);
            i += 1;
        }
    }

    output
}

// ─────────────────────── Control Data Sets ───────────────────────

/// MCDS record — tracks migration state for a dataset.
#[derive(Debug, Clone)]
pub struct MigrationRecord {
    /// Dataset name.
    pub dsn: String,
    /// Current tier.
    pub tier: StorageTier,
    /// Original size in bytes.
    pub original_size: u64,
    /// Compressed size in bytes.
    pub compressed_size: u64,
    /// Source volume serial.
    pub source_volume: String,
    /// Migration timestamp.
    pub migrated_at: SystemTime,
}

/// BCDS record — tracks backup state for a dataset.
#[derive(Debug, Clone)]
pub struct BackupRecord {
    /// Dataset name.
    pub dsn: String,
    /// Backup version number.
    pub version: u32,
    /// Backup timestamp.
    pub backed_up_at: SystemTime,
    /// Backup size in bytes.
    pub backup_size: u64,
    /// Target volume.
    pub target_volume: String,
}

/// HSM Control Data Sets (CDS).
#[derive(Debug, Clone, Default)]
pub struct HsmControlDataSets {
    /// MCDS — Migration Control Data Set.
    pub mcds: HashMap<String, MigrationRecord>,
    /// BCDS — Backup Control Data Set.
    pub bcds: HashMap<String, Vec<BackupRecord>>,
    /// OCDS — Offline Control Data Set (volume-level tracking).
    pub ocds: HashMap<String, Vec<String>>,
}

impl HsmControlDataSets {
    /// Record a migration.
    pub fn record_migration(&mut self, record: MigrationRecord) {
        let dsn = record.dsn.clone();
        let tier = record.tier;
        // Track in OCDS by tier
        self.ocds
            .entry(tier.name().to_string())
            .or_default()
            .push(dsn.clone());
        self.mcds.insert(dsn, record);
    }

    /// Remove a migration record (on recall).
    pub fn remove_migration(&mut self, dsn: &str) -> Option<MigrationRecord> {
        self.mcds.remove(dsn)
    }

    /// Record a backup.
    pub fn record_backup(&mut self, record: BackupRecord) {
        self.bcds
            .entry(record.dsn.clone())
            .or_default()
            .push(record);
    }

    /// Get migration record for a dataset.
    pub fn get_migration(&self, dsn: &str) -> Option<&MigrationRecord> {
        self.mcds.get(dsn)
    }

    /// Get all backup records for a dataset.
    pub fn get_backups(&self, dsn: &str) -> Option<&Vec<BackupRecord>> {
        self.bcds.get(dsn)
    }

    /// Count of migrated datasets.
    pub fn migration_count(&self) -> usize {
        self.mcds.len()
    }

    /// Count of backup records.
    pub fn backup_count(&self) -> usize {
        self.bcds.values().map(|v| v.len()).sum()
    }
}

// ─────────────────────── HSM Configuration ───────────────────────

/// HSM configuration.
#[derive(Debug, Clone)]
pub struct HsmConfig {
    /// ML0 (primary) base directory.
    pub ml0_path: PathBuf,
    /// ML1 (secondary) base directory.
    pub ml1_path: PathBuf,
    /// ML2 (archive) base directory.
    pub ml2_path: PathBuf,
    /// ML0 utilization threshold (percent) to trigger auto-migration.
    pub ml0_threshold: u8,
    /// Default days-since-last-reference for ML0→ML1 migration.
    pub primary_age_days: u32,
    /// Default days-since-last-reference for ML1→ML2 migration.
    pub secondary_age_days: u32,
}

impl HsmConfig {
    /// Create a new HSM configuration.
    pub fn new(base: impl AsRef<Path>) -> Self {
        let base = base.as_ref().to_path_buf();
        Self {
            ml0_path: base.join("ml0"),
            ml1_path: base.join("ml1"),
            ml2_path: base.join("ml2"),
            ml0_threshold: 85,
            primary_age_days: 30,
            secondary_age_days: 90,
        }
    }

    /// Get the path for a tier.
    pub fn tier_path(&self, tier: StorageTier) -> &Path {
        match tier {
            StorageTier::Ml0 => &self.ml0_path,
            StorageTier::Ml1 => &self.ml1_path,
            StorageTier::Ml2 => &self.ml2_path,
        }
    }
}

// ─────────────────────── HSM Engine ───────────────────────

/// DFSMShsm engine.
#[derive(Debug)]
pub struct Hsm {
    /// Configuration.
    pub config: HsmConfig,
    /// Control Data Sets.
    pub cds: HsmControlDataSets,
    /// Dataset status tracking.
    statuses: HashMap<String, MigrationStatus>,
}

impl Hsm {
    /// Create a new HSM instance.
    pub fn new(config: HsmConfig) -> Result<Self, DatasetError> {
        std::fs::create_dir_all(&config.ml0_path)?;
        std::fs::create_dir_all(&config.ml1_path)?;
        std::fs::create_dir_all(&config.ml2_path)?;

        Ok(Self {
            config,
            cds: HsmControlDataSets::default(),
            statuses: HashMap::new(),
        })
    }

    /// Get migration status for a dataset.
    pub fn status(&self, dsn: &str) -> Option<&MigrationStatus> {
        self.statuses.get(dsn)
    }

    /// Register a dataset as active on ML0.
    pub fn register_dataset(
        &mut self,
        dsn: &str,
        volume: &str,
    ) -> Result<(), DatasetError> {
        let path = self.dataset_path(dsn, StorageTier::Ml0);
        let size = if path.exists() {
            std::fs::metadata(&path)?.len()
        } else {
            0
        };

        self.statuses.insert(
            dsn.to_string(),
            MigrationStatus {
                dsn: dsn.to_string(),
                tier: StorageTier::Ml0,
                original_size: size,
                compressed_size: size,
                last_referenced: SystemTime::now(),
                source_volume: volume.to_string(),
            },
        );
        Ok(())
    }

    /// HMIGRATE — migrate a dataset to a lower tier.
    ///
    /// If `target` is None, migrates to the next lower tier.
    pub fn hmigrate(
        &mut self,
        dsn: &str,
        target: Option<StorageTier>,
    ) -> Result<String, DatasetError> {
        let status = self.statuses.get(dsn).ok_or_else(|| DatasetError::IoError {
            message: format!("Dataset '{}' not found in HSM", dsn),
        })?;

        let current_tier = status.tier;
        let target_tier = target.unwrap_or_else(|| {
            current_tier.next_lower().unwrap_or(current_tier)
        });

        if target_tier == current_tier {
            return Ok(format!("ARC0001I {} ALREADY ON {}", dsn, current_tier.name()));
        }

        // Read source data
        let source_path = self.dataset_path(dsn, current_tier);
        let data = if source_path.exists() {
            std::fs::read(&source_path)?
        } else {
            return Err(DatasetError::IoError {
                message: format!("Dataset file not found: {}", source_path.display()),
            });
        };

        let original_size = data.len() as u64;

        // Compress for migration
        let compressed = rle_compress(&data);
        let compressed_size = compressed.len() as u64;

        // Write to target tier
        let target_path = self.dataset_path(dsn, target_tier);
        if let Some(parent) = target_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&target_path, &compressed)?;

        // Remove from source tier
        let _ = std::fs::remove_file(&source_path);

        let source_volume = status.source_volume.clone();

        // Update status
        self.statuses.insert(
            dsn.to_string(),
            MigrationStatus {
                dsn: dsn.to_string(),
                tier: target_tier,
                original_size,
                compressed_size,
                last_referenced: SystemTime::now(),
                source_volume: source_volume.clone(),
            },
        );

        // Record in MCDS
        self.cds.record_migration(MigrationRecord {
            dsn: dsn.to_string(),
            tier: target_tier,
            original_size,
            compressed_size,
            source_volume,
            migrated_at: SystemTime::now(),
        });

        Ok(format!(
            "ARC0002I {} MIGRATED FROM {} TO {} ({}B → {}B)",
            dsn,
            current_tier.name(),
            target_tier.name(),
            original_size,
            compressed_size
        ))
    }

    /// HRECALL — recall a migrated dataset to ML0.
    pub fn hrecall(&mut self, dsn: &str) -> Result<String, DatasetError> {
        let status = self.statuses.get(dsn).ok_or_else(|| DatasetError::IoError {
            message: format!("Dataset '{}' not found in HSM", dsn),
        })?;

        let current_tier = status.tier;

        if current_tier == StorageTier::Ml0 {
            return Ok(format!("ARC0001I {} ALREADY ON ML0", dsn));
        }

        // Read compressed data from current tier
        let source_path = self.dataset_path(dsn, current_tier);
        let compressed = std::fs::read(&source_path)?;

        // Decompress
        let data = rle_decompress(&compressed);
        let original_size = data.len() as u64;

        // Write to ML0
        let target_path = self.dataset_path(dsn, StorageTier::Ml0);
        if let Some(parent) = target_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&target_path, &data)?;

        // Remove from migration tier
        let _ = std::fs::remove_file(&source_path);

        let source_volume = status.source_volume.clone();

        // Update status
        self.statuses.insert(
            dsn.to_string(),
            MigrationStatus {
                dsn: dsn.to_string(),
                tier: StorageTier::Ml0,
                original_size,
                compressed_size: original_size,
                last_referenced: SystemTime::now(),
                source_volume,
            },
        );

        // Remove MCDS record
        self.cds.remove_migration(dsn);

        Ok(format!(
            "ARC0003I {} RECALLED FROM {} TO ML0 ({}B)",
            dsn,
            current_tier.name(),
            original_size
        ))
    }

    /// Automatic space management — migrate eligible datasets based on age.
    ///
    /// Returns list of migration messages.
    pub fn auto_migrate(
        &mut self,
        now: SystemTime,
    ) -> Result<Vec<String>, DatasetError> {
        let primary_age = Duration::from_secs(self.config.primary_age_days as u64 * 86400);
        let secondary_age = Duration::from_secs(self.config.secondary_age_days as u64 * 86400);

        // Collect eligible datasets
        let mut eligible: Vec<(String, StorageTier)> = Vec::new();

        for status in self.statuses.values() {
            let age = now
                .duration_since(status.last_referenced)
                .unwrap_or(Duration::ZERO);

            match status.tier {
                StorageTier::Ml0 if age >= primary_age => {
                    eligible.push((status.dsn.clone(), StorageTier::Ml1));
                }
                StorageTier::Ml1 if age >= secondary_age => {
                    eligible.push((status.dsn.clone(), StorageTier::Ml2));
                }
                _ => {}
            }
        }

        let mut messages = Vec::new();
        for (dsn, target) in eligible {
            let msg = self.hmigrate(&dsn, Some(target))?;
            messages.push(msg);
        }

        Ok(messages)
    }

    // ─────────────────────── Backup & Recovery ───────────────────────

    /// HBACKUP — create a backup copy of a dataset.
    ///
    /// If `max_versions` > 0, only the most recent N backups are retained.
    pub fn hbackup(
        &mut self,
        dsn: &str,
        max_versions: u32,
    ) -> Result<String, DatasetError> {
        // Find the dataset file (on ML0 or migrated)
        let status = self.statuses.get(dsn).ok_or_else(|| DatasetError::IoError {
            message: format!("Dataset '{}' not found in HSM", dsn),
        })?;

        let data_path = self.dataset_path(dsn, status.tier);
        let data = std::fs::read(&data_path)?;

        // Create backup directory
        let backup_dir = self.config.ml1_path.join("BACKUPS");
        std::fs::create_dir_all(&backup_dir)?;

        // Determine version number
        let existing = self.cds.bcds.entry(dsn.to_string()).or_default();
        let version = existing.last().map(|r| r.version + 1).unwrap_or(1);

        // Write backup file (compressed)
        let compressed = rle_compress(&data);
        let backup_filename = format!("{}.V{:04}", dsn.replace('.', "_"), version);
        let backup_path = backup_dir.join(&backup_filename);
        std::fs::write(&backup_path, &compressed)?;

        let backup_size = compressed.len() as u64;

        // Record in BCDS
        self.cds.record_backup(BackupRecord {
            dsn: dsn.to_string(),
            version,
            backed_up_at: SystemTime::now(),
            backup_size,
            target_volume: "BKVOL1".to_string(),
        });

        // Enforce max_versions
        if max_versions > 0 {
            let records = self.cds.bcds.get_mut(dsn).unwrap();
            while records.len() > max_versions as usize {
                let old = records.remove(0);
                let old_filename = format!("{}.V{:04}", dsn.replace('.', "_"), old.version);
                let old_path = backup_dir.join(&old_filename);
                let _ = std::fs::remove_file(&old_path);
            }
        }

        let versions_kept = self.cds.bcds.get(dsn).map(|v| v.len()).unwrap_or(0);
        Ok(format!(
            "ARC0010I {} BACKED UP (VERSION {} OF {}, {} BYTES)",
            dsn, version, versions_kept, backup_size
        ))
    }

    /// HRECOVER — recover a dataset from backup.
    ///
    /// If `from_version` is None, recovers the most recent backup.
    /// If `replace` is true, overwrites existing dataset.
    pub fn hrecover(
        &mut self,
        dsn: &str,
        from_version: Option<u32>,
        replace: bool,
    ) -> Result<String, DatasetError> {
        let records = self.cds.bcds.get(dsn).ok_or_else(|| DatasetError::IoError {
            message: format!("No backup found for '{}'", dsn),
        })?;

        // Find the appropriate backup version
        let record = if let Some(ver) = from_version {
            records
                .iter()
                .find(|r| r.version == ver)
                .ok_or_else(|| DatasetError::IoError {
                    message: format!("Backup version {} not found for '{}'", ver, dsn),
                })?
        } else {
            records.last().ok_or_else(|| DatasetError::IoError {
                message: format!("No backup versions available for '{}'", dsn),
            })?
        };

        let version = record.version;

        // Read backup file
        let backup_dir = self.config.ml1_path.join("BACKUPS");
        let backup_filename = format!("{}.V{:04}", dsn.replace('.', "_"), version);
        let backup_path = backup_dir.join(&backup_filename);
        let compressed = std::fs::read(&backup_path)?;
        let data = rle_decompress(&compressed);

        // Check if target exists
        let target_path = self.dataset_path(dsn, StorageTier::Ml0);
        if target_path.exists() && !replace {
            return Err(DatasetError::IoError {
                message: format!(
                    "Dataset '{}' exists. Use REPLACE to overwrite.",
                    dsn
                ),
            });
        }

        // Write restored data
        if let Some(parent) = target_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&target_path, &data)?;

        let original_size = data.len() as u64;

        // Update status to ML0
        self.statuses.insert(
            dsn.to_string(),
            MigrationStatus {
                dsn: dsn.to_string(),
                tier: StorageTier::Ml0,
                original_size,
                compressed_size: original_size,
                last_referenced: SystemTime::now(),
                source_volume: "VOL001".to_string(),
            },
        );

        Ok(format!(
            "ARC0011I {} RECOVERED FROM VERSION {} ({} BYTES)",
            dsn, version, original_size
        ))
    }

    /// Automatic backup — back up datasets based on backup frequency.
    ///
    /// `backed_up_today` is a set of DSNs already backed up today (to avoid duplicates).
    /// Returns list of backup messages.
    pub fn auto_backup(
        &mut self,
        eligible_dsns: &[String],
        max_versions: u32,
        backed_up_today: &[String],
    ) -> Result<Vec<String>, DatasetError> {
        let mut messages = Vec::new();

        for dsn in eligible_dsns {
            if backed_up_today.contains(dsn) {
                continue; // Skip — already backed up today
            }
            if self.statuses.contains_key(dsn) {
                let msg = self.hbackup(dsn, max_versions)?;
                messages.push(msg);
            }
        }

        Ok(messages)
    }

    /// Get dataset path for a given tier.
    fn dataset_path(&self, dsn: &str, tier: StorageTier) -> PathBuf {
        let mut path = self.config.tier_path(tier).to_path_buf();
        for component in dsn.split('.') {
            path.push(component);
        }
        path
    }
}

// ─────────────────────── ABARS ───────────────────────

/// Aggregate group definition for ABARS.
#[derive(Debug, Clone)]
pub struct AggregateGroup {
    /// Aggregate group name.
    pub name: String,
    /// Dataset filter patterns (e.g., "PAYROLL.**").
    pub filters: Vec<String>,
}

impl AggregateGroup {
    /// Create a new aggregate group.
    pub fn new(name: &str, filters: Vec<String>) -> Self {
        Self {
            name: name.to_uppercase(),
            filters,
        }
    }

    /// Check if a DSN matches any filter in the aggregate.
    pub fn matches(&self, dsn: &str) -> bool {
        let dsn_upper = dsn.to_uppercase();
        self.filters.iter().any(|f| {
            let filter = f.to_uppercase();
            if filter.ends_with(".**") {
                let prefix = &filter[..filter.len() - 3];
                dsn_upper.starts_with(prefix) && dsn_upper.len() > prefix.len()
            } else if filter.ends_with(".*") {
                let prefix = &filter[..filter.len() - 2];
                dsn_upper.starts_with(prefix)
                    && dsn_upper[prefix.len()..].starts_with('.')
                    && !dsn_upper[prefix.len() + 1..].contains('.')
            } else {
                dsn_upper == filter
            }
        })
    }

    /// List all matching datasets from a set.
    pub fn matching_datasets<'a>(&self, all_dsns: &'a [String]) -> Vec<&'a String> {
        all_dsns.iter().filter(|d| self.matches(d)).collect()
    }
}

/// ABARS engine for aggregate backup/recovery.
#[derive(Debug, Default)]
pub struct Abars {
    /// Defined aggregate groups.
    pub groups: HashMap<String, AggregateGroup>,
}

impl Abars {
    /// Define an aggregate group.
    pub fn define_group(&mut self, group: AggregateGroup) {
        self.groups.insert(group.name.clone(), group);
    }

    /// Get an aggregate group.
    pub fn get_group(&self, name: &str) -> Option<&AggregateGroup> {
        self.groups.get(&name.to_uppercase())
    }

    /// ABACKUP — back up all datasets in an aggregate group.
    pub fn abackup(
        &self,
        group_name: &str,
        hsm: &mut Hsm,
        all_dsns: &[String],
    ) -> Result<Vec<String>, DatasetError> {
        let group = self.groups.get(&group_name.to_uppercase()).ok_or_else(|| {
            DatasetError::IoError {
                message: format!("Aggregate group '{}' not found", group_name),
            }
        })?;

        let matching = group.matching_datasets(all_dsns);
        let mut messages = Vec::new();

        for dsn in matching {
            let msg = hsm.hbackup(dsn, 0)?; // No version limit for aggregate
            messages.push(msg);
        }

        messages.push(format!(
            "ARC0020I AGGREGATE {} BACKED UP ({} DATASETS)",
            group_name,
            messages.len()
        ));
        Ok(messages)
    }

    /// ARECOVER — recover all datasets in an aggregate group.
    pub fn arecover(
        &self,
        group_name: &str,
        hsm: &mut Hsm,
        all_dsns: &[String],
    ) -> Result<Vec<String>, DatasetError> {
        let group = self.groups.get(&group_name.to_uppercase()).ok_or_else(|| {
            DatasetError::IoError {
                message: format!("Aggregate group '{}' not found", group_name),
            }
        })?;

        let matching = group.matching_datasets(all_dsns);
        let mut messages = Vec::new();

        for dsn in matching {
            let msg = hsm.hrecover(dsn, None, true)?;
            messages.push(msg);
        }

        messages.push(format!(
            "ARC0021I AGGREGATE {} RECOVERED ({} DATASETS)",
            group_name,
            messages.len()
        ));
        Ok(messages)
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn test_dir() -> PathBuf {
        let count = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        std::env::temp_dir().join(format!("hsm_test_{}", count))
    }

    fn cleanup(path: &Path) {
        let _ = std::fs::remove_dir_all(path);
    }

    fn create_hsm(dir: &Path) -> Hsm {
        let config = HsmConfig::new(dir);
        Hsm::new(config).unwrap()
    }

    fn create_test_dataset(hsm: &mut Hsm, dsn: &str, data: &[u8]) {
        let path = hsm.dataset_path(dsn, StorageTier::Ml0);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }
        std::fs::write(&path, data).unwrap();
        hsm.register_dataset(dsn, "VOL001").unwrap();
    }

    // ─── DFSMS-104.1: Storage Tier Model ───

    #[test]
    fn test_storage_tiers_available() {
        let dir = test_dir();
        cleanup(&dir);

        let config = HsmConfig::new(&dir);
        let _hsm = Hsm::new(config.clone()).unwrap();

        assert!(config.ml0_path.exists());
        assert!(config.ml1_path.exists());
        assert!(config.ml2_path.exists());

        cleanup(&dir);
    }

    #[test]
    fn test_dataset_status_active_ml0() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        create_test_dataset(&mut hsm, "MY.DATA", b"test content");

        let status = hsm.status("MY.DATA").unwrap();
        assert_eq!(status.tier, StorageTier::Ml0);
        assert_eq!(status.dsn, "MY.DATA");

        cleanup(&dir);
    }

    #[test]
    fn test_tier_next_lower() {
        assert_eq!(StorageTier::Ml0.next_lower(), Some(StorageTier::Ml1));
        assert_eq!(StorageTier::Ml1.next_lower(), Some(StorageTier::Ml2));
        assert_eq!(StorageTier::Ml2.next_lower(), None);
    }

    // ─── DFSMS-104.2: HMIGRATE ───

    #[test]
    fn test_hmigrate_ml0_to_ml1() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        create_test_dataset(&mut hsm, "MY.LARGE.DATA", b"AAAAAAAAAA");

        let msg = hsm.hmigrate("MY.LARGE.DATA", None).unwrap();
        assert!(msg.contains("MIGRATED FROM ML0 TO ML1"));

        let status = hsm.status("MY.LARGE.DATA").unwrap();
        assert_eq!(status.tier, StorageTier::Ml1);

        // Source file should be removed
        assert!(!hsm.dataset_path("MY.LARGE.DATA", StorageTier::Ml0).exists());
        // Target file should exist
        assert!(hsm.dataset_path("MY.LARGE.DATA", StorageTier::Ml1).exists());

        cleanup(&dir);
    }

    #[test]
    fn test_hmigrate_direct_to_ml2() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        create_test_dataset(&mut hsm, "ARCHIVE.DATA", b"archive content");

        let msg = hsm.hmigrate("ARCHIVE.DATA", Some(StorageTier::Ml2)).unwrap();
        assert!(msg.contains("MIGRATED FROM ML0 TO ML2"));

        let status = hsm.status("ARCHIVE.DATA").unwrap();
        assert_eq!(status.tier, StorageTier::Ml2);

        cleanup(&dir);
    }

    #[test]
    fn test_hmigrate_already_migrated_moves_to_next() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        create_test_dataset(&mut hsm, "STEP.DATA", b"step migration data");

        // First: ML0 → ML1
        hsm.hmigrate("STEP.DATA", None).unwrap();
        assert_eq!(hsm.status("STEP.DATA").unwrap().tier, StorageTier::Ml1);

        // Second: ML1 → ML2
        hsm.hmigrate("STEP.DATA", None).unwrap();
        assert_eq!(hsm.status("STEP.DATA").unwrap().tier, StorageTier::Ml2);

        cleanup(&dir);
    }

    // ─── DFSMS-104.3: HRECALL ───

    #[test]
    fn test_hrecall_from_ml1() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        let original_data = b"important data to recall";
        create_test_dataset(&mut hsm, "RECALL.DS", original_data);

        hsm.hmigrate("RECALL.DS", None).unwrap();
        assert_eq!(hsm.status("RECALL.DS").unwrap().tier, StorageTier::Ml1);

        let msg = hsm.hrecall("RECALL.DS").unwrap();
        assert!(msg.contains("RECALLED FROM ML1 TO ML0"));

        let status = hsm.status("RECALL.DS").unwrap();
        assert_eq!(status.tier, StorageTier::Ml0);

        // Verify data integrity
        let recalled = std::fs::read(hsm.dataset_path("RECALL.DS", StorageTier::Ml0)).unwrap();
        assert_eq!(recalled, original_data);

        cleanup(&dir);
    }

    #[test]
    fn test_hrecall_from_ml2_to_ml0() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        let original_data = b"deep archive data";
        create_test_dataset(&mut hsm, "DEEP.DS", original_data);

        // Migrate directly to ML2
        hsm.hmigrate("DEEP.DS", Some(StorageTier::Ml2)).unwrap();
        assert_eq!(hsm.status("DEEP.DS").unwrap().tier, StorageTier::Ml2);

        // Recall should go directly to ML0 (not ML1)
        let msg = hsm.hrecall("DEEP.DS").unwrap();
        assert!(msg.contains("RECALLED FROM ML2 TO ML0"));
        assert_eq!(hsm.status("DEEP.DS").unwrap().tier, StorageTier::Ml0);

        // Verify data
        let recalled = std::fs::read(hsm.dataset_path("DEEP.DS", StorageTier::Ml0)).unwrap();
        assert_eq!(recalled, original_data);

        cleanup(&dir);
    }

    #[test]
    fn test_hrecall_already_on_ml0() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        create_test_dataset(&mut hsm, "ACTIVE.DS", b"already active");

        let msg = hsm.hrecall("ACTIVE.DS").unwrap();
        assert!(msg.contains("ALREADY ON ML0"));

        cleanup(&dir);
    }

    // ─── DFSMS-104.4: Automatic Space Management ───

    #[test]
    fn test_auto_migrate_age_based() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        hsm.config.primary_age_days = 30;
        hsm.config.secondary_age_days = 90;

        // Create datasets with different "ages"
        create_test_dataset(&mut hsm, "OLD.DS", b"old data content here");
        create_test_dataset(&mut hsm, "NEW.DS", b"new data");

        // Simulate OLD.DS being 31 days old
        let old_time = SystemTime::now() - Duration::from_secs(31 * 86400);
        if let Some(status) = hsm.statuses.get_mut("OLD.DS") {
            status.last_referenced = old_time;
        }

        // Run auto-migration at current time
        let messages = hsm.auto_migrate(SystemTime::now()).unwrap();

        // OLD.DS should be migrated, NEW.DS should stay
        assert_eq!(messages.len(), 1);
        assert!(messages[0].contains("OLD.DS"));
        assert_eq!(hsm.status("OLD.DS").unwrap().tier, StorageTier::Ml1);
        assert_eq!(hsm.status("NEW.DS").unwrap().tier, StorageTier::Ml0);

        cleanup(&dir);
    }

    #[test]
    fn test_auto_migrate_ml1_to_ml2() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        hsm.config.primary_age_days = 30;
        hsm.config.secondary_age_days = 90;

        create_test_dataset(&mut hsm, "AGING.DS", b"aging data");

        // Migrate to ML1 first
        hsm.hmigrate("AGING.DS", None).unwrap();
        assert_eq!(hsm.status("AGING.DS").unwrap().tier, StorageTier::Ml1);

        // Make it 91 days old on ML1
        let old_time = SystemTime::now() - Duration::from_secs(91 * 86400);
        if let Some(status) = hsm.statuses.get_mut("AGING.DS") {
            status.last_referenced = old_time;
        }

        let messages = hsm.auto_migrate(SystemTime::now()).unwrap();
        assert_eq!(messages.len(), 1);
        assert_eq!(hsm.status("AGING.DS").unwrap().tier, StorageTier::Ml2);

        cleanup(&dir);
    }

    // ─── DFSMS-104.5: Compression ───

    #[test]
    fn test_rle_compress_decompress_roundtrip() {
        let original = b"AAAAAAAAAA BBBBBB CCCCCCCCC DDD";
        let compressed = rle_compress(original);
        let decompressed = rle_decompress(&compressed);
        assert_eq!(decompressed, original);
    }

    #[test]
    fn test_rle_compression_ratio() {
        // Highly compressible data (repeated chars)
        let data = vec![b'A'; 10000];
        let compressed = rle_compress(&data);
        // RLE should compress 10000 A's to just 4 bytes
        assert!(compressed.len() < data.len() / 2);
        // Verify roundtrip
        assert_eq!(rle_decompress(&compressed), data);
    }

    #[test]
    fn test_migration_compression_reduces_size() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        // Create highly compressible dataset (100 KB of repeated bytes)
        let mut data = Vec::with_capacity(100_000);
        for _ in 0..10 {
            data.extend_from_slice(&vec![b'A'; 5000]);
            data.extend_from_slice(&vec![b'B'; 5000]);
        }
        create_test_dataset(&mut hsm, "BIG.DATA", &data);

        hsm.hmigrate("BIG.DATA", None).unwrap();

        let status = hsm.status("BIG.DATA").unwrap();
        assert!(
            status.compressed_size < status.original_size,
            "Expected compression: {} < {}",
            status.compressed_size,
            status.original_size
        );

        // Recall and verify integrity
        hsm.hrecall("BIG.DATA").unwrap();
        let recalled = std::fs::read(hsm.dataset_path("BIG.DATA", StorageTier::Ml0)).unwrap();
        assert_eq!(recalled, data);

        cleanup(&dir);
    }

    // ─── DFSMS-104.6: Control Data Sets ───

    #[test]
    fn test_mcds_tracks_migration() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        create_test_dataset(&mut hsm, "TRACKED.DS", b"tracked data");

        hsm.hmigrate("TRACKED.DS", None).unwrap();

        let mcds_record = hsm.cds.get_migration("TRACKED.DS").unwrap();
        assert_eq!(mcds_record.tier, StorageTier::Ml1);
        assert_eq!(mcds_record.dsn, "TRACKED.DS");
        assert!(mcds_record.original_size > 0);

        cleanup(&dir);
    }

    #[test]
    fn test_bcds_tracks_backups() {
        let mut cds = HsmControlDataSets::default();
        cds.record_backup(BackupRecord {
            dsn: "MY.DS".to_string(),
            version: 1,
            backed_up_at: SystemTime::now(),
            backup_size: 1024,
            target_volume: "BKVOL1".to_string(),
        });

        assert_eq!(cds.backup_count(), 1);
        let backups = cds.get_backups("MY.DS").unwrap();
        assert_eq!(backups.len(), 1);
        assert_eq!(backups[0].version, 1);
    }

    #[test]
    fn test_cds_state_recovery() {
        let mut cds = HsmControlDataSets::default();

        // Record migrations
        cds.record_migration(MigrationRecord {
            dsn: "DS1".to_string(),
            tier: StorageTier::Ml1,
            original_size: 1000,
            compressed_size: 500,
            source_volume: "VOL001".to_string(),
            migrated_at: SystemTime::now(),
        });
        cds.record_migration(MigrationRecord {
            dsn: "DS2".to_string(),
            tier: StorageTier::Ml2,
            original_size: 2000,
            compressed_size: 800,
            source_volume: "VOL002".to_string(),
            migrated_at: SystemTime::now(),
        });

        // Simulate "restart" by checking state
        assert_eq!(cds.migration_count(), 2);
        assert_eq!(cds.get_migration("DS1").unwrap().tier, StorageTier::Ml1);
        assert_eq!(cds.get_migration("DS2").unwrap().tier, StorageTier::Ml2);
    }

    #[test]
    fn test_mcds_removed_on_recall() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        create_test_dataset(&mut hsm, "RECALL.ME", b"recall test data");

        hsm.hmigrate("RECALL.ME", None).unwrap();
        assert!(hsm.cds.get_migration("RECALL.ME").is_some());

        hsm.hrecall("RECALL.ME").unwrap();
        assert!(hsm.cds.get_migration("RECALL.ME").is_none());

        cleanup(&dir);
    }

    // ─── DFSMS-104.7: Integration Tests ───

    #[test]
    fn test_full_migration_recall_cycle() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);

        // Create 10 datasets with varying content
        let datasets: Vec<(String, Vec<u8>)> = (0..10)
            .map(|i| {
                let dsn = format!("TEST.DS{:02}", i);
                let data = format!("Dataset {} content: {}", i, "X".repeat(100 * (i + 1)))
                    .into_bytes();
                (dsn, data)
            })
            .collect();

        for (dsn, data) in &datasets {
            create_test_dataset(&mut hsm, dsn, data);
        }

        // Migrate even-numbered datasets
        for i in (0..10).step_by(2) {
            let dsn = format!("TEST.DS{:02}", i);
            hsm.hmigrate(&dsn, None).unwrap();
        }

        // Verify migration status
        for i in 0..10 {
            let dsn = format!("TEST.DS{:02}", i);
            let expected = if i % 2 == 0 {
                StorageTier::Ml1
            } else {
                StorageTier::Ml0
            };
            assert_eq!(hsm.status(&dsn).unwrap().tier, expected);
        }

        // Recall all migrated and verify content
        for i in (0..10).step_by(2) {
            let dsn = format!("TEST.DS{:02}", i);
            hsm.hrecall(&dsn).unwrap();
        }

        for (dsn, original_data) in &datasets {
            let path = hsm.dataset_path(dsn, StorageTier::Ml0);
            let recalled = std::fs::read(&path).unwrap();
            assert_eq!(&recalled, original_data, "Data mismatch for {}", dsn);
        }

        cleanup(&dir);
    }

    #[test]
    fn test_auto_migrate_multiple_eligible() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        hsm.config.primary_age_days = 30;

        // Create 10 datasets
        for i in 0..10 {
            let dsn = format!("AUTO.DS{:02}", i);
            create_test_dataset(&mut hsm, &dsn, format!("data-{}", i).as_bytes());
        }

        // Age first 5 datasets beyond threshold
        let old_time = SystemTime::now() - Duration::from_secs(35 * 86400);
        for i in 0..5 {
            let dsn = format!("AUTO.DS{:02}", i);
            if let Some(status) = hsm.statuses.get_mut(&dsn) {
                status.last_referenced = old_time;
            }
        }

        let messages = hsm.auto_migrate(SystemTime::now()).unwrap();
        assert_eq!(messages.len(), 5);

        // First 5 should be on ML1, rest on ML0
        for i in 0..10 {
            let dsn = format!("AUTO.DS{:02}", i);
            let expected = if i < 5 {
                StorageTier::Ml1
            } else {
                StorageTier::Ml0
            };
            assert_eq!(
                hsm.status(&dsn).unwrap().tier,
                expected,
                "Wrong tier for {}",
                dsn
            );
        }

        cleanup(&dir);
    }

    // ─── DFSMS-105.1: HBACKUP ───

    #[test]
    fn test_hbackup_creates_backup() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        create_test_dataset(&mut hsm, "MY.CRITICAL.DATA", b"critical data content");

        let msg = hsm.hbackup("MY.CRITICAL.DATA", 0).unwrap();
        assert!(msg.contains("BACKED UP"));
        assert!(msg.contains("VERSION 1"));

        let backups = hsm.cds.get_backups("MY.CRITICAL.DATA").unwrap();
        assert_eq!(backups.len(), 1);
        assert_eq!(backups[0].version, 1);

        cleanup(&dir);
    }

    #[test]
    fn test_hbackup_version_retention() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        create_test_dataset(&mut hsm, "VERSIONED.DS", b"version data");

        // Take 4 backups with max 3 versions
        for _ in 0..4 {
            hsm.hbackup("VERSIONED.DS", 3).unwrap();
        }

        let backups = hsm.cds.get_backups("VERSIONED.DS").unwrap();
        assert_eq!(backups.len(), 3); // Only 3 kept
        assert_eq!(backups[0].version, 2); // Oldest is version 2
        assert_eq!(backups[2].version, 4); // Newest is version 4

        cleanup(&dir);
    }

    // ─── DFSMS-105.2: HRECOVER ───

    #[test]
    fn test_hrecover_latest() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        let original = b"recover this data";
        create_test_dataset(&mut hsm, "RECOVER.DS", original);

        hsm.hbackup("RECOVER.DS", 0).unwrap();

        // Delete the original
        let path = hsm.dataset_path("RECOVER.DS", StorageTier::Ml0);
        std::fs::remove_file(&path).unwrap();

        // Recover
        let msg = hsm.hrecover("RECOVER.DS", None, true).unwrap();
        assert!(msg.contains("RECOVERED FROM VERSION 1"));

        let restored = std::fs::read(&path).unwrap();
        assert_eq!(restored, original);

        cleanup(&dir);
    }

    #[test]
    fn test_hrecover_specific_version() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);

        // Create and backup v1
        create_test_dataset(&mut hsm, "MULTI.DS", b"version 1 data");
        hsm.hbackup("MULTI.DS", 0).unwrap();

        // Overwrite and backup v2
        let path = hsm.dataset_path("MULTI.DS", StorageTier::Ml0);
        std::fs::write(&path, b"version 2 data").unwrap();
        hsm.hbackup("MULTI.DS", 0).unwrap();

        // Overwrite and backup v3
        std::fs::write(&path, b"version 3 data").unwrap();
        hsm.hbackup("MULTI.DS", 0).unwrap();

        // Recover version 2 specifically
        let msg = hsm.hrecover("MULTI.DS", Some(2), true).unwrap();
        assert!(msg.contains("RECOVERED FROM VERSION 2"));

        let restored = std::fs::read(&path).unwrap();
        assert_eq!(restored, b"version 2 data");

        cleanup(&dir);
    }

    #[test]
    fn test_hrecover_replace_required() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        create_test_dataset(&mut hsm, "EXISTS.DS", b"existing data");
        hsm.hbackup("EXISTS.DS", 0).unwrap();

        // Trying to recover without replace should fail
        let result = hsm.hrecover("EXISTS.DS", None, false);
        assert!(result.is_err());

        // With replace should succeed
        let msg = hsm.hrecover("EXISTS.DS", None, true).unwrap();
        assert!(msg.contains("RECOVERED"));

        cleanup(&dir);
    }

    // ─── DFSMS-105.3: Automatic Backup ───

    #[test]
    fn test_auto_backup_eligible() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        for i in 0..5 {
            create_test_dataset(
                &mut hsm,
                &format!("AUTO.BK{:02}", i),
                format!("data-{}", i).as_bytes(),
            );
        }

        let eligible: Vec<String> = (0..5).map(|i| format!("AUTO.BK{:02}", i)).collect();
        let messages = hsm.auto_backup(&eligible, 3, &[]).unwrap();
        assert_eq!(messages.len(), 5);

        // All should have backups
        for dsn in &eligible {
            assert!(hsm.cds.get_backups(dsn).is_some());
        }

        cleanup(&dir);
    }

    #[test]
    fn test_auto_backup_skips_already_backed_up() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        create_test_dataset(&mut hsm, "DAILY.DS", b"daily backup data");

        let eligible = vec!["DAILY.DS".to_string()];
        let backed_up = vec!["DAILY.DS".to_string()];

        // Should skip since already backed up today
        let messages = hsm.auto_backup(&eligible, 3, &backed_up).unwrap();
        assert_eq!(messages.len(), 0);

        cleanup(&dir);
    }

    // ─── DFSMS-105.4: ABARS Aggregate Definition ───

    #[test]
    fn test_aggregate_group_definition() {
        let mut abars = Abars::default();
        let group = AggregateGroup::new(
            "PAYROLL.AG",
            vec!["PAYROLL.**".to_string()],
        );
        abars.define_group(group);

        let g = abars.get_group("PAYROLL.AG").unwrap();
        assert_eq!(g.name, "PAYROLL.AG");
        assert_eq!(g.filters, vec!["PAYROLL.**"]);
    }

    #[test]
    fn test_aggregate_group_matching() {
        let group = AggregateGroup::new(
            "PAYROLL.AG",
            vec!["PAYROLL.**".to_string()],
        );

        assert!(group.matches("PAYROLL.DATA.FILE"));
        assert!(group.matches("PAYROLL.HISTORY.2026"));
        assert!(!group.matches("HR.DATA.FILE"));
        assert!(!group.matches("PAYROLL")); // Must have qualifiers after prefix
    }

    #[test]
    fn test_aggregate_group_listing() {
        let group = AggregateGroup::new(
            "APP.AG",
            vec!["APP.**".to_string(), "SHARED.CONFIG".to_string()],
        );

        let all = vec![
            "APP.DATA.FILE1".to_string(),
            "APP.LOG.2026".to_string(),
            "SHARED.CONFIG".to_string(),
            "OTHER.DS".to_string(),
        ];

        let matches = group.matching_datasets(&all);
        assert_eq!(matches.len(), 3);

        cleanup(&PathBuf::new()); // no-op, just for consistency
    }

    // ─── DFSMS-105.5: ABACKUP/ARECOVER ───

    #[test]
    fn test_abackup_aggregate() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        let mut abars = Abars::default();

        // Create datasets
        let dsns: Vec<String> = (0..5)
            .map(|i| format!("PAY.DATA.FILE{}", i))
            .collect();

        for dsn in &dsns {
            create_test_dataset(&mut hsm, dsn, format!("data-{}", dsn).as_bytes());
        }

        // Also a non-matching dataset
        create_test_dataset(&mut hsm, "OTHER.DS", b"other");

        // Define aggregate
        abars.define_group(AggregateGroup::new(
            "PAY.AG",
            vec!["PAY.**".to_string()],
        ));

        let all_dsns: Vec<String> = dsns.iter().chain(std::iter::once(&"OTHER.DS".to_string())).cloned().collect();
        let messages = abars.abackup("PAY.AG", &mut hsm, &all_dsns).unwrap();

        // 5 backup messages + 1 summary
        assert_eq!(messages.len(), 6);
        assert!(messages.last().unwrap().contains("5 DATASETS"));

        // Verify backups exist for PAY datasets but not OTHER
        for dsn in &dsns {
            assert!(hsm.cds.get_backups(dsn).is_some());
        }
        assert!(hsm.cds.get_backups("OTHER.DS").is_none());

        cleanup(&dir);
    }

    #[test]
    fn test_arecover_aggregate() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        let mut abars = Abars::default();

        // Create and backup datasets
        let dsns: Vec<String> = (0..5)
            .map(|i| format!("REC.DATA.FILE{}", i))
            .collect();

        let originals: Vec<Vec<u8>> = dsns
            .iter()
            .map(|dsn| format!("original-data-{}", dsn).into_bytes())
            .collect();

        for (dsn, data) in dsns.iter().zip(originals.iter()) {
            create_test_dataset(&mut hsm, dsn, data);
            hsm.hbackup(dsn, 0).unwrap();
        }

        // Delete all originals
        for dsn in &dsns {
            let path = hsm.dataset_path(dsn, StorageTier::Ml0);
            let _ = std::fs::remove_file(&path);
        }

        // Define aggregate and recover
        abars.define_group(AggregateGroup::new(
            "REC.AG",
            vec!["REC.**".to_string()],
        ));

        let messages = abars.arecover("REC.AG", &mut hsm, &dsns).unwrap();
        // 5 recover messages + 1 summary
        assert_eq!(messages.len(), 6);

        // Verify all datasets restored with correct content
        for (dsn, original_data) in dsns.iter().zip(originals.iter()) {
            let path = hsm.dataset_path(dsn, StorageTier::Ml0);
            let restored = std::fs::read(&path).unwrap();
            assert_eq!(&restored, original_data, "Mismatch for {}", dsn);
        }

        cleanup(&dir);
    }

    // ─── DFSMS-105.6: Integration Test ───

    #[test]
    fn test_full_backup_recover_cycle() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);

        // Create dataset and take 3 versioned backups
        create_test_dataset(&mut hsm, "CYCLE.DS", b"version 1");
        hsm.hbackup("CYCLE.DS", 5).unwrap();

        let path = hsm.dataset_path("CYCLE.DS", StorageTier::Ml0);
        std::fs::write(&path, b"version 2").unwrap();
        hsm.hbackup("CYCLE.DS", 5).unwrap();

        std::fs::write(&path, b"version 3").unwrap();
        hsm.hbackup("CYCLE.DS", 5).unwrap();

        // Recover version 2
        let msg = hsm.hrecover("CYCLE.DS", Some(2), true).unwrap();
        assert!(msg.contains("VERSION 2"));
        let restored = std::fs::read(&path).unwrap();
        assert_eq!(restored, b"version 2");

        // Recover latest (version 3)
        let msg = hsm.hrecover("CYCLE.DS", None, true).unwrap();
        assert!(msg.contains("VERSION 3"));
        let restored = std::fs::read(&path).unwrap();
        assert_eq!(restored, b"version 3");

        cleanup(&dir);
    }

    #[test]
    fn test_full_aggregate_backup_recover_5ds() {
        let dir = test_dir();
        cleanup(&dir);

        let mut hsm = create_hsm(&dir);
        let mut abars = Abars::default();

        let dsns: Vec<String> = (0..5)
            .map(|i| format!("FULL.AGG.DS{}", i))
            .collect();

        let originals: Vec<Vec<u8>> = dsns
            .iter()
            .map(|dsn| format!("content-of-{}", dsn).into_bytes())
            .collect();

        for (dsn, data) in dsns.iter().zip(originals.iter()) {
            create_test_dataset(&mut hsm, dsn, data);
        }

        abars.define_group(AggregateGroup::new(
            "FULL.AG",
            vec!["FULL.**".to_string()],
        ));

        // ABACKUP
        let msgs = abars.abackup("FULL.AG", &mut hsm, &dsns).unwrap();
        assert!(msgs.last().unwrap().contains("5 DATASETS"));

        // Delete originals
        for dsn in &dsns {
            let path = hsm.dataset_path(dsn, StorageTier::Ml0);
            let _ = std::fs::remove_file(&path);
        }

        // ARECOVER
        let msgs = abars.arecover("FULL.AG", &mut hsm, &dsns).unwrap();
        assert!(msgs.last().unwrap().contains("5 DATASETS"));

        // Verify all match originals
        for (dsn, original) in dsns.iter().zip(originals.iter()) {
            let path = hsm.dataset_path(dsn, StorageTier::Ml0);
            assert_eq!(&std::fs::read(&path).unwrap(), original);
        }

        cleanup(&dir);
    }
}
