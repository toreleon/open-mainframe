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

    /// Get dataset path for a given tier.
    fn dataset_path(&self, dsn: &str, tier: StorageTier) -> PathBuf {
        let mut path = self.config.tier_path(tier).to_path_buf();
        for component in dsn.split('.') {
            path.push(component);
        }
        path
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
}
