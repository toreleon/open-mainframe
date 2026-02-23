//! # DFSMSrmm — Removable Media Manager (Tape Management)
//!
//! Implements z/OS tape volume lifecycle management:
//!
//! - **Volume Registry** — Volume state machine (Scratch → Private → Retained → Scratch)
//! - **VRS** (Vital Record Specifications) — Retention policies by name pattern
//! - **Scratch/Private Pools** — Tape allocation and recycling
//! - **VRSEL** — Batch retention evaluation and volume recycling

use std::collections::HashMap;
use std::time::{Duration, SystemTime};

// ─────────────────────── Volume State ───────────────────────

/// Volume lifecycle state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VolumeState {
    /// Available for allocation.
    Scratch,
    /// Allocated to a dataset, actively in use.
    Private,
    /// Retention policy active, cannot be scratched.
    Retained,
}

impl VolumeState {
    /// Display name.
    pub fn name(self) -> &'static str {
        match self {
            VolumeState::Scratch => "SCRATCH",
            VolumeState::Private => "PRIVATE",
            VolumeState::Retained => "RETAINED",
        }
    }
}

/// A tape volume in the registry.
#[derive(Debug, Clone)]
pub struct TapeVolume {
    /// Volume serial (6 characters).
    pub volser: String,
    /// Current state.
    pub state: VolumeState,
    /// Datasets stored on this volume.
    pub datasets: Vec<TapeDataset>,
    /// When the volume was last used.
    pub last_used: SystemTime,
    /// Pool name (e.g., "SCRATCH1", "OFFSITE").
    pub pool: String,
}

/// A dataset on a tape volume.
#[derive(Debug, Clone)]
pub struct TapeDataset {
    /// Dataset name.
    pub dsn: String,
    /// File sequence number on tape.
    pub file_seq: u32,
    /// Creation date.
    pub created: SystemTime,
    /// Whether the dataset is still cataloged.
    pub cataloged: bool,
}

// ─────────────────────── VRS ───────────────────────

/// VRS retention type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RetentionType {
    /// Retain for a fixed number of days.
    Days(u32),
    /// Retain while the dataset remains cataloged.
    WhileCatalog,
    /// Retain a fixed number of cycles (backup generations).
    Cycles(u32),
}

/// A Vital Record Specification.
#[derive(Debug, Clone)]
pub struct Vrs {
    /// VRS name.
    pub name: String,
    /// Dataset name pattern (e.g., "PROD.BACKUP.**").
    pub pattern: String,
    /// Retention type.
    pub retention: RetentionType,
}

impl Vrs {
    /// Create a new VRS with days-based retention.
    pub fn days(name: &str, pattern: &str, days: u32) -> Self {
        Self {
            name: name.to_string(),
            pattern: pattern.to_uppercase(),
            retention: RetentionType::Days(days),
        }
    }

    /// Create a new VRS with WHILECATALOG retention.
    pub fn while_catalog(name: &str, pattern: &str) -> Self {
        Self {
            name: name.to_string(),
            pattern: pattern.to_uppercase(),
            retention: RetentionType::WhileCatalog,
        }
    }

    /// Create a new VRS with cycle-based retention.
    pub fn cycles(name: &str, pattern: &str, count: u32) -> Self {
        Self {
            name: name.to_string(),
            pattern: pattern.to_uppercase(),
            retention: RetentionType::Cycles(count),
        }
    }

    /// Check if a dataset name matches this VRS pattern.
    pub fn matches_dsn(&self, dsn: &str) -> bool {
        let dsn_upper = dsn.to_uppercase();
        let pat = &self.pattern;
        if pat.ends_with(".**") {
            let prefix = &pat[..pat.len() - 3];
            dsn_upper.starts_with(prefix) && dsn_upper.len() > prefix.len()
        } else if pat.ends_with(".*") {
            let prefix = &pat[..pat.len() - 2];
            dsn_upper.starts_with(prefix)
                && dsn_upper[prefix.len()..].starts_with('.')
                && !dsn_upper[prefix.len() + 1..].contains('.')
        } else {
            dsn_upper == *pat
        }
    }

    /// Check if retention is still active for a dataset.
    pub fn is_retained(&self, dataset: &TapeDataset, now: SystemTime) -> bool {
        match &self.retention {
            RetentionType::Days(days) => {
                let age = now
                    .duration_since(dataset.created)
                    .unwrap_or(Duration::ZERO);
                age < Duration::from_secs(*days as u64 * 86400)
            }
            RetentionType::WhileCatalog => dataset.cataloged,
            RetentionType::Cycles(_) => {
                // Cycle-based retention is checked externally
                true
            }
        }
    }
}

// ─────────────────────── RMM Engine ───────────────────────

/// DFSMSrmm tape management engine.
#[derive(Debug)]
pub struct Rmm {
    /// Volume registry.
    volumes: HashMap<String, TapeVolume>,
    /// VRS policies.
    vrs_policies: Vec<Vrs>,
}

impl Rmm {
    /// Create a new RMM instance.
    pub fn new() -> Self {
        Self {
            volumes: HashMap::new(),
            vrs_policies: Vec::new(),
        }
    }

    /// Add a VRS policy.
    pub fn add_vrs(&mut self, vrs: Vrs) {
        self.vrs_policies.push(vrs);
    }

    /// Register a scratch volume.
    pub fn add_scratch_volume(&mut self, volser: &str, pool: &str) {
        self.volumes.insert(
            volser.to_uppercase(),
            TapeVolume {
                volser: volser.to_uppercase(),
                state: VolumeState::Scratch,
                datasets: Vec::new(),
                last_used: SystemTime::now(),
                pool: pool.to_string(),
            },
        );
    }

    /// Allocate a scratch volume for a new dataset.
    ///
    /// Moves the volume from Scratch to Private state.
    pub fn allocate_volume(&mut self, dsn: &str) -> Option<String> {
        // Find first scratch volume
        let volser = self
            .volumes
            .values()
            .find(|v| v.state == VolumeState::Scratch)
            .map(|v| v.volser.clone())?;

        let vol = self.volumes.get_mut(&volser)?;
        vol.state = VolumeState::Private;
        vol.last_used = SystemTime::now();
        vol.datasets.push(TapeDataset {
            dsn: dsn.to_uppercase(),
            file_seq: vol.datasets.len() as u32 + 1,
            created: SystemTime::now(),
            cataloged: true,
        });

        Some(volser)
    }

    /// Get a volume by volser.
    pub fn get_volume(&self, volser: &str) -> Option<&TapeVolume> {
        self.volumes.get(&volser.to_uppercase())
    }

    /// Get mutable volume by volser.
    pub fn get_volume_mut(&mut self, volser: &str) -> Option<&mut TapeVolume> {
        self.volumes.get_mut(&volser.to_uppercase())
    }

    /// Count scratch volumes.
    pub fn scratch_count(&self) -> usize {
        self.volumes
            .values()
            .filter(|v| v.state == VolumeState::Scratch)
            .count()
    }

    /// Count private volumes.
    pub fn private_count(&self) -> usize {
        self.volumes
            .values()
            .filter(|v| v.state == VolumeState::Private || v.state == VolumeState::Retained)
            .count()
    }

    /// Total volume count.
    pub fn volume_count(&self) -> usize {
        self.volumes.len()
    }

    /// Find the matching VRS policy for a dataset name.
    fn find_vrs(&self, dsn: &str) -> Option<&Vrs> {
        self.vrs_policies.iter().find(|v| v.matches_dsn(dsn))
    }

    /// Check if a volume should remain in private state.
    ///
    /// A volume stays private if ANY dataset on it has active retention.
    fn has_active_retention(&self, volume: &TapeVolume, now: SystemTime) -> bool {
        for dataset in &volume.datasets {
            if let Some(vrs) = self.find_vrs(&dataset.dsn) {
                if vrs.is_retained(dataset, now) {
                    return true;
                }
            }
        }
        false
    }

    /// VRSEL — Vital Record Selection processing.
    ///
    /// Evaluates all private/retained volumes against VRS policies.
    /// Volumes with expired retention are moved to scratch pool.
    ///
    /// Returns (scratched_count, retained_count).
    pub fn vrsel(&mut self, now: SystemTime) -> (usize, usize) {
        // Collect decisions first to avoid borrow issues
        let decisions: Vec<(String, bool)> = self
            .volumes
            .values()
            .filter(|v| v.state == VolumeState::Private || v.state == VolumeState::Retained)
            .map(|v| {
                let retain = self.has_active_retention(v, now);
                (v.volser.clone(), retain)
            })
            .collect();

        let mut scratched = 0;
        let mut retained = 0;

        for (volser, should_retain) in decisions {
            if let Some(vol) = self.volumes.get_mut(&volser) {
                if should_retain {
                    vol.state = VolumeState::Retained;
                    retained += 1;
                } else {
                    vol.state = VolumeState::Scratch;
                    vol.datasets.clear();
                    scratched += 1;
                }
            }
        }

        (scratched, retained)
    }
}

impl Default for Rmm {
    fn default() -> Self {
        Self::new()
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn past_time(days: u64) -> SystemTime {
        SystemTime::now() - Duration::from_secs(days * 86400)
    }

    // ─── DFSMS-107.1: Volume Registry and State Machine ───

    #[test]
    fn test_volume_lifecycle() {
        let mut rmm = Rmm::new();
        rmm.add_scratch_volume("VOL001", "SCRATCH1");

        // Initial state: Scratch
        let vol = rmm.get_volume("VOL001").unwrap();
        assert_eq!(vol.state, VolumeState::Scratch);

        // Allocate: Scratch → Private
        let volser = rmm.allocate_volume("MY.TAPE.DATA").unwrap();
        assert_eq!(volser, "VOL001");
        let vol = rmm.get_volume("VOL001").unwrap();
        assert_eq!(vol.state, VolumeState::Private);
        assert_eq!(vol.datasets.len(), 1);
    }

    #[test]
    fn test_volume_state_names() {
        assert_eq!(VolumeState::Scratch.name(), "SCRATCH");
        assert_eq!(VolumeState::Private.name(), "PRIVATE");
        assert_eq!(VolumeState::Retained.name(), "RETAINED");
    }

    #[test]
    fn test_retention_to_scratch_transition() {
        let mut rmm = Rmm::new();
        rmm.add_vrs(Vrs::days("V1", "SHORT.**", 7));
        rmm.add_scratch_volume("VOL001", "POOL1");

        // Allocate with a short-retention dataset
        rmm.allocate_volume("SHORT.DATA").unwrap();

        // Set creation date to 10 days ago
        if let Some(vol) = rmm.get_volume_mut("VOL001") {
            vol.datasets[0].created = past_time(10);
        }

        // VRSEL should move to scratch (7-day retention expired)
        let (scratched, _) = rmm.vrsel(SystemTime::now());
        assert_eq!(scratched, 1);

        let vol = rmm.get_volume("VOL001").unwrap();
        assert_eq!(vol.state, VolumeState::Scratch);
    }

    // ─── DFSMS-107.2: VRS Policies ───

    #[test]
    fn test_vrs_days_retention() {
        let vrs = Vrs::days("DAILY", "PROD.BACKUP.**", 365);

        assert!(vrs.matches_dsn("PROD.BACKUP.D20260101"));
        assert!(!vrs.matches_dsn("TEST.BACKUP.D20260101"));

        // Recent dataset — retained
        let ds = TapeDataset {
            dsn: "PROD.BACKUP.D20260101".to_string(),
            file_seq: 1,
            created: SystemTime::now(),
            cataloged: true,
        };
        assert!(vrs.is_retained(&ds, SystemTime::now()));

        // Old dataset — expired
        let old_ds = TapeDataset {
            dsn: "PROD.BACKUP.D20240101".to_string(),
            file_seq: 1,
            created: past_time(400),
            cataloged: true,
        };
        assert!(!vrs.is_retained(&old_ds, SystemTime::now()));
    }

    #[test]
    fn test_vrs_while_catalog() {
        let vrs = Vrs::while_catalog("WCAT", "CRITICAL.**");

        let cataloged_ds = TapeDataset {
            dsn: "CRITICAL.DATA".to_string(),
            file_seq: 1,
            created: past_time(1000), // Very old
            cataloged: true,
        };
        assert!(vrs.is_retained(&cataloged_ds, SystemTime::now()));

        let uncataloged_ds = TapeDataset {
            dsn: "CRITICAL.DATA".to_string(),
            file_seq: 1,
            created: SystemTime::now(),
            cataloged: false,
        };
        assert!(!vrs.is_retained(&uncataloged_ds, SystemTime::now()));
    }

    #[test]
    fn test_vrs_pattern_matching() {
        let vrs = Vrs::days("TEST", "APP.DATA.**", 30);
        assert!(vrs.matches_dsn("APP.DATA.FILE1"));
        assert!(vrs.matches_dsn("APP.DATA.BACKUP.V1"));
        assert!(!vrs.matches_dsn("APP.LOG.FILE"));
        assert!(!vrs.matches_dsn("APP.DATA")); // no extra qualifier
    }

    // ─── DFSMS-107.3: Scratch/Private Pool ───

    #[test]
    fn test_scratch_pool_allocation() {
        let mut rmm = Rmm::new();

        // Add 100 scratch volumes
        for i in 0..100 {
            rmm.add_scratch_volume(&format!("V{:05}", i), "POOL1");
        }
        assert_eq!(rmm.scratch_count(), 100);
        assert_eq!(rmm.private_count(), 0);

        // Allocate 10
        for i in 0..10 {
            let vol = rmm.allocate_volume(&format!("DS.{}", i)).unwrap();
            assert!(!vol.is_empty());
        }

        assert_eq!(rmm.scratch_count(), 90);
        assert_eq!(rmm.private_count(), 10);
    }

    #[test]
    fn test_expired_returns_to_scratch() {
        let mut rmm = Rmm::new();
        rmm.add_vrs(Vrs::days("TEMP", "TEMP.**", 1));

        rmm.add_scratch_volume("V00001", "POOL1");
        rmm.allocate_volume("TEMP.WORK").unwrap();

        // Make creation 2 days ago
        if let Some(vol) = rmm.get_volume_mut("V00001") {
            vol.datasets[0].created = past_time(2);
        }

        let (scratched, _) = rmm.vrsel(SystemTime::now());
        assert_eq!(scratched, 1);
        assert_eq!(rmm.scratch_count(), 1);
    }

    // ─── DFSMS-107.4: VRSEL Processing ───

    #[test]
    fn test_vrsel_1000_volumes() {
        let mut rmm = Rmm::new();
        rmm.add_vrs(Vrs::days("SHORT", "SHORT.**", 30));
        rmm.add_vrs(Vrs::days("LONG", "LONG.**", 365));

        // Create 1000 volumes: 500 short-retention (expired), 500 long-retention
        for i in 0..500 {
            let volser = format!("S{:05}", i);
            rmm.add_scratch_volume(&volser, "POOL1");
            rmm.allocate_volume(&format!("SHORT.DS{}", i)).unwrap();

            // Make them 60 days old (expired for 30-day VRS)
            if let Some(vol) = rmm.get_volume_mut(&volser) {
                vol.datasets[0].created = past_time(60);
            }
        }

        for i in 0..500 {
            let volser = format!("L{:05}", i);
            rmm.add_scratch_volume(&volser, "POOL1");
            rmm.allocate_volume(&format!("LONG.DS{}", i)).unwrap();

            // Make them 60 days old (still valid for 365-day VRS)
            if let Some(vol) = rmm.get_volume_mut(&volser) {
                vol.datasets[0].created = past_time(60);
            }
        }

        let (scratched, retained) = rmm.vrsel(SystemTime::now());
        assert_eq!(scratched, 500); // Short-retention expired
        assert_eq!(retained, 500); // Long-retention still active
    }

    #[test]
    fn test_vrsel_multi_dataset_volume() {
        let mut rmm = Rmm::new();
        rmm.add_vrs(Vrs::days("SHORT", "SHORT.**", 7));
        rmm.add_vrs(Vrs::days("LONG", "LONG.**", 365));

        rmm.add_scratch_volume("MULTI1", "POOL1");
        rmm.allocate_volume("SHORT.OLD").unwrap();

        // Add a second dataset with long retention
        if let Some(vol) = rmm.get_volume_mut("MULTI1") {
            vol.datasets[0].created = past_time(30); // Expired
            vol.datasets.push(TapeDataset {
                dsn: "LONG.KEEP".to_string(),
                file_seq: 2,
                created: SystemTime::now(), // Fresh
                cataloged: true,
            });
        }

        // Volume should stay retained because LONG.KEEP has active retention
        let (scratched, retained) = rmm.vrsel(SystemTime::now());
        assert_eq!(scratched, 0);
        assert_eq!(retained, 1);
    }

    // ─── DFSMS-107.5: Integration Tests ───

    #[test]
    fn test_full_vrsel_scenario() {
        let mut rmm = Rmm::new();

        // Define VRS policies
        rmm.add_vrs(Vrs::days("D7", "TEMP.**", 7));
        rmm.add_vrs(Vrs::days("D30", "PROD.DAILY.**", 30));
        rmm.add_vrs(Vrs::days("D365", "PROD.ANNUAL.**", 365));
        rmm.add_vrs(Vrs::while_catalog("WCAT", "CRITICAL.**"));

        // Create 10 volumes with different datasets
        let scenarios = [
            ("VOL001", "TEMP.WORK1", 10u64, true),     // Expired (7-day, 10 days old)
            ("VOL002", "TEMP.WORK2", 3, true),          // Active (7-day, 3 days old)
            ("VOL003", "PROD.DAILY.D1", 40, true),      // Expired (30-day, 40 days old)
            ("VOL004", "PROD.DAILY.D2", 15, true),      // Active (30-day, 15 days old)
            ("VOL005", "PROD.ANNUAL.2025", 200, true),   // Active (365-day, 200 days old)
            ("VOL006", "PROD.ANNUAL.2024", 400, true),   // Expired (365-day, 400 days old)
            ("VOL007", "CRITICAL.SYS", 1000, true),      // Retained (WHILECATALOG, cataloged)
            ("VOL008", "CRITICAL.OLD", 500, false),       // Expired (WHILECATALOG, uncataloged)
            ("VOL009", "UNKNOWN.DS", 5, true),            // No VRS → no retention → scratch
            ("VOL010", "TEMP.RECENT", 1, true),           // Active (7-day, 1 day old)
        ];

        for (volser, dsn, age_days, cataloged) in &scenarios {
            rmm.add_scratch_volume(volser, "POOL1");
            rmm.allocate_volume(dsn).unwrap();
            if let Some(vol) = rmm.get_volume_mut(volser) {
                vol.datasets[0].created = past_time(*age_days);
                vol.datasets[0].cataloged = *cataloged;
            }
        }

        let (scratched, retained) = rmm.vrsel(SystemTime::now());

        // Should scratch: VOL001 (temp expired), VOL003 (daily expired),
        //   VOL006 (annual expired), VOL008 (uncataloged), VOL009 (no VRS)
        assert_eq!(scratched, 5, "Expected 5 scratched");

        // Should retain: VOL002, VOL004, VOL005, VOL007, VOL010
        assert_eq!(retained, 5, "Expected 5 retained");

        // Verify specific volumes
        assert_eq!(
            rmm.get_volume("VOL001").unwrap().state,
            VolumeState::Scratch
        );
        assert_eq!(
            rmm.get_volume("VOL002").unwrap().state,
            VolumeState::Retained
        );
        assert_eq!(
            rmm.get_volume("VOL007").unwrap().state,
            VolumeState::Retained
        );
        assert_eq!(
            rmm.get_volume("VOL009").unwrap().state,
            VolumeState::Scratch
        );
    }
}
