//! VSAM free space management for KSDS datasets.
//!
//! Implements the `FREESPACE(ci_pct, ca_pct)` parameter that reserves
//! space in each Control Interval (CI) and Control Area (CA) during
//! initial load to reduce CI/CA splits during subsequent inserts.
//!
//! # Architecture
//!
//! - **CI-level tracking**: Each CI has a capacity and occupancy count.
//!   When a CI is full, a CI split moves the upper half of records to a
//!   new CI and updates the index.
//! - **CA-level tracking**: A CA is a group of CIs. When all CIs in a
//!   CA are full, a CA split extends the data component.
//! - **Split statistics**: CI split and CA split counts are maintained
//!   for LISTCAT reporting.
//!
//! # Example
//!
//! ```
//! use open_mainframe_dataset::vsam::freespace::{FreeSpaceConfig, FreeSpaceManager};
//!
//! let config = FreeSpaceConfig::new(20, 10); // 20% CI, 10% CA
//! let mut mgr = FreeSpaceManager::new(config, 4096, 100);
//! mgr.initialize_cis(16);
//! let ci = mgr.find_ci_with_space(50).unwrap();
//! mgr.record_insert(ci, 50);
//! ```

use crate::error::DatasetError;

/// Free space configuration from DEFINE CLUSTER FREESPACE parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FreeSpaceConfig {
    /// Percentage of each CI to leave free (0–100).
    pub ci_pct: u8,
    /// Percentage of each CA to leave free (0–100).
    pub ca_pct: u8,
}

impl FreeSpaceConfig {
    /// Create a new FREESPACE configuration.
    ///
    /// `ci_pct` is the percentage of each CI left free during initial load.
    /// `ca_pct` is the percentage of each CA left free during initial load.
    pub fn new(ci_pct: u8, ca_pct: u8) -> Self {
        Self {
            ci_pct: ci_pct.min(100),
            ca_pct: ca_pct.min(100),
        }
    }

    /// No free space reserved.
    pub fn none() -> Self {
        Self {
            ci_pct: 0,
            ca_pct: 0,
        }
    }
}

impl Default for FreeSpaceConfig {
    fn default() -> Self {
        Self::none()
    }
}

/// Per-CI occupancy tracking.
#[derive(Debug, Clone)]
pub struct CiInfo {
    /// CI index (0-based).
    pub ci_index: usize,
    /// Maximum number of records this CI can hold.
    pub capacity: usize,
    /// Current number of records in this CI.
    pub occupancy: usize,
}

impl CiInfo {
    /// Returns true if the CI has room for at least one more record.
    pub fn has_space(&self, record_size: usize) -> bool {
        let _ = record_size; // capacity already accounts for record size
        self.occupancy < self.capacity
    }

    /// Returns the number of free slots.
    pub fn free_slots(&self) -> usize {
        self.capacity.saturating_sub(self.occupancy)
    }

    /// Returns the occupancy percentage (0–100).
    pub fn occupancy_pct(&self) -> u8 {
        if self.capacity == 0 {
            return 100;
        }
        ((self.occupancy as u64 * 100) / self.capacity as u64) as u8
    }
}

/// Free space manager for a VSAM KSDS.
///
/// Tracks per-CI occupancy and handles CI/CA splits.
pub struct FreeSpaceManager {
    /// Free space configuration.
    config: FreeSpaceConfig,
    /// CI size in bytes.
    ci_size: usize,
    /// Maximum record size in bytes.
    record_size: usize,
    /// Per-CI occupancy info.
    cis: Vec<CiInfo>,
    /// Number of CIs per CA.
    cis_per_ca: usize,
    /// CI split count.
    ci_splits: u64,
    /// CA split count.
    ca_splits: u64,
}

impl FreeSpaceManager {
    /// Create a new free space manager.
    ///
    /// `ci_size` is the control interval size in bytes.
    /// `record_size` is the maximum record size in bytes.
    pub fn new(config: FreeSpaceConfig, ci_size: usize, record_size: usize) -> Self {
        // Default CA = 16 CIs (typical z/OS default)
        let cis_per_ca = 16;

        Self {
            config,
            ci_size,
            record_size,
            cis: Vec::new(),
            cis_per_ca,
            ci_splits: 0,
            ca_splits: 0,
        }
    }

    /// Initialize CIs for initial load with free space reservation.
    ///
    /// Creates `num_cis` control intervals, each with capacity reduced
    /// by the CI free-space percentage.
    pub fn initialize_cis(&mut self, num_cis: usize) {
        let raw_capacity = if self.record_size > 0 {
            self.ci_size / self.record_size
        } else {
            1
        };

        // Apply CI free space percentage
        let usable = if self.config.ci_pct > 0 {
            let reserved = (raw_capacity as u64 * self.config.ci_pct as u64) / 100;
            raw_capacity.saturating_sub(reserved as usize).max(1)
        } else {
            raw_capacity
        };

        // Apply CA free space: skip ca_pct% of CIs in each CA
        let cis_to_skip_per_ca = if self.config.ca_pct > 0 {
            let skip = (self.cis_per_ca as u64 * self.config.ca_pct as u64) / 100;
            skip as usize
        } else {
            0
        };

        self.cis.clear();
        for i in 0..num_cis {
            let ca_position = i % self.cis_per_ca;
            let is_ca_reserved = ca_position >= (self.cis_per_ca - cis_to_skip_per_ca);

            self.cis.push(CiInfo {
                ci_index: i,
                capacity: if is_ca_reserved { 0 } else { usable },
                occupancy: 0,
            });
        }
    }

    /// Find a CI with space for a record of the given size.
    ///
    /// Returns the CI index, or None if all CIs are full (need a split).
    pub fn find_ci_with_space(&self, record_size: usize) -> Option<usize> {
        self.cis
            .iter()
            .find(|ci| ci.has_space(record_size))
            .map(|ci| ci.ci_index)
    }

    /// Record that a record was inserted into the given CI.
    pub fn record_insert(&mut self, ci_index: usize, _record_size: usize) {
        if let Some(ci) = self.cis.iter_mut().find(|ci| ci.ci_index == ci_index) {
            ci.occupancy += 1;
        }
    }

    /// Record that a record was deleted from the given CI.
    pub fn record_delete(&mut self, ci_index: usize) {
        if let Some(ci) = self.cis.iter_mut().find(|ci| ci.ci_index == ci_index) {
            ci.occupancy = ci.occupancy.saturating_sub(1);
        }
    }

    /// Perform a CI split: move upper half of records from `ci_index` to a new CI.
    ///
    /// Returns `(new_ci_index, records_to_move)` where `records_to_move` is the
    /// count of records that should be physically moved to the new CI.
    pub fn ci_split(&mut self, ci_index: usize) -> Result<(usize, usize), DatasetError> {
        let ci = self
            .cis
            .iter()
            .find(|ci| ci.ci_index == ci_index)
            .ok_or_else(|| {
                DatasetError::InvalidParameter(format!("CI {} not found", ci_index))
            })?;

        let total = ci.occupancy;
        let capacity = ci.capacity;
        let to_move = total / 2;
        let to_keep = total - to_move;

        // Create new CI
        let new_ci_index = self.cis.len();
        self.cis.push(CiInfo {
            ci_index: new_ci_index,
            capacity,
            occupancy: to_move,
        });

        // Update old CI
        if let Some(old_ci) = self.cis.iter_mut().find(|ci| ci.ci_index == ci_index) {
            old_ci.occupancy = to_keep;
        }

        self.ci_splits += 1;
        Ok((new_ci_index, to_move))
    }

    /// Perform a CA split: add a new CA worth of CIs.
    ///
    /// Returns the range of new CI indices.
    pub fn ca_split(&mut self) -> (usize, usize) {
        let start = self.cis.len();
        let raw_capacity = if self.record_size > 0 {
            self.ci_size / self.record_size
        } else {
            1
        };

        for i in 0..self.cis_per_ca {
            self.cis.push(CiInfo {
                ci_index: start + i,
                capacity: raw_capacity,
                occupancy: 0,
            });
        }

        self.ca_splits += 1;
        (start, start + self.cis_per_ca)
    }

    /// Get CI split count.
    pub fn ci_split_count(&self) -> u64 {
        self.ci_splits
    }

    /// Get CA split count.
    pub fn ca_split_count(&self) -> u64 {
        self.ca_splits
    }

    /// Get the total number of CIs.
    pub fn ci_count(&self) -> usize {
        self.cis.len()
    }

    /// Get CI info by index.
    pub fn ci_info(&self, ci_index: usize) -> Option<&CiInfo> {
        self.cis.iter().find(|ci| ci.ci_index == ci_index)
    }

    /// Get the free space configuration.
    pub fn config(&self) -> &FreeSpaceConfig {
        &self.config
    }

    /// Get the total free slots across all CIs.
    pub fn total_free_slots(&self) -> usize {
        self.cis.iter().map(|ci| ci.free_slots()).sum()
    }

    /// Get the total occupied slots across all CIs.
    pub fn total_occupied(&self) -> usize {
        self.cis.iter().map(|ci| ci.occupancy).sum()
    }

    /// Get statistics for LISTCAT reporting.
    pub fn statistics(&self) -> FreeSpaceStatistics {
        FreeSpaceStatistics {
            ci_splits: self.ci_splits,
            ca_splits: self.ca_splits,
            total_cis: self.cis.len(),
            total_records: self.total_occupied(),
            total_free_slots: self.total_free_slots(),
            ci_free_pct: self.config.ci_pct,
            ca_free_pct: self.config.ca_pct,
        }
    }
}

/// Statistics for LISTCAT output.
#[derive(Debug, Clone)]
pub struct FreeSpaceStatistics {
    /// Number of CI splits that have occurred.
    pub ci_splits: u64,
    /// Number of CA splits that have occurred.
    pub ca_splits: u64,
    /// Total number of CIs in the dataset.
    pub total_cis: usize,
    /// Total number of records stored.
    pub total_records: usize,
    /// Total free record slots.
    pub total_free_slots: usize,
    /// Configured CI free percentage.
    pub ci_free_pct: u8,
    /// Configured CA free percentage.
    pub ca_free_pct: u8,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Story 604.1: FREESPACE parameter reserves space during initial load.
    #[test]
    fn test_freespace_initial_load() {
        let config = FreeSpaceConfig::new(20, 10);
        let mut mgr = FreeSpaceManager::new(config, 4096, 100);
        // 4096 / 100 = 40 records per CI raw
        // 20% CI free → 40 - 8 = 32 records usable per CI
        mgr.initialize_cis(16);

        // First 14 CIs should have capacity (16 - 10% of 16 = 14.4 → 14 usable)
        let ci0 = mgr.ci_info(0).unwrap();
        assert_eq!(ci0.capacity, 32); // 40 * 0.80 = 32

        // Last ~2 CIs in the CA should be reserved (capacity 0)
        // CA free 10% of 16 = 1.6 → 1 CI reserved
        let ci15 = mgr.ci_info(15).unwrap();
        assert_eq!(ci15.capacity, 0); // Reserved by CA free space
    }

    /// Story 604.1: No free space config means full capacity.
    #[test]
    fn test_no_freespace() {
        let config = FreeSpaceConfig::none();
        let mut mgr = FreeSpaceManager::new(config, 4096, 100);
        mgr.initialize_cis(4);

        let ci0 = mgr.ci_info(0).unwrap();
        assert_eq!(ci0.capacity, 40); // Full: 4096 / 100
    }

    /// Story 604.1: Find CI with available space.
    #[test]
    fn test_find_ci_with_space() {
        let config = FreeSpaceConfig::none();
        let mut mgr = FreeSpaceManager::new(config, 4096, 100);
        mgr.initialize_cis(4);

        assert!(mgr.find_ci_with_space(100).is_some());

        // Fill first CI
        for _ in 0..40 {
            mgr.record_insert(0, 100);
        }

        // CI 0 is now full, should find CI 1
        let ci = mgr.find_ci_with_space(100).unwrap();
        assert_ne!(ci, 0);
    }

    /// Story 604.2: CI split moves half the records.
    #[test]
    fn test_ci_split() {
        let config = FreeSpaceConfig::none();
        let mut mgr = FreeSpaceManager::new(config, 4096, 100);
        mgr.initialize_cis(4);

        // Fill CI 0 with 40 records
        for _ in 0..40 {
            mgr.record_insert(0, 100);
        }
        assert_eq!(mgr.ci_info(0).unwrap().occupancy, 40);

        // Split CI 0
        let (new_ci, moved) = mgr.ci_split(0).unwrap();
        assert_eq!(moved, 20); // Half of 40

        // Old CI should have 20 records
        assert_eq!(mgr.ci_info(0).unwrap().occupancy, 20);
        // New CI should have 20 records
        assert_eq!(mgr.ci_info(new_ci).unwrap().occupancy, 20);
        assert_eq!(mgr.ci_split_count(), 1);
    }

    /// Story 604.2: CA split adds new CIs.
    #[test]
    fn test_ca_split() {
        let config = FreeSpaceConfig::none();
        let mut mgr = FreeSpaceManager::new(config, 4096, 100);
        mgr.initialize_cis(16);
        assert_eq!(mgr.ci_count(), 16);

        let (start, end) = mgr.ca_split();
        assert_eq!(start, 16);
        assert_eq!(end, 32);
        assert_eq!(mgr.ci_count(), 32);
        assert_eq!(mgr.ca_split_count(), 1);
    }

    /// Story 604.2: Split statistics are reported.
    #[test]
    fn test_split_statistics() {
        let config = FreeSpaceConfig::new(20, 10);
        let mut mgr = FreeSpaceManager::new(config, 4096, 100);
        mgr.initialize_cis(16);

        // Fill and split
        for _ in 0..32 {
            mgr.record_insert(0, 100);
        }
        mgr.ci_split(0).unwrap();

        let stats = mgr.statistics();
        assert_eq!(stats.ci_splits, 1);
        assert_eq!(stats.ca_splits, 0);
        assert_eq!(stats.ci_free_pct, 20);
        assert_eq!(stats.ca_free_pct, 10);
        assert!(stats.total_records > 0);
    }

    /// Story 604.1: Record delete frees a slot.
    #[test]
    fn test_record_delete_frees_slot() {
        let config = FreeSpaceConfig::none();
        let mut mgr = FreeSpaceManager::new(config, 4096, 100);
        mgr.initialize_cis(2);

        // Fill CI 0
        for _ in 0..40 {
            mgr.record_insert(0, 100);
        }
        assert_eq!(mgr.ci_info(0).unwrap().free_slots(), 0);

        // Delete one record
        mgr.record_delete(0);
        assert_eq!(mgr.ci_info(0).unwrap().free_slots(), 1);
        assert_eq!(mgr.ci_info(0).unwrap().occupancy, 39);
    }

    /// Story 604.1: FREESPACE config with max values.
    #[test]
    fn test_freespace_config_max() {
        let config = FreeSpaceConfig::new(200, 150); // clamped to 100
        assert_eq!(config.ci_pct, 100);
        assert_eq!(config.ca_pct, 100);
    }

    /// Story 604.2: Multiple CI splits accumulate.
    #[test]
    fn test_multiple_ci_splits() {
        let config = FreeSpaceConfig::none();
        let mut mgr = FreeSpaceManager::new(config, 4096, 100);
        mgr.initialize_cis(4);

        // Fill and split CI 0
        for _ in 0..40 {
            mgr.record_insert(0, 100);
        }
        mgr.ci_split(0).unwrap();

        // Fill and split CI 1
        for _ in 0..40 {
            mgr.record_insert(1, 100);
        }
        mgr.ci_split(1).unwrap();

        assert_eq!(mgr.ci_split_count(), 2);
        assert_eq!(mgr.ci_count(), 6); // 4 original + 2 from splits
    }
}
