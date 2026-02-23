//! # Space & Volume Management
//!
//! Implements z/OS-style space allocation, extent tracking, VTOC (Volume Table
//! of Contents), multi-volume datasets, and volume-level operations.

use std::collections::{BTreeMap, HashMap};

// ─────────────────────── Space Units ───────────────────────

/// Space allocation unit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpaceAllocationUnit {
    /// Tracks (approximately 56 KB per track on 3390).
    Tracks,
    /// Cylinders (15 tracks per cylinder on 3390, ~850 KB).
    Cylinders,
    /// Bytes (block size specified, count is number of blocks).
    Bytes(u32),
    /// Records (average record length used to compute blocks).
    Records(u32),
}

/// Bytes per track on a 3390 device.
pub const BYTES_PER_TRACK_3390: u64 = 56_664;
/// Tracks per cylinder on a 3390 device.
pub const TRACKS_PER_CYLINDER_3390: u64 = 15;
/// Bytes per cylinder on a 3390 device.
pub const BYTES_PER_CYLINDER_3390: u64 = BYTES_PER_TRACK_3390 * TRACKS_PER_CYLINDER_3390;
/// Maximum extents for a non-extended format dataset.
pub const MAX_EXTENTS: usize = 16;

/// Space allocation request (maps to JCL SPACE parameter).
#[derive(Debug, Clone)]
pub struct SpaceAllocation {
    /// Allocation unit.
    pub unit: SpaceAllocationUnit,
    /// Primary quantity.
    pub primary: u32,
    /// Secondary quantity.
    pub secondary: u32,
    /// Release unused space on close (RLSE).
    pub rlse: bool,
    /// Directory blocks for PDS/PDSE.
    pub directory_blocks: u32,
}

impl SpaceAllocation {
    /// Create a new space allocation.
    pub fn new(unit: SpaceAllocationUnit, primary: u32, secondary: u32) -> Self {
        Self {
            unit,
            primary,
            secondary,
            rlse: false,
            directory_blocks: 0,
        }
    }

    /// Set the RLSE (release unused space) flag.
    pub fn with_rlse(mut self) -> Self {
        self.rlse = true;
        self
    }

    /// Set directory blocks for PDS.
    pub fn with_directory(mut self, blocks: u32) -> Self {
        self.directory_blocks = blocks;
        self
    }

    /// Convert primary allocation to bytes.
    pub fn primary_bytes(&self) -> u64 {
        self.quantity_to_bytes(self.primary)
    }

    /// Convert secondary allocation to bytes.
    pub fn secondary_bytes(&self) -> u64 {
        self.quantity_to_bytes(self.secondary)
    }

    fn quantity_to_bytes(&self, qty: u32) -> u64 {
        let qty = qty as u64;
        match self.unit {
            SpaceAllocationUnit::Tracks => qty * BYTES_PER_TRACK_3390,
            SpaceAllocationUnit::Cylinders => qty * BYTES_PER_CYLINDER_3390,
            SpaceAllocationUnit::Bytes(blksize) => qty * blksize as u64,
            SpaceAllocationUnit::Records(avglen) => {
                // Approximate: records * avg length, rounded up to tracks
                let total_bytes = qty * avglen as u64;
                let tracks = (total_bytes + BYTES_PER_TRACK_3390 - 1) / BYTES_PER_TRACK_3390;
                tracks * BYTES_PER_TRACK_3390
            }
        }
    }
}

// ─────────────────────── Extent Tracking ───────────────────────

/// A single extent (contiguous space allocation on a volume).
#[derive(Debug, Clone)]
pub struct Extent {
    /// Extent sequence number (0-based).
    pub sequence: u32,
    /// Volume serial.
    pub volser: String,
    /// Starting position (byte offset on volume).
    pub start: u64,
    /// Size in bytes.
    pub size: u64,
}

/// Tracks extents for a dataset across one or more volumes.
#[derive(Debug, Clone)]
pub struct ExtentList {
    /// All extents in order.
    extents: Vec<Extent>,
    /// Maximum extents allowed.
    max_extents: usize,
    /// Secondary allocation size in bytes.
    secondary_bytes: u64,
    /// Release unused space on close.
    rlse: bool,
    /// Total bytes written.
    bytes_used: u64,
}

impl ExtentList {
    /// Create a new extent list with primary allocation.
    pub fn new(space: &SpaceAllocation, volser: &str) -> Self {
        let primary = space.primary_bytes();
        let initial_extent = Extent {
            sequence: 0,
            volser: volser.to_string(),
            start: 0,
            size: primary,
        };
        Self {
            extents: vec![initial_extent],
            max_extents: MAX_EXTENTS,
            secondary_bytes: space.secondary_bytes(),
            rlse: space.rlse,
            bytes_used: 0,
        }
    }

    /// Total allocated bytes across all extents.
    pub fn total_allocated(&self) -> u64 {
        self.extents.iter().map(|e| e.size).sum()
    }

    /// Get the number of extents.
    pub fn extent_count(&self) -> usize {
        self.extents.len()
    }

    /// Record bytes written and allocate secondary extents as needed.
    /// Returns Err if max extents exceeded.
    pub fn write_bytes(&mut self, count: u64, volumes: &[String]) -> Result<(), SpaceError> {
        self.bytes_used += count;

        while self.bytes_used > self.total_allocated() {
            if self.extents.len() >= self.max_extents {
                return Err(SpaceError::MaxExtentsExceeded {
                    dataset: String::new(),
                    max: self.max_extents,
                });
            }
            if self.secondary_bytes == 0 {
                return Err(SpaceError::NoSecondarySpace);
            }
            let seq = self.extents.len() as u32;
            // Use the last volume or pick from additional volumes
            let vol = if seq as usize - 1 < volumes.len() {
                volumes[seq as usize - 1].clone()
            } else {
                self.extents.last().unwrap().volser.clone()
            };
            let last_end = self
                .extents
                .iter()
                .filter(|e| e.volser == vol)
                .map(|e| e.start + e.size)
                .max()
                .unwrap_or(0);
            self.extents.push(Extent {
                sequence: seq,
                volser: vol,
                start: last_end,
                size: self.secondary_bytes,
            });
        }
        Ok(())
    }

    /// Release unused space (RLSE processing on close).
    pub fn release_unused(&mut self) -> u64 {
        if !self.rlse {
            return 0;
        }
        let total = self.total_allocated();
        if self.bytes_used >= total {
            return 0;
        }
        let excess = total - self.bytes_used;
        // Trim the last extent
        if let Some(last) = self.extents.last_mut() {
            if last.size > excess {
                last.size -= excess;
            } else {
                last.size = 0;
            }
        }
        excess
    }

    /// Get all extents.
    pub fn extents(&self) -> &[Extent] {
        &self.extents
    }

    /// Get bytes used.
    pub fn bytes_used(&self) -> u64 {
        self.bytes_used
    }
}

// ─────────────────────── VTOC ───────────────────────

/// DSCB Format 1 — Dataset descriptor record in VTOC.
#[derive(Debug, Clone)]
pub struct DscbFormat1 {
    /// Dataset name.
    pub dsname: String,
    /// Volume serial.
    pub volser: String,
    /// Creation date (Julian: YYDDD).
    pub creation_date: String,
    /// Expiration date (Julian: YYDDD).
    pub expiration_date: String,
    /// Record format (F, FB, V, VB, etc.).
    pub recfm: String,
    /// Logical record length.
    pub lrecl: u32,
    /// Block size.
    pub blksize: u32,
    /// Number of extents.
    pub extent_count: u32,
    /// Extents for this volume.
    pub extents: Vec<Extent>,
}

/// Volume Table of Contents — tracks all datasets on a volume.
#[derive(Debug)]
pub struct Vtoc {
    /// Volume serial.
    pub volser: String,
    /// Device type.
    pub devtype: String,
    /// Free space on volume (bytes).
    pub free_space: u64,
    /// Total capacity (bytes).
    pub capacity: u64,
    /// DSCBs indexed by dataset name.
    dscbs: BTreeMap<String, DscbFormat1>,
    /// Free space fragments (start, size).
    free_fragments: Vec<(u64, u64)>,
}

impl Vtoc {
    /// Create a new VTOC for a volume.
    pub fn new(volser: &str, capacity: u64) -> Self {
        Self {
            volser: volser.to_string(),
            devtype: "3390".to_string(),
            free_space: capacity,
            capacity,
            dscbs: BTreeMap::new(),
            free_fragments: vec![(0, capacity)],
        }
    }

    /// Allocate space for a dataset, creating a DSCB entry.
    pub fn allocate(
        &mut self,
        dsname: &str,
        recfm: &str,
        lrecl: u32,
        blksize: u32,
        space: &SpaceAllocation,
    ) -> Result<(), SpaceError> {
        let primary = space.primary_bytes();
        if primary > self.free_space {
            return Err(SpaceError::InsufficientSpace {
                volser: self.volser.clone(),
                requested: primary,
                available: self.free_space,
            });
        }

        // Find a free fragment large enough
        let frag_idx = self
            .free_fragments
            .iter()
            .position(|(_, size)| *size >= primary)
            .ok_or_else(|| SpaceError::InsufficientSpace {
                volser: self.volser.clone(),
                requested: primary,
                available: self.free_space,
            })?;

        let (frag_start, frag_size) = self.free_fragments[frag_idx];
        let extent = Extent {
            sequence: 0,
            volser: self.volser.clone(),
            start: frag_start,
            size: primary,
        };

        // Update free fragment
        if frag_size > primary {
            self.free_fragments[frag_idx] = (frag_start + primary, frag_size - primary);
        } else {
            self.free_fragments.remove(frag_idx);
        }
        self.free_space -= primary;

        let dscb = DscbFormat1 {
            dsname: dsname.to_string(),
            volser: self.volser.clone(),
            creation_date: "26054".to_string(), // 2026, day 54
            expiration_date: "00000".to_string(),
            recfm: recfm.to_string(),
            lrecl,
            blksize,
            extent_count: 1,
            extents: vec![extent],
        };
        self.dscbs.insert(dsname.to_string(), dscb);
        Ok(())
    }

    /// List all datasets on this volume.
    pub fn list_datasets(&self) -> Vec<&DscbFormat1> {
        self.dscbs.values().collect()
    }

    /// Get DSCB for a specific dataset.
    pub fn get_dscb(&self, dsname: &str) -> Option<&DscbFormat1> {
        self.dscbs.get(dsname)
    }

    /// Delete a dataset, freeing its space.
    pub fn delete(&mut self, dsname: &str) -> Result<(), SpaceError> {
        let dscb = self
            .dscbs
            .remove(dsname)
            .ok_or_else(|| SpaceError::DatasetNotFound(dsname.to_string()))?;
        for ext in &dscb.extents {
            self.free_space += ext.size;
            self.free_fragments.push((ext.start, ext.size));
        }
        // Sort fragments by start position for defrag
        self.free_fragments.sort_by_key(|(start, _)| *start);
        self.merge_adjacent_fragments();
        Ok(())
    }

    /// Defragment the volume by merging adjacent free fragments.
    pub fn defrag(&mut self) -> usize {
        let before = self.free_fragments.len();
        self.free_fragments.sort_by_key(|(start, _)| *start);
        self.merge_adjacent_fragments();
        let after = self.free_fragments.len();
        if before > after {
            before - after
        } else {
            0
        }
    }

    /// Number of free space fragments.
    pub fn fragment_count(&self) -> usize {
        self.free_fragments.len()
    }

    fn merge_adjacent_fragments(&mut self) {
        if self.free_fragments.len() <= 1 {
            return;
        }
        let mut merged = Vec::new();
        let mut current = self.free_fragments[0];
        for &(start, size) in &self.free_fragments[1..] {
            if start == current.0 + current.1 {
                // Adjacent — merge
                current.1 += size;
            } else {
                merged.push(current);
                current = (start, size);
            }
        }
        merged.push(current);
        self.free_fragments = merged;
    }
}

// ─────────────────────── Multi-Volume ───────────────────────

/// Multi-volume dataset — a logical dataset spanning multiple volumes.
#[derive(Debug)]
pub struct MultiVolumeDataset {
    /// Dataset name.
    pub dsname: String,
    /// Volumes in order.
    pub volumes: Vec<String>,
    /// Data stored per volume (volume -> bytes).
    volume_data: HashMap<String, Vec<u8>>,
}

impl MultiVolumeDataset {
    /// Create a multi-volume dataset.
    pub fn new(dsname: &str, volumes: Vec<String>) -> Self {
        Self {
            dsname: dsname.to_string(),
            volumes,
            volume_data: HashMap::new(),
        }
    }

    /// Write data, distributing across volumes with the given capacity per volume.
    pub fn write(&mut self, data: &[u8], capacity_per_volume: usize) {
        let mut offset = 0;
        for vol in &self.volumes {
            if offset >= data.len() {
                break;
            }
            let end = (offset + capacity_per_volume).min(data.len());
            self.volume_data
                .insert(vol.clone(), data[offset..end].to_vec());
            offset = end;
        }
    }

    /// Read data sequentially across all volumes.
    pub fn read_sequential(&self) -> Vec<u8> {
        let mut result = Vec::new();
        for vol in &self.volumes {
            if let Some(data) = self.volume_data.get(vol) {
                result.extend_from_slice(data);
            }
        }
        result
    }

    /// Number of volumes with data.
    pub fn active_volumes(&self) -> usize {
        self.volume_data.len()
    }
}

// ─────────────────────── Errors ───────────────────────

/// Space management errors.
#[derive(Debug, Clone)]
pub enum SpaceError {
    /// Maximum extents exceeded (B37 ABEND equivalent).
    MaxExtentsExceeded { dataset: String, max: usize },
    /// No secondary space defined.
    NoSecondarySpace,
    /// Insufficient space on volume.
    InsufficientSpace {
        volser: String,
        requested: u64,
        available: u64,
    },
    /// Dataset not found on volume.
    DatasetNotFound(String),
}

impl std::fmt::Display for SpaceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SpaceError::MaxExtentsExceeded { dataset, max } => {
                write!(f, "B37: max extents ({max}) exceeded for {dataset}")
            }
            SpaceError::NoSecondarySpace => {
                write!(f, "E37: no secondary space defined")
            }
            SpaceError::InsufficientSpace {
                volser,
                requested,
                available,
            } => {
                write!(
                    f,
                    "D37: insufficient space on {volser}: need {requested}, have {available}"
                )
            }
            SpaceError::DatasetNotFound(dsn) => {
                write!(f, "dataset {dsn} not found on volume")
            }
        }
    }
}

impl std::error::Error for SpaceError {}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ─── DFSMS-109.1: Space Allocation Model ───

    #[test]
    fn test_space_allocation_tracks() {
        let space = SpaceAllocation::new(SpaceAllocationUnit::Tracks, 10, 5);
        assert_eq!(space.primary_bytes(), 10 * BYTES_PER_TRACK_3390);
        assert_eq!(space.secondary_bytes(), 5 * BYTES_PER_TRACK_3390);
    }

    #[test]
    fn test_space_allocation_cylinders() {
        let space = SpaceAllocation::new(SpaceAllocationUnit::Cylinders, 1, 1);
        assert_eq!(space.primary_bytes(), BYTES_PER_CYLINDER_3390);
        assert_eq!(space.secondary_bytes(), BYTES_PER_CYLINDER_3390);
    }

    #[test]
    fn test_space_allocation_bytes() {
        let space =
            SpaceAllocation::new(SpaceAllocationUnit::Bytes(27920), 100, 50).with_rlse();
        assert_eq!(space.primary_bytes(), 100 * 27920);
        assert_eq!(space.secondary_bytes(), 50 * 27920);
        assert!(space.rlse);
    }

    #[test]
    fn test_space_allocation_records() {
        let space = SpaceAllocation::new(SpaceAllocationUnit::Records(80), 1000, 500);
        // 1000 records * 80 bytes = 80,000 bytes → ceil(80000/56664) = 2 tracks
        let expected = 2 * BYTES_PER_TRACK_3390;
        assert_eq!(space.primary_bytes(), expected);
    }

    // ─── DFSMS-109.2: Extent Tracking ───

    #[test]
    fn test_extent_primary_allocation() {
        let space = SpaceAllocation::new(SpaceAllocationUnit::Tracks, 10, 5);
        let extents = ExtentList::new(&space, "VOL001");
        assert_eq!(extents.extent_count(), 1);
        assert_eq!(extents.total_allocated(), 10 * BYTES_PER_TRACK_3390);
    }

    #[test]
    fn test_extent_secondary_allocation() {
        let space = SpaceAllocation::new(SpaceAllocationUnit::Tracks, 10, 5);
        let mut extents = ExtentList::new(&space, "VOL001");

        // Fill primary (10 tracks)
        extents
            .write_bytes(10 * BYTES_PER_TRACK_3390, &[])
            .unwrap();
        assert_eq!(extents.extent_count(), 1);

        // Exceed primary — should allocate secondary
        extents.write_bytes(1, &[]).unwrap();
        assert_eq!(extents.extent_count(), 2);
        assert_eq!(
            extents.total_allocated(),
            15 * BYTES_PER_TRACK_3390
        );
    }

    #[test]
    fn test_extent_max_exceeded() {
        let space = SpaceAllocation::new(SpaceAllocationUnit::Tracks, 1, 1);
        let mut extents = ExtentList::new(&space, "VOL001");

        // Fill 16 extents (1 primary + 15 secondary)
        for _ in 0..16 {
            extents
                .write_bytes(BYTES_PER_TRACK_3390, &[])
                .unwrap();
        }
        assert_eq!(extents.extent_count(), 16);

        // 17th extent should fail (B37)
        let result = extents.write_bytes(BYTES_PER_TRACK_3390, &[]);
        assert!(result.is_err());
        match result.unwrap_err() {
            SpaceError::MaxExtentsExceeded { max, .. } => assert_eq!(max, 16),
            other => panic!("expected MaxExtentsExceeded, got: {other:?}"),
        }
    }

    #[test]
    fn test_extent_listing() {
        let space = SpaceAllocation::new(SpaceAllocationUnit::Tracks, 5, 3);
        let mut extents = ExtentList::new(&space, "VOL001");
        extents
            .write_bytes(6 * BYTES_PER_TRACK_3390, &[])
            .unwrap();

        let all = extents.extents();
        assert_eq!(all.len(), 2);
        assert_eq!(all[0].sequence, 0);
        assert_eq!(all[0].size, 5 * BYTES_PER_TRACK_3390);
        assert_eq!(all[1].sequence, 1);
        assert_eq!(all[1].size, 3 * BYTES_PER_TRACK_3390);
    }

    // ─── DFSMS-109.3: VTOC ───

    #[test]
    fn test_vtoc_allocate_and_list() {
        let capacity = 100 * BYTES_PER_CYLINDER_3390;
        let mut vtoc = Vtoc::new("VOL001", capacity);

        let space = SpaceAllocation::new(SpaceAllocationUnit::Tracks, 10, 5);
        vtoc.allocate("MY.DATA.SET", "FB", 80, 27920, &space)
            .unwrap();

        let datasets = vtoc.list_datasets();
        assert_eq!(datasets.len(), 1);
        assert_eq!(datasets[0].dsname, "MY.DATA.SET");
    }

    #[test]
    fn test_vtoc_dscb_contents() {
        let capacity = 100 * BYTES_PER_CYLINDER_3390;
        let mut vtoc = Vtoc::new("VOL001", capacity);

        let space = SpaceAllocation::new(SpaceAllocationUnit::Cylinders, 2, 1);
        vtoc.allocate("SYS1.PARMLIB", "FB", 80, 27920, &space)
            .unwrap();

        let dscb = vtoc.get_dscb("SYS1.PARMLIB").unwrap();
        assert_eq!(dscb.dsname, "SYS1.PARMLIB");
        assert_eq!(dscb.volser, "VOL001");
        assert_eq!(dscb.recfm, "FB");
        assert_eq!(dscb.lrecl, 80);
        assert_eq!(dscb.blksize, 27920);
        assert_eq!(dscb.extent_count, 1);
        assert!(!dscb.creation_date.is_empty());
    }

    #[test]
    fn test_vtoc_insufficient_space() {
        let capacity = BYTES_PER_TRACK_3390; // Only 1 track
        let mut vtoc = Vtoc::new("VOL001", capacity);

        let space = SpaceAllocation::new(SpaceAllocationUnit::Cylinders, 1, 0);
        let result = vtoc.allocate("BIG.FILE", "FB", 80, 27920, &space);
        assert!(result.is_err());
    }

    #[test]
    fn test_vtoc_delete_frees_space() {
        let capacity = 100 * BYTES_PER_CYLINDER_3390;
        let mut vtoc = Vtoc::new("VOL001", capacity);

        let space = SpaceAllocation::new(SpaceAllocationUnit::Cylinders, 5, 0);
        vtoc.allocate("TEMP.FILE", "FB", 80, 27920, &space)
            .unwrap();

        let free_before = vtoc.free_space;
        vtoc.delete("TEMP.FILE").unwrap();
        assert!(vtoc.free_space > free_before);
        assert_eq!(vtoc.free_space, capacity);
    }

    // ─── DFSMS-109.4: Multi-Volume Datasets ───

    #[test]
    fn test_multi_volume_write_read() {
        let mut mvds = MultiVolumeDataset::new(
            "BIG.DATASET",
            vec!["VOL001".into(), "VOL002".into(), "VOL003".into()],
        );

        let data: Vec<u8> = (0..300).map(|i| (i % 256) as u8).collect();
        mvds.write(&data, 100);

        assert_eq!(mvds.active_volumes(), 3);
        let read_back = mvds.read_sequential();
        assert_eq!(read_back, data);
    }

    #[test]
    fn test_multi_volume_partial_fill() {
        let mut mvds = MultiVolumeDataset::new(
            "SMALL.DATASET",
            vec!["VOL001".into(), "VOL002".into()],
        );

        let data = vec![42u8; 50];
        mvds.write(&data, 100); // Fits on first volume

        assert_eq!(mvds.active_volumes(), 1);
        assert_eq!(mvds.read_sequential(), data);
    }

    // ─── DFSMS-109.5: Volume Operations ───

    #[test]
    fn test_defrag_merges_fragments() {
        let capacity = 100 * BYTES_PER_TRACK_3390;
        let mut vtoc = Vtoc::new("VOL001", capacity);

        // Allocate three datasets
        for i in 0..3 {
            let space = SpaceAllocation::new(SpaceAllocationUnit::Tracks, 5, 0);
            vtoc.allocate(&format!("DS{i}"), "FB", 80, 27920, &space)
                .unwrap();
        }

        // Delete the middle one to create fragmentation
        vtoc.delete("DS1").unwrap();

        let frags_before = vtoc.fragment_count();

        // Delete first to create adjacent free space
        vtoc.delete("DS0").unwrap();

        // Defrag should merge adjacent free fragments
        let merged = vtoc.defrag();
        assert!(merged > 0 || vtoc.fragment_count() < frags_before + 1);
    }

    #[test]
    fn test_rlse_releases_unused_space() {
        let space = SpaceAllocation::new(SpaceAllocationUnit::Tracks, 10, 0).with_rlse();
        let mut extents = ExtentList::new(&space, "VOL001");

        // Use only 3 tracks worth
        extents
            .write_bytes(3 * BYTES_PER_TRACK_3390, &[])
            .unwrap();

        let released = extents.release_unused();
        assert_eq!(released, 7 * BYTES_PER_TRACK_3390);
        assert_eq!(extents.total_allocated(), 3 * BYTES_PER_TRACK_3390);
    }

    #[test]
    fn test_rlse_not_set_no_release() {
        let space = SpaceAllocation::new(SpaceAllocationUnit::Tracks, 10, 0);
        let mut extents = ExtentList::new(&space, "VOL001");
        extents
            .write_bytes(3 * BYTES_PER_TRACK_3390, &[])
            .unwrap();

        let released = extents.release_unused();
        assert_eq!(released, 0);
        assert_eq!(extents.total_allocated(), 10 * BYTES_PER_TRACK_3390);
    }

    // ─── DFSMS-109.6: Integration Tests ───

    #[test]
    fn test_all_space_units() {
        let trk = SpaceAllocation::new(SpaceAllocationUnit::Tracks, 10, 5);
        let cyl = SpaceAllocation::new(SpaceAllocationUnit::Cylinders, 1, 1);
        let bytes = SpaceAllocation::new(SpaceAllocationUnit::Bytes(27920), 100, 50);

        assert!(trk.primary_bytes() > 0);
        assert!(cyl.primary_bytes() > 0);
        assert!(bytes.primary_bytes() > 0);
        assert!(cyl.primary_bytes() > trk.primary_bytes()); // 1 cyl > 10 tracks
    }

    #[test]
    fn test_extent_tracking_up_to_16() {
        let space = SpaceAllocation::new(SpaceAllocationUnit::Tracks, 1, 1);
        let mut extents = ExtentList::new(&space, "VOL001");

        // Allocate exactly 16 extents (should succeed)
        for _ in 0..16 {
            extents
                .write_bytes(BYTES_PER_TRACK_3390, &[])
                .unwrap();
        }
        assert_eq!(extents.extent_count(), 16);

        // 17th should fail
        assert!(extents.write_bytes(BYTES_PER_TRACK_3390, &[]).is_err());
    }

    #[test]
    fn test_full_lifecycle() {
        // Allocate volume, create datasets, delete, defrag
        let capacity = 50 * BYTES_PER_CYLINDER_3390;
        let mut vtoc = Vtoc::new("WORK01", capacity);

        // Allocate several datasets
        for i in 0..5 {
            let space = SpaceAllocation::new(SpaceAllocationUnit::Cylinders, 2, 1);
            vtoc.allocate(&format!("USER.DATA{i}"), "VB", 32760, 32760, &space)
                .unwrap();
        }
        assert_eq!(vtoc.list_datasets().len(), 5);

        // Delete 2 datasets
        vtoc.delete("USER.DATA1").unwrap();
        vtoc.delete("USER.DATA3").unwrap();
        assert_eq!(vtoc.list_datasets().len(), 3);

        // Defrag
        vtoc.defrag();

        // Allocate a new dataset in recovered space
        let space = SpaceAllocation::new(SpaceAllocationUnit::Cylinders, 3, 0);
        vtoc.allocate("NEW.DATASET", "FB", 80, 27920, &space)
            .unwrap();
        assert_eq!(vtoc.list_datasets().len(), 4);
    }

    #[test]
    fn test_multi_volume_extent_allocation() {
        let space = SpaceAllocation::new(SpaceAllocationUnit::Tracks, 5, 5);
        let mut extents = ExtentList::new(&space, "VOL001");

        // Fill primary and get a secondary on VOL002
        extents
            .write_bytes(6 * BYTES_PER_TRACK_3390, &["VOL002".to_string()])
            .unwrap();

        assert_eq!(extents.extent_count(), 2);
        assert_eq!(extents.extents()[0].volser, "VOL001");
        assert_eq!(extents.extents()[1].volser, "VOL002");
    }
}
