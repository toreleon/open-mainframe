//! Subpool management — tracks storage blocks by subpool number.

use std::collections::HashMap;

/// Represents an allocated storage block.
#[derive(Debug, Clone)]
pub struct StorageBlock {
    /// Virtual address (simulated).
    pub address: u64,
    /// Length in bytes.
    pub length: u32,
    /// Subpool number.
    pub subpool: u8,
}

/// Manages storage subpools for a task.
#[derive(Debug)]
pub struct SubpoolManager {
    blocks: HashMap<u64, StorageBlock>,
    next_address: u64,
}

impl SubpoolManager {
    /// Create a new subpool manager.
    pub fn new() -> Self {
        Self {
            blocks: HashMap::new(),
            // Start at a simulated address
            next_address: 0x0001_0000,
        }
    }

    /// Allocate storage from a subpool.
    ///
    /// Returns the address of the allocated block.
    pub fn allocate(&mut self, subpool: u8, length: u32) -> u64 {
        let address = self.next_address;
        // Align to 8 bytes
        let aligned_length = (length as u64 + 7) & !7;
        self.next_address += aligned_length;

        self.blocks.insert(
            address,
            StorageBlock {
                address,
                length,
                subpool,
            },
        );
        address
    }

    /// Free a previously allocated storage block.
    ///
    /// Returns `true` if the block was found and freed.
    pub fn free(&mut self, address: u64) -> bool {
        self.blocks.remove(&address).is_some()
    }

    /// Get information about an allocated block.
    pub fn get_block(&self, address: u64) -> Option<&StorageBlock> {
        self.blocks.get(&address)
    }

    /// Get total allocated bytes across all subpools.
    pub fn total_allocated(&self) -> u64 {
        self.blocks.values().map(|b| b.length as u64).sum()
    }

    /// Get allocated bytes for a specific subpool.
    pub fn subpool_allocated(&self, subpool: u8) -> u64 {
        self.blocks
            .values()
            .filter(|b| b.subpool == subpool)
            .map(|b| b.length as u64)
            .sum()
    }

    /// Free all storage in a specific subpool.
    pub fn free_subpool(&mut self, subpool: u8) {
        self.blocks.retain(|_, b| b.subpool != subpool);
    }

    /// Free all storage.
    pub fn free_all(&mut self) {
        self.blocks.clear();
    }

    /// Number of active allocations.
    pub fn active_count(&self) -> usize {
        self.blocks.len()
    }
}

impl Default for SubpoolManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn allocate_returns_unique_addresses() {
        let mut mgr = SubpoolManager::new();
        let a1 = mgr.allocate(0, 100);
        let a2 = mgr.allocate(0, 200);
        assert_ne!(a1, a2);
    }

    #[test]
    fn free_releases_block() {
        let mut mgr = SubpoolManager::new();
        let addr = mgr.allocate(0, 100);
        assert!(mgr.free(addr));
        assert!(mgr.get_block(addr).is_none());
    }

    #[test]
    fn free_nonexistent_returns_false() {
        let mut mgr = SubpoolManager::new();
        assert!(!mgr.free(0xDEAD));
    }

    #[test]
    fn total_allocated_tracks_bytes() {
        let mut mgr = SubpoolManager::new();
        mgr.allocate(0, 100);
        mgr.allocate(1, 200);
        assert_eq!(mgr.total_allocated(), 300);
    }

    #[test]
    fn subpool_allocated_filters_by_subpool() {
        let mut mgr = SubpoolManager::new();
        mgr.allocate(0, 100);
        mgr.allocate(1, 200);
        mgr.allocate(0, 50);
        assert_eq!(mgr.subpool_allocated(0), 150);
        assert_eq!(mgr.subpool_allocated(1), 200);
    }

    #[test]
    fn free_subpool_removes_all_in_subpool() {
        let mut mgr = SubpoolManager::new();
        mgr.allocate(0, 100);
        mgr.allocate(1, 200);
        mgr.allocate(0, 50);
        mgr.free_subpool(0);
        assert_eq!(mgr.active_count(), 1);
        assert_eq!(mgr.total_allocated(), 200);
    }

    #[test]
    fn free_all_clears_everything() {
        let mut mgr = SubpoolManager::new();
        mgr.allocate(0, 100);
        mgr.allocate(1, 200);
        mgr.free_all();
        assert_eq!(mgr.active_count(), 0);
        assert_eq!(mgr.total_allocated(), 0);
    }

    #[test]
    fn addresses_are_8_byte_aligned() {
        let mut mgr = SubpoolManager::new();
        let a1 = mgr.allocate(0, 1);
        let a2 = mgr.allocate(0, 1);
        assert_eq!((a2 - a1) % 8, 0);
    }
}
