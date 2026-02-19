//! LE Storage Management — heap allocation, deallocation, reallocation, and user heaps.
//!
//! Implements CEEGTST/CEEFRST/CEECZST/CEECRHP/CEEDSHP callable services.
//! Provides a default heap (heap_id=0) and user-created heaps.

use std::collections::BTreeMap;

/// Heap allocation metadata.
#[derive(Debug, Clone)]
struct Allocation {
    /// Offset within the heap's data vector.
    offset: usize,
    /// Size of this allocation.
    size: usize,
    /// Which heap this belongs to.
    heap_id: u32,
}

/// A single heap instance (default or user-created).
#[derive(Debug)]
#[allow(dead_code)]
struct Heap {
    /// Heap identifier.
    id: u32,
    /// Backing storage (simplified — one contiguous buffer).
    data: Vec<u8>,
    /// Total allocated bytes.
    allocated: usize,
    /// Options used when creating.
    options: HeapOptions,
}

/// Options for heap creation.
#[derive(Debug, Clone)]
pub struct HeapOptions {
    /// Initial size in bytes.
    pub initial_size: usize,
    /// Increment size when more space is needed.
    pub increment: usize,
}

impl Default for HeapOptions {
    fn default() -> Self {
        Self {
            initial_size: 65536,
            increment: 32768,
        }
    }
}

/// Handle returned from CEEGTST — identifies an allocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HeapAddress(u64);

impl HeapAddress {
    /// Get the raw address value.
    pub fn raw(&self) -> u64 {
        self.0
    }
}

/// LE heap manager — manages the default heap and user-created heaps.
#[derive(Debug)]
pub struct HeapManager {
    /// All heaps: heap_id → heap.
    heaps: BTreeMap<u32, Heap>,
    /// All allocations: address → metadata.
    allocations: BTreeMap<u64, Allocation>,
    /// Next allocation address (simulated).
    next_address: u64,
    /// Next user heap ID.
    next_heap_id: u32,
}

impl HeapManager {
    /// Create a new heap manager with the default heap (id=0).
    pub fn new() -> Self {
        let default_heap = Heap {
            id: 0,
            data: Vec::with_capacity(65536),
            allocated: 0,
            options: HeapOptions::default(),
        };
        let mut heaps = BTreeMap::new();
        heaps.insert(0, default_heap);

        Self {
            heaps,
            allocations: BTreeMap::new(),
            next_address: 0x1000, // start above zero
            next_heap_id: 1,
        }
    }

    // ─────── CEEGTST — Get storage ───────

    /// CEEGTST — allocate storage from a heap.
    ///
    /// `heap_id` = 0 for the default heap, or a user heap ID.
    /// Returns the allocated address or an error string.
    pub fn ceegtst(&mut self, heap_id: u32, size: usize) -> Result<HeapAddress, String> {
        if size == 0 {
            return Err("allocation size must be greater than 0".to_string());
        }

        let heap = self
            .heaps
            .get_mut(&heap_id)
            .ok_or_else(|| format!("heap {} does not exist", heap_id))?;

        // Ensure capacity.
        let needed = heap.allocated + size;
        if needed > heap.data.len() {
            let grow = std::cmp::max(heap.options.increment, needed - heap.data.len());
            heap.data.resize(heap.data.len() + grow, 0);
        }

        let offset = heap.allocated;
        heap.allocated += size;

        let addr = self.next_address;
        self.next_address += size as u64;

        self.allocations.insert(
            addr,
            Allocation {
                offset,
                size,
                heap_id,
            },
        );

        Ok(HeapAddress(addr))
    }

    // ─────── CEEFRST — Free storage ───────

    /// CEEFRST — free storage previously allocated with CEEGTST.
    pub fn ceefrst(&mut self, address: HeapAddress) -> Result<(), String> {
        self.allocations
            .remove(&address.0)
            .ok_or_else(|| format!("address 0x{:x} not found", address.0))?;
        Ok(())
    }

    // ─────── CEECZST — Reallocate storage ───────

    /// CEECZST — reallocate storage to a new size.
    ///
    /// Data is preserved up to the minimum of old and new sizes.
    /// Returns the new address (may differ from old).
    pub fn ceeczst(
        &mut self,
        address: HeapAddress,
        new_size: usize,
    ) -> Result<HeapAddress, String> {
        let old_alloc = self
            .allocations
            .get(&address.0)
            .ok_or_else(|| format!("address 0x{:x} not found", address.0))?
            .clone();

        let heap_id = old_alloc.heap_id;

        // Read old data.
        let old_data = {
            let heap = self.heaps.get(&heap_id).unwrap();
            let end = std::cmp::min(old_alloc.offset + old_alloc.size, heap.data.len());
            heap.data[old_alloc.offset..end].to_vec()
        };

        // Allocate new block.
        let new_addr = self.ceegtst(heap_id, new_size)?;

        // Copy preserved data.
        let copy_len = std::cmp::min(old_data.len(), new_size);
        if copy_len > 0 {
            let new_alloc = self.allocations.get(&new_addr.0).unwrap();
            let heap = self.heaps.get_mut(&heap_id).unwrap();
            let dst_start = new_alloc.offset;
            heap.data[dst_start..dst_start + copy_len].copy_from_slice(&old_data[..copy_len]);
        }

        // Free old block.
        self.allocations.remove(&address.0);

        Ok(new_addr)
    }

    // ─────── CEECRHP — Create user heap ───────

    /// CEECRHP — create a new user-defined heap.
    ///
    /// Returns the new heap ID.
    pub fn ceecrhp(&mut self, options: HeapOptions) -> u32 {
        let id = self.next_heap_id;
        self.next_heap_id += 1;

        let heap = Heap {
            id,
            data: Vec::with_capacity(options.initial_size),
            allocated: 0,
            options,
        };
        self.heaps.insert(id, heap);
        id
    }

    // ─────── CEEDSHP — Destroy user heap ───────

    /// CEEDSHP — destroy a user heap and release all its storage.
    ///
    /// Cannot destroy the default heap (id=0).
    pub fn ceedshp(&mut self, heap_id: u32) -> Result<(), String> {
        if heap_id == 0 {
            return Err("cannot destroy the default heap".to_string());
        }
        if self.heaps.remove(&heap_id).is_none() {
            return Err(format!("heap {} does not exist", heap_id));
        }
        // Remove all allocations belonging to this heap.
        self.allocations.retain(|_, a| a.heap_id != heap_id);
        Ok(())
    }

    // ─────── Query methods ───────

    /// Get the number of active allocations.
    pub fn allocation_count(&self) -> usize {
        self.allocations.len()
    }

    /// Get the number of heaps (including default).
    pub fn heap_count(&self) -> usize {
        self.heaps.len()
    }

    /// Check if an address is currently allocated.
    pub fn is_allocated(&self, address: HeapAddress) -> bool {
        self.allocations.contains_key(&address.0)
    }

    /// Get the size of an allocation.
    pub fn allocation_size(&self, address: HeapAddress) -> Option<usize> {
        self.allocations.get(&address.0).map(|a| a.size)
    }

    /// Write data to an allocation (for testing).
    pub fn write(&mut self, address: HeapAddress, data: &[u8]) -> Result<(), String> {
        let alloc = self
            .allocations
            .get(&address.0)
            .ok_or_else(|| format!("address 0x{:x} not found", address.0))?
            .clone();

        if data.len() > alloc.size {
            return Err("data exceeds allocation size".to_string());
        }

        let heap = self.heaps.get_mut(&alloc.heap_id).unwrap();
        heap.data[alloc.offset..alloc.offset + data.len()].copy_from_slice(data);
        Ok(())
    }

    /// Read data from an allocation (for testing).
    pub fn read(&self, address: HeapAddress, len: usize) -> Result<Vec<u8>, String> {
        let alloc = self
            .allocations
            .get(&address.0)
            .ok_or_else(|| format!("address 0x{:x} not found", address.0))?;

        let read_len = std::cmp::min(len, alloc.size);
        let heap = self.heaps.get(&alloc.heap_id).unwrap();
        Ok(heap.data[alloc.offset..alloc.offset + read_len].to_vec())
    }
}

impl Default for HeapManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ─────── Story LE102.1: Heap Allocation Services ───────

    #[test]
    fn test_ceegtst_allocate_default_heap() {
        let mut mgr = HeapManager::new();

        let addr = mgr.ceegtst(0, 1024).unwrap();
        assert!(addr.raw() > 0);
        assert_eq!(mgr.allocation_count(), 1);
        assert_eq!(mgr.allocation_size(addr), Some(1024));
    }

    #[test]
    fn test_ceegtst_zero_size_fails() {
        let mut mgr = HeapManager::new();
        assert!(mgr.ceegtst(0, 0).is_err());
    }

    #[test]
    fn test_ceegtst_invalid_heap_fails() {
        let mut mgr = HeapManager::new();
        assert!(mgr.ceegtst(999, 100).is_err());
    }

    #[test]
    fn test_ceefrst_free_storage() {
        let mut mgr = HeapManager::new();
        let addr = mgr.ceegtst(0, 512).unwrap();

        mgr.ceefrst(addr).unwrap();
        assert_eq!(mgr.allocation_count(), 0);
        assert!(!mgr.is_allocated(addr));
    }

    #[test]
    fn test_ceefrst_invalid_address_fails() {
        let mut mgr = HeapManager::new();
        assert!(mgr.ceefrst(HeapAddress(0xDEAD)).is_err());
    }

    #[test]
    fn test_ceeczst_reallocate_preserves_data() {
        let mut mgr = HeapManager::new();
        let addr = mgr.ceegtst(0, 8).unwrap();

        // Write data.
        mgr.write(addr, &[1, 2, 3, 4, 5, 6, 7, 8]).unwrap();

        // Reallocate to larger size.
        let new_addr = mgr.ceeczst(addr, 16).unwrap();

        // Old address should be freed.
        assert!(!mgr.is_allocated(addr));
        assert!(mgr.is_allocated(new_addr));
        assert_eq!(mgr.allocation_size(new_addr), Some(16));

        // Data should be preserved.
        let data = mgr.read(new_addr, 8).unwrap();
        assert_eq!(data, vec![1, 2, 3, 4, 5, 6, 7, 8]);
    }

    #[test]
    fn test_ceeczst_shrink() {
        let mut mgr = HeapManager::new();
        let addr = mgr.ceegtst(0, 16).unwrap();
        mgr.write(addr, &[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]).unwrap();

        let new_addr = mgr.ceeczst(addr, 4).unwrap();
        let data = mgr.read(new_addr, 4).unwrap();
        assert_eq!(data, vec![1, 2, 3, 4]);
    }

    #[test]
    fn test_multiple_allocations() {
        let mut mgr = HeapManager::new();
        let a1 = mgr.ceegtst(0, 100).unwrap();
        let a2 = mgr.ceegtst(0, 200).unwrap();
        let a3 = mgr.ceegtst(0, 300).unwrap();

        assert_eq!(mgr.allocation_count(), 3);
        assert_ne!(a1.raw(), a2.raw());
        assert_ne!(a2.raw(), a3.raw());

        mgr.ceefrst(a2).unwrap();
        assert_eq!(mgr.allocation_count(), 2);
        assert!(mgr.is_allocated(a1));
        assert!(!mgr.is_allocated(a2));
        assert!(mgr.is_allocated(a3));
    }

    // ─────── Story LE102.2: User Heaps ───────

    #[test]
    fn test_ceecrhp_create_user_heap() {
        let mut mgr = HeapManager::new();

        let heap_id = mgr.ceecrhp(HeapOptions {
            initial_size: 4096,
            increment: 2048,
        });
        assert!(heap_id > 0); // Not the default heap.
        assert_eq!(mgr.heap_count(), 2); // default + user
    }

    #[test]
    fn test_allocate_from_user_heap() {
        let mut mgr = HeapManager::new();
        let heap_id = mgr.ceecrhp(HeapOptions::default());

        let addr = mgr.ceegtst(heap_id, 256).unwrap();
        assert!(mgr.is_allocated(addr));
    }

    #[test]
    fn test_ceedshp_destroy_user_heap() {
        let mut mgr = HeapManager::new();
        let heap_id = mgr.ceecrhp(HeapOptions::default());

        // Allocate some storage.
        let a1 = mgr.ceegtst(heap_id, 100).unwrap();
        let a2 = mgr.ceegtst(heap_id, 200).unwrap();
        assert_eq!(mgr.allocation_count(), 2);

        // Also allocate on default heap.
        let a3 = mgr.ceegtst(0, 50).unwrap();

        // Destroy user heap.
        mgr.ceedshp(heap_id).unwrap();

        // User heap allocations gone, default heap allocation preserved.
        assert!(!mgr.is_allocated(a1));
        assert!(!mgr.is_allocated(a2));
        assert!(mgr.is_allocated(a3));
        assert_eq!(mgr.allocation_count(), 1);
        assert_eq!(mgr.heap_count(), 1);
    }

    #[test]
    fn test_ceedshp_cannot_destroy_default() {
        let mut mgr = HeapManager::new();
        assert!(mgr.ceedshp(0).is_err());
    }

    #[test]
    fn test_ceedshp_invalid_heap_fails() {
        let mut mgr = HeapManager::new();
        assert!(mgr.ceedshp(999).is_err());
    }
}
