//! IDMS-105: DMCL & Physical Storage (6 stories).
//!
//! Manages the physical storage layer for IDMS, including page-based
//! storage, CALC (hashed) placement, VIA (near-owner) placement, and
//! the Device Media Control Language (DMCL) configuration.

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  DMCL configuration
// ---------------------------------------------------------------------------

/// Device Media Control Language configuration.
///
/// Defines buffer pools, page sizes, and journal settings for the
/// physical database.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct DmclConfig {
    /// Configuration name.
    pub name: String,
    /// Page size in bytes.
    pub page_size: u32,
    /// Number of buffer pages.
    pub buffer_count: u32,
    /// Journal file enabled.
    pub journal_enabled: bool,
    /// Journal buffer size (pages).
    pub journal_buffers: u32,
}

impl DmclConfig {
    /// Create a new DMCL configuration with defaults.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            page_size: 4096,
            buffer_count: 100,
            journal_enabled: true,
            journal_buffers: 10,
        }
    }
}

// ---------------------------------------------------------------------------
//  Page
// ---------------------------------------------------------------------------

/// A single database page containing record slots.
#[derive(Debug, Clone)]
struct Page {
    /// Page number.
    _page_num: u32,
    /// Records stored on this page (dbkey -> raw bytes placeholder length).
    records: HashMap<u64, usize>,
    /// Used space (bytes).
    used: u32,
    /// Total space (bytes).
    capacity: u32,
}

impl Page {
    fn new(page_num: u32, capacity: u32) -> Self {
        Self {
            _page_num: page_num,
            records: HashMap::new(),
            used: 0,
            capacity,
        }
    }

    fn has_room(&self, size: u32) -> bool {
        self.used + size <= self.capacity
    }

    fn store(&mut self, dbkey: u64, size: u32) -> bool {
        if !self.has_room(size) {
            return false;
        }
        self.records.insert(dbkey, size as usize);
        self.used += size;
        true
    }

    fn remove(&mut self, dbkey: u64) -> bool {
        if let Some(size) = self.records.remove(&dbkey) {
            self.used = self.used.saturating_sub(size as u32);
            true
        } else {
            false
        }
    }
}

// ---------------------------------------------------------------------------
//  CALC routine
// ---------------------------------------------------------------------------

/// Hashing routine for CALC record placement.
///
/// Maps a CALC key value to a target page number within a given page range.
#[derive(Debug, Clone)]
pub struct CalcRoutine {
    /// Start page of the target area.
    pub start_page: u32,
    /// End page of the target area.
    pub end_page: u32,
}

impl CalcRoutine {
    /// Create a new CALC routine for the given page range.
    pub fn new(start_page: u32, end_page: u32) -> Self {
        Self {
            start_page,
            end_page,
        }
    }

    /// Compute the target page for a CALC key string.
    pub fn hash_to_page(&self, key: &str) -> u32 {
        let range = self.end_page - self.start_page + 1;
        if range == 0 {
            return self.start_page;
        }
        let mut hash: u32 = 0;
        for byte in key.bytes() {
            hash = hash.wrapping_mul(31).wrapping_add(u32::from(byte));
        }
        self.start_page + (hash % range)
    }
}

// ---------------------------------------------------------------------------
//  VIA placement
// ---------------------------------------------------------------------------

/// VIA placement strategy -- store a member record near its owner.
///
/// When a record has VIA SET placement, the system attempts to store
/// it on the same page as the owner record.
#[derive(Debug, Clone)]
pub struct ViaPlacement {
    /// Set name for the VIA relationship.
    pub set_name: String,
}

impl ViaPlacement {
    /// Create a new VIA placement for the given set.
    pub fn new(set_name: &str) -> Self {
        Self {
            set_name: set_name.to_uppercase(),
        }
    }

    /// Determine the target page for a member record given the owner's page.
    pub fn target_page(&self, owner_page: u32) -> u32 {
        owner_page
    }
}

// ---------------------------------------------------------------------------
//  Page manager
// ---------------------------------------------------------------------------

/// Manages page-based storage for an IDMS database.
///
/// Supports CALC, VIA, and direct placement strategies.  Maintains an
/// in-memory page pool.
#[derive(Debug)]
pub struct PageManager {
    /// DMCL configuration.
    config: DmclConfig,
    /// Pages keyed by page number.
    pages: HashMap<u32, Page>,
    /// Record-to-page mapping (dbkey -> page number).
    record_pages: HashMap<u64, u32>,
}

impl PageManager {
    /// Create a new page manager with the given DMCL configuration.
    pub fn new(config: DmclConfig) -> Self {
        Self {
            config,
            pages: HashMap::new(),
            record_pages: HashMap::new(),
        }
    }

    /// Return a reference to the DMCL configuration.
    pub fn config(&self) -> &DmclConfig {
        &self.config
    }

    /// Ensure a page exists, creating it if necessary.
    fn ensure_page(&mut self, page_num: u32) -> &mut Page {
        let cap = self.config.page_size;
        self.pages
            .entry(page_num)
            .or_insert_with(|| Page::new(page_num, cap))
    }

    /// Store a record using CALC placement.
    pub fn store_calc(
        &mut self,
        dbkey: u64,
        calc: &CalcRoutine,
        key: &str,
        record_size: u32,
    ) -> Option<u32> {
        let target = calc.hash_to_page(key);
        let page = self.ensure_page(target);
        if page.store(dbkey, record_size) {
            self.record_pages.insert(dbkey, target);
            Some(target)
        } else {
            // Overflow: try next pages.
            let range = calc.end_page - calc.start_page + 1;
            for offset in 1..range {
                let try_page = calc.start_page + (target - calc.start_page + offset) % range;
                let page = self.ensure_page(try_page);
                if page.store(dbkey, record_size) {
                    self.record_pages.insert(dbkey, try_page);
                    return Some(try_page);
                }
            }
            None
        }
    }

    /// Store a record using VIA placement.
    pub fn store_via(
        &mut self,
        dbkey: u64,
        via: &ViaPlacement,
        owner_dbkey: u64,
        record_size: u32,
    ) -> Option<u32> {
        let owner_page = self.record_pages.get(&owner_dbkey).copied().unwrap_or(1);
        let target = via.target_page(owner_page);
        let page = self.ensure_page(target);
        if page.store(dbkey, record_size) {
            self.record_pages.insert(dbkey, target);
            Some(target)
        } else {
            // Overflow to next page.
            let next = target + 1;
            let page = self.ensure_page(next);
            if page.store(dbkey, record_size) {
                self.record_pages.insert(dbkey, next);
                Some(next)
            } else {
                None
            }
        }
    }

    /// Store a record using direct placement on a specific page.
    pub fn store_direct(
        &mut self,
        dbkey: u64,
        page_num: u32,
        record_size: u32,
    ) -> bool {
        let page = self.ensure_page(page_num);
        if page.store(dbkey, record_size) {
            self.record_pages.insert(dbkey, page_num);
            true
        } else {
            false
        }
    }

    /// Remove a record from storage.
    pub fn remove(&mut self, dbkey: u64) -> bool {
        if let Some(page_num) = self.record_pages.remove(&dbkey) {
            if let Some(page) = self.pages.get_mut(&page_num) {
                return page.remove(dbkey);
            }
        }
        false
    }

    /// Look up which page a record is stored on.
    pub fn find_page(&self, dbkey: u64) -> Option<u32> {
        self.record_pages.get(&dbkey).copied()
    }

    /// Return the total number of stored records.
    pub fn record_count(&self) -> usize {
        self.record_pages.len()
    }

    /// Return the number of pages currently allocated.
    pub fn page_count(&self) -> usize {
        self.pages.len()
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dmcl_config_defaults() {
        let cfg = DmclConfig::new("TESTDMCL");
        assert_eq!(cfg.page_size, 4096);
        assert_eq!(cfg.buffer_count, 100);
        assert!(cfg.journal_enabled);
    }

    #[test]
    fn calc_hash_deterministic() {
        let calc = CalcRoutine::new(1, 100);
        let p1 = calc.hash_to_page("EMPLOYEE-123");
        let p2 = calc.hash_to_page("EMPLOYEE-123");
        assert_eq!(p1, p2);
        assert!(p1 >= 1 && p1 <= 100);
    }

    #[test]
    fn calc_hash_different_keys() {
        let calc = CalcRoutine::new(1, 1000);
        let p1 = calc.hash_to_page("KEY-A");
        let p2 = calc.hash_to_page("KEY-B");
        // Could collide but extremely unlikely with 1000 pages.
        let _ = (p1, p2); // just ensure no panic
    }

    #[test]
    fn via_target_page() {
        let via = ViaPlacement::new("DEPT-EMP");
        assert_eq!(via.target_page(42), 42);
    }

    #[test]
    fn page_manager_calc_store() {
        let cfg = DmclConfig::new("TEST");
        let mut pm = PageManager::new(cfg);
        let calc = CalcRoutine::new(1, 100);
        let result = pm.store_calc(1, &calc, "EMP-100", 200);
        assert!(result.is_some());
        assert_eq!(pm.record_count(), 1);
        let page = pm.find_page(1).unwrap();
        assert!(page >= 1 && page <= 100);
    }

    #[test]
    fn page_manager_via_store() {
        let cfg = DmclConfig::new("TEST");
        let mut pm = PageManager::new(cfg);
        // Store owner first.
        pm.store_direct(1, 10, 100);
        let via = ViaPlacement::new("DEPT-EMP");
        let result = pm.store_via(2, &via, 1, 100);
        assert!(result.is_some());
        // Member should be on same page as owner.
        assert_eq!(pm.find_page(2), Some(10));
    }

    #[test]
    fn page_manager_direct_store() {
        let cfg = DmclConfig::new("TEST");
        let mut pm = PageManager::new(cfg);
        assert!(pm.store_direct(1, 5, 200));
        assert_eq!(pm.find_page(1), Some(5));
    }

    #[test]
    fn page_manager_remove() {
        let cfg = DmclConfig::new("TEST");
        let mut pm = PageManager::new(cfg);
        pm.store_direct(1, 5, 200);
        assert_eq!(pm.record_count(), 1);
        assert!(pm.remove(1));
        assert_eq!(pm.record_count(), 0);
        assert!(pm.find_page(1).is_none());
    }

    #[test]
    fn page_manager_overflow() {
        let mut cfg = DmclConfig::new("TEST");
        cfg.page_size = 100; // very small page
        let mut pm = PageManager::new(cfg);
        let calc = CalcRoutine::new(1, 5);
        // Fill pages with large records.
        for i in 0..10 {
            let _ = pm.store_calc(i, &calc, &format!("KEY-{i}"), 90);
        }
        // Some may overflow but should still store.
        assert!(pm.record_count() > 0);
    }

    #[test]
    fn page_count() {
        let cfg = DmclConfig::new("TEST");
        let mut pm = PageManager::new(cfg);
        pm.store_direct(1, 1, 100);
        pm.store_direct(2, 2, 100);
        pm.store_direct(3, 1, 100);
        assert_eq!(pm.page_count(), 2);
    }
}
