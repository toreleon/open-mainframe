//! LOAD/DELETE — module loading and lifecycle management.

use std::collections::HashMap;

use crate::error::{MvsError, Result};
use crate::program::search::ProgramSearchOrder;

/// Represents a loaded program module.
#[derive(Debug, Clone)]
pub struct LoadedModule {
    /// Program name.
    pub name: String,
    /// Entry point address (simulated).
    pub entry_point: u64,
    /// Module length in bytes.
    pub length: u32,
    /// Use count (number of LOAD requests).
    pub use_count: u32,
    /// Dataset where the module was found.
    pub dataset: String,
}

/// Manages loaded modules and their lifecycles.
#[derive(Debug, Default)]
pub struct ModuleManager {
    modules: HashMap<String, LoadedModule>,
    next_entry_point: u64,
    search_order: ProgramSearchOrder,
}

impl ModuleManager {
    /// Create a new module manager with the given search order.
    pub fn new(search_order: ProgramSearchOrder) -> Self {
        Self {
            modules: HashMap::new(),
            next_entry_point: 0x0080_0000,
            search_order,
        }
    }

    /// Get a reference to the search order.
    pub fn search_order(&self) -> &ProgramSearchOrder {
        &self.search_order
    }

    /// Get a mutable reference to the search order.
    pub fn search_order_mut(&mut self) -> &mut ProgramSearchOrder {
        &mut self.search_order
    }

    /// LOAD — load a module into storage.
    ///
    /// If already loaded, increments the use count and returns the existing entry.
    pub fn load(&mut self, name: &str) -> Result<&LoadedModule> {
        let upper = name.to_uppercase();

        if let Some(module) = self.modules.get_mut(&upper) {
            module.use_count += 1;
            return Ok(self.modules.get(&upper).unwrap());
        }

        // Search for the program
        let dataset = self
            .search_order
            .search(&upper)
            .ok_or_else(|| MvsError::ProgramNotFound {
                name: upper.clone(),
            })?
            .to_string();

        let entry_point = self.next_entry_point;
        self.next_entry_point += 0x1000; // 4K alignment

        let module = LoadedModule {
            name: upper.clone(),
            entry_point,
            length: 4096, // Simulated module size
            use_count: 1,
            dataset,
        };
        self.modules.insert(upper.clone(), module);
        Ok(self.modules.get(&upper).unwrap())
    }

    /// DELETE — decrement use count or remove module from storage.
    ///
    /// Returns `true` if the module was fully removed.
    pub fn delete(&mut self, name: &str) -> Result<bool> {
        let upper = name.to_uppercase();

        let module = self
            .modules
            .get_mut(&upper)
            .ok_or_else(|| MvsError::ProgramNotFound {
                name: upper.clone(),
            })?;

        module.use_count -= 1;
        if module.use_count == 0 {
            self.modules.remove(&upper);
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Look up a loaded module by name.
    pub fn get(&self, name: &str) -> Option<&LoadedModule> {
        self.modules.get(&name.to_uppercase())
    }

    /// Number of loaded modules.
    pub fn loaded_count(&self) -> usize {
        self.modules.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_manager() -> ModuleManager {
        let mut search = ProgramSearchOrder::new();
        search.register_program("IEFBR14", "SYS1.LINKLIB");
        search.register_program("SORT", "SYS1.LINKLIB");
        ModuleManager::new(search)
    }

    #[test]
    fn load_returns_entry_point() {
        let mut mgr = make_manager();
        let module = mgr.load("IEFBR14").unwrap();
        assert!(module.entry_point > 0);
        assert_eq!(module.use_count, 1);
        assert_eq!(module.dataset, "SYS1.LINKLIB");
    }

    #[test]
    fn load_increments_use_count() {
        let mut mgr = make_manager();
        mgr.load("IEFBR14").unwrap();
        let module = mgr.load("IEFBR14").unwrap();
        assert_eq!(module.use_count, 2);
    }

    #[test]
    fn load_same_entry_point_on_reload() {
        let mut mgr = make_manager();
        let ep1 = mgr.load("IEFBR14").unwrap().entry_point;
        let ep2 = mgr.load("IEFBR14").unwrap().entry_point;
        assert_eq!(ep1, ep2);
    }

    #[test]
    fn delete_decrements_use_count() {
        let mut mgr = make_manager();
        mgr.load("IEFBR14").unwrap();
        mgr.load("IEFBR14").unwrap();
        let removed = mgr.delete("IEFBR14").unwrap();
        assert!(!removed);
        assert_eq!(mgr.get("IEFBR14").unwrap().use_count, 1);
    }

    #[test]
    fn delete_removes_when_count_zero() {
        let mut mgr = make_manager();
        mgr.load("IEFBR14").unwrap();
        let removed = mgr.delete("IEFBR14").unwrap();
        assert!(removed);
        assert!(mgr.get("IEFBR14").is_none());
    }

    #[test]
    fn load_not_found_returns_error() {
        let mut mgr = make_manager();
        let err = mgr.load("NONEXIST").unwrap_err();
        assert!(matches!(err, MvsError::ProgramNotFound { .. }));
    }

    #[test]
    fn delete_not_loaded_returns_error() {
        let mut mgr = make_manager();
        let err = mgr.delete("IEFBR14").unwrap_err();
        assert!(matches!(err, MvsError::ProgramNotFound { .. }));
    }
}
