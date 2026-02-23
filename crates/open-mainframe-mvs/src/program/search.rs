//! Program search order — STEPLIB, JOBLIB, LNKLST resolution.

use std::collections::HashMap;

/// Program search order for LINK/LOAD/XCTL.
///
/// Searches datasets in order: STEPLIB → JOBLIB → LNKLST.
#[derive(Debug, Clone, Default)]
pub struct ProgramSearchOrder {
    /// STEPLIB datasets (searched first).
    pub steplib: Vec<String>,
    /// JOBLIB datasets (searched second).
    pub joblib: Vec<String>,
    /// LNKLST datasets (searched last, system default).
    pub lnklst: Vec<String>,
    /// Loaded program cache: name → dataset where found.
    cache: HashMap<String, String>,
}

impl ProgramSearchOrder {
    /// Create a new empty search order.
    pub fn new() -> Self {
        Self::default()
    }

    /// Search for a program by name.
    ///
    /// Returns the dataset name where the program was found.
    /// For simulation, we check if any dataset in the search order
    /// has been registered as containing the program.
    pub fn search(&self, name: &str) -> Option<&str> {
        let upper = name.to_uppercase();
        self.cache.get(&upper).map(|s| s.as_str())
    }

    /// Register a program as existing in a dataset.
    ///
    /// This simulates the program being present in a load library.
    pub fn register_program(&mut self, name: &str, dataset: &str) {
        self.cache
            .insert(name.to_uppercase(), dataset.to_uppercase());
    }

    /// Get the full search order as a list of dataset names.
    pub fn search_path(&self) -> Vec<&str> {
        let mut path: Vec<&str> = Vec::new();
        for ds in &self.steplib {
            path.push(ds);
        }
        for ds in &self.joblib {
            path.push(ds);
        }
        for ds in &self.lnklst {
            path.push(ds);
        }
        path
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn search_steplib_first() {
        let mut order = ProgramSearchOrder::new();
        order.steplib.push("MY.STEPLIB".to_string());
        order.joblib.push("MY.JOBLIB".to_string());
        order.lnklst.push("SYS1.LINKLIB".to_string());

        order.register_program("MYPROG", "MY.STEPLIB");
        assert_eq!(order.search("MYPROG"), Some("MY.STEPLIB"));
    }

    #[test]
    fn search_not_found() {
        let order = ProgramSearchOrder::new();
        assert!(order.search("NONEXIST").is_none());
    }

    #[test]
    fn search_path_order() {
        let mut order = ProgramSearchOrder::new();
        order.steplib.push("STEP.LIB".to_string());
        order.joblib.push("JOB.LIB".to_string());
        order.lnklst.push("SYS1.LINKLIB".to_string());

        let path = order.search_path();
        assert_eq!(path, vec!["STEP.LIB", "JOB.LIB", "SYS1.LINKLIB"]);
    }

    #[test]
    fn case_insensitive_search() {
        let mut order = ProgramSearchOrder::new();
        order.register_program("myprog", "MY.LIB");
        assert_eq!(order.search("MYPROG"), Some("MY.LIB"));
    }
}
