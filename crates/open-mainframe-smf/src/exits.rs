//! SMF Exit Framework.
//!
//! Provides the exit trait and pipeline for filtering, modifying, or routing
//! SMF records:
//! - **IEFU83** — record filtering exit (suppress unwanted records)
//! - **IEFU84** — subsystem identification exit (stamp subsystem ID)
//! - **IFASMFEX** — dynamic exit registration at runtime

use crate::record::SmfRecord;

// ---------------------------------------------------------------------------
//  Exit action
// ---------------------------------------------------------------------------

/// Action returned by an SMF exit after processing a record.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SmfExitAction {
    /// Let the record pass through unchanged.
    Pass,
    /// Suppress (drop) the record.
    Suppress,
    /// The record was modified in-place.
    Modify,
}

// ---------------------------------------------------------------------------
//  Exit trait
// ---------------------------------------------------------------------------

/// Trait for SMF exit programs.
///
/// Exits are invoked during record writing to filter, modify, or annotate
/// records before they are committed to the recording destination.
pub trait SmfExit: Send + Sync {
    /// Process an SMF record. May mutate the record and return an action.
    fn process(&self, record: &mut SmfRecord) -> SmfExitAction;

    /// Name of this exit (e.g., "IEFU83").
    fn name(&self) -> &str;
}

// ---------------------------------------------------------------------------
//  IEFU83 — Record Filtering Exit
// ---------------------------------------------------------------------------

/// IEFU83 exit — filters SMF records based on user-defined criteria.
pub struct Iefu83Exit {
    /// Predicate: returns true to suppress the record.
    suppress_fn: Box<dyn Fn(&SmfRecord) -> bool + Send + Sync>,
}

impl Iefu83Exit {
    /// Create a new IEFU83 exit with a suppression predicate.
    pub fn new(suppress_fn: Box<dyn Fn(&SmfRecord) -> bool + Send + Sync>) -> Self {
        Self { suppress_fn }
    }
}

impl SmfExit for Iefu83Exit {
    fn process(&self, record: &mut SmfRecord) -> SmfExitAction {
        if (self.suppress_fn)(record) {
            SmfExitAction::Suppress
        } else {
            SmfExitAction::Pass
        }
    }

    fn name(&self) -> &str {
        "IEFU83"
    }
}

impl std::fmt::Debug for Iefu83Exit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Iefu83Exit").finish()
    }
}

// ---------------------------------------------------------------------------
//  IEFU84 — Subsystem Identification Exit
// ---------------------------------------------------------------------------

/// IEFU84 exit — stamps subsystem identification into records.
#[derive(Debug)]
pub struct Iefu84Exit {
    /// Subsystem ID to stamp into every record.
    subsystem_id: String,
}

impl Iefu84Exit {
    /// Create a new IEFU84 exit with the given subsystem ID.
    pub fn new(subsystem_id: impl Into<String>) -> Self {
        Self {
            subsystem_id: subsystem_id.into(),
        }
    }
}

impl SmfExit for Iefu84Exit {
    fn process(&self, record: &mut SmfRecord) -> SmfExitAction {
        record.header.subsystem_id = self.subsystem_id.clone();
        SmfExitAction::Modify
    }

    fn name(&self) -> &str {
        "IEFU84"
    }
}

// ---------------------------------------------------------------------------
//  Dynamic exit registration (IFASMFEX)
// ---------------------------------------------------------------------------

/// A registered exit entry with its applicable type filter.
struct ExitEntry {
    exit: Box<dyn SmfExit>,
    /// Record types this exit applies to. Empty = all types.
    types: Vec<u8>,
}

/// Dynamic exit registry (IFASMFEX equivalent).
///
/// Manages a pipeline of exits that are invoked in registration order.
pub struct SmfExitRegistry {
    entries: Vec<ExitEntry>,
}

impl std::fmt::Debug for SmfExitRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SmfExitRegistry")
            .field("exit_count", &self.entries.len())
            .finish()
    }
}

impl Default for SmfExitRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl SmfExitRegistry {
    /// Create a new empty exit registry.
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    /// Register an exit for specific record types (IFASMFEX).
    /// If `types` is empty, the exit applies to all record types.
    pub fn register(&mut self, exit: Box<dyn SmfExit>, types: Vec<u8>) {
        self.entries.push(ExitEntry { exit, types });
    }

    /// Deregister all exits with the given name.
    pub fn deregister(&mut self, name: &str) {
        self.entries.retain(|e| e.exit.name() != name);
    }

    /// Process a record through the exit pipeline.
    /// Returns the final action. If any exit returns Suppress, the record is dropped.
    pub fn process(&self, record: &mut SmfRecord) -> SmfExitAction {
        let record_type = record.header.record_type;
        let mut final_action = SmfExitAction::Pass;

        for entry in &self.entries {
            // Check if this exit applies to this record type.
            if !entry.types.is_empty() && !entry.types.contains(&record_type) {
                continue;
            }

            let action = entry.exit.process(record);
            match action {
                SmfExitAction::Suppress => return SmfExitAction::Suppress,
                SmfExitAction::Modify => final_action = SmfExitAction::Modify,
                SmfExitAction::Pass => {}
            }
        }

        final_action
    }

    /// Number of registered exits.
    pub fn exit_count(&self) -> usize {
        self.entries.len()
    }

    /// Get names of all registered exits.
    pub fn exit_names(&self) -> Vec<&str> {
        self.entries.iter().map(|e| e.exit.name()).collect()
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_iefu83_pass() {
        let exit = Iefu83Exit::new(Box::new(|_| false));
        let mut rec = SmfRecord::new(30, vec![0; 10]);
        assert_eq!(exit.process(&mut rec), SmfExitAction::Pass);
    }

    #[test]
    fn test_iefu83_suppress() {
        // Suppress Type 14 records.
        let exit = Iefu83Exit::new(Box::new(|r| r.header.record_type == 14));
        let mut rec14 = SmfRecord::new(14, vec![0; 10]);
        assert_eq!(exit.process(&mut rec14), SmfExitAction::Suppress);

        let mut rec30 = SmfRecord::new(30, vec![0; 10]);
        assert_eq!(exit.process(&mut rec30), SmfExitAction::Pass);
    }

    #[test]
    fn test_iefu83_name() {
        let exit = Iefu83Exit::new(Box::new(|_| false));
        assert_eq!(exit.name(), "IEFU83");
    }

    #[test]
    fn test_iefu84_stamps_subsystem() {
        let exit = Iefu84Exit::new("PROD");
        let mut rec = SmfRecord::new(30, vec![0; 10]);
        let action = exit.process(&mut rec);
        assert_eq!(action, SmfExitAction::Modify);
        assert_eq!(rec.header.subsystem_id, "PROD");
    }

    #[test]
    fn test_iefu84_name() {
        let exit = Iefu84Exit::new("DB2A");
        assert_eq!(exit.name(), "IEFU84");
    }

    #[test]
    fn test_registry_register_and_process() {
        let mut registry = SmfExitRegistry::new();
        registry.register(
            Box::new(Iefu84Exit::new("PROD")),
            vec![30, 80],
        );
        assert_eq!(registry.exit_count(), 1);

        let mut rec30 = SmfRecord::new(30, vec![0; 10]);
        let action = registry.process(&mut rec30);
        assert_eq!(action, SmfExitAction::Modify);
        assert_eq!(rec30.header.subsystem_id, "PROD");

        // Type 4 should not be affected.
        let mut rec4 = SmfRecord::new(4, vec![0; 10]);
        let action = registry.process(&mut rec4);
        assert_eq!(action, SmfExitAction::Pass);
        assert_eq!(rec4.header.subsystem_id, "");
    }

    #[test]
    fn test_registry_pipeline_order() {
        let mut registry = SmfExitRegistry::new();
        // First exit: stamp subsystem.
        registry.register(Box::new(Iefu84Exit::new("SUB1")), vec![]);
        // Second exit: suppress Type 14.
        registry.register(
            Box::new(Iefu83Exit::new(Box::new(|r| r.header.record_type == 14))),
            vec![],
        );

        // Type 30: modified by first, passed by second.
        let mut rec30 = SmfRecord::new(30, vec![0; 10]);
        let action = registry.process(&mut rec30);
        assert_eq!(action, SmfExitAction::Modify);
        assert_eq!(rec30.header.subsystem_id, "SUB1");

        // Type 14: modified by first, then suppressed by second.
        let mut rec14 = SmfRecord::new(14, vec![0; 10]);
        let action = registry.process(&mut rec14);
        assert_eq!(action, SmfExitAction::Suppress);
    }

    #[test]
    fn test_registry_deregister() {
        let mut registry = SmfExitRegistry::new();
        registry.register(Box::new(Iefu84Exit::new("PROD")), vec![]);
        registry.register(
            Box::new(Iefu83Exit::new(Box::new(|_| false))),
            vec![],
        );
        assert_eq!(registry.exit_count(), 2);

        registry.deregister("IEFU84");
        assert_eq!(registry.exit_count(), 1);
        assert_eq!(registry.exit_names(), vec!["IEFU83"]);
    }

    #[test]
    fn test_registry_all_types_filter() {
        let mut registry = SmfExitRegistry::new();
        // Register for all types (empty vec).
        registry.register(Box::new(Iefu84Exit::new("ALL")), vec![]);

        let mut rec = SmfRecord::new(200, vec![0; 5]);
        let action = registry.process(&mut rec);
        assert_eq!(action, SmfExitAction::Modify);
        assert_eq!(rec.header.subsystem_id, "ALL");
    }

    #[test]
    fn test_registry_empty() {
        let registry = SmfExitRegistry::new();
        let mut rec = SmfRecord::new(30, vec![0; 5]);
        assert_eq!(registry.process(&mut rec), SmfExitAction::Pass);
        assert_eq!(registry.exit_count(), 0);
    }

    #[test]
    fn test_suppress_short_circuits() {
        let mut registry = SmfExitRegistry::new();
        // First exit suppresses everything.
        registry.register(
            Box::new(Iefu83Exit::new(Box::new(|_| true))),
            vec![],
        );
        // Second exit would modify, but should not run.
        registry.register(Box::new(Iefu84Exit::new("SHOULD_NOT_SET")), vec![]);

        let mut rec = SmfRecord::new(30, vec![0; 5]);
        let action = registry.process(&mut rec);
        assert_eq!(action, SmfExitAction::Suppress);
        // Subsystem was not set because suppress short-circuited.
        assert_eq!(rec.header.subsystem_id, "");
    }
}
