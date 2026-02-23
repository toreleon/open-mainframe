//! # WLM Service Definition Persistence
//!
//! Provides a policy store that persists service definitions as JSON,
//! supports loading/restoring policies, and atomic policy activation
//! via `VARY WLM,POLICY=`.

use std::collections::HashMap;

use crate::policy::ServiceDefinition;
use crate::service::WlmError;

// ─────────────────────── Policy Store ───────────────────────

/// A store for persisted WLM service definitions.
///
/// Holds multiple named policies and tracks which one is currently active.
/// Corresponds to the WLM couple dataset on z/OS.
#[derive(Debug)]
pub struct PolicyStore {
    /// Persisted policies by name (serialized JSON).
    definitions: HashMap<String, String>,
    /// Name of the currently active policy.
    active_policy: Option<String>,
    /// The active (deserialized) service definition.
    active_definition: Option<ServiceDefinition>,
}

impl PolicyStore {
    /// Create a new, empty policy store.
    pub fn new() -> Self {
        Self {
            definitions: HashMap::new(),
            active_policy: None,
            active_definition: None,
        }
    }

    /// Store a service definition, serializing all classes, rules, and groups.
    pub fn store(&mut self, def: &ServiceDefinition) -> Result<(), WlmError> {
        let json = def.to_json()?;
        self.definitions
            .insert(def.policy.name.to_uppercase(), json);
        Ok(())
    }

    /// Load a persisted service definition by name.
    pub fn load(&self, name: &str) -> Result<ServiceDefinition, WlmError> {
        let upper = name.to_uppercase();
        match self.definitions.get(&upper) {
            Some(json) => ServiceDefinition::from_json(json),
            None => Err(WlmError::Other(format!("Policy not found: {upper}"))),
        }
    }

    /// List all stored policy names.
    pub fn list(&self) -> Vec<&str> {
        self.definitions.keys().map(|s| s.as_str()).collect()
    }

    /// Remove a stored policy.
    pub fn remove(&mut self, name: &str) -> Result<(), WlmError> {
        let upper = name.to_uppercase();
        if self.active_policy.as_deref() == Some(&upper) {
            return Err(WlmError::Other(format!(
                "Cannot remove active policy: {upper}"
            )));
        }
        match self.definitions.remove(&upper) {
            Some(_) => Ok(()),
            None => Err(WlmError::Other(format!("Policy not found: {upper}"))),
        }
    }

    /// Check if a policy exists.
    pub fn contains(&self, name: &str) -> bool {
        self.definitions.contains_key(&name.to_uppercase())
    }

    /// Number of stored policies.
    pub fn count(&self) -> usize {
        self.definitions.len()
    }

    /// Get the name of the currently active policy.
    pub fn active_policy_name(&self) -> Option<&str> {
        self.active_policy.as_deref()
    }

    /// Get a reference to the active service definition.
    pub fn active_definition(&self) -> Option<&ServiceDefinition> {
        self.active_definition.as_ref()
    }

    // ─── VARY WLM,POLICY= ───

    /// Activate a policy atomically, replacing the current active policy.
    ///
    /// Corresponds to `VARY WLM,POLICY=<name>`.
    ///
    /// The new policy is deserialized, validated, and if valid, atomically
    /// replaces the current active policy. If validation fails, the
    /// current policy remains unchanged.
    pub fn activate(&mut self, name: &str) -> Result<ActivationResult, WlmError> {
        let upper = name.to_uppercase();

        // Load and deserialize.
        let def = self.load(&upper)?;

        // Validate the policy.
        if let Err(errors) = def.validate() {
            return Err(WlmError::Other(format!(
                "Policy validation failed: {}",
                errors.join("; ")
            )));
        }

        let previous = self.active_policy.clone();

        // Atomic swap.
        self.active_policy = Some(upper.clone());
        self.active_definition = Some(def);

        Ok(ActivationResult {
            policy_name: upper,
            previous_policy: previous,
            class_count: self
                .active_definition
                .as_ref()
                .map(|d| d.policy.class_count())
                .unwrap_or(0),
        })
    }

    /// Deactivate the current policy (enter goal mode = COMPAT).
    pub fn deactivate(&mut self) -> Option<String> {
        let prev = self.active_policy.take();
        self.active_definition = None;
        prev
    }

    /// Export all stored policies as a single JSON document.
    pub fn export_all(&self) -> Result<String, WlmError> {
        let all: HashMap<&str, &str> = self
            .definitions
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_str()))
            .collect();
        serde_json::to_string_pretty(&all)
            .map_err(|e| WlmError::Other(format!("Export error: {e}")))
    }

    /// Import policies from an exported JSON document.
    pub fn import_all(&mut self, json: &str) -> Result<usize, WlmError> {
        let all: HashMap<String, String> = serde_json::from_str(json)
            .map_err(|e| WlmError::Other(format!("Import error: {e}")))?;
        let count = all.len();
        for (name, policy_json) in all {
            // Validate each policy can be deserialized.
            ServiceDefinition::from_json(&policy_json)?;
            self.definitions.insert(name.to_uppercase(), policy_json);
        }
        Ok(count)
    }
}

impl Default for PolicyStore {
    fn default() -> Self {
        Self::new()
    }
}

// ─────────────────────── Activation Result ───────────────────────

/// Result of a policy activation.
#[derive(Debug, Clone)]
pub struct ActivationResult {
    /// Name of the newly active policy.
    pub policy_name: String,
    /// Name of the previously active policy, if any.
    pub previous_policy: Option<String>,
    /// Number of service classes in the new policy.
    pub class_count: usize,
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::policy::{CpuCapType, ReportClass, ResourceGroup, Workload};
    use crate::service::{GoalType, Importance, ServiceClass, ServicePolicy};

    fn make_definition(name: &str) -> ServiceDefinition {
        let mut policy = ServicePolicy::new(name);

        policy
            .define_class(ServiceClass::new(
                "ONLINE",
                GoalType::ResponseTime {
                    target_seconds: 0.2,
                    percentile: 95.0,
                },
                Importance(1),
            ))
            .unwrap();

        policy
            .define_class(ServiceClass::new(
                "BATCH",
                GoalType::Velocity(50),
                Importance(3),
            ))
            .unwrap();

        let mut def = ServiceDefinition::new(policy);

        let mut wl = Workload::new("PROD");
        wl.add_class("ONLINE");
        wl.add_class("BATCH");
        def.define_workload(wl).unwrap();

        let mut rg = ResourceGroup::new("POOL1");
        rg.set_cpu_limit(30.0, CpuCapType::Soft);
        rg.add_class("ONLINE");
        def.define_resource_group(rg).unwrap();

        def.define_report_class(ReportClass::new("RPT1")).unwrap();

        def
    }

    // ─── WLM-108.1: Policy Serialization ───

    #[test]
    fn test_store_policy() {
        let mut store = PolicyStore::new();
        let def = make_definition("PROD");
        store.store(&def).unwrap();
        assert!(store.contains("PROD"));
        assert_eq!(store.count(), 1);
    }

    #[test]
    fn test_store_serializes_all_components() {
        let mut store = PolicyStore::new();
        let def = make_definition("PROD");
        store.store(&def).unwrap();

        let loaded = store.load("PROD").unwrap();
        assert_eq!(loaded.policy.class_count(), 2);
        assert_eq!(loaded.workloads.len(), 1);
        assert_eq!(loaded.resource_groups.len(), 1);
        assert_eq!(loaded.report_classes.len(), 1);

        let rg = loaded.resource_groups.get("POOL1").unwrap();
        assert_eq!(rg.cpu_limit, Some(30.0));
    }

    #[test]
    fn test_store_multiple_policies() {
        let mut store = PolicyStore::new();
        store.store(&make_definition("PROD")).unwrap();
        store.store(&make_definition("TEST")).unwrap();
        store.store(&make_definition("DR")).unwrap();
        assert_eq!(store.count(), 3);
    }

    #[test]
    fn test_list_policies() {
        let mut store = PolicyStore::new();
        store.store(&make_definition("ALPHA")).unwrap();
        store.store(&make_definition("BETA")).unwrap();
        let mut names = store.list();
        names.sort();
        assert_eq!(names, vec!["ALPHA", "BETA"]);
    }

    // ─── WLM-108.2: Policy Deserialization ───

    #[test]
    fn test_load_policy() {
        let mut store = PolicyStore::new();
        store.store(&make_definition("PROD")).unwrap();

        let loaded = store.load("PROD").unwrap();
        assert_eq!(loaded.policy.name, "PROD");
        assert!(loaded.validate().is_ok());
    }

    #[test]
    fn test_load_nonexistent_policy() {
        let store = PolicyStore::new();
        assert!(store.load("NOPE").is_err());
    }

    #[test]
    fn test_remove_policy() {
        let mut store = PolicyStore::new();
        store.store(&make_definition("PROD")).unwrap();
        store.remove("PROD").unwrap();
        assert!(!store.contains("PROD"));
    }

    #[test]
    fn test_export_import_roundtrip() {
        let mut store = PolicyStore::new();
        store.store(&make_definition("PROD")).unwrap();
        store.store(&make_definition("TEST")).unwrap();

        let exported = store.export_all().unwrap();

        let mut store2 = PolicyStore::new();
        let count = store2.import_all(&exported).unwrap();
        assert_eq!(count, 2);
        assert!(store2.contains("PROD"));
        assert!(store2.contains("TEST"));
    }

    // ─── WLM-108.3: Policy Activation (VARY WLM,POLICY=) ───

    #[test]
    fn test_activate_policy() {
        let mut store = PolicyStore::new();
        store.store(&make_definition("PROD")).unwrap();

        let result = store.activate("PROD").unwrap();
        assert_eq!(result.policy_name, "PROD");
        assert!(result.previous_policy.is_none());
        assert_eq!(result.class_count, 2);

        assert_eq!(store.active_policy_name(), Some("PROD"));
        assert!(store.active_definition().is_some());
    }

    #[test]
    fn test_activate_replaces_current_atomically() {
        let mut store = PolicyStore::new();
        store.store(&make_definition("PROD")).unwrap();
        store.store(&make_definition("DR")).unwrap();

        store.activate("PROD").unwrap();
        let result = store.activate("DR").unwrap();
        assert_eq!(result.policy_name, "DR");
        assert_eq!(result.previous_policy, Some("PROD".to_string()));
        assert_eq!(store.active_policy_name(), Some("DR"));
    }

    #[test]
    fn test_activate_nonexistent_fails() {
        let mut store = PolicyStore::new();
        assert!(store.activate("MISSING").is_err());
    }

    #[test]
    fn test_activate_invalid_policy_keeps_current() {
        let mut store = PolicyStore::new();
        store.store(&make_definition("GOOD")).unwrap();
        store.activate("GOOD").unwrap();

        // Create an invalid policy (references nonexistent class).
        let mut bad_policy = ServicePolicy::new("BAD");
        bad_policy
            .define_class(ServiceClass::new(
                "X",
                GoalType::Discretionary,
                Importance(5),
            ))
            .unwrap();
        let mut bad_def = ServiceDefinition::new(bad_policy);
        let mut wl = Workload::new("W");
        wl.add_class("NONEXISTENT");
        bad_def.define_workload(wl).unwrap();
        store.store(&bad_def).unwrap();

        // Activation should fail, keeping GOOD active.
        assert!(store.activate("BAD").is_err());
        assert_eq!(store.active_policy_name(), Some("GOOD"));
    }

    #[test]
    fn test_cannot_remove_active_policy() {
        let mut store = PolicyStore::new();
        store.store(&make_definition("PROD")).unwrap();
        store.activate("PROD").unwrap();

        assert!(store.remove("PROD").is_err());
    }

    #[test]
    fn test_deactivate() {
        let mut store = PolicyStore::new();
        store.store(&make_definition("PROD")).unwrap();
        store.activate("PROD").unwrap();

        let prev = store.deactivate();
        assert_eq!(prev, Some("PROD".to_string()));
        assert!(store.active_policy_name().is_none());
        assert!(store.active_definition().is_none());
    }

    // ─── WLM-108.4: Persistence Integration Tests ───

    #[test]
    fn test_full_persistence_workflow() {
        let mut store = PolicyStore::new();

        // Store multiple policies.
        store.store(&make_definition("DAYTIME")).unwrap();
        store.store(&make_definition("NIGHTTIME")).unwrap();
        store.store(&make_definition("WEEKEND")).unwrap();
        assert_eq!(store.count(), 3);

        // Activate daytime policy.
        let r = store.activate("DAYTIME").unwrap();
        assert_eq!(r.class_count, 2);
        assert!(r.previous_policy.is_none());

        // Switch to nighttime.
        let r = store.activate("NIGHTTIME").unwrap();
        assert_eq!(r.previous_policy, Some("DAYTIME".to_string()));

        // Switch to weekend.
        let r = store.activate("WEEKEND").unwrap();
        assert_eq!(r.previous_policy, Some("NIGHTTIME".to_string()));

        // Verify active.
        let active = store.active_definition().unwrap();
        assert_eq!(active.policy.name, "WEEKEND");
        assert!(active.validate().is_ok());

        // Deactivate.
        store.deactivate();
        assert!(store.active_policy_name().is_none());

        // Remove a policy.
        store.remove("WEEKEND").unwrap();
        assert_eq!(store.count(), 2);
    }

    #[test]
    fn test_case_insensitive_names() {
        let mut store = PolicyStore::new();
        store.store(&make_definition("prod")).unwrap();
        assert!(store.contains("PROD"));
        assert!(store.contains("prod"));

        let loaded = store.load("Prod").unwrap();
        assert_eq!(loaded.policy.name, "PROD");
    }
}
