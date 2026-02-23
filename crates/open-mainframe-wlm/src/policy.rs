//! # WLM Policy Extensions — Workloads, Report Classes, Resource Groups
//!
//! Extends the service policy data model with workload groupings,
//! report classes for RMF-style reporting, and resource groups for
//! CPU/memory capping.

use std::collections::HashMap;

use crate::service::{ServicePolicy, WlmError};

// ─────────────────────── Workload ───────────────────────

/// A workload groups related service classes for organizational purposes.
///
/// Example: workload "PRODUCTION" contains service classes "ONLINE" and "BATCH".
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Workload {
    /// Workload name.
    pub name: String,
    /// Description.
    pub description: String,
    /// Service class names that belong to this workload.
    pub service_classes: Vec<String>,
}

impl Workload {
    /// Create a new workload.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            description: String::new(),
            service_classes: Vec::new(),
        }
    }

    /// Add a service class to this workload.
    pub fn add_class(&mut self, class_name: &str) {
        let upper = class_name.to_uppercase();
        if !self.service_classes.contains(&upper) {
            self.service_classes.push(upper);
        }
    }

    /// Remove a service class.
    pub fn remove_class(&mut self, class_name: &str) {
        let upper = class_name.to_uppercase();
        self.service_classes.retain(|c| c != &upper);
    }
}

// ─────────────────────── Report Class ───────────────────────

/// A report class for RMF-style performance reporting.
///
/// Work items are assigned to a report class for monitoring and tuning
/// independent of the service class used for goal management.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ReportClass {
    /// Report class name.
    pub name: String,
    /// Description.
    pub description: String,
}

impl ReportClass {
    /// Create a new report class.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            description: String::new(),
        }
    }
}

// ─────────────────────── Resource Group ───────────────────────

/// CPU cap type.
#[derive(Debug, Clone, Copy, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum CpuCapType {
    /// Hard cap — work is delayed when limit is reached.
    Hard,
    /// Soft cap — exceeded only when system has spare capacity.
    Soft,
}

/// A resource group for CPU and memory capping.
///
/// Resource groups limit the resources consumed by associated service classes.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ResourceGroup {
    /// Resource group name.
    pub name: String,
    /// Description.
    pub description: String,
    /// Maximum CPU percentage (0.0 to 100.0). None = uncapped.
    pub cpu_limit: Option<f64>,
    /// CPU cap type.
    pub cpu_cap_type: CpuCapType,
    /// Maximum memory in megabytes. None = uncapped.
    pub memory_limit_mb: Option<u64>,
    /// Minimum CPU percentage guaranteed. None = no minimum.
    pub cpu_minimum: Option<f64>,
    /// Service class names assigned to this resource group.
    pub service_classes: Vec<String>,
}

impl ResourceGroup {
    /// Create a new resource group.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            description: String::new(),
            cpu_limit: None,
            cpu_cap_type: CpuCapType::Soft,
            memory_limit_mb: None,
            cpu_minimum: None,
            service_classes: Vec::new(),
        }
    }

    /// Set CPU cap.
    pub fn set_cpu_limit(&mut self, percent: f64, cap_type: CpuCapType) {
        self.cpu_limit = Some(percent.clamp(0.0, 100.0));
        self.cpu_cap_type = cap_type;
    }

    /// Set memory limit.
    pub fn set_memory_limit(&mut self, mb: u64) {
        self.memory_limit_mb = Some(mb);
    }

    /// Set CPU minimum.
    pub fn set_cpu_minimum(&mut self, percent: f64) {
        self.cpu_minimum = Some(percent.clamp(0.0, 100.0));
    }

    /// Add a service class.
    pub fn add_class(&mut self, class_name: &str) {
        let upper = class_name.to_uppercase();
        if !self.service_classes.contains(&upper) {
            self.service_classes.push(upper);
        }
    }
}

// ─────────────────────── Extended Policy ───────────────────────

/// An extended service definition containing all WLM policy elements.
///
/// This wraps a `ServicePolicy` and adds workloads, report classes,
/// and resource groups.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ServiceDefinition {
    /// The underlying service policy (service classes + goals).
    pub policy: ServicePolicy,
    /// Workloads by name.
    pub workloads: HashMap<String, Workload>,
    /// Report classes by name.
    pub report_classes: HashMap<String, ReportClass>,
    /// Resource groups by name.
    pub resource_groups: HashMap<String, ResourceGroup>,
}

impl ServiceDefinition {
    /// Create a new service definition wrapping a policy.
    pub fn new(policy: ServicePolicy) -> Self {
        Self {
            policy,
            workloads: HashMap::new(),
            report_classes: HashMap::new(),
            resource_groups: HashMap::new(),
        }
    }

    /// Define a workload.
    pub fn define_workload(&mut self, workload: Workload) -> Result<(), WlmError> {
        let name = workload.name.clone();
        if self.workloads.contains_key(&name) {
            return Err(WlmError::Other(format!("Workload already exists: {name}")));
        }
        self.workloads.insert(name, workload);
        Ok(())
    }

    /// Define a report class.
    pub fn define_report_class(&mut self, rc: ReportClass) -> Result<(), WlmError> {
        let name = rc.name.clone();
        if self.report_classes.contains_key(&name) {
            return Err(WlmError::Other(format!(
                "Report class already exists: {name}"
            )));
        }
        self.report_classes.insert(name, rc);
        Ok(())
    }

    /// Define a resource group.
    pub fn define_resource_group(&mut self, rg: ResourceGroup) -> Result<(), WlmError> {
        let name = rg.name.clone();
        if self.resource_groups.contains_key(&name) {
            return Err(WlmError::Other(format!(
                "Resource group already exists: {name}"
            )));
        }
        self.resource_groups.insert(name, rg);
        Ok(())
    }

    /// Validate the service definition.
    ///
    /// Checks that:
    /// 1. All service classes referenced in workloads exist in the policy.
    /// 2. All service classes referenced in resource groups exist in the policy.
    /// 3. No service class belongs to more than one resource group.
    pub fn validate(&self) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        // Check workload references.
        for (wl_name, wl) in &self.workloads {
            for class_name in &wl.service_classes {
                if self.policy.lookup_class(class_name).is_none() {
                    errors.push(format!(
                        "Workload {wl_name} references undefined service class {class_name}"
                    ));
                }
            }
        }

        // Check resource group references.
        let mut class_to_rg: HashMap<String, String> = HashMap::new();
        for (rg_name, rg) in &self.resource_groups {
            for class_name in &rg.service_classes {
                if self.policy.lookup_class(class_name).is_none() {
                    errors.push(format!(
                        "Resource group {rg_name} references undefined service class {class_name}"
                    ));
                }
                if let Some(other_rg) = class_to_rg.get(class_name) {
                    errors.push(format!(
                        "Service class {class_name} assigned to multiple resource groups: {other_rg} and {rg_name}"
                    ));
                } else {
                    class_to_rg.insert(class_name.clone(), rg_name.clone());
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Serialize to JSON.
    pub fn to_json(&self) -> Result<String, WlmError> {
        serde_json::to_string_pretty(self)
            .map_err(|e| WlmError::Other(format!("Serialization error: {e}")))
    }

    /// Deserialize from JSON.
    pub fn from_json(json: &str) -> Result<Self, WlmError> {
        serde_json::from_str(json)
            .map_err(|e| WlmError::Other(format!("Deserialization error: {e}")))
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::service::{GoalType, Importance, ServiceClass};

    fn setup_definition() -> ServiceDefinition {
        let mut policy = ServicePolicy::new("PROD");

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

        policy
            .define_class(ServiceClass::new(
                "TSO",
                GoalType::ResponseTime {
                    target_seconds: 1.0,
                    percentile: 90.0,
                },
                Importance(2),
            ))
            .unwrap();

        policy
            .define_class(ServiceClass::new(
                "LOWPRI",
                GoalType::Discretionary,
                Importance(5),
            ))
            .unwrap();

        ServiceDefinition::new(policy)
    }

    // ─── WLM-100.4: Workload and Report Class ───

    #[test]
    fn test_workload_creation() {
        let mut wl = Workload::new("PRODUCTION");
        assert_eq!(wl.name, "PRODUCTION");
        wl.add_class("ONLINE");
        wl.add_class("BATCH");
        assert_eq!(wl.service_classes.len(), 2);
    }

    #[test]
    fn test_workload_no_duplicates() {
        let mut wl = Workload::new("PROD");
        wl.add_class("ONLINE");
        wl.add_class("ONLINE");
        assert_eq!(wl.service_classes.len(), 1);
    }

    #[test]
    fn test_workload_remove_class() {
        let mut wl = Workload::new("PROD");
        wl.add_class("ONLINE");
        wl.add_class("BATCH");
        wl.remove_class("ONLINE");
        assert_eq!(wl.service_classes.len(), 1);
        assert_eq!(wl.service_classes[0], "BATCH");
    }

    #[test]
    fn test_report_class() {
        let rc = ReportClass::new("CICSTXNS");
        assert_eq!(rc.name, "CICSTXNS");
    }

    #[test]
    fn test_define_workload_in_definition() {
        let mut def = setup_definition();

        let mut wl = Workload::new("PRODUCTION");
        wl.add_class("ONLINE");
        wl.add_class("BATCH");
        def.define_workload(wl).unwrap();

        assert_eq!(def.workloads.len(), 1);
        assert_eq!(
            def.workloads.get("PRODUCTION").unwrap().service_classes.len(),
            2
        );
    }

    #[test]
    fn test_define_report_class_in_definition() {
        let mut def = setup_definition();
        def.define_report_class(ReportClass::new("MONITOR")).unwrap();
        assert_eq!(def.report_classes.len(), 1);
    }

    #[test]
    fn test_duplicate_workload() {
        let mut def = setup_definition();
        def.define_workload(Workload::new("WL1")).unwrap();
        assert!(def.define_workload(Workload::new("WL1")).is_err());
    }

    // ─── WLM-100.5: Resource Group Definition ───

    #[test]
    fn test_resource_group_cpu_cap() {
        let mut rg = ResourceGroup::new("CICSPOOL");
        rg.set_cpu_limit(20.0, CpuCapType::Hard);
        assert_eq!(rg.cpu_limit, Some(20.0));
        assert_eq!(rg.cpu_cap_type, CpuCapType::Hard);
    }

    #[test]
    fn test_resource_group_memory_limit() {
        let mut rg = ResourceGroup::new("BATCHPOOL");
        rg.set_memory_limit(4096);
        assert_eq!(rg.memory_limit_mb, Some(4096));
    }

    #[test]
    fn test_resource_group_cpu_minimum() {
        let mut rg = ResourceGroup::new("PRIORITY");
        rg.set_cpu_minimum(10.0);
        assert_eq!(rg.cpu_minimum, Some(10.0));
    }

    #[test]
    fn test_resource_group_service_classes() {
        let mut rg = ResourceGroup::new("POOL1");
        rg.add_class("ONLINE");
        rg.add_class("TSO");
        assert_eq!(rg.service_classes.len(), 2);
    }

    #[test]
    fn test_define_resource_group_in_definition() {
        let mut def = setup_definition();

        let mut rg = ResourceGroup::new("POOL1");
        rg.set_cpu_limit(50.0, CpuCapType::Soft);
        rg.add_class("ONLINE");
        def.define_resource_group(rg).unwrap();

        assert_eq!(def.resource_groups.len(), 1);
    }

    // ─── WLM-100.6: Service Policy Composition & Validation ───

    #[test]
    fn test_validate_valid_definition() {
        let mut def = setup_definition();

        let mut wl = Workload::new("PROD");
        wl.add_class("ONLINE");
        wl.add_class("BATCH");
        def.define_workload(wl).unwrap();

        let mut rg = ResourceGroup::new("POOL1");
        rg.add_class("ONLINE");
        def.define_resource_group(rg).unwrap();

        assert!(def.validate().is_ok());
    }

    #[test]
    fn test_validate_missing_class_in_workload() {
        let mut def = setup_definition();

        let mut wl = Workload::new("BAD");
        wl.add_class("NONEXISTENT");
        def.define_workload(wl).unwrap();

        let errors = def.validate().unwrap_err();
        assert!(errors
            .iter()
            .any(|e| e.contains("NONEXISTENT") && e.contains("undefined")));
    }

    #[test]
    fn test_validate_missing_class_in_resource_group() {
        let mut def = setup_definition();

        let mut rg = ResourceGroup::new("POOL");
        rg.add_class("MISSING");
        def.define_resource_group(rg).unwrap();

        let errors = def.validate().unwrap_err();
        assert!(errors.iter().any(|e| e.contains("MISSING")));
    }

    #[test]
    fn test_validate_duplicate_resource_group_assignment() {
        let mut def = setup_definition();

        let mut rg1 = ResourceGroup::new("POOL1");
        rg1.add_class("ONLINE");
        def.define_resource_group(rg1).unwrap();

        let mut rg2 = ResourceGroup::new("POOL2");
        rg2.add_class("ONLINE");
        def.define_resource_group(rg2).unwrap();

        let errors = def.validate().unwrap_err();
        assert!(errors
            .iter()
            .any(|e| e.contains("ONLINE") && e.contains("multiple resource groups")));
    }

    // ─── WLM-100.7: Serialization/Deserialization ───

    #[test]
    fn test_json_roundtrip() {
        let mut def = setup_definition();

        let mut wl = Workload::new("PROD");
        wl.add_class("ONLINE");
        def.define_workload(wl).unwrap();

        let mut rg = ResourceGroup::new("POOL1");
        rg.set_cpu_limit(30.0, CpuCapType::Hard);
        rg.add_class("BATCH");
        def.define_resource_group(rg).unwrap();

        def.define_report_class(ReportClass::new("RPT1")).unwrap();

        let json = def.to_json().unwrap();
        let restored = ServiceDefinition::from_json(&json).unwrap();

        assert_eq!(restored.policy.name, "PROD");
        assert_eq!(restored.policy.class_count(), 4);
        assert_eq!(restored.workloads.len(), 1);
        assert_eq!(restored.resource_groups.len(), 1);
        assert_eq!(restored.report_classes.len(), 1);

        let rg = restored.resource_groups.get("POOL1").unwrap();
        assert_eq!(rg.cpu_limit, Some(30.0));
        assert_eq!(rg.cpu_cap_type, CpuCapType::Hard);
    }

    #[test]
    fn test_multi_period_serialization() {
        let mut policy = ServicePolicy::new("TEST");
        let mut sc = ServiceClass::new("AGING", GoalType::Velocity(80), Importance(2));
        sc.add_period(GoalType::Velocity(50), Importance(3), 500);
        sc.add_period(GoalType::Discretionary, Importance(5), 1000);
        policy.define_class(sc).unwrap();

        let def = ServiceDefinition::new(policy);
        let json = def.to_json().unwrap();
        let restored = ServiceDefinition::from_json(&json).unwrap();

        let class = restored.policy.lookup_class("AGING").unwrap();
        assert_eq!(class.periods.len(), 3);
        assert_eq!(class.periods[1].duration, 500);
    }

    #[test]
    fn test_complete_policy_composition() {
        let mut def = setup_definition();

        // Define workloads.
        let mut prod_wl = Workload::new("PRODUCTION");
        prod_wl.description = "Production workloads".into();
        prod_wl.add_class("ONLINE");
        prod_wl.add_class("BATCH");
        def.define_workload(prod_wl).unwrap();

        let mut dev_wl = Workload::new("DEVELOPMENT");
        dev_wl.add_class("TSO");
        dev_wl.add_class("LOWPRI");
        def.define_workload(dev_wl).unwrap();

        // Define report classes.
        def.define_report_class(ReportClass::new("CICSTXNS")).unwrap();
        def.define_report_class(ReportClass::new("BATCHJOBS")).unwrap();

        // Define resource groups.
        let mut rg1 = ResourceGroup::new("ONLINEPOOL");
        rg1.set_cpu_limit(40.0, CpuCapType::Hard);
        rg1.set_memory_limit(8192);
        rg1.add_class("ONLINE");
        def.define_resource_group(rg1).unwrap();

        let mut rg2 = ResourceGroup::new("BATCHPOOL");
        rg2.set_cpu_limit(30.0, CpuCapType::Soft);
        rg2.set_cpu_minimum(10.0);
        rg2.add_class("BATCH");
        def.define_resource_group(rg2).unwrap();

        // Validate.
        assert!(def.validate().is_ok());

        // Serialization roundtrip.
        let json = def.to_json().unwrap();
        let restored = ServiceDefinition::from_json(&json).unwrap();
        assert!(restored.validate().is_ok());
        assert_eq!(restored.workloads.len(), 2);
        assert_eq!(restored.resource_groups.len(), 2);
        assert_eq!(restored.report_classes.len(), 2);
    }
}
