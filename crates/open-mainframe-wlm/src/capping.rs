//! # Resource Group Capping Engine
//!
//! Enforces CPU and memory caps on resource groups. Supports hard and soft
//! cap types and tracks utilization for throttling decisions.

use std::collections::HashMap;

use crate::policy::{CpuCapType, ResourceGroup};

// ─────────────────────── Utilization Tracking ───────────────────────

/// Current resource utilization for a resource group.
#[derive(Debug, Clone, Default)]
pub struct GroupUtilization {
    /// CPU utilization as a percentage (0.0 - 100.0).
    pub cpu_percent: f64,
    /// Memory usage in megabytes.
    pub memory_mb: u64,
    /// Number of active work units.
    pub active_work_units: u32,
    /// Whether the group is currently throttled.
    pub throttled: bool,
    /// Total CPU seconds consumed in this interval.
    pub cpu_seconds: f64,
    /// Memory high-water mark.
    pub memory_hwm_mb: u64,
}

/// Throttle decision.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThrottleAction {
    /// Allow work to proceed.
    Allow,
    /// Delay/throttle work.
    Throttle,
    /// Allow temporarily (soft cap exceeded but resources available).
    AllowTemporary,
}

// ─────────────────────── Enforcement Backend ───────────────────────

/// Runtime environment for enforcement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeEnvironment {
    /// Linux — use cgroups for enforcement.
    Linux,
    /// Kubernetes — use resource quotas.
    Kubernetes,
    /// Simulated — no real OS enforcement, just tracking.
    Simulated,
}

/// An enforcement action to configure OS-level controls.
#[derive(Debug, Clone)]
pub enum EnforcementAction {
    /// Configure a cgroup CPU limit.
    CgroupCpuLimit {
        /// Group name.
        group: String,
        /// CPU quota in microseconds per period.
        quota_us: u64,
        /// Period in microseconds.
        period_us: u64,
    },
    /// Configure a cgroup memory limit.
    CgroupMemoryLimit {
        /// Group name.
        group: String,
        /// Memory limit in bytes.
        limit_bytes: u64,
    },
    /// Configure a Kubernetes resource quota.
    K8sResourceQuota {
        /// Namespace.
        namespace: String,
        /// CPU limit (milliCPU).
        cpu_milli: u64,
        /// Memory limit in bytes.
        memory_bytes: u64,
    },
}

// ─────────────────────── Capping Engine ───────────────────────

/// The resource group capping engine.
#[derive(Debug)]
pub struct CappingEngine {
    /// Runtime environment.
    pub environment: RuntimeEnvironment,
    /// Resource group utilization by name.
    utilization: HashMap<String, GroupUtilization>,
    /// System-wide CPU count (for percentage calculation).
    system_cpu_count: u32,
    /// Whether system has spare capacity (for soft cap decisions).
    spare_capacity: bool,
}

impl CappingEngine {
    /// Create a new capping engine.
    pub fn new(environment: RuntimeEnvironment, cpu_count: u32) -> Self {
        Self {
            environment,
            utilization: HashMap::new(),
            system_cpu_count: cpu_count,
            spare_capacity: true,
        }
    }

    /// Set system spare capacity flag.
    pub fn set_spare_capacity(&mut self, available: bool) {
        self.spare_capacity = available;
    }

    /// Update CPU utilization for a resource group.
    pub fn update_cpu(&mut self, group_name: &str, cpu_percent: f64, cpu_seconds: f64) {
        let util = self
            .utilization
            .entry(group_name.to_uppercase())
            .or_default();
        util.cpu_percent = cpu_percent;
        util.cpu_seconds = cpu_seconds;
    }

    /// Update memory utilization for a resource group.
    pub fn update_memory(&mut self, group_name: &str, memory_mb: u64) {
        let util = self
            .utilization
            .entry(group_name.to_uppercase())
            .or_default();
        util.memory_mb = memory_mb;
        if memory_mb > util.memory_hwm_mb {
            util.memory_hwm_mb = memory_mb;
        }
    }

    /// Update active work unit count.
    pub fn update_work_units(&mut self, group_name: &str, count: u32) {
        let util = self
            .utilization
            .entry(group_name.to_uppercase())
            .or_default();
        util.active_work_units = count;
    }

    /// Evaluate whether work should be throttled for a resource group.
    pub fn evaluate_throttle(&mut self, group: &ResourceGroup) -> ThrottleAction {
        let name = group.name.to_uppercase();
        let util = self.utilization.entry(name).or_default();

        // Check CPU cap.
        if let Some(cpu_limit) = group.cpu_limit {
            if util.cpu_percent >= cpu_limit {
                match group.cpu_cap_type {
                    CpuCapType::Hard => {
                        util.throttled = true;
                        return ThrottleAction::Throttle;
                    }
                    CpuCapType::Soft => {
                        if self.spare_capacity {
                            return ThrottleAction::AllowTemporary;
                        }
                        util.throttled = true;
                        return ThrottleAction::Throttle;
                    }
                }
            }
        }

        // Check memory limit.
        if let Some(mem_limit) = group.memory_limit_mb {
            if util.memory_mb >= mem_limit {
                util.throttled = true;
                return ThrottleAction::Throttle;
            }
        }

        util.throttled = false;
        ThrottleAction::Allow
    }

    /// Check if a resource group is approaching its memory limit.
    /// Returns percentage of limit used.
    pub fn memory_usage_percent(&self, group: &ResourceGroup) -> Option<f64> {
        let name = group.name.to_uppercase();
        let util = self.utilization.get(&name)?;
        let limit = group.memory_limit_mb?;
        if limit == 0 {
            return Some(0.0);
        }
        Some((util.memory_mb as f64 / limit as f64) * 100.0)
    }

    /// Generate enforcement actions for a resource group.
    pub fn generate_enforcement(&self, group: &ResourceGroup) -> Vec<EnforcementAction> {
        let mut actions = Vec::new();

        match self.environment {
            RuntimeEnvironment::Linux => {
                if let Some(cpu_limit) = group.cpu_limit {
                    // Convert CPU percentage to cgroup quota.
                    let period_us: u64 = 100_000; // 100ms
                    let quota_us =
                        ((cpu_limit / 100.0) * self.system_cpu_count as f64 * period_us as f64)
                            as u64;
                    actions.push(EnforcementAction::CgroupCpuLimit {
                        group: group.name.clone(),
                        quota_us,
                        period_us,
                    });
                }
                if let Some(mem_mb) = group.memory_limit_mb {
                    actions.push(EnforcementAction::CgroupMemoryLimit {
                        group: group.name.clone(),
                        limit_bytes: mem_mb * 1024 * 1024,
                    });
                }
            }
            RuntimeEnvironment::Kubernetes => {
                let cpu_milli = group
                    .cpu_limit
                    .map(|p| (p / 100.0 * self.system_cpu_count as f64 * 1000.0) as u64)
                    .unwrap_or(0);
                let memory_bytes = group.memory_limit_mb.unwrap_or(0) * 1024 * 1024;

                if cpu_milli > 0 || memory_bytes > 0 {
                    actions.push(EnforcementAction::K8sResourceQuota {
                        namespace: group.name.to_lowercase(),
                        cpu_milli,
                        memory_bytes,
                    });
                }
            }
            RuntimeEnvironment::Simulated => {
                // No enforcement actions in simulation mode.
            }
        }

        actions
    }

    /// Get utilization for a group.
    pub fn get_utilization(&self, group_name: &str) -> Option<&GroupUtilization> {
        self.utilization.get(&group_name.to_uppercase())
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::policy::CpuCapType;

    fn make_group(name: &str, cpu: Option<f64>, cap_type: CpuCapType, mem: Option<u64>) -> ResourceGroup {
        let mut rg = ResourceGroup::new(name);
        if let Some(cpu_pct) = cpu {
            rg.set_cpu_limit(cpu_pct, cap_type);
        }
        if let Some(mb) = mem {
            rg.set_memory_limit(mb);
        }
        rg
    }

    // ─── WLM-103.1: CPU Capping ───

    #[test]
    fn test_cpu_hard_cap_throttle() {
        let mut engine = CappingEngine::new(RuntimeEnvironment::Simulated, 4);
        let group = make_group("POOL1", Some(20.0), CpuCapType::Hard, None);

        engine.update_cpu("POOL1", 25.0, 100.0);
        let action = engine.evaluate_throttle(&group);
        assert_eq!(action, ThrottleAction::Throttle);
    }

    #[test]
    fn test_cpu_under_cap_allowed() {
        let mut engine = CappingEngine::new(RuntimeEnvironment::Simulated, 4);
        let group = make_group("POOL1", Some(20.0), CpuCapType::Hard, None);

        engine.update_cpu("POOL1", 15.0, 50.0);
        let action = engine.evaluate_throttle(&group);
        assert_eq!(action, ThrottleAction::Allow);
    }

    #[test]
    fn test_cpu_at_exact_cap() {
        let mut engine = CappingEngine::new(RuntimeEnvironment::Simulated, 4);
        let group = make_group("POOL1", Some(20.0), CpuCapType::Hard, None);

        engine.update_cpu("POOL1", 20.0, 80.0);
        let action = engine.evaluate_throttle(&group);
        assert_eq!(action, ThrottleAction::Throttle);
    }

    // ─── WLM-103.2: Memory Limits ───

    #[test]
    fn test_memory_limit_throttle() {
        let mut engine = CappingEngine::new(RuntimeEnvironment::Simulated, 4);
        let group = make_group("POOL1", None, CpuCapType::Soft, Some(2048));

        engine.update_memory("POOL1", 2100);
        let action = engine.evaluate_throttle(&group);
        assert_eq!(action, ThrottleAction::Throttle);
    }

    #[test]
    fn test_memory_approaching_limit() {
        let mut engine = CappingEngine::new(RuntimeEnvironment::Simulated, 4);
        let group = make_group("POOL1", None, CpuCapType::Soft, Some(2048));

        engine.update_memory("POOL1", 1900);
        let pct = engine.memory_usage_percent(&group).unwrap();
        assert!(pct > 90.0);
        assert!(pct < 100.0);
    }

    #[test]
    fn test_memory_hwm() {
        let mut engine = CappingEngine::new(RuntimeEnvironment::Simulated, 4);
        engine.update_memory("POOL1", 1000);
        engine.update_memory("POOL1", 2000);
        engine.update_memory("POOL1", 1500);

        let util = engine.get_utilization("POOL1").unwrap();
        assert_eq!(util.memory_hwm_mb, 2000);
    }

    // ─── WLM-103.3: Soft/Hard Cap Types ───

    #[test]
    fn test_soft_cap_with_spare_capacity() {
        let mut engine = CappingEngine::new(RuntimeEnvironment::Simulated, 4);
        engine.set_spare_capacity(true);
        let group = make_group("POOL1", Some(20.0), CpuCapType::Soft, None);

        engine.update_cpu("POOL1", 25.0, 100.0);
        let action = engine.evaluate_throttle(&group);
        assert_eq!(action, ThrottleAction::AllowTemporary);
    }

    #[test]
    fn test_soft_cap_without_spare_capacity() {
        let mut engine = CappingEngine::new(RuntimeEnvironment::Simulated, 4);
        engine.set_spare_capacity(false);
        let group = make_group("POOL1", Some(20.0), CpuCapType::Soft, None);

        engine.update_cpu("POOL1", 25.0, 100.0);
        let action = engine.evaluate_throttle(&group);
        assert_eq!(action, ThrottleAction::Throttle);
    }

    // ─── WLM-103.4: Enforcement Actions ───

    #[test]
    fn test_linux_cgroup_enforcement() {
        let engine = CappingEngine::new(RuntimeEnvironment::Linux, 4);
        let group = make_group("POOL1", Some(20.0), CpuCapType::Hard, Some(2048));

        let actions = engine.generate_enforcement(&group);
        assert_eq!(actions.len(), 2);

        // CPU cgroup: 20% of 4 CPUs = 0.8 CPU = 80000us per 100000us period.
        match &actions[0] {
            EnforcementAction::CgroupCpuLimit {
                quota_us,
                period_us,
                ..
            } => {
                assert_eq!(*period_us, 100_000);
                assert_eq!(*quota_us, 80_000);
            }
            _ => panic!("Expected CgroupCpuLimit"),
        }

        // Memory cgroup: 2048 MB = 2048 * 1024 * 1024 bytes.
        match &actions[1] {
            EnforcementAction::CgroupMemoryLimit { limit_bytes, .. } => {
                assert_eq!(*limit_bytes, 2048 * 1024 * 1024);
            }
            _ => panic!("Expected CgroupMemoryLimit"),
        }
    }

    #[test]
    fn test_k8s_enforcement() {
        let engine = CappingEngine::new(RuntimeEnvironment::Kubernetes, 4);
        let group = make_group("POOL1", Some(25.0), CpuCapType::Soft, Some(4096));

        let actions = engine.generate_enforcement(&group);
        assert_eq!(actions.len(), 1);

        match &actions[0] {
            EnforcementAction::K8sResourceQuota {
                cpu_milli,
                memory_bytes,
                ..
            } => {
                // 25% of 4 CPUs = 1000 milliCPU.
                assert_eq!(*cpu_milli, 1000);
                assert_eq!(*memory_bytes, 4096 * 1024 * 1024);
            }
            _ => panic!("Expected K8sResourceQuota"),
        }
    }

    #[test]
    fn test_simulated_no_enforcement() {
        let engine = CappingEngine::new(RuntimeEnvironment::Simulated, 4);
        let group = make_group("POOL1", Some(20.0), CpuCapType::Hard, Some(2048));

        let actions = engine.generate_enforcement(&group);
        assert!(actions.is_empty());
    }

    // ─── WLM-103.5: Integration ───

    #[test]
    fn test_full_capping_scenario() {
        let mut engine = CappingEngine::new(RuntimeEnvironment::Simulated, 4);
        engine.set_spare_capacity(true);

        let hard_group = make_group("CRITICAL", Some(40.0), CpuCapType::Hard, Some(4096));
        let soft_group = make_group("BATCH", Some(30.0), CpuCapType::Soft, None);

        // CRITICAL under limits.
        engine.update_cpu("CRITICAL", 30.0, 200.0);
        engine.update_memory("CRITICAL", 3000);
        assert_eq!(engine.evaluate_throttle(&hard_group), ThrottleAction::Allow);

        // BATCH over CPU but soft cap + spare.
        engine.update_cpu("BATCH", 35.0, 150.0);
        assert_eq!(
            engine.evaluate_throttle(&soft_group),
            ThrottleAction::AllowTemporary
        );

        // Remove spare capacity → BATCH throttled.
        engine.set_spare_capacity(false);
        assert_eq!(
            engine.evaluate_throttle(&soft_group),
            ThrottleAction::Throttle
        );
    }
}
