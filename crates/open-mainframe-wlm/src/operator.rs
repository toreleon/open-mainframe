//! # WLM Operator Commands & Monitoring
//!
//! Provides:
//! - `D WLM` (DISPLAY WLM) — show active policy, service class PI, resource group utilization
//! - `VARY WLM,POLICY=` — switch active policy
//! - Prometheus-style metrics export
//! - SMF Type 72/99 record generation

use std::collections::HashMap;

// ─────────────────────── Display WLM ───────────────────────

/// A service class status entry in a DISPLAY WLM response.
#[derive(Debug, Clone)]
pub struct ClassStatus {
    /// Service class name.
    pub name: String,
    /// Current performance index.
    pub pi: f64,
    /// Goal description.
    pub goal_description: String,
    /// Number of active work units.
    pub active_work_units: u32,
    /// Importance.
    pub importance: u8,
}

/// A resource group status entry.
#[derive(Debug, Clone)]
pub struct ResourceGroupStatus {
    /// Resource group name.
    pub name: String,
    /// CPU utilization percent.
    pub cpu_percent: f64,
    /// Memory usage MB.
    pub memory_mb: u64,
    /// Whether throttled.
    pub throttled: bool,
    /// CPU cap percent (None = uncapped).
    pub cpu_cap: Option<f64>,
}

/// Response from a DISPLAY WLM command.
#[derive(Debug, Clone)]
pub struct DisplayWlmResponse {
    /// Active policy name, or None if in COMPAT mode.
    pub active_policy: Option<String>,
    /// WLM mode.
    pub mode: WlmMode,
    /// Service class statuses.
    pub classes: Vec<ClassStatus>,
    /// Resource group statuses.
    pub resource_groups: Vec<ResourceGroupStatus>,
    /// Number of managed initiators.
    pub initiator_count: u32,
    /// Number of active enclaves.
    pub enclave_count: u32,
}

impl DisplayWlmResponse {
    /// Format as a display panel (like z/OS console output).
    pub fn format(&self) -> String {
        let mut lines = Vec::new();
        lines.push("IWM029I  DISPLAY WLM".to_string());

        match &self.active_policy {
            Some(name) => {
                lines.push(format!("  ACTIVE POLICY: {name}"));
                lines.push(format!("  MODE: {}", self.mode.as_str()));
            }
            None => {
                lines.push("  NO ACTIVE POLICY (COMPAT MODE)".to_string());
            }
        }

        if !self.classes.is_empty() {
            lines.push(String::new());
            lines.push("  SERVICE CLASS     PI    GOAL             IMP  WORK".to_string());
            lines.push("  ─────────────── ──── ─────────────────── ─── ─────".to_string());
            for c in &self.classes {
                lines.push(format!(
                    "  {:<15} {:>4.1} {:<19} {:>3}  {:>4}",
                    c.name, c.pi, c.goal_description, c.importance, c.active_work_units
                ));
            }
        }

        if !self.resource_groups.is_empty() {
            lines.push(String::new());
            lines.push("  RESOURCE GROUP    CPU%  MEM(MB) CAP%   THROTTLED".to_string());
            lines.push("  ─────────────── ───── ─────── ────── ─────────".to_string());
            for rg in &self.resource_groups {
                let cap = rg
                    .cpu_cap
                    .map(|c| format!("{c:>5.1}"))
                    .unwrap_or_else(|| " NONE".to_string());
                lines.push(format!(
                    "  {:<15} {:>5.1} {:>7} {} {:>9}",
                    rg.name,
                    rg.cpu_percent,
                    rg.memory_mb,
                    cap,
                    if rg.throttled { "YES" } else { "NO" }
                ));
            }
        }

        lines.push(format!(
            "  INITIATORS: {}  ENCLAVES: {}",
            self.initiator_count, self.enclave_count
        ));

        lines.join("\n")
    }
}

/// WLM operating mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WlmMode {
    /// Goal mode — active policy drives resource management.
    Goal,
    /// Compatibility mode — no active policy.
    Compat,
}

impl WlmMode {
    /// String representation.
    pub fn as_str(&self) -> &str {
        match self {
            Self::Goal => "GOAL",
            Self::Compat => "COMPAT",
        }
    }
}

// ─────────────────────── VARY WLM ───────────────────────

/// A parsed VARY WLM command.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VaryWlmCommand {
    /// VARY WLM,POLICY=<name> — activate a policy.
    Policy(String),
    /// VARY WLM,COMPAT — switch to compatibility mode.
    Compat,
}

impl VaryWlmCommand {
    /// Parse a VARY WLM command string.
    ///
    /// Accepts:
    /// - `VARY WLM,POLICY=<name>`
    /// - `V WLM,POLICY=<name>`
    /// - `VARY WLM,COMPAT`
    pub fn parse(input: &str) -> Option<Self> {
        let upper = input.trim().to_uppercase();
        let rest = upper
            .strip_prefix("VARY WLM,")
            .or_else(|| upper.strip_prefix("V WLM,"))?;

        if let Some(name) = rest.strip_prefix("POLICY=") {
            let name = name.trim();
            if name.is_empty() {
                return None;
            }
            Some(Self::Policy(name.to_string()))
        } else if rest.trim() == "COMPAT" {
            Some(Self::Compat)
        } else {
            None
        }
    }
}

// ─────────────────────── Prometheus Metrics ───────────────────────

/// A single Prometheus metric.
#[derive(Debug, Clone)]
pub struct PrometheusMetric {
    /// Metric name (e.g., `wlm_performance_index`).
    pub name: String,
    /// Labels (e.g., `{"class": "ONLINE"}`).
    pub labels: HashMap<String, String>,
    /// Metric value.
    pub value: f64,
}

impl PrometheusMetric {
    /// Format as Prometheus text exposition format line.
    pub fn format(&self) -> String {
        if self.labels.is_empty() {
            format!("{} {}", self.name, self.value)
        } else {
            let labels: Vec<String> = self
                .labels
                .iter()
                .map(|(k, v)| format!("{k}=\"{v}\""))
                .collect();
            format!("{}{{{}}} {}", self.name, labels.join(","), self.value)
        }
    }
}

/// Generate Prometheus metrics from WLM state.
pub fn generate_metrics(response: &DisplayWlmResponse) -> Vec<PrometheusMetric> {
    let mut metrics = Vec::new();

    // Per-class PI metrics.
    for c in &response.classes {
        let mut labels = HashMap::new();
        labels.insert("class".to_string(), c.name.clone());
        metrics.push(PrometheusMetric {
            name: "wlm_performance_index".to_string(),
            labels: labels.clone(),
            value: c.pi,
        });
        metrics.push(PrometheusMetric {
            name: "wlm_active_work_units".to_string(),
            labels,
            value: f64::from(c.active_work_units),
        });
    }

    // Per-resource-group metrics.
    for rg in &response.resource_groups {
        let mut labels = HashMap::new();
        labels.insert("group".to_string(), rg.name.clone());
        metrics.push(PrometheusMetric {
            name: "wlm_resource_group_cpu_percent".to_string(),
            labels: labels.clone(),
            value: rg.cpu_percent,
        });
        metrics.push(PrometheusMetric {
            name: "wlm_resource_group_memory_mb".to_string(),
            labels: labels.clone(),
            value: rg.memory_mb as f64,
        });
        metrics.push(PrometheusMetric {
            name: "wlm_resource_group_throttled".to_string(),
            labels,
            value: if rg.throttled { 1.0 } else { 0.0 },
        });
    }

    // Global metrics.
    metrics.push(PrometheusMetric {
        name: "wlm_initiator_count".to_string(),
        labels: HashMap::new(),
        value: f64::from(response.initiator_count),
    });
    metrics.push(PrometheusMetric {
        name: "wlm_enclave_count".to_string(),
        labels: HashMap::new(),
        value: f64::from(response.enclave_count),
    });
    metrics.push(PrometheusMetric {
        name: "wlm_goal_mode".to_string(),
        labels: HashMap::new(),
        value: if response.mode == WlmMode::Goal {
            1.0
        } else {
            0.0
        },
    });

    metrics
}

/// Format all metrics as Prometheus text exposition.
pub fn format_prometheus(metrics: &[PrometheusMetric]) -> String {
    metrics
        .iter()
        .map(|m| m.format())
        .collect::<Vec<_>>()
        .join("\n")
}

// ─────────────────────── SMF Records ───────────────────────

/// SMF record type for WLM.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SmfWlmRecordType {
    /// Type 72 — workload activity.
    Type72,
    /// Type 99 — policy change.
    Type99,
}

/// A simplified SMF record for WLM.
#[derive(Debug, Clone)]
pub struct SmfWlmRecord {
    /// Record type.
    pub record_type: SmfWlmRecordType,
    /// Timestamp (epoch seconds).
    pub timestamp: f64,
    /// Record data.
    pub data: SmfWlmData,
}

/// SMF record data variants.
#[derive(Debug, Clone)]
pub enum SmfWlmData {
    /// Type 72 — workload activity interval data.
    WorkloadActivity {
        /// Policy name.
        policy_name: String,
        /// Per-class measurements.
        class_measurements: Vec<ClassMeasurement>,
    },
    /// Type 99 — policy change event.
    PolicyChange {
        /// Previous policy name.
        old_policy: Option<String>,
        /// New policy name.
        new_policy: String,
        /// Number of service classes.
        class_count: usize,
    },
}

/// A per-class measurement in an SMF Type 72 record.
#[derive(Debug, Clone)]
pub struct ClassMeasurement {
    /// Service class name.
    pub class_name: String,
    /// Average PI during the interval.
    pub avg_pi: f64,
    /// Maximum PI during the interval.
    pub max_pi: f64,
    /// Total service units consumed.
    pub service_units: u64,
    /// Work units completed during interval.
    pub completions: u32,
}

/// Generate an SMF Type 72 workload activity record.
pub fn generate_type72(
    policy_name: &str,
    measurements: Vec<ClassMeasurement>,
    timestamp: f64,
) -> SmfWlmRecord {
    SmfWlmRecord {
        record_type: SmfWlmRecordType::Type72,
        timestamp,
        data: SmfWlmData::WorkloadActivity {
            policy_name: policy_name.to_uppercase(),
            class_measurements: measurements,
        },
    }
}

/// Generate an SMF Type 99 policy change record.
pub fn generate_type99(
    old_policy: Option<&str>,
    new_policy: &str,
    class_count: usize,
    timestamp: f64,
) -> SmfWlmRecord {
    SmfWlmRecord {
        record_type: SmfWlmRecordType::Type99,
        timestamp,
        data: SmfWlmData::PolicyChange {
            old_policy: old_policy.map(|s| s.to_uppercase()),
            new_policy: new_policy.to_uppercase(),
            class_count,
        },
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn make_display_response() -> DisplayWlmResponse {
        DisplayWlmResponse {
            active_policy: Some("PROD".to_string()),
            mode: WlmMode::Goal,
            classes: vec![
                ClassStatus {
                    name: "ONLINE".to_string(),
                    pi: 0.8,
                    goal_description: "RT(200ms,95%)".to_string(),
                    active_work_units: 42,
                    importance: 1,
                },
                ClassStatus {
                    name: "BATCH".to_string(),
                    pi: 1.2,
                    goal_description: "VEL(50)".to_string(),
                    active_work_units: 15,
                    importance: 3,
                },
            ],
            resource_groups: vec![ResourceGroupStatus {
                name: "ONLINEPOOL".to_string(),
                cpu_percent: 35.2,
                memory_mb: 4096,
                throttled: false,
                cpu_cap: Some(40.0),
            }],
            initiator_count: 5,
            enclave_count: 3,
        }
    }

    // ─── WLM-109.1: DISPLAY WLM Command ───

    #[test]
    fn test_display_wlm_format() {
        let response = make_display_response();
        let output = response.format();
        assert!(output.contains("IWM029I  DISPLAY WLM"));
        assert!(output.contains("ACTIVE POLICY: PROD"));
        assert!(output.contains("MODE: GOAL"));
        assert!(output.contains("ONLINE"));
        assert!(output.contains("BATCH"));
        assert!(output.contains("ONLINEPOOL"));
        assert!(output.contains("INITIATORS: 5"));
        assert!(output.contains("ENCLAVES: 3"));
    }

    #[test]
    fn test_display_wlm_compat_mode() {
        let response = DisplayWlmResponse {
            active_policy: None,
            mode: WlmMode::Compat,
            classes: Vec::new(),
            resource_groups: Vec::new(),
            initiator_count: 0,
            enclave_count: 0,
        };
        let output = response.format();
        assert!(output.contains("NO ACTIVE POLICY (COMPAT MODE)"));
    }

    // ─── WLM-109.2: VARY WLM Command ───

    #[test]
    fn test_vary_wlm_policy() {
        let cmd = VaryWlmCommand::parse("VARY WLM,POLICY=PROD").unwrap();
        assert_eq!(cmd, VaryWlmCommand::Policy("PROD".to_string()));
    }

    #[test]
    fn test_vary_wlm_short_form() {
        let cmd = VaryWlmCommand::parse("V WLM,POLICY=NIGHTTIME").unwrap();
        assert_eq!(cmd, VaryWlmCommand::Policy("NIGHTTIME".to_string()));
    }

    #[test]
    fn test_vary_wlm_compat() {
        let cmd = VaryWlmCommand::parse("VARY WLM,COMPAT").unwrap();
        assert_eq!(cmd, VaryWlmCommand::Compat);
    }

    #[test]
    fn test_vary_wlm_case_insensitive() {
        let cmd = VaryWlmCommand::parse("vary wlm,policy=prod").unwrap();
        assert_eq!(cmd, VaryWlmCommand::Policy("PROD".to_string()));
    }

    #[test]
    fn test_vary_wlm_invalid() {
        assert!(VaryWlmCommand::parse("VARY WLM,NONSENSE").is_none());
        assert!(VaryWlmCommand::parse("VARY WLM,POLICY=").is_none());
        assert!(VaryWlmCommand::parse("VARY SOMETHING").is_none());
    }

    // ─── WLM-109.3: Prometheus Metrics ───

    #[test]
    fn test_prometheus_pi_metric() {
        let response = make_display_response();
        let metrics = generate_metrics(&response);

        let pi_metric = metrics
            .iter()
            .find(|m| {
                m.name == "wlm_performance_index"
                    && m.labels.get("class").map(String::as_str) == Some("ONLINE")
            })
            .unwrap();
        assert!((pi_metric.value - 0.8).abs() < f64::EPSILON);
    }

    #[test]
    fn test_prometheus_resource_group_metric() {
        let response = make_display_response();
        let metrics = generate_metrics(&response);

        let cpu = metrics
            .iter()
            .find(|m| {
                m.name == "wlm_resource_group_cpu_percent"
                    && m.labels.get("group").map(String::as_str) == Some("ONLINEPOOL")
            })
            .unwrap();
        assert!((cpu.value - 35.2).abs() < f64::EPSILON);
    }

    #[test]
    fn test_prometheus_format() {
        let response = make_display_response();
        let metrics = generate_metrics(&response);
        let output = format_prometheus(&metrics);
        assert!(output.contains("wlm_performance_index"));
        assert!(output.contains("wlm_goal_mode"));
        assert!(output.contains("wlm_initiator_count"));
    }

    #[test]
    fn test_prometheus_global_metrics() {
        let response = make_display_response();
        let metrics = generate_metrics(&response);

        let init = metrics
            .iter()
            .find(|m| m.name == "wlm_initiator_count")
            .unwrap();
        assert!((init.value - 5.0).abs() < f64::EPSILON);

        let goal = metrics
            .iter()
            .find(|m| m.name == "wlm_goal_mode")
            .unwrap();
        assert!((goal.value - 1.0).abs() < f64::EPSILON);
    }

    // ─── WLM-109.4: SMF Type 72/99 Records ───

    #[test]
    fn test_smf_type72() {
        let measurements = vec![
            ClassMeasurement {
                class_name: "ONLINE".to_string(),
                avg_pi: 0.85,
                max_pi: 1.1,
                service_units: 50000,
                completions: 120,
            },
            ClassMeasurement {
                class_name: "BATCH".to_string(),
                avg_pi: 1.3,
                max_pi: 2.0,
                service_units: 200000,
                completions: 8,
            },
        ];
        let record = generate_type72("PROD", measurements, 1000.0);
        assert_eq!(record.record_type, SmfWlmRecordType::Type72);
        match &record.data {
            SmfWlmData::WorkloadActivity {
                policy_name,
                class_measurements,
            } => {
                assert_eq!(policy_name, "PROD");
                assert_eq!(class_measurements.len(), 2);
                assert!((class_measurements[0].avg_pi - 0.85).abs() < f64::EPSILON);
            }
            _ => panic!("Expected WorkloadActivity"),
        }
    }

    #[test]
    fn test_smf_type99() {
        let record = generate_type99(Some("DAYTIME"), "NIGHTTIME", 4, 2000.0);
        assert_eq!(record.record_type, SmfWlmRecordType::Type99);
        match &record.data {
            SmfWlmData::PolicyChange {
                old_policy,
                new_policy,
                class_count,
            } => {
                assert_eq!(old_policy.as_deref(), Some("DAYTIME"));
                assert_eq!(new_policy, "NIGHTTIME");
                assert_eq!(*class_count, 4);
            }
            _ => panic!("Expected PolicyChange"),
        }
    }

    #[test]
    fn test_smf_type99_initial_activation() {
        let record = generate_type99(None, "PROD", 3, 100.0);
        match &record.data {
            SmfWlmData::PolicyChange { old_policy, .. } => {
                assert!(old_policy.is_none());
            }
            _ => panic!("Expected PolicyChange"),
        }
    }

    // ─── WLM-109.5: Monitoring Integration Tests ───

    #[test]
    fn test_full_monitoring_workflow() {
        // 1. Parse a VARY WLM command.
        let cmd = VaryWlmCommand::parse("VARY WLM,POLICY=PROD").unwrap();
        assert_eq!(cmd, VaryWlmCommand::Policy("PROD".to_string()));

        // 2. Generate policy change SMF record.
        let smf = generate_type99(None, "PROD", 3, 100.0);
        assert_eq!(smf.record_type, SmfWlmRecordType::Type99);

        // 3. Build display response.
        let response = make_display_response();

        // 4. Format display output.
        let display = response.format();
        assert!(display.contains("PROD"));

        // 5. Generate Prometheus metrics.
        let metrics = generate_metrics(&response);
        assert!(!metrics.is_empty());
        let output = format_prometheus(&metrics);
        assert!(output.contains("wlm_performance_index"));

        // 6. Generate interval SMF record.
        let type72 = generate_type72(
            "PROD",
            vec![ClassMeasurement {
                class_name: "ONLINE".to_string(),
                avg_pi: 0.9,
                max_pi: 1.0,
                service_units: 10000,
                completions: 50,
            }],
            200.0,
        );
        assert_eq!(type72.record_type, SmfWlmRecordType::Type72);
    }
}
