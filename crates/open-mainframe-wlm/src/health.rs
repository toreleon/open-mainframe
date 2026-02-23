//! # WLM Health API
//!
//! REST/JSON-style API responses for monitoring WLM state programmatically.
//!
//! Provides serializable response structs for:
//! - `GET /api/wlm/policy` — active policy query
//! - `GET /api/wlm/classes` — service class status
//! - `GET /api/wlm/resource-groups` — resource group status
//! - `GET /api/wlm/initiators` — initiator status
//! - `GET /api/wlm/enclaves` — enclave summary

use std::collections::HashMap;

use crate::operator::DisplayWlmResponse;

// ─────────────────────── API Responses ───────────────────────

/// Response for GET /api/wlm/policy.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PolicyResponse {
    /// Active policy name, or null if COMPAT mode.
    pub active_policy: Option<String>,
    /// WLM mode ("GOAL" or "COMPAT").
    pub mode: String,
    /// Number of service classes.
    pub class_count: usize,
    /// Number of resource groups.
    pub resource_group_count: usize,
    /// Number of managed initiators.
    pub initiator_count: u32,
    /// Number of active enclaves.
    pub enclave_count: u32,
}

/// A service class entry in the classes API response.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ClassResponse {
    /// Service class name.
    pub name: String,
    /// Goal description (e.g., "ResponseTime(200ms,95%)").
    pub goal: String,
    /// Current performance index.
    pub performance_index: f64,
    /// Number of active work units.
    pub work_count: u32,
    /// Importance level (1-5).
    pub importance: u8,
}

/// Response for GET /api/wlm/classes.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ClassesResponse {
    /// List of service class statuses.
    pub classes: Vec<ClassResponse>,
}

/// A resource group entry in the API response.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ResourceGroupResponse {
    /// Resource group name.
    pub name: String,
    /// CPU cap percentage, or null if uncapped.
    pub cpu_cap: Option<f64>,
    /// Current CPU usage percentage.
    pub cpu_usage: f64,
    /// Memory usage in MB.
    pub memory_mb: u64,
    /// Whether the group is currently throttled.
    pub throttled: bool,
}

/// Response for GET /api/wlm/resource-groups.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ResourceGroupsResponse {
    /// List of resource group statuses.
    pub groups: Vec<ResourceGroupResponse>,
}

/// Response for GET /api/wlm/initiators.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct InitiatorResponse {
    /// Total initiator count.
    pub total: u32,
    /// Active (running a job) count.
    pub active: u32,
    /// Idle count.
    pub idle: u32,
    /// Starting count.
    pub starting: u32,
    /// Draining count.
    pub draining: u32,
}

/// Response for GET /api/wlm/enclaves.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct EnclaveResponse {
    /// Total active enclave count.
    pub active_count: u32,
    /// Active enclaves by subsystem type.
    pub by_subsystem: HashMap<String, u32>,
}

// ─────────────────────── Builder from DisplayWlmResponse ───────────────────────

/// Build a PolicyResponse from a DisplayWlmResponse.
pub fn build_policy_response(d: &DisplayWlmResponse) -> PolicyResponse {
    PolicyResponse {
        active_policy: d.active_policy.clone(),
        mode: d.mode.as_str().to_string(),
        class_count: d.classes.len(),
        resource_group_count: d.resource_groups.len(),
        initiator_count: d.initiator_count,
        enclave_count: d.enclave_count,
    }
}

/// Build a ClassesResponse from a DisplayWlmResponse.
pub fn build_classes_response(d: &DisplayWlmResponse) -> ClassesResponse {
    ClassesResponse {
        classes: d
            .classes
            .iter()
            .map(|c| ClassResponse {
                name: c.name.clone(),
                goal: c.goal_description.clone(),
                performance_index: c.pi,
                work_count: c.active_work_units,
                importance: c.importance,
            })
            .collect(),
    }
}

/// Build a ResourceGroupsResponse from a DisplayWlmResponse.
pub fn build_resource_groups_response(d: &DisplayWlmResponse) -> ResourceGroupsResponse {
    ResourceGroupsResponse {
        groups: d
            .resource_groups
            .iter()
            .map(|rg| ResourceGroupResponse {
                name: rg.name.clone(),
                cpu_cap: rg.cpu_cap,
                cpu_usage: rg.cpu_percent,
                memory_mb: rg.memory_mb,
                throttled: rg.throttled,
            })
            .collect(),
    }
}

/// Build an InitiatorResponse from component counts.
pub fn build_initiator_response(
    total: u32,
    active: u32,
    idle: u32,
    starting: u32,
    draining: u32,
) -> InitiatorResponse {
    InitiatorResponse {
        total,
        active,
        idle,
        starting,
        draining,
    }
}

/// Build an EnclaveResponse.
pub fn build_enclave_response(
    active_count: u32,
    by_subsystem: HashMap<String, u32>,
) -> EnclaveResponse {
    EnclaveResponse {
        active_count,
        by_subsystem,
    }
}

// ─────────────────────── JSON Serialization ───────────────────────

/// Serialize any response to JSON.
pub fn to_json<T: serde::Serialize>(response: &T) -> Result<String, String> {
    serde_json::to_string_pretty(response).map_err(|e| format!("JSON serialization error: {e}"))
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::operator::{ClassStatus, ResourceGroupStatus};

    fn make_display() -> DisplayWlmResponse {
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
                ClassStatus {
                    name: "TSO".to_string(),
                    pi: 0.5,
                    goal_description: "RT(1s,90%)".to_string(),
                    active_work_units: 8,
                    importance: 2,
                },
            ],
            resource_groups: vec![
                ResourceGroupStatus {
                    name: "ONLINEPOOL".to_string(),
                    cpu_percent: 35.2,
                    memory_mb: 4096,
                    throttled: false,
                    cpu_cap: Some(40.0),
                },
                ResourceGroupStatus {
                    name: "BATCHPOOL".to_string(),
                    cpu_percent: 28.0,
                    memory_mb: 2048,
                    throttled: false,
                    cpu_cap: Some(30.0),
                },
            ],
            initiator_count: 5,
            enclave_count: 3,
        }
    }

    // ─── WLM-110.1: Active Policy Query ───

    #[test]
    fn test_policy_response() {
        let d = make_display();
        let resp = build_policy_response(&d);
        assert_eq!(resp.active_policy, Some("PROD".to_string()));
        assert_eq!(resp.mode, "GOAL");
        assert_eq!(resp.class_count, 3);
        assert_eq!(resp.resource_group_count, 2);
    }

    #[test]
    fn test_policy_response_json() {
        let d = make_display();
        let resp = build_policy_response(&d);
        let json = to_json(&resp).unwrap();
        assert!(json.contains("\"active_policy\": \"PROD\""));
        assert!(json.contains("\"mode\": \"GOAL\""));

        // Roundtrip.
        let parsed: PolicyResponse = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.class_count, 3);
    }

    #[test]
    fn test_policy_response_compat() {
        let d = DisplayWlmResponse {
            active_policy: None,
            mode: WlmMode::Compat,
            classes: Vec::new(),
            resource_groups: Vec::new(),
            initiator_count: 0,
            enclave_count: 0,
        };
        let resp = build_policy_response(&d);
        assert!(resp.active_policy.is_none());
        assert_eq!(resp.mode, "COMPAT");
    }

    // ─── WLM-110.2: Service Class Status ───

    #[test]
    fn test_classes_response() {
        let d = make_display();
        let resp = build_classes_response(&d);
        assert_eq!(resp.classes.len(), 3);
        assert_eq!(resp.classes[0].name, "ONLINE");
        assert!((resp.classes[0].performance_index - 0.8).abs() < f64::EPSILON);
        assert_eq!(resp.classes[0].work_count, 42);
    }

    #[test]
    fn test_classes_response_json() {
        let d = make_display();
        let resp = build_classes_response(&d);
        let json = to_json(&resp).unwrap();
        assert!(json.contains("\"name\": \"ONLINE\""));
        assert!(json.contains("\"performance_index\""));

        let parsed: ClassesResponse = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.classes.len(), 3);
    }

    // ─── WLM-110.3: Resource Group Status ───

    #[test]
    fn test_resource_groups_response() {
        let d = make_display();
        let resp = build_resource_groups_response(&d);
        assert_eq!(resp.groups.len(), 2);
        assert_eq!(resp.groups[0].name, "ONLINEPOOL");
        assert_eq!(resp.groups[0].cpu_cap, Some(40.0));
        assert!(!resp.groups[0].throttled);
    }

    #[test]
    fn test_resource_groups_response_json() {
        let d = make_display();
        let resp = build_resource_groups_response(&d);
        let json = to_json(&resp).unwrap();
        assert!(json.contains("\"cpu_cap\": 40.0"));
        assert!(json.contains("BATCHPOOL"));

        let parsed: ResourceGroupsResponse = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.groups.len(), 2);
    }

    // ─── WLM-110.4: Initiator Status ───

    #[test]
    fn test_initiator_response() {
        let resp = build_initiator_response(10, 6, 3, 1, 0);
        assert_eq!(resp.total, 10);
        assert_eq!(resp.active, 6);
        assert_eq!(resp.idle, 3);
        assert_eq!(resp.starting, 1);
        assert_eq!(resp.draining, 0);
    }

    #[test]
    fn test_initiator_response_json() {
        let resp = build_initiator_response(10, 6, 3, 1, 0);
        let json = to_json(&resp).unwrap();
        assert!(json.contains("\"total\": 10"));
        assert!(json.contains("\"active\": 6"));

        let parsed: InitiatorResponse = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.total, 10);
    }

    // ─── WLM-110.5: Enclave Summary ───

    #[test]
    fn test_enclave_response() {
        let mut by_sub = HashMap::new();
        by_sub.insert("DB2".to_string(), 2);
        by_sub.insert("CICS".to_string(), 1);

        let resp = build_enclave_response(3, by_sub);
        assert_eq!(resp.active_count, 3);
        assert_eq!(resp.by_subsystem.get("DB2"), Some(&2));
    }

    #[test]
    fn test_enclave_response_json() {
        let mut by_sub = HashMap::new();
        by_sub.insert("DB2".to_string(), 2);

        let resp = build_enclave_response(2, by_sub);
        let json = to_json(&resp).unwrap();
        assert!(json.contains("\"active_count\": 2"));
        assert!(json.contains("DB2"));

        let parsed: EnclaveResponse = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.active_count, 2);
    }

    // ─── WLM-110.6: Health API Integration Tests ───

    #[test]
    fn test_full_health_api_workflow() {
        let d = make_display();

        // Build all responses.
        let policy = build_policy_response(&d);
        let classes = build_classes_response(&d);
        let groups = build_resource_groups_response(&d);
        let initiators = build_initiator_response(5, 3, 2, 0, 0);

        let mut by_sub = HashMap::new();
        by_sub.insert("CICS".to_string(), 2);
        by_sub.insert("DB2".to_string(), 1);
        let enclaves = build_enclave_response(3, by_sub);

        // Serialize all to JSON.
        let policy_json = to_json(&policy).unwrap();
        let classes_json = to_json(&classes).unwrap();
        let groups_json = to_json(&groups).unwrap();
        let init_json = to_json(&initiators).unwrap();
        let enc_json = to_json(&enclaves).unwrap();

        // Verify all roundtrip.
        let _: PolicyResponse = serde_json::from_str(&policy_json).unwrap();
        let _: ClassesResponse = serde_json::from_str(&classes_json).unwrap();
        let _: ResourceGroupsResponse = serde_json::from_str(&groups_json).unwrap();
        let _: InitiatorResponse = serde_json::from_str(&init_json).unwrap();
        let _: EnclaveResponse = serde_json::from_str(&enc_json).unwrap();

        // Verify data integrity.
        assert_eq!(policy.class_count, classes.classes.len());
        assert_eq!(policy.resource_group_count, groups.groups.len());
        assert_eq!(policy.initiator_count, initiators.total);
        assert_eq!(policy.enclave_count, enclaves.active_count);
    }

    #[test]
    fn test_empty_system() {
        let d = DisplayWlmResponse {
            active_policy: None,
            mode: WlmMode::Compat,
            classes: Vec::new(),
            resource_groups: Vec::new(),
            initiator_count: 0,
            enclave_count: 0,
        };

        let policy = build_policy_response(&d);
        let classes = build_classes_response(&d);
        let groups = build_resource_groups_response(&d);

        assert!(policy.active_policy.is_none());
        assert!(classes.classes.is_empty());
        assert!(groups.groups.is_empty());

        // All serialize fine.
        to_json(&policy).unwrap();
        to_json(&classes).unwrap();
        to_json(&groups).unwrap();
    }
}
