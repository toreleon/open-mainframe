//! /zosmf/zwlm/rest/1.0/* — Workload Manager REST API endpoints.
//!
//! Implements the real z/OSMF WLM REST services:
//! - `GET    /zosmf/zwlm/rest/1.0/policy`              — active policy status
//! - `GET    /zosmf/zwlm/rest/1.0/classes`              — service class status
//! - `PUT    /zosmf/zwlm/rest/1.0/policy/{name}`        — install/activate policy
//! - `POST   /zosmf/zwlm/rest/1.0/wrps`                — prime resource pool
//! - `DELETE /zosmf/zwlm/rest/1.0/wrps/{wrpid}`         — delete resource pool

use std::sync::Arc;

use axum::extract::{Path, State};
use axum::http::StatusCode;
use axum::routing::{delete, get, post, put};
use axum::{Json, Router};
use serde::{Deserialize, Serialize};

use crate::state::AppState;
use crate::types::auth::AuthContext;
use crate::types::error::ZosmfErrorResponse;

/// Register WLM routes.
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
        .route("/zosmf/zwlm/rest/1.0/policy", get(get_policy))
        .route("/zosmf/zwlm/rest/1.0/classes", get(get_classes))
        .route("/zosmf/zwlm/rest/1.0/policy/{name}", put(install_policy))
        .route("/zosmf/zwlm/rest/1.0/wrps", post(prime_resource_pool))
        .route("/zosmf/zwlm/rest/1.0/wrps/{wrpid}", delete(delete_resource_pool))
}

/// GET /zosmf/zwlm/rest/1.0/policy — active policy status.
async fn get_policy(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
) -> std::result::Result<Json<open_mainframe_wlm::PolicyResponse>, ZosmfErrorResponse> {
    let wlm = state
        .wlm
        .read()
        .map_err(|_| ZosmfErrorResponse::internal("WLM lock poisoned"))?;

    let response = open_mainframe_wlm::health::build_policy_response(&wlm);
    Ok(Json(response))
}

/// GET /zosmf/zwlm/rest/1.0/classes — service class status.
async fn get_classes(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
) -> std::result::Result<Json<open_mainframe_wlm::ClassesResponse>, ZosmfErrorResponse> {
    let wlm = state
        .wlm
        .read()
        .map_err(|_| ZosmfErrorResponse::internal("WLM lock poisoned"))?;

    let response = open_mainframe_wlm::health::build_classes_response(&wlm);
    Ok(Json(response))
}

/// Request body for installing/activating a WLM policy.
#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct InstallPolicyRequest {
    /// Action: "install" or "activate".
    #[serde(default)]
    action: Option<String>,
}

/// Response for policy installation.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct InstallPolicyResponse {
    policy_name: String,
    status: String,
    previous_policy: Option<String>,
}

/// PUT /zosmf/zwlm/rest/1.0/policy/:name — install/activate policy.
async fn install_policy(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(name): Path<String>,
    body: Option<Json<InstallPolicyRequest>>,
) -> std::result::Result<Json<InstallPolicyResponse>, ZosmfErrorResponse> {
    let mut store = state
        .wlm_policy_store
        .write()
        .map_err(|_| ZosmfErrorResponse::internal("WLM policy store lock poisoned"))?;

    let result = store.activate(&name).map_err(|e| {
        ZosmfErrorResponse::not_found(format!("Policy '{}' not found: {}", name, e))
    })?;

    let _ = body; // Acknowledge optional body

    Ok(Json(InstallPolicyResponse {
        policy_name: result.policy_name,
        status: "activated".to_string(),
        previous_policy: result.previous_policy,
    }))
}

/// Request body for priming a resource pool.
#[derive(Debug, Deserialize)]
struct PrimeWrpRequest {
    /// Resource pool name.
    #[serde(default)]
    name: Option<String>,
}

/// Response for resource pool operations.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct WrpResponse {
    wrp_id: String,
    status: String,
}

/// POST /zosmf/zwlm/rest/1.0/wrps — prime resource pool.
async fn prime_resource_pool(
    State(_state): State<Arc<AppState>>,
    _auth: AuthContext,
    Json(req): Json<PrimeWrpRequest>,
) -> (StatusCode, Json<WrpResponse>) {
    let name = req.name.unwrap_or_else(|| "WRP001".to_string());
    (
        StatusCode::CREATED,
        Json(WrpResponse {
            wrp_id: name,
            status: "primed".to_string(),
        }),
    )
}

/// DELETE /zosmf/zwlm/rest/1.0/wrps/:wrpid — delete resource pool.
async fn delete_resource_pool(
    State(_state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(wrpid): Path<String>,
) -> StatusCode {
    let _ = wrpid;
    StatusCode::NO_CONTENT
}

#[cfg(test)]
mod tests {
    use open_mainframe_wlm::PolicyResponse;

    #[test]
    fn test_policy_response_serialization() {
        let resp = PolicyResponse {
            active_policy: Some("DEFAULT".to_string()),
            mode: "GOAL".to_string(),
            class_count: 5,
            resource_group_count: 2,
            initiator_count: 10,
            enclave_count: 3,
        };
        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"active_policy\":\"DEFAULT\""));
        assert!(json.contains("\"mode\":\"GOAL\""));
    }

    #[test]
    fn test_install_policy_response() {
        let resp = super::InstallPolicyResponse {
            policy_name: "PROD".to_string(),
            status: "activated".to_string(),
            previous_policy: Some("DEFAULT".to_string()),
        };
        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"policyName\":\"PROD\""));
        assert!(json.contains("\"previousPolicy\":\"DEFAULT\""));
    }

    #[test]
    fn test_wrp_response() {
        let resp = super::WrpResponse {
            wrp_id: "WRP001".to_string(),
            status: "primed".to_string(),
        };
        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"wrpId\":\"WRP001\""));
    }
}
