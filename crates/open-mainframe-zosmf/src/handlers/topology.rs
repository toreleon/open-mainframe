//! /zosmf/resttopology/* — z/OS system topology REST API endpoints.
//!
//! Implements the real z/OSMF Topology REST services:
//! - `GET /zosmf/resttopology/systems`           — list z/OS systems
//! - `GET /zosmf/resttopology/systems/{sysname}` — system details

use std::sync::Arc;

use axum::extract::{Path, State};
use axum::routing::get;
use axum::{Json, Router};
use serde::Serialize;

use crate::state::AppState;
use crate::sysplex::SystemInstance;
use crate::types::auth::AuthContext;
use crate::types::error::ZosmfErrorResponse;

/// Register topology routes.
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
        .route("/zosmf/resttopology/systems", get(list_systems))
        .route(
            "/zosmf/resttopology/systems/{sysname}",
            get(get_system),
        )
}

/// A z/OS system entry in topology responses (matches real z/OSMF format).
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
struct TopologySystem {
    system_nick_name: String,
    group_names: String,
    url: String,
    sysplex: String,
    #[serde(rename = "zosVR")]
    zos_vr: String,
    #[serde(rename = "jesType")]
    jes_type: String,
    sysname: String,
    status: String,
}

/// Response for listing all systems.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct TopologyListResponse {
    items: Vec<TopologySystem>,
    num_rows: usize,
}

/// Convert a SystemInstance to a TopologySystem for API response.
fn system_to_topology(sys: &SystemInstance, state: &AppState) -> TopologySystem {
    let hostname = &state.config.zosmf_info.hostname;
    let port = state.config.server.port;
    let sysplex_name = state
        .sysplex
        .read()
        .map(|s| s.name.clone())
        .unwrap_or_else(|_| "LOCAL".to_string());

    TopologySystem {
        system_nick_name: sys.sysname.clone(),
        group_names: "SYSPLEX".to_string(),
        url: format!("https://{}:{}/zosmf", hostname, port),
        sysplex: sysplex_name,
        zos_vr: sys.zos_vr.clone(),
        jes_type: sys.jes_type.clone(),
        sysname: sys.sysname.clone(),
        status: sys.status.clone(),
    }
}

/// GET /zosmf/resttopology/systems — list z/OS systems in the sysplex.
async fn list_systems(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
) -> Json<TopologyListResponse> {
    let sysplex = state.sysplex.read().unwrap();
    let items: Vec<TopologySystem> = sysplex
        .list_systems()
        .iter()
        .map(|sys| system_to_topology(sys, &state))
        .collect();
    let num_rows = items.len();

    Json(TopologyListResponse { items, num_rows })
}

/// GET /zosmf/resttopology/systems/:sysname — get details for a specific system.
async fn get_system(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(sysname): Path<String>,
) -> std::result::Result<Json<TopologySystem>, ZosmfErrorResponse> {
    let sysplex = state.sysplex.read().map_err(|_| {
        ZosmfErrorResponse::internal("Sysplex lock poisoned")
    })?;

    if let Some(sys) = sysplex.get_system(&sysname) {
        Ok(Json(system_to_topology(sys, &state)))
    } else {
        Err(ZosmfErrorResponse::not_found(format!(
            "System '{}' not found in topology",
            sysname.to_uppercase()
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_topology_system_serialization() {
        let system = TopologySystem {
            system_nick_name: "SYS1".to_string(),
            group_names: "SYSPLEX".to_string(),
            url: "https://localhost:10443/zosmf".to_string(),
            sysplex: "LOCAL".to_string(),
            zos_vr: "V2R5".to_string(),
            jes_type: "JES2".to_string(),
            sysname: "SYS1".to_string(),
            status: "active".to_string(),
        };
        let json = serde_json::to_string(&system).unwrap();
        assert!(json.contains("\"systemNickName\":\"SYS1\""));
        assert!(json.contains("\"jesType\":\"JES2\""));
        assert!(json.contains("\"zosVR\":\"V2R5\""));
        assert!(json.contains("\"sysplex\":\"LOCAL\""));
    }

    #[test]
    fn test_topology_list_response() {
        let resp = TopologyListResponse {
            items: vec![],
            num_rows: 0,
        };
        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"numRows\":0"));
        assert!(json.contains("\"items\":[]"));
    }
}
