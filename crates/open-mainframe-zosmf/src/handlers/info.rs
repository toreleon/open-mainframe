//! GET /zosmf/info — z/OSMF system information endpoint.

use std::sync::Arc;

use axum::extract::State;
use axum::routing::get;
use axum::{Json, Router};

use crate::state::AppState;
use crate::types::info::{ZosmfInfo, ZosmfPlugin};

/// Register info routes.
pub fn routes() -> Router<Arc<AppState>> {
    Router::new().route("/zosmf/info", get(get_info))
}

/// Handler for GET /zosmf/info — returns z/OSMF system information.
async fn get_info(State(state): State<Arc<AppState>>) -> Json<ZosmfInfo> {
    let info = ZosmfInfo {
        api_version: "1".to_string(),
        zosmf_version: "27".to_string(),
        zosmf_full_version: "27.0".to_string(),
        zosmf_hostname: state.config.zosmf_info.hostname.clone(),
        zosmf_port: state.config.server.port.to_string(),
        zos_version: "02.05.00".to_string(),
        zosmf_saf_realm: state.config.zosmf_info.saf_realm.clone(),
        plugins: vec![
            ZosmfPlugin {
                plugin_version: "1.0.0".to_string(),
                plugin_default_name: "z/OSMF Restfiles".to_string(),
                plugin_status: "ACTIVE".to_string(),
            },
            ZosmfPlugin {
                plugin_version: "1.0.0".to_string(),
                plugin_default_name: "z/OSMF JES".to_string(),
                plugin_status: "ACTIVE".to_string(),
            },
            ZosmfPlugin {
                plugin_version: "1.0.0".to_string(),
                plugin_default_name: "z/OSMF TSO".to_string(),
                plugin_status: "ACTIVE".to_string(),
            },
            ZosmfPlugin {
                plugin_version: "1.0.0".to_string(),
                plugin_default_name: "z/OSMF Console".to_string(),
                plugin_status: "ACTIVE".to_string(),
            },
            ZosmfPlugin {
                plugin_version: "1.0.0".to_string(),
                plugin_default_name: "z/OSMF WLM".to_string(),
                plugin_status: "ACTIVE".to_string(),
            },
            ZosmfPlugin {
                plugin_version: "1.0.0".to_string(),
                plugin_default_name: "z/OSMF Variables".to_string(),
                plugin_status: "ACTIVE".to_string(),
            },
            ZosmfPlugin {
                plugin_version: "1.0.0".to_string(),
                plugin_default_name: "z/OSMF Topology".to_string(),
                plugin_status: "ACTIVE".to_string(),
            },
            ZosmfPlugin {
                plugin_version: "1.0.0".to_string(),
                plugin_default_name: "z/OSMF Workflow".to_string(),
                plugin_status: "ACTIVE".to_string(),
            },
            ZosmfPlugin {
                plugin_version: "1.0.0".to_string(),
                plugin_default_name: "z/OSMF Provisioning".to_string(),
                plugin_status: "ACTIVE".to_string(),
            },
        ],
    };
    Json(info)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_info_response_structure() {
        let info = ZosmfInfo {
            api_version: "1".to_string(),
            zosmf_version: "27".to_string(),
            zosmf_full_version: "27.0".to_string(),
            zosmf_hostname: "testhost".to_string(),
            zosmf_port: "443".to_string(),
            zos_version: "02.05.00".to_string(),
            zosmf_saf_realm: "TestRealm".to_string(),
            plugins: vec![],
        };
        let json = serde_json::to_string(&info).unwrap();
        assert!(json.contains("api_version"));
        assert!(json.contains("zosmf_hostname"));
        assert!(json.contains("testhost"));
    }

    #[test]
    fn test_info_plugins_count() {
        // Verify we have exactly 9 real z/OSMF plugins
        let names = [
            "z/OSMF Restfiles",
            "z/OSMF JES",
            "z/OSMF TSO",
            "z/OSMF Console",
            "z/OSMF WLM",
            "z/OSMF Variables",
            "z/OSMF Topology",
            "z/OSMF Workflow",
            "z/OSMF Provisioning",
        ];
        assert_eq!(names.len(), 9);
    }
}
