//! /zosmf/provisioning/rest/1.0/* — z/OSMF Provisioning REST API endpoints.
//!
//! Implements the real z/OSMF Cloud Provisioning and Management REST services:
//! - `GET    /zosmf/provisioning/rest/1.0/psc`        — list published templates
//! - `POST   /zosmf/provisioning/rest/1.0/scr`        — provision an instance
//! - `GET    /zosmf/provisioning/rest/1.0/scr`        — list provisioned instances
//! - `GET    /zosmf/provisioning/rest/1.0/scr/{id}`   — get provisioned instance
//! - `DELETE /zosmf/provisioning/rest/1.0/scr/{id}`   — deprovision an instance

use std::sync::Arc;

use axum::extract::{Path, State};
use axum::http::StatusCode;
use axum::routing::{delete, get, post};
use axum::{Json, Router};
use serde::{Deserialize, Serialize};

use crate::state::{AppState, ProvisionedInstance, ProvisioningTemplate};
use crate::types::auth::AuthContext;
use crate::types::error::ZosmfErrorResponse;

/// Register provisioning routes.
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
        .route(
            "/zosmf/provisioning/rest/1.0/psc",
            get(list_templates),
        )
        .route(
            "/zosmf/provisioning/rest/1.0/scr",
            post(provision_instance),
        )
        .route(
            "/zosmf/provisioning/rest/1.0/scr",
            get(list_instances),
        )
        .route(
            "/zosmf/provisioning/rest/1.0/scr/{id}",
            get(get_instance),
        )
        .route(
            "/zosmf/provisioning/rest/1.0/scr/{id}",
            delete(deprovision_instance),
        )
}

/// Response for template list.
#[derive(Debug, Serialize)]
#[serde(rename_all = "kebab-case")]
struct TemplateListResponse {
    psc: Vec<ProvisioningTemplate>,
    num_rows: usize,
}

/// GET /zosmf/provisioning/rest/1.0/psc — list published software service templates.
async fn list_templates(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
) -> Json<TemplateListResponse> {
    let templates: Vec<ProvisioningTemplate> = state
        .provisioning_templates
        .iter()
        .map(|entry| entry.value().clone())
        .collect();
    let num_rows = templates.len();

    Json(TemplateListResponse {
        psc: templates,
        num_rows,
    })
}

/// Request body for provisioning an instance.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[allow(dead_code)]
struct ProvisionRequest {
    template_name: String,
    #[serde(default)]
    domain_name: Option<String>,
    #[serde(default)]
    tenant_name: Option<String>,
    #[serde(default)]
    system: Option<String>,
}

/// Response for provisioned instance list.
#[derive(Debug, Serialize)]
#[serde(rename_all = "kebab-case")]
struct InstanceListResponse {
    scr: Vec<ProvisionedInstance>,
    num_rows: usize,
}

/// POST /zosmf/provisioning/rest/1.0/scr — provision a new instance.
async fn provision_instance(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    Json(req): Json<ProvisionRequest>,
) -> std::result::Result<(StatusCode, Json<ProvisionedInstance>), ZosmfErrorResponse> {
    let id = format!("scr-{:08x}", {
        let mut hash: u32 = 0;
        for b in req.template_name.bytes() {
            hash = hash.wrapping_mul(31).wrapping_add(b as u32);
        }
        hash ^ (state.provisioning_instances.len() as u32)
    });

    let system = req.system.unwrap_or_else(|| "SYS1".to_string());

    let instance = ProvisionedInstance {
        object_id: id.clone(),
        name: format!("{}-instance", req.template_name),
        template_name: req.template_name,
        state: "being-provisioned".to_string(),
        owner: auth.userid.clone(),
        created: chrono::Utc::now().format("%Y-%m-%dT%H:%M:%SZ").to_string(),
        system,
    };

    state
        .provisioning_instances
        .insert(id.clone(), instance.clone());

    tracing::info!(instance_id = %id, template = %instance.template_name, "Instance provisioned");
    Ok((StatusCode::CREATED, Json(instance)))
}

/// GET /zosmf/provisioning/rest/1.0/scr — list provisioned instances.
async fn list_instances(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
) -> Json<InstanceListResponse> {
    let instances: Vec<ProvisionedInstance> = state
        .provisioning_instances
        .iter()
        .map(|entry| entry.value().clone())
        .collect();
    let num_rows = instances.len();

    Json(InstanceListResponse {
        scr: instances,
        num_rows,
    })
}

/// GET /zosmf/provisioning/rest/1.0/scr/:id — get provisioned instance details.
async fn get_instance(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(id): Path<String>,
) -> std::result::Result<Json<ProvisionedInstance>, ZosmfErrorResponse> {
    state
        .provisioning_instances
        .get(&id)
        .map(|entry| Json(entry.value().clone()))
        .ok_or_else(|| {
            ZosmfErrorResponse::not_found(format!(
                "Provisioned instance '{}' not found",
                id
            ))
        })
}

/// DELETE /zosmf/provisioning/rest/1.0/scr/:id — deprovision an instance.
async fn deprovision_instance(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(id): Path<String>,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    state
        .provisioning_instances
        .remove(&id)
        .map(|_| {
            tracing::info!(instance_id = %id, "Instance deprovisioned");
            StatusCode::NO_CONTENT
        })
        .ok_or_else(|| {
            ZosmfErrorResponse::not_found(format!(
                "Provisioned instance '{}' not found",
                id
            ))
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_template_serialization() {
        let tmpl = ProvisioningTemplate {
            name: "DB2 Subsystem".to_string(),
            description: "Provision a DB2 subsystem".to_string(),
            version: "1.0".to_string(),
            domain_name: "default".to_string(),
            state: "published".to_string(),
        };
        let json = serde_json::to_string(&tmpl).unwrap();
        assert!(json.contains("\"name\":\"DB2 Subsystem\""));
        assert!(json.contains("\"state\":\"published\""));
    }

    #[test]
    fn test_instance_serialization() {
        let inst = ProvisionedInstance {
            object_id: "scr-12345678".to_string(),
            name: "db2-instance".to_string(),
            template_name: "DB2 Subsystem".to_string(),
            state: "provisioned".to_string(),
            owner: "IBMUSER".to_string(),
            created: "2025-01-15T10:30:00Z".to_string(),
            system: "SYS1".to_string(),
        };
        let json = serde_json::to_string(&inst).unwrap();
        assert!(json.contains("\"object-id\":\"scr-12345678\""));
        assert!(json.contains("\"template-name\":\"DB2 Subsystem\""));
    }

    #[test]
    fn test_template_list_response() {
        let resp = TemplateListResponse {
            psc: vec![],
            num_rows: 0,
        };
        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"num-rows\":0"));
        assert!(json.contains("\"psc\":[]"));
    }

    #[test]
    fn test_provision_request_deserialization() {
        let json = r#"{
            "template-name": "CICS TS",
            "domain-name": "default"
        }"#;
        let req: ProvisionRequest = serde_json::from_str(json).unwrap();
        assert_eq!(req.template_name, "CICS TS");
        assert_eq!(req.domain_name.unwrap(), "default");
    }
}
