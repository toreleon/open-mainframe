//! /zosmf/workflow/rest/1.0/* — z/OSMF Workflow REST API endpoints.
//!
//! Implements the real z/OSMF Workflow REST services:
//! - `POST   /zosmf/workflow/rest/1.0/workflows`        — create workflow
//! - `GET    /zosmf/workflow/rest/1.0/workflows`        — list workflows
//! - `GET    /zosmf/workflow/rest/1.0/workflows/{key}`  — get workflow details
//! - `DELETE /zosmf/workflow/rest/1.0/workflows/{key}`  — delete workflow

use std::sync::Arc;

use axum::extract::{Path, State};
use axum::http::StatusCode;
use axum::routing::{delete, get, post};
use axum::{Json, Router};
use serde::{Deserialize, Serialize};

use crate::state::{AppState, WorkflowInstance};
use crate::types::auth::AuthContext;
use crate::types::error::ZosmfErrorResponse;

/// Register workflow routes.
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
        .route("/zosmf/workflow/rest/1.0/workflows", post(create_workflow))
        .route("/zosmf/workflow/rest/1.0/workflows", get(list_workflows))
        .route(
            "/zosmf/workflow/rest/1.0/workflows/{key}",
            get(get_workflow),
        )
        .route(
            "/zosmf/workflow/rest/1.0/workflows/{key}",
            delete(delete_workflow),
        )
}

/// Request body for creating a workflow.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct CreateWorkflowRequest {
    workflow_name: String,
    #[serde(default)]
    workflow_description: Option<String>,
    workflow_definition_file: String,
    system: String,
    #[serde(default)]
    owner: Option<String>,
}

/// Response for workflow list.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct WorkflowListResponse {
    workflows: Vec<WorkflowInstance>,
    num_rows: usize,
}

/// POST /zosmf/workflow/rest/1.0/workflows — create a new workflow.
async fn create_workflow(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    Json(req): Json<CreateWorkflowRequest>,
) -> std::result::Result<(StatusCode, Json<WorkflowInstance>), ZosmfErrorResponse> {
    let key = format!("wf-{:08x}", {
        let mut hash: u32 = 0;
        for b in req.workflow_name.bytes() {
            hash = hash.wrapping_mul(31).wrapping_add(b as u32);
        }
        hash ^ (state.workflows.len() as u32)
    });

    let owner = req.owner.unwrap_or_else(|| auth.userid.clone());

    let instance = WorkflowInstance {
        workflow_key: key.clone(),
        workflow_name: req.workflow_name,
        workflow_description: req
            .workflow_description
            .unwrap_or_else(|| format!("Workflow from {}", req.workflow_definition_file)),
        owner,
        status: "in-progress".to_string(),
        created: chrono::Utc::now().format("%Y-%m-%dT%H:%M:%SZ").to_string(),
        system: req.system,
    };

    state.workflows.insert(key.clone(), instance.clone());

    tracing::info!(workflow_key = %key, workflow_name = %instance.workflow_name, "Workflow created");
    Ok((StatusCode::CREATED, Json(instance)))
}

/// GET /zosmf/workflow/rest/1.0/workflows — list all workflows.
async fn list_workflows(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
) -> Json<WorkflowListResponse> {
    let workflows: Vec<WorkflowInstance> = state
        .workflows
        .iter()
        .map(|entry| entry.value().clone())
        .collect();
    let num_rows = workflows.len();

    Json(WorkflowListResponse {
        workflows,
        num_rows,
    })
}

/// GET /zosmf/workflow/rest/1.0/workflows/:key — get workflow details.
async fn get_workflow(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(key): Path<String>,
) -> std::result::Result<Json<WorkflowInstance>, ZosmfErrorResponse> {
    state
        .workflows
        .get(&key)
        .map(|entry| Json(entry.value().clone()))
        .ok_or_else(|| {
            ZosmfErrorResponse::not_found(format!("Workflow '{}' not found", key))
        })
}

/// DELETE /zosmf/workflow/rest/1.0/workflows/:key — delete a workflow.
async fn delete_workflow(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(key): Path<String>,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    state
        .workflows
        .remove(&key)
        .map(|_| {
            tracing::info!(workflow_key = %key, "Workflow deleted");
            StatusCode::NO_CONTENT
        })
        .ok_or_else(|| {
            ZosmfErrorResponse::not_found(format!("Workflow '{}' not found", key))
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_workflow_instance_serialization() {
        let wf = WorkflowInstance {
            workflow_key: "wf-12345678".to_string(),
            workflow_name: "Configure DB2".to_string(),
            workflow_description: "Setup DB2 subsystem".to_string(),
            owner: "IBMUSER".to_string(),
            status: "in-progress".to_string(),
            created: "2025-01-15T10:30:00Z".to_string(),
            system: "SYS1".to_string(),
        };
        let json = serde_json::to_string(&wf).unwrap();
        assert!(json.contains("\"workflowKey\":\"wf-12345678\""));
        assert!(json.contains("\"workflowName\":\"Configure DB2\""));
        assert!(json.contains("\"owner\":\"IBMUSER\""));
        assert!(json.contains("\"status\":\"in-progress\""));
    }

    #[test]
    fn test_workflow_list_response() {
        let resp = WorkflowListResponse {
            workflows: vec![],
            num_rows: 0,
        };
        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"numRows\":0"));
        assert!(json.contains("\"workflows\":[]"));
    }

    #[test]
    fn test_create_workflow_request_deserialization() {
        let json = r#"{
            "workflowName": "Test Workflow",
            "workflowDefinitionFile": "/u/ibmuser/wf.xml",
            "system": "SYS1"
        }"#;
        let req: CreateWorkflowRequest = serde_json::from_str(json).unwrap();
        assert_eq!(req.workflow_name, "Test Workflow");
        assert_eq!(req.system, "SYS1");
        assert!(req.owner.is_none());
    }
}
