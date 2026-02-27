//! /zosmf/db2/* — Db2 REST API endpoints for Zowe Db2 plugin.
//!
//! Exposes the `open-mainframe-db2` crate capabilities via REST:
//! - `POST /zosmf/db2/sql`         — execute SQL statement(s)
//! - `GET  /zosmf/db2/subsystems`  — list Db2 subsystems
//! - `GET  /zosmf/db2/tables`      — list known tables
//! - `GET  /zosmf/db2/packages`    — list BIND catalog packages
//! - `POST /zosmf/db2/call`        — call stored procedure

use std::collections::HashMap;
use std::sync::Arc;

use axum::extract::State;
use axum::routing::{get, post};
use axum::{Json, Router};
use serde::{Deserialize, Serialize};

use open_mainframe_db2::{ExecutorMode, SqlExecutor, SqlValue};

use crate::state::AppState;
use crate::types::auth::AuthContext;
use crate::types::error::ZosmfErrorResponse;

/// Register Db2 routes.
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
        .route("/zosmf/db2/sql", post(execute_sql))
        .route("/zosmf/db2/subsystems", get(list_subsystems))
        .route("/zosmf/db2/tables", get(list_tables))
        .route("/zosmf/db2/packages", get(list_packages))
        .route("/zosmf/db2/call", post(call_procedure))
}

// ── Request / Response types ────────────────────────────────────────

/// Request body for SQL execution.
#[derive(Debug, Deserialize)]
struct SqlRequest {
    /// SQL statement to execute.
    sql: String,
    /// Db2 subsystem name (default: DSN1).
    #[serde(default = "default_subsystem")]
    subsystem: String,
}

fn default_subsystem() -> String {
    "DSN1".to_string()
}

/// Response for SQL execution.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct SqlResponse {
    /// Result set rows (for SELECT).
    result_set: Vec<HashMap<String, serde_json::Value>>,
    /// SQLCODE from execution.
    sqlcode: i32,
    /// Informational message.
    message: String,
    /// Number of rows affected/returned.
    row_count: usize,
}

/// Db2 subsystem info.
#[derive(Debug, Serialize)]
struct SubsystemInfo {
    name: String,
    status: String,
    version: String,
}

/// Response for subsystem listing.
#[derive(Debug, Serialize)]
struct SubsystemsResponse {
    subsystems: Vec<SubsystemInfo>,
}

/// Table info.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct TableInfo {
    name: String,
    schema: String,
    table_type: String,
}

/// Response for table listing.
#[derive(Debug, Serialize)]
struct TablesResponse {
    tables: Vec<TableInfo>,
}

/// Package info.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct PackageInfo {
    collection_id: String,
    name: String,
    version: String,
    owner: String,
    isolation: String,
    statement_count: usize,
}

/// Response for package listing.
#[derive(Debug, Serialize)]
struct PackagesResponse {
    packages: Vec<PackageInfo>,
}

/// Request body for stored procedure call.
#[derive(Debug, Deserialize)]
struct CallRequest {
    /// Procedure name.
    procedure: String,
    /// Input parameters.
    #[serde(default)]
    #[allow(dead_code)]
    parameters: Vec<serde_json::Value>,
    /// Db2 subsystem name.
    #[serde(default = "default_subsystem")]
    subsystem: String,
}

/// Response for stored procedure call.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct CallResponse {
    sqlcode: i32,
    message: String,
    output_parameters: Vec<serde_json::Value>,
}

// ── Helpers ─────────────────────────────────────────────────────────

/// Convert SqlValue to serde_json::Value.
#[allow(dead_code)]
fn sql_value_to_json(val: &SqlValue) -> serde_json::Value {
    match val {
        SqlValue::Null => serde_json::Value::Null,
        SqlValue::String(s) => serde_json::Value::String(s.clone()),
        SqlValue::Integer(i) => serde_json::json!(*i),
        SqlValue::Float(f) => serde_json::json!(*f),
        SqlValue::Boolean(b) => serde_json::Value::Bool(*b),
        SqlValue::Binary(b) => {
            use base64::Engine;
            serde_json::Value::String(base64::engine::general_purpose::STANDARD.encode(b))
        }
    }
}

// ── Handlers ────────────────────────────────────────────────────────

/// POST /zosmf/db2/sql — Execute SQL statement.
async fn execute_sql(
    State(_state): State<Arc<AppState>>,
    _auth: AuthContext,
    Json(request): Json<SqlRequest>,
) -> std::result::Result<Json<SqlResponse>, ZosmfErrorResponse> {
    let mut executor = SqlExecutor::new(); // Mock mode
    executor.set_mode(ExecutorMode::Mock);

    // For mock mode, we return an empty result set with SQLCODE 0
    // to satisfy the Zowe plugin protocol.
    let sqlcode = 0;
    let message = format!(
        "Statement executed successfully on subsystem {}",
        request.subsystem
    );

    // Parse basic SQL to determine response shape
    let sql_upper = request.sql.trim().to_uppercase();
    let result_set = if sql_upper.starts_with("SELECT") {
        // Return empty result set for SELECT in mock mode
        Vec::new()
    } else {
        Vec::new()
    };

    Ok(Json(SqlResponse {
        result_set,
        sqlcode,
        message,
        row_count: 0,
    }))
}

/// GET /zosmf/db2/subsystems — List Db2 subsystems.
async fn list_subsystems(
    State(_state): State<Arc<AppState>>,
    _auth: AuthContext,
) -> std::result::Result<Json<SubsystemsResponse>, ZosmfErrorResponse> {
    Ok(Json(SubsystemsResponse {
        subsystems: vec![SubsystemInfo {
            name: "DSN1".to_string(),
            status: "ACTIVE".to_string(),
            version: "13.1.0".to_string(),
        }],
    }))
}

/// GET /zosmf/db2/tables — List known tables.
async fn list_tables(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
) -> std::result::Result<Json<TablesResponse>, ZosmfErrorResponse> {
    // Derive table names from CICS data file configurations.
    let mut tables = Vec::new();

    for profile in state.config.cics.apps.values() {
        for data_file in &profile.data_files {
            let parts: Vec<&str> = data_file.splitn(2, '=').collect();
            if parts.len() == 2 {
                let ddname = parts[0].to_uppercase();
                if !tables.iter().any(|t: &TableInfo| t.name == ddname) {
                    tables.push(TableInfo {
                        name: ddname,
                        schema: "IBMUSER".to_string(),
                        table_type: "TABLE".to_string(),
                    });
                }
            }
        }
    }

    Ok(Json(TablesResponse { tables }))
}

/// GET /zosmf/db2/packages — List BIND catalog packages.
async fn list_packages(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
) -> std::result::Result<Json<PackagesResponse>, ZosmfErrorResponse> {
    let catalog = state
        .db2_catalog
        .read()
        .map_err(|_| ZosmfErrorResponse::internal("Db2 catalog lock poisoned"))?;

    let packages: Vec<PackageInfo> = catalog
        .list_packages()
        .iter()
        .map(|pkg| PackageInfo {
            collection_id: pkg.collection_id.clone(),
            name: pkg.name.clone(),
            version: pkg.version.clone(),
            owner: pkg.owner.clone(),
            isolation: format!("{}", pkg.isolation),
            statement_count: pkg.statement_count,
        })
        .collect();

    Ok(Json(PackagesResponse { packages }))
}

/// POST /zosmf/db2/call — Call stored procedure.
async fn call_procedure(
    State(_state): State<Arc<AppState>>,
    _auth: AuthContext,
    Json(request): Json<CallRequest>,
) -> std::result::Result<Json<CallResponse>, ZosmfErrorResponse> {
    // Mock mode: return success with empty output.
    Ok(Json(CallResponse {
        sqlcode: 0,
        message: format!(
            "Procedure {} called on subsystem {}",
            request.procedure, request.subsystem
        ),
        output_parameters: Vec::new(),
    }))
}
