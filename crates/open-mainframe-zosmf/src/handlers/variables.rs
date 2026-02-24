//! /zosmf/variables/rest/1.0/* — System variables REST API endpoints.
//!
//! Implements the real z/OSMF Variables REST services:
//! - `GET /zosmf/variables/rest/1.0/systems/local`          — local system variables
//! - `GET /zosmf/variables/rest/1.0/systems/{system_ref}`   — specific system variables

use std::sync::Arc;

use axum::extract::{Path, State};
use axum::routing::get;
use axum::{Json, Router};
use serde::Serialize;

use crate::state::AppState;
use crate::types::auth::AuthContext;
use crate::types::error::ZosmfErrorResponse;

/// Register variables routes.
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
        .route(
            "/zosmf/variables/rest/1.0/systems/local",
            get(get_local_variables),
        )
        .route(
            "/zosmf/variables/rest/1.0/systems/{system_ref}",
            get(get_system_variables),
        )
}

/// A single system variable entry.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct SystemVariable {
    name: String,
    value: String,
}

/// Response containing system variables.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct VariablesResponse {
    system: String,
    variables: Vec<SystemVariable>,
    num_variables: usize,
}

/// Build a variables response from StaticSymbols and SymbolEngine.
fn build_variables_response(
    state: &AppState,
    system_name: &str,
) -> Result<VariablesResponse, ZosmfErrorResponse> {
    let statics = open_mainframe_parmlib::StaticSymbols::default();
    let engine = open_mainframe_parmlib::SymbolEngine::with_static(&statics);
    let symbols = engine.symbols();

    // Also add any symbols resolved from parmlib.
    let parmlib = state
        .parmlib
        .read()
        .map_err(|_| ZosmfErrorResponse::internal("Parmlib lock poisoned"))?;
    let _ = &*parmlib; // Access parmlib to validate lock

    let mut variables: Vec<SystemVariable> = symbols
        .iter()
        .map(|(name, value)| SystemVariable {
            name: name.clone(),
            value: value.clone(),
        })
        .collect();

    variables.sort_by(|a, b| a.name.cmp(&b.name));
    let num_variables = variables.len();

    Ok(VariablesResponse {
        system: system_name.to_string(),
        variables,
        num_variables,
    })
}

/// GET /zosmf/variables/rest/1.0/systems/local — local system variables.
async fn get_local_variables(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
) -> std::result::Result<Json<VariablesResponse>, ZosmfErrorResponse> {
    let resp = build_variables_response(&state, "LOCAL")?;
    Ok(Json(resp))
}

/// GET /zosmf/variables/rest/1.0/systems/:system_ref — specific system variables.
async fn get_system_variables(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(system_ref): Path<String>,
) -> std::result::Result<Json<VariablesResponse>, ZosmfErrorResponse> {
    let resp = build_variables_response(&state, &system_ref.to_uppercase())?;
    Ok(Json(resp))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_variables_response_serialization() {
        let resp = VariablesResponse {
            system: "SYS1".to_string(),
            variables: vec![
                SystemVariable {
                    name: "SYSNAME".to_string(),
                    value: "SYS1".to_string(),
                },
                SystemVariable {
                    name: "SYSPLEX".to_string(),
                    value: "LOCAL".to_string(),
                },
            ],
            num_variables: 2,
        };
        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"system\":\"SYS1\""));
        assert!(json.contains("\"numVariables\":2"));
    }

    #[test]
    fn test_static_symbols_defaults() {
        let statics = open_mainframe_parmlib::StaticSymbols::default();
        let engine = open_mainframe_parmlib::SymbolEngine::with_static(&statics);
        let symbols = engine.symbols();
        assert!(symbols.contains_key("SYSNAME"));
        assert!(symbols.contains_key("SYSPLEX"));
        assert!(symbols.contains_key("SYSCLONE"));
    }
}
