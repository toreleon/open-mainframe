//! /CICSSystemManagement/* — CMCI (CICS Management Client Interface) REST API endpoints.
//!
//! Implements the CMCI REST API used by `@zowe/cics-for-zowe-cli`.
//! The Zowe CICS plugin sends HTTP requests to:
//!   `{scheme}://{host}:{port}/CICSSystemManagement/{resourceType}/{CICSplexName}/{regionName}`
//!
//! Supported resource types:
//! - `CICSProgram` — installed programs (GET list, PUT NEWCOPY)
//! - `CICSLocalTransaction` — installed transactions (GET list)
//! - `CICSLocalFile` — installed files (GET list, PUT open/close)
//! - `CICSTask` — active tasks (GET list)
//! - `CICSDefinitionProgram` — program definitions (GET, POST create, DELETE)
//! - `CICSDefinitionTransaction` — transaction definitions (GET, POST create, DELETE)
//! - `CICSDefinitionFile` — file definitions (GET)

use std::sync::Arc;

use axum::extract::{Path, Query, State};
use axum::http::{HeaderMap, StatusCode};
use axum::response::IntoResponse;
use axum::routing::get;
use axum::Router;
use serde::Deserialize;

use crate::state::AppState;
use crate::types::auth::AuthContext;
use crate::types::error::ZosmfErrorResponse;

/// CMCI XML namespace.
const CMCI_NS: &str = "http://www.ibm.com/xmlns/prod/CICS/smw2int";

/// Register CMCI routes.
pub fn routes() -> Router<Arc<AppState>> {
    Router::new().route(
        "/CICSSystemManagement/{resource_type}/{cics_plex}/{region}",
        get(cmci_get)
            .post(cmci_post)
            .put(cmci_put)
            .delete(cmci_delete),
    )
}

/// CMCI query parameters.
#[derive(Debug, Default, Deserialize)]
#[allow(non_snake_case)]
struct CmciQuery {
    /// CMCI CRITERIA filter expression, e.g. `PROGRAM=COSGN*`.
    CRITERIA: Option<String>,
    /// CMCI PARAMETER expression, e.g. `CSDGROUP(MYGROUP)`.
    #[allow(dead_code)]
    PARAMETER: Option<String>,
}

/// Path parameters for CMCI routes.
#[derive(Debug, Deserialize)]
struct CmciPath {
    resource_type: String,
    #[allow(dead_code)]
    cics_plex: String,
    #[allow(dead_code)]
    region: String,
}

// ── XML Helpers ─────────────────────────────────────────────────────

/// Build a CMCI success response envelope.
fn cmci_ok_response(record_count: usize, records_xml: &str, _resource_tag: &str) -> String {
    format!(
        r#"<?xml version="1.0" encoding="UTF-8"?>
<response xmlns="{CMCI_NS}" version="3.0" connect_version="0560">
  <resultsummary api_response1="1024" api_response2="0" api_response1_alt="OK" api_response2_alt="" recordcount="{record_count}" displayed_recordcount="{record_count}"/>
  <records>{records_xml}
  </records>
</response>"#,
        CMCI_NS = CMCI_NS,
        record_count = record_count,
        records_xml = records_xml,
    )
}

/// Build a CMCI success response for mutating operations (no records).
fn cmci_ok_mutate(message: &str) -> String {
    format!(
        r#"<?xml version="1.0" encoding="UTF-8"?>
<response xmlns="{CMCI_NS}" version="3.0" connect_version="0560">
  <resultsummary api_response1="1024" api_response2="0" api_response1_alt="OK" api_response2_alt="{message}" recordcount="0" displayed_recordcount="0"/>
</response>"#,
        CMCI_NS = CMCI_NS,
        message = message,
    )
}

/// Build a CMCI error response.
fn cmci_error_response(code: u32, message: &str) -> String {
    format!(
        r#"<?xml version="1.0" encoding="UTF-8"?>
<response xmlns="{CMCI_NS}" version="3.0" connect_version="0560">
  <resultsummary api_response1="{code}" api_response2="0" api_response1_alt="ERROR" api_response2_alt="{message}" recordcount="0" displayed_recordcount="0"/>
</response>"#,
        CMCI_NS = CMCI_NS,
        code = code,
        message = xml_escape(message),
    )
}

/// Escape special XML characters.
fn xml_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

/// Check if a value matches a CMCI CRITERIA pattern (supports trailing `*` wildcard).
fn criteria_matches(value: &str, pattern: &str) -> bool {
    if let Some(prefix) = pattern.strip_suffix('*') {
        value.to_uppercase().starts_with(&prefix.to_uppercase())
    } else {
        value.eq_ignore_ascii_case(pattern)
    }
}

/// Parse a simple CRITERIA expression like `PROGRAM=COSGN*` or `TRANID=CC*`.
/// Returns `(field_name, pattern)` or `None` if unparseable.
fn parse_criteria(criteria: &str) -> Option<(String, String)> {
    let trimmed = criteria.trim();
    // Remove outer parentheses if present
    let inner = if trimmed.starts_with('(') && trimmed.ends_with(')') {
        &trimmed[1..trimmed.len() - 1]
    } else {
        trimmed
    };
    let parts: Vec<&str> = inner.splitn(2, '=').collect();
    if parts.len() == 2 {
        let field = parts[0].trim().to_uppercase();
        let value = parts[1].trim().trim_matches('\'').to_string();
        Some((field, value))
    } else {
        None
    }
}

// ── Resource Collectors ─────────────────────────────────────────────

/// Collect programs from all configured CICS apps.
fn collect_programs(state: &AppState, criteria: &Option<String>) -> Vec<(String, String)> {
    let filter = criteria.as_ref().and_then(|c| parse_criteria(c));
    let mut programs: Vec<(String, String)> = Vec::new();

    // From static config apps
    for (app_name, profile) in &state.config.cics.apps {
        // Scan program_dir if available
        if let Some(ref dir) = profile.program_dir {
            if let Ok(entries) = std::fs::read_dir(dir) {
                for entry in entries.flatten() {
                    let path = entry.path();
                    if path.extension().and_then(|e| e.to_str()) == Some("cbl") {
                        if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                            let name = stem.to_uppercase();
                            if let Some((ref field, ref pattern)) = filter {
                                if field == "PROGRAM" && !criteria_matches(&name, pattern) {
                                    continue;
                                }
                            }
                            if !programs.iter().any(|(n, _)| n == &name) {
                                programs.push((name, app_name.clone()));
                            }
                        }
                    }
                }
            }
        }
    }

    // From dynamic apps
    for entry in state.cics_dynamic_apps.iter() {
        let app_name = entry.key().clone();
        let profile = entry.value();
        if let Some(ref dir) = profile.program_dir {
            if let Ok(entries) = std::fs::read_dir(dir) {
                for entry in entries.flatten() {
                    let path = entry.path();
                    if path.extension().and_then(|e| e.to_str()) == Some("cbl") {
                        if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                            let name = stem.to_uppercase();
                            if let Some((ref field, ref pattern)) = filter {
                                if field == "PROGRAM" && !criteria_matches(&name, pattern) {
                                    continue;
                                }
                            }
                            if !programs.iter().any(|(n, _)| n == &name) {
                                programs.push((name, app_name.clone()));
                            }
                        }
                    }
                }
            }
        }
    }

    programs
}

/// Collect transactions from all configured CICS apps.
fn collect_transactions(state: &AppState, criteria: &Option<String>) -> Vec<(String, String)> {
    let filter = criteria.as_ref().and_then(|c| parse_criteria(c));
    let mut transactions: Vec<(String, String)> = Vec::new();

    for profile in state.config.cics.apps.values() {
        for (transid, program) in &profile.transids {
            if let Some((ref field, ref pattern)) = filter {
                if (field == "TRANID" || field == "TRANSACTION")
                    && !criteria_matches(transid, pattern)
                {
                    continue;
                }
            }
            if !transactions.iter().any(|(t, _)| t == transid) {
                transactions.push((transid.clone(), program.clone()));
            }
        }
    }

    for entry in state.cics_dynamic_apps.iter() {
        let profile = entry.value();
        for (transid, program) in &profile.transids {
            if let Some((ref field, ref pattern)) = filter {
                if (field == "TRANID" || field == "TRANSACTION")
                    && !criteria_matches(transid, pattern)
                {
                    continue;
                }
            }
            if !transactions.iter().any(|(t, _)| t == transid) {
                transactions.push((transid.clone(), program.clone()));
            }
        }
    }

    transactions
}

/// Collect files from all configured CICS apps.
fn collect_files(state: &AppState, criteria: &Option<String>) -> Vec<(String, String, String)> {
    let filter = criteria.as_ref().and_then(|c| parse_criteria(c));
    let mut files: Vec<(String, String, String)> = Vec::new();

    for profile in state.config.cics.apps.values() {
        for data_file in &profile.data_files {
            // Format: DDNAME=path[:key_len[:rec_len]]
            let parts: Vec<&str> = data_file.splitn(2, '=').collect();
            if parts.len() == 2 {
                let ddname = parts[0].to_uppercase();
                let path = parts[1].to_string();
                if let Some((ref field, ref pattern)) = filter {
                    if field == "FILE" && !criteria_matches(&ddname, pattern) {
                        continue;
                    }
                }
                if !files.iter().any(|(n, _, _)| n == &ddname) {
                    files.push((ddname, path, "ENABLED".to_string()));
                }
            }
        }
    }

    for entry in state.cics_dynamic_apps.iter() {
        let profile = entry.value();
        for data_file in &profile.data_files {
            let parts: Vec<&str> = data_file.splitn(2, '=').collect();
            if parts.len() == 2 {
                let ddname = parts[0].to_uppercase();
                let path = parts[1].to_string();
                if let Some((ref field, ref pattern)) = filter {
                    if field == "FILE" && !criteria_matches(&ddname, pattern) {
                        continue;
                    }
                }
                if !files.iter().any(|(n, _, _)| n == &ddname) {
                    files.push((ddname, path, "ENABLED".to_string()));
                }
            }
        }
    }

    files
}

// ── Handlers ────────────────────────────────────────────────────────

/// GET /CICSSystemManagement/{resource_type}/{cics_plex}/{region}
async fn cmci_get(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(path): Path<CmciPath>,
    Query(query): Query<CmciQuery>,
) -> std::result::Result<impl IntoResponse, ZosmfErrorResponse> {
    let resource = path.resource_type.to_lowercase();

    let xml = match resource.as_str() {
        "cicsprogram" => {
            let programs = collect_programs(&state, &query.CRITERIA);
            let mut records = String::new();
            for (name, _app) in &programs {
                records.push_str(&format!(
                    "\n    <cicsprogram program=\"{}\" status=\"ENABLED\" language=\"COBOL\" usecount=\"0\" newcopystatus=\"\" runtime=\"UNKNOWN\" />",
                    xml_escape(name)
                ));
            }
            cmci_ok_response(programs.len(), &records, "cicsprogram")
        }
        "cicslocaltransaction" => {
            let txns = collect_transactions(&state, &query.CRITERIA);
            let mut records = String::new();
            for (transid, program) in &txns {
                records.push_str(&format!(
                    "\n    <cicslocaltransaction tranid=\"{}\" program=\"{}\" status=\"ENABLED\" />",
                    xml_escape(transid),
                    xml_escape(program)
                ));
            }
            cmci_ok_response(txns.len(), &records, "cicslocaltransaction")
        }
        "cicslocalfile" => {
            let files = collect_files(&state, &query.CRITERIA);
            let mut records = String::new();
            for (ddname, _path, status) in &files {
                records.push_str(&format!(
                    "\n    <cicslocalfile file=\"{}\" openstatus=\"OPEN\" enablestatus=\"{}\" />",
                    xml_escape(ddname),
                    xml_escape(status)
                ));
            }
            cmci_ok_response(files.len(), &records, "cicslocalfile")
        }
        "cicstask" => {
            let mut records = String::new();
            let count = state.cics_exec_sessions.len();
            for entry in state.cics_exec_sessions.iter() {
                let key = entry.key();
                let runner = entry.value();
                records.push_str(&format!(
                    "\n    <cicstask task=\"{}\" tranid=\"CICS\" userid=\"{}\" />",
                    xml_escape(key),
                    xml_escape(&runner.userid)
                ));
            }
            cmci_ok_response(count, &records, "cicstask")
        }
        "cicsdefinitionprogram" => {
            let programs = collect_programs(&state, &query.CRITERIA);
            let mut records = String::new();
            for (name, _app) in &programs {
                records.push_str(&format!(
                    "\n    <cicsdefinitionprogram name=\"{}\" language=\"COBOL\" status=\"ENABLED\" />",
                    xml_escape(name)
                ));
            }
            cmci_ok_response(programs.len(), &records, "cicsdefinitionprogram")
        }
        "cicsdefinitiontransaction" => {
            let txns = collect_transactions(&state, &query.CRITERIA);
            let mut records = String::new();
            for (transid, program) in &txns {
                records.push_str(&format!(
                    "\n    <cicsdefinitiontransaction name=\"{}\" program=\"{}\" status=\"ENABLED\" />",
                    xml_escape(transid),
                    xml_escape(program)
                ));
            }
            cmci_ok_response(txns.len(), &records, "cicsdefinitiontransaction")
        }
        "cicsdefinitionfile" => {
            let files = collect_files(&state, &query.CRITERIA);
            let mut records = String::new();
            for (ddname, _path, _status) in &files {
                records.push_str(&format!(
                    "\n    <cicsdefinitionfile name=\"{}\" status=\"ENABLED\" />",
                    xml_escape(ddname)
                ));
            }
            cmci_ok_response(files.len(), &records, "cicsdefinitionfile")
        }
        _ => cmci_error_response(
            1027,
            &format!("Resource type '{}' is not supported", path.resource_type),
        ),
    };

    Ok((
        StatusCode::OK,
        [(
            "content-type",
            "application/xml; charset=UTF-8",
        )],
        xml,
    )
        .into_response())
}

/// POST /CICSSystemManagement/{resource_type}/{cics_plex}/{region}
/// Used to create/install definitions.
async fn cmci_post(
    State(_state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(path): Path<CmciPath>,
    _headers: HeaderMap,
    _body: String,
) -> std::result::Result<impl IntoResponse, ZosmfErrorResponse> {
    let resource = path.resource_type.to_lowercase();

    // For definition resources, accept the POST and return success.
    // OpenMainframe discovers programs from disk, so "creating" a definition
    // is a no-op but must succeed for the Zowe workflow.
    let xml = match resource.as_str() {
        "cicsdefinitionprogram" | "cicsdefinitiontransaction" | "cicsdefinitionfile" => {
            cmci_ok_mutate("Definition accepted")
        }
        _ => cmci_error_response(
            1027,
            &format!(
                "POST not supported for resource type '{}'",
                path.resource_type
            ),
        ),
    };

    Ok((
        StatusCode::OK,
        [(
            "content-type",
            "application/xml; charset=UTF-8",
        )],
        xml,
    )
        .into_response())
}

/// PUT /CICSSystemManagement/{resource_type}/{cics_plex}/{region}
/// Used to set attributes (e.g., NEWCOPY, enable/disable).
async fn cmci_put(
    State(_state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(path): Path<CmciPath>,
    _headers: HeaderMap,
    _body: String,
) -> std::result::Result<impl IntoResponse, ZosmfErrorResponse> {
    let resource = path.resource_type.to_lowercase();

    let xml = match resource.as_str() {
        "cicsprogram" => {
            // NEWCOPY support: OpenMainframe compiles from disk fresh each time,
            // so NEWCOPY is effectively a no-op but must return success.
            cmci_ok_mutate("NEWCOPY successful")
        }
        "cicslocaltransaction" => cmci_ok_mutate("Transaction updated"),
        "cicslocalfile" => cmci_ok_mutate("File status updated"),
        "cicstask" => cmci_ok_mutate("Task updated"),
        _ => cmci_error_response(
            1027,
            &format!(
                "PUT not supported for resource type '{}'",
                path.resource_type
            ),
        ),
    };

    Ok((
        StatusCode::OK,
        [(
            "content-type",
            "application/xml; charset=UTF-8",
        )],
        xml,
    )
        .into_response())
}

/// DELETE /CICSSystemManagement/{resource_type}/{cics_plex}/{region}
/// Used to discard/delete definitions.
async fn cmci_delete(
    State(_state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(path): Path<CmciPath>,
    Query(_query): Query<CmciQuery>,
) -> std::result::Result<impl IntoResponse, ZosmfErrorResponse> {
    let resource = path.resource_type.to_lowercase();

    let xml = match resource.as_str() {
        "cicsdefinitionprogram" | "cicsdefinitiontransaction" | "cicsdefinitionfile" => {
            cmci_ok_mutate("Definition discarded")
        }
        "cicsprogram" | "cicslocaltransaction" | "cicslocalfile" => {
            cmci_ok_mutate("Resource discarded")
        }
        _ => cmci_error_response(
            1027,
            &format!(
                "DELETE not supported for resource type '{}'",
                path.resource_type
            ),
        ),
    };

    Ok((
        StatusCode::OK,
        [(
            "content-type",
            "application/xml; charset=UTF-8",
        )],
        xml,
    )
        .into_response())
}
