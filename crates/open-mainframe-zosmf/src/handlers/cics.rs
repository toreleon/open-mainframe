//! /zosmf/cicsApp/* — CICS terminal interaction REST API endpoints.
//!
//! Supports two modes:
//! - **Legacy BMS-only** — request includes `bmsSource` → screen rendering only
//! - **Full execution** — request includes `appName` → COBOL compilation + execution
//!
//! Endpoints:
//! - `POST   /zosmf/cicsApp/terminal`               — start CICS terminal session
//! - `GET    /zosmf/cicsApp/terminal/:sessionKey`    — get current screen state
//! - `PUT    /zosmf/cicsApp/terminal/:sessionKey`    — send AID + field data
//! - `DELETE /zosmf/cicsApp/terminal/:sessionKey`    — end session
//! - `GET    /zosmf/cicsApp/apps`                    — list registered CICS apps
//! - `POST   /zosmf/cicsApp/apps`                    — register a new CICS app
//! - `DELETE /zosmf/cicsApp/apps/:appName`           — unregister a dynamic CICS app

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use axum::extract::{Path, State};
use axum::http::StatusCode;
use axum::routing::{delete, get, post, put};
use axum::{Json, Router};
use open_mainframe_cics::bms::BmsParser;
use open_mainframe_cics::runtime::eib::aid;
use open_mainframe_cics::terminal::TerminalManager;
use tokio::sync::oneshot;

use crate::cics_runner::{CicsAppConfig, CicsSessionRunner, SessionCommand, SessionResponse};
use crate::config::CicsAppProfile;
use crate::state::{AppState, CicsSessionHandle};
use crate::types::auth::AuthContext;
use crate::types::error::ZosmfErrorResponse;

/// Register CICS routes.
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
        .route("/zosmf/cicsApp/terminal", post(start_session))
        .route("/zosmf/cicsApp/terminal/{session_key}", get(get_screen))
        .route("/zosmf/cicsApp/terminal/{session_key}", put(send_input))
        .route(
            "/zosmf/cicsApp/terminal/{session_key}",
            delete(stop_session),
        )
        .route("/zosmf/cicsApp/apps", get(list_apps))
        .route("/zosmf/cicsApp/apps", post(register_app))
        .route("/zosmf/cicsApp/apps/{app_name}", delete(unregister_app))
}

// ── Shared helpers ───────────────────────────────────────────────────

/// Generate a unique session key.
fn generate_session_key() -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    let ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    format!("CICS-{:016X}", ts)
}

/// Convert a `CicsAppProfile` (from zosmf.toml or dynamic registration) into a `CicsAppConfig`.
/// System copybooks are automatically appended to include_paths.
fn profile_to_config(
    profile: &CicsAppProfile,
    userid: &str,
    system_copybooks: &[String],
) -> CicsAppConfig {
    let mut include_paths: Vec<PathBuf> =
        profile.include_paths.iter().map(PathBuf::from).collect();
    for cp in system_copybooks {
        let p = PathBuf::from(cp);
        if !include_paths.contains(&p) {
            include_paths.push(p);
        }
    }
    CicsAppConfig {
        initial_program: PathBuf::from(&profile.program),
        include_paths,
        bms_dir: profile.bms_dir.as_ref().map(PathBuf::from),
        data_files: profile.data_files.clone(),
        transid_map: profile.transids.clone(),
        program_dir: profile.program_dir.as_ref().map(PathBuf::from),
        userid: userid.to_string(),
    }
}

/// Parse `fields` from request JSON into `HashMap<String, String>`.
fn parse_fields_string(request: &serde_json::Value) -> HashMap<String, String> {
    let mut fields = HashMap::new();
    if let Some(obj) = request["fields"].as_object() {
        for (k, v) in obj {
            if let Some(s) = v.as_str() {
                fields.insert(k.clone(), s.to_string());
            }
        }
    }
    fields
}

/// Convert a `ScreenOutput` to JSON with an added `sessionKey` field.
fn screen_to_json(
    screen: &open_mainframe_lib::headless::ScreenOutput,
    session_key: &str,
) -> serde_json::Value {
    let mut json = serde_json::to_value(screen).unwrap_or_default();
    json["sessionKey"] = serde_json::Value::String(session_key.to_string());
    json
}

/// Convert an `EndOutput` to JSON with an added `sessionKey` field.
fn end_to_json(
    end: &open_mainframe_lib::headless::EndOutput,
    session_key: &str,
) -> serde_json::Value {
    let mut json = serde_json::to_value(end).unwrap_or_default();
    json["sessionKey"] = serde_json::Value::String(session_key.to_string());
    json
}

// ── POST: start session ──────────────────────────────────────────────

/// POST /zosmf/cicsApp/terminal — start a CICS terminal session.
///
/// **Full execution mode** (new):
/// ```json
/// { "appName": "CARDDEMO" }
/// ```
///
/// **Legacy BMS-only mode** (backward compatible):
/// ```json
/// {
///   "bmsSource": "... raw BMS source ...",
///   "mapset": "COSGN00",
///   "map": "COSGN0A",
///   "fields": { "TITLE": "Welcome" }
/// }
/// ```
async fn start_session(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    Json(request): Json<serde_json::Value>,
) -> Result<(StatusCode, Json<serde_json::Value>), ZosmfErrorResponse> {
    // Legacy BMS-only path: if `bmsSource` is present, use the old handler.
    if request.get("bmsSource").is_some() {
        return start_session_bms_only(state, auth, request).await;
    }

    // Full execution path: resolve app profile from dynamic registry or zosmf.toml.
    let app_name = request["appName"]
        .as_str()
        .map(|s| s.to_uppercase())
        .or_else(|| state.config.cics.default_app.clone().map(|s| s.to_uppercase()))
        .ok_or_else(|| {
            ZosmfErrorResponse::bad_request(
                "Missing 'appName' and no default CICS app configured. \
                 Register an app via POST /zosmf/cicsApp/apps first.",
            )
        })?;

    // Check dynamic registry first, then static TOML config.
    let profile = state
        .cics_dynamic_apps
        .get(&app_name)
        .map(|r| r.value().clone())
        .or_else(|| state.config.cics.apps.get(&app_name).cloned())
        .ok_or_else(|| {
            ZosmfErrorResponse::bad_request(format!(
                "CICS app '{}' not found. Register it via POST /zosmf/cicsApp/apps \
                 or add it to zosmf.toml [cics.apps].",
                app_name
            ))
        })?;

    let app_config = profile_to_config(&profile, &auth.userid, &state.config.cics.system_copybooks);
    let session_key = generate_session_key();

    let runner = CicsSessionRunner::spawn(app_config).map_err(|e| {
        ZosmfErrorResponse::internal(format!("Failed to start CICS session: {e}"))
    })?;

    // Get the initial screen from the session thread.
    let (reply_tx, reply_rx) = oneshot::channel();
    runner
        .cmd_tx
        .send(SessionCommand::GetScreen { reply: reply_tx })
        .await
        .map_err(|_| ZosmfErrorResponse::internal("CICS session thread not responding"))?;

    let response = tokio::time::timeout(std::time::Duration::from_secs(30), reply_rx)
        .await
        .map_err(|_| ZosmfErrorResponse::internal("CICS session startup timed out"))?
        .map_err(|_| ZosmfErrorResponse::internal("CICS session thread died during startup"))?;

    let json = match response {
        SessionResponse::Screen(screen) => screen_to_json(&screen, &session_key),
        SessionResponse::End(end) => end_to_json(&end, &session_key),
        SessionResponse::Error(e) => {
            return Err(ZosmfErrorResponse::internal(e));
        }
    };

    state
        .cics_exec_sessions
        .insert(session_key.clone(), runner);

    tracing::info!(
        session_key = %session_key,
        app = %app_name,
        "CICS execution session started"
    );

    Ok((StatusCode::CREATED, Json(json)))
}

// ── GET: get screen ──────────────────────────────────────────────────

/// GET /zosmf/cicsApp/terminal/:sessionKey — get current screen state.
async fn get_screen(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(session_key): Path<String>,
) -> Result<Json<serde_json::Value>, ZosmfErrorResponse> {
    // Try full-execution sessions first.
    if let Some(runner) = state.cics_exec_sessions.get(&session_key) {
        let (reply_tx, reply_rx) = oneshot::channel();
        runner
            .cmd_tx
            .send(SessionCommand::GetScreen { reply: reply_tx })
            .await
            .map_err(|_| ZosmfErrorResponse::internal("Session thread not responding"))?;

        let response = tokio::time::timeout(std::time::Duration::from_secs(10), reply_rx)
            .await
            .map_err(|_| ZosmfErrorResponse::internal("GetScreen timed out"))?
            .map_err(|_| ZosmfErrorResponse::internal("Session thread died"))?;

        return match response {
            SessionResponse::Screen(screen) => Ok(Json(screen_to_json(&screen, &session_key))),
            SessionResponse::End(end) => {
                drop(runner);
                state.cics_exec_sessions.remove(&session_key);
                Ok(Json(end_to_json(&end, &session_key)))
            }
            SessionResponse::Error(e) => Err(ZosmfErrorResponse::internal(e)),
        };
    }

    // Fall back to legacy BMS sessions.
    let session = state.cics_sessions.get(&session_key).ok_or_else(|| {
        ZosmfErrorResponse::not_found(format!("CICS session '{}' not found", session_key))
    })?;

    let mut response = build_screen_response(&session);
    response["sessionKey"] = serde_json::Value::String(session_key);
    Ok(Json(response))
}

// ── PUT: send input ──────────────────────────────────────────────────

/// PUT /zosmf/cicsApp/terminal/:sessionKey — send AID key + field data.
///
/// Request body:
/// ```json
/// {
///   "aid": "ENTER",
///   "fields": { "USRIDINI": "ADMIN", "PASSINI": "password" }
/// }
/// ```
async fn send_input(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(session_key): Path<String>,
    Json(request): Json<serde_json::Value>,
) -> Result<Json<serde_json::Value>, ZosmfErrorResponse> {
    // Try full-execution sessions first.
    if let Some(runner) = state.cics_exec_sessions.get(&session_key) {
        let aid = request["aid"]
            .as_str()
            .unwrap_or("Enter")
            .to_string();
        let fields = parse_fields_string(&request);

        let (reply_tx, reply_rx) = oneshot::channel();
        runner
            .cmd_tx
            .send(SessionCommand::SendInput {
                aid,
                fields,
                reply: reply_tx,
            })
            .await
            .map_err(|_| ZosmfErrorResponse::internal("Session thread not responding"))?;

        let response = tokio::time::timeout(std::time::Duration::from_secs(30), reply_rx)
            .await
            .map_err(|_| ZosmfErrorResponse::internal("CICS execution timed out"))?
            .map_err(|_| ZosmfErrorResponse::internal("Session thread died"))?;

        return match response {
            SessionResponse::Screen(screen) => Ok(Json(screen_to_json(&screen, &session_key))),
            SessionResponse::End(end) => {
                drop(runner);
                state.cics_exec_sessions.remove(&session_key);
                Ok(Json(end_to_json(&end, &session_key)))
            }
            SessionResponse::Error(e) => {
                drop(runner);
                state.cics_exec_sessions.remove(&session_key);
                Err(ZosmfErrorResponse::internal(e))
            }
        };
    }

    // Fall back to legacy BMS sessions.
    send_input_bms_only(state, session_key, request).await
}

// ── DELETE: stop session ─────────────────────────────────────────────

/// DELETE /zosmf/cicsApp/terminal/:sessionKey — end CICS terminal session.
async fn stop_session(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(session_key): Path<String>,
) -> Result<StatusCode, ZosmfErrorResponse> {
    // Try full-execution sessions first.
    if state.cics_exec_sessions.remove(&session_key).is_some() {
        tracing::info!(session_key = %session_key, "CICS execution session stopped");
        return Ok(StatusCode::OK);
    }

    // Fall back to legacy BMS sessions.
    state
        .cics_sessions
        .remove(&session_key)
        .ok_or_else(|| {
            ZosmfErrorResponse::not_found(format!("CICS session '{}' not found", session_key))
        })?;

    tracing::info!(session_key = %session_key, "CICS terminal session stopped");
    Ok(StatusCode::OK)
}

// ── GET: list apps ──────────────────────────────────────────────────

/// GET /zosmf/cicsApp/apps — list all registered CICS applications.
///
/// Returns apps from both the static TOML config and the dynamic runtime registry.
async fn list_apps(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
) -> Json<serde_json::Value> {
    let mut apps = serde_json::Map::new();

    // Static apps from zosmf.toml
    for (name, profile) in &state.config.cics.apps {
        apps.insert(
            name.clone(),
            serde_json::json!({
                "program": profile.program,
                "include_paths": profile.include_paths,
                "bms_dir": profile.bms_dir,
                "program_dir": profile.program_dir,
                "data_files": profile.data_files,
                "transids": profile.transids,
                "source": "config",
            }),
        );
    }

    // Dynamic apps (runtime-registered) — override static if same name
    for entry in state.cics_dynamic_apps.iter() {
        let (name, profile) = entry.pair();
        apps.insert(
            name.clone(),
            serde_json::json!({
                "program": profile.program,
                "include_paths": profile.include_paths,
                "bms_dir": profile.bms_dir,
                "program_dir": profile.program_dir,
                "data_files": profile.data_files,
                "transids": profile.transids,
                "source": "dynamic",
            }),
        );
    }

    Json(serde_json::json!({
        "apps": apps,
        "default_app": state.config.cics.default_app,
        "system_copybooks": state.config.cics.system_copybooks,
    }))
}

// ── POST: register app ──────────────────────────────────────────────

/// POST /zosmf/cicsApp/apps — register a new CICS application at runtime.
///
/// Request body:
/// ```json
/// {
///   "name": "MYAPP",
///   "program": "/workspace/app/cbl/ENTRY.cbl",
///   "include_paths": ["/workspace/app/cpy"],
///   "bms_dir": "/workspace/app/bms",
///   "program_dir": "/workspace/app/cbl",
///   "data_files": ["ACCTDAT=/workspace/app/data/acct.txt:11:300"],
///   "transids": { "CC00": "COSGN00C" }
/// }
/// ```
async fn register_app(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Json(request): Json<serde_json::Value>,
) -> Result<(StatusCode, Json<serde_json::Value>), ZosmfErrorResponse> {
    let name = request["name"]
        .as_str()
        .ok_or_else(|| ZosmfErrorResponse::bad_request("Missing 'name' field"))?
        .to_uppercase();

    let program = request["program"]
        .as_str()
        .ok_or_else(|| ZosmfErrorResponse::bad_request("Missing 'program' field"))?
        .to_string();

    // Validate that the program file exists
    if !std::path::Path::new(&program).exists() {
        return Err(ZosmfErrorResponse::bad_request(format!(
            "Program file '{}' does not exist",
            program
        )));
    }

    let include_paths: Vec<String> = request["include_paths"]
        .as_array()
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(String::from))
                .collect()
        })
        .unwrap_or_default();

    let bms_dir = request["bms_dir"].as_str().map(String::from);
    let program_dir = request["program_dir"].as_str().map(String::from);

    let data_files: Vec<String> = request["data_files"]
        .as_array()
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(String::from))
                .collect()
        })
        .unwrap_or_default();

    let transids: HashMap<String, String> = request["transids"]
        .as_object()
        .map(|obj| {
            obj.iter()
                .filter_map(|(k, v)| v.as_str().map(|s| (k.clone(), s.to_string())))
                .collect()
        })
        .unwrap_or_default();

    let profile = CicsAppProfile {
        program: program.clone(),
        include_paths,
        bms_dir,
        data_files,
        transids,
        program_dir,
    };

    state.cics_dynamic_apps.insert(name.clone(), profile);

    tracing::info!(app = %name, program = %program, "CICS application registered dynamically");

    Ok((
        StatusCode::CREATED,
        Json(serde_json::json!({
            "name": name,
            "status": "registered",
        })),
    ))
}

// ── DELETE: unregister app ──────────────────────────────────────────

/// DELETE /zosmf/cicsApp/apps/:appName — unregister a dynamically registered CICS app.
///
/// Only removes apps from the dynamic registry, not from the static TOML config.
async fn unregister_app(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(app_name): Path<String>,
) -> Result<StatusCode, ZosmfErrorResponse> {
    let app_name = app_name.to_uppercase();

    if state.cics_dynamic_apps.remove(&app_name).is_some() {
        tracing::info!(app = %app_name, "CICS application unregistered");
        Ok(StatusCode::OK)
    } else {
        Err(ZosmfErrorResponse::not_found(format!(
            "Dynamic CICS app '{}' not found (static config apps cannot be removed via API)",
            app_name
        )))
    }
}

// =====================================================================
// Legacy BMS-only handlers (preserved for backward compatibility)
// =====================================================================

/// Map AID name to byte value (legacy BMS mode).
fn parse_aid(name: &str) -> u8 {
    match name.to_uppercase().as_str() {
        "ENTER" => aid::ENTER,
        "PF1" | "F1" => aid::PF1,
        "PF2" | "F2" => aid::PF2,
        "PF3" | "F3" => aid::PF3,
        "PF4" | "F4" => aid::PF4,
        "PF5" | "F5" => aid::PF5,
        "PF6" | "F6" => aid::PF6,
        "PF7" | "F7" => aid::PF7,
        "PF8" | "F8" => aid::PF8,
        "PF9" | "F9" => aid::PF9,
        "PF10" | "F10" => aid::PF10,
        "PF11" | "F11" => aid::PF11,
        "PF12" | "F12" => aid::PF12,
        "CLEAR" => aid::CLEAR,
        "PA1" => aid::PA1,
        "PA2" => aid::PA2,
        "PA3" => aid::PA3,
        _ => aid::ENTER,
    }
}

/// AID byte to name (legacy BMS mode).
fn aid_name(aid_byte: u8) -> &'static str {
    match aid_byte {
        0x7D => "ENTER",
        0xF1 => "PF1",
        0xF2 => "PF2",
        0xF3 => "PF3",
        0xF4 => "PF4",
        0xF5 => "PF5",
        0xF6 => "PF6",
        0xF7 => "PF7",
        0xF8 => "PF8",
        0xF9 => "PF9",
        0x7A => "PF10",
        0x7B => "PF11",
        0x7C => "PF12",
        0x6D => "CLEAR",
        0x6C => "PA1",
        0x6E => "PA2",
        0x6B => "PA3",
        _ => "UNKNOWN",
    }
}

/// Build JSON representation of the current screen state (legacy BMS mode).
fn build_screen_response(session: &CicsSessionHandle) -> serde_json::Value {
    let term = session.terminal_manager.get(&session.terminal_id);

    let (rows, cols, cursor_row, cursor_col, screen_text, last_aid) = match term {
        Some(t) => {
            let (r, c) = t.screen_size().dimensions();
            let cursor = t.cursor();
            (r, c, cursor.row, cursor.col, t.screen_text(), t.last_aid())
        }
        None => (24, 80, 1, 1, String::new(), aid::NO_AID),
    };

    let mut fields = Vec::new();
    if let (Some(mapset_name), Some(map_name)) =
        (&session.current_mapset, &session.current_map)
    {
        if let Some(mapset) = session.mapsets.get(mapset_name) {
            if let Some(map) = mapset.maps.iter().find(|m| m.name == *map_name) {
                for field in &map.fields {
                    if field.name.is_empty() {
                        continue;
                    }
                    let value = extract_field_from_text(
                        &screen_text,
                        cols,
                        field.row,
                        field.column,
                        field.length,
                    );
                    fields.push(serde_json::json!({
                        "name": field.name,
                        "row": field.row,
                        "col": field.column,
                        "length": field.length,
                        "value": value,
                        "protected": field.attributes.protected,
                        "numeric": field.attributes.numeric,
                        "hidden": field.attributes.dark,
                    }));
                }
            }
        }
    }

    serde_json::json!({
        "screen": {
            "rows": rows,
            "cols": cols,
            "cursorRow": cursor_row,
            "cursorCol": cursor_col,
            "aid": aid_name(last_aid),
            "mapset": session.current_mapset,
            "map": session.current_map,
            "fields": fields,
            "text": screen_text.trim_end(),
        }
    })
}

/// Extract a field value from screen text (line-based, legacy BMS mode).
fn extract_field_from_text(
    text: &str,
    _cols: usize,
    row: usize,
    col: usize,
    length: usize,
) -> String {
    let lines: Vec<&str> = text.lines().collect();
    if row == 0 || row > lines.len() {
        return String::new();
    }
    let line = lines[row - 1];
    let start = col.saturating_sub(1);
    let end = (start + length).min(line.len());
    if start >= line.len() {
        return String::new();
    }
    line[start..end].trim_end().to_string()
}

/// Legacy BMS-only start_session implementation.
async fn start_session_bms_only(
    state: Arc<AppState>,
    auth: AuthContext,
    request: serde_json::Value,
) -> Result<(StatusCode, Json<serde_json::Value>), ZosmfErrorResponse> {
    let bms_source = request["bmsSource"]
        .as_str()
        .ok_or_else(|| ZosmfErrorResponse::bad_request("Missing 'bmsSource' field"))?;

    let mapset_name = request["mapset"]
        .as_str()
        .unwrap_or("UNKNOWN")
        .to_uppercase();
    let map_name = request["map"].as_str().unwrap_or("").to_uppercase();

    let mut parser = BmsParser::new();
    let mapset = parser.parse(bms_source).map_err(|e| {
        ZosmfErrorResponse::bad_request(format!("BMS parse error: {}", e))
    })?;

    let target_map = if map_name.is_empty() {
        mapset
            .maps
            .first()
            .ok_or_else(|| ZosmfErrorResponse::bad_request("No maps found in mapset"))?
    } else {
        mapset
            .maps
            .iter()
            .find(|m| m.name == map_name)
            .ok_or_else(|| {
                ZosmfErrorResponse::bad_request(format!(
                    "Map '{}' not found in mapset",
                    map_name
                ))
            })?
    };

    let actual_map_name = target_map.name.clone();

    let mut field_data: HashMap<String, Vec<u8>> = HashMap::new();
    if let Some(fields) = request["fields"].as_object() {
        for (k, v) in fields {
            if let Some(s) = v.as_str() {
                field_data.insert(k.to_uppercase(), s.as_bytes().to_vec());
            }
        }
    }

    let session_key = generate_session_key();
    let terminal_id = format!("T{:04}", state.cics_sessions.len() + 1);

    let mut terminal_manager = TerminalManager::new();
    let send_opts = open_mainframe_cics::terminal::SendMapOptions::initial();
    terminal_manager
        .send_map(&terminal_id, target_map, &field_data, send_opts)
        .map_err(|e| ZosmfErrorResponse::internal(format!("SEND MAP failed: {}", e)))?;

    let mut mapsets = HashMap::new();
    mapsets.insert(mapset.name.clone(), mapset);

    tracing::info!(session_key = %session_key, mapset = %mapset_name, "CICS terminal session started (BMS-only)");

    let session = CicsSessionHandle {
        terminal_manager,
        terminal_id,
        mapsets,
        current_mapset: Some(mapset_name),
        current_map: Some(actual_map_name),
        userid: auth.userid,
    };

    let mut response = build_screen_response(&session);
    response["sessionKey"] = serde_json::Value::String(session_key.clone());

    state.cics_sessions.insert(session_key.clone(), session);
    Ok((StatusCode::CREATED, Json(response)))
}

/// Legacy BMS-only send_input implementation.
async fn send_input_bms_only(
    state: Arc<AppState>,
    session_key: String,
    request: serde_json::Value,
) -> Result<Json<serde_json::Value>, ZosmfErrorResponse> {
    let mut session = state.cics_sessions.get_mut(&session_key).ok_or_else(|| {
        ZosmfErrorResponse::not_found(format!("CICS session '{}' not found", session_key))
    })?;

    let aid_str = request["aid"].as_str().unwrap_or("ENTER");
    let aid_byte = parse_aid(aid_str);

    let mut field_data: HashMap<String, Vec<u8>> = HashMap::new();
    if let Some(fields) = request["fields"].as_object() {
        for (k, v) in fields {
            if let Some(s) = v.as_str() {
                field_data.insert(k.to_uppercase(), s.as_bytes().to_vec());
            }
        }
    }

    let tid = session.terminal_id.clone();

    session
        .terminal_manager
        .simulate_input(&tid, aid_byte, field_data.clone())
        .map_err(|e| ZosmfErrorResponse::internal(format!("simulate_input failed: {}", e)))?;

    let mut received_data = HashMap::new();
    if let (Some(mapset_name), Some(map_name)) =
        (&session.current_mapset, &session.current_map)
    {
        let mapset_name = mapset_name.clone();
        let map_name = map_name.clone();
        let map_clone = session
            .mapsets
            .get(&mapset_name)
            .and_then(|ms| ms.maps.iter().find(|m| m.name == map_name).cloned());
        if let Some(map) = map_clone {
            if let Ok(data) = session.terminal_manager.receive_map(&tid, &map) {
                received_data = data;
            }
        }
    }

    let mut received_fields = serde_json::Map::new();
    for (name, value) in &received_data {
        let text = String::from_utf8_lossy(value).trim_end().to_string();
        received_fields.insert(name.clone(), serde_json::Value::String(text));
    }

    let mut response = build_screen_response(&session);
    response["sessionKey"] = serde_json::Value::String(session_key);
    response["aid"] = serde_json::Value::String(aid_str.to_uppercase());
    response["receivedFields"] = serde_json::Value::Object(received_fields);

    Ok(Json(response))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_session_key() {
        let key = generate_session_key();
        assert!(key.starts_with("CICS-"));
    }

    #[test]
    fn test_parse_aid() {
        assert_eq!(parse_aid("ENTER"), aid::ENTER);
        assert_eq!(parse_aid("PF3"), aid::PF3);
        assert_eq!(parse_aid("F3"), aid::PF3);
        assert_eq!(parse_aid("CLEAR"), aid::CLEAR);
    }

    #[test]
    fn test_aid_name() {
        assert_eq!(aid_name(0x7D), "ENTER");
        assert_eq!(aid_name(0xF3), "PF3");
    }

    #[test]
    fn test_extract_field_from_text() {
        let text = "Hello World         \nSecond Line         \n";
        assert_eq!(extract_field_from_text(text, 80, 1, 1, 5), "Hello");
        assert_eq!(extract_field_from_text(text, 80, 1, 7, 5), "World");
        assert_eq!(extract_field_from_text(text, 80, 2, 1, 11), "Second Line");
    }

    #[test]
    fn test_build_screen_response_with_session() {
        let bms_source = r#"
TEST     DFHMSD TYPE=MAP,LANG=COBOL
TESTM    DFHMDI SIZE=(24,80)
NAME     DFHMDF POS=(5,10),LENGTH=20,ATTRB=(UNPROT,IC)
STATUS   DFHMDF POS=(6,10),LENGTH=10,ATTRB=(PROT),INITIAL='ACTIVE'
         DFHMSD TYPE=FINAL
"#;
        let mut parser = BmsParser::new();
        let mapset = parser.parse(bms_source).unwrap();
        let map = &mapset.maps[0];

        let mut terminal_manager = TerminalManager::new();
        let mut data = HashMap::new();
        data.insert("NAME".to_string(), b"John Doe".to_vec());

        let opts = open_mainframe_cics::terminal::SendMapOptions::initial();
        terminal_manager
            .send_map("T0001", map, &data, opts)
            .unwrap();

        let mut mapsets = HashMap::new();
        mapsets.insert("TEST".to_string(), mapset);

        let session = CicsSessionHandle {
            terminal_manager,
            terminal_id: "T0001".to_string(),
            mapsets,
            current_mapset: Some("TEST".to_string()),
            current_map: Some("TESTM".to_string()),
            userid: "ADMIN".to_string(),
        };

        let response = build_screen_response(&session);
        let screen = &response["screen"];

        assert_eq!(screen["rows"], 24);
        assert_eq!(screen["cols"], 80);
        assert_eq!(screen["mapset"], "TEST");
        assert_eq!(screen["map"], "TESTM");

        let fields = screen["fields"].as_array().unwrap();
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0]["name"], "NAME");
        assert_eq!(fields[1]["name"], "STATUS");
        assert_eq!(fields[1]["value"], "ACTIVE");
        assert!(fields[1]["protected"].as_bool().unwrap());
    }
}
