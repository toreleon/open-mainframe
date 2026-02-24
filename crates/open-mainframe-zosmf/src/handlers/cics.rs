//! /zosmf/cicsApp/* — CICS terminal interaction REST API endpoints.
//!
//! Provides BMS screen interaction via REST:
//! - `POST   /zosmf/cicsApp/terminal`               — start CICS terminal session
//! - `GET    /zosmf/cicsApp/terminal/:sessionKey`    — get current screen state
//! - `PUT    /zosmf/cicsApp/terminal/:sessionKey`    — send AID + field data
//! - `DELETE /zosmf/cicsApp/terminal/:sessionKey`    — end session

use std::collections::HashMap;
use std::sync::Arc;

use axum::extract::{Path, State};
use axum::http::StatusCode;
use axum::routing::{delete, get, post, put};
use axum::{Json, Router};
use open_mainframe_cics::bms::BmsParser;
use open_mainframe_cics::runtime::eib::aid;
use open_mainframe_cics::terminal::TerminalManager;

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
}

/// Generate a unique session key.
fn generate_session_key() -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    let ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    format!("CICS-{:016X}", ts)
}

/// Map AID name to byte value.
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

/// AID byte to name.
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

/// Build JSON representation of the current screen state.
fn build_screen_response(
    session: &CicsSessionHandle,
) -> serde_json::Value {
    let term = session
        .terminal_manager
        .get(&session.terminal_id);

    let (rows, cols, cursor_row, cursor_col, screen_text, last_aid) = match term {
        Some(t) => {
            let (r, c) = t.screen_size().dimensions();
            let cursor = t.cursor();
            (r, c, cursor.row, cursor.col, t.screen_text(), t.last_aid())
        }
        None => (24, 80, 1, 1, String::new(), aid::NO_AID),
    };

    // Build field info from current mapset/map
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
                    // Read field value from screen text
                    let value = extract_field_from_text(
                        &screen_text, cols, field.row, field.column, field.length,
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

/// Extract a field value from screen text (line-based).
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

// ── Endpoints ─────────────────────────────────────────────────────────

/// POST /zosmf/cicsApp/terminal — start a CICS terminal session.
///
/// Request body:
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
    let bms_source = request["bmsSource"]
        .as_str()
        .ok_or_else(|| ZosmfErrorResponse::bad_request("Missing 'bmsSource' field"))?;

    let mapset_name = request["mapset"]
        .as_str()
        .unwrap_or("UNKNOWN")
        .to_uppercase();
    let map_name = request["map"].as_str().unwrap_or("").to_uppercase();

    // Parse BMS source
    let mut parser = BmsParser::new();
    let mapset = parser.parse(bms_source).map_err(|e| {
        ZosmfErrorResponse::bad_request(format!("BMS parse error: {}", e))
    })?;

    // Determine which map to display
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
                ZosmfErrorResponse::bad_request(format!("Map '{}' not found in mapset", map_name))
            })?
    };

    let actual_map_name = target_map.name.clone();

    // Build field data from request
    let mut field_data: HashMap<String, Vec<u8>> = HashMap::new();
    if let Some(fields) = request["fields"].as_object() {
        for (k, v) in fields {
            if let Some(s) = v.as_str() {
                field_data.insert(k.to_uppercase(), s.as_bytes().to_vec());
            }
        }
    }

    // Create terminal session
    let session_key = generate_session_key();
    let terminal_id = format!("T{:04}", state.cics_sessions.len() + 1);

    let mut terminal_manager = TerminalManager::new();
    let send_opts =
        open_mainframe_cics::terminal::SendMapOptions::initial();
    terminal_manager
        .send_map(&terminal_id, target_map, &field_data, send_opts)
        .map_err(|e| {
            ZosmfErrorResponse::internal(format!("SEND MAP failed: {}", e))
        })?;

    let mut mapsets = HashMap::new();
    mapsets.insert(mapset.name.clone(), mapset);

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

    state.cics_sessions.insert(session_key, session);

    Ok((StatusCode::CREATED, Json(response)))
}

/// GET /zosmf/cicsApp/terminal/:sessionKey — get current screen state.
async fn get_screen(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(session_key): Path<String>,
) -> Result<Json<serde_json::Value>, ZosmfErrorResponse> {
    let session = state.cics_sessions.get(&session_key).ok_or_else(|| {
        ZosmfErrorResponse::not_found(format!("CICS session '{}' not found", session_key))
    })?;

    let mut response = build_screen_response(&session);
    response["sessionKey"] = serde_json::Value::String(session_key);
    Ok(Json(response))
}

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
    let mut session = state.cics_sessions.get_mut(&session_key).ok_or_else(|| {
        ZosmfErrorResponse::not_found(format!("CICS session '{}' not found", session_key))
    })?;

    let aid_str = request["aid"].as_str().unwrap_or("ENTER");
    let aid_byte = parse_aid(aid_str);

    // Build field data
    let mut field_data: HashMap<String, Vec<u8>> = HashMap::new();
    if let Some(fields) = request["fields"].as_object() {
        for (k, v) in fields {
            if let Some(s) = v.as_str() {
                field_data.insert(k.to_uppercase(), s.as_bytes().to_vec());
            }
        }
    }

    // Clone terminal_id to avoid borrow conflict
    let tid = session.terminal_id.clone();

    // Simulate input on the terminal
    session
        .terminal_manager
        .simulate_input(&tid, aid_byte, field_data.clone())
        .map_err(|e| {
            ZosmfErrorResponse::internal(format!("simulate_input failed: {}", e))
        })?;

    // If we have a current map, do RECEIVE MAP to extract the data
    let mut received_data = HashMap::new();
    if let (Some(mapset_name), Some(map_name)) =
        (&session.current_mapset, &session.current_map)
    {
        let mapset_name = mapset_name.clone();
        let map_name = map_name.clone();
        // Clone map to avoid holding immutable borrow on session while calling mutable method
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

    // Build a response with the received fields
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

/// DELETE /zosmf/cicsApp/terminal/:sessionKey — end CICS terminal session.
async fn stop_session(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(session_key): Path<String>,
) -> Result<StatusCode, ZosmfErrorResponse> {
    state
        .cics_sessions
        .remove(&session_key)
        .ok_or_else(|| {
            ZosmfErrorResponse::not_found(format!("CICS session '{}' not found", session_key))
        })?;

    Ok(StatusCode::OK)
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
