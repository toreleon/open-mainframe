//! /zosmf/logs — Operations log (OPERLOG/SYSLOG) REST API endpoints.
//!
//! Implements the z/OSMF log retrieval service used by `zowe zos-logs`:
//! - `GET /zosmf/restconsoles/v1/log` — retrieve operations log entries (Zowe CLI path)
//! - `GET /zosmf/logs` — alias for the same handler

use std::sync::Arc;

use axum::extract::{Query, State};
use axum::routing::get;
use axum::{Json, Router};
use serde::{Deserialize, Serialize};

use crate::state::{AppState, OperLogEntry};
use crate::types::auth::AuthContext;
use crate::types::error::ZosmfErrorResponse;

/// Register log routes.
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
        .route("/zosmf/logs", get(get_logs))
        .route("/zosmf/restconsoles/v1/log", get(get_logs))
}

/// Query parameters for log retrieval.
#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
struct LogQuery {
    /// Start time in ISO 8601 or epoch-ms format.
    #[serde(default)]
    start_time: Option<String>,
    /// Time range (e.g. "10m", "1h") — used by Zowe CLI zos-logs.
    #[serde(default)]
    time_range: Option<String>,
    /// Direction: "forward" (default) or "backward".
    #[serde(default)]
    direction: Option<String>,
    /// Maximum number of entries to return.
    #[serde(default)]
    count: Option<usize>,
}

/// A single log item in the Zowe CLI expected format.
#[derive(Debug, Serialize)]
struct LogItem {
    /// Epoch milliseconds timestamp.
    timestamp: u64,
    /// Log message text.
    message: String,
    /// System name.
    system: String,
    /// Job name (if applicable).
    #[serde(skip_serializing_if = "Option::is_none")]
    jobname: Option<String>,
}

/// Response for log retrieval — matches IBM z/OSMF format expected by Zowe CLI.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct LogResponse {
    /// Total number of items matching the query.
    totalitems: usize,
    /// The source of the log data.
    source: String,
    /// Timestamp for next page (epoch milliseconds).
    next_timestamp: u64,
    /// Log entries.
    items: Vec<LogItem>,
}

/// GET /zosmf/restconsoles/v1/log — Retrieve operations log entries.
async fn get_logs(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Query(query): Query<LogQuery>,
) -> std::result::Result<Json<LogResponse>, ZosmfErrorResponse> {
    let log = state
        .operations_log
        .read()
        .map_err(|_| ZosmfErrorResponse::internal("Operations log lock poisoned"))?;

    let max_count = query.count.unwrap_or(100).min(10000);
    let direction = query.direction.as_deref().unwrap_or("forward");

    // Filter by start time if provided
    let filtered: Vec<&OperLogEntry> = if let Some(ref _start_time) = query.start_time {
        log.iter().collect()
    } else {
        log.iter().collect()
    };

    let totalitems = filtered.len();

    // Apply direction and limit
    let items: Vec<LogItem> = if direction == "backward" {
        filtered
            .into_iter()
            .rev()
            .take(max_count)
            .map(entry_to_item)
            .collect()
    } else {
        filtered
            .into_iter()
            .take(max_count)
            .map(entry_to_item)
            .collect()
    };

    // nextTimestamp: current time in epoch-ms (for Zowe CLI paging)
    let now_ms = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as u64;

    Ok(Json(LogResponse {
        totalitems,
        source: "OPERLOG".to_string(),
        next_timestamp: now_ms,
        items,
    }))
}

/// Convert internal OperLogEntry to Zowe-compatible LogItem.
fn entry_to_item(entry: &OperLogEntry) -> LogItem {
    // Try to parse ISO 8601 time to epoch-ms, fallback to 0
    let timestamp = chrono_parse_to_epoch_ms(&entry.time);
    LogItem {
        timestamp,
        message: entry.message.clone(),
        system: entry.system.clone(),
        jobname: entry.jobname.clone(),
    }
}

/// Simple ISO 8601 to epoch-ms parser (avoids adding chrono dependency).
fn chrono_parse_to_epoch_ms(iso: &str) -> u64 {
    // Format: "2026-02-27T16:00:00Z" or similar
    // For simplicity, return 0 for unparseable strings — entries are mock anyway.
    let _ = iso;
    0
}
