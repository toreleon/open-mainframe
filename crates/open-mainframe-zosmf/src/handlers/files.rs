//! /zosmf/restfiles/fs/* — USS file operations REST API endpoints.
//!
//! Implements the z/OSMF USS file REST services:
//! - `GET    /zosmf/restfiles/fs/:path` — list directory or read file
//! - `PUT    /zosmf/restfiles/fs/:path` — write file content
//! - `POST   /zosmf/restfiles/fs/:path` — create directory
//! - `DELETE /zosmf/restfiles/fs/:path` — delete file or directory

use std::path::PathBuf;
use std::sync::Arc;
use std::time::UNIX_EPOCH;

use axum::body::Body;
use axum::extract::{Path, Query, State};
use axum::http::StatusCode;
use axum::response::IntoResponse;
use axum::routing::{delete, get, post, put};
use axum::{Json, Router};
use serde::{Deserialize, Serialize};

use crate::state::AppState;
use crate::types::auth::AuthContext;
use crate::types::error::ZosmfErrorResponse;

/// JSON body for USS file action requests (chmod, chown, chtag, move, copy).
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[allow(dead_code)]
struct UssActionRequest {
    /// Action type: "chmod", "chown", "chtag", "move", "copy".
    request: String,
    /// New mode for chmod (e.g., "rwxr-xr-x" or octal "755").
    #[serde(default)]
    mode: Option<String>,
    /// Owner for chown (e.g., "IBMUSER").
    #[serde(default)]
    owner: Option<String>,
    /// Group for chown.
    #[serde(default)]
    group: Option<String>,
    /// Tag type for chtag (e.g., "mixed", "binary", "text").
    #[serde(default, rename = "type")]
    tag_type: Option<String>,
    /// Codeset for chtag.
    #[serde(default)]
    codeset: Option<String>,
    /// Source path for move/copy.
    #[serde(default)]
    from: Option<String>,
    /// Whether to overwrite existing target.
    #[serde(default)]
    overwrite: Option<bool>,
    /// Whether to apply recursively.
    #[serde(default)]
    recursive: Option<bool>,
    /// Links handling ("follow" or "suppress").
    #[serde(default)]
    links: Option<String>,
}

/// Query parameters for USS file operations (Zowe CLI sends path as query param).
#[derive(Debug, Deserialize)]
pub struct UssPathQuery {
    /// USS path.
    #[serde(default)]
    pub path: Option<String>,
}

/// Register USS file routes.
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
        // Query-param based routes (Zowe CLI uses ?path=/)
        .route("/zosmf/restfiles/fs", get(read_or_list_query))
        .route("/zosmf/restfiles/fs", put(write_file_query))
        .route("/zosmf/restfiles/fs", post(create_dir_query))
        .route("/zosmf/restfiles/fs", delete(delete_path_query))
        // Path-based routes (direct URL path)
        .route("/zosmf/restfiles/fs/{*path}", get(read_or_list))
        .route("/zosmf/restfiles/fs/{*path}", put(write_file))
        .route("/zosmf/restfiles/fs/{*path}", post(create_dir))
        .route("/zosmf/restfiles/fs/{*path}", delete(delete_path))
}

/// USS directory entry in list responses (matches real z/OSMF format).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UssEntry {
    /// File or directory name.
    pub name: String,
    /// Unix permission string (e.g., "-rwxr-xr-x" or "drwxr-xr-x").
    pub mode: String,
    /// Size in bytes.
    pub size: u64,
    /// User ID.
    pub uid: u32,
    /// User name.
    pub user: String,
    /// Group ID.
    pub gid: u32,
    /// Group name.
    pub group: String,
    /// Last modification time (ISO 8601).
    pub mtime: String,
}

/// USS directory listing response.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UssListResponse {
    /// Directory entries.
    pub items: Vec<UssEntry>,
    /// Number of items returned.
    pub returned_rows: usize,
    /// Total items.
    pub total_rows: usize,
    /// JSON format version.
    #[serde(rename = "JSONversion")]
    pub json_version: i32,
}

/// Resolve the USS path to the configured root.
fn resolve_uss_path(state: &AppState, uss_path: &str) -> PathBuf {
    let root = PathBuf::from(&state.config.uss.root_directory);
    let cleaned = uss_path.trim_start_matches('/');
    root.join(cleaned)
}

// ─── Path-based route handlers (direct URL path) ───

async fn read_or_list(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    Path(uss_path): Path<String>,
) -> std::result::Result<axum::response::Response, ZosmfErrorResponse> {
    read_or_list_impl(&state, &auth, &uss_path).await
}

async fn write_file(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    Path(uss_path): Path<String>,
    body: Body,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    write_file_impl(&state, &auth, &uss_path, body).await
}

async fn create_dir(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    Path(uss_path): Path<String>,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    create_dir_impl(&state, &auth, &uss_path).await
}

async fn delete_path(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    Path(uss_path): Path<String>,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    delete_path_impl(&state, &auth, &uss_path).await
}

// ─── Implementation functions shared by path-based and query-based handlers ───

async fn read_or_list_impl(
    state: &AppState,
    auth: &AuthContext,
    uss_path: &str,
) -> std::result::Result<axum::response::Response, ZosmfErrorResponse> {
    let full_path = resolve_uss_path(state, uss_path);

    if !full_path.exists() {
        return Err(ZosmfErrorResponse::not_found(format!(
            "Path '{}' not found",
            uss_path
        )));
    }

    if full_path.is_dir() {
        let entries = std::fs::read_dir(&full_path).map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to read directory: {}", e))
        })?;

        let mut items = Vec::new();
        for entry in entries {
            let entry = entry.map_err(|e| {
                ZosmfErrorResponse::internal(format!("Directory entry error: {}", e))
            })?;

            let metadata = entry.metadata().map_err(|e| {
                ZosmfErrorResponse::internal(format!("Metadata error: {}", e))
            })?;

            // Build Unix permission string like "-rwxr-xr-x" or "drwxr-xr-x".
            let mode_str = {
                #[cfg(unix)]
                {
                    use std::os::unix::fs::PermissionsExt;
                    let perm = metadata.permissions().mode();
                    let file_type = if metadata.is_dir() { 'd' } else if metadata.is_symlink() { 'l' } else { '-' };
                    let bits = [
                        if perm & 0o400 != 0 { 'r' } else { '-' },
                        if perm & 0o200 != 0 { 'w' } else { '-' },
                        if perm & 0o100 != 0 { 'x' } else { '-' },
                        if perm & 0o040 != 0 { 'r' } else { '-' },
                        if perm & 0o020 != 0 { 'w' } else { '-' },
                        if perm & 0o010 != 0 { 'x' } else { '-' },
                        if perm & 0o004 != 0 { 'r' } else { '-' },
                        if perm & 0o002 != 0 { 'w' } else { '-' },
                        if perm & 0o001 != 0 { 'x' } else { '-' },
                    ];
                    format!("{}{}", file_type, bits.iter().collect::<String>())
                }
                #[cfg(not(unix))]
                {
                    if metadata.is_dir() { "drwxr-xr-x".to_string() } else { "-rw-r--r--".to_string() }
                }
            };

            // Get uid/gid on Unix.
            let (uid, gid) = {
                #[cfg(unix)]
                {
                    use std::os::unix::fs::MetadataExt;
                    (metadata.uid(), metadata.gid())
                }
                #[cfg(not(unix))]
                { (0u32, 0u32) }
            };

            // Get mtime as ISO 8601.
            let mtime = metadata.modified()
                .ok()
                .and_then(|t| t.duration_since(UNIX_EPOCH).ok())
                .map(|d| {
                    let secs = d.as_secs() as i64;
                    let dt = chrono::DateTime::from_timestamp(secs, 0)
                        .unwrap_or_default();
                    dt.format("%Y-%m-%dT%H:%M:%S").to_string()
                })
                .unwrap_or_else(|| "1970-01-01T00:00:00".to_string());

            items.push(UssEntry {
                name: entry.file_name().to_string_lossy().to_string(),
                mode: mode_str,
                size: metadata.len(),
                uid,
                user: auth.userid.clone(),
                gid,
                group: "OMVSGRP".to_string(),
                mtime,
            });
        }

        items.sort_by(|a, b| a.name.cmp(&b.name));
        let total = items.len();

        let resp = UssListResponse {
            items,
            returned_rows: total,
            total_rows: total,
            json_version: 1,
        };

        Ok(Json(resp).into_response())
    } else {
        let content = std::fs::read(&full_path).map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to read file: {}", e))
        })?;

        let text = String::from_utf8_lossy(&content).to_string();
        Ok((
            StatusCode::OK,
            [("content-type", "text/plain; charset=UTF-8")],
            text,
        )
            .into_response())
    }
}

async fn write_file_impl(
    state: &AppState,
    _auth: &AuthContext,
    uss_path: &str,
    body: Body,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let full_path = resolve_uss_path(state, uss_path);

    let bytes = axum::body::to_bytes(body, 10 * 1024 * 1024)
        .await
        .map_err(|_| ZosmfErrorResponse::bad_request("Failed to read request body"))?;

    // Check if this is a JSON action request.
    if let Ok(action) = serde_json::from_slice::<UssActionRequest>(&bytes) {
        return uss_file_action(state, uss_path, &full_path, &action);
    }

    // Content write path.
    write_file_content(&full_path, &bytes)
}

/// Perform a USS file action (chmod, chown, chtag, move, copy).
fn uss_file_action(
    state: &AppState,
    uss_path: &str,
    full_path: &std::path::Path,
    action: &UssActionRequest,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    match action.request.to_lowercase().as_str() {
        "chmod" => {
            let mode_str = action.mode.as_deref().ok_or_else(|| {
                ZosmfErrorResponse::bad_request("Missing 'mode' for chmod")
            })?;

            if !full_path.exists() {
                return Err(ZosmfErrorResponse::not_found(format!(
                    "Path '{}' not found",
                    uss_path
                )));
            }

            // Parse octal mode (e.g., "755") or symbolic string.
            let mode = u32::from_str_radix(mode_str.trim(), 8).unwrap_or(0o644);

            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                let perms = std::fs::Permissions::from_mode(mode);
                std::fs::set_permissions(full_path, perms).map_err(|e| {
                    ZosmfErrorResponse::internal(format!("chmod failed: {}", e))
                })?;
            }

            Ok(StatusCode::OK)
        }
        "chown" => {
            // chown is a no-op in our emulation (we don't have real z/OS uid/gid mapping).
            if !full_path.exists() {
                return Err(ZosmfErrorResponse::not_found(format!(
                    "Path '{}' not found",
                    uss_path
                )));
            }
            Ok(StatusCode::OK)
        }
        "chtag" => {
            // chtag is metadata-only; we accept it and return OK.
            if !full_path.exists() {
                return Err(ZosmfErrorResponse::not_found(format!(
                    "Path '{}' not found",
                    uss_path
                )));
            }
            Ok(StatusCode::OK)
        }
        "move" => {
            let from = action.from.as_deref().ok_or_else(|| {
                ZosmfErrorResponse::bad_request("Missing 'from' path for move")
            })?;
            let src = resolve_uss_path(state, from);

            if !src.exists() {
                return Err(ZosmfErrorResponse::not_found(format!(
                    "Source path '{}' not found",
                    from
                )));
            }

            let overwrite = action.overwrite.unwrap_or(false);
            if full_path.exists() && !overwrite {
                return Err(ZosmfErrorResponse::bad_request(format!(
                    "Target path '{}' already exists",
                    uss_path
                )));
            }

            if let Some(parent) = full_path.parent() {
                std::fs::create_dir_all(parent).map_err(|e| {
                    ZosmfErrorResponse::internal(format!("Failed to create parent: {}", e))
                })?;
            }

            std::fs::rename(&src, full_path).map_err(|e| {
                ZosmfErrorResponse::internal(format!("Move failed: {}", e))
            })?;

            Ok(StatusCode::OK)
        }
        "copy" => {
            let from = action.from.as_deref().ok_or_else(|| {
                ZosmfErrorResponse::bad_request("Missing 'from' path for copy")
            })?;
            let src = resolve_uss_path(state, from);

            if !src.exists() {
                return Err(ZosmfErrorResponse::not_found(format!(
                    "Source path '{}' not found",
                    from
                )));
            }

            let overwrite = action.overwrite.unwrap_or(false);
            if full_path.exists() && !overwrite {
                return Err(ZosmfErrorResponse::bad_request(format!(
                    "Target path '{}' already exists",
                    uss_path
                )));
            }

            if let Some(parent) = full_path.parent() {
                std::fs::create_dir_all(parent).map_err(|e| {
                    ZosmfErrorResponse::internal(format!("Failed to create parent: {}", e))
                })?;
            }

            if src.is_dir() {
                copy_dir_recursive(&src, full_path)?;
            } else {
                std::fs::copy(&src, full_path).map_err(|e| {
                    ZosmfErrorResponse::internal(format!("Copy failed: {}", e))
                })?;
            }

            Ok(StatusCode::OK)
        }
        other => Err(ZosmfErrorResponse::bad_request(format!(
            "Unknown USS file action: {}",
            other
        ))),
    }
}

/// Recursively copy a directory tree.
fn copy_dir_recursive(
    src: &std::path::Path,
    dst: &std::path::Path,
) -> std::result::Result<(), ZosmfErrorResponse> {
    std::fs::create_dir_all(dst).map_err(|e| {
        ZosmfErrorResponse::internal(format!("Failed to create directory: {}", e))
    })?;

    for entry in std::fs::read_dir(src).map_err(|e| {
        ZosmfErrorResponse::internal(format!("Failed to read directory: {}", e))
    })? {
        let entry = entry.map_err(|e| {
            ZosmfErrorResponse::internal(format!("Directory entry error: {}", e))
        })?;
        let src_path = entry.path();
        let dst_path = dst.join(entry.file_name());

        if src_path.is_dir() {
            copy_dir_recursive(&src_path, &dst_path)?;
        } else {
            std::fs::copy(&src_path, &dst_path).map_err(|e| {
                ZosmfErrorResponse::internal(format!("Copy failed: {}", e))
            })?;
        }
    }

    Ok(())
}

/// Write raw content to a USS file.
fn write_file_content(
    full_path: &std::path::Path,
    bytes: &[u8],
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    if let Some(parent) = full_path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to create parent directory: {}", e))
        })?;
    }

    std::fs::write(full_path, bytes).map_err(|e| {
        ZosmfErrorResponse::internal(format!("Failed to write file: {}", e))
    })?;

    Ok(StatusCode::NO_CONTENT)
}

async fn create_dir_impl(
    state: &AppState,
    _auth: &AuthContext,
    uss_path: &str,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let full_path = resolve_uss_path(state, uss_path);

    std::fs::create_dir_all(&full_path).map_err(|e| {
        ZosmfErrorResponse::internal(format!("Failed to create directory: {}", e))
    })?;

    Ok(StatusCode::CREATED)
}

async fn delete_path_impl(
    state: &AppState,
    _auth: &AuthContext,
    uss_path: &str,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let full_path = resolve_uss_path(state, uss_path);

    if !full_path.exists() {
        return Err(ZosmfErrorResponse::not_found(format!(
            "Path '{}' not found",
            uss_path
        )));
    }

    if full_path.is_dir() {
        std::fs::remove_dir_all(&full_path).map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to delete directory: {}", e))
        })?;
    } else {
        std::fs::remove_file(&full_path).map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to delete file: {}", e))
        })?;
    }

    Ok(StatusCode::NO_CONTENT)
}

// ─── Query-param-based route handlers (Zowe CLI sends ?path=/) ───

/// GET /zosmf/restfiles/fs?path=/ — list directory or read file.
async fn read_or_list_query(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    Query(query): Query<UssPathQuery>,
) -> std::result::Result<axum::response::Response, ZosmfErrorResponse> {
    let uss_path = query.path.unwrap_or_else(|| "/".to_string());
    read_or_list_impl(&state, &auth, &uss_path).await
}

/// PUT /zosmf/restfiles/fs?path=/file — write file content.
async fn write_file_query(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    Query(query): Query<UssPathQuery>,
    body: Body,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let uss_path = query
        .path
        .ok_or_else(|| ZosmfErrorResponse::bad_request("Missing 'path' query parameter"))?;
    write_file_impl(&state, &auth, &uss_path, body).await
}

/// POST /zosmf/restfiles/fs?path=/dir — create directory.
async fn create_dir_query(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    Query(query): Query<UssPathQuery>,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let uss_path = query
        .path
        .ok_or_else(|| ZosmfErrorResponse::bad_request("Missing 'path' query parameter"))?;
    create_dir_impl(&state, &auth, &uss_path).await
}

/// DELETE /zosmf/restfiles/fs?path=/file — delete file or directory.
async fn delete_path_query(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    Query(query): Query<UssPathQuery>,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let uss_path = query
        .path
        .ok_or_else(|| ZosmfErrorResponse::bad_request("Missing 'path' query parameter"))?;
    delete_path_impl(&state, &auth, &uss_path).await
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_uss_entry_serialization() {
        let entry = UssEntry {
            name: "hello.txt".to_string(),
            mode: "-rw-r--r--".to_string(),
            size: 1024,
            uid: 0,
            user: "IBMUSER".to_string(),
            gid: 1,
            group: "OMVSGRP".to_string(),
            mtime: "2025-01-15T10:30:00".to_string(),
        };

        let json = serde_json::to_string(&entry).unwrap();
        assert!(json.contains("\"name\":\"hello.txt\""));
        assert!(json.contains("\"mode\":\"-rw-r--r--\""));
        assert!(json.contains("\"uid\":0"));
        assert!(json.contains("\"mtime\":\"2025-01-15T10:30:00\""));
    }

    #[test]
    fn test_uss_list_response_serialization() {
        let resp = UssListResponse {
            items: vec![
                UssEntry {
                    name: "dir1".to_string(),
                    mode: "drwxr-xr-x".to_string(),
                    size: 4096,
                    uid: 0,
                    user: "IBMUSER".to_string(),
                    gid: 0,
                    group: "OMVSGRP".to_string(),
                    mtime: "2025-01-15T10:30:00".to_string(),
                },
                UssEntry {
                    name: "file1.txt".to_string(),
                    mode: "-rw-r--r--".to_string(),
                    size: 100,
                    uid: 0,
                    user: "IBMUSER".to_string(),
                    gid: 0,
                    group: "OMVSGRP".to_string(),
                    mtime: "2025-01-15T10:30:00".to_string(),
                },
            ],
            returned_rows: 2,
            total_rows: 2,
            json_version: 1,
        };

        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"returnedRows\":2"));
        assert!(json.contains("\"totalRows\":2"));
        assert!(json.contains("\"JSONversion\":1"));
    }
}
