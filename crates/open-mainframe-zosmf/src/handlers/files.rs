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
use axum::http::{HeaderMap, StatusCode};
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
        // Mounted filesystem routes
        .route("/zosmf/restfiles/mfs", get(list_filesystems))
        .route("/zosmf/restfiles/mfs/{*fsname}", put(mount_filesystem))
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
    /// File codeset tag (e.g., "t ISO8859-1", "b", "untagged").
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tag: Option<String>,
}

/// USS directory listing response.
///
/// Note: `totalRows` is returned via the `X-IBM-Response-Rows` header, not in the body.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UssListResponse {
    /// Directory entries.
    pub items: Vec<UssEntry>,
    /// Number of items returned.
    pub returned_rows: usize,
    /// JSON format version.
    #[serde(rename = "JSONversion")]
    pub json_version: i32,
}

/// Compute a simple ETag from content bytes.
fn compute_etag(content: &[u8]) -> String {
    let mut hash: u32 = 0;
    for &byte in content {
        hash = hash.wrapping_mul(31).wrapping_add(byte as u32);
    }
    format!("\"{:08X}\"", hash)
}

/// Resolve the USS path to the configured root.
///
/// Checks the mount table first for USS mounts, then falls back
/// to the standard USS root directory resolution.
fn resolve_uss_path(state: &AppState, uss_path: &str) -> PathBuf {
    // Check mount table for USS mounts
    if let Ok(mount_table) = state.mount_table.read() {
        if let Some(mounted) = mount_table.resolve_uss(uss_path) {
            return mounted.host_path;
        }
    }

    let root = PathBuf::from(&state.config.uss.root_directory);
    let cleaned = uss_path.trim_start_matches('/');
    root.join(cleaned)
}

// ─── Path-based route handlers (direct URL path) ───

async fn read_or_list(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    headers: HeaderMap,
    Path(uss_path): Path<String>,
) -> std::result::Result<axum::response::Response, ZosmfErrorResponse> {
    read_or_list_impl(&state, &auth, &uss_path, &headers).await
}

async fn write_file(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    headers: HeaderMap,
    Path(uss_path): Path<String>,
    body: Body,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    write_file_impl(&state, &auth, &uss_path, &headers, body).await
}

async fn create_dir(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    Path(uss_path): Path<String>,
    body: Body,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    create_dir_impl(&state, &auth, &uss_path, body).await
}

async fn delete_path(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    headers: HeaderMap,
    Path(uss_path): Path<String>,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    delete_path_impl(&state, &auth, &uss_path, &headers).await
}

// ─── Implementation functions shared by path-based and query-based handlers ───

async fn read_or_list_impl(
    state: &AppState,
    auth: &AuthContext,
    uss_path: &str,
    headers: &HeaderMap,
) -> std::result::Result<axum::response::Response, ZosmfErrorResponse> {
    let data_type = headers
        .get("x-ibm-data-type")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("text");
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
                tag: Some("untagged".to_string()),
            });
        }

        items.sort_by(|a, b| a.name.cmp(&b.name));
        let total = items.len();

        let resp = UssListResponse {
            items,
            returned_rows: total,
            json_version: 1,
        };

        let total_str = total.to_string();
        Ok((
            StatusCode::OK,
            [("x-ibm-response-rows", total_str.as_str())],
            Json(resp),
        )
            .into_response())
    } else {
        let content = std::fs::read(&full_path).map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to read file: {}", e))
        })?;

        let etag = compute_etag(&content);

        // Check If-None-Match.
        if let Some(if_none_match) = headers.get("if-none-match").and_then(|v| v.to_str().ok()) {
            if if_none_match == etag || if_none_match == "*" {
                return Ok((StatusCode::NOT_MODIFIED, [("etag", etag.as_str())]).into_response());
            }
        }

        if data_type == "binary" {
            Ok((
                StatusCode::OK,
                [
                    ("content-type", "application/octet-stream"),
                    ("etag", etag.as_str()),
                ],
                content,
            )
                .into_response())
        } else {
            let text = String::from_utf8_lossy(&content).to_string();
            Ok((
                StatusCode::OK,
                [
                    ("content-type", "text/plain; charset=UTF-8"),
                    ("etag", etag.as_str()),
                ],
                text,
            )
                .into_response())
        }
    }
}

async fn write_file_impl(
    state: &AppState,
    _auth: &AuthContext,
    uss_path: &str,
    headers: &HeaderMap,
    body: Body,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let data_type = headers
        .get("x-ibm-data-type")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("text");
    let full_path = resolve_uss_path(state, uss_path);

    let bytes = axum::body::to_bytes(body, 10 * 1024 * 1024)
        .await
        .map_err(|_| ZosmfErrorResponse::bad_request("Failed to read request body"))?;

    // Binary writes skip the JSON action check.
    if data_type != "binary" {
        // Check if this is a JSON action request.
        if let Ok(action) = serde_json::from_slice::<UssActionRequest>(&bytes) {
            return uss_file_action(state, uss_path, &full_path, &action);
        }
    }

    // If-Match: verify ETag before writing (optimistic concurrency).
    if let Some(if_match) = headers.get("if-match").and_then(|v| v.to_str().ok()) {
        if full_path.exists() && full_path.is_file() {
            if let Ok(current_content) = std::fs::read(&full_path) {
                let current_etag = compute_etag(&current_content);
                if if_match != current_etag && if_match != "*" {
                    return Err(ZosmfErrorResponse {
                        status: StatusCode::PRECONDITION_FAILED,
                        body: crate::types::error::ZosmfErrorBody {
                            rc: 4,
                            reason: 0,
                            category: 1,
                            message: "ETag mismatch — content has been modified".to_string(),
                            details: Vec::new(),
                            stack: None,
                        },
                    });
                }
            }
        }
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
/// Returns 201 Created if file is new, 204 No Content if overwriting.
fn write_file_content(
    full_path: &std::path::Path,
    bytes: &[u8],
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let existed = full_path.exists();

    if let Some(parent) = full_path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to create parent directory: {}", e))
        })?;
    }

    std::fs::write(full_path, bytes).map_err(|e| {
        ZosmfErrorResponse::internal(format!("Failed to write file: {}", e))
    })?;

    if existed {
        Ok(StatusCode::NO_CONTENT)
    } else {
        Ok(StatusCode::CREATED)
    }
}

/// JSON body for USS create requests.
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)]
struct UssCreateRequest {
    /// Type of resource: "mkdir" or "file".
    #[serde(rename = "type", default)]
    create_type: Option<String>,
    /// Unix permissions mode (e.g., "rwxr-xr-x" or "755").
    #[serde(default)]
    mode: Option<String>,
}

async fn create_dir_impl(
    state: &AppState,
    _auth: &AuthContext,
    uss_path: &str,
    body: Body,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let full_path = resolve_uss_path(state, uss_path);

    let bytes = axum::body::to_bytes(body, 1024 * 1024)
        .await
        .map_err(|_| ZosmfErrorResponse::bad_request("Failed to read request body"))?;

    let create_req: Option<UssCreateRequest> = if bytes.is_empty() {
        None
    } else {
        serde_json::from_slice(&bytes).ok()
    };

    let create_type = create_req
        .as_ref()
        .and_then(|r| r.create_type.as_deref())
        .unwrap_or("mkdir");

    let mode = create_req
        .as_ref()
        .and_then(|r| r.mode.as_deref());

    if create_type == "file" {
        // Create an empty file.
        if let Some(parent) = full_path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| {
                ZosmfErrorResponse::internal(format!("Failed to create parent: {}", e))
            })?;
        }
        std::fs::write(&full_path, b"").map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to create file: {}", e))
        })?;
    } else {
        // Default: create directory.
        std::fs::create_dir_all(&full_path).map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to create directory: {}", e))
        })?;
    }

    // Apply mode if specified.
    if let Some(mode_str) = mode {
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mode_val = u32::from_str_radix(mode_str.trim(), 8).unwrap_or(0o755);
            let perms = std::fs::Permissions::from_mode(mode_val);
            let _ = std::fs::set_permissions(&full_path, perms);
        }
        let _ = mode_str; // suppress unused warning on non-unix
    }

    Ok(StatusCode::CREATED)
}

async fn delete_path_impl(
    state: &AppState,
    _auth: &AuthContext,
    uss_path: &str,
    headers: &HeaderMap,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let recursive = headers
        .get("x-ibm-option")
        .and_then(|v| v.to_str().ok())
        .map(|v| v.to_lowercase().contains("recursive"))
        .unwrap_or(false);

    let full_path = resolve_uss_path(state, uss_path);

    if !full_path.exists() {
        return Err(ZosmfErrorResponse::not_found(format!(
            "Path '{}' not found",
            uss_path
        )));
    }

    if full_path.is_dir() {
        if recursive {
            std::fs::remove_dir_all(&full_path).map_err(|e| {
                ZosmfErrorResponse::internal(format!("Failed to delete directory: {}", e))
            })?;
        } else {
            // Without recursive flag, only empty directories can be deleted.
            std::fs::remove_dir(&full_path).map_err(|_| {
                ZosmfErrorResponse::bad_request(format!(
                    "Directory '{}' is not empty; use X-IBM-Option: recursive",
                    uss_path
                ))
            })?;
        }
    } else {
        std::fs::remove_file(&full_path).map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to delete file: {}", e))
        })?;
    }

    Ok(StatusCode::NO_CONTENT)
}

// ─── Mounted Filesystem (MFS) handlers ───

/// A mounted filesystem entry.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MfsEntry {
    /// Filesystem name.
    pub name: String,
    /// Mount point.
    pub mount_point: String,
    /// Filesystem type (e.g., "ZFS", "HFS", "TFS").
    pub fstname: String,
    /// Status.
    pub status: String,
    /// Mode (read-write or read-only).
    pub mode: Vec<String>,
}

/// Response for listing mounted filesystems.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MfsListResponse {
    pub items: Vec<MfsEntry>,
    pub returned_rows: usize,
}

/// GET /zosmf/restfiles/mfs — list mounted filesystems.
async fn list_filesystems(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
) -> Json<MfsListResponse> {
    let mut items = Vec::new();

    // System root filesystem.
    items.push(MfsEntry {
        name: "OMVS.ROOT".to_string(),
        mount_point: "/".to_string(),
        fstname: "ZFS".to_string(),
        status: "active".to_string(),
        mode: vec!["rdwr".to_string()],
    });

    items.push(MfsEntry {
        name: "OMVS.USR".to_string(),
        mount_point: state.config.uss.root_directory.clone(),
        fstname: "ZFS".to_string(),
        status: "active".to_string(),
        mode: vec!["rdwr".to_string()],
    });

    // Include external mounts from the mount table.
    if let Ok(mount_table) = state.mount_table.read() {
        for entry in mount_table.list() {
            let (name, mount_point, fstname) = match entry.mount_type {
                crate::mounts::MountType::DatasetPds => {
                    (format!("EXT.{}", entry.virtual_path), entry.virtual_path.clone(), "EXT".to_string())
                }
                crate::mounts::MountType::DatasetSeq => {
                    (format!("EXT.{}", entry.virtual_path), entry.virtual_path.clone(), "EXT".to_string())
                }
                crate::mounts::MountType::Uss => {
                    (format!("EXT.USS.{}", entry.mount_id), entry.virtual_path.clone(), "EXTUSS".to_string())
                }
            };
            let mode_str = if entry.read_only { "rdonly" } else { "rdwr" };
            items.push(MfsEntry {
                name,
                mount_point,
                fstname,
                status: "active".to_string(),
                mode: vec![mode_str.to_string()],
            });
        }
    }

    let count = items.len();
    Json(MfsListResponse {
        returned_rows: count,
        items,
    })
}

/// JSON body for mount requests.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "kebab-case")]
struct MountRequest {
    /// Action: "mount" or "unmount".
    #[serde(default = "default_mount_action")]
    action: String,
    /// Mount type: "dataset-pds", "dataset-seq", or "uss".
    #[serde(default, rename = "mount-type")]
    mount_type: Option<String>,
    /// Mount point (virtual path for USS mounts).
    #[serde(default, rename = "mount-point")]
    mount_point: Option<String>,
    /// Host path to mount from.
    #[serde(default, rename = "host-path")]
    host_path: Option<String>,
    /// Read-only mount.
    #[serde(default)]
    read_only: bool,
    /// Glob pattern for which host files to expose (e.g. "*.cbl").
    #[serde(default, rename = "file-filter")]
    file_filter: Option<String>,
}

fn default_mount_action() -> String {
    "mount".to_string()
}

/// PUT /zosmf/restfiles/mfs/:fsname — mount or unmount a filesystem.
async fn mount_filesystem(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(fsname): Path<String>,
    body: Body,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let bytes = axum::body::to_bytes(body, 1024 * 1024)
        .await
        .map_err(|_| ZosmfErrorResponse::bad_request("Failed to read request body"))?;

    // If body is empty, treat as a no-op for backward compatibility
    if bytes.is_empty() {
        return Ok(StatusCode::NO_CONTENT);
    }

    let req: MountRequest = serde_json::from_slice(&bytes).map_err(|e| {
        ZosmfErrorResponse::bad_request(format!("Invalid JSON: {}", e))
    })?;

    match req.action.to_lowercase().as_str() {
        "mount" => {
            let host_path = req.host_path.ok_or_else(|| {
                ZosmfErrorResponse::bad_request("Missing 'host-path' for mount")
            })?;
            let mount_type_str = req.mount_type.as_deref().unwrap_or("uss");
            let mount_point = req.mount_point.unwrap_or_else(|| fsname.clone());

            let mt = match mount_type_str {
                "dataset-pds" => crate::mounts::MountType::DatasetPds,
                "dataset-seq" => crate::mounts::MountType::DatasetSeq,
                "uss" => crate::mounts::MountType::Uss,
                other => return Err(ZosmfErrorResponse::bad_request(format!("Unknown mount type: {}", other))),
            };

            let mut mount_table = state.mount_table.write().map_err(|_| {
                ZosmfErrorResponse::internal("Mount table lock poisoned")
            })?;
            mount_table.add_mount(
                mt,
                std::path::PathBuf::from(&host_path),
                mount_point.clone(),
                req.read_only,
                req.file_filter,
            );

            tracing::info!(fsname = %fsname, mount_point = %mount_point, host_path = %host_path, "Filesystem mounted");
            Ok(StatusCode::NO_CONTENT)
        }
        "unmount" => {
            let mut mount_table = state.mount_table.write().map_err(|_| {
                ZosmfErrorResponse::internal("Mount table lock poisoned")
            })?;
            // Try to unmount by mount ID first
            let removed = mount_table.remove_mount(&fsname);
            if !removed {
                // Try matching by virtual path
                let mounts: Vec<_> = mount_table.list().iter()
                    .filter(|m| m.virtual_path == fsname)
                    .map(|m| m.mount_id.clone())
                    .collect();
                if mounts.is_empty() {
                    // Try extracting mount ID from EXT.USS.MNTxxxxx format
                    let maybe_id = fsname.rsplit('.').next().unwrap_or(&fsname);
                    mount_table.remove_mount(maybe_id);
                }
                for mid in mounts {
                    mount_table.remove_mount(&mid);
                }
            }
            tracing::info!(fsname = %fsname, "Filesystem unmounted");
            Ok(StatusCode::NO_CONTENT)
        }
        other => Err(ZosmfErrorResponse::bad_request(format!("Unknown mount action: {}", other))),
    }
}

// ─── Query-param-based route handlers (Zowe CLI sends ?path=/) ───

/// GET /zosmf/restfiles/fs?path=/ — list directory or read file.
async fn read_or_list_query(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    headers: HeaderMap,
    Query(query): Query<UssPathQuery>,
) -> std::result::Result<axum::response::Response, ZosmfErrorResponse> {
    let uss_path = query.path.unwrap_or_else(|| "/".to_string());
    read_or_list_impl(&state, &auth, &uss_path, &headers).await
}

/// PUT /zosmf/restfiles/fs?path=/file — write file content.
async fn write_file_query(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    headers: HeaderMap,
    Query(query): Query<UssPathQuery>,
    body: Body,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let uss_path = query
        .path
        .ok_or_else(|| ZosmfErrorResponse::bad_request("Missing 'path' query parameter"))?;
    write_file_impl(&state, &auth, &uss_path, &headers, body).await
}

/// POST /zosmf/restfiles/fs?path=/dir — create directory.
async fn create_dir_query(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    Query(query): Query<UssPathQuery>,
    body: Body,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let uss_path = query
        .path
        .ok_or_else(|| ZosmfErrorResponse::bad_request("Missing 'path' query parameter"))?;
    create_dir_impl(&state, &auth, &uss_path, body).await
}

/// DELETE /zosmf/restfiles/fs?path=/file — delete file or directory.
async fn delete_path_query(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    headers: HeaderMap,
    Query(query): Query<UssPathQuery>,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let uss_path = query
        .path
        .ok_or_else(|| ZosmfErrorResponse::bad_request("Missing 'path' query parameter"))?;
    delete_path_impl(&state, &auth, &uss_path, &headers).await
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
            tag: Some("untagged".to_string()),
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
                    tag: None,
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
                    tag: None,
                },
            ],
            returned_rows: 2,
            json_version: 1,
        };

        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"returnedRows\":2"));
        assert!(!json.contains("\"totalRows\""));
        assert!(json.contains("\"JSONversion\":1"));
    }

    #[test]
    fn test_mfs_entry_serialization() {
        let entry = MfsEntry {
            name: "OMVS.ROOT".to_string(),
            mount_point: "/".to_string(),
            fstname: "ZFS".to_string(),
            status: "active".to_string(),
            mode: vec!["rdwr".to_string()],
        };
        let json = serde_json::to_string(&entry).unwrap();
        assert!(json.contains("\"name\":\"OMVS.ROOT\""));
        assert!(json.contains("\"mountPoint\":\"/\""));
        assert!(json.contains("\"fstname\":\"ZFS\""));
    }

    #[test]
    fn test_mfs_list_response() {
        let resp = MfsListResponse {
            items: vec![MfsEntry {
                name: "OMVS.ROOT".to_string(),
                mount_point: "/".to_string(),
                fstname: "ZFS".to_string(),
                status: "active".to_string(),
                mode: vec!["rdwr".to_string()],
            }],
            returned_rows: 1,
        };
        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"returnedRows\":1"));
        assert!(json.contains("\"mountPoint\":\"/\""));
    }
}
