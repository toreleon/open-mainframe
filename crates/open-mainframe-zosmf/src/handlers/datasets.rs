//! /zosmf/restfiles/ds/* — dataset REST API endpoints.
//!
//! Implements the z/OSMF dataset REST services:
//! - `GET  /zosmf/restfiles/ds?dslevel=PATTERN` — list datasets
//! - `GET  /zosmf/restfiles/ds/:dsn/member` — list PDS members
//! - `GET  /zosmf/restfiles/ds/:dsn` — read dataset/member content
//! - `PUT  /zosmf/restfiles/ds/:dsn` — write dataset/member content
//! - `POST /zosmf/restfiles/ds/:dsn` — create dataset
//! - `DELETE /zosmf/restfiles/ds/:dsn` — delete dataset/member

use std::sync::Arc;

use axum::body::Body;
use axum::extract::{Path, Query, State};
use axum::http::{HeaderMap, StatusCode};
use axum::response::IntoResponse;
use axum::routing::{delete, get, post, put};
use axum::{Json, Router};
use open_mainframe_dataset::{DatasetAttributes, DatasetOrg, Pds, RecordFormat};
use serde::Deserialize;

use crate::state::AppState;
use crate::types::auth::AuthContext;
use crate::types::datasets::{
    DatasetCreateParams, DatasetListItem, DatasetListQuery, DatasetListResponse, MemberListItem,
    MemberListQuery, MemberListResponse,
};
use crate::types::error::ZosmfErrorResponse;

/// Compute a simple ETag from content bytes using a hash.
fn compute_etag(content: &[u8]) -> String {
    // Simple CRC32-like hash for ETag.
    let mut hash: u32 = 0;
    for &byte in content {
        hash = hash.wrapping_mul(31).wrapping_add(byte as u32);
    }
    format!("\"{:08X}\"", hash)
}

/// JSON body for dataset action requests (rename, copy, migrate, recall).
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "kebab-case")]
struct DatasetActionRequest {
    /// Action type: "rename", "copy", "hmigrate", "hrecall".
    request: String,
    /// Source dataset for rename/copy.
    #[serde(default)]
    from_dataset: Option<FromDataset>,
}

/// Source dataset reference for rename/copy actions.
#[derive(Debug, Clone, Deserialize)]
struct FromDataset {
    /// Source dataset name.
    dsn: String,
    /// Source member name (for copy).
    #[serde(default)]
    member: Option<String>,
}

/// Register dataset routes.
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
        .route("/zosmf/restfiles/ds", get(list_datasets))
        .route("/zosmf/restfiles/ds/{dsn}", get(read_dataset))
        .route("/zosmf/restfiles/ds/{dsn}", put(write_dataset))
        .route("/zosmf/restfiles/ds/{dsn}", post(create_dataset))
        .route("/zosmf/restfiles/ds/{dsn}", delete(delete_dataset))
        .route("/zosmf/restfiles/ds/{dsn}/member", get(list_members))
        .route("/zosmf/restfiles/ams", put(execute_ams))
}

/// Map RecordFormat enum to z/OSMF string.
fn recfm_to_string(recfm: &RecordFormat) -> String {
    match recfm {
        RecordFormat::Fixed => "F".to_string(),
        RecordFormat::FixedBlocked => "FB".to_string(),
        RecordFormat::Variable => "V".to_string(),
        RecordFormat::VariableBlocked => "VB".to_string(),
        RecordFormat::Undefined => "U".to_string(),
        RecordFormat::VariableSpanned => "VS".to_string(),
        RecordFormat::VariableBlockedSpanned => "VBS".to_string(),
    }
}

/// Map DatasetOrg enum to z/OSMF string.
fn dsorg_to_string(dsorg: &DatasetOrg) -> String {
    match dsorg {
        DatasetOrg::Sequential => "PS".to_string(),
        DatasetOrg::Partitioned => "PO".to_string(),
        DatasetOrg::Direct => "DA".to_string(),
        DatasetOrg::IndexedSequential => "IS".to_string(),
        DatasetOrg::VsamEntrySequenced => "VS".to_string(),
        DatasetOrg::VsamKeySequenced => "VS".to_string(),
        DatasetOrg::VsamRelativeRecord => "VS".to_string(),
    }
}

/// Parse dataset name, handling member notation `DSN(MEMBER)`.
fn parse_dsn(dsn: &str) -> (&str, Option<&str>) {
    if let Some(paren_pos) = dsn.find('(') {
        let ds = &dsn[..paren_pos];
        let member_end = dsn.find(')').unwrap_or(dsn.len());
        let member = &dsn[paren_pos + 1..member_end];
        (ds, Some(member))
    } else {
        (dsn, None)
    }
}

/// GET /zosmf/restfiles/ds?dslevel=PATTERN — list datasets matching pattern.
async fn list_datasets(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    headers: HeaderMap,
    Query(query): Query<DatasetListQuery>,
) -> std::result::Result<impl IntoResponse, ZosmfErrorResponse> {
    // X-IBM-Max-Items: 0 means unlimited per z/OSMF spec.
    let max_items_raw: Option<usize> = headers
        .get("x-ibm-max-items")
        .and_then(|v| v.to_str().ok())
        .and_then(|v| v.parse().ok());
    let max_items: usize = max_items_raw
        .map(|n| if n == 0 { usize::MAX } else { n })
        .unwrap_or(usize::MAX);

    let attributes_filter = headers
        .get("x-ibm-attributes")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("dsname");

    let catalog = state
        .catalog
        .read()
        .map_err(|_| ZosmfErrorResponse::internal("Catalog lock poisoned"))?;

    let mut all_names = catalog.list(&query.dslevel);

    // Apply start filter for pagination.
    if let Some(ref start) = query.start {
        let start_upper = start.to_uppercase();
        all_names.retain(|n| n.to_uppercase() > start_upper);
    }

    let total_rows = all_names.len();

    let items: Vec<DatasetListItem> = all_names
        .into_iter()
        .take(max_items)
        .map(|dsn| {
            // Try to get full attributes via lookup.
            if attributes_filter == "base" || attributes_filter == "vol" {
                if let Ok(dsref) = catalog.lookup(&dsn) {
                    return DatasetListItem {
                        dsname: dsn,
                        dsorg: Some(dsorg_to_string(&dsref.attributes.dsorg)),
                        recfm: Some(recfm_to_string(&dsref.attributes.recfm)),
                        lrecl: Some(dsref.attributes.lrecl.to_string()),
                        blksz: Some(dsref.attributes.blksize.to_string()),
                        vol: Some("WORK01".to_string()),
                        cdate: None,
                        rdate: None,
                        catnm: None,
                        dev: None,
                        edate: None,
                        extx: None,
                        migr: Some("NO".to_string()),
                        mvol: Some("N".to_string()),
                        ovf: None,
                        sizex: None,
                        spacu: None,
                        used: None,
                        vols: Some("WORK01".to_string()),
                        dsntp: if dsref.attributes.dsorg == DatasetOrg::Partitioned { Some("PDS".to_string()) } else { None },
                    };
                }
            }
            // Base attributes — just dsname + dsorg.
            let dsorg = catalog
                .lookup(&dsn)
                .ok()
                .map(|r| dsorg_to_string(&r.attributes.dsorg));
            DatasetListItem {
                dsname: dsn,
                dsorg,
                recfm: None,
                lrecl: None,
                blksz: None,
                vol: None,
                cdate: None,
                rdate: None,
                catnm: None,
                dev: None,
                edate: None,
                extx: None,
                migr: None,
                mvol: None,
                ovf: None,
                sizex: None,
                spacu: None,
                used: None,
                vols: None,
                dsntp: None,
            }
        })
        .collect();

    let returned_rows = items.len();
    let truncated = max_items_raw.is_some() && max_items_raw.unwrap() > 0 && total_rows > returned_rows;

    let status = if truncated {
        StatusCode::PARTIAL_CONTENT
    } else {
        StatusCode::OK
    };

    let resp = DatasetListResponse {
        items,
        returned_rows,
        json_version: 1,
    };

    let total_rows_str = total_rows.to_string();
    Ok((
        status,
        [("x-ibm-response-rows", total_rows_str.as_str())],
        Json(resp),
    )
        .into_response())
}

/// GET /zosmf/restfiles/ds/:dsn/member — list PDS members.
async fn list_members(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    headers: HeaderMap,
    Path(dsn): Path<String>,
    Query(member_query): Query<MemberListQuery>,
) -> std::result::Result<impl IntoResponse, ZosmfErrorResponse> {
    // X-IBM-Max-Items: 0 means unlimited per z/OSMF spec.
    let max_items_raw: Option<usize> = headers
        .get("x-ibm-max-items")
        .and_then(|v| v.to_str().ok())
        .and_then(|v| v.parse().ok());
    let max_items: usize = max_items_raw
        .map(|n| if n == 0 { usize::MAX } else { n })
        .unwrap_or(usize::MAX);

    let catalog = state
        .catalog
        .read()
        .map_err(|_| ZosmfErrorResponse::internal("Catalog lock poisoned"))?;

    let dsref = catalog.lookup(&dsn).map_err(|_| {
        ZosmfErrorResponse::not_found(format!("Dataset '{}' not found", dsn.to_uppercase()))
    })?;

    let pds_path = dsref
        .path
        .as_ref()
        .ok_or_else(|| ZosmfErrorResponse::not_found("Dataset path not resolved"))?;

    let pds = Pds::open(pds_path).map_err(|_| {
        ZosmfErrorResponse::not_found(format!(
            "PDS '{}' not found or not a PDS",
            dsn.to_uppercase()
        ))
    })?;

    let all_members = pds.list_members();

    // Apply pattern filter (supports `*` wildcard).
    let filtered_members: Vec<_> = all_members
        .into_iter()
        .filter(|m| {
            if let Some(ref pat) = member_query.pattern {
                let pat_upper = pat.to_uppercase();
                if pat_upper.contains('*') {
                    let prefix = pat_upper.split('*').next().unwrap_or("");
                    m.name.to_uppercase().starts_with(prefix)
                } else {
                    m.name.to_uppercase() == pat_upper
                }
            } else {
                true
            }
        })
        .filter(|m| {
            if let Some(ref start) = member_query.start {
                m.name.to_uppercase() > start.to_uppercase()
            } else {
                true
            }
        })
        .collect();

    let total_rows = filtered_members.len();

    let items: Vec<MemberListItem> = filtered_members
        .iter()
        .take(max_items)
        .map(|m| {
            let (vers, modification, c4date, m4date, cnorc, inorc) =
                if let Some(ref stats) = m.stats {
                    (
                        Some(stats.version),
                        Some(stats.modified_lines),
                        Some(stats.created.clone()),
                        Some(stats.modified.clone()),
                        Some(stats.current_lines),
                        Some(stats.initial_lines),
                    )
                } else {
                    (None, None, None, None, None, None)
                };
            MemberListItem {
                member: m.name.clone(),
                vers,
                modification,
                c4date,
                m4date,
                cnorc,
                inorc,
            }
        })
        .collect();

    let returned_rows = items.len();
    let truncated = max_items_raw.is_some() && max_items_raw.unwrap() > 0 && total_rows > returned_rows;

    let status = if truncated {
        StatusCode::PARTIAL_CONTENT
    } else {
        StatusCode::OK
    };

    let resp = MemberListResponse {
        items,
        returned_rows,
        json_version: 1,
    };

    let total_rows_str = total_rows.to_string();
    Ok((
        status,
        [("x-ibm-response-rows", total_rows_str.as_str())],
        Json(resp),
    )
        .into_response())
}

/// Build a content response with ETag header, supporting If-None-Match.
fn build_content_response(
    content: &[u8],
    data_type: &str,
    headers: &HeaderMap,
) -> std::result::Result<axum::response::Response, ZosmfErrorResponse> {
    let etag = compute_etag(content);

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
            content.to_vec(),
        )
            .into_response())
    } else {
        let text = String::from_utf8_lossy(content).to_string();
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

/// GET /zosmf/restfiles/ds/:dsn — read dataset or member content.
async fn read_dataset(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    headers: HeaderMap,
    Path(dsn): Path<String>,
) -> std::result::Result<impl IntoResponse, ZosmfErrorResponse> {
    let data_type = headers
        .get("x-ibm-data-type")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("text");
    let (ds_name, member) = parse_dsn(&dsn);

    let catalog = state
        .catalog
        .read()
        .map_err(|_| ZosmfErrorResponse::internal("Catalog lock poisoned"))?;

    if let Some(member_name) = member {
        // Read PDS member via Pds API.
        let dsref = catalog.lookup(ds_name).map_err(|_| {
            ZosmfErrorResponse::not_found(format!(
                "Dataset '{}' not found",
                ds_name.to_uppercase()
            ))
        })?;

        let pds_path = dsref
            .path
            .as_ref()
            .ok_or_else(|| ZosmfErrorResponse::not_found("Dataset path not resolved"))?;

        let pds = Pds::open(pds_path).map_err(|_| {
            ZosmfErrorResponse::not_found(format!(
                "PDS '{}' not found or not a PDS",
                ds_name.to_uppercase()
            ))
        })?;

        let content = pds.read_member(member_name).map_err(|_| {
            ZosmfErrorResponse::not_found(format!(
                "Member '{}({})' not found",
                ds_name.to_uppercase(),
                member_name.to_uppercase()
            ))
        })?;

        build_content_response(&content, data_type, &headers)
    } else {
        // Read sequential dataset.
        let dsref = catalog.lookup(ds_name).map_err(|_| {
            ZosmfErrorResponse::not_found(format!(
                "Dataset '{}' not found",
                ds_name.to_uppercase()
            ))
        })?;

        let path = dsref
            .path
            .as_ref()
            .ok_or_else(|| ZosmfErrorResponse::not_found("Dataset path not resolved"))?;

        // If this is a PDS directory, return an error (use /member endpoint).
        if path.is_dir() {
            return Err(ZosmfErrorResponse::bad_request(
                "Cannot read PDS as sequential; use /member endpoint or specify member name",
            ));
        }

        let content = std::fs::read(path).map_err(|_| {
            ZosmfErrorResponse::not_found(format!(
                "Cannot read dataset '{}'",
                ds_name.to_uppercase()
            ))
        })?;

        build_content_response(&content, data_type, &headers)
    }
}

/// PUT /zosmf/restfiles/ds/:dsn — write content or perform dataset action (rename/copy/migrate/recall).
///
/// If the body is JSON with a `"request"` field, dispatch to the action handler.
/// Otherwise, treat as a content write.
async fn write_dataset(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    headers: HeaderMap,
    Path(dsn): Path<String>,
    body: Body,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let data_type = headers
        .get("x-ibm-data-type")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("text");

    let bytes = axum::body::to_bytes(body, 10 * 1024 * 1024)
        .await
        .map_err(|_| ZosmfErrorResponse::bad_request("Failed to read request body"))?;

    // Binary writes skip the JSON action check — raw bytes are never JSON actions.
    if data_type != "binary" {
        // Check if this is a JSON action request by attempting to parse.
        if let Ok(action) = serde_json::from_slice::<DatasetActionRequest>(&bytes) {
            return dataset_action(&state, &dsn, &action);
        }
    }

    // If-Match: verify ETag before writing (optimistic concurrency).
    if let Some(if_match) = headers.get("if-match").and_then(|v| v.to_str().ok()) {
        let (ds_name, member) = parse_dsn(&dsn);
        let catalog = state
            .catalog
            .read()
            .map_err(|_| ZosmfErrorResponse::internal("Catalog lock poisoned"))?;

        let current_content = if let Some(member_name) = member {
            let dsref = catalog.lookup(ds_name).ok();
            dsref
                .and_then(|d| d.path.as_ref().and_then(|p| Pds::open(p).ok()))
                .and_then(|pds| pds.read_member(member_name).ok())
        } else {
            let dsref = catalog.lookup(ds_name).ok();
            dsref.and_then(|d| d.path.as_ref().and_then(|p| std::fs::read(p).ok()))
        };

        if let Some(content) = current_content {
            let current_etag = compute_etag(&content);
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

    // Content write path.
    write_dataset_content(&state, &dsn, bytes.to_vec())
}

/// Perform a dataset action (rename, copy, migrate, recall).
fn dataset_action(
    state: &AppState,
    dsn: &str,
    action: &DatasetActionRequest,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let dsn_upper = dsn.to_uppercase();
    match action.request.to_lowercase().as_str() {
        "rename" => {
            let from = action.from_dataset.as_ref().ok_or_else(|| {
                ZosmfErrorResponse::bad_request("Missing 'from-dataset' for rename")
            })?;
            let from_dsn = from.dsn.to_uppercase();

            let mut catalog = state
                .catalog
                .write()
                .map_err(|_| ZosmfErrorResponse::internal("Catalog lock poisoned"))?;

            if !catalog.exists(&from_dsn) {
                return Err(ZosmfErrorResponse::not_found(format!(
                    "Source dataset '{}' not found",
                    from_dsn
                )));
            }
            if catalog.exists(&dsn_upper) {
                return Err(ZosmfErrorResponse::bad_request(format!(
                    "Target dataset '{}' already exists",
                    dsn_upper
                )));
            }

            catalog.rename(&from_dsn, &dsn_upper).map_err(|e| {
                ZosmfErrorResponse::internal(format!("Rename failed: {}", e))
            })?;

            Ok(StatusCode::OK)
        }
        "copy" => {
            let from = action.from_dataset.as_ref().ok_or_else(|| {
                ZosmfErrorResponse::bad_request("Missing 'from-dataset' for copy")
            })?;
            let from_dsn = from.dsn.to_uppercase();

            let catalog = state
                .catalog
                .read()
                .map_err(|_| ZosmfErrorResponse::internal("Catalog lock poisoned"))?;

            // Read source content.
            let src_content = if let Some(ref member) = from.member {
                let dsref = catalog.lookup(&from_dsn).map_err(|_| {
                    ZosmfErrorResponse::not_found(format!("Source dataset '{}' not found", from_dsn))
                })?;
                let pds_path = dsref.path.as_ref().ok_or_else(|| {
                    ZosmfErrorResponse::not_found("Source path not resolved")
                })?;
                let pds = Pds::open(pds_path).map_err(|_| {
                    ZosmfErrorResponse::not_found(format!("Source PDS '{}' not found", from_dsn))
                })?;
                pds.read_member(member).map_err(|_| {
                    ZosmfErrorResponse::not_found(format!("Source member '{}({})' not found", from_dsn, member))
                })?
            } else {
                let dsref = catalog.lookup(&from_dsn).map_err(|_| {
                    ZosmfErrorResponse::not_found(format!("Source dataset '{}' not found", from_dsn))
                })?;
                let path = dsref.path.as_ref().ok_or_else(|| {
                    ZosmfErrorResponse::not_found("Source path not resolved")
                })?;
                std::fs::read(path).map_err(|e| {
                    ZosmfErrorResponse::internal(format!("Failed to read source: {}", e))
                })?
            };

            // Write to target.
            drop(catalog);
            write_dataset_content(state, dsn, src_content)?;

            Ok(StatusCode::OK)
        }
        "hmigrate" => {
            let mut catalog = state
                .catalog
                .write()
                .map_err(|_| ZosmfErrorResponse::internal("Catalog lock poisoned"))?;

            if !catalog.exists(&dsn_upper) {
                return Err(ZosmfErrorResponse::not_found(format!(
                    "Dataset '{}' not found",
                    dsn_upper
                )));
            }

            catalog.set_migrated(&dsn_upper, true).map_err(|e| {
                ZosmfErrorResponse::internal(format!("Migrate failed: {}", e))
            })?;

            Ok(StatusCode::OK)
        }
        "hrecall" => {
            let mut catalog = state
                .catalog
                .write()
                .map_err(|_| ZosmfErrorResponse::internal("Catalog lock poisoned"))?;

            if !catalog.exists(&dsn_upper) {
                return Err(ZosmfErrorResponse::not_found(format!(
                    "Dataset '{}' not found",
                    dsn_upper
                )));
            }

            catalog.set_migrated(&dsn_upper, false).map_err(|e| {
                ZosmfErrorResponse::internal(format!("Recall failed: {}", e))
            })?;

            Ok(StatusCode::OK)
        }
        other => Err(ZosmfErrorResponse::bad_request(format!(
            "Unknown dataset action: {}",
            other
        ))),
    }
}

/// Write content to a dataset or PDS member.
fn write_dataset_content(
    state: &AppState,
    dsn: &str,
    content: Vec<u8>,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let (ds_name, member) = parse_dsn(dsn);

    let catalog = state
        .catalog
        .read()
        .map_err(|_| ZosmfErrorResponse::internal("Catalog lock poisoned"))?;

    if let Some(member_name) = member {
        // Write PDS member.
        let dsref = catalog.lookup(ds_name).map_err(|_| {
            ZosmfErrorResponse::not_found(format!(
                "Dataset '{}' not found",
                ds_name.to_uppercase()
            ))
        })?;

        let pds_path = dsref
            .path
            .as_ref()
            .ok_or_else(|| ZosmfErrorResponse::not_found("Dataset path not resolved"))?;

        let mut pds = Pds::open(pds_path).map_err(|_| {
            ZosmfErrorResponse::not_found(format!(
                "PDS '{}' not found",
                ds_name.to_uppercase()
            ))
        })?;

        let is_new = !pds.has_member(member_name);
        if is_new {
            pds.add_member(member_name, &content).map_err(|e| {
                ZosmfErrorResponse::internal(format!("Failed to add member: {}", e))
            })?;
        } else {
            pds.update_member(member_name, &content).map_err(|e| {
                ZosmfErrorResponse::internal(format!("Failed to update member: {}", e))
            })?;
        }

        return if is_new {
            Ok(StatusCode::CREATED)
        } else {
            Ok(StatusCode::NO_CONTENT)
        };
    } else {
        // Write sequential dataset.
        let dsref = catalog.lookup(ds_name).map_err(|_| {
            ZosmfErrorResponse::not_found(format!(
                "Dataset '{}' not found",
                ds_name.to_uppercase()
            ))
        })?;

        let path = dsref
            .path
            .as_ref()
            .ok_or_else(|| ZosmfErrorResponse::not_found("Dataset path not resolved"))?;

        std::fs::write(path, &content).map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to write dataset: {}", e))
        })?;
    }

    Ok(StatusCode::NO_CONTENT)
}

/// POST /zosmf/restfiles/ds/:dsn — create a new dataset.
async fn create_dataset(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(dsn): Path<String>,
    body: Body,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let bytes = axum::body::to_bytes(body, 1024 * 1024)
        .await
        .map_err(|_| ZosmfErrorResponse::bad_request("Failed to read request body"))?;

    let params: DatasetCreateParams = serde_json::from_slice(&bytes).map_err(|e| {
        ZosmfErrorResponse::bad_request(format!("Invalid JSON: {}", e))
    })?;
    let dsn_upper = dsn.to_uppercase();

    let mut catalog = state
        .catalog
        .write()
        .map_err(|_| ZosmfErrorResponse::internal("Catalog lock poisoned"))?;

    if catalog.exists(&dsn_upper) {
        return Err(ZosmfErrorResponse::internal(format!(
            "Dataset '{}' already exists",
            dsn_upper
        )));
    }

    let dsorg = params
        .dsorg
        .as_deref()
        .and_then(DatasetOrg::parse)
        .unwrap_or(DatasetOrg::Sequential);

    let recfm = params
        .recfm
        .as_deref()
        .and_then(RecordFormat::parse)
        .unwrap_or(RecordFormat::FixedBlocked);

    let lrecl = params.lrecl.unwrap_or(80);
    let blksize = params.effective_blksize().unwrap_or(lrecl * 10);

    // Validate allocation parameters.
    if recfm == RecordFormat::FixedBlocked && blksize > 0 && lrecl > blksize {
        return Err(ZosmfErrorResponse::bad_request(format!(
            "LRECL ({}) cannot exceed BLKSIZE ({}) for FB format",
            lrecl, blksize
        )));
    }

    let base_dir = catalog.base_dir().to_path_buf();
    let ds_path = base_dir.join(dsn_upper.replace('.', "/"));

    let attributes = DatasetAttributes {
        recfm,
        lrecl,
        blksize,
        dsorg,
        keylen: None,
        keyoff: None,
    };

    if dsorg == DatasetOrg::Partitioned {
        // Create PDS directory structure.
        Pds::create(&ds_path).map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to create PDS: {}", e))
        })?;
    } else {
        // Create sequential dataset as a file.
        if let Some(parent) = ds_path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| {
                ZosmfErrorResponse::internal(format!("Failed to create directory: {}", e))
            })?;
        }
        std::fs::write(&ds_path, b"").map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to create dataset: {}", e))
        })?;
    }

    let entry = open_mainframe_dataset::CatalogEntry {
        dsn: dsn_upper,
        path: ds_path,
        attributes,
        is_pds: dsorg == DatasetOrg::Partitioned,
    };

    catalog.add_entry(entry);

    Ok(StatusCode::CREATED)
}

/// DELETE /zosmf/restfiles/ds/:dsn — delete dataset or PDS member.
async fn delete_dataset(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(dsn): Path<String>,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let (ds_name, member) = parse_dsn(&dsn);

    if let Some(member_name) = member {
        // Delete PDS member.
        let catalog = state
            .catalog
            .read()
            .map_err(|_| ZosmfErrorResponse::internal("Catalog lock poisoned"))?;

        let dsref = catalog.lookup(ds_name).map_err(|_| {
            ZosmfErrorResponse::not_found(format!(
                "Dataset '{}' not found",
                ds_name.to_uppercase()
            ))
        })?;

        let pds_path = dsref
            .path
            .as_ref()
            .ok_or_else(|| ZosmfErrorResponse::not_found("Dataset path not resolved"))?;

        let mut pds = Pds::open(pds_path).map_err(|_| {
            ZosmfErrorResponse::not_found(format!(
                "PDS '{}' not found",
                ds_name.to_uppercase()
            ))
        })?;

        pds.delete_member(member_name).map_err(|_| {
            ZosmfErrorResponse::not_found(format!(
                "Member '{}' not found in PDS '{}'",
                member_name.to_uppercase(),
                ds_name.to_uppercase()
            ))
        })?;
    } else {
        // Delete entire dataset.
        let mut catalog = state
            .catalog
            .write()
            .map_err(|_| ZosmfErrorResponse::internal("Catalog lock poisoned"))?;

        if !catalog.exists(ds_name) {
            return Err(ZosmfErrorResponse::not_found(format!(
                "Dataset '{}' not found",
                ds_name.to_uppercase()
            )));
        }

        catalog.delete(ds_name).map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to delete dataset: {}", e))
        })?;
    }

    Ok(StatusCode::NO_CONTENT)
}

/// Request body for IDCAMS (AMS) command execution.
#[derive(Debug, Deserialize)]
struct AmsRequest {
    /// IDCAMS command input lines.
    input: Vec<String>,
}

/// Response body for IDCAMS execution.
#[derive(Debug, serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct AmsResponse {
    /// IDCAMS return code.
    return_code: u32,
    /// IDCAMS output text.
    output: String,
}

/// PUT /zosmf/restfiles/ams — execute IDCAMS (Access Method Services) commands.
async fn execute_ams(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Json(req): Json<AmsRequest>,
) -> std::result::Result<Json<AmsResponse>, ZosmfErrorResponse> {
    let catalog = state
        .catalog
        .read()
        .map_err(|_| ZosmfErrorResponse::internal("Catalog lock poisoned"))?;

    let input = req.input.join("\n");
    let mut idcams = open_mainframe_dataset::Idcams::new(catalog.base_dir());

    let result = idcams.execute(&input).map_err(|e| {
        ZosmfErrorResponse::internal(format!("IDCAMS execution failed: {}", e))
    })?;

    Ok(Json(AmsResponse {
        return_code: result.return_code,
        output: result.output,
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_dsn_simple() {
        let (ds, member) = parse_dsn("IBMUSER.DATA");
        assert_eq!(ds, "IBMUSER.DATA");
        assert_eq!(member, None);
    }

    #[test]
    fn test_parse_dsn_with_member() {
        let (ds, member) = parse_dsn("IBMUSER.COBOL(PAYROLL)");
        assert_eq!(ds, "IBMUSER.COBOL");
        assert_eq!(member, Some("PAYROLL"));
    }

    #[test]
    fn test_recfm_to_string() {
        assert_eq!(recfm_to_string(&RecordFormat::FixedBlocked), "FB");
        assert_eq!(recfm_to_string(&RecordFormat::Variable), "V");
        assert_eq!(recfm_to_string(&RecordFormat::Undefined), "U");
    }

    #[test]
    fn test_dsorg_to_string() {
        assert_eq!(dsorg_to_string(&DatasetOrg::Sequential), "PS");
        assert_eq!(dsorg_to_string(&DatasetOrg::Partitioned), "PO");
    }

    #[test]
    fn test_dataset_list_response_serialization() {
        let resp = DatasetListResponse {
            items: vec![DatasetListItem {
                dsname: "IBMUSER.DATA".to_string(),
                dsorg: Some("PS".to_string()),
                recfm: Some("FB".to_string()),
                lrecl: Some("80".to_string()),
                blksz: Some("800".to_string()),
                vol: None,
                cdate: None,
                rdate: None,
                catnm: None,
                dev: None,
                edate: None,
                extx: None,
                migr: None,
                mvol: None,
                ovf: None,
                sizex: None,
                spacu: None,
                used: None,
                vols: None,
                dsntp: None,
            }],
            returned_rows: 1,
            json_version: 1,
        };

        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"dsname\":\"IBMUSER.DATA\""));
        assert!(json.contains("\"lrecl\":\"80\""));
        assert!(json.contains("\"returnedRows\":1"));
        assert!(json.contains("\"JSONversion\":1"));
    }

    #[test]
    fn test_member_list_response_serialization() {
        let resp = MemberListResponse {
            items: vec![MemberListItem {
                member: "PAYROLL".to_string(),
                vers: Some(1),
                modification: Some(5),
                c4date: Some("2024-01-15".to_string()),
                m4date: Some("2024-06-20".to_string()),
                cnorc: Some(150),
                inorc: Some(100),
            }],
            returned_rows: 1,
            json_version: 1,
        };

        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"member\":\"PAYROLL\""));
        assert!(json.contains("\"mod\":5"));
        assert!(json.contains("\"vers\":1"));
    }

    #[test]
    fn test_ams_response_serialization() {
        let resp = AmsResponse {
            return_code: 0,
            output: "IDC0001I FUNCTION COMPLETED".to_string(),
        };
        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"returnCode\":0"));
        assert!(json.contains("\"output\":\"IDC0001I FUNCTION COMPLETED\""));
    }

    #[test]
    fn test_ams_request_deserialization() {
        let json = r#"{"input": ["DEFINE CLUSTER (NAME(MY.CLUSTER) INDEXED)"]}"#;
        let req: AmsRequest = serde_json::from_str(json).unwrap();
        assert_eq!(req.input.len(), 1);
        assert!(req.input[0].contains("DEFINE CLUSTER"));
    }
}
