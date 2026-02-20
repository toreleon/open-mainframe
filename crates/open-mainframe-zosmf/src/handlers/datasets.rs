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

use crate::state::AppState;
use crate::types::auth::AuthContext;
use crate::types::datasets::{
    DatasetCreateParams, DatasetListItem, DatasetListQuery, DatasetListResponse, MemberListItem,
    MemberListResponse,
};
use crate::types::error::ZosmfErrorResponse;

/// Register dataset routes.
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
        .route("/zosmf/restfiles/ds", get(list_datasets))
        .route("/zosmf/restfiles/ds/{dsn}", get(read_dataset))
        .route("/zosmf/restfiles/ds/{dsn}", put(write_dataset))
        .route("/zosmf/restfiles/ds/{dsn}", post(create_dataset))
        .route("/zosmf/restfiles/ds/{dsn}", delete(delete_dataset))
        .route("/zosmf/restfiles/ds/{dsn}/member", get(list_members))
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
) -> std::result::Result<Json<DatasetListResponse>, ZosmfErrorResponse> {
    let max_items: usize = headers
        .get("x-ibm-max-items")
        .and_then(|v| v.to_str().ok())
        .and_then(|v| v.parse().ok())
        .unwrap_or(usize::MAX);

    let attributes_filter = headers
        .get("x-ibm-attributes")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("base");

    let catalog = state
        .catalog
        .read()
        .map_err(|_| ZosmfErrorResponse::internal("Catalog lock poisoned"))?;

    let all_names = catalog.list(&query.dslevel);
    let total_rows = all_names.len();

    let items: Vec<DatasetListItem> = all_names
        .into_iter()
        .take(max_items)
        .map(|dsn| {
            // Try to get full attributes via lookup.
            if attributes_filter != "base" {
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
            }
        })
        .collect();

    let returned_rows = items.len();

    Ok(Json(DatasetListResponse {
        items,
        returned_rows,
        total_rows,
        json_version: 1,
    }))
}

/// GET /zosmf/restfiles/ds/:dsn/member — list PDS members.
async fn list_members(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    headers: HeaderMap,
    Path(dsn): Path<String>,
) -> std::result::Result<Json<MemberListResponse>, ZosmfErrorResponse> {
    let max_items: usize = headers
        .get("x-ibm-max-items")
        .and_then(|v| v.to_str().ok())
        .and_then(|v| v.parse().ok())
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
    let total_rows = all_members.len();

    let items: Vec<MemberListItem> = all_members
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

    Ok(Json(MemberListResponse {
        items,
        returned_rows,
        total_rows,
        json_version: 1,
    }))
}

/// GET /zosmf/restfiles/ds/:dsn — read dataset or member content.
async fn read_dataset(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(dsn): Path<String>,
) -> std::result::Result<impl IntoResponse, ZosmfErrorResponse> {
    let (ds_name, member) = parse_dsn(&dsn);

    let catalog = state
        .catalog
        .read()
        .map_err(|_| ZosmfErrorResponse::internal("Catalog lock poisoned"))?;

    if let Some(member_name) = member {
        // Read PDS member.
        let dsref = catalog.lookup_member(ds_name, member_name).map_err(|_| {
            ZosmfErrorResponse::not_found(format!(
                "Member '{}({})' not found",
                ds_name.to_uppercase(),
                member_name.to_uppercase()
            ))
        })?;

        let path = dsref
            .path
            .as_ref()
            .ok_or_else(|| ZosmfErrorResponse::not_found("Member path not resolved"))?;

        let content = std::fs::read(path).map_err(|_| {
            ZosmfErrorResponse::not_found(format!(
                "Cannot read member '{}({})'",
                ds_name.to_uppercase(),
                member_name.to_uppercase()
            ))
        })?;

        let text = String::from_utf8_lossy(&content).to_string();
        Ok((
            StatusCode::OK,
            [("content-type", "text/plain; charset=UTF-8")],
            text,
        ))
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

        let text = String::from_utf8_lossy(&content).to_string();
        Ok((
            StatusCode::OK,
            [("content-type", "text/plain; charset=UTF-8")],
            text,
        ))
    }
}

/// PUT /zosmf/restfiles/ds/:dsn — write dataset or member content.
async fn write_dataset(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(dsn): Path<String>,
    body: Body,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let (ds_name, member) = parse_dsn(&dsn);

    let bytes = axum::body::to_bytes(body, 10 * 1024 * 1024)
        .await
        .map_err(|_| ZosmfErrorResponse::bad_request("Failed to read request body"))?;

    let content = bytes.to_vec();

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

        if pds.has_member(member_name) {
            pds.update_member(member_name, &content).map_err(|e| {
                ZosmfErrorResponse::internal(format!("Failed to update member: {}", e))
            })?;
        } else {
            pds.add_member(member_name, &content).map_err(|e| {
                ZosmfErrorResponse::internal(format!("Failed to add member: {}", e))
            })?;
        }
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
    Json(params): Json<DatasetCreateParams>,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
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
    let blksize = params.blksz.unwrap_or(lrecl * 10);

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
            }],
            returned_rows: 1,
            total_rows: 1,
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
            total_rows: 1,
            json_version: 1,
        };

        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"member\":\"PAYROLL\""));
        assert!(json.contains("\"mod\":5"));
        assert!(json.contains("\"vers\":1"));
    }
}
