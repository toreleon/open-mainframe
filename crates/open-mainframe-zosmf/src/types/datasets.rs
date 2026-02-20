//! z/OSMF dataset REST API types.

use serde::{Deserialize, Serialize};

/// Response body for dataset list operations.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DatasetListResponse {
    /// List of dataset items.
    pub items: Vec<DatasetListItem>,
    /// Number of rows returned.
    pub returned_rows: usize,
    /// Total rows matching the query.
    pub total_rows: usize,
    /// JSON format version.
    #[serde(rename = "JSONversion")]
    pub json_version: i32,
}

/// A single dataset in a list response — field names match z/OSMF spec.
///
/// Numeric fields are serialized as JSON strings per z/OSMF convention.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DatasetListItem {
    /// Dataset name.
    pub dsname: String,
    /// Dataset organization (PS, PO, VS, etc.).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dsorg: Option<String>,
    /// Record format (F, FB, V, VB, U).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub recfm: Option<String>,
    /// Logical record length (as string per z/OSMF convention).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lrecl: Option<String>,
    /// Block size (as string per z/OSMF convention).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub blksz: Option<String>,
    /// Volume serial.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub vol: Option<String>,
    /// Creation date.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cdate: Option<String>,
    /// Last reference date.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rdate: Option<String>,
    /// Catalog name.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub catnm: Option<String>,
}

/// Response body for PDS member list operations.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MemberListResponse {
    /// List of member items.
    pub items: Vec<MemberListItem>,
    /// Number of rows returned.
    pub returned_rows: usize,
    /// Total rows.
    pub total_rows: usize,
    /// JSON format version.
    #[serde(rename = "JSONversion")]
    pub json_version: i32,
}

/// A PDS member entry with optional ISPF statistics.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemberListItem {
    /// Member name.
    pub member: String,
    /// Version number.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub vers: Option<u32>,
    /// Modification level.
    #[serde(rename = "mod", skip_serializing_if = "Option::is_none")]
    pub modification: Option<u32>,
    /// Created date (4-digit format).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub c4date: Option<String>,
    /// Modified date (4-digit format).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub m4date: Option<String>,
    /// Current number of records.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cnorc: Option<u32>,
    /// Initial number of records.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inorc: Option<u32>,
}

/// Parameters for creating a new dataset.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DatasetCreateParams {
    /// Dataset organization.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dsorg: Option<String>,
    /// Record format.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub recfm: Option<String>,
    /// Logical record length.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lrecl: Option<u32>,
    /// Block size.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub blksz: Option<u32>,
    /// Primary space allocation.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub primary: Option<u32>,
    /// Secondary space allocation.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub secondary: Option<u32>,
    /// Allocation unit (TRK, CYL, BLK).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub alcunit: Option<String>,
}

/// Query parameters for dataset list.
#[derive(Debug, Clone, Deserialize)]
pub struct DatasetListQuery {
    /// Dataset level filter pattern (e.g., `HLQ.*`).
    #[serde(rename = "dslevel")]
    pub dslevel: String,
}
