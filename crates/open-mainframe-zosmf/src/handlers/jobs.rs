//! /zosmf/restjobs/jobs/* — job management REST API endpoints.
//!
//! Implements the z/OSMF job REST services:
//! - `GET    /zosmf/restjobs/jobs` — list jobs by owner/prefix
//! - `GET    /zosmf/restjobs/jobs/:jobname/:jobid` — get job status
//! - `PUT    /zosmf/restjobs/jobs` — submit JCL
//! - `GET    /zosmf/restjobs/jobs/:jobname/:jobid/files` — list spool files
//! - `GET    /zosmf/restjobs/jobs/:jobname/:jobid/files/:id/records` — read spool content
//! - `PUT    /zosmf/restjobs/jobs/:jobname/:jobid` — hold/release/cancel job
//! - `DELETE /zosmf/restjobs/jobs/:jobname/:jobid` — purge job

use std::sync::Arc;

use axum::body::Body;
use axum::extract::{Path, Query, State};
use axum::http::StatusCode;
use axum::response::IntoResponse;
use axum::routing::{delete, get, put};
use axum::{Json, Router};
use open_mainframe_jes2::{Job, JobClass, JobId, JobState};

use crate::state::AppState;
use crate::types::auth::AuthContext;
use crate::types::error::ZosmfErrorResponse;
use crate::types::jobs::{
    JobActionRequest, JobFeedback, JobListQuery, JobResponse, JobSubmitResponse, SpoolFile,
};

/// Register job routes.
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
        .route("/zosmf/restjobs/jobs", get(list_jobs))
        .route("/zosmf/restjobs/jobs", put(submit_job))
        .route("/zosmf/restjobs/jobs/{jobname}/{jobid}", get(get_job_status))
        .route(
            "/zosmf/restjobs/jobs/{jobname}/{jobid}",
            put(job_action),
        )
        .route(
            "/zosmf/restjobs/jobs/{jobname}/{jobid}",
            delete(purge_job),
        )
        .route(
            "/zosmf/restjobs/jobs/{jobname}/{jobid}/files",
            get(list_spool_files),
        )
        .route(
            "/zosmf/restjobs/jobs/{jobname}/{jobid}/files/{file_id}/records",
            get(read_spool_content),
        )
}

/// Format a JobId as z/OSMF string (e.g., "JOB00042").
fn format_job_id(id: JobId) -> String {
    format!("JOB{:05}", id.0)
}

/// Parse a z/OSMF job ID string back to JobId.
fn parse_job_id(s: &str) -> Option<JobId> {
    let num_str = s.strip_prefix("JOB").or_else(|| s.strip_prefix("STC")).or_else(|| s.strip_prefix("TSU"))?;
    let num: u32 = num_str.parse().ok()?;
    Some(JobId(num))
}

/// Map JobState to z/OSMF status string.
fn job_state_to_status(state: &JobState) -> &'static str {
    match state {
        JobState::Input | JobState::Conversion | JobState::Ready => "INPUT",
        JobState::Running => "ACTIVE",
        JobState::Output | JobState::Purge => "OUTPUT",
        JobState::Held { .. } => "INPUT",
        JobState::Cancelled => "OUTPUT",
    }
}

/// Map JobClass to z/OSMF type string.
fn job_class_to_type(class: &JobClass) -> &'static str {
    match class {
        JobClass::Standard(_) => "JOB",
        JobClass::Stc => "STC",
        JobClass::Tsu => "TSU",
    }
}

/// Map JobClass to string.
fn job_class_to_string(class: &JobClass) -> String {
    match class {
        JobClass::Standard(c) => c.to_string(),
        JobClass::Stc => "STC".to_string(),
        JobClass::Tsu => "TSU".to_string(),
    }
}

/// Map Job state to phase number.
fn job_state_to_phase(state: &JobState) -> i32 {
    match state {
        JobState::Input => 1,
        JobState::Conversion => 2,
        JobState::Ready => 3,
        JobState::Running => 10,
        JobState::Output => 20,
        JobState::Purge => 30,
        JobState::Held { .. } => 1,
        JobState::Cancelled => 20,
    }
}

/// Map Job state to phase name.
fn job_state_to_phase_name(state: &JobState) -> &'static str {
    match state {
        JobState::Input => "Job is awaiting execution",
        JobState::Conversion => "Job is in conversion",
        JobState::Ready => "Job is ready for execution",
        JobState::Running => "Job is actively executing",
        JobState::Output => "Job is on the output queue",
        JobState::Purge => "Job is being purged",
        JobState::Held { .. } => "Job is held",
        JobState::Cancelled => "Job has been cancelled",
    }
}

/// Convert a Job to JobResponse.
fn job_to_response(job: &Job) -> JobResponse {
    let jobid = format_job_id(job.id);
    JobResponse {
        jobid: jobid.clone(),
        jobname: job.name.clone(),
        owner: job.owner.clone(),
        status: job_state_to_status(&job.state).to_string(),
        job_type: job_class_to_type(&job.class).to_string(),
        class: job_class_to_string(&job.class),
        retcode: if job.state == JobState::Output || job.state == JobState::Cancelled {
            Some(format!("CC {:04}", job.max_rc))
        } else {
            None
        },
        url: Some(format!(
            "/zosmf/restjobs/jobs/{}/{}",
            job.name, jobid
        )),
        files_url: Some(format!(
            "/zosmf/restjobs/jobs/{}/{}/files",
            job.name, jobid
        )),
        phase: Some(job_state_to_phase(&job.state)),
        phase_name: Some(job_state_to_phase_name(&job.state).to_string()),
    }
}

/// Check if a job name matches a wildcard pattern.
fn matches_prefix(name: &str, pattern: &str) -> bool {
    if pattern == "*" {
        return true;
    }
    if let Some(prefix) = pattern.strip_suffix('*') {
        name.to_uppercase().starts_with(&prefix.to_uppercase())
    } else {
        name.eq_ignore_ascii_case(pattern)
    }
}

/// GET /zosmf/restjobs/jobs — list jobs by owner/prefix.
async fn list_jobs(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    Query(query): Query<JobListQuery>,
) -> std::result::Result<Json<Vec<JobResponse>>, ZosmfErrorResponse> {
    let jes = state
        .jes2
        .read()
        .map_err(|_| ZosmfErrorResponse::internal("JES2 lock poisoned"))?;

    let owner_filter = query.owner.as_deref().unwrap_or(&auth.userid);
    let prefix_filter = query.prefix.as_deref().unwrap_or("*");

    let jobs: Vec<JobResponse> = jes
        .all_jobs()
        .into_iter()
        .filter(|job| {
            let owner_match = owner_filter == "*"
                || job.owner.eq_ignore_ascii_case(owner_filter);
            let prefix_match = matches_prefix(&job.name, prefix_filter);
            let status_match = query
                .status
                .as_ref()
                .map(|s| job_state_to_status(&job.state) == s.to_uppercase())
                .unwrap_or(true);
            let jobid_match = query
                .jobid
                .as_ref()
                .map(|id| format_job_id(job.id) == id.to_uppercase())
                .unwrap_or(true);
            owner_match && prefix_match && status_match && jobid_match
        })
        .map(job_to_response)
        .collect();

    Ok(Json(jobs))
}

/// GET /zosmf/restjobs/jobs/:jobname/:jobid — get job status.
async fn get_job_status(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path((jobname, jobid)): Path<(String, String)>,
) -> std::result::Result<Json<JobResponse>, ZosmfErrorResponse> {
    let jes = state
        .jes2
        .read()
        .map_err(|_| ZosmfErrorResponse::internal("JES2 lock poisoned"))?;

    let id = parse_job_id(&jobid).ok_or_else(|| {
        ZosmfErrorResponse::not_found(format!("Invalid job ID: {}", jobid))
    })?;

    let job = jes.get_job(id).ok_or_else(|| {
        ZosmfErrorResponse::not_found(format!("Job {} ({}) not found", jobname, jobid))
    })?;

    Ok(Json(job_to_response(job)))
}

/// PUT /zosmf/restjobs/jobs — submit JCL.
async fn submit_job(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    body: Body,
) -> std::result::Result<(StatusCode, Json<JobSubmitResponse>), ZosmfErrorResponse> {
    let bytes = axum::body::to_bytes(body, 10 * 1024 * 1024)
        .await
        .map_err(|_| ZosmfErrorResponse::bad_request("Failed to read request body"))?;

    let jcl = String::from_utf8(bytes.to_vec())
        .map_err(|_| ZosmfErrorResponse::bad_request("Invalid UTF-8 in JCL body"))?;

    // Extract job name and class from JCL.
    let job_name = extract_job_name(&jcl).unwrap_or_else(|| "NONAME".to_string());
    let job_class = extract_class(&jcl).unwrap_or('A');

    let mut jes = state
        .jes2
        .write()
        .map_err(|_| ZosmfErrorResponse::internal("JES2 lock poisoned"))?;

    let job_id = jes.submit(&job_name, job_class, 1, false);

    // Set the job owner.
    if let Some(job) = jes.get_job_mut(job_id) {
        job.owner = auth.userid.clone();
    }

    // Write JCL to spool.
    let spool_key = jes.spool.allocate(job_id, "JESJCL", "JCL", 'A');
    for line in jcl.lines() {
        let _ = jes.spool.write(spool_key, line);
    }

    let jobid_str = format_job_id(job_id);

    Ok((
        StatusCode::CREATED,
        Json(JobSubmitResponse {
            jobid: jobid_str,
            jobname: job_name,
            owner: auth.userid,
            status: "INPUT".to_string(),
        }),
    ))
}

/// GET /zosmf/restjobs/jobs/:jobname/:jobid/files — list spool files.
async fn list_spool_files(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path((jobname, jobid)): Path<(String, String)>,
) -> std::result::Result<Json<Vec<SpoolFile>>, ZosmfErrorResponse> {
    let jes = state
        .jes2
        .read()
        .map_err(|_| ZosmfErrorResponse::internal("JES2 lock poisoned"))?;

    let id = parse_job_id(&jobid).ok_or_else(|| {
        ZosmfErrorResponse::not_found(format!("Invalid job ID: {}", jobid))
    })?;

    let _job = jes.get_job(id).ok_or_else(|| {
        ZosmfErrorResponse::not_found(format!("Job {} ({}) not found", jobname, jobid))
    })?;

    let spool_datasets = jes.spool.list_for_job(id);

    let files: Vec<SpoolFile> = spool_datasets
        .into_iter()
        .enumerate()
        .map(|(idx, sd)| {
            let record_count = sd.record_count;
            let byte_count: u64 = sd.data.iter().map(|l| l.len() as u64 + 1).sum();

            SpoolFile {
                id: idx as u32,
                ddname: sd.dd_name.clone(),
                stepname: Some(sd.step_name.clone()),
                procstep: None,
                class: sd.sysout_class.to_string(),
                byte_count,
                record_count,
                records_url: Some(format!(
                    "/zosmf/restjobs/jobs/{}/{}/files/{}/records",
                    jobname, jobid, idx
                )),
            }
        })
        .collect();

    Ok(Json(files))
}

/// GET /zosmf/restjobs/jobs/:jobname/:jobid/files/:id/records — read spool content.
async fn read_spool_content(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path((_jobname, jobid, file_id)): Path<(String, String, u32)>,
) -> std::result::Result<impl IntoResponse, ZosmfErrorResponse> {
    let jes = state
        .jes2
        .read()
        .map_err(|_| ZosmfErrorResponse::internal("JES2 lock poisoned"))?;

    let id = parse_job_id(&jobid).ok_or_else(|| {
        ZosmfErrorResponse::not_found(format!("Invalid job ID: {}", jobid))
    })?;

    let spool_datasets = jes.spool.list_for_job(id);

    let sd = spool_datasets.get(file_id as usize).ok_or_else(|| {
        ZosmfErrorResponse::not_found(format!("Spool file {} not found", file_id))
    })?;

    let content = sd.data.join("\n");

    Ok((
        StatusCode::OK,
        [("content-type", "text/plain; charset=UTF-8")],
        content,
    ))
}

/// PUT /zosmf/restjobs/jobs/:jobname/:jobid — hold/release/cancel job.
async fn job_action(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path((jobname, jobid)): Path<(String, String)>,
    Json(action): Json<JobActionRequest>,
) -> std::result::Result<Json<JobFeedback>, ZosmfErrorResponse> {
    let id = parse_job_id(&jobid).ok_or_else(|| {
        ZosmfErrorResponse::not_found(format!("Invalid job ID: {}", jobid))
    })?;

    let mut jes = state
        .jes2
        .write()
        .map_err(|_| ZosmfErrorResponse::internal("JES2 lock poisoned"))?;

    match action.request.to_lowercase().as_str() {
        "hold" => {
            jes.hold(id).map_err(|e| {
                ZosmfErrorResponse::bad_request(format!("Cannot hold job: {}", e))
            })?;
        }
        "release" => {
            jes.release(id).map_err(|e| {
                ZosmfErrorResponse::bad_request(format!("Cannot release job: {}", e))
            })?;
        }
        "cancel" => {
            jes.cancel(id).map_err(|e| {
                ZosmfErrorResponse::bad_request(format!("Cannot cancel job: {}", e))
            })?;
        }
        other => {
            return Err(ZosmfErrorResponse::bad_request(format!(
                "Unknown action: {}",
                other
            )));
        }
    }

    Ok(Json(JobFeedback {
        jobid: jobid.clone(),
        jobname,
        status: 0,
        message: format!("Job {} {} successfully", jobid, action.request),
    }))
}

/// DELETE /zosmf/restjobs/jobs/:jobname/:jobid — purge job and output.
async fn purge_job(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path((jobname, jobid)): Path<(String, String)>,
) -> std::result::Result<Json<JobFeedback>, ZosmfErrorResponse> {
    let id = parse_job_id(&jobid).ok_or_else(|| {
        ZosmfErrorResponse::not_found(format!("Invalid job ID: {}", jobid))
    })?;

    let mut jes = state
        .jes2
        .write()
        .map_err(|_| ZosmfErrorResponse::internal("JES2 lock poisoned"))?;

    jes.purge(id).map_err(|e| {
        ZosmfErrorResponse::not_found(format!("Cannot purge job: {}", e))
    })?;

    Ok(Json(JobFeedback {
        jobid: jobid.clone(),
        jobname,
        status: 0,
        message: format!("Job {} purged", jobid),
    }))
}

/// Extract job name from JCL (first //name JOB card).
fn extract_job_name(jcl: &str) -> Option<String> {
    for line in jcl.lines() {
        let line = line.trim();
        if line.starts_with("//") && !line.starts_with("//*") {
            let rest = &line[2..];
            if let Some(space_pos) = rest.find(|c: char| c.is_whitespace()) {
                let name = &rest[..space_pos];
                let after = rest[space_pos..].trim_start();
                if after.starts_with("JOB") {
                    return Some(name.to_uppercase());
                }
            }
        }
    }
    None
}

/// Extract job class from JCL JOB card CLASS parameter.
fn extract_class(jcl: &str) -> Option<char> {
    for line in jcl.lines() {
        let line_upper = line.to_uppercase();
        if let Some(pos) = line_upper.find("CLASS=") {
            let after = &line_upper[pos + 6..];
            return after.chars().next().filter(|c| c.is_ascii_alphanumeric());
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_job_id() {
        assert_eq!(format_job_id(JobId(42)), "JOB00042");
        assert_eq!(format_job_id(JobId(1)), "JOB00001");
        assert_eq!(format_job_id(JobId(99999)), "JOB99999");
    }

    #[test]
    fn test_parse_job_id() {
        assert_eq!(parse_job_id("JOB00042"), Some(JobId(42)));
        assert_eq!(parse_job_id("STC00001"), Some(JobId(1)));
        assert_eq!(parse_job_id("INVALID"), None);
    }

    #[test]
    fn test_extract_job_name() {
        let jcl = "//PAYROLL  JOB (ACCT),'PAYROLL RUN',CLASS=A\n//STEP1 EXEC PGM=IEFBR14";
        assert_eq!(extract_job_name(jcl), Some("PAYROLL".to_string()));
    }

    #[test]
    fn test_extract_job_name_no_job_card() {
        let jcl = "//STEP1 EXEC PGM=IEFBR14";
        assert_eq!(extract_job_name(jcl), None);
    }

    #[test]
    fn test_extract_class() {
        let jcl = "//MYJOB JOB (ACCT),'TEST',CLASS=B,MSGCLASS=X";
        assert_eq!(extract_class(jcl), Some('B'));
    }

    #[test]
    fn test_extract_class_missing() {
        let jcl = "//MYJOB JOB (ACCT),'TEST'";
        assert_eq!(extract_class(jcl), None);
    }

    #[test]
    fn test_matches_prefix() {
        assert!(matches_prefix("PAYROLL", "PAY*"));
        assert!(matches_prefix("PAYROLL", "*"));
        assert!(matches_prefix("PAYROLL", "PAYROLL"));
        assert!(!matches_prefix("PAYROLL", "PAY"));
        assert!(!matches_prefix("MYJOB", "PAY*"));
    }

    #[test]
    fn test_job_state_to_status() {
        assert_eq!(job_state_to_status(&JobState::Input), "INPUT");
        assert_eq!(job_state_to_status(&JobState::Running), "ACTIVE");
        assert_eq!(job_state_to_status(&JobState::Output), "OUTPUT");
    }

    #[test]
    fn test_job_response_serialization() {
        let resp = JobResponse {
            jobid: "JOB00042".to_string(),
            jobname: "PAYROLL".to_string(),
            owner: "IBMUSER".to_string(),
            status: "ACTIVE".to_string(),
            job_type: "JOB".to_string(),
            class: "A".to_string(),
            retcode: None,
            url: None,
            files_url: None,
            phase: Some(10),
            phase_name: Some("Job is actively executing".to_string()),
        };

        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"jobid\":\"JOB00042\""));
        assert!(json.contains("\"type\":\"JOB\""));
        assert!(json.contains("\"phase-name\":\"Job is actively executing\""));
    }
}
