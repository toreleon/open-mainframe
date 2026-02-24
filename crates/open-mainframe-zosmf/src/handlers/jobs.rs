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
use open_mainframe_dataset::Pds;
use open_mainframe_jes2::{Job, JobClass, JobId, JobState};
use serde::Deserialize;

use crate::state::AppState;
use crate::types::auth::AuthContext;
use crate::types::error::ZosmfErrorResponse;
use crate::types::jobs::{
    JobActionRequest, JobFeedback, JobListQuery, JobResponse, SpoolFile,
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

/// Generate a job correlator string.
fn generate_job_correlator(job: &Job) -> String {
    let jobid = format_job_id(job.id);
    format!(
        "{}{}{}00000000",
        jobid,
        job.name,
        "D00000000000000000000000".get(..24 - job.name.len()).unwrap_or("")
    )
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
        subsystem: "JES2".to_string(),
        job_correlator: generate_job_correlator(job),
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
        exec_started: None,
        exec_ended: None,
        exec_member: job.system.clone(),
        exec_submitted: None,
        exec_system: job.system.clone(),
        reason_not_running: None,
        step_data: None,
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

    let max_jobs: usize = query
        .max_jobs
        .map(|n| if n == 0 { usize::MAX } else { n })
        .unwrap_or(usize::MAX);

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
            let system_match = query
                .exec_member
                .as_ref()
                .map(|sys| {
                    job.system
                        .as_ref()
                        .map(|s| s.eq_ignore_ascii_case(sys))
                        .unwrap_or(false)
                })
                .unwrap_or(true);
            owner_match && prefix_match && status_match && jobid_match && system_match
        })
        .take(max_jobs)
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
    headers: axum::http::HeaderMap,
    body: Body,
) -> std::result::Result<(StatusCode, Json<JobResponse>), ZosmfErrorResponse> {
    let bytes = axum::body::to_bytes(body, 10 * 1024 * 1024)
        .await
        .map_err(|_| ZosmfErrorResponse::bad_request("Failed to read request body"))?;

    // Check if JSON body with "file" field for dataset-based submission.
    let jcl = if let Ok(submit_req) = serde_json::from_slice::<JobSubmitFromDataset>(&bytes) {
        read_jcl_from_dataset(&state, &submit_req.file)?
    } else {
        String::from_utf8(bytes.to_vec())
            .map_err(|_| ZosmfErrorResponse::bad_request("Invalid UTF-8 in JCL body"))?
    };

    // Extract job name and class from JCL.
    let job_name = extract_job_name(&jcl).unwrap_or_else(|| "NONAME".to_string());
    let job_class = extract_class(&jcl).unwrap_or('A');

    // Determine target system: check X-IBM-Target-System header, then /*ROUTE XEQ in JCL.
    let target_system = headers
        .get("X-IBM-Target-System")
        .and_then(|v| v.to_str().ok())
        .map(|s| s.to_uppercase())
        .or_else(|| extract_route_xeq(&jcl));

    // Get the dataset base directory. If a target system is specified and has its own
    // dataset_dir, use that; otherwise fall back to the catalog's base directory.
    let dataset_dir = if let Some(ref sys_name) = target_system {
        let sysplex = state.sysplex.read().map_err(|_| {
            ZosmfErrorResponse::internal("Sysplex lock poisoned")
        })?;
        if let Some(sys) = sysplex.get_system(sys_name) {
            if let Some(ref dir) = sys.dataset_dir {
                let p = std::path::PathBuf::from(dir);
                let _ = std::fs::create_dir_all(&p);
                p
            } else {
                let catalog = state.catalog.read().map_err(|_| {
                    ZosmfErrorResponse::internal("Catalog lock poisoned")
                })?;
                catalog.base_dir().to_path_buf()
            }
        } else {
            return Err(ZosmfErrorResponse::not_found(format!(
                "Target system '{}' not found in sysplex", sys_name
            )));
        }
    } else {
        let catalog = state.catalog.read().map_err(|_| {
            ZosmfErrorResponse::internal("Catalog lock poisoned")
        })?;
        catalog.base_dir().to_path_buf()
    };

    let mut jes = state
        .jes2
        .write()
        .map_err(|_| ZosmfErrorResponse::internal("JES2 lock poisoned"))?;

    let job_id = jes.submit(&job_name, job_class, 1, false);

    // Set the job owner and target system.
    if let Some(job) = jes.get_job_mut(job_id) {
        job.owner = auth.userid.clone();
        job.system = target_system.clone();
    }

    // Write JCL to spool.
    let spool_key = jes.spool.allocate(job_id, "JESJCL", "JCL", 'A');
    for line in jcl.lines() {
        let _ = jes.spool.write(spool_key, line);
    }

    // --- Execute the JCL ---
    // Transition job to Running state.
    if let Some(job) = jes.get_job_mut(job_id) {
        job.state = JobState::Running;
    }

    // Configure the JCL executor with the resolved dataset directory.
    // Include dataset overrides from the mount table so JCL jobs can access mounted datasets.
    let dataset_overrides = state
        .mount_table
        .read()
        .map(|mt| mt.dataset_overrides())
        .unwrap_or_default();
    let exec_config = open_mainframe_jcl::ExecutionConfig {
        program_dir: dataset_dir.join("..").join("bin"),
        dataset_dir: dataset_dir.clone(),
        work_dir: std::env::temp_dir().join(format!("openmainframe-work-{}", job_id.0)),
        sysout_dir: std::env::temp_dir().join(format!("openmainframe-sysout-{}", job_id.0)),
        dataset_overrides,
    };

    // Parse and execute the JCL.
    let exec_result = open_mainframe_jcl::run_with_config(&jcl, exec_config);

    // Write execution output to spool and update job state.
    let max_rc = match &exec_result {
        Ok(result) => {
            // Write JESMSGLG with step results
            let msg_key = jes.spool.allocate(job_id, "JESMSGLG", "JES2", 'A');
            let _ = jes.spool.write(msg_key, &format!(
                " JOB {} {} -- {} STEPS EXECUTED",
                format_job_id(job_id),
                job_name,
                result.steps.len()
            ));
            for step in &result.steps {
                let step_name = step.name.as_deref().unwrap_or("?");
                let _ = jes.spool.write(msg_key, &format!(
                    " STEP {} RC={:04}",
                    step_name, step.return_code
                ));
                // Write step stdout if non-empty
                if !step.stdout.is_empty() {
                    let out_key = jes.spool.allocate(
                        job_id,
                        "SYSPRINT",
                        step_name,
                        'A',
                    );
                    for line in step.stdout.lines() {
                        let _ = jes.spool.write(out_key, line);
                    }
                }
                // Write step stderr if non-empty
                if !step.stderr.is_empty() {
                    let err_key = jes.spool.allocate(
                        job_id,
                        "SYSOUT",
                        step_name,
                        'A',
                    );
                    for line in step.stderr.lines() {
                        let _ = jes.spool.write(err_key, line);
                    }
                }
            }
            let _ = jes.spool.write(msg_key, &format!(
                " JOB {} ENDED -- RC={:04}",
                format_job_id(job_id),
                result.return_code
            ));
            result.return_code
        }
        Err(e) => {
            // Write error to spool
            let msg_key = jes.spool.allocate(job_id, "JESMSGLG", "JES2", 'A');
            let _ = jes.spool.write(msg_key, &format!(
                " JOB {} {} -- JCL ERROR: {}",
                format_job_id(job_id),
                job_name,
                e
            ));
            12 // JCL error return code
        }
    };

    // Transition job to Output state with the return code.
    if let Some(job) = jes.get_job_mut(job_id) {
        job.state = JobState::Output;
        job.max_rc = max_rc;
    }

    // Return full JobResponse (same as list/status) per real z/OSMF behavior.
    let job = jes.get_job(job_id).ok_or_else(|| {
        ZosmfErrorResponse::internal("Job disappeared after submit")
    })?;
    let response = job_to_response(job);

    Ok((StatusCode::CREATED, Json(response)))
}

/// JSON body for submitting a job from a dataset.
#[derive(Debug, Clone, Deserialize)]
struct JobSubmitFromDataset {
    /// Dataset path, e.g. "//'IBMUSER.JCL(PAYROLL)'" or "//'IBMUSER.DATA'".
    file: String,
}

/// Read JCL content from a dataset reference like "//'DSN.NAME(MEMBER)'".
fn read_jcl_from_dataset(
    state: &AppState,
    file_ref: &str,
) -> std::result::Result<String, ZosmfErrorResponse> {
    // Strip "//" prefix and surrounding quotes: "//'DSN.NAME(MEM)'" => "DSN.NAME(MEM)"
    let dsn_str = file_ref
        .trim_start_matches('/')
        .trim_start_matches('/')
        .trim_matches('\'')
        .trim_matches('"');

    let (ds_name, member) = if let Some(paren_pos) = dsn_str.find('(') {
        let ds = &dsn_str[..paren_pos];
        let member_end = dsn_str.find(')').unwrap_or(dsn_str.len());
        let mem = &dsn_str[paren_pos + 1..member_end];
        (ds, Some(mem))
    } else {
        (dsn_str, None)
    };

    let catalog = state
        .catalog
        .read()
        .map_err(|_| ZosmfErrorResponse::internal("Catalog lock poisoned"))?;

    let dsref = catalog.lookup(ds_name).map_err(|_| {
        ZosmfErrorResponse::not_found(format!("Dataset '{}' not found", ds_name))
    })?;

    let path = dsref
        .path
        .as_ref()
        .ok_or_else(|| ZosmfErrorResponse::not_found("Dataset path not resolved"))?;

    let content = if let Some(mem_name) = member {
        let pds = Pds::open(path).map_err(|_| {
            ZosmfErrorResponse::not_found(format!("PDS '{}' not found", ds_name))
        })?;
        pds.read_member(mem_name).map_err(|_| {
            ZosmfErrorResponse::not_found(format!(
                "Member '{}({})' not found",
                ds_name, mem_name
            ))
        })?
    } else {
        std::fs::read(path).map_err(|_| {
            ZosmfErrorResponse::not_found(format!("Cannot read dataset '{}'", ds_name))
        })?
    };

    String::from_utf8(content)
        .map_err(|_| ZosmfErrorResponse::bad_request("Dataset content is not valid UTF-8 JCL"))
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

    let job = jes.get_job(id).ok_or_else(|| {
        ZosmfErrorResponse::not_found(format!("Job {} ({}) not found", jobname, jobid))
    })?;
    let correlator = generate_job_correlator(job);

    let spool_datasets = jes.spool.list_for_job(id);

    let files: Vec<SpoolFile> = spool_datasets
        .into_iter()
        .enumerate()
        .map(|(idx, sd)| {
            let record_count = sd.record_count;
            let byte_count: u64 = sd.data.iter().map(|l| l.len() as u64 + 1).sum();

            SpoolFile {
                jobid: jobid.clone(),
                jobname: jobname.clone(),
                id: idx as u32,
                ddname: sd.dd_name.clone(),
                stepname: Some(sd.step_name.clone()),
                procstep: None,
                class: sd.sysout_class.to_string(),
                recfm: "VB".to_string(),
                lrecl: 137,
                byte_count,
                record_count,
                job_correlator: correlator.clone(),
                subsystem: "JES2".to_string(),
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
        original_jobid: None,
        owner: None,
        member: None,
        sysname: None,
        job_correlator: None,
        internal_code: None,
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
        original_jobid: None,
        owner: None,
        member: None,
        sysname: None,
        job_correlator: None,
        internal_code: None,
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

/// Extract target system from JES2 JECL `/*ROUTE XEQ sysname` statement.
fn extract_route_xeq(jcl: &str) -> Option<String> {
    for line in jcl.lines() {
        let trimmed = line.trim().to_uppercase();
        if trimmed.starts_with("/*ROUTE") {
            // Parse: /*ROUTE XEQ sysname
            let parts: Vec<&str> = trimmed.split_whitespace().collect();
            if parts.len() >= 3 && parts[1] == "XEQ" {
                return Some(parts[2].to_string());
            }
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
            subsystem: "JES2".to_string(),
            job_correlator: "JOB00042PAYROLLD00000000000000000".to_string(),
            url: None,
            files_url: None,
            phase: Some(10),
            phase_name: Some("Job is actively executing".to_string()),
            exec_started: None,
            exec_ended: None,
            exec_member: None,
            exec_submitted: None,
            exec_system: None,
            reason_not_running: None,
            step_data: None,
        };

        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"jobid\":\"JOB00042\""));
        assert!(json.contains("\"type\":\"JOB\""));
        assert!(json.contains("\"subsystem\":\"JES2\""));
        assert!(json.contains("\"job-correlator\""));
        assert!(json.contains("\"phase-name\":\"Job is actively executing\""));
    }

    #[test]
    fn test_extract_route_xeq() {
        let jcl = "/*ROUTE XEQ SYSA\n//MYJOB JOB (ACCT),'TEST'\n//STEP1 EXEC PGM=IEFBR14";
        assert_eq!(extract_route_xeq(jcl), Some("SYSA".to_string()));
    }

    #[test]
    fn test_extract_route_xeq_missing() {
        let jcl = "//MYJOB JOB (ACCT),'TEST'\n//STEP1 EXEC PGM=IEFBR14";
        assert_eq!(extract_route_xeq(jcl), None);
    }

    #[test]
    fn test_extract_route_xeq_case_insensitive() {
        let jcl = "/*route xeq sysb\n//JOB1 JOB (A)";
        assert_eq!(extract_route_xeq(jcl), Some("SYSB".to_string()));
    }
}
