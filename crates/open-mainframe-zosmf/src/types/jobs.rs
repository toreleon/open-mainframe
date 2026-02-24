//! z/OSMF jobs REST API types.

use serde::{Deserialize, Serialize};

/// A job entry in list, status, and submit responses.
///
/// Real z/OSMF returns the same full object for submit, list, and status.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobResponse {
    /// Job ID (e.g., JOB00042).
    pub jobid: String,
    /// Job name.
    pub jobname: String,
    /// Job owner userid.
    pub owner: String,
    /// Job status: INPUT, ACTIVE, or OUTPUT.
    pub status: String,
    /// Job type (JOB, STC, TSU).
    #[serde(rename = "type")]
    pub job_type: String,
    /// Job class.
    pub class: String,
    /// Return code (e.g., "CC 0000"), null while active.
    pub retcode: Option<String>,
    /// Subsystem (JES2 or JES3).
    pub subsystem: String,
    /// Job correlator — unique opaque identifier.
    #[serde(rename = "job-correlator")]
    pub job_correlator: String,
    /// URL for this job resource.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
    /// URL to spool files.
    #[serde(rename = "files-url", skip_serializing_if = "Option::is_none")]
    pub files_url: Option<String>,
    /// Current phase number.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub phase: Option<i32>,
    /// Phase name.
    #[serde(rename = "phase-name", skip_serializing_if = "Option::is_none")]
    pub phase_name: Option<String>,
    /// Execution started timestamp.
    #[serde(rename = "exec-started", skip_serializing_if = "Option::is_none")]
    pub exec_started: Option<String>,
    /// Execution ended timestamp.
    #[serde(rename = "exec-ended", skip_serializing_if = "Option::is_none")]
    pub exec_ended: Option<String>,
    /// Execution member (system member name).
    #[serde(rename = "exec-member", skip_serializing_if = "Option::is_none")]
    pub exec_member: Option<String>,
    /// Execution submitted timestamp.
    #[serde(rename = "exec-submitted", skip_serializing_if = "Option::is_none")]
    pub exec_submitted: Option<String>,
    /// Execution system name.
    #[serde(rename = "exec-system", skip_serializing_if = "Option::is_none")]
    pub exec_system: Option<String>,
    /// Reason not running.
    #[serde(rename = "reason-not-running", skip_serializing_if = "Option::is_none")]
    pub reason_not_running: Option<String>,
    /// Step data (populated when exec-data=Y).
    #[serde(rename = "step-data", skip_serializing_if = "Option::is_none")]
    pub step_data: Option<Vec<JobStepData>>,
}

/// Step data for a job execution step.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobStepData {
    /// SMF system ID.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub smfid: Option<String>,
    /// Completion code.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub completion: Option<String>,
    /// Step name.
    #[serde(rename = "step-name", skip_serializing_if = "Option::is_none")]
    pub step_name: Option<String>,
    /// Proc step name.
    #[serde(rename = "proc-step-name", skip_serializing_if = "Option::is_none")]
    pub proc_step_name: Option<String>,
    /// Program name.
    #[serde(rename = "program-name", skip_serializing_if = "Option::is_none")]
    pub program_name: Option<String>,
    /// Whether step is active.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub active: Option<bool>,
}

/// A spool file entry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpoolFile {
    /// Job ID this spool file belongs to.
    pub jobid: String,
    /// Job name.
    pub jobname: String,
    /// Spool file numeric ID.
    pub id: u32,
    /// DD name.
    pub ddname: String,
    /// Step name.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stepname: Option<String>,
    /// Proc step name.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub procstep: Option<String>,
    /// Output class.
    pub class: String,
    /// Record format (F, FB, V, VB, U).
    pub recfm: String,
    /// Logical record length.
    pub lrecl: u32,
    /// Byte count.
    #[serde(rename = "byte-count")]
    pub byte_count: u64,
    /// Record count.
    #[serde(rename = "record-count")]
    pub record_count: u64,
    /// Job correlator.
    #[serde(rename = "job-correlator")]
    pub job_correlator: String,
    /// Subsystem (JES2).
    pub subsystem: String,
    /// URL to fetch records.
    #[serde(rename = "records-url", skip_serializing_if = "Option::is_none")]
    pub records_url: Option<String>,
}

/// Job action request body (hold, release, cancel).
#[derive(Debug, Clone, Deserialize)]
pub struct JobActionRequest {
    /// Requested action: "hold", "release", or "cancel".
    pub request: String,
    /// Version for the request format.
    #[serde(default)]
    pub version: Option<String>,
}

/// Feedback response for job actions.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobFeedback {
    /// Job ID.
    pub jobid: String,
    /// Job name.
    pub jobname: String,
    /// Status code (0 = success).
    pub status: i32,
    /// Feedback message.
    pub message: String,
    /// Original job ID.
    #[serde(rename = "original-jobid", skip_serializing_if = "Option::is_none")]
    pub original_jobid: Option<String>,
    /// Job owner.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub owner: Option<String>,
    /// System member name.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub member: Option<String>,
    /// System name.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sysname: Option<String>,
    /// Job correlator.
    #[serde(rename = "job-correlator", skip_serializing_if = "Option::is_none")]
    pub job_correlator: Option<String>,
    /// Internal code.
    #[serde(rename = "internal-code", skip_serializing_if = "Option::is_none")]
    pub internal_code: Option<String>,
}

/// Query parameters for job list.
#[derive(Debug, Clone, Deserialize)]
pub struct JobListQuery {
    /// Owner filter.
    #[serde(default)]
    pub owner: Option<String>,
    /// Prefix filter (supports `*` wildcard).
    #[serde(default)]
    pub prefix: Option<String>,
    /// Job ID filter.
    #[serde(default)]
    pub jobid: Option<String>,
    /// Status filter (INPUT, ACTIVE, OUTPUT).
    #[serde(default)]
    pub status: Option<String>,
    /// Max jobs to return (0 = unlimited).
    #[serde(rename = "max-jobs", default)]
    pub max_jobs: Option<usize>,
    /// Include execution data (Y/N).
    #[serde(rename = "exec-data", default)]
    pub exec_data: Option<String>,
    /// Filter by execution system name.
    #[serde(rename = "exec-member", default)]
    pub exec_member: Option<String>,
}
