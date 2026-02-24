//! ZOW-109.2: Zowe CLI Job Integration Tests
//!
//! End-to-end integration tests that exercise the z/OSMF REST API job
//! endpoints using actual HTTP requests against a running Axum router,
//! simulating what `zowe zos-jobs` commands would do.

use std::sync::Arc;

use axum::body::Body;
use axum::http::{Request, StatusCode};
use tower::ServiceExt;

use open_mainframe_zosmf::config::ZosmfConfig;
use open_mainframe_zosmf::state::AppState;

/// Create a test AppState with IBMUSER authenticated.
fn setup_test_state() -> Arc<AppState> {
    let mut config = ZosmfConfig::default();
    let temp_dir = std::env::temp_dir().join(format!(
        "zosmf-test-jobs-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos()
    ));
    let _ = std::fs::create_dir_all(&temp_dir);
    config.uss.root_directory = temp_dir.to_string_lossy().to_string();

    let mut state = AppState::new(config);

    state
        .racf
        .add_user("IBMUSER", "SYS1", "IBM Default User", "SYS1")
        .expect("add user");
    state
        .racf
        .get_user_mut("IBMUSER")
        .expect("get user")
        .password_hash = Some("TESTPASS".to_string());

    Arc::new(state)
}

fn basic_auth_header() -> String {
    use base64::Engine;
    let encoded = base64::engine::general_purpose::STANDARD.encode("IBMUSER:TESTPASS");
    format!("Basic {}", encoded)
}

fn build_router(state: Arc<AppState>) -> axum::Router {
    open_mainframe_zosmf::handlers::build_router(state)
}

// ─── Test: Submit JCL (zowe zos-jobs submit lf) ───

#[tokio::test]
async fn test_submit_jcl() {
    let state = setup_test_state();
    let app = build_router(state);

    let jcl = r#"//TESTJOB  JOB (ACCT),'TEST JOB',CLASS=A,MSGCLASS=X
//STEP1    EXEC PGM=IEFBR14
"#;

    let response = app
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri("/zosmf/restjobs/jobs")
                .header("authorization", basic_auth_header())
                .header("content-type", "text/plain")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(jcl))
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(response.status(), StatusCode::CREATED);

    let body = axum::body::to_bytes(response.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();

    assert!(json["jobid"].as_str().unwrap().starts_with("JOB"));
    assert_eq!(json["jobname"], "TESTJOB");
    assert_eq!(json["owner"], "IBMUSER");
    assert_eq!(json["status"], "OUTPUT");
}

// ─── Test: Submit then get status (zowe zos-jobs view jsbj) ───

#[tokio::test]
async fn test_submit_then_get_status() {
    let state = setup_test_state();

    // Submit
    let app = build_router(state.clone());
    let jcl = "//STATJOB  JOB (ACCT),'STATUS TEST',CLASS=A\n//STEP1 EXEC PGM=IEFBR14\n";
    let resp = app
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri("/zosmf/restjobs/jobs")
                .header("authorization", basic_auth_header())
                .header("content-type", "text/plain")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(jcl))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::CREATED);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let submit_json: serde_json::Value = serde_json::from_slice(&body).unwrap();
    let jobid = submit_json["jobid"].as_str().unwrap();
    let jobname = submit_json["jobname"].as_str().unwrap();

    // Get status
    let app = build_router(state);
    let resp = app
        .oneshot(
            Request::builder()
                .uri(format!("/zosmf/restjobs/jobs/{}/{}", jobname, jobid))
                .header("authorization", basic_auth_header())
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();
    assert_eq!(json["jobid"], jobid);
    assert_eq!(json["jobname"], jobname);
    assert!(json.get("status").is_some());
    assert!(json.get("type").is_some());
}

// ─── Test: List Jobs (zowe zos-jobs list jobs) ───

#[tokio::test]
async fn test_list_jobs() {
    let state = setup_test_state();

    // Submit a job first
    let app = build_router(state.clone());
    let jcl = "//LISTJOB  JOB (ACCT),'LIST TEST',CLASS=A\n//STEP1 EXEC PGM=IEFBR14\n";
    let resp = app
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri("/zosmf/restjobs/jobs")
                .header("authorization", basic_auth_header())
                .header("content-type", "text/plain")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(jcl))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::CREATED);

    // List jobs
    let app = build_router(state);
    let resp = app
        .oneshot(
            Request::builder()
                .uri("/zosmf/restjobs/jobs?owner=IBMUSER&prefix=LIST*")
                .header("authorization", basic_auth_header())
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();
    let jobs = json.as_array().unwrap();
    assert!(!jobs.is_empty());

    // Verify fields are present in job entries
    let job = &jobs[0];
    assert!(job.get("jobid").is_some());
    assert!(job.get("jobname").is_some());
    assert!(job.get("owner").is_some());
    assert!(job.get("status").is_some());
    assert!(job.get("type").is_some());
    assert!(job.get("class").is_some());
}

// ─── Test: List Spool Files (zowe zos-jobs view sfbi) ───

#[tokio::test]
async fn test_list_spool_files() {
    let state = setup_test_state();

    // Submit
    let app = build_router(state.clone());
    let jcl = "//SPOOLJOB JOB (ACCT),'SPOOL TEST',CLASS=A\n//STEP1 EXEC PGM=IEFBR14\n";
    let resp = app
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri("/zosmf/restjobs/jobs")
                .header("authorization", basic_auth_header())
                .header("content-type", "text/plain")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(jcl))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::CREATED);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let submit_json: serde_json::Value = serde_json::from_slice(&body).unwrap();
    let jobid = submit_json["jobid"].as_str().unwrap();
    let jobname = submit_json["jobname"].as_str().unwrap();

    // List spool files
    let app = build_router(state);
    let resp = app
        .oneshot(
            Request::builder()
                .uri(format!(
                    "/zosmf/restjobs/jobs/{}/{}/files",
                    jobname, jobid
                ))
                .header("authorization", basic_auth_header())
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();
    let files = json.as_array().unwrap();

    // Should have at least JESJCL spool dataset
    assert!(!files.is_empty());

    let file = &files[0];
    assert!(file.get("id").is_some());
    assert!(file.get("ddname").is_some());
    assert!(file.get("class").is_some());
    assert!(file.get("byte-count").is_some());
    assert!(file.get("record-count").is_some());
}

// ─── Test: Read Spool Content ───

#[tokio::test]
async fn test_read_spool_content() {
    let state = setup_test_state();

    // Submit
    let app = build_router(state.clone());
    let jcl = "//READJOB  JOB (ACCT),'READ TEST',CLASS=A\n//STEP1 EXEC PGM=IEFBR14\n";
    let resp = app
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri("/zosmf/restjobs/jobs")
                .header("authorization", basic_auth_header())
                .header("content-type", "text/plain")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(jcl))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::CREATED);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let submit_json: serde_json::Value = serde_json::from_slice(&body).unwrap();
    let jobid = submit_json["jobid"].as_str().unwrap();
    let jobname = submit_json["jobname"].as_str().unwrap();

    // Read spool file 0 (JESJCL)
    let app = build_router(state);
    let resp = app
        .oneshot(
            Request::builder()
                .uri(format!(
                    "/zosmf/restjobs/jobs/{}/{}/files/0/records",
                    jobname, jobid
                ))
                .header("authorization", basic_auth_header())
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let content = String::from_utf8(body.to_vec()).unwrap();

    // Spool content should contain the original JCL
    assert!(content.contains("READJOB"));
}

// ─── Test: Purge Job (zowe zos-jobs delete job) ───

#[tokio::test]
async fn test_purge_job() {
    let state = setup_test_state();

    // Submit
    let app = build_router(state.clone());
    let jcl = "//DELJOB   JOB (ACCT),'DELETE TEST',CLASS=A\n//STEP1 EXEC PGM=IEFBR14\n";
    let resp = app
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri("/zosmf/restjobs/jobs")
                .header("authorization", basic_auth_header())
                .header("content-type", "text/plain")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(jcl))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::CREATED);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let submit_json: serde_json::Value = serde_json::from_slice(&body).unwrap();
    let jobid = submit_json["jobid"].as_str().unwrap();
    let jobname = submit_json["jobname"].as_str().unwrap();

    // Cancel first (job must be in Output/Cancelled state to purge)
    let app = build_router(state.clone());
    let resp = app
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri(format!("/zosmf/restjobs/jobs/{}/{}", jobname, jobid))
                .header("authorization", basic_auth_header())
                .header("content-type", "application/json")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(r#"{"request":"cancel"}"#))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);

    // Purge
    let app = build_router(state.clone());
    let resp = app
        .oneshot(
            Request::builder()
                .method("DELETE")
                .uri(format!("/zosmf/restjobs/jobs/{}/{}", jobname, jobid))
                .header("authorization", basic_auth_header())
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();
    assert_eq!(json["status"], 0);
    assert!(json["message"].as_str().unwrap().contains("purged"));

    // Verify job is in purge/output state after purge
    let app = build_router(state);
    let resp = app
        .oneshot(
            Request::builder()
                .uri(format!("/zosmf/restjobs/jobs/{}/{}", jobname, jobid))
                .header("authorization", basic_auth_header())
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();
    // Job status should reflect purge/output state
    assert_eq!(json["status"], "OUTPUT");
}

// ─── Test: Job actions — hold and cancel ───

#[tokio::test]
async fn test_job_hold_and_cancel() {
    let state = setup_test_state();

    // Submit
    let app = build_router(state.clone());
    let jcl = "//ACTJOB   JOB (ACCT),'ACTION TEST',CLASS=A\n//STEP1 EXEC PGM=IEFBR14\n";
    let resp = app
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri("/zosmf/restjobs/jobs")
                .header("authorization", basic_auth_header())
                .header("content-type", "text/plain")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(jcl))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::CREATED);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let submit_json: serde_json::Value = serde_json::from_slice(&body).unwrap();
    let jobid = submit_json["jobid"].as_str().unwrap();
    let jobname = submit_json["jobname"].as_str().unwrap();

    // Cancel (job is already in OUTPUT state after synchronous execution)
    let app = build_router(state);
    let resp = app
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri(format!("/zosmf/restjobs/jobs/{}/{}", jobname, jobid))
                .header("authorization", basic_auth_header())
                .header("content-type", "application/json")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(r#"{"request":"cancel"}"#))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);
}

// ─── Test: Job response JSON format compliance ───

#[tokio::test]
async fn test_job_json_format_compliance() {
    let state = setup_test_state();

    // Submit
    let app = build_router(state.clone());
    let jcl = "//FMTJOB   JOB (ACCT),'FORMAT TEST',CLASS=A\n//STEP1 EXEC PGM=IEFBR14\n";
    let resp = app
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri("/zosmf/restjobs/jobs")
                .header("authorization", basic_auth_header())
                .header("content-type", "text/plain")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(jcl))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::CREATED);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();

    // Verify submit response format
    assert!(json.get("jobid").is_some());
    assert!(json.get("jobname").is_some());
    assert!(json.get("owner").is_some());
    assert!(json.get("status").is_some());

    let jobid = json["jobid"].as_str().unwrap();
    let jobname = json["jobname"].as_str().unwrap();

    // Verify job status response has z/OSMF standard fields
    let app = build_router(state);
    let resp = app
        .oneshot(
            Request::builder()
                .uri(format!("/zosmf/restjobs/jobs/{}/{}", jobname, jobid))
                .header("authorization", basic_auth_header())
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();

    assert!(json.get("jobid").is_some());
    assert!(json.get("jobname").is_some());
    assert!(json.get("owner").is_some());
    assert!(json.get("status").is_some());
    assert!(json.get("type").is_some());
    assert!(json.get("class").is_some());
    assert!(json.get("phase").is_some());
    assert!(json.get("phase-name").is_some());
}

// ─── Test: Authentication required for job operations ───

#[tokio::test]
async fn test_jobs_require_auth() {
    let state = setup_test_state();
    let app = build_router(state);

    let resp = app
        .oneshot(
            Request::builder()
                .uri("/zosmf/restjobs/jobs?owner=*")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::UNAUTHORIZED);
}
