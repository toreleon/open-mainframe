//! Integration tests for the OpenMainframe CLI.
//!
//! These tests verify that the CLI commands work correctly end-to-end.

use std::process::Command;

/// Get the path to the built binary.
fn get_bin_path() -> std::path::PathBuf {
    let mut path = std::env::current_exe().unwrap();
    path.pop(); // Remove test binary name
    path.pop(); // Remove deps
    path.push("open-mainframe");
    path
}

/// Helper to get fixture path.
fn fixture(name: &str) -> std::path::PathBuf {
    let mut path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push("fixtures");
    path.push(name);
    path
}

/// Run the CLI with given arguments and return (stdout, stderr, success).
fn run_cli(args: &[&str]) -> (String, String, bool) {
    let output = Command::new(get_bin_path())
        .args(args)
        .output()
        .expect("Failed to execute command");

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    (stdout, stderr, output.status.success())
}

#[test]
fn test_help_command() {
    let (stdout, _, success) = run_cli(&["--help"]);
    assert!(success);
    assert!(stdout.contains("OpenMainframe mainframe emulator"));
    assert!(stdout.contains("compile"));
    assert!(stdout.contains("run"));
    assert!(stdout.contains("interpret"));
}

#[test]
fn test_version_command() {
    let (stdout, _, success) = run_cli(&["--version"]);
    assert!(success);
    assert!(stdout.contains("open-mainframe"));
}

#[test]
fn test_interpret_hello() {
    let (stdout, stderr, success) = run_cli(&["interpret", fixture("hello.cbl").to_str().unwrap()]);
    if !success {
        eprintln!("STDERR: {}", stderr);
    }
    assert!(success, "Command failed with stderr: {}", stderr);
    assert!(stdout.contains("Hello, World!"), "Output: {}", stdout);
}

#[test]
fn test_interpret_compute() {
    let (stdout, stderr, success) =
        run_cli(&["interpret", fixture("compute.cbl").to_str().unwrap()]);
    if !success {
        eprintln!("STDERR: {}", stderr);
    }
    assert!(success, "Command failed with stderr: {}", stderr);
    assert!(stdout.contains("SUM:"), "Output: {}", stdout);
    assert!(stdout.contains("125"), "Expected 125 in output: {}", stdout);
    assert!(stdout.contains("DIFF:"), "Output: {}", stdout);
    assert!(stdout.contains("75"), "Expected 75 in output: {}", stdout);
    assert!(stdout.contains("DOUBLE:"), "Output: {}", stdout);
    assert!(stdout.contains("200"), "Expected 200 in output: {}", stdout);
}

#[test]
fn test_interpret_conditions() {
    let (stdout, stderr, success) =
        run_cli(&["interpret", fixture("conditions.cbl").to_str().unwrap()]);
    if !success {
        eprintln!("STDERR: {}", stderr);
    }
    assert!(success, "Command failed with stderr: {}", stderr);
    assert!(
        stdout.contains("NUM IS LESS THAN LIMIT"),
        "Output: {}",
        stdout
    );
    assert!(stdout.contains("NUM EQUALS 50"), "Output: {}", stdout);
    assert!(
        stdout.contains("NUM IS GREATER THAN 25"),
        "Output: {}",
        stdout
    );
}

#[test]
fn test_interpret_perform() {
    let (stdout, stderr, success) =
        run_cli(&["interpret", fixture("perform.cbl").to_str().unwrap()]);
    if !success {
        eprintln!("STDERR: {}", stderr);
    }
    assert!(success, "Command failed with stderr: {}", stderr);
    assert!(stdout.contains("COUNTING TO 5"), "Output: {}", stdout);
    assert!(stdout.contains("COUNT: 1"), "Output: {}", stdout);
    assert!(stdout.contains("COUNT: 5"), "Output: {}", stdout);
    assert!(stdout.contains("DONE COUNTING"), "Output: {}", stdout);
}

#[test]
fn test_check_valid_cobol() {
    let (_, stderr, success) = run_cli(&["check", fixture("hello.cbl").to_str().unwrap()]);
    assert!(success, "Check command failed: {}", stderr);
}

#[test]
fn test_lex_command() {
    let (stdout, stderr, success) = run_cli(&["lex", fixture("hello.cbl").to_str().unwrap()]);
    assert!(success, "Lex command failed: {}", stderr);
    assert!(stdout.contains("Identification"), "Output: {}", stdout);
    assert!(stdout.contains("Division"), "Output: {}", stdout);
    assert!(stdout.contains("ProgramId"), "Output: {}", stdout);
}

#[test]
fn test_config_show() {
    let (stdout, _, success) = run_cli(&["config", "show"]);
    assert!(success);
    assert!(stdout.contains("[compiler]"));
    assert!(stdout.contains("[runtime]"));
    assert!(stdout.contains("[dataset]"));
    assert!(stdout.contains("[jcl]"));
}

#[test]
fn test_config_paths() {
    let (stdout, _, success) = run_cli(&["config", "paths"]);
    assert!(success);
    assert!(stdout.contains("Configuration file search paths"));
    assert!(stdout.contains("Environment variables"));
    assert!(stdout.contains("OPEN_MAINFRAME_SOURCE_FORMAT"));
}

#[test]
fn test_missing_file_error() {
    let (_, stderr, success) = run_cli(&["interpret", "nonexistent.cbl"]);
    assert!(!success);
    assert!(
        stderr.contains("No such file") || stderr.contains("not found") || stderr.contains("error"),
        "Expected error message, got: {}",
        stderr
    );
}

#[test]
fn test_interpret_cics_xctl() {
    let (stdout, stderr, success) =
        run_cli(&["interpret", fixture("cics-main.cbl").to_str().unwrap()]);
    if !success {
        eprintln!("STDERR: {}", stderr);
    }
    assert!(success, "Command failed with stderr: {}", stderr);
    // Main program starts
    assert!(
        stdout.contains("MAIN PROGRAM STARTING"),
        "Output: {}",
        stdout
    );
    // XCTL is issued
    assert!(
        stdout.contains("[CICS] XCTL PROGRAM(CICSSUB)"),
        "Expected XCTL message, got: {}",
        stdout
    );
    // Sub program is reached via XCTL
    assert!(
        stdout.contains("SUB PROGRAM REACHED VIA XCTL"),
        "XCTL did not dispatch to sub program! Output: {}",
        stdout
    );
    // Sub program returns
    assert!(
        stdout.contains("[CICS] RETURN TRANSID(SUB1)"),
        "Expected RETURN from sub program, got: {}",
        stdout
    );
    // Main should NOT continue after XCTL
    assert!(
        !stdout.contains("SHOULD NOT REACH HERE"),
        "XCTL did not stop main program! Output: {}",
        stdout
    );
}

#[test]
fn test_interpret_cics_return() {
    let (stdout, stderr, success) =
        run_cli(&["interpret", fixture("cics-return.cbl").to_str().unwrap()]);
    if !success {
        eprintln!("STDERR: {}", stderr);
    }
    assert!(success, "Command failed with stderr: {}", stderr);
    // Program should display the starting message
    assert!(
        stdout.contains("CICS TEST STARTING"),
        "Output: {}",
        stdout
    );
    // CICS RETURN should log the return
    assert!(
        stdout.contains("[CICS] RETURN TRANSID(MENU)"),
        "Expected CICS RETURN message, got: {}",
        stdout
    );
    // RETURN should stop execution, so "SHOULD NOT REACH HERE" must NOT appear
    assert!(
        !stdout.contains("SHOULD NOT REACH HERE"),
        "RETURN did not stop execution! Output: {}",
        stdout
    );
}

#[test]
fn test_interpret_cics_send_map() {
    let (stdout, stderr, success) =
        run_cli(&["interpret", fixture("cics-signon.cbl").to_str().unwrap()]);
    if !success {
        eprintln!("STDERR: {}", stderr);
    }
    assert!(success, "Command failed with stderr: {}", stderr);
    // Program should start
    assert!(
        stdout.contains("SIGNON PROGRAM STARTING"),
        "Output: {}",
        stdout
    );
    // Program should detect initial start
    assert!(
        stdout.contains("INITIAL START - SENDING MAP"),
        "Output: {}",
        stdout
    );
    // SEND MAP should be issued
    assert!(
        stdout.contains("[CICS] SEND MAP"),
        "Expected SEND MAP message, got: {}",
        stdout
    );
    // RETURN TRANSID should be issued
    assert!(
        stdout.contains("[CICS] RETURN TRANSID(SIGN)"),
        "Expected RETURN TRANSID, got: {}",
        stdout
    );
}

#[test]
fn test_interpret_cics_xctl_chain() {
    // Multi-program XCTL: login → menu via XCTL with COMMAREA
    let (stdout, stderr, success) =
        run_cli(&["interpret", fixture("CICSLOGIN.cbl").to_str().unwrap()]);
    if !success {
        eprintln!("STDERR: {}", stderr);
    }
    assert!(success, "Command failed with stderr: {}", stderr);
    // Login program starts
    assert!(
        stdout.contains("LOGIN SCREEN"),
        "Output: {}",
        stdout
    );
    // XCTL issued to menu
    assert!(
        stdout.contains("[CICS] XCTL PROGRAM(CICSMENU)"),
        "Expected XCTL to CICSMENU, got: {}",
        stdout
    );
    // Menu program reached
    assert!(
        stdout.contains("MENU PROGRAM STARTING"),
        "Menu program should start via XCTL! Output: {}",
        stdout
    );
    // Menu sees COMMAREA
    assert!(
        stdout.contains("RECEIVED COMMAREA"),
        "Menu should receive COMMAREA! Output: {}",
        stdout
    );
    // SEND MAP issued by menu
    assert!(
        stdout.contains("[CICS] SEND MAP"),
        "Menu should SEND MAP, got: {}",
        stdout
    );
    // RETURN TRANSID from menu
    assert!(
        stdout.contains("[CICS] RETURN TRANSID(MENU)"),
        "Menu should RETURN TRANSID(MENU), got: {}",
        stdout
    );
    // Login should NOT continue after XCTL
    assert!(
        !stdout.contains("SHOULD NOT REACH HERE"),
        "XCTL should stop login! Output: {}",
        stdout
    );
}

#[test]
fn test_interpret_cics_abend_handler() {
    // Tests HANDLE ABEND → ABEND → handler paragraph invocation
    let (stdout, stderr, success) =
        run_cli(&["interpret", fixture("cics-abend.cbl").to_str().unwrap()]);
    if !success {
        eprintln!("STDERR: {}", stderr);
    }
    assert!(success, "Command failed with stderr: {}", stderr);
    // Program starts
    assert!(
        stdout.contains("ABEND TEST STARTING"),
        "Output: {}",
        stdout
    );
    // Handler registered
    assert!(
        stdout.contains("HANDLER REGISTERED"),
        "Output: {}",
        stdout
    );
    // Handler paragraph was invoked after ABEND
    assert!(
        stdout.contains("ABEND HANDLER INVOKED"),
        "Handler not invoked! Output: {}",
        stdout
    );
    assert!(
        stdout.contains("STATUS: HANDLED"),
        "Handler did not complete! Output: {}",
        stdout
    );
    // Execution should NOT continue after ABEND
    assert!(
        !stdout.contains("SHOULD NOT REACH HERE"),
        "ABEND did not stop main flow! Output: {}",
        stdout
    );
    // Handler issues RETURN TRANSID(MENU)
    assert!(
        stdout.contains("[CICS] RETURN TRANSID(MENU)"),
        "Handler should RETURN, got: {}",
        stdout
    );
}

#[test]
fn test_completions() {
    let (stdout, _, success) = run_cli(&["completions", "bash"]);
    assert!(success);
    assert!(stdout.contains("open-mainframe"));
}

// ─── JSON Output Tests (E-AV-002) ───────────────────────────────────────

#[test]
fn test_check_json_output() {
    let (stdout, _, success) = run_cli(&[
        "--format", "json",
        "check",
        fixture("hello.cbl").to_str().unwrap(),
    ]);
    assert!(success, "Check --format json failed");
    let json: serde_json::Value = serde_json::from_str(&stdout)
        .expect(&format!("Invalid JSON output: {}", stdout));
    assert_eq!(json["status"], "success");
    assert!(json["program_id"].is_string());
    assert!(json["summary"]["errors"].is_number());
    assert_eq!(json["summary"]["errors"], 0);
}

#[test]
fn test_compile_json_output() {
    let (stdout, _, success) = run_cli(&[
        "--format", "json",
        "compile",
        fixture("hello.cbl").to_str().unwrap(),
    ]);
    assert!(success, "Compile --format json failed");
    let json: serde_json::Value = serde_json::from_str(&stdout)
        .expect(&format!("Invalid JSON output: {}", stdout));
    assert_eq!(json["status"], "success");
    assert!(json["program_id"].is_string());
    assert!(json["diagnostics"].is_array());
    assert!(json["summary"].is_object());
}

#[test]
fn test_interpret_json_output() {
    let (stdout, _, success) = run_cli(&[
        "--format", "json",
        "interpret",
        fixture("hello.cbl").to_str().unwrap(),
    ]);
    assert!(success, "Interpret --format json failed");
    let json: serde_json::Value = serde_json::from_str(&stdout)
        .expect(&format!("Invalid JSON output: {}", stdout));
    assert_eq!(json["status"], "success");
    assert!(json["output"].is_array());
    let output_lines = json["output"].as_array().unwrap();
    let has_hello = output_lines.iter().any(|l| l.as_str().unwrap_or("").contains("Hello, World!"));
    assert!(has_hello, "Expected 'Hello, World!' in output: {:?}", output_lines);
    assert_eq!(json["return_code"], 0);
}

#[test]
fn test_interpret_json_compute() {
    let (stdout, _, success) = run_cli(&[
        "--format", "json",
        "interpret",
        fixture("compute.cbl").to_str().unwrap(),
    ]);
    assert!(success, "Interpret --format json failed for compute");
    let json: serde_json::Value = serde_json::from_str(&stdout)
        .expect(&format!("Invalid JSON output: {}", stdout));
    assert_eq!(json["status"], "success");
    let output_lines = json["output"].as_array().unwrap();
    let has_sum = output_lines.iter().any(|l| l.as_str().unwrap_or("").contains("SUM:"));
    assert!(has_sum, "Expected 'SUM:' in output: {:?}", output_lines);
}

#[test]
fn test_text_format_unchanged() {
    // Verify that --format text produces output containing the same COBOL output
    // (timestamps from tracing will differ between runs, so compare content not exact strings)
    let (stdout_default, _, success1) = run_cli(&[
        "interpret",
        fixture("hello.cbl").to_str().unwrap(),
    ]);
    let (stdout_text, _, success2) = run_cli(&[
        "--format", "text",
        "interpret",
        fixture("hello.cbl").to_str().unwrap(),
    ]);
    assert!(success1);
    assert!(success2);
    assert!(stdout_default.contains("Hello, World!"), "Default output should contain program output");
    assert!(stdout_text.contains("Hello, World!"), "Text format should contain program output");
    // JSON format should NOT appear in text mode
    assert!(!stdout_text.contains(r#""status""#), "Text format should not contain JSON");
}

#[test]
fn test_help_shows_format_flag() {
    let (stdout, _, success) = run_cli(&["--help"]);
    assert!(success);
    assert!(stdout.contains("--format"), "Help should show --format flag");
}
