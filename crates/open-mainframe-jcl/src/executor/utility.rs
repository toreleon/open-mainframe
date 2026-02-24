//! Utility helpers for the JCL executor.
//!
//! Provides SYSIN reading, output formatting, and file-based utility
//! implementations that need direct disk I/O (e.g. IDCAMS).
//!
//! Standard IBM utilities (IEFBR14, IEBCOMPR, IKJEFT01, etc.) are dispatched
//! via the shared `open-mainframe-utilities` crate through a bridge adapter.
//!
//! ## Standard Return Codes
//!
//! IBM utilities follow a common return code convention:
//! - **0** — Success
//! - **4** — Warning (e.g. member not found but processing continued)
//! - **8** — Error (e.g. comparison found differences)
//! - **12** — Severe error (e.g. unrecoverable I/O)
//! - **16** — Control statement error

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::error::JclError;
use super::StepResult;

// =========================================================================
// Standard return codes
// =========================================================================

/// Successful completion.
pub const RC_SUCCESS: u32 = 0;
/// Warning — processing completed with minor issues.
pub const RC_WARNING: u32 = 4;
/// Error — processing completed but with errors.
pub const RC_ERROR: u32 = 8;
/// Severe error — processing could not complete.
pub const RC_SEVERE: u32 = 12;
/// Control statement error.
pub const RC_CTRL_ERROR: u32 = 16;

// =========================================================================
// SYSIN helpers
// =========================================================================

/// Read SYSIN DD as a string. Returns `Err` if the DD is missing or unreadable.
pub fn read_sysin(dd_files: &HashMap<String, PathBuf>) -> Result<String, JclError> {
    let sysin = dd_files
        .get("SYSIN")
        .ok_or_else(|| JclError::ExecutionFailed {
            message: "SYSIN DD not allocated".to_string(),
        })?;
    std::fs::read_to_string(sysin).map_err(|e| JclError::ExecutionFailed {
        message: format!("Failed to read SYSIN: {e}"),
    })
}

/// Read SYSIN DD and return non-blank, non-comment lines (uppercase).
/// Lines starting with `/*` are treated as end-of-data.
/// Continuation lines ending with `-` are joined.
pub fn read_sysin_statements(dd_files: &HashMap<String, PathBuf>) -> Result<Vec<String>, JclError> {
    let content = read_sysin(dd_files)?;
    let mut stmts = Vec::new();
    let mut current = String::new();

    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("/*") {
            break;
        }
        if trimmed.is_empty() {
            continue;
        }
        if let Some(continued) = trimmed.strip_suffix('-') {
            current.push_str(continued);
            current.push(' ');
        } else {
            current.push_str(trimmed);
            stmts.push(current.to_ascii_uppercase());
            current.clear();
        }
    }
    if !current.is_empty() {
        stmts.push(current.to_ascii_uppercase());
    }
    Ok(stmts)
}

// =========================================================================
// Utility output builder
// =========================================================================

/// Builder for utility SYSPRINT output with consistent formatting.
pub struct UtilityOutput {
    program: String,
    lines: Vec<String>,
    rc: u32,
}

impl UtilityOutput {
    /// Create a new output builder for the named program.
    pub fn new(program: &str) -> Self {
        Self {
            program: program.to_string(),
            lines: Vec::new(),
            rc: RC_SUCCESS,
        }
    }

    /// Add an informational message.
    pub fn info(&mut self, msg: &str) {
        self.lines.push(format!("{}: {msg}", self.program));
    }

    /// Add a warning message and raise RC to at least 4.
    pub fn warn(&mut self, msg: &str) {
        self.lines.push(format!("{}: WARNING - {msg}", self.program));
        self.rc = self.rc.max(RC_WARNING);
    }

    /// Add an error message and raise RC to at least 8.
    pub fn error(&mut self, msg: &str) {
        self.lines.push(format!("{}: ERROR - {msg}", self.program));
        self.rc = self.rc.max(RC_ERROR);
    }

    /// Set the return code to a specific value (only raises, never lowers).
    pub fn set_rc(&mut self, rc: u32) {
        self.rc = self.rc.max(rc);
    }

    /// Build the final `StepResult`.
    pub fn into_step_result(self, step_name: Option<&str>) -> StepResult {
        let stdout = if self.lines.is_empty() {
            String::new()
        } else {
            self.lines.join("\n") + "\n"
        };
        StepResult {
            name: step_name.map(|s| s.to_string()),
            return_code: self.rc,
            stdout,
            stderr: String::new(),
            success: self.rc <= RC_WARNING,
        }
    }

    /// Current return code.
    pub fn rc(&self) -> u32 {
        self.rc
    }
}

// =========================================================================
// IDCAMS — delegates to open-mainframe-dataset's Idcams implementation
// =========================================================================

/// Execute IDCAMS with file-based DD mappings.
///
/// Reads SYSIN, resolves INFILE/OUTFILE DD names to dataset paths,
/// and delegates to `open_mainframe_dataset::idcams::Idcams` for execution.
///
/// The `dataset_dir` parameter should be the executor's dataset directory
/// so that DEFINE CLUSTER/GDG/etc. create files in the correct location.
pub fn execute_idcams(
    step_name: Option<&str>,
    dd_files: &HashMap<String, PathBuf>,
    _parm: Option<&str>,
    dataset_dir: &Path,
) -> Result<StepResult, JclError> {
    let statements = read_sysin_statements(dd_files)?;

    // Resolve INFILE(ddname)/OUTFILE(ddname) to INDATASET(path) so the
    // dataset crate's IDCAMS can handle REPRO correctly.
    let resolved: Vec<String> = statements
        .iter()
        .map(|stmt| resolve_dd_references(stmt, dd_files))
        .collect();

    let control = resolved.join("\n");

    // Use the dataset directory as the base for DEFINE operations
    let base_dir = dataset_dir;

    let mut idcams = open_mainframe_dataset::idcams::Idcams::new(base_dir);
    let result = match idcams.execute(&control) {
        Ok(r) => r,
        Err(e) => {
            // If parsing fails entirely, return RC=16 (control statement error)
            return Ok(StepResult {
                name: step_name.map(|s| s.to_string()),
                return_code: 16,
                stdout: format!("IDCAMS: {e}\n"),
                stderr: String::new(),
                success: false,
            });
        }
    };

    // Write SYSPRINT output if DD allocated
    if let Some(sysprint_path) = dd_files.get("SYSPRINT") {
        let _ = std::fs::write(sysprint_path, &result.output);
    }

    Ok(StepResult {
        name: step_name.map(|s| s.to_string()),
        return_code: result.return_code,
        stdout: result.output,
        stderr: String::new(),
        success: result.return_code <= 4,
    })
}

/// Replace INFILE(ddname)/OUTFILE(ddname) in a statement with
/// INDATASET(path)/OUTDATASET(path) by resolving DD names to file paths.
fn resolve_dd_references(stmt: &str, dd_files: &HashMap<String, PathBuf>) -> String {
    let mut result = stmt.to_string();

    // Process longer keywords first so INFILE matches before IFILE
    for (keyword_in, keyword_out) in &[
        ("INFILE", "INDATASET"),
        ("OUTFILE", "OUTDATASET"),
        ("IFILE", "INDATASET"),
        ("OFILE", "OUTDATASET"),
    ] {
        let search = format!("{keyword_in}(");
        if let Some(start) = result.find(&search) {
            let after = &result[start + search.len()..];
            if let Some(end) = after.find(')') {
                let dd_name = after[..end].trim();
                if let Some(path) = dd_files.get(dd_name) {
                    let replacement = format!("{keyword_out}({})", path.display());
                    let full_match = format!("{keyword_in}({})", dd_name);
                    result = result.replace(&full_match, &replacement);
                }
            }
        }
    }

    result
}

/// Extract a parameter value from a control statement.
/// For `DEFINE CLUSTER (NAME(MY.KSDS) ...)`, extracts "MY.KSDS".
/// For `DELETE dsname`, extracts "dsname" if param_name is empty.
pub fn extract_param(text: &str, param_name: &str) -> Option<String> {
    if param_name.is_empty() {
        let parts: Vec<&str> = text.split_whitespace().collect();
        return parts.get(1).map(|s| s.trim_matches(|c: char| !c.is_alphanumeric() && c != '.').to_string());
    }
    let search = format!("{}(", param_name);
    if let Some(start) = text.find(&search) {
        let after = &text[start + search.len()..];
        if let Some(end) = after.find(')') {
            return Some(after[..end].trim().to_string());
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_idcams_define_cluster() {
        let temp_dir = std::env::temp_dir().join("jcl_idcams_test");
        let _ = fs::remove_dir_all(&temp_dir);
        fs::create_dir_all(&temp_dir).unwrap();

        let dataset_dir = temp_dir.join("datasets");
        fs::create_dir_all(&dataset_dir).unwrap();

        let sysin_path = temp_dir.join("SYSIN.dat");
        fs::write(&sysin_path, "  DEFINE CLUSTER (NAME(MY.KSDS) -\n    KEYS(10 0) RECORDS(100))\n").unwrap();

        let mut dd_files = HashMap::new();
        dd_files.insert("SYSIN".to_string(), sysin_path);
        dd_files.insert("SYSPRINT".to_string(), temp_dir.join("SYSPRINT.dat"));

        let result = execute_idcams(Some("IDCAMS01"), &dd_files, None, &dataset_dir).unwrap();

        assert!(result.success);
        assert!(result.stdout.contains("DEFINE CLUSTER"));
        assert!(result.stdout.contains("MY.KSDS"));

        let _ = fs::remove_dir_all(&temp_dir);
    }

    #[test]
    fn test_idcams_missing_sysin() {
        let dd_files = HashMap::new();
        let result = execute_idcams(Some("STEP1"), &dd_files, None, Path::new("/tmp"));
        assert!(result.is_err());
    }

    #[test]
    fn test_extract_param_name() {
        assert_eq!(
            extract_param("DEFINE CLUSTER (NAME(MY.KSDS) KEYS(10 0))", "NAME"),
            Some("MY.KSDS".to_string())
        );
    }

    #[test]
    fn test_extract_param_delete() {
        assert_eq!(
            extract_param("DELETE MY.DATASET", ""),
            Some("MY.DATASET".to_string())
        );
    }

    #[test]
    fn test_utility_output_builder() {
        let mut out = UtilityOutput::new("TESTPGM");
        out.info("STARTED");
        out.warn("MEMBER NOT FOUND");
        assert_eq!(out.rc(), RC_WARNING);

        let result = out.into_step_result(Some("S1"));
        assert_eq!(result.return_code, RC_WARNING);
        assert!(result.success); // RC_WARNING is still success
        assert!(result.stdout.contains("TESTPGM: STARTED"));
        assert!(result.stdout.contains("TESTPGM: WARNING"));
    }

    #[test]
    fn test_utility_output_error_raises_rc() {
        let mut out = UtilityOutput::new("TESTPGM");
        out.info("OK");
        out.error("SOMETHING FAILED");
        assert_eq!(out.rc(), RC_ERROR);

        let result = out.into_step_result(None);
        assert!(!result.success);
    }

    #[test]
    fn test_read_sysin_statements() {
        let temp_dir = std::env::temp_dir().join("jcl_sysin_test");
        let _ = fs::remove_dir_all(&temp_dir);
        fs::create_dir_all(&temp_dir).unwrap();

        let sysin_path = temp_dir.join("SYSIN");
        fs::write(
            &sysin_path,
            "  COPY OUTDD=OUT,-\n    INDD=IN\n  SELECT MEMBER=A\n/*\n  IGNORED\n",
        )
        .unwrap();

        let mut dd_files = HashMap::new();
        dd_files.insert("SYSIN".to_string(), sysin_path);

        let stmts = read_sysin_statements(&dd_files).unwrap();
        assert_eq!(stmts.len(), 2);
        assert!(stmts[0].contains("COPY OUTDD=OUT,"));
        assert!(stmts[0].contains("INDD=IN"));
        assert!(stmts[1].contains("SELECT MEMBER=A"));

        let _ = fs::remove_dir_all(&temp_dir);
    }

    #[test]
    fn test_return_code_constants() {
        assert_eq!(RC_SUCCESS, 0);
        assert_eq!(RC_WARNING, 4);
        assert_eq!(RC_ERROR, 8);
        assert_eq!(RC_SEVERE, 12);
        assert_eq!(RC_CTRL_ERROR, 16);
    }
}
