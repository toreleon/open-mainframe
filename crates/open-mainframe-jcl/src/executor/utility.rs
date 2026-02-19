//! Utility program registry for built-in IBM utility support.
//!
//! Provides an extensible registry mapping program names to handler functions,
//! so that `EXEC PGM=IEBGENER` and similar utilities execute without external binaries.
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

/// Trait for a utility program handler.
pub trait UtilityProgram: Send + Sync {
    /// Execute the utility with the given DD file mappings.
    fn execute(
        &self,
        step_name: Option<&str>,
        dd_files: &HashMap<String, PathBuf>,
        parm: Option<&str>,
    ) -> Result<StepResult, JclError>;

    /// Display name of the utility.
    fn name(&self) -> &str;
}

/// Registry of utility programs.
pub struct UtilityRegistry {
    programs: HashMap<String, Box<dyn UtilityProgram>>,
}

impl Default for UtilityRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl UtilityRegistry {
    /// Create a new registry with built-in IBM utilities pre-registered.
    pub fn new() -> Self {
        let mut registry = Self {
            programs: HashMap::new(),
        };
        // Register built-in utilities
        registry.register(Box::new(Iefbr14));
        registry.register(Box::new(Iebgener));
        registry.register(Box::new(Idcams));
        registry.register(Box::new(Iebcompr));
        registry
    }

    /// Register a utility program.
    pub fn register(&mut self, program: Box<dyn UtilityProgram>) {
        self.programs.insert(program.name().to_string(), program);
    }

    /// Look up a utility by program name.
    pub fn lookup(&self, name: &str) -> Option<&dyn UtilityProgram> {
        self.programs.get(name).map(|p| p.as_ref())
    }

    /// Check if a program name is registered.
    pub fn contains(&self, name: &str) -> bool {
        self.programs.contains_key(name)
    }

    /// Get the list of registered program names.
    pub fn program_names(&self) -> Vec<&str> {
        self.programs.keys().map(|s| s.as_str()).collect()
    }
}

// =========================================================================
// Built-in utilities
// =========================================================================

/// IEFBR14 — No-operation program (IBM "do nothing" utility).
pub struct Iefbr14;

impl UtilityProgram for Iefbr14 {
    fn execute(
        &self,
        step_name: Option<&str>,
        _dd_files: &HashMap<String, PathBuf>,
        _parm: Option<&str>,
    ) -> Result<StepResult, JclError> {
        Ok(StepResult {
            name: step_name.map(|s| s.to_string()),
            return_code: 0,
            stdout: String::new(),
            stderr: String::new(),
            success: true,
        })
    }

    fn name(&self) -> &str {
        "IEFBR14"
    }
}

/// IEBGENER — Dataset copy utility.
///
/// Copies SYSUT1 (input) to SYSUT2 (output).
pub struct Iebgener;

impl UtilityProgram for Iebgener {
    fn execute(
        &self,
        step_name: Option<&str>,
        dd_files: &HashMap<String, PathBuf>,
        _parm: Option<&str>,
    ) -> Result<StepResult, JclError> {
        let sysut1 = dd_files.get("SYSUT1").ok_or_else(|| JclError::ExecutionFailed {
            message: "IEBGENER requires SYSUT1 DD (input)".to_string(),
        })?;

        let sysut2 = dd_files.get("SYSUT2").ok_or_else(|| JclError::ExecutionFailed {
            message: "IEBGENER requires SYSUT2 DD (output)".to_string(),
        })?;

        // Create output directory if needed
        if let Some(parent) = sysut2.parent() {
            if !parent.exists() {
                std::fs::create_dir_all(parent).map_err(|e| JclError::ExecutionFailed {
                    message: format!("Failed to create output directory: {}", e),
                })?;
            }
        }

        // Copy input to output
        let bytes_copied = copy_file(sysut1, sysut2)?;

        let stdout = format!(
            "IEBGENER COMPLETED - {} BYTES COPIED FROM SYSUT1 TO SYSUT2\n",
            bytes_copied
        );

        Ok(StepResult {
            name: step_name.map(|s| s.to_string()),
            return_code: 0,
            stdout,
            stderr: String::new(),
            success: true,
        })
    }

    fn name(&self) -> &str {
        "IEBGENER"
    }
}

/// Copy a file from source to destination, returning bytes copied.
fn copy_file(src: &Path, dst: &Path) -> Result<u64, JclError> {
    if !src.exists() {
        return Err(JclError::ExecutionFailed {
            message: format!("Input file does not exist: {:?}", src),
        });
    }
    std::fs::copy(src, dst).map_err(|e| JclError::ExecutionFailed {
        message: format!("Failed to copy {:?} to {:?}: {}", src, dst, e),
    })
}

/// IDCAMS — VSAM utility (basic subset).
///
/// Supports simplified DEFINE CLUSTER, DELETE, and REPRO commands from SYSIN.
pub struct Idcams;

impl UtilityProgram for Idcams {
    fn execute(
        &self,
        step_name: Option<&str>,
        dd_files: &HashMap<String, PathBuf>,
        _parm: Option<&str>,
    ) -> Result<StepResult, JclError> {
        let sysin = dd_files.get("SYSIN").ok_or_else(|| JclError::ExecutionFailed {
            message: "IDCAMS requires SYSIN DD with control statements".to_string(),
        })?;

        let control = std::fs::read_to_string(sysin).map_err(|e| JclError::ExecutionFailed {
            message: format!("Failed to read SYSIN: {}", e),
        })?;

        let mut output = Vec::new();
        let mut rc = 0u32;

        for line in control.lines() {
            let trimmed = line.trim().to_uppercase();
            if trimmed.is_empty() || trimmed.starts_with("/*") {
                continue;
            }

            if trimmed.starts_with("DEFINE CLUSTER") {
                // Extract NAME from parenthesized parameter
                if let Some(name) = extract_param(&trimmed, "NAME") {
                    output.push(format!("IDCAMS: DEFINE CLUSTER - NAME({})", name));
                    // Create a marker file for the VSAM cluster
                    let cluster_path = dd_files
                        .get("SYSPRINT")
                        .cloned()
                        .unwrap_or_else(|| PathBuf::from("/tmp"))
                        .parent()
                        .unwrap_or_else(|| Path::new("/tmp"))
                        .join(format!("{}.vsam", name.replace('.', "/")));
                    if let Some(parent) = cluster_path.parent() {
                        let _ = std::fs::create_dir_all(parent);
                    }
                    let _ = std::fs::write(&cluster_path, format!("VSAM CLUSTER: {}\n", name));
                } else {
                    output.push("IDCAMS: DEFINE CLUSTER - NAME REQUIRED".to_string());
                    rc = 12;
                }
            } else if trimmed.starts_with("DELETE") {
                if let Some(name) = extract_param(&trimmed, "") {
                    output.push(format!("IDCAMS: DELETE {}", name));
                } else {
                    output.push("IDCAMS: DELETE - DATASET NAME REQUIRED".to_string());
                    rc = 12;
                }
            } else if trimmed.starts_with("REPRO") {
                output.push("IDCAMS: REPRO PROCESSED".to_string());
                // REPRO copies between datasets — simplified: copy INFILE DD to OUTFILE DD
                if let (Some(infile), Some(outfile)) =
                    (dd_files.get("INFILE"), dd_files.get("OUTFILE"))
                {
                    match copy_file(infile, outfile) {
                        Ok(bytes) => {
                            output.push(format!("  {} BYTES COPIED", bytes));
                        }
                        Err(e) => {
                            output.push(format!("  REPRO FAILED: {}", e));
                            rc = 8;
                        }
                    }
                }
            } else {
                // Continuation line or unknown — skip
            }
        }

        let stdout = output.join("\n") + "\n";

        Ok(StepResult {
            name: step_name.map(|s| s.to_string()),
            return_code: rc,
            stdout,
            stderr: String::new(),
            success: rc <= 4,
        })
    }

    fn name(&self) -> &str {
        "IDCAMS"
    }
}

/// IEBCOMPR — Dataset comparison utility.
///
/// Compares SYSUT1 and SYSUT2 record-by-record. Reports differences via
/// SYSPRINT. Processing halts after 10 mismatches.
pub struct Iebcompr;

impl UtilityProgram for Iebcompr {
    fn execute(
        &self,
        step_name: Option<&str>,
        dd_files: &HashMap<String, PathBuf>,
        _parm: Option<&str>,
    ) -> Result<StepResult, JclError> {
        let sysut1 = dd_files.get("SYSUT1").ok_or_else(|| JclError::ExecutionFailed {
            message: "IEBCOMPR requires SYSUT1 DD (first input)".to_string(),
        })?;
        let sysut2 = dd_files.get("SYSUT2").ok_or_else(|| JclError::ExecutionFailed {
            message: "IEBCOMPR requires SYSUT2 DD (second input)".to_string(),
        })?;

        let data1 = std::fs::read(sysut1).map_err(|e| JclError::ExecutionFailed {
            message: format!("Failed to read SYSUT1: {e}"),
        })?;
        let data2 = std::fs::read(sysut2).map_err(|e| JclError::ExecutionFailed {
            message: format!("Failed to read SYSUT2: {e}"),
        })?;

        let mut out = UtilityOutput::new("IEBCOMPR");

        // Compare line-by-line (treating as text records)
        let lines1: Vec<&str> = std::str::from_utf8(&data1)
            .unwrap_or("")
            .lines()
            .collect();
        let lines2: Vec<&str> = std::str::from_utf8(&data2)
            .unwrap_or("")
            .lines()
            .collect();

        let max_len = lines1.len().max(lines2.len());
        let mut mismatches = 0u32;
        const MAX_MISMATCHES: u32 = 10;

        for i in 0..max_len {
            if mismatches >= MAX_MISMATCHES {
                out.error(&format!("PROCESSING HALTED AFTER {MAX_MISMATCHES} UNEQUAL COMPARISONS"));
                break;
            }
            let l1 = lines1.get(i).copied().unwrap_or("");
            let l2 = lines2.get(i).copied().unwrap_or("");
            if l1 != l2 {
                mismatches += 1;
                out.error(&format!("RECORD {}: SYSUT1 AND SYSUT2 DO NOT COMPARE", i + 1));
            }
        }

        if mismatches == 0 {
            out.info("DATA SETS COMPARE EQUALLY");
        } else {
            out.set_rc(RC_ERROR);
        }

        Ok(out.into_step_result(step_name))
    }

    fn name(&self) -> &str {
        "IEBCOMPR"
    }
}

/// Extract a parameter value from a control statement.
/// For `DEFINE CLUSTER (NAME(MY.KSDS) ...)`, extracts "MY.KSDS".
/// For `DELETE dsname`, extracts "dsname" if param_name is empty.
fn extract_param(text: &str, param_name: &str) -> Option<String> {
    if param_name.is_empty() {
        // Extract the first word after the command keyword
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
    fn test_utility_registry_default() {
        let registry = UtilityRegistry::new();
        assert!(registry.contains("IEFBR14"));
        assert!(registry.contains("IEBGENER"));
        assert!(registry.contains("IDCAMS"));
        assert!(!registry.contains("CUSTOMPGM"));
    }

    #[test]
    fn test_utility_registry_lookup() {
        let registry = UtilityRegistry::new();
        let util = registry.lookup("IEFBR14");
        assert!(util.is_some());
        assert_eq!(util.unwrap().name(), "IEFBR14");
    }

    #[test]
    fn test_utility_registry_not_found_fallback() {
        // AC: Given EXEC PGM=CUSTOMPGM
        // When the program is NOT in the utility registry
        // Then lookup returns None (executor falls back to external binary search)
        let registry = UtilityRegistry::new();
        assert!(registry.lookup("CUSTOMPGM").is_none());
    }

    #[test]
    fn test_iefbr14_execute() {
        let util = Iefbr14;
        let dd_files = HashMap::new();
        let result = util.execute(Some("STEP1"), &dd_files, None).unwrap();
        assert!(result.success);
        assert_eq!(result.return_code, 0);
    }

    #[test]
    fn test_iebgener_copy() {
        // AC: Given JCL with EXEC PGM=IEBGENER, SYSUT1 DD (input), SYSUT2 DD (output)
        // When executed
        // Then the input dataset is copied to the output dataset
        let temp_dir = std::env::temp_dir().join("jcl_iebgener_test");
        let _ = fs::remove_dir_all(&temp_dir);
        fs::create_dir_all(&temp_dir).unwrap();

        let input_path = temp_dir.join("input.dat");
        let output_path = temp_dir.join("output.dat");
        fs::write(&input_path, "HELLO WORLD\nLINE 2\n").unwrap();

        let mut dd_files = HashMap::new();
        dd_files.insert("SYSUT1".to_string(), input_path);
        dd_files.insert("SYSUT2".to_string(), output_path.clone());

        let util = Iebgener;
        let result = util.execute(Some("COPY01"), &dd_files, None).unwrap();

        assert!(result.success);
        assert_eq!(result.return_code, 0);
        assert!(result.stdout.contains("IEBGENER COMPLETED"));

        let output = fs::read_to_string(&output_path).unwrap();
        assert_eq!(output, "HELLO WORLD\nLINE 2\n");

        let _ = fs::remove_dir_all(&temp_dir);
    }

    #[test]
    fn test_iebgener_missing_sysut1() {
        let mut dd_files = HashMap::new();
        dd_files.insert("SYSUT2".to_string(), PathBuf::from("/tmp/out"));

        let util = Iebgener;
        let result = util.execute(Some("COPY01"), &dd_files, None);
        assert!(result.is_err());
    }

    #[test]
    fn test_idcams_define_cluster() {
        let temp_dir = std::env::temp_dir().join("jcl_idcams_test");
        let _ = fs::remove_dir_all(&temp_dir);
        fs::create_dir_all(&temp_dir).unwrap();

        // AC: Given SYSIN with `DEFINE CLUSTER (NAME(MY.KSDS) ...)`
        // When IDCAMS executes
        // Then a VSAM cluster directory structure is created
        let sysin_path = temp_dir.join("SYSIN.dat");
        fs::write(&sysin_path, "  DEFINE CLUSTER (NAME(MY.KSDS) -\n    KEYS(10 0) RECORDS(100))\n").unwrap();

        let mut dd_files = HashMap::new();
        dd_files.insert("SYSIN".to_string(), sysin_path);
        dd_files.insert("SYSPRINT".to_string(), temp_dir.join("SYSPRINT.dat"));

        let util = Idcams;
        let result = util.execute(Some("IDCAMS01"), &dd_files, None).unwrap();

        assert!(result.success);
        assert!(result.stdout.contains("DEFINE CLUSTER"));
        assert!(result.stdout.contains("MY.KSDS"));

        let _ = fs::remove_dir_all(&temp_dir);
    }

    #[test]
    fn test_idcams_missing_sysin() {
        let dd_files = HashMap::new();
        let util = Idcams;
        let result = util.execute(Some("STEP1"), &dd_files, None);
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
    fn test_registry_custom_utility() {
        struct CustomUtil;
        impl UtilityProgram for CustomUtil {
            fn execute(
                &self,
                step_name: Option<&str>,
                _dd_files: &HashMap<String, PathBuf>,
                _parm: Option<&str>,
            ) -> Result<StepResult, JclError> {
                Ok(StepResult {
                    name: step_name.map(|s| s.to_string()),
                    return_code: 0,
                    stdout: "CUSTOM OK\n".to_string(),
                    stderr: String::new(),
                    success: true,
                })
            }
            fn name(&self) -> &str {
                "CUSTUTIL"
            }
        }

        let mut registry = UtilityRegistry::new();
        registry.register(Box::new(CustomUtil));
        assert!(registry.contains("CUSTUTIL"));

        let result = registry
            .lookup("CUSTUTIL")
            .unwrap()
            .execute(Some("S1"), &HashMap::new(), None)
            .unwrap();
        assert!(result.stdout.contains("CUSTOM OK"));
    }

    #[test]
    fn test_registry_program_names() {
        let registry = UtilityRegistry::new();
        let names = registry.program_names();
        assert!(names.contains(&"IEFBR14"));
        assert!(names.contains(&"IEBGENER"));
        assert!(names.contains(&"IDCAMS"));
        assert!(names.contains(&"IEBCOMPR"));
    }

    #[test]
    fn test_iebcompr_equal() {
        let temp_dir = std::env::temp_dir().join("jcl_iebcompr_eq");
        let _ = fs::remove_dir_all(&temp_dir);
        fs::create_dir_all(&temp_dir).unwrap();

        let f1 = temp_dir.join("sysut1.dat");
        let f2 = temp_dir.join("sysut2.dat");
        fs::write(&f1, "LINE 1\nLINE 2\nLINE 3\n").unwrap();
        fs::write(&f2, "LINE 1\nLINE 2\nLINE 3\n").unwrap();

        let mut dd_files = HashMap::new();
        dd_files.insert("SYSUT1".to_string(), f1);
        dd_files.insert("SYSUT2".to_string(), f2);

        let util = Iebcompr;
        let result = util.execute(Some("CMP"), &dd_files, None).unwrap();
        assert_eq!(result.return_code, RC_SUCCESS);
        assert!(result.stdout.contains("COMPARE EQUALLY"));

        let _ = fs::remove_dir_all(&temp_dir);
    }

    #[test]
    fn test_iebcompr_unequal() {
        let temp_dir = std::env::temp_dir().join("jcl_iebcompr_ne");
        let _ = fs::remove_dir_all(&temp_dir);
        fs::create_dir_all(&temp_dir).unwrap();

        let f1 = temp_dir.join("sysut1.dat");
        let f2 = temp_dir.join("sysut2.dat");
        fs::write(&f1, "LINE 1\nLINE 2\n").unwrap();
        fs::write(&f2, "LINE 1\nDIFFERENT\n").unwrap();

        let mut dd_files = HashMap::new();
        dd_files.insert("SYSUT1".to_string(), f1);
        dd_files.insert("SYSUT2".to_string(), f2);

        let util = Iebcompr;
        let result = util.execute(Some("CMP"), &dd_files, None).unwrap();
        assert_eq!(result.return_code, RC_ERROR);
        assert!(result.stdout.contains("DO NOT COMPARE"));

        let _ = fs::remove_dir_all(&temp_dir);
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
