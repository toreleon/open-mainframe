//! JCL job executor.
//!
//! Executes JCL jobs by running compiled COBOL programs.

use std::collections::HashMap;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use crate::ast::*;
use crate::error::JclError;

/// Result of executing a job step.
#[derive(Debug, Clone)]
pub struct StepResult {
    /// Step name.
    pub name: Option<String>,
    /// Return code (0-4095).
    pub return_code: u32,
    /// Standard output.
    pub stdout: String,
    /// Standard error.
    pub stderr: String,
    /// Whether the step completed successfully.
    pub success: bool,
}

/// Result of executing a job.
#[derive(Debug)]
pub struct JobResult {
    /// Job name.
    pub name: String,
    /// Results for each step.
    pub steps: Vec<StepResult>,
    /// Overall job return code (max of step return codes).
    pub return_code: u32,
    /// Whether all steps completed successfully.
    pub success: bool,
}

/// Execution environment configuration.
#[derive(Debug, Clone)]
pub struct ExecutionConfig {
    /// Directory containing compiled programs.
    pub program_dir: PathBuf,
    /// Directory for dataset files.
    pub dataset_dir: PathBuf,
    /// Working directory for execution.
    pub work_dir: PathBuf,
    /// Sysout files directory.
    pub sysout_dir: PathBuf,
}

impl Default for ExecutionConfig {
    fn default() -> Self {
        let current = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
        Self {
            program_dir: current.join("bin"),
            dataset_dir: current.join("datasets"),
            work_dir: current.join("work"),
            sysout_dir: current.join("sysout"),
        }
    }
}

/// JCL job executor.
pub struct JobExecutor {
    /// Execution configuration.
    config: ExecutionConfig,
    /// DD name to file path mappings for current step.
    dd_files: HashMap<String, PathBuf>,
    /// Passed datasets between steps.
    passed_datasets: HashMap<String, PathBuf>,
}

impl JobExecutor {
    /// Create a new job executor with default configuration.
    pub fn new() -> Self {
        Self::with_config(ExecutionConfig::default())
    }

    /// Create a new job executor with the given configuration.
    pub fn with_config(config: ExecutionConfig) -> Self {
        Self {
            config,
            dd_files: HashMap::new(),
            passed_datasets: HashMap::new(),
        }
    }

    /// Execute a JCL job.
    pub fn execute(&mut self, job: &Job) -> Result<JobResult, JclError> {
        // Create working directories
        self.create_directories()?;

        let mut step_results = Vec::new();
        let mut max_rc = 0u32;
        let mut all_success = true;

        for (step_idx, step) in job.steps.iter().enumerate() {
            // Check conditions (simplified)
            if !self.should_execute_step(step, &step_results) {
                continue;
            }

            let result = self.execute_step(step, step_idx)?;
            max_rc = max_rc.max(result.return_code);
            all_success = all_success && result.success;
            step_results.push(result);
        }

        Ok(JobResult {
            name: job.name.clone(),
            steps: step_results,
            return_code: max_rc,
            success: all_success,
        })
    }

    /// Create necessary directories.
    fn create_directories(&self) -> Result<(), JclError> {
        for dir in [&self.config.work_dir, &self.config.sysout_dir] {
            if !dir.exists() {
                std::fs::create_dir_all(dir).map_err(|e| JclError::ExecutionFailed {
                    message: format!("Failed to create directory {:?}: {}", dir, e),
                })?;
            }
        }
        Ok(())
    }

    /// Check if a step should be executed based on COND parameter.
    fn should_execute_step(&self, step: &Step, previous: &[StepResult]) -> bool {
        if step.params.cond.is_none() {
            return true;
        }

        let conditions = step.params.cond.as_ref().unwrap();

        // IBM convention: step is BYPASSED (not executed) if ANY condition is TRUE
        for cond in conditions {
            let compare_rc = if let Some(ref step_name) = cond.step {
                previous
                    .iter()
                    .find(|r| r.name.as_deref() == Some(step_name))
                    .map(|r| r.return_code)
            } else {
                previous.last().map(|r| r.return_code)
            };

            if let Some(rc) = compare_rc {
                let condition_met = match cond.operator {
                    ConditionOperator::Gt => cond.code > rc,
                    ConditionOperator::Ge => cond.code >= rc,
                    ConditionOperator::Eq => cond.code == rc,
                    ConditionOperator::Ne => cond.code != rc,
                    ConditionOperator::Lt => cond.code < rc,
                    ConditionOperator::Le => cond.code <= rc,
                };
                if condition_met {
                    return false; // Step is bypassed
                }
            }
        }

        true
    }

    /// Execute a single step.
    fn execute_step(&mut self, step: &Step, step_idx: usize) -> Result<StepResult, JclError> {
        // Clear DD files from previous step
        self.dd_files.clear();

        // Set up DD files for this step
        self.setup_dd_files(step, step_idx)?;

        // Execute based on type
        match &step.exec {
            ExecType::Program(pgm) => self.execute_program(step, pgm),
            ExecType::Procedure(proc) => {
                // Procedures not yet implemented
                Err(JclError::ExecutionFailed {
                    message: format!("Procedure {} execution not implemented", proc),
                })
            }
        }
    }

    /// Set up DD file mappings.
    fn setup_dd_files(&mut self, step: &Step, step_idx: usize) -> Result<(), JclError> {
        for dd in &step.dd_statements {
            let path = match &dd.definition {
                DdDefinition::Dataset(def) => self.resolve_dataset(&def.dsn, &def.disp)?,
                DdDefinition::Inline(def) => {
                    // Write inline data to temp file
                    let path = self
                        .config
                        .work_dir
                        .join(format!("{}_{}.inline", dd.name, step_idx));
                    self.write_inline_data(&path, &def.data)?;
                    path
                }
                DdDefinition::Sysout(def) => {
                    // Create sysout file
                    self.config
                        .sysout_dir
                        .join(format!("{}_{}.{}.txt", dd.name, step_idx, def.class))
                }
                DdDefinition::Dummy => {
                    // Dummy - use /dev/null equivalent
                    PathBuf::from("/dev/null")
                }
                DdDefinition::Concatenation(datasets) => {
                    // For now, just use first dataset
                    if let Some(first) = datasets.first() {
                        self.resolve_dataset(&first.dsn, &first.disp)?
                    } else {
                        PathBuf::from("/dev/null")
                    }
                }
            };

            self.dd_files.insert(dd.name.clone(), path);
        }

        Ok(())
    }

    /// Resolve a dataset name to a file path.
    fn resolve_dataset(
        &mut self,
        dsn: &str,
        disp: &Option<Disposition>,
    ) -> Result<PathBuf, JclError> {
        self.resolve_dataset_with_amp(dsn, disp, &None)
    }

    /// Resolve a dataset name to a file path, with AMP parameters for VSAM.
    fn resolve_dataset_with_amp(
        &mut self,
        dsn: &str,
        disp: &Option<Disposition>,
        _amp: &Option<AmpParams>,
    ) -> Result<PathBuf, JclError> {
        // Check if passed from previous step
        if let Some(path) = self.passed_datasets.get(dsn) {
            return Ok(path.clone());
        }

        // Convert DSN to file path
        // MY.DATA.SET -> datasets/MY/DATA/SET

        // Handle member names like MY.PDS(MEMBER)
        let (dsn_path, member) = if let Some(pos) = dsn.find('(') {
            let member_end = dsn.find(')').unwrap_or(dsn.len());
            let member = &dsn[pos + 1..member_end];
            let base_dsn = &dsn[..pos];
            (base_dsn, Some(member))
        } else {
            (dsn, None)
        };

        let mut path = self.config.dataset_dir.clone();
        for part in dsn_path.split('.') {
            path.push(part);
        }

        if let Some(member_name) = member {
            // PDS member - add member file
            path.push(format!("{}.cbl", member_name));
        } else {
            // Check if this is a VSAM cluster by looking for .vsam file
            let vsam_path = path.with_extension("vsam");
            if vsam_path.exists() {
                return Ok(vsam_path);
            }
        }

        // Handle disposition
        if let Some(disp) = disp {
            match disp.status {
                DispStatus::New => {
                    // Create parent directories
                    if let Some(parent) = path.parent() {
                        std::fs::create_dir_all(parent).map_err(|e| JclError::ExecutionFailed {
                            message: format!("Failed to create directory for {}: {}", dsn, e),
                        })?;
                    }
                }
                DispStatus::Old | DispStatus::Shr => {
                    // Dataset should exist
                    // We'll check at actual I/O time
                }
                DispStatus::Mod => {
                    // Dataset may or may not exist (append mode)
                }
            }
        }

        Ok(path)
    }

    /// Write inline data to a file.
    fn write_inline_data(&self, path: &Path, data: &[String]) -> Result<(), JclError> {
        let mut file = std::fs::File::create(path).map_err(|e| JclError::ExecutionFailed {
            message: format!("Failed to create inline data file: {}", e),
        })?;

        for line in data {
            writeln!(file, "{}", line).map_err(|e| JclError::ExecutionFailed {
                message: format!("Failed to write inline data: {}", e),
            })?;
        }

        Ok(())
    }

    /// Execute a program.
    fn execute_program(&mut self, step: &Step, pgm: &str) -> Result<StepResult, JclError> {
        // Handle built-in system utilities
        match pgm {
            "IEFBR14" => {
                // No-op program - just return success
                return Ok(StepResult {
                    name: step.name.clone(),
                    return_code: 0,
                    stdout: String::new(),
                    stderr: String::new(),
                    success: true,
                });
            }
            "SORT" | "DFSORT" | "ICEMAN" => {
                // Sort utility - handle internally
                return self.execute_sort(step);
            }
            _ => {}
        }

        // Find the program executable
        let exe_path = self.find_program(pgm)?;

        // Build command
        let mut cmd = Command::new(&exe_path);
        cmd.current_dir(&self.config.work_dir);

        // Set environment variables for DD names
        for (dd_name, path) in &self.dd_files {
            cmd.env(format!("DD_{}", dd_name), path);
        }

        // Add PARM as command line argument
        if let Some(ref parm) = step.params.parm {
            cmd.arg(parm);
        }

        // Set up stdio
        cmd.stdin(Stdio::null());
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());

        // Execute
        let output = cmd.output().map_err(|e| JclError::ExecutionFailed {
            message: format!("Failed to execute program {}: {}", pgm, e),
        })?;

        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();

        // Get return code (COBOL convention: 0=success, 4=warning, 8=error, 12+=severe)
        let return_code = output.status.code().unwrap_or(255) as u32;
        let success = return_code <= 4;

        Ok(StepResult {
            name: step.name.clone(),
            return_code,
            stdout,
            stderr,
            success,
        })
    }

    /// Execute SORT utility.
    fn execute_sort(&self, step: &Step) -> Result<StepResult, JclError> {
        // Get required DD files
        let sortin = self.dd_files.get("SORTIN").ok_or_else(|| JclError::ExecutionFailed {
            message: "SORT requires SORTIN DD".to_string(),
        })?;

        let sortout = self.dd_files.get("SORTOUT").ok_or_else(|| JclError::ExecutionFailed {
            message: "SORT requires SORTOUT DD".to_string(),
        })?;

        // Get control statements from SYSIN if available
        let control_statements = if let Some(sysin_path) = self.dd_files.get("SYSIN") {
            std::fs::read_to_string(sysin_path).unwrap_or_default()
        } else if let Some(ref parm) = step.params.parm {
            // Use PARM as control statement
            parm.clone()
        } else {
            return Err(JclError::ExecutionFailed {
                message: "SORT requires SYSIN or PARM with control statements".to_string(),
            });
        };

        // Parse control statements
        let parsed = open_mainframe_sort::parse_control_statements(&control_statements)
            .map_err(|e| JclError::ExecutionFailed {
                message: format!("SORT control statement error: {}", e),
            })?;

        // Build sort engine
        let mut engine = if parsed.copy_mode {
            open_mainframe_sort::SortEngine::copy()
        } else {
            let sort_spec = parsed.sort.ok_or_else(|| JclError::ExecutionFailed {
                message: "SORT FIELDS specification required".to_string(),
            })?;
            open_mainframe_sort::SortEngine::new(sort_spec)
        };

        if let Some(include) = parsed.include {
            engine = engine.with_include(include);
        }
        if let Some(omit) = parsed.omit {
            engine = engine.with_omit(omit);
        }
        if let Some(inrec) = parsed.inrec {
            engine = engine.with_inrec(inrec);
        }
        if let Some(outrec) = parsed.outrec {
            engine = engine.with_outrec(outrec);
        }
        if let Some(sum_fields) = parsed.sum_fields {
            engine = engine.with_sum(sum_fields);
        }

        // Execute sort
        let stats = engine.sort_file(sortin, sortout).map_err(|e| JclError::ExecutionFailed {
            message: format!("SORT failed: {}", e),
        })?;

        let stdout = format!(
            "SORT COMPLETED - IN: {} OUT: {} FILTERED: {}\n",
            stats.input_records, stats.output_records, stats.filtered_records
        );

        Ok(StepResult {
            name: step.name.clone(),
            return_code: 0,
            stdout,
            stderr: String::new(),
            success: true,
        })
    }

    /// Find the program executable.
    fn find_program(&self, pgm: &str) -> Result<PathBuf, JclError> {
        // Look in program directory
        let candidates = [
            self.config.program_dir.join(pgm),
            self.config.program_dir.join(format!("{}.exe", pgm)),
            self.config.program_dir.join(pgm.to_lowercase()),
            self.config
                .program_dir
                .join(format!("{}.exe", pgm.to_lowercase())),
        ];

        for path in &candidates {
            if path.exists() {
                return Ok(path.clone());
            }
        }

        // Check if it's a well-known system utility
        match pgm {
            "IEFBR14" => {
                // No-op program - just return success
                // We'll handle this specially in execute_program
                Ok(PathBuf::from("IEFBR14"))
            }
            _ => Err(JclError::ExecutionFailed {
                message: format!(
                    "Program {} not found. Searched: {:?}",
                    pgm, self.config.program_dir
                ),
            }),
        }
    }
}

impl Default for JobExecutor {
    fn default() -> Self {
        Self::new()
    }
}

/// Execute a JCL job from source.
pub fn run(source: &str) -> Result<JobResult, JclError> {
    let job = crate::parser::parse(source)?;
    let mut executor = JobExecutor::new();
    executor.execute(&job)
}

/// Execute a JCL job from source with custom configuration.
pub fn run_with_config(source: &str, config: ExecutionConfig) -> Result<JobResult, JclError> {
    let job = crate::parser::parse(source)?;
    let mut executor = JobExecutor::with_config(config);
    executor.execute(&job)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_execution_config_default() {
        let config = ExecutionConfig::default();
        assert!(config.program_dir.ends_with("bin"));
        assert!(config.dataset_dir.ends_with("datasets"));
    }

    #[test]
    fn test_resolve_dataset_path() {
        let config = ExecutionConfig {
            dataset_dir: PathBuf::from("/data"),
            ..Default::default()
        };
        let mut executor = JobExecutor::with_config(config);

        let path = executor.resolve_dataset("MY.DATA.SET", &None).unwrap();
        assert_eq!(path, PathBuf::from("/data/MY/DATA/SET"));
    }

    #[test]
    fn test_resolve_pds_member() {
        let config = ExecutionConfig {
            dataset_dir: PathBuf::from("/data"),
            ..Default::default()
        };
        let mut executor = JobExecutor::with_config(config);

        let path = executor.resolve_dataset("MY.PDS(MEMBER)", &None).unwrap();
        assert_eq!(path, PathBuf::from("/data/MY/PDS/MEMBER.cbl"));
    }

    #[test]
    fn test_should_execute_step_no_cond() {
        let executor = JobExecutor::new();
        let step = Step::program(Some("STEP1".to_string()), "TEST");
        assert!(executor.should_execute_step(&step, &[]));
    }

    #[test]
    fn test_should_execute_step_with_cond() {
        let executor = JobExecutor::new();
        let mut step = Step::program(Some("STEP2".to_string()), "TEST");
        step.params.cond = Some(vec![Condition {
            code: 4,
            operator: ConditionOperator::Gt,
            step: None,
        }]);

        // Previous step returned 0, condition is "bypass if 4 > 0" which is true
        let prev = vec![StepResult {
            name: Some("STEP1".to_string()),
            return_code: 0,
            stdout: String::new(),
            stderr: String::new(),
            success: true,
        }];

        // Step should be bypassed
        assert!(!executor.should_execute_step(&step, &prev));
    }

    #[test]
    fn test_sort_jcl_integration() {
        use std::fs;

        // Create temp directory for test
        let temp_dir = std::env::temp_dir().join("jcl_sort_test");
        let _ = fs::remove_dir_all(&temp_dir);
        fs::create_dir_all(&temp_dir).unwrap();

        // Create input file with consistent 7-char records
        let input_path = temp_dir.join("SORTIN.dat");
        let output_path = temp_dir.join("SORTOUT.dat");
        let sysin_path = temp_dir.join("SYSIN.dat");

        fs::write(&input_path, "Charlie\nAlpha..\nBravo..\nDelta..\n").unwrap();
        fs::write(&sysin_path, "  SORT FIELDS=(1,7,CH,A)\n").unwrap();

        // Create executor with temp directories
        let config = ExecutionConfig {
            program_dir: temp_dir.clone(),
            dataset_dir: temp_dir.clone(),
            work_dir: temp_dir.clone(),
            sysout_dir: temp_dir.clone(),
        };
        let mut executor = JobExecutor::with_config(config);

        // Set up DD files manually
        executor.dd_files.insert("SORTIN".to_string(), input_path);
        executor.dd_files.insert("SORTOUT".to_string(), output_path.clone());
        executor.dd_files.insert("SYSIN".to_string(), sysin_path);

        // Create a step
        let step = Step::program(Some("SORT01".to_string()), "SORT");

        // Execute sort
        let result = executor.execute_sort(&step).unwrap();

        assert!(result.success);
        assert_eq!(result.return_code, 0);
        assert!(result.stdout.contains("SORT COMPLETED"));

        // Verify output is sorted
        let output = fs::read_to_string(&output_path).unwrap();
        assert_eq!(output, "Alpha..\nBravo..\nCharlie\nDelta..\n");

        // Cleanup
        let _ = fs::remove_dir_all(&temp_dir);
    }
}
