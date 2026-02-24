//! JCL job executor.
//!
//! Executes JCL jobs by running compiled COBOL programs.

pub mod iebcopy;
pub mod iebgener;
pub mod utility;

use std::collections::HashMap;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use crate::ast::*;
use crate::error::JclError;
use crate::procedure::{FilesystemProcLib, ProcedureExpander};

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
    /// Dataset overrides: DSN → host path.
    /// Used by the mount bridge to map specific dataset names to host filesystem paths.
    /// For PDS mounts the path should be a directory; for sequential mounts a file.
    /// Keys are uppercase DSN strings (e.g., "MOUNT.COBOL.SRC").
    pub dataset_overrides: HashMap<String, PathBuf>,
}

impl Default for ExecutionConfig {
    fn default() -> Self {
        let current = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
        Self {
            program_dir: current.join("bin"),
            dataset_dir: current.join("datasets"),
            work_dir: current.join("work"),
            sysout_dir: current.join("sysout"),
            dataset_overrides: HashMap::new(),
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
    /// Shared utility registry from `open-mainframe-utilities` crate.
    shared_utilities: open_mainframe_utilities::UtilityRegistry,
    /// GDG generation map: tracks resolved (+N) generations within a job.
    /// Key: (base_dsn, relative_gen), Value: absolute_gen.
    /// On real z/OS, GDG(+1) refers to the same generation throughout a job.
    gdg_resolved: HashMap<(String, i32), u32>,
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
            shared_utilities: open_mainframe_utilities::UtilityRegistry::with_builtins(),
            gdg_resolved: HashMap::new(),
        }
    }

    /// Get a reference to the shared utility registry.
    pub fn shared_utilities(&self) -> &open_mainframe_utilities::UtilityRegistry {
        &self.shared_utilities
    }

    /// Execute a JCL job.
    pub fn execute(&mut self, job: &Job) -> Result<JobResult, JclError> {
        // Clear per-job GDG resolution cache
        self.gdg_resolved.clear();

        // Create working directories
        self.create_directories()?;

        // Expand procedure references before execution
        let expanded_job = self.expand_procedures(job)?;

        let mut step_results = Vec::new();
        let mut max_rc = 0u32;
        let mut all_success = true;
        let mut step_idx = 0;

        self.execute_entries(
            &expanded_job.entries,
            &mut step_results,
            &mut max_rc,
            &mut all_success,
            &mut step_idx,
        )?;

        Ok(JobResult {
            name: expanded_job.name.clone(),
            steps: step_results,
            return_code: max_rc,
            success: all_success,
        })
    }

    /// Expand all procedure references in a job using the ProcedureExpander.
    fn expand_procedures(&self, job: &Job) -> Result<Job, JclError> {
        // Check if there are any procedure references to expand
        let has_procs = job.entries.iter().any(|entry| {
            matches!(entry, JobEntry::Step(step) if matches!(step.exec, ExecType::Procedure(_)))
        });

        if !has_procs {
            return Ok(job.clone());
        }

        let mut expander = ProcedureExpander::new(
            job.in_stream_procs.clone(),
            job.symbols.clone(),
        );

        // Set up filesystem procedure library from JCLLIB ORDER
        if !job.jcllib_order.is_empty() {
            let proc_lib = FilesystemProcLib::new(
                &self.config.dataset_dir,
                &job.jcllib_order,
            );
            expander = expander.with_library(Box::new(proc_lib));
        }

        // Set DD overrides from calling JCL
        if !job.dd_overrides.is_empty() {
            expander = expander.with_dd_overrides(job.dd_overrides.clone());
        }

        expander.expand(job)
    }

    /// Execute a list of job entries (steps and IF constructs).
    fn execute_entries(
        &mut self,
        entries: &[JobEntry],
        step_results: &mut Vec<StepResult>,
        max_rc: &mut u32,
        all_success: &mut bool,
        step_idx: &mut usize,
    ) -> Result<(), JclError> {
        for entry in entries {
            match entry {
                JobEntry::Step(ref step) => {
                    if !self.should_execute_step(step, step_results) {
                        *step_idx += 1;
                        continue;
                    }

                    let result = self.execute_step(step, *step_idx)?;
                    *max_rc = (*max_rc).max(result.return_code);
                    *all_success = *all_success && result.success;
                    step_results.push(result);
                    *step_idx += 1;
                }
                JobEntry::If(if_construct) => {
                    let condition_met =
                        self.evaluate_condition(&if_construct.condition, step_results);
                    let branch = if condition_met {
                        &if_construct.then_entries
                    } else {
                        &if_construct.else_entries
                    };
                    self.execute_entries(
                        branch,
                        step_results,
                        max_rc,
                        all_success,
                        step_idx,
                    )?;
                }
            }
        }
        Ok(())
    }

    /// Evaluate an IF condition expression against step results.
    fn evaluate_condition(
        &self,
        condition: &ConditionExpr,
        step_results: &[StepResult],
    ) -> bool {
        Self::eval_condition(condition, step_results)
    }

    /// Recursive condition evaluator (static to avoid clippy only_used_in_recursion).
    fn eval_condition(
        condition: &ConditionExpr,
        step_results: &[StepResult],
    ) -> bool {
        match condition {
            ConditionExpr::RcCompare {
                step_name,
                operator,
                value,
            } => {
                let rc = step_results
                    .iter()
                    .find(|r| r.name.as_deref() == Some(step_name))
                    .map(|r| r.return_code);

                if let Some(rc) = rc {
                    match operator {
                        ConditionOperator::Gt => rc > *value,
                        ConditionOperator::Ge => rc >= *value,
                        ConditionOperator::Eq => rc == *value,
                        ConditionOperator::Ne => rc != *value,
                        ConditionOperator::Lt => rc < *value,
                        ConditionOperator::Le => rc <= *value,
                    }
                } else {
                    false
                }
            }
            ConditionExpr::Abend { step_name } => {
                step_results
                    .iter()
                    .find(|r| r.name.as_deref() == Some(step_name))
                    .map(|r| !r.success && r.return_code > 4095)
                    .unwrap_or(false)
            }
            ConditionExpr::AbendCc {
                step_name,
                operator: _,
                value: _,
            } => {
                step_results
                    .iter()
                    .find(|r| r.name.as_deref() == Some(step_name))
                    .map(|r| !r.success && r.return_code > 4095)
                    .unwrap_or(false)
            }
            ConditionExpr::Run { step_name } => {
                step_results
                    .iter()
                    .any(|r| r.name.as_deref() == Some(step_name))
            }
            ConditionExpr::Not(expr) => !Self::eval_condition(expr, step_results),
            ConditionExpr::And(exprs) => exprs
                .iter()
                .all(|e| Self::eval_condition(e, step_results)),
            ConditionExpr::Or(exprs) => exprs
                .iter()
                .any(|e| Self::eval_condition(e, step_results)),
        }
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

    /// Ensure a path exists as a directory, handling file-at-directory conflicts.
    ///
    /// When mapping flat z/OS dataset qualifiers to a filesystem hierarchy,
    /// a qualifier like `A.B` may exist as a file while `A.B.C` requires
    /// `A/B/` to be a directory. This method detects such conflicts and
    /// renames the blocking file to `<name>.data` so the directory can
    /// be created.
    fn ensure_directory_path(&self, dir: &Path) -> Result<(), JclError> {
        if dir.is_dir() {
            return Ok(());
        }

        // Walk from the root of dataset_dir toward dir, looking for
        // components that are files when they need to be directories.
        let mut current = self.config.dataset_dir.clone();
        if let Ok(rel) = dir.strip_prefix(&self.config.dataset_dir) {
            for component in rel.components() {
                current.push(component);
                if current.is_file() {
                    // Rename blocking file: A/B → A/B.data, then create A/B/
                    let renamed = current.with_extension("data");
                    std::fs::rename(&current, &renamed).map_err(|e| {
                        JclError::ExecutionFailed {
                            message: format!(
                                "Failed to rename blocking file {:?} to {:?}: {}",
                                current, renamed, e
                            ),
                        }
                    })?;
                    std::fs::create_dir_all(&current).map_err(|e| {
                        JclError::ExecutionFailed {
                            message: format!(
                                "Failed to create directory {:?}: {}",
                                current, e
                            ),
                        }
                    })?;
                }
            }
        }

        // Final create_dir_all for any remaining path
        std::fs::create_dir_all(dir).map_err(|e| JclError::ExecutionFailed {
            message: format!("Failed to create directory {:?}: {}", dir, e),
        })
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
                // Procedures should have been expanded by expand_procedures().
                // If we reach here, expansion failed or was skipped.
                Err(JclError::ExecutionFailed {
                    message: format!(
                        "Procedure '{}' was not expanded before execution. \
                         Check JCLLIB ORDER and procedure library paths.",
                        proc
                    ),
                })
            }
        }
    }

    /// Build a full DSN string including GDG reference if present.
    /// The parser strips `(+1)` from DSN and stores it in `gdg_generation`,
    /// but `resolve_dataset` expects the full DSN with GDG reference inline.
    fn full_dsn(def: &crate::ast::DatasetDef) -> String {
        if let Some(gen) = def.gdg_generation {
            if gen > 0 {
                format!("{}(+{})", def.dsn, gen)
            } else if gen < 0 {
                format!("{}({})", def.dsn, gen)
            } else {
                format!("{}(0)", def.dsn)
            }
        } else {
            def.dsn.clone()
        }
    }

    /// Set up DD file mappings.
    fn setup_dd_files(&mut self, step: &Step, step_idx: usize) -> Result<(), JclError> {
        for dd in &step.dd_statements {
            let path = match &dd.definition {
                DdDefinition::Dataset(def) => {
                    let dsn = Self::full_dsn(def);
                    self.resolve_dataset(&dsn, &def.disp)?
                }
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
                    // Concatenation: combine all datasets into a single temp file
                    if datasets.len() == 1 {
                        let dsn = Self::full_dsn(&datasets[0]);
                        self.resolve_dataset(&dsn, &datasets[0].disp)?
                    } else {
                        let concat_path = self.config.work_dir.join(
                            format!("{}_{}.concat", dd.name, step_idx),
                        );
                        self.concatenate_datasets(datasets, &concat_path)?;
                        concat_path
                    }
                }
                DdDefinition::UssFile(def) => {
                    // USS file: use the path directly
                    PathBuf::from(&def.path)
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

        // Handle member names like MY.PDS(MEMBER) or GDG references like MY.GDG(+1)
        let (dsn_path, member) = if let Some(pos) = dsn.find('(') {
            let member_end = dsn.find(')').unwrap_or(dsn.len());
            let member = &dsn[pos + 1..member_end];
            let base_dsn = &dsn[..pos];

            // Check for GDG relative generation: (+1), (0), (-1), etc.
            let trimmed = member.trim();
            if trimmed == "0"
                || trimmed.starts_with('+')
                || trimmed.starts_with('-')
            {
                if let Ok(gen) = trimmed.trim_start_matches('+').parse::<i32>() {
                    let resolved = self.resolve_gdg(base_dsn, gen)?;
                    // resolve_gdg returns "BASE.G0004V00" — convert to path
                    let mut path = self.config.dataset_dir.clone();
                    for part in resolved.split('.') {
                        path.push(part);
                    }
                    // For new generations (+1), create parent dirs
                    if gen > 0 {
                        if let Some(parent) = path.parent() {
                            self.ensure_directory_path(parent)?;
                        }
                    }
                    return Ok(path);
                }
                // If parse fails, fall through to treat as PDS member
                (base_dsn, Some(member))
            } else {
                (base_dsn, Some(member))
            }
        } else {
            (dsn, None)
        };

        // Check dataset overrides (mount bridge) before standard resolution.
        let dsn_upper = dsn_path.to_uppercase();
        if let Some(override_path) = self.config.dataset_overrides.get(&dsn_upper) {
            if let Some(member_name) = member {
                // PDS member from override: look for file in host directory
                let member_upper = member_name.to_uppercase();
                // Try exact member name first
                let member_path = override_path.join(&member_upper);
                if member_path.exists() {
                    return Ok(member_path);
                }
                // Try with common extensions (matching host filenames)
                for ext in &["cbl", "jcl", "ctl", "prc", "cob", "cpy", "txt"] {
                    let ext_path = override_path.join(format!("{}.{}", member_upper, ext));
                    if ext_path.exists() {
                        return Ok(ext_path);
                    }
                    // Also try lowercase extension with uppercase member
                    let lower_path = override_path.join(format!("{}.{}", member_name.to_lowercase(), ext));
                    if lower_path.exists() {
                        return Ok(lower_path);
                    }
                }
                // Default: use the exact member name in the override directory
                return Ok(override_path.join(member_name));
            } else {
                // Sequential dataset or PDS base directory
                return Ok(override_path.clone());
            }
        }

        let mut path = self.config.dataset_dir.clone();
        for part in dsn_path.split('.') {
            path.push(part);
        }

        if let Some(member_name) = member {
            // PDS member - look for the member file (try exact name first,
            // then common extensions like .cbl, .jcl, .ctl)
            let member_path = path.join(member_name);
            if member_path.exists() {
                return Ok(member_path);
            }
            for ext in &["cbl", "jcl", "ctl", "prc"] {
                let ext_path = path.join(format!("{}.{}", member_name, ext));
                if ext_path.exists() {
                    return Ok(ext_path);
                }
            }
            // Default: use the exact member name (may be created by the step)
            path.push(member_name);
        } else {
            // Check if this is a VSAM cluster by looking for .vsam file
            let vsam_path = path.with_extension("vsam");
            if vsam_path.exists() {
                return Ok(vsam_path);
            }

            // Handle file-at-directory conflict: if the path is a directory
            // (because ensure_directory_path renamed the original file to
            // <name>/data), return the .data file inside the directory.
            if path.is_dir() {
                let data_path = path.join("data");
                if data_path.exists() {
                    return Ok(data_path);
                }
            }
        }

        // Handle disposition
        if let Some(disp) = disp {
            match disp.status {
                DispStatus::New => {
                    // Create parent directories.
                    // On z/OS, dataset qualifiers are flat names — a DSN like A.B.C
                    // doesn't imply A.B is a directory. In our filesystem mapping,
                    // A.B might already exist as a file while A.B.C needs A/B/ to
                    // be a directory. Resolve conflicts by renaming the blocking file.
                    if let Some(parent) = path.parent() {
                        self.ensure_directory_path(parent)?;
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

    /// Concatenate multiple datasets into a single output file.
    fn concatenate_datasets(
        &mut self,
        datasets: &[DatasetDef],
        output_path: &Path,
    ) -> Result<(), JclError> {
        let mut out = std::fs::File::create(output_path).map_err(|e| JclError::ExecutionFailed {
            message: format!("Failed to create concatenation file: {}", e),
        })?;

        for ds in datasets {
            let dsn = Self::full_dsn(ds);
            let path = self.resolve_dataset(&dsn, &ds.disp)?;
            if path.exists() {
                // Skip directories (PDS/PDSE/LOADLIB) in concatenation — they
                // act as search libraries and cannot be read as sequential data.
                if path.is_dir() {
                    continue;
                }
                let content = std::fs::read(&path).map_err(|e| JclError::ExecutionFailed {
                    message: format!("Failed to read dataset {} for concatenation: {}", ds.dsn, e),
                })?;
                out.write_all(&content).map_err(|e| JclError::ExecutionFailed {
                    message: format!("Failed to write concatenated data: {}", e),
                })?;
            }
        }
        Ok(())
    }

    /// Resolve a GDG relative reference to an absolute dataset name.
    ///
    /// Given a base name like "MY.GDG" and generation +1, scans the dataset directory
    /// for existing generations (G0001V00 pattern) and returns the resolved name.
    pub fn resolve_gdg(&mut self, base: &str, generation: i32) -> Result<String, JclError> {
        let base_upper = base.to_uppercase();

        // On real z/OS, a relative GDG reference like (+1) or (0) always refers
        // to the same absolute generation throughout a single job. Return the
        // previously resolved generation if we've seen this (base, gen) pair.
        let key = (base_upper.clone(), generation);
        if let Some(&abs) = self.gdg_resolved.get(&key) {
            return Ok(format!("{}.G{:04}V00", base_upper, abs));
        }

        // Convert DSN to directory path
        let mut base_path = self.config.dataset_dir.clone();
        for part in base_upper.split('.') {
            base_path.push(part);
        }

        // Find existing generations by scanning directory
        let mut max_gen = 0u32;
        if base_path.exists() && base_path.is_dir() {
            if let Ok(entries) = std::fs::read_dir(&base_path) {
                for entry in entries.flatten() {
                    let name = entry.file_name().to_string_lossy().to_string();
                    // Match pattern G0001V00
                    if name.starts_with('G') && name.len() >= 8 && name.contains('V') {
                        if let Ok(g) = name[1..5].parse::<u32>() {
                            max_gen = max_gen.max(g);
                        }
                    }
                }
            }
        }

        let target_gen = (max_gen as i32 + generation).max(0) as u32;
        // Special case: generation 0 means current (max_gen)
        let abs_gen = if generation == 0 {
            max_gen
        } else {
            target_gen
        };

        // Cache the resolution so subsequent references within this job
        // resolve to the same generation.
        self.gdg_resolved.insert(key, abs_gen);

        Ok(format!("{}.G{:04}V00", base_upper, abs_gen))
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
        // Handle SORT utilities specially (they need step-level access)
        match pgm {
            "SORT" | "DFSORT" | "ICEMAN" => {
                return self.execute_sort(step);
            }
            _ => {}
        }

        // File-based utilities that need direct disk I/O
        match pgm {
            "IEBGENER" => {
                return iebgener::Iebgener.execute(
                    step.name.as_deref(),
                    &self.dd_files,
                    step.params.parm.as_deref(),
                );
            }
            "IEBCOPY" => {
                return iebcopy::Iebcopy.execute(
                    step.name.as_deref(),
                    &self.dd_files,
                    step.params.parm.as_deref(),
                );
            }
            "IDCAMS" => {
                return utility::execute_idcams(
                    step.name.as_deref(),
                    &self.dd_files,
                    step.params.parm.as_deref(),
                    &self.config.dataset_dir,
                );
            }
            _ => {}
        }

        // Check the shared utilities crate for known IBM utilities
        // (IEFBR14, IEBCOMPR, IKJEFT01, IKJEFT1A, IKJEFT1B, IRXJCL, BPXBATCH, etc.)
        if self.shared_utilities.is_registered(pgm) {
            return self.execute_via_shared_utility(step, pgm);
        }

        // Find the program executable — if not found, run as a stub
        let exe_path = match self.find_program(pgm) {
            Ok(path) => path,
            Err(_) => {
                // Program not found on disk — execute as a generic stub.
                // This handles COBOL batch programs, CICS utilities, etc.
                // that don't have real executables in the emulated environment.
                return self.execute_stub_program(step, pgm);
            }
        };

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

    /// Extract LRECL from DD statements in a step (checks SORTIN, SORTOUT, or any DD with DCB).
    fn extract_lrecl(step: &Step) -> Option<usize> {
        // Check SORTIN first, then SORTOUT
        for dd_name in &["SORTIN", "SORTOUT"] {
            for dd in &step.dd_statements {
                if dd.name == *dd_name {
                    if let DdDefinition::Dataset(ref def) = dd.definition {
                        if let Some(ref dcb) = def.dcb {
                            if let Some(lrecl) = dcb.lrecl {
                                if lrecl > 0 {
                                    return Some(lrecl as usize);
                                }
                            }
                        }
                    }
                }
            }
        }
        None
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

        // Read SYMNAMES DD if present — defines symbolic field names.
        // Field entries (NAME,pos,len,fmt) and constant entries (NAME,C'value')
        // get substituted into the control statements.
        let symnames: HashMap<String, String> =
            if let Some(sym_path) = self.dd_files.get("SYMNAMES") {
                Self::parse_symnames(sym_path)
            } else {
                HashMap::new()
            };

        // Get control statements from SYSIN if available
        let mut control_statements = if let Some(sysin_path) = self.dd_files.get("SYSIN") {
            std::fs::read_to_string(sysin_path).unwrap_or_default()
        } else if let Some(ref parm) = step.params.parm {
            // Use PARM as control statement
            parm.clone()
        } else {
            // No SYSIN or PARM — default to OPTION COPY if SYMNAMES present
            if !symnames.is_empty() {
                "  OPTION COPY\n".to_string()
            } else {
                return Err(JclError::ExecutionFailed {
                    message: "SORT requires SYSIN or PARM with control statements".to_string(),
                });
            }
        };

        // Resolve symbolic field names from SYMNAMES.
        // Sort by name length descending to avoid partial replacements
        // (e.g., "TRAN-CARD" matching before "TRAN-CARD-NUM").
        if !symnames.is_empty() {
            let mut sorted_names: Vec<_> = symnames.iter().collect();
            sorted_names.sort_by(|a, b| b.0.len().cmp(&a.0.len()));
            for (name, expansion) in sorted_names {
                control_statements = control_statements.replace(name, expansion);
            }
        }

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

        // If DCB specifies LRECL and RECFM=FB, use Fixed record format.
        // This is critical for binary VSAM input that cannot be read as text lines.
        if let Some(lrecl) = Self::extract_lrecl(step) {
            engine = engine.with_record_length(lrecl);
        } else {
            // Check if sortin is a VSAM file (.vsam extension) — use fixed-length
            // records derived from file size if available.
            let sortin_str = sortin.to_string_lossy();
            if sortin_str.ends_with(".vsam") {
                if let Ok(meta) = std::fs::metadata(sortin) {
                    let size = meta.len() as usize;
                    // Try common LRECL values that divide evenly
                    for &candidate in &[350, 300, 250, 200, 150, 100, 80, 50] {
                        if size > 0 && size % candidate == 0 {
                            engine = engine.with_record_length(candidate);
                            break;
                        }
                    }
                }
            }
        }

        // Execute sort — if SORTIN doesn't exist, produce empty output (RC=0).
        // This matches z/OS behavior where sorting an empty or non-existent input
        // produces an empty output file with RC=0.
        if !sortin.exists() {
            // Create empty output file
            if let Some(parent) = sortout.parent() {
                let _ = std::fs::create_dir_all(parent);
            }
            let _ = std::fs::write(sortout, b"");
            return Ok(StepResult {
                name: step.name.clone(),
                return_code: 0,
                stdout: "SORT COMPLETED - IN: 0 OUT: 0 FILTERED: 0\n".to_string(),
                stderr: String::new(),
                success: true,
            });
        }

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

    /// Execute a utility via the shared `open-mainframe-utilities` crate.
    ///
    /// Bridges between the JCL executor's file-based DD model and the
    /// utilities crate's in-memory DD model:
    /// 1. Reads file DDs into `UtilityContext` as inline data
    /// 2. Dispatches to the shared registry
    /// 3. Writes output DDs back to disk
    /// 4. Converts `UtilityResult` → `StepResult`
    fn execute_via_shared_utility(
        &self,
        step: &Step,
        pgm: &str,
    ) -> Result<StepResult, JclError> {
        use open_mainframe_utilities::{DdAllocation, UtilityContext};

        let step_name = step.name.as_deref().unwrap_or("");
        let mut ctx = UtilityContext::new(step_name, pgm);

        // Convert file-based DDs to in-memory DDs
        for (dd_name, path) in &self.dd_files {
            if path.as_os_str() == "/dev/null" {
                ctx.add_dd(DdAllocation::dummy(dd_name));
            } else if path.exists() {
                if path.is_dir() {
                    // Directory (PDS/PDSE/LOADLIB) — treat as allocated but with no inline data
                    ctx.add_dd(DdAllocation::output(dd_name));
                } else if let Ok(content) = std::fs::read_to_string(path) {
                    let lines: Vec<String> = content.lines().map(|l| l.to_string()).collect();
                    ctx.add_dd(DdAllocation::inline(dd_name, lines));
                } else {
                    ctx.add_dd(DdAllocation::output(dd_name));
                }
            } else {
                ctx.add_dd(DdAllocation::output(dd_name));
            }
        }

        // Dispatch via shared registry
        let result = self
            .shared_utilities
            .dispatch(pgm, &mut ctx)
            .map_err(|e| JclError::ExecutionFailed {
                message: e.to_string(),
            })?;

        // Write output DDs back to disk
        for dd_name in ctx.dd_names() {
            if let Some(dd) = ctx.get_dd(&dd_name) {
                if !dd.output.is_empty() {
                    if let Some(path) = self.dd_files.get(&dd_name) {
                        if path.as_os_str() != "/dev/null" {
                            let content = dd.output.join("\n") + "\n";
                            let _ = std::fs::write(path, &content);
                        }
                    }
                }
            }
        }

        // Convert UtilityResult → StepResult
        let sysprint = ctx.sysprint_output();
        let stdout = if sysprint.is_empty() {
            String::new()
        } else {
            sysprint.join("\n") + "\n"
        };

        Ok(StepResult {
            name: step.name.clone(),
            return_code: result.condition_code,
            stdout,
            stderr: String::new(),
            success: result.condition_code <= 4,
        })
    }

    /// Execute an unknown program as a generic stub.
    ///
    /// When a program is not in the utility registry and not found as an
    /// executable on disk, this runs it as a no-op stub that reads available
    /// input DDs, writes a summary to output DDs, and returns RC=0.
    /// This handles COBOL batch programs, CICS utilities, and other programs
    /// that don't have real executables in the emulated environment.
    fn execute_stub_program(&self, step: &Step, pgm: &str) -> Result<StepResult, JclError> {
        let mut lines = Vec::new();
        lines.push(format!("{}: PROGRAM STARTED (STUB)", pgm));

        if let Some(ref parm) = step.params.parm {
            lines.push(format!("{}: PARM={}", pgm, parm));
        }

        // Log allocated DDs
        let mut dd_names: Vec<&String> = self.dd_files.keys().collect();
        dd_names.sort();
        for dd in &dd_names {
            lines.push(format!("{}: DD ALLOCATED: {}", pgm, dd));
        }

        // Read input DDs if present
        for dd_name in &["SYSUT1", "SYSIN", "CARDIN", "CUSTFILE", "TRNXFILE", "XREFFILE"] {
            if let Some(path) = self.dd_files.get(*dd_name) {
                if path.exists() {
                    if let Ok(meta) = std::fs::metadata(path) {
                        lines.push(format!("{}: {}: {} BYTES READ", pgm, dd_name, meta.len()));
                    }
                }
            }
        }

        // Write output marker to SYSPRINT/SYSOUT if allocated
        for dd_name in &["SYSPRINT", "SYSOUT"] {
            if let Some(path) = self.dd_files.get(*dd_name) {
                let _ = std::fs::write(path, format!("{} COMPLETED SUCCESSFULLY\n", pgm));
            }
        }

        lines.push(format!("{}: PROGRAM ENDED - RC=0", pgm));

        Ok(StepResult {
            name: step.name.clone(),
            return_code: 0,
            stdout: lines.join("\n") + "\n",
            stderr: String::new(),
            success: true,
        })
    }

    /// Parse a SYMNAMES DD file into a map of symbolic name -> expansion string.
    ///
    /// SYMNAMES format (one entry per line):
    /// - Field:    `NAME,position,length,format`  expands to `pos,len,fmt`
    /// - Constant: `NAME,C'value'` or `NAME,X'hex'` expands to `C'value'`
    /// Lines starting with `*` or blank lines are ignored.
    fn parse_symnames(path: &Path) -> HashMap<String, String> {
        let mut map = HashMap::new();
        if let Ok(content) = std::fs::read_to_string(path) {
            for line in content.lines() {
                let trimmed = line.trim();
                if trimmed.is_empty() || trimmed.starts_with('*') || trimmed.starts_with("/*") {
                    continue;
                }
                // Split only on the first comma to get name + rest
                if let Some(comma_pos) = trimmed.find(',') {
                    let name = trimmed[..comma_pos].trim().to_uppercase();
                    let rest = trimmed[comma_pos + 1..].trim();

                    // Check if rest starts with a constant literal (C'...' or X'...')
                    let upper_rest = rest.to_uppercase();
                    if upper_rest.starts_with("C'") || upper_rest.starts_with("X'") {
                        // Extract just the literal: C'value' or X'hex'
                        // Find the closing quote after the opening C' or X'
                        let literal = if let Some(end_quote) = rest[2..].find('\'') {
                            &rest[..end_quote + 3] // prefix(2) + content + closing quote
                        } else {
                            rest // No closing quote found, use the whole thing
                        };
                        map.insert(name, literal.to_string());
                    } else {
                        // Field symbol: pos,len,format
                        let parts: Vec<&str> = rest.split(',').collect();
                        if parts.len() >= 3 {
                            if let (Ok(pos), Ok(len)) = (
                                parts[0].trim().parse::<u32>(),
                                parts[1].trim().parse::<u32>(),
                            ) {
                                let fmt = parts[2].trim().to_uppercase();
                                map.insert(name, format!("{},{},{}", pos, len, fmt));
                            }
                        }
                    }
                }
            }
        }
        map
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

        Err(JclError::ExecutionFailed {
            message: format!(
                "Program {} not found. Searched: {:?}",
                pgm, self.config.program_dir
            ),
        })
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
    use std::fs;

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
        // When no file exists on disk, default to exact member name (no extension)
        assert_eq!(path, PathBuf::from("/data/MY/PDS/MEMBER"));
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
            procstep: None,
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
            dataset_overrides: HashMap::new(),
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

    // ======================================================================
    // Epic 103: IF/THEN/ELSE/ENDIF — Story 103.3: Condition Evaluation
    // ======================================================================

    /// Story 103.3: RC=0 condition evaluates to true when step returns 0.
    #[test]
    fn test_evaluate_condition_rc_eq_true() {
        let executor = JobExecutor::new();
        let condition = ConditionExpr::RcCompare {
            step_name: "STEP1".to_string(),
            operator: ConditionOperator::Eq,
            value: 0,
        };
        let results = vec![StepResult {
            name: Some("STEP1".to_string()),
            return_code: 0,
            stdout: String::new(),
            stderr: String::new(),
            success: true,
        }];

        assert!(executor.evaluate_condition(&condition, &results));
    }

    /// Story 103.3: RC=0 condition evaluates to false when step returns 4.
    #[test]
    fn test_evaluate_condition_rc_eq_false() {
        let executor = JobExecutor::new();
        let condition = ConditionExpr::RcCompare {
            step_name: "STEP1".to_string(),
            operator: ConditionOperator::Eq,
            value: 0,
        };
        let results = vec![StepResult {
            name: Some("STEP1".to_string()),
            return_code: 4,
            stdout: String::new(),
            stderr: String::new(),
            success: true,
        }];

        assert!(!executor.evaluate_condition(&condition, &results));
    }

    /// Story 103.3: ABEND condition is true for abended step.
    #[test]
    fn test_evaluate_condition_abend() {
        let executor = JobExecutor::new();
        let condition = ConditionExpr::Abend {
            step_name: "STEP1".to_string(),
        };
        // An abended step has high RC and not successful
        let results = vec![StepResult {
            name: Some("STEP1".to_string()),
            return_code: 4096, // > 4095 = abend
            stdout: String::new(),
            stderr: String::new(),
            success: false,
        }];

        assert!(executor.evaluate_condition(&condition, &results));
    }

    /// Story 103.3: RUN condition is true when step exists in results.
    #[test]
    fn test_evaluate_condition_run() {
        let executor = JobExecutor::new();
        let condition = ConditionExpr::Run {
            step_name: "STEP1".to_string(),
        };
        let results = vec![StepResult {
            name: Some("STEP1".to_string()),
            return_code: 0,
            stdout: String::new(),
            stderr: String::new(),
            success: true,
        }];

        assert!(executor.evaluate_condition(&condition, &results));
        // Step that didn't run
        assert!(!executor.evaluate_condition(
            &ConditionExpr::Run {
                step_name: "STEP2".to_string()
            },
            &results
        ));
    }

    /// Story 103.3: AND condition — both must be true.
    #[test]
    fn test_evaluate_condition_and() {
        let executor = JobExecutor::new();
        let condition = ConditionExpr::And(vec![
            ConditionExpr::RcCompare {
                step_name: "STEP1".to_string(),
                operator: ConditionOperator::Eq,
                value: 0,
            },
            ConditionExpr::RcCompare {
                step_name: "STEP2".to_string(),
                operator: ConditionOperator::Le,
                value: 4,
            },
        ]);
        let results = vec![
            StepResult {
                name: Some("STEP1".to_string()),
                return_code: 0,
                stdout: String::new(),
                stderr: String::new(),
                success: true,
            },
            StepResult {
                name: Some("STEP2".to_string()),
                return_code: 4,
                stdout: String::new(),
                stderr: String::new(),
                success: true,
            },
        ];

        assert!(executor.evaluate_condition(&condition, &results));
    }

    /// Story 103.3: OR condition — at least one must be true.
    #[test]
    fn test_evaluate_condition_or() {
        let executor = JobExecutor::new();
        let condition = ConditionExpr::Or(vec![
            ConditionExpr::RcCompare {
                step_name: "STEP1".to_string(),
                operator: ConditionOperator::Eq,
                value: 0,
            },
            ConditionExpr::Abend {
                step_name: "STEP1".to_string(),
            },
        ]);
        let results = vec![StepResult {
            name: Some("STEP1".to_string()),
            return_code: 0,
            stdout: String::new(),
            stderr: String::new(),
            success: true,
        }];

        // First condition true (RC=0), so OR is true
        assert!(executor.evaluate_condition(&condition, &results));
    }

    // ======================================================================
    // Epic 107: GDG Resolution
    // ======================================================================

    /// Story 107.2: GDG resolution for new generation (+1).
    #[test]
    fn test_resolve_gdg_positive() {
        let temp_dir = std::env::temp_dir().join("jcl_gdg_test");
        let _ = fs::remove_dir_all(&temp_dir);
        let dataset_dir = temp_dir.join("datasets");

        // Create GDG base directory with existing generations
        let gdg_dir = dataset_dir.join("MY").join("GDG");
        fs::create_dir_all(&gdg_dir).unwrap();
        fs::write(gdg_dir.join("G0001V00"), "gen1").unwrap();
        fs::write(gdg_dir.join("G0002V00"), "gen2").unwrap();
        fs::write(gdg_dir.join("G0003V00"), "gen3").unwrap();

        let config = ExecutionConfig {
            dataset_dir,
            ..Default::default()
        };
        let mut executor = JobExecutor::with_config(config);

        // +1 should resolve to G0004V00
        let resolved = executor.resolve_gdg("MY.GDG", 1).unwrap();
        assert_eq!(resolved, "MY.GDG.G0004V00");

        // 0 should resolve to current = G0003V00
        let resolved = executor.resolve_gdg("MY.GDG", 0).unwrap();
        assert_eq!(resolved, "MY.GDG.G0003V00");

        let _ = fs::remove_dir_all(&temp_dir);
    }

    /// Story 103.3: NOT condition.
    #[test]
    fn test_evaluate_condition_not() {
        let executor = JobExecutor::new();
        let condition = ConditionExpr::Not(Box::new(ConditionExpr::RcCompare {
            step_name: "STEP1".to_string(),
            operator: ConditionOperator::Gt,
            value: 4,
        }));
        let results = vec![StepResult {
            name: Some("STEP1".to_string()),
            return_code: 0, // 0 > 4 is false, so NOT(false) = true
            stdout: String::new(),
            stderr: String::new(),
            success: true,
        }];

        assert!(executor.evaluate_condition(&condition, &results));
    }

    // ======================================================================
    // Epic 108: Utility Registry Integration Tests
    // ======================================================================

    /// Story 108.1: Executor uses shared utilities crate for standard IBM utilities.
    #[test]
    fn test_executor_shared_utilities() {
        let executor = JobExecutor::new();
        assert!(executor.shared_utilities().is_registered("IEFBR14"));
        assert!(executor.shared_utilities().is_registered("IEBCOMPR"));
        assert!(executor.shared_utilities().is_registered("IKJEFT01"));
        assert!(executor.shared_utilities().is_registered("IKJEFT1B"));
    }

    // ======================================================================
    // Epic 109: Executor Test Suite Expansion (Story 109.3)
    // ======================================================================

    /// Story 109.3: COND bypass — step bypassed when condition is true.
    #[test]
    fn test_cond_bypass_skips_step() {
        let executor = JobExecutor::new();
        let mut step = Step::program(Some("STEP2".to_string()), "TEST");
        step.params.cond = Some(vec![Condition {
            code: 0,
            operator: ConditionOperator::Eq,
            step: Some("STEP1".to_string()),
            procstep: None,
        }]);

        let prev = vec![StepResult {
            name: Some("STEP1".to_string()),
            return_code: 0,
            stdout: String::new(),
            stderr: String::new(),
            success: true,
        }];

        // COND=(0,EQ,STEP1) — "bypass if 0 EQ STEP1.RC", STEP1.RC=0, so 0==0 true => bypass
        assert!(!executor.should_execute_step(&step, &prev));
    }

    /// Story 109.3: COND not met — step executes.
    #[test]
    fn test_cond_not_met_executes_step() {
        let executor = JobExecutor::new();
        let mut step = Step::program(Some("STEP2".to_string()), "TEST");
        step.params.cond = Some(vec![Condition {
            code: 0,
            operator: ConditionOperator::Eq,
            step: Some("STEP1".to_string()),
            procstep: None,
        }]);

        let prev = vec![StepResult {
            name: Some("STEP1".to_string()),
            return_code: 4, // RC=4, COND=(0,EQ,STEP1) => 0==4 false => execute
            stdout: String::new(),
            stderr: String::new(),
            success: true,
        }];

        assert!(executor.should_execute_step(&step, &prev));
    }

    /// Story 109.3: Multiple COND conditions — any true => bypass.
    #[test]
    fn test_cond_multiple_any_true_bypasses() {
        let executor = JobExecutor::new();
        let mut step = Step::program(Some("STEP3".to_string()), "TEST");
        step.params.cond = Some(vec![
            Condition {
                code: 8,
                operator: ConditionOperator::Gt,
                step: Some("STEP1".to_string()),
                procstep: None,
            },
            Condition {
                code: 0,
                operator: ConditionOperator::Eq,
                step: Some("STEP2".to_string()),
                procstep: None,
            },
        ]);

        let prev = vec![
            StepResult {
                name: Some("STEP1".to_string()),
                return_code: 4,
                stdout: String::new(),
                stderr: String::new(),
                success: true,
            },
            StepResult {
                name: Some("STEP2".to_string()),
                return_code: 0, // COND=(0,EQ,STEP2) => 0==0 true => bypass
                stdout: String::new(),
                stderr: String::new(),
                success: true,
            },
        ];

        assert!(!executor.should_execute_step(&step, &prev));
    }

    /// Story 109.3: Dataset creation with DISP=NEW creates parent dirs.
    #[test]
    fn test_resolve_dataset_new_creates_dirs() {
        let temp_dir = std::env::temp_dir().join("jcl_exec_new_test");
        let _ = fs::remove_dir_all(&temp_dir);
        let dataset_dir = temp_dir.join("datasets");

        let config = ExecutionConfig {
            dataset_dir: dataset_dir.clone(),
            work_dir: temp_dir.join("work"),
            sysout_dir: temp_dir.join("sysout"),
            ..Default::default()
        };
        let mut executor = JobExecutor::with_config(config);

        let disp = Disposition {
            status: DispStatus::New,
            normal: Some(DispAction::Catlg),
            abnormal: Some(DispAction::Delete),
        };

        let path = executor
            .resolve_dataset("MY.NEW.DATA", &Some(disp))
            .unwrap();
        assert_eq!(path, dataset_dir.join("MY").join("NEW").join("DATA"));
        // Parent directories should be created
        assert!(dataset_dir.join("MY").join("NEW").exists());

        let _ = fs::remove_dir_all(&temp_dir);
    }

    /// Story 109.3: DD concatenation merges multiple datasets.
    #[test]
    fn test_concatenate_datasets() {
        let temp_dir = std::env::temp_dir().join("jcl_concat_test");
        let _ = fs::remove_dir_all(&temp_dir);
        let dataset_dir = temp_dir.join("datasets");
        fs::create_dir_all(&dataset_dir).unwrap();

        // Create source files
        fs::write(dataset_dir.join("FILE1"), "AAA\n").unwrap();
        fs::write(dataset_dir.join("FILE2"), "BBB\n").unwrap();

        let config = ExecutionConfig {
            dataset_dir: dataset_dir.clone(),
            work_dir: temp_dir.join("work"),
            sysout_dir: temp_dir.join("sysout"),
            ..Default::default()
        };
        let mut executor = JobExecutor::with_config(config);

        let datasets = vec![
            DatasetDef {
                dsn: "FILE1".to_string(),
                ..Default::default()
            },
            DatasetDef {
                dsn: "FILE2".to_string(),
                ..Default::default()
            },
        ];

        let output_path = temp_dir.join("concat.out");
        executor
            .concatenate_datasets(&datasets, &output_path)
            .unwrap();

        let content = fs::read_to_string(&output_path).unwrap();
        assert!(content.contains("AAA"));
        assert!(content.contains("BBB"));

        let _ = fs::remove_dir_all(&temp_dir);
    }

    /// Story 109.3: Inline data written to temp file.
    #[test]
    fn test_write_inline_data() {
        let temp_dir = std::env::temp_dir().join("jcl_inline_test");
        let _ = fs::remove_dir_all(&temp_dir);
        fs::create_dir_all(&temp_dir).unwrap();

        let executor = JobExecutor::new();
        let path = temp_dir.join("inline.dat");
        let data = vec![
            "  SORT FIELDS=(1,10,CH,A)".to_string(),
            "  OPTION COPY".to_string(),
        ];
        executor.write_inline_data(&path, &data).unwrap();

        let content = fs::read_to_string(&path).unwrap();
        assert!(content.contains("SORT FIELDS"));
        assert!(content.contains("OPTION COPY"));

        let _ = fs::remove_dir_all(&temp_dir);
    }

    /// Story 109.3: GDG negative generation reference.
    #[test]
    fn test_resolve_gdg_negative() {
        let temp_dir = std::env::temp_dir().join("jcl_gdg_neg_test");
        let _ = fs::remove_dir_all(&temp_dir);
        let dataset_dir = temp_dir.join("datasets");

        let gdg_dir = dataset_dir.join("MY").join("GDG");
        fs::create_dir_all(&gdg_dir).unwrap();
        fs::write(gdg_dir.join("G0001V00"), "gen1").unwrap();
        fs::write(gdg_dir.join("G0002V00"), "gen2").unwrap();
        fs::write(gdg_dir.join("G0003V00"), "gen3").unwrap();

        let config = ExecutionConfig {
            dataset_dir,
            ..Default::default()
        };
        let mut executor = JobExecutor::with_config(config);

        // -1 from max (3) = G0002V00
        let resolved = executor.resolve_gdg("MY.GDG", -1).unwrap();
        assert_eq!(resolved, "MY.GDG.G0002V00");

        let _ = fs::remove_dir_all(&temp_dir);
    }

    /// Regression: resolve_dataset with GDG relative generation (+1) syntax
    /// should resolve through resolve_gdg instead of treating as PDS member.
    #[test]
    fn test_resolve_dataset_gdg_relative() {
        let temp_dir = std::env::temp_dir().join("jcl_gdg_dsn_test");
        let _ = fs::remove_dir_all(&temp_dir);
        let dataset_dir = temp_dir.join("datasets");

        // Create GDG base with existing generations
        let gdg_dir = dataset_dir.join("MY").join("GDG");
        fs::create_dir_all(&gdg_dir).unwrap();
        fs::write(gdg_dir.join("G0001V00"), "gen1").unwrap();
        fs::write(gdg_dir.join("G0002V00"), "gen2").unwrap();

        let config = ExecutionConfig {
            dataset_dir: dataset_dir.clone(),
            work_dir: temp_dir.join("work"),
            sysout_dir: temp_dir.join("sysout"),
            ..Default::default()
        };
        let mut executor = JobExecutor::with_config(config);

        // (+1) → next generation = G0003V00
        let path = executor.resolve_dataset("MY.GDG(+1)", &None).unwrap();
        assert_eq!(path, dataset_dir.join("MY").join("GDG").join("G0003V00"));

        // (0) → current generation = G0002V00
        let path = executor.resolve_dataset("MY.GDG(0)", &None).unwrap();
        assert_eq!(path, dataset_dir.join("MY").join("GDG").join("G0002V00"));

        // (-1) → previous generation = G0001V00
        let path = executor.resolve_dataset("MY.GDG(-1)", &None).unwrap();
        assert_eq!(path, dataset_dir.join("MY").join("GDG").join("G0001V00"));

        let _ = fs::remove_dir_all(&temp_dir);
    }

    /// Story 109.3: Unknown program returns error.
    #[test]
    fn test_find_program_not_found() {
        let temp_dir = std::env::temp_dir().join("jcl_notfound_test");
        let _ = fs::remove_dir_all(&temp_dir);
        fs::create_dir_all(&temp_dir).unwrap();

        let config = ExecutionConfig {
            program_dir: temp_dir.clone(),
            ..Default::default()
        };
        let executor = JobExecutor::with_config(config);

        let result = executor.find_program("NONEXISTENT");
        assert!(result.is_err());
        let msg = format!("{}", result.unwrap_err());
        assert!(msg.contains("NONEXISTENT"), "Error should mention program name: {}", msg);

        let _ = fs::remove_dir_all(&temp_dir);
    }

    /// Test dataset_overrides: mounted DSN resolves to host path instead of dataset_dir.
    #[test]
    fn test_resolve_dataset_with_override() {
        let temp_dir = std::env::temp_dir().join("jcl_override_test");
        let _ = fs::remove_dir_all(&temp_dir);
        let mount_dir = temp_dir.join("mounted");
        fs::create_dir_all(&mount_dir).unwrap();

        // Create a member file in the "mounted" directory (uppercase name, like real mount bridge)
        fs::write(mount_dir.join("PAYROLL.cbl"), "IDENTIFICATION DIVISION.\n").unwrap();

        let mut overrides = HashMap::new();
        overrides.insert("MOUNT.COBOL.SRC".to_string(), mount_dir.clone());

        let config = ExecutionConfig {
            dataset_dir: temp_dir.join("datasets"),
            dataset_overrides: overrides,
            ..Default::default()
        };
        let mut executor = JobExecutor::with_config(config);

        // Base DSN override should return the mounted directory
        let path = executor.resolve_dataset("MOUNT.COBOL.SRC", &None).unwrap();
        assert_eq!(path, mount_dir);

        // PDS member should resolve into the mounted directory with extension
        let path = executor.resolve_dataset("MOUNT.COBOL.SRC(PAYROLL)", &None).unwrap();
        assert_eq!(path, mount_dir.join("PAYROLL.cbl"));

        // Non-overridden DSN should resolve normally
        let path = executor.resolve_dataset("OTHER.DATA.SET", &None).unwrap();
        assert_eq!(path, temp_dir.join("datasets").join("OTHER").join("DATA").join("SET"));

        let _ = fs::remove_dir_all(&temp_dir);
    }
}
