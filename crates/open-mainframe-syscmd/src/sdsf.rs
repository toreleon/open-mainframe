//! # SDSF — System Display and Search Facility
//!
//! Provides SDSF panel data model, rendering, line commands,
//! REXX API (ISFEXEC), and JES2 integration.

use std::collections::HashMap;

// ─────────────────────── Job Types & Status ───────────────────────

/// Job type in SDSF.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JobType {
    /// Batch job (JOB).
    Job,
    /// Started task (STC).
    Stc,
    /// TSO user (TSU).
    Tsu,
}

impl JobType {
    /// Display string.
    pub fn as_str(self) -> &'static str {
        match self {
            JobType::Job => "JOB",
            JobType::Stc => "STC",
            JobType::Tsu => "TSU",
        }
    }
}

/// Job status in SDSF.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JobStatus {
    /// Job is actively executing.
    Active,
    /// Job is waiting for input.
    Input,
    /// Job has output available.
    Output,
    /// Job is complete (no output held).
    Complete,
    /// Job is canceled.
    Canceled,
    /// Job ABENDed.
    Abended,
}

impl JobStatus {
    /// Display string.
    pub fn as_str(self) -> &'static str {
        match self {
            JobStatus::Active => "ACTIVE",
            JobStatus::Input => "INPUT",
            JobStatus::Output => "OUTPUT",
            JobStatus::Complete => "COMPLETE",
            JobStatus::Canceled => "CANCELED",
            JobStatus::Abended => "ABENDED",
        }
    }
}

// ─────────────────────── SDSF Job Entry ───────────────────────

/// An SDSF job entry.
#[derive(Debug, Clone)]
pub struct SdsfJob {
    /// Job name.
    pub jobname: String,
    /// Job ID (e.g., JOB12345).
    pub jobid: String,
    /// Job type.
    pub job_type: JobType,
    /// Owner (submitter).
    pub owner: String,
    /// Current status.
    pub status: JobStatus,
    /// Current step name (if active).
    pub step_name: String,
    /// Current program (if active).
    pub program: String,
    /// CPU time in seconds.
    pub cpu_time: f64,
    /// Return code (if complete).
    pub return_code: Option<u16>,
    /// Priority.
    pub priority: u8,
    /// SYSOUT datasets.
    pub sysout_datasets: Vec<SysoutDataset>,
    /// JCL source text.
    pub jcl: String,
}

/// A SYSOUT dataset for the O/H panel.
#[derive(Debug, Clone)]
pub struct SysoutDataset {
    /// DD name.
    pub ddname: String,
    /// Step name.
    pub step_name: String,
    /// Output class.
    pub class: char,
    /// Number of records.
    pub record_count: u32,
    /// Content lines.
    pub content: Vec<String>,
}

// ─────────────────────── System Log ───────────────────────

/// A system log entry.
#[derive(Debug, Clone)]
pub struct LogEntry {
    /// Timestamp.
    pub timestamp: String,
    /// Job name that issued the message.
    pub jobname: String,
    /// Message ID.
    pub message_id: String,
    /// Message text.
    pub text: String,
}

// ─────────────────────── SDSF Panel ───────────────────────

/// Panel type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PanelType {
    /// DA — Display Active.
    Da,
    /// ST — Status.
    St,
    /// O — Output.
    Output,
    /// H — Held output.
    Held,
    /// LOG — System log.
    Log,
}

/// A rendered panel row.
#[derive(Debug, Clone)]
pub struct PanelRow {
    /// Column values.
    pub columns: Vec<String>,
}

/// A rendered panel.
#[derive(Debug, Clone)]
pub struct RenderedPanel {
    /// Panel type.
    pub panel_type: PanelType,
    /// Column headers.
    pub headers: Vec<String>,
    /// Data rows.
    pub rows: Vec<PanelRow>,
    /// Total matching entries.
    pub total: usize,
    /// Filter applied.
    pub filter: Option<String>,
}

// ─────────────────────── Line Commands ───────────────────────

/// SDSF line command.
#[derive(Debug, Clone, PartialEq)]
pub enum LineCommand {
    /// S — Select (browse output).
    Select,
    /// SJ — Display JCL.
    SelectJcl,
    /// SE — Display JES messages.
    SelectMessages,
    /// SP — Purge job.
    Purge,
    /// SB — Browse output.
    Browse,
    /// ? — Job details.
    Details,
}

/// Result of executing a line command.
#[derive(Debug, Clone)]
pub struct LineCommandResult {
    /// Whether the command succeeded.
    pub success: bool,
    /// Output lines.
    pub lines: Vec<String>,
}

// ─────────────────────── SDSF Engine ───────────────────────

/// The SDSF engine — manages jobs, renders panels, executes line commands.
#[derive(Debug)]
pub struct SdsfEngine {
    /// All jobs.
    jobs: Vec<SdsfJob>,
    /// System log.
    log: Vec<LogEntry>,
    /// Prefix filter.
    prefix: Option<String>,
    /// Sort field.
    sort_field: SortField,
    /// Sort ascending.
    sort_ascending: bool,
}

/// Sort field options.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SortField {
    /// Sort by job name.
    Name,
    /// Sort by time (CPU time).
    Time,
    /// Sort by priority.
    Priority,
    /// Sort by status.
    Status,
}

impl SdsfEngine {
    /// Create a new SDSF engine.
    pub fn new() -> Self {
        Self {
            jobs: Vec::new(),
            log: Vec::new(),
            prefix: None,
            sort_field: SortField::Name,
            sort_ascending: true,
        }
    }

    /// Add a job.
    pub fn add_job(&mut self, job: SdsfJob) {
        self.jobs.push(job);
    }

    /// Add a log entry.
    pub fn add_log(&mut self, entry: LogEntry) {
        self.log.push(entry);
    }

    /// Set prefix filter.
    pub fn set_prefix(&mut self, prefix: Option<&str>) {
        self.prefix = prefix.map(|s| s.to_uppercase());
    }

    /// Set sort field.
    pub fn set_sort(&mut self, field: SortField, ascending: bool) {
        self.sort_field = field;
        self.sort_ascending = ascending;
    }

    /// Get filtered and sorted jobs.
    fn filtered_jobs(&self, status_filter: Option<&[JobStatus]>) -> Vec<&SdsfJob> {
        let mut jobs: Vec<&SdsfJob> = self
            .jobs
            .iter()
            .filter(|j| {
                if let Some(pfx) = &self.prefix {
                    let pattern = pfx.trim_end_matches('*');
                    if !j.jobname.starts_with(pattern) {
                        return false;
                    }
                }
                if let Some(statuses) = status_filter {
                    if !statuses.contains(&j.status) {
                        return false;
                    }
                }
                true
            })
            .collect();

        jobs.sort_by(|a, b| {
            let cmp = match self.sort_field {
                SortField::Name => a.jobname.cmp(&b.jobname),
                SortField::Time => a.cpu_time.partial_cmp(&b.cpu_time).unwrap_or(std::cmp::Ordering::Equal),
                SortField::Priority => a.priority.cmp(&b.priority),
                SortField::Status => a.status.as_str().cmp(b.status.as_str()),
            };
            if self.sort_ascending { cmp } else { cmp.reverse() }
        });

        jobs
    }

    /// Render the DA (Display Active) panel.
    pub fn render_da(&self) -> RenderedPanel {
        let active_statuses = [JobStatus::Active, JobStatus::Input];
        let jobs = self.filtered_jobs(Some(&active_statuses));

        let headers = vec![
            "JOBNAME".into(), "TYPE".into(), "STATUS".into(),
            "STEP".into(), "CPU TIME".into(),
        ];

        let rows: Vec<PanelRow> = jobs
            .iter()
            .map(|j| PanelRow {
                columns: vec![
                    j.jobname.clone(),
                    j.job_type.as_str().into(),
                    j.status.as_str().into(),
                    j.step_name.clone(),
                    format!("{:.2}", j.cpu_time),
                ],
            })
            .collect();

        let total = rows.len();
        RenderedPanel {
            panel_type: PanelType::Da,
            headers,
            rows,
            total,
            filter: self.prefix.clone(),
        }
    }

    /// Render the ST (Status) panel.
    pub fn render_st(&self) -> RenderedPanel {
        let jobs = self.filtered_jobs(None);

        let headers = vec![
            "JOBNAME".into(), "JOBID".into(), "OWNER".into(),
            "STATUS".into(), "RC".into(),
        ];

        let rows: Vec<PanelRow> = jobs
            .iter()
            .map(|j| PanelRow {
                columns: vec![
                    j.jobname.clone(),
                    j.jobid.clone(),
                    j.owner.clone(),
                    j.status.as_str().into(),
                    j.return_code
                        .map(|rc| format!("{:04}", rc))
                        .unwrap_or_else(|| "----".into()),
                ],
            })
            .collect();

        let total = rows.len();
        RenderedPanel {
            panel_type: PanelType::St,
            headers,
            rows,
            total,
            filter: self.prefix.clone(),
        }
    }

    /// Render the O (Output) panel for a specific job.
    pub fn render_output(&self, jobname: &str) -> RenderedPanel {
        let job = self.jobs.iter().find(|j| j.jobname == jobname);

        let headers = vec![
            "DDNAME".into(), "STEP".into(), "CLASS".into(), "RECORDS".into(),
        ];

        let rows: Vec<PanelRow> = if let Some(j) = job {
            j.sysout_datasets
                .iter()
                .map(|d| PanelRow {
                    columns: vec![
                        d.ddname.clone(),
                        d.step_name.clone(),
                        d.class.to_string(),
                        d.record_count.to_string(),
                    ],
                })
                .collect()
        } else {
            Vec::new()
        };

        let total = rows.len();
        RenderedPanel {
            panel_type: PanelType::Output,
            headers,
            rows,
            total,
            filter: None,
        }
    }

    /// Render the LOG panel.
    pub fn render_log(&self) -> RenderedPanel {
        let headers = vec![
            "TIME".into(), "JOBNAME".into(), "MSGID".into(), "TEXT".into(),
        ];

        let rows: Vec<PanelRow> = self
            .log
            .iter()
            .map(|e| PanelRow {
                columns: vec![
                    e.timestamp.clone(),
                    e.jobname.clone(),
                    e.message_id.clone(),
                    e.text.clone(),
                ],
            })
            .collect();

        let total = rows.len();
        RenderedPanel {
            panel_type: PanelType::Log,
            headers,
            rows,
            total,
            filter: None,
        }
    }

    /// Execute a line command on a job.
    pub fn execute_line_command(&self, jobname: &str, cmd: LineCommand) -> LineCommandResult {
        let job = self.jobs.iter().find(|j| j.jobname == jobname);

        match (job, &cmd) {
            (Some(j), LineCommand::Select | LineCommand::Browse) => {
                // Show SYSOUT listing
                let mut lines = vec![format!("--- Output for {} ---", j.jobname)];
                for ds in &j.sysout_datasets {
                    lines.push(format!("DD={} STEP={} CLASS={} RECORDS={}", ds.ddname, ds.step_name, ds.class, ds.record_count));
                    for line in &ds.content {
                        lines.push(format!("  {line}"));
                    }
                }
                LineCommandResult { success: true, lines }
            }
            (Some(j), LineCommand::SelectJcl) => {
                let lines: Vec<String> = j.jcl.lines().map(String::from).collect();
                LineCommandResult { success: true, lines }
            }
            (Some(j), LineCommand::Details) => {
                let lines = vec![
                    format!("JOBNAME: {}", j.jobname),
                    format!("JOBID:   {}", j.jobid),
                    format!("TYPE:    {}", j.job_type.as_str()),
                    format!("OWNER:   {}", j.owner),
                    format!("STATUS:  {}", j.status.as_str()),
                    format!("STEP:    {}", j.step_name),
                    format!("PROGRAM: {}", j.program),
                    format!("CPU:     {:.2} SEC", j.cpu_time),
                    format!("RC:      {}", j.return_code.map(|rc| format!("{:04}", rc)).unwrap_or_else(|| "N/A".into())),
                    format!("SYSOUT:  {} DATASETS", j.sysout_datasets.len()),
                ];
                LineCommandResult { success: true, lines }
            }
            (Some(_), LineCommand::SelectMessages) => {
                // Show JES messages from log for this job
                let lines: Vec<String> = self.log.iter()
                    .filter(|l| l.jobname == jobname)
                    .map(|l| format!("{} {} {}", l.timestamp, l.message_id, l.text))
                    .collect();
                LineCommandResult { success: true, lines }
            }
            (Some(_), LineCommand::Purge) => {
                LineCommandResult {
                    success: true,
                    lines: vec![format!("$HASP395 {} PURGED", jobname)],
                }
            }
            (None, _) => LineCommandResult {
                success: false,
                lines: vec![format!("JOB {} NOT FOUND", jobname)],
            },
        }
    }

    /// ISFEXEC — REXX API for querying SDSF panels.
    ///
    /// Returns stem variables as a HashMap. Keys follow REXX convention:
    /// `JNAME.n`, `JNUM.n`, `JTYPE.n`, etc. with `JNAME.0` = count.
    pub fn isfexec(&self, panel: &str, filter: Option<&str>) -> HashMap<String, String> {
        let mut vars = HashMap::new();

        let old_prefix = self.prefix.clone();
        // Apply ISFFILTER
        let filtered_jobs: Vec<&SdsfJob> = self.jobs.iter().filter(|j| {
            if let Some(f) = filter {
                let pattern = f.to_uppercase().replace('*', "");
                j.jobname.starts_with(&pattern)
            } else if let Some(pfx) = &old_prefix {
                let pattern = pfx.trim_end_matches('*');
                j.jobname.starts_with(pattern)
            } else {
                true
            }
        }).collect();

        match panel.to_uppercase().as_str() {
            "DA" => {
                let active: Vec<&&SdsfJob> = filtered_jobs.iter()
                    .filter(|j| j.status == JobStatus::Active || j.status == JobStatus::Input)
                    .collect();

                vars.insert("JNAME.0".into(), active.len().to_string());
                for (i, j) in active.iter().enumerate() {
                    let n = i + 1;
                    vars.insert(format!("JNAME.{n}"), j.jobname.clone());
                    vars.insert(format!("JNUM.{n}"), j.jobid.clone());
                    vars.insert(format!("JTYPE.{n}"), j.job_type.as_str().into());
                    vars.insert(format!("JSTATUS.{n}"), j.status.as_str().into());
                }
            }
            "ST" => {
                vars.insert("JNAME.0".into(), filtered_jobs.len().to_string());
                for (i, j) in filtered_jobs.iter().enumerate() {
                    let n = i + 1;
                    vars.insert(format!("JNAME.{n}"), j.jobname.clone());
                    vars.insert(format!("JNUM.{n}"), j.jobid.clone());
                    vars.insert(format!("JTYPE.{n}"), j.job_type.as_str().into());
                    vars.insert(format!("JSTATUS.{n}"), j.status.as_str().into());
                    vars.insert(format!("JOWNER.{n}"), j.owner.clone());
                    vars.insert(format!("JRC.{n}"), j.return_code.map(|rc| rc.to_string()).unwrap_or_default());
                }
            }
            _ => {
                vars.insert("ISFMSG.0".into(), "1".into());
                vars.insert("ISFMSG.1".into(), format!("UNKNOWN PANEL: {panel}"));
            }
        }

        vars
    }

    /// Update a job's status (simulates JES2 refresh).
    pub fn update_job_status(&mut self, jobname: &str, new_status: JobStatus, rc: Option<u16>) {
        if let Some(job) = self.jobs.iter_mut().find(|j| j.jobname == jobname) {
            job.status = new_status;
            if let Some(code) = rc {
                job.return_code = Some(code);
            }
        }
    }

    /// Number of jobs.
    pub fn job_count(&self) -> usize {
        self.jobs.len()
    }
}

impl Default for SdsfEngine {
    fn default() -> Self {
        Self::new()
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn make_job(name: &str, id: &str, jtype: JobType, status: JobStatus, rc: Option<u16>) -> SdsfJob {
        SdsfJob {
            jobname: name.into(),
            jobid: id.into(),
            job_type: jtype,
            owner: "USER01".into(),
            status,
            step_name: "STEP1".into(),
            program: "MYPROG".into(),
            cpu_time: 1.5,
            return_code: rc,
            priority: 5,
            sysout_datasets: Vec::new(),
            jcl: "//JOB CARD\n//STEP EXEC PGM=MYPROG\n".into(),
        }
    }

    fn setup_engine() -> SdsfEngine {
        let mut engine = SdsfEngine::new();

        engine.add_job(make_job("PAYROLL", "JOB00001", JobType::Job, JobStatus::Active, None));
        engine.add_job(make_job("BACKUP", "JOB00002", JobType::Job, JobStatus::Active, None));
        engine.add_job(make_job("CICS", "STC00003", JobType::Stc, JobStatus::Active, None));
        engine.add_job(make_job("TSOU01", "TSU00004", JobType::Tsu, JobStatus::Active, None));
        engine.add_job(make_job("REPORT1", "JOB00005", JobType::Job, JobStatus::Complete, Some(0)));
        engine.add_job(make_job("REPORT2", "JOB00006", JobType::Job, JobStatus::Output, Some(4)));

        // Add sysout to REPORT1
        let mut report1 = engine.jobs[4].clone();
        report1.sysout_datasets = vec![
            SysoutDataset { ddname: "SYSPRINT".into(), step_name: "STEP1".into(), class: 'A', record_count: 100, content: vec!["REPORT LINE 1".into()] },
            SysoutDataset { ddname: "SYSOUT".into(), step_name: "STEP1".into(), class: 'A', record_count: 50, content: vec!["OUTPUT LINE 1".into()] },
            SysoutDataset { ddname: "SYSMSG".into(), step_name: "STEP1".into(), class: 'M', record_count: 10, content: vec!["MSG LINE 1".into()] },
        ];
        engine.jobs[4] = report1;

        // Add log entries
        engine.add_log(LogEntry {
            timestamp: "12:00:00".into(), jobname: "PAYROLL".into(),
            message_id: "IEF403I".into(), text: "PAYROLL - STARTED".into(),
        });
        engine.add_log(LogEntry {
            timestamp: "12:01:00".into(), jobname: "REPORT1".into(),
            message_id: "IEF404I".into(), text: "REPORT1 - ENDED".into(),
        });
        engine.add_log(LogEntry {
            timestamp: "12:02:00".into(), jobname: "*MASTER*".into(),
            message_id: "IEE136I".into(), text: "LOCAL: DATE=2026.054".into(),
        });

        engine
    }

    // ─── SYS-103.1: Data Model and Engine ───

    #[test]
    fn test_filtering_by_prefix() {
        let mut engine = setup_engine();
        engine.set_prefix(Some("PAY*"));
        let panel = engine.render_da();
        assert_eq!(panel.total, 1);
        assert_eq!(panel.rows[0].columns[0], "PAYROLL");
    }

    #[test]
    fn test_sorting_by_name() {
        let engine = setup_engine();
        let panel = engine.render_da();
        // Should be sorted alphabetically
        let names: Vec<&str> = panel.rows.iter().map(|r| r.columns[0].as_str()).collect();
        let mut sorted = names.clone();
        sorted.sort();
        assert_eq!(names, sorted);
    }

    #[test]
    fn test_sorting_by_priority() {
        let mut engine = setup_engine();
        engine.set_sort(SortField::Priority, true);
        let panel = engine.render_da();
        assert!(panel.total > 0);
    }

    // ─── SYS-103.2: DA Panel ───

    #[test]
    fn test_da_panel_shows_active_jobs() {
        let engine = setup_engine();
        let panel = engine.render_da();

        // Should show 4 active jobs (PAYROLL, BACKUP, CICS, TSOU01)
        assert_eq!(panel.total, 4);
        assert_eq!(panel.panel_type, PanelType::Da);

        // Should show JOB/STC/TSU types
        let types: Vec<&str> = panel.rows.iter().map(|r| r.columns[1].as_str()).collect();
        assert!(types.contains(&"JOB"));
        assert!(types.contains(&"STC"));
        assert!(types.contains(&"TSU"));
    }

    #[test]
    fn test_da_panel_columns() {
        let engine = setup_engine();
        let panel = engine.render_da();
        assert_eq!(panel.headers, vec!["JOBNAME", "TYPE", "STATUS", "STEP", "CPU TIME"]);
    }

    #[test]
    fn test_da_panel_prefix_filter() {
        let mut engine = setup_engine();
        engine.set_prefix(Some("REPORT*"));
        let panel = engine.render_da();
        // No active REPORT jobs
        assert_eq!(panel.total, 0);
    }

    // ─── SYS-103.3: ST Panel ───

    #[test]
    fn test_st_panel_shows_all_jobs() {
        let engine = setup_engine();
        let panel = engine.render_st();
        assert_eq!(panel.total, 6);
        assert_eq!(panel.panel_type, PanelType::St);
    }

    #[test]
    fn test_st_panel_shows_return_code() {
        let engine = setup_engine();
        let panel = engine.render_st();

        // Find REPORT1 (RC=0)
        let report1 = panel.rows.iter().find(|r| r.columns[0] == "REPORT1").unwrap();
        assert_eq!(report1.columns[3], "COMPLETE");
        assert_eq!(report1.columns[4], "0000");

        // Find PAYROLL (no RC)
        let payroll = panel.rows.iter().find(|r| r.columns[0] == "PAYROLL").unwrap();
        assert_eq!(payroll.columns[4], "----");
    }

    // ─── SYS-103.4: O/H Panels ───

    #[test]
    fn test_output_panel_shows_sysout() {
        let engine = setup_engine();
        let panel = engine.render_output("REPORT1");
        assert_eq!(panel.total, 3);
        assert_eq!(panel.panel_type, PanelType::Output);

        let ddnames: Vec<&str> = panel.rows.iter().map(|r| r.columns[0].as_str()).collect();
        assert!(ddnames.contains(&"SYSPRINT"));
        assert!(ddnames.contains(&"SYSOUT"));
        assert!(ddnames.contains(&"SYSMSG"));
    }

    #[test]
    fn test_select_on_output_shows_content() {
        let engine = setup_engine();
        let result = engine.execute_line_command("REPORT1", LineCommand::Select);
        assert!(result.success);
        assert!(result.lines.iter().any(|l| l.contains("REPORT LINE 1")));
    }

    // ─── SYS-103.5: LOG Panel ───

    #[test]
    fn test_log_panel_chronological() {
        let engine = setup_engine();
        let panel = engine.render_log();
        assert_eq!(panel.total, 3);
        assert_eq!(panel.panel_type, PanelType::Log);

        // Should be in chronological order
        assert_eq!(panel.rows[0].columns[0], "12:00:00");
        assert_eq!(panel.rows[1].columns[0], "12:01:00");
        assert_eq!(panel.rows[2].columns[0], "12:02:00");
    }

    #[test]
    fn test_log_panel_has_message_ids() {
        let engine = setup_engine();
        let panel = engine.render_log();
        let msg_ids: Vec<&str> = panel.rows.iter().map(|r| r.columns[2].as_str()).collect();
        assert!(msg_ids.contains(&"IEF403I"));
        assert!(msg_ids.contains(&"IEE136I"));
    }

    // ─── SYS-103.6: Line Commands ───

    #[test]
    fn test_line_command_select_jcl() {
        let engine = setup_engine();
        let result = engine.execute_line_command("PAYROLL", LineCommand::SelectJcl);
        assert!(result.success);
        assert!(result.lines.iter().any(|l| l.contains("JOB CARD")));
    }

    #[test]
    fn test_line_command_details() {
        let engine = setup_engine();
        let result = engine.execute_line_command("PAYROLL", LineCommand::Details);
        assert!(result.success);
        assert!(result.lines.iter().any(|l| l.contains("PAYROLL")));
        assert!(result.lines.iter().any(|l| l.contains("JOB")));
    }

    #[test]
    fn test_line_command_purge() {
        let engine = setup_engine();
        let result = engine.execute_line_command("REPORT1", LineCommand::Purge);
        assert!(result.success);
        assert!(result.lines[0].contains("PURGED"));
    }

    #[test]
    fn test_line_command_not_found() {
        let engine = setup_engine();
        let result = engine.execute_line_command("NOSUCH", LineCommand::Select);
        assert!(!result.success);
    }

    // ─── SYS-103.7: ISFEXEC REXX API ───

    #[test]
    fn test_isfexec_da() {
        let engine = setup_engine();
        let vars = engine.isfexec("DA", None);
        let count: usize = vars.get("JNAME.0").unwrap().parse().unwrap();
        assert_eq!(count, 4);
        assert_eq!(vars.get("JNAME.1").map(String::as_str), Some("PAYROLL"));
    }

    #[test]
    fn test_isfexec_with_filter() {
        let engine = setup_engine();
        let vars = engine.isfexec("DA", Some("PAY*"));
        let count: usize = vars.get("JNAME.0").unwrap().parse().unwrap();
        assert_eq!(count, 1);
        assert_eq!(vars.get("JNAME.1").map(String::as_str), Some("PAYROLL"));
    }

    #[test]
    fn test_isfexec_st() {
        let engine = setup_engine();
        let vars = engine.isfexec("ST", None);
        let count: usize = vars.get("JNAME.0").unwrap().parse().unwrap();
        assert_eq!(count, 6);
        // Check owner
        assert_eq!(vars.get("JOWNER.1").map(String::as_str), Some("USER01"));
    }

    // ─── SYS-103.8: JES2 Integration ───

    #[test]
    fn test_update_job_status() {
        let mut engine = setup_engine();
        engine.update_job_status("PAYROLL", JobStatus::Complete, Some(0));

        let panel = engine.render_da();
        // PAYROLL should no longer be on DA (it's complete now)
        assert!(!panel.rows.iter().any(|r| r.columns[0] == "PAYROLL"));

        let panel = engine.render_st();
        let payroll = panel.rows.iter().find(|r| r.columns[0] == "PAYROLL").unwrap();
        assert_eq!(payroll.columns[3], "COMPLETE");
        assert_eq!(payroll.columns[4], "0000");
    }

    #[test]
    fn test_spool_browsing() {
        let engine = setup_engine();
        let result = engine.execute_line_command("REPORT1", LineCommand::Browse);
        assert!(result.success);
        assert!(result.lines.iter().any(|l| l.contains("SYSPRINT")));
        assert!(result.lines.iter().any(|l| l.contains("REPORT LINE 1")));
    }

    // ─── SYS-103.9: Integration Tests ───

    #[test]
    fn test_100_jobs_all_panels() {
        let mut engine = SdsfEngine::new();
        for i in 0..100 {
            let status = match i % 4 {
                0 => JobStatus::Active,
                1 => JobStatus::Complete,
                2 => JobStatus::Output,
                _ => JobStatus::Input,
            };
            engine.add_job(make_job(
                &format!("JOB{:05}", i),
                &format!("JOB{:05}", i),
                JobType::Job,
                status,
                if status == JobStatus::Complete { Some(0) } else { None },
            ));
        }

        let da = engine.render_da();
        assert_eq!(da.total, 50); // Active + Input

        let st = engine.render_st();
        assert_eq!(st.total, 100);
    }

    #[test]
    fn test_all_panels_render() {
        let engine = setup_engine();

        let da = engine.render_da();
        assert!(da.total > 0);

        let st = engine.render_st();
        assert!(st.total > 0);

        let output = engine.render_output("REPORT1");
        assert!(output.total > 0);

        let log = engine.render_log();
        assert!(log.total > 0);
    }

    #[test]
    fn test_select_messages_line_command() {
        let engine = setup_engine();
        let result = engine.execute_line_command("PAYROLL", LineCommand::SelectMessages);
        assert!(result.success);
        assert!(result.lines.iter().any(|l| l.contains("STARTED")));
    }
}
