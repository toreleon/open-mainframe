//! Internal Reader (INTRDR) and SDSF — job-within-job submission and output browsing.
//!
//! - **Internal Reader**: `SYSOUT=(,INTRDR)` — JCL written to this DD is submitted
//!   as a new job to JES2, enabling dynamic job chaining.
//! - **SDSF Panel Model**: Status display (ST), output browsing (S), held output (H),
//!   and active job display (DA).

use crate::job::{Job, JobId, JobState};
use crate::queue::Jes2;
use crate::spool::SpoolManager;

// ---------------------------------------------------------------------------
//  Internal Reader
// ---------------------------------------------------------------------------

/// The JES2 internal reader — accepts JCL and submits it as a new job.
///
/// When a running job writes JCL to a DD defined as `SYSOUT=(,INTRDR)`,
/// the internal reader captures that JCL and submits it to JES2.
#[derive(Debug, Clone)]
pub struct InternalReader {
    /// Buffer for accumulating JCL lines before submission.
    buffer: Vec<String>,
    /// Jobs submitted through this reader (parent_job_id, child_job_id).
    submissions: Vec<(JobId, JobId)>,
}

impl Default for InternalReader {
    fn default() -> Self {
        Self::new()
    }
}

impl InternalReader {
    /// Create a new internal reader.
    pub fn new() -> Self {
        Self {
            buffer: Vec::new(),
            submissions: Vec::new(),
        }
    }

    /// Write a line of JCL to the internal reader buffer.
    pub fn write_line(&mut self, line: &str) {
        self.buffer.push(line.to_string());
    }

    /// Write multiple lines at once.
    pub fn write_lines(&mut self, lines: &[&str]) {
        for line in lines {
            self.buffer.push(line.to_string());
        }
    }

    /// Get the current buffer contents (for inspection).
    pub fn buffer(&self) -> &[String] {
        &self.buffer
    }

    /// Submit the buffered JCL as a new job to JES2.
    ///
    /// Extracts the job name and class from the JOB card, submits to JES2,
    /// and records the parent→child relationship.
    ///
    /// Returns the new job ID, or `None` if no valid JOB card was found.
    pub fn submit(&mut self, parent_job: JobId, jes: &mut Jes2) -> Option<JobId> {
        if self.buffer.is_empty() {
            return None;
        }

        // Parse the JOB card from the buffer.
        let (name, class, priority) = parse_job_card(&self.buffer)?;

        let job_id = jes.submit(&name, class, priority, false);
        self.submissions.push((parent_job, job_id));
        self.buffer.clear();

        Some(job_id)
    }

    /// Clear the buffer without submitting.
    pub fn discard(&mut self) {
        self.buffer.clear();
    }

    /// Get all submissions (parent_job, child_job) pairs.
    pub fn submissions(&self) -> &[(JobId, JobId)] {
        &self.submissions
    }

    /// Get child jobs submitted by a specific parent.
    pub fn children_of(&self, parent: JobId) -> Vec<JobId> {
        self.submissions
            .iter()
            .filter(|(p, _)| *p == parent)
            .map(|(_, c)| *c)
            .collect()
    }

    /// Total number of jobs submitted through the internal reader.
    pub fn total_submissions(&self) -> usize {
        self.submissions.len()
    }
}

/// Parse a JOB card from JCL lines.
///
/// Looks for `//name JOB ...` and extracts name, class (default 'A'), priority (default 1).
fn parse_job_card(lines: &[String]) -> Option<(String, char, u8)> {
    for line in lines {
        let trimmed = line.trim();
        if !trimmed.starts_with("//") {
            continue;
        }
        let rest = trimmed[2..].trim();
        // Use split_whitespace to handle multiple spaces between tokens.
        let tokens: Vec<&str> = rest.split_whitespace().collect();
        if tokens.len() < 2 || !tokens[1].eq_ignore_ascii_case("JOB") {
            continue;
        }

        let name = tokens[0].to_uppercase();
        let mut class = 'A';
        let mut priority = 1u8;

        // Parse operands (everything after "JOB").
        let operands = tokens[2..].join(" ");
        if !operands.is_empty() {
            for param in operands.split(',') {
                let param = param.trim();
                if let Some(val) = param.strip_prefix("CLASS=") {
                    if let Some(c) = val.chars().next() {
                        class = c.to_ascii_uppercase();
                    }
                } else if let Some(val) = param.strip_prefix("PRTY=") {
                    if let Ok(p) = val.parse::<u8>() {
                        priority = p;
                    }
                }
            }
        }

        return Some((name, class, priority));
    }
    None
}

// ---------------------------------------------------------------------------
//  SDSF Panel Model
// ---------------------------------------------------------------------------

/// SDSF panel type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SdsfPanel {
    /// ST — Status display (all jobs).
    Status,
    /// DA — Display active jobs.
    DisplayActive,
    /// H — Held output queue.
    HeldOutput,
    /// O — Output queue.
    OutputQueue,
    /// I — Input queue.
    InputQueue,
}

impl std::fmt::Display for SdsfPanel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Status => write!(f, "ST"),
            Self::DisplayActive => write!(f, "DA"),
            Self::HeldOutput => write!(f, "H"),
            Self::OutputQueue => write!(f, "O"),
            Self::InputQueue => write!(f, "I"),
        }
    }
}

/// SDSF line command.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SdsfLineCommand {
    /// `S` — Select (browse output).
    Select,
    /// `P` — Purge job.
    Purge,
    /// `C` — Cancel job.
    Cancel,
    /// `H` — Hold job/output.
    Hold,
    /// `A` — Release job/output.
    Release,
    /// `?` — Display job details.
    Details,
}

/// A row in an SDSF panel display.
#[derive(Debug, Clone)]
pub struct SdsfRow {
    /// Job name.
    pub jobname: String,
    /// Job number (JOBnnnnn).
    pub jobid: String,
    /// Job owner.
    pub owner: String,
    /// Current status.
    pub status: String,
    /// Return code (if completed).
    pub retcode: Option<String>,
    /// Execution class.
    pub class: String,
    /// Priority.
    pub priority: u8,
}

/// SDSF output browse entry — one SYSOUT dataset.
#[derive(Debug, Clone)]
pub struct SdsfOutputEntry {
    /// DD name.
    pub ddname: String,
    /// Step name.
    pub stepname: String,
    /// SYSOUT class.
    pub class: char,
    /// Number of records.
    pub records: u64,
    /// Spool key for retrieval.
    pub spool_key: u64,
}

/// Generate an SDSF status panel (ST).
///
/// Lists all jobs with their status information.
pub fn sdsf_status(jes: &Jes2) -> Vec<SdsfRow> {
    let jobs = jes.all_jobs();
    let mut rows: Vec<SdsfRow> = jobs.iter().map(|j| job_to_row(j)).collect();
    // Sort by job ID.
    rows.sort_by_key(|r| r.jobid.clone());
    rows
}

/// Generate an SDSF display active panel (DA).
///
/// Lists only running jobs.
pub fn sdsf_display_active(jes: &Jes2) -> Vec<SdsfRow> {
    let jobs = jes.all_jobs();
    jobs.iter()
        .filter(|j| j.state == JobState::Running)
        .map(|j| job_to_row(j))
        .collect()
}

/// Generate an SDSF held output panel (H).
///
/// Lists jobs in Output state that are held.
pub fn sdsf_held_output(jes: &Jes2) -> Vec<SdsfRow> {
    let jobs = jes.all_jobs();
    jobs.iter()
        .filter(|j| j.state.is_held() || j.state == JobState::Output)
        .map(|j| job_to_row(j))
        .collect()
}

/// Generate an SDSF input queue panel (I).
///
/// Lists jobs in Input or Conversion state.
pub fn sdsf_input_queue(jes: &Jes2) -> Vec<SdsfRow> {
    let jobs = jes.all_jobs();
    jobs.iter()
        .filter(|j| matches!(j.state, JobState::Input | JobState::Conversion | JobState::Ready))
        .map(|j| job_to_row(j))
        .collect()
}

/// Generate an SDSF output queue panel (O).
///
/// Lists jobs in Output state.
pub fn sdsf_output_queue(jes: &Jes2) -> Vec<SdsfRow> {
    let jobs = jes.all_jobs();
    jobs.iter()
        .filter(|j| j.state == JobState::Output)
        .map(|j| job_to_row(j))
        .collect()
}

/// Browse output for a job — list its SYSOUT datasets.
pub fn sdsf_browse_output(job_id: JobId, spool: &SpoolManager) -> Vec<SdsfOutputEntry> {
    spool
        .list_for_job(job_id)
        .into_iter()
        .map(|ds| SdsfOutputEntry {
            ddname: ds.dd_name.clone(),
            stepname: ds.step_name.clone(),
            class: ds.sysout_class,
            records: ds.record_count,
            spool_key: ds.key,
        })
        .collect()
}

/// Execute an SDSF line command against a job.
///
/// Returns a description of the action taken.
pub fn sdsf_line_action(
    cmd: &SdsfLineCommand,
    job_id: JobId,
    jes: &mut Jes2,
) -> Result<String, crate::Jes2Error> {
    match cmd {
        SdsfLineCommand::Purge => {
            jes.purge(job_id)?;
            Ok(format!("{job_id} PURGED"))
        }
        SdsfLineCommand::Cancel => {
            jes.cancel(job_id)?;
            Ok(format!("{job_id} CANCELLED"))
        }
        SdsfLineCommand::Hold => {
            jes.hold(job_id)?;
            Ok(format!("{job_id} HELD"))
        }
        SdsfLineCommand::Release => {
            jes.release(job_id)?;
            Ok(format!("{job_id} RELEASED"))
        }
        SdsfLineCommand::Select => Ok(format!("{job_id} SELECTED FOR BROWSE")),
        SdsfLineCommand::Details => {
            let job = jes
                .get_job(job_id)
                .ok_or_else(|| crate::Jes2Error::JobNotFound(job_id.to_string()))?;
            Ok(format!(
                "{} {} CLASS={} PRTY={} STATUS={:?} RC={}",
                job.id, job.name, job.class, job.priority, job.state, job.max_rc
            ))
        }
    }
}

/// Format an SDSF panel header.
pub fn sdsf_header(panel: SdsfPanel) -> String {
    let title = match panel {
        SdsfPanel::Status => "SDSF STATUS DISPLAY ALL CLASSES",
        SdsfPanel::DisplayActive => "SDSF DA - DISPLAY ACTIVE",
        SdsfPanel::HeldOutput => "SDSF HELD OUTPUT DISPLAY",
        SdsfPanel::OutputQueue => "SDSF OUTPUT QUEUE",
        SdsfPanel::InputQueue => "SDSF INPUT QUEUE",
    };
    format!(
        "{}\nNP   JOBNAME  JOBID     OWNER    STATUS    RETCODE  CLASS PRTY",
        title
    )
}

/// Format an SDSF row for display.
pub fn format_sdsf_row(row: &SdsfRow) -> String {
    let rc = row.retcode.as_deref().unwrap_or("");
    format!(
        "     {:<8} {:<9} {:<8} {:<9} {:<8} {:<5} {}",
        row.jobname, row.jobid, row.owner, row.status, rc, row.class, row.priority
    )
}

// ---------------------------------------------------------------------------
//  Internal helpers
// ---------------------------------------------------------------------------

fn job_to_row(job: &Job) -> SdsfRow {
    let status = match job.state {
        JobState::Input => "INPUT".to_string(),
        JobState::Conversion => "CONV".to_string(),
        JobState::Ready => "READY".to_string(),
        JobState::Running => "ACTIVE".to_string(),
        JobState::Output => "OUTPUT".to_string(),
        JobState::Purge => "PURGE".to_string(),
        JobState::Held { .. } => "HELD".to_string(),
        JobState::Cancelled => "CANCEL".to_string(),
    };
    let retcode = if job.state == JobState::Output || job.state == JobState::Purge {
        Some(format!("CC {:04}", job.max_rc))
    } else {
        None
    };
    SdsfRow {
        jobname: job.name.clone(),
        jobid: job.id.to_string(),
        owner: job.owner.clone(),
        status,
        retcode,
        class: job.class.to_string(),
        priority: job.priority,
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // ─── J107.1: Internal Reader ───

    #[test]
    fn test_intrdr_write_and_submit() {
        let mut intrdr = InternalReader::new();
        let mut jes = Jes2::new();

        intrdr.write_lines(&[
            "//CHILD1   JOB CLASS=B,PRTY=10",
            "//STEP1    EXEC PGM=IEFBR14",
        ]);

        let parent = JobId(100);
        let child_id = intrdr.submit(parent, &mut jes).unwrap();
        assert_eq!(jes.job_count(), 1);

        let child = jes.get_job(child_id).unwrap();
        assert_eq!(child.name, "CHILD1");
        assert_eq!(child.priority, 10);
    }

    #[test]
    fn test_intrdr_class_from_job_card() {
        let mut intrdr = InternalReader::new();
        let mut jes = Jes2::new();

        intrdr.write_line("//MYJOB    JOB CLASS=Z");
        let child_id = intrdr.submit(JobId(1), &mut jes).unwrap();

        let child = jes.get_job(child_id).unwrap();
        assert_eq!(child.name, "MYJOB");
    }

    #[test]
    fn test_intrdr_empty_buffer_returns_none() {
        let mut intrdr = InternalReader::new();
        let mut jes = Jes2::new();
        assert!(intrdr.submit(JobId(1), &mut jes).is_none());
    }

    #[test]
    fn test_intrdr_no_job_card_returns_none() {
        let mut intrdr = InternalReader::new();
        let mut jes = Jes2::new();
        intrdr.write_line("//STEP1 EXEC PGM=IEFBR14");
        assert!(intrdr.submit(JobId(1), &mut jes).is_none());
    }

    #[test]
    fn test_intrdr_submissions_tracking() {
        let mut intrdr = InternalReader::new();
        let mut jes = Jes2::new();

        let parent = JobId(1);

        intrdr.write_line("//JOB1 JOB CLASS=A");
        intrdr.submit(parent, &mut jes);

        intrdr.write_line("//JOB2 JOB CLASS=A");
        intrdr.submit(parent, &mut jes);

        assert_eq!(intrdr.total_submissions(), 2);
        assert_eq!(intrdr.children_of(parent).len(), 2);
    }

    #[test]
    fn test_intrdr_discard() {
        let mut intrdr = InternalReader::new();
        intrdr.write_line("//SOMETHING JOB CLASS=A");
        assert_eq!(intrdr.buffer().len(), 1);
        intrdr.discard();
        assert!(intrdr.buffer().is_empty());
    }

    #[test]
    fn test_intrdr_default_class_and_priority() {
        let mut intrdr = InternalReader::new();
        let mut jes = Jes2::new();

        intrdr.write_line("//SIMPLEJ  JOB");
        let child_id = intrdr.submit(JobId(1), &mut jes).unwrap();

        let child = jes.get_job(child_id).unwrap();
        assert_eq!(child.name, "SIMPLEJ");
        assert_eq!(child.priority, 1);
    }

    // ─── J107.2: SDSF Panel Model ───

    fn setup_jes() -> Jes2 {
        let mut jes = Jes2::new();
        let j1 = jes.submit("PAYROLL", 'A', 10, false);
        let j2 = jes.submit("REPORTS", 'B', 5, false);
        let j3 = jes.submit("BACKUP", 'A', 15, false);

        // Advance j1 to Running.
        jes.advance(j1).unwrap();
        jes.advance(j1).unwrap();
        jes.advance(j1).unwrap();

        // Advance j2 to Output.
        jes.advance(j2).unwrap();
        jes.advance(j2).unwrap();
        jes.advance(j2).unwrap();
        jes.advance(j2).unwrap();

        // j3 stays in Input.
        let _ = j3;

        jes
    }

    #[test]
    fn test_sdsf_status_all_jobs() {
        let jes = setup_jes();
        let rows = sdsf_status(&jes);
        assert_eq!(rows.len(), 3);
    }

    #[test]
    fn test_sdsf_display_active() {
        let jes = setup_jes();
        let rows = sdsf_display_active(&jes);
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].jobname, "PAYROLL");
        assert_eq!(rows[0].status, "ACTIVE");
    }

    #[test]
    fn test_sdsf_input_queue() {
        let jes = setup_jes();
        let rows = sdsf_input_queue(&jes);
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].jobname, "BACKUP");
    }

    #[test]
    fn test_sdsf_output_queue() {
        let jes = setup_jes();
        let rows = sdsf_output_queue(&jes);
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].jobname, "REPORTS");
        assert!(rows[0].retcode.is_some());
    }

    #[test]
    fn test_sdsf_browse_output() {
        let mut spool = SpoolManager::new();
        let job_id = JobId(1);
        spool.allocate(job_id, "SYSPRINT", "STEP1", 'A');
        spool.allocate(job_id, "SYSOUT", "STEP1", 'A');
        spool.allocate(job_id, "JESMSGLG", "JES2", 'X');

        let entries = sdsf_browse_output(job_id, &spool);
        assert_eq!(entries.len(), 3);
    }

    #[test]
    fn test_sdsf_line_cancel() {
        let mut jes = setup_jes();
        let j1 = jes.all_jobs()[0].id;
        let result = sdsf_line_action(&SdsfLineCommand::Cancel, j1, &mut jes).unwrap();
        assert!(result.contains("CANCELLED"));
    }

    #[test]
    fn test_sdsf_line_hold_release() {
        let mut jes = Jes2::new();
        let j = jes.submit("TESTJOB", 'A', 5, false);

        let result = sdsf_line_action(&SdsfLineCommand::Hold, j, &mut jes).unwrap();
        assert!(result.contains("HELD"));

        let result = sdsf_line_action(&SdsfLineCommand::Release, j, &mut jes).unwrap();
        assert!(result.contains("RELEASED"));
    }

    #[test]
    fn test_sdsf_line_details() {
        let mut jes = Jes2::new();
        let j = jes.submit("DETAIL", 'B', 8, false);

        let result = sdsf_line_action(&SdsfLineCommand::Details, j, &mut jes).unwrap();
        assert!(result.contains("DETAIL"));
        assert!(result.contains("CLASS=B"));
        assert!(result.contains("PRTY=8"));
    }

    #[test]
    fn test_sdsf_line_purge() {
        let mut jes = Jes2::new();
        let j = jes.submit("PURGEME", 'A', 1, false);
        // Advance to Output.
        jes.advance(j).unwrap();
        jes.advance(j).unwrap();
        jes.advance(j).unwrap();
        jes.advance(j).unwrap();

        let result = sdsf_line_action(&SdsfLineCommand::Purge, j, &mut jes).unwrap();
        assert!(result.contains("PURGED"));
    }

    #[test]
    fn test_sdsf_header() {
        let header = sdsf_header(SdsfPanel::Status);
        assert!(header.contains("STATUS DISPLAY"));
        assert!(header.contains("JOBNAME"));
    }

    #[test]
    fn test_format_sdsf_row() {
        let row = SdsfRow {
            jobname: "TESTJOB".to_string(),
            jobid: "JOB00001".to_string(),
            owner: "USER01".to_string(),
            status: "ACTIVE".to_string(),
            retcode: None,
            class: "A".to_string(),
            priority: 10,
        };
        let formatted = format_sdsf_row(&row);
        assert!(formatted.contains("TESTJOB"));
        assert!(formatted.contains("JOB00001"));
        assert!(formatted.contains("ACTIVE"));
    }

    #[test]
    fn test_sdsf_panel_display() {
        assert_eq!(SdsfPanel::Status.to_string(), "ST");
        assert_eq!(SdsfPanel::DisplayActive.to_string(), "DA");
        assert_eq!(SdsfPanel::HeldOutput.to_string(), "H");
        assert_eq!(SdsfPanel::OutputQueue.to_string(), "O");
        assert_eq!(SdsfPanel::InputQueue.to_string(), "I");
    }

    #[test]
    fn test_parse_job_card_basic() {
        let lines = vec!["//MYJOB JOB CLASS=B,PRTY=7".to_string()];
        let (name, class, prty) = parse_job_card(&lines).unwrap();
        assert_eq!(name, "MYJOB");
        assert_eq!(class, 'B');
        assert_eq!(prty, 7);
    }

    #[test]
    fn test_parse_job_card_no_params() {
        let lines = vec!["//BARE JOB".to_string()];
        let (name, class, prty) = parse_job_card(&lines).unwrap();
        assert_eq!(name, "BARE");
        assert_eq!(class, 'A'); // default
        assert_eq!(prty, 1); // default
    }
}
