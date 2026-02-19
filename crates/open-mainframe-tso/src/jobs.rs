//! TSO job management commands — SUBMIT, STATUS, CANCEL, OUTPUT.
//!
//! These commands bridge the TSO interactive session to the JES2 batch
//! subsystem, allowing users to submit JCL, monitor job status, cancel
//! running jobs, and retrieve spool output.

use open_mainframe_jes2::{Jes2, Job, JobId, JobState};

use crate::commands::CommandResult;
use crate::parser::ParsedCommand;
use crate::session::TsoSession;

/// Try to execute a job-management command.  Returns `Some(result)` if the
/// command was handled, `None` if it is not a job command.
pub fn try_execute(
    session: &mut TsoSession,
    jes: &mut Jes2,
    cmd: &ParsedCommand,
) -> Option<CommandResult> {
    match cmd.name.as_str() {
        "SUBMIT" | "SUB" => Some(cmd_submit(session, jes, cmd)),
        "STATUS" | "ST" => Some(cmd_status(session, jes, cmd)),
        "CANCEL" | "CAN" => Some(cmd_cancel(jes, cmd)),
        "OUTPUT" | "OUT" => Some(cmd_output(jes, cmd)),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// SUBMIT (T101.1)
// ---------------------------------------------------------------------------

fn cmd_submit(
    session: &mut TsoSession,
    jes: &mut Jes2,
    cmd: &ParsedCommand,
) -> CommandResult {
    // SUBMIT 'dsn' or SUBMIT dsn
    let raw_dsn = match cmd.first_positional().or_else(|| cmd.keyword("DA")) {
        Some(d) => d.to_string(),
        None => return CommandResult::error("IKJ56702I MISSING DATASET NAME"),
    };

    let dsn = session.qualify_dsn(&raw_dsn);
    let path = session.resolve_dsn(&dsn);

    // Read the JCL from the dataset
    let jcl = match std::fs::read_to_string(&path) {
        Ok(content) => content,
        Err(_) => {
            return CommandResult::error(&format!(
                "IKJ56704I DATA SET {dsn} NOT FOUND"
            ))
        }
    };

    // Extract job name from the JOB card (first line starting with //)
    let job_name = extract_job_name(&jcl).unwrap_or_else(|| session.userid.clone());

    // Extract class from JCL CLASS= parameter, default to 'A'
    let class = extract_class(&jcl).unwrap_or('A');

    // Submit to JES2
    let job_id = jes.submit(&job_name, class, 1, false);

    // Set owner to current user
    if let Some(job) = jes.get_job_mut(job_id) {
        job.owner = session.userid.clone();
    }

    CommandResult::ok(vec![
        format!("IKJ56250I JOB {job_name}({job_id}) SUBMITTED"),
    ])
}

// ---------------------------------------------------------------------------
// STATUS (T101.2)
// ---------------------------------------------------------------------------

fn cmd_status(
    session: &mut TsoSession,
    jes: &mut Jes2,
    cmd: &ParsedCommand,
) -> CommandResult {
    // STATUS [jobname] — show jobs owned by current user
    let filter_name = cmd.first_positional().map(|s| s.to_ascii_uppercase());

    let jobs = jes.active_jobs();
    let userid = &session.userid;

    let matching: Vec<&Job> = jobs
        .into_iter()
        .filter(|j| j.owner == *userid)
        .filter(|j| {
            if let Some(ref name) = filter_name {
                j.name == *name
            } else {
                true
            }
        })
        .collect();

    if matching.is_empty() {
        let msg = if let Some(ref name) = filter_name {
            format!("IKJ56247I NO JOBS FOUND FOR {name}")
        } else {
            "IKJ56247I NO JOBS FOUND".to_string()
        };
        return CommandResult { output: vec![msg], rc: 4 };
    }

    let mut lines = Vec::new();
    for job in &matching {
        let state_str = format_state(job.state);
        lines.push(format!(
            "IKJ56211I JOB {}({}) ON {} - {}",
            job.name, job.id, format_queue(job.state), state_str
        ));
    }

    CommandResult::ok(lines)
}

// ---------------------------------------------------------------------------
// CANCEL (T101.2)
// ---------------------------------------------------------------------------

fn cmd_cancel(jes: &mut Jes2, cmd: &ParsedCommand) -> CommandResult {
    // CANCEL jobname(jobnnn) or CANCEL jobnnn or CANCEL jobname
    // Parser may interpret CJOB(JOB00001) as keyword CJOB=JOB00001.
    let job_id = match resolve_job_ref(jes, cmd) {
        Some(id) => id,
        None => {
            return CommandResult::error("IKJ56703I JOB NOT FOUND");
        }
    };

    match jes.cancel(job_id) {
        Ok(()) => CommandResult::ok(vec![
            format!("IKJ56249I JOB {job_id} CANCELLED"),
        ]),
        Err(e) => CommandResult::error(&format!("IKJ56703I {e}")),
    }
}

// ---------------------------------------------------------------------------
// OUTPUT (T101.2)
// ---------------------------------------------------------------------------

fn cmd_output(jes: &mut Jes2, cmd: &ParsedCommand) -> CommandResult {
    // OUTPUT jobname(jobnnn) [PRINT(*)]
    // Parser may interpret OUTJOB(JOB00001) as keyword OUTJOB=JOB00001.
    let job_id = match resolve_job_ref(jes, cmd) {
        Some(id) => id,
        None => {
            return CommandResult::error("IKJ56703I JOB NOT FOUND ON OUTPUT QUEUE");
        }
    };

    let job = match jes.get_job(job_id) {
        Some(j) => j,
        None => {
            return CommandResult::error(&format!("IKJ56703I JOB {job_id} NOT FOUND"))
        }
    };

    if job.state != JobState::Output && job.state != JobState::Cancelled {
        return CommandResult::error(&format!(
            "IKJ56703I JOB {job_id} NOT ON OUTPUT QUEUE (STATUS: {})",
            format_state(job.state)
        ));
    }

    let spool_keys = job.spool_keys.clone();
    let job_name = job.name.clone();

    let mut lines = vec![format!(
        "IKJ56210I OUTPUT FOR JOB {job_name}({job_id})"
    )];

    if spool_keys.is_empty() {
        lines.push("  (NO SYSOUT DATASETS)".to_string());
    } else {
        for key in &spool_keys {
            if let Some(ds) = jes.spool.get(*key) {
                lines.push(format!(
                    "  SYSOUT={} DD={} STEP={} RECORDS={}",
                    ds.sysout_class, ds.dd_name, ds.step_name, ds.record_count
                ));
                if let Ok(data) = jes.spool.read(*key) {
                    for line in data {
                        lines.push(format!("  {line}"));
                    }
                }
            }
        }
    }

    CommandResult::ok(lines)
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Resolve a job reference from command arguments.
///
/// Handles multiple forms:
/// - Positional "JOBnnnnn" → direct job ID
/// - Positional "JOBNAME" → find by name in active jobs
/// - Keyword JOBNAME(JOBnnnnn) → parsed as keyword by TSO parser, extract ID
fn resolve_job_ref(jes: &Jes2, cmd: &ParsedCommand) -> Option<JobId> {
    // 1. Check positional argument
    if let Some(arg) = cmd.first_positional() {
        if let Some(id) = parse_job_id(arg) {
            return Some(id);
        }
        // Try by name
        let upper = arg.to_ascii_uppercase();
        if let Some(job) = jes.active_jobs().into_iter().find(|j| j.name == upper) {
            return Some(job.id);
        }
    }

    // 2. Check keywords — parser treats JOBNAME(JOB00001) as keyword JOBNAME=JOB00001
    for (key, val) in &cmd.keywords {
        if let Some(id) = parse_raw_job_id(&val.to_ascii_uppercase()) {
            return Some(id);
        }
        // Also try key as job name if value doesn't parse as ID
        let _ = key; // key might be the job name
    }

    None
}

/// Extract job name from JCL — first `//NAME` on a JOB card.
fn extract_job_name(jcl: &str) -> Option<String> {
    for line in jcl.lines() {
        let line = line.trim();
        if line.starts_with("//") && !line.starts_with("//*") {
            let rest = &line[2..];
            // Job name is the first token
            let name_end = rest.find(|c: char| c.is_whitespace()).unwrap_or(rest.len());
            let name = &rest[..name_end];
            if !name.is_empty() {
                // Check that the next token is "JOB"
                let after = rest[name_end..].trim_start();
                if after.to_ascii_uppercase().starts_with("JOB") {
                    return Some(name.to_ascii_uppercase());
                }
            }
        }
    }
    None
}

/// Extract CLASS=X from a JCL JOB card.
fn extract_class(jcl: &str) -> Option<char> {
    let upper = jcl.to_ascii_uppercase();
    if let Some(pos) = upper.find("CLASS=") {
        let after = &upper[pos + 6..];
        after.chars().next()
    } else {
        None
    }
}

/// Parse a job ID from a string like "JOB00001" or "MYJOB(JOB00001)".
fn parse_job_id(s: &str) -> Option<JobId> {
    let upper = s.to_ascii_uppercase();

    // Try "NAME(JOBnnnnn)" pattern
    if let Some(open) = upper.find('(') {
        if let Some(close) = upper.find(')') {
            let inner = &upper[open + 1..close];
            return parse_raw_job_id(inner);
        }
    }

    // Try bare "JOBnnnnn"
    parse_raw_job_id(&upper)
}

/// Parse "JOBnnnnn" into a JobId.
fn parse_raw_job_id(s: &str) -> Option<JobId> {
    s.strip_prefix("JOB")
        .and_then(|num| num.parse::<u32>().ok())
        .map(JobId)
}

/// Format a job state for display.
fn format_state(state: JobState) -> &'static str {
    match state {
        JobState::Input => "ON INPUT QUEUE",
        JobState::Conversion => "IN CONVERSION",
        JobState::Ready => "AWAITING EXECUTION",
        JobState::Running => "EXECUTING",
        JobState::Output => "ON OUTPUT QUEUE",
        JobState::Purge => "BEING PURGED",
        JobState::Held { .. } => "HELD",
        JobState::Cancelled => "CANCELLED",
    }
}

/// Format the queue name for STATUS display.
fn format_queue(state: JobState) -> &'static str {
    match state {
        JobState::Input | JobState::Conversion => "INPUT",
        JobState::Ready | JobState::Running => "EXECUTION",
        JobState::Output | JobState::Purge => "OUTPUT",
        JobState::Held { .. } => "HELD",
        JobState::Cancelled => "OUTPUT",
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_command;
    use std::path::PathBuf;

    fn test_session() -> TsoSession {
        let temp = std::env::temp_dir().join("tso_job_tests");
        let _ = std::fs::create_dir_all(&temp);
        TsoSession::new("USER01", temp)
    }

    fn write_jcl(session: &TsoSession, dsn: &str, content: &str) -> PathBuf {
        let qualified = session.qualify_dsn(dsn);
        let path = session.resolve_dsn(&qualified);
        if let Some(parent) = path.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        std::fs::write(&path, content).unwrap();
        path
    }

    #[test]
    fn test_submit() {
        let mut s = test_session();
        let mut jes = Jes2::new();

        let jcl = "//MYJOB  JOB (ACCT),'TEST',CLASS=A\n//STEP1  EXEC PGM=IEFBR14\n";
        write_jcl(&s, "TEST.JCL", jcl);

        let cmd = parse_command("SUBMIT TEST.JCL");
        let result = try_execute(&mut s, &mut jes, &cmd).unwrap();
        assert_eq!(result.rc, 0);
        assert!(result.output[0].contains("SUBMITTED"));
        assert!(result.output[0].contains("MYJOB"));
        assert_eq!(jes.job_count(), 1);
    }

    #[test]
    fn test_submit_qualified_dsn() {
        let mut s = test_session();
        let mut jes = Jes2::new();

        // Parser strips quotes from positional, then qualify_dsn adds prefix.
        // Use unquoted positional to stay consistent.
        let jcl = "//BATCH1  JOB ,'RUN',CLASS=B\n//S1 EXEC PGM=IEFBR14\n";
        write_jcl(&s, "MY.JCL.LIB", jcl);

        let cmd = parse_command("SUBMIT MY.JCL.LIB");
        let result = try_execute(&mut s, &mut jes, &cmd).unwrap();
        assert_eq!(result.rc, 0, "output: {:?}", result.output);
        assert!(result.output[0].contains("BATCH1"));
    }

    #[test]
    fn test_submit_not_found() {
        let mut s = test_session();
        let mut jes = Jes2::new();

        let cmd = parse_command("SUBMIT 'NO.SUCH.JCL'");
        let result = try_execute(&mut s, &mut jes, &cmd).unwrap();
        assert!(result.rc > 0);
        assert!(result.output[0].contains("NOT FOUND"));
    }

    #[test]
    fn test_status_no_jobs() {
        let mut s = test_session();
        let mut jes = Jes2::new();

        let cmd = parse_command("STATUS");
        let result = try_execute(&mut s, &mut jes, &cmd).unwrap();
        assert_eq!(result.rc, 4);
        assert!(result.output[0].contains("NO JOBS"));
    }

    #[test]
    fn test_status_with_jobs() {
        let mut s = test_session();
        let mut jes = Jes2::new();

        // Submit a job owned by our user
        let jcl = "//TESTJOB JOB ,'TEST'\n//S1 EXEC PGM=IEFBR14\n";
        write_jcl(&s, "STATUS.JCL", jcl);

        let cmd = parse_command("SUBMIT STATUS.JCL");
        try_execute(&mut s, &mut jes, &cmd);

        let cmd = parse_command("STATUS");
        let result = try_execute(&mut s, &mut jes, &cmd).unwrap();
        assert_eq!(result.rc, 0);
        assert!(result.output[0].contains("TESTJOB"));
        assert!(result.output[0].contains("INPUT"));
    }

    #[test]
    fn test_status_filter_by_name() {
        let mut s = test_session();
        let mut jes = Jes2::new();

        let jcl1 = "//JOBONE  JOB ,'T1'\n//S1 EXEC PGM=IEFBR14\n";
        let jcl2 = "//JOBTWO  JOB ,'T2'\n//S1 EXEC PGM=IEFBR14\n";
        write_jcl(&s, "JCL1", jcl1);
        write_jcl(&s, "JCL2", jcl2);

        let cmd = parse_command("SUBMIT JCL1");
        try_execute(&mut s, &mut jes, &cmd);
        let cmd = parse_command("SUBMIT JCL2");
        try_execute(&mut s, &mut jes, &cmd);

        let cmd = parse_command("STATUS JOBONE");
        let result = try_execute(&mut s, &mut jes, &cmd).unwrap();
        assert_eq!(result.rc, 0);
        assert_eq!(result.output.len(), 1);
        assert!(result.output[0].contains("JOBONE"));
    }

    #[test]
    fn test_cancel_by_id() {
        let mut s = test_session();
        let mut jes = Jes2::new();

        let jcl = "//CJOB    JOB ,'CANCEL'\n//S1 EXEC PGM=IEFBR14\n";
        write_jcl(&s, "CANCEL.JCL", jcl);

        let cmd = parse_command("SUBMIT CANCEL.JCL");
        try_execute(&mut s, &mut jes, &cmd);

        // JES2 starts job numbering at 1
        let cmd = parse_command("CANCEL CJOB(JOB00001)");
        let result = try_execute(&mut s, &mut jes, &cmd).unwrap();
        assert_eq!(result.rc, 0);
        assert!(result.output[0].contains("CANCELLED"));
    }

    #[test]
    fn test_cancel_by_name() {
        let mut s = test_session();
        let mut jes = Jes2::new();

        let jcl = "//CJOB2   JOB ,'CANCEL2'\n//S1 EXEC PGM=IEFBR14\n";
        write_jcl(&s, "CANCEL2.JCL", jcl);

        let cmd = parse_command("SUBMIT CANCEL2.JCL");
        try_execute(&mut s, &mut jes, &cmd);

        let cmd = parse_command("CANCEL CJOB2");
        let result = try_execute(&mut s, &mut jes, &cmd).unwrap();
        assert_eq!(result.rc, 0);
        assert!(result.output[0].contains("CANCELLED"));
    }

    #[test]
    fn test_cancel_not_found() {
        let mut s = test_session();
        let mut jes = Jes2::new();

        let cmd = parse_command("CANCEL XYZJOB");
        let result = try_execute(&mut s, &mut jes, &cmd).unwrap();
        assert!(result.rc > 0);
        assert!(result.output[0].contains("NOT FOUND"));
    }

    #[test]
    fn test_output_no_spool() {
        let mut s = test_session();
        let mut jes = Jes2::new();

        // Submit and advance to Output state
        let jcl = "//OUTJOB  JOB ,'OUTPUT'\n//S1 EXEC PGM=IEFBR14\n";
        write_jcl(&s, "OUT.JCL", jcl);

        let cmd = parse_command("SUBMIT OUT.JCL");
        try_execute(&mut s, &mut jes, &cmd);

        // JES2 starts numbering at 1
        let id = JobId(1);
        jes.advance(id).unwrap(); // -> Conversion
        jes.advance(id).unwrap(); // -> Ready
        jes.advance(id).unwrap(); // -> Running
        jes.advance(id).unwrap(); // -> Output

        let cmd = parse_command("OUTPUT OUTJOB(JOB00001)");
        let result = try_execute(&mut s, &mut jes, &cmd).unwrap();
        assert_eq!(result.rc, 0);
        assert!(result.output[0].contains("OUTPUT FOR JOB"));
        assert!(result.output.iter().any(|l| l.contains("NO SYSOUT")));
    }

    #[test]
    fn test_output_with_spool() {
        let mut s = test_session();
        let mut jes = Jes2::new();

        let jcl = "//SPLJOB  JOB ,'SPOOL'\n//S1 EXEC PGM=IEFBR14\n";
        write_jcl(&s, "SPL.JCL", jcl);

        let cmd = parse_command("SUBMIT SPL.JCL");
        try_execute(&mut s, &mut jes, &cmd);

        let id = JobId(1);

        // Add spool data
        let key = jes.spool.allocate(id, "SYSPRINT", "STEP1", 'A');
        jes.spool.write(key, "HELLO FROM SPOOL").unwrap();
        if let Some(job) = jes.get_job_mut(id) {
            job.spool_keys.push(key);
        }

        // Advance to Output
        jes.advance(id).unwrap(); // -> Conversion
        jes.advance(id).unwrap(); // -> Ready
        jes.advance(id).unwrap(); // -> Running
        jes.advance(id).unwrap(); // -> Output

        let cmd = parse_command("OUTPUT SPLJOB(JOB00001)");
        let result = try_execute(&mut s, &mut jes, &cmd).unwrap();
        assert_eq!(result.rc, 0);
        assert!(result.output.iter().any(|l| l.contains("SYSPRINT")));
        assert!(result.output.iter().any(|l| l.contains("HELLO FROM SPOOL")));
    }

    #[test]
    fn test_output_not_on_output_queue() {
        let mut s = test_session();
        let mut jes = Jes2::new();

        let jcl = "//RUNJOB  JOB ,'RUN'\n//S1 EXEC PGM=IEFBR14\n";
        write_jcl(&s, "RUN.JCL", jcl);

        let cmd = parse_command("SUBMIT RUN.JCL");
        try_execute(&mut s, &mut jes, &cmd);

        // Job is still in Input state
        let cmd = parse_command("OUTPUT RUNJOB(JOB00001)");
        let result = try_execute(&mut s, &mut jes, &cmd).unwrap();
        assert!(result.rc > 0);
        assert!(result.output[0].contains("NOT ON OUTPUT QUEUE"));
    }

    #[test]
    fn test_not_job_command() {
        let mut s = test_session();
        let mut jes = Jes2::new();

        let cmd = parse_command("LISTDS 'SYS1.PARMLIB'");
        assert!(try_execute(&mut s, &mut jes, &cmd).is_none());
    }

    #[test]
    fn test_extract_job_name() {
        assert_eq!(
            extract_job_name("//MYJOB  JOB (ACCT),'TEST'\n//S1 EXEC PGM=X\n"),
            Some("MYJOB".to_string())
        );
        assert_eq!(extract_job_name("// some comment"), None);
        assert_eq!(extract_job_name("//* comment\n//RUN JOB\n"), Some("RUN".to_string()));
    }

    #[test]
    fn test_extract_class() {
        assert_eq!(extract_class("//J JOB ,'T',CLASS=B\n"), Some('B'));
        assert_eq!(extract_class("//J JOB ,'T'\n"), None);
    }

    #[test]
    fn test_parse_job_id() {
        assert_eq!(parse_job_id("JOB00001"), Some(JobId(1)));
        assert_eq!(parse_job_id("MYJOB(JOB00005)"), Some(JobId(5)));
        assert_eq!(parse_job_id("NOJOB"), None);
    }
}
