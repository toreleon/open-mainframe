//! JES2 operator command parser and dispatcher.
//!
//! Implements the `$` prefix command set used by z/OS operators to control
//! JES2 subsystem resources — jobs, initiators, and the system itself.

use crate::job::{JobClass, JobId, JobState};
use crate::queue::Jes2;

// ---------------------------------------------------------------------------
// Command types
// ---------------------------------------------------------------------------

/// A parsed JES2 operator command.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Jes2Command {
    /// `$DA` — display active (running) jobs.
    DisplayActive,
    /// `$DJ JOBnnnnn` — display a specific job.
    DisplayJob(JobId),
    /// `$DQ` — display queue summary.
    DisplayQueue,

    /// `$S JOBnnnnn` — start (release) a held job.
    StartJob(JobId),
    /// `$SI n` — start initiator n.
    StartInitiator(u32),

    /// `$P JOBnnnnn` — purge a completed job.
    PurgeJob(JobId),
    /// `$PI n` — stop (drain) initiator n.
    StopInitiator(u32),
    /// `$PJES2` — orderly JES2 shutdown.
    StopJes2,

    /// `$A JOBnnnnn` — release a held job.
    ReleaseJob(JobId),

    /// `$C JOBnnnnn` — cancel a running job.
    CancelJob(JobId),

    /// `$H JOBnnnnn` — hold a job.
    HoldJob(JobId),
    /// `$HQ` — hold the entire queue (drain).
    HoldQueue,

    /// `$TI n,CLASSES=ABCD` — modify initiator classes.
    ModifyInitiator {
        id: u32,
        classes: Vec<char>,
    },

    /// `$LJ` — list all jobs.
    ListJobs,
    /// `$LI` — list initiators.
    ListInitiators,

    /// `$ZJES2` — halt JES2 immediately.
    HaltJes2,

    /// `$E JOBnnnnn` — restart a job.
    RestartJob(JobId),
}

// ---------------------------------------------------------------------------
// Initiator
// ---------------------------------------------------------------------------

/// A JES2 initiator that selects and runs jobs from the queue.
#[derive(Debug, Clone)]
pub struct Initiator {
    /// Initiator identifier (1-based).
    pub id: u32,
    /// Classes this initiator is configured to process.
    pub classes: Vec<JobClass>,
    /// Whether the initiator is active (started).
    pub active: bool,
    /// Job currently being processed (if any).
    pub current_job: Option<JobId>,
}

impl Initiator {
    /// Create a new initiator accepting the given classes.
    pub fn new(id: u32, classes: Vec<JobClass>) -> Self {
        Self {
            id,
            classes,
            active: false,
            current_job: None,
        }
    }
}

// ---------------------------------------------------------------------------
// Command response
// ---------------------------------------------------------------------------

/// Result of executing a JES2 command.
#[derive(Debug, Clone)]
pub struct CommandResponse {
    /// HASP message number (e.g. "HASP893").
    pub message_id: String,
    /// Response text lines.
    pub lines: Vec<String>,
    /// Whether the command succeeded.
    pub success: bool,
}

impl CommandResponse {
    fn ok(message_id: &str, lines: Vec<String>) -> Self {
        Self {
            message_id: message_id.to_string(),
            lines,
            success: true,
        }
    }

    fn err(message_id: &str, msg: &str) -> Self {
        Self {
            message_id: message_id.to_string(),
            lines: vec![msg.to_string()],
            success: false,
        }
    }
}

// ---------------------------------------------------------------------------
// Command parser
// ---------------------------------------------------------------------------

/// Parse a JES2 operator command string into a `Jes2Command`.
///
/// Commands start with `$` followed by a verb letter and optional operand.
pub fn parse_command(input: &str) -> Option<Jes2Command> {
    let input = input.trim();
    if !input.starts_with('$') {
        return None;
    }
    let rest = &input[1..];

    // Two-character commands with operands
    if let Some(operand) = rest.strip_prefix("DA") {
        if operand.is_empty() {
            return Some(Jes2Command::DisplayActive);
        }
    }
    if let Some(operand) = rest.strip_prefix("DQ") {
        if operand.is_empty() {
            return Some(Jes2Command::DisplayQueue);
        }
    }
    if let Some(operand) = rest.strip_prefix("DJ") {
        let operand = operand.trim();
        if let Some(id) = parse_job_operand(operand) {
            return Some(Jes2Command::DisplayJob(id));
        }
    }
    if let Some(operand) = rest.strip_prefix("LJ") {
        if operand.is_empty() {
            return Some(Jes2Command::ListJobs);
        }
    }
    if let Some(operand) = rest.strip_prefix("LI") {
        if operand.is_empty() {
            return Some(Jes2Command::ListInitiators);
        }
    }
    if let Some(operand) = rest.strip_prefix("HQ") {
        if operand.is_empty() {
            return Some(Jes2Command::HoldQueue);
        }
    }

    // Start initiator: $SI n
    if let Some(operand) = rest.strip_prefix("SI") {
        let operand = operand.trim();
        if let Ok(n) = operand.parse::<u32>() {
            return Some(Jes2Command::StartInitiator(n));
        }
    }

    // Stop initiator: $PI n
    if let Some(operand) = rest.strip_prefix("PI") {
        let operand = operand.trim();
        if let Ok(n) = operand.parse::<u32>() {
            return Some(Jes2Command::StopInitiator(n));
        }
    }

    // Modify initiator: $TI n,CLASSES=ABCD
    if let Some(operand) = rest.strip_prefix("TI") {
        let operand = operand.trim();
        return parse_modify_initiator(operand);
    }

    // JES2 system commands
    if rest.eq_ignore_ascii_case("PJES2") {
        return Some(Jes2Command::StopJes2);
    }
    if rest.eq_ignore_ascii_case("ZJES2") {
        return Some(Jes2Command::HaltJes2);
    }

    // Single-letter verb + job operand
    if rest.len() >= 2 {
        let verb = rest.as_bytes()[0];
        let operand = rest[1..].trim();

        match verb {
            b'S' => parse_job_operand(operand).map(Jes2Command::StartJob),
            b'P' => parse_job_operand(operand).map(Jes2Command::PurgeJob),
            b'A' => parse_job_operand(operand).map(Jes2Command::ReleaseJob),
            b'C' => parse_job_operand(operand).map(Jes2Command::CancelJob),
            b'H' => parse_job_operand(operand).map(Jes2Command::HoldJob),
            b'E' => parse_job_operand(operand).map(Jes2Command::RestartJob),
            _ => None,
        }
    } else {
        None
    }
}

/// Parse "JOBnnnnn" or just a number into a `JobId`.
fn parse_job_operand(s: &str) -> Option<JobId> {
    let s = s.trim();
    if let Some(num_str) = s.strip_prefix("JOB") {
        num_str.parse::<u32>().ok().map(JobId)
    } else {
        s.parse::<u32>().ok().map(JobId)
    }
}

/// Parse "$TI n,CLASSES=ABCD".
fn parse_modify_initiator(s: &str) -> Option<Jes2Command> {
    let parts: Vec<&str> = s.splitn(2, ',').collect();
    if parts.len() != 2 {
        return None;
    }
    let id = parts[0].trim().parse::<u32>().ok()?;
    let kv = parts[1].trim();
    let classes_str = kv.strip_prefix("CLASSES=")?;
    let classes: Vec<char> = classes_str.chars().filter(|c| c.is_alphanumeric()).collect();
    if classes.is_empty() {
        return None;
    }
    Some(Jes2Command::ModifyInitiator { id, classes })
}

// ---------------------------------------------------------------------------
// Command dispatcher
// ---------------------------------------------------------------------------

/// Execute a parsed command against the JES2 subsystem.
pub fn execute_command(
    jes: &mut Jes2,
    initiators: &mut [Initiator],
    cmd: &Jes2Command,
) -> CommandResponse {
    match cmd {
        Jes2Command::DisplayActive => display_active(jes),
        Jes2Command::DisplayJob(id) => display_job(jes, *id),
        Jes2Command::DisplayQueue => display_queue(jes),
        Jes2Command::StartJob(id) => start_job(jes, *id),
        Jes2Command::StartInitiator(id) => start_initiator(initiators, *id),
        Jes2Command::PurgeJob(id) => purge_job(jes, *id),
        Jes2Command::StopInitiator(id) => stop_initiator(initiators, *id),
        Jes2Command::StopJes2 => CommandResponse::ok("HASP085", vec!["JES2 TERMINATION IN PROGRESS".to_string()]),
        Jes2Command::ReleaseJob(id) => release_job(jes, *id),
        Jes2Command::CancelJob(id) => cancel_job(jes, *id),
        Jes2Command::HoldJob(id) => hold_job(jes, *id),
        Jes2Command::HoldQueue => CommandResponse::ok("HASP600", vec!["QUEUE HELD - NO NEW JOBS WILL BE SELECTED".to_string()]),
        Jes2Command::ModifyInitiator { id, classes } => modify_initiator(initiators, *id, classes),
        Jes2Command::ListJobs => list_jobs(jes),
        Jes2Command::ListInitiators => list_initiators(initiators),
        Jes2Command::HaltJes2 => CommandResponse::ok("HASP098", vec!["JES2 HALT - IMMEDIATE SHUTDOWN".to_string()]),
        Jes2Command::RestartJob(id) => restart_job(jes, *id),
    }
}

fn display_active(jes: &Jes2) -> CommandResponse {
    let running = jes.running_jobs();
    if running.is_empty() {
        return CommandResponse::ok("HASP893", vec!["NO ACTIVE JOBS".to_string()]);
    }
    let mut lines = Vec::new();
    for job in &running {
        lines.push(format!(
            "{} {} CLASS={} PRTY={}",
            job.id, job.name, job.class, job.priority
        ));
    }
    CommandResponse::ok("HASP893", lines)
}

fn display_job(jes: &Jes2, id: JobId) -> CommandResponse {
    match jes.get_job(id) {
        Some(job) => {
            let lines = vec![
                format!("{} {}", job.id, job.name),
                format!("  CLASS={} PRTY={} STATE={:?}", job.class, job.priority, job.state),
                format!("  OWNER={} MAXRC={}", job.owner, job.max_rc),
                format!("  SPOOL DATASETS: {}", job.spool_keys.len()),
            ];
            CommandResponse::ok("HASP890", lines)
        }
        None => CommandResponse::err("HASP889", &format!("JOB {} NOT FOUND", id)),
    }
}

fn display_queue(jes: &Jes2) -> CommandResponse {
    let input = jes.jobs_in_state(JobState::Input).len();
    let ready = jes.jobs_in_state(JobState::Ready).len();
    let running = jes.running_jobs().len();
    let output = jes.jobs_in_state(JobState::Output).len();
    let held = jes.held_jobs().len();
    let lines = vec![
        format!("INPUT={input} READY={ready} RUNNING={running} OUTPUT={output} HELD={held}"),
        format!("SPOOL DATASETS: {}", jes.spool.dataset_count()),
    ];
    CommandResponse::ok("HASP646", lines)
}

fn start_job(jes: &mut Jes2, id: JobId) -> CommandResponse {
    match jes.release(id) {
        Ok(()) => CommandResponse::ok("HASP280", vec![format!("{} RELEASED", id)]),
        Err(e) => CommandResponse::err("HASP281", &format!("{id}: {e}")),
    }
}

fn purge_job(jes: &mut Jes2, id: JobId) -> CommandResponse {
    match jes.purge(id) {
        Ok(()) => CommandResponse::ok("HASP250", vec![format!("{} PURGED", id)]),
        Err(e) => CommandResponse::err("HASP251", &format!("{id}: {e}")),
    }
}

fn release_job(jes: &mut Jes2, id: JobId) -> CommandResponse {
    match jes.release(id) {
        Ok(()) => CommandResponse::ok("HASP280", vec![format!("{} RELEASED", id)]),
        Err(e) => CommandResponse::err("HASP281", &format!("{id}: {e}")),
    }
}

fn cancel_job(jes: &mut Jes2, id: JobId) -> CommandResponse {
    match jes.cancel(id) {
        Ok(()) => CommandResponse::ok("HASP290", vec![format!("{} CANCELLED", id)]),
        Err(e) => CommandResponse::err("HASP291", &format!("{id}: {e}")),
    }
}

fn hold_job(jes: &mut Jes2, id: JobId) -> CommandResponse {
    match jes.hold(id) {
        Ok(()) => CommandResponse::ok("HASP270", vec![format!("{} HELD", id)]),
        Err(e) => CommandResponse::err("HASP271", &format!("{id}: {e}")),
    }
}

fn restart_job(jes: &mut Jes2, id: JobId) -> CommandResponse {
    // Restart = cancel current execution and re-queue as Ready
    let job = match jes.get_job_mut(id) {
        Some(j) => j,
        None => return CommandResponse::err("HASP301", &format!("JOB {id} NOT FOUND")),
    };
    match job.state {
        JobState::Running => {
            job.state = JobState::Ready;
            job.max_rc = 0;
            CommandResponse::ok("HASP300", vec![format!("{} RESTARTED", id)])
        }
        _ => CommandResponse::err("HASP301", &format!("{id}: NOT RUNNING")),
    }
}

fn start_initiator(initiators: &mut [Initiator], id: u32) -> CommandResponse {
    if let Some(init) = initiators.iter_mut().find(|i| i.id == id) {
        init.active = true;
        CommandResponse::ok("HASP410", vec![format!("INITIATOR {id} STARTED")])
    } else {
        CommandResponse::err("HASP411", &format!("INITIATOR {id} NOT DEFINED"))
    }
}

fn stop_initiator(initiators: &mut [Initiator], id: u32) -> CommandResponse {
    if let Some(init) = initiators.iter_mut().find(|i| i.id == id) {
        init.active = false;
        CommandResponse::ok("HASP420", vec![format!("INITIATOR {id} DRAINED")])
    } else {
        CommandResponse::err("HASP421", &format!("INITIATOR {id} NOT DEFINED"))
    }
}

fn modify_initiator(initiators: &mut [Initiator], id: u32, classes: &[char]) -> CommandResponse {
    if let Some(init) = initiators.iter_mut().find(|i| i.id == id) {
        init.classes = classes
            .iter()
            .filter_map(|&c| JobClass::standard(c))
            .collect();
        let class_str: String = classes.iter().collect();
        CommandResponse::ok("HASP430", vec![format!("INITIATOR {id} CLASSES={class_str}")])
    } else {
        CommandResponse::err("HASP431", &format!("INITIATOR {id} NOT DEFINED"))
    }
}

fn list_jobs(jes: &Jes2) -> CommandResponse {
    let jobs = jes.active_jobs();
    if jobs.is_empty() {
        return CommandResponse::ok("HASP646", vec!["NO JOBS".to_string()]);
    }
    let mut lines = Vec::new();
    for job in &jobs {
        lines.push(format!(
            "{} {} CLASS={} PRTY={} {:?}",
            job.id, job.name, job.class, job.priority, job.state
        ));
    }
    CommandResponse::ok("HASP646", lines)
}

fn list_initiators(initiators: &[Initiator]) -> CommandResponse {
    if initiators.is_empty() {
        return CommandResponse::ok("HASP440", vec!["NO INITIATORS DEFINED".to_string()]);
    }
    let mut lines = Vec::new();
    for init in initiators {
        let status = if init.active { "ACTIVE" } else { "DRAINED" };
        let classes: String = init.classes.iter().map(|c| c.to_string()).collect::<Vec<_>>().join(",");
        let job_str = match init.current_job {
            Some(id) => format!("JOB={id}"),
            None => "NO JOB".to_string(),
        };
        lines.push(format!("INIT{} {status} CLASSES={classes} {job_str}", init.id));
    }
    CommandResponse::ok("HASP440", lines)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_display_active() {
        assert_eq!(parse_command("$DA"), Some(Jes2Command::DisplayActive));
    }

    #[test]
    fn parse_display_job() {
        assert_eq!(
            parse_command("$DJ JOB00123"),
            Some(Jes2Command::DisplayJob(JobId(123)))
        );
        assert_eq!(
            parse_command("$DJ 42"),
            Some(Jes2Command::DisplayJob(JobId(42)))
        );
    }

    #[test]
    fn parse_display_queue() {
        assert_eq!(parse_command("$DQ"), Some(Jes2Command::DisplayQueue));
    }

    #[test]
    fn parse_hold_release_cancel() {
        assert_eq!(
            parse_command("$H JOB00001"),
            Some(Jes2Command::HoldJob(JobId(1)))
        );
        assert_eq!(
            parse_command("$A JOB00001"),
            Some(Jes2Command::ReleaseJob(JobId(1)))
        );
        assert_eq!(
            parse_command("$C JOB00001"),
            Some(Jes2Command::CancelJob(JobId(1)))
        );
    }

    #[test]
    fn parse_purge() {
        assert_eq!(
            parse_command("$P JOB00005"),
            Some(Jes2Command::PurgeJob(JobId(5)))
        );
    }

    #[test]
    fn parse_system_commands() {
        assert_eq!(parse_command("$PJES2"), Some(Jes2Command::StopJes2));
        assert_eq!(parse_command("$ZJES2"), Some(Jes2Command::HaltJes2));
    }

    #[test]
    fn parse_initiator_commands() {
        assert_eq!(
            parse_command("$SI 1"),
            Some(Jes2Command::StartInitiator(1))
        );
        assert_eq!(
            parse_command("$PI 2"),
            Some(Jes2Command::StopInitiator(2))
        );
    }

    #[test]
    fn parse_modify_initiator() {
        assert_eq!(
            parse_command("$TI 1,CLASSES=ABCD"),
            Some(Jes2Command::ModifyInitiator {
                id: 1,
                classes: vec!['A', 'B', 'C', 'D'],
            })
        );
    }

    #[test]
    fn parse_list_commands() {
        assert_eq!(parse_command("$LJ"), Some(Jes2Command::ListJobs));
        assert_eq!(parse_command("$LI"), Some(Jes2Command::ListInitiators));
    }

    #[test]
    fn parse_restart() {
        assert_eq!(
            parse_command("$E JOB00010"),
            Some(Jes2Command::RestartJob(JobId(10)))
        );
    }

    #[test]
    fn parse_invalid() {
        assert!(parse_command("not a command").is_none());
        assert!(parse_command("$").is_none());
        assert!(parse_command("$X").is_none());
    }

    #[test]
    fn execute_display_active_empty() {
        let mut jes = Jes2::new();
        let mut inits = Vec::new();
        let resp = execute_command(&mut jes, &mut inits, &Jes2Command::DisplayActive);
        assert!(resp.success);
        assert_eq!(resp.lines[0], "NO ACTIVE JOBS");
    }

    #[test]
    fn execute_display_active_with_jobs() {
        let mut jes = Jes2::new();
        let id = jes.submit("RUN1", 'A', 10, false);
        jes.advance(id).unwrap();
        jes.advance(id).unwrap();
        jes.advance(id).unwrap(); // Running

        let mut inits = Vec::new();
        let resp = execute_command(&mut jes, &mut inits, &Jes2Command::DisplayActive);
        assert!(resp.success);
        assert_eq!(resp.lines.len(), 1);
        assert!(resp.lines[0].contains("RUN1"));
    }

    #[test]
    fn execute_display_job() {
        let mut jes = Jes2::new();
        let id = jes.submit("TEST", 'B', 5, false);
        let mut inits = Vec::new();

        let resp = execute_command(&mut jes, &mut inits, &Jes2Command::DisplayJob(id));
        assert!(resp.success);
        assert!(resp.lines[0].contains("TEST"));

        // Nonexistent
        let resp = execute_command(&mut jes, &mut inits, &Jes2Command::DisplayJob(JobId(999)));
        assert!(!resp.success);
    }

    #[test]
    fn execute_hold_release_cancel() {
        let mut jes = Jes2::new();
        let id = jes.submit("WORK", 'A', 8, false);
        jes.advance(id).unwrap(); // Conversion
        jes.advance(id).unwrap(); // Ready
        let mut inits = Vec::new();

        // Hold
        let resp = execute_command(&mut jes, &mut inits, &Jes2Command::HoldJob(id));
        assert!(resp.success);
        assert!(jes.get_job(id).unwrap().state.is_held());

        // Release
        let resp = execute_command(&mut jes, &mut inits, &Jes2Command::ReleaseJob(id));
        assert!(resp.success);
        assert_eq!(jes.get_job(id).unwrap().state, JobState::Ready);

        // Advance to Running and cancel
        jes.advance(id).unwrap();
        let resp = execute_command(&mut jes, &mut inits, &Jes2Command::CancelJob(id));
        assert!(resp.success);
        assert_eq!(jes.get_job(id).unwrap().state, JobState::Cancelled);
    }

    #[test]
    fn execute_purge() {
        let mut jes = Jes2::new();
        let id = jes.submit("DONE", 'A', 5, false);
        // Drive to Output
        for _ in 0..4 {
            jes.advance(id).unwrap();
        }
        let mut inits = Vec::new();

        let resp = execute_command(&mut jes, &mut inits, &Jes2Command::PurgeJob(id));
        assert!(resp.success);
        assert!(resp.lines[0].contains("PURGED"));
    }

    #[test]
    fn execute_initiator_lifecycle() {
        let mut jes = Jes2::new();
        let mut inits = vec![
            Initiator::new(1, vec![JobClass::Standard('A')]),
            Initiator::new(2, vec![JobClass::Standard('B')]),
        ];

        // Start
        let resp = execute_command(&mut jes, &mut inits, &Jes2Command::StartInitiator(1));
        assert!(resp.success);
        assert!(inits[0].active);

        // Modify classes
        let resp = execute_command(
            &mut jes,
            &mut inits,
            &Jes2Command::ModifyInitiator {
                id: 1,
                classes: vec!['A', 'B', 'C', 'D'],
            },
        );
        assert!(resp.success);
        assert_eq!(inits[0].classes.len(), 4);

        // Stop
        let resp = execute_command(&mut jes, &mut inits, &Jes2Command::StopInitiator(1));
        assert!(resp.success);
        assert!(!inits[0].active);

        // Undefined initiator
        let resp = execute_command(&mut jes, &mut inits, &Jes2Command::StartInitiator(99));
        assert!(!resp.success);
    }

    #[test]
    fn execute_list_jobs() {
        let mut jes = Jes2::new();
        jes.submit("JOB1", 'A', 5, false);
        jes.submit("JOB2", 'B', 10, false);
        let mut inits = Vec::new();

        let resp = execute_command(&mut jes, &mut inits, &Jes2Command::ListJobs);
        assert!(resp.success);
        assert_eq!(resp.lines.len(), 2);
    }

    #[test]
    fn execute_list_initiators() {
        let mut jes = Jes2::new();
        let mut inits = vec![
            Initiator::new(1, vec![JobClass::Standard('A')]),
        ];
        inits[0].active = true;

        let resp = execute_command(&mut jes, &mut inits, &Jes2Command::ListInitiators);
        assert!(resp.success);
        assert!(resp.lines[0].contains("ACTIVE"));
    }

    #[test]
    fn execute_restart_running_job() {
        let mut jes = Jes2::new();
        let id = jes.submit("RESTART", 'A', 5, false);
        jes.advance(id).unwrap(); // Conversion
        jes.advance(id).unwrap(); // Ready
        jes.advance(id).unwrap(); // Running
        let mut inits = Vec::new();

        let resp = execute_command(&mut jes, &mut inits, &Jes2Command::RestartJob(id));
        assert!(resp.success);
        assert_eq!(jes.get_job(id).unwrap().state, JobState::Ready);
    }

    #[test]
    fn execute_restart_non_running_fails() {
        let mut jes = Jes2::new();
        let id = jes.submit("NOTRUN", 'A', 5, false);
        let mut inits = Vec::new();

        let resp = execute_command(&mut jes, &mut inits, &Jes2Command::RestartJob(id));
        assert!(!resp.success);
    }

    #[test]
    fn execute_display_queue() {
        let mut jes = Jes2::new();
        jes.submit("J1", 'A', 5, false);
        jes.submit("J2", 'A', 5, true); // held
        let mut inits = Vec::new();

        let resp = execute_command(&mut jes, &mut inits, &Jes2Command::DisplayQueue);
        assert!(resp.success);
        assert!(resp.lines[0].contains("INPUT=1"));
        assert!(resp.lines[0].contains("HELD=1"));
    }
}
