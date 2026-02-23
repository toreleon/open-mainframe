//! System command dispatcher, parser, and implementations.

use std::collections::HashMap;

// ─────────────────────── Command Types ───────────────────────

/// A parsed system command.
#[derive(Debug, Clone, PartialEq)]
pub enum SystemCommand {
    /// DISPLAY A,L — list active address spaces.
    DisplayActive { filter: Option<String> },
    /// DISPLAY J,jobname — job information.
    DisplayJob { jobname: String },
    /// DISPLAY T — date and time.
    DisplayTime,
    /// DISPLAY M — memory/storage information.
    DisplayMemory,
    /// START — start an address space.
    Start(StartParams),
    /// STOP/P — stop an address space.
    Stop { name: String },
    /// MODIFY/F — send modify command.
    Modify(ModifyParams),
    /// CANCEL — cancel a job.
    Cancel(CancelOptions),
    /// FORCE — force-terminate an address space.
    Force { name: String },
    /// REPLY — reply to a WTOR.
    Reply { id: u32, text: String },
    /// JES2 command (prefixed with $).
    Jes2 { verb: String, operand: String },
    /// Unknown command.
    Unknown(String),
}

/// START command parameters.
#[derive(Debug, Clone, PartialEq)]
pub struct StartParams {
    /// Program/procedure name.
    pub name: String,
    /// Parameter string.
    pub parm: Option<String>,
}

/// MODIFY command parameters.
#[derive(Debug, Clone, PartialEq)]
pub struct ModifyParams {
    /// Target address space name.
    pub name: String,
    /// Modify text.
    pub text: String,
}

/// CANCEL command options.
#[derive(Debug, Clone, PartialEq)]
pub struct CancelOptions {
    /// Job name to cancel.
    pub name: String,
    /// Whether to produce a dump.
    pub dump: bool,
}

// ─────────────────────── Command Parser ───────────────────────

/// Parse a system command string into a SystemCommand.
pub fn parse_command(input: &str) -> SystemCommand {
    let input = input.trim();
    if input.is_empty() {
        return SystemCommand::Unknown(String::new());
    }

    // JES2 commands start with $
    if input.starts_with('$') {
        return parse_jes2_command(input);
    }

    let upper = input.to_uppercase();
    let parts: Vec<&str> = upper.splitn(2, ' ').collect();
    let cmd = parts[0];
    let rest = if parts.len() > 1 { parts[1].trim() } else { "" };

    match cmd {
        "D" | "DISPLAY" => parse_display(rest),
        "S" | "START" => parse_start(rest, input),
        "P" | "STOP" => SystemCommand::Stop {
            name: rest.to_string(),
        },
        "F" | "MODIFY" => parse_modify(rest),
        "C" | "CANCEL" => parse_cancel(rest),
        "FORCE" => parse_force(rest),
        "R" | "REPLY" => parse_reply(rest, input),
        _ => SystemCommand::Unknown(input.to_string()),
    }
}

fn parse_display(rest: &str) -> SystemCommand {
    let parts: Vec<&str> = rest.splitn(2, ',').collect();
    let sub = parts[0].trim();

    match sub {
        "A" => {
            let filter = if parts.len() > 1 {
                let opts = parts[1].trim();
                if opts == "L" {
                    None
                } else {
                    Some(opts.to_string())
                }
            } else {
                None
            };
            SystemCommand::DisplayActive { filter }
        }
        "J" => {
            let jobname = if parts.len() > 1 {
                parts[1].trim().to_string()
            } else {
                String::new()
            };
            SystemCommand::DisplayJob { jobname }
        }
        "T" => SystemCommand::DisplayTime,
        "M" => SystemCommand::DisplayMemory,
        _ => {
            // D A,L,jobname pattern
            if let Some(filter_part) = sub.strip_prefix("A,") {
                if filter_part == "L" {
                    SystemCommand::DisplayActive { filter: None }
                } else {
                    SystemCommand::DisplayActive {
                        filter: Some(filter_part.to_string()),
                    }
                }
            } else {
                SystemCommand::Unknown(format!("DISPLAY {rest}"))
            }
        }
    }
}

fn parse_start(rest: &str, original: &str) -> SystemCommand {
    // S MYPROG,PARM='DATA'
    let name_end = rest.find(',').unwrap_or(rest.len());
    let name = rest[..name_end].trim().to_string();

    // Extract PARM= from original (preserve case)
    let parm = if let Some(parm_pos) = original.to_uppercase().find("PARM=") {
        let after = &original[parm_pos + 5..];
        let value = if let Some(stripped) = after.strip_prefix('\'') {
            // Quoted value
            let end = stripped.find('\'').unwrap_or(stripped.len());
            stripped[..end].to_string()
        } else {
            after.split(',').next().unwrap_or("").to_string()
        };
        Some(value)
    } else {
        None
    };

    SystemCommand::Start(StartParams { name, parm })
}

fn parse_modify(rest: &str) -> SystemCommand {
    let comma_pos = rest.find(',').unwrap_or(rest.len());
    let name = rest[..comma_pos].trim().to_string();
    let text = if comma_pos < rest.len() {
        rest[comma_pos + 1..].trim().to_string()
    } else {
        String::new()
    };
    SystemCommand::Modify(ModifyParams { name, text })
}

fn parse_cancel(rest: &str) -> SystemCommand {
    let dump = rest.contains("DUMP");
    let name = rest
        .split(',')
        .next()
        .unwrap_or("")
        .trim()
        .to_string();
    SystemCommand::Cancel(CancelOptions { name, dump })
}

fn parse_force(rest: &str) -> SystemCommand {
    let name = rest
        .split(',')
        .next()
        .unwrap_or("")
        .trim()
        .to_string();
    SystemCommand::Force { name }
}

fn parse_reply(rest: &str, original: &str) -> SystemCommand {
    // R 05,'YES' or R 05,YES
    let comma_pos = rest.find(',').unwrap_or(rest.len());
    let id_str = rest[..comma_pos].trim();
    let id: u32 = id_str.parse().unwrap_or(0);

    // Get reply text from original (preserve case)
    let text = if comma_pos < rest.len() {
        let orig_upper = original.to_uppercase();
        let reply_start = orig_upper.find(&rest[comma_pos..]).unwrap_or(0);
        let raw = &original[reply_start + 1..];
        let trimmed = raw.trim();
        if trimmed.starts_with('\'') && trimmed.ends_with('\'') {
            trimmed[1..trimmed.len() - 1].to_string()
        } else {
            trimmed.to_string()
        }
    } else {
        String::new()
    };

    SystemCommand::Reply { id, text }
}

fn parse_jes2_command(input: &str) -> SystemCommand {
    // $DA, $DJ(12345), $SA, $PA, etc.
    let without_dollar = &input[1..];
    if without_dollar.is_empty() {
        return SystemCommand::Unknown(input.to_string());
    }

    let verb = without_dollar[..1].to_uppercase();
    let operand = if without_dollar.len() > 1 {
        without_dollar[1..].to_uppercase()
    } else {
        String::new()
    };

    SystemCommand::Jes2 { verb, operand }
}

// ─────────────────────── Command Registry ───────────────────────

/// Handler function type for registered commands.
pub type CommandHandler = Box<dyn Fn(&SystemCommand, &SystemState) -> CommandOutput>;

/// Registry of command handlers.
pub struct CommandRegistry {
    handlers: HashMap<String, CommandHandler>,
}

impl std::fmt::Debug for CommandRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CommandRegistry")
            .field("handler_count", &self.handlers.len())
            .finish()
    }
}

impl CommandRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self {
            handlers: HashMap::new(),
        }
    }

    /// Register a command handler.
    pub fn register(&mut self, name: &str, handler: CommandHandler) {
        self.handlers.insert(name.to_uppercase(), handler);
    }

    /// Check if a command is registered.
    pub fn is_registered(&self, name: &str) -> bool {
        self.handlers.contains_key(&name.to_uppercase())
    }

    /// Number of registered commands.
    pub fn count(&self) -> usize {
        self.handlers.len()
    }
}

impl Default for CommandRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// ─────────────────────── Command Output ───────────────────────

/// Output from a command execution.
#[derive(Debug, Clone)]
pub struct CommandOutput {
    /// Message lines.
    pub messages: Vec<String>,
    /// Whether the command succeeded.
    pub success: bool,
    /// Return code (0 = success).
    pub rc: u32,
}

impl CommandOutput {
    /// Create a success output.
    pub fn ok(messages: Vec<String>) -> Self {
        Self {
            messages,
            success: true,
            rc: 0,
        }
    }

    /// Create an error output.
    pub fn error(message: &str) -> Self {
        Self {
            messages: vec![message.to_string()],
            success: false,
            rc: 4,
        }
    }
}

// ─────────────────────── System State ───────────────────────

/// Address space status.
#[derive(Debug, Clone, PartialEq)]
pub enum AsidStatus {
    /// Address space is active (IN = swapped in).
    In,
    /// Address space is swapped out.
    Out,
    /// Address space is starting.
    Starting,
    /// Address space is stopping.
    Stopping,
}

impl AsidStatus {
    /// Display string.
    pub fn as_str(&self) -> &'static str {
        match self {
            AsidStatus::In => "IN",
            AsidStatus::Out => "OUT",
            AsidStatus::Starting => "STARTING",
            AsidStatus::Stopping => "STOPPING",
        }
    }
}

/// An active address space.
#[derive(Debug, Clone)]
pub struct AddressSpace {
    /// Job name.
    pub jobname: String,
    /// Address Space ID.
    pub asid: u16,
    /// Status.
    pub status: AsidStatus,
    /// Current step name.
    pub step_name: String,
    /// Current program name.
    pub program: String,
    /// CPU time in seconds.
    pub cpu_time: f64,
    /// Initiator name (for batch jobs).
    pub initiator: Option<String>,
}

/// A WTOR (Write To Operator with Reply) outstanding message.
#[derive(Debug, Clone)]
pub struct WtorEntry {
    /// Reply ID.
    pub id: u32,
    /// Message text.
    pub message: String,
    /// Requesting jobname.
    pub jobname: String,
    /// Whether reply has been provided.
    pub replied: bool,
    /// Reply text (once provided).
    pub reply_text: Option<String>,
}

/// Memory/storage information.
#[derive(Debug, Clone)]
pub struct MemoryInfo {
    /// Storage areas.
    pub areas: Vec<StorageArea>,
}

/// A named storage area.
#[derive(Debug, Clone)]
pub struct StorageArea {
    /// Area name (CSA, SQA, REAL, AUX, etc.).
    pub name: String,
    /// Total size in KB.
    pub total_kb: u64,
    /// Used size in KB.
    pub used_kb: u64,
}

/// Job information for DISPLAY J.
#[derive(Debug, Clone)]
pub struct JobInfo {
    /// Job name.
    pub jobname: String,
    /// Current step.
    pub step: String,
    /// Current program.
    pub program: String,
    /// CPU time.
    pub cpu_time: f64,
    /// Initiator.
    pub initiator: String,
    /// Job status.
    pub status: String,
}

/// System date/time for DISPLAY T.
#[derive(Debug, Clone)]
pub struct SystemTime {
    /// Date string.
    pub date: String,
    /// Time string.
    pub time: String,
    /// IPL date.
    pub ipl_date: String,
    /// IPL volume.
    pub ipl_volume: String,
}

/// Result of DISPLAY commands.
#[derive(Debug, Clone)]
pub enum DisplayResult {
    /// Active address spaces.
    ActiveSpaces(Vec<AddressSpace>),
    /// Job info.
    Job(Option<JobInfo>),
    /// System time.
    Time(SystemTime),
    /// Memory info.
    Memory(MemoryInfo),
}

/// Result of a REPLY command.
#[derive(Debug, Clone)]
pub struct ReplyResult {
    /// Whether the reply was delivered.
    pub delivered: bool,
    /// Reply ID.
    pub id: u32,
    /// Message.
    pub message: String,
}

// ─────────────────────── System State Container ───────────────────────

/// System state for command execution.
#[derive(Debug)]
pub struct SystemState {
    /// Active address spaces.
    pub address_spaces: Vec<AddressSpace>,
    /// Outstanding WTORs.
    pub wtors: Vec<WtorEntry>,
    /// Memory info.
    pub memory: MemoryInfo,
    /// System time.
    pub time: SystemTime,
    /// JES2 jobs (jobname → info).
    pub jes2_jobs: HashMap<String, JobInfo>,
}

impl SystemState {
    /// Create a new system state.
    pub fn new() -> Self {
        Self {
            address_spaces: Vec::new(),
            wtors: Vec::new(),
            memory: MemoryInfo {
                areas: vec![
                    StorageArea { name: "REAL".into(), total_kb: 8_388_608, used_kb: 4_000_000 },
                    StorageArea { name: "AUX".into(), total_kb: 16_777_216, used_kb: 2_000_000 },
                    StorageArea { name: "CSA".into(), total_kb: 65_536, used_kb: 32_000 },
                    StorageArea { name: "SQA".into(), total_kb: 32_768, used_kb: 16_000 },
                ],
            },
            time: SystemTime {
                date: "2026.054".into(),
                time: "12.00.00".into(),
                ipl_date: "2026.001".into(),
                ipl_volume: "SYSRES".into(),
            },
            jes2_jobs: HashMap::new(),
        }
    }
}

impl Default for SystemState {
    fn default() -> Self {
        Self::new()
    }
}

// ─────────────────────── Command Dispatcher ───────────────────────

/// The command dispatcher — parses and executes system commands.
#[derive(Debug)]
pub struct CommandDispatcher {
    /// Registered custom command handlers.
    pub registry: CommandRegistry,
    /// System state.
    pub state: SystemState,
}

impl CommandDispatcher {
    /// Create a new command dispatcher.
    pub fn new() -> Self {
        Self {
            registry: CommandRegistry::new(),
            state: SystemState::new(),
        }
    }

    /// Execute a command string.
    pub fn execute(&mut self, input: &str) -> CommandOutput {
        let cmd = parse_command(input);
        self.dispatch(&cmd)
    }

    /// Dispatch a parsed command.
    pub fn dispatch(&mut self, cmd: &SystemCommand) -> CommandOutput {
        match cmd {
            SystemCommand::DisplayActive { filter } => self.display_active(filter.as_deref()),
            SystemCommand::DisplayJob { jobname } => self.display_job(jobname),
            SystemCommand::DisplayTime => self.display_time(),
            SystemCommand::DisplayMemory => self.display_memory(),
            SystemCommand::Start(params) => self.start(params),
            SystemCommand::Stop { name } => self.stop(name),
            SystemCommand::Modify(params) => self.modify(params),
            SystemCommand::Cancel(opts) => self.cancel(opts),
            SystemCommand::Force { name } => self.force(name),
            SystemCommand::Reply { id, text } => self.reply(*id, text),
            SystemCommand::Jes2 { verb, operand } => self.jes2_command(verb, operand),
            SystemCommand::Unknown(text) => CommandOutput::error(&format!(
                "IEE305I COMMAND '{}' INVALID",
                text
            )),
        }
    }

    /// Add an address space to the system state.
    pub fn add_address_space(&mut self, aspace: AddressSpace) {
        self.state.address_spaces.push(aspace);
    }

    /// Add a WTOR to the system state.
    pub fn add_wtor(&mut self, entry: WtorEntry) {
        self.state.wtors.push(entry);
    }

    /// DISPLAY A,L — active address spaces.
    fn display_active(&self, filter: Option<&str>) -> CommandOutput {
        let spaces: Vec<&AddressSpace> = if let Some(f) = filter {
            self.state
                .address_spaces
                .iter()
                .filter(|a| a.jobname.contains(f))
                .collect()
        } else {
            self.state.address_spaces.iter().collect()
        };

        let mut msgs = Vec::new();
        msgs.push("IEE114I ACTIVE ADDRESS SPACE LIST".to_string());
        msgs.push("  JOBNAME  ASID  STATUS    STEP      PROGRAM".to_string());

        for a in &spaces {
            msgs.push(format!(
                "  {:<8} {:04X}  {:<9} {:<9} {}",
                a.jobname, a.asid, a.status.as_str(), a.step_name, a.program
            ));
        }
        msgs.push(format!("  {} ADDRESS SPACES ACTIVE", spaces.len()));

        CommandOutput::ok(msgs)
    }

    /// DISPLAY J — job information.
    fn display_job(&self, jobname: &str) -> CommandOutput {
        let aspace = self.state.address_spaces.iter().find(|a| a.jobname == jobname);
        if let Some(a) = aspace {
            let msgs = vec![
                format!("IEE115I JOB {}", a.jobname),
                format!("  STEP: {}", a.step_name),
                format!("  PROGRAM: {}", a.program),
                format!("  CPU TIME: {:.2} SECONDS", a.cpu_time),
                format!(
                    "  INITIATOR: {}",
                    a.initiator.as_deref().unwrap_or("N/A")
                ),
                format!("  STATUS: {}", a.status.as_str()),
            ];
            CommandOutput::ok(msgs)
        } else {
            CommandOutput::error(&format!("IEE341I JOB {} NOT FOUND", jobname))
        }
    }

    /// DISPLAY T — date and time.
    fn display_time(&self) -> CommandOutput {
        let t = &self.state.time;
        let msgs = vec![
            format!(
                "IEE136I LOCAL: DATE={} TIME={} IPL DATE={} VOLUME={}",
                t.date, t.time, t.ipl_date, t.ipl_volume
            ),
        ];
        CommandOutput::ok(msgs)
    }

    /// DISPLAY M — memory/storage.
    fn display_memory(&self) -> CommandOutput {
        let mut msgs = Vec::new();
        msgs.push("IEE174I STORAGE SUMMARY".to_string());
        for area in &self.state.memory.areas {
            let pct = if area.total_kb > 0 {
                (area.used_kb as f64 / area.total_kb as f64 * 100.0) as u32
            } else {
                0
            };
            msgs.push(format!(
                "  {:<6} TOTAL={:>10}K  USED={:>10}K  ({}%)",
                area.name, area.total_kb, area.used_kb, pct
            ));
        }
        CommandOutput::ok(msgs)
    }

    /// START command.
    fn start(&mut self, params: &StartParams) -> CommandOutput {
        let asid = (self.state.address_spaces.len() as u16) + 1;
        let aspace = AddressSpace {
            jobname: params.name.clone(),
            asid,
            status: AsidStatus::Starting,
            step_name: "START".into(),
            program: params.name.clone(),
            cpu_time: 0.0,
            initiator: None,
        };
        self.state.address_spaces.push(aspace);

        let msg = if let Some(parm) = &params.parm {
            format!("IEE122I {} STARTED, PARM='{}'", params.name, parm)
        } else {
            format!("IEE122I {} STARTED", params.name)
        };
        CommandOutput::ok(vec![msg])
    }

    /// STOP command.
    fn stop(&mut self, name: &str) -> CommandOutput {
        if let Some(a) = self.state.address_spaces.iter_mut().find(|a| a.jobname == name) {
            a.status = AsidStatus::Stopping;
            CommandOutput::ok(vec![format!("IEE140I {} STOPPING", name)])
        } else {
            CommandOutput::error(&format!("IEE341I {} NOT FOUND", name))
        }
    }

    /// MODIFY command.
    fn modify(&self, params: &ModifyParams) -> CommandOutput {
        let found = self
            .state
            .address_spaces
            .iter()
            .any(|a| a.jobname == params.name);
        if found {
            CommandOutput::ok(vec![format!(
                "IEE295I MODIFY COMMAND ACCEPTED FOR {}",
                params.name
            )])
        } else {
            CommandOutput::error(&format!("IEE341I {} NOT FOUND", params.name))
        }
    }

    /// CANCEL command.
    fn cancel(&mut self, opts: &CancelOptions) -> CommandOutput {
        if let Some(a) = self
            .state
            .address_spaces
            .iter_mut()
            .find(|a| a.jobname == opts.name)
        {
            a.status = AsidStatus::Stopping;
            let msg = if opts.dump {
                format!("IEE301I {} CANCEL WITH DUMP", opts.name)
            } else {
                format!("IEE301I {} CANCEL COMMAND ACCEPTED", opts.name)
            };
            CommandOutput::ok(vec![msg])
        } else {
            CommandOutput::error(&format!("IEE341I {} NOT FOUND", opts.name))
        }
    }

    /// FORCE command.
    fn force(&mut self, name: &str) -> CommandOutput {
        let idx = self
            .state
            .address_spaces
            .iter()
            .position(|a| a.jobname == name);
        if let Some(idx) = idx {
            self.state.address_spaces.remove(idx);
            CommandOutput::ok(vec![format!("IEE334I {} FORCE COMPLETE", name)])
        } else {
            CommandOutput::error(&format!("IEE341I {} NOT FOUND", name))
        }
    }

    /// REPLY command.
    fn reply(&mut self, id: u32, text: &str) -> CommandOutput {
        if let Some(wtor) = self.state.wtors.iter_mut().find(|w| w.id == id && !w.replied) {
            wtor.replied = true;
            wtor.reply_text = Some(text.to_string());
            CommandOutput::ok(vec![format!("IEE600I REPLY TO {:02} IS '{}'", id, text)])
        } else {
            CommandOutput::error(&format!("IEE064I REPLY ID {:02} NOT VALID", id))
        }
    }

    /// JES2 command routing.
    fn jes2_command(&self, verb: &str, operand: &str) -> CommandOutput {
        match verb {
            "D" => {
                // $DA — display active
                if operand.starts_with('A') {
                    let mut msgs = vec!["$HASP646 ACTIVE JOBS".to_string()];
                    for a in &self.state.address_spaces {
                        msgs.push(format!("  {} ASID={:04X} {}", a.jobname, a.asid, a.status.as_str()));
                    }
                    CommandOutput::ok(msgs)
                } else if operand.starts_with("J(") || operand.starts_with('J') {
                    // $DJ(12345) or $DJ,jobname
                    let job_ref = operand
                        .trim_start_matches("J(")
                        .trim_start_matches('J')
                        .trim_end_matches(')')
                        .trim_start_matches(',');
                    if let Some(job) = self.state.jes2_jobs.get(job_ref) {
                        CommandOutput::ok(vec![
                            format!("$HASP890 JOB({})", job.jobname),
                            format!("  STATUS={} STEP={} PROGRAM={}", job.status, job.step, job.program),
                        ])
                    } else if let Some(a) = self.state.address_spaces.iter().find(|a| a.jobname == job_ref) {
                        CommandOutput::ok(vec![
                            format!("$HASP890 JOB({})", a.jobname),
                            format!("  STATUS={} STEP={} PROGRAM={}", a.status.as_str(), a.step_name, a.program),
                        ])
                    } else {
                        CommandOutput::error(&format!("$HASP891 JOB {} NOT FOUND", job_ref))
                    }
                } else {
                    CommandOutput::ok(vec![format!("$HASP646 DISPLAY {operand}")])
                }
            }
            "S" => CommandOutput::ok(vec![format!("$HASP600 START {operand} COMMAND ACCEPTED")]),
            "P" => CommandOutput::ok(vec![format!("$HASP601 STOP {operand} COMMAND ACCEPTED")]),
            "C" => CommandOutput::ok(vec![format!("$HASP602 CANCEL {operand} COMMAND ACCEPTED")]),
            "A" => CommandOutput::ok(vec![format!("$HASP603 ACTIVATE {operand} COMMAND ACCEPTED")]),
            "T" => CommandOutput::ok(vec![format!("$HASP604 SET {operand} COMMAND ACCEPTED")]),
            _ => CommandOutput::error(&format!("$HASP605 ${verb}{operand} INVALID")),
        }
    }
}

impl Default for CommandDispatcher {
    fn default() -> Self {
        Self::new()
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn setup_dispatcher() -> CommandDispatcher {
        let mut disp = CommandDispatcher::new();
        disp.add_address_space(AddressSpace {
            jobname: "PAYROLL".into(),
            asid: 0x0010,
            status: AsidStatus::In,
            step_name: "STEP1".into(),
            program: "PAYPROG".into(),
            cpu_time: 12.5,
            initiator: Some("INIT1".into()),
        });
        disp.add_address_space(AddressSpace {
            jobname: "BACKUP".into(),
            asid: 0x0020,
            status: AsidStatus::In,
            step_name: "BACKUP01".into(),
            program: "IDCAMS".into(),
            cpu_time: 5.0,
            initiator: Some("INIT2".into()),
        });
        disp.add_address_space(AddressSpace {
            jobname: "CICS".into(),
            asid: 0x0030,
            status: AsidStatus::In,
            step_name: "CICS".into(),
            program: "DFHSIP".into(),
            cpu_time: 1000.0,
            initiator: None,
        });
        disp.add_wtor(WtorEntry {
            id: 5,
            message: "IEC501A MOUNT TAPE ON 0A80".into(),
            jobname: "BACKUP".into(),
            replied: false,
            reply_text: None,
        });
        disp
    }

    // ─── SYS-102.1: Command Dispatcher and Parser ───

    #[test]
    fn test_parse_display_a_l() {
        let cmd = parse_command("D A,L");
        assert_eq!(cmd, SystemCommand::DisplayActive { filter: None });
    }

    #[test]
    fn test_parse_unknown_command() {
        let cmd = parse_command("XYZZY");
        match cmd {
            SystemCommand::Unknown(_) => {}
            _ => panic!("expected Unknown"),
        }
    }

    #[test]
    fn test_unknown_command_error_message() {
        let mut disp = CommandDispatcher::new();
        let out = disp.execute("XYZZY");
        assert!(!out.success);
        assert!(out.messages[0].contains("IEE305I"));
    }

    #[test]
    fn test_command_registry() {
        let mut reg = CommandRegistry::new();
        reg.register("TEST", Box::new(|_, _| CommandOutput::ok(vec!["OK".into()])));
        assert!(reg.is_registered("TEST"));
        assert!(!reg.is_registered("NOTEST"));
        assert_eq!(reg.count(), 1);
    }

    // ─── SYS-102.2: DISPLAY A,L ───

    #[test]
    fn test_display_active_all() {
        let mut disp = setup_dispatcher();
        let out = disp.execute("D A,L");
        assert!(out.success);
        assert!(out.messages.iter().any(|m| m.contains("PAYROLL")));
        assert!(out.messages.iter().any(|m| m.contains("BACKUP")));
        assert!(out.messages.iter().any(|m| m.contains("CICS")));
        assert!(out.messages.iter().any(|m| m.contains("3 ADDRESS SPACES")));
    }

    #[test]
    fn test_display_active_with_filter() {
        let mut disp = setup_dispatcher();
        let out = disp.execute("D A,PAY");
        assert!(out.success);
        assert!(out.messages.iter().any(|m| m.contains("PAYROLL")));
        assert!(!out.messages.iter().any(|m| m.contains("BACKUP")));
    }

    #[test]
    fn test_display_active_shows_status() {
        let mut disp = setup_dispatcher();
        let out = disp.execute("D A,L");
        assert!(out.messages.iter().any(|m| m.contains("IN")));
    }

    // ─── SYS-102.3: DISPLAY J ───

    #[test]
    fn test_display_job() {
        let mut disp = setup_dispatcher();
        let out = disp.execute("D J,PAYROLL");
        assert!(out.success);
        assert!(out.messages.iter().any(|m| m.contains("PAYROLL")));
        assert!(out.messages.iter().any(|m| m.contains("STEP1")));
        assert!(out.messages.iter().any(|m| m.contains("PAYPROG")));
        assert!(out.messages.iter().any(|m| m.contains("12.50")));
        assert!(out.messages.iter().any(|m| m.contains("INIT1")));
    }

    #[test]
    fn test_display_job_not_found() {
        let mut disp = setup_dispatcher();
        let out = disp.execute("D J,NOSUCH");
        assert!(!out.success);
        assert!(out.messages[0].contains("NOT FOUND"));
    }

    // ─── SYS-102.4: DISPLAY T and DISPLAY M ───

    #[test]
    fn test_display_time() {
        let mut disp = setup_dispatcher();
        let out = disp.execute("D T");
        assert!(out.success);
        assert!(out.messages[0].contains("IEE136I"));
        assert!(out.messages[0].contains("DATE="));
        assert!(out.messages[0].contains("TIME="));
        assert!(out.messages[0].contains("IPL"));
    }

    #[test]
    fn test_display_memory() {
        let mut disp = setup_dispatcher();
        let out = disp.execute("D M");
        assert!(out.success);
        assert!(out.messages[0].contains("IEE174I"));
        assert!(out.messages.iter().any(|m| m.contains("CSA")));
        assert!(out.messages.iter().any(|m| m.contains("SQA")));
        assert!(out.messages.iter().any(|m| m.contains("REAL")));
    }

    // ─── SYS-102.5: START/STOP/MODIFY ───

    #[test]
    fn test_start_command() {
        let mut disp = CommandDispatcher::new();
        let out = disp.execute("S MYPROG");
        assert!(out.success);
        assert!(out.messages[0].contains("STARTED"));
        assert_eq!(disp.state.address_spaces.len(), 1);
        assert_eq!(disp.state.address_spaces[0].jobname, "MYPROG");
    }

    #[test]
    fn test_start_with_parm() {
        let mut disp = CommandDispatcher::new();
        let out = disp.execute("S MYPROG,PARM='DATA'");
        assert!(out.success);
        assert!(out.messages[0].contains("PARM='DATA'"));
    }

    #[test]
    fn test_stop_command() {
        let mut disp = setup_dispatcher();
        let out = disp.execute("P PAYROLL");
        assert!(out.success);
        assert!(out.messages[0].contains("STOPPING"));
        assert_eq!(
            disp.state.address_spaces[0].status,
            AsidStatus::Stopping
        );
    }

    #[test]
    fn test_modify_command() {
        let mut disp = setup_dispatcher();
        let out = disp.execute("F CICS,CEMT SET PROG(MYPROG) NEW");
        assert!(out.success);
        assert!(out.messages[0].contains("MODIFY COMMAND ACCEPTED"));
    }

    #[test]
    fn test_modify_not_found() {
        let mut disp = setup_dispatcher();
        let out = disp.execute("F NOSUCH,PARAM");
        assert!(!out.success);
    }

    // ─── SYS-102.6: CANCEL and FORCE ───

    #[test]
    fn test_cancel_command() {
        let mut disp = setup_dispatcher();
        let out = disp.execute("C PAYROLL");
        assert!(out.success);
        assert!(out.messages[0].contains("CANCEL"));
    }

    #[test]
    fn test_cancel_not_found() {
        let mut disp = setup_dispatcher();
        let out = disp.execute("C NOSUCH");
        assert!(!out.success);
    }

    #[test]
    fn test_force_command() {
        let mut disp = setup_dispatcher();
        assert_eq!(disp.state.address_spaces.len(), 3);
        let out = disp.execute("FORCE PAYROLL,ARM");
        assert!(out.success);
        assert!(out.messages[0].contains("FORCE COMPLETE"));
        assert_eq!(disp.state.address_spaces.len(), 2);
    }

    // ─── SYS-102.7: REPLY ───

    #[test]
    fn test_reply_valid() {
        let mut disp = setup_dispatcher();
        let out = disp.execute("R 05,'YES'");
        assert!(out.success);
        assert!(out.messages[0].contains("IEE600I"));

        let wtor = &disp.state.wtors[0];
        assert!(wtor.replied);
        assert_eq!(wtor.reply_text.as_deref(), Some("YES"));
    }

    #[test]
    fn test_reply_invalid_id() {
        let mut disp = setup_dispatcher();
        let out = disp.execute("R 99,NO");
        assert!(!out.success);
        assert!(out.messages[0].contains("IEE064I"));
    }

    // ─── SYS-102.8: JES2 Command Routing ───

    #[test]
    fn test_jes2_display_active() {
        let mut disp = setup_dispatcher();
        let out = disp.execute("$DA");
        assert!(out.success);
        assert!(out.messages[0].contains("$HASP646"));
        assert!(out.messages.iter().any(|m| m.contains("PAYROLL")));
    }

    #[test]
    fn test_jes2_display_job() {
        let mut disp = setup_dispatcher();
        let out = disp.execute("$DJ,PAYROLL");
        assert!(out.success);
        assert!(out.messages[0].contains("$HASP890"));
    }

    #[test]
    fn test_jes2_start_stop() {
        let mut disp = CommandDispatcher::new();
        let out = disp.execute("$SA");
        assert!(out.success);
        assert!(out.messages[0].contains("$HASP600"));

        let out = disp.execute("$PA");
        assert!(out.success);
        assert!(out.messages[0].contains("$HASP601"));
    }

    #[test]
    fn test_jes2_cancel_activate_set() {
        let mut disp = CommandDispatcher::new();
        let out = disp.execute("$CA");
        assert!(out.success);
        assert!(out.messages[0].contains("$HASP602"));

        let out = disp.execute("$AA");
        assert!(out.success);
        assert!(out.messages[0].contains("$HASP603"));

        let out = disp.execute("$TA");
        assert!(out.success);
        assert!(out.messages[0].contains("$HASP604"));
    }

    // ─── SYS-102.9: Output Formatting & Integration ───

    #[test]
    fn test_output_uses_iee_prefix() {
        let mut disp = setup_dispatcher();

        let out = disp.execute("D A,L");
        assert!(out.messages[0].starts_with("IEE"));

        let out = disp.execute("D T");
        assert!(out.messages[0].starts_with("IEE"));

        let out = disp.execute("D M");
        assert!(out.messages[0].starts_with("IEE"));
    }

    #[test]
    fn test_full_command_scenario() {
        let mut disp = CommandDispatcher::new();

        // Start several jobs
        disp.execute("S PAYROLL");
        disp.execute("S BACKUP");
        disp.execute("S REPORT");

        // Verify active
        let out = disp.execute("D A,L");
        assert!(out.messages.iter().any(|m| m.contains("3 ADDRESS SPACES")));

        // Display specific job
        let out = disp.execute("D J,PAYROLL");
        assert!(out.success);

        // Modify
        let out = disp.execute("F BACKUP,PARAM1");
        assert!(out.success);

        // Cancel one
        let out = disp.execute("C REPORT");
        assert!(out.success);

        // Force another
        let out = disp.execute("FORCE BACKUP,ARM");
        assert!(out.success);
        assert_eq!(disp.state.address_spaces.len(), 2);

        // Display time and memory
        assert!(disp.execute("D T").success);
        assert!(disp.execute("D M").success);
    }

    #[test]
    fn test_jes2_routing_integration() {
        let mut disp = setup_dispatcher();

        // All JES2 verbs
        assert!(disp.execute("$DA").success);
        assert!(disp.execute("$DJ,PAYROLL").success);
        assert!(disp.execute("$SA").success);
        assert!(disp.execute("$PA").success);
        assert!(disp.execute("$CA").success);
        assert!(disp.execute("$AA").success);
        assert!(disp.execute("$TA").success);
    }
}
