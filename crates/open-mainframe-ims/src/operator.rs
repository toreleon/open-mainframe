//! IMS-TM105: Operator Command Framework.
//!
//! Implements the IMS operator command model:
//! - `/DIS` (Display) -- transactions, programs, regions, status
//! - `/STA`, `/STO` (Start/Stop) -- transactions, programs, regions
//! - `/ASSIGN`, `/CHANGE` -- alter transaction attributes
//! - `/CHE` (Checkpoint) -- various checkpoint flavours
//! - Programmatic CMD/GCMD execution

use crate::{ImsError, ImsResult, StatusCode};

// ---------------------------------------------------------------------------
// Display commands
// ---------------------------------------------------------------------------

/// Target resource for a DISPLAY command.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DisplayTarget {
    /// Display transaction status.
    Transaction(String),
    /// Display program status.
    Program(String),
    /// Display region status.
    Region(String),
    /// Display overall IMS status.
    Status,
}

/// A `/DIS` command.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DisplayCommand {
    /// What to display.
    pub target: DisplayTarget,
}

impl DisplayCommand {
    /// Create a display-transaction command.
    pub fn transaction(name: &str) -> Self {
        Self {
            target: DisplayTarget::Transaction(name.to_string()),
        }
    }

    /// Create a display-program command.
    pub fn program(name: &str) -> Self {
        Self {
            target: DisplayTarget::Program(name.to_string()),
        }
    }

    /// Create a display-region command.
    pub fn region(name: &str) -> Self {
        Self {
            target: DisplayTarget::Region(name.to_string()),
        }
    }

    /// Create a display-status command.
    pub fn status() -> Self {
        Self {
            target: DisplayTarget::Status,
        }
    }

    /// Format the command as a human-readable string.
    pub fn to_command_string(&self) -> String {
        match &self.target {
            DisplayTarget::Transaction(n) => format!("/DIS TRAN {}", n),
            DisplayTarget::Program(n) => format!("/DIS PGM {}", n),
            DisplayTarget::Region(n) => format!("/DIS REGION {}", n),
            DisplayTarget::Status => "/DIS STATUS".to_string(),
        }
    }
}

// ---------------------------------------------------------------------------
// Start / Stop commands
// ---------------------------------------------------------------------------

/// Target resource for START/STOP commands.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StartStopTarget {
    /// Transaction code.
    Transaction(String),
    /// Program name.
    Program(String),
    /// Region identifier.
    Region(String),
}

/// A `/STA` or `/STO` command.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StartStopCommand {
    /// Whether this is a start (`true`) or stop (`false`).
    pub is_start: bool,
    /// Target resource.
    pub target: StartStopTarget,
}

impl StartStopCommand {
    /// Create a start-transaction command.
    pub fn start_transaction(name: &str) -> Self {
        Self {
            is_start: true,
            target: StartStopTarget::Transaction(name.to_string()),
        }
    }

    /// Create a stop-transaction command.
    pub fn stop_transaction(name: &str) -> Self {
        Self {
            is_start: false,
            target: StartStopTarget::Transaction(name.to_string()),
        }
    }

    /// Create a start-program command.
    pub fn start_program(name: &str) -> Self {
        Self {
            is_start: true,
            target: StartStopTarget::Program(name.to_string()),
        }
    }

    /// Create a stop-program command.
    pub fn stop_program(name: &str) -> Self {
        Self {
            is_start: false,
            target: StartStopTarget::Program(name.to_string()),
        }
    }

    /// Create a start-region command.
    pub fn start_region(name: &str) -> Self {
        Self {
            is_start: true,
            target: StartStopTarget::Region(name.to_string()),
        }
    }

    /// Create a stop-region command.
    pub fn stop_region(name: &str) -> Self {
        Self {
            is_start: false,
            target: StartStopTarget::Region(name.to_string()),
        }
    }

    /// Format the command as a human-readable string.
    pub fn to_command_string(&self) -> String {
        let verb = if self.is_start { "/STA" } else { "/STO" };
        match &self.target {
            StartStopTarget::Transaction(n) => format!("{} TRAN {}", verb, n),
            StartStopTarget::Program(n) => format!("{} PGM {}", verb, n),
            StartStopTarget::Region(n) => format!("{} REGION {}", verb, n),
        }
    }
}

// ---------------------------------------------------------------------------
// Assign / Change commands
// ---------------------------------------------------------------------------

/// An `/ASSIGN` or `/CHANGE` command.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignChangeCommand {
    /// The resource being modified.
    pub resource: String,
    /// The attribute name.
    pub attribute: String,
    /// The new value.
    pub value: String,
}

impl AssignChangeCommand {
    /// `/ASSIGN CLASS` -- assign a class to a transaction.
    pub fn assign_class(trancode: &str, class: u16) -> Self {
        Self {
            resource: trancode.to_string(),
            attribute: "CLASS".to_string(),
            value: class.to_string(),
        }
    }

    /// `/CHANGE TRAN` -- change a transaction attribute.
    pub fn change_tran(trancode: &str, attribute: &str, value: &str) -> Self {
        Self {
            resource: trancode.to_string(),
            attribute: attribute.to_string(),
            value: value.to_string(),
        }
    }

    /// Format the command as a human-readable string.
    pub fn to_command_string(&self) -> String {
        format!("/CHANGE {} {} {}", self.resource, self.attribute, self.value)
    }
}

// ---------------------------------------------------------------------------
// Checkpoint commands
// ---------------------------------------------------------------------------

/// Checkpoint mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CheckpointMode {
    /// Normal checkpoint.
    Normal,
    /// Freeze checkpoint -- quiesces the system before checkpointing.
    Freeze,
    /// Purge checkpoint -- processes all in-flight work, then checkpoints.
    Purge,
}

/// A `/CHE` (checkpoint) command.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckpointCommand {
    /// Checkpoint mode.
    pub mode: CheckpointMode,
}

impl CheckpointCommand {
    /// `/CHE` -- normal checkpoint.
    pub fn normal() -> Self {
        Self {
            mode: CheckpointMode::Normal,
        }
    }

    /// `/CHE FREEZE`.
    pub fn freeze() -> Self {
        Self {
            mode: CheckpointMode::Freeze,
        }
    }

    /// `/CHE PURGE`.
    pub fn purge() -> Self {
        Self {
            mode: CheckpointMode::Purge,
        }
    }

    /// Format the command as a human-readable string.
    pub fn to_command_string(&self) -> String {
        match self.mode {
            CheckpointMode::Normal => "/CHE".to_string(),
            CheckpointMode::Freeze => "/CHE FREEZE".to_string(),
            CheckpointMode::Purge => "/CHE PURGE".to_string(),
        }
    }
}

// ---------------------------------------------------------------------------
// Unified IMS command enum
// ---------------------------------------------------------------------------

/// Top-level IMS operator command.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImsCommand {
    /// Display command.
    Display(DisplayCommand),
    /// Start or stop command.
    StartStop(StartStopCommand),
    /// Assign or change command.
    AssignChange(AssignChangeCommand),
    /// Checkpoint command.
    Checkpoint(CheckpointCommand),
}

impl ImsCommand {
    /// Format the command as a human-readable string.
    pub fn to_command_string(&self) -> String {
        match self {
            ImsCommand::Display(c) => c.to_command_string(),
            ImsCommand::StartStop(c) => c.to_command_string(),
            ImsCommand::AssignChange(c) => c.to_command_string(),
            ImsCommand::Checkpoint(c) => c.to_command_string(),
        }
    }
}

// ---------------------------------------------------------------------------
// Command result
// ---------------------------------------------------------------------------

/// Result of executing an IMS command.
#[derive(Debug, Clone)]
pub struct CommandResult {
    /// Whether the command succeeded.
    pub success: bool,
    /// Return code (0 = success).
    pub return_code: u32,
    /// Human-readable response text.
    pub response: String,
}

impl CommandResult {
    /// Create a successful result.
    pub fn ok(response: &str) -> Self {
        Self {
            success: true,
            return_code: 0,
            response: response.to_string(),
        }
    }

    /// Create a failure result.
    pub fn error(return_code: u32, response: &str) -> Self {
        Self {
            success: false,
            return_code,
            response: response.to_string(),
        }
    }
}

// ---------------------------------------------------------------------------
// Command processor
// ---------------------------------------------------------------------------

/// Parses and executes IMS operator commands.
#[derive(Debug, Default)]
pub struct ImsCommandProcessor {
    /// Whether the processor is active (IMS is up).
    active: bool,
}

impl ImsCommandProcessor {
    /// Create a new command processor.
    pub fn new() -> Self {
        Self { active: true }
    }

    /// Parse a command string into an [`ImsCommand`].
    ///
    /// Supported formats:
    /// - `/DIS TRAN <name>`, `/DIS PGM <name>`, `/DIS REGION <name>`, `/DIS STATUS`
    /// - `/STA TRAN <name>`, `/STO TRAN <name>`, `/STA PGM <name>`, etc.
    /// - `/ASSIGN CLASS <tran> <class>`
    /// - `/CHANGE TRAN <name> <attr> <value>`
    /// - `/CHE`, `/CHE FREEZE`, `/CHE PURGE`
    pub fn parse(&self, cmd_text: &str) -> ImsResult<ImsCommand> {
        let parts: Vec<&str> = cmd_text.split_whitespace().collect();
        if parts.is_empty() {
            return Err(ImsError::DliError {
                status: StatusCode::AD,
            });
        }

        let verb = parts[0].to_uppercase();
        match verb.as_str() {
            "/DIS" | "/DISPLAY" => self.parse_display(&parts[1..]),
            "/STA" | "/START" => self.parse_start_stop(true, &parts[1..]),
            "/STO" | "/STOP" => self.parse_start_stop(false, &parts[1..]),
            "/ASSIGN" => self.parse_assign(&parts[1..]),
            "/CHANGE" => self.parse_change(&parts[1..]),
            "/CHE" | "/CHECKPOINT" => self.parse_checkpoint(&parts[1..]),
            _ => Err(ImsError::DliError {
                status: StatusCode::AD,
            }),
        }
    }

    fn parse_display(&self, args: &[&str]) -> ImsResult<ImsCommand> {
        if args.is_empty() {
            return Err(ImsError::DliError {
                status: StatusCode::AD,
            });
        }
        let resource = args[0].to_uppercase();
        match resource.as_str() {
            "TRAN" => {
                let name = args.get(1).unwrap_or(&"*");
                Ok(ImsCommand::Display(DisplayCommand::transaction(name)))
            }
            "PGM" => {
                let name = args.get(1).unwrap_or(&"*");
                Ok(ImsCommand::Display(DisplayCommand::program(name)))
            }
            "REGION" => {
                let name = args.get(1).unwrap_or(&"*");
                Ok(ImsCommand::Display(DisplayCommand::region(name)))
            }
            "STATUS" => Ok(ImsCommand::Display(DisplayCommand::status())),
            _ => Err(ImsError::DliError {
                status: StatusCode::AD,
            }),
        }
    }

    fn parse_start_stop(&self, is_start: bool, args: &[&str]) -> ImsResult<ImsCommand> {
        if args.len() < 2 {
            return Err(ImsError::DliError {
                status: StatusCode::AD,
            });
        }
        let resource = args[0].to_uppercase();
        let name = args[1];
        let cmd = match resource.as_str() {
            "TRAN" => {
                if is_start {
                    StartStopCommand::start_transaction(name)
                } else {
                    StartStopCommand::stop_transaction(name)
                }
            }
            "PGM" => {
                if is_start {
                    StartStopCommand::start_program(name)
                } else {
                    StartStopCommand::stop_program(name)
                }
            }
            "REGION" => {
                if is_start {
                    StartStopCommand::start_region(name)
                } else {
                    StartStopCommand::stop_region(name)
                }
            }
            _ => {
                return Err(ImsError::DliError {
                    status: StatusCode::AD,
                })
            }
        };
        Ok(ImsCommand::StartStop(cmd))
    }

    fn parse_assign(&self, args: &[&str]) -> ImsResult<ImsCommand> {
        // /ASSIGN CLASS <tran> <class>
        if args.len() < 3 {
            return Err(ImsError::DliError {
                status: StatusCode::AD,
            });
        }
        let attr = args[0].to_uppercase();
        if attr != "CLASS" {
            return Err(ImsError::DliError {
                status: StatusCode::AD,
            });
        }
        let trancode = args[1];
        let class: u16 = args[2].parse().map_err(|_| ImsError::DliError {
            status: StatusCode::AD,
        })?;
        Ok(ImsCommand::AssignChange(AssignChangeCommand::assign_class(
            trancode, class,
        )))
    }

    fn parse_change(&self, args: &[&str]) -> ImsResult<ImsCommand> {
        // /CHANGE TRAN <name> <attr> <value>
        if args.len() < 4 {
            return Err(ImsError::DliError {
                status: StatusCode::AD,
            });
        }
        let resource_type = args[0].to_uppercase();
        if resource_type != "TRAN" {
            return Err(ImsError::DliError {
                status: StatusCode::AD,
            });
        }
        Ok(ImsCommand::AssignChange(AssignChangeCommand::change_tran(
            args[1], args[2], args[3],
        )))
    }

    fn parse_checkpoint(&self, args: &[&str]) -> ImsResult<ImsCommand> {
        if args.is_empty() {
            return Ok(ImsCommand::Checkpoint(CheckpointCommand::normal()));
        }
        let mode = args[0].to_uppercase();
        match mode.as_str() {
            "FREEZE" => Ok(ImsCommand::Checkpoint(CheckpointCommand::freeze())),
            "PURGE" => Ok(ImsCommand::Checkpoint(CheckpointCommand::purge())),
            _ => Err(ImsError::DliError {
                status: StatusCode::AD,
            }),
        }
    }

    /// Execute a command and return a formatted result.
    pub fn execute_command(&self, cmd_text: &str) -> ImsResult<CommandResult> {
        if !self.active {
            return Ok(CommandResult::error(8, "IMS system is not active"));
        }
        let cmd = self.parse(cmd_text)?;
        let response = match &cmd {
            ImsCommand::Display(d) => format!("DFS058I {} COMMAND COMPLETED", d.to_command_string()),
            ImsCommand::StartStop(ss) => {
                let action = if ss.is_start { "STARTED" } else { "STOPPED" };
                format!("DFS058I RESOURCE {} {}", action, ss.to_command_string())
            }
            ImsCommand::AssignChange(ac) => {
                format!(
                    "DFS058I {} {} SET TO {}",
                    ac.resource, ac.attribute, ac.value
                )
            }
            ImsCommand::Checkpoint(ck) => {
                format!("DFS058I CHECKPOINT {} COMPLETED", ck.to_command_string())
            }
        };
        Ok(CommandResult::ok(&response))
    }

    /// Set the active state of the processor.
    pub fn set_active(&mut self, active: bool) {
        self.active = active;
    }

    /// Return whether the processor is active.
    pub fn is_active(&self) -> bool {
        self.active
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_command_strings() {
        assert_eq!(
            DisplayCommand::transaction("TRAN1").to_command_string(),
            "/DIS TRAN TRAN1"
        );
        assert_eq!(
            DisplayCommand::program("PGM1").to_command_string(),
            "/DIS PGM PGM1"
        );
        assert_eq!(
            DisplayCommand::region("REG1").to_command_string(),
            "/DIS REGION REG1"
        );
        assert_eq!(DisplayCommand::status().to_command_string(), "/DIS STATUS");
    }

    #[test]
    fn test_start_stop_command_strings() {
        assert_eq!(
            StartStopCommand::start_transaction("T1").to_command_string(),
            "/STA TRAN T1"
        );
        assert_eq!(
            StartStopCommand::stop_transaction("T1").to_command_string(),
            "/STO TRAN T1"
        );
        assert_eq!(
            StartStopCommand::start_program("P1").to_command_string(),
            "/STA PGM P1"
        );
        assert_eq!(
            StartStopCommand::stop_program("P1").to_command_string(),
            "/STO PGM P1"
        );
        assert_eq!(
            StartStopCommand::start_region("R1").to_command_string(),
            "/STA REGION R1"
        );
        assert_eq!(
            StartStopCommand::stop_region("R1").to_command_string(),
            "/STO REGION R1"
        );
    }

    #[test]
    fn test_assign_change_command() {
        let cmd = AssignChangeCommand::assign_class("TRAN1", 5);
        assert_eq!(cmd.resource, "TRAN1");
        assert_eq!(cmd.attribute, "CLASS");
        assert_eq!(cmd.value, "5");

        let cmd = AssignChangeCommand::change_tran("TRAN2", "PRIORITY", "10");
        assert_eq!(cmd.resource, "TRAN2");
        assert_eq!(cmd.attribute, "PRIORITY");
        assert_eq!(cmd.value, "10");
    }

    #[test]
    fn test_checkpoint_commands() {
        assert_eq!(CheckpointCommand::normal().to_command_string(), "/CHE");
        assert_eq!(
            CheckpointCommand::freeze().to_command_string(),
            "/CHE FREEZE"
        );
        assert_eq!(
            CheckpointCommand::purge().to_command_string(),
            "/CHE PURGE"
        );
    }

    #[test]
    fn test_ims_command_to_string() {
        let cmd = ImsCommand::Display(DisplayCommand::status());
        assert_eq!(cmd.to_command_string(), "/DIS STATUS");
    }

    #[test]
    fn test_parse_display_tran() {
        let proc = ImsCommandProcessor::new();
        let cmd = proc.parse("/DIS TRAN MYTRAN").unwrap();
        assert_eq!(
            cmd,
            ImsCommand::Display(DisplayCommand::transaction("MYTRAN"))
        );
    }

    #[test]
    fn test_parse_display_status() {
        let proc = ImsCommandProcessor::new();
        let cmd = proc.parse("/DIS STATUS").unwrap();
        assert_eq!(cmd, ImsCommand::Display(DisplayCommand::status()));
    }

    #[test]
    fn test_parse_start_tran() {
        let proc = ImsCommandProcessor::new();
        let cmd = proc.parse("/STA TRAN T1").unwrap();
        assert_eq!(
            cmd,
            ImsCommand::StartStop(StartStopCommand::start_transaction("T1"))
        );
    }

    #[test]
    fn test_parse_stop_pgm() {
        let proc = ImsCommandProcessor::new();
        let cmd = proc.parse("/STO PGM PGM01").unwrap();
        assert_eq!(
            cmd,
            ImsCommand::StartStop(StartStopCommand::stop_program("PGM01"))
        );
    }

    #[test]
    fn test_parse_assign_class() {
        let proc = ImsCommandProcessor::new();
        let cmd = proc.parse("/ASSIGN CLASS TRAN1 5").unwrap();
        match cmd {
            ImsCommand::AssignChange(ac) => {
                assert_eq!(ac.resource, "TRAN1");
                assert_eq!(ac.attribute, "CLASS");
                assert_eq!(ac.value, "5");
            }
            _ => panic!("Expected AssignChange"),
        }
    }

    #[test]
    fn test_parse_change_tran() {
        let proc = ImsCommandProcessor::new();
        let cmd = proc.parse("/CHANGE TRAN TRAN1 PRIORITY 10").unwrap();
        match cmd {
            ImsCommand::AssignChange(ac) => {
                assert_eq!(ac.resource, "TRAN1");
                assert_eq!(ac.attribute, "PRIORITY");
                assert_eq!(ac.value, "10");
            }
            _ => panic!("Expected AssignChange"),
        }
    }

    #[test]
    fn test_parse_checkpoint_normal() {
        let proc = ImsCommandProcessor::new();
        let cmd = proc.parse("/CHE").unwrap();
        assert_eq!(cmd, ImsCommand::Checkpoint(CheckpointCommand::normal()));
    }

    #[test]
    fn test_parse_checkpoint_freeze() {
        let proc = ImsCommandProcessor::new();
        let cmd = proc.parse("/CHE FREEZE").unwrap();
        assert_eq!(cmd, ImsCommand::Checkpoint(CheckpointCommand::freeze()));
    }

    #[test]
    fn test_parse_checkpoint_purge() {
        let proc = ImsCommandProcessor::new();
        let cmd = proc.parse("/CHE PURGE").unwrap();
        assert_eq!(cmd, ImsCommand::Checkpoint(CheckpointCommand::purge()));
    }

    #[test]
    fn test_parse_invalid_command() {
        let proc = ImsCommandProcessor::new();
        assert!(proc.parse("/INVALID").is_err());
    }

    #[test]
    fn test_parse_empty_command() {
        let proc = ImsCommandProcessor::new();
        assert!(proc.parse("").is_err());
    }

    #[test]
    fn test_execute_command_display() {
        let proc = ImsCommandProcessor::new();
        let result = proc.execute_command("/DIS STATUS").unwrap();
        assert!(result.success);
        assert_eq!(result.return_code, 0);
        assert!(result.response.contains("COMMAND COMPLETED"));
    }

    #[test]
    fn test_execute_command_start() {
        let proc = ImsCommandProcessor::new();
        let result = proc.execute_command("/STA TRAN TRAN1").unwrap();
        assert!(result.success);
        assert!(result.response.contains("STARTED"));
    }

    #[test]
    fn test_execute_command_inactive() {
        let mut proc = ImsCommandProcessor::new();
        proc.set_active(false);
        let result = proc.execute_command("/DIS STATUS").unwrap();
        assert!(!result.success);
        assert!(result.response.contains("not active"));
    }

    #[test]
    fn test_command_result_ok() {
        let r = CommandResult::ok("success");
        assert!(r.success);
        assert_eq!(r.return_code, 0);
    }

    #[test]
    fn test_command_result_error() {
        let r = CommandResult::error(8, "fail");
        assert!(!r.success);
        assert_eq!(r.return_code, 8);
    }

    #[test]
    fn test_processor_active_state() {
        let mut proc = ImsCommandProcessor::new();
        assert!(proc.is_active());
        proc.set_active(false);
        assert!(!proc.is_active());
    }
}
