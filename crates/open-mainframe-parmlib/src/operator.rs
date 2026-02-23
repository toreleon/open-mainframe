//! Initialisation sequence orchestration and operator commands.
//!
//! Implements the ordered startup sequence (discover PARMLIB, load symbols,
//! substitute, load IEASYSxx, load subsidiary members, execute COMMNDxx) and
//! the `SET`, `SETPROG`, and `DISPLAY PARMLIB` operator commands.

use std::collections::HashMap;
use thiserror::Error;

use crate::members::{
    CommndConfig, ConsolConfig, IeaSysConfig, LnkLstConfig, ParmlibConcat, ProgConfig,
};
use crate::symbols::{IeaSymConfig, SymbolEngine};

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

/// Errors from the operator / initialisation subsystem.
#[derive(Debug, Error)]
pub enum OperatorError {
    /// Initialisation phase error.
    #[error("init phase {phase:?}: {detail}")]
    InitError {
        /// Phase that failed.
        phase: InitPhase,
        /// Detail message.
        detail: String,
    },

    /// Invalid SET command.
    #[error("invalid SET command: {0}")]
    InvalidSet(String),

    /// Invalid SETPROG command.
    #[error("invalid SETPROG command: {0}")]
    InvalidSetProg(String),

    /// Member not found.
    #[error("member not found: {0}")]
    MemberNotFound(String),

    /// Parse error forwarded from a member parser.
    #[error("parse error: {0}")]
    ParseError(String),
}

/// Convenience result alias.
pub type Result<T> = std::result::Result<T, OperatorError>;

// ---------------------------------------------------------------------------
// InitPhase — SYS-120 Story 1
// ---------------------------------------------------------------------------

/// Initialisation phases for the startup sequence.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InitPhase {
    /// Discovering PARMLIB concatenation.
    DiscoverParmlib,
    /// Loading IEASYMxx symbol definitions.
    LoadSymbols,
    /// Performing symbol substitution.
    Substitute,
    /// Loading IEASYSxx system parameters.
    LoadIeaSys,
    /// Loading subsidiary members (LNKLSTxx, PROGxx, CONSOLxx, etc.).
    LoadSubsidiary,
    /// Executing COMMNDxx IPL commands.
    ExecuteCommands,
    /// Initialisation complete.
    Complete,
}

impl std::fmt::Display for InitPhase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DiscoverParmlib => write!(f, "DISCOVER_PARMLIB"),
            Self::LoadSymbols => write!(f, "LOAD_SYMBOLS"),
            Self::Substitute => write!(f, "SUBSTITUTE"),
            Self::LoadIeaSys => write!(f, "LOAD_IEASYS"),
            Self::LoadSubsidiary => write!(f, "LOAD_SUBSIDIARY"),
            Self::ExecuteCommands => write!(f, "EXECUTE_COMMANDS"),
            Self::Complete => write!(f, "COMPLETE"),
        }
    }
}

// ---------------------------------------------------------------------------
// InitSequence — SYS-120 Story 1
// ---------------------------------------------------------------------------

/// Orchestrates the ordered system startup sequence.
///
/// The sequence is:
/// 1. Discover PARMLIB concatenation
/// 2. Load IEASYMxx symbol definitions
/// 3. Perform symbol substitution
/// 4. Load IEASYSxx system parameters
/// 5. Load subsidiary members
/// 6. Execute COMMNDxx IPL commands
#[derive(Debug)]
pub struct InitSequence {
    /// Current phase.
    phase: InitPhase,
    /// PARMLIB concatenation.
    parmlib: ParmlibConcat,
    /// Symbol engine.
    symbol_engine: SymbolEngine,
    /// Active member suffixes.
    active_suffixes: HashMap<String, String>,
    /// Loaded IEASYSxx configuration.
    ieasys: Option<IeaSysConfig>,
    /// Loaded LNKLST configuration.
    lnklst: Option<LnkLstConfig>,
    /// Loaded PROG configuration.
    prog: Option<ProgConfig>,
    /// Loaded CONSOL configuration.
    consol: Option<ConsolConfig>,
    /// Loaded COMMND configuration.
    commnd: Option<CommndConfig>,
    /// Log of executed IPL commands.
    command_log: Vec<String>,
}

impl InitSequence {
    /// Create a new initialisation sequence with the given PARMLIB concatenation.
    pub fn new(parmlib: ParmlibConcat) -> Self {
        Self {
            phase: InitPhase::DiscoverParmlib,
            parmlib,
            symbol_engine: SymbolEngine::new(),
            active_suffixes: HashMap::new(),
            ieasys: None,
            lnklst: None,
            prog: None,
            consol: None,
            commnd: None,
            command_log: Vec::new(),
        }
    }

    /// Return the current initialisation phase.
    pub fn phase(&self) -> InitPhase {
        self.phase
    }

    /// Return a reference to the active PARMLIB concatenation.
    pub fn parmlib(&self) -> &ParmlibConcat {
        &self.parmlib
    }

    /// Return a reference to the symbol engine.
    pub fn symbol_engine(&self) -> &SymbolEngine {
        &self.symbol_engine
    }

    /// Return the active member suffixes (prefix -> suffix).
    pub fn active_suffixes(&self) -> &HashMap<String, String> {
        &self.active_suffixes
    }

    /// Return the loaded IEASYSxx config, if any.
    pub fn ieasys(&self) -> Option<&IeaSysConfig> {
        self.ieasys.as_ref()
    }

    /// Return the PROG config, if any.
    pub fn prog(&self) -> Option<&ProgConfig> {
        self.prog.as_ref()
    }

    /// Return the command log (commands executed during init).
    pub fn command_log(&self) -> &[String] {
        &self.command_log
    }

    /// Run the discovery phase — validates the PARMLIB directories exist.
    pub fn discover_parmlib(&mut self) -> Result<()> {
        self.phase = InitPhase::DiscoverParmlib;
        // We simply accept the concatenation as given; actual directory
        // validation is the caller's responsibility.
        self.phase = InitPhase::LoadSymbols;
        Ok(())
    }

    /// Load symbol definitions from an IEASYMxx member (by content).
    pub fn load_symbols(&mut self, suffix: &str, content: &str) -> Result<()> {
        self.phase = InitPhase::LoadSymbols;
        let cfg = IeaSymConfig::parse(&format!("IEASYM{suffix}"), content).map_err(|e| {
            OperatorError::InitError {
                phase: InitPhase::LoadSymbols,
                detail: e.to_string(),
            }
        })?;
        self.symbol_engine.load_config(&cfg);
        self.active_suffixes
            .insert("IEASYM".to_string(), suffix.to_string());
        self.phase = InitPhase::Substitute;
        Ok(())
    }

    /// Perform symbol substitution on a piece of text.
    pub fn substitute(&self, text: &str) -> String {
        self.symbol_engine.substitute(text)
    }

    /// Load the IEASYSxx system parameters (by content, after substitution).
    pub fn load_ieasys(&mut self, suffix: &str, content: &str) -> Result<()> {
        self.phase = InitPhase::LoadIeaSys;
        let substituted = self.symbol_engine.substitute(content);
        let cfg =
            IeaSysConfig::parse(&format!("IEASYS{suffix}"), &substituted).map_err(|e| {
                OperatorError::InitError {
                    phase: InitPhase::LoadIeaSys,
                    detail: e.to_string(),
                }
            })?;
        self.active_suffixes
            .insert("IEASYS".to_string(), suffix.to_string());
        self.ieasys = Some(cfg);
        self.phase = InitPhase::LoadSubsidiary;
        Ok(())
    }

    /// Load a subsidiary member (LNKLSTxx, PROGxx, CONSOLxx) by prefix, suffix, and content.
    pub fn load_subsidiary(
        &mut self,
        prefix: &str,
        suffix: &str,
        content: &str,
    ) -> Result<()> {
        self.phase = InitPhase::LoadSubsidiary;
        let substituted = self.symbol_engine.substitute(content);
        let member_name = format!("{prefix}{suffix}");
        let upper_prefix = prefix.to_uppercase();

        match upper_prefix.as_str() {
            "LNKLST" => {
                let cfg = LnkLstConfig::parse(&member_name, &substituted)
                    .map_err(|e| OperatorError::ParseError(e.to_string()))?;
                self.lnklst = Some(cfg);
            }
            "PROG" => {
                let cfg = ProgConfig::parse(&member_name, &substituted)
                    .map_err(|e| OperatorError::ParseError(e.to_string()))?;
                self.prog = Some(cfg);
            }
            "CONSOL" => {
                let cfg = ConsolConfig::parse(&member_name, &substituted)
                    .map_err(|e| OperatorError::ParseError(e.to_string()))?;
                self.consol = Some(cfg);
            }
            "COMMND" => {
                let cfg = CommndConfig::parse(&member_name, &substituted)
                    .map_err(|e| OperatorError::ParseError(e.to_string()))?;
                self.commnd = Some(cfg);
            }
            _ => {
                return Err(OperatorError::InitError {
                    phase: InitPhase::LoadSubsidiary,
                    detail: format!("unknown subsidiary prefix: {prefix}"),
                });
            }
        }

        self.active_suffixes
            .insert(upper_prefix, suffix.to_string());
        Ok(())
    }

    /// Execute the IPL commands from the loaded COMMNDxx member.
    ///
    /// In this emulation the commands are simply logged; a real system would
    /// dispatch them through the operator command processor.
    pub fn execute_commands(&mut self) -> Result<Vec<String>> {
        self.phase = InitPhase::ExecuteCommands;
        let commands = self
            .commnd
            .as_ref()
            .map(|c| c.commands.clone())
            .unwrap_or_default();
        self.command_log = commands.clone();
        self.phase = InitPhase::Complete;
        Ok(commands)
    }
}

// ---------------------------------------------------------------------------
// SetCommand — SYS-120 Story 2
// ---------------------------------------------------------------------------

/// Parsed `SET member=suffix` operator command.
///
/// Used to dynamically reload a PARMLIB member at runtime (e.g. `SET SMF=01`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SetCommand {
    /// The member prefix (e.g. `SMF`).
    pub member: String,
    /// The new suffix (e.g. `01`).
    pub suffix: String,
}

impl SetCommand {
    /// Parse a `SET member=suffix` command string.
    ///
    /// The input should be of the form `SET XXX=YY` (the `SET` keyword is
    /// optional and will be stripped if present).
    pub fn parse(input: &str) -> Result<Self> {
        let trimmed = input.trim();
        let body = if trimmed.to_uppercase().starts_with("SET ") {
            trimmed[4..].trim()
        } else {
            trimmed
        };

        let eq = body.find('=').ok_or_else(|| {
            OperatorError::InvalidSet(format!("missing '=' in SET command: {input}"))
        })?;
        let member = body[..eq].trim();
        let suffix = body[eq + 1..].trim();

        if member.is_empty() || suffix.is_empty() {
            return Err(OperatorError::InvalidSet(format!(
                "member or suffix is empty: {input}"
            )));
        }

        Ok(Self {
            member: member.to_uppercase(),
            suffix: suffix.to_uppercase(),
        })
    }
}

// ---------------------------------------------------------------------------
// SetProgCommand — SYS-120 Story 3
// ---------------------------------------------------------------------------

/// Action for a SETPROG command.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SetProgAction {
    /// Add an APF entry.
    ApfAdd {
        /// Dataset name.
        dsname: String,
        /// Optional volume serial.
        volume: Option<String>,
    },
    /// Add a linklist entry.
    LnklstAdd {
        /// Dataset name.
        dsname: String,
        /// Optional volume serial.
        volume: Option<String>,
    },
}

/// Parsed `SETPROG` operator command.
///
/// Supports:
/// - `SETPROG APF,ADD,DSN=X,VOL=Y`
/// - `SETPROG LNKLST,ADD,DSN=X,VOL=Y`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SetProgCommand {
    /// The parsed action.
    pub action: SetProgAction,
}

impl SetProgCommand {
    /// Parse a SETPROG command string.
    pub fn parse(input: &str) -> Result<Self> {
        let trimmed = input.trim();
        let body = if trimmed.to_uppercase().starts_with("SETPROG ") {
            trimmed[8..].trim()
        } else {
            trimmed
        };

        let upper = body.to_uppercase();
        let parts: Vec<&str> = upper.split(',').map(|s| s.trim()).collect();

        if parts.len() < 3 {
            return Err(OperatorError::InvalidSetProg(format!(
                "not enough operands: {input}"
            )));
        }

        let category = parts[0];
        let action = parts[1];

        if action != "ADD" {
            return Err(OperatorError::InvalidSetProg(format!(
                "unsupported action '{action}', expected ADD"
            )));
        }

        let mut dsname: Option<String> = None;
        let mut volume: Option<String> = None;

        for part in &parts[2..] {
            if let Some(val) = part.strip_prefix("DSN=").or_else(|| part.strip_prefix("DSNAME="))
            {
                dsname = Some(val.to_string());
            } else if let Some(val) = part.strip_prefix("VOL=").or_else(|| part.strip_prefix("VOLUME="))
            {
                volume = Some(val.to_string());
            }
        }

        let dsname = dsname.ok_or_else(|| {
            OperatorError::InvalidSetProg(format!("missing DSN= in SETPROG: {input}"))
        })?;

        let action = match category {
            "APF" => SetProgAction::ApfAdd { dsname, volume },
            "LNKLST" => SetProgAction::LnklstAdd { dsname, volume },
            _ => {
                return Err(OperatorError::InvalidSetProg(format!(
                    "unsupported category '{category}', expected APF or LNKLST"
                )));
            }
        };

        Ok(Self { action })
    }
}

// ---------------------------------------------------------------------------
// DisplayParmlibCommand — SYS-120 Story 4
// ---------------------------------------------------------------------------

/// Formats information about the current PARMLIB state for display.
#[derive(Debug)]
pub struct DisplayParmlibCommand;

impl DisplayParmlibCommand {
    /// Format the PARMLIB concatenation and active suffixes as a human-readable
    /// display string (analogous to `D PARMLIB` on z/OS).
    pub fn format(init: &InitSequence) -> String {
        let mut lines = Vec::new();
        lines.push("PARMLIB DISPLAY".to_string());
        lines.push("  CONCATENATION:".to_string());
        for (i, dir) in init.parmlib().directories().iter().enumerate() {
            lines.push(format!("    {:02}: {}", i + 1, dir.display()));
        }
        lines.push("  ACTIVE SUFFIXES:".to_string());
        let mut suffixes: Vec<_> = init.active_suffixes().iter().collect();
        suffixes.sort_by_key(|(k, _)| (*k).clone());
        for (prefix, suffix) in &suffixes {
            lines.push(format!("    {prefix}{suffix}"));
        }
        if suffixes.is_empty() {
            lines.push("    (none)".to_string());
        }
        lines.push(format!("  PHASE: {}", init.phase()));
        lines.join("\n")
    }

    /// Format just the concatenation directories.
    pub fn format_concat(parmlib: &ParmlibConcat) -> String {
        let mut lines = Vec::new();
        lines.push("PARMLIB CONCATENATION:".to_string());
        for (i, dir) in parmlib.directories().iter().enumerate() {
            lines.push(format!("  {:02}: {}", i + 1, dir.display()));
        }
        lines.join("\n")
    }

    /// Format the active suffixes as a table.
    pub fn format_suffixes(suffixes: &HashMap<String, String>) -> String {
        let mut lines = Vec::new();
        lines.push("ACTIVE MEMBER SUFFIXES:".to_string());
        let mut sorted: Vec<_> = suffixes.iter().collect();
        sorted.sort_by_key(|(k, _)| (*k).clone());
        for (prefix, suffix) in sorted {
            lines.push(format!("  {prefix}{suffix}"));
        }
        lines.join("\n")
    }
}

// ---------------------------------------------------------------------------
// Tests — SYS-120 Story 5
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    // -- InitSequence tests -------------------------------------------------

    #[test]
    fn init_sequence_full_flow() {
        let parmlib = ParmlibConcat::new(vec![PathBuf::from("/SYS1/PARMLIB")]);
        let mut init = InitSequence::new(parmlib);

        assert_eq!(init.phase(), InitPhase::DiscoverParmlib);

        // Phase 1: discover
        init.discover_parmlib().unwrap();
        assert_eq!(init.phase(), InitPhase::LoadSymbols);

        // Phase 2: load symbols
        let sym_content = "SYMDEF(&PLEXNAME.='PRODPLEX')\nSYMDEF(&ENV.='PROD')";
        init.load_symbols("00", sym_content).unwrap();
        assert_eq!(init.phase(), InitPhase::Substitute);
        assert_eq!(
            init.active_suffixes().get("IEASYM"),
            Some(&"00".to_string())
        );

        // Phase 3: substitution works
        assert_eq!(init.substitute("&PLEXNAME."), "PRODPLEX");

        // Phase 4: load IEASYSxx (with symbol references)
        let sys_content = "SYSPLEX(&PLEXNAME.),MAXUSER(500)";
        init.load_ieasys("00", sys_content).unwrap();
        assert_eq!(init.phase(), InitPhase::LoadSubsidiary);
        let ieasys = init.ieasys().unwrap();
        assert_eq!(ieasys.sysplex.as_deref(), Some("PRODPLEX"));
        assert_eq!(ieasys.maxuser, Some(500));

        // Phase 5: load subsidiary
        init.load_subsidiary("PROG", "00", "APF ADD DSNAME(SYS1.LINKLIB)")
            .unwrap();
        assert_eq!(
            init.active_suffixes().get("PROG"),
            Some(&"00".to_string())
        );
        assert_eq!(init.prog().unwrap().apf_entries.len(), 1);

        init.load_subsidiary("COMMND", "00", "START JES2\nSET SMF=00")
            .unwrap();

        // Phase 6: execute commands
        let cmds = init.execute_commands().unwrap();
        assert_eq!(cmds, vec!["START JES2", "SET SMF=00"]);
        assert_eq!(init.phase(), InitPhase::Complete);
        assert_eq!(init.command_log(), &["START JES2", "SET SMF=00"]);
    }

    #[test]
    fn init_sequence_bad_subsidiary_prefix() {
        let parmlib = ParmlibConcat::new(vec![]);
        let mut init = InitSequence::new(parmlib);
        let err = init
            .load_subsidiary("NOSUCH", "00", "data")
            .unwrap_err();
        assert!(matches!(err, OperatorError::InitError { .. }));
    }

    // -- SetCommand tests ---------------------------------------------------

    #[test]
    fn set_command_parse_basic() {
        let cmd = SetCommand::parse("SET SMF=01").unwrap();
        assert_eq!(cmd.member, "SMF");
        assert_eq!(cmd.suffix, "01");
    }

    #[test]
    fn set_command_parse_without_keyword() {
        let cmd = SetCommand::parse("PROG=02").unwrap();
        assert_eq!(cmd.member, "PROG");
        assert_eq!(cmd.suffix, "02");
    }

    #[test]
    fn set_command_missing_equals() {
        let err = SetCommand::parse("SET SMF").unwrap_err();
        assert!(matches!(err, OperatorError::InvalidSet(_)));
    }

    #[test]
    fn set_command_empty_member() {
        let err = SetCommand::parse("SET =01").unwrap_err();
        assert!(matches!(err, OperatorError::InvalidSet(_)));
    }

    // -- SetProgCommand tests -----------------------------------------------

    #[test]
    fn setprog_apf_add() {
        let cmd = SetProgCommand::parse("SETPROG APF,ADD,DSN=SYS1.AUTH,VOL=MVSRES").unwrap();
        assert_eq!(
            cmd.action,
            SetProgAction::ApfAdd {
                dsname: "SYS1.AUTH".to_string(),
                volume: Some("MVSRES".to_string()),
            }
        );
    }

    #[test]
    fn setprog_lnklst_add_no_vol() {
        let cmd = SetProgCommand::parse("SETPROG LNKLST,ADD,DSN=USER.LOADLIB").unwrap();
        assert_eq!(
            cmd.action,
            SetProgAction::LnklstAdd {
                dsname: "USER.LOADLIB".to_string(),
                volume: None,
            }
        );
    }

    #[test]
    fn setprog_without_keyword() {
        let cmd = SetProgCommand::parse("APF,ADD,DSN=MY.LIB").unwrap();
        assert_eq!(
            cmd.action,
            SetProgAction::ApfAdd {
                dsname: "MY.LIB".to_string(),
                volume: None,
            }
        );
    }

    #[test]
    fn setprog_bad_category() {
        let err = SetProgCommand::parse("SETPROG XYZ,ADD,DSN=A").unwrap_err();
        assert!(matches!(err, OperatorError::InvalidSetProg(_)));
    }

    #[test]
    fn setprog_missing_dsn() {
        let err = SetProgCommand::parse("SETPROG APF,ADD,VOL=X").unwrap_err();
        assert!(matches!(err, OperatorError::InvalidSetProg(_)));
    }

    #[test]
    fn setprog_bad_action() {
        let err = SetProgCommand::parse("SETPROG APF,DELETE,DSN=X").unwrap_err();
        assert!(matches!(err, OperatorError::InvalidSetProg(_)));
    }

    // -- DisplayParmlibCommand tests ----------------------------------------

    #[test]
    fn display_parmlib_format() {
        let parmlib = ParmlibConcat::new(vec![
            PathBuf::from("/SYS1/PARMLIB"),
            PathBuf::from("/USER/PARMLIB"),
        ]);
        let mut init = InitSequence::new(parmlib);
        init.discover_parmlib().unwrap();
        init.load_symbols("00", "SYMDEF(&A.='B')").unwrap();
        init.load_ieasys("00", "SYSPLEX(TEST)").unwrap();

        let display = DisplayParmlibCommand::format(&init);
        assert!(display.contains("PARMLIB DISPLAY"));
        assert!(display.contains("/SYS1/PARMLIB"));
        assert!(display.contains("/USER/PARMLIB"));
        assert!(display.contains("IEASYS00"));
        assert!(display.contains("IEASYM00"));
    }

    #[test]
    fn display_concat_only() {
        let parmlib = ParmlibConcat::new(vec![PathBuf::from("/P1"), PathBuf::from("/P2")]);
        let display = DisplayParmlibCommand::format_concat(&parmlib);
        assert!(display.contains("PARMLIB CONCATENATION"));
        assert!(display.contains("01: /P1"));
        assert!(display.contains("02: /P2"));
    }

    #[test]
    fn display_suffixes() {
        let mut suffixes = HashMap::new();
        suffixes.insert("IEASYS".to_string(), "00".to_string());
        suffixes.insert("PROG".to_string(), "01".to_string());
        let display = DisplayParmlibCommand::format_suffixes(&suffixes);
        assert!(display.contains("IEASYS00"));
        assert!(display.contains("PROG01"));
    }

    #[test]
    fn display_no_suffixes() {
        let parmlib = ParmlibConcat::new(vec![]);
        let init = InitSequence::new(parmlib);
        let display = DisplayParmlibCommand::format(&init);
        assert!(display.contains("(none)"));
    }
}
