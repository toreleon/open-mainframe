//! JES2 Installation Exits Framework.
//!
//! Implements the z/OS JES2 exit mechanism (EXIT1 through EXIT255) that allows
//! installation-specific customization of job processing at every stage.
//!
//! ## IBM Exit Numbers
//!
//! | Exit | Purpose |
//! |------|---------|
//! | EXIT1 | Job selection |
//! | EXIT2 | Queue transition (job state change notification) |
//! | EXIT5 | SYSOUT selection / output routing |
//! | EXIT6 | JCL pre-scan |
//! | EXIT7 | JCL statement scan |
//! | EXIT15 | Job termination |
//! | EXIT44 | SPOOL data access |

use std::collections::BTreeMap;
use std::fmt;
use std::sync::Arc;

use serde::{Deserialize, Serialize};
use tracing::{debug, info, trace};

// ─────────────────────── Exit Number Constants ───────────────────────

/// Exit number type (1-255).
pub type ExitNumber = u16;

/// EXIT1 — Job selection exit.
pub const EXIT1: ExitNumber = 1;
/// EXIT2 — Queue transition (job state change notification).
pub const EXIT2: ExitNumber = 2;
/// EXIT5 — SYSOUT selection / output routing.
pub const EXIT5: ExitNumber = 5;
/// EXIT6 — JCL pre-scan.
pub const EXIT6: ExitNumber = 6;
/// EXIT7 — JCL statement scan.
pub const EXIT7: ExitNumber = 7;
/// EXIT15 — Job termination.
pub const EXIT15: ExitNumber = 15;
/// EXIT44 — SPOOL data access.
pub const EXIT44: ExitNumber = 44;

/// Format an exit number as `EXITnn`.
pub fn exit_name(number: ExitNumber) -> String {
    format!("EXIT{number}")
}

// ─────────────────────── Exit Action ───────────────────────

/// Action returned by an exit routine, controlling subsequent processing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExitAction {
    /// Continue with normal processing.
    Continue,
    /// Fail the operation with a return code.
    Fail(u32),
    /// Bypass normal processing (exit handled it).
    Bypass,
}

impl ExitAction {
    /// Whether this action continues normal processing.
    pub fn is_continue(&self) -> bool {
        matches!(self, Self::Continue)
    }
}

// ─────────────────────── Exit Context ───────────────────────

/// Context passed to exit routines with job and processing details.
///
/// Fields are optional because different exits receive different data.
/// Exit routines can modify certain fields (e.g., `priority`, `sysout_class`)
/// to alter processing behavior.
#[derive(Debug, Clone, Default)]
pub struct ExitContext {
    /// The exit number being invoked.
    pub exit_number: ExitNumber,
    /// JES2 job ID string (e.g. "JOB00001").
    pub job_id: Option<String>,
    /// Job name (e.g. "PAYROLL").
    pub job_name: Option<String>,
    /// Job class character.
    pub job_class: Option<char>,
    /// Job priority (0-15).
    pub priority: Option<u8>,
    /// Submitting userid.
    pub userid: Option<String>,
    /// Current queue/state name (e.g. "INPUT", "READY").
    pub queue: Option<String>,
    /// Target queue/state for transitions.
    pub target_queue: Option<String>,
    /// SYSOUT class for output-related exits.
    pub sysout_class: Option<char>,
    /// JCL statements (for EXIT6/EXIT7).
    pub jcl_statements: Vec<String>,
    /// Spool dataset key (for EXIT44).
    pub spool_key: Option<u64>,
    /// Spool data content (for EXIT44 read access).
    pub spool_data: Option<Vec<u8>>,
    /// Job return code (for EXIT15).
    pub return_code: Option<u32>,
    /// Destination for output routing (for EXIT5).
    pub destination: Option<String>,
    /// Flag set by exit to indicate it modified the context.
    pub modified: bool,
    /// Extensible key-value pairs for custom data passing.
    pub user_data: BTreeMap<String, String>,
}

impl ExitContext {
    /// Create a context for job selection (EXIT1).
    pub fn job_selection(
        job_id: &str,
        job_name: &str,
        job_class: char,
        priority: u8,
        userid: &str,
    ) -> Self {
        Self {
            exit_number: EXIT1,
            job_id: Some(job_id.to_string()),
            job_name: Some(job_name.to_string()),
            job_class: Some(job_class),
            priority: Some(priority),
            userid: Some(userid.to_string()),
            queue: Some("READY".to_string()),
            ..Default::default()
        }
    }

    /// Create a context for queue transition (EXIT2).
    pub fn queue_transition(
        job_id: &str,
        job_name: &str,
        from_queue: &str,
        to_queue: &str,
    ) -> Self {
        Self {
            exit_number: EXIT2,
            job_id: Some(job_id.to_string()),
            job_name: Some(job_name.to_string()),
            queue: Some(from_queue.to_string()),
            target_queue: Some(to_queue.to_string()),
            ..Default::default()
        }
    }

    /// Create a context for SYSOUT selection (EXIT5).
    pub fn sysout_selection(
        job_id: &str,
        job_name: &str,
        sysout_class: char,
        destination: Option<&str>,
    ) -> Self {
        Self {
            exit_number: EXIT5,
            job_id: Some(job_id.to_string()),
            job_name: Some(job_name.to_string()),
            sysout_class: Some(sysout_class),
            destination: destination.map(|s| s.to_string()),
            ..Default::default()
        }
    }

    /// Create a context for JCL pre-scan (EXIT6).
    pub fn jcl_prescan(job_id: &str, job_name: &str, jcl: Vec<String>) -> Self {
        Self {
            exit_number: EXIT6,
            job_id: Some(job_id.to_string()),
            job_name: Some(job_name.to_string()),
            jcl_statements: jcl,
            ..Default::default()
        }
    }

    /// Create a context for JCL statement scan (EXIT7).
    pub fn jcl_scan(
        job_id: &str,
        job_name: &str,
        userid: &str,
        jcl: Vec<String>,
    ) -> Self {
        Self {
            exit_number: EXIT7,
            job_id: Some(job_id.to_string()),
            job_name: Some(job_name.to_string()),
            userid: Some(userid.to_string()),
            jcl_statements: jcl,
            ..Default::default()
        }
    }

    /// Create a context for job termination (EXIT15).
    pub fn job_termination(
        job_id: &str,
        job_name: &str,
        userid: &str,
        return_code: u32,
    ) -> Self {
        Self {
            exit_number: EXIT15,
            job_id: Some(job_id.to_string()),
            job_name: Some(job_name.to_string()),
            userid: Some(userid.to_string()),
            return_code: Some(return_code),
            ..Default::default()
        }
    }

    /// Create a context for SPOOL data access (EXIT44).
    pub fn spool_access(
        job_id: &str,
        job_name: &str,
        userid: &str,
        spool_key: u64,
    ) -> Self {
        Self {
            exit_number: EXIT44,
            job_id: Some(job_id.to_string()),
            job_name: Some(job_name.to_string()),
            userid: Some(userid.to_string()),
            spool_key: Some(spool_key),
            ..Default::default()
        }
    }
}

// ─────────────────────── Exit Trait ───────────────────────

/// Trait for JES2 installation exit routines.
///
/// Implementations are registered with the `ExitDispatcher` and invoked at
/// the appropriate processing points.
pub trait Jes2Exit: Send + Sync {
    /// Human-readable name of this exit routine.
    fn name(&self) -> &str;

    /// Invoke the exit with the given context.
    ///
    /// The exit routine may inspect and modify the context, then return
    /// an `ExitAction` to control further processing.
    fn invoke(&self, context: &mut ExitContext) -> ExitAction;
}

// ─────────────────────── Exit Configuration ───────────────────────

/// Configuration for an individual exit point.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExitConfig {
    /// Exit number (1-255).
    pub exit_number: ExitNumber,
    /// Whether this exit point is enabled.
    pub enabled: bool,
    /// Registered routine names (for display).
    pub routine_names: Vec<String>,
    /// Whether tracing is enabled for this exit.
    pub trace: bool,
}

impl ExitConfig {
    /// Create a default configuration for an exit point.
    pub fn new(exit_number: ExitNumber) -> Self {
        Self {
            exit_number,
            enabled: true,
            routine_names: Vec::new(),
            trace: false,
        }
    }
}

/// Result of $D EXIT command — display exit information.
#[derive(Debug, Clone)]
pub struct ExitDisplayResult {
    /// Exit number.
    pub exit_number: ExitNumber,
    /// Whether the exit is enabled.
    pub enabled: bool,
    /// Names of registered routines.
    pub routines: Vec<String>,
    /// Whether tracing is active.
    pub trace: bool,
}

impl fmt::Display for ExitDisplayResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let status = if self.enabled { "ENABLED" } else { "DISABLED" };
        let trace = if self.trace { ",TRACE=YES" } else { "" };
        if self.routines.is_empty() {
            write!(f, "$HASP893 EXIT({:03}) STATUS={status}{trace},ROUTINES=NONE", self.exit_number)
        } else {
            write!(
                f,
                "$HASP893 EXIT({:03}) STATUS={status}{trace},ROUTINES=({})",
                self.exit_number,
                self.routines.join(",")
            )
        }
    }
}

// ─────────────────────── Exit Dispatcher ───────────────────────

/// Central dispatcher for JES2 installation exits.
///
/// Manages registration, configuration, and invocation of exit routines
/// at numbered exit points (EXIT1 through EXIT255).
pub struct ExitDispatcher {
    /// Registered exit routines per exit number.
    exits: BTreeMap<ExitNumber, Vec<Arc<dyn Jes2Exit>>>,
    /// Configuration per exit number.
    configs: BTreeMap<ExitNumber, ExitConfig>,
}

impl fmt::Debug for ExitDispatcher {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExitDispatcher")
            .field("exit_count", &self.exits.len())
            .field("configs", &self.configs)
            .finish()
    }
}

impl Default for ExitDispatcher {
    fn default() -> Self {
        Self::new()
    }
}

impl ExitDispatcher {
    /// Create a new exit dispatcher with no registered exits.
    pub fn new() -> Self {
        Self {
            exits: BTreeMap::new(),
            configs: BTreeMap::new(),
        }
    }

    /// Register an exit routine at the given exit number.
    ///
    /// Multiple routines can be registered for the same exit number;
    /// they are invoked in registration order.
    pub fn register(&mut self, exit_number: ExitNumber, exit: Arc<dyn Jes2Exit>) {
        info!(
            "JES2 EXIT: registering routine '{}' at EXIT{:03}",
            exit.name(),
            exit_number
        );

        // Ensure config exists.
        let config = self
            .configs
            .entry(exit_number)
            .or_insert_with(|| ExitConfig::new(exit_number));
        config.routine_names.push(exit.name().to_string());

        // Add to exit list.
        self.exits
            .entry(exit_number)
            .or_default()
            .push(exit);
    }

    /// Unregister an exit routine by name.
    ///
    /// Returns `true` if a routine was removed.
    pub fn unregister(&mut self, exit_number: ExitNumber, name: &str) -> bool {
        let Some(exits) = self.exits.get_mut(&exit_number) else {
            return false;
        };

        let before = exits.len();
        exits.retain(|e| e.name() != name);
        let removed = exits.len() < before;

        if removed {
            if let Some(config) = self.configs.get_mut(&exit_number) {
                config.routine_names.retain(|n| n != name);
            }
        }

        removed
    }

    /// Invoke all registered exit routines for the given exit number.
    ///
    /// Routines are called in registration order. If any routine returns
    /// `ExitAction::Fail` or `ExitAction::Bypass`, the chain stops and
    /// that action is returned. Otherwise, `ExitAction::Continue` is returned.
    pub fn invoke(&self, context: &mut ExitContext) -> ExitAction {
        let exit_number = context.exit_number;

        // Check if exit is enabled.
        if let Some(config) = self.configs.get(&exit_number) {
            if !config.enabled {
                trace!("EXIT{:03}: disabled, skipping", exit_number);
                return ExitAction::Continue;
            }
        }

        let Some(exits) = self.exits.get(&exit_number) else {
            return ExitAction::Continue;
        };

        let tracing = self
            .configs
            .get(&exit_number)
            .map_or(false, |c| c.trace);

        for exit in exits {
            if tracing {
                debug!(
                    "EXIT{:03}: invoking routine '{}'",
                    exit_number,
                    exit.name()
                );
            }

            let action = exit.invoke(context);

            if tracing {
                debug!(
                    "EXIT{:03}: routine '{}' returned {:?}",
                    exit_number,
                    exit.name(),
                    action
                );
            }

            match action {
                ExitAction::Continue => continue,
                other => return other,
            }
        }

        ExitAction::Continue
    }

    /// Enable an exit point.
    pub fn enable(&mut self, exit_number: ExitNumber) {
        self.configs
            .entry(exit_number)
            .or_insert_with(|| ExitConfig::new(exit_number))
            .enabled = true;
    }

    /// Disable an exit point (routines remain registered but won't be invoked).
    pub fn disable(&mut self, exit_number: ExitNumber) {
        self.configs
            .entry(exit_number)
            .or_insert_with(|| ExitConfig::new(exit_number))
            .enabled = false;
    }

    /// Set trace mode for an exit point.
    pub fn set_trace(&mut self, exit_number: ExitNumber, trace: bool) {
        self.configs
            .entry(exit_number)
            .or_insert_with(|| ExitConfig::new(exit_number))
            .trace = trace;
    }

    /// Check if an exit point is enabled.
    pub fn is_enabled(&self, exit_number: ExitNumber) -> bool {
        self.configs
            .get(&exit_number)
            .map_or(true, |c| c.enabled)
    }

    /// Display exit information ($D EXIT(nn)).
    pub fn display_exit(&self, exit_number: ExitNumber) -> ExitDisplayResult {
        let config = self.configs.get(&exit_number);
        ExitDisplayResult {
            exit_number,
            enabled: config.map_or(true, |c| c.enabled),
            routines: config.map_or_else(Vec::new, |c| c.routine_names.clone()),
            trace: config.map_or(false, |c| c.trace),
        }
    }

    /// List all configured exit points.
    pub fn list_exits(&self) -> Vec<ExitDisplayResult> {
        self.configs
            .keys()
            .map(|&n| self.display_exit(n))
            .collect()
    }

    /// Get the number of registered routines for an exit point.
    pub fn routine_count(&self, exit_number: ExitNumber) -> usize {
        self.exits.get(&exit_number).map_or(0, |v| v.len())
    }

    /// Get the total number of configured exit points.
    pub fn exit_count(&self) -> usize {
        self.configs.len()
    }

    // ─────── Configuration from JES2PARM (SYS-108.5) ───────

    /// Apply an EXIT definition from JES2PARM.
    ///
    /// Format: `EXIT(nn) ROUTINES=(name) STATUS=ENABLED TRACE=NO`
    pub fn apply_exit_parm(&mut self, parm: &ExitParm) {
        let config = self
            .configs
            .entry(parm.exit_number)
            .or_insert_with(|| ExitConfig::new(parm.exit_number));

        config.enabled = parm.enabled;
        config.trace = parm.trace;

        if let Some(ref name) = parm.routine_name {
            if !config.routine_names.contains(name) {
                config.routine_names.push(name.clone());
            }
        }
    }

    /// Process a $T EXIT command (modify exit configuration).
    ///
    /// Returns a response message.
    pub fn modify_exit(
        &mut self,
        exit_number: ExitNumber,
        enabled: Option<bool>,
        trace: Option<bool>,
    ) -> String {
        let config = self
            .configs
            .entry(exit_number)
            .or_insert_with(|| ExitConfig::new(exit_number));

        if let Some(e) = enabled {
            config.enabled = e;
        }
        if let Some(t) = trace {
            config.trace = t;
        }

        format!(
            "$HASP893 EXIT({:03}) STATUS={},TRACE={}",
            exit_number,
            if config.enabled { "ENABLED" } else { "DISABLED" },
            if config.trace { "YES" } else { "NO" },
        )
    }
}

// ─────────────────────── Exit Configuration Parsing ───────────────────────

/// Parsed EXIT definition from JES2PARM.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExitParm {
    /// Exit number (1-255).
    pub exit_number: ExitNumber,
    /// Routine name (if specified).
    pub routine_name: Option<String>,
    /// Whether the exit is enabled (default: true).
    pub enabled: bool,
    /// Whether tracing is enabled (default: false).
    pub trace: bool,
}

/// Parse an EXIT parameter string.
///
/// Format: `EXIT(nn) ROUTINES=(name) STATUS=ENABLED TRACE=NO`
/// Returns `None` if the input doesn't start with "EXIT(".
pub fn parse_exit_parm(input: &str) -> Option<ExitParm> {
    let input = input.trim().to_uppercase();
    if !input.starts_with("EXIT(") {
        return None;
    }

    // Extract exit number.
    let close = input.find(')')?;
    let num_str = &input[5..close];
    let exit_number: ExitNumber = num_str.parse().ok()?;

    if exit_number == 0 || exit_number > 255 {
        return None;
    }

    let rest = &input[close + 1..];

    // Parse optional parameters.
    let routine_name = extract_param(rest, "ROUTINES=");
    let status = extract_param(rest, "STATUS=");
    let trace_str = extract_param(rest, "TRACE=");

    let enabled = status.as_deref() != Some("DISABLED");
    let trace = trace_str.as_deref() == Some("YES");

    Some(ExitParm {
        exit_number,
        routine_name,
        enabled,
        trace,
    })
}

/// Extract a parameter value from a string.
fn extract_param(input: &str, key: &str) -> Option<String> {
    let pos = input.find(key)?;
    let value_start = pos + key.len();
    let rest = &input[value_start..];

    if rest.starts_with('(') {
        // Parenthesized value.
        let end = rest.find(')')?;
        Some(rest[1..end].to_string())
    } else {
        // Space-delimited or end-of-string.
        let end = rest.find(' ').unwrap_or(rest.len());
        Some(rest[..end].to_string())
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ─── Test helpers: concrete exit implementations ───

    /// A simple exit that always returns Continue.
    struct ContinueExit {
        name: String,
    }

    impl Jes2Exit for ContinueExit {
        fn name(&self) -> &str {
            &self.name
        }
        fn invoke(&self, _ctx: &mut ExitContext) -> ExitAction {
            ExitAction::Continue
        }
    }

    /// An exit that returns Fail with a given RC.
    struct FailExit {
        name: String,
        rc: u32,
    }

    impl Jes2Exit for FailExit {
        fn name(&self) -> &str {
            &self.name
        }
        fn invoke(&self, _ctx: &mut ExitContext) -> ExitAction {
            ExitAction::Fail(self.rc)
        }
    }

    /// An exit that returns Bypass.
    struct BypassExit {
        name: String,
    }

    impl Jes2Exit for BypassExit {
        fn name(&self) -> &str {
            &self.name
        }
        fn invoke(&self, _ctx: &mut ExitContext) -> ExitAction {
            ExitAction::Bypass
        }
    }

    /// An exit that modifies context (e.g., changes priority).
    struct PriorityBoostExit;

    impl Jes2Exit for PriorityBoostExit {
        fn name(&self) -> &str {
            "PRTYBOOST"
        }
        fn invoke(&self, ctx: &mut ExitContext) -> ExitAction {
            if let Some(ref mut p) = ctx.priority {
                *p = (*p).min(14) + 1; // boost priority by 1
                ctx.modified = true;
            }
            ExitAction::Continue
        }
    }

    /// EXIT7 security check — rejects jobs with certain JCL.
    struct SecurityJclExit;

    impl Jes2Exit for SecurityJclExit {
        fn name(&self) -> &str {
            "SECJCL"
        }
        fn invoke(&self, ctx: &mut ExitContext) -> ExitAction {
            for stmt in &ctx.jcl_statements {
                if stmt.contains("DISP=OLD,DSN=SYS1.PARMLIB") {
                    return ExitAction::Fail(8);
                }
            }
            ExitAction::Continue
        }
    }

    /// EXIT5 output rerouting exit.
    struct RerouteOutputExit;

    impl Jes2Exit for RerouteOutputExit {
        fn name(&self) -> &str {
            "REROUTE"
        }
        fn invoke(&self, ctx: &mut ExitContext) -> ExitAction {
            // Reroute class X output to HOLD.
            if ctx.sysout_class == Some('X') {
                ctx.destination = Some("HOLD".to_string());
                ctx.modified = true;
            }
            ExitAction::Continue
        }
    }

    /// EXIT15 job termination audit exit.
    struct AuditTerminationExit {
        log: Arc<std::sync::Mutex<Vec<String>>>,
    }

    impl Jes2Exit for AuditTerminationExit {
        fn name(&self) -> &str {
            "AUDTERM"
        }
        fn invoke(&self, ctx: &mut ExitContext) -> ExitAction {
            let entry = format!(
                "JOB={} RC={} USER={}",
                ctx.job_name.as_deref().unwrap_or("?"),
                ctx.return_code.unwrap_or(0),
                ctx.userid.as_deref().unwrap_or("?"),
            );
            self.log.lock().unwrap().push(entry);
            ExitAction::Continue
        }
    }

    /// EXIT44 spool access control exit.
    struct SpoolAccessExit;

    impl Jes2Exit for SpoolAccessExit {
        fn name(&self) -> &str {
            "SPOOLCHK"
        }
        fn invoke(&self, ctx: &mut ExitContext) -> ExitAction {
            // Only allow the job owner to access spool data.
            if ctx.userid.as_deref() == Some("UNAUTHORIZED") {
                return ExitAction::Fail(8);
            }
            ExitAction::Continue
        }
    }

    // ─────── SYS-108.1: Exit Framework Infrastructure ───────

    #[test]
    fn test_create_dispatcher() {
        let dispatcher = ExitDispatcher::new();
        assert_eq!(dispatcher.exit_count(), 0);
    }

    #[test]
    fn test_register_and_invoke_continue() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(
            EXIT1,
            Arc::new(ContinueExit {
                name: "CONT1".into(),
            }),
        );

        let mut ctx = ExitContext::job_selection("JOB00001", "TEST", 'A', 10, "USER1");
        let action = dispatcher.invoke(&mut ctx);
        assert_eq!(action, ExitAction::Continue);
    }

    #[test]
    fn test_register_and_invoke_fail() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(
            EXIT1,
            Arc::new(FailExit {
                name: "FAIL1".into(),
                rc: 8,
            }),
        );

        let mut ctx = ExitContext::job_selection("JOB00001", "TEST", 'A', 10, "USER1");
        let action = dispatcher.invoke(&mut ctx);
        assert_eq!(action, ExitAction::Fail(8));
    }

    #[test]
    fn test_register_and_invoke_bypass() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(
            EXIT1,
            Arc::new(BypassExit {
                name: "BYP1".into(),
            }),
        );

        let mut ctx = ExitContext::job_selection("JOB00001", "TEST", 'A', 10, "USER1");
        let action = dispatcher.invoke(&mut ctx);
        assert_eq!(action, ExitAction::Bypass);
    }

    #[test]
    fn test_chain_processing_continue_then_fail() {
        let mut dispatcher = ExitDispatcher::new();
        // First exit continues, second fails — chain should stop at second.
        dispatcher.register(
            EXIT1,
            Arc::new(ContinueExit {
                name: "CONT".into(),
            }),
        );
        dispatcher.register(
            EXIT1,
            Arc::new(FailExit {
                name: "FAIL".into(),
                rc: 12,
            }),
        );

        let mut ctx = ExitContext::job_selection("JOB00001", "TEST", 'A', 10, "USER1");
        let action = dispatcher.invoke(&mut ctx);
        assert_eq!(action, ExitAction::Fail(12));
    }

    #[test]
    fn test_chain_bypass_stops_chain() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(
            EXIT1,
            Arc::new(BypassExit {
                name: "BYP".into(),
            }),
        );
        dispatcher.register(
            EXIT1,
            Arc::new(FailExit {
                name: "NEVER".into(),
                rc: 99,
            }),
        );

        let mut ctx = ExitContext::job_selection("JOB00001", "TEST", 'A', 10, "USER1");
        let action = dispatcher.invoke(&mut ctx);
        assert_eq!(action, ExitAction::Bypass); // FailExit never reached
    }

    #[test]
    fn test_unregister_exit() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(
            EXIT1,
            Arc::new(FailExit {
                name: "MYFAIL".into(),
                rc: 4,
            }),
        );
        assert_eq!(dispatcher.routine_count(EXIT1), 1);

        assert!(dispatcher.unregister(EXIT1, "MYFAIL"));
        assert_eq!(dispatcher.routine_count(EXIT1), 0);

        // Invoke with no routines → Continue.
        let mut ctx = ExitContext::job_selection("JOB00001", "TEST", 'A', 10, "USER1");
        assert_eq!(dispatcher.invoke(&mut ctx), ExitAction::Continue);
    }

    #[test]
    fn test_unregister_nonexistent() {
        let mut dispatcher = ExitDispatcher::new();
        assert!(!dispatcher.unregister(EXIT1, "NOPE"));
    }

    #[test]
    fn test_no_exits_returns_continue() {
        let dispatcher = ExitDispatcher::new();
        let mut ctx = ExitContext::job_selection("JOB00001", "TEST", 'A', 10, "USER1");
        assert_eq!(dispatcher.invoke(&mut ctx), ExitAction::Continue);
    }

    #[test]
    fn test_exit_modifies_context() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(EXIT1, Arc::new(PriorityBoostExit));

        let mut ctx = ExitContext::job_selection("JOB00001", "TEST", 'A', 10, "USER1");
        dispatcher.invoke(&mut ctx);
        assert_eq!(ctx.priority, Some(11)); // boosted from 10 to 11
        assert!(ctx.modified);
    }

    // ─────── SYS-108.2: Job Selection Exits (EXIT1, EXIT2) ───────

    #[test]
    fn test_exit1_job_selection_context() {
        let ctx = ExitContext::job_selection("JOB00001", "PAYROLL", 'A', 15, "JSMITH");
        assert_eq!(ctx.exit_number, EXIT1);
        assert_eq!(ctx.job_id.as_deref(), Some("JOB00001"));
        assert_eq!(ctx.job_name.as_deref(), Some("PAYROLL"));
        assert_eq!(ctx.job_class, Some('A'));
        assert_eq!(ctx.priority, Some(15));
        assert_eq!(ctx.userid.as_deref(), Some("JSMITH"));
        assert_eq!(ctx.queue.as_deref(), Some("READY"));
    }

    #[test]
    fn test_exit1_bypass_skips_selection() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(
            EXIT1,
            Arc::new(BypassExit {
                name: "CUSTSEL".into(),
            }),
        );

        let mut ctx = ExitContext::job_selection("JOB00001", "TEST", 'A', 10, "USER1");
        let action = dispatcher.invoke(&mut ctx);
        assert_eq!(action, ExitAction::Bypass);
    }

    #[test]
    fn test_exit1_fail_rejects_job() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(
            EXIT1,
            Arc::new(FailExit {
                name: "REJECT".into(),
                rc: 8,
            }),
        );

        let mut ctx = ExitContext::job_selection("JOB00001", "BADJOB", 'A', 10, "USER1");
        let action = dispatcher.invoke(&mut ctx);
        assert_eq!(action, ExitAction::Fail(8));
    }

    #[test]
    fn test_exit2_queue_transition_context() {
        let ctx = ExitContext::queue_transition("JOB00001", "PAYROLL", "INPUT", "CONVERSION");
        assert_eq!(ctx.exit_number, EXIT2);
        assert_eq!(ctx.queue.as_deref(), Some("INPUT"));
        assert_eq!(ctx.target_queue.as_deref(), Some("CONVERSION"));
    }

    #[test]
    fn test_exit2_notification() {
        let log = Arc::new(std::sync::Mutex::new(Vec::<String>::new()));
        let log_clone = log.clone();

        struct TransitionLogger {
            log: Arc<std::sync::Mutex<Vec<String>>>,
        }
        impl Jes2Exit for TransitionLogger {
            fn name(&self) -> &str {
                "TRANSLOG"
            }
            fn invoke(&self, ctx: &mut ExitContext) -> ExitAction {
                let msg = format!(
                    "{}: {} -> {}",
                    ctx.job_id.as_deref().unwrap_or("?"),
                    ctx.queue.as_deref().unwrap_or("?"),
                    ctx.target_queue.as_deref().unwrap_or("?"),
                );
                self.log.lock().unwrap().push(msg);
                ExitAction::Continue
            }
        }

        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(EXIT2, Arc::new(TransitionLogger { log: log_clone }));

        let mut ctx = ExitContext::queue_transition("JOB00001", "PAYROLL", "READY", "RUNNING");
        dispatcher.invoke(&mut ctx);

        let entries = log.lock().unwrap();
        assert_eq!(entries.len(), 1);
        assert!(entries[0].contains("READY -> RUNNING"));
    }

    // ─────── SYS-108.3: JCL/Output Exits (EXIT5, EXIT6, EXIT7) ───────

    #[test]
    fn test_exit5_sysout_rerouting() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(EXIT5, Arc::new(RerouteOutputExit));

        // Class X output should be rerouted to HOLD.
        let mut ctx = ExitContext::sysout_selection("JOB00001", "TEST", 'X', None);
        dispatcher.invoke(&mut ctx);
        assert_eq!(ctx.destination.as_deref(), Some("HOLD"));
        assert!(ctx.modified);

        // Class A output should not be rerouted.
        let mut ctx2 = ExitContext::sysout_selection("JOB00001", "TEST", 'A', None);
        dispatcher.invoke(&mut ctx2);
        assert!(ctx2.destination.is_none());
        assert!(!ctx2.modified);
    }

    #[test]
    fn test_exit6_jcl_prescan() {
        let jcl = vec![
            "//JOB1 JOB (ACCT),'TEST',CLASS=A".to_string(),
            "//STEP1 EXEC PGM=IEFBR14".to_string(),
        ];

        let ctx = ExitContext::jcl_prescan("JOB00001", "JOB1", jcl.clone());
        assert_eq!(ctx.exit_number, EXIT6);
        assert_eq!(ctx.jcl_statements.len(), 2);
    }

    #[test]
    fn test_exit7_jcl_scan_security_reject() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(EXIT7, Arc::new(SecurityJclExit));

        // JCL accessing SYS1.PARMLIB should be rejected.
        let jcl = vec![
            "//STEP1 EXEC PGM=IEBGENER".to_string(),
            "//SYSUT1 DD DISP=OLD,DSN=SYS1.PARMLIB".to_string(),
        ];
        let mut ctx = ExitContext::jcl_scan("JOB00001", "BADJOB", "HACKER", jcl);
        let action = dispatcher.invoke(&mut ctx);
        assert_eq!(action, ExitAction::Fail(8));
    }

    #[test]
    fn test_exit7_jcl_scan_allowed() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(EXIT7, Arc::new(SecurityJclExit));

        // Normal JCL should pass.
        let jcl = vec![
            "//STEP1 EXEC PGM=IEFBR14".to_string(),
            "//SYSUT1 DD DSN=USER.DATA,DISP=SHR".to_string(),
        ];
        let mut ctx = ExitContext::jcl_scan("JOB00001", "GOODJOB", "USER1", jcl);
        let action = dispatcher.invoke(&mut ctx);
        assert_eq!(action, ExitAction::Continue);
    }

    // ─────── SYS-108.4: Security and Spool Exits (EXIT15, EXIT44) ───────

    #[test]
    fn test_exit15_job_termination_audit() {
        let log = Arc::new(std::sync::Mutex::new(Vec::<String>::new()));
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(
            EXIT15,
            Arc::new(AuditTerminationExit { log: log.clone() }),
        );

        let mut ctx = ExitContext::job_termination("JOB00001", "PAYROLL", "JSMITH", 0);
        dispatcher.invoke(&mut ctx);

        let mut ctx2 = ExitContext::job_termination("JOB00002", "REPORT", "JDOE", 8);
        dispatcher.invoke(&mut ctx2);

        let entries = log.lock().unwrap();
        assert_eq!(entries.len(), 2);
        assert!(entries[0].contains("PAYROLL"));
        assert!(entries[0].contains("RC=0"));
        assert!(entries[1].contains("REPORT"));
        assert!(entries[1].contains("RC=8"));
    }

    #[test]
    fn test_exit44_spool_access_allowed() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(EXIT44, Arc::new(SpoolAccessExit));

        let mut ctx = ExitContext::spool_access("JOB00001", "TEST", "JSMITH", 42);
        let action = dispatcher.invoke(&mut ctx);
        assert_eq!(action, ExitAction::Continue);
    }

    #[test]
    fn test_exit44_spool_access_denied() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(EXIT44, Arc::new(SpoolAccessExit));

        let mut ctx = ExitContext::spool_access("JOB00001", "TEST", "UNAUTHORIZED", 42);
        let action = dispatcher.invoke(&mut ctx);
        assert_eq!(action, ExitAction::Fail(8));
    }

    #[test]
    fn test_exit44_bypass_skips_access_check() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(
            EXIT44,
            Arc::new(BypassExit {
                name: "SPOOLBYP".into(),
            }),
        );

        let mut ctx = ExitContext::spool_access("JOB00001", "TEST", "ANYONE", 42);
        let action = dispatcher.invoke(&mut ctx);
        assert_eq!(action, ExitAction::Bypass);
    }

    // ─────── SYS-108.5: Exit Configuration and Loading ───────

    #[test]
    fn test_parse_exit_parm_basic() {
        let parm = parse_exit_parm("EXIT(7) ROUTINES=(SECJCL) STATUS=ENABLED TRACE=NO").unwrap();
        assert_eq!(parm.exit_number, 7);
        assert_eq!(parm.routine_name.as_deref(), Some("SECJCL"));
        assert!(parm.enabled);
        assert!(!parm.trace);
    }

    #[test]
    fn test_parse_exit_parm_disabled() {
        let parm = parse_exit_parm("EXIT(15) STATUS=DISABLED").unwrap();
        assert_eq!(parm.exit_number, 15);
        assert!(!parm.enabled);
        assert!(parm.routine_name.is_none());
    }

    #[test]
    fn test_parse_exit_parm_with_trace() {
        let parm = parse_exit_parm("EXIT(44) ROUTINES=(SPOOLCHK) TRACE=YES").unwrap();
        assert_eq!(parm.exit_number, 44);
        assert!(parm.trace);
    }

    #[test]
    fn test_parse_exit_parm_invalid() {
        assert!(parse_exit_parm("NOTANEXIT").is_none());
        assert!(parse_exit_parm("EXIT(0)").is_none());
        assert!(parse_exit_parm("EXIT(999)").is_none());
    }

    #[test]
    fn test_apply_exit_parm() {
        let mut dispatcher = ExitDispatcher::new();
        let parm = ExitParm {
            exit_number: 7,
            routine_name: Some("SECJCL".into()),
            enabled: true,
            trace: true,
        };
        dispatcher.apply_exit_parm(&parm);

        let display = dispatcher.display_exit(7);
        assert!(display.enabled);
        assert!(display.trace);
        assert!(display.routines.contains(&"SECJCL".to_string()));
    }

    #[test]
    fn test_enable_disable_exit() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(
            EXIT7,
            Arc::new(FailExit {
                name: "ALWAYSFAIL".into(),
                rc: 4,
            }),
        );

        // Disable the exit — should return Continue.
        dispatcher.disable(EXIT7);
        assert!(!dispatcher.is_enabled(EXIT7));

        let mut ctx = ExitContext::jcl_scan("JOB00001", "TEST", "USER1", vec![]);
        let action = dispatcher.invoke(&mut ctx);
        assert_eq!(action, ExitAction::Continue);

        // Re-enable — should return Fail.
        dispatcher.enable(EXIT7);
        assert!(dispatcher.is_enabled(EXIT7));

        let mut ctx2 = ExitContext::jcl_scan("JOB00001", "TEST", "USER1", vec![]);
        let action2 = dispatcher.invoke(&mut ctx2);
        assert_eq!(action2, ExitAction::Fail(4));
    }

    #[test]
    fn test_display_exit_command() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(
            EXIT7,
            Arc::new(ContinueExit {
                name: "JCLCHK".into(),
            }),
        );
        dispatcher.register(
            EXIT7,
            Arc::new(SecurityJclExit),
        );

        let display = dispatcher.display_exit(EXIT7);
        assert_eq!(display.exit_number, EXIT7);
        assert!(display.enabled);
        assert_eq!(display.routines.len(), 2);
        assert!(display.routines.contains(&"JCLCHK".to_string()));
        assert!(display.routines.contains(&"SECJCL".to_string()));

        // Verify display format.
        let text = format!("{display}");
        assert!(text.contains("EXIT(007)"));
        assert!(text.contains("STATUS=ENABLED"));
        assert!(text.contains("ROUTINES=(JCLCHK,SECJCL)"));
    }

    #[test]
    fn test_modify_exit_command() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(
            EXIT7,
            Arc::new(ContinueExit {
                name: "JCLCHK".into(),
            }),
        );

        // $T EXIT(7),STATUS=DISABLED,TRACE=YES
        let response = dispatcher.modify_exit(EXIT7, Some(false), Some(true));
        assert!(response.contains("STATUS=DISABLED"));
        assert!(response.contains("TRACE=YES"));

        assert!(!dispatcher.is_enabled(EXIT7));
        let display = dispatcher.display_exit(EXIT7);
        assert!(display.trace);
    }

    #[test]
    fn test_list_exits() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.register(EXIT1, Arc::new(ContinueExit { name: "SEL1".into() }));
        dispatcher.register(EXIT7, Arc::new(SecurityJclExit));
        dispatcher.register(EXIT15, Arc::new(AuditTerminationExit {
            log: Arc::new(std::sync::Mutex::new(Vec::new())),
        }));

        let exits = dispatcher.list_exits();
        assert_eq!(exits.len(), 3);

        let numbers: Vec<ExitNumber> = exits.iter().map(|e| e.exit_number).collect();
        assert!(numbers.contains(&EXIT1));
        assert!(numbers.contains(&EXIT7));
        assert!(numbers.contains(&EXIT15));
    }

    #[test]
    fn test_display_no_routines() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.enable(EXIT2); // Configure but no routines registered.

        let display = dispatcher.display_exit(EXIT2);
        assert!(display.routines.is_empty());
        let text = format!("{display}");
        assert!(text.contains("ROUTINES=NONE"));
    }

    #[test]
    fn test_exit_name_formatting() {
        assert_eq!(exit_name(1), "EXIT1");
        assert_eq!(exit_name(7), "EXIT7");
        assert_eq!(exit_name(44), "EXIT44");
        assert_eq!(exit_name(255), "EXIT255");
    }

    #[test]
    fn test_set_trace() {
        let mut dispatcher = ExitDispatcher::new();
        dispatcher.set_trace(EXIT1, true);
        let display = dispatcher.display_exit(EXIT1);
        assert!(display.trace);

        dispatcher.set_trace(EXIT1, false);
        let display = dispatcher.display_exit(EXIT1);
        assert!(!display.trace);
    }

    // ─────── SYS-108.6: Integration Tests (in-module) ───────

    #[test]
    fn test_integration_job_lifecycle_exits() {
        let mut dispatcher = ExitDispatcher::new();

        // Register exits at multiple lifecycle points.
        let audit_log = Arc::new(std::sync::Mutex::new(Vec::<String>::new()));

        // EXIT1 — priority boost for class A jobs.
        dispatcher.register(EXIT1, Arc::new(PriorityBoostExit));

        // EXIT7 — security scan.
        dispatcher.register(EXIT7, Arc::new(SecurityJclExit));

        // EXIT5 — reroute class X output.
        dispatcher.register(EXIT5, Arc::new(RerouteOutputExit));

        // EXIT15 — audit trail.
        dispatcher.register(
            EXIT15,
            Arc::new(AuditTerminationExit {
                log: audit_log.clone(),
            }),
        );

        // EXIT44 — spool access control.
        dispatcher.register(EXIT44, Arc::new(SpoolAccessExit));

        // Simulate job lifecycle:

        // 1. Job selection (EXIT1) — priority boosted.
        let mut ctx1 = ExitContext::job_selection("JOB00001", "PAYROLL", 'A', 10, "JSMITH");
        let action = dispatcher.invoke(&mut ctx1);
        assert_eq!(action, ExitAction::Continue);
        assert_eq!(ctx1.priority, Some(11)); // boosted

        // 2. JCL scan (EXIT7) — allowed.
        let jcl = vec!["//STEP1 EXEC PGM=PAYROLL".to_string()];
        let mut ctx7 = ExitContext::jcl_scan("JOB00001", "PAYROLL", "JSMITH", jcl);
        assert_eq!(dispatcher.invoke(&mut ctx7), ExitAction::Continue);

        // 3. Output routing (EXIT5) — class A not rerouted.
        let mut ctx5 = ExitContext::sysout_selection("JOB00001", "PAYROLL", 'A', None);
        dispatcher.invoke(&mut ctx5);
        assert!(ctx5.destination.is_none());

        // 4. Job termination (EXIT15) — audited.
        let mut ctx15 = ExitContext::job_termination("JOB00001", "PAYROLL", "JSMITH", 0);
        dispatcher.invoke(&mut ctx15);

        let entries = audit_log.lock().unwrap();
        assert_eq!(entries.len(), 1);
        assert!(entries[0].contains("PAYROLL"));
    }

    #[test]
    fn test_integration_exit_ordering() {
        let mut dispatcher = ExitDispatcher::new();
        let order = Arc::new(std::sync::Mutex::new(Vec::<String>::new()));

        struct OrderedExit {
            name: String,
            log: Arc<std::sync::Mutex<Vec<String>>>,
        }
        impl Jes2Exit for OrderedExit {
            fn name(&self) -> &str {
                &self.name
            }
            fn invoke(&self, _ctx: &mut ExitContext) -> ExitAction {
                self.log.lock().unwrap().push(self.name.clone());
                ExitAction::Continue
            }
        }

        // Register three exits in order.
        for name in ["FIRST", "SECOND", "THIRD"] {
            dispatcher.register(
                EXIT7,
                Arc::new(OrderedExit {
                    name: name.into(),
                    log: order.clone(),
                }),
            );
        }

        let mut ctx = ExitContext::jcl_scan("JOB00001", "TEST", "USER1", vec![]);
        dispatcher.invoke(&mut ctx);

        let log = order.lock().unwrap();
        assert_eq!(log.as_slice(), &["FIRST", "SECOND", "THIRD"]);
    }

    #[test]
    fn test_integration_multiple_exit_points() {
        let mut dispatcher = ExitDispatcher::new();
        let events = Arc::new(std::sync::Mutex::new(Vec::<String>::new()));

        struct EventLogger {
            name: String,
            log: Arc<std::sync::Mutex<Vec<String>>>,
        }
        impl Jes2Exit for EventLogger {
            fn name(&self) -> &str {
                &self.name
            }
            fn invoke(&self, ctx: &mut ExitContext) -> ExitAction {
                let msg = format!("EXIT{}:{}", ctx.exit_number, self.name);
                self.log.lock().unwrap().push(msg);
                ExitAction::Continue
            }
        }

        // Register at different exit points.
        for (num, name) in [
            (EXIT1, "JOBSEL"),
            (EXIT6, "JCLPRE"),
            (EXIT7, "JCLSCN"),
            (EXIT2, "QUEUETR"),
            (EXIT15, "JOBTERM"),
        ] {
            dispatcher.register(
                num,
                Arc::new(EventLogger {
                    name: name.into(),
                    log: events.clone(),
                }),
            );
        }

        // Simulate full lifecycle through exits.
        let mut ctx1 = ExitContext::job_selection("JOB00001", "TEST", 'A', 10, "USER1");
        dispatcher.invoke(&mut ctx1);

        let mut ctx6 = ExitContext::jcl_prescan("JOB00001", "TEST", vec![]);
        dispatcher.invoke(&mut ctx6);

        let mut ctx7 = ExitContext::jcl_scan("JOB00001", "TEST", "USER1", vec![]);
        dispatcher.invoke(&mut ctx7);

        let mut ctx2 = ExitContext::queue_transition("JOB00001", "TEST", "READY", "RUNNING");
        dispatcher.invoke(&mut ctx2);

        let mut ctx15 = ExitContext::job_termination("JOB00001", "TEST", "USER1", 0);
        dispatcher.invoke(&mut ctx15);

        let log = events.lock().unwrap();
        assert_eq!(log.len(), 5);
        assert_eq!(log[0], "EXIT1:JOBSEL");
        assert_eq!(log[1], "EXIT6:JCLPRE");
        assert_eq!(log[2], "EXIT7:JCLSCN");
        assert_eq!(log[3], "EXIT2:QUEUETR");
        assert_eq!(log[4], "EXIT15:JOBTERM");
    }
}
