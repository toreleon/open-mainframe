//! Signal Handling (USS-101).
//!
//! Provides POSIX signal infrastructure:
//! - Signal delivery with default actions
//! - sigaction() for handler registration
//! - sigprocmask() for signal blocking
//! - sigsuspend() for atomic wait
//! - kill() for sending signals

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors for signal operations.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, miette::Diagnostic)]
pub enum SignalError {
    /// Cannot catch or ignore SIGKILL/SIGSTOP.
    #[error("cannot catch or ignore signal {signal:?}")]
    UncatchableSignal { signal: Signal },

    /// No such process.
    #[error("no such process: pid {pid}")]
    NoSuchProcess { pid: u32 },

    /// Permission denied.
    #[error("permission denied: cannot signal pid {pid}")]
    PermissionDenied { pid: u32 },
}

// ---------------------------------------------------------------------------
//  Signal enum
// ---------------------------------------------------------------------------

/// POSIX signals.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Signal {
    SIGHUP = 1,
    SIGINT = 2,
    SIGQUIT = 3,
    SIGILL = 4,
    SIGABRT = 6,
    SIGFPE = 8,
    SIGKILL = 9,
    SIGSEGV = 11,
    SIGPIPE = 13,
    SIGALRM = 14,
    SIGTERM = 15,
    SIGUSR1 = 16,
    SIGUSR2 = 17,
    SIGCHLD = 18,
    SIGCONT = 19,
    SIGSTOP = 20,
    SIGTSTP = 21,
    SIGTTIN = 22,
    SIGTTOU = 23,
}

impl Signal {
    /// Return signal number.
    pub fn number(self) -> u32 {
        self as u32
    }

    /// Try to create a signal from a number.
    pub fn from_number(n: u32) -> Option<Self> {
        match n {
            1 => Some(Self::SIGHUP),
            2 => Some(Self::SIGINT),
            3 => Some(Self::SIGQUIT),
            4 => Some(Self::SIGILL),
            6 => Some(Self::SIGABRT),
            8 => Some(Self::SIGFPE),
            9 => Some(Self::SIGKILL),
            11 => Some(Self::SIGSEGV),
            13 => Some(Self::SIGPIPE),
            14 => Some(Self::SIGALRM),
            15 => Some(Self::SIGTERM),
            16 => Some(Self::SIGUSR1),
            17 => Some(Self::SIGUSR2),
            18 => Some(Self::SIGCHLD),
            19 => Some(Self::SIGCONT),
            20 => Some(Self::SIGSTOP),
            21 => Some(Self::SIGTSTP),
            22 => Some(Self::SIGTTIN),
            23 => Some(Self::SIGTTOU),
            _ => None,
        }
    }

    /// Whether this signal can be caught or ignored.
    pub fn is_catchable(self) -> bool {
        !matches!(self, Self::SIGKILL | Self::SIGSTOP)
    }

    /// Default action for this signal.
    pub fn default_action(self) -> DefaultAction {
        match self {
            Self::SIGHUP | Self::SIGINT | Self::SIGQUIT | Self::SIGILL | Self::SIGABRT
            | Self::SIGFPE | Self::SIGKILL | Self::SIGSEGV | Self::SIGPIPE
            | Self::SIGALRM | Self::SIGTERM | Self::SIGUSR1 | Self::SIGUSR2 => {
                DefaultAction::Terminate
            }
            Self::SIGSTOP | Self::SIGTSTP | Self::SIGTTIN | Self::SIGTTOU => {
                DefaultAction::Stop
            }
            Self::SIGCONT => DefaultAction::Continue,
            Self::SIGCHLD => DefaultAction::Ignore,
        }
    }
}

impl std::fmt::Display for Signal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

// ---------------------------------------------------------------------------
//  Default actions
// ---------------------------------------------------------------------------

/// Default action when no handler is installed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefaultAction {
    /// Terminate the process.
    Terminate,
    /// Stop the process.
    Stop,
    /// Continue the process.
    Continue,
    /// Ignore the signal.
    Ignore,
}

// ---------------------------------------------------------------------------
//  Signal Action
// ---------------------------------------------------------------------------

/// What to do when a signal is delivered (sigaction).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SignalAction {
    /// Use the default action.
    Default,
    /// Ignore the signal.
    Ignore,
    /// Call a handler (identified by name for simulation).
    Handler {
        name: String,
        flags: SignalActionFlags,
    },
}

/// Flags for sigaction.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SignalActionFlags {
    /// SA_RESTART: restart interrupted system calls.
    pub restart: bool,
    /// SA_NOCLDSTOP: don't generate SIGCHLD for stopped children.
    pub no_child_stop: bool,
}

// ---------------------------------------------------------------------------
//  Signal Set
// ---------------------------------------------------------------------------

/// A set of signals represented as a bitmask.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SignalSet(pub u64);

impl SignalSet {
    /// Empty signal set.
    pub fn empty() -> Self {
        Self(0)
    }

    /// Full signal set (all signals).
    pub fn full() -> Self {
        Self(u64::MAX)
    }

    /// Add a signal to the set.
    pub fn add(&mut self, sig: Signal) {
        self.0 |= 1 << sig.number();
    }

    /// Remove a signal from the set.
    pub fn remove(&mut self, sig: Signal) {
        self.0 &= !(1 << sig.number());
    }

    /// Check if a signal is in the set.
    pub fn contains(&self, sig: Signal) -> bool {
        (self.0 & (1 << sig.number())) != 0
    }

    /// Union of two sets.
    pub fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    /// Intersection of two sets.
    pub fn intersection(self, other: Self) -> Self {
        Self(self.0 & other.0)
    }
}

// ---------------------------------------------------------------------------
//  Signal State (per-process)
// ---------------------------------------------------------------------------

/// Per-process signal state.
#[derive(Debug, Clone)]
pub struct SignalState {
    /// Signal mask (blocked signals).
    pub mask: SignalSet,
    /// Registered signal handlers.
    pub handlers: HashMap<Signal, SignalAction>,
    /// Pending signals (delivered but blocked).
    pub pending: SignalSet,
}

impl Default for SignalState {
    fn default() -> Self {
        Self::new()
    }
}

impl SignalState {
    /// Create a new signal state with defaults.
    pub fn new() -> Self {
        Self {
            mask: SignalSet::empty(),
            handlers: HashMap::new(),
            pending: SignalSet::empty(),
        }
    }

    /// sigaction() — register a signal handler.
    pub fn sigaction(
        &mut self,
        sig: Signal,
        action: SignalAction,
    ) -> Result<Option<SignalAction>, SignalError> {
        if !sig.is_catchable() {
            return Err(SignalError::UncatchableSignal { signal: sig });
        }
        let old = self.handlers.insert(sig, action);
        Ok(old)
    }

    /// Get the current action for a signal.
    pub fn get_action(&self, sig: Signal) -> &SignalAction {
        self.handlers.get(&sig).unwrap_or(&SignalAction::Default)
    }

    /// sigprocmask(SIG_BLOCK) — add signals to mask.
    pub fn block_signals(&mut self, set: &SignalSet) -> SignalSet {
        let old_mask = self.mask;
        self.mask = self.mask.union(*set);
        old_mask
    }

    /// sigprocmask(SIG_UNBLOCK) — remove signals from mask.
    pub fn unblock_signals(&mut self, set: &SignalSet) -> SignalSet {
        let old_mask = self.mask;
        // Remove the signals in `set` from the mask.
        self.mask.0 &= !set.0;
        old_mask
    }

    /// sigprocmask(SIG_SETMASK) — replace signal mask.
    pub fn set_signal_mask(&mut self, new_mask: SignalSet) -> SignalSet {
        let old_mask = self.mask;
        self.mask = new_mask;
        old_mask
    }

    /// Deliver a signal to this process.
    /// Returns the action that should be taken.
    pub fn deliver_signal(&mut self, sig: Signal) -> SignalDeliveryResult {
        // SIGKILL and SIGSTOP cannot be blocked, caught, or ignored.
        if !sig.is_catchable() {
            return match sig.default_action() {
                DefaultAction::Terminate => SignalDeliveryResult::Terminate,
                DefaultAction::Stop => SignalDeliveryResult::Stop,
                _ => SignalDeliveryResult::Terminate,
            };
        }

        // Is signal blocked?
        if self.mask.contains(sig) {
            self.pending.add(sig);
            return SignalDeliveryResult::Blocked;
        }

        // Look up handler.
        match self.get_action(sig).clone() {
            SignalAction::Default => match sig.default_action() {
                DefaultAction::Terminate => SignalDeliveryResult::Terminate,
                DefaultAction::Stop => SignalDeliveryResult::Stop,
                DefaultAction::Continue => SignalDeliveryResult::Continue,
                DefaultAction::Ignore => SignalDeliveryResult::Ignored,
            },
            SignalAction::Ignore => SignalDeliveryResult::Ignored,
            SignalAction::Handler { name, flags } => {
                SignalDeliveryResult::HandlerCalled { name, flags }
            }
        }
    }

    /// Check for pending signals that are now unblocked and return them.
    pub fn check_pending(&mut self) -> Vec<Signal> {
        let mut delivered = Vec::new();
        for num in 1..=23 {
            if let Some(sig) = Signal::from_number(num) {
                if self.pending.contains(sig) && !self.mask.contains(sig) {
                    self.pending.remove(sig);
                    delivered.push(sig);
                }
            }
        }
        delivered
    }

    /// sigsuspend() — atomically set mask and check for pending signals.
    /// Returns signals that would be delivered with the temporary mask.
    pub fn sigsuspend(&mut self, temp_mask: SignalSet) -> Vec<Signal> {
        let saved_mask = self.mask;
        self.mask = temp_mask;
        let delivered = self.check_pending();
        self.mask = saved_mask;
        delivered
    }
}

// ---------------------------------------------------------------------------
//  Signal Delivery Result
// ---------------------------------------------------------------------------

/// Result of delivering a signal.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SignalDeliveryResult {
    /// Process should be terminated (default action for SIGTERM, etc.).
    Terminate,
    /// Process should be stopped (SIGSTOP, SIGTSTP).
    Stop,
    /// Process should continue (SIGCONT).
    Continue,
    /// Signal was ignored.
    Ignored,
    /// Signal was blocked and added to pending set.
    Blocked,
    /// A user-defined handler was called.
    HandlerCalled {
        name: String,
        flags: SignalActionFlags,
    },
}

// ---------------------------------------------------------------------------
//  Kill target
// ---------------------------------------------------------------------------

/// Target for kill().
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KillTarget {
    /// Send to a specific process.
    Process(u32),
    /// Send to all processes in a process group.
    ProcessGroup(u32),
    /// Send to all processes (pid = -1).
    All,
}

impl KillTarget {
    /// Parse a pid argument to kill target.
    pub fn from_pid(pid: i32) -> Self {
        if pid > 0 {
            Self::Process(pid as u32)
        } else if pid == 0 {
            Self::ProcessGroup(0) // Caller's process group.
        } else if pid == -1 {
            Self::All
        } else {
            Self::ProcessGroup((-pid) as u32)
        }
    }
}

// ===========================================================================
//  Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_signal_numbers() {
        assert_eq!(Signal::SIGHUP.number(), 1);
        assert_eq!(Signal::SIGKILL.number(), 9);
        assert_eq!(Signal::SIGTERM.number(), 15);
    }

    #[test]
    fn test_signal_from_number() {
        assert_eq!(Signal::from_number(9), Some(Signal::SIGKILL));
        assert_eq!(Signal::from_number(99), None);
    }

    #[test]
    fn test_signal_catchable() {
        assert!(Signal::SIGTERM.is_catchable());
        assert!(Signal::SIGINT.is_catchable());
        assert!(!Signal::SIGKILL.is_catchable());
        assert!(!Signal::SIGSTOP.is_catchable());
    }

    #[test]
    fn test_default_actions() {
        assert_eq!(Signal::SIGTERM.default_action(), DefaultAction::Terminate);
        assert_eq!(Signal::SIGKILL.default_action(), DefaultAction::Terminate);
        assert_eq!(Signal::SIGSTOP.default_action(), DefaultAction::Stop);
        assert_eq!(Signal::SIGCONT.default_action(), DefaultAction::Continue);
        assert_eq!(Signal::SIGCHLD.default_action(), DefaultAction::Ignore);
    }

    #[test]
    fn test_signal_set_operations() {
        let mut set = SignalSet::empty();
        assert!(!set.contains(Signal::SIGINT));
        set.add(Signal::SIGINT);
        assert!(set.contains(Signal::SIGINT));
        set.remove(Signal::SIGINT);
        assert!(!set.contains(Signal::SIGINT));
    }

    #[test]
    fn test_signal_set_union_intersection() {
        let mut a = SignalSet::empty();
        a.add(Signal::SIGINT);
        a.add(Signal::SIGTERM);

        let mut b = SignalSet::empty();
        b.add(Signal::SIGTERM);
        b.add(Signal::SIGHUP);

        let union = a.union(b);
        assert!(union.contains(Signal::SIGINT));
        assert!(union.contains(Signal::SIGTERM));
        assert!(union.contains(Signal::SIGHUP));

        let inter = a.intersection(b);
        assert!(!inter.contains(Signal::SIGINT));
        assert!(inter.contains(Signal::SIGTERM));
        assert!(!inter.contains(Signal::SIGHUP));
    }

    #[test]
    fn test_deliver_sigterm_default_terminates() {
        let mut state = SignalState::new();
        let result = state.deliver_signal(Signal::SIGTERM);
        assert_eq!(result, SignalDeliveryResult::Terminate);
    }

    #[test]
    fn test_deliver_sigkill_always_terminates() {
        let mut state = SignalState::new();
        // Even with a handler registered, SIGKILL should terminate.
        // (sigaction for SIGKILL should fail, but delivery always terminates.)
        let result = state.deliver_signal(Signal::SIGKILL);
        assert_eq!(result, SignalDeliveryResult::Terminate);
    }

    #[test]
    fn test_deliver_sigchld_default_ignored() {
        let mut state = SignalState::new();
        let result = state.deliver_signal(Signal::SIGCHLD);
        assert_eq!(result, SignalDeliveryResult::Ignored);
    }

    #[test]
    fn test_sigaction_registers_handler() {
        let mut state = SignalState::new();
        let action = SignalAction::Handler {
            name: "my_handler".to_string(),
            flags: SignalActionFlags::default(),
        };
        let old = state.sigaction(Signal::SIGINT, action).unwrap();
        assert!(old.is_none());

        let result = state.deliver_signal(Signal::SIGINT);
        assert!(matches!(
            result,
            SignalDeliveryResult::HandlerCalled { ref name, .. } if name == "my_handler"
        ));
    }

    #[test]
    fn test_sigaction_sa_restart() {
        let mut state = SignalState::new();
        let flags = SignalActionFlags {
            restart: true,
            no_child_stop: false,
        };
        let action = SignalAction::Handler {
            name: "restart_handler".to_string(),
            flags,
        };
        state.sigaction(Signal::SIGINT, action).unwrap();
        let result = state.deliver_signal(Signal::SIGINT);
        match result {
            SignalDeliveryResult::HandlerCalled { flags, .. } => {
                assert!(flags.restart);
            }
            _ => panic!("expected handler called"),
        }
    }

    #[test]
    fn test_sigaction_on_sigkill_fails() {
        let mut state = SignalState::new();
        let err = state
            .sigaction(Signal::SIGKILL, SignalAction::Ignore)
            .unwrap_err();
        assert!(matches!(err, SignalError::UncatchableSignal { .. }));
    }

    #[test]
    fn test_sigprocmask_block_and_deliver() {
        let mut state = SignalState::new();
        let mut block_set = SignalSet::empty();
        block_set.add(Signal::SIGINT);
        state.block_signals(&block_set);

        // SIGINT should be blocked.
        let result = state.deliver_signal(Signal::SIGINT);
        assert_eq!(result, SignalDeliveryResult::Blocked);
        assert!(state.pending.contains(Signal::SIGINT));
    }

    #[test]
    fn test_sigprocmask_unblock_delivers_pending() {
        let mut state = SignalState::new();
        let mut block_set = SignalSet::empty();
        block_set.add(Signal::SIGINT);
        state.block_signals(&block_set);
        state.deliver_signal(Signal::SIGINT);

        // Unblock SIGINT.
        state.unblock_signals(&block_set);
        let pending = state.check_pending();
        assert_eq!(pending, vec![Signal::SIGINT]);
    }

    #[test]
    fn test_sigsuspend() {
        let mut state = SignalState::new();
        // Block SIGINT initially.
        let mut block_set = SignalSet::empty();
        block_set.add(Signal::SIGINT);
        state.block_signals(&block_set);
        // Deliver SIGINT — it becomes pending.
        state.deliver_signal(Signal::SIGINT);

        // sigsuspend with empty mask — SIGINT should be delivered.
        let delivered = state.sigsuspend(SignalSet::empty());
        assert_eq!(delivered, vec![Signal::SIGINT]);
    }

    #[test]
    fn test_kill_target_parsing() {
        assert_eq!(KillTarget::from_pid(42), KillTarget::Process(42));
        assert_eq!(KillTarget::from_pid(0), KillTarget::ProcessGroup(0));
        assert_eq!(KillTarget::from_pid(-1), KillTarget::All);
        assert_eq!(KillTarget::from_pid(-10), KillTarget::ProcessGroup(10));
    }

    #[test]
    fn test_signal_ignore_action() {
        let mut state = SignalState::new();
        state.sigaction(Signal::SIGINT, SignalAction::Ignore).unwrap();
        let result = state.deliver_signal(Signal::SIGINT);
        assert_eq!(result, SignalDeliveryResult::Ignored);
    }

    #[test]
    fn test_signal_display() {
        assert_eq!(Signal::SIGKILL.to_string(), "SIGKILL");
        assert_eq!(Signal::SIGTERM.to_string(), "SIGTERM");
    }
}
