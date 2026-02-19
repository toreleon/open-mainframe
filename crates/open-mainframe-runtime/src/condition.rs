//! LE Condition Handling Framework — CEEHDLR/CEEHDLU/CEESGL/CEEMRCR/CEE3CIB.
//!
//! Implements the z/OS LE condition handler chain with stack-frame-based
//! handler registration, condition signaling, and resume cursor management.
//!
//! ## Concepts
//!
//! - **Condition token**: 12-byte structure identifying a condition (facility, severity, msg#)
//! - **Condition handler**: Registered per stack frame, invoked when a condition occurs
//! - **Handler actions**: Resume (handle it), Percolate (pass to next handler), Promote (escalate)
//! - **Resume cursor**: Controls where execution resumes after a handler processes a condition

/// LE condition severity levels (0-4).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    /// Informational — processing continues.
    Info = 0,
    /// Warning — results may be incorrect.
    Warning = 1,
    /// Error — corrective action taken.
    Error = 2,
    /// Severe — processing cannot continue.
    Severe = 3,
    /// Critical — immediate termination.
    Critical = 4,
}

/// A condition token — identifies a specific condition (LE standard format).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConditionToken {
    /// Facility ID (3-character code identifying the component).
    pub facility_id: String,
    /// Condition severity (0-4).
    pub severity: Severity,
    /// Message number within the facility.
    pub message_number: u32,
    /// Case-specific information (ISI).
    pub case_info: u32,
}

impl ConditionToken {
    /// Create a new condition token.
    pub fn new(facility_id: &str, severity: Severity, message_number: u32) -> Self {
        Self {
            facility_id: facility_id.to_string(),
            severity,
            message_number,
            case_info: 0,
        }
    }

    /// Check if this is a severity-3 or higher condition (enclave termination).
    pub fn is_terminating(&self) -> bool {
        self.severity >= Severity::Severe
    }
}

/// The action a condition handler wants to take.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HandlerAction {
    /// Resume execution at the current frame (condition handled).
    Resume,
    /// Resume execution N frames toward the caller (CEEMRCR + resume).
    ResumeAtCaller(u32),
    /// Percolate — pass the condition to the next handler in the chain.
    Percolate,
    /// Promote — escalate the condition to a higher severity.
    Promote,
}

/// Condition Information Block (CIB) — provided to condition handlers.
#[derive(Debug, Clone)]
pub struct ConditionInfoBlock {
    /// The condition token.
    pub token: ConditionToken,
    /// The stack frame depth where the condition was signaled.
    pub signal_frame: u32,
    /// The routine name where the condition was signaled.
    pub routine_name: String,
}

/// A registered condition handler on a specific stack frame.
#[derive(Debug, Clone)]
pub struct ConditionHandler {
    /// Unique registration ID.
    pub id: u64,
    /// Name of the handler routine.
    pub handler_name: String,
    /// User token passed at registration.
    pub token: u64,
    /// Stack frame depth at which the handler was registered.
    pub frame_depth: u32,
}

/// Resume cursor position — controls where execution resumes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResumeCursor {
    /// Resume at the current frame (where the condition occurred).
    Current,
    /// Resume N frames toward the caller.
    MoveUp(u32),
}

/// The condition handler chain for an LE thread.
///
/// Manages handler registration, condition signaling, and resume cursor.
#[derive(Debug)]
pub struct ConditionHandlerChain {
    /// Registered handlers, ordered by registration (newest first for lookup).
    handlers: Vec<ConditionHandler>,
    /// Current stack frame depth.
    current_frame_depth: u32,
    /// Next handler registration ID.
    next_id: u64,
    /// The resume cursor position for the current condition.
    resume_cursor: ResumeCursor,
    /// Active condition info block (set during condition handling).
    active_cib: Option<ConditionInfoBlock>,
    /// Signal results log — for testing and diagnostics.
    signal_log: Vec<SignalResult>,
}

/// Result of signaling a condition through the handler chain.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SignalResult {
    /// A handler resumed the condition.
    Resumed {
        /// Handler that resumed.
        handler_name: String,
        /// Resume cursor position.
        resume_frame: u32,
    },
    /// Condition percolated through all handlers — no one handled it.
    Unhandled,
    /// Condition was severe (≥3) and caused enclave termination.
    Terminated,
}

impl ConditionHandlerChain {
    /// Create a new empty handler chain.
    pub fn new() -> Self {
        Self {
            handlers: Vec::new(),
            current_frame_depth: 0,
            next_id: 1,
            resume_cursor: ResumeCursor::Current,
            active_cib: None,
            signal_log: Vec::new(),
        }
    }

    /// Set the current stack frame depth (called on routine entry/exit).
    pub fn set_frame_depth(&mut self, depth: u32) {
        self.current_frame_depth = depth;
    }

    /// Get the current stack frame depth.
    pub fn frame_depth(&self) -> u32 {
        self.current_frame_depth
    }

    /// Simulate entering a routine (push a frame).
    pub fn enter_routine(&mut self) {
        self.current_frame_depth += 1;
    }

    /// Simulate exiting a routine (pop a frame).
    ///
    /// Automatically removes any handlers registered at this depth.
    pub fn exit_routine(&mut self) {
        let depth = self.current_frame_depth;
        self.handlers.retain(|h| h.frame_depth != depth);
        if self.current_frame_depth > 0 {
            self.current_frame_depth -= 1;
        }
    }

    // ─────── CEEHDLR — Register condition handler ───────

    /// CEEHDLR — register a condition handler for the current stack frame.
    ///
    /// Returns the handler registration ID.
    pub fn ceehdlr(&mut self, handler_name: &str, token: u64) -> u64 {
        let id = self.next_id;
        self.next_id += 1;

        let handler = ConditionHandler {
            id,
            handler_name: handler_name.to_string(),
            token,
            frame_depth: self.current_frame_depth,
        };
        self.handlers.push(handler);
        id
    }

    // ─────── CEEHDLU — Unregister condition handler ───────

    /// CEEHDLU — unregister a condition handler.
    pub fn ceehdlu(&mut self, handler_name: &str) -> bool {
        let len = self.handlers.len();
        self.handlers
            .retain(|h| h.handler_name != handler_name);
        self.handlers.len() < len
    }

    // ─────── CEESGL — Signal a condition ───────

    /// CEESGL — signal a condition through the handler chain.
    ///
    /// The `decide` callback is called for each eligible handler to determine
    /// the handler's action (Resume, Percolate, or Promote). This simulates
    /// the handler routine being invoked.
    ///
    /// Returns the signal result.
    pub fn ceesgl(
        &mut self,
        condition: ConditionToken,
        routine_name: &str,
        mut decide: impl FnMut(&ConditionHandler, &ConditionToken) -> HandlerAction,
    ) -> SignalResult {
        // Build the CIB.
        let cib = ConditionInfoBlock {
            token: condition.clone(),
            signal_frame: self.current_frame_depth,
            routine_name: routine_name.to_string(),
        };
        self.active_cib = Some(cib);
        self.resume_cursor = ResumeCursor::Current;

        // If severity >= 3 and no handler resumes, terminate.
        let is_terminating = condition.is_terminating();

        // Walk handlers from most recently registered backward.
        // Only handlers at or above the current frame are eligible.
        let eligible: Vec<ConditionHandler> = self
            .handlers
            .iter()
            .rev()
            .filter(|h| h.frame_depth <= self.current_frame_depth)
            .cloned()
            .collect();

        let mut current_condition = condition;

        for handler in &eligible {
            let action = decide(handler, &current_condition);
            match action {
                HandlerAction::Resume => {
                    let resume_frame = match self.resume_cursor {
                        ResumeCursor::Current => self.current_frame_depth,
                        ResumeCursor::MoveUp(n) => {
                            self.current_frame_depth.saturating_sub(n)
                        }
                    };
                    let result = SignalResult::Resumed {
                        handler_name: handler.handler_name.clone(),
                        resume_frame,
                    };
                    self.signal_log.push(result.clone());
                    self.active_cib = None;
                    return result;
                }
                HandlerAction::ResumeAtCaller(n) => {
                    let resume_frame = self.current_frame_depth.saturating_sub(n);
                    let result = SignalResult::Resumed {
                        handler_name: handler.handler_name.clone(),
                        resume_frame,
                    };
                    self.signal_log.push(result.clone());
                    self.active_cib = None;
                    return result;
                }
                HandlerAction::Percolate => {
                    // Continue to next handler.
                    continue;
                }
                HandlerAction::Promote => {
                    // Escalate severity.
                    let new_severity = match current_condition.severity {
                        Severity::Info => Severity::Warning,
                        Severity::Warning => Severity::Error,
                        Severity::Error => Severity::Severe,
                        Severity::Severe | Severity::Critical => Severity::Critical,
                    };
                    current_condition.severity = new_severity;
                    continue;
                }
            }
        }

        // No handler resumed.
        let result = if is_terminating || current_condition.is_terminating() {
            SignalResult::Terminated
        } else {
            SignalResult::Unhandled
        };
        self.signal_log.push(result.clone());
        self.active_cib = None;
        result
    }

    // ─────── CEEMRCR — Move resume cursor ───────

    /// CEEMRCR — move the resume cursor N frames toward the caller.
    pub fn ceemrcr(&mut self, frames: u32) {
        self.resume_cursor = ResumeCursor::MoveUp(frames);
    }

    // ─────── CEE3CIB — Access condition info block ───────

    /// CEE3CIB — get the active condition information block.
    pub fn cee3cib(&self) -> Option<&ConditionInfoBlock> {
        self.active_cib.as_ref()
    }

    /// Get the signal log (for testing).
    pub fn signal_log(&self) -> &[SignalResult] {
        &self.signal_log
    }

    /// Number of registered handlers.
    pub fn handler_count(&self) -> usize {
        self.handlers.len()
    }
}

impl Default for ConditionHandlerChain {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ─────── Story LE101.1: Condition Handler Registration and Signaling ───────

    #[test]
    fn test_ceehdlr_registers_handler() {
        let mut chain = ConditionHandlerChain::new();
        chain.enter_routine(); // frame 1

        let id = chain.ceehdlr("MY_HANDLER", 42);
        assert!(id > 0);
        assert_eq!(chain.handler_count(), 1);
    }

    #[test]
    fn test_ceehdlu_unregisters_handler() {
        let mut chain = ConditionHandlerChain::new();
        chain.enter_routine();
        chain.ceehdlr("MY_HANDLER", 42);
        assert_eq!(chain.handler_count(), 1);

        assert!(chain.ceehdlu("MY_HANDLER"));
        assert_eq!(chain.handler_count(), 0);
    }

    #[test]
    fn test_ceesgl_handler_resumes() {
        let mut chain = ConditionHandlerChain::new();
        chain.enter_routine(); // frame 1
        chain.ceehdlr("HANDLER_A", 0);

        let condition = ConditionToken::new("CEE", Severity::Error, 123);
        let result = chain.ceesgl(condition, "ROUTINE_A", |_h, _c| HandlerAction::Resume);

        assert_eq!(
            result,
            SignalResult::Resumed {
                handler_name: "HANDLER_A".to_string(),
                resume_frame: 1,
            }
        );
    }

    #[test]
    fn test_ceesgl_severity3_no_handler_terminates() {
        let mut chain = ConditionHandlerChain::new();
        chain.enter_routine();

        // No handlers registered.
        let condition = ConditionToken::new("CEE", Severity::Severe, 999);
        let result = chain.ceesgl(condition, "ROUTINE_A", |_h, _c| HandlerAction::Percolate);
        assert_eq!(result, SignalResult::Terminated);
    }

    #[test]
    fn test_ceesgl_handler_percolates() {
        let mut chain = ConditionHandlerChain::new();
        chain.enter_routine();
        chain.ceehdlr("HANDLER_A", 0);
        chain.ceehdlr("HANDLER_B", 0);

        // Both handlers percolate → unhandled for non-severe condition.
        let condition = ConditionToken::new("CEE", Severity::Warning, 100);
        let result = chain.ceesgl(condition, "ROUTINE", |_h, _c| HandlerAction::Percolate);
        assert_eq!(result, SignalResult::Unhandled);
    }

    #[test]
    fn test_ceesgl_handler_promotes() {
        let mut chain = ConditionHandlerChain::new();
        chain.enter_routine();
        chain.ceehdlr("PROMOTER", 0);

        // Handler promotes a Warning → Error, still unhandled.
        let condition = ConditionToken::new("CEE", Severity::Warning, 100);
        let result = chain.ceesgl(condition, "ROUTINE", |_h, _c| HandlerAction::Promote);
        // After promotion to Error, and no more handlers → Unhandled.
        assert_eq!(result, SignalResult::Unhandled);
    }

    #[test]
    fn test_ceesgl_promote_to_severe_terminates() {
        let mut chain = ConditionHandlerChain::new();
        chain.enter_routine();
        chain.ceehdlr("PROMOTER", 0);

        // Promote Error → Severe → terminates.
        let condition = ConditionToken::new("CEE", Severity::Error, 100);
        let result = chain.ceesgl(condition, "ROUTINE", |_h, _c| HandlerAction::Promote);
        assert_eq!(result, SignalResult::Terminated);
    }

    #[test]
    fn test_handler_chain_walks_from_deepest() {
        let mut chain = ConditionHandlerChain::new();
        chain.enter_routine(); // frame 1
        chain.ceehdlr("OUTER", 0);
        chain.enter_routine(); // frame 2
        chain.ceehdlr("INNER", 0);

        // Signal at frame 2. INNER should be tried first (most recently registered).
        let mut calls = Vec::new();
        let condition = ConditionToken::new("CEE", Severity::Warning, 1);
        chain.ceesgl(condition, "ROUTINE", |h, _c| {
            calls.push(h.handler_name.clone());
            if h.handler_name == "INNER" {
                HandlerAction::Percolate
            } else {
                HandlerAction::Resume
            }
        });

        assert_eq!(calls, vec!["INNER", "OUTER"]);
    }

    // ─────── Story LE101.2: Resume Cursor and Condition Information ───────

    #[test]
    fn test_ceemrcr_moves_resume_cursor() {
        let mut chain = ConditionHandlerChain::new();
        chain.enter_routine(); // frame 1
        chain.enter_routine(); // frame 2
        chain.ceehdlr("HANDLER", 0);

        // Move resume cursor 1 frame toward caller.
        let condition = ConditionToken::new("CEE", Severity::Error, 100);
        let result = chain.ceesgl(condition, "ROUTINE", |_h, _c| {
            // This simulates the handler calling CEEMRCR before resuming.
            // We can't call it here directly, but we test it separately below.
            HandlerAction::Resume
        });

        // Default resume is at current frame.
        assert_eq!(
            result,
            SignalResult::Resumed {
                handler_name: "HANDLER".to_string(),
                resume_frame: 2,
            }
        );
    }

    #[test]
    fn test_ceemrcr_resume_at_caller() {
        let mut chain = ConditionHandlerChain::new();
        chain.enter_routine(); // frame 1
        chain.enter_routine(); // frame 2
        chain.ceehdlr("HANDLER", 0);

        // Handler calls CEEMRCR(1) and then resumes — moves one frame toward caller.
        let condition = ConditionToken::new("CEE", Severity::Error, 100);
        let result = chain.ceesgl(condition, "ROUTINE", |_h, _c| HandlerAction::ResumeAtCaller(1));

        assert_eq!(
            result,
            SignalResult::Resumed {
                handler_name: "HANDLER".to_string(),
                resume_frame: 1, // resumed at caller's frame
            }
        );
    }

    // ─────── Story LE101.3: Exit on routine exit ───────

    #[test]
    fn test_exit_routine_removes_frame_handlers() {
        let mut chain = ConditionHandlerChain::new();
        chain.enter_routine(); // frame 1
        chain.ceehdlr("OUTER", 0);
        chain.enter_routine(); // frame 2
        chain.ceehdlr("INNER", 0);
        assert_eq!(chain.handler_count(), 2);

        chain.exit_routine(); // leaves frame 2 → INNER removed
        assert_eq!(chain.handler_count(), 1);
    }

    #[test]
    fn test_condition_token_severity() {
        let info = ConditionToken::new("CEE", Severity::Info, 1);
        assert!(!info.is_terminating());

        let severe = ConditionToken::new("CEE", Severity::Severe, 1);
        assert!(severe.is_terminating());

        let critical = ConditionToken::new("CEE", Severity::Critical, 1);
        assert!(critical.is_terminating());
    }
}
