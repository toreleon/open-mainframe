//! RACF Exits — customization hook infrastructure.
//!
//! Implements the z/OS RACF exit points:
//! - **ICHRTX00** — RACROUTE REQUEST=AUTH pre-processing exit
//! - **ICHRCX01** — SAF callable services exit
//! - **ICHRCX02** — RACROUTE REQUEST=VERIFY pre-processing exit
//! - **ICHPWX01** — Password quality exit
//! - **ICHPWX11** — New password phrase exit
//! - **IRREVX01** — Event notification exit
//!
//! On z/OS, these are installation-supplied load modules. In OpenMainframe,
//! they are implemented as Rust closures/trait objects registered with the
//! `ExitManager`.

use std::collections::BTreeMap;

use crate::types::AccessLevel;

// ---------------------------------------------------------------------------
//  Exit action
// ---------------------------------------------------------------------------

/// The action an exit instructs the system to take.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExitAction {
    /// Continue with normal RACF processing.
    Continue,
    /// Allow the request (bypass RACF).
    Allow,
    /// Deny the request.
    Deny,
}

// ---------------------------------------------------------------------------
//  Exit context structures
// ---------------------------------------------------------------------------

/// Context passed to the ICHRTX00 (pre-authorization) exit.
#[derive(Debug, Clone)]
pub struct AuthExitContext {
    /// The resource class being checked.
    pub class: String,
    /// The resource entity name.
    pub entity: String,
    /// The userid requesting access.
    pub userid: String,
    /// The access level requested.
    pub access: AccessLevel,
}

/// Context passed to the ICHRCX02 (pre-verify) exit.
#[derive(Debug, Clone)]
pub struct VerifyExitContext {
    /// The userid being verified.
    pub userid: String,
    /// Whether a password was provided.
    pub has_password: bool,
    /// Whether a PassTicket was provided.
    pub has_passticket: bool,
    /// The application name (if any).
    pub application: Option<String>,
}

/// Context passed to the ICHPWX01 (password quality) exit.
#[derive(Debug, Clone)]
pub struct PasswordExitContext {
    /// The userid changing password.
    pub userid: String,
    /// The new password (for quality checking).
    pub new_password: String,
}

/// Password exit result.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PasswordExitResult {
    /// Password meets quality requirements.
    Accept,
    /// Password rejected with a reason.
    Reject(String),
}

/// Context for event notification (IRREVX01).
#[derive(Debug, Clone)]
pub struct EventContext {
    /// Event type.
    pub event_type: EventType,
    /// Userid associated with the event.
    pub userid: String,
    /// Additional details.
    pub details: String,
}

/// Types of security events.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EventType {
    /// Successful authentication.
    AuthSuccess,
    /// Failed authentication.
    AuthFailure,
    /// Password changed.
    PasswordChange,
    /// User revoked due to failed attempts.
    UserRevoked,
    /// Profile created.
    ProfileCreated,
    /// Profile deleted.
    ProfileDeleted,
    /// Profile modified.
    ProfileModified,
    /// Access granted.
    AccessGranted,
    /// Access denied.
    AccessDenied,
}

// ---------------------------------------------------------------------------
//  Exit function types
// ---------------------------------------------------------------------------

/// Type for ICHRTX00 (pre-auth) exit functions.
pub type AuthExitFn = Box<dyn Fn(&AuthExitContext) -> ExitAction + Send + Sync>;

/// Type for ICHRCX02 (pre-verify) exit functions.
pub type VerifyExitFn = Box<dyn Fn(&VerifyExitContext) -> ExitAction + Send + Sync>;

/// Type for ICHPWX01 (password quality) exit functions.
pub type PasswordExitFn = Box<dyn Fn(&PasswordExitContext) -> PasswordExitResult + Send + Sync>;

/// Type for IRREVX01 (event notification) exit functions.
pub type EventExitFn = Box<dyn Fn(&EventContext) + Send + Sync>;

// ---------------------------------------------------------------------------
//  Exit manager
// ---------------------------------------------------------------------------

/// The Exit Manager — holds registered exit functions.
///
/// Exits are identified by name and can be registered/unregistered at runtime.
/// Multiple exits can be registered for the same exit point; they are invoked
/// in registration order.
pub struct ExitManager {
    /// ICHRTX00 exits (pre-auth).
    auth_exits: BTreeMap<String, AuthExitFn>,
    /// ICHRCX02 exits (pre-verify).
    verify_exits: BTreeMap<String, VerifyExitFn>,
    /// ICHPWX01 exits (password quality).
    password_exits: BTreeMap<String, PasswordExitFn>,
    /// IRREVX01 exits (event notification).
    event_exits: BTreeMap<String, EventExitFn>,
}

impl Default for ExitManager {
    fn default() -> Self {
        Self::new()
    }
}

impl ExitManager {
    /// Create a new exit manager with no exits registered.
    pub fn new() -> Self {
        Self {
            auth_exits: BTreeMap::new(),
            verify_exits: BTreeMap::new(),
            password_exits: BTreeMap::new(),
            event_exits: BTreeMap::new(),
        }
    }

    // ─────── Registration ───────

    /// Register an ICHRTX00 (pre-authorization) exit.
    pub fn register_auth_exit(&mut self, name: &str, exit: AuthExitFn) {
        self.auth_exits.insert(name.to_string(), exit);
    }

    /// Register an ICHRCX02 (pre-verify) exit.
    pub fn register_verify_exit(&mut self, name: &str, exit: VerifyExitFn) {
        self.verify_exits.insert(name.to_string(), exit);
    }

    /// Register an ICHPWX01 (password quality) exit.
    pub fn register_password_exit(&mut self, name: &str, exit: PasswordExitFn) {
        self.password_exits.insert(name.to_string(), exit);
    }

    /// Register an IRREVX01 (event notification) exit.
    pub fn register_event_exit(&mut self, name: &str, exit: EventExitFn) {
        self.event_exits.insert(name.to_string(), exit);
    }

    // ─────── Unregistration ───────

    /// Unregister an exit by name.
    pub fn unregister_auth_exit(&mut self, name: &str) -> bool {
        self.auth_exits.remove(name).is_some()
    }

    /// Unregister a verify exit by name.
    pub fn unregister_verify_exit(&mut self, name: &str) -> bool {
        self.verify_exits.remove(name).is_some()
    }

    /// Unregister a password exit by name.
    pub fn unregister_password_exit(&mut self, name: &str) -> bool {
        self.password_exits.remove(name).is_some()
    }

    /// Unregister an event exit by name.
    pub fn unregister_event_exit(&mut self, name: &str) -> bool {
        self.event_exits.remove(name).is_some()
    }

    // ─────── Invocation ───────

    /// Invoke all ICHRTX00 (pre-auth) exits.
    ///
    /// If any exit returns `Allow` or `Deny`, that action is returned immediately.
    /// If all exits return `Continue`, the result is `Continue`.
    pub fn invoke_auth_exits(&self, ctx: &AuthExitContext) -> ExitAction {
        for exit in self.auth_exits.values() {
            match exit(ctx) {
                ExitAction::Continue => continue,
                action => return action,
            }
        }
        ExitAction::Continue
    }

    /// Invoke all ICHRCX02 (pre-verify) exits.
    pub fn invoke_verify_exits(&self, ctx: &VerifyExitContext) -> ExitAction {
        for exit in self.verify_exits.values() {
            match exit(ctx) {
                ExitAction::Continue => continue,
                action => return action,
            }
        }
        ExitAction::Continue
    }

    /// Invoke all ICHPWX01 (password quality) exits.
    ///
    /// If any exit rejects the password, the rejection is returned.
    pub fn invoke_password_exits(&self, ctx: &PasswordExitContext) -> PasswordExitResult {
        for exit in self.password_exits.values() {
            let result = exit(ctx);
            if let PasswordExitResult::Reject(_) = &result {
                return result;
            }
        }
        PasswordExitResult::Accept
    }

    /// Invoke all IRREVX01 (event notification) exits.
    pub fn invoke_event_exits(&self, ctx: &EventContext) {
        for exit in self.event_exits.values() {
            exit(ctx);
        }
    }

    // ─────── Query ───────

    /// Get the number of registered auth exits.
    pub fn auth_exit_count(&self) -> usize {
        self.auth_exits.len()
    }

    /// Get the number of registered verify exits.
    pub fn verify_exit_count(&self) -> usize {
        self.verify_exits.len()
    }

    /// Get the number of registered password exits.
    pub fn password_exit_count(&self) -> usize {
        self.password_exits.len()
    }

    /// Get the number of registered event exits.
    pub fn event_exit_count(&self) -> usize {
        self.event_exits.len()
    }

    /// List registered auth exit names.
    pub fn list_auth_exits(&self) -> Vec<&str> {
        self.auth_exits.keys().map(|k| k.as_str()).collect()
    }

    /// List registered event exit names.
    pub fn list_event_exits(&self) -> Vec<&str> {
        self.event_exits.keys().map(|k| k.as_str()).collect()
    }
}

// Debug can't be derived because of the closures, so implement manually.
impl std::fmt::Debug for ExitManager {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExitManager")
            .field("auth_exits", &self.auth_exits.keys().collect::<Vec<_>>())
            .field("verify_exits", &self.verify_exits.keys().collect::<Vec<_>>())
            .field("password_exits", &self.password_exits.keys().collect::<Vec<_>>())
            .field("event_exits", &self.event_exits.keys().collect::<Vec<_>>())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU32, Ordering};
    use std::sync::Arc;

    // ─────── S109.1: ICHRTX00 Pre-Auth Exit ───────

    #[test]
    fn test_auth_exit_continue() {
        let mut mgr = ExitManager::new();
        mgr.register_auth_exit("test", Box::new(|_| ExitAction::Continue));

        let ctx = AuthExitContext {
            class: "DATASET".into(),
            entity: "PROD.DATA".into(),
            userid: "JSMITH".into(),
            access: AccessLevel::Read,
        };
        assert_eq!(mgr.invoke_auth_exits(&ctx), ExitAction::Continue);
    }

    #[test]
    fn test_auth_exit_allow_bypasses() {
        let mut mgr = ExitManager::new();
        mgr.register_auth_exit("bypass", Box::new(|ctx| {
            if ctx.userid == "SYSADM" {
                ExitAction::Allow
            } else {
                ExitAction::Continue
            }
        }));

        let ctx = AuthExitContext {
            class: "DATASET".into(),
            entity: "PROD.DATA".into(),
            userid: "SYSADM".into(),
            access: AccessLevel::Read,
        };
        assert_eq!(mgr.invoke_auth_exits(&ctx), ExitAction::Allow);
    }

    #[test]
    fn test_auth_exit_deny() {
        let mut mgr = ExitManager::new();
        mgr.register_auth_exit("block", Box::new(|ctx| {
            if ctx.class == "FACILITY" && ctx.entity.starts_with("RESTRICTED") {
                ExitAction::Deny
            } else {
                ExitAction::Continue
            }
        }));

        let ctx = AuthExitContext {
            class: "FACILITY".into(),
            entity: "RESTRICTED.RESOURCE".into(),
            userid: "JSMITH".into(),
            access: AccessLevel::Read,
        };
        assert_eq!(mgr.invoke_auth_exits(&ctx), ExitAction::Deny);
    }

    // ─────── S109.2: ICHRCX02 Pre-Verify Exit ───────

    #[test]
    fn test_verify_exit() {
        let mut mgr = ExitManager::new();
        mgr.register_verify_exit("mfa_check", Box::new(|_ctx| ExitAction::Continue));

        let ctx = VerifyExitContext {
            userid: "JSMITH".into(),
            has_password: true,
            has_passticket: false,
            application: None,
        };
        assert_eq!(mgr.invoke_verify_exits(&ctx), ExitAction::Continue);
    }

    // ─────── S109.3: ICHPWX01 Password Quality Exit ───────

    #[test]
    fn test_password_exit_accept() {
        let mut mgr = ExitManager::new();
        mgr.register_password_exit("quality", Box::new(|_| PasswordExitResult::Accept));

        let ctx = PasswordExitContext {
            userid: "JSMITH".into(),
            new_password: "GOODPASS".into(),
        };
        assert_eq!(mgr.invoke_password_exits(&ctx), PasswordExitResult::Accept);
    }

    #[test]
    fn test_password_exit_reject_weak() {
        let mut mgr = ExitManager::new();
        mgr.register_password_exit("no_userid", Box::new(|ctx| {
            if ctx.new_password.contains(&ctx.userid) {
                PasswordExitResult::Reject("Password must not contain userid".into())
            } else {
                PasswordExitResult::Accept
            }
        }));

        let ctx = PasswordExitContext {
            userid: "JSMITH".into(),
            new_password: "JSMITHXX".into(),
        };
        assert!(matches!(
            mgr.invoke_password_exits(&ctx),
            PasswordExitResult::Reject(_)
        ));
    }

    // ─────── S109.4: IRREVX01 Event Notification Exit ───────

    #[test]
    fn test_event_exit_invoked() {
        let counter = Arc::new(AtomicU32::new(0));
        let counter_clone = counter.clone();

        let mut mgr = ExitManager::new();
        mgr.register_event_exit("counter", Box::new(move |_| {
            counter_clone.fetch_add(1, Ordering::SeqCst);
        }));

        let ctx = EventContext {
            event_type: EventType::AuthSuccess,
            userid: "JSMITH".into(),
            details: "Authenticated successfully".into(),
        };

        mgr.invoke_event_exits(&ctx);
        mgr.invoke_event_exits(&ctx);
        assert_eq!(counter.load(Ordering::SeqCst), 2);
    }

    #[test]
    fn test_multiple_event_exits() {
        let counter1 = Arc::new(AtomicU32::new(0));
        let counter2 = Arc::new(AtomicU32::new(0));
        let c1 = counter1.clone();
        let c2 = counter2.clone();

        let mut mgr = ExitManager::new();
        mgr.register_event_exit("exit1", Box::new(move |_| {
            c1.fetch_add(1, Ordering::SeqCst);
        }));
        mgr.register_event_exit("exit2", Box::new(move |_| {
            c2.fetch_add(1, Ordering::SeqCst);
        }));

        let ctx = EventContext {
            event_type: EventType::PasswordChange,
            userid: "JSMITH".into(),
            details: "Password changed".into(),
        };

        mgr.invoke_event_exits(&ctx);
        assert_eq!(counter1.load(Ordering::SeqCst), 1);
        assert_eq!(counter2.load(Ordering::SeqCst), 1);
    }

    // ─────── Registration / Unregistration ───────

    #[test]
    fn test_unregister_exits() {
        let mut mgr = ExitManager::new();
        mgr.register_auth_exit("test", Box::new(|_| ExitAction::Continue));
        assert_eq!(mgr.auth_exit_count(), 1);

        assert!(mgr.unregister_auth_exit("test"));
        assert_eq!(mgr.auth_exit_count(), 0);

        assert!(!mgr.unregister_auth_exit("noexist"));
    }

    #[test]
    fn test_list_exits() {
        let mut mgr = ExitManager::new();
        mgr.register_auth_exit("exit_a", Box::new(|_| ExitAction::Continue));
        mgr.register_auth_exit("exit_b", Box::new(|_| ExitAction::Continue));

        let names = mgr.list_auth_exits();
        assert_eq!(names.len(), 2);
        assert!(names.contains(&"exit_a"));
        assert!(names.contains(&"exit_b"));
    }

    #[test]
    fn test_no_exits_returns_continue() {
        let mgr = ExitManager::new();
        let ctx = AuthExitContext {
            class: "DATASET".into(),
            entity: "X".into(),
            userid: "Y".into(),
            access: AccessLevel::None,
        };
        assert_eq!(mgr.invoke_auth_exits(&ctx), ExitAction::Continue);
    }
}
