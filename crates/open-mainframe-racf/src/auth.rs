//! RACF authentication services — password, passphrase, PassTicket, and certificate authentication.

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};
use tracing::{debug, info, warn};

use crate::database::RacfDatabase;
use crate::types::UserAttribute;

// ─────────────────────── Password Policy ───────────────────────

/// Password policy settings (configured via SETROPTS).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PasswordPolicy {
    /// Maximum number of days before password expires (0 = never).
    pub interval: u32,
    /// Number of password history entries to retain.
    pub history: u32,
    /// Minimum password length.
    pub min_length: u32,
    /// Maximum password length.
    pub max_length: u32,
    /// Maximum consecutive failed attempts before revoke.
    pub max_failures: u32,
}

impl Default for PasswordPolicy {
    fn default() -> Self {
        Self {
            interval: 90,
            history: 32,
            min_length: 4,
            max_length: 8,
            max_failures: 3,
        }
    }
}

// ─────────────────────── Authentication Results ───────────────────────

/// Result of a password authentication attempt.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AuthResult {
    /// Authentication succeeded.
    Success,
    /// Password is correct but expired — must change.
    Expired,
    /// Password does not match.
    InvalidPassword,
    /// User is revoked (cannot log on).
    Revoked,
    /// User not found.
    UserNotFound,
    /// No password set for user.
    NoPassword,
}

impl AuthResult {
    /// Whether authentication succeeded (including expired requiring change).
    pub fn is_authenticated(&self) -> bool {
        matches!(self, Self::Success | Self::Expired)
    }
}

/// Result of a password change attempt.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PasswordChangeResult {
    /// Password changed successfully.
    Success,
    /// New password was previously used (in history).
    PreviouslyUsed,
    /// New password does not meet policy requirements.
    PolicyViolation(String),
    /// Old password is incorrect.
    InvalidOldPassword,
    /// User not found.
    UserNotFound,
}

// ─────────────────────── PassTicket Types ───────────────────────

/// PassTicket key storage type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PassTicketKeyType {
    /// Key stored in masked form (KEYMASKED).
    KeyMasked,
    /// Key stored in encrypted form (KEYENCRYPTED).
    KeyEncrypted,
}

impl Default for PassTicketKeyType {
    fn default() -> Self {
        Self::KeyMasked
    }
}

/// PassTicket application profile — defines which apps can use PassTicket auth.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PassTicketProfile {
    /// Application name (e.g. "CICSAPP1").
    pub application: String,
    /// Shared secret key (simplified — in production this is DES-encrypted).
    pub secret_key: String,
    /// Key storage type.
    pub key_type: PassTicketKeyType,
    /// PassTicket validity window in minutes (default 10).
    pub timeout_minutes: u32,
    /// If true, evaluate password as PassTicket even without explicit APPL parameter.
    pub noeval: bool,
}

/// A generated PassTicket.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PassTicket {
    /// The ticket value (8-character string).
    pub ticket: String,
    /// The timestamp when generated (epoch seconds).
    pub timestamp: u64,
    /// The userid this ticket was generated for.
    pub userid: String,
    /// The application this ticket is valid for.
    pub application: String,
}

/// Result of PassTicket validation with detailed reason.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PassTicketValidationResult {
    /// PassTicket is valid.
    Success,
    /// PassTicket was already used (replay detected).
    Replay,
    /// PassTicket has expired (was valid but outside the time window).
    Expired,
    /// PassTicket value is invalid (wrong key or malformed).
    InvalidTicket,
    /// No PTKTDATA profile found for the application.
    NoProfile,
}

impl PassTicketValidationResult {
    /// Whether the validation succeeded.
    pub fn is_valid(&self) -> bool {
        matches!(self, Self::Success)
    }
}

/// Result from RLIST PTKTDATA — shows profile configuration.
#[derive(Debug, Clone)]
pub struct PassTicketListResult {
    /// Application name.
    pub application: String,
    /// Key type (KEYMASKED or KEYENCRYPTED).
    pub key_type: PassTicketKeyType,
    /// Timeout in minutes.
    pub timeout_minutes: u32,
    /// NOEVAL flag.
    pub noeval: bool,
}

// ─────────────────────── Certificate Mapping ───────────────────────

/// Certificate-to-userid mapping entry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CertificateMapping {
    /// Certificate subject DN (Distinguished Name).
    pub subject_dn: String,
    /// Issuer DN.
    pub issuer_dn: String,
    /// Mapped userid.
    pub userid: String,
}

// ─────────────────────── Authentication Service ───────────────────────

/// Maximum sequence number to try during PassTicket validation.
const MAX_PASSTICKET_SEQ: u64 = 256;

/// RACF authentication service — manages password auth, PassTickets, and certificate mapping.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuthService {
    /// Password policy settings.
    pub policy: PasswordPolicy,
    /// PassTicket application profiles: app name → profile.
    pub passticket_profiles: BTreeMap<String, PassTicketProfile>,
    /// Certificate mappings: subject DN → mapping.
    pub certificate_mappings: BTreeMap<String, CertificateMapping>,
    /// Used PassTickets for replay prevention: "userid:app:ticket" → expiry timestamp.
    pub used_passtickets: BTreeMap<String, u64>,
    /// Monotonic sequence counter for PassTicket uniqueness.
    passticket_seq: u64,
}

impl Default for AuthService {
    fn default() -> Self {
        Self::new()
    }
}

impl AuthService {
    /// Create a new authentication service with default policy.
    pub fn new() -> Self {
        Self {
            policy: PasswordPolicy::default(),
            passticket_profiles: BTreeMap::new(),
            certificate_mappings: BTreeMap::new(),
            used_passtickets: BTreeMap::new(),
            passticket_seq: 0,
        }
    }

    /// Create with a specific password policy.
    pub fn with_policy(policy: PasswordPolicy) -> Self {
        Self {
            policy,
            ..Self::new()
        }
    }

    // ─────── Password Authentication ───────

    /// Authenticate a user with a password.
    ///
    /// Checks revoke status, password match, expiration, and updates failed count.
    pub fn authenticate(
        &self,
        db: &mut RacfDatabase,
        userid: &str,
        password: &str,
    ) -> AuthResult {
        let userid = userid.trim().to_uppercase();

        let Some(user) = db.get_user(&userid) else {
            return AuthResult::UserNotFound;
        };

        // Check revoke.
        if user.is_revoked() {
            info!("AUTH: user {} is revoked", userid);
            return AuthResult::Revoked;
        }

        // Check password exists.
        let Some(hash) = &user.password_hash else {
            return AuthResult::NoPassword;
        };

        // Verify password (simplified — direct comparison).
        if hash != password {
            info!("AUTH: user {} password mismatch", userid);
            // Increment failed count.
            let user = db.get_user_mut(&userid).unwrap();
            user.failed_password_count += 1;

            // Auto-revoke if max failures exceeded.
            if user.failed_password_count >= self.policy.max_failures {
                warn!(
                    "AUTH: user {} auto-revoked after {} failures",
                    userid, user.failed_password_count
                );
                user.attributes.insert(UserAttribute::Revoke);
            }

            return AuthResult::InvalidPassword;
        }

        // Reset failed count on success.
        let password_changed = user.password_changed.clone();
        let user = db.get_user_mut(&userid).unwrap();
        user.failed_password_count = 0;

        // Check expiration.
        if self.policy.interval > 0 {
            if let Some(changed_str) = &password_changed {
                if let Ok(changed_ts) = changed_str.parse::<u64>() {
                    let now = current_timestamp();
                    let age_days = (now.saturating_sub(changed_ts)) / 86400;
                    if age_days >= u64::from(self.policy.interval) {
                        debug!("AUTH: user {} password expired (age={}d)", userid, age_days);
                        return AuthResult::Expired;
                    }
                }
            }
        }

        info!("AUTH: user {} authenticated successfully", userid);
        AuthResult::Success
    }

    /// Change a user's password.
    ///
    /// Validates old password, checks history, enforces policy, and updates profile.
    pub fn change_password(
        &self,
        db: &mut RacfDatabase,
        userid: &str,
        old_password: &str,
        new_password: &str,
    ) -> PasswordChangeResult {
        let userid = userid.trim().to_uppercase();

        let Some(user) = db.get_user(&userid) else {
            return PasswordChangeResult::UserNotFound;
        };

        // Verify old password.
        if let Some(hash) = &user.password_hash {
            if hash != old_password {
                return PasswordChangeResult::InvalidOldPassword;
            }
        }

        // Check password policy — length.
        let new_len = new_password.len() as u32;
        if new_len < self.policy.min_length {
            return PasswordChangeResult::PolicyViolation(format!(
                "password too short (minimum {} characters)",
                self.policy.min_length
            ));
        }
        if new_len > self.policy.max_length {
            return PasswordChangeResult::PolicyViolation(format!(
                "password too long (maximum {} characters)",
                self.policy.max_length
            ));
        }

        // Check history.
        let history = &user.password_history;
        if history.contains(&new_password.to_string()) {
            return PasswordChangeResult::PreviouslyUsed;
        }
        // Also check current password.
        if user.password_hash.as_deref() == Some(new_password) {
            return PasswordChangeResult::PreviouslyUsed;
        }

        // Apply the change.
        let user = db.get_user_mut(&userid).unwrap();

        // Push old password to history.
        if let Some(old) = &user.password_hash {
            user.password_history.push(old.clone());
            // Trim history to configured size.
            let max = self.policy.history as usize;
            while user.password_history.len() > max {
                user.password_history.remove(0);
            }
        }

        user.password_hash = Some(new_password.to_string());
        user.password_changed = Some(format!("{}", current_timestamp()));
        user.failed_password_count = 0;

        info!("AUTH: user {} password changed", userid);
        PasswordChangeResult::Success
    }

    // ─────── PassTicket Management (SYS-110.1) ───────

    /// Register a PassTicket application profile (simplified interface).
    pub fn add_passticket_profile(&mut self, application: &str, secret_key: &str) {
        self.rdefine_ptktdata(
            application,
            secret_key,
            PassTicketKeyType::KeyMasked,
            None,
            false,
        );
    }

    /// RDEFINE PTKTDATA — create a PassTicket profile for an application.
    ///
    /// Equivalent to: `RDEFINE PTKTDATA appname SSIGNON(KEYMASKED(key))`
    pub fn rdefine_ptktdata(
        &mut self,
        application: &str,
        secret_key: &str,
        key_type: PassTicketKeyType,
        timeout: Option<u32>,
        noeval: bool,
    ) {
        let app = application.trim().to_uppercase();
        self.passticket_profiles.insert(
            app.clone(),
            PassTicketProfile {
                application: app,
                secret_key: secret_key.to_string(),
                key_type,
                timeout_minutes: timeout.unwrap_or(10),
                noeval,
            },
        );
    }

    /// RLIST PTKTDATA — list a PassTicket application profile.
    ///
    /// Returns profile details including application name, key type, and timeout.
    pub fn rlist_ptktdata(&self, application: &str) -> Option<PassTicketListResult> {
        let app = application.trim().to_uppercase();
        self.passticket_profiles.get(&app).map(|p| PassTicketListResult {
            application: p.application.clone(),
            key_type: p.key_type,
            timeout_minutes: p.timeout_minutes,
            noeval: p.noeval,
        })
    }

    /// Delete a PTKTDATA profile.
    pub fn rdelete_ptktdata(&mut self, application: &str) -> bool {
        let app = application.trim().to_uppercase();
        self.passticket_profiles.remove(&app).is_some()
    }

    /// List all PTKTDATA application names.
    pub fn list_ptktdata_applications(&self) -> Vec<&str> {
        self.passticket_profiles.keys().map(|k| k.as_str()).collect()
    }

    // ─────── PassTicket Generation (SYS-110.2) ───────

    /// Generate a PassTicket for a user and application.
    ///
    /// Returns `None` if no PTKTDATA profile exists for the application.
    /// Each call produces a unique ticket, even within the same time window,
    /// via a monotonic sequence counter.
    pub fn generate_passticket(
        &mut self,
        userid: &str,
        application: &str,
    ) -> Option<PassTicket> {
        let app = application.trim().to_uppercase();
        let userid = userid.trim().to_uppercase();

        let profile = self.passticket_profiles.get(&app)?;
        let timestamp = current_timestamp();

        // Use monotonic seq to ensure uniqueness within the same time window.
        let seq = self.passticket_seq;
        self.passticket_seq += 1;

        let ticket = generate_ticket_value(&userid, &app, &profile.secret_key, timestamp, seq);

        Some(PassTicket {
            ticket,
            timestamp,
            userid,
            application: app,
        })
    }

    // ─────── PassTicket Validation (SYS-110.3) ───────

    /// Validate a PassTicket with detailed result.
    ///
    /// Checks the ticket against the PTKTDATA profile key and the current/adjacent
    /// time windows. Detects replay, expiry, and invalid tickets.
    pub fn validate_passticket(
        &mut self,
        userid: &str,
        application: &str,
        ticket: &str,
    ) -> PassTicketValidationResult {
        let app = application.trim().to_uppercase();
        let userid = userid.trim().to_uppercase();

        let Some(profile) = self.passticket_profiles.get(&app) else {
            debug!("PASSTICKET: no profile for application {}", app);
            return PassTicketValidationResult::NoProfile;
        };

        let secret = profile.secret_key.clone();
        let timeout_secs = u64::from(profile.timeout_minutes) * 60;
        let now = current_timestamp();

        // Check replay — ticket must not have been used before.
        let replay_key = format!("{}:{}:{}", userid, app, ticket);
        if self.used_passtickets.contains_key(&replay_key) {
            warn!("PASSTICKET: replay detected for {}:{}", userid, app);
            return PassTicketValidationResult::Replay;
        }

        // Check current and adjacent time windows (±1 window for clock skew).
        let valid_windows = [now, now.wrapping_sub(timeout_secs), now + timeout_secs];
        for &ts in &valid_windows {
            for seq in 0..MAX_PASSTICKET_SEQ.min(self.passticket_seq + 1) {
                let expected = generate_ticket_value(&userid, &app, &secret, ts, seq);
                if expected == ticket {
                    // Record ticket as used with expiry.
                    self.used_passtickets
                        .insert(replay_key, now + timeout_secs * 2);
                    info!("PASSTICKET: valid ticket for {}:{}", userid, app);
                    return PassTicketValidationResult::Success;
                }
            }
        }

        // Check expired windows (2-3 windows back) to distinguish expired from invalid.
        let expired_windows = [
            now.wrapping_sub(timeout_secs * 2),
            now.wrapping_sub(timeout_secs * 3),
        ];
        for &ts in &expired_windows {
            for seq in 0..MAX_PASSTICKET_SEQ.min(self.passticket_seq + 1) {
                let expected = generate_ticket_value(&userid, &app, &secret, ts, seq);
                if expected == ticket {
                    debug!("PASSTICKET: expired ticket for {}:{}", userid, app);
                    return PassTicketValidationResult::Expired;
                }
            }
        }

        debug!("PASSTICKET: invalid ticket for {}:{}", userid, app);
        PassTicketValidationResult::InvalidTicket
    }

    /// Check if an application has NOEVAL set.
    pub fn is_noeval_application(&self, application: &str) -> bool {
        let app = application.trim().to_uppercase();
        self.passticket_profiles
            .get(&app)
            .map_or(false, |p| p.noeval)
    }

    // ─────── Certificate Mapping ───────

    /// Add a certificate-to-userid mapping.
    pub fn add_certificate_mapping(
        &mut self,
        subject_dn: &str,
        issuer_dn: &str,
        userid: &str,
    ) {
        let mapping = CertificateMapping {
            subject_dn: subject_dn.to_string(),
            issuer_dn: issuer_dn.to_string(),
            userid: userid.trim().to_uppercase(),
        };
        self.certificate_mappings
            .insert(subject_dn.to_string(), mapping);
    }

    /// Look up a userid by certificate subject DN.
    pub fn lookup_certificate(&self, subject_dn: &str) -> Option<&str> {
        self.certificate_mappings
            .get(subject_dn)
            .map(|m| m.userid.as_str())
    }

    /// Remove a certificate mapping.
    pub fn remove_certificate_mapping(&mut self, subject_dn: &str) -> bool {
        self.certificate_mappings.remove(subject_dn).is_some()
    }

    // ─────── Maintenance ───────

    /// Purge expired PassTicket replay entries.
    pub fn purge_expired_passtickets(&mut self) {
        let now = current_timestamp();
        self.used_passtickets.retain(|_, expiry| *expiry > now);
    }
}

/// Generate a simplified PassTicket value.
///
/// Real z/OS uses DES-MAC with the secured signon key. This implementation
/// uses a simplified hash for demonstration purposes. The `seq` parameter
/// ensures uniqueness when multiple tickets are generated in the same time window.
fn generate_ticket_value(
    userid: &str,
    application: &str,
    secret: &str,
    timestamp: u64,
    seq: u64,
) -> String {
    // Time window = timestamp / 600 (10-minute windows).
    let window = timestamp / 600;
    let input = format!("{}:{}:{}:{}:{}", userid, application, secret, window, seq);

    // Simple hash — take a numeric hash and format as 8-digit string.
    let hash = input
        .bytes()
        .fold(0u64, |acc, b| acc.wrapping_mul(31).wrapping_add(u64::from(b)));
    format!("{:08}", hash % 100_000_000)
}

fn current_timestamp() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup() -> (RacfDatabase, AuthService) {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();
        db.add_user("JSMITH", "DEPT01", "John Smith", "ADMIN1")
            .unwrap();

        // Set password with recent change timestamp.
        let user = db.get_user_mut("JSMITH").unwrap();
        user.password_hash = Some("SECRET1".to_string());
        user.password_changed = Some(format!("{}", current_timestamp()));

        let auth = AuthService::new();
        (db, auth)
    }

    // ─────── Password Authentication (existing) ───────

    #[test]
    fn test_authenticate_success() {
        let (mut db, auth) = setup();
        let result = auth.authenticate(&mut db, "JSMITH", "SECRET1");
        assert_eq!(result, AuthResult::Success);
        assert!(result.is_authenticated());
    }

    #[test]
    fn test_authenticate_invalid_password() {
        let (mut db, auth) = setup();
        let result = auth.authenticate(&mut db, "JSMITH", "WRONG");
        assert_eq!(result, AuthResult::InvalidPassword);
        assert!(!result.is_authenticated());

        // Failed count should be incremented.
        let user = db.get_user("JSMITH").unwrap();
        assert_eq!(user.failed_password_count, 1);
    }

    #[test]
    fn test_authenticate_auto_revoke_on_max_failures() {
        let (mut db, auth) = setup();

        // Fail 3 times (default max_failures = 3).
        auth.authenticate(&mut db, "JSMITH", "WRONG");
        auth.authenticate(&mut db, "JSMITH", "WRONG");
        auth.authenticate(&mut db, "JSMITH", "WRONG");

        // User should now be revoked.
        let user = db.get_user("JSMITH").unwrap();
        assert!(user.is_revoked());

        // Subsequent auth should return Revoked.
        let result = auth.authenticate(&mut db, "JSMITH", "SECRET1");
        assert_eq!(result, AuthResult::Revoked);
    }

    #[test]
    fn test_authenticate_expired_password() {
        let (mut db, auth) = setup();

        // Set password_changed to 100 days ago.
        let user = db.get_user_mut("JSMITH").unwrap();
        let old_time = current_timestamp().saturating_sub(100 * 86400);
        user.password_changed = Some(format!("{}", old_time));

        let result = auth.authenticate(&mut db, "JSMITH", "SECRET1");
        assert_eq!(result, AuthResult::Expired);
        assert!(result.is_authenticated());
    }

    #[test]
    fn test_authenticate_revoked_user() {
        let (mut db, auth) = setup();
        db.alter_user("JSMITH", None, None, None, &[UserAttribute::Revoke], &[])
            .unwrap();

        let result = auth.authenticate(&mut db, "JSMITH", "SECRET1");
        assert_eq!(result, AuthResult::Revoked);
    }

    #[test]
    fn test_authenticate_user_not_found() {
        let (mut db, auth) = setup();
        let result = auth.authenticate(&mut db, "NOBODY", "SECRET1");
        assert_eq!(result, AuthResult::UserNotFound);
    }

    #[test]
    fn test_authenticate_no_password() {
        let (mut db, _) = setup();
        db.add_user("NOPASS", "DEPT01", "No Pass", "ADMIN1")
            .unwrap();
        let auth = AuthService::new();

        let result = auth.authenticate(&mut db, "NOPASS", "anything");
        assert_eq!(result, AuthResult::NoPassword);
    }

    #[test]
    fn test_change_password_success() {
        let (mut db, auth) = setup();
        let result = auth.change_password(&mut db, "JSMITH", "SECRET1", "NEWPASS1");
        assert_eq!(result, PasswordChangeResult::Success);

        // New password should work.
        let auth_result = auth.authenticate(&mut db, "JSMITH", "NEWPASS1");
        assert_eq!(auth_result, AuthResult::Success);
    }

    #[test]
    fn test_change_password_history_check() {
        let (mut db, auth) = setup();

        // Change password a few times.
        auth.change_password(&mut db, "JSMITH", "SECRET1", "PASS002");
        auth.change_password(&mut db, "JSMITH", "PASS002", "PASS003");

        // Try to reuse SECRET1 (should be in history).
        let result = auth.change_password(&mut db, "JSMITH", "PASS003", "SECRET1");
        assert_eq!(result, PasswordChangeResult::PreviouslyUsed);
    }

    #[test]
    fn test_change_password_reuse_current() {
        let (mut db, auth) = setup();
        let result = auth.change_password(&mut db, "JSMITH", "SECRET1", "SECRET1");
        assert_eq!(result, PasswordChangeResult::PreviouslyUsed);
    }

    #[test]
    fn test_change_password_too_short() {
        let (mut db, auth) = setup();
        let result = auth.change_password(&mut db, "JSMITH", "SECRET1", "AB");
        assert!(matches!(result, PasswordChangeResult::PolicyViolation(_)));
    }

    #[test]
    fn test_change_password_too_long() {
        let (mut db, auth) = setup();
        let result = auth.change_password(&mut db, "JSMITH", "SECRET1", "TOOLONGPASSWORD");
        assert!(matches!(result, PasswordChangeResult::PolicyViolation(_)));
    }

    #[test]
    fn test_change_password_wrong_old() {
        let (mut db, auth) = setup();
        let result = auth.change_password(&mut db, "JSMITH", "WRONG", "NEWPASS1");
        assert_eq!(result, PasswordChangeResult::InvalidOldPassword);
    }

    #[test]
    fn test_failed_count_resets_on_success() {
        let (mut db, auth) = setup();
        auth.authenticate(&mut db, "JSMITH", "WRONG");
        auth.authenticate(&mut db, "JSMITH", "WRONG");
        assert_eq!(db.get_user("JSMITH").unwrap().failed_password_count, 2);

        auth.authenticate(&mut db, "JSMITH", "SECRET1");
        assert_eq!(db.get_user("JSMITH").unwrap().failed_password_count, 0);
    }

    // ─────── SYS-110.1: PTKTDATA Profile Management ───────

    #[test]
    fn test_rdefine_ptktdata_keymasked() {
        let mut auth = AuthService::new();
        auth.rdefine_ptktdata(
            "CICSAPP1",
            "E001193519561977",
            PassTicketKeyType::KeyMasked,
            None,
            false,
        );

        let listing = auth.rlist_ptktdata("CICSAPP1").unwrap();
        assert_eq!(listing.application, "CICSAPP1");
        assert_eq!(listing.key_type, PassTicketKeyType::KeyMasked);
        assert_eq!(listing.timeout_minutes, 10);
        assert!(!listing.noeval);
    }

    #[test]
    fn test_rdefine_ptktdata_zosmf() {
        let mut auth = AuthService::new();
        auth.rdefine_ptktdata(
            "ZOSMF",
            "ZOSMFKEY12345678",
            PassTicketKeyType::KeyMasked,
            Some(15),
            false,
        );

        let listing = auth.rlist_ptktdata("ZOSMF").unwrap();
        assert_eq!(listing.application, "ZOSMF");
        assert_eq!(listing.timeout_minutes, 15);
    }

    #[test]
    fn test_rdefine_ptktdata_keyencrypted() {
        let mut auth = AuthService::new();
        auth.rdefine_ptktdata(
            "DB2APP",
            "ENCRYPTEDKEY",
            PassTicketKeyType::KeyEncrypted,
            None,
            false,
        );

        let listing = auth.rlist_ptktdata("DB2APP").unwrap();
        assert_eq!(listing.key_type, PassTicketKeyType::KeyEncrypted);
    }

    #[test]
    fn test_rdefine_ptktdata_with_noeval() {
        let mut auth = AuthService::new();
        auth.rdefine_ptktdata(
            "AUTOAPP",
            "AUTOKEY",
            PassTicketKeyType::KeyMasked,
            None,
            true,
        );

        assert!(auth.is_noeval_application("AUTOAPP"));
        let listing = auth.rlist_ptktdata("AUTOAPP").unwrap();
        assert!(listing.noeval);
    }

    #[test]
    fn test_rlist_ptktdata_not_found() {
        let auth = AuthService::new();
        assert!(auth.rlist_ptktdata("NOAPP").is_none());
    }

    #[test]
    fn test_no_profile_fails_generation() {
        let mut auth = AuthService::new();
        assert!(auth.generate_passticket("JSMITH", "NOAPP").is_none());
    }

    #[test]
    fn test_rdelete_ptktdata() {
        let mut auth = AuthService::new();
        auth.add_passticket_profile("CICSAPP1", "KEY1");
        assert!(auth.rdelete_ptktdata("CICSAPP1"));
        assert!(auth.rlist_ptktdata("CICSAPP1").is_none());
    }

    #[test]
    fn test_list_ptktdata_applications() {
        let mut auth = AuthService::new();
        auth.add_passticket_profile("APP1", "KEY1");
        auth.add_passticket_profile("APP2", "KEY2");
        auth.add_passticket_profile("APP3", "KEY3");

        let apps = auth.list_ptktdata_applications();
        assert_eq!(apps.len(), 3);
        assert!(apps.contains(&"APP1"));
        assert!(apps.contains(&"APP2"));
        assert!(apps.contains(&"APP3"));
    }

    // ─────── SYS-110.2: PassTicket Generation ───────

    #[test]
    fn test_passticket_generate() {
        let mut auth = AuthService::new();
        auth.add_passticket_profile("CICSAPP1", "MYSECRETKEY");

        let ticket = auth.generate_passticket("JSMITH", "CICSAPP1").unwrap();
        assert_eq!(ticket.userid, "JSMITH");
        assert_eq!(ticket.application, "CICSAPP1");
        assert_eq!(ticket.ticket.len(), 8);
    }

    #[test]
    fn test_passticket_same_second_uniqueness() {
        let mut auth = AuthService::new();
        auth.add_passticket_profile("CICSAPP1", "MYSECRETKEY");

        // Generate two tickets for the same user/app — they should be different.
        let t1 = auth.generate_passticket("JSMITH", "CICSAPP1").unwrap();
        let t2 = auth.generate_passticket("JSMITH", "CICSAPP1").unwrap();
        assert_ne!(t1.ticket, t2.ticket, "same-second tickets must differ");
    }

    #[test]
    fn test_passticket_uses_ptktdata_key() {
        let mut auth = AuthService::new();
        auth.add_passticket_profile("APP1", "KEY_A");
        let t1 = auth.generate_passticket("JSMITH", "APP1").unwrap();

        // Different key produces different ticket (same seq, but different AuthService instance).
        let mut auth2 = AuthService::new();
        auth2.add_passticket_profile("APP1", "KEY_B");
        let t2 = auth2.generate_passticket("JSMITH", "APP1").unwrap();

        assert_ne!(t1.ticket, t2.ticket, "different keys produce different tickets");
    }

    // ─────── SYS-110.3: PassTicket Validation & Replay ───────

    #[test]
    fn test_passticket_validate_success() {
        let mut auth = AuthService::new();
        auth.add_passticket_profile("CICSAPP1", "MYSECRETKEY");

        let ticket = auth.generate_passticket("JSMITH", "CICSAPP1").unwrap();
        let result = auth.validate_passticket("JSMITH", "CICSAPP1", &ticket.ticket);
        assert_eq!(result, PassTicketValidationResult::Success);
        assert!(result.is_valid());
    }

    #[test]
    fn test_passticket_replay_rejected() {
        let mut auth = AuthService::new();
        auth.add_passticket_profile("CICSAPP1", "MYSECRETKEY");

        let ticket = auth.generate_passticket("JSMITH", "CICSAPP1").unwrap();

        // First use should succeed.
        assert!(auth
            .validate_passticket("JSMITH", "CICSAPP1", &ticket.ticket)
            .is_valid());
        // Second use (replay) should fail with Replay.
        let result = auth.validate_passticket("JSMITH", "CICSAPP1", &ticket.ticket);
        assert_eq!(result, PassTicketValidationResult::Replay);
    }

    #[test]
    fn test_passticket_no_profile_validation() {
        let mut auth = AuthService::new();
        let result = auth.validate_passticket("JSMITH", "NOAPP", "00000000");
        assert_eq!(result, PassTicketValidationResult::NoProfile);
    }

    #[test]
    fn test_passticket_invalid_ticket() {
        let mut auth = AuthService::new();
        auth.add_passticket_profile("CICSAPP1", "MYSECRETKEY");
        let result = auth.validate_passticket("JSMITH", "CICSAPP1", "00000000");
        assert_eq!(result, PassTicketValidationResult::InvalidTicket);
    }

    #[test]
    fn test_passticket_wrong_application() {
        let mut auth = AuthService::new();
        auth.add_passticket_profile("CICSAPP1", "KEY1");
        auth.add_passticket_profile("CICSAPP2", "KEY2");

        let ticket = auth.generate_passticket("JSMITH", "CICSAPP1").unwrap();
        // Validating against wrong application should fail.
        let result = auth.validate_passticket("JSMITH", "CICSAPP2", &ticket.ticket);
        assert!(!result.is_valid());
    }

    #[test]
    fn test_passticket_replay_cache_purge() {
        let mut auth = AuthService::new();
        // Insert an "already expired" entry.
        auth.used_passtickets
            .insert("old:key:ticket".to_string(), 0);
        auth.purge_expired_passtickets();
        assert!(auth.used_passtickets.is_empty());
    }

    #[test]
    fn test_passticket_multiple_generate_all_validate() {
        let mut auth = AuthService::new();
        auth.add_passticket_profile("CICSAPP1", "KEY1");

        // Generate 10 tickets rapidly.
        let tickets: Vec<PassTicket> = (0..10)
            .map(|_| auth.generate_passticket("JSMITH", "CICSAPP1").unwrap())
            .collect();

        // All should be unique.
        let unique: std::collections::HashSet<&str> =
            tickets.iter().map(|t| t.ticket.as_str()).collect();
        assert_eq!(unique.len(), 10, "all 10 tickets must be unique");

        // All should validate successfully.
        for ticket in &tickets {
            let result = auth.validate_passticket("JSMITH", "CICSAPP1", &ticket.ticket);
            assert!(result.is_valid(), "ticket {} should validate", ticket.ticket);
        }
    }

    // ─────── Certificate Mapping (existing) ───────

    #[test]
    fn test_certificate_mapping() {
        let mut auth = AuthService::new();
        auth.add_certificate_mapping(
            "CN=John Smith,O=ACME,C=US",
            "CN=ACME CA,O=ACME,C=US",
            "JSMITH",
        );

        assert_eq!(
            auth.lookup_certificate("CN=John Smith,O=ACME,C=US"),
            Some("JSMITH")
        );
    }

    #[test]
    fn test_certificate_mapping_not_found() {
        let auth = AuthService::new();
        assert!(auth.lookup_certificate("CN=Nobody").is_none());
    }

    #[test]
    fn test_certificate_mapping_remove() {
        let mut auth = AuthService::new();
        auth.add_certificate_mapping("CN=John,O=X,C=US", "CN=CA,O=X,C=US", "JSMITH");
        assert!(auth.remove_certificate_mapping("CN=John,O=X,C=US"));
        assert!(auth.lookup_certificate("CN=John,O=X,C=US").is_none());
    }
}
