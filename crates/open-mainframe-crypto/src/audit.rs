//! SEC-108: RACF Audit & SMF Integration.
//!
//! Provides SMF Type 80/81/83 record representations, IRRDBU00 database
//! unload functionality, and an audit trail query interface for RACF
//! security event logging.

use std::fmt;

// ---------------------------------------------------------------------------
// Story 1: SMF Type 80 — Authorization events
// ---------------------------------------------------------------------------

/// The type of a RACF authorization event.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AuthEventType {
    /// Resource access check.
    ResourceAccess,
    /// Logon attempt.
    Logon,
    /// Password change.
    PasswordChange,
    /// Certificate-based authentication.
    CertAuth,
    /// PassTicket validation.
    PassTicket,
}

impl fmt::Display for AuthEventType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ResourceAccess => write!(f, "RESOURCE_ACCESS"),
            Self::Logon => write!(f, "LOGON"),
            Self::PasswordChange => write!(f, "PASSWORD_CHANGE"),
            Self::CertAuth => write!(f, "CERT_AUTH"),
            Self::PassTicket => write!(f, "PASSTICKET"),
        }
    }
}

/// The result of an authorization event.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AuthResult {
    /// Access was granted.
    Success,
    /// Access was denied.
    Failure,
    /// Warning issued but access granted (e.g., WARNING mode profile).
    Warning,
}

impl fmt::Display for AuthResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Success => write!(f, "SUCCESS"),
            Self::Failure => write!(f, "FAILURE"),
            Self::Warning => write!(f, "WARNING"),
        }
    }
}

/// SMF Type 80 Record — Authorization success/failure events.
///
/// Captures RACF security events such as resource access checks, logon
/// attempts, and password changes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SmfType80Record {
    /// Type of security event.
    pub event_type: AuthEventType,
    /// User ID involved.
    pub user: String,
    /// Resource name (dataset, general resource, etc.).
    pub resource: String,
    /// Resource class (DATASET, FACILITY, etc.).
    pub class: String,
    /// Access level requested/granted.
    pub access: String,
    /// Outcome of the authorization check.
    pub result: AuthResult,
    /// Timestamp string (ISO 8601 format or z/OS STCK).
    pub timestamp: String,
}

impl SmfType80Record {
    /// Create a new SMF Type 80 record.
    pub fn new(
        event_type: AuthEventType,
        user: &str,
        resource: &str,
        class: &str,
        access: &str,
        result: AuthResult,
        timestamp: &str,
    ) -> Self {
        Self {
            event_type,
            user: user.to_uppercase(),
            resource: resource.to_string(),
            class: class.to_uppercase(),
            access: access.to_uppercase(),
            result,
            timestamp: timestamp.to_string(),
        }
    }
}

impl fmt::Display for SmfType80Record {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "SMF80 {} {} USER({}) RESOURCE({}) CLASS({}) ACCESS({}) AT {}",
            self.event_type, self.result, self.user, self.resource, self.class, self.access, self.timestamp
        )
    }
}

// ---------------------------------------------------------------------------
// Story 2: SMF Type 80 — Profile change events
// ---------------------------------------------------------------------------

/// The type of RACF profile change.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ProfileChangeType {
    /// User added (ADDUSER).
    AddUser,
    /// User modified (ALTUSER).
    AltUser,
    /// User deleted (DELUSER).
    DelUser,
    /// Group added (ADDGROUP).
    AddGroup,
    /// Group modified (ALTGROUP).
    AltGroup,
    /// Group deleted (DELGROUP).
    DelGroup,
    /// Access permitted (PERMIT).
    Permit,
    /// User connected (CONNECT).
    Connect,
    /// User removed from group (REMOVE).
    Remove,
    /// Dataset profile defined (ADDSD).
    AddSd,
    /// General resource profile defined (RDEFINE).
    Rdefine,
}

impl fmt::Display for ProfileChangeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AddUser => write!(f, "ADDUSER"),
            Self::AltUser => write!(f, "ALTUSER"),
            Self::DelUser => write!(f, "DELUSER"),
            Self::AddGroup => write!(f, "ADDGROUP"),
            Self::AltGroup => write!(f, "ALTGROUP"),
            Self::DelGroup => write!(f, "DELGROUP"),
            Self::Permit => write!(f, "PERMIT"),
            Self::Connect => write!(f, "CONNECT"),
            Self::Remove => write!(f, "REMOVE"),
            Self::AddSd => write!(f, "ADDSD"),
            Self::Rdefine => write!(f, "RDEFINE"),
        }
    }
}

/// SMF Type 80 Profile Change Record.
///
/// Captures RACF administrative commands that modify the security database.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SmfType80ProfileChange {
    /// Type of profile change.
    pub change_type: ProfileChangeType,
    /// User who issued the command.
    pub issuer: String,
    /// Target of the command (user, group, or resource name).
    pub target: String,
    /// Timestamp string.
    pub timestamp: String,
    /// Additional detail (e.g., new attributes, access level).
    pub detail: String,
}

impl SmfType80ProfileChange {
    /// Create a new profile change record.
    pub fn new(
        change_type: ProfileChangeType,
        issuer: &str,
        target: &str,
        timestamp: &str,
        detail: &str,
    ) -> Self {
        Self {
            change_type,
            issuer: issuer.to_uppercase(),
            target: target.to_string(),
            timestamp: timestamp.to_string(),
            detail: detail.to_string(),
        }
    }
}

impl fmt::Display for SmfType80ProfileChange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "SMF80 CHANGE {} ISSUER({}) TARGET({}) AT {} {}",
            self.change_type, self.issuer, self.target, self.timestamp, self.detail
        )
    }
}

// ---------------------------------------------------------------------------
// Story 3: SMF Type 81 — RACF initialization events
// ---------------------------------------------------------------------------

/// The type of RACF initialization event.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InitEventType {
    /// RACF started (IPL or restart).
    Startup,
    /// RACF database switched.
    DatabaseSwitch,
    /// SETROPTS command processed.
    SetroptsChange,
    /// Class activated/deactivated.
    ClassChange,
}

impl fmt::Display for InitEventType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Startup => write!(f, "STARTUP"),
            Self::DatabaseSwitch => write!(f, "DB_SWITCH"),
            Self::SetroptsChange => write!(f, "SETROPTS"),
            Self::ClassChange => write!(f, "CLASS_CHANGE"),
        }
    }
}

/// SMF Type 81 Record — RACF initialization events.
///
/// Captures system-level RACF events such as startup, database switches,
/// and SETROPTS changes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SmfType81Record {
    /// Type of initialization event.
    pub event_type: InitEventType,
    /// Timestamp string.
    pub timestamp: String,
    /// Description of the event.
    pub description: String,
    /// System ID where the event occurred.
    pub system_id: String,
}

impl SmfType81Record {
    /// Create a new SMF Type 81 record.
    pub fn new(
        event_type: InitEventType,
        timestamp: &str,
        description: &str,
        system_id: &str,
    ) -> Self {
        Self {
            event_type,
            timestamp: timestamp.to_string(),
            description: description.to_string(),
            system_id: system_id.to_uppercase(),
        }
    }
}

impl fmt::Display for SmfType81Record {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "SMF81 {} SYS({}) AT {} {}",
            self.event_type, self.system_id, self.timestamp, self.description
        )
    }
}

// ---------------------------------------------------------------------------
// Story 4: SMF Type 83 — Database change with before/after state
// ---------------------------------------------------------------------------

/// SMF Type 83 Record — RACF database changes with before/after images.
///
/// Captures the state of a RACF profile before and after a modification
/// for audit trail and forensics purposes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SmfType83Record {
    /// Profile class (DATASET, USER, GROUP, etc.).
    pub class: String,
    /// Profile name.
    pub profile_name: String,
    /// Before-image (serialized profile state).
    pub before_image: String,
    /// After-image (serialized profile state).
    pub after_image: String,
    /// User who made the change.
    pub changed_by: String,
    /// Timestamp string.
    pub timestamp: String,
}

impl SmfType83Record {
    /// Create a new SMF Type 83 record.
    pub fn new(
        class: &str,
        profile_name: &str,
        before_image: &str,
        after_image: &str,
        changed_by: &str,
        timestamp: &str,
    ) -> Self {
        Self {
            class: class.to_uppercase(),
            profile_name: profile_name.to_string(),
            before_image: before_image.to_string(),
            after_image: after_image.to_string(),
            changed_by: changed_by.to_uppercase(),
            timestamp: timestamp.to_string(),
        }
    }
}

impl fmt::Display for SmfType83Record {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "SMF83 CLASS({}) PROFILE({}) BY({}) AT {}",
            self.class, self.profile_name, self.changed_by, self.timestamp
        )
    }
}

// ---------------------------------------------------------------------------
// Story 5: IRRDBU00 — Database unload
// ---------------------------------------------------------------------------

/// IRRDBU00 database unload record types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnloadRecordType {
    /// User basic data.
    UserBasic,
    /// Group basic data.
    GroupBasic,
    /// Connect (user-to-group) data.
    ConnectData,
    /// Dataset profile data.
    DatasetAccess,
    /// General resource data.
    GeneralResource,
}

impl UnloadRecordType {
    /// Returns the IRRDBU00 record type prefix.
    pub fn prefix(self) -> &'static str {
        match self {
            Self::UserBasic => "0200",
            Self::GroupBasic => "0100",
            Self::ConnectData => "0205",
            Self::DatasetAccess => "0400",
            Self::GeneralResource => "0500",
        }
    }
}

impl fmt::Display for UnloadRecordType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UserBasic => write!(f, "USER_BASIC"),
            Self::GroupBasic => write!(f, "GROUP_BASIC"),
            Self::ConnectData => write!(f, "CONNECT_DATA"),
            Self::DatasetAccess => write!(f, "DATASET_ACCESS"),
            Self::GeneralResource => write!(f, "GENERAL_RESOURCE"),
        }
    }
}

/// An unloaded RACF record (flat format from IRRDBU00).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnloadRecord {
    /// The record type.
    pub record_type: UnloadRecordType,
    /// The profile name.
    pub name: String,
    /// Additional fields as key-value pairs.
    pub fields: Vec<(String, String)>,
}

/// IRRDBU00 — RACF database unload utility.
///
/// Converts RACF profile data into flat-file records for auditing and
/// analysis.
#[derive(Debug, Default)]
pub struct Irrdbu00 {
    /// Collected unload records.
    records: Vec<UnloadRecord>,
}

impl Irrdbu00 {
    /// Create a new IRRDBU00 instance.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a record to the unload.
    pub fn add_record(&mut self, record: UnloadRecord) {
        self.records.push(record);
    }

    /// Unload all records to fixed-format strings.
    ///
    /// Each line has the format: `<prefix> <name> <field1>=<value1> <field2>=<value2> ...`
    pub fn unload(&self) -> Vec<String> {
        self.records
            .iter()
            .map(|r| {
                let mut line = format!("{} {}", r.record_type.prefix(), r.name);
                for (k, v) in &r.fields {
                    line.push_str(&format!(" {}={}", k, v));
                }
                line
            })
            .collect()
    }

    /// Returns the number of records.
    pub fn len(&self) -> usize {
        self.records.len()
    }

    /// Returns `true` if there are no records.
    pub fn is_empty(&self) -> bool {
        self.records.is_empty()
    }
}

// ---------------------------------------------------------------------------
// Story 6: Audit Trail
// ---------------------------------------------------------------------------

/// Audit event wrapper for all record types.
#[derive(Debug, Clone)]
pub enum AuditEvent {
    /// Authorization event.
    Auth(SmfType80Record),
    /// Profile change event.
    ProfileChange(SmfType80ProfileChange),
    /// Initialization event.
    Init(SmfType81Record),
    /// Database change event.
    DbChange(SmfType83Record),
}

/// Audit trail for collecting and querying RACF security events.
#[derive(Debug, Default)]
pub struct AuditTrail {
    /// All collected events in chronological order.
    events: Vec<AuditEvent>,
}

impl AuditTrail {
    /// Create a new empty audit trail.
    pub fn new() -> Self {
        Self::default()
    }

    /// Record an authorization event.
    pub fn record_auth(&mut self, record: SmfType80Record) {
        self.events.push(AuditEvent::Auth(record));
    }

    /// Record a profile change event.
    pub fn record_profile_change(&mut self, record: SmfType80ProfileChange) {
        self.events.push(AuditEvent::ProfileChange(record));
    }

    /// Record an initialization event.
    pub fn record_init(&mut self, record: SmfType81Record) {
        self.events.push(AuditEvent::Init(record));
    }

    /// Record a database change event.
    pub fn record_db_change(&mut self, record: SmfType83Record) {
        self.events.push(AuditEvent::DbChange(record));
    }

    /// Returns all events.
    pub fn events(&self) -> &[AuditEvent] {
        &self.events
    }

    /// Returns the total number of events.
    pub fn len(&self) -> usize {
        self.events.len()
    }

    /// Returns `true` if there are no events.
    pub fn is_empty(&self) -> bool {
        self.events.is_empty()
    }

    /// Query events for a specific user.
    pub fn events_for_user(&self, user: &str) -> Vec<&AuditEvent> {
        let upper = user.to_uppercase();
        self.events
            .iter()
            .filter(|e| match e {
                AuditEvent::Auth(r) => r.user == upper,
                AuditEvent::ProfileChange(r) => r.issuer == upper,
                AuditEvent::Init(_) => false,
                AuditEvent::DbChange(r) => r.changed_by == upper,
            })
            .collect()
    }

    /// Query events that resulted in failures.
    pub fn failures(&self) -> Vec<&AuditEvent> {
        self.events
            .iter()
            .filter(|e| matches!(e, AuditEvent::Auth(r) if r.result == AuthResult::Failure))
            .collect()
    }

    /// Query authorization events for a specific resource class.
    pub fn events_for_class(&self, class: &str) -> Vec<&AuditEvent> {
        let upper = class.to_uppercase();
        self.events
            .iter()
            .filter(|e| matches!(e, AuditEvent::Auth(r) if r.class == upper))
            .collect()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_auth_success() -> SmfType80Record {
        SmfType80Record::new(
            AuthEventType::ResourceAccess,
            "JSMITH",
            "SYS1.PARMLIB",
            "DATASET",
            "READ",
            AuthResult::Success,
            "2024-01-15T10:30:00Z",
        )
    }

    fn sample_auth_failure() -> SmfType80Record {
        SmfType80Record::new(
            AuthEventType::ResourceAccess,
            "HACKER",
            "SYS1.PARMLIB",
            "DATASET",
            "ALTER",
            AuthResult::Failure,
            "2024-01-15T10:31:00Z",
        )
    }

    fn sample_profile_change() -> SmfType80ProfileChange {
        SmfType80ProfileChange::new(
            ProfileChangeType::Permit,
            "ADMIN1",
            "SYS1.PARMLIB",
            "2024-01-15T10:32:00Z",
            "PERMIT USER(JSMITH) ACCESS(READ)",
        )
    }

    fn sample_init() -> SmfType81Record {
        SmfType81Record::new(
            InitEventType::Startup,
            "2024-01-15T08:00:00Z",
            "RACF initialized",
            "SYS1",
        )
    }

    fn sample_db_change() -> SmfType83Record {
        SmfType83Record::new(
            "USER",
            "JSMITH",
            "SPECIAL=NO",
            "SPECIAL=YES",
            "ADMIN1",
            "2024-01-15T10:33:00Z",
        )
    }

    // --- SMF Type 80 ---

    #[test]
    fn smf80_auth_display() {
        let r = sample_auth_success();
        let s = format!("{}", r);
        assert!(s.contains("SMF80"));
        assert!(s.contains("JSMITH"));
        assert!(s.contains("SUCCESS"));
    }

    #[test]
    fn smf80_user_uppercased() {
        let r = SmfType80Record::new(
            AuthEventType::Logon,
            "jsmith",
            "TSO",
            "FACILITY",
            "READ",
            AuthResult::Success,
            "2024-01-01T00:00:00Z",
        );
        assert_eq!(r.user, "JSMITH");
    }

    // --- SMF Type 80 Profile Change ---

    #[test]
    fn smf80_profile_change_display() {
        let r = sample_profile_change();
        let s = format!("{}", r);
        assert!(s.contains("PERMIT"));
        assert!(s.contains("ADMIN1"));
    }

    #[test]
    fn profile_change_types_display() {
        assert_eq!(format!("{}", ProfileChangeType::AddUser), "ADDUSER");
        assert_eq!(format!("{}", ProfileChangeType::Rdefine), "RDEFINE");
    }

    // --- SMF Type 81 ---

    #[test]
    fn smf81_display() {
        let r = sample_init();
        let s = format!("{}", r);
        assert!(s.contains("SMF81"));
        assert!(s.contains("STARTUP"));
        assert!(s.contains("SYS1"));
    }

    // --- SMF Type 83 ---

    #[test]
    fn smf83_display() {
        let r = sample_db_change();
        let s = format!("{}", r);
        assert!(s.contains("SMF83"));
        assert!(s.contains("JSMITH"));
        assert!(s.contains("ADMIN1"));
    }

    #[test]
    fn smf83_before_after() {
        let r = sample_db_change();
        assert_eq!(r.before_image, "SPECIAL=NO");
        assert_eq!(r.after_image, "SPECIAL=YES");
    }

    // --- IRRDBU00 ---

    #[test]
    fn irrdbu00_unload() {
        let mut unloader = Irrdbu00::new();
        unloader.add_record(UnloadRecord {
            record_type: UnloadRecordType::UserBasic,
            name: "JSMITH".into(),
            fields: vec![
                ("DFLTGRP".into(), "SYS1".into()),
                ("NAME".into(), "JOHN SMITH".into()),
            ],
        });
        unloader.add_record(UnloadRecord {
            record_type: UnloadRecordType::GroupBasic,
            name: "DEPT01".into(),
            fields: vec![("SUPGRP".into(), "SYS1".into())],
        });

        let lines = unloader.unload();
        assert_eq!(lines.len(), 2);
        assert!(lines[0].starts_with("0200 JSMITH"));
        assert!(lines[0].contains("DFLTGRP=SYS1"));
        assert!(lines[1].starts_with("0100 DEPT01"));
    }

    #[test]
    fn irrdbu00_empty() {
        let unloader = Irrdbu00::new();
        assert!(unloader.is_empty());
        assert_eq!(unloader.len(), 0);
        assert!(unloader.unload().is_empty());
    }

    #[test]
    fn unload_record_type_prefix() {
        assert_eq!(UnloadRecordType::UserBasic.prefix(), "0200");
        assert_eq!(UnloadRecordType::GroupBasic.prefix(), "0100");
        assert_eq!(UnloadRecordType::ConnectData.prefix(), "0205");
        assert_eq!(UnloadRecordType::DatasetAccess.prefix(), "0400");
        assert_eq!(UnloadRecordType::GeneralResource.prefix(), "0500");
    }

    // --- Audit Trail ---

    #[test]
    fn audit_trail_collect_and_query() {
        let mut trail = AuditTrail::new();
        assert!(trail.is_empty());

        trail.record_auth(sample_auth_success());
        trail.record_auth(sample_auth_failure());
        trail.record_profile_change(sample_profile_change());
        trail.record_init(sample_init());
        trail.record_db_change(sample_db_change());

        assert_eq!(trail.len(), 5);
    }

    #[test]
    fn audit_trail_events_for_user() {
        let mut trail = AuditTrail::new();
        trail.record_auth(sample_auth_success());
        trail.record_auth(sample_auth_failure());
        trail.record_profile_change(sample_profile_change());

        let jsmith_events = trail.events_for_user("JSMITH");
        assert_eq!(jsmith_events.len(), 1);

        let admin_events = trail.events_for_user("ADMIN1");
        assert_eq!(admin_events.len(), 1);
    }

    #[test]
    fn audit_trail_failures() {
        let mut trail = AuditTrail::new();
        trail.record_auth(sample_auth_success());
        trail.record_auth(sample_auth_failure());

        let failures = trail.failures();
        assert_eq!(failures.len(), 1);
    }

    #[test]
    fn audit_trail_events_for_class() {
        let mut trail = AuditTrail::new();
        trail.record_auth(sample_auth_success());
        trail.record_auth(sample_auth_failure());

        let dataset_events = trail.events_for_class("DATASET");
        assert_eq!(dataset_events.len(), 2);

        let facility_events = trail.events_for_class("FACILITY");
        assert!(facility_events.is_empty());
    }

    // --- Auth Event Type Display ---

    #[test]
    fn auth_event_type_display() {
        assert_eq!(format!("{}", AuthEventType::ResourceAccess), "RESOURCE_ACCESS");
        assert_eq!(format!("{}", AuthEventType::Logon), "LOGON");
        assert_eq!(format!("{}", AuthEventType::PasswordChange), "PASSWORD_CHANGE");
    }

    #[test]
    fn auth_result_display() {
        assert_eq!(format!("{}", AuthResult::Success), "SUCCESS");
        assert_eq!(format!("{}", AuthResult::Failure), "FAILURE");
        assert_eq!(format!("{}", AuthResult::Warning), "WARNING");
    }
}
