//! Type 80 — Security Audit Records.
//!
//! Tracks RACF security events:
//! - RACF command events (ADDUSER, ALTUSER, PERMIT, etc.)
//! - Access violation events
//! - Logon/logoff events
//! - Event severity and filtering

use crate::record::{extend_padded, push_u16, push_u32, SmfRecord};

// ---------------------------------------------------------------------------
//  Security event type
// ---------------------------------------------------------------------------

/// Type of security event recorded in a Type 80 record.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum SecurityEventType {
    /// RACF command execution (ADDUSER, ALTUSER, PERMIT, etc.).
    RacfCommand,
    /// Access violation (unauthorized access attempt).
    AccessViolation,
    /// Successful logon.
    Logon,
    /// Logoff.
    Logoff,
    /// Password change.
    PasswordChange,
    /// Profile access.
    ProfileAccess,
}

impl SecurityEventType {
    /// Get numeric subtype code.
    pub fn code(&self) -> u16 {
        match self {
            SecurityEventType::RacfCommand => 1,
            SecurityEventType::AccessViolation => 2,
            SecurityEventType::Logon => 3,
            SecurityEventType::Logoff => 4,
            SecurityEventType::PasswordChange => 5,
            SecurityEventType::ProfileAccess => 6,
        }
    }

    /// Construct from numeric code.
    pub fn from_code(code: u16) -> Option<Self> {
        match code {
            1 => Some(SecurityEventType::RacfCommand),
            2 => Some(SecurityEventType::AccessViolation),
            3 => Some(SecurityEventType::Logon),
            4 => Some(SecurityEventType::Logoff),
            5 => Some(SecurityEventType::PasswordChange),
            6 => Some(SecurityEventType::ProfileAccess),
            _ => None,
        }
    }
}

// ---------------------------------------------------------------------------
//  Event severity
// ---------------------------------------------------------------------------

/// Severity level for security events.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
pub enum EventSeverity {
    /// Informational event.
    Info,
    /// Warning (e.g., near-violation).
    Warning,
    /// Access violation or security failure.
    Violation,
    /// Critical security event.
    Critical,
}

impl EventSeverity {
    /// Get numeric code.
    pub fn code(&self) -> u8 {
        match self {
            EventSeverity::Info => 0,
            EventSeverity::Warning => 1,
            EventSeverity::Violation => 2,
            EventSeverity::Critical => 3,
        }
    }
}

// ---------------------------------------------------------------------------
//  Access level
// ---------------------------------------------------------------------------

/// RACF access level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum AccessLevel {
    /// No access.
    None,
    /// Execute access.
    Execute,
    /// Read access.
    Read,
    /// Update access.
    Update,
    /// Control access.
    Control,
    /// Alter access.
    Alter,
}

impl AccessLevel {
    /// Get display name.
    pub fn name(&self) -> &str {
        match self {
            AccessLevel::None => "NONE",
            AccessLevel::Execute => "EXECUTE",
            AccessLevel::Read => "READ",
            AccessLevel::Update => "UPDATE",
            AccessLevel::Control => "CONTROL",
            AccessLevel::Alter => "ALTER",
        }
    }

    /// Get numeric code.
    pub fn code(&self) -> u8 {
        match self {
            AccessLevel::None => 0,
            AccessLevel::Execute => 1,
            AccessLevel::Read => 2,
            AccessLevel::Update => 3,
            AccessLevel::Control => 4,
            AccessLevel::Alter => 5,
        }
    }
}

// ---------------------------------------------------------------------------
//  Type 80 Record
// ---------------------------------------------------------------------------

/// Type 80 record — RACF security audit.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Type80Record {
    /// Security event type.
    pub event_type: SecurityEventType,
    /// Event severity.
    pub severity: EventSeverity,
    /// User ID that triggered the event.
    pub user_id: String,
    /// Resource name (dataset, profile, etc.).
    pub resource_name: String,
    /// Resource class (DATASET, FACILITY, etc.).
    pub resource_class: String,
    /// Access level attempted.
    pub access_attempted: AccessLevel,
    /// Access level permitted (may be lower than attempted for violations).
    pub access_permitted: AccessLevel,
    /// Whether the access was allowed.
    pub access_allowed: bool,
    /// Command text (for RACF command events).
    pub command_text: String,
    /// Terminal ID (for logon/logoff events).
    pub terminal_id: String,
    /// Event timestamp (hundredths of seconds since midnight).
    pub event_time: u32,
    /// Result or reason code.
    pub result_code: u16,
}

impl Default for Type80Record {
    fn default() -> Self {
        Self {
            event_type: SecurityEventType::ProfileAccess,
            severity: EventSeverity::Info,
            user_id: String::new(),
            resource_name: String::new(),
            resource_class: String::new(),
            access_attempted: AccessLevel::None,
            access_permitted: AccessLevel::None,
            access_allowed: true,
            command_text: String::new(),
            terminal_id: String::new(),
            event_time: 0,
            result_code: 0,
        }
    }
}

impl Type80Record {
    /// Create a RACF command event record.
    pub fn racf_command(user_id: &str, command_text: &str, result_code: u16) -> Self {
        Self {
            event_type: SecurityEventType::RacfCommand,
            severity: EventSeverity::Info,
            user_id: user_id.to_string(),
            command_text: command_text.to_string(),
            result_code,
            access_allowed: result_code == 0,
            ..Default::default()
        }
    }

    /// Create an access violation record.
    pub fn access_violation(
        user_id: &str,
        resource_name: &str,
        resource_class: &str,
        access_attempted: AccessLevel,
    ) -> Self {
        Self {
            event_type: SecurityEventType::AccessViolation,
            severity: EventSeverity::Violation,
            user_id: user_id.to_string(),
            resource_name: resource_name.to_string(),
            resource_class: resource_class.to_string(),
            access_attempted,
            access_permitted: AccessLevel::None,
            access_allowed: false,
            ..Default::default()
        }
    }

    /// Create a logon event record.
    pub fn logon(user_id: &str, terminal_id: &str, event_time: u32) -> Self {
        Self {
            event_type: SecurityEventType::Logon,
            severity: EventSeverity::Info,
            user_id: user_id.to_string(),
            terminal_id: terminal_id.to_string(),
            event_time,
            access_allowed: true,
            ..Default::default()
        }
    }

    /// Create a logoff event record.
    pub fn logoff(user_id: &str, terminal_id: &str, event_time: u32) -> Self {
        Self {
            event_type: SecurityEventType::Logoff,
            severity: EventSeverity::Info,
            user_id: user_id.to_string(),
            terminal_id: terminal_id.to_string(),
            event_time,
            access_allowed: true,
            ..Default::default()
        }
    }

    /// Convert to a generic SMF record.
    pub fn to_record(&self) -> SmfRecord {
        let mut data = Vec::new();
        push_u16(&mut data, self.event_type.code());
        data.push(self.severity.code());
        data.push(if self.access_allowed { 1 } else { 0 });
        extend_padded(&mut data, &self.user_id, 8);
        extend_padded(&mut data, &self.resource_name, 44);
        extend_padded(&mut data, &self.resource_class, 8);
        data.push(self.access_attempted.code());
        data.push(self.access_permitted.code());
        extend_padded(&mut data, &self.command_text, 64);
        extend_padded(&mut data, &self.terminal_id, 8);
        push_u32(&mut data, self.event_time);
        push_u16(&mut data, self.result_code);

        let mut record = SmfRecord::new(80, data);
        record.header.subtype = self.event_type.code();
        record
    }
}

// ---------------------------------------------------------------------------
//  Event filter
// ---------------------------------------------------------------------------

/// Filter criteria for security events.
#[derive(Debug, Clone, Default)]
pub struct SecurityEventFilter {
    /// Filter by event types (empty = all).
    pub event_types: Vec<SecurityEventType>,
    /// Minimum severity level.
    pub min_severity: Option<EventSeverity>,
    /// Filter by user ID pattern.
    pub user_pattern: Option<String>,
    /// Only include violations.
    pub violations_only: bool,
}

impl SecurityEventFilter {
    /// Check if a record matches the filter.
    pub fn matches(&self, record: &Type80Record) -> bool {
        if !self.event_types.is_empty()
            && !self.event_types.contains(&record.event_type)
        {
            return false;
        }
        if let Some(min_sev) = &self.min_severity {
            if record.severity < *min_sev {
                return false;
            }
        }
        if let Some(pattern) = &self.user_pattern {
            if pattern.ends_with('*') {
                let prefix = &pattern[..pattern.len() - 1];
                if !record.user_id.starts_with(prefix) {
                    return false;
                }
            } else if record.user_id != *pattern {
                return false;
            }
        }
        if self.violations_only && record.access_allowed {
            return false;
        }
        true
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_event_type_codes() {
        assert_eq!(SecurityEventType::RacfCommand.code(), 1);
        assert_eq!(SecurityEventType::AccessViolation.code(), 2);
        assert_eq!(SecurityEventType::Logon.code(), 3);
        assert_eq!(SecurityEventType::Logoff.code(), 4);
    }

    #[test]
    fn test_event_type_from_code() {
        assert_eq!(
            SecurityEventType::from_code(1),
            Some(SecurityEventType::RacfCommand)
        );
        assert_eq!(SecurityEventType::from_code(99), None);
    }

    #[test]
    fn test_racf_command_record() {
        let rec = Type80Record::racf_command(
            "ADMIN01",
            "PERMIT PAYROLL.DATA CLASS(DATASET) ID(JSMITH) ACCESS(READ)",
            0,
        );
        assert_eq!(rec.event_type, SecurityEventType::RacfCommand);
        assert_eq!(rec.user_id, "ADMIN01");
        assert!(rec.access_allowed);
        assert_eq!(rec.result_code, 0);

        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 80);
        assert_eq!(smf.header.subtype, 1);
    }

    #[test]
    fn test_access_violation_record() {
        let rec = Type80Record::access_violation(
            "JSMITH",
            "PAYROLL.DATA",
            "DATASET",
            AccessLevel::Read,
        );
        assert_eq!(rec.event_type, SecurityEventType::AccessViolation);
        assert_eq!(rec.severity, EventSeverity::Violation);
        assert!(!rec.access_allowed);
        assert_eq!(rec.access_attempted, AccessLevel::Read);
        assert_eq!(rec.resource_name, "PAYROLL.DATA");
        assert_eq!(rec.resource_class, "DATASET");

        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 80);
        assert_eq!(smf.header.subtype, 2);
    }

    #[test]
    fn test_logon_record() {
        let rec = Type80Record::logon("JSMITH", "TERM001", 5220000);
        assert_eq!(rec.event_type, SecurityEventType::Logon);
        assert_eq!(rec.terminal_id, "TERM001");
        assert_eq!(rec.event_time, 5220000);
        assert!(rec.access_allowed);

        let smf = rec.to_record();
        assert_eq!(smf.header.subtype, 3);
    }

    #[test]
    fn test_logoff_record() {
        let rec = Type80Record::logoff("JSMITH", "TERM001", 6000000);
        assert_eq!(rec.event_type, SecurityEventType::Logoff);

        let smf = rec.to_record();
        assert_eq!(smf.header.subtype, 4);
    }

    #[test]
    fn test_severity_ordering() {
        assert!(EventSeverity::Info < EventSeverity::Warning);
        assert!(EventSeverity::Warning < EventSeverity::Violation);
        assert!(EventSeverity::Violation < EventSeverity::Critical);
    }

    #[test]
    fn test_access_level_names() {
        assert_eq!(AccessLevel::Read.name(), "READ");
        assert_eq!(AccessLevel::Update.name(), "UPDATE");
        assert_eq!(AccessLevel::Alter.name(), "ALTER");
    }

    #[test]
    fn test_event_filter_all() {
        let filter = SecurityEventFilter::default();
        let rec = Type80Record::logon("USER1", "T1", 0);
        assert!(filter.matches(&rec));
    }

    #[test]
    fn test_event_filter_by_type() {
        let filter = SecurityEventFilter {
            event_types: vec![SecurityEventType::AccessViolation],
            ..Default::default()
        };
        let violation = Type80Record::access_violation("U1", "R1", "DATASET", AccessLevel::Read);
        let logon = Type80Record::logon("U1", "T1", 0);
        assert!(filter.matches(&violation));
        assert!(!filter.matches(&logon));
    }

    #[test]
    fn test_event_filter_by_severity() {
        let filter = SecurityEventFilter {
            min_severity: Some(EventSeverity::Violation),
            ..Default::default()
        };
        let violation = Type80Record::access_violation("U1", "R1", "DATASET", AccessLevel::Read);
        let logon = Type80Record::logon("U1", "T1", 0); // Info severity
        assert!(filter.matches(&violation));
        assert!(!filter.matches(&logon));
    }

    #[test]
    fn test_event_filter_by_user_pattern() {
        let filter = SecurityEventFilter {
            user_pattern: Some("PAY*".to_string()),
            ..Default::default()
        };
        let rec1 = Type80Record::logon("PAYADM", "T1", 0);
        let rec2 = Type80Record::logon("ADMIN", "T1", 0);
        assert!(filter.matches(&rec1));
        assert!(!filter.matches(&rec2));
    }

    #[test]
    fn test_event_filter_violations_only() {
        let filter = SecurityEventFilter {
            violations_only: true,
            ..Default::default()
        };
        let violation = Type80Record::access_violation("U1", "R1", "DATASET", AccessLevel::Read);
        let logon = Type80Record::logon("U1", "T1", 0);
        assert!(filter.matches(&violation));
        assert!(!filter.matches(&logon));
    }

    #[test]
    fn test_user_id_in_record_data() {
        let rec = Type80Record::access_violation("JSMITH", "PAY.DATA", "DATASET", AccessLevel::Read);
        let smf = rec.to_record();
        // User ID at offset 4..12.
        let uid = String::from_utf8_lossy(&smf.data[4..12])
            .trim_end()
            .to_string();
        assert_eq!(uid, "JSMITH");
    }
}
