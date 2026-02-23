//! Daemon Infrastructure & Codepage Services (USS-110).
//!
//! Provides:
//! - inetd super-server daemon
//! - cron scheduler and syslogd
//! - Codepage auto-conversion and file tagging

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors for daemon and codepage operations.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, miette::Diagnostic)]
pub enum DaemonError {
    /// Service not found.
    #[error("service not found: {name}")]
    ServiceNotFound { name: String },

    /// Invalid crontab entry.
    #[error("invalid crontab entry: {detail}")]
    InvalidCrontab { detail: String },

    /// Invalid codepage.
    #[error("unknown codepage: {ccsid}")]
    UnknownCodepage { ccsid: u16 },

    /// Conversion error.
    #[error("codepage conversion error: {detail}")]
    ConversionError { detail: String },
}

// ---------------------------------------------------------------------------
//  inetd
// ---------------------------------------------------------------------------

/// Protocol for an inetd service.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InetProtocol {
    Tcp,
    Udp,
}

/// Wait mode for an inetd service.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InetWait {
    Wait,
    NoWait,
}

/// An entry in /etc/inetd.conf.
#[derive(Debug, Clone)]
pub struct InetdEntry {
    /// Service name (e.g., "telnet").
    pub service: String,
    /// Socket type (stream, dgram).
    pub socket_type: String,
    /// Protocol.
    pub protocol: InetProtocol,
    /// Wait mode.
    pub wait: InetWait,
    /// User to run as.
    pub user: String,
    /// Server program path.
    pub server_program: String,
    /// Server arguments.
    pub server_args: Vec<String>,
}

/// The inetd super-server daemon.
#[derive(Debug)]
pub struct Inetd {
    /// Configuration entries.
    entries: Vec<InetdEntry>,
    /// Active connections: service -> Vec<pid>.
    active: HashMap<String, Vec<u32>>,
}

impl Inetd {
    /// Create a new inetd daemon.
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            active: HashMap::new(),
        }
    }

    /// Load configuration from /etc/inetd.conf content.
    pub fn load_config(&mut self, content: &str) {
        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() >= 6 {
                let protocol = match parts[2] {
                    "tcp" => InetProtocol::Tcp,
                    _ => InetProtocol::Udp,
                };
                let wait = match parts[3] {
                    "wait" => InetWait::Wait,
                    _ => InetWait::NoWait,
                };
                self.entries.push(InetdEntry {
                    service: parts[0].to_string(),
                    socket_type: parts[1].to_string(),
                    protocol,
                    wait,
                    user: parts[4].to_string(),
                    server_program: parts[5].to_string(),
                    server_args: parts[6..].iter().map(|s| s.to_string()).collect(),
                });
            }
        }
    }

    /// Get a service entry by name.
    pub fn get_service(&self, name: &str) -> Option<&InetdEntry> {
        self.entries.iter().find(|e| e.service == name)
    }

    /// Spawn a service handler (simulated — records PID).
    pub fn spawn_handler(&mut self, service: &str, pid: u32) -> Result<(), DaemonError> {
        if !self.entries.iter().any(|e| e.service == service) {
            return Err(DaemonError::ServiceNotFound {
                name: service.to_string(),
            });
        }
        self.active
            .entry(service.to_string())
            .or_default()
            .push(pid);
        Ok(())
    }

    /// Get active handlers for a service.
    pub fn active_handlers(&self, service: &str) -> &[u32] {
        self.active
            .get(service)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// Number of configured services.
    pub fn service_count(&self) -> usize {
        self.entries.len()
    }
}

impl Default for Inetd {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
//  cron
// ---------------------------------------------------------------------------

/// A crontab schedule.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CronSchedule {
    /// Minute (0-59 or * for any).
    pub minute: CronField,
    /// Hour (0-23 or * for any).
    pub hour: CronField,
    /// Day of month (1-31 or *).
    pub day_of_month: CronField,
    /// Month (1-12 or *).
    pub month: CronField,
    /// Day of week (0-7, 0 and 7 = Sunday, or *).
    pub day_of_week: CronField,
}

/// A cron field value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CronField {
    /// Any value (*).
    Any,
    /// Specific value.
    Value(u32),
    /// Range (e.g., 1-5).
    Range(u32, u32),
    /// Step (e.g., */5).
    Step(u32),
}

/// A crontab entry.
#[derive(Debug, Clone)]
pub struct CrontabEntry {
    /// Schedule.
    pub schedule: CronSchedule,
    /// Command to run.
    pub command: String,
    /// User to run as.
    pub user: String,
}

/// Parse a cron field.
pub fn parse_cron_field(s: &str) -> Result<CronField, DaemonError> {
    if s == "*" {
        return Ok(CronField::Any);
    }
    if let Some(step) = s.strip_prefix("*/") {
        let n: u32 = step.parse().map_err(|_| DaemonError::InvalidCrontab {
            detail: format!("invalid step: {s}"),
        })?;
        return Ok(CronField::Step(n));
    }
    if let Some(dash_pos) = s.find('-') {
        let start: u32 = s[..dash_pos]
            .parse()
            .map_err(|_| DaemonError::InvalidCrontab {
                detail: format!("invalid range: {s}"),
            })?;
        let end: u32 = s[dash_pos + 1..]
            .parse()
            .map_err(|_| DaemonError::InvalidCrontab {
                detail: format!("invalid range: {s}"),
            })?;
        return Ok(CronField::Range(start, end));
    }
    let n: u32 = s.parse().map_err(|_| DaemonError::InvalidCrontab {
        detail: format!("invalid field: {s}"),
    })?;
    Ok(CronField::Value(n))
}

/// Parse a crontab line.
pub fn parse_crontab_entry(line: &str) -> Result<CrontabEntry, DaemonError> {
    let parts: Vec<&str> = line.split_whitespace().collect();
    if parts.len() < 6 {
        return Err(DaemonError::InvalidCrontab {
            detail: "expected at least 6 fields".to_string(),
        });
    }

    let schedule = CronSchedule {
        minute: parse_cron_field(parts[0])?,
        hour: parse_cron_field(parts[1])?,
        day_of_month: parse_cron_field(parts[2])?,
        month: parse_cron_field(parts[3])?,
        day_of_week: parse_cron_field(parts[4])?,
    };

    let command = parts[5..].join(" ");

    Ok(CrontabEntry {
        schedule,
        command,
        user: String::new(),
    })
}

/// Check if a cron field matches a value.
pub fn cron_field_matches(field: &CronField, value: u32) -> bool {
    match field {
        CronField::Any => true,
        CronField::Value(v) => *v == value,
        CronField::Range(start, end) => value >= *start && value <= *end,
        CronField::Step(step) => value % step == 0,
    }
}

/// Check if a cron schedule matches the given time.
pub fn cron_matches(
    schedule: &CronSchedule,
    minute: u32,
    hour: u32,
    day: u32,
    month: u32,
    weekday: u32,
) -> bool {
    cron_field_matches(&schedule.minute, minute)
        && cron_field_matches(&schedule.hour, hour)
        && cron_field_matches(&schedule.day_of_month, day)
        && cron_field_matches(&schedule.month, month)
        && cron_field_matches(&schedule.day_of_week, weekday)
}

// ---------------------------------------------------------------------------
//  syslogd
// ---------------------------------------------------------------------------

/// Syslog severity level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SyslogLevel {
    Emergency = 0,
    Alert = 1,
    Critical = 2,
    Error = 3,
    Warning = 4,
    Notice = 5,
    Info = 6,
    Debug = 7,
}

impl std::fmt::Display for SyslogLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Emergency => write!(f, "EMERG"),
            Self::Alert => write!(f, "ALERT"),
            Self::Critical => write!(f, "CRIT"),
            Self::Error => write!(f, "ERR"),
            Self::Warning => write!(f, "WARNING"),
            Self::Notice => write!(f, "NOTICE"),
            Self::Info => write!(f, "INFO"),
            Self::Debug => write!(f, "DEBUG"),
        }
    }
}

/// A syslog message.
#[derive(Debug, Clone)]
pub struct SyslogMessage {
    /// Severity level.
    pub level: SyslogLevel,
    /// Message text.
    pub message: String,
    /// Facility (e.g., "daemon", "auth").
    pub facility: String,
}

/// Simple syslog daemon.
#[derive(Debug)]
pub struct Syslogd {
    /// Log file path.
    pub log_file: String,
    /// Minimum log level.
    pub min_level: SyslogLevel,
    /// Collected messages.
    pub messages: Vec<SyslogMessage>,
}

impl Syslogd {
    /// Create a new syslogd.
    pub fn new(log_file: &str) -> Self {
        Self {
            log_file: log_file.to_string(),
            min_level: SyslogLevel::Info,
            messages: Vec::new(),
        }
    }

    /// Log a message.
    pub fn log(&mut self, level: SyslogLevel, facility: &str, message: &str) {
        if level <= self.min_level {
            self.messages.push(SyslogMessage {
                level,
                message: message.to_string(),
                facility: facility.to_string(),
            });
        }
    }

    /// Get all messages.
    pub fn get_messages(&self) -> &[SyslogMessage] {
        &self.messages
    }

    /// Get messages filtered by level.
    pub fn get_messages_by_level(&self, level: SyslogLevel) -> Vec<&SyslogMessage> {
        self.messages.iter().filter(|m| m.level == level).collect()
    }
}

// ---------------------------------------------------------------------------
//  Codepage Auto-Conversion
// ---------------------------------------------------------------------------

/// Well-known codepage IDs.
pub const CCSID_IBM_1047: u16 = 1047; // EBCDIC
pub const CCSID_ISO_8859_1: u16 = 819; // ASCII/Latin-1
pub const CCSID_UTF_8: u16 = 1208;
pub const CCSID_BINARY: u16 = 65535; // No conversion

/// A file tag (codepage tag).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileTag {
    /// CCSID.
    pub ccsid: u16,
    /// Text flag (true = text file, false = binary).
    pub text_flag: bool,
}

impl FileTag {
    /// Create a tag for IBM-1047 (EBCDIC).
    pub fn ebcdic() -> Self {
        Self {
            ccsid: CCSID_IBM_1047,
            text_flag: true,
        }
    }

    /// Create a tag for ISO 8859-1 (ASCII).
    pub fn ascii() -> Self {
        Self {
            ccsid: CCSID_ISO_8859_1,
            text_flag: true,
        }
    }

    /// Create a tag for UTF-8.
    pub fn utf8() -> Self {
        Self {
            ccsid: CCSID_UTF_8,
            text_flag: true,
        }
    }

    /// Create a binary (untagged) tag.
    pub fn binary() -> Self {
        Self {
            ccsid: CCSID_BINARY,
            text_flag: false,
        }
    }
}

/// Auto-conversion settings.
#[derive(Debug, Clone)]
pub struct AutoConvertSettings {
    /// Whether auto-conversion is enabled (_BPXK_AUTOCVT).
    pub enabled: bool,
    /// Program CCSID (the CCSID the program expects).
    pub program_ccsid: u16,
}

impl Default for AutoConvertSettings {
    fn default() -> Self {
        Self {
            enabled: false,
            program_ccsid: CCSID_IBM_1047,
        }
    }
}

/// Simple iconv (codepage conversion).
/// This is a minimal simulation — maps a few common characters.
pub fn iconv(from_ccsid: u16, to_ccsid: u16, data: &[u8]) -> Result<Vec<u8>, DaemonError> {
    // If same codepage, no conversion needed.
    if from_ccsid == to_ccsid {
        return Ok(data.to_vec());
    }

    // IBM-1047 (EBCDIC) <-> ISO 8859-1 (ASCII) conversion table for common chars.
    if from_ccsid == CCSID_IBM_1047 && to_ccsid == CCSID_ISO_8859_1 {
        return Ok(data.iter().map(|&b| ebcdic_to_ascii(b)).collect());
    }

    if from_ccsid == CCSID_ISO_8859_1 && to_ccsid == CCSID_IBM_1047 {
        return Ok(data.iter().map(|&b| ascii_to_ebcdic(b)).collect());
    }

    Err(DaemonError::ConversionError {
        detail: format!("unsupported conversion: CCSID {from_ccsid} -> {to_ccsid}"),
    })
}

/// EBCDIC (IBM-1047) to ASCII (ISO 8859-1) for common characters.
fn ebcdic_to_ascii(b: u8) -> u8 {
    match b {
        0xC1..=0xC9 => b - 0xC1 + b'A', // A-I
        0xD1..=0xD9 => b - 0xD1 + b'J', // J-R
        0xE2..=0xE9 => b - 0xE2 + b'S', // S-Z
        0x81..=0x89 => b - 0x81 + b'a', // a-i
        0x91..=0x99 => b - 0x91 + b'j', // j-r
        0xA2..=0xA9 => b - 0xA2 + b's', // s-z
        0xF0..=0xF9 => b - 0xF0 + b'0', // 0-9
        0x40 => b' ',                    // space
        0x4B => b'.',                    // period
        0x7D => b'\'',                   // apostrophe
        0x6B => b',',                    // comma
        0x15 => b'\n',                   // newline
        _ => b'?',                       // unmapped
    }
}

/// ASCII (ISO 8859-1) to EBCDIC (IBM-1047) for common characters.
fn ascii_to_ebcdic(b: u8) -> u8 {
    match b {
        b'A'..=b'I' => b - b'A' + 0xC1,
        b'J'..=b'R' => b - b'J' + 0xD1,
        b'S'..=b'Z' => b - b'S' + 0xE2,
        b'a'..=b'i' => b - b'a' + 0x81,
        b'j'..=b'r' => b - b'j' + 0x91,
        b's'..=b'z' => b - b's' + 0xA2,
        b'0'..=b'9' => b - b'0' + 0xF0,
        b' ' => 0x40,
        b'.' => 0x4B,
        b'\'' => 0x7D,
        b',' => 0x6B,
        b'\n' => 0x15,
        _ => 0x6F, // unmapped -> '?'
    }
}

/// Apply auto-conversion on read.
pub fn auto_convert_read(
    data: &[u8],
    file_tag: &FileTag,
    settings: &AutoConvertSettings,
) -> Result<Vec<u8>, DaemonError> {
    if !settings.enabled || !file_tag.text_flag || file_tag.ccsid == CCSID_BINARY {
        return Ok(data.to_vec());
    }
    if file_tag.ccsid == settings.program_ccsid {
        return Ok(data.to_vec());
    }
    iconv(file_tag.ccsid, settings.program_ccsid, data)
}

/// chtag equivalent — create a file tag.
pub fn chtag(ccsid: u16, text_flag: bool) -> FileTag {
    FileTag { ccsid, text_flag }
}

// ===========================================================================
//  Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_inetd_load_config() {
        let config = "telnet stream tcp nowait root /usr/sbin/telnetd telnetd\nftp stream tcp nowait root /usr/sbin/ftpd ftpd";
        let mut inetd = Inetd::new();
        inetd.load_config(config);
        assert_eq!(inetd.service_count(), 2);

        let telnet = inetd.get_service("telnet").unwrap();
        assert_eq!(telnet.protocol, InetProtocol::Tcp);
        assert_eq!(telnet.wait, InetWait::NoWait);
        assert_eq!(telnet.server_program, "/usr/sbin/telnetd");
    }

    #[test]
    fn test_inetd_spawn_handler() {
        let config = "telnet stream tcp nowait root /usr/sbin/telnetd telnetd";
        let mut inetd = Inetd::new();
        inetd.load_config(config);

        inetd.spawn_handler("telnet", 100).unwrap();
        assert_eq!(inetd.active_handlers("telnet"), &[100]);
    }

    #[test]
    fn test_inetd_unknown_service() {
        let mut inetd = Inetd::new();
        let err = inetd.spawn_handler("unknown", 1).unwrap_err();
        assert!(matches!(err, DaemonError::ServiceNotFound { .. }));
    }

    #[test]
    fn test_cron_parse_every_hour() {
        let entry = parse_crontab_entry("0 * * * * /usr/local/bin/cleanup.sh").unwrap();
        assert_eq!(entry.schedule.minute, CronField::Value(0));
        assert_eq!(entry.schedule.hour, CronField::Any);
        assert_eq!(entry.command, "/usr/local/bin/cleanup.sh");
    }

    #[test]
    fn test_cron_field_matches() {
        assert!(cron_field_matches(&CronField::Any, 42));
        assert!(cron_field_matches(&CronField::Value(5), 5));
        assert!(!cron_field_matches(&CronField::Value(5), 6));
        assert!(cron_field_matches(&CronField::Range(1, 5), 3));
        assert!(!cron_field_matches(&CronField::Range(1, 5), 6));
        assert!(cron_field_matches(&CronField::Step(5), 15));
        assert!(!cron_field_matches(&CronField::Step(5), 7));
    }

    #[test]
    fn test_cron_matches_hourly() {
        let schedule = CronSchedule {
            minute: CronField::Value(0),
            hour: CronField::Any,
            day_of_month: CronField::Any,
            month: CronField::Any,
            day_of_week: CronField::Any,
        };
        // At the top of every hour.
        assert!(cron_matches(&schedule, 0, 14, 23, 2, 1));
        assert!(!cron_matches(&schedule, 30, 14, 23, 2, 1));
    }

    #[test]
    fn test_syslogd() {
        let mut syslog = Syslogd::new("/var/log/syslog");
        syslog.log(SyslogLevel::Error, "daemon", "disk full");
        syslog.log(SyslogLevel::Info, "auth", "user logged in");

        assert_eq!(syslog.get_messages().len(), 2);
        let errs = syslog.get_messages_by_level(SyslogLevel::Error);
        assert_eq!(errs.len(), 1);
        assert_eq!(errs[0].message, "disk full");
    }

    #[test]
    fn test_syslog_level_display() {
        assert_eq!(SyslogLevel::Error.to_string(), "ERR");
        assert_eq!(SyslogLevel::Info.to_string(), "INFO");
        assert_eq!(SyslogLevel::Emergency.to_string(), "EMERG");
    }

    #[test]
    fn test_iconv_ebcdic_to_ascii() {
        // "HELLO" in EBCDIC (IBM-1047).
        let ebcdic = vec![0xC8, 0xC5, 0xD3, 0xD3, 0xD6];
        let ascii = iconv(CCSID_IBM_1047, CCSID_ISO_8859_1, &ebcdic).unwrap();
        assert_eq!(&ascii, b"HELLO");
    }

    #[test]
    fn test_iconv_ascii_to_ebcdic() {
        let ascii = b"HELLO";
        let ebcdic = iconv(CCSID_ISO_8859_1, CCSID_IBM_1047, ascii).unwrap();
        assert_eq!(ebcdic, vec![0xC8, 0xC5, 0xD3, 0xD3, 0xD6]);
    }

    #[test]
    fn test_iconv_roundtrip() {
        let original = b"Hello World 123";
        let ebcdic = iconv(CCSID_ISO_8859_1, CCSID_IBM_1047, original).unwrap();
        let back = iconv(CCSID_IBM_1047, CCSID_ISO_8859_1, &ebcdic).unwrap();
        assert_eq!(&back, original);
    }

    #[test]
    fn test_iconv_same_ccsid() {
        let data = b"no conversion";
        let result = iconv(819, 819, data).unwrap();
        assert_eq!(&result, data);
    }

    #[test]
    fn test_auto_convert_read() {
        let ascii_data = b"Hello";
        let tag = FileTag::ascii();
        let settings = AutoConvertSettings {
            enabled: true,
            program_ccsid: CCSID_IBM_1047,
        };
        let result = auto_convert_read(ascii_data, &tag, &settings).unwrap();
        // Should convert ASCII -> EBCDIC.
        let expected = iconv(CCSID_ISO_8859_1, CCSID_IBM_1047, ascii_data).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_auto_convert_disabled() {
        let data = b"no change";
        let tag = FileTag::ascii();
        let settings = AutoConvertSettings {
            enabled: false,
            ..Default::default()
        };
        let result = auto_convert_read(data, &tag, &settings).unwrap();
        assert_eq!(&result, data);
    }

    #[test]
    fn test_chtag() {
        let tag = chtag(CCSID_ISO_8859_1, true);
        assert_eq!(tag.ccsid, 819);
        assert!(tag.text_flag);
    }

    #[test]
    fn test_file_tag_constructors() {
        assert_eq!(FileTag::ebcdic().ccsid, 1047);
        assert_eq!(FileTag::ascii().ccsid, 819);
        assert_eq!(FileTag::utf8().ccsid, 1208);
        assert_eq!(FileTag::binary().ccsid, 65535);
        assert!(!FileTag::binary().text_flag);
    }

    // Map 'O' specifically since our table doesn't have all characters
    #[test]
    fn test_iconv_lowercase() {
        let ascii = b"hello";
        let ebcdic = iconv(CCSID_ISO_8859_1, CCSID_IBM_1047, ascii).unwrap();
        let back = iconv(CCSID_IBM_1047, CCSID_ISO_8859_1, &ebcdic).unwrap();
        assert_eq!(&back, ascii);
    }
}
