//! LE Message Services — message retrieval, condition token encoding/decoding.
//!
//! Implements the following LE callable services:
//! - **CEEMSG**  — Get and dispatch a runtime message to MSGFILE.
//! - **CEEMOUT** — Format and dispatch a message to output.
//! - **CEEMGET** — Get a message text into a caller-supplied buffer.
//! - **CEENCOD** — Encode a condition token from severity, message number, etc.
//! - **CEEDCOD** — Decode a condition token into its components.
//!
//! The LE condition token is a 12-byte structure containing severity,
//! message number, facility ID, and instance-specific information.

use crate::date_time::FeedbackCode;

// ---------------------------------------------------------------------------
//  Condition Token
// ---------------------------------------------------------------------------

/// A 12-byte LE condition token.
///
/// Layout:
/// - bytes 0-1: Condition_ID (u16)
/// - byte  2:   Severity (0-4)
/// - byte  3:   Control (reserved)
/// - bytes 4-6: Facility_ID (3-char, e.g. "CEE", "IGZ")
/// - byte  7:   ISI flag
/// - bytes 8-11: Message_Number (u32)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConditionToken {
    /// Condition identifier.
    pub condition_id: u16,
    /// Severity: 0=info, 1=warning, 2=error, 3=severe, 4=critical.
    pub severity: u8,
    /// Message number.
    pub msg_number: u32,
    /// Case (or control) severity.
    pub case_severity: u8,
    /// Facility identifier (e.g. "CEE", "IGZ", "IBM", "EDC").
    pub facility_id: String,
    /// Instance-specific information.
    pub isi: u32,
}

impl ConditionToken {
    /// Create a success (all-zero) token.
    pub fn success() -> Self {
        Self {
            condition_id: 0,
            severity: 0,
            msg_number: 0,
            case_severity: 0,
            facility_id: String::new(),
            isi: 0,
        }
    }

    /// Check if this token indicates success.
    pub fn is_success(&self) -> bool {
        self.severity == 0 && self.condition_id == 0
    }

    /// Encode the token into a 12-byte array.
    pub fn encode(&self) -> [u8; 12] {
        let mut buf = [0u8; 12];
        buf[0] = (self.condition_id >> 8) as u8;
        buf[1] = self.condition_id as u8;
        buf[2] = self.severity;
        buf[3] = self.case_severity;
        let fid = self.facility_id.as_bytes();
        for (i, byte) in fid.iter().take(3).enumerate() {
            buf[4 + i] = *byte;
        }
        buf[7] = (self.isi >> 24) as u8;
        buf[8] = (self.msg_number >> 24) as u8;
        buf[9] = (self.msg_number >> 16) as u8;
        buf[10] = (self.msg_number >> 8) as u8;
        buf[11] = self.msg_number as u8;
        buf
    }

    /// Decode a token from a 12-byte array.
    pub fn decode(buf: &[u8; 12]) -> Self {
        let condition_id = ((buf[0] as u16) << 8) | (buf[1] as u16);
        let severity = buf[2];
        let case_severity = buf[3];
        let facility_id = String::from_utf8_lossy(&buf[4..7])
            .trim_end_matches('\0')
            .to_string();
        let isi = (buf[7] as u32) << 24;
        let msg_number = ((buf[8] as u32) << 24)
            | ((buf[9] as u32) << 16)
            | ((buf[10] as u32) << 8)
            | (buf[11] as u32);
        Self {
            condition_id,
            severity,
            msg_number,
            case_severity,
            facility_id,
            isi,
        }
    }
}

// ---------------------------------------------------------------------------
//  Message Catalog
// ---------------------------------------------------------------------------

/// A message entry in the catalog.
#[derive(Debug, Clone)]
pub struct MessageEntry {
    /// Facility identifier.
    pub facility: String,
    /// Message number.
    pub number: u32,
    /// Severity level.
    pub severity: u8,
    /// Message text (may contain %1, %2 insert placeholders).
    pub text: String,
}

/// In-memory LE message catalog.
///
/// Stores messages indexed by (facility, message_number).
#[derive(Debug, Clone, Default)]
pub struct MessageCatalog {
    entries: Vec<MessageEntry>,
}

impl MessageCatalog {
    /// Create a new catalog pre-loaded with standard LE messages.
    pub fn new() -> Self {
        let mut cat = Self::default();
        cat.load_defaults();
        cat
    }

    /// Register a message in the catalog.
    pub fn register(&mut self, facility: &str, number: u32, severity: u8, text: &str) {
        self.entries.push(MessageEntry {
            facility: facility.to_string(),
            number,
            severity,
            text: text.to_string(),
        });
    }

    /// Look up a message by facility and number.
    pub fn get(&self, facility: &str, number: u32) -> Option<&MessageEntry> {
        self.entries
            .iter()
            .find(|e| e.facility == facility && e.number == number)
    }

    /// Load default LE system messages.
    fn load_defaults(&mut self) {
        // CEE system messages
        self.register("CEE", 1, 0, "Operation successful.");
        self.register("CEE", 198, 1, "The termination of a thread was signaled.");
        self.register("CEE", 199, 1, "The termination of an enclave was signaled.");
        self.register("CEE", 200, 3, "The condition was not handled by the user.");
        self.register("CEE", 201, 3, "A system abend was issued by the user.");
        self.register("CEE", 250, 2, "An insufficient amount of storage was available.");
        self.register("CEE", 251, 2, "The storage could not be obtained.");
        self.register("CEE", 350, 2, "Message not found for message number %1.");
        self.register("CEE", 500, 3, "An unhandled condition was signaled.");
        self.register("CEE", 2001, 2, "An invalid date value was specified.");
        self.register("CEE", 2002, 2, "An invalid date format was specified.");
        self.register("CEE", 2104, 3, "A math domain error occurred.");

        // IGZ COBOL runtime messages
        self.register("IGZ", 6, 2, "An INSPECT statement found an invalid argument.");
        self.register("IGZ", 13, 2, "The PERFORM range was invalid.");
        self.register("IGZ", 15, 2, "A subscript or index was out of range.");
        self.register("IGZ", 35, 3, "An arithmetic overflow condition occurred.");
        self.register("IGZ", 37, 2, "An invalid data exception occurred.");

        // IBM PL/I runtime messages
        self.register("IBM", 1001, 2, "A FIXED OVERFLOW condition was raised.");
        self.register("IBM", 1002, 2, "An OVERFLOW condition was raised.");
        self.register("IBM", 1003, 2, "An UNDERFLOW condition was raised.");
        self.register("IBM", 1004, 2, "A ZERODIVIDE condition was raised.");
        self.register("IBM", 1005, 2, "A CONVERSION condition was raised.");
    }
}

// ---------------------------------------------------------------------------
//  Callable Services
// ---------------------------------------------------------------------------

/// CEEMGET — Get a message text into a buffer.
///
/// Returns `(message_text, actual_length, FeedbackCode)`.
pub fn ceemget(
    catalog: &MessageCatalog,
    token: &ConditionToken,
) -> (String, usize, FeedbackCode) {
    let fac = if token.facility_id.is_empty() {
        "CEE"
    } else {
        &token.facility_id
    };
    match catalog.get(fac, token.msg_number) {
        Some(entry) => {
            let text = entry.text.clone();
            let len = text.len();
            (text, len, FeedbackCode::success())
        }
        None => {
            let text = format!(
                "{}{:04} Message not found.",
                fac, token.msg_number
            );
            let len = text.len();
            (text, len, FeedbackCode::error(350))
        }
    }
}

/// CEEMSG — Get and dispatch a runtime message to output.
///
/// Returns `(formatted_message, FeedbackCode)`.
pub fn ceemsg(
    catalog: &MessageCatalog,
    token: &ConditionToken,
) -> (String, FeedbackCode) {
    let (text, _, fc) = ceemget(catalog, token);
    let fac = if token.facility_id.is_empty() {
        "CEE"
    } else {
        &token.facility_id
    };
    let prefix = format!("{}{:04}{} ", fac, token.msg_number, severity_char(token.severity));
    (format!("{prefix}{text}"), fc)
}

/// CEEMOUT — Format and dispatch a message with inserts to output.
///
/// Returns `(formatted_message, FeedbackCode)`.
pub fn ceemout(
    catalog: &MessageCatalog,
    token: &ConditionToken,
    inserts: &[&str],
) -> (String, FeedbackCode) {
    let (mut text, _, fc) = ceemget(catalog, token);
    // Replace %1, %2, etc. with insert values.
    for (i, insert) in inserts.iter().enumerate() {
        let placeholder = format!("%{}", i + 1);
        text = text.replace(&placeholder, insert);
    }
    let fac = if token.facility_id.is_empty() {
        "CEE"
    } else {
        &token.facility_id
    };
    let prefix = format!("{}{:04}{} ", fac, token.msg_number, severity_char(token.severity));
    (format!("{prefix}{text}"), fc)
}

/// CEENCOD — Encode a condition token from components.
///
/// Returns `(ConditionToken, FeedbackCode)`.
pub fn ceencod(
    severity: u8,
    msg_number: u32,
    case_severity: u8,
    facility_id: &str,
    isi: u32,
) -> (ConditionToken, FeedbackCode) {
    if severity > 4 {
        return (
            ConditionToken::success(),
            FeedbackCode::error(200),
        );
    }
    let token = ConditionToken {
        condition_id: 1,
        severity,
        msg_number,
        case_severity,
        facility_id: facility_id.to_string(),
        isi,
    };
    (token, FeedbackCode::success())
}

/// CEEDCOD — Decode a condition token into components.
///
/// Returns `(severity, msg_number, case_severity, facility_id, isi, FeedbackCode)`.
pub fn ceedcod(
    token: &ConditionToken,
) -> (u8, u32, u8, String, u32, FeedbackCode) {
    (
        token.severity,
        token.msg_number,
        token.case_severity,
        token.facility_id.clone(),
        token.isi,
        FeedbackCode::success(),
    )
}

fn severity_char(severity: u8) -> char {
    match severity {
        0 => 'I',
        1 => 'W',
        2 => 'E',
        3 => 'S',
        4 => 'C',
        _ => 'U',
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // ─── LE105.1: Message Retrieval and Condition Tokens ───

    #[test]
    fn test_condition_token_success() {
        let token = ConditionToken::success();
        assert!(token.is_success());
        assert_eq!(token.severity, 0);
    }

    #[test]
    fn test_condition_token_encode_decode_roundtrip() {
        let token = ConditionToken {
            condition_id: 1,
            severity: 2,
            msg_number: 2104,
            case_severity: 2,
            facility_id: "CEE".to_string(),
            isi: 0,
        };
        let encoded = token.encode();
        let decoded = ConditionToken::decode(&encoded);
        assert_eq!(decoded.condition_id, token.condition_id);
        assert_eq!(decoded.severity, token.severity);
        assert_eq!(decoded.msg_number, token.msg_number);
        assert_eq!(decoded.case_severity, token.case_severity);
        assert_eq!(decoded.facility_id, token.facility_id);
    }

    #[test]
    fn test_ceencod_ceedcod_roundtrip() {
        let (token, fc) = ceencod(2, 2104, 2, "CEE", 0);
        assert!(fc.is_success());
        assert_eq!(token.severity, 2);
        assert_eq!(token.msg_number, 2104);
        assert_eq!(token.facility_id, "CEE");

        let (sev, num, csev, fac, isi, fc) = ceedcod(&token);
        assert!(fc.is_success());
        assert_eq!(sev, 2);
        assert_eq!(num, 2104);
        assert_eq!(csev, 2);
        assert_eq!(fac, "CEE");
        assert_eq!(isi, 0);
    }

    #[test]
    fn test_ceencod_invalid_severity() {
        let (_, fc) = ceencod(5, 100, 0, "CEE", 0);
        assert!(!fc.is_success());
    }

    #[test]
    fn test_message_catalog_default() {
        let cat = MessageCatalog::new();
        let entry = cat.get("CEE", 2104).unwrap();
        assert_eq!(entry.severity, 3);
        assert!(entry.text.contains("math domain"));
    }

    #[test]
    fn test_message_catalog_igz() {
        let cat = MessageCatalog::new();
        let entry = cat.get("IGZ", 35).unwrap();
        assert_eq!(entry.severity, 3);
        assert!(entry.text.contains("arithmetic overflow"));
    }

    #[test]
    fn test_message_catalog_ibm() {
        let cat = MessageCatalog::new();
        let entry = cat.get("IBM", 1004).unwrap();
        assert_eq!(entry.severity, 2);
        assert!(entry.text.contains("ZERODIVIDE"));
    }

    #[test]
    fn test_message_catalog_not_found() {
        let cat = MessageCatalog::new();
        assert!(cat.get("CEE", 9999).is_none());
    }

    #[test]
    fn test_ceemget_found() {
        let cat = MessageCatalog::new();
        let token = ConditionToken {
            condition_id: 1,
            severity: 3,
            msg_number: 2104,
            case_severity: 3,
            facility_id: "CEE".to_string(),
            isi: 0,
        };
        let (text, len, fc) = ceemget(&cat, &token);
        assert!(fc.is_success());
        assert!(len > 0);
        assert!(text.contains("math domain"));
    }

    #[test]
    fn test_ceemget_not_found() {
        let cat = MessageCatalog::new();
        let token = ConditionToken {
            condition_id: 1,
            severity: 2,
            msg_number: 9999,
            case_severity: 2,
            facility_id: "CEE".to_string(),
            isi: 0,
        };
        let (text, _, fc) = ceemget(&cat, &token);
        assert!(!fc.is_success());
        assert!(text.contains("Message not found"));
    }

    #[test]
    fn test_ceemsg_format() {
        let cat = MessageCatalog::new();
        let token = ConditionToken {
            condition_id: 1,
            severity: 2,
            msg_number: 2001,
            case_severity: 2,
            facility_id: "CEE".to_string(),
            isi: 0,
        };
        let (msg, fc) = ceemsg(&cat, &token);
        assert!(fc.is_success());
        assert!(msg.starts_with("CEE2001E"));
    }

    #[test]
    fn test_ceemout_with_inserts() {
        let mut cat = MessageCatalog::new();
        cat.register("TST", 1, 2, "Value %1 is not in range %2.");
        let token = ConditionToken {
            condition_id: 1,
            severity: 2,
            msg_number: 1,
            case_severity: 2,
            facility_id: "TST".to_string(),
            isi: 0,
        };
        let (msg, fc) = ceemout(&cat, &token, &["42", "0-100"]);
        assert!(fc.is_success());
        assert!(msg.contains("Value 42 is not in range 0-100"));
    }

    #[test]
    fn test_severity_chars() {
        assert_eq!(severity_char(0), 'I');
        assert_eq!(severity_char(1), 'W');
        assert_eq!(severity_char(2), 'E');
        assert_eq!(severity_char(3), 'S');
        assert_eq!(severity_char(4), 'C');
    }

    #[test]
    fn test_encode_decode_all_zeros() {
        let token = ConditionToken::success();
        let encoded = token.encode();
        assert_eq!(encoded, [0; 12]);
        let decoded = ConditionToken::decode(&encoded);
        assert!(decoded.is_success());
    }

    #[test]
    fn test_custom_catalog_entry() {
        let mut cat = MessageCatalog::default();
        cat.register("APP", 100, 1, "Custom warning message.");
        let entry = cat.get("APP", 100).unwrap();
        assert_eq!(entry.severity, 1);
        assert_eq!(entry.text, "Custom warning message.");
    }
}
