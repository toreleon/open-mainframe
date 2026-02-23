//! ADA-103: Direct Call Interface — ACB (5 stories).
//!
//! Provides the ADABAS Control Block (ACB) structure, ACB command codes,
//! format buffer parsing, and command execution dispatch.

use std::collections::HashMap;

use crate::AdabasError;
use crate::storage::Isn;

// ── AcbCommand ─────────────────────────────────────────────────────

/// ADABAS command codes used in the ACB.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AcbCommand {
    /// L1 — Read record by ISN.
    L1,
    /// L2 — Read physical sequential.
    L2,
    /// L3 — Read logical sequential by descriptor.
    L3,
    /// L4 — Read by ISN from ISN list.
    L4,
    /// L5 — Read physical sequential (variant).
    L5,
    /// L6 — Read sorted.
    L6,
    /// L9 — Read histogram (descriptor value distribution).
    L9,
    /// S1 — Find records matching criteria (first call).
    S1,
    /// S2 — Find records matching criteria (continuation).
    S2,
    /// S4 — Find and sort results.
    S4,
    /// S8 — Find with multiple search criteria.
    S8,
    /// S9 — Sort ISN list.
    S9,
    /// A1 — Update a record.
    A1,
    /// E1 — Delete a record.
    E1,
    /// N1 — Store a new record (system-assigned ISN).
    N1,
    /// N2 — Store a new record (user-supplied ISN).
    N2,
    /// ET — End transaction (commit).
    Et,
    /// BT — Backout transaction (rollback).
    Bt,
    /// OP — Open (session start).
    Op,
    /// CL — Close (session end).
    Cl,
}

impl AcbCommand {
    /// Parse a two-character command code string into an `AcbCommand`.
    pub fn parse(code: &str) -> Result<Self, AdabasError> {
        match code.to_uppercase().as_str() {
            "L1" => Ok(Self::L1),
            "L2" => Ok(Self::L2),
            "L3" => Ok(Self::L3),
            "L4" => Ok(Self::L4),
            "L5" => Ok(Self::L5),
            "L6" => Ok(Self::L6),
            "L9" => Ok(Self::L9),
            "S1" => Ok(Self::S1),
            "S2" => Ok(Self::S2),
            "S4" => Ok(Self::S4),
            "S8" => Ok(Self::S8),
            "S9" => Ok(Self::S9),
            "A1" => Ok(Self::A1),
            "E1" => Ok(Self::E1),
            "N1" => Ok(Self::N1),
            "N2" => Ok(Self::N2),
            "ET" => Ok(Self::Et),
            "BT" => Ok(Self::Bt),
            "OP" => Ok(Self::Op),
            "CL" => Ok(Self::Cl),
            _ => Err(AdabasError::InvalidCommand {
                code: code.to_string(),
            }),
        }
    }

    /// Return the two-character string representation.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::L1 => "L1",
            Self::L2 => "L2",
            Self::L3 => "L3",
            Self::L4 => "L4",
            Self::L5 => "L5",
            Self::L6 => "L6",
            Self::L9 => "L9",
            Self::S1 => "S1",
            Self::S2 => "S2",
            Self::S4 => "S4",
            Self::S8 => "S8",
            Self::S9 => "S9",
            Self::A1 => "A1",
            Self::E1 => "E1",
            Self::N1 => "N1",
            Self::N2 => "N2",
            Self::Et => "ET",
            Self::Bt => "BT",
            Self::Op => "OP",
            Self::Cl => "CL",
        }
    }
}

impl std::fmt::Display for AcbCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

// ── FieldRef ───────────────────────────────────────────────────────

/// A reference to a field within a format buffer specification.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldRef {
    /// The field name (2 characters).
    pub name: String,
    /// Optional length override.
    pub length: Option<u16>,
    /// Optional format override (e.g., "A", "N", "P").
    pub format: Option<String>,
}

// ── FormatBuffer ───────────────────────────────────────────────────

/// Parsed format buffer specifying which fields to read/write.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormatBuffer {
    /// The field references in order.
    pub fields: Vec<FieldRef>,
}

impl FormatBuffer {
    /// Create an empty format buffer.
    pub fn new() -> Self {
        Self {
            fields: Vec::new(),
        }
    }

    /// Add a field reference.
    pub fn add_field(&mut self, field_ref: FieldRef) {
        self.fields.push(field_ref);
    }
}

impl Default for FormatBuffer {
    fn default() -> Self {
        Self::new()
    }
}

/// Parse a format buffer specification string (e.g., `"AA,AB,AC."`) into
/// a list of [`FieldRef`] values.
///
/// Format: comma-separated two-character field names, optionally followed
/// by a length and/or format code, terminated by a period.
pub fn parse_format_buffer(spec: &str) -> Result<Vec<FieldRef>, AdabasError> {
    let trimmed = spec.trim().trim_end_matches('.');
    if trimmed.is_empty() {
        return Ok(Vec::new());
    }

    let mut refs = Vec::new();
    for part in trimmed.split(',') {
        let part = part.trim();
        if part.is_empty() {
            continue;
        }

        // Parse "AA" or "AA,10" or "AA,10,A"
        let (name, rest) = if part.len() >= 2 && part.as_bytes()[..2].iter().all(|b| b.is_ascii_alphabetic()) {
            (part[..2].to_string(), &part[2..])
        } else {
            return Err(AdabasError::InvalidFormatBuffer {
                spec: spec.to_string(),
            });
        };

        let mut length = None;
        let mut format = None;

        if !rest.is_empty() {
            // Could be ",10" or ",10,A" — but we split on commas at the top
            // level, so rest here would be like "10" or "10A".
            let rest = rest.trim();
            if let Ok(n) = rest.parse::<u16>() {
                length = Some(n);
            } else if rest.len() == 1 && rest.as_bytes()[0].is_ascii_alphabetic() {
                format = Some(rest.to_string());
            }
        }

        refs.push(FieldRef {
            name,
            length,
            format,
        });
    }

    Ok(refs)
}

// ── Acb ────────────────────────────────────────────────────────────

/// ADABAS Control Block: the primary interface for issuing commands.
#[derive(Debug, Clone)]
pub struct Acb {
    /// The command code to execute.
    pub command_code: AcbCommand,
    /// File number to operate on.
    pub file_number: u16,
    /// ISN (for read/update/delete).
    pub isn: Isn,
    /// Format buffer specification string.
    pub format_buffer: String,
    /// Record buffer: data for store/update or result of read.
    pub record_buffer: Vec<u8>,
    /// Search buffer specification string.
    pub search_buffer: String,
    /// Value buffer for search values.
    pub value_buffer: Vec<u8>,
    /// ISN buffer for search results.
    pub isn_buffer: Vec<Isn>,
    /// Response code (set after execution).
    pub response_code: u16,
    /// Command ID for multi-step operations.
    pub command_id: u32,
    /// Additional options / additions.
    pub additions: HashMap<String, String>,
}

impl Acb {
    /// Create a new ACB with the given command and file number.
    pub fn new(command_code: AcbCommand, file_number: u16) -> Self {
        Self {
            command_code,
            file_number,
            isn: 0,
            format_buffer: String::new(),
            record_buffer: Vec::new(),
            search_buffer: String::new(),
            value_buffer: Vec::new(),
            isn_buffer: Vec::new(),
            response_code: 0,
            command_id: 0,
            additions: HashMap::new(),
        }
    }

    /// Set the ISN.
    pub fn with_isn(mut self, isn: Isn) -> Self {
        self.isn = isn;
        self
    }

    /// Set the format buffer.
    pub fn with_format_buffer(mut self, fb: impl Into<String>) -> Self {
        self.format_buffer = fb.into();
        self
    }

    /// Set the record buffer.
    pub fn with_record_buffer(mut self, rb: Vec<u8>) -> Self {
        self.record_buffer = rb;
        self
    }

    /// Set the search buffer.
    pub fn with_search_buffer(mut self, sb: impl Into<String>) -> Self {
        self.search_buffer = sb.into();
        self
    }

    /// Set the value buffer.
    pub fn with_value_buffer(mut self, vb: Vec<u8>) -> Self {
        self.value_buffer = vb;
        self
    }
}

// ── AcbResult ──────────────────────────────────────────────────────

/// The result of executing an ACB command.
#[derive(Debug, Clone)]
pub struct AcbResult {
    /// Response code (0 = success).
    pub response_code: u16,
    /// ISN of the affected/returned record.
    pub isn: Isn,
    /// Record data (for read commands).
    pub record_buffer: Vec<u8>,
    /// ISN list (for search commands).
    pub isn_buffer: Vec<Isn>,
    /// Number of ISNs found (for search commands).
    pub isn_quantity: u64,
}

impl AcbResult {
    /// Create a successful result.
    pub fn success() -> Self {
        Self {
            response_code: 0,
            isn: 0,
            record_buffer: Vec::new(),
            isn_buffer: Vec::new(),
            isn_quantity: 0,
        }
    }

    /// Create a successful result with ISN.
    pub fn with_isn(mut self, isn: Isn) -> Self {
        self.isn = isn;
        self
    }

    /// Set the record buffer.
    pub fn with_record(mut self, data: Vec<u8>) -> Self {
        self.record_buffer = data;
        self
    }

    /// Set the ISN buffer.
    pub fn with_isn_buffer(mut self, isns: Vec<Isn>) -> Self {
        self.isn_quantity = isns.len() as u64;
        self.isn_buffer = isns;
        self
    }

    /// Create an error result.
    pub fn error(response_code: u16) -> Self {
        Self {
            response_code,
            isn: 0,
            record_buffer: Vec::new(),
            isn_buffer: Vec::new(),
            isn_quantity: 0,
        }
    }
}

// ── Tests ──────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_command_codes() {
        assert_eq!(AcbCommand::parse("L1").unwrap(), AcbCommand::L1);
        assert_eq!(AcbCommand::parse("s1").unwrap(), AcbCommand::S1);
        assert_eq!(AcbCommand::parse("ET").unwrap(), AcbCommand::Et);
        assert_eq!(AcbCommand::parse("n2").unwrap(), AcbCommand::N2);
        assert!(AcbCommand::parse("XX").is_err());
    }

    #[test]
    fn command_as_str() {
        assert_eq!(AcbCommand::L1.as_str(), "L1");
        assert_eq!(AcbCommand::Et.as_str(), "ET");
        assert_eq!(AcbCommand::N1.to_string(), "N1");
    }

    #[test]
    fn parse_format_buffer_simple() {
        let refs = parse_format_buffer("AA,AB,AC.").unwrap();
        assert_eq!(refs.len(), 3);
        assert_eq!(refs[0].name, "AA");
        assert_eq!(refs[1].name, "AB");
        assert_eq!(refs[2].name, "AC");
    }

    #[test]
    fn parse_format_buffer_empty() {
        let refs = parse_format_buffer(".").unwrap();
        assert!(refs.is_empty());
        let refs2 = parse_format_buffer("").unwrap();
        assert!(refs2.is_empty());
    }

    #[test]
    fn parse_format_buffer_single() {
        let refs = parse_format_buffer("AA.").unwrap();
        assert_eq!(refs.len(), 1);
        assert_eq!(refs[0].name, "AA");
    }

    #[test]
    fn acb_construction() {
        let acb = Acb::new(AcbCommand::L1, 5)
            .with_isn(42)
            .with_format_buffer("AA,AB.")
            .with_record_buffer(vec![1, 2, 3]);
        assert_eq!(acb.command_code, AcbCommand::L1);
        assert_eq!(acb.file_number, 5);
        assert_eq!(acb.isn, 42);
        assert_eq!(acb.format_buffer, "AA,AB.");
        assert_eq!(acb.record_buffer, vec![1, 2, 3]);
    }

    #[test]
    fn acb_result_success() {
        let r = AcbResult::success().with_isn(10).with_record(b"data".to_vec());
        assert_eq!(r.response_code, 0);
        assert_eq!(r.isn, 10);
        assert_eq!(r.record_buffer, b"data");
    }

    #[test]
    fn acb_result_error() {
        let r = AcbResult::error(148);
        assert_eq!(r.response_code, 148);
    }

    #[test]
    fn acb_result_with_isn_buffer() {
        let r = AcbResult::success().with_isn_buffer(vec![1, 3, 5]);
        assert_eq!(r.isn_quantity, 3);
        assert_eq!(r.isn_buffer, vec![1, 3, 5]);
    }

    #[test]
    fn format_buffer_add_field() {
        let mut fb = FormatBuffer::new();
        fb.add_field(FieldRef {
            name: "AA".to_string(),
            length: None,
            format: None,
        });
        assert_eq!(fb.fields.len(), 1);
    }

    #[test]
    fn acb_with_search_and_value() {
        let acb = Acb::new(AcbCommand::S1, 3)
            .with_search_buffer("AA,EQ.")
            .with_value_buffer(b"SMITH".to_vec());
        assert_eq!(acb.search_buffer, "AA,EQ.");
        assert_eq!(acb.value_buffer, b"SMITH");
    }
}
