//! ADA-105: Read Commands (6 stories).
//!
//! Provides ADABAS read command variants (L1 through L6), read results,
//! and read options like hold/no-hold, prefetch, and multifetch.

use std::collections::HashMap;

use crate::storage::Isn;

// ── ReadCommand ────────────────────────────────────────────────────

/// ADABAS read command variants.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReadCommand {
    /// L1 — Read a record by ISN.
    ReadByIsn,
    /// L2 — Read physical sequential (next physical record).
    ReadPhysicalSequential,
    /// L3 — Read logical sequential by descriptor value.
    ReadLogicalByDescriptor,
    /// L4 — Read by ISN from an ISN list.
    ReadByIsnList,
    /// L5 — Read physical sequential (variant, same as L2 but different cursor).
    ReadPhysicalSequentialAlt,
    /// L6 — Read records sorted by a descriptor.
    ReadSorted,
}

impl ReadCommand {
    /// Return the command code string.
    pub fn code(&self) -> &'static str {
        match self {
            Self::ReadByIsn => "L1",
            Self::ReadPhysicalSequential => "L2",
            Self::ReadLogicalByDescriptor => "L3",
            Self::ReadByIsnList => "L4",
            Self::ReadPhysicalSequentialAlt => "L5",
            Self::ReadSorted => "L6",
        }
    }

    /// Parse a command code string.
    pub fn parse(code: &str) -> Option<Self> {
        match code.to_uppercase().as_str() {
            "L1" => Some(Self::ReadByIsn),
            "L2" => Some(Self::ReadPhysicalSequential),
            "L3" => Some(Self::ReadLogicalByDescriptor),
            "L4" => Some(Self::ReadByIsnList),
            "L5" => Some(Self::ReadPhysicalSequentialAlt),
            "L6" => Some(Self::ReadSorted),
            _ => None,
        }
    }
}

// ── ReadOptions ────────────────────────────────────────────────────

/// Options controlling how a read command behaves.
#[derive(Debug, Clone)]
pub struct ReadOptions {
    /// Place a hold on the record (for subsequent update).
    pub hold: bool,
    /// Prefetch: request N records ahead.
    pub prefetch: u16,
    /// Multifetch: return multiple records in one call.
    pub multifetch: u16,
    /// Descriptor name (for L3 / L6 commands).
    pub descriptor: Option<String>,
    /// Starting value for descriptor-based reads.
    pub start_value: Option<String>,
    /// ISN list for L4 commands.
    pub isn_list: Vec<Isn>,
}

impl ReadOptions {
    /// Create default read options (no hold, no prefetch).
    pub fn new() -> Self {
        Self {
            hold: false,
            prefetch: 0,
            multifetch: 0,
            descriptor: None,
            start_value: None,
            isn_list: Vec::new(),
        }
    }

    /// Enable hold on read.
    pub fn with_hold(mut self) -> Self {
        self.hold = true;
        self
    }

    /// Set prefetch count.
    pub fn with_prefetch(mut self, count: u16) -> Self {
        self.prefetch = count;
        self
    }

    /// Set multifetch count.
    pub fn with_multifetch(mut self, count: u16) -> Self {
        self.multifetch = count;
        self
    }

    /// Set the descriptor for logical sequential reads.
    pub fn with_descriptor(mut self, desc: impl Into<String>) -> Self {
        self.descriptor = Some(desc.into());
        self
    }

    /// Set the starting value for descriptor-based reads.
    pub fn with_start_value(mut self, value: impl Into<String>) -> Self {
        self.start_value = Some(value.into());
        self
    }

    /// Set the ISN list for L4 commands.
    pub fn with_isn_list(mut self, isns: Vec<Isn>) -> Self {
        self.isn_list = isns;
        self
    }
}

impl Default for ReadOptions {
    fn default() -> Self {
        Self::new()
    }
}

// ── ReadResult ─────────────────────────────────────────────────────

/// The result of a read command, containing field values for a record.
#[derive(Debug, Clone)]
pub struct ReadResult {
    /// The ISN of the returned record.
    pub isn: Isn,
    /// Field name -> field value (as bytes).
    pub fields: HashMap<String, Vec<u8>>,
    /// Whether the record is held for update.
    pub held: bool,
    /// Response code (0 = success, 3 = end of file).
    pub response_code: u16,
}

impl ReadResult {
    /// Create a successful read result.
    pub fn new(isn: Isn) -> Self {
        Self {
            isn,
            fields: HashMap::new(),
            held: false,
            response_code: 0,
        }
    }

    /// Set a field value.
    pub fn set_field(&mut self, name: impl Into<String>, value: Vec<u8>) {
        self.fields.insert(name.into(), value);
    }

    /// Get a field value.
    pub fn get_field(&self, name: &str) -> Option<&[u8]> {
        self.fields.get(name).map(|v| v.as_slice())
    }

    /// Create an end-of-file result.
    pub fn end_of_file() -> Self {
        Self {
            isn: 0,
            fields: HashMap::new(),
            held: false,
            response_code: 3,
        }
    }

    /// Whether this result indicates end-of-file.
    pub fn is_eof(&self) -> bool {
        self.response_code == 3
    }

    /// Mark this result as held.
    pub fn with_hold(mut self) -> Self {
        self.held = true;
        self
    }
}

// ── ReadCursor ─────────────────────────────────────────────────────

/// A cursor for sequential read operations (L2, L3, L5, L6).
#[derive(Debug, Clone)]
pub struct ReadCursor {
    /// The type of read.
    pub command: ReadCommand,
    /// Current position (ISN index or descriptor position).
    position: usize,
    /// Ordered list of ISNs to iterate.
    isn_sequence: Vec<Isn>,
    /// Whether the cursor is exhausted.
    exhausted: bool,
}

impl ReadCursor {
    /// Create a new read cursor over a sequence of ISNs.
    pub fn new(command: ReadCommand, isn_sequence: Vec<Isn>) -> Self {
        Self {
            command,
            position: 0,
            isn_sequence,
            exhausted: false,
        }
    }

    /// Get the next ISN, or None if exhausted.
    pub fn next_isn(&mut self) -> Option<Isn> {
        if self.exhausted || self.position >= self.isn_sequence.len() {
            self.exhausted = true;
            return None;
        }
        let isn = self.isn_sequence[self.position];
        self.position += 1;
        Some(isn)
    }

    /// Reset the cursor to the beginning.
    pub fn reset(&mut self) {
        self.position = 0;
        self.exhausted = false;
    }

    /// Whether the cursor has been exhausted.
    pub fn is_exhausted(&self) -> bool {
        self.exhausted
    }

    /// Return how many ISNs remain.
    pub fn remaining(&self) -> usize {
        if self.exhausted {
            0
        } else {
            self.isn_sequence.len().saturating_sub(self.position)
        }
    }
}

// ── Tests ──────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_command_codes() {
        assert_eq!(ReadCommand::ReadByIsn.code(), "L1");
        assert_eq!(ReadCommand::ReadSorted.code(), "L6");
    }

    #[test]
    fn read_command_parse() {
        assert_eq!(ReadCommand::parse("L1"), Some(ReadCommand::ReadByIsn));
        assert_eq!(ReadCommand::parse("l3"), Some(ReadCommand::ReadLogicalByDescriptor));
        assert_eq!(ReadCommand::parse("XX"), None);
    }

    #[test]
    fn read_options_default() {
        let opts = ReadOptions::new();
        assert!(!opts.hold);
        assert_eq!(opts.prefetch, 0);
        assert_eq!(opts.multifetch, 0);
        assert!(opts.descriptor.is_none());
    }

    #[test]
    fn read_options_builders() {
        let opts = ReadOptions::new()
            .with_hold()
            .with_prefetch(10)
            .with_multifetch(5)
            .with_descriptor("AA")
            .with_start_value("SMITH")
            .with_isn_list(vec![1, 2, 3]);
        assert!(opts.hold);
        assert_eq!(opts.prefetch, 10);
        assert_eq!(opts.multifetch, 5);
        assert_eq!(opts.descriptor, Some("AA".to_string()));
        assert_eq!(opts.start_value, Some("SMITH".to_string()));
        assert_eq!(opts.isn_list, vec![1, 2, 3]);
    }

    #[test]
    fn read_result_fields() {
        let mut result = ReadResult::new(42);
        result.set_field("AA", b"SMITH".to_vec());
        result.set_field("AB", b"NYC".to_vec());
        assert_eq!(result.get_field("AA"), Some(b"SMITH".as_slice()));
        assert_eq!(result.get_field("AB"), Some(b"NYC".as_slice()));
        assert!(result.get_field("ZZ").is_none());
        assert!(!result.is_eof());
    }

    #[test]
    fn read_result_eof() {
        let eof = ReadResult::end_of_file();
        assert!(eof.is_eof());
        assert_eq!(eof.response_code, 3);
    }

    #[test]
    fn read_result_with_hold() {
        let result = ReadResult::new(1).with_hold();
        assert!(result.held);
    }

    #[test]
    fn read_cursor_sequential() {
        let mut cursor = ReadCursor::new(ReadCommand::ReadPhysicalSequential, vec![10, 20, 30]);
        assert_eq!(cursor.remaining(), 3);
        assert_eq!(cursor.next_isn(), Some(10));
        assert_eq!(cursor.next_isn(), Some(20));
        assert_eq!(cursor.remaining(), 1);
        assert_eq!(cursor.next_isn(), Some(30));
        assert_eq!(cursor.next_isn(), None);
        assert!(cursor.is_exhausted());
    }

    #[test]
    fn read_cursor_reset() {
        let mut cursor = ReadCursor::new(ReadCommand::ReadByIsn, vec![1, 2]);
        cursor.next_isn();
        cursor.next_isn();
        cursor.next_isn(); // exhausts
        assert!(cursor.is_exhausted());
        cursor.reset();
        assert!(!cursor.is_exhausted());
        assert_eq!(cursor.next_isn(), Some(1));
    }

    #[test]
    fn read_cursor_empty() {
        let mut cursor = ReadCursor::new(ReadCommand::ReadPhysicalSequential, vec![]);
        assert_eq!(cursor.remaining(), 0);
        assert_eq!(cursor.next_isn(), None);
        assert!(cursor.is_exhausted());
    }
}
