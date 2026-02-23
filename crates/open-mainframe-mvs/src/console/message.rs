//! Console message types — WTO message representation.

use serde::{Deserialize, Serialize};

/// Operator console message.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConsoleMessage {
    /// Unique message identifier.
    pub id: u32,
    /// Primary message text.
    pub text: String,
    /// Routing codes (bits 0-15 map to routing codes 1-16).
    pub routing_codes: u16,
    /// Descriptor codes (bits 0-12 map to descriptor codes 1-13).
    pub descriptor_codes: u16,
    /// Whether this is a multi-line WTO.
    pub is_multiline: bool,
    /// Individual lines for multi-line messages.
    pub lines: Vec<String>,
    /// When the message was issued.
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

/// Routing codes — determine which console(s) receive the message.
///
/// Bit 0 = routing code 1 (master console), etc.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct RoutingCode(pub u16);

impl RoutingCode {
    /// Master console (routing code 1).
    pub const MASTER: Self = Self(0x8000);
    /// Operator information (routing code 2).
    pub const OPERATOR_INFO: Self = Self(0x4000);
    /// Tape pool (routing code 3).
    pub const TAPE_POOL: Self = Self(0x2000);
    /// Direct access pool (routing code 4).
    pub const DASD_POOL: Self = Self(0x1000);
    /// Tape library (routing code 5).
    pub const TAPE_LIB: Self = Self(0x0800);
    /// Programmer information (routing code 11).
    pub const PROGRAMMER: Self = Self(0x0020);

    /// Check if a specific routing code bit is set.
    pub fn has_code(&self, code: u8) -> bool {
        if code == 0 || code > 16 {
            return false;
        }
        let bit = 1u16 << (16 - code);
        self.0 & bit != 0
    }

    /// Combine two routing codes.
    pub fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }
}

/// Descriptor codes — classify the nature of the message.
///
/// Bit 0 = descriptor code 1 (system failure), etc.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct DescriptorCode(pub u16);

impl DescriptorCode {
    /// System failure (descriptor code 1).
    pub const SYSTEM_FAILURE: Self = Self(0x8000);
    /// Immediate action required (descriptor code 2).
    pub const IMMEDIATE_ACTION: Self = Self(0x4000);
    /// Eventual action required (descriptor code 3).
    pub const EVENTUAL_ACTION: Self = Self(0x2000);
    /// System status (descriptor code 4).
    pub const SYSTEM_STATUS: Self = Self(0x1000);
    /// Immediate command response (descriptor code 5).
    pub const COMMAND_RESPONSE: Self = Self(0x0800);
    /// Job status (descriptor code 6).
    pub const JOB_STATUS: Self = Self(0x0400);
    /// Informational (descriptor code 12).
    pub const INFORMATIONAL: Self = Self(0x0010);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn routing_code_has_code() {
        let rc = RoutingCode::MASTER;
        assert!(rc.has_code(1));
        assert!(!rc.has_code(2));
    }

    #[test]
    fn routing_code_union() {
        let combined = RoutingCode::MASTER.union(RoutingCode::OPERATOR_INFO);
        assert!(combined.has_code(1));
        assert!(combined.has_code(2));
        assert!(!combined.has_code(3));
    }

    #[test]
    fn routing_code_invalid_values() {
        let rc = RoutingCode(0xFFFF);
        assert!(!rc.has_code(0));
        assert!(!rc.has_code(17));
    }

    #[test]
    fn routing_code_bit_mapping() {
        // Routing code 11 = bit position 5 from left = 0x0020
        let rc = RoutingCode::PROGRAMMER;
        assert!(rc.has_code(11));
        assert!(!rc.has_code(10));
        assert!(!rc.has_code(12));
    }
}
