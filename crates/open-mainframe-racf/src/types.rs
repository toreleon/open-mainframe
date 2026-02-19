//! Core RACF types — user attributes, group authority levels, access levels.

use serde::{Deserialize, Serialize};
use std::fmt;

/// User attributes that can be assigned via ADDUSER/ALTUSER.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum UserAttribute {
    /// Full RACF administrative authority.
    Special,
    /// Universal read access to all RACF-protected resources.
    Operations,
    /// Authority to set audit controls.
    Auditor,
    /// Account is revoked (cannot log on).
    Revoke,
    /// Group access authority — new datasets get group access.
    Grpacc,
    /// Automatic dataset protection — datasets auto-protected on creation.
    Adsp,
    /// Restricted user — no access unless explicitly permitted.
    Restricted,
}

impl fmt::Display for UserAttribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Special => write!(f, "SPECIAL"),
            Self::Operations => write!(f, "OPERATIONS"),
            Self::Auditor => write!(f, "AUDITOR"),
            Self::Revoke => write!(f, "REVOKE"),
            Self::Grpacc => write!(f, "GRPACC"),
            Self::Adsp => write!(f, "ADSP"),
            Self::Restricted => write!(f, "RESTRICTED"),
        }
    }
}

/// Group connect authority levels (ordered from least to most authority).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum ConnectAuthority {
    /// Can use the group's resources.
    Use = 0,
    /// Can create new resources under the group.
    Create = 1,
    /// Can connect other users to the group.
    Connect = 2,
    /// Can add new users to the group (highest group-level authority).
    Join = 3,
}

impl fmt::Display for ConnectAuthority {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Use => write!(f, "USE"),
            Self::Create => write!(f, "CREATE"),
            Self::Connect => write!(f, "CONNECT"),
            Self::Join => write!(f, "JOIN"),
        }
    }
}

impl Default for ConnectAuthority {
    fn default() -> Self {
        Self::Use
    }
}

/// RACF access levels for resource protection (ordered by privilege).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum AccessLevel {
    /// No access.
    None = 0,
    /// Read-only access.
    Read = 2,
    /// Read and write access.
    Update = 4,
    /// Full control (includes creating/deleting members).
    Control = 8,
    /// Owner-level access (can change protection).
    Alter = 12,
}

impl fmt::Display for AccessLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::None => write!(f, "NONE"),
            Self::Read => write!(f, "READ"),
            Self::Update => write!(f, "UPDATE"),
            Self::Control => write!(f, "CONTROL"),
            Self::Alter => write!(f, "ALTER"),
        }
    }
}

impl Default for AccessLevel {
    fn default() -> Self {
        Self::None
    }
}
