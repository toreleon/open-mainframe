//! RACF user and group profile data structures.

use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

use crate::types::{ConnectAuthority, UserAttribute};

/// A user's connection to a group.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConnectEntry {
    /// The group name.
    pub group: String,
    /// The authority level for this connection.
    pub authority: ConnectAuthority,
}

/// RACF user profile — stores all attributes for a single user.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UserProfile {
    /// User ID (1-8 uppercase alphanumeric characters).
    pub userid: String,
    /// User's full name.
    pub name: String,
    /// Default group for this user.
    pub default_group: String,
    /// Owner of this profile (user ID or group name).
    pub owner: String,
    /// User attributes (SPECIAL, OPERATIONS, etc.).
    pub attributes: BTreeSet<UserAttribute>,
    /// CLAUTH — classes user can define profiles in.
    pub clauth: BTreeSet<String>,
    /// Group connections: group name → connect entry.
    pub connect_groups: BTreeMap<String, ConnectEntry>,
    /// Password hash (simplified — stores hashed password).
    pub password_hash: Option<String>,
    /// Timestamp of last password change (ISO 8601).
    pub password_changed: Option<String>,
    /// Number of password history entries to retain.
    pub password_history: Vec<String>,
    /// Number of consecutive failed password attempts.
    pub failed_password_count: u32,
    /// Whether the profile was created by the system (vs. administrator).
    pub system_defined: bool,
    /// Creation timestamp (ISO 8601).
    pub created: String,
    /// Last update timestamp (ISO 8601).
    pub last_updated: String,
}

impl UserProfile {
    /// Create a new user profile with the specified attributes.
    pub fn new(
        userid: String,
        default_group: String,
        name: String,
        owner: String,
    ) -> Self {
        let now = now_iso8601();
        let mut connect_groups = BTreeMap::new();
        connect_groups.insert(
            default_group.clone(),
            ConnectEntry {
                group: default_group.clone(),
                authority: ConnectAuthority::Use,
            },
        );
        Self {
            userid,
            name,
            default_group,
            owner,
            attributes: BTreeSet::new(),
            clauth: BTreeSet::new(),
            connect_groups,
            password_hash: None,
            password_changed: None,
            password_history: Vec::new(),
            failed_password_count: 0,
            system_defined: false,
            created: now.clone(),
            last_updated: now,
        }
    }

    /// Check if the user has a specific attribute.
    pub fn has_attribute(&self, attr: UserAttribute) -> bool {
        self.attributes.contains(&attr)
    }

    /// Check if the user is revoked.
    pub fn is_revoked(&self) -> bool {
        self.has_attribute(UserAttribute::Revoke)
    }

    /// Check if the user has SPECIAL authority.
    pub fn is_special(&self) -> bool {
        self.has_attribute(UserAttribute::Special)
    }

    /// Check if the user is connected to the given group.
    pub fn is_connected_to(&self, group: &str) -> bool {
        self.connect_groups.contains_key(group)
    }
}

/// RACF group profile — stores attributes for a group.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GroupProfile {
    /// Group name (1-8 uppercase alphanumeric characters).
    pub group: String,
    /// Superior group in the group hierarchy.
    pub superior_group: Option<String>,
    /// Owner of this group profile (user ID or group name).
    pub owner: String,
    /// Subgroups directly under this group.
    pub subgroups: BTreeSet<String>,
    /// Connected users: userid → connect authority.
    pub members: BTreeMap<String, ConnectAuthority>,
    /// Whether the group was created by the system.
    pub system_defined: bool,
    /// Creation timestamp (ISO 8601).
    pub created: String,
    /// Last update timestamp (ISO 8601).
    pub last_updated: String,
}

impl GroupProfile {
    /// Create a new group profile.
    pub fn new(group: String, superior_group: Option<String>, owner: String) -> Self {
        let now = now_iso8601();
        Self {
            group,
            superior_group,
            owner,
            subgroups: BTreeSet::new(),
            members: BTreeMap::new(),
            system_defined: false,
            created: now.clone(),
            last_updated: now,
        }
    }

    /// Check if a user is a member of this group.
    pub fn has_member(&self, userid: &str) -> bool {
        self.members.contains_key(userid)
    }

    /// Get the connect authority for a user, if connected.
    pub fn member_authority(&self, userid: &str) -> Option<ConnectAuthority> {
        self.members.get(userid).copied()
    }
}

/// Get the current time as an ISO 8601 string (simplified — uses system time).
fn now_iso8601() -> String {
    // Use a simple format compatible with serde serialization.
    // In production this would use chrono or time crate.
    let duration = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    format!("{}", duration.as_secs())
}
