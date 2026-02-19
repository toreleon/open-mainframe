//! RACF database — in-memory store with optional persistence for user and group profiles.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use tracing::{debug, info};

use crate::error::RacfError;
use crate::profile::{ConnectEntry, GroupProfile, UserProfile};
use crate::types::{ConnectAuthority, UserAttribute};

/// The RACF database — holds all user profiles, group profiles, and connections.
///
/// Provides ADDUSER/ALTUSER/LISTUSER/DELUSER, ADDGROUP/ALTGROUP/LISTGRP/DELGROUP,
/// CONNECT/REMOVE, and SEARCH operations.
#[derive(Debug)]
pub struct RacfDatabase {
    /// User profiles keyed by uppercase user ID.
    users: BTreeMap<String, UserProfile>,
    /// Group profiles keyed by uppercase group name.
    groups: BTreeMap<String, GroupProfile>,
    /// Optional path for persistent storage.
    persist_path: Option<PathBuf>,
}

/// Result of a LISTUSER command.
#[derive(Debug, Clone)]
pub struct ListUserResult {
    /// The user profile.
    pub profile: UserProfile,
}

/// Result of a LISTGRP command.
#[derive(Debug, Clone)]
pub struct ListGroupResult {
    /// The group profile.
    pub profile: GroupProfile,
}

/// Result of a SEARCH command.
#[derive(Debug, Clone)]
pub struct SearchResult {
    /// Matching profile names.
    pub entries: Vec<String>,
}

impl RacfDatabase {
    /// Create a new empty RACF database (in-memory only).
    pub fn new() -> Self {
        let mut db = Self {
            users: BTreeMap::new(),
            groups: BTreeMap::new(),
            persist_path: None,
        };
        // Create the default SYS1 group (root of the group hierarchy).
        let sys1 = GroupProfile {
            group: "SYS1".to_string(),
            superior_group: None,
            owner: "SYS1".to_string(),
            subgroups: Default::default(),
            members: Default::default(),
            system_defined: true,
            created: "0".to_string(),
            last_updated: "0".to_string(),
        };
        db.groups.insert("SYS1".to_string(), sys1);
        db
    }

    /// Create a RACF database with persistent storage at the given path.
    pub fn with_persistence(path: impl AsRef<Path>) -> Self {
        let mut db = Self::new();
        db.persist_path = Some(path.as_ref().to_path_buf());
        db
    }

    /// Load a RACF database from persistent storage.
    pub fn load(path: impl AsRef<Path>) -> Result<Self, RacfError> {
        let path = path.as_ref();
        if !path.exists() {
            info!("RACF database not found at {}, creating new", path.display());
            return Ok(Self::with_persistence(path));
        }
        let data = std::fs::read_to_string(path).map_err(|e| RacfError::IoError {
            message: format!("failed to read {}: {}", path.display(), e),
        })?;
        let stored: StoredDatabase =
            serde_json::from_str(&data).map_err(|e| RacfError::IoError {
                message: format!("failed to parse RACF database: {}", e),
            })?;
        info!(
            "RACF database loaded: {} users, {} groups",
            stored.users.len(),
            stored.groups.len()
        );
        Ok(Self {
            users: stored.users,
            groups: stored.groups,
            persist_path: Some(path.to_path_buf()),
        })
    }

    /// Persist the database to disk (if a persist path is configured).
    pub fn save(&self) -> Result<(), RacfError> {
        let Some(path) = &self.persist_path else {
            return Ok(());
        };
        let stored = StoredDatabase {
            users: self.users.clone(),
            groups: self.groups.clone(),
        };
        let data = serde_json::to_string_pretty(&stored).map_err(|e| RacfError::IoError {
            message: format!("failed to serialize RACF database: {}", e),
        })?;
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| RacfError::IoError {
                message: format!("failed to create directory {}: {}", parent.display(), e),
            })?;
        }
        std::fs::write(path, data).map_err(|e| RacfError::IoError {
            message: format!("failed to write {}: {}", path.display(), e),
        })?;
        debug!("RACF database saved to {}", path.display());
        Ok(())
    }

    // ───────────────────────── User Profile Commands ─────────────────────────

    /// ADDUSER — create a new user profile.
    ///
    /// The user is automatically connected to their default group with USE authority.
    pub fn add_user(
        &mut self,
        userid: &str,
        default_group: &str,
        name: &str,
        owner: &str,
    ) -> Result<(), RacfError> {
        let userid = normalize_name(userid);
        let default_group = normalize_name(default_group);
        let owner = normalize_name(owner);

        validate_name(&userid).map_err(|_| RacfError::InvalidUserId {
            userid: userid.clone(),
        })?;

        if self.users.contains_key(&userid) {
            return Err(RacfError::UserExists {
                userid: userid.clone(),
            });
        }

        // Default group must exist.
        if !self.groups.contains_key(&default_group) {
            return Err(RacfError::GroupNotFound {
                group: default_group.clone(),
            });
        }

        let profile = UserProfile::new(
            userid.clone(),
            default_group.clone(),
            name.to_string(),
            owner,
        );

        // Connect user to default group.
        if let Some(group) = self.groups.get_mut(&default_group) {
            group.members.insert(userid.clone(), ConnectAuthority::Use);
        }

        info!("ADDUSER {} DFLTGRP({})", userid, default_group);
        self.users.insert(userid, profile);
        self.auto_save();
        Ok(())
    }

    /// ALTUSER — modify an existing user profile.
    ///
    /// Attributes listed in `add_attrs` are added; those in `remove_attrs` are removed.
    pub fn alter_user(
        &mut self,
        userid: &str,
        name: Option<&str>,
        default_group: Option<&str>,
        owner: Option<&str>,
        add_attrs: &[UserAttribute],
        remove_attrs: &[UserAttribute],
    ) -> Result<(), RacfError> {
        let userid = normalize_name(userid);
        let profile = self
            .users
            .get_mut(&userid)
            .ok_or_else(|| RacfError::UserNotFound {
                userid: userid.clone(),
            })?;

        if let Some(n) = name {
            profile.name = n.to_string();
        }
        if let Some(dg) = default_group {
            let dg = normalize_name(dg);
            if !self.groups.contains_key(&dg) {
                return Err(RacfError::GroupNotFound { group: dg });
            }
            profile.default_group = dg;
        }
        if let Some(o) = owner {
            profile.owner = normalize_name(o);
        }
        for attr in add_attrs {
            profile.attributes.insert(*attr);
        }
        for attr in remove_attrs {
            profile.attributes.remove(attr);
        }

        info!("ALTUSER {}", userid);
        self.auto_save();
        Ok(())
    }

    /// LISTUSER — retrieve user profile information.
    pub fn list_user(&self, userid: &str) -> Result<ListUserResult, RacfError> {
        let userid = normalize_name(userid);
        let profile = self
            .users
            .get(&userid)
            .ok_or_else(|| RacfError::UserNotFound {
                userid: userid.clone(),
            })?;
        Ok(ListUserResult {
            profile: profile.clone(),
        })
    }

    /// DELUSER — delete a user profile and all connect entries.
    pub fn delete_user(&mut self, userid: &str) -> Result<(), RacfError> {
        let userid = normalize_name(userid);
        let profile = self
            .users
            .remove(&userid)
            .ok_or_else(|| RacfError::UserNotFound {
                userid: userid.clone(),
            })?;

        // Remove user from all connected groups.
        for group_name in profile.connect_groups.keys() {
            if let Some(group) = self.groups.get_mut(group_name) {
                group.members.remove(&userid);
            }
        }

        info!("DELUSER {}", userid);
        self.auto_save();
        Ok(())
    }

    // ───────────────────────── Group Profile Commands ─────────────────────────

    /// ADDGROUP — create a new group profile.
    pub fn add_group(
        &mut self,
        group: &str,
        superior_group: &str,
        owner: &str,
    ) -> Result<(), RacfError> {
        let group = normalize_name(group);
        let superior_group = normalize_name(superior_group);
        let owner = normalize_name(owner);

        validate_name(&group).map_err(|_| RacfError::InvalidGroupName {
            group: group.clone(),
        })?;

        if self.groups.contains_key(&group) {
            return Err(RacfError::GroupExists {
                group: group.clone(),
            });
        }

        if !self.groups.contains_key(&superior_group) {
            return Err(RacfError::GroupNotFound {
                group: superior_group.clone(),
            });
        }

        let profile = GroupProfile::new(
            group.clone(),
            Some(superior_group.clone()),
            owner,
        );

        // Register as subgroup of the superior group.
        if let Some(sup) = self.groups.get_mut(&superior_group) {
            sup.subgroups.insert(group.clone());
        }

        info!("ADDGROUP {} SUPGROUP({})", group, superior_group);
        self.groups.insert(group, profile);
        self.auto_save();
        Ok(())
    }

    /// ALTGROUP — modify an existing group profile.
    pub fn alter_group(
        &mut self,
        group: &str,
        owner: Option<&str>,
    ) -> Result<(), RacfError> {
        let group = normalize_name(group);
        let profile = self
            .groups
            .get_mut(&group)
            .ok_or_else(|| RacfError::GroupNotFound {
                group: group.clone(),
            })?;

        if let Some(o) = owner {
            profile.owner = normalize_name(o);
        }

        info!("ALTGROUP {}", group);
        self.auto_save();
        Ok(())
    }

    /// LISTGRP — retrieve group profile information.
    pub fn list_group(&self, group: &str) -> Result<ListGroupResult, RacfError> {
        let group = normalize_name(group);
        let profile = self
            .groups
            .get(&group)
            .ok_or_else(|| RacfError::GroupNotFound {
                group: group.clone(),
            })?;
        Ok(ListGroupResult {
            profile: profile.clone(),
        })
    }

    /// DELGROUP — delete a group profile.
    ///
    /// The group must have no connected users (other than system-level).
    pub fn delete_group(&mut self, group: &str) -> Result<(), RacfError> {
        let group = normalize_name(group);

        // Prevent deletion of SYS1.
        if group == "SYS1" {
            return Err(RacfError::GroupNotEmpty {
                group: group.clone(),
            });
        }

        let profile = self
            .groups
            .get(&group)
            .ok_or_else(|| RacfError::GroupNotFound {
                group: group.clone(),
            })?;

        if !profile.members.is_empty() {
            return Err(RacfError::GroupNotEmpty {
                group: group.clone(),
            });
        }

        let superior = profile.superior_group.clone();

        // Remove from superior group's subgroups list.
        if let Some(sup_name) = &superior {
            if let Some(sup) = self.groups.get_mut(sup_name) {
                sup.subgroups.remove(&group);
            }
        }

        self.groups.remove(&group);
        info!("DELGROUP {}", group);
        self.auto_save();
        Ok(())
    }

    // ───────────────────────── Connect / Remove ─────────────────────────

    /// CONNECT — connect a user to a group with a specified authority.
    pub fn connect(
        &mut self,
        userid: &str,
        group: &str,
        authority: ConnectAuthority,
    ) -> Result<(), RacfError> {
        let userid = normalize_name(userid);
        let group = normalize_name(group);

        // User must exist.
        if !self.users.contains_key(&userid) {
            return Err(RacfError::UserNotFound {
                userid: userid.clone(),
            });
        }

        // Group must exist.
        if !self.groups.contains_key(&group) {
            return Err(RacfError::GroupNotFound {
                group: group.clone(),
            });
        }

        // Check if already connected.
        let user = self.users.get(&userid).unwrap();
        if user.is_connected_to(&group) {
            return Err(RacfError::AlreadyConnected {
                userid: userid.clone(),
                group: group.clone(),
            });
        }

        // Add connection to user profile.
        let user = self.users.get_mut(&userid).unwrap();
        user.connect_groups.insert(
            group.clone(),
            ConnectEntry {
                group: group.clone(),
                authority,
            },
        );

        // Add member to group profile.
        let grp = self.groups.get_mut(&group).unwrap();
        grp.members.insert(userid.clone(), authority);

        info!("CONNECT {} GROUP({}) AUTH({})", userid, group, authority);
        self.auto_save();
        Ok(())
    }

    /// REMOVE — remove a user's connection from a group.
    ///
    /// Cannot remove a user from their default group.
    pub fn remove(
        &mut self,
        userid: &str,
        group: &str,
    ) -> Result<(), RacfError> {
        let userid = normalize_name(userid);
        let group = normalize_name(group);

        let user = self
            .users
            .get(&userid)
            .ok_or_else(|| RacfError::UserNotFound {
                userid: userid.clone(),
            })?;

        if !user.is_connected_to(&group) {
            return Err(RacfError::NotConnected {
                userid: userid.clone(),
                group: group.clone(),
            });
        }

        // Cannot remove from default group.
        if user.default_group == group {
            return Err(RacfError::CannotRemoveDefaultGroup {
                userid: userid.clone(),
                group: group.clone(),
            });
        }

        // Remove from user profile.
        let user = self.users.get_mut(&userid).unwrap();
        user.connect_groups.remove(&group);

        // Remove from group profile.
        if let Some(grp) = self.groups.get_mut(&group) {
            grp.members.remove(&userid);
        }

        info!("REMOVE {} GROUP({})", userid, group);
        self.auto_save();
        Ok(())
    }

    // ───────────────────────── Search ─────────────────────────

    /// SEARCH — search for profiles matching a mask pattern.
    ///
    /// `class` can be "USER" or "GROUP".
    /// `mask` supports `*` as a wildcard matching any characters.
    pub fn search(&self, class: &str, mask: &str) -> SearchResult {
        let class = class.to_uppercase();
        let mask = mask.to_uppercase();

        let entries: Vec<String> = match class.as_str() {
            "USER" => self
                .users
                .keys()
                .filter(|k| matches_mask(k, &mask))
                .cloned()
                .collect(),
            "GROUP" => self
                .groups
                .keys()
                .filter(|k| matches_mask(k, &mask))
                .cloned()
                .collect(),
            _ => Vec::new(),
        };

        SearchResult { entries }
    }

    /// Get a user profile reference (for internal use).
    pub fn get_user(&self, userid: &str) -> Option<&UserProfile> {
        self.users.get(&normalize_name(userid))
    }

    /// Get a mutable user profile reference (for internal use).
    pub fn get_user_mut(&mut self, userid: &str) -> Option<&mut UserProfile> {
        self.users.get_mut(&normalize_name(userid))
    }

    /// Get a group profile reference (for internal use).
    pub fn get_group(&self, group: &str) -> Option<&GroupProfile> {
        self.groups.get(&normalize_name(group))
    }

    /// Check if a user exists.
    pub fn user_exists(&self, userid: &str) -> bool {
        self.users.contains_key(&normalize_name(userid))
    }

    /// Check if a group exists.
    pub fn group_exists(&self, group: &str) -> bool {
        self.groups.contains_key(&normalize_name(group))
    }

    /// Get the total number of user profiles.
    pub fn user_count(&self) -> usize {
        self.users.len()
    }

    /// Get the total number of group profiles.
    pub fn group_count(&self) -> usize {
        self.groups.len()
    }

    /// Auto-save if persistence is configured (ignore errors silently).
    fn auto_save(&self) {
        if self.persist_path.is_some() {
            let _ = self.save();
        }
    }
}

impl Default for RacfDatabase {
    fn default() -> Self {
        Self::new()
    }
}

/// Serializable form of the RACF database.
#[derive(serde::Serialize, serde::Deserialize)]
struct StoredDatabase {
    users: BTreeMap<String, UserProfile>,
    groups: BTreeMap<String, GroupProfile>,
}

/// Normalize a RACF name to uppercase.
fn normalize_name(name: &str) -> String {
    name.trim().to_uppercase()
}

/// Validate a RACF name: 1-8 alphanumeric characters (plus @, #, $).
fn validate_name(name: &str) -> Result<(), ()> {
    if name.is_empty() || name.len() > 8 {
        return Err(());
    }
    if !name
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '@' || c == '#' || c == '$')
    {
        return Err(());
    }
    Ok(())
}

/// Match a name against a mask pattern where `*` matches any sequence of characters.
fn matches_mask(name: &str, mask: &str) -> bool {
    if mask == "*" {
        return true;
    }

    // Simple glob matching: split on '*' and check sequential containment.
    let parts: Vec<&str> = mask.split('*').collect();

    if parts.len() == 1 {
        // No wildcard — exact match.
        return name == mask;
    }

    let mut pos = 0;
    for (i, part) in parts.iter().enumerate() {
        if part.is_empty() {
            continue;
        }
        if i == 0 {
            // First part must be a prefix.
            if !name.starts_with(part) {
                return false;
            }
            pos = part.len();
        } else if i == parts.len() - 1 {
            // Last part must be a suffix.
            if !name[pos..].ends_with(part) {
                return false;
            }
        } else {
            // Middle parts must appear in order.
            match name[pos..].find(part) {
                Some(idx) => pos += idx + part.len(),
                None => return false,
            }
        }
    }

    true
}

#[cfg(test)]
mod tests {
    use super::*;

    // ─────── Story S100.1: User Profile Management ───────

    #[test]
    fn test_adduser_with_default_group() {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();

        db.add_user("JSMITH", "DEPT01", "John Smith", "ADMIN1")
            .unwrap();

        let result = db.list_user("JSMITH").unwrap();
        assert_eq!(result.profile.userid, "JSMITH");
        assert_eq!(result.profile.default_group, "DEPT01");
        assert_eq!(result.profile.name, "John Smith");
        assert_eq!(result.profile.owner, "ADMIN1");
        assert!(result.profile.is_connected_to("DEPT01"));
    }

    #[test]
    fn test_adduser_duplicate_fails() {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();
        db.add_user("JSMITH", "DEPT01", "John Smith", "ADMIN1")
            .unwrap();

        let err = db
            .add_user("JSMITH", "DEPT01", "Duplicate", "ADMIN1")
            .unwrap_err();
        assert!(matches!(err, RacfError::UserExists { .. }));
    }

    #[test]
    fn test_altuser_add_attributes() {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();
        db.add_user("JSMITH", "DEPT01", "John Smith", "ADMIN1")
            .unwrap();

        db.alter_user(
            "JSMITH",
            None,
            None,
            None,
            &[UserAttribute::Special, UserAttribute::Operations],
            &[],
        )
        .unwrap();

        let result = db.list_user("JSMITH").unwrap();
        assert!(result.profile.is_special());
        assert!(result.profile.has_attribute(UserAttribute::Operations));
    }

    #[test]
    fn test_listuser_shows_all_fields() {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();
        db.add_user("JSMITH", "DEPT01", "John Smith", "ADMIN1")
            .unwrap();
        db.alter_user(
            "JSMITH",
            None,
            None,
            None,
            &[UserAttribute::Special],
            &[],
        )
        .unwrap();

        let result = db.list_user("JSMITH").unwrap();
        assert_eq!(result.profile.name, "John Smith");
        assert_eq!(result.profile.default_group, "DEPT01");
        assert!(result.profile.attributes.contains(&UserAttribute::Special));
        assert!(result.profile.connect_groups.contains_key("DEPT01"));
    }

    #[test]
    fn test_deluser_removes_profile_and_connections() {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();
        db.add_group("DEPT02", "SYS1", "ADMIN1").unwrap();
        db.add_user("JSMITH", "DEPT01", "John Smith", "ADMIN1")
            .unwrap();
        db.connect("JSMITH", "DEPT02", ConnectAuthority::Use)
            .unwrap();

        db.delete_user("JSMITH").unwrap();

        assert!(!db.user_exists("JSMITH"));
        // Verify removed from groups.
        let grp1 = db.list_group("DEPT01").unwrap();
        assert!(!grp1.profile.has_member("JSMITH"));
        let grp2 = db.list_group("DEPT02").unwrap();
        assert!(!grp2.profile.has_member("JSMITH"));
    }

    // ─────── Story S100.2: Group Profile Management ───────

    #[test]
    fn test_addgroup_under_sys1() {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();

        let result = db.list_group("DEPT01").unwrap();
        assert_eq!(result.profile.group, "DEPT01");
        assert_eq!(result.profile.superior_group, Some("SYS1".to_string()));
        assert_eq!(result.profile.owner, "ADMIN1");

        // SYS1 should list DEPT01 as a subgroup.
        let sys1 = db.list_group("SYS1").unwrap();
        assert!(sys1.profile.subgroups.contains("DEPT01"));
    }

    #[test]
    fn test_connect_user_to_group() {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();
        db.add_group("DEPT02", "SYS1", "ADMIN1").unwrap();
        db.add_user("JSMITH", "DEPT01", "John Smith", "ADMIN1")
            .unwrap();

        db.connect("JSMITH", "DEPT02", ConnectAuthority::Use)
            .unwrap();

        let user = db.list_user("JSMITH").unwrap();
        assert!(user.profile.is_connected_to("DEPT02"));

        let grp = db.list_group("DEPT02").unwrap();
        assert!(grp.profile.has_member("JSMITH"));
        assert_eq!(
            grp.profile.member_authority("JSMITH"),
            Some(ConnectAuthority::Use)
        );
    }

    #[test]
    fn test_remove_user_from_group() {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();
        db.add_group("DEPT02", "SYS1", "ADMIN1").unwrap();
        db.add_user("JSMITH", "DEPT01", "John Smith", "ADMIN1")
            .unwrap();
        db.connect("JSMITH", "DEPT02", ConnectAuthority::Use)
            .unwrap();

        db.remove("JSMITH", "DEPT02").unwrap();

        let user = db.list_user("JSMITH").unwrap();
        assert!(!user.profile.is_connected_to("DEPT02"));

        let grp = db.list_group("DEPT02").unwrap();
        assert!(!grp.profile.has_member("JSMITH"));
    }

    #[test]
    fn test_remove_from_default_group_fails() {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();
        db.add_user("JSMITH", "DEPT01", "John Smith", "ADMIN1")
            .unwrap();

        let err = db.remove("JSMITH", "DEPT01").unwrap_err();
        assert!(matches!(err, RacfError::CannotRemoveDefaultGroup { .. }));
    }

    #[test]
    fn test_connect_authority_levels() {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();
        db.add_group("ADMINS", "SYS1", "ADMIN1").unwrap();
        db.add_user("JSMITH", "DEPT01", "John Smith", "ADMIN1")
            .unwrap();

        db.connect("JSMITH", "ADMINS", ConnectAuthority::Join)
            .unwrap();

        let grp = db.list_group("ADMINS").unwrap();
        assert_eq!(
            grp.profile.member_authority("JSMITH"),
            Some(ConnectAuthority::Join)
        );
    }

    #[test]
    fn test_delgroup_empty() {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();

        db.delete_group("DEPT01").unwrap();
        assert!(!db.group_exists("DEPT01"));

        // SYS1 should no longer list it as subgroup.
        let sys1 = db.list_group("SYS1").unwrap();
        assert!(!sys1.profile.subgroups.contains("DEPT01"));
    }

    #[test]
    fn test_delgroup_with_members_fails() {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();
        db.add_user("JSMITH", "DEPT01", "John Smith", "ADMIN1")
            .unwrap();

        let err = db.delete_group("DEPT01").unwrap_err();
        assert!(matches!(err, RacfError::GroupNotEmpty { .. }));
    }

    // ─────── Story S100.3: RACF Database Storage and Search ───────

    #[test]
    fn test_search_users_by_mask() {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();
        db.add_user("JSMITH", "DEPT01", "John Smith", "ADMIN1")
            .unwrap();
        db.add_user("JJONES", "DEPT01", "Jane Jones", "ADMIN1")
            .unwrap();
        db.add_user("ASMITH", "DEPT01", "Alice Smith", "ADMIN1")
            .unwrap();

        let result = db.search("USER", "J*");
        assert_eq!(result.entries.len(), 2);
        assert!(result.entries.contains(&"JSMITH".to_string()));
        assert!(result.entries.contains(&"JJONES".to_string()));
    }

    #[test]
    fn test_search_groups_by_mask() {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();
        db.add_group("DEPT02", "SYS1", "ADMIN1").unwrap();
        db.add_group("ADMIN1", "SYS1", "ADMIN1").unwrap();

        let result = db.search("GROUP", "DEPT*");
        assert_eq!(result.entries.len(), 2);
        assert!(result.entries.contains(&"DEPT01".to_string()));
        assert!(result.entries.contains(&"DEPT02".to_string()));
    }

    #[test]
    fn test_search_all_users() {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();
        db.add_user("JSMITH", "DEPT01", "John Smith", "ADMIN1")
            .unwrap();
        db.add_user("ASMITH", "DEPT01", "Alice Smith", "ADMIN1")
            .unwrap();

        let result = db.search("USER", "*");
        assert_eq!(result.entries.len(), 2);
    }

    #[test]
    fn test_persistence_round_trip() {
        let dir = std::env::temp_dir().join("racf_test_persist");
        let _ = std::fs::remove_dir_all(&dir);
        let db_path = dir.join("racf.json");

        // Create and populate.
        {
            let mut db = RacfDatabase::with_persistence(&db_path);
            db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();
            db.add_user("JSMITH", "DEPT01", "John Smith", "ADMIN1")
                .unwrap();
            db.save().unwrap();
        }

        // Reload and verify.
        {
            let db = RacfDatabase::load(&db_path).unwrap();
            assert!(db.user_exists("JSMITH"));
            assert!(db.group_exists("DEPT01"));
            let user = db.list_user("JSMITH").unwrap();
            assert_eq!(user.profile.name, "John Smith");
        }

        let _ = std::fs::remove_dir_all(&dir);
    }

    // ─────── Mask matching ───────

    #[test]
    fn test_matches_mask() {
        assert!(matches_mask("JSMITH", "J*"));
        assert!(matches_mask("JSMITH", "*SMITH"));
        assert!(matches_mask("JSMITH", "J*TH"));
        assert!(matches_mask("JSMITH", "*"));
        assert!(matches_mask("JSMITH", "JSMITH"));
        assert!(!matches_mask("JSMITH", "A*"));
        assert!(!matches_mask("JSMITH", "JONES"));
    }

    // ─────── Case insensitivity ───────

    #[test]
    fn test_case_insensitive_operations() {
        let mut db = RacfDatabase::new();
        db.add_group("dept01", "sys1", "admin1").unwrap();
        db.add_user("jsmith", "dept01", "John Smith", "admin1")
            .unwrap();

        // Should find via uppercase lookup.
        assert!(db.user_exists("JSMITH"));
        assert!(db.group_exists("DEPT01"));
        let user = db.list_user("jsmith").unwrap();
        assert_eq!(user.profile.userid, "JSMITH");
    }

    // ─────── Validation ───────

    #[test]
    fn test_invalid_userid_rejected() {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();

        // Too long.
        let err = db
            .add_user("TOOLONGNAME", "DEPT01", "Name", "ADMIN1")
            .unwrap_err();
        assert!(matches!(err, RacfError::InvalidUserId { .. }));

        // Empty.
        let err = db.add_user("", "DEPT01", "Name", "ADMIN1").unwrap_err();
        assert!(matches!(err, RacfError::InvalidUserId { .. }));
    }
}
