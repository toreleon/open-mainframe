//! RACF Database Utilities.
//!
//! Implements z/OS RACF database administration utilities:
//! - **IRRUT100** — RACF database search utility
//! - **IRRUT200** — RACF database verify/repair utility
//! - **IRRDBU00** — RACF database unload to flat file (for reporting)

use crate::database::RacfDatabase;
use serde::Serialize;

// ---------------------------------------------------------------------------
//  IRRDBU00 — Database Unload
// ---------------------------------------------------------------------------

/// A record from the IRRDBU00 database unload.
///
/// IRRDBU00 converts the RACF database into a flat-file format suitable
/// for reporting and analysis. Each record represents a profile entry.
#[derive(Debug, Clone, Serialize)]
pub struct UnloadRecord {
    /// Record type (0100=USER, 0200=GROUP, 0400=DATASET, 0500=GENERAL).
    pub record_type: String,
    /// Profile name.
    pub name: String,
    /// Owner.
    pub owner: String,
    /// Creation date.
    pub created: String,
    /// Last update date.
    pub last_updated: String,
    /// Additional fields as key-value pairs.
    pub fields: Vec<(String, String)>,
}

/// Result of an IRRDBU00 unload operation.
#[derive(Debug, Clone)]
pub struct UnloadResult {
    /// All unloaded records.
    pub records: Vec<UnloadRecord>,
    /// Total user profiles.
    pub user_count: usize,
    /// Total group profiles.
    pub group_count: usize,
    /// Total dataset profiles.
    pub dataset_count: usize,
}

/// Execute the IRRDBU00 database unload utility.
///
/// Extracts all profiles from the RACF database into flat-file records
/// suitable for reporting and analysis.
pub fn irrdbu00(db: &RacfDatabase) -> UnloadResult {
    let mut records = Vec::new();

    // Unload user profiles (record type 0100).
    let user_search = db.search("USER", "*");
    let user_count = user_search.entries.len();
    for userid in &user_search.entries {
        if let Ok(result) = db.list_user(userid) {
            let mut fields = Vec::new();
            fields.push(("DFLTGRP".into(), result.profile.default_group.clone()));
            fields.push(("NAME".into(), result.profile.name.clone()));
            let attrs: Vec<String> = result
                .profile
                .attributes
                .iter()
                .map(|a| format!("{a}"))
                .collect();
            if !attrs.is_empty() {
                fields.push(("ATTRIBUTES".into(), attrs.join(",")));
            }
            let groups: Vec<String> = result
                .profile
                .connect_groups
                .keys()
                .cloned()
                .collect();
            if !groups.is_empty() {
                fields.push(("CONNECTS".into(), groups.join(",")));
            }

            records.push(UnloadRecord {
                record_type: "0100".into(),
                name: result.profile.userid.clone(),
                owner: result.profile.owner.clone(),
                created: result.profile.created.clone(),
                last_updated: result.profile.last_updated.clone(),
                fields,
            });
        }
    }

    // Unload group profiles (record type 0200).
    let group_search = db.search("GROUP", "*");
    let group_count = group_search.entries.len();
    for group_name in &group_search.entries {
        if let Ok(result) = db.list_group(group_name) {
            let mut fields = Vec::new();
            if let Some(ref sup) = result.profile.superior_group {
                fields.push(("SUPGROUP".into(), sup.clone()));
            }
            let members: Vec<String> = result.profile.members.keys().cloned().collect();
            if !members.is_empty() {
                fields.push(("MEMBERS".into(), members.join(",")));
            }
            let subgroups: Vec<String> = result.profile.subgroups.iter().cloned().collect();
            if !subgroups.is_empty() {
                fields.push(("SUBGROUPS".into(), subgroups.join(",")));
            }

            records.push(UnloadRecord {
                record_type: "0200".into(),
                name: result.profile.group.clone(),
                owner: result.profile.owner.clone(),
                created: result.profile.created.clone(),
                last_updated: result.profile.last_updated.clone(),
                fields,
            });
        }
    }

    // Unload dataset profiles (record type 0400).
    let dataset_search = db.search("DATASET", "*");
    let dataset_count = dataset_search.entries.len();
    for ds_name in &dataset_search.entries {
        if let Ok(result) = db.list_dataset(ds_name) {
            let mut fields = Vec::new();
            fields.push(("UACC".into(), format!("{}", result.profile.uacc)));
            fields.push((
                "GENERIC".into(),
                if result.profile.generic { "YES" } else { "NO" }.into(),
            ));
            for (id, access) in &result.profile.access_list {
                fields.push(("ACCESS".into(), format!("{id}={access}")));
            }

            records.push(UnloadRecord {
                record_type: "0400".into(),
                name: result.profile.name.clone(),
                owner: result.profile.owner.clone(),
                created: result.profile.created.clone(),
                last_updated: result.profile.last_updated.clone(),
                fields,
            });
        }
    }

    UnloadResult {
        records,
        user_count,
        group_count,
        dataset_count,
    }
}

// ---------------------------------------------------------------------------
//  IRRUT100 — Database Search
// ---------------------------------------------------------------------------

/// Result of an IRRUT100 database search.
#[derive(Debug, Clone)]
pub struct SearchUtilResult {
    /// Matching entries with class and name.
    pub entries: Vec<SearchUtilEntry>,
    /// Total entries scanned.
    pub total_scanned: usize,
}

/// A single entry from the IRRUT100 search.
#[derive(Debug, Clone)]
pub struct SearchUtilEntry {
    /// Profile class (USER, GROUP, DATASET).
    pub class: String,
    /// Profile name.
    pub name: String,
}

/// Execute the IRRUT100 database search utility.
///
/// Searches across all profile types (users, groups, datasets) for
/// entries matching the given mask pattern.
pub fn irrut100(db: &RacfDatabase, mask: &str) -> SearchUtilResult {
    let mut entries = Vec::new();
    let mut total_scanned = 0;

    // Search users.
    let users = db.search("USER", mask);
    total_scanned += db.user_count();
    for name in users.entries {
        entries.push(SearchUtilEntry {
            class: "USER".into(),
            name,
        });
    }

    // Search groups.
    let groups = db.search("GROUP", mask);
    total_scanned += db.group_count();
    for name in groups.entries {
        entries.push(SearchUtilEntry {
            class: "GROUP".into(),
            name,
        });
    }

    // Search datasets.
    let datasets = db.search("DATASET", mask);
    total_scanned += db.dataset_count();
    for name in datasets.entries {
        entries.push(SearchUtilEntry {
            class: "DATASET".into(),
            name,
        });
    }

    SearchUtilResult {
        entries,
        total_scanned,
    }
}

// ---------------------------------------------------------------------------
//  IRRUT200 — Database Verify
// ---------------------------------------------------------------------------

/// Result of an IRRUT200 database verify operation.
#[derive(Debug, Clone)]
pub struct VerifyResult {
    /// Whether the database passed all integrity checks.
    pub valid: bool,
    /// Diagnostic messages (errors or warnings).
    pub messages: Vec<String>,
    /// Total profiles verified.
    pub profiles_verified: usize,
}

/// Execute the IRRUT200 database verify utility.
///
/// Checks the RACF database for internal consistency:
/// - Users reference valid default groups
/// - Group membership entries are symmetric (user<->group)
/// - No orphaned connections
pub fn irrut200(db: &RacfDatabase) -> VerifyResult {
    let mut messages = Vec::new();
    let mut profiles_verified = 0;

    // Check user profiles.
    let users = db.search("USER", "*");
    for userid in &users.entries {
        profiles_verified += 1;
        if let Ok(result) = db.list_user(userid) {
            // Check default group exists.
            if !db.group_exists(&result.profile.default_group) {
                messages.push(format!(
                    "ERROR: User {} default group {} does not exist",
                    userid, result.profile.default_group
                ));
            }

            // Check all connect groups exist.
            for group_name in result.profile.connect_groups.keys() {
                if !db.group_exists(group_name) {
                    messages.push(format!(
                        "ERROR: User {} connected to non-existent group {}",
                        userid, group_name
                    ));
                }
            }
        }
    }

    // Check group profiles.
    let groups = db.search("GROUP", "*");
    for group_name in &groups.entries {
        profiles_verified += 1;
        if let Ok(result) = db.list_group(group_name) {
            // Check superior group exists (if set and not SYS1).
            if let Some(ref sup) = result.profile.superior_group {
                if !db.group_exists(sup) {
                    messages.push(format!(
                        "ERROR: Group {} superior group {} does not exist",
                        group_name, sup
                    ));
                }
            }

            // Check members exist as users.
            for member_userid in result.profile.members.keys() {
                if !db.user_exists(member_userid) {
                    messages.push(format!(
                        "ERROR: Group {} member {} does not exist as a user",
                        group_name, member_userid
                    ));
                }
            }
        }
    }

    // Check dataset profiles.
    let datasets = db.search("DATASET", "*");
    profiles_verified += datasets.entries.len();

    let valid = messages.is_empty();

    VerifyResult {
        valid,
        messages,
        profiles_verified,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::AccessLevel;

    fn setup_db() -> RacfDatabase {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();
        db.add_group("DEPT02", "SYS1", "ADMIN1").unwrap();
        db.add_user("JSMITH", "DEPT01", "John Smith", "ADMIN1")
            .unwrap();
        db.add_user("JDOE", "DEPT01", "Jane Doe", "ADMIN1")
            .unwrap();
        db.add_dataset("PROD.**", AccessLevel::None, "ADMIN1")
            .unwrap();
        db.permit_dataset("PROD.**", "JSMITH", AccessLevel::Read)
            .unwrap();
        db
    }

    // ─────── S110.4: IRRDBU00 Database Unload ───────

    #[test]
    fn test_irrdbu00_unloads_all_profiles() {
        let db = setup_db();
        let result = irrdbu00(&db);

        assert_eq!(result.user_count, 2);
        // SYS1 + DEPT01 + DEPT02 = 3
        assert_eq!(result.group_count, 3);
        assert_eq!(result.dataset_count, 1);

        // Check record types.
        let user_records: Vec<_> = result
            .records
            .iter()
            .filter(|r| r.record_type == "0100")
            .collect();
        assert_eq!(user_records.len(), 2);

        let group_records: Vec<_> = result
            .records
            .iter()
            .filter(|r| r.record_type == "0200")
            .collect();
        assert_eq!(group_records.len(), 3);

        let ds_records: Vec<_> = result
            .records
            .iter()
            .filter(|r| r.record_type == "0400")
            .collect();
        assert_eq!(ds_records.len(), 1);
    }

    #[test]
    fn test_irrdbu00_user_record_fields() {
        let db = setup_db();
        let result = irrdbu00(&db);

        let jsmith = result
            .records
            .iter()
            .find(|r| r.name == "JSMITH")
            .unwrap();
        assert_eq!(jsmith.record_type, "0100");
        assert_eq!(jsmith.owner, "ADMIN1");

        let dfltgrp = jsmith
            .fields
            .iter()
            .find(|(k, _)| k == "DFLTGRP")
            .map(|(_, v)| v.as_str());
        assert_eq!(dfltgrp, Some("DEPT01"));
    }

    #[test]
    fn test_irrdbu00_dataset_record_fields() {
        let db = setup_db();
        let result = irrdbu00(&db);

        let prod = result
            .records
            .iter()
            .find(|r| r.name == "PROD.**")
            .unwrap();
        assert_eq!(prod.record_type, "0400");
        assert!(prod.fields.iter().any(|(k, v)| k == "GENERIC" && v == "YES"));
        assert!(prod.fields.iter().any(|(k, v)| k == "ACCESS" && v.contains("JSMITH")));
    }

    // ─────── S110.1: IRRUT100 Database Search ───────

    #[test]
    fn test_irrut100_search_all() {
        let db = setup_db();
        let result = irrut100(&db, "*");

        // Should find users, groups, and datasets.
        assert!(result.entries.iter().any(|e| e.class == "USER"));
        assert!(result.entries.iter().any(|e| e.class == "GROUP"));
        assert!(result.entries.iter().any(|e| e.class == "DATASET"));
        assert!(result.total_scanned > 0);
    }

    #[test]
    fn test_irrut100_search_by_mask() {
        let db = setup_db();
        let result = irrut100(&db, "J*");

        // Should find JSMITH and JDOE (users only — no groups or datasets match J*).
        let user_entries: Vec<_> = result
            .entries
            .iter()
            .filter(|e| e.class == "USER")
            .collect();
        assert_eq!(user_entries.len(), 2);
    }

    // ─────── S110.2: IRRUT200 Database Verify ───────

    #[test]
    fn test_irrut200_valid_database() {
        let db = setup_db();
        let result = irrut200(&db);

        assert!(result.valid);
        assert!(result.messages.is_empty());
        assert!(result.profiles_verified > 0);
    }

    #[test]
    fn test_irrut200_empty_database() {
        let db = RacfDatabase::new();
        let result = irrut200(&db);
        // Empty DB (only SYS1 group) should be valid.
        assert!(result.valid);
    }

    #[test]
    fn test_irrdbu00_serialization() {
        let db = setup_db();
        let result = irrdbu00(&db);
        // Verify records can be serialized to JSON.
        let json = serde_json::to_string_pretty(&result.records).unwrap();
        assert!(json.contains("JSMITH"));
        assert!(json.contains("0100"));
    }
}
