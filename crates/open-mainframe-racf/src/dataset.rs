//! RACF dataset profiles — ADDSD/ALTDSD/LISTDSD/DELDSD, PERMIT, and authorization checking.

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

use crate::types::AccessLevel;

/// A dataset profile protecting one or more datasets by name pattern.
///
/// Profiles can be **discrete** (exact dataset name) or **generic** (contains wildcards).
/// Generic patterns use z/OS conventions:
/// - `*`  — matches a single qualifier (e.g. `SYS1.*.DATA`)
/// - `**` — matches zero or more qualifiers (e.g. `SYS1.**`)
/// - `%`  — matches a single character within a qualifier
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DatasetProfile {
    /// The profile name / pattern (uppercase, dot-separated qualifiers).
    pub name: String,
    /// Whether this is a generic profile (contains wildcard characters).
    pub generic: bool,
    /// Universal access — the default access level for users not on the access list.
    pub uacc: AccessLevel,
    /// Owner of this profile (user ID or group name).
    pub owner: String,
    /// Access list: user/group ID → granted access level.
    pub access_list: BTreeMap<String, AccessLevel>,
    /// Whether the profile was auto-created (e.g. HLQ model).
    pub auto_created: bool,
    /// Creation timestamp.
    pub created: String,
    /// Last update timestamp.
    pub last_updated: String,
}

impl DatasetProfile {
    /// Create a new dataset profile.
    pub fn new(name: String, uacc: AccessLevel, owner: String) -> Self {
        let generic = is_generic_pattern(&name);
        let now = now_iso8601();
        Self {
            name,
            generic,
            uacc,
            owner,
            access_list: BTreeMap::new(),
            auto_created: false,
            created: now.clone(),
            last_updated: now,
        }
    }

    /// Get the effective access level for a given user/group ID.
    ///
    /// Returns the access-list entry if present, otherwise UACC.
    pub fn effective_access(&self, id: &str) -> AccessLevel {
        let id = id.to_uppercase();
        self.access_list
            .get(&id)
            .copied()
            .unwrap_or(self.uacc)
    }
}

/// Check if a dataset profile name contains generic wildcards.
fn is_generic_pattern(name: &str) -> bool {
    name.contains('*') || name.contains('%')
}

/// Match a concrete dataset name against a generic profile pattern.
///
/// Pattern syntax:
/// - `*`  — matches exactly one qualifier (dot-separated segment)
/// - `**` — matches zero or more qualifiers
/// - `%`  — matches exactly one character within a qualifier
///
/// Returns `true` if the dataset name matches the pattern.
pub fn dataset_matches(dataset: &str, pattern: &str) -> bool {
    let ds_parts: Vec<&str> = dataset.split('.').collect();
    let pat_parts: Vec<&str> = pattern.split('.').collect();
    match_parts(&ds_parts, 0, &pat_parts, 0)
}

/// Recursive qualifier-level matching.
fn match_parts(ds: &[&str], di: usize, pat: &[&str], pi: usize) -> bool {
    // Both exhausted — match.
    if di == ds.len() && pi == pat.len() {
        return true;
    }

    // Pattern exhausted but dataset has more qualifiers — no match.
    if pi == pat.len() {
        return false;
    }

    let p = pat[pi];

    // Handle `**` — matches zero or more qualifiers.
    if p == "**" {
        // Try matching ** against 0, 1, 2, ... remaining dataset qualifiers.
        for skip in 0..=(ds.len() - di) {
            if match_parts(ds, di + skip, pat, pi + 1) {
                return true;
            }
        }
        return false;
    }

    // Dataset exhausted but pattern still has entries — no match (unless all remaining are **).
    if di == ds.len() {
        return false;
    }

    // Handle `*` — matches exactly one qualifier (any content).
    if p == "*" {
        return match_parts(ds, di + 1, pat, pi + 1);
    }

    // Handle qualifier with possible `%` characters.
    if qualifier_matches(ds[di], p) {
        return match_parts(ds, di + 1, pat, pi + 1);
    }

    false
}

/// Match a single qualifier against a pattern qualifier that may contain `%` wildcards.
/// `%` matches exactly one character.
fn qualifier_matches(qualifier: &str, pattern: &str) -> bool {
    if qualifier.len() != pattern.len() {
        return false;
    }
    qualifier
        .chars()
        .zip(pattern.chars())
        .all(|(q, p)| p == '%' || q == p)
}

/// Compute the specificity score of a dataset profile pattern.
///
/// More specific patterns get higher scores. Used to select the best-matching
/// profile when multiple generics match a dataset name.
///
/// Scoring: each qualifier contributes to the score:
/// - Literal qualifier: 3 points
/// - `*` (single qualifier wildcard): 2 points
/// - Qualifier with `%`: 2 points (partially specific)
/// - `**` (multi-qualifier wildcard): 1 point
pub fn pattern_specificity(pattern: &str) -> u32 {
    pattern.split('.').map(|q| {
        if q == "**" {
            1
        } else if q == "*" || q.contains('%') {
            2
        } else {
            3
        }
    }).sum()
}

fn now_iso8601() -> String {
    let duration = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    format!("{}", duration.as_secs())
}

#[cfg(test)]
mod tests {
    use super::*;

    // ─────── Pattern matching tests ───────

    #[test]
    fn test_exact_match() {
        assert!(dataset_matches("SYS1.PARMLIB", "SYS1.PARMLIB"));
        assert!(!dataset_matches("SYS1.PARMLIB", "SYS1.PROCLIB"));
    }

    #[test]
    fn test_single_star_matches_one_qualifier() {
        assert!(dataset_matches("SYS1.PARMLIB", "SYS1.*"));
        assert!(dataset_matches("SYS1.PROCLIB", "SYS1.*"));
        // * must match exactly one qualifier — not zero, not two.
        assert!(!dataset_matches("SYS1", "SYS1.*"));
        assert!(!dataset_matches("SYS1.PARMLIB.DATA", "SYS1.*"));
    }

    #[test]
    fn test_double_star_matches_any_qualifiers() {
        assert!(dataset_matches("SYS1.PARMLIB", "SYS1.**"));
        assert!(dataset_matches("SYS1.PARMLIB.MEMBER1", "SYS1.**"));
        assert!(dataset_matches("SYS1", "SYS1.**"));
        assert!(!dataset_matches("PROD.DATA", "SYS1.**"));
    }

    #[test]
    fn test_percent_matches_single_char() {
        assert!(dataset_matches("SYS1.PARMLIB", "SYS1.PARM%IB"));
        assert!(dataset_matches("SYS1.PARXLIB", "SYS1.PAR%LIB"));
        assert!(!dataset_matches("SYS1.PARMXLIB", "SYS1.PARM%IB"));
    }

    #[test]
    fn test_mixed_wildcards() {
        // SYS1.*.DATA matches SYS1.ANYTHING.DATA
        assert!(dataset_matches("SYS1.FOO.DATA", "SYS1.*.DATA"));
        assert!(!dataset_matches("SYS1.FOO.BAR.DATA", "SYS1.*.DATA"));

        // SYS1.**.DATA matches any depth ending in DATA
        assert!(dataset_matches("SYS1.FOO.DATA", "SYS1.**.DATA"));
        assert!(dataset_matches("SYS1.FOO.BAR.DATA", "SYS1.**.DATA"));
        assert!(dataset_matches("SYS1.DATA", "SYS1.**.DATA"));
    }

    #[test]
    fn test_most_specific_wins() {
        // SYS1.PARMLIB.* is more specific than SYS1.**
        let spec1 = pattern_specificity("SYS1.PARMLIB.*");
        let spec2 = pattern_specificity("SYS1.**");
        assert!(spec1 > spec2);

        // SYS1.PARMLIB is more specific than SYS1.*
        let spec3 = pattern_specificity("SYS1.PARMLIB");
        let spec4 = pattern_specificity("SYS1.*");
        assert!(spec3 > spec4);
    }

    // ─────── Dataset profile tests ───────

    #[test]
    fn test_profile_creation() {
        let profile = DatasetProfile::new(
            "PROD.PAYROLL.**".to_string(),
            AccessLevel::None,
            "ADMIN1".to_string(),
        );
        assert!(profile.generic);
        assert_eq!(profile.uacc, AccessLevel::None);
        assert_eq!(profile.owner, "ADMIN1");
    }

    #[test]
    fn test_discrete_profile_not_generic() {
        let profile = DatasetProfile::new(
            "SYS1.PARMLIB".to_string(),
            AccessLevel::Read,
            "SYS1".to_string(),
        );
        assert!(!profile.generic);
    }

    #[test]
    fn test_effective_access_from_list() {
        let mut profile = DatasetProfile::new(
            "PROD.**".to_string(),
            AccessLevel::None,
            "ADMIN1".to_string(),
        );
        profile.access_list.insert("JSMITH".to_string(), AccessLevel::Read);

        assert_eq!(profile.effective_access("JSMITH"), AccessLevel::Read);
        assert_eq!(profile.effective_access("UNKNOWN"), AccessLevel::None);
    }
}
