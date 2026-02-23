//! Security Labels (SECLABEL) — Mandatory Access Control (MAC).
//!
//! Implements the z/OS RACF SECLABEL class for mandatory access control:
//! - **Security Levels** — hierarchical classification (TOP SECRET > SECRET > CONFIDENTIAL > UNCLASSIFIED)
//! - **Security Categories** — non-hierarchical compartments (PAYROLL, PERSONNEL, FINANCIAL)
//! - **SECLABEL profiles** — combinations of level + categories
//! - **Dominance checking** — user label must dominate resource label for access

use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

// ---------------------------------------------------------------------------
//  Security Level (hierarchical)
// ---------------------------------------------------------------------------

/// A security level — hierarchical classification.
///
/// Higher numeric value = higher classification.
/// Standard z/OS levels: UNCLASSIFIED(0) < CONFIDENTIAL(1) < SECRET(2) < TOP SECRET(3).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityLevel {
    /// Level name (e.g. "TOP SECRET").
    pub name: String,
    /// Numeric value — higher = more classified.
    pub value: u32,
}

// ---------------------------------------------------------------------------
//  Security Category (non-hierarchical)
// ---------------------------------------------------------------------------

/// A security category — non-hierarchical compartment.
///
/// Categories represent separate domains (e.g. PAYROLL, PERSONNEL).
/// A user must have ALL categories on a resource's label to access it.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityCategory {
    /// Category name (e.g. "PAYROLL").
    pub name: String,
    /// Category number (0-255).
    pub number: u16,
}

// ---------------------------------------------------------------------------
//  Security Label
// ---------------------------------------------------------------------------

/// A security label — combination of a security level and zero or more categories.
///
/// Labels are defined as SECLABEL class profiles. They encode mandatory
/// access control policy: a user's label must *dominate* the resource's
/// label for access to be granted.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityLabel {
    /// Label name (profile name in SECLABEL class).
    pub name: String,
    /// The security level associated with this label.
    pub level: u32,
    /// The set of category names included in this label.
    pub categories: BTreeSet<String>,
}

impl SecurityLabel {
    /// Create a new security label with the given level and categories.
    pub fn new(name: &str, level: u32, categories: impl IntoIterator<Item = String>) -> Self {
        Self {
            name: name.trim().to_uppercase(),
            level,
            categories: categories.into_iter().map(|c| c.to_uppercase()).collect(),
        }
    }

    /// Check if this label dominates another label.
    ///
    /// Label A dominates label B if:
    /// 1. A's security level >= B's security level, AND
    /// 2. A's categories are a superset of B's categories.
    pub fn dominates(&self, other: &SecurityLabel) -> bool {
        // Level check: must be >= the other.
        if self.level < other.level {
            return false;
        }
        // Category check: must include all of other's categories.
        other.categories.is_subset(&self.categories)
    }
}

// ---------------------------------------------------------------------------
//  SECLABEL Manager
// ---------------------------------------------------------------------------

/// Manages security labels, levels, and categories for MAC enforcement.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SeclabelManager {
    /// Defined security levels: name → SecurityLevel.
    levels: BTreeMap<String, SecurityLevel>,
    /// Defined security categories: name → SecurityCategory.
    categories: BTreeMap<String, SecurityCategory>,
    /// Defined security labels: name → SecurityLabel.
    labels: BTreeMap<String, SecurityLabel>,
    /// User SECLABEL assignments: userid → label name.
    user_labels: BTreeMap<String, String>,
    /// Resource SECLABEL assignments: (class, resource) → label name.
    resource_labels: BTreeMap<(String, String), String>,
}

impl SeclabelManager {
    /// Create a new SECLABEL manager.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a manager pre-populated with standard z/OS security levels.
    pub fn with_standard_levels() -> Self {
        let mut mgr = Self::new();
        mgr.define_level("UNCLASSIFIED", 0);
        mgr.define_level("CONFIDENTIAL", 1);
        mgr.define_level("SECRET", 2);
        mgr.define_level("TOPSECRET", 3);
        mgr
    }

    // ─────── Level management ───────

    /// Define a security level.
    pub fn define_level(&mut self, name: &str, value: u32) {
        let upper = name.trim().to_uppercase();
        self.levels.insert(
            upper.clone(),
            SecurityLevel {
                name: upper,
                value,
            },
        );
    }

    /// Get a security level by name.
    pub fn get_level(&self, name: &str) -> Option<&SecurityLevel> {
        self.levels.get(&name.trim().to_uppercase())
    }

    /// List all defined levels.
    pub fn list_levels(&self) -> Vec<&SecurityLevel> {
        self.levels.values().collect()
    }

    // ─────── Category management ───────

    /// Define a security category.
    pub fn define_category(&mut self, name: &str, number: u16) {
        let upper = name.trim().to_uppercase();
        self.categories.insert(
            upper.clone(),
            SecurityCategory {
                name: upper,
                number,
            },
        );
    }

    /// Get a security category by name.
    pub fn get_category(&self, name: &str) -> Option<&SecurityCategory> {
        self.categories.get(&name.trim().to_uppercase())
    }

    /// List all defined categories.
    pub fn list_categories(&self) -> Vec<&SecurityCategory> {
        self.categories.values().collect()
    }

    // ─────── Label management ───────

    /// Define a security label (SECLABEL class profile).
    ///
    /// The label combines a security level with zero or more categories.
    /// `level_name` must refer to a previously defined security level.
    pub fn define_label(
        &mut self,
        name: &str,
        level_name: &str,
        category_names: &[&str],
    ) -> bool {
        let level_upper = level_name.trim().to_uppercase();
        let Some(level) = self.levels.get(&level_upper) else {
            return false;
        };
        let level_value = level.value;

        let categories: BTreeSet<String> = category_names
            .iter()
            .map(|c| c.trim().to_uppercase())
            .collect();

        let label_name = name.trim().to_uppercase();
        self.labels.insert(
            label_name.clone(),
            SecurityLabel {
                name: label_name,
                level: level_value,
                categories,
            },
        );
        true
    }

    /// Get a security label by name.
    pub fn get_label(&self, name: &str) -> Option<&SecurityLabel> {
        self.labels.get(&name.trim().to_uppercase())
    }

    /// Delete a security label.
    pub fn delete_label(&mut self, name: &str) -> bool {
        self.labels.remove(&name.trim().to_uppercase()).is_some()
    }

    /// List all defined labels.
    pub fn list_labels(&self) -> Vec<&SecurityLabel> {
        self.labels.values().collect()
    }

    // ─────── Label assignment ───────

    /// Assign a security label to a user (ALTUSER SECLABEL(label)).
    pub fn assign_user_label(&mut self, userid: &str, label_name: &str) -> bool {
        let label_upper = label_name.trim().to_uppercase();
        if !self.labels.contains_key(&label_upper) {
            return false;
        }
        self.user_labels
            .insert(userid.trim().to_uppercase(), label_upper);
        true
    }

    /// Get the security label assigned to a user.
    pub fn user_label(&self, userid: &str) -> Option<&SecurityLabel> {
        let userid_upper = userid.trim().to_uppercase();
        self.user_labels
            .get(&userid_upper)
            .and_then(|name| self.labels.get(name))
    }

    /// Remove a user's security label assignment.
    pub fn remove_user_label(&mut self, userid: &str) -> bool {
        self.user_labels
            .remove(&userid.trim().to_uppercase())
            .is_some()
    }

    /// Assign a security label to a resource (RALTER SECLABEL(label)).
    pub fn assign_resource_label(
        &mut self,
        class: &str,
        resource: &str,
        label_name: &str,
    ) -> bool {
        let label_upper = label_name.trim().to_uppercase();
        if !self.labels.contains_key(&label_upper) {
            return false;
        }
        let key = (
            class.trim().to_uppercase(),
            resource.trim().to_uppercase(),
        );
        self.resource_labels.insert(key, label_upper);
        true
    }

    /// Get the security label assigned to a resource.
    pub fn resource_label(&self, class: &str, resource: &str) -> Option<&SecurityLabel> {
        let key = (
            class.trim().to_uppercase(),
            resource.trim().to_uppercase(),
        );
        self.resource_labels
            .get(&key)
            .and_then(|name| self.labels.get(name))
    }

    // ─────── MAC enforcement ───────

    /// Check MAC (Mandatory Access Control) dominance.
    ///
    /// Returns `true` if the user's security label dominates the resource's label
    /// (i.e., user level >= resource level AND user categories include all resource categories).
    ///
    /// If either user or resource has no label assigned, returns `true` (no MAC restriction).
    pub fn check_mac(
        &self,
        userid: &str,
        class: &str,
        resource: &str,
    ) -> MacCheckResult {
        let user_label = self.user_label(userid);
        let resource_label = self.resource_label(class, resource);

        match (user_label, resource_label) {
            (None, _) => MacCheckResult {
                allowed: true,
                reason: MacReason::NoUserLabel,
                user_label: None,
                resource_label: None,
            },
            (_, None) => MacCheckResult {
                allowed: true,
                reason: MacReason::NoResourceLabel,
                user_label: user_label.map(|l| l.name.clone()),
                resource_label: None,
            },
            (Some(ul), Some(rl)) => {
                let dominates = ul.dominates(rl);
                MacCheckResult {
                    allowed: dominates,
                    reason: if dominates {
                        MacReason::Dominates
                    } else if ul.level < rl.level {
                        MacReason::InsufficientLevel
                    } else {
                        MacReason::MissingCategories
                    },
                    user_label: Some(ul.name.clone()),
                    resource_label: Some(rl.name.clone()),
                }
            }
        }
    }
}

/// Result of a MAC (Mandatory Access Control) check.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacCheckResult {
    /// Whether access is allowed under MAC policy.
    pub allowed: bool,
    /// The reason for the decision.
    pub reason: MacReason,
    /// The user's security label name (if assigned).
    pub user_label: Option<String>,
    /// The resource's security label name (if assigned).
    pub resource_label: Option<String>,
}

/// Reason for a MAC decision.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacReason {
    /// User label dominates resource label — access allowed.
    Dominates,
    /// User has no security label — MAC not enforced.
    NoUserLabel,
    /// Resource has no security label — MAC not enforced.
    NoResourceLabel,
    /// User's security level is lower than resource's.
    InsufficientLevel,
    /// User is missing required security categories.
    MissingCategories,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup() -> SeclabelManager {
        let mut mgr = SeclabelManager::with_standard_levels();
        mgr.define_category("PAYROLL", 1);
        mgr.define_category("PERSONNEL", 2);
        mgr.define_category("FINANCIAL", 3);

        // Define labels.
        mgr.define_label("UNCLASS", "UNCLASSIFIED", &[]);
        mgr.define_label("CONF", "CONFIDENTIAL", &[]);
        mgr.define_label("SECRET", "SECRET", &[]);
        mgr.define_label("TOPSECRET", "TOPSECRET", &[]);
        mgr.define_label("CONF_PAY", "CONFIDENTIAL", &["PAYROLL"]);
        mgr.define_label("SECRET_PAY", "SECRET", &["PAYROLL"]);
        mgr.define_label(
            "SECRET_PAY_FIN",
            "SECRET",
            &["PAYROLL", "FINANCIAL"],
        );
        mgr
    }

    // ─────── S107.1: Security Levels ───────

    #[test]
    fn test_standard_levels_defined() {
        let mgr = SeclabelManager::with_standard_levels();
        assert!(mgr.get_level("UNCLASSIFIED").is_some());
        assert!(mgr.get_level("CONFIDENTIAL").is_some());
        assert!(mgr.get_level("SECRET").is_some());
        assert!(mgr.get_level("TOPSECRET").is_some());
    }

    #[test]
    fn test_level_hierarchy() {
        let mgr = SeclabelManager::with_standard_levels();
        assert!(mgr.get_level("TOPSECRET").unwrap().value > mgr.get_level("SECRET").unwrap().value);
        assert!(mgr.get_level("SECRET").unwrap().value > mgr.get_level("CONFIDENTIAL").unwrap().value);
        assert!(mgr.get_level("CONFIDENTIAL").unwrap().value > mgr.get_level("UNCLASSIFIED").unwrap().value);
    }

    // ─────── S107.2: Security Categories ───────

    #[test]
    fn test_categories() {
        let mgr = setup();
        assert!(mgr.get_category("PAYROLL").is_some());
        assert!(mgr.get_category("PERSONNEL").is_some());
        assert!(mgr.get_category("FINANCIAL").is_some());
        assert_eq!(mgr.list_categories().len(), 3);
    }

    // ─────── S107.3: SECLABEL Profiles ───────

    #[test]
    fn test_label_definition() {
        let mgr = setup();
        let label = mgr.get_label("CONF_PAY").unwrap();
        assert_eq!(label.level, 1); // CONFIDENTIAL
        assert!(label.categories.contains("PAYROLL"));
    }

    #[test]
    fn test_label_definition_invalid_level_fails() {
        let mut mgr = SeclabelManager::new();
        assert!(!mgr.define_label("BAD", "NOSUCHLEVEL", &[]));
    }

    #[test]
    fn test_label_delete() {
        let mut mgr = setup();
        assert!(mgr.delete_label("CONF_PAY"));
        assert!(mgr.get_label("CONF_PAY").is_none());
    }

    // ─────── S107.4: Label Dominance Checking ───────

    #[test]
    fn test_dominance_same_level_no_categories() {
        let mgr = setup();
        let secret = mgr.get_label("SECRET").unwrap();
        let conf = mgr.get_label("CONF").unwrap();

        // SECRET dominates CONFIDENTIAL (higher level).
        assert!(secret.dominates(conf));
        // CONFIDENTIAL does not dominate SECRET.
        assert!(!conf.dominates(secret));
        // SECRET dominates itself.
        assert!(secret.dominates(secret));
    }

    #[test]
    fn test_dominance_with_categories() {
        let mgr = setup();
        let secret_pay_fin = mgr.get_label("SECRET_PAY_FIN").unwrap();
        let secret_pay = mgr.get_label("SECRET_PAY").unwrap();
        let conf_pay = mgr.get_label("CONF_PAY").unwrap();

        // SECRET(PAYROLL,FINANCIAL) dominates SECRET(PAYROLL).
        assert!(secret_pay_fin.dominates(secret_pay));
        // SECRET(PAYROLL) does NOT dominate SECRET(PAYROLL,FINANCIAL) — missing FINANCIAL.
        assert!(!secret_pay.dominates(secret_pay_fin));
        // SECRET(PAYROLL) dominates CONFIDENTIAL(PAYROLL) — higher level, same categories.
        assert!(secret_pay.dominates(conf_pay));
    }

    #[test]
    fn test_dominance_higher_level_missing_category() {
        let mgr = setup();
        let secret = mgr.get_label("SECRET").unwrap(); // SECRET, no categories
        let conf_pay = mgr.get_label("CONF_PAY").unwrap(); // CONFIDENTIAL + PAYROLL

        // SECRET (no categories) does NOT dominate CONFIDENTIAL+PAYROLL — missing PAYROLL category.
        assert!(!secret.dominates(conf_pay));
    }

    // ─────── S107.5: MAC Enforcement ───────

    #[test]
    fn test_mac_check_dominance_allowed() {
        let mut mgr = setup();
        mgr.assign_user_label("JSMITH", "SECRET_PAY");
        mgr.assign_resource_label("DATASET", "PROD.PAYROLL.DATA", "CONF_PAY");

        let result = mgr.check_mac("JSMITH", "DATASET", "PROD.PAYROLL.DATA");
        assert!(result.allowed);
        assert_eq!(result.reason, MacReason::Dominates);
    }

    #[test]
    fn test_mac_check_insufficient_level() {
        let mut mgr = setup();
        mgr.assign_user_label("JSMITH", "CONF_PAY");
        mgr.assign_resource_label("DATASET", "PROD.SECRET.DATA", "SECRET_PAY");

        let result = mgr.check_mac("JSMITH", "DATASET", "PROD.SECRET.DATA");
        assert!(!result.allowed);
        assert_eq!(result.reason, MacReason::InsufficientLevel);
    }

    #[test]
    fn test_mac_check_missing_categories() {
        let mut mgr = setup();
        mgr.assign_user_label("JSMITH", "SECRET_PAY");
        mgr.assign_resource_label("DATASET", "PROD.COMBINED", "SECRET_PAY_FIN");

        let result = mgr.check_mac("JSMITH", "DATASET", "PROD.COMBINED");
        assert!(!result.allowed);
        assert_eq!(result.reason, MacReason::MissingCategories);
    }

    #[test]
    fn test_mac_check_no_user_label() {
        let mut mgr = setup();
        mgr.assign_resource_label("DATASET", "PROD.DATA", "SECRET");

        let result = mgr.check_mac("JSMITH", "DATASET", "PROD.DATA");
        assert!(result.allowed);
        assert_eq!(result.reason, MacReason::NoUserLabel);
    }

    #[test]
    fn test_mac_check_no_resource_label() {
        let mut mgr = setup();
        mgr.assign_user_label("JSMITH", "SECRET");

        let result = mgr.check_mac("JSMITH", "DATASET", "PROD.DATA");
        assert!(result.allowed);
        assert_eq!(result.reason, MacReason::NoResourceLabel);
    }

    #[test]
    fn test_assign_invalid_label_fails() {
        let mut mgr = setup();
        assert!(!mgr.assign_user_label("JSMITH", "NOSUCHLABEL"));
        assert!(!mgr.assign_resource_label("DATASET", "X", "NOSUCHLABEL"));
    }

    #[test]
    fn test_remove_user_label() {
        let mut mgr = setup();
        mgr.assign_user_label("JSMITH", "SECRET");
        assert!(mgr.user_label("JSMITH").is_some());
        assert!(mgr.remove_user_label("JSMITH"));
        assert!(mgr.user_label("JSMITH").is_none());
    }
}
