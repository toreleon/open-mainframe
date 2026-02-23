//! SEC-107: Security Labels (MAC — Mandatory Access Control).
//!
//! Implements Bell-LaPadula security labels with hierarchical levels and
//! category sets for z/OS RACF SECLABEL class profiles.

use std::collections::BTreeSet;
use std::fmt;

// ---------------------------------------------------------------------------
// Story 1: Security Level
// ---------------------------------------------------------------------------

/// Hierarchical security clearance levels.
///
/// Ordered from least sensitive (Unclassified) to most sensitive (TopSecret).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SecurityLevel {
    /// Publicly available information.
    Unclassified = 0,
    /// Sensitive but not restricted.
    Confidential = 1,
    /// Restricted information.
    Secret = 2,
    /// Highest classification level.
    TopSecret = 3,
}

impl SecurityLevel {
    /// Returns the numeric ordering value for this level.
    pub fn numeric(self) -> u8 {
        self as u8
    }
}

impl fmt::Display for SecurityLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unclassified => write!(f, "UNCLASSIFIED"),
            Self::Confidential => write!(f, "CONFIDENTIAL"),
            Self::Secret => write!(f, "SECRET"),
            Self::TopSecret => write!(f, "TOP SECRET"),
        }
    }
}

// ---------------------------------------------------------------------------
// Story 2: Security Category
// ---------------------------------------------------------------------------

/// A named security category (compartment).
///
/// Categories represent non-hierarchical compartments such as PAYROLL,
/// PERSONNEL, FINANCIAL, etc.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SecurityCategory(String);

impl SecurityCategory {
    /// Create a new security category.
    pub fn new(name: &str) -> Self {
        Self(name.to_uppercase())
    }

    /// Returns the category name.
    pub fn name(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for SecurityCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// ---------------------------------------------------------------------------
// Story 3: Security Label
// ---------------------------------------------------------------------------

/// A security label combining a hierarchical level with a set of categories.
///
/// Used in RACF SECLABEL class to enforce Mandatory Access Control (MAC)
/// through Bell-LaPadula dominance relationships.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SecurityLabel {
    /// The hierarchical security level.
    pub level: SecurityLevel,
    /// The set of security categories (compartments).
    pub categories: BTreeSet<SecurityCategory>,
}

impl SecurityLabel {
    /// Create a new security label with the given level and no categories.
    pub fn new(level: SecurityLevel) -> Self {
        Self {
            level,
            categories: BTreeSet::new(),
        }
    }

    /// Create a new security label with level and categories.
    pub fn with_categories(level: SecurityLevel, categories: Vec<SecurityCategory>) -> Self {
        Self {
            level,
            categories: categories.into_iter().collect(),
        }
    }

    /// Add a category to this label.
    pub fn add_category(&mut self, category: SecurityCategory) {
        self.categories.insert(category);
    }

    /// Remove a category from this label.
    pub fn remove_category(&mut self, category: &SecurityCategory) -> bool {
        self.categories.remove(category)
    }

    /// Returns `true` if this label has the specified category.
    pub fn has_category(&self, category: &SecurityCategory) -> bool {
        self.categories.contains(category)
    }
}

impl fmt::Display for SecurityLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.level)?;
        if !self.categories.is_empty() {
            let cats: Vec<String> = self.categories.iter().map(|c| c.to_string()).collect();
            write!(f, "/{}", cats.join(","))?;
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Story 4: Seclabel Profile
// ---------------------------------------------------------------------------

/// A RACF SECLABEL class profile defining a valid label combination.
///
/// Each named SECLABEL profile maps a friendly name to a `SecurityLabel`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SeclabelProfile {
    /// The SECLABEL profile name (e.g., `SYSHIGH`, `SYSLOW`).
    pub name: String,
    /// The security label this profile defines.
    pub label: SecurityLabel,
}

impl SeclabelProfile {
    /// Create a new SECLABEL profile.
    pub fn new(name: &str, label: SecurityLabel) -> Self {
        Self {
            name: name.to_uppercase(),
            label,
        }
    }
}

impl fmt::Display for SeclabelProfile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SECLABEL({}) = {}", self.name, self.label)
    }
}

// ---------------------------------------------------------------------------
// Story 5: Dominance Check (Bell-LaPadula)
// ---------------------------------------------------------------------------

/// Perform a Bell-LaPadula dominance check.
///
/// Returns `true` if `subject_label` dominates `object_label`, meaning:
/// - The subject's level is >= the object's level, **AND**
/// - The subject's categories are a superset of the object's categories.
///
/// This implements the "simple security property" / "no read up" rule.
pub fn dominance_check(subject_label: &SecurityLabel, object_label: &SecurityLabel) -> bool {
    // Level check: subject must be at least as high.
    if subject_label.level < object_label.level {
        return false;
    }
    // Category check: subject categories must contain all object categories.
    object_label
        .categories
        .iter()
        .all(|cat| subject_label.categories.contains(cat))
}

// ---------------------------------------------------------------------------
// Story 6: MLS Mode
// ---------------------------------------------------------------------------

/// Multi-Level Security (MLS) processing mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MlsMode {
    /// MAC is actively enforced; access violations are denied and logged.
    Active,
    /// MAC is checked but not enforced; violations are logged but allowed.
    Quiet,
    /// MAC processing is disabled.
    Off,
}

impl MlsMode {
    /// Returns `true` if MAC enforcement is active.
    pub fn is_enforcing(self) -> bool {
        self == Self::Active
    }

    /// Returns `true` if MAC checks produce audit records.
    pub fn is_auditing(self) -> bool {
        matches!(self, Self::Active | Self::Quiet)
    }
}

impl fmt::Display for MlsMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Active => write!(f, "MLS(ACTIVE)"),
            Self::Quiet => write!(f, "MLS(QUIET)"),
            Self::Off => write!(f, "MLS(OFF)"),
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn cat(name: &str) -> SecurityCategory {
        SecurityCategory::new(name)
    }

    // --- Security Level ---

    #[test]
    fn level_ordering() {
        assert!(SecurityLevel::TopSecret > SecurityLevel::Secret);
        assert!(SecurityLevel::Secret > SecurityLevel::Confidential);
        assert!(SecurityLevel::Confidential > SecurityLevel::Unclassified);
    }

    #[test]
    fn level_numeric() {
        assert_eq!(SecurityLevel::Unclassified.numeric(), 0);
        assert_eq!(SecurityLevel::TopSecret.numeric(), 3);
    }

    #[test]
    fn level_display() {
        assert_eq!(format!("{}", SecurityLevel::TopSecret), "TOP SECRET");
        assert_eq!(format!("{}", SecurityLevel::Unclassified), "UNCLASSIFIED");
    }

    // --- Security Category ---

    #[test]
    fn category_uppercased() {
        let c = SecurityCategory::new("payroll");
        assert_eq!(c.name(), "PAYROLL");
    }

    // --- Security Label ---

    #[test]
    fn label_display_no_categories() {
        let label = SecurityLabel::new(SecurityLevel::Secret);
        assert_eq!(format!("{}", label), "SECRET");
    }

    #[test]
    fn label_display_with_categories() {
        let label = SecurityLabel::with_categories(
            SecurityLevel::Secret,
            vec![cat("PAYROLL"), cat("FINANCIAL")],
        );
        let display = format!("{}", label);
        assert!(display.starts_with("SECRET/"));
        assert!(display.contains("FINANCIAL"));
        assert!(display.contains("PAYROLL"));
    }

    #[test]
    fn label_add_remove_category() {
        let mut label = SecurityLabel::new(SecurityLevel::Confidential);
        label.add_category(cat("PERSONNEL"));
        assert!(label.has_category(&cat("PERSONNEL")));
        assert!(label.remove_category(&cat("PERSONNEL")));
        assert!(!label.has_category(&cat("PERSONNEL")));
    }

    // --- Seclabel Profile ---

    #[test]
    fn seclabel_profile_display() {
        let label = SecurityLabel::new(SecurityLevel::TopSecret);
        let profile = SeclabelProfile::new("SYSHIGH", label);
        assert_eq!(format!("{}", profile), "SECLABEL(SYSHIGH) = TOP SECRET");
    }

    // --- Dominance Check ---

    #[test]
    fn dominance_same_label() {
        let label = SecurityLabel::with_categories(
            SecurityLevel::Secret,
            vec![cat("PAYROLL")],
        );
        assert!(dominance_check(&label, &label));
    }

    #[test]
    fn dominance_higher_level_superset_categories() {
        let subject = SecurityLabel::with_categories(
            SecurityLevel::TopSecret,
            vec![cat("PAYROLL"), cat("FINANCIAL")],
        );
        let object = SecurityLabel::with_categories(
            SecurityLevel::Secret,
            vec![cat("PAYROLL")],
        );
        assert!(dominance_check(&subject, &object));
    }

    #[test]
    fn dominance_fails_lower_level() {
        let subject = SecurityLabel::new(SecurityLevel::Confidential);
        let object = SecurityLabel::new(SecurityLevel::Secret);
        assert!(!dominance_check(&subject, &object));
    }

    #[test]
    fn dominance_fails_missing_category() {
        let subject = SecurityLabel::with_categories(
            SecurityLevel::TopSecret,
            vec![cat("PAYROLL")],
        );
        let object = SecurityLabel::with_categories(
            SecurityLevel::Secret,
            vec![cat("PAYROLL"), cat("PERSONNEL")],
        );
        assert!(!dominance_check(&subject, &object));
    }

    #[test]
    fn dominance_unclassified_no_categories() {
        let subject = SecurityLabel::new(SecurityLevel::Secret);
        let object = SecurityLabel::new(SecurityLevel::Unclassified);
        assert!(dominance_check(&subject, &object));
    }

    #[test]
    fn dominance_equal_level_subset_categories() {
        let subject = SecurityLabel::with_categories(
            SecurityLevel::Secret,
            vec![cat("PAYROLL"), cat("FINANCIAL"), cat("PERSONNEL")],
        );
        let object = SecurityLabel::with_categories(
            SecurityLevel::Secret,
            vec![cat("PAYROLL"), cat("FINANCIAL")],
        );
        assert!(dominance_check(&subject, &object));
    }

    #[test]
    fn dominance_same_level_missing_one_category() {
        let subject = SecurityLabel::with_categories(
            SecurityLevel::Secret,
            vec![cat("PAYROLL")],
        );
        let object = SecurityLabel::with_categories(
            SecurityLevel::Secret,
            vec![cat("PAYROLL"), cat("PERSONNEL")],
        );
        assert!(!dominance_check(&subject, &object));
    }

    // --- MLS Mode ---

    #[test]
    fn mls_active_enforces_and_audits() {
        assert!(MlsMode::Active.is_enforcing());
        assert!(MlsMode::Active.is_auditing());
    }

    #[test]
    fn mls_quiet_audits_but_not_enforces() {
        assert!(!MlsMode::Quiet.is_enforcing());
        assert!(MlsMode::Quiet.is_auditing());
    }

    #[test]
    fn mls_off_neither() {
        assert!(!MlsMode::Off.is_enforcing());
        assert!(!MlsMode::Off.is_auditing());
    }

    #[test]
    fn mls_display() {
        assert_eq!(format!("{}", MlsMode::Active), "MLS(ACTIVE)");
        assert_eq!(format!("{}", MlsMode::Quiet), "MLS(QUIET)");
        assert_eq!(format!("{}", MlsMode::Off), "MLS(OFF)");
    }
}
