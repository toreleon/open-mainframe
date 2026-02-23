//! CRYPTO-103: CSFKEYS/CSFSERV RACF Integration.
//!
//! Provides RACF profiles for controlling access to individual ICSF keys
//! (CSFKEYS class) and ICSF services (CSFSERV class), including wildcard
//! generic profiles.

use std::fmt;

// ---------------------------------------------------------------------------
// Access level for RACF crypto profiles
// ---------------------------------------------------------------------------

/// RACF access levels for crypto profile authorization.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CryptoAccessLevel {
    /// No access.
    None,
    /// Read-only access.
    Read,
    /// Read and update access.
    Update,
    /// Full control access.
    Control,
    /// Owner-level access (can change protection).
    Alter,
}

impl fmt::Display for CryptoAccessLevel {
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

// ---------------------------------------------------------------------------
// Story 1: CSFKEYS profiles
// ---------------------------------------------------------------------------

/// A RACF CSFKEYS class profile controlling access to a specific ICSF key.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CsfKeysProfile {
    /// The key label this profile protects.
    pub key_label: String,
    /// Per-user access list.
    pub access_list: Vec<(String, CryptoAccessLevel)>,
    /// Default (UACC) access level for users not in the access list.
    pub uacc: CryptoAccessLevel,
}

impl CsfKeysProfile {
    /// Create a new CSFKEYS profile for a key label.
    pub fn new(key_label: &str, uacc: CryptoAccessLevel) -> Self {
        Self {
            key_label: key_label.to_string(),
            access_list: Vec::new(),
            uacc,
        }
    }

    /// Permit a user at a given access level.
    pub fn permit(&mut self, user: &str, level: CryptoAccessLevel) {
        // Remove any existing entry for this user, then add.
        self.access_list.retain(|(u, _)| u != user);
        self.access_list
            .push((user.to_uppercase(), level));
    }

    /// Returns the effective access level for a given user.
    pub fn effective_access(&self, user: &str) -> CryptoAccessLevel {
        let upper = user.to_uppercase();
        for (u, lvl) in &self.access_list {
            if *u == upper {
                return *lvl;
            }
        }
        self.uacc
    }
}

// ---------------------------------------------------------------------------
// Story 2: CSFSERV profiles
// ---------------------------------------------------------------------------

/// A RACF CSFSERV class profile controlling access to an ICSF service.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CsfServProfile {
    /// The ICSF service name this profile protects (e.g., `CSFENC`, `CSFDEC`).
    pub service_name: String,
    /// Per-user access list.
    pub access_list: Vec<(String, CryptoAccessLevel)>,
    /// Default (UACC) access level.
    pub uacc: CryptoAccessLevel,
}

impl CsfServProfile {
    /// Create a new CSFSERV profile for a service.
    pub fn new(service_name: &str, uacc: CryptoAccessLevel) -> Self {
        Self {
            service_name: service_name.to_string(),
            access_list: Vec::new(),
            uacc,
        }
    }

    /// Permit a user at a given access level.
    pub fn permit(&mut self, user: &str, level: CryptoAccessLevel) {
        self.access_list.retain(|(u, _)| u != user);
        self.access_list
            .push((user.to_uppercase(), level));
    }

    /// Returns the effective access level for a given user.
    pub fn effective_access(&self, user: &str) -> CryptoAccessLevel {
        let upper = user.to_uppercase();
        for (u, lvl) in &self.access_list {
            if *u == upper {
                return *lvl;
            }
        }
        self.uacc
    }
}

// ---------------------------------------------------------------------------
// Story 3: Generic CSFKEYS profiles with wildcard matching
// ---------------------------------------------------------------------------

/// A generic CSFKEYS profile supporting wildcard matching (e.g., `PROD.**`).
///
/// Wildcards follow z/OS RACF conventions:
/// - `*` matches a single qualifier (no dots).
/// - `**` matches zero or more qualifiers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GcsfKeysProfile {
    /// The pattern string (may contain `*` and `**`).
    pub pattern: String,
    /// Per-user access list.
    pub access_list: Vec<(String, CryptoAccessLevel)>,
    /// Default (UACC) access level.
    pub uacc: CryptoAccessLevel,
}

impl GcsfKeysProfile {
    /// Create a new generic CSFKEYS profile.
    pub fn new(pattern: &str, uacc: CryptoAccessLevel) -> Self {
        Self {
            pattern: pattern.to_string(),
            access_list: Vec::new(),
            uacc,
        }
    }

    /// Permit a user at a given access level.
    pub fn permit(&mut self, user: &str, level: CryptoAccessLevel) {
        self.access_list.retain(|(u, _)| u != user);
        self.access_list
            .push((user.to_uppercase(), level));
    }

    /// Returns the effective access level for a given user.
    pub fn effective_access(&self, user: &str) -> CryptoAccessLevel {
        let upper = user.to_uppercase();
        for (u, lvl) in &self.access_list {
            if *u == upper {
                return *lvl;
            }
        }
        self.uacc
    }

    /// Check whether a key label matches this profile's wildcard pattern.
    pub fn matches(&self, key_label: &str) -> bool {
        wildcard_match(&self.pattern, key_label)
    }
}

/// Match a z/OS-style wildcard pattern against a key label.
///
/// - `*` matches exactly one qualifier (segment between dots).
/// - `**` matches zero or more qualifiers.
fn wildcard_match(pattern: &str, input: &str) -> bool {
    let pat_parts: Vec<&str> = pattern.split('.').collect();
    let inp_parts: Vec<&str> = input.split('.').collect();
    wm_recursive(&pat_parts, &inp_parts)
}

fn wm_recursive(pat: &[&str], inp: &[&str]) -> bool {
    if pat.is_empty() {
        return inp.is_empty();
    }
    if pat[0] == "**" {
        // ** matches zero or more qualifiers.
        for skip in 0..=inp.len() {
            if wm_recursive(&pat[1..], &inp[skip..]) {
                return true;
            }
        }
        return false;
    }
    if inp.is_empty() {
        return false;
    }
    if pat[0] == "*" || pat[0].eq_ignore_ascii_case(inp[0]) {
        return wm_recursive(&pat[1..], &inp[1..]);
    }
    false
}

// ---------------------------------------------------------------------------
// Story 4: Access check functions
// ---------------------------------------------------------------------------

/// Check whether a user has at least READ access to a key label.
///
/// Searches through discrete CSFKEYS profiles first, then generic profiles.
/// Returns `true` if any matching profile grants at least READ access.
pub fn check_key_access(
    user: &str,
    key_label: &str,
    discrete_profiles: &[CsfKeysProfile],
    generic_profiles: &[GcsfKeysProfile],
) -> bool {
    // Check discrete profiles first (exact match on key_label).
    for profile in discrete_profiles {
        if profile.key_label == key_label {
            return profile.effective_access(user) >= CryptoAccessLevel::Read;
        }
    }
    // Fall back to generic profiles (most specific first — shortest pattern).
    // In a real implementation, profiles would be ordered by specificity.
    for profile in generic_profiles {
        if profile.matches(key_label) {
            return profile.effective_access(user) >= CryptoAccessLevel::Read;
        }
    }
    false
}

/// Check whether a user has at least READ access to an ICSF service.
///
/// Returns `true` if a matching CSFSERV profile grants at least READ access.
pub fn check_service_access(
    user: &str,
    service: &str,
    profiles: &[CsfServProfile],
) -> bool {
    for profile in profiles {
        if profile.service_name.eq_ignore_ascii_case(service) {
            return profile.effective_access(user) >= CryptoAccessLevel::Read;
        }
    }
    false
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn csfkeys_profile_permit_and_access() {
        let mut p = CsfKeysProfile::new("MY.AES.KEY1", CryptoAccessLevel::None);
        p.permit("JSMITH", CryptoAccessLevel::Read);
        p.permit("ADMIN1", CryptoAccessLevel::Alter);

        assert_eq!(p.effective_access("JSMITH"), CryptoAccessLevel::Read);
        assert_eq!(p.effective_access("ADMIN1"), CryptoAccessLevel::Alter);
        assert_eq!(p.effective_access("NOBODY"), CryptoAccessLevel::None);
    }

    #[test]
    fn csfkeys_permit_overwrites_previous() {
        let mut p = CsfKeysProfile::new("KEY1", CryptoAccessLevel::None);
        p.permit("USER1", CryptoAccessLevel::Read);
        p.permit("USER1", CryptoAccessLevel::Control);
        assert_eq!(p.effective_access("USER1"), CryptoAccessLevel::Control);
        assert_eq!(p.access_list.len(), 1);
    }

    #[test]
    fn csfserv_profile() {
        let mut p = CsfServProfile::new("CSFENC", CryptoAccessLevel::None);
        p.permit("BATCHUSR", CryptoAccessLevel::Update);
        assert_eq!(p.effective_access("BATCHUSR"), CryptoAccessLevel::Update);
        assert_eq!(p.effective_access("RANDOM"), CryptoAccessLevel::None);
    }

    #[test]
    fn generic_wildcard_star() {
        let p = GcsfKeysProfile::new("PROD.*", CryptoAccessLevel::Read);
        assert!(p.matches("PROD.KEY1"));
        assert!(!p.matches("PROD.A.B"));
        assert!(!p.matches("DEV.KEY1"));
    }

    #[test]
    fn generic_wildcard_double_star() {
        let p = GcsfKeysProfile::new("PROD.**", CryptoAccessLevel::Read);
        assert!(p.matches("PROD.KEY1"));
        assert!(p.matches("PROD.A.B.C"));
        assert!(!p.matches("DEV.KEY1"));
    }

    #[test]
    fn generic_double_star_matches_zero() {
        let p = GcsfKeysProfile::new("**", CryptoAccessLevel::Read);
        assert!(p.matches("ANYTHING"));
        assert!(p.matches("A.B.C.D"));
    }

    #[test]
    fn check_key_access_discrete_match() {
        let mut profile = CsfKeysProfile::new("MY.KEY", CryptoAccessLevel::None);
        profile.permit("USER1", CryptoAccessLevel::Read);
        assert!(check_key_access("USER1", "MY.KEY", &[profile], &[]));
    }

    #[test]
    fn check_key_access_discrete_denied() {
        let profile = CsfKeysProfile::new("MY.KEY", CryptoAccessLevel::None);
        assert!(!check_key_access("USER1", "MY.KEY", &[profile], &[]));
    }

    #[test]
    fn check_key_access_generic_fallback() {
        let mut generic = GcsfKeysProfile::new("PROD.**", CryptoAccessLevel::None);
        generic.permit("USER1", CryptoAccessLevel::Control);
        assert!(check_key_access("USER1", "PROD.AES.KEY1", &[], &[generic]));
    }

    #[test]
    fn check_key_access_no_match() {
        assert!(!check_key_access("USER1", "UNKNOWN.KEY", &[], &[]));
    }

    #[test]
    fn check_service_access_granted() {
        let mut p = CsfServProfile::new("CSFENC", CryptoAccessLevel::None);
        p.permit("USER1", CryptoAccessLevel::Read);
        assert!(check_service_access("USER1", "CSFENC", &[p]));
    }

    #[test]
    fn check_service_access_denied() {
        let p = CsfServProfile::new("CSFENC", CryptoAccessLevel::None);
        assert!(!check_service_access("USER1", "CSFENC", &[p]));
    }

    #[test]
    fn check_service_access_case_insensitive() {
        let mut p = CsfServProfile::new("CSFENC", CryptoAccessLevel::Read);
        p.permit("user1", CryptoAccessLevel::Read);
        assert!(check_service_access("user1", "csfenc", &[p]));
    }

    #[test]
    fn crypto_access_level_ordering() {
        assert!(CryptoAccessLevel::Alter > CryptoAccessLevel::Control);
        assert!(CryptoAccessLevel::Control > CryptoAccessLevel::Update);
        assert!(CryptoAccessLevel::Update > CryptoAccessLevel::Read);
        assert!(CryptoAccessLevel::Read > CryptoAccessLevel::None);
    }

    #[test]
    fn display_crypto_access_level() {
        assert_eq!(format!("{}", CryptoAccessLevel::None), "NONE");
        assert_eq!(format!("{}", CryptoAccessLevel::Read), "READ");
        assert_eq!(format!("{}", CryptoAccessLevel::Alter), "ALTER");
    }
}
