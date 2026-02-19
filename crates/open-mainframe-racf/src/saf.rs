//! SAF (System Authorization Facility) router and RACROUTE interface.
//!
//! The SAF router dispatches security requests from subsystems (CICS, DB2, JES2, etc.)
//! to the active security product (RACF). This module implements the RACROUTE macro
//! interface with REQUEST=AUTH, FASTAUTH, VERIFY, and EXTRACT.

use serde::{Deserialize, Serialize};
use tracing::{debug, info};

use crate::database::RacfDatabase;
use crate::resource::{AuthReason, ResourceManager};
use crate::types::AccessLevel;

// ─────────────────────── Return Codes ───────────────────────

/// SAF router return codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[repr(u8)]
pub enum SafRc {
    /// Request processed successfully by the security product.
    Success = 0,
    /// RACF not active — caller should use installation default.
    NotActive = 4,
    /// Request failed — check RACF return code for details.
    Failed = 8,
}

/// RACF return codes (set when SAF RC = 0 or 8).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[repr(u8)]
pub enum RacfRc {
    /// Authorized / request successful.
    Authorized = 0,
    /// Not authorized.
    NotAuthorized = 8,
    /// No matching profile found.
    NoProfile = 14,
}

/// Combined result of a RACROUTE call.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RacrouteResult {
    /// SAF return code.
    pub saf_rc: SafRc,
    /// RACF return code.
    pub racf_rc: RacfRc,
}

impl RacrouteResult {
    /// Whether the request indicates successful authorization.
    pub fn is_authorized(&self) -> bool {
        self.saf_rc == SafRc::Success && self.racf_rc == RacfRc::Authorized
    }
}

/// Result from a RACROUTE REQUEST=EXTRACT call.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExtractResult {
    /// SAF return code.
    pub saf_rc: SafRc,
    /// RACF return code.
    pub racf_rc: RacfRc,
    /// Extracted field values (field name → value).
    pub fields: Vec<(String, String)>,
}

// ─────────────────────── SAF Router ───────────────────────

/// The SAF router — dispatches RACROUTE requests to the RACF database.
///
/// In a real z/OS system, the SAF router is a system service that forwards
/// security requests to the installed security product. This implementation
/// directly calls into `RacfDatabase`.
pub struct SafRouter {
    /// Whether RACF is active (can be deactivated for testing).
    active: bool,
}

impl SafRouter {
    /// Create a new SAF router in active state.
    pub fn new() -> Self {
        Self { active: true }
    }

    /// Set RACF active/inactive.
    pub fn set_active(&mut self, active: bool) {
        self.active = active;
    }

    /// Whether RACF is currently active.
    pub fn is_active(&self) -> bool {
        self.active
    }

    /// RACROUTE REQUEST=AUTH — authorization check.
    ///
    /// Check if `userid` has `access` to `entity` in the given `class`.
    /// Supports class "DATASET" (dispatched to dataset profile checking) and
    /// all general resource classes via the `ResourceManager`.
    pub fn auth(
        &self,
        db: &RacfDatabase,
        class: &str,
        entity: &str,
        userid: &str,
        access: AccessLevel,
    ) -> RacrouteResult {
        if !self.active {
            info!("SAF: RACF not active, returning RC=4");
            return RacrouteResult {
                saf_rc: SafRc::NotActive,
                racf_rc: RacfRc::Authorized,
            };
        }

        let class = class.to_uppercase();
        debug!("SAF AUTH: class={} entity={} user={} access={}", class, entity, userid, access);

        match class.as_str() {
            "DATASET" => self.auth_dataset(db, entity, userid, access),
            _ => {
                // General resource classes — dispatch to ResourceManager if available.
                debug!("SAF AUTH: general resource class '{}', returning no-profile (use auth_resource for general resources)", class);
                RacrouteResult {
                    saf_rc: SafRc::Failed,
                    racf_rc: RacfRc::NoProfile,
                }
            }
        }
    }

    /// RACROUTE REQUEST=AUTH for general resource classes.
    ///
    /// Check if `userid` has `access` to `entity` in the given general resource
    /// `class` using the ResourceManager.
    pub fn auth_resource(
        &self,
        resource_mgr: &ResourceManager,
        class: &str,
        entity: &str,
        userid: &str,
        access: AccessLevel,
    ) -> RacrouteResult {
        if !self.active {
            return RacrouteResult {
                saf_rc: SafRc::NotActive,
                racf_rc: RacfRc::Authorized,
            };
        }

        let result = resource_mgr.check_access(class, entity, userid, access);

        match result.reason {
            AuthReason::Authorized => RacrouteResult {
                saf_rc: SafRc::Success,
                racf_rc: RacfRc::Authorized,
            },
            AuthReason::InsufficientAccess | AuthReason::ClassNotActive => RacrouteResult {
                saf_rc: SafRc::Failed,
                racf_rc: RacfRc::NotAuthorized,
            },
            AuthReason::NoProfile => RacrouteResult {
                saf_rc: SafRc::Failed,
                racf_rc: RacfRc::NoProfile,
            },
        }
    }

    /// RACROUTE REQUEST=FASTAUTH — fast authorization check against in-storage profiles.
    ///
    /// Functionally equivalent to AUTH but intended for RACLIST'd profiles that are
    /// cached in memory. In this implementation, it delegates to the same path.
    pub fn fastauth(
        &self,
        db: &RacfDatabase,
        class: &str,
        entity: &str,
        userid: &str,
        access: AccessLevel,
    ) -> RacrouteResult {
        if !self.active {
            return RacrouteResult {
                saf_rc: SafRc::NotActive,
                racf_rc: RacfRc::Authorized,
            };
        }

        debug!("SAF FASTAUTH: class={} entity={} user={}", class, entity, userid);
        // In this implementation FASTAUTH uses the same path as AUTH.
        // A production system would use pre-built in-storage profile tables.
        self.auth(db, class, entity, userid, access)
    }

    /// RACROUTE REQUEST=VERIFY — user authentication.
    ///
    /// Validates that the given password matches the user's stored password hash.
    pub fn verify(
        &self,
        db: &RacfDatabase,
        userid: &str,
        password: &str,
    ) -> RacrouteResult {
        if !self.active {
            return RacrouteResult {
                saf_rc: SafRc::NotActive,
                racf_rc: RacfRc::Authorized,
            };
        }

        let userid_upper = userid.trim().to_uppercase();
        debug!("SAF VERIFY: userid={}", userid_upper);

        let Some(user) = db.get_user(&userid_upper) else {
            return RacrouteResult {
                saf_rc: SafRc::Failed,
                racf_rc: RacfRc::NoProfile,
            };
        };

        // Check if user is revoked.
        if user.is_revoked() {
            info!("SAF VERIFY: user {} is revoked", userid_upper);
            return RacrouteResult {
                saf_rc: SafRc::Failed,
                racf_rc: RacfRc::NotAuthorized,
            };
        }

        // Verify password (simplified — compares against stored hash directly).
        match &user.password_hash {
            Some(hash) if hash == password => {
                info!("SAF VERIFY: user {} authenticated", userid_upper);
                RacrouteResult {
                    saf_rc: SafRc::Success,
                    racf_rc: RacfRc::Authorized,
                }
            }
            Some(_) => {
                info!("SAF VERIFY: user {} password mismatch", userid_upper);
                RacrouteResult {
                    saf_rc: SafRc::Failed,
                    racf_rc: RacfRc::NotAuthorized,
                }
            }
            None => {
                // No password set — treat as not authorized.
                info!("SAF VERIFY: user {} has no password set", userid_upper);
                RacrouteResult {
                    saf_rc: SafRc::Failed,
                    racf_rc: RacfRc::NotAuthorized,
                }
            }
        }
    }

    /// RACROUTE REQUEST=EXTRACT — extract profile field values.
    ///
    /// Retrieves specific fields from a user or group profile.
    /// Supported classes: "USER", "GROUP".
    /// Supported fields for USER: NAME, DFLTGRP, OWNER.
    /// Supported fields for GROUP: OWNER, SUPGRP.
    pub fn extract(
        &self,
        db: &RacfDatabase,
        class: &str,
        entity: &str,
        fields: &[&str],
    ) -> ExtractResult {
        if !self.active {
            return ExtractResult {
                saf_rc: SafRc::NotActive,
                racf_rc: RacfRc::Authorized,
                fields: Vec::new(),
            };
        }

        let class = class.to_uppercase();
        let entity_upper = entity.trim().to_uppercase();
        debug!("SAF EXTRACT: class={} entity={} fields={:?}", class, entity_upper, fields);

        match class.as_str() {
            "USER" => self.extract_user(db, &entity_upper, fields),
            "GROUP" => self.extract_group(db, &entity_upper, fields),
            _ => ExtractResult {
                saf_rc: SafRc::Failed,
                racf_rc: RacfRc::NoProfile,
                fields: Vec::new(),
            },
        }
    }

    // ─────── Internal dispatch ───────

    fn auth_dataset(
        &self,
        db: &RacfDatabase,
        entity: &str,
        userid: &str,
        access: AccessLevel,
    ) -> RacrouteResult {
        match db.check_dataset_access(entity, userid, access) {
            Ok(result) => {
                if result.granted {
                    RacrouteResult {
                        saf_rc: SafRc::Success,
                        racf_rc: RacfRc::Authorized,
                    }
                } else {
                    RacrouteResult {
                        saf_rc: SafRc::Failed,
                        racf_rc: RacfRc::NotAuthorized,
                    }
                }
            }
            Err(_) => {
                // No covering profile found.
                RacrouteResult {
                    saf_rc: SafRc::Failed,
                    racf_rc: RacfRc::NoProfile,
                }
            }
        }
    }

    fn extract_user(
        &self,
        db: &RacfDatabase,
        entity: &str,
        fields: &[&str],
    ) -> ExtractResult {
        let Some(user) = db.get_user(entity) else {
            return ExtractResult {
                saf_rc: SafRc::Failed,
                racf_rc: RacfRc::NoProfile,
                fields: Vec::new(),
            };
        };

        let extracted: Vec<(String, String)> = fields
            .iter()
            .filter_map(|f| {
                let f_upper = f.to_uppercase();
                match f_upper.as_str() {
                    "NAME" => Some((f_upper, user.name.clone())),
                    "DFLTGRP" => Some((f_upper, user.default_group.clone())),
                    "OWNER" => Some((f_upper, user.owner.clone())),
                    _ => None,
                }
            })
            .collect();

        ExtractResult {
            saf_rc: SafRc::Success,
            racf_rc: RacfRc::Authorized,
            fields: extracted,
        }
    }

    fn extract_group(
        &self,
        db: &RacfDatabase,
        entity: &str,
        fields: &[&str],
    ) -> ExtractResult {
        let Some(group) = db.get_group(entity) else {
            return ExtractResult {
                saf_rc: SafRc::Failed,
                racf_rc: RacfRc::NoProfile,
                fields: Vec::new(),
            };
        };

        let extracted: Vec<(String, String)> = fields
            .iter()
            .filter_map(|f| {
                let f_upper = f.to_uppercase();
                match f_upper.as_str() {
                    "OWNER" => Some((f_upper, group.owner.clone())),
                    "SUPGRP" => Some((
                        f_upper,
                        group.superior_group.clone().unwrap_or_default(),
                    )),
                    _ => None,
                }
            })
            .collect();

        ExtractResult {
            saf_rc: SafRc::Success,
            racf_rc: RacfRc::Authorized,
            fields: extracted,
        }
    }
}

impl Default for SafRouter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{AccessLevel, UserAttribute};

    fn setup_db() -> RacfDatabase {
        let mut db = RacfDatabase::new();
        db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();
        db.add_user("JSMITH", "DEPT01", "John Smith", "ADMIN1").unwrap();
        db.add_dataset("PROD.PAYROLL.**", AccessLevel::None, "ADMIN1").unwrap();
        db.permit_dataset("PROD.PAYROLL.**", "JSMITH", AccessLevel::Read).unwrap();

        // Set a password for JSMITH.
        let user = db.get_user_mut("JSMITH").unwrap();
        user.password_hash = Some("SECRET123".to_string());

        db
    }

    // ─────── Story S104.1: SAF Router and RACROUTE REQUEST=AUTH ───────

    #[test]
    fn test_auth_granted() {
        let db = setup_db();
        let saf = SafRouter::new();

        let result = saf.auth(&db, "DATASET", "PROD.PAYROLL.DATA", "JSMITH", AccessLevel::Read);
        assert!(result.is_authorized());
        assert_eq!(result.saf_rc, SafRc::Success);
        assert_eq!(result.racf_rc, RacfRc::Authorized);
    }

    #[test]
    fn test_auth_denied() {
        let db = setup_db();
        let saf = SafRouter::new();

        let result = saf.auth(&db, "DATASET", "PROD.PAYROLL.DATA", "JSMITH", AccessLevel::Update);
        assert!(!result.is_authorized());
        assert_eq!(result.saf_rc, SafRc::Failed);
        assert_eq!(result.racf_rc, RacfRc::NotAuthorized);
    }

    #[test]
    fn test_auth_no_profile() {
        let db = setup_db();
        let saf = SafRouter::new();

        let result = saf.auth(&db, "DATASET", "UNPROTECTED.DATA", "JSMITH", AccessLevel::Read);
        assert_eq!(result.saf_rc, SafRc::Failed);
        assert_eq!(result.racf_rc, RacfRc::NoProfile);
    }

    #[test]
    fn test_auth_racf_not_active() {
        let db = setup_db();
        let mut saf = SafRouter::new();
        saf.set_active(false);

        let result = saf.auth(&db, "DATASET", "PROD.PAYROLL.DATA", "JSMITH", AccessLevel::Read);
        assert_eq!(result.saf_rc, SafRc::NotActive);
    }

    #[test]
    fn test_auth_unsupported_class() {
        let db = setup_db();
        let saf = SafRouter::new();

        let result = saf.auth(&db, "CICSTRN", "TRXN01", "JSMITH", AccessLevel::Read);
        assert_eq!(result.saf_rc, SafRc::Failed);
        assert_eq!(result.racf_rc, RacfRc::NoProfile);
    }

    // ─────── S102: SAF + General Resources integration ───────

    #[test]
    fn test_auth_resource_granted() {
        let saf = SafRouter::new();
        let mut rm = crate::resource::ResourceManager::new();
        rm.activate_class("FACILITY");
        rm.rdefine("FACILITY", "IRR.RADMIN", AccessLevel::None, "SYS1").unwrap();
        rm.permit("FACILITY", "IRR.RADMIN", "JSMITH", AccessLevel::Read).unwrap();

        let result = saf.auth_resource(&rm, "FACILITY", "IRR.RADMIN", "JSMITH", AccessLevel::Read);
        assert!(result.is_authorized());
    }

    #[test]
    fn test_auth_resource_denied() {
        let saf = SafRouter::new();
        let mut rm = crate::resource::ResourceManager::new();
        rm.activate_class("FACILITY");
        rm.rdefine("FACILITY", "IRR.RADMIN", AccessLevel::None, "SYS1").unwrap();

        let result = saf.auth_resource(&rm, "FACILITY", "IRR.RADMIN", "JSMITH", AccessLevel::Read);
        assert!(!result.is_authorized());
        assert_eq!(result.racf_rc, RacfRc::NotAuthorized);
    }

    #[test]
    fn test_auth_resource_no_profile() {
        let saf = SafRouter::new();
        let mut rm = crate::resource::ResourceManager::new();
        rm.activate_class("FACILITY");

        let result = saf.auth_resource(&rm, "FACILITY", "NOEXIST", "JSMITH", AccessLevel::Read);
        assert_eq!(result.racf_rc, RacfRc::NoProfile);
    }

    #[test]
    fn test_auth_resource_not_active() {
        let mut saf = SafRouter::new();
        saf.set_active(false);
        let rm = crate::resource::ResourceManager::new();

        let result = saf.auth_resource(&rm, "FACILITY", "ANYTHING", "JSMITH", AccessLevel::Read);
        assert_eq!(result.saf_rc, SafRc::NotActive);
    }

    #[test]
    fn test_fastauth_same_as_auth() {
        let db = setup_db();
        let saf = SafRouter::new();

        let auth_result = saf.auth(&db, "DATASET", "PROD.PAYROLL.DATA", "JSMITH", AccessLevel::Read);
        let fast_result = saf.fastauth(&db, "DATASET", "PROD.PAYROLL.DATA", "JSMITH", AccessLevel::Read);
        assert_eq!(auth_result, fast_result);
    }

    // ─────── Story S104.2: RACROUTE REQUEST=VERIFY and EXTRACT ───────

    #[test]
    fn test_verify_valid_password() {
        let db = setup_db();
        let saf = SafRouter::new();

        let result = saf.verify(&db, "JSMITH", "SECRET123");
        assert!(result.is_authorized());
    }

    #[test]
    fn test_verify_invalid_password() {
        let db = setup_db();
        let saf = SafRouter::new();

        let result = saf.verify(&db, "JSMITH", "WRONGPASS");
        assert!(!result.is_authorized());
        assert_eq!(result.racf_rc, RacfRc::NotAuthorized);
    }

    #[test]
    fn test_verify_user_not_found() {
        let db = setup_db();
        let saf = SafRouter::new();

        let result = saf.verify(&db, "NOBODY", "SECRET123");
        assert_eq!(result.racf_rc, RacfRc::NoProfile);
    }

    #[test]
    fn test_verify_revoked_user() {
        let mut db = setup_db();
        db.alter_user("JSMITH", None, None, None, &[UserAttribute::Revoke], &[]).unwrap();
        let saf = SafRouter::new();

        let result = saf.verify(&db, "JSMITH", "SECRET123");
        assert!(!result.is_authorized());
        assert_eq!(result.racf_rc, RacfRc::NotAuthorized);
    }

    #[test]
    fn test_verify_no_password_set() {
        let mut db = RacfDatabase::new();
        db.add_group("GRP", "SYS1", "ADMIN").unwrap();
        db.add_user("NOPASS", "GRP", "No Password", "ADMIN").unwrap();
        let saf = SafRouter::new();

        let result = saf.verify(&db, "NOPASS", "anything");
        assert!(!result.is_authorized());
    }

    #[test]
    fn test_extract_user_fields() {
        let db = setup_db();
        let saf = SafRouter::new();

        let result = saf.extract(&db, "USER", "JSMITH", &["NAME", "DFLTGRP", "OWNER"]);
        assert_eq!(result.saf_rc, SafRc::Success);
        assert_eq!(result.fields.len(), 3);
        assert!(result.fields.contains(&("NAME".to_string(), "John Smith".to_string())));
        assert!(result.fields.contains(&("DFLTGRP".to_string(), "DEPT01".to_string())));
        assert!(result.fields.contains(&("OWNER".to_string(), "ADMIN1".to_string())));
    }

    #[test]
    fn test_extract_user_not_found() {
        let db = setup_db();
        let saf = SafRouter::new();

        let result = saf.extract(&db, "USER", "NOBODY", &["NAME"]);
        assert_eq!(result.saf_rc, SafRc::Failed);
        assert_eq!(result.racf_rc, RacfRc::NoProfile);
    }

    #[test]
    fn test_extract_group_fields() {
        let db = setup_db();
        let saf = SafRouter::new();

        let result = saf.extract(&db, "GROUP", "DEPT01", &["OWNER", "SUPGRP"]);
        assert_eq!(result.saf_rc, SafRc::Success);
        assert!(result.fields.contains(&("OWNER".to_string(), "ADMIN1".to_string())));
        assert!(result.fields.contains(&("SUPGRP".to_string(), "SYS1".to_string())));
    }

    #[test]
    fn test_extract_unknown_field_skipped() {
        let db = setup_db();
        let saf = SafRouter::new();

        let result = saf.extract(&db, "USER", "JSMITH", &["NAME", "NOSUCHFIELD"]);
        assert_eq!(result.fields.len(), 1);
        assert!(result.fields.contains(&("NAME".to_string(), "John Smith".to_string())));
    }

    #[test]
    fn test_extract_racf_not_active() {
        let db = setup_db();
        let mut saf = SafRouter::new();
        saf.set_active(false);

        let result = saf.extract(&db, "USER", "JSMITH", &["NAME"]);
        assert_eq!(result.saf_rc, SafRc::NotActive);
        assert!(result.fields.is_empty());
    }
}
