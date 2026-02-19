//! General Resource Framework.
//!
//! Implements RDEFINE/RALTER/RLIST/RDELETE commands and PERMIT for
//! general resource classes (FACILITY, PROGRAM, OPERCMDS, STARTED, etc.).
//!
//! On z/OS, general resources are profiles in named classes that protect
//! non-dataset entities — transactions, commands, programs, facilities, etc.

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};
use tracing::{debug, info};

use crate::error::RacfError;
use crate::types::AccessLevel;

// ---------------------------------------------------------------------------
//  Resource profile
// ---------------------------------------------------------------------------

/// A general resource profile.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceProfile {
    /// Resource class name (e.g. "FACILITY").
    pub class: String,
    /// Profile name/pattern within the class.
    pub name: String,
    /// Universal access level.
    pub uacc: AccessLevel,
    /// Profile owner.
    pub owner: String,
    /// Access list: userid/group → access level.
    pub access_list: BTreeMap<String, AccessLevel>,
    /// Whether this is a generic profile (contains wildcards).
    pub generic: bool,
    /// Conditional access entries.
    pub conditional_access: Vec<ConditionalAccess>,
    /// Installation data (free text, up to 255 chars).
    pub installation_data: String,
}

impl ResourceProfile {
    /// Create a new resource profile.
    pub fn new(class: &str, name: &str, uacc: AccessLevel, owner: &str) -> Self {
        let name_upper = name.trim().to_uppercase();
        let generic = name_upper.contains('*') || name_upper.contains('%');
        Self {
            class: class.trim().to_uppercase(),
            name: name_upper,
            uacc,
            owner: owner.trim().to_uppercase(),
            access_list: BTreeMap::new(),
            generic,
            conditional_access: Vec::new(),
            installation_data: String::new(),
        }
    }

    /// Determine effective access for a userid.
    ///
    /// Returns the access-list entry if present, otherwise UACC.
    pub fn effective_access(&self, userid: &str) -> AccessLevel {
        let upper = userid.trim().to_uppercase();
        self.access_list
            .get(&upper)
            .copied()
            .unwrap_or(self.uacc)
    }
}

// ---------------------------------------------------------------------------
//  Conditional access
// ---------------------------------------------------------------------------

/// Type of condition for conditional access.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum WhenCondition {
    /// WHEN(PROGRAM(name)) — condition on executing program.
    Program(String),
    /// WHEN(TERMINAL(name)) — condition on terminal ID.
    Terminal(String),
    /// WHEN(CONSOLE(name)) — condition on operator console.
    Console(String),
    /// WHEN(JESINPUT) — condition on JES input.
    JesInput,
    /// WHEN(SERVAUTH(name)) — condition on server authentication.
    ServAuth(String),
    /// WHEN(SYSID(name)) — condition on system ID.
    SysId(String),
    /// WHEN(APPCPORT(name)) — condition on APPC port.
    AppcPort(String),
}

/// A conditional access entry — grants access only when a condition is met.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConditionalAccess {
    /// The userid or group this applies to.
    pub id: String,
    /// The access level granted.
    pub access: AccessLevel,
    /// The condition that must be met.
    pub condition: WhenCondition,
}

// ---------------------------------------------------------------------------
//  Resource class descriptor
// ---------------------------------------------------------------------------

/// A resource class definition (CDT — Class Descriptor Table entry).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClassDescriptor {
    /// Class name (1-8 chars).
    pub name: String,
    /// Maximum profile name length.
    pub max_length: usize,
    /// Whether generic profiles are allowed.
    pub generic_allowed: bool,
    /// Default UACC for profiles in this class.
    pub default_uacc: AccessLevel,
    /// Whether the class is currently active.
    pub active: bool,
    /// Whether the class is RACLISTed (in-storage profiles).
    pub raclisted: bool,
}

impl ClassDescriptor {
    /// Create a new class descriptor.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.trim().to_uppercase(),
            max_length: 246,
            generic_allowed: true,
            default_uacc: AccessLevel::None,
            active: false,
            raclisted: false,
        }
    }
}

// ---------------------------------------------------------------------------
//  IBM-supplied resource classes
// ---------------------------------------------------------------------------

/// Well-known IBM resource class names.
pub struct IbmClasses;

impl IbmClasses {
    /// General system facility resources (IRR.*, BPX.*, STGADMIN.*).
    pub const FACILITY: &'static str = "FACILITY";
    /// Program access control.
    pub const PROGRAM: &'static str = "PROGRAM";
    /// Operator commands.
    pub const OPERCMDS: &'static str = "OPERCMDS";
    /// Started task identities.
    pub const STARTED: &'static str = "STARTED";
    /// Surrogate user submission.
    pub const SURROGAT: &'static str = "SURROGAT";
    /// JES spool access.
    pub const JESSPOOL: &'static str = "JESSPOOL";
    /// JES job-level access.
    pub const JESJOBS: &'static str = "JESJOBS";
    /// JES input.
    pub const JESINPUT: &'static str = "JESINPUT";
    /// JES output writers.
    pub const WRITER: &'static str = "WRITER";
    /// CICS transactions.
    pub const CICSTRN: &'static str = "CICSTRN";
    /// CICS group transactions.
    pub const GCICSTRN: &'static str = "GCICSTRN";
    /// CICS terminal transactions.
    pub const TCICSTRN: &'static str = "TCICSTRN";
    /// DB2 resources.
    pub const DSNR: &'static str = "DSNR";
    /// MQ connection.
    pub const MQCONN: &'static str = "MQCONN";
    /// MQ administration.
    pub const MQADMIN: &'static str = "MQADMIN";
    /// MQ queue access.
    pub const MQQUEUE: &'static str = "MQQUEUE";
    /// MQ process.
    pub const MQPROC: &'static str = "MQPROC";
    /// MQ commands.
    pub const MQCMDS: &'static str = "MQCMDS";
    /// Cryptography keys.
    pub const CSFKEYS: &'static str = "CSFKEYS";
    /// Cryptography group keys.
    pub const GCSFKEYS: &'static str = "GCSFKEYS";
    /// Cryptographic services.
    pub const CSFSERV: &'static str = "CSFSERV";
    /// Network server authentication.
    pub const SERVAUTH: &'static str = "SERVAUTH";
    /// Application (APPL).
    pub const APPL: &'static str = "APPL";
    /// Terminal access.
    pub const TERMINAL: &'static str = "TERMINAL";
    /// Console access.
    pub const CONSOLE: &'static str = "CONSOLE";
    /// Network nodes.
    pub const NODES: &'static str = "NODES";
    /// Security labels (MAC).
    pub const SECLABEL: &'static str = "SECLABEL";
    /// Security levels (MAC).
    pub const SECLEVEL: &'static str = "SECLEVEL";
    /// Unix privileges.
    pub const UNIXPRIV: &'static str = "UNIXPRIV";
    /// File-system security.
    pub const FSSEC: &'static str = "FSSEC";
    /// Digital certificates.
    pub const DIGTCERT: &'static str = "DIGTCERT";
    /// Digital certificate keyrings.
    pub const DIGTRING: &'static str = "DIGTRING";
    /// R data library.
    pub const RDATALIB: &'static str = "RDATALIB";
    /// PassTicket data.
    pub const PTKTDATA: &'static str = "PTKTDATA";
    /// PassTicket validation.
    pub const PTKTVAL: &'static str = "PTKTVAL";
    /// Global classes.
    pub const GLOBAL: &'static str = "GLOBAL";
    /// RACF variables.
    pub const RACFVARS: &'static str = "RACFVARS";
    /// Log streams.
    pub const LOGSTRM: &'static str = "LOGSTRM";
    /// DASD volumes.
    pub const DASDVOL: &'static str = "DASDVOL";
    /// Tape volumes.
    pub const TAPEVOL: &'static str = "TAPEVOL";
    /// Temporary datasets.
    pub const TEMPDSN: &'static str = "TEMPDSN";
    /// TSO authorisation.
    pub const TSOAUTH: &'static str = "TSOAUTH";
    /// TSO procedures.
    pub const TSOPROC: &'static str = "TSOPROC";
    /// SDSF (System Display & Search Facility).
    pub const SDSF: &'static str = "SDSF";

    /// All IBM-supplied class names.
    pub fn all() -> &'static [&'static str] {
        &[
            Self::FACILITY, Self::PROGRAM, Self::OPERCMDS, Self::STARTED,
            Self::SURROGAT, Self::JESSPOOL, Self::JESJOBS, Self::JESINPUT,
            Self::WRITER, Self::CICSTRN, Self::GCICSTRN, Self::TCICSTRN,
            Self::DSNR, Self::MQCONN, Self::MQADMIN, Self::MQQUEUE,
            Self::MQPROC, Self::MQCMDS, Self::CSFKEYS, Self::GCSFKEYS,
            Self::CSFSERV, Self::SERVAUTH, Self::APPL, Self::TERMINAL,
            Self::CONSOLE, Self::NODES, Self::SECLABEL, Self::SECLEVEL,
            Self::UNIXPRIV, Self::FSSEC, Self::DIGTCERT, Self::DIGTRING,
            Self::RDATALIB, Self::PTKTDATA, Self::PTKTVAL, Self::GLOBAL,
            Self::RACFVARS, Self::LOGSTRM, Self::DASDVOL, Self::TAPEVOL,
            Self::TEMPDSN, Self::TSOAUTH, Self::TSOPROC, Self::SDSF,
        ]
    }
}

// ---------------------------------------------------------------------------
//  Resource manager
// ---------------------------------------------------------------------------

/// Result from RLIST.
#[derive(Debug, Clone)]
pub struct ListResourceResult {
    /// The resource profile.
    pub profile: ResourceProfile,
}

/// The General Resource Manager — stores profiles for all non-dataset resource classes.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ResourceManager {
    /// Class descriptors (CDT) keyed by class name.
    classes: BTreeMap<String, ClassDescriptor>,
    /// Resource profiles keyed by (class, profile_name).
    profiles: BTreeMap<(String, String), ResourceProfile>,
}

impl ResourceManager {
    /// Create a new resource manager with IBM-supplied class definitions.
    pub fn new() -> Self {
        let mut mgr = Self {
            classes: BTreeMap::new(),
            profiles: BTreeMap::new(),
        };
        // Pre-define all IBM classes (inactive by default).
        for &class_name in IbmClasses::all() {
            mgr.classes
                .insert(class_name.to_string(), ClassDescriptor::new(class_name));
        }
        mgr
    }

    // ─────── Class management ───────

    /// Activate a resource class (SETROPTS CLASSACT).
    pub fn activate_class(&mut self, class: &str) -> bool {
        let class = class.trim().to_uppercase();
        if let Some(cd) = self.classes.get_mut(&class) {
            cd.active = true;
            info!("SETROPTS CLASSACT({})", class);
            true
        } else {
            false
        }
    }

    /// Deactivate a resource class.
    pub fn deactivate_class(&mut self, class: &str) -> bool {
        let class = class.trim().to_uppercase();
        if let Some(cd) = self.classes.get_mut(&class) {
            cd.active = false;
            info!("SETROPTS NOCLASSACT({})", class);
            true
        } else {
            false
        }
    }

    /// RACLIST a class (enable in-storage profiles).
    pub fn raclist_class(&mut self, class: &str) -> bool {
        let class = class.trim().to_uppercase();
        if let Some(cd) = self.classes.get_mut(&class) {
            cd.raclisted = true;
            info!("SETROPTS RACLIST({})", class);
            true
        } else {
            false
        }
    }

    /// Check if a class is active.
    pub fn is_class_active(&self, class: &str) -> bool {
        let class = class.trim().to_uppercase();
        self.classes.get(&class).map_or(false, |cd| cd.active)
    }

    /// Check if a class is RACLISTed.
    pub fn is_class_raclisted(&self, class: &str) -> bool {
        let class = class.trim().to_uppercase();
        self.classes.get(&class).map_or(false, |cd| cd.raclisted)
    }

    /// Get a class descriptor.
    pub fn get_class(&self, class: &str) -> Option<&ClassDescriptor> {
        self.classes.get(&class.trim().to_uppercase())
    }

    /// Define a custom class (CDT entry).
    pub fn define_class(&mut self, descriptor: ClassDescriptor) {
        info!("CDT: defined class {}", descriptor.name);
        self.classes.insert(descriptor.name.clone(), descriptor);
    }

    /// List all defined class names.
    pub fn list_classes(&self) -> Vec<String> {
        self.classes.keys().cloned().collect()
    }

    /// List all active class names.
    pub fn list_active_classes(&self) -> Vec<String> {
        self.classes
            .iter()
            .filter(|(_, cd)| cd.active)
            .map(|(name, _)| name.clone())
            .collect()
    }

    // ─────── Profile commands ───────

    /// RDEFINE — define a new general resource profile.
    pub fn rdefine(
        &mut self,
        class: &str,
        profile_name: &str,
        uacc: AccessLevel,
        owner: &str,
    ) -> Result<(), RacfError> {
        let class = class.trim().to_uppercase();
        let profile_name = profile_name.trim().to_uppercase();

        // Class must be known.
        if !self.classes.contains_key(&class) {
            return Err(RacfError::ResourceClassNotFound {
                class: class.clone(),
            });
        }

        let key = (class.clone(), profile_name.clone());
        if self.profiles.contains_key(&key) {
            return Err(RacfError::ResourceProfileExists {
                class: class.clone(),
                name: profile_name,
            });
        }

        let profile = ResourceProfile::new(&class, &profile_name, uacc, owner);
        info!("RDEFINE {} '{}' UACC({})", class, profile_name, uacc);
        self.profiles.insert(key, profile);
        Ok(())
    }

    /// RALTER — alter an existing general resource profile.
    pub fn ralter(
        &mut self,
        class: &str,
        profile_name: &str,
        uacc: Option<AccessLevel>,
        owner: Option<&str>,
        installation_data: Option<&str>,
    ) -> Result<(), RacfError> {
        let class = class.trim().to_uppercase();
        let profile_name = profile_name.trim().to_uppercase();
        let key = (class.clone(), profile_name.clone());

        let profile = self.profiles.get_mut(&key).ok_or_else(|| {
            RacfError::ResourceProfileNotFound {
                class: class.clone(),
                name: profile_name.clone(),
            }
        })?;

        if let Some(u) = uacc {
            profile.uacc = u;
        }
        if let Some(o) = owner {
            profile.owner = o.trim().to_uppercase();
        }
        if let Some(data) = installation_data {
            profile.installation_data = data.to_string();
        }

        info!("RALTER {} '{}'", class, profile_name);
        Ok(())
    }

    /// RLIST — retrieve a general resource profile.
    pub fn rlist(
        &self,
        class: &str,
        profile_name: &str,
    ) -> Result<ListResourceResult, RacfError> {
        let class = class.trim().to_uppercase();
        let profile_name = profile_name.trim().to_uppercase();
        let key = (class.clone(), profile_name.clone());

        let profile = self.profiles.get(&key).ok_or(
            RacfError::ResourceProfileNotFound {
                class,
                name: profile_name,
            }
        )?;

        Ok(ListResourceResult {
            profile: profile.clone(),
        })
    }

    /// RDELETE — delete a general resource profile.
    pub fn rdelete(
        &mut self,
        class: &str,
        profile_name: &str,
    ) -> Result<(), RacfError> {
        let class = class.trim().to_uppercase();
        let profile_name = profile_name.trim().to_uppercase();
        let key = (class.clone(), profile_name.clone());

        self.profiles.remove(&key).ok_or_else(|| {
            RacfError::ResourceProfileNotFound {
                class: class.clone(),
                name: profile_name.clone(),
            }
        })?;

        info!("RDELETE {} '{}'", class, profile_name);
        Ok(())
    }

    /// PERMIT — add or update an access-list entry on a general resource profile.
    pub fn permit(
        &mut self,
        class: &str,
        profile_name: &str,
        id: &str,
        access: AccessLevel,
    ) -> Result<(), RacfError> {
        let class = class.trim().to_uppercase();
        let profile_name = profile_name.trim().to_uppercase();
        let id = id.trim().to_uppercase();
        let key = (class.clone(), profile_name.clone());

        let profile = self.profiles.get_mut(&key).ok_or_else(|| {
            RacfError::ResourceProfileNotFound {
                class: class.clone(),
                name: profile_name.clone(),
            }
        })?;

        profile.access_list.insert(id.clone(), access);
        info!(
            "PERMIT '{}' CLASS({}) ID({}) ACCESS({})",
            profile_name, class, id, access
        );
        Ok(())
    }

    /// PERMIT with WHEN — add a conditional access entry.
    pub fn permit_when(
        &mut self,
        class: &str,
        profile_name: &str,
        id: &str,
        access: AccessLevel,
        condition: WhenCondition,
    ) -> Result<(), RacfError> {
        let class = class.trim().to_uppercase();
        let profile_name = profile_name.trim().to_uppercase();
        let id = id.trim().to_uppercase();
        let key = (class.clone(), profile_name.clone());

        let profile = self.profiles.get_mut(&key).ok_or_else(|| {
            RacfError::ResourceProfileNotFound {
                class: class.clone(),
                name: profile_name.clone(),
            }
        })?;

        profile.conditional_access.push(ConditionalAccess {
            id,
            access,
            condition,
        });

        Ok(())
    }

    // ─────── Authorization check ───────

    /// Check resource access for a userid.
    ///
    /// Finds the best-matching profile in the class and checks the access list.
    pub fn check_access(
        &self,
        class: &str,
        resource: &str,
        userid: &str,
        requested: AccessLevel,
    ) -> ResourceAuthResult {
        let class = class.trim().to_uppercase();
        let resource = resource.trim().to_uppercase();
        let userid = userid.trim().to_uppercase();

        debug!(
            "RESOURCE AUTH: class={} resource={} user={} access={}",
            class, resource, userid, requested
        );

        // Check if class is active.
        if !self.is_class_active(&class) {
            return ResourceAuthResult {
                granted: false,
                access: AccessLevel::None,
                profile_name: None,
                reason: AuthReason::ClassNotActive,
            };
        }

        // 1. Try exact (discrete) profile match.
        let exact_key = (class.clone(), resource.clone());
        if let Some(profile) = self.profiles.get(&exact_key) {
            if !profile.generic {
                let access = profile.effective_access(&userid);
                return ResourceAuthResult {
                    granted: access >= requested,
                    access,
                    profile_name: Some(profile.name.clone()),
                    reason: if access >= requested {
                        AuthReason::Authorized
                    } else {
                        AuthReason::InsufficientAccess
                    },
                };
            }
        }

        // 2. Find best-matching generic profile.
        let mut best: Option<(&ResourceProfile, u32)> = None;
        for ((c, _), profile) in &self.profiles {
            if c != &class {
                continue;
            }
            if !profile.generic {
                if profile.name == resource {
                    let access = profile.effective_access(&userid);
                    return ResourceAuthResult {
                        granted: access >= requested,
                        access,
                        profile_name: Some(profile.name.clone()),
                        reason: if access >= requested {
                            AuthReason::Authorized
                        } else {
                            AuthReason::InsufficientAccess
                        },
                    };
                }
                continue;
            }
            if resource_matches(&resource, &profile.name) {
                let spec = pattern_specificity(&profile.name);
                if best.as_ref().map_or(true, |(_, s)| spec > *s) {
                    best = Some((profile, spec));
                }
            }
        }

        match best {
            Some((profile, _)) => {
                let access = profile.effective_access(&userid);
                ResourceAuthResult {
                    granted: access >= requested,
                    access,
                    profile_name: Some(profile.name.clone()),
                    reason: if access >= requested {
                        AuthReason::Authorized
                    } else {
                        AuthReason::InsufficientAccess
                    },
                }
            }
            None => ResourceAuthResult {
                granted: false,
                access: AccessLevel::None,
                profile_name: None,
                reason: AuthReason::NoProfile,
            },
        }
    }

    /// Check resource access with a WHEN condition.
    pub fn check_access_conditional(
        &self,
        class: &str,
        resource: &str,
        userid: &str,
        requested: AccessLevel,
        condition: &WhenCondition,
    ) -> ResourceAuthResult {
        let class_upper = class.trim().to_uppercase();
        let resource_upper = resource.trim().to_uppercase();
        let userid_upper = userid.trim().to_uppercase();

        if !self.is_class_active(&class_upper) {
            return ResourceAuthResult {
                granted: false,
                access: AccessLevel::None,
                profile_name: None,
                reason: AuthReason::ClassNotActive,
            };
        }

        // Find matching profile(s) and check conditional access list.
        for ((c, _), profile) in &self.profiles {
            if c != &class_upper {
                continue;
            }
            let matches = if profile.generic {
                resource_matches(&resource_upper, &profile.name)
            } else {
                profile.name == resource_upper
            };
            if !matches {
                continue;
            }

            // Check conditional access entries.
            for ca in &profile.conditional_access {
                if ca.id == userid_upper && ca.condition == *condition && ca.access >= requested {
                    return ResourceAuthResult {
                        granted: true,
                        access: ca.access,
                        profile_name: Some(profile.name.clone()),
                        reason: AuthReason::Authorized,
                    };
                }
            }
        }

        // Fall back to normal access check.
        self.check_access(class, resource, userid, requested)
    }

    // ─────── Search / query ───────

    /// Search for profiles within a class matching a mask.
    pub fn search(&self, class: &str, mask: &str) -> Vec<String> {
        let class = class.trim().to_uppercase();
        let mask = mask.trim().to_uppercase();

        self.profiles
            .iter()
            .filter(|((c, _), _)| c == &class)
            .filter(|((_, name), _)| matches_mask(name, &mask))
            .map(|((_, name), _)| name.clone())
            .collect()
    }

    /// List all profiles in a class.
    pub fn list_profiles(&self, class: &str) -> Vec<String> {
        let class = class.trim().to_uppercase();
        self.profiles
            .iter()
            .filter(|((c, _), _)| c == &class)
            .map(|((_, name), _)| name.clone())
            .collect()
    }

    /// Total number of resource profiles across all classes.
    pub fn profile_count(&self) -> usize {
        self.profiles.len()
    }

    /// Number of profiles in a specific class.
    pub fn class_profile_count(&self, class: &str) -> usize {
        let class = class.trim().to_uppercase();
        self.profiles
            .keys()
            .filter(|(c, _)| c == &class)
            .count()
    }
}

// ---------------------------------------------------------------------------
//  Authorization result
// ---------------------------------------------------------------------------

/// Result of a general resource authorization check.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResourceAuthResult {
    /// Whether access is granted.
    pub granted: bool,
    /// The access level the user has.
    pub access: AccessLevel,
    /// The profile name that governed the decision (if any).
    pub profile_name: Option<String>,
    /// The reason for the decision.
    pub reason: AuthReason,
}

/// Reason for an authorization decision.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AuthReason {
    /// Access granted.
    Authorized,
    /// Access denied — insufficient access level.
    InsufficientAccess,
    /// No profile found covering the resource.
    NoProfile,
    /// Resource class is not active.
    ClassNotActive,
}

// ---------------------------------------------------------------------------
//  Pattern matching for resource names
// ---------------------------------------------------------------------------

/// Match a resource name against a generic profile pattern.
///
/// Patterns support:
/// - `*` matches any sequence of characters within a single qualifier (dot-separated).
/// - `**` matches any sequence of qualifiers.
/// - `%` matches exactly one character.
fn resource_matches(name: &str, pattern: &str) -> bool {
    let name_parts: Vec<&str> = name.split('.').collect();
    let pat_parts: Vec<&str> = pattern.split('.').collect();
    match_parts(&name_parts, &pat_parts)
}

fn match_parts(name: &[&str], pattern: &[&str]) -> bool {
    if pattern.is_empty() {
        return name.is_empty();
    }
    if pattern[0] == "**" {
        // ** matches zero or more qualifiers.
        for i in 0..=name.len() {
            if match_parts(&name[i..], &pattern[1..]) {
                return true;
            }
        }
        return false;
    }
    if name.is_empty() {
        return false;
    }
    if match_qualifier(name[0], pattern[0]) {
        match_parts(&name[1..], &pattern[1..])
    } else {
        false
    }
}

/// Match a single qualifier against a pattern qualifier.
///
/// `*` within a qualifier matches any characters; `%` matches one character.
fn match_qualifier(name: &str, pattern: &str) -> bool {
    if pattern == "*" {
        return true;
    }
    match_chars(name.as_bytes(), pattern.as_bytes())
}

fn match_chars(name: &[u8], pattern: &[u8]) -> bool {
    if pattern.is_empty() {
        return name.is_empty();
    }
    if pattern[0] == b'*' {
        for i in 0..=name.len() {
            if match_chars(&name[i..], &pattern[1..]) {
                return true;
            }
        }
        return false;
    }
    if name.is_empty() {
        return false;
    }
    if pattern[0] == b'%' || pattern[0] == name[0] {
        match_chars(&name[1..], &pattern[1..])
    } else {
        false
    }
}

/// Compute specificity of a pattern (higher = more specific).
fn pattern_specificity(pattern: &str) -> u32 {
    let mut score = 0u32;
    for part in pattern.split('.') {
        if part == "**" {
            score += 1;
        } else if part == "*" || part.contains('%') || part.contains('*') {
            score += 2;
        } else {
            score += 3;
        }
    }
    score
}

/// Simple mask matching: `*` matches any sequence.
fn matches_mask(name: &str, mask: &str) -> bool {
    if mask == "*" {
        return true;
    }
    let parts: Vec<&str> = mask.split('*').collect();
    if parts.len() == 1 {
        return name == mask;
    }
    let mut pos = 0;
    for (i, part) in parts.iter().enumerate() {
        if part.is_empty() {
            continue;
        }
        if i == 0 {
            if !name.starts_with(part) {
                return false;
            }
            pos = part.len();
        } else if i == parts.len() - 1 {
            if !name[pos..].ends_with(part) {
                return false;
            }
        } else {
            match name[pos..].find(part) {
                Some(idx) => pos += idx + part.len(),
                None => return false,
            }
        }
    }
    true
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // ─────── S102.1: General Resource Commands ───────

    #[test]
    fn test_rdefine_creates_profile() {
        let mut mgr = ResourceManager::new();
        mgr.rdefine("FACILITY", "IRR.RADMIN.LISTUSER", AccessLevel::None, "ADMIN1")
            .unwrap();

        let result = mgr.rlist("FACILITY", "IRR.RADMIN.LISTUSER").unwrap();
        assert_eq!(result.profile.class, "FACILITY");
        assert_eq!(result.profile.name, "IRR.RADMIN.LISTUSER");
        assert_eq!(result.profile.uacc, AccessLevel::None);
        assert_eq!(result.profile.owner, "ADMIN1");
        assert!(!result.profile.generic);
    }

    #[test]
    fn test_rdefine_generic_profile() {
        let mut mgr = ResourceManager::new();
        mgr.rdefine("FACILITY", "IRR.RADMIN.*", AccessLevel::None, "ADMIN1")
            .unwrap();

        let result = mgr.rlist("FACILITY", "IRR.RADMIN.*").unwrap();
        assert!(result.profile.generic);
    }

    #[test]
    fn test_rdefine_duplicate_fails() {
        let mut mgr = ResourceManager::new();
        mgr.rdefine("FACILITY", "IRR.RADMIN", AccessLevel::None, "ADMIN1")
            .unwrap();

        let err = mgr
            .rdefine("FACILITY", "IRR.RADMIN", AccessLevel::Read, "ADMIN1")
            .unwrap_err();
        assert!(matches!(err, RacfError::ResourceProfileExists { .. }));
    }

    #[test]
    fn test_rdefine_unknown_class_fails() {
        let mut mgr = ResourceManager::new();
        let err = mgr
            .rdefine("NONEXISTENT", "RESOURCE1", AccessLevel::None, "ADMIN1")
            .unwrap_err();
        assert!(matches!(err, RacfError::ResourceClassNotFound { .. }));
    }

    #[test]
    fn test_ralter_changes_uacc() {
        let mut mgr = ResourceManager::new();
        mgr.rdefine("FACILITY", "IRR.TEST", AccessLevel::None, "ADMIN1")
            .unwrap();

        mgr.ralter("FACILITY", "IRR.TEST", Some(AccessLevel::Read), None, None)
            .unwrap();

        let result = mgr.rlist("FACILITY", "IRR.TEST").unwrap();
        assert_eq!(result.profile.uacc, AccessLevel::Read);
    }

    #[test]
    fn test_ralter_changes_owner() {
        let mut mgr = ResourceManager::new();
        mgr.rdefine("FACILITY", "IRR.TEST", AccessLevel::None, "ADMIN1")
            .unwrap();

        mgr.ralter("FACILITY", "IRR.TEST", None, Some("ADMIN2"), None)
            .unwrap();

        let result = mgr.rlist("FACILITY", "IRR.TEST").unwrap();
        assert_eq!(result.profile.owner, "ADMIN2");
    }

    #[test]
    fn test_rdelete_removes_profile() {
        let mut mgr = ResourceManager::new();
        mgr.rdefine("FACILITY", "IRR.TEST", AccessLevel::None, "ADMIN1")
            .unwrap();

        mgr.rdelete("FACILITY", "IRR.TEST").unwrap();

        let err = mgr.rlist("FACILITY", "IRR.TEST").unwrap_err();
        assert!(matches!(err, RacfError::ResourceProfileNotFound { .. }));
    }

    #[test]
    fn test_rdelete_not_found() {
        let mut mgr = ResourceManager::new();
        let err = mgr.rdelete("FACILITY", "NOEXIST").unwrap_err();
        assert!(matches!(err, RacfError::ResourceProfileNotFound { .. }));
    }

    #[test]
    fn test_permit_adds_access_entry() {
        let mut mgr = ResourceManager::new();
        mgr.rdefine("FACILITY", "IRR.RADMIN", AccessLevel::None, "ADMIN1")
            .unwrap();

        mgr.permit("FACILITY", "IRR.RADMIN", "JSMITH", AccessLevel::Read)
            .unwrap();

        let result = mgr.rlist("FACILITY", "IRR.RADMIN").unwrap();
        assert_eq!(
            result.profile.access_list.get("JSMITH"),
            Some(&AccessLevel::Read)
        );
    }

    #[test]
    fn test_permit_updates_existing_entry() {
        let mut mgr = ResourceManager::new();
        mgr.rdefine("FACILITY", "IRR.RADMIN", AccessLevel::None, "ADMIN1")
            .unwrap();
        mgr.permit("FACILITY", "IRR.RADMIN", "JSMITH", AccessLevel::Read)
            .unwrap();
        mgr.permit("FACILITY", "IRR.RADMIN", "JSMITH", AccessLevel::Update)
            .unwrap();

        let result = mgr.rlist("FACILITY", "IRR.RADMIN").unwrap();
        assert_eq!(
            result.profile.access_list.get("JSMITH"),
            Some(&AccessLevel::Update)
        );
    }

    // ─────── S102.2: Class Activation and Authorization ───────

    #[test]
    fn test_class_activation() {
        let mut mgr = ResourceManager::new();
        assert!(!mgr.is_class_active("FACILITY"));

        mgr.activate_class("FACILITY");
        assert!(mgr.is_class_active("FACILITY"));

        mgr.deactivate_class("FACILITY");
        assert!(!mgr.is_class_active("FACILITY"));
    }

    #[test]
    fn test_raclist_class() {
        let mut mgr = ResourceManager::new();
        assert!(!mgr.is_class_raclisted("FACILITY"));

        mgr.raclist_class("FACILITY");
        assert!(mgr.is_class_raclisted("FACILITY"));
    }

    #[test]
    fn test_check_access_granted() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("FACILITY");
        mgr.rdefine("FACILITY", "IRR.RADMIN.LISTUSER", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.permit("FACILITY", "IRR.RADMIN.LISTUSER", "JSMITH", AccessLevel::Read)
            .unwrap();

        let result = mgr.check_access("FACILITY", "IRR.RADMIN.LISTUSER", "JSMITH", AccessLevel::Read);
        assert!(result.granted);
        assert_eq!(result.access, AccessLevel::Read);
        assert_eq!(result.reason, AuthReason::Authorized);
    }

    #[test]
    fn test_check_access_denied() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("FACILITY");
        mgr.rdefine("FACILITY", "IRR.RADMIN.LISTUSER", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.permit("FACILITY", "IRR.RADMIN.LISTUSER", "JSMITH", AccessLevel::Read)
            .unwrap();

        let result = mgr.check_access("FACILITY", "IRR.RADMIN.LISTUSER", "JSMITH", AccessLevel::Update);
        assert!(!result.granted);
        assert_eq!(result.reason, AuthReason::InsufficientAccess);
    }

    #[test]
    fn test_check_access_uacc_fallback() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("FACILITY");
        mgr.rdefine("FACILITY", "BPX.SUPERUSER", AccessLevel::Read, "SYS1")
            .unwrap();

        let result = mgr.check_access("FACILITY", "BPX.SUPERUSER", "ANYBODY", AccessLevel::Read);
        assert!(result.granted);
        assert_eq!(result.access, AccessLevel::Read);
    }

    #[test]
    fn test_check_access_class_not_active() {
        let mut mgr = ResourceManager::new();
        mgr.rdefine("FACILITY", "IRR.TEST", AccessLevel::Read, "SYS1")
            .unwrap();

        let result = mgr.check_access("FACILITY", "IRR.TEST", "JSMITH", AccessLevel::Read);
        assert!(!result.granted);
        assert_eq!(result.reason, AuthReason::ClassNotActive);
    }

    #[test]
    fn test_check_access_no_profile() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("FACILITY");

        let result = mgr.check_access("FACILITY", "NOEXIST", "JSMITH", AccessLevel::Read);
        assert!(!result.granted);
        assert_eq!(result.reason, AuthReason::NoProfile);
    }

    #[test]
    fn test_check_access_generic_match() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("FACILITY");
        mgr.rdefine("FACILITY", "IRR.RADMIN.**", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.permit("FACILITY", "IRR.RADMIN.**", "JSMITH", AccessLevel::Read)
            .unwrap();

        let result = mgr.check_access("FACILITY", "IRR.RADMIN.LISTUSER", "JSMITH", AccessLevel::Read);
        assert!(result.granted);
        assert_eq!(result.profile_name, Some("IRR.RADMIN.**".to_string()));
    }

    #[test]
    fn test_check_access_most_specific_generic_wins() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("FACILITY");
        // Broad generic — UACC NONE.
        mgr.rdefine("FACILITY", "IRR.**", AccessLevel::None, "SYS1")
            .unwrap();
        // More specific — UACC READ.
        mgr.rdefine("FACILITY", "IRR.RADMIN.*", AccessLevel::Read, "SYS1")
            .unwrap();

        let result = mgr.check_access("FACILITY", "IRR.RADMIN.LISTUSER", "ANYBODY", AccessLevel::Read);
        assert!(result.granted);
        assert_eq!(result.profile_name, Some("IRR.RADMIN.*".to_string()));
    }

    // ─────── S102.2: Conditional Access ───────

    #[test]
    fn test_conditional_access_program() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("FACILITY");
        mgr.rdefine("FACILITY", "IRR.SECURE", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.permit_when(
            "FACILITY",
            "IRR.SECURE",
            "JSMITH",
            AccessLevel::Read,
            WhenCondition::Program("MYPROG".to_string()),
        )
        .unwrap();

        // With matching condition → granted.
        let result = mgr.check_access_conditional(
            "FACILITY",
            "IRR.SECURE",
            "JSMITH",
            AccessLevel::Read,
            &WhenCondition::Program("MYPROG".to_string()),
        );
        assert!(result.granted);

        // Without condition → denied (no standard access list entry).
        let result = mgr.check_access("FACILITY", "IRR.SECURE", "JSMITH", AccessLevel::Read);
        assert!(!result.granted);
    }

    // ─────── S102.3: Class Descriptor Table ───────

    #[test]
    fn test_ibm_classes_predefined() {
        let mgr = ResourceManager::new();
        assert!(mgr.get_class("FACILITY").is_some());
        assert!(mgr.get_class("PROGRAM").is_some());
        assert!(mgr.get_class("OPERCMDS").is_some());
        assert!(mgr.get_class("CICSTRN").is_some());
        assert!(mgr.get_class("MQQUEUE").is_some());
        assert!(mgr.get_class("UNIXPRIV").is_some());
    }

    #[test]
    fn test_custom_class_definition() {
        let mut mgr = ResourceManager::new();
        let mut cd = ClassDescriptor::new("MYCLASS");
        cd.max_length = 64;
        cd.generic_allowed = false;
        mgr.define_class(cd);

        let class = mgr.get_class("MYCLASS").unwrap();
        assert_eq!(class.max_length, 64);
        assert!(!class.generic_allowed);
    }

    #[test]
    fn test_list_active_classes() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("FACILITY");
        mgr.activate_class("PROGRAM");

        let active = mgr.list_active_classes();
        assert!(active.contains(&"FACILITY".to_string()));
        assert!(active.contains(&"PROGRAM".to_string()));
        assert!(!active.contains(&"OPERCMDS".to_string()));
    }

    // ─────── S102.4: IBM Class Names ───────

    #[test]
    fn test_ibm_classes_all() {
        let all = IbmClasses::all();
        assert!(all.len() >= 44);
        assert!(all.contains(&"FACILITY"));
        assert!(all.contains(&"JESSPOOL"));
        assert!(all.contains(&"MQQUEUE"));
        assert!(all.contains(&"DIGTCERT"));
        assert!(all.contains(&"SDSF"));
    }

    // ─────── Search / Query ───────

    #[test]
    fn test_search_profiles() {
        let mut mgr = ResourceManager::new();
        mgr.rdefine("FACILITY", "IRR.RADMIN.LISTUSER", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.rdefine("FACILITY", "IRR.RADMIN.ADDUSER", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.rdefine("FACILITY", "BPX.SUPERUSER", AccessLevel::None, "SYS1")
            .unwrap();

        let results = mgr.search("FACILITY", "IRR*");
        assert_eq!(results.len(), 2);
    }

    #[test]
    fn test_list_profiles_in_class() {
        let mut mgr = ResourceManager::new();
        mgr.rdefine("FACILITY", "IRR.RADMIN", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.rdefine("PROGRAM", "IEFBR14", AccessLevel::Read, "SYS1")
            .unwrap();

        assert_eq!(mgr.list_profiles("FACILITY").len(), 1);
        assert_eq!(mgr.list_profiles("PROGRAM").len(), 1);
        assert_eq!(mgr.profile_count(), 2);
    }

    #[test]
    fn test_class_profile_count() {
        let mut mgr = ResourceManager::new();
        mgr.rdefine("FACILITY", "A", AccessLevel::None, "SYS1").unwrap();
        mgr.rdefine("FACILITY", "B", AccessLevel::None, "SYS1").unwrap();
        mgr.rdefine("PROGRAM", "C", AccessLevel::None, "SYS1").unwrap();

        assert_eq!(mgr.class_profile_count("FACILITY"), 2);
        assert_eq!(mgr.class_profile_count("PROGRAM"), 1);
        assert_eq!(mgr.class_profile_count("OPERCMDS"), 0);
    }

    // ─────── Pattern matching ───────

    #[test]
    fn test_resource_matches_exact() {
        assert!(resource_matches("IRR.RADMIN.LISTUSER", "IRR.RADMIN.LISTUSER"));
        assert!(!resource_matches("IRR.RADMIN.LISTUSER", "IRR.RADMIN.ADDUSER"));
    }

    #[test]
    fn test_resource_matches_star() {
        assert!(resource_matches("IRR.RADMIN.LISTUSER", "IRR.RADMIN.*"));
        assert!(resource_matches("IRR.RADMIN.ADDUSER", "IRR.RADMIN.*"));
        assert!(!resource_matches("IRR.OTHER.LISTUSER", "IRR.RADMIN.*"));
    }

    #[test]
    fn test_resource_matches_double_star() {
        assert!(resource_matches("IRR.RADMIN.LISTUSER", "IRR.**"));
        assert!(resource_matches("IRR.A.B.C", "IRR.**"));
        assert!(resource_matches("IRR", "IRR.**"));
        assert!(!resource_matches("BPX.SUPERUSER", "IRR.**"));
    }

    #[test]
    fn test_resource_matches_percent() {
        assert!(resource_matches("IRR.RADMIN.LISTU", "IRR.RADMIN.LIST%"));
        assert!(!resource_matches("IRR.RADMIN.LISTUS", "IRR.RADMIN.LIST%"));
    }

    #[test]
    fn test_case_insensitive() {
        let mut mgr = ResourceManager::new();
        mgr.rdefine("facility", "irr.radmin", AccessLevel::None, "admin1")
            .unwrap();

        let result = mgr.rlist("FACILITY", "IRR.RADMIN").unwrap();
        assert_eq!(result.profile.name, "IRR.RADMIN");
    }

    #[test]
    fn test_installation_data() {
        let mut mgr = ResourceManager::new();
        mgr.rdefine("FACILITY", "IRR.TEST", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.ralter(
            "FACILITY",
            "IRR.TEST",
            None,
            None,
            Some("Custom install data"),
        )
        .unwrap();

        let result = mgr.rlist("FACILITY", "IRR.TEST").unwrap();
        assert_eq!(result.profile.installation_data, "Custom install data");
    }
}
