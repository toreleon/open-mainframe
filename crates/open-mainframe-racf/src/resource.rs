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
    /// STDATA segment (STARTED class only).
    pub stdata: Option<StartedData>,
    /// Program members (PROGRAM class only — ADDMEM entries).
    pub program_members: Vec<ProgramMember>,
    /// Grouping class member resources (ADDMEM for grouping classes).
    pub group_members: Vec<String>,
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
            stdata: None,
            program_members: Vec::new(),
            group_members: Vec::new(),
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
    /// POSIT number — identifies the class position in the CDT.
    pub posit: u16,
    /// Maximum profile name length (MAXLNTH).
    pub max_length: usize,
    /// Whether generic profiles are allowed (GENERIC).
    pub generic_allowed: bool,
    /// Default UACC for profiles in this class.
    pub default_uacc: AccessLevel,
    /// Whether the class is currently active (SETROPTS CLASSACT).
    pub active: bool,
    /// Whether the class is RACLISTed (in-storage profiles).
    pub raclisted: bool,
    /// Allowed first character pattern for profile names.
    pub first_char_alpha_national: bool,
    /// Allowed subsequent character pattern (alphanumeric + national).
    pub other_char_alpha_national: bool,
    /// Member class name — if this is a grouping class.
    pub member_class: Option<String>,
    /// Grouping class name — the grouping class for this member class.
    pub group_class: Option<String>,
    /// Whether OPERATIONS attribute grants implicit access.
    pub operations: bool,
}

impl ClassDescriptor {
    /// Create a new class descriptor with default properties.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.trim().to_uppercase(),
            posit: 0,
            max_length: 246,
            generic_allowed: true,
            default_uacc: AccessLevel::None,
            active: false,
            raclisted: false,
            first_char_alpha_national: true,
            other_char_alpha_national: true,
            member_class: None,
            group_class: None,
            operations: false,
        }
    }

    /// Create a class descriptor with POSIT number.
    pub fn with_posit(name: &str, posit: u16) -> Self {
        let mut cd = Self::new(name);
        cd.posit = posit;
        cd
    }
}

/// STDATA segment for STARTED class profiles.
///
/// Defines the identity and attributes for a started task (STC).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StartedData {
    /// The userid the started task runs under.
    pub user: String,
    /// The group for the started task.
    pub group: String,
    /// Whether the started task is TRUSTED (bypasses access checking).
    pub trusted: bool,
    /// Whether the started task is PRIVILEGED.
    pub privileged: bool,
}

/// Program member entry for PROGRAM class profiles (ADDMEM).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProgramMember {
    /// Load library containing the program.
    pub library: String,
    /// Whether padding checking is performed (PADCHK/NOPADCHK).
    pub padchk: bool,
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
    /// Grouping class for FACILITY.
    pub const GFACILITY: &'static str = "GFACILITY";
    /// Grouping class for TERMINAL.
    pub const GTERMINL: &'static str = "GTERMINL";

    /// All IBM-supplied class names (including grouping classes).
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
            Self::GFACILITY, Self::GTERMINL,
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
        // Pre-define all IBM classes with proper POSIT numbers and relationships.
        mgr.init_ibm_classes();
        mgr
    }

    /// Initialize all IBM-supplied class descriptors with CDT properties.
    fn init_ibm_classes(&mut self) {
        // Define each class with POSIT, relationships, and properties.
        let class_defs: Vec<(&str, u16)> = vec![
            (IbmClasses::FACILITY, 26),
            (IbmClasses::PROGRAM, 18),
            (IbmClasses::OPERCMDS, 23),
            (IbmClasses::STARTED, 27),
            (IbmClasses::SURROGAT, 39),
            (IbmClasses::JESSPOOL, 20),
            (IbmClasses::JESJOBS, 19),
            (IbmClasses::JESINPUT, 35),
            (IbmClasses::WRITER, 36),
            (IbmClasses::CICSTRN, 7),
            (IbmClasses::GCICSTRN, 8),
            (IbmClasses::TCICSTRN, 9),
            (IbmClasses::DSNR, 11),
            (IbmClasses::MQCONN, 56),
            (IbmClasses::MQADMIN, 57),
            (IbmClasses::MQQUEUE, 58),
            (IbmClasses::MQPROC, 59),
            (IbmClasses::MQCMDS, 60),
            (IbmClasses::CSFKEYS, 130),
            (IbmClasses::GCSFKEYS, 131),
            (IbmClasses::CSFSERV, 132),
            (IbmClasses::SERVAUTH, 37),
            (IbmClasses::APPL, 3),
            (IbmClasses::TERMINAL, 5),
            (IbmClasses::CONSOLE, 6),
            (IbmClasses::NODES, 10),
            (IbmClasses::SECLABEL, 28),
            (IbmClasses::SECLEVEL, 29),
            (IbmClasses::UNIXPRIV, 42),
            (IbmClasses::FSSEC, 43),
            (IbmClasses::DIGTCERT, 63),
            (IbmClasses::DIGTRING, 64),
            (IbmClasses::RDATALIB, 65),
            (IbmClasses::PTKTDATA, 61),
            (IbmClasses::PTKTVAL, 62),
            (IbmClasses::GLOBAL, 30),
            (IbmClasses::RACFVARS, 33),
            (IbmClasses::LOGSTRM, 50),
            (IbmClasses::DASDVOL, 12),
            (IbmClasses::TAPEVOL, 13),
            (IbmClasses::TEMPDSN, 14),
            (IbmClasses::TSOAUTH, 4),
            (IbmClasses::TSOPROC, 15),
            (IbmClasses::SDSF, 70),
        ];

        for (name, posit) in class_defs {
            self.classes
                .insert(name.to_string(), ClassDescriptor::with_posit(name, posit));
        }

        // Also define grouping class constants.
        let gfacility = ClassDescriptor::with_posit(IbmClasses::GFACILITY, 126);
        self.classes
            .insert(IbmClasses::GFACILITY.to_string(), gfacility);

        let gterminl = ClassDescriptor::with_posit(IbmClasses::GTERMINL, 127);
        self.classes
            .insert(IbmClasses::GTERMINL.to_string(), gterminl);

        // Set up member/grouping class relationships.
        self.set_class_pair(IbmClasses::FACILITY, IbmClasses::GFACILITY);
        self.set_class_pair(IbmClasses::TERMINAL, IbmClasses::GTERMINL);
        self.set_class_pair(IbmClasses::CICSTRN, IbmClasses::GCICSTRN);
        self.set_class_pair(IbmClasses::CSFKEYS, IbmClasses::GCSFKEYS);

        // Set JESSPOOL max_length to support 4-qualifier naming.
        if let Some(cd) = self.classes.get_mut(IbmClasses::JESSPOOL) {
            cd.max_length = 72; // userid.jobname.jobid.dsid
        }

        // STARTED class uses 8-char naming.
        if let Some(cd) = self.classes.get_mut(IbmClasses::STARTED) {
            cd.generic_allowed = true;
        }
    }

    /// Set up a member/grouping class pair relationship.
    fn set_class_pair(&mut self, member_name: &str, group_name: &str) {
        if let Some(member_cd) = self.classes.get_mut(member_name) {
            member_cd.group_class = Some(group_name.to_string());
        }
        if let Some(group_cd) = self.classes.get_mut(group_name) {
            group_cd.member_class = Some(member_name.to_string());
        }
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

    // ─────── STARTED class STDATA ───────

    /// RDEFINE STARTED with STDATA segment — define a started task identity.
    pub fn rdefine_started(
        &mut self,
        profile_name: &str,
        owner: &str,
        stdata: StartedData,
    ) -> Result<(), RacfError> {
        let class = IbmClasses::STARTED.to_string();
        let profile_name = profile_name.trim().to_uppercase();

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

        let mut profile = ResourceProfile::new(&class, &profile_name, AccessLevel::None, owner);
        profile.stdata = Some(stdata);
        info!("RDEFINE STARTED '{}' STDATA(...)", profile_name);
        self.profiles.insert(key, profile);
        Ok(())
    }

    /// Resolve the started task identity for a given procedure name.
    ///
    /// Searches STARTED class for the best-matching profile (exact then generic).
    pub fn resolve_started_identity(&self, proc_name: &str) -> Option<&StartedData> {
        let proc_upper = proc_name.trim().to_uppercase();

        // Try exact match first.
        let exact_key = (IbmClasses::STARTED.to_string(), proc_upper.clone());
        if let Some(profile) = self.profiles.get(&exact_key) {
            return profile.stdata.as_ref();
        }

        // Try generic match (most specific wins).
        let mut best: Option<(&ResourceProfile, u32)> = None;
        for ((c, _), profile) in &self.profiles {
            if c != IbmClasses::STARTED {
                continue;
            }
            if !profile.generic {
                continue;
            }
            if resource_matches(&proc_upper, &profile.name) {
                let spec = pattern_specificity(&profile.name);
                if best.as_ref().map_or(true, |(_, s)| spec > *s) {
                    best = Some((profile, spec));
                }
            }
        }

        best.and_then(|(p, _)| p.stdata.as_ref())
    }

    // ─────── PROGRAM class ADDMEM ───────

    /// RDEFINE PROGRAM with ADDMEM — define a program with controlled libraries.
    pub fn rdefine_program(
        &mut self,
        profile_name: &str,
        owner: &str,
        members: Vec<ProgramMember>,
    ) -> Result<(), RacfError> {
        let class = IbmClasses::PROGRAM.to_string();
        let profile_name = profile_name.trim().to_uppercase();

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

        let mut profile = ResourceProfile::new(&class, &profile_name, AccessLevel::None, owner);
        profile.program_members = members;
        info!("RDEFINE PROGRAM '{}' ADDMEM(...)", profile_name);
        self.profiles.insert(key, profile);
        Ok(())
    }

    /// Check if a program in a library is in a controlled program list.
    pub fn is_program_controlled(&self, program: &str, library: &str) -> bool {
        let program_upper = program.trim().to_uppercase();
        let library_upper = library.trim().to_uppercase();

        let key = (IbmClasses::PROGRAM.to_string(), program_upper);
        if let Some(profile) = self.profiles.get(&key) {
            return profile
                .program_members
                .iter()
                .any(|m| m.library.to_uppercase() == library_upper);
        }
        false
    }

    // ─────── Grouping class operations ───────

    /// Add a member resource to a grouping class profile.
    pub fn add_group_member(
        &mut self,
        group_class: &str,
        profile_name: &str,
        member_name: &str,
    ) -> Result<(), RacfError> {
        let class = group_class.trim().to_uppercase();
        let profile_name = profile_name.trim().to_uppercase();
        let member = member_name.trim().to_uppercase();
        let key = (class.clone(), profile_name.clone());

        let profile = self.profiles.get_mut(&key).ok_or_else(|| {
            RacfError::ResourceProfileNotFound {
                class: class.clone(),
                name: profile_name.clone(),
            }
        })?;

        if !profile.group_members.contains(&member) {
            profile.group_members.push(member);
        }
        Ok(())
    }

    /// Remove a member resource from a grouping class profile.
    pub fn remove_group_member(
        &mut self,
        group_class: &str,
        profile_name: &str,
        member_name: &str,
    ) -> Result<(), RacfError> {
        let class = group_class.trim().to_uppercase();
        let profile_name = profile_name.trim().to_uppercase();
        let member = member_name.trim().to_uppercase();
        let key = (class.clone(), profile_name.clone());

        let profile = self.profiles.get_mut(&key).ok_or_else(|| {
            RacfError::ResourceProfileNotFound {
                class: class.clone(),
                name: profile_name.clone(),
            }
        })?;

        profile.group_members.retain(|m| m != &member);
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

    /// Check access with grouping class resolution.
    ///
    /// If no profile is found in the member class, checks the corresponding
    /// grouping class. If the resource is listed as a member of a grouping
    /// profile, that profile's access list is used.
    pub fn check_access_with_grouping(
        &self,
        class: &str,
        resource: &str,
        userid: &str,
        requested: AccessLevel,
    ) -> ResourceAuthResult {
        // First try the member class directly.
        let result = self.check_access(class, resource, userid, requested);
        if result.reason != AuthReason::NoProfile {
            return result;
        }

        // No profile in the member class — check grouping class.
        let class_upper = class.trim().to_uppercase();
        let resource_upper = resource.trim().to_uppercase();
        let userid_upper = userid.trim().to_uppercase();

        let group_class_name = match self
            .classes
            .get(&class_upper)
            .and_then(|cd| cd.group_class.as_ref())
        {
            Some(gc) => gc.clone(),
            None => return result, // No grouping class
        };

        // Check if grouping class is active.
        if !self.is_class_active(&group_class_name) {
            return result;
        }

        // Search grouping class profiles for one that lists this resource as a member.
        for ((c, _), profile) in &self.profiles {
            if c != &group_class_name {
                continue;
            }
            if profile.group_members.contains(&resource_upper) {
                let access = profile.effective_access(&userid_upper);
                return ResourceAuthResult {
                    granted: access >= requested,
                    access,
                    profile_name: Some(format!("{}({})", group_class_name, profile.name)),
                    reason: if access >= requested {
                        AuthReason::Authorized
                    } else {
                        AuthReason::InsufficientAccess
                    },
                };
            }
        }

        result
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

    // ═══════════════════════════════════════════════════════════════
    // SYS-109: General Resource Class Framework
    // ═══════════════════════════════════════════════════════════════

    // ─────── SYS-109.1: CDT Infrastructure ───────

    #[test]
    fn test_cdt_posit_numbers() {
        let mgr = ResourceManager::new();
        let facility = mgr.get_class("FACILITY").unwrap();
        assert_eq!(facility.posit, 26);

        let opercmds = mgr.get_class("OPERCMDS").unwrap();
        assert_eq!(opercmds.posit, 23);

        let started = mgr.get_class("STARTED").unwrap();
        assert_eq!(started.posit, 27);
    }

    #[test]
    fn test_cdt_custom_class_with_posit() {
        let mut mgr = ResourceManager::new();
        let mut cd = ClassDescriptor::with_posit("MYCLASS", 200);
        cd.max_length = 246;
        mgr.define_class(cd);

        let class = mgr.get_class("MYCLASS").unwrap();
        assert_eq!(class.posit, 200);
        assert_eq!(class.max_length, 246);
    }

    #[test]
    fn test_cdt_only_defined_classes_can_be_activated() {
        let mut mgr = ResourceManager::new();
        // Defined class can be activated.
        assert!(mgr.activate_class("FACILITY"));
        // Undefined class cannot.
        assert!(!mgr.activate_class("NOTACLASS"));
    }

    #[test]
    fn test_cdt_class_properties() {
        let mgr = ResourceManager::new();
        let facility = mgr.get_class("FACILITY").unwrap();
        assert!(facility.first_char_alpha_national);
        assert!(facility.other_char_alpha_national);
        assert!(facility.generic_allowed);
        assert_eq!(facility.default_uacc, AccessLevel::None);
    }

    #[test]
    fn test_cdt_jesspool_max_length() {
        let mgr = ResourceManager::new();
        let jesspool = mgr.get_class("JESSPOOL").unwrap();
        assert_eq!(jesspool.max_length, 72); // Supports 4-qualifier naming
    }

    #[test]
    fn test_cdt_grouping_class_relationships() {
        let mgr = ResourceManager::new();
        let facility = mgr.get_class("FACILITY").unwrap();
        assert_eq!(facility.group_class, Some("GFACILITY".to_string()));

        let gfacility = mgr.get_class("GFACILITY").unwrap();
        assert_eq!(gfacility.member_class, Some("FACILITY".to_string()));
    }

    // ─────── SYS-109.2: FACILITY Class ───────

    #[test]
    fn test_facility_irr_radmin_protection() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("FACILITY");

        // RDEFINE FACILITY IRR.RADMIN.* UACC(NONE)
        mgr.rdefine("FACILITY", "IRR.RADMIN.*", AccessLevel::None, "SYS1")
            .unwrap();

        // User without access is denied.
        let result = mgr.check_access(
            "FACILITY",
            "IRR.RADMIN.LISTUSER",
            "JSMITH",
            AccessLevel::Read,
        );
        assert!(!result.granted);

        // PERMIT read access.
        mgr.permit("FACILITY", "IRR.RADMIN.*", "SYSADM", AccessLevel::Read)
            .unwrap();
        let result = mgr.check_access(
            "FACILITY",
            "IRR.RADMIN.LISTUSER",
            "SYSADM",
            AccessLevel::Read,
        );
        assert!(result.granted);
    }

    #[test]
    fn test_facility_bpx_superuser() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("FACILITY");

        // RDEFINE FACILITY BPX.SUPERUSER UACC(NONE)
        mgr.rdefine("FACILITY", "BPX.SUPERUSER", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.permit("FACILITY", "BPX.SUPERUSER", "SYSADM", AccessLevel::Read)
            .unwrap();

        // SYSADM has READ access → USS privileged operation succeeds.
        let result = mgr.check_access("FACILITY", "BPX.SUPERUSER", "SYSADM", AccessLevel::Read);
        assert!(result.granted);

        // Unpermitted user is denied.
        let result = mgr.check_access("FACILITY", "BPX.SUPERUSER", "JSMITH", AccessLevel::Read);
        assert!(!result.granted);
    }

    #[test]
    fn test_facility_irr_pwreset_protection() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("FACILITY");

        mgr.rdefine("FACILITY", "IRR.PWRESET.*", AccessLevel::None, "SYS1")
            .unwrap();

        // Unpermitted user cannot reset passwords.
        let result = mgr.check_access(
            "FACILITY",
            "IRR.PWRESET.JSMITH",
            "NOAUTH",
            AccessLevel::Read,
        );
        assert!(!result.granted);
    }

    #[test]
    fn test_facility_generic_matching() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("FACILITY");

        mgr.rdefine("FACILITY", "IRR.**", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.permit("FACILITY", "IRR.**", "AUDITOR", AccessLevel::Read)
            .unwrap();

        // Any IRR.* resource should match.
        let result = mgr.check_access(
            "FACILITY",
            "IRR.RADMIN.ADDUSER",
            "AUDITOR",
            AccessLevel::Read,
        );
        assert!(result.granted);

        let result = mgr.check_access(
            "FACILITY",
            "IRR.PWRESET.JSMITH",
            "AUDITOR",
            AccessLevel::Read,
        );
        assert!(result.granted);
    }

    // ─────── SYS-109.3: OPERCMDS and STARTED Classes ───────

    #[test]
    fn test_opercmds_display_command() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("OPERCMDS");

        // MVS.DISPLAY.* format.
        mgr.rdefine("OPERCMDS", "MVS.DISPLAY.*", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.permit("OPERCMDS", "MVS.DISPLAY.*", "OPER1", AccessLevel::Control)
            .unwrap();

        // OPER1 can issue DISPLAY command.
        let result = mgr.check_access(
            "OPERCMDS",
            "MVS.DISPLAY.TCPIP",
            "OPER1",
            AccessLevel::Control,
        );
        assert!(result.granted);

        // Unpermitted user cannot.
        let result = mgr.check_access(
            "OPERCMDS",
            "MVS.DISPLAY.TCPIP",
            "JSMITH",
            AccessLevel::Control,
        );
        assert!(!result.granted);
    }

    #[test]
    fn test_opercmds_naming_convention() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("OPERCMDS");

        // IBM naming: MVS.verb.resource
        mgr.rdefine("OPERCMDS", "MVS.VARY.TCPIP", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.rdefine("OPERCMDS", "MVS.CANCEL.JOB", AccessLevel::None, "SYS1")
            .unwrap();

        assert_eq!(mgr.class_profile_count("OPERCMDS"), 2);
    }

    #[test]
    fn test_started_class_stdata() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("STARTED");

        let stdata = StartedData {
            user: "JES2".to_string(),
            group: "STC".to_string(),
            trusted: true,
            privileged: false,
        };
        // STARTED profiles use procname.jobname format.
        mgr.rdefine_started("JES2.*", "SYS1", stdata).unwrap();

        // Resolve identity for JES2 started task (procname.jobname).
        let identity = mgr.resolve_started_identity("JES2.JES2").unwrap();
        assert_eq!(identity.user, "JES2");
        assert_eq!(identity.group, "STC");
        assert!(identity.trusted);
    }

    #[test]
    fn test_started_generic_matching() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("STARTED");

        let stdata = StartedData {
            user: "STCUSR".to_string(),
            group: "STC".to_string(),
            trusted: false,
            privileged: false,
        };
        // ** matches zero or more qualifiers.
        mgr.rdefine_started("**", "SYS1", stdata).unwrap();

        // Generic ** should match any started task name.
        let identity = mgr.resolve_started_identity("ANYPROC").unwrap();
        assert_eq!(identity.user, "STCUSR");
    }

    #[test]
    fn test_setropts_classact_opercmds_started() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("OPERCMDS");
        mgr.activate_class("STARTED");
        mgr.raclist_class("OPERCMDS");
        mgr.raclist_class("STARTED");

        assert!(mgr.is_class_active("OPERCMDS"));
        assert!(mgr.is_class_active("STARTED"));
        assert!(mgr.is_class_raclisted("OPERCMDS"));
        assert!(mgr.is_class_raclisted("STARTED"));
    }

    // ─────── SYS-109.4: SURROGAT and JESSPOOL Classes ───────

    #[test]
    fn test_surrogat_surrogate_submission() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("SURROGAT");

        // RDEFINE SURROGAT JSMITH.SUBMIT UACC(NONE)
        mgr.rdefine("SURROGAT", "JSMITH.SUBMIT", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.permit("SURROGAT", "JSMITH.SUBMIT", "BATCHUSR", AccessLevel::Read)
            .unwrap();

        // BATCHUSR can submit as JSMITH.
        let result = mgr.check_access(
            "SURROGAT",
            "JSMITH.SUBMIT",
            "BATCHUSR",
            AccessLevel::Read,
        );
        assert!(result.granted);

        // NOAUTH cannot.
        let result = mgr.check_access(
            "SURROGAT",
            "JSMITH.SUBMIT",
            "NOAUTH",
            AccessLevel::Read,
        );
        assert!(!result.granted);
    }

    #[test]
    fn test_surrogat_no_profile_denies() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("SURROGAT");

        // No SURROGAT profile for target user → denied (ICH408I).
        let result = mgr.check_access(
            "SURROGAT",
            "UNKNOWN.SUBMIT",
            "BATCHUSR",
            AccessLevel::Read,
        );
        assert!(!result.granted);
        assert_eq!(result.reason, AuthReason::NoProfile);
    }

    #[test]
    fn test_jesspool_four_qualifier_naming() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("JESSPOOL");

        // userid.jobname.jobid.dsid format
        mgr.rdefine(
            "JESSPOOL",
            "JSMITH.MYJOB.JOB00123.D0000101",
            AccessLevel::None,
            "SYS1",
        )
        .unwrap();

        let result = mgr.rlist("JESSPOOL", "JSMITH.MYJOB.JOB00123.D0000101").unwrap();
        assert_eq!(result.profile.name, "JSMITH.MYJOB.JOB00123.D0000101");
    }

    #[test]
    fn test_jesspool_generic_pattern() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("JESSPOOL");

        // userid.*.*.** — covers all spool output for JSMITH
        mgr.rdefine("JESSPOOL", "JSMITH.*.*.**", AccessLevel::None, "SYS1")
            .unwrap();

        let result = mgr.check_access(
            "JESSPOOL",
            "JSMITH.MYJOB.JOB00001.D0000101",
            "VIEWER",
            AccessLevel::Read,
        );
        assert!(!result.granted); // UACC(NONE) by default
    }

    // ─────── SYS-109.5: UNIXPRIV and PROGRAM Classes ───────

    #[test]
    fn test_unixpriv_superuser_filesys() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("UNIXPRIV");

        mgr.rdefine("UNIXPRIV", "SUPERUSER.FILESYS", AccessLevel::None, "SYS1")
            .unwrap();

        // Without access → chown denied.
        let result = mgr.check_access(
            "UNIXPRIV",
            "SUPERUSER.FILESYS",
            "JSMITH",
            AccessLevel::Read,
        );
        assert!(!result.granted);

        // With PERMIT → chown allowed.
        mgr.permit("UNIXPRIV", "SUPERUSER.FILESYS", "SYSADM", AccessLevel::Read)
            .unwrap();
        let result = mgr.check_access(
            "UNIXPRIV",
            "SUPERUSER.FILESYS",
            "SYSADM",
            AccessLevel::Read,
        );
        assert!(result.granted);
    }

    #[test]
    fn test_unixpriv_profiles() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("UNIXPRIV");

        // Define standard UNIXPRIV profiles.
        mgr.rdefine("UNIXPRIV", "SUPERUSER.FILESYS", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.rdefine("UNIXPRIV", "SUPERUSER.PROCESS", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.rdefine("UNIXPRIV", "CHOWN.UNRESTRICTED", AccessLevel::None, "SYS1")
            .unwrap();

        assert_eq!(mgr.class_profile_count("UNIXPRIV"), 3);
    }

    #[test]
    fn test_program_addmem() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("PROGRAM");

        let members = vec![ProgramMember {
            library: "LOAD.LIB".to_string(),
            padchk: false,
        }];
        mgr.rdefine_program("MYPROG", "SYS1", members).unwrap();

        // Verify program is controlled from the specified library.
        assert!(mgr.is_program_controlled("MYPROG", "LOAD.LIB"));
        assert!(!mgr.is_program_controlled("MYPROG", "OTHER.LIB"));
    }

    #[test]
    fn test_program_control_multiple_libs() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("PROGRAM");

        let members = vec![
            ProgramMember {
                library: "SYS1.LINKLIB".to_string(),
                padchk: true,
            },
            ProgramMember {
                library: "USER.LOADLIB".to_string(),
                padchk: false,
            },
        ];
        mgr.rdefine_program("IEFBR14", "SYS1", members).unwrap();

        assert!(mgr.is_program_controlled("IEFBR14", "SYS1.LINKLIB"));
        assert!(mgr.is_program_controlled("IEFBR14", "USER.LOADLIB"));
        assert!(!mgr.is_program_controlled("IEFBR14", "UNAUTH.LIB"));
    }

    // ─────── SYS-109.6: Grouping Classes ───────

    #[test]
    fn test_grouping_class_member_inheritance() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("FACILITY");
        mgr.activate_class("GFACILITY");

        // Define a grouping profile in GFACILITY with members.
        mgr.rdefine("GFACILITY", "MYGROUP", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.permit("GFACILITY", "MYGROUP", "JSMITH", AccessLevel::Read)
            .unwrap();
        mgr.add_group_member("GFACILITY", "MYGROUP", "RESOURCE1")
            .unwrap();
        mgr.add_group_member("GFACILITY", "MYGROUP", "RESOURCE2")
            .unwrap();

        // Check access to RESOURCE1 in FACILITY class — resolved via GFACILITY.
        let result = mgr.check_access_with_grouping(
            "FACILITY",
            "RESOURCE1",
            "JSMITH",
            AccessLevel::Read,
        );
        assert!(result.granted);
        assert!(result.profile_name.unwrap().starts_with("GFACILITY"));
    }

    #[test]
    fn test_grouping_class_member_not_found() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("FACILITY");
        mgr.activate_class("GFACILITY");

        mgr.rdefine("GFACILITY", "MYGROUP", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.add_group_member("GFACILITY", "MYGROUP", "RESOURCE1")
            .unwrap();

        // RESOURCE_NOT_IN_GROUP should not be found.
        let result = mgr.check_access_with_grouping(
            "FACILITY",
            "RESOURCE_NOT_IN_GROUP",
            "JSMITH",
            AccessLevel::Read,
        );
        assert!(!result.granted);
        assert_eq!(result.reason, AuthReason::NoProfile);
    }

    #[test]
    fn test_grouping_class_direct_profile_takes_precedence() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("FACILITY");
        mgr.activate_class("GFACILITY");

        // Direct profile in FACILITY — grants READ.
        mgr.rdefine("FACILITY", "RESOURCE1", AccessLevel::Read, "SYS1")
            .unwrap();

        // Grouping profile — would grant NONE.
        mgr.rdefine("GFACILITY", "MYGROUP", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.add_group_member("GFACILITY", "MYGROUP", "RESOURCE1")
            .unwrap();

        // Direct profile takes precedence.
        let result = mgr.check_access_with_grouping(
            "FACILITY",
            "RESOURCE1",
            "ANYBODY",
            AccessLevel::Read,
        );
        assert!(result.granted);
        assert_eq!(result.profile_name, Some("RESOURCE1".to_string()));
    }

    #[test]
    fn test_cdt_class_pair_relationships() {
        let mgr = ResourceManager::new();

        // CICSTRN ↔ GCICSTRN
        let cicstrn = mgr.get_class("CICSTRN").unwrap();
        assert_eq!(cicstrn.group_class, Some("GCICSTRN".to_string()));

        let gcicstrn = mgr.get_class("GCICSTRN").unwrap();
        assert_eq!(gcicstrn.member_class, Some("CICSTRN".to_string()));

        // CSFKEYS ↔ GCSFKEYS
        let csfkeys = mgr.get_class("CSFKEYS").unwrap();
        assert_eq!(csfkeys.group_class, Some("GCSFKEYS".to_string()));
    }

    #[test]
    fn test_add_remove_group_members() {
        let mut mgr = ResourceManager::new();
        mgr.rdefine("GFACILITY", "MYGROUP", AccessLevel::None, "SYS1")
            .unwrap();

        mgr.add_group_member("GFACILITY", "MYGROUP", "RES1").unwrap();
        mgr.add_group_member("GFACILITY", "MYGROUP", "RES2").unwrap();
        mgr.add_group_member("GFACILITY", "MYGROUP", "RES3").unwrap();

        let result = mgr.rlist("GFACILITY", "MYGROUP").unwrap();
        assert_eq!(result.profile.group_members.len(), 3);

        mgr.remove_group_member("GFACILITY", "MYGROUP", "RES2").unwrap();
        let result = mgr.rlist("GFACILITY", "MYGROUP").unwrap();
        assert_eq!(result.profile.group_members.len(), 2);
        assert!(!result.profile.group_members.contains(&"RES2".to_string()));
    }

    // ─────── SYS-109.7: Integration Tests ───────

    #[test]
    fn test_integration_cics_transaction_security() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("CICSTRN");
        mgr.raclist_class("CICSTRN");

        // Define CICS transaction profiles.
        mgr.rdefine("CICSTRN", "CEMT", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.permit("CICSTRN", "CEMT", "CICSADM", AccessLevel::Read)
            .unwrap();

        // CICSADM can run CEMT.
        let result = mgr.check_access("CICSTRN", "CEMT", "CICSADM", AccessLevel::Read);
        assert!(result.granted);
        assert_eq!(result.reason, AuthReason::Authorized);

        // Unpermitted user cannot.
        let result = mgr.check_access("CICSTRN", "CEMT", "JSMITH", AccessLevel::Read);
        assert!(!result.granted);
    }

    #[test]
    fn test_integration_jes_job_security() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("SURROGAT");
        mgr.activate_class("JESJOBS");
        mgr.raclist_class("SURROGAT");

        // SURROGAT: allow BATCHUSR to submit as JSMITH.
        mgr.rdefine("SURROGAT", "JSMITH.SUBMIT", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.permit("SURROGAT", "JSMITH.SUBMIT", "BATCHUSR", AccessLevel::Read)
            .unwrap();

        let result = mgr.check_access(
            "SURROGAT",
            "JSMITH.SUBMIT",
            "BATCHUSR",
            AccessLevel::Read,
        );
        assert!(result.granted);

        // JESJOBS: define job-level security.
        mgr.rdefine("JESJOBS", "SUBMIT.JSMITH.*", AccessLevel::None, "SYS1")
            .unwrap();
        mgr.permit("JESJOBS", "SUBMIT.JSMITH.*", "BATCHUSR", AccessLevel::Read)
            .unwrap();

        let result = mgr.check_access(
            "JESJOBS",
            "SUBMIT.JSMITH.MYJOB",
            "BATCHUSR",
            AccessLevel::Read,
        );
        assert!(result.granted);
    }

    #[test]
    fn test_integration_uss_unixpriv_authorization() {
        let mut mgr = ResourceManager::new();
        mgr.activate_class("UNIXPRIV");
        mgr.raclist_class("UNIXPRIV");

        // Define standard USS privileged operations.
        for profile in [
            "SUPERUSER.FILESYS",
            "SUPERUSER.PROCESS",
            "CHOWN.UNRESTRICTED",
        ] {
            mgr.rdefine("UNIXPRIV", profile, AccessLevel::None, "SYS1")
                .unwrap();
        }

        // Grant SYSADM access to filesystem operations.
        mgr.permit("UNIXPRIV", "SUPERUSER.FILESYS", "SYSADM", AccessLevel::Read)
            .unwrap();

        // SYSADM can perform filesystem privileged ops.
        let result = mgr.check_access(
            "UNIXPRIV",
            "SUPERUSER.FILESYS",
            "SYSADM",
            AccessLevel::Read,
        );
        assert!(result.granted);

        // SYSADM cannot perform process privileged ops (not permitted).
        let result = mgr.check_access(
            "UNIXPRIV",
            "SUPERUSER.PROCESS",
            "SYSADM",
            AccessLevel::Read,
        );
        assert!(!result.granted);
    }

    #[test]
    fn test_integration_all_tests_pass() {
        // Verify that all classes can be activated and profiled.
        let mut mgr = ResourceManager::new();

        for &class in &["FACILITY", "OPERCMDS", "STARTED", "SURROGAT",
                         "JESSPOOL", "UNIXPRIV", "PROGRAM", "CICSTRN",
                         "GFACILITY", "GCICSTRN"] {
            assert!(mgr.activate_class(class), "Failed to activate {}", class);
        }

        // Define a profile in each class.
        mgr.rdefine("FACILITY", "TEST.RESOURCE", AccessLevel::Read, "SYS1").unwrap();
        mgr.rdefine("OPERCMDS", "MVS.DISPLAY.A", AccessLevel::None, "SYS1").unwrap();
        mgr.rdefine("SURROGAT", "TEST.SUBMIT", AccessLevel::None, "SYS1").unwrap();
        mgr.rdefine("JESSPOOL", "TEST.JOB.JOB1.D1", AccessLevel::None, "SYS1").unwrap();
        mgr.rdefine("UNIXPRIV", "SUPERUSER.TEST", AccessLevel::None, "SYS1").unwrap();

        let stdata = StartedData {
            user: "TESTUSER".to_string(),
            group: "STC".to_string(),
            trusted: false,
            privileged: false,
        };
        mgr.rdefine_started("TESTPROC.*", "SYS1", stdata).unwrap();

        let members = vec![ProgramMember {
            library: "SYS1.LINKLIB".to_string(),
            padchk: true,
        }];
        mgr.rdefine_program("TESTPGM", "SYS1", members).unwrap();

        // Verify counts.
        assert!(mgr.profile_count() >= 7);
        assert!(mgr.is_program_controlled("TESTPGM", "SYS1.LINKLIB"));
        assert!(mgr.resolve_started_identity("TESTPROC.TESTPROC").is_some());
    }
}
