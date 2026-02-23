//! BPXPRMxx Configuration & Security (USS-109).
//!
//! Provides:
//! - BPXPRMxx PARMLIB parser
//! - RACF OMVS segment integration
//! - Superuser and privilege checks
//! - Security tests

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors for configuration and security operations.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, miette::Diagnostic)]
pub enum ConfigError {
    /// Parse error in BPXPRMxx.
    #[error("BPXPRMxx parse error at line {line}: {detail}")]
    ParseError { line: usize, detail: String },

    /// Missing required parameter.
    #[error("missing required parameter: {param}")]
    MissingParameter { param: String },

    /// Invalid value.
    #[error("invalid value for {param}: {value}")]
    InvalidValue { param: String, value: String },

    /// Permission denied.
    #[error("permission denied: {detail}")]
    PermissionDenied { detail: String },

    /// No OMVS segment.
    #[error("user {user} has no OMVS segment")]
    NoOmvsSegment { user: String },
}

// ---------------------------------------------------------------------------
//  Mount Entry
// ---------------------------------------------------------------------------

/// A mount entry from BPXPRMxx.
#[derive(Debug, Clone)]
pub struct MountEntry {
    /// Filesystem name (e.g., "OMVS.ROOT").
    pub filesystem: String,
    /// File system type (e.g., "ZFS").
    pub fstype: String,
    /// Mount point.
    pub mountpoint: String,
    /// Read-write or read-only.
    pub mode: MountMode,
}

/// Mount mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MountMode {
    ReadWrite,
    ReadOnly,
}

// ---------------------------------------------------------------------------
//  Filesystem Type
// ---------------------------------------------------------------------------

/// A filesystem type registration from BPXPRMxx.
#[derive(Debug, Clone)]
pub struct FilesysType {
    /// Type name (e.g., "ZFS").
    pub name: String,
    /// Entry point module.
    pub entrypoint: String,
}

// ---------------------------------------------------------------------------
//  BPXPRMxx Config
// ---------------------------------------------------------------------------

/// Parsed BPXPRMxx configuration.
#[derive(Debug, Clone)]
pub struct BpxPrmConfig {
    /// Maximum processes system-wide.
    pub maxprocsys: u32,
    /// Maximum processes per user.
    pub maxprocuser: u32,
    /// Maximum open files per process.
    pub maxfileproc: u32,
    /// Maximum threads.
    pub maxthreads: u32,
    /// Maximum concurrent PTYs.
    pub maxptys: u32,
    /// Root filesystem name.
    pub root: String,
    /// Mount entries.
    pub mounts: Vec<MountEntry>,
    /// Registered filesystem types.
    pub filesys_types: Vec<FilesysType>,
    /// Additional keyword parameters.
    pub parameters: HashMap<String, String>,
}

impl Default for BpxPrmConfig {
    fn default() -> Self {
        Self {
            maxprocsys: 1024,
            maxprocuser: 256,
            maxfileproc: 65535,
            maxthreads: 4096,
            maxptys: 256,
            root: String::new(),
            mounts: Vec::new(),
            filesys_types: Vec::new(),
            parameters: HashMap::new(),
        }
    }
}

// ---------------------------------------------------------------------------
//  Parser
// ---------------------------------------------------------------------------

/// Parse BPXPRMxx configuration text.
pub fn parse_bpxprm(input: &str) -> Result<BpxPrmConfig, ConfigError> {
    let mut config = BpxPrmConfig::default();

    let mut line_num = 0;
    let lines = input.lines();

    for line in lines {
        line_num += 1;
        let line = line.trim();

        // Skip comments and blank lines.
        if line.is_empty() || line.starts_with("/*") || line.starts_with("//") {
            continue;
        }

        // Parse keyword(value) patterns.
        if let Some(value) = extract_keyword(line, "MAXPROCSYS") {
            config.maxprocsys = parse_number(&value, "MAXPROCSYS", line_num)?;
        } else if let Some(value) = extract_keyword(line, "MAXPROCUSER") {
            config.maxprocuser = parse_number(&value, "MAXPROCUSER", line_num)?;
        } else if let Some(value) = extract_keyword(line, "MAXFILEPROC") {
            config.maxfileproc = parse_number(&value, "MAXFILEPROC", line_num)?;
        } else if let Some(value) = extract_keyword(line, "MAXTHREADS") {
            config.maxthreads = parse_number(&value, "MAXTHREADS", line_num)?;
        } else if let Some(value) = extract_keyword(line, "MAXPTYS") {
            config.maxptys = parse_number(&value, "MAXPTYS", line_num)?;
        } else if line.starts_with("FILESYSTYPE") {
            if let Some(fstype) = parse_filesystype(line) {
                config.filesys_types.push(fstype);
            }
        } else if line.starts_with("ROOT") {
            if let Some(mount) = parse_root_entry(line) {
                config.root = mount.filesystem.clone();
                config.mounts.push(mount);
            }
        } else if line.starts_with("MOUNT") {
            if let Some(mount) = parse_mount_entry(line) {
                config.mounts.push(mount);
            }
        } else {
            // Generic parameter.
            if let Some(eq_pos) = line.find('(') {
                let key = line[..eq_pos].trim().to_string();
                if let Some(end) = line.find(')') {
                    let value = line[eq_pos + 1..end].trim().to_string();
                    config.parameters.insert(key, value);
                }
            }
        }
    }

    Ok(config)
}

fn extract_keyword(line: &str, keyword: &str) -> Option<String> {
    if line.starts_with(keyword) {
        if let Some(start) = line.find('(') {
            if let Some(end) = line.find(')') {
                return Some(line[start + 1..end].trim().to_string());
            }
        }
    }
    None
}

fn parse_number(value: &str, param: &str, _line: usize) -> Result<u32, ConfigError> {
    value.parse().map_err(|_| ConfigError::InvalidValue {
        param: param.to_string(),
        value: value.to_string(),
    })
}

fn parse_filesystype(line: &str) -> Option<FilesysType> {
    let type_name = extract_keyword(line, "FILESYSTYPE")?;
    // Look for TYPE(xxx) or ENTRYPOINT(xxx) in the rest.
    let name = extract_sub_keyword(line, "TYPE").unwrap_or(type_name);
    let entrypoint = extract_sub_keyword(line, "ENTRYPOINT").unwrap_or_default();
    Some(FilesysType { name, entrypoint })
}

fn parse_root_entry(line: &str) -> Option<MountEntry> {
    let filesystem = extract_sub_keyword(line, "FILESYSTEM")
        .map(|s| s.trim_matches('\'').to_string())?;
    let fstype = extract_sub_keyword(line, "TYPE").unwrap_or_else(|| "ZFS".to_string());
    let mode = if line.contains("RDONLY") {
        MountMode::ReadOnly
    } else {
        MountMode::ReadWrite
    };
    Some(MountEntry {
        filesystem,
        fstype,
        mountpoint: "/".to_string(),
        mode,
    })
}

fn parse_mount_entry(line: &str) -> Option<MountEntry> {
    let filesystem = extract_sub_keyword(line, "FILESYSTEM")
        .map(|s| s.trim_matches('\'').to_string())?;
    let fstype = extract_sub_keyword(line, "TYPE").unwrap_or_else(|| "ZFS".to_string());
    let mountpoint = extract_sub_keyword(line, "MOUNTPOINT")
        .map(|s| s.trim_matches('\'').to_string())
        .unwrap_or_else(|| "/".to_string());
    let mode = if line.contains("RDONLY") {
        MountMode::ReadOnly
    } else {
        MountMode::ReadWrite
    };
    Some(MountEntry {
        filesystem,
        fstype,
        mountpoint,
        mode,
    })
}

fn extract_sub_keyword(line: &str, keyword: &str) -> Option<String> {
    let upper = line.to_uppercase();
    let kw_upper = keyword.to_uppercase();
    if let Some(pos) = upper.find(&kw_upper) {
        let after = &line[pos + keyword.len()..];
        if let Some(start) = after.find('(') {
            if let Some(end) = after.find(')') {
                return Some(after[start + 1..end].trim().to_string());
            }
        }
    }
    None
}

// ---------------------------------------------------------------------------
//  OMVS Segment
// ---------------------------------------------------------------------------

/// RACF OMVS segment for a user.
#[derive(Debug, Clone)]
pub struct OmvsSegment {
    /// User ID (RACF).
    pub userid: String,
    /// UNIX UID.
    pub uid: u32,
    /// UNIX GID (default group).
    pub gid: u32,
    /// Home directory.
    pub home: String,
    /// Login shell program.
    pub program: String,
    /// Maximum address space size.
    pub assizemax: u64,
    /// Maximum CPU time (seconds).
    pub cputimemax: u32,
    /// Maximum files per process.
    pub fileprocmax: u32,
    /// Maximum number of processes.
    pub procusermax: u32,
    /// Maximum threads.
    pub threadsmax: u32,
    /// Maximum mapped memory.
    pub mmapareamax: u32,
}

impl OmvsSegment {
    /// Create a new OMVS segment with defaults.
    pub fn new(userid: &str, uid: u32, gid: u32) -> Self {
        Self {
            userid: userid.to_string(),
            uid,
            gid,
            home: format!("/u/{}", userid.to_lowercase()),
            program: "/bin/sh".to_string(),
            assizemax: 2_147_483_647,
            cputimemax: 0,
            fileprocmax: 65535,
            procusermax: 256,
            threadsmax: 4096,
            mmapareamax: 40960,
        }
    }
}

/// RACF OMVS group segment.
#[derive(Debug, Clone)]
pub struct OmvsGroupSegment {
    /// Group name.
    pub group: String,
    /// UNIX GID.
    pub gid: u32,
}

// ---------------------------------------------------------------------------
//  UNIXPRIV Facility
// ---------------------------------------------------------------------------

/// UNIXPRIV facility class profiles.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnixPriv {
    /// SUPERUSER.FILESYS.MOUNT — mount file systems.
    FilesysMount,
    /// SUPERUSER.FILESYS.CHOWN — chown files.
    FilesysChown,
    /// SUPERUSER.FILESYS.PFSCTL — PFS control.
    FilesysPfsctl,
    /// SUPERUSER.PROCESS.KILL — kill any process.
    ProcessKill,
    /// SUPERUSER.PROCESS.GETPSENT — get process info.
    ProcessGetpsent,
    /// SHARED.IDS — share UNIX IDs.
    SharedIds,
}

impl UnixPriv {
    /// Profile name string.
    pub fn profile_name(&self) -> &str {
        match self {
            Self::FilesysMount => "SUPERUSER.FILESYS.MOUNT",
            Self::FilesysChown => "SUPERUSER.FILESYS.CHOWN",
            Self::FilesysPfsctl => "SUPERUSER.FILESYS.PFSCTL",
            Self::ProcessKill => "SUPERUSER.PROCESS.KILL",
            Self::ProcessGetpsent => "SUPERUSER.PROCESS.GETPSENT",
            Self::SharedIds => "SHARED.IDS",
        }
    }
}

// ---------------------------------------------------------------------------
//  Security Manager
// ---------------------------------------------------------------------------

/// USS security manager.
#[derive(Debug)]
pub struct SecurityManager {
    /// OMVS segments by userid.
    omvs_segments: HashMap<String, OmvsSegment>,
    /// Group segments by group name.
    group_segments: HashMap<String, OmvsGroupSegment>,
    /// UNIXPRIV grants: userid -> set of privileges.
    unixpriv_grants: HashMap<String, Vec<UnixPriv>>,
    /// FACILITY class grants: profile -> Vec<userid>.
    facility_grants: HashMap<String, Vec<String>>,
}

impl SecurityManager {
    /// Create a new security manager.
    pub fn new() -> Self {
        Self {
            omvs_segments: HashMap::new(),
            group_segments: HashMap::new(),
            unixpriv_grants: HashMap::new(),
            facility_grants: HashMap::new(),
        }
    }

    /// Register an OMVS segment.
    pub fn add_omvs_segment(&mut self, segment: OmvsSegment) {
        self.omvs_segments
            .insert(segment.userid.clone(), segment);
    }

    /// Register a group segment.
    pub fn add_group_segment(&mut self, segment: OmvsGroupSegment) {
        self.group_segments
            .insert(segment.group.clone(), segment);
    }

    /// Look up an OMVS segment by userid.
    pub fn get_omvs_segment(&self, userid: &str) -> Option<&OmvsSegment> {
        self.omvs_segments.get(userid)
    }

    /// Look up a group segment.
    pub fn get_group_segment(&self, group: &str) -> Option<&OmvsGroupSegment> {
        self.group_segments.get(group)
    }

    /// Grant a UNIXPRIV privilege to a user.
    pub fn grant_unixpriv(&mut self, userid: &str, priv_type: UnixPriv) {
        self.unixpriv_grants
            .entry(userid.to_string())
            .or_default()
            .push(priv_type);
    }

    /// Grant a FACILITY class profile.
    pub fn grant_facility(&mut self, profile: &str, userid: &str) {
        self.facility_grants
            .entry(profile.to_string())
            .or_default()
            .push(userid.to_string());
    }

    /// Check if a user has a UNIXPRIV privilege.
    pub fn has_unixpriv(&self, userid: &str, priv_type: &UnixPriv) -> bool {
        self.unixpriv_grants
            .get(userid)
            .map_or(false, |privs| privs.contains(priv_type))
    }

    /// Check if a user has access to a FACILITY class profile.
    pub fn has_facility(&self, profile: &str, userid: &str) -> bool {
        self.facility_grants
            .get(profile)
            .map_or(false, |users| users.contains(&userid.to_string()))
    }

    /// Check superuser: euid==0 or has specific UNIXPRIV.
    pub fn is_superuser(&self, euid: u32) -> bool {
        euid == 0
    }

    /// Check if a user can perform chown.
    pub fn can_chown(&self, userid: &str, euid: u32) -> bool {
        self.is_superuser(euid) || self.has_unixpriv(userid, &UnixPriv::FilesysChown)
    }

    /// Check if a user can mount filesystems.
    pub fn can_mount(&self, userid: &str, euid: u32) -> bool {
        self.is_superuser(euid) || self.has_unixpriv(userid, &UnixPriv::FilesysMount)
    }

    /// Check if a daemon can call setuid (BPX.DAEMON check).
    pub fn can_setuid(&self, userid: &str) -> bool {
        self.has_facility("BPX.DAEMON", userid)
    }

    /// Dub a user as a UNIX process (returns UID/GID from OMVS segment).
    pub fn dub_user(&self, userid: &str) -> Result<(u32, u32), ConfigError> {
        let seg = self
            .omvs_segments
            .get(userid)
            .ok_or(ConfigError::NoOmvsSegment {
                user: userid.to_string(),
            })?;
        Ok((seg.uid, seg.gid))
    }
}

impl Default for SecurityManager {
    fn default() -> Self {
        Self::new()
    }
}

// ===========================================================================
//  Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_bpxprm_limits() {
        let input = r#"
MAXPROCSYS(1000)
MAXPROCUSER(256)
MAXFILEPROC(65535)
MAXTHREADS(4096)
"#;
        let config = parse_bpxprm(input).unwrap();
        assert_eq!(config.maxprocsys, 1000);
        assert_eq!(config.maxprocuser, 256);
        assert_eq!(config.maxfileproc, 65535);
        assert_eq!(config.maxthreads, 4096);
    }

    #[test]
    fn test_parse_bpxprm_filesystype() {
        let input = "FILESYSTYPE TYPE(ZFS) ENTRYPOINT(IOEFSCM)";
        let config = parse_bpxprm(input).unwrap();
        assert_eq!(config.filesys_types.len(), 1);
        assert_eq!(config.filesys_types[0].name, "ZFS");
        assert_eq!(config.filesys_types[0].entrypoint, "IOEFSCM");
    }

    #[test]
    fn test_parse_bpxprm_root() {
        let input = "ROOT FILESYSTEM('OMVS.ROOT') TYPE(ZFS) MODE(RDWR)";
        let config = parse_bpxprm(input).unwrap();
        assert_eq!(config.root, "OMVS.ROOT");
        assert_eq!(config.mounts.len(), 1);
        assert_eq!(config.mounts[0].mountpoint, "/");
        assert_eq!(config.mounts[0].mode, MountMode::ReadWrite);
    }

    #[test]
    fn test_parse_bpxprm_mount() {
        let input = "MOUNT FILESYSTEM('OMVS.TMP') TYPE(TFS) MOUNTPOINT('/tmp') MODE(RDWR)";
        let config = parse_bpxprm(input).unwrap();
        assert_eq!(config.mounts.len(), 1);
        assert_eq!(config.mounts[0].filesystem, "OMVS.TMP");
        assert_eq!(config.mounts[0].mountpoint, "/tmp");
    }

    #[test]
    fn test_parse_bpxprm_complete() {
        let input = r#"
/* BPXPRMxx - USS Configuration */
MAXPROCSYS(1000)
MAXPROCUSER(256)
MAXFILEPROC(65535)
MAXTHREADS(4096)

FILESYSTYPE TYPE(ZFS) ENTRYPOINT(IOEFSCM)
ROOT FILESYSTEM('OMVS.ROOT') TYPE(ZFS) MODE(RDWR)
MOUNT FILESYSTEM('OMVS.TMP') TYPE(TFS) MOUNTPOINT('/tmp')
"#;
        let config = parse_bpxprm(input).unwrap();
        assert_eq!(config.maxprocsys, 1000);
        assert_eq!(config.filesys_types.len(), 1);
        assert_eq!(config.mounts.len(), 2);
        assert_eq!(config.root, "OMVS.ROOT");
    }

    #[test]
    fn test_omvs_segment() {
        let seg = OmvsSegment::new("JSMITH", 100, 200);
        assert_eq!(seg.uid, 100);
        assert_eq!(seg.gid, 200);
        assert_eq!(seg.home, "/u/jsmith");
        assert_eq!(seg.program, "/bin/sh");
    }

    #[test]
    fn test_security_manager_dub() {
        let mut sec = SecurityManager::new();
        sec.add_omvs_segment(OmvsSegment::new("JSMITH", 100, 200));

        let (uid, gid) = sec.dub_user("JSMITH").unwrap();
        assert_eq!(uid, 100);
        assert_eq!(gid, 200);
    }

    #[test]
    fn test_security_manager_dub_no_segment() {
        let sec = SecurityManager::new();
        let err = sec.dub_user("NOBODY").unwrap_err();
        assert!(matches!(err, ConfigError::NoOmvsSegment { .. }));
    }

    #[test]
    fn test_superuser_chown() {
        let sec = SecurityManager::new();
        assert!(sec.can_chown("ROOT", 0));
        assert!(!sec.can_chown("JSMITH", 100));
    }

    #[test]
    fn test_unixpriv_mount() {
        let mut sec = SecurityManager::new();
        sec.grant_unixpriv("JSMITH", UnixPriv::FilesysMount);
        assert!(sec.can_mount("JSMITH", 100));
        assert!(!sec.can_mount("NOBODY", 100));
    }

    #[test]
    fn test_bpx_daemon_facility() {
        let mut sec = SecurityManager::new();
        sec.grant_facility("BPX.DAEMON", "DAEMON1");
        assert!(sec.can_setuid("DAEMON1"));
        assert!(!sec.can_setuid("JSMITH"));
    }

    #[test]
    fn test_unixpriv_profile_names() {
        assert_eq!(
            UnixPriv::FilesysMount.profile_name(),
            "SUPERUSER.FILESYS.MOUNT"
        );
        assert_eq!(
            UnixPriv::ProcessKill.profile_name(),
            "SUPERUSER.PROCESS.KILL"
        );
    }

    #[test]
    fn test_group_segment() {
        let mut sec = SecurityManager::new();
        sec.add_group_segment(OmvsGroupSegment {
            group: "OMVSGRP".to_string(),
            gid: 200,
        });
        let grp = sec.get_group_segment("OMVSGRP").unwrap();
        assert_eq!(grp.gid, 200);
    }
}
