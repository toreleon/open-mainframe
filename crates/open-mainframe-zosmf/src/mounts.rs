//! Mount table for mapping host filesystem paths into the OpenMainframe virtual environment.
//!
//! Supports three mount types:
//! - **Dataset PDS**: Map a host directory as a PDS (files become members)
//! - **Dataset Sequential**: Map a host file as a sequential dataset
//! - **USS**: Map a host directory to a USS path (bind mount)

use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

/// Type of mount — determines how the host path is exposed.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum MountType {
    /// Host directory → PDS: files in directory become members.
    DatasetPds,
    /// Host file → sequential dataset.
    DatasetSeq,
    /// Host directory → USS path (bind mount).
    Uss,
}

/// A single mount entry mapping a host path to a virtual path.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MountEntry {
    /// Unique mount identifier.
    pub mount_id: String,
    /// Type of mount.
    pub mount_type: MountType,
    /// Real path on the host filesystem.
    pub host_path: PathBuf,
    /// Virtual path in OpenMainframe (DSN for dataset mounts, USS path for USS mounts).
    pub virtual_path: String,
    /// Whether the mount is read-only.
    #[serde(default)]
    pub read_only: bool,
    /// Glob pattern to filter which files are exposed (e.g., "*.cbl").
    #[serde(default)]
    pub file_filter: Option<String>,
}

/// Mount table tracking all active mounts.
#[derive(Debug, Default)]
pub struct MountTable {
    /// Active mount entries.
    mounts: Vec<MountEntry>,
    /// Auto-incrementing ID counter.
    next_id: u64,
}

impl MountTable {
    /// Create a new empty mount table.
    pub fn new() -> Self {
        Self {
            mounts: Vec::new(),
            next_id: 1,
        }
    }

    /// Add a mount entry. Returns the assigned mount ID.
    pub fn add_mount(&mut self, mount_type: MountType, host_path: PathBuf, virtual_path: String, read_only: bool, file_filter: Option<String>) -> String {
        let mount_id = format!("MNT{:05}", self.next_id);
        self.next_id += 1;

        self.mounts.push(MountEntry {
            mount_id: mount_id.clone(),
            mount_type,
            host_path,
            virtual_path,
            read_only,
            file_filter,
        });

        mount_id
    }

    /// Remove a mount by ID. Returns true if found and removed.
    pub fn remove_mount(&mut self, mount_id: &str) -> bool {
        let len_before = self.mounts.len();
        self.mounts.retain(|m| m.mount_id != mount_id);
        self.mounts.len() < len_before
    }

    /// Get all active mounts.
    pub fn list(&self) -> &[MountEntry] {
        &self.mounts
    }

    /// Resolve a dataset name (DSN) to a host path via mount table.
    ///
    /// For PDS mounts: `DSN` → directory path, `DSN(MEMBER)` → file in directory.
    /// For sequential mounts: `DSN` → file path.
    ///
    /// Returns `None` if no mount matches the DSN.
    pub fn resolve_dataset(&self, dsn: &str, member: Option<&str>) -> Option<MountedPath> {
        let dsn_upper = dsn.to_uppercase();

        for entry in &self.mounts {
            let vpath_upper = entry.virtual_path.to_uppercase();

            match entry.mount_type {
                MountType::DatasetPds => {
                    if dsn_upper == vpath_upper {
                        if let Some(mem) = member {
                            // Resolve member to a file in the host directory
                            let member_path = self.find_member_file(&entry.host_path, mem, entry.file_filter.as_deref());
                            return Some(MountedPath {
                                host_path: member_path,
                                read_only: entry.read_only,
                                mount_id: entry.mount_id.clone(),
                                is_pds: true,
                            });
                        } else {
                            // Return the directory itself (for listing)
                            return Some(MountedPath {
                                host_path: entry.host_path.clone(),
                                read_only: entry.read_only,
                                mount_id: entry.mount_id.clone(),
                                is_pds: true,
                            });
                        }
                    }
                }
                MountType::DatasetSeq => {
                    if dsn_upper == vpath_upper {
                        return Some(MountedPath {
                            host_path: entry.host_path.clone(),
                            read_only: entry.read_only,
                            mount_id: entry.mount_id.clone(),
                            is_pds: false,
                        });
                    }
                }
                MountType::Uss => {
                    // USS mounts don't resolve dataset names
                }
            }
        }

        None
    }

    /// Resolve a USS path to a host path via mount table.
    ///
    /// Checks if the USS path falls under any USS mount point.
    /// Returns the mapped host path if found.
    pub fn resolve_uss(&self, uss_path: &str) -> Option<MountedPath> {
        let normalized = normalize_uss_path(uss_path);

        for entry in &self.mounts {
            if entry.mount_type != MountType::Uss {
                continue;
            }

            let mount_point = normalize_uss_path(&entry.virtual_path);

            if normalized == mount_point || normalized.starts_with(&format!("{}/", mount_point)) {
                // Strip the mount point prefix and join with host path
                let relative = if normalized == mount_point {
                    String::new()
                } else {
                    normalized[mount_point.len() + 1..].to_string()
                };

                let host_path = if relative.is_empty() {
                    entry.host_path.clone()
                } else {
                    entry.host_path.join(&relative)
                };

                return Some(MountedPath {
                    host_path,
                    read_only: entry.read_only,
                    mount_id: entry.mount_id.clone(),
                    is_pds: false,
                });
            }
        }

        None
    }

    /// List PDS members from a mounted directory.
    ///
    /// Returns member names (uppercased, extension-stripped, truncated to 8 chars).
    pub fn list_pds_members(&self, dsn: &str) -> Option<Vec<String>> {
        let dsn_upper = dsn.to_uppercase();

        for entry in &self.mounts {
            if entry.mount_type != MountType::DatasetPds {
                continue;
            }
            if entry.virtual_path.to_uppercase() != dsn_upper {
                continue;
            }

            let dir = &entry.host_path;
            if !dir.is_dir() {
                return Some(Vec::new());
            }

            let mut members = Vec::new();
            if let Ok(entries) = std::fs::read_dir(dir) {
                for file_entry in entries.flatten() {
                    let path = file_entry.path();
                    if !path.is_file() {
                        continue;
                    }

                    let name = file_entry.file_name().to_string_lossy().to_string();

                    // Apply file filter if set
                    if let Some(ref filter) = entry.file_filter {
                        if !matches_glob(&name, filter) {
                            continue;
                        }
                    }

                    // Convert to member name: strip extension, uppercase, truncate to 8
                    let member_name = file_to_member_name(&name);
                    if !member_name.is_empty() {
                        members.push(member_name);
                    }
                }
            }

            members.sort();
            members.dedup();
            return Some(members);
        }

        None
    }

    /// List datasets from all dataset mounts matching a pattern.
    pub fn list_mounted_datasets(&self, pattern: &str) -> Vec<String> {
        let pattern_upper = pattern.to_uppercase();
        let mut results = Vec::new();

        for entry in &self.mounts {
            match entry.mount_type {
                MountType::DatasetPds | MountType::DatasetSeq => {
                    let vpath_upper = entry.virtual_path.to_uppercase();
                    if matches_dsn_pattern(&vpath_upper, &pattern_upper) {
                        results.push(vpath_upper);
                    }
                }
                MountType::Uss => {}
            }
        }

        results
    }

    /// Check if a dataset name is provided by a mount.
    pub fn is_mounted_dataset(&self, dsn: &str) -> bool {
        let dsn_upper = dsn.to_uppercase();
        self.mounts.iter().any(|m| {
            matches!(m.mount_type, MountType::DatasetPds | MountType::DatasetSeq)
                && m.virtual_path.to_uppercase() == dsn_upper
        })
    }

    /// Build a map of DSN → host path for all dataset mounts.
    /// Used to populate JCL executor's `dataset_overrides`.
    pub fn dataset_overrides(&self) -> std::collections::HashMap<String, PathBuf> {
        let mut map = std::collections::HashMap::new();
        for entry in &self.mounts {
            match entry.mount_type {
                MountType::DatasetPds | MountType::DatasetSeq => {
                    map.insert(entry.virtual_path.to_uppercase(), entry.host_path.clone());
                }
                MountType::Uss => {}
            }
        }
        map
    }

    /// Find a member file in a host directory.
    ///
    /// Searches for files matching the member name (case-insensitive),
    /// trying exact match first, then with common extensions.
    fn find_member_file(&self, dir: &Path, member: &str, file_filter: Option<&str>) -> PathBuf {
        let member_upper = member.to_uppercase();

        if let Ok(entries) = std::fs::read_dir(dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if !path.is_file() {
                    continue;
                }

                let name = entry.file_name().to_string_lossy().to_string();

                // Apply file filter
                if let Some(filter) = file_filter {
                    if !matches_glob(&name, filter) {
                        continue;
                    }
                }

                let converted = file_to_member_name(&name);
                if converted == member_upper {
                    return path;
                }
            }
        }

        // Fallback: construct a path (for writes to new members)
        dir.join(&member_upper)
    }
}

/// Result of a mount resolution — the host path and metadata.
#[derive(Debug, Clone)]
pub struct MountedPath {
    /// Resolved host filesystem path.
    pub host_path: PathBuf,
    /// Whether the mount is read-only.
    pub read_only: bool,
    /// ID of the mount that resolved this path.
    pub mount_id: String,
    /// Whether the mounted dataset is a PDS (directory of members).
    pub is_pds: bool,
}

/// Convert a filename to a PDS member name.
///
/// Strips extension, uppercases, truncates to 8 characters.
fn file_to_member_name(filename: &str) -> String {
    let stem = Path::new(filename)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or(filename);

    let mut name = stem.to_uppercase();
    name.truncate(8);
    // Filter to valid member name chars (A-Z, 0-9, @, #, $)
    name.retain(|c| c.is_ascii_alphanumeric() || c == '@' || c == '#' || c == '$');
    name
}

/// Normalize a USS path (ensure leading /, remove trailing /).
fn normalize_uss_path(path: &str) -> String {
    let trimmed = path.trim();
    let with_slash = if trimmed.starts_with('/') {
        trimmed.to_string()
    } else {
        format!("/{}", trimmed)
    };
    with_slash.trim_end_matches('/').to_string()
}

/// Simple glob matching supporting `*` wildcard.
fn matches_glob(name: &str, pattern: &str) -> bool {
    let name_lower = name.to_lowercase();
    let pattern_lower = pattern.to_lowercase();

    if !pattern_lower.contains('*') {
        return name_lower == pattern_lower;
    }

    let parts: Vec<&str> = pattern_lower.split('*').collect();
    let mut pos = 0;

    for (i, part) in parts.iter().enumerate() {
        if part.is_empty() {
            continue;
        }
        if let Some(found) = name_lower[pos..].find(part) {
            if i == 0 && found != 0 {
                return false;
            }
            pos += found + part.len();
        } else {
            return false;
        }
    }

    if !pattern_lower.ends_with('*') && pos != name_lower.len() {
        return false;
    }

    true
}

/// DSN pattern matching (supports * wildcard).
fn matches_dsn_pattern(dsn: &str, pattern: &str) -> bool {
    if pattern == "*" || pattern.is_empty() {
        return true;
    }
    matches_glob(dsn, pattern)
}

/// Parse a CLI mount argument in the format `host_path:virtual_path` or `host_path:virtual_path:type`.
///
/// Type can be `pds`, `seq`, or `uss`. Defaults to `pds` for --mount-dataset and `uss` for --mount-uss.
pub fn parse_mount_arg(arg: &str, default_type: MountType) -> Option<(MountType, PathBuf, String)> {
    let parts: Vec<&str> = arg.splitn(3, ':').collect();
    if parts.len() < 2 {
        return None;
    }

    let host_path = PathBuf::from(parts[0]);
    let virtual_path = parts[1].to_string();

    let mount_type = if parts.len() >= 3 {
        match parts[2].to_lowercase().as_str() {
            "pds" => MountType::DatasetPds,
            "seq" => MountType::DatasetSeq,
            "uss" => MountType::Uss,
            _ => default_type,
        }
    } else {
        default_type
    };

    Some((mount_type, host_path, virtual_path))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mount_table_add_remove() {
        let mut table = MountTable::new();
        let id = table.add_mount(
            MountType::DatasetPds,
            PathBuf::from("/home/user/cobol"),
            "USER.COBOL.SRC".to_string(),
            false,
            Some("*.cbl".to_string()),
        );
        assert_eq!(table.list().len(), 1);
        assert!(table.remove_mount(&id));
        assert!(table.list().is_empty());
    }

    #[test]
    fn test_mount_table_resolve_dataset_pds() {
        let mut table = MountTable::new();
        table.add_mount(
            MountType::DatasetPds,
            PathBuf::from("/tmp/test-cobol"),
            "USER.COBOL.SRC".to_string(),
            false,
            None,
        );

        // Resolve PDS itself
        let result = table.resolve_dataset("USER.COBOL.SRC", None).unwrap();
        assert_eq!(result.host_path, PathBuf::from("/tmp/test-cobol"));
        assert!(result.is_pds);

        // Case insensitive
        let result = table.resolve_dataset("user.cobol.src", None).unwrap();
        assert_eq!(result.host_path, PathBuf::from("/tmp/test-cobol"));

        // Non-matching DSN
        assert!(table.resolve_dataset("OTHER.DATASET", None).is_none());
    }

    #[test]
    fn test_mount_table_resolve_dataset_seq() {
        let mut table = MountTable::new();
        table.add_mount(
            MountType::DatasetSeq,
            PathBuf::from("/tmp/input.dat"),
            "USER.INPUT.DATA".to_string(),
            true,
            None,
        );

        let result = table.resolve_dataset("USER.INPUT.DATA", None).unwrap();
        assert_eq!(result.host_path, PathBuf::from("/tmp/input.dat"));
        assert!(result.read_only);
        assert!(!result.is_pds);
    }

    #[test]
    fn test_mount_table_resolve_uss() {
        let mut table = MountTable::new();
        table.add_mount(
            MountType::Uss,
            PathBuf::from("/home/user/project"),
            "/u/ibmuser/project".to_string(),
            false,
            None,
        );

        // Exact mount point
        let result = table.resolve_uss("/u/ibmuser/project").unwrap();
        assert_eq!(result.host_path, PathBuf::from("/home/user/project"));

        // Subpath
        let result = table.resolve_uss("/u/ibmuser/project/src/main.c").unwrap();
        assert_eq!(result.host_path, PathBuf::from("/home/user/project/src/main.c"));

        // Non-matching path
        assert!(table.resolve_uss("/u/ibmuser/other").is_none());
    }

    #[test]
    fn test_mount_table_list_mounted_datasets() {
        let mut table = MountTable::new();
        table.add_mount(MountType::DatasetPds, PathBuf::from("/tmp/a"), "USER.COBOL.SRC".to_string(), false, None);
        table.add_mount(MountType::DatasetSeq, PathBuf::from("/tmp/b"), "USER.INPUT.DATA".to_string(), false, None);
        table.add_mount(MountType::Uss, PathBuf::from("/tmp/c"), "/u/ibmuser".to_string(), false, None);

        let results = table.list_mounted_datasets("USER.*");
        assert_eq!(results.len(), 2);
        assert!(results.contains(&"USER.COBOL.SRC".to_string()));
        assert!(results.contains(&"USER.INPUT.DATA".to_string()));
    }

    #[test]
    fn test_file_to_member_name() {
        assert_eq!(file_to_member_name("program.cbl"), "PROGRAM");
        assert_eq!(file_to_member_name("PAYROLL.cob"), "PAYROLL");
        assert_eq!(file_to_member_name("hello"), "HELLO");
        assert_eq!(file_to_member_name("verylongname.cbl"), "VERYLONG");
    }

    #[test]
    fn test_normalize_uss_path() {
        assert_eq!(normalize_uss_path("/u/ibmuser/"), "/u/ibmuser");
        assert_eq!(normalize_uss_path("/u/ibmuser"), "/u/ibmuser");
        assert_eq!(normalize_uss_path("u/ibmuser"), "/u/ibmuser");
    }

    #[test]
    fn test_matches_glob() {
        assert!(matches_glob("program.cbl", "*.cbl"));
        assert!(matches_glob("PAYROLL.CBL", "*.cbl"));
        assert!(!matches_glob("program.txt", "*.cbl"));
        assert!(matches_glob("anything", "*"));
    }

    #[test]
    fn test_parse_mount_arg() {
        let (mt, hp, vp) = parse_mount_arg("/home/user/cobol:USER.COBOL.SRC", MountType::DatasetPds).unwrap();
        assert_eq!(mt, MountType::DatasetPds);
        assert_eq!(hp, PathBuf::from("/home/user/cobol"));
        assert_eq!(vp, "USER.COBOL.SRC");

        let (mt, hp, vp) = parse_mount_arg("/home/user/data.dat:USER.DATA:seq", MountType::DatasetPds).unwrap();
        assert_eq!(mt, MountType::DatasetSeq);
        assert_eq!(hp, PathBuf::from("/home/user/data.dat"));
        assert_eq!(vp, "USER.DATA");

        assert!(parse_mount_arg("no-colon", MountType::Uss).is_none());
    }

    #[test]
    fn test_is_mounted_dataset() {
        let mut table = MountTable::new();
        table.add_mount(MountType::DatasetPds, PathBuf::from("/tmp"), "MY.PDS".to_string(), false, None);
        assert!(table.is_mounted_dataset("MY.PDS"));
        assert!(table.is_mounted_dataset("my.pds"));
        assert!(!table.is_mounted_dataset("OTHER.DS"));
    }

    #[test]
    fn test_list_pds_members_from_directory() {
        let temp_dir = std::env::temp_dir().join("test_mount_pds_members");
        let _ = std::fs::remove_dir_all(&temp_dir);
        std::fs::create_dir_all(&temp_dir).unwrap();

        // Create some source files
        std::fs::write(temp_dir.join("program1.cbl"), "       ID DIVISION.\n").unwrap();
        std::fs::write(temp_dir.join("program2.cbl"), "       ID DIVISION.\n").unwrap();
        std::fs::write(temp_dir.join("readme.txt"), "not a cobol file").unwrap();
        std::fs::create_dir(temp_dir.join("subdir")).unwrap();

        let mut table = MountTable::new();
        table.add_mount(
            MountType::DatasetPds,
            temp_dir.clone(),
            "USER.COBOL.SRC".to_string(),
            false,
            Some("*.cbl".to_string()),
        );

        let members = table.list_pds_members("USER.COBOL.SRC").unwrap();
        assert_eq!(members.len(), 2);
        assert!(members.contains(&"PROGRAM1".to_string()));
        assert!(members.contains(&"PROGRAM2".to_string()));
        // readme.txt should be filtered out by *.cbl filter
        assert!(!members.contains(&"README".to_string()));

        let _ = std::fs::remove_dir_all(&temp_dir);
    }
}
