//! PDS/PDSE (Partitioned Data Set) member management.
//!
//! A PDS is a directory of members, where each member is a sequential
//! collection of records (like a file within a library). PDS datasets
//! are fundamental to z/OS — all load modules, JCL procedures, COBOL
//! copybooks, and source code are stored in PDS/PDSE format.
//!
//! # Architecture
//!
//! Per AD-3.0-02, PDS is implemented as:
//! - A JSON directory file (`_pds_directory.json`) containing member metadata
//! - A data subdirectory (`_pds_members/`) containing one file per member
//!
//! The directory is loaded into memory as a sorted `Vec<PdsMember>` for
//! fast lookup and listing. Member operations update both in-memory
//! directory and on-disk files.
//!
//! # Example
//!
//! ```ignore
//! use open_mainframe_dataset::pds::Pds;
//!
//! let mut pds = Pds::create("/datasets/MY/SOURCE")?;
//! pds.add_member("MAINPGM", b"       IDENTIFICATION DIVISION.\n")?;
//! let data = pds.read_member("MAINPGM")?;
//! ```

use std::path::{Path, PathBuf};

use crate::error::DatasetError;

/// Name of the directory metadata file within a PDS.
const DIRECTORY_FILE: &str = "_pds_directory.json";

/// Name of the member data subdirectory within a PDS.
const MEMBERS_DIR: &str = "_pds_members";

/// Maximum member name length (8 characters on z/OS).
const MAX_MEMBER_NAME_LEN: usize = 8;

/// ISPF-style statistics for a PDS member.
///
/// These statistics track member modification history and are
/// displayed by ISPF when browsing a PDS directory.
#[derive(Debug, Clone, PartialEq)]
pub struct IspfStats {
    /// Creation date (YYYY-MM-DD format).
    pub created: String,
    /// Last modification date (YYYY-MM-DD format).
    pub modified: String,
    /// User ID of last modifier.
    pub user_id: String,
    /// Version number (incremented on each save).
    pub version: u32,
    /// Number of current lines/records.
    pub current_lines: u32,
    /// Number of initial lines/records (at creation).
    pub initial_lines: u32,
    /// Number of modified lines since last save.
    pub modified_lines: u32,
}

impl Default for IspfStats {
    fn default() -> Self {
        Self {
            created: String::new(),
            modified: String::new(),
            user_id: String::new(),
            version: 1,
            current_lines: 0,
            initial_lines: 0,
            modified_lines: 0,
        }
    }
}

/// A PDS directory entry for a single member.
///
/// Contains the member name, optional alias, and optional ISPF statistics.
#[derive(Debug, Clone)]
pub struct PdsMember {
    /// Member name (up to 8 characters, uppercase).
    pub name: String,
    /// Whether this is an alias entry pointing to another member.
    pub alias_of: Option<String>,
    /// Optional ISPF statistics.
    pub stats: Option<IspfStats>,
}

impl PdsMember {
    /// Creates a new PDS member entry.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into().to_uppercase(),
            alias_of: None,
            stats: None,
        }
    }

    /// Creates a new PDS member entry with ISPF statistics.
    pub fn with_stats(name: impl Into<String>, stats: IspfStats) -> Self {
        Self {
            name: name.into().to_uppercase(),
            alias_of: None,
            stats: Some(stats),
        }
    }
}

/// A Partitioned Data Set (PDS/PDSE).
///
/// Manages a collection of members stored as individual files
/// within a filesystem directory. The member directory is maintained
/// as a sorted list for efficient lookup and listing.
#[derive(Debug)]
pub struct Pds {
    /// Root path of the PDS on the filesystem.
    root: PathBuf,
    /// In-memory directory of members (sorted by name).
    directory: Vec<PdsMember>,
}

impl Pds {
    /// Creates a new empty PDS at the given path.
    ///
    /// Creates the directory structure on disk:
    /// - `<root>/_pds_directory.json` — member metadata
    /// - `<root>/_pds_members/` — member data files
    pub fn create(root: impl AsRef<Path>) -> Result<Self, DatasetError> {
        let root = root.as_ref().to_path_buf();

        std::fs::create_dir_all(&root)?;
        std::fs::create_dir_all(root.join(MEMBERS_DIR))?;

        let pds = Self {
            root,
            directory: Vec::new(),
        };
        pds.save_directory()?;

        Ok(pds)
    }

    /// Opens an existing PDS at the given path.
    ///
    /// Loads the directory from disk into memory.
    pub fn open(root: impl AsRef<Path>) -> Result<Self, DatasetError> {
        let root = root.as_ref().to_path_buf();
        let dir_path = root.join(DIRECTORY_FILE);

        if !dir_path.exists() {
            return Err(DatasetError::NotFound {
                name: root.display().to_string(),
            });
        }

        let dir_json = std::fs::read_to_string(&dir_path)?;
        let directory = parse_directory(&dir_json)?;

        Ok(Self { root, directory })
    }

    /// Returns the PDS root path.
    pub fn root(&self) -> &Path {
        &self.root
    }

    /// Returns the number of members in the directory.
    pub fn member_count(&self) -> usize {
        self.directory.len()
    }

    /// Lists all members in sorted order.
    pub fn list_members(&self) -> &[PdsMember] {
        &self.directory
    }

    /// Checks whether a member exists.
    pub fn has_member(&self, name: &str) -> bool {
        let upper = name.to_uppercase();
        self.directory.iter().any(|m| m.name == upper)
    }

    /// Finds a member entry by name.
    pub fn find_member(&self, name: &str) -> Option<&PdsMember> {
        let upper = name.to_uppercase();
        self.directory.iter().find(|m| m.name == upper)
    }

    /// Adds a new member with the given content.
    ///
    /// The member name must be 1–8 characters. Content is written
    /// as a file in the members directory. Returns an error if the
    /// member already exists.
    pub fn add_member(
        &mut self,
        name: &str,
        content: &[u8],
    ) -> Result<(), DatasetError> {
        self.add_member_with_stats(name, content, None)
    }

    /// Adds a new member with content and optional ISPF statistics.
    pub fn add_member_with_stats(
        &mut self,
        name: &str,
        content: &[u8],
        stats: Option<IspfStats>,
    ) -> Result<(), DatasetError> {
        let upper = validate_member_name(name)?;

        if self.has_member(&upper) {
            return Err(DatasetError::AlreadyExists { name: upper });
        }

        // Write member data
        let member_path = self.member_path(&upper);
        std::fs::write(&member_path, content)?;

        // Add directory entry in sorted position
        let entry = PdsMember {
            name: upper,
            alias_of: None,
            stats,
        };
        let pos = self
            .directory
            .binary_search_by(|m| m.name.cmp(&entry.name))
            .unwrap_err();
        self.directory.insert(pos, entry);

        self.save_directory()?;
        Ok(())
    }

    /// Reads a member's content.
    ///
    /// Returns the full content of the member as bytes.
    pub fn read_member(&self, name: &str) -> Result<Vec<u8>, DatasetError> {
        let upper = validate_member_name(name)?;

        // If it's an alias, resolve to the real member
        let real_name = if let Some(member) = self.find_member(&upper) {
            if let Some(ref target) = member.alias_of {
                target.clone()
            } else {
                upper.clone()
            }
        } else {
            return Err(DatasetError::NotFound { name: upper });
        };

        let member_path = self.member_path(&real_name);
        if !member_path.exists() {
            return Err(DatasetError::NotFound { name: real_name });
        }

        Ok(std::fs::read(&member_path)?)
    }

    /// Updates (replaces) a member's content.
    ///
    /// If the member doesn't exist, it is created.
    pub fn update_member(
        &mut self,
        name: &str,
        content: &[u8],
    ) -> Result<(), DatasetError> {
        let upper = validate_member_name(name)?;

        let member_path = self.member_path(&upper);
        std::fs::write(&member_path, content)?;

        if !self.has_member(&upper) {
            // Add new entry
            let entry = PdsMember::new(&upper);
            let pos = self
                .directory
                .binary_search_by(|m| m.name.cmp(&entry.name))
                .unwrap_err();
            self.directory.insert(pos, entry);
            self.save_directory()?;
        }

        Ok(())
    }

    /// Deletes a member and its data.
    pub fn delete_member(&mut self, name: &str) -> Result<(), DatasetError> {
        let upper = validate_member_name(name)?;

        let idx = self
            .directory
            .iter()
            .position(|m| m.name == upper)
            .ok_or_else(|| DatasetError::NotFound {
                name: upper.clone(),
            })?;

        // Remove data file
        let member_path = self.member_path(&upper);
        if member_path.exists() {
            std::fs::remove_file(&member_path)?;
        }

        // Remove directory entry
        self.directory.remove(idx);

        // Also remove any aliases pointing to this member
        self.directory.retain(|m| m.alias_of.as_deref() != Some(&upper));

        self.save_directory()?;
        Ok(())
    }

    /// Renames a member.
    ///
    /// The directory entry is updated; content is unchanged.
    pub fn rename_member(
        &mut self,
        old_name: &str,
        new_name: &str,
    ) -> Result<(), DatasetError> {
        let old_upper = validate_member_name(old_name)?;
        let new_upper = validate_member_name(new_name)?;

        if self.has_member(&new_upper) {
            return Err(DatasetError::AlreadyExists { name: new_upper });
        }

        let idx = self
            .directory
            .iter()
            .position(|m| m.name == old_upper)
            .ok_or_else(|| DatasetError::NotFound {
                name: old_upper.clone(),
            })?;

        // Rename the data file
        let old_path = self.member_path(&old_upper);
        let new_path = self.member_path(&new_upper);
        if old_path.exists() {
            std::fs::rename(&old_path, &new_path)?;
        }

        // Update directory entry
        let mut entry = self.directory.remove(idx);
        entry.name = new_upper.clone();

        // Update any aliases pointing to the old name
        for m in &mut self.directory {
            if m.alias_of.as_deref() == Some(&old_upper) {
                m.alias_of = Some(new_upper.clone());
            }
        }

        // Re-insert in sorted position
        let pos = self
            .directory
            .binary_search_by(|m| m.name.cmp(&entry.name))
            .unwrap_err();
        self.directory.insert(pos, entry);

        self.save_directory()?;
        Ok(())
    }

    /// Adds an alias that points to an existing member.
    pub fn add_alias(
        &mut self,
        alias_name: &str,
        target_name: &str,
    ) -> Result<(), DatasetError> {
        let alias_upper = validate_member_name(alias_name)?;
        let target_upper = validate_member_name(target_name)?;

        if self.has_member(&alias_upper) {
            return Err(DatasetError::AlreadyExists {
                name: alias_upper,
            });
        }

        if !self.has_member(&target_upper) {
            return Err(DatasetError::NotFound {
                name: target_upper.clone(),
            });
        }

        let entry = PdsMember {
            name: alias_upper,
            alias_of: Some(target_upper),
            stats: None,
        };
        let pos = self
            .directory
            .binary_search_by(|m| m.name.cmp(&entry.name))
            .unwrap_err();
        self.directory.insert(pos, entry);

        self.save_directory()?;
        Ok(())
    }

    /// Returns the path to a member's data file.
    fn member_path(&self, name: &str) -> PathBuf {
        self.root.join(MEMBERS_DIR).join(name)
    }

    /// Persists the directory to disk as JSON.
    fn save_directory(&self) -> Result<(), DatasetError> {
        let json = serialize_directory(&self.directory);
        std::fs::write(self.root.join(DIRECTORY_FILE), json)?;
        Ok(())
    }
}

/// Validates a member name (1–8 uppercase alphanumeric characters).
fn validate_member_name(name: &str) -> Result<String, DatasetError> {
    let upper = name.trim().to_uppercase();
    if upper.is_empty() || upper.len() > MAX_MEMBER_NAME_LEN {
        return Err(DatasetError::InvalidName { name: upper });
    }
    // Allow alphanumeric plus @, #, $  (mainframe naming rules)
    if !upper
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '@' || c == '#' || c == '$')
    {
        return Err(DatasetError::InvalidName { name: upper });
    }
    Ok(upper)
}

/// Serialize directory entries to a JSON string.
///
/// Uses a simple manual format to avoid needing serde as a dependency.
fn serialize_directory(directory: &[PdsMember]) -> String {
    let mut json = String::from("[\n");
    for (i, member) in directory.iter().enumerate() {
        json.push_str("  {\n");
        json.push_str(&format!("    \"name\": \"{}\",\n", member.name));

        if let Some(ref alias) = member.alias_of {
            json.push_str(&format!("    \"alias_of\": \"{}\",\n", alias));
        } else {
            json.push_str("    \"alias_of\": null,\n");
        }

        if let Some(ref stats) = member.stats {
            json.push_str("    \"stats\": {\n");
            json.push_str(&format!("      \"created\": \"{}\",\n", stats.created));
            json.push_str(&format!("      \"modified\": \"{}\",\n", stats.modified));
            json.push_str(&format!("      \"user_id\": \"{}\",\n", stats.user_id));
            json.push_str(&format!("      \"version\": {},\n", stats.version));
            json.push_str(&format!("      \"current_lines\": {},\n", stats.current_lines));
            json.push_str(&format!("      \"initial_lines\": {},\n", stats.initial_lines));
            json.push_str(&format!("      \"modified_lines\": {}\n", stats.modified_lines));
            json.push_str("    }\n");
        } else {
            json.push_str("    \"stats\": null\n");
        }

        if i < directory.len() - 1 {
            json.push_str("  },\n");
        } else {
            json.push_str("  }\n");
        }
    }
    json.push(']');
    json
}

/// Parse directory entries from a JSON string.
///
/// Simple manual parser matching the format from `serialize_directory`.
fn parse_directory(json: &str) -> Result<Vec<PdsMember>, DatasetError> {
    let mut members = Vec::new();
    let trimmed = json.trim();

    if trimmed == "[]" {
        return Ok(members);
    }

    // Split by member objects (delimited by { ... })
    let mut depth = 0;
    let mut obj_start = None;

    for (i, c) in trimmed.char_indices() {
        match c {
            '{' => {
                if depth == 0 {
                    obj_start = Some(i);
                }
                depth += 1;
            }
            '}' => {
                depth -= 1;
                if depth == 0 {
                    if let Some(start) = obj_start {
                        let obj_str = &trimmed[start..=i];
                        members.push(parse_member_entry(obj_str)?);
                    }
                }
            }
            _ => {}
        }
    }

    // Ensure sorted order
    members.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(members)
}

/// Parse a single member entry from a JSON object string.
fn parse_member_entry(obj: &str) -> Result<PdsMember, DatasetError> {
    let name = extract_json_string(obj, "name").ok_or_else(|| DatasetError::InvalidFormat {
        message: "PDS directory entry missing 'name'".to_string(),
    })?;

    let alias_of = extract_json_string(obj, "alias_of");

    let stats = if obj.contains("\"stats\": {") {
        Some(parse_stats(obj)?)
    } else {
        None
    };

    Ok(PdsMember {
        name,
        alias_of,
        stats,
    })
}

/// Parse ISPF stats from within a member JSON object.
fn parse_stats(obj: &str) -> Result<IspfStats, DatasetError> {
    Ok(IspfStats {
        created: extract_json_string(obj, "created").unwrap_or_default(),
        modified: extract_json_string(obj, "modified").unwrap_or_default(),
        user_id: extract_json_string(obj, "user_id").unwrap_or_default(),
        version: extract_json_number(obj, "version").unwrap_or(1),
        current_lines: extract_json_number(obj, "current_lines").unwrap_or(0),
        initial_lines: extract_json_number(obj, "initial_lines").unwrap_or(0),
        modified_lines: extract_json_number(obj, "modified_lines").unwrap_or(0),
    })
}

/// Extract a string value from a JSON-like object by key.
fn extract_json_string(obj: &str, key: &str) -> Option<String> {
    let search = format!("\"{}\":", key);
    let start = obj.find(&search)?;
    let after = &obj[start + search.len()..];
    let after = after.trim_start();

    if after.starts_with("null") {
        return None;
    }

    if let Some(content) = after.strip_prefix('"') {
        let end = content.find('"')?;
        Some(content[..end].to_string())
    } else {
        None
    }
}

/// Extract a numeric value from a JSON-like object by key.
fn extract_json_number(obj: &str, key: &str) -> Option<u32> {
    let search = format!("\"{}\":", key);
    let start = obj.find(&search)?;
    let after = &obj[start + search.len()..];
    let after = after.trim_start();

    // Read digits
    let num_str: String = after.chars().take_while(|c| c.is_ascii_digit()).collect();
    num_str.parse().ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn test_dir() -> PathBuf {
        let count = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        std::env::temp_dir().join(format!("pds_test_{}", count))
    }

    fn cleanup(path: &Path) {
        let _ = std::fs::remove_dir_all(path);
    }

    // === Story 601.1: PDS Directory Structure ===

    #[test]
    fn test_create_pds() {
        let dir = test_dir();
        cleanup(&dir);

        let pds = Pds::create(&dir).unwrap();

        // Empty directory is created on disk
        assert!(dir.join(DIRECTORY_FILE).exists());
        assert!(dir.join(MEMBERS_DIR).exists());
        assert_eq!(pds.member_count(), 0);

        cleanup(&dir);
    }

    #[test]
    fn test_directory_sorted_order() {
        let dir = test_dir();
        cleanup(&dir);

        let mut pds = Pds::create(&dir).unwrap();
        pds.add_member("SUB1", b"sub1 content").unwrap();
        pds.add_member("MAIN", b"main content").unwrap();
        pds.add_member("COPYBK", b"copybk content").unwrap();

        // All three members returned in sorted order
        let members = pds.list_members();
        assert_eq!(members.len(), 3);
        assert_eq!(members[0].name, "COPYBK");
        assert_eq!(members[1].name, "MAIN");
        assert_eq!(members[2].name, "SUB1");

        cleanup(&dir);
    }

    #[test]
    fn test_open_existing_pds() {
        let dir = test_dir();
        cleanup(&dir);

        // Create and add members
        {
            let mut pds = Pds::create(&dir).unwrap();
            pds.add_member("MAIN", b"main content").unwrap();
            pds.add_member("SUB1", b"sub1 content").unwrap();
        }

        // Reopen and verify
        let pds = Pds::open(&dir).unwrap();
        assert_eq!(pds.member_count(), 2);
        assert!(pds.has_member("MAIN"));
        assert!(pds.has_member("SUB1"));

        cleanup(&dir);
    }

    #[test]
    fn test_has_member() {
        let dir = test_dir();
        cleanup(&dir);

        let mut pds = Pds::create(&dir).unwrap();
        pds.add_member("MAINPGM", b"content").unwrap();

        assert!(pds.has_member("MAINPGM"));
        assert!(pds.has_member("mainpgm")); // case-insensitive
        assert!(!pds.has_member("NOTHERE"));

        cleanup(&dir);
    }

    // === Story 601.2: Member I/O Operations ===

    #[test]
    fn test_add_and_read_member() {
        let dir = test_dir();
        cleanup(&dir);

        let mut pds = Pds::create(&dir).unwrap();
        let content = b"       IDENTIFICATION DIVISION.\n       PROGRAM-ID. MAINPGM.\n";
        pds.add_member("MAINPGM", content).unwrap();

        // Member is stored and directory updated
        assert_eq!(pds.member_count(), 1);

        // Full content returned
        let data = pds.read_member("MAINPGM").unwrap();
        assert_eq!(&data, content);

        cleanup(&dir);
    }

    #[test]
    fn test_add_duplicate_member_error() {
        let dir = test_dir();
        cleanup(&dir);

        let mut pds = Pds::create(&dir).unwrap();
        pds.add_member("MAINPGM", b"v1").unwrap();

        let result = pds.add_member("MAINPGM", b"v2");
        assert!(result.is_err());

        cleanup(&dir);
    }

    #[test]
    fn test_read_nonexistent_member() {
        let dir = test_dir();
        cleanup(&dir);

        let pds = Pds::create(&dir).unwrap();
        let result = pds.read_member("NOPE");
        assert!(result.is_err());

        cleanup(&dir);
    }

    #[test]
    fn test_update_member() {
        let dir = test_dir();
        cleanup(&dir);

        let mut pds = Pds::create(&dir).unwrap();
        pds.add_member("MAINPGM", b"version 1").unwrap();
        pds.update_member("MAINPGM", b"version 2").unwrap();

        let data = pds.read_member("MAINPGM").unwrap();
        assert_eq!(&data, b"version 2");
        assert_eq!(pds.member_count(), 1);

        cleanup(&dir);
    }

    #[test]
    fn test_delete_member() {
        let dir = test_dir();
        cleanup(&dir);

        let mut pds = Pds::create(&dir).unwrap();
        pds.add_member("TEMP", b"temporary").unwrap();
        assert_eq!(pds.member_count(), 1);

        pds.delete_member("TEMP").unwrap();

        // Directory entry and data file are removed
        assert_eq!(pds.member_count(), 0);
        assert!(!pds.has_member("TEMP"));
        assert!(!dir.join(MEMBERS_DIR).join("TEMP").exists());

        cleanup(&dir);
    }

    #[test]
    fn test_delete_nonexistent_member() {
        let dir = test_dir();
        cleanup(&dir);

        let mut pds = Pds::create(&dir).unwrap();
        let result = pds.delete_member("NOPE");
        assert!(result.is_err());

        cleanup(&dir);
    }

    #[test]
    fn test_rename_member() {
        let dir = test_dir();
        cleanup(&dir);

        let mut pds = Pds::create(&dir).unwrap();
        pds.add_member("OLD", b"my content").unwrap();

        pds.rename_member("OLD", "NEW").unwrap();

        // Directory entry updated; content unchanged
        assert!(!pds.has_member("OLD"));
        assert!(pds.has_member("NEW"));
        let data = pds.read_member("NEW").unwrap();
        assert_eq!(&data, b"my content");

        cleanup(&dir);
    }

    #[test]
    fn test_rename_to_existing_name_error() {
        let dir = test_dir();
        cleanup(&dir);

        let mut pds = Pds::create(&dir).unwrap();
        pds.add_member("A", b"a").unwrap();
        pds.add_member("B", b"b").unwrap();

        let result = pds.rename_member("A", "B");
        assert!(result.is_err());

        cleanup(&dir);
    }

    #[test]
    fn test_alias() {
        let dir = test_dir();
        cleanup(&dir);

        let mut pds = Pds::create(&dir).unwrap();
        pds.add_member("REALNAME", b"real content").unwrap();
        pds.add_alias("MYALIAS", "REALNAME").unwrap();

        // Alias shows in directory
        assert_eq!(pds.member_count(), 2);
        assert!(pds.has_member("MYALIAS"));

        // Reading alias returns the real member's content
        let data = pds.read_member("MYALIAS").unwrap();
        assert_eq!(&data, b"real content");

        cleanup(&dir);
    }

    #[test]
    fn test_alias_to_nonexistent_error() {
        let dir = test_dir();
        cleanup(&dir);

        let mut pds = Pds::create(&dir).unwrap();
        let result = pds.add_alias("MYALIAS", "NOPE");
        assert!(result.is_err());

        cleanup(&dir);
    }

    #[test]
    fn test_delete_member_removes_aliases() {
        let dir = test_dir();
        cleanup(&dir);

        let mut pds = Pds::create(&dir).unwrap();
        pds.add_member("TARGET", b"data").unwrap();
        pds.add_alias("ALIAS1", "TARGET").unwrap();

        pds.delete_member("TARGET").unwrap();
        assert_eq!(pds.member_count(), 0);

        cleanup(&dir);
    }

    // === Story 601.3: ISPF-Style Statistics ===

    #[test]
    fn test_member_with_ispf_stats() {
        let dir = test_dir();
        cleanup(&dir);

        let stats = IspfStats {
            created: "2024-01-15".to_string(),
            modified: "2024-06-20".to_string(),
            user_id: "DEVUSER".to_string(),
            version: 3,
            current_lines: 150,
            initial_lines: 100,
            modified_lines: 50,
        };

        let mut pds = Pds::create(&dir).unwrap();
        pds.add_member_with_stats("MAINPGM", b"content", Some(stats.clone()))
            .unwrap();

        // Verify stats are returned in directory listing
        let members = pds.list_members();
        assert_eq!(members.len(), 1);
        let member = &members[0];
        assert_eq!(member.name, "MAINPGM");
        let s = member.stats.as_ref().unwrap();
        assert_eq!(s.created, "2024-01-15");
        assert_eq!(s.modified, "2024-06-20");
        assert_eq!(s.user_id, "DEVUSER");
        assert_eq!(s.version, 3);
        assert_eq!(s.current_lines, 150);
        assert_eq!(s.initial_lines, 100);
        assert_eq!(s.modified_lines, 50);

        cleanup(&dir);
    }

    #[test]
    fn test_ispf_stats_persisted_to_disk() {
        let dir = test_dir();
        cleanup(&dir);

        let stats = IspfStats {
            created: "2024-03-01".to_string(),
            modified: "2024-03-15".to_string(),
            user_id: "ADMIN".to_string(),
            version: 2,
            current_lines: 200,
            initial_lines: 180,
            modified_lines: 20,
        };

        {
            let mut pds = Pds::create(&dir).unwrap();
            pds.add_member_with_stats("MYPROG", b"source", Some(stats))
                .unwrap();
        }

        // Reopen and verify stats survived persistence
        let pds = Pds::open(&dir).unwrap();
        let member = pds.find_member("MYPROG").unwrap();
        let s = member.stats.as_ref().unwrap();
        assert_eq!(s.created, "2024-03-01");
        assert_eq!(s.modified, "2024-03-15");
        assert_eq!(s.user_id, "ADMIN");
        assert_eq!(s.version, 2);
        assert_eq!(s.current_lines, 200);

        cleanup(&dir);
    }

    #[test]
    fn test_validate_member_name() {
        assert!(validate_member_name("MAIN").is_ok());
        assert!(validate_member_name("A").is_ok());
        assert!(validate_member_name("ABCDEFGH").is_ok()); // 8 chars max
        assert!(validate_member_name("ABCDEFGHI").is_err()); // 9 chars too long
        assert!(validate_member_name("").is_err());
        assert!(validate_member_name("BAD NAME").is_err()); // space
        assert!(validate_member_name("BAD.NAME").is_err()); // dot
    }

    #[test]
    fn test_directory_serialization_roundtrip() {
        let members = vec![
            PdsMember::new("ALPHA"),
            PdsMember::with_stats(
                "BETA",
                IspfStats {
                    created: "2024-01-01".to_string(),
                    modified: "2024-06-15".to_string(),
                    user_id: "USR".to_string(),
                    version: 5,
                    current_lines: 42,
                    initial_lines: 30,
                    modified_lines: 12,
                },
            ),
        ];

        let json = serialize_directory(&members);
        let parsed = parse_directory(&json).unwrap();

        assert_eq!(parsed.len(), 2);
        assert_eq!(parsed[0].name, "ALPHA");
        assert!(parsed[0].stats.is_none());
        assert_eq!(parsed[1].name, "BETA");
        let s = parsed[1].stats.as_ref().unwrap();
        assert_eq!(s.version, 5);
        assert_eq!(s.current_lines, 42);
    }
}
