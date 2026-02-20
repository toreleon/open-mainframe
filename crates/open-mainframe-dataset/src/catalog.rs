//! Dataset catalog for name resolution.
//!
//! Maps dataset names (DSN) to physical file paths.
//! Supports hierarchical naming (USER.DATA.FILE -> /datasets/USER/DATA/FILE).

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::error::DatasetError;
use crate::types::{DatasetAttributes, DatasetRef, RecordFormat};

/// Dataset catalog entry.
#[derive(Debug, Clone)]
pub struct CatalogEntry {
    /// Dataset name.
    pub dsn: String,
    /// Physical path.
    pub path: PathBuf,
    /// Dataset attributes.
    pub attributes: DatasetAttributes,
    /// Whether this is a PDS (partitioned dataset).
    pub is_pds: bool,
}

/// Dataset catalog for resolving names to paths.
pub struct Catalog {
    /// Base directory for datasets.
    base_dir: PathBuf,
    /// Explicit catalog entries.
    entries: HashMap<String, CatalogEntry>,
    /// File extensions to try when locating files.
    extensions: Vec<String>,
}

impl Catalog {
    /// Create a new catalog with the given base directory.
    pub fn new(base_dir: impl AsRef<Path>) -> Self {
        Self {
            base_dir: base_dir.as_ref().to_path_buf(),
            entries: HashMap::new(),
            extensions: vec![
                String::new(),
                ".dat".to_string(),
                ".txt".to_string(),
                ".cbl".to_string(),
                ".cob".to_string(),
                ".cpy".to_string(),
            ],
        }
    }

    /// Add an explicit catalog entry.
    pub fn add_entry(&mut self, entry: CatalogEntry) {
        self.entries.insert(entry.dsn.to_uppercase(), entry);
    }

    /// Look up a dataset by name.
    pub fn lookup(&self, dsn: &str) -> Result<DatasetRef, DatasetError> {
        let dsn_upper = dsn.to_uppercase();

        // Check explicit catalog first
        if let Some(entry) = self.entries.get(&dsn_upper) {
            return Ok(DatasetRef {
                dsn: entry.dsn.clone(),
                member: None,
                disp: Default::default(),
                attributes: entry.attributes.clone(),
                path: Some(entry.path.clone()),
            });
        }

        // Convert DSN to path
        let path = self.dsn_to_path(&dsn_upper)?;

        // Infer attributes from extension
        let attributes = self.infer_attributes(&path);

        Ok(DatasetRef {
            dsn: dsn_upper,
            member: None,
            disp: Default::default(),
            attributes,
            path: Some(path),
        })
    }

    /// Look up a PDS member.
    pub fn lookup_member(&self, dsn: &str, member: &str) -> Result<DatasetRef, DatasetError> {
        let dsn_upper = dsn.to_uppercase();
        let member_upper = member.to_uppercase();

        // Get the PDS path
        let pds_path = self.dsn_to_path(&dsn_upper)?;

        // Member is a file within the PDS directory
        let member_path = pds_path.join(&member_upper);

        // Try various extensions for the member
        let resolved_path = self.resolve_with_extensions(&member_path)?;

        let attributes = self.infer_attributes(&resolved_path);

        Ok(DatasetRef {
            dsn: dsn_upper,
            member: Some(member_upper),
            disp: Default::default(),
            attributes,
            path: Some(resolved_path),
        })
    }

    /// Convert a DSN to a path.
    fn dsn_to_path(&self, dsn: &str) -> Result<PathBuf, DatasetError> {
        // Replace dots with path separators
        // USER.DATA.FILE -> base_dir/USER/DATA/FILE
        let mut path = self.base_dir.clone();

        for component in dsn.split('.') {
            path.push(component);
        }

        // Try to find with various extensions
        self.resolve_with_extensions(&path)
    }

    /// Try to resolve a path with various extensions.
    fn resolve_with_extensions(&self, base_path: &Path) -> Result<PathBuf, DatasetError> {
        // First check if the path exists as-is
        if base_path.exists() {
            return Ok(base_path.to_path_buf());
        }

        // Try with extensions
        for ext in &self.extensions {
            let mut path_with_ext = base_path.to_path_buf();
            if !ext.is_empty() {
                let file_name = path_with_ext
                    .file_name()
                    .and_then(|n| n.to_str())
                    .unwrap_or("");
                path_with_ext.set_file_name(format!("{}{}", file_name, ext));
            }

            if path_with_ext.exists() {
                return Ok(path_with_ext);
            }
        }

        // Return the base path even if it doesn't exist (for new datasets)
        Ok(base_path.to_path_buf())
    }

    /// Infer dataset attributes from file extension.
    fn infer_attributes(&self, path: &Path) -> DatasetAttributes {
        let ext = path
            .extension()
            .and_then(|e| e.to_str())
            .unwrap_or("")
            .to_lowercase();

        match ext.as_str() {
            "cbl" | "cob" | "cpy" => DatasetAttributes::cobol_source(),
            "txt" | "lst" => DatasetAttributes {
                recfm: RecordFormat::Variable,
                lrecl: 255,
                blksize: 2550,
                ..Default::default()
            },
            "dat" => DatasetAttributes::default(),
            _ => DatasetAttributes::default(),
        }
    }

    /// Check if a dataset exists.
    pub fn exists(&self, dsn: &str) -> bool {
        let dsn_upper = dsn.to_uppercase();

        // Check catalog
        if self.entries.contains_key(&dsn_upper) {
            return true;
        }

        // Check filesystem
        if let Ok(path) = self.dsn_to_path(&dsn_upper) {
            return path.exists();
        }

        false
    }

    /// Delete a dataset.
    pub fn delete(&mut self, dsn: &str) -> Result<(), DatasetError> {
        let dsn_upper = dsn.to_uppercase();

        // Remove from catalog
        self.entries.remove(&dsn_upper);

        // Delete from filesystem
        if let Ok(path) = self.dsn_to_path(&dsn_upper) {
            if path.is_file() {
                std::fs::remove_file(&path).map_err(|e| DatasetError::IoError {
                    message: format!("Failed to delete {}: {}", path.display(), e),
                })?;
            } else if path.is_dir() {
                std::fs::remove_dir_all(&path).map_err(|e| DatasetError::IoError {
                    message: format!("Failed to delete {}: {}", path.display(), e),
                })?;
            }
        }

        Ok(())
    }

    /// List datasets matching a pattern.
    ///
    /// Checks both explicit catalog entries and scans the filesystem
    /// under the base directory for matching datasets.
    pub fn list(&self, pattern: &str) -> Vec<String> {
        let pattern_upper = pattern.to_uppercase();
        let mut results = Vec::new();

        // Check catalog entries
        for dsn in self.entries.keys() {
            if Self::matches_pattern(dsn, &pattern_upper) {
                results.push(dsn.clone());
            }
        }

        // Scan filesystem for matching datasets
        if self.base_dir.exists() && self.base_dir.is_dir() {
            self.scan_directory(&self.base_dir.clone(), "", &pattern_upper, &mut results);
        }

        results.sort();
        results.dedup();
        results
    }

    /// Recursively scan a directory to discover datasets.
    ///
    /// Builds DSN from path components separated by dots.
    #[allow(clippy::only_used_in_recursion)]
    fn scan_directory(&self, dir: &Path, prefix: &str, pattern: &str, results: &mut Vec<String>) {
        let entries = match std::fs::read_dir(dir) {
            Ok(e) => e,
            Err(_) => return,
        };

        for entry in entries.flatten() {
            let name = entry.file_name();
            let name_str = name.to_string_lossy().to_uppercase();

            // Skip hidden/metadata files
            if name_str.starts_with('.') || name_str.starts_with('_') {
                continue;
            }

            let dsn = if prefix.is_empty() {
                name_str.to_string()
            } else {
                format!("{}.{}", prefix, name_str)
            };

            let path = entry.path();
            if path.is_file() {
                // Strip known extensions for DSN
                let dsn_clean = strip_dataset_extension(&dsn);
                if Self::matches_pattern(&dsn_clean, pattern) && !results.contains(&dsn_clean) {
                    results.push(dsn_clean);
                }
            } else if path.is_dir() {
                // Check if this directory itself matches as a dataset (e.g., PDS)
                if Self::matches_pattern(&dsn, pattern) && !results.contains(&dsn) {
                    // Only add directories that look like datasets (contain _pds_directory or files)
                    if path.join("_pds_directory.json").exists() {
                        results.push(dsn.clone());
                    }
                }
                // Recurse into subdirectories
                self.scan_directory(&path, &dsn, pattern, results);
            }
        }
    }

    /// Save catalog entries to a persistent file.
    ///
    /// Writes all explicit catalog entries to `<base_dir>/.catalog.idx`
    /// in a simple line-based format: `DSN|PATH|RECFM|LRECL|BLKSIZE|IS_PDS`
    pub fn save(&self) -> Result<(), DatasetError> {
        let catalog_path = self.base_dir.join(".catalog.idx");
        std::fs::create_dir_all(&self.base_dir).map_err(|e| DatasetError::IoError {
            message: format!("Failed to create catalog directory: {}", e),
        })?;

        let mut lines = Vec::new();
        for entry in self.entries.values() {
            let recfm = match entry.attributes.recfm {
                RecordFormat::Fixed => "F",
                RecordFormat::FixedBlocked => "FB",
                RecordFormat::Variable => "V",
                RecordFormat::VariableBlocked => "VB",
                RecordFormat::Undefined => "U",
                RecordFormat::VariableSpanned => "VS",
                RecordFormat::VariableBlockedSpanned => "VBS",
            };
            lines.push(format!(
                "{}|{}|{}|{}|{}|{}",
                entry.dsn,
                entry.path.display(),
                recfm,
                entry.attributes.lrecl,
                entry.attributes.blksize,
                if entry.is_pds { "Y" } else { "N" },
            ));
        }

        lines.sort();
        let content = lines.join("\n") + "\n";
        std::fs::write(&catalog_path, content).map_err(|e| DatasetError::IoError {
            message: format!("Failed to write catalog: {}", e),
        })?;

        Ok(())
    }

    /// Load catalog entries from a persistent file.
    ///
    /// Reads entries from `<base_dir>/.catalog.idx` and populates the
    /// in-memory catalog. Existing entries are not cleared — loaded
    /// entries are merged in.
    pub fn load(&mut self) -> Result<usize, DatasetError> {
        let catalog_path = self.base_dir.join(".catalog.idx");
        if !catalog_path.exists() {
            return Ok(0);
        }

        let content = std::fs::read_to_string(&catalog_path).map_err(|e| DatasetError::IoError {
            message: format!("Failed to read catalog: {}", e),
        })?;

        let mut count = 0;
        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() {
                continue;
            }

            let parts: Vec<&str> = line.split('|').collect();
            if parts.len() < 6 {
                continue;
            }

            let dsn = parts[0].to_string();
            let path = PathBuf::from(parts[1]);
            let recfm = RecordFormat::parse(parts[2]).unwrap_or_default();
            let lrecl: u32 = parts[3].parse().unwrap_or(80);
            let blksize: u32 = parts[4].parse().unwrap_or(800);
            let is_pds = parts[5] == "Y";

            let entry = CatalogEntry {
                dsn: dsn.clone(),
                path,
                attributes: DatasetAttributes {
                    recfm,
                    lrecl,
                    blksize,
                    ..Default::default()
                },
                is_pds,
            };

            self.entries.insert(dsn.to_uppercase(), entry);
            count += 1;
        }

        Ok(count)
    }

    /// Check if a DSN matches a pattern (supports * wildcards).
    fn matches_pattern(dsn: &str, pattern: &str) -> bool {
        if pattern == "*" || pattern.is_empty() {
            return true;
        }

        let parts: Vec<&str> = pattern.split('*').collect();
        if parts.len() == 1 {
            // No wildcards
            return dsn == pattern;
        }

        let mut pos = 0;
        for (i, part) in parts.iter().enumerate() {
            if part.is_empty() {
                continue;
            }

            if let Some(found) = dsn[pos..].find(part) {
                if i == 0 && found != 0 {
                    // First part must match at start
                    return false;
                }
                pos += found + part.len();
            } else {
                return false;
            }
        }

        // If pattern doesn't end with *, DSN must end at current position
        if !pattern.ends_with('*') && pos != dsn.len() {
            return false;
        }

        true
    }

    /// Rename a dataset in the catalog.
    ///
    /// Renames the catalog entry and the underlying file/directory on disk.
    pub fn rename(&mut self, old_dsn: &str, new_dsn: &str) -> Result<(), DatasetError> {
        let old_upper = old_dsn.to_uppercase();
        let new_upper = new_dsn.to_uppercase();

        let entry = self.entries.remove(&old_upper).ok_or_else(|| {
            DatasetError::NotFound {
                name: old_upper.clone(),
            }
        })?;

        // Compute new path.
        let new_path = {
            let mut p = self.base_dir.clone();
            for component in new_upper.split('.') {
                p.push(component);
            }
            p
        };

        // Rename on filesystem.
        if entry.path.exists() {
            if let Some(parent) = new_path.parent() {
                std::fs::create_dir_all(parent).map_err(|e| DatasetError::IoError {
                    message: format!("Failed to create directory: {}", e),
                })?;
            }
            std::fs::rename(&entry.path, &new_path).map_err(|e| DatasetError::IoError {
                message: format!("Failed to rename: {}", e),
            })?;
        }

        let new_entry = CatalogEntry {
            dsn: new_upper.clone(),
            path: new_path,
            attributes: entry.attributes,
            is_pds: entry.is_pds,
        };
        self.entries.insert(new_upper, new_entry);

        Ok(())
    }

    /// Set migration status on a dataset.
    ///
    /// This is a metadata-only operation; the dataset remains on disk.
    pub fn set_migrated(&mut self, dsn: &str, _migrated: bool) -> Result<(), DatasetError> {
        let dsn_upper = dsn.to_uppercase();
        if !self.entries.contains_key(&dsn_upper) {
            // Try to look up from filesystem and add as entry.
            let dsref = self.lookup(&dsn_upper)?;
            if let Some(path) = dsref.path {
                self.entries.insert(dsn_upper.clone(), CatalogEntry {
                    dsn: dsn_upper,
                    path,
                    attributes: dsref.attributes,
                    is_pds: false,
                });
            }
        }
        // Migration is a metadata concept — on a real mainframe this would move data to tape.
        // For our emulation, this is a no-op (data stays on disk).
        Ok(())
    }

    /// Get the base directory.
    pub fn base_dir(&self) -> &Path {
        &self.base_dir
    }

    /// Set file extensions to try.
    pub fn set_extensions(&mut self, extensions: Vec<String>) {
        self.extensions = extensions;
    }
}

/// Strip known dataset file extensions from a DSN.
fn strip_dataset_extension(dsn: &str) -> String {
    for ext in &[".DAT", ".TXT", ".CBL", ".COB", ".CPY", ".VSAM", ".AIX", ".PATH"] {
        if let Some(stripped) = dsn.strip_suffix(ext) {
            return stripped.to_string();
        }
    }
    dsn.to_string()
}

impl Default for Catalog {
    fn default() -> Self {
        let base = std::env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("."))
            .join("datasets");
        Self::new(base)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_catalog_lookup() {
        let temp_dir = std::env::temp_dir().join("open_mainframe_catalog_test");
        std::fs::create_dir_all(&temp_dir).ok();

        // Create a test file
        let test_path = temp_dir.join("USER/DATA/FILE.dat");
        std::fs::create_dir_all(test_path.parent().unwrap()).ok();
        std::fs::write(&test_path, "test").ok();

        let catalog = Catalog::new(&temp_dir);
        let dataset = catalog.lookup("USER.DATA.FILE").unwrap();

        assert_eq!(dataset.dsn, "USER.DATA.FILE");
        assert!(dataset.path.unwrap().exists());

        // Cleanup
        std::fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn test_pattern_matching() {
        assert!(Catalog::matches_pattern("USER.DATA.FILE", "USER.DATA.FILE"));
        assert!(Catalog::matches_pattern("USER.DATA.FILE", "USER.*"));
        assert!(Catalog::matches_pattern("USER.DATA.FILE", "*.FILE"));
        assert!(Catalog::matches_pattern("USER.DATA.FILE", "USER.*.FILE"));
        assert!(Catalog::matches_pattern("USER.DATA.FILE", "*"));
        assert!(!Catalog::matches_pattern("USER.DATA.FILE", "OTHER.*"));
        assert!(!Catalog::matches_pattern(
            "USER.DATA.FILE",
            "USER.DATA.OTHER"
        ));
    }

    #[test]
    fn test_explicit_entry() {
        let mut catalog = Catalog::new("/datasets");

        catalog.add_entry(CatalogEntry {
            dsn: "MY.SPECIAL.DATASET".to_string(),
            path: PathBuf::from("/special/location/data.dat"),
            attributes: DatasetAttributes::default(),
            is_pds: false,
        });

        let dataset = catalog.lookup("MY.SPECIAL.DATASET").unwrap();
        assert_eq!(
            dataset.path.unwrap(),
            PathBuf::from("/special/location/data.dat")
        );
    }

    #[test]
    fn test_catalog_save_and_load() {
        let temp_dir = std::env::temp_dir().join("open_mainframe_catalog_save_test");
        std::fs::create_dir_all(&temp_dir).ok();

        // Create catalog with entries and save
        {
            let mut catalog = Catalog::new(&temp_dir);
            catalog.add_entry(CatalogEntry {
                dsn: "MY.TEST.DATA".to_string(),
                path: PathBuf::from("/datasets/MY/TEST/DATA.dat"),
                attributes: DatasetAttributes {
                    recfm: RecordFormat::FixedBlocked,
                    lrecl: 80,
                    blksize: 3200,
                    ..Default::default()
                },
                is_pds: false,
            });
            catalog.add_entry(CatalogEntry {
                dsn: "MY.SOURCE.LIB".to_string(),
                path: PathBuf::from("/datasets/MY/SOURCE/LIB"),
                attributes: DatasetAttributes {
                    recfm: RecordFormat::Variable,
                    lrecl: 255,
                    blksize: 2550,
                    ..Default::default()
                },
                is_pds: true,
            });
            catalog.save().unwrap();
        }

        // Load into fresh catalog
        {
            let mut catalog = Catalog::new(&temp_dir);
            let count = catalog.load().unwrap();
            assert_eq!(count, 2);

            // Verify entries loaded correctly
            let ds = catalog.lookup("MY.TEST.DATA").unwrap();
            assert_eq!(ds.dsn, "MY.TEST.DATA");

            let ds2 = catalog.lookup("MY.SOURCE.LIB").unwrap();
            assert_eq!(ds2.dsn, "MY.SOURCE.LIB");
        }

        std::fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn test_catalog_load_empty() {
        let temp_dir = std::env::temp_dir().join("open_mainframe_catalog_empty_test");
        std::fs::create_dir_all(&temp_dir).ok();

        let mut catalog = Catalog::new(&temp_dir);
        // No catalog file exists — should return 0
        let count = catalog.load().unwrap();
        assert_eq!(count, 0);

        std::fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn test_catalog_filesystem_scan() {
        let temp_dir = std::env::temp_dir().join("open_mainframe_catalog_scan_test");
        let _ = std::fs::remove_dir_all(&temp_dir);
        std::fs::create_dir_all(&temp_dir).ok();

        // Create some dataset files on disk
        let data_dir = temp_dir.join("USER/DATA");
        std::fs::create_dir_all(&data_dir).ok();
        std::fs::write(data_dir.join("FILE1.dat"), "data1").ok();
        std::fs::write(data_dir.join("FILE2.dat"), "data2").ok();

        let catalog = Catalog::new(&temp_dir);
        let results = catalog.list("USER.*");

        // Should find datasets via filesystem scan
        assert!(results.contains(&"USER.DATA.FILE1".to_string()));
        assert!(results.contains(&"USER.DATA.FILE2".to_string()));

        std::fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn test_catalog_list_deduplicates() {
        let temp_dir = std::env::temp_dir().join("open_mainframe_catalog_dedup_test");
        let _ = std::fs::remove_dir_all(&temp_dir);
        std::fs::create_dir_all(&temp_dir).ok();

        // Create a file on disk
        let data_dir = temp_dir.join("MY/DATASET");
        std::fs::create_dir_all(data_dir.parent().unwrap()).ok();
        std::fs::write(&data_dir, "content").ok();

        // Also add it as an explicit catalog entry
        let mut catalog = Catalog::new(&temp_dir);
        catalog.add_entry(CatalogEntry {
            dsn: "MY.DATASET".to_string(),
            path: data_dir.clone(),
            attributes: DatasetAttributes::default(),
            is_pds: false,
        });

        let results = catalog.list("MY.*");
        // Should not have duplicates
        let count = results.iter().filter(|s| *s == "MY.DATASET").count();
        assert_eq!(count, 1);

        std::fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn test_infer_attributes() {
        let catalog = Catalog::new("/datasets");

        let attrs = catalog.infer_attributes(Path::new("/test/file.cbl"));
        assert_eq!(attrs.lrecl, 80);
        assert_eq!(attrs.recfm, RecordFormat::FixedBlocked);

        let attrs = catalog.infer_attributes(Path::new("/test/file.txt"));
        assert!(attrs.recfm.is_variable());
    }
}
