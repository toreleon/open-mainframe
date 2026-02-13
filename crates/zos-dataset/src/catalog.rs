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
    pub fn list(&self, pattern: &str) -> Vec<String> {
        let pattern_upper = pattern.to_uppercase();
        let mut results = Vec::new();

        // Check catalog entries
        for dsn in self.entries.keys() {
            if Self::matches_pattern(dsn, &pattern_upper) {
                results.push(dsn.clone());
            }
        }

        // TODO: Also scan filesystem for matching datasets

        results.sort();
        results
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

    /// Get the base directory.
    pub fn base_dir(&self) -> &Path {
        &self.base_dir
    }

    /// Set file extensions to try.
    pub fn set_extensions(&mut self, extensions: Vec<String>) {
        self.extensions = extensions;
    }
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
        let temp_dir = std::env::temp_dir().join("zos_catalog_test");
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
    fn test_infer_attributes() {
        let catalog = Catalog::new("/datasets");

        let attrs = catalog.infer_attributes(Path::new("/test/file.cbl"));
        assert_eq!(attrs.lrecl, 80);
        assert_eq!(attrs.recfm, RecordFormat::FixedBlocked);

        let attrs = catalog.infer_attributes(Path::new("/test/file.txt"));
        assert!(attrs.recfm.is_variable());
    }
}
