//! Directory & File Metadata Operations (USS-103).
//!
//! Provides directory traversal and metadata operations:
//! - mkdir/rmdir
//! - opendir/readdir/closedir
//! - getcwd/chdir
//! - rename/unlink
//! - chmod/chown/utime

use std::collections::HashMap;
use std::time::SystemTime;

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors for directory and metadata operations.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, miette::Diagnostic)]
pub enum DirectoryError {
    /// Path not found.
    #[error("no such file or directory: {path}")]
    NotFound { path: String },

    /// Not a directory.
    #[error("not a directory: {path}")]
    NotADirectory { path: String },

    /// Directory not empty.
    #[error("directory not empty: {path}")]
    NotEmpty { path: String },

    /// File exists.
    #[error("file exists: {path}")]
    FileExists { path: String },

    /// Permission denied.
    #[error("permission denied: {path}")]
    PermissionDenied { path: String },

    /// Invalid directory handle.
    #[error("invalid directory handle: {handle}")]
    InvalidHandle { handle: u32 },
}

// ---------------------------------------------------------------------------
//  File Metadata
// ---------------------------------------------------------------------------

/// POSIX file metadata (returned by stat).
#[derive(Debug, Clone)]
pub struct FileMetadata {
    /// Inode number.
    pub ino: u64,
    /// File mode (permissions).
    pub mode: u32,
    /// Number of hard links.
    pub nlink: u32,
    /// Owner UID.
    pub uid: u32,
    /// Owner GID.
    pub gid: u32,
    /// File size.
    pub size: u64,
    /// Last access time.
    pub atime: SystemTime,
    /// Last modification time.
    pub mtime: SystemTime,
    /// Last status change time.
    pub ctime: SystemTime,
    /// Is directory.
    pub is_dir: bool,
    /// Is regular file.
    pub is_file: bool,
    /// Is symbolic link.
    pub is_symlink: bool,
}

// ---------------------------------------------------------------------------
//  Directory Handle
// ---------------------------------------------------------------------------

/// An open directory handle for iteration.
#[derive(Debug)]
pub struct DirHandle {
    /// Handle ID.
    pub handle: u32,
    /// Path.
    pub path: String,
    /// Entries in this directory.
    pub entries: Vec<DirEntry>,
    /// Current position.
    pub position: usize,
}

/// A directory entry returned by readdir.
#[derive(Debug, Clone)]
pub struct DirEntry {
    /// Entry name.
    pub name: String,
    /// Inode number.
    pub ino: u64,
    /// Is directory.
    pub is_dir: bool,
}

// ---------------------------------------------------------------------------
//  Time Value
// ---------------------------------------------------------------------------

/// Time value for utime().
#[derive(Debug, Clone)]
pub struct TimeValue {
    /// Access time.
    pub atime: SystemTime,
    /// Modification time.
    pub mtime: SystemTime,
}

// ---------------------------------------------------------------------------
//  Directory Manager
// ---------------------------------------------------------------------------

/// Manages directory operations and metadata.
#[derive(Debug)]
pub struct DirectoryManager {
    /// Directory tree: path -> list of (name, is_dir).
    tree: HashMap<String, Vec<DirEntry>>,
    /// File metadata by path.
    metadata: HashMap<String, FileMetadata>,
    /// Next inode.
    next_ino: u64,
    /// Current working directory.
    cwd: String,
    /// Open directory handles.
    dir_handles: HashMap<u32, DirHandle>,
    /// Next handle ID.
    next_handle: u32,
}

impl DirectoryManager {
    /// Create a new directory manager with root.
    pub fn new() -> Self {
        let now = SystemTime::now();
        let mut mgr = Self {
            tree: HashMap::new(),
            metadata: HashMap::new(),
            next_ino: 2,
            cwd: "/".to_string(),
            dir_handles: HashMap::new(),
            next_handle: 1,
        };

        // Create root.
        let root_meta = FileMetadata {
            ino: 1,
            mode: 0o755,
            nlink: 2,
            uid: 0,
            gid: 0,
            size: 0,
            atime: now,
            mtime: now,
            ctime: now,
            is_dir: true,
            is_file: false,
            is_symlink: false,
        };
        mgr.metadata.insert("/".to_string(), root_meta);
        mgr.tree.insert(
            "/".to_string(),
            vec![
                DirEntry {
                    name: ".".to_string(),
                    ino: 1,
                    is_dir: true,
                },
                DirEntry {
                    name: "..".to_string(),
                    ino: 1,
                    is_dir: true,
                },
            ],
        );
        mgr
    }

    /// Get current working directory.
    pub fn getcwd(&self) -> &str {
        &self.cwd
    }

    /// Change current working directory.
    pub fn chdir(&mut self, path: &str) -> Result<(), DirectoryError> {
        let resolved = self.resolve(path);
        if !self.metadata.contains_key(&resolved) {
            return Err(DirectoryError::NotFound { path: resolved });
        }
        let meta = &self.metadata[&resolved];
        if !meta.is_dir {
            return Err(DirectoryError::NotADirectory { path: resolved });
        }
        self.cwd = resolved;
        Ok(())
    }

    /// mkdir — create a directory.
    pub fn mkdir(
        &mut self,
        path: &str,
        mode: u32,
        uid: u32,
        gid: u32,
    ) -> Result<(), DirectoryError> {
        let resolved = self.resolve(path);
        if self.metadata.contains_key(&resolved) {
            return Err(DirectoryError::FileExists { path: resolved });
        }

        let (parent, name) = self.split_path(&resolved);
        if !self.metadata.contains_key(&parent) {
            return Err(DirectoryError::NotFound { path: parent });
        }

        let now = SystemTime::now();
        let ino = self.alloc_ino();

        let meta = FileMetadata {
            ino,
            mode,
            nlink: 2,
            uid,
            gid,
            size: 0,
            atime: now,
            mtime: now,
            ctime: now,
            is_dir: true,
            is_file: false,
            is_symlink: false,
        };
        self.metadata.insert(resolved.clone(), meta);

        // Create . and .. entries.
        let parent_ino = self.metadata[&parent].ino;
        self.tree.insert(
            resolved.clone(),
            vec![
                DirEntry {
                    name: ".".to_string(),
                    ino,
                    is_dir: true,
                },
                DirEntry {
                    name: "..".to_string(),
                    ino: parent_ino,
                    is_dir: true,
                },
            ],
        );

        // Add to parent.
        let entries = self.tree.entry(parent).or_default();
        entries.push(DirEntry {
            name: name.to_string(),
            ino,
            is_dir: true,
        });

        Ok(())
    }

    /// rmdir — remove an empty directory.
    pub fn rmdir(&mut self, path: &str) -> Result<(), DirectoryError> {
        let resolved = self.resolve(path);
        let meta = self
            .metadata
            .get(&resolved)
            .ok_or(DirectoryError::NotFound {
                path: resolved.clone(),
            })?;

        if !meta.is_dir {
            return Err(DirectoryError::NotADirectory {
                path: resolved.clone(),
            });
        }

        // Check if empty (only . and ..).
        if let Some(entries) = self.tree.get(&resolved) {
            let non_dot_entries = entries
                .iter()
                .filter(|e| e.name != "." && e.name != "..")
                .count();
            if non_dot_entries > 0 {
                return Err(DirectoryError::NotEmpty { path: resolved });
            }
        }

        // Remove from parent.
        let (parent, name) = self.split_path(&resolved);
        if let Some(entries) = self.tree.get_mut(&parent) {
            entries.retain(|e| e.name != name);
        }

        self.tree.remove(&resolved);
        self.metadata.remove(&resolved);
        Ok(())
    }

    /// opendir — open a directory for reading.
    pub fn opendir(&mut self, path: &str) -> Result<u32, DirectoryError> {
        let resolved = self.resolve(path);
        let meta = self
            .metadata
            .get(&resolved)
            .ok_or(DirectoryError::NotFound {
                path: resolved.clone(),
            })?;

        if !meta.is_dir {
            return Err(DirectoryError::NotADirectory {
                path: resolved.clone(),
            });
        }

        let entries = self.tree.get(&resolved).cloned().unwrap_or_default();
        let handle = self.next_handle;
        self.next_handle += 1;

        self.dir_handles.insert(
            handle,
            DirHandle {
                handle,
                path: resolved,
                entries,
                position: 0,
            },
        );

        Ok(handle)
    }

    /// readdir — read next entry from directory handle.
    pub fn readdir(&mut self, handle: u32) -> Result<Option<DirEntry>, DirectoryError> {
        let dh = self
            .dir_handles
            .get_mut(&handle)
            .ok_or(DirectoryError::InvalidHandle { handle })?;

        if dh.position >= dh.entries.len() {
            return Ok(None);
        }

        let entry = dh.entries[dh.position].clone();
        dh.position += 1;
        Ok(Some(entry))
    }

    /// closedir — close a directory handle.
    pub fn closedir(&mut self, handle: u32) -> Result<(), DirectoryError> {
        self.dir_handles
            .remove(&handle)
            .ok_or(DirectoryError::InvalidHandle { handle })?;
        Ok(())
    }

    /// Create a regular file entry (for metadata tracking).
    pub fn create_file(
        &mut self,
        path: &str,
        mode: u32,
        uid: u32,
        gid: u32,
    ) -> Result<u64, DirectoryError> {
        let resolved = self.resolve(path);
        if self.metadata.contains_key(&resolved) {
            return Err(DirectoryError::FileExists { path: resolved });
        }

        let (parent, name) = self.split_path(&resolved);
        let name = name.to_string();
        if !self.metadata.contains_key(&parent) {
            return Err(DirectoryError::NotFound {
                path: parent.clone(),
            });
        }

        let now = SystemTime::now();
        let ino = self.alloc_ino();

        let meta = FileMetadata {
            ino,
            mode,
            nlink: 1,
            uid,
            gid,
            size: 0,
            atime: now,
            mtime: now,
            ctime: now,
            is_dir: false,
            is_file: true,
            is_symlink: false,
        };
        self.metadata.insert(resolved, meta);

        let entries = self.tree.entry(parent).or_default();
        entries.push(DirEntry {
            name,
            ino,
            is_dir: false,
        });

        Ok(ino)
    }

    /// rename — atomically rename a file.
    pub fn rename(&mut self, old: &str, new: &str) -> Result<(), DirectoryError> {
        let old_resolved = self.resolve(old);
        let new_resolved = self.resolve(new);

        if !self.metadata.contains_key(&old_resolved) {
            return Err(DirectoryError::NotFound {
                path: old_resolved,
            });
        }

        // Move metadata.
        if let Some(meta) = self.metadata.remove(&old_resolved) {
            let mut meta = meta;
            meta.ctime = SystemTime::now();
            self.metadata.insert(new_resolved.clone(), meta);
        }

        // Move directory tree entry if it's a directory.
        if let Some(entries) = self.tree.remove(&old_resolved) {
            self.tree.insert(new_resolved.clone(), entries);
        }

        // Update parent directory entries.
        let (old_parent, old_name) = self.split_path(&old_resolved);
        let (new_parent, new_name) = self.split_path(&new_resolved);

        if let Some(entries) = self.tree.get_mut(&old_parent) {
            entries.retain(|e| e.name != old_name);
        }

        let ino = self.metadata[&new_resolved].ino;
        let is_dir = self.metadata[&new_resolved].is_dir;
        let entries = self.tree.entry(new_parent).or_default();
        entries.push(DirEntry {
            name: new_name.to_string(),
            ino,
            is_dir,
        });

        Ok(())
    }

    /// unlink — remove a file.
    pub fn unlink(&mut self, path: &str) -> Result<(), DirectoryError> {
        let resolved = self.resolve(path);
        let meta = self
            .metadata
            .get(&resolved)
            .ok_or(DirectoryError::NotFound {
                path: resolved.clone(),
            })?;

        if meta.is_dir {
            return Err(DirectoryError::NotADirectory {
                path: resolved.clone(),
            });
        }

        let nlink = meta.nlink;

        // Remove from parent.
        let (parent, name) = self.split_path(&resolved);
        if let Some(entries) = self.tree.get_mut(&parent) {
            entries.retain(|e| e.name != name);
        }

        if nlink <= 1 {
            self.metadata.remove(&resolved);
        } else if let Some(meta) = self.metadata.get_mut(&resolved) {
            meta.nlink -= 1;
            meta.ctime = SystemTime::now();
        }

        Ok(())
    }

    /// chmod — change file permissions.
    pub fn chmod(&mut self, path: &str, mode: u32) -> Result<(), DirectoryError> {
        let resolved = self.resolve(path);
        let meta = self
            .metadata
            .get_mut(&resolved)
            .ok_or(DirectoryError::NotFound { path: resolved })?;
        meta.mode = mode;
        meta.ctime = SystemTime::now();
        Ok(())
    }

    /// chown — change file owner.
    pub fn chown(
        &mut self,
        path: &str,
        uid: u32,
        gid: u32,
        caller_uid: u32,
    ) -> Result<(), DirectoryError> {
        let resolved = self.resolve(path);
        let meta = self
            .metadata
            .get_mut(&resolved)
            .ok_or(DirectoryError::NotFound {
                path: resolved.clone(),
            })?;

        // Only root can chown.
        if caller_uid != 0 {
            return Err(DirectoryError::PermissionDenied { path: resolved });
        }

        meta.uid = uid;
        meta.gid = gid;
        meta.ctime = SystemTime::now();
        Ok(())
    }

    /// utime — change file timestamps.
    pub fn utime(&mut self, path: &str, times: &TimeValue) -> Result<(), DirectoryError> {
        let resolved = self.resolve(path);
        let meta = self
            .metadata
            .get_mut(&resolved)
            .ok_or(DirectoryError::NotFound { path: resolved })?;
        meta.atime = times.atime;
        meta.mtime = times.mtime;
        meta.ctime = SystemTime::now();
        Ok(())
    }

    /// Get file metadata (stat).
    pub fn stat(&self, path: &str) -> Result<&FileMetadata, DirectoryError> {
        let resolved = self.resolve(path);
        self.metadata
            .get(&resolved)
            .ok_or(DirectoryError::NotFound { path: resolved })
    }

    // --- internal helpers ---

    fn alloc_ino(&mut self) -> u64 {
        let ino = self.next_ino;
        self.next_ino += 1;
        ino
    }

    fn resolve(&self, path: &str) -> String {
        if path.starts_with('/') {
            normalize(path)
        } else {
            let full = format!("{}/{}", self.cwd.trim_end_matches('/'), path);
            normalize(&full)
        }
    }

    fn split_path<'a>(&self, path: &'a str) -> (String, &'a str) {
        if path == "/" {
            return ("/".to_string(), "");
        }
        let trimmed = path.trim_end_matches('/');
        match trimmed.rfind('/') {
            Some(0) => ("/".to_string(), &trimmed[1..]),
            Some(pos) => (trimmed[..pos].to_string(), &trimmed[pos + 1..]),
            None => ("/".to_string(), trimmed),
        }
    }
}

impl Default for DirectoryManager {
    fn default() -> Self {
        Self::new()
    }
}

fn normalize(path: &str) -> String {
    if path == "/" {
        return "/".to_string();
    }
    let trimmed = path.trim_end_matches('/');
    if trimmed.is_empty() {
        "/".to_string()
    } else {
        trimmed.to_string()
    }
}

// ===========================================================================
//  Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mkdir_and_stat() {
        let mut mgr = DirectoryManager::new();
        mgr.mkdir("/data", 0o755, 0, 0).unwrap();
        let meta = mgr.stat("/data").unwrap();
        assert!(meta.is_dir);
        assert_eq!(meta.mode, 0o755);
    }

    #[test]
    fn test_mkdir_already_exists() {
        let mut mgr = DirectoryManager::new();
        mgr.mkdir("/data", 0o755, 0, 0).unwrap();
        let err = mgr.mkdir("/data", 0o755, 0, 0).unwrap_err();
        assert!(matches!(err, DirectoryError::FileExists { .. }));
    }

    #[test]
    fn test_rmdir_empty() {
        let mut mgr = DirectoryManager::new();
        mgr.mkdir("/emptydir", 0o755, 0, 0).unwrap();
        mgr.rmdir("/emptydir").unwrap();
        assert!(mgr.stat("/emptydir").is_err());
    }

    #[test]
    fn test_rmdir_not_empty() {
        let mut mgr = DirectoryManager::new();
        mgr.mkdir("/data", 0o755, 0, 0).unwrap();
        mgr.create_file("/data/file.txt", 0o644, 0, 0).unwrap();
        let err = mgr.rmdir("/data").unwrap_err();
        assert!(matches!(err, DirectoryError::NotEmpty { .. }));
    }

    #[test]
    fn test_opendir_readdir_closedir() {
        let mut mgr = DirectoryManager::new();
        mgr.mkdir("/data", 0o755, 0, 0).unwrap();
        mgr.create_file("/data/a.txt", 0o644, 0, 0).unwrap();
        mgr.create_file("/data/b.txt", 0o644, 0, 0).unwrap();

        let handle = mgr.opendir("/data").unwrap();
        let mut names = Vec::new();
        while let Ok(Some(entry)) = mgr.readdir(handle) {
            names.push(entry.name.clone());
        }
        assert!(names.contains(&".".to_string()));
        assert!(names.contains(&"..".to_string()));
        assert!(names.contains(&"a.txt".to_string()));
        assert!(names.contains(&"b.txt".to_string()));

        mgr.closedir(handle).unwrap();
    }

    #[test]
    fn test_getcwd_chdir() {
        let mut mgr = DirectoryManager::new();
        assert_eq!(mgr.getcwd(), "/");
        mgr.mkdir("/data", 0o755, 0, 0).unwrap();
        mgr.chdir("/data").unwrap();
        assert_eq!(mgr.getcwd(), "/data");
    }

    #[test]
    fn test_chdir_not_found() {
        let mut mgr = DirectoryManager::new();
        let err = mgr.chdir("/nonexistent").unwrap_err();
        assert!(matches!(err, DirectoryError::NotFound { .. }));
    }

    #[test]
    fn test_rename() {
        let mut mgr = DirectoryManager::new();
        mgr.create_file("/old.txt", 0o644, 0, 0).unwrap();
        mgr.rename("/old.txt", "/new.txt").unwrap();
        assert!(mgr.stat("/old.txt").is_err());
        assert!(mgr.stat("/new.txt").is_ok());
    }

    #[test]
    fn test_unlink() {
        let mut mgr = DirectoryManager::new();
        mgr.create_file("/temp.txt", 0o644, 0, 0).unwrap();
        mgr.unlink("/temp.txt").unwrap();
        assert!(mgr.stat("/temp.txt").is_err());
    }

    #[test]
    fn test_chmod() {
        let mut mgr = DirectoryManager::new();
        mgr.create_file("/file.txt", 0o644, 0, 0).unwrap();
        mgr.chmod("/file.txt", 0o755).unwrap();
        let meta = mgr.stat("/file.txt").unwrap();
        assert_eq!(meta.mode, 0o755);
    }

    #[test]
    fn test_chown_by_root() {
        let mut mgr = DirectoryManager::new();
        mgr.create_file("/file.txt", 0o644, 0, 0).unwrap();
        mgr.chown("/file.txt", 100, 200, 0).unwrap();
        let meta = mgr.stat("/file.txt").unwrap();
        assert_eq!(meta.uid, 100);
        assert_eq!(meta.gid, 200);
    }

    #[test]
    fn test_chown_permission_denied() {
        let mut mgr = DirectoryManager::new();
        mgr.create_file("/file.txt", 0o644, 0, 0).unwrap();
        let err = mgr.chown("/file.txt", 100, 200, 500).unwrap_err();
        assert!(matches!(err, DirectoryError::PermissionDenied { .. }));
    }

    #[test]
    fn test_utime() {
        let mut mgr = DirectoryManager::new();
        mgr.create_file("/file.txt", 0o644, 0, 0).unwrap();
        let tv = TimeValue {
            atime: SystemTime::UNIX_EPOCH,
            mtime: SystemTime::UNIX_EPOCH,
        };
        mgr.utime("/file.txt", &tv).unwrap();
        let meta = mgr.stat("/file.txt").unwrap();
        assert_eq!(meta.atime, SystemTime::UNIX_EPOCH);
        assert_eq!(meta.mtime, SystemTime::UNIX_EPOCH);
    }
}
