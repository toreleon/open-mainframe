//! zFS Hierarchical File System (USS-102).
//!
//! Provides a POSIX-compliant hierarchical file system:
//! - VFS layer and mount table
//! - Inode table and file metadata
//! - File I/O (open/close/read/write/lseek)
//! - POSIX permission checks
//! - Symbolic and hard links
//! - File locking (fcntl/flock)
//! - MVS dataset bridge (//'DSN' syntax)

use std::collections::HashMap;
use std::time::SystemTime;

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors for file system operations.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, miette::Diagnostic)]
pub enum FsError {
    /// File not found (ENOENT).
    #[error("no such file or directory: {path}")]
    NotFound { path: String },

    /// Permission denied (EACCES).
    #[error("permission denied: {path}")]
    PermissionDenied { path: String },

    /// File exists (EEXIST).
    #[error("file exists: {path}")]
    FileExists { path: String },

    /// Not a directory (ENOTDIR).
    #[error("not a directory: {path}")]
    NotADirectory { path: String },

    /// Is a directory (EISDIR).
    #[error("is a directory: {path}")]
    IsADirectory { path: String },

    /// Directory not empty (ENOTEMPTY).
    #[error("directory not empty: {path}")]
    DirectoryNotEmpty { path: String },

    /// Bad file descriptor (EBADF).
    #[error("bad file descriptor: {fd}")]
    BadFileDescriptor { fd: u32 },

    /// Resource busy / lock conflict (EAGAIN).
    #[error("resource temporarily unavailable (lock conflict)")]
    WouldBlock,

    /// Invalid argument (EINVAL).
    #[error("invalid argument: {detail}")]
    InvalidArgument { detail: String },

    /// Too many open files.
    #[error("too many open files: limit {limit}")]
    TooManyOpenFiles { limit: u32 },

    /// Cross-device link.
    #[error("cross-device link")]
    CrossDeviceLink,

    /// MVS dataset not found.
    #[error("MVS dataset not found: {dsn}")]
    DatasetNotFound { dsn: String },
}

// ---------------------------------------------------------------------------
//  File Type
// ---------------------------------------------------------------------------

/// Type of file system entry.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileType {
    RegularFile,
    Directory,
    SymbolicLink,
    Fifo,
    Socket,
    CharDevice,
    BlockDevice,
}

impl std::fmt::Display for FileType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RegularFile => write!(f, "-"),
            Self::Directory => write!(f, "d"),
            Self::SymbolicLink => write!(f, "l"),
            Self::Fifo => write!(f, "p"),
            Self::Socket => write!(f, "s"),
            Self::CharDevice => write!(f, "c"),
            Self::BlockDevice => write!(f, "b"),
        }
    }
}

// ---------------------------------------------------------------------------
//  Codepage Tag
// ---------------------------------------------------------------------------

/// File codepage tag for auto-conversion.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodepageTag {
    /// CCSID (e.g. 819 = ISO8859-1, 1047 = IBM-1047).
    pub ccsid: u16,
    /// Whether the file is tagged as text (vs binary).
    pub text_flag: bool,
}

// ---------------------------------------------------------------------------
//  Inode
// ---------------------------------------------------------------------------

/// A zFS inode representing file metadata.
#[derive(Debug, Clone)]
pub struct Inode {
    /// Inode number.
    pub ino: u64,
    /// File mode (type + permissions in octal, e.g. 0o100644).
    pub mode: u32,
    /// Number of hard links.
    pub nlink: u32,
    /// Owner user ID.
    pub uid: u32,
    /// Owner group ID.
    pub gid: u32,
    /// File size in bytes.
    pub size: u64,
    /// Last access time.
    pub atime: SystemTime,
    /// Last modification time.
    pub mtime: SystemTime,
    /// Last status change time.
    pub ctime: SystemTime,
    /// File type.
    pub file_type: FileType,
    /// Codepage tag for auto-conversion.
    pub tag: Option<CodepageTag>,
    /// Symlink target (only valid for SymbolicLink type).
    pub symlink_target: Option<String>,
    /// File data (simplified in-memory storage).
    pub data: Vec<u8>,
}

impl Inode {
    /// Create a new inode.
    pub fn new(ino: u64, file_type: FileType, mode: u32, uid: u32, gid: u32) -> Self {
        let now = SystemTime::now();
        Self {
            ino,
            mode,
            nlink: 1,
            uid,
            gid,
            size: 0,
            atime: now,
            mtime: now,
            ctime: now,
            file_type,
            tag: None,
            symlink_target: None,
            data: Vec::new(),
        }
    }

    /// Get the permission bits (lower 12 bits of mode).
    pub fn permissions(&self) -> u32 {
        self.mode & 0o7777
    }

    /// Check if setuid bit is set.
    pub fn is_setuid(&self) -> bool {
        self.mode & 0o4000 != 0
    }

    /// Check if setgid bit is set.
    pub fn is_setgid(&self) -> bool {
        self.mode & 0o2000 != 0
    }
}

// ---------------------------------------------------------------------------
//  Open Flags
// ---------------------------------------------------------------------------

/// Flags for open().
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpenFlags {
    pub read: bool,
    pub write: bool,
    pub create: bool,
    pub exclusive: bool,
    pub truncate: bool,
    pub append: bool,
}

impl OpenFlags {
    pub fn read_only() -> Self {
        Self {
            read: true,
            write: false,
            create: false,
            exclusive: false,
            truncate: false,
            append: false,
        }
    }

    pub fn read_write() -> Self {
        Self {
            read: true,
            write: true,
            create: false,
            exclusive: false,
            truncate: false,
            append: false,
        }
    }

    pub fn write_create() -> Self {
        Self {
            read: false,
            write: true,
            create: true,
            exclusive: false,
            truncate: false,
            append: false,
        }
    }
}

// ---------------------------------------------------------------------------
//  Seek Whence
// ---------------------------------------------------------------------------

/// Whence parameter for lseek().
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SeekWhence {
    /// From beginning of file.
    Set,
    /// From current position.
    Current,
    /// From end of file.
    End,
}

// ---------------------------------------------------------------------------
//  File Lock
// ---------------------------------------------------------------------------

/// Type of file lock.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LockType {
    /// Shared read lock.
    ReadLock,
    /// Exclusive write lock.
    WriteLock,
    /// Unlock.
    Unlock,
}

/// An advisory file lock.
#[derive(Debug, Clone)]
pub struct FileLock {
    /// Type of lock.
    pub lock_type: LockType,
    /// Starting offset.
    pub start: u64,
    /// Length (0 means to end of file).
    pub length: u64,
    /// PID of lock holder.
    pub pid: u32,
}

// ---------------------------------------------------------------------------
//  Open File Description
// ---------------------------------------------------------------------------

/// An open file description tracked by the VFS.
#[derive(Debug, Clone)]
pub struct OpenFile {
    /// File descriptor number.
    pub fd: u32,
    /// Inode number this fd refers to.
    pub inode: u64,
    /// Current file offset.
    pub offset: u64,
    /// Open flags.
    pub flags: OpenFlags,
}

// ---------------------------------------------------------------------------
//  Mount Entry
// ---------------------------------------------------------------------------

/// A mount table entry.
#[derive(Debug, Clone)]
pub struct MountEntry {
    /// Filesystem name (e.g., "OMVS.ROOT").
    pub filesystem: String,
    /// Mount point path.
    pub mountpoint: String,
    /// Filesystem type (e.g., "ZFS", "TFS").
    pub fstype: String,
    /// Read-write or read-only.
    pub read_only: bool,
}

// ---------------------------------------------------------------------------
//  Directory Entry
// ---------------------------------------------------------------------------

/// A directory entry.
#[derive(Debug, Clone)]
pub struct DirectoryEntry {
    /// Entry name.
    pub name: String,
    /// Inode number.
    pub ino: u64,
    /// File type.
    pub file_type: FileType,
}

// ---------------------------------------------------------------------------
//  MVS Dataset Reference
// ---------------------------------------------------------------------------

/// A parsed MVS dataset reference from //'DSN' syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MvsDatasetRef {
    /// Dataset name.
    pub dataset_name: String,
    /// PDS member name (if applicable).
    pub member: Option<String>,
}

impl MvsDatasetRef {
    /// Parse //'DSN.NAME' or //'DSN.NAME(MEMBER)' syntax.
    pub fn parse(path: &str) -> Option<Self> {
        let trimmed = path.trim();
        if !trimmed.starts_with("//'") || !trimmed.ends_with('\'') {
            return None;
        }
        let inner = &trimmed[3..trimmed.len() - 1];
        if let Some(paren_start) = inner.find('(') {
            if inner.ends_with(')') {
                let dsn = inner[..paren_start].to_string();
                let member = inner[paren_start + 1..inner.len() - 1].to_string();
                Some(Self {
                    dataset_name: dsn,
                    member: Some(member),
                })
            } else {
                None
            }
        } else {
            Some(Self {
                dataset_name: inner.to_string(),
                member: None,
            })
        }
    }
}

// ---------------------------------------------------------------------------
//  Permission Checker
// ---------------------------------------------------------------------------

/// Check POSIX file permissions.
pub fn check_permission(
    inode: &Inode,
    uid: u32,
    gid: u32,
    want_read: bool,
    want_write: bool,
    want_exec: bool,
) -> bool {
    // Superuser can do anything.
    if uid == 0 {
        return true;
    }

    let perms = inode.permissions();
    let (r, w, x) = if uid == inode.uid {
        // Owner bits.
        ((perms >> 6) & 4 != 0, (perms >> 6) & 2 != 0, (perms >> 6) & 1 != 0)
    } else if gid == inode.gid {
        // Group bits.
        ((perms >> 3) & 4 != 0, (perms >> 3) & 2 != 0, (perms >> 3) & 1 != 0)
    } else {
        // Other bits.
        (perms & 4 != 0, perms & 2 != 0, perms & 1 != 0)
    };

    (!want_read || r) && (!want_write || w) && (!want_exec || x)
}

// ---------------------------------------------------------------------------
//  ZFS File System
// ---------------------------------------------------------------------------

/// The zFS file system implementation.
#[derive(Debug)]
pub struct Zfs {
    /// Inode table.
    inodes: HashMap<u64, Inode>,
    /// Next inode number.
    next_ino: u64,
    /// Directory entries: parent_ino -> Vec<DirectoryEntry>.
    directories: HashMap<u64, Vec<DirectoryEntry>>,
    /// Path-to-inode cache.
    path_cache: HashMap<String, u64>,
    /// Open file table.
    open_files: HashMap<u32, OpenFile>,
    /// Next file descriptor number.
    next_fd: u32,
    /// Maximum open files per process.
    max_files: u32,
    /// Mount table.
    mounts: Vec<MountEntry>,
    /// Advisory file locks: inode -> Vec<FileLock>.
    locks: HashMap<u64, Vec<FileLock>>,
}

impl Zfs {
    /// Create a new zFS with a root directory.
    pub fn new(max_files: u32) -> Self {
        let mut fs = Self {
            inodes: HashMap::new(),
            next_ino: 2,
            directories: HashMap::new(),
            path_cache: HashMap::new(),
            open_files: HashMap::new(),
            next_fd: 3, // 0=stdin, 1=stdout, 2=stderr
            max_files,
            mounts: Vec::new(),
            locks: HashMap::new(),
        };

        // Create root inode (ino=1).
        let root = Inode::new(1, FileType::Directory, 0o755, 0, 0);
        fs.inodes.insert(1, root);
        fs.directories.insert(
            1,
            vec![
                DirectoryEntry {
                    name: ".".to_string(),
                    ino: 1,
                    file_type: FileType::Directory,
                },
                DirectoryEntry {
                    name: "..".to_string(),
                    ino: 1,
                    file_type: FileType::Directory,
                },
            ],
        );
        fs.path_cache.insert("/".to_string(), 1);
        fs
    }

    /// Mount a filesystem.
    pub fn mount(&mut self, entry: MountEntry) {
        self.mounts.push(entry);
    }

    /// Get the mount table.
    pub fn mount_table(&self) -> &[MountEntry] {
        &self.mounts
    }

    /// Resolve a path to an inode number.
    pub fn resolve_path(&self, path: &str) -> Result<u64, FsError> {
        // Check cache first.
        if let Some(&ino) = self.path_cache.get(path) {
            return Ok(ino);
        }

        // Walk the path from root.
        let normalized = normalize_path(path);
        if let Some(&ino) = self.path_cache.get(&normalized) {
            return Ok(ino);
        }

        Err(FsError::NotFound {
            path: path.to_string(),
        })
    }

    /// Get an inode by number.
    pub fn get_inode(&self, ino: u64) -> Option<&Inode> {
        self.inodes.get(&ino)
    }

    /// Allocate a new inode number.
    fn alloc_ino(&mut self) -> u64 {
        let ino = self.next_ino;
        self.next_ino += 1;
        ino
    }

    /// Allocate a new file descriptor.
    fn alloc_fd(&mut self) -> Result<u32, FsError> {
        if self.open_files.len() as u32 >= self.max_files {
            return Err(FsError::TooManyOpenFiles {
                limit: self.max_files,
            });
        }
        let fd = self.next_fd;
        self.next_fd += 1;
        Ok(fd)
    }

    /// Create a file at the given path.
    pub fn create_file(
        &mut self,
        path: &str,
        mode: u32,
        uid: u32,
        gid: u32,
    ) -> Result<u64, FsError> {
        let normalized = normalize_path(path);
        if self.path_cache.contains_key(&normalized) {
            return Err(FsError::FileExists {
                path: normalized,
            });
        }

        let (parent_path, name) = split_path(&normalized);
        let parent_ino = self.resolve_path(&parent_path)?;

        // Verify parent is a directory.
        let parent_inode = self.inodes.get(&parent_ino).ok_or(FsError::NotFound {
            path: parent_path.clone(),
        })?;
        if parent_inode.file_type != FileType::Directory {
            return Err(FsError::NotADirectory {
                path: parent_path,
            });
        }

        let ino = self.alloc_ino();
        let inode = Inode::new(ino, FileType::RegularFile, mode, uid, gid);
        self.inodes.insert(ino, inode);
        self.path_cache.insert(normalized.clone(), ino);

        // Add to parent directory.
        let entries = self.directories.entry(parent_ino).or_default();
        entries.push(DirectoryEntry {
            name: name.to_string(),
            ino,
            file_type: FileType::RegularFile,
        });

        Ok(ino)
    }

    /// Create a directory at the given path.
    pub fn mkdir(
        &mut self,
        path: &str,
        mode: u32,
        uid: u32,
        gid: u32,
    ) -> Result<u64, FsError> {
        let normalized = normalize_path(path);
        if self.path_cache.contains_key(&normalized) {
            return Err(FsError::FileExists {
                path: normalized,
            });
        }

        let (parent_path, name) = split_path(&normalized);
        let parent_ino = self.resolve_path(&parent_path)?;

        let ino = self.alloc_ino();
        let mut inode = Inode::new(ino, FileType::Directory, mode, uid, gid);
        inode.nlink = 2; // . and ..

        // Increment parent nlink.
        if let Some(pinode) = self.inodes.get_mut(&parent_ino) {
            pinode.nlink += 1;
        }

        self.inodes.insert(ino, inode);
        self.path_cache.insert(normalized.clone(), ino);

        // Create . and .. entries.
        self.directories.insert(
            ino,
            vec![
                DirectoryEntry {
                    name: ".".to_string(),
                    ino,
                    file_type: FileType::Directory,
                },
                DirectoryEntry {
                    name: "..".to_string(),
                    ino: parent_ino,
                    file_type: FileType::Directory,
                },
            ],
        );

        // Add to parent directory.
        let entries = self.directories.entry(parent_ino).or_default();
        entries.push(DirectoryEntry {
            name: name.to_string(),
            ino,
            file_type: FileType::Directory,
        });

        Ok(ino)
    }

    /// open() — Open a file and return a file descriptor.
    pub fn open(
        &mut self,
        path: &str,
        flags: OpenFlags,
        mode: u32,
        uid: u32,
        gid: u32,
    ) -> Result<u32, FsError> {
        // Check for MVS dataset syntax.
        if path.starts_with("//'") {
            let dsn_ref = MvsDatasetRef::parse(path).ok_or(FsError::InvalidArgument {
                detail: format!("invalid MVS dataset syntax: {path}"),
            })?;
            // Simulate MVS dataset access — in a real system this would bridge to MVS.
            return Err(FsError::DatasetNotFound {
                dsn: dsn_ref.dataset_name,
            });
        }

        let normalized = normalize_path(path);

        let ino = match self.resolve_path(&normalized) {
            Ok(ino) => {
                if flags.exclusive && flags.create {
                    return Err(FsError::FileExists { path: normalized });
                }
                // Check permissions.
                let inode = self.inodes.get(&ino).ok_or(FsError::NotFound {
                    path: normalized.clone(),
                })?;
                if inode.file_type == FileType::Directory && flags.write {
                    return Err(FsError::IsADirectory { path: normalized });
                }
                if !check_permission(inode, uid, gid, flags.read, flags.write, false) {
                    return Err(FsError::PermissionDenied { path: normalized });
                }
                // Truncate if requested.
                if flags.truncate {
                    if let Some(inode) = self.inodes.get_mut(&ino) {
                        inode.data.clear();
                        inode.size = 0;
                        inode.mtime = SystemTime::now();
                    }
                }
                ino
            }
            Err(_) if flags.create => self.create_file(&normalized, mode, uid, gid)?,
            Err(e) => return Err(e),
        };

        let fd = self.alloc_fd()?;
        self.open_files.insert(
            fd,
            OpenFile {
                fd,
                inode: ino,
                offset: 0,
                flags,
            },
        );
        Ok(fd)
    }

    /// close() — Close a file descriptor.
    pub fn close(&mut self, fd: u32) -> Result<(), FsError> {
        self.open_files
            .remove(&fd)
            .ok_or(FsError::BadFileDescriptor { fd })?;
        Ok(())
    }

    /// read() — Read bytes from a file descriptor.
    pub fn read(&mut self, fd: u32, count: usize) -> Result<Vec<u8>, FsError> {
        let open_file = self
            .open_files
            .get(&fd)
            .ok_or(FsError::BadFileDescriptor { fd })?;

        if !open_file.flags.read {
            return Err(FsError::BadFileDescriptor { fd });
        }

        let ino = open_file.inode;
        let offset = open_file.offset as usize;

        let inode = self.inodes.get(&ino).ok_or(FsError::BadFileDescriptor { fd })?;
        let end = std::cmp::min(offset + count, inode.data.len());
        let data = if offset < inode.data.len() {
            inode.data[offset..end].to_vec()
        } else {
            Vec::new()
        };

        let bytes_read = data.len();
        // Update offset.
        if let Some(of) = self.open_files.get_mut(&fd) {
            of.offset += bytes_read as u64;
        }
        // Update atime.
        if let Some(inode) = self.inodes.get_mut(&ino) {
            inode.atime = SystemTime::now();
        }

        Ok(data)
    }

    /// write() — Write bytes to a file descriptor.
    pub fn write(&mut self, fd: u32, data: &[u8]) -> Result<usize, FsError> {
        let open_file = self
            .open_files
            .get(&fd)
            .ok_or(FsError::BadFileDescriptor { fd })?;

        if !open_file.flags.write {
            return Err(FsError::BadFileDescriptor { fd });
        }

        let ino = open_file.inode;
        let offset = if open_file.flags.append {
            self.inodes
                .get(&ino)
                .map(|i| i.size)
                .unwrap_or(0) as usize
        } else {
            open_file.offset as usize
        };

        let inode = self
            .inodes
            .get_mut(&ino)
            .ok_or(FsError::BadFileDescriptor { fd })?;

        // Extend data if needed.
        let needed = offset + data.len();
        if needed > inode.data.len() {
            inode.data.resize(needed, 0);
        }
        inode.data[offset..offset + data.len()].copy_from_slice(data);
        inode.size = inode.data.len() as u64;
        inode.mtime = SystemTime::now();

        let written = data.len();
        if let Some(of) = self.open_files.get_mut(&fd) {
            of.offset = (offset + written) as u64;
        }

        Ok(written)
    }

    /// lseek() — Reposition the file offset.
    pub fn lseek(
        &mut self,
        fd: u32,
        offset: i64,
        whence: SeekWhence,
    ) -> Result<u64, FsError> {
        let open_file = self
            .open_files
            .get(&fd)
            .ok_or(FsError::BadFileDescriptor { fd })?;

        let ino = open_file.inode;
        let current_offset = open_file.offset;
        let file_size = self
            .inodes
            .get(&ino)
            .map(|i| i.size)
            .unwrap_or(0);

        let new_offset = match whence {
            SeekWhence::Set => {
                if offset < 0 {
                    return Err(FsError::InvalidArgument {
                        detail: "negative offset with SEEK_SET".to_string(),
                    });
                }
                offset as u64
            }
            SeekWhence::Current => {
                let new = current_offset as i64 + offset;
                if new < 0 {
                    return Err(FsError::InvalidArgument {
                        detail: "resulting offset is negative".to_string(),
                    });
                }
                new as u64
            }
            SeekWhence::End => {
                let new = file_size as i64 + offset;
                if new < 0 {
                    return Err(FsError::InvalidArgument {
                        detail: "resulting offset is negative".to_string(),
                    });
                }
                new as u64
            }
        };

        if let Some(of) = self.open_files.get_mut(&fd) {
            of.offset = new_offset;
        }
        Ok(new_offset)
    }

    /// Create a symbolic link.
    pub fn symlink(
        &mut self,
        target: &str,
        link_path: &str,
        uid: u32,
        gid: u32,
    ) -> Result<u64, FsError> {
        let normalized = normalize_path(link_path);
        if self.path_cache.contains_key(&normalized) {
            return Err(FsError::FileExists {
                path: normalized,
            });
        }

        let (parent_path, name) = split_path(&normalized);
        let name = name.to_string();
        let parent_ino = self.resolve_path(&parent_path)?;

        let ino = self.alloc_ino();
        let mut inode = Inode::new(ino, FileType::SymbolicLink, 0o777, uid, gid);
        inode.symlink_target = Some(target.to_string());
        inode.data = target.as_bytes().to_vec();
        inode.size = target.len() as u64;

        self.inodes.insert(ino, inode);
        self.path_cache.insert(normalized, ino);

        let entries = self.directories.entry(parent_ino).or_default();
        entries.push(DirectoryEntry {
            name,
            ino,
            file_type: FileType::SymbolicLink,
        });

        Ok(ino)
    }

    /// Create a hard link.
    pub fn link(&mut self, existing_path: &str, new_path: &str) -> Result<(), FsError> {
        let existing_ino = self.resolve_path(existing_path)?;

        // Cannot hard link directories.
        if let Some(inode) = self.inodes.get(&existing_ino) {
            if inode.file_type == FileType::Directory {
                return Err(FsError::IsADirectory {
                    path: existing_path.to_string(),
                });
            }
        }

        let normalized_new = normalize_path(new_path);
        if self.path_cache.contains_key(&normalized_new) {
            return Err(FsError::FileExists {
                path: normalized_new,
            });
        }

        let (parent_path, name) = split_path(&normalized_new);
        let name = name.to_string();
        let parent_ino = self.resolve_path(&parent_path)?;

        // Increment nlink.
        if let Some(inode) = self.inodes.get_mut(&existing_ino) {
            inode.nlink += 1;
            inode.ctime = SystemTime::now();
        }

        self.path_cache.insert(normalized_new, existing_ino);

        let file_type = self
            .inodes
            .get(&existing_ino)
            .map(|i| i.file_type)
            .unwrap_or(FileType::RegularFile);
        let entries = self.directories.entry(parent_ino).or_default();
        entries.push(DirectoryEntry {
            name,
            ino: existing_ino,
            file_type,
        });

        Ok(())
    }

    /// Read a symbolic link target.
    pub fn readlink(&self, path: &str) -> Result<String, FsError> {
        let ino = self.resolve_path(path)?;
        let inode = self.inodes.get(&ino).ok_or(FsError::NotFound {
            path: path.to_string(),
        })?;
        match &inode.symlink_target {
            Some(target) => Ok(target.clone()),
            None => Err(FsError::InvalidArgument {
                detail: "not a symbolic link".to_string(),
            }),
        }
    }

    /// Set an advisory file lock (fcntl F_SETLK).
    pub fn set_lock(
        &mut self,
        fd: u32,
        lock: FileLock,
    ) -> Result<(), FsError> {
        let open_file = self
            .open_files
            .get(&fd)
            .ok_or(FsError::BadFileDescriptor { fd })?;
        let ino = open_file.inode;

        if lock.lock_type == LockType::Unlock {
            // Remove locks for this PID on this inode.
            if let Some(locks) = self.locks.get_mut(&ino) {
                locks.retain(|l| l.pid != lock.pid);
            }
            return Ok(());
        }

        // Check for conflicts.
        if let Some(locks) = self.locks.get(&ino) {
            for existing in locks {
                if existing.pid == lock.pid {
                    continue; // Same process can upgrade/downgrade.
                }
                if ranges_overlap(existing.start, existing.length, lock.start, lock.length) {
                    // Read locks don't conflict with read locks.
                    if existing.lock_type == LockType::ReadLock
                        && lock.lock_type == LockType::ReadLock
                    {
                        continue;
                    }
                    return Err(FsError::WouldBlock);
                }
            }
        }

        self.locks.entry(ino).or_default().push(lock);
        Ok(())
    }

    /// stat() — Get file metadata by path.
    pub fn stat(&self, path: &str) -> Result<&Inode, FsError> {
        let ino = self.resolve_path(path)?;
        self.inodes.get(&ino).ok_or(FsError::NotFound {
            path: path.to_string(),
        })
    }

    /// List directory contents.
    pub fn readdir(&self, path: &str) -> Result<Vec<DirectoryEntry>, FsError> {
        let ino = self.resolve_path(path)?;
        let inode = self.inodes.get(&ino).ok_or(FsError::NotFound {
            path: path.to_string(),
        })?;
        if inode.file_type != FileType::Directory {
            return Err(FsError::NotADirectory {
                path: path.to_string(),
            });
        }
        Ok(self.directories.get(&ino).cloned().unwrap_or_default())
    }
}

// ---------------------------------------------------------------------------
//  Helper functions
// ---------------------------------------------------------------------------

/// Normalize a path (remove trailing slashes, handle //).
fn normalize_path(path: &str) -> String {
    if path == "/" {
        return "/".to_string();
    }
    let trimmed = path.trim_end_matches('/');
    if trimmed.is_empty() {
        "/".to_string()
    } else if trimmed.starts_with('/') {
        trimmed.to_string()
    } else {
        format!("/{trimmed}")
    }
}

/// Split a path into (parent, basename).
fn split_path(path: &str) -> (String, &str) {
    let normalized = path.strip_suffix('/').unwrap_or(path);

    match normalized.rfind('/') {
        Some(0) => ("/".to_string(), &normalized[1..]),
        Some(pos) => (normalized[..pos].to_string(), &normalized[pos + 1..]),
        None => ("/".to_string(), normalized),
    }
}

/// Check if two byte ranges overlap.
fn ranges_overlap(start1: u64, len1: u64, start2: u64, len2: u64) -> bool {
    let end1 = if len1 == 0 { u64::MAX } else { start1 + len1 };
    let end2 = if len2 == 0 { u64::MAX } else { start2 + len2 };
    start1 < end2 && start2 < end1
}

// ===========================================================================
//  Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_root_fs() {
        let fs = Zfs::new(1024);
        let root = fs.get_inode(1).unwrap();
        assert_eq!(root.file_type, FileType::Directory);
        assert_eq!(root.permissions(), 0o755);
    }

    #[test]
    fn test_mount_table() {
        let mut fs = Zfs::new(1024);
        fs.mount(MountEntry {
            filesystem: "OMVS.ROOT".to_string(),
            mountpoint: "/".to_string(),
            fstype: "ZFS".to_string(),
            read_only: false,
        });
        fs.mount(MountEntry {
            filesystem: "OMVS.TMP".to_string(),
            mountpoint: "/tmp".to_string(),
            fstype: "TFS".to_string(),
            read_only: false,
        });
        assert_eq!(fs.mount_table().len(), 2);
        assert_eq!(fs.mount_table()[0].mountpoint, "/");
        assert_eq!(fs.mount_table()[1].fstype, "TFS");
    }

    #[test]
    fn test_create_file_and_stat() {
        let mut fs = Zfs::new(1024);
        let ino = fs.create_file("/test.txt", 0o644, 100, 200).unwrap();
        let inode = fs.get_inode(ino).unwrap();
        assert_eq!(inode.file_type, FileType::RegularFile);
        assert_eq!(inode.permissions(), 0o644);
        assert_eq!(inode.uid, 100);
        assert_eq!(inode.gid, 200);

        let stat = fs.stat("/test.txt").unwrap();
        assert_eq!(stat.ino, ino);
    }

    #[test]
    fn test_open_read_write_close() {
        let mut fs = Zfs::new(1024);
        let fd = fs
            .open(
                "/data.txt",
                OpenFlags {
                    read: true,
                    write: true,
                    create: true,
                    exclusive: false,
                    truncate: false,
                    append: false,
                },
                0o644,
                0,
                0,
            )
            .unwrap();

        let written = fs.write(fd, b"Hello, USS!").unwrap();
        assert_eq!(written, 11);

        // Seek back to start.
        let pos = fs.lseek(fd, 0, SeekWhence::Set).unwrap();
        assert_eq!(pos, 0);

        let data = fs.read(fd, 100).unwrap();
        assert_eq!(&data, b"Hello, USS!");

        fs.close(fd).unwrap();
    }

    #[test]
    fn test_write_advances_offset() {
        let mut fs = Zfs::new(1024);
        let fd = fs
            .open(
                "/test.bin",
                OpenFlags::write_create(),
                0o644,
                0,
                0,
            )
            .unwrap();
        fs.write(fd, &[1, 2, 3, 4, 5]).unwrap();
        // Write 5 more bytes.
        fs.write(fd, &[6, 7, 8, 9, 10]).unwrap();

        let inode = fs.stat("/test.bin").unwrap();
        assert_eq!(inode.size, 10);
    }

    #[test]
    fn test_lseek_set_current_end() {
        let mut fs = Zfs::new(1024);
        let fd = fs
            .open(
                "/seek.txt",
                OpenFlags {
                    read: true,
                    write: true,
                    create: true,
                    ..OpenFlags::read_write()
                },
                0o644,
                0,
                0,
            )
            .unwrap();
        fs.write(fd, b"0123456789").unwrap();

        // SEEK_SET
        assert_eq!(fs.lseek(fd, 0, SeekWhence::Set).unwrap(), 0);
        // SEEK_END
        assert_eq!(fs.lseek(fd, 0, SeekWhence::End).unwrap(), 10);
        // SEEK_CUR
        assert_eq!(fs.lseek(fd, -5, SeekWhence::Current).unwrap(), 5);
    }

    #[test]
    fn test_permission_check_owner() {
        let inode = Inode::new(1, FileType::RegularFile, 0o600, 100, 200);
        assert!(check_permission(&inode, 100, 200, true, false, false));
        assert!(check_permission(&inode, 100, 200, false, true, false));
        assert!(!check_permission(&inode, 200, 200, true, false, false));
    }

    #[test]
    fn test_permission_check_group() {
        let inode = Inode::new(1, FileType::RegularFile, 0o060, 100, 200);
        assert!(check_permission(&inode, 300, 200, true, true, false));
        assert!(!check_permission(&inode, 300, 300, true, false, false));
    }

    #[test]
    fn test_permission_check_other() {
        let inode = Inode::new(1, FileType::Directory, 0o755, 100, 200);
        assert!(check_permission(&inode, 999, 999, true, false, true));
        assert!(!check_permission(&inode, 999, 999, false, true, false));
    }

    #[test]
    fn test_permission_check_superuser() {
        let inode = Inode::new(1, FileType::RegularFile, 0o000, 100, 200);
        assert!(check_permission(&inode, 0, 0, true, true, true));
    }

    #[test]
    fn test_setuid_bit() {
        let inode = Inode::new(1, FileType::RegularFile, 0o4755, 100, 200);
        assert!(inode.is_setuid());
        assert!(!inode.is_setgid());
    }

    #[test]
    fn test_symlink_and_readlink() {
        let mut fs = Zfs::new(1024);
        fs.create_file("/target.txt", 0o644, 0, 0).unwrap();
        fs.symlink("/target.txt", "/link.txt", 0, 0).unwrap();

        let target = fs.readlink("/link.txt").unwrap();
        assert_eq!(target, "/target.txt");

        let inode = fs.stat("/link.txt").unwrap();
        assert_eq!(inode.file_type, FileType::SymbolicLink);
    }

    #[test]
    fn test_hard_link() {
        let mut fs = Zfs::new(1024);
        let ino = fs.create_file("/original.txt", 0o644, 0, 0).unwrap();
        fs.link("/original.txt", "/hardlink.txt").unwrap();

        let orig = fs.get_inode(ino).unwrap();
        assert_eq!(orig.nlink, 2);

        // Both paths resolve to same inode.
        let ino1 = fs.resolve_path("/original.txt").unwrap();
        let ino2 = fs.resolve_path("/hardlink.txt").unwrap();
        assert_eq!(ino1, ino2);
    }

    #[test]
    fn test_file_lock_read_read_compatible() {
        let mut fs = Zfs::new(1024);
        let fd1 = fs
            .open("/locktest.txt", OpenFlags::write_create(), 0o644, 0, 0)
            .unwrap();
        let fd2 = fs
            .open(
                "/locktest.txt",
                OpenFlags::read_only(),
                0o644,
                0,
                0,
            )
            .unwrap();

        fs.set_lock(
            fd1,
            FileLock {
                lock_type: LockType::ReadLock,
                start: 0,
                length: 100,
                pid: 1,
            },
        )
        .unwrap();

        // Another read lock should succeed.
        fs.set_lock(
            fd2,
            FileLock {
                lock_type: LockType::ReadLock,
                start: 0,
                length: 100,
                pid: 2,
            },
        )
        .unwrap();
    }

    #[test]
    fn test_file_lock_read_write_conflict() {
        let mut fs = Zfs::new(1024);
        let fd1 = fs
            .open("/locktest2.txt", OpenFlags::write_create(), 0o644, 0, 0)
            .unwrap();
        let fd2 = fs
            .open(
                "/locktest2.txt",
                OpenFlags::read_write(),
                0o644,
                0,
                0,
            )
            .unwrap();

        fs.set_lock(
            fd1,
            FileLock {
                lock_type: LockType::ReadLock,
                start: 0,
                length: 100,
                pid: 1,
            },
        )
        .unwrap();

        // Write lock should conflict.
        let err = fs
            .set_lock(
                fd2,
                FileLock {
                    lock_type: LockType::WriteLock,
                    start: 50,
                    length: 50,
                    pid: 2,
                },
            )
            .unwrap_err();
        assert!(matches!(err, FsError::WouldBlock));
    }

    #[test]
    fn test_mvs_dataset_ref_parse() {
        let r = MvsDatasetRef::parse("//'MY.DATA.SET'").unwrap();
        assert_eq!(r.dataset_name, "MY.DATA.SET");
        assert!(r.member.is_none());

        let r2 = MvsDatasetRef::parse("//'MY.PDS(MEMBER)'").unwrap();
        assert_eq!(r2.dataset_name, "MY.PDS");
        assert_eq!(r2.member.as_deref(), Some("MEMBER"));
    }

    #[test]
    fn test_mvs_dataset_ref_invalid() {
        assert!(MvsDatasetRef::parse("/data/file.txt").is_none());
        assert!(MvsDatasetRef::parse("//NOQUOTE").is_none());
    }

    #[test]
    fn test_open_mvs_dataset_returns_error() {
        let mut fs = Zfs::new(1024);
        let err = fs
            .open(
                "//'MY.DATA.SET'",
                OpenFlags::read_only(),
                0o644,
                0,
                0,
            )
            .unwrap_err();
        assert!(matches!(err, FsError::DatasetNotFound { .. }));
    }

    #[test]
    fn test_mkdir_and_readdir() {
        let mut fs = Zfs::new(1024);
        fs.mkdir("/data", 0o755, 0, 0).unwrap();
        fs.create_file("/data/file1.txt", 0o644, 0, 0).unwrap();
        fs.create_file("/data/file2.txt", 0o644, 0, 0).unwrap();

        let entries = fs.readdir("/data").unwrap();
        // Should have ., .., file1.txt, file2.txt
        assert_eq!(entries.len(), 4);
        let names: Vec<&str> = entries.iter().map(|e| e.name.as_str()).collect();
        assert!(names.contains(&"."));
        assert!(names.contains(&".."));
        assert!(names.contains(&"file1.txt"));
        assert!(names.contains(&"file2.txt"));
    }

    #[test]
    fn test_file_type_display() {
        assert_eq!(FileType::RegularFile.to_string(), "-");
        assert_eq!(FileType::Directory.to_string(), "d");
        assert_eq!(FileType::SymbolicLink.to_string(), "l");
    }

    #[test]
    fn test_open_exclusive_create_fails_if_exists() {
        let mut fs = Zfs::new(1024);
        fs.create_file("/existing.txt", 0o644, 0, 0).unwrap();
        let err = fs
            .open(
                "/existing.txt",
                OpenFlags {
                    read: true,
                    write: true,
                    create: true,
                    exclusive: true,
                    truncate: false,
                    append: false,
                },
                0o644,
                0,
                0,
            )
            .unwrap_err();
        assert!(matches!(err, FsError::FileExists { .. }));
    }

    #[test]
    fn test_open_permission_denied() {
        let mut fs = Zfs::new(1024);
        fs.create_file("/secret.txt", 0o600, 100, 200).unwrap();
        let err = fs
            .open(
                "/secret.txt",
                OpenFlags::read_only(),
                0,
                200,
                300,
            )
            .unwrap_err();
        assert!(matches!(err, FsError::PermissionDenied { .. }));
    }

    #[test]
    fn test_normalize_path() {
        assert_eq!(normalize_path("/"), "/");
        assert_eq!(normalize_path("/data/"), "/data");
        assert_eq!(normalize_path("data"), "/data");
    }

    #[test]
    fn test_ranges_overlap() {
        assert!(ranges_overlap(0, 10, 5, 10));
        assert!(!ranges_overlap(0, 5, 10, 5));
        assert!(ranges_overlap(0, 0, 5, 5)); // 0 length = to end of file
    }
}
