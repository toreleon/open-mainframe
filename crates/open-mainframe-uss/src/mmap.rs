//! Memory-Mapped Files (USS-111).
//!
//! Provides POSIX memory-mapped file support:
//! - mmap() / BPX1MMP / BPX4MMP — map pages of memory
//! - munmap() / BPX1MUN / BPX4MUN — unmap previously mapped addresses
//! - mprotect() / BPX1MPR / BPX4MPR — set protection of memory mapping
//! - msync() / BPX1MSY / BPX4MSY — synchronize memory with physical storage
//!
//! z/OS-specific: supports above-the-bar storage (64-bit virtual addresses)
//! and map lengths greater than 2 GB.

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors for memory-mapped file operations.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, miette::Diagnostic)]
pub enum MmapError {
    /// Invalid argument (EINVAL).
    #[error("invalid argument: {detail}")]
    InvalidArgument { detail: String },

    /// Permission denied (EACCES).
    #[error("permission denied for mapping")]
    PermissionDenied,

    /// No memory available (ENOMEM).
    #[error("insufficient memory for mapping")]
    NoMemory,

    /// Bad file descriptor.
    #[error("bad file descriptor: {fd}")]
    BadFileDescriptor { fd: u32 },

    /// Mapping not found.
    #[error("no mapping at address {addr:#x}")]
    NoMapping { addr: u64 },
}

// ---------------------------------------------------------------------------
//  Mapping Flags
// ---------------------------------------------------------------------------

/// Flags controlling how a memory mapping behaves.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MapFlags {
    /// MAP_SHARED: writes visible to other processes; changes written to file.
    Shared,
    /// MAP_PRIVATE: copy-on-write; changes not visible to others.
    Private,
    /// MAP_ANONYMOUS: not backed by a file; used for shared memory.
    Anonymous,
}

/// Memory protection flags for mmap/mprotect.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProtFlags {
    /// PROT_READ: pages can be read.
    pub read: bool,
    /// PROT_WRITE: pages can be written.
    pub write: bool,
    /// PROT_EXEC: pages can be executed.
    pub exec: bool,
}

impl ProtFlags {
    /// No access (PROT_NONE).
    pub fn none() -> Self {
        Self {
            read: false,
            write: false,
            exec: false,
        }
    }

    /// Read-only.
    pub fn read_only() -> Self {
        Self {
            read: true,
            write: false,
            exec: false,
        }
    }

    /// Read-write.
    pub fn read_write() -> Self {
        Self {
            read: true,
            write: true,
            exec: false,
        }
    }

    /// Read-execute.
    pub fn read_exec() -> Self {
        Self {
            read: true,
            write: false,
            exec: true,
        }
    }
}

// ---------------------------------------------------------------------------
//  Memory Mapping
// ---------------------------------------------------------------------------

/// A single memory mapping region.
#[derive(Debug, Clone)]
pub struct MemoryMapping {
    /// Base virtual address of the mapping.
    pub addr: u64,
    /// Length of the mapping in bytes.
    pub length: u64,
    /// Protection flags.
    pub prot: ProtFlags,
    /// Mapping flags (shared, private, anonymous).
    pub flags: MapFlags,
    /// File descriptor (None for anonymous mappings).
    pub fd: Option<u32>,
    /// Offset in the file.
    pub offset: u64,
    /// The mapping data (simulated in-memory).
    pub data: Vec<u8>,
    /// Whether the mapping has been modified (dirty).
    pub dirty: bool,
}

// ---------------------------------------------------------------------------
//  Msync Flags
// ---------------------------------------------------------------------------

/// Flags for msync().
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MsyncFlags {
    /// MS_SYNC: synchronous write.
    Sync,
    /// MS_ASYNC: asynchronous write.
    Async,
    /// MS_INVALIDATE: invalidate cached copies.
    Invalidate,
}

// ---------------------------------------------------------------------------
//  Memory Map Manager
// ---------------------------------------------------------------------------

/// Manages memory-mapped file regions.
#[derive(Debug)]
pub struct MmapManager {
    /// Active mappings keyed by base address.
    mappings: HashMap<u64, MemoryMapping>,
    /// Next virtual address to assign (simulated).
    next_addr: u64,
    /// Page size (4096 bytes on z/OS).
    page_size: u64,
}

impl MmapManager {
    /// Create a new mmap manager.
    pub fn new() -> Self {
        Self {
            mappings: HashMap::new(),
            next_addr: 0x1000_0000, // Start at a reasonable address.
            page_size: 4096,
        }
    }

    /// Return the system page size.
    pub fn page_size(&self) -> u64 {
        self.page_size
    }

    /// mmap() — map pages of memory.
    ///
    /// Maps a region of memory, optionally backed by a file.
    /// The `file_data` parameter provides the initial file content for
    /// file-backed mappings (pass empty slice for anonymous mappings).
    pub fn mmap(
        &mut self,
        length: u64,
        prot: ProtFlags,
        flags: MapFlags,
        fd: Option<u32>,
        offset: u64,
        file_data: &[u8],
    ) -> Result<u64, MmapError> {
        if length == 0 {
            return Err(MmapError::InvalidArgument {
                detail: "length must be > 0".to_string(),
            });
        }

        // Offset must be page-aligned.
        if offset % self.page_size != 0 {
            return Err(MmapError::InvalidArgument {
                detail: format!(
                    "offset {offset} is not page-aligned (page size = {})",
                    self.page_size
                ),
            });
        }

        // Anonymous mappings must not have a file descriptor.
        if flags == MapFlags::Anonymous && fd.is_some() {
            return Err(MmapError::InvalidArgument {
                detail: "anonymous mapping must not specify a file descriptor".to_string(),
            });
        }

        // File-backed mappings need a valid fd.
        if flags != MapFlags::Anonymous && fd.is_none() {
            return Err(MmapError::InvalidArgument {
                detail: "file-backed mapping requires a file descriptor".to_string(),
            });
        }

        // Align length to page boundary.
        let aligned_length =
            ((length + self.page_size - 1) / self.page_size) * self.page_size;

        // Allocate virtual address.
        let addr = self.next_addr;
        self.next_addr += aligned_length;

        // Initialize data.
        let data = if flags == MapFlags::Anonymous {
            vec![0u8; aligned_length as usize]
        } else {
            let offset_usize = offset as usize;
            let end = std::cmp::min(offset_usize + aligned_length as usize, file_data.len());
            let mut buf = vec![0u8; aligned_length as usize];
            if offset_usize < file_data.len() {
                let copy_len = end - offset_usize;
                buf[..copy_len].copy_from_slice(&file_data[offset_usize..end]);
            }
            buf
        };

        let mapping = MemoryMapping {
            addr,
            length: aligned_length,
            prot,
            flags,
            fd,
            offset,
            data,
            dirty: false,
        };
        self.mappings.insert(addr, mapping);
        Ok(addr)
    }

    /// munmap() — unmap a previously mapped region.
    pub fn munmap(&mut self, addr: u64, _length: u64) -> Result<(), MmapError> {
        self.mappings
            .remove(&addr)
            .ok_or(MmapError::NoMapping { addr })?;
        Ok(())
    }

    /// mprotect() — change protection of a mapping.
    pub fn mprotect(
        &mut self,
        addr: u64,
        _length: u64,
        prot: ProtFlags,
    ) -> Result<(), MmapError> {
        let mapping = self
            .mappings
            .get_mut(&addr)
            .ok_or(MmapError::NoMapping { addr })?;
        mapping.prot = prot;
        Ok(())
    }

    /// msync() — synchronize a mapping with its backing file.
    ///
    /// Returns the dirty data for the caller to write back to the file.
    pub fn msync(
        &mut self,
        addr: u64,
        _length: u64,
        _flags: MsyncFlags,
    ) -> Result<Option<Vec<u8>>, MmapError> {
        let mapping = self
            .mappings
            .get_mut(&addr)
            .ok_or(MmapError::NoMapping { addr })?;

        if mapping.dirty && mapping.flags == MapFlags::Shared {
            mapping.dirty = false;
            Ok(Some(mapping.data.clone()))
        } else {
            Ok(None)
        }
    }

    /// Read bytes from a mapping.
    pub fn read(
        &self,
        addr: u64,
        offset: usize,
        count: usize,
    ) -> Result<Vec<u8>, MmapError> {
        let mapping = self
            .mappings
            .get(&addr)
            .ok_or(MmapError::NoMapping { addr })?;

        if !mapping.prot.read {
            return Err(MmapError::PermissionDenied);
        }

        let end = std::cmp::min(offset + count, mapping.data.len());
        if offset >= mapping.data.len() {
            return Ok(Vec::new());
        }
        Ok(mapping.data[offset..end].to_vec())
    }

    /// Write bytes to a mapping.
    pub fn write(
        &mut self,
        addr: u64,
        offset: usize,
        data: &[u8],
    ) -> Result<usize, MmapError> {
        let mapping = self
            .mappings
            .get_mut(&addr)
            .ok_or(MmapError::NoMapping { addr })?;

        if !mapping.prot.write {
            return Err(MmapError::PermissionDenied);
        }

        let end = std::cmp::min(offset + data.len(), mapping.data.len());
        if offset >= mapping.data.len() {
            return Ok(0);
        }
        let write_len = end - offset;
        mapping.data[offset..end].copy_from_slice(&data[..write_len]);
        mapping.dirty = true;
        Ok(write_len)
    }

    /// Get a mapping by address.
    pub fn get_mapping(&self, addr: u64) -> Option<&MemoryMapping> {
        self.mappings.get(&addr)
    }

    /// Number of active mappings.
    pub fn mapping_count(&self) -> usize {
        self.mappings.len()
    }
}

impl Default for MmapManager {
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
    fn test_mmap_anonymous() {
        let mut mgr = MmapManager::new();
        let addr = mgr
            .mmap(4096, ProtFlags::read_write(), MapFlags::Anonymous, None, 0, &[])
            .unwrap();
        assert!(addr > 0);
        assert_eq!(mgr.mapping_count(), 1);

        let mapping = mgr.get_mapping(addr).unwrap();
        assert_eq!(mapping.length, 4096);
        assert!(mapping.prot.read);
        assert!(mapping.prot.write);
    }

    #[test]
    fn test_mmap_file_backed() {
        let mut mgr = MmapManager::new();
        let file_data = b"Hello, mmap!";
        let addr = mgr
            .mmap(
                4096,
                ProtFlags::read_only(),
                MapFlags::Shared,
                Some(3),
                0,
                file_data,
            )
            .unwrap();

        let data = mgr.read(addr, 0, 12).unwrap();
        assert_eq!(&data, b"Hello, mmap!");
    }

    #[test]
    fn test_mmap_write_and_read() {
        let mut mgr = MmapManager::new();
        let addr = mgr
            .mmap(4096, ProtFlags::read_write(), MapFlags::Shared, Some(3), 0, &[0; 4096])
            .unwrap();

        mgr.write(addr, 0, b"written data").unwrap();
        let data = mgr.read(addr, 0, 12).unwrap();
        assert_eq!(&data, b"written data");

        let mapping = mgr.get_mapping(addr).unwrap();
        assert!(mapping.dirty);
    }

    #[test]
    fn test_mmap_read_only_rejects_write() {
        let mut mgr = MmapManager::new();
        let addr = mgr
            .mmap(4096, ProtFlags::read_only(), MapFlags::Anonymous, None, 0, &[])
            .unwrap();

        let err = mgr.write(addr, 0, b"data").unwrap_err();
        assert!(matches!(err, MmapError::PermissionDenied));
    }

    #[test]
    fn test_munmap() {
        let mut mgr = MmapManager::new();
        let addr = mgr
            .mmap(4096, ProtFlags::read_write(), MapFlags::Anonymous, None, 0, &[])
            .unwrap();
        assert_eq!(mgr.mapping_count(), 1);

        mgr.munmap(addr, 4096).unwrap();
        assert_eq!(mgr.mapping_count(), 0);
    }

    #[test]
    fn test_munmap_no_mapping() {
        let mut mgr = MmapManager::new();
        let err = mgr.munmap(0xDEAD, 4096).unwrap_err();
        assert!(matches!(err, MmapError::NoMapping { .. }));
    }

    #[test]
    fn test_mprotect() {
        let mut mgr = MmapManager::new();
        let addr = mgr
            .mmap(4096, ProtFlags::read_write(), MapFlags::Anonymous, None, 0, &[])
            .unwrap();

        mgr.mprotect(addr, 4096, ProtFlags::read_only()).unwrap();
        let mapping = mgr.get_mapping(addr).unwrap();
        assert!(mapping.prot.read);
        assert!(!mapping.prot.write);
    }

    #[test]
    fn test_msync_shared_dirty() {
        let mut mgr = MmapManager::new();
        let addr = mgr
            .mmap(4096, ProtFlags::read_write(), MapFlags::Shared, Some(3), 0, &[0; 4096])
            .unwrap();
        mgr.write(addr, 0, b"sync me").unwrap();

        let synced = mgr.msync(addr, 4096, MsyncFlags::Sync).unwrap();
        assert!(synced.is_some());
        assert_eq!(&synced.unwrap()[..7], b"sync me");

        // After sync, dirty should be cleared.
        let mapping = mgr.get_mapping(addr).unwrap();
        assert!(!mapping.dirty);
    }

    #[test]
    fn test_msync_private_not_dirty() {
        let mut mgr = MmapManager::new();
        let addr = mgr
            .mmap(4096, ProtFlags::read_write(), MapFlags::Private, Some(3), 0, &[0; 4096])
            .unwrap();
        mgr.write(addr, 0, b"private").unwrap();

        // Private mappings don't sync back to file.
        let synced = mgr.msync(addr, 4096, MsyncFlags::Sync).unwrap();
        assert!(synced.is_none());
    }

    #[test]
    fn test_mmap_page_alignment() {
        let mut mgr = MmapManager::new();
        // Non-page-aligned offset should fail.
        let err = mgr
            .mmap(4096, ProtFlags::read_write(), MapFlags::Shared, Some(3), 100, &[])
            .unwrap_err();
        assert!(matches!(err, MmapError::InvalidArgument { .. }));
    }

    #[test]
    fn test_mmap_zero_length() {
        let mut mgr = MmapManager::new();
        let err = mgr
            .mmap(0, ProtFlags::read_write(), MapFlags::Anonymous, None, 0, &[])
            .unwrap_err();
        assert!(matches!(err, MmapError::InvalidArgument { .. }));
    }

    #[test]
    fn test_mmap_length_aligned_to_page() {
        let mut mgr = MmapManager::new();
        // Request 100 bytes, should be rounded up to 4096.
        let addr = mgr
            .mmap(100, ProtFlags::read_write(), MapFlags::Anonymous, None, 0, &[])
            .unwrap();
        let mapping = mgr.get_mapping(addr).unwrap();
        assert_eq!(mapping.length, 4096);
    }

    #[test]
    fn test_page_size() {
        let mgr = MmapManager::new();
        assert_eq!(mgr.page_size(), 4096);
    }
}
