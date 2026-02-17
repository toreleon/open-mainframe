//! CICS file operations.
//!
//! Implements READ, WRITE, REWRITE, DELETE file commands.
//! Provides both in-memory (`FileManager`) and disk-backed (`PersistentFileManager`)
//! storage via VSAM dataset integration.

use crate::{CicsError, CicsResponse, CicsResult};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// A CICS file record.
#[derive(Debug, Clone)]
pub struct FileRecord {
    /// Record key (RIDFLD)
    pub key: Vec<u8>,
    /// Record data
    pub data: Vec<u8>,
}

impl FileRecord {
    /// Create a new record.
    pub fn new(key: Vec<u8>, data: Vec<u8>) -> Self {
        Self { key, data }
    }

    /// Create from strings.
    pub fn from_strings(key: &str, data: &str) -> Self {
        Self {
            key: key.as_bytes().to_vec(),
            data: data.as_bytes().to_vec(),
        }
    }

    /// Get key as string.
    pub fn key_string(&self) -> String {
        String::from_utf8_lossy(&self.key).to_string()
    }

    /// Get data as string.
    pub fn data_string(&self) -> String {
        String::from_utf8_lossy(&self.data).to_string()
    }
}

/// CICS file definition.
#[derive(Debug, Clone)]
pub struct CicsFile {
    /// File name (DD name)
    pub name: String,
    /// Record length
    pub record_length: usize,
    /// Key length
    pub key_length: usize,
    /// Key position within record
    pub key_position: usize,
    /// Browsing enabled
    pub browse_enabled: bool,
    /// Read enabled
    pub read_enabled: bool,
    /// Update enabled
    pub update_enabled: bool,
    /// Add enabled
    pub add_enabled: bool,
    /// Delete enabled
    pub delete_enabled: bool,
}

impl CicsFile {
    /// Create a new file definition.
    pub fn new(name: &str, record_length: usize, key_length: usize) -> Self {
        Self {
            name: name.to_uppercase(),
            record_length,
            key_length,
            key_position: 0,
            browse_enabled: true,
            read_enabled: true,
            update_enabled: true,
            add_enabled: true,
            delete_enabled: true,
        }
    }

    /// Set key position.
    pub fn with_key_position(mut self, pos: usize) -> Self {
        self.key_position = pos;
        self
    }

    /// Disable browse.
    pub fn disable_browse(mut self) -> Self {
        self.browse_enabled = false;
        self
    }

    /// Disable update.
    pub fn disable_update(mut self) -> Self {
        self.update_enabled = false;
        self
    }
}

/// File operation mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileMode {
    /// Normal read
    Read,
    /// Read for update
    Update,
    /// Browse forward
    BrowseNext,
    /// Browse backward
    BrowsePrev,
}

/// File browse state.
#[derive(Debug)]
struct BrowseState {
    /// File name
    file_name: String,
    /// Current position (key)
    current_key: Option<Vec<u8>>,
}

/// CICS file manager.
pub struct FileManager {
    /// Registered files
    files: HashMap<String, CicsFile>,
    /// File data (mock storage)
    data: HashMap<String, Vec<FileRecord>>,
    /// Records locked for update
    locked_records: HashMap<String, Vec<u8>>,
    /// Active browse sessions
    browse_sessions: HashMap<u32, BrowseState>,
    /// Next browse token
    next_token: u32,
}

impl FileManager {
    /// Create a new file manager.
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            data: HashMap::new(),
            locked_records: HashMap::new(),
            browse_sessions: HashMap::new(),
            next_token: 1,
        }
    }

    /// Register a file.
    pub fn register(&mut self, file: CicsFile) {
        let name = file.name.clone();
        self.files.insert(name.clone(), file);
        self.data.insert(name, Vec::new());
    }

    /// Check if file exists.
    pub fn exists(&self, name: &str) -> bool {
        self.files.contains_key(&name.to_uppercase())
    }

    /// Get file definition.
    pub fn get_file(&self, name: &str) -> Option<&CicsFile> {
        self.files.get(&name.to_uppercase())
    }

    /// Load test data into file.
    pub fn load_data(&mut self, file_name: &str, records: Vec<FileRecord>) {
        if let Some(data) = self.data.get_mut(&file_name.to_uppercase()) {
            *data = records;
            // Sort by key
            data.sort_by(|a, b| a.key.cmp(&b.key));
        }
    }

    /// Execute READ command.
    pub fn read(
        &mut self,
        file_name: &str,
        key: &[u8],
        mode: FileMode,
    ) -> CicsResult<FileRecord> {
        let name = file_name.to_uppercase();

        // Check file exists
        let file = self.files.get(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        // Check read enabled
        if !file.read_enabled {
            return Err(CicsError::FileError(CicsResponse::Invreq));
        }

        // Check for update mode
        if mode == FileMode::Update && !file.update_enabled {
            return Err(CicsError::FileError(CicsResponse::Invreq));
        }

        // Find record
        let records = self.data.get(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        // Use prefix matching: when KEYLENGTH differs from the file's key
        // length, CICS compares only the shorter of the two.  This lets a
        // COBOL program with `KEYLENGTH(8)` match records whose stored key
        // is wider (or vice-versa).
        let record = records
            .iter()
            .find(|r| {
                let min_len = r.key.len().min(key.len());
                min_len > 0 && r.key[..min_len] == key[..min_len]
            })
            .cloned()
            .ok_or_else(|| CicsError::FileError(CicsResponse::Notfnd))?;

        // Lock record for update
        if mode == FileMode::Update {
            self.locked_records.insert(name, key.to_vec());
        }

        Ok(record)
    }

    /// Execute WRITE command.
    pub fn write(&mut self, file_name: &str, record: FileRecord) -> CicsResult<()> {
        let name = file_name.to_uppercase();

        // Check file exists
        let file = self.files.get(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        // Check add enabled
        if !file.add_enabled {
            return Err(CicsError::FileError(CicsResponse::Invreq));
        }

        // Check record length
        if record.data.len() > file.record_length {
            return Err(CicsError::FileError(CicsResponse::Lengerr));
        }

        // Check key length
        if record.key.len() != file.key_length {
            return Err(CicsError::FileError(CicsResponse::Invreq));
        }

        // Check for duplicate key
        let records = self.data.get_mut(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        if records.iter().any(|r| r.key == record.key) {
            return Err(CicsError::FileError(CicsResponse::Duprec));
        }

        // Add record
        records.push(record);
        records.sort_by(|a, b| a.key.cmp(&b.key));

        Ok(())
    }

    /// Execute REWRITE command.
    pub fn rewrite(&mut self, file_name: &str, data: &[u8]) -> CicsResult<()> {
        let name = file_name.to_uppercase();

        // Check file exists
        let file = self.files.get(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        // Check update enabled
        if !file.update_enabled {
            return Err(CicsError::FileError(CicsResponse::Invreq));
        }

        // Check record length
        if data.len() > file.record_length {
            return Err(CicsError::FileError(CicsResponse::Lengerr));
        }

        // Check record is locked
        let locked_key = self.locked_records.get(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Invreq)
        })?.clone();

        // Find and update record
        let records = self.data.get_mut(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        let record = records
            .iter_mut()
            .find(|r| r.key == locked_key)
            .ok_or_else(|| CicsError::FileError(CicsResponse::Notfnd))?;

        record.data = data.to_vec();

        // Release lock
        self.locked_records.remove(&name);

        Ok(())
    }

    /// Execute DELETE command.
    pub fn delete(&mut self, file_name: &str, key: Option<&[u8]>) -> CicsResult<()> {
        let name = file_name.to_uppercase();

        // Check file exists
        let file = self.files.get(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        // Check delete enabled
        if !file.delete_enabled {
            return Err(CicsError::FileError(CicsResponse::Invreq));
        }

        // Determine key to delete
        let delete_key = if let Some(k) = key {
            k.to_vec()
        } else {
            // Use locked record
            self.locked_records.get(&name).ok_or_else(|| {
                CicsError::FileError(CicsResponse::Invreq)
            })?.clone()
        };

        // Find and remove record
        let records = self.data.get_mut(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        let pos = records
            .iter()
            .position(|r| r.key == delete_key)
            .ok_or_else(|| CicsError::FileError(CicsResponse::Notfnd))?;

        records.remove(pos);

        // Release lock
        self.locked_records.remove(&name);

        Ok(())
    }

    /// Start browse session.
    pub fn startbr(&mut self, file_name: &str, key: &[u8]) -> CicsResult<u32> {
        let name = file_name.to_uppercase();

        // Check file exists
        let file = self.files.get(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        // Check browse enabled
        if !file.browse_enabled {
            return Err(CicsError::FileError(CicsResponse::Invreq));
        }

        // Create browse session
        let token = self.next_token;
        self.next_token += 1;

        self.browse_sessions.insert(token, BrowseState {
            file_name: name,
            current_key: Some(key.to_vec()),
        });

        Ok(token)
    }

    /// Read next record in browse.
    pub fn readnext(&mut self, token: u32) -> CicsResult<FileRecord> {
        let session = self.browse_sessions.get_mut(&token).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Invreq)
        })?;

        let records = self.data.get(&session.file_name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        // Find current position
        let current_key = session.current_key.as_ref().ok_or_else(|| {
            CicsError::FileError(CicsResponse::Endfile)
        })?;

        // Find record at or after current key
        let pos = records
            .iter()
            .position(|r| r.key >= *current_key)
            .ok_or_else(|| CicsError::FileError(CicsResponse::Endfile))?;

        let record = records[pos].clone();

        // Advance to next
        if pos + 1 < records.len() {
            session.current_key = Some(records[pos + 1].key.clone());
        } else {
            session.current_key = None;
        }

        Ok(record)
    }

    /// Read previous record in browse.
    pub fn readprev(&mut self, token: u32) -> CicsResult<FileRecord> {
        let session = self.browse_sessions.get_mut(&token).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Invreq)
        })?;

        let records = self.data.get(&session.file_name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        // Find current position
        let current_key = session.current_key.as_ref().ok_or_else(|| {
            CicsError::FileError(CicsResponse::Endfile)
        })?;

        // Find record before current key
        let pos = records
            .iter()
            .rposition(|r| r.key < *current_key)
            .ok_or_else(|| CicsError::FileError(CicsResponse::Endfile))?;

        let record = records[pos].clone();

        // Move to previous
        session.current_key = Some(record.key.clone());

        Ok(record)
    }

    /// Reset browse position.
    pub fn resetbr(&mut self, token: u32, key: &[u8]) -> CicsResult<()> {
        let session = self.browse_sessions.get_mut(&token).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Invreq)
        })?;

        session.current_key = Some(key.to_vec());
        Ok(())
    }

    /// End browse session.
    pub fn endbr(&mut self, token: u32) -> CicsResult<()> {
        self.browse_sessions.remove(&token).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Invreq)
        })?;
        Ok(())
    }

    /// Unlock record without update.
    pub fn unlock(&mut self, file_name: &str) -> CicsResult<()> {
        let name = file_name.to_uppercase();
        self.locked_records.remove(&name);
        Ok(())
    }

    /// Get record count for a file.
    pub fn record_count(&self, file_name: &str) -> usize {
        self.data
            .get(&file_name.to_uppercase())
            .map(|r| r.len())
            .unwrap_or(0)
    }
}

impl Default for FileManager {
    fn default() -> Self {
        Self::new()
    }
}

/// VSAM access method for a CICS file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VsamAccessMethod {
    /// Key-Sequenced Data Set — records ordered by primary key.
    Ksds,
    /// Entry-Sequenced Data Set — records in arrival order.
    Esds,
    /// Relative Record Data Set — records by slot number.
    Rrds,
}

/// File Control Table (FCT) entry.
///
/// Defines the mapping between a CICS logical file name and its backing
/// VSAM dataset, including record layout and access permissions.
#[derive(Debug, Clone)]
pub struct FctEntry {
    /// CICS file name (e.g., "CUSTFILE").
    pub file_name: String,
    /// Dataset name (e.g., "MY.CUST.KSDS").
    pub dsname: String,
    /// VSAM access method.
    pub access_method: VsamAccessMethod,
    /// Record length in bytes.
    pub record_length: usize,
    /// Key length in bytes (KSDS only, 0 for ESDS/RRDS).
    pub key_length: usize,
    /// Key position within record (KSDS only).
    pub key_position: usize,
    /// Read access allowed.
    pub read: bool,
    /// Update access allowed.
    pub update: bool,
    /// Add (write new records) access allowed.
    pub add: bool,
    /// Delete access allowed.
    pub delete: bool,
    /// Browse access allowed.
    pub browse: bool,
}

impl FctEntry {
    /// Create a new FCT entry for a KSDS file.
    pub fn ksds(
        file_name: &str,
        dsname: &str,
        record_length: usize,
        key_length: usize,
    ) -> Self {
        Self {
            file_name: file_name.to_uppercase(),
            dsname: dsname.to_string(),
            access_method: VsamAccessMethod::Ksds,
            record_length,
            key_length,
            key_position: 0,
            read: true,
            update: true,
            add: true,
            delete: true,
            browse: true,
        }
    }

    /// Create a new FCT entry for an ESDS file.
    pub fn esds(file_name: &str, dsname: &str, record_length: usize) -> Self {
        Self {
            file_name: file_name.to_uppercase(),
            dsname: dsname.to_string(),
            access_method: VsamAccessMethod::Esds,
            record_length,
            key_length: 0,
            key_position: 0,
            read: true,
            update: false,
            add: true,
            delete: false,
            browse: true,
        }
    }

    /// Create a new FCT entry for an RRDS file.
    pub fn rrds(file_name: &str, dsname: &str, record_length: usize) -> Self {
        Self {
            file_name: file_name.to_uppercase(),
            dsname: dsname.to_string(),
            access_method: VsamAccessMethod::Rrds,
            record_length,
            key_length: 0,
            key_position: 0,
            read: true,
            update: true,
            add: true,
            delete: true,
            browse: true,
        }
    }

    /// Set key position (KSDS).
    pub fn with_key_position(mut self, pos: usize) -> Self {
        self.key_position = pos;
        self
    }

    /// Restrict to read-only access.
    pub fn read_only(mut self) -> Self {
        self.update = false;
        self.add = false;
        self.delete = false;
        self
    }

    /// Convert this FCT entry into a `CicsFile` definition.
    pub fn to_cics_file(&self) -> CicsFile {
        CicsFile {
            name: self.file_name.clone(),
            record_length: self.record_length,
            key_length: self.key_length,
            key_position: self.key_position,
            browse_enabled: self.browse,
            read_enabled: self.read,
            update_enabled: self.update,
            add_enabled: self.add,
            delete_enabled: self.delete,
        }
    }
}

/// Backing store for a single persistent file.
enum BackingStore {
    /// KSDS dataset.
    Ksds(open_mainframe_dataset::Ksds),
    /// ESDS dataset.
    Esds(open_mainframe_dataset::Esds),
    /// RRDS dataset.
    Rrds(open_mainframe_dataset::Rrds),
}

/// Persistent CICS file manager backed by VSAM datasets.
///
/// Unlike the in-memory `FileManager`, this manager persists records to disk
/// via the `open-mainframe-dataset` crate.  Each CICS file maps to a VSAM
/// cluster whose type (KSDS, ESDS, or RRDS) is specified by the FCT entry.
pub struct PersistentFileManager {
    /// File definitions (access control, record layout).
    files: HashMap<String, CicsFile>,
    /// FCT entries for dataset metadata.
    fct: HashMap<String, FctEntry>,
    /// Open VSAM backing stores.
    stores: HashMap<String, BackingStore>,
    /// Records locked for update.
    locked_records: HashMap<String, Vec<u8>>,
    /// Base directory for dataset files.
    base_path: PathBuf,
}

impl PersistentFileManager {
    /// Create a new persistent file manager rooted at `base_path`.
    ///
    /// All VSAM cluster files will be created under this directory.
    pub fn new<P: AsRef<Path>>(base_path: P) -> Self {
        Self {
            files: HashMap::new(),
            fct: HashMap::new(),
            stores: HashMap::new(),
            locked_records: HashMap::new(),
            base_path: base_path.as_ref().to_path_buf(),
        }
    }

    /// Load FCT entries, creating or opening the backing VSAM datasets.
    pub fn load_fct(&mut self, entries: Vec<FctEntry>) -> CicsResult<()> {
        for entry in entries {
            self.register_fct(entry)?;
        }
        Ok(())
    }

    /// Register a single FCT entry.
    pub fn register_fct(&mut self, entry: FctEntry) -> CicsResult<()> {
        let name = entry.file_name.clone();
        let cics_file = entry.to_cics_file();

        // Derive the file path from dsname
        let file_path = self.dataset_path(&entry.dsname);

        // Create or open the backing store
        let store = self.open_or_create_store(&entry, &file_path)?;

        self.files.insert(name.clone(), cics_file);
        self.fct.insert(name.clone(), entry);
        self.stores.insert(name, store);

        Ok(())
    }

    /// Check if a file is registered.
    pub fn exists(&self, name: &str) -> bool {
        self.files.contains_key(&name.to_uppercase())
    }

    /// Get the file definition.
    pub fn get_file(&self, name: &str) -> Option<&CicsFile> {
        self.files.get(&name.to_uppercase())
    }

    /// Get the FCT entry.
    pub fn get_fct(&self, name: &str) -> Option<&FctEntry> {
        self.fct.get(&name.to_uppercase())
    }

    /// Execute READ command against a persistent file.
    pub fn read(
        &mut self,
        file_name: &str,
        key: &[u8],
        mode: FileMode,
    ) -> CicsResult<FileRecord> {
        let name = file_name.to_uppercase();

        let file = self.files.get(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        if !file.read_enabled {
            return Err(CicsError::FileError(CicsResponse::Invreq));
        }
        if mode == FileMode::Update && !file.update_enabled {
            return Err(CicsError::FileError(CicsResponse::Invreq));
        }

        let key_length = file.key_length;
        let key_position = file.key_position;

        let store = self.stores.get_mut(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        let record_data = match store {
            BackingStore::Ksds(ksds) => {
                let result = ksds.read_key(key);
                if result.status == open_mainframe_dataset::FileStatus::RecordNotFound {
                    // Try generic (prefix) match
                    let result = ksds.read_key_generic(key);
                    result.value.ok_or_else(|| CicsError::FileError(CicsResponse::Notfnd))?
                } else {
                    result.value.ok_or_else(|| CicsError::FileError(CicsResponse::Notfnd))?
                }
            }
            BackingStore::Esds(esds) => {
                // For ESDS, the key is treated as an RBA
                let rba = if key.len() >= 8 {
                    u64::from_le_bytes(key[..8].try_into().unwrap_or([0; 8]))
                } else {
                    let mut buf = [0u8; 8];
                    buf[..key.len()].copy_from_slice(key);
                    u64::from_le_bytes(buf)
                };
                let result = esds.read_rba(rba);
                result.value.ok_or_else(|| CicsError::FileError(CicsResponse::Notfnd))?
            }
            BackingStore::Rrds(rrds) => {
                // For RRDS, the key is treated as a slot number
                let slot = if key.len() >= 8 {
                    u64::from_le_bytes(key[..8].try_into().unwrap_or([0; 8]))
                } else {
                    let mut buf = [0u8; 8];
                    buf[..key.len()].copy_from_slice(key);
                    u64::from_le_bytes(buf)
                };
                let result = rrds.read(slot);
                result.value.ok_or_else(|| CicsError::FileError(CicsResponse::Notfnd))?
            }
        };

        // Extract key from record data for KSDS
        let rec_key = if key_length > 0 && key_position + key_length <= record_data.len() {
            record_data[key_position..key_position + key_length].to_vec()
        } else {
            key.to_vec()
        };

        if mode == FileMode::Update {
            self.locked_records.insert(name, rec_key.clone());
        }

        Ok(FileRecord {
            key: rec_key,
            data: record_data,
        })
    }

    /// Execute WRITE command against a persistent file.
    pub fn write(&mut self, file_name: &str, record: FileRecord) -> CicsResult<()> {
        let name = file_name.to_uppercase();

        let file = self.files.get(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        if !file.add_enabled {
            return Err(CicsError::FileError(CicsResponse::Invreq));
        }
        if record.data.len() > file.record_length {
            return Err(CicsError::FileError(CicsResponse::Lengerr));
        }

        let store = self.stores.get_mut(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        match store {
            BackingStore::Ksds(ksds) => {
                // For KSDS, the key is embedded in the record data
                let mut padded = record.data.clone();
                padded.resize(ksds.record_size(), 0);
                let result = ksds.write(&padded);
                if result.status == open_mainframe_dataset::FileStatus::DuplicateKey {
                    return Err(CicsError::FileError(CicsResponse::Duprec));
                }
                if !result.status.is_success() {
                    return Err(CicsError::FileError(CicsResponse::Ioerr));
                }
            }
            BackingStore::Esds(esds) => {
                let result = esds.write(&record.data);
                if !result.status.is_success() {
                    return Err(CicsError::FileError(CicsResponse::Ioerr));
                }
            }
            BackingStore::Rrds(rrds) => {
                // For RRDS, key bytes encode the slot number
                let slot = if record.key.len() >= 8 {
                    u64::from_le_bytes(record.key[..8].try_into().unwrap_or([0; 8]))
                } else {
                    let mut buf = [0u8; 8];
                    buf[..record.key.len()].copy_from_slice(&record.key);
                    u64::from_le_bytes(buf)
                };
                let result = rrds.write(slot, &record.data);
                if result.status == open_mainframe_dataset::FileStatus::DuplicateKey {
                    return Err(CicsError::FileError(CicsResponse::Duprec));
                }
                if !result.status.is_success() {
                    return Err(CicsError::FileError(CicsResponse::Ioerr));
                }
            }
        }

        Ok(())
    }

    /// Execute REWRITE command against a persistent file.
    pub fn rewrite(&mut self, file_name: &str, data: &[u8]) -> CicsResult<()> {
        let name = file_name.to_uppercase();

        let file = self.files.get(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        if !file.update_enabled {
            return Err(CicsError::FileError(CicsResponse::Invreq));
        }
        if data.len() > file.record_length {
            return Err(CicsError::FileError(CicsResponse::Lengerr));
        }

        let _locked_key = self.locked_records.get(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Invreq)
        })?.clone();

        let store = self.stores.get_mut(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        match store {
            BackingStore::Ksds(ksds) => {
                let mut padded = data.to_vec();
                padded.resize(ksds.record_size(), 0);
                let result = ksds.rewrite(&padded);
                if !result.status.is_success() {
                    return Err(CicsError::FileError(CicsResponse::Ioerr));
                }
            }
            BackingStore::Esds(_) => {
                // ESDS does not support rewrite
                return Err(CicsError::FileError(CicsResponse::Invreq));
            }
            BackingStore::Rrds(rrds) => {
                let slot = if _locked_key.len() >= 8 {
                    u64::from_le_bytes(_locked_key[..8].try_into().unwrap_or([0; 8]))
                } else {
                    let mut buf = [0u8; 8];
                    buf[.._locked_key.len()].copy_from_slice(&_locked_key);
                    u64::from_le_bytes(buf)
                };
                let result = rrds.rewrite(slot, data);
                if !result.status.is_success() {
                    return Err(CicsError::FileError(CicsResponse::Ioerr));
                }
            }
        }

        self.locked_records.remove(&name);
        Ok(())
    }

    /// Execute DELETE command against a persistent file.
    pub fn delete(&mut self, file_name: &str, key: Option<&[u8]>) -> CicsResult<()> {
        let name = file_name.to_uppercase();

        let file = self.files.get(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        if !file.delete_enabled {
            return Err(CicsError::FileError(CicsResponse::Invreq));
        }

        let delete_key = if let Some(k) = key {
            k.to_vec()
        } else {
            self.locked_records.get(&name).ok_or_else(|| {
                CicsError::FileError(CicsResponse::Invreq)
            })?.clone()
        };

        let store = self.stores.get_mut(&name).ok_or_else(|| {
            CicsError::FileError(CicsResponse::Filenotfound)
        })?;

        match store {
            BackingStore::Ksds(ksds) => {
                // Position on the record first
                let read_result = ksds.read_key(&delete_key);
                if read_result.status == open_mainframe_dataset::FileStatus::RecordNotFound {
                    return Err(CicsError::FileError(CicsResponse::Notfnd));
                }
                let result = ksds.delete();
                if !result.status.is_success() {
                    return Err(CicsError::FileError(CicsResponse::Ioerr));
                }
            }
            BackingStore::Esds(_) => {
                // ESDS does not support delete
                return Err(CicsError::FileError(CicsResponse::Invreq));
            }
            BackingStore::Rrds(rrds) => {
                let slot = if delete_key.len() >= 8 {
                    u64::from_le_bytes(delete_key[..8].try_into().unwrap_or([0; 8]))
                } else {
                    let mut buf = [0u8; 8];
                    buf[..delete_key.len()].copy_from_slice(&delete_key);
                    u64::from_le_bytes(buf)
                };
                let result = rrds.delete(slot);
                if result.status == open_mainframe_dataset::FileStatus::RecordNotFound {
                    return Err(CicsError::FileError(CicsResponse::Notfnd));
                }
                if !result.status.is_success() {
                    return Err(CicsError::FileError(CicsResponse::Ioerr));
                }
            }
        }

        self.locked_records.remove(&name);
        Ok(())
    }

    /// Unlock a record without update.
    pub fn unlock(&mut self, file_name: &str) -> CicsResult<()> {
        let name = file_name.to_uppercase();
        self.locked_records.remove(&name);
        Ok(())
    }

    /// Close all backing stores, flushing headers to disk.
    pub fn close_all(&mut self) {
        for (_name, store) in self.stores.iter_mut() {
            match store {
                BackingStore::Ksds(ksds) => { let _ = ksds.close(); }
                BackingStore::Esds(esds) => { let _ = esds.close(); }
                BackingStore::Rrds(rrds) => { let _ = rrds.close(); }
            }
        }
    }

    /// Derive a file system path from a dataset name.
    fn dataset_path(&self, dsname: &str) -> PathBuf {
        let sanitized = dsname.replace('.', "/");
        self.base_path.join(format!("{}.vsam", sanitized))
    }

}

impl Drop for PersistentFileManager {
    fn drop(&mut self) {
        self.close_all();
    }
}

impl PersistentFileManager {
    /// Open an existing dataset or create a new one.
    fn open_or_create_store(
        &self,
        entry: &FctEntry,
        path: &Path,
    ) -> CicsResult<BackingStore> {
        match entry.access_method {
            VsamAccessMethod::Ksds => {
                if path.exists() {
                    let ksds = open_mainframe_dataset::Ksds::open_update(path)
                        .map_err(|e| CicsError::Io(std::io::Error::new(
                            std::io::ErrorKind::Other,
                            e.to_string(),
                        )))?;
                    Ok(BackingStore::Ksds(ksds))
                } else {
                    let params = open_mainframe_dataset::ClusterParams::ksds(
                        &entry.dsname,
                        entry.record_length,
                        entry.key_position,
                        entry.key_length,
                    )
                    .with_path(path);
                    let mut ksds = open_mainframe_dataset::Ksds::from_params(params)
                        .map_err(|e| CicsError::Io(std::io::Error::new(
                            std::io::ErrorKind::Other,
                            e.to_string(),
                        )))?;
                    ksds.create().map_err(|e| CicsError::Io(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        e.to_string(),
                    )))?;
                    Ok(BackingStore::Ksds(ksds))
                }
            }
            VsamAccessMethod::Esds => {
                if path.exists() {
                    let esds = open_mainframe_dataset::Esds::open(path)
                        .map_err(|e| CicsError::Io(std::io::Error::new(
                            std::io::ErrorKind::Other,
                            e.to_string(),
                        )))?;
                    Ok(BackingStore::Esds(esds))
                } else {
                    let params = open_mainframe_dataset::ClusterParams::esds(
                        &entry.dsname,
                        entry.record_length,
                    )
                    .with_path(path);
                    let esds = open_mainframe_dataset::Esds::from_params(params)
                        .map_err(|e| CicsError::Io(std::io::Error::new(
                            std::io::ErrorKind::Other,
                            e.to_string(),
                        )))?;
                    Ok(BackingStore::Esds(esds))
                }
            }
            VsamAccessMethod::Rrds => {
                if path.exists() {
                    let rrds = open_mainframe_dataset::Rrds::open(path)
                        .map_err(|e| CicsError::Io(std::io::Error::new(
                            std::io::ErrorKind::Other,
                            e.to_string(),
                        )))?;
                    Ok(BackingStore::Rrds(rrds))
                } else {
                    let params = open_mainframe_dataset::ClusterParams::rrds(
                        &entry.dsname,
                        entry.record_length,
                    )
                    .with_path(path);
                    let rrds = open_mainframe_dataset::Rrds::from_params(params)
                        .map_err(|e| CicsError::Io(std::io::Error::new(
                            std::io::ErrorKind::Other,
                            e.to_string(),
                        )))?;
                    Ok(BackingStore::Rrds(rrds))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup_test_file() -> FileManager {
        let mut fm = FileManager::new();
        let file = CicsFile::new("CUSTFILE", 100, 6);
        fm.register(file);

        // Load test data
        let records = vec![
            FileRecord::from_strings("000001", "Customer 1 data"),
            FileRecord::from_strings("000002", "Customer 2 data"),
            FileRecord::from_strings("000003", "Customer 3 data"),
        ];
        fm.load_data("CUSTFILE", records);

        fm
    }

    #[test]
    fn test_file_registration() {
        let mut fm = FileManager::new();
        let file = CicsFile::new("TESTFILE", 80, 4);
        fm.register(file);

        assert!(fm.exists("TESTFILE"));
        assert!(fm.exists("testfile")); // case insensitive
        assert!(!fm.exists("NOFILE"));
    }

    #[test]
    fn test_read_success() {
        let mut fm = setup_test_file();

        let record = fm.read("CUSTFILE", b"000001", FileMode::Read).unwrap();
        assert_eq!(record.key_string(), "000001");
        assert!(record.data_string().contains("Customer 1"));
    }

    #[test]
    fn test_read_not_found() {
        let mut fm = setup_test_file();

        let result = fm.read("CUSTFILE", b"999999", FileMode::Read);
        assert!(matches!(result, Err(CicsError::FileError(CicsResponse::Notfnd))));
    }

    #[test]
    fn test_read_for_update() {
        let mut fm = setup_test_file();

        let _record = fm.read("CUSTFILE", b"000002", FileMode::Update).unwrap();

        // Record should be locked
        assert!(fm.locked_records.contains_key("CUSTFILE"));
    }

    #[test]
    fn test_write_success() {
        let mut fm = setup_test_file();

        let record = FileRecord::from_strings("000004", "New customer data");
        fm.write("CUSTFILE", record).unwrap();

        assert_eq!(fm.record_count("CUSTFILE"), 4);
    }

    #[test]
    fn test_write_duplicate() {
        let mut fm = setup_test_file();

        let record = FileRecord::from_strings("000001", "Duplicate key");
        let result = fm.write("CUSTFILE", record);

        assert!(matches!(result, Err(CicsError::FileError(CicsResponse::Duprec))));
    }

    #[test]
    fn test_rewrite() {
        let mut fm = setup_test_file();

        // Read for update
        let _record = fm.read("CUSTFILE", b"000002", FileMode::Update).unwrap();

        // Rewrite
        fm.rewrite("CUSTFILE", b"Updated customer 2 data").unwrap();

        // Verify update
        let updated = fm.read("CUSTFILE", b"000002", FileMode::Read).unwrap();
        assert!(updated.data_string().contains("Updated"));
    }

    #[test]
    fn test_rewrite_without_lock() {
        let mut fm = setup_test_file();

        // Try to rewrite without read for update
        let result = fm.rewrite("CUSTFILE", b"New data");

        assert!(matches!(result, Err(CicsError::FileError(CicsResponse::Invreq))));
    }

    #[test]
    fn test_delete_with_key() {
        let mut fm = setup_test_file();

        fm.delete("CUSTFILE", Some(b"000002")).unwrap();

        assert_eq!(fm.record_count("CUSTFILE"), 2);

        // Verify record is gone
        let result = fm.read("CUSTFILE", b"000002", FileMode::Read);
        assert!(matches!(result, Err(CicsError::FileError(CicsResponse::Notfnd))));
    }

    #[test]
    fn test_delete_after_read_for_update() {
        let mut fm = setup_test_file();

        // Read for update
        let _record = fm.read("CUSTFILE", b"000003", FileMode::Update).unwrap();

        // Delete without key (uses locked record)
        fm.delete("CUSTFILE", None).unwrap();

        assert_eq!(fm.record_count("CUSTFILE"), 2);
    }

    #[test]
    fn test_browse_forward() {
        let mut fm = setup_test_file();

        // Start browse
        let token = fm.startbr("CUSTFILE", b"000001").unwrap();

        // Read first
        let rec1 = fm.readnext(token).unwrap();
        assert_eq!(rec1.key_string(), "000001");

        // Read second
        let rec2 = fm.readnext(token).unwrap();
        assert_eq!(rec2.key_string(), "000002");

        // Read third
        let rec3 = fm.readnext(token).unwrap();
        assert_eq!(rec3.key_string(), "000003");

        // End of file
        let result = fm.readnext(token);
        assert!(matches!(result, Err(CicsError::FileError(CicsResponse::Endfile))));

        // End browse
        fm.endbr(token).unwrap();
    }

    #[test]
    fn test_browse_backward() {
        let mut fm = setup_test_file();

        // Start browse at end
        let token = fm.startbr("CUSTFILE", b"999999").unwrap();

        // Read last (before our position)
        let rec = fm.readprev(token).unwrap();
        assert_eq!(rec.key_string(), "000003");

        // Read previous
        let rec = fm.readprev(token).unwrap();
        assert_eq!(rec.key_string(), "000002");

        fm.endbr(token).unwrap();
    }

    #[test]
    fn test_resetbr() {
        let mut fm = setup_test_file();

        let token = fm.startbr("CUSTFILE", b"000001").unwrap();

        // Read first
        let _rec1 = fm.readnext(token).unwrap();

        // Reset to different position
        fm.resetbr(token, b"000003").unwrap();

        // Read should return record at new position
        let rec = fm.readnext(token).unwrap();
        assert_eq!(rec.key_string(), "000003");

        fm.endbr(token).unwrap();
    }

    #[test]
    fn test_unlock() {
        let mut fm = setup_test_file();

        // Read for update
        let _record = fm.read("CUSTFILE", b"000001", FileMode::Update).unwrap();
        assert!(fm.locked_records.contains_key("CUSTFILE"));

        // Unlock
        fm.unlock("CUSTFILE").unwrap();
        assert!(!fm.locked_records.contains_key("CUSTFILE"));
    }

    #[test]
    fn test_file_not_found() {
        let mut fm = FileManager::new();

        let result = fm.read("NOFILE", b"KEY", FileMode::Read);
        assert!(matches!(result, Err(CicsError::FileError(CicsResponse::Filenotfound))));
    }

    #[test]
    fn test_disabled_operations() {
        let mut fm = FileManager::new();
        let file = CicsFile::new("READONLY", 80, 4).disable_update();
        fm.register(file);
        fm.load_data("READONLY", vec![FileRecord::from_strings("0001", "Data")]);

        // Read should work
        let _rec = fm.read("READONLY", b"0001", FileMode::Read).unwrap();

        // Read for update should fail
        let result = fm.read("READONLY", b"0001", FileMode::Update);
        assert!(matches!(result, Err(CicsError::FileError(CicsResponse::Invreq))));
    }

    // --- FCT Entry Tests (Story 202.2) ---

    #[test]
    fn test_fct_entry_ksds() {
        let entry = FctEntry::ksds("CUSTFILE", "MY.CUST.KSDS", 200, 8);
        assert_eq!(entry.file_name, "CUSTFILE");
        assert_eq!(entry.dsname, "MY.CUST.KSDS");
        assert_eq!(entry.access_method, VsamAccessMethod::Ksds);
        assert_eq!(entry.record_length, 200);
        assert_eq!(entry.key_length, 8);
        assert!(entry.read);
        assert!(entry.update);
        assert!(entry.add);
        assert!(entry.delete);
        assert!(entry.browse);
    }

    #[test]
    fn test_fct_entry_esds() {
        let entry = FctEntry::esds("LOGFILE", "MY.LOG.ESDS", 500);
        assert_eq!(entry.access_method, VsamAccessMethod::Esds);
        assert_eq!(entry.key_length, 0);
        assert!(entry.read);
        assert!(!entry.update); // ESDS no update
        assert!(entry.add);
        assert!(!entry.delete); // ESDS no delete
    }

    #[test]
    fn test_fct_entry_rrds() {
        let entry = FctEntry::rrds("SLOTFILE", "MY.SLOT.RRDS", 100);
        assert_eq!(entry.access_method, VsamAccessMethod::Rrds);
        assert!(entry.update);
        assert!(entry.delete);
    }

    #[test]
    fn test_fct_read_only() {
        let entry = FctEntry::ksds("READONLY", "MY.RO.KSDS", 200, 8).read_only();
        assert!(entry.read);
        assert!(!entry.update);
        assert!(!entry.add);
        assert!(!entry.delete);
    }

    #[test]
    fn test_fct_key_position() {
        let entry = FctEntry::ksds("F", "D", 200, 8).with_key_position(10);
        assert_eq!(entry.key_position, 10);
    }

    #[test]
    fn test_fct_to_cics_file() {
        let entry = FctEntry::ksds("CUSTFILE", "MY.CUST.KSDS", 200, 8)
            .with_key_position(4)
            .read_only();
        let file = entry.to_cics_file();
        assert_eq!(file.name, "CUSTFILE");
        assert_eq!(file.record_length, 200);
        assert_eq!(file.key_length, 8);
        assert_eq!(file.key_position, 4);
        assert!(file.read_enabled);
        assert!(!file.update_enabled);
        assert!(!file.add_enabled);
    }

    // --- PersistentFileManager Tests (Story 202.1) ---

    use std::sync::atomic::{AtomicUsize, Ordering};

    static PERSIST_COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn persist_test_dir(name: &str) -> PathBuf {
        let count = PERSIST_COUNTER.fetch_add(1, Ordering::SeqCst);
        std::env::temp_dir().join(format!("cics_persist_{}_{}", name, count))
    }

    fn cleanup_dir(path: &Path) {
        let _ = std::fs::remove_dir_all(path);
    }

    #[test]
    fn test_persistent_ksds_write_and_read() {
        let dir = persist_test_dir("ksds_wr");
        let _ = std::fs::create_dir_all(&dir);

        let entry = FctEntry::ksds("CUSTFILE", "MY.CUST.KSDS", 100, 10);
        let mut pfm = PersistentFileManager::new(&dir);
        pfm.register_fct(entry).unwrap();

        assert!(pfm.exists("CUSTFILE"));

        // Build a record: key at offset 0, length 10
        let mut data = vec![0u8; 100];
        data[0..10].copy_from_slice(b"KEY0000001");
        data[10..20].copy_from_slice(b"CUST DATA ");

        let record = FileRecord::new(b"KEY0000001".to_vec(), data);
        pfm.write("CUSTFILE", record).unwrap();

        // Read it back
        let rec = pfm.read("CUSTFILE", b"KEY0000001", FileMode::Read).unwrap();
        assert_eq!(&rec.key, b"KEY0000001");
        assert_eq!(&rec.data[10..20], b"CUST DATA ");

        cleanup_dir(&dir);
    }

    #[test]
    fn test_persistent_ksds_duplicate_key() {
        let dir = persist_test_dir("ksds_dup");
        let _ = std::fs::create_dir_all(&dir);

        let entry = FctEntry::ksds("FILE1", "MY.DUP.KSDS", 50, 6);
        let mut pfm = PersistentFileManager::new(&dir);
        pfm.register_fct(entry).unwrap();

        let mut data = vec![0u8; 50];
        data[0..6].copy_from_slice(b"KEY001");

        pfm.write("FILE1", FileRecord::new(b"KEY001".to_vec(), data.clone())).unwrap();
        let result = pfm.write("FILE1", FileRecord::new(b"KEY001".to_vec(), data));
        assert!(matches!(result, Err(CicsError::FileError(CicsResponse::Duprec))));

        cleanup_dir(&dir);
    }

    #[test]
    fn test_persistent_ksds_not_found() {
        let dir = persist_test_dir("ksds_nf");
        let _ = std::fs::create_dir_all(&dir);

        let entry = FctEntry::ksds("FILE1", "MY.NF.KSDS", 50, 6);
        let mut pfm = PersistentFileManager::new(&dir);
        pfm.register_fct(entry).unwrap();

        let result = pfm.read("FILE1", b"NOKEY!", FileMode::Read);
        assert!(matches!(result, Err(CicsError::FileError(CicsResponse::Notfnd))));

        cleanup_dir(&dir);
    }

    #[test]
    fn test_persistent_ksds_rewrite() {
        let dir = persist_test_dir("ksds_rw");
        let _ = std::fs::create_dir_all(&dir);

        let entry = FctEntry::ksds("FILE1", "MY.RW.KSDS", 50, 6);
        let mut pfm = PersistentFileManager::new(&dir);
        pfm.register_fct(entry).unwrap();

        let mut data = vec![0u8; 50];
        data[0..6].copy_from_slice(b"KEY001");
        data[6..16].copy_from_slice(b"OLD VALUE ");
        pfm.write("FILE1", FileRecord::new(b"KEY001".to_vec(), data)).unwrap();

        // Read for update
        let _rec = pfm.read("FILE1", b"KEY001", FileMode::Update).unwrap();

        // Rewrite with new value (same key)
        let mut new_data = vec![0u8; 50];
        new_data[0..6].copy_from_slice(b"KEY001");
        new_data[6..16].copy_from_slice(b"NEW VALUE ");
        pfm.rewrite("FILE1", &new_data).unwrap();

        // Verify
        let rec = pfm.read("FILE1", b"KEY001", FileMode::Read).unwrap();
        assert_eq!(&rec.data[6..16], b"NEW VALUE ");

        cleanup_dir(&dir);
    }

    #[test]
    fn test_persistent_ksds_delete() {
        let dir = persist_test_dir("ksds_del");
        let _ = std::fs::create_dir_all(&dir);

        let entry = FctEntry::ksds("FILE1", "MY.DEL.KSDS", 50, 6);
        let mut pfm = PersistentFileManager::new(&dir);
        pfm.register_fct(entry).unwrap();

        let mut data = vec![0u8; 50];
        data[0..6].copy_from_slice(b"KEY001");
        pfm.write("FILE1", FileRecord::new(b"KEY001".to_vec(), data)).unwrap();

        // Delete by key
        pfm.delete("FILE1", Some(b"KEY001")).unwrap();

        // Verify gone
        let result = pfm.read("FILE1", b"KEY001", FileMode::Read);
        assert!(matches!(result, Err(CicsError::FileError(CicsResponse::Notfnd))));

        cleanup_dir(&dir);
    }

    #[test]
    fn test_persistent_data_survives_new_transaction() {
        // This tests the core acceptance criterion: data persists across
        // transactions (separate PersistentFileManager instances).
        let dir = persist_test_dir("ksds_persist");
        let _ = std::fs::create_dir_all(&dir);

        // Transaction 1: write a record
        {
            let entry = FctEntry::ksds("CUSTFILE", "MY.CUST.KSDS", 100, 10);
            let mut pfm = PersistentFileManager::new(&dir);
            pfm.register_fct(entry).unwrap();

            let mut data = vec![0u8; 100];
            data[0..10].copy_from_slice(b"KEY0000001");
            data[10..24].copy_from_slice(b"Customer Data!");
            pfm.write("CUSTFILE", FileRecord::new(b"KEY0000001".to_vec(), data)).unwrap();
        }
        // PersistentFileManager dropped — simulates transaction end

        // Transaction 2: read the record back
        {
            let entry = FctEntry::ksds("CUSTFILE", "MY.CUST.KSDS", 100, 10);
            let mut pfm = PersistentFileManager::new(&dir);
            pfm.register_fct(entry).unwrap();

            let rec = pfm.read("CUSTFILE", b"KEY0000001", FileMode::Read).unwrap();
            assert_eq!(&rec.data[10..24], b"Customer Data!");
        }

        cleanup_dir(&dir);
    }

    #[test]
    fn test_persistent_file_not_registered() {
        let dir = persist_test_dir("notfound");
        let _ = std::fs::create_dir_all(&dir);

        let mut pfm = PersistentFileManager::new(&dir);
        let result = pfm.read("NOPE", b"KEY", FileMode::Read);
        assert!(matches!(result, Err(CicsError::FileError(CicsResponse::Filenotfound))));

        cleanup_dir(&dir);
    }

    #[test]
    fn test_persistent_rewrite_without_lock() {
        let dir = persist_test_dir("nolock");
        let _ = std::fs::create_dir_all(&dir);

        let entry = FctEntry::ksds("FILE1", "MY.NL.KSDS", 50, 6);
        let mut pfm = PersistentFileManager::new(&dir);
        pfm.register_fct(entry).unwrap();

        let result = pfm.rewrite("FILE1", b"whatever");
        assert!(matches!(result, Err(CicsError::FileError(CicsResponse::Invreq))));

        cleanup_dir(&dir);
    }

    #[test]
    fn test_persistent_load_fct_batch() {
        let dir = persist_test_dir("batch_fct");
        let _ = std::fs::create_dir_all(&dir);

        let entries = vec![
            FctEntry::ksds("FILE1", "MY.F1.KSDS", 100, 8),
            FctEntry::ksds("FILE2", "MY.F2.KSDS", 200, 10),
            FctEntry::esds("LOG1", "MY.L1.ESDS", 500),
        ];

        let mut pfm = PersistentFileManager::new(&dir);
        pfm.load_fct(entries).unwrap();

        assert!(pfm.exists("FILE1"));
        assert!(pfm.exists("FILE2"));
        assert!(pfm.exists("LOG1"));
        assert!(!pfm.exists("FILE3"));

        // Verify FCT metadata
        let fct = pfm.get_fct("FILE1").unwrap();
        assert_eq!(fct.dsname, "MY.F1.KSDS");
        assert_eq!(fct.record_length, 100);

        let fct2 = pfm.get_fct("LOG1").unwrap();
        assert_eq!(fct2.access_method, VsamAccessMethod::Esds);

        cleanup_dir(&dir);
    }

    #[test]
    fn test_persistent_read_only_prevents_write() {
        let dir = persist_test_dir("ro_write");
        let _ = std::fs::create_dir_all(&dir);

        let entry = FctEntry::ksds("FILE1", "MY.RO.KSDS", 50, 6).read_only();
        let mut pfm = PersistentFileManager::new(&dir);
        pfm.register_fct(entry).unwrap();

        let mut data = vec![0u8; 50];
        data[0..6].copy_from_slice(b"KEY001");
        let result = pfm.write("FILE1", FileRecord::new(b"KEY001".to_vec(), data));
        assert!(matches!(result, Err(CicsError::FileError(CicsResponse::Invreq))));

        cleanup_dir(&dir);
    }

    #[test]
    fn test_persistent_unlock() {
        let dir = persist_test_dir("unlock");
        let _ = std::fs::create_dir_all(&dir);

        let entry = FctEntry::ksds("FILE1", "MY.UL.KSDS", 50, 6);
        let mut pfm = PersistentFileManager::new(&dir);
        pfm.register_fct(entry).unwrap();

        let mut data = vec![0u8; 50];
        data[0..6].copy_from_slice(b"KEY001");
        pfm.write("FILE1", FileRecord::new(b"KEY001".to_vec(), data)).unwrap();

        // Read for update
        let _rec = pfm.read("FILE1", b"KEY001", FileMode::Update).unwrap();
        assert!(pfm.locked_records.contains_key("FILE1"));

        // Unlock
        pfm.unlock("FILE1").unwrap();
        assert!(!pfm.locked_records.contains_key("FILE1"));

        cleanup_dir(&dir);
    }
}
