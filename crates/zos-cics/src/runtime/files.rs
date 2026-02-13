//! CICS file operations.
//!
//! Implements READ, WRITE, REWRITE, DELETE file commands.

use crate::{CicsError, CicsResponse, CicsResult};
use std::collections::HashMap;

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
}
