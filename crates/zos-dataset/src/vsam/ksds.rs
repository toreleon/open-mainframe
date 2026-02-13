//! KSDS (Key-Sequenced Data Set) implementation.
//!
//! KSDS is a VSAM file organization where records are stored in key sequence.
//! Each record has a unique primary key that determines its logical position.

use std::fs::{File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::Path;

use super::btree::BPlusTree;
use super::cluster::{ClusterParams, KeySpec, VsamCluster, VsamType};
use crate::error::DatasetError;

/// VSAM file status codes following IBM conventions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileStatus {
    /// Operation successful.
    Success = 0,
    /// End of file reached during sequential read.
    EndOfFile = 10,
    /// Duplicate key on write.
    DuplicateKey = 22,
    /// Record not found.
    RecordNotFound = 23,
    /// Key sequence error (key changed on rewrite).
    KeySequenceError = 21,
    /// No current record (REWRITE/DELETE without prior READ).
    NoCurrentRecord = 43,
    /// Record length error.
    RecordLengthError = 44,
    /// File not open.
    FileNotOpen = 41,
    /// Logic error (operation not valid for file state).
    LogicError = 92,
    /// I/O error.
    IoError = 90,
}

impl FileStatus {
    /// Returns the two-digit status code string.
    pub fn code(&self) -> &'static str {
        match self {
            FileStatus::Success => "00",
            FileStatus::EndOfFile => "10",
            FileStatus::KeySequenceError => "21",
            FileStatus::DuplicateKey => "22",
            FileStatus::RecordNotFound => "23",
            FileStatus::NoCurrentRecord => "43",
            FileStatus::RecordLengthError => "44",
            FileStatus::FileNotOpen => "41",
            FileStatus::LogicError => "92",
            FileStatus::IoError => "90",
        }
    }

    /// Returns true if the status indicates success.
    pub fn is_success(&self) -> bool {
        matches!(self, FileStatus::Success)
    }
}

/// Result of a KSDS operation, including file status.
#[derive(Debug)]
pub struct KsdsResult<T> {
    /// The result value (if any).
    pub value: Option<T>,
    /// The file status code.
    pub status: FileStatus,
}

impl<T> KsdsResult<T> {
    /// Creates a successful result.
    pub fn success(value: T) -> Self {
        Self {
            value: Some(value),
            status: FileStatus::Success,
        }
    }

    /// Creates a result with no value.
    pub fn empty(status: FileStatus) -> Self {
        Self {
            value: None,
            status,
        }
    }

    /// Creates a successful empty result.
    pub fn ok() -> Self {
        Self {
            value: None,
            status: FileStatus::Success,
        }
    }
}

/// Header size for the cluster file.
const HEADER_SIZE: u64 = 128;

/// A Key-Sequenced Data Set (KSDS).
///
/// KSDS provides indexed access to records by their primary key.
/// Records are stored in key sequence and can be accessed:
/// - By exact key (point lookup)
/// - By partial key (generic read)
/// - Sequentially (in key order)
#[derive(Debug)]
pub struct Ksds {
    /// The underlying VSAM cluster.
    cluster: VsamCluster,
    /// In-memory B+ tree index mapping keys to file offsets.
    index: BPlusTree<Vec<u8>, u64>,
    /// Last file status from an operation.
    last_status: FileStatus,
    /// Current key for sequential reads and rewrite/delete.
    current_key: Option<Vec<u8>>,
    /// Current record offset for rewrite/delete.
    current_offset: Option<u64>,
    /// Iterator position for sequential reads.
    sequential_keys: Option<Vec<Vec<u8>>>,
    /// Current index in sequential keys.
    sequential_index: usize,
    /// Whether the file is open for writing.
    write_mode: bool,
}

impl Ksds {
    /// Creates a new KSDS with the given parameters.
    ///
    /// This creates the cluster definition but does not create the file on disk.
    /// Call `create()` to actually create the file.
    pub fn new(
        name: &str,
        record_size: usize,
        key_offset: usize,
        key_length: usize,
    ) -> Result<Self, DatasetError> {
        let cluster = VsamCluster::new_ksds(name, record_size, key_offset, key_length)?;

        Ok(Self {
            cluster,
            index: BPlusTree::new(),
            last_status: FileStatus::Success,
            current_key: None,
            current_offset: None,
            sequential_keys: None,
            sequential_index: 0,
            write_mode: false,
        })
    }

    /// Creates a new KSDS from cluster parameters.
    pub fn from_params(params: ClusterParams) -> Result<Self, DatasetError> {
        if params.vsam_type != VsamType::Ksds {
            return Err(DatasetError::InvalidParameter(
                "KSDS requires KSDS cluster type".to_string(),
            ));
        }

        let cluster = VsamCluster::new(params)?;

        Ok(Self {
            cluster,
            index: BPlusTree::new(),
            last_status: FileStatus::Success,
            current_key: None,
            current_offset: None,
            sequential_keys: None,
            sequential_index: 0,
            write_mode: false,
        })
    }

    /// Creates the KSDS file on disk.
    pub fn create(&mut self) -> Result<(), DatasetError> {
        self.cluster.create()?;
        self.write_mode = true;
        Ok(())
    }

    /// Opens an existing KSDS for reading.
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Self, DatasetError> {
        let cluster = VsamCluster::open(&path)?;

        if cluster.vsam_type() != VsamType::Ksds {
            return Err(DatasetError::InvalidParameter(
                "File is not a KSDS".to_string(),
            ));
        }

        let mut ksds = Self {
            cluster,
            index: BPlusTree::new(),
            last_status: FileStatus::Success,
            current_key: None,
            current_offset: None,
            sequential_keys: None,
            sequential_index: 0,
            write_mode: false,
        };

        // Load index from file
        ksds.load_index()?;

        Ok(ksds)
    }

    /// Opens an existing KSDS for update (read/write).
    pub fn open_update<P: AsRef<Path>>(path: P) -> Result<Self, DatasetError> {
        let mut ksds = Self::open(path)?;
        ksds.write_mode = true;
        Ok(ksds)
    }

    /// Loads the index by reading all records from the file.
    fn load_index(&mut self) -> Result<(), DatasetError> {
        let key_spec = self.cluster.key_spec().ok_or_else(|| {
            DatasetError::InvalidParameter("KSDS missing key specification".to_string())
        })?;

        let record_size = self.cluster.record_size();
        let record_count = self.cluster.record_count();

        if record_count == 0 {
            return Ok(());
        }

        let mut file = File::open(self.cluster.path())
            .map_err(|e| DatasetError::IoError { message: e.to_string() })?;

        // Skip header
        file.seek(SeekFrom::Start(HEADER_SIZE))
            .map_err(|e| DatasetError::IoError { message: e.to_string() })?;

        let mut buffer = vec![0u8; record_size];

        for i in 0..record_count {
            let offset = HEADER_SIZE + (i * record_size as u64);

            file.seek(SeekFrom::Start(offset))
                .map_err(|e| DatasetError::IoError { message: e.to_string() })?;

            let bytes_read = file
                .read(&mut buffer)
                .map_err(|e| DatasetError::IoError { message: e.to_string() })?;

            if bytes_read < record_size {
                break;
            }

            // Extract key and add to index
            let key = key_spec.extract_key(&buffer).to_vec();
            self.index.insert(key, offset);
        }

        Ok(())
    }

    /// Reads a record by exact key.
    ///
    /// Returns the record if found, or None if not found.
    /// Sets file status 00 on success, 23 if record not found.
    pub fn read_key(&mut self, key: &[u8]) -> KsdsResult<Vec<u8>> {
        if !self.cluster.is_open() {
            self.last_status = FileStatus::FileNotOpen;
            return KsdsResult::empty(FileStatus::FileNotOpen);
        }

        let key_vec = key.to_vec();

        match self.index.get(&key_vec) {
            Some(&offset) => {
                match self.read_record_at(offset) {
                    Ok(record) => {
                        self.last_status = FileStatus::Success;
                        self.current_key = Some(key_vec);
                        self.current_offset = Some(offset);
                        KsdsResult::success(record)
                    }
                    Err(_) => {
                        self.last_status = FileStatus::IoError;
                        self.current_key = None;
                        self.current_offset = None;
                        KsdsResult::empty(FileStatus::IoError)
                    }
                }
            }
            None => {
                self.last_status = FileStatus::RecordNotFound;
                self.current_key = None;
                self.current_offset = None;
                KsdsResult::empty(FileStatus::RecordNotFound)
            }
        }
    }

    /// Reads a record by partial key (generic read).
    ///
    /// Returns the first record where the key starts with the given prefix.
    /// Sets file status 00 on success, 23 if no matching record found.
    pub fn read_key_generic(&mut self, partial_key: &[u8]) -> KsdsResult<Vec<u8>> {
        if !self.cluster.is_open() {
            self.last_status = FileStatus::FileNotOpen;
            return KsdsResult::empty(FileStatus::FileNotOpen);
        }

        // Find the first key that starts with the partial key
        let all_records = self.index.iter();

        for (key, offset) in all_records {
            if key.starts_with(partial_key) {
                match self.read_record_at(offset) {
                    Ok(record) => {
                        self.last_status = FileStatus::Success;
                        self.current_key = Some(key);
                        self.current_offset = Some(offset);
                        return KsdsResult::success(record);
                    }
                    Err(_) => {
                        self.last_status = FileStatus::IoError;
                        self.current_key = None;
                        self.current_offset = None;
                        return KsdsResult::empty(FileStatus::IoError);
                    }
                }
            }
        }

        self.last_status = FileStatus::RecordNotFound;
        KsdsResult::empty(FileStatus::RecordNotFound)
    }

    /// Reads a record at the given file offset.
    fn read_record_at(&self, offset: u64) -> Result<Vec<u8>, DatasetError> {
        let mut file = File::open(self.cluster.path())
            .map_err(|e| DatasetError::IoError { message: e.to_string() })?;

        let record_size = self.cluster.record_size();
        let mut buffer = vec![0u8; record_size];

        file.seek(SeekFrom::Start(offset))
            .map_err(|e| DatasetError::IoError { message: e.to_string() })?;

        file.read_exact(&mut buffer)
            .map_err(|e| DatasetError::IoError { message: e.to_string() })?;

        Ok(buffer)
    }

    /// Writes a record to the KSDS.
    ///
    /// The key is extracted from the record using the key specification.
    /// Sets file status 00 on success, 22 if duplicate key.
    pub fn write(&mut self, record: &[u8]) -> KsdsResult<()> {
        if !self.cluster.is_open() {
            self.last_status = FileStatus::FileNotOpen;
            return KsdsResult::empty(FileStatus::FileNotOpen);
        }

        if !self.write_mode {
            self.last_status = FileStatus::LogicError;
            return KsdsResult::empty(FileStatus::LogicError);
        }

        let record_size = self.cluster.record_size();
        if record.len() > record_size {
            self.last_status = FileStatus::RecordLengthError;
            return KsdsResult::empty(FileStatus::RecordLengthError);
        }

        let key_spec = match self.cluster.key_spec() {
            Some(ks) => ks,
            None => {
                self.last_status = FileStatus::LogicError;
                return KsdsResult::empty(FileStatus::LogicError);
            }
        };

        // Extract key from record
        let key = key_spec.extract_key(record).to_vec();

        // Check for duplicate key
        if self.index.contains_key(&key) {
            self.last_status = FileStatus::DuplicateKey;
            return KsdsResult::empty(FileStatus::DuplicateKey);
        }

        // Pad record if necessary
        let mut padded_record = record.to_vec();
        padded_record.resize(record_size, 0);

        // Calculate offset for new record
        let offset = HEADER_SIZE + (self.cluster.record_count() * record_size as u64);

        // Write record to file
        match self.write_record_at(offset, &padded_record) {
            Ok(()) => {
                // Update index and record count
                self.index.insert(key.clone(), offset);
                self.cluster.increment_record_count();
                self.current_key = Some(key);
                self.current_offset = Some(offset);
                self.last_status = FileStatus::Success;
                KsdsResult::ok()
            }
            Err(_) => {
                self.last_status = FileStatus::IoError;
                self.current_key = None;
                self.current_offset = None;
                KsdsResult::empty(FileStatus::IoError)
            }
        }
    }

    /// Writes a record at the given file offset.
    fn write_record_at(&self, offset: u64, record: &[u8]) -> Result<(), DatasetError> {
        let mut file = OpenOptions::new()
            .write(true)
            .open(self.cluster.path())
            .map_err(|e| DatasetError::IoError { message: e.to_string() })?;

        file.seek(SeekFrom::Start(offset))
            .map_err(|e| DatasetError::IoError { message: e.to_string() })?;

        file.write_all(record)
            .map_err(|e| DatasetError::IoError { message: e.to_string() })?;

        Ok(())
    }

    /// Returns the last file status code.
    pub fn status(&self) -> FileStatus {
        self.last_status
    }

    /// Returns the record count.
    pub fn record_count(&self) -> u64 {
        self.cluster.record_count()
    }

    /// Returns the record size.
    pub fn record_size(&self) -> usize {
        self.cluster.record_size()
    }

    /// Returns the key specification.
    pub fn key_spec(&self) -> Option<&KeySpec> {
        self.cluster.key_spec()
    }

    /// Closes the KSDS, flushing any pending changes.
    pub fn close(&mut self) -> Result<(), DatasetError> {
        self.cluster.close()
    }

    /// Rewrites the current record (must have been read first).
    ///
    /// The key cannot be changed. If the key in the new record differs
    /// from the current key, file status 21 is returned.
    pub fn rewrite(&mut self, record: &[u8]) -> KsdsResult<()> {
        if !self.cluster.is_open() {
            self.last_status = FileStatus::FileNotOpen;
            return KsdsResult::empty(FileStatus::FileNotOpen);
        }

        if !self.write_mode {
            self.last_status = FileStatus::LogicError;
            return KsdsResult::empty(FileStatus::LogicError);
        }

        // Check for current record
        let (current_key, current_offset) = match (&self.current_key, self.current_offset) {
            (Some(key), Some(offset)) => (key.clone(), offset),
            _ => {
                self.last_status = FileStatus::NoCurrentRecord;
                return KsdsResult::empty(FileStatus::NoCurrentRecord);
            }
        };

        let record_size = self.cluster.record_size();
        if record.len() > record_size {
            self.last_status = FileStatus::RecordLengthError;
            return KsdsResult::empty(FileStatus::RecordLengthError);
        }

        let key_spec = match self.cluster.key_spec() {
            Some(ks) => ks,
            None => {
                self.last_status = FileStatus::LogicError;
                return KsdsResult::empty(FileStatus::LogicError);
            }
        };

        // Check that key hasn't changed
        let new_key = key_spec.extract_key(record).to_vec();
        if new_key != current_key {
            self.last_status = FileStatus::KeySequenceError;
            return KsdsResult::empty(FileStatus::KeySequenceError);
        }

        // Pad record if necessary
        let mut padded_record = record.to_vec();
        padded_record.resize(record_size, 0);

        // Write record at current offset
        match self.write_record_at(current_offset, &padded_record) {
            Ok(()) => {
                self.last_status = FileStatus::Success;
                KsdsResult::ok()
            }
            Err(_) => {
                self.last_status = FileStatus::IoError;
                KsdsResult::empty(FileStatus::IoError)
            }
        }
    }

    /// Deletes the current record (must have been read first).
    ///
    /// The record is removed from the index. Note: this implementation
    /// marks the record as deleted but doesn't reclaim space.
    pub fn delete(&mut self) -> KsdsResult<()> {
        if !self.cluster.is_open() {
            self.last_status = FileStatus::FileNotOpen;
            return KsdsResult::empty(FileStatus::FileNotOpen);
        }

        if !self.write_mode {
            self.last_status = FileStatus::LogicError;
            return KsdsResult::empty(FileStatus::LogicError);
        }

        // Check for current record
        let current_key = match &self.current_key {
            Some(key) => key.clone(),
            None => {
                self.last_status = FileStatus::NoCurrentRecord;
                return KsdsResult::empty(FileStatus::NoCurrentRecord);
            }
        };

        // Remove from index
        self.index.remove(&current_key);

        // Clear current position
        self.current_key = None;
        self.current_offset = None;

        self.last_status = FileStatus::Success;
        KsdsResult::ok()
    }

    /// Positions for sequential reading at the first record >= key.
    ///
    /// After START, use read_next() to read records sequentially.
    pub fn start(&mut self, key: &[u8]) -> KsdsResult<()> {
        if !self.cluster.is_open() {
            self.last_status = FileStatus::FileNotOpen;
            return KsdsResult::empty(FileStatus::FileNotOpen);
        }

        // Get all keys in order
        let all_records = self.index.iter();
        let keys: Vec<Vec<u8>> = all_records.into_iter().map(|(k, _)| k).collect();

        // Find first key >= the given key
        let start_idx = keys.iter().position(|k| k.as_slice() >= key);

        match start_idx {
            Some(idx) => {
                self.sequential_keys = Some(keys);
                self.sequential_index = idx;
                self.last_status = FileStatus::Success;
                KsdsResult::ok()
            }
            None => {
                self.sequential_keys = None;
                self.sequential_index = 0;
                self.last_status = FileStatus::RecordNotFound;
                KsdsResult::empty(FileStatus::RecordNotFound)
            }
        }
    }

    /// Reads the next record in key sequence.
    ///
    /// Must call start() first to position the cursor.
    /// Returns file status 10 (end of file) when no more records.
    pub fn read_next(&mut self) -> KsdsResult<Vec<u8>> {
        if !self.cluster.is_open() {
            self.last_status = FileStatus::FileNotOpen;
            return KsdsResult::empty(FileStatus::FileNotOpen);
        }

        let keys = match &self.sequential_keys {
            Some(keys) => keys,
            None => {
                self.last_status = FileStatus::LogicError;
                return KsdsResult::empty(FileStatus::LogicError);
            }
        };

        if self.sequential_index >= keys.len() {
            self.last_status = FileStatus::EndOfFile;
            return KsdsResult::empty(FileStatus::EndOfFile);
        }

        let key = &keys[self.sequential_index];
        self.sequential_index += 1;

        match self.index.get(key) {
            Some(&offset) => {
                match self.read_record_at(offset) {
                    Ok(record) => {
                        self.current_key = Some(key.clone());
                        self.current_offset = Some(offset);
                        self.last_status = FileStatus::Success;
                        KsdsResult::success(record)
                    }
                    Err(_) => {
                        self.last_status = FileStatus::IoError;
                        KsdsResult::empty(FileStatus::IoError)
                    }
                }
            }
            None => {
                // Record was deleted during iteration
                self.last_status = FileStatus::RecordNotFound;
                KsdsResult::empty(FileStatus::RecordNotFound)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn test_path(name: &str) -> std::path::PathBuf {
        let count = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        std::env::temp_dir().join(format!("ksds_test_{}_{}.vsam", name, count))
    }

    fn cleanup(path: &std::path::Path) {
        let _ = std::fs::remove_file(path);
    }

    #[test]
    fn test_file_status_codes() {
        assert_eq!(FileStatus::Success.code(), "00");
        assert_eq!(FileStatus::RecordNotFound.code(), "23");
        assert_eq!(FileStatus::DuplicateKey.code(), "22");
        assert!(FileStatus::Success.is_success());
        assert!(!FileStatus::RecordNotFound.is_success());
    }

    #[test]
    fn test_create_ksds() {
        let path = test_path("create");

        let params = ClusterParams::ksds("TEST.KSDS", 100, 0, 10).with_path(&path);
        let mut ksds = Ksds::from_params(params).unwrap();
        ksds.create().unwrap();

        assert!(path.exists());
        assert_eq!(ksds.record_count(), 0);

        cleanup(&path);
    }

    #[test]
    fn test_write_and_read_key() {
        let path = test_path("write_read");

        let params = ClusterParams::ksds("TEST.KSDS", 100, 0, 10).with_path(&path);
        let mut ksds = Ksds::from_params(params).unwrap();
        ksds.create().unwrap();

        // Create a record with key "KEY0000001"
        let mut record = vec![0u8; 100];
        record[0..10].copy_from_slice(b"KEY0000001");
        record[10..20].copy_from_slice(b"DATA VALUE");

        // Write the record
        let result = ksds.write(&record);
        assert!(result.status.is_success());
        assert_eq!(ksds.record_count(), 1);

        // Read the record by key
        let result = ksds.read_key(b"KEY0000001");
        assert!(result.status.is_success());
        assert!(result.value.is_some());

        let read_record = result.value.unwrap();
        assert_eq!(&read_record[0..10], b"KEY0000001");
        assert_eq!(&read_record[10..20], b"DATA VALUE");

        cleanup(&path);
    }

    #[test]
    fn test_read_key_not_found() {
        let path = test_path("not_found");

        let params = ClusterParams::ksds("TEST.KSDS", 100, 0, 10).with_path(&path);
        let mut ksds = Ksds::from_params(params).unwrap();
        ksds.create().unwrap();

        // Try to read a non-existent key
        let result = ksds.read_key(b"NONEXISTENT");
        assert_eq!(result.status, FileStatus::RecordNotFound);
        assert!(result.value.is_none());
        assert_eq!(ksds.status().code(), "23");

        cleanup(&path);
    }

    #[test]
    fn test_read_key_generic() {
        let path = test_path("generic");

        let params = ClusterParams::ksds("TEST.KSDS", 100, 0, 10).with_path(&path);
        let mut ksds = Ksds::from_params(params).unwrap();
        ksds.create().unwrap();

        // Write multiple records
        for i in 0..5 {
            let mut record = vec![0u8; 100];
            let key = format!("KEY000000{}", i);
            record[0..10].copy_from_slice(key.as_bytes());
            ksds.write(&record);
        }

        // Generic read with partial key "KEY00"
        let result = ksds.read_key_generic(b"KEY00");
        assert!(result.status.is_success());
        assert!(result.value.is_some());

        // Should return the first matching key
        let record = result.value.unwrap();
        assert!(record[0..5] == *b"KEY00");

        cleanup(&path);
    }

    #[test]
    fn test_duplicate_key() {
        let path = test_path("duplicate");

        let params = ClusterParams::ksds("TEST.KSDS", 100, 0, 10).with_path(&path);
        let mut ksds = Ksds::from_params(params).unwrap();
        ksds.create().unwrap();

        // Write first record
        let mut record = vec![0u8; 100];
        record[0..10].copy_from_slice(b"KEY0000001");

        let result = ksds.write(&record);
        assert!(result.status.is_success());

        // Try to write duplicate
        let result = ksds.write(&record);
        assert_eq!(result.status, FileStatus::DuplicateKey);
        assert_eq!(ksds.status().code(), "22");

        cleanup(&path);
    }

    #[test]
    fn test_open_existing_ksds() {
        let path = test_path("open_existing");

        // Create and populate KSDS
        {
            let params = ClusterParams::ksds("TEST.KSDS", 100, 0, 10).with_path(&path);
            let mut ksds = Ksds::from_params(params).unwrap();
            ksds.create().unwrap();

            for i in 0..3 {
                let mut record = vec![0u8; 100];
                let key = format!("KEY000000{}", i);
                record[0..10].copy_from_slice(key.as_bytes());
                record[10..20].copy_from_slice(format!("VALUE{:05}", i).as_bytes());
                ksds.write(&record);
            }

            ksds.close().unwrap();
        }

        // Reopen and verify
        {
            let mut ksds = Ksds::open(&path).unwrap();

            // Should have 3 records
            assert_eq!(ksds.record_count(), 3);

            // Read each record
            for i in 0..3 {
                let key = format!("KEY000000{}", i);
                let result = ksds.read_key(key.as_bytes());
                assert!(result.status.is_success());
                assert!(result.value.is_some());
            }
        }

        cleanup(&path);
    }

    #[test]
    fn test_record_too_long() {
        let path = test_path("too_long");

        let params = ClusterParams::ksds("TEST.KSDS", 50, 0, 10).with_path(&path);
        let mut ksds = Ksds::from_params(params).unwrap();
        ksds.create().unwrap();

        // Try to write a record that's too long
        let record = vec![0u8; 100]; // 100 bytes > 50 max
        let result = ksds.write(&record);
        assert_eq!(result.status, FileStatus::RecordLengthError);
        assert_eq!(ksds.status().code(), "44");

        cleanup(&path);
    }

    #[test]
    fn test_rewrite() {
        let path = test_path("rewrite");

        let params = ClusterParams::ksds("TEST.KSDS", 100, 0, 10).with_path(&path);
        let mut ksds = Ksds::from_params(params).unwrap();
        ksds.create().unwrap();

        // Write initial record
        let mut record = vec![0u8; 100];
        record[0..10].copy_from_slice(b"KEY0000001");
        record[10..20].copy_from_slice(b"OLD VALUE ");
        ksds.write(&record);

        // Read the record
        let result = ksds.read_key(b"KEY0000001");
        assert!(result.status.is_success());

        // Rewrite with new value (same key)
        let mut new_record = vec![0u8; 100];
        new_record[0..10].copy_from_slice(b"KEY0000001");
        new_record[10..20].copy_from_slice(b"NEW VALUE ");

        let result = ksds.rewrite(&new_record);
        assert!(result.status.is_success());

        // Verify the update
        let result = ksds.read_key(b"KEY0000001");
        assert!(result.status.is_success());
        let record = result.value.unwrap();
        assert_eq!(&record[10..20], b"NEW VALUE ");

        cleanup(&path);
    }

    #[test]
    fn test_rewrite_key_change_error() {
        let path = test_path("rewrite_keychange");

        let params = ClusterParams::ksds("TEST.KSDS", 100, 0, 10).with_path(&path);
        let mut ksds = Ksds::from_params(params).unwrap();
        ksds.create().unwrap();

        // Write initial record
        let mut record = vec![0u8; 100];
        record[0..10].copy_from_slice(b"KEY0000001");
        ksds.write(&record);

        // Read the record
        ksds.read_key(b"KEY0000001");

        // Try to rewrite with different key
        let mut new_record = vec![0u8; 100];
        new_record[0..10].copy_from_slice(b"KEY0000002"); // Different key!

        let result = ksds.rewrite(&new_record);
        assert_eq!(result.status, FileStatus::KeySequenceError);
        assert_eq!(ksds.status().code(), "21");

        cleanup(&path);
    }

    #[test]
    fn test_rewrite_no_current_record() {
        let path = test_path("rewrite_nocurrent");

        let params = ClusterParams::ksds("TEST.KSDS", 100, 0, 10).with_path(&path);
        let mut ksds = Ksds::from_params(params).unwrap();
        ksds.create().unwrap();

        // Try to rewrite without reading first
        let mut record = vec![0u8; 100];
        record[0..10].copy_from_slice(b"KEY0000001");

        let result = ksds.rewrite(&record);
        assert_eq!(result.status, FileStatus::NoCurrentRecord);
        assert_eq!(ksds.status().code(), "43");

        cleanup(&path);
    }

    #[test]
    fn test_delete() {
        let path = test_path("delete");

        let params = ClusterParams::ksds("TEST.KSDS", 100, 0, 10).with_path(&path);
        let mut ksds = Ksds::from_params(params).unwrap();
        ksds.create().unwrap();

        // Write record
        let mut record = vec![0u8; 100];
        record[0..10].copy_from_slice(b"KEY0000001");
        ksds.write(&record);

        // Read the record
        let result = ksds.read_key(b"KEY0000001");
        assert!(result.status.is_success());

        // Delete the record
        let result = ksds.delete();
        assert!(result.status.is_success());

        // Verify it's gone
        let result = ksds.read_key(b"KEY0000001");
        assert_eq!(result.status, FileStatus::RecordNotFound);

        cleanup(&path);
    }

    #[test]
    fn test_delete_no_current_record() {
        let path = test_path("delete_nocurrent");

        let params = ClusterParams::ksds("TEST.KSDS", 100, 0, 10).with_path(&path);
        let mut ksds = Ksds::from_params(params).unwrap();
        ksds.create().unwrap();

        // Try to delete without reading first
        let result = ksds.delete();
        assert_eq!(result.status, FileStatus::NoCurrentRecord);
        assert_eq!(ksds.status().code(), "43");

        cleanup(&path);
    }

    #[test]
    fn test_sequential_read() {
        let path = test_path("sequential");

        let params = ClusterParams::ksds("TEST.KSDS", 100, 0, 10).with_path(&path);
        let mut ksds = Ksds::from_params(params).unwrap();
        ksds.create().unwrap();

        // Write records in non-sequential order
        for i in [3, 1, 4, 1, 5, 9, 2, 6].iter() {
            let mut record = vec![0u8; 100];
            let key = format!("KEY000000{}", i);
            record[0..10].copy_from_slice(key.as_bytes());
            let _ = ksds.write(&record);
        }

        // Start at beginning
        let result = ksds.start(b"KEY0000001");
        assert!(result.status.is_success());

        // Read sequentially and verify order
        let mut keys_read = Vec::new();
        loop {
            let result = ksds.read_next();
            if result.status == FileStatus::EndOfFile {
                break;
            }
            assert!(result.status.is_success());
            let record = result.value.unwrap();
            keys_read.push(record[0..10].to_vec());
        }

        // Should be in sorted order (unique keys only)
        assert!(keys_read.len() >= 2);
        for i in 1..keys_read.len() {
            assert!(keys_read[i] > keys_read[i - 1], "Keys should be in order");
        }

        cleanup(&path);
    }

    #[test]
    fn test_start_key_not_found() {
        let path = test_path("start_notfound");

        let params = ClusterParams::ksds("TEST.KSDS", 100, 0, 10).with_path(&path);
        let mut ksds = Ksds::from_params(params).unwrap();
        ksds.create().unwrap();

        // Write records
        for i in 0..3 {
            let mut record = vec![0u8; 100];
            let key = format!("KEY000000{}", i);
            record[0..10].copy_from_slice(key.as_bytes());
            ksds.write(&record);
        }

        // Start with key that doesn't exist and is > all keys
        let result = ksds.start(b"ZZZZZZZZZZ");
        assert_eq!(result.status, FileStatus::RecordNotFound);
        assert_eq!(ksds.status().code(), "23");

        cleanup(&path);
    }

    #[test]
    fn test_start_middle() {
        let path = test_path("start_middle");

        let params = ClusterParams::ksds("TEST.KSDS", 100, 0, 10).with_path(&path);
        let mut ksds = Ksds::from_params(params).unwrap();
        ksds.create().unwrap();

        // Write records 0-9
        for i in 0..10 {
            let mut record = vec![0u8; 100];
            let key = format!("KEY000000{}", i);
            record[0..10].copy_from_slice(key.as_bytes());
            ksds.write(&record);
        }

        // Start at key 5
        let result = ksds.start(b"KEY0000005");
        assert!(result.status.is_success());

        // Read next should get key 5
        let result = ksds.read_next();
        assert!(result.status.is_success());
        let record = result.value.unwrap();
        assert_eq!(&record[0..10], b"KEY0000005");

        cleanup(&path);
    }
}
