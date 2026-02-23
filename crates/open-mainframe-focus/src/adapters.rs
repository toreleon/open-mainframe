//! FOC-107: Data Adapters (6 stories).
//!
//! `DataAdapter` trait and concrete implementations for FOCUS native files,
//! sequential records, VSAM (simulated), DB2, and IMS/DL/I.

use std::collections::HashMap;
use thiserror::Error;

use crate::table_engine::CellValue;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum AdapterError {
    #[error("adapter not found: {0}")]
    NotFound(String),
    #[error("record not found for key: {0}")]
    RecordNotFound(String),
    #[error("adapter I/O error: {0}")]
    IoError(String),
    #[error("adapter is not open")]
    NotOpen,
    #[error("duplicate key: {0}")]
    DuplicateKey(String),
    #[error("end of file")]
    EndOfFile,
}

/// A data record for adapter I/O.
pub type AdapterRecord = HashMap<String, CellValue>;

// ---------------------------------------------------------------------------
// DataAdapter trait
// ---------------------------------------------------------------------------

/// Common interface for FOCUS data adapters.
pub trait DataAdapter {
    /// Open the adapter for access.
    fn open(&mut self) -> Result<(), AdapterError>;

    /// Read the next sequential record.
    fn read_next(&mut self) -> Result<Option<AdapterRecord>, AdapterError>;

    /// Read a record by key value.
    fn read_by_key(&self, key: &str) -> Result<Option<AdapterRecord>, AdapterError>;

    /// Write (insert) a new record.
    fn write(&mut self, record: AdapterRecord) -> Result<(), AdapterError>;

    /// Update an existing record identified by key.
    fn update(&mut self, key: &str, record: AdapterRecord) -> Result<(), AdapterError>;

    /// Delete a record by key.
    fn delete(&mut self, key: &str) -> Result<(), AdapterError>;

    /// Close the adapter.
    fn close(&mut self) -> Result<(), AdapterError>;

    /// Return the adapter name.
    fn adapter_name(&self) -> &str;

    /// Return total record count.
    fn record_count(&self) -> usize;
}

// ---------------------------------------------------------------------------
// FocusNativeAdapter — in-memory .FOC file simulation
// ---------------------------------------------------------------------------

/// In-memory FOCUS native file adapter (column-oriented storage simulation).
pub struct FocusNativeAdapter {
    name: String,
    key_field: String,
    records: Vec<AdapterRecord>,
    cursor: usize,
    is_open: bool,
}

impl FocusNativeAdapter {
    pub fn new(name: &str, key_field: &str) -> Self {
        Self {
            name: name.to_string(),
            key_field: key_field.to_string(),
            records: Vec::new(),
            cursor: 0,
            is_open: false,
        }
    }

    /// Pre-load records into the adapter.
    pub fn load(&mut self, records: Vec<AdapterRecord>) {
        self.records = records;
    }
}

impl DataAdapter for FocusNativeAdapter {
    fn open(&mut self) -> Result<(), AdapterError> {
        self.is_open = true;
        self.cursor = 0;
        Ok(())
    }

    fn read_next(&mut self) -> Result<Option<AdapterRecord>, AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        if self.cursor >= self.records.len() {
            return Ok(None);
        }
        let rec = self.records[self.cursor].clone();
        self.cursor += 1;
        Ok(Some(rec))
    }

    fn read_by_key(&self, key: &str) -> Result<Option<AdapterRecord>, AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        Ok(self.records.iter().find(|r| {
            r.get(&self.key_field).map_or(false, |v| v.as_str() == key)
        }).cloned())
    }

    fn write(&mut self, record: AdapterRecord) -> Result<(), AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        // Check for duplicate key
        let key = record.get(&self.key_field).map_or(String::new(), |v| v.as_str());
        if self.records.iter().any(|r| {
            r.get(&self.key_field).map_or(false, |v| v.as_str() == key)
        }) {
            return Err(AdapterError::DuplicateKey(key));
        }
        self.records.push(record);
        Ok(())
    }

    fn update(&mut self, key: &str, record: AdapterRecord) -> Result<(), AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        let kf = self.key_field.clone();
        let rec = self.records.iter_mut().find(|r| {
            r.get(&kf).map_or(false, |v| v.as_str() == key)
        });
        match rec {
            Some(r) => {
                for (k, v) in record {
                    r.insert(k, v);
                }
                Ok(())
            }
            None => Err(AdapterError::RecordNotFound(key.to_string())),
        }
    }

    fn delete(&mut self, key: &str) -> Result<(), AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        let kf = self.key_field.clone();
        let before = self.records.len();
        self.records.retain(|r| {
            r.get(&kf).map_or(true, |v| v.as_str() != key)
        });
        if self.records.len() == before {
            Err(AdapterError::RecordNotFound(key.to_string()))
        } else {
            Ok(())
        }
    }

    fn close(&mut self) -> Result<(), AdapterError> {
        self.is_open = false;
        Ok(())
    }

    fn adapter_name(&self) -> &str {
        &self.name
    }

    fn record_count(&self) -> usize {
        self.records.len()
    }
}

// ---------------------------------------------------------------------------
// SequentialAdapter — fixed-format sequential records
// ---------------------------------------------------------------------------

/// Sequential file adapter for fixed-format records.
pub struct SequentialAdapter {
    name: String,
    records: Vec<String>,
    cursor: usize,
    is_open: bool,
    record_length: usize,
}

impl SequentialAdapter {
    pub fn new(name: &str, record_length: usize) -> Self {
        Self {
            name: name.to_string(),
            records: Vec::new(),
            cursor: 0,
            is_open: false,
            record_length,
        }
    }

    pub fn load_lines(&mut self, lines: Vec<String>) {
        self.records = lines;
    }

    /// Get the record length.
    pub fn record_length(&self) -> usize {
        self.record_length
    }
}

impl DataAdapter for SequentialAdapter {
    fn open(&mut self) -> Result<(), AdapterError> {
        self.is_open = true;
        self.cursor = 0;
        Ok(())
    }

    fn read_next(&mut self) -> Result<Option<AdapterRecord>, AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        if self.cursor >= self.records.len() {
            return Ok(None);
        }
        let line = &self.records[self.cursor];
        self.cursor += 1;
        let mut rec = AdapterRecord::new();
        rec.insert("_LINE".to_string(), CellValue::Str(line.clone()));
        rec.insert("_RECNO".to_string(), CellValue::Num(self.cursor as f64));
        Ok(Some(rec))
    }

    fn read_by_key(&self, key: &str) -> Result<Option<AdapterRecord>, AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        for (i, line) in self.records.iter().enumerate() {
            if line.contains(key) {
                let mut rec = AdapterRecord::new();
                rec.insert("_LINE".to_string(), CellValue::Str(line.clone()));
                rec.insert("_RECNO".to_string(), CellValue::Num((i + 1) as f64));
                return Ok(Some(rec));
            }
        }
        Ok(None)
    }

    fn write(&mut self, record: AdapterRecord) -> Result<(), AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        let line = record
            .get("_LINE")
            .map_or(String::new(), |v| v.as_str());
        self.records.push(line);
        Ok(())
    }

    fn update(&mut self, key: &str, record: AdapterRecord) -> Result<(), AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        let line = record.get("_LINE").map_or(String::new(), |v| v.as_str());
        for rec_line in &mut self.records {
            if rec_line.contains(key) {
                *rec_line = line;
                return Ok(());
            }
        }
        Err(AdapterError::RecordNotFound(key.to_string()))
    }

    fn delete(&mut self, key: &str) -> Result<(), AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        let before = self.records.len();
        self.records.retain(|l| !l.contains(key));
        if self.records.len() == before {
            Err(AdapterError::RecordNotFound(key.to_string()))
        } else {
            Ok(())
        }
    }

    fn close(&mut self) -> Result<(), AdapterError> {
        self.is_open = false;
        Ok(())
    }

    fn adapter_name(&self) -> &str {
        &self.name
    }

    fn record_count(&self) -> usize {
        self.records.len()
    }
}

// ---------------------------------------------------------------------------
// VsamAdapter — simulated KSDS
// ---------------------------------------------------------------------------

/// Simulated VSAM KSDS adapter with key-sequenced access.
pub struct VsamAdapter {
    name: String,
    key_field: String,
    records: Vec<AdapterRecord>,
    cursor: usize,
    is_open: bool,
}

impl VsamAdapter {
    pub fn new(name: &str, key_field: &str) -> Self {
        Self {
            name: name.to_string(),
            key_field: key_field.to_string(),
            records: Vec::new(),
            cursor: 0,
            is_open: false,
        }
    }

    pub fn load(&mut self, records: Vec<AdapterRecord>) {
        self.records = records;
        // Sort by key for KSDS simulation
        let kf = self.key_field.clone();
        self.records.sort_by(|a, b| {
            let ak = a.get(&kf).map_or(String::new(), |v| v.as_str());
            let bk = b.get(&kf).map_or(String::new(), |v| v.as_str());
            ak.cmp(&bk)
        });
    }
}

impl DataAdapter for VsamAdapter {
    fn open(&mut self) -> Result<(), AdapterError> {
        self.is_open = true;
        self.cursor = 0;
        Ok(())
    }

    fn read_next(&mut self) -> Result<Option<AdapterRecord>, AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        if self.cursor >= self.records.len() {
            return Ok(None);
        }
        let rec = self.records[self.cursor].clone();
        self.cursor += 1;
        Ok(Some(rec))
    }

    fn read_by_key(&self, key: &str) -> Result<Option<AdapterRecord>, AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        Ok(self.records.iter().find(|r| {
            r.get(&self.key_field).map_or(false, |v| v.as_str() == key)
        }).cloned())
    }

    fn write(&mut self, record: AdapterRecord) -> Result<(), AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        let key = record.get(&self.key_field).map_or(String::new(), |v| v.as_str());
        if self.records.iter().any(|r| {
            r.get(&self.key_field).map_or(false, |v| v.as_str() == key)
        }) {
            return Err(AdapterError::DuplicateKey(key));
        }
        self.records.push(record);
        // Maintain sort order
        let kf = self.key_field.clone();
        self.records.sort_by(|a, b| {
            let ak = a.get(&kf).map_or(String::new(), |v| v.as_str());
            let bk = b.get(&kf).map_or(String::new(), |v| v.as_str());
            ak.cmp(&bk)
        });
        Ok(())
    }

    fn update(&mut self, key: &str, record: AdapterRecord) -> Result<(), AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        let kf = self.key_field.clone();
        let rec = self.records.iter_mut().find(|r| {
            r.get(&kf).map_or(false, |v| v.as_str() == key)
        });
        match rec {
            Some(r) => {
                for (k, v) in record {
                    r.insert(k, v);
                }
                Ok(())
            }
            None => Err(AdapterError::RecordNotFound(key.to_string())),
        }
    }

    fn delete(&mut self, key: &str) -> Result<(), AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        let kf = self.key_field.clone();
        let before = self.records.len();
        self.records.retain(|r| {
            r.get(&kf).map_or(true, |v| v.as_str() != key)
        });
        if self.records.len() == before {
            Err(AdapterError::RecordNotFound(key.to_string()))
        } else {
            Ok(())
        }
    }

    fn close(&mut self) -> Result<(), AdapterError> {
        self.is_open = false;
        Ok(())
    }

    fn adapter_name(&self) -> &str {
        &self.name
    }

    fn record_count(&self) -> usize {
        self.records.len()
    }
}

// ---------------------------------------------------------------------------
// Db2Adapter — SQL-based (simulated)
// ---------------------------------------------------------------------------

/// Simulated DB2 SQL adapter.
pub struct Db2Adapter {
    name: String,
    table_name: String,
    key_field: String,
    records: Vec<AdapterRecord>,
    cursor: usize,
    is_open: bool,
}

impl Db2Adapter {
    pub fn new(name: &str, table_name: &str, key_field: &str) -> Self {
        Self {
            name: name.to_string(),
            table_name: table_name.to_string(),
            key_field: key_field.to_string(),
            records: Vec::new(),
            cursor: 0,
            is_open: false,
        }
    }

    pub fn load(&mut self, records: Vec<AdapterRecord>) {
        self.records = records;
    }

    pub fn table_name(&self) -> &str {
        &self.table_name
    }
}

impl DataAdapter for Db2Adapter {
    fn open(&mut self) -> Result<(), AdapterError> {
        self.is_open = true;
        self.cursor = 0;
        Ok(())
    }

    fn read_next(&mut self) -> Result<Option<AdapterRecord>, AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        if self.cursor >= self.records.len() {
            return Ok(None);
        }
        let rec = self.records[self.cursor].clone();
        self.cursor += 1;
        Ok(Some(rec))
    }

    fn read_by_key(&self, key: &str) -> Result<Option<AdapterRecord>, AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        Ok(self.records.iter().find(|r| {
            r.get(&self.key_field).map_or(false, |v| v.as_str() == key)
        }).cloned())
    }

    fn write(&mut self, record: AdapterRecord) -> Result<(), AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        self.records.push(record);
        Ok(())
    }

    fn update(&mut self, key: &str, record: AdapterRecord) -> Result<(), AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        let kf = self.key_field.clone();
        let rec = self.records.iter_mut().find(|r| {
            r.get(&kf).map_or(false, |v| v.as_str() == key)
        });
        match rec {
            Some(r) => {
                for (k, v) in record {
                    r.insert(k, v);
                }
                Ok(())
            }
            None => Err(AdapterError::RecordNotFound(key.to_string())),
        }
    }

    fn delete(&mut self, key: &str) -> Result<(), AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        let kf = self.key_field.clone();
        let before = self.records.len();
        self.records.retain(|r| {
            r.get(&kf).map_or(true, |v| v.as_str() != key)
        });
        if self.records.len() == before {
            Err(AdapterError::RecordNotFound(key.to_string()))
        } else {
            Ok(())
        }
    }

    fn close(&mut self) -> Result<(), AdapterError> {
        self.is_open = false;
        Ok(())
    }

    fn adapter_name(&self) -> &str {
        &self.name
    }

    fn record_count(&self) -> usize {
        self.records.len()
    }
}

// ---------------------------------------------------------------------------
// ImsAdapter — DL/I-based (simulated)
// ---------------------------------------------------------------------------

/// Simulated IMS DL/I adapter.
pub struct ImsAdapter {
    name: String,
    pcb_name: String,
    key_field: String,
    records: Vec<AdapterRecord>,
    cursor: usize,
    is_open: bool,
}

impl ImsAdapter {
    pub fn new(name: &str, pcb_name: &str, key_field: &str) -> Self {
        Self {
            name: name.to_string(),
            pcb_name: pcb_name.to_string(),
            key_field: key_field.to_string(),
            records: Vec::new(),
            cursor: 0,
            is_open: false,
        }
    }

    pub fn load(&mut self, records: Vec<AdapterRecord>) {
        self.records = records;
    }

    pub fn pcb_name(&self) -> &str {
        &self.pcb_name
    }
}

impl DataAdapter for ImsAdapter {
    fn open(&mut self) -> Result<(), AdapterError> {
        self.is_open = true;
        self.cursor = 0;
        Ok(())
    }

    fn read_next(&mut self) -> Result<Option<AdapterRecord>, AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        if self.cursor >= self.records.len() {
            return Ok(None);
        }
        let rec = self.records[self.cursor].clone();
        self.cursor += 1;
        Ok(Some(rec))
    }

    fn read_by_key(&self, key: &str) -> Result<Option<AdapterRecord>, AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        Ok(self.records.iter().find(|r| {
            r.get(&self.key_field).map_or(false, |v| v.as_str() == key)
        }).cloned())
    }

    fn write(&mut self, record: AdapterRecord) -> Result<(), AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        self.records.push(record);
        Ok(())
    }

    fn update(&mut self, key: &str, record: AdapterRecord) -> Result<(), AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        let kf = self.key_field.clone();
        let rec = self.records.iter_mut().find(|r| {
            r.get(&kf).map_or(false, |v| v.as_str() == key)
        });
        match rec {
            Some(r) => {
                for (k, v) in record {
                    r.insert(k, v);
                }
                Ok(())
            }
            None => Err(AdapterError::RecordNotFound(key.to_string())),
        }
    }

    fn delete(&mut self, key: &str) -> Result<(), AdapterError> {
        if !self.is_open {
            return Err(AdapterError::NotOpen);
        }
        let kf = self.key_field.clone();
        let before = self.records.len();
        self.records.retain(|r| {
            r.get(&kf).map_or(true, |v| v.as_str() != key)
        });
        if self.records.len() == before {
            Err(AdapterError::RecordNotFound(key.to_string()))
        } else {
            Ok(())
        }
    }

    fn close(&mut self) -> Result<(), AdapterError> {
        self.is_open = false;
        Ok(())
    }

    fn adapter_name(&self) -> &str {
        &self.name
    }

    fn record_count(&self) -> usize {
        self.records.len()
    }
}

// ---------------------------------------------------------------------------
// AdapterRegistry
// ---------------------------------------------------------------------------

/// Maps MFD suffix/access file to appropriate adapter constructor names.
pub struct AdapterRegistry {
    mappings: HashMap<String, String>,
}

impl AdapterRegistry {
    pub fn new() -> Self {
        let mut mappings = HashMap::new();
        mappings.insert("FOC".to_string(), "FOCUS_NATIVE".to_string());
        mappings.insert("SEQ".to_string(), "SEQUENTIAL".to_string());
        mappings.insert("VSAM".to_string(), "VSAM".to_string());
        mappings.insert("DB2".to_string(), "DB2".to_string());
        mappings.insert("IMS".to_string(), "IMS".to_string());
        Self { mappings }
    }

    /// Look up adapter type for a given suffix.
    pub fn lookup(&self, suffix: &str) -> Option<&str> {
        self.mappings.get(&suffix.to_uppercase()).map(|s| s.as_str())
    }

    /// Register a new suffix-to-adapter mapping.
    pub fn register(&mut self, suffix: &str, adapter_type: &str) {
        self.mappings
            .insert(suffix.to_uppercase(), adapter_type.to_string());
    }

    /// List all registered suffix mappings.
    pub fn list_mappings(&self) -> Vec<(&str, &str)> {
        self.mappings.iter().map(|(k, v)| (k.as_str(), v.as_str())).collect()
    }
}

impl Default for AdapterRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn make_record(key: &str, name: &str) -> AdapterRecord {
        HashMap::from([
            ("ID".to_string(), CellValue::Str(key.to_string())),
            ("NAME".to_string(), CellValue::Str(name.to_string())),
        ])
    }

    // --- FocusNativeAdapter tests ---

    #[test]
    fn test_focus_native_open_close() {
        let mut adapter = FocusNativeAdapter::new("TEST", "ID");
        adapter.open().unwrap();
        assert_eq!(adapter.adapter_name(), "TEST");
        adapter.close().unwrap();
    }

    #[test]
    fn test_focus_native_write_read() {
        let mut adapter = FocusNativeAdapter::new("TEST", "ID");
        adapter.open().unwrap();
        adapter.write(make_record("1", "Alice")).unwrap();
        adapter.write(make_record("2", "Bob")).unwrap();
        assert_eq!(adapter.record_count(), 2);

        // Re-open to reset cursor
        adapter.open().unwrap();
        let r1 = adapter.read_next().unwrap().unwrap();
        assert_eq!(r1.get("NAME").unwrap().as_str(), "Alice");
        let r2 = adapter.read_next().unwrap().unwrap();
        assert_eq!(r2.get("NAME").unwrap().as_str(), "Bob");
        assert!(adapter.read_next().unwrap().is_none());
    }

    #[test]
    fn test_focus_native_read_by_key() {
        let mut adapter = FocusNativeAdapter::new("TEST", "ID");
        adapter.open().unwrap();
        adapter.write(make_record("1", "Alice")).unwrap();
        adapter.write(make_record("2", "Bob")).unwrap();

        let found = adapter.read_by_key("2").unwrap().unwrap();
        assert_eq!(found.get("NAME").unwrap().as_str(), "Bob");
        assert!(adapter.read_by_key("99").unwrap().is_none());
    }

    #[test]
    fn test_focus_native_update() {
        let mut adapter = FocusNativeAdapter::new("TEST", "ID");
        adapter.open().unwrap();
        adapter.write(make_record("1", "Alice")).unwrap();
        adapter
            .update(
                "1",
                HashMap::from([("NAME".to_string(), CellValue::Str("Alicia".to_string()))]),
            )
            .unwrap();
        let rec = adapter.read_by_key("1").unwrap().unwrap();
        assert_eq!(rec.get("NAME").unwrap().as_str(), "Alicia");
    }

    #[test]
    fn test_focus_native_delete() {
        let mut adapter = FocusNativeAdapter::new("TEST", "ID");
        adapter.open().unwrap();
        adapter.write(make_record("1", "Alice")).unwrap();
        adapter.write(make_record("2", "Bob")).unwrap();
        adapter.delete("1").unwrap();
        assert_eq!(adapter.record_count(), 1);
        assert!(adapter.read_by_key("1").unwrap().is_none());
    }

    #[test]
    fn test_focus_native_duplicate_key() {
        let mut adapter = FocusNativeAdapter::new("TEST", "ID");
        adapter.open().unwrap();
        adapter.write(make_record("1", "Alice")).unwrap();
        assert!(adapter.write(make_record("1", "Dup")).is_err());
    }

    #[test]
    fn test_focus_native_not_open() {
        let mut adapter = FocusNativeAdapter::new("TEST", "ID");
        assert!(adapter.read_next().is_err());
    }

    // --- SequentialAdapter tests ---

    #[test]
    fn test_sequential_read() {
        let mut adapter = SequentialAdapter::new("SEQ", 80);
        adapter.load_lines(vec!["LINE 1".into(), "LINE 2".into(), "LINE 3".into()]);
        adapter.open().unwrap();
        assert_eq!(adapter.record_length(), 80);

        let r1 = adapter.read_next().unwrap().unwrap();
        assert_eq!(r1.get("_LINE").unwrap().as_str(), "LINE 1");
        let r2 = adapter.read_next().unwrap().unwrap();
        assert_eq!(r2.get("_LINE").unwrap().as_str(), "LINE 2");
    }

    #[test]
    fn test_sequential_write() {
        let mut adapter = SequentialAdapter::new("SEQ", 80);
        adapter.open().unwrap();
        let mut rec = AdapterRecord::new();
        rec.insert("_LINE".to_string(), CellValue::Str("NEW LINE".into()));
        adapter.write(rec).unwrap();
        assert_eq!(adapter.record_count(), 1);
    }

    #[test]
    fn test_sequential_read_by_key() {
        let mut adapter = SequentialAdapter::new("SEQ", 80);
        adapter.load_lines(vec!["ALICE DATA".into(), "BOB DATA".into()]);
        adapter.open().unwrap();
        let found = adapter.read_by_key("BOB").unwrap().unwrap();
        assert!(found.get("_LINE").unwrap().as_str().contains("BOB"));
    }

    // --- VsamAdapter tests ---

    #[test]
    fn test_vsam_sorted_access() {
        let mut adapter = VsamAdapter::new("VSAM", "ID");
        adapter.load(vec![
            make_record("C", "Carol"),
            make_record("A", "Alice"),
            make_record("B", "Bob"),
        ]);
        adapter.open().unwrap();
        // Should be sorted by key
        let r1 = adapter.read_next().unwrap().unwrap();
        assert_eq!(r1.get("ID").unwrap().as_str(), "A");
        let r2 = adapter.read_next().unwrap().unwrap();
        assert_eq!(r2.get("ID").unwrap().as_str(), "B");
    }

    #[test]
    fn test_vsam_key_access() {
        let mut adapter = VsamAdapter::new("VSAM", "ID");
        adapter.load(vec![make_record("1", "Alice"), make_record("2", "Bob")]);
        adapter.open().unwrap();
        let found = adapter.read_by_key("2").unwrap().unwrap();
        assert_eq!(found.get("NAME").unwrap().as_str(), "Bob");
    }

    #[test]
    fn test_vsam_write_maintains_order() {
        let mut adapter = VsamAdapter::new("VSAM", "ID");
        adapter.open().unwrap();
        adapter.write(make_record("C", "Carol")).unwrap();
        adapter.write(make_record("A", "Alice")).unwrap();
        adapter.open().unwrap();
        let first = adapter.read_next().unwrap().unwrap();
        assert_eq!(first.get("ID").unwrap().as_str(), "A");
    }

    #[test]
    fn test_vsam_duplicate_key() {
        let mut adapter = VsamAdapter::new("VSAM", "ID");
        adapter.open().unwrap();
        adapter.write(make_record("1", "Alice")).unwrap();
        assert!(adapter.write(make_record("1", "Dup")).is_err());
    }

    // --- Db2Adapter tests ---

    #[test]
    fn test_db2_basic_operations() {
        let mut adapter = Db2Adapter::new("DB2", "SCHEMA.EMP", "ID");
        assert_eq!(adapter.table_name(), "SCHEMA.EMP");
        adapter.open().unwrap();
        adapter.write(make_record("1", "Alice")).unwrap();
        adapter.write(make_record("2", "Bob")).unwrap();
        assert_eq!(adapter.record_count(), 2);
        let found = adapter.read_by_key("1").unwrap().unwrap();
        assert_eq!(found.get("NAME").unwrap().as_str(), "Alice");
    }

    #[test]
    fn test_db2_update() {
        let mut adapter = Db2Adapter::new("DB2", "EMP", "ID");
        adapter.open().unwrap();
        adapter.write(make_record("1", "Alice")).unwrap();
        adapter
            .update(
                "1",
                HashMap::from([("NAME".to_string(), CellValue::Str("Alicia".to_string()))]),
            )
            .unwrap();
        let rec = adapter.read_by_key("1").unwrap().unwrap();
        assert_eq!(rec.get("NAME").unwrap().as_str(), "Alicia");
    }

    #[test]
    fn test_db2_delete() {
        let mut adapter = Db2Adapter::new("DB2", "EMP", "ID");
        adapter.open().unwrap();
        adapter.write(make_record("1", "Alice")).unwrap();
        adapter.delete("1").unwrap();
        assert_eq!(adapter.record_count(), 0);
    }

    // --- ImsAdapter tests ---

    #[test]
    fn test_ims_basic_operations() {
        let mut adapter = ImsAdapter::new("IMS", "EMPPCB", "ID");
        assert_eq!(adapter.pcb_name(), "EMPPCB");
        adapter.open().unwrap();
        adapter.write(make_record("1", "Alice")).unwrap();
        assert_eq!(adapter.record_count(), 1);
        let found = adapter.read_by_key("1").unwrap().unwrap();
        assert_eq!(found.get("NAME").unwrap().as_str(), "Alice");
    }

    #[test]
    fn test_ims_sequential_read() {
        let mut adapter = ImsAdapter::new("IMS", "PCB1", "ID");
        adapter.load(vec![make_record("1", "Alice"), make_record("2", "Bob")]);
        adapter.open().unwrap();
        let r1 = adapter.read_next().unwrap().unwrap();
        assert_eq!(r1.get("ID").unwrap().as_str(), "1");
    }

    #[test]
    fn test_ims_delete() {
        let mut adapter = ImsAdapter::new("IMS", "PCB1", "ID");
        adapter.open().unwrap();
        adapter.write(make_record("1", "Alice")).unwrap();
        adapter.delete("1").unwrap();
        assert_eq!(adapter.record_count(), 0);
    }

    // --- AdapterRegistry tests ---

    #[test]
    fn test_registry_default_mappings() {
        let reg = AdapterRegistry::new();
        assert_eq!(reg.lookup("FOC"), Some("FOCUS_NATIVE"));
        assert_eq!(reg.lookup("SEQ"), Some("SEQUENTIAL"));
        assert_eq!(reg.lookup("VSAM"), Some("VSAM"));
        assert_eq!(reg.lookup("DB2"), Some("DB2"));
        assert_eq!(reg.lookup("IMS"), Some("IMS"));
    }

    #[test]
    fn test_registry_custom_mapping() {
        let mut reg = AdapterRegistry::new();
        reg.register("ORACLE", "ORACLE_ADAPTER");
        assert_eq!(reg.lookup("ORACLE"), Some("ORACLE_ADAPTER"));
    }

    #[test]
    fn test_registry_case_insensitive() {
        let reg = AdapterRegistry::new();
        assert_eq!(reg.lookup("foc"), Some("FOCUS_NATIVE"));
        assert_eq!(reg.lookup("Db2"), Some("DB2"));
    }

    #[test]
    fn test_registry_unknown_suffix() {
        let reg = AdapterRegistry::new();
        assert!(reg.lookup("UNKNOWN").is_none());
    }

    #[test]
    fn test_registry_list_mappings() {
        let reg = AdapterRegistry::new();
        let mappings = reg.list_mappings();
        assert!(mappings.len() >= 5);
    }
}
