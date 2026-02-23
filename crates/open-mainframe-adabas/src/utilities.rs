//! ADA-109: Utilities & DDM (5 stories).
//!
//! Provides ADABAS utility programs: ADALOD (load), ADAUNI (unload),
//! ADASAV (save/restore), and the Data Definition Module (DDM) for
//! Natural interface support.

use std::collections::HashMap;

use crate::fdt::{Fdt, FieldType};
use crate::storage::{AdabasFile, Isn};
use crate::AdabasError;

// ── AdalodUtility ──────────────────────────────────────────────────

/// ADALOD utility: load data into an ADABAS file.
#[derive(Debug)]
pub struct AdalodUtility {
    /// Target file number.
    pub file_number: u16,
    /// Records to load (raw byte records).
    records: Vec<Vec<u8>>,
    /// Load mode.
    pub mode: LoadMode,
}

/// Load mode for ADALOD.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LoadMode {
    /// Initial load (file must be empty).
    Initial,
    /// Mass update (add to existing data).
    MassUpdate,
}

impl AdalodUtility {
    /// Create a new ADALOD utility for the given file.
    pub fn new(file_number: u16, mode: LoadMode) -> Self {
        Self {
            file_number,
            records: Vec::new(),
            mode,
        }
    }

    /// Add a record to be loaded.
    pub fn add_record(&mut self, data: Vec<u8>) {
        self.records.push(data);
    }

    /// Execute the load, returning the number of records loaded and
    /// the assigned ISNs.
    pub fn execute(&self, file: &mut AdabasFile) -> Result<Vec<Isn>, AdabasError> {
        if self.mode == LoadMode::Initial && file.data_storage.block_count() > 0 {
            return Err(AdabasError::FileNotEmpty {
                file_number: self.file_number,
            });
        }

        let mut isns = Vec::with_capacity(self.records.len());
        for record in &self.records {
            let isn = file.allocate_isn();
            file.store_record(isn, record.clone())?;
            isns.push(isn);
        }
        Ok(isns)
    }

    /// Return the number of records queued for load.
    pub fn record_count(&self) -> usize {
        self.records.len()
    }
}

// ── AdauniUtility ──────────────────────────────────────────────────

/// ADAUNI utility: unload data from an ADABAS file.
#[derive(Debug)]
pub struct AdauniUtility {
    /// Source file number.
    pub file_number: u16,
    /// ISNs to unload (empty = unload all).
    isn_list: Vec<Isn>,
}

impl AdauniUtility {
    /// Create a new ADAUNI utility for the given file.
    pub fn new(file_number: u16) -> Self {
        Self {
            file_number,
            isn_list: Vec::new(),
        }
    }

    /// Restrict unload to specific ISNs.
    pub fn with_isn_list(mut self, isns: Vec<Isn>) -> Self {
        self.isn_list = isns;
        self
    }

    /// Execute the unload, returning (ISN, data) pairs.
    pub fn execute(&self, file: &AdabasFile) -> Result<Vec<(Isn, Vec<u8>)>, AdabasError> {
        let mut result = Vec::new();

        if self.isn_list.is_empty() {
            // Unload all records sequentially by ISN.
            for isn in 1..=file.top_isn() {
                if let Ok(data) = file.read_record(isn) {
                    result.push((isn, data.to_vec()));
                }
            }
        } else {
            for &isn in &self.isn_list {
                let data = file.read_record(isn)?;
                result.push((isn, data.to_vec()));
            }
        }

        Ok(result)
    }
}

// ── AdasavUtility ──────────────────────────────────────────────────

/// ADASAV utility: save/restore the database.
#[derive(Debug, Clone)]
pub struct AdasavUtility {
    /// The operation to perform.
    pub operation: SaveOperation,
}

/// Save/restore operation type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SaveOperation {
    /// Save the database to a backup image.
    Save,
    /// Restore the database from a backup image.
    Restore,
}

/// A serialized backup image of a file.
#[derive(Debug, Clone)]
pub struct BackupImage {
    /// File number.
    pub file_number: u16,
    /// File name.
    pub file_name: String,
    /// Records: (ISN, data).
    pub records: Vec<(Isn, Vec<u8>)>,
}

impl AdasavUtility {
    /// Create a new ADASAV utility.
    pub fn new(operation: SaveOperation) -> Self {
        Self { operation }
    }

    /// Save a file to a backup image.
    pub fn save_file(&self, file: &AdabasFile) -> BackupImage {
        let mut records = Vec::new();
        for isn in 1..=file.top_isn() {
            if let Ok(data) = file.read_record(isn) {
                records.push((isn, data.to_vec()));
            }
        }
        BackupImage {
            file_number: file.file_number,
            file_name: file.name.clone(),
            records,
        }
    }

    /// Restore a file from a backup image.
    pub fn restore_file(&self, image: &BackupImage) -> Result<AdabasFile, AdabasError> {
        let mut file = AdabasFile::new(image.file_number, &image.file_name);
        for (isn, data) in &image.records {
            // Ensure the file's ISN counter is up to date.
            while file.top_isn() < *isn {
                file.allocate_isn();
            }
            file.store_record(*isn, data.clone())?;
        }
        Ok(file)
    }
}

// ── Ddm ────────────────────────────────────────────────────────────

/// Data Definition Module: Natural interface descriptor for an ADABAS file.
///
/// A DDM maps long field names to ADABAS short names and provides
/// the interface used by Natural (4GL) to access ADABAS files.
#[derive(Debug, Clone)]
pub struct Ddm {
    /// DDM name (up to 32 characters).
    pub name: String,
    /// File number this DDM refers to.
    pub file_number: u16,
    /// Field mappings: long name -> short name (2 chars).
    field_map: HashMap<String, String>,
    /// Ordered field entries for display.
    entries: Vec<DdmEntry>,
}

/// A single entry in a DDM.
#[derive(Debug, Clone)]
pub struct DdmEntry {
    /// The long (Natural) field name.
    pub long_name: String,
    /// The short (ADABAS) field name.
    pub short_name: String,
    /// Field format character.
    pub format: char,
    /// Field length.
    pub length: u16,
    /// Level number.
    pub level: u8,
}

impl Ddm {
    /// Create a new DDM.
    pub fn new(name: impl Into<String>, file_number: u16) -> Self {
        Self {
            name: name.into(),
            file_number,
            field_map: HashMap::new(),
            entries: Vec::new(),
        }
    }

    /// Add a field mapping.
    pub fn add_field(
        &mut self,
        long_name: impl Into<String>,
        short_name: impl Into<String>,
        format: char,
        length: u16,
        level: u8,
    ) {
        let long = long_name.into();
        let short = short_name.into();
        self.field_map.insert(long.clone(), short.clone());
        self.entries.push(DdmEntry {
            long_name: long,
            short_name: short,
            format,
            length,
            level,
        });
    }

    /// Resolve a long name to a short name.
    pub fn resolve(&self, long_name: &str) -> Option<&str> {
        self.field_map.get(long_name).map(|s| s.as_str())
    }

    /// Return all DDM entries.
    pub fn entries(&self) -> &[DdmEntry] {
        &self.entries
    }

    /// Return the field count.
    pub fn field_count(&self) -> usize {
        self.entries.len()
    }

    /// Generate a DDM from an FDT (using short names as both long and short).
    pub fn from_fdt(name: &str, file_number: u16, fdt: &Fdt) -> Self {
        let mut ddm = Self::new(name, file_number);
        for field in fdt.fields() {
            let fmt = match field.field_type {
                FieldType::Alpha | FieldType::Wide => 'A',
                FieldType::Numeric | FieldType::Unpacked => 'N',
                FieldType::Packed => 'P',
                FieldType::Binary => 'B',
            };
            ddm.add_field(
                &field.name,
                &field.name,
                fmt,
                field.length,
                field.level,
            );
        }
        ddm
    }
}

// ── Tests ──────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fdt::FieldDef;

    #[test]
    fn adalod_initial_load() {
        let mut file = AdabasFile::new(1, "TEST");
        let mut loader = AdalodUtility::new(1, LoadMode::Initial);
        loader.add_record(b"rec1".to_vec());
        loader.add_record(b"rec2".to_vec());
        loader.add_record(b"rec3".to_vec());
        assert_eq!(loader.record_count(), 3);

        let isns = loader.execute(&mut file).unwrap();
        assert_eq!(isns.len(), 3);
        assert_eq!(file.read_record(isns[0]).unwrap(), b"rec1");
        assert_eq!(file.read_record(isns[2]).unwrap(), b"rec3");
    }

    #[test]
    fn adalod_initial_on_nonempty_file() {
        let mut file = AdabasFile::new(1, "TEST");
        let isn = file.allocate_isn();
        file.store_record(isn, b"existing".to_vec()).unwrap();

        let loader = AdalodUtility::new(1, LoadMode::Initial);
        assert!(loader.execute(&mut file).is_err());
    }

    #[test]
    fn adalod_mass_update() {
        let mut file = AdabasFile::new(1, "TEST");
        let isn = file.allocate_isn();
        file.store_record(isn, b"existing".to_vec()).unwrap();

        let mut loader = AdalodUtility::new(1, LoadMode::MassUpdate);
        loader.add_record(b"new1".to_vec());
        let isns = loader.execute(&mut file).unwrap();
        assert_eq!(isns.len(), 1);
    }

    #[test]
    fn adauni_unload_all() {
        let mut file = AdabasFile::new(1, "TEST");
        for i in 0..3 {
            let isn = file.allocate_isn();
            file.store_record(isn, format!("rec{i}").into_bytes())
                .unwrap();
        }

        let unloader = AdauniUtility::new(1);
        let result = unloader.execute(&file).unwrap();
        assert_eq!(result.len(), 3);
        assert_eq!(result[0].1, b"rec0");
    }

    #[test]
    fn adauni_unload_specific() {
        let mut file = AdabasFile::new(1, "TEST");
        for _ in 0..5 {
            let isn = file.allocate_isn();
            file.store_record(isn, b"data".to_vec()).unwrap();
        }

        let unloader = AdauniUtility::new(1).with_isn_list(vec![2, 4]);
        let result = unloader.execute(&file).unwrap();
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].0, 2);
        assert_eq!(result[1].0, 4);
    }

    #[test]
    fn adasav_save_and_restore() {
        let mut file = AdabasFile::new(1, "EMPLOYEES");
        for i in 0..3 {
            let isn = file.allocate_isn();
            file.store_record(isn, format!("emp{i}").into_bytes())
                .unwrap();
        }

        let saver = AdasavUtility::new(SaveOperation::Save);
        let image = saver.save_file(&file);
        assert_eq!(image.records.len(), 3);

        let restorer = AdasavUtility::new(SaveOperation::Restore);
        let restored = restorer.restore_file(&image).unwrap();
        assert_eq!(restored.read_record(1).unwrap(), b"emp0");
        assert_eq!(restored.read_record(3).unwrap(), b"emp2");
    }

    #[test]
    fn ddm_basic() {
        let mut ddm = Ddm::new("EMPLOYEES", 1);
        ddm.add_field("LAST-NAME", "AA", 'A', 20, 1);
        ddm.add_field("FIRST-NAME", "AB", 'A', 20, 1);
        ddm.add_field("SALARY", "AC", 'N', 8, 1);

        assert_eq!(ddm.resolve("LAST-NAME"), Some("AA"));
        assert_eq!(ddm.resolve("SALARY"), Some("AC"));
        assert!(ddm.resolve("UNKNOWN").is_none());
        assert_eq!(ddm.field_count(), 3);
    }

    #[test]
    fn ddm_from_fdt() {
        let mut fdt = Fdt::new();
        fdt.add_field(FieldDef::new("AA", 1, FieldType::Alpha, 20))
            .unwrap();
        fdt.add_field(FieldDef::new("AB", 1, FieldType::Numeric, 8))
            .unwrap();

        let ddm = Ddm::from_fdt("TEST-DDM", 1, &fdt);
        assert_eq!(ddm.field_count(), 2);
        assert_eq!(ddm.resolve("AA"), Some("AA"));
    }

    #[test]
    fn ddm_entries() {
        let mut ddm = Ddm::new("TEST", 1);
        ddm.add_field("FIELD1", "AA", 'A', 10, 1);
        let entries = ddm.entries();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].long_name, "FIELD1");
        assert_eq!(entries[0].short_name, "AA");
        assert_eq!(entries[0].format, 'A');
    }

    #[test]
    fn backup_image_contents() {
        let mut file = AdabasFile::new(5, "BACKUP-TEST");
        let isn = file.allocate_isn();
        file.store_record(isn, b"test".to_vec()).unwrap();

        let saver = AdasavUtility::new(SaveOperation::Save);
        let image = saver.save_file(&file);
        assert_eq!(image.file_number, 5);
        assert_eq!(image.file_name, "BACKUP-TEST");
        assert_eq!(image.records.len(), 1);
    }
}
