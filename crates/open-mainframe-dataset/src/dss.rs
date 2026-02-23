//! # DFSMSdss — Data Set Services (ADRDSSU)
//!
//! Implements z/OS DFSMSdss commands:
//!
//! - **DUMP** — Logical dataset-level backup to dump dataset
//! - **RESTORE** — Recover datasets from dump dataset
//! - **COPY** — Move/copy datasets between volumes
//! - **PRINT** — Display dataset contents
//! - **INCLUDE/EXCLUDE** — Wildcard-based dataset filtering
//! - **Dump Format** — Header + per-dataset catalog info + data

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use crate::error::DatasetError;
use crate::hsm::rle_compress;
use crate::hsm::rle_decompress;

// ─────────────────────── Dump Format ───────────────────────

/// Dump dataset header.
#[derive(Debug, Clone)]
pub struct DumpHeader {
    /// Dump creation timestamp.
    pub created: SystemTime,
    /// System identifier.
    pub system_id: String,
    /// DFSMSdss version.
    pub version: String,
    /// Number of datasets in the dump.
    pub dataset_count: u32,
}

/// A single dataset record within a dump.
#[derive(Debug, Clone)]
pub struct DumpDatasetRecord {
    /// Dataset name.
    pub dsn: String,
    /// Original size in bytes.
    pub original_size: u64,
    /// Data (compressed if COMPRESS was specified).
    pub data: Vec<u8>,
    /// Whether data is compressed.
    pub compressed: bool,
    /// Dataset attributes (key-value metadata).
    pub attributes: HashMap<String, String>,
}

/// A complete dump dataset.
#[derive(Debug, Clone)]
pub struct DumpDataset {
    /// Dump header.
    pub header: DumpHeader,
    /// Dataset records.
    pub records: Vec<DumpDatasetRecord>,
}

impl DumpDataset {
    /// Create a new empty dump.
    fn new(system_id: &str) -> Self {
        Self {
            header: DumpHeader {
                created: SystemTime::now(),
                system_id: system_id.to_string(),
                version: "ADRDSSU V1.0".to_string(),
                dataset_count: 0,
            },
            records: Vec::new(),
        }
    }

    /// Add a dataset record.
    fn add_record(&mut self, record: DumpDatasetRecord) {
        self.records.push(record);
        self.header.dataset_count = self.records.len() as u32;
    }

    /// Serialize to bytes for storage.
    pub fn serialize(&self) -> Vec<u8> {
        let mut output = Vec::new();

        // Header line
        output.extend_from_slice(
            format!(
                "ADRDSSU DUMP V1.0 SYSTEM={} DATASETS={}\n",
                self.header.system_id, self.header.dataset_count
            )
            .as_bytes(),
        );

        // Each dataset record
        for rec in &self.records {
            output.extend_from_slice(
                format!(
                    "DATASET={} SIZE={} COMPRESSED={}\n",
                    rec.dsn, rec.original_size, rec.compressed
                )
                .as_bytes(),
            );

            // Attributes
            for (k, v) in &rec.attributes {
                output.extend_from_slice(format!("ATTR={},{}\n", k, v).as_bytes());
            }

            // Data as hex
            output.extend_from_slice(b"DATA=");
            for byte in &rec.data {
                output.extend_from_slice(format!("{byte:02X}").as_bytes());
            }
            output.push(b'\n');
            output.extend_from_slice(b"ENDDS\n");
        }

        output.extend_from_slice(b"ENDDUMP\n");
        output
    }

    /// Deserialize from bytes.
    pub fn deserialize(data: &[u8]) -> Result<Self, DatasetError> {
        let text = String::from_utf8_lossy(data);
        let mut dump = DumpDataset::new("RESTORED");
        let mut current_dsn = String::new();
        let mut current_size: u64 = 0;
        let mut current_compressed = false;
        let mut current_data = Vec::new();
        let mut current_attrs = HashMap::new();
        let mut in_dataset = false;

        for line in text.lines() {
            if line.starts_with("ADRDSSU DUMP") {
                if let Some(sys) = line.split("SYSTEM=").nth(1) {
                    if let Some(sys_id) = sys.split_whitespace().next() {
                        dump.header.system_id = sys_id.to_string();
                    }
                }
            } else if line.starts_with("DATASET=") {
                in_dataset = true;
                let parts: Vec<&str> = line.split_whitespace().collect();
                current_dsn = parts[0]
                    .strip_prefix("DATASET=")
                    .unwrap_or("")
                    .to_string();
                for part in &parts[1..] {
                    if let Some(sz) = part.strip_prefix("SIZE=") {
                        current_size = sz.parse().unwrap_or(0);
                    }
                    if let Some(c) = part.strip_prefix("COMPRESSED=") {
                        current_compressed = c == "true";
                    }
                }
            } else if line.starts_with("ATTR=") && in_dataset {
                let kv = line.strip_prefix("ATTR=").unwrap_or("");
                if let Some((k, v)) = kv.split_once(',') {
                    current_attrs.insert(k.to_string(), v.to_string());
                }
            } else if line.starts_with("DATA=") && in_dataset {
                let hex = line.strip_prefix("DATA=").unwrap_or("");
                current_data = hex_to_bytes(hex);
            } else if line == "ENDDS" && in_dataset {
                dump.add_record(DumpDatasetRecord {
                    dsn: std::mem::take(&mut current_dsn),
                    original_size: current_size,
                    data: std::mem::take(&mut current_data),
                    compressed: current_compressed,
                    attributes: std::mem::take(&mut current_attrs),
                });
                in_dataset = false;
                current_size = 0;
                current_compressed = false;
            }
        }

        Ok(dump)
    }
}

fn hex_to_bytes(hex: &str) -> Vec<u8> {
    let mut bytes = Vec::new();
    let mut chars = hex.chars();
    while let (Some(h), Some(l)) = (chars.next(), chars.next()) {
        if let Ok(byte) = u8::from_str_radix(&format!("{h}{l}"), 16) {
            bytes.push(byte);
        }
    }
    bytes
}

// ─────────────────────── INCLUDE/EXCLUDE Filter ───────────────────────

/// Dataset filter for INCLUDE/EXCLUDE.
#[derive(Debug, Clone)]
pub struct DssFilter {
    /// Include patterns.
    pub include: Vec<String>,
    /// Exclude patterns.
    pub exclude: Vec<String>,
    /// BY criteria (attribute-based, simplified).
    pub by_criteria: Vec<ByCriterion>,
}

/// A BY criterion for attribute-based filtering.
#[derive(Debug, Clone)]
pub struct ByCriterion {
    /// Attribute name (e.g., CREDT, REFDT).
    pub attribute: String,
    /// Operator (GE, LE, EQ, etc.).
    pub operator: String,
    /// Value.
    pub value: String,
}

impl DssFilter {
    /// Create a new filter.
    pub fn new(include: Vec<String>, exclude: Vec<String>) -> Self {
        Self {
            include,
            exclude,
            by_criteria: Vec::new(),
        }
    }

    /// Create an include-only filter.
    pub fn include_only(patterns: Vec<String>) -> Self {
        Self::new(patterns, Vec::new())
    }

    /// Check if a DSN matches the filter.
    pub fn matches(&self, dsn: &str) -> bool {
        let dsn_upper = dsn.to_uppercase();

        // Must match at least one include pattern
        let included = self.include.is_empty()
            || self.include.iter().any(|p| pattern_matches(&dsn_upper, p));

        if !included {
            return false;
        }

        // Must not match any exclude pattern
        !self.exclude.iter().any(|p| pattern_matches(&dsn_upper, p))
    }

    /// Filter a list of DSNs.
    pub fn filter_dsns<'a>(&self, dsns: &'a [String]) -> Vec<&'a String> {
        dsns.iter().filter(|d| self.matches(d)).collect()
    }
}

/// Pattern matching for DSN filters.
/// Supports `**` (any qualifiers) and `*` (single qualifier component).
fn pattern_matches(dsn: &str, pattern: &str) -> bool {
    let pat = pattern.to_uppercase();
    if pat.ends_with(".**") {
        let prefix = &pat[..pat.len() - 3];
        dsn.starts_with(prefix) && (dsn.len() > prefix.len())
    } else if pat.ends_with(".*") {
        let prefix = &pat[..pat.len() - 2];
        dsn.starts_with(prefix)
            && dsn[prefix.len()..].starts_with('.')
            && !dsn[prefix.len() + 1..].contains('.')
    } else {
        dsn == pat
    }
}

// ─────────────────────── DFSMSdss Engine ───────────────────────

/// DFSMSdss (ADRDSSU) engine.
#[derive(Debug)]
pub struct Dss {
    /// Base directory for datasets.
    base_dir: PathBuf,
    /// Output messages.
    output: Vec<String>,
    /// Return code.
    return_code: u32,
}

impl Dss {
    /// Create a new DFSMSdss instance.
    pub fn new(base_dir: impl AsRef<Path>) -> Self {
        Self {
            base_dir: base_dir.as_ref().to_path_buf(),
            output: Vec::new(),
            return_code: 0,
        }
    }

    /// Get accumulated output messages.
    pub fn output(&self) -> &[String] {
        &self.output
    }

    /// Get return code.
    pub fn return_code(&self) -> u32 {
        self.return_code
    }

    /// Logical DUMP — dump matching datasets to a dump dataset.
    pub fn dump(
        &mut self,
        filter: &DssFilter,
        available_dsns: &[String],
        compress: bool,
    ) -> Result<DumpDataset, DatasetError> {
        self.output.clear();
        self.return_code = 0;
        self.output.push("ADR101I DUMP PROCESSING BEGINS".to_string());

        let mut dump = DumpDataset::new("OPENMF");
        let matching = filter.filter_dsns(available_dsns);
        let mut dumped = 0u32;

        for dsn in matching {
            let path = self.dsn_to_path(dsn);
            if !path.exists() {
                self.output.push(format!("ADR302W DATASET {} NOT FOUND ON VOLUME", dsn));
                continue;
            }

            let raw_data = std::fs::read(&path)?;
            let original_size = raw_data.len() as u64;

            let (data, is_compressed) = if compress {
                (rle_compress(&raw_data), true)
            } else {
                (raw_data, false)
            };

            let mut attributes = HashMap::new();
            attributes.insert("DSORG".to_string(), "PS".to_string());

            dump.add_record(DumpDatasetRecord {
                dsn: dsn.clone(),
                original_size,
                data,
                compressed: is_compressed,
                attributes,
            });

            self.output.push(format!("ADR006I DATASET {} DUMPED ({} BYTES)", dsn, original_size));
            dumped += 1;
        }

        self.output.push(format!(
            "ADR013I DUMP COMPLETE - {} DATASETS DUMPED",
            dumped
        ));

        Ok(dump)
    }

    /// Logical RESTORE — restore datasets from a dump dataset.
    pub fn restore(
        &mut self,
        dump: &DumpDataset,
        filter: &DssFilter,
        rename: Option<(&str, &str)>,
        replace: bool,
    ) -> Result<u32, DatasetError> {
        self.output.clear();
        self.return_code = 0;
        self.output.push("ADR101I RESTORE PROCESSING BEGINS".to_string());

        let mut restored = 0u32;

        for record in &dump.records {
            if !filter.matches(&record.dsn) {
                continue;
            }

            // Apply rename if specified
            let target_dsn = if let Some((from, to)) = rename {
                let from_upper = from.to_uppercase();
                let to_upper = to.to_uppercase();
                let from_prefix = from_upper.trim_end_matches(".**");
                let to_prefix = to_upper.trim_end_matches(".**");
                if record.dsn.to_uppercase().starts_with(from_prefix) {
                    format!(
                        "{}{}",
                        to_prefix,
                        &record.dsn[from_prefix.len()..]
                    )
                } else {
                    record.dsn.clone()
                }
            } else {
                record.dsn.clone()
            };

            let target_path = self.dsn_to_path(&target_dsn);

            if target_path.exists() && !replace {
                self.output.push(format!(
                    "ADR303E DATASET {} EXISTS - USE REPLACE",
                    target_dsn
                ));
                self.return_code = 8;
                continue;
            }

            // Decompress if needed
            let data = if record.compressed {
                rle_decompress(&record.data)
            } else {
                record.data.clone()
            };

            if let Some(parent) = target_path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            std::fs::write(&target_path, &data)?;

            self.output.push(format!(
                "ADR006I DATASET {} RESTORED ({} BYTES)",
                target_dsn,
                data.len()
            ));
            restored += 1;
        }

        self.output.push(format!(
            "ADR013I RESTORE COMPLETE - {} DATASETS RESTORED",
            restored
        ));

        Ok(restored)
    }

    /// COPY — copy datasets with optional delete of source.
    pub fn copy(
        &mut self,
        filter: &DssFilter,
        available_dsns: &[String],
        target_dir: &Path,
        rename: Option<(&str, &str)>,
        delete_source: bool,
    ) -> Result<u32, DatasetError> {
        self.output.clear();
        self.return_code = 0;
        self.output.push("ADR101I COPY PROCESSING BEGINS".to_string());

        let matching = filter.filter_dsns(available_dsns);
        let mut copied = 0u32;

        for dsn in matching {
            let source_path = self.dsn_to_path(dsn);
            if !source_path.exists() {
                self.output.push(format!("ADR302W DATASET {} NOT FOUND", dsn));
                continue;
            }

            // Apply rename if specified
            let target_dsn = if let Some((from, to)) = rename {
                let from_upper = from.to_uppercase();
                let to_upper = to.to_uppercase();
                let from_prefix = from_upper.trim_end_matches(".**");
                let to_prefix = to_upper.trim_end_matches(".**");
                if dsn.to_uppercase().starts_with(from_prefix) {
                    format!("{}{}", to_prefix, &dsn[from_prefix.len()..])
                } else {
                    dsn.clone()
                }
            } else {
                dsn.clone()
            };

            let mut target_path = target_dir.to_path_buf();
            for component in target_dsn.split('.') {
                target_path.push(component);
            }

            if let Some(parent) = target_path.parent() {
                std::fs::create_dir_all(parent)?;
            }

            let data = std::fs::read(&source_path)?;
            std::fs::write(&target_path, &data)?;

            self.output.push(format!("ADR006I DATASET {} COPIED", dsn));
            copied += 1;

            if delete_source {
                let _ = std::fs::remove_file(&source_path);
                self.output.push(format!("ADR007I DATASET {} DELETED FROM SOURCE", dsn));
            }
        }

        self.output.push(format!(
            "ADR013I COPY COMPLETE - {} DATASETS COPIED",
            copied
        ));

        Ok(copied)
    }

    /// PRINT — display dataset contents.
    pub fn print_dataset(
        &mut self,
        dsn: &str,
        max_records: Option<usize>,
    ) -> Result<String, DatasetError> {
        self.output.clear();
        self.return_code = 0;

        let path = self.dsn_to_path(dsn);
        if !path.exists() {
            self.return_code = 8;
            return Err(DatasetError::IoError {
                message: format!("Dataset '{}' not found", dsn),
            });
        }

        let data = std::fs::read(&path)?;
        let limit = max_records.unwrap_or(usize::MAX);
        let mut result = String::new();

        result.push_str(&format!("ADR101I PRINT DATASET {}\n", dsn));

        let chunk_size = 16;
        let mut records_printed = 0;

        for (i, chunk) in data.chunks(chunk_size).enumerate() {
            if records_printed >= limit {
                break;
            }

            let offset = i * chunk_size;

            // Hex portion
            result.push_str(&format!("{:08X}  ", offset));
            for byte in chunk {
                result.push_str(&format!("{:02X} ", byte));
            }
            // Pad if last chunk is short
            for _ in chunk.len()..chunk_size {
                result.push_str("   ");
            }

            // Character portion
            result.push_str(" |");
            for byte in chunk {
                let c = if *byte >= 0x20 && *byte < 0x7F {
                    *byte as char
                } else {
                    '.'
                };
                result.push(c);
            }
            result.push_str("|\n");
            records_printed += 1;
        }

        result.push_str(&format!(
            "ADR013I {} RECORDS PRINTED\n",
            records_printed
        ));

        self.output.push(format!("ADR006I PRINT {} COMPLETE", dsn));
        Ok(result)
    }

    /// Convert DSN to filesystem path.
    fn dsn_to_path(&self, dsn: &str) -> PathBuf {
        let mut path = self.base_dir.clone();
        for component in dsn.split('.') {
            path.push(component);
        }
        path
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn test_dir() -> PathBuf {
        let count = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        std::env::temp_dir().join(format!("dss_test_{}", count))
    }

    fn cleanup(path: &Path) {
        let _ = std::fs::remove_dir_all(path);
    }

    fn create_test_file(dir: &Path, dsn: &str, data: &[u8]) -> PathBuf {
        let mut path = dir.to_path_buf();
        for component in dsn.split('.') {
            path.push(component);
        }
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }
        std::fs::write(&path, data).unwrap();
        path
    }

    // ─── DFSMS-106.1: Logical DUMP ───

    #[test]
    fn test_dump_matching_datasets() {
        let dir = test_dir();
        cleanup(&dir);

        create_test_file(&dir, "MY.DATA.FILE1", b"file1 content");
        create_test_file(&dir, "MY.DATA.FILE2", b"file2 content");
        create_test_file(&dir, "OTHER.DS", b"other");

        let mut dss = Dss::new(&dir);
        let filter = DssFilter::include_only(vec!["MY.DATA.**".to_string()]);
        let dsns = vec![
            "MY.DATA.FILE1".to_string(),
            "MY.DATA.FILE2".to_string(),
            "OTHER.DS".to_string(),
        ];

        let dump = dss.dump(&filter, &dsns, false).unwrap();
        assert_eq!(dump.header.dataset_count, 2);
        assert_eq!(dump.records.len(), 2);
        assert_eq!(dss.return_code(), 0);

        cleanup(&dir);
    }

    #[test]
    fn test_dump_with_compress() {
        let dir = test_dir();
        cleanup(&dir);

        let data = vec![b'X'; 1000];
        create_test_file(&dir, "BIG.DS", &data);

        let mut dss = Dss::new(&dir);
        let filter = DssFilter::include_only(vec!["BIG.DS".to_string()]);
        let dsns = vec!["BIG.DS".to_string()];

        let dump = dss.dump(&filter, &dsns, true).unwrap();
        assert!(dump.records[0].compressed);
        assert!(dump.records[0].data.len() < 1000);

        cleanup(&dir);
    }

    // ─── DFSMS-106.2: Logical RESTORE ───

    #[test]
    fn test_restore_from_dump() {
        let dir = test_dir();
        cleanup(&dir);

        let original = b"restore this content";
        create_test_file(&dir, "RESTORE.DS", original);

        let mut dss = Dss::new(&dir);
        let filter = DssFilter::include_only(vec!["RESTORE.DS".to_string()]);
        let dsns = vec!["RESTORE.DS".to_string()];

        // Dump
        let dump = dss.dump(&filter, &dsns, false).unwrap();

        // Delete original
        let path = dir.join("RESTORE/DS");
        std::fs::remove_file(&path).unwrap();

        // Restore
        let count = dss.restore(&dump, &filter, None, true).unwrap();
        assert_eq!(count, 1);

        let restored = std::fs::read(&path).unwrap();
        assert_eq!(restored, original);

        cleanup(&dir);
    }

    #[test]
    fn test_restore_with_rename() {
        let dir = test_dir();
        cleanup(&dir);

        create_test_file(&dir, "MY.DATA.FILE", b"rename test");

        let mut dss = Dss::new(&dir);
        let filter = DssFilter::include_only(vec!["MY.DATA.**".to_string()]);
        let dsns = vec!["MY.DATA.FILE".to_string()];

        let dump = dss.dump(&filter, &dsns, false).unwrap();

        let count = dss
            .restore(&dump, &filter, Some(("MY.DATA.**", "MY.COPY.**")), true)
            .unwrap();
        assert_eq!(count, 1);

        let renamed_path = dir.join("MY/COPY/FILE");
        assert!(renamed_path.exists());
        assert_eq!(std::fs::read(&renamed_path).unwrap(), b"rename test");

        cleanup(&dir);
    }

    #[test]
    fn test_restore_replace_required() {
        let dir = test_dir();
        cleanup(&dir);

        create_test_file(&dir, "EXISTS.DS", b"existing");

        let mut dss = Dss::new(&dir);
        let filter = DssFilter::include_only(vec!["EXISTS.DS".to_string()]);
        let dsns = vec!["EXISTS.DS".to_string()];

        let dump = dss.dump(&filter, &dsns, false).unwrap();

        // Without replace, should fail (CC=8)
        dss.restore(&dump, &filter, None, false).unwrap();
        assert_eq!(dss.return_code(), 8);

        // With replace, should succeed
        let count = dss.restore(&dump, &filter, None, true).unwrap();
        assert_eq!(count, 1);

        cleanup(&dir);
    }

    // ─── DFSMS-106.3: Dataset COPY ───

    #[test]
    fn test_copy_datasets() {
        let dir = test_dir();
        let target = test_dir();
        cleanup(&dir);
        cleanup(&target);

        create_test_file(&dir, "COPY.DS1", b"copy data 1");
        create_test_file(&dir, "COPY.DS2", b"copy data 2");

        let mut dss = Dss::new(&dir);
        let filter = DssFilter::include_only(vec!["COPY.**".to_string()]);
        let dsns = vec!["COPY.DS1".to_string(), "COPY.DS2".to_string()];

        let count = dss.copy(&filter, &dsns, &target, None, false).unwrap();
        assert_eq!(count, 2);

        assert!(target.join("COPY/DS1").exists());
        assert!(target.join("COPY/DS2").exists());

        // Source should still exist (no delete)
        assert!(dir.join("COPY/DS1").exists());

        cleanup(&dir);
        cleanup(&target);
    }

    #[test]
    fn test_copy_with_delete() {
        let dir = test_dir();
        let target = test_dir();
        cleanup(&dir);
        cleanup(&target);

        create_test_file(&dir, "MOVE.DS", b"move data");

        let mut dss = Dss::new(&dir);
        let filter = DssFilter::include_only(vec!["MOVE.DS".to_string()]);
        let dsns = vec!["MOVE.DS".to_string()];

        let count = dss.copy(&filter, &dsns, &target, None, true).unwrap();
        assert_eq!(count, 1);

        // Target exists, source deleted
        assert!(target.join("MOVE/DS").exists());
        assert!(!dir.join("MOVE/DS").exists());

        cleanup(&dir);
        cleanup(&target);
    }

    // ─── DFSMS-106.4: INCLUDE/EXCLUDE Filtering ───

    #[test]
    fn test_include_filter() {
        let filter = DssFilter::include_only(vec!["PROD.**".to_string()]);
        assert!(filter.matches("PROD.DATA.FILE"));
        assert!(filter.matches("PROD.BACKUP.DAILY"));
        assert!(!filter.matches("TEST.DATA.FILE"));
    }

    #[test]
    fn test_include_exclude_filter() {
        let filter = DssFilter::new(
            vec!["PROD.**".to_string()],
            vec!["PROD.TEMP.**".to_string()],
        );
        assert!(filter.matches("PROD.DATA.FILE"));
        assert!(!filter.matches("PROD.TEMP.WORK"));
        assert!(!filter.matches("TEST.DS"));
    }

    #[test]
    fn test_filter_dsns() {
        let filter = DssFilter::new(
            vec!["APP.**".to_string()],
            vec!["APP.LOG.**".to_string()],
        );

        let dsns = vec![
            "APP.DATA.FILE".to_string(),
            "APP.LOG.2026".to_string(),
            "APP.CONFIG".to_string(),
            "OTHER.DS".to_string(),
        ];

        let matched = filter.filter_dsns(&dsns);
        assert_eq!(matched.len(), 2); // APP.DATA.FILE and APP.CONFIG
    }

    // ─── DFSMS-106.5: PRINT ───

    #[test]
    fn test_print_dataset() {
        let dir = test_dir();
        cleanup(&dir);

        create_test_file(&dir, "PRINT.DS", b"Hello PRINT");

        let mut dss = Dss::new(&dir);
        let result = dss.print_dataset("PRINT.DS", None).unwrap();

        assert!(result.contains("PRINT DATASET PRINT.DS"));
        assert!(result.contains("Hello PRINT")); // In character portion
        assert!(result.contains("RECORDS PRINTED"));

        cleanup(&dir);
    }

    #[test]
    fn test_print_with_count() {
        let dir = test_dir();
        cleanup(&dir);

        // Create 256 bytes of data (16 records of 16 bytes)
        let data: Vec<u8> = (0..=255).collect();
        create_test_file(&dir, "MANY.DS", &data);

        let mut dss = Dss::new(&dir);
        let result = dss.print_dataset("MANY.DS", Some(3)).unwrap();
        assert!(result.contains("3 RECORDS PRINTED"));

        cleanup(&dir);
    }

    // ─── DFSMS-106.6: Dump Format ───

    #[test]
    fn test_dump_format_header() {
        let dir = test_dir();
        cleanup(&dir);

        create_test_file(&dir, "FMT.DS", b"format test");

        let mut dss = Dss::new(&dir);
        let filter = DssFilter::include_only(vec!["FMT.DS".to_string()]);
        let dsns = vec!["FMT.DS".to_string()];

        let dump = dss.dump(&filter, &dsns, false).unwrap();

        assert_eq!(dump.header.system_id, "OPENMF");
        assert_eq!(dump.header.version, "ADRDSSU V1.0");
        assert_eq!(dump.header.dataset_count, 1);
    }

    #[test]
    fn test_dump_serialize_deserialize() {
        let dir = test_dir();
        cleanup(&dir);

        create_test_file(&dir, "SER.DS1", b"dataset one");
        create_test_file(&dir, "SER.DS2", b"dataset two");

        let mut dss = Dss::new(&dir);
        let filter = DssFilter::include_only(vec!["SER.**".to_string()]);
        let dsns = vec!["SER.DS1".to_string(), "SER.DS2".to_string()];

        let dump = dss.dump(&filter, &dsns, false).unwrap();

        // Serialize and deserialize
        let serialized = dump.serialize();
        let restored = DumpDataset::deserialize(&serialized).unwrap();

        assert_eq!(restored.header.dataset_count, 2);
        assert_eq!(restored.records.len(), 2);
        assert_eq!(restored.records[0].dsn, "SER.DS1");
        assert_eq!(restored.records[1].dsn, "SER.DS2");
        assert_eq!(restored.records[0].data, b"dataset one");
        assert_eq!(restored.records[1].data, b"dataset two");

        cleanup(&dir);
    }

    // ─── DFSMS-106.7: Integration Tests ───

    #[test]
    fn test_dump_restore_cycle_5_datasets() {
        let dir = test_dir();
        cleanup(&dir);

        // Create 5 datasets with different content
        let datasets: Vec<(String, Vec<u8>)> = (0..5)
            .map(|i| {
                let dsn = format!("CYCLE.DS{}", i);
                let data = format!("Content for dataset {}: {}", i, "Z".repeat(50 * (i + 1)))
                    .into_bytes();
                (dsn, data)
            })
            .collect();

        for (dsn, data) in &datasets {
            create_test_file(&dir, dsn, data);
        }

        let dsns: Vec<String> = datasets.iter().map(|(d, _)| d.clone()).collect();
        let filter = DssFilter::include_only(vec!["CYCLE.**".to_string()]);

        // Dump with compression
        let mut dss = Dss::new(&dir);
        let dump = dss.dump(&filter, &dsns, true).unwrap();
        assert_eq!(dump.records.len(), 5);

        // Delete all originals
        for (dsn, _) in &datasets {
            let mut path = dir.clone();
            for c in dsn.split('.') {
                path.push(c);
            }
            let _ = std::fs::remove_file(&path);
        }

        // Restore all
        let count = dss.restore(&dump, &filter, None, true).unwrap();
        assert_eq!(count, 5);

        // Verify content
        for (dsn, original) in &datasets {
            let mut path = dir.clone();
            for c in dsn.split('.') {
                path.push(c);
            }
            let restored = std::fs::read(&path).unwrap();
            assert_eq!(&restored, original, "Mismatch for {}", dsn);
        }

        cleanup(&dir);
    }

    #[test]
    fn test_copy_with_rename_and_replace() {
        let dir = test_dir();
        let target = test_dir();
        cleanup(&dir);
        cleanup(&target);

        create_test_file(&dir, "SRC.DATA.A", b"data A");
        create_test_file(&dir, "SRC.DATA.B", b"data B");

        // Pre-create a target to test replace
        create_test_file(&target, "DST.DATA.A", b"old data A");

        let mut dss = Dss::new(&dir);
        let filter = DssFilter::include_only(vec!["SRC.DATA.**".to_string()]);
        let dsns = vec!["SRC.DATA.A".to_string(), "SRC.DATA.B".to_string()];

        let count = dss
            .copy(&filter, &dsns, &target, Some(("SRC.DATA.**", "DST.DATA.**")), false)
            .unwrap();
        assert_eq!(count, 2);

        // Verify renamed targets
        assert_eq!(
            std::fs::read(target.join("DST/DATA/A")).unwrap(),
            b"data A"
        );
        assert_eq!(
            std::fs::read(target.join("DST/DATA/B")).unwrap(),
            b"data B"
        );

        cleanup(&dir);
        cleanup(&target);
    }

    #[test]
    fn test_dump_format_round_trip_with_attributes() {
        let mut dump = DumpDataset::new("TESTSYS");

        let mut attrs = HashMap::new();
        attrs.insert("DSORG".to_string(), "PS".to_string());
        attrs.insert("RECFM".to_string(), "FB".to_string());

        dump.add_record(DumpDatasetRecord {
            dsn: "ATTR.DS".to_string(),
            original_size: 100,
            data: b"test data".to_vec(),
            compressed: false,
            attributes: attrs,
        });

        let serialized = dump.serialize();
        let text = String::from_utf8_lossy(&serialized);
        assert!(text.contains("SYSTEM=TESTSYS"));
        assert!(text.contains("DATASET=ATTR.DS"));
        assert!(text.contains("ATTR=DSORG,PS"));
        assert!(text.contains("ATTR=RECFM,FB"));

        let restored = DumpDataset::deserialize(&serialized).unwrap();
        assert_eq!(restored.records[0].dsn, "ATTR.DS");
        assert_eq!(
            restored.records[0].attributes.get("DSORG").unwrap(),
            "PS"
        );
        assert_eq!(
            restored.records[0].attributes.get("RECFM").unwrap(),
            "FB"
        );
        assert_eq!(restored.records[0].data, b"test data");
    }
}
