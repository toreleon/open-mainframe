//! JES2 spool management — SYSOUT storage and retrieval.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Spool dataset
// ---------------------------------------------------------------------------

/// A single dataset stored on the JES2 spool (typically a SYSOUT DD).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpoolDataset {
    /// Unique spool key.
    pub key: u64,
    /// Job identifier that owns this dataset.
    pub job_id: super::job::JobId,
    /// DD name (e.g. "SYSPRINT", "SYSOUT").
    pub dd_name: String,
    /// Step name that produced the output.
    pub step_name: String,
    /// SYSOUT class (A-Z, 0-9).
    pub sysout_class: char,
    /// Lines of output stored on the spool.
    pub data: Vec<String>,
    /// Number of records written.
    pub record_count: u64,
}

// ---------------------------------------------------------------------------
// Spool manager
// ---------------------------------------------------------------------------

/// Manages the JES2 spool storage.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SpoolManager {
    datasets: HashMap<u64, SpoolDataset>,
    next_key: u64,
}

impl SpoolManager {
    /// Create a new empty spool manager.
    pub fn new() -> Self {
        Self {
            datasets: HashMap::new(),
            next_key: 1,
        }
    }

    /// Allocate a new spool dataset and return its key.
    pub fn allocate(
        &mut self,
        job_id: super::job::JobId,
        dd_name: &str,
        step_name: &str,
        sysout_class: char,
    ) -> u64 {
        let key = self.next_key;
        self.next_key += 1;
        let ds = SpoolDataset {
            key,
            job_id,
            dd_name: dd_name.to_string(),
            step_name: step_name.to_string(),
            sysout_class,
            data: Vec::new(),
            record_count: 0,
        };
        self.datasets.insert(key, ds);
        key
    }

    /// Write a line to a spool dataset.
    pub fn write(&mut self, key: u64, line: &str) -> crate::Result<()> {
        let ds = self
            .datasets
            .get_mut(&key)
            .ok_or(crate::Jes2Error::SpoolDatasetNotFound(key))?;
        ds.data.push(line.to_string());
        ds.record_count += 1;
        Ok(())
    }

    /// Read all lines from a spool dataset.
    pub fn read(&self, key: u64) -> crate::Result<&[String]> {
        let ds = self
            .datasets
            .get(&key)
            .ok_or(crate::Jes2Error::SpoolDatasetNotFound(key))?;
        Ok(&ds.data)
    }

    /// Get a spool dataset by key.
    pub fn get(&self, key: u64) -> Option<&SpoolDataset> {
        self.datasets.get(&key)
    }

    /// Remove a spool dataset (purge).
    pub fn deallocate(&mut self, key: u64) -> Option<SpoolDataset> {
        self.datasets.remove(&key)
    }

    /// Remove all spool datasets owned by a job.
    pub fn purge_job(&mut self, job_id: super::job::JobId) -> Vec<SpoolDataset> {
        let keys: Vec<u64> = self
            .datasets
            .iter()
            .filter(|(_, ds)| ds.job_id == job_id)
            .map(|(&k, _)| k)
            .collect();
        keys.into_iter()
            .filter_map(|k| self.datasets.remove(&k))
            .collect()
    }

    /// List all spool datasets for a given job.
    pub fn list_for_job(&self, job_id: super::job::JobId) -> Vec<&SpoolDataset> {
        self.datasets
            .values()
            .filter(|ds| ds.job_id == job_id)
            .collect()
    }

    /// Total number of spool datasets.
    pub fn dataset_count(&self) -> usize {
        self.datasets.len()
    }

    /// List all spool datasets in a given SYSOUT class.
    pub fn list_by_sysout_class(&self, class: char) -> Vec<&SpoolDataset> {
        self.datasets
            .values()
            .filter(|ds| ds.sysout_class == class)
            .collect()
    }

    /// Total number of records across all spool datasets.
    pub fn total_records(&self) -> u64 {
        self.datasets.values().map(|ds| ds.record_count).sum()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::job::JobId;

    #[test]
    fn allocate_and_write() {
        let mut spool = SpoolManager::new();
        let key = spool.allocate(JobId(1), "SYSPRINT", "STEP1", 'A');
        spool.write(key, "LINE 1").unwrap();
        spool.write(key, "LINE 2").unwrap();

        let data = spool.read(key).unwrap();
        assert_eq!(data.len(), 2);
        assert_eq!(data[0], "LINE 1");
        assert_eq!(data[1], "LINE 2");

        let ds = spool.get(key).unwrap();
        assert_eq!(ds.record_count, 2);
        assert_eq!(ds.sysout_class, 'A');
    }

    #[test]
    fn read_nonexistent_fails() {
        let spool = SpoolManager::new();
        assert!(spool.read(999).is_err());
    }

    #[test]
    fn deallocate() {
        let mut spool = SpoolManager::new();
        let key = spool.allocate(JobId(1), "SYSOUT", "STEP1", 'X');
        assert_eq!(spool.dataset_count(), 1);
        let ds = spool.deallocate(key).unwrap();
        assert_eq!(ds.dd_name, "SYSOUT");
        assert_eq!(spool.dataset_count(), 0);
    }

    #[test]
    fn purge_job() {
        let mut spool = SpoolManager::new();
        spool.allocate(JobId(1), "SYSPRINT", "STEP1", 'A');
        spool.allocate(JobId(1), "SYSOUT", "STEP2", 'A');
        spool.allocate(JobId(2), "SYSPRINT", "STEP1", 'B');

        let purged = spool.purge_job(JobId(1));
        assert_eq!(purged.len(), 2);
        assert_eq!(spool.dataset_count(), 1);
    }

    #[test]
    fn list_for_job() {
        let mut spool = SpoolManager::new();
        spool.allocate(JobId(1), "DD1", "S1", 'A');
        spool.allocate(JobId(2), "DD2", "S1", 'B');
        spool.allocate(JobId(1), "DD3", "S2", 'A');

        let job1_ds = spool.list_for_job(JobId(1));
        assert_eq!(job1_ds.len(), 2);
    }

    #[test]
    fn unique_keys() {
        let mut spool = SpoolManager::new();
        let k1 = spool.allocate(JobId(1), "A", "S", 'A');
        let k2 = spool.allocate(JobId(1), "B", "S", 'A');
        assert_ne!(k1, k2);
    }

    #[test]
    fn list_by_sysout_class() {
        let mut spool = SpoolManager::new();
        spool.allocate(JobId(1), "DD1", "S1", 'A');
        spool.allocate(JobId(2), "DD2", "S1", 'B');
        spool.allocate(JobId(3), "DD3", "S1", 'A');

        let class_a = spool.list_by_sysout_class('A');
        assert_eq!(class_a.len(), 2);

        let class_b = spool.list_by_sysout_class('B');
        assert_eq!(class_b.len(), 1);

        let class_x = spool.list_by_sysout_class('X');
        assert_eq!(class_x.len(), 0);
    }

    #[test]
    fn total_records() {
        let mut spool = SpoolManager::new();
        let k1 = spool.allocate(JobId(1), "A", "S", 'A');
        let k2 = spool.allocate(JobId(2), "B", "S", 'B');

        spool.write(k1, "LINE1").unwrap();
        spool.write(k1, "LINE2").unwrap();
        spool.write(k2, "LINE1").unwrap();

        assert_eq!(spool.total_records(), 3);
    }

    #[test]
    fn spool_serialization_roundtrip() {
        let mut spool = SpoolManager::new();
        let key = spool.allocate(JobId(1), "SYSPRINT", "STEP1", 'A');
        spool.write(key, "TEST LINE").unwrap();

        let json = serde_json::to_string(&spool).unwrap();
        let restored: SpoolManager = serde_json::from_str(&json).unwrap();

        assert_eq!(restored.dataset_count(), 1);
        let data = restored.read(key).unwrap();
        assert_eq!(data, &["TEST LINE"]);
    }
}
