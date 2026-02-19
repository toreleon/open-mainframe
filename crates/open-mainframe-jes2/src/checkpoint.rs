//! JES2 checkpoint and recovery — persist/restore spool and job state.

use crate::job::{Job, JobClassDef, JobState};
use crate::spool::SpoolManager;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Start mode
// ---------------------------------------------------------------------------

/// JES2 initialization start mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StartMode {
    /// Cold start — discard all checkpoint data; start fresh.
    Cold,
    /// Warm start — recover all queued, running, and output jobs from the
    /// last checkpoint.
    Warm,
    /// Quick start — like warm but skip full spool scan.
    Quick,
}

// ---------------------------------------------------------------------------
// Checkpoint data
// ---------------------------------------------------------------------------

/// Serializable snapshot of the JES2 subsystem state, written to the
/// checkpoint dataset(s).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CheckpointData {
    /// Job queue (all non-purged jobs).
    pub jobs: HashMap<u32, Job>,
    /// Next job number counter.
    pub next_job_num: u32,
    /// Job class definitions.
    pub class_defs: HashMap<String, JobClassDef>,
    /// Spool datasets.
    pub spool: SpoolManager,
    /// Monotonically increasing checkpoint sequence number.
    pub sequence: u64,
}

// ---------------------------------------------------------------------------
// Checkpoint manager
// ---------------------------------------------------------------------------

/// Configuration for checkpoint behaviour.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CheckpointConfig {
    /// Use dual checkpoint datasets for redundancy.
    pub dual: bool,
    /// Logical names for checkpoint datasets.
    pub dataset_names: Vec<String>,
}

impl Default for CheckpointConfig {
    fn default() -> Self {
        Self {
            dual: true,
            dataset_names: vec!["SYS1.HASPCKPT".to_string(), "SYS1.HASPCKP2".to_string()],
        }
    }
}

/// Manages checkpoint creation and recovery.
#[derive(Debug)]
pub struct CheckpointManager {
    /// Configuration.
    pub config: CheckpointConfig,
    /// The latest checkpoint data (in-memory mirror).
    latest: Option<CheckpointData>,
    /// Current sequence number.
    sequence: u64,
}

impl CheckpointManager {
    /// Create a new checkpoint manager.
    pub fn new(config: CheckpointConfig) -> Self {
        Self {
            config,
            latest: None,
            sequence: 0,
        }
    }

    /// Take a checkpoint — snapshot the current subsystem state.
    ///
    /// In a real system this would write to DASD. Here we store it in memory
    /// and return the serialized JSON for the caller to persist.
    pub fn take_checkpoint(
        &mut self,
        jobs: &HashMap<u32, Job>,
        next_job_num: u32,
        class_defs: &HashMap<String, JobClassDef>,
        spool: &SpoolManager,
    ) -> String {
        self.sequence += 1;
        let data = CheckpointData {
            jobs: jobs.clone(),
            next_job_num,
            class_defs: class_defs.clone(),
            spool: spool.clone(),
            sequence: self.sequence,
        };
        let json = serde_json::to_string(&data).expect("checkpoint serialization");
        self.latest = Some(data);
        json
    }

    /// Perform a warm start — restore state from checkpoint data.
    ///
    /// Running jobs are reverted to Ready state (they need to be restarted).
    pub fn warm_start(
        &mut self,
        checkpoint_json: &str,
    ) -> Result<CheckpointData, String> {
        let mut data: CheckpointData =
            serde_json::from_str(checkpoint_json).map_err(|e| e.to_string())?;

        // Running jobs revert to Ready on warm start (they were interrupted)
        for job in data.jobs.values_mut() {
            if job.state == JobState::Running {
                job.state = JobState::Ready;
            }
        }

        // Remove purged jobs — they're done
        data.jobs.retain(|_, j| !matches!(j.state, JobState::Purge));

        self.sequence = data.sequence;
        self.latest = Some(data.clone());
        Ok(data)
    }

    /// Perform a cold start — discard all state and return empty data.
    pub fn cold_start(&mut self) -> CheckpointData {
        self.sequence = 0;
        let data = CheckpointData {
            jobs: HashMap::new(),
            next_job_num: 1,
            class_defs: HashMap::new(),
            spool: SpoolManager::new(),
            sequence: 0,
        };
        self.latest = Some(data.clone());
        data
    }

    /// Get the current sequence number.
    pub fn sequence(&self) -> u64 {
        self.sequence
    }

    /// Get the latest checkpoint data (if any).
    pub fn latest(&self) -> Option<&CheckpointData> {
        self.latest.as_ref()
    }
}

// ---------------------------------------------------------------------------
// HASP messages
// ---------------------------------------------------------------------------

/// Format a HASP job-completion message.
///
/// Example: `$HASP250 JOB00123 ENDED - RC=0000`
pub fn hasp250_message(job: &Job) -> String {
    format!("$HASP250 {} ENDED - RC={:04}", job.id, job.max_rc)
}

/// Format a HASP job-submitted message.
///
/// Example: `$HASP100 PAYROLL ON READER1`
pub fn hasp100_message(job: &Job) -> String {
    format!("$HASP100 {} ({}) ON READER1", job.name, job.id)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::job::{Job, JobClass, JobClassDef, JobId, JobState};
    use crate::spool::SpoolManager;

    fn sample_state() -> (HashMap<u32, Job>, u32, HashMap<String, JobClassDef>, SpoolManager) {
        let mut jobs = HashMap::new();
        let mut job1 = Job::new(JobId(1), "RUNNING".to_string(), JobClass::Standard('A'), 10, false);
        job1.state = JobState::Running;
        job1.max_rc = 0;
        jobs.insert(1, job1);

        let mut job2 = Job::new(JobId(2), "OUTPUT".to_string(), JobClass::Standard('B'), 5, false);
        job2.state = JobState::Output;
        job2.max_rc = 4;
        jobs.insert(2, job2);

        let mut job3 = Job::new(JobId(3), "PURGED".to_string(), JobClass::Standard('A'), 1, false);
        job3.state = JobState::Purge;
        jobs.insert(3, job3);

        let mut class_defs = HashMap::new();
        class_defs.insert("A".to_string(), JobClassDef::default_for(JobClass::Standard('A')));

        let mut spool = SpoolManager::new();
        let key = spool.allocate(JobId(2), "SYSPRINT", "STEP1", 'A');
        spool.write(key, "OUTPUT LINE 1").unwrap();
        spool.write(key, "OUTPUT LINE 2").unwrap();

        (jobs, 4, class_defs, spool)
    }

    #[test]
    fn take_checkpoint_roundtrip() {
        let (jobs, next_num, class_defs, spool) = sample_state();
        let mut mgr = CheckpointManager::new(CheckpointConfig::default());

        let json = mgr.take_checkpoint(&jobs, next_num, &class_defs, &spool);
        assert_eq!(mgr.sequence(), 1);

        let data: CheckpointData = serde_json::from_str(&json).unwrap();
        assert_eq!(data.jobs.len(), 3);
        assert_eq!(data.next_job_num, 4);
        assert_eq!(data.sequence, 1);
        assert_eq!(data.spool.dataset_count(), 1);
    }

    #[test]
    fn warm_start_reverts_running() {
        let (jobs, next_num, class_defs, spool) = sample_state();
        let mut mgr = CheckpointManager::new(CheckpointConfig::default());

        let json = mgr.take_checkpoint(&jobs, next_num, &class_defs, &spool);
        let data = mgr.warm_start(&json).unwrap();

        // Running job reverted to Ready
        let job1 = data.jobs.get(&1).unwrap();
        assert_eq!(job1.state, JobState::Ready);

        // Output job preserved
        let job2 = data.jobs.get(&2).unwrap();
        assert_eq!(job2.state, JobState::Output);

        // Purged job removed
        assert!(!data.jobs.contains_key(&3));

        // Spool data preserved
        assert_eq!(data.spool.dataset_count(), 1);
    }

    #[test]
    fn cold_start_clears_everything() {
        let (jobs, next_num, class_defs, spool) = sample_state();
        let mut mgr = CheckpointManager::new(CheckpointConfig::default());
        mgr.take_checkpoint(&jobs, next_num, &class_defs, &spool);

        let data = mgr.cold_start();
        assert_eq!(data.jobs.len(), 0);
        assert_eq!(data.next_job_num, 1);
        assert_eq!(data.spool.dataset_count(), 0);
        assert_eq!(mgr.sequence(), 0);
    }

    #[test]
    fn warm_start_invalid_json() {
        let mut mgr = CheckpointManager::new(CheckpointConfig::default());
        assert!(mgr.warm_start("not json").is_err());
    }

    #[test]
    fn dual_checkpoint_config() {
        let config = CheckpointConfig::default();
        assert!(config.dual);
        assert_eq!(config.dataset_names.len(), 2);
        assert_eq!(config.dataset_names[0], "SYS1.HASPCKPT");
    }

    #[test]
    fn hasp_messages() {
        let mut job = Job::new(JobId(123), "PAYROLL".to_string(), JobClass::Standard('A'), 10, false);
        job.max_rc = 0;
        assert_eq!(hasp250_message(&job), "$HASP250 JOB00123 ENDED - RC=0000");
        assert_eq!(hasp100_message(&job), "$HASP100 PAYROLL (JOB00123) ON READER1");

        job.max_rc = 12;
        assert_eq!(hasp250_message(&job), "$HASP250 JOB00123 ENDED - RC=0012");
    }

    #[test]
    fn checkpoint_sequence_increments() {
        let (jobs, next_num, class_defs, spool) = sample_state();
        let mut mgr = CheckpointManager::new(CheckpointConfig::default());

        mgr.take_checkpoint(&jobs, next_num, &class_defs, &spool);
        assert_eq!(mgr.sequence(), 1);
        mgr.take_checkpoint(&jobs, next_num, &class_defs, &spool);
        assert_eq!(mgr.sequence(), 2);
    }

    #[test]
    fn warm_start_preserves_spool_data() {
        let (jobs, next_num, class_defs, spool) = sample_state();
        let mut mgr = CheckpointManager::new(CheckpointConfig::default());

        let json = mgr.take_checkpoint(&jobs, next_num, &class_defs, &spool);
        let data = mgr.warm_start(&json).unwrap();

        // Spool dataset for job 2 should still have its data
        let datasets = data.spool.list_for_job(JobId(2));
        assert_eq!(datasets.len(), 1);
        let content = data.spool.read(datasets[0].key).unwrap();
        assert_eq!(content, &["OUTPUT LINE 1", "OUTPUT LINE 2"]);
    }
}
