//! JES2 job queue with priority-based scheduling and class selection.

use crate::error::Jes2Error;
use crate::job::{Job, JobClass, JobClassDef, JobId, JobState};
use crate::spool::SpoolManager;
use std::collections::HashMap;

/// The JES2 subsystem — manages the job queue, class definitions, and spool.
#[derive(Debug)]
pub struct Jes2 {
    /// All jobs indexed by their JES2 id.
    jobs: HashMap<u32, Job>,
    /// Next job number to assign.
    next_job_num: u32,
    /// Job class definitions.
    class_defs: HashMap<String, JobClassDef>,
    /// Spool manager.
    pub spool: SpoolManager,
}

impl Default for Jes2 {
    fn default() -> Self {
        Self::new()
    }
}

impl Jes2 {
    /// Create a new JES2 subsystem with default class definitions (A-Z, 0-9,
    /// STC, TSU).
    pub fn new() -> Self {
        let mut class_defs = HashMap::new();
        // Standard classes A-Z
        for c in b'A'..=b'Z' {
            let cls = JobClass::Standard(c as char);
            class_defs.insert(cls.to_string(), JobClassDef::default_for(cls));
        }
        // Standard classes 0-9
        for c in b'0'..=b'9' {
            let cls = JobClass::Standard(c as char);
            class_defs.insert(cls.to_string(), JobClassDef::default_for(cls));
        }
        // Pseudo-classes
        class_defs.insert("STC".to_string(), JobClassDef::default_for(JobClass::Stc));
        class_defs.insert("TSU".to_string(), JobClassDef::default_for(JobClass::Tsu));

        Self {
            jobs: HashMap::new(),
            next_job_num: 1,
            class_defs,
            spool: SpoolManager::new(),
        }
    }

    // -----------------------------------------------------------------------
    // Job submission
    // -----------------------------------------------------------------------

    /// Submit a new job. Returns the assigned `JobId`.
    ///
    /// - `name`: job name (e.g. "PAYROLL")
    /// - `class`: execution class character (A-Z, 0-9) or use
    ///   [`submit_stc`]/[`submit_tsu`] for pseudo-classes
    /// - `priority`: 0-15 (clamped)
    /// - `hold`: if `true`, job enters Held state (`TYPRUN=HOLD`)
    pub fn submit(&mut self, name: &str, class: char, priority: u8, hold: bool) -> JobId {
        let cls = JobClass::standard(class).unwrap_or(JobClass::Standard('A'));
        self.submit_with_class(name, cls, priority, hold)
    }

    /// Submit a started task (STC).
    pub fn submit_stc(&mut self, name: &str, priority: u8) -> JobId {
        self.submit_with_class(name, JobClass::Stc, priority, false)
    }

    /// Submit a time-sharing user session (TSU).
    pub fn submit_tsu(&mut self, name: &str, priority: u8) -> JobId {
        self.submit_with_class(name, JobClass::Tsu, priority, false)
    }

    fn submit_with_class(
        &mut self,
        name: &str,
        class: JobClass,
        priority: u8,
        hold: bool,
    ) -> JobId {
        let id = JobId(self.next_job_num);
        self.next_job_num += 1;
        let job = Job::new(id, name.to_string(), class, priority, hold);
        self.jobs.insert(id.0, job);
        id
    }

    // -----------------------------------------------------------------------
    // Job selection (initiator interface)
    // -----------------------------------------------------------------------

    /// Select the highest-priority job in the given class that is in `Ready`
    /// state.  Returns `None` if no eligible job exists.
    pub fn select_for_class(&self, class: &JobClass) -> Option<JobId> {
        self.jobs
            .values()
            .filter(|j| j.state == JobState::Ready && &j.class == class)
            .max_by_key(|j| j.priority)
            .map(|j| j.id)
    }

    /// Select the highest-priority job from any of the given classes.
    pub fn select_for_classes(&self, classes: &[JobClass]) -> Option<JobId> {
        self.jobs
            .values()
            .filter(|j| j.state == JobState::Ready && classes.contains(&j.class))
            .max_by_key(|j| j.priority)
            .map(|j| j.id)
    }

    // -----------------------------------------------------------------------
    // Job lifecycle operations
    // -----------------------------------------------------------------------

    /// Advance a job to the next state.
    pub fn advance(&mut self, id: JobId) -> crate::Result<JobState> {
        let job = self
            .jobs
            .get_mut(&id.0)
            .ok_or_else(|| Jes2Error::JobNotFound(id.to_string()))?;
        job.advance()
    }

    /// Hold a job (`$H JOBnnnnn`).
    pub fn hold(&mut self, id: JobId) -> crate::Result<()> {
        let job = self
            .jobs
            .get_mut(&id.0)
            .ok_or_else(|| Jes2Error::JobNotFound(id.to_string()))?;
        job.hold()
    }

    /// Release a held job (`$A JOBnnnnn`).
    pub fn release(&mut self, id: JobId) -> crate::Result<()> {
        let job = self
            .jobs
            .get_mut(&id.0)
            .ok_or_else(|| Jes2Error::JobNotFound(id.to_string()))?;
        job.release()
    }

    /// Cancel a job (`$C JOBnnnnn`).
    pub fn cancel(&mut self, id: JobId) -> crate::Result<()> {
        let job = self
            .jobs
            .get_mut(&id.0)
            .ok_or_else(|| Jes2Error::JobNotFound(id.to_string()))?;
        job.cancel()
    }

    /// Purge a job and its spool datasets (`$P JOBnnnnn`).
    pub fn purge(&mut self, id: JobId) -> crate::Result<()> {
        let job = self
            .jobs
            .get_mut(&id.0)
            .ok_or_else(|| Jes2Error::JobNotFound(id.to_string()))?;
        // Job must be in Output or Cancelled state to purge
        match job.state {
            JobState::Output | JobState::Cancelled => {
                job.state = JobState::Purge;
                self.spool.purge_job(id);
                Ok(())
            }
            _ => Err(Jes2Error::InvalidTransition {
                job: id.to_string(),
                from: job.state,
                to: JobState::Purge,
            }),
        }
    }

    // -----------------------------------------------------------------------
    // Query
    // -----------------------------------------------------------------------

    /// Get a reference to a job.
    pub fn get_job(&self, id: JobId) -> Option<&Job> {
        self.jobs.get(&id.0)
    }

    /// Get a mutable reference to a job.
    pub fn get_job_mut(&mut self, id: JobId) -> Option<&mut Job> {
        self.jobs.get_mut(&id.0)
    }

    /// Total number of jobs in the system (all states).
    pub fn job_count(&self) -> usize {
        self.jobs.len()
    }

    /// List all active jobs (not purged).
    pub fn active_jobs(&self) -> Vec<&Job> {
        self.jobs
            .values()
            .filter(|j| !matches!(j.state, JobState::Purge))
            .collect()
    }

    /// List all running jobs.
    pub fn running_jobs(&self) -> Vec<&Job> {
        self.jobs
            .values()
            .filter(|j| j.state == JobState::Running)
            .collect()
    }

    /// List jobs in a given state.
    pub fn jobs_in_state(&self, state: JobState) -> Vec<&Job> {
        self.jobs.values().filter(|j| j.state == state).collect()
    }

    /// List held jobs.
    pub fn held_jobs(&self) -> Vec<&Job> {
        self.jobs.values().filter(|j| j.state.is_held()).collect()
    }

    // -----------------------------------------------------------------------
    // Class configuration
    // -----------------------------------------------------------------------

    /// Get class definition.
    pub fn get_class_def(&self, class: &JobClass) -> Option<&JobClassDef> {
        self.class_defs.get(&class.to_string())
    }

    /// Update a class definition. Returns the previous definition if it
    /// existed.
    pub fn set_class_def(&mut self, def: JobClassDef) -> Option<JobClassDef> {
        self.class_defs.insert(def.class.to_string(), def)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn submit_and_count() {
        let mut jes = Jes2::new();
        let id = jes.submit("JOB1", 'A', 10, false);
        assert_eq!(jes.job_count(), 1);
        let job = jes.get_job(id).unwrap();
        assert_eq!(job.name, "JOB1");
        assert_eq!(job.class, JobClass::Standard('A'));
        assert_eq!(job.priority, 10);
        assert_eq!(job.state, JobState::Input);
    }

    #[test]
    fn priority_ordering_selection() {
        let mut jes = Jes2::new();
        let low = jes.submit("LOW", 'A', 5, false);
        let high = jes.submit("HIGH", 'A', 15, false);
        let med = jes.submit("MED", 'A', 10, false);

        // Advance all to Ready
        for id in [low, high, med] {
            jes.advance(id).unwrap(); // -> Conversion
            jes.advance(id).unwrap(); // -> Ready
        }

        // Initiator should pick highest-priority
        let selected = jes.select_for_class(&JobClass::Standard('A')).unwrap();
        assert_eq!(selected, high);
    }

    #[test]
    fn class_selection() {
        let mut jes = Jes2::new();
        let a_job = jes.submit("JOBA", 'A', 10, false);
        let b_job = jes.submit("JOBB", 'B', 15, false);

        // Advance both to Ready
        for id in [a_job, b_job] {
            jes.advance(id).unwrap();
            jes.advance(id).unwrap();
        }

        // Selecting for class B should pick the B job
        let selected = jes.select_for_class(&JobClass::Standard('B')).unwrap();
        assert_eq!(selected, b_job);

        // Selecting from multiple classes picks highest priority overall
        let selected = jes
            .select_for_classes(&[JobClass::Standard('A'), JobClass::Standard('B')])
            .unwrap();
        assert_eq!(selected, b_job); // priority 15 > 10
    }

    #[test]
    fn hold_on_submit() {
        let mut jes = Jes2::new();
        let id = jes.submit("HOLD", 'A', 5, true);
        let job = jes.get_job(id).unwrap();
        assert!(job.state.is_held());

        // Held job should not be selectable
        assert!(jes.select_for_class(&JobClass::Standard('A')).is_none());

        // Release it
        jes.release(id).unwrap();
        let job = jes.get_job(id).unwrap();
        assert_eq!(job.state, JobState::Input);
    }

    #[test]
    fn hold_and_release_ready_job() {
        let mut jes = Jes2::new();
        let id = jes.submit("WORK", 'A', 8, false);
        jes.advance(id).unwrap(); // Conversion
        jes.advance(id).unwrap(); // Ready

        jes.hold(id).unwrap();
        assert!(jes.get_job(id).unwrap().state.is_held());
        assert!(jes.select_for_class(&JobClass::Standard('A')).is_none());

        jes.release(id).unwrap();
        assert_eq!(jes.get_job(id).unwrap().state, JobState::Ready);
        // Now selectable again
        assert_eq!(
            jes.select_for_class(&JobClass::Standard('A')).unwrap(),
            id
        );
    }

    #[test]
    fn cancel_running_job() {
        let mut jes = Jes2::new();
        let id = jes.submit("RUN", 'A', 5, false);
        jes.advance(id).unwrap(); // Conversion
        jes.advance(id).unwrap(); // Ready
        jes.advance(id).unwrap(); // Running

        jes.cancel(id).unwrap();
        assert_eq!(jes.get_job(id).unwrap().state, JobState::Cancelled);
    }

    #[test]
    fn purge_output_job() {
        let mut jes = Jes2::new();
        let id = jes.submit("OUT", 'A', 5, false);
        // Drive to Output
        jes.advance(id).unwrap(); // Conversion
        jes.advance(id).unwrap(); // Ready
        jes.advance(id).unwrap(); // Running
        jes.advance(id).unwrap(); // Output

        jes.purge(id).unwrap();
        assert_eq!(jes.get_job(id).unwrap().state, JobState::Purge);
    }

    #[test]
    fn purge_input_fails() {
        let mut jes = Jes2::new();
        let id = jes.submit("EARLY", 'A', 5, false);
        assert!(jes.purge(id).is_err());
    }

    #[test]
    fn stc_and_tsu() {
        let mut jes = Jes2::new();
        let stc = jes.submit_stc("VTAM", 15);
        let tsu = jes.submit_tsu("ISPF", 10);

        let stc_job = jes.get_job(stc).unwrap();
        assert_eq!(stc_job.class, JobClass::Stc);

        let tsu_job = jes.get_job(tsu).unwrap();
        assert_eq!(tsu_job.class, JobClass::Tsu);
    }

    #[test]
    fn running_jobs_query() {
        let mut jes = Jes2::new();
        let id1 = jes.submit("RUN1", 'A', 5, false);
        let _id2 = jes.submit("WAIT", 'A', 5, false);

        // Advance id1 to Running
        jes.advance(id1).unwrap();
        jes.advance(id1).unwrap();
        jes.advance(id1).unwrap();

        let running = jes.running_jobs();
        assert_eq!(running.len(), 1);
        assert_eq!(running[0].id, id1);
    }

    #[test]
    fn class_def_configuration() {
        let mut jes = Jes2::new();
        let def = jes.get_class_def(&JobClass::Standard('A')).unwrap();
        assert_eq!(def.proclib, "PROC00");

        // Override
        let mut custom = JobClassDef::default_for(JobClass::Standard('A'));
        custom.proclib = "MYPROC".to_string();
        custom.msgclass = 'X';
        custom.max_rc = 8;
        jes.set_class_def(custom);

        let def = jes.get_class_def(&JobClass::Standard('A')).unwrap();
        assert_eq!(def.proclib, "MYPROC");
        assert_eq!(def.msgclass, 'X');
        assert_eq!(def.max_rc, 8);
    }

    #[test]
    fn held_jobs_query() {
        let mut jes = Jes2::new();
        let id1 = jes.submit("H1", 'A', 5, true);
        let _id2 = jes.submit("NH", 'A', 5, false);
        let id3 = jes.submit("H2", 'B', 3, false);
        jes.hold(id3).unwrap();

        let held = jes.held_jobs();
        assert_eq!(held.len(), 2);
        let held_ids: Vec<JobId> = held.iter().map(|j| j.id).collect();
        assert!(held_ids.contains(&id1));
        assert!(held_ids.contains(&id3));
    }

    #[test]
    fn default_classes_exist() {
        let jes = Jes2::new();
        // All A-Z
        for c in b'A'..=b'Z' {
            assert!(jes
                .get_class_def(&JobClass::Standard(c as char))
                .is_some());
        }
        // All 0-9
        for c in b'0'..=b'9' {
            assert!(jes
                .get_class_def(&JobClass::Standard(c as char))
                .is_some());
        }
        // Pseudo-classes
        assert!(jes.get_class_def(&JobClass::Stc).is_some());
        assert!(jes.get_class_def(&JobClass::Tsu).is_some());
    }

    #[test]
    fn spool_integration() {
        let mut jes = Jes2::new();
        let id = jes.submit("SPOOL", 'A', 5, false);

        // Allocate spool dataset
        let key = jes.spool.allocate(id, "SYSPRINT", "STEP1", 'A');

        // Track it on the job
        jes.get_job_mut(id).unwrap().spool_keys.push(key);

        // Write some output
        jes.spool.write(key, "HELLO FROM JES2").unwrap();

        // Verify
        let data = jes.spool.read(key).unwrap();
        assert_eq!(data, &["HELLO FROM JES2"]);

        // Advance to Output and purge
        jes.advance(id).unwrap(); // Conversion
        jes.advance(id).unwrap(); // Ready
        jes.advance(id).unwrap(); // Running
        jes.advance(id).unwrap(); // Output
        jes.purge(id).unwrap();

        // Spool dataset should be gone
        assert!(jes.spool.get(key).is_none());
    }
}
