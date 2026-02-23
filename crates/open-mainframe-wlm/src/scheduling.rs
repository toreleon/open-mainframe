//! # WLM-Managed Initiator Scheduling
//!
//! Dynamically starts and stops batch initiators based on service class goals,
//! job queue depth, scheduling environments, and job class affinity.

use std::collections::HashMap;

use crate::service::Importance;

// ─────────────────────── Job Queue ───────────────────────

/// A queued batch job.
#[derive(Debug, Clone)]
pub struct QueuedJob {
    /// Job name.
    pub name: String,
    /// Job class.
    pub job_class: String,
    /// Service class assigned.
    pub service_class: String,
    /// Importance of the service class.
    pub importance: Importance,
    /// Current PI for this service class.
    pub pi: f64,
    /// Scheduling environment required (if any).
    pub schenv: Option<String>,
    /// Time queued (seconds since epoch).
    pub queued_time: f64,
}

/// Distribution of queued jobs by service class.
#[derive(Debug, Clone, Default)]
pub struct QueueDistribution {
    /// Count per service class.
    pub by_class: HashMap<String, u32>,
    /// Total queued.
    pub total: u32,
    /// Count with PI > 1.0 (goals at risk).
    pub goals_at_risk: u32,
}

// ─────────────────────── Initiator ───────────────────────

/// State of a managed initiator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InitiatorState {
    /// Initiator is starting up.
    Starting,
    /// Initiator is active (processing a job).
    Active,
    /// Initiator is idle (waiting for work).
    Idle,
    /// Initiator is draining (finishing current job, then stop).
    Draining,
    /// Initiator is stopped.
    Stopped,
}

/// A managed batch initiator.
#[derive(Debug, Clone)]
pub struct ManagedInitiator {
    /// Initiator ID.
    pub id: u32,
    /// Current state.
    pub state: InitiatorState,
    /// Job classes this initiator can process.
    pub job_classes: Vec<String>,
    /// Current job name (if active).
    pub current_job: Option<String>,
    /// Time of last activity (seconds since epoch).
    pub last_activity: f64,
    /// Scheduling environments available.
    pub available_schenvs: Vec<String>,
}

impl ManagedInitiator {
    /// Check if this initiator can handle a job class.
    pub fn supports_class(&self, class: &str) -> bool {
        self.job_classes.is_empty() || self.job_classes.iter().any(|c| c == class)
    }

    /// Check if this initiator provides a scheduling environment.
    pub fn has_schenv(&self, schenv: &str) -> bool {
        self.available_schenvs
            .iter()
            .any(|s| s.eq_ignore_ascii_case(schenv))
    }

    /// Seconds idle.
    pub fn idle_seconds(&self, current_time: f64) -> f64 {
        if self.state == InitiatorState::Idle {
            current_time - self.last_activity
        } else {
            0.0
        }
    }
}

// ─────────────────────── Scheduling Decision ───────────────────────

/// A scheduling decision from the WLM scheduler.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SchedulingDecision {
    /// Start a new initiator for the given job classes.
    StartInitiator { job_classes: Vec<String> },
    /// Stop an idle initiator.
    StopInitiator { initiator_id: u32 },
    /// No action needed.
    NoAction,
}

// ─────────────────────── Scheduling Environment ───────────────────────

/// A scheduling environment definition.
#[derive(Debug, Clone)]
pub struct SchedulingEnvironment {
    /// Environment name (e.g., "DB2PROD").
    pub name: String,
    /// Description.
    pub description: String,
    /// Whether this environment is currently available.
    pub available: bool,
}

impl SchedulingEnvironment {
    /// Create a new scheduling environment.
    pub fn new(name: &str, available: bool) -> Self {
        Self {
            name: name.to_uppercase(),
            description: String::new(),
            available,
        }
    }
}

// ─────────────────────── Managed Initiator Scheduler ───────────────────────

/// The WLM managed initiator scheduler.
#[derive(Debug)]
pub struct InitiatorScheduler {
    /// Managed initiators.
    initiators: Vec<ManagedInitiator>,
    /// Next initiator ID.
    next_id: u32,
    /// Idle timeout in seconds before stopping an initiator.
    idle_timeout: f64,
    /// Maximum number of initiators.
    max_initiators: u32,
    /// Scheduling environments.
    environments: HashMap<String, SchedulingEnvironment>,
}

impl InitiatorScheduler {
    /// Create a new scheduler.
    pub fn new(max_initiators: u32) -> Self {
        Self {
            initiators: Vec::new(),
            next_id: 1,
            idle_timeout: 300.0, // 5 minutes
            max_initiators,
            environments: HashMap::new(),
        }
    }

    /// Set idle timeout.
    pub fn set_idle_timeout(&mut self, seconds: f64) {
        self.idle_timeout = seconds;
    }

    /// Register a scheduling environment.
    pub fn add_environment(&mut self, env: SchedulingEnvironment) {
        self.environments.insert(env.name.clone(), env);
    }

    /// Check if a scheduling environment is available.
    pub fn is_schenv_available(&self, name: &str) -> bool {
        self.environments
            .get(&name.to_uppercase())
            .is_some_and(|e| e.available)
    }

    /// Analyze the job queue.
    pub fn analyze_queue(&self, jobs: &[QueuedJob]) -> QueueDistribution {
        let mut dist = QueueDistribution::default();
        for job in jobs {
            *dist.by_class.entry(job.service_class.clone()).or_insert(0) += 1;
            dist.total += 1;
            if job.pi > 1.0 {
                dist.goals_at_risk += 1;
            }
        }
        dist
    }

    /// Evaluate whether to start or stop initiators.
    pub fn evaluate(
        &self,
        jobs: &[QueuedJob],
        current_time: f64,
    ) -> Vec<SchedulingDecision> {
        let mut decisions = Vec::new();

        // Check for idle initiators to stop.
        for init in &self.initiators {
            if init.state == InitiatorState::Idle
                && init.idle_seconds(current_time) >= self.idle_timeout
            {
                decisions.push(SchedulingDecision::StopInitiator {
                    initiator_id: init.id,
                });
            }
        }

        // Check for jobs needing initiators.
        let active_count = self.initiators.iter().filter(|i| {
            i.state == InitiatorState::Active || i.state == InitiatorState::Starting
        }).count() as u32;

        // Find high-priority jobs that need service.
        let urgent_jobs: Vec<&QueuedJob> = jobs
            .iter()
            .filter(|j| j.pi > 1.0 && j.importance.0 <= 2)
            .collect();

        if !urgent_jobs.is_empty() && active_count < self.max_initiators {
            // Determine which job classes need initiators.
            let mut needed_classes: Vec<String> = Vec::new();
            for job in &urgent_jobs {
                // Check scheduling environment.
                if let Some(ref schenv) = job.schenv {
                    if !self.is_schenv_available(schenv) {
                        continue; // Can't start — environment not available.
                    }
                }

                // Check if an idle initiator already supports this class.
                let has_idle = self.initiators.iter().any(|i| {
                    i.state == InitiatorState::Idle && i.supports_class(&job.job_class)
                });

                if !has_idle && !needed_classes.contains(&job.job_class) {
                    needed_classes.push(job.job_class.clone());
                }
            }

            if !needed_classes.is_empty() {
                decisions.push(SchedulingDecision::StartInitiator {
                    job_classes: needed_classes,
                });
            }
        }

        if decisions.is_empty() {
            decisions.push(SchedulingDecision::NoAction);
        }

        decisions
    }

    /// Start an initiator.
    pub fn start_initiator(&mut self, job_classes: Vec<String>, schenvs: Vec<String>) -> u32 {
        let id = self.next_id;
        self.next_id += 1;

        self.initiators.push(ManagedInitiator {
            id,
            state: InitiatorState::Starting,
            job_classes,
            current_job: None,
            last_activity: 0.0,
            available_schenvs: schenvs,
        });

        id
    }

    /// Activate an initiator (assign it a job).
    pub fn activate_initiator(&mut self, id: u32, job_name: &str, current_time: f64) {
        if let Some(init) = self.initiators.iter_mut().find(|i| i.id == id) {
            init.state = InitiatorState::Active;
            init.current_job = Some(job_name.to_string());
            init.last_activity = current_time;
        }
    }

    /// Mark an initiator as idle.
    pub fn idle_initiator(&mut self, id: u32, current_time: f64) {
        if let Some(init) = self.initiators.iter_mut().find(|i| i.id == id) {
            init.state = InitiatorState::Idle;
            init.current_job = None;
            init.last_activity = current_time;
        }
    }

    /// Stop an initiator.
    pub fn stop_initiator(&mut self, id: u32) {
        if let Some(init) = self.initiators.iter_mut().find(|i| i.id == id) {
            init.state = InitiatorState::Stopped;
            init.current_job = None;
        }
    }

    /// Get active initiator count.
    pub fn active_count(&self) -> usize {
        self.initiators
            .iter()
            .filter(|i| i.state == InitiatorState::Active)
            .count()
    }

    /// Get idle initiator count.
    pub fn idle_count(&self) -> usize {
        self.initiators
            .iter()
            .filter(|i| i.state == InitiatorState::Idle)
            .count()
    }

    /// Get total initiator count (excluding stopped).
    pub fn total_count(&self) -> usize {
        self.initiators
            .iter()
            .filter(|i| i.state != InitiatorState::Stopped)
            .count()
    }

    /// Get an initiator by ID.
    pub fn get_initiator(&self, id: u32) -> Option<&ManagedInitiator> {
        self.initiators.iter().find(|i| i.id == id)
    }
}

impl Default for InitiatorScheduler {
    fn default() -> Self {
        Self::new(10)
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn make_job(
        name: &str,
        class: &str,
        svc_class: &str,
        imp: u8,
        pi: f64,
        schenv: Option<&str>,
    ) -> QueuedJob {
        QueuedJob {
            name: name.into(),
            job_class: class.into(),
            service_class: svc_class.into(),
            importance: Importance(imp),
            pi,
            schenv: schenv.map(|s| s.to_string()),
            queued_time: 0.0,
        }
    }

    // ─── WLM-104.1: Job Queue Monitoring ───

    #[test]
    fn test_queue_analysis() {
        let scheduler = InitiatorScheduler::new(10);
        let jobs = vec![
            make_job("JOB1", "A", "BATCH_HIGH", 1, 1.5, None),
            make_job("JOB2", "A", "BATCH_HIGH", 1, 0.8, None),
            make_job("JOB3", "B", "BATCH_LOW", 3, 1.2, None),
        ];

        let dist = scheduler.analyze_queue(&jobs);
        assert_eq!(dist.total, 3);
        assert_eq!(dist.goals_at_risk, 2);
        assert_eq!(*dist.by_class.get("BATCH_HIGH").unwrap(), 2);
        assert_eq!(*dist.by_class.get("BATCH_LOW").unwrap(), 1);
    }

    #[test]
    fn test_empty_queue() {
        let scheduler = InitiatorScheduler::new(10);
        let dist = scheduler.analyze_queue(&[]);
        assert_eq!(dist.total, 0);
        assert_eq!(dist.goals_at_risk, 0);
    }

    // ─── WLM-104.2: Initiator Start Decision ───

    #[test]
    fn test_start_initiator_for_urgent_jobs() {
        let scheduler = InitiatorScheduler::new(10);
        let jobs = vec![
            make_job("JOB1", "A", "HIGH", 1, 1.5, None),
        ];

        let decisions = scheduler.evaluate(&jobs, 1000.0);
        assert!(decisions.iter().any(|d| matches!(d, SchedulingDecision::StartInitiator { .. })));
    }

    #[test]
    fn test_no_start_if_goals_met() {
        let scheduler = InitiatorScheduler::new(10);
        let jobs = vec![
            make_job("JOB1", "A", "HIGH", 1, 0.5, None),
        ];

        let decisions = scheduler.evaluate(&jobs, 1000.0);
        assert!(decisions.contains(&SchedulingDecision::NoAction));
    }

    // ─── WLM-104.3: Initiator Stop Decision ───

    #[test]
    fn test_stop_idle_initiator() {
        let mut scheduler = InitiatorScheduler::new(10);
        scheduler.set_idle_timeout(300.0);

        let id = scheduler.start_initiator(vec!["A".into()], Vec::new());
        scheduler.idle_initiator(id, 100.0);

        // At time 500, initiator has been idle for 400 seconds > 300 timeout.
        let decisions = scheduler.evaluate(&[], 500.0);
        assert!(decisions.iter().any(|d| matches!(d, SchedulingDecision::StopInitiator { initiator_id } if *initiator_id == id)));
    }

    #[test]
    fn test_no_stop_if_recently_idle() {
        let mut scheduler = InitiatorScheduler::new(10);
        scheduler.set_idle_timeout(300.0);

        let id = scheduler.start_initiator(vec!["A".into()], Vec::new());
        scheduler.idle_initiator(id, 100.0);

        // At time 200, only 100 seconds idle < 300 timeout.
        let decisions = scheduler.evaluate(&[], 200.0);
        assert!(decisions.contains(&SchedulingDecision::NoAction));
    }

    // ─── WLM-104.4: SCHENV Evaluation ───

    #[test]
    fn test_schenv_available() {
        let mut scheduler = InitiatorScheduler::new(10);
        scheduler.add_environment(SchedulingEnvironment::new("DB2PROD", true));

        let jobs = vec![
            make_job("JOB1", "A", "HIGH", 1, 1.5, Some("DB2PROD")),
        ];

        let decisions = scheduler.evaluate(&jobs, 1000.0);
        assert!(decisions.iter().any(|d| matches!(d, SchedulingDecision::StartInitiator { .. })));
    }

    #[test]
    fn test_schenv_unavailable() {
        let mut scheduler = InitiatorScheduler::new(10);
        scheduler.add_environment(SchedulingEnvironment::new("DB2PROD", false));

        let jobs = vec![
            make_job("JOB1", "A", "HIGH", 1, 1.5, Some("DB2PROD")),
        ];

        let decisions = scheduler.evaluate(&jobs, 1000.0);
        // Should not start — environment unavailable.
        assert!(decisions.contains(&SchedulingDecision::NoAction));
    }

    // ─── WLM-104.5: Job-Class Affinity ───

    #[test]
    fn test_job_class_affinity() {
        let mut scheduler = InitiatorScheduler::new(10);

        let id = scheduler.start_initiator(vec!["A".into(), "B".into()], Vec::new());
        let init = scheduler.get_initiator(id).unwrap();
        assert!(init.supports_class("A"));
        assert!(init.supports_class("B"));
        assert!(!init.supports_class("C"));
    }

    #[test]
    fn test_empty_class_supports_all() {
        let mut scheduler = InitiatorScheduler::new(10);
        let id = scheduler.start_initiator(Vec::new(), Vec::new());
        let init = scheduler.get_initiator(id).unwrap();
        assert!(init.supports_class("ANY"));
    }

    // ─── WLM-104.6: Integration ───

    #[test]
    fn test_full_initiator_lifecycle() {
        let mut scheduler = InitiatorScheduler::new(5);
        scheduler.set_idle_timeout(60.0);
        scheduler.add_environment(SchedulingEnvironment::new("DB2PROD", true));

        // Start an initiator.
        let id = scheduler.start_initiator(vec!["A".into()], vec!["DB2PROD".into()]);
        assert_eq!(scheduler.total_count(), 1);

        // Activate it with a job.
        scheduler.activate_initiator(id, "MYJOB", 100.0);
        assert_eq!(scheduler.active_count(), 1);

        // Job finishes, initiator goes idle.
        scheduler.idle_initiator(id, 200.0);
        assert_eq!(scheduler.idle_count(), 1);
        assert_eq!(scheduler.active_count(), 0);

        // After timeout, it should be stopped.
        let decisions = scheduler.evaluate(&[], 300.0);
        assert!(decisions.iter().any(|d| matches!(d, SchedulingDecision::StopInitiator { .. })));

        scheduler.stop_initiator(id);
        assert_eq!(scheduler.total_count(), 0);
    }

    #[test]
    fn test_multiple_initiators() {
        let mut scheduler = InitiatorScheduler::new(10);

        let id1 = scheduler.start_initiator(vec!["A".into()], Vec::new());
        let id2 = scheduler.start_initiator(vec!["B".into()], Vec::new());

        scheduler.activate_initiator(id1, "JOB1", 100.0);
        scheduler.activate_initiator(id2, "JOB2", 100.0);
        assert_eq!(scheduler.active_count(), 2);

        scheduler.idle_initiator(id1, 200.0);
        assert_eq!(scheduler.active_count(), 1);
        assert_eq!(scheduler.idle_count(), 1);
    }
}
