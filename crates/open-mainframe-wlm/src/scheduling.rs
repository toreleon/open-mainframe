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

// ─────────────────────── Resource State ───────────────────────

/// State of a resource in a scheduling environment.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResourceState {
    /// Resource is available.
    Available,
    /// Resource is unavailable.
    Unavailable,
    /// Resource state is unknown (not yet reported).
    Unknown,
}

/// A resource required by a scheduling environment.
#[derive(Debug, Clone)]
pub struct ResourceDefinition {
    /// Resource name (e.g., "DB2", "MQ").
    pub name: String,
    /// Current state.
    pub state: ResourceState,
    /// Description.
    pub description: String,
}

impl ResourceDefinition {
    /// Create a new resource definition.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            state: ResourceState::Unknown,
            description: String::new(),
        }
    }
}

// ─────────────────────── Scheduling Environment ───────────────────────

/// A scheduling environment definition.
///
/// A SCHENV defines resource requirements for batch jobs. Jobs that specify
/// a SCHENV are only dispatched when all required resources are available.
#[derive(Debug, Clone)]
pub struct SchedulingEnvironment {
    /// Environment name (e.g., "DB2PROD").
    pub name: String,
    /// Description.
    pub description: String,
    /// Required resources and their states.
    pub resources: Vec<ResourceDefinition>,
}

impl SchedulingEnvironment {
    /// Create a new scheduling environment.
    pub fn new(name: &str, available: bool) -> Self {
        // For backward-compat: if created with available=true, start with no resource deps.
        // If false, add a synthetic unavailable resource.
        let mut env = Self {
            name: name.to_uppercase(),
            description: String::new(),
            resources: Vec::new(),
        };
        if !available {
            let mut res = ResourceDefinition::new(&env.name);
            res.state = ResourceState::Unavailable;
            env.resources.push(res);
        }
        env
    }

    /// Create a scheduling environment with specific resource dependencies.
    pub fn with_resources(name: &str, resources: Vec<ResourceDefinition>) -> Self {
        Self {
            name: name.to_uppercase(),
            description: String::new(),
            resources,
        }
    }

    /// Whether the scheduling environment is available (all resources available or unknown).
    pub fn is_available(&self) -> bool {
        if self.resources.is_empty() {
            return true;
        }
        self.resources
            .iter()
            .all(|r| r.state != ResourceState::Unavailable)
    }

    /// Set the state of a named resource.
    pub fn set_resource_state(&mut self, resource_name: &str, state: ResourceState) {
        let upper = resource_name.to_uppercase();
        if let Some(res) = self.resources.iter_mut().find(|r| r.name == upper) {
            res.state = state;
        }
    }

    /// Get unavailable resource names.
    pub fn unavailable_resources(&self) -> Vec<&str> {
        self.resources
            .iter()
            .filter(|r| r.state == ResourceState::Unavailable)
            .map(|r| r.name.as_str())
            .collect()
    }
}

// ─────────────────────── Application Environment ───────────────────────

/// State of an application environment server.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ServerState {
    /// Server is starting.
    Starting,
    /// Server is ready to accept work.
    Ready,
    /// Server is stopping.
    Stopping,
    /// Server has stopped.
    Stopped,
}

/// A server instance within an application environment.
#[derive(Debug, Clone)]
pub struct AppServer {
    /// Server instance name.
    pub name: String,
    /// Current state.
    pub state: ServerState,
    /// Start timestamp.
    pub started_at: f64,
}

/// An application environment definition.
///
/// Application environments manage server lifetimes. WLM starts servers
/// on demand when work arrives and stops them when idle.
#[derive(Debug, Clone)]
pub struct ApplicationEnvironment {
    /// Environment name.
    pub name: String,
    /// Description.
    pub description: String,
    /// Start procedure name (JCL PROC to start a server).
    pub start_procedure: String,
    /// Maximum number of server instances.
    pub start_limit: u32,
    /// Servers in this environment.
    pub servers: Vec<AppServer>,
}

impl ApplicationEnvironment {
    /// Create a new application environment.
    pub fn new(name: &str, start_procedure: &str, start_limit: u32) -> Self {
        Self {
            name: name.to_uppercase(),
            description: String::new(),
            start_procedure: start_procedure.to_string(),
            start_limit,
            servers: Vec::new(),
        }
    }

    /// Number of running servers (Starting or Ready).
    pub fn running_count(&self) -> u32 {
        self.servers
            .iter()
            .filter(|s| s.state == ServerState::Starting || s.state == ServerState::Ready)
            .count() as u32
    }

    /// Number of ready servers.
    pub fn ready_count(&self) -> u32 {
        self.servers
            .iter()
            .filter(|s| s.state == ServerState::Ready)
            .count() as u32
    }

    /// Whether more servers can be started.
    pub fn can_start_more(&self) -> bool {
        self.running_count() < self.start_limit
    }

    /// Start a new server. Returns the server name, or None if at limit.
    pub fn start_server(&mut self, timestamp: f64) -> Option<String> {
        if !self.can_start_more() {
            return None;
        }
        let instance = self.servers.len() + 1;
        let name = format!("{}S{:03}", self.name, instance);
        self.servers.push(AppServer {
            name: name.clone(),
            state: ServerState::Starting,
            started_at: timestamp,
        });
        Some(name)
    }

    /// Mark a server as ready.
    pub fn server_ready(&mut self, server_name: &str) {
        if let Some(srv) = self.servers.iter_mut().find(|s| s.name == server_name) {
            srv.state = ServerState::Ready;
        }
    }

    /// Stop a server.
    pub fn stop_server(&mut self, server_name: &str) {
        if let Some(srv) = self.servers.iter_mut().find(|s| s.name == server_name) {
            srv.state = ServerState::Stopped;
        }
    }
}

/// A request to start a server in an application environment.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ServerStartRequest {
    /// Application environment name.
    pub app_env: String,
    /// Start procedure name.
    pub start_procedure: String,
    /// Server instance name.
    pub server_name: String,
}

/// Manager for application environments.
#[derive(Debug)]
pub struct AppEnvironmentManager {
    /// Application environments by name.
    environments: HashMap<String, ApplicationEnvironment>,
}

impl AppEnvironmentManager {
    /// Create a new manager.
    pub fn new() -> Self {
        Self {
            environments: HashMap::new(),
        }
    }

    /// Define an application environment.
    pub fn define(&mut self, env: ApplicationEnvironment) {
        self.environments.insert(env.name.clone(), env);
    }

    /// Get an application environment.
    pub fn get(&self, name: &str) -> Option<&ApplicationEnvironment> {
        self.environments.get(&name.to_uppercase())
    }

    /// Get a mutable reference to an application environment.
    pub fn get_mut(&mut self, name: &str) -> Option<&mut ApplicationEnvironment> {
        self.environments.get_mut(&name.to_uppercase())
    }

    /// Evaluate whether servers need to be started for pending work.
    /// Returns a list of server start requests.
    pub fn evaluate_start_on_demand(
        &mut self,
        pending_app_envs: &[&str],
        timestamp: f64,
    ) -> Vec<ServerStartRequest> {
        let mut requests = Vec::new();
        for env_name in pending_app_envs {
            let upper = env_name.to_uppercase();
            if let Some(env) = self.environments.get_mut(&upper) {
                // Start a server if none are running or ready.
                if env.ready_count() == 0 && env.can_start_more() {
                    if let Some(server_name) = env.start_server(timestamp) {
                        requests.push(ServerStartRequest {
                            app_env: env.name.clone(),
                            start_procedure: env.start_procedure.clone(),
                            server_name,
                        });
                    }
                }
            }
        }
        requests
    }

    /// List all environment names.
    pub fn list(&self) -> Vec<&str> {
        self.environments.keys().map(|s| s.as_str()).collect()
    }

    /// Total running servers across all environments.
    pub fn total_running(&self) -> u32 {
        self.environments.values().map(|e| e.running_count()).sum()
    }
}

impl Default for AppEnvironmentManager {
    fn default() -> Self {
        Self::new()
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
            .is_some_and(|e| e.is_available())
    }

    /// Set the state of a resource in a scheduling environment.
    pub fn set_resource_state(&mut self, schenv: &str, resource: &str, state: ResourceState) {
        let upper = schenv.to_uppercase();
        if let Some(env) = self.environments.get_mut(&upper) {
            env.set_resource_state(resource, state);
        }
    }

    /// Get a scheduling environment by name.
    pub fn get_environment(&self, name: &str) -> Option<&SchedulingEnvironment> {
        self.environments.get(&name.to_uppercase())
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

    // ═══════════════════════════════════════════════════════════════
    // WLM-107: Scheduling & Application Environments
    // ═══════════════════════════════════════════════════════════════

    // ─── WLM-107.1: Scheduling Environment Definition ───

    #[test]
    fn test_schenv_with_resources() {
        let mut db2_res = ResourceDefinition::new("DB2");
        db2_res.state = ResourceState::Available;

        let env = SchedulingEnvironment::with_resources("DB2PROD", vec![db2_res]);
        assert!(env.is_available());
        assert_eq!(env.name, "DB2PROD");
    }

    #[test]
    fn test_schenv_unavailable_resource() {
        let mut db2_res = ResourceDefinition::new("DB2");
        db2_res.state = ResourceState::Unavailable;

        let env = SchedulingEnvironment::with_resources("DB2PROD", vec![db2_res]);
        assert!(!env.is_available());
        assert_eq!(env.unavailable_resources(), vec!["DB2"]);
    }

    #[test]
    fn test_schenv_multiple_resources() {
        let mut db2 = ResourceDefinition::new("DB2");
        db2.state = ResourceState::Available;
        let mut mq = ResourceDefinition::new("MQ");
        mq.state = ResourceState::Available;

        let env = SchedulingEnvironment::with_resources("FULLENV", vec![db2, mq]);
        assert!(env.is_available());
    }

    #[test]
    fn test_schenv_dispatches_when_available() {
        let mut scheduler = InitiatorScheduler::new(10);

        let mut db2_res = ResourceDefinition::new("DB2");
        db2_res.state = ResourceState::Available;
        scheduler.add_environment(SchedulingEnvironment::with_resources("DB2PROD", vec![db2_res]));

        // Job requiring DB2PROD should be dispatchable.
        let jobs = vec![make_job("JOB1", "A", "HIGH", 1, 1.5, Some("DB2PROD"))];
        let decisions = scheduler.evaluate(&jobs, 1000.0);
        assert!(decisions
            .iter()
            .any(|d| matches!(d, SchedulingDecision::StartInitiator { .. })));
    }

    // ─── WLM-107.2: Application Environment Definition ───

    #[test]
    fn test_app_env_start_limit() {
        let mut app_env = ApplicationEnvironment::new("WAS", "WASSTART", 5);
        assert_eq!(app_env.start_limit, 5);
        assert_eq!(app_env.running_count(), 0);
        assert!(app_env.can_start_more());

        // Start 3 servers.
        for _ in 0..3 {
            app_env.start_server(100.0);
        }
        assert_eq!(app_env.running_count(), 3);
        assert!(app_env.can_start_more()); // 3 < 5

        // Start 2 more.
        app_env.start_server(101.0);
        app_env.start_server(102.0);
        assert_eq!(app_env.running_count(), 5);
        assert!(!app_env.can_start_more()); // 5 == 5
    }

    #[test]
    fn test_app_env_at_limit_returns_none() {
        let mut app_env = ApplicationEnvironment::new("WAS", "WASSTART", 2);
        app_env.start_server(100.0);
        app_env.start_server(101.0);
        assert!(app_env.start_server(102.0).is_none());
    }

    #[test]
    fn test_app_env_server_lifecycle() {
        let mut app_env = ApplicationEnvironment::new("CICSENV", "CICSPROC", 3);

        let name = app_env.start_server(100.0).unwrap();
        assert_eq!(app_env.ready_count(), 0); // Still starting.

        app_env.server_ready(&name);
        assert_eq!(app_env.ready_count(), 1);

        app_env.stop_server(&name);
        assert_eq!(app_env.ready_count(), 0);
        assert_eq!(app_env.running_count(), 0);
    }

    // ─── WLM-107.3: Server Start-on-Demand ───

    #[test]
    fn test_start_on_demand_no_servers() {
        let mut mgr = AppEnvironmentManager::new();
        mgr.define(ApplicationEnvironment::new("WASENV", "WASSTART", 5));

        // Work arrives for WASENV — no servers running.
        let requests = mgr.evaluate_start_on_demand(&["WASENV"], 100.0);
        assert_eq!(requests.len(), 1);
        assert_eq!(requests[0].app_env, "WASENV");
        assert_eq!(requests[0].start_procedure, "WASSTART");
    }

    #[test]
    fn test_no_start_when_server_ready() {
        let mut mgr = AppEnvironmentManager::new();
        let mut env = ApplicationEnvironment::new("WASENV", "WASSTART", 5);
        let name = env.start_server(50.0).unwrap();
        env.server_ready(&name);
        mgr.define(env);

        // Work arrives but a server is already ready.
        let requests = mgr.evaluate_start_on_demand(&["WASENV"], 100.0);
        assert!(requests.is_empty());
    }

    #[test]
    fn test_start_on_demand_multiple_envs() {
        let mut mgr = AppEnvironmentManager::new();
        mgr.define(ApplicationEnvironment::new("ENV1", "PROC1", 5));
        mgr.define(ApplicationEnvironment::new("ENV2", "PROC2", 5));

        let requests = mgr.evaluate_start_on_demand(&["ENV1", "ENV2"], 100.0);
        assert_eq!(requests.len(), 2);
    }

    #[test]
    fn test_start_on_demand_at_limit() {
        let mut mgr = AppEnvironmentManager::new();
        let mut env = ApplicationEnvironment::new("WASENV", "WASSTART", 1);
        env.start_server(50.0); // Already at limit.
        mgr.define(env);

        let requests = mgr.evaluate_start_on_demand(&["WASENV"], 100.0);
        assert!(requests.is_empty());
    }

    // ─── WLM-107.4: Resource State Tracking ───

    #[test]
    fn test_resource_goes_unavailable() {
        let mut db2 = ResourceDefinition::new("DB2");
        db2.state = ResourceState::Available;
        let mut env = SchedulingEnvironment::with_resources("DB2PROD", vec![db2]);

        assert!(env.is_available());

        // DB2 goes down.
        env.set_resource_state("DB2", ResourceState::Unavailable);
        assert!(!env.is_available());
        assert_eq!(env.unavailable_resources(), vec!["DB2"]);
    }

    #[test]
    fn test_resource_becomes_available() {
        let mut db2 = ResourceDefinition::new("DB2");
        db2.state = ResourceState::Unavailable;
        let mut env = SchedulingEnvironment::with_resources("DB2PROD", vec![db2]);

        assert!(!env.is_available());

        env.set_resource_state("DB2", ResourceState::Available);
        assert!(env.is_available());
    }

    #[test]
    fn test_scheduler_resource_state_tracking() {
        let mut scheduler = InitiatorScheduler::new(10);

        let mut db2 = ResourceDefinition::new("DB2");
        db2.state = ResourceState::Available;
        scheduler.add_environment(SchedulingEnvironment::with_resources("DB2PROD", vec![db2]));

        assert!(scheduler.is_schenv_available("DB2PROD"));

        // Resource goes unavailable.
        scheduler.set_resource_state("DB2PROD", "DB2", ResourceState::Unavailable);
        assert!(!scheduler.is_schenv_available("DB2PROD"));

        // Jobs requiring DB2PROD should wait (no initiator started).
        let jobs = vec![make_job("JOB1", "A", "HIGH", 1, 1.5, Some("DB2PROD"))];
        let decisions = scheduler.evaluate(&jobs, 1000.0);
        assert!(decisions.contains(&SchedulingDecision::NoAction));

        // Resource comes back.
        scheduler.set_resource_state("DB2PROD", "DB2", ResourceState::Available);
        assert!(scheduler.is_schenv_available("DB2PROD"));

        let decisions = scheduler.evaluate(&jobs, 1001.0);
        assert!(decisions
            .iter()
            .any(|d| matches!(d, SchedulingDecision::StartInitiator { .. })));
    }

    #[test]
    fn test_partial_resource_failure() {
        let mut db2 = ResourceDefinition::new("DB2");
        db2.state = ResourceState::Available;
        let mut mq = ResourceDefinition::new("MQ");
        mq.state = ResourceState::Available;

        let mut env = SchedulingEnvironment::with_resources("FULLENV", vec![db2, mq]);
        assert!(env.is_available());

        // MQ goes down — environment should be unavailable.
        env.set_resource_state("MQ", ResourceState::Unavailable);
        assert!(!env.is_available());
        assert_eq!(env.unavailable_resources(), vec!["MQ"]);
    }

    // ─── WLM-107.5: Environment Integration Tests ───

    #[test]
    fn test_full_environment_workflow() {
        // Set up scheduler with scheduling environments.
        let mut scheduler = InitiatorScheduler::new(10);
        let mut db2 = ResourceDefinition::new("DB2");
        db2.state = ResourceState::Available;
        scheduler.add_environment(SchedulingEnvironment::with_resources("DB2PROD", vec![db2]));

        // Set up application environment manager.
        let mut app_mgr = AppEnvironmentManager::new();
        app_mgr.define(ApplicationEnvironment::new("WASENV", "WASSTART", 3));

        // Job arrives requiring DB2PROD.
        let jobs = vec![make_job("JOB1", "A", "HIGH", 1, 1.5, Some("DB2PROD"))];
        let decisions = scheduler.evaluate(&jobs, 1000.0);
        assert!(decisions
            .iter()
            .any(|d| matches!(d, SchedulingDecision::StartInitiator { .. })));

        // Work arrives for WASENV — trigger start-on-demand.
        let start_reqs = app_mgr.evaluate_start_on_demand(&["WASENV"], 1000.0);
        assert_eq!(start_reqs.len(), 1);
        assert_eq!(start_reqs[0].start_procedure, "WASSTART");

        // Server becomes ready.
        let srv_name = &start_reqs[0].server_name;
        let env = app_mgr.get_mut("WASENV").unwrap();
        env.server_ready(srv_name);
        assert_eq!(env.ready_count(), 1);

        // Now DB2 goes down.
        scheduler.set_resource_state("DB2PROD", "DB2", ResourceState::Unavailable);
        let decisions = scheduler.evaluate(&jobs, 1001.0);
        assert!(decisions.contains(&SchedulingDecision::NoAction));

        // DB2 comes back.
        scheduler.set_resource_state("DB2PROD", "DB2", ResourceState::Available);
        let decisions = scheduler.evaluate(&jobs, 1002.0);
        assert!(decisions
            .iter()
            .any(|d| matches!(d, SchedulingDecision::StartInitiator { .. })));
    }

    #[test]
    fn test_app_env_manager_list_and_totals() {
        let mut mgr = AppEnvironmentManager::new();

        let mut env1 = ApplicationEnvironment::new("ENV1", "P1", 5);
        env1.start_server(100.0);
        env1.start_server(101.0);

        let mut env2 = ApplicationEnvironment::new("ENV2", "P2", 3);
        env2.start_server(100.0);

        mgr.define(env1);
        mgr.define(env2);

        assert_eq!(mgr.list().len(), 2);
        assert_eq!(mgr.total_running(), 3);
    }

    #[test]
    fn test_unknown_resource_state_allows_dispatch() {
        // Unknown state should not block — only Unavailable blocks.
        let res = ResourceDefinition::new("NEWDB");
        assert_eq!(res.state, ResourceState::Unknown);

        let env = SchedulingEnvironment::with_resources("NEWENV", vec![res]);
        assert!(env.is_available());
    }
}
