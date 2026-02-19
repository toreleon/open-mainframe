//! LE Program Model — process/enclave/thread hierarchy and lifecycle services.
//!
//! Implements the z/OS Language Environment execution model:
//! - **Process** → contains one or more enclaves
//! - **Enclave** → execution context for an HLL program (main + call chain)
//! - **Thread** → unit of execution within an enclave
//!
//! Provides CEE3ABD, CEE3GRC, CEE3INF, CEEGPID callable service equivalents.

use std::collections::BTreeMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

/// Unique ID generator for enclaves and threads.
static NEXT_ID: AtomicU64 = AtomicU64::new(1);

fn next_id() -> u64 {
    NEXT_ID.fetch_add(1, Ordering::Relaxed)
}

/// System type returned by CEE3INF.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SystemType {
    /// z/OS operating system.
    ZOS,
}

/// Environment type returned by CEE3INF.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EnvironmentType {
    /// Batch job execution.
    Batch,
    /// TSO/E interactive session.
    Tso,
    /// CICS transaction.
    Cics,
    /// z/OS UNIX System Services.
    Uss,
}

/// Termination reason for an enclave.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TerminationReason {
    /// Normal termination (STOP RUN or return from main).
    Normal,
    /// Abend with a user code (U-code).
    AbendUser(u32),
    /// Abend with a system code (S-code).
    AbendSystem(u32),
}

/// Exit procedure registered via CEERTX.
#[derive(Debug, Clone)]
pub struct ExitProcedure {
    /// Unique registration ID.
    pub id: u64,
    /// User token passed at registration.
    pub token: u64,
    /// Name of the exit routine (for diagnostics).
    pub name: String,
}

/// State of an enclave.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EnclaveState {
    /// Enclave is being initialized.
    Initializing,
    /// Enclave is actively running.
    Running,
    /// Enclave is terminating.
    Terminating,
    /// Enclave has terminated.
    Terminated,
}

/// State of a thread within an enclave.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThreadState {
    /// Thread is running.
    Running,
    /// Thread has terminated.
    Terminated,
}

/// A thread within an LE enclave.
#[derive(Debug, Clone)]
pub struct LeThread {
    /// Unique thread ID.
    pub id: u64,
    /// Thread state.
    pub state: ThreadState,
    /// Currently executing routine name.
    pub current_routine: String,
    /// Call depth (stack frame count).
    pub call_depth: u32,
}

/// An LE enclave — execution context for an HLL program.
#[derive(Debug, Clone)]
pub struct Enclave {
    /// Unique enclave ID.
    pub id: u64,
    /// Enclave state.
    pub state: EnclaveState,
    /// Return code (set by CEE3SRC or on termination).
    pub return_code: i32,
    /// Main routine name.
    pub main_routine: String,
    /// Threads within this enclave: thread_id → thread.
    pub threads: BTreeMap<u64, LeThread>,
    /// ID of the initial (main) thread.
    pub main_thread_id: u64,
    /// Registered exit procedures (CEERTX).
    pub exit_procedures: Vec<ExitProcedure>,
    /// Termination reason (set when terminated).
    pub termination_reason: Option<TerminationReason>,
}

impl Enclave {
    /// Create a new enclave with a main routine.
    fn new(main_routine: String) -> Self {
        let id = next_id();
        let thread_id = next_id();
        let main_thread = LeThread {
            id: thread_id,
            state: ThreadState::Running,
            current_routine: main_routine.clone(),
            call_depth: 1,
        };
        let mut threads = BTreeMap::new();
        threads.insert(thread_id, main_thread);

        Self {
            id,
            state: EnclaveState::Running,
            return_code: 0,
            main_routine,
            threads,
            main_thread_id: thread_id,
            exit_procedures: Vec::new(),
            termination_reason: None,
        }
    }
}

/// An LE process — top-level container holding one or more enclaves.
#[derive(Debug)]
pub struct LeProcess {
    /// Active enclaves: enclave_id → enclave.
    enclaves: BTreeMap<u64, Enclave>,
    /// Current (active) enclave ID.
    current_enclave_id: Option<u64>,
    /// System type.
    system_type: SystemType,
    /// Environment type.
    environment_type: EnvironmentType,
    /// LE product version string.
    product_version: String,
    /// Platform identifier.
    platform: String,
    /// Exit procedure callbacks (for testing — stores procedure IDs that were driven).
    driven_exits: Arc<Mutex<Vec<u64>>>,
}

impl LeProcess {
    /// Create a new LE process.
    pub fn new(env_type: EnvironmentType) -> Self {
        Self {
            enclaves: BTreeMap::new(),
            current_enclave_id: None,
            system_type: SystemType::ZOS,
            environment_type: env_type,
            product_version: "V2R5".to_string(),
            platform: "z/OS".to_string(),
            driven_exits: Arc::new(Mutex::new(Vec::new())),
        }
    }

    /// Create a new enclave and set it as the current one.
    ///
    /// Equivalent to LE initialization when `EXEC PGM=MYPROG` runs.
    pub fn create_enclave(&mut self, main_routine: &str) -> u64 {
        let enclave = Enclave::new(main_routine.to_string());
        let id = enclave.id;
        self.enclaves.insert(id, enclave);
        self.current_enclave_id = Some(id);
        id
    }

    /// Get the current enclave reference.
    pub fn current_enclave(&self) -> Option<&Enclave> {
        self.current_enclave_id
            .and_then(|id| self.enclaves.get(&id))
    }

    /// Get a mutable reference to the current enclave.
    pub fn current_enclave_mut(&mut self) -> Option<&mut Enclave> {
        self.current_enclave_id
            .and_then(|id| self.enclaves.get_mut(&id))
    }

    // ─────── CEE3ABD — Abend enclave ───────

    /// CEE3ABD — terminate the current enclave with an abend.
    ///
    /// Drives all registered exit procedures, then terminates the enclave.
    /// Returns the list of exit procedure IDs that were driven.
    pub fn cee3abd(&mut self, abend_code: u32) -> Vec<u64> {
        let Some(enclave) = self.current_enclave_mut() else {
            return Vec::new();
        };

        enclave.state = EnclaveState::Terminating;
        enclave.termination_reason = Some(TerminationReason::AbendUser(abend_code));

        // Drive exit procedures in reverse registration order (LIFO).
        let exit_ids: Vec<u64> = enclave
            .exit_procedures
            .iter()
            .rev()
            .map(|e| e.id)
            .collect();

        // Record driven exits.
        let mut driven = self.driven_exits.lock().unwrap();
        driven.extend(&exit_ids);
        drop(driven);

        // Terminate all threads.
        let enclave = self.current_enclave_mut().unwrap();
        for thread in enclave.threads.values_mut() {
            thread.state = ThreadState::Terminated;
        }
        enclave.state = EnclaveState::Terminated;

        exit_ids
    }

    // ─────── CEE3GRC — Get return code ───────

    /// CEE3GRC — get the current enclave's return code.
    pub fn cee3grc(&self) -> Option<i32> {
        self.current_enclave().map(|e| e.return_code)
    }

    /// Set the current enclave's return code.
    pub fn set_return_code(&mut self, rc: i32) {
        if let Some(enclave) = self.current_enclave_mut() {
            enclave.return_code = rc;
        }
    }

    // ─────── CEE3INF — System/environment information ───────

    /// CEE3INF — get system type, environment type, and runtime info.
    pub fn cee3inf(&self) -> (SystemType, EnvironmentType) {
        (self.system_type, self.environment_type)
    }

    // ─────── CEEGPID — Product/platform identification ───────

    /// CEEGPID — get LE product version and platform identifier.
    pub fn ceegpid(&self) -> (&str, &str) {
        (&self.product_version, &self.platform)
    }

    // ─────── Thread management ───────

    /// Create a new thread in the current enclave.
    ///
    /// Returns the new thread ID.
    pub fn create_thread(&mut self, routine_name: &str) -> Option<u64> {
        let enclave = self.current_enclave_mut()?;
        if enclave.state != EnclaveState::Running {
            return None;
        }

        let thread = LeThread {
            id: next_id(),
            state: ThreadState::Running,
            current_routine: routine_name.to_string(),
            call_depth: 1,
        };
        let id = thread.id;
        enclave.threads.insert(id, thread);
        Some(id)
    }

    /// Terminate a thread in the current enclave.
    pub fn terminate_thread(&mut self, thread_id: u64) -> bool {
        let Some(enclave) = self.current_enclave_mut() else {
            return false;
        };
        if let Some(thread) = enclave.threads.get_mut(&thread_id) {
            thread.state = ThreadState::Terminated;
            true
        } else {
            false
        }
    }

    /// Get a thread's current routine name (CEE3GRN equivalent).
    pub fn cee3grn(&self, thread_id: u64) -> Option<&str> {
        let enclave = self.current_enclave()?;
        enclave
            .threads
            .get(&thread_id)
            .map(|t| t.current_routine.as_str())
    }

    // ─────── Exit procedures (CEERTX/CEEUTX) ───────

    /// CEERTX — register an enclave termination exit procedure.
    ///
    /// Returns the registration ID.
    pub fn ceertx(&mut self, name: &str, token: u64) -> Option<u64> {
        let enclave = self.current_enclave_mut()?;
        let exit = ExitProcedure {
            id: next_id(),
            token,
            name: name.to_string(),
        };
        let id = exit.id;
        enclave.exit_procedures.push(exit);
        Some(id)
    }

    /// CEEUTX — unregister an exit procedure by its registration ID.
    pub fn ceeutx(&mut self, exit_id: u64) -> bool {
        let Some(enclave) = self.current_enclave_mut() else {
            return false;
        };
        let len = enclave.exit_procedures.len();
        enclave.exit_procedures.retain(|e| e.id != exit_id);
        enclave.exit_procedures.len() < len
    }

    /// Get the list of exit procedure IDs that were driven during termination.
    pub fn driven_exits(&self) -> Vec<u64> {
        self.driven_exits.lock().unwrap().clone()
    }

    // ─────── Normal termination ───────

    /// Terminate the current enclave normally.
    pub fn terminate_enclave(&mut self, return_code: i32) -> Vec<u64> {
        let Some(enclave) = self.current_enclave_mut() else {
            return Vec::new();
        };

        enclave.state = EnclaveState::Terminating;
        enclave.return_code = return_code;
        enclave.termination_reason = Some(TerminationReason::Normal);

        // Drive exit procedures.
        let exit_ids: Vec<u64> = enclave
            .exit_procedures
            .iter()
            .rev()
            .map(|e| e.id)
            .collect();

        let mut driven = self.driven_exits.lock().unwrap();
        driven.extend(&exit_ids);
        drop(driven);

        let enclave = self.current_enclave_mut().unwrap();
        for thread in enclave.threads.values_mut() {
            thread.state = ThreadState::Terminated;
        }
        enclave.state = EnclaveState::Terminated;

        exit_ids
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ─────── Story LE100.1: Enclave Lifecycle ───────

    #[test]
    fn test_create_enclave_with_main_routine() {
        let mut proc = LeProcess::new(EnvironmentType::Batch);
        let eid = proc.create_enclave("MYPROG");

        let enclave = proc.current_enclave().unwrap();
        assert_eq!(enclave.id, eid);
        assert_eq!(enclave.main_routine, "MYPROG");
        assert_eq!(enclave.state, EnclaveState::Running);
        assert_eq!(enclave.threads.len(), 1);
    }

    #[test]
    fn test_cee3abd_abend_terminates_enclave() {
        let mut proc = LeProcess::new(EnvironmentType::Batch);
        proc.create_enclave("MYPROG");

        let driven = proc.cee3abd(4000);

        let enclave = proc.current_enclave().unwrap();
        assert_eq!(enclave.state, EnclaveState::Terminated);
        assert_eq!(
            enclave.termination_reason,
            Some(TerminationReason::AbendUser(4000))
        );
        assert!(driven.is_empty()); // no exit procs registered
    }

    #[test]
    fn test_cee3abd_drives_exit_procedures() {
        let mut proc = LeProcess::new(EnvironmentType::Batch);
        proc.create_enclave("MYPROG");
        let exit1 = proc.ceertx("EXIT1", 100).unwrap();
        let exit2 = proc.ceertx("EXIT2", 200).unwrap();

        let driven = proc.cee3abd(4000);

        // Should be driven in reverse order (LIFO).
        assert_eq!(driven, vec![exit2, exit1]);
    }

    #[test]
    fn test_cee3grc_return_code() {
        let mut proc = LeProcess::new(EnvironmentType::Batch);
        proc.create_enclave("MYPROG");

        assert_eq!(proc.cee3grc(), Some(0));

        proc.set_return_code(16);
        assert_eq!(proc.cee3grc(), Some(16));
    }

    // ─────── Story LE100.2: Thread Management and Program Information ───────

    #[test]
    fn test_create_thread_in_enclave() {
        let mut proc = LeProcess::new(EnvironmentType::Batch);
        proc.create_enclave("MYPROG");

        let tid = proc.create_thread("WORKER1").unwrap();

        let enclave = proc.current_enclave().unwrap();
        assert_eq!(enclave.threads.len(), 2); // main + worker
        let thread = enclave.threads.get(&tid).unwrap();
        assert_eq!(thread.current_routine, "WORKER1");
        assert_eq!(thread.state, ThreadState::Running);
    }

    #[test]
    fn test_terminate_thread() {
        let mut proc = LeProcess::new(EnvironmentType::Batch);
        proc.create_enclave("MYPROG");
        let tid = proc.create_thread("WORKER1").unwrap();

        assert!(proc.terminate_thread(tid));

        let enclave = proc.current_enclave().unwrap();
        assert_eq!(
            enclave.threads.get(&tid).unwrap().state,
            ThreadState::Terminated
        );
    }

    #[test]
    fn test_cee3inf_returns_system_info() {
        let proc = LeProcess::new(EnvironmentType::Tso);
        let (sys, env) = proc.cee3inf();
        assert_eq!(sys, SystemType::ZOS);
        assert_eq!(env, EnvironmentType::Tso);
    }

    #[test]
    fn test_ceegpid_returns_version() {
        let proc = LeProcess::new(EnvironmentType::Batch);
        let (version, platform) = proc.ceegpid();
        assert_eq!(version, "V2R5");
        assert_eq!(platform, "z/OS");
    }

    #[test]
    fn test_cee3grn_current_routine() {
        let mut proc = LeProcess::new(EnvironmentType::Batch);
        proc.create_enclave("MYPROG");

        let main_tid = proc.current_enclave().unwrap().main_thread_id;
        assert_eq!(proc.cee3grn(main_tid), Some("MYPROG"));
    }

    // ─────── Exit procedures (CEERTX/CEEUTX) ───────

    #[test]
    fn test_ceertx_registers_exit() {
        let mut proc = LeProcess::new(EnvironmentType::Batch);
        proc.create_enclave("MYPROG");

        let eid = proc.ceertx("CLEANUP", 42).unwrap();
        assert!(eid > 0);

        let enclave = proc.current_enclave().unwrap();
        assert_eq!(enclave.exit_procedures.len(), 1);
        assert_eq!(enclave.exit_procedures[0].name, "CLEANUP");
        assert_eq!(enclave.exit_procedures[0].token, 42);
    }

    #[test]
    fn test_ceeutx_unregisters_exit() {
        let mut proc = LeProcess::new(EnvironmentType::Batch);
        proc.create_enclave("MYPROG");

        let eid = proc.ceertx("CLEANUP", 42).unwrap();
        assert!(proc.ceeutx(eid));

        let enclave = proc.current_enclave().unwrap();
        assert!(enclave.exit_procedures.is_empty());
    }

    #[test]
    fn test_normal_termination_drives_exits() {
        let mut proc = LeProcess::new(EnvironmentType::Batch);
        proc.create_enclave("MYPROG");
        let exit1 = proc.ceertx("EXIT1", 0).unwrap();

        let driven = proc.terminate_enclave(0);
        assert_eq!(driven, vec![exit1]);

        let enclave = proc.current_enclave().unwrap();
        assert_eq!(enclave.state, EnclaveState::Terminated);
        assert_eq!(
            enclave.termination_reason,
            Some(TerminationReason::Normal)
        );
    }
}
