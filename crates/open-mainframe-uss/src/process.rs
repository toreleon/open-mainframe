//! POSIX Process Model (USS-100).
//!
//! Provides the UNIX process abstraction for z/OS USS including:
//! - Process table and PID management
//! - fork() process duplication
//! - exec() family for program replacement
//! - spawn() combined fork+exec
//! - wait/waitpid for child synchronization
//! - Process groups and sessions
//! - MVS-to-UNIX dubbing

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors for process operations.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, miette::Diagnostic)]
pub enum ProcessError {
    /// Process table is full (EAGAIN).
    #[error("process table full (EAGAIN): limit {limit} reached")]
    TableFull { limit: u32 },

    /// No such process (ESRCH).
    #[error("no such process: pid {pid}")]
    NoSuchProcess { pid: u32 },

    /// Permission denied (EPERM).
    #[error("permission denied (EPERM)")]
    PermissionDenied,

    /// Executable not found (ENOENT).
    #[error("executable not found: {path}")]
    ExecutableNotFound { path: String },

    /// No child processes (ECHILD).
    #[error("no child processes (ECHILD)")]
    NoChildProcesses,

    /// Process is not a group leader (for setsid).
    #[error("process {pid} is already a group leader, cannot create session")]
    AlreadyGroupLeader { pid: u32 },

    /// No OMVS segment for dubbing.
    #[error("no OMVS segment for task — cannot dub as UNIX process")]
    NoOmvsSegment,
}

// ---------------------------------------------------------------------------
//  Process State
// ---------------------------------------------------------------------------

/// State of a UNIX process.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessState {
    /// Process is executing.
    Running,
    /// Process is stopped by a signal.
    Stopped,
    /// Process has exited but parent hasn't waited.
    Zombie,
    /// Process is sleeping (waiting for I/O or event).
    Sleeping,
}

impl std::fmt::Display for ProcessState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Running => write!(f, "Running"),
            Self::Stopped => write!(f, "Stopped"),
            Self::Zombie => write!(f, "Zombie"),
            Self::Sleeping => write!(f, "Sleeping"),
        }
    }
}

// ---------------------------------------------------------------------------
//  File Descriptor
// ---------------------------------------------------------------------------

/// A UNIX file descriptor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FileDescriptor {
    /// Regular file.
    Regular {
        inode: u64,
        offset: u64,
        flags: OpenFlags,
    },
    /// Pipe endpoint.
    Pipe { pipe_id: u32, read_end: bool },
    /// Socket.
    Socket { socket_id: u32 },
    /// Directory stream.
    Directory { inode: u64, position: usize },
}

/// Flags for open().
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpenFlags {
    pub read: bool,
    pub write: bool,
    pub append: bool,
    pub create: bool,
    pub truncate: bool,
    pub exclusive: bool,
}

impl OpenFlags {
    /// Read-only flags.
    pub fn read_only() -> Self {
        Self {
            read: true,
            write: false,
            append: false,
            create: false,
            truncate: false,
            exclusive: false,
        }
    }

    /// Read-write flags.
    pub fn read_write() -> Self {
        Self {
            read: true,
            write: true,
            append: false,
            create: false,
            truncate: false,
            exclusive: false,
        }
    }
}

impl Default for OpenFlags {
    fn default() -> Self {
        Self::read_only()
    }
}

// ---------------------------------------------------------------------------
//  UNIX Process
// ---------------------------------------------------------------------------

/// A UNIX process representation.
#[derive(Debug, Clone)]
pub struct UnixProcess {
    /// Process ID.
    pub pid: u32,
    /// Parent process ID.
    pub ppid: u32,
    /// Process group ID.
    pub pgid: u32,
    /// Session ID.
    pub sid: u32,
    /// Real user ID.
    pub uid: u32,
    /// Real group ID.
    pub gid: u32,
    /// Effective user ID.
    pub euid: u32,
    /// Effective group ID.
    pub egid: u32,
    /// Process state.
    pub state: ProcessState,
    /// Blocked signal mask (as bitmask).
    pub signal_mask: u64,
    /// Open file descriptors (indexed by fd number).
    pub file_descriptors: Vec<Option<FileDescriptor>>,
    /// Current working directory.
    pub cwd: String,
    /// File creation mask.
    pub umask: u16,
    /// Exit status (set when process exits).
    pub exit_status: Option<i32>,
    /// Program path.
    pub program: String,
    /// Whether this process was dubbed from MVS.
    pub dubbed: bool,
}

impl UnixProcess {
    /// Create a new process with the given PID and parent PID.
    pub fn new(pid: u32, ppid: u32, uid: u32, gid: u32) -> Self {
        // Start with stdin(0), stdout(1), stderr(2) as None (to be inherited or opened).
        let file_descriptors = vec![None; 3];
        Self {
            pid,
            ppid,
            pgid: pid,
            sid: pid,
            uid,
            gid,
            euid: uid,
            egid: gid,
            state: ProcessState::Running,
            signal_mask: 0,
            file_descriptors,
            cwd: "/".to_string(),
            umask: 0o022,
            exit_status: None,
            program: String::new(),
            dubbed: false,
        }
    }
}

// ---------------------------------------------------------------------------
//  Wait Status
// ---------------------------------------------------------------------------

/// Wait result returned by waitpid.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WaitStatus {
    /// PID of the child.
    pub pid: u32,
    /// Raw status value.
    pub status: i32,
    /// How the child changed state.
    pub kind: WaitStatusKind,
}

/// Kind of wait status.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WaitStatusKind {
    /// Child exited normally.
    Exited { code: i32 },
    /// Child was killed by a signal.
    Signaled { signal: u32 },
    /// Child was stopped by a signal.
    Stopped { signal: u32 },
    /// Child was continued.
    Continued,
}

impl WaitStatus {
    /// WIFEXITED: true if the child terminated normally.
    pub fn if_exited(&self) -> bool {
        matches!(self.kind, WaitStatusKind::Exited { .. })
    }

    /// WEXITSTATUS: exit code (valid only if if_exited() is true).
    pub fn exit_status(&self) -> i32 {
        match self.kind {
            WaitStatusKind::Exited { code } => code,
            _ => -1,
        }
    }

    /// WIFSTOPPED: true if the child is stopped.
    pub fn if_stopped(&self) -> bool {
        matches!(self.kind, WaitStatusKind::Stopped { .. })
    }

    /// WIFSIGNALED: true if the child was killed by a signal.
    pub fn if_signaled(&self) -> bool {
        matches!(self.kind, WaitStatusKind::Signaled { .. })
    }
}

// ---------------------------------------------------------------------------
//  Wait Flags
// ---------------------------------------------------------------------------

/// Flags for waitpid().
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WaitFlags {
    /// WNOHANG: return immediately if no child has exited.
    pub no_hang: bool,
    /// WUNTRACED: also report stopped children.
    pub untraced: bool,
}

impl WaitFlags {
    /// Default flags (block until child exits).
    pub fn blocking() -> Self {
        Self {
            no_hang: false,
            untraced: false,
        }
    }
}

impl Default for WaitFlags {
    fn default() -> Self {
        Self::blocking()
    }
}

// ---------------------------------------------------------------------------
//  Spawn Attributes
// ---------------------------------------------------------------------------

/// Options for spawn().
#[derive(Debug, Clone)]
pub struct SpawnAttributes {
    /// Program path.
    pub program: String,
    /// Arguments.
    pub args: Vec<String>,
    /// Environment variables.
    pub env: HashMap<String, String>,
    /// Inherit file descriptors from parent.
    pub inherit_fds: bool,
    /// BPX_SHAREAS=YES: run child as thread in parent address space.
    pub share_address_space: bool,
}

// ---------------------------------------------------------------------------
//  Fork Result
// ---------------------------------------------------------------------------

/// Result of fork().
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ForkResult {
    /// In the parent — contains child PID.
    Parent { child_pid: u32 },
    /// In the child — pid is 0.
    Child,
}

// ---------------------------------------------------------------------------
//  OMVS Segment (for dubbing)
// ---------------------------------------------------------------------------

/// RACF OMVS segment for an MVS user — used for dubbing.
#[derive(Debug, Clone)]
pub struct OmvsSegment {
    /// UNIX user ID.
    pub uid: u32,
    /// UNIX group ID.
    pub gid: u32,
    /// Home directory.
    pub home: String,
    /// Login shell program.
    pub program: String,
}

// ---------------------------------------------------------------------------
//  Process Manager
// ---------------------------------------------------------------------------

/// Manages the UNIX process table.
#[derive(Debug)]
pub struct ProcessManager {
    /// PID -> process mapping.
    processes: HashMap<u32, UnixProcess>,
    /// Next PID to assign.
    next_pid: u32,
    /// Maximum processes system-wide (MAXPROCSYS).
    max_procs_sys: u32,
    /// Maximum processes per user (MAXPROCUSER).
    max_procs_user: u32,
}

impl ProcessManager {
    /// Create a new process manager with the given limits.
    pub fn new(max_procs_sys: u32, max_procs_user: u32) -> Self {
        Self {
            processes: HashMap::new(),
            next_pid: 1,
            max_procs_sys,
            max_procs_user,
        }
    }

    /// Return the number of active processes.
    pub fn process_count(&self) -> usize {
        self.processes.len()
    }

    /// Look up a process by PID.
    pub fn get_process(&self, pid: u32) -> Option<&UnixProcess> {
        self.processes.get(&pid)
    }

    /// Look up a process mutably by PID.
    pub fn get_process_mut(&mut self, pid: u32) -> Option<&mut UnixProcess> {
        self.processes.get_mut(&pid)
    }

    /// Count processes owned by a specific UID.
    fn count_user_processes(&self, uid: u32) -> u32 {
        self.processes
            .values()
            .filter(|p| p.uid == uid)
            .count() as u32
    }

    /// Allocate the next PID.
    fn allocate_pid(&mut self) -> u32 {
        let pid = self.next_pid;
        self.next_pid += 1;
        pid
    }

    /// Create the init process (PID 1).
    pub fn create_init_process(&mut self) -> Result<u32, ProcessError> {
        let pid = self.allocate_pid();
        let mut proc = UnixProcess::new(pid, 0, 0, 0);
        proc.program = "/sbin/init".to_string();
        self.processes.insert(pid, proc);
        Ok(pid)
    }

    /// fork() — duplicate a process. Returns the fork result.
    pub fn fork(&mut self, parent_pid: u32) -> Result<ForkResult, ProcessError> {
        // Check system limit.
        if self.processes.len() as u32 >= self.max_procs_sys {
            return Err(ProcessError::TableFull {
                limit: self.max_procs_sys,
            });
        }

        let parent = self
            .processes
            .get(&parent_pid)
            .ok_or(ProcessError::NoSuchProcess { pid: parent_pid })?
            .clone();

        // Check per-user limit.
        if self.count_user_processes(parent.uid) >= self.max_procs_user {
            return Err(ProcessError::TableFull {
                limit: self.max_procs_user,
            });
        }

        let child_pid = self.allocate_pid();

        let mut child = parent.clone();
        child.pid = child_pid;
        child.ppid = parent_pid;
        child.pgid = parent.pgid;
        child.sid = parent.sid;
        child.state = ProcessState::Running;
        child.exit_status = None;

        self.processes.insert(child_pid, child);
        Ok(ForkResult::Parent { child_pid })
    }

    /// exec() — replace the process image with a new program.
    pub fn exec(
        &mut self,
        pid: u32,
        program: &str,
        _args: &[String],
    ) -> Result<(), ProcessError> {
        if program.is_empty() {
            return Err(ProcessError::ExecutableNotFound {
                path: program.to_string(),
            });
        }

        let proc = self
            .processes
            .get_mut(&pid)
            .ok_or(ProcessError::NoSuchProcess { pid })?;

        // Replace program image.
        proc.program = program.to_string();
        // Reset signal handlers to default (except ignored ones).
        proc.signal_mask = 0;
        // Close close-on-exec file descriptors (simplified: just keep them).
        Ok(())
    }

    /// spawn() — combined fork+exec.
    pub fn spawn(
        &mut self,
        parent_pid: u32,
        attrs: &SpawnAttributes,
    ) -> Result<u32, ProcessError> {
        if attrs.program.is_empty() {
            return Err(ProcessError::ExecutableNotFound {
                path: attrs.program.clone(),
            });
        }

        let fork_result = self.fork(parent_pid)?;
        let child_pid = match fork_result {
            ForkResult::Parent { child_pid } => child_pid,
            ForkResult::Child => unreachable!(),
        };

        let child = self
            .processes
            .get_mut(&child_pid)
            .ok_or(ProcessError::NoSuchProcess { pid: child_pid })?;

        child.program = attrs.program.clone();

        if !attrs.inherit_fds {
            child.file_descriptors = vec![None; 3];
        }

        Ok(child_pid)
    }

    /// Exit a process with the given status code.
    pub fn exit_process(&mut self, pid: u32, status: i32) -> Result<(), ProcessError> {
        let proc = self
            .processes
            .get_mut(&pid)
            .ok_or(ProcessError::NoSuchProcess { pid })?;
        proc.state = ProcessState::Zombie;
        proc.exit_status = Some(status);
        proc.file_descriptors.clear();
        Ok(())
    }

    /// waitpid() — wait for a child process state change.
    pub fn waitpid(
        &mut self,
        parent_pid: u32,
        target_pid: Option<u32>,
        flags: WaitFlags,
    ) -> Result<Option<WaitStatus>, ProcessError> {
        // Verify parent exists.
        if !self.processes.contains_key(&parent_pid) {
            return Err(ProcessError::NoSuchProcess { pid: parent_pid });
        }

        // Find matching children.
        let child_pids: Vec<u32> = self
            .processes
            .values()
            .filter(|p| {
                p.ppid == parent_pid
                    && match target_pid {
                        Some(tid) => p.pid == tid,
                        None => true,
                    }
            })
            .map(|p| p.pid)
            .collect();

        if child_pids.is_empty() {
            return Err(ProcessError::NoChildProcesses);
        }

        // Look for zombie/stopped children.
        for &cpid in &child_pids {
            if let Some(child) = self.processes.get(&cpid) {
                match child.state {
                    ProcessState::Zombie => {
                        let status = child.exit_status.unwrap_or(0);
                        let wait_status = WaitStatus {
                            pid: cpid,
                            status,
                            kind: WaitStatusKind::Exited { code: status },
                        };
                        // Reap the zombie.
                        self.processes.remove(&cpid);
                        return Ok(Some(wait_status));
                    }
                    ProcessState::Stopped if flags.untraced => {
                        let wait_status = WaitStatus {
                            pid: cpid,
                            status: 0,
                            kind: WaitStatusKind::Stopped { signal: 19 }, // SIGSTOP
                        };
                        return Ok(Some(wait_status));
                    }
                    _ => {}
                }
            }
        }

        // WNOHANG: return None if nothing ready.
        if flags.no_hang {
            return Ok(None);
        }

        // In a real implementation this would block; here we return None.
        Ok(None)
    }

    /// setpgid() — set process group ID.
    pub fn setpgid(&mut self, pid: u32, pgid: u32) -> Result<(), ProcessError> {
        let target_pid = if pid == 0 {
            // Find self — caller convention. We just return error for now.
            return Err(ProcessError::NoSuchProcess { pid: 0 });
        } else {
            pid
        };

        let target_pgid = if pgid == 0 { target_pid } else { pgid };

        let proc = self
            .processes
            .get_mut(&target_pid)
            .ok_or(ProcessError::NoSuchProcess { pid: target_pid })?;
        proc.pgid = target_pgid;
        Ok(())
    }

    /// setsid() — create a new session.
    pub fn setsid(&mut self, pid: u32) -> Result<u32, ProcessError> {
        // Check that this process is not already a process group leader.
        let proc = self
            .processes
            .get(&pid)
            .ok_or(ProcessError::NoSuchProcess { pid })?;

        if proc.pgid == pid {
            // Check if any other process shares this group.
            let group_members: Vec<u32> = self
                .processes
                .values()
                .filter(|p| p.pgid == pid && p.pid != pid)
                .map(|p| p.pid)
                .collect();
            if group_members.is_empty() {
                // It's the only member, allow setsid (common case for fork'd children).
            } else {
                return Err(ProcessError::AlreadyGroupLeader { pid });
            }
        }

        let proc = self
            .processes
            .get_mut(&pid)
            .ok_or(ProcessError::NoSuchProcess { pid })?;
        proc.sid = pid;
        proc.pgid = pid;
        Ok(pid)
    }

    /// Get the process group ID for a process.
    pub fn getpgid(&self, pid: u32) -> Result<u32, ProcessError> {
        let proc = self
            .processes
            .get(&pid)
            .ok_or(ProcessError::NoSuchProcess { pid })?;
        Ok(proc.pgid)
    }

    /// Get the session ID for a process.
    pub fn getsid(&self, pid: u32) -> Result<u32, ProcessError> {
        let proc = self
            .processes
            .get(&pid)
            .ok_or(ProcessError::NoSuchProcess { pid })?;
        Ok(proc.sid)
    }

    /// Send a signal to all processes in a process group.
    pub fn signal_process_group(&mut self, pgid: u32, signal: u32) -> Vec<u32> {
        let pids: Vec<u32> = self
            .processes
            .values()
            .filter(|p| p.pgid == pgid)
            .map(|p| p.pid)
            .collect();
        // Return the list of signaled PIDs for the caller to handle delivery.
        let _ = signal;
        pids
    }

    /// Dub an MVS task as a UNIX process using an OMVS segment.
    pub fn dub(
        &mut self,
        omvs_segment: Option<&OmvsSegment>,
    ) -> Result<u32, ProcessError> {
        let seg = omvs_segment.ok_or(ProcessError::NoOmvsSegment)?;

        if self.processes.len() as u32 >= self.max_procs_sys {
            return Err(ProcessError::TableFull {
                limit: self.max_procs_sys,
            });
        }

        let pid = self.allocate_pid();
        let mut proc = UnixProcess::new(pid, 1, seg.uid, seg.gid);
        proc.cwd = seg.home.clone();
        proc.program = seg.program.clone();
        proc.dubbed = true;
        self.processes.insert(pid, proc);
        Ok(pid)
    }

    /// Stop a process (e.g., via SIGSTOP).
    pub fn stop_process(&mut self, pid: u32) -> Result<(), ProcessError> {
        let proc = self
            .processes
            .get_mut(&pid)
            .ok_or(ProcessError::NoSuchProcess { pid })?;
        proc.state = ProcessState::Stopped;
        Ok(())
    }

    /// Continue a stopped process (e.g., via SIGCONT).
    pub fn continue_process(&mut self, pid: u32) -> Result<(), ProcessError> {
        let proc = self
            .processes
            .get_mut(&pid)
            .ok_or(ProcessError::NoSuchProcess { pid })?;
        if proc.state == ProcessState::Stopped {
            proc.state = ProcessState::Running;
        }
        Ok(())
    }
}

// ===========================================================================
//  Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_process_table_and_pid() {
        let mut mgr = ProcessManager::new(1000, 256);
        let pid = mgr.create_init_process().unwrap();
        assert_eq!(pid, 1);
        assert_eq!(mgr.process_count(), 1);

        let proc = mgr.get_process(pid).unwrap();
        assert_eq!(proc.state, ProcessState::Running);
        assert_eq!(proc.uid, 0);
    }

    #[test]
    fn test_process_table_full() {
        let mut mgr = ProcessManager::new(2, 256);
        mgr.create_init_process().unwrap();
        let pid1 = mgr.fork(1).unwrap();
        assert!(matches!(pid1, ForkResult::Parent { child_pid: 2 }));
        // Table is now full (2 processes).
        let err = mgr.fork(1).unwrap_err();
        assert!(matches!(err, ProcessError::TableFull { limit: 2 }));
    }

    #[test]
    fn test_fork_returns_child_pid() {
        let mut mgr = ProcessManager::new(100, 100);
        mgr.create_init_process().unwrap();
        let result = mgr.fork(1).unwrap();
        match result {
            ForkResult::Parent { child_pid } => {
                assert_eq!(child_pid, 2);
                let child = mgr.get_process(child_pid).unwrap();
                assert_eq!(child.ppid, 1);
                assert_eq!(child.state, ProcessState::Running);
            }
            ForkResult::Child => panic!("expected Parent result"),
        }
    }

    #[test]
    fn test_fork_inherits_file_descriptors() {
        let mut mgr = ProcessManager::new(100, 100);
        mgr.create_init_process().unwrap();
        let parent = mgr.get_process_mut(1).unwrap();
        parent.file_descriptors.push(Some(FileDescriptor::Regular {
            inode: 42,
            offset: 0,
            flags: OpenFlags::read_only(),
        }));
        let result = mgr.fork(1).unwrap();
        let child_pid = match result {
            ForkResult::Parent { child_pid } => child_pid,
            _ => panic!("expected Parent"),
        };
        let child = mgr.get_process(child_pid).unwrap();
        assert_eq!(child.file_descriptors.len(), 4);
        assert!(matches!(
            &child.file_descriptors[3],
            Some(FileDescriptor::Regular { inode: 42, .. })
        ));
    }

    #[test]
    fn test_exec_replaces_program() {
        let mut mgr = ProcessManager::new(100, 100);
        mgr.create_init_process().unwrap();
        mgr.exec(1, "/bin/ls", &["-la".to_string()]).unwrap();
        let proc = mgr.get_process(1).unwrap();
        assert_eq!(proc.program, "/bin/ls");
    }

    #[test]
    fn test_exec_invalid_path() {
        let mut mgr = ProcessManager::new(100, 100);
        mgr.create_init_process().unwrap();
        let err = mgr.exec(1, "", &[]).unwrap_err();
        assert!(matches!(err, ProcessError::ExecutableNotFound { .. }));
    }

    #[test]
    fn test_spawn_combined_fork_exec() {
        let mut mgr = ProcessManager::new(100, 100);
        mgr.create_init_process().unwrap();
        let attrs = SpawnAttributes {
            program: "/bin/program".to_string(),
            args: vec!["arg1".to_string()],
            env: HashMap::new(),
            inherit_fds: true,
            share_address_space: false,
        };
        let child_pid = mgr.spawn(1, &attrs).unwrap();
        assert_eq!(child_pid, 2);
        let child = mgr.get_process(child_pid).unwrap();
        assert_eq!(child.program, "/bin/program");
    }

    #[test]
    fn test_spawn_share_address_space() {
        let mut mgr = ProcessManager::new(100, 100);
        mgr.create_init_process().unwrap();
        let attrs = SpawnAttributes {
            program: "/bin/worker".to_string(),
            args: vec![],
            env: {
                let mut e = HashMap::new();
                e.insert("_BPX_SHAREAS".to_string(), "YES".to_string());
                e
            },
            inherit_fds: true,
            share_address_space: true,
        };
        let child_pid = mgr.spawn(1, &attrs).unwrap();
        let child = mgr.get_process(child_pid).unwrap();
        assert_eq!(child.program, "/bin/worker");
    }

    #[test]
    fn test_waitpid_exited_child() {
        let mut mgr = ProcessManager::new(100, 100);
        mgr.create_init_process().unwrap();
        mgr.fork(1).unwrap();
        mgr.exit_process(2, 0).unwrap();

        let status = mgr
            .waitpid(1, Some(2), WaitFlags::blocking())
            .unwrap()
            .unwrap();
        assert!(status.if_exited());
        assert_eq!(status.exit_status(), 0);
        assert_eq!(status.pid, 2);
        // Zombie should be reaped.
        assert!(mgr.get_process(2).is_none());
    }

    #[test]
    fn test_waitpid_stopped_child() {
        let mut mgr = ProcessManager::new(100, 100);
        mgr.create_init_process().unwrap();
        mgr.fork(1).unwrap();
        mgr.stop_process(2).unwrap();

        let flags = WaitFlags {
            no_hang: false,
            untraced: true,
        };
        let status = mgr.waitpid(1, Some(2), flags).unwrap().unwrap();
        assert!(status.if_stopped());
    }

    #[test]
    fn test_waitpid_no_children() {
        let mut mgr = ProcessManager::new(100, 100);
        mgr.create_init_process().unwrap();
        let err = mgr
            .waitpid(1, None, WaitFlags::blocking())
            .unwrap_err();
        assert!(matches!(err, ProcessError::NoChildProcesses));
    }

    #[test]
    fn test_setpgid() {
        let mut mgr = ProcessManager::new(100, 100);
        mgr.create_init_process().unwrap();
        mgr.fork(1).unwrap();
        // Set child 2 to be in its own process group.
        mgr.setpgid(2, 0).unwrap();
        let pgid = mgr.getpgid(2).unwrap();
        assert_eq!(pgid, 2);
    }

    #[test]
    fn test_setsid() {
        let mut mgr = ProcessManager::new(100, 100);
        mgr.create_init_process().unwrap();
        mgr.fork(1).unwrap();
        // Put child 2 in a different group first.
        mgr.setpgid(2, 99).unwrap();
        let sid = mgr.setsid(2).unwrap();
        assert_eq!(sid, 2);
        let proc = mgr.get_process(2).unwrap();
        assert_eq!(proc.sid, 2);
        assert_eq!(proc.pgid, 2);
    }

    #[test]
    fn test_dub_with_omvs_segment() {
        let mut mgr = ProcessManager::new(100, 100);
        let seg = OmvsSegment {
            uid: 100,
            gid: 200,
            home: "/u/jsmith".to_string(),
            program: "/bin/sh".to_string(),
        };
        let pid = mgr.dub(Some(&seg)).unwrap();
        let proc = mgr.get_process(pid).unwrap();
        assert_eq!(proc.uid, 100);
        assert_eq!(proc.gid, 200);
        assert_eq!(proc.cwd, "/u/jsmith");
        assert!(proc.dubbed);
    }

    #[test]
    fn test_dub_without_omvs_segment() {
        let mut mgr = ProcessManager::new(100, 100);
        let err = mgr.dub(None).unwrap_err();
        assert!(matches!(err, ProcessError::NoOmvsSegment));
    }

    #[test]
    fn test_signal_process_group() {
        let mut mgr = ProcessManager::new(100, 100);
        mgr.create_init_process().unwrap();
        mgr.fork(1).unwrap();
        mgr.fork(1).unwrap();
        // All three in group 1.
        let pids = mgr.signal_process_group(1, 15);
        assert_eq!(pids.len(), 3);
    }

    #[test]
    fn test_process_state_display() {
        assert_eq!(ProcessState::Running.to_string(), "Running");
        assert_eq!(ProcessState::Zombie.to_string(), "Zombie");
        assert_eq!(ProcessState::Stopped.to_string(), "Stopped");
        assert_eq!(ProcessState::Sleeping.to_string(), "Sleeping");
    }

    #[test]
    fn test_per_user_limit() {
        let mut mgr = ProcessManager::new(100, 2);
        mgr.create_init_process().unwrap(); // PID 1, UID 0
        mgr.fork(1).unwrap(); // PID 2, UID 0
        // UID 0 now has 2 processes — at limit.
        let err = mgr.fork(1).unwrap_err();
        assert!(matches!(err, ProcessError::TableFull { limit: 2 }));
    }

    #[test]
    fn test_continue_stopped_process() {
        let mut mgr = ProcessManager::new(100, 100);
        mgr.create_init_process().unwrap();
        mgr.stop_process(1).unwrap();
        assert_eq!(
            mgr.get_process(1).unwrap().state,
            ProcessState::Stopped
        );
        mgr.continue_process(1).unwrap();
        assert_eq!(
            mgr.get_process(1).unwrap().state,
            ProcessState::Running
        );
    }
}
