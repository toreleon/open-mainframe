---
version: 'v5.0'
planningGroup: 'PG-15'
technology: 'USS & POSIX (UNIX System Services)'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-uss-v5.0.md'
---

# Architecture: USS & POSIX (UNIX System Services)

## 1. Crate Strategy

**New crate:** `open-mainframe-uss`

Rationale: USS is a large, self-contained subsystem (process model, file system, shell, utilities, IPC, signals) with 11 epics. It has its own lifecycle and a distinct API surface (BPX callable services). A separate crate keeps the dependency graph clean while allowing integration with MVS, RACF, and encoding crates.

## 2. Module Layout

```
crates/open-mainframe-uss/src/
├── lib.rs
├── process/
│   ├── mod.rs          # ProcessManager, PID table, dubbing
│   ├── fork.rs         # fork/spawn/exec/wait
│   ├── signals.rs      # Signal handling (sigaction, sigprocmask)
│   └── session.rs      # Sessions, process groups, job control
├── fs/
│   ├── mod.rs          # VFS layer, mount table
│   ├── zfs.rs          # zFS file system implementation
│   ├── inode.rs        # Inode table, file metadata
│   ├── directory.rs    # Directory operations
│   ├── permissions.rs  # POSIX permission checks (rwx, setuid/setgid)
│   └── mvs_bridge.rs   # //'DSN' syntax to MVS dataset access
├── threads/
│   ├── mod.rs          # pthreads abstraction
│   ├── mutex.rs        # pthread_mutex_*
│   └── cond.rs         # pthread_cond_*, pthread_rwlock_*
├── ipc/
│   ├── mod.rs          # IPC registry
│   ├── pipe.rs         # pipe(), FIFO (mkfifo)
│   ├── mqueue.rs       # POSIX message queues
│   ├── shm.rs          # Shared memory
│   └── semaphore.rs    # POSIX semaphores
├── socket/
│   ├── mod.rs          # Socket API (socket/bind/listen/accept/connect)
│   ├── tcp.rs          # TCP streams
│   ├── udp.rs          # UDP datagrams
│   └── select.rs       # select()/poll() multiplexing
├── shell/
│   ├── mod.rs          # Shell interpreter (/bin/sh)
│   ├── parser.rs       # Shell grammar parser
│   ├── executor.rs     # Command execution, piping, redirection
│   ├── builtins.rs     # Built-in commands (cd, export, echo, etc.)
│   └── job_control.rs  # Background jobs, fg/bg/jobs
├── utilities/
│   ├── mod.rs          # Utility registry
│   ├── file_utils.rs   # ls, cp, mv, rm, mkdir, chmod, chown, cat, find
│   ├── text_utils.rs   # grep, sed, awk, sort, head, tail, wc
│   └── misc_utils.rs   # date, env, expr, test, printf, echo
├── daemons/
│   ├── mod.rs          # Daemon management
│   ├── inetd.rs        # Super-server daemon
│   ├── cron.rs         # Scheduled task daemon
│   └── syslogd.rs      # System logging daemon
├── config/
│   ├── mod.rs          # BPXPRMxx parser
│   └── parmlib.rs      # PARMLIB parameter processing
├── services/
│   ├── mod.rs          # BPX1xxx/BPX4xxx callable services
│   └── codepage.rs     # Auto-conversion, file tagging, iconv
└── security.rs         # RACF OMVS segment, UID/GID, superuser
```

## 3. Key Types

```rust
/// UNIX process representation
pub struct UnixProcess {
    pub pid: u32,
    pub ppid: u32,
    pub pgid: u32,
    pub sid: u32,
    pub uid: u32,
    pub gid: u32,
    pub euid: u32,
    pub egid: u32,
    pub state: ProcessState,
    pub signal_mask: SignalSet,
    pub signal_handlers: HashMap<Signal, SignalAction>,
    pub file_descriptors: Vec<Option<FileDescriptor>>,
    pub cwd: PathBuf,
    pub umask: u16,
}

pub enum ProcessState {
    Running,
    Stopped,
    Zombie,
    Sleeping,
}

/// POSIX signal
pub enum Signal {
    SIGHUP, SIGINT, SIGQUIT, SIGILL, SIGABRT, SIGFPE,
    SIGKILL, SIGSEGV, SIGPIPE, SIGALRM, SIGTERM,
    SIGUSR1, SIGUSR2, SIGCHLD, SIGSTOP, SIGTSTP,
    SIGCONT, SIGTTIN, SIGTTOU,
}

/// zFS inode
pub struct Inode {
    pub ino: u64,
    pub mode: u32,       // file type + permissions
    pub nlink: u32,
    pub uid: u32,
    pub gid: u32,
    pub size: u64,
    pub atime: SystemTime,
    pub mtime: SystemTime,
    pub ctime: SystemTime,
    pub file_type: FileType,
    pub tag: Option<CodepageTag>,
}

pub enum FileType {
    RegularFile, Directory, SymbolicLink, Fifo, Socket,
    CharDevice, BlockDevice,
}

/// BPXPRMxx configuration
pub struct BpxPrmConfig {
    pub maxprocsys: u32,
    pub maxprocuser: u32,
    pub maxfileproc: u32,
    pub maxthreads: u32,
    pub root: String,
    pub mounts: Vec<MountEntry>,
    pub filesys_types: Vec<FilesysType>,
}

/// File descriptor
pub enum FileDescriptor {
    Regular { inode: u64, offset: u64, flags: OpenFlags },
    Pipe { pipe_id: u32, read_end: bool },
    Socket { socket_id: u32 },
    Directory { inode: u64, position: usize },
}
```

## 4. Design Decisions

### DD-5.0-USS-01: Process Model via Tokio Tasks
**Decision:** USS processes map to Tokio tasks (not OS processes). Fork creates a new task with cloned state. This enables the POSIX process model without OS-level forking.

### DD-5.0-USS-02: zFS as Directory-Backed Storage
**Decision:** zFS is implemented as a directory tree on the host filesystem. Inodes are tracked in-memory with metadata persisted to a sidecar file. This provides real POSIX semantics while leveraging the host OS.

### DD-5.0-USS-03: Shell as Interpreter
**Decision:** The POSIX shell (/bin/sh) is implemented as a Rust interpreter, not by delegating to the host shell. This ensures z/OS-specific behaviors (EBCDIC, //'DSN' syntax) work correctly.

### DD-5.0-USS-04: Sockets via Tokio Networking
**Decision:** POSIX sockets map to Tokio's async TCP/UDP primitives. The USS socket API provides the POSIX interface while Tokio handles actual networking.
