#![forbid(unsafe_code)]
//! z/OS UNIX System Services (USS) & POSIX.
//!
//! This crate provides:
//!
//! - **POSIX Process Model** — fork, exec, spawn, wait, process groups, sessions, dubbing
//! - **Signal Handling** — sigaction, sigprocmask, sigsuspend, kill
//! - **zFS Hierarchical File System** — VFS, inodes, file I/O, permissions, links, locks
//! - **Directory & File Metadata** — mkdir, readdir, rename, unlink, chmod, chown
//! - **Pthreads** — mutexes, condition variables, read-write locks, thread-specific data
//! - **IPC Mechanisms** — pipes, FIFOs, message queues, shared memory, semaphores
//! - **POSIX Sockets** — TCP/UDP, bind, listen, accept, connect, select/poll
//! - **UNIX Shell** — tokenizer, parser, variable expansion, job control, builtins
//! - **Core UNIX Utilities** — grep, sort, sed, awk, find, wc, head, tail, test, expr
//! - **BPXPRMxx Configuration** — PARMLIB parser, RACF OMVS segments, security
//! - **Daemon Infrastructure** — inetd, cron, syslogd, codepage auto-conversion

pub mod config;
pub mod daemons;
pub mod directory;
pub mod ipc;
pub mod process;
pub mod shell;
pub mod signal;
pub mod socket;
pub mod threads;
pub mod utilities;
pub mod zfs;

// Re-export key types from each module.

pub use config::{
    BpxPrmConfig, ConfigError, FilesysType, MountMode, OmvsGroupSegment,
    OmvsSegment as ConfigOmvsSegment, SecurityManager, UnixPriv, parse_bpxprm,
};
pub use daemons::{
    AutoConvertSettings, CronSchedule, CrontabEntry, DaemonError, FileTag, Inetd,
    InetdEntry, SyslogLevel, Syslogd, iconv,
};
pub use directory::{
    DirEntry, DirHandle, DirectoryError, DirectoryManager, FileMetadata, TimeValue,
};
pub use ipc::{
    Fifo, IpcError, IpcRegistry, Message, MessageQueue, MqAttr, Pipe, PipeFds,
    Semaphore, SharedMemory,
};
pub use process::{
    FileDescriptor, ForkResult, OmvsSegment, OpenFlags, ProcessError, ProcessManager,
    ProcessState, SpawnAttributes, UnixProcess, WaitFlags, WaitStatus, WaitStatusKind,
};
pub use shell::{
    BuiltinCommand, CommandList, ControlFlow, Job, JobState, ListConnector, Pipeline,
    Redirect, RedirectType, Shell, ShellError, SimpleCommand, Token, expand_variables,
    parse_pipeline, parse_profile, tokenize,
};
pub use signal::{
    DefaultAction, KillTarget, Signal, SignalAction, SignalActionFlags,
    SignalDeliveryResult, SignalError, SignalSet, SignalState,
};
pub use socket::{
    AddressFamily, PollEvent, PollFlags, Socket, SocketAddress, SocketError,
    SocketManager, SocketOption, SocketState, SocketType,
};
pub use threads::{
    PThread, PthreadCond, PthreadMutex, PthreadRwLock, MutexType, ThreadError,
    ThreadKey, ThreadManager, ThreadState,
};
pub use utilities::{
    AwkProgram, FindOptions, FindResult, GrepMatch, GrepOptions, LsEntry, LsOptions,
    SedCommand, SedSubstitution, SortOptions, TestExpr, UtilityCategory, UtilityError,
    UtilityInfo, UtilityRegistry, WcResult, awk, evaluate_test, expr_eval, format_date,
    format_permissions, glob_match, grep, head, parse_sed, sed, sort_lines, tail, wc,
};
pub use zfs::{
    CodepageTag, DirectoryEntry, FileLock, FileType, FsError, Inode, LockType,
    MountEntry as ZfsMountEntry, MvsDatasetRef, SeekWhence, Zfs,
};
