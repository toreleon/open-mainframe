# open-mainframe-uss

A high-performance Rust implementation of **z/OS UNIX System Services (USS)** for the OpenMainframe project — providing a complete POSIX-compliant environment including a hierarchical file system (zFS), process management, multi-threading, and a full-featured UNIX shell.

## Overview

USS provides a UNIX-like environment on z/OS, enabling the execution of POSIX-compliant applications alongside traditional MVS workloads. This crate reimplements the core USS services (BPX SVCs), providing a bridge between mainframe data management and modern UNIX concepts.

The implementation comprises:
1. **zFS File System** — A virtual hierarchical file system with support for inodes, permissions, symbolic links, and file locking.
2. **Process & Thread Model** — Implementation of POSIX `fork`, `exec`, `spawn`, and `pthreads`, including signal handling and synchronization primitives.
3. **UNIX Shell** — A full-featured shell interpreter with support for variable expansion, redirection, and job control.
4. **Inter-Process Communication (IPC)** — Support for pipes, FIFOs, message queues, and shared memory.
5. **Network Stack** — POSIX-compatible socket API supporting TCP/UDP and UNIX domain sockets.
6. **BPXWDYN Integration** — A text-based interface for dynamic allocation, allowing USS applications to access MVS datasets.

## Architecture

```
    UNIX Application / Shell
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  USS System Call Interface             │
    │  - POSIX API (fork, exec, open, socket)                │
    │  - BPX1xxx / BPX4xxx SVC emulation                     │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │               Process & Thread Manager                 │
    │  - PID and PPID tracking                               │
    │  - Pthreads (Mutex, CondVar)                           │
    │  - Signal delivery and masking                         │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  zFS / VFS Layer                       │
    │  - Hierarchical structure (Inodes)                     │
    │  - Permission & Ownership (UID/GID)                    │
    │  - MVS Dataset Mapping                                 │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                 Subsystem Connectors                   │
    │  - BPXWDYN (MVS Allocation)                            │
    │  - RACF (OMVS Segments)                                │
    │  - IPC Registry (Shm, MsgQ)                            │
    └────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Description | Lines |
|--------|-------------|------:|
| `zfs` | zFS (z/OS File System): VFS implementation with inodes and links | ~1,409 |
| `shell` | UNIX Shell: Tokenizer, parser, and interpreter with job control | ~1,002 |
| `process` | Process model: fork, exec, spawn, and PID management | ~967 |
| `utilities`| Core UNIX utilities: Implementation of grep, sed, awk, find, etc. | ~926 |
| `socket` | Network services: POSIX Sockets (TCP/UDP) and address mapping | ~885 |
| `threads` | Pthreads: Multi-threading, mutexes, and condition variables | ~840 |
| `ipc` | IPC mechanisms: Pipes, FIFOs, message queues, and shared memory | ~815 |
| `directory`| Metadata services: Directory traversal and file attributes | ~737 |
| `daemons` | System daemons: cron, syslogd, and inetd emulation | ~690 |
| `bpxwdyn` | BPXWDYN: Text-based interface for MVS dataset allocation | ~627 |
| `config` | BPXPRMxx: Configuration parser and OMVS segment logic | ~609 |
| `signal` | Signal management: Delivery, masking, and sigaction | ~594 |
| `mmap` | Memory management: implementation of mmap and mprotect | ~532 |

**Total**: ~10,721 lines of Rust.

## Key Types and Components

### Filesystem & I/O

| Type | Description |
|------|-------------|
| `Zfs` | The main virtual file system handle. |
| `Inode` | Represents a file or directory in the hierarchy. |
| `FileDescriptor`| Per-process handle for open files and sockets. |
| `BpxwdynManager`| Facilitator for MVS dataset access from USS. |

### Process & Execution

| Type | Description |
|------|-------------|
| `UnixProcess` | Represents a running process with its own environment and state. |
| `ProcessManager`| Central coordinator for process lifecycle and scheduling. |
| `Shell` | The core execution engine for shell scripts. |
| `PThread` | Representation of a POSIX thread. |

### Configuration

| Type | Description |
|------|-------------|
| `BpxPrmConfig` | Parser for the BPXPRMxx PARMLIB member. |
| `OmvsSegment` | User security attributes (UID, GID, Home, Shell) from RACF. |

## Implementation Details

### zFS Virtual File System

The `zFS` implementation provides a full VFS layer:
- **Hierarchical Inodes**: Files and directories are managed via a persistent inode table.
- **Permissions**: Full support for POSIX mode bits (rwx) and ownership checks.
- **MVS Mapping**: Special mount points can be defined to map MVS datasets into the USS tree (e.g., `/mvs/SYS1/PARMLIB`).

### POSIX Process Emulation

Since this is a simulated environment, `fork` and `exec` are implemented via state-cloning and module re-initialization:
- **fork**: Creates a new `UnixProcess` with cloned environment and file descriptors.
- **exec**: Replaces the active program image while preserving the PID and open files.
- **signals**: Asynchronous signal delivery is implemented via a priority queue per process.

### BPXWDYN Interface

BPXWDYN allows USS programs to perform MVS dynamic allocation using a string-based API. This crate implements the parser and mapping to the `open-mainframe-mvs` SVC 99 engine, enabling commands like:
`alloc fi(sysut1) da('user.data') shr`

## Feature Coverage

| Feature | Category | Status |
|---------|----------|--------|
| fork / exec     | Process  | Implemented |
| Pthreads        | Process  | Implemented (Mutex, CondVar) |
| zFS VFS         | Storage  | Implemented (Inodes, Links) |
| Pipes / FIFOs   | IPC      | Implemented |
| POSIX Sockets   | Network  | Implemented (TCP/UDP) |
| UNIX Shell      | Interface| Implemented (Expansion, Redir) |
| UNIX Utilities  | Tools    | Implemented (10+ utilities) |
| BPXWDYN         | Interop  | Implemented |
| cron / syslogd  | Daemons  | Implemented |

## Usage Examples

### Executing a Shell Script

```rust
use open_mainframe_uss::shell::Shell;

let mut shell = Shell::new();
// Execute a pipeline with redirection
let result = shell.execute("cat /etc/passwd | grep root > /tmp/out").unwrap();
```

### Programmatic File Access (BPX1OPN)

```rust
use open_mainframe_uss::zfs::{Zfs, OpenFlags};

let mut fs = Zfs::new();
let fd = fs.open("/tmp/test.txt", OpenFlags::O_CREAT | OpenFlags::O_RDWR, 0o644).unwrap();
fs.write(fd, b"Hello USS").unwrap();
fs.close(fd).unwrap();
```

## Testing

The USS crate includes an extensive test suite (500+ tests):
- **VFS Tests**: Consistency checks for multi-level directory structures and hard links.
- **Shell Tests**: Validates complex quoting, variable expansion, and subshell execution.
- **Process Tests**: Ensures correct signal delivery and `waitpid` behavior.
- **Socket Tests**: Validates local and loopback TCP communication.

```sh
cargo test -p open-mainframe-uss
```

## Limitations and Future Work

- **Mount Points**: While zFS supports mounts, the `MOUNT` and `UNMOUNT` commands are currently partially implemented.
- **Dubbing**: The process of "dubbing" an MVS task into an OMVS thread is in progress.
- **Character Conversion**: Automatic conversion between EBCDIC and ASCII for file I/O is supported but requires explicit tagging.
