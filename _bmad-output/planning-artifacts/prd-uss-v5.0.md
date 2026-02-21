---
version: 'v5.0'
planningGroup: 'PG-15'
technology: 'USS & POSIX (UNIX System Services)'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'gap-analysis/batch-18-uss.md'
  - 'zos-complete-inventory.md (AREA-18)'
  - 'gap-analysis/batch-22-priority-matrix.md'
epicIds: ['USS-100', 'USS-101', 'USS-102', 'USS-103', 'USS-104', 'USS-105', 'USS-106', 'USS-107', 'USS-108', 'USS-109', 'USS-110']
sysEpicIds: ['SYS-118', 'SYS-119']
totalFRs: 14
totalNFRs: 4
---

# PRD: USS & POSIX (UNIX System Services)

## 1. Problem Statement

OpenMainframe has no POSIX/UNIX environment. z/OS USS is core infrastructure required by z/OSMF, OpenSSH, Java SDK, and hundreds of UNIX-origin tools. Without USS, the system cannot host UNIX workloads, run shell scripts, or provide the hierarchical file system (zFS) that modern z/OS depends on.

## 2. User Personas

- **UNIX Application Developer** — ports UNIX applications to z/OS, uses shell, make, compilers
- **System Programmer** — configures BPXPRMxx, manages file systems, administers UNIX security
- **DevOps Engineer** — uses USS shell for automation scripts, CI/CD pipelines on z/OS
- **Operator** — monitors USS processes, manages daemons (inetd, cron, syslogd)

## 3. Functional Requirements

| ID | Requirement |
|----|-------------|
| FR-USS-001 | Implement POSIX process model: fork, exec, spawn, wait, kill, getpid, setpgid, setsid |
| FR-USS-002 | Implement POSIX signal handling: sigaction, sigprocmask, sigsuspend with all standard signals (SIGHUP through SIGTTOU) |
| FR-USS-003 | Implement zFS hierarchical file system with POSIX file operations: open, close, read, write, lseek, stat, chmod, chown, mkdir, rmdir, link, unlink, symlink, readlink, rename |
| FR-USS-004 | Implement directory operations: opendir, readdir, closedir, getcwd, chdir |
| FR-USS-005 | Implement pthreads: pthread_create, pthread_join, pthread_mutex_*, pthread_cond_*, pthread_rwlock_* |
| FR-USS-006 | Implement IPC mechanisms: pipe, FIFO (mkfifo), POSIX message queues (mq_open/mq_send/mq_receive), shared memory (shm_open/shm_unlink), semaphores (sem_open/sem_post/sem_wait) |
| FR-USS-007 | Implement POSIX sockets: socket, bind, listen, accept, connect, send, recv, select/poll with IPv4/IPv6 |
| FR-USS-008 | Implement UNIX shell (/bin/sh) with job control, piping, redirection, variable expansion, and control flow (if/then/else, for, while, case) |
| FR-USS-009 | Implement core UNIX utilities: ls, cat, cp, mv, rm, mkdir, chmod, chown, grep, sed, awk, find, sort, head, tail, wc, echo, printf, test, expr, date, env |
| FR-USS-010 | Implement BPXPRMxx PARMLIB configuration parsing: MAXPROCSYS, MAXPROCUSER, MAXFILEPROC, MAXTHREADS, FILESYSTYPE, ROOT, MOUNT |
| FR-USS-011 | Implement RACF OMVS segment integration: UID/GID mapping, UNIXPRIV class, BPX.DAEMON facility, superuser checks |
| FR-USS-012 | Implement ASCII/EBCDIC auto-conversion: _BPXK_AUTOCVT, file tagging (chtag), iconv() |
| FR-USS-013 | Implement BPX1xxx/BPX4xxx callable services interface for assembler/LE callers |
| FR-USS-014 | Implement daemon infrastructure: inetd (super-server), cron (scheduler), syslogd (logging) |

## 4. Non-Functional Requirements

| ID | Requirement |
|----|-------------|
| NFR-USS-001 | fork/spawn latency < 50ms for in-process (shared address space) mode |
| NFR-USS-002 | zFS supports at least 10,000 files per directory and file sizes up to 8GB |
| NFR-USS-003 | All POSIX APIs follow Single UNIX Specification v1 (UNIX 95) semantics |
| NFR-USS-004 | File system operations integrate with existing open-mainframe-dataset for MVS dataset access via //'DSN' syntax |

## 5. Scope

**MVP (Phase C):** Process model, file system, shell, core utilities, signal handling, RACF integration
**Full:** Pthreads, IPC, sockets, daemons, BPX callable services, codepage auto-conversion
**Deferred:** TFS (temporary file system), NFS server, HFS compatibility

## 6. Dependencies

- `open-mainframe-encoding` — EBCDIC/ASCII codepage conversion
- `open-mainframe-racf` — OMVS segment, UNIXPRIV class, BPX.DAEMON
- `open-mainframe-dataset` — MVS dataset access from USS via //'DSN' syntax
- PG-16 (Networking) — sockets depend on TCP/IP stack
- PG-1 (MVS System Services) — address space management for fork/spawn
