---
version: 'v5.0'
planningGroup: 'PG-15'
technology: 'USS & POSIX (UNIX System Services)'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-uss-v5.0.md'
  - 'architecture-uss-v5.0.md'
totalEpics: 11
totalStories: 60
frCoverage: '14/14 (100%)'
nfrCoverage: '4/4 (100%)'
---

# Epics & Stories: USS & POSIX (UNIX System Services)

## Epic Overview

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| USS-100 | POSIX Process Model | XL | 7 | C |
| USS-101 | Signal Handling | L | 5 | C |
| USS-102 | zFS Hierarchical File System | XL | 8 | C |
| USS-103 | Directory & File Metadata Operations | L | 5 | C |
| USS-104 | Pthreads | L | 5 | C |
| USS-105 | IPC Mechanisms | L | 6 | C |
| USS-106 | POSIX Sockets | L | 6 | C |
| USS-107 | UNIX Shell (/bin/sh) | XL | 6 | C |
| USS-108 | Core UNIX Utilities | L | 5 | C |
| USS-109 | BPXPRMxx Configuration & Security | M | 4 | C |
| USS-110 | Daemon Infrastructure & Codepage Services | M | 3 | C |

---

## USS-100: POSIX Process Model

**User Value:** UNIX applications can create processes via fork/exec/spawn and manage their lifecycle, enabling multi-process UNIX workloads on z/OS.

### USS-100.1: Process Table and PID Management

**As a** UNIX developer, **I want** a process table tracking PIDs, PPIDs, UIDs, and states, **so that** every UNIX process has a unique identity.

**Acceptance Criteria:**
- Given a new process is created, when assigned a PID, then it is unique within the system and tracked in the process table
- Given the process table, when queried by PID, then process state (Running/Stopped/Zombie/Sleeping) is returned
- Given MAXPROCSYS=1000, when the limit is reached, then fork/spawn fails with EAGAIN

### USS-100.2: fork() and Process Duplication

**As a** UNIX developer, **I want** fork() to create a child process duplicating the parent, **so that** standard UNIX process creation patterns work.

**Acceptance Criteria:**
- Given fork() is called, when it returns to the parent, then the child's PID is returned
- Given fork() is called, when it returns to the child, then 0 is returned
- Given fork(), when the child inherits the parent's file descriptors, then both parent and child share the same open files

### USS-100.3: exec() Family

**As a** UNIX developer, **I want** exec() to replace the current process image with a new program, **so that** programs can be launched.

**Acceptance Criteria:**
- Given exec("/bin/ls", args), when called, then the current process image is replaced with /bin/ls
- Given exec() with an invalid path, when called, then errno is set to ENOENT
- Given execmvs() for an MVS program, when called, then the MVS program executes within the UNIX process context

### USS-100.4: spawn() — Combined Fork+Exec

**As a** UNIX developer, **I want** spawn() to create a new process running a specified program, **so that** process creation is efficient.

**Acceptance Criteria:**
- Given spawn("/bin/program", args, inherit), when called, then a new process is created running /bin/program
- Given _BPX_SHAREAS=YES, when spawn() is called, then the child runs as a thread in the parent's address space

### USS-100.5: wait() and waitpid()

**As a** UNIX developer, **I want** wait/waitpid to collect child process exit status, **so that** parents can synchronize with children.

**Acceptance Criteria:**
- Given a child process exits with status 0, when waitpid(pid) is called, then WIFEXITED is true and WEXITSTATUS is 0
- Given a child is stopped by SIGSTOP, when waitpid with WUNTRACED is called, then WIFSTOPPED is true

### USS-100.6: Process Groups and Sessions

**As a** UNIX developer, **I want** setpgid/getpgrp/setsid for process groups and sessions, **so that** job control works.

**Acceptance Criteria:**
- Given setpgid(0, 0), when called, then the calling process becomes the leader of a new process group
- Given setsid(), when called by a non-group-leader, then a new session is created and the process becomes session leader

### USS-100.7: Dubbing — MVS-to-UNIX Bridge

**As a** system programmer, **I want** MVS tasks dubbed as UNIX processes on first syscall, **so that** MVS programs can use UNIX services.

**Acceptance Criteria:**
- Given an MVS task with OMVS segment, when it issues the first BPX1xxx call, then it is dubbed as a UNIX process with UID/GID from the OMVS segment
- Given an MVS task without OMVS segment, when it calls a BPX service, then it runs with the DEFAULT OMVS segment or gets EPERM

---

## USS-101: Signal Handling

**User Value:** UNIX applications can send and handle signals following POSIX semantics, enabling standard interrupt, termination, and inter-process communication patterns.

### USS-101.1: Signal Delivery Infrastructure

**As a** UNIX developer, **I want** signals delivered to processes per POSIX semantics, **so that** standard signal behavior works.

**Acceptance Criteria:**
- Given SIGTERM sent to a process, when no handler is registered, then the process is terminated (default action)
- Given SIGKILL sent to a process, when delivered, then the process is unconditionally terminated (cannot be caught)
- Given SIGCHLD sent to parent, when a child exits, then the parent is notified

### USS-101.2: sigaction() — Signal Handler Registration

**As a** UNIX developer, **I want** sigaction() to register signal handlers, **so that** I can customize signal responses.

**Acceptance Criteria:**
- Given sigaction(SIGINT, handler), when SIGINT is delivered, then the handler function is invoked
- Given SA_RESTART flag, when a signal interrupts a system call, then the system call is automatically restarted

### USS-101.3: sigprocmask() — Signal Blocking

**As a** UNIX developer, **I want** sigprocmask() to block/unblock signals, **so that** critical sections are protected.

**Acceptance Criteria:**
- Given sigprocmask(SIG_BLOCK, {SIGINT}), when SIGINT is sent, then it is queued and delivered after unblocking

### USS-101.4: sigsuspend() — Wait for Signal

**As a** UNIX developer, **I want** sigsuspend() to atomically set signal mask and wait, **so that** race-free signal waiting works.

**Acceptance Criteria:**
- Given sigsuspend(empty_mask), when a signal arrives, then the process wakes and the handler runs

### USS-101.5: kill() — Send Signal to Process

**As a** UNIX developer, **I want** kill(pid, signal) to send signals to processes, **so that** inter-process signaling works.

**Acceptance Criteria:**
- Given kill(child_pid, SIGTERM), when the child has no handler, then the child is terminated
- Given kill(-pgid, SIGHUP), when called, then all processes in the process group receive SIGHUP

---

## USS-102: zFS Hierarchical File System

**User Value:** A POSIX-compliant hierarchical file system provides the foundation for all UNIX file operations, enabling standard UNIX applications to work with files and directories.

### USS-102.1: VFS Layer and Mount Table

**As a** system programmer, **I want** a VFS abstraction with a mount table, **so that** multiple file system types can be mounted.

**Acceptance Criteria:**
- Given a mount table with / (zFS) and /tmp (TFS), when a path is resolved, then the correct file system handles the operation
- Given MOUNT FILESYSTEM('OMVS.ROOT') TYPE(ZFS) MODE(RDWR) MOUNTPOINT('/'), when processed, then the root file system is mounted

### USS-102.2: Inode Table and File Metadata

**As a** UNIX developer, **I want** inodes tracking file metadata (mode, uid, gid, size, timestamps), **so that** stat() returns correct information.

**Acceptance Criteria:**
- Given a file with mode 0644, uid 100, gid 200, when stat() is called, then all metadata fields are returned correctly
- Given a file is modified, when mtime is checked, then it reflects the modification time

### USS-102.3: File Open/Close/Read/Write/Lseek

**As a** UNIX developer, **I want** standard file I/O operations, **so that** POSIX file access works.

**Acceptance Criteria:**
- Given open("/data/file.txt", O_RDWR|O_CREAT, 0644), when called, then a file descriptor is returned
- Given write(fd, data, 100), when called, then 100 bytes are written and the file offset advances
- Given lseek(fd, 0, SEEK_SET), when called, then the file offset returns to the beginning

### USS-102.4: POSIX Permission Checks

**As a** UNIX developer, **I want** rwx permission checks on file operations, **so that** access control works.

**Acceptance Criteria:**
- Given a file with mode 0600 owned by uid 100, when uid 200 attempts read, then EACCES is returned
- Given a directory with mode 0755, when any user lists contents, then opendir succeeds
- Given setuid bit on an executable, when exec'd, then euid changes to the file owner

### USS-102.5: Symbolic Links and Hard Links

**As a** UNIX developer, **I want** symlink/link/readlink operations, **so that** file aliasing works.

**Acceptance Criteria:**
- Given symlink("/data/target", "/data/link"), when stat("/data/link") is called, then it returns the target's metadata
- Given link("/data/file", "/data/hardlink"), when called, then both names point to the same inode (nlink=2)

### USS-102.6: File Locking (fcntl/flock)

**As a** UNIX developer, **I want** advisory file locking via fcntl(F_SETLK), **so that** concurrent file access is coordinated.

**Acceptance Criteria:**
- Given fcntl(fd, F_SETLK, read_lock), when another process attempts write lock, then it gets EAGAIN
- Given fcntl(fd, F_SETLKW, write_lock), when a conflicting lock exists, then the call blocks until released

### USS-102.7: MVS Dataset Bridge (//'DSN' Syntax)

**As a** UNIX developer, **I want** //'DATASET.NAME' syntax to access MVS datasets from USS, **so that** UNIX tools can read/write MVS data.

**Acceptance Criteria:**
- Given open("//'MY.DATA.SET'", O_RDONLY), when the dataset exists, then it is opened for reading
- Given a PDS member access via //'MY.PDS(MEMBER)', when opened, then the member content is accessible

### USS-102.8: File System Tests

**Acceptance Criteria:**
- Given a comprehensive test suite, when `cargo test -p open-mainframe-uss` fs tests run, then all pass
- Given standard POSIX test vectors, when applied, then file I/O, permissions, and links behave correctly

---

## USS-103: Directory & File Metadata Operations

**User Value:** Directory traversal and metadata operations enable standard UNIX navigation and file management.

### USS-103.1: mkdir/rmdir

**As a** UNIX developer, **I want** mkdir/rmdir to create and remove directories.

**Acceptance Criteria:**
- Given mkdir("/data/newdir", 0755), when called, then the directory is created with the specified permissions
- Given rmdir("/data/emptydir"), when the directory is empty, then it is removed

### USS-103.2: opendir/readdir/closedir

**As a** UNIX developer, **I want** directory iteration, **so that** I can list directory contents.

**Acceptance Criteria:**
- Given opendir("/data"), when readdir is called repeatedly, then all entries (including . and ..) are returned
- Given closedir(), when called, then the directory stream is freed

### USS-103.3: getcwd/chdir

**As a** UNIX developer, **I want** current working directory management.

**Acceptance Criteria:**
- Given chdir("/data"), when getcwd() is called, then "/data" is returned

### USS-103.4: rename/unlink

**As a** UNIX developer, **I want** to rename and delete files.

**Acceptance Criteria:**
- Given rename("/data/old", "/data/new"), when called, then the file is atomically renamed
- Given unlink("/data/file"), when nlink reaches 0, then the file is removed

### USS-103.5: chmod/chown/utime

**As a** UNIX developer, **I want** to change file metadata.

**Acceptance Criteria:**
- Given chmod("/data/file", 0755), when called, then the file permissions are updated
- Given chown("/data/file", 100, 200), when called by root, then uid and gid are changed

---

## USS-104: Pthreads

**User Value:** Multi-threaded UNIX applications can use POSIX threads for parallel execution within a single process.

### USS-104.1: pthread_create/pthread_join

**As a** UNIX developer, **I want** to create and join threads, **so that** parallel execution works.

**Acceptance Criteria:**
- Given pthread_create(&tid, NULL, func, arg), when called, then a new thread executes func(arg)
- Given pthread_join(tid, &result), when the thread completes, then the result is returned

### USS-104.2: pthread_mutex_*

**As a** UNIX developer, **I want** mutexes for mutual exclusion.

**Acceptance Criteria:**
- Given pthread_mutex_lock(&mutex), when another thread holds it, then the call blocks
- Given PTHREAD_MUTEX_RECURSIVE attribute, when the same thread locks twice, then it succeeds (requires two unlocks)

### USS-104.3: pthread_cond_*

**As a** UNIX developer, **I want** condition variables for thread synchronization.

**Acceptance Criteria:**
- Given pthread_cond_wait(&cond, &mutex), when another thread calls pthread_cond_signal(&cond), then the waiter wakes

### USS-104.4: pthread_rwlock_*

**As a** UNIX developer, **I want** read-write locks for concurrent read access.

**Acceptance Criteria:**
- Given pthread_rwlock_rdlock(), when multiple readers hold the lock, then all succeed concurrently
- Given a writer holding the lock, when a reader attempts rdlock, then it blocks

### USS-104.5: Thread-Specific Data and Cancellation

**As a** UNIX developer, **I want** pthread_key_create/pthread_setspecific and pthread_cancel.

**Acceptance Criteria:**
- Given pthread_key_create(&key, destructor), when thread exits, then destructor is called with the key's value
- Given pthread_cancel(tid), when the target thread reaches a cancellation point, then it is terminated

---

## USS-105: IPC Mechanisms

**User Value:** UNIX inter-process communication primitives enable standard patterns for data exchange between processes.

### USS-105.1: pipe()

**As a** UNIX developer, **I want** pipe() for unidirectional data flow between processes.

**Acceptance Criteria:**
- Given pipe(fds), when write(fds[1], data) is called, then read(fds[0]) retrieves the data
- Given the write end is closed, when read() is called, then EOF (0 bytes) is returned

### USS-105.2: FIFO (Named Pipes)

**As a** UNIX developer, **I want** mkfifo() for named pipes in the file system.

**Acceptance Criteria:**
- Given mkfifo("/tmp/myfifo", 0644), when one process opens for writing and another for reading, then data flows between them

### USS-105.3: POSIX Message Queues

**As a** UNIX developer, **I want** mq_open/mq_send/mq_receive for message-based IPC.

**Acceptance Criteria:**
- Given mq_open("/myqueue", O_CREAT|O_RDWR, 0644, &attr), when mq_send is called, then the message is queued
- Given mq_receive on a non-empty queue, when called, then the highest priority message is returned

### USS-105.4: Shared Memory

**As a** UNIX developer, **I want** shm_open/mmap for shared memory segments.

**Acceptance Criteria:**
- Given shm_open("/myshm", O_CREAT|O_RDWR, 0644) + ftruncate + mmap, when two processes map it, then writes are visible to both

### USS-105.5: POSIX Semaphores

**As a** UNIX developer, **I want** sem_open/sem_post/sem_wait for synchronization.

**Acceptance Criteria:**
- Given sem_open("/mysem", O_CREAT, 0644, 1), when sem_wait reduces to 0, then another sem_wait blocks

### USS-105.6: IPC Tests

**Acceptance Criteria:**
- Given pipe, FIFO, message queue, shared memory, and semaphore tests, when `cargo test -p open-mainframe-uss` IPC tests run, then all pass

---

## USS-106: POSIX Sockets

**User Value:** Network applications can use the standard POSIX socket API for TCP/UDP communication.

### USS-106.1: Socket Creation and Address Binding

**As a** UNIX developer, **I want** socket/bind for creating network endpoints.

**Acceptance Criteria:**
- Given socket(AF_INET, SOCK_STREAM, 0), when called, then a TCP socket file descriptor is returned
- Given bind(fd, addr, addrlen) with port 8080, when called, then the socket is bound to the address

### USS-106.2: TCP Server (listen/accept)

**As a** UNIX developer, **I want** listen/accept for TCP servers.

**Acceptance Criteria:**
- Given listen(fd, 5) + accept(fd, &addr, &len), when a client connects, then accept returns a new fd for the connection

### USS-106.3: TCP Client (connect)

**As a** UNIX developer, **I want** connect() for TCP clients.

**Acceptance Criteria:**
- Given connect(fd, server_addr, len), when the server is listening, then the connection is established

### USS-106.4: send/recv and sendto/recvfrom

**As a** UNIX developer, **I want** data transfer operations for TCP and UDP.

**Acceptance Criteria:**
- Given send(fd, data, len, 0) on a connected TCP socket, when called, then data is transmitted
- Given recvfrom(fd, buf, len, 0, &addr, &addrlen) on a UDP socket, when a datagram arrives, then the data and source address are returned

### USS-106.5: select/poll Multiplexing

**As a** UNIX developer, **I want** select/poll for I/O multiplexing.

**Acceptance Criteria:**
- Given select(nfds, &readfds, NULL, NULL, &timeout), when a fd becomes readable, then select returns with that fd set
- Given poll(fds, nfds, timeout) with POLLIN events, when data arrives, then revents has POLLIN set

### USS-106.6: IPv6 and Socket Options

**As a** UNIX developer, **I want** AF_INET6 and setsockopt for socket configuration.

**Acceptance Criteria:**
- Given socket(AF_INET6, SOCK_STREAM, 0), when created, then IPv6 TCP communication works
- Given setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, 1), when set, then address reuse is enabled

---

## USS-107: UNIX Shell (/bin/sh)

**User Value:** A POSIX-compliant shell enables interactive and scripted UNIX operations, forming the primary user interface for USS.

### USS-107.1: Shell Parser

**As a** UNIX user, **I want** shell syntax parsed (commands, pipes, redirections, variables), **so that** shell scripts work.

**Acceptance Criteria:**
- Given `ls -la | grep .txt > output.txt`, when parsed, then pipeline of [ls -la] -> [grep .txt] with output redirect is recognized
- Given `var="hello"; echo $var`, when parsed, then variable assignment and expansion are recognized

### USS-107.2: Command Execution and Piping

**As a** UNIX user, **I want** commands executed with piping and redirection.

**Acceptance Criteria:**
- Given `cat file.txt | sort | uniq`, when executed, then each command runs in a child process connected by pipes
- Given `echo hello > /tmp/out 2>&1`, when executed, then stdout and stderr go to /tmp/out

### USS-107.3: Control Flow (if/for/while/case)

**As a** UNIX user, **I want** shell control flow statements.

**Acceptance Criteria:**
- Given `if [ -f /etc/profile ]; then echo exists; fi`, when /etc/profile exists, then "exists" is printed
- Given `for f in *.txt; do echo $f; done`, when 3 .txt files exist, then all 3 names are printed

### USS-107.4: Variable Expansion and Quoting

**As a** UNIX user, **I want** $VAR, ${VAR}, ${VAR:-default}, backtick, and $(cmd) substitution.

**Acceptance Criteria:**
- Given `echo ${HOME:-/tmp}`, when HOME is unset, then "/tmp" is printed
- Given `echo $(date)`, when executed, then the output of the date command is substituted

### USS-107.5: Job Control (fg/bg/jobs)

**As a** UNIX user, **I want** background jobs and job control.

**Acceptance Criteria:**
- Given `sleep 100 &`, when executed, then the process runs in the background and a job number is displayed
- Given `jobs`, when called, then all background jobs are listed with status
- Given `fg %1`, when called, then job 1 is brought to the foreground

### USS-107.6: Built-in Commands and Profile

**As a** UNIX user, **I want** built-in commands (cd, export, source, exit, umask) and /etc/profile loading.

**Acceptance Criteria:**
- Given `cd /tmp && pwd`, when executed, then "/tmp" is printed
- Given /etc/profile contains `export PATH=/usr/bin:$PATH`, when a login shell starts, then PATH is set

---

## USS-108: Core UNIX Utilities

**User Value:** Standard UNIX utilities enable file management, text processing, and system administration from the command line.

### USS-108.1: File Utilities (ls, cp, mv, rm, mkdir, cat, find)

**As a** UNIX user, **I want** standard file management commands.

**Acceptance Criteria:**
- Given `ls -la /data`, when called, then all files are listed with permissions, owner, size, and modification time
- Given `cp -r /src /dst`, when called, then the directory tree is recursively copied
- Given `find /data -name "*.txt" -type f`, when called, then all .txt files under /data are listed

### USS-108.2: Text Utilities (grep, head, tail, wc, sort)

**As a** UNIX user, **I want** standard text processing commands.

**Acceptance Criteria:**
- Given `grep -n "pattern" file.txt`, when called, then matching lines are printed with line numbers
- Given `sort -k2 -n file.txt`, when called, then lines are sorted numerically by the second field
- Given `wc -l file.txt`, when called, then the line count is printed

### USS-108.3: sed — Stream Editor

**As a** UNIX user, **I want** sed for stream editing.

**Acceptance Criteria:**
- Given `sed 's/old/new/g' file.txt`, when called, then all occurrences of "old" are replaced with "new"
- Given `sed -n '10,20p' file.txt`, when called, then lines 10-20 are printed

### USS-108.4: awk — Pattern Processing

**As a** UNIX user, **I want** awk for pattern-based text processing.

**Acceptance Criteria:**
- Given `awk '{print $1, $3}' file.txt`, when called, then fields 1 and 3 of each line are printed
- Given `awk -F: '{sum+=$3} END {print sum}' /etc/passwd`, when called, then the sum of the third field is printed

### USS-108.5: Miscellaneous Utilities (chmod, chown, date, env, echo, printf, test, expr)

**As a** UNIX user, **I want** common system utilities.

**Acceptance Criteria:**
- Given `chmod 755 script.sh`, when called, then the file permissions are changed
- Given `date +%Y-%m-%d`, when called, then the current date is printed in the specified format
- Given `test -f /etc/profile && echo yes`, when the file exists, then "yes" is printed

---

## USS-109: BPXPRMxx Configuration & Security

**User Value:** System programmers can configure USS via PARMLIB and control access via RACF OMVS segments.

### USS-109.1: BPXPRMxx PARMLIB Parser

**As a** system programmer, **I want** BPXPRMxx configuration parsed from PARMLIB, **so that** USS parameters are set at IPL.

**Acceptance Criteria:**
- Given BPXPRMxx with MAXPROCSYS(1000), MAXPROCUSER(256), MAXFILEPROC(65535), when parsed, then limits are applied
- Given FILESYSTYPE TYPE(ZFS) ENTRYPOINT(IOEFSCM), when parsed, then zFS is registered as a file system type
- Given ROOT FILESYSTEM('OMVS.ROOT') TYPE(ZFS) MODE(RDWR), when parsed, then the root file system is mounted

### USS-109.2: RACF OMVS Segment Integration

**As a** security administrator, **I want** RACF OMVS segments mapped to UNIX UIDs/GIDs, **so that** UNIX security works.

**Acceptance Criteria:**
- Given user JSMITH with OMVS segment (UID=100, HOME=/u/jsmith, PROGRAM=/bin/sh), when JSMITH is dubbed, then UID/GID/HOME/SHELL are set
- Given UNIXPRIV class profile SUPERUSER.FILESYS.MOUNT, when a user has READ access, then they can mount file systems

### USS-109.3: Superuser and Privilege Checks

**As a** system programmer, **I want** superuser (UID 0) and UNIXPRIV checks, **so that** privileged operations are controlled.

**Acceptance Criteria:**
- Given a process with euid=0, when it calls chown(), then the call succeeds regardless of file ownership
- Given BPX.DAEMON in FACILITY class, when a daemon calls setuid(), then it can change identity

### USS-109.4: Security and Configuration Tests

**Acceptance Criteria:**
- Given BPXPRMxx test fixtures, when parsed, then all parameters are correctly applied
- Given `cargo test -p open-mainframe-uss` security tests, then all pass

---

## USS-110: Daemon Infrastructure & Codepage Services

**User Value:** USS daemons provide essential services (network super-server, scheduling, logging) and codepage auto-conversion enables seamless ASCII/EBCDIC handling.

### USS-110.1: inetd Super-Server

**As a** system programmer, **I want** inetd to manage network service daemons, **so that** services are started on demand.

**Acceptance Criteria:**
- Given /etc/inetd.conf with `telnet stream tcp nowait root /usr/sbin/telnetd`, when a connection arrives on port 23, then telnetd is spawned

### USS-110.2: cron Scheduler and syslogd

**As a** system programmer, **I want** cron for scheduled tasks and syslogd for system logging.

**Acceptance Criteria:**
- Given crontab entry `0 * * * * /usr/local/bin/cleanup.sh`, when the hour changes, then the script runs
- Given syslog(LOG_ERR, "disk full"), when called, then the message is written to the configured log file

### USS-110.3: Codepage Auto-Conversion and File Tagging

**As a** UNIX developer, **I want** _BPXK_AUTOCVT and file tagging, **so that** ASCII files are auto-converted on read/write.

**Acceptance Criteria:**
- Given _BPXK_AUTOCVT=ON and a file tagged as ISO8859-1 (CCSID 819), when read by an EBCDIC program, then content is auto-converted
- Given chtag -t -c ISO8859-1 file.txt, when called, then the file is tagged with the specified codepage
- Given iconv("IBM-1047", "ISO8859-1", data), when called, then EBCDIC data is converted to ASCII

---

## FR/NFR Coverage Matrix

| FR | Stories |
|----|---------|
| FR-USS-001 | USS-100.1, USS-100.2, USS-100.3, USS-100.4, USS-100.5, USS-100.6 |
| FR-USS-002 | USS-101.1, USS-101.2, USS-101.3, USS-101.4, USS-101.5 |
| FR-USS-003 | USS-102.1, USS-102.2, USS-102.3, USS-102.5, USS-102.6 |
| FR-USS-004 | USS-103.1, USS-103.2, USS-103.3, USS-103.4 |
| FR-USS-005 | USS-104.1, USS-104.2, USS-104.3, USS-104.4, USS-104.5 |
| FR-USS-006 | USS-105.1, USS-105.2, USS-105.3, USS-105.4, USS-105.5 |
| FR-USS-007 | USS-106.1, USS-106.2, USS-106.3, USS-106.4, USS-106.5, USS-106.6 |
| FR-USS-008 | USS-107.1, USS-107.2, USS-107.3, USS-107.4, USS-107.5, USS-107.6 |
| FR-USS-009 | USS-108.1, USS-108.2, USS-108.3, USS-108.4, USS-108.5 |
| FR-USS-010 | USS-109.1 |
| FR-USS-011 | USS-109.2, USS-109.3 |
| FR-USS-012 | USS-110.3 |
| FR-USS-013 | USS-100.7 |
| FR-USS-014 | USS-110.1, USS-110.2 |

| NFR | Stories |
|-----|---------|
| NFR-USS-001 | USS-100.4 |
| NFR-USS-002 | USS-102.3, USS-102.8 |
| NFR-USS-003 | All USS-100 through USS-108 |
| NFR-USS-004 | USS-102.7 |

**Coverage: 14/14 FRs (100%), 4/4 NFRs (100%)**
