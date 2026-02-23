# Gap Analysis: z/OS UNIX System Services (USS)

## Official Specification Summary

z/OS UNIX System Services (z/OS UNIX, informally USS) is a base element of z/OS that provides a POSIX-compliant UNIX operating environment within the z/OS operating system. First introduced as OpenEdition MVS in MVS/ESA SP 4.3, USS is the first UNIX 95-certified operating system not derived from AT&T source code. It enables UNIX applications from other platforms to run on IBM System z mainframes, often requiring only a recompile.

USS is classified as **Core Infrastructure** on z/OS:
- Required by z/OS Management Facility (z/OSMF), OpenSSH, IBM HTTP Server, z/OS SDK for Java, XML services, z/OS PKI services
- Provides a hierarchical file system (zFS), POSIX process model, pthreads, sockets, IPC, and a full shell environment
- Integrates with RACF for security (OMVS segment, UNIXPRIV class)
- Bridges MVS and UNIX worlds via BPXWDYN, BPXBATCH, and `//'dataset.name'` syntax
- Assembler callable services use BPX1xxx (31-bit AMODE 31) and BPX4xxx (64-bit AMODE 64) naming conventions
- Enhanced ASCII support with automatic codepage conversion (_BPXK_AUTOCVT, chtag, iconv)
- Daemon infrastructure (inetd, cron, syslogd) managed through /etc/rc and BPX.DAEMON FACILITY class

Key documentation:
- **z/OS UNIX System Services Planning (GA32-0884)** -- system setup, BPXPRMxx, file system configuration
- **z/OS UNIX System Services User's Guide (SA23-2279)** -- shell usage, file operations, tcsh
- **z/OS UNIX System Services Command Reference (SA23-2280)** -- all USS commands
- **z/OS UNIX System Services Programming: Assembler Callable Services Reference (SA23-2281)** -- BPX1xxx/BPX4xxx services
- **z/OS XL C/C++ Runtime Library Reference (SA22-7821)** -- POSIX C functions on z/OS
- **z/OS Communications Server: IP Sockets Application Programming Interface Guide (SC27-3660)** -- sockets
- **z/OS Security Server RACF Security Administrator's Guide (SA23-2289)** -- OMVS segment, UNIXPRIV

## Key Features & Capabilities

### 1. POSIX Compliance & Standards Certification

| Standard | Status | Notes |
|----------|--------|-------|
| POSIX.1 (IEEE 1003.1) | Supported | System interfaces -- process, file, signal, I/O |
| POSIX.2 (IEEE 1003.2) | Supported | Shell and utilities specification |
| XPG4 (X/Open Portability Guide 4) | Certified | OS/390 OpenEdition certified by X/Open; IBM conformance document available |
| XPG4.2 | Certified | Extended XPG4; basis for UNIX 95 brand |
| UNIX 95 (SUS v1) | Certified | z/OS 1.2+ is registered as UNIX 95 compliant with The Open Group |
| UNIX 98 (SUS v2) | Not certified | z/OS has not been formally certified at this level |
| UNIX 03 (SUS v3, POSIX.1-2001) | Aligned (partial) | z/OS 1.9+ progressively adds UNIX 03 features; not formally certified |

Key points:
- z/OS is the **first UNIX 95 operating system not derived from AT&T source code**
- The Open Group certifies z/OS as a compliant UNIX operating system
- IBM published the **OS/390 OpenEdition XPG4 Conformance Document** covering XPG4 Internationalized System Calls and Libraries Extended, and XPG4 Commands and Utilities
- Progressive UNIX 03 alignment began with z/OS 1.9 (September 2007) and continues through current releases

### 2. Process Model

#### Process Lifecycle Callable Services

| Function | BPX1 (31-bit) | BPX4 (64-bit) | Description |
|----------|---------------|---------------|-------------|
| fork | BPX1FRK | BPX4FRK | Create a new process (duplicate calling process) |
| exec | BPX1EXC | BPX4EXC | Run a program (replace process image) |
| execmvs | BPX1EXM | BPX4EXM | Run an MVS program specifically |
| spawn | BPX1SPN | BPX4SPN | Spawn a process (combined fork+exec) |
| wait | BPX1WAT | BPX4WAT | Wait for a child process to end |
| waitpid | BPX1WTE | BPX4WTE | Wait for a specific child process |
| kill | BPX1KIL | BPX4KIL | Send a signal to a process |
| getpid | BPX1GPI | BPX4GPI | Get the process ID |
| getppid | BPX1GPP | BPX4GPP | Get the parent process ID |
| getpgrp | BPX1GPG | BPX4GPG | Get process group ID |
| setpgid | BPX1SPG | BPX4SPG | Set process group ID for job control |
| setsid | BPX1SSI | BPX4SSI | Create session and set process group ID |
| attach_exec | BPX1ATX | BPX4ATX | Attach a z/OS UNIX program |
| attach_execmvs | BPX1ATM | BPX4ATM | Attach an MVS program |

#### Process Model Architecture

| Concept | Description |
|---------|-------------|
| Address space | Each fork()/spawn() creates a new MVS address space via BPXAS initiator |
| BPXAS | Started task PROC in SYS1.PROCLIB; manages address spaces for forked/spawned processes |
| _BPX_SHAREAS | When set to YES, spawn() reuses parent address space (lightweight, TCB-based) |
| Process ID (PID) | Every UNIX process has a unique PID; tracked by the kernel |
| Process group | Set of related processes for job control; managed via setpgid()/getpgrp() |
| Session | Collection of process groups; created via setsid(); session leader has no controlling terminal |
| Dubbing | First UNIX syscall from an MVS task "dubs" it as a USS process, creating UNIX identity |

#### Signal Handling

| Structure/Macro | Purpose |
|-----------------|---------|
| BPXYPPSD | Maps signal delivery data |
| BPXYSINF | Maps the SIGINFO_T structure |
| BPXYINHE | Maps the spawn inheritance structure (signal disposition for spawned children) |
| sigaction | Set signal handler (BPX1SA/BPX4SA) |
| sigprocmask | Block/unblock signals (BPX1SPM/BPX4SPM) |
| sigsuspend | Wait for a signal (BPX1SSU/BPX4SSU) |

Standard POSIX signals are supported: SIGHUP, SIGINT, SIGQUIT, SIGILL, SIGABRT, SIGFPE, SIGKILL, SIGSEGV, SIGPIPE, SIGALRM, SIGTERM, SIGUSR1, SIGUSR2, SIGCHLD, SIGSTOP, SIGTSTP, SIGCONT, SIGTTIN, SIGTTOU.

### 3. File System

#### File System Types

| Type | Status | Description |
|------|--------|-------------|
| zFS (z/OS File System) | Current/Primary | POSIX-conforming hierarchical file system stored in zFS data sets (linear VSAM); supports multi-file-system aggregates |
| HFS (Hierarchical File System) | Deprecated | Legacy file system type; migration to zFS via BPXWMIGF command |
| TFS (Temporary File System) | Available | Memory-based file system for /tmp; high performance, non-persistent |

#### Standard Directory Structure

| Directory | Purpose | Sharing |
|-----------|---------|---------|
| / (root) | Root of the hierarchical file system; sysplex root in shared FS | Sysplex-wide |
| /bin | System binaries and utilities (sh, ls, cat, etc.) | Shared (version FS) |
| /dev | Character special files for terminal devices; created dynamically at IPL | System-specific |
| /etc | System configuration data (/etc/profile, /etc/resolv.conf, etc.) | System-specific |
| /tmp | Temporary data for products and applications; optionally TFS-backed | System-specific |
| /var | Variable runtime data (logs, spool, caches) | System-specific |
| /usr | Shared read-only binaries, libraries, documentation | Shared (version FS) |
| /u or /home | User home directories | Shared or per-system |

System-specific directories (/etc, /dev, /tmp, /var) cannot be shared across systems in a sysplex; they are mount points in the system-specific file system data set.

#### File System Operations

| Operation | Command / Callable Service | Description |
|-----------|---------------------------|-------------|
| Mount | `mount` / TSO MOUNT / BPXPRMxx MOUNT | Mount a zFS data set at a mount point |
| Unmount | `unmount` / TSO UNMOUNT | Unmount a file system |
| chmod | `chmod` / BPX1CHM/BPX4CHM | Change file permission bits (symbolic or octal) |
| chown | `chown` / BPX1CHO/BPX4CHO | Change file owner and group |
| chattr | BPX1CHR/BPX4CHR | Change file attributes (extended) |
| stat | `ls -l` / BPX1STA/BPX4STA | Get file status information |
| Symbolic link | `ln -s` / BPX1SYM/BPX4SYM | Create a symbolic link |
| Hard link | `ln` / BPX1LNK/BPX4LNK | Create a hard link |
| mkfifo | `mkfifo` / BPX1MKD | Create a FIFO (named pipe) special file |
| mknod | `mknod` | Create a FIFO or character special file |
| Migrate HFS->zFS | `BPXWMIGF` | Migrate legacy HFS to zFS |

#### File Types Supported

| Type | ls indicator | Description |
|------|-------------|-------------|
| Regular file | `-` | Ordinary data files |
| Directory | `d` | Container for other files |
| Symbolic link | `l` | Pointer to another path name |
| Hard link | (none) | Additional directory entry for existing inode |
| FIFO (named pipe) | `p` | IPC named pipe, created via mkfifo |
| Character special | `c` | Device files in /dev (terminals, pseudo-TTYs) |
| Socket | `s` | AF_UNIX domain sockets |

### 4. Shell & Command Environment

#### Available Shells

| Shell | Path | Provider | Notes |
|-------|------|----------|-------|
| z/OS shell (sh) | /bin/sh | IBM (shipped) | Default POSIX shell; Korn shell-compatible |
| tcsh (C shell) | /bin/tcsh | IBM (shipped) | Enhanced C shell; shipped with z/OS |
| bash | /bin/bash (typical) | Rocket Software (free download) | GNU Bash port for z/OS; also shipped with IzODA |

#### Shell Access Methods

| Method | Description |
|--------|-------------|
| TSO OMVS | Interactive shell from TSO; runs /bin/sh or configured shell |
| BPXBATCH | JCL program to run shell commands, scripts, or executables in batch |
| OSHELL | Invokes BPXBATCH from TSO/E |
| rlogin/telnet | Remote login to USS shell via network |
| SSH (OpenSSH) | Secure shell access; preferred method for remote USS access |

#### BPXBATCH (JCL-to-USS Bridge)

BPXBATCH enables running USS programs and shell commands from JCL:

```
//STEP1  EXEC PGM=BPXBATCH,PARM='SH /path/to/script.sh'
//STDIN  DD PATH='/dev/null'
//STDOUT DD PATH='/tmp/out.txt',
//          PATHOPTS=(OWRONLY,OCREAT,OTRUNC),
//          PATHMODE=(SIRUSR,SIWUSR)
//STDERR DD PATH='/tmp/err.txt',
//          PATHOPTS=(OWRONLY,OCREAT,OTRUNC),
//          PATHMODE=(SIRUSR,SIWUSR)
```

BPXBATCH modes:
| Mode | PARM syntax | Description |
|------|-------------|-------------|
| SH | `PARM='SH command'` | Run command under /bin/sh |
| PGM | `PARM='PGM /path/to/program args'` | Run an executable directly (no shell) |

Related: COZBATCH is an alternative batch program similar to BPXBATCH.

#### REXX-USS Bridge: bpxwunix()

The `bpxwunix()` function is a REXX wrapper for fork()/exec() that lets REXX programs invoke USS commands and capture output.

### 5. Pthreads (POSIX Threads)

#### Thread Management Callable Services

| Function | BPX1 (31-bit) | BPX4 (64-bit) | Description |
|----------|---------------|---------------|-------------|
| pthread_create | BPX1PTC | BPX4PTC | Create a new thread |
| pthread_exit_and_get | BPX1PTX | BPX4PTX | Exit thread and get a new one |
| pthread_detach | BPX1PTD | BPX4PTD | Detach a thread (no join needed) |
| pthread_join | BPX1PTJ | BPX4PTJ | Wait for a thread to terminate |
| pthread_kill | BPX1PTK | BPX4PTK | Send a signal to a specific thread |
| pthread_cancel | BPX1PTB | BPX4PTB | Cancel (request termination of) a thread |
| pthread_self | BPX1PTS | BPX4PTS | Query the current thread ID |
| pthread_quiesce | BPX1PTQ | BPX4PTQ | Quiesce threads in a process |
| pthread_setintr | BPX1PSI | BPX4PSI | Examine and change interrupt state |
| pthread_setintrtype | BPX1PST | BPX4PST | Examine and change interrupt type |
| pthread_tag_np | BPX1PTT | BPX4PTT | Tag thread with user data (z/OS extension) |
| pthread_security_np | BPX1TLS | BPX4TLS | Create/delete thread-level security (z/OS extension) |

#### Synchronization Primitives

| Primitive | C Functions | Description |
|-----------|-------------|-------------|
| Mutex | pthread_mutex_init, pthread_mutex_lock, pthread_mutex_trylock, pthread_mutex_unlock, pthread_mutex_destroy | Mutual exclusion lock; only one thread can hold at a time |
| Condition variable | pthread_cond_init, pthread_cond_wait, pthread_cond_timedwait, pthread_cond_signal, pthread_cond_broadcast, pthread_cond_destroy | Allow threads to wait for events/conditions; must be used with a mutex |
| Read-write lock | pthread_rwlock_init, pthread_rwlock_rdlock, pthread_rwlock_wrlock, pthread_rwlock_tryrdlock, pthread_rwlock_trywrlock, pthread_rwlock_unlock, pthread_rwlock_destroy | Multiple readers OR one exclusive writer; higher parallelism than mutex |

#### Thread-Specific Data

| Function | Description |
|----------|-------------|
| pthread_key_create | Create a thread-specific data key with optional destructor |
| pthread_getspecific | Get the thread-specific value for a key |
| pthread_setspecific | Set the thread-specific value for a key |
| pthread_key_delete | Delete a thread-specific data key |

z/OS-specific extensions:
- **pthread_security_np** (BPX1TLS/BPX4TLS) -- create or delete thread-level security; allows individual threads to run under different RACF identities
- **pthread_tag_np** (BPX1PTT/BPX4PTT) -- tag a thread with application-specific data

### 6. Sockets API

#### Address Families

| Family | Constant | Description |
|--------|----------|-------------|
| IPv4 | AF_INET | TCP/UDP over IPv4; SOCK_STREAM (TCP), SOCK_DGRAM (UDP), SOCK_RAW |
| IPv6 | AF_INET6 | TCP/UDP over IPv6; basic and subset of advanced features |
| UNIX domain | AF_UNIX | Local IPC via filesystem-based socket paths; no network protocol |

#### Socket Callable Services

| Function | BPX1 (31-bit) | BPX4 (64-bit) | Description |
|----------|---------------|---------------|-------------|
| socket | BPX1SOC | BPX4SOC | Create a socket or socket pair |
| bind | BPX1BND | BPX4BND | Bind a local name/address to a socket |
| listen | BPX1LSN | BPX4LSN | Prepare server socket to queue connections |
| accept | BPX1ACP | BPX4ACP | Accept incoming connection request |
| connect | BPX1CON | BPX4CON | Establish connection to remote socket |
| send | BPX1SND | BPX4SND | Send data on a connected socket |
| sendto | BPX1STO | BPX4STO | Send data to a specific destination |
| sendmsg | BPX1SMS | BPX4SMS | Send message with ancillary data |
| recv | BPX1RCV | BPX4RCV | Receive data from a socket |
| recvfrom | BPX1RFM | BPX4RFM | Receive data with sender address |
| recvmsg | BPX1RMS | BPX4RMS | Receive message with ancillary data |
| select | BPX1SEL | BPX4SEL | Monitor activity on multiple file descriptors |
| poll | BPX1POL | BPX4POL | Monitor activity on multiple file descriptors and message queues |
| getaddrinfo | (C library) | (C library) | Resolve hostname/service to socket addresses (IPv4/IPv6) |
| getsockopt | BPX1OPT | BPX4OPT | Get socket options |
| setsockopt | BPX1OPT | BPX4OPT | Set socket options |
| shutdown | BPX1SHT | BPX4SHT | Shut down socket send/receive |
| close | BPX1CLO | BPX4CLO | Close a socket descriptor |

Socket APIs are provided via two interfaces:
1. **z/OS UNIX Assembler Callable Services** -- BPX1xxx/BPX4xxx generalized call-based interface
2. **z/OS C sockets** -- POSIX/XPG4-compliant C library functions for application development

### 7. BPX Callable Services (BPX1xxx / BPX4xxx)

#### Naming Convention

| Prefix | AMODE | Description |
|--------|-------|-------------|
| BPX1xxx | 31-bit (AMODE 31) | 31-bit interface for assembler, COBOL, PL/I callers |
| BPX4xxx | 64-bit (AMODE 64) | 64-bit interface for programs using 64-bit addressing |

#### Core File I/O Services

| Function | BPX1 | BPX4 | Description |
|----------|------|------|-------------|
| open | BPX1OPN | BPX4OPN | Open a file |
| openat | BPX1OAT | BPX4OAT | Open file relative to directory FD |
| close | BPX1CLO | BPX4CLO | Close a file descriptor |
| read | BPX1RED | BPX4RED | Read from a file or socket |
| write | BPX1WRT | BPX4WRT | Write to a file or socket |
| readv | BPX1RDV | BPX4RDV | Read into scatter buffer |
| writev | BPX1WRV | BPX4WRV | Write from gather buffer |
| lseek | BPX1LSK | BPX4LSK | Reposition read/write offset |
| stat | BPX1STA | BPX4STA | Get file status by path name |
| fstat | BPX1FST | BPX4FST | Get file status by file descriptor |
| lstat | BPX1LST | BPX4LST | Get file status (no symlink follow) |
| statvfs | BPX1STV | BPX4STV | Get file system status |
| chattr | BPX1CHR | BPX4CHR | Change file attributes |
| chmod | BPX1CHM | BPX4CHM | Change file mode (permissions) |
| chown | BPX1CHO | BPX4CHO | Change file owner/group |
| access | BPX1ACC | BPX4ACC | Check file accessibility |
| rename | BPX1REN | BPX4REN | Rename a file or directory |
| unlink | BPX1UNL | BPX4UNL | Remove a directory entry |
| link | BPX1LNK | BPX4LNK | Create a hard link |
| symlink | BPX1SYM | BPX4SYM | Create a symbolic link |
| readlink | BPX1RDL | BPX4RDL | Read a symbolic link value |
| mkdir | BPX1MKD | BPX4MKD | Create a directory |
| rmdir | BPX1RMD | BPX4RMD | Remove a directory |
| opendir | BPX1OPD | BPX4OPD | Open a directory for reading |
| readdir | BPX1RDD | BPX4RDD | Read an entry from a directory |
| closedir | BPX1CLD | BPX4CLD | Close a directory stream |
| fcntl | BPX1FCT | BPX4FCT | File control (locks, flags, etc.) |
| ioctl | BPX1IOC | BPX4IOC | Device control |
| dup/dup2 | BPX1DUP | BPX4DUP | Duplicate a file descriptor |
| pipe | BPX1PIP | BPX4PIP | Create an unnamed pipe |
| umask | BPX1UMK | BPX4UMK | Set file mode creation mask |

#### BPXWDYN -- Dynamic Allocation from USS

BPXWDYN is a text interface to a subset of SVC 99 (dynamic allocation) and SVC 109 (dynamic output) services, designed for invocation from REXX and high-level languages:

| Operation | Description |
|-----------|-------------|
| ALLOC | Dynamic allocation of a dataset or file |
| FREE | Dynamic unallocation (deallocation) |
| CONCAT | Dynamic concatenation of datasets |
| INFO | Retrieve allocation information |
| DYNOUT | Dynamic output (SVC 109) |

Enhanced entry point **BPXWDY2** preserves the invoker program mask and simplifies HLL invocation.

Example (REXX):
```rexx
address syscall
"bpxwdyn alloc dd(MYFILE) dsn('USER.DATA.SET') shr"
```

Example (COBOL):
```
CALL 'BPXWDYN' USING 'ALLOC DD(MYFILE) PATH(/u/user/file.txt)
  PATHOPTS(ORDWR,OCREAT) PATHMODE(SIRWXU) FILEDATA(TEXT)'
```

### 8. /etc Configuration Files

| File | Purpose | Key Contents |
|------|---------|-------------|
| /etc/profile | System-wide shell initialization | PATH, LIBPATH, locale settings, STEPLIB, _BPXK_AUTOCVT, umask |
| /etc/resolv.conf | DNS resolver configuration | Nameserver addresses; equivalent to TCPIP.DATA for USS programs |
| /etc/hosts | Static hostname-to-IP mappings | Local name resolution supplement |
| /etc/services | Service name-to-port mappings | Maps service names (ftp, ssh, telnet) to port numbers and protocols |
| /etc/inetd.conf | inetd super-server configuration | Service name, socket type, protocol, wait/nowait, user, server path |
| /etc/syslog.conf | syslogd configuration | Log routing rules: facility.priority -> destination |
| /etc/rc | System initialization script | Starts daemons (syslogd, inetd, cron) at USS initialization |
| $HOME/.profile | Per-user shell initialization | User-specific environment variables, PATH customizations |

#### Resolver Search Order (for USS callable sockets programs)

1. GLOBALTCPIPDATA
2. /etc/resolv.conf
3. //SYSTCPD DD statement
4. userid.TCPIP.DATA
5. SYS1.TCPPARMS(TCPDATA)
6. DEFAULTTCPIPDATA
7. TCPIP.TCPIP.DATA

#### BPXPRMxx Parmlib Member

The BPXPRMxx member in SYS1.PARMLIB controls USS system-wide configuration:

| Parameter | Description |
|-----------|-------------|
| ROOT | Defines and mounts the root file system |
| MOUNT | Specifies additional file systems to mount |
| MAXPROCSYS | Maximum number of processes system-wide |
| MAXPROCUSER | Maximum number of processes per user |
| MAXFILEPROC | Maximum open files per process |
| MAXTHREADS | Maximum threads per process |
| MAXTHREADTASKS | Maximum thread tasks per address space |
| RESOLVER_PROC | Specifies the resolver started procedure |
| AUTOCVT | System-wide automatic codepage conversion setting |
| SUPERUSER | Default superuser settings |

### 9. Security (RACF Integration)

#### OMVS Segment

Every USS user requires an OMVS segment in their RACF user profile, and every group requires an OMVS segment in its RACF group profile:

| RACF Object | OMVS Field | Description |
|-------------|-----------|-------------|
| User profile | UID | Unique UNIX user identifier (0 = superuser) |
| User profile | HOME | Home directory path (e.g., /u/userid) |
| User profile | PROGRAM | Initial program/shell (e.g., /bin/sh) |
| Group profile | GID | Unique UNIX group identifier |

RACF commands:
```
ALTUSER userid OMVS(UID(AUTOUID) HOME(/u/userid) PROGRAM(/bin/sh))
ALTGROUP groupname OMVS(GID(AUTOGID))
LU userid OMVS    /* Display OMVS segment */
LG groupname OMVS /* Display group OMVS segment */
```

#### UNIXPRIV Class Profiles

The UNIXPRIV class provides granular superuser privilege delegation without requiring UID 0:

| Resource Profile | Access | Purpose |
|-----------------|--------|---------|
| SUPERUSER.FILESYS | READ | Read any local file, read/search any local directory |
| SUPERUSER.FILESYS | UPDATE | Write to any local file |
| SUPERUSER.FILESYS.CHANGEPERMS | READ | Use chmod/setfacl on any file |
| SUPERUSER.FILESYS.CHOWN | READ | Use chown to change ownership of any file |
| SUPERUSER.FILESYS.DIRSRCH | READ | Read and search any local directory |
| SUPERUSER.FILESYS.MOUNT | READ | Mount/unmount file systems (nosetuid) |
| SUPERUSER.FILESYS.MOUNT | UPDATE | Mount/unmount file systems (setuid allowed) |
| SUPERUSER.FILESYS.QUIESCE | READ | Quiesce/unquiesce file systems |
| SUPERUSER.FILESYS.PFSCTL | READ | Use pfsctl() callable service |
| SUPERUSER.FILESYS.ACLOVERRIDE | READ | ACL contents override SUPERUSER.FILESYS access |
| SUPERUSER.FILESYS.USERMOUNT | READ | Allow non-superuser file system mounting |
| SUPERUSER.FILESYS.VREGISTER | READ | Use vregister callable service |
| SUPERUSER.IPC.RMID | READ | Remove IPC resources owned by others |
| SUPERUSER.PROCESS.GETPSENT | READ | Query process information of any process |
| SUPERUSER.PROCESS.KILL | READ | Send signals to any process |
| SUPERUSER.PROCESS.PTRACE | READ | Trace any process |
| SUPERUSER.SETPRIORITY | READ | Change priority of any process |
| SUPERUSER.SHMMCV.LIMIT | READ | Override shared memory/mutex/condvar limits |
| CHOWN.UNRESTRICTED | READ | Change file ownership to any UID/GID |
| FILE.GROUPOWNER.SETGID | READ | Control group ownership inheritance |
| RESTRICTED.FILESYS.ACCESS | READ | Restricted access mode |
| SHARED.IDS | READ | Allow shared UID/GID usage |

UNIXPRIV activation:
```
SETROPTS CLASSACT(UNIXPRIV)
SETROPTS RACLIST(UNIXPRIV)
RDEFINE UNIXPRIV SUPERUSER.FILESYS.CHOWN UACC(NONE)
PERMIT SUPERUSER.FILESYS.CHOWN CLASS(UNIXPRIV) ACCESS(READ) ID(ADMIN1)
SETROPTS RACLIST(UNIXPRIV) REFRESH
```

#### Superuser Authority

| Method | Description |
|--------|-------------|
| UID 0 | Direct superuser; full access to all UNIX functions |
| BPX.SUPERUSER | FACILITY class profile; READ access grants ability to `su` to superuser mode |
| UNIXPRIV class | Granular delegation of individual superuser functions |

Best practice: Use two user IDs -- one with UID 0 for maintenance, one with non-zero UID for normal work.

#### BPX FACILITY Class Resources

| Resource | Purpose |
|----------|---------|
| BPX.DAEMON | Controls who can use setuid/seteuid/setreuid and daemon-mode spawn |
| BPX.SUPERUSER | Controls who can switch to superuser mode (su) |
| BPX.SERVER | Controls server-level security functions |
| BPX.SAFFASTPATH | Controls SAF fast path for file access checking |
| BPX.JOBNAME | Controls who can set _BPX_JOBNAME (assign MVS job names) |
| BPX.FILEATTR.APF | Controls APF-authorization of USS executables |
| BPX.FILEATTR.PROGCTL | Controls program-controlled attribute on USS executables |

#### File Permission Bits & ACLs

Standard UNIX permission model:
| Permission | Octal | Applies to |
|-----------|-------|------------|
| Read (r) | 4 | Owner / Group / Other |
| Write (w) | 2 | Owner / Group / Other |
| Execute (x) | 1 | Owner / Group / Other |
| Setuid (s) | 4000 | Execute with file owner's UID |
| Setgid (s) | 2000 | Execute with file group's GID; new files inherit parent GID |
| Sticky (t) | 1000 | Only owner can delete files in directory |

Access Control Lists (ACLs):
- Provide fine-grained access beyond standard owner/group/other bits
- Administered via `setfacl` (set) and `getfacl` (query) UNIX commands
- Checked by RACF; requires FSSEC class to be active
- ACL checking enabled by HFSACL option in RACF
- ACLs work in conjunction with permission bits (not replacement)

### 10. Interprocess Communication (IPC)

#### System V IPC Callable Services

**Shared Memory:**

| Function | BPX1 (31-bit) | BPX4 (64-bit) | Description |
|----------|---------------|---------------|-------------|
| shmget | BPX1MGT | BPX4MGT | Create or find a shared memory segment |
| shmat | BPX1MAT | BPX4MAT | Attach a shared memory segment |
| shmdt | BPX1MDT | BPX4MDT | Detach a shared memory segment |
| shmctl | BPX1MCT | BPX4MCT | Control shared memory segment (stat/remove/set) |

**Semaphores:**

| Function | BPX1 (31-bit) | BPX4 (64-bit) | Description |
|----------|---------------|---------------|-------------|
| semget | BPX1SGT | BPX4SGT | Create or find a set of semaphores |
| semop | BPX1SOP | BPX4SOP | Perform semaphore operations (wait/post) |
| semctl | BPX1SCT | BPX4SCT | Control semaphores (stat/remove/set/getval/setval) |

**Message Queues:**

| Function | BPX1 (31-bit) | BPX4 (64-bit) | Description |
|----------|---------------|---------------|-------------|
| msgget | BPX1QGT | BPX4QGT | Create or find a message queue |
| msgsnd | BPX1QSN | BPX4QSN | Send a message to a queue |
| msgrcv | BPX1QRC | BPX4QRC | Receive a message from a queue |
| msgctl | BPX1QCT | BPX4QCT | Control message queue (stat/remove/set) |

**IPC Query Service:**

| Function | BPX1 (31-bit) | BPX4 (64-bit) | Description |
|----------|---------------|---------------|-------------|
| w_getipc | BPX1GET | BPX4GET | Query interprocess communications |

**IPC Mapping Macros (Assembler):**

| Macro | Purpose |
|-------|---------|
| BPXYIPCP | Maps interprocess communication permissions structure |
| BPXYIPCQ | Maps the w_getipc query structure |
| BPXYMSG | Maps IPC message queue structures |
| BPXYSEM | Maps IPC semaphore structures |
| BPXYSHM | Maps IPC shared memory segment structures |

#### Pipes and Named Pipes

| Mechanism | Command/Service | Description |
|-----------|----------------|-------------|
| Unnamed pipe | `pipe()` / BPX1PIP/BPX4PIP | Anonymous unidirectional channel between related processes |
| Shell pipe | `cmd1 \| cmd2` | Shell-level pipe connecting stdout to stdin |
| Named pipe (FIFO) | `mkfifo` / `mknod` | Persistent filesystem entry; unrelated processes can communicate |

Named pipes (FIFOs):
- Created with `mkfifo pathname` or `mknod pathname p`
- Identified by `p` in `ls -l` file type indicator
- Must be opened at both ends (reader and writer) before I/O proceeds
- Used by FTP and other z/OS services for inter-process data transfer

### 11. Memory-Mapped Files

| Function | BPX1 (31-bit) | BPX4 (64-bit) | Description |
|----------|---------------|---------------|-------------|
| mmap | BPX1MMP | BPX4MMP | Map pages of memory (file or anonymous) |
| munmap | BPX1MUN | BPX4MUN | Unmap previously mapped addresses |
| mprotect | BPX1MPR | BPX4MPR | Set protection of memory mapping (read/write/execute) |
| msync | BPX1MSY | BPX4MSY | Synchronize memory with physical storage |

Key mapping flags:
| Flag | Description |
|------|-------------|
| MAP_SHARED | Writes visible to other processes mapping the same file; changes written to file |
| MAP_PRIVATE | Copy-on-write; changes not visible to others, not written to file |
| MAP_ANONYMOUS | Not backed by a file; used for shared memory between related processes |

z/OS-specific enhancements (APAR OA60306):
- Support for **above-the-bar storage** (64-bit virtual addresses) for memory maps
- Allows map lengths **greater than 2 GB**, alleviating below-the-bar memory constraints
- Page size alignment: offset must be a multiple of the system page size (4096 bytes)
- Applications must be modified (recompiled with BPX4MMP) to use enhanced support

### 12. USS-MVS Integration

#### Accessing MVS Datasets from USS

| Syntax | Description |
|--------|-------------|
| `//'FULLY.QUALIFIED.DSN'` | Access MVS dataset by fully qualified name from USS programs |
| `//DD:ddname` | Access by DD name (after allocation) |
| `//DSN:dataset.name(member)` | Access PDS member |

MVS dataset I/O is done via z/OS C library `fopen()` in record mode:
```c
FILE *fp = fopen("//'USER.DATA.SET'", "r");
FILE *fp = fopen("//DD:MYFILE", "rb,type=record");
```

#### BPXWDYN Dynamic Allocation

BPXWDYN provides SVC 99 dynamic allocation from USS/REXX/HLLs:

| Keyword | Description |
|---------|-------------|
| DD(name) | Specify DD name |
| DSN('dataset.name') | Specify dataset name |
| PATH(/path/to/file) | USS file path |
| PATHOPTS(flags) | File open options (ORDWR, OCREAT, etc.) |
| PATHMODE(perms) | File permissions (SIRWXU, etc.) |
| PATHDISP(normal,abnormal) | Disposition handling |
| FILEDATA(TEXT\|BINARY\|RECORD) | Data type specification |
| SHR / OLD / MOD / NEW | Dataset disposition |
| RTDDN | Return allocated DD name (REXX only) |
| RTDSN | Return dataset name (REXX only) |

#### Codepage Tagging and Conversion

| Tool/Variable | Description |
|---------------|-------------|
| chtag | Tag a file with its character encoding (e.g., `chtag -tc ISO8859-1 file`) |
| ls -T | Display file tag information |
| ls -ETl | Display extended attributes including file tags |
| iconv | Convert file encoding between codepages (e.g., ISO8859-1 <-> IBM-1047) |
| iconv -l | List all supported character encoding schemes on z/OS |
| tag command | Query and set file codepage tags |
| _BPXK_AUTOCVT | Environment variable: ON enables automatic codepage conversion on I/O |
| _CEE_RUNOPTS | FILETAG(AUTOCVT,AUTOTAG) -- LE runtime option for automatic tagging |
| _TAG_REDIR_ERR | Tag redirected stderr as text |
| _TAG_REDIR_IN | Tag redirected stdin as text |
| _TAG_REDIR_OUT | Tag redirected stdout as text |
| AUTOCVT (BPXPRMxx) | System-wide automatic conversion setting |

#### Enhanced ASCII Support

z/OS UNIX defaults to EBCDIC (IBM-1047) but supports full ASCII/UTF-8 operation:

| Encoding | CCSID | Description |
|----------|-------|-------------|
| IBM-1047 | 1047 | z/OS default EBCDIC |
| ISO8859-1 | 819 | Latin-1 ASCII |
| UTF-8 | 1208 | Unicode UTF-8 |
| ASCII | 367 | US-ASCII |

Recommended environment setup for ASCII applications:
```shell
export _BPXK_AUTOCVT="ON"
export _CEE_RUNOPTS="FILETAG(AUTOCVT,AUTOTAG) POSIX(ON)"
export _TAG_REDIR_ERR="txt"
export _TAG_REDIR_IN="txt"
export _TAG_REDIR_OUT="txt"
```

Conversion flow:
1. File is tagged with its encoding via `chtag`
2. `_BPXK_AUTOCVT=ON` enables kernel-level automatic conversion on read/write
3. Programs read/write in their native encoding; kernel converts in-flight
4. z/OS Unicode Conversion Service provides hardware-assisted codepage conversion
5. Fallback: `iconv` command for manual conversion

**Caution:** Java programs should NOT use _BPXK_AUTOCVT=ON as Java performs its own encoding handling.

#### FILEDATA Keyword (Dynamic Allocation)

| Value | Description |
|-------|-------------|
| TEXT | File contains text data; subject to codepage conversion and newline handling |
| BINARY | File contains binary data; no conversion |
| RECORD | File uses record-oriented I/O (typical for MVS datasets) |

### 13. Daemon Support

#### Core USS Daemons

| Daemon | Purpose | Configuration |
|--------|---------|---------------|
| inetd | Internet super-server; listens on configured ports and spawns service handlers | /etc/inetd.conf |
| cron | Scheduled task execution at specified dates/times | crontab files; crond |
| syslogd | System log daemon; collects and routes log messages from applications | /etc/syslog.conf |
| sshd | Secure Shell daemon (OpenSSH); provides encrypted remote access | /etc/ssh/sshd_config |

#### Daemon Startup (/etc/rc)

USS daemons are started by the /etc/rc script, which executes during USS initialization:

```shell
# Typical /etc/rc contents
_BPX_JOBNAME='SYSLOGD' /usr/sbin/syslogd -i &
_BPX_JOBNAME='INETD'   /usr/sbin/inetd /etc/inetd.conf &
_BPX_JOBNAME='CRON'    /usr/sbin/cron &
```

The ETCRC and ETCINIT procedures execute /etc/rc and log to syslog or /etc/log.

#### z/OS-Specific Daemon Environment Variables

| Variable | Values | Description |
|----------|--------|-------------|
| _BPX_JOBNAME | jobname string | Assigns a distinct MVS job name to the USS process; visible in SDSF and WLM |
| _BPX_SHAREAS | YES / NO / REUSE / MUST | Controls whether spawned processes share the parent address space |
| _BPX_BATCH_SPAWN | YES | Enables spawn optimization for batch environments |
| _BPX_USERID | userid | Set the user identity for the process |

_BPX_JOBNAME details:
- Requires READ access to BPX.JOBNAME in the FACILITY class
- Allows MVS operator commands (D OMVS,PID=) to identify the daemon
- Enables WLM classification for prioritizing daemon traffic
- Example RACF setup:
  ```
  RDEFINE FACILITY BPX.JOBNAME UACC(NONE)
  PERMIT BPX.JOBNAME CLASS(FACILITY) ACCESS(READ) ID(daemonuser)
  SETROPTS RACLIST(FACILITY) REFRESH
  ```

_BPX_SHAREAS details:
| Value | Behavior |
|-------|----------|
| YES | Spawned process shares parent address space (lightweight TCBs) |
| NO | Spawned process gets its own BPXAS address space (default) |
| REUSE | Reuse current address space if possible |
| MUST | Must share; fail if not possible |

#### BPX.DAEMON Security

| Requirement | Description |
|-------------|-------------|
| BPX.DAEMON (FACILITY) | Must be defined; controls access to setuid/seteuid/setreuid/spawn with identity change |
| UID 0 | Daemons must run as superuser (UID 0) |
| BPXROOT user | Default superuser identity (UID 0) recommended by IBM documentation |
| RACF STARTED class | Maps daemon started task names to RACF identities |

Access restrictions:
- BPX.DAEMON must be restricted to: z/OS UNIX kernel userid, system daemons (inetd, syslogd, ftpd, sshd), and authorized system software

#### syslogd Configuration

| Item | Description |
|------|-------------|
| Config file | /etc/syslog.conf |
| Socket path | /dev/log (AF_UNIX datagram socket) |
| Local mode | `syslogd -i` (disable network logging, local only) |
| Log rotation | Cron job to signal syslogd at midnight with date-stamped directories; or use automatic archive |
| Users | IBM FTPD, TELNETD, IDS, OpenSSH all log to syslogd |

**Important:** OpenSSH sshd does NOT log to the z/OS console if syslogd is not running. Always run syslogd in local mode (-i) for SSH log availability.

## IBM Documentation References

| Document | Publication Number | Description |
|----------|-------------------|-------------|
| z/OS UNIX System Services Planning | GA32-0884 | System setup, BPXPRMxx, file system, security |
| z/OS UNIX System Services User's Guide | SA23-2279 | Shell usage, tcsh, file operations |
| z/OS UNIX System Services Command Reference | SA23-2280 | All USS commands (chmod, mount, chtag, etc.) |
| z/OS UNIX System Services Programming: Assembler Callable Services Reference | SA23-2281 | BPX1xxx/BPX4xxx callable services |
| z/OS XL C/C++ Runtime Library Reference | SA22-7821 | POSIX C functions, pthreads, sockets |
| z/OS Communications Server: IP Sockets Application Programming | SC27-3660 | Socket programming, AF_INET/AF_INET6/AF_UNIX |
| z/OS Security Server RACF Security Administrator's Guide | SA23-2289 | OMVS segment, UNIXPRIV, BPX FACILITY class |
| z/OS Using REXX and z/OS UNIX System Services | SA23-2283 | REXX syscall interface, BPXWDYN |
| OS/390 OpenEdition XPG4 Conformance Document | (public.dhe.ibm.com) | XPG4 conformance details |
| IBM Redpaper: z/OS UNIX Security Fundamentals | REDP-4193 | Comprehensive security reference |
| IBM Redbook: ABCs of z/OS System Programming Vol. 9 | SG24-6989 | USS concepts, fork, spawn, IPC |

## Implementation Status

| Feature | Spec Section | Status | Notes |
|---------|-------------|--------|-------|
| **1. POSIX Compliance** | | | |
| POSIX.1 process/file/signal/IO interfaces | 1 | YES | Implemented across process, signal, zfs, directory modules |
| POSIX.2 shell and utilities | 1 | YES | shell.rs + utilities.rs |
| **2. Process Model** | | | |
| fork (BPX1FRK/BPX4FRK) | 2 | YES | process.rs: ProcessManager::fork() |
| exec (BPX1EXC/BPX4EXC) | 2 | YES | process.rs: ProcessManager::exec() |
| spawn (BPX1SPN/BPX4SPN) | 2 | YES | process.rs: ProcessManager::spawn() |
| waitpid (BPX1WTE/BPX4WTE) | 2 | YES | process.rs: ProcessManager::waitpid() |
| kill (BPX1KIL/BPX4KIL) | 2 | YES | signal.rs: SignalState + KillTarget |
| getpid/getppid | 2 | YES | process.rs: UnixProcess fields |
| setpgid (BPX1SPG/BPX4SPG) | 2 | YES | process.rs: ProcessManager::setpgid() |
| setsid (BPX1SSI/BPX4SSI) | 2 | YES | process.rs: ProcessManager::setsid() |
| Address space / BPXAS model | 2 | YES | process.rs: spawn with share_address_space |
| _BPX_SHAREAS support | 2 | YES | process.rs: SpawnAttributes::share_address_space |
| Dubbing | 2 | YES | process.rs: ProcessManager::dub() |
| sigaction (BPX1SA/BPX4SA) | 2 | YES | signal.rs: SignalState::sigaction() |
| sigprocmask (BPX1SPM/BPX4SPM) | 2 | YES | signal.rs: SignalState::sigprocmask() |
| sigsuspend (BPX1SSU/BPX4SSU) | 2 | YES | signal.rs: SignalState::sigsuspend() |
| All POSIX signals (19 signals) | 2 | YES | signal.rs: Signal enum |
| **3. File System** | | | |
| zFS file system | 3 | YES | zfs.rs: Zfs struct with full VFS |
| File types (regular, dir, symlink, FIFO, socket, char/block dev) | 3 | YES | zfs.rs: FileType enum |
| chmod/chown (BPX1CHM/BPX1CHO) | 3 | YES | directory.rs + zfs.rs permission model |
| stat | 3 | YES | directory.rs: DirectoryManager::stat() |
| Symbolic links | 3 | YES | zfs.rs: Zfs::symlink(), readlink() |
| Hard links | 3 | YES | zfs.rs: Zfs::hard_link() |
| mkfifo | 3 | YES | ipc.rs: IpcRegistry::create_fifo() |
| Mount table | 3 | YES | zfs.rs: mount_table |
| MVS dataset bridge (//'DSN' syntax) | 3 | YES | zfs.rs: MvsDatasetRef parsing |
| File locking (advisory locks) | 3 | YES | zfs.rs: FileLock, read/write lock conflicts |
| Permission checking (owner/group/other/superuser) | 3 | YES | zfs.rs: check_permission() |
| Setuid bit | 3 | YES | zfs.rs: Inode permissions |
| ACL support (setfacl/getfacl) | 9 | GAP | Fine-grained ACLs not implemented |
| **4. Shell & Command Environment** | | | |
| Shell tokenizer | 4 | YES | shell.rs: tokenize() |
| Pipeline parser | 4 | YES | shell.rs: parse_pipeline() |
| Variable expansion (${VAR}, ${VAR:-default}) | 4 | YES | shell.rs: expand_variables() |
| Job control (jobs, fg, bg) | 4 | YES | shell.rs: Job, JobState |
| Builtins (cd, export, source, exit, umask, echo, pwd) | 4 | YES | shell.rs: BuiltinCommand enum |
| Control flow (if/for/while/case) | 4 | YES | shell.rs: ControlFlow AST |
| Profile parsing (/etc/profile, ~/.profile) | 4 | YES | shell.rs: parse_profile() |
| BPXBATCH (JCL-to-USS bridge) | 4 | GAP | Not implemented (JCL execution outside USS scope) |
| **5. Pthreads** | | | |
| pthread_create (BPX1PTC/BPX4PTC) | 5 | YES | threads.rs: ThreadManager::create_thread() |
| pthread_join (BPX1PTJ/BPX4PTJ) | 5 | YES | threads.rs: ThreadManager::join_thread() |
| pthread_exit | 5 | YES | threads.rs: ThreadManager::exit_thread() |
| pthread_cancel (BPX1PTB/BPX4PTB) | 5 | YES | threads.rs: ThreadManager::cancel_thread() |
| pthread_detach (BPX1PTD/BPX4PTD) | 5 | GAP (now implemented) | threads.rs: ThreadManager::detach_thread() |
| pthread_mutex_* (Normal, Recursive, ErrorCheck) | 5 | YES | threads.rs: PthreadMutex |
| pthread_cond_* (wait, signal, broadcast) | 5 | YES | threads.rs: PthreadCond |
| pthread_rwlock_* (rdlock, wrlock, unlock) | 5 | YES | threads.rs: PthreadRwLock |
| Thread-specific data (key_create, setspecific, getspecific) | 5 | YES | threads.rs: ThreadManager key_create/set_specific/get_specific |
| pthread_security_np (BPX1TLS/BPX4TLS) | 5 | GAP (now implemented) | threads.rs: set_thread_security()/delete_thread_security() |
| pthread_tag_np (BPX1PTT/BPX4PTT) | 5 | GAP (now implemented) | threads.rs: tag_thread()/get_thread_tag() |
| **6. Sockets API** | | | |
| socket (BPX1SOC/BPX4SOC) | 6 | YES | socket.rs: SocketManager::socket() |
| bind (BPX1BND/BPX4BND) | 6 | YES | socket.rs: SocketManager::bind() |
| listen (BPX1LSN/BPX4LSN) | 6 | YES | socket.rs: SocketManager::listen() |
| accept (BPX1ACP/BPX4ACP) | 6 | YES | socket.rs: SocketManager::accept() |
| connect (BPX1CON/BPX4CON) | 6 | YES | socket.rs: SocketManager::connect() |
| send/recv (BPX1SND/BPX1RCV) | 6 | YES | socket.rs: SocketManager::send()/recv() |
| sendto/recvfrom (BPX1STO/BPX1RFM) | 6 | YES | socket.rs: SocketManager::sendto()/recvfrom() |
| select (BPX1SEL/BPX4SEL) | 6 | YES | socket.rs: SocketManager::select() |
| poll (BPX1POL/BPX4POL) | 6 | YES | socket.rs: SocketManager::poll() |
| setsockopt/getsockopt (BPX1OPT/BPX4OPT) | 6 | YES | socket.rs: SocketManager::setsockopt() |
| shutdown (BPX1SHT/BPX4SHT) | 6 | GAP (now implemented) | socket.rs: SocketManager::shutdown() |
| close (BPX1CLO/BPX4CLO) | 6 | YES | socket.rs: SocketManager::close() |
| AF_INET (IPv4) | 6 | YES | socket.rs: AddressFamily::Inet |
| AF_INET6 (IPv6) | 6 | YES | socket.rs: AddressFamily::Inet6 |
| AF_UNIX (UNIX domain) | 6 | YES | socket.rs: AddressFamily::Unix |
| SOCK_STREAM (TCP) | 6 | YES | socket.rs: SocketType::Stream |
| SOCK_DGRAM (UDP) | 6 | YES | socket.rs: SocketType::Datagram |
| SOCK_RAW | 6 | GAP (now implemented) | socket.rs: SocketType::Raw |
| sendmsg/recvmsg (BPX1SMS/BPX1RMS) | 6 | GAP | Ancillary data (cmsg) not implemented |
| getaddrinfo | 6 | GAP | DNS resolution not implemented |
| **7. BPX Callable Services** | | | |
| File I/O (open/close/read/write/lseek) | 7 | YES | zfs.rs: Zfs open/close/read/write/lseek |
| stat/fstat/lstat | 7 | YES | directory.rs + zfs.rs |
| chmod/chown/chattr | 7 | YES | directory.rs: chmod/chown |
| rename/unlink/link/symlink/readlink | 7 | YES | directory.rs + zfs.rs |
| mkdir/rmdir/opendir/readdir/closedir | 7 | YES | directory.rs: DirectoryManager |
| pipe (BPX1PIP/BPX4PIP) | 7 | YES | ipc.rs: IpcRegistry::create_pipe() |
| umask (BPX1UMK/BPX4UMK) | 7 | YES | shell.rs: umask builtin |
| BPXWDYN dynamic allocation | 7 | GAP (now implemented) | bpxwdyn.rs: BpxwdynManager (ALLOC/FREE/CONCAT/INFO) |
| **8. /etc Configuration Files** | | | |
| /etc/profile parsing | 8 | YES | shell.rs: parse_profile() |
| /etc/inetd.conf parsing | 8 | YES | daemons.rs: Inetd::load_config() |
| /etc/syslog.conf | 8 | YES | daemons.rs: Syslogd |
| $HOME/.profile | 8 | YES | shell.rs: parse_profile() |
| /etc/resolv.conf | 8 | GAP | DNS resolver config not implemented |
| /etc/hosts | 8 | GAP | Static hostname mapping not implemented |
| /etc/services | 8 | GAP | Service-to-port mapping not implemented |
| /etc/rc startup script | 8 | GAP | Daemon startup orchestration not implemented |
| **9. Security (RACF Integration)** | | | |
| OMVS segment (UID, HOME, PROGRAM) | 9 | YES | config.rs: OmvsSegment |
| OMVS group segment (GID) | 9 | YES | config.rs: OmvsGroupSegment |
| UNIXPRIV class profiles | 9 | YES | config.rs: SecurityManager, UnixPriv enum |
| Superuser authority (UID 0) | 9 | YES | config.rs: SecurityManager::is_superuser() |
| BPX.SUPERUSER facility | 9 | YES | config.rs: SecurityManager::check_facility() |
| BPX.DAEMON facility | 9 | YES | config.rs: SecurityManager::check_facility() |
| File permission bits (rwx/setuid/setgid/sticky) | 9 | YES | zfs.rs: Inode permissions |
| ACLs (setfacl/getfacl) | 9 | GAP | Fine-grained ACLs not implemented |
| **10. IPC** | | | |
| Unnamed pipes | 10 | YES | ipc.rs: IpcRegistry::create_pipe() |
| Named pipes (FIFOs) | 10 | YES | ipc.rs: IpcRegistry::create_fifo() |
| Message queues (POSIX style) | 10 | YES | ipc.rs: IpcRegistry message queue support |
| Shared memory (POSIX style) | 10 | YES | ipc.rs: IpcRegistry shared memory support |
| Semaphores (POSIX style) | 10 | YES | ipc.rs: IpcRegistry semaphore support |
| System V IPC style (shmget/shmat with keys) | 10 | GAP | Only POSIX-style IPC; no SysV key-based API |
| w_getipc query (BPX1GET/BPX4GET) | 10 | GAP | IPC query service not implemented |
| **11. Memory-Mapped Files** | | | |
| mmap (BPX1MMP/BPX4MMP) | 11 | GAP (now implemented) | mmap.rs: MmapManager::mmap() |
| munmap (BPX1MUN/BPX4MUN) | 11 | GAP (now implemented) | mmap.rs: MmapManager::munmap() |
| mprotect (BPX1MPR/BPX4MPR) | 11 | GAP (now implemented) | mmap.rs: MmapManager::mprotect() |
| msync (BPX1MSY/BPX4MSY) | 11 | GAP (now implemented) | mmap.rs: MmapManager::msync() |
| MAP_SHARED / MAP_PRIVATE / MAP_ANONYMOUS | 11 | GAP (now implemented) | mmap.rs: MapFlags enum |
| PROT_READ / PROT_WRITE / PROT_EXEC | 11 | GAP (now implemented) | mmap.rs: ProtFlags struct |
| **12. USS-MVS Integration** | | | |
| MVS dataset access (//'DSN' syntax) | 12 | YES | zfs.rs: MvsDatasetRef parsing |
| BPXWDYN dynamic allocation | 12 | GAP (now implemented) | bpxwdyn.rs: BpxwdynManager |
| Codepage tagging (chtag) | 12 | YES | daemons.rs: chtag(), FileTag |
| iconv codepage conversion | 12 | YES | daemons.rs: iconv() |
| Auto-conversion (_BPXK_AUTOCVT) | 12 | YES | daemons.rs: AutoConvertSettings, auto_convert_read() |
| FILEDATA keyword (TEXT/BINARY/RECORD) | 12 | GAP (now implemented) | bpxwdyn.rs: FileDataType enum |
| **13. Daemon Support** | | | |
| inetd super-server | 13 | YES | daemons.rs: Inetd |
| cron scheduler | 13 | YES | daemons.rs: CronSchedule, CrontabEntry |
| syslogd | 13 | YES | daemons.rs: Syslogd |
| sshd configuration | 13 | GAP | OpenSSH sshd config not implemented |
| _BPX_JOBNAME support | 13 | GAP | MVS job name assignment not implemented |
| _BPX_SHAREAS support | 13 | YES | process.rs: SpawnAttributes::share_address_space |
| **Utilities** | | | |
| grep | - | YES | utilities.rs: grep() |
| sort | - | YES | utilities.rs: sort_lines() |
| sed | - | YES | utilities.rs: sed() |
| awk | - | YES | utilities.rs: awk() |
| find | - | YES | utilities.rs: FindOptions |
| wc | - | YES | utilities.rs: wc() |
| head/tail | - | YES | utilities.rs: head()/tail() |
| test/expr | - | YES | utilities.rs: evaluate_test()/expr_eval() |
| date | - | YES | utilities.rs: format_date() |
| BPXPRMxx parsing | - | YES | config.rs: parse_bpxprm() |

### Summary

- **Total features tracked:** 108
- **Already implemented (YES):** 82
- **Newly implemented (GAP now implemented):** 14
- **Remaining gaps (GAP):** 12

Newly implemented in this review:
1. **mmap.rs** (new module) -- mmap/munmap/mprotect/msync with MAP_SHARED/MAP_PRIVATE/MAP_ANONYMOUS, ProtFlags, page alignment
2. **bpxwdyn.rs** (new module) -- BPXWDYN dynamic allocation with ALLOC/FREE/CONCAT/INFO, DD/DSN/PATH keywords, FILEDATA types
3. **socket.rs** -- Added shutdown() (SHUT_RD/SHUT_WR/SHUT_RDWR) and SOCK_RAW type
4. **threads.rs** -- Added pthread_security_np, pthread_tag_np, pthread_detach (z/OS extensions)

Remaining gaps are primarily:
- ACLs (setfacl/getfacl)
- sendmsg/recvmsg with ancillary data
- getaddrinfo DNS resolution
- /etc/resolv.conf, /etc/hosts, /etc/services parsers
- System V IPC key-based API (shmget/shmat)
- w_getipc query service
- sshd configuration
- /etc/rc daemon startup orchestration
- _BPX_JOBNAME support
- BPXBATCH JCL bridge
