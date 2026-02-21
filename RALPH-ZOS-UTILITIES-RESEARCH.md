# Ralph Loop Prompt: z/OS Utilities & Subsystems — Comprehensive Research

## Mission

You are conducting **comprehensive research** on all z/OS utilities, subsystems, services, and system programs that a real IBM z/OS system provides — to produce a complete inventory and gap analysis for **OpenMainframe**, an open-source Rust reimplementation of z/OS.

**Goal:** Produce a definitive catalog of every z/OS component that needs to exist to make OpenMainframe a credible, production-capable z/OS-like platform. This research feeds into BMAD planning (PRD → Architecture → Epics) for future implementation.

**21 gap analysis batches exist.** Your job is to go **deeper and wider** — finding components, utilities, services, and system behaviors that the existing gap analyses missed, then producing a unified research document that covers the complete z/OS surface area.

---

## Iteration Protocol

**Before doing ANY work, determine where you are:**

1. Check the progress tracker file: `_bmad-output/zos-research-progress.md`
   - If it does not exist, create it (see Progress Tracker Format below)
   - If it exists, read it to find the current research area and step

2. Find the **first research area with status `pending` or `in_progress`** and work on it

3. For each research area:
   - Search the web for IBM z/OS documentation, IBM Knowledge Center, and Redbooks
   - Catalog every utility, service, command, and system program in that area
   - Cross-reference against what OpenMainframe already has (check `crates/` directory)
   - Cross-reference against existing gap analyses in `_bmad-output/gap-analysis/`
   - Document findings in the research output file

4. After completing a research area:
   - Update the progress tracker: mark area `complete`, set next area to `in_progress`
   - Append findings to the research output document

5. If ALL research areas are `complete`:
   - Produce the final unified inventory and priority assessment
   - Output the completion promise

---

## Existing Codebase Context

### Already Implemented (24 crates)

| Crate | What It Covers |
|-------|---------------|
| `open-mainframe-lang-core` | AST, spans, diagnostics framework |
| `open-mainframe-encoding` | EBCDIC encoding (21 code pages) |
| `open-mainframe-cobol` | COBOL compiler with LLVM codegen |
| `open-mainframe-jcl` | JCL parser + executor with utility registry |
| `open-mainframe-rexx` | REXX interpreter (basic) |
| `open-mainframe-hlasm` | High-Level Assembler (basic) |
| `open-mainframe-pli` | PL/I parser (basic) |
| `open-mainframe-runtime` | Language Environment runtime (basic) |
| `open-mainframe-dataset` | VSAM, QSAM, PDS, GDG, catalog, IDCAMS |
| `open-mainframe-jes2` | JES2 job queue, spool, job lifecycle |
| `open-mainframe-racf` | RACF user/group/profile, SAF auth |
| `open-mainframe-tso` | TSO session, commands, REXX integration |
| `open-mainframe-ispf` | ISPF panels, tables, editor |
| `open-mainframe-wlm` | Workload Manager (basic) |
| `open-mainframe-smf` | SMF records (basic) |
| `open-mainframe-sort` | DFSORT (SORT/MERGE/COPY, INCLUDE/OMIT, INREC/OUTREC, SUM) |
| `open-mainframe-cics` | CICS transaction processing |
| `open-mainframe-ims` | IMS/DB (DL/I database) |
| `open-mainframe-db2` | DB2 SQL + EXEC SQL |
| `open-mainframe-mq` | IBM MQ messaging |
| `open-mainframe-tui` | TN3270E terminal server |
| `open-mainframe-deploy` | K8s, Docker, container orchestration |
| `open-mainframe-zosmf` | z/OSMF REST API server (Zowe-compatible) |
| `open-mainframe-complexity` | Code complexity analysis |

### Existing Gap Analyses (21 batches)

| Batch | Coverage |
|-------|----------|
| 1 | REXX |
| 2 | HLASM |
| 3 | PL/I |
| 4 | Easytrieve |
| 5 | CLIST |
| 6 | Natural |
| 7 | FOCUS |
| 8 | RACF |
| 9 | TSO/ISPF |
| 10 | IBM MQ |
| 11 | JES2 |
| 12 | Language Environment |
| 13 | IMS TM + MFS |
| 14 | SMF |
| 15 | IDMS |
| 16 | ADABAS |
| 17 | WLM |
| 18 | z/OS UNIX (USS) |
| 19 | DFSMS + Catalog |
| 20 | Networking |
| 21 | Utilities (IEBGENER, IEBCOPY, DFSORT, etc.) |
| 22 | Priority Matrix (summary) |

---

## Research Areas

The following areas must be researched. Each area should identify **every** utility, command, service, SVC, system program, and callable service that a real z/OS system provides in that domain.

### AREA-1: MVS System Services & Supervisor Call Instructions (SVCs)

Research all z/OS MVS system services:
- **SVCs** — all numbered SVC calls (SVC 0 through SVC 255) and their functions
- **PC routines** — program call entry points for system services
- **Authorized services** — MODESET, TESTAUTH, cross-memory services
- **System macros** — WTO, WTOR, WTL, GETMAIN, FREEMAIN, STORAGE, ATTACH, DETACH, POST, WAIT, ENQ, DEQ, STIMER, TIME, STIMERM
- **Recovery** — ESTAE, ESPIE, SETRP, FRR, percolation model
- **Storage management** — CSA, SQA, LSQA, private area, subpools, TCB/RB chain
- **Dispatchable units** — task management (TCB), SRB, IRB, address spaces, enclaves
- **Cross-memory** — AXSET, SSAR, PC/PT, data spaces, hiperspaces

**IBM References:**
- z/OS MVS Programming: Authorized Assembler Services Reference (SA23-1372)
- z/OS MVS Programming: Assembler Services Reference (SA23-1369)
- z/OS MVS System Codes (SA38-0665)

### AREA-2: z/OS System Commands & Operator Console

Research all operator console commands:
- **MVS commands** — START, STOP, MODIFY, DISPLAY, CANCEL, FORCE, HALT, QUIESCE, REPLY, ROUTE, SET, SETPROG, SETOMVS
- **JES2 commands** — $DA, $DJ, $DQ, $DP, $DU, $S, $P, $C, $A, $T, $VS
- **System display commands** — D A, D ASM, D C, D ETR, D GRS, D IOS, D J, D M, D NET, D OMVS, D PROD, D R, D SMF, D SMS, D SR, D T, D U, D WLM, D XCF
- **SDSF** — System Display and Search Facility (panels, commands, REXX interface)
- **Automation** — MPF (Message Processing Facility), automated operations, NetView
- **Hardware commands** — CF (Coupling Facility), HMC, SE commands
- **Console types** — MCS, SMCS, extended MCS, subsystem consoles

**IBM References:**
- z/OS MVS System Commands (SA38-0666)
- z/OS JES2 Commands (SA32-0994)

### AREA-3: Program Management & Loader Services

Research program loading, linking, and binder:
- **Binder (IEWL/IEWBLINK)** — link-edit, create load modules/program objects
- **Program management services** — CSVQUERY, CSVDYNEX, dynamic LPA, STEPLIB/JOBLIB
- **Loader** — LOAD macro, LINK macro, XCTL, ATTACH with PGM=
- **Load module format** — CESD, RLD, TXT records, scatter/load, RMODE, AMODE
- **PDSE program objects** — differences from load modules
- **Program properties table** — PPT entries, authorized programs
- **LPA (Link Pack Area)** — MLPA, FLPA, PLPA, dynamic LPA management
- **Catalog search order** — STEPLIB, JOBLIB, LPA, LINKLIB, LNKLST

**IBM References:**
- z/OS MVS Program Management: User's Guide and Reference (SA23-1393)
- z/OS MVS Program Management: Advanced Facilities (SA23-1392)

### AREA-4: Data Management Beyond DFSMS

Research data management components not covered in Batch 19:
- **Access methods** — BSAM, QSAM, BPAM, VSAM, OAM, EXCP, BDAM
- **Tape management** — RMM, CA-1/TLMS, tape mount automation, SMS tape classes
- **DFSMS components** — DFSMShsm (HSM), DFSMSrmm, DFSMSopt (optical), DFSMStvs (transactional VSAM)
- **Striped datasets** — extended format, large format, extended addressability
- **PDSEs** — program library vs data library, member generations
- **zFS file system** — zFS aggregates, mount points, zfsadm commands
- **HFS** — deprecated but still referenced, migration to zFS
- **DADSM** — Direct Access Device Space Management (VTOC, VVDS management)
- **Checkpoint/Restart** — CHKPT macro, automatic restart, deferred restart, RD parameter

**IBM References:**
- z/OS DFSMS Using Data Sets (SC23-6855)
- z/OS DFSMShsm Managing Your Own Data (SC23-6870)
- z/OS DFSMS Access Method Services for Catalogs (SC23-6853)

### AREA-5: JES2 Deep Dive — Beyond Basic Job Management

Research JES2 features beyond what Batch 11 covers:
- **NJE (Network Job Entry)** — RSCS, HASP, multi-system job networking
- **JES2 initialization** — HASPPARM, JES2 PROCLIB, warm/cold/hot start
- **JES2 exits** — EXIT1 through EXIT255, installation exits for job processing
- **SPOOL management** — SPOOL volumes, offloading, checkpoint datasets
- **Output management** — PSF (Print Services Facility), JES2 FSS, remote printers
- **JES2 MAS (Multi-Access Spool)** — shared spool, member-level processing
- **Execution batch monitor** — JES2 batch scheduling
- **JESMSG routing** — JESMSGLG, JESJCL, JESYSMSG, message routing
- **JES3** — differences from JES2 (converter/interpreter, dependent job control, main device scheduler)

**IBM References:**
- z/OS JES2 Initialization and Tuning Guide (SA32-0992)
- z/OS JES2 Installation Exits (SA32-0993)

### AREA-6: RACF Deep Dive — Beyond Basic Security

Research RACF features beyond what Batch 8 covers:
- **General resource classes** — FACILITY, OPERCMDS, CONSOLE, SURROGAT, STARTED, DSNR, CSFKEYS, CSFSERV
- **RACF database** — RACF database structure, backup/recovery, database utilities
- **Digital certificates** — RACDCERT, key rings, certificate management
- **Security labels** — MLS (Multi-Level Security), security labels, categories
- **Delegated authority** — CLAUTH, group-level administration, SPECIAL/OPERATIONS/AUDITOR
- **PassTickets** — PassTicket generation, application authentication
- **ICSF integration** — cryptographic services, key management
- **ACF2/TopSecret** — awareness of alternative SAF products and their command syntax differences
- **z/OS MFA** — Multi-Factor Authentication
- **RACF audit** — SMF type 80 records, event logging, report utilities (DSMON, IRRUT100, IRRUT200, IRRUT400)

**IBM References:**
- z/OS Security Server RACF Security Administrator's Guide (SA23-2289)
- z/OS Security Server RACF Command Language Reference (SA23-2292)

### AREA-7: Batch Processing Ecosystem

Research the complete batch processing ecosystem:
- **IBM batch utilities not in Batch 21** — IEFBR14 (done), ADRDSSU/DFDSS (dump/restore), IEHDASDR (DASD init), ICKDSF (DASD init/format)
- **Compilers as batch programs** — IGYCRCTL (COBOL), IEV90 (HLASM), IBMZPLI (PL/I), IKFCBL00 (OS COBOL)
- **Pre/post processors** — DFSORT (done), SYNCSORT (compatible), FILEAID/FILE-AID, ABEND-AID
- **Report writers** — CA-DELIVER, CONTROL-D, INFOPAC, XPEDITER
- **Scheduling** — CA-7, TWS/OPC (Tivoli Workload Scheduler), Control-M, ZEKE
- **Tape utilities** — IEHINITT (initialize tapes), IEBPTPCH (done), tape copy utilities
- **GDG management** — automatic model DSCB, GDG limit processing, IDCAMS DEFINE GDG
- **SYSOUT management** — JES2 output processing, external writer, PSF, remote printing
- **Restart/recovery** — checkpoint/restart (CHKPT), deferred restart, automatic restart management (ARM)

**IBM References:**
- z/OS DFSMSdfp Utilities (SC23-6864)
- z/OS MVS JCL Reference (SA23-1385)

### AREA-8: TSO/ISPF Deep Dive

Research TSO/ISPF features beyond what Batch 9 covers:
- **TSO commands not yet cataloged** — ALLOCATE, FREE, ATTRIB, CALL, EXEC, LINK, LOADGO, RUN, LISTDS, LISTCAT, LISTALC, STATUS, SUBMIT, CANCEL, OUTPUT, SEND, RECEIVE, PROFILE, LOGON, LOGOFF, RENAME, DELETE, EDIT (line mode), TEST (debug)
- **ISPF services** — BROWSE, EDIT, VIEW, UTILITIES, DSLIST, MEMBER, MOVE/COPY, DATA SET, RESET, SETTINGS
- **ISPF options** — Option 1 (Browse), 2 (Edit), 3 (Utilities), 4 (Foreground), 5 (Batch), 6 (Command), 7 (Dialog Test), 8 (LM Utilities), 9 (IBM Products), 10 (SCLM), 11 (Workplace)
- **ISPF programming services** — ISPEXEC (SELECT, DISPLAY, VGET/VPUT, TBOPEN/TBCLOSE/TBADD/TBGET/TBSCAN, FTOPEN/FTINCL/FTCLOSE, BRIF/EDIF/VIIF, LMINIT/LMOPEN/LMGET/LMPUT)
- **ISPF editor commands** — line commands (I, D, C, M, R, A, B, X, F, L, )), primary commands (FIND, CHANGE, EXCLUDE, RESET, SORT, SUBMIT, SAVE, CANCEL, COPY, MOVE, CREATE, REPLACE)
- **PDF (Program Development Facility)** — SuperC (ISRSUPC), Search-For (ISRSFIND), compare utilities
- **ISPF configuration** — ISPF.CONF, allocation, concatenation, ISPF table libraries
- **ISPF WorkPlace** — Object/Action model

**IBM References:**
- z/OS ISPF User's Guide (SC19-3627)
- z/OS ISPF Services Guide (SC19-3626)
- z/OS ISPF Dialog Developer's Guide and Reference (SC19-3619)

### AREA-9: z/OS UNIX System Services (USS) Complete Inventory

Research USS completeness beyond Batch 18:
- **Kernel services** — BPX1xxx/BPX4xxx callable services, syscall table
- **Shell utilities** — /bin/sh, awk, grep, sed, ls, cp, mv, rm, mkdir, chmod, chown, tar, pax, cpio, make, cc (xlc)
- **POSIX compliance** — threads (pthreads), signals, IPC (semaphores, shared memory, message queues)
- **z/OS-specific USS** — BPXBATCH, tsocmd, mvscmd, opercmd, extattr, chtag, oget/oput/ocopy
- **Network services** — inetd, ftpd, sshd, rlogind, named, sendmail
- **File systems** — zFS, HFS, TFS (temporary), NFS client/server, automount
- **Security** — USS UID/GID mapping to RACF, OMVS segment, UNIXPRIV class, superuser
- **Java/Node.js/Python** — z/OS runtime environments, ASCII/EBCDIC considerations
- **ISHELL** — ISPF shell interface to USS

**IBM References:**
- z/OS UNIX System Services Command Reference (SA23-2280)
- z/OS UNIX System Services Programming: Assembler Callable Services Reference (SA23-2281)

### AREA-10: System Logger, Coupling Facility & Sysplex

Research sysplex and shared services:
- **System Logger** — log streams, IXGLOGR, coupling facility log streams, DASD-only log streams
- **Coupling Facility** — CF structures, cache structures, lock structures, list structures
- **XCF (Cross-System Coupling Facility)** — XCF groups, members, signaling
- **GRS (Global Resource Serialization)** — ENQ/DEQ, RESERVE, ring/star mode, contention management
- **Parallel Sysplex** — shared DASD, multi-system operations, sysplex-wide workload balancing
- **VTAM** — SNA, APPN, Enterprise Extender, VTAM definitions
- **Automatic Restart Manager (ARM)** — policy, element registration, restart management
- **System Automation** — SA z/OS, NetView, automated operations

**IBM References:**
- z/OS MVS Setting Up a Sysplex (SA23-1399)
- z/OS MVS Programming: Sysplex Services Guide (SA23-1400)

### AREA-11: Debugging, Dump Analysis & Problem Determination

Research diagnostic tools:
- **IPCS** — Interactive Problem Control System (dump analysis, VERBEXIT, formatting)
- **SVC dump** — SDUMP macro, SYS1.DUMPxx datasets, dump suppression
- **SNAP dump** — SNAP macro, in-program diagnostics
- **SYSABEND/SYSUDUMP/SYSMDUMP** — DD name dump types
- **GTF (Generalized Trace Facility)** — system tracing, event selection
- **Component trace** — CTRACE, component-specific tracing
- **Logrec** — hardware/software error recording, EREP reports
- **Health Checker** — IBM Health Checker for z/OS, user-defined checks
- **RMF (Resource Measurement Facility)** — performance monitoring, reports, Monitor I/II/III
- **ABEND codes** — system (Sxxx) and user (Uxxx) abend codes, reason codes
- **Language Environment debugging** — CEEDUMP, LE runtime options (TRAP, TERMTHDACT, STORAGE)

**IBM References:**
- z/OS MVS IPCS User's Guide (SA23-1384)
- z/OS MVS Diagnosis: Reference (GA32-0904)
- z/OS MVS System Codes (SA38-0665)

### AREA-12: Compilers, Preprocessors & Language Toolchain

Research the complete compilation toolchain:
- **COBOL** — Enterprise COBOL compiler options, copybooks (COPY/REPLACE), CICS/DB2/SQL preprocessors, compiler-directing statements
- **HLASM** — assembler options, macro libraries, conditional assembly, ADATA
- **PL/I** — Enterprise PL/I compiler, include processing, preprocessors
- **C/C++** — XL C/C++ for z/OS, POSIX compliance, DLL support, IPA (Inter-Procedural Analysis)
- **Java** — IBM Semeru Runtime, z/OS-specific extensions
- **REXX** — REXX compiler (optional), compiled REXX
- **Binder** — IEWL/IEWBLINK options, binder API, side decks, DLL support, program objects
- **Debug tools** — IBM Debug for z/OS (formerly Debug Tool), CEDF (CICS), INSPECT
- **Code coverage** — IBM Application Discovery, IBM zUnit

**IBM References:**
- Enterprise COBOL for z/OS Programming Guide (SC27-8712)
- z/OS XL C/C++ User's Guide (SC09-4765)

### AREA-13: DB2, IMS & Data Server Ecosystem

Research database subsystem completeness:
- **DB2 for z/OS** — SQL DDL/DML, stored procedures, triggers, UDFs, LOBs, XML, temporal tables, ZPARM, buffer pools, utilities (LOAD, UNLOAD, REORG, RUNSTATS, COPY, RECOVER)
- **DB2 bind** — BIND PLAN, BIND PACKAGE, DBRM, precompiler integration
- **IMS DB** — DL/I calls (GU, GN, GNP, GHU, GHN, ISRT, DLET, REPL), PCB/PSB, DBD
- **IMS TM** — MFS, message queues, MPP/BMP/IFP region types, IMS Connect
- **QMF** — Query Management Facility (interactive SQL, report formatting)
- **SPUFI** — SQL Processing Using File Input (ISPF interface to DB2)
- **DSNTEP2/DSNTEP4** — DB2 batch SQL processors
- **DSNTIAD** — DB2 batch dynamic SQL processor
- **DSNUPROC** — DB2 utility JCL procedure

**IBM References:**
- Db2 for z/OS SQL Reference (SC27-8855)
- Db2 for z/OS Utility Guide and Reference (SC27-8810)
- IMS Application Programming Guide (SC18-9698)

### AREA-14: CICS Deep Dive

Research CICS features beyond what the existing crate covers:
- **CICS commands** — EXEC CICS (SEND MAP, RECEIVE MAP, READ, WRITE, REWRITE, DELETE, BROWSE, LINK, XCTL, RETURN, START, RETRIEVE, ASKTIME, FORMATTIME, WRITEQ TD/TS, READQ, ENQ, DEQ, SYNCPOINT)
- **BMS (Basic Mapping Support)** — DFHMSD, DFHMDI, DFHMDF macros, map generation
- **CICS web services** — CICS web support, URIMAP, PIPELINE, JSON/XML transformation
- **CICS resources** — FILE, PROGRAM, TRANSACTION, TDQUEUE, TSQUEUE, MAPSET, ENQMODEL
- **CSD (CICS System Definition)** — DFHCSDUP, CEDA/CEDB/CEDC transactions
- **CICS monitoring** — CICS statistics, monitoring data, SMF 110 records
- **CICS DB2** — CICS-DB2 attachment facility, DSNCRCT, DB2CONN/DB2ENTRY/DB2TRAN
- **CICS MQ** — CICS-MQ adapter, CKQC connection
- **CEMT/CEDA/CEDF** — master terminal, resource definition, execution diagnostic facility

**IBM References:**
- CICS Transaction Server for z/OS Application Programming Reference (SC34-7022)
- CICS Transaction Server for z/OS System Programming Reference (SC34-7023)

### AREA-15: System Initialization & Configuration (IPL to Ready)

Research the z/OS boot process and system configuration:
- **IPL process** — nucleus load, NIP (Nucleus Initialization Program), master scheduler initialization
- **Parmlib** — SYS1.PARMLIB members: IEASYSxx, IEASLPxx, COMMNDxx, MPFLSTxx, PROGxx, SCHEDxx, SMFPRMxx, IKJTSOxx, LPALSTxx, LNKLSTxx, APFLSTxx
- **PROCLIB** — SYS1.PROCLIB, started task JCL procedures
- **Started tasks** — JES2, VTAM, TSO, TCPIP, RRS, workload manager, OMVS
- **System datasets** — SYS1.LINKLIB, SYS1.LPALIB, SYS1.NUCLEUS, SYS1.PROCLIB, SYS1.PARMLIB, SYS1.MACLIB, SYS1.MODGEN, SYS1.SAMPLIB
- **Catalog structure** — master catalog, user catalogs, alias definitions, catalog search order
- **APF authorization** — Authorized Program Facility list, APF-authorized libraries
- **Linklist** — LNKLST concatenation, dynamic linklist update (SETPROG)
- **PPT/SCHEDxx** — Program Properties Table, scheduling parameters

**IBM References:**
- z/OS MVS Initialization and Tuning Guide (SA23-1379)
- z/OS MVS Initialization and Tuning Reference (SA23-1380)

### AREA-16: IBM MQ Deep Dive

Research MQ features beyond what Batch 10 covers:
- **MQ commands** — MQSC commands (DEFINE/ALTER/DELETE/DISPLAY QLOCAL/QREMOTE/QALIAS/QMODEL/CHANNEL/PROCESS/NAMELIST/AUTHINFO)
- **Channel types** — SDR, RCVR, SVR, RQSTR, CLUSSDR, CLUSRCVR, SVRCONN, CLNTCONN
- **MQ API** — MQCONN, MQOPEN, MQPUT, MQGET, MQINQ, MQSET, MQCLOSE, MQDISC, MQSUB, MQSUBRQ
- **Trigger monitoring** — CKTI, trigger messages, initiation queues
- **Dead letter queue** — DLQ handler (CSQUDLQH)
- **MQ security** — OAM (Object Authority Manager), channel authentication records
- **MQ clustering** — cluster queues, workload balancing, cluster channels
- **Publish/subscribe** — topics, subscriptions, retained publications
- **MQ client** — client connections, client channel definition table (CCDT)

**IBM References:**
- IBM MQ for z/OS documentation (SC34-6946)

### AREA-17: SNA, VTAM & Network Subsystems

Research legacy and modern networking:
- **VTAM** — VTAM definitions (APPL, LU, PU, NCP), session management, LOGMODE
- **SNA** — LU types (LU0, LU1, LU2, LU6.2), conversation protocols
- **TN3270** — TN3270E protocol (RFC 2355), extended attributes, device types
- **TCP/IP** — z/OS Communications Server, TCPIP PROFILE, PORT reservations, VIPADYNAMIC
- **Enterprise Extender** — SNA over IP, HPR/IP
- **AT-TLS** — Application Transparent TLS, policy rules
- **FTP server/client** — z/OS FTP customization, JES interface, MVS/HFS mode
- **SSH/SFTP** — z/OS OpenSSH configuration
- **NFS** — z/OS NFS server/client
- **SMTP** — z/OS SMTP server

**IBM References:**
- z/OS Communications Server: IP Configuration Reference (SC27-3651)
- z/OS Communications Server: SNA Network Implementation Guide (SC27-3672)

### AREA-18: Crypto, PKI & Security Infrastructure

Research security infrastructure:
- **ICSF** — Integrated Cryptographic Service Facility, CSFPCI, CSFENC/CSFDEC, CSFHSK
- **Crypto Express** — hardware crypto accelerators, clear key/secure key
- **PKI** — z/OS PKI Services, certificate authority, certificate lifecycle
- **LDAP** — z/OS LDAP server (IBM Tivoli Directory Server)
- **Kerberos** — Network Authentication Service
- **z/OS encryption** — dataset encryption, encryption key management
- **Pervasive encryption** — z14+ pervasive encryption, protected key
- **RACF digital certificates** — key rings, certificate utilities, RACDCERT commands

**IBM References:**
- z/OS Cryptographic Services ICSF Application Programmer's Guide (SA23-2231)
- z/OS Security Server RACF Security Administrator's Guide (SA23-2289)

### AREA-19: SMF Complete Record Type Inventory

Research all SMF record types:
- **Job-related** — Types 4, 5, 30 (job/step), 26 (JES2)
- **Dataset** — Types 14, 15, 17, 18, 60-66 (VSAM), 42 (SMS)
- **Security** — Type 80 (RACF), 83 (RACF data set)
- **TSO** — Types 32, 33, 34, 40, 41
- **DB2** — Types 100, 101, 102
- **CICS** — Type 110
- **MQ** — Type 116
- **RMF** — Types 70-79
- **TCP/IP** — Types 118, 119
- **System** — Types 0, 1, 2, 3, 6, 7, 8, 9, 19, 20, 21, 22, 23, 24, 25, 28, 29, 35, 36, 37, 38, 39, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 54, 56, 58, 59, 67, 68, 69, 84, 85, 86, 88, 89, 90, 92

**IBM References:**
- z/OS MVS System Management Facilities (SMF) (SA38-0667)

### AREA-20: Miscellaneous z/OS Components

Research remaining z/OS components not covered above:
- **RRS (Resource Recovery Services)** — two-phase commit coordination
- **Transactional VSAM (TVS)** — VSAM record-level sharing with RRS
- **System REXX** — AXR address space, system-level REXX execution
- **z/OS Management Facility (z/OSMF)** — beyond REST API: workflows, software management, network configuration, capacity provisioning
- **IBM Application Discovery** — program analysis, impact analysis
- **IBM Z Service Management** — OPERLOG, SYSLOG, log management
- **Infoprint Server** — print management, AFP/IPDS printing
- **BookManager** — online documentation
- **z/OS Container Extensions (zCX)** — Docker containers on z/OS
- **z/OS Connect EE** — RESTful API creation from CICS/IMS/DB2 programs
- **IBM Z Open Automation Utilities** — modern z/OS automation API
- **IBM z/OS Debugger** — interactive debug facility
- **REXX ISPF Edit Macros** — ISREDIT interface
- **SDSF REXX interface** — programmatic access to SDSF functions
- **RACF ICHEINTY/RACROUTE** — internal SAF macros

**IBM References:**
- z/OS product documentation library

---

## Output Format

### Research Output File

Create `_bmad-output/zos-complete-inventory.md` with this structure for each area:

```markdown
## AREA-N: [Title]

### Components Inventory

| Component | Type | Description | In OpenMainframe? | In Gap Analysis? | Priority |
|-----------|------|-------------|-------------------|-----------------|----------|
| NAME | utility/service/command/SVC | Brief description | YES/PARTIAL/NO | Batch N or N/A | HIGH/MED/LOW |

### New Gaps Discovered (not in existing gap analyses)

- **Component Name** — description of what it does and why OpenMainframe needs it
  - Classification: [utility|service|command|SVC|callable_service|system_program]
  - IBM Reference: [doc number or URL]
  - Complexity: [S|M|L|XL]
  - Priority: [Critical|High|Medium|Low]
  - Dependencies: [list of dependent components]

### Recommendations for OpenMainframe

[Specific recommendations for what to implement and in what order]
```

### Final Summary Section

After all areas are complete, produce a final summary:

```markdown
## Final Summary: z/OS Component Census

### Total Components Cataloged: N
### Already in OpenMainframe: N (N%)
### In Existing Gap Analyses: N (N%)
### Newly Discovered Gaps: N

### Top 20 Missing Components by Priority
[Ranked list with justification]

### Recommended New Epics
[New epics that should be added to the priority matrix beyond the existing 214]

### Updated Phase Recommendations
[Adjustments to the Phase A-F priority ordering from Batch 22]
```

---

## Progress Tracker Format

Create `_bmad-output/zos-research-progress.md` with this format:

```markdown
---
currentArea: AREA-1
areasComplete: 0
areasTotal: 20
lastVerified: ""
---

# z/OS Complete Inventory — Research Progress

## Research Areas
- [ ] AREA-1: MVS System Services & SVCs
- [ ] AREA-2: System Commands & Operator Console
- [ ] AREA-3: Program Management & Loader Services
- [ ] AREA-4: Data Management Beyond DFSMS
- [ ] AREA-5: JES2 Deep Dive
- [ ] AREA-6: RACF Deep Dive
- [ ] AREA-7: Batch Processing Ecosystem
- [ ] AREA-8: TSO/ISPF Deep Dive
- [ ] AREA-9: z/OS UNIX (USS) Complete Inventory
- [ ] AREA-10: System Logger, Coupling Facility & Sysplex
- [ ] AREA-11: Debugging, Dump Analysis & Problem Determination
- [ ] AREA-12: Compilers, Preprocessors & Language Toolchain
- [ ] AREA-13: DB2, IMS & Data Server Ecosystem
- [ ] AREA-14: CICS Deep Dive
- [ ] AREA-15: System Initialization & Configuration
- [ ] AREA-16: IBM MQ Deep Dive
- [ ] AREA-17: SNA, VTAM & Network Subsystems
- [ ] AREA-18: Crypto, PKI & Security Infrastructure
- [ ] AREA-19: SMF Complete Record Type Inventory
- [ ] AREA-20: Miscellaneous z/OS Components
```

---

## Web Research Strategy

For each area, use these search strategies:
1. **IBM Knowledge Center** — search `site:ibm.com/docs/en/zos` for the specific area
2. **IBM Redbooks** — search `site:redbooks.ibm.com` for implementation guides
3. **z/OS product list** — IBM z/OS V2R5 product documentation
4. **MVS 3.8j reference** — for historical utility documentation (Hercules community)
5. **Zowe docs** — for any API exposure of the components

**Important:** When searching, use queries like:
- `"z/OS" "[component name]" reference guide`
- `"z/OS 2.5" "[utility name]" documentation`
- `IBM z/OS "[service name]" programming guide`

---

## Rules

1. **Depth over breadth** — For each area, catalog EVERY component, not just the common ones
2. **Cross-reference everything** — Check against existing codebase AND existing gap analyses
3. **Cite IBM docs** — Every component should have an IBM documentation reference
4. **Classify clearly** — Each component gets a type (utility/service/command/SVC/callable_service)
5. **Priority with rationale** — Every priority assignment must have a one-sentence justification
6. **No duplicates** — If a component appears in multiple areas, list it once with cross-references
7. **Real-world focus** — Prioritize components that real z/OS shops use daily over obscure features
8. **Implementation feasibility** — Note which components require hardware emulation vs pure software

---

## Completion Promise

When ALL 20 research areas are complete and the final summary is produced:

```
Z/OS UTILITIES RESEARCH COMPLETE
```

Signal completion with the promise tag containing the text above.
