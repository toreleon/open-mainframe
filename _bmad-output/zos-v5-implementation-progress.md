---
currentIG: IG-5
currentEpic: DFSMS-105
epicsComplete: 22
epicsTotal: 162
storiesComplete: 127
storiesTotal: 949
lastUpdated: "2026-02-23"
---

# z/OS v5.0 Implementation Progress

## Phase A — Foundation Layer

### IG-1: MVS System Services (NEW: open-mainframe-mvs)
- [x] Scaffold crate
- [x] SYS-100: MVS System Services Core (8 stories)
- [x] SYS-101: ESTAE/ESPIE Recovery & Extended Services (8 stories)

### IG-2: RACF Extensions (EXTEND: open-mainframe-racf)
- [x] SYS-109: General Resource Class Framework (7 stories)
- [x] SYS-110: PassTicket & Digital Certificate Mgmt (7 stories)

### IG-3: JES2 Extensions (EXTEND: open-mainframe-jes2)
- [x] SYS-108: JES2 Installation Exits Framework (6 stories)

### IG-4: Utilities & DFSORT (NEW: open-mainframe-utilities)
- [x] Scaffold crate
- [x] UTIL-110: Utility Framework (4 stories)
- [x] UTIL-100: IEBCOPY (6 stories)
- [x] UTIL-101: IEBGENER Enhancements (4 stories)
- [x] UTIL-102: IEBCOMPR (4 stories)
- [x] UTIL-103: IEBUPDTE (4 stories)
- [x] UTIL-104: DFSORT Enhancements (7 stories)
- [x] UTIL-105: IEBPTPCH (4 stories)
- [x] UTIL-106: IEBDG (4 stories)
- [x] UTIL-107: IEH System Utilities (6 stories)
- [x] UTIL-108: AMASPZAP (6 stories)
- [x] UTIL-109: TSO/REXX/USS Batch (6 stories)

### IG-5: DFSMS, HSM & Catalog (EXTEND: open-mainframe-dataset)
- [x] DFSMS-100: SMS Construct Data Model (7 stories)
- [x] DFSMS-101: ACS Routine Interpreter (7 stories)
- [x] DFSMS-102: ICF Catalogs (9 stories)
- [x] DFSMS-103: IDCAMS Extensions (6 stories)
- [x] DFSMS-104: DFSMShsm Migration (7 stories)
- [ ] DFSMS-105: DFSMShsm Backup (5 stories)
- [ ] DFSMS-106: DFSMSdss DUMP/RESTORE (6 stories)
- [ ] DFSMS-107: DFSMSrmm Tape Mgmt (5 stories)
- [ ] DFSMS-108: PDSE & Member Generations (5 stories)
- [ ] DFSMS-109: Space & Volume Mgmt (5 stories)
- [ ] DFSMS-110: GDG-to-ICF Integration (4 stories)

### IG-6: ABEND Framework (EXTEND: open-mainframe-runtime)
- [ ] SYS-111: ABEND Code Framework & Dump Support (8 stories)

## Phase B — Interactive & Scripting

### IG-7: System Commands & Console (NEW: open-mainframe-syscmd)
- [ ] Scaffold crate
- [ ] SYS-102: System Command Dispatcher & DISPLAY (9 stories)
- [ ] SYS-103: SDSF (9 stories)

### IG-8: Program Management & Binder (NEW: open-mainframe-pgmmgmt)
- [ ] Scaffold crate
- [ ] SYS-104: Binder & Program Execution (10 stories)
- [ ] SYS-105: Object & Load Module Format (6 stories)

### IG-9: WLM Extensions (EXTEND: open-mainframe-wlm)
- [ ] WLM-100: Service Definition Data Model (7 stories)
- [ ] WLM-101: Classification Rule Engine (6 stories)
- [ ] WLM-102: Goal Evaluation & PI (6 stories)
- [ ] WLM-103: Resource Groups & Capping (5 stories)
- [ ] WLM-104: WLM-Managed Initiator Scheduling (6 stories)
- [ ] WLM-105: Enclave Framework (5 stories)
- [ ] WLM-106: IWM Services API (7 stories)
- [ ] WLM-107: Scheduling & App Environments (5 stories)
- [ ] WLM-108: Service Definition Persistence (4 stories)
- [ ] WLM-109: Operator Commands & Monitoring (5 stories)
- [ ] WLM-110: WLM Health API (6 stories)

## Phase C — Middleware & Communications

### IG-10: USS & POSIX (NEW: open-mainframe-uss)
- [ ] Scaffold crate
- [ ] USS-100: POSIX Process Model (7 stories)
- [ ] USS-101: Signal Handling (5 stories)
- [ ] USS-102: zFS Hierarchical File System (8 stories)
- [ ] USS-103: Directory & File Metadata (5 stories)
- [ ] USS-104: Pthreads (5 stories)
- [ ] USS-105: IPC Mechanisms (6 stories)
- [ ] USS-106: POSIX Sockets (6 stories)
- [ ] USS-107: UNIX Shell (/bin/sh) (6 stories)
- [ ] USS-108: Core UNIX Utilities (5 stories)
- [ ] USS-109: BPXPRMxx Config & Security (4 stories)
- [ ] USS-110: Daemon Infrastructure (3 stories)

### IG-11: Networking & TCP/IP (NEW: open-mainframe-networking)
- [ ] Scaffold crate
- [ ] NET-100: VTAM Application Interface (5 stories)
- [ ] NET-101: SNA LU Type Support (4 stories)
- [ ] NET-102: APPC / LU 6.2 / CPI-C (6 stories)
- [ ] NET-103: TCP/IP Stack Configuration (5 stories)
- [ ] NET-104: Sockets API Compatibility (6 stories)
- [ ] NET-105: AT-TLS & Security (5 stories)
- [ ] NET-106: FTP Client/Server (6 stories)
- [ ] NET-107: SSH Server & TN3270E (5 stories)
- [ ] NET-108: Sysplex Networking (5 stories)
- [ ] NET-109: IP Filtering & IPSec (5 stories)

### IG-12: SMF Extensions (EXTEND: open-mainframe-smf)
- [ ] SMF-100: SMF Record Infrastructure (5 stories)
- [ ] SMF-101: SMFPRMxx Configuration (4 stories)
- [ ] SMF-102: SMF Record Writing API (5 stories)
- [ ] SMF-103: SMF Exit Framework (5 stories)
- [ ] SMF-104: Type 30 — Job Accounting (6 stories)
- [ ] SMF-105: Types 14/15/17/18 — Dataset Activity (5 stories)
- [ ] SMF-106: Type 80 — Security Audit (5 stories)
- [ ] SMF-107: Types 70-79 — Performance (5 stories)
- [ ] SMF-108: Types 100-120 — Subsystem (5 stories)
- [ ] SMF-109: SMF Dump Utilities (5 stories)
- [ ] SMF-110: Observability Bridge (4 stories)

### IG-13: COBOL Precompilers (NEW: open-mainframe-precompilers)
- [ ] Scaffold crate
- [ ] SYS-112: DB2 COBOL Precompiler (8 stories)
- [ ] SYS-113: CICS COBOL Precompiler (6 stories)

### IG-14: CICS BMS & Extensions (EXTEND: open-mainframe-cics)
- [ ] SYS-116: CICS Web Services & REST/JSON (7 stories)
- [ ] CICS-210: DOCUMENT Commands (5 stories)
- [ ] CICS-211: CICS System Programming (6 stories)

## Phase D — Extended Languages

### IG-15: CLIST (NEW: open-mainframe-clist)
- [ ] Scaffold crate
- [ ] CL-100: Lexer & Parser (5 stories)
- [ ] CL-101: Interpreter Core (6 stories)
- [ ] CL-102: Built-in Functions (4 stories)
- [ ] CL-103: I/O and Error Handling (6 stories)
- [ ] CL-104: TSO/ISPF Integration (6 stories)

### IG-16: System Init & PARMLIB (NEW: open-mainframe-parmlib)
- [ ] Scaffold crate
- [ ] SYS-117: PARMLIB Framework & Core Members (8 stories)
- [ ] SYS-118: System Symbol Substitution (4 stories)
- [ ] SYS-119: Subsystem Configuration Members (7 stories)
- [ ] SYS-120: Initialization Sequence & Operator Commands (5 stories)

### IG-17: Crypto, PKI & Security (NEW: open-mainframe-crypto + EXTEND: open-mainframe-racf)
- [ ] Scaffold crate (open-mainframe-crypto)
- [ ] CRYPTO-100: ICSF Symmetric & Hashing (6 stories)
- [ ] CRYPTO-101: ICSF Asymmetric Keys (5 stories)
- [ ] CRYPTO-102: Key Management & Stores (6 stories)
- [ ] CRYPTO-103: CSFKEYS/CSFSERV Integration (4 stories)
- [ ] SEC-107: Security Labels (MAC) (5 stories)
- [ ] SEC-108: RACF Audit & SMF Integration (6 stories)
- [ ] SEC-109: RACF Exits, Utilities & Config (6 stories)

## Phase E — Transaction Monitors & Databases

### IG-18: IMS TM & MFS (EXTEND: open-mainframe-ims)
- [ ] IMS-TM100: Alt PCB & Extended Messages (5 stories)
- [ ] IMS-TM101: Conversational Transactions (4 stories)
- [ ] IMS-TM102: Advanced System Service Calls (5 stories)
- [ ] IMS-TM103: Environment Query Calls (4 stories)
- [ ] IMS-TM104: Region Model & Scheduling (6 stories)
- [ ] IMS-TM105: Operator Command Framework (6 stories)
- [ ] IMS-TM106: OTMA Protocol (5 stories)
- [ ] IMS-TM107: IMS Connect Gateway (6 stories)
- [ ] IMS-TM108: Fast Path (EMH + IFP) (4 stories)
- [ ] IMS-TM109: MSC & Shared Queues (4 stories)
- [ ] MFS-100: MFS Source Language Parser (5 stories)
- [ ] MFS-101: MFS Control Block Compiler (5 stories)
- [ ] MFS-102: MFS Runtime Integration (5 stories)
- [ ] IMS-TM110: EXEC DLI Code Generation (4 stories)

### IG-19: DB2 Utilities & BIND (EXTEND: open-mainframe-db2)
- [ ] SYS-114: DB2 BIND & Package Mgmt (7 stories)
- [ ] SYS-115: DB2 Operational Utilities (5 stories)

### IG-20: Easytrieve (NEW: open-mainframe-easytrieve)
- [ ] Scaffold crate
- [ ] EZ-100: Lexer & Parser (5 stories)
- [ ] EZ-101: Interpreter Core (5 stories)
- [ ] EZ-102: File Processing (5 stories)
- [ ] EZ-103: Report Generator (6 stories)
- [ ] EZ-104: SQL/Database Integration (4 stories)
- [ ] EZ-105: SORT & Utilities (4 stories)
- [ ] EZ-106: Macros, External Calls & Tests (4 stories)

### IG-21: IDMS (NEW: open-mainframe-idms)
- [ ] Scaffold crate
- [ ] IDMS-100: CODASYL Data Model Core (5 stories)
- [ ] IDMS-101: Schema & Subschema DDL Parser (5 stories)
- [ ] IDMS-102: Navigational DML Engine (7 stories)
- [ ] IDMS-103: Currency Indicator System (4 stories)
- [ ] IDMS-104: COBOL DML Precompiler (4 stories)
- [ ] IDMS-105: DMCL & Physical Storage (6 stories)
- [ ] IDMS-106: IDMS-DC Transaction Processing (6 stories)
- [ ] IDMS-107: ADS/Online 4GL (4 stories)
- [ ] IDMS-108: SQL Option (5 stories)
- [ ] IDMS-109: Recovery & Operations (5 stories)
- [ ] IDMS-110: Lock Management (5 stories)

### IG-22: ADABAS (NEW: open-mainframe-adabas)
- [ ] Scaffold crate
- [ ] ADA-100: Inverted-List Storage Engine (6 stories)
- [ ] ADA-101: FDT & Field System (5 stories)
- [ ] ADA-102: Descriptor Engine (5 stories)
- [ ] ADA-103: Direct Call Interface (ACB) (5 stories)
- [ ] ADA-104: Search Commands (5 stories)
- [ ] ADA-105: Read Commands (6 stories)
- [ ] ADA-106: Modification Commands (4 stories)
- [ ] ADA-107: Transaction Management (5 stories)
- [ ] ADA-108: Nucleus & Logging (6 stories)
- [ ] ADA-109: Utilities & DDM (5 stories)

## Phase F — Niche & Legacy

### IG-23: Natural (NEW: open-mainframe-natural)
- [ ] Scaffold crate
- [ ] NAT-100: Lexer & Parser (6 stories)
- [ ] NAT-101: Data Model & DEFINE DATA (5 stories)
- [ ] NAT-102: Interpreter Core (5 stories)
- [ ] NAT-103: Data Manipulation (5 stories)
- [ ] NAT-104: ADABAS Database Access (6 stories)
- [ ] NAT-105: SQL Database Access (4 stories)
- [ ] NAT-106: Output & Reporting (5 stories)
- [ ] NAT-107: Interactive I/O & Maps (5 stories)
- [ ] NAT-108: System Variables & Functions (5 stories)
- [ ] NAT-109: Error Handling & Work Files (4 stories)
- [ ] NAT-110: Environment & Security (5 stories)

### IG-24: FOCUS (NEW: open-mainframe-focus)
- [ ] Scaffold crate
- [ ] FOC-100: Multi-Dialect Parser (6 stories)
- [ ] FOC-101: Master File Descriptor (5 stories)
- [ ] FOC-102: TABLE Request Engine (6 stories)
- [ ] FOC-103: GRAPH Engine (4 stories)
- [ ] FOC-104: MODIFY/MAINTAIN Engine (5 stories)
- [ ] FOC-105: Dialogue Manager (5 stories)
- [ ] FOC-106: Built-in Functions (5 stories)
- [ ] FOC-107: Data Adapters (6 stories)
- [ ] FOC-108: Output Formatting (5 stories)
- [ ] FOC-109: Joins & Multi-Source (4 stories)
- [ ] FOC-110: Mainframe Integration (4 stories)

## Summary
- Implementation groups: 24
- New crates to scaffold: 14
- Existing crates to extend: 10
- Total epics: 162
- Total stories: 949
- Epics complete: 19
- Stories complete: 105
