# Batch 22: Priority Matrix — Prioritized Gap Backlog

## Methodology

All gaps from Batches 1–21 are ranked by three criteria:

| Criterion | Weight | Description |
|-----------|--------|-------------|
| **(a) Industry Usage** | 40% | How many real mainframe shops actively use this technology. Scale: 5=universal, 4=very common, 3=common, 2=niche, 1=rare/legacy |
| **(b) Dependency Impact** | 30% | Whether existing OpenMainframe features depend on this gap being filled, or whether filling it unlocks other high-value work. Scale: 5=critical blocker, 4=high enabler, 3=moderate, 2=low, 1=standalone |
| **(c) Implementation Complexity** | 30% | Inverse complexity — easier = higher score. Scale: 5=S (quick win), 4=M, 3=L, 2=XL, 1=XXL |

**Composite score** = (a × 0.4) + (b × 0.3) + (c × 0.3)

## Master Inventory Summary

| Batch | Technology | Category | Features | Present | Partial | Missing | Epics | Overall Size |
|-------|-----------|----------|----------|---------|---------|---------|-------|-------------|
| 1 | REXX | Language | 85+ | 0 | 0 | 85+ | 12 (R100–R111) | XL |
| 2 | HLASM | Language | 90+ | 0 | 0 | 90+ | 11 (H100–H110) | XL |
| 3 | PL/I | Language | 100+ | 0 | 0 | 100+ | 13 (PL100–PL112) | XL |
| 4 | Easytrieve | Language | 60+ | 0 | 0 | 60+ | 8 (ET100–ET107) | L |
| 5 | CLIST | Language | 50+ | 0 | 0 | 50+ | 7 (CL100–CL106) | L |
| 6 | Natural | Language | 70+ | 0 | 0 | 70+ | 10 (NAT100–NAT109) | XL |
| 7 | FOCUS | Language | 55+ | 0 | 0 | 55+ | 8 (FOC100–FOC107) | L |
| 8 | RACF | Security | 80+ | 0 | 2 | 78+ | 10 (RACF100–RACF109) | XL |
| 9 | TSO/ISPF | System | 90+ | 3 | 5 | 82+ | 12 (TSO100–TSO111) | XL |
| 10 | IBM MQ | Messaging | 95+ | 0 | 3 | 92+ | 11 (MQ100–MQ110) | XL |
| 11 | JES2 | System | 85+ | 5 | 8 | 72+ | 10 (JES100–JES109) | XL |
| 12 | Language Environment | Runtime | 75+ | 3 | 5 | 67+ | 9 (LE100–LE108) | XL |
| 13 | IMS TM + MFS | Transaction | 80+ | 0 | 2 | 78+ | 10 (IMS100–IMS109) | XL |
| 14 | SMF | Monitoring | 65+ | 2 | 3 | 60+ | 9 (SMF100–SMF108) | L |
| 15 | IDMS | Database | 70+ | 0 | 0 | 70+ | 9 (IDMS100–IDMS108) | XL |
| 16 | ADABAS | Database | 65+ | 0 | 0 | 65+ | 9 (ADA100–ADA108) | XL |
| 17 | WLM | System | 75+ | 0 | 4 | 71+ | 12 (WLM100–WLM111) | XL |
| 18 | z/OS UNIX (USS) | System | 130+ | 0 | 5 | 125+ | 12 (USS100–USS111) | XL |
| 19 | DFSMS + Catalog | Storage | 110+ | 18 | 10 | 82+ | 11 (DFSMS100–DFSMS110) | XL |
| 20 | Networking | Communications | 95+ | 2 | 5 | 88+ | 10 (NET100–NET109) | XL |
| 21 | Utilities | System | 68 | 10 | 5 | 53 | 11 (UTIL100–UTIL110) | L |
| | **TOTALS** | | **1,733+** | **43** | **57** | **1,633+** | **214** | |

## Priority Rankings

### Tier 1 — Critical (Score 4.0+) — Must-Have for Viable Emulation

| Rank | Technology | (a) Usage | (b) Dependency | (c) Ease | Score | Rationale |
|------|-----------|-----------|---------------|----------|-------|-----------|
| 1 | **RACF** (Batch 8) | 5 | 5 | 2 | 4.1 | Every z/OS system; existing CICS/JCL/TSO all need security hooks; no RACF = no multi-user |
| 2 | **JES2** (Batch 11) | 5 | 5 | 2 | 4.1 | JCL executor depends on JES2 for job submission, spool, output; partially started |
| 3 | **Language Environment** (Batch 12) | 5 | 5 | 2 | 4.1 | Every compiled program needs LE runtime (condition handling, storage, math); COBOL/CICS depend on it |
| 4 | **REXX** (Batch 1) | 5 | 4 | 2 | 3.8 | Used in virtually every shop; TSO/ISPF automation, JCL procedures, IKJEFT01 batch |
| 5 | **TSO/ISPF** (Batch 9) | 5 | 4 | 2 | 3.8 | Primary interactive interface; TUI crate provides 3270 but no TSO command processor or ISPF panels |
| 6 | **Utilities** (Batch 21) | 5 | 4 | 3 | 4.1 | Every JCL job uses IEBGENER/IEBCOPY/SORT; framework exists; high-value incremental wins |
| 7 | **DFSMS + Catalog** (Batch 19) | 5 | 5 | 2 | 4.1 | Dataset management is fundamental; most implementation exists (catalog, GDG, VSAM, PDS); SMS gaps remain |

### Tier 2 — High (Score 3.0–3.9) — Important for Production Workloads

| Rank | Technology | (a) Usage | (b) Dependency | (c) Ease | Score | Rationale |
|------|-----------|-----------|---------------|----------|-------|-----------|
| 8 | **HLASM** (Batch 2) | 4 | 4 | 2 | 3.4 | System exits, macros, authorized programs all need assembler; hard but high leverage |
| 9 | **WLM** (Batch 17) | 5 | 4 | 2 | 3.8 | Every z/OS system; JES2/CICS/DB2 all reference WLM for scheduling; no implementation yet |
| 10 | **IBM MQ** (Batch 10) | 4 | 3 | 2 | 3.1 | Very common middleware; CICS/IMS integration; no implementation yet |
| 11 | **z/OS UNIX (USS)** (Batch 18) | 4 | 3 | 2 | 3.1 | Required for modern z/OS (Java, Node.js, Python on z); no USS implementation |
| 12 | **Networking** (Batch 20) | 4 | 3 | 2 | 3.1 | TN3270E exists (good foundation); VTAM/TCP-IP stack needed for full connectivity |
| 13 | **SMF** (Batch 14) | 4 | 3 | 3 | 3.4 | Every z/OS system; batch metrics crate exists as foundation; essential for auditing/chargeback |
| 14 | **PL/I** (Batch 3) | 3 | 3 | 2 | 2.7 | Large legacy codebase in financial/insurance; needs LE runtime |

### Tier 3 — Medium (Score 2.0–2.9) — Valuable for Specific Workloads

| Rank | Technology | (a) Usage | (b) Dependency | (c) Ease | Score | Rationale |
|------|-----------|-----------|---------------|----------|-------|-----------|
| 15 | **IMS TM + MFS** (Batch 13) | 3 | 2 | 2 | 2.4 | Common in banking/insurance; IMS crate has DB but no TM; niche but deep |
| 16 | **CLIST** (Batch 5) | 3 | 3 | 4 | 3.1 | TSO automation language; simpler than REXX; requires TSO command processor |
| 17 | **Easytrieve** (Batch 4) | 2 | 1 | 3 | 2.0 | Report generator; common in specific verticals; relatively self-contained |
| 18 | **IDMS** (Batch 15) | 2 | 1 | 2 | 1.7 | Broadcom product; significant in government/insurance; complex CODASYL model |
| 19 | **ADABAS** (Batch 16) | 2 | 1 | 2 | 1.7 | Software AG product; significant in European enterprises; inverted list architecture |

### Tier 4 — Low (Score < 2.0) — Niche/Legacy Technologies

| Rank | Technology | (a) Usage | (b) Dependency | (c) Ease | Score | Rationale |
|------|-----------|-----------|---------------|----------|-------|-----------|
| 20 | **Natural** (Batch 6) | 2 | 1 | 1 | 1.4 | Software AG 4GL; paired with ADABAS; complex but niche |
| 21 | **FOCUS** (Batch 7) | 1 | 1 | 2 | 1.1 | Information Builders 4GL; very niche; declining usage |

## Prioritized Epic Backlog

The following orders all 214 epics from the 21 gap analysis batches into a single prioritized backlog. Epics within each tier are ordered by suggested implementation sequence (dependencies first).

### Phase A — Foundation Layer (Tier 1 Epics)

These epics provide the infrastructure that most other work depends on.

| Priority | Epic ID | Technology | Epic Name | Size | Dependencies |
|----------|---------|-----------|-----------|------|-------------|
| A-01 | RACF-100 | RACF | RACF Database and User Profiles | L | None (foundation) |
| A-02 | RACF-101 | RACF | Authentication — RACINIT/RACHECK/ICHEINTY | L | RACF-100 |
| A-03 | LE-100 | LE | Condition Handling (CEEHDLR/CEESGL/ESPIE/ESTAE) | L | None |
| A-04 | LE-101 | LE | Storage Management (CEEGTST/CEEFMST/CEECZST) | M | None |
| A-05 | LE-102 | LE | Math and Date/Time Functions | M | None |
| A-06 | JES2-100 | JES2 | Job Queue and Scheduling | L | None (extends JCL executor) |
| A-07 | JES2-101 | JES2 | Spool Management (SYSOUT classes, JESDS) | L | JES2-100 |
| A-08 | JES2-102 | JES2 | JES2 Commands ($D/$S/$P/$A/$C) | M | JES2-100 |
| A-09 | UTIL-110 | Utilities | Utility Framework Enhancements | S | JCL executor |
| A-10 | UTIL-100 | Utilities | IEBCOPY — PDS Copy/Compress/Merge | L | dataset crate (pds.rs) |
| A-11 | UTIL-101 | Utilities | IEBGENER Enhancements — Control Statements | M | dataset crate |
| A-12 | UTIL-104 | Utilities | DFSORT Enhancements — OUTFIL/IFTHEN/ICETOOL | L | sort crate |
| A-13 | DFSMS-100 | DFSMS | SMS Constructs (Data/Storage/Mgmt Class) | L | dataset crate |
| A-14 | DFSMS-101 | DFSMS | ACS Routines | M | DFSMS-100 |
| A-15 | DFSMS-102 | DFSMS | ICF Catalog Enhancements (BCS/VVDS) | L | dataset crate (catalog.rs) |

### Phase B — Interactive & Scripting Layer (Tier 1–2 Epics)

These enable the interactive user experience and automation.

| Priority | Epic ID | Technology | Epic Name | Size | Dependencies |
|----------|---------|-----------|-----------|------|-------------|
| B-01 | TSO-100 | TSO/ISPF | TSO Command Processor | L | RACF-100 |
| B-02 | TSO-101 | TSO/ISPF | TSO Built-in Commands (ALLOCATE, LISTDS, SUBMIT, etc.) | L | TSO-100 |
| B-03 | R-100 | REXX | REXX Parser and Interpreter Core | XL | None |
| B-04 | R-101 | REXX | REXX Built-in Functions (100+ functions) | L | R-100 |
| B-05 | R-102 | REXX | REXX I/O (EXECIO, OUTTRAP) | M | R-100 |
| B-06 | R-109 | REXX | REXX Host Environments (TSO, MVS, ISPEXEC) | L | R-100, TSO-100 |
| B-07 | UTIL-109 | Utilities | TSO/REXX Batch Processors (IKJEFT01, IRXJCL, BPXBATCH) | L | TSO-100, R-100 |
| B-08 | TSO-102 | TSO/ISPF | ISPF Dialog Manager (DISPLAY, TBOPEN, SELECT) | XL | TSO-100 |
| B-09 | TSO-103 | TSO/ISPF | ISPF Editor (EDIT macro, line commands) | L | TSO-102 |
| B-10 | TSO-104 | TSO/ISPF | ISPF Panels and Skeletons (ISPSLIB, ISPPLIB) | L | TSO-102 |
| B-11 | RACF-102 | RACF | Dataset Protection (RACHECK against profiles) | L | RACF-100, DFSMS-102 |
| B-12 | RACF-103 | RACF | RACF Commands (ADDUSER, ALTUSER, PERMIT, etc.) | M | RACF-100, TSO-100 |
| B-13 | WLM-100 | WLM | Service Classes and Goals | L | None |
| B-14 | WLM-101 | WLM | Workload Classification Rules | L | WLM-100 |
| B-15 | WLM-102 | WLM | WLM-Managed Initiators | M | WLM-100, JES2-100 |

### Phase C — Middleware & Communications Layer (Tier 2 Epics)

These add enterprise middleware and network capabilities.

| Priority | Epic ID | Technology | Epic Name | Size | Dependencies |
|----------|---------|-----------|-----------|------|-------------|
| C-01 | H-100 | HLASM | HLASM Parser — Instructions and Macro Expansion | XL | None |
| C-02 | H-101 | HLASM | HLASM Assembler — Object Deck Generation | XL | H-100 |
| C-03 | MQ-100 | IBM MQ | Queue Manager Core (MQCONN/MQOPEN/MQPUT/MQGET) | L | None |
| C-04 | MQ-101 | IBM MQ | Message and Queue Properties | M | MQ-100 |
| C-05 | MQ-102 | IBM MQ | Trigger Monitoring and MQSC Commands | L | MQ-100 |
| C-06 | USS-100 | USS | POSIX File System (zFS) and Path Resolution | L | None |
| C-07 | USS-101 | USS | Process Model (fork/exec/spawn/BPX1xxx) | XL | USS-100 |
| C-08 | USS-102 | USS | USS Shell (/bin/sh) and Utilities | L | USS-100, USS-101 |
| C-09 | NET-100 | Networking | TCP/IP Stack Configuration (TCPIP PROFILE) | L | None |
| C-10 | NET-101 | Networking | Sockets API (AF_INET/AF_INET6) | L | NET-100 |
| C-11 | NET-102 | Networking | AT-TLS (Application Transparent TLS) | M | NET-100 |
| C-12 | NET-103 | Networking | Application Services (FTP, SSH, NFS, DNS) | L | NET-100 |
| C-13 | SMF-100 | SMF | SMF Record Format and Writer | M | None |
| C-14 | SMF-101 | SMF | Job/Step Records (Types 4, 5, 30) | M | SMF-100, JES2-100 |
| C-15 | SMF-102 | SMF | Dataset Activity Records (Types 14, 15) | M | SMF-100, DFSMS-102 |

### Phase D — Extended Language Support (Tier 2–3 Epics)

These add additional language runtimes beyond COBOL (which already exists).

| Priority | Epic ID | Technology | Epic Name | Size | Dependencies |
|----------|---------|-----------|-----------|------|-------------|
| D-01 | PL100 | PL/I | PL/I Parser — Declarations and Statements | XL | LE-100 |
| D-02 | PL101 | PL/I | PL/I Data Types (FIXED DEC, CHAR, BIT, PICTURE) | L | PL100 |
| D-03 | PL102 | PL/I | PL/I I/O (GET/PUT/READ/WRITE, FILE declaration) | L | PL100 |
| D-04 | CL100 | CLIST | CLIST Interpreter Core | L | TSO-100 |
| D-05 | CL101 | CLIST | CLIST Built-in Functions and I/O | M | CL100 |
| D-06 | H-102 | HLASM | HLASM Macro Language (GBLA/LCLA/SETC/AIF/AGO) | L | H-100 |
| D-07 | H-103 | HLASM | HLASM System Macros (WTO/GETMAIN/FREEMAIN/SVC) | L | H-100, LE-100 |
| D-08 | LE-103 | LE | LE Enclave Model (CEEINITIALIZE/CEETERMINATE) | L | LE-100 |
| D-09 | LE-104 | LE | LE Message Services (CEEMSG/CEEMOUT) | S | LE-100 |
| D-10 | LE-105 | LE | LE National Language Support (CEESETL) | M | LE-100 |

### Phase E — Transaction Monitors & Databases (Tier 3 Epics)

These add specialized subsystem support for specific verticals.

| Priority | Epic ID | Technology | Epic Name | Size | Dependencies |
|----------|---------|-----------|-----------|------|-------------|
| E-01 | IMS-100 | IMS TM | IMS Transaction Manager Core | XL | LE-100 |
| E-02 | IMS-101 | IMS TM | MFS (Message Format Services) | L | IMS-100 |
| E-03 | IMS-102 | IMS TM | IMS DC Commands (/DIS, /STA, /STO) | M | IMS-100 |
| E-04 | ET100 | Easytrieve | Easytrieve Parser and Interpreter | L | None |
| E-05 | ET101 | Easytrieve | Easytrieve File and Report Processing | L | ET100 |
| E-06 | IDMS-100 | IDMS | IDMS Schema/Subschema and DML | XL | LE-100 |
| E-07 | IDMS-101 | IDMS | IDMS DC Task/Screen Management | L | IDMS-100 |
| E-08 | ADA-100 | ADABAS | ADABAS Nucleus and Direct Calls | XL | None |
| E-09 | ADA-101 | ADABAS | ADABAS FDT and Descriptor Management | L | ADA-100 |

### Phase F — Niche & Legacy (Tier 4 Epics)

| Priority | Epic ID | Technology | Epic Name | Size | Dependencies |
|----------|---------|-----------|-----------|------|-------------|
| F-01 | NAT-100 | Natural | Natural Parser and Runtime Core | XL | ADA-100 |
| F-02 | NAT-101 | Natural | Natural Statements and Data Types | L | NAT-100 |
| F-03 | FOC-100 | FOCUS | FOCUS/WebFOCUS Interpreter Core | L | None |
| F-04 | FOC-101 | FOCUS | FOCUS TABLE/GRAPH Reporting | L | FOC-100 |

### Remaining Epics (not individually listed)

The phases above list the top ~50 highest-priority epics. The remaining ~164 epics across all batches fall within each technology's gap analysis and should be implemented in the sequence defined within each batch's epic structure (e.g., RACF-104 through RACF-109 follow RACF-100–103 in the same dependency order).

## Cross-Cutting Dependency Graph

```
RACF-100 ──────────┬──> TSO-100 ──────┬──> ISPF (TSO-102+)
                   │                  ├──> REXX host envs (R-109)
                   │                  ├──> CLIST (CL100+)
                   │                  └──> IKJEFT01 (UTIL-109)
                   │
LE-100 ────────────┼──> COBOL (exists) ──> CICS (exists)
                   │                  ──> DB2 (exists)
                   ├──> PL/I (PL100+)
                   ├──> HLASM exits (H-103)
                   └──> IMS TM (IMS-100)

JES2-100 ──────────┼──> JCL executor (extends existing)
                   ├──> WLM initiators (WLM-102)
                   ├──> SMF job records (SMF-101)
                   └──> TSO SUBMIT (TSO-101)

DFSMS-100 ─────────┼──> SMS-managed allocation
                   ├──> RACF dataset protect (RACF-102)
                   └──> Utility access (UTIL-100+)

USS-100 ───────────┼──> BPXBATCH (UTIL-109)
                   ├──> SSH/SFTP (NET-103)
                   └──> Java/Python on z (future)

Sort crate ────────┼──> ICETOOL (UTIL-104)
                   └──> ICEGENER (UTIL-104)

Dataset crate ─────┼──> IEBCOPY (UTIL-100)
                   ├──> IEHPROGM (UTIL-107)
                   └──> IEBGENER+ (UTIL-101)
```

## Quick Wins — Highest Value/Effort Ratio

These epics have the best value-for-effort ratio and could be tackled first to show rapid progress:

| Epic | Technology | Size | Why Quick Win |
|------|-----------|------|---------------|
| UTIL-110 | Utilities | S | Framework improvements for all utilities; small effort, big leverage |
| UTIL-101 | Utilities | M | IEBGENER already works for simple copy; adding control statements is incremental |
| UTIL-102 | Utilities | S | IEBCOMPR is a simple record comparison; dataset crate provides I/O |
| SMF-100 | SMF | M | Record format definition; batch_metrics.rs exists as foundation |
| LE-102 | LE | M | Math/date functions are self-contained; no external dependencies |
| LE-104 | LE | S | Message services are thin wrappers |
| UTIL-105 | Utilities | S | IEBPTPCH is straightforward print formatting |
| UTIL-106 | Utilities | S | IEBDG is a self-contained test data generator |
| JES2-102 | JES2 | M | JES2 commands add operator control; builds on existing JCL infrastructure |
| RACF-100 | RACF | L | Foundation for all security; unblocks many downstream epics |

## Summary Statistics

| Metric | Value |
|--------|-------|
| Total technologies assessed | 21 |
| Total features analyzed | 1,733+ |
| Features present | 43 (2.5%) |
| Features partial | 57 (3.3%) |
| Features missing | 1,633+ (94.2%) |
| Total proposed epics | 214 |
| Tier 1 (Critical) technologies | 7 |
| Tier 2 (High) technologies | 7 |
| Tier 3 (Medium) technologies | 5 |
| Tier 4 (Low) technologies | 2 |
| S-sized epics | ~25 |
| M-sized epics | ~55 |
| L-sized epics | ~85 |
| XL-sized epics | ~49 |

The project has strong foundations in JCL parsing/execution, COBOL compilation, CICS runtime, DB2 SQL, VSAM/dataset management, DFSORT, encoding, and TN3270E. The largest gaps are in security (RACF), interactive environment (TSO/ISPF), scripting (REXX), runtime services (LE), and job management (JES2). Filling these five areas would transform OpenMainframe from a compilation/execution tool into a credible z/OS emulation platform.
