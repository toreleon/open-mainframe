---
version: 'v5.0-addendum'
planningGroup: 'PG-6'
technology: 'DFSMS, HSM & Catalog Extensions'
date: '2026-02-21'
status: 'complete'
parentDocument: 'epics-dataset-v3.0.md'
inputDocuments:
  - 'epics-dataset-v3.0.md'
  - 'architecture-dataset-v3.0.md'
  - 'zos-complete-inventory.md (AREA-4)'
  - 'gap-analysis/batch-19-dfsms-catalog.md'
newEpics: 11
newStories: 68
---

# DFSMS, HSM & Catalog Extensions Addendum (v5.0)

This addendum extends the existing Dataset v3.0 planning (Epics 600-608) with DFSMS-100 through DFSMS-110 covering SMS policy management, ICF catalogs, DFSMShsm, DFSMSdss, DFSMSrmm, and advanced PDSE/space management.

## Relationship to Existing Epics

| Existing v3.0 | New v5.0 Epic | Relationship |
|---------------|---------------|-------------|
| 600 (VSAM AIX) | DFSMS-103 | DFSMS-103 completes AIX build/maintenance, EXAMINE/DIAGNOSE |
| 601 (PDS/PDSE) | DFSMS-108 | DFSMS-108 adds program objects, member generations beyond basic PDS |
| 602 (LDS) | — | No extension needed; LDS is adequately covered |
| 603 (Concurrent Access) | — | No extension needed |
| 604 (Free Space) | — | No extension needed |
| 605 (Spanned Records) | — | No extension needed |
| 606 (IDCAMS REPRO) | DFSMS-103 | DFSMS-103 extends IDCAMS with DEFINE NONVSAM/ALIAS, EXAMINE, DIAGNOSE, EXPORT/IMPORT |
| 607 (Catalog Persistence) | DFSMS-102 | DFSMS-102 replaces flat-file catalog with full ICF infrastructure |
| 608 (BSAM/BPAM) | — | No extension needed |
| — (new) | DFSMS-100 | SMS construct data model (data/storage/management class, storage group) |
| — (new) | DFSMS-101 | ACS routine interpreter |
| — (new) | DFSMS-104 | DFSMShsm migration & recall |
| — (new) | DFSMS-105 | DFSMShsm backup & ABARS |
| — (new) | DFSMS-106 | DFSMSdss DUMP/RESTORE/COPY (ADRDSSU) |
| — (new) | DFSMS-107 | DFSMSrmm tape management |
| — (new) | DFSMS-109 | Space & volume management |
| — (new) | DFSMS-110 | GDG-to-ICF catalog integration |

**Dependency:** DFSMS-102 (ICF Catalogs) depends on Epic 600 (VSAM) being at least partially complete since BCS is a KSDS. DFSMS-104/105 depend on DFSMS-100 (SMS management class). DFSMS-103 depends on DFSMS-102 (EXAMINE/DIAGNOSE need ICF catalog). DFSMS-110 depends on DFSMS-102 plus existing GDG (Epic 600 area / gdg/ module).

---

## DFSMS-100: SMS Construct Data Model

**Description:** Define data structures for Storage Management Subsystem (SMS) constructs — data class (default allocation attributes), storage class (performance/availability goals), management class (lifecycle policies), and storage group (volume pool). Includes construct validation, serialization, and the SCDS/ACDS configuration model.

**User Value:** JCL DD statements with DATACLAS, STORCLAS, MGMTCLAS parameters are enforced — allocation defaults, performance goals, and lifecycle policies are applied per SMS policy, matching how real z/OS installations manage storage.

**Size:** L | **Stories:** 7

### DFSMS-100.1: Data Class Definition

**As a** storage administrator, **I want** data class definitions with default allocation attributes, **so that** datasets allocated with a data class inherit consistent RECFM, LRECL, BLKSIZE, and space settings.

**Acceptance Criteria:**
- Given a data class `DCSTD80` with RECFM=FB, LRECL=80, BLKSIZE=27920, SPACE=(TRK,(10,5)), when a dataset is allocated with DATACLAS=DCSTD80, then these defaults are used unless overridden by JCL
- Given data class with RETPD=30, when a dataset is allocated, then the retention period is set to 30 days
- Given `DataClass` struct serialized to SCDS, when loaded, then all attributes are preserved

### DFSMS-100.2: Storage Class Definition

**As a** storage administrator, **I want** storage class definitions with performance and availability goals, **so that** datasets are placed on appropriate storage based on their access requirements.

**Acceptance Criteria:**
- Given a storage class `SCFAST` with guaranteed_space=true, accessibility=CONTINUOUS, when assigned to a dataset, then the dataset is placed on a volume meeting those goals
- Given a storage class with I/O priority, when the dataset is accessed, then the priority is available for WLM integration

### DFSMS-100.3: Management Class Definition

**As a** storage administrator, **I want** management class definitions with backup and migration policies, **so that** dataset lifecycle is automated per policy.

**Acceptance Criteria:**
- Given a management class with backup_frequency=DAILY, migration_age=30, retention=365, when assigned to a dataset, then DFSMShsm uses these values for backup scheduling and migration decisions
- Given management class with versions_backup=3, when backups are taken, then up to 3 backup versions are retained

### DFSMS-100.4: Storage Group Definition

**As a** storage administrator, **I want** storage group definitions (POOL, VIO, DUMMY, TAPE types), **so that** datasets are allocated to specific volume pools.

**Acceptance Criteria:**
- Given a POOL storage group with volumes [VOL001, VOL002, VOL003], when a dataset is allocated to this group, then it is placed on a volume with sufficient space
- Given a VIO (Virtual I/O) storage group, when a temporary dataset is allocated, then it uses virtual storage (no physical volume)

### DFSMS-100.5: SCDS/ACDS Configuration Model

**As a** storage administrator, **I want** SCDS (source) and ACDS (active) configuration datasets, **so that** SMS configuration follows the standard activation workflow.

**Acceptance Criteria:**
- Given SMS constructs defined in SCDS, when activated, then ACDS is created as a snapshot of the active configuration
- Given ACDS, when SMS processes a dataset allocation, then constructs are looked up from the active configuration
- Given SCDS modification without activation, when allocation occurs, then the old ACDS configuration is used

### DFSMS-100.6: Construct Validation and Cross-References

**As a** storage administrator, **I want** SMS constructs validated for consistency, **so that** invalid configurations are rejected before activation.

**Acceptance Criteria:**
- Given a management class referencing a non-existent storage group, when validated, then an error is reported
- Given a data class with RECFM=FB but no LRECL, when validated, then a warning is issued
- Given circular or conflicting construct references, when validated, then errors are reported

### DFSMS-100.7: SMS Construct Tests

**Acceptance Criteria:**
- Given SCDS with 5 data classes, 3 storage classes, 2 management classes, and 2 storage groups, when validated and activated, then ACDS contains all constructs
- Given `cargo test` SMS construct tests, then all pass

---

## DFSMS-101: ACS Routine Interpreter

**Description:** Implement the ACS routine scripting language used for Automatic Class Selection — FILTLIST pattern matching, SELECT/WHEN/DO/END conditional logic, SET assignment, system variables (&DSN, &DSTYPE, &HLQ, &JOBNAME, etc.). Four routine types (DATACLAS, STORCLAS, MGMTCLAS, STORGRP).

**User Value:** Datasets are automatically assigned SMS constructs at allocation time based on site policy rules, matching the standard z/OS ACS routine behavior.

**Size:** L | **Stories:** 7

### DFSMS-101.1: ACS Routine Parser

**As a** storage administrator, **I want** ACS routines parsed from source text, **so that** existing ACS routine definitions can be loaded.

**Acceptance Criteria:**
- Given ACS routine source with FILTLIST, SELECT/WHEN/DO/END, SET statements, when parsed, then an AST of ACS statements is produced
- Given continuation lines (+ at column 72), when parsed, then they are handled correctly
- Given comments (/* ... */), when parsed, then they are ignored

### DFSMS-101.2: FILTLIST Pattern Matching

**As a** storage administrator, **I want** FILTLIST definitions with wildcards, **so that** dataset names can be matched against patterns.

**Acceptance Criteria:**
- Given `FILTLIST PRODDSNS INCLUDE('PROD.**','PROD1.*')`, when dataset `PROD.DATA.FILE` is checked, then it matches
- Given `FILTLIST EXCLDSNS INCLUDE('TEMP.**','TEST.**')`, when dataset `PROD.DATA` is checked, then it does not match
- Given &DSN compared against a FILTLIST via `WHEN (&DSN = &PRODDSNS)`, when evaluated, then the pattern match is performed

### DFSMS-101.3: SELECT/WHEN/DO/END Logic

**As a** storage administrator, **I want** conditional logic in ACS routines, **so that** different datasets get different class assignments.

**Acceptance Criteria:**
- Given `SELECT; WHEN (&DSN = &PRODDSNS) SET &STORCLAS = 'SCFAST'; WHEN (&DSN = &TESTDSNS) SET &STORCLAS = 'SCSTD'; OTHERWISE SET &STORCLAS = 'SCDEFAULT'; END`, when a PROD dataset is allocated, then STORCLAS=SCFAST is assigned
- Given nested SELECT blocks, when evaluated, then inner blocks are processed correctly

### DFSMS-101.4: System Variables

**As a** storage administrator, **I want** ACS system variables (&DSN, &DSTYPE, &HLQ, &JOBNAME, &ACESSION, &UNIT, etc.), **so that** routines can make decisions based on allocation context.

**Acceptance Criteria:**
- Given &DSN set to the dataset name being allocated, when referenced in a WHEN clause, then the current dataset name is used
- Given &HLQ extracting the high-level qualifier, when `&HLQ = 'SYS1'`, then system datasets are identified
- Given &DSTYPE set to VSAM/HFS/PDS/etc., when checked, then the dataset type is available

### DFSMS-101.5: Four Routine Types and Execution

**As a** storage administrator, **I want** DATACLAS, STORCLAS, MGMTCLAS, and STORGRP routines, **so that** all four SMS construct types are assigned automatically.

**Acceptance Criteria:**
- Given four ACS routines registered, when a dataset is allocated, then all four are invoked in order: DATACLAS → STORCLAS → MGMTCLAS → STORGRP
- Given a routine that does not SET a value, when allocation proceeds, then no construct of that type is assigned (unless JCL explicitly specifies one)
- Given JCL explicitly specifying DATACLAS=X, when the ACS routine runs, then the JCL value takes precedence over ACS assignment

### DFSMS-101.6: ACS-to-Allocation Integration

**As a** storage administrator, **I want** ACS routines wired into the dataset allocation flow, **so that** every allocation is processed through SMS policy.

**Acceptance Criteria:**
- Given an SMS-managed volume, when DYNALLOC (SVC 99) or JCL DD NEW allocates a dataset, then ACS routines are invoked before allocation completes
- Given ACS assigns DATACLAS=DCSTD80, when the dataset is created, then default attributes from DCSTD80 are applied

### DFSMS-101.7: ACS Routine Tests

**Acceptance Criteria:**
- Given an ACS routine with 3 FILTLISTs and 5 WHEN conditions, when tested against 10 dataset names, then correct classes are assigned for each
- Given `cargo test` ACS routine tests, then all pass

---

## DFSMS-102: ICF Catalog Infrastructure

**Description:** Replace the flat-file catalog with ICF (Integrated Catalog Facility) structure — master catalog, user catalogs, BCS (Basic Catalog Structure as KSDS with 45-byte key), VVDS (VSAM Volume Data Set as ESDS per volume). Implement alias definitions, connector records, catalog search order, EXAMINE/DIAGNOSE commands.

**User Value:** Dataset cataloging follows z/OS conventions with master/user catalog hierarchy, proper search order, and catalog integrity checking.

**Size:** XL | **Stories:** 9

### DFSMS-102.1: Master Catalog

**As a** system administrator, **I want** a master catalog containing SYS1.* datasets and user catalog connectors, **so that** system datasets and catalog routing work correctly.

**Acceptance Criteria:**
- Given system startup, when the master catalog is loaded, then all SYS1.* dataset entries are accessible
- Given a user catalog connector in the master catalog, when a dataset with matching alias is looked up, then the search is routed to the user catalog
- Given the master catalog, when LISTCAT LEVEL(SYS1) is issued, then all system datasets are listed

### DFSMS-102.2: User Catalogs

**As a** system administrator, **I want** user catalogs for application-level dataset grouping, **so that** different applications' datasets are managed separately.

**Acceptance Criteria:**
- Given `DEFINE USERCATALOG (NAME(UCAT.PROD) MEGABYTES(50))`, when executed, then a new user catalog is created
- Given datasets cataloged in UCAT.PROD, when LISTCAT is issued against UCAT.PROD, then only that catalog's entries are shown
- Given UCAT.PROD connected to the master via alias PROD, when DSN PROD.DATA.FILE is referenced, then UCAT.PROD is searched

### DFSMS-102.3: BCS (Basic Catalog Structure)

**As a** developer, **I want** BCS implemented as a KSDS with 45-byte catalog key, **so that** catalog entries follow z/OS naming and lookup conventions.

**Acceptance Criteria:**
- Given a BCS entry for VSAM cluster MY.KSDS, when stored, then the entry includes cluster name, component names (data/index), volume serial, and extent info
- Given a BCS entry for non-VSAM dataset MY.SEQ.FILE, when stored, then the entry includes DSN, volume serial, and dataset attributes
- Given key-based lookup by DSN, when the BCS KSDS is searched, then O(log n) performance is achieved

### DFSMS-102.4: VVDS (VSAM Volume Data Set)

**As a** developer, **I want** VVDS (ESDS per volume) with VVR and NVR records, **so that** volume-level dataset extent and attribute information is tracked.

**Acceptance Criteria:**
- Given a VSAM dataset on VOL001, when cataloged, then a VVR (VSAM Volume Record) is created in VOL001's VVDS with extent information
- Given an SMS-managed non-VSAM dataset on VOL001, when cataloged, then an NVR (Non-VSAM Volume Record) is created
- Given VVCR (VSAM Volume Control Record) in the VVDS, when read, then it links the VVDS to its owning BCS

### DFSMS-102.5: Alias and Connector Records

**As a** system administrator, **I want** HLQ alias definitions and user catalog connector records, **so that** catalog search routing works per z/OS conventions.

**Acceptance Criteria:**
- Given `DEFINE ALIAS (NAME(PROD) RELATE(UCAT.PROD))`, when executed, then the master catalog contains an alias pointing PROD.** lookups to UCAT.PROD
- Given connector record for UCAT.PROD in master, when the master is listed, then the connector shows catalog name and volume

### DFSMS-102.6: Catalog Search Order

**As a** developer, **I want** catalog search to follow z/OS order (alias → master → user), **so that** dataset lookups resolve correctly.

**Acceptance Criteria:**
- Given DSN PROD.DATA.FILE and alias PROD→UCAT.PROD, when looked up, then UCAT.PROD is searched first
- Given DSN SYS1.PARMLIB with no alias match, when looked up, then the master catalog is searched
- Given DSN not found in any catalog, when looked up, then "NOT CATALOGED" is returned

### DFSMS-102.7: EXAMINE (BCS Integrity Check)

**As a** system administrator, **I want** EXAMINE to check BCS structural integrity, **so that** catalog corruption is detected.

**Acceptance Criteria:**
- Given a healthy BCS, when EXAMINE runs, then CC=0 with "NO ERRORS FOUND"
- Given a BCS with orphaned entries (referenced component missing), when EXAMINE runs, then the orphaned entries are reported with CC=4

### DFSMS-102.8: DIAGNOSE (BCS-VVDS Synchronization)

**As a** system administrator, **I want** DIAGNOSE to compare BCS and VVDS for synchronization, **so that** inconsistencies are detected.

**Acceptance Criteria:**
- Given BCS entry for MY.KSDS on VOL001, when DIAGNOSE checks and VOL001's VVDS has a matching VVR, then CC=0
- Given BCS entry but no matching VVR in VVDS, when DIAGNOSE checks, then the mismatch is reported with CC=4
- Given VVR in VVDS but no matching BCS entry, when DIAGNOSE checks, then the orphaned VVR is reported

### DFSMS-102.9: ICF Catalog Tests

**Acceptance Criteria:**
- Given master catalog with 3 user catalogs and aliases, when 20 datasets are cataloged across them, then all are findable via standard search order
- Given intentional BCS-VVDS mismatch, when EXAMINE and DIAGNOSE run, then errors are correctly reported
- Given `cargo test` ICF catalog tests, then all pass

---

## DFSMS-103: IDCAMS Enhancements

**Description:** Complete the IDCAMS implementation — functional DEFINE AIX with build, DEFINE PATH, DEFINE NONVSAM, DEFINE ALIAS, ALTER with full parameter support, EXAMINE, DIAGNOSE, EXPORT/IMPORT. Extends existing IDCAMS parser and execution engine.

**User Value:** All standard IDCAMS commands work for catalog management, enabling the full range of dataset administration operations.

**Size:** M | **Stories:** 6

### DFSMS-103.1: DEFINE NONVSAM and DEFINE ALIAS

**As a** system administrator, **I want** DEFINE NONVSAM and DEFINE ALIAS commands, **so that** non-VSAM datasets and HLQ aliases can be managed.

**Acceptance Criteria:**
- Given `DEFINE NONVSAM (NAME(MY.SEQ.FILE) VOLUMES(VOL001) DEVT(3390))`, when executed, then a catalog entry is created for the non-VSAM dataset
- Given `DEFINE ALIAS (NAME(PROD) RELATE(UCAT.PROD))`, when executed, then an HLQ alias is created in the master catalog

### DFSMS-103.2: Functional DEFINE AIX and BLDINDEX

**As a** developer, **I want** DEFINE AIX to create a functional alternate index, **so that** VSAM datasets can be accessed by alternate keys.

**Acceptance Criteria:**
- Given `DEFINE AIX (NAME(MY.AIX) RELATE(MY.KSDS) KEYS(30 10) UNIQUEKEY)`, when executed, then an AIX definition is created and linked to the base cluster
- Given `BLDINDEX INDATASET(MY.KSDS) OUTDATASET(MY.AIX)`, when executed, then the AIX is populated from existing base cluster records

### DFSMS-103.3: ALTER Full Parameter Support

**As a** system administrator, **I want** ALTER to modify more dataset attributes beyond rename, **so that** dataset configuration can be changed without recreation.

**Acceptance Criteria:**
- Given `ALTER MY.KSDS ADDVOLUMES(VOL002)`, when executed, then an additional volume is associated
- Given `ALTER MY.KSDS FREESPACE(20 10)`, when executed, then the FREESPACE parameter is updated
- Given `ALTER MY.KSDS NEWNAME(MY.KSDS.NEW)`, when executed, then the dataset is renamed (existing behavior preserved)

### DFSMS-103.4: EXPORT and IMPORT

**As a** system administrator, **I want** EXPORT/IMPORT for portable dataset transfer, **so that** datasets can be moved between systems.

**Acceptance Criteria:**
- Given `EXPORT MY.KSDS OUTFILE(EXPDD)`, when executed, then the dataset is exported to a portable sequential format with catalog information
- Given `IMPORT INFILE(IMPDD) OUTDATASET(MY.KSDS.NEW)`, when executed, then the dataset is imported and cataloged

### DFSMS-103.5: EXAMINE and DIAGNOSE Commands

**As a** system administrator, **I want** EXAMINE and DIAGNOSE as IDCAMS commands, **so that** catalog integrity can be checked via standard AMS interface.

**Acceptance Criteria:**
- Given `EXAMINE NAME(UCAT.PROD)`, when executed via IDCAMS, then BCS integrity is checked (delegates to DFSMS-102.7)
- Given `DIAGNOSE ICFCATALOG NAME(UCAT.PROD)`, when executed, then BCS-VVDS synchronization is checked (delegates to DFSMS-102.8)

### DFSMS-103.6: IDCAMS Enhancement Tests

**Acceptance Criteria:**
- Given DEFINE NONVSAM + DEFINE ALIAS + DEFINE AIX + BLDINDEX + ALTER + EXPORT + IMPORT sequence, when executed, then all commands complete successfully
- Given `cargo test` IDCAMS enhancement tests, then all pass

---

## DFSMS-104: DFSMShsm — Migration & Recall

**Description:** Implement hierarchical storage management — ML0/ML1/ML2 storage tiers, automatic space management (age-based migration), HMIGRATE/HRECALL commands, compression for migrated data, management-class-driven migration policies.

**User Value:** Datasets automatically migrate to lower-cost storage tiers based on age and access patterns, and are transparently recalled when accessed — matching standard z/OS HSM behavior.

**Size:** L | **Stories:** 7

### DFSMS-104.1: Storage Tier Model (ML0/ML1/ML2)

**As a** storage administrator, **I want** three storage tiers (ML0 active, ML1 secondary DASD, ML2 archive), **so that** data migrates based on access patterns.

**Acceptance Criteria:**
- Given HSM configuration with ML0 path, ML1 path, and ML2 path, when initialized, then three tiers are available
- Given a dataset on ML0, when its migration status is queried, then it shows as "active on ML0"
- Given a migrated dataset, when its status is queried, then it shows the current tier (ML1 or ML2)

### DFSMS-104.2: HMIGRATE Command

**As a** storage administrator, **I want** HMIGRATE to manually migrate a dataset to a lower tier, **so that** specific datasets can be moved to free space.

**Acceptance Criteria:**
- Given `HMIGRATE 'MY.LARGE.DATA'`, when executed, then the dataset is compressed and moved from ML0 to ML1
- Given `HMIGRATE 'MY.LARGE.DATA' ML2`, when executed, then the dataset is moved directly to ML2 (archive)
- Given a migrated dataset, when HMIGRATE is issued again, then it moves to the next lower tier

### DFSMS-104.3: HRECALL Command

**As a** storage administrator, **I want** HRECALL to bring migrated datasets back to ML0, **so that** data is available for processing.

**Acceptance Criteria:**
- Given `HRECALL 'MY.LARGE.DATA'`, when executed, then the dataset is decompressed and restored to ML0
- Given the dataset was on ML2, when recalled, then it is restored to ML0 (not ML1)
- Given automatic recall (dataset opened by a job), when a migrated dataset is referenced in JCL, then HRECALL is triggered automatically

### DFSMS-104.4: Automatic Space Management

**As a** storage administrator, **I want** automatic migration based on days-since-last-reference, **so that** unused datasets are migrated without manual intervention.

**Acceptance Criteria:**
- Given management class with migration_age=30, when a dataset has not been referenced for 30 days, then automatic migration moves it from ML0 to ML1
- Given ML1 management class secondary_age=90, when an ML1 dataset has not been referenced for 90 days, then it migrates to ML2
- Given automatic space management runs (scheduled), when ML0 volume utilization exceeds threshold, then eligible datasets are migrated

### DFSMS-104.5: Migration Data Compression

**As a** developer, **I want** migrated data compressed on ML1 and ML2, **so that** storage is used efficiently.

**Acceptance Criteria:**
- Given a 100MB dataset migrated to ML1, when stored, then it is compressed (expect 40-70% compression for text data)
- Given a compressed ML1 dataset, when recalled, then it is decompressed to original size and format

### DFSMS-104.6: HSM Control Data Sets (CDS)

**As a** developer, **I want** CDS (MCDS, BCDS, OCDS) for HSM state tracking, **so that** migration and backup state persists.

**Acceptance Criteria:**
- Given MCDS (Migration Control Data Set), when a dataset is migrated, then an MCDS record tracks the migration (source volume, target tier, original size, compressed size)
- Given BCDS (Backup Control Data Set), when a backup is taken, then the BCDS records it
- Given HSM restart, when CDS is loaded, then all migration/backup state is recovered

### DFSMS-104.7: DFSMShsm Migration Tests

**Acceptance Criteria:**
- Given 10 datasets with varying ages, when automatic space management runs, then eligible datasets migrate per management class policy
- Given HMIGRATE + HRECALL cycle, when completed, then dataset content is identical before and after
- Given `cargo test` HSM migration tests, then all pass

---

## DFSMS-105: DFSMShsm — Backup & ABARS

**Description:** Implement HBACKUP/HRECOVER for dataset-level backup/recovery. ABARS (Aggregate Backup and Recovery Support) for disaster recovery — aggregate definition, ABACKUP/ARECOVER commands. Management-class-driven backup frequency.

**User Value:** Datasets are backed up per policy with point-in-time recovery capability, and aggregate backup provides disaster recovery for related dataset groups.

**Size:** L | **Stories:** 6

### DFSMS-105.1: HBACKUP Command

**As a** storage administrator, **I want** HBACKUP for dataset-level backup, **so that** individual datasets can be backed up on demand.

**Acceptance Criteria:**
- Given `HBACKUP 'MY.CRITICAL.DATA'`, when executed, then a backup copy is created with timestamp
- Given management class with versions_backup=3, when 4 backups are taken, then only the 3 most recent are retained

### DFSMS-105.2: HRECOVER Command

**As a** storage administrator, **I want** HRECOVER to restore datasets from backup, **so that** data can be recovered after corruption or deletion.

**Acceptance Criteria:**
- Given `HRECOVER 'MY.CRITICAL.DATA'`, when executed, then the most recent backup is restored
- Given `HRECOVER 'MY.CRITICAL.DATA' FROMDATE(2026001)`, when executed, then the backup from or before Julian date 2026001 is restored
- Given the original dataset still exists, when HRECOVER REPLACE is specified, then the existing dataset is overwritten

### DFSMS-105.3: Automatic Backup (Management Class Driven)

**As a** storage administrator, **I want** backups triggered automatically based on management class frequency, **so that** no manual intervention is needed for routine backups.

**Acceptance Criteria:**
- Given management class with backup_frequency=DAILY, when the daily backup window runs, then all datasets with this class are backed up
- Given a dataset already backed up today, when the backup window runs again, then it is skipped (no duplicate)

### DFSMS-105.4: ABARS Aggregate Definition

**As a** disaster recovery administrator, **I want** aggregate groups of related datasets, **so that** entire application environments can be backed up and recovered together.

**Acceptance Criteria:**
- Given `DEFINE AGGREGATEGROUP (NAME(PAYROLL.AG) DATASETFILTER('PAYROLL.**'))`, when defined, then all datasets matching PAYROLL.** are included in the aggregate
- Given the aggregate group, when listed, then all included datasets are shown

### DFSMS-105.5: ABACKUP/ARECOVER Commands

**As a** disaster recovery administrator, **I want** ABACKUP and ARECOVER for aggregate groups, **so that** entire application environments can be backed up and restored.

**Acceptance Criteria:**
- Given `ABACKUP PAYROLL.AG`, when executed, then all datasets in the aggregate are backed up to a single recoverable unit
- Given `ARECOVER PAYROLL.AG`, when executed on a recovery system, then all aggregate datasets are restored with proper catalog entries

### DFSMS-105.6: DFSMShsm Backup Tests

**Acceptance Criteria:**
- Given HBACKUP + HRECOVER cycle with 3 versions, when version 2 is specifically recovered, then correct data is restored
- Given ABACKUP + ARECOVER of 5-dataset aggregate, when recovered, then all 5 datasets match originals
- Given `cargo test` HSM backup tests, then all pass

---

## DFSMS-106: DFSMSdss — DUMP/RESTORE/COPY (ADRDSSU)

**Description:** Implement DFSMSdss commands — logical DUMP/RESTORE for dataset-level backup, COPY for dataset movement between volumes, INCLUDE/EXCLUDE filtering with wildcards, PRINT for dataset display.

**User Value:** Datasets can be dumped (backed up), restored, and copied between storage volumes using standard DFSMSdss (ADRDSSU) commands, enabling operational data management.

**Size:** L | **Stories:** 7

### DFSMS-106.1: Logical DUMP

**As a** storage administrator, **I want** logical DUMP for dataset-level backup, **so that** individual datasets or filtered groups can be backed up.

**Acceptance Criteria:**
- Given `DUMP DATASET(INCLUDE('MY.DATA.**')) OUTDDNAME(DUMPDD)`, when executed, then all matching datasets are dumped to a sequential dump dataset
- Given DUMP with COMPRESS, when executed, then the dump dataset is compressed
- Given DUMP, when complete, then CC=0 with count of datasets dumped

### DFSMS-106.2: Logical RESTORE

**As a** storage administrator, **I want** logical RESTORE from a dump dataset, **so that** datasets can be recovered.

**Acceptance Criteria:**
- Given `RESTORE DATASET(INCLUDE('MY.DATA.**')) INDDNAME(DUMPDD)`, when executed, then datasets are restored from the dump
- Given RESTORE with RENAME(MY.DATA.**,MY.COPY.**), when executed, then datasets are restored with new names
- Given RESTORE with REPLACE, when a target dataset exists, then it is overwritten

### DFSMS-106.3: Dataset COPY

**As a** storage administrator, **I want** COPY to move datasets between volumes, **so that** data can be reorganized across storage.

**Acceptance Criteria:**
- Given `COPY DATASET(INCLUDE('MY.DATA.**')) OUTDY(3390) STORCLAS(SCNEW)`, when executed, then datasets are copied to a new volume with the specified storage class
- Given COPY with DELETE, when executed, then the source datasets are deleted after successful copy

### DFSMS-106.4: INCLUDE/EXCLUDE Filtering

**As a** storage administrator, **I want** INCLUDE and EXCLUDE filters with wildcards, **so that** DUMP/RESTORE/COPY can target specific dataset groups.

**Acceptance Criteria:**
- Given `INCLUDE('PROD.**')`, when evaluated, then all datasets starting with PROD are selected
- Given `INCLUDE('PROD.**') EXCLUDE('PROD.TEMP.**')`, when evaluated, then PROD datasets are selected except PROD.TEMP
- Given `BY((CREDT,GE,2026001))`, when evaluated, then only datasets created on or after Julian date 2026001 are selected

### DFSMS-106.5: DFSMSdss PRINT

**As a** storage administrator, **I want** PRINT to display dataset contents, **so that** data can be inspected without separate utilities.

**Acceptance Criteria:**
- Given `PRINT DATASET(MY.DATA.FILE)`, when executed, then dataset contents are displayed in hex and character format
- Given PRINT with COUNT(100), when executed, then only the first 100 records are displayed

### DFSMS-106.6: Dump Dataset Format

**As a** developer, **I want** a well-defined dump dataset format, **so that** DUMP and RESTORE are interoperable.

**Acceptance Criteria:**
- Given a dump dataset, when read, then a header identifies the dump (date, time, system, DFSMSdss version)
- Given a dump dataset with multiple datasets, when each is read, then catalog information and data are present for each

### DFSMS-106.7: DFSMSdss Tests

**Acceptance Criteria:**
- Given DUMP + RESTORE cycle for 5 datasets (sequential, PDS, VSAM KSDS), when complete, then all restored datasets match originals
- Given COPY with RENAME and REPLACE, when complete, then target datasets exist with correct data
- Given `cargo test` DFSMSdss tests, then all pass

---

## DFSMS-107: DFSMSrmm — Tape Management

**Description:** Implement tape volume lifecycle management — VRS (Vital Record Specifications) retention policies, scratch/private pool management, VRSEL retention evaluation, volume state machine.

**User Value:** Tape volumes are managed with retention policies, enabling automated tape recycling and ensuring critical data is retained per policy.

**Size:** M | **Stories:** 5

### DFSMS-107.1: Volume Registry and State Machine

**As a** tape administrator, **I want** a tape volume registry tracking volume state, **so that** tape lifecycle is managed.

**Acceptance Criteria:**
- Given a volume registered, when its lifecycle is tracked, then it transitions through states: SCRATCH → PRIVATE (allocated) → RETAINED → SCRATCH (recycled)
- Given a volume in PRIVATE state, when its retention expires, then it transitions to SCRATCH pool

### DFSMS-107.2: VRS (Vital Record Specifications)

**As a** tape administrator, **I want** VRS retention policies, **so that** tape data is retained based on defined rules.

**Acceptance Criteria:**
- Given a dataset VRS with retention=365 days, when a tape dataset matches, then the volume is retained for 365 days
- Given a volume VRS with WHILECATALOG, when the dataset remains cataloged, then the volume is retained indefinitely
- Given a name VRS with pattern PROD.BACKUP.**, when matching datasets are evaluated, then the VRS policy applies

### DFSMS-107.3: Scratch/Private Pool Management

**As a** tape administrator, **I want** scratch and private pool tracking, **so that** tape allocation uses available scratch volumes.

**Acceptance Criteria:**
- Given 100 scratch volumes in pool, when a new tape dataset is allocated, then a scratch volume is assigned and moved to private pool
- Given a private volume whose retention has expired, when VRSEL runs, then it moves to scratch pool

### DFSMS-107.4: VRSEL Processing

**As a** tape administrator, **I want** VRSEL (Vital Record Selection) batch processing, **so that** volume retention is evaluated periodically.

**Acceptance Criteria:**
- Given VRSEL runs against 1000 volumes, when each is checked against VRS policies, then expired volumes are moved to scratch, retained volumes stay private
- Given a volume with multiple datasets, when any dataset has active retention, then the volume remains private

### DFSMS-107.5: DFSMSrmm Tests

**Acceptance Criteria:**
- Given 10 volumes with various VRS policies, when VRSEL runs, then correct volumes transition to scratch
- Given `cargo test` DFSMSrmm tests, then all pass

---

## DFSMS-108: PDSE Program Objects & Member Generations

**Description:** Extend PDS implementation with PDSE-specific features — program object format (AMODE/RMODE attributes), member generations (version history, MAXGENS parameter), extended attributes.

**User Value:** Load module libraries support program objects with addressing mode attributes and member versioning, matching z/OS PDSE capabilities.

**Size:** M | **Stories:** 5

### DFSMS-108.1: Program Object Format

**As a** systems programmer, **I want** program objects with AMODE and RMODE attributes stored in PDSE load libraries, **so that** compiled programs have proper addressing mode metadata.

**Acceptance Criteria:**
- Given a program object with AMODE=31, RMODE=ANY, when stored in a PDSE, then the addressing mode attributes are preserved in the directory entry
- Given LISTDS command on a PDSE load library, when listing members, then AMODE/RMODE attributes are displayed

### DFSMS-108.2: Member Generations

**As a** developer, **I want** member generations for version history, **so that** previous versions of members can be accessed.

**Acceptance Criteria:**
- Given a PDSE created with MAXGENS=5, when member A is saved 6 times, then the current version plus 5 previous generations are retained
- Given `READ MEMBER(A) GENERATION(-2)`, when executed, then the version from 2 saves ago is returned
- Given MAXGENS reached, when a new version is saved, then the oldest generation is deleted

### DFSMS-108.3: Extended Attributes (EATTR)

**As a** systems programmer, **I want** EATTR=OPT for extended format PDSE, **so that** PDSE can hold more than 65,535 members.

**Acceptance Criteria:**
- Given PDSE with EATTR=OPT, when more than 65,535 members are added, then the directory supports them
- Given PDSE with EATTR=OPT, when individual members exceed 15MB, then large members are supported

### DFSMS-108.4: PDSE Directory Enhancements

**As a** developer, **I want** PDSE directory entries with extended user data, **so that** additional metadata is stored per member.

**Acceptance Criteria:**
- Given a PDSE directory entry, when program object attributes are stored, then class descriptors, deferred classes, and alias info are in the user data area

### DFSMS-108.5: PDSE Enhancement Tests

**Acceptance Criteria:**
- Given a PDSE with MAXGENS=3 and 5 member saves, then current + 3 generations are accessible
- Given program objects with AMODE 24/31/64 and RMODE 24/ANY, then attributes are correctly stored and retrieved
- Given `cargo test` PDSE tests, then all pass

---

## DFSMS-109: Space & Volume Management

**Description:** Implement space allocation model (primary/secondary in tracks/cylinders/bytes), VTOC (Volume Table of Contents), multi-volume datasets, and volume-level operations. Maps physical z/OS concepts to the filesystem-based implementation.

**User Value:** Dataset space allocation follows z/OS conventions with proper primary/secondary extents, volume tracking, and space management.

**Size:** L | **Stories:** 6

### DFSMS-109.1: Space Allocation Model

**As a** developer, **I want** space allocation in tracks, cylinders, bytes, and records, **so that** JCL SPACE parameters are processed correctly.

**Acceptance Criteria:**
- Given `SPACE=(TRK,(10,5))`, when a dataset is allocated, then primary space of 10 tracks (~560KB) and secondary of 5 tracks are allocated
- Given `SPACE=(CYL,(1,1))`, when allocated, then primary of 1 cylinder (~850KB for 3390) and secondary of 1 cylinder
- Given `SPACE=(27920,(100,50),RLSE)`, when allocated in bytes, then 100 blocks primary and the RLSE (release unused space) flag is set

### DFSMS-109.2: Extent Tracking

**As a** developer, **I want** primary and secondary extent tracking, **so that** datasets can grow beyond initial allocation.

**Acceptance Criteria:**
- Given a dataset allocated with SPACE=(TRK,(10,5)), when 10 tracks are filled, then a secondary extent of 5 tracks is allocated
- Given 16 extents (maximum for non-extended format), when the 17th is needed, then an error is returned (B37 ABEND equivalent)
- Given LISTCAT for a dataset, when extent information is displayed, then all extents are shown

### DFSMS-109.3: VTOC (Volume Table of Contents)

**As a** developer, **I want** a VTOC data structure per volume, **so that** volume-level dataset tracking follows z/OS conventions.

**Acceptance Criteria:**
- Given a volume VOL001, when its VTOC is read, then all datasets on that volume are listed with extent information
- Given a new dataset allocated on VOL001, when the allocation completes, then a DSCB (Dataset Control Block) entry is created in the VTOC
- Given DSCB Format 1 (dataset descriptor), when read, then it contains DSN, creation date, expiration date, RECFM, LRECL, BLKSIZE, extents

### DFSMS-109.4: Multi-Volume Datasets

**As a** developer, **I want** datasets that span multiple volumes, **so that** large datasets are supported.

**Acceptance Criteria:**
- Given a dataset with primary space exceeding volume capacity, when allocated, then secondary extents are placed on additional volumes
- Given a multi-volume dataset, when read sequentially, then data from all volumes is presented as a continuous stream

### DFSMS-109.5: Volume Operations

**As a** storage administrator, **I want** DEFRAG and RELEASE operations, **so that** volume space is managed efficiently.

**Acceptance Criteria:**
- Given a fragmented volume, when DEFRAG runs, then datasets are reorganized to reduce free-space fragmentation
- Given a dataset with RLSE (release) attribute, when closed, then unused allocated space is freed

### DFSMS-109.6: Space Management Tests

**Acceptance Criteria:**
- Given dataset allocation with TRK, CYL, and byte units, when allocated, then correct sizes are created
- Given secondary extent allocation up to 16 extents, then extent tracking works correctly
- Given `cargo test` space management tests, then all pass

---

## DFSMS-110: GDG-to-ICF Catalog Integration

**Description:** Wire existing GDG implementation into ICF catalog infrastructure — GDG base as catalog entry, generation catalog entries with proper aging, rolloff via catalog operations, LISTCAT GDG-specific output.

**User Value:** GDG (Generation Data Group) families are properly integrated with the ICF catalog, enabling standard catalog operations and IDCAMS management.

**Size:** S | **Stories:** 3

### DFSMS-110.1: GDG Base as ICF Catalog Entry

**As a** developer, **I want** GDG base entries stored in the ICF catalog, **so that** DEFINE GDG creates a proper catalog entry.

**Acceptance Criteria:**
- Given `DEFINE GDG (NAME(MY.GDG) LIMIT(10) SCRATCH)`, when executed, then a GDG base record is created in the appropriate user catalog (per alias) or master catalog
- Given LISTCAT LEVEL(MY.GDG), when executed, then the GDG base and all generations are listed with generation numbers

### DFSMS-110.2: Generation Catalog Entries

**As a** developer, **I want** each GDG generation stored as a catalog entry linked to its base, **so that** relative references resolve through the catalog.

**Acceptance Criteria:**
- Given MY.GDG(+1) in JCL, when resolved, then the catalog creates a new generation entry (GnnnnV00) linked to the GDG base
- Given MY.GDG(0) in JCL, when resolved, then the current (most recent) generation's catalog entry is returned
- Given MY.GDG(-2) in JCL, when resolved, then the generation from 2 versions ago is returned

### DFSMS-110.3: GDG-ICF Integration Tests

**Acceptance Criteria:**
- Given a GDG base with LIMIT=5 and 7 generations created, then only 5 generations remain cataloged after rolloff
- Given GDG integrated with ICF catalog, when LISTCAT ALL is issued, then GDG base and generation details are displayed
- Given `cargo test` GDG-ICF tests, then all pass

---

## Updated Dependency Graph

```
Epic 600 (VSAM AIX) ──────┐
Epic 606 (IDCAMS REPRO) ──┤
Epic 607 (Catalog) ────────┼──► DFSMS-102 (ICF Catalogs) ──► DFSMS-103 (IDCAMS Enhancements)
                           │                                ► DFSMS-110 (GDG-ICF Integration)
                           │
DFSMS-100 (SMS Model) ────┼──► DFSMS-101 (ACS Routines)
                           ├──► DFSMS-104 (HSM Migration) ──► DFSMS-105 (HSM Backup/ABARS)
                           ├──► DFSMS-107 (Tape Mgmt)
                           │
DFSMS-102 (ICF Catalogs) ─┼──► DFSMS-106 (DFSMSdss)
                           ├──► DFSMS-109 (Space/Volume)
                           │
Epic 601 (PDS) ────────────┼──► DFSMS-108 (PDSE Enhancements)
```

## Coverage Notes

**Dataset v3.0 + DFSMS v5.0 addendum covers:**
- VSAM (KSDS, ESDS, RRDS, LDS, AIX) → Epics 600-605 ✓
- PDS/PDSE basic → Epic 601 ✓
- PDSE advanced (program objects, member generations) → DFSMS-108 ✓
- IDCAMS → Epic 606 + DFSMS-103 ✓
- Catalog (flat→ICF) → Epic 607 + DFSMS-102 ✓
- BSAM/BPAM → Epic 608 ✓
- SMS policy engine → DFSMS-100, DFSMS-101 ✓
- DFSMShsm → DFSMS-104, DFSMS-105 ✓
- DFSMSdss → DFSMS-106 ✓
- DFSMSrmm → DFSMS-107 ✓
- Space/Volume management → DFSMS-109 ✓
- GDG-ICF integration → DFSMS-110 ✓

**Remaining deferred (low priority):**
- COMMDS (sysplex SMS config propagation) — multi-system feature
- ISMF panels — depends on TSO/ISPF
- Cloud ML2 (S3-compatible) — future extension
- Physical DUMP/RESTORE (track-level) — rarely needed in emulation
- FlashCopy / Concurrent Copy / SnapShot — hardware-specific features
- Striped datasets — performance optimization
- Compressed datasets — hardware-assisted

## Summary

| Metric | Value |
|--------|-------|
| New epics | 11 (DFSMS-100 through DFSMS-110) |
| New stories | 68 |
| Combined with v3.0 | 20 epics (600-608 + DFSMS-100-110), 87 stories |
| FR coverage | 82+ missing features addressed |
| Gap batch covered | Batch 19 (DFSMS + Catalog) |
