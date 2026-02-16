# Dataset Crate — Product Requirements

## Overview

The `open-mainframe-dataset` crate provides z/OS dataset and VSAM (Virtual Storage Access Method) support for the OpenMainframe platform. It includes QSAM sequential file I/O, VSAM KSDS/ESDS/RRDS with B+ tree indexing, GDG (Generation Data Group) management, a dataset catalog, and an IDCAMS utility implementation.

## Current State Assessment

- **Lines of code:** 7,155
- **Test count:** 99 (all passing)
- **Maturity:** High (VSAM core, GDG, IDCAMS), Moderate (catalog, types)
- **Files:** 17 Rust source files across 5 modules (vsam, gdg, idcams, catalog, qsam, types, error)

### What Works Well

**VSAM Key-Sequenced Dataset (KSDS):**
- B+ tree indexing for O(log n) key-based access
- Point lookups by exact key
- Generic (partial) key reads
- Sequential read in key order (forward and backward)
- Insert, Update (rewrite), Delete with automatic rebalancing
- IBM-compatible FileStatus codes (00, 10, 21-23, 35, 41, 43-44, 46-48, 90, 92-93)
- Persistent binary storage format with 128-byte header
- Index rebuild capability

**VSAM Entry-Sequenced Dataset (ESDS):**
- Arrival sequence record storage
- RBA (Relative Byte Address) access
- Sequential read from any RBA
- Record appending with automatic RBA assignment
- Complete record retrieval

**VSAM Relative Record Dataset (RRDS):**
- Slot-based storage (RRN = Relative Record Number)
- Sparse slot allocation
- Read/Write/Delete by slot number
- Sequential enumeration of occupied slots
- Persistent slot occupancy tracking

**Sequential File I/O (QSAM):**
- Fixed-length records (F, FB)
- Variable-length records with RDW (V, VB)
- Undefined/line-based records (U)
- Buffered I/O with configurable block sizes
- Open modes: Input, Output, Extend, InputOutput

**Generation Data Groups (GDGs):**
- GDG base creation with configurable options (limit, scratch, empty)
- Generation numbering (GxxxxVyy format)
- Automatic generation rolloff by limit
- Relative reference support (0, -1, -2, etc.)
- Absolute reference support (G0001V00)
- Persistent catalog with binary format

**Dataset Catalog:**
- Hierarchical name resolution (DSN → filesystem path)
- Wildcard pattern matching
- Extension-based attribute inference
- PDS member lookup

**IDCAMS Utility:**
- DEFINE CLUSTER (KSDS, ESDS, RRDS)
- DEFINE GDG
- DELETE, ALTER (rename), LISTCAT, PRINT, REPRO, VERIFY
- Command parsing with continuation support

**Data Types:**
- All IBM DCB record formats (F, FB, V, VB, U, VS, VBS)
- 7 dataset organizations (PS, PO, DA, IS, ESDS, KSDS, RRDS)
- Disposition specifications (New, Old, Shr, Mod)
- Record format validation

### What Does NOT Work

- No VSAM Linear Data Set (LDS) support
- No alternate indexes (AIX) for any VSAM type
- No PDS/PDSE member management (directory structure, member add/delete/rename)
- No BSAM (Basic Sequential Access Method) support
- No BPAM (Basic Partitioned Access Method) support
- No VSAM control interval (CI) / control area (CA) management
- No VSAM free space management (FREESPACE parameter)
- No VSAM spanned record support (VS, VBS parsed but not validated/used)
- No record compression
- No concurrent access / record locking
- No IDCAMS DEFINE ALTERNATEINDEX
- No IDCAMS DEFINE NONVSAM / DEFINE ALIAS
- No IDCAMS REPRO with FROMKEY/TOKEY filtering (parsed but not used)
- No filesystem scanning in catalog list()
- No catalog persistence to external file
- No DASD volume / extent management
- No dynamic space allocation
- No SMS (Storage Management Subsystem) integration
- No crash recovery for partial writes

## Functional Requirements

### FR-v3.0-600: VSAM Alternate Index Support
Implement DEFINE ALTERNATEINDEX for KSDS and ESDS. Support unique and non-unique alternate keys, PATH for alternate index access, and automatic AIX maintenance on base cluster updates.
- **Priority:** CRITICAL
- **Gap Type:** Missing feature
- **IBM Reference:** DFSMS VSAM Guide — Alternate Indexes. AIXs are widely used in CICS applications for multi-key access to VSAM datasets. Both IMS and CICS rely heavily on alternate indexes.

### FR-v3.0-601: PDS/PDSE Member Management
Implement partitioned dataset directory operations: add member, delete member, rename member, list directory, and member-level I/O. Support PDS member aliases and ISPF-style statistics.
- **Priority:** CRITICAL
- **Gap Type:** Missing feature
- **IBM Reference:** DFSMS Using Data Sets — Partitioned Data Sets. PDS and PDSE are fundamental to z/OS — all load modules, JCL procedures, COBOL copybooks, and source code are stored in PDS/PDSE.

### FR-v3.0-602: VSAM Linear Data Set (LDS)
Implement LDS with 4096-byte CI access. LDS provides memory-mapped file semantics used by DB2, DFSORT, and other system components.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** DFSMS VSAM Guide — Linear Data Sets. LDS is used by DB2 for tablespace storage, by DFSMShsm for migration, and by z/OS UNIX zFS for file system storage.

### FR-v3.0-603: Concurrent Access and Record Locking
Implement shared and exclusive record-level locking for VSAM datasets. Support DISP=SHR for read sharing and DISP=OLD for exclusive access.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** DFSMS VSAM Guide — Sharing VSAM Data Sets. Record-level sharing (RLS) is essential for CICS file control and multi-user environments.

### FR-v3.0-604: VSAM Free Space Management
Implement the FREESPACE parameter (CI%, CA%) for KSDS. Maintain free space during initial load and CI/CA splits.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** DFSMS VSAM Guide — Free Space. Free space reduces CI/CA splits, improving insert performance for KSDS.

### FR-v3.0-605: Spanned Record Support
Implement variable-length spanned records (VS, VBS) that span multiple control intervals. Support records larger than the CI size.
- **Priority:** MAJOR
- **Gap Type:** Incomplete implementation (parsed but not validated/used)
- **IBM Reference:** DFSMS VSAM Guide — Spanned Records. Spanned records allow variable-length records to exceed CI size, used for large records in ESDS and KSDS.

### FR-v3.0-606: IDCAMS REPRO Filtering
Implement FROMKEY/TOKEY, FROMADDRESS/TOADDRESS, COUNT, and SKIP parameters on REPRO for selective dataset copy.
- **Priority:** MINOR
- **Gap Type:** Incomplete implementation (parsed but not used)
- **IBM Reference:** DFSMS Access Method Services Reference — REPRO command. Filtered copy is commonly used for dataset extraction and subsetting.

### FR-v3.0-607: Catalog Persistence and Filesystem Scanning
Implement persistent catalog storage (save/load catalog to file) and filesystem scanning in the list() method to discover datasets.
- **Priority:** MINOR
- **Gap Type:** Incomplete implementation (TODO in code)
- **IBM Reference:** DFSMS Managing Catalogs — ICF Catalog. z/OS catalogs are persistent structures that track all datasets.

### FR-v3.0-608: BSAM and BPAM Access Methods
Implement BSAM (Basic Sequential Access Method) for block-level I/O and BPAM (Basic Partitioned Access Method) for PDS directory access. These are lower-level interfaces used by system utilities.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** DFSMS Using Data Sets — Sequential and Partitioned Data Sets. BSAM/BPAM are used by system utilities and assembler programs. Most COBOL programs use QSAM instead.
