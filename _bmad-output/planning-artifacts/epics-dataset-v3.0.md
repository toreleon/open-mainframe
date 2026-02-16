# Dataset Crate — Epics & Stories

## Epic 600: VSAM Alternate Index Support

**Goal:** Implement alternate indexes (AIX) for KSDS and ESDS, enabling multi-key access paths to VSAM datasets.

**Crate:** `open-mainframe-dataset`
**FRs:** FR-v3.0-600

### Story 600.1: Alternate Index Data Structure

As a **VSAM developer**,
I want **an AlternateIndex type backed by a secondary B+ tree**,
So that **I can look up base cluster records by alternate key values**.

**Acceptance Criteria:**

**Given** a KSDS base cluster with records containing a NAME field at offset 10, length 30
**When** an AIX is defined with key offset=10, key length=30, uniquekey=true
**Then** a secondary B+ tree is created mapping alternate key → primary key

**Given** a non-unique AIX
**When** two records have the same alternate key value
**Then** the AIX stores both primary keys under that alternate key

**Complexity:** L

### Story 600.2: Automatic AIX Maintenance

As a **VSAM developer**,
I want **AIX entries to be automatically updated when base cluster records change**,
So that **the AIX stays consistent with the base data**.

**Acceptance Criteria:**

**Given** a KSDS with a defined AIX
**When** a record is inserted into the base cluster
**Then** the corresponding AIX entry is created automatically

**Given** a record update that changes the alternate key field
**When** the REWRITE completes
**Then** the old AIX entry is removed and a new one is created

**Given** a record is deleted from the base cluster
**When** the DELETE completes
**Then** the corresponding AIX entry is removed

**Complexity:** L

### Story 600.3: PATH and IDCAMS Integration

As a **system administrator**,
I want **DEFINE ALTERNATEINDEX and DEFINE PATH in IDCAMS**,
So that **I can create and manage alternate indexes via standard utilities**.

**Acceptance Criteria:**

**Given** `DEFINE ALTERNATEINDEX (NAME(MY.AIX) RELATE(MY.BASE) KEYS(30 10) UNIQUEKEY)`
**When** the IDCAMS command executes
**Then** an AIX is created linked to MY.BASE with the specified key definition

**Given** `DEFINE PATH (NAME(MY.PATH) PATHENTRY(MY.AIX))`
**When** the command executes
**Then** reads through MY.PATH use the AIX key for lookup and return base cluster records

**Complexity:** M

---

## Epic 601: PDS/PDSE Member Management

**Goal:** Implement partitioned dataset directory and member operations for source libraries, copybooks, and load modules.

**Crate:** `open-mainframe-dataset`
**FRs:** FR-v3.0-601

### Story 601.1: PDS Directory Structure

As a **developer**,
I want **a PDS directory that stores member names, pointers, and optional statistics**,
So that **I can manage collections of related members like a source library**.

**Acceptance Criteria:**

**Given** a new PDS created with DSORG=PO
**When** the dataset is initialized
**Then** an empty directory is created on disk alongside a member data directory

**Given** a PDS with members MAIN, SUB1, COPYBK
**When** the directory is listed
**Then** all three members are returned in sorted order with their attributes

**Complexity:** M

### Story 601.2: Member I/O Operations

As a **developer**,
I want **to add, read, update, and delete individual PDS members**,
So that **I can manage COBOL source, copybooks, and JCL procedures in partitioned datasets**.

**Acceptance Criteria:**

**Given** an empty PDS
**When** I add member "MAINPGM" with COBOL source content
**Then** the member is stored and the directory is updated

**Given** a PDS containing member "MAINPGM"
**When** I read member "MAINPGM"
**Then** the full content is returned as sequential records

**Given** a PDS member "OLD"
**When** I rename it to "NEW"
**Then** the directory entry is updated; content is unchanged

**Given** a PDS member "TEMP"
**When** I delete it
**Then** the directory entry and data file are removed

**Complexity:** M

### Story 601.3: ISPF-Style Statistics

As a **developer**,
I want **PDS directory entries to carry ISPF statistics (create date, change date, user ID, line count)**,
So that **member metadata matches what ISPF users expect to see**.

**Acceptance Criteria:**

**Given** a member added with ISPF statistics
**When** the directory is listed
**Then** each entry shows creation date, modification date, user ID, version, and line counts

**Complexity:** S

---

## Epic 602: VSAM Linear Data Set (LDS)

**Goal:** Implement LDS for page-level I/O used by DB2 tablespaces and other subsystem backing stores.

**Crate:** `open-mainframe-dataset`
**FRs:** FR-v3.0-602

### Story 602.1: LDS Create and Page I/O

As a **developer**,
I want **a Linear Data Set type with 4096-byte page-aligned read/write**,
So that **subsystems like DB2 can use LDS as persistent page storage**.

**Acceptance Criteria:**

**Given** a new LDS defined with `DEFINE CLUSTER (NAME(MY.LDS) LINEAR)`
**When** the IDCAMS command executes
**Then** an empty page-aligned file is created

**Given** an open LDS
**When** I write 4096 bytes at page offset 0
**Then** the data is persisted and readable at page offset 0

**Given** an LDS with 100 pages
**When** I read page 50
**Then** exactly 4096 bytes are returned starting at byte offset 204800

**Complexity:** M

### Story 602.2: LDS Extend and Space Management

As a **developer**,
I want **LDS to grow dynamically as pages are written beyond the current size**,
So that **callers do not need to pre-allocate the full dataset**.

**Acceptance Criteria:**

**Given** an LDS currently holding 10 pages
**When** I write to page 20
**Then** the LDS extends to at least 21 pages (pages 10-19 are zero-filled)

**Complexity:** S

---

## Epic 603: Concurrent Access and Record Locking

**Goal:** Enable safe concurrent access to VSAM datasets with record-level locking.

**Crate:** `open-mainframe-dataset`
**FRs:** FR-v3.0-603

### Story 603.1: Record-Level Lock Manager

As a **developer**,
I want **shared and exclusive locks on individual VSAM records**,
So that **multiple programs can safely access the same dataset concurrently**.

**Acceptance Criteria:**

**Given** process A acquires an exclusive lock on key "CUST001"
**When** process B attempts to read "CUST001" with exclusive lock
**Then** process B blocks until process A releases the lock

**Given** process A holds a shared lock on key "CUST001"
**When** process B requests a shared lock on the same key
**Then** process B succeeds immediately (shared locks are compatible)

**Complexity:** L

### Story 603.2: Dataset-Level DISP Locking

As a **developer**,
I want **DISP=OLD to acquire exclusive access and DISP=SHR to acquire shared access at the dataset level**,
So that **JCL disposition semantics are enforced**.

**Acceptance Criteria:**

**Given** a dataset opened with DISP=OLD
**When** another process tries to open the same dataset
**Then** the second open fails or waits (exclusive access)

**Given** a dataset opened with DISP=SHR
**When** another process opens it with DISP=SHR
**Then** both succeed (shared read access)

**Complexity:** M

### Story 603.3: Stale Lock Detection and Timeout

As a **developer**,
I want **stale locks from crashed processes to be detected and cleaned up**,
So that **datasets do not remain permanently locked after failures**.

**Acceptance Criteria:**

**Given** process A locked key "CUST001" and then crashed
**When** process B requests a lock on "CUST001"
**Then** the stale lock is detected (process A no longer exists) and released

**Given** a lock request with a 5-second timeout
**When** the lock is not available within 5 seconds
**Then** an error is returned instead of blocking forever

**Complexity:** M

---

## Epic 604: VSAM Free Space Management

**Goal:** Implement FREESPACE parameter for KSDS to reduce CI/CA splits during insertions.

**Crate:** `open-mainframe-dataset`
**FRs:** FR-v3.0-604

### Story 604.1: FREESPACE Parameter and Initial Load

As a **VSAM developer**,
I want **FREESPACE(ci_pct, ca_pct) to reserve space during initial dataset load**,
So that **subsequent inserts have room without triggering splits**.

**Acceptance Criteria:**

**Given** `DEFINE CLUSTER (... FREESPACE(20 10))`
**When** records are loaded during initial REPRO
**Then** 20% of each CI and 10% of each CA are left free

**Complexity:** M

### Story 604.2: CI/CA Split Tracking

As a **VSAM developer**,
I want **CI splits to occur when a full CI needs an insert, moving half the records to a new CI**,
So that **inserts into sorted datasets are handled correctly**.

**Acceptance Criteria:**

**Given** a CI at 100% capacity
**When** a new record is inserted with a key that belongs in this CI
**Then** the CI splits: lower half stays, upper half moves to a new CI, index is updated

**Given** LISTCAT for a KSDS
**When** the statistics are displayed
**Then** CI split count and CA split count are reported

**Complexity:** L

---

## Epic 605: Spanned Record Support

**Goal:** Implement variable-length spanned records that can exceed the CI size.

**Crate:** `open-mainframe-dataset`
**FRs:** FR-v3.0-605

### Story 605.1: Spanned Record Read/Write

As a **VSAM developer**,
I want **records larger than the CI size to span multiple CIs**,
So that **I can store large records in VSAM datasets**.

**Acceptance Criteria:**

**Given** a KSDS with CI size 4096 bytes
**When** a 10,000-byte record is written
**Then** the record spans 3 CIs with segment descriptor words linking them

**Given** a spanned record stored across 3 CIs
**When** a READ retrieves it by key
**Then** the complete 10,000-byte record is assembled and returned

**Complexity:** L

### Story 605.2: Record Format Validation for VS/VBS

As a **developer**,
I want **VS and VBS record formats to be validated and enforced**,
So that **spanned records are only allowed when the format permits it**.

**Acceptance Criteria:**

**Given** a dataset defined with RECFM=VS
**When** a record larger than CI size is written
**Then** the record is accepted and stored as spanned

**Given** a dataset defined with RECFM=V (not spanned)
**When** a record larger than CI size is written
**Then** an error is returned (record too large)

**Complexity:** S

---

## Epic 606: IDCAMS REPRO Filtering

**Goal:** Implement selective copy filters on the REPRO command.

**Crate:** `open-mainframe-dataset`
**FRs:** FR-v3.0-606

### Story 606.1: Key-Range and Count Filtering

As a **system administrator**,
I want **REPRO with FROMKEY/TOKEY/COUNT/SKIP to copy a subset of records**,
So that **I can extract specific ranges from large datasets**.

**Acceptance Criteria:**

**Given** `REPRO INFILE(BIG) OUTFILE(SMALL) FROMKEY('A') TOKEY('M')`
**When** the command executes
**Then** only records with keys from 'A' through 'M' (inclusive) are copied

**Given** `REPRO INFILE(BIG) OUTFILE(SMALL) SKIP(100) COUNT(50)`
**When** the command executes
**Then** 50 records are copied starting from the 101st record

**Complexity:** M

---

## Epic 607: Catalog Persistence and Discovery

**Goal:** Make the dataset catalog persistent and support filesystem-based dataset discovery.

**Crate:** `open-mainframe-dataset`
**FRs:** FR-v3.0-607

### Story 607.1: Persistent Catalog Storage

As a **system administrator**,
I want **the dataset catalog to persist to disk and reload on startup**,
So that **dataset registrations survive across program invocations**.

**Acceptance Criteria:**

**Given** a catalog with 100 registered datasets
**When** the catalog is saved
**Then** all entries are persisted to a catalog file

**Given** a saved catalog file
**When** the system starts up
**Then** all previously registered datasets are available

**Complexity:** M

### Story 607.2: Filesystem Scanning for Dataset Discovery

As a **system administrator**,
I want **the catalog to scan configured directories for datasets matching naming conventions**,
So that **datasets created by other processes are automatically discovered**.

**Acceptance Criteria:**

**Given** a directory `/data/vsam/` containing KSDS files
**When** catalog.list("**") is called
**Then** all datasets in the directory are returned (not just manually registered ones)

**Complexity:** S

---

## Epic 608: BSAM and BPAM Access Methods

**Goal:** Implement block-level access methods for system utilities and low-level programs.

**Crate:** `open-mainframe-dataset`
**FRs:** FR-v3.0-608

### Story 608.1: BSAM Block-Level Sequential I/O

As a **developer**,
I want **BSAM for reading and writing physical blocks (not logical records)**,
So that **system utilities can process data at the block level**.

**Acceptance Criteria:**

**Given** a dataset with BLKSIZE=27920, LRECL=80, RECFM=FB
**When** BSAM reads a block
**Then** the full 27920-byte block is returned (containing 349 records)

**Given** a BSAM write of a 27920-byte block
**When** the block is written
**Then** it is stored as a physical block on disk

**Complexity:** M

### Story 608.2: BPAM Directory Access

As a **developer**,
I want **BPAM for reading PDS directory blocks**,
So that **utilities can enumerate PDS members at the physical level**.

**Acceptance Criteria:**

**Given** a PDS with 10 members
**When** BPAM reads the directory
**Then** directory blocks are returned with 8-byte member names and TTR pointers

**Complexity:** M
