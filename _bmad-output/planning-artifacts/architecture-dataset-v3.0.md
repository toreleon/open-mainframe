# Dataset Crate — Architecture Decisions

## AD-3.0-01: Alternate Index as Secondary B+ Tree

**Context:** VSAM alternate indexes (AIX) provide secondary access paths to KSDS and ESDS datasets. An AIX is itself a KSDS where the key is the alternate key field and the data portion contains the primary key (for KSDS) or RBA (for ESDS). AIXs can be unique or non-unique (allowing duplicate alternate keys).

**Decision:** Implement AIX as a separate B+ tree (`BPlusTree<Vec<u8>, Vec<Vec<u8>>>`) where the key is the alternate key value and the value is a list of primary keys (for non-unique) or a single primary key (for unique). AIX maintenance is automatic: when a record is inserted/updated/deleted in the base cluster, the corresponding AIX entries are updated. A PATH object connects an AIX to its base cluster for transparent access.

**Consequences:**
- Each AIX adds insert/update/delete overhead to the base cluster
- Non-unique AIXs require vec-of-keys storage per alternate key
- PATH provides a unified access interface (read via AIX, write to base)
- DEFINE ALTERNATEINDEX and DEFINE PATH become new IDCAMS commands
- BLDINDEX command may be needed for building AIX from existing data

## AD-3.0-02: PDS Directory as In-Memory B-Tree

**Context:** A PDS (Partitioned Data Set) consists of a directory and member data. The directory is a sorted list of member entries with 8-character names, TTR pointers (track and record addresses), and optional user data (ISPF statistics). The PDSE (Extended) format adds improvements like member-level sharing and larger sizes.

**Decision:** Implement PDS as a directory file (JSON or binary) alongside a data directory on the filesystem. Each member is a separate file in the data directory. The directory is loaded into memory as a sorted `Vec<PdsMember>` for fast lookup. Member operations (add, delete, rename, list) update both the in-memory directory and the on-disk files. PDSE is treated identically to PDS for this implementation (no distinction in access patterns).

**Consequences:**
- Simple filesystem mapping (directory file + member files in subdirectory)
- Member I/O uses QSAM reader/writer for each member
- Directory listing is O(n) scan (adequate for typical PDS sizes)
- No TTR addressing (filesystem paths replace physical track references)
- ISPF statistics (change date, user ID, line count) stored in directory metadata

## AD-3.0-03: Record-Level Locking via File System Locks

**Context:** VSAM Record Level Sharing (RLS) in z/OS uses the Coupling Facility for cross-system locking. For the OpenMainframe platform, we need a simpler mechanism that supports concurrent access to VSAM datasets from multiple program invocations.

**Decision:** Implement record-level locking using a lock file alongside each VSAM cluster. The lock file maps key values (or RBAs/RRNs) to lock holders (process ID + timestamp). Shared locks allow concurrent readers; exclusive locks block all other access. Lock acquisition uses file-based advisory locking (flock) for process-level coordination. Lock timeout prevents deadlocks.

**Consequences:**
- Works across processes on the same machine
- Lock file adds I/O overhead per record access
- No cross-machine sharing (would need distributed lock manager)
- Stale locks (from crashed processes) detected via timestamp + process existence check
- DISP=SHR maps to shared lock; DISP=OLD maps to exclusive lock on entire dataset

## AD-3.0-04: LDS as Memory-Mapped File

**Context:** VSAM Linear Data Sets store data as a sequence of 4KB pages without record structure. They are used as memory-mapped files by DB2, DFSORT, and z/OS UNIX zFS. Access is by relative byte address within the dataset.

**Decision:** Implement LDS as a file with 4096-byte page alignment. Read/write operations address pages by offset. Use `mmap` (memory-mapped I/O) when available for performance, falling back to standard file I/O. The LDS has no record structure — callers read/write arbitrary byte ranges aligned to page boundaries.

**Consequences:**
- Simple implementation — essentially a page-addressed binary file
- Memory mapping provides fast random access for large datasets
- No record format validation (callers manage their own data layout)
- CI size is always 4096 bytes (or multiples thereof)
- Useful as backing store for DB2 tablespaces and other subsystems

## AD-3.0-05: Free Space as CI/CA Split Tracking

**Context:** KSDS FREESPACE parameter reserves space in each CI and CA for future insertions. When a CI is full and an insert is needed, a CI split moves half the records to a new CI. When a CA is full, a CA split creates a new CA.

**Decision:** Track free space at the CI level by maintaining a per-CI occupancy map. The `FREESPACE(ci_pct, ca_pct)` parameter reserves ci_pct% of each CI and ca_pct% of each CA during initial load. During runtime, CI splits are triggered when an insert finds no free space. The split creates a new CI, moves the upper half of records, and updates the index. CA splits extend the data component.

**Consequences:**
- Improves insert performance by reducing splits
- Initial load pre-allocates space (larger initial file size)
- CI/CA split tracking requires additional metadata per CI
- The existing B+ tree implementation handles index updates during splits
- Free space statistics are reportable via LISTCAT
