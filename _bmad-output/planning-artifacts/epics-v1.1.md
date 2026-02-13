---
version: 'v1.1'
baseVersion: 'v1.0'
date: '2026-02-13'
status: 'draft'
inputDocuments: [prd-v1.1.md, architecture-v1.1.md]
totalEpics: 6
totalStories: 42
frCoverage: '53/53'
---

# zOS-clone v1.1 - Epic Breakdown

## Overview

This document provides the epic and story breakdown for zOS-clone v1.1 (Batch Workload Ready), building on the MVP foundation.

## Requirements Inventory

### Functional Requirements (FR-v1.1)

**VSAM File Support (FR-v1.1-001 to FR-v1.1-014)**
- FR-v1.1-001: KSDS cluster definition
- FR-v1.1-002: KSDS keyed READ
- FR-v1.1-003: KSDS keyed WRITE
- FR-v1.1-004: KSDS REWRITE/DELETE
- FR-v1.1-005: KSDS sequential READ
- FR-v1.1-006: ESDS cluster definition
- FR-v1.1-007: ESDS sequential I/O
- FR-v1.1-008: ESDS RBA access
- FR-v1.1-009: RRDS cluster definition
- FR-v1.1-010: RRDS keyed access
- FR-v1.1-011: VSAM file status codes
- FR-v1.1-012: VSAM cluster attributes
- FR-v1.1-013: VSAM via JCL DD
- FR-v1.1-014: VSAM reference in JCL

**SORT Utility (FR-v1.1-015 to FR-v1.1-025)**
- FR-v1.1-015: SORT via JCL
- FR-v1.1-016: SORT FIELDS
- FR-v1.1-017: INCLUDE/OMIT
- FR-v1.1-018: OUTREC
- FR-v1.1-019: INREC
- FR-v1.1-020: SUM
- FR-v1.1-021: Multiple inputs
- FR-v1.1-022: MERGE
- FR-v1.1-023: COPY
- FR-v1.1-024: CLI sort
- FR-v1.1-025: DFSORT syntax

**GDG Support (FR-v1.1-026 to FR-v1.1-034)**
- FR-v1.1-026: GDG base definition
- FR-v1.1-027: Relative generation (+1, -1, 0)
- FR-v1.1-028: Absolute generation
- FR-v1.1-029: Create new generation
- FR-v1.1-030: Access current generation
- FR-v1.1-031: Access previous generations
- FR-v1.1-032: GDG limit rolloff
- FR-v1.1-033: LISTCAT GDG
- FR-v1.1-034: Delete generations

**IDCAMS Utility (FR-v1.1-035 to FR-v1.1-044)**
- FR-v1.1-035: IDCAMS via JCL
- FR-v1.1-036: DEFINE CLUSTER
- FR-v1.1-037: DEFINE GDG
- FR-v1.1-038: DELETE
- FR-v1.1-039: ALTER
- FR-v1.1-040: LISTCAT
- FR-v1.1-041: PRINT
- FR-v1.1-042: REPRO
- FR-v1.1-043: VERIFY
- FR-v1.1-044: CLI idcams

**Package Distribution (FR-v1.1-045 to FR-v1.1-053)**
- FR-v1.1-045: apt install
- FR-v1.1-046: yum install
- FR-v1.1-047: Package upgrade
- FR-v1.1-048: Repository config
- FR-v1.1-049: Man pages
- FR-v1.1-050: Shell completions
- FR-v1.1-051: CLI sort command
- FR-v1.1-052: CLI idcams command
- FR-v1.1-053: CLI gdg command

## Epic List

| Epic | Title | Stories | Key FRs | Crate(s) |
|------|-------|---------|---------|----------|
| 15 | VSAM Core Infrastructure | 8 | FR-v1.1-001 to FR-v1.1-005, FR-v1.1-011-012 | zos-dataset |
| 16 | VSAM ESDS & RRDS | 5 | FR-v1.1-006 to FR-v1.1-010 | zos-dataset |
| 17 | SORT Utility | 9 | FR-v1.1-015 to FR-v1.1-025 | zos-sort |
| 18 | GDG Support | 7 | FR-v1.1-026 to FR-v1.1-034 | zos-dataset |
| 19 | IDCAMS Utility | 8 | FR-v1.1-035 to FR-v1.1-044 | zos-dataset |
| 20 | Package Distribution | 5 | FR-v1.1-045 to FR-v1.1-053 | CI/CD |

**Total: 6 Epics, 42 Stories**

---

## Epic 15: VSAM Core Infrastructure

**Goal:** Implement KSDS (Key-Sequenced Data Set) support with B+ tree indexing as the foundation for all VSAM operations.

**Crate:** `zos-dataset/vsam`
**FRs:** FR-v1.1-001 to FR-v1.1-005, FR-v1.1-011 to FR-v1.1-014
**Depends on:** MVP Epic 8 (Dataset & File Operations)

### Story 15.1: Implement B+ Tree Index Structure

As a **developer**,
I want **a B+ tree implementation for KSDS indexes**,
So that **I can efficiently locate records by key**.

**Acceptance Criteria:**

**Given** an empty B+ tree with order 100
**When** I insert 10,000 records with sequential keys
**Then** all records are retrievable by key in O(log n) time
**And** the tree remains balanced

**Given** a B+ tree with records
**When** I perform a range query (key >= X AND key <= Y)
**Then** I can iterate through matching records via leaf node links

**Given** a B+ tree
**When** I delete a key
**Then** the tree rebalances correctly and the key is no longer found

**Complexity:** L
**Supports:** FR-v1.1-002, NFR-v1.1-P1

---

### Story 15.2: Implement VSAM Cluster Definition

As a **developer**,
I want **to define VSAM KSDS clusters with key specifications**,
So that **I can create indexed datasets**.

**Acceptance Criteria:**

**Given** cluster parameters (name, key offset, key length, record size)
**When** I call `VsamCluster::new_ksds(params)`
**Then** a cluster metadata structure is created
**And** the key specification is validated (offset + length <= record size)

**Given** a KSDS cluster definition
**When** I create the cluster on disk
**Then** a `.vsam` file is created with proper header
**And** the catalog is updated with cluster information

**Given** invalid parameters (e.g., key length > record size)
**When** I attempt cluster creation
**Then** a descriptive error is returned

**Complexity:** M
**Supports:** FR-v1.1-001, FR-v1.1-012

---

### Story 15.3: Implement KSDS Keyed Read

As a **developer**,
I want **to read records by key from a KSDS**,
So that **I can retrieve specific records efficiently**.

**Acceptance Criteria:**

**Given** a KSDS with records
**When** I call `ksds.read_key(b"12345")`
**Then** I receive the record with that exact key
**And** file status 00 is set

**Given** a KSDS
**When** I read a key that doesn't exist
**Then** None is returned
**And** file status 23 (record not found) is set

**Given** a partial key for generic read
**When** I call `ksds.read_key_generic(b"123")`
**Then** I receive the first record where key starts with "123"

**Complexity:** M
**Supports:** FR-v1.1-002, FR-v1.1-011

---

### Story 15.4: Implement KSDS Keyed Write

As a **developer**,
I want **to write records with keys to a KSDS**,
So that **I can add new indexed records**.

**Acceptance Criteria:**

**Given** an open KSDS in output mode
**When** I write a record with a new key
**Then** the record is stored and indexed
**And** file status 00 is set

**Given** a KSDS with existing records
**When** I write a record with a duplicate key
**Then** the write fails
**And** file status 22 (duplicate key) is set

**Given** a record larger than RECORDSIZE MAX
**When** I attempt to write
**Then** an error is returned with file status 44

**Complexity:** M
**Supports:** FR-v1.1-003, FR-v1.1-011

---

### Story 15.5: Implement KSDS Update and Delete

As a **developer**,
I want **to update and delete records in a KSDS**,
So that **I can modify existing indexed data**.

**Acceptance Criteria:**

**Given** a KSDS open for I/O
**When** I read a record, modify it, and call REWRITE
**Then** the record is updated in place
**And** the key cannot change (error if attempted)

**Given** a KSDS open for I/O
**When** I read a record and call DELETE
**Then** the record is removed from data and index
**And** file status 00 is set

**Given** an attempt to REWRITE/DELETE without prior READ
**When** the operation is attempted
**Then** file status 43 is set (no current record)

**Complexity:** M
**Supports:** FR-v1.1-004, FR-v1.1-011

---

### Story 15.6: Implement KSDS Sequential Access

As a **developer**,
I want **to read KSDS records sequentially**,
So that **I can process all records in key order**.

**Acceptance Criteria:**

**Given** a KSDS with records
**When** I call START with a key and READ NEXT repeatedly
**Then** records are returned in ascending key order starting from the START key

**Given** a KSDS positioned at a record
**When** I call READ NEXT past the last record
**Then** end-of-file condition is indicated (file status 10)

**Given** START with KEY >= "ABC"
**When** no records have key >= "ABC"
**Then** file status 23 is set (key not found)

**Complexity:** M
**Supports:** FR-v1.1-005, FR-v1.1-011

---

### Story 15.7: Implement VSAM File Status Codes

As a **developer**,
I want **proper VSAM file status codes returned**,
So that **COBOL programs can handle VSAM errors correctly**.

**Acceptance Criteria:**

**Given** any VSAM operation
**When** it completes
**Then** the file status is set according to IBM documentation

**File Status Code Mapping:**
- 00: Successful completion
- 10: End of file
- 22: Duplicate key
- 23: Record not found
- 35: File not found
- 43: No current record for REWRITE/DELETE
- 44: Record length violation
- 46: Read past end of sequential read
- 47: File not open for input
- 48: File not open for output
- 92: Logic error
- 93: Resource unavailable

**Complexity:** S
**Supports:** FR-v1.1-011

---

### Story 15.8: Implement VSAM JCL Integration

As an **operations engineer**,
I want **to allocate and reference VSAM clusters in JCL**,
So that **batch jobs can use VSAM datasets**.

**Acceptance Criteria:**

**Given** JCL with DD referencing existing VSAM cluster
```jcl
//VSAMDD   DD DSN=MY.KSDS.CLUSTER,DISP=SHR
```
**When** the job step executes
**Then** the VSAM cluster is opened and available to the program

**Given** JCL with AMP parameters
```jcl
//VSAMDD   DD DSN=MY.KSDS,DISP=SHR,AMP=(BUFND=10,BUFNI=5)
```
**When** the DD is processed
**Then** buffer parameters are applied to VSAM open

**Complexity:** M
**Supports:** FR-v1.1-013, FR-v1.1-014

---

## Epic 16: VSAM ESDS & RRDS

**Goal:** Extend VSAM support to Entry-Sequenced and Relative Record datasets.

**Crate:** `zos-dataset/vsam`
**FRs:** FR-v1.1-006 to FR-v1.1-010

### Story 16.1: Implement ESDS Cluster

As a **developer**,
I want **to create ESDS (Entry-Sequenced) clusters**,
So that **I can store records in arrival order**.

**Acceptance Criteria:**

**Given** ESDS cluster parameters (no key specification needed)
**When** I create the cluster
**Then** an ESDS file is created
**And** records can be written sequentially

**Given** an ESDS
**When** I write records
**Then** they are stored in the order written
**And** each record is assigned an RBA (Relative Byte Address)

**Complexity:** M
**Supports:** FR-v1.1-006

---

### Story 16.2: Implement ESDS Sequential I/O

As a **developer**,
I want **to read and write ESDS records sequentially**,
So that **I can process entry-sequenced data**.

**Acceptance Criteria:**

**Given** an ESDS open for output
**When** I write records
**Then** they are appended to the dataset
**And** I can retrieve the RBA of each written record

**Given** an ESDS open for input
**When** I read sequentially
**Then** records are returned in entry sequence order
**And** end-of-file is indicated after last record

**Complexity:** M
**Supports:** FR-v1.1-007

---

### Story 16.3: Implement ESDS RBA Access

As a **developer**,
I want **to access ESDS records by RBA**,
So that **I can retrieve specific records directly**.

**Acceptance Criteria:**

**Given** an ESDS with records
**When** I read by RBA
**Then** the record at that byte address is returned

**Given** an invalid RBA (beyond data extent)
**When** I attempt to read
**Then** appropriate error status is returned

**Complexity:** S
**Supports:** FR-v1.1-008

---

### Story 16.4: Implement RRDS Cluster

As a **developer**,
I want **to create RRDS (Relative Record) clusters**,
So that **I can access records by slot number**.

**Acceptance Criteria:**

**Given** RRDS cluster parameters (fixed record size)
**When** I create the cluster
**Then** an RRDS file is created with numbered slots

**Given** an RRDS
**When** I write to slot N
**Then** the record is stored at position N
**And** I can later retrieve it by slot number

**Complexity:** M
**Supports:** FR-v1.1-009

---

### Story 16.5: Implement RRDS Keyed Access

As a **developer**,
I want **to read and write RRDS records by relative record number**,
So that **I can directly access specific slots**.

**Acceptance Criteria:**

**Given** an RRDS
**When** I write a record with relative key 5
**Then** the record is stored in slot 5

**Given** an RRDS with a record in slot 5
**When** I read with relative key 5
**Then** the record is returned

**Given** an empty slot
**When** I read that slot
**Then** file status 23 is returned (record not found)

**Complexity:** S
**Supports:** FR-v1.1-010

---

## Epic 17: SORT Utility

**Goal:** Implement a DFSORT-compatible sort utility for batch processing.

**Crate:** `zos-sort` (new crate)
**FRs:** FR-v1.1-015 to FR-v1.1-025

### Story 17.1: Create SORT Crate and Control Statement Parser

As a **developer**,
I want **a parser for DFSORT control statements**,
So that **existing SORT control cards work with zOS-clone**.

**Acceptance Criteria:**

**Given** control statement: `SORT FIELDS=(1,10,CH,A,15,5,ZD,D)`
**When** parsed
**Then** I get a SortSpec with two fields:
  - Field 1: position 1, length 10, character, ascending
  - Field 2: position 15, length 5, zoned decimal, descending

**Given** invalid control statement syntax
**When** parsed
**Then** clear error message with line number is returned

**Complexity:** L
**Supports:** FR-v1.1-025

---

### Story 17.2: Implement Sort Engine (In-Memory)

As a **developer**,
I want **an in-memory sort engine**,
So that **small to medium datasets sort quickly**.

**Acceptance Criteria:**

**Given** a dataset that fits in memory
**When** I invoke SORT
**Then** records are sorted according to FIELDS specification
**And** output is written to SORTOUT

**Given** multiple sort keys
**When** sorting
**Then** records are ordered by first key, then second key for ties, etc.

**Given** numeric fields (ZD, PD, BI)
**When** sorting
**Then** numeric values are compared correctly (not character comparison)

**Complexity:** M
**Supports:** FR-v1.1-015, FR-v1.1-016

---

### Story 17.3: Implement External Merge Sort

As a **developer**,
I want **external merge sort for large datasets**,
So that **files larger than memory can be sorted**.

**Acceptance Criteria:**

**Given** a dataset larger than available memory
**When** I invoke SORT
**Then** the dataset is sorted using external merge
**And** temporary files are created and cleaned up

**Given** the sort completes
**When** I examine output
**Then** all records are present and correctly ordered

**Complexity:** L
**Supports:** NFR-v1.1-P3, NFR-v1.1-P4

---

### Story 17.4: Implement INCLUDE/OMIT Filtering

As an **operations engineer**,
I want **INCLUDE and OMIT statements**,
So that **I can filter records during sort**.

**Acceptance Criteria:**

**Given** `INCLUDE COND=(10,2,CH,EQ,C'NY')`
**When** sorting
**Then** only records where positions 10-11 equal "NY" are included

**Given** `OMIT COND=(1,1,CH,EQ,C'X')`
**When** sorting
**Then** records starting with "X" are excluded

**Given** compound conditions with AND/OR
**When** filtering
**Then** boolean logic is applied correctly

**Complexity:** M
**Supports:** FR-v1.1-017

---

### Story 17.5: Implement OUTREC Reformatting

As an **operations engineer**,
I want **OUTREC for output record reformatting**,
So that **I can restructure records during sort**.

**Acceptance Criteria:**

**Given** `OUTREC FIELDS=(1,10,25,15)`
**When** sorting
**Then** output records contain positions 1-10 followed by positions 25-39

**Given** `OUTREC FIELDS=(1:10,C'***',15:5)`
**When** processing
**Then** literal "***" is inserted between fields

**Given** arithmetic expressions in OUTREC
**When** processing
**Then** numeric calculations are performed

**Complexity:** M
**Supports:** FR-v1.1-018

---

### Story 17.6: Implement INREC Reformatting

As an **operations engineer**,
I want **INREC for input record reformatting**,
So that **I can preprocess records before sorting**.

**Acceptance Criteria:**

**Given** `INREC FIELDS=(1,10,C' ',20,5)`
**When** records are read
**Then** they are reformatted before sorting
**And** sort keys apply to reformatted records

**Complexity:** M
**Supports:** FR-v1.1-019

---

### Story 17.7: Implement SUM Operation

As an **operations engineer**,
I want **SUM fields for summarization**,
So that **I can aggregate numeric values for duplicate keys**.

**Acceptance Criteria:**

**Given** `SUM FIELDS=(20,10,ZD)`
**When** multiple records have the same sort key
**Then** they are combined into one record
**And** the SUM field contains the total

**Given** `SUM FIELDS=NONE`
**When** processing
**Then** duplicate key records are removed (keeping first)

**Complexity:** M
**Supports:** FR-v1.1-020

---

### Story 17.8: Implement MERGE and COPY Operations

As an **operations engineer**,
I want **MERGE and COPY operations**,
So that **I can combine sorted files or reformat without sorting**.

**Acceptance Criteria:**

**Given** `MERGE FIELDS=(1,10,CH,A)` with multiple SORTIN files
**When** executing
**Then** pre-sorted inputs are merged maintaining order

**Given** `OPTION COPY`
**When** executing
**Then** records are copied without sorting
**And** INREC/OUTREC transformations still apply

**Complexity:** M
**Supports:** FR-v1.1-022, FR-v1.1-023

---

### Story 17.9: Implement SORT JCL and CLI Integration

As an **operations engineer**,
I want **to invoke SORT via JCL and CLI**,
So that **SORT works in batch jobs and interactively**.

**Acceptance Criteria:**

**Given** JCL:
```jcl
//STEP1    EXEC PGM=SORT
//SORTIN   DD DSN=INPUT.FILE
//SORTOUT  DD DSN=OUTPUT.FILE
//SYSIN    DD *
  SORT FIELDS=(1,10,CH,A)
/*
```
**When** the job executes
**Then** the sort is performed and output written

**Given** CLI: `zos-clone sort --input file.dat --output sorted.dat --fields "1,10,CH,A"`
**When** executed
**Then** file is sorted and written to output

**Complexity:** M
**Supports:** FR-v1.1-015, FR-v1.1-024, FR-v1.1-051

---

## Epic 18: GDG Support

**Goal:** Implement Generation Data Group support for dataset versioning.

**Crate:** `zos-dataset/gdg`
**FRs:** FR-v1.1-026 to FR-v1.1-034

### Story 18.1: Implement GDG Base Definition

As an **operations engineer**,
I want **to define GDG base entries**,
So that **I can create versioned dataset groups**.

**Acceptance Criteria:**

**Given** IDCAMS command: `DEFINE GDG (NAME(MY.GDG.BASE) LIMIT(10) SCRATCH)`
**When** executed
**Then** a GDG base entry is created in the catalog
**And** the limit and scratch attributes are stored

**Given** attempt to define existing GDG base
**When** executed
**Then** appropriate error is returned

**Complexity:** M
**Supports:** FR-v1.1-026

---

### Story 18.2: Implement Relative Generation References

As an **operations engineer**,
I want **to reference GDG datasets with relative generation numbers**,
So that **I can easily refer to current and previous generations**.

**Acceptance Criteria:**

**Given** GDG base MY.GDG.BASE with generations G0001V00, G0002V00
**When** JCL references `MY.GDG.BASE(0)`
**Then** it resolves to MY.GDG.BASE.G0002V00 (current)

**Given** same GDG
**When** JCL references `MY.GDG.BASE(-1)`
**Then** it resolves to MY.GDG.BASE.G0001V00 (previous)

**Given** same GDG
**When** JCL references `MY.GDG.BASE(+1)` with DISP=(NEW,CATLG)
**Then** a new generation G0003V00 is created

**Complexity:** M
**Supports:** FR-v1.1-027, FR-v1.1-029, FR-v1.1-030, FR-v1.1-031

---

### Story 18.3: Implement Absolute Generation References

As an **operations engineer**,
I want **to reference GDG datasets with absolute generation numbers**,
So that **I can access specific historical generations**.

**Acceptance Criteria:**

**Given** GDG with multiple generations
**When** JCL references `MY.GDG.BASE.G0001V00`
**Then** that specific generation is accessed regardless of relative position

**Given** nonexistent absolute generation
**When** referenced
**Then** appropriate error (dataset not found) is returned

**Complexity:** S
**Supports:** FR-v1.1-028

---

### Story 18.4: Implement GDG Limit Rolloff

As a **system**,
I want **automatic rolloff when GDG limit is reached**,
So that **old generations are managed automatically**.

**Acceptance Criteria:**

**Given** GDG with LIMIT(3) and generations G0001, G0002, G0003
**When** a new generation (+1) is created
**Then** G0004 is created
**And** G0001 is rolled off (deleted if SCRATCH, uncataloged if NOSCRATCH)

**Given** rolloff in a job
**When** the job ends
**Then** rolloff is performed at job completion (not during step)

**Complexity:** M
**Supports:** FR-v1.1-032

---

### Story 18.5: Implement GDG LISTCAT

As an **operations engineer**,
I want **to list GDG generations via IDCAMS**,
So that **I can see what generations exist**.

**Acceptance Criteria:**

**Given** IDCAMS: `LISTCAT ENT(MY.GDG.BASE) GDG ALL`
**When** executed
**Then** output shows:
  - GDG base attributes (LIMIT, SCRATCH, EMPTY)
  - All generations with creation dates
  - Status of each generation

**Complexity:** S
**Supports:** FR-v1.1-033

---

### Story 18.6: Implement GDG Generation Delete

As an **operations engineer**,
I want **to delete GDG generations**,
So that **I can manage disk space manually**.

**Acceptance Criteria:**

**Given** IDCAMS: `DELETE MY.GDG.BASE.G0001V00`
**When** executed
**Then** that specific generation is deleted
**And** catalog is updated

**Given** `DELETE MY.GDG.BASE GDG FORCE`
**When** executed
**Then** all generations are deleted
**And** GDG base is deleted

**Complexity:** S
**Supports:** FR-v1.1-034

---

### Story 18.7: Implement GDG CLI Integration

As a **developer**,
I want **GDG operations via CLI**,
So that **I can manage GDGs without JCL**.

**Acceptance Criteria:**

**Given** `zos-clone gdg create MY.GDG.BASE --limit 10`
**When** executed
**Then** GDG base is created

**Given** `zos-clone gdg list MY.GDG.BASE`
**When** executed
**Then** generations are listed with timestamps

**Complexity:** S
**Supports:** FR-v1.1-053

---

## Epic 19: IDCAMS Utility

**Goal:** Implement IDCAMS for dataset and catalog management.

**Crate:** `zos-dataset/idcams`
**FRs:** FR-v1.1-035 to FR-v1.1-044

### Story 19.1: Implement IDCAMS Command Parser

As a **developer**,
I want **a parser for IDCAMS commands**,
So that **IDCAMS control cards are interpreted correctly**.

**Acceptance Criteria:**

**Given** IDCAMS command with continuation:
```
  DEFINE CLUSTER (NAME(MY.CLUSTER) -
    KEYS(10 0) -
    RECORDSIZE(100 200))
```
**When** parsed
**Then** I get a DefineCommand with all parameters

**Given** invalid syntax
**When** parsed
**Then** clear error with line number is returned

**Complexity:** M
**Supports:** FR-v1.1-035

---

### Story 19.2: Implement DEFINE CLUSTER

As an **operations engineer**,
I want **DEFINE CLUSTER command**,
So that **I can create VSAM clusters**.

**Acceptance Criteria:**

**Given** DEFINE CLUSTER with KSDS parameters (NAME, KEYS, RECORDSIZE)
**When** executed
**Then** KSDS cluster is created
**And** catalog entry is added

**Given** DEFINE CLUSTER for ESDS (NONINDEXED)
**When** executed
**Then** ESDS cluster is created

**Given** DEFINE CLUSTER for RRDS (NUMBERED)
**When** executed
**Then** RRDS cluster is created

**Complexity:** M
**Supports:** FR-v1.1-036

---

### Story 19.3: Implement DELETE Command

As an **operations engineer**,
I want **DELETE command**,
So that **I can remove datasets and catalog entries**.

**Acceptance Criteria:**

**Given** `DELETE MY.DATASET`
**When** executed
**Then** dataset is deleted from disk
**And** catalog entry is removed

**Given** `DELETE MY.DATASET PURGE`
**When** executed
**Then** dataset is deleted regardless of retention date

**Given** nonexistent dataset
**When** DELETE executed
**Then** error is returned (unless NOERASE specified)

**Complexity:** S
**Supports:** FR-v1.1-038

---

### Story 19.4: Implement ALTER Command

As an **operations engineer**,
I want **ALTER command**,
So that **I can rename datasets**.

**Acceptance Criteria:**

**Given** `ALTER MY.OLD.NAME NEWNAME(MY.NEW.NAME)`
**When** executed
**Then** dataset is renamed
**And** catalog is updated

**Given** ALTER on nonexistent dataset
**When** executed
**Then** appropriate error is returned

**Complexity:** S
**Supports:** FR-v1.1-039

---

### Story 19.5: Implement LISTCAT Command

As an **operations engineer**,
I want **LISTCAT command**,
So that **I can examine catalog entries**.

**Acceptance Criteria:**

**Given** `LISTCAT ENT(MY.DATASET) ALL`
**When** executed
**Then** output shows:
  - Dataset name and type
  - Attributes (record format, size)
  - Statistics (records, etc.)

**Given** `LISTCAT LVL(MY.HLQ) NAME`
**When** executed
**Then** all datasets under MY.HLQ are listed

**Complexity:** M
**Supports:** FR-v1.1-040

---

### Story 19.6: Implement PRINT Command

As an **operations engineer**,
I want **PRINT command**,
So that **I can view dataset contents**.

**Acceptance Criteria:**

**Given** `PRINT INDATASET(MY.DATASET)`
**When** executed
**Then** dataset records are printed in hex and character

**Given** `PRINT INDATASET(MY.DATASET) CHARACTER`
**When** executed
**Then** only character representation is shown

**Given** SKIP and COUNT parameters
**When** specified
**Then** output is limited accordingly

**Complexity:** S
**Supports:** FR-v1.1-041

---

### Story 19.7: Implement REPRO Command

As an **operations engineer**,
I want **REPRO command**,
So that **I can copy datasets**.

**Acceptance Criteria:**

**Given** `REPRO INDATASET(SOURCE) OUTDATASET(TARGET)`
**When** executed
**Then** all records are copied to target

**Given** sequential to VSAM copy
**When** executed
**Then** records are loaded into VSAM cluster

**Given** FROMKEY/TOKEY parameters
**When** specified
**Then** only records in key range are copied

**Complexity:** M
**Supports:** FR-v1.1-042

---

### Story 19.8: Implement VERIFY and CLI Integration

As an **operations engineer**,
I want **VERIFY command and CLI access to IDCAMS**,
So that **I can verify VSAM integrity and use IDCAMS interactively**.

**Acceptance Criteria:**

**Given** `VERIFY DATASET(MY.VSAM.CLUSTER)`
**When** executed
**Then** VSAM end-of-file markers are verified/corrected

**Given** `zos-clone idcams` with stdin commands
**When** executed
**Then** IDCAMS commands are processed

**Given** `zos-clone idcams --command "LISTCAT ENT(*)"`
**When** executed
**Then** single command is executed

**Complexity:** M
**Supports:** FR-v1.1-043, FR-v1.1-044, FR-v1.1-052

---

## Epic 20: Package Distribution

**Goal:** Create apt and yum packages for enterprise Linux deployment.

**Crate:** CI/CD infrastructure
**FRs:** FR-v1.1-045 to FR-v1.1-050

### Story 20.1: Create Debian Package Build

As a **system administrator**,
I want **apt/deb packages**,
So that **I can install zOS-clone on Debian/Ubuntu systems**.

**Acceptance Criteria:**

**Given** the release workflow
**When** a new version is tagged
**Then** a .deb package is built
**And** it includes binary, man pages, and completions

**Given** the .deb package
**When** installed via `apt install ./zos-clone.deb`
**Then** zos-clone is available system-wide

**Complexity:** M
**Supports:** FR-v1.1-045

---

### Story 20.2: Create RPM Package Build

As a **system administrator**,
I want **yum/rpm packages**,
So that **I can install zOS-clone on RHEL/Fedora systems**.

**Acceptance Criteria:**

**Given** the release workflow
**When** a new version is tagged
**Then** a .rpm package is built
**And** it includes binary, man pages, and completions

**Given** the .rpm package
**When** installed via `yum install ./zos-clone.rpm`
**Then** zos-clone is available system-wide

**Complexity:** M
**Supports:** FR-v1.1-046

---

### Story 20.3: Create Man Pages

As a **developer**,
I want **man pages for all commands**,
So that **users can access help via standard Unix interface**.

**Acceptance Criteria:**

**Given** `man zos-clone`
**When** accessed
**Then** main command documentation is shown

**Given** `man zos-clone-compile`
**When** accessed
**Then** compile subcommand documentation is shown

**Similar for:** run, check, idcams, sort, gdg

**Complexity:** M
**Supports:** FR-v1.1-049

---

### Story 20.4: Package Shell Completions

As a **developer**,
I want **shell completions in packages**,
So that **tab completion works after install**.

**Acceptance Criteria:**

**Given** zOS-clone installed via package
**When** user types `zos-clone <TAB>` in bash
**Then** subcommands are suggested

**Given** zOS-clone installed via package
**When** user types `zos-clone compile --<TAB>` in bash
**Then** options are suggested

**Also works for:** zsh, fish

**Complexity:** S
**Supports:** FR-v1.1-050

---

### Story 20.5: Create Package Repository

As a **system administrator**,
I want **package repositories**,
So that **I can install/upgrade via standard package manager**.

**Acceptance Criteria:**

**Given** repository URL added to apt sources
**When** running `apt update && apt install zos-clone`
**Then** latest version is installed from repository

**Given** new version released
**When** running `apt upgrade`
**Then** zos-clone is upgraded to new version

**Similar for:** yum/dnf repository

**Complexity:** L
**Supports:** FR-v1.1-047, FR-v1.1-048

---

## Requirements Traceability

### FR to Story Mapping

| Requirement | Story | Status |
|-------------|-------|--------|
| FR-v1.1-001 | 15.2 | Planned |
| FR-v1.1-002 | 15.3 | Planned |
| FR-v1.1-003 | 15.4 | Planned |
| FR-v1.1-004 | 15.5 | Planned |
| FR-v1.1-005 | 15.6 | Planned |
| FR-v1.1-006 | 16.1 | Planned |
| FR-v1.1-007 | 16.2 | Planned |
| FR-v1.1-008 | 16.3 | Planned |
| FR-v1.1-009 | 16.4 | Planned |
| FR-v1.1-010 | 16.5 | Planned |
| FR-v1.1-011 | 15.7 | Planned |
| FR-v1.1-012 | 15.2 | Planned |
| FR-v1.1-013 | 15.8 | Planned |
| FR-v1.1-014 | 15.8 | Planned |
| FR-v1.1-015 | 17.9 | Planned |
| FR-v1.1-016 | 17.2 | Planned |
| FR-v1.1-017 | 17.4 | Planned |
| FR-v1.1-018 | 17.5 | Planned |
| FR-v1.1-019 | 17.6 | Planned |
| FR-v1.1-020 | 17.7 | Planned |
| FR-v1.1-021 | 17.9 | Planned |
| FR-v1.1-022 | 17.8 | Planned |
| FR-v1.1-023 | 17.8 | Planned |
| FR-v1.1-024 | 17.9 | Planned |
| FR-v1.1-025 | 17.1 | Planned |
| FR-v1.1-026 | 18.1 | Planned |
| FR-v1.1-027 | 18.2 | Planned |
| FR-v1.1-028 | 18.3 | Planned |
| FR-v1.1-029 | 18.2 | Planned |
| FR-v1.1-030 | 18.2 | Planned |
| FR-v1.1-031 | 18.2 | Planned |
| FR-v1.1-032 | 18.4 | Planned |
| FR-v1.1-033 | 18.5 | Planned |
| FR-v1.1-034 | 18.6 | Planned |
| FR-v1.1-035 | 19.1 | Planned |
| FR-v1.1-036 | 19.2 | Planned |
| FR-v1.1-037 | 18.1 | Planned |
| FR-v1.1-038 | 19.3 | Planned |
| FR-v1.1-039 | 19.4 | Planned |
| FR-v1.1-040 | 19.5 | Planned |
| FR-v1.1-041 | 19.6 | Planned |
| FR-v1.1-042 | 19.7 | Planned |
| FR-v1.1-043 | 19.8 | Planned |
| FR-v1.1-044 | 19.8 | Planned |
| FR-v1.1-045 | 20.1 | Planned |
| FR-v1.1-046 | 20.2 | Planned |
| FR-v1.1-047 | 20.5 | Planned |
| FR-v1.1-048 | 20.5 | Planned |
| FR-v1.1-049 | 20.3 | Planned |
| FR-v1.1-050 | 20.4 | Planned |
| FR-v1.1-051 | 17.9 | Planned |
| FR-v1.1-052 | 19.8 | Planned |
| FR-v1.1-053 | 18.7 | Planned |
