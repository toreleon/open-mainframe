---
version: 'v5.0'
planningGroup: 'PG-28'
technology: 'ADABAS'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-adabas-v5.0.md'
  - 'architecture-adabas-v5.0.md'
totalEpics: 10
totalStories: 52
frCoverage: '12/12 (100%)'
nfrCoverage: '3/3 (100%)'
---

# Epics & Stories: ADABAS

## Epic Overview

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| ADA-100 | Inverted-List Storage Engine | L | 6 | E |
| ADA-101 | FDT & Field System | M | 5 | E |
| ADA-102 | Descriptor Engine | M | 5 | E |
| ADA-103 | Direct Call Interface (ACB) | M | 5 | E |
| ADA-104 | Search Commands | M | 5 | E |
| ADA-105 | Read Commands | M | 6 | E |
| ADA-106 | Modification Commands | M | 4 | E |
| ADA-107 | Transaction Management | M | 5 | E |
| ADA-108 | Nucleus & Logging | L | 6 | E |
| ADA-109 | Utilities & DDM | M | 5 | E |

---

## ADA-100: Inverted-List Storage Engine

### ADA-100.1: Associator — Address Converter
**Acceptance Criteria:** Given ISN 12345, when looked up in the address converter, then the physical storage location (block/offset) is returned.

### ADA-100.2: Associator — Inverted Lists
**Acceptance Criteria:** Given descriptor ENAME with value "SMITH", when the inverted list is queried, then ISN list {100, 205, 340} is returned.

### ADA-100.3: Data Storage — Record Storage
**Acceptance Criteria:** Given compressed record data, when stored in Data Storage, then the ISN is assigned and the address converter is updated.

### ADA-100.4: Data Compression
**Acceptance Criteria:** Given record with fields EMPID='123456', ENAME='SMITH' + 25 trailing blanks, when compressed, then trailing blanks are removed achieving ≥40% compression.

### ADA-100.5: ISN Allocation and Reuse
**Acceptance Criteria:** Given a new record, when stored, then the next available ISN is assigned. Given a deleted record, when its ISN is recycled, then it can be reused.

### ADA-100.6: Storage Engine Tests
**Acceptance Criteria:** Given store/retrieve/delete/compress round-trips, when tested, then all data integrity is maintained.

---

## ADA-101: FDT & Field System

### ADA-101.1: FDT Definition with 7 Data Types
**Acceptance Criteria:** Given FDT with fields AA(A,20), AB(N,8), AC(P,5,2), AD(B,4), when defined, then each field's type, length, and decimal places are recorded.

### ADA-101.2: Multiple-Value (MU) Fields
**Acceptance Criteria:** Given field AE(A,30,MU), when a record stores 3 values for AE, then all 3 are stored and individually addressable.

### ADA-101.3: Periodic Group (PE) Fields
**Acceptance Criteria:** Given PE group AF containing fields AG, AH, when a record has 2 occurrences of the PE group, then each occurrence has its own AG/AH values.

### ADA-101.4: LOB Fields (LA/LB)
**Acceptance Criteria:** Given field AI(A,0,LB) for large binary, when a 1MB value is stored, then it is stored in LOB storage with a reference in the record.

### ADA-101.5: FDT Tests
**Acceptance Criteria:** Given FDT definitions with all field types and options, when tested, then correct field handling occurs.

---

## ADA-102: Descriptor Engine

### ADA-102.1: Standard Descriptors
**Acceptance Criteria:** Given field ENAME defined as DE (descriptor), when records are stored/updated/deleted, then the inverted list for ENAME is automatically maintained.

### ADA-102.2: Super-Descriptors
**Acceptance Criteria:** Given super-descriptor S1 composed of DEPT(1-3) + EMPNO, when defined, then a composite inverted list enables searching by combined values.

### ADA-102.3: Sub-Descriptors
**Acceptance Criteria:** Given sub-descriptor S2 from ENAME(1:3), when defined, then an inverted list on the first 3 characters enables prefix searching.

### ADA-102.4: Phonetic Descriptors
**Acceptance Criteria:** Given phonetic descriptor PH from ENAME, when "SMITH" and "SMYTH" are stored, then both are findable via phonetic search.

### ADA-102.5: Descriptor Tests
**Acceptance Criteria:** Given all 6 descriptor types with CRUD operations, when tested, then inverted lists are correctly maintained.

---

## ADA-103: Direct Call Interface (ACB)

### ADA-103.1: 80-Byte ACB Structure
**Acceptance Criteria:** Given an ACB with command_code=L1, file_number=5, ISN=12345, when dispatched, then the L1 read handler is invoked for file 5.

### ADA-103.2: Format Buffer (FB) Parsing
**Acceptance Criteria:** Given FB "AA,AB,AC.", when parsed, then fields AA, AB, AC are identified for the read/write operation.

### ADA-103.3: Search Buffer (SB) and Value Buffer (VB)
**Acceptance Criteria:** Given SB "AA,EQ." and VB "SMITH" for an S1 command, when parsed, then the search criteria ENAME = 'SMITH' is established.

### ADA-103.4: ISN Buffer (IB)
**Acceptance Criteria:** Given an S1 search returning ISN list, when placed in the ISN buffer, then subsequent L1 reads can use the ISN buffer for batch retrieval.

### ADA-103.5: ACB Response Codes
**Acceptance Criteria:** Given response code 0 (success), 3 (end of file), 113 (record not found), 145 (ISN not found), when returned, then callers can check the ACB response code field.

---

## ADA-104: Search Commands

### ADA-104.1: S1 — Find Records
**Acceptance Criteria:** Given S1 with SB "AA,EQ." VB "SMITH", when executed, then ISN list of matching records is returned in IB.

### ADA-104.2: S2 — Find Sorted
**Acceptance Criteria:** Given S2 with sort criteria, when executed, then ISN list is returned sorted by specified descriptor.

### ADA-104.3: S4 — Coupled File Search
**Acceptance Criteria:** Given S4 searching across coupled files, when executed, then ISNs from the target file matching the coupled criteria are returned.

### ADA-104.4: S8/S9 — ISN List Operations
**Acceptance Criteria:** Given S8 (logical AND/OR on ISN lists), when two ISN lists are combined, then the intersection or union is produced.

### ADA-104.5: Search Command Tests
**Acceptance Criteria:** Given search scenarios with simple and compound criteria, when tested, then correct ISN lists are returned.

---

## ADA-105: Read Commands

### ADA-105.1: L1/L4 — Read by ISN
**Acceptance Criteria:** Given L1 with ISN=12345 and FB "AA,AB.", when executed, then fields AA and AB of record 12345 are returned in RB.

### ADA-105.2: L2/L5 — Read Physical Sequential
**Acceptance Criteria:** Given L2, when called repeatedly, then records are returned in physical storage order.

### ADA-105.3: L3/L6 — Read Logical Sequential
**Acceptance Criteria:** Given L3 with descriptor ENAME, when called repeatedly, then records are returned in ENAME ascending order.

### ADA-105.4: L9 — Histogram
**Acceptance Criteria:** Given L9 on descriptor DEPT, when executed, then each unique DEPT value and its count are returned.

### ADA-105.5: LF — Read FDT
**Acceptance Criteria:** Given LF for file 5, when executed, then the FDT (field names, types, lengths, options) is returned.

### ADA-105.6: Read Command Tests
**Acceptance Criteria:** Given all read variants, when tested, then correct data retrieval occurs.

---

## ADA-106: Modification Commands

### ADA-106.1: N1/N2 — Store Record
**Acceptance Criteria:** Given N1 with FB and RB, when executed, then a new record is stored with auto-assigned ISN, data is compressed, and descriptors are updated.

### ADA-106.2: A1 — Update Record
**Acceptance Criteria:** Given A1 with ISN, FB, and RB, when executed, then the specified fields are updated, compression is reapplied, and affected descriptors are maintained.

### ADA-106.3: E1 — Delete Record
**Acceptance Criteria:** Given E1 with ISN, when executed, then the record is removed, ISN is recycled, and all descriptor entries are removed.

### ADA-106.4: Modification Tests
**Acceptance Criteria:** Given store/update/delete scenarios with descriptor maintenance, when tested, then data integrity is preserved.

---

## ADA-107: Transaction Management

### ADA-107.1: OP/CL — Open/Close Session
**Acceptance Criteria:** Given OP, when issued, then a user session is established with default ETB settings. Given CL, then the session is closed.

### ADA-107.2: ET — End Transaction (Commit)
**Acceptance Criteria:** Given ET, when issued, then all changes since the last ET/BT are committed and locks released.

### ADA-107.3: BT — Backout Transaction (Rollback)
**Acceptance Criteria:** Given BT, when issued, then all changes since the last ET are undone using before-images from PLOG.

### ADA-107.4: HI/RI — Hold ISN
**Acceptance Criteria:** Given a read with hold (L1 in hold mode), when issued, then the ISN is locked for exclusive access until ET or BT.

### ADA-107.5: Transaction Tests
**Acceptance Criteria:** Given commit, rollback, and concurrent access scenarios, when tested, then ACID properties hold.

---

## ADA-108: Nucleus & Logging

### ADA-108.1: Multi-Threaded Nucleus
**Acceptance Criteria:** Given concurrent command requests, when processed, then the nucleus dispatches to worker threads with shared buffer pool access.

### ADA-108.2: Buffer Pool Management
**Acceptance Criteria:** Given configurable buffer pool size, when pages are accessed, then LRU caching minimizes I/O.

### ADA-108.3: Protection Log (PLOG)
**Acceptance Criteria:** Given before/after images, when written to PLOG on each modification, then recovery can reconstruct consistent state.

### ADA-108.4: Command Log (CLOG)
**Acceptance Criteria:** Given each command, when logged to CLOG, then command history is available for auditing and replay.

### ADA-108.5: Automatic Restart
**Acceptance Criteria:** Given nucleus failure, when restarted, then PLOG is used to roll back uncommitted transactions.

### ADA-108.6: Nucleus Tests
**Acceptance Criteria:** Given multi-threaded access, buffer management, and recovery scenarios, when tested, then all pass.

---

## ADA-109: Utilities & DDM

### ADA-109.1: ADACMP — Compress/Decompress
**Acceptance Criteria:** Given a sequential dataset, when ADACMP runs, then records are compressed/decompressed per FDT.

### ADA-109.2: ADALOD — Load Data
**Acceptance Criteria:** Given compressed input, when ADALOD runs, then records are stored with ISN assignment and index building.

### ADA-109.3: ADAULD — Unload Data
**Acceptance Criteria:** Given a file, when ADAULD runs, then all records are unloaded in compressed format.

### ADA-109.4: DDM Definitions
**Acceptance Criteria:** Given a DDM for file EMPLOYEES with logical field names and views, when defined, then Natural programs can reference the DDM for database access.

### ADA-109.5: Utility & DDM Tests
**Acceptance Criteria:** Given load/unload round-trips and DDM definitions, when tested, then all produce correct results.

---

## FR/NFR Coverage

**Coverage: 12/12 FRs (100%), 3/3 NFRs (100%)**
