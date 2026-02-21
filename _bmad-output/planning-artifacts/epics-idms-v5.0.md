---
version: 'v5.0'
planningGroup: 'PG-27'
technology: 'IDMS'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-idms-v5.0.md'
  - 'architecture-idms-v5.0.md'
totalEpics: 11
totalStories: 56
frCoverage: '12/12 (100%)'
nfrCoverage: '3/3 (100%)'
---

# Epics & Stories: IDMS

## Epic Overview

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| IDMS-100 | CODASYL Data Model Core | M | 5 | E |
| IDMS-101 | Schema & Subschema DDL Parser | M | 5 | E |
| IDMS-102 | Navigational DML Engine | L | 7 | E |
| IDMS-103 | Currency Indicator System | M | 4 | E |
| IDMS-104 | COBOL DML Precompiler | M | 4 | E |
| IDMS-105 | DMCL & Physical Storage | L | 6 | E |
| IDMS-106 | IDMS-DC Transaction Processing | L | 6 | E |
| IDMS-107 | ADS/Online 4GL (Basic) | M | 4 | E |
| IDMS-108 | SQL Option | M | 5 | E |
| IDMS-109 | Recovery & Operations | M | 5 | E |
| IDMS-110 | Lock Management & Tests | M | 5 | E |

---

## IDMS-100: CODASYL Data Model Core

### IDMS-100.1: Record Type Definitions
**Acceptance Criteria:** Given a record type EMPLOYEE with elements EMPID, ENAME, SALARY, when defined, then the record is stored with its element layout and db-key assignment.

### IDMS-100.2: Set (Owner-Member) Relationships
**Acceptance Criteria:** Given set DEPT-EMP with owner DEPARTMENT and member EMPLOYEE, when defined, then NEXT/PRIOR/OWNER pointers enable navigation from department to its employees and back.

### IDMS-100.3: Area Definitions
**Acceptance Criteria:** Given area EMP-AREA with page range 1-1000, when defined, then records stored in this area reside on pages within the range.

### IDMS-100.4: CALC and VIA Access Strategies
**Acceptance Criteria:** Given EMPLOYEE with CALC key EMPID, when STORE is issued, then the record is placed on a page determined by hashing EMPID. Given ORDER with VIA DEPT-ORDER, when stored, then the order is placed near its owner department.

### IDMS-100.5: Db-Key Management
**Acceptance Criteria:** Given a stored record, when its db-key is examined, then it encodes the page number and line index uniquely identifying the record.

---

## IDMS-101: Schema & Subschema DDL Parser

### IDMS-101.1: RECORD Definition Parsing
**Acceptance Criteria:** Given `ADD RECORD NAME IS EMPLOYEE ... 02 EMPID PIC 9(6) ... 02 ENAME PIC X(30)`, when parsed, then record with elements is created.

### IDMS-101.2: SET Definition Parsing
**Acceptance Criteria:** Given `ADD SET NAME IS DEPT-EMP OWNER IS DEPARTMENT MEMBER IS EMPLOYEE LINKED TO OWNER`, when parsed, then set with owner/member/pointer type is created.

### IDMS-101.3: AREA Definition Parsing
**Acceptance Criteria:** Given `ADD AREA NAME IS EMP-AREA`, when parsed, then area definition is recorded.

### IDMS-101.4: Subschema Parsing
**Acceptance Criteria:** Given a subschema including subset of records/sets/areas, when parsed, then a restricted view is created for application use.

### IDMS-101.5: DDL Parser Tests
**Acceptance Criteria:** Given complete schema DDL, when parsed, then all records, sets, and areas are correctly represented.

---

## IDMS-102: Navigational DML Engine

### IDMS-102.1: OBTAIN CALC — Direct Access by Key
**Acceptance Criteria:** Given `OBTAIN CALC EMPLOYEE`, when EMPID is set in the record area, then the employee with matching EMPID is retrieved via hash access.

### IDMS-102.2: OBTAIN NEXT/PRIOR/FIRST/LAST WITHIN SET
**Acceptance Criteria:** Given `OBTAIN NEXT EMPLOYEE WITHIN DEPT-EMP`, when issued, then the next employee in the current department's set is retrieved.

### IDMS-102.3: OBTAIN OWNER WITHIN SET
**Acceptance Criteria:** Given `OBTAIN OWNER WITHIN DEPT-EMP`, when issued after obtaining an employee, then the owning department is retrieved.

### IDMS-102.4: STORE Record
**Acceptance Criteria:** Given `STORE EMPLOYEE`, when issued with element values set, then the record is stored using CALC/VIA strategy and connected to mandatory sets.

### IDMS-102.5: MODIFY Record
**Acceptance Criteria:** Given `MODIFY EMPLOYEE`, when issued after obtaining a record and changing element values, then the record is updated in place.

### IDMS-102.6: ERASE Record
**Acceptance Criteria:** Given `ERASE EMPLOYEE`, when issued, then the record is removed and disconnected from all sets. Given `ERASE ALL`, then member records are also erased recursively.

### IDMS-102.7: CONNECT/DISCONNECT Set Operations
**Acceptance Criteria:** Given `CONNECT EMPLOYEE TO DEPT-EMP`, when issued, then the current employee is connected to the current department's set. Given `DISCONNECT`, then it is removed from the set.

---

## IDMS-103: Currency Indicator System

### IDMS-103.1: Run-Unit Currency
**Acceptance Criteria:** Given any DML operation that accesses a record, when completed, then the run-unit current record is updated.

### IDMS-103.2: Record-Type Currency
**Acceptance Criteria:** Given OBTAIN EMPLOYEE, when completed, then current of EMPLOYEE record type is updated.

### IDMS-103.3: Set-Type Currency
**Acceptance Criteria:** Given OBTAIN NEXT WITHIN DEPT-EMP, when completed, then current of DEPT-EMP set is updated.

### IDMS-103.4: Area Currency
**Acceptance Criteria:** Given OBTAIN record in EMP-AREA, when completed, then current of EMP-AREA is updated.

---

## IDMS-104: COBOL DML Precompiler

### IDMS-104.1: DML Statement Extraction
**Acceptance Criteria:** Given COBOL with embedded `OBTAIN CALC EMPLOYEE`, when scanned, then the DML statement is extracted.

### IDMS-104.2: CALL 'IDMS' Generation
**Acceptance Criteria:** Given `OBTAIN CALC EMPLOYEE`, when transformed, then `CALL 'IDMS' USING SUBSCHEMA-CTRL, verb-code, record-name-code` is generated.

### IDMS-104.3: SUBSCHEMA-CTRL Generation
**Acceptance Criteria:** Given an IDMS program, when preprocessed, then SUBSCHEMA-CTRL copybook with ERRSTAT, DBKEY, RECORD-NAME fields is generated in WORKING-STORAGE.

### IDMS-104.4: Precompiler Tests
**Acceptance Criteria:** Given COBOL with various DML verbs, when precompiled, then correct CALL statements are generated.

---

## IDMS-105: DMCL & Physical Storage

### IDMS-105.1: DMCL Configuration
**Acceptance Criteria:** Given DMCL mapping area EMP-AREA to file /data/emp-area.dat, when processed, then the area's I/O targets the specified file.

### IDMS-105.2: Page Management
**Acceptance Criteria:** Given fixed-size pages (4096 bytes), when records are stored, then they are placed within pages using slot management.

### IDMS-105.3: Buffer Pool
**Acceptance Criteria:** Given a buffer pool of 100 pages, when pages are accessed, then LRU caching reduces physical I/O.

### IDMS-105.4: CALC Hash Access
**Acceptance Criteria:** Given CALC key EMPID, when hashed, then a target page is computed; overflow chains handle collisions.

### IDMS-105.5: VIA Clustered Storage
**Acceptance Criteria:** Given VIA set DEPT-ORDER, when an order is stored, then it is placed on the same page as (or near) its owner department.

### IDMS-105.6: Storage Tests
**Acceptance Criteria:** Given CALC and VIA storage scenarios with overflow, when tested, then records are correctly placed and retrievable.

---

## IDMS-106: IDMS-DC Transaction Processing

### IDMS-106.1: MAP Definition and BIND MAP
**Acceptance Criteria:** Given a MAP definition with screen fields, when BIND MAP is issued, then the map is associated with the current task.

### IDMS-106.2: MAP IN / MAP OUT
**Acceptance Criteria:** Given MAP IN, when issued, then 3270 input data is mapped to program fields. Given MAP OUT, then program fields are mapped to 3270 output.

### IDMS-106.3: LINK and XCTL
**Acceptance Criteria:** Given `LINK PROGRAM 'SUBPGM'`, when issued, then control transfers to SUBPGM with return. Given `XCTL`, then no return.

### IDMS-106.4: Task Dispatching
**Acceptance Criteria:** Given a task code entered at a terminal, when dispatched, then the associated program is loaded and executed.

### IDMS-106.5: Scratch and Queue Storage
**Acceptance Criteria:** Given PUT SCRATCH, when issued, then data is stored in scratch area for the task. Given GET SCRATCH, then it is retrieved.

### IDMS-106.6: IDMS-DC Tests
**Acceptance Criteria:** Given MAP, LINK/XCTL, and task dispatch scenarios, when tested, then correct TP behavior occurs.

---

## IDMS-107: ADS/Online 4GL (Basic)

### IDMS-107.1: Dialog Definition
**Acceptance Criteria:** Given an ADS dialog with premap and response processes, when defined, then the dialog flow is established.

### IDMS-107.2: Premap Process
**Acceptance Criteria:** Given a premap process that obtains data and populates map fields, when executed before MAP OUT, then the screen displays current data.

### IDMS-107.3: Response Process
**Acceptance Criteria:** Given a response process that processes user input, when executed after MAP IN, then data validation and DML operations occur.

### IDMS-107.4: ADS Tests
**Acceptance Criteria:** Given basic dialog scenarios, when tested, then premap/response flow works correctly.

---

## IDMS-108: SQL Option

### IDMS-108.1: SELECT Against Network Records
**Acceptance Criteria:** Given `SELECT EMPID, ENAME FROM EMPLOYEE WHERE DEPTID = 'D01'`, when executed against CODASYL data, then matching records are returned as a result set.

### IDMS-108.2: INSERT/UPDATE/DELETE via SQL
**Acceptance Criteria:** Given `INSERT INTO EMPLOYEE VALUES (...)`, when executed, then a new record is stored with automatic set connections.

### IDMS-108.3: Cursor Support
**Acceptance Criteria:** Given DECLARE CURSOR / OPEN / FETCH / CLOSE, when used, then result sets are navigated row by row.

### IDMS-108.4: SQL-to-DML Translation
**Acceptance Criteria:** Given SQL WHERE clauses, when translated to navigational DML, then CALC/set navigation is used for efficient access.

### IDMS-108.5: SQL Option Tests
**Acceptance Criteria:** Given SQL DML against network data, when tested, then correct results are returned.

---

## IDMS-109: Recovery & Operations

### IDMS-109.1: Journal/Log Writing
**Acceptance Criteria:** Given write-ahead logging, when a record is modified, then before/after images are written to the journal before the data page.

### IDMS-109.2: Warmstart Recovery
**Acceptance Criteria:** Given a system failure, when warmstart is performed, then uncommitted changes are rolled back using journal before-images.

### IDMS-109.3: Coldstart
**Acceptance Criteria:** Given coldstart, when initiated, then the system starts fresh with empty run-state.

### IDMS-109.4: DCMT Operator Commands
**Acceptance Criteria:** Given `DCMT DISPLAY ACTIVE TASKS`, when issued, then active tasks are displayed. Given `DCMT VARY TASK`, then task attributes are modified.

### IDMS-109.5: Recovery Tests
**Acceptance Criteria:** Given journal writing and warmstart scenarios, when tested, then data integrity is maintained.

---

## IDMS-110: Lock Management & Tests

### IDMS-110.1: Record-Level Locking
**Acceptance Criteria:** Given concurrent access to the same record, when one task has an exclusive lock, then other tasks wait.

### IDMS-110.2: Area-Level Locking (READY)
**Acceptance Criteria:** Given `READY EMP-AREA USAGE-MODE UPDATE`, when issued, then the area is locked for update access.

### IDMS-110.3: Deadlock Detection
**Acceptance Criteria:** Given two tasks each waiting for the other's lock, when detected, then one task is rolled back.

### IDMS-110.4: COMMIT/ROLLBACK
**Acceptance Criteria:** Given COMMIT, when issued, then changes are committed and locks released. Given ROLLBACK, then changes are undone.

### IDMS-110.5: Concurrency Tests
**Acceptance Criteria:** Given concurrent DML scenarios with locking, when tested, then data integrity and deadlock handling work correctly.

---

## FR/NFR Coverage

**Coverage: 12/12 FRs (100%), 3/3 NFRs (100%)**
