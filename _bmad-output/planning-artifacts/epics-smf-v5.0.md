---
version: 'v5.0'
planningGroup: 'PG-17'
technology: 'SMF (System Management Facilities)'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-smf-v5.0.md'
  - 'architecture-smf-v5.0.md'
totalEpics: 11
totalStories: 54
frCoverage: '12/12 (100%)'
nfrCoverage: '4/4 (100%)'
---

# Epics & Stories: SMF (System Management Facilities)

## Epic Overview

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| SMF-100 | SMF Record Infrastructure | M | 5 | C |
| SMF-101 | SMFPRMxx Configuration Parser | S | 4 | C |
| SMF-102 | SMF Record Writing API | M | 5 | C |
| SMF-103 | SMF Exit Framework | M | 5 | C |
| SMF-104 | Type 30 — Job Accounting Records | L | 6 | C |
| SMF-105 | Types 14/15/17/18 — Dataset Activity | M | 5 | C |
| SMF-106 | Type 80 — Security Audit Records | M | 5 | C |
| SMF-107 | Types 70-79 — Performance Records | L | 5 | C |
| SMF-108 | Types 100-120 — Subsystem Records | L | 5 | C |
| SMF-109 | SMF Dump Utilities | M | 5 | C |
| SMF-110 | Observability Bridge | M | 4 | C |

---

## SMF-100: SMF Record Infrastructure

**User Value:** A standard SMF record format enables all subsystems to write binary-compatible accounting and audit records.

### SMF-100.1: Standard Header Structure

**As a** system programmer, **I want** SMF records with the standard header layout, **so that** records are compatible with z/OS SMF analysis tools.

**Acceptance Criteria:**
- Given an SmfHeader, when serialized, then it produces the exact binary layout (length, segment, flag, type, time, date, SID, subsystem, subtype)
- Given a raw binary SMF header, when deserialized, then all fields are correctly extracted

### SMF-100.2: Self-Defining Sections (Triplets)

**As a** system programmer, **I want** self-defining section triplets, **so that** variable-length record sections are navigable.

**Acceptance Criteria:**
- Given a triplet (offset=100, length=32, count=5), when the section is accessed, then 5 entries of 32 bytes each starting at byte 100 are returned

### SMF-100.3: Binary Serialization/Deserialization

**As a** developer, **I want** binary serde for SMF records, **so that** records can be written to and read from datasets.

**Acceptance Criteria:**
- Given an SmfRecord, when serialized to bytes and deserialized back, then the round-trip produces identical data
- Given big-endian byte order and packed decimal dates, when used, then z/OS binary compatibility is maintained

### SMF-100.4: Record Type Registry

**As a** developer, **I want** a registry mapping record types (0-255) to their parser/builder, **so that** new record types can be added declaratively.

**Acceptance Criteria:**
- Given register_type(30, Type30Parser), when a Type 30 record is received, then the correct parser handles it

### SMF-100.5: Record Infrastructure Tests

**Acceptance Criteria:**
- Given test SMF records, when serialized/deserialized, then binary compatibility is verified

---

## SMF-101: SMFPRMxx Configuration Parser

**User Value:** System programmers configure SMF behavior via PARMLIB, controlling which record types are collected and how.

### SMF-101.1: TYPE/NOTYPE Filtering

**As a** system programmer, **I want** TYPE/NOTYPE to control which record types are active.

**Acceptance Criteria:**
- Given `TYPE(30,70:79,80)`, when parsed, then types 30, 70-79, and 80 are active
- Given `NOTYPE(0:29)`, when parsed, then types 0-29 are suppressed

### SMF-101.2: Exit Configuration

**As a** system programmer, **I want** EXITS(IEFU83, IEFU84) to register SMF exits.

**Acceptance Criteria:**
- Given `EXITS(IEFU83)`, when parsed, then the IEFU83 exit is activated for record filtering

### SMF-101.3: Recording Parameters

**As a** system programmer, **I want** INTERVAL, RECORDING, and MAXBUFSIZE parameters.

**Acceptance Criteria:**
- Given `INTERVAL(SMF,0030)` (30 minutes), when parsed, then interval records are generated every 30 minutes
- Given `RECORDING(LOGSTREAM)`, when parsed, then SMF writes to a log stream instead of a dataset

### SMF-101.4: Configuration Tests

**Acceptance Criteria:**
- Given various SMFPRMxx configurations, when parsed, then all parameters are correctly applied

---

## SMF-102: SMF Record Writing API

**User Value:** All subsystems can write SMF records via a standard API, centralizing system accounting and auditing.

### SMF-102.1: SMFWTM — Write Record

**As a** developer, **I want** SMFWTM to write an SMF record to the active recording destination.

**Acceptance Criteria:**
- Given SMFWTM(record_type=30, data), when called, then the record is buffered with a proper header
- Given the record type is in NOTYPE list, when SMFWTM is called, then the record is silently dropped

### SMF-102.2: SMFEWTM — Extended Write

**As a** developer, **I want** SMFEWTM for writing records with subsystem identification.

**Acceptance Criteria:**
- Given SMFEWTM(type=110, subsystem="DB2A"), when called, then the subsystem field in the header is set

### SMF-102.3: Buffer Management

**As a** system programmer, **I want** record buffering with configurable flush intervals.

**Acceptance Criteria:**
- Given MAXBUFSIZE=64KB, when the buffer fills, then it is automatically flushed to the recording destination
- Given a timer-based flush, when the interval expires, then partial buffers are flushed

### SMF-102.4: User Record Types (128-255)

**As a** application developer, **I want** to write custom record types (128-255).

**Acceptance Criteria:**
- Given SMFWTM(type=200, data), when called, then the user record is written with the standard header

### SMF-102.5: Writing API Tests

**Acceptance Criteria:**
- Given record write tests, when run, then all record types are correctly buffered and flushed

---

## SMF-103: SMF Exit Framework

**User Value:** System programmers can filter, modify, or route SMF records via exit programs, customizing SMF behavior.

### SMF-103.1: IEFU83 — Record Filtering Exit

**As a** system programmer, **I want** IEFU83 to examine and optionally suppress SMF records.

**Acceptance Criteria:**
- Given an IEFU83 exit that suppresses Type 14 records for temporary datasets, when a Type 14 record for a temp dataset is written, then it is dropped

### SMF-103.2: IEFU84 — Subsystem Identification Exit

**As a** system programmer, **I want** IEFU84 to add subsystem identification to records.

**Acceptance Criteria:**
- Given IEFU84 adds subsystem ID "PROD" to all records, when records are written, then the subsystem field contains "PROD"

### SMF-103.3: IFASMFEX — Dynamic Exit Registration

**As a** system programmer, **I want** dynamic exit registration at runtime.

**Acceptance Criteria:**
- Given IFASMFEX(register, my_exit, types=[30, 80]), when called, then the exit is invoked for Type 30 and Type 80 records

### SMF-103.4: Exit Processing Pipeline

**As a** system programmer, **I want** multiple exits chained in order.

**Acceptance Criteria:**
- Given exits A, B, C registered, when a record is written, then exits are invoked in registration order; if any returns Suppress, the record is dropped

### SMF-103.5: Exit Framework Tests

**Acceptance Criteria:**
- Given exit registration and filtering tests, when run, then exits correctly filter and modify records

---

## SMF-104: Type 30 — Job Accounting Records

**User Value:** Job accounting enables chargeback, capacity planning, and job performance analysis.

### SMF-104.1: Subtype 1 — Job Initiation

**As a** system programmer, **I want** Type 30 subtype 1 when a job starts.

**Acceptance Criteria:**
- Given job PAYROLL starts, when initiation occurs, then a Type 30.1 record is written with job name, job ID, class, priority, and start time

### SMF-104.2: Subtype 2 — Interval Record

**As a** system programmer, **I want** Type 30 subtype 2 at configured intervals during job execution.

**Acceptance Criteria:**
- Given INTERVAL(SMF,0030), when 30 minutes pass during job execution, then a Type 30.2 record is written with CPU time, EXCP count, and service units

### SMF-104.3: Subtype 3 — Step Termination

**As a** system programmer, **I want** Type 30 subtype 3 when a job step ends.

**Acceptance Criteria:**
- Given step STEP01 completes with RC=0, when step termination occurs, then a Type 30.3 record is written with step name, program name, completion code, CPU time, and elapsed time

### SMF-104.4: Subtype 4 — Job Termination

**As a** system programmer, **I want** Type 30 subtype 4 when a job ends.

**Acceptance Criteria:**
- Given job PAYROLL completes, when job termination occurs, then a Type 30.4 record is written with total CPU, elapsed time, max return code, and service class

### SMF-104.5: WLM Service Class Integration

**As a** capacity planner, **I want** WLM service class in Type 30 records.

**Acceptance Criteria:**
- Given job classified as service class BATCH_HIGH, when Type 30 is written, then the service class field contains "BATCH_HI"

### SMF-104.6: Job Accounting Tests

**Acceptance Criteria:**
- Given a job lifecycle (initiation → step → termination), when SMF records are collected, then all subtypes are present and consistent

---

## SMF-105: Types 14/15/17/18 — Dataset Activity

**User Value:** Dataset activity tracking enables storage auditing and lifecycle management.

### SMF-105.1: Type 14 — Dataset Input (Read)

**As a** system programmer, **I want** Type 14 records when datasets are read.

**Acceptance Criteria:**
- Given dataset MY.DATA opened for input, when the CLOSE occurs, then a Type 14 record is written with DSN, VOLSER, EXCP count, and job name

### SMF-105.2: Type 15 — Dataset Output (Write)

**As a** system programmer, **I want** Type 15 records when datasets are written.

**Acceptance Criteria:**
- Given dataset MY.DATA opened for output, when CLOSE occurs, then a Type 15 record is written

### SMF-105.3: Type 17 — Dataset Scratch

**As a** system programmer, **I want** Type 17 records when datasets are deleted.

**Acceptance Criteria:**
- Given IDCAMS DELETE MY.DATA, when executed, then a Type 17 record is written with DSN, VOLSER, and deletion time

### SMF-105.4: Type 18 — Dataset Rename

**As a** system programmer, **I want** Type 18 records when datasets are renamed.

**Acceptance Criteria:**
- Given ALTER MY.DATA NEWNAME(MY.NEWDATA), when executed, then a Type 18 record is written with old and new names

### SMF-105.5: Dataset Activity Tests

**Acceptance Criteria:**
- Given dataset open/close/delete/rename operations, when SMF records are collected, then Types 14/15/17/18 are present

---

## SMF-106: Type 80 — Security Audit Records

**User Value:** Security auditing via SMF Type 80 enables compliance and forensic analysis of RACF events.

### SMF-106.1: RACF Command Events

**As a** security auditor, **I want** Type 80 records for RACF commands (ADDUSER, ALTUSER, PERMIT, etc.).

**Acceptance Criteria:**
- Given `PERMIT PAYROLL.DATA CLASS(DATASET) ID(JSMITH) ACCESS(READ)`, when executed, then a Type 80 record is written with command text, issuer, and result

### SMF-106.2: Access Violation Events

**As a** security auditor, **I want** Type 80 records for access violations.

**Acceptance Criteria:**
- Given user JSMITH denied access to PAYROLL.DATA, when the violation occurs, then a Type 80 record logs the user, resource, class, and access attempted

### SMF-106.3: Logon/Logoff Events

**As a** security auditor, **I want** Type 80 records for logon/logoff activity.

**Acceptance Criteria:**
- Given successful TSO logon by JSMITH, when the logon completes, then a Type 80 subtype records the event with timestamp and terminal

### SMF-106.4: Event Severity and Filtering

**As a** security auditor, **I want** events categorized by severity and filterable.

**Acceptance Criteria:**
- Given AUDIT(ALL) on a RACF profile, when any access occurs, then Type 80 records are generated for all access levels

### SMF-106.5: Security Audit Tests

**Acceptance Criteria:**
- Given RACF command, access, and logon events, when SMF records are collected, then Type 80 records accurately reflect the events

---

## SMF-107: Types 70-79 — Performance Records

**User Value:** Performance records enable capacity planning, bottleneck analysis, and SLA monitoring.

### SMF-107.1: Type 70 — CPU Activity

**As a** capacity planner, **I want** Type 70 records with CPU utilization data.

**Acceptance Criteria:**
- Given a recording interval, when it expires, then a Type 70 record is written with total CPU, wait time, and per-processor breakdowns

### SMF-107.2: Type 71 — Paging Activity

**As a** capacity planner, **I want** Type 71 records with paging statistics.

**Acceptance Criteria:**
- Given a recording interval, when it expires, then a Type 71 record captures page-in/page-out rates, slot usage, and auxiliary storage utilization

### SMF-107.3: Type 72 — Workload Activity

**As a** capacity planner, **I want** Type 72 records with WLM workload metrics.

**Acceptance Criteria:**
- Given WLM service classes, when a Type 72 record is written, then each service class has CPU time, response time, PI (Performance Index), and transaction count

### SMF-107.4: Type 74 — Device Activity

**As a** capacity planner, **I want** Type 74 records with I/O device statistics.

**Acceptance Criteria:**
- Given DASD volumes, when a Type 74 is written, then each volume has I/O count, response time, connect time, and queue depth

### SMF-107.5: Performance Record Tests

**Acceptance Criteria:**
- Given performance metric collection, when interval records are generated, then Types 70-74 contain accurate data

---

## SMF-108: Types 100-120 — Subsystem Records

**User Value:** Subsystem-specific SMF records enable detailed monitoring and accounting for DB2, CICS, MQ, and TCP/IP.

### SMF-108.1: Type 100/101 — DB2 Records

**As a** DBA, **I want** DB2 accounting and performance records.

**Acceptance Criteria:**
- Given a DB2 thread completes, when a Type 101 record is written, then SQL statement counts, elapsed time, and buffer pool statistics are included

### SMF-108.2: Type 110 — CICS Records

**As a** CICS administrator, **I want** CICS transaction monitoring records.

**Acceptance Criteria:**
- Given a CICS transaction completes, when a Type 110 record is written, then transaction ID, elapsed time, CPU time, and response time are included

### SMF-108.3: Type 115/116 — MQ Records

**As a** MQ administrator, **I want** MQ accounting and statistics records.

**Acceptance Criteria:**
- Given MQ channel activity, when a Type 116 record is written, then messages sent/received, bytes transferred, and channel status are included

### SMF-108.4: Type 119 — TCP/IP Records

**As a** network administrator, **I want** TCP/IP connection records.

**Acceptance Criteria:**
- Given a TCP connection closes, when a Type 119 record is written, then source/dest IP, port, bytes transferred, and duration are included

### SMF-108.5: Subsystem Record Tests

**Acceptance Criteria:**
- Given DB2, CICS, MQ, and TCP/IP subsystem activity, when records are collected, then Types 100-119 are accurate

---

## SMF-109: SMF Dump Utilities

**User Value:** Dump utilities extract and filter SMF records from datasets for analysis and reporting.

### SMF-109.1: IFASMFDP — Dump Program

**As a** system programmer, **I want** IFASMFDP to dump SMF records from datasets with filtering.

**Acceptance Criteria:**
- Given IFASMFDP with TYPE(30,80), when run, then only Type 30 and Type 80 records are dumped to the output dataset

### SMF-109.2: Date/Time Filtering

**As a** system programmer, **I want** to filter by date and time range.

**Acceptance Criteria:**
- Given START(0800) END(1700) DATE(2026052), when applied, then only records from 8am to 5pm on that date are included

### SMF-109.3: Jobname/System Filtering

**As a** system programmer, **I want** to filter by job name and system ID.

**Acceptance Criteria:**
- Given JOBNAME(PAY*), when applied, then only records from jobs starting with PAY are included

### SMF-109.4: Output Formatting

**As a** system programmer, **I want** output to sequential dataset or human-readable report.

**Acceptance Criteria:**
- Given OUTDD(SMFOUT), when dump runs, then records are written to the SMFOUT DD in binary format
- Given FORMAT(REPORT), when specified, then a human-readable summary is generated

### SMF-109.5: Dump Utility Tests

**Acceptance Criteria:**
- Given test SMF datasets, when dump utility is run with various filters, then correct records are extracted

---

## SMF-110: Observability Bridge

**User Value:** Bridging SMF to modern observability tools enables enterprise monitoring without z/OS-specific tooling.

### SMF-110.1: SMF → Prometheus Exporter

**As a** DevOps engineer, **I want** SMF records exposed as Prometheus metrics.

**Acceptance Criteria:**
- Given Type 30 job records, when the bridge runs, then metrics like `smf_job_cpu_seconds`, `smf_job_elapsed_seconds` are available at /metrics
- Given Type 72 workload records, when bridged, then `smf_wlm_performance_index` is exposed per service class

### SMF-110.2: SMF → OpenTelemetry Spans

**As a** DevOps engineer, **I want** SMF records converted to OpenTelemetry spans.

**Acceptance Criteria:**
- Given a Type 30 job lifecycle, when bridged, then a trace span with initiation → step → termination is created

### SMF-110.3: Prometheus → SMF Reverse Bridge

**As a** system programmer, **I want** existing Prometheus metrics written as SMF user records.

**Acceptance Criteria:**
- Given application metrics in Prometheus format, when the reverse bridge runs, then SMF Type 200 (user) records are generated

### SMF-110.4: Bridge Tests

**Acceptance Criteria:**
- Given SMF records and Prometheus metrics, when bridged in both directions, then data is accurately converted

---

## FR/NFR Coverage Matrix

| FR | Stories |
|----|---------|
| FR-SMF-001 | SMF-100.1, SMF-100.2 |
| FR-SMF-002 | SMF-100.3 |
| FR-SMF-003 | SMF-101.1, SMF-101.2, SMF-101.3 |
| FR-SMF-004 | SMF-102.1, SMF-102.2, SMF-102.3, SMF-102.4 |
| FR-SMF-005 | SMF-103.1, SMF-103.2, SMF-103.3, SMF-103.4 |
| FR-SMF-006 | SMF-104.1, SMF-104.2, SMF-104.3, SMF-104.4, SMF-104.5 |
| FR-SMF-007 | SMF-105.1, SMF-105.2, SMF-105.3, SMF-105.4 |
| FR-SMF-008 | SMF-106.1, SMF-106.2, SMF-106.3, SMF-106.4 |
| FR-SMF-009 | SMF-107.1, SMF-107.2, SMF-107.3, SMF-107.4 |
| FR-SMF-010 | SMF-108.1, SMF-108.2, SMF-108.3, SMF-108.4 |
| FR-SMF-011 | SMF-109.1, SMF-109.2, SMF-109.3, SMF-109.4 |
| FR-SMF-012 | SMF-110.1, SMF-110.2, SMF-110.3 |

| NFR | Stories |
|-----|---------|
| NFR-SMF-001 | SMF-102.3 |
| NFR-SMF-002 | SMF-109.1 |
| NFR-SMF-003 | SMF-102.4 |
| NFR-SMF-004 | SMF-100.3 |

**Coverage: 12/12 FRs (100%), 4/4 NFRs (100%)**
