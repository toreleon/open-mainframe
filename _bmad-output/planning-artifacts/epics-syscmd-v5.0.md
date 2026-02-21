---
version: 'v5.0'
planningGroup: 'PG-10'
technology: 'System Commands & Console'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-syscmd-v5.0.md'
  - 'architecture-syscmd-v5.0.md'
totalEpics: 2
totalStories: 18
frCoverage: '13/13 (100%)'
nfrCoverage: '4/4 (100%)'
---

# Epics & Stories: System Commands & Console

## Epic Overview

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| SYS-102 | System Command Dispatcher & DISPLAY Commands | L | 9 | B |
| SYS-103 | SDSF — System Display and Search Facility | XL | 9 | B |

---

## SYS-102: System Command Dispatcher & DISPLAY Commands

**User Value:** Operators can issue MVS system commands (DISPLAY, START, STOP, CANCEL, REPLY) to monitor and control the system.

### SYS-102.1: Command Dispatcher and Parser

**As a** operator, **I want** system commands parsed and dispatched, **so that** commands execute correctly.

**Acceptance Criteria:**
- Given `D A,L`, when parsed, then command=DISPLAY, subcommand=A, option=L
- Given an unknown command, when dispatched, then error message IEE305I is returned
- Given `CommandRegistry`, when a command is registered, then it is dispatchable by name

### SYS-102.2: DISPLAY A,L — Active Address Spaces

**As a** operator, **I want** `D A,L` to list active jobs and address spaces, **so that** I can see what's running.

**Acceptance Criteria:**
- Given 10 active jobs, when `D A,L` is issued, then all 10 are listed with jobname, ASID, status (IN/OUT/SWAPPED), step name
- Given job filters, when `D A,L,jobname` is issued, then only matching jobs are shown

### SYS-102.3: DISPLAY J — Job Information

**As a** operator, **I want** `D J,jobname` to show detailed job information.

**Acceptance Criteria:**
- Given running job PAYROLL, when `D J,PAYROLL` is issued, then current step, program, CPU time, and initiator are displayed

### SYS-102.4: DISPLAY T and DISPLAY M

**As a** operator, **I want** `D T` for date/time and `D M` for storage information.

**Acceptance Criteria:**
- Given `D T`, when issued, then current date, time, and IPL info are displayed in IEE136I format
- Given `D M`, when issued, then storage summary (real, auxiliary, CSA, SQA) is displayed

### SYS-102.5: START/STOP/MODIFY Commands

**As a** operator, **I want** to start, stop, and modify address spaces.

**Acceptance Criteria:**
- Given `S MYPROG,PARM='DATA'`, when issued, then address space is started with the specified program and parameters
- Given `P MYPROG`, when issued, then the address space receives stop signal
- Given `F MYPROG,PARM1`, when issued, then the MODIFY command text is delivered to the address space

### SYS-102.6: CANCEL and FORCE Commands

**As a** operator, **I want** to cancel jobs and force-terminate address spaces.

**Acceptance Criteria:**
- Given `C MYJOB`, when issued, then the job receives cancel notification
- Given `FORCE MYJOB,ARM`, when issued, then the address space is forcefully terminated

### SYS-102.7: REPLY Command (WTOR Integration)

**As a** operator, **I want** to reply to WTOR (Write To Operator with Reply) messages.

**Acceptance Criteria:**
- Given outstanding WTOR with reply ID 05, when `R 05,'YES'` is issued, then the reply is delivered to the waiting program
- Given invalid reply ID, when REPLY is issued, then error message is returned

### SYS-102.8: JES2 Command Routing

**As a** operator, **I want** JES2 commands ($D, $S, $P, $C, $A, $T) routed to JES2.

**Acceptance Criteria:**
- Given `$DA`, when issued, then the command is routed to JES2 and active jobs are displayed
- Given `$DJ(12345)`, when issued, then job 12345 details are shown

### SYS-102.9: Command Output Formatting and Tests

**Acceptance Criteria:**
- Given command output, when formatted, then messages use IEE prefix with standard routing codes
- Given `cargo test -p open-mainframe-console` command tests, then all pass

---

## SYS-103: SDSF — System Display and Search Facility

**User Value:** Operators and developers can browse active jobs, job status, output, and system log through SDSF panels — the primary operational interface.

### SYS-103.1: SDSF Data Model and Engine

**As a** developer, **I want** a panel data model with filtering and sorting, **so that** SDSF panels are efficiently managed.

**Acceptance Criteria:**
- Given SdsfPanel model, when jobs are loaded, then filtering by name/status and sorting by time/priority are supported
- Given 1000 jobs, when DA panel data is generated, then it completes in < 500ms

### SYS-103.2: DA Panel — Active Jobs

**As a** operator, **I want** the DA panel to show currently active jobs with live status.

**Acceptance Criteria:**
- Given 50 active jobs, when DA panel is rendered, then jobname, type (JOB/STC/TSU), status (ACTIVE/INPUT), step name, and CPU time are displayed
- Given PREFIX=PAY*, when applied, then only jobs starting with PAY are shown

### SYS-103.3: ST Panel — Job Status

**As a** developer, **I want** the ST panel to show all jobs with their completion status.

**Acceptance Criteria:**
- Given completed jobs, when ST panel is rendered, then jobname, job ID, owner, status (OUTPUT/COMPLETE), and return code are shown

### SYS-103.4: O/H Panels — Output and Held Queues

**As a** developer, **I want** O and H panels to browse job output.

**Acceptance Criteria:**
- Given job with 3 SYSOUT datasets, when O panel is rendered, then all 3 are listed with DD name, step, class, and record count
- Given `S` line command on an output dataset, when selected, then the output content is displayed for browsing

### SYS-103.5: LOG Panel — System Log

**As a** operator, **I want** the LOG panel to show system messages.

**Acceptance Criteria:**
- Given system messages from WTO, when LOG panel is rendered, then messages are displayed chronologically with timestamp, job name, and message ID

### SYS-103.6: SDSF Line Commands

**As a** user, **I want** line commands (S, SJ, SE, SP, SB, ?) on SDSF panels.

**Acceptance Criteria:**
- Given `S` on a job in ST panel, when issued, then the job's output is displayed
- Given `?` on a job, when issued, then job details (JCL, messages, etc.) are shown
- Given `SJ` on a job, when issued, then the JCL for the job is displayed

### SYS-103.7: SDSF REXX API (ISFEXEC)

**As a** REXX programmer, **I want** ISFEXEC/ISFCALLS to query SDSF programmatically.

**Acceptance Criteria:**
- Given `ADDRESS SDSF "ISFEXEC DA"` from REXX, when executed, then stem variables are populated with job data (JNAME., JNUM., JTYPE.)
- Given ISFFILTER set before ISFEXEC, when executed, then only matching jobs are returned

### SYS-103.8: SDSF-JES2 Integration

**As a** developer, **I want** SDSF integrated with JES2 spool for real-time data.

**Acceptance Criteria:**
- Given a job completing while DA panel is active, when the next refresh occurs, then the job status updates
- Given spool data for a job, when browsed via O panel, then actual spool records are displayed

### SYS-103.9: SDSF Integration Tests

**Acceptance Criteria:**
- Given 100 jobs in various states, when all SDSF panels are rendered, then correct data appears on each
- Given `cargo test -p open-mainframe-console` SDSF tests, then all pass

---

## FR/NFR Coverage Matrix

| FR | Stories |
|----|---------|
| FR-CMD-001 | SYS-102.2 |
| FR-CMD-002 | SYS-102.3 |
| FR-CMD-003 | SYS-102.4 |
| FR-CMD-004 | SYS-102.4 |
| FR-CMD-005 | SYS-102.5 |
| FR-CMD-006 | SYS-102.6 |
| FR-CMD-007 | SYS-102.7 |
| FR-CMD-008 | SYS-103.2 |
| FR-CMD-009 | SYS-103.3 |
| FR-CMD-010 | SYS-103.4 |
| FR-CMD-011 | SYS-103.5 |
| FR-CMD-012 | SYS-103.7 |
| FR-CMD-013 | SYS-102.8 |

**Coverage: 13/13 FRs (100%), 4/4 NFRs (100%)**
