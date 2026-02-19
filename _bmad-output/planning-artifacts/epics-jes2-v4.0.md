# JES2 Crate — Epics & Stories

## Epic J100: Job Queue & Scheduling Engine

**Goal:** Implement the JES2 job queue with priority scheduling, job states, and class-based selection.

**Crate:** `open-mainframe-jes2`
**FRs:** FR-v4.0-J100

### Story J100.1: Job Queue Data Structure

As a **batch programmer**,
I want **jobs to be queued with priority and class attributes**,
So that **the system processes jobs in the correct order**.

**Acceptance Criteria:**

**Given** a job submitted with `CLASS=A,PRTY=15`
**When** added to the job queue
**Then** it is placed ahead of lower-priority jobs in the same class

**Given** multiple jobs in the queue
**When** an initiator requests work for CLASS=A
**Then** the highest-priority CLASS=A job is selected

**Complexity:** L

### Story J100.2: Job State Machine

As a **system operator**,
I want **jobs to progress through defined states**,
So that **I can track job status and intervene when needed**.

**Acceptance Criteria:**

**Given** a job enters the system via SUBMIT
**When** it progresses through processing
**Then** it transitions: Input → Conversion → Ready → Running → Output → Purge

**Given** a job is in Output state
**When** `$P JOBnnnnn` is issued
**Then** the job is purged from the system

**Given** `//JOB1 JOB ...,TYPRUN=HOLD`
**When** submitted
**Then** the job enters HELD state and does not progress until released with `$A JOBnnnnn`

**Complexity:** M

### Story J100.3: Job Class Definitions

As a **system programmer**,
I want **job classes (A-Z, 0-9) with configurable attributes**,
So that **I can partition workload by type**.

**Acceptance Criteria:**

**Given** `JOBCLASS(A) PROCLIB=PROC00,MSGCLASS=X,MAXRC=4`
**When** a CLASS=A job runs
**Then** it uses PROC00 for procedure libraries and routes job log to MSGCLASS X

**Given** STC (started task) and TSU (time-sharing user) pseudo-classes
**When** a started task runs
**Then** it uses the STC class attributes

**Complexity:** M

---

## Epic J101: Initiator Management

**Goal:** Implement JES2-managed initiators that select and execute jobs from the queue.

**Crate:** `open-mainframe-jes2`
**FRs:** FR-v4.0-J101

### Story J101.1: Initiator Pool

As a **system operator**,
I want **initiators to be started and stopped dynamically**,
So that **I can control how many jobs run concurrently**.

**Acceptance Criteria:**

**Given** `$SI1-5,CLASSES=AB` (start initiators 1-5 for classes A and B)
**When** jobs are in the queue for CLASS=A
**Then** the initiators select and execute those jobs concurrently

**Given** `$PI3` (stop initiator 3)
**When** the current job in initiator 3 completes
**Then** initiator 3 drains and stops accepting new work

**Complexity:** M

### Story J101.2: WLM-Managed Initiators

As a **system programmer**,
I want **WLM to manage initiator count based on workload goals**,
So that **the system automatically scales batch processing capacity**.

**Acceptance Criteria:**

**Given** WLM service class BATCHHI with goal: 80% of jobs complete within 30 minutes
**When** batch work accumulates beyond the goal
**Then** WLM starts additional initiators to meet the goal

**Complexity:** L

---

## Epic J102: Spool Management

**Goal:** Implement JES2 spool for storing SYSOUT datasets and job output.

**Crate:** `open-mainframe-jes2`
**FRs:** FR-v4.0-J102

### Story J102.1: Spool Storage and SYSOUT

As a **batch programmer**,
I want **job output (SYSOUT) to be stored on the JES2 spool**,
So that **I can view and manage output after job completion**.

**Acceptance Criteria:**

**Given** a JCL step with `//OUT DD SYSOUT=A`
**When** the step writes to the DD
**Then** the data is stored on the JES2 spool with SYSOUT class A

**Given** a job completes and has SYSOUT datasets
**When** `$HASP250 JOB00123 ENDED - RC=0000` is issued
**Then** the job output is available for viewing/printing in the output queue

**Complexity:** L

### Story J102.2: Checkpoint and Recovery

As a **system programmer**,
I want **JES2 checkpoint for spool recovery**,
So that **spool data survives system restarts**.

**Acceptance Criteria:**

**Given** JES2 is configured with dual checkpoint datasets
**When** JES2 restarts after a failure
**Then** warm start recovers all queued and output jobs from the checkpoint

**Given** a cold start is performed
**When** JES2 initializes
**Then** the spool is rebuilt from remaining checkpoint data

**Complexity:** L

---

## Epic J103: Output Processing

**Goal:** Implement SYSOUT routing, output descriptors, and disposition processing.

**Crate:** `open-mainframe-jes2`
**FRs:** FR-v4.0-J103

### Story J103.1: SYSOUT Class Routing and Output Disposition

As a **batch programmer**,
I want **SYSOUT class routing and output disposition control**,
So that **my job output goes to the right destination with the right handling**.

**Acceptance Criteria:**

**Given** `//OUT DD SYSOUT=A,DEST=RMT001`
**When** the output is processed
**Then** it is routed to SYSOUT class A at destination RMT001

**Given** `//OUTDEF OUTPUT JESDS=ALL,OUTDISP=(WRITE,PURGE)`
**When** the job completes normally
**Then** the output is written (printed) and then purged

**Given** `MSGCLASS=X` on the JOB statement
**When** the job completes
**Then** the job log and JES messages are routed to SYSOUT class X

**Complexity:** M

---

## Epic J104: JES2 Operator Commands

**Goal:** Implement the JES2 $ prefix command set for system operation.

**Crate:** `open-mainframe-jes2`
**FRs:** FR-v4.0-J104

### Story J104.1: Display and Control Commands

As a **system operator**,
I want **$D/$S/$P/$A/$C/$T/$H commands to monitor and control JES2**,
So that **I can manage jobs, initiators, printers, and queues**.

**Acceptance Criteria:**

**Given** `$DA` (display active jobs)
**When** executed
**Then** all running jobs are displayed with job name, number, step, and initiator

**Given** `$HQ JOB00123` (hold job)
**When** executed
**Then** the job is placed in HOLD state

**Given** `$CJ JOB00123` (cancel job)
**When** executed
**Then** the running job is cancelled; output is retained for the operator

**Given** `$TI1,CLASSES=ABCD` (modify initiator 1 classes)
**When** executed
**Then** initiator 1 now accepts jobs from classes A, B, C, and D

Commands: $D (display), $S (start), $P (stop/drain), $A (release), $C (cancel), $T (modify), $H (hold), $E (restart), $Z (halt), $L (list)

**Complexity:** L

---

## Epic J105: JECL Statement Processing

**Goal:** Parse JES2 control language (JECL) statements in JCL input.

**Crate:** `open-mainframe-jes2`
**FRs:** FR-v4.0-J105

### Story J105.1: JECL Parser

As a **batch programmer**,
I want **JECL statements (/*JOBPARM, /*ROUTE, /*OUTPUT, /*PRIORITY) parsed and applied**,
So that **I can control JES2 processing from within my JCL**.

**Acceptance Criteria:**

**Given** `/*JOBPARM SYSAFF=(SY1,SY2),LINECT=60,COPIES=2`
**When** parsed
**Then** the job is constrained to systems SY1 and SY2 with 60-line pages and 2 copies

**Given** `/*ROUTE PRINT RMT001`
**When** parsed
**Then** all SYSOUT for this job is routed to RMT001

**Given** `/*PRIORITY 15`
**When** parsed
**Then** the job priority is set to 15

**Complexity:** M

---

## Epic J106: JES2 Initialization & Configuration

**Goal:** Parse JES2PARM configuration and support initialization parameters.

**Crate:** `open-mainframe-jes2`
**FRs:** FR-v4.0-J106

### Story J106.1: JES2PARM Configuration Parser

As a **system programmer**,
I want **JES2PARM initialization statements parsed**,
So that **JES2 behavior is configurable via standard configuration files**.

**Acceptance Criteria:**

**Given** `JOBCLASS(A) PROCLIB=PROC00,MSGCLASS=X,MAXRC=4` in JES2PARM
**When** JES2 initializes
**Then** job class A is configured with the specified attributes

**Given** `SPOOLDEF VOLUMES(SPOOL1,SPOOL2),TGSIZE(100)` in JES2PARM
**When** JES2 initializes
**Then** spool is configured on the specified volumes

Key statements: JOBCLASS, OUTCLASS, SPOOL, SPOOLDEF, CKPTDEF, INIT, INITDEF, PRT, PUN, RDR, NJEDEF, NODE, LINE, MASDEF, MEMBER

**Complexity:** M

---

## Epic J107: Internal Reader and SDSF Integration

**Goal:** Implement the internal reader for job-within-job submission and SDSF for output browsing.

**Crate:** `open-mainframe-jes2`
**FRs:** FR-v4.0-J107

### Story J107.1: Internal Reader (INTRDR)

As a **batch programmer**,
I want **SYSOUT to the internal reader to submit jobs from within jobs**,
So that **I can chain and generate jobs dynamically**.

**Acceptance Criteria:**

**Given** `//SUBMIT DD SYSOUT=(,INTRDR)`
**When** JCL is written to this DD
**Then** the JCL is submitted as a new job to JES2

**Complexity:** M

### Story J107.2: SDSF Panel Model

As a **system operator**,
I want **SDSF-style job status display and output browsing**,
So that **I can monitor and manage jobs interactively**.

**Acceptance Criteria:**

**Given** the SDSF status display (ST panel)
**When** the operator enters `ST`
**Then** all jobs are listed with jobname, number, owner, status, RC, class

**Given** the operator selects a job's output with `S` line command
**When** processed
**Then** the SYSOUT datasets are listed for browsing

**Complexity:** L

---
