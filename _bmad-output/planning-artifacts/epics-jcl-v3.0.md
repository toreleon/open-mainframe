# JCL Crate — Epics & Stories

## Epic 100: Symbolic Parameter Substitution & SET Statement

**Goal:** Enable symbolic parameter substitution (&NAME) in JCL text, including SET statement support and double-period handling.

**Crate:** `open-mainframe-jcl`
**FRs:** FR-v3.0-101

### Story 100.1: Symbol Table and Text-Level Substitution

As a **JCL developer**,
I want **symbolic parameters (&NAME) to be resolved in operand text**,
So that **I can parameterize my JCL for different environments**.

**Acceptance Criteria:**

**Given** JCL with `//STEP1 EXEC PGM=&PROGRAM` and a symbol PROGRAM=MYAPP
**When** substitution runs
**Then** the operand becomes `PGM=MYAPP`

**Given** `DSN=&HLQ..DATA.&ENV` with HLQ=PROD, ENV=DAILY
**When** substitution runs
**Then** DSN resolves to `PROD.DATA.DAILY` (double period becomes single)

**Given** `&&TEMP` in a dataset name
**When** substitution runs
**Then** it resolves to a system-generated temporary name

**Complexity:** M

### Story 100.2: SET Statement Parsing

As a **JCL developer**,
I want **SET statements to define symbolic parameter values**,
So that **I can define symbols inline in my JCL**.

**Acceptance Criteria:**

**Given** `// SET HLQ=PROD,ENV=DAILY`
**When** parsed
**Then** symbols HLQ and ENV are added to the symbol table with their values

**Given** multiple SET statements
**When** a later SET redefines a symbol
**Then** the later value overrides the earlier one

**Complexity:** S

---

## Epic 101: In-Stream Procedure Support (PROC/PEND)

**Goal:** Parse and expand in-stream procedures defined within the JCL stream.

**Crate:** `open-mainframe-jcl`
**FRs:** FR-v3.0-100

### Story 101.1: In-Stream Procedure Parsing

As a **JCL developer**,
I want **in-stream procedures (PROC...PEND) to be parsed**,
So that **I can define reusable step groups within my JCL**.

**Acceptance Criteria:**

**Given** JCL containing:
```
//MYPROC PROC HLQ=TEST
//STEP1  EXEC PGM=MYPROG
//INPUT  DD DSN=&HLQ..DATA,DISP=SHR
//       PEND
```
**When** parsed
**Then** an in-stream procedure named MYPROC with default HLQ=TEST is stored

**Complexity:** M

### Story 101.2: In-Stream Procedure Expansion

As a **JCL developer**,
I want **EXEC MYPROC to expand the in-stream procedure**,
So that **procedure steps are inserted into the job with symbolics resolved**.

**Acceptance Criteria:**

**Given** `//RUN EXEC MYPROC,HLQ=PROD`
**When** the procedure is expanded
**Then** STEP1 uses DSN=PROD.DATA (HLQ overridden to PROD)

**Given** `//RUN EXEC MYPROC` (no override)
**When** the procedure is expanded
**Then** STEP1 uses DSN=TEST.DATA (default HLQ=TEST from PROC)

**Complexity:** M

---

## Epic 102: Cataloged Procedure Support

**Goal:** Resolve and expand cataloged procedures from a procedure library.

**Crate:** `open-mainframe-jcl`
**FRs:** FR-v3.0-100, FR-v3.0-103, FR-v3.0-110

### Story 102.1: JCLLIB Statement Parsing

As a **JCL developer**,
I want **JCLLIB ORDER to specify procedure library search order**,
So that **the system knows where to find my cataloged procedures**.

**Acceptance Criteria:**

**Given** `// JCLLIB ORDER=(PROD.PROCLIB,TEST.PROCLIB)`
**When** parsed
**Then** the library search order is [PROD.PROCLIB, TEST.PROCLIB]

**Complexity:** S

### Story 102.2: ProcedureLibrary Trait and Filesystem Implementation

As a **system integrator**,
I want **a ProcedureLibrary trait for resolving procedure names to JCL text**,
So that **procedure sources can be loaded from PDS directories or other backends**.

**Acceptance Criteria:**

**Given** a PDS directory at `datasets/PROD/PROCLIB/`
**When** EXEC MYPROC is encountered
**Then** the system reads `datasets/PROD/PROCLIB/MYPROC` (or .jcl) and parses it

**Given** JCLLIB ORDER with multiple libraries
**When** a procedure is not found in the first library
**Then** subsequent libraries are searched in order

**Complexity:** M

### Story 102.3: Cataloged Procedure Expansion with Overrides

As a **JCL developer**,
I want **DD overrides on cataloged procedure steps**,
So that **I can customize datasets without modifying the procedure**.

**Acceptance Criteria:**

**Given** a cataloged procedure MYPROC with STEP1.INPUT DD DSN=DEFAULT.DATA
**When** the calling JCL specifies `//STEP1.INPUT DD DSN=OVERRIDE.DATA,DISP=SHR`
**Then** the DD is replaced with the override values

**Given** a new DD added as `//STEP1.EXTRA DD DSN=NEW.DATA,DISP=SHR`
**When** the procedure is expanded
**Then** the EXTRA DD is added to STEP1

**Complexity:** L

### Story 102.4: Nested Procedure Expansion

As a **JCL developer**,
I want **procedures to call other procedures (nesting up to 15 levels)**,
So that **complex job flows can be built from composable pieces**.

**Acceptance Criteria:**

**Given** PROC-A calls PROC-B which calls PROC-C
**When** expanded
**Then** all three levels are correctly resolved with symbolic substitution

**Given** nesting depth exceeds 15
**When** expansion runs
**Then** an error is reported: "Procedure nesting exceeds maximum depth of 15"

**Complexity:** M

---

## Epic 103: IF/THEN/ELSE/ENDIF Conditional Processing

**Goal:** Parse and execute conditional step execution using IF/THEN/ELSE/ENDIF.

**Crate:** `open-mainframe-jcl`
**FRs:** FR-v3.0-102

### Story 103.1: Condition Expression Parser

As a **JCL developer**,
I want **IF conditions to be parsed into an expression tree**,
So that **complex conditions can be evaluated at runtime**.

**Acceptance Criteria:**

**Given** `// IF (STEP1.RC = 0) THEN`
**When** parsed
**Then** a condition expression comparing STEP1's RC to 0 with EQ operator is produced

**Given** `// IF (STEP1.RC <= 4 & STEP2.RC = 0) | STEP3.ABEND THEN`
**When** parsed
**Then** nested logical AND/OR with ABEND keyword is produced

**Complexity:** M

### Story 103.2: IF/THEN/ELSE/ENDIF AST and Parser

As a **JCL developer**,
I want **IF/THEN/ELSE/ENDIF blocks to be parsed into an AST**,
So that **conditional step groups are properly structured**.

**Acceptance Criteria:**

**Given** JCL with IF/THEN containing EXEC steps and ELSE with different steps
**When** parsed
**Then** an IfConstruct AST node contains then_steps and else_steps

**Given** nested IF/THEN/ELSE/ENDIF up to 15 levels
**When** parsed
**Then** the nesting is correctly represented in the AST

**Complexity:** L

### Story 103.3: Condition Evaluation at Runtime

As a **JCL developer**,
I want **IF conditions evaluated against actual step results**,
So that **steps execute or are bypassed based on runtime outcomes**.

**Acceptance Criteria:**

**Given** STEP1 returns RC=0 and the IF condition is `STEP1.RC = 0`
**When** evaluated
**Then** the THEN branch steps execute

**Given** STEP1 abends and the IF condition is `STEP1.ABEND`
**When** evaluated
**Then** the condition is true

**Complexity:** M

---

## Epic 104: Extended DD Parameters

**Goal:** Parse the full set of DD statement parameters from IBM JCL Reference.

**Crate:** `open-mainframe-jcl`
**FRs:** FR-v3.0-105

### Story 104.1: SMS Class Parameters (DATACLAS, MGMTCLAS, STORCLAS)

As a **JCL developer**,
I want **SMS class parameters parsed on DD statements**,
So that **SMS-managed dataset attributes are captured**.

**Acceptance Criteria:**

**Given** `//OUTPUT DD DSN=MY.DATA,DISP=(NEW,CATLG),STORCLAS=FAST,DATACLAS=LARGE,MGMTCLAS=WEEKLY`
**When** parsed
**Then** DatasetDef has storclas=Some("FAST"), dataclas=Some("LARGE"), mgmtclas=Some("WEEKLY")

**Complexity:** S

### Story 104.2: LABEL, EXPDT, RETPD Parameters

As a **JCL developer**,
I want **LABEL, EXPDT, and RETPD parameters parsed**,
So that **tape and expiration-controlled datasets are supported**.

**Acceptance Criteria:**

**Given** `//TAPE DD DSN=MY.TAPE,LABEL=(1,SL),EXPDT=2026/365`
**When** parsed
**Then** DatasetDef has label with sequence=1, type=SL, and expdt=2026/365

**Given** `//OUTPUT DD DSN=MY.DATA,RETPD=30`
**When** parsed
**Then** DatasetDef has retpd=Some(30)

**Complexity:** M

### Story 104.3: DSNTYPE, LIKE, REFDD, KEYLEN, KEYOFF

As a **JCL developer**,
I want **extended dataset definition parameters parsed**,
So that **PDSE, model references, and VSAM key specifications work**.

**Acceptance Criteria:**

**Given** `//OUTPUT DD DSN=MY.PDSE,DSNTYPE=LIBRARY`
**When** parsed
**Then** DatasetDef has dsntype=Some(Library)

**Given** `//OUTPUT DD DSN=NEW.DS,LIKE=MODEL.DS`
**When** parsed
**Then** DatasetDef has like=Some("MODEL.DS")

**Complexity:** M

### Story 104.4: USS Path Parameters (PATH, PATHDISP, PATHMODE, PATHOPTS)

As a **JCL developer**,
I want **Unix System Services file parameters parsed**,
So that **JCL can reference USS files via PATH=**.

**Acceptance Criteria:**

**Given** `//USSFILE DD PATH='/u/user/file.txt',PATHOPTS=(ORDWR,OCREAT),PATHMODE=(SIRWXU)`
**When** parsed
**Then** DdDefinition is a UssFile variant with path, options, and mode

**Complexity:** M

---

## Epic 105: Extended JOB and EXEC Parameters

**Goal:** Parse the full set of JOB and EXEC statement parameters.

**Crate:** `open-mainframe-jcl`
**FRs:** FR-v3.0-106, FR-v3.0-107

### Story 105.1: JOB Statement Extended Parameters

As a **JCL developer**,
I want **all common JOB parameters parsed**,
So that **production JOB cards are fully supported**.

**Acceptance Criteria:**

**Given** `//MYJOB JOB (ACCT),'NAME',CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),TIME=(5,0),TYPRUN=SCAN`
**When** parsed
**Then** JobParams has time=Some((5,0)), msglevel=Some((1,1)), typrun=Some(Scan)

**Given** `MEMLIMIT=2G,JOBRC=MAXRC,SCHENV=MYENV`
**When** parsed
**Then** corresponding JobParams fields are populated

**Complexity:** M

### Story 105.2: Full COND Parameter Parsing

As a **JCL developer**,
I want **COND parameters fully parsed including step.procstep qualification**,
So that **conditional execution works with procedure steps**.

**Acceptance Criteria:**

**Given** `COND=(4,LT,STEP1)`
**When** parsed
**Then** Condition with code=4, operator=Lt, step=Some("STEP1")

**Given** `COND=((4,LT,STEP1),(0,EQ,STEP2.SORT01))`
**When** parsed
**Then** two Condition entries, second with step=Some("STEP2") and procstep=Some("SORT01")

**Given** `COND=EVEN` or `COND=ONLY`
**When** parsed
**Then** special execution mode (EVEN: execute even if previous step abended, ONLY: execute only if previous step abended)

**Complexity:** M

---

## Epic 106: INCLUDE Statement and DD Concatenation

**Goal:** Support INCLUDE for JCL fragment inclusion and proper DD concatenation execution.

**Crate:** `open-mainframe-jcl`
**FRs:** FR-v3.0-103, FR-v3.0-108

### Story 106.1: INCLUDE Statement

As a **JCL developer**,
I want **INCLUDE members resolved from the procedure library**,
So that **shared DD groups can be reused across jobs**.

**Acceptance Criteria:**

**Given** `// INCLUDE MEMBER=STDDD`
**When** the INCLUDE is resolved
**Then** the JCL text from STDDD member is inserted at that point

**Complexity:** M

### Story 106.2: DD Concatenation Execution

As a **JCL developer**,
I want **concatenated DDs to present all datasets as a single logical file**,
So that **programs read from multiple input datasets seamlessly**.

**Acceptance Criteria:**

**Given** JCL:
```
//INPUT  DD DSN=FIRST.DATA,DISP=SHR
//       DD DSN=SECOND.DATA,DISP=SHR
//       DD DSN=THIRD.DATA,DISP=SHR
```
**When** the program reads from INPUT
**Then** it reads FIRST.DATA, then SECOND.DATA, then THIRD.DATA in sequence

**Complexity:** L

---

## Epic 107: GDG Support and OUTPUT Statement

**Goal:** Support Generation Data Groups and the OUTPUT JCL statement.

**Crate:** `open-mainframe-jcl`
**FRs:** FR-v3.0-104, FR-v3.0-109

### Story 107.1: GDG Relative Reference Parsing

As a **JCL developer**,
I want **GDG relative references like MY.GDG(+1) parsed**,
So that **rolling dataset patterns work correctly**.

**Acceptance Criteria:**

**Given** `DSN=MY.GDG(+1),DISP=(NEW,CATLG)`
**When** parsed
**Then** DatasetDef has dsn="MY.GDG" and gdg_generation=Some(+1)

**Given** `DSN=MY.GDG(0),DISP=SHR`
**When** parsed
**Then** gdg_generation=Some(0) (current generation)

**Complexity:** S

### Story 107.2: GDG Resolution at Execution Time

As a **JCL developer**,
I want **GDG relative references resolved to absolute dataset names**,
So that **the correct generation is read or created**.

**Acceptance Criteria:**

**Given** MY.GDG base with existing generations G0001V00 through G0005V00
**When** MY.GDG(0) is resolved
**Then** it maps to MY.GDG.G0005V00

**When** MY.GDG(+1) is resolved
**Then** it maps to MY.GDG.G0006V00 (to be created)

**Complexity:** M

### Story 107.3: OUTPUT JCL Statement

As a **JCL developer**,
I want **OUTPUT statements parsed for SYSOUT control**,
So that **print output routing and attributes are configurable**.

**Acceptance Criteria:**

**Given** `//OUT1 OUTPUT CLASS=A,DEST=REMOTE1,COPIES=3,FORMS=STD`
**When** parsed
**Then** an OutputStatement with class, dest, copies, forms is produced

**Given** a DD with `OUTPUT=*.OUT1`
**When** resolved
**Then** the OUTPUT statement attributes apply to that DD's sysout

**Complexity:** M

---

## Epic 108: Utility Program Registry

**Goal:** Create an extensible utility program registry with built-in IBM utility support.

**Crate:** `open-mainframe-jcl`
**FRs:** FR-v3.0-112

### Story 108.1: UtilityProgram Trait and Registry

As a **system integrator**,
I want **a UtilityProgram trait and registry for built-in programs**,
So that **common IBM utilities execute without external binaries**.

**Acceptance Criteria:**

**Given** EXEC PGM=IEBGENER
**When** the program is in the utility registry
**Then** the built-in IEBGENER handler executes instead of searching for an external binary

**Given** EXEC PGM=CUSTOMPGM
**When** the program is NOT in the utility registry
**Then** the executor falls back to external binary search (existing behavior)

**Complexity:** M

### Story 108.2: IEBGENER Utility (Dataset Copy)

As a **JCL developer**,
I want **IEBGENER to copy SYSUT1 to SYSUT2**,
So that **simple dataset copy operations work in JCL**.

**Acceptance Criteria:**

**Given** JCL with EXEC PGM=IEBGENER, SYSUT1 DD (input), SYSUT2 DD (output)
**When** executed
**Then** the input dataset is copied to the output dataset

**Complexity:** S

### Story 108.3: IDCAMS Utility (VSAM)

As a **JCL developer**,
I want **basic IDCAMS commands (DEFINE CLUSTER, DELETE, REPRO) supported**,
So that **VSAM dataset management works from JCL**.

**Acceptance Criteria:**

**Given** SYSIN with `DEFINE CLUSTER (NAME(MY.KSDS) ...)`
**When** IDCAMS executes
**Then** a VSAM cluster directory structure is created

**Complexity:** L

---

## Epic 109: Error Reporting and Test Coverage

**Goal:** Improve error reporting with source locations and add comprehensive test coverage.

**Crate:** `open-mainframe-jcl`
**FRs:** FR-v3.0-111, FR-v3.0-114

### Story 109.1: Span-Based Error Reporting

As a **JCL developer**,
I want **parse errors to include line numbers and source context**,
So that **I can quickly find and fix JCL syntax errors**.

**Acceptance Criteria:**

**Given** a parse error on line 5
**When** the error is displayed
**Then** the output includes line 5 content with a caret or highlight at the error position

**Complexity:** M

### Story 109.2: Parser Test Suite Expansion

As a **developer**,
I want **comprehensive parser tests covering all statement types and parameter combinations**,
So that **regressions are caught immediately**.

**Acceptance Criteria:**

**Given** the test suite
**When** run
**Then** it covers: multi-step jobs, continuation lines, all DISP combinations, DCB variants, SPACE with all units, inline data with DLM=, SYSOUT with writer/form, DUMMY DDs, comment handling, null statements, error cases

**Complexity:** M

### Story 109.3: Executor Test Suite

As a **developer**,
I want **executor tests covering step execution, condition checking, and dataset passing**,
So that **runtime behavior is verified**.

**Acceptance Criteria:**

**Given** the test suite
**When** run
**Then** it covers: IEFBR14 execution, SORT execution, COND bypass logic, DISP=PASS dataset passing between steps, dataset creation (NEW), DD concatenation

**Complexity:** M
