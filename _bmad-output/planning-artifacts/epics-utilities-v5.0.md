---
version: 'v5.0'
planningGroup: 'PG-5'
technology: 'Utilities & DFSORT'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-utilities-v5.0.md'
  - 'architecture-utilities-v5.0.md'
totalEpics: 11
totalStories: 55
frCoverage: '14/14 (100%)'
nfrCoverage: '6/6 (100%)'
---

# Epics & Stories: Utilities & DFSORT

## Epic Overview

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| UTIL-110 | Utility Framework | M | 4 | A |
| UTIL-100 | IEBCOPY — PDS Copy, Compress, Merge | L | 6 | A |
| UTIL-101 | IEBGENER Enhancements | M | 4 | A |
| UTIL-104 | DFSORT Enhancements (OUTFIL, IFTHEN, ICETOOL) | L | 7 | A |
| UTIL-103 | IEBUPDTE — PDS Update Utility | M | 4 | B |
| UTIL-109 | TSO/REXX/USS Batch Processors | M | 6 | B |
| UTIL-102 | IEBCOMPR — Dataset Compare | S | 4 | B |
| UTIL-105 | IEBPTPCH — Print/Punch | S | 4 | C |
| UTIL-106 | IEBDG — Test Data Generator | S | 4 | C |
| UTIL-107 | IEH System Utilities | M | 6 | C |
| UTIL-108 | AMASPZAP — Superzap | S | 6 | C |

---

## UTIL-110: Utility Framework

**User Value:** JCL job steps that reference utility programs are dispatched correctly with DD table access, control statement parsing, and standardized condition codes.

### UTIL-110.1: UtilityProgram Trait and Registry

**As a** JCL developer, **I want** utility programs recognized by `EXEC PGM=` in JCL, **so that** standard utility steps execute.

**Acceptance Criteria:**
- Given the `UtilityProgram` trait and `UtilityRegistry`, when `PGM=IEBCOPY` is in JCL, then the registry dispatches to the IEBCOPY implementation
- Given a registered utility, when `execute()` returns `UtilityResult`, then the condition code is propagated to the JCL step CC
- Given `SYSIN DD *` with control statements, when a utility reads SYSIN, then the statements are available as `Vec<String>`

### UTIL-110.2: Common DD Handling and Message Formatting

**As a** utility program, **I want** standard DD lookup and message output, **so that** all utilities consistently access datasets and write messages.

**Acceptance Criteria:**
- Given SYSUT1 DD, when `context.open_input("SYSUT1")` is called, then a reader for the allocated dataset is returned
- Given SYSPRINT DD, when `context.write_message(msg)` is called, then the message is written to SYSPRINT
- Given a message ID "IEB1013I", when formatted, then it follows IBM convention: `IEB1013I COPY OPERATION SUCCESSFUL`

### UTIL-110.3: Condition Code Protocol and IEFBR14

**As a** JCL developer, **I want** utilities to return standard condition codes, **so that** COND= and IF/THEN logic works correctly.

**Acceptance Criteria:**
- Given CC=0, when utility succeeds, then COND= processing treats it as success
- Given CC=8, when utility has errors, then COND= processing can skip subsequent steps
- Given `PGM=IEFBR14`, when executed, then CC=0 is returned with no action (already implemented — verify integration)

### UTIL-110.4: Framework Integration Tests

**As a** developer, **I want** tests proving the framework dispatches utilities correctly.

**Acceptance Criteria:**
- Given a JCL job with 3 utility steps, when executed, then all 3 utilities run in sequence with correct CCs
- Given an unregistered program name, when dispatched, then ABEND S806 occurs (program not found)

---

## UTIL-100: IEBCOPY — PDS Copy, Compress, Merge

**User Value:** PDS libraries can be copied, compressed, and merged — the most common PDS maintenance operation.

### UTIL-100.1: Basic Member Copy (SELECT/EXCLUDE)

**As a** systems programmer, **I want** to copy selected members between PDS datasets.

**Acceptance Criteria:**
- Given `COPY OUTDD=OUT,INDD=IN` with `SELECT MEMBER=(A,B,C)`, when executed, then members A, B, C are copied from IN to OUT
- Given `EXCLUDE MEMBER=(X,Y)`, when executed, then all members except X and Y are copied
- Given no SELECT/EXCLUDE, when executed, then all members are copied

### UTIL-100.2: Copy with REPLACE Option

**As a** systems programmer, **I want** REPLACE to overwrite existing members in the output PDS.

**Acceptance Criteria:**
- Given member A exists in both input and output, when COPY with REPLACE is executed, then the output member is overwritten
- Given REPLACE not specified and member exists in output, when COPY runs, then the member is skipped with CC=4 warning

### UTIL-100.3: PDS Compress In-Place

**As a** systems programmer, **I want** to compress a PDS to reclaim embedded free space.

**Acceptance Criteria:**
- Given a PDS with deleted members (embedded free space), when COPY with same INDD/OUTDD is executed, then the PDS is reorganized with members packed contiguously
- Given the compress operation, when complete, then the directory is updated and CC=0

### UTIL-100.4: Merge Multiple Input PDS

**As a** systems programmer, **I want** to merge multiple PDS datasets into one output.

**Acceptance Criteria:**
- Given `COPY OUTDD=OUT,INDD=(IN1,IN2,IN3)`, when executed, then members from all three inputs are merged into OUT
- Given duplicate member names across inputs, when REPLACE is specified, then the last input's version wins

### UTIL-100.5: Load/Unload (Flat File Transport)

**As a** systems programmer, **I want** to unload a PDS to sequential and reload from sequential.

**Acceptance Criteria:**
- Given COPY from PDS to sequential (OUTDD DSORG=PS), when executed, then the PDS is unloaded into transport format
- Given COPY from sequential to PDS, when the input is in unload format, then members are restored

### UTIL-100.6: IEBCOPY Integration Tests

**As a** developer, **I want** comprehensive IEBCOPY tests.

**Acceptance Criteria:**
- Given a PDS with 50 members, when COPY SELECT=(5 members) runs, then exactly 5 are copied and CC=0
- Given compress of a PDS with 10 deleted members, then space is reclaimed
- Given `cargo test` utility IEBCOPY tests, then all pass

---

## UTIL-101: IEBGENER Enhancements

**User Value:** IEBGENER supports control statement reformatting beyond simple copy — enabling field rearrangement and member creation.

### UTIL-101.1: GENERATE Statement

**As a** developer, **I want** GENERATE MAXFLDS/MAXLITS to control reformatting capacity.

**Acceptance Criteria:**
- Given `GENERATE MAXFLDS=10,MAXLITS=50`, when specified, then up to 10 field specifications and 50 literal characters are supported

### UTIL-101.2: RECORD Statement (Field Rearrangement)

**As a** developer, **I want** RECORD FIELD= to rearrange fields during copy.

**Acceptance Criteria:**
- Given `RECORD FIELD=(20,1,,1),FIELD=(10,30,,21)`, when executed, then the first 20 bytes of input map to position 1 of output, bytes 30-39 of input map to position 21
- Given FIELD with literal: `FIELD=(5,'CONST',,41)`, when executed, then 'CONST' is placed at output position 41

### UTIL-101.3: MEMBER Statement (PDS Member Creation)

**As a** developer, **I want** MEMBER NAME= to create PDS members from sequential input.

**Acceptance Criteria:**
- Given `MEMBER NAME=MEMA`, when executed with sequential SYSUT1, then the sequential data becomes member MEMA in the output PDS

### UTIL-101.4: IEBGENER Enhancement Tests

**Acceptance Criteria:**
- Given reformatting with 3 FIELD specifications, when executed, then output records match expected layout
- Given member creation from sequential, then the PDS member exists with correct content

---

## UTIL-104: DFSORT Enhancements (OUTFIL, IFTHEN, ICETOOL)

**User Value:** DFSORT can split output to multiple files, conditionally modify records, and ICETOOL provides multi-operation batch processing.

### UTIL-104.1: OUTFIL Statement — Multiple Outputs

**As a** developer, **I want** OUTFIL to split sorted data to multiple output files.

**Acceptance Criteria:**
- Given `OUTFIL FNAMES=OUT1,INCLUDE=(5,2,CH,EQ,C'NY')`, when executed, then only records with NY at position 5 go to OUT1
- Given `OUTFIL FNAMES=OUT2,SAVE`, when executed, then records not selected by any other OUTFIL go to OUT2

### UTIL-104.2: OUTFIL OUTREC — Output Record Reformatting

**As a** developer, **I want** OUTFIL OUTREC to reformat records on output.

**Acceptance Criteria:**
- Given `OUTFIL OUTREC=(1,20,C' - ',25,30)`, when executed, then output records are reformatted with fields 1-20, literal ' - ', and fields 25-54

### UTIL-104.3: IFTHEN — Conditional Processing

**As a** developer, **I want** IFTHEN=(WHEN=condition,OVERLAY=fields) for conditional modification.

**Acceptance Criteria:**
- Given `IFTHEN=(WHEN=(5,2,CH,EQ,C'NY'),OVERLAY=(80:C'EAST'))`, when a record has NY at position 5, then 'EAST' is placed at position 80
- Given `IFTHEN=(WHEN=INIT,OVERLAY=(80:C'NONE'))`, when processing starts, then all records get 'NONE' at position 80 before other IFTHEN conditions

### UTIL-104.4: FINDREP — Find and Replace

**As a** developer, **I want** FINDREP for string find-and-replace in records.

**Acceptance Criteria:**
- Given `INREC FINDREP=(IN=C'OLD',OUT=C'NEW')`, when executed, then all occurrences of 'OLD' are replaced with 'NEW' in input records

### UTIL-104.5: DFSORT Symbols and Built-in Functions

**As a** developer, **I want** DFSORT symbols (DATE1-4, SEQNUM, etc.) and functions (COUNT, MIN, MAX, AVG, TOT).

**Acceptance Criteria:**
- Given `OUTREC=(1,20,SEQNUM,8,ZD)`, when executed, then a sequence number is appended to each record
- Given `OUTREC=(1,20,DATE1=(MD4/))`, when executed, then current date in MM/DD/YYYY format is appended

### UTIL-104.6: ICETOOL Operations

**As a** developer, **I want** ICETOOL with SORT, COPY, COUNT, STATS, DISPLAY, OCCUR, SELECT, UNIQUE operations.

**Acceptance Criteria:**
- Given `SORT FROM(IN1) TO(OUT1) USING(CTL1)`, when executed, then IN1 is sorted per CTL1 control statements
- Given `COUNT FROM(IN1) EMPTY`, when the dataset has 0 records, then CC=0; when non-empty, CC=12
- Given `STATS FROM(IN1) ON(5,8,ZD)`, when executed, then MIN, MAX, AVG, TOTAL, COUNT for field at position 5 are reported
- Given `OCCUR FROM(IN1) ON(1,10,CH) LIST(OUT1)`, when executed, then unique value occurrences are written to OUT1

### UTIL-104.7: DFSORT Enhancement Tests

**Acceptance Criteria:**
- Given OUTFIL with 3 output streams, when sort runs, then records are correctly distributed
- Given IFTHEN with 3 conditions, when processed, then records are conditionally modified
- Given ICETOOL with 4 operations, when executed, then all operations complete successfully

---

## UTIL-103: IEBUPDTE — PDS Update Utility

### UTIL-103.1: ADD Operation
**As a** developer, **I want** `./ ADD NAME=member` to create a new PDS member.

### UTIL-103.2: REPL Operation
**As a** developer, **I want** `./ REPL NAME=member` to replace an existing member.

### UTIL-103.3: CHANGE Operation
**As a** developer, **I want** `./ CHANGE NAME=member` to modify specific records in a member using sequence numbers.

### UTIL-103.4: IEBUPDTE Tests

---

## UTIL-109: TSO/REXX/USS Batch Processors

### UTIL-109.1: IKJEFT01 — TSO Command Execution in Batch
**As a** developer, **I want** `PGM=IKJEFT01` with SYSTSIN containing TSO commands.

**Acceptance Criteria:**
- Given SYSTSIN with `ALLOCATE DA('MY.DATA') FI(INFILE) SHR`, when executed, then TSO ALLOCATE runs and the allocation is visible to the step

### UTIL-109.2: IKJEFT1A/IKJEFT1B Variants
**As a** developer, **I want** IKJEFT1A (authorized) and IKJEFT1B (non-authorized TSO in batch).

### UTIL-109.3: IRXJCL — REXX Execution in Batch
**As a** developer, **I want** `PGM=IRXJCL` with SYSTSIN containing REXX exec name.

**Acceptance Criteria:**
- Given SYSTSIN with `%MYREXX`, when executed, then the REXX exec MYREXX runs and output goes to SYSTSPRT

### UTIL-109.4: BPXBATCH — USS Command Execution in Batch
**As a** developer, **I want** `PGM=BPXBATCH` to run USS commands from JCL.

**Acceptance Criteria:**
- Given STDPARM with `/bin/ls /home/user`, when executed, then the listing goes to STDOUT DD

### UTIL-109.5: Batch Processor DD Conventions
**As a** developer, **I want** standard DD names (SYSTSIN/SYSTSPRT/STDIN/STDOUT/STDERR) used correctly.

### UTIL-109.6: Batch Processor Tests

---

## UTIL-102: IEBCOMPR — Dataset Compare

### UTIL-102.1: Sequential Dataset Comparison
### UTIL-102.2: PDS Member Comparison
### UTIL-102.3: Mismatch Reporting
### UTIL-102.4: IEBCOMPR Tests

---

## UTIL-105: IEBPTPCH — Print/Punch

### UTIL-105.1: PRINT Operation
### UTIL-105.2: PUNCH Operation
### UTIL-105.3: Field Selection and Formatting
### UTIL-105.4: IEBPTPCH Tests

---

## UTIL-106: IEBDG — Test Data Generator

### UTIL-106.1: DSD/FD/CREATE Statements
### UTIL-106.2: Pattern Generation (WAVE, ROLL, RANDOM)
### UTIL-106.3: Field Types and Lengths
### UTIL-106.4: IEBDG Tests

---

## UTIL-107: IEH System Utilities

### UTIL-107.1: IEHPROGM — Scratch/Rename/Catalog
### UTIL-107.2: IEHLIST — Catalog and VTOC Listing
### UTIL-107.3: IEHMOVE — Dataset Move
### UTIL-107.4: IEHPROGM Tests
### UTIL-107.5: IEHLIST Tests
### UTIL-107.6: IEHMOVE Tests

---

## UTIL-108: AMASPZAP — Superzap

### UTIL-108.1: VERIFY Control Statement
### UTIL-108.2: REP Control Statement (Replace)
### UTIL-108.3: DUMP Control Statement
### UTIL-108.4: NAME/CCHHR Addressing Modes
### UTIL-108.5: Safety Checks (VERIFY before REP)
### UTIL-108.6: AMASPZAP Tests

---

## FR/NFR Coverage Matrix

| FR | Stories |
|----|---------|
| FR-UTIL-001 | UTIL-100.1, UTIL-100.2 |
| FR-UTIL-002 | UTIL-100.3 |
| FR-UTIL-003 | UTIL-100.4 |
| FR-UTIL-004 | UTIL-101.1, UTIL-101.2, UTIL-101.3 |
| FR-UTIL-005 | UTIL-104.1, UTIL-104.2 |
| FR-UTIL-006 | UTIL-104.3 |
| FR-UTIL-007 | UTIL-104.6 |
| FR-UTIL-008 | UTIL-103.1, UTIL-103.2, UTIL-103.3 |
| FR-UTIL-009 | UTIL-102.1, UTIL-102.2, UTIL-102.3 |
| FR-UTIL-010 | UTIL-109.1 |
| FR-UTIL-011 | UTIL-109.3 |
| FR-UTIL-012 | UTIL-109.4 |
| FR-UTIL-013 | UTIL-110.3 |
| FR-UTIL-014 | UTIL-110.2 |

| NFR | Stories |
|-----|---------|
| NFR-UTIL-001 | UTIL-100.6 |
| NFR-UTIL-002 | UTIL-104.7 |
| NFR-UTIL-003 | UTIL-110.3 |
| NFR-UTIL-004 | UTIL-110.2 |
| NFR-UTIL-005 | UTIL-110.1 |
| NFR-UTIL-006 | UTIL-110.1 |

**Coverage: 14/14 FRs (100%), 6/6 NFRs (100%)**
