# JCL (Job Control Language) Gap Closure — Product Requirements

## Overview

The `open-mainframe-jcl` crate provides JCL parsing and job execution for the OpenMainframe platform. JCL is the language used on IBM z/OS to describe batch jobs — specifying which programs to run, what datasets to use, and how to handle output. The crate currently implements basic JOB/EXEC/DD statement parsing and a simple executor that can run compiled programs and DFSORT.

## Current State Assessment

- **Lines of code:** 2,778
- **Test count:** 21 (all passing)
- **Maturity:** Basic
- **Files:** 8 Rust source files (lib.rs, error.rs, ast/mod.rs, lexer/mod.rs, lexer/scanner.rs, lexer/token.rs, parser/mod.rs, executor/mod.rs)

### What Works

- JOB statement: name, accounting, programmer, CLASS, MSGCLASS, NOTIFY, REGION
- EXEC statement: PGM=, PROC= (parse only, no execution), PARM=, REGION=, COND= (partial)
- DD statement: DSN=, DISP= (full 3-part), UNIT=, VOL=SER=, SPACE=, DCB= (RECFM/LRECL/BLKSIZE/DSORG), AMP= (VSAM params), SYSOUT=, DUMMY, inline data (DD *)
- Continuation lines (comma-continuation)
- Comment lines (//* skipped)
- Null statement (//)
- Job execution with IEFBR14 and SORT/DFSORT/ICEMAN utilities
- COND parameter evaluation for conditional step execution
- Dataset path resolution including PDS member names

### What Does NOT Work

- Procedures (PROC/PEND) — tokens defined but parser skips them
- IF/THEN/ELSE/ENDIF — parser skips them
- SET statement — parser skips
- INCLUDE statement — parser skips
- JCLLIB statement — parser skips
- OUTPUT statement — token defined but not parsed
- CNTL/ENDCNTL — not implemented
- XMIT — not implemented
- COMMAND — not implemented
- Symbolic parameter substitution (&PARAM)
- Cataloged procedure library resolution
- Most DD parameters (LABEL, DATACLAS, MGMTCLAS, STORCLAS, EXPDT, RETPD, etc.)
- DD concatenation execution
- GDG (Generation Data Group) support
- MSGLEVEL, TIME on JOB
- JES2/JES3 control statements
- Multi-step dataset passing (DISP=PASS partially wired)
- Proper error messages with line/column numbers

## Functional Requirements

### FR-v3.0-100: Procedure Support (PROC/PEND)
Parse and expand in-stream procedures (PROC...PEND) and cataloged procedures (EXEC PROC=name). Resolve symbolic parameters with defaults. Support procedure nesting up to 15 levels.
- **Priority:** CRITICAL
- **Gap Type:** Missing feature
- **IBM Reference:** z/OS MVS JCL Reference SA23-1385, Chapters 23-24. Procedures are fundamental to production JCL — virtually all production batch jobs use cataloged procs.

### FR-v3.0-101: Symbolic Parameter Substitution
Substitute symbolic parameters (&NAME, &&TEMP) in JCL text. Support SET statement for defining symbols. Handle double periods for dataset name qualification (e.g., &HLQ..DATA).
- **Priority:** CRITICAL
- **Gap Type:** Missing feature
- **IBM Reference:** SA23-1385, Chapter 25 (SET). Symbolic parameters are how JCL is parameterized for different environments (dev/test/prod).

### FR-v3.0-102: IF/THEN/ELSE/ENDIF Conditional Processing
Parse and evaluate IF/THEN/ELSE/ENDIF constructs. Support RC (return code), ABENDCC, ABEND, and RUN keywords. Support relational operators (<, <=, >, >=, =, <>) and logical operators (AND, OR, NOT). Support nesting up to 15 levels.
- **Priority:** CRITICAL
- **Gap Type:** Missing feature
- **IBM Reference:** SA23-1385, Chapter 17. Modern JCL uses IF/THEN/ELSE extensively instead of the older COND parameter.

### FR-v3.0-103: INCLUDE Statement and JCLLIB
Parse INCLUDE statements to incorporate JCL fragments from procedure libraries. Parse JCLLIB ORDER statement to specify the search order for procedures and INCLUDE members.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** SA23-1385, Chapters 18-19. INCLUDE is used for shared DD groups. JCLLIB specifies where to find procedures.

### FR-v3.0-104: OUTPUT JCL Statement
Parse OUTPUT statements for SYSOUT processing control. Support parameters: CLASS, DEST, FORMS, COPIES, JESDS, OUTDISP, WRITER, etc.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** SA23-1385, Chapter 22. OUTPUT controls print routing and is common in production JCL.

### FR-v3.0-105: Extended DD Parameters
Add parsing for missing DD parameters: LABEL, DATACLAS, MGMTCLAS, STORCLAS, EXPDT, RETPD, DSNTYPE, LIKE, REFDD, FREE, HOLD, SPIN, SEGMENT, KEYLEN, KEYOFF, AVGREC, EATTR, SECMODEL, PATH/PATHDISP/PATHMODE/PATHOPTS (USS files).
- **Priority:** MAJOR
- **Gap Type:** Incomplete implementation
- **IBM Reference:** SA23-1385, Chapter 12. Production JCL commonly uses SMS classes (DATACLAS/MGMTCLAS/STORCLAS) and LABEL for tape datasets.

### FR-v3.0-106: JOB Statement Extended Parameters
Add parsing for missing JOB parameters: TIME, MSGLEVEL, TYPRUN, RESTART, BYTES, LINES, PAGES, ADDRSPC, CCSID, JESLOG, JOBRC, SCHENV, MEMLIMIT.
- **Priority:** MAJOR
- **Gap Type:** Incomplete implementation
- **IBM Reference:** SA23-1385, Chapter 20. TIME and MSGLEVEL are used in nearly every production JOB card.

### FR-v3.0-107: COND Parameter Full Parsing
Complete COND parameter parsing (currently skipped with comment). Parse COND=(code,op,stepname.procstepname) with full step qualification. Support COND=EVEN and COND=ONLY.
- **Priority:** MAJOR
- **Gap Type:** Incomplete implementation
- **IBM Reference:** SA23-1385, Chapters 13-16. COND is the traditional way to control step execution (though IF/THEN/ELSE is preferred in modern JCL).

### FR-v3.0-108: DD Concatenation Execution
Properly execute concatenated datasets (multiple DDs without names following a named DD). Currently AST supports Concatenation variant but executor only uses the first dataset.
- **Priority:** MAJOR
- **Gap Type:** Incomplete implementation
- **IBM Reference:** DD concatenation is a fundamental JCL feature for combining multiple input datasets.

### FR-v3.0-109: GDG (Generation Data Group) Support
Parse GDG relative references (e.g., DSN=MY.GDG(+1), DSN=MY.GDG(0), DSN=MY.GDG(-1)). Resolve relative generation numbers to absolute dataset names.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** GDGs are heavily used in production batch for maintaining rolling datasets (daily/weekly files).

### FR-v3.0-110: Procedure Execution
Execute cataloged procedures by resolving EXEC PROC=name against a procedure library (PDS). Support parameter overrides on EXEC (e.g., EXEC MYPROC,REGION=4M). Support DD overrides (stepname.ddname).
- **Priority:** CRITICAL
- **Gap Type:** Missing feature — executor returns error for procedures
- **IBM Reference:** SA23-1385, Ch. 13-16. Procedure execution is the primary mechanism for running production batch.

### FR-v3.0-111: Error Reporting with Source Locations
Enhance JclError to include line numbers, column ranges, and source context for all parse errors. Currently errors have plain string messages without position information.
- **Priority:** MINOR
- **Gap Type:** Incomplete implementation
- **IBM Reference:** N/A (quality-of-life improvement)

### FR-v3.0-112: System Utility Programs
Add built-in execution for common IBM utilities beyond IEFBR14 and SORT: IEBGENER (copy), IEBCOPY (PDS copy), IDCAMS (VSAM utility), IEHPROGM (scratch/rename), IKJEFT01 (TSO batch).
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** z/OS Utilities Reference. These utilities are used in the majority of production JCL.

### FR-v3.0-113: CNTL/ENDCNTL and XMIT Statements
Parse CNTL/ENDCNTL for program control. Parse XMIT for JES network transmission.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** SA23-1385, Chapters 11, 26. Rarely used in modern JCL but required for completeness.

### FR-v3.0-114: Comprehensive Test Coverage
Add tests for: continuation lines with various patterns, multi-step jobs with dataset passing, DISP handling (NEW/CATLG, OLD/KEEP, MOD), all DD parameter combinations, error cases (invalid syntax, missing required parameters).
- **Priority:** MAJOR
- **Gap Type:** No tests (for many existing features)
- **IBM Reference:** N/A
