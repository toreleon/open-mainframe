---
version: 'v5.0'
planningGroup: 'PG-21'
technology: 'CLIST (Command List)'
date: '2026-02-21'
status: 'complete'
totalFRs: 10
totalNFRs: 3
---

# PRD: CLIST (Command List)

## 1. Overview

CLIST (Command List) is the original TSO/E procedural scripting language on z/OS. Predating REXX, CLIST provides sequential command execution, variable manipulation, control flow, file I/O, and ISPF integration. Unlike REXX, CLIST can only execute within a TSO/E address space.

This PRD defines requirements for a CLIST interpreter within the OpenMainframe ecosystem, enabling execution of legacy CLIST scripts for TSO automation, ISPF panel driving, and batch submission.

## 2. Functional Requirements

### FR-CLIST-001: CLIST Source Parsing
The system SHALL parse CLIST source format including line-based statements, `+`/`-` continuation characters, label syntax (colon-terminated), and case handling (default uppercase, ASIS for mixed case). Maximum logical line length: 32,756 characters.

### FR-CLIST-002: Statement Recognition
The system SHALL recognize and execute all ~25 CLIST statements: PROC, SET, READ, READDVAL, WRITE, WRITENR, IF/THEN/ELSE, DO/WHILE/UNTIL/END, SELECT/WHEN/OTHERWISE/END, GOTO, EXIT, END, RETURN, ERROR, ATTN, CONTROL, OPENFILE, GETFILE, PUTFILE, CLOSFILE, DATA/ENDDATA, TERMIN, GLOBAL, NGLOBAL, SYSCALL, SYSREF, LISTDSI.

### FR-CLIST-003: Symbolic Variables
The system SHALL support symbolic variables with `&` prefix, implicit string typing, `&&` deferred substitution, nested variable resolution, and a maximum variable name length of 252 characters. All values are character strings; numeric operations are performed via `&EVAL()`.

### FR-CLIST-004: System Control Variables
The system SHALL provide ~37 read-only system control variables including &LASTCC, &MAXCC, &SYSDATE, &SYSTIME, &SYSUID, &SYSNEST, &SYSENV, &SYSISPF, &SYSLTERM, &SYSWTERM, &SYSCPU, &SYSSRV, &SYSRACF, &SYSTSOE, and others per SA32-0978.

### FR-CLIST-005: Built-in Functions
The system SHALL implement ~15 built-in functions: &EVAL (arithmetic), &DATATYPE, &LENGTH, &SUBSTR, &STR, &NRSTR, &SYSDSN, &SYSINDEX, &SYSCAPS, &SYSLC, &SYSNSUB.

### FR-CLIST-006: Terminal I/O
The system SHALL support terminal interaction via READ (single variable), READDVAL (multiple variables), WRITE (with newline), WRITENR (no newline), and TERMIN (raw terminal input).

### FR-CLIST-007: File I/O
The system SHALL support sequential file operations via OPENFILE (INPUT/OUTPUT/UPDATE), GETFILE, PUTFILE, CLOSFILE. Files must be pre-allocated via TSO ALLOCATE, and the DD name variable receives each record.

### FR-CLIST-008: Error and Attention Handling
The system SHALL support ERROR routines (triggered on non-zero return codes), ATTN routines (triggered on terminal attention/PA1), and RETURN for resuming execution after error handling.

### FR-CLIST-009: Subprocedures
The system SHALL support internal subprocedures via SYSCALL (invocation), SYSREF (pass by reference), GLOBAL/NGLOBAL (inter-CLIST variable sharing), and nested CLIST execution via the EXEC command.

### FR-CLIST-010: TSO/ISPF Integration
The system SHALL dispatch unrecognized statements as TSO commands, support ISPEXEC prefix for ISPF Dialog Manager services, support ISREDIT prefix for ISPF Edit macro services, and implement LISTDSI for dataset information retrieval.

## 3. Non-Functional Requirements

### NFR-CLIST-001: Test Coverage
All CLIST parser and interpreter functionality SHALL have unit tests achieving ≥90% code coverage. Integration tests SHALL validate end-to-end CLIST script execution.

### NFR-CLIST-002: Diagnostic Quality
Parse errors and runtime errors SHALL produce diagnostics with source line numbers, variable names, and descriptive messages using `miette` for error rendering.

### NFR-CLIST-003: Performance
CLIST execution SHALL process at least 10,000 statements per second for in-memory operations (excluding I/O wait), enabling practical execution of large CLISTs.
