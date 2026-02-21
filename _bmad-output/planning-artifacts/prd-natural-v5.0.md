---
version: 'v5.0'
planningGroup: 'PG-29'
technology: 'Natural (Software AG)'
date: '2026-02-21'
status: 'complete'
totalFRs: 12
totalNFRs: 3
---

# PRD: Natural

## 1. Overview

Natural is Software AG's 4GL/development environment for z/OS, tightly integrated with ADABAS. It provides a comprehensive programming language with 130+ statements, 11 data types, report generation, 3270 terminal interaction, and database access (ADABAS and SQL). Natural supports multiple object types: Programs, Subprograms, Subroutines, Maps, Copycodes, Data Areas, and Classes.

## 2. Functional Requirements

### FR-NAT-001: Source Parsing (130+ statements)
### FR-NAT-002: Data Model (11 types: A,B,C,D,F,I,L,N,P,T,U, DEFINE DATA, arrays, dynamic vars)
### FR-NAT-003: Interpreter Core (control flow, PERFORM, CALLNAT, FETCH, STACK)
### FR-NAT-004: Data Manipulation (COMPUTE, MOVE, COMPRESS, SEPARATE, EXAMINE, SORT)
### FR-NAT-005: ADABAS DML (DDM, READ, FIND, HISTOGRAM, GET, STORE, UPDATE, DELETE, ET/BT)
### FR-NAT-006: SQL Database Access (SELECT, INSERT, UPDATE, DELETE, COMMIT, ROLLBACK)
### FR-NAT-007: Output & Reporting (DISPLAY, WRITE, PRINT, control breaks, page formatting)
### FR-NAT-008: Interactive I/O & Maps (INPUT, REINPUT, map objects, PF keys)
### FR-NAT-009: System Variables & Functions (70+ variables, 25+ functions)
### FR-NAT-010: Error Handling & Work Files (ON ERROR, work file I/O)
### FR-NAT-011: System Commands & Environment (LOGON, CATALOG, STOW, libraries)
### FR-NAT-012: Natural Security & RPC (library protection, EntireX RPC)

## 3. Non-Functional Requirements

### NFR-NAT-001: Test Coverage ≥90%
### NFR-NAT-002: Diagnostic Quality with miette
### NFR-NAT-003: Performance — 50,000+ statements/sec
