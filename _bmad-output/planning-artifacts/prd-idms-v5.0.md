---
version: 'v5.0'
planningGroup: 'PG-27'
technology: 'IDMS (CA IDMS/DB/DC)'
date: '2026-02-21'
status: 'complete'
totalFRs: 12
totalNFRs: 3
---

# PRD: IDMS (CA IDMS/DB/DC)

## 1. Overview

CA IDMS (Integrated Database Management System) is a CODASYL-compliant network database and transaction processing system for z/OS. It provides a network (graph) data model with navigational DML, online TP facilities (IDMS-DC), a 4GL (ADS/Online), and SQL access. IDMS remains in production at thousands of sites worldwide.

## 2. Functional Requirements

### FR-IDMS-001: CODASYL Network Data Model
The system SHALL implement records, sets (owner-member relationships), areas, and db-keys per CODASYL standards. Records belong to areas; sets define directed 1:N relationships between owner and member records with NEXT/PRIOR/OWNER pointers.

### FR-IDMS-002: Schema & Subschema DDL
The system SHALL parse IDMS Schema DDL (RECORD, SET, AREA, ELEMENT definitions) and Subschema DDL (restricted views of the schema). Elements within records have types: binary, character, packed decimal, float.

### FR-IDMS-003: Navigational DML Engine
The system SHALL implement ~16 DML verbs: OBTAIN (7 variants), FIND, GET, STORE, MODIFY, ERASE, CONNECT, DISCONNECT, READY, FINISH, BIND, ACCEPT, COMMIT, ROLLBACK. OBTAIN combines FIND + GET.

### FR-IDMS-004: Currency Indicator System
The system SHALL maintain 4 currency types: run-unit current, current of record type, current of set type, current of area. Each DML verb updates currencies per CODASYL rules.

### FR-IDMS-005: COBOL DML Precompiler
The system SHALL transform embedded IDMS DML in COBOL source to CALL 'IDMS' statements, generating IDMS communication block (SUBSCHEMA-CTRL) and status checking code.

### FR-IDMS-006: DMCL & Physical Storage
The system SHALL implement area-to-file mapping via DMCL (Device-Media Control Language), buffer pool management, page-level storage, and journal/log writing.

### FR-IDMS-007: CALC and VIA Access Strategies
The system SHALL support CALC (hash-based direct access by key) and VIA (clustered storage near set owner) access strategies per record definition.

### FR-IDMS-008: IDMS-DC Transaction Processing
The system SHALL implement IDMS-DC TP facilities: MAP/BIND MAP/MAP IN/MAP OUT for 3270 screens, LINK/XCTL for program transfer, task dispatching, scratch/queue storage.

### FR-IDMS-009: ADS/Online 4GL (Basic)
The system SHALL support basic ADS dialog definitions: premap/response processes, dialog flow, and compilation to online dialogs.

### FR-IDMS-010: SQL Option
The system SHALL support SQL DML (SELECT, INSERT, UPDATE, DELETE) against CODASYL-defined records, enabling relational access to network databases.

### FR-IDMS-011: Recovery & Operations
The system SHALL support journal-based recovery (write-ahead logging), warmstart/coldstart, and DCMT operator commands for system management.

### FR-IDMS-012: Lock Management
The system SHALL implement record-level and area-level locking for concurrent access with deadlock detection.

## 3. Non-Functional Requirements

### NFR-IDMS-001: Test Coverage ≥90%
### NFR-IDMS-002: Diagnostic Quality — miette errors with line numbers
### NFR-IDMS-003: Navigational Performance — 50,000+ OBTAIN operations per second in-memory
