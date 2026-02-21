---
version: 'v5.0'
planningGroup: 'PG-28'
technology: 'ADABAS (Adaptable Database System)'
date: '2026-02-21'
status: 'complete'
totalFRs: 12
totalNFRs: 3
---

# PRD: ADABAS

## 1. Overview

ADABAS (Adaptable Database System) is Software AG's high-performance DBMS for z/OS featuring an inverted-list storage architecture. It provides built-in data compression, multi-threaded nucleus operation, and serves as the database engine for the Natural 4GL. ADABAS is used in banking, insurance, and government worldwide.

## 2. Functional Requirements

### FR-ADA-001: Inverted-List Storage Engine
The system SHALL implement ADABAS's three-component storage: Associator (indexes, address converter), Data Storage (compressed records), and Work area (temporary). Records are identified by Internal Sequence Number (ISN).

### FR-ADA-002: Field Definition Table (FDT)
The system SHALL support FDT definitions with 7 data types: A (alpha), B (binary), F (floating), G (EBCDIC float), N (unpacked numeric), P (packed decimal), U (unsigned numeric), W (wide character). Field options: MU (multiple-value), PE (periodic group), FI (fixed storage), NU (null-suppression), DE (descriptor), UQ (unique descriptor), LB/LA (LOB).

### FR-ADA-003: Descriptor Engine
The system SHALL support 6 descriptor types: standard, super-descriptor (composite), sub-descriptor (substring), phonetic, hyper-descriptor (user-defined), and collation descriptor. Descriptors enable inverted-list indexing for search optimization.

### FR-ADA-004: Direct Call Interface (ACB)
The system SHALL implement the 80-byte ADABAS Control Block (ACB) interface with command code, file number, ISN, format buffer (FB), record buffer (RB), search buffer (SB), value buffer (VB), and ISN buffer (IB) parameters.

### FR-ADA-005: Search Commands
The system SHALL implement S1 (find with criteria), S2 (find sorted), S4 (find coupled files), S8 (process ISN list), S9 (sort ISN list).

### FR-ADA-006: Read Commands
The system SHALL implement L1/L4 (read by ISN), L2/L5 (read physical sequential), L3/L6 (read logical sequential by descriptor), L9 (histogram — descriptor value distribution), LF (read FDT).

### FR-ADA-007: Modification Commands
The system SHALL implement N1/N2 (store record), A1 (update record), E1 (delete record) with automatic index maintenance and data compression.

### FR-ADA-008: Transaction Management
The system SHALL implement OP (open session), CL (close session), ET (end transaction/commit), BT (backout transaction/rollback), with hold (HI/RI) and release (RC) operations.

### FR-ADA-009: ADABAS Utilities
The system SHALL provide ADACMP (compress/decompress), ADALOD (load), ADAULD (unload), ADAORD (reorder), ADASAV (save/restore), ADAFRM (format), and ADADBS (status display).

### FR-ADA-010: Nucleus
The system SHALL implement a multi-threaded ADABAS nucleus with buffer pool management, protection log (PLOG), command log (CLOG), and automatic restart.

### FR-ADA-011: DDM & Natural Integration
The system SHALL support Data Definition Module (DDM) definitions that provide logical views of ADABAS files for Natural 4GL access.

### FR-ADA-012: Data Compression
The system SHALL implement ADABAS null-suppression compression achieving typical 40-60% storage reduction.

## 3. Non-Functional Requirements

### NFR-ADA-001: Test Coverage ≥90%
### NFR-ADA-002: Compression Ratio — achieve ≥40% compression on typical business data
### NFR-ADA-003: Performance — 50,000+ reads/sec in-memory
