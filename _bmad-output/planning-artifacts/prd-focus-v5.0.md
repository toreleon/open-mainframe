---
version: 'v5.0'
planningGroup: 'PG-30'
technology: 'FOCUS (Information Builders)'
date: '2026-02-21'
status: 'complete'
totalFRs: 12
totalNFRs: 3
---

# PRD: FOCUS

## 1. Overview

FOCUS is a non-procedural 4GL and reporting system for z/OS by Information Builders (now part of TIBCO). It provides a unified environment for reporting (TABLE), graphing (GRAPH), transaction processing (MODIFY/MAINTAIN), and procedural scripting (Dialogue Manager). FOCUS accesses 14+ data sources through a metadata abstraction layer (Master File Descriptor).

## 2. Functional Requirements

### FR-FOC-001: Language Parser (TABLE, GRAPH, MODIFY, SQL, Dialogue Manager dialects)
### FR-FOC-002: Master File Descriptor (MFD) & Access File (metadata, data types, segment hierarchy)
### FR-FOC-003: TABLE Request Engine (PRINT, aggregation, BY/ACROSS, WHERE, DEFINE, COMPUTE)
### FR-FOC-004: GRAPH Engine (BAR, PIE, LINE, AREA, SCATTER)
### FR-FOC-005: MODIFY/MAINTAIN Transaction Engine
### FR-FOC-006: Dialogue Manager (amper variables, control flow, -IF/-SET/-RUN)
### FR-FOC-007: Built-in Functions (150+ functions: character, date, numeric, conversion, system)
### FR-FOC-008: Data Adapters (FOCUS native, sequential, VSAM, DB2, IMS, ADABAS, SQL)
### FR-FOC-009: Output Formatting (StyleSheet, HTML/PDF/Excel, HOLD files)
### FR-FOC-010: Joins & Multi-Source Operations (JOIN, COMBINE, MATCH FILE)
### FR-FOC-011: Mainframe Environment Integration (FILEDEF, DYNAM ALLOCATE, TSO/CICS)
### FR-FOC-012: WebFOCUS Compatibility (basic report serving)

## 3. Non-Functional Requirements

### NFR-FOC-001: Test Coverage ≥90%
### NFR-FOC-002: Report Performance — generate 10,000+ row reports in <1 second
### NFR-FOC-003: Multi-adapter — support at least 5 data source types
