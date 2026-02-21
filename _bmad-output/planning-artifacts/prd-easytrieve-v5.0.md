---
version: 'v5.0'
planningGroup: 'PG-26'
technology: 'Easytrieve'
date: '2026-02-21'
status: 'complete'
totalFRs: 10
totalNFRs: 3
---

# PRD: Easytrieve

## 1. Overview

Easytrieve Plus is a high-level report-generation and data-extraction language for z/OS batch processing. Widely used for ad-hoc reporting, file processing, and data transformation, it provides simpler syntax than COBOL with built-in report formatting, automatic file processing, and SQL database access.

## 2. Functional Requirements

### FR-EZ-001: Source Parsing
The system SHALL parse 80-column Easytrieve source format with free-form or positional statements, continuation via `+`, comments (`*`), and inline comments.

### FR-EZ-002: File Definitions
The system SHALL support FILE declarations with LRECL, RECFM (F/V/FB/VB), KEY fields, and file types (sequential, indexed/VSAM, printer). Multiple files in a single program with synchronization via MATCH.

### FR-EZ-003: Field Definitions
The system SHALL support field definitions with 6 data types: A (alphanumeric), N (numeric display), P (packed decimal), B (binary), U (unsigned numeric), I (integer). Fields have name, start position, length, decimal places, and HEADING attribute.

### FR-EZ-004: Control Flow
The system SHALL support IF/ELSE/END-IF, DO/END-DO, PERFORM, GOTO, STOP, and conditional expressions with AND/OR/NOT.

### FR-EZ-005: Data Manipulation
The system SHALL support MOVE, MOVE LIKE, arithmetic (COMPUTE style via field = expression), string operations (TRIM, PAD, SUBSTR), date handling, and SEARCH/TABLE for lookups.

### FR-EZ-006: Automatic File Processing
The system SHALL support automatic input/output processing: JOB reads input files automatically, processes each record through user code, and terminates at end-of-file.

### FR-EZ-007: Report Generation
The system SHALL support REPORT with LINE, TITLE, HEADING, CONTROL (control breaks), SUM (automatic totaling), PAGE-BREAK, SEQUENCE. Reports generate formatted output with headers, detail lines, subtotals, and grand totals.

### FR-EZ-008: SORT Activity
The system SHALL support SORT with USING (input file), GIVING (output file), KEY fields, and SELECT/REJECT criteria. SORT can operate standalone or as input to subsequent JOB activities.

### FR-EZ-009: SQL/Database Access
The system SHALL support embedded SQL for DB2 access (SELECT, INSERT, UPDATE, DELETE) and IMS/DLI calls for hierarchical database access within Easytrieve programs.

### FR-EZ-010: Macros and External Calls
The system SHALL support %INCLUDE for macros/copybooks, CALL for external program invocation, and LINK/TRANSFER for inter-program control.

## 3. Non-Functional Requirements

### NFR-EZ-001: Test Coverage
All parser and interpreter functionality SHALL have ≥90% code coverage.

### NFR-EZ-002: Diagnostic Quality
Errors SHALL include line numbers and descriptive messages via miette.

### NFR-EZ-003: Performance
Processing SHALL handle at least 100,000 records per second for in-memory operations.
