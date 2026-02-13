---
version: 'v1.4'
baseVersion: 'v1.3'
date: '2026-02-13'
status: 'draft'
---

# Product Requirements Document - zOS-clone v1.4: CardDemo Compatibility

## Overview

**Goal:** Run the AWS CardDemo application (https://github.com/aws-samples/aws-mainframe-modernization-carddemo) on zOS-clone.

CardDemo is a comprehensive COBOL/CICS credit card management application that serves as an industry-standard benchmark for mainframe modernization capabilities.

## CardDemo Application Analysis

### Programs (Core Set)
| Program | Type | Function |
|---------|------|----------|
| COSGN00C | CICS | Sign-on screen |
| COMEN01C | CICS | Main menu |
| COACTVWC | CICS | Account view |
| COACTUPC | CICS | Account update |
| COCRDLIC | CICS | Credit card list |
| COCRDSLC | CICS | Credit card view |
| COCRDUPC | CICS | Credit card update |
| COTRN00C | CICS | Transaction menu |
| COUSR00C | CICS | User management |
| CBTRN02C | Batch | Transaction processing |

### Required CICS Commands
From code analysis:
- `EXEC CICS RETURN TRANSID COMMAREA LENGTH`
- `EXEC CICS RECEIVE MAP MAPSET RESP RESP2`
- `EXEC CICS SEND MAP MAPSET FROM ERASE CURSOR`
- `EXEC CICS SEND TEXT FROM LENGTH ERASE FREEKB`
- `EXEC CICS READ DATASET INTO LENGTH RIDFLD KEYLENGTH RESP RESP2`
- `EXEC CICS XCTL PROGRAM COMMAREA`
- `EXEC CICS ASSIGN APPLID SYSID`

### Required Copybooks
- COCOM01Y - COMMAREA structure
- COTTL01Y - Title constants
- CSDAT01Y - Date fields
- CSMSG01Y - Messages
- CSUSR01Y - User security record
- DFHAID - AID key values
- DFHBMSCA - BMS attributes
- BMS map copybooks (COSGN00, COMEN01, etc.)

### Data Files
- USRSEC - User security (VSAM KSDS)
- ACCTDATA - Accounts (FB/300)
- CARDDATA - Credit cards (FB/150)
- CUSTDATA - Customers (FB/500)
- TRANSACT - Transactions (VSAM KSDS)

---

## Functional Requirements

### FR-v1.4-001: BMS Map Definition Parser
Parse BMS map source files to extract field definitions, positions, and attributes.

**Acceptance Criteria:**
- Parse DFHMSD macro (mapset definition)
- Parse DFHMDI macro (map definition)
- Parse DFHMDF macro (field definition)
- Extract field positions, lengths, attributes
- Generate COBOL copybook structures

### FR-v1.4-002: CICS SEND MAP Command
Implement screen output via BMS maps.

**Acceptance Criteria:**
- SEND MAP('name') MAPSET('name')
- FROM(data-area) option
- ERASE option (clear screen)
- CURSOR option (position cursor)

### FR-v1.4-003: CICS RECEIVE MAP Command
Implement screen input via BMS maps.

**Acceptance Criteria:**
- RECEIVE MAP('name') MAPSET('name')
- INTO(data-area) option
- RESP/RESP2 options

### FR-v1.4-004: CICS XCTL Command
Implement program transfer with COMMAREA.

**Acceptance Criteria:**
- XCTL PROGRAM('name')
- COMMAREA(data-area) option
- LENGTH(value) option

### FR-v1.4-005: CICS ASSIGN Command
Implement system value retrieval.

**Acceptance Criteria:**
- ASSIGN APPLID(field)
- ASSIGN SYSID(field)
- ASSIGN USERID(field)

### FR-v1.4-006: Execute Interface Block (EIB)
Implement complete EIB structure.

**Acceptance Criteria:**
- EIBCALEN - COMMAREA length
- EIBAID - Attention ID (key pressed)
- EIBTRNID - Transaction ID
- EIBTIME - Time
- EIBDATE - Date
- EIBTASKN - Task number

### FR-v1.4-007: DFHAID Copybook
Provide standard AID key definitions.

**Acceptance Criteria:**
- DFHENTER (Enter key)
- DFHCLEAR (Clear key)
- DFHPF1-DFHPF24 (PF keys)
- DFHPA1-DFHPA3 (PA keys)

### FR-v1.4-008: DFHBMSCA Copybook
Provide BMS attribute constants.

**Acceptance Criteria:**
- DFHBMPRF (protected field)
- DFHBMUNP (unprotected field)
- DFHBMBRY (bright)
- DFHBMDAR (dark)

### FR-v1.4-009: COBOL COPY Statement
Implement copybook inclusion.

**Acceptance Criteria:**
- COPY copybook-name
- COPY copybook-name REPLACING
- Search copybook paths
- Handle nested COPY

### FR-v1.4-010: VSAM KSDS READ with Key
Implement keyed file access.

**Acceptance Criteria:**
- READ with RIDFLD (record ID field)
- READ with KEYLENGTH
- RESP/RESP2 for status
- Handle NOTFND condition

### FR-v1.4-011: COBOL REDEFINES
Support field redefinitions.

**Acceptance Criteria:**
- Parse REDEFINES clause
- Share storage between fields
- Support nested REDEFINES

### FR-v1.4-012: 88-Level Conditions
Implement condition names.

**Acceptance Criteria:**
- Parse 88-level items
- SET condition-name TO TRUE
- Evaluate conditions in IF/EVALUATE

### FR-v1.4-013: FUNCTION UPPER-CASE
Implement string function (may already exist).

### FR-v1.4-014: FUNCTION CURRENT-DATE
Implement date function (may already exist).

### FR-v1.4-015: Reference Modification
Support substring operations.

**Acceptance Criteria:**
- field-name(start:length)
- Dynamic start/length values

### FR-v1.4-016: Terminal Emulator Integration
Connect BMS to terminal display.

**Acceptance Criteria:**
- 3270 terminal emulation (basic)
- Field positioning
- Attribute handling
- Keyboard input

---

## Non-Functional Requirements

### NFR-v1.4-001: CardDemo Test Suite
Create automated tests using CardDemo scenarios.

### NFR-v1.4-002: Compatibility Metrics
Track percentage of CardDemo code that compiles/runs.

### NFR-v1.4-003: Documentation
Document CardDemo setup and execution.

---

## Out of Scope for v1.4

- DB2 integration modules (optional in CardDemo)
- IMS integration modules (optional in CardDemo)
- MQ integration modules (optional in CardDemo)
- Full 3270 terminal emulation (basic text mode acceptable)

---

## Success Criteria

1. **Compilation:** All core CardDemo COBOL programs compile without errors
2. **Execution:** Sign-on flow (COSGN00C → COMEN01C) executes
3. **Data Access:** User authentication against USRSEC file works
4. **Navigation:** Menu navigation between screens works

---

## Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|------------|
| BMS complexity | High | Focus on subset needed for CardDemo |
| Terminal emulation | Medium | Start with text-based output |
| VSAM compatibility | Medium | Leverage existing VSAM support |
