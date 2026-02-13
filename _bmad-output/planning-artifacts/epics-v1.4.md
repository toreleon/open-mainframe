---
version: 'v1.4'
baseVersion: 'v1.3'
date: '2026-02-13'
status: 'draft'
---

# Epics - zOS-clone v1.4: CardDemo Compatibility

## Epic Overview

| Epic | Name | Stories | Complexity | Phase |
|------|------|---------|------------|-------|
| 37 | COBOL Copybook Support | 4 | M | Foundation |
| 38 | EIB and Standard Copybooks | 3 | S | Foundation |
| 39 | BMS Map Support | 6 | L | BMS |
| 40 | CICS Screen Commands | 5 | L | BMS |
| 41 | CICS Program Control | 4 | M | CICS |
| 42 | VSAM Keyed Access | 4 | M | Data |
| 43 | CardDemo Integration | 4 | M | Integration |

**Total: 7 Epics, ~30 Stories**

---

## Epic 37: COBOL Copybook Support

**Goal:** Implement COPY statement for copybook inclusion.

**Crate:** `zos-cobol`
**FRs:** FR-v1.4-009

### Story 37.1: COPY Statement Parser

As a **COBOL developer**,
I want **COPY statement recognized**,
So that **copybooks are included in compilation**.

**Acceptance Criteria:**

**Given** COPY MYBOOK
**When** parsed
**Then** copybook name extracted

**Given** COPY MYBOOK IN LIBRARY
**When** parsed
**Then** library qualifier extracted

**Complexity:** M
**Supports:** FR-v1.4-009

---

### Story 37.2: Copybook Path Resolution

As a **COBOL runtime**,
I want **copybook files located**,
So that **they can be included**.

**Acceptance Criteria:**

**Given** copybook name COCOM01Y
**When** resolved
**Then** file found in search paths

**Given** ZOS_COPYBOOK_PATH environment
**When** set
**Then** paths searched in order

**Complexity:** M
**Supports:** FR-v1.4-009

---

### Story 37.3: COPY REPLACING

As a **COBOL developer**,
I want **COPY with REPLACING**,
So that **I can customize copybook content**.

**Acceptance Criteria:**

**Given** COPY MYBOOK REPLACING ==OLD== BY ==NEW==
**When** included
**Then** all occurrences replaced

**Given** multiple REPLACING pairs
**When** processed
**Then** all replacements applied

**Complexity:** M
**Supports:** FR-v1.4-009

---

### Story 37.4: Nested Copybooks

As a **COBOL developer**,
I want **copybooks that include other copybooks**,
So that **complex structures work**.

**Acceptance Criteria:**

**Given** copybook A includes COPY B
**When** A is included
**Then** B is also resolved

**Given** circular reference
**When** detected
**Then** error reported

**Complexity:** S
**Supports:** FR-v1.4-009

---

## Epic 38: EIB and Standard Copybooks

**Goal:** Provide CICS standard data structures.

**Crate:** `zos-cics`
**FRs:** FR-v1.4-006, FR-v1.4-007, FR-v1.4-008

### Story 38.1: Execute Interface Block

As a **CICS developer**,
I want **EIB structure available**,
So that **I can access transaction context**.

**Acceptance Criteria:**

**Given** CICS program
**When** EIB referenced
**Then** EIBCALEN, EIBAID, EIBTRNID, EIBTIME, EIBDATE available

**Given** key pressed
**When** EIBAID checked
**Then** correct AID value present

**Complexity:** M
**Supports:** FR-v1.4-006

---

### Story 38.2: DFHAID Copybook

As a **CICS developer**,
I want **DFHAID copybook**,
So that **I can test attention identifiers**.

**Acceptance Criteria:**

**Given** COPY DFHAID
**When** included
**Then** DFHENTER, DFHPF1-24, DFHPA1-3, DFHCLEAR defined

**Complexity:** S
**Supports:** FR-v1.4-007

---

### Story 38.3: DFHBMSCA Copybook

As a **CICS developer**,
I want **DFHBMSCA copybook**,
So that **I can use BMS attributes**.

**Acceptance Criteria:**

**Given** COPY DFHBMSCA
**When** included
**Then** DFHBMPRF, DFHBMUNP, DFHBMBRY, DFHBMDAR defined

**Complexity:** S
**Supports:** FR-v1.4-008

---

## Epic 39: BMS Map Support

**Goal:** Parse and use BMS map definitions.

**Crate:** `zos-cics`
**FRs:** FR-v1.4-001

### Story 39.1: BMS Source Parser

As a **CICS developer**,
I want **BMS map source parsed**,
So that **screen definitions are loaded**.

**Acceptance Criteria:**

**Given** DFHMSD TYPE=MAP,... macro
**When** parsed
**Then** mapset name and options extracted

**Given** DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
**When** parsed
**Then** map dimensions stored

**Complexity:** L
**Supports:** FR-v1.4-001

---

### Story 39.2: Field Definition Parsing

As a **CICS runtime**,
I want **DFHMDF fields parsed**,
So that **field positions and attributes known**.

**Acceptance Criteria:**

**Given** DFHMDF POS=(1,1),LENGTH=8,ATTRB=(ASKIP,BRT)
**When** parsed
**Then** position, length, attributes extracted

**Given** DFHMDF with INITIAL='text'
**When** parsed
**Then** initial value stored

**Complexity:** M
**Supports:** FR-v1.4-001

---

### Story 39.3: BMS Copybook Generation

As a **COBOL developer**,
I want **COBOL copybooks from BMS maps**,
So that **I can use them in programs**.

**Acceptance Criteria:**

**Given** BMS map definition
**When** processed
**Then** DSECT copybook generated (like COSGN0AI/COSGN0AO)

**Given** field with PICIN/PICOUT
**When** generated
**Then** correct PIC clauses in copybook

**Complexity:** M
**Supports:** FR-v1.4-001

---

### Story 39.4: Map Repository

As a **CICS runtime**,
I want **compiled maps stored**,
So that **SEND/RECEIVE can use them**.

**Acceptance Criteria:**

**Given** map compiled
**When** stored
**Then** retrievable by mapset/map name

**Given** map not found
**When** accessed
**Then** MAPFAIL condition raised

**Complexity:** S
**Supports:** FR-v1.4-001

---

### Story 39.5: Field Attribute Handling

As a **CICS runtime**,
I want **field attributes processed**,
So that **display is correct**.

**Acceptance Criteria:**

**Given** ASKIP attribute
**When** displayed
**Then** field not enterable

**Given** BRT attribute
**When** displayed
**Then** field highlighted

**Given** DRK attribute
**When** displayed
**Then** field not visible

**Complexity:** M
**Supports:** FR-v1.4-001

---

### Story 39.6: Cursor Positioning

As a **CICS runtime**,
I want **cursor positioned correctly**,
So that **user input works**.

**Acceptance Criteria:**

**Given** field length set to -1
**When** SEND with CURSOR
**Then** cursor positioned at that field

**Complexity:** S
**Supports:** FR-v1.4-001

---

## Epic 40: CICS Screen Commands

**Goal:** Implement SEND MAP and RECEIVE MAP.

**Crate:** `zos-cics`
**FRs:** FR-v1.4-002, FR-v1.4-003

### Story 40.1: SEND MAP Implementation

As a **CICS developer**,
I want **EXEC CICS SEND MAP**,
So that **I can display screens**.

**Acceptance Criteria:**

**Given** SEND MAP('MAP') MAPSET('SET')
**When** executed
**Then** map displayed on terminal

**Given** FROM(data-area)
**When** specified
**Then** data merged with map

**Complexity:** L
**Supports:** FR-v1.4-002

---

### Story 40.2: SEND MAP Options

As a **CICS developer**,
I want **SEND MAP options**,
So that **display is controlled**.

**Acceptance Criteria:**

**Given** ERASE option
**When** SEND executed
**Then** screen cleared first

**Given** CURSOR option
**When** SEND executed
**Then** cursor positioned

**Complexity:** M
**Supports:** FR-v1.4-002

---

### Story 40.3: RECEIVE MAP Implementation

As a **CICS developer**,
I want **EXEC CICS RECEIVE MAP**,
So that **I can get user input**.

**Acceptance Criteria:**

**Given** RECEIVE MAP('MAP') MAPSET('SET')
**When** executed
**Then** input copied to data area

**Given** INTO(data-area)
**When** specified
**Then** data placed in area

**Complexity:** L
**Supports:** FR-v1.4-003

---

### Story 40.4: SEND TEXT Implementation

As a **CICS developer**,
I want **EXEC CICS SEND TEXT**,
So that **I can display messages**.

**Acceptance Criteria:**

**Given** SEND TEXT FROM(msg) LENGTH(len)
**When** executed
**Then** text displayed

**Given** ERASE FREEKB options
**When** specified
**Then** screen cleared, keyboard unlocked

**Complexity:** M
**Supports:** FR-v1.4-002

---

### Story 40.5: Terminal Buffer Management

As a **CICS runtime**,
I want **terminal buffers managed**,
So that **I/O is efficient**.

**Acceptance Criteria:**

**Given** screen output
**When** buffered
**Then** sent efficiently

**Given** screen input
**When** received
**Then** parsed correctly

**Complexity:** M
**Supports:** FR-v1.4-002, FR-v1.4-003

---

## Epic 41: CICS Program Control

**Goal:** Implement program transfer and system functions.

**Crate:** `zos-cics`
**FRs:** FR-v1.4-004, FR-v1.4-005

### Story 41.1: XCTL Implementation

As a **CICS developer**,
I want **EXEC CICS XCTL**,
So that **I can transfer to another program**.

**Acceptance Criteria:**

**Given** XCTL PROGRAM('PGMNAME')
**When** executed
**Then** control transfers to program

**Given** COMMAREA(data) LENGTH(len)
**When** specified
**Then** data passed to new program

**Complexity:** M
**Supports:** FR-v1.4-004

---

### Story 41.2: Program Load and Link

As a **CICS runtime**,
I want **programs loaded dynamically**,
So that **XCTL works**.

**Acceptance Criteria:**

**Given** XCTL to COMEN01C
**When** executed
**Then** COMEN01C loaded and started

**Given** program not found
**When** XCTL executed
**Then** PGMIDERR condition raised

**Complexity:** M
**Supports:** FR-v1.4-004

---

### Story 41.3: ASSIGN Implementation

As a **CICS developer**,
I want **EXEC CICS ASSIGN**,
So that **I can get system values**.

**Acceptance Criteria:**

**Given** ASSIGN APPLID(field)
**When** executed
**Then** application ID returned

**Given** ASSIGN SYSID(field)
**When** executed
**Then** system ID returned

**Given** ASSIGN USERID(field)
**When** executed
**Then** user ID returned

**Complexity:** S
**Supports:** FR-v1.4-005

---

### Story 41.4: RETURN with TRANSID

As a **CICS developer**,
I want **RETURN TRANSID COMMAREA**,
So that **conversation continues**.

**Acceptance Criteria:**

**Given** RETURN TRANSID('TRAN') COMMAREA(data)
**When** executed
**Then** next input invokes TRAN with data

**Complexity:** M
**Supports:** FR-v1.4-004

---

## Epic 42: VSAM Keyed Access

**Goal:** Implement CICS file READ with keys.

**Crate:** `zos-cics`, `zos-dataset`
**FRs:** FR-v1.4-010

### Story 42.1: READ with RIDFLD

As a **CICS developer**,
I want **READ with RIDFLD**,
So that **I can access records by key**.

**Acceptance Criteria:**

**Given** READ DATASET('FILE') INTO(data) RIDFLD(key)
**When** executed
**Then** record with matching key returned

**Given** key not found
**When** READ executed
**Then** NOTFND condition (resp=13)

**Complexity:** M
**Supports:** FR-v1.4-010

---

### Story 42.2: KEYLENGTH Option

As a **CICS developer**,
I want **KEYLENGTH option**,
So that **partial key access works**.

**Acceptance Criteria:**

**Given** READ with KEYLENGTH(8)
**When** executed
**Then** first 8 bytes of RIDFLD used as key

**Complexity:** S
**Supports:** FR-v1.4-010

---

### Story 42.3: File Control Table

As a **CICS administrator**,
I want **FCT definitions**,
So that **datasets are mapped to files**.

**Acceptance Criteria:**

**Given** FCT entry USRSEC -> AWS.M2.CARDDEMO.USRSEC
**When** READ DATASET('USRSEC')
**Then** correct VSAM file accessed

**Complexity:** M
**Supports:** FR-v1.4-010

---

### Story 42.4: RESP/RESP2 Handling

As a **CICS developer**,
I want **RESP and RESP2 options**,
So that **I can handle errors**.

**Acceptance Criteria:**

**Given** READ with RESP(rc) RESP2(rc2)
**When** executed
**Then** return codes set

**Given** successful read
**When** RESP checked
**Then** value is 0

**Given** record not found
**When** RESP checked
**Then** value is 13 (NOTFND)

**Complexity:** S
**Supports:** FR-v1.4-010

---

## Epic 43: CardDemo Integration

**Goal:** Run CardDemo application.

**Crate:** All
**FRs:** All

### Story 43.1: CardDemo Copybook Setup

As a **administrator**,
I want **CardDemo copybooks installed**,
So that **programs compile**.

**Acceptance Criteria:**

**Given** CardDemo copybooks
**When** installed in copybook path
**Then** COBOL compiler finds them

**Complexity:** S

---

### Story 43.2: CardDemo Data Setup

As a **administrator**,
I want **CardDemo data files created**,
So that **application runs**.

**Acceptance Criteria:**

**Given** USRSEC file definition
**When** created
**Then** user records accessible

**Given** sample users
**When** loaded
**Then** sign-on works

**Complexity:** M

---

### Story 43.3: Sign-on Flow Test

As a **tester**,
I want **sign-on to work**,
So that **basic functionality verified**.

**Acceptance Criteria:**

**Given** user enters credentials
**When** sign-on submitted
**Then** user authenticated or error shown

**Given** valid user
**When** authenticated
**Then** menu displayed

**Complexity:** M

---

### Story 43.4: Navigation Test

As a **tester**,
I want **menu navigation to work**,
So that **application usable**.

**Acceptance Criteria:**

**Given** user at menu
**When** option selected
**Then** correct screen displayed

**Given** PF3 pressed
**When** processed
**Then** return to previous screen

**Complexity:** M

---

## Implementation Order

### Phase 1: Foundation (Epics 37, 38)
1. COPY statement support
2. EIB and standard copybooks
3. Basic COBOL features

### Phase 2: BMS (Epics 39, 40)
1. BMS map parser
2. SEND/RECEIVE MAP
3. Terminal integration

### Phase 3: CICS & Data (Epics 41, 42)
1. XCTL and program control
2. VSAM keyed access
3. FCT support

### Phase 4: Integration (Epic 43)
1. CardDemo setup
2. End-to-end testing
3. Documentation
