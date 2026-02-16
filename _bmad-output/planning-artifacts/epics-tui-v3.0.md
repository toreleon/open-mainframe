# TUI Crate — Epics & Stories

## Epic 900: Extended Screen Size Support

**Goal:** Support 3270 terminal models beyond Model 2 (24x80) for larger screen applications.

**Crate:** `open-mainframe-tui`
**FRs:** FR-v3.0-900

### Story 900.1: Multi-Model Screen Buffer

As a **CICS developer**,
I want **configurable terminal models (Model 2-5) with appropriate screen dimensions**,
So that **applications designed for larger screens render correctly**.

**Acceptance Criteria:**

**Given** SessionConfig with terminal_model=Model4
**When** the session initializes
**Then** the screen buffer is 43 rows x 80 columns

**Given** SessionConfig with terminal_model=Model5
**When** the session initializes
**Then** the screen buffer is 27 rows x 132 columns

**Given** a BMS map designed for Model 2 (24x80)
**When** rendered on Model 4 (43x80)
**Then** the map renders in the top 24 rows; remaining rows are blank

**Complexity:** M

### Story 900.2: Dynamic Screen Size via EWA

As a **CICS developer**,
I want **alternate screen size selection (EWA) during session lifetime**,
So that **programs can switch between default and alternate screen sizes**.

**Acceptance Criteria:**

**Given** a Model 5 terminal with default size 24x80 and alternate 27x132
**When** EWA (Erase/Write Alternate) is issued
**Then** the screen switches to 27x132 alternate size

**Given** EW (Erase/Write) after EWA
**When** issued
**Then** the screen reverts to 24x80 default size

**Complexity:** M

---

## Epic 901: PA Key Support

**Goal:** Implement PA1-PA3 attention keys for system functions and short-read operations.

**Crate:** `open-mainframe-tui`
**FRs:** FR-v3.0-901

### Story 901.1: PA Key Mapping and Short Read

As a **CICS user**,
I want **PA1, PA2, PA3 keys that send only the AID byte without field data**,
So that **I can trigger system functions without transmitting screen data**.

**Acceptance Criteria:**

**Given** Alt+1 is pressed (mapped to PA1)
**When** the AID is processed
**Then** only the AID code and cursor address are returned (no field data)

**Given** HANDLE AID PA1(HELP-ROUTINE) is active
**When** PA1 is pressed
**Then** control transfers to HELP-ROUTINE

**Complexity:** S

---

## Epic 902: Cursor Positioning

**Goal:** Implement explicit cursor positioning via SEND MAP CURSOR option.

**Crate:** `open-mainframe-tui`
**FRs:** FR-v3.0-903

### Story 902.1: CURSOR(data-value) Positioning

As a **CICS developer**,
I want **SEND MAP with CURSOR(position) to place the cursor at specific screen coordinates**,
So that **I can direct user attention to specific fields**.

**Acceptance Criteria:**

**Given** SEND MAP MAP('MAP1') CURSOR(480)
**When** the map is rendered
**Then** the cursor is placed at screen position 480 (row 6, col 0 on 24x80)

**Given** CURSOR option and IC attribute both present
**When** SEND MAP is processed
**Then** CURSOR option takes precedence over IC attribute

**Given** CURSOR position within a protected field
**When** SEND MAP is processed
**Then** the cursor moves to the next unprotected field

**Complexity:** M

---

## Epic 903: Field Validation Attributes

**Goal:** Implement 3270 extended field validation for data entry enforcement.

**Crate:** `open-mainframe-tui`
**FRs:** FR-v3.0-902

### Story 903.1: MUSTENTER and MUSTFILL Validation

As a **CICS developer**,
I want **MUSTENTER to require field modification and MUSTFILL to require all positions filled**,
So that **data entry validation is enforced at the terminal level**.

**Acceptance Criteria:**

**Given** a field with MUSTENTER validation
**When** Enter is pressed without modifying the field
**Then** the keyboard is locked and an error indicator is displayed

**Given** a field with MUSTFILL validation and 3 of 10 positions filled
**When** Enter is pressed
**Then** the keyboard is locked and an error indicator is displayed

**Given** a field with MUSTFILL validation and all 10 positions filled
**When** Enter is pressed
**Then** the AID is processed normally

**Complexity:** M

---

## Epic 904: TN3270 Protocol Support

**Goal:** Enable connection to actual z/OS CICS regions via TN3270 protocol.

**Crate:** `open-mainframe-tui`
**FRs:** FR-v3.0-904

### Story 904.1: TN3270E Connection and Negotiation

As a **user**,
I want **TN3270E protocol support for connecting to real z/OS systems**,
So that **I can use the TUI as a 3270 terminal emulator**.

**Acceptance Criteria:**

**Given** a TN3270 server address and port
**When** a connection is established
**Then** TN3270E negotiation completes (device type, function negotiation)

**Given** an active TN3270 connection
**When** the host sends a Write command with data stream
**Then** the screen is rendered from the 3270 data stream

**Given** the user presses Enter
**When** the AID is sent
**Then** field data is encoded as a 3270 inbound data stream and transmitted

**Complexity:** XL

### Story 904.2: TN3270 Data Stream Processing

As a **user**,
I want **outbound 3270 data stream parsing (Write, EW, EWA, WSF) and inbound stream generation**,
So that **the full 3270 protocol is supported for real mainframe interaction**.

**Acceptance Criteria:**

**Given** a Write command with SBA, SF, SFE orders
**When** processed
**Then** fields are created on the screen at the specified positions with attributes

**Given** user input on an unprotected field
**When** Enter is pressed
**Then** an inbound data stream with AID, cursor address, and SBA+data pairs is generated

**Complexity:** XL

---

## Epic 905: Structured Field Support

**Goal:** Implement WSF (Write Structured Field) and Query Reply for device capability negotiation.

**Crate:** `open-mainframe-tui`
**FRs:** FR-v3.0-905

### Story 905.1: Query Reply

As a **developer**,
I want **Query Reply to report terminal capabilities (colors, highlighting, screen sizes, partitions)**,
So that **host applications can adapt to the terminal's supported features**.

**Acceptance Criteria:**

**Given** a Read Partition Query structured field
**When** processed
**Then** Query Reply structured fields are generated reporting usable area, colors, highlighting, and implicit partition

**Complexity:** L

---

## Epic 906: Operator Information Area

**Goal:** Implement the OIA status line showing terminal state indicators.

**Crate:** `open-mainframe-tui`
**FRs:** FR-v3.0-906

### Story 906.1: OIA Status Indicators

As a **user**,
I want **an OIA line showing system lock, keyboard state, insert mode, and connection status**,
So that **I can see terminal state at a glance like a real 3270 terminal**.

**Acceptance Criteria:**

**Given** the keyboard is locked (waiting for host response)
**When** the OIA is displayed
**Then** a keyboard lock indicator (X SYSTEM) is shown

**Given** insert mode is active
**When** the OIA is displayed
**Then** an insert mode indicator (^) is shown

**Given** the terminal is connected
**When** the OIA is displayed
**Then** a connection indicator is shown with the LU name

**Complexity:** M

---

## Epic 907: DBCS Field Rendering

**Goal:** Render double-byte character set fields for CJK language support.

**Crate:** `open-mainframe-tui`
**FRs:** FR-v3.0-907

### Story 907.1: SO/SI Character Rendering

As a **user viewing CJK data**,
I want **DBCS fields rendered with correct double-width character display**,
So that **Japanese, Chinese, and Korean text appears correctly on 3270 screens**.

**Acceptance Criteria:**

**Given** a field containing SO + DBCS character pairs + SI
**When** rendered
**Then** each DBCS character occupies two column positions on screen

**Given** cursor navigation in a DBCS field
**When** moving right
**Then** the cursor skips two columns per DBCS character

**Complexity:** L
