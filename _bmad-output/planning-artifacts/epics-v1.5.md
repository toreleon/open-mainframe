---
version: 'v1.5'
baseVersion: 'v1.4'
date: '2026-02-13'
status: 'draft'
---

# Epics - zOS-clone v1.5: Interactive CICS Terminal

## Epic Overview

| Epic | Name | Stories | Complexity | Phase |
|------|------|---------|------------|-------|
| 44 | TUI Rendering Engine | 6 | L | Foundation |
| 45 | Keyboard Input & Field Navigation | 5 | L | Foundation |
| 46 | Pseudo-Conversational Session Manager | 5 | L | Core |
| 47 | Interactive CICS CLI Command | 4 | M | Core |
| 48 | Screen Attributes & Color | 4 | M | Polish |
| 49 | DB2 Runtime Integration | 4 | M | Optional |
| 50 | CardDemo Interactive Integration | 4 | M | Integration |

**Total: 7 Epics, ~32 Stories**

---

## Requirements Coverage Map

| Requirement | Epic(s) |
|-------------|---------|
| FR-v1.5-001 TUI Screen Renderer | 44 |
| FR-v1.5-002 Field Navigation | 45 |
| FR-v1.5-003 Keyboard Input Capture | 45 |
| FR-v1.5-004 AID Key Processing | 45, 46 |
| FR-v1.5-005 Pseudo-Conversational Session | 46 |
| FR-v1.5-006 Interactive CICS Command | 47 |
| FR-v1.5-007 Screen Attribute Rendering | 48 |
| FR-v1.5-008 Status Line Display | 44, 47 |
| FR-v1.5-009 SEND TEXT Rendering | 44 |
| FR-v1.5-010 DB2 Runtime Integration | 49 |
| FR-v1.5-011 SEND MAP DATAONLY | 44 |
| FR-v1.5-012 Color Support | 48 |
| NFR-v1.5-001 Input Latency | 45 |
| NFR-v1.5-002 Terminal Compatibility | 44, 48 |
| NFR-v1.5-003 Graceful Degradation | 48 |
| NFR-v1.5-004 Resource Usage | 44 |
| NFR-v1.5-005 Test Coverage | 50 |

---

## Epic 44: TUI Rendering Engine

**Goal:** Render the 3270 ScreenBuffer to a real terminal using ratatui/crossterm.

**Crate:** `zos-tui` (new)
**FRs:** FR-v1.5-001, FR-v1.5-008, FR-v1.5-009, FR-v1.5-011

### Story 44.1: ratatui/crossterm Integration Scaffold

As a **developer**,
I want **the zos-tui crate initialized with ratatui and crossterm**,
So that **I have a foundation for terminal rendering**.

**Acceptance Criteria:**

**Given** the zos-tui crate is created
**When** built
**Then** ratatui and crossterm compile and link correctly

**Given** a basic terminal setup
**When** raw mode is entered
**Then** terminal enters alternate screen and raw input mode

**Given** a session ends or panics
**When** cleanup runs
**Then** terminal is restored to normal mode (cursor visible, echo on)

**Complexity:** M
**Supports:** FR-v1.5-001

---

### Story 44.2: ScreenBuffer to ratatui Widget

As a **developer**,
I want **a ratatui widget that renders a ScreenBuffer**,
So that **3270 screens display in the terminal**.

**Acceptance Criteria:**

**Given** a ScreenBuffer with 24 rows x 80 columns
**When** rendered as a ratatui widget
**Then** all characters appear at correct row/column positions

**Given** a field with INITIAL value "SIGN-ON"
**When** rendered
**Then** text "SIGN-ON" appears at the field's position

**Given** a ScreenBuffer with cursor at row 7, col 21
**When** rendered
**Then** terminal cursor is positioned at row 7, col 21

**Complexity:** L
**Supports:** FR-v1.5-001

---

### Story 44.3: SEND MAP Display Integration

As a **CICS runtime**,
I want **SEND MAP to trigger a TUI screen refresh**,
So that **users see updated screens**.

**Acceptance Criteria:**

**Given** a COBOL program executes SEND MAP('COSGN0A') MAPSET('COSGN00')
**When** the CicsBridge processes the command
**Then** the TUI renders the updated ScreenBuffer

**Given** SEND MAP with ERASE option
**When** processed
**Then** screen is cleared before rendering the map

**Given** SEND MAP with CURSOR option
**When** processed
**Then** cursor is positioned at the specified field

**Complexity:** M
**Supports:** FR-v1.5-001

---

### Story 44.4: SEND MAP DATAONLY and MAPONLY

As a **CICS developer**,
I want **partial screen updates**,
So that **only changed data refreshes**.

**Acceptance Criteria:**

**Given** SEND MAP with DATAONLY
**When** processed
**Then** only variable data fields are updated; static labels preserved

**Given** SEND MAP with MAPONLY
**When** processed
**Then** only static map content is sent; variable data cleared

**Given** a combination of DATAONLY and FROM(data-area)
**When** processed
**Then** data from the specified area is merged into variable fields only

**Complexity:** M
**Supports:** FR-v1.5-011

---

### Story 44.5: SEND TEXT Display

As a **CICS developer**,
I want **SEND TEXT rendered in the TUI**,
So that **text messages display correctly**.

**Acceptance Criteria:**

**Given** SEND TEXT FROM(msg) LENGTH(80)
**When** processed
**Then** text displayed starting at row 1, col 1

**Given** SEND TEXT with ERASE
**When** processed
**Then** screen cleared before text display

**Given** text longer than 80 characters
**When** displayed
**Then** text wraps to next line

**Complexity:** S
**Supports:** FR-v1.5-009

---

### Story 44.6: Status Line Widget

As a **terminal user**,
I want **a status line below the 3270 screen**,
So that **I can see program name, cursor position, and key hints**.

**Acceptance Criteria:**

**Given** an active session running COSGN00C
**When** the status line renders
**Then** program name "COSGN00C" is displayed

**Given** cursor at row 7, col 21
**When** the status line renders
**Then** "Row 7 Col 21" is displayed

**Given** any active session
**When** the status line renders
**Then** key hints like "F3=Exit ENTER=Submit" are displayed

**Complexity:** S
**Supports:** FR-v1.5-008

---

## Epic 45: Keyboard Input & Field Navigation

**Goal:** Capture keyboard input and implement 3270 field-level editing.

**Crate:** `zos-tui`
**FRs:** FR-v1.5-002, FR-v1.5-003, FR-v1.5-004

### Story 45.1: Raw Keyboard Event Loop

As a **TUI runtime**,
I want **keyboard events captured in raw mode**,
So that **all keys including function keys are available**.

**Acceptance Criteria:**

**Given** the TUI is in raw mode
**When** user presses a printable key
**Then** a character event is generated

**Given** the TUI is in raw mode
**When** user presses F1-F12
**Then** function key events are generated

**Given** the TUI is in raw mode
**When** user presses Ctrl+C
**Then** a session termination signal is generated

**Complexity:** M
**Supports:** FR-v1.5-003, NFR-v1.5-001

---

### Story 45.2: AID Key Mapping

As a **TUI runtime**,
I want **terminal keys mapped to 3270 AID codes**,
So that **CICS programs receive correct attention identifiers**.

**Acceptance Criteria:**

**Given** user presses Enter
**When** mapped
**Then** AID code is DFHENTER (0x7D)

**Given** user presses F1 through F12
**When** mapped
**Then** AID codes are DFHPF1 (0xF1) through DFHPF12 (0x7C)

**Given** user presses Escape
**When** mapped
**Then** AID code is DFHCLEAR (0x6D)

**Given** key mapping configuration exists
**When** loaded
**Then** custom key bindings override defaults

**Complexity:** S
**Supports:** FR-v1.5-003, FR-v1.5-004

---

### Story 45.3: Field-Level Character Input

As a **terminal user**,
I want **typed characters entered into the current field**,
So that **I can fill in form data**.

**Acceptance Criteria:**

**Given** cursor is in an unprotected field
**When** user types a character
**Then** character appears at cursor position and cursor advances

**Given** cursor is in a protected field
**When** user types a character
**Then** input is rejected (no change, optional beep)

**Given** cursor is in a numeric field
**When** user types a non-numeric character
**Then** input is rejected

**Given** a field is full (cursor at last position)
**When** user types another character
**Then** cursor does not advance past field boundary

**Given** a character is typed in a field
**When** the field's MDT is checked
**Then** MDT is set to true (field modified)

**Complexity:** M
**Supports:** FR-v1.5-002, FR-v1.5-004

---

### Story 45.4: Tab and Field Navigation

As a **terminal user**,
I want **Tab to move between input fields**,
So that **I can navigate the form**.

**Acceptance Criteria:**

**Given** cursor is in field N
**When** Tab is pressed
**Then** cursor moves to the first position of the next unprotected field

**Given** cursor is in the last unprotected field
**When** Tab is pressed
**Then** cursor wraps to the first unprotected field

**Given** cursor is in field N
**When** Backtab (Shift+Tab) is pressed
**Then** cursor moves to the first position of the previous unprotected field

**Given** an autoskip field that is full
**When** the last character is typed
**Then** cursor automatically advances to the next unprotected field

**Given** user presses Home
**When** processed
**Then** cursor moves to the first unprotected field on the screen

**Complexity:** M
**Supports:** FR-v1.5-002

---

### Story 45.5: Editing Keys (Backspace, Delete, Clear)

As a **terminal user**,
I want **standard editing keys to work**,
So that **I can correct input**.

**Acceptance Criteria:**

**Given** cursor is in an unprotected field at position > 0
**When** Backspace is pressed
**Then** previous character is deleted, remaining characters shift left, cursor moves back

**Given** cursor is in an unprotected field
**When** Delete is pressed
**Then** character at cursor is deleted, remaining characters shift left

**Given** an unprotected field with content
**When** Ctrl+U or field-erase key is pressed
**Then** field content is cleared from cursor to end of field

**Given** CLEAR key (Escape) is pressed
**When** processed
**Then** all unprotected fields are cleared, cursor moves to first field, AID=DFHCLEAR

**Complexity:** M
**Supports:** FR-v1.5-003

---

## Epic 46: Pseudo-Conversational Session Manager

**Goal:** Manage the full CICS pseudo-conversational lifecycle with RETURN TRANSID.

**Crate:** `zos-tui`, `zos-cics`
**FRs:** FR-v1.5-005, FR-v1.5-004

### Story 46.1: Session State Machine

As a **CICS runtime**,
I want **a session state machine managing conversation flow**,
So that **programs chain correctly**.

**Acceptance Criteria:**

**Given** a session starts with program COSGN00C
**When** the session state machine initializes
**Then** state is set to "Executing" with program COSGN00C

**Given** program issues SEND MAP
**When** state machine processes it
**Then** state transitions to "ScreenDisplayed"

**Given** state is "ScreenDisplayed"
**When** user presses an AID key
**Then** state transitions to "InputReceived"

**Given** program issues RETURN TRANSID('COSG')
**When** state machine processes it
**Then** state transitions to "WaitingForInput" with pending TRANSID "COSG"

**Given** state is "WaitingForInput" and user presses AID key
**When** state machine processes it
**Then** state transitions to "Executing" with the TRANSID's program

**Given** program issues RETURN without TRANSID
**When** state machine processes it
**Then** state transitions to "SessionEnded"

**Complexity:** L
**Supports:** FR-v1.5-005

---

### Story 46.2: COMMAREA Persistence

As a **CICS runtime**,
I want **COMMAREA preserved between conversation rounds**,
So that **programs maintain state**.

**Acceptance Criteria:**

**Given** program A issues RETURN TRANSID('B') COMMAREA(data) LENGTH(100)
**When** user presses Enter and program B starts
**Then** program B receives COMMAREA with the saved data

**Given** program B receives COMMAREA
**When** EIBCALEN is checked
**Then** EIBCALEN equals 100

**Given** program B modifies COMMAREA and RETURNs with TRANSID
**When** next program starts
**Then** updated COMMAREA is passed

**Complexity:** M
**Supports:** FR-v1.5-005

---

### Story 46.3: XCTL Within Session

As a **CICS runtime**,
I want **XCTL to work within an interactive session**,
So that **programs transfer control seamlessly**.

**Acceptance Criteria:**

**Given** program A issues XCTL PROGRAM('B') COMMAREA(data)
**When** processed within the session
**Then** program B starts immediately without user interaction

**Given** program B issues SEND MAP
**When** processed
**Then** the new screen renders in the TUI

**Given** XCTL chains A → B → C
**When** each transfer occurs
**Then** each program's SEND MAP renders correctly

**Complexity:** M
**Supports:** FR-v1.5-005

---

### Story 46.4: Transaction ID to Program Resolution

As a **CICS runtime**,
I want **TRANSID mapped to programs**,
So that **RETURN TRANSID knows which program to load**.

**Acceptance Criteria:**

**Given** a program-to-transid mapping configuration
**When** RETURN TRANSID('COSG') executes
**Then** the session loads the program associated with 'COSG'

**Given** TRANSID not found in mapping
**When** RETURN TRANSID executes
**Then** an error is displayed on the terminal status line

**Given** `--transid-map <file>` CLI option
**When** specified
**Then** mappings loaded from the file

**Complexity:** M
**Supports:** FR-v1.5-005

---

### Story 46.5: Session Error Handling and Recovery

As a **terminal user**,
I want **errors displayed gracefully**,
So that **I can understand and recover from problems**.

**Acceptance Criteria:**

**Given** a program ABENDs during execution
**When** the error is caught
**Then** error message displayed on status line, session continues

**Given** HANDLE ABEND is set in the program
**When** an abend occurs
**Then** the abend handler program is invoked

**Given** an unrecoverable error occurs
**When** the session cannot continue
**Then** terminal is restored to normal mode, error details printed to stderr

**Complexity:** M
**Supports:** FR-v1.5-005

---

## Epic 47: Interactive CICS CLI Command

**Goal:** Add the `zos-clone cics` subcommand for interactive sessions.

**Crate:** `zos-clone`
**FRs:** FR-v1.5-006

### Story 47.1: `cics` Subcommand Definition

As a **user**,
I want **`zos-clone cics <program>` to launch an interactive session**,
So that **I can run CICS applications**.

**Acceptance Criteria:**

**Given** `zos-clone cics COSGN00C.cbl -I ./copybooks`
**When** executed
**Then** TUI initializes and program begins execution

**Given** required program file not found
**When** command starts
**Then** clear error message with file path suggestion

**Given** `zos-clone cics --help`
**When** executed
**Then** all options documented with examples

**Complexity:** M
**Supports:** FR-v1.5-006

---

### Story 47.2: VSAM Data File Loading

As a **user**,
I want **`--data DDNAME=path:key_len:rec_len` to load data files**,
So that **CICS file operations work**.

**Acceptance Criteria:**

**Given** `--data USRSEC=./data/usrsec.dat:10:100`
**When** session starts
**Then** USRSEC file loaded into CICS file manager

**Given** multiple `--data` options
**When** session starts
**Then** all files loaded

**Given** a data file not found
**When** session starts
**Then** warning displayed, session continues (file will return NOTFND on access)

**Complexity:** S
**Supports:** FR-v1.5-006

---

### Story 47.3: Program Directory for XCTL

As a **user**,
I want **`--program-dir <path>` to specify where XCTL targets are found**,
So that **multi-program applications work**.

**Acceptance Criteria:**

**Given** `--program-dir ./programs`
**When** XCTL PROGRAM('COMEN01C') executes
**Then** COMEN01C.cbl loaded from ./programs/

**Given** program not in program-dir
**When** XCTL executes
**Then** PGMIDERR condition raised, error displayed

**Given** no --program-dir specified
**When** XCTL executes
**Then** searches current directory and include paths

**Complexity:** S
**Supports:** FR-v1.5-006

---

### Story 47.4: Clean Session Shutdown

As a **user**,
I want **sessions to exit cleanly**,
So that **my terminal is always restored**.

**Acceptance Criteria:**

**Given** program RETURNs without TRANSID
**When** conversation ends
**Then** TUI exits, terminal restored, success message printed

**Given** user presses Ctrl+C
**When** signal caught
**Then** TUI exits, terminal restored, "Session interrupted" message

**Given** a panic occurs in the runtime
**When** the panic hook fires
**Then** terminal is restored before the panic message displays

**Complexity:** M
**Supports:** FR-v1.5-006

---

## Epic 48: Screen Attributes & Color

**Goal:** Render 3270 field attributes and colors accurately.

**Crate:** `zos-tui`
**FRs:** FR-v1.5-007, FR-v1.5-012

### Story 48.1: Field Attribute to Style Mapping

As a **TUI renderer**,
I want **3270 field attributes mapped to terminal styles**,
So that **fields look correct**.

**Acceptance Criteria:**

**Given** a protected field (ASKIP/PROT)
**When** rendered
**Then** displayed with dim/normal style, cursor skips over it

**Given** a bright field (BRT)
**When** rendered
**Then** displayed with bold style

**Given** a dark field (DRK)
**When** rendered
**Then** characters hidden (same foreground as background)

**Given** an unprotected field (UNPROT)
**When** rendered
**Then** displayed with underline or reverse video to indicate editability

**Complexity:** M
**Supports:** FR-v1.5-007

---

### Story 48.2: Color Theme Configuration

As a **user**,
I want **3270 colors rendered in the terminal**,
So that **the experience matches real terminals**.

**Acceptance Criteria:**

**Given** default color theme
**When** applied
**Then** green text on black background (classic 3270)

**Given** protected fields
**When** rendered with color
**Then** displayed in blue or cyan

**Given** error/bright fields
**When** rendered with color
**Then** displayed in red or white

**Given** `--color-theme <name>` option
**When** specified
**Then** alternate color theme applied

**Complexity:** S
**Supports:** FR-v1.5-012

---

### Story 48.3: Terminal Capability Detection

As a **TUI runtime**,
I want **terminal capabilities auto-detected**,
So that **rendering adapts to the environment**.

**Acceptance Criteria:**

**Given** TERM=xterm-256color
**When** capabilities detected
**Then** full color mode enabled

**Given** TERM=dumb or NO_COLOR set
**When** capabilities detected
**Then** monochrome mode with bold/dim/underline only

**Given** terminal does not support F13-F24
**When** key mapping loads
**Then** alternative bindings (Shift+F1-F12 or Alt+1-9) offered

**Complexity:** S
**Supports:** NFR-v1.5-002, NFR-v1.5-003

---

### Story 48.4: Cursor Visual Feedback

As a **terminal user**,
I want **the cursor to show where I am typing**,
So that **I know which field is active**.

**Acceptance Criteria:**

**Given** cursor is in an unprotected field
**When** rendered
**Then** blinking cursor visible at current position

**Given** cursor position changes (tab, character input)
**When** rerendered
**Then** cursor moves to new position immediately

**Given** no unprotected fields on screen
**When** rendered
**Then** cursor is hidden (keyboard locked state)

**Complexity:** S
**Supports:** FR-v1.5-002

---

## Epic 49: DB2 Runtime Integration

**Goal:** Enable SQL execution during interactive CICS sessions.

**Crate:** `zos-db2`, `zos-tui`
**FRs:** FR-v1.5-010

### Story 49.1: DB2 Connection Configuration

As a **user**,
I want **`--db2-url <connection-string>` to connect to a database**,
So that **SQL operations work**.

**Acceptance Criteria:**

**Given** `--db2-url postgres://user:pass@host/db`
**When** session starts
**Then** DB2 runtime connects to the database

**Given** no --db2-url specified
**When** session starts
**Then** DB2 runtime initializes in offline mode (all SQL returns SQLCODE=-805)

**Given** connection fails
**When** session starts
**Then** warning displayed, session continues in offline mode

**Complexity:** M
**Supports:** FR-v1.5-010

---

### Story 49.2: EXEC SQL Execution in Interpreter

As a **COBOL interpreter**,
I want **EXEC SQL statements routed to the DB2 runtime**,
So that **database operations execute during interactive sessions**.

**Acceptance Criteria:**

**Given** EXEC SQL SELECT ACCT_NAME INTO :WS-NAME FROM ACCOUNTS WHERE ACCT_ID = :WS-ID
**When** executed during a CICS session
**Then** SQL is executed, WS-NAME populated from result

**Given** SQL returns no rows
**When** SQLCODE checked
**Then** SQLCODE = +100

**Given** SQL syntax error
**When** executed
**Then** SQLCODE set to negative value, SQLCA populated

**Complexity:** L
**Supports:** FR-v1.5-010

---

### Story 49.3: Cursor Operations

As a **COBOL developer**,
I want **DECLARE CURSOR / OPEN / FETCH / CLOSE to work**,
So that **I can process result sets**.

**Acceptance Criteria:**

**Given** EXEC SQL DECLARE C1 CURSOR FOR SELECT ...
**When** OPEN C1 and FETCH C1 INTO :vars executed
**Then** each FETCH returns the next row

**Given** no more rows
**When** FETCH executed
**Then** SQLCODE = +100

**Given** CLOSE C1 executed
**When** cursor closed
**Then** cursor resources released

**Complexity:** M
**Supports:** FR-v1.5-010

---

### Story 49.4: SQL Dialect Translation

As a **DB2 runtime**,
I want **DB2 SQL translated to PostgreSQL**,
So that **DB2-specific syntax works against PostgreSQL**.

**Acceptance Criteria:**

**Given** DB2 FETCH FIRST n ROWS ONLY
**When** translated
**Then** PostgreSQL LIMIT n generated

**Given** DB2 DATE/TIME functions
**When** translated
**Then** PostgreSQL equivalents used

**Given** unsupported DB2 syntax
**When** translation fails
**Then** original SQL passed through with warning

**Complexity:** M
**Supports:** FR-v1.5-010

---

## Epic 50: CardDemo Interactive Integration

**Goal:** End-to-end verification of CardDemo running interactively.

**Crate:** All
**FRs:** All, NFR-v1.5-005

### Story 50.1: Sign-on Screen Test

As a **tester**,
I want **the CardDemo sign-on screen to render and accept input**,
So that **the basic flow works**.

**Acceptance Criteria:**

**Given** `zos-clone cics COSGN00C.cbl -I ./copybooks --data USRSEC=usrsec.dat:10:100`
**When** session starts
**Then** sign-on screen renders with User ID and Password fields

**Given** user types credentials and presses Enter
**When** RECEIVE MAP processes input
**Then** program validates credentials against USRSEC

**Given** valid credentials
**When** authentication succeeds
**Then** XCTL transfers to COMEN01C (main menu)

**Complexity:** M

---

### Story 50.2: Menu Navigation Test

As a **tester**,
I want **menu navigation between CardDemo screens**,
So that **the full application flow works**.

**Acceptance Criteria:**

**Given** main menu displayed (COMEN01C)
**When** user selects an option and presses Enter
**Then** the corresponding screen displays

**Given** user is on a sub-screen
**When** PF3 is pressed
**Then** user returns to the previous menu

**Given** user is on the main menu
**When** PF3 is pressed
**Then** session ends cleanly

**Complexity:** M

---

### Story 50.3: Automated Screen Snapshot Tests

As a **developer**,
I want **snapshot tests for CardDemo screens**,
So that **rendering regressions are caught**.

**Acceptance Criteria:**

**Given** a BMS map definition (e.g., COSGN00.bms)
**When** rendered to text
**Then** output matches the approved snapshot

**Given** a snapshot mismatch
**When** test runs
**Then** diff is displayed showing changed fields

**Given** all CardDemo BMS maps
**When** snapshot tests run
**Then** all maps render correctly

**Complexity:** M

---

### Story 50.4: End-to-End Session Test

As a **tester**,
I want **an automated end-to-end test of the sign-on flow**,
So that **CI can verify interactive functionality**.

**Acceptance Criteria:**

**Given** a test harness simulating terminal input
**When** sign-on credentials are provided
**Then** authentication succeeds and menu screen renders

**Given** the test harness sends PF3
**When** at the main menu
**Then** session ends with exit code 0

**Given** invalid credentials
**When** sign-on attempted
**Then** error message appears on sign-on screen

**Complexity:** L

---

## Implementation Order

### Phase 1: Foundation (Epics 44, 45)

1. Create `zos-tui` crate with ratatui/crossterm
2. Implement ScreenBuffer-to-widget rendering
3. Implement raw keyboard event loop
4. Implement field navigation (tab, character input)
5. Implement AID key mapping

### Phase 2: Core (Epics 46, 47)

1. Implement pseudo-conversational session state machine
2. Implement COMMAREA persistence
3. Create `cics` CLI subcommand
4. Wire SEND MAP / RECEIVE MAP to TUI
5. Implement XCTL within sessions

### Phase 3: Polish (Epic 48)

1. Field attribute rendering (bright, dark, protected)
2. Color theme support
3. Terminal capability detection
4. Cursor visual feedback

### Phase 4: Optional (Epic 49)

1. DB2 connection configuration
2. SQL execution integration
3. Cursor operations
4. SQL dialect translation

### Phase 5: Integration (Epic 50)

1. CardDemo sign-on test
2. Menu navigation test
3. Screen snapshot tests
4. Automated E2E tests
