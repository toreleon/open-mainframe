# TUI Crate — Product Requirements

## Overview

The `open-mainframe-tui` crate provides a terminal-based 3270 emulation layer for the OpenMainframe platform. It renders BMS maps from `open-mainframe-cics` as interactive terminal screens using `ratatui`, handles keyboard input mapped to 3270 AID keys, manages field-level editing with MDT tracking, and supports pseudo-conversational CICS sessions.

## Current State Assessment

- **Lines of code:** 2,408
- **Test count:** 43 (all passing, including 10 integration tests)
- **Maturity:** Moderate (core 3270 emulation complete, advanced features missing)
- **Files:** 11 Rust source files across session, rendering, input, and testing modules

### What Works Well

**3270 Screen Rendering:**
- 24x80 screen buffer rendering via ratatui
- Attribute byte handling (protected, unprotected, bright, dark)
- Extended color attributes (8 BMS colors: Blue, Red, Pink, Green, Turquoise, Yellow, White)
- Extended highlight attributes (Blink, Reverse, Underscore)
- Two rendering paths: ScreenBuffer-based and FieldTable-based
- Status line with cursor position, program name, TRANSID, and messages

**Field Management:**
- BMS map parsing to field table
- Modified Data Tag (MDT) tracking for RECEIVE MAP
- Tab navigation (forward, backward, home)
- Cursor movement (up, down, left, right)
- Character input with validation (numeric fields reject alpha)
- Autoskip on field full
- Erase EOF (Ctrl+U)
- FRSET (field reset) for clearing MDT flags

**Keyboard/AID Key Support:**
- Full AID key mapping: Enter, Clear (Esc), PF1-PF24 (F1-F12, Shift+F1-F12)
- Navigation: Tab, BackTab (Shift+Tab), Home, End, arrows
- Editing: Backspace, Delete, EraseEof
- Ctrl+C termination

**CICS Integration:**
- SEND MAP with options: ERASE, ERASEAUP, DATAONLY, MAPONLY, FRSET, CURSOR
- SEND TEXT support
- RECEIVE MAP with field data collection
- Pseudo-conversational session state machine (Executing, WaitingForInput, InputReceived, Ending)
- TRANSID→program mapping

**Testing Infrastructure:**
- MockEventSource for keyboard input simulation
- MockTerminal for CICS callback verification
- Snapshot tests for rendering
- 43 tests including integration tests

**Color Themes:**
- Classic green-screen, modern, and monochrome themes
- Environment-based color support detection (NO_COLOR, TERM)
- Configurable via SessionConfig

### What Does NOT Work

- Only Model 2 (24x80) screen size — no Model 3 (32x80), Model 4 (43x80), Model 5 (27x132)
- No alternate screen size support (EWA command)
- No PA1-PA3 AID key mapping (only PF keys and Enter/Clear)
- No structured field support (WSF, Query Reply)
- No field validation attributes (MUSTENTER, MUSTFILL, TRIGGER)
- No field outlining attribute
- No light pen detection
- No DBCS field rendering (SO/SI double-byte characters)
- No cursor addressing via CURSOR option coordinates (only IC attribute)
- No printer support (LU Type 3)
- No file transfer (IND$FILE)
- No operator information area (OIA) emulation
- No 3270 data stream protocol (no actual TN3270 connection)
- No ATTRB(ASKIP) visual distinction from PROT in rendering

## Functional Requirements

### FR-v3.0-900: Extended Screen Size Support
Support 3270 terminal models beyond Model 2. Implement Model 3 (32x80), Model 4 (43x80), and Model 5 (27x132). Support dynamic screen size selection via configuration and EWA (Erase/Write Alternate) command.
- **Priority:** MAJOR
- **Gap Type:** Incomplete implementation (hardcoded 24x80)
- **IBM Reference:** 3270 Data Stream Programmer's Reference GA23-0059 — Models 2-5 have different default and alternate screen sizes. CICS ASSIGN SCRNHT/SCRNWD returns the current model's dimensions.

### FR-v3.0-901: PA Key Support
Map PA1, PA2, PA3 attention keys. PA keys send the AID byte without field data (short read), used for system functions and CICS HANDLE AID.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** 3270 Data Stream Programmer's Reference — PA keys (Program Attention) are AID keys that send only the cursor address, not field data. Used extensively in CICS applications for "no data" signaling.

### FR-v3.0-902: Field Validation Attributes
Implement MUSTENTER (field must be modified before AID), MUSTFILL (all positions must be filled), and TRIGGER (cause automatic AID when field is filled) validation attributes.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** 3270 Extended Attributes — Field validation attributes enforce data entry rules at the terminal level, reducing server-side validation load.

### FR-v3.0-903: Cursor Positioning via SEND MAP CURSOR Option
Implement explicit cursor positioning using the CURSOR(row, col) option on SEND MAP, in addition to the existing IC (Initial Cursor) attribute support.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** CICS Application Programming Reference — SEND MAP CURSOR(data-value) positions the cursor at the specified screen position. This supplements IC attribute positioning.

### FR-v3.0-904: TN3270 Protocol Support
Implement the TN3270/TN3270E telnet protocol for connecting to actual mainframe CICS regions or 3270 terminal emulators. Support inbound and outbound data stream processing.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** RFC 2355 (TN3270E) — TN3270 encapsulates 3270 data streams over TCP/IP. Enables connection to z/OS CICS, TSO, and ISPF.

### FR-v3.0-905: Structured Field Support (WSF/Query Reply)
Implement Write Structured Field (WSF) command processing and Query Reply for device capability negotiation. Support Read Partition, Create Partition, and Erase/Reset.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** 3270 Data Stream Programmer's Reference Chapter 5 — Structured fields enable extended features like oversize screens, partitions, and device queries.

### FR-v3.0-906: Operator Information Area (OIA)
Implement the OIA status indicators: system lock, keyboard lock, insert mode, input inhibited, connection status, and wait indicators.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** 3270 Data Stream Programmer's Reference — The OIA is a reserved line at the bottom of the 3270 screen showing terminal status indicators.

### FR-v3.0-907: DBCS Field Rendering
Support rendering of DBCS (Double-Byte Character Set) fields with SO/SI shift codes for CJK language support in 3270 screens.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** 3270 Data Stream Programmer's Reference — DBCS fields use SO (0x0E) and SI (0x0F) to delimit double-byte character regions within fields.
