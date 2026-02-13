---
version: 'v1.5'
baseVersion: 'v1.4'
date: '2026-02-13'
status: 'draft'
---

# Product Requirements Document - zOS-clone v1.5: Interactive CICS Terminal

## Overview

**Goal:** Provide a fully interactive 3270 terminal experience for running CICS applications (starting with CardDemo) through the terminal, enabling real-time screen display, keyboard input, field navigation, and multi-program conversation flows.

v1.5 builds directly on v1.4's CardDemo compatibility by connecting the existing internal 3270 infrastructure (ScreenBuffer, TerminalManager, MapRenderer, BMS parser) to an actual interactive terminal display using a TUI (Text User Interface) library.

### Goals

1. **Interactive Terminal Display**: Render BMS maps as visual 3270 screens in the user's terminal with proper field positioning, attributes, and colors
2. **Real-Time Keyboard Input**: Capture keystrokes including function keys (PF1-24, PA1-3, CLEAR, ENTER) and route them as AID codes to CICS programs
3. **Pseudo-Conversational Flow**: Support the full RETURN TRANSID cycle so users can navigate through CardDemo screens naturally
4. **Field-Level Interaction**: Tab between input fields, enforce field attributes (protected, numeric, autoskip), and track Modified Data Tags (MDT)
5. **DB2 SQL Execution**: Integrate DB2 runtime so CardDemo programs that query databases function during interactive sessions

### Prerequisites

- v1.4 complete (all 29 CardDemo programs compile, sign-on flow executes)
- Existing infrastructure: ScreenBuffer, Terminal, TerminalManager, MapRenderer, BmsParser, EIB, CicsBridge
- XCTL program chaining and COMMAREA passing operational

---

## CardDemo Interactive Flow Analysis

### Target User Experience

```
┌──────────────────────────────────────────────────────────────────────────────┐
│  CardDemo Sign-on                                              zOS-clone    │
│                                                                             │
│                    AWS Mainframe Modernization                               │
│                        CardDemo Application                                 │
│                                                                             │
│                                                                             │
│  User ID  . . . : [________]                                                │
│  Password . . . : [________]                                                │
│                                                                             │
│                                                                             │
│  Enter=Sign On   PF3=Exit                                                   │
│                                                                             │
│                                                                             │
│                                                                             │
│                                                                             │
│                                                                             │
│                                                                             │
│                                                                             │
│                                                                             │
│                                                                             │
│                                                                             │
│                                                                             │
│  Message: Welcome to CardDemo                                               │
└──────────────────────────────────────────────────────────────────────────────┘
  F1=Help  F3=Exit  F7=Back  F8=Forward                         Row 7  Col 21
```

### Interaction Flow

1. User starts: `zos-clone cics COSGN00C.cbl -I ./copybooks --data USRSEC=usrsec.dat:10:100`
2. COSGN00C executes, issues SEND MAP → TUI renders sign-on screen
3. User types credentials in input fields, presses ENTER
4. RECEIVE MAP captures input → COSGN00C validates → XCTL to COMEN01C
5. COMEN01C issues SEND MAP → TUI renders main menu
6. User presses PF key → navigates to next program
7. Cycle continues until user presses PF3 at top menu → RETURN exits

---

## Functional Requirements

### FR-v1.5-001: TUI Screen Renderer

Render the 3270 ScreenBuffer to the user's terminal using a TUI library.

**Acceptance Criteria:**

- 24x80 character grid rendered with proper field positioning
- Protected fields displayed with appropriate visual style (dim/normal)
- Bright fields rendered with bold/highlight
- Dark fields hidden from display
- Initial field values displayed
- Screen title and status line rendered
- ERASE option clears screen before rendering

### FR-v1.5-002: Field Navigation

Support tabbing between unprotected input fields.

**Acceptance Criteria:**

- Tab key advances cursor to next unprotected field
- Shift-Tab (or equivalent) moves to previous unprotected field
- Autoskip fields advance cursor automatically when full
- Cursor wraps from last field to first field
- IC (Insert Cursor) attribute positions cursor on SEND MAP

### FR-v1.5-003: Keyboard Input Capture

Capture all keystrokes in raw terminal mode and map to 3270 conventions.

**Acceptance Criteria:**

- Printable characters entered into current field at cursor position
- Enter key generates AID code DFHENTER (0x7D)
- F1-F12 mapped to DFHPF1-DFHPF12
- Shift+F1-F12 (or configurable alt keys) mapped to DFHPF13-DFHPF24
- Escape or configurable key mapped to DFHCLEAR
- Backspace deletes character in current field
- Delete clears character at cursor
- Home moves cursor to first unprotected field

### FR-v1.5-004: AID Key Processing

Route AID keys to the CICS program via EIB.

**Acceptance Criteria:**

- EIBAID set to pressed key's AID code before RECEIVE MAP returns
- Modified Data Tag (MDT) tracked for each field
- Only modified fields returned to program on RECEIVE MAP
- SEND MAP with ERASEAUP resets all MDTs

### FR-v1.5-005: Pseudo-Conversational Session Manager

Manage the RETURN TRANSID / next-input cycle for multi-screen applications.

**Acceptance Criteria:**

- After RETURN TRANSID('xxxx') COMMAREA(data), session waits for next AID key
- On AID key press, the program associated with TRANSID is loaded
- COMMAREA data passed to new program invocation
- EIBCALEN reflects COMMAREA length
- Session persists until RETURN without TRANSID (conversation end)
- XCTL within a conversation transfers control without user interaction

### FR-v1.5-006: Interactive CICS Command (`cics` subcommand)

New CLI subcommand for interactive CICS execution.

**Acceptance Criteria:**

- `zos-clone cics <program> [options]` launches interactive session
- `-I <path>` for copybook include paths
- `--data <DDNAME=file:key_len:rec_len>` for VSAM data files
- `--program-dir <path>` for locating XCTL target programs
- `--transid <id>` for initial transaction ID
- `--userid <id>` for ASSIGN USERID
- Session exits cleanly on conversation end or Ctrl+C

### FR-v1.5-007: Screen Attribute Rendering

Map 3270 field attributes to terminal visual styles.

**Acceptance Criteria:**

- ASKIP (autoskip) fields: dim text, not editable
- BRT (bright) fields: bold text
- DRK (dark) fields: invisible (for passwords)
- PROT (protected) fields: not editable, normal text
- UNPROT (unprotected) fields: editable, underline or reverse video
- NUM (numeric) fields: reject non-numeric input
- FSET (field set) fields: MDT set automatically

### FR-v1.5-008: Status Line Display

Show terminal status information below the 3270 screen area.

**Acceptance Criteria:**

- Current program name displayed
- Current transaction ID displayed
- Cursor position (row, column) displayed
- Key mapping hints displayed (F1=Help, F3=Exit, etc.)
- Error/info messages from runtime displayed

### FR-v1.5-009: SEND TEXT Rendering

Render EXEC CICS SEND TEXT output in the terminal.

**Acceptance Criteria:**

- SEND TEXT FROM(data) LENGTH(len) displays text content
- ERASE clears screen first
- FREEKB unlocks keyboard for input
- Text wraps at column 80
- Subsequent RECEIVE waits for any AID key

### FR-v1.5-010: DB2 Runtime Integration

Execute EXEC SQL statements during interactive CICS sessions.

**Acceptance Criteria:**

- EXEC SQL SELECT INTO populates host variables during program execution
- EXEC SQL OPEN CURSOR / FETCH / CLOSE support for result sets
- SQLCA status codes set after each SQL operation
- Connection configured via `--db2-url <connection-string>` or config file
- Graceful fallback when no DB2 connection configured (SQLCODE=-805 or similar)

### FR-v1.5-011: SEND MAP DATAONLY Support

Support partial screen updates without resending static fields.

**Acceptance Criteria:**

- SEND MAP DATAONLY sends only variable data fields
- Static text (INITIAL values, labels) preserved from previous SEND
- MAPONLY sends only static map content without variable data
- Combines correctly with ERASE and CURSOR options

### FR-v1.5-012: Color Support

Render extended 3270 color attributes when the terminal supports color.

**Acceptance Criteria:**

- Default color: green on black (classic 3270)
- Protected field color: blue or cyan
- Input field color: green
- Error/bright field color: red or white
- Color mappable via configuration
- Graceful fallback to monochrome attributes

---

## Non-Functional Requirements

### NFR-v1.5-001: Input Latency

Keyboard input must feel instantaneous.

| Metric | Target |
|--------|--------|
| Key-to-screen latency | <16ms (single frame) |
| AID key to SEND MAP render | <100ms |
| XCTL program load | <500ms |

### NFR-v1.5-002: Terminal Compatibility

Support common terminal emulators.

| Terminal | Support Level |
|----------|--------------|
| Linux console | Full |
| xterm/xterm-256color | Full |
| Windows Terminal (WSL) | Full |
| macOS Terminal.app | Full |
| iTerm2 | Full |
| tmux/screen | Full (with proper TERM) |
| SSH sessions | Full |

### NFR-v1.5-003: Graceful Degradation

Handle terminal capability differences.

- Detect color support via TERM environment variable
- Fall back to monochrome if 256-color not available
- Handle missing function key support with alternative key bindings
- Restore terminal state on exit (normal mode, cursor visible, etc.)

### NFR-v1.5-004: Resource Usage

Interactive sessions must be lightweight.

| Metric | Target |
|--------|--------|
| Memory per session | <50MB |
| CPU idle (waiting for input) | <1% |
| Binary size increase | <5MB |

### NFR-v1.5-005: Test Coverage

Automated testing for terminal interaction.

| Metric | Target |
|--------|--------|
| Unit test coverage (new code) | 80%+ |
| Integration test: sign-on flow | Automated |
| Integration test: menu navigation | Automated |
| Snapshot tests for screen rendering | All BMS maps |

---

## Out of Scope for v1.5

- TN3270/TN3270E network protocol (wire protocol for remote 3270 terminals)
- Web-based HTML renderer
- REST API mode
- Multi-user concurrent sessions
- CICS transaction server (full TS emulation)
- Printing support (3270 print orders)
- Light pen / selector pen emulation
- Partitioned screen support (multiple logical screens)

---

## Technical Approach

### TUI Library: `ratatui` + `crossterm`

**Rationale:**

- `ratatui` is the most actively maintained Rust TUI framework (successor to `tui-rs`)
- `crossterm` provides cross-platform terminal manipulation (Linux, macOS, Windows)
- Raw mode input capture for function keys
- Efficient differential rendering (only redraws changed cells)
- Widget system maps well to 3270 screen concepts

**Dependencies:**

```toml
ratatui = "0.29"
crossterm = "0.28"
```

### Architecture: New `zos-tui` Crate

A new crate `zos-tui` bridges the existing `zos-cics` infrastructure to the terminal:

```
User Terminal (keyboard/screen)
        │
        ▼
    zos-tui          ← New crate: TUI rendering + input
        │
        ▼
    zos-cics          ← Existing: TerminalManager, ScreenBuffer, EIB
        │
        ▼
    zos-cobol         ← Existing: COBOL interpreter
```

### Session Lifecycle

```
1. CLI parse → load initial program
2. Interpret program → hits SEND MAP
3. ScreenBuffer updated → zos-tui renders to terminal
4. Wait for input → user types, tabs, presses AID key
5. Input captured → zos-tui populates ScreenBuffer modified fields
6. RECEIVE MAP returns → program processes input
7. Program may XCTL → load next program → goto 2
8. Program may RETURN TRANSID → wait for input → goto 4 with new program
9. Program RETURNs without TRANSID → session ends
```

---

## Success Criteria

1. **Visual Display:** CardDemo sign-on screen (COSGN00C) renders correctly with all fields positioned
2. **User Input:** User can type credentials, tab between fields, and press Enter
3. **Authentication:** Sign-on validates against USRSEC file and shows success/error
4. **Navigation:** Main menu (COMEN01C) renders after successful sign-on via XCTL
5. **Conversation:** Full pseudo-conversational flow works (RETURN TRANSID → next screen)
6. **Exit:** PF3 at main menu exits the session cleanly
7. **Terminal Restore:** Terminal returns to normal mode after session ends

---

## Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|------------|
| Function key mapping varies across terminals | Medium | Configurable key bindings, document common mappings |
| ratatui rendering performance for full 24x80 refresh | Low | Differential rendering already built into ratatui |
| Complex field attribute combinations | Medium | Start with subset used by CardDemo, extend iteratively |
| Pseudo-conversation state management | High | Build on existing RETURN TRANSID in CicsBridge |
| DB2 integration complexity | Medium | Optional feature, graceful fallback when not configured |
| Terminal state corruption on crash | Medium | Panic handler to restore terminal, Ctrl+C handler |

---

## Dependencies

### Internal

- `zos-cics`: TerminalManager, ScreenBuffer, MapRenderer, EIB, CicsBridge
- `zos-cobol`: COBOL interpreter for program execution
- `zos-db2`: SQL execution during CICS transactions (optional)
- `zos-dataset`: VSAM file access for data files

### External (New)

- `ratatui` ~0.29: TUI framework for screen rendering
- `crossterm` ~0.28: Cross-platform terminal manipulation

### External (Existing)

- `clap`: CLI argument parsing
- `miette`: Error diagnostics
- `tracing`: Structured logging
