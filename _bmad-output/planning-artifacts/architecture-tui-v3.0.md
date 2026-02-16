# TUI Crate — Architecture Decisions

## AD-3.0-01: Multi-Model Screen Buffer

**Context:** The current implementation hardcodes a 24x80 (Model 2) screen buffer. Real 3270 terminals support multiple models with different dimensions, and CICS applications may request alternate screen sizes via EWA.

**Decision:** Make `ScreenBuffer` generic over dimensions by storing `rows` and `cols` as fields rather than constants. Add a `TerminalModel` enum (Model2, Model3, Model4, Model5) with default and alternate sizes. The `SessionConfig` gains a `terminal_model` field. Screen buffers are allocated at the configured size. The ratatui rendering area is dynamically sized to match the model dimensions, with scrolling or scaling if the physical terminal is smaller.

**Consequences:**
- Backward compatible — Model2 remains the default
- Larger models require more terminal real estate (Model 5 needs 132 columns)
- BMS maps designed for specific models may not render correctly on smaller models
- ASSIGN SCRNHT/SCRNWD returns the configured model's dimensions
- Status line positioning adjusts to screen height

## AD-3.0-02: PA Key Mapping via Ctrl Sequences

**Context:** PA1-PA3 keys have no standard mapping on modern keyboards. PF keys map naturally to F1-F12, but PA keys need a different approach.

**Decision:** Map PA keys to Alt+1 (PA1), Alt+2 (PA2), Alt+3 (PA3). These are unused in the current keymap and follow a mnemonic pattern. The `InputAction::AidKey` variant already supports arbitrary AID names, so PA keys are added as additional AID values. PA keys trigger a "short read" — only the AID byte and cursor address are sent, no field data.

**Consequences:**
- PA keys are accessible without special terminal configuration
- Short read behavior differs from Enter/PF keys (no field data)
- CICS HANDLE AID PA1/PA2/PA3 labels become functional
- Alt+key mappings may conflict with terminal emulator shortcuts on some platforms

## AD-3.0-03: Explicit Cursor Positioning

**Context:** Currently cursor position is set only via the IC (Initial Cursor) BMS attribute on the first unprotected field with IC set. CICS SEND MAP CURSOR(data-value) allows explicit cursor positioning to any screen coordinate.

**Decision:** Add a `cursor_position: Option<(u16, u16)>` field to the SEND MAP options. When present, the cursor is placed at the specified (row, col) after all field rendering. This overrides IC attribute positioning. The FieldTable gains a `set_cursor_position(row, col)` method that finds the field containing the position and sets the appropriate cursor offset.

**Consequences:**
- CURSOR option takes precedence over IC attribute
- Invalid coordinates (beyond screen bounds) are clamped to the nearest valid field position
- If the position is within a protected field, the cursor is placed at the next unprotected field
- Requires field table to support arbitrary cursor placement (not just field-start)

## AD-3.0-04: TN3270 as Optional Feature

**Context:** Adding TN3270 protocol support would allow the TUI to connect to actual z/OS systems. However, this is a complex protocol (RFC 2355) and is not needed for the primary use case of running local CICS applications.

**Decision:** Implement TN3270 as an optional feature behind a `tn3270` Cargo feature flag. The protocol module handles TCP connection, TN3270E negotiation, and data stream encoding/decoding. The existing `EventSource` trait is extended with a `TN3270EventSource` that reads from the network. The `Session` struct works identically regardless of whether input comes from local keyboard or TN3270 connection.

**Consequences:**
- No additional dependencies when feature is disabled
- TN3270 adds async I/O dependency (tokio or similar)
- Authentication (Telnet AUTH) and encryption (TLS) add complexity
- Testing requires a TN3270 server or mock
- Enables "remote terminal" mode for connecting to real z/OS
