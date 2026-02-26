# open-mainframe-tui

Interactive 3270 terminal User Interface (TUI) for the OpenMainframe project.

## Features

- **3270 Rendering** — Renders 3270 terminal screens in the user's terminal using `ratatui` and `crossterm`.
- **Interactive Session** — Manages the lifecycle of an interactive session, handling keyboard input and screen updates.
- **CICS Integration** — Primary interface for executing CICS applications like CardDemo interactively.
- **DBCS Support** — Support for Double-Byte Character Sets.
- **OIA & Status** — Implementation of the Operator Information Area and terminal status indicators.
- **TN3270 Support** — Emulation of TN3270 protocol characteristics.

## Architecture

```text
User Terminal (keyboard/screen)
        │
        ▼
    open-mainframe-tui          ← This crate: TUI rendering + input
        │
        ▼
    open-mainframe-cics          ← CICS TerminalManager, ScreenBuffer
        │
        ▼
    open-mainframe-cobol         ← COBOL interpreter
```

## Usage

```rust
use open_mainframe_tui::session::{SessionConfig, Session};

// Implementation details...
```
