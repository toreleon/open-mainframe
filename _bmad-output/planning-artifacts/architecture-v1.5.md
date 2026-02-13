---
version: 'v1.5'
baseVersion: 'v1.4'
date: '2026-02-13'
status: 'draft'
---

# Architecture Decision Document - v1.5: Interactive CICS Terminal

_Addendum to the base architecture document. All patterns, naming conventions, and enforcement guidelines from the base architecture remain in effect._

## New Crate: `zos-tui`

### Responsibility

Interactive terminal rendering and input handling for CICS applications. Bridges the existing `zos-cics` infrastructure (ScreenBuffer, TerminalManager, MapRenderer) to a real terminal via ratatui/crossterm.

### Updated Dependency Graph

```
                    ┌─────────────┐
                    │  zos-clone  │  (binary)
                    │    (CLI)    │
                    └──────┬──────┘
                           │
      ┌────────────────────┼────────────────────┐
      │                    │                    │
      ▼                    ▼                    ▼
┌───────────┐       ┌───────────┐        ┌───────────┐
│  zos-tui  │       │  zos-jcl  │        │zos-dataset│
│   (TUI)   │       │(interpret)│        │  (files)  │
└─────┬─────┘       └─────┬─────┘        └─────┬─────┘
      │                    │                    │
      ▼                    └────────┬───────────┘
┌───────────┐                      │
│ zos-cics  │◄─────────────────────┘
│  (CICS)   │
└─────┬─────┘
      │
      ▼
┌───────────┐    ┌───────────┐    ┌───────────┐
│ zos-cobol │    │zos-runtime│    │  zos-db2  │
│(compiler) │    │ (library) │    │  (SQL)    │
└─────┬─────┘    └─────┬─────┘    └───────────┘
      │                │
      └────────┬───────┘
               ▼
         ┌───────────┐
         │zos-encoding│
         │  (codec)  │
         └───────────┘
```

### Crate API

```rust
// zos-tui/src/lib.rs

/// Configuration for an interactive CICS session
pub struct SessionConfig {
    pub initial_program: String,
    pub include_paths: Vec<PathBuf>,
    pub data_files: Vec<DataFileSpec>,
    pub program_dir: Option<PathBuf>,
    pub transid_map: HashMap<String, String>,
    pub color_theme: ColorTheme,
    pub db2_url: Option<String>,
    pub userid: Option<String>,
}

/// Run an interactive CICS session
pub fn run_session(config: SessionConfig) -> Result<(), SessionError>;
```

### Internal Module Structure

```
crates/zos-tui/
├── Cargo.toml
├── src/
│   ├── lib.rs               # Public API: SessionConfig, run_session()
│   ├── session.rs            # Session state machine (SessionState enum)
│   ├── renderer.rs           # ScreenBuffer → ratatui Widget conversion
│   ├── input.rs              # Keyboard event processing, AID mapping
│   ├── fields.rs             # Field navigation, MDT tracking, editing
│   ├── styles.rs             # 3270 attribute → ratatui Style mapping
│   ├── color.rs              # Color themes and terminal capability detection
│   ├── status.rs             # Status line widget
│   └── error.rs              # SessionError type
└── tests/
    ├── renderer_tests.rs     # Snapshot tests for screen rendering
    ├── input_tests.rs        # Key mapping tests
    ├── fields_tests.rs       # Field navigation tests
    └── session_tests.rs      # Session state machine tests
```

### Cargo.toml

```toml
[package]
name = "zos-tui"
version.workspace = true
edition.workspace = true

[dependencies]
zos-cics = { path = "../zos-cics" }
zos-cobol = { path = "../zos-cobol" }
zos-db2 = { path = "../zos-db2", optional = true }
ratatui = "0.29"
crossterm = "0.28"
thiserror.workspace = true
tracing.workspace = true

[dev-dependencies]
insta = "1"  # Snapshot testing

[features]
default = []
db2 = ["dep:zos-db2"]
```

---

## Architectural Decisions

### Decision: TUI Library Selection

**Choice:** `ratatui` 0.29 + `crossterm` 0.28

**Alternatives Considered:**

| Option | Pros | Cons |
|--------|------|------|
| ratatui + crossterm | Most active Rust TUI, cross-platform, efficient diffing | Learning curve for custom widgets |
| cursive | Simpler API | Less active, less flexible rendering |
| termion | Lightweight | Linux-only, no Windows support |
| Raw crossterm only | Full control | Must implement all rendering logic |
| Web UI (HTML) | Rich graphics | Requires web server, browser dependency |

**Rationale:**
- ratatui provides differential rendering (only redraws changed cells), critical for <16ms latency
- crossterm handles all terminal quirks across platforms (Linux, macOS, WSL)
- Widget system allows composable UI: screen widget + status line widget
- Active community with regular releases

### Decision: Session Architecture

**Choice:** Single-threaded event loop with blocking I/O

**Implementation:**

```rust
pub enum SessionState {
    /// Program is executing COBOL statements
    Executing { program: String },
    /// Screen displayed, waiting for user input (AID key)
    WaitingForInput {
        pending_transid: Option<String>,
        commarea: Option<Vec<u8>>,
    },
    /// User pressed AID key, program will process input
    InputReceived { aid: u8 },
    /// Session is ending
    Ending { exit_code: i32 },
}

pub fn run_session(config: SessionConfig) -> Result<()> {
    let mut terminal = setup_terminal()?;
    let mut state = SessionState::Executing {
        program: config.initial_program.clone(),
    };

    loop {
        match &state {
            SessionState::Executing { .. } => {
                // Run COBOL interpreter until SEND MAP or RETURN
                state = execute_program(&mut terminal, &state)?;
            }
            SessionState::WaitingForInput { .. } => {
                // Render screen, wait for key press
                render(&mut terminal)?;
                let event = crossterm::event::read()?;
                state = process_input(event, &state)?;
            }
            SessionState::Ending { .. } => {
                break;
            }
            _ => { /* transition states */ }
        }
    }

    restore_terminal(&mut terminal)?;
    Ok(())
}
```

**Rationale:**
- CICS pseudo-conversational model is inherently sequential (execute → display → wait → repeat)
- No need for async/tokio overhead for a single-user terminal session
- Simpler error handling and state management
- Terminal I/O is blocking by nature

### Decision: CICS Bridge Integration Point

**Choice:** Callback-based bridge between interpreter and TUI

The COBOL interpreter calls CICS commands via the existing CicsBridge. For v1.5, the bridge gains a `TerminalCallback` trait that the TUI implements:

```rust
// In zos-cics
pub trait TerminalCallback {
    /// Called when SEND MAP is executed - render the screen
    fn on_send_map(&mut self, screen: &ScreenBuffer, options: &SendMapOptions) -> Result<()>;

    /// Called when RECEIVE MAP is executed - wait for input
    fn on_receive_map(&mut self, screen: &mut ScreenBuffer) -> Result<u8>; // Returns AID

    /// Called when SEND TEXT is executed
    fn on_send_text(&mut self, text: &str, erase: bool) -> Result<()>;

    /// Called when program RETURNs
    fn on_return(&mut self, transid: Option<&str>, commarea: Option<&[u8]>) -> Result<()>;
}
```

**Rationale:**
- Clean separation: zos-cics doesn't depend on zos-tui (no circular dependency)
- Existing non-interactive mode continues to work (different callback implementation)
- Testable: mock callbacks for unit tests

### Decision: Field Navigation Model

**Choice:** Field table with ordered index

```rust
pub struct FieldTable {
    fields: Vec<FieldEntry>,
    /// Indices of unprotected fields, in screen order
    input_order: Vec<usize>,
    /// Currently focused input field index (into input_order)
    active_field: Option<usize>,
}

pub struct FieldEntry {
    pub name: String,
    pub row: u16,
    pub col: u16,
    pub length: u16,
    pub attribute: FieldAttribute,
    pub modified: bool,  // MDT
    pub content: String,
}
```

**Rationale:**
- Pre-computed input order enables O(1) tab navigation
- MDT tracking per field enables efficient RECEIVE MAP (only return modified fields)
- Matches 3270 field buffer architecture

### Decision: Color Theme System

**Choice:** Named themes with TOML configuration

```toml
# Default theme: classic green-screen
[theme.classic]
background = "black"
protected = "blue"
unprotected = "green"
bright = "white"
dark = "black"     # Same as background = hidden
input_highlight = "reverse"
status_line = "cyan"
error = "red"

# Alternative: modern
[theme.modern]
background = "black"
protected = "gray"
unprotected = "white"
bright = "yellow"
# ...
```

**Rationale:**
- Named themes are user-friendly
- TOML matches project configuration convention
- Sensible defaults (classic 3270 green) require zero configuration

---

## Integration with Existing Infrastructure

### What Changes in Existing Crates

#### `zos-cics` Changes

1. **Add `TerminalCallback` trait** to `terminal/mod.rs`
2. **Modify `TerminalManager`** to accept a boxed callback: `Box<dyn TerminalCallback>`
3. **Existing non-interactive behavior** preserved via a `StdioCallback` implementation that prints to stdout (current behavior)

#### `zos-clone` Changes

1. **Add `cics` subcommand** to `commands/mod.rs` and `cli.rs`
2. **New file** `commands/cics.rs` for the interactive command handler
3. **Existing `interpret` command unchanged** (continues to work as before)

#### `zos-db2` Changes (Optional)

1. **Expose `Db2Runtime` initialization** from the library for use by `zos-tui`
2. **No structural changes** - existing runtime API sufficient

### What Does NOT Change

- `zos-cobol`: No changes (interpreter interface unchanged)
- `zos-encoding`: No changes
- `zos-dataset`: No changes
- `zos-runtime`: No changes
- `zos-jcl`: No changes

---

## Error Handling

### New Error Type

```rust
// zos-tui/src/error.rs
#[derive(Debug, Error, Diagnostic)]
pub enum SessionError {
    #[error("Terminal initialization failed: {0}")]
    TerminalInit(#[from] std::io::Error),

    #[error("Program not found: {name}")]
    ProgramNotFound { name: String },

    #[error("COBOL execution error: {0}")]
    Execution(#[from] CobolError),

    #[error("CICS error: {0}")]
    Cics(#[from] CicsError),

    #[error("Session interrupted by user")]
    Interrupted,

    #[error("DB2 connection failed: {0}")]
    Db2Connection(String),
}
```

### Terminal Restoration Guarantee

```rust
// Panic handler ensures terminal is always restored
fn setup_panic_handler() {
    let original_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let _ = crossterm::terminal::disable_raw_mode();
        let _ = crossterm::execute!(
            std::io::stdout(),
            crossterm::terminal::LeaveAlternateScreen,
            crossterm::cursor::Show
        );
        original_hook(info);
    }));
}
```

---

## Testing Strategy

### Unit Tests

| Component | Test Approach |
|-----------|--------------|
| `renderer.rs` | Snapshot tests comparing rendered output to golden files |
| `input.rs` | Key event → AID code mapping assertions |
| `fields.rs` | Field navigation sequences, MDT tracking |
| `session.rs` | State machine transition table tests |
| `styles.rs` | Attribute → Style mapping assertions |

### Integration Tests

| Test | Description |
|------|-------------|
| Sign-on render | Load COSGN00C, verify screen snapshot |
| Input simulation | Simulate keystrokes, verify field content |
| XCTL chain | Verify COSGN00C → COMEN01C transition |
| RETURN TRANSID | Verify pseudo-conversation round-trip |
| Error recovery | Verify ABEND handling and terminal restoration |

### Test Infrastructure

```rust
/// Mock terminal for testing without actual TUI
pub struct MockTerminal {
    screens: Vec<String>,  // Captured screen snapshots
    input_queue: VecDeque<crossterm::event::Event>,
}

impl TerminalCallback for MockTerminal {
    fn on_send_map(&mut self, screen: &ScreenBuffer, _opts: &SendMapOptions) -> Result<()> {
        self.screens.push(screen.to_text());
        Ok(())
    }

    fn on_receive_map(&mut self, _screen: &mut ScreenBuffer) -> Result<u8> {
        // Return next queued input
        // ...
    }
}
```

---

## Updated Workspace Cargo.toml

```toml
[workspace]
members = [
    "crates/zos-clone",
    "crates/zos-cobol",
    "crates/zos-cics",
    "crates/zos-tui",        # NEW
    "crates/zos-jcl",
    "crates/zos-runtime",
    "crates/zos-dataset",
    "crates/zos-encoding",
    "crates/zos-db2",
]

[workspace.dependencies]
# ... existing dependencies ...
ratatui = "0.29"             # NEW
crossterm = "0.28"           # NEW
```

---

## Implementation Priority

1. **zos-tui scaffold** - Crate creation, ratatui setup, terminal init/restore
2. **ScreenBuffer renderer** - Widget that displays the 24x80 grid
3. **Keyboard event loop** - Raw mode input, AID mapping
4. **Field navigation** - Tab, character input, MDT
5. **TerminalCallback trait** - Add to zos-cics, implement StdioCallback
6. **Session state machine** - Execute/WaitingForInput/Ending lifecycle
7. **`cics` CLI command** - Wire everything together
8. **SEND MAP integration** - Connect CicsBridge to TUI renderer
9. **RECEIVE MAP integration** - Connect TUI input to CicsBridge
10. **Attributes & color** - Visual styling
11. **DB2 integration** - Optional SQL execution
12. **CardDemo E2E tests** - Verification
