//! Interactive 3270 terminal UI for OpenMainframe CICS applications.
//!
//! This crate provides a TUI (Text User Interface) that renders 3270 terminal
//! screens in the user's terminal using ratatui/crossterm, enabling interactive
//! execution of CICS applications like CardDemo.
//!
//! # Architecture
//!
//! ```text
//! User Terminal (keyboard/screen)
//!         │
//!         ▼
//!     open-mainframe-tui          ← This crate: TUI rendering + input
//!         │
//!         ▼
//!     open-mainframe-cics          ← Existing: TerminalManager, ScreenBuffer, EIB
//!         │
//!         ▼
//!     open-mainframe-cobol         ← Existing: COBOL interpreter
//! ```
//!
//! # Usage
//!
//! ```ignore
//! use open_mainframe_tui::session::{SessionConfig, Session, setup_terminal, restore_terminal};
//!
//! let config = SessionConfig {
//!     initial_program: "COSGN00C.cbl".into(),
//!     include_paths: vec!["./copybooks".into()],
//!     ..Default::default()
//! };
//!
//! let session = Session::new(config);
//! // Session manages the TUI lifecycle
//! ```

pub mod color;
pub mod error;
pub mod event;
pub mod fields;
pub mod input;
pub mod mock;
pub mod oia;
pub mod renderer;
pub mod session;
pub mod status;
pub mod styles;

// Re-export key types for convenience.
pub use color::ColorTheme;
pub use error::SessionError;
pub use event::{CrosstermEventSource, EventSource, MockEventSource};
pub use session::{Session, SessionConfig, SessionState, TerminalModel};
