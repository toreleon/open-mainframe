//! TSO/E command processor — interactive dataset management, profile, and core commands.
//!
//! This crate implements the TSO (Time Sharing Option) command processor,
//! providing:
//!
//! - **Command Parser** — keyword/positional/flag extraction from TSO command lines
//! - **ALLOCATE/FREE** — dynamic dataset allocation and deallocation
//! - **LISTDS/LISTALC** — dataset information and allocation queries
//! - **PROFILE** — session settings (PREFIX, MSGID, WTPMSG)
//! - **DELETE/RENAME** — dataset management
//! - **ALTLIB** — alternative library search paths
//! - **HELP** — command help system
//! - **SUBMIT/STATUS/CANCEL/OUTPUT** — JES2 job management

pub mod commands;
pub mod error;
pub mod jobs;
pub mod parser;
pub mod session;

pub use commands::{execute, CommandResult};
pub use error::{TsoError, Result};
pub use parser::{parse_command, ParsedCommand};
pub use session::{
    AllocDisp, AllocEntry, AltlibEntry, AltlibLevel, AltlibType,
    DcbAttrs, TsoProfile, TsoSession,
};
