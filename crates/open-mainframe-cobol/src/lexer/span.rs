//! Source location tracking for error reporting.
//!
//! These types are now defined in [`open_mainframe_lang_core`] and re-exported
//! here for backward compatibility. All language crates share the same
//! `Span`, `FileId`, and `Location` types.
//!
//! Tests for these types live in `open-mainframe-lang-core`.

pub use open_mainframe_lang_core::{offset_to_line_col, FileId, Location, Span};
