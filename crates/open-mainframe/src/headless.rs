//! Headless JSON protocol for programmatic CICS testing.
//!
//! When `--headless` is passed on the CLI, the TUI is replaced by a line-based
//! JSON protocol on stdin/stdout.  Each screen the CICS application displays
//! is emitted as a single JSON line; the caller responds with a JSON line
//! containing the AID key and field values.

use std::collections::HashMap;
use std::io::{self, BufRead, Write};

use serde::{Deserialize, Serialize};
use tracing::debug;

use open_mainframe_cics::runtime::eib::aid;
use open_mainframe_tui::session::Session;

// ---------------------------------------------------------------------------
// Output types (stdout)
// ---------------------------------------------------------------------------

/// A complete screen snapshot sent to the caller.
#[derive(Debug, Clone, Serialize)]
pub struct ScreenOutput {
    /// Always `"screen"`.
    #[serde(rename = "type")]
    pub kind: String,
    /// Current CICS program name.
    pub program: String,
    /// Current transaction ID (may be empty).
    pub transid: String,
    /// Screen dimensions `[rows, cols]`.
    pub size: [usize; 2],
    /// Cursor position.
    pub cursor: CursorPosition,
    /// Status-line message (may be empty).
    pub message: String,
    /// All fields currently on the screen.
    pub fields: Vec<FieldOutput>,
}

/// Cursor row/col (1-based).
#[derive(Debug, Clone, Serialize)]
pub struct CursorPosition {
    pub row: usize,
    pub col: usize,
}

/// A single field on the screen.
#[derive(Debug, Clone, Serialize)]
pub struct FieldOutput {
    pub name: String,
    pub row: usize,
    pub col: usize,
    pub length: usize,
    pub value: String,
    pub protected: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub color: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub highlight: Option<String>,
}

/// Session-end message sent to the caller.
#[derive(Debug, Clone, Serialize)]
pub struct EndOutput {
    /// `"end"` or `"error"`.
    #[serde(rename = "type")]
    pub kind: String,
    /// Reason: `"return"`, `"abend"`, `"error"`, etc.
    pub reason: String,
    /// Program that was running when the session ended.
    pub program: String,
    /// Optional human-readable message.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
}

// ---------------------------------------------------------------------------
// Input type (stdin)
// ---------------------------------------------------------------------------

/// One line of input from the caller.
#[derive(Debug, Deserialize)]
pub struct HeadlessInput {
    /// AID key name: `"Enter"`, `"PF1"`–`"PF12"`, `"PA1"`–`"PA3"`, `"Clear"`.
    pub aid: String,
    /// Field values to set before submitting.  Keys are field names, values
    /// are the text to place in each field.
    #[serde(default)]
    pub fields: HashMap<String, String>,
}

// ---------------------------------------------------------------------------
// Public helpers
// ---------------------------------------------------------------------------

/// Capture the current screen state from the session as a `ScreenOutput`.
pub fn capture_screen(session: &Session) -> ScreenOutput {
    let ft = session.field_table();
    let (rows, cols) = session.screen_dimensions();

    let cursor = ft
        .cursor_position()
        .map(|p| CursorPosition { row: p.row, col: p.col })
        .unwrap_or(CursorPosition { row: 1, col: 1 });

    let fields: Vec<FieldOutput> = ft
        .fields()
        .iter()
        .filter(|f| f.length > 0)
        .map(|f| {
            let value = String::from_utf8_lossy(&f.content).to_string();
            let color = f.color.as_ref().map(|c| format!("{:?}", c));
            let highlight = f.highlight.as_ref().map(|h| format!("{:?}", h));
            // Unnamed BMS fields (labels/static text) get a positional name
            let name = if f.name.is_empty() {
                format!("_R{}C{}", f.row, f.col)
            } else {
                f.name.clone()
            };
            FieldOutput {
                name,
                row: f.row,
                col: f.col,
                length: f.length,
                value,
                protected: f.attribute.is_protected(),
                color,
                highlight,
            }
        })
        .collect();

    ScreenOutput {
        kind: "screen".to_string(),
        program: session.current_program().to_string(),
        transid: session.transid().to_string(),
        size: [rows, cols],
        cursor,
        message: session.message().to_string(),
        fields,
    }
}

/// Read one JSON line from stdin and parse it as `HeadlessInput`.
pub fn read_headless_input() -> Result<HeadlessInput, String> {
    let stdin = io::stdin();
    let mut line = String::new();
    stdin
        .lock()
        .read_line(&mut line)
        .map_err(|e| format!("Failed to read stdin: {e}"))?;

    if line.is_empty() {
        return Err("EOF on stdin — no more input".to_string());
    }

    let input: HeadlessInput = serde_json::from_str(line.trim())
        .map_err(|e| format!("Invalid JSON input: {e}"))?;
    debug!(aid = %input.aid, fields = input.fields.len(), "Headless input received");
    Ok(input)
}

/// Write a serializable value as a single JSON line to stdout.
pub fn print_json(value: &impl Serialize) {
    let json = serde_json::to_string(value).expect("JSON serialization failed");
    let stdout = io::stdout();
    let mut out = stdout.lock();
    let _ = writeln!(out, "{}", json);
    let _ = out.flush();
}

/// Convert an AID name (e.g. `"Enter"`, `"PF3"`) to the 3270 AID byte.
pub fn aid_from_name(name: &str) -> Result<u8, String> {
    match name {
        "Enter" => Ok(aid::ENTER),
        "PF1" => Ok(aid::PF1),
        "PF2" => Ok(aid::PF2),
        "PF3" => Ok(aid::PF3),
        "PF4" => Ok(aid::PF4),
        "PF5" => Ok(aid::PF5),
        "PF6" => Ok(aid::PF6),
        "PF7" => Ok(aid::PF7),
        "PF8" => Ok(aid::PF8),
        "PF9" => Ok(aid::PF9),
        "PF10" => Ok(aid::PF10),
        "PF11" => Ok(aid::PF11),
        "PF12" => Ok(aid::PF12),
        "PA1" => Ok(aid::PA1),
        "PA2" => Ok(aid::PA2),
        "PA3" => Ok(aid::PA3),
        "Clear" => Ok(aid::CLEAR),
        _ => Err(format!("Unknown AID key: {name}")),
    }
}

/// Convert a `HeadlessInput` into the (aid_byte, field_data) tuple that
/// `CicsBridge::set_pending_input()` expects.
pub fn input_to_pending(input: &HeadlessInput) -> Result<(u8, HashMap<String, Vec<u8>>), String> {
    let aid_byte = aid_from_name(&input.aid)?;
    let fields: HashMap<String, Vec<u8>> = input
        .fields
        .iter()
        .map(|(k, v)| (k.to_uppercase(), v.as_bytes().to_vec()))
        .collect();
    Ok((aid_byte, fields))
}
