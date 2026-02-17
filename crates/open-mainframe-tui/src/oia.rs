//! Operator Information Area (OIA) for 3270 terminal emulation.
//!
//! The OIA is the bottom status line of a 3270 terminal, displaying
//! indicators for system lock, keyboard state, insert mode, connection
//! status, and cursor position.

use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::widgets::Widget;

use crate::color::ColorTheme;

/// Keyboard lock state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum KeyboardLock {
    /// Keyboard is unlocked (normal input).
    #[default]
    Unlocked,
    /// System lock — waiting for host response.
    SystemLock,
    /// Input inhibited (e.g., field validation error).
    InputInhibited,
    /// Machine check / error state.
    MachineCheck,
}

/// Connection state.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum ConnectionState {
    /// Not connected.
    #[default]
    Disconnected,
    /// Connected with the given LU name.
    Connected(String),
}

/// OIA state — all the information displayed in the Operator Information Area.
#[derive(Debug, Clone, Default)]
pub struct OiaState {
    /// Keyboard lock indicator.
    pub keyboard_lock: KeyboardLock,
    /// Insert mode active.
    pub insert_mode: bool,
    /// Connection state (LU name if connected).
    pub connection: ConnectionState,
    /// Cursor row (1-based).
    pub cursor_row: usize,
    /// Cursor col (1-based).
    pub cursor_col: usize,
    /// Communication error code (if any).
    pub comm_error: Option<String>,
    /// Current screen number (for multi-screen sessions).
    pub screen_number: u8,
}

impl OiaState {
    /// Create a new OIA with unlocked keyboard and connected status.
    pub fn connected(lu_name: &str) -> Self {
        Self {
            connection: ConnectionState::Connected(lu_name.to_string()),
            ..Default::default()
        }
    }

    /// Lock the keyboard (system lock — waiting for host).
    pub fn lock_keyboard(&mut self) {
        self.keyboard_lock = KeyboardLock::SystemLock;
    }

    /// Unlock the keyboard.
    pub fn unlock_keyboard(&mut self) {
        self.keyboard_lock = KeyboardLock::Unlocked;
    }

    /// Set input inhibited (e.g., field validation failure).
    pub fn inhibit_input(&mut self) {
        self.keyboard_lock = KeyboardLock::InputInhibited;
    }

    /// Toggle insert mode.
    pub fn toggle_insert(&mut self) {
        self.insert_mode = !self.insert_mode;
    }

    /// Update cursor position.
    pub fn set_cursor(&mut self, row: usize, col: usize) {
        self.cursor_row = row;
        self.cursor_col = col;
    }

    /// Check if the keyboard is locked.
    pub fn is_locked(&self) -> bool {
        self.keyboard_lock != KeyboardLock::Unlocked
    }

    /// Get the keyboard lock indicator text.
    pub fn lock_indicator(&self) -> &str {
        match self.keyboard_lock {
            KeyboardLock::Unlocked => "",
            KeyboardLock::SystemLock => "X SYSTEM",
            KeyboardLock::InputInhibited => "X INPUT",
            KeyboardLock::MachineCheck => "X MACH",
        }
    }

    /// Get the insert mode indicator.
    pub fn insert_indicator(&self) -> &str {
        if self.insert_mode { "^" } else { "" }
    }

    /// Get the connection indicator.
    pub fn connection_indicator(&self) -> String {
        match &self.connection {
            ConnectionState::Disconnected => "4 DISCONN".to_string(),
            ConnectionState::Connected(lu) => {
                if lu.is_empty() {
                    "4 ONLINE".to_string()
                } else {
                    format!("4 {}", lu)
                }
            }
        }
    }
}

/// Widget that renders the OIA line.
pub struct OiaLine<'a> {
    state: &'a OiaState,
    theme: &'a ColorTheme,
}

impl<'a> OiaLine<'a> {
    pub fn new(state: &'a OiaState, theme: &'a ColorTheme) -> Self {
        Self { state, theme }
    }
}

impl Widget for OiaLine<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        if area.height == 0 || area.width == 0 {
            return;
        }

        let style = Style::default()
            .fg(self.theme.status_fg)
            .bg(self.theme.status_bg);

        // Fill background
        for x in area.x..area.right() {
            buf[(x, area.y)].set_char(' ').set_style(style);
        }

        let width = area.width as usize;
        let mut line = vec![b' '; width];

        // Position 0-7: Keyboard lock indicator
        let lock = self.state.lock_indicator();
        for (i, ch) in lock.bytes().enumerate() {
            if i < 8 && i < width {
                line[i] = ch;
            }
        }

        // Position 10: Insert mode indicator
        if self.state.insert_mode && width > 10 {
            line[10] = b'^';
        }

        // Position 13-24: Connection indicator
        let conn = self.state.connection_indicator();
        for (i, ch) in conn.bytes().enumerate() {
            let pos = 13 + i;
            if pos < width {
                line[pos] = ch;
            }
        }

        // Right side: cursor position (Row Col)
        let cursor = format!("{:03}/{:03}", self.state.cursor_row, self.state.cursor_col);
        let cursor_start = width.saturating_sub(cursor.len() + 1);
        for (i, ch) in cursor.bytes().enumerate() {
            let pos = cursor_start + i;
            if pos < width {
                line[pos] = ch;
            }
        }

        // Render the line
        let lock_end = lock.len().min(width);
        for (i, &byte) in line.iter().enumerate() {
            let x = area.x + i as u16;
            if x < area.right() {
                let cell_style = if i < lock_end && !lock.is_empty() {
                    // Lock indicator in bold/error color
                    Style::default()
                        .fg(self.theme.error)
                        .bg(self.theme.status_bg)
                        .add_modifier(Modifier::BOLD)
                } else {
                    style
                };
                buf[(x, area.y)]
                    .set_char(byte as char)
                    .set_style(cell_style);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_oia_default_unlocked() {
        let oia = OiaState::default();
        assert_eq!(oia.keyboard_lock, KeyboardLock::Unlocked);
        assert!(!oia.insert_mode);
        assert!(!oia.is_locked());
        assert_eq!(oia.lock_indicator(), "");
    }

    #[test]
    fn test_oia_system_lock() {
        let mut oia = OiaState::default();
        oia.lock_keyboard();
        assert!(oia.is_locked());
        assert_eq!(oia.lock_indicator(), "X SYSTEM");
    }

    #[test]
    fn test_oia_input_inhibited() {
        let mut oia = OiaState::default();
        oia.inhibit_input();
        assert!(oia.is_locked());
        assert_eq!(oia.lock_indicator(), "X INPUT");
    }

    #[test]
    fn test_oia_unlock() {
        let mut oia = OiaState::default();
        oia.lock_keyboard();
        oia.unlock_keyboard();
        assert!(!oia.is_locked());
        assert_eq!(oia.lock_indicator(), "");
    }

    #[test]
    fn test_oia_insert_mode() {
        let mut oia = OiaState::default();
        assert_eq!(oia.insert_indicator(), "");
        oia.toggle_insert();
        assert!(oia.insert_mode);
        assert_eq!(oia.insert_indicator(), "^");
        oia.toggle_insert();
        assert!(!oia.insert_mode);
    }

    #[test]
    fn test_oia_connected() {
        let oia = OiaState::connected("LU001");
        assert_eq!(oia.connection_indicator(), "4 LU001");
    }

    #[test]
    fn test_oia_disconnected() {
        let oia = OiaState::default();
        assert_eq!(oia.connection_indicator(), "4 DISCONN");
    }

    #[test]
    fn test_oia_cursor_position() {
        let mut oia = OiaState::default();
        oia.set_cursor(5, 42);
        assert_eq!(oia.cursor_row, 5);
        assert_eq!(oia.cursor_col, 42);
    }

    #[test]
    fn test_oia_machine_check() {
        let mut oia = OiaState::default();
        oia.keyboard_lock = KeyboardLock::MachineCheck;
        assert!(oia.is_locked());
        assert_eq!(oia.lock_indicator(), "X MACH");
    }
}
