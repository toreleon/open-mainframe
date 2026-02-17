//! Keyboard event processing and AID key mapping.
//!
//! Maps terminal keyboard events (via crossterm) to 3270 AID codes
//! and field editing operations.

use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use open_mainframe_cics::runtime::eib::aid;

/// Actions that result from keyboard input.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InputAction {
    /// A printable character to insert at the cursor.
    Character(char),
    /// An AID key was pressed (Enter, PFn, PA, CLEAR).
    AidKey(u8),
    /// Tab to next unprotected field.
    Tab,
    /// Back-tab to previous unprotected field.
    BackTab,
    /// Backspace: delete character before cursor.
    Backspace,
    /// Delete: delete character at cursor.
    Delete,
    /// Home: move to first unprotected field.
    Home,
    /// End: move to end of current field data.
    End,
    /// Arrow key movement.
    CursorUp,
    CursorDown,
    CursorLeft,
    CursorRight,
    /// Erase to end of field (Ctrl+U or similar).
    EraseEof,
    /// Session termination requested (Ctrl+C).
    Terminate,
    /// No action (ignore this event).
    None,
}

/// Map a crossterm keyboard event to a TUI InputAction.
pub fn map_key_event(event: &Event) -> InputAction {
    match event {
        Event::Key(KeyEvent {
            code, modifiers, ..
        }) => map_key_code(code, modifiers),
        _ => InputAction::None,
    }
}

fn map_key_code(code: &KeyCode, modifiers: &KeyModifiers) -> InputAction {
    // Check for Ctrl combinations first
    if modifiers.contains(KeyModifiers::CONTROL) {
        return match code {
            KeyCode::Char('c') => InputAction::Terminate,
            KeyCode::Char('u') => InputAction::EraseEof,
            _ => InputAction::None,
        };
    }

    // Alt+1/2/3 → PA1/PA2/PA3
    if modifiers.contains(KeyModifiers::ALT) {
        return match code {
            KeyCode::Char('1') => InputAction::AidKey(aid::PA1),
            KeyCode::Char('2') => InputAction::AidKey(aid::PA2),
            KeyCode::Char('3') => InputAction::AidKey(aid::PA3),
            _ => InputAction::None,
        };
    }

    match code {
        // Printable characters
        KeyCode::Char(c) => InputAction::Character(*c),

        // AID keys
        KeyCode::Enter => InputAction::AidKey(aid::ENTER),
        KeyCode::Esc => InputAction::AidKey(aid::CLEAR),

        // Function keys F1-F12
        KeyCode::F(1) => InputAction::AidKey(aid::PF1),
        KeyCode::F(2) => InputAction::AidKey(aid::PF2),
        KeyCode::F(3) => InputAction::AidKey(aid::PF3),
        KeyCode::F(4) => InputAction::AidKey(aid::PF4),
        KeyCode::F(5) => InputAction::AidKey(aid::PF5),
        KeyCode::F(6) => InputAction::AidKey(aid::PF6),
        KeyCode::F(7) => InputAction::AidKey(aid::PF7),
        KeyCode::F(8) => InputAction::AidKey(aid::PF8),
        KeyCode::F(9) => InputAction::AidKey(aid::PF9),
        KeyCode::F(10) => InputAction::AidKey(aid::PF10),
        KeyCode::F(11) => InputAction::AidKey(aid::PF11),
        KeyCode::F(12) => InputAction::AidKey(aid::PF12),

        // PF13-PF24 via Shift+F1-F12 (some terminals send F13-F24 directly)
        KeyCode::F(n) if *n >= 13 && *n <= 24 => {
            // Map F13->PF13, etc.
            let pf_codes: [u8; 12] = [
                0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0x4A, 0x4B, 0x4C,
            ];
            InputAction::AidKey(pf_codes[(*n - 13) as usize])
        }

        // Navigation
        KeyCode::Tab => {
            if modifiers.contains(KeyModifiers::SHIFT) {
                InputAction::BackTab
            } else {
                InputAction::Tab
            }
        }
        KeyCode::BackTab => InputAction::BackTab,
        KeyCode::Home => InputAction::Home,
        KeyCode::End => InputAction::End,
        KeyCode::Up => InputAction::CursorUp,
        KeyCode::Down => InputAction::CursorDown,
        KeyCode::Left => InputAction::CursorLeft,
        KeyCode::Right => InputAction::CursorRight,

        // Editing
        KeyCode::Backspace => InputAction::Backspace,
        KeyCode::Delete => InputAction::Delete,

        _ => InputAction::None,
    }
}

/// Get a human-readable name for an AID code.
pub fn aid_name(aid: u8) -> &'static str {
    match aid {
        aid::ENTER => "ENTER",
        aid::CLEAR => "CLEAR",
        aid::PA1 => "PA1",
        aid::PA2 => "PA2",
        aid::PA3 => "PA3",
        aid::PF1 => "PF1",
        aid::PF2 => "PF2",
        aid::PF3 => "PF3",
        aid::PF4 => "PF4",
        aid::PF5 => "PF5",
        aid::PF6 => "PF6",
        aid::PF7 => "PF7",
        aid::PF8 => "PF8",
        aid::PF9 => "PF9",
        aid::PF10 => "PF10",
        aid::PF11 => "PF11",
        aid::PF12 => "PF12",
        _ => "UNKNOWN",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crossterm::event::{KeyEventKind, KeyEventState};

    fn make_key_event(code: KeyCode, modifiers: KeyModifiers) -> Event {
        Event::Key(KeyEvent {
            code,
            modifiers,
            kind: KeyEventKind::Press,
            state: KeyEventState::NONE,
        })
    }

    #[test]
    fn test_enter_maps_to_aid() {
        let event = make_key_event(KeyCode::Enter, KeyModifiers::NONE);
        assert_eq!(map_key_event(&event), InputAction::AidKey(aid::ENTER));
    }

    #[test]
    fn test_pf3_maps_to_aid() {
        let event = make_key_event(KeyCode::F(3), KeyModifiers::NONE);
        assert_eq!(map_key_event(&event), InputAction::AidKey(aid::PF3));
    }

    #[test]
    fn test_escape_maps_to_clear() {
        let event = make_key_event(KeyCode::Esc, KeyModifiers::NONE);
        assert_eq!(map_key_event(&event), InputAction::AidKey(aid::CLEAR));
    }

    #[test]
    fn test_ctrl_c_terminates() {
        let event = make_key_event(KeyCode::Char('c'), KeyModifiers::CONTROL);
        assert_eq!(map_key_event(&event), InputAction::Terminate);
    }

    #[test]
    fn test_character_input() {
        let event = make_key_event(KeyCode::Char('A'), KeyModifiers::NONE);
        assert_eq!(map_key_event(&event), InputAction::Character('A'));
    }

    #[test]
    fn test_tab_navigation() {
        let event = make_key_event(KeyCode::Tab, KeyModifiers::NONE);
        assert_eq!(map_key_event(&event), InputAction::Tab);

        let event = make_key_event(KeyCode::BackTab, KeyModifiers::NONE);
        assert_eq!(map_key_event(&event), InputAction::BackTab);
    }

    #[test]
    fn test_aid_name() {
        assert_eq!(aid_name(aid::ENTER), "ENTER");
        assert_eq!(aid_name(aid::PF3), "PF3");
        assert_eq!(aid_name(aid::CLEAR), "CLEAR");
        assert_eq!(aid_name(aid::PA1), "PA1");
        assert_eq!(aid_name(aid::PA2), "PA2");
        assert_eq!(aid_name(aid::PA3), "PA3");
    }

    #[test]
    fn test_pa_key_mapping() {
        // Alt+1 → PA1
        let event = make_key_event(KeyCode::Char('1'), KeyModifiers::ALT);
        assert_eq!(map_key_event(&event), InputAction::AidKey(aid::PA1));

        // Alt+2 → PA2
        let event = make_key_event(KeyCode::Char('2'), KeyModifiers::ALT);
        assert_eq!(map_key_event(&event), InputAction::AidKey(aid::PA2));

        // Alt+3 → PA3
        let event = make_key_event(KeyCode::Char('3'), KeyModifiers::ALT);
        assert_eq!(map_key_event(&event), InputAction::AidKey(aid::PA3));
    }

    #[test]
    fn test_alt_other_keys_ignored() {
        // Alt+4 should produce no action
        let event = make_key_event(KeyCode::Char('4'), KeyModifiers::ALT);
        assert_eq!(map_key_event(&event), InputAction::None);
    }
}
