//! Terminal state and operations.
//!
//! Represents a 3270 terminal with screen buffer and input handling.

use super::screen::{ScreenBuffer, ScreenPosition};
use crate::bms::{BmsField, BmsMap, MapRenderer, ScreenSize};
use crate::runtime::eib::aid;
use crate::{CicsError, CicsResult};
use std::collections::HashMap;

/// Terminal state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TerminalState {
    /// Terminal is ready for input
    #[default]
    Ready,
    /// Waiting for input
    WaitingForInput,
    /// Input received
    InputReceived,
    /// Error state
    Error,
}

/// A 3270 terminal.
pub struct Terminal {
    /// Terminal ID
    id: String,
    /// Screen buffer
    screen: ScreenBuffer,
    /// Current state
    state: TerminalState,
    /// Current map name
    current_map: Option<String>,
    /// Last AID received
    last_aid: u8,
    /// Input data from fields
    input_fields: HashMap<String, Vec<u8>>,
    /// Raw input buffer
    input_buffer: Vec<u8>,
    /// Field positions (name -> position)
    field_positions: HashMap<String, (ScreenPosition, usize)>,
}

impl Terminal {
    /// Create a new terminal.
    pub fn new(id: &str, size: ScreenSize) -> Self {
        Self {
            id: id.to_uppercase(),
            screen: ScreenBuffer::new(size),
            state: TerminalState::Ready,
            current_map: None,
            last_aid: aid::NO_AID,
            input_fields: HashMap::new(),
            input_buffer: Vec::new(),
            field_positions: HashMap::new(),
        }
    }

    /// Get terminal ID.
    pub fn id(&self) -> &str {
        &self.id
    }

    /// Get screen size.
    pub fn screen_size(&self) -> ScreenSize {
        self.screen.screen_size()
    }

    /// Get current state.
    pub fn state(&self) -> TerminalState {
        self.state
    }

    /// Get current map name.
    pub fn current_map(&self) -> Option<&str> {
        self.current_map.as_deref()
    }

    /// Set current map name.
    pub fn set_current_map(&mut self, name: Option<String>) {
        self.current_map = name;
    }

    /// Get last AID.
    pub fn last_aid(&self) -> u8 {
        self.last_aid
    }

    /// Clear the screen.
    pub fn clear_screen(&mut self) {
        self.screen.clear();
        self.field_positions.clear();
        self.current_map = None;
    }

    /// Apply a BMS map to the screen.
    pub fn apply_map(
        &mut self,
        map: &BmsMap,
        renderer: &MapRenderer,
        cursor: Option<ScreenPosition>,
    ) -> CicsResult<()> {
        let (_, cols) = self.screen.dimensions();

        for field in &map.fields {
            // Write attribute
            let attr_pos = ScreenPosition::from_offset(field.attribute_position(cols), cols);
            self.screen.write_attribute(attr_pos, field.attributes.to_attribute_byte());

            // Write field data
            let field_pos = ScreenPosition::new(field.row, field.column);
            let data = self.get_field_display_data(field, renderer);
            self.screen.write_string(field_pos, &data);

            // Store field position
            if !field.name.is_empty() {
                self.field_positions.insert(
                    field.name.to_uppercase(),
                    (field_pos, field.length)
                );
            }

            // Set cursor if field has IC attribute
            if field.attributes.initial_cursor {
                self.screen.set_cursor(field_pos);
            }
        }

        // Override cursor if specified
        if let Some(pos) = cursor {
            self.screen.set_cursor(pos);
        }

        self.state = TerminalState::WaitingForInput;
        Ok(())
    }

    fn get_field_display_data(&self, field: &BmsField, _renderer: &MapRenderer) -> Vec<u8> {
        // This is a simplified version - in practice would use the renderer
        if let Some(initial) = &field.initial {
            let mut data = initial.as_bytes().to_vec();
            data.resize(field.length, b' ');
            data
        } else {
            vec![b' '; field.length]
        }
    }

    /// Extract map data from screen buffer.
    pub fn extract_map_data(&self, map: &BmsMap) -> CicsResult<HashMap<String, Vec<u8>>> {
        let mut data = HashMap::new();

        for field in &map.fields {
            if field.name.is_empty() {
                continue;
            }

            // Check if we have input data for this field
            if let Some(input) = self.input_fields.get(&field.name.to_uppercase()) {
                data.insert(field.name.clone(), input.clone());
            } else {
                // Read from screen buffer
                let pos = ScreenPosition::new(field.row, field.column);
                let value = self.screen.read_range(pos, field.length);
                data.insert(field.name.clone(), value);
            }
        }

        Ok(data)
    }

    /// Write text to screen.
    pub fn write_text(&mut self, text: &str, _header: bool) -> CicsResult<()> {
        // Simple implementation - write text starting at current cursor
        let cursor = self.screen.cursor();
        self.screen.write_string(cursor, text.as_bytes());
        self.state = TerminalState::WaitingForInput;
        Ok(())
    }

    /// Get raw input data.
    pub fn get_input_data(&mut self, max_length: usize) -> CicsResult<Vec<u8>> {
        if self.state != TerminalState::InputReceived {
            return Err(CicsError::InvalidRequest("No input available".to_string()));
        }

        let mut data = self.input_buffer.clone();
        data.truncate(max_length);

        self.state = TerminalState::Ready;
        Ok(data)
    }

    /// Set raw simulated input (for testing CONVERSE/RECEIVE).
    ///
    /// Sets the AID and raw input buffer directly, bypassing field mapping.
    pub fn set_raw_input(&mut self, aid: u8, data: Vec<u8>) {
        self.last_aid = aid;
        self.input_buffer = data;
        self.state = TerminalState::InputReceived;
    }

    /// Set simulated input (for testing).
    pub fn set_input(&mut self, aid: u8, fields: HashMap<String, Vec<u8>>) {
        self.last_aid = aid;
        self.input_fields = fields;
        self.state = TerminalState::InputReceived;

        // Build raw input buffer from fields
        self.input_buffer.clear();
        for (name, value) in &self.input_fields {
            if let Some(&(pos, len)) = self.field_positions.get(name) {
                // Format: SBA + position + data
                let (_, cols) = self.screen.dimensions();
                let offset = pos.to_offset(cols);
                self.input_buffer.push(0x11); // SBA
                self.input_buffer.push(((offset >> 6) & 0x3F) as u8 | 0x40);
                self.input_buffer.push((offset & 0x3F) as u8 | 0x40);

                for &b in value.iter().take(len) {
                    self.input_buffer.push(b);
                }
            }
        }
    }

    /// Write to screen buffer without changing terminal state.
    ///
    /// Used by CONVERSE to write output while preserving input readiness.
    pub fn screen_write_string(&mut self, start: ScreenPosition, data: &[u8]) {
        self.screen.write_string(start, data);
    }

    /// Get cursor position.
    pub fn cursor(&self) -> ScreenPosition {
        self.screen.cursor()
    }

    /// Get screen as text (for debugging).
    pub fn screen_text(&self) -> String {
        self.screen.to_text()
    }
}

impl Default for Terminal {
    fn default() -> Self {
        Self::new("TERM", ScreenSize::Model2)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bms::BmsParser;

    fn create_test_map() -> BmsMap {
        let source = r#"
TEST     DFHMSD TYPE=MAP,LANG=COBOL
TESTM    DFHMDI SIZE=(24,80)
LABEL    DFHMDF POS=(1,1),LENGTH=10,ATTRB=(PROT),INITIAL='Name:'
NAME     DFHMDF POS=(1,12),LENGTH=20,ATTRB=(UNPROT,IC)
         DFHMSD TYPE=FINAL
"#;
        let mut parser = BmsParser::new();
        let mapset = parser.parse(source).unwrap();
        mapset.maps[0].clone()
    }

    #[test]
    fn test_terminal_creation() {
        let term = Terminal::new("T001", ScreenSize::Model2);
        assert_eq!(term.id(), "T001");
        assert_eq!(term.state(), TerminalState::Ready);
    }

    #[test]
    fn test_apply_map() {
        let mut term = Terminal::new("T001", ScreenSize::Model2);
        let map = create_test_map();
        let renderer = MapRenderer::new(ScreenSize::Model2);

        term.apply_map(&map, &renderer, None).unwrap();

        assert_eq!(term.current_map(), None); // Must be set explicitly
        assert_eq!(term.state(), TerminalState::WaitingForInput);

        // Check cursor is at NAME field (IC attribute)
        assert_eq!(term.cursor(), ScreenPosition::new(1, 12));
    }

    #[test]
    fn test_clear_screen() {
        let mut term = Terminal::new("T001", ScreenSize::Model2);
        term.set_current_map(Some("TEST".to_string()));

        term.clear_screen();

        assert_eq!(term.current_map(), None);
        assert!(term.field_positions.is_empty());
    }

    #[test]
    fn test_set_input() {
        let mut term = Terminal::new("T001", ScreenSize::Model2);

        let mut fields = HashMap::new();
        fields.insert("NAME".to_string(), b"John Doe".to_vec());

        term.set_input(aid::ENTER, fields);

        assert_eq!(term.last_aid(), aid::ENTER);
        assert_eq!(term.state(), TerminalState::InputReceived);
    }

    #[test]
    fn test_extract_map_data() {
        let mut term = Terminal::new("T001", ScreenSize::Model2);
        let map = create_test_map();
        let renderer = MapRenderer::new(ScreenSize::Model2);

        // Apply map
        term.apply_map(&map, &renderer, None).unwrap();
        term.set_current_map(Some(map.name.clone()));

        // Set input
        let mut fields = HashMap::new();
        fields.insert("NAME".to_string(), b"Test User".to_vec());
        term.set_input(aid::ENTER, fields);

        // Extract
        let data = term.extract_map_data(&map).unwrap();

        assert_eq!(data.get("NAME").map(|v| v.as_slice()), Some(b"Test User".as_slice()));
    }

    // === Story 203.2: Dynamic terminal screen size ===

    #[test]
    fn test_terminal_screen_size_model2() {
        let term = Terminal::new("T001", ScreenSize::Model2);
        assert_eq!(term.screen_size(), ScreenSize::Model2);
        assert_eq!(term.screen_size().dimensions(), (24, 80));
    }

    #[test]
    fn test_terminal_screen_size_model3() {
        let term = Terminal::new("T001", ScreenSize::Model3);
        assert_eq!(term.screen_size(), ScreenSize::Model3);
        assert_eq!(term.screen_size().dimensions(), (32, 80));
    }

    #[test]
    fn test_terminal_screen_size_model4() {
        let term = Terminal::new("T001", ScreenSize::Model4);
        assert_eq!(term.screen_size(), ScreenSize::Model4);
        assert_eq!(term.screen_size().dimensions(), (43, 80));
    }

    #[test]
    fn test_terminal_screen_size_model5() {
        let term = Terminal::new("T001", ScreenSize::Model5);
        assert_eq!(term.screen_size(), ScreenSize::Model5);
        assert_eq!(term.screen_size().dimensions(), (27, 132));
    }

    #[test]
    fn test_write_text() {
        let mut term = Terminal::new("T001", ScreenSize::Model2);

        term.write_text("Hello, World!", false).unwrap();

        assert_eq!(term.state(), TerminalState::WaitingForInput);
        let screen = term.screen_text();
        assert!(screen.contains("Hello, World!"));
    }
}
