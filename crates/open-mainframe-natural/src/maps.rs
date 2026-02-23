// SPDX-License-Identifier: Apache-2.0
//! NAT-107: Interactive I/O & Maps for Natural.
//!
//! Provides INPUT/REINPUT statement support, MapDefinition with field
//! positions, attributes and variable bindings, INPUT USING MAP,
//! PF key handling, and a TerminalSimulator for testing.

use std::collections::HashMap;

use crate::data_model::NaturalValue;

// ---------------------------------------------------------------------------
// Field attributes
// ---------------------------------------------------------------------------

/// Display attributes for a map field.
#[derive(Debug, Clone, PartialEq)]
pub enum FieldAttribute {
    /// Normal intensity
    Normal,
    /// High intensity (bright)
    Intensified,
    /// Non-display (password)
    NonDisplay,
    /// Protected (output only)
    Protected,
    /// Modifiable (input)
    Modifiable,
    /// Detectable (AD=D)
    Detectable,
}

/// Color constants.
#[derive(Debug, Clone, PartialEq)]
pub enum FieldColor {
    Default,
    Blue,
    Red,
    Pink,
    Green,
    Turquoise,
    Yellow,
    White,
}

// ---------------------------------------------------------------------------
// Map field
// ---------------------------------------------------------------------------

/// A field on a Natural map.
#[derive(Debug, Clone)]
pub struct MapField {
    pub name: String,
    pub row: u16,
    pub col: u16,
    pub length: u16,
    pub attribute: FieldAttribute,
    pub color: FieldColor,
    pub variable: String,
    pub prompt: Option<String>,
    pub initial_value: Option<String>,
}

impl MapField {
    pub fn new(name: &str, row: u16, col: u16, length: u16, variable: &str) -> Self {
        Self {
            name: name.to_string(),
            row,
            col,
            length,
            attribute: FieldAttribute::Modifiable,
            color: FieldColor::Default,
            variable: variable.to_string(),
            prompt: None,
            initial_value: None,
        }
    }

    pub fn with_attribute(mut self, attr: FieldAttribute) -> Self {
        self.attribute = attr;
        self
    }

    pub fn with_color(mut self, color: FieldColor) -> Self {
        self.color = color;
        self
    }

    pub fn with_prompt(mut self, prompt: &str) -> Self {
        self.prompt = Some(prompt.to_string());
        self
    }

    pub fn with_initial_value(mut self, value: &str) -> Self {
        self.initial_value = Some(value.to_string());
        self
    }

    pub fn is_input(&self) -> bool {
        self.attribute == FieldAttribute::Modifiable
    }

    pub fn is_output(&self) -> bool {
        self.attribute == FieldAttribute::Protected
    }
}

// ---------------------------------------------------------------------------
// Map definition
// ---------------------------------------------------------------------------

/// A complete Natural map definition.
#[derive(Debug, Clone)]
pub struct MapDefinition {
    pub name: String,
    pub fields: Vec<MapField>,
    pub width: u16,
    pub height: u16,
    pub title: Option<String>,
    pub help_text: Option<String>,
}

impl MapDefinition {
    pub fn new(name: &str, width: u16, height: u16) -> Self {
        Self {
            name: name.to_string(),
            fields: Vec::new(),
            width,
            height,
            title: None,
            help_text: None,
        }
    }

    pub fn add_field(&mut self, field: MapField) {
        self.fields.push(field);
    }

    /// Get all input (modifiable) fields.
    pub fn input_fields(&self) -> Vec<&MapField> {
        self.fields.iter().filter(|f| f.is_input()).collect()
    }

    /// Get all output (protected) fields.
    pub fn output_fields(&self) -> Vec<&MapField> {
        self.fields.iter().filter(|f| f.is_output()).collect()
    }

    /// Get field by variable name.
    pub fn get_field_by_variable(&self, var: &str) -> Option<&MapField> {
        self.fields.iter().find(|f| f.variable == var)
    }

    /// Get field by name.
    pub fn get_field(&self, name: &str) -> Option<&MapField> {
        self.fields.iter().find(|f| f.name == name)
    }
}

// ---------------------------------------------------------------------------
// Terminal simulator
// ---------------------------------------------------------------------------

/// PF key codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PfKey {
    Enter,
    Pf1, Pf2, Pf3, Pf4, Pf5, Pf6,
    Pf7, Pf8, Pf9, Pf10, Pf11, Pf12,
    Pf13, Pf14, Pf15, Pf16, Pf17, Pf18,
    Pf19, Pf20, Pf21, Pf22, Pf23, Pf24,
}

impl PfKey {
    /// Parse a PF key number (1-24) into a `PfKey`.
    pub fn from_number(n: u8) -> Option<Self> {
        match n {
            1 => Some(PfKey::Pf1), 2 => Some(PfKey::Pf2), 3 => Some(PfKey::Pf3),
            4 => Some(PfKey::Pf4), 5 => Some(PfKey::Pf5), 6 => Some(PfKey::Pf6),
            7 => Some(PfKey::Pf7), 8 => Some(PfKey::Pf8), 9 => Some(PfKey::Pf9),
            10 => Some(PfKey::Pf10), 11 => Some(PfKey::Pf11), 12 => Some(PfKey::Pf12),
            13 => Some(PfKey::Pf13), 14 => Some(PfKey::Pf14), 15 => Some(PfKey::Pf15),
            16 => Some(PfKey::Pf16), 17 => Some(PfKey::Pf17), 18 => Some(PfKey::Pf18),
            19 => Some(PfKey::Pf19), 20 => Some(PfKey::Pf20), 21 => Some(PfKey::Pf21),
            22 => Some(PfKey::Pf22), 23 => Some(PfKey::Pf23), 24 => Some(PfKey::Pf24),
            _ => None,
        }
    }

    /// Get the *PF-KEY system variable value for this key.
    pub fn to_sysvar_value(&self) -> i32 {
        match self {
            PfKey::Enter => 0,
            PfKey::Pf1 => 1, PfKey::Pf2 => 2, PfKey::Pf3 => 3,
            PfKey::Pf4 => 4, PfKey::Pf5 => 5, PfKey::Pf6 => 6,
            PfKey::Pf7 => 7, PfKey::Pf8 => 8, PfKey::Pf9 => 9,
            PfKey::Pf10 => 10, PfKey::Pf11 => 11, PfKey::Pf12 => 12,
            PfKey::Pf13 => 13, PfKey::Pf14 => 14, PfKey::Pf15 => 15,
            PfKey::Pf16 => 16, PfKey::Pf17 => 17, PfKey::Pf18 => 18,
            PfKey::Pf19 => 19, PfKey::Pf20 => 20, PfKey::Pf21 => 21,
            PfKey::Pf22 => 22, PfKey::Pf23 => 23, PfKey::Pf24 => 24,
        }
    }
}

/// Simulated terminal for testing map-based I/O.
pub struct TerminalSimulator {
    /// Queued input values: (variable_name, value)
    input_queue: Vec<(String, String)>,
    /// Last PF key pressed
    pub last_pf_key: PfKey,
    /// Output screen buffer
    pub screen: Vec<Vec<char>>,
    pub screen_width: u16,
    pub screen_height: u16,
    /// Reinput state
    pub reinput_message: Option<String>,
    pub reinput_mark_field: Option<String>,
}

impl TerminalSimulator {
    pub fn new(width: u16, height: u16) -> Self {
        let screen = vec![vec![' '; width as usize]; height as usize];
        Self {
            input_queue: Vec::new(),
            last_pf_key: PfKey::Enter,
            screen,
            screen_width: width,
            screen_height: height,
            reinput_message: None,
            reinput_mark_field: None,
        }
    }

    /// Queue an input value for a variable.
    pub fn queue_input(&mut self, variable: &str, value: &str) {
        self.input_queue.push((variable.to_string(), value.to_string()));
    }

    /// Set the PF key that will be "pressed".
    pub fn set_pf_key(&mut self, key: PfKey) {
        self.last_pf_key = key;
    }

    /// Process INPUT statement: return collected variable values.
    pub fn process_input(&mut self, prompt: Option<&str>, vars: &[String]) -> HashMap<String, NaturalValue> {
        let mut result = HashMap::new();
        if let Some(p) = prompt {
            self.write_to_screen(0, 0, p);
        }
        for var in vars {
            let value = self.get_queued_input(var);
            result.insert(var.clone(), NaturalValue::Alpha(value));
        }
        result
    }

    /// Process INPUT USING MAP: render map and collect input.
    pub fn process_map(&mut self, map: &MapDefinition) -> HashMap<String, NaturalValue> {
        let mut result = HashMap::new();

        // Render the map
        if let Some(ref title) = map.title {
            self.write_to_screen(0, 0, title);
        }

        for field in &map.fields {
            // Render prompt
            if let Some(ref prompt) = field.prompt {
                let col = field.col.saturating_sub(prompt.len() as u16 + 1);
                self.write_to_screen(field.row, col, prompt);
            }

            // Render initial/current value
            if let Some(ref init) = field.initial_value {
                self.write_to_screen(field.row, field.col, init);
            }

            // Collect input
            if field.is_input() {
                let value = self.get_queued_input(&field.variable);
                result.insert(field.variable.clone(), NaturalValue::Alpha(value));
            }
        }

        result
    }

    /// Process REINPUT: show error message and mark field.
    pub fn process_reinput(&mut self, message: &str, mark_field: Option<&str>) {
        self.reinput_message = Some(message.to_string());
        self.reinput_mark_field = mark_field.map(|s| s.to_string());
        // Write message to top of screen
        self.write_to_screen(0, 0, message);
    }

    fn get_queued_input(&mut self, variable: &str) -> String {
        if let Some(pos) = self.input_queue.iter().position(|(v, _)| v == variable) {
            self.input_queue.remove(pos).1
        } else {
            String::new()
        }
    }

    fn write_to_screen(&mut self, row: u16, col: u16, text: &str) {
        let r = row as usize;
        let c = col as usize;
        if r < self.screen.len() {
            for (i, ch) in text.chars().enumerate() {
                let pos = c + i;
                if pos < self.screen[r].len() {
                    self.screen[r][pos] = ch;
                }
            }
        }
    }

    /// Get a line of the screen as a string.
    pub fn get_screen_line(&self, row: u16) -> String {
        if (row as usize) < self.screen.len() {
            self.screen[row as usize].iter().collect()
        } else {
            String::new()
        }
    }
}

impl Default for TerminalSimulator {
    fn default() -> Self {
        Self::new(80, 24)
    }
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, thiserror::Error)]
pub enum MapError {
    #[error("map not found: {0}")]
    MapNotFound(String),
    #[error("field not found: {0}")]
    FieldNotFound(String),
    #[error("invalid position: row {row}, col {col}")]
    InvalidPosition { row: u16, col: u16 },
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_map_field_creation() {
        let field = MapField::new("F-NAME", 3, 15, 20, "#NAME");
        assert_eq!(field.name, "F-NAME");
        assert_eq!(field.row, 3);
        assert!(field.is_input());
        assert!(!field.is_output());
    }

    #[test]
    fn test_map_field_protected() {
        let field = MapField::new("F-LABEL", 1, 1, 10, "#LABEL")
            .with_attribute(FieldAttribute::Protected);
        assert!(field.is_output());
        assert!(!field.is_input());
    }

    #[test]
    fn test_map_field_builder() {
        let field = MapField::new("F-PWD", 5, 15, 8, "#PASSWORD")
            .with_attribute(FieldAttribute::NonDisplay)
            .with_color(FieldColor::Green)
            .with_prompt("Password:");
        assert_eq!(field.attribute, FieldAttribute::NonDisplay);
        assert_eq!(field.color, FieldColor::Green);
        assert_eq!(field.prompt.as_deref(), Some("Password:"));
    }

    #[test]
    fn test_map_definition() {
        let mut map = MapDefinition::new("EMPL-MAP", 80, 24);
        map.add_field(MapField::new("F-NAME", 3, 15, 20, "#NAME"));
        map.add_field(MapField::new("F-DEPT", 4, 15, 5, "#DEPT"));
        map.add_field(MapField::new("F-SALARY", 5, 15, 10, "#SALARY")
            .with_attribute(FieldAttribute::Protected));

        assert_eq!(map.fields.len(), 3);
        assert_eq!(map.input_fields().len(), 2);
        assert_eq!(map.output_fields().len(), 1);
    }

    #[test]
    fn test_map_get_field() {
        let mut map = MapDefinition::new("TEST", 80, 24);
        map.add_field(MapField::new("F-X", 1, 1, 5, "#X"));
        assert!(map.get_field("F-X").is_some());
        assert!(map.get_field("F-Y").is_none());
    }

    #[test]
    fn test_map_get_field_by_variable() {
        let mut map = MapDefinition::new("TEST", 80, 24);
        map.add_field(MapField::new("F-X", 1, 1, 5, "#X"));
        assert!(map.get_field_by_variable("#X").is_some());
        assert!(map.get_field_by_variable("#Y").is_none());
    }

    #[test]
    fn test_pf_key_from_number() {
        assert_eq!(PfKey::from_number(1), Some(PfKey::Pf1));
        assert_eq!(PfKey::from_number(12), Some(PfKey::Pf12));
        assert_eq!(PfKey::from_number(24), Some(PfKey::Pf24));
        assert_eq!(PfKey::from_number(0), None);
        assert_eq!(PfKey::from_number(25), None);
    }

    #[test]
    fn test_pf_key_sysvar() {
        assert_eq!(PfKey::Enter.to_sysvar_value(), 0);
        assert_eq!(PfKey::Pf3.to_sysvar_value(), 3);
        assert_eq!(PfKey::Pf12.to_sysvar_value(), 12);
    }

    #[test]
    fn test_terminal_input() {
        let mut term = TerminalSimulator::new(80, 24);
        term.queue_input("#NAME", "Smith");
        let result = term.process_input(Some("Enter name:"), &["#NAME".into()]);
        assert_eq!(result.get("#NAME").unwrap().to_display_string(), "Smith");
    }

    #[test]
    fn test_terminal_input_no_queue() {
        let mut term = TerminalSimulator::new(80, 24);
        let result = term.process_input(None, &["#X".into()]);
        assert_eq!(result.get("#X").unwrap().to_display_string(), "");
    }

    #[test]
    fn test_terminal_map_processing() {
        let mut map = MapDefinition::new("TEST-MAP", 80, 24);
        map.add_field(MapField::new("F-NAME", 3, 15, 20, "#NAME")
            .with_prompt("Name:"));
        map.add_field(MapField::new("F-AGE", 4, 15, 3, "#AGE")
            .with_prompt("Age:"));

        let mut term = TerminalSimulator::new(80, 24);
        term.queue_input("#NAME", "Alice");
        term.queue_input("#AGE", "30");

        let result = term.process_map(&map);
        assert_eq!(result.get("#NAME").unwrap().to_display_string(), "Alice");
        assert_eq!(result.get("#AGE").unwrap().to_display_string(), "30");
    }

    #[test]
    fn test_terminal_reinput() {
        let mut term = TerminalSimulator::new(80, 24);
        term.process_reinput("Invalid input!", Some("#NAME"));
        assert_eq!(term.reinput_message.as_deref(), Some("Invalid input!"));
        assert_eq!(term.reinput_mark_field.as_deref(), Some("#NAME"));
    }

    #[test]
    fn test_terminal_screen_write() {
        let mut term = TerminalSimulator::new(80, 24);
        term.process_input(Some("Hello World"), &[]);
        let line = term.get_screen_line(0);
        assert!(line.starts_with("Hello World"));
    }

    #[test]
    fn test_terminal_pf_key() {
        let mut term = TerminalSimulator::new(80, 24);
        term.set_pf_key(PfKey::Pf3);
        assert_eq!(term.last_pf_key, PfKey::Pf3);
    }

    #[test]
    fn test_terminal_default() {
        let term = TerminalSimulator::default();
        assert_eq!(term.screen_width, 80);
        assert_eq!(term.screen_height, 24);
    }

    #[test]
    fn test_map_with_title() {
        let mut map = MapDefinition::new("TEST", 80, 24);
        map.title = Some("Employee Entry".into());
        map.add_field(MapField::new("F-X", 1, 1, 10, "#X"));

        let mut term = TerminalSimulator::new(80, 24);
        term.queue_input("#X", "test");
        term.process_map(&map);
        let line = term.get_screen_line(0);
        assert!(line.starts_with("Employee Entry"));
    }

    #[test]
    fn test_map_initial_value() {
        let mut map = MapDefinition::new("TEST", 80, 24);
        map.add_field(MapField::new("F-STATUS", 1, 10, 5, "#STATUS")
            .with_initial_value("ACTIVE")
            .with_attribute(FieldAttribute::Protected));

        let mut term = TerminalSimulator::new(80, 24);
        let result = term.process_map(&map);
        // Protected field should not be in result
        assert!(result.get("#STATUS").is_none());
    }

    #[test]
    fn test_field_attributes_all() {
        let attrs = vec![
            FieldAttribute::Normal,
            FieldAttribute::Intensified,
            FieldAttribute::NonDisplay,
            FieldAttribute::Protected,
            FieldAttribute::Modifiable,
            FieldAttribute::Detectable,
        ];
        assert_eq!(attrs.len(), 6);
    }

    #[test]
    fn test_field_colors_all() {
        let colors = vec![
            FieldColor::Default, FieldColor::Blue, FieldColor::Red,
            FieldColor::Pink, FieldColor::Green, FieldColor::Turquoise,
            FieldColor::Yellow, FieldColor::White,
        ];
        assert_eq!(colors.len(), 8);
    }
}
