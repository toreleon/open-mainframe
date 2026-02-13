//! Field navigation, MDT tracking, and field-level editing.
//!
//! Manages the field table for a 3270 screen, supporting tab navigation
//! between unprotected fields, character input, and Modified Data Tag tracking.

use zos_cics::bms::{AttributeByte, BmsMap};
use zos_cics::terminal::ScreenPosition;

/// Entry in the field table, tracking runtime state for each BMS field.
#[derive(Debug, Clone)]
pub struct FieldEntry {
    /// Field name (from BMS definition).
    pub name: String,
    /// Screen position (1-based row, col).
    pub row: usize,
    pub col: usize,
    /// Field length in characters.
    pub length: usize,
    /// 3270 attribute byte.
    pub attribute: AttributeByte,
    /// Whether the field has been modified by the user (MDT).
    pub modified: bool,
    /// Current field content (displayable characters).
    pub content: Vec<u8>,
    /// Cursor position within field (0-based offset from field start).
    pub cursor_offset: usize,
    /// Whether this field has the initial cursor (IC) attribute.
    pub initial_cursor: bool,
}

impl FieldEntry {
    /// Check if this field accepts input.
    pub fn is_input(&self) -> bool {
        !self.attribute.is_protected()
    }

    /// Check if this field is auto-skip.
    pub fn is_autoskip(&self) -> bool {
        self.attribute.is_protected() && self.attribute.is_numeric()
    }

    /// Check if this field accepts only numeric input.
    pub fn is_numeric(&self) -> bool {
        self.attribute.is_numeric() && !self.attribute.is_protected()
    }

    /// Get the screen position for a given offset within the field.
    pub fn position_at(&self, offset: usize) -> ScreenPosition {
        // Handle wrapping across rows (80-column screen)
        let cols = 80usize;
        let total_offset = (self.row - 1) * cols + (self.col - 1) + offset;
        ScreenPosition::new(total_offset / cols + 1, total_offset % cols + 1)
    }
}

/// Field table managing all fields on the current screen.
pub struct FieldTable {
    /// All fields on the screen.
    fields: Vec<FieldEntry>,
    /// Indices of unprotected (input) fields, in screen order.
    input_order: Vec<usize>,
    /// Currently active input field index (into input_order).
    active_input: Option<usize>,
}

impl FieldTable {
    /// Create an empty field table.
    pub fn new() -> Self {
        Self {
            fields: Vec::new(),
            input_order: Vec::new(),
            active_input: None,
        }
    }

    /// Build a field table from a BMS map definition.
    pub fn from_bms_map(map: &BmsMap) -> Self {
        let mut fields = Vec::new();
        let mut input_order = Vec::new();
        let mut initial_cursor_idx = None;

        for bms_field in &map.fields {
            let attr = bms_field.attributes.to_attribute_byte();
            let content = if let Some(ref initial) = bms_field.initial {
                let mut data = initial.as_bytes().to_vec();
                data.resize(bms_field.length, b' ');
                data
            } else {
                vec![b' '; bms_field.length]
            };

            let entry = FieldEntry {
                name: bms_field.name.clone(),
                row: bms_field.row,
                col: bms_field.column,
                length: bms_field.length,
                attribute: attr,
                modified: false,
                content,
                cursor_offset: 0,
                initial_cursor: bms_field.attributes.initial_cursor,
            };

            let idx = fields.len();
            if entry.is_input() {
                if entry.initial_cursor {
                    initial_cursor_idx = Some(input_order.len());
                }
                input_order.push(idx);
            }
            fields.push(entry);
        }

        // Set active input to the IC field, or the first input field
        let active_input = initial_cursor_idx.or(if input_order.is_empty() {
            None
        } else {
            Some(0)
        });

        Self {
            fields,
            input_order,
            active_input,
        }
    }

    /// Get all fields.
    pub fn fields(&self) -> &[FieldEntry] {
        &self.fields
    }

    /// Get the currently active (focused) input field, if any.
    pub fn active_field(&self) -> Option<&FieldEntry> {
        self.active_input
            .and_then(|i| self.input_order.get(i))
            .and_then(|&fi| self.fields.get(fi))
    }

    /// Get the currently active input field (mutable).
    pub fn active_field_mut(&mut self) -> Option<&mut FieldEntry> {
        let fi = self
            .active_input
            .and_then(|i| self.input_order.get(i).copied())?;
        self.fields.get_mut(fi)
    }

    /// Get the cursor screen position based on the active field.
    pub fn cursor_position(&self) -> Option<ScreenPosition> {
        self.active_field()
            .map(|f| f.position_at(f.cursor_offset))
    }

    /// Move to the next unprotected field (Tab).
    pub fn tab_forward(&mut self) {
        if self.input_order.is_empty() {
            return;
        }
        match self.active_input {
            Some(i) => {
                self.active_input = Some((i + 1) % self.input_order.len());
            }
            None => {
                self.active_input = Some(0);
            }
        }
        // Reset cursor to start of new field
        if let Some(field) = self.active_field_mut() {
            field.cursor_offset = 0;
        }
    }

    /// Move to the previous unprotected field (Back-tab).
    pub fn tab_backward(&mut self) {
        if self.input_order.is_empty() {
            return;
        }
        match self.active_input {
            Some(0) => {
                self.active_input = Some(self.input_order.len() - 1);
            }
            Some(i) => {
                self.active_input = Some(i - 1);
            }
            None => {
                self.active_input = Some(self.input_order.len() - 1);
            }
        }
        if let Some(field) = self.active_field_mut() {
            field.cursor_offset = 0;
        }
    }

    /// Move to the first unprotected field (Home).
    pub fn home(&mut self) {
        if !self.input_order.is_empty() {
            self.active_input = Some(0);
            if let Some(field) = self.active_field_mut() {
                field.cursor_offset = 0;
            }
        }
    }

    /// Type a character into the active field.
    /// Returns true if the character was accepted, false if rejected.
    pub fn type_char(&mut self, ch: char) -> bool {
        let fi = match self
            .active_input
            .and_then(|i| self.input_order.get(i).copied())
        {
            Some(fi) => fi,
            None => return false,
        };

        let field = &self.fields[fi];

        // Reject non-numeric input in numeric fields
        if field.is_numeric() && !ch.is_ascii_digit() {
            return false;
        }

        let offset = field.cursor_offset;
        let length = field.length;

        if offset >= length {
            // Field is full - check if autoskip
            if self.should_autoskip(fi) {
                self.tab_forward();
                return self.type_char(ch);
            }
            return false;
        }

        // Insert character
        let field = &mut self.fields[fi];
        field.content[offset] = ch as u8;
        field.modified = true;
        field.cursor_offset += 1;

        // Auto-advance if field is now full and next field is autoskip-compatible
        if field.cursor_offset >= field.length && self.should_autoskip(fi) {
            self.tab_forward();
        }

        true
    }

    /// Check if we should auto-skip from the given field.
    fn should_autoskip(&self, _field_idx: usize) -> bool {
        // In 3270, autoskip means the cursor auto-advances to the next
        // unprotected field when the current field is full.
        // This is the default behavior for input fields.
        true
    }

    /// Handle backspace in the active field.
    pub fn backspace(&mut self) -> bool {
        let field = match self.active_field_mut() {
            Some(f) => f,
            None => return false,
        };

        if field.cursor_offset == 0 {
            return false;
        }

        // Shift characters left
        let offset = field.cursor_offset;
        for i in (offset - 1)..(field.length - 1) {
            field.content[i] = field.content[i + 1];
        }
        field.content[field.length - 1] = b' ';
        field.cursor_offset -= 1;
        field.modified = true;
        true
    }

    /// Handle delete at cursor in the active field.
    pub fn delete(&mut self) -> bool {
        let field = match self.active_field_mut() {
            Some(f) => f,
            None => return false,
        };

        let offset = field.cursor_offset;
        if offset >= field.length {
            return false;
        }

        // Shift characters left from cursor
        for i in offset..(field.length - 1) {
            field.content[i] = field.content[i + 1];
        }
        field.content[field.length - 1] = b' ';
        field.modified = true;
        true
    }

    /// Erase from cursor to end of field.
    pub fn erase_eof(&mut self) -> bool {
        let field = match self.active_field_mut() {
            Some(f) => f,
            None => return false,
        };

        let offset = field.cursor_offset;
        for i in offset..field.length {
            field.content[i] = b' ';
        }
        field.modified = true;
        true
    }

    /// Move cursor left within the active field.
    pub fn cursor_left(&mut self) {
        if let Some(field) = self.active_field_mut() {
            if field.cursor_offset > 0 {
                field.cursor_offset -= 1;
            }
        }
    }

    /// Move cursor right within the active field.
    pub fn cursor_right(&mut self) {
        if let Some(field) = self.active_field_mut() {
            if field.cursor_offset < field.length - 1 {
                field.cursor_offset += 1;
            }
        }
    }

    /// Clear all unprotected fields and reset MDTs.
    pub fn clear_unprotected(&mut self) {
        for &idx in &self.input_order {
            let field = &mut self.fields[idx];
            field.content = vec![b' '; field.length];
            field.modified = false;
            field.cursor_offset = 0;
        }
        self.home();
    }

    /// Reset all MDT flags (for FRSET option).
    pub fn reset_mdt(&mut self) {
        for field in &mut self.fields {
            field.modified = false;
        }
    }

    /// Get modified fields as name->data pairs for RECEIVE MAP.
    pub fn get_modified_fields(&self) -> Vec<(String, Vec<u8>)> {
        let mut result = Vec::new();
        for &idx in &self.input_order {
            let field = &self.fields[idx];
            if field.modified && !field.name.is_empty() {
                result.push((field.name.clone(), field.content.clone()));
            }
        }
        result
    }

    /// Get all input field data (modified or not) for RECEIVE MAP.
    pub fn get_all_input_fields(&self) -> Vec<(String, Vec<u8>)> {
        let mut result = Vec::new();
        for &idx in &self.input_order {
            let field = &self.fields[idx];
            if !field.name.is_empty() {
                result.push((field.name.clone(), field.content.clone()));
            }
        }
        result
    }

    /// Update a field's content (e.g., from SEND MAP data).
    pub fn set_field_data(&mut self, name: &str, data: &[u8]) {
        for field in &mut self.fields {
            if field.name.eq_ignore_ascii_case(name) {
                let len = field.length.min(data.len());
                field.content[..len].copy_from_slice(&data[..len]);
                // Pad with spaces
                for i in len..field.length {
                    field.content[i] = b' ';
                }
                break;
            }
        }
    }

    /// Get the number of input fields.
    pub fn input_field_count(&self) -> usize {
        self.input_order.len()
    }
}

impl Default for FieldTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use zos_cics::bms::BmsParser;

    fn create_test_map() -> BmsMap {
        let source = r#"
TEST     DFHMSD TYPE=MAP,LANG=COBOL
TESTM    DFHMDI SIZE=(24,80)
LABEL    DFHMDF POS=(1,1),LENGTH=10,ATTRB=(PROT),INITIAL='Name:'
NAME     DFHMDF POS=(1,12),LENGTH=20,ATTRB=(UNPROT,IC)
AGE      DFHMDF POS=(2,12),LENGTH=3,ATTRB=(NUM,UNPROT)
STATUS   DFHMDF POS=(3,12),LENGTH=15,ATTRB=(PROT)
         DFHMSD TYPE=FINAL
"#;
        let mut parser = BmsParser::new();
        let mapset = parser.parse(source).unwrap();
        mapset.maps[0].clone()
    }

    #[test]
    fn test_field_table_creation() {
        let map = create_test_map();
        let table = FieldTable::from_bms_map(&map);
        assert_eq!(table.fields.len(), 4);
        assert_eq!(table.input_field_count(), 2); // NAME and AGE
    }

    #[test]
    fn test_tab_navigation() {
        let map = create_test_map();
        let mut table = FieldTable::from_bms_map(&map);

        // Should start on NAME (IC attribute)
        let active = table.active_field().unwrap();
        assert_eq!(active.name, "NAME");

        // Tab forward to AGE
        table.tab_forward();
        let active = table.active_field().unwrap();
        assert_eq!(active.name, "AGE");

        // Tab forward wraps to NAME
        table.tab_forward();
        let active = table.active_field().unwrap();
        assert_eq!(active.name, "NAME");
    }

    #[test]
    fn test_back_tab() {
        let map = create_test_map();
        let mut table = FieldTable::from_bms_map(&map);

        // Start on NAME, back-tab to AGE (wraps)
        table.tab_backward();
        let active = table.active_field().unwrap();
        assert_eq!(active.name, "AGE");
    }

    #[test]
    fn test_type_char() {
        let map = create_test_map();
        let mut table = FieldTable::from_bms_map(&map);

        assert!(table.type_char('J'));
        assert!(table.type_char('o'));
        assert!(table.type_char('h'));
        assert!(table.type_char('n'));

        let field = table.active_field().unwrap();
        assert_eq!(&field.content[..4], b"John");
        assert!(field.modified);
    }

    #[test]
    fn test_numeric_field_rejects_alpha() {
        let map = create_test_map();
        let mut table = FieldTable::from_bms_map(&map);

        // Navigate to AGE field
        table.tab_forward();
        let active = table.active_field().unwrap();
        assert_eq!(active.name, "AGE");

        // Numeric should accept digits
        assert!(table.type_char('2'));
        assert!(table.type_char('5'));

        // Should reject letters
        assert!(!table.type_char('A'));
    }

    #[test]
    fn test_backspace() {
        let map = create_test_map();
        let mut table = FieldTable::from_bms_map(&map);

        table.type_char('A');
        table.type_char('B');
        table.type_char('C');

        assert!(table.backspace());
        let field = table.active_field().unwrap();
        assert_eq!(&field.content[..3], b"AB ");
        assert_eq!(field.cursor_offset, 2);
    }

    #[test]
    fn test_clear_unprotected() {
        let map = create_test_map();
        let mut table = FieldTable::from_bms_map(&map);

        table.type_char('X');
        table.clear_unprotected();

        let field = table.active_field().unwrap();
        assert!(field.content.iter().all(|&c| c == b' '));
        assert!(!field.modified);
    }

    #[test]
    fn test_get_modified_fields() {
        let map = create_test_map();
        let mut table = FieldTable::from_bms_map(&map);

        table.type_char('A');
        let modified = table.get_modified_fields();
        assert_eq!(modified.len(), 1);
        assert_eq!(modified[0].0, "NAME");
    }
}
