//! Field navigation, MDT tracking, and field-level editing.
//!
//! Manages the field table for a 3270 screen, supporting tab navigation
//! between unprotected fields, character input, and Modified Data Tag tracking.

use open_mainframe_cics::bms::{AttributeByte, BmsMap, FieldColor, FieldHighlight};
use open_mainframe_cics::terminal::ScreenPosition;

/// Field validation attribute (3270 extended).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FieldValidation {
    /// No validation.
    #[default]
    None,
    /// Field must be modified before Enter is pressed.
    MustEnter,
    /// All positions in the field must be filled.
    MustFill,
}

/// Result of field validation.
#[derive(Debug, Clone)]
pub struct ValidationError {
    /// Name of the field that failed validation.
    pub field_name: String,
    /// The validation that failed.
    pub validation: FieldValidation,
    /// Human-readable error message.
    pub message: String,
}

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
    /// Extended color attribute (from BMS COLOR= specification).
    pub color: Option<FieldColor>,
    /// Extended highlight attribute (from BMS HILIGHT= specification).
    pub highlight: Option<FieldHighlight>,
    /// Whether the field has been modified by the user (MDT).
    pub modified: bool,
    /// Current field content (displayable characters).
    pub content: Vec<u8>,
    /// Cursor position within field (0-based offset from field start).
    pub cursor_offset: usize,
    /// Whether this field has the initial cursor (IC) attribute.
    pub initial_cursor: bool,
    /// Field validation attribute (MUSTENTER, MUSTFILL).
    pub validation: FieldValidation,
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
    /// Uses the given column width for row wrapping.
    pub fn position_at_cols(&self, offset: usize, cols: usize) -> ScreenPosition {
        let total_offset = (self.row - 1) * cols + (self.col - 1) + offset;
        ScreenPosition::new(total_offset / cols + 1, total_offset % cols + 1)
    }

    /// Get the screen position for a given offset within the field.
    /// Uses the default 80-column width.
    pub fn position_at(&self, offset: usize) -> ScreenPosition {
        self.position_at_cols(offset, 80)
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
    /// Screen columns (default 80).
    cols: usize,
    /// Screen rows (default 24).
    rows: usize,
}

impl FieldTable {
    /// Create an empty field table.
    pub fn new() -> Self {
        Self {
            fields: Vec::new(),
            input_order: Vec::new(),
            active_input: None,
            cols: 80,
            rows: 24,
        }
    }

    /// Create an empty field table with custom screen dimensions.
    pub fn with_dimensions(rows: usize, cols: usize) -> Self {
        Self {
            fields: Vec::new(),
            input_order: Vec::new(),
            active_input: None,
            cols,
            rows,
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
                color: bms_field.attributes.color,
                highlight: bms_field.attributes.highlight,
                modified: false,
                content,
                cursor_offset: 0,
                initial_cursor: bms_field.attributes.initial_cursor,
                validation: FieldValidation::None,
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

        // Derive dimensions from the BMS map SIZE
        let (rows, cols) = map.size;

        Self {
            fields,
            input_order,
            active_input,
            cols,
            rows,
        }
    }

    /// Get the screen column count.
    pub fn screen_cols(&self) -> usize {
        self.cols
    }

    /// Get the screen row count.
    pub fn screen_rows(&self) -> usize {
        self.rows
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

    /// Set the active field to the one containing the given screen position.
    /// Used for SEND MAP CURSOR(row,col) support.
    /// If the position lands in a protected field (or between fields),
    /// advance to the next unprotected field.
    pub fn set_cursor_to_position(&mut self, pos: &ScreenPosition) {
        // First try to find an unprotected field containing this position
        for (input_idx, &field_idx) in self.input_order.iter().enumerate() {
            let field = &self.fields[field_idx];
            if field.row == pos.row && pos.col >= field.col && pos.col < field.col + field.length {
                self.active_input = Some(input_idx);
                let offset = pos.col - field.col;
                self.fields[field_idx].cursor_offset = offset;
                return;
            }
        }

        // Position is in a protected field or empty area — advance to
        // the next unprotected field after this screen position.
        let target_linear = (pos.row - 1) * self.cols + (pos.col - 1);
        let mut best: Option<(usize, usize)> = None; // (input_idx, linear_pos)
        for (input_idx, &field_idx) in self.input_order.iter().enumerate() {
            let field = &self.fields[field_idx];
            let field_linear = (field.row - 1) * self.cols + (field.col - 1);
            if field_linear >= target_linear
                && (best.is_none() || field_linear < best.unwrap().1)
            {
                best = Some((input_idx, field_linear));
            }
        }
        // If no field after the position, wrap to the first unprotected field
        if let Some((input_idx, _)) = best {
            self.active_input = Some(input_idx);
            let field_idx = self.input_order[input_idx];
            self.fields[field_idx].cursor_offset = 0;
        } else if !self.input_order.is_empty() {
            self.active_input = Some(0);
            let field_idx = self.input_order[0];
            self.fields[field_idx].cursor_offset = 0;
        }
    }

    /// Set cursor from a linear screen offset (CURSOR(data-value)).
    /// Converts the offset to row/col based on current screen dimensions.
    pub fn set_cursor_to_offset(&mut self, offset: usize) {
        let row = offset / self.cols + 1;
        let col = offset % self.cols + 1;
        self.set_cursor_to_position(&ScreenPosition::new(row, col));
    }

    /// Get the cursor screen position based on the active field.
    pub fn cursor_position(&self) -> Option<ScreenPosition> {
        let cols = self.cols;
        self.active_field()
            .map(|f| f.position_at_cols(f.cursor_offset, cols))
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

    /// Move cursor up: find the input field on the row above that overlaps
    /// the current cursor column, or wrap to the last row.
    pub fn cursor_up(&mut self) {
        let (cur_row, cur_col) = match self.active_field() {
            Some(f) => (f.row, f.col + f.cursor_offset),
            None => return,
        };
        // Find the closest input field on a preceding row at same column
        let target_row = if cur_row > 1 { cur_row - 1 } else { self.rows };
        self.move_to_row(target_row, cur_col);
    }

    /// Move cursor down: find the input field on the row below that overlaps
    /// the current cursor column, or wrap to the first row.
    pub fn cursor_down(&mut self) {
        let (cur_row, cur_col) = match self.active_field() {
            Some(f) => (f.row, f.col + f.cursor_offset),
            None => return,
        };
        let target_row = if cur_row < self.rows { cur_row + 1 } else { 1 };
        self.move_to_row(target_row, cur_col);
    }

    /// Move to the input field on `target_row` closest to `target_col`.
    fn move_to_row(&mut self, target_row: usize, target_col: usize) {
        let mut best: Option<(usize, usize)> = None; // (input_idx, col_distance)
        for (input_idx, &field_idx) in self.input_order.iter().enumerate() {
            let field = &self.fields[field_idx];
            if field.row == target_row {
                // Check if target_col is within or near this field
                let field_end = field.col + field.length;
                let dist = if target_col >= field.col && target_col < field_end {
                    0
                } else if target_col < field.col {
                    field.col - target_col
                } else {
                    target_col - field_end + 1
                };
                if best.is_none() || dist < best.unwrap().1 {
                    best = Some((input_idx, dist));
                }
            }
        }
        if let Some((input_idx, _)) = best {
            let field_idx = self.input_order[input_idx];
            let field = &self.fields[field_idx];
            let offset = if target_col >= field.col && target_col < field.col + field.length {
                target_col - field.col
            } else {
                0
            };
            self.active_input = Some(input_idx);
            self.fields[field_idx].cursor_offset = offset;
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

    /// Set a validation attribute on a named field.
    pub fn set_field_validation(&mut self, name: &str, validation: FieldValidation) {
        for field in &mut self.fields {
            if field.name.eq_ignore_ascii_case(name) {
                field.validation = validation;
                break;
            }
        }
    }

    /// Validate all input fields before processing an AID key.
    ///
    /// Returns a list of validation errors. An empty list means all fields
    /// pass validation. The first error's field should receive focus and the
    /// keyboard should be locked.
    pub fn validate_fields(&self) -> Vec<ValidationError> {
        let mut errors = Vec::new();

        for &idx in &self.input_order {
            let field = &self.fields[idx];
            match field.validation {
                FieldValidation::None => {}
                FieldValidation::MustEnter => {
                    if !field.modified {
                        errors.push(ValidationError {
                            field_name: field.name.clone(),
                            validation: FieldValidation::MustEnter,
                            message: format!(
                                "Field '{}' must be modified before pressing Enter",
                                field.name
                            ),
                        });
                    }
                }
                FieldValidation::MustFill => {
                    let filled = field
                        .content
                        .iter()
                        .filter(|&&c| c != b' ' && c != 0)
                        .count();
                    if filled < field.length {
                        errors.push(ValidationError {
                            field_name: field.name.clone(),
                            validation: FieldValidation::MustFill,
                            message: format!(
                                "Field '{}' must be completely filled ({}/{})",
                                field.name, filled, field.length
                            ),
                        });
                    }
                }
            }
        }

        errors
    }
}

impl Default for FieldTable {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for FieldTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FieldTable")
            .field("field_count", &self.fields.len())
            .field("input_count", &self.input_order.len())
            .field("rows", &self.rows)
            .field("cols", &self.cols)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use open_mainframe_cics::bms::BmsParser;

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

    #[test]
    fn test_extended_color_attributes() {
        let source = r#"
COLTEST  DFHMSD TYPE=MAP,LANG=COBOL
COLM     DFHMDI SIZE=(24,80)
TITLE    DFHMDF POS=(1,1),LENGTH=20,ATTRB=(PROT,BRT),COLOR=BLUE
ERROR    DFHMDF POS=(2,1),LENGTH=40,ATTRB=(PROT),COLOR=RED,HILIGHT=REVERSE
INPUT    DFHMDF POS=(3,1),LENGTH=10,ATTRB=(UNPROT),COLOR=GREEN
         DFHMSD TYPE=FINAL
"#;
        let mut parser = BmsParser::new();
        let mapset = parser.parse(source).unwrap();
        let map = &mapset.maps[0];
        let table = FieldTable::from_bms_map(map);

        // TITLE: bright protected with blue color
        let title = table.fields().iter().find(|f| f.name == "TITLE").unwrap();
        assert!(title.attribute.is_protected());
        assert!(title.attribute.is_bright());
        assert_eq!(title.color, Some(FieldColor::Blue));
        assert_eq!(title.highlight, None);

        // ERROR: protected with red color and reverse highlight
        let error = table.fields().iter().find(|f| f.name == "ERROR").unwrap();
        assert!(error.attribute.is_protected());
        assert_eq!(error.color, Some(FieldColor::Red));
        assert_eq!(error.highlight, Some(FieldHighlight::Reverse));

        // INPUT: unprotected with green color
        let input = table.fields().iter().find(|f| f.name == "INPUT").unwrap();
        assert!(!input.attribute.is_protected());
        assert_eq!(input.color, Some(FieldColor::Green));
    }

    #[test]
    fn test_cursor_offset_positioning() {
        // NAME is at row 1, col 12 => linear offset = 0*80 + 11 = 11
        let map = create_test_map();
        let mut table = FieldTable::from_bms_map(&map);

        // Set cursor to offset 11 (row 1, col 12) — should land in NAME
        table.set_cursor_to_offset(11);
        let active = table.active_field().unwrap();
        assert_eq!(active.name, "NAME");
        assert_eq!(active.cursor_offset, 0);
    }

    #[test]
    fn test_cursor_offset_within_field() {
        let map = create_test_map();
        let mut table = FieldTable::from_bms_map(&map);

        // offset 14 = row 1, col 15 => NAME starts at col 12, so offset = 3
        table.set_cursor_to_offset(14);
        let active = table.active_field().unwrap();
        assert_eq!(active.name, "NAME");
        assert_eq!(active.cursor_offset, 3);
    }

    #[test]
    fn test_cursor_in_protected_field_advances() {
        // LABEL is at row 1, col 1 (protected). Setting cursor there
        // should advance to NAME (row 1, col 12).
        let map = create_test_map();
        let mut table = FieldTable::from_bms_map(&map);

        table.set_cursor_to_offset(0); // row 1, col 1 — LABEL (protected)
        let active = table.active_field().unwrap();
        assert_eq!(active.name, "NAME"); // advanced to next unprotected
    }

    // -----------------------------------------------------------------------
    // Field Validation Tests (Epic 903)
    // -----------------------------------------------------------------------

    #[test]
    fn test_mustenter_fails_when_not_modified() {
        let map = create_test_map();
        let mut table = FieldTable::from_bms_map(&map);
        table.set_field_validation("NAME", FieldValidation::MustEnter);

        let errors = table.validate_fields();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].field_name, "NAME");
        assert_eq!(errors[0].validation, FieldValidation::MustEnter);
    }

    #[test]
    fn test_mustenter_passes_when_modified() {
        let map = create_test_map();
        let mut table = FieldTable::from_bms_map(&map);
        table.set_field_validation("NAME", FieldValidation::MustEnter);

        // Type something to modify the field
        table.type_char('J');
        let errors = table.validate_fields();
        assert!(errors.is_empty());
    }

    #[test]
    fn test_mustfill_fails_when_partially_filled() {
        let map = create_test_map();
        let mut table = FieldTable::from_bms_map(&map);
        table.set_field_validation("NAME", FieldValidation::MustFill);

        // Type 3 characters in a 20-character field
        table.type_char('A');
        table.type_char('B');
        table.type_char('C');

        let errors = table.validate_fields();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].field_name, "NAME");
        assert_eq!(errors[0].validation, FieldValidation::MustFill);
    }

    #[test]
    fn test_mustfill_passes_when_all_filled() {
        let map = create_test_map();
        let mut table = FieldTable::from_bms_map(&map);

        // Set MUSTFILL on AGE (3 characters)
        table.tab_forward(); // go to AGE
        table.set_field_validation("AGE", FieldValidation::MustFill);

        table.type_char('2');
        table.type_char('5');
        table.type_char('0');

        let errors = table.validate_fields();
        assert!(errors.is_empty());
    }

    #[test]
    fn test_mustfill_empty_field_fails() {
        let map = create_test_map();
        let mut table = FieldTable::from_bms_map(&map);
        table.set_field_validation("NAME", FieldValidation::MustFill);

        // Don't type anything
        let errors = table.validate_fields();
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn test_no_validation_passes() {
        let map = create_test_map();
        let table = FieldTable::from_bms_map(&map);

        // No validation set — should pass
        let errors = table.validate_fields();
        assert!(errors.is_empty());
    }

    #[test]
    fn test_multiple_validation_errors() {
        let map = create_test_map();
        let mut table = FieldTable::from_bms_map(&map);
        table.set_field_validation("NAME", FieldValidation::MustEnter);
        table.set_field_validation("AGE", FieldValidation::MustFill);

        // Don't modify either field
        let errors = table.validate_fields();
        assert_eq!(errors.len(), 2);
    }

    #[test]
    fn test_cursor_position_precedence_over_ic() {
        let map = create_test_map();
        let mut table = FieldTable::from_bms_map(&map);

        // IC is on NAME, but CURSOR(position) should override
        // AGE is at row 2, col 12 => offset = 80 + 11 = 91
        table.set_cursor_to_offset(91);
        let active = table.active_field().unwrap();
        assert_eq!(active.name, "AGE");
    }
}
