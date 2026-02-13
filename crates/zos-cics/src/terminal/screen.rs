//! Screen buffer for terminal I/O.
//!
//! Manages the 3270 screen buffer including character cells and attributes.

use crate::bms::{AttributeByte, ScreenSize};

/// Position on screen.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct ScreenPosition {
    /// Row (1-based)
    pub row: usize,
    /// Column (1-based)
    pub col: usize,
}

impl ScreenPosition {
    /// Create a new position.
    pub fn new(row: usize, col: usize) -> Self {
        Self { row, col }
    }

    /// Convert to buffer offset (0-based).
    pub fn to_offset(&self, columns: usize) -> usize {
        (self.row.saturating_sub(1)) * columns + (self.col.saturating_sub(1))
    }

    /// Create from buffer offset.
    pub fn from_offset(offset: usize, columns: usize) -> Self {
        Self {
            row: offset / columns + 1,
            col: offset % columns + 1,
        }
    }
}

/// A cell on the screen.
#[derive(Debug, Clone, Copy, Default)]
pub struct ScreenCell {
    /// Character (in EBCDIC or ASCII depending on context)
    pub char: u8,
    /// Attribute byte (if this is a field start)
    pub attribute: Option<AttributeByte>,
    /// Whether this cell is protected
    pub protected: bool,
    /// Whether this cell has been modified
    pub modified: bool,
}

impl ScreenCell {
    /// Create an empty cell.
    pub fn empty() -> Self {
        Self {
            char: b' ',
            attribute: None,
            protected: false,
            modified: false,
        }
    }

    /// Create a cell with a character.
    pub fn with_char(ch: u8) -> Self {
        Self {
            char: ch,
            attribute: None,
            protected: false,
            modified: false,
        }
    }

    /// Create an attribute cell.
    pub fn attribute(attr: AttributeByte) -> Self {
        Self {
            char: 0,
            attribute: Some(attr),
            protected: attr.is_protected(),
            modified: false,
        }
    }
}

/// Screen buffer for a 3270 terminal.
pub struct ScreenBuffer {
    /// Screen size
    size: ScreenSize,
    /// Buffer contents
    cells: Vec<ScreenCell>,
    /// Cursor position
    cursor: ScreenPosition,
}

impl ScreenBuffer {
    /// Create a new screen buffer.
    pub fn new(size: ScreenSize) -> Self {
        let (rows, cols) = size.dimensions();
        let total = rows * cols;

        Self {
            size,
            cells: vec![ScreenCell::empty(); total],
            cursor: ScreenPosition::new(1, 1),
        }
    }

    /// Get screen dimensions.
    pub fn dimensions(&self) -> (usize, usize) {
        self.size.dimensions()
    }

    /// Get total cells.
    pub fn total_cells(&self) -> usize {
        self.cells.len()
    }

    /// Clear the screen.
    pub fn clear(&mut self) {
        for cell in &mut self.cells {
            *cell = ScreenCell::empty();
        }
        self.cursor = ScreenPosition::new(1, 1);
    }

    /// Clear all unprotected fields.
    pub fn clear_unprotected(&mut self) {
        for cell in &mut self.cells {
            if !cell.protected && cell.attribute.is_none() {
                cell.char = b' ';
                cell.modified = false;
            }
        }
    }

    /// Get cell at position.
    pub fn get(&self, pos: ScreenPosition) -> Option<&ScreenCell> {
        let (_, cols) = self.dimensions();
        let offset = pos.to_offset(cols);
        self.cells.get(offset)
    }

    /// Get cell at position (mutable).
    pub fn get_mut(&mut self, pos: ScreenPosition) -> Option<&mut ScreenCell> {
        let (_, cols) = self.dimensions();
        let offset = pos.to_offset(cols);
        self.cells.get_mut(offset)
    }

    /// Get cell at offset.
    pub fn get_offset(&self, offset: usize) -> Option<&ScreenCell> {
        self.cells.get(offset)
    }

    /// Get cell at offset (mutable).
    pub fn get_offset_mut(&mut self, offset: usize) -> Option<&mut ScreenCell> {
        self.cells.get_mut(offset)
    }

    /// Write a character at position.
    pub fn write_char(&mut self, pos: ScreenPosition, ch: u8) {
        if let Some(cell) = self.get_mut(pos) {
            cell.char = ch;
            cell.modified = true;
        }
    }

    /// Write a string starting at position.
    pub fn write_string(&mut self, start: ScreenPosition, s: &[u8]) {
        let (_, cols) = self.dimensions();
        let mut offset = start.to_offset(cols);

        for &ch in s {
            if offset >= self.cells.len() {
                break;
            }
            self.cells[offset].char = ch;
            self.cells[offset].modified = true;
            offset += 1;
        }
    }

    /// Read a range from the buffer.
    pub fn read_range(&self, start: ScreenPosition, length: usize) -> Vec<u8> {
        let (_, cols) = self.dimensions();
        let offset = start.to_offset(cols);
        let end = (offset + length).min(self.cells.len());

        self.cells[offset..end]
            .iter()
            .map(|cell| cell.char)
            .collect()
    }

    /// Write an attribute at position.
    pub fn write_attribute(&mut self, pos: ScreenPosition, attr: AttributeByte) {
        if let Some(cell) = self.get_mut(pos) {
            *cell = ScreenCell::attribute(attr);
        }

        // Set protected flag on following cells until next attribute
        let (_, cols) = self.dimensions();
        let start_offset = pos.to_offset(cols);
        let protected = attr.is_protected();

        for offset in (start_offset + 1)..self.cells.len() {
            if self.cells[offset].attribute.is_some() {
                break;
            }
            self.cells[offset].protected = protected;
        }
    }

    /// Get cursor position.
    pub fn cursor(&self) -> ScreenPosition {
        self.cursor
    }

    /// Set cursor position.
    pub fn set_cursor(&mut self, pos: ScreenPosition) {
        self.cursor = pos;
    }

    /// Get all modified fields.
    pub fn get_modified_fields(&self) -> Vec<(ScreenPosition, Vec<u8>)> {
        let mut fields = Vec::new();
        let (_, cols) = self.dimensions();

        let mut field_start = None;
        let mut current_attr = None;

        for (offset, cell) in self.cells.iter().enumerate() {
            if let Some(attr) = cell.attribute {
                // End previous field if modified
                if let (Some(start), Some(_)) = (field_start, current_attr) {
                    let data = self.read_range(start, offset - start.to_offset(cols));
                    if data.iter().any(|&c| c != b' ') || self.is_field_modified(start, offset) {
                        fields.push((start, data));
                    }
                }

                // Start new field
                field_start = Some(ScreenPosition::from_offset(offset + 1, cols));
                current_attr = Some(attr);
            }
        }

        fields
    }

    fn is_field_modified(&self, start: ScreenPosition, end_offset: usize) -> bool {
        let (_, cols) = self.dimensions();
        let start_offset = start.to_offset(cols);

        for offset in start_offset..end_offset {
            if self.cells.get(offset).map(|c| c.modified).unwrap_or(false) {
                return true;
            }
        }
        false
    }

    /// Convert to text representation (for debugging).
    pub fn to_text(&self) -> String {
        let (rows, cols) = self.dimensions();
        let mut result = String::new();

        for row in 0..rows {
            for col in 0..cols {
                let offset = row * cols + col;
                let ch = self.cells[offset].char;

                // Convert to displayable ASCII
                let display = if (0x20..0x7F).contains(&ch) {
                    ch as char
                } else if ch == 0 {
                    ' '
                } else {
                    '.'
                };
                result.push(display);
            }
            result.push('\n');
        }

        result
    }
}

impl Default for ScreenBuffer {
    fn default() -> Self {
        Self::new(ScreenSize::Model2)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_screen_position() {
        let pos = ScreenPosition::new(2, 10);
        // Row 2, Column 10 on 80-column screen
        // Offset = (2-1) * 80 + (10-1) = 80 + 9 = 89
        assert_eq!(pos.to_offset(80), 89);

        let from_offset = ScreenPosition::from_offset(89, 80);
        assert_eq!(from_offset, pos);
    }

    #[test]
    fn test_screen_buffer_creation() {
        let buffer = ScreenBuffer::new(ScreenSize::Model2);
        assert_eq!(buffer.dimensions(), (24, 80));
        assert_eq!(buffer.total_cells(), 1920);
    }

    #[test]
    fn test_clear_screen() {
        let mut buffer = ScreenBuffer::new(ScreenSize::Model2);
        buffer.write_char(ScreenPosition::new(1, 1), b'X');
        buffer.clear();

        let cell = buffer.get(ScreenPosition::new(1, 1)).unwrap();
        assert_eq!(cell.char, b' ');
    }

    #[test]
    fn test_write_string() {
        let mut buffer = ScreenBuffer::new(ScreenSize::Model2);
        buffer.write_string(ScreenPosition::new(5, 10), b"Hello");

        let data = buffer.read_range(ScreenPosition::new(5, 10), 5);
        assert_eq!(&data, b"Hello");
    }

    #[test]
    fn test_write_attribute() {
        let mut buffer = ScreenBuffer::new(ScreenSize::Model2);
        let attr = AttributeByte::new(true, false, false, false, false);
        buffer.write_attribute(ScreenPosition::new(1, 1), attr);

        let cell = buffer.get(ScreenPosition::new(1, 1)).unwrap();
        assert!(cell.attribute.is_some());
    }

    #[test]
    fn test_cursor() {
        let mut buffer = ScreenBuffer::new(ScreenSize::Model2);
        buffer.set_cursor(ScreenPosition::new(10, 20));

        assert_eq!(buffer.cursor(), ScreenPosition::new(10, 20));
    }

    #[test]
    fn test_clear_unprotected() {
        let mut buffer = ScreenBuffer::new(ScreenSize::Model2);

        // Write protected and unprotected fields
        let protected_attr = AttributeByte::new(true, false, false, false, false);
        let unprotected_attr = AttributeByte::new(false, false, false, false, false);

        buffer.write_attribute(ScreenPosition::new(1, 1), protected_attr);
        buffer.write_string(ScreenPosition::new(1, 2), b"Protected");

        buffer.write_attribute(ScreenPosition::new(2, 1), unprotected_attr);
        buffer.write_string(ScreenPosition::new(2, 2), b"Unprotected");

        buffer.clear_unprotected();

        // Protected field should remain
        let prot_data = buffer.read_range(ScreenPosition::new(1, 2), 9);
        assert_eq!(&prot_data, b"Protected");

        // Unprotected field should be cleared
        let unprot_data = buffer.read_range(ScreenPosition::new(2, 2), 11);
        assert!(unprot_data.iter().all(|&c| c == b' '));
    }
}
