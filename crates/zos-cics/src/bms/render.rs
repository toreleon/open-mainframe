//! BMS map rendering to 3270 data streams.
//!
//! Generates 3270 output data streams from BMS maps.

use super::field::BmsField;
use super::parser::BmsMap;
use super::{AttributeByte, ScreenSize};
use std::collections::HashMap;

/// 3270 orders (commands in data stream).
#[allow(dead_code)]
pub mod orders {
    /// Set Buffer Address
    pub const SBA: u8 = 0x11;
    /// Start Field
    pub const SF: u8 = 0x1D;
    /// Start Field Extended
    pub const SFE: u8 = 0x29;
    /// Set Attribute
    pub const SA: u8 = 0x28;
    /// Insert Cursor
    pub const IC: u8 = 0x13;
    /// Program Tab
    pub const PT: u8 = 0x05;
    /// Repeat to Address
    pub const RA: u8 = 0x3C;
    /// Erase Unprotected to Address
    pub const EUA: u8 = 0x12;
    /// Modify Field
    pub const MF: u8 = 0x2C;
}

/// 3270 commands.
#[allow(dead_code)]
pub mod commands {
    /// Write
    pub const WRITE: u8 = 0xF1;
    /// Erase/Write
    pub const ERASE_WRITE: u8 = 0xF5;
    /// Erase/Write Alternate
    pub const ERASE_WRITE_ALT: u8 = 0x7E;
    /// Erase All Unprotected
    pub const ERASE_ALL_UNPROTECTED: u8 = 0x6F;
    /// Read Buffer
    pub const READ_BUFFER: u8 = 0xF2;
    /// Read Modified
    pub const READ_MODIFIED: u8 = 0xF6;
}

/// Write Control Character (WCC).
#[derive(Debug, Clone, Copy, Default)]
pub struct Wcc(u8);

#[allow(dead_code)]
impl Wcc {
    /// Reset MDT bits.
    pub const RESET_MDT: u8 = 0x01;
    /// Sound alarm.
    pub const ALARM: u8 = 0x04;
    /// Unlock keyboard.
    pub const KEYBOARD_RESTORE: u8 = 0x02;
    /// Reset partition characteristics.
    pub const RESET_PARTITION: u8 = 0x40;

    /// Create new WCC.
    pub fn new() -> Self {
        Self(Self::KEYBOARD_RESTORE)
    }

    /// Add reset MDT.
    pub fn with_reset_mdt(mut self) -> Self {
        self.0 |= Self::RESET_MDT;
        self
    }

    /// Add alarm.
    pub fn with_alarm(mut self) -> Self {
        self.0 |= Self::ALARM;
        self
    }

    /// Get byte value.
    pub fn byte(&self) -> u8 {
        self.0 | 0x40 // Always set bit 6
    }
}

/// Map renderer for generating 3270 output.
pub struct MapRenderer {
    /// Screen size
    screen_size: ScreenSize,
    /// Output buffer
    buffer: Vec<u8>,
    /// Field data values
    data: HashMap<String, Vec<u8>>,
}

impl MapRenderer {
    /// Create a new renderer.
    pub fn new(screen_size: ScreenSize) -> Self {
        Self {
            screen_size,
            buffer: Vec::new(),
            data: HashMap::new(),
        }
    }

    /// Set field data.
    pub fn set_field(&mut self, name: &str, value: &[u8]) {
        self.data.insert(name.to_uppercase(), value.to_vec());
    }

    /// Set field data from string.
    pub fn set_field_string(&mut self, name: &str, value: &str) {
        self.data.insert(name.to_uppercase(), value.as_bytes().to_vec());
    }

    /// Render map to 3270 data stream.
    pub fn render(&mut self, map: &BmsMap, erase: bool) -> Vec<u8> {
        self.buffer.clear();

        // Add command and WCC
        if erase {
            self.buffer.push(commands::ERASE_WRITE);
        } else {
            self.buffer.push(commands::WRITE);
        }

        let wcc = Wcc::new().with_reset_mdt();
        self.buffer.push(wcc.byte());

        // Render each field
        for field in &map.fields {
            self.render_field(field);
        }

        self.buffer.clone()
    }

    fn render_field(&mut self, field: &BmsField) {
        let (_, cols) = self.screen_size.dimensions();

        // Set buffer address to attribute position
        let attr_pos = field.attribute_position(cols);
        self.write_sba(attr_pos);

        // Write start field
        let attr = field.attributes.to_attribute_byte();
        self.buffer.push(orders::SF);
        self.buffer.push(self.encode_attribute(attr));

        // Get field data
        let data = self.get_field_data(field);

        // Write field data
        for &b in &data {
            self.buffer.push(self.translate_to_ebcdic(b));
        }

        // Pad with nulls if data is shorter than field
        for _ in data.len()..field.length {
            self.buffer.push(0x00);
        }

        // Set cursor if IC attribute
        if field.attributes.initial_cursor {
            let field_pos = field.buffer_position(cols);
            self.write_sba(field_pos);
            self.buffer.push(orders::IC);
        }
    }

    fn get_field_data(&self, field: &BmsField) -> Vec<u8> {
        // Check for runtime data first
        if let Some(data) = self.data.get(&field.name.to_uppercase()) {
            return data.clone();
        }

        // Use initial value if available
        if let Some(initial) = &field.initial {
            return initial.as_bytes().to_vec();
        }

        Vec::new()
    }

    fn write_sba(&mut self, position: usize) {
        self.buffer.push(orders::SBA);
        let (hi, lo) = self.encode_address(position);
        self.buffer.push(hi);
        self.buffer.push(lo);
    }

    fn encode_address(&self, position: usize) -> (u8, u8) {
        // 14-bit address encoding for 3270
        let addr = position & 0x3FFF;

        // For addresses 0-4095, use 12-bit encoding
        if addr < 4096 {
            let hi = ((addr >> 6) & 0x3F) as u8 | 0x40;
            let lo = (addr & 0x3F) as u8 | 0x40;
            (hi, lo)
        } else {
            // For larger addresses, use 14-bit encoding
            let hi = ((addr >> 8) & 0x3F) as u8;
            let lo = (addr & 0xFF) as u8;
            (hi, lo)
        }
    }

    fn encode_attribute(&self, attr: AttributeByte) -> u8 {
        // Convert attribute byte to 3270 format
        // Basic encoding: start with 0x40 (makes it a displayable character)
        let mut encoded = 0x40u8;

        if attr.is_protected() {
            encoded |= 0x20;
        }
        if attr.is_numeric() {
            encoded |= 0x10;
        }

        // Intensity bits
        if attr.is_dark() {
            encoded |= 0x0C;
        } else if attr.is_bright() {
            encoded |= 0x08;
        }

        if attr.is_modified() {
            encoded |= 0x01;
        }

        encoded
    }

    fn translate_to_ebcdic(&self, ascii: u8) -> u8 {
        // Simple ASCII to EBCDIC translation for common characters
        match ascii {
            b'A'..=b'I' => ascii - b'A' + 0xC1,
            b'J'..=b'R' => ascii - b'J' + 0xD1,
            b'S'..=b'Z' => ascii - b'S' + 0xE2,
            b'a'..=b'i' => ascii - b'a' + 0x81,
            b'j'..=b'r' => ascii - b'j' + 0x91,
            b's'..=b'z' => ascii - b's' + 0xA2,
            b'0'..=b'9' => ascii - b'0' + 0xF0,
            b' ' => 0x40,
            b'.' => 0x4B,
            b',' => 0x6B,
            b':' => 0x7A,
            b'-' => 0x60,
            b'_' => 0x6D,
            b'/' => 0x61,
            b'(' => 0x4D,
            b')' => 0x5D,
            b'$' => 0x5B,
            b'#' => 0x7B,
            b'@' => 0x7C,
            b'%' => 0x6C,
            b'&' => 0x50,
            b'!' => 0x5A,
            b'=' => 0x7E,
            b'+' => 0x4E,
            b'*' => 0x5C,
            b'<' => 0x4C,
            b'>' => 0x6E,
            b'?' => 0x6F,
            b';' => 0x5E,
            b'\'' => 0x7D,
            b'"' => 0x7F,
            _ => 0x40, // Default to space
        }
    }

    /// Render map to text representation (for debugging).
    pub fn render_text(&self, map: &BmsMap) -> String {
        let (rows, cols) = self.screen_size.dimensions();
        let mut screen: Vec<Vec<char>> = vec![vec![' '; cols]; rows];

        for field in &map.fields {
            let row = field.row.saturating_sub(1);
            let col = field.column.saturating_sub(1);

            // Get field data
            let data = self.get_field_data(field);
            let display: String = if data.is_empty() {
                "_".repeat(field.length)
            } else {
                String::from_utf8_lossy(&data)
                    .chars()
                    .take(field.length)
                    .collect()
            };

            // Write to screen buffer
            for (i, ch) in display.chars().enumerate() {
                if col + i < cols && row < rows {
                    screen[row][col + i] = ch;
                }
            }
        }

        screen.iter()
            .map(|row| row.iter().collect::<String>())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl Default for MapRenderer {
    fn default() -> Self {
        Self::new(ScreenSize::Model2)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bms::parser::BmsParser;

    #[test]
    fn test_wcc() {
        let wcc = Wcc::new().with_reset_mdt().with_alarm();
        let byte = wcc.byte();

        assert_ne!(byte & Wcc::RESET_MDT, 0);
        assert_ne!(byte & Wcc::ALARM, 0);
        assert_ne!(byte & Wcc::KEYBOARD_RESTORE, 0);
    }

    #[test]
    fn test_address_encoding() {
        let renderer = MapRenderer::new(ScreenSize::Model2);

        // Position 0
        let (hi, lo) = renderer.encode_address(0);
        assert_eq!(hi, 0x40);
        assert_eq!(lo, 0x40);

        // Position 80 (start of row 2)
        let (hi, lo) = renderer.encode_address(80);
        assert_eq!(hi, 0x41);
        assert_eq!(lo, 0x50);
    }

    #[test]
    fn test_ebcdic_translation() {
        let renderer = MapRenderer::new(ScreenSize::Model2);

        assert_eq!(renderer.translate_to_ebcdic(b'A'), 0xC1);
        assert_eq!(renderer.translate_to_ebcdic(b'Z'), 0xE9);
        assert_eq!(renderer.translate_to_ebcdic(b'0'), 0xF0);
        assert_eq!(renderer.translate_to_ebcdic(b'9'), 0xF9);
        assert_eq!(renderer.translate_to_ebcdic(b' '), 0x40);
    }

    #[test]
    fn test_render_simple_map() {
        let source = r#"
TEST     DFHMSD TYPE=MAP,LANG=COBOL
TESTM    DFHMDI SIZE=(24,80)
TITLE    DFHMDF POS=(1,1),LENGTH=10,ATTRB=(ASKIP),INITIAL='TITLE'
INPUT    DFHMDF POS=(2,1),LENGTH=20,ATTRB=(UNPROT,IC)
         DFHMSD TYPE=FINAL
"#;

        let mut parser = BmsParser::new();
        let mapset = parser.parse(source).unwrap();
        let map = &mapset.maps[0];

        let mut renderer = MapRenderer::new(ScreenSize::Model2);
        let stream = renderer.render(map, true);

        // Should start with ERASE_WRITE and WCC
        assert_eq!(stream[0], commands::ERASE_WRITE);
        assert!(stream.len() > 2);
    }

    #[test]
    fn test_render_with_data() {
        let source = r#"
TEST     DFHMSD TYPE=MAP,LANG=COBOL
TESTM    DFHMDI SIZE=(24,80)
NAME     DFHMDF POS=(1,10),LENGTH=20,ATTRB=(PROT)
         DFHMSD TYPE=FINAL
"#;

        let mut parser = BmsParser::new();
        let mapset = parser.parse(source).unwrap();
        let map = &mapset.maps[0];

        let mut renderer = MapRenderer::new(ScreenSize::Model2);
        renderer.set_field_string("NAME", "John Doe");

        let text = renderer.render_text(map);
        assert!(text.contains("John Doe"));
    }

    #[test]
    fn test_attribute_encoding() {
        let renderer = MapRenderer::new(ScreenSize::Model2);

        // Protected, bright
        let attr = AttributeByte::new(true, false, true, false, false);
        let encoded = renderer.encode_attribute(attr);
        assert_ne!(encoded & 0x20, 0); // Protected bit
        assert_ne!(encoded & 0x08, 0); // Bright bit

        // Unprotected, numeric
        let attr2 = AttributeByte::new(false, true, false, false, false);
        let encoded2 = renderer.encode_attribute(attr2);
        assert_eq!(encoded2 & 0x20, 0); // Not protected
        assert_ne!(encoded2 & 0x10, 0); // Numeric bit
    }
}
