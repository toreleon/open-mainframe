//! BMS map rendering to 3270 data streams.
//!
//! Generates 3270 output data streams from BMS maps.

use super::field::BmsField;
use super::parser::BmsMap;
use super::{AttributeByte, ScreenSize};
use std::collections::HashMap;

/// 3270 extended attribute type codes.
#[allow(dead_code)]
pub mod attr_types {
    /// Basic 3270 field attribute.
    pub const BASIC_3270: u8 = 0xC0;
    /// Extended color attribute.
    pub const COLOR: u8 = 0x41;
    /// Extended highlighting attribute.
    pub const HIGHLIGHT: u8 = 0x42;
}

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

        // Determine if we need extended attributes (SFE)
        let has_color = field.attributes.color.is_some();
        let has_highlight = field.attributes.highlight.is_some();

        if has_color || has_highlight {
            // Use SFE (Start Field Extended) with attribute pairs
            self.write_sfe(field);
        } else {
            // Use basic SF (Start Field)
            let attr = field.attributes.to_attribute_byte();
            self.buffer.push(orders::SF);
            self.buffer.push(self.encode_attribute(attr));
        }

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

    /// Write SFE (Start Field Extended) order with color/highlight attributes.
    ///
    /// SFE format: 0x29, count, [type, value]...
    /// - Type 0xC0: basic 3270 attribute
    /// - Type 0x41: color
    /// - Type 0x42: highlight
    fn write_sfe(&mut self, field: &BmsField) {
        let attr = field.attributes.to_attribute_byte();
        let ext = field.attributes.to_extended();

        // Count attribute pairs: always basic + optional color + optional highlight
        let mut pair_count = 1u8; // basic attribute
        if ext.color.is_some() {
            pair_count += 1;
        }
        if ext.highlight.is_some() {
            pair_count += 1;
        }

        self.buffer.push(orders::SFE);
        self.buffer.push(pair_count);

        // Basic 3270 field attribute
        self.buffer.push(attr_types::BASIC_3270);
        self.buffer.push(self.encode_attribute(attr));

        // Extended color
        if let Some(color) = ext.color {
            self.buffer.push(attr_types::COLOR);
            self.buffer.push(color.code());
        }

        // Extended highlighting
        if let Some(highlight) = ext.highlight {
            self.buffer.push(attr_types::HIGHLIGHT);
            self.buffer.push(highlight.code());
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
        use open_mainframe_encoding::CP037;
        CP037.ascii_to_ebcdic_byte(ascii)
    }

    /// Translate an EBCDIC byte back to ASCII using Code Page 037.
    #[cfg(test)]
    fn translate_to_ascii(&self, ebcdic: u8) -> u8 {
        use open_mainframe_encoding::CP037;
        CP037.ebcdic_to_ascii_byte(ebcdic)
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

        // Letters
        assert_eq!(renderer.translate_to_ebcdic(b'A'), 0xC1);
        assert_eq!(renderer.translate_to_ebcdic(b'Z'), 0xE9);
        assert_eq!(renderer.translate_to_ebcdic(b'a'), 0x81);
        assert_eq!(renderer.translate_to_ebcdic(b'z'), 0xA9);

        // Digits
        assert_eq!(renderer.translate_to_ebcdic(b'0'), 0xF0);
        assert_eq!(renderer.translate_to_ebcdic(b'9'), 0xF9);

        // Space
        assert_eq!(renderer.translate_to_ebcdic(b' '), 0x40);

        // Special characters that were in the old 33-char table
        assert_eq!(renderer.translate_to_ebcdic(b'.'), 0x4B);
        assert_eq!(renderer.translate_to_ebcdic(b','), 0x6B);
        assert_eq!(renderer.translate_to_ebcdic(b'$'), 0x5B);
        assert_eq!(renderer.translate_to_ebcdic(b'#'), 0x7B);
        assert_eq!(renderer.translate_to_ebcdic(b'@'), 0x7C);
    }

    #[test]
    fn test_ebcdic_translation_extended_chars() {
        // Story 204.1: Characters that were NOT in the old 33-char stub
        let renderer = MapRenderer::new(ScreenSize::Model2);

        // Brackets, braces, pipe — these previously defaulted to space
        assert_eq!(renderer.translate_to_ebcdic(b'['), 0xAD);
        assert_eq!(renderer.translate_to_ebcdic(b']'), 0xBD);
        assert_eq!(renderer.translate_to_ebcdic(b'{'), 0xC0);
        assert_eq!(renderer.translate_to_ebcdic(b'}'), 0xD0);
        assert_eq!(renderer.translate_to_ebcdic(b'|'), 0x4F);
        assert_eq!(renderer.translate_to_ebcdic(b'\\'), 0xE0);
        assert_eq!(renderer.translate_to_ebcdic(b'^'), 0x5F);
        assert_eq!(renderer.translate_to_ebcdic(b'~'), 0xA1);
        assert_eq!(renderer.translate_to_ebcdic(b'`'), 0x79);
    }

    #[test]
    fn test_ebcdic_roundtrip() {
        // Story 204.1: Any printable ASCII round-trips correctly
        let renderer = MapRenderer::new(ScreenSize::Model2);

        for ch in 0x20u8..0x7F {
            let ebcdic = renderer.translate_to_ebcdic(ch);
            let back = renderer.translate_to_ascii(ebcdic);
            assert_eq!(back, ch, "Roundtrip failed for ASCII 0x{:02X} ('{}')", ch, ch as char);
        }
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

    // === Story 209.1: SFE Extended Attribute Rendering ===

    #[test]
    fn test_render_field_with_color() {
        // AC: Given a BMS field with COLOR=RED
        // When rendered to a 3270 data stream
        // Then SFE order includes color attribute bytes
        let source = r#"
TEST     DFHMSD TYPE=MAP,LANG=COBOL
TESTM    DFHMDI SIZE=(24,80)
ERRMSG   DFHMDF POS=(10,5),LENGTH=40,ATTRB=(PROT),COLOR=RED
         DFHMSD TYPE=FINAL
"#;

        let mut parser = BmsParser::new();
        let mapset = parser.parse(source).unwrap();
        let map = &mapset.maps[0];

        let mut renderer = MapRenderer::new(ScreenSize::Model2);
        let stream = renderer.render(map, true);

        // Find SFE order in the stream (0x29)
        let sfe_pos = stream.iter().position(|&b| b == orders::SFE);
        assert!(sfe_pos.is_some(), "SFE order not found in stream");
        let pos = sfe_pos.unwrap();

        // After SFE: pair count, then pairs
        let pair_count = stream[pos + 1];
        assert_eq!(pair_count, 2); // basic + color

        // Basic attribute pair
        assert_eq!(stream[pos + 2], attr_types::BASIC_3270);
        // stream[pos + 3] = encoded basic attribute

        // Color attribute pair
        assert_eq!(stream[pos + 4], attr_types::COLOR);
        assert_eq!(stream[pos + 5], 0xF2); // RED = 0xF2
    }

    #[test]
    fn test_render_field_with_highlight() {
        let source = r#"
TEST     DFHMSD TYPE=MAP,LANG=COBOL
TESTM    DFHMDI SIZE=(24,80)
WARN     DFHMDF POS=(12,5),LENGTH=30,ATTRB=(PROT),HILIGHT=REVERSE
         DFHMSD TYPE=FINAL
"#;

        let mut parser = BmsParser::new();
        let mapset = parser.parse(source).unwrap();
        let map = &mapset.maps[0];

        let mut renderer = MapRenderer::new(ScreenSize::Model2);
        let stream = renderer.render(map, true);

        let sfe_pos = stream.iter().position(|&b| b == orders::SFE);
        assert!(sfe_pos.is_some(), "SFE order not found");
        let pos = sfe_pos.unwrap();

        let pair_count = stream[pos + 1];
        assert_eq!(pair_count, 2); // basic + highlight

        // Highlight attribute pair
        assert_eq!(stream[pos + 4], attr_types::HIGHLIGHT);
        assert_eq!(stream[pos + 5], 0xF2); // REVERSE = 0xF2
    }

    #[test]
    fn test_render_field_with_color_and_highlight() {
        // AC: Given COLOR=RED and HILIGHT=REVERSE
        // Then SFE orders include both color and highlight attribute bytes
        let source = r#"
TEST     DFHMSD TYPE=MAP,LANG=COBOL
TESTM    DFHMDI SIZE=(24,80)
ALERT    DFHMDF POS=(15,10),LENGTH=20,ATTRB=(PROT,BRT),COLOR=RED,HILIGHT=REVERSE
         DFHMSD TYPE=FINAL
"#;

        let mut parser = BmsParser::new();
        let mapset = parser.parse(source).unwrap();
        let map = &mapset.maps[0];

        let mut renderer = MapRenderer::new(ScreenSize::Model2);
        let stream = renderer.render(map, true);

        let sfe_pos = stream.iter().position(|&b| b == orders::SFE);
        assert!(sfe_pos.is_some(), "SFE order not found");
        let pos = sfe_pos.unwrap();

        // 3 pairs: basic + color + highlight
        assert_eq!(stream[pos + 1], 3);

        // Basic attribute
        assert_eq!(stream[pos + 2], attr_types::BASIC_3270);

        // Color (RED)
        assert_eq!(stream[pos + 4], attr_types::COLOR);
        assert_eq!(stream[pos + 5], 0xF2);

        // Highlight (REVERSE)
        assert_eq!(stream[pos + 6], attr_types::HIGHLIGHT);
        assert_eq!(stream[pos + 7], 0xF2);
    }

    #[test]
    fn test_render_field_without_extended_uses_sf() {
        // Fields without color/highlight should still use basic SF
        let source = r#"
TEST     DFHMSD TYPE=MAP,LANG=COBOL
TESTM    DFHMDI SIZE=(24,80)
PLAIN    DFHMDF POS=(3,5),LENGTH=10,ATTRB=(PROT)
         DFHMSD TYPE=FINAL
"#;

        let mut parser = BmsParser::new();
        let mapset = parser.parse(source).unwrap();
        let map = &mapset.maps[0];

        let mut renderer = MapRenderer::new(ScreenSize::Model2);
        let stream = renderer.render(map, true);

        // Should have SF but NOT SFE
        assert!(stream.contains(&orders::SF), "SF order should be present");
        assert!(!stream.contains(&orders::SFE), "SFE should NOT be present for plain field");
    }

    #[test]
    fn test_render_mixed_fields() {
        // Mix of fields: some with color, some without
        let source = r#"
TEST     DFHMSD TYPE=MAP,LANG=COBOL
TESTM    DFHMDI SIZE=(24,80)
TITLE    DFHMDF POS=(1,1),LENGTH=20,ATTRB=(ASKIP)
ERROR    DFHMDF POS=(24,1),LENGTH=60,ATTRB=(PROT),COLOR=RED,HILIGHT=BLINK
         DFHMSD TYPE=FINAL
"#;

        let mut parser = BmsParser::new();
        let mapset = parser.parse(source).unwrap();
        let map = &mapset.maps[0];

        let mut renderer = MapRenderer::new(ScreenSize::Model2);
        let stream = renderer.render(map, true);

        // Should have both SF (for TITLE) and SFE (for ERROR)
        assert!(stream.contains(&orders::SF));
        assert!(stream.contains(&orders::SFE));
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
