//! BMS (Basic Mapping Support) for CICS.
//!
//! BMS is used to define screen layouts for 3270 terminal I/O.
//! This module handles:
//! - Parsing BMS map definitions
//! - Generating COBOL copybooks for symbolic maps
//! - Map rendering to 3270 data streams
//! - Map data extraction from terminal input

mod parser;
mod field;
mod render;
mod symbolic;

pub use parser::{BmsParser, BmsMap, BmsMapset};
pub use field::{BmsField, FieldAttribute, FieldJustify, FieldType};
pub use render::MapRenderer;
pub use symbolic::{SymbolicMapGenerator, decompose_from_buffer, decompose_from_display_string, compose_to_display_string};


/// Screen dimensions for 3270 terminals.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[derive(Default)]
pub enum ScreenSize {
    /// Model 2: 24x80 (default)
    #[default]
    Model2,
    /// Model 3: 32x80
    Model3,
    /// Model 4: 43x80
    Model4,
    /// Model 5: 27x132
    Model5,
}

impl ScreenSize {
    /// Get rows and columns.
    pub fn dimensions(&self) -> (usize, usize) {
        match self {
            ScreenSize::Model2 => (24, 80),
            ScreenSize::Model3 => (32, 80),
            ScreenSize::Model4 => (43, 80),
            ScreenSize::Model5 => (27, 132),
        }
    }

    /// Get total character positions.
    pub fn total_positions(&self) -> usize {
        let (rows, cols) = self.dimensions();
        rows * cols
    }
}


/// 3270 field attribute byte.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[derive(Default)]
pub struct AttributeByte(pub u8);

impl AttributeByte {
    /// Protected field.
    pub const PROTECTED: u8 = 0x20;
    /// Numeric field.
    pub const NUMERIC: u8 = 0x10;
    /// Bright (high intensity).
    pub const BRIGHT: u8 = 0x08;
    /// Non-display (dark).
    pub const DARK: u8 = 0x0C;
    /// MDT (Modified Data Tag).
    pub const MDT: u8 = 0x01;
    /// Skip field (protected + numeric).
    pub const SKIP: u8 = Self::PROTECTED | Self::NUMERIC;
    /// Autoskip field.
    pub const AUTOSKIP: u8 = Self::SKIP;

    /// Create a new attribute byte.
    pub fn new(protected: bool, numeric: bool, bright: bool, dark: bool, mdt: bool) -> Self {
        let mut value = 0u8;
        if protected {
            value |= Self::PROTECTED;
        }
        if numeric {
            value |= Self::NUMERIC;
        }
        if dark {
            value |= Self::DARK;
        } else if bright {
            value |= Self::BRIGHT;
        }
        if mdt {
            value |= Self::MDT;
        }
        Self(value)
    }

    /// Check if field is protected.
    pub fn is_protected(&self) -> bool {
        self.0 & Self::PROTECTED != 0
    }

    /// Check if field is numeric.
    pub fn is_numeric(&self) -> bool {
        self.0 & Self::NUMERIC != 0
    }

    /// Check if field is bright.
    pub fn is_bright(&self) -> bool {
        (self.0 & 0x0C) == Self::BRIGHT
    }

    /// Check if field is dark (non-display).
    pub fn is_dark(&self) -> bool {
        (self.0 & 0x0C) == Self::DARK
    }

    /// Check if MDT is set.
    pub fn is_modified(&self) -> bool {
        self.0 & Self::MDT != 0
    }
}


/// Extended attribute for 3270 fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct ExtendedAttribute {
    /// Color attribute.
    pub color: Option<FieldColor>,
    /// Highlighting attribute.
    pub highlight: Option<FieldHighlight>,
}

/// 3270 field colors.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

impl FieldColor {
    /// Get 3270 color code.
    pub fn code(&self) -> u8 {
        match self {
            FieldColor::Default => 0x00,
            FieldColor::Blue => 0xF1,
            FieldColor::Red => 0xF2,
            FieldColor::Pink => 0xF3,
            FieldColor::Green => 0xF4,
            FieldColor::Turquoise => 0xF5,
            FieldColor::Yellow => 0xF6,
            FieldColor::White => 0xF7,
        }
    }
}

/// 3270 field highlighting.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldHighlight {
    Normal,
    Blink,
    Reverse,
    Underscore,
}

impl FieldHighlight {
    /// Get 3270 highlight code.
    pub fn code(&self) -> u8 {
        match self {
            FieldHighlight::Normal => 0x00,
            FieldHighlight::Blink => 0xF1,
            FieldHighlight::Reverse => 0xF2,
            FieldHighlight::Underscore => 0xF4,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_screen_size() {
        assert_eq!(ScreenSize::Model2.dimensions(), (24, 80));
        assert_eq!(ScreenSize::Model2.total_positions(), 1920);
        assert_eq!(ScreenSize::Model5.dimensions(), (27, 132));
    }

    #[test]
    fn test_attribute_byte() {
        let attr = AttributeByte::new(true, false, true, false, false);
        assert!(attr.is_protected());
        assert!(!attr.is_numeric());
        assert!(attr.is_bright());
        assert!(!attr.is_dark());
        assert!(!attr.is_modified());
    }

    #[test]
    fn test_attribute_dark() {
        let attr = AttributeByte::new(true, false, false, true, false);
        assert!(attr.is_protected());
        assert!(attr.is_dark());
        assert!(!attr.is_bright());
    }

    #[test]
    fn test_color_codes() {
        assert_eq!(FieldColor::Blue.code(), 0xF1);
        assert_eq!(FieldColor::Green.code(), 0xF4);
        assert_eq!(FieldColor::Yellow.code(), 0xF6);
    }
}
