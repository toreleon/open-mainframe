//! BMS field definitions.
//!
//! Defines the structure and attributes of fields in a BMS map.

use super::{AttributeByte, ExtendedAttribute, FieldColor, FieldHighlight};

/// Type of BMS field.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FieldType {
    /// Alphanumeric input/output field
    #[default]
    Alphanumeric,
    /// Numeric input field
    Numeric,
    /// Protected (output only)
    Protected,
    /// Protected numeric
    ProtectedNumeric,
    /// Skip field (auto-skip when cursor reaches it)
    Skip,
}

/// Field justification.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FieldJustify {
    /// Left justified (default)
    #[default]
    Left,
    /// Right justified
    Right,
    /// Right justified with zero fill
    RightZero,
    /// Right justified with blank fill
    RightBlank,
}

/// Field attribute specification from BMS source.
#[derive(Debug, Clone, Default)]
pub struct FieldAttribute {
    /// Field is protected
    pub protected: bool,
    /// Field is numeric
    pub numeric: bool,
    /// Bright intensity
    pub bright: bool,
    /// Dark (non-display)
    pub dark: bool,
    /// Field is for input
    pub input: bool,
    /// Field is for output
    pub output: bool,
    /// Initial MDT setting
    pub modified: bool,
    /// Cursor initial position
    pub initial_cursor: bool,
    /// Full field (output fills entire length)
    pub full_field: bool,
    /// Justification
    pub justify: FieldJustify,
    /// Field color
    pub color: Option<FieldColor>,
    /// Field highlighting
    pub highlight: Option<FieldHighlight>,
}

impl FieldAttribute {
    /// Create input field attributes.
    pub fn input() -> Self {
        Self {
            input: true,
            output: true,
            ..Default::default()
        }
    }

    /// Create output field attributes.
    pub fn output() -> Self {
        Self {
            protected: true,
            output: true,
            ..Default::default()
        }
    }

    /// Create askip (auto-skip) field attributes.
    pub fn askip() -> Self {
        Self {
            protected: true,
            numeric: true,
            output: true,
            ..Default::default()
        }
    }

    /// Set bright intensity.
    pub fn with_bright(mut self) -> Self {
        self.bright = true;
        self.dark = false;
        self
    }

    /// Set dark (non-display).
    pub fn with_dark(mut self) -> Self {
        self.dark = true;
        self.bright = false;
        self
    }

    /// Set numeric.
    pub fn with_numeric(mut self) -> Self {
        self.numeric = true;
        self
    }

    /// Set color.
    pub fn with_color(mut self, color: FieldColor) -> Self {
        self.color = Some(color);
        self
    }

    /// Set justification.
    pub fn with_justify(mut self, justify: FieldJustify) -> Self {
        self.justify = justify;
        self
    }

    /// Convert to 3270 attribute byte.
    pub fn to_attribute_byte(&self) -> AttributeByte {
        AttributeByte::new(
            self.protected,
            self.numeric,
            self.bright,
            self.dark,
            self.modified,
        )
    }

    /// Convert to extended attributes.
    pub fn to_extended(&self) -> ExtendedAttribute {
        ExtendedAttribute {
            color: self.color,
            highlight: self.highlight,
        }
    }
}

/// A field in a BMS map.
#[derive(Debug, Clone)]
pub struct BmsField {
    /// Field name (used in symbolic map)
    pub name: String,
    /// Row position (1-based)
    pub row: usize,
    /// Column position (1-based)
    pub column: usize,
    /// Field length
    pub length: usize,
    /// Field type
    pub field_type: FieldType,
    /// Field attributes
    pub attributes: FieldAttribute,
    /// Initial value (for output fields)
    pub initial: Option<String>,
    /// Picture clause for formatting
    pub picture: Option<String>,
    /// Group name (for field groups)
    pub group: Option<String>,
    /// Occurs count (for array fields)
    pub occurs: Option<usize>,
}

impl BmsField {
    /// Create a new field.
    pub fn new(name: &str, row: usize, column: usize, length: usize) -> Self {
        Self {
            name: name.to_string(),
            row,
            column,
            length,
            field_type: FieldType::Alphanumeric,
            attributes: FieldAttribute::default(),
            initial: None,
            picture: None,
            group: None,
            occurs: None,
        }
    }

    /// Set field as input.
    pub fn input(mut self) -> Self {
        self.attributes = FieldAttribute::input();
        self
    }

    /// Set field as output.
    pub fn output(mut self) -> Self {
        self.field_type = FieldType::Protected;
        self.attributes = FieldAttribute::output();
        self
    }

    /// Set field as askip.
    pub fn askip(mut self) -> Self {
        self.field_type = FieldType::Skip;
        self.attributes = FieldAttribute::askip();
        self
    }

    /// Set initial value.
    pub fn with_initial(mut self, value: &str) -> Self {
        self.initial = Some(value.to_string());
        self
    }

    /// Set picture clause.
    pub fn with_picture(mut self, picture: &str) -> Self {
        self.picture = Some(picture.to_string());
        self
    }

    /// Set as numeric field.
    pub fn numeric(mut self) -> Self {
        self.field_type = FieldType::Numeric;
        self.attributes.numeric = true;
        self
    }

    /// Set bright intensity.
    pub fn bright(mut self) -> Self {
        self.attributes = self.attributes.with_bright();
        self
    }

    /// Set dark (non-display).
    pub fn dark(mut self) -> Self {
        self.attributes = self.attributes.with_dark();
        self
    }

    /// Set color.
    pub fn with_color(mut self, color: FieldColor) -> Self {
        self.attributes.color = Some(color);
        self
    }

    /// Set right justification.
    pub fn justify_right(mut self) -> Self {
        self.attributes.justify = FieldJustify::Right;
        self
    }

    /// Calculate buffer position (0-based).
    pub fn buffer_position(&self, columns: usize) -> usize {
        (self.row - 1) * columns + (self.column - 1)
    }

    /// Get attribute byte position (one before field).
    pub fn attribute_position(&self, columns: usize) -> usize {
        let pos = self.buffer_position(columns);
        if pos == 0 {
            columns * 24 - 1 // Wrap to end of screen
        } else {
            pos - 1
        }
    }

    /// Check if field is input capable.
    pub fn is_input(&self) -> bool {
        self.attributes.input && !self.attributes.protected
    }

    /// Check if field is output capable.
    pub fn is_output(&self) -> bool {
        self.attributes.output
    }

    /// Get COBOL data name for field.
    pub fn cobol_name(&self, suffix: &str) -> String {
        format!("{}{}", self.name.replace('-', "_"), suffix)
    }
}

/// Group of related fields.
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct FieldGroup {
    /// Group name
    pub name: String,
    /// Fields in the group
    pub fields: Vec<String>,
    /// Occurs count
    pub occurs: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_field_creation() {
        let field = BmsField::new("CUSTNO", 5, 15, 8)
            .input()
            .numeric();

        assert_eq!(field.name, "CUSTNO");
        assert_eq!(field.row, 5);
        assert_eq!(field.column, 15);
        assert_eq!(field.length, 8);
        assert!(field.is_input());
        assert_eq!(field.field_type, FieldType::Numeric);
    }

    #[test]
    fn test_output_field() {
        let field = BmsField::new("LABEL", 1, 1, 20)
            .output()
            .with_initial("Customer Name:");

        assert!(!field.is_input());
        assert!(field.is_output());
        assert_eq!(field.initial, Some("Customer Name:".to_string()));
    }

    #[test]
    fn test_buffer_position() {
        let field = BmsField::new("TEST", 2, 10, 5);
        // Row 2, Column 10 on 80-column screen
        // Position = (2-1) * 80 + (10-1) = 80 + 9 = 89
        assert_eq!(field.buffer_position(80), 89);
    }

    #[test]
    fn test_attribute_byte() {
        let attr = FieldAttribute::input()
            .with_numeric()
            .with_bright();

        let byte = attr.to_attribute_byte();
        assert!(!byte.is_protected());
        assert!(byte.is_numeric());
        assert!(byte.is_bright());
    }

    #[test]
    fn test_askip_field() {
        let field = BmsField::new("SKIP", 1, 1, 1).askip();

        assert_eq!(field.field_type, FieldType::Skip);
        assert!(field.attributes.protected);
        assert!(field.attributes.numeric);
    }

    #[test]
    fn test_field_with_color() {
        let field = BmsField::new("COLORED", 1, 1, 10)
            .with_color(FieldColor::Green);

        assert_eq!(field.attributes.color, Some(FieldColor::Green));
    }

    #[test]
    fn test_cobol_name() {
        let field = BmsField::new("CUST-NAME", 1, 1, 30);
        assert_eq!(field.cobol_name("I"), "CUST_NAMEI");
        assert_eq!(field.cobol_name("O"), "CUST_NAMEO");
    }
}
