//! XML and JSON processing for COBOL-2014.
//!
//! Implements the XML and JSON statements from the COBOL-2014 standard:
//! - XML GENERATE
//! - XML PARSE
//! - JSON GENERATE
//! - JSON PARSE

mod xml;
mod json;

pub use xml::*;
pub use json::*;

/// Result type for XML/JSON operations.
pub type XmlJsonResult<T> = Result<T, XmlJsonError>;

/// Errors from XML/JSON operations.
#[derive(Debug, Clone, PartialEq)]
pub enum XmlJsonError {
    /// Parse error with position
    ParseError { position: usize, message: String },
    /// Invalid data
    InvalidData(String),
    /// Missing required field
    MissingField(String),
    /// Type conversion error
    ConversionError(String),
    /// Buffer overflow
    Overflow,
    /// Encoding error
    EncodingError(String),
}

impl std::fmt::Display for XmlJsonError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ParseError { position, message } => {
                write!(f, "Parse error at position {}: {}", position, message)
            }
            Self::InvalidData(msg) => write!(f, "Invalid data: {}", msg),
            Self::MissingField(name) => write!(f, "Missing required field: {}", name),
            Self::ConversionError(msg) => write!(f, "Conversion error: {}", msg),
            Self::Overflow => write!(f, "Buffer overflow"),
            Self::EncodingError(msg) => write!(f, "Encoding error: {}", msg),
        }
    }
}

impl std::error::Error for XmlJsonError {}

/// A COBOL data field for generation/parsing.
#[derive(Debug, Clone)]
pub struct CobolField {
    /// Field name
    pub name: String,
    /// Level number
    pub level: u8,
    /// Value (as string representation)
    pub value: String,
    /// Field type
    pub field_type: FieldType,
    /// Child fields
    pub children: Vec<CobolField>,
}

/// Field type for XML/JSON mapping.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldType {
    /// Alphanumeric field
    Alphanumeric,
    /// Numeric field
    Numeric,
    /// Boolean field
    Boolean,
    /// Group field (contains children)
    Group,
}

impl CobolField {
    /// Create a new group field.
    pub fn group(name: &str, level: u8) -> Self {
        Self {
            name: name.to_string(),
            level,
            value: String::new(),
            field_type: FieldType::Group,
            children: Vec::new(),
        }
    }

    /// Create a new alphanumeric field.
    pub fn alphanumeric(name: &str, level: u8, value: &str) -> Self {
        Self {
            name: name.to_string(),
            level,
            value: value.to_string(),
            field_type: FieldType::Alphanumeric,
            children: Vec::new(),
        }
    }

    /// Create a new numeric field.
    pub fn numeric(name: &str, level: u8, value: &str) -> Self {
        Self {
            name: name.to_string(),
            level,
            value: value.to_string(),
            field_type: FieldType::Numeric,
            children: Vec::new(),
        }
    }

    /// Add a child field.
    pub fn add_child(&mut self, child: CobolField) {
        self.children.push(child);
    }

    /// Check if this is a group field.
    pub fn is_group(&self) -> bool {
        self.field_type == FieldType::Group
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cobol_field_creation() {
        let mut group = CobolField::group("CUSTOMER", 1);
        group.add_child(CobolField::alphanumeric("CUST-NAME", 5, "John"));
        group.add_child(CobolField::numeric("CUST-ID", 5, "12345"));

        assert!(group.is_group());
        assert_eq!(group.children.len(), 2);
    }

    #[test]
    fn test_error_display() {
        let err = XmlJsonError::ParseError {
            position: 42,
            message: "Unexpected token".to_string(),
        };
        assert!(err.to_string().contains("42"));
        assert!(err.to_string().contains("Unexpected token"));
    }
}
