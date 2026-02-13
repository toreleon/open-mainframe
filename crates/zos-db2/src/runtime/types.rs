//! DB2 to PostgreSQL data type mapping.
//!
//! Handles conversion between DB2 data types and PostgreSQL equivalents.

use std::collections::HashMap;

/// DB2 data type representation.
#[derive(Debug, Clone, PartialEq)]
pub enum Db2Type {
    /// CHAR(n) - fixed-length character string
    Char(u32),
    /// VARCHAR(n) - variable-length character string
    Varchar(u32),
    /// INTEGER - 32-bit signed integer
    Integer,
    /// SMALLINT - 16-bit signed integer
    Smallint,
    /// BIGINT - 64-bit signed integer
    Bigint,
    /// DECIMAL(p, s) - exact numeric
    Decimal(u8, u8),
    /// NUMERIC(p, s) - exact numeric (alias for DECIMAL)
    Numeric(u8, u8),
    /// FLOAT - approximate numeric
    Float,
    /// DOUBLE - double precision
    Double,
    /// REAL - single precision
    Real,
    /// DATE - date value
    Date,
    /// TIME - time value
    Time,
    /// TIMESTAMP - date and time
    Timestamp,
    /// BLOB - binary large object
    Blob,
    /// CLOB - character large object
    Clob,
    /// GRAPHIC(n) - double-byte character
    Graphic(u32),
    /// VARGRAPHIC(n) - variable double-byte character
    Vargraphic(u32),
    /// BINARY(n) - fixed-length binary
    Binary(u32),
    /// VARBINARY(n) - variable-length binary
    Varbinary(u32),
}

impl Db2Type {
    /// Parse a DB2 type from a string.
    pub fn parse(type_str: &str) -> Option<Self> {
        let upper = type_str.to_uppercase();
        let trimmed = upper.trim();

        // Handle types with parameters
        if let Some(idx) = trimmed.find('(') {
            let base = &trimmed[..idx];
            let params = &trimmed[idx + 1..trimmed.len() - 1];

            match base {
                "CHAR" | "CHARACTER" => {
                    let len: u32 = params.trim().parse().ok()?;
                    Some(Db2Type::Char(len))
                }
                "VARCHAR" => {
                    let len: u32 = params.trim().parse().ok()?;
                    Some(Db2Type::Varchar(len))
                }
                "DECIMAL" | "DEC" => {
                    let parts: Vec<&str> = params.split(',').collect();
                    let precision: u8 = parts.first()?.trim().parse().ok()?;
                    let scale: u8 = parts.get(1).and_then(|s| s.trim().parse().ok()).unwrap_or(0);
                    Some(Db2Type::Decimal(precision, scale))
                }
                "NUMERIC" | "NUM" => {
                    let parts: Vec<&str> = params.split(',').collect();
                    let precision: u8 = parts.first()?.trim().parse().ok()?;
                    let scale: u8 = parts.get(1).and_then(|s| s.trim().parse().ok()).unwrap_or(0);
                    Some(Db2Type::Numeric(precision, scale))
                }
                "GRAPHIC" => {
                    let len: u32 = params.trim().parse().ok()?;
                    Some(Db2Type::Graphic(len))
                }
                "VARGRAPHIC" => {
                    let len: u32 = params.trim().parse().ok()?;
                    Some(Db2Type::Vargraphic(len))
                }
                "BINARY" => {
                    let len: u32 = params.trim().parse().ok()?;
                    Some(Db2Type::Binary(len))
                }
                "VARBINARY" => {
                    let len: u32 = params.trim().parse().ok()?;
                    Some(Db2Type::Varbinary(len))
                }
                _ => None,
            }
        } else {
            // Simple types without parameters
            match trimmed {
                "INTEGER" | "INT" => Some(Db2Type::Integer),
                "SMALLINT" => Some(Db2Type::Smallint),
                "BIGINT" => Some(Db2Type::Bigint),
                "FLOAT" => Some(Db2Type::Float),
                "DOUBLE" | "DOUBLE PRECISION" => Some(Db2Type::Double),
                "REAL" => Some(Db2Type::Real),
                "DATE" => Some(Db2Type::Date),
                "TIME" => Some(Db2Type::Time),
                "TIMESTAMP" => Some(Db2Type::Timestamp),
                "BLOB" => Some(Db2Type::Blob),
                "CLOB" => Some(Db2Type::Clob),
                _ => None,
            }
        }
    }

    /// Convert to PostgreSQL type string.
    pub fn to_postgres(&self) -> String {
        match self {
            Db2Type::Char(n) => format!("CHAR({})", n),
            Db2Type::Varchar(n) => format!("VARCHAR({})", n),
            Db2Type::Integer => "INTEGER".to_string(),
            Db2Type::Smallint => "SMALLINT".to_string(),
            Db2Type::Bigint => "BIGINT".to_string(),
            Db2Type::Decimal(p, s) => format!("NUMERIC({}, {})", p, s),
            Db2Type::Numeric(p, s) => format!("NUMERIC({}, {})", p, s),
            Db2Type::Float => "DOUBLE PRECISION".to_string(),
            Db2Type::Double => "DOUBLE PRECISION".to_string(),
            Db2Type::Real => "REAL".to_string(),
            Db2Type::Date => "DATE".to_string(),
            Db2Type::Time => "TIME".to_string(),
            Db2Type::Timestamp => "TIMESTAMP".to_string(),
            Db2Type::Blob => "BYTEA".to_string(),
            Db2Type::Clob => "TEXT".to_string(),
            Db2Type::Graphic(n) => format!("VARCHAR({})", n * 2), // Double-byte to UTF-8
            Db2Type::Vargraphic(n) => format!("VARCHAR({})", n * 2),
            Db2Type::Binary(_) => "BYTEA".to_string(), // PostgreSQL doesn't have fixed binary
            Db2Type::Varbinary(_) => "BYTEA".to_string(),
        }
    }

    /// Get the Rust type for this DB2 type.
    pub fn to_rust_type(&self) -> &'static str {
        match self {
            Db2Type::Char(_) | Db2Type::Varchar(_) | Db2Type::Clob => "String",
            Db2Type::Integer => "i32",
            Db2Type::Smallint => "i16",
            Db2Type::Bigint => "i64",
            Db2Type::Decimal(_, _) | Db2Type::Numeric(_, _) => "rust_decimal::Decimal",
            Db2Type::Float | Db2Type::Double => "f64",
            Db2Type::Real => "f32",
            Db2Type::Date => "chrono::NaiveDate",
            Db2Type::Time => "chrono::NaiveTime",
            Db2Type::Timestamp => "chrono::NaiveDateTime",
            Db2Type::Blob | Db2Type::Binary(_) | Db2Type::Varbinary(_) => "Vec<u8>",
            Db2Type::Graphic(_) | Db2Type::Vargraphic(_) => "String",
        }
    }
}

/// Type mapping configuration.
pub struct TypeMapping {
    /// Custom type overrides
    overrides: HashMap<String, String>,
}

impl TypeMapping {
    /// Create a new type mapping with default conversions.
    pub fn new() -> Self {
        Self {
            overrides: HashMap::new(),
        }
    }

    /// Add a custom type override.
    pub fn add_override(&mut self, db2_type: &str, pg_type: &str) {
        self.overrides.insert(db2_type.to_uppercase(), pg_type.to_string());
    }

    /// Convert a DB2 type to PostgreSQL.
    pub fn convert(&self, db2_type: &str) -> String {
        // Check for custom override first
        if let Some(pg_type) = self.overrides.get(&db2_type.to_uppercase()) {
            return pg_type.clone();
        }

        // Parse and convert
        if let Some(parsed) = Db2Type::parse(db2_type) {
            parsed.to_postgres()
        } else {
            // Unknown type - return as-is with a comment
            format!("/* unknown: {} */ TEXT", db2_type)
        }
    }

    /// Convert a DB2 type to Rust type.
    pub fn to_rust(&self, db2_type: &str) -> &'static str {
        if let Some(parsed) = Db2Type::parse(db2_type) {
            parsed.to_rust_type()
        } else {
            "String" // Default fallback
        }
    }
}

impl Default for TypeMapping {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert COBOL PICTURE to approximate DB2 type.
pub fn picture_to_db2_type(picture: &str) -> Option<Db2Type> {
    let upper = picture.to_uppercase();
    let clean = upper.replace(' ', "");

    // Count effective characters (handle repeat notation like X(10) or 9(5))
    let x_count = count_picture_chars(&clean, 'X');
    let nine_count = count_picture_chars(&clean, '9');
    let v_present = clean.contains('V');
    let s_present = clean.contains('S');

    if x_count > 0 {
        // Alphanumeric - CHAR or VARCHAR
        Some(Db2Type::Char(x_count))
    } else if nine_count > 0 {
        if v_present {
            // Has decimal point - DECIMAL
            // Count digits after V for scale
            let after_v = clean.split('V').nth(1).unwrap_or("");
            let scale = count_picture_chars(after_v, '9');
            Some(Db2Type::Decimal(nine_count as u8, scale as u8))
        } else if s_present || nine_count <= 4 {
            Some(Db2Type::Smallint)
        } else if nine_count <= 9 {
            Some(Db2Type::Integer)
        } else {
            Some(Db2Type::Bigint)
        }
    } else {
        None
    }
}

/// Count effective picture characters, handling repeat notation like X(10).
fn count_picture_chars(picture: &str, ch: char) -> u32 {
    let mut count = 0u32;
    let chars: Vec<char> = picture.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        if chars[i] == ch {
            // Check for repeat notation
            if i + 1 < chars.len() && chars[i + 1] == '(' {
                // Find closing paren
                if let Some(end) = picture[i + 2..].find(')') {
                    let num_str = &picture[i + 2..i + 2 + end];
                    if let Ok(n) = num_str.parse::<u32>() {
                        count += n;
                        i += 3 + end;
                        continue;
                    }
                }
            }
            count += 1;
        }
        i += 1;
    }

    count
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_char() {
        assert_eq!(Db2Type::parse("CHAR(10)"), Some(Db2Type::Char(10)));
        assert_eq!(Db2Type::parse("CHARACTER(5)"), Some(Db2Type::Char(5)));
    }

    #[test]
    fn test_parse_varchar() {
        assert_eq!(Db2Type::parse("VARCHAR(100)"), Some(Db2Type::Varchar(100)));
    }

    #[test]
    fn test_parse_integer() {
        assert_eq!(Db2Type::parse("INTEGER"), Some(Db2Type::Integer));
        assert_eq!(Db2Type::parse("INT"), Some(Db2Type::Integer));
    }

    #[test]
    fn test_parse_decimal() {
        assert_eq!(Db2Type::parse("DECIMAL(10, 2)"), Some(Db2Type::Decimal(10, 2)));
        assert_eq!(Db2Type::parse("DEC(5,0)"), Some(Db2Type::Decimal(5, 0)));
    }

    #[test]
    fn test_to_postgres() {
        assert_eq!(Db2Type::Char(10).to_postgres(), "CHAR(10)");
        assert_eq!(Db2Type::Blob.to_postgres(), "BYTEA");
        assert_eq!(Db2Type::Clob.to_postgres(), "TEXT");
        assert_eq!(Db2Type::Decimal(10, 2).to_postgres(), "NUMERIC(10, 2)");
    }

    #[test]
    fn test_to_rust_type() {
        assert_eq!(Db2Type::Integer.to_rust_type(), "i32");
        assert_eq!(Db2Type::Varchar(100).to_rust_type(), "String");
        assert_eq!(Db2Type::Timestamp.to_rust_type(), "chrono::NaiveDateTime");
    }

    #[test]
    fn test_type_mapping() {
        let mapping = TypeMapping::new();
        assert_eq!(mapping.convert("INTEGER"), "INTEGER");
        assert_eq!(mapping.convert("CLOB"), "TEXT");
        assert_eq!(mapping.convert("BLOB"), "BYTEA");
    }

    #[test]
    fn test_type_mapping_override() {
        let mut mapping = TypeMapping::new();
        mapping.add_override("CLOB", "JSONB");
        assert_eq!(mapping.convert("CLOB"), "JSONB");
    }

    #[test]
    fn test_picture_to_db2_type() {
        assert_eq!(picture_to_db2_type("X(10)"), Some(Db2Type::Char(10)));
        assert_eq!(picture_to_db2_type("9(5)V99"), Some(Db2Type::Decimal(7, 2)));
        assert_eq!(picture_to_db2_type("S9(4)"), Some(Db2Type::Smallint));
    }

    #[test]
    fn test_unknown_type() {
        let mapping = TypeMapping::new();
        let result = mapping.convert("CUSTOM_TYPE");
        assert!(result.contains("TEXT"));
        assert!(result.contains("unknown"));
    }
}
