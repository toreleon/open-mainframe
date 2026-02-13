//! JSON GENERATE and JSON PARSE implementation.

use super::{CobolField, FieldType, XmlJsonError, XmlJsonResult};

/// Options for JSON GENERATE.
#[derive(Debug, Clone, Default)]
pub struct JsonGenerateOptions {
    /// Pretty print with indentation
    pub pretty_print: bool,
    /// Suppress null values
    pub suppress_nulls: bool,
    /// Suppress numeric leading zeros
    pub suppress_zeros: bool,
    /// Custom name mappings
    pub name_of: Vec<(String, String)>,
    /// Convert names to camelCase
    pub camel_case: bool,
}

/// Generate JSON from a COBOL data structure.
///
/// Implements the COBOL JSON GENERATE statement.
pub fn json_generate(data: &CobolField, options: &JsonGenerateOptions) -> XmlJsonResult<String> {
    let mut output = String::new();
    generate_value(&mut output, data, options, 0)?;
    Ok(output)
}

/// Generate a JSON value.
fn generate_value(
    output: &mut String,
    field: &CobolField,
    options: &JsonGenerateOptions,
    depth: usize,
) -> XmlJsonResult<()> {
    let indent = if options.pretty_print {
        "  ".repeat(depth)
    } else {
        String::new()
    };
    let newline = if options.pretty_print { "\n" } else { "" };
    let space = if options.pretty_print { " " } else { "" };

    // Get property name
    let name = get_mapped_name(&field.name, &options.name_of, options.camel_case);

    if field.is_group() {
        // Object
        output.push_str(&format!("{}\"{}\":{}{{", indent, name, space));
        output.push_str(newline);

        let children: Vec<_> = field
            .children
            .iter()
            .filter(|c| !options.suppress_nulls || !c.value.is_empty() || c.is_group())
            .collect();

        for (i, child) in children.iter().enumerate() {
            generate_value(output, child, options, depth + 1)?;
            if i < children.len() - 1 {
                output.push(',');
            }
            output.push_str(newline);
        }

        output.push_str(&format!("{}}}", indent));
    } else {
        // Leaf value
        let value = format_json_value(field, options);
        output.push_str(&format!("{}\"{}\":{}{}",indent, name, space, value));
    }

    Ok(())
}

/// Generate JSON for a standalone value (no wrapping object).
pub fn json_generate_value(data: &CobolField, options: &JsonGenerateOptions) -> XmlJsonResult<String> {
    if data.is_group() {
        let mut output = String::new();
        let newline = if options.pretty_print { "\n" } else { "" };

        output.push('{');
        output.push_str(newline);

        let children: Vec<_> = data
            .children
            .iter()
            .filter(|c| !options.suppress_nulls || !c.value.is_empty() || c.is_group())
            .collect();

        for (i, child) in children.iter().enumerate() {
            generate_value(&mut output, child, options, 1)?;
            if i < children.len() - 1 {
                output.push(',');
            }
            output.push_str(newline);
        }

        output.push('}');
        Ok(output)
    } else {
        Ok(format_json_value(data, options))
    }
}

/// Format a COBOL field value as JSON.
fn format_json_value(field: &CobolField, options: &JsonGenerateOptions) -> String {
    match field.field_type {
        FieldType::Numeric => {
            let value = if options.suppress_zeros {
                suppress_leading_zeros(&field.value)
            } else {
                field.value.clone()
            };
            // Return as JSON number (no quotes)
            if value.is_empty() {
                "0".to_string()
            } else {
                value
            }
        }
        FieldType::Boolean => {
            let lower = field.value.to_ascii_lowercase();
            if lower == "true" || lower == "1" || lower == "y" || lower == "yes" {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        FieldType::Alphanumeric | FieldType::Group => {
            // String value with escaping
            format!("\"{}\"", escape_json(&field.value))
        }
    }
}

/// Suppress leading zeros in numeric values.
fn suppress_leading_zeros(s: &str) -> String {
    // Handle negative numbers
    let (sign, num) = if s.starts_with('-') {
        ("-", &s[1..])
    } else {
        ("", s)
    };

    // Handle decimal points
    if let Some(dot_pos) = num.find('.') {
        let int_part = &num[..dot_pos];
        let dec_part = &num[dot_pos..];
        let trimmed = int_part.trim_start_matches('0');
        if trimmed.is_empty() {
            format!("{}0{}", sign, dec_part)
        } else {
            format!("{}{}{}", sign, trimmed, dec_part)
        }
    } else {
        let trimmed = num.trim_start_matches('0');
        if trimmed.is_empty() {
            "0".to_string()
        } else {
            format!("{}{}", sign, trimmed)
        }
    }
}

/// Get mapped name with optional camelCase conversion.
fn get_mapped_name(name: &str, name_of: &[(String, String)], camel_case: bool) -> String {
    // Check explicit mappings first
    for (from, to) in name_of {
        if from.eq_ignore_ascii_case(name) {
            return to.clone();
        }
    }

    if camel_case {
        to_camel_case(name)
    } else {
        // Convert COBOL name (with hyphens) to underscores
        name.replace('-', "_").to_lowercase()
    }
}

/// Convert COBOL-style name to camelCase.
fn to_camel_case(name: &str) -> String {
    let mut result = String::new();
    let mut capitalize_next = false;

    for c in name.chars() {
        if c == '-' || c == '_' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(c.to_ascii_uppercase());
            capitalize_next = false;
        } else {
            result.push(c.to_ascii_lowercase());
        }
    }

    result
}

/// Escape special JSON characters.
fn escape_json(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            c if c.is_control() => {
                result.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => result.push(c),
        }
    }
    result
}

/// JSON value types.
#[derive(Debug, Clone, PartialEq)]
pub enum JsonValue {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<JsonValue>),
    Object(Vec<(String, JsonValue)>),
}

impl JsonValue {
    /// Check if this is a null value.
    pub fn is_null(&self) -> bool {
        matches!(self, JsonValue::Null)
    }

    /// Get as boolean.
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            JsonValue::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// Get as number.
    pub fn as_number(&self) -> Option<f64> {
        match self {
            JsonValue::Number(n) => Some(*n),
            _ => None,
        }
    }

    /// Get as string.
    pub fn as_str(&self) -> Option<&str> {
        match self {
            JsonValue::String(s) => Some(s),
            _ => None,
        }
    }

    /// Get as array.
    pub fn as_array(&self) -> Option<&[JsonValue]> {
        match self {
            JsonValue::Array(arr) => Some(arr),
            _ => None,
        }
    }

    /// Get object property.
    pub fn get(&self, key: &str) -> Option<&JsonValue> {
        match self {
            JsonValue::Object(props) => props.iter().find(|(k, _)| k == key).map(|(_, v)| v),
            _ => None,
        }
    }
}

/// JSON Parser.
#[derive(Debug)]
pub struct JsonParser {
    input: Vec<char>,
    pos: usize,
}

impl JsonParser {
    /// Create a new parser.
    pub fn new(json: &str) -> Self {
        Self {
            input: json.chars().collect(),
            pos: 0,
        }
    }

    /// Parse the JSON input.
    pub fn parse(&mut self) -> XmlJsonResult<JsonValue> {
        self.skip_whitespace();
        let value = self.parse_value()?;
        self.skip_whitespace();

        if self.pos < self.input.len() {
            return Err(XmlJsonError::ParseError {
                position: self.pos,
                message: "Unexpected content after JSON value".to_string(),
            });
        }

        Ok(value)
    }

    fn parse_value(&mut self) -> XmlJsonResult<JsonValue> {
        self.skip_whitespace();

        if self.pos >= self.input.len() {
            return Err(XmlJsonError::ParseError {
                position: self.pos,
                message: "Unexpected end of input".to_string(),
            });
        }

        match self.current() {
            'n' => self.parse_null(),
            't' | 'f' => self.parse_bool(),
            '"' => self.parse_string().map(JsonValue::String),
            '[' => self.parse_array(),
            '{' => self.parse_object(),
            c if c == '-' || c.is_ascii_digit() => self.parse_number(),
            c => Err(XmlJsonError::ParseError {
                position: self.pos,
                message: format!("Unexpected character: {}", c),
            }),
        }
    }

    fn parse_null(&mut self) -> XmlJsonResult<JsonValue> {
        if self.consume_literal("null") {
            Ok(JsonValue::Null)
        } else {
            Err(XmlJsonError::ParseError {
                position: self.pos,
                message: "Expected 'null'".to_string(),
            })
        }
    }

    fn parse_bool(&mut self) -> XmlJsonResult<JsonValue> {
        if self.consume_literal("true") {
            Ok(JsonValue::Bool(true))
        } else if self.consume_literal("false") {
            Ok(JsonValue::Bool(false))
        } else {
            Err(XmlJsonError::ParseError {
                position: self.pos,
                message: "Expected 'true' or 'false'".to_string(),
            })
        }
    }

    fn parse_string(&mut self) -> XmlJsonResult<String> {
        if self.current() != '"' {
            return Err(XmlJsonError::ParseError {
                position: self.pos,
                message: "Expected '\"'".to_string(),
            });
        }
        self.advance();

        let mut result = String::new();
        while self.pos < self.input.len() && self.current() != '"' {
            if self.current() == '\\' {
                self.advance();
                if self.pos >= self.input.len() {
                    return Err(XmlJsonError::ParseError {
                        position: self.pos,
                        message: "Unexpected end of string".to_string(),
                    });
                }
                match self.current() {
                    '"' => result.push('"'),
                    '\\' => result.push('\\'),
                    '/' => result.push('/'),
                    'b' => result.push('\x08'),
                    'f' => result.push('\x0c'),
                    'n' => result.push('\n'),
                    'r' => result.push('\r'),
                    't' => result.push('\t'),
                    'u' => {
                        self.advance();
                        let hex: String = (0..4)
                            .filter_map(|_| {
                                if self.pos < self.input.len() {
                                    let c = self.current();
                                    self.advance();
                                    Some(c)
                                } else {
                                    None
                                }
                            })
                            .collect();
                        if hex.len() != 4 {
                            return Err(XmlJsonError::ParseError {
                                position: self.pos,
                                message: "Invalid unicode escape".to_string(),
                            });
                        }
                        let code = u32::from_str_radix(&hex, 16).map_err(|_| {
                            XmlJsonError::ParseError {
                                position: self.pos,
                                message: "Invalid unicode escape".to_string(),
                            }
                        })?;
                        if let Some(c) = char::from_u32(code) {
                            result.push(c);
                        }
                        continue; // Already advanced past the hex digits
                    }
                    c => {
                        return Err(XmlJsonError::ParseError {
                            position: self.pos,
                            message: format!("Invalid escape character: {}", c),
                        });
                    }
                }
                self.advance();
            } else {
                result.push(self.current());
                self.advance();
            }
        }

        if self.pos >= self.input.len() {
            return Err(XmlJsonError::ParseError {
                position: self.pos,
                message: "Unterminated string".to_string(),
            });
        }
        self.advance(); // Skip closing quote

        Ok(result)
    }

    fn parse_number(&mut self) -> XmlJsonResult<JsonValue> {
        let start = self.pos;

        // Optional negative sign
        if self.current() == '-' {
            self.advance();
        }

        // Integer part
        if self.current() == '0' {
            self.advance();
        } else if self.current().is_ascii_digit() {
            while self.pos < self.input.len() && self.current().is_ascii_digit() {
                self.advance();
            }
        } else {
            return Err(XmlJsonError::ParseError {
                position: self.pos,
                message: "Expected digit".to_string(),
            });
        }

        // Fractional part
        if self.pos < self.input.len() && self.current() == '.' {
            self.advance();
            if self.pos >= self.input.len() || !self.current().is_ascii_digit() {
                return Err(XmlJsonError::ParseError {
                    position: self.pos,
                    message: "Expected digit after decimal point".to_string(),
                });
            }
            while self.pos < self.input.len() && self.current().is_ascii_digit() {
                self.advance();
            }
        }

        // Exponent
        if self.pos < self.input.len() && (self.current() == 'e' || self.current() == 'E') {
            self.advance();
            if self.pos < self.input.len() && (self.current() == '+' || self.current() == '-') {
                self.advance();
            }
            if self.pos >= self.input.len() || !self.current().is_ascii_digit() {
                return Err(XmlJsonError::ParseError {
                    position: self.pos,
                    message: "Expected digit in exponent".to_string(),
                });
            }
            while self.pos < self.input.len() && self.current().is_ascii_digit() {
                self.advance();
            }
        }

        let num_str: String = self.input[start..self.pos].iter().collect();
        let num = num_str.parse::<f64>().map_err(|_| XmlJsonError::ParseError {
            position: start,
            message: "Invalid number".to_string(),
        })?;

        Ok(JsonValue::Number(num))
    }

    fn parse_array(&mut self) -> XmlJsonResult<JsonValue> {
        if self.current() != '[' {
            return Err(XmlJsonError::ParseError {
                position: self.pos,
                message: "Expected '['".to_string(),
            });
        }
        self.advance();
        self.skip_whitespace();

        let mut items = Vec::new();

        if self.current() != ']' {
            loop {
                items.push(self.parse_value()?);
                self.skip_whitespace();

                if self.current() == ']' {
                    break;
                }
                if self.current() != ',' {
                    return Err(XmlJsonError::ParseError {
                        position: self.pos,
                        message: "Expected ',' or ']'".to_string(),
                    });
                }
                self.advance();
                self.skip_whitespace();
            }
        }

        self.advance(); // Skip ']'
        Ok(JsonValue::Array(items))
    }

    fn parse_object(&mut self) -> XmlJsonResult<JsonValue> {
        if self.current() != '{' {
            return Err(XmlJsonError::ParseError {
                position: self.pos,
                message: "Expected '{'".to_string(),
            });
        }
        self.advance();
        self.skip_whitespace();

        let mut props = Vec::new();

        if self.current() != '}' {
            loop {
                self.skip_whitespace();
                let key = self.parse_string()?;
                self.skip_whitespace();

                if self.current() != ':' {
                    return Err(XmlJsonError::ParseError {
                        position: self.pos,
                        message: "Expected ':'".to_string(),
                    });
                }
                self.advance();
                self.skip_whitespace();

                let value = self.parse_value()?;
                props.push((key, value));
                self.skip_whitespace();

                if self.current() == '}' {
                    break;
                }
                if self.current() != ',' {
                    return Err(XmlJsonError::ParseError {
                        position: self.pos,
                        message: "Expected ',' or '}'".to_string(),
                    });
                }
                self.advance();
            }
        }

        self.advance(); // Skip '}'
        Ok(JsonValue::Object(props))
    }

    fn current(&self) -> char {
        self.input.get(self.pos).copied().unwrap_or('\0')
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.input.len() && self.current().is_whitespace() {
            self.advance();
        }
    }

    fn consume_literal(&mut self, lit: &str) -> bool {
        let remaining: String = self.input[self.pos..].iter().collect();
        if remaining.starts_with(lit) {
            self.pos += lit.len();
            true
        } else {
            false
        }
    }
}

/// Parse JSON string into a CobolField structure.
pub fn json_parse(json: &str, template: &CobolField) -> XmlJsonResult<CobolField> {
    let mut parser = JsonParser::new(json);
    let value = parser.parse()?;
    map_json_to_cobol(&value, template)
}

/// Map a JsonValue to a CobolField based on a template.
fn map_json_to_cobol(value: &JsonValue, template: &CobolField) -> XmlJsonResult<CobolField> {
    let mut result = template.clone();

    match value {
        JsonValue::Object(props) => {
            if !template.is_group() {
                return Err(XmlJsonError::ConversionError(
                    "Expected group field for JSON object".to_string(),
                ));
            }

            for child in &mut result.children {
                // Find matching property (case-insensitive, handle hyphens/underscores)
                let child_name = child.name.replace('-', "_").to_lowercase();
                if let Some((_, prop_value)) = props.iter().find(|(k, _)| {
                    k.replace('-', "_").to_lowercase() == child_name
                        || to_camel_case(k) == to_camel_case(&child.name)
                }) {
                    *child = map_json_to_cobol(prop_value, child)?;
                }
            }
        }
        JsonValue::String(s) => {
            result.value = s.clone();
        }
        JsonValue::Number(n) => {
            result.value = n.to_string();
        }
        JsonValue::Bool(b) => {
            result.value = if *b { "true" } else { "false" }.to_string();
        }
        JsonValue::Null => {
            result.value = String::new();
        }
        JsonValue::Array(_) => {
            // Arrays would require OCCURS handling - simplified for now
            return Err(XmlJsonError::ConversionError(
                "Array mapping not yet supported".to_string(),
            ));
        }
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_json_generate_simple() {
        let mut data = CobolField::group("CUSTOMER", 1);
        data.add_child(CobolField::alphanumeric("NAME", 5, "John"));
        data.add_child(CobolField::numeric("ID", 5, "123"));

        let options = JsonGenerateOptions::default();
        let json = json_generate_value(&data, &options).unwrap();

        assert!(json.contains("\"name\""));
        assert!(json.contains("\"John\""));
        assert!(json.contains("\"id\""));
        assert!(json.contains("123"));
    }

    #[test]
    fn test_json_generate_camel_case() {
        let mut data = CobolField::group("CUST-RECORD", 1);
        data.add_child(CobolField::alphanumeric("FIRST-NAME", 5, "John"));
        data.add_child(CobolField::alphanumeric("LAST-NAME", 5, "Doe"));

        let options = JsonGenerateOptions {
            camel_case: true,
            ..Default::default()
        };
        let json = json_generate_value(&data, &options).unwrap();

        assert!(json.contains("\"firstName\""));
        assert!(json.contains("\"lastName\""));
    }

    #[test]
    fn test_json_generate_escape() {
        let data = CobolField::alphanumeric("TEXT", 1, "Hello \"World\"\nNew line");

        let options = JsonGenerateOptions::default();
        let json = json_generate_value(&data, &options).unwrap();

        assert!(json.contains("\\\"World\\\""));
        assert!(json.contains("\\n"));
    }

    #[test]
    fn test_json_parse_simple() {
        let json = r#"{"name": "John", "id": 123}"#;
        let mut parser = JsonParser::new(json);

        let value = parser.parse().unwrap();

        assert!(matches!(value, JsonValue::Object(_)));
        assert_eq!(value.get("name").and_then(|v| v.as_str()), Some("John"));
        assert_eq!(value.get("id").and_then(|v| v.as_number()), Some(123.0));
    }

    #[test]
    fn test_json_parse_nested() {
        let json = r#"{"customer": {"name": "John", "active": true}}"#;
        let mut parser = JsonParser::new(json);

        let value = parser.parse().unwrap();

        let customer = value.get("customer").unwrap();
        assert_eq!(customer.get("name").and_then(|v| v.as_str()), Some("John"));
        assert_eq!(customer.get("active").and_then(|v| v.as_bool()), Some(true));
    }

    #[test]
    fn test_json_parse_array() {
        let json = r#"[1, 2, 3, "four"]"#;
        let mut parser = JsonParser::new(json);

        let value = parser.parse().unwrap();

        let arr = value.as_array().unwrap();
        assert_eq!(arr.len(), 4);
        assert_eq!(arr[0].as_number(), Some(1.0));
        assert_eq!(arr[3].as_str(), Some("four"));
    }

    #[test]
    fn test_json_parse_escaped_string() {
        let json = r#""Hello \"World\"\nNew line""#;
        let mut parser = JsonParser::new(json);

        let value = parser.parse().unwrap();

        assert_eq!(value.as_str(), Some("Hello \"World\"\nNew line"));
    }

    #[test]
    fn test_json_parse_numbers() {
        // Integer
        let mut p1 = JsonParser::new("42");
        assert_eq!(p1.parse().unwrap().as_number(), Some(42.0));

        // Negative
        let mut p2 = JsonParser::new("-17");
        assert_eq!(p2.parse().unwrap().as_number(), Some(-17.0));

        // Decimal
        let mut p3 = JsonParser::new("3.14");
        assert_eq!(p3.parse().unwrap().as_number(), Some(3.14));

        // Scientific
        let mut p4 = JsonParser::new("1.5e10");
        assert_eq!(p4.parse().unwrap().as_number(), Some(1.5e10));
    }

    #[test]
    fn test_suppress_leading_zeros() {
        assert_eq!(suppress_leading_zeros("00123"), "123");
        assert_eq!(suppress_leading_zeros("000"), "0");
        assert_eq!(suppress_leading_zeros("100"), "100");
        assert_eq!(suppress_leading_zeros("-00123"), "-123");
        assert_eq!(suppress_leading_zeros("00.50"), "0.50");
    }

    #[test]
    fn test_to_camel_case() {
        assert_eq!(to_camel_case("FIRST-NAME"), "firstName");
        assert_eq!(to_camel_case("CUSTOMER_ID"), "customerId");
        assert_eq!(to_camel_case("NAME"), "name");
    }

    #[test]
    fn test_json_parse_to_cobol() {
        let json = r#"{"name": "John Doe", "id": "12345"}"#;

        let mut template = CobolField::group("CUSTOMER", 1);
        template.add_child(CobolField::alphanumeric("NAME", 5, ""));
        template.add_child(CobolField::numeric("ID", 5, ""));

        let result = json_parse(json, &template).unwrap();

        assert_eq!(result.children[0].value, "John Doe");
        assert_eq!(result.children[1].value, "12345");
    }
}
