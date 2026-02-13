//! XML GENERATE and XML PARSE implementation.

use super::{CobolField, FieldType, XmlJsonError, XmlJsonResult};

/// Options for XML GENERATE.
#[derive(Debug, Clone, Default)]
pub struct XmlGenerateOptions {
    /// Include XML declaration
    pub with_declaration: bool,
    /// Include namespace
    pub namespace: Option<String>,
    /// Encoding declaration
    pub encoding: Option<String>,
    /// Pretty print with indentation
    pub pretty_print: bool,
    /// Custom element name mappings
    pub name_of: Vec<(String, String)>,
    /// Suppress numeric leading zeros
    pub suppress_zeros: bool,
}

/// Generate XML from a COBOL data structure.
///
/// Implements the COBOL XML GENERATE statement.
pub fn xml_generate(data: &CobolField, options: &XmlGenerateOptions) -> XmlJsonResult<String> {
    let mut output = String::new();

    // Add XML declaration if requested
    if options.with_declaration {
        let encoding = options.encoding.as_deref().unwrap_or("UTF-8");
        output.push_str(&format!("<?xml version=\"1.0\" encoding=\"{}\"?>\n", encoding));
    }

    // Generate the element tree
    generate_element(&mut output, data, options, 0)?;

    Ok(output)
}

/// Generate a single XML element.
fn generate_element(
    output: &mut String,
    field: &CobolField,
    options: &XmlGenerateOptions,
    depth: usize,
) -> XmlJsonResult<()> {
    let indent = if options.pretty_print {
        "  ".repeat(depth)
    } else {
        String::new()
    };
    let newline = if options.pretty_print { "\n" } else { "" };

    // Get element name (apply name mapping if present)
    let element_name = get_mapped_name(&field.name, &options.name_of);

    if field.is_group() {
        // Group element with children
        output.push_str(&format!("{}<{}>", indent, element_name));
        output.push_str(newline);

        for child in &field.children {
            generate_element(output, child, options, depth + 1)?;
        }

        output.push_str(&format!("{}</{}>", indent, element_name));
        output.push_str(newline);
    } else {
        // Leaf element with value
        let value = escape_xml(&field.value);
        let value = if options.suppress_zeros && field.field_type == FieldType::Numeric {
            suppress_leading_zeros(&value)
        } else {
            value
        };

        output.push_str(&format!("{}<{}>{}</{}>", indent, element_name, value, element_name));
        output.push_str(newline);
    }

    Ok(())
}

/// Get the mapped name for an element.
fn get_mapped_name(name: &str, name_of: &[(String, String)]) -> String {
    for (from, to) in name_of {
        if from.eq_ignore_ascii_case(name) {
            return to.clone();
        }
    }
    // Convert COBOL name to XML-friendly name (replace hyphens with underscores)
    name.replace('-', "_")
}

/// Escape special XML characters.
fn escape_xml(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

/// Suppress leading zeros in numeric values.
fn suppress_leading_zeros(s: &str) -> String {
    let trimmed = s.trim_start_matches('0');
    if trimmed.is_empty() {
        "0".to_string()
    } else {
        trimmed.to_string()
    }
}

/// XML Parse event types.
#[derive(Debug, Clone, PartialEq)]
pub enum XmlEvent {
    /// Start of document
    StartDocument,
    /// End of document
    EndDocument,
    /// Start of an element
    StartElement { name: String, attributes: Vec<(String, String)> },
    /// End of an element
    EndElement { name: String },
    /// Character content
    Content { text: String },
    /// Processing instruction
    ProcessingInstruction { target: String, data: String },
    /// Comment
    Comment { text: String },
}

/// XML Parser state.
#[derive(Debug)]
pub struct XmlParser {
    /// Input XML
    input: Vec<char>,
    /// Current position
    pos: usize,
    /// Element stack
    stack: Vec<String>,
}

impl XmlParser {
    /// Create a new parser.
    pub fn new(xml: &str) -> Self {
        Self {
            input: xml.chars().collect(),
            pos: 0,
            stack: Vec::new(),
        }
    }

    /// Get the next event.
    pub fn next_event(&mut self) -> XmlJsonResult<Option<XmlEvent>> {
        self.skip_whitespace();

        if self.pos >= self.input.len() {
            if !self.stack.is_empty() {
                return Err(XmlJsonError::ParseError {
                    position: self.pos,
                    message: "Unexpected end of document".to_string(),
                });
            }
            return Ok(None);
        }

        if self.current() == '<' {
            self.advance();

            if self.current() == '?' {
                // Processing instruction
                self.advance();
                return self.parse_processing_instruction();
            } else if self.current() == '!' {
                self.advance();
                if self.starts_with("--") {
                    // Comment
                    return self.parse_comment();
                } else if self.starts_with("[CDATA[") {
                    // CDATA section
                    return self.parse_cdata();
                }
                // DOCTYPE or other - skip
                while self.pos < self.input.len() && self.current() != '>' {
                    self.advance();
                }
                self.advance();
                return self.next_event();
            } else if self.current() == '/' {
                // End element
                self.advance();
                return self.parse_end_element();
            } else {
                // Start element
                return self.parse_start_element();
            }
        } else {
            // Content
            return self.parse_content();
        }
    }

    /// Parse all events.
    pub fn parse_all(&mut self) -> XmlJsonResult<Vec<XmlEvent>> {
        let mut events = vec![XmlEvent::StartDocument];

        while let Some(event) = self.next_event()? {
            events.push(event);
        }

        events.push(XmlEvent::EndDocument);
        Ok(events)
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

    fn starts_with(&self, s: &str) -> bool {
        let remaining: String = self.input[self.pos..].iter().collect();
        remaining.starts_with(s)
    }

    fn parse_name(&mut self) -> String {
        let start = self.pos;
        while self.pos < self.input.len() {
            let c = self.current();
            if c.is_alphanumeric() || c == '_' || c == '-' || c == ':' || c == '.' {
                self.advance();
            } else {
                break;
            }
        }
        self.input[start..self.pos].iter().collect()
    }

    fn parse_start_element(&mut self) -> XmlJsonResult<Option<XmlEvent>> {
        let name = self.parse_name();
        let mut attributes = Vec::new();

        // Parse attributes
        loop {
            self.skip_whitespace();

            if self.current() == '/' {
                self.advance();
                if self.current() != '>' {
                    return Err(XmlJsonError::ParseError {
                        position: self.pos,
                        message: "Expected '>' after '/'".to_string(),
                    });
                }
                self.advance();
                // Self-closing element - emit start and end
                return Ok(Some(XmlEvent::StartElement { name: name.clone(), attributes }));
            } else if self.current() == '>' {
                self.advance();
                self.stack.push(name.clone());
                return Ok(Some(XmlEvent::StartElement { name, attributes }));
            } else {
                // Parse attribute
                let attr_name = self.parse_name();
                self.skip_whitespace();

                if self.current() != '=' {
                    return Err(XmlJsonError::ParseError {
                        position: self.pos,
                        message: "Expected '=' in attribute".to_string(),
                    });
                }
                self.advance();
                self.skip_whitespace();

                let quote = self.current();
                if quote != '"' && quote != '\'' {
                    return Err(XmlJsonError::ParseError {
                        position: self.pos,
                        message: "Expected quote in attribute value".to_string(),
                    });
                }
                self.advance();

                let mut value = String::new();
                while self.pos < self.input.len() && self.current() != quote {
                    value.push(self.current());
                    self.advance();
                }
                self.advance();

                attributes.push((attr_name, unescape_xml(&value)));
            }
        }
    }

    fn parse_end_element(&mut self) -> XmlJsonResult<Option<XmlEvent>> {
        let name = self.parse_name();
        self.skip_whitespace();

        if self.current() != '>' {
            return Err(XmlJsonError::ParseError {
                position: self.pos,
                message: "Expected '>' in end element".to_string(),
            });
        }
        self.advance();

        if self.stack.last().map(|s| s.as_str()) != Some(&name) {
            return Err(XmlJsonError::ParseError {
                position: self.pos,
                message: format!("Mismatched end element: {}", name),
            });
        }
        self.stack.pop();

        Ok(Some(XmlEvent::EndElement { name }))
    }

    fn parse_content(&mut self) -> XmlJsonResult<Option<XmlEvent>> {
        let mut text = String::new();
        while self.pos < self.input.len() && self.current() != '<' {
            text.push(self.current());
            self.advance();
        }
        Ok(Some(XmlEvent::Content { text: unescape_xml(&text) }))
    }

    fn parse_processing_instruction(&mut self) -> XmlJsonResult<Option<XmlEvent>> {
        let target = self.parse_name();
        self.skip_whitespace();

        let mut data = String::new();
        while self.pos < self.input.len() && !self.starts_with("?>") {
            data.push(self.current());
            self.advance();
        }
        self.pos += 2; // Skip ?>

        Ok(Some(XmlEvent::ProcessingInstruction { target, data: data.trim().to_string() }))
    }

    fn parse_comment(&mut self) -> XmlJsonResult<Option<XmlEvent>> {
        self.pos += 2; // Skip --
        let mut text = String::new();
        while self.pos < self.input.len() && !self.starts_with("-->") {
            text.push(self.current());
            self.advance();
        }
        self.pos += 3; // Skip -->

        Ok(Some(XmlEvent::Comment { text }))
    }

    fn parse_cdata(&mut self) -> XmlJsonResult<Option<XmlEvent>> {
        self.pos += 7; // Skip [CDATA[
        let mut text = String::new();
        while self.pos < self.input.len() && !self.starts_with("]]>") {
            text.push(self.current());
            self.advance();
        }
        self.pos += 3; // Skip ]]>

        Ok(Some(XmlEvent::Content { text }))
    }
}

/// Unescape XML entities.
fn unescape_xml(s: &str) -> String {
    s.replace("&lt;", "<")
        .replace("&gt;", ">")
        .replace("&amp;", "&")
        .replace("&quot;", "\"")
        .replace("&apos;", "'")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_xml_generate_simple() {
        let mut data = CobolField::group("CUSTOMER", 1);
        data.add_child(CobolField::alphanumeric("NAME", 5, "John"));
        data.add_child(CobolField::numeric("ID", 5, "123"));

        let options = XmlGenerateOptions::default();
        let xml = xml_generate(&data, &options).unwrap();

        assert!(xml.contains("<CUSTOMER>"));
        assert!(xml.contains("<NAME>John</NAME>"));
        assert!(xml.contains("<ID>123</ID>"));
        assert!(xml.contains("</CUSTOMER>"));
    }

    #[test]
    fn test_xml_generate_with_declaration() {
        let data = CobolField::alphanumeric("ITEM", 1, "test");

        let options = XmlGenerateOptions {
            with_declaration: true,
            encoding: Some("UTF-8".to_string()),
            ..Default::default()
        };

        let xml = xml_generate(&data, &options).unwrap();
        assert!(xml.starts_with("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"));
    }

    #[test]
    fn test_xml_generate_escape() {
        let data = CobolField::alphanumeric("TEXT", 1, "<hello & world>");

        let options = XmlGenerateOptions::default();
        let xml = xml_generate(&data, &options).unwrap();

        assert!(xml.contains("&lt;hello &amp; world&gt;"));
    }

    #[test]
    fn test_xml_parse_simple() {
        let xml = "<root><child>value</child></root>";
        let mut parser = XmlParser::new(xml);

        let events = parser.parse_all().unwrap();

        assert!(events.contains(&XmlEvent::StartDocument));
        assert!(events.contains(&XmlEvent::StartElement {
            name: "root".to_string(),
            attributes: vec![],
        }));
        assert!(events.contains(&XmlEvent::StartElement {
            name: "child".to_string(),
            attributes: vec![],
        }));
        assert!(events.contains(&XmlEvent::Content { text: "value".to_string() }));
        assert!(events.contains(&XmlEvent::EndElement { name: "child".to_string() }));
        assert!(events.contains(&XmlEvent::EndElement { name: "root".to_string() }));
        assert!(events.contains(&XmlEvent::EndDocument));
    }

    #[test]
    fn test_xml_parse_attributes() {
        let xml = r#"<item id="123" name="test">value</item>"#;
        let mut parser = XmlParser::new(xml);

        let events = parser.parse_all().unwrap();

        let start_event = events.iter().find(|e| matches!(e, XmlEvent::StartElement { name, .. } if name == "item"));
        assert!(start_event.is_some());

        if let Some(XmlEvent::StartElement { attributes, .. }) = start_event {
            assert_eq!(attributes.len(), 2);
            assert!(attributes.contains(&("id".to_string(), "123".to_string())));
            assert!(attributes.contains(&("name".to_string(), "test".to_string())));
        }
    }

    #[test]
    fn test_xml_parse_escaped_content() {
        let xml = "<text>&lt;hello&gt;</text>";
        let mut parser = XmlParser::new(xml);

        let events = parser.parse_all().unwrap();

        assert!(events.contains(&XmlEvent::Content { text: "<hello>".to_string() }));
    }

    #[test]
    fn test_suppress_leading_zeros() {
        assert_eq!(suppress_leading_zeros("00123"), "123");
        assert_eq!(suppress_leading_zeros("000"), "0");
        assert_eq!(suppress_leading_zeros("100"), "100");
    }
}
