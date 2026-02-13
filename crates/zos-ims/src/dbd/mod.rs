//! DBD (Database Definition) parsing.
//!
//! Parses IMS DBD macros to define database structure:
//! - DBD (Database Definition)
//! - DATASET (Data set group)
//! - SEGM (Segment definition)
//! - FIELD (Field definition)
//! - LCHILD (Logical child)
//! - DBDGEN (End of DBD)

use std::collections::HashMap;

/// A complete Database Definition.
#[derive(Debug, Clone)]
pub struct DatabaseDefinition {
    /// Database name (1-8 characters)
    pub name: String,
    /// Access method (HISAM, HIDAM, HDAM, etc.)
    pub access: AccessMethod,
    /// Root segment name
    pub root_segment: String,
    /// All segments indexed by name
    pub segments: HashMap<String, SegmentDefinition>,
    /// Segment hierarchy (parent -> children)
    pub hierarchy: HashMap<String, Vec<String>>,
}

impl DatabaseDefinition {
    /// Create a new database definition.
    pub fn new(name: &str, access: AccessMethod) -> Self {
        Self {
            name: name.to_string(),
            access,
            root_segment: String::new(),
            segments: HashMap::new(),
            hierarchy: HashMap::new(),
        }
    }

    /// Add a segment to the database.
    pub fn add_segment(&mut self, segment: SegmentDefinition) {
        let name = segment.name.clone();
        let parent = segment.parent.clone();

        if parent.is_empty() {
            self.root_segment = name.clone();
        } else {
            self.hierarchy
                .entry(parent)
                .or_insert_with(Vec::new)
                .push(name.clone());
        }

        self.segments.insert(name, segment);
    }

    /// Get a segment by name.
    pub fn get_segment(&self, name: &str) -> Option<&SegmentDefinition> {
        self.segments.get(name)
    }

    /// Get children of a segment.
    pub fn get_children(&self, parent: &str) -> Vec<&SegmentDefinition> {
        self.hierarchy
            .get(parent)
            .map(|children| {
                children
                    .iter()
                    .filter_map(|n| self.segments.get(n))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Get the path from root to a segment.
    pub fn get_path(&self, segment_name: &str) -> Vec<&str> {
        let mut path = Vec::new();
        let mut current = segment_name;

        while let Some(seg) = self.segments.get(current) {
            path.push(seg.name.as_str());
            if seg.parent.is_empty() {
                break;
            }
            current = &seg.parent;
        }

        path.reverse();
        path
    }
}

/// IMS access methods.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AccessMethod {
    /// Hierarchical Indexed Sequential
    HISAM,
    /// Hierarchical Indexed Direct
    HIDAM,
    /// Hierarchical Direct
    HDAM,
    /// Hierarchical Sequential
    HSAM,
    /// Simple HSAM
    SHSAM,
    /// Simple HISAM
    SHISAM,
    /// Main Storage Database
    MSDB,
    /// Data Entry Database
    DEDB,
    /// Generalized Sequential
    GSAM,
}

impl AccessMethod {
    /// Parse from string.
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "HISAM" => Some(AccessMethod::HISAM),
            "HIDAM" => Some(AccessMethod::HIDAM),
            "HDAM" => Some(AccessMethod::HDAM),
            "HSAM" => Some(AccessMethod::HSAM),
            "SHSAM" => Some(AccessMethod::SHSAM),
            "SHISAM" => Some(AccessMethod::SHISAM),
            "MSDB" => Some(AccessMethod::MSDB),
            "DEDB" => Some(AccessMethod::DEDB),
            "GSAM" => Some(AccessMethod::GSAM),
            _ => None,
        }
    }
}

/// A segment definition within a DBD.
#[derive(Debug, Clone)]
pub struct SegmentDefinition {
    /// Segment name (1-8 characters)
    pub name: String,
    /// Parent segment name (empty for root)
    pub parent: String,
    /// Segment length in bytes
    pub bytes: usize,
    /// Minimum length (for variable)
    pub min_bytes: Option<usize>,
    /// Fields in this segment
    pub fields: Vec<FieldDefinition>,
    /// Sequence field name
    pub sequence_field: Option<String>,
    /// Whether segment is fixed or variable length
    pub variable_length: bool,
    /// Compression routine
    pub comprtn: Option<String>,
}

impl SegmentDefinition {
    /// Create a new segment definition.
    pub fn new(name: &str, parent: &str, bytes: usize) -> Self {
        Self {
            name: name.to_string(),
            parent: parent.to_string(),
            bytes,
            min_bytes: None,
            fields: Vec::new(),
            sequence_field: None,
            variable_length: false,
            comprtn: None,
        }
    }

    /// Add a field to the segment.
    pub fn add_field(&mut self, field: FieldDefinition) {
        self.fields.push(field);
    }

    /// Get a field by name.
    pub fn get_field(&self, name: &str) -> Option<&FieldDefinition> {
        self.fields.iter().find(|f| f.name == name)
    }

    /// Set the sequence field.
    pub fn set_sequence_field(&mut self, field_name: &str) {
        self.sequence_field = Some(field_name.to_string());
    }
}

/// A field definition within a segment.
#[derive(Debug, Clone)]
pub struct FieldDefinition {
    /// Field name (1-8 characters)
    pub name: String,
    /// Starting position (1-based)
    pub start: usize,
    /// Field length in bytes
    pub bytes: usize,
    /// Field type
    pub field_type: FieldType,
    /// Is this the sequence field?
    pub is_sequence: bool,
    /// Is this field unique within parent?
    pub is_unique: bool,
}

impl FieldDefinition {
    /// Create a new field definition.
    pub fn new(name: &str, start: usize, bytes: usize, field_type: FieldType) -> Self {
        Self {
            name: name.to_string(),
            start,
            bytes,
            field_type,
            is_sequence: false,
            is_unique: false,
        }
    }
}

/// IMS field types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldType {
    /// Character data
    Character,
    /// Packed decimal
    Packed,
    /// Zoned decimal
    Zoned,
    /// Binary
    Binary,
    /// Hexadecimal
    Hex,
}

impl FieldType {
    /// Parse from type character.
    pub fn from_char(c: char) -> Self {
        match c.to_ascii_uppercase() {
            'C' => FieldType::Character,
            'P' => FieldType::Packed,
            'Z' => FieldType::Zoned,
            'X' => FieldType::Hex,
            'B' => FieldType::Binary,
            _ => FieldType::Character,
        }
    }
}

/// DBD Parser.
pub struct DbdParser {
    lines: Vec<String>,
    current_line: usize,
}

impl DbdParser {
    /// Create a new DBD parser.
    pub fn new() -> Self {
        Self {
            lines: Vec::new(),
            current_line: 0,
        }
    }

    /// Parse DBD source.
    pub fn parse(&mut self, source: &str) -> Result<DatabaseDefinition, DbdParseError> {
        self.lines = source.lines().map(|s| s.to_string()).collect();
        self.current_line = 0;

        let mut dbd: Option<DatabaseDefinition> = None;
        let mut current_segment: Option<SegmentDefinition> = None;

        while self.current_line < self.lines.len() {
            let line = &self.lines[self.current_line];
            let trimmed = line.trim();

            // Skip comments and empty lines
            if trimmed.is_empty() || trimmed.starts_with('*') || trimmed.starts_with(".*") {
                self.current_line += 1;
                continue;
            }

            // Parse macro
            if let Some(macro_content) = self.extract_macro(trimmed) {
                let (macro_name, params) = self.parse_macro_line(&macro_content);

                match macro_name.as_str() {
                    "DBD" => {
                        let name = params.get("NAME").cloned().unwrap_or_default();
                        let access_str = params.get("ACCESS").cloned().unwrap_or_default();
                        let access = AccessMethod::from_str(&access_str)
                            .unwrap_or(AccessMethod::HISAM);
                        dbd = Some(DatabaseDefinition::new(&name, access));
                    }
                    "SEGM" => {
                        // Save previous segment
                        if let (Some(ref mut db), Some(seg)) = (&mut dbd, current_segment.take()) {
                            db.add_segment(seg);
                        }

                        let name = params.get("NAME").cloned().unwrap_or_default();
                        let parent = params.get("PARENT").cloned().unwrap_or_default();
                        // Handle PARENT=0 as root
                        let parent = if parent == "0" { String::new() } else { parent };
                        let bytes: usize = params
                            .get("BYTES")
                            .and_then(|s| s.parse().ok())
                            .unwrap_or(0);

                        current_segment = Some(SegmentDefinition::new(&name, &parent, bytes));
                    }
                    "FIELD" => {
                        if let Some(ref mut seg) = current_segment {
                            let name = params.get("NAME").cloned().unwrap_or_default();
                            let start: usize = params
                                .get("START")
                                .and_then(|s| s.parse().ok())
                                .unwrap_or(1);
                            let bytes: usize = params
                                .get("BYTES")
                                .and_then(|s| s.parse().ok())
                                .unwrap_or(0);
                            let type_char = params
                                .get("TYPE")
                                .and_then(|s| s.chars().next())
                                .unwrap_or('C');

                            let mut field = FieldDefinition::new(
                                &name,
                                start,
                                bytes,
                                FieldType::from_char(type_char),
                            );

                            if params.contains_key("SEQ") {
                                field.is_sequence = true;
                                seg.set_sequence_field(&name);
                            }

                            seg.add_field(field);
                        }
                    }
                    "DBDGEN" | "FINISH" | "END" => {
                        // End of DBD - save last segment first
                        if let (Some(ref mut db), Some(seg)) = (&mut dbd, current_segment.take()) {
                            db.add_segment(seg);
                        }
                        break;
                    }
                    _ => {
                        // Ignore other macros
                    }
                }
            }

            self.current_line += 1;
        }

        // Save last segment if not already saved
        if let (Some(ref mut db), Some(seg)) = (&mut dbd, current_segment.take()) {
            db.add_segment(seg);
        }

        dbd.ok_or(DbdParseError::NoDbd)
    }

    /// Extract macro from line (handles label and macro name).
    fn extract_macro(&self, line: &str) -> Option<String> {
        // Format: [label] MACRO params
        let parts: Vec<&str> = line.split_whitespace().collect();

        if parts.is_empty() {
            return None;
        }

        // Check if first part is a known macro
        let known_macros = ["DBD", "SEGM", "FIELD", "DATASET", "LCHILD", "DBDGEN", "FINISH", "END"];

        if known_macros.contains(&parts[0].to_uppercase().as_str()) {
            return Some(line.to_string());
        }

        // First part might be a label
        if parts.len() > 1 && known_macros.contains(&parts[1].to_uppercase().as_str()) {
            return Some(parts[1..].join(" "));
        }

        None
    }

    /// Parse a macro line into name and parameters.
    fn parse_macro_line(&self, line: &str) -> (String, HashMap<String, String>) {
        let mut params = HashMap::new();
        let parts: Vec<&str> = line.splitn(2, char::is_whitespace).collect();

        let macro_name = parts[0].to_uppercase();

        if parts.len() > 1 {
            // Parse parameters
            let param_str = parts[1].trim();
            let mut current_key = String::new();
            let mut current_value = String::new();
            let mut in_parens = 0;
            let mut in_key = true;

            for c in param_str.chars() {
                match c {
                    '(' => {
                        in_parens += 1;
                        if in_parens == 1 && !current_key.is_empty() {
                            in_key = false;
                        } else {
                            current_value.push(c);
                        }
                    }
                    ')' => {
                        in_parens -= 1;
                        if in_parens == 0 && !in_key {
                            // End of parameter
                            let key = current_key.trim().to_uppercase();
                            let value = current_value.trim().trim_matches('\'').to_string();
                            if !key.is_empty() {
                                params.insert(key, value);
                            }
                            current_key.clear();
                            current_value.clear();
                            in_key = true;
                        } else {
                            current_value.push(c);
                        }
                    }
                    ',' if in_parens == 0 => {
                        // Parameter separator - save current key/value
                        let key = current_key.trim().to_uppercase();
                        if !key.is_empty() {
                            if current_value.is_empty() {
                                // Flag parameter (no value)
                                params.insert(key, String::new());
                            } else {
                                // Key=value parameter
                                let value = current_value.trim().trim_matches('\'').to_string();
                                params.insert(key, value);
                            }
                        }
                        current_key.clear();
                        current_value.clear();
                        in_key = true;
                    }
                    '=' if in_parens == 0 => {
                        // Simple key=value (without parens)
                        in_key = false;
                    }
                    ' ' if in_parens == 0 && current_key.is_empty() => {
                        // Skip leading spaces
                    }
                    _ => {
                        if in_key {
                            current_key.push(c);
                        } else {
                            current_value.push(c);
                        }
                    }
                }
            }

            // Handle remaining parameter
            let key = current_key.trim().to_uppercase();
            if !key.is_empty() {
                if current_value.is_empty() {
                    params.insert(key, String::new());
                } else {
                    let value = current_value.trim().trim_matches('\'').to_string();
                    params.insert(key, value);
                }
            }
        }

        (macro_name, params)
    }
}

impl Default for DbdParser {
    fn default() -> Self {
        Self::new()
    }
}

/// DBD parse errors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DbdParseError {
    /// No DBD macro found
    NoDbd,
    /// Invalid segment definition
    InvalidSegment(String),
    /// Invalid field definition
    InvalidField(String),
    /// Syntax error
    SyntaxError { line: usize, message: String },
}

impl std::fmt::Display for DbdParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DbdParseError::NoDbd => write!(f, "No DBD macro found"),
            DbdParseError::InvalidSegment(s) => write!(f, "Invalid segment: {}", s),
            DbdParseError::InvalidField(s) => write!(f, "Invalid field: {}", s),
            DbdParseError::SyntaxError { line, message } => {
                write!(f, "Syntax error at line {}: {}", line, message)
            }
        }
    }
}

impl std::error::Error for DbdParseError {}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE_DBD: &str = r#"
         DBD   NAME=CUSTDB,ACCESS=HIDAM
         SEGM  NAME=CUSTOMER,BYTES=100,PARENT=0
         FIELD NAME=CUSTNO,START=1,BYTES=10,TYPE=C,SEQ
         FIELD NAME=CUSTNAME,START=11,BYTES=30,TYPE=C
         SEGM  NAME=ORDER,BYTES=50,PARENT=CUSTOMER
         FIELD NAME=ORDERNO,START=1,BYTES=8,TYPE=C,SEQ
         FIELD NAME=ORDDATE,START=9,BYTES=8,TYPE=C
         SEGM  NAME=ITEM,BYTES=40,PARENT=ORDER
         FIELD NAME=ITEMNO,START=1,BYTES=6,TYPE=C,SEQ
         FIELD NAME=QUANTITY,START=7,BYTES=4,TYPE=P
         DBDGEN
    "#;

    #[test]
    fn test_parse_dbd() {
        let mut parser = DbdParser::new();
        let dbd = parser.parse(SAMPLE_DBD).unwrap();

        assert_eq!(dbd.name, "CUSTDB");
        assert_eq!(dbd.access, AccessMethod::HIDAM);
        assert_eq!(dbd.root_segment, "CUSTOMER");
    }

    #[test]
    fn test_segments() {
        let mut parser = DbdParser::new();
        let dbd = parser.parse(SAMPLE_DBD).unwrap();

        assert_eq!(dbd.segments.len(), 3);
        assert!(dbd.segments.contains_key("CUSTOMER"));
        assert!(dbd.segments.contains_key("ORDER"));
        assert!(dbd.segments.contains_key("ITEM"));
    }

    #[test]
    fn test_hierarchy() {
        let mut parser = DbdParser::new();
        let dbd = parser.parse(SAMPLE_DBD).unwrap();

        let children = dbd.get_children("CUSTOMER");
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].name, "ORDER");

        let children = dbd.get_children("ORDER");
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].name, "ITEM");
    }

    #[test]
    fn test_fields() {
        let mut parser = DbdParser::new();
        let dbd = parser.parse(SAMPLE_DBD).unwrap();

        let customer = dbd.get_segment("CUSTOMER").unwrap();
        assert_eq!(customer.fields.len(), 2);
        assert_eq!(customer.sequence_field, Some("CUSTNO".to_string()));

        let custno = customer.get_field("CUSTNO").unwrap();
        assert_eq!(custno.start, 1);
        assert_eq!(custno.bytes, 10);
        assert!(custno.is_sequence);
    }

    #[test]
    fn test_path() {
        let mut parser = DbdParser::new();
        let dbd = parser.parse(SAMPLE_DBD).unwrap();

        let path = dbd.get_path("ITEM");
        assert_eq!(path, vec!["CUSTOMER", "ORDER", "ITEM"]);
    }

    #[test]
    fn test_access_method() {
        assert_eq!(AccessMethod::from_str("HIDAM"), Some(AccessMethod::HIDAM));
        assert_eq!(AccessMethod::from_str("hisam"), Some(AccessMethod::HISAM));
        assert_eq!(AccessMethod::from_str("UNKNOWN"), None);
    }

    #[test]
    fn test_field_type() {
        assert_eq!(FieldType::from_char('C'), FieldType::Character);
        assert_eq!(FieldType::from_char('P'), FieldType::Packed);
        assert_eq!(FieldType::from_char('Z'), FieldType::Zoned);
        assert_eq!(FieldType::from_char('X'), FieldType::Hex);
    }
}
