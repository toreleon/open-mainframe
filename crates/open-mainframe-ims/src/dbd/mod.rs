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

/// A secondary index definition (XDFLD macro).
#[derive(Debug, Clone)]
pub struct SecondaryIndex {
    /// Index name (the XDFLD NAME value)
    pub name: String,
    /// Target segment that is indexed
    pub segment: String,
    /// Search field in the target segment used as the index key
    pub search_field: String,
    /// Whether the index key is unique
    pub unique: bool,
}

/// A logical child relationship (LCHILD macro).
#[derive(Debug, Clone)]
pub struct LogicalChild {
    /// Logical child segment name
    pub name: String,
    /// The database containing the logical child segment
    pub lchild_db: String,
    /// The physical parent segment in this DBD that has the relationship
    pub parent_segment: String,
    /// Optional pointer field for the relationship
    pub pointer: Option<String>,
    /// Whether this is a paired relationship (bidirectional)
    pub paired: bool,
}

/// A DATASET macro definition (physical storage characteristics).
#[derive(Debug, Clone)]
pub struct DatasetDefinition {
    /// DD name (the DD1 value, e.g. "CUSTDD")
    pub dd_name: String,
    /// Device type (e.g. "3390")
    pub device: Option<String>,
    /// Block size in bytes
    pub block_size: Option<usize>,
    /// Scan size (optional)
    pub scan: Option<usize>,
    /// Associated segment name (context from parser position)
    pub segment: String,
}

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
    /// Secondary indexes defined via XDFLD
    pub secondary_indexes: Vec<SecondaryIndex>,
    /// Logical child relationships defined via LCHILD
    pub logical_children: Vec<LogicalChild>,
    /// Physical dataset definitions from DATASET macros
    pub datasets: Vec<DatasetDefinition>,
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
            secondary_indexes: Vec::new(),
            logical_children: Vec::new(),
            datasets: Vec::new(),
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

    /// Add a secondary index definition.
    pub fn add_secondary_index(&mut self, index: SecondaryIndex) {
        self.secondary_indexes.push(index);
    }

    /// Get a secondary index by name.
    pub fn get_secondary_index(&self, name: &str) -> Option<&SecondaryIndex> {
        self.secondary_indexes.iter().find(|idx| idx.name.eq_ignore_ascii_case(name))
    }

    /// Add a logical child relationship.
    pub fn add_logical_child(&mut self, lchild: LogicalChild) {
        self.logical_children.push(lchild);
    }

    /// Add a dataset definition.
    pub fn add_dataset(&mut self, dataset: DatasetDefinition) {
        self.datasets.push(dataset);
    }

    /// Get dataset definitions for a segment.
    pub fn get_datasets(&self, segment: &str) -> Vec<&DatasetDefinition> {
        self.datasets
            .iter()
            .filter(|d| d.segment.eq_ignore_ascii_case(segment))
            .collect()
    }

    /// Get logical children of a segment.
    pub fn get_logical_children(&self, parent_segment: &str) -> Vec<&LogicalChild> {
        self.logical_children
            .iter()
            .filter(|lc| lc.parent_segment.eq_ignore_ascii_case(parent_segment))
            .collect()
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

/// Parse BYTES parameter — either a simple number or `(max,min)` tuple.
///
/// Returns `(max_bytes, Option<min_bytes>)`.
fn parse_bytes_param(raw: &str) -> (usize, Option<usize>) {
    let trimmed = raw.trim();
    if trimmed.contains(',') {
        // Tuple form: "200,50" (parentheses already stripped by macro parser)
        let parts: Vec<&str> = trimmed.split(',').collect();
        let max_bytes = parts[0].trim().parse::<usize>().unwrap_or(0);
        let min_bytes = parts.get(1).and_then(|s| s.trim().parse::<usize>().ok());
        (max_bytes, min_bytes)
    } else {
        // Simple numeric value
        let bytes = trimmed.parse::<usize>().unwrap_or(0);
        (bytes, None)
    }
}

/// Parse NAME parameter — either a simple name or `(segment,database)` tuple.
///
/// Returns `(name, database)` where database is empty if not a tuple.
fn parse_name_tuple(raw: &str) -> (String, String) {
    let trimmed = raw.trim();
    if trimmed.contains(',') {
        // Tuple form: "ORDLINE,ORDERDB" (parentheses already stripped by macro parser)
        let parts: Vec<&str> = trimmed.splitn(2, ',').collect();
        let name = parts[0].trim().to_string();
        let db = parts.get(1).map(|s| s.trim().to_string()).unwrap_or_default();
        (name, db)
    } else {
        (trimmed.to_string(), String::new())
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
        let mut last_segment_name = String::new();

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

                        // Parse BYTES — could be a simple number or tuple (max,min)
                        let bytes_raw = params.get("BYTES").cloned().unwrap_or_default();
                        let (max_bytes, min_bytes) = parse_bytes_param(&bytes_raw);

                        last_segment_name = name.clone();
                        let mut seg = SegmentDefinition::new(&name, &parent, max_bytes);

                        // Variable-length segment support
                        if let Some(min) = min_bytes {
                            seg.min_bytes = Some(min);
                            seg.variable_length = true;
                        }

                        // Compression routine
                        if let Some(comprtn) = params.get("COMPRTN") {
                            if !comprtn.is_empty() {
                                seg.comprtn = Some(comprtn.clone());
                                seg.variable_length = true;
                            }
                        }

                        current_segment = Some(seg);
                    }
                    "DATASET" => {
                        // Physical dataset definition
                        if let Some(ref mut db) = dbd {
                            let dd_name = params.get("DD1").cloned().unwrap_or_default();
                            let device = params.get("DEVICE").cloned();
                            let block_size = params.get("SIZE")
                                .and_then(|s| s.parse::<usize>().ok());
                            let scan = params.get("SCAN")
                                .and_then(|s| s.parse::<usize>().ok());

                            db.add_dataset(DatasetDefinition {
                                dd_name,
                                device,
                                block_size,
                                scan,
                                segment: last_segment_name.clone(),
                            });
                        }
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
                    "XDFLD" => {
                        // Secondary index definition
                        if let Some(ref mut db) = dbd {
                            let name = params.get("NAME").cloned().unwrap_or_default();
                            let segment = params.get("SEGMENT").cloned()
                                .unwrap_or_else(|| last_segment_name.clone());
                            let srch = params.get("SRCH").cloned().unwrap_or_default();
                            let unique = params.contains_key("UNIQUE");

                            db.add_secondary_index(SecondaryIndex {
                                name,
                                segment,
                                search_field: srch,
                                unique,
                            });
                        }
                    }
                    "LCHILD" => {
                        // Logical child relationship
                        if let Some(ref mut db) = dbd {
                            let name_raw = params.get("NAME").cloned().unwrap_or_default();

                            // NAME can be a simple name or tuple (segment,database)
                            let (lchild_name, lchild_db) = parse_name_tuple(&name_raw);

                            // SOURCE/INDEX overrides the db from NAME tuple if present
                            let lchild_db = params.get("SOURCE")
                                .or(params.get("INDEX"))
                                .cloned()
                                .unwrap_or(lchild_db);

                            let pointer = params.get("POINTER").cloned();
                            let paired = params.contains_key("PAIRED");

                            db.add_logical_child(LogicalChild {
                                name: lchild_name,
                                lchild_db,
                                parent_segment: last_segment_name.clone(),
                                pointer,
                                paired,
                            });
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
        let known_macros = ["DBD", "SEGM", "FIELD", "DATASET", "LCHILD", "XDFLD", "DBDGEN", "FINISH", "END"];

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

    #[test]
    fn test_parse_xdfld_secondary_index() {
        let source = r#"
             DBD   NAME=CUSTDB,ACCESS=HIDAM
             SEGM  NAME=CUSTOMER,BYTES=100,PARENT=0
             FIELD NAME=CUSTNO,START=1,BYTES=10,TYPE=C,SEQ
             FIELD NAME=CUSTNAME,START=11,BYTES=30,TYPE=C
             XDFLD NAME=CUSTNAME_IX,SEGMENT=CUSTOMER,SRCH=CUSTNAME
             SEGM  NAME=ORDER,BYTES=50,PARENT=CUSTOMER
             FIELD NAME=ORDERNO,START=1,BYTES=8,TYPE=C,SEQ
             DBDGEN
        "#;

        let mut parser = DbdParser::new();
        let dbd = parser.parse(source).unwrap();

        assert_eq!(dbd.secondary_indexes.len(), 1);
        let idx = &dbd.secondary_indexes[0];
        assert_eq!(idx.name, "CUSTNAME_IX");
        assert_eq!(idx.segment, "CUSTOMER");
        assert_eq!(idx.search_field, "CUSTNAME");
        assert!(!idx.unique);
    }

    #[test]
    fn test_parse_xdfld_unique_index() {
        let source = r#"
             DBD   NAME=CUSTDB,ACCESS=HIDAM
             SEGM  NAME=CUSTOMER,BYTES=100,PARENT=0
             FIELD NAME=CUSTNO,START=1,BYTES=10,TYPE=C,SEQ
             FIELD NAME=SSN,START=41,BYTES=9,TYPE=C
             XDFLD NAME=SSN_IX,SEGMENT=CUSTOMER,SRCH=SSN,UNIQUE
             DBDGEN
        "#;

        let mut parser = DbdParser::new();
        let dbd = parser.parse(source).unwrap();

        assert_eq!(dbd.secondary_indexes.len(), 1);
        assert!(dbd.secondary_indexes[0].unique);
    }

    #[test]
    fn test_parse_lchild() {
        let source = r#"
             DBD   NAME=CUSTDB,ACCESS=HIDAM
             SEGM  NAME=CUSTOMER,BYTES=100,PARENT=0
             FIELD NAME=CUSTNO,START=1,BYTES=10,TYPE=C,SEQ
             LCHILD NAME=ORDER,SOURCE=ORDERDB
             SEGM  NAME=ADDRESS,BYTES=200,PARENT=CUSTOMER
             FIELD NAME=ADDRNO,START=1,BYTES=4,TYPE=C,SEQ
             DBDGEN
        "#;

        let mut parser = DbdParser::new();
        let dbd = parser.parse(source).unwrap();

        assert_eq!(dbd.logical_children.len(), 1);
        let lc = &dbd.logical_children[0];
        assert_eq!(lc.name, "ORDER");
        assert_eq!(lc.lchild_db, "ORDERDB");
        assert_eq!(lc.parent_segment, "CUSTOMER");
        assert!(!lc.paired);
    }

    #[test]
    fn test_get_secondary_index() {
        let mut dbd = DatabaseDefinition::new("TEST", AccessMethod::HIDAM);
        dbd.add_secondary_index(SecondaryIndex {
            name: "CUSTNAME_IX".to_string(),
            segment: "CUSTOMER".to_string(),
            search_field: "CUSTNAME".to_string(),
            unique: false,
        });

        assert!(dbd.get_secondary_index("CUSTNAME_IX").is_some());
        assert!(dbd.get_secondary_index("custname_ix").is_some());
        assert!(dbd.get_secondary_index("NONEXIST").is_none());
    }

    // --- Epic 410: DBD Parser Enhancements ---

    #[test]
    fn test_parse_dataset_macro() {
        let source = r#"
             DBD   NAME=CUSTDB,ACCESS=HIDAM
             DATASET DD1=CUSTDD,DEVICE=3390,SIZE=4096
             SEGM  NAME=CUSTOMER,BYTES=100,PARENT=0
             FIELD NAME=CUSTNO,START=1,BYTES=10,TYPE=C,SEQ
             DBDGEN
        "#;

        let mut parser = DbdParser::new();
        let dbd = parser.parse(source).unwrap();

        assert_eq!(dbd.datasets.len(), 1);
        let ds = &dbd.datasets[0];
        assert_eq!(ds.dd_name, "CUSTDD");
        assert_eq!(ds.device, Some("3390".to_string()));
        assert_eq!(ds.block_size, Some(4096));
    }

    #[test]
    fn test_parse_dataset_with_size_in_parens() {
        let source = r#"
             DBD   NAME=CUSTDB,ACCESS=HIDAM
             DATASET DD1=ORDERDD,DEVICE=3390,SIZE=(4096)
             SEGM  NAME=ORDERS,BYTES=200,PARENT=0
             FIELD NAME=ORDNO,START=1,BYTES=8,TYPE=C,SEQ
             DBDGEN
        "#;

        let mut parser = DbdParser::new();
        let dbd = parser.parse(source).unwrap();

        assert_eq!(dbd.datasets.len(), 1);
        assert_eq!(dbd.datasets[0].dd_name, "ORDERDD");
        assert_eq!(dbd.datasets[0].device, Some("3390".to_string()));
    }

    #[test]
    fn test_get_datasets_for_segment() {
        let mut dbd = DatabaseDefinition::new("TEST", AccessMethod::HIDAM);
        dbd.add_dataset(DatasetDefinition {
            dd_name: "CUSTDD".to_string(),
            device: Some("3390".to_string()),
            block_size: Some(4096),
            scan: None,
            segment: "CUSTOMER".to_string(),
        });

        let datasets = dbd.get_datasets("CUSTOMER");
        assert_eq!(datasets.len(), 1);
        assert_eq!(datasets[0].dd_name, "CUSTDD");

        let datasets = dbd.get_datasets("NONEXIST");
        assert!(datasets.is_empty());
    }

    #[test]
    fn test_lchild_with_name_tuple() {
        let source = r#"
             DBD   NAME=CUSTDB,ACCESS=HIDAM
             SEGM  NAME=CUSTOMER,BYTES=100,PARENT=0
             FIELD NAME=CUSTNO,START=1,BYTES=10,TYPE=C,SEQ
             LCHILD NAME=(ORDLINE,ORDERDB),POINTER=SNGL
             DBDGEN
        "#;

        let mut parser = DbdParser::new();
        let dbd = parser.parse(source).unwrap();

        assert_eq!(dbd.logical_children.len(), 1);
        let lc = &dbd.logical_children[0];
        assert_eq!(lc.name, "ORDLINE");
        assert_eq!(lc.lchild_db, "ORDERDB");
        assert_eq!(lc.parent_segment, "CUSTOMER");
        assert_eq!(lc.pointer, Some("SNGL".to_string()));
    }

    #[test]
    fn test_lchild_with_name_tuple_and_paired() {
        let source = r#"
             DBD   NAME=CUSTDB,ACCESS=HIDAM
             SEGM  NAME=CUSTOMER,BYTES=100,PARENT=0
             FIELD NAME=CUSTNO,START=1,BYTES=10,TYPE=C,SEQ
             LCHILD NAME=(DETAIL,DETAILDB),POINTER=DBLE,PAIRED
             DBDGEN
        "#;

        let mut parser = DbdParser::new();
        let dbd = parser.parse(source).unwrap();

        assert_eq!(dbd.logical_children.len(), 1);
        let lc = &dbd.logical_children[0];
        assert_eq!(lc.name, "DETAIL");
        assert_eq!(lc.lchild_db, "DETAILDB");
        assert_eq!(lc.pointer, Some("DBLE".to_string()));
        assert!(lc.paired);
    }

    #[test]
    fn test_variable_length_segment_bytes_tuple() {
        let source = r#"
             DBD   NAME=CUSTDB,ACCESS=HIDAM
             SEGM  NAME=REMARK,BYTES=(200,50),PARENT=0
             FIELD NAME=RMKNO,START=1,BYTES=4,TYPE=C,SEQ
             DBDGEN
        "#;

        let mut parser = DbdParser::new();
        let dbd = parser.parse(source).unwrap();

        let remark = dbd.get_segment("REMARK").unwrap();
        assert_eq!(remark.bytes, 200);
        assert_eq!(remark.min_bytes, Some(50));
        assert!(remark.variable_length);
    }

    #[test]
    fn test_variable_length_segment_with_comprtn() {
        let source = r#"
             DBD   NAME=CUSTDB,ACCESS=HIDAM
             SEGM  NAME=REMARK,BYTES=(200,50),COMPRTN=CMPRTN01,PARENT=0
             FIELD NAME=RMKNO,START=1,BYTES=4,TYPE=C,SEQ
             DBDGEN
        "#;

        let mut parser = DbdParser::new();
        let dbd = parser.parse(source).unwrap();

        let remark = dbd.get_segment("REMARK").unwrap();
        assert_eq!(remark.bytes, 200);
        assert_eq!(remark.min_bytes, Some(50));
        assert!(remark.variable_length);
        assert_eq!(remark.comprtn, Some("CMPRTN01".to_string()));
    }

    #[test]
    fn test_fixed_length_segment_no_min_bytes() {
        let source = r#"
             DBD   NAME=CUSTDB,ACCESS=HIDAM
             SEGM  NAME=CUSTOMER,BYTES=100,PARENT=0
             FIELD NAME=CUSTNO,START=1,BYTES=10,TYPE=C,SEQ
             DBDGEN
        "#;

        let mut parser = DbdParser::new();
        let dbd = parser.parse(source).unwrap();

        let customer = dbd.get_segment("CUSTOMER").unwrap();
        assert_eq!(customer.bytes, 100);
        assert_eq!(customer.min_bytes, None);
        assert!(!customer.variable_length);
        assert_eq!(customer.comprtn, None);
    }

    #[test]
    fn test_parse_bytes_param_simple() {
        assert_eq!(parse_bytes_param("100"), (100, None));
    }

    #[test]
    fn test_parse_bytes_param_tuple() {
        assert_eq!(parse_bytes_param("200,50"), (200, Some(50)));
    }

    #[test]
    fn test_parse_name_tuple_simple() {
        assert_eq!(parse_name_tuple("ORDER"), ("ORDER".to_string(), String::new()));
    }

    #[test]
    fn test_parse_name_tuple_pair() {
        assert_eq!(parse_name_tuple("ORDLINE,ORDERDB"), ("ORDLINE".to_string(), "ORDERDB".to_string()));
    }

    #[test]
    fn test_combined_dataset_lchild_variable_segment() {
        let source = r#"
             DBD   NAME=ORDERDB,ACCESS=HDAM
             DATASET DD1=ORDDD,DEVICE=3390,SIZE=8192
             SEGM  NAME=ORDER,BYTES=100,PARENT=0
             FIELD NAME=ORDNO,START=1,BYTES=8,TYPE=C,SEQ
             LCHILD NAME=(SHIPMENT,SHIPDB),POINTER=SNGL
             SEGM  NAME=LINEITEM,BYTES=(80,20),COMPRTN=CMPACK01,PARENT=ORDER
             FIELD NAME=LINENO,START=1,BYTES=4,TYPE=C,SEQ
             DBDGEN
        "#;

        let mut parser = DbdParser::new();
        let dbd = parser.parse(source).unwrap();

        // DATASET check
        assert_eq!(dbd.datasets.len(), 1);
        assert_eq!(dbd.datasets[0].dd_name, "ORDDD");
        assert_eq!(dbd.datasets[0].block_size, Some(8192));

        // LCHILD check
        assert_eq!(dbd.logical_children.len(), 1);
        assert_eq!(dbd.logical_children[0].name, "SHIPMENT");
        assert_eq!(dbd.logical_children[0].lchild_db, "SHIPDB");

        // Variable-length segment check
        let lineitem = dbd.get_segment("LINEITEM").unwrap();
        assert_eq!(lineitem.bytes, 80);
        assert_eq!(lineitem.min_bytes, Some(20));
        assert!(lineitem.variable_length);
        assert_eq!(lineitem.comprtn, Some("CMPACK01".to_string()));
    }

    #[test]
    fn test_get_logical_children() {
        let mut dbd = DatabaseDefinition::new("TEST", AccessMethod::HIDAM);
        dbd.add_logical_child(LogicalChild {
            name: "ORDER".to_string(),
            lchild_db: "ORDERDB".to_string(),
            parent_segment: "CUSTOMER".to_string(),
            pointer: None,
            paired: false,
        });

        let children = dbd.get_logical_children("CUSTOMER");
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].name, "ORDER");

        let children = dbd.get_logical_children("NONEXIST");
        assert!(children.is_empty());
    }
}
