//! FOC-101: Master File Descriptor (5 stories).
//!
//! Parses MFD (Master File Descriptor) definitions that describe the layout
//! of FOCUS data files, including field metadata, segment hierarchies, and
//! access file mappings.

use std::collections::HashMap;
use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum MfdError {
    #[error("MFD parse error: {0}")]
    ParseError(String),
    #[error("unknown data type: {0}")]
    UnknownDataType(String),
    #[error("segment not found: {0}")]
    SegmentNotFound(String),
    #[error("field not found: {0}")]
    FieldNotFound(String),
    #[error("access file error: {0}")]
    AccessFileError(String),
}

// ---------------------------------------------------------------------------
// Data types
// ---------------------------------------------------------------------------

/// FOCUS data types found in MFD USAGE specifications.
#[derive(Debug, Clone, PartialEq)]
pub enum FocusDataType {
    /// Alphanumeric — `An` where n is length.
    Alpha(usize),
    /// Integer — `I` with optional length.
    Integer(usize),
    /// Floating point — `F` or `Fn.d`.
    Float(usize, usize),
    /// Double precision — `D` or `Dn.d`.
    Double(usize, usize),
    /// Packed decimal — `Pn.d`.
    Packed(usize, usize),
    /// Date — `YYMD`, `YMD`, `MDY`, etc.
    Date(String),
    /// Text — large text field.
    Text(usize),
}

impl FocusDataType {
    /// Parse a USAGE string such as `A20`, `I4`, `F8.2`, `P10.2`, `YYMD`.
    pub fn parse(s: &str) -> Result<Self, MfdError> {
        let s = s.trim();
        if s.is_empty() {
            return Err(MfdError::UnknownDataType(s.to_string()));
        }
        let first = s.chars().next().unwrap();
        match first {
            'A' | 'a' => {
                let len: usize = s[1..].parse().unwrap_or(1);
                Ok(FocusDataType::Alpha(len))
            }
            'I' | 'i' => {
                let len: usize = s[1..].parse().unwrap_or(4);
                Ok(FocusDataType::Integer(len))
            }
            'F' | 'f' => {
                let (w, d) = parse_width_decimal(&s[1..]);
                Ok(FocusDataType::Float(w, d))
            }
            'D' | 'd' => {
                let (w, d) = parse_width_decimal(&s[1..]);
                Ok(FocusDataType::Double(w, d))
            }
            'P' | 'p' => {
                let (w, d) = parse_width_decimal(&s[1..]);
                Ok(FocusDataType::Packed(w, d))
            }
            'T' | 't' => {
                let len: usize = s[1..].parse().unwrap_or(80);
                Ok(FocusDataType::Text(len))
            }
            'Y' | 'M' | 'y' | 'm' => Ok(FocusDataType::Date(s.to_uppercase())),
            _ => Err(MfdError::UnknownDataType(s.to_string())),
        }
    }
}

fn parse_width_decimal(s: &str) -> (usize, usize) {
    if let Some(dot_pos) = s.find('.') {
        let w: usize = s[..dot_pos].parse().unwrap_or(8);
        let d: usize = s[dot_pos + 1..].parse().unwrap_or(0);
        (w, d)
    } else {
        let w: usize = s.parse().unwrap_or(8);
        (w, 0)
    }
}

// ---------------------------------------------------------------------------
// Field definition
// ---------------------------------------------------------------------------

/// A single field in an MFD.
#[derive(Debug, Clone, PartialEq)]
pub struct FieldDef {
    pub name: String,
    pub alias: Option<String>,
    pub data_type: FocusDataType,
    pub length: usize,
    pub index: bool,
    pub missing_value: Option<String>,
    pub title: Option<String>,
    pub description: Option<String>,
}

impl FieldDef {
    pub fn new(name: &str, data_type: FocusDataType) -> Self {
        let length = match &data_type {
            FocusDataType::Alpha(n) | FocusDataType::Text(n) => *n,
            FocusDataType::Integer(n) => *n,
            FocusDataType::Float(w, _) | FocusDataType::Double(w, _) | FocusDataType::Packed(w, _) => *w,
            FocusDataType::Date(_) => 10,
        };
        Self {
            name: name.to_string(),
            alias: None,
            data_type,
            length,
            index: false,
            missing_value: None,
            title: None,
            description: None,
        }
    }

    /// Returns the effective display name (alias if set, otherwise field name).
    pub fn display_name(&self) -> &str {
        self.alias.as_deref().unwrap_or(&self.name)
    }
}

// ---------------------------------------------------------------------------
// Segment
// ---------------------------------------------------------------------------

/// A segment in a multi-segment MFD hierarchy.
#[derive(Debug, Clone, PartialEq)]
pub struct Segment {
    pub name: String,
    pub segment_type: SegmentType,
    pub parent: Option<String>,
    pub fields: Vec<FieldDef>,
}

/// Segment types in the hierarchy.
#[derive(Debug, Clone, PartialEq)]
pub enum SegmentType {
    /// Root segment (S1).
    Root,
    /// Child segment (Sn with parent).
    Child,
    /// Key-sequenced child.
    KeySequenced,
}

impl Segment {
    pub fn new(name: &str, segment_type: SegmentType) -> Self {
        Self {
            name: name.to_string(),
            segment_type,
            parent: None,
            fields: Vec::new(),
        }
    }

    pub fn add_field(&mut self, field: FieldDef) {
        self.fields.push(field);
    }

    pub fn find_field(&self, name: &str) -> Option<&FieldDef> {
        self.fields
            .iter()
            .find(|f| f.name.eq_ignore_ascii_case(name) || f.alias.as_deref().is_some_and(|a| a.eq_ignore_ascii_case(name)))
    }
}

// ---------------------------------------------------------------------------
// Access file
// ---------------------------------------------------------------------------

/// Maps an MFD to its physical data source.
#[derive(Debug, Clone, PartialEq)]
pub struct AccessFile {
    pub filename: String,
    pub suffix: String,
    pub adapter: String,
    pub connection: Option<String>,
    pub dataset: Option<String>,
    pub properties: HashMap<String, String>,
}

impl AccessFile {
    pub fn new(filename: &str, suffix: &str, adapter: &str) -> Self {
        Self {
            filename: filename.to_string(),
            suffix: suffix.to_string(),
            adapter: adapter.to_string(),
            connection: None,
            dataset: None,
            properties: HashMap::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// MFD Parser
// ---------------------------------------------------------------------------

/// Parsed Master File Descriptor.
#[derive(Debug, Clone)]
pub struct MasterFileDescriptor {
    pub filename: String,
    pub suffix: String,
    pub segments: Vec<Segment>,
    pub access_file: Option<AccessFile>,
}

impl MasterFileDescriptor {
    /// Look up a field across all segments.
    pub fn find_field(&self, name: &str) -> Option<&FieldDef> {
        for seg in &self.segments {
            if let Some(f) = seg.find_field(name) {
                return Some(f);
            }
        }
        None
    }

    /// Get all fields across all segments.
    pub fn all_fields(&self) -> Vec<&FieldDef> {
        self.segments.iter().flat_map(|s| &s.fields).collect()
    }

    /// Find a segment by name.
    pub fn find_segment(&self, name: &str) -> Option<&Segment> {
        self.segments.iter().find(|s| s.name.eq_ignore_ascii_case(name))
    }

    /// Get child segments of a given parent.
    pub fn child_segments(&self, parent: &str) -> Vec<&Segment> {
        self.segments
            .iter()
            .filter(|s| s.parent.as_deref().is_some_and(|p| p.eq_ignore_ascii_case(parent)))
            .collect()
    }
}

/// Parses MFD source text.
pub struct MfdParser;

impl MfdParser {
    /// Parse an MFD definition string.
    ///
    /// Expected format (line-based, keyword=value pairs):
    /// ```text
    /// FILENAME=EMPLOYEE, SUFFIX=FOC
    /// SEGNAME=EMPSEG, SEGTYPE=S1
    /// FIELDNAME=EMPID, ALIAS=EID, USAGE=A6, INDEX=I
    /// FIELDNAME=NAME, ALIAS=ENAME, USAGE=A30
    /// FIELDNAME=SALARY, USAGE=F8.2
    /// ```
    pub fn parse(src: &str) -> Result<MasterFileDescriptor, MfdError> {
        let mut filename = String::new();
        let mut suffix = String::from("FOC");
        let mut segments: Vec<Segment> = Vec::new();
        let mut current_segment: Option<Segment> = None;

        for line in src.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('*') {
                continue; // skip blanks and comments
            }

            let pairs = parse_kv_line(line);

            if let Some(fname) = pairs.get("FILENAME") {
                filename = fname.clone();
                if let Some(sfx) = pairs.get("SUFFIX") {
                    suffix = sfx.clone();
                }
                continue;
            }

            if let Some(segname) = pairs.get("SEGNAME") {
                // Save previous segment
                if let Some(seg) = current_segment.take() {
                    segments.push(seg);
                }
                let seg_type_str = pairs.get("SEGTYPE").cloned().unwrap_or_default();
                let seg_type = match seg_type_str.as_str() {
                    "S1" => SegmentType::Root,
                    "KS" => SegmentType::KeySequenced,
                    _ => SegmentType::Child,
                };
                let mut seg = Segment::new(segname, seg_type);
                if let Some(parent) = pairs.get("PARENT") {
                    seg.parent = Some(parent.clone());
                }
                current_segment = Some(seg);
                continue;
            }

            if let Some(fieldname) = pairs.get("FIELDNAME") {
                let usage_str = pairs.get("USAGE").cloned().unwrap_or_else(|| "A1".to_string());
                let data_type = FocusDataType::parse(&usage_str)?;
                let mut field = FieldDef::new(fieldname, data_type);
                if let Some(alias) = pairs.get("ALIAS") {
                    field.alias = Some(alias.clone());
                }
                if let Some(idx) = pairs.get("INDEX") {
                    field.index = idx == "I" || idx == "Y";
                }
                if let Some(title) = pairs.get("TITLE") {
                    field.title = Some(title.clone());
                }
                if let Some(missing) = pairs.get("MISSING") {
                    field.missing_value = Some(missing.clone());
                }
                if let Some(desc) = pairs.get("DESCRIPTION") {
                    field.description = Some(desc.clone());
                }

                if let Some(ref mut seg) = current_segment {
                    seg.add_field(field);
                } else {
                    // Create a default root segment if none declared
                    let mut seg = Segment::new("DEFAULT", SegmentType::Root);
                    seg.add_field(field);
                    current_segment = Some(seg);
                }
            }
        }

        // Push final segment
        if let Some(seg) = current_segment {
            segments.push(seg);
        }

        if filename.is_empty() {
            return Err(MfdError::ParseError("FILENAME not specified".into()));
        }

        Ok(MasterFileDescriptor {
            filename,
            suffix,
            segments,
            access_file: None,
        })
    }

    /// Parse an access file definition.
    pub fn parse_access_file(src: &str) -> Result<AccessFile, MfdError> {
        let mut filename = String::new();
        let mut suffix = String::new();
        let mut adapter = String::new();
        let mut connection = None;
        let mut dataset = None;
        let mut properties = HashMap::new();

        for line in src.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('*') {
                continue;
            }
            let pairs = parse_kv_line(line);
            if let Some(f) = pairs.get("FILENAME") {
                filename = f.clone();
            }
            if let Some(s) = pairs.get("SUFFIX") {
                suffix = s.clone();
            }
            if let Some(a) = pairs.get("ADAPTER") {
                adapter = a.clone();
            }
            if let Some(c) = pairs.get("CONNECTION") {
                connection = Some(c.clone());
            }
            if let Some(d) = pairs.get("DATASET") {
                dataset = Some(d.clone());
            }
            // Remaining pairs go to properties
            for (k, v) in &pairs {
                if !["FILENAME", "SUFFIX", "ADAPTER", "CONNECTION", "DATASET"].contains(&k.as_str())
                {
                    properties.insert(k.clone(), v.clone());
                }
            }
        }

        if filename.is_empty() {
            return Err(MfdError::AccessFileError("FILENAME not specified".into()));
        }

        let mut af = AccessFile::new(&filename, &suffix, &adapter);
        af.connection = connection;
        af.dataset = dataset;
        af.properties = properties;
        Ok(af)
    }
}

/// Parse a single MFD line of `KEY=VALUE, KEY=VALUE, ...` pairs.
fn parse_kv_line(line: &str) -> HashMap<String, String> {
    let mut map = HashMap::new();
    for pair in line.split(',') {
        let pair = pair.trim();
        if let Some(eq_pos) = pair.find('=') {
            let key = pair[..eq_pos].trim().to_uppercase();
            let value = pair[eq_pos + 1..].trim().to_string();
            map.insert(key, value);
        }
    }
    map
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_mfd() -> &'static str {
        "FILENAME=EMPLOYEE, SUFFIX=FOC\n\
         SEGNAME=EMPSEG, SEGTYPE=S1\n\
         FIELDNAME=EMPID, ALIAS=EID, USAGE=A6, INDEX=I\n\
         FIELDNAME=NAME, ALIAS=ENAME, USAGE=A30\n\
         FIELDNAME=SALARY, USAGE=F8.2\n\
         FIELDNAME=HIREDATE, USAGE=YYMD\n\
         FIELDNAME=DEPT, USAGE=A10"
    }

    #[test]
    fn test_parse_mfd_basic() {
        let mfd = MfdParser::parse(sample_mfd()).unwrap();
        assert_eq!(mfd.filename, "EMPLOYEE");
        assert_eq!(mfd.suffix, "FOC");
        assert_eq!(mfd.segments.len(), 1);
        assert_eq!(mfd.segments[0].name, "EMPSEG");
        assert_eq!(mfd.segments[0].fields.len(), 5);
    }

    #[test]
    fn test_field_metadata() {
        let mfd = MfdParser::parse(sample_mfd()).unwrap();
        let empid = mfd.find_field("EMPID").unwrap();
        assert_eq!(empid.name, "EMPID");
        assert_eq!(empid.alias, Some("EID".to_string()));
        assert!(empid.index);
        assert_eq!(empid.data_type, FocusDataType::Alpha(6));
    }

    #[test]
    fn test_field_display_name() {
        let mfd = MfdParser::parse(sample_mfd()).unwrap();
        let empid = mfd.find_field("EMPID").unwrap();
        assert_eq!(empid.display_name(), "EID");
        let salary = mfd.find_field("SALARY").unwrap();
        assert_eq!(salary.display_name(), "SALARY");
    }

    #[test]
    fn test_data_type_parsing() {
        assert_eq!(FocusDataType::parse("A20").unwrap(), FocusDataType::Alpha(20));
        assert_eq!(FocusDataType::parse("I4").unwrap(), FocusDataType::Integer(4));
        assert_eq!(FocusDataType::parse("F8.2").unwrap(), FocusDataType::Float(8, 2));
        assert_eq!(FocusDataType::parse("D16.4").unwrap(), FocusDataType::Double(16, 4));
        assert_eq!(FocusDataType::parse("P10.2").unwrap(), FocusDataType::Packed(10, 2));
        assert!(matches!(FocusDataType::parse("YYMD").unwrap(), FocusDataType::Date(_)));
    }

    #[test]
    fn test_data_type_unknown() {
        assert!(FocusDataType::parse("Z99").is_err());
    }

    #[test]
    fn test_multi_segment_mfd() {
        let src = "\
FILENAME=ORDER, SUFFIX=FOC\n\
SEGNAME=ORDERSEG, SEGTYPE=S1\n\
FIELDNAME=ORDERID, USAGE=A10, INDEX=I\n\
FIELDNAME=CUSTID, USAGE=A8\n\
SEGNAME=ITEMSEG, SEGTYPE=S2, PARENT=ORDERSEG\n\
FIELDNAME=ITEMID, USAGE=A10, INDEX=I\n\
FIELDNAME=QTY, USAGE=I4\n\
FIELDNAME=PRICE, USAGE=F8.2";
        let mfd = MfdParser::parse(src).unwrap();
        assert_eq!(mfd.segments.len(), 2);
        assert_eq!(mfd.segments[0].name, "ORDERSEG");
        assert_eq!(mfd.segments[0].segment_type, SegmentType::Root);
        assert_eq!(mfd.segments[1].name, "ITEMSEG");
        assert_eq!(mfd.segments[1].parent, Some("ORDERSEG".to_string()));
    }

    #[test]
    fn test_segment_hierarchy() {
        let src = "\
FILENAME=COMPANY, SUFFIX=FOC\n\
SEGNAME=DEPT, SEGTYPE=S1\n\
FIELDNAME=DEPTID, USAGE=A4\n\
SEGNAME=EMP, SEGTYPE=S2, PARENT=DEPT\n\
FIELDNAME=EMPID, USAGE=A6\n\
SEGNAME=SKILLS, SEGTYPE=S3, PARENT=EMP\n\
FIELDNAME=SKILL, USAGE=A20";
        let mfd = MfdParser::parse(src).unwrap();
        assert_eq!(mfd.segments.len(), 3);
        let children = mfd.child_segments("DEPT");
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].name, "EMP");
        let grandchildren = mfd.child_segments("EMP");
        assert_eq!(grandchildren.len(), 1);
        assert_eq!(grandchildren[0].name, "SKILLS");
    }

    #[test]
    fn test_find_field_across_segments() {
        let src = "\
FILENAME=MULTI, SUFFIX=FOC\n\
SEGNAME=SEG1, SEGTYPE=S1\n\
FIELDNAME=FIELD_A, USAGE=A10\n\
SEGNAME=SEG2, SEGTYPE=S2, PARENT=SEG1\n\
FIELDNAME=FIELD_B, USAGE=I4";
        let mfd = MfdParser::parse(src).unwrap();
        assert!(mfd.find_field("FIELD_A").is_some());
        assert!(mfd.find_field("FIELD_B").is_some());
        assert!(mfd.find_field("FIELD_C").is_none());
    }

    #[test]
    fn test_all_fields() {
        let mfd = MfdParser::parse(sample_mfd()).unwrap();
        let all = mfd.all_fields();
        assert_eq!(all.len(), 5);
    }

    #[test]
    fn test_access_file_parsing() {
        let src = "\
FILENAME=EMPLOYEE, SUFFIX=DB2\n\
ADAPTER=DB2, CONNECTION=DSN=SAMPLE\n\
DATASET=SCHEMA.EMPLOYEE";
        let af = MfdParser::parse_access_file(src).unwrap();
        assert_eq!(af.filename, "EMPLOYEE");
        assert_eq!(af.suffix, "DB2");
        assert_eq!(af.adapter, "DB2");
        assert_eq!(af.connection, Some("DSN=SAMPLE".to_string()));
        assert_eq!(af.dataset, Some("SCHEMA.EMPLOYEE".to_string()));
    }

    #[test]
    fn test_mfd_missing_filename() {
        let src = "SEGNAME=SEG1, SEGTYPE=S1\nFIELDNAME=X, USAGE=A1";
        assert!(MfdParser::parse(src).is_err());
    }

    #[test]
    fn test_access_file_properties() {
        let src = "FILENAME=TEST, SUFFIX=SEQ, ADAPTER=SEQUENTIAL, RECLEN=80";
        let af = MfdParser::parse_access_file(src).unwrap();
        assert_eq!(af.properties.get("RECLEN"), Some(&"80".to_string()));
    }

    #[test]
    fn test_field_with_title_and_missing() {
        let src = "\
FILENAME=T, SUFFIX=FOC\n\
SEGNAME=S, SEGTYPE=S1\n\
FIELDNAME=AMT, USAGE=F8.2, TITLE=Amount, MISSING=0.00";
        let mfd = MfdParser::parse(src).unwrap();
        let f = mfd.find_field("AMT").unwrap();
        assert_eq!(f.title, Some("Amount".to_string()));
        assert_eq!(f.missing_value, Some("0.00".to_string()));
    }

    #[test]
    fn test_index_field() {
        let mfd = MfdParser::parse(sample_mfd()).unwrap();
        let empid = mfd.find_field("EMPID").unwrap();
        assert!(empid.index);
        let name = mfd.find_field("NAME").unwrap();
        assert!(!name.index);
    }

    #[test]
    fn test_comment_lines_skipped() {
        let src = "\
* This is a comment\n\
FILENAME=T, SUFFIX=FOC\n\
* Another comment\n\
SEGNAME=S, SEGTYPE=S1\n\
FIELDNAME=X, USAGE=A5";
        let mfd = MfdParser::parse(src).unwrap();
        assert_eq!(mfd.filename, "T");
        assert_eq!(mfd.segments[0].fields.len(), 1);
    }
}
