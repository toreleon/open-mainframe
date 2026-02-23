//! IDMS-101: Schema & Subschema DDL (5 stories).
//!
//! Provides parsing of SCHEMA DDL statements (`ADD RECORD`, `ADD SET`,
//! `ADD AREA`) and SUBSCHEMA DDL that creates a filtered view of the
//! full schema for application programs.

use std::collections::HashSet;

use crate::codasyl::{
    AreaDef, CodasylSchema, FieldType, IdmsError, RecordType, SetOrder, SetType,
};

// ---------------------------------------------------------------------------
//  Schema parser
// ---------------------------------------------------------------------------

/// Parser for IDMS SCHEMA DDL statements.
///
/// Accepts a simplified DDL syntax:
/// ```text
/// ADD SCHEMA NAME IS schema_name.
/// ADD AREA NAME IS area_name RANGE IS start_page THRU end_page.
/// ADD RECORD NAME IS record_name ID IS nnn AREA IS area_name.
///     ADD FIELD NAME IS field_name TYPE IS CHAR(n)|INT|LONG|DECIMAL(p,s)|BINARY(n).
/// ADD SET NAME IS set_name OWNER IS owner_name ORDER IS FIRST|LAST|NEXT|PRIOR|SORTED.
///     ADD MEMBER IS member_name.
/// ```
#[derive(Debug)]
pub struct SchemaParser;

impl SchemaParser {
    /// Parse DDL text and return a populated [`CodasylSchema`].
    pub fn parse(input: &str) -> Result<CodasylSchema, SchemaError> {
        let mut schema = CodasylSchema::default();
        let lines: Vec<&str> = input.lines().map(str::trim).collect();
        let mut i = 0;

        while i < lines.len() {
            let line = lines[i];
            if line.is_empty() || line.starts_with("//") || line.starts_with('*') {
                i += 1;
                continue;
            }
            let tokens: Vec<&str> = line.split_whitespace().collect();
            if tokens.len() < 2 {
                i += 1;
                continue;
            }

            match (tokens[0].to_uppercase().as_str(), tokens[1].to_uppercase().as_str()) {
                ("ADD", "SCHEMA") => {
                    schema.name = Self::extract_name(&tokens)?;
                }
                ("ADD", "AREA") => {
                    let area = Self::parse_area(&tokens)?;
                    schema.add_area(area).map_err(SchemaError::Model)?;
                }
                ("ADD", "RECORD") => {
                    let mut rec = Self::parse_record(&tokens)?;
                    // Gather subsequent field lines.
                    i += 1;
                    while i < lines.len() {
                        let fline = lines[i].trim();
                        let ftokens: Vec<&str> = fline.split_whitespace().collect();
                        if ftokens.len() >= 2
                            && ftokens[0].eq_ignore_ascii_case("ADD")
                            && ftokens[1].eq_ignore_ascii_case("FIELD")
                        {
                            let (fname, ftype) = Self::parse_field(&ftokens)?;
                            rec.add_field(&fname, ftype);
                            i += 1;
                        } else {
                            break;
                        }
                    }
                    schema.add_record(rec).map_err(SchemaError::Model)?;
                    continue; // already advanced i
                }
                ("ADD", "SET") => {
                    let mut set = Self::parse_set(&tokens)?;
                    // Gather subsequent member lines.
                    i += 1;
                    while i < lines.len() {
                        let mline = lines[i].trim();
                        let mtokens: Vec<&str> = mline.split_whitespace().collect();
                        if mtokens.len() >= 2
                            && mtokens[0].eq_ignore_ascii_case("ADD")
                            && mtokens[1].eq_ignore_ascii_case("MEMBER")
                        {
                            let member_name = Self::extract_is_value(&mtokens, "MEMBER")?;
                            set.add_member(&member_name);
                            i += 1;
                        } else {
                            break;
                        }
                    }
                    schema.add_set(set).map_err(SchemaError::Model)?;
                    continue;
                }
                _ => {}
            }
            i += 1;
        }

        if schema.name.is_empty() {
            return Err(SchemaError::MissingSchemaName);
        }
        Ok(schema)
    }

    // -- helpers --

    fn extract_name(tokens: &[&str]) -> Result<String, SchemaError> {
        Self::extract_is_value(tokens, "NAME")
    }

    /// Extract `keyword IS value` from a token list.
    pub fn extract_is_value(tokens: &[&str], keyword: &str) -> Result<String, SchemaError> {
        for (j, tok) in tokens.iter().enumerate() {
            if tok.eq_ignore_ascii_case(keyword) {
                // expect IS next, then value
                if j + 2 < tokens.len() && tokens[j + 1].eq_ignore_ascii_case("IS") {
                    return Ok(tokens[j + 2].trim_end_matches('.').to_uppercase());
                }
            }
        }
        Err(SchemaError::MissingKeyword(keyword.to_string()))
    }

    fn parse_area(tokens: &[&str]) -> Result<AreaDef, SchemaError> {
        let name = Self::extract_is_value(tokens, "NAME")?;
        let start = Self::extract_is_value(tokens, "RANGE")?
            .parse::<u32>()
            .map_err(|_| SchemaError::InvalidNumber("start page".into()))?;
        // Find THRU
        let end_str = {
            let mut val = None;
            for (j, tok) in tokens.iter().enumerate() {
                if tok.eq_ignore_ascii_case("THRU") && j + 1 < tokens.len() {
                    val = Some(tokens[j + 1].trim_end_matches('.'));
                    break;
                }
            }
            val.ok_or_else(|| SchemaError::MissingKeyword("THRU".into()))?
        };
        let end = end_str
            .parse::<u32>()
            .map_err(|_| SchemaError::InvalidNumber("end page".into()))?;
        Ok(AreaDef::new(&name, start, end))
    }

    fn parse_record(tokens: &[&str]) -> Result<RecordType, SchemaError> {
        let name = Self::extract_is_value(tokens, "NAME")?;
        let id_str = Self::extract_is_value(tokens, "ID")?;
        let id = id_str
            .parse::<u32>()
            .map_err(|_| SchemaError::InvalidNumber("record ID".into()))?;
        let area = Self::extract_is_value(tokens, "AREA")?;
        Ok(RecordType::new(id, &name, &area))
    }

    fn parse_set(tokens: &[&str]) -> Result<SetType, SchemaError> {
        let name = Self::extract_is_value(tokens, "NAME")?;
        let owner = Self::extract_is_value(tokens, "OWNER")?;
        let order_str = Self::extract_is_value(tokens, "ORDER")?;
        let order = match order_str.as_str() {
            "FIRST" => SetOrder::First,
            "LAST" => SetOrder::Last,
            "NEXT" => SetOrder::Next,
            "PRIOR" => SetOrder::Prior,
            "SORTED" => SetOrder::Sorted,
            _ => return Err(SchemaError::InvalidSetOrder(order_str)),
        };
        Ok(SetType::new(&name, &owner, order))
    }

    fn parse_field(tokens: &[&str]) -> Result<(String, FieldType), SchemaError> {
        let name = Self::extract_is_value(tokens, "NAME")?;
        let type_str = Self::extract_is_value(tokens, "TYPE")?;
        let ft = Self::parse_field_type(&type_str)?;
        Ok((name, ft))
    }

    fn parse_field_type(s: &str) -> Result<FieldType, SchemaError> {
        let s = s.to_uppercase();
        if s == "INT" {
            return Ok(FieldType::Int);
        }
        if s == "LONG" {
            return Ok(FieldType::Long);
        }
        if let Some(inner) = s.strip_prefix("CHAR(").and_then(|r| r.strip_suffix(')')) {
            let n: usize = inner
                .parse()
                .map_err(|_| SchemaError::InvalidNumber("CHAR length".into()))?;
            return Ok(FieldType::Char(n));
        }
        if let Some(inner) = s.strip_prefix("DECIMAL(").and_then(|r| r.strip_suffix(')')) {
            let parts: Vec<&str> = inner.split(',').collect();
            if parts.len() != 2 {
                return Err(SchemaError::InvalidFieldType(s));
            }
            let p: u8 = parts[0]
                .trim()
                .parse()
                .map_err(|_| SchemaError::InvalidNumber("precision".into()))?;
            let sc: u8 = parts[1]
                .trim()
                .parse()
                .map_err(|_| SchemaError::InvalidNumber("scale".into()))?;
            return Ok(FieldType::Decimal(p, sc));
        }
        if let Some(inner) = s.strip_prefix("BINARY(").and_then(|r| r.strip_suffix(')')) {
            let n: usize = inner
                .parse()
                .map_err(|_| SchemaError::InvalidNumber("BINARY length".into()))?;
            return Ok(FieldType::Binary(n));
        }
        Err(SchemaError::InvalidFieldType(s))
    }
}

// ---------------------------------------------------------------------------
//  Subschema
// ---------------------------------------------------------------------------

/// A subschema -- a filtered subset of a full schema that exposes only
/// the record types, set types, and areas needed by a particular program.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Subschema {
    /// Subschema name.
    pub name: String,
    /// Source schema name.
    pub schema_name: String,
    /// Included record type names.
    pub record_names: HashSet<String>,
    /// Included set type names.
    pub set_names: HashSet<String>,
    /// Included area names.
    pub area_names: HashSet<String>,
}

impl Subschema {
    /// Create a new empty subschema referencing the given schema.
    pub fn new(name: &str, schema_name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            schema_name: schema_name.to_uppercase(),
            record_names: HashSet::new(),
            set_names: HashSet::new(),
            area_names: HashSet::new(),
        }
    }

    /// Include a record type.
    pub fn include_record(&mut self, name: &str) {
        self.record_names.insert(name.to_uppercase());
    }

    /// Include a set type.
    pub fn include_set(&mut self, name: &str) {
        self.set_names.insert(name.to_uppercase());
    }

    /// Include an area.
    pub fn include_area(&mut self, name: &str) {
        self.area_names.insert(name.to_uppercase());
    }

    /// Validate that all included names exist in the full schema.
    pub fn validate(&self, schema: &CodasylSchema) -> Result<(), SchemaError> {
        for r in &self.record_names {
            if schema.get_record(r).is_none() {
                return Err(SchemaError::Model(IdmsError::RecordNotFound(r.clone())));
            }
        }
        for s in &self.set_names {
            if schema.get_set(s).is_none() {
                return Err(SchemaError::Model(IdmsError::SetNotFound(s.clone())));
            }
        }
        for a in &self.area_names {
            if schema.get_area(a).is_none() {
                return Err(SchemaError::Model(IdmsError::AreaNotFound(a.clone())));
            }
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
//  Subschema parser
// ---------------------------------------------------------------------------

/// Parser for SUBSCHEMA DDL.
///
/// Syntax:
/// ```text
/// ADD SUBSCHEMA NAME IS sub_name OF SCHEMA schema_name.
/// ADD RECORD NAME IS record_name.
/// ADD SET NAME IS set_name.
/// ADD AREA NAME IS area_name.
/// ```
#[derive(Debug)]
pub struct SubschemaParser;

impl SubschemaParser {
    /// Parse subschema DDL text.
    pub fn parse(input: &str) -> Result<Subschema, SchemaError> {
        let mut sub: Option<Subschema> = None;

        for line in input.lines().map(str::trim) {
            if line.is_empty() || line.starts_with("//") || line.starts_with('*') {
                continue;
            }
            let tokens: Vec<&str> = line.split_whitespace().collect();
            if tokens.len() < 2 {
                continue;
            }
            if !tokens[0].eq_ignore_ascii_case("ADD") {
                continue;
            }

            match tokens[1].to_uppercase().as_str() {
                "SUBSCHEMA" => {
                    let name = SchemaParser::extract_is_value(&tokens, "NAME")?;
                    let schema_name = Self::extract_of_schema(&tokens)?;
                    sub = Some(Subschema::new(&name, &schema_name));
                }
                "RECORD" => {
                    let s = sub.as_mut().ok_or(SchemaError::MissingSchemaName)?;
                    let rname = SchemaParser::extract_is_value(&tokens, "NAME")?;
                    s.include_record(&rname);
                }
                "SET" => {
                    let s = sub.as_mut().ok_or(SchemaError::MissingSchemaName)?;
                    let sname = SchemaParser::extract_is_value(&tokens, "NAME")?;
                    s.include_set(&sname);
                }
                "AREA" => {
                    let s = sub.as_mut().ok_or(SchemaError::MissingSchemaName)?;
                    let aname = SchemaParser::extract_is_value(&tokens, "NAME")?;
                    s.include_area(&aname);
                }
                _ => {}
            }
        }

        sub.ok_or(SchemaError::MissingSchemaName)
    }

    /// Extract schema name from `OF SCHEMA schema_name` pattern.
    fn extract_of_schema(tokens: &[&str]) -> Result<String, SchemaError> {
        for (j, tok) in tokens.iter().enumerate() {
            if tok.eq_ignore_ascii_case("OF")
                && j + 2 < tokens.len()
                && tokens[j + 1].eq_ignore_ascii_case("SCHEMA")
            {
                return Ok(tokens[j + 2].trim_end_matches('.').to_uppercase());
            }
        }
        Err(SchemaError::MissingKeyword("OF SCHEMA".to_string()))
    }
}

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors from schema/subschema parsing.
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum SchemaError {
    /// Missing SCHEMA NAME statement.
    #[error("missing SCHEMA NAME statement")]
    MissingSchemaName,
    /// Required keyword not found.
    #[error("missing keyword: {0}")]
    MissingKeyword(String),
    /// Invalid numeric value.
    #[error("invalid number for {0}")]
    InvalidNumber(String),
    /// Invalid set order value.
    #[error("invalid set order: {0}")]
    InvalidSetOrder(String),
    /// Invalid field type string.
    #[error("invalid field type: {0}")]
    InvalidFieldType(String),
    /// Underlying model error.
    #[error(transparent)]
    Model(#[from] IdmsError),
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE_DDL: &str = "\
ADD SCHEMA NAME IS EMPSCHM.
ADD AREA NAME IS EMP-AREA RANGE IS 1 THRU 500.
ADD AREA NAME IS DEPT-AREA RANGE IS 501 THRU 600.
ADD RECORD NAME IS DEPARTMENT ID IS 100 AREA IS DEPT-AREA.
    ADD FIELD NAME IS DEPT-ID TYPE IS INT.
    ADD FIELD NAME IS DEPT-NAME TYPE IS CHAR(30).
ADD RECORD NAME IS EMPLOYEE ID IS 200 AREA IS EMP-AREA.
    ADD FIELD NAME IS EMP-ID TYPE IS INT.
    ADD FIELD NAME IS EMP-NAME TYPE IS CHAR(40).
    ADD FIELD NAME IS SALARY TYPE IS DECIMAL(9,2).
ADD SET NAME IS DEPT-EMP OWNER IS DEPARTMENT ORDER IS LAST.
    ADD MEMBER IS EMPLOYEE.
";

    #[test]
    fn parse_schema_ddl() {
        let schema = SchemaParser::parse(SAMPLE_DDL).unwrap();
        assert_eq!(schema.name, "EMPSCHM");
        assert_eq!(schema.records.len(), 2);
        assert_eq!(schema.sets.len(), 1);
        assert_eq!(schema.areas.len(), 2);
        let emp = schema.get_record("EMPLOYEE").unwrap();
        assert_eq!(emp.fields.len(), 3);
        assert_eq!(emp.record_id, 200);
        let set = schema.get_set("DEPT-EMP").unwrap();
        assert_eq!(set.members, vec!["EMPLOYEE"]);
    }

    #[test]
    fn parse_schema_missing_name() {
        let ddl = "ADD AREA NAME IS FOO RANGE IS 1 THRU 10.";
        assert!(SchemaParser::parse(ddl).is_err());
    }

    #[test]
    fn parse_subschema() {
        let ddl = "\
ADD SUBSCHEMA NAME IS EMPSUB OF SCHEMA EMPSCHM.
ADD RECORD NAME IS EMPLOYEE.
ADD SET NAME IS DEPT-EMP.
ADD AREA NAME IS EMP-AREA.
";
        let sub = SubschemaParser::parse(ddl).unwrap();
        assert_eq!(sub.name, "EMPSUB");
        assert_eq!(sub.schema_name, "EMPSCHM");
        assert!(sub.record_names.contains("EMPLOYEE"));
        assert!(sub.set_names.contains("DEPT-EMP"));
        assert!(sub.area_names.contains("EMP-AREA"));
    }

    #[test]
    fn subschema_validate_ok() {
        let schema = SchemaParser::parse(SAMPLE_DDL).unwrap();
        let mut sub = Subschema::new("EMPSUB", "EMPSCHM");
        sub.include_record("EMPLOYEE");
        sub.include_set("DEPT-EMP");
        sub.include_area("EMP-AREA");
        assert!(sub.validate(&schema).is_ok());
    }

    #[test]
    fn subschema_validate_missing_record() {
        let schema = SchemaParser::parse(SAMPLE_DDL).unwrap();
        let mut sub = Subschema::new("BADSUB", "EMPSCHM");
        sub.include_record("NONEXISTENT");
        assert!(sub.validate(&schema).is_err());
    }

    #[test]
    fn parse_field_types() {
        let ft = SchemaParser::parse_field_type("INT").unwrap();
        assert_eq!(ft, FieldType::Int);
        let ft2 = SchemaParser::parse_field_type("CHAR(20)").unwrap();
        assert_eq!(ft2, FieldType::Char(20));
        let ft3 = SchemaParser::parse_field_type("DECIMAL(7,3)").unwrap();
        assert_eq!(ft3, FieldType::Decimal(7, 3));
        let ft4 = SchemaParser::parse_field_type("BINARY(64)").unwrap();
        assert_eq!(ft4, FieldType::Binary(64));
        let ft5 = SchemaParser::parse_field_type("LONG").unwrap();
        assert_eq!(ft5, FieldType::Long);
        assert!(SchemaParser::parse_field_type("BOGUS").is_err());
    }
}
