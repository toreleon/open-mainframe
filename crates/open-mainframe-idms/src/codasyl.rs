//! IDMS-100: CODASYL Data Model (5 stories).
//!
//! The CODASYL network data model organises data into record types linked
//! by owner-member set relationships.  Each set type defines a one-to-many
//! relationship between an owner record type and one or more member record
//! types.  Records reside in named storage areas.

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Location mode
// ---------------------------------------------------------------------------

/// How records of this type are physically placed in the database.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum LocationMode {
    /// Hashed placement by CALC key field(s).
    Calc {
        /// Field names used for the CALC key.
        fields: Vec<String>,
        /// Duplicate handling.
        duplicates: DuplicateOption,
    },
    /// Clustered near the owner record via a named set.
    Via {
        /// Set name for clustering.
        set_name: String,
    },
    /// Direct placement by database key.
    Direct,
}

/// Handling of duplicate CALC key values or sorted set keys.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum DuplicateOption {
    /// Duplicates are not allowed.
    NotAllowed,
    /// Duplicates are placed first.
    First,
    /// Duplicates are placed last.
    Last,
}

// ---------------------------------------------------------------------------
//  Set membership class
// ---------------------------------------------------------------------------

/// Set membership class governing connect/disconnect rules.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum SetMembership {
    /// Must belong to a set; automatically connected on STORE.
    MandatoryAutomatic,
    /// Must belong to a set; explicit CONNECT required.
    MandatoryManual,
    /// May belong to a set; automatically connected on STORE.
    OptionalAutomatic,
    /// May belong to a set; explicit CONNECT required.
    OptionalManual,
}

// ---------------------------------------------------------------------------
//  Set implementation mode
// ---------------------------------------------------------------------------

/// Physical implementation of a set (pointer structure).
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum SetMode {
    /// Chain (linked list) with optional LINKED TO PRIOR.
    Chain {
        /// Whether prior pointers are maintained.
        linked_to_prior: bool,
    },
    /// B-tree index implementation.
    Index,
}

// ---------------------------------------------------------------------------
//  Field definition
// ---------------------------------------------------------------------------

/// Data type of a record field.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum FieldType {
    /// Fixed-length character string.
    Char(usize),
    /// Signed 32-bit integer.
    Int,
    /// Signed 64-bit integer.
    Long,
    /// Decimal with precision and scale.
    Decimal(u8, u8),
    /// Binary blob.
    Binary(usize),
}

/// A single field in a record type.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct FieldDef {
    /// Field name.
    pub name: String,
    /// Data type.
    pub field_type: FieldType,
}

// ---------------------------------------------------------------------------
//  Record Type
// ---------------------------------------------------------------------------

/// A CODASYL record type -- the fundamental unit of stored data.
///
/// Each record type has a unique numeric ID, a name, a list of field
/// definitions, and an optional CALC key used for hashed placement.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct RecordType {
    /// Numeric record ID (unique within schema).
    pub record_id: u32,
    /// Record type name (e.g., `EMPLOYEE`).
    pub name: String,
    /// Ordered list of fields.
    pub fields: Vec<FieldDef>,
    /// Optional CALC key field name for hashed placement (legacy convenience).
    pub calc_key: Option<String>,
    /// Location mode (CALC, VIA, DIRECT).
    pub location_mode: Option<LocationMode>,
    /// Name of the area where this record is stored.
    pub area_name: String,
}

impl RecordType {
    /// Create a new record type with no fields.
    pub fn new(record_id: u32, name: &str, area: &str) -> Self {
        Self {
            record_id,
            name: name.to_uppercase(),
            fields: Vec::new(),
            calc_key: None,
            location_mode: None,
            area_name: area.to_uppercase(),
        }
    }

    /// Add a field to this record type.
    pub fn add_field(&mut self, name: &str, field_type: FieldType) {
        self.fields.push(FieldDef {
            name: name.to_uppercase(),
            field_type,
        });
    }

    /// Look up a field definition by name (case-insensitive).
    pub fn find_field(&self, name: &str) -> Option<&FieldDef> {
        let upper = name.to_uppercase();
        self.fields.iter().find(|f| f.name == upper)
    }
}

// ---------------------------------------------------------------------------
//  Set ordering
// ---------------------------------------------------------------------------

/// Order in which new member records are inserted into a set occurrence.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum SetOrder {
    /// Insert as first member.
    First,
    /// Insert as last member.
    Last,
    /// Insert after current of set.
    Next,
    /// Insert before current of set.
    Prior,
    /// Insert in sorted order by sort key.
    Sorted,
}

// ---------------------------------------------------------------------------
//  Set Type
// ---------------------------------------------------------------------------

/// A CODASYL set type -- an owner-member relationship.
///
/// A set occurrence is a single owner record instance together with zero or
/// more member record instances linked in the specified order.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct SetType {
    /// Set type name (e.g., `DEPT-EMPLOYEE`).
    pub name: String,
    /// Owner record type name.
    pub owner: String,
    /// Member record type names.
    pub members: Vec<String>,
    /// Insertion order.
    pub order: SetOrder,
    /// Optional sort key field name (only meaningful when order == Sorted).
    pub sort_key: Option<String>,
    /// Set membership class.
    pub membership: Option<SetMembership>,
    /// Physical implementation mode (CHAIN or INDEX).
    pub mode: Option<SetMode>,
    /// Duplicate key handling for sorted sets.
    pub duplicates: Option<DuplicateOption>,
}

impl SetType {
    /// Create a new set type.
    pub fn new(name: &str, owner: &str, order: SetOrder) -> Self {
        Self {
            name: name.to_uppercase(),
            owner: owner.to_uppercase(),
            members: Vec::new(),
            order,
            sort_key: None,
            membership: None,
            mode: None,
            duplicates: None,
        }
    }

    /// Add a member record type.
    pub fn add_member(&mut self, member: &str) {
        self.members.push(member.to_uppercase());
    }
}

// ---------------------------------------------------------------------------
//  Area definition
// ---------------------------------------------------------------------------

/// A storage area -- a named region of the database consisting of a
/// contiguous range of pages.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct AreaDef {
    /// Area name (e.g., `EMP-AREA`).
    pub name: String,
    /// First page number in this area.
    pub start_page: u32,
    /// Last page number in this area.
    pub end_page: u32,
}

impl AreaDef {
    /// Create a new area definition.
    pub fn new(name: &str, start_page: u32, end_page: u32) -> Self {
        Self {
            name: name.to_uppercase(),
            start_page,
            end_page,
        }
    }

    /// Number of pages in this area.
    pub fn page_count(&self) -> u32 {
        self.end_page.saturating_sub(self.start_page) + 1
    }
}

// ---------------------------------------------------------------------------
//  Schema
// ---------------------------------------------------------------------------

/// A CODASYL schema -- the complete logical description of an IDMS database.
///
/// Contains all record types, set types, and area definitions.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct CodasylSchema {
    /// Schema name.
    pub name: String,
    /// Record types keyed by name.
    pub records: HashMap<String, RecordType>,
    /// Set types keyed by name.
    pub sets: HashMap<String, SetType>,
    /// Area definitions keyed by name.
    pub areas: HashMap<String, AreaDef>,
}

impl CodasylSchema {
    /// Create a new empty schema.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            records: HashMap::new(),
            sets: HashMap::new(),
            areas: HashMap::new(),
        }
    }

    /// Add a record type.  Returns an error if the name already exists.
    pub fn add_record(&mut self, record: RecordType) -> Result<(), IdmsError> {
        if self.records.contains_key(&record.name) {
            return Err(IdmsError::DuplicateRecord(record.name.clone()));
        }
        self.records.insert(record.name.clone(), record);
        Ok(())
    }

    /// Add a set type.  Returns an error if the name already exists.
    pub fn add_set(&mut self, set: SetType) -> Result<(), IdmsError> {
        if self.sets.contains_key(&set.name) {
            return Err(IdmsError::DuplicateSet(set.name.clone()));
        }
        self.sets.insert(set.name.clone(), set);
        Ok(())
    }

    /// Add an area definition.  Returns an error if the name already exists.
    pub fn add_area(&mut self, area: AreaDef) -> Result<(), IdmsError> {
        if self.areas.contains_key(&area.name) {
            return Err(IdmsError::DuplicateArea(area.name.clone()));
        }
        self.areas.insert(area.name.clone(), area);
        Ok(())
    }

    /// Look up a record type by name (case-insensitive).
    pub fn get_record(&self, name: &str) -> Option<&RecordType> {
        self.records.get(&name.to_uppercase())
    }

    /// Look up a set type by name (case-insensitive).
    pub fn get_set(&self, name: &str) -> Option<&SetType> {
        self.sets.get(&name.to_uppercase())
    }

    /// Look up an area definition by name (case-insensitive).
    pub fn get_area(&self, name: &str) -> Option<&AreaDef> {
        self.areas.get(&name.to_uppercase())
    }
}

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors produced by the CODASYL data-model layer.
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum IdmsError {
    /// Duplicate record type name.
    #[error("duplicate record type: {0}")]
    DuplicateRecord(String),
    /// Duplicate set type name.
    #[error("duplicate set type: {0}")]
    DuplicateSet(String),
    /// Duplicate area name.
    #[error("duplicate area: {0}")]
    DuplicateArea(String),
    /// Record type not found.
    #[error("record type not found: {0}")]
    RecordNotFound(String),
    /// Set type not found.
    #[error("set type not found: {0}")]
    SetNotFound(String),
    /// Area not found.
    #[error("area not found: {0}")]
    AreaNotFound(String),
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn record_type_fields() {
        let mut rec = RecordType::new(100, "EMPLOYEE", "EMP-AREA");
        rec.add_field("EMP-ID", FieldType::Int);
        rec.add_field("EMP-NAME", FieldType::Char(30));
        assert_eq!(rec.fields.len(), 2);
        assert!(rec.find_field("emp-id").is_some());
        assert!(rec.find_field("NONEXISTENT").is_none());
    }

    #[test]
    fn set_type_members() {
        let mut set = SetType::new("DEPT-EMPLOYEE", "DEPARTMENT", SetOrder::Last);
        set.add_member("EMPLOYEE");
        assert_eq!(set.members, vec!["EMPLOYEE"]);
        assert_eq!(set.owner, "DEPARTMENT");
    }

    #[test]
    fn area_page_count() {
        let area = AreaDef::new("EMP-AREA", 1, 100);
        assert_eq!(area.page_count(), 100);
    }

    #[test]
    fn schema_add_and_lookup() {
        let mut schema = CodasylSchema::new("EMPSCHM");
        let rec = RecordType::new(100, "EMPLOYEE", "EMP-AREA");
        schema.add_record(rec).unwrap();
        assert!(schema.get_record("employee").is_some());
        assert!(schema.get_record("NONEXISTENT").is_none());
    }

    #[test]
    fn schema_duplicate_record_error() {
        let mut schema = CodasylSchema::new("EMPSCHM");
        let rec1 = RecordType::new(100, "EMPLOYEE", "EMP-AREA");
        let rec2 = RecordType::new(101, "EMPLOYEE", "EMP-AREA");
        schema.add_record(rec1).unwrap();
        let err = schema.add_record(rec2).unwrap_err();
        assert!(matches!(err, IdmsError::DuplicateRecord(_)));
    }

    #[test]
    fn schema_sets_and_areas() {
        let mut schema = CodasylSchema::new("EMPSCHM");

        let mut set = SetType::new("DEPT-EMP", "DEPARTMENT", SetOrder::Sorted);
        set.sort_key = Some("EMP-NAME".to_string());
        set.add_member("EMPLOYEE");
        schema.add_set(set).unwrap();
        assert!(schema.get_set("dept-emp").is_some());

        let area = AreaDef::new("EMP-AREA", 1, 500);
        schema.add_area(area).unwrap();
        assert!(schema.get_area("emp-area").is_some());
    }

    #[test]
    fn schema_duplicate_set_error() {
        let mut schema = CodasylSchema::new("EMPSCHM");
        let set1 = SetType::new("DEPT-EMP", "DEPARTMENT", SetOrder::Last);
        let set2 = SetType::new("DEPT-EMP", "DEPARTMENT", SetOrder::First);
        schema.add_set(set1).unwrap();
        assert!(schema.add_set(set2).is_err());
    }

    #[test]
    fn schema_duplicate_area_error() {
        let mut schema = CodasylSchema::new("EMPSCHM");
        let a1 = AreaDef::new("EMP-AREA", 1, 100);
        let a2 = AreaDef::new("EMP-AREA", 101, 200);
        schema.add_area(a1).unwrap();
        assert!(schema.add_area(a2).is_err());
    }

    #[test]
    fn set_order_variants() {
        assert_eq!(SetOrder::First, SetOrder::First);
        assert_ne!(SetOrder::First, SetOrder::Last);
        assert_ne!(SetOrder::Next, SetOrder::Prior);
    }

    #[test]
    fn field_type_variants() {
        let ft = FieldType::Decimal(9, 2);
        assert_eq!(ft, FieldType::Decimal(9, 2));
        assert_ne!(ft, FieldType::Int);
    }
}
