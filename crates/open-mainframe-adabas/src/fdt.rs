//! ADA-101: FDT & Field System (5 stories).
//!
//! Provides the Field Definition Table (FDT), individual field definitions,
//! field types, group fields, and multiple-value / periodic-group support.

use std::collections::HashMap;

use crate::AdabasError;

// ── FieldType ──────────────────────────────────────────────────────

/// The data type of an ADABAS field.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FieldType {
    /// Alphanumeric (left-justified, space-padded).
    Alpha,
    /// Numeric (unpacked decimal, right-justified, zero-padded).
    Numeric,
    /// Packed decimal (BCD).
    Packed,
    /// Unpacked decimal.
    Unpacked,
    /// Binary (fixed-length raw bytes).
    Binary,
    /// Wide (Unicode / UTF-16).
    Wide,
    /// Fixed-point (binary integer, 2 or 4 bytes).
    FixedPoint,
    /// Floating-point (single or double precision).
    Float,
}

impl std::fmt::Display for FieldType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Alpha => "A",
            Self::Numeric => "N",
            Self::Packed => "P",
            Self::Unpacked => "U",
            Self::Binary => "B",
            Self::Wide => "W",
            Self::FixedPoint => "F",
            Self::Float => "G",
        };
        f.write_str(s)
    }
}

// ── FieldDef ───────────────────────────────────────────────────────

/// Options that can be applied to a field in the FDT.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FieldOption {
    /// DE — Descriptor (create inverted list).
    Descriptor,
    /// UQ — Unique descriptor (no duplicate values).
    Unique,
    /// NU — Null suppression (don't index null values).
    NullSuppression,
    /// FI — Fixed storage (always stored at defined length).
    FixedStorage,
    /// MU — Multiple-value field.
    MultipleValue,
    /// PE — Periodic group.
    PeriodicGroup,
    /// LA — Long alpha field (LOB-like).
    LongAlpha,
    /// LB — Large object field.
    LargeObject,
    /// NB — No blank compression.
    NoBlankCompression,
    /// NC — No character compression.
    NoCharCompression,
    /// NN — Not null constraint.
    NotNull,
    /// HF — High-order first (big-endian) for binary.
    HighOrderFirst,
    /// XI — Exclude from replication.
    ExcludeReplication,
}

/// A single field definition in the FDT.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldDef {
    /// Two-character short name (e.g. "AA", "AB").
    pub name: String,
    /// Level number (1 = top-level, 2+ = within a group).
    pub level: u8,
    /// Field data type.
    pub field_type: FieldType,
    /// Maximum byte length.
    pub length: u16,
    /// Display format override (optional).
    pub format: Option<String>,
    /// Whether this field is a descriptor (indexed).
    pub is_descriptor: bool,
    /// Whether this is a multiple-value (MU) field.
    pub is_multiple_value: bool,
    /// Whether this field belongs to a periodic group.
    pub is_periodic: bool,
    /// Set of field options applied to this field.
    pub options: Vec<FieldOption>,
}

impl FieldDef {
    /// Create a new field definition.
    pub fn new(name: impl Into<String>, level: u8, field_type: FieldType, length: u16) -> Self {
        Self {
            name: name.into(),
            level,
            field_type,
            length,
            format: None,
            is_descriptor: false,
            is_multiple_value: false,
            is_periodic: false,
            options: Vec::new(),
        }
    }

    /// Mark this field as a descriptor.
    pub fn with_descriptor(mut self) -> Self {
        self.is_descriptor = true;
        self
    }

    /// Mark this field as a multiple-value field.
    pub fn with_multiple_value(mut self) -> Self {
        self.is_multiple_value = true;
        self
    }

    /// Mark this field as belonging to a periodic group.
    pub fn with_periodic(mut self) -> Self {
        self.is_periodic = true;
        self
    }

    /// Set the display format.
    pub fn with_format(mut self, fmt: impl Into<String>) -> Self {
        self.format = Some(fmt.into());
        self
    }

    /// Add a field option.
    pub fn with_option(mut self, option: FieldOption) -> Self {
        if !self.options.contains(&option) {
            self.options.push(option);
        }
        self
    }

    /// Check whether a specific option is set.
    pub fn has_option(&self, option: FieldOption) -> bool {
        self.options.contains(&option)
    }
}

// ── GroupField ──────────────────────────────────────────────────────

/// A group of fields (or a periodic group).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GroupField {
    /// Two-character group name.
    pub name: String,
    /// Level of the group definition.
    pub level: u8,
    /// Whether this is a periodic group (PE).
    pub is_periodic: bool,
    /// Member field names (in order).
    pub members: Vec<String>,
}

impl GroupField {
    /// Create a new group field.
    pub fn new(name: impl Into<String>, level: u8) -> Self {
        Self {
            name: name.into(),
            level,
            is_periodic: false,
            members: Vec::new(),
        }
    }

    /// Mark this group as periodic.
    pub fn with_periodic(mut self) -> Self {
        self.is_periodic = true;
        self
    }

    /// Add a member field to the group.
    pub fn add_member(&mut self, field_name: impl Into<String>) {
        self.members.push(field_name.into());
    }
}

// ── MultipleValueField ─────────────────────────────────────────────

/// Represents a multiple-value (MU) or periodic-group (PE) occurrence.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MultipleValueField {
    /// The field name this MU/PE applies to.
    pub field_name: String,
    /// Whether this is a periodic group (PE) vs simple MU.
    pub is_periodic: bool,
    /// Maximum number of occurrences (0 = unlimited).
    pub max_occurrences: u16,
    /// Current values stored as ordered list of byte vectors.
    pub values: Vec<Vec<u8>>,
}

impl MultipleValueField {
    /// Create a new multiple-value field.
    pub fn new(field_name: impl Into<String>, is_periodic: bool, max_occurrences: u16) -> Self {
        Self {
            field_name: field_name.into(),
            is_periodic,
            max_occurrences,
            values: Vec::new(),
        }
    }

    /// Add a value occurrence.
    pub fn add_value(&mut self, value: Vec<u8>) -> Result<(), AdabasError> {
        if self.max_occurrences > 0 && self.values.len() >= self.max_occurrences as usize {
            return Err(AdabasError::MaxOccurrencesExceeded {
                field: self.field_name.clone(),
                max: self.max_occurrences,
            });
        }
        self.values.push(value);
        Ok(())
    }

    /// Return the count of current occurrences.
    pub fn occurrence_count(&self) -> usize {
        self.values.len()
    }
}

// ── Fdt ────────────────────────────────────────────────────────────

/// Field Definition Table: the schema of an ADABAS file.
#[derive(Debug, Clone)]
pub struct Fdt {
    /// Ordered list of field definitions.
    fields: Vec<FieldDef>,
    /// Fast lookup by field name.
    index: HashMap<String, usize>,
    /// Group definitions.
    groups: Vec<GroupField>,
}

impl Fdt {
    /// Create an empty FDT.
    pub fn new() -> Self {
        Self {
            fields: Vec::new(),
            index: HashMap::new(),
            groups: Vec::new(),
        }
    }

    /// Add a field definition to the FDT.
    pub fn add_field(&mut self, field: FieldDef) -> Result<(), AdabasError> {
        if self.index.contains_key(&field.name) {
            return Err(AdabasError::DuplicateField {
                name: field.name.clone(),
            });
        }
        let pos = self.fields.len();
        self.index.insert(field.name.clone(), pos);
        self.fields.push(field);
        Ok(())
    }

    /// Look up a field by name.
    pub fn get_field(&self, name: &str) -> Option<&FieldDef> {
        self.index.get(name).map(|&i| &self.fields[i])
    }

    /// Return all field definitions in order.
    pub fn fields(&self) -> &[FieldDef] {
        &self.fields
    }

    /// Add a group definition.
    pub fn add_group(&mut self, group: GroupField) {
        self.groups.push(group);
    }

    /// Get all group definitions.
    pub fn groups(&self) -> &[GroupField] {
        &self.groups
    }

    /// Return the number of fields.
    pub fn field_count(&self) -> usize {
        self.fields.len()
    }

    /// Return descriptor field names.
    pub fn descriptor_fields(&self) -> Vec<&FieldDef> {
        self.fields.iter().filter(|f| f.is_descriptor).collect()
    }
}

impl Default for Fdt {
    fn default() -> Self {
        Self::new()
    }
}

// ── Tests ──────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn field_def_creation() {
        let f = FieldDef::new("AA", 1, FieldType::Alpha, 20);
        assert_eq!(f.name, "AA");
        assert_eq!(f.level, 1);
        assert_eq!(f.field_type, FieldType::Alpha);
        assert_eq!(f.length, 20);
        assert!(!f.is_descriptor);
    }

    #[test]
    fn field_def_builders() {
        let f = FieldDef::new("AB", 1, FieldType::Numeric, 8)
            .with_descriptor()
            .with_multiple_value()
            .with_format("N8");
        assert!(f.is_descriptor);
        assert!(f.is_multiple_value);
        assert_eq!(f.format, Some("N8".to_string()));
    }

    #[test]
    fn fdt_add_and_lookup() {
        let mut fdt = Fdt::new();
        fdt.add_field(FieldDef::new("AA", 1, FieldType::Alpha, 20))
            .unwrap();
        fdt.add_field(FieldDef::new("AB", 1, FieldType::Numeric, 8).with_descriptor())
            .unwrap();
        assert_eq!(fdt.field_count(), 2);
        assert_eq!(fdt.get_field("AA").unwrap().length, 20);
        assert!(fdt.get_field("ZZ").is_none());
    }

    #[test]
    fn fdt_duplicate_field() {
        let mut fdt = Fdt::new();
        fdt.add_field(FieldDef::new("AA", 1, FieldType::Alpha, 20))
            .unwrap();
        let res = fdt.add_field(FieldDef::new("AA", 1, FieldType::Binary, 4));
        assert!(res.is_err());
    }

    #[test]
    fn fdt_descriptor_fields() {
        let mut fdt = Fdt::new();
        fdt.add_field(FieldDef::new("AA", 1, FieldType::Alpha, 20))
            .unwrap();
        fdt.add_field(FieldDef::new("AB", 1, FieldType::Numeric, 8).with_descriptor())
            .unwrap();
        let descs = fdt.descriptor_fields();
        assert_eq!(descs.len(), 1);
        assert_eq!(descs[0].name, "AB");
    }

    #[test]
    fn group_field_basic() {
        let mut grp = GroupField::new("GA", 1);
        grp.add_member("AA");
        grp.add_member("AB");
        assert_eq!(grp.members.len(), 2);
        assert!(!grp.is_periodic);
    }

    #[test]
    fn group_field_periodic() {
        let grp = GroupField::new("GB", 1).with_periodic();
        assert!(grp.is_periodic);
    }

    #[test]
    fn multiple_value_field_add() {
        let mut mv = MultipleValueField::new("AC", false, 3);
        mv.add_value(b"one".to_vec()).unwrap();
        mv.add_value(b"two".to_vec()).unwrap();
        mv.add_value(b"three".to_vec()).unwrap();
        assert_eq!(mv.occurrence_count(), 3);
        let err = mv.add_value(b"four".to_vec());
        assert!(err.is_err());
    }

    #[test]
    fn multiple_value_unlimited() {
        let mut mv = MultipleValueField::new("AD", false, 0);
        for i in 0..100 {
            mv.add_value(vec![i]).unwrap();
        }
        assert_eq!(mv.occurrence_count(), 100);
    }

    #[test]
    fn field_type_display() {
        assert_eq!(FieldType::Alpha.to_string(), "A");
        assert_eq!(FieldType::Packed.to_string(), "P");
        assert_eq!(FieldType::Wide.to_string(), "W");
    }

    #[test]
    fn fdt_groups() {
        let mut fdt = Fdt::new();
        fdt.add_group(GroupField::new("GA", 1));
        assert_eq!(fdt.groups().len(), 1);
    }
}
