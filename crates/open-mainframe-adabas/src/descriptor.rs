//! ADA-102: Descriptor Engine (5 stories).
//!
//! Provides descriptor types that ADABAS uses for indexing: standard
//! descriptors, super-descriptors, sub-descriptors, phonetic descriptors,
//! and hyper-descriptors.

use crate::storage::Isn;

// ── Descriptor ─────────────────────────────────────────────────────

/// A standard descriptor: a single field marked as indexed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Descriptor {
    /// The two-character field name that is indexed.
    pub field_name: String,
    /// Whether to maintain a unique index (disallow duplicate values).
    pub unique: bool,
}

impl Descriptor {
    /// Create a new standard descriptor.
    pub fn new(field_name: impl Into<String>) -> Self {
        Self {
            field_name: field_name.into(),
            unique: false,
        }
    }

    /// Mark this descriptor as unique.
    pub fn with_unique(mut self) -> Self {
        self.unique = true;
        self
    }
}

// ── SuperDescriptor ────────────────────────────────────────────────

/// A super-descriptor composed of portions from multiple parent fields.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SuperDescriptor {
    /// The super-descriptor name (2 characters).
    pub name: String,
    /// Parent field components: (field_name, from_byte, to_byte).
    pub components: Vec<SuperComponent>,
}

/// One component of a super-descriptor, referencing a substring of a parent field.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SuperComponent {
    /// Parent field name.
    pub field_name: String,
    /// Starting byte position (1-based).
    pub from_byte: u16,
    /// Ending byte position (1-based, inclusive).
    pub to_byte: u16,
}

impl SuperDescriptor {
    /// Create a new super-descriptor with the given name.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            components: Vec::new(),
        }
    }

    /// Add a parent field component.
    pub fn add_component(
        &mut self,
        field_name: impl Into<String>,
        from_byte: u16,
        to_byte: u16,
    ) {
        self.components.push(SuperComponent {
            field_name: field_name.into(),
            from_byte,
            to_byte,
        });
    }

    /// Derive the super-descriptor value from individual field values.
    pub fn derive_value(&self, field_values: &[(&str, &[u8])]) -> Vec<u8> {
        let mut result = Vec::new();
        for comp in &self.components {
            for &(name, value) in field_values {
                if name == comp.field_name {
                    let start = (comp.from_byte as usize).saturating_sub(1);
                    let end = (comp.to_byte as usize).min(value.len());
                    if start < end {
                        result.extend_from_slice(&value[start..end]);
                    }
                    break;
                }
            }
        }
        result
    }
}

// ── SubDescriptor ──────────────────────────────────────────────────

/// A sub-descriptor defined on a substring of a single parent field.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SubDescriptor {
    /// The sub-descriptor name (2 characters).
    pub name: String,
    /// Parent field name.
    pub parent_field: String,
    /// Starting byte position (1-based).
    pub from_byte: u16,
    /// Ending byte position (1-based, inclusive).
    pub to_byte: u16,
}

impl SubDescriptor {
    /// Create a new sub-descriptor.
    pub fn new(
        name: impl Into<String>,
        parent_field: impl Into<String>,
        from_byte: u16,
        to_byte: u16,
    ) -> Self {
        Self {
            name: name.into(),
            parent_field: parent_field.into(),
            from_byte,
            to_byte,
        }
    }

    /// Extract the sub-descriptor value from a parent field value.
    pub fn extract_value(&self, parent_value: &[u8]) -> Vec<u8> {
        let start = (self.from_byte as usize).saturating_sub(1);
        let end = (self.to_byte as usize).min(parent_value.len());
        if start < end {
            parent_value[start..end].to_vec()
        } else {
            Vec::new()
        }
    }
}

// ── PhoneticDescriptor ─────────────────────────────────────────────

/// A phonetic descriptor that enables phonetic (sound-alike) matching.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhoneticDescriptor {
    /// The phonetic descriptor name.
    pub name: String,
    /// Parent field name on which phonetic matching is based.
    pub parent_field: String,
}

impl PhoneticDescriptor {
    /// Create a new phonetic descriptor.
    pub fn new(name: impl Into<String>, parent_field: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            parent_field: parent_field.into(),
        }
    }

    /// Compute a simple phonetic code from a value (Soundex-like).
    ///
    /// This is a simplified implementation: takes the first letter then
    /// maps consonants to digits, yielding a 4-character code.
    pub fn phonetic_code(value: &str) -> String {
        let upper: Vec<char> = value.to_uppercase().chars().collect();
        if upper.is_empty() {
            return String::new();
        }

        let first = upper[0];
        let mut code = String::new();
        code.push(first);

        for &ch in upper.iter().skip(1) {
            if code.len() >= 4 {
                break;
            }
            let digit = match ch {
                'B' | 'F' | 'P' | 'V' => '1',
                'C' | 'G' | 'J' | 'K' | 'Q' | 'S' | 'X' | 'Z' => '2',
                'D' | 'T' => '3',
                'L' => '4',
                'M' | 'N' => '5',
                'R' => '6',
                _ => continue,
            };
            // Skip duplicate consecutive digits.
            if code.ends_with(digit) {
                continue;
            }
            code.push(digit);
        }

        while code.len() < 4 {
            code.push('0');
        }
        code
    }
}

// ── HyperDescriptor ───────────────────────────────────────────────

/// A hyper-descriptor with user-defined logic for value generation.
///
/// In a real ADABAS system the user supplies an exit routine; here we
/// model it as a named descriptor with a closure-like callback that
/// generates index values from a record.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HyperDescriptor {
    /// The hyper-descriptor name.
    pub name: String,
    /// Parent field name(s) that the exit uses.
    pub parent_fields: Vec<String>,
    /// Description of the user-defined logic.
    pub description: String,
}

impl HyperDescriptor {
    /// Create a new hyper-descriptor.
    pub fn new(name: impl Into<String>, description: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            parent_fields: Vec::new(),
            description: description.into(),
        }
    }

    /// Add a parent field.
    pub fn add_parent_field(&mut self, field: impl Into<String>) {
        self.parent_fields.push(field.into());
    }

    /// Generate descriptor values from record fields (default: concatenate).
    pub fn generate_values(&self, field_values: &[(&str, &[u8])]) -> Vec<Vec<u8>> {
        // Default implementation: produce a single concatenated value.
        let mut combined = Vec::new();
        for parent in &self.parent_fields {
            for &(name, value) in field_values {
                if name == parent.as_str() {
                    combined.extend_from_slice(value);
                    break;
                }
            }
        }
        if combined.is_empty() {
            Vec::new()
        } else {
            vec![combined]
        }
    }
}

/// A collection of all descriptor types for a file.
#[derive(Debug, Clone, Default)]
pub struct DescriptorSet {
    /// Standard descriptors.
    pub descriptors: Vec<Descriptor>,
    /// Super-descriptors.
    pub super_descriptors: Vec<SuperDescriptor>,
    /// Sub-descriptors.
    pub sub_descriptors: Vec<SubDescriptor>,
    /// Phonetic descriptors.
    pub phonetic_descriptors: Vec<PhoneticDescriptor>,
    /// Hyper-descriptors.
    pub hyper_descriptors: Vec<HyperDescriptor>,
}

impl DescriptorSet {
    /// Create an empty descriptor set.
    pub fn new() -> Self {
        Self::default()
    }

    /// Return all descriptor names (of all types).
    pub fn all_names(&self) -> Vec<&str> {
        let mut names: Vec<&str> = Vec::new();
        for d in &self.descriptors {
            names.push(&d.field_name);
        }
        for d in &self.super_descriptors {
            names.push(&d.name);
        }
        for d in &self.sub_descriptors {
            names.push(&d.name);
        }
        for d in &self.phonetic_descriptors {
            names.push(&d.name);
        }
        for d in &self.hyper_descriptors {
            names.push(&d.name);
        }
        names
    }

    /// Update the inverted list when a record is stored.
    pub fn update_inverted_lists(
        &self,
        isn: Isn,
        field_values: &[(&str, &[u8])],
        inverted_lists: &mut std::collections::HashMap<String, crate::storage::InvertedList>,
    ) {
        // Standard descriptors
        for desc in &self.descriptors {
            for &(name, value) in field_values {
                if name == desc.field_name {
                    let key = String::from_utf8_lossy(value).to_string();
                    inverted_lists
                        .entry(desc.field_name.clone())
                        .or_default()
                        .insert(key, isn);
                    break;
                }
            }
        }

        // Super-descriptors
        for sd in &self.super_descriptors {
            let val = sd.derive_value(field_values);
            if !val.is_empty() {
                let key = String::from_utf8_lossy(&val).to_string();
                inverted_lists
                    .entry(sd.name.clone())
                    .or_default()
                    .insert(key, isn);
            }
        }

        // Sub-descriptors
        for sub in &self.sub_descriptors {
            for &(name, value) in field_values {
                if name == sub.parent_field {
                    let val = sub.extract_value(value);
                    if !val.is_empty() {
                        let key = String::from_utf8_lossy(&val).to_string();
                        inverted_lists
                            .entry(sub.name.clone())
                            .or_default()
                            .insert(key, isn);
                    }
                    break;
                }
            }
        }

        // Phonetic descriptors
        for pd in &self.phonetic_descriptors {
            for &(name, value) in field_values {
                if name == pd.parent_field {
                    let text = String::from_utf8_lossy(value).to_string();
                    let code = PhoneticDescriptor::phonetic_code(&text);
                    if !code.is_empty() {
                        inverted_lists
                            .entry(pd.name.clone())
                            .or_default()
                            .insert(code, isn);
                    }
                    break;
                }
            }
        }
    }

    /// Remove entries from inverted lists when a record is deleted.
    pub fn remove_from_inverted_lists(
        &self,
        isn: Isn,
        field_values: &[(&str, &[u8])],
        inverted_lists: &mut std::collections::HashMap<String, crate::storage::InvertedList>,
    ) {
        for desc in &self.descriptors {
            for &(name, value) in field_values {
                if name == desc.field_name {
                    let key = String::from_utf8_lossy(value).to_string();
                    if let Some(il) = inverted_lists.get_mut(&desc.field_name) {
                        il.remove(&key, isn);
                    }
                    break;
                }
            }
        }
    }
}

// ── Tests ──────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn descriptor_basic() {
        let d = Descriptor::new("AA");
        assert_eq!(d.field_name, "AA");
        assert!(!d.unique);
    }

    #[test]
    fn descriptor_unique() {
        let d = Descriptor::new("AB").with_unique();
        assert!(d.unique);
    }

    #[test]
    fn super_descriptor_derive() {
        let mut sd = SuperDescriptor::new("S1");
        sd.add_component("AA", 1, 3);
        sd.add_component("AB", 1, 2);
        let values: Vec<(&str, &[u8])> = vec![("AA", b"ABCDEF"), ("AB", b"XY")];
        let result = sd.derive_value(&values);
        assert_eq!(result, b"ABCXY");
    }

    #[test]
    fn super_descriptor_empty_field() {
        let mut sd = SuperDescriptor::new("S2");
        sd.add_component("ZZ", 1, 3);
        let values: Vec<(&str, &[u8])> = vec![("AA", b"ABCDEF")];
        let result = sd.derive_value(&values);
        assert!(result.is_empty());
    }

    #[test]
    fn sub_descriptor_extract() {
        let sub = SubDescriptor::new("T1", "AA", 2, 4);
        let value = b"ABCDEF";
        let result = sub.extract_value(value);
        assert_eq!(result, b"BCD");
    }

    #[test]
    fn sub_descriptor_out_of_range() {
        let sub = SubDescriptor::new("T2", "AA", 10, 20);
        let value = b"AB";
        let result = sub.extract_value(value);
        assert!(result.is_empty());
    }

    #[test]
    fn phonetic_code_smith() {
        let code = PhoneticDescriptor::phonetic_code("Smith");
        assert_eq!(code, "S530");
    }

    #[test]
    fn phonetic_code_smythe() {
        let code = PhoneticDescriptor::phonetic_code("Smythe");
        // S530 — same as Smith (phonetic match).
        assert_eq!(code, "S530");
    }

    #[test]
    fn phonetic_code_empty() {
        let code = PhoneticDescriptor::phonetic_code("");
        assert!(code.is_empty());
    }

    #[test]
    fn hyper_descriptor_basic() {
        let mut hd = HyperDescriptor::new("H1", "concatenate first+last name");
        hd.add_parent_field("AA");
        hd.add_parent_field("AB");
        let values: Vec<(&str, &[u8])> = vec![("AA", b"JOHN"), ("AB", b"DOE")];
        let result = hd.generate_values(&values);
        assert_eq!(result, vec![b"JOHNDOE".to_vec()]);
    }

    #[test]
    fn descriptor_set_all_names() {
        let mut ds = DescriptorSet::new();
        ds.descriptors.push(Descriptor::new("AA"));
        ds.super_descriptors.push(SuperDescriptor::new("S1"));
        ds.sub_descriptors
            .push(SubDescriptor::new("T1", "AB", 1, 3));
        ds.phonetic_descriptors
            .push(PhoneticDescriptor::new("P1", "AA"));
        ds.hyper_descriptors
            .push(HyperDescriptor::new("H1", "test"));
        let names = ds.all_names();
        assert_eq!(names.len(), 5);
    }

    #[test]
    fn descriptor_set_update_inverted_lists() {
        let mut ds = DescriptorSet::new();
        ds.descriptors.push(Descriptor::new("AA"));
        let mut lists = std::collections::HashMap::new();
        let values: Vec<(&str, &[u8])> = vec![("AA", b"SMITH")];
        ds.update_inverted_lists(1, &values, &mut lists);
        ds.update_inverted_lists(2, &values, &mut lists);
        let isns = lists.get("AA").unwrap().search("SMITH");
        assert_eq!(isns, vec![1, 2]);
    }
}
