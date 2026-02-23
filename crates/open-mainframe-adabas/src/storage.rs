//! ADA-100: Inverted-List Storage Engine (6 stories).
//!
//! Provides the core storage abstraction for ADABAS: Data Storage for
//! compressed records, Associator Storage for inverted lists, and the
//! Address Converter that maps ISNs to RABNs.

use std::collections::{BTreeMap, HashMap};

use crate::AdabasError;

// ── Types ──────────────────────────────────────────────────────────

/// Internal Sequence Number: unique identifier for a record within a file.
pub type Isn = u64;

/// Relative ADABAS Block Number: physical block address.
pub type Rabn = u64;

// ── AdabasFile ─────────────────────────────────────────────────────

/// An ADABAS file identified by a file number, with associated storage areas.
#[derive(Debug, Clone)]
pub struct AdabasFile {
    /// The file number (1-5000).
    pub file_number: u16,
    /// Human-readable file name.
    pub name: String,
    /// Data Storage area for this file.
    pub data_storage: DataStorage,
    /// Associator Storage area for this file.
    pub associator: AssociatorStorage,
    /// Next ISN to assign.
    next_isn: Isn,
}

impl AdabasFile {
    /// Create a new ADABAS file with the given number and name.
    pub fn new(file_number: u16, name: impl Into<String>) -> Self {
        Self {
            file_number,
            name: name.into(),
            data_storage: DataStorage::new(),
            associator: AssociatorStorage::new(),
            next_isn: 1,
        }
    }

    /// Allocate and return the next ISN for this file.
    pub fn allocate_isn(&mut self) -> Isn {
        let isn = self.next_isn;
        self.next_isn += 1;
        isn
    }

    /// Return the current highest allocated ISN.
    pub fn top_isn(&self) -> Isn {
        self.next_isn.saturating_sub(1)
    }

    /// Store a compressed record and map the ISN.
    pub fn store_record(&mut self, isn: Isn, data: Vec<u8>) -> Result<Rabn, AdabasError> {
        let rabn = self.data_storage.store(data)?;
        self.associator.address_converter.insert(isn, rabn);
        Ok(rabn)
    }

    /// Read a record by ISN.
    pub fn read_record(&self, isn: Isn) -> Result<&[u8], AdabasError> {
        let rabn = self
            .associator
            .address_converter
            .lookup(isn)
            .ok_or(AdabasError::IsnNotFound { isn })?;
        self.data_storage
            .read(rabn)
            .ok_or(AdabasError::RabnNotFound { rabn })
    }

    /// Delete a record by ISN.
    pub fn delete_record(&mut self, isn: Isn) -> Result<Vec<u8>, AdabasError> {
        let rabn = self
            .associator
            .address_converter
            .remove(isn)
            .ok_or(AdabasError::IsnNotFound { isn })?;
        self.data_storage
            .remove(rabn)
            .ok_or(AdabasError::RabnNotFound { rabn })
    }

    /// Update an existing record by ISN.
    pub fn update_record(&mut self, isn: Isn, data: Vec<u8>) -> Result<(), AdabasError> {
        let rabn = self
            .associator
            .address_converter
            .lookup(isn)
            .ok_or(AdabasError::IsnNotFound { isn })?;
        self.data_storage.update(rabn, data)
    }
}

// ── DataStorage ────────────────────────────────────────────────────

/// Stores compressed records, keyed by RABN.
#[derive(Debug, Clone)]
pub struct DataStorage {
    blocks: HashMap<Rabn, Vec<u8>>,
    next_rabn: Rabn,
}

impl DataStorage {
    /// Create an empty Data Storage area.
    pub fn new() -> Self {
        Self {
            blocks: HashMap::new(),
            next_rabn: 1,
        }
    }

    /// Store a compressed record, returning the assigned RABN.
    pub fn store(&mut self, data: Vec<u8>) -> Result<Rabn, AdabasError> {
        let rabn = self.next_rabn;
        self.next_rabn += 1;
        self.blocks.insert(rabn, data);
        Ok(rabn)
    }

    /// Read a record by RABN.
    pub fn read(&self, rabn: Rabn) -> Option<&[u8]> {
        self.blocks.get(&rabn).map(|v| v.as_slice())
    }

    /// Remove a record by RABN, returning its data.
    pub fn remove(&mut self, rabn: Rabn) -> Option<Vec<u8>> {
        self.blocks.remove(&rabn)
    }

    /// Update a record at a given RABN.
    pub fn update(&mut self, rabn: Rabn, data: Vec<u8>) -> Result<(), AdabasError> {
        if !self.blocks.contains_key(&rabn) {
            return Err(AdabasError::RabnNotFound { rabn });
        }
        self.blocks.insert(rabn, data);
        Ok(())
    }

    /// Return the number of stored blocks.
    pub fn block_count(&self) -> usize {
        self.blocks.len()
    }
}

impl Default for DataStorage {
    fn default() -> Self {
        Self::new()
    }
}

// ── AssociatorStorage ──────────────────────────────────────────────

/// Stores inverted lists and the address converter for a file.
#[derive(Debug, Clone)]
pub struct AssociatorStorage {
    /// Maps ISN -> RABN.
    pub address_converter: AddressConverter,
    /// Inverted lists keyed by descriptor name.
    pub inverted_lists: HashMap<String, InvertedList>,
}

impl AssociatorStorage {
    /// Create an empty Associator Storage.
    pub fn new() -> Self {
        Self {
            address_converter: AddressConverter::new(),
            inverted_lists: HashMap::new(),
        }
    }

    /// Get or create the inverted list for a descriptor.
    pub fn inverted_list_for(&mut self, descriptor_name: &str) -> &mut InvertedList {
        self.inverted_lists
            .entry(descriptor_name.to_string())
            .or_default()
    }

    /// Look up ISNs for a given descriptor value.
    pub fn search(&self, descriptor_name: &str, value: &str) -> Vec<Isn> {
        self.inverted_lists
            .get(descriptor_name)
            .map(|il| il.search(value))
            .unwrap_or_default()
    }
}

impl Default for AssociatorStorage {
    fn default() -> Self {
        Self::new()
    }
}

// ── AddressConverter ───────────────────────────────────────────────

/// Maps ISN (Internal Sequence Number) to RABN (Relative ADABAS Block Number).
#[derive(Debug, Clone)]
pub struct AddressConverter {
    map: HashMap<Isn, Rabn>,
}

impl AddressConverter {
    /// Create an empty address converter.
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    /// Insert an ISN->RABN mapping.
    pub fn insert(&mut self, isn: Isn, rabn: Rabn) {
        self.map.insert(isn, rabn);
    }

    /// Look up the RABN for an ISN.
    pub fn lookup(&self, isn: Isn) -> Option<Rabn> {
        self.map.get(&isn).copied()
    }

    /// Remove the mapping for an ISN, returning the RABN if it existed.
    pub fn remove(&mut self, isn: Isn) -> Option<Rabn> {
        self.map.remove(&isn)
    }

    /// Return how many mappings exist.
    pub fn len(&self) -> usize {
        self.map.len()
    }

    /// Whether the address converter is empty.
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

impl Default for AddressConverter {
    fn default() -> Self {
        Self::new()
    }
}

// ── InvertedList ───────────────────────────────────────────────────

/// An inverted-list index structure: maps descriptor values to sorted ISN lists.
#[derive(Debug, Clone)]
pub struct InvertedList {
    entries: BTreeMap<String, Vec<Isn>>,
}

impl InvertedList {
    /// Create a new empty inverted list.
    pub fn new() -> Self {
        Self {
            entries: BTreeMap::new(),
        }
    }

    /// Add an ISN for the given descriptor value.
    pub fn insert(&mut self, value: impl Into<String>, isn: Isn) {
        let list = self.entries.entry(value.into()).or_default();
        if let Err(pos) = list.binary_search(&isn) {
            list.insert(pos, isn);
        }
    }

    /// Remove an ISN for the given descriptor value.
    pub fn remove(&mut self, value: &str, isn: Isn) {
        if let Some(list) = self.entries.get_mut(value) {
            if let Ok(pos) = list.binary_search(&isn) {
                list.remove(pos);
            }
            if list.is_empty() {
                self.entries.remove(value);
            }
        }
    }

    /// Search for all ISNs matching a given value (exact match).
    pub fn search(&self, value: &str) -> Vec<Isn> {
        self.entries.get(value).cloned().unwrap_or_default()
    }

    /// Range search: return ISNs whose descriptor values fall within `[low, high]`.
    pub fn range_search(&self, low: &str, high: &str) -> Vec<Isn> {
        let mut result = Vec::new();
        for (_key, isns) in self.entries.range(low.to_string()..=high.to_string()) {
            result.extend(isns);
        }
        result.sort_unstable();
        result.dedup();
        result
    }

    /// Return the number of distinct values in this list.
    pub fn value_count(&self) -> usize {
        self.entries.len()
    }
}

impl Default for InvertedList {
    fn default() -> Self {
        Self::new()
    }
}

// ── Tests ──────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adabas_file_store_and_read() {
        let mut file = AdabasFile::new(1, "EMPLOYEES");
        let isn = file.allocate_isn();
        assert_eq!(isn, 1);
        file.store_record(isn, b"hello".to_vec()).unwrap();
        assert_eq!(file.read_record(isn).unwrap(), b"hello");
    }

    #[test]
    fn adabas_file_delete() {
        let mut file = AdabasFile::new(1, "TEST");
        let isn = file.allocate_isn();
        file.store_record(isn, b"data".to_vec()).unwrap();
        let removed = file.delete_record(isn).unwrap();
        assert_eq!(removed, b"data");
        assert!(file.read_record(isn).is_err());
    }

    #[test]
    fn adabas_file_update() {
        let mut file = AdabasFile::new(1, "TEST");
        let isn = file.allocate_isn();
        file.store_record(isn, b"old".to_vec()).unwrap();
        file.update_record(isn, b"new".to_vec()).unwrap();
        assert_eq!(file.read_record(isn).unwrap(), b"new");
    }

    #[test]
    fn data_storage_round_trip() {
        let mut ds = DataStorage::new();
        let rabn = ds.store(b"test".to_vec()).unwrap();
        assert_eq!(ds.read(rabn), Some(b"test".as_slice()));
        assert_eq!(ds.block_count(), 1);
    }

    #[test]
    fn address_converter_operations() {
        let mut ac = AddressConverter::new();
        assert!(ac.is_empty());
        ac.insert(1, 100);
        ac.insert(2, 200);
        assert_eq!(ac.len(), 2);
        assert_eq!(ac.lookup(1), Some(100));
        assert_eq!(ac.remove(1), Some(100));
        assert_eq!(ac.lookup(1), None);
    }

    #[test]
    fn inverted_list_insert_and_search() {
        let mut il = InvertedList::new();
        il.insert("SMITH", 1);
        il.insert("SMITH", 3);
        il.insert("JONES", 2);
        assert_eq!(il.search("SMITH"), vec![1, 3]);
        assert_eq!(il.search("JONES"), vec![2]);
        assert_eq!(il.search("DOE"), vec![]);
    }

    #[test]
    fn inverted_list_remove() {
        let mut il = InvertedList::new();
        il.insert("A", 1);
        il.insert("A", 2);
        il.remove("A", 1);
        assert_eq!(il.search("A"), vec![2]);
        il.remove("A", 2);
        assert_eq!(il.value_count(), 0);
    }

    #[test]
    fn inverted_list_range_search() {
        let mut il = InvertedList::new();
        il.insert("A", 1);
        il.insert("B", 2);
        il.insert("C", 3);
        il.insert("D", 4);
        let result = il.range_search("B", "C");
        assert_eq!(result, vec![2, 3]);
    }

    #[test]
    fn inverted_list_deduplicates() {
        let mut il = InvertedList::new();
        il.insert("X", 5);
        il.insert("X", 5);
        assert_eq!(il.search("X"), vec![5]);
    }

    #[test]
    fn associator_search() {
        let mut assoc = AssociatorStorage::new();
        assoc.inverted_list_for("AA").insert("SMITH", 1);
        assoc.inverted_list_for("AA").insert("SMITH", 3);
        assert_eq!(assoc.search("AA", "SMITH"), vec![1, 3]);
        assert_eq!(assoc.search("AA", "DOE"), vec![]);
        assert_eq!(assoc.search("BB", "SMITH"), vec![]);
    }

    #[test]
    fn file_top_isn() {
        let mut file = AdabasFile::new(10, "T");
        assert_eq!(file.top_isn(), 0);
        file.allocate_isn();
        file.allocate_isn();
        assert_eq!(file.top_isn(), 2);
    }
}
