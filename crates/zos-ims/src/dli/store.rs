//! In-memory hierarchical data store for IMS databases.
//!
//! Provides a storage layer for IMS segments that can be queried
//! using DL/I navigation patterns.

use std::collections::HashMap;

/// A hierarchical segment record.
#[derive(Debug, Clone)]
pub struct SegmentRecord {
    /// Unique record ID
    pub id: u64,
    /// Segment type name
    pub segment_name: String,
    /// Parent record ID (0 for root segments)
    pub parent_id: u64,
    /// Sequence number within parent (for ordering)
    pub sequence: u32,
    /// Segment data
    pub data: Vec<u8>,
    /// Key field values for qualification
    pub keys: HashMap<String, Vec<u8>>,
}

impl SegmentRecord {
    /// Create a new segment record.
    pub fn new(id: u64, segment_name: &str, parent_id: u64, data: Vec<u8>) -> Self {
        Self {
            id,
            segment_name: segment_name.to_string(),
            parent_id,
            sequence: 0,
            data,
            keys: HashMap::new(),
        }
    }

    /// Add a key field.
    pub fn with_key(mut self, field: &str, value: Vec<u8>) -> Self {
        self.keys.insert(field.to_uppercase(), value);
        self
    }

    /// Set sequence number.
    pub fn with_sequence(mut self, seq: u32) -> Self {
        self.sequence = seq;
        self
    }

    /// Get a key value.
    pub fn get_key(&self, field: &str) -> Option<&[u8]> {
        self.keys.get(&field.to_uppercase()).map(|v| v.as_slice())
    }
}

/// In-memory hierarchical database store.
#[derive(Debug)]
pub struct HierarchicalStore {
    /// Database name
    pub name: String,
    /// All records indexed by ID
    records: HashMap<u64, SegmentRecord>,
    /// Records indexed by segment type
    by_segment: HashMap<String, Vec<u64>>,
    /// Children indexed by parent ID
    children: HashMap<u64, Vec<u64>>,
    /// Root segment IDs
    roots: Vec<u64>,
    /// Next record ID
    next_id: u64,
}

impl HierarchicalStore {
    /// Create a new empty store.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            records: HashMap::new(),
            by_segment: HashMap::new(),
            children: HashMap::new(),
            roots: Vec::new(),
            next_id: 1,
        }
    }

    /// Insert a root segment.
    pub fn insert_root(&mut self, segment_name: &str, data: Vec<u8>) -> u64 {
        let id = self.next_id;
        self.next_id += 1;

        let mut record = SegmentRecord::new(id, segment_name, 0, data);
        record.sequence = self.roots.len() as u32;

        self.records.insert(id, record);
        self.by_segment
            .entry(segment_name.to_uppercase())
            .or_default()
            .push(id);
        self.roots.push(id);

        id
    }

    /// Insert a child segment under a parent.
    pub fn insert_child(&mut self, parent_id: u64, segment_name: &str, data: Vec<u8>) -> Option<u64> {
        if !self.records.contains_key(&parent_id) {
            return None;
        }

        let id = self.next_id;
        self.next_id += 1;

        let children = self.children.entry(parent_id).or_default();
        let sequence = children.len() as u32;

        let mut record = SegmentRecord::new(id, segment_name, parent_id, data);
        record.sequence = sequence;

        self.records.insert(id, record);
        self.by_segment
            .entry(segment_name.to_uppercase())
            .or_default()
            .push(id);
        children.push(id);

        Some(id)
    }

    /// Insert a segment with keys.
    pub fn insert_with_keys(
        &mut self,
        parent_id: u64,
        segment_name: &str,
        data: Vec<u8>,
        keys: HashMap<String, Vec<u8>>,
    ) -> Option<u64> {
        let id = if parent_id == 0 {
            self.insert_root(segment_name, data)
        } else {
            self.insert_child(parent_id, segment_name, data)?
        };

        if let Some(record) = self.records.get_mut(&id) {
            record.keys = keys;
        }

        Some(id)
    }

    /// Get a record by ID.
    pub fn get(&self, id: u64) -> Option<&SegmentRecord> {
        self.records.get(&id)
    }

    /// Get a mutable record by ID.
    pub fn get_mut(&mut self, id: u64) -> Option<&mut SegmentRecord> {
        self.records.get_mut(&id)
    }

    /// Find first root segment of a type.
    pub fn find_root(&self, segment_name: &str) -> Option<&SegmentRecord> {
        let upper = segment_name.to_uppercase();
        self.roots.iter()
            .filter_map(|&id| self.records.get(&id))
            .find(|r| r.segment_name.to_uppercase() == upper)
    }

    /// Find root matching qualification.
    pub fn find_root_qualified(
        &self,
        segment_name: &str,
        field: &str,
        value: &[u8],
        op: QualOp,
    ) -> Option<&SegmentRecord> {
        let upper = segment_name.to_uppercase();
        self.roots.iter()
            .filter_map(|&id| self.records.get(&id))
            .filter(|r| r.segment_name.to_uppercase() == upper)
            .find(|r| r.get_key(field).map(|v| op.matches(v, value)).unwrap_or(false))
    }

    /// Find children of a parent.
    pub fn find_children(&self, parent_id: u64, segment_name: Option<&str>) -> Vec<&SegmentRecord> {
        self.children
            .get(&parent_id)
            .map(|ids| {
                ids.iter()
                    .filter_map(|&id| self.records.get(&id))
                    .filter(|r| {
                        segment_name
                            .map(|s| r.segment_name.to_uppercase() == s.to_uppercase())
                            .unwrap_or(true)
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Find first child of a type.
    pub fn find_first_child(&self, parent_id: u64, segment_name: &str) -> Option<&SegmentRecord> {
        let children = self.find_children(parent_id, Some(segment_name));
        children.into_iter().next()
    }

    /// Find child matching qualification.
    pub fn find_child_qualified(
        &self,
        parent_id: u64,
        segment_name: &str,
        field: &str,
        value: &[u8],
        op: QualOp,
    ) -> Option<&SegmentRecord> {
        self.find_children(parent_id, Some(segment_name))
            .into_iter()
            .find(|r| r.get_key(field).map(|v| op.matches(v, value)).unwrap_or(false))
    }

    /// Get next segment in hierarchical sequence (preorder traversal).
    pub fn get_next(&self, current_id: u64, segment_name: Option<&str>) -> Option<&SegmentRecord> {
        // First try to descend to first child
        if let Some(children) = self.children.get(&current_id) {
            if let Some(&first_child) = children.first() {
                let record = self.records.get(&first_child)?;
                if segment_name.map(|s| record.segment_name.eq_ignore_ascii_case(s)).unwrap_or(true) {
                    return Some(record);
                }
                // Keep traversing until we find matching segment
                return self.get_next(first_child, segment_name);
            }
        }

        // No children, try next sibling
        self.get_next_sibling(current_id, segment_name)
    }

    /// Get next sibling or ancestor's sibling.
    fn get_next_sibling(&self, current_id: u64, segment_name: Option<&str>) -> Option<&SegmentRecord> {
        let current = self.records.get(&current_id)?;
        let parent_id = current.parent_id;

        let siblings = if parent_id == 0 {
            &self.roots
        } else {
            self.children.get(&parent_id)?
        };

        // Find current position and get next
        let pos = siblings.iter().position(|&id| id == current_id)?;

        for &sibling_id in siblings.iter().skip(pos + 1) {
            let sibling = self.records.get(&sibling_id)?;
            if segment_name.map(|s| sibling.segment_name.eq_ignore_ascii_case(s)).unwrap_or(true) {
                return Some(sibling);
            }
            // Check descendants of sibling
            if let Some(found) = self.find_first_descendant(sibling_id, segment_name) {
                return Some(found);
            }
        }

        // No more siblings, go up
        if parent_id != 0 {
            return self.get_next_sibling(parent_id, segment_name);
        }

        None
    }

    /// Find first descendant matching segment name.
    fn find_first_descendant(&self, id: u64, segment_name: Option<&str>) -> Option<&SegmentRecord> {
        let children = self.children.get(&id)?;

        for &child_id in children {
            let child = self.records.get(&child_id)?;
            if segment_name.map(|s| child.segment_name.eq_ignore_ascii_case(s)).unwrap_or(true) {
                return Some(child);
            }
            if let Some(found) = self.find_first_descendant(child_id, segment_name) {
                return Some(found);
            }
        }

        None
    }

    /// Get next child within a parent.
    pub fn get_next_within_parent(
        &self,
        current_id: u64,
        parent_id: u64,
        segment_name: Option<&str>,
    ) -> Option<&SegmentRecord> {
        let current = self.records.get(&current_id)?;

        // Ensure current is under the parent
        if current.parent_id != parent_id {
            // Current might be deeper, check ancestry
            let mut ancestor_id = current.parent_id;
            let mut found_parent = false;

            while ancestor_id != 0 {
                if ancestor_id == parent_id {
                    found_parent = true;
                    break;
                }
                if let Some(ancestor) = self.records.get(&ancestor_id) {
                    ancestor_id = ancestor.parent_id;
                } else {
                    break;
                }
            }

            if !found_parent {
                return None;
            }
        }

        // First try children
        if let Some(children) = self.children.get(&current_id) {
            for &child_id in children {
                let child = self.records.get(&child_id)?;
                if segment_name.map(|s| child.segment_name.eq_ignore_ascii_case(s)).unwrap_or(true) {
                    return Some(child);
                }
            }
        }

        // Try siblings (but stay within parent tree)
        self.get_next_sibling_within_parent(current_id, parent_id, segment_name)
    }

    /// Get next sibling staying within parent boundaries.
    fn get_next_sibling_within_parent(
        &self,
        current_id: u64,
        boundary_parent: u64,
        segment_name: Option<&str>,
    ) -> Option<&SegmentRecord> {
        let current = self.records.get(&current_id)?;
        let parent_id = current.parent_id;

        // Don't go above boundary parent
        if parent_id == 0 || current_id == boundary_parent {
            return None;
        }

        let siblings = self.children.get(&parent_id)?;
        let pos = siblings.iter().position(|&id| id == current_id)?;

        for &sibling_id in siblings.iter().skip(pos + 1) {
            let sibling = self.records.get(&sibling_id)?;
            if segment_name.map(|s| sibling.segment_name.eq_ignore_ascii_case(s)).unwrap_or(true) {
                return Some(sibling);
            }
            if let Some(found) = self.find_first_descendant(sibling_id, segment_name) {
                return Some(found);
            }
        }

        // Go up (but not past boundary)
        if parent_id != boundary_parent {
            return self.get_next_sibling_within_parent(parent_id, boundary_parent, segment_name);
        }

        None
    }

    /// Delete a record and all its descendants.
    pub fn delete(&mut self, id: u64) -> bool {
        let record = match self.records.remove(&id) {
            Some(r) => r,
            None => return false,
        };

        // Remove from by_segment index
        if let Some(ids) = self.by_segment.get_mut(&record.segment_name.to_uppercase()) {
            ids.retain(|&rid| rid != id);
        }

        // Remove from parent's children
        if record.parent_id == 0 {
            self.roots.retain(|&rid| rid != id);
        } else if let Some(siblings) = self.children.get_mut(&record.parent_id) {
            siblings.retain(|&rid| rid != id);
        }

        // Recursively delete children
        if let Some(child_ids) = self.children.remove(&id) {
            for child_id in child_ids {
                self.delete(child_id);
            }
        }

        true
    }

    /// Update a record's data.
    pub fn update(&mut self, id: u64, data: Vec<u8>) -> bool {
        if let Some(record) = self.records.get_mut(&id) {
            record.data = data;
            true
        } else {
            false
        }
    }

    /// Update a record's data and keys.
    pub fn update_with_keys(&mut self, id: u64, data: Vec<u8>, keys: HashMap<String, Vec<u8>>) -> bool {
        if let Some(record) = self.records.get_mut(&id) {
            record.data = data;
            record.keys = keys;
            true
        } else {
            false
        }
    }

    /// Check if key changed (for REPL validation).
    pub fn key_changed(&self, id: u64, new_keys: &HashMap<String, Vec<u8>>, key_field: &str) -> bool {
        if let Some(record) = self.records.get(&id) {
            let old_key = record.keys.get(&key_field.to_uppercase());
            let new_key = new_keys.get(&key_field.to_uppercase());

            match (old_key, new_key) {
                (Some(old), Some(new)) => old != new,
                (None, None) => false,
                _ => true, // One exists, other doesn't
            }
        } else {
            false
        }
    }

    /// Check if a duplicate key exists.
    pub fn duplicate_key_exists(
        &self,
        segment_name: &str,
        parent_id: u64,
        key_field: &str,
        key_value: &[u8],
    ) -> bool {
        let siblings = if parent_id == 0 {
            self.roots.iter().cloned().collect::<Vec<_>>()
        } else {
            self.children.get(&parent_id).cloned().unwrap_or_default()
        };

        siblings.iter()
            .filter_map(|&id| self.records.get(&id))
            .filter(|r| r.segment_name.eq_ignore_ascii_case(segment_name))
            .any(|r| r.get_key(key_field).map(|v| v == key_value).unwrap_or(false))
    }

    /// Get record count.
    pub fn len(&self) -> usize {
        self.records.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.records.is_empty()
    }

    /// Get root IDs.
    pub fn root_ids(&self) -> &[u64] {
        &self.roots
    }

    /// Get first root.
    pub fn first_root(&self) -> Option<&SegmentRecord> {
        self.roots.first().and_then(|&id| self.records.get(&id))
    }
}

/// Qualification operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QualOp {
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
}

impl QualOp {
    /// Check if value matches qualification.
    pub fn matches(&self, field_value: &[u8], qual_value: &[u8]) -> bool {
        match self {
            QualOp::Eq => field_value == qual_value,
            QualOp::Ne => field_value != qual_value,
            QualOp::Gt => field_value > qual_value,
            QualOp::Ge => field_value >= qual_value,
            QualOp::Lt => field_value < qual_value,
            QualOp::Le => field_value <= qual_value,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_store() -> HierarchicalStore {
        let mut store = HierarchicalStore::new("TESTDB");

        // Create customers with orders and items
        let cust1 = store.insert_root("CUSTOMER", b"Customer 1".to_vec());
        let mut keys = HashMap::new();
        keys.insert("CUSTNO".to_string(), b"00001".to_vec());
        store.get_mut(cust1).unwrap().keys = keys;

        let ord1 = store.insert_child(cust1, "ORDER", b"Order 1".to_vec()).unwrap();
        store.get_mut(ord1).unwrap().keys.insert("ORDNO".to_string(), b"O001".to_vec());

        store.insert_child(ord1, "ITEM", b"Item 1".to_vec());
        store.insert_child(ord1, "ITEM", b"Item 2".to_vec());

        let ord2 = store.insert_child(cust1, "ORDER", b"Order 2".to_vec()).unwrap();
        store.get_mut(ord2).unwrap().keys.insert("ORDNO".to_string(), b"O002".to_vec());

        let cust2 = store.insert_root("CUSTOMER", b"Customer 2".to_vec());
        store.get_mut(cust2).unwrap().keys.insert("CUSTNO".to_string(), b"00002".to_vec());

        store
    }

    #[test]
    fn test_insert_and_get() {
        let store = create_test_store();
        assert_eq!(store.len(), 6);

        let cust = store.find_root("CUSTOMER").unwrap();
        assert_eq!(cust.data, b"Customer 1");
    }

    #[test]
    fn test_find_qualified() {
        let store = create_test_store();

        let cust = store.find_root_qualified("CUSTOMER", "CUSTNO", b"00002", QualOp::Eq).unwrap();
        assert_eq!(cust.data, b"Customer 2");
    }

    #[test]
    fn test_find_children() {
        let store = create_test_store();

        let cust = store.find_root("CUSTOMER").unwrap();
        let orders = store.find_children(cust.id, Some("ORDER"));
        assert_eq!(orders.len(), 2);
    }

    #[test]
    fn test_get_next() {
        let store = create_test_store();

        let cust = store.find_root("CUSTOMER").unwrap();
        let next = store.get_next(cust.id, None).unwrap();
        assert_eq!(next.segment_name, "ORDER");
        assert_eq!(next.data, b"Order 1");
    }

    #[test]
    fn test_get_next_with_filter() {
        let store = create_test_store();

        let cust = store.find_root("CUSTOMER").unwrap();
        let next = store.get_next(cust.id, Some("ITEM")).unwrap();
        assert_eq!(next.segment_name, "ITEM");
        assert_eq!(next.data, b"Item 1");
    }

    #[test]
    fn test_get_next_traverses_siblings() {
        let store = create_test_store();

        // Start at first customer, find second customer
        let cust1 = store.find_root("CUSTOMER").unwrap();
        let next_cust = store.get_next(cust1.id, Some("CUSTOMER")).unwrap();
        assert_eq!(next_cust.data, b"Customer 2");
    }

    #[test]
    fn test_delete() {
        let mut store = create_test_store();

        let cust1 = store.find_root("CUSTOMER").unwrap().id;
        assert!(store.delete(cust1));

        // Customer 1 and all children should be gone
        assert!(store.get(cust1).is_none());
        assert_eq!(store.len(), 1); // Only Customer 2 remains
    }

    #[test]
    fn test_update() {
        let mut store = create_test_store();

        let cust1 = store.find_root("CUSTOMER").unwrap().id;
        store.update(cust1, b"Updated Customer".to_vec());

        assert_eq!(store.get(cust1).unwrap().data, b"Updated Customer");
    }

    #[test]
    fn test_duplicate_key_check() {
        let store = create_test_store();

        // Should find duplicate
        assert!(store.duplicate_key_exists("CUSTOMER", 0, "CUSTNO", b"00001"));

        // Should not find duplicate
        assert!(!store.duplicate_key_exists("CUSTOMER", 0, "CUSTNO", b"99999"));
    }

    #[test]
    fn test_key_changed() {
        let store = create_test_store();

        let cust1 = store.find_root("CUSTOMER").unwrap().id;

        // Same key
        let mut keys = HashMap::new();
        keys.insert("CUSTNO".to_string(), b"00001".to_vec());
        assert!(!store.key_changed(cust1, &keys, "CUSTNO"));

        // Different key
        keys.insert("CUSTNO".to_string(), b"99999".to_vec());
        assert!(store.key_changed(cust1, &keys, "CUSTNO"));
    }
}
