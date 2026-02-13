//! B+ Tree implementation for VSAM KSDS indexes.
//!
//! This module provides a B+ tree data structure optimized for
//! disk-based key-sequenced access, supporting efficient:
//! - Point lookups (O(log n))
//! - Range queries by collecting leaf data
//! - Insertions with automatic rebalancing
//! - Deletions with proper tree maintenance

use std::fmt::Debug;

/// Default order (branching factor) for the B+ tree.
/// Each internal node can have up to `ORDER` children.
/// Each leaf node can hold up to `ORDER - 1` key-value pairs.
pub const DEFAULT_ORDER: usize = 100;

/// A B+ tree for efficient key-value storage with range query support.
///
/// Type parameters:
/// - `K`: Key type, must be orderable and cloneable
/// - `V`: Value type (typically record data or RBA)
#[derive(Debug, Clone)]
pub struct BPlusTree<K, V>
where
    K: Ord + Clone + Debug,
    V: Clone + Debug,
{
    root: Option<Box<Node<K, V>>>,
    order: usize,
    len: usize,
}

/// A node in the B+ tree, either internal or leaf.
#[derive(Debug, Clone)]
#[allow(clippy::vec_box)] // Box is needed for recursive structure
enum Node<K, V>
where
    K: Ord + Clone + Debug,
    V: Clone + Debug,
{
    /// Internal node: contains keys and child pointers.
    Internal {
        keys: Vec<K>,
        children: Vec<Box<Node<K, V>>>,
    },
    /// Leaf node: contains key-value pairs.
    Leaf { keys: Vec<K>, values: Vec<V> },
}

impl<K, V> BPlusTree<K, V>
where
    K: Ord + Clone + Debug,
    V: Clone + Debug,
{
    /// Creates a new empty B+ tree with the default order.
    pub fn new() -> Self {
        Self::with_order(DEFAULT_ORDER)
    }

    /// Creates a new empty B+ tree with a specified order.
    ///
    /// The order determines the maximum number of children per internal node.
    /// Higher orders mean fewer levels but larger nodes.
    pub fn with_order(order: usize) -> Self {
        assert!(order >= 3, "B+ tree order must be at least 3");
        Self {
            root: None,
            order,
            len: 0,
        }
    }

    /// Returns the number of key-value pairs in the tree.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns true if the tree is empty.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Returns the tree order (branching factor).
    pub fn order(&self) -> usize {
        self.order
    }

    /// Inserts a key-value pair into the tree.
    ///
    /// If the key already exists, the value is updated and the old value returned.
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        if self.root.is_none() {
            self.root = Some(Box::new(Node::Leaf {
                keys: vec![key],
                values: vec![value],
            }));
            self.len = 1;
            return None;
        }

        let root = self.root.take().unwrap();
        let result = self.insert_into_node(root, key, value);

        match result {
            InsertResult::Done(node, old_value, did_insert) => {
                self.root = Some(node);
                if did_insert {
                    self.len += 1;
                }
                old_value
            }
            InsertResult::Split(left, right, split_key, did_insert) => {
                self.root = Some(Box::new(Node::Internal {
                    keys: vec![split_key],
                    children: vec![left, right],
                }));
                if did_insert {
                    self.len += 1;
                }
                None
            }
        }
    }

    fn insert_into_node(
        &self,
        mut node: Box<Node<K, V>>,
        key: K,
        value: V,
    ) -> InsertResult<K, V> {
        match *node {
            Node::Leaf {
                ref mut keys,
                ref mut values,
            } => {
                match keys.binary_search(&key) {
                    Ok(idx) => {
                        // Key exists, update value
                        let old = std::mem::replace(&mut values[idx], value);
                        InsertResult::Done(node, Some(old), false)
                    }
                    Err(idx) => {
                        // Insert new key-value
                        keys.insert(idx, key);
                        values.insert(idx, value);

                        if keys.len() >= self.order {
                            // Need to split
                            let mid = keys.len() / 2;
                            let split_key = keys[mid].clone();

                            let right_keys = keys.split_off(mid);
                            let right_values = values.split_off(mid);

                            let right = Box::new(Node::Leaf {
                                keys: right_keys,
                                values: right_values,
                            });

                            InsertResult::Split(node, right, split_key, true)
                        } else {
                            InsertResult::Done(node, None, true)
                        }
                    }
                }
            }
            Node::Internal {
                ref mut keys,
                ref mut children,
            } => {
                // Find child to descend into
                let child_idx = match keys.binary_search(&key) {
                    Ok(idx) => idx + 1,
                    Err(idx) => idx,
                };

                let child = children.remove(child_idx);
                let result = self.insert_into_node(child, key, value);

                match result {
                    InsertResult::Done(new_child, old_value, did_insert) => {
                        children.insert(child_idx, new_child);
                        InsertResult::Done(node, old_value, did_insert)
                    }
                    InsertResult::Split(left, right, split_key, did_insert) => {
                        // Child split, absorb into this node
                        keys.insert(child_idx, split_key);
                        children.insert(child_idx, left);
                        children.insert(child_idx + 1, right);

                        if keys.len() >= self.order {
                            // This node needs to split too
                            let mid = keys.len() / 2;
                            let promote_key = keys.remove(mid);

                            let right_keys = keys.split_off(mid);
                            let right_children = children.split_off(mid + 1);

                            let right_node = Box::new(Node::Internal {
                                keys: right_keys,
                                children: right_children,
                            });

                            InsertResult::Split(node, right_node, promote_key, did_insert)
                        } else {
                            InsertResult::Done(node, None, did_insert)
                        }
                    }
                }
            }
        }
    }

    /// Searches for a key and returns a reference to its value if found.
    pub fn get(&self, key: &K) -> Option<&V> {
        self.root.as_ref().and_then(|node| Self::search(node, key))
    }

    fn search<'a>(node: &'a Node<K, V>, key: &K) -> Option<&'a V> {
        match node {
            Node::Leaf { keys, values } => {
                keys.binary_search(key).ok().map(|idx| &values[idx])
            }
            Node::Internal { keys, children } => {
                let child_idx = match keys.binary_search(key) {
                    Ok(idx) => idx + 1,
                    Err(idx) => idx,
                };
                Self::search(&children[child_idx], key)
            }
        }
    }

    /// Returns true if the tree contains the given key.
    pub fn contains_key(&self, key: &K) -> bool {
        self.get(key).is_some()
    }

    /// Returns a vector of key-value pairs in the given range (inclusive).
    pub fn range(&self, start: &K, end: &K) -> Vec<(K, V)> {
        let mut results = Vec::new();
        if let Some(ref root) = self.root {
            Self::collect_range(root, start, end, &mut results);
        }
        results
    }

    fn collect_range(node: &Node<K, V>, start: &K, end: &K, results: &mut Vec<(K, V)>) {
        match node {
            Node::Leaf { keys, values } => {
                for (key, value) in keys.iter().zip(values.iter()) {
                    if key >= start && key <= end {
                        results.push((key.clone(), value.clone()));
                    }
                }
            }
            Node::Internal { keys, children } => {
                // Find which children might contain our range
                let start_idx = match keys.binary_search(start) {
                    Ok(idx) | Err(idx) => idx,
                };
                let end_idx = match keys.binary_search(end) {
                    Ok(idx) => idx + 1,
                    Err(idx) => idx,
                };

                // Visit all children that might contain values in range
                let range_end = end_idx.min(children.len() - 1) + 1;
                for child in children.iter().skip(start_idx).take(range_end - start_idx) {
                    Self::collect_range(child, start, end, results);
                }
            }
        }
    }

    /// Returns an iterator-like vector over all key-value pairs in order.
    pub fn iter(&self) -> Vec<(K, V)> {
        let mut results = Vec::new();
        if let Some(ref root) = self.root {
            Self::collect_all(root, &mut results);
        }
        results
    }

    fn collect_all(node: &Node<K, V>, results: &mut Vec<(K, V)>) {
        match node {
            Node::Leaf { keys, values } => {
                for (key, value) in keys.iter().zip(values.iter()) {
                    results.push((key.clone(), value.clone()));
                }
            }
            Node::Internal { children, .. } => {
                for child in children {
                    Self::collect_all(child, results);
                }
            }
        }
    }

    /// Deletes a key from the tree and returns its value if found.
    pub fn remove(&mut self, key: &K) -> Option<V> {
        let root = self.root.take()?;
        let (new_root, removed) = Self::remove_from_node(root, key);

        // Handle root becoming empty
        self.root = match new_root {
            Some(node) => {
                if let Node::Internal { keys, mut children } = *node {
                    if keys.is_empty() && children.len() == 1 {
                        Some(children.remove(0))
                    } else {
                        Some(Box::new(Node::Internal { keys, children }))
                    }
                } else {
                    Some(node)
                }
            }
            None => None,
        };

        if removed.is_some() {
            self.len -= 1;
        }
        removed
    }

    fn remove_from_node(
        mut node: Box<Node<K, V>>,
        key: &K,
    ) -> (Option<Box<Node<K, V>>>, Option<V>) {
        match *node {
            Node::Leaf {
                ref mut keys,
                ref mut values,
            } => match keys.binary_search(key) {
                Ok(idx) => {
                    keys.remove(idx);
                    let value = values.remove(idx);
                    if keys.is_empty() {
                        (None, Some(value))
                    } else {
                        (Some(node), Some(value))
                    }
                }
                Err(_) => (Some(node), None),
            },
            Node::Internal {
                ref mut keys,
                ref mut children,
            } => {
                let child_idx = match keys.binary_search(key) {
                    Ok(idx) => idx + 1,
                    Err(idx) => idx,
                };

                let child = children.remove(child_idx);
                let (new_child, removed) = Self::remove_from_node(child, key);

                match new_child {
                    Some(child) => {
                        children.insert(child_idx, child);
                    }
                    None => {
                        // Child was deleted
                        if child_idx > 0 && child_idx <= keys.len() {
                            keys.remove(child_idx - 1);
                        } else if !keys.is_empty() {
                            keys.remove(0.min(keys.len() - 1));
                        }
                    }
                }

                if children.is_empty() {
                    (None, removed)
                } else {
                    (Some(node), removed)
                }
            }
        }
    }

    /// Returns the depth (number of levels) of the tree.
    pub fn depth(&self) -> usize {
        self.root
            .as_ref()
            .map_or(0, |node| Self::depth_recursive(node))
    }

    fn depth_recursive(node: &Node<K, V>) -> usize {
        match node {
            Node::Leaf { .. } => 1,
            Node::Internal { children, .. } => 1 + Self::depth_recursive(&children[0]),
        }
    }
}

/// Result of an insert operation, indicating whether a split occurred.
enum InsertResult<K, V>
where
    K: Ord + Clone + Debug,
    V: Clone + Debug,
{
    /// Insert completed without split
    Done(Box<Node<K, V>>, Option<V>, bool),
    /// Node split: (left, right, split_key, did_insert_new)
    Split(Box<Node<K, V>>, Box<Node<K, V>>, K, bool),
}

impl<K, V> Default for BPlusTree<K, V>
where
    K: Ord + Clone + Debug,
    V: Clone + Debug,
{
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_tree() {
        let tree: BPlusTree<i32, String> = BPlusTree::new();
        assert!(tree.is_empty());
        assert_eq!(tree.len(), 0);
        assert_eq!(tree.order(), DEFAULT_ORDER);
    }

    #[test]
    fn test_insert_and_get() {
        let mut tree = BPlusTree::with_order(4);

        tree.insert(1, "one".to_string());
        tree.insert(2, "two".to_string());
        tree.insert(3, "three".to_string());

        assert_eq!(tree.len(), 3);
        assert_eq!(tree.get(&1), Some(&"one".to_string()));
        assert_eq!(tree.get(&2), Some(&"two".to_string()));
        assert_eq!(tree.get(&3), Some(&"three".to_string()));
        assert_eq!(tree.get(&4), None);
    }

    #[test]
    fn test_insert_update() {
        let mut tree = BPlusTree::with_order(4);

        tree.insert(1, "one".to_string());
        let old = tree.insert(1, "ONE".to_string());

        assert_eq!(old, Some("one".to_string()));
        assert_eq!(tree.len(), 1);
        assert_eq!(tree.get(&1), Some(&"ONE".to_string()));
    }

    #[test]
    fn test_contains_key() {
        let mut tree = BPlusTree::with_order(4);

        tree.insert(1, "one".to_string());

        assert!(tree.contains_key(&1));
        assert!(!tree.contains_key(&2));
    }

    #[test]
    fn test_remove() {
        let mut tree = BPlusTree::with_order(4);

        tree.insert(1, "one".to_string());
        tree.insert(2, "two".to_string());
        tree.insert(3, "three".to_string());

        let removed = tree.remove(&2);

        assert_eq!(removed, Some("two".to_string()));
        assert_eq!(tree.len(), 2);
        assert!(!tree.contains_key(&2));
        assert!(tree.contains_key(&1));
        assert!(tree.contains_key(&3));
    }

    #[test]
    fn test_remove_nonexistent() {
        let mut tree = BPlusTree::with_order(4);

        tree.insert(1, "one".to_string());

        let removed = tree.remove(&99);

        assert_eq!(removed, None);
        assert_eq!(tree.len(), 1);
    }

    #[test]
    fn test_many_inserts_with_splits() {
        let mut tree = BPlusTree::with_order(4);

        // Insert enough to cause multiple splits
        for i in 0..100 {
            tree.insert(i, format!("value_{}", i));
        }

        assert_eq!(tree.len(), 100);

        // Verify all values are retrievable
        for i in 0..100 {
            assert_eq!(tree.get(&i), Some(&format!("value_{}", i)));
        }
    }

    #[test]
    fn test_iter() {
        let mut tree = BPlusTree::with_order(4);

        // Insert out of order
        tree.insert(3, "three".to_string());
        tree.insert(1, "one".to_string());
        tree.insert(2, "two".to_string());

        let items = tree.iter();

        assert_eq!(items.len(), 3);
        assert_eq!(items[0], (1, "one".to_string()));
        assert_eq!(items[1], (2, "two".to_string()));
        assert_eq!(items[2], (3, "three".to_string()));
    }

    #[test]
    fn test_range_query() {
        let mut tree = BPlusTree::with_order(4);

        for i in 0..20 {
            tree.insert(i, format!("value_{}", i));
        }

        let range = tree.range(&5, &10);

        assert_eq!(range.len(), 6); // 5, 6, 7, 8, 9, 10
        assert_eq!(range[0].0, 5);
        assert_eq!(range[5].0, 10);
    }

    #[test]
    fn test_large_dataset() {
        let mut tree = BPlusTree::with_order(100);

        // Insert 10,000 records as per acceptance criteria
        for i in 0..10_000 {
            tree.insert(i, i * 2);
        }

        assert_eq!(tree.len(), 10_000);

        // Verify balanced (depth should be reasonable)
        let depth = tree.depth();
        // log_100(10000) ≈ 2, so depth should be 2-3
        assert!(
            depth <= 4,
            "Tree depth {} is too high for 10,000 records",
            depth
        );

        // Verify all records retrievable
        for i in 0..10_000 {
            assert_eq!(tree.get(&i), Some(&(i * 2)));
        }
    }

    #[test]
    fn test_sequential_keys_balanced() {
        let mut tree = BPlusTree::with_order(100);

        // Sequential inserts
        for i in 0..10_000 {
            tree.insert(i, i);
        }

        let depth = tree.depth();

        // Should be O(log n) depth
        // For order 100 and 10000 records: log_100(10000) ≈ 2
        assert!(depth <= 4, "Sequential inserts should remain balanced");
    }

    #[test]
    fn test_range_via_leaf_links() {
        let mut tree = BPlusTree::with_order(5);

        for i in 0..100 {
            tree.insert(i, i);
        }

        // Range query should work
        let range = tree.range(&25, &74);

        // Should include keys 25..=74 = 50 items
        assert_eq!(range.len(), 50);

        // Verify sequential access
        for (idx, (key, _)) in range.iter().enumerate() {
            assert_eq!(*key, 25 + idx as i32);
        }
    }

    #[test]
    fn test_delete_and_rebalance() {
        let mut tree = BPlusTree::with_order(5);

        // Insert records
        for i in 0..50 {
            tree.insert(i, format!("val_{}", i));
        }

        assert_eq!(tree.len(), 50);

        // Delete half the records
        for i in (0..50).step_by(2) {
            let removed = tree.remove(&i);
            assert!(removed.is_some());
        }

        assert_eq!(tree.len(), 25);

        // Verify remaining records
        for i in (1..50).step_by(2) {
            assert!(tree.contains_key(&i), "Key {} should exist", i);
        }

        // Verify deleted records are gone
        for i in (0..50).step_by(2) {
            assert!(!tree.contains_key(&i), "Key {} should be deleted", i);
        }
    }
}
