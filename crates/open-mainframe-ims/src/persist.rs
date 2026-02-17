//! Database persistence for IMS.
//!
//! Provides a `PersistentStore` trait that abstracts in-memory vs PostgreSQL
//! storage. The in-memory `HierarchicalStore` implements this trait directly.
//! A `PostgresStore` implementation (feature-gated behind `postgres`) uses
//! the generated schema for persistent storage.

use std::collections::HashMap;

use crate::dli::store::{HierarchicalStore, QualOp, SegmentRecord};
#[cfg(feature = "postgres")]
use crate::ImsResult;

/// Trait abstracting IMS database storage operations.
///
/// Both `HierarchicalStore` (in-memory) and `PostgresStore` implement this
/// trait so that the runtime can work with either storage backend.
pub trait PersistentStore {
    /// Insert a root segment, returning its record ID.
    fn store_insert_root(&mut self, segment_name: &str, data: Vec<u8>) -> u64;

    /// Insert a child segment under a parent.
    fn store_insert_child(
        &mut self,
        parent_id: u64,
        segment_name: &str,
        data: Vec<u8>,
    ) -> Option<u64>;

    /// Insert a segment with key fields.
    fn store_insert_with_keys(
        &mut self,
        parent_id: u64,
        segment_name: &str,
        data: Vec<u8>,
        keys: HashMap<String, Vec<u8>>,
    ) -> Option<u64>;

    /// Get a record by ID.
    fn store_get(&self, id: u64) -> Option<SegmentRecord>;

    /// Find the first root segment of a given type.
    fn store_find_root(&self, segment_name: &str) -> Option<SegmentRecord>;

    /// Find a root matching a qualification.
    fn store_find_root_qualified(
        &self,
        segment_name: &str,
        field: &str,
        value: &[u8],
        op: QualOp,
    ) -> Option<SegmentRecord>;

    /// Find the first child of a given type under a parent.
    fn store_find_first_child(
        &self,
        parent_id: u64,
        segment_name: &str,
    ) -> Option<SegmentRecord>;

    /// Find a child matching a qualification.
    fn store_find_child_qualified(
        &self,
        parent_id: u64,
        segment_name: &str,
        field: &str,
        value: &[u8],
        op: QualOp,
    ) -> Option<SegmentRecord>;

    /// Delete a record and all its descendants.
    fn store_delete(&mut self, id: u64) -> bool;

    /// Update a record's data and keys.
    fn store_update(&mut self, id: u64, data: Vec<u8>, keys: HashMap<String, Vec<u8>>) -> bool;

    /// Check for duplicate key.
    fn store_duplicate_key_exists(
        &self,
        segment_name: &str,
        parent_id: u64,
        key_field: &str,
        key_value: &[u8],
    ) -> bool;

    /// Get total record count.
    fn store_len(&self) -> usize;

    /// Create a snapshot for commit/rollback.
    fn store_snapshot(&self) -> Box<dyn PersistentStoreSnapshot>;

    /// Restore from a snapshot.
    fn store_restore(&mut self, snapshot: &dyn PersistentStoreSnapshot);

    /// Get the store name.
    fn store_name(&self) -> &str;
}

/// A snapshot of a persistent store, used for commit/rollback.
pub trait PersistentStoreSnapshot: std::fmt::Debug {
    /// Get the underlying HierarchicalStore if this is an in-memory snapshot.
    fn as_hierarchical_store(&self) -> Option<&HierarchicalStore>;
}

/// In-memory snapshot wrapping a cloned HierarchicalStore.
#[derive(Debug)]
pub struct InMemorySnapshot {
    store: HierarchicalStore,
}

impl PersistentStoreSnapshot for InMemorySnapshot {
    fn as_hierarchical_store(&self) -> Option<&HierarchicalStore> {
        Some(&self.store)
    }
}

/// Implement PersistentStore for HierarchicalStore (in-memory backend).
impl PersistentStore for HierarchicalStore {
    fn store_insert_root(&mut self, segment_name: &str, data: Vec<u8>) -> u64 {
        self.insert_root(segment_name, data)
    }

    fn store_insert_child(
        &mut self,
        parent_id: u64,
        segment_name: &str,
        data: Vec<u8>,
    ) -> Option<u64> {
        self.insert_child(parent_id, segment_name, data)
    }

    fn store_insert_with_keys(
        &mut self,
        parent_id: u64,
        segment_name: &str,
        data: Vec<u8>,
        keys: HashMap<String, Vec<u8>>,
    ) -> Option<u64> {
        self.insert_with_keys(parent_id, segment_name, data, keys)
    }

    fn store_get(&self, id: u64) -> Option<SegmentRecord> {
        self.get(id).cloned()
    }

    fn store_find_root(&self, segment_name: &str) -> Option<SegmentRecord> {
        self.find_root(segment_name).cloned()
    }

    fn store_find_root_qualified(
        &self,
        segment_name: &str,
        field: &str,
        value: &[u8],
        op: QualOp,
    ) -> Option<SegmentRecord> {
        self.find_root_qualified(segment_name, field, value, op).cloned()
    }

    fn store_find_first_child(
        &self,
        parent_id: u64,
        segment_name: &str,
    ) -> Option<SegmentRecord> {
        self.find_first_child(parent_id, segment_name).cloned()
    }

    fn store_find_child_qualified(
        &self,
        parent_id: u64,
        segment_name: &str,
        field: &str,
        value: &[u8],
        op: QualOp,
    ) -> Option<SegmentRecord> {
        self.find_child_qualified(parent_id, segment_name, field, value, op)
            .cloned()
    }

    fn store_delete(&mut self, id: u64) -> bool {
        self.delete(id)
    }

    fn store_update(&mut self, id: u64, data: Vec<u8>, keys: HashMap<String, Vec<u8>>) -> bool {
        self.update_with_keys(id, data, keys)
    }

    fn store_duplicate_key_exists(
        &self,
        segment_name: &str,
        parent_id: u64,
        key_field: &str,
        key_value: &[u8],
    ) -> bool {
        self.duplicate_key_exists(segment_name, parent_id, key_field, key_value)
    }

    fn store_len(&self) -> usize {
        self.len()
    }

    fn store_snapshot(&self) -> Box<dyn PersistentStoreSnapshot> {
        Box::new(InMemorySnapshot {
            store: self.snapshot(),
        })
    }

    fn store_restore(&mut self, snapshot: &dyn PersistentStoreSnapshot) {
        if let Some(hs) = snapshot.as_hierarchical_store() {
            self.restore_from(hs);
        }
    }

    fn store_name(&self) -> &str {
        &self.name
    }
}

// -----------------------------------------------------------------------
// PostgresStore (feature-gated)
// -----------------------------------------------------------------------

/// Configuration for PostgreSQL-backed IMS store.
#[cfg(feature = "postgres")]
#[derive(Debug, Clone)]
pub struct PostgresStoreConfig {
    /// PostgreSQL connection string
    pub connection_string: String,
    /// Database (DBD) name — used for table prefixes
    pub db_name: String,
    /// Whether to auto-create tables on init
    pub auto_create_tables: bool,
}

/// PostgreSQL-backed persistent store for IMS databases.
///
/// Uses the closure table pattern generated by `schema::PostgresSchema`
/// to persist hierarchical segment data in PostgreSQL.
#[cfg(feature = "postgres")]
pub struct PostgresStore {
    config: PostgresStoreConfig,
    client: postgres::Client,
    name: String,
}

#[cfg(feature = "postgres")]
impl PostgresStore {
    /// Create a new PostgresStore with the given configuration.
    pub fn new(config: PostgresStoreConfig) -> ImsResult<Self> {
        let client = postgres::Client::connect(&config.connection_string, postgres::NoTls)
            .map_err(|e| crate::ImsError::ConnectionError(e.to_string()))?;

        let name = config.db_name.clone();

        let mut store = Self {
            config,
            client,
            name,
        };

        if store.config.auto_create_tables {
            store.ensure_tables()?;
        }

        Ok(store)
    }

    /// Ensure the required tables exist.
    fn ensure_tables(&mut self) -> ImsResult<()> {
        let segments_table = format!("{}_segments", self.config.db_name.to_lowercase());
        let hierarchy_table = format!("{}_hierarchy", self.config.db_name.to_lowercase());

        let ddl = format!(
            r#"
            CREATE TABLE IF NOT EXISTS {seg} (
                id BIGSERIAL PRIMARY KEY,
                segment_type VARCHAR(8) NOT NULL,
                parent_id BIGINT REFERENCES {seg}(id) ON DELETE CASCADE,
                sequence_num INTEGER NOT NULL DEFAULT 0,
                data JSONB NOT NULL DEFAULT '{{}}'::jsonb,
                raw_data BYTEA,
                created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
                updated_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
            );
            CREATE TABLE IF NOT EXISTS {hier} (
                ancestor_id BIGINT NOT NULL REFERENCES {seg}(id) ON DELETE CASCADE,
                descendant_id BIGINT NOT NULL REFERENCES {seg}(id) ON DELETE CASCADE,
                depth INTEGER NOT NULL,
                PRIMARY KEY (ancestor_id, descendant_id)
            );
            CREATE INDEX IF NOT EXISTS {seg}_type_idx ON {seg}(segment_type);
            CREATE INDEX IF NOT EXISTS {seg}_parent_idx ON {seg}(parent_id);
            "#,
            seg = segments_table,
            hier = hierarchy_table,
        );

        self.client
            .batch_execute(&ddl)
            .map_err(|e| crate::ImsError::IoError(e.to_string()))?;

        Ok(())
    }

    /// Bulk-load from an in-memory HierarchicalStore to PostgreSQL.
    pub fn sync_from_memory(&mut self, store: &HierarchicalStore) -> ImsResult<()> {
        let seg_table = format!("{}_segments", self.config.db_name.to_lowercase());

        // Clear existing data
        self.client
            .execute(&format!("DELETE FROM {}", seg_table), &[])
            .map_err(|e| crate::ImsError::IoError(e.to_string()))?;

        // Insert all records
        for root_id in store.root_ids() {
            self.sync_record(store, *root_id, None, &seg_table)?;
        }

        Ok(())
    }

    /// Recursively sync a record and its children.
    fn sync_record(
        &mut self,
        store: &HierarchicalStore,
        id: u64,
        pg_parent_id: Option<i64>,
        seg_table: &str,
    ) -> ImsResult<i64> {
        let record = store
            .get(id)
            .ok_or_else(|| crate::ImsError::SegmentNotFound(format!("Record {}", id)))?;

        let keys_json = serde_json::to_value(&record.keys)
            .unwrap_or_else(|_| serde_json::Value::Object(serde_json::Map::new()));

        let pg_id: i64 = self
            .client
            .query_one(
                &format!(
                    "INSERT INTO {} (segment_type, parent_id, sequence_num, data, raw_data) \
                     VALUES ($1, $2, $3, $4, $5) RETURNING id",
                    seg_table
                ),
                &[
                    &record.segment_name,
                    &pg_parent_id,
                    &(record.sequence as i32),
                    &keys_json,
                    &record.data.as_slice(),
                ],
            )
            .map_err(|e| crate::ImsError::IoError(e.to_string()))?
            .get(0);

        // Sync children
        let children = store.find_children(id, None);
        for child in children {
            self.sync_record(store, child.id, Some(pg_id), seg_table)?;
        }

        Ok(pg_id)
    }
}

/// Bulk-export method on HierarchicalStore: sync all data to a PostgreSQL store.
#[cfg(feature = "postgres")]
impl HierarchicalStore {
    /// Export all data from this in-memory store to a PostgresStore.
    pub fn sync_to_postgres(&self, pg_store: &mut PostgresStore) -> ImsResult<()> {
        pg_store.sync_from_memory(self)
    }
}

// -----------------------------------------------------------------------
// Tests
// -----------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dli::store::HierarchicalStore;

    #[test]
    fn test_persistent_store_trait_insert_root() {
        let mut store = HierarchicalStore::new("TESTDB");
        let id = store.store_insert_root("CUSTOMER", b"Cust1".to_vec());
        assert!(id > 0);
        assert_eq!(store.store_len(), 1);
    }

    #[test]
    fn test_persistent_store_trait_insert_child() {
        let mut store = HierarchicalStore::new("TESTDB");
        let root = store.store_insert_root("CUSTOMER", b"Cust1".to_vec());
        let child = store.store_insert_child(root, "ORDER", b"Ord1".to_vec());
        assert!(child.is_some());
        assert_eq!(store.store_len(), 2);
    }

    #[test]
    fn test_persistent_store_trait_find_root() {
        let mut store = HierarchicalStore::new("TESTDB");
        store.store_insert_root("CUSTOMER", b"Cust1".to_vec());
        let found = store.store_find_root("CUSTOMER");
        assert!(found.is_some());
        assert_eq!(found.unwrap().data, b"Cust1");
    }

    #[test]
    fn test_persistent_store_trait_find_qualified() {
        let mut store = HierarchicalStore::new("TESTDB");
        let mut keys = HashMap::new();
        keys.insert("CUSTNO".to_string(), b"C001".to_vec());
        store.store_insert_with_keys(0, "CUSTOMER", b"Cust1".to_vec(), keys);

        let found = store.store_find_root_qualified("CUSTOMER", "CUSTNO", b"C001", QualOp::Eq);
        assert!(found.is_some());
    }

    #[test]
    fn test_persistent_store_trait_delete() {
        let mut store = HierarchicalStore::new("TESTDB");
        let id = store.store_insert_root("CUSTOMER", b"Cust1".to_vec());
        assert_eq!(store.store_len(), 1);
        assert!(store.store_delete(id));
        assert_eq!(store.store_len(), 0);
    }

    #[test]
    fn test_persistent_store_trait_snapshot_restore() {
        let mut store = HierarchicalStore::new("TESTDB");
        store.store_insert_root("CUSTOMER", b"Cust1".to_vec());

        let snapshot = store.store_snapshot();
        store.store_insert_root("CUSTOMER", b"Cust2".to_vec());
        assert_eq!(store.store_len(), 2);

        store.store_restore(snapshot.as_ref());
        assert_eq!(store.store_len(), 1);
    }

    #[test]
    fn test_persistent_store_name() {
        let store = HierarchicalStore::new("MYDB");
        assert_eq!(store.store_name(), "MYDB");
    }
}
