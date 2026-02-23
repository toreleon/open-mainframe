//! CRYPTO-102: Key Management & Key Stores.
//!
//! Provides CKDS (Cryptographic Key Data Set), PKDS (Public Key Data Set),
//! TKDS (Token Key Data Set), key lifecycle management, and master key
//! wrapping modeled after z/OS ICSF key store facilities.

use crate::error::CryptoError;
use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Story 1: CKDS — Cryptographic Key Data Set
// ---------------------------------------------------------------------------

/// Cryptographic Key Data Set (CKDS).
///
/// Stores symmetric keys indexed by label, modeled after the z/OS ICSF CKDS.
#[derive(Debug, Clone, Default)]
pub struct Ckds {
    /// Key label to key bytes mapping.
    entries: HashMap<String, Vec<u8>>,
}

impl Ckds {
    /// Create an empty CKDS.
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert a key into the CKDS under the given label.
    pub fn insert(&mut self, label: &str, key_bytes: Vec<u8>) -> crate::Result<()> {
        if self.entries.contains_key(label) {
            return Err(CryptoError::KeyExists {
                label: label.to_string(),
                store: "CKDS".into(),
            });
        }
        self.entries.insert(label.to_string(), key_bytes);
        Ok(())
    }

    /// Retrieve key bytes by label.
    pub fn get(&self, label: &str) -> crate::Result<&[u8]> {
        self.entries.get(label).map(|v| v.as_slice()).ok_or_else(|| {
            CryptoError::KeyNotFound {
                label: label.to_string(),
                store: "CKDS".into(),
            }
        })
    }

    /// Delete a key by label.
    pub fn delete(&mut self, label: &str) -> crate::Result<Vec<u8>> {
        self.entries.remove(label).ok_or_else(|| CryptoError::KeyNotFound {
            label: label.to_string(),
            store: "CKDS".into(),
        })
    }

    /// List all key labels in the CKDS.
    pub fn list(&self) -> Vec<&str> {
        let mut labels: Vec<&str> = self.entries.keys().map(String::as_str).collect();
        labels.sort();
        labels
    }

    /// Returns the number of keys in the store.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Returns `true` if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

// ---------------------------------------------------------------------------
// Story 2: PKDS — Public Key Data Set
// ---------------------------------------------------------------------------

/// A stored public/private key pair record.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PkdsEntry {
    /// Public key bytes.
    pub public_key: Vec<u8>,
    /// Private key bytes (may be wrapped).
    pub private_key: Vec<u8>,
    /// Key type description (e.g., "RSA-2048", "EC-P256").
    pub key_type: String,
}

/// Public Key Data Set (PKDS).
///
/// Stores RSA/EC key pairs indexed by label.
#[derive(Debug, Clone, Default)]
pub struct Pkds {
    /// Key label to entry mapping.
    entries: HashMap<String, PkdsEntry>,
}

impl Pkds {
    /// Create an empty PKDS.
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert a key pair into the PKDS.
    pub fn insert(&mut self, label: &str, entry: PkdsEntry) -> crate::Result<()> {
        if self.entries.contains_key(label) {
            return Err(CryptoError::KeyExists {
                label: label.to_string(),
                store: "PKDS".into(),
            });
        }
        self.entries.insert(label.to_string(), entry);
        Ok(())
    }

    /// Retrieve a key pair entry by label.
    pub fn get(&self, label: &str) -> crate::Result<&PkdsEntry> {
        self.entries.get(label).ok_or_else(|| CryptoError::KeyNotFound {
            label: label.to_string(),
            store: "PKDS".into(),
        })
    }

    /// Delete a key pair by label.
    pub fn delete(&mut self, label: &str) -> crate::Result<PkdsEntry> {
        self.entries.remove(label).ok_or_else(|| CryptoError::KeyNotFound {
            label: label.to_string(),
            store: "PKDS".into(),
        })
    }

    /// List all key pair labels.
    pub fn list(&self) -> Vec<&str> {
        let mut labels: Vec<&str> = self.entries.keys().map(String::as_str).collect();
        labels.sort();
        labels
    }

    /// Returns the number of entries.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Returns `true` if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

// ---------------------------------------------------------------------------
// Story 3: TKDS — Token Key Data Set (PKCS#11)
// ---------------------------------------------------------------------------

/// PKCS#11 token attribute name constants.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenAttribute {
    /// CKA_CLASS — object class.
    CkaClass,
    /// CKA_KEY_TYPE — key type.
    CkaKeyType,
    /// CKA_LABEL — human-readable label.
    CkaLabel,
    /// CKA_ENCRYPT — object can encrypt.
    CkaEncrypt,
    /// CKA_DECRYPT — object can decrypt.
    CkaDecrypt,
    /// CKA_SIGN — object can sign.
    CkaSign,
    /// CKA_VERIFY — object can verify.
    CkaVerify,
    /// CKA_WRAP — object can wrap keys.
    CkaWrap,
    /// CKA_UNWRAP — object can unwrap keys.
    CkaUnwrap,
    /// CKA_EXTRACTABLE — object can be extracted.
    CkaExtractable,
    /// CKA_TOKEN — persistent token object.
    CkaToken,
}

impl std::fmt::Display for TokenAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CkaClass => write!(f, "CKA_CLASS"),
            Self::CkaKeyType => write!(f, "CKA_KEY_TYPE"),
            Self::CkaLabel => write!(f, "CKA_LABEL"),
            Self::CkaEncrypt => write!(f, "CKA_ENCRYPT"),
            Self::CkaDecrypt => write!(f, "CKA_DECRYPT"),
            Self::CkaSign => write!(f, "CKA_SIGN"),
            Self::CkaVerify => write!(f, "CKA_VERIFY"),
            Self::CkaWrap => write!(f, "CKA_WRAP"),
            Self::CkaUnwrap => write!(f, "CKA_UNWRAP"),
            Self::CkaExtractable => write!(f, "CKA_EXTRACTABLE"),
            Self::CkaToken => write!(f, "CKA_TOKEN"),
        }
    }
}

/// An entry in the TKDS (PKCS#11 token key store).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TkdsEntry {
    /// Key bytes.
    pub key_bytes: Vec<u8>,
    /// Token attributes.
    pub attributes: HashMap<TokenAttribute, String>,
}

/// Token Key Data Set (TKDS).
///
/// Stores PKCS#11 token objects with attributes.
#[derive(Debug, Clone, Default)]
pub struct Tkds {
    /// Token handle to entry mapping.
    entries: HashMap<String, TkdsEntry>,
    /// Next handle counter.
    next_handle: u64,
}

impl Tkds {
    /// Create an empty TKDS.
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert a token object and return its handle.
    pub fn insert(
        &mut self,
        label: &str,
        key_bytes: Vec<u8>,
        attributes: HashMap<TokenAttribute, String>,
    ) -> crate::Result<String> {
        if self.entries.contains_key(label) {
            return Err(CryptoError::KeyExists {
                label: label.to_string(),
                store: "TKDS".into(),
            });
        }
        let entry = TkdsEntry {
            key_bytes,
            attributes,
        };
        self.entries.insert(label.to_string(), entry);
        self.next_handle += 1;
        Ok(label.to_string())
    }

    /// Retrieve a token object by label.
    pub fn get(&self, label: &str) -> crate::Result<&TkdsEntry> {
        self.entries.get(label).ok_or_else(|| CryptoError::KeyNotFound {
            label: label.to_string(),
            store: "TKDS".into(),
        })
    }

    /// Delete a token object by label.
    pub fn delete(&mut self, label: &str) -> crate::Result<TkdsEntry> {
        self.entries.remove(label).ok_or_else(|| CryptoError::KeyNotFound {
            label: label.to_string(),
            store: "TKDS".into(),
        })
    }

    /// List all token labels.
    pub fn list(&self) -> Vec<&str> {
        let mut labels: Vec<&str> = self.entries.keys().map(String::as_str).collect();
        labels.sort();
        labels
    }

    /// Returns the number of entries.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Returns `true` if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

// ---------------------------------------------------------------------------
// Story 4: Key Lifecycle
// ---------------------------------------------------------------------------

/// Key lifecycle operations.
///
/// Manages generation, deletion, and rekeying of keys across all stores.
#[derive(Debug)]
pub struct KeyLifecycle {
    /// The CKDS backing store.
    pub ckds: Ckds,
    /// The PKDS backing store.
    pub pkds: Pkds,
}

impl KeyLifecycle {
    /// Create a new key lifecycle manager with empty stores.
    pub fn new() -> Self {
        Self {
            ckds: Ckds::new(),
            pkds: Pkds::new(),
        }
    }

    /// Generate a symmetric key and store it in the CKDS.
    pub fn generate_symmetric(
        &mut self,
        label: &str,
        key_bytes: Vec<u8>,
    ) -> crate::Result<()> {
        self.ckds.insert(label, key_bytes)
    }

    /// Generate an asymmetric key pair and store it in the PKDS.
    pub fn generate_asymmetric(
        &mut self,
        label: &str,
        entry: PkdsEntry,
    ) -> crate::Result<()> {
        self.pkds.insert(label, entry)
    }

    /// Delete a symmetric key from the CKDS.
    pub fn delete_symmetric(&mut self, label: &str) -> crate::Result<()> {
        self.ckds.delete(label)?;
        Ok(())
    }

    /// Delete an asymmetric key pair from the PKDS.
    pub fn delete_asymmetric(&mut self, label: &str) -> crate::Result<()> {
        self.pkds.delete(label)?;
        Ok(())
    }

    /// Rekey: replace a symmetric key with a new one.
    pub fn rekey_symmetric(
        &mut self,
        label: &str,
        new_key_bytes: Vec<u8>,
    ) -> crate::Result<Vec<u8>> {
        let old = self.ckds.delete(label)?;
        self.ckds.insert(label, new_key_bytes)?;
        Ok(old)
    }
}

impl Default for KeyLifecycle {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Story 5: Master Key
// ---------------------------------------------------------------------------

/// Simulated master key for ICSF key store encryption.
///
/// Uses XOR-based wrapping/unwrapping (NOT cryptographically secure).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MasterKey {
    /// Master key bytes.
    bytes: Vec<u8>,
}

impl MasterKey {
    /// Create a new master key from raw bytes.
    pub fn new(bytes: Vec<u8>) -> Self {
        Self { bytes }
    }

    /// Returns the master key bytes.
    pub fn as_bytes(&self) -> &[u8] {
        &self.bytes
    }

    /// Wrap (encrypt) a key under this master key using XOR.
    pub fn wrap(&self, key_bytes: &[u8]) -> Vec<u8> {
        key_bytes
            .iter()
            .enumerate()
            .map(|(i, &b)| b ^ self.bytes[i % self.bytes.len()])
            .collect()
    }

    /// Unwrap (decrypt) a wrapped key under this master key using XOR.
    pub fn unwrap(&self, wrapped: &[u8]) -> Vec<u8> {
        // XOR is its own inverse.
        self.wrap(wrapped)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // --- CKDS ---

    #[test]
    fn ckds_insert_get_delete() {
        let mut store = Ckds::new();
        assert!(store.is_empty());
        store.insert("AES.KEY1", vec![0xAA; 16]).unwrap();
        assert_eq!(store.len(), 1);
        let key = store.get("AES.KEY1").unwrap();
        assert_eq!(key, &[0xAA; 16]);
        store.delete("AES.KEY1").unwrap();
        assert!(store.is_empty());
    }

    #[test]
    fn ckds_duplicate_insert_fails() {
        let mut store = Ckds::new();
        store.insert("KEY1", vec![1, 2, 3]).unwrap();
        assert!(store.insert("KEY1", vec![4, 5, 6]).is_err());
    }

    #[test]
    fn ckds_get_missing_fails() {
        let store = Ckds::new();
        assert!(store.get("NOPE").is_err());
    }

    #[test]
    fn ckds_delete_missing_fails() {
        let mut store = Ckds::new();
        assert!(store.delete("NOPE").is_err());
    }

    #[test]
    fn ckds_list_sorted() {
        let mut store = Ckds::new();
        store.insert("ZZZ", vec![1]).unwrap();
        store.insert("AAA", vec![2]).unwrap();
        store.insert("MMM", vec![3]).unwrap();
        let labels = store.list();
        assert_eq!(labels, vec!["AAA", "MMM", "ZZZ"]);
    }

    // --- PKDS ---

    #[test]
    fn pkds_insert_get_delete() {
        let mut store = Pkds::new();
        let entry = PkdsEntry {
            public_key: vec![1; 32],
            private_key: vec![2; 32],
            key_type: "EC-P256".into(),
        };
        store.insert("EC.KEY1", entry.clone()).unwrap();
        let got = store.get("EC.KEY1").unwrap();
        assert_eq!(got, &entry);
        store.delete("EC.KEY1").unwrap();
        assert!(store.is_empty());
    }

    #[test]
    fn pkds_duplicate_fails() {
        let mut store = Pkds::new();
        let entry = PkdsEntry {
            public_key: vec![1],
            private_key: vec![2],
            key_type: "RSA-2048".into(),
        };
        store.insert("KEY", entry.clone()).unwrap();
        assert!(store.insert("KEY", entry).is_err());
    }

    #[test]
    fn pkds_list_sorted() {
        let mut store = Pkds::new();
        let mut mk = |label: &str| {
            store
                .insert(
                    label,
                    PkdsEntry {
                        public_key: vec![1],
                        private_key: vec![2],
                        key_type: "RSA".into(),
                    },
                )
                .unwrap();
        };
        mk("B");
        mk("A");
        mk("C");
        assert_eq!(store.list(), vec!["A", "B", "C"]);
    }

    // --- TKDS ---

    #[test]
    fn tkds_insert_get_delete() {
        let mut store = Tkds::new();
        let mut attrs = HashMap::new();
        attrs.insert(TokenAttribute::CkaClass, "CKO_SECRET_KEY".into());
        attrs.insert(TokenAttribute::CkaKeyType, "CKK_AES".into());

        store.insert("TOKEN1", vec![0xBB; 32], attrs.clone()).unwrap();
        let entry = store.get("TOKEN1").unwrap();
        assert_eq!(entry.key_bytes, vec![0xBB; 32]);
        assert_eq!(entry.attributes.get(&TokenAttribute::CkaClass).unwrap(), "CKO_SECRET_KEY");

        store.delete("TOKEN1").unwrap();
        assert!(store.is_empty());
    }

    #[test]
    fn tkds_duplicate_fails() {
        let mut store = Tkds::new();
        store.insert("T1", vec![1], HashMap::new()).unwrap();
        assert!(store.insert("T1", vec![2], HashMap::new()).is_err());
    }

    // --- Key Lifecycle ---

    #[test]
    fn lifecycle_generate_delete_rekey() {
        let mut lc = KeyLifecycle::new();
        lc.generate_symmetric("SYM1", vec![0x11; 16]).unwrap();
        assert_eq!(lc.ckds.get("SYM1").unwrap(), &[0x11; 16]);

        let old = lc.rekey_symmetric("SYM1", vec![0x22; 16]).unwrap();
        assert_eq!(old, vec![0x11; 16]);
        assert_eq!(lc.ckds.get("SYM1").unwrap(), &[0x22; 16]);

        lc.delete_symmetric("SYM1").unwrap();
        assert!(lc.ckds.is_empty());
    }

    #[test]
    fn lifecycle_asymmetric() {
        let mut lc = KeyLifecycle::new();
        let entry = PkdsEntry {
            public_key: vec![1; 64],
            private_key: vec![2; 64],
            key_type: "EC-P256".into(),
        };
        lc.generate_asymmetric("ASYM1", entry).unwrap();
        assert_eq!(lc.pkds.len(), 1);
        lc.delete_asymmetric("ASYM1").unwrap();
        assert!(lc.pkds.is_empty());
    }

    // --- Master Key ---

    #[test]
    fn master_key_wrap_unwrap_round_trip() {
        let mk = MasterKey::new(vec![0xDE, 0xAD, 0xBE, 0xEF]);
        let original = vec![0x01, 0x02, 0x03, 0x04, 0x05, 0x06];
        let wrapped = mk.wrap(&original);
        assert_ne!(wrapped, original);
        let unwrapped = mk.unwrap(&wrapped);
        assert_eq!(unwrapped, original);
    }

    #[test]
    fn master_key_wrap_different_keys_differ() {
        let mk1 = MasterKey::new(vec![0x11; 4]);
        let mk2 = MasterKey::new(vec![0x22; 4]);
        let data = vec![0xFF; 8];
        assert_ne!(mk1.wrap(&data), mk2.wrap(&data));
    }

    #[test]
    fn master_key_empty_data() {
        let mk = MasterKey::new(vec![0xAA; 4]);
        let wrapped = mk.wrap(&[]);
        assert!(wrapped.is_empty());
    }

    // --- Token Attribute Display ---

    #[test]
    fn token_attribute_display() {
        assert_eq!(format!("{}", TokenAttribute::CkaClass), "CKA_CLASS");
        assert_eq!(format!("{}", TokenAttribute::CkaKeyType), "CKA_KEY_TYPE");
        assert_eq!(format!("{}", TokenAttribute::CkaEncrypt), "CKA_ENCRYPT");
    }
}
