//! CRYPTO-100: ICSF Symmetric & Hashing Operations.
//!
//! Provides simulated symmetric encryption/decryption, hashing, HMAC, and
//! pseudo-random number generation modeled after z/OS ICSF callable services.

use crate::error::CryptoError;
use std::fmt;

// ---------------------------------------------------------------------------
// Story 1: Algorithm enums
// ---------------------------------------------------------------------------

/// Symmetric encryption algorithms supported by ICSF.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymmetricAlgorithm {
    /// AES with a 128-bit key.
    Aes128,
    /// AES with a 192-bit key.
    Aes192,
    /// AES with a 256-bit key.
    Aes256,
    /// Triple-DES (3DES) with a 192-bit key.
    TripleDes,
}

impl SymmetricAlgorithm {
    /// Returns the expected key length in bytes for this algorithm.
    pub fn key_len(self) -> usize {
        match self {
            Self::Aes128 => 16,
            Self::Aes192 => 24,
            Self::Aes256 => 32,
            Self::TripleDes => 24,
        }
    }
}

impl fmt::Display for SymmetricAlgorithm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Aes128 => write!(f, "AES-128"),
            Self::Aes192 => write!(f, "AES-192"),
            Self::Aes256 => write!(f, "AES-256"),
            Self::TripleDes => write!(f, "3DES"),
        }
    }
}

/// Cipher block modes of operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CipherMode {
    /// Cipher Block Chaining.
    Cbc,
    /// Electronic Codebook.
    Ecb,
    /// Counter mode.
    Ctr,
}

impl fmt::Display for CipherMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Cbc => write!(f, "CBC"),
            Self::Ecb => write!(f, "ECB"),
            Self::Ctr => write!(f, "CTR"),
        }
    }
}

// ---------------------------------------------------------------------------
// Story 7: ICSF return/reason code struct
// ---------------------------------------------------------------------------

/// Result from an ICSF callable service, carrying return/reason codes and
/// optional output data.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IcsfResult {
    /// ICSF return code (0 = success, 4 = warning, 8+ = error).
    pub return_code: i32,
    /// ICSF reason code providing additional detail.
    pub reason_code: i32,
    /// Output data (ciphertext, plaintext, hash, etc.).
    pub data: Vec<u8>,
}

impl IcsfResult {
    /// Create a successful result with data.
    pub fn ok(data: Vec<u8>) -> Self {
        Self {
            return_code: 0,
            reason_code: 0,
            data,
        }
    }

    /// Create an error result.
    pub fn err(return_code: i32, reason_code: i32) -> Self {
        Self {
            return_code,
            reason_code,
            data: Vec::new(),
        }
    }

    /// Returns `true` if the service completed successfully.
    pub fn is_ok(&self) -> bool {
        self.return_code == 0
    }
}

// ---------------------------------------------------------------------------
// Story 2: Symmetric key + encrypt/decrypt
// ---------------------------------------------------------------------------

/// An ICSF-managed symmetric key wrapper.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IcsfSymmetricKey {
    /// Raw key bytes.
    bytes: Vec<u8>,
}

impl IcsfSymmetricKey {
    /// Create a new symmetric key from raw bytes.
    pub fn new(bytes: Vec<u8>) -> Self {
        Self { bytes }
    }

    /// Returns the key bytes.
    pub fn as_bytes(&self) -> &[u8] {
        &self.bytes
    }

    /// Returns the key length in bytes.
    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    /// Returns `true` if the key is empty.
    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }
}

/// XOR-based simulated encryption (demonstration only, NOT cryptographically secure).
fn xor_transform(key: &[u8], data: &[u8]) -> Vec<u8> {
    data.iter()
        .enumerate()
        .map(|(i, &b)| b ^ key[i % key.len()])
        .collect()
}

/// Encrypt plaintext using a symmetric key, algorithm, and mode.
///
/// This is a simulated implementation using XOR-based transformation.
/// It is **not** cryptographically secure and is intended for demonstration.
pub fn encrypt(
    key: &IcsfSymmetricKey,
    plaintext: &[u8],
    algorithm: SymmetricAlgorithm,
    _mode: CipherMode,
) -> crate::Result<IcsfResult> {
    let expected = algorithm.key_len();
    if key.len() != expected {
        return Err(CryptoError::InvalidKeyLength {
            algorithm: algorithm.to_string(),
            expected,
            actual: key.len(),
        });
    }
    if plaintext.is_empty() {
        return Err(CryptoError::EmptyData {
            operation: "encrypt".into(),
        });
    }
    let ciphertext = xor_transform(key.as_bytes(), plaintext);
    Ok(IcsfResult::ok(ciphertext))
}

/// Decrypt ciphertext using a symmetric key, algorithm, and mode.
///
/// This is a simulated implementation using XOR-based transformation.
pub fn decrypt(
    key: &IcsfSymmetricKey,
    ciphertext: &[u8],
    algorithm: SymmetricAlgorithm,
    _mode: CipherMode,
) -> crate::Result<IcsfResult> {
    let expected = algorithm.key_len();
    if key.len() != expected {
        return Err(CryptoError::InvalidKeyLength {
            algorithm: algorithm.to_string(),
            expected,
            actual: key.len(),
        });
    }
    if ciphertext.is_empty() {
        return Err(CryptoError::EmptyData {
            operation: "decrypt".into(),
        });
    }
    // XOR is its own inverse.
    let plaintext = xor_transform(key.as_bytes(), ciphertext);
    Ok(IcsfResult::ok(plaintext))
}

// ---------------------------------------------------------------------------
// Story 3: Hash algorithms + one_way_hash
// ---------------------------------------------------------------------------

/// Hash algorithms supported by ICSF.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HashAlgorithm {
    /// SHA-256 (256-bit / 32-byte digest).
    Sha256,
    /// SHA-384 (384-bit / 48-byte digest).
    Sha384,
    /// SHA-512 (512-bit / 64-byte digest).
    Sha512,
}

impl HashAlgorithm {
    /// Returns the digest length in bytes.
    pub fn digest_len(self) -> usize {
        match self {
            Self::Sha256 => 32,
            Self::Sha384 => 48,
            Self::Sha512 => 64,
        }
    }
}

impl fmt::Display for HashAlgorithm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Sha256 => write!(f, "SHA-256"),
            Self::Sha384 => write!(f, "SHA-384"),
            Self::Sha512 => write!(f, "SHA-512"),
        }
    }
}

/// Simulated Merkle-Damgard-style hash (NOT cryptographically secure).
///
/// Produces a digest of the length specified by `algorithm`. This is a
/// deterministic, consistent hash simulation for demonstration purposes.
pub fn one_way_hash(algorithm: HashAlgorithm, data: &[u8]) -> Vec<u8> {
    let digest_len = algorithm.digest_len();
    let mut state = vec![0u8; digest_len];

    // Initialize state with algorithm-specific constants.
    for (i, byte) in state.iter_mut().enumerate() {
        *byte = (digest_len as u8).wrapping_add(i as u8);
    }

    // Process each byte of input in a Merkle-Damgard-like fashion.
    for (idx, &b) in data.iter().enumerate() {
        let pos = idx % digest_len;
        state[pos] = state[pos]
            .wrapping_mul(31)
            .wrapping_add(b)
            .wrapping_add(idx as u8);
        // Cascade to the next position for diffusion.
        let next = (pos + 1) % digest_len;
        state[next] = state[next].wrapping_add(state[pos].wrapping_mul(7));
    }

    // Finalization pass for avalanche.
    for round in 0..4 {
        for i in 0..digest_len {
            let prev = if i == 0 { digest_len - 1 } else { i - 1 };
            state[i] = state[i]
                .wrapping_mul(17)
                .wrapping_add(state[prev])
                .wrapping_add(round);
        }
    }

    state
}

// ---------------------------------------------------------------------------
// Story 4: HMAC
// ---------------------------------------------------------------------------

/// Generate an HMAC tag for the given data.
///
/// Uses a simplified HMAC construction: `hash(key XOR opad || hash(key XOR ipad || data))`.
pub fn hmac_generate(algorithm: HashAlgorithm, key: &[u8], data: &[u8]) -> Vec<u8> {
    let block_size = algorithm.digest_len();

    // Prepare padded key.
    let mut padded_key = vec![0u8; block_size];
    if key.len() > block_size {
        let hk = one_way_hash(algorithm, key);
        padded_key[..hk.len()].copy_from_slice(&hk);
    } else {
        padded_key[..key.len()].copy_from_slice(key);
    }

    // Inner: key XOR ipad (0x36), then concatenate data.
    let mut inner_input: Vec<u8> = padded_key.iter().map(|b| b ^ 0x36).collect();
    inner_input.extend_from_slice(data);
    let inner_hash = one_way_hash(algorithm, &inner_input);

    // Outer: key XOR opad (0x5c), then concatenate inner hash.
    let mut outer_input: Vec<u8> = padded_key.iter().map(|b| b ^ 0x5c).collect();
    outer_input.extend_from_slice(&inner_hash);

    one_way_hash(algorithm, &outer_input)
}

/// Verify an HMAC tag for the given data.
///
/// Returns `true` if the computed HMAC matches the provided `mac`.
pub fn hmac_verify(algorithm: HashAlgorithm, key: &[u8], data: &[u8], mac: &[u8]) -> bool {
    let computed = hmac_generate(algorithm, key, data);
    // Constant-time-ish comparison (no short-circuit, but not timing-safe in
    // a real sense since this is a simulator).
    if computed.len() != mac.len() {
        return false;
    }
    let mut diff = 0u8;
    for (a, b) in computed.iter().zip(mac.iter()) {
        diff |= a ^ b;
    }
    diff == 0
}

// ---------------------------------------------------------------------------
// Story 5: PRNG
// ---------------------------------------------------------------------------

/// Generate pseudo-random bytes using a simple LCG-based PRNG.
///
/// This is **not** cryptographically secure. Uses a counter-based approach
/// seeded from a deterministic LCG for reproducible test output.
pub fn generate_random(length: usize) -> Vec<u8> {
    // Simple LCG: state = state * 6364136223846793005 + 1442695040888963407
    // Seed derived from a compile-time constant to be deterministic per call.
    let mut state: u64 = 0x5EED_CAFE_BABE_F00D;
    let mut result = Vec::with_capacity(length);
    for i in 0..length {
        state = state
            .wrapping_mul(6_364_136_223_846_793_005)
            .wrapping_add(1_442_695_040_888_963_407)
            .wrapping_add(i as u64);
        result.push((state >> 33) as u8);
    }
    result
}

// ---------------------------------------------------------------------------
// Story 6: ICSF Configuration
// ---------------------------------------------------------------------------

/// ICSF installation configuration (CSFPRMxx parmlib member).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IcsfConfig {
    /// Cryptographic Key Data Set name (e.g., `CSF.CKDS`).
    pub ckds_dataset: String,
    /// Public Key Data Set name (e.g., `CSF.PKDS`).
    pub pkds_dataset: String,
    /// Whether FIPS 140-2 mode is active.
    pub fips_mode: bool,
}

impl IcsfConfig {
    /// Parse an ICSF configuration from CSFPRMxx-style lines.
    ///
    /// Expected format (one setting per line):
    /// ```text
    /// CKDSN(CSF.CKDS)
    /// PKDSN(CSF.PKDS)
    /// FIPSMODE(YES)
    /// ```
    pub fn parse(input: &str) -> crate::Result<Self> {
        let mut ckds = None;
        let mut pkds = None;
        let mut fips = false;

        for line in input.lines() {
            let trimmed = line.trim();
            if let Some(inner) = Self::extract_paren(trimmed, "CKDSN") {
                ckds = Some(inner.to_string());
            } else if let Some(inner) = Self::extract_paren(trimmed, "PKDSN") {
                pkds = Some(inner.to_string());
            } else if let Some(inner) = Self::extract_paren(trimmed, "FIPSMODE") {
                fips = inner.eq_ignore_ascii_case("YES");
            }
        }

        let ckds_dataset = ckds.ok_or_else(|| CryptoError::InvalidConfig {
            message: "missing CKDSN parameter".into(),
        })?;
        let pkds_dataset = pkds.ok_or_else(|| CryptoError::InvalidConfig {
            message: "missing PKDSN parameter".into(),
        })?;

        Ok(Self {
            ckds_dataset,
            pkds_dataset,
            fips_mode: fips,
        })
    }

    /// Extract the value between parentheses for a keyword, e.g. `CKDSN(CSF.CKDS)`.
    fn extract_paren<'a>(line: &'a str, keyword: &str) -> Option<&'a str> {
        let upper = line.to_ascii_uppercase();
        if let Some(start) = upper.find(&keyword.to_ascii_uppercase()) {
            let rest = &line[start + keyword.len()..];
            if rest.starts_with('(') {
                if let Some(end) = rest.find(')') {
                    return Some(&rest[1..end]);
                }
            }
        }
        None
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // --- Encrypt / Decrypt round-trip ---

    #[test]
    fn encrypt_decrypt_aes128_cbc() {
        let key = IcsfSymmetricKey::new(vec![0xAA; 16]);
        let plaintext = b"Hello, ICSF world!";
        let enc = encrypt(&key, plaintext, SymmetricAlgorithm::Aes128, CipherMode::Cbc).unwrap();
        assert!(enc.is_ok());
        assert_ne!(enc.data, plaintext);

        let dec = decrypt(&key, &enc.data, SymmetricAlgorithm::Aes128, CipherMode::Cbc).unwrap();
        assert!(dec.is_ok());
        assert_eq!(dec.data, plaintext);
    }

    #[test]
    fn encrypt_decrypt_aes256_ecb() {
        let key = IcsfSymmetricKey::new(vec![0x55; 32]);
        let plaintext = b"AES-256 ECB test data";
        let enc = encrypt(&key, plaintext, SymmetricAlgorithm::Aes256, CipherMode::Ecb).unwrap();
        let dec = decrypt(&key, &enc.data, SymmetricAlgorithm::Aes256, CipherMode::Ecb).unwrap();
        assert_eq!(dec.data, plaintext);
    }

    #[test]
    fn encrypt_decrypt_triple_des_ctr() {
        let key = IcsfSymmetricKey::new(vec![0xBB; 24]);
        let plaintext = b"3DES CTR mode data";
        let enc = encrypt(&key, plaintext, SymmetricAlgorithm::TripleDes, CipherMode::Ctr).unwrap();
        let dec = decrypt(&key, &enc.data, SymmetricAlgorithm::TripleDes, CipherMode::Ctr).unwrap();
        assert_eq!(dec.data, plaintext);
    }

    #[test]
    fn encrypt_wrong_key_length_fails() {
        let key = IcsfSymmetricKey::new(vec![0xAA; 10]);
        let result = encrypt(&key, b"data", SymmetricAlgorithm::Aes128, CipherMode::Cbc);
        assert!(result.is_err());
    }

    #[test]
    fn encrypt_empty_data_fails() {
        let key = IcsfSymmetricKey::new(vec![0xAA; 16]);
        let result = encrypt(&key, b"", SymmetricAlgorithm::Aes128, CipherMode::Cbc);
        assert!(result.is_err());
    }

    // --- Hash consistency ---

    #[test]
    fn hash_sha256_consistent() {
        let data = b"test data for hashing";
        let h1 = one_way_hash(HashAlgorithm::Sha256, data);
        let h2 = one_way_hash(HashAlgorithm::Sha256, data);
        assert_eq!(h1, h2);
        assert_eq!(h1.len(), 32);
    }

    #[test]
    fn hash_sha384_length() {
        let h = one_way_hash(HashAlgorithm::Sha384, b"data");
        assert_eq!(h.len(), 48);
    }

    #[test]
    fn hash_sha512_length() {
        let h = one_way_hash(HashAlgorithm::Sha512, b"data");
        assert_eq!(h.len(), 64);
    }

    #[test]
    fn hash_different_data_different_hash() {
        let h1 = one_way_hash(HashAlgorithm::Sha256, b"data1");
        let h2 = one_way_hash(HashAlgorithm::Sha256, b"data2");
        assert_ne!(h1, h2);
    }

    #[test]
    fn hash_different_algorithms_different_hash() {
        let data = b"same data";
        let h256 = one_way_hash(HashAlgorithm::Sha256, data);
        let h512 = one_way_hash(HashAlgorithm::Sha512, data);
        assert_ne!(h256.len(), h512.len());
    }

    // --- HMAC ---

    #[test]
    fn hmac_verify_valid() {
        let key = b"secret key";
        let data = b"message to authenticate";
        let mac = hmac_generate(HashAlgorithm::Sha256, key, data);
        assert!(hmac_verify(HashAlgorithm::Sha256, key, data, &mac));
    }

    #[test]
    fn hmac_verify_wrong_key() {
        let key = b"secret key";
        let data = b"message";
        let mac = hmac_generate(HashAlgorithm::Sha256, key, data);
        assert!(!hmac_verify(HashAlgorithm::Sha256, b"wrong key", data, &mac));
    }

    #[test]
    fn hmac_verify_tampered_data() {
        let key = b"key";
        let mac = hmac_generate(HashAlgorithm::Sha256, key, b"original");
        assert!(!hmac_verify(HashAlgorithm::Sha256, key, b"tampered", &mac));
    }

    #[test]
    fn hmac_consistency() {
        let key = b"key";
        let data = b"data";
        let m1 = hmac_generate(HashAlgorithm::Sha384, key, data);
        let m2 = hmac_generate(HashAlgorithm::Sha384, key, data);
        assert_eq!(m1, m2);
    }

    // --- PRNG ---

    #[test]
    fn generate_random_correct_length() {
        let r = generate_random(64);
        assert_eq!(r.len(), 64);
    }

    #[test]
    fn generate_random_zero_length() {
        let r = generate_random(0);
        assert!(r.is_empty());
    }

    // --- Config ---

    #[test]
    fn parse_csfprmxx_config() {
        let input = "CKDSN(CSF.CKDS)\nPKDSN(CSF.PKDS)\nFIPSMODE(YES)\n";
        let cfg = IcsfConfig::parse(input).unwrap();
        assert_eq!(cfg.ckds_dataset, "CSF.CKDS");
        assert_eq!(cfg.pkds_dataset, "CSF.PKDS");
        assert!(cfg.fips_mode);
    }

    #[test]
    fn parse_config_fips_no() {
        let input = "CKDSN(MY.CKDS)\nPKDSN(MY.PKDS)\nFIPSMODE(NO)\n";
        let cfg = IcsfConfig::parse(input).unwrap();
        assert!(!cfg.fips_mode);
    }

    #[test]
    fn parse_config_missing_ckds_fails() {
        let input = "PKDSN(CSF.PKDS)\n";
        let result = IcsfConfig::parse(input);
        assert!(result.is_err());
    }

    // --- ICSF Result ---

    #[test]
    fn icsf_result_ok() {
        let r = IcsfResult::ok(vec![1, 2, 3]);
        assert!(r.is_ok());
        assert_eq!(r.data, vec![1, 2, 3]);
    }

    #[test]
    fn icsf_result_err() {
        let r = IcsfResult::err(8, 4004);
        assert!(!r.is_ok());
        assert!(r.data.is_empty());
    }

    // --- Algorithm key_len ---

    #[test]
    fn algorithm_key_lengths() {
        assert_eq!(SymmetricAlgorithm::Aes128.key_len(), 16);
        assert_eq!(SymmetricAlgorithm::Aes192.key_len(), 24);
        assert_eq!(SymmetricAlgorithm::Aes256.key_len(), 32);
        assert_eq!(SymmetricAlgorithm::TripleDes.key_len(), 24);
    }

    // --- Display impls ---

    #[test]
    fn display_symmetric_algorithm() {
        assert_eq!(format!("{}", SymmetricAlgorithm::Aes128), "AES-128");
        assert_eq!(format!("{}", SymmetricAlgorithm::TripleDes), "3DES");
    }

    #[test]
    fn display_cipher_mode() {
        assert_eq!(format!("{}", CipherMode::Cbc), "CBC");
        assert_eq!(format!("{}", CipherMode::Ecb), "ECB");
        assert_eq!(format!("{}", CipherMode::Ctr), "CTR");
    }

    #[test]
    fn display_hash_algorithm() {
        assert_eq!(format!("{}", HashAlgorithm::Sha256), "SHA-256");
        assert_eq!(format!("{}", HashAlgorithm::Sha384), "SHA-384");
        assert_eq!(format!("{}", HashAlgorithm::Sha512), "SHA-512");
    }

    #[test]
    fn symmetric_key_is_empty() {
        let empty = IcsfSymmetricKey::new(vec![]);
        assert!(empty.is_empty());
        let key = IcsfSymmetricKey::new(vec![1, 2, 3]);
        assert!(!key.is_empty());
    }
}
