//! EUSRIDPWD (SECMEC 0x0009) — Encrypted User ID and Password.
//!
//! Implements the server side of the DRDA DH key exchange + DES/CBC
//! encryption used by ibm_db / CLI ODBC driver.
//!
//! Algorithm:
//! 1. Client sends ACCSEC with SECTKN (32-byte DH public key)
//! 2. Server generates DH keypair, responds with its public key
//! 3. Both sides compute shared secret via DH
//! 4. DES key = shared_secret[12..20], IV = server_public[12..20]
//! 5. Client encrypts userid/password with DES/CBC/PKCS5 and sends in SECCHK
//! 6. Server decrypts using the same key/IV

use crate::error::{DrdaError, DrdaResult};

use num_bigint::BigUint;
use rand::Rng;

/// DRDA-specified 256-bit DH prime.
const PRIME: [u8; 32] = [
    0xC6, 0x21, 0x12, 0xD7, 0x3E, 0xE6, 0x13, 0xF0,
    0x94, 0x7A, 0xB3, 0x1F, 0x0F, 0x68, 0x46, 0xA1,
    0xBF, 0xF5, 0xB3, 0xA4, 0xCA, 0x0D, 0x60, 0xBC,
    0x1E, 0x4C, 0x7A, 0x0D, 0x8C, 0x16, 0xB3, 0xE3,
];

/// DRDA-specified 256-bit DH generator.
const GENERATOR: [u8; 32] = [
    0x46, 0x90, 0xFA, 0x1F, 0x7B, 0x9E, 0x1D, 0x44,
    0x42, 0xC8, 0x6C, 0x91, 0x14, 0x60, 0x3F, 0xDE,
    0xCF, 0x07, 0x1E, 0xDC, 0xEC, 0x5F, 0x62, 0x6E,
    0x21, 0xE2, 0x56, 0xAE, 0xD9, 0xEA, 0x34, 0xE4,
];

/// Server-side DH state for EUSRIDPWD.
#[derive(Debug)]
pub struct EusridpwdState {
    /// Server's DH private key.
    server_private: BigUint,
    /// Server's DH public key (32 bytes, big-endian).
    pub server_public: [u8; 32],
    /// Client's DH public key (32 bytes, received from ACCSEC).
    pub client_public: [u8; 32],
}

impl EusridpwdState {
    /// Create a new EUSRIDPWD state with a generated server keypair.
    pub fn new(client_public_key: &[u8]) -> Self {
        let p = BigUint::from_bytes_be(&PRIME);
        let g = BigUint::from_bytes_be(&GENERATOR);

        // Generate random private key (255 bits)
        let mut rng = rand::thread_rng();
        let mut priv_bytes = [0u8; 32];
        rng.fill(&mut priv_bytes);
        // Ensure it's in range [2, p-1)
        priv_bytes[0] &= 0x7F; // Clear top bit to stay under p
        let server_private = BigUint::from_bytes_be(&priv_bytes);

        // Compute server public key
        let server_public_int = g.modpow(&server_private, &p);
        let server_public = normalize_to_32_bytes(&server_public_int);

        // Store client public key
        let mut client_public = [0u8; 32];
        let len = client_public_key.len().min(32);
        let offset = 32 - len;
        client_public[offset..].copy_from_slice(&client_public_key[..len]);

        Self {
            server_private,
            server_public,
            client_public,
        }
    }

    /// Decrypt the encrypted userid and password from SECCHK.
    ///
    /// Returns (userid_bytes, password_bytes) in EBCDIC cp500 encoding.
    pub fn decrypt_credentials(
        &self,
        encrypted_userid: &[u8],
        encrypted_password: &[u8],
    ) -> DrdaResult<(Vec<u8>, Vec<u8>)> {
        let p = BigUint::from_bytes_be(&PRIME);
        let client_pub = BigUint::from_bytes_be(&self.client_public);

        // Compute DH shared secret
        let shared_secret_int = client_pub.modpow(&self.server_private, &p);
        let shared_secret = normalize_to_32_bytes(&shared_secret_int);

        // DES key: shared_secret[12..20]
        let des_key: [u8; 8] = shared_secret[12..20]
            .try_into()
            .map_err(|_| DrdaError::Protocol("Invalid shared secret length".to_string()))?;

        // IV: server's OWN public key[12..20]
        let iv: [u8; 8] = self.server_public[12..20]
            .try_into()
            .map_err(|_| DrdaError::Protocol("Invalid server public key length".to_string()))?;

        tracing::debug!(
            des_key_hex = %hex_bytes(&des_key),
            iv_hex = %hex_bytes(&iv),
            shared_secret_hex = %hex_bytes(&shared_secret[..16]),
            "EUSRIDPWD decryption params"
        );

        // Decrypt userid
        let userid = des_cbc_decrypt(&des_key, &iv, encrypted_userid)?;

        // Decrypt password (fresh cipher context)
        let password = des_cbc_decrypt(&des_key, &iv, encrypted_password)?;

        Ok((userid, password))
    }
}

/// DES/CBC/PKCS5 decryption.
fn des_cbc_decrypt(key: &[u8; 8], iv: &[u8; 8], ciphertext: &[u8]) -> DrdaResult<Vec<u8>> {
    use cbc::cipher::{BlockDecryptMut, KeyIvInit};
    use des::Des;

    type DesCbcDec = cbc::Decryptor<Des>;

    let mut buf = ciphertext.to_vec();
    let decryptor = DesCbcDec::new(key.into(), iv.into());

    // Decrypt with PKCS5/PKCS7 unpadding
    let plaintext = decryptor
        .decrypt_padded_mut::<cbc::cipher::block_padding::Pkcs7>(&mut buf)
        .map_err(|e| DrdaError::Protocol(format!("DES decryption failed: {}", e)))?;

    Ok(plaintext.to_vec())
}

/// Normalize a BigUint to exactly 32 bytes big-endian.
fn normalize_to_32_bytes(value: &BigUint) -> [u8; 32] {
    let bytes = value.to_bytes_be();
    let mut result = [0u8; 32];
    if bytes.len() == 33 && bytes[0] == 0 {
        result.copy_from_slice(&bytes[1..33]);
    } else if bytes.len() <= 32 {
        let offset = 32 - bytes.len();
        result[offset..].copy_from_slice(&bytes);
    } else {
        result.copy_from_slice(&bytes[bytes.len() - 32..]);
    }
    result
}

fn hex_bytes(b: &[u8]) -> String {
    b.iter().map(|x| format!("{:02x}", x)).collect::<Vec<_>>().join("")
}
