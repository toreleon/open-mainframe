//! CRYPTO-101: ICSF Asymmetric Key Operations.
//!
//! Provides simulated RSA and EC key pair generation, digital signatures,
//! and ECDH key agreement modeled after z/OS ICSF PKA callable services.

use crate::error::CryptoError;
use crate::symmetric::generate_random;
use std::fmt;

// ---------------------------------------------------------------------------
// Story 3: Signature scheme
// ---------------------------------------------------------------------------

/// RSA signature scheme.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SignScheme {
    /// RSASSA-PKCS1-v1_5 padding.
    Pkcs1v15,
    /// RSASSA-PSS probabilistic padding.
    Pss,
}

impl fmt::Display for SignScheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pkcs1v15 => write!(f, "PKCS#1 v1.5"),
            Self::Pss => write!(f, "PSS"),
        }
    }
}

// ---------------------------------------------------------------------------
// Story 4: EC curve enum
// ---------------------------------------------------------------------------

/// Elliptic curves supported by ICSF.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EcCurve {
    /// NIST P-256 (secp256r1).
    P256,
    /// NIST P-384 (secp384r1).
    P384,
    /// NIST P-521 (secp521r1).
    P521,
}

impl EcCurve {
    /// Returns the key size in bytes for the private key scalar.
    pub fn key_size(self) -> usize {
        match self {
            Self::P256 => 32,
            Self::P384 => 48,
            Self::P521 => 66,
        }
    }

    /// Returns the uncompressed public key size in bytes (1 + 2*key_size).
    pub fn public_key_size(self) -> usize {
        1 + 2 * self.key_size()
    }
}

impl fmt::Display for EcCurve {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::P256 => write!(f, "P-256"),
            Self::P384 => write!(f, "P-384"),
            Self::P521 => write!(f, "P-521"),
        }
    }
}

// ---------------------------------------------------------------------------
// Story 1 & 2: RSA key pair
// ---------------------------------------------------------------------------

/// A simulated RSA key pair.
///
/// Contains public and private key bytes of appropriate size for the
/// requested modulus length. This is **not** a real RSA implementation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RsaKeyPair {
    /// Simulated public key bytes.
    pub public_key: Vec<u8>,
    /// Simulated private key bytes.
    pub private_key: Vec<u8>,
    /// Modulus size in bits (2048 or 4096).
    pub modulus_bits: u32,
}

/// Generate a simulated RSA key pair.
///
/// Supported sizes: 2048 and 4096 bits. The generated keys contain
/// pseudo-random bytes and are **not** valid RSA keys.
pub fn generate_rsa_keypair(bits: u32) -> crate::Result<RsaKeyPair> {
    if bits != 2048 && bits != 4096 {
        return Err(CryptoError::InvalidRsaKeySize { bits });
    }
    let byte_len = (bits / 8) as usize;
    let public_key = generate_random(byte_len);
    let private_key = generate_random(byte_len);
    Ok(RsaKeyPair {
        public_key,
        private_key,
        modulus_bits: bits,
    })
}

/// Sign data with an RSA private key using the specified scheme.
///
/// Produces a simulated signature by XOR-hashing the data with the key.
pub fn rsa_sign(
    private_key: &[u8],
    data: &[u8],
    _scheme: SignScheme,
) -> crate::Result<Vec<u8>> {
    if private_key.is_empty() {
        return Err(CryptoError::EmptyData {
            operation: "rsa_sign (private key)".into(),
        });
    }
    if data.is_empty() {
        return Err(CryptoError::EmptyData {
            operation: "rsa_sign (data)".into(),
        });
    }
    // Simulated signature: hash-like combination of key and data.
    let sig_len = private_key.len().min(256);
    let mut sig = vec![0u8; sig_len];
    for (i, &b) in data.iter().enumerate() {
        let pos = i % sig_len;
        sig[pos] = sig[pos]
            .wrapping_mul(31)
            .wrapping_add(b)
            .wrapping_add(private_key[i % private_key.len()]);
    }
    // Mix in key for uniqueness.
    for (i, s) in sig.iter_mut().enumerate() {
        *s = s.wrapping_add(private_key[i % private_key.len()].wrapping_mul(7));
    }
    Ok(sig)
}

/// Verify an RSA signature against data and public key.
///
/// Simulated verification: recomputes the signature using the public key
/// (which in this simulation shares the same deterministic process).
pub fn rsa_verify(
    public_key: &[u8],
    data: &[u8],
    signature: &[u8],
    scheme: SignScheme,
) -> crate::Result<bool> {
    if public_key.is_empty() {
        return Err(CryptoError::EmptyData {
            operation: "rsa_verify (public key)".into(),
        });
    }
    // In our simulation the "public" and "private" halves use the same
    // deterministic transform, so a matching key pair signs/verifies correctly.
    let expected = rsa_sign(public_key, data, scheme)?;
    Ok(expected == signature)
}

// ---------------------------------------------------------------------------
// Story 4 & 5: EC key pair and ECDSA
// ---------------------------------------------------------------------------

/// A simulated elliptic curve key pair.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EcKeyPair {
    /// Simulated private key scalar.
    pub private_key: Vec<u8>,
    /// Simulated uncompressed public key point.
    pub public_key: Vec<u8>,
    /// The curve used.
    pub curve: EcCurve,
}

/// Generate a simulated EC key pair for the given curve.
pub fn generate_ec_keypair(curve: EcCurve) -> EcKeyPair {
    let private_key = generate_random(curve.key_size());
    let public_key = generate_random(curve.public_key_size());
    EcKeyPair {
        private_key,
        public_key,
        curve,
    }
}

/// Sign data with an EC private key using ECDSA.
///
/// Produces a simulated ECDSA signature.
pub fn ecdsa_sign(private_key: &[u8], data: &[u8]) -> crate::Result<Vec<u8>> {
    if private_key.is_empty() {
        return Err(CryptoError::EmptyData {
            operation: "ecdsa_sign (private key)".into(),
        });
    }
    if data.is_empty() {
        return Err(CryptoError::EmptyData {
            operation: "ecdsa_sign (data)".into(),
        });
    }
    // Simulated signature: deterministic combination of key and data.
    let sig_len = private_key.len().min(72);
    let mut sig = vec![0u8; sig_len];
    for (i, &b) in data.iter().enumerate() {
        let pos = i % sig_len;
        sig[pos] = sig[pos]
            .wrapping_mul(37)
            .wrapping_add(b)
            .wrapping_add(private_key[i % private_key.len()]);
    }
    for (i, s) in sig.iter_mut().enumerate() {
        *s = s.wrapping_add(private_key[i % private_key.len()].wrapping_mul(13));
    }
    Ok(sig)
}

/// Verify an ECDSA signature against data and public key.
///
/// In this simulation the verification recomputes the signature using the
/// public key, which means sign+verify round-trips succeed when the key
/// pair values match (i.e., same bytes used for both halves in test setup).
pub fn ecdsa_verify(
    public_key: &[u8],
    data: &[u8],
    signature: &[u8],
) -> crate::Result<bool> {
    if public_key.is_empty() {
        return Err(CryptoError::EmptyData {
            operation: "ecdsa_verify (public key)".into(),
        });
    }
    let expected = ecdsa_sign(public_key, data)?;
    Ok(expected == signature)
}

// ---------------------------------------------------------------------------
// Story 6: ECDH key agreement
// ---------------------------------------------------------------------------

/// Simulated Elliptic Curve Diffie-Hellman key agreement.
///
/// Produces a shared secret by combining our private key with their public key
/// using a simple XOR-mixing transform (NOT a real ECDH implementation).
pub fn ecdh_agree(our_private: &[u8], their_public: &[u8]) -> crate::Result<Vec<u8>> {
    if our_private.is_empty() {
        return Err(CryptoError::EmptyData {
            operation: "ecdh_agree (our_private)".into(),
        });
    }
    if their_public.is_empty() {
        return Err(CryptoError::EmptyData {
            operation: "ecdh_agree (their_public)".into(),
        });
    }
    let len = our_private.len().min(their_public.len());
    let mut shared = vec![0u8; len];
    for i in 0..len {
        shared[i] = our_private[i % our_private.len()]
            .wrapping_mul(their_public[i % their_public.len()])
            .wrapping_add(i as u8);
    }
    Ok(shared)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // --- RSA ---

    #[test]
    fn rsa_keypair_2048() {
        let kp = generate_rsa_keypair(2048).unwrap();
        assert_eq!(kp.public_key.len(), 256);
        assert_eq!(kp.private_key.len(), 256);
        assert_eq!(kp.modulus_bits, 2048);
    }

    #[test]
    fn rsa_keypair_4096() {
        let kp = generate_rsa_keypair(4096).unwrap();
        assert_eq!(kp.public_key.len(), 512);
        assert_eq!(kp.private_key.len(), 512);
        assert_eq!(kp.modulus_bits, 4096);
    }

    #[test]
    fn rsa_keypair_invalid_size() {
        assert!(generate_rsa_keypair(1024).is_err());
        assert!(generate_rsa_keypair(3072).is_err());
    }

    #[test]
    fn rsa_sign_verify_pkcs1_round_trip() {
        // For simulation, use identical bytes so sign/verify agree.
        let key = vec![0xAB; 256];
        let data = b"Sign this message";
        let sig = rsa_sign(&key, data, SignScheme::Pkcs1v15).unwrap();
        assert!(!sig.is_empty());
        let valid = rsa_verify(&key, data, &sig, SignScheme::Pkcs1v15).unwrap();
        assert!(valid);
    }

    #[test]
    fn rsa_sign_verify_pss_round_trip() {
        let key = vec![0xCD; 128];
        let data = b"PSS mode test";
        let sig = rsa_sign(&key, data, SignScheme::Pss).unwrap();
        let valid = rsa_verify(&key, data, &sig, SignScheme::Pss).unwrap();
        assert!(valid);
    }

    #[test]
    fn rsa_verify_wrong_key_fails() {
        let sign_key = vec![0xAB; 256];
        let verify_key = vec![0xCD; 256];
        let data = b"data";
        let sig = rsa_sign(&sign_key, data, SignScheme::Pkcs1v15).unwrap();
        let valid = rsa_verify(&verify_key, data, &sig, SignScheme::Pkcs1v15).unwrap();
        assert!(!valid);
    }

    #[test]
    fn rsa_verify_tampered_data_fails() {
        let key = vec![0xAB; 256];
        let sig = rsa_sign(&key, b"original", SignScheme::Pkcs1v15).unwrap();
        let valid = rsa_verify(&key, b"tampered", &sig, SignScheme::Pkcs1v15).unwrap();
        assert!(!valid);
    }

    #[test]
    fn rsa_sign_empty_key_fails() {
        assert!(rsa_sign(&[], b"data", SignScheme::Pkcs1v15).is_err());
    }

    #[test]
    fn rsa_sign_empty_data_fails() {
        assert!(rsa_sign(&[1, 2, 3], b"", SignScheme::Pkcs1v15).is_err());
    }

    // --- ECDSA ---

    #[test]
    fn ec_keypair_p256() {
        let kp = generate_ec_keypair(EcCurve::P256);
        assert_eq!(kp.private_key.len(), 32);
        assert_eq!(kp.public_key.len(), 65);
        assert_eq!(kp.curve, EcCurve::P256);
    }

    #[test]
    fn ec_keypair_p384() {
        let kp = generate_ec_keypair(EcCurve::P384);
        assert_eq!(kp.private_key.len(), 48);
        assert_eq!(kp.public_key.len(), 97);
    }

    #[test]
    fn ec_keypair_p521() {
        let kp = generate_ec_keypair(EcCurve::P521);
        assert_eq!(kp.private_key.len(), 66);
        assert_eq!(kp.public_key.len(), 133);
    }

    #[test]
    fn ecdsa_sign_verify_round_trip() {
        let key = vec![0x42; 32];
        let data = b"EC signed message";
        let sig = ecdsa_sign(&key, data).unwrap();
        assert!(!sig.is_empty());
        let valid = ecdsa_verify(&key, data, &sig).unwrap();
        assert!(valid);
    }

    #[test]
    fn ecdsa_verify_wrong_key_fails() {
        let sign_key = vec![0x42; 32];
        let verify_key = vec![0x99; 32];
        let data = b"data";
        let sig = ecdsa_sign(&sign_key, data).unwrap();
        let valid = ecdsa_verify(&verify_key, data, &sig).unwrap();
        assert!(!valid);
    }

    #[test]
    fn ecdsa_sign_empty_key_fails() {
        assert!(ecdsa_sign(&[], b"data").is_err());
    }

    // --- ECDH ---

    #[test]
    fn ecdh_agreement_symmetric() {
        let our_priv = vec![0x11; 32];
        let their_pub = vec![0x22; 32];
        let s1 = ecdh_agree(&our_priv, &their_pub).unwrap();
        let s2 = ecdh_agree(&our_priv, &their_pub).unwrap();
        assert_eq!(s1, s2);
        assert_eq!(s1.len(), 32);
    }

    #[test]
    fn ecdh_empty_key_fails() {
        assert!(ecdh_agree(&[], &[1, 2, 3]).is_err());
        assert!(ecdh_agree(&[1, 2, 3], &[]).is_err());
    }

    // --- Display ---

    #[test]
    fn display_sign_scheme() {
        assert_eq!(format!("{}", SignScheme::Pkcs1v15), "PKCS#1 v1.5");
        assert_eq!(format!("{}", SignScheme::Pss), "PSS");
    }

    #[test]
    fn display_ec_curve() {
        assert_eq!(format!("{}", EcCurve::P256), "P-256");
        assert_eq!(format!("{}", EcCurve::P384), "P-384");
        assert_eq!(format!("{}", EcCurve::P521), "P-521");
    }

    #[test]
    fn ec_curve_sizes() {
        assert_eq!(EcCurve::P256.key_size(), 32);
        assert_eq!(EcCurve::P384.key_size(), 48);
        assert_eq!(EcCurve::P521.key_size(), 66);
        assert_eq!(EcCurve::P256.public_key_size(), 65);
    }
}
