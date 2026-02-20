//! Minimal JWT (JSON Web Token) implementation using HMAC-SHA256.
//!
//! Implements HS256 JWT generation and validation without the `jsonwebtoken` crate
//! (which requires the `time` crate incompatible with MSRV 1.82).

use base64::Engine;
use hmac::{Hmac, Mac};
use serde::{Deserialize, Serialize};
use sha2::Sha256;

type HmacSha256 = Hmac<Sha256>;

/// JWT claims payload.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Claims {
    /// Subject (userid).
    pub sub: String,
    /// Issued at (seconds since epoch).
    pub iat: u64,
    /// Expiration (seconds since epoch).
    pub exp: u64,
}

/// JWT header (always HS256).
#[derive(Debug, Serialize, Deserialize)]
struct Header {
    alg: String,
    typ: String,
}

/// Encode and sign a JWT token.
pub fn encode(claims: &Claims, secret: &str) -> String {
    let header = Header {
        alg: "HS256".to_string(),
        typ: "JWT".to_string(),
    };

    let b64 = base64::engine::general_purpose::URL_SAFE_NO_PAD;

    let header_json = serde_json::to_string(&header).expect("header serialization");
    let claims_json = serde_json::to_string(claims).expect("claims serialization");

    let header_b64 = b64.encode(header_json.as_bytes());
    let claims_b64 = b64.encode(claims_json.as_bytes());

    let signing_input = format!("{}.{}", header_b64, claims_b64);

    let mut mac = HmacSha256::new_from_slice(secret.as_bytes())
        .expect("HMAC accepts any key length");
    mac.update(signing_input.as_bytes());
    let signature = mac.finalize().into_bytes();
    let signature_b64 = b64.encode(signature);

    format!("{}.{}", signing_input, signature_b64)
}

/// Decode and verify a JWT token.
pub fn decode(token: &str, secret: &str) -> std::result::Result<Claims, JwtError> {
    let b64 = base64::engine::general_purpose::URL_SAFE_NO_PAD;

    let parts: Vec<&str> = token.split('.').collect();
    if parts.len() != 3 {
        return Err(JwtError::InvalidFormat);
    }

    let signing_input = format!("{}.{}", parts[0], parts[1]);

    // Verify signature.
    let mut mac = HmacSha256::new_from_slice(secret.as_bytes())
        .map_err(|_| JwtError::InvalidSignature)?;
    mac.update(signing_input.as_bytes());

    let signature_bytes = b64.decode(parts[2]).map_err(|_| JwtError::InvalidSignature)?;
    mac.verify_slice(&signature_bytes)
        .map_err(|_| JwtError::InvalidSignature)?;

    // Decode claims.
    let claims_bytes = b64.decode(parts[1]).map_err(|_| JwtError::InvalidFormat)?;
    let claims: Claims =
        serde_json::from_slice(&claims_bytes).map_err(|_| JwtError::InvalidFormat)?;

    Ok(claims)
}

/// JWT error types.
#[derive(Debug, thiserror::Error)]
pub enum JwtError {
    /// Token format is invalid (not 3 dot-separated parts).
    #[error("invalid JWT format")]
    InvalidFormat,
    /// HMAC signature verification failed.
    #[error("invalid JWT signature")]
    InvalidSignature,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encode_decode_roundtrip() {
        let claims = Claims {
            sub: "IBMUSER".to_string(),
            iat: 1000000,
            exp: 2000000,
        };
        let secret = "test-secret";

        let token = encode(&claims, secret);
        assert_eq!(token.split('.').count(), 3);

        let decoded = decode(&token, secret).unwrap();
        assert_eq!(decoded.sub, "IBMUSER");
        assert_eq!(decoded.iat, 1000000);
        assert_eq!(decoded.exp, 2000000);
    }

    #[test]
    fn test_decode_invalid_signature() {
        let claims = Claims {
            sub: "IBMUSER".to_string(),
            iat: 1000000,
            exp: 2000000,
        };

        let token = encode(&claims, "secret1");
        let result = decode(&token, "secret2");
        assert!(result.is_err());
    }

    #[test]
    fn test_decode_invalid_format() {
        let result = decode("not.a.valid.jwt.token", "secret");
        assert!(result.is_err());

        let result = decode("onlyonepart", "secret");
        assert!(result.is_err());
    }

    #[test]
    fn test_decode_tampered_payload() {
        let claims = Claims {
            sub: "IBMUSER".to_string(),
            iat: 1000000,
            exp: 2000000,
        };
        let token = encode(&claims, "secret");

        // Tamper with the payload.
        let parts: Vec<&str> = token.split('.').collect();
        let tampered = format!("{}.dGFtcGVyZWQ.{}", parts[0], parts[2]);
        let result = decode(&tampered, "secret");
        assert!(result.is_err());
    }
}
