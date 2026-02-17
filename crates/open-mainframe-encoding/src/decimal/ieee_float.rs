//! IEEE 754 binary floating point encoding for COBOL FLOAT-SHORT and FLOAT-LONG.
//!
//! COBOL FLOAT-SHORT maps to IEEE 754 single precision (4 bytes, f32).
//! COBOL FLOAT-LONG maps to IEEE 754 double precision (8 bytes, f64).
//!
//! On z/OS, IEEE floats are stored in **big-endian** byte order.
//! This module provides encode/decode for that format.

use crate::error::EncodingError;

/// IEEE 754 single-precision float wrapper (COBOL FLOAT-SHORT, 4 bytes).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IeeeFloat(pub f32);

/// IEEE 754 double-precision float wrapper (COBOL FLOAT-LONG, 8 bytes).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IeeeDouble(pub f64);

/// Encode an f64 value as IEEE 754 single precision (4 bytes, big-endian).
///
/// # Errors
/// Returns `EncodingError::ConversionFailed` if the value is NaN, infinity,
/// or cannot be represented as f32 without overflow.
pub fn encode_ieee_short(value: f64) -> Result<[u8; 4], EncodingError> {
    let f = value as f32;
    if f.is_nan() {
        return Err(EncodingError::ConversionFailed {
            message: "NaN cannot be encoded as IEEE FLOAT-SHORT".to_string(),
        });
    }
    if f.is_infinite() && !value.is_infinite() {
        return Err(EncodingError::ConversionFailed {
            message: format!(
                "Value {} overflows IEEE 754 single precision",
                value
            ),
        });
    }
    Ok(f.to_be_bytes())
}

/// Decode IEEE 754 single precision (4 bytes, big-endian) to f64.
///
/// # Errors
/// Returns `EncodingError::ConversionFailed` if the bytes represent NaN.
pub fn decode_ieee_short(bytes: &[u8; 4]) -> Result<f64, EncodingError> {
    let f = f32::from_be_bytes(*bytes);
    if f.is_nan() {
        return Err(EncodingError::ConversionFailed {
            message: "NaN encountered in IEEE FLOAT-SHORT decode".to_string(),
        });
    }
    Ok(f as f64)
}

/// Encode an f64 value as IEEE 754 double precision (8 bytes, big-endian).
///
/// # Errors
/// Returns `EncodingError::ConversionFailed` if the value is NaN.
pub fn encode_ieee_long(value: f64) -> Result<[u8; 8], EncodingError> {
    if value.is_nan() {
        return Err(EncodingError::ConversionFailed {
            message: "NaN cannot be encoded as IEEE FLOAT-LONG".to_string(),
        });
    }
    Ok(value.to_be_bytes())
}

/// Decode IEEE 754 double precision (8 bytes, big-endian) to f64.
///
/// # Errors
/// Returns `EncodingError::ConversionFailed` if the bytes represent NaN.
pub fn decode_ieee_long(bytes: &[u8; 8]) -> Result<f64, EncodingError> {
    let f = f64::from_be_bytes(*bytes);
    if f.is_nan() {
        return Err(EncodingError::ConversionFailed {
            message: "NaN encountered in IEEE FLOAT-LONG decode".to_string(),
        });
    }
    Ok(f)
}

/// Encode an f64 as IEEE short (4 bytes) from a slice.
///
/// Convenience wrapper that writes into a mutable slice.
pub fn encode_ieee_short_into(value: f64, buf: &mut [u8]) -> Result<(), EncodingError> {
    if buf.len() < 4 {
        return Err(EncodingError::ConversionFailed {
            message: format!("Buffer too small for IEEE short: {} < 4", buf.len()),
        });
    }
    let bytes = encode_ieee_short(value)?;
    buf[..4].copy_from_slice(&bytes);
    Ok(())
}

/// Encode an f64 as IEEE long (8 bytes) from a slice.
///
/// Convenience wrapper that writes into a mutable slice.
pub fn encode_ieee_long_into(value: f64, buf: &mut [u8]) -> Result<(), EncodingError> {
    if buf.len() < 8 {
        return Err(EncodingError::ConversionFailed {
            message: format!("Buffer too small for IEEE long: {} < 8", buf.len()),
        });
    }
    let bytes = encode_ieee_long(value)?;
    buf[..8].copy_from_slice(&bytes);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- Epic 703: IEEE Float Encoding ---

    #[test]
    fn test_encode_ieee_short_pi() {
        // AC: Given a FLOAT-SHORT field with value 3.14
        // When encoded as IEEE 754 single precision (big-endian)
        // Then the correct 4-byte representation is produced
        let bytes = encode_ieee_short(3.14).unwrap();
        // 3.14 as f32 = 0x4048F5C3 in big-endian
        let expected = (3.14_f32).to_be_bytes();
        assert_eq!(bytes, expected);
    }

    #[test]
    fn test_decode_ieee_short() {
        let encoded = encode_ieee_short(3.14).unwrap();
        let decoded = decode_ieee_short(&encoded).unwrap();
        // f32 precision: compare within tolerance
        assert!((decoded - 3.14).abs() < 0.001);
    }

    #[test]
    fn test_encode_ieee_long() {
        // AC: Given a FLOAT-LONG field
        // When encoded as IEEE 754 double precision (big-endian)
        // Then the correct 8-byte representation is produced
        let bytes = encode_ieee_long(3.141592653589793).unwrap();
        let expected = 3.141592653589793_f64.to_be_bytes();
        assert_eq!(bytes, expected);
    }

    #[test]
    fn test_decode_ieee_long() {
        // AC: Given a FLOAT-LONG field
        // When decoded from 8 big-endian bytes
        // Then the correct f64 value is returned
        let encoded = encode_ieee_long(3.141592653589793).unwrap();
        let decoded = decode_ieee_long(&encoded).unwrap();
        assert!((decoded - 3.141592653589793).abs() < 1e-15);
    }

    #[test]
    fn test_ieee_short_roundtrip() {
        let values = [0.0, 1.0, -1.0, 0.5, 100.0, -3.14, 1e10, 1e-10];
        for &v in &values {
            let bytes = encode_ieee_short(v).unwrap();
            let decoded = decode_ieee_short(&bytes).unwrap();
            let expected = v as f32 as f64; // round to f32 precision
            assert!(
                (decoded - expected).abs() < 1e-6 * expected.abs().max(1.0),
                "Roundtrip failed for {}: got {}",
                v,
                decoded
            );
        }
    }

    #[test]
    fn test_ieee_long_roundtrip() {
        let values = [
            0.0, 1.0, -1.0, 0.5, 100.0, -3.141592653589793, 1e100, 1e-100,
        ];
        for &v in &values {
            let bytes = encode_ieee_long(v).unwrap();
            let decoded = decode_ieee_long(&bytes).unwrap();
            assert_eq!(decoded, v, "Roundtrip failed for {}", v);
        }
    }

    #[test]
    fn test_ieee_short_zero() {
        let bytes = encode_ieee_short(0.0).unwrap();
        assert_eq!(bytes, [0x00, 0x00, 0x00, 0x00]);
        let decoded = decode_ieee_short(&bytes).unwrap();
        assert_eq!(decoded, 0.0);
    }

    #[test]
    fn test_ieee_long_zero() {
        let bytes = encode_ieee_long(0.0).unwrap();
        assert_eq!(bytes, [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);
        let decoded = decode_ieee_long(&bytes).unwrap();
        assert_eq!(decoded, 0.0);
    }

    #[test]
    fn test_ieee_short_negative_zero() {
        let bytes = encode_ieee_short(-0.0).unwrap();
        let decoded = decode_ieee_short(&bytes).unwrap();
        // -0.0 == 0.0 in IEEE
        assert_eq!(decoded, 0.0);
    }

    #[test]
    fn test_ieee_short_nan_rejected() {
        let result = encode_ieee_short(f64::NAN);
        assert!(result.is_err());
    }

    #[test]
    fn test_ieee_long_nan_rejected() {
        let result = encode_ieee_long(f64::NAN);
        assert!(result.is_err());
    }

    #[test]
    fn test_decode_ieee_short_nan_rejected() {
        // NaN bytes for f32 (0x7FC00000)
        let bytes: [u8; 4] = [0x7F, 0xC0, 0x00, 0x00];
        let result = decode_ieee_short(&bytes);
        assert!(result.is_err());
    }

    #[test]
    fn test_decode_ieee_long_nan_rejected() {
        // NaN bytes for f64 (0x7FF8000000000000)
        let bytes: [u8; 8] = [0x7F, 0xF8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        let result = decode_ieee_long(&bytes);
        assert!(result.is_err());
    }

    #[test]
    fn test_ieee_short_infinity() {
        // +Infinity is valid IEEE
        let bytes = encode_ieee_short(f64::INFINITY).unwrap();
        let decoded = decode_ieee_short(&bytes).unwrap();
        assert!(decoded.is_infinite() && decoded > 0.0);

        // -Infinity
        let bytes = encode_ieee_short(f64::NEG_INFINITY).unwrap();
        let decoded = decode_ieee_short(&bytes).unwrap();
        assert!(decoded.is_infinite() && decoded < 0.0);
    }

    #[test]
    fn test_ieee_long_infinity() {
        let bytes = encode_ieee_long(f64::INFINITY).unwrap();
        let decoded = decode_ieee_long(&bytes).unwrap();
        assert!(decoded.is_infinite() && decoded > 0.0);
    }

    #[test]
    fn test_ieee_short_big_endian() {
        // 1.0 as f32 big-endian = 0x3F800000
        let bytes = encode_ieee_short(1.0).unwrap();
        assert_eq!(bytes, [0x3F, 0x80, 0x00, 0x00]);
    }

    #[test]
    fn test_ieee_long_big_endian() {
        // 1.0 as f64 big-endian = 0x3FF0000000000000
        let bytes = encode_ieee_long(1.0).unwrap();
        assert_eq!(bytes, [0x3F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);
    }

    #[test]
    fn test_encode_ieee_short_into() {
        let mut buf = [0u8; 8];
        encode_ieee_short_into(1.0, &mut buf).unwrap();
        assert_eq!(&buf[..4], &[0x3F, 0x80, 0x00, 0x00]);
    }

    #[test]
    fn test_encode_ieee_long_into() {
        let mut buf = [0u8; 8];
        encode_ieee_long_into(1.0, &mut buf).unwrap();
        assert_eq!(buf, [0x3F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);
    }

    #[test]
    fn test_encode_ieee_short_into_buffer_too_small() {
        let mut buf = [0u8; 3];
        let result = encode_ieee_short_into(1.0, &mut buf);
        assert!(result.is_err());
    }

    #[test]
    fn test_encode_ieee_long_into_buffer_too_small() {
        let mut buf = [0u8; 7];
        let result = encode_ieee_long_into(1.0, &mut buf);
        assert!(result.is_err());
    }

    #[test]
    fn test_ieee_float_struct() {
        let f = IeeeFloat(3.14);
        assert!((f.0 - 3.14).abs() < 0.001);

        let f2 = f;
        assert_eq!(f, f2);
    }

    #[test]
    fn test_ieee_double_struct() {
        let d = IeeeDouble(3.141592653589793);
        assert_eq!(d.0, 3.141592653589793);

        let d2 = d;
        assert_eq!(d, d2);
    }
}
