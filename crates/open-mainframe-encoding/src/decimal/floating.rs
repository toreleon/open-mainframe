//! IBM Hexadecimal Floating Point (HFP) encoding and decoding.
//!
//! Implements COMP-1 (short, 4-byte) and COMP-2 (long, 8-byte) floating
//! point in IBM's Hexadecimal Floating Point format used on System/370
//! and z/Architecture mainframes.
//!
//! # HFP Format
//!
//! IBM HFP uses **base-16** (hexadecimal) exponents rather than IEEE 754's
//! base-2. The bit layout is:
//!
//! **Short (COMP-1, 4 bytes):**
//! ```text
//! [S|EEEEEEE|FFFFFFFFFFFFFFFFFFFFFFFF]
//!  1    7              24 bits
//! ```
//!
//! **Long (COMP-2, 8 bytes):**
//! ```text
//! [S|EEEEEEE|FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF]
//!  1    7                          56 bits
//! ```
//!
//! Where:
//! - **S**: Sign bit (0 = positive, 1 = negative)
//! - **E**: Exponent with bias 64 (base-16 exponent)
//! - **F**: Fraction (normalized so first hex digit is non-zero)
//!
//! # Key Differences from IEEE 754
//!
//! - Base-16 exponent (not base-2)
//! - No implicit leading bit
//! - No NaN or Infinity representations
//! - No denormalized numbers
//! - Exponent bias is 64 (not 127/1023)

use super::Result;
use crate::error::EncodingError;

/// IBM HFP short-precision floating point (COMP-1, 4 bytes).
///
/// Stores a 32-bit hexadecimal floating point value.
/// Range: approximately +/- 5.4e-79 to 7.2e+75.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HfpFloat {
    /// The IEEE 754 f64 representation of this value.
    value: f64,
}

impl HfpFloat {
    /// Create a new HFP float from an f64 value.
    ///
    /// Returns an error if the value is NaN or Infinity (HFP has no
    /// representation for these).
    pub fn new(value: f64) -> Result<Self> {
        if value.is_nan() {
            return Err(EncodingError::ConversionFailed {
                message: "HFP does not support NaN".to_string(),
            });
        }
        if value.is_infinite() {
            return Err(EncodingError::ConversionFailed {
                message: "HFP does not support Infinity".to_string(),
            });
        }
        Ok(Self { value })
    }

    /// Get the f64 value.
    pub fn value(&self) -> f64 {
        self.value
    }

    /// Encode to 4-byte IBM HFP format.
    pub fn encode(&self) -> [u8; 4] {
        encode_hfp_short(self.value)
    }

    /// Decode from 4-byte IBM HFP format.
    pub fn decode(bytes: &[u8; 4]) -> Result<Self> {
        let value = decode_hfp_short(bytes);
        Ok(Self { value })
    }

    /// Decode from a byte slice (must be exactly 4 bytes).
    pub fn decode_slice(bytes: &[u8]) -> Result<Self> {
        if bytes.len() != 4 {
            return Err(EncodingError::ConversionFailed {
                message: format!("HFP short requires 4 bytes, got {}", bytes.len()),
            });
        }
        let arr: [u8; 4] = bytes.try_into().unwrap();
        Self::decode(&arr)
    }
}

impl From<HfpFloat> for f64 {
    fn from(hfp: HfpFloat) -> f64 {
        hfp.value
    }
}

impl From<HfpFloat> for f32 {
    fn from(hfp: HfpFloat) -> f32 {
        hfp.value as f32
    }
}

/// IBM HFP long-precision floating point (COMP-2, 8 bytes).
///
/// Stores a 64-bit hexadecimal floating point value.
/// Range: approximately +/- 5.4e-79 to 7.2e+75 with more precision.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HfpDouble {
    /// The IEEE 754 f64 representation of this value.
    value: f64,
}

impl HfpDouble {
    /// Create a new HFP double from an f64 value.
    ///
    /// Returns an error if the value is NaN or Infinity.
    pub fn new(value: f64) -> Result<Self> {
        if value.is_nan() {
            return Err(EncodingError::ConversionFailed {
                message: "HFP does not support NaN".to_string(),
            });
        }
        if value.is_infinite() {
            return Err(EncodingError::ConversionFailed {
                message: "HFP does not support Infinity".to_string(),
            });
        }
        Ok(Self { value })
    }

    /// Get the f64 value.
    pub fn value(&self) -> f64 {
        self.value
    }

    /// Encode to 8-byte IBM HFP format.
    pub fn encode(&self) -> [u8; 8] {
        encode_hfp_long(self.value)
    }

    /// Decode from 8-byte IBM HFP format.
    pub fn decode(bytes: &[u8; 8]) -> Result<Self> {
        let value = decode_hfp_long(bytes);
        Ok(Self { value })
    }

    /// Decode from a byte slice (must be exactly 8 bytes).
    pub fn decode_slice(bytes: &[u8]) -> Result<Self> {
        if bytes.len() != 8 {
            return Err(EncodingError::ConversionFailed {
                message: format!("HFP long requires 8 bytes, got {}", bytes.len()),
            });
        }
        let arr: [u8; 8] = bytes.try_into().unwrap();
        Self::decode(&arr)
    }
}

impl From<HfpDouble> for f64 {
    fn from(hfp: HfpDouble) -> f64 {
        hfp.value
    }
}

// ---------------------------------------------------------------------------
// Core encoding/decoding functions
// ---------------------------------------------------------------------------

/// Encode an f64 value to 4-byte IBM HFP short format.
///
/// # HFP Short Layout
///
/// ```text
/// Byte 0: [S|EEEEEEE]  sign + biased exponent
/// Bytes 1-3: fraction (24 bits, base-16 normalized)
/// ```
///
/// Value = (-1)^S * 0.fraction * 16^(exponent - 64)
pub fn encode_hfp_short(value: f64) -> [u8; 4] {
    if value == 0.0 {
        return [0u8; 4];
    }

    let sign = if value < 0.0 { 1u8 } else { 0u8 };
    let abs_val = value.abs();

    // Compute base-16 exponent and fraction.
    // value = fraction * 16^hex_exp, where 1/16 <= fraction < 1.
    let log16 = abs_val.log2() / 4.0; // log16(x) = log2(x)/4
    let mut hex_exp = log16.ceil() as i32;

    // fraction = abs_val / 16^hex_exp => should be in [1/16, 1)
    let mut fraction = abs_val / (16.0_f64).powi(hex_exp);

    // Normalize: ensure 1/16 <= fraction < 1
    if fraction >= 1.0 {
        hex_exp += 1;
        fraction /= 16.0;
    }
    if fraction < 1.0 / 16.0 && fraction > 0.0 {
        hex_exp -= 1;
        fraction *= 16.0;
    }

    let biased_exp = (hex_exp + 64).clamp(0, 127) as u8;

    // Convert fraction to 24-bit integer: fraction * 2^24
    let frac_bits = (fraction * (1u64 << 24) as f64).round() as u32;
    // Clamp to 24 bits
    let frac_bits = frac_bits & 0x00FF_FFFF;

    let byte0 = (sign << 7) | biased_exp;
    let byte1 = ((frac_bits >> 16) & 0xFF) as u8;
    let byte2 = ((frac_bits >> 8) & 0xFF) as u8;
    let byte3 = (frac_bits & 0xFF) as u8;

    [byte0, byte1, byte2, byte3]
}

/// Decode a 4-byte IBM HFP short value to f64.
pub fn decode_hfp_short(bytes: &[u8; 4]) -> f64 {
    let sign = (bytes[0] >> 7) & 1;
    let biased_exp = (bytes[0] & 0x7F) as i32;
    let fraction = ((bytes[1] as u32) << 16) | ((bytes[2] as u32) << 8) | (bytes[3] as u32);

    if fraction == 0 && biased_exp == 0 {
        return 0.0;
    }

    let hex_exp = biased_exp - 64;
    let frac_f64 = fraction as f64 / (1u64 << 24) as f64;
    let value = frac_f64 * (16.0_f64).powi(hex_exp);

    if sign == 1 {
        -value
    } else {
        value
    }
}

/// Encode an f64 value to 8-byte IBM HFP long format.
///
/// # HFP Long Layout
///
/// ```text
/// Byte 0: [S|EEEEEEE]  sign + biased exponent
/// Bytes 1-7: fraction (56 bits, base-16 normalized)
/// ```
///
/// Value = (-1)^S * 0.fraction * 16^(exponent - 64)
pub fn encode_hfp_long(value: f64) -> [u8; 8] {
    if value == 0.0 {
        return [0u8; 8];
    }

    let sign = if value < 0.0 { 1u8 } else { 0u8 };
    let abs_val = value.abs();

    // Compute base-16 exponent and fraction.
    let log16 = abs_val.log2() / 4.0;
    let mut hex_exp = log16.ceil() as i32;

    let mut fraction = abs_val / (16.0_f64).powi(hex_exp);

    // Normalize: ensure 1/16 <= fraction < 1
    if fraction >= 1.0 {
        hex_exp += 1;
        fraction /= 16.0;
    }
    if fraction < 1.0 / 16.0 && fraction > 0.0 {
        hex_exp -= 1;
        fraction *= 16.0;
    }

    let biased_exp = (hex_exp + 64).clamp(0, 127) as u8;

    // Convert fraction to 56-bit integer: fraction * 2^56
    let frac_bits = (fraction * (1u64 << 56) as f64).round() as u64;
    let frac_bits = frac_bits & 0x00FF_FFFF_FFFF_FFFF;

    let byte0 = (sign << 7) | biased_exp;
    let mut result = [0u8; 8];
    result[0] = byte0;
    result[1] = ((frac_bits >> 48) & 0xFF) as u8;
    result[2] = ((frac_bits >> 40) & 0xFF) as u8;
    result[3] = ((frac_bits >> 32) & 0xFF) as u8;
    result[4] = ((frac_bits >> 24) & 0xFF) as u8;
    result[5] = ((frac_bits >> 16) & 0xFF) as u8;
    result[6] = ((frac_bits >> 8) & 0xFF) as u8;
    result[7] = (frac_bits & 0xFF) as u8;

    result
}

/// Decode an 8-byte IBM HFP long value to f64.
pub fn decode_hfp_long(bytes: &[u8; 8]) -> f64 {
    let sign = (bytes[0] >> 7) & 1;
    let biased_exp = (bytes[0] & 0x7F) as i32;
    let fraction = ((bytes[1] as u64) << 48)
        | ((bytes[2] as u64) << 40)
        | ((bytes[3] as u64) << 32)
        | ((bytes[4] as u64) << 24)
        | ((bytes[5] as u64) << 16)
        | ((bytes[6] as u64) << 8)
        | (bytes[7] as u64);

    if fraction == 0 && biased_exp == 0 {
        return 0.0;
    }

    let hex_exp = biased_exp - 64;
    let frac_f64 = fraction as f64 / (1u64 << 56) as f64;
    let value = frac_f64 * (16.0_f64).powi(hex_exp);

    if sign == 1 {
        -value
    } else {
        value
    }
}

/// Convert an IEEE 754 f32 to 4-byte HFP short bytes.
pub fn ieee_to_hfp_short(value: f32) -> Result<[u8; 4]> {
    if value.is_nan() {
        return Err(EncodingError::ConversionFailed {
            message: "Cannot convert NaN to HFP".to_string(),
        });
    }
    if value.is_infinite() {
        return Err(EncodingError::ConversionFailed {
            message: "Cannot convert Infinity to HFP".to_string(),
        });
    }
    Ok(encode_hfp_short(value as f64))
}

/// Convert an IEEE 754 f64 to 8-byte HFP long bytes.
pub fn ieee_to_hfp_long(value: f64) -> Result<[u8; 8]> {
    if value.is_nan() {
        return Err(EncodingError::ConversionFailed {
            message: "Cannot convert NaN to HFP".to_string(),
        });
    }
    if value.is_infinite() {
        return Err(EncodingError::ConversionFailed {
            message: "Cannot convert Infinity to HFP".to_string(),
        });
    }
    Ok(encode_hfp_long(value))
}

/// Convert 4-byte HFP short bytes to IEEE 754 f32.
pub fn hfp_short_to_ieee(bytes: &[u8; 4]) -> f32 {
    decode_hfp_short(bytes) as f32
}

/// Convert 8-byte HFP long bytes to IEEE 754 f64.
pub fn hfp_long_to_ieee(bytes: &[u8; 8]) -> f64 {
    decode_hfp_long(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;

    // ---- HFP Short (COMP-1) tests ----

    #[test]
    fn test_hfp_short_zero() {
        let bytes = encode_hfp_short(0.0);
        assert_eq!(bytes, [0x00, 0x00, 0x00, 0x00]);
        assert_eq!(decode_hfp_short(&bytes), 0.0);
    }

    #[test]
    fn test_hfp_short_one() {
        // 1.0 in HFP: exponent = 64+1 = 65 = 0x41
        // fraction = 1/16 = 0x100000 (first hex digit is 1)
        let bytes = encode_hfp_short(1.0);
        assert_eq!(bytes[0], 0x41); // sign=0, exp=65
        assert_eq!(bytes[1], 0x10); // fraction high byte
        let decoded = decode_hfp_short(&bytes);
        assert!((decoded - 1.0).abs() < 1e-6);
    }

    #[test]
    fn test_hfp_short_negative_one() {
        let bytes = encode_hfp_short(-1.0);
        assert_eq!(bytes[0], 0xC1); // sign=1, exp=65
        let decoded = decode_hfp_short(&bytes);
        assert!((decoded + 1.0).abs() < 1e-6);
    }

    #[test]
    fn test_hfp_short_roundtrip() {
        let test_values = [
            0.0, 1.0, -1.0, 0.5, -0.5, 100.0, -100.0, 0.125, 3.14159, -2.71828, 256.0,
            0.00390625,
        ];

        for &val in &test_values {
            let encoded = encode_hfp_short(val);
            let decoded = decode_hfp_short(&encoded);
            let tolerance = if val == 0.0 { 0.0 } else { val.abs() * 1e-6 };
            assert!(
                (decoded - val).abs() <= tolerance,
                "roundtrip failed for {}: got {}",
                val,
                decoded
            );
        }
    }

    #[test]
    fn test_hfp_short_struct() {
        let hfp = HfpFloat::new(42.0).unwrap();
        assert!((hfp.value() - 42.0).abs() < 1e-10);

        let bytes = hfp.encode();
        let decoded = HfpFloat::decode(&bytes).unwrap();
        assert!((decoded.value() - 42.0).abs() < 1e-4);
    }

    #[test]
    fn test_hfp_float_rejects_nan() {
        assert!(HfpFloat::new(f64::NAN).is_err());
    }

    #[test]
    fn test_hfp_float_rejects_infinity() {
        assert!(HfpFloat::new(f64::INFINITY).is_err());
        assert!(HfpFloat::new(f64::NEG_INFINITY).is_err());
    }

    #[test]
    fn test_hfp_float_into_f64() {
        let hfp = HfpFloat::new(3.14).unwrap();
        let f: f64 = hfp.into();
        assert!((f - 3.14).abs() < 1e-10);
    }

    #[test]
    fn test_hfp_float_decode_slice() {
        let bytes = encode_hfp_short(1.0);
        let hfp = HfpFloat::decode_slice(&bytes).unwrap();
        assert!((hfp.value() - 1.0).abs() < 1e-6);

        // Wrong length
        assert!(HfpFloat::decode_slice(&[0u8; 3]).is_err());
    }

    // ---- HFP Long (COMP-2) tests ----

    #[test]
    fn test_hfp_long_zero() {
        let bytes = encode_hfp_long(0.0);
        assert_eq!(bytes, [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);
        assert_eq!(decode_hfp_long(&bytes), 0.0);
    }

    #[test]
    fn test_hfp_long_one() {
        let bytes = encode_hfp_long(1.0);
        assert_eq!(bytes[0], 0x41); // sign=0, exp=65
        assert_eq!(bytes[1], 0x10); // first hex digit = 1
        let decoded = decode_hfp_long(&bytes);
        assert!((decoded - 1.0).abs() < 1e-14);
    }

    #[test]
    fn test_hfp_long_negative_one() {
        let bytes = encode_hfp_long(-1.0);
        assert_eq!(bytes[0], 0xC1); // sign=1, exp=65
        let decoded = decode_hfp_long(&bytes);
        assert!((decoded + 1.0).abs() < 1e-14);
    }

    #[test]
    fn test_hfp_long_roundtrip() {
        let test_values = [
            0.0,
            1.0,
            -1.0,
            0.5,
            -0.5,
            100.0,
            -100.0,
            0.125,
            3.141592653589793,
            -2.718281828459045,
            1e10,
            1e-10,
            256.0,
        ];

        for &val in &test_values {
            let encoded = encode_hfp_long(val);
            let decoded = decode_hfp_long(&encoded);
            let tolerance = if val == 0.0 { 0.0 } else { val.abs() * 1e-14 };
            assert!(
                (decoded - val).abs() <= tolerance,
                "long roundtrip failed for {}: got {} (diff={})",
                val,
                decoded,
                (decoded - val).abs()
            );
        }
    }

    #[test]
    fn test_hfp_double_struct() {
        let hfp = HfpDouble::new(42.0).unwrap();
        assert!((hfp.value() - 42.0).abs() < 1e-14);

        let bytes = hfp.encode();
        let decoded = HfpDouble::decode(&bytes).unwrap();
        assert!((decoded.value() - 42.0).abs() < 1e-12);
    }

    #[test]
    fn test_hfp_double_rejects_nan() {
        assert!(HfpDouble::new(f64::NAN).is_err());
    }

    #[test]
    fn test_hfp_double_rejects_infinity() {
        assert!(HfpDouble::new(f64::INFINITY).is_err());
        assert!(HfpDouble::new(f64::NEG_INFINITY).is_err());
    }

    #[test]
    fn test_hfp_double_into_f64() {
        let hfp = HfpDouble::new(2.71828).unwrap();
        let f: f64 = hfp.into();
        assert!((f - 2.71828).abs() < 1e-10);
    }

    #[test]
    fn test_hfp_double_decode_slice() {
        let bytes = encode_hfp_long(1.0);
        let hfp = HfpDouble::decode_slice(&bytes).unwrap();
        assert!((hfp.value() - 1.0).abs() < 1e-14);

        // Wrong length
        assert!(HfpDouble::decode_slice(&[0u8; 5]).is_err());
    }

    // ---- IEEE conversion tests ----

    #[test]
    fn test_ieee_to_hfp_short_conversion() {
        let hfp_bytes = ieee_to_hfp_short(1.0_f32).unwrap();
        let ieee_val = hfp_short_to_ieee(&hfp_bytes);
        assert!((ieee_val - 1.0).abs() < 1e-6);
    }

    #[test]
    fn test_ieee_to_hfp_long_conversion() {
        let hfp_bytes = ieee_to_hfp_long(3.14159265).unwrap();
        let ieee_val = hfp_long_to_ieee(&hfp_bytes);
        assert!((ieee_val - 3.14159265).abs() < 1e-10);
    }

    #[test]
    fn test_ieee_to_hfp_rejects_nan() {
        assert!(ieee_to_hfp_short(f32::NAN).is_err());
        assert!(ieee_to_hfp_long(f64::NAN).is_err());
    }

    #[test]
    fn test_ieee_to_hfp_rejects_infinity() {
        assert!(ieee_to_hfp_short(f32::INFINITY).is_err());
        assert!(ieee_to_hfp_long(f64::INFINITY).is_err());
    }

    // ---- Known IBM reference values ----

    #[test]
    fn test_known_hfp_value_positive_1() {
        // IBM reference: 1.0 = 0x41100000
        let bytes: [u8; 4] = [0x41, 0x10, 0x00, 0x00];
        let value = decode_hfp_short(&bytes);
        assert!((value - 1.0).abs() < 1e-6);
    }

    #[test]
    fn test_known_hfp_value_negative_1() {
        // IBM reference: -1.0 = 0xC1100000
        let bytes: [u8; 4] = [0xC1, 0x10, 0x00, 0x00];
        let value = decode_hfp_short(&bytes);
        assert!((value + 1.0).abs() < 1e-6);
    }

    #[test]
    fn test_known_hfp_value_half() {
        // 0.5 in HFP: 0x40800000
        // exponent = 64 = 0x40, fraction = 0x800000 = 8/16 = 0.5
        let bytes: [u8; 4] = [0x40, 0x80, 0x00, 0x00];
        let value = decode_hfp_short(&bytes);
        assert!((value - 0.5).abs() < 1e-6);
    }

    #[test]
    fn test_known_hfp_value_100() {
        // 100.0 in HFP: 0x42640000
        // exponent = 66 = 0x42, fraction = 0x640000 = (6*16+4)/256 * 16^2 = 100
        let bytes: [u8; 4] = [0x42, 0x64, 0x00, 0x00];
        let value = decode_hfp_short(&bytes);
        assert!((value - 100.0).abs() < 1e-4);
    }

    #[test]
    fn test_hfp_short_small_value() {
        // Test a small value to ensure normalization works
        let val = 0.001;
        let encoded = encode_hfp_short(val);
        let decoded = decode_hfp_short(&encoded);
        assert!(
            (decoded - val).abs() < val * 1e-5,
            "small value failed: {} vs {}",
            val,
            decoded
        );
    }

    #[test]
    fn test_hfp_long_large_value() {
        let val = 1e15;
        let encoded = encode_hfp_long(val);
        let decoded = decode_hfp_long(&encoded);
        assert!(
            (decoded - val).abs() < val * 1e-14,
            "large value failed: {} vs {}",
            val,
            decoded
        );
    }
}
