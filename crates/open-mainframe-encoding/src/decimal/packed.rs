//! Packed Decimal (COMP-3) encoding and decoding.
//!
//! Packed decimal format stores two decimal digits per byte, with the
//! rightmost nibble containing the sign. This is the most common format
//! for COBOL COMP-3 (packed decimal) data items.
//!
//! # Format
//!
//! For a PIC S9(n) COMP-3 declaration:
//! - Each pair of digits occupies one byte
//! - The sign nibble is in the rightmost position
//! - Storage size = ceil((n + 1) / 2) bytes
//!
//! Example: +12345 in PIC S9(5) COMP-3
//! - Layout: d1|d2 d3|d4 d5|sign
//! - Binary: 0x12 0x34 0x5C (3 bytes)
//! - The 'C' nibble indicates positive
//!
//! Example: +123456 in PIC S9(6) COMP-3
//! - Layout: 0|d1 d2|d3 d4|d5 d6|sign (leading zero pad for even digits)
//! - Binary: 0x01 0x23 0x45 0x6C (4 bytes)

use rust_decimal::Decimal;
use std::str::FromStr;

use super::{Result, Sign};
use crate::error::EncodingError;

/// Packed decimal value with metadata.
#[derive(Debug, Clone, PartialEq)]
pub struct PackedDecimal {
    /// The decimal value
    pub value: Decimal,
    /// Number of integer digits (before decimal point)
    pub integer_digits: usize,
    /// Number of fractional digits (after decimal point)
    pub decimal_digits: usize,
    /// Whether the value is signed
    pub signed: bool,
}

impl PackedDecimal {
    /// Create a new packed decimal specification.
    ///
    /// # Arguments
    /// * `value` - The decimal value
    /// * `integer_digits` - Number of digits before the decimal point
    /// * `decimal_digits` - Number of digits after the decimal point
    /// * `signed` - Whether the field is signed
    pub fn new(value: Decimal, integer_digits: usize, decimal_digits: usize, signed: bool) -> Self {
        Self {
            value,
            integer_digits,
            decimal_digits,
            signed,
        }
    }

    /// Calculate the storage size in bytes.
    ///
    /// Storage = ceil((total_digits + 1) / 2)
    pub fn storage_size(&self) -> usize {
        let total_digits = self.integer_digits + self.decimal_digits;
        (total_digits + 2) / 2 // +1 for sign nibble, then round up
    }

    /// Encode the value to packed decimal bytes.
    pub fn encode(&self) -> Result<Vec<u8>> {
        pack_decimal(
            &self.value,
            self.integer_digits,
            self.decimal_digits,
            self.signed,
        )
    }

    /// Decode packed decimal bytes to a value.
    pub fn decode(bytes: &[u8], integer_digits: usize, decimal_digits: usize) -> Result<Self> {
        let (value, sign) = unpack_decimal(bytes, decimal_digits)?;
        Ok(Self {
            value,
            integer_digits,
            decimal_digits,
            signed: sign != Sign::Unsigned,
        })
    }
}

/// Encode a decimal value to packed decimal bytes.
///
/// # Arguments
/// * `value` - The decimal value to encode
/// * `integer_digits` - Number of digits before the decimal point
/// * `decimal_digits` - Number of digits after the decimal point
/// * `signed` - Whether to include a sign nibble
///
/// # Returns
/// A vector of bytes in packed decimal format.
///
/// # Errors
/// Returns an error if the value cannot fit in the specified precision.
pub fn pack_decimal(
    value: &Decimal,
    integer_digits: usize,
    decimal_digits: usize,
    signed: bool,
) -> Result<Vec<u8>> {
    let total_digits = integer_digits + decimal_digits;

    // Determine sign
    let is_negative = value.is_sign_negative();
    let sign = if signed {
        if is_negative {
            Sign::Negative
        } else {
            Sign::Positive
        }
    } else {
        Sign::Unsigned
    };

    // Scale the value to remove decimal point
    let scale_factor = if decimal_digits > 0 {
        Decimal::from_str(&format!("1{}", "0".repeat(decimal_digits))).map_err(|e| {
            EncodingError::ConversionFailed {
                message: format!("Failed to create scale factor: {}", e),
            }
        })?
    } else {
        Decimal::from(1)
    };

    let scaled = (value.abs() * scale_factor).trunc();

    // Convert to string of digits
    let digit_string = scaled.to_string();
    let digit_string = digit_string.trim_start_matches('-');

    // Check if it fits
    if digit_string.len() > total_digits {
        return Err(EncodingError::ConversionFailed {
            message: format!(
                "Value {} requires {} digits but only {} available",
                value,
                digit_string.len(),
                total_digits
            ),
        });
    }

    // Pad with leading zeros to fill total_digits
    let padded = format!("{:0>width$}", digit_string, width = total_digits);
    let digits: Vec<u8> = padded
        .chars()
        .map(|c| c.to_digit(10).unwrap() as u8)
        .collect();

    // Build nibbles: all digits plus sign nibble
    let mut nibbles: Vec<u8> = digits;
    nibbles.push(sign.to_packed_nibble());

    // If odd number of nibbles, prepend a zero (shouldn't happen with sign added)
    if nibbles.len() % 2 == 1 {
        nibbles.insert(0, 0);
    }

    // Pack nibbles into bytes
    let mut result = Vec::with_capacity(nibbles.len() / 2);
    for chunk in nibbles.chunks(2) {
        result.push((chunk[0] << 4) | chunk[1]);
    }

    Ok(result)
}

/// Decode packed decimal bytes to a decimal value.
///
/// # Arguments
/// * `bytes` - The packed decimal bytes
/// * `decimal_digits` - Number of digits after the decimal point
///
/// # Returns
/// A tuple of (Decimal value, Sign).
///
/// # Errors
/// Returns an error if the bytes contain invalid packed decimal data.
pub fn unpack_decimal(bytes: &[u8], decimal_digits: usize) -> Result<(Decimal, Sign)> {
    if bytes.is_empty() {
        return Err(EncodingError::ConversionFailed {
            message: "Empty packed decimal".to_string(),
        });
    }

    let mut digits = Vec::new();

    // Extract all digit nibbles except the last (sign) nibble
    for (i, &byte) in bytes.iter().enumerate() {
        let high = (byte >> 4) & 0x0F;
        let low = byte & 0x0F;

        if i == bytes.len() - 1 {
            // Last byte: high nibble is digit, low nibble is sign
            if high > 9 {
                return Err(EncodingError::ConversionFailed {
                    message: format!("Invalid digit nibble: 0x{:X}", high),
                });
            }
            digits.push(high);
        } else {
            // Non-last byte: both nibbles are digits
            if high > 9 || low > 9 {
                return Err(EncodingError::ConversionFailed {
                    message: format!("Invalid digit nibble in byte 0x{:02X}", byte),
                });
            }
            digits.push(high);
            digits.push(low);
        }
    }

    // Extract sign from last nibble
    let sign_nibble = bytes[bytes.len() - 1] & 0x0F;
    let sign = Sign::from_packed_nibble(sign_nibble)?;

    // Remove leading zeros for string building (but keep at least one digit)
    while digits.len() > 1 && digits[0] == 0 {
        digits.remove(0);
    }

    // Build the numeric string
    let digit_string: String = digits
        .iter()
        .map(|d| char::from_digit(*d as u32, 10).unwrap())
        .collect();

    // Insert decimal point if needed
    let numeric_string = if decimal_digits > 0 && digit_string.len() > decimal_digits {
        let int_part = &digit_string[..digit_string.len() - decimal_digits];
        let dec_part = &digit_string[digit_string.len() - decimal_digits..];
        format!("{}.{}", int_part, dec_part)
    } else if decimal_digits > 0 {
        // Pad with leading zeros for decimal part
        let padded = format!("{:0>width$}", digit_string, width = decimal_digits + 1);
        let int_part = &padded[..padded.len() - decimal_digits];
        let dec_part = &padded[padded.len() - decimal_digits..];
        format!("{}.{}", int_part, dec_part)
    } else {
        digit_string
    };

    let mut value =
        Decimal::from_str(&numeric_string).map_err(|e| EncodingError::ConversionFailed {
            message: format!("Failed to parse decimal '{}': {}", numeric_string, e),
        })?;

    // Apply sign
    if sign == Sign::Negative {
        value = -value;
    }

    Ok((value, sign))
}

// ─────────────────────── i64 helpers ───────────────────────

/// Parse packed decimal bytes directly to i64.
///
/// Lightweight version for use cases that don't need fractional
/// decimal places (e.g., SORT field comparison, IMS segment extraction).
///
/// Sign nibble: 0x0C/0x0A/0x0E = positive, 0x0D/0x0B = negative, 0x0F = unsigned.
pub fn unpack_to_i64(bytes: &[u8]) -> i64 {
    if bytes.is_empty() {
        return 0;
    }

    let mut value: i64 = 0;

    for (i, &byte) in bytes.iter().enumerate() {
        let high = (byte >> 4) & 0x0F;
        let low = byte & 0x0F;

        if i == bytes.len() - 1 {
            // Last byte: high nibble is digit, low nibble is sign
            value = value * 10 + high as i64;
            if low == 0x0D || low == 0x0B {
                value = -value;
            }
        } else {
            // Non-last bytes: both nibbles are digits
            value = value * 100 + high as i64 * 10 + low as i64;
        }
    }

    value
}

/// Pack an i64 value into packed decimal bytes.
///
/// The target slice determines the field width. Digits are right-justified
/// with leading zeros. The sign nibble is written in the last byte's low nibble.
pub fn pack_from_i64(value: i64, target: &mut [u8]) {
    if target.is_empty() {
        return;
    }

    let is_negative = value < 0;
    let abs_val = value.unsigned_abs();

    // Total digit capacity: (len * 2) - 1 (last nibble is sign)
    let total_digits = target.len() * 2 - 1;
    let mut digits = vec![0u8; total_digits];
    let mut v = abs_val;
    for d in digits.iter_mut().rev() {
        *d = (v % 10) as u8;
        v /= 10;
    }

    let mut digit_idx = 0;
    let last_idx = target.len() - 1;
    for (i, byte) in target.iter_mut().enumerate() {
        if i < last_idx {
            let high = digits.get(digit_idx).copied().unwrap_or(0);
            let low = digits.get(digit_idx + 1).copied().unwrap_or(0);
            *byte = (high << 4) | low;
            digit_idx += 2;
        } else {
            let high = digits.get(digit_idx).copied().unwrap_or(0);
            let sign: u8 = if is_negative { 0x0D } else { 0x0C };
            *byte = (high << 4) | sign;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pack_positive_integer() {
        let value = Decimal::from(12345);
        let packed = pack_decimal(&value, 5, 0, true).unwrap();
        // +12345 in COMP-3 with 5 digits: d1|d2 d3|d4 d5|sign = 0x12 0x34 0x5C
        assert_eq!(packed, vec![0x12, 0x34, 0x5C]);
    }

    #[test]
    fn test_pack_negative_integer() {
        let value = Decimal::from(-12345);
        let packed = pack_decimal(&value, 5, 0, true).unwrap();
        // -12345 in COMP-3: 0x12 0x34 0x5D
        assert_eq!(packed, vec![0x12, 0x34, 0x5D]);
    }

    #[test]
    fn test_pack_with_decimals() {
        let value = Decimal::from_str("123.45").unwrap();
        let packed = pack_decimal(&value, 3, 2, true).unwrap();
        // +12345 (scaled) in PIC S9(3)V99 COMP-3: 0x12 0x34 0x5C
        assert_eq!(packed, vec![0x12, 0x34, 0x5C]);
    }

    #[test]
    fn test_pack_even_digits() {
        // 6 digits should produce 4 bytes (7 nibbles with sign, padded to 8)
        let value = Decimal::from(123456);
        let packed = pack_decimal(&value, 6, 0, true).unwrap();
        // 6 digits + sign = 7 nibbles -> 4 bytes with leading zero
        // 0|1 2|3 4|5 6|C -> 0x01 0x23 0x45 0x6C
        assert_eq!(packed, vec![0x01, 0x23, 0x45, 0x6C]);
    }

    #[test]
    fn test_unpack_positive() {
        let bytes = vec![0x12, 0x34, 0x5C];
        let (value, sign) = unpack_decimal(&bytes, 0).unwrap();
        assert_eq!(value, Decimal::from(12345));
        assert_eq!(sign, Sign::Positive);
    }

    #[test]
    fn test_unpack_negative() {
        let bytes = vec![0x12, 0x34, 0x5D];
        let (value, sign) = unpack_decimal(&bytes, 0).unwrap();
        assert_eq!(value, Decimal::from(-12345));
        assert_eq!(sign, Sign::Negative);
    }

    #[test]
    fn test_unpack_with_decimals() {
        let bytes = vec![0x12, 0x34, 0x5C];
        let (value, _sign) = unpack_decimal(&bytes, 2).unwrap();
        assert_eq!(value, Decimal::from_str("123.45").unwrap());
    }

    #[test]
    fn test_unpack_even_digits() {
        let bytes = vec![0x01, 0x23, 0x45, 0x6C];
        let (value, sign) = unpack_decimal(&bytes, 0).unwrap();
        assert_eq!(value, Decimal::from(123456));
        assert_eq!(sign, Sign::Positive);
    }

    #[test]
    fn test_roundtrip_5_digits() {
        let original = Decimal::from(12345);
        let packed = pack_decimal(&original, 5, 0, true).unwrap();
        let (unpacked, _sign) = unpack_decimal(&packed, 0).unwrap();
        assert_eq!(original, unpacked);
    }

    #[test]
    fn test_roundtrip_6_digits() {
        let original = Decimal::from(123456);
        let packed = pack_decimal(&original, 6, 0, true).unwrap();
        let (unpacked, _sign) = unpack_decimal(&packed, 0).unwrap();
        assert_eq!(original, unpacked);
    }

    #[test]
    fn test_roundtrip_with_decimals() {
        let original = Decimal::from_str("9876.54").unwrap();
        let packed = pack_decimal(&original, 4, 2, true).unwrap();
        let (unpacked, _sign) = unpack_decimal(&packed, 2).unwrap();
        assert_eq!(original, unpacked);
    }

    #[test]
    fn test_roundtrip_large() {
        let original = Decimal::from_str("9876543210.12").unwrap();
        let packed = pack_decimal(&original, 10, 2, true).unwrap();
        let (unpacked, _sign) = unpack_decimal(&packed, 2).unwrap();
        assert_eq!(original, unpacked);
    }

    #[test]
    fn test_max_precision() {
        // Test 18-digit precision (IBM standard)
        let value = Decimal::from_str("123456789012345678").unwrap();
        let packed = pack_decimal(&value, 18, 0, true).unwrap();
        let (unpacked, _sign) = unpack_decimal(&packed, 0).unwrap();
        assert_eq!(value, unpacked);
    }

    #[test]
    fn test_packed_decimal_struct() {
        let pd = PackedDecimal::new(Decimal::from(12345), 5, 0, true);
        assert_eq!(pd.storage_size(), 3);

        let encoded = pd.encode().unwrap();
        let decoded = PackedDecimal::decode(&encoded, 5, 0).unwrap();
        assert_eq!(pd.value, decoded.value);
    }

    #[test]
    fn test_zero() {
        let value = Decimal::from(0);
        let packed = pack_decimal(&value, 3, 0, true).unwrap();
        // +000 -> 00 0C
        assert_eq!(packed, vec![0x00, 0x0C]);

        let (unpacked, sign) = unpack_decimal(&packed, 0).unwrap();
        assert_eq!(unpacked, Decimal::from(0));
        assert_eq!(sign, Sign::Positive);
    }

    #[test]
    fn test_single_digit() {
        let value = Decimal::from(5);
        let packed = pack_decimal(&value, 1, 0, true).unwrap();
        // +5 -> 5C
        assert_eq!(packed, vec![0x5C]);

        let (unpacked, _sign) = unpack_decimal(&packed, 0).unwrap();
        assert_eq!(unpacked, Decimal::from(5));
    }

    // ─── i64 helper tests ───

    #[test]
    fn test_unpack_to_i64_positive() {
        assert_eq!(unpack_to_i64(&[0x12, 0x34, 0x5C]), 12345);
    }

    #[test]
    fn test_unpack_to_i64_negative() {
        assert_eq!(unpack_to_i64(&[0x12, 0x34, 0x5D]), -12345);
    }

    #[test]
    fn test_unpack_to_i64_unsigned() {
        assert_eq!(unpack_to_i64(&[0x12, 0x34, 0x5F]), 12345);
    }

    #[test]
    fn test_unpack_to_i64_empty() {
        assert_eq!(unpack_to_i64(&[]), 0);
    }

    #[test]
    fn test_pack_from_i64_positive() {
        let mut buf = [0u8; 3];
        pack_from_i64(12345, &mut buf);
        assert_eq!(buf, [0x12, 0x34, 0x5C]);
    }

    #[test]
    fn test_pack_from_i64_negative() {
        let mut buf = [0u8; 3];
        pack_from_i64(-12345, &mut buf);
        assert_eq!(buf, [0x12, 0x34, 0x5D]);
    }

    #[test]
    fn test_pack_from_i64_even_digits() {
        let mut buf = [0u8; 4];
        pack_from_i64(123456, &mut buf);
        assert_eq!(buf, [0x01, 0x23, 0x45, 0x6C]);
    }

    #[test]
    fn test_i64_roundtrip() {
        let mut buf = [0u8; 3];
        pack_from_i64(12345, &mut buf);
        assert_eq!(unpack_to_i64(&buf), 12345);

        pack_from_i64(-999, &mut buf);
        assert_eq!(unpack_to_i64(&buf), -999);
    }
}
