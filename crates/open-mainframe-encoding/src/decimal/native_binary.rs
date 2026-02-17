//! COMP-5 native binary encoding and decoding.
//!
//! COMP-5 (native binary) stores integers in big-endian two's complement
//! format, like COMP/COMP-4. The key difference is that COMP-5 uses the
//! **full storage range** rather than being limited by PIC digit count.
//!
//! # Difference from COMP
//!
//! | Format | PIC S9(4) halfword range |
//! |--------|-------------------------|
//! | COMP   | -9999 to +9999          |
//! | COMP-5 | -32768 to +32767        |
//!
//! COMP-5 values can use the full range of the underlying storage size:
//! - Halfword (2 bytes): signed -32768..32767, unsigned 0..65535
//! - Fullword (4 bytes): signed -2147483648..2147483647, unsigned 0..4294967295
//! - Doubleword (8 bytes): signed -9223372036854775808..9223372036854775807

use rust_decimal::Decimal;

use super::Result;
use super::binary::{decode_binary, encode_binary, storage_size_for_digits};
use crate::error::EncodingError;

/// Native binary integer (COMP-5) value with metadata.
///
/// Unlike [`super::BinaryInteger`] (COMP/COMP-4), this type allows the full
/// storage range for the allocated size. The PIC clause determines storage
/// size but does NOT limit the value range.
#[derive(Debug, Clone, PartialEq)]
pub struct NativeBinaryInteger {
    /// The integer value.
    pub value: Decimal,
    /// Number of digits in the PIC clause (determines storage size only).
    pub digits: usize,
    /// Whether the value is signed.
    pub signed: bool,
}

impl NativeBinaryInteger {
    /// Create a new COMP-5 native binary integer.
    pub fn new(value: Decimal, digits: usize, signed: bool) -> Self {
        Self {
            value,
            digits,
            signed,
        }
    }

    /// Calculate the storage size in bytes (same rules as COMP).
    ///
    /// - 1-4 digits: 2 bytes (halfword)
    /// - 5-9 digits: 4 bytes (fullword)
    /// - 10-18 digits: 8 bytes (doubleword)
    pub fn storage_size(&self) -> usize {
        storage_size_for_digits(self.digits)
    }

    /// Encode the value to binary bytes.
    ///
    /// Uses the same big-endian encoding as COMP, but the full storage
    /// range is allowed (validated by `encode_binary` against storage size).
    pub fn encode(&self) -> Result<Vec<u8>> {
        // COMP-5 reuses the same encoding; encode_binary already validates
        // against storage-size range (not PIC-digit range).
        encode_binary(&self.value, self.digits, self.signed)
    }

    /// Decode binary bytes to a COMP-5 value.
    pub fn decode(bytes: &[u8], signed: bool) -> Result<Self> {
        let value = decode_binary(bytes, signed)?;
        let digits = digits_for_native_size(bytes.len());
        Ok(Self {
            value,
            digits,
            signed,
        })
    }

    /// Get the minimum allowed value for this specification.
    pub fn min_value(&self) -> i64 {
        let size = self.storage_size();
        if self.signed {
            match size {
                2 => i16::MIN as i64,
                4 => i32::MIN as i64,
                8 => i64::MIN,
                _ => i64::MIN,
            }
        } else {
            0
        }
    }

    /// Get the maximum allowed value for this specification.
    pub fn max_value(&self) -> i64 {
        let size = self.storage_size();
        if self.signed {
            match size {
                2 => i16::MAX as i64,
                4 => i32::MAX as i64,
                8 => i64::MAX,
                _ => i64::MAX,
            }
        } else {
            match size {
                2 => u16::MAX as i64,
                4 => u32::MAX as i64,
                // u64::MAX doesn't fit in i64, return i64::MAX
                8 => i64::MAX,
                _ => i64::MAX,
            }
        }
    }
}

/// Infer a reasonable digit count from storage size (for decoding).
fn digits_for_native_size(size: usize) -> usize {
    match size {
        2 => 4,
        4 => 9,
        8 => 18,
        _ => 18,
    }
}

/// Encode a value as COMP-5 native binary.
///
/// This is a convenience function equivalent to [`encode_binary`] since
/// the byte-level encoding is identical — the distinction between COMP
/// and COMP-5 is purely in range validation semantics at a higher level.
pub fn encode_native_binary(value: &Decimal, digits: usize, signed: bool) -> Result<Vec<u8>> {
    encode_binary(value, digits, signed)
}

/// Decode COMP-5 native binary bytes.
///
/// Identical to [`decode_binary`] at the byte level.
pub fn decode_native_binary(bytes: &[u8], signed: bool) -> Result<Decimal> {
    decode_binary(bytes, signed)
}

/// Validate that a value fits within COMP-5 storage range.
///
/// Unlike COMP where the range is limited by PIC digits (e.g. PIC S9(4)
/// limits to -9999..9999), COMP-5 allows the full storage range
/// (e.g. PIC S9(4) allows -32768..32767).
pub fn validate_native_range(value: i64, digits: usize, signed: bool) -> Result<()> {
    let size = storage_size_for_digits(digits);
    let (min, max): (i64, i64) = if signed {
        match size {
            2 => (i16::MIN as i64, i16::MAX as i64),
            4 => (i32::MIN as i64, i32::MAX as i64),
            _ => (i64::MIN, i64::MAX),
        }
    } else {
        match size {
            2 => (0, u16::MAX as i64),
            4 => (0, u32::MAX as i64),
            _ => (0, i64::MAX),
        }
    };

    if value < min || value > max {
        return Err(EncodingError::OutOfRange {
            value: value.to_string(),
            min: min.to_string(),
            max: max.to_string(),
        });
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_comp5_allows_full_halfword_range() {
        // COMP PIC S9(4) limits to -9999..9999, but COMP-5 allows -32768..32767
        let value = Decimal::from(32767);
        let encoded = encode_native_binary(&value, 4, true).unwrap();
        assert_eq!(encoded, vec![0x7F, 0xFF]);

        let decoded = decode_native_binary(&encoded, true).unwrap();
        assert_eq!(decoded, value);
    }

    #[test]
    fn test_comp5_negative_halfword_min() {
        let value = Decimal::from(-32768);
        let encoded = encode_native_binary(&value, 4, true).unwrap();
        assert_eq!(encoded, vec![0x80, 0x00]);

        let decoded = decode_native_binary(&encoded, true).unwrap();
        assert_eq!(decoded, value);
    }

    #[test]
    fn test_comp5_unsigned_halfword_max() {
        let value = Decimal::from(65535);
        let encoded = encode_native_binary(&value, 4, false).unwrap();
        assert_eq!(encoded, vec![0xFF, 0xFF]);

        let decoded = decode_native_binary(&encoded, false).unwrap();
        assert_eq!(decoded, value);
    }

    #[test]
    fn test_comp5_fullword_range() {
        // COMP PIC S9(9) limits to -999999999..999999999
        // COMP-5 allows -2147483648..2147483647
        let value = Decimal::from(2147483647_i64);
        let encoded = encode_native_binary(&value, 9, true).unwrap();
        assert_eq!(encoded, vec![0x7F, 0xFF, 0xFF, 0xFF]);
    }

    #[test]
    fn test_comp5_fullword_negative_min() {
        let value = Decimal::from(-2147483648_i64);
        let encoded = encode_native_binary(&value, 9, true).unwrap();
        assert_eq!(encoded, vec![0x80, 0x00, 0x00, 0x00]);
    }

    #[test]
    fn test_comp5_doubleword() {
        let value = Decimal::from_str("9223372036854775807").unwrap(); // i64::MAX
        let encoded = encode_native_binary(&value, 18, true).unwrap();
        assert_eq!(
            encoded,
            vec![0x7F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]
        );
    }

    #[test]
    fn test_validate_native_range_ok() {
        // Full halfword range for COMP-5 PIC S9(4)
        assert!(validate_native_range(32767, 4, true).is_ok());
        assert!(validate_native_range(-32768, 4, true).is_ok());
        assert!(validate_native_range(0, 4, true).is_ok());
    }

    #[test]
    fn test_validate_native_range_out_of_range() {
        // 32768 exceeds signed halfword
        assert!(validate_native_range(32768, 4, true).is_err());
        assert!(validate_native_range(-32769, 4, true).is_err());
    }

    #[test]
    fn test_validate_native_range_unsigned() {
        assert!(validate_native_range(65535, 4, false).is_ok());
        assert!(validate_native_range(65536, 4, false).is_err());
        assert!(validate_native_range(-1, 4, false).is_err());
    }

    #[test]
    fn test_native_binary_integer_struct() {
        let nbi = NativeBinaryInteger::new(Decimal::from(30000), 4, true);
        assert_eq!(nbi.storage_size(), 2);
        assert_eq!(nbi.min_value(), -32768);
        assert_eq!(nbi.max_value(), 32767);

        let encoded = nbi.encode().unwrap();
        let decoded = NativeBinaryInteger::decode(&encoded, true).unwrap();
        assert_eq!(nbi.value, decoded.value);
    }

    #[test]
    fn test_native_binary_integer_roundtrip() {
        for val in &[-32768_i64, -1, 0, 1, 32767] {
            let nbi = NativeBinaryInteger::new(Decimal::from(*val), 4, true);
            let encoded = nbi.encode().unwrap();
            let decoded = NativeBinaryInteger::decode(&encoded, true).unwrap();
            assert_eq!(nbi.value, decoded.value, "roundtrip failed for {}", val);
        }
    }

    #[test]
    fn test_comp5_zero() {
        let value = Decimal::from(0);
        let encoded = encode_native_binary(&value, 4, true).unwrap();
        assert_eq!(encoded, vec![0x00, 0x00]);
    }
}
