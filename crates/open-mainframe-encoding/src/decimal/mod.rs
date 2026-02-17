//! Decimal data type support for COBOL numeric formats.
//!
//! This module provides encoding and decoding for the three main
//! COBOL numeric storage formats:
//!
//! - **Packed Decimal (COMP-3)**: BCD encoding with sign nibble
//! - **Zoned Decimal (DISPLAY)**: One digit per byte with zone nibbles
//! - **Binary (COMP/COMP-4)**: Big-endian two's complement integers

mod binary;
mod floating;
mod native_binary;
mod packed;
mod zoned;

pub use binary::{decode_binary, encode_binary, BinaryInteger};
pub use floating::{
    decode_hfp_long, decode_hfp_short, encode_hfp_long, encode_hfp_short, hfp_long_to_ieee,
    hfp_short_to_ieee, ieee_to_hfp_long, ieee_to_hfp_short, HfpDouble, HfpFloat,
};
pub use native_binary::{
    decode_native_binary, encode_native_binary, validate_native_range, NativeBinaryInteger,
};
pub use packed::{pack_decimal, unpack_decimal, PackedDecimal};
pub use zoned::{unzone_decimal, zone_decimal, ZonedDecimal};

use crate::error::EncodingError;

/// Result type for decimal operations.
pub type Result<T> = std::result::Result<T, EncodingError>;

/// Sign representation in COBOL numeric formats.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sign {
    /// Positive value (sign nibble 0xC or 0xF)
    Positive,
    /// Negative value (sign nibble 0xD)
    Negative,
    /// Unsigned value (sign nibble 0xF, treated as positive)
    Unsigned,
}

impl Sign {
    /// Convert sign to the standard positive packed decimal sign nibble (0xC).
    pub fn to_packed_positive_nibble(self) -> u8 {
        match self {
            Sign::Positive | Sign::Unsigned => 0x0C,
            Sign::Negative => 0x0D,
        }
    }

    /// Convert sign to the preferred packed decimal sign nibble.
    /// Uses 0xF for unsigned, 0xC for positive, 0xD for negative.
    pub fn to_packed_nibble(self) -> u8 {
        match self {
            Sign::Positive => 0x0C,
            Sign::Negative => 0x0D,
            Sign::Unsigned => 0x0F,
        }
    }

    /// Parse a packed decimal sign nibble.
    pub fn from_packed_nibble(nibble: u8) -> Result<Self> {
        match nibble & 0x0F {
            0x0C | 0x0A | 0x0E => Ok(Sign::Positive),
            0x0D | 0x0B => Ok(Sign::Negative),
            0x0F => Ok(Sign::Unsigned),
            _ => Err(EncodingError::ConversionFailed {
                message: format!("Invalid sign nibble: 0x{:X}", nibble),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sign_to_packed_nibble() {
        assert_eq!(Sign::Positive.to_packed_nibble(), 0x0C);
        assert_eq!(Sign::Negative.to_packed_nibble(), 0x0D);
        assert_eq!(Sign::Unsigned.to_packed_nibble(), 0x0F);
    }

    #[test]
    fn test_sign_from_packed_nibble() {
        assert_eq!(Sign::from_packed_nibble(0x0C).unwrap(), Sign::Positive);
        assert_eq!(Sign::from_packed_nibble(0x0D).unwrap(), Sign::Negative);
        assert_eq!(Sign::from_packed_nibble(0x0F).unwrap(), Sign::Unsigned);
        // Alternative positive signs
        assert_eq!(Sign::from_packed_nibble(0x0A).unwrap(), Sign::Positive);
        assert_eq!(Sign::from_packed_nibble(0x0E).unwrap(), Sign::Positive);
        // Alternative negative sign
        assert_eq!(Sign::from_packed_nibble(0x0B).unwrap(), Sign::Negative);
    }
}
