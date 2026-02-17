//! EBCDIC encoding and decimal arithmetic for OpenMainframe.
//!
//! This crate provides data encoding and conversion facilities for
//! processing mainframe data on Linux systems.
//!
//! # Features
//!
//! - **EBCDIC Conversion**: 21 IBM code pages (CP037, CP500, CP1047, national, Euro-enabled)
//! - **Packed Decimal**: COMP-3 BCD encoding with 18-digit precision
//! - **Zoned Decimal**: DISPLAY format numeric encoding
//! - **Binary Integer**: COMP/COMP-4 big-endian integer encoding
//!
//! # Example
//!
//! ```rust
//! use open_mainframe_encoding::ebcdic::{CP037, CodePage};
//! use open_mainframe_encoding::decimal::{pack_decimal, unpack_decimal};
//! use rust_decimal::Decimal;
//! use std::str::FromStr;
//!
//! // EBCDIC conversion
//! let text = "HELLO";
//! let ebcdic = CP037.encode(text).unwrap();
//! let ascii = CP037.decode(&ebcdic).unwrap();
//! assert_eq!(ascii, text);
//!
//! // Packed decimal
//! let value = Decimal::from_str("12345").unwrap();
//! let packed = pack_decimal(&value, 5, 0, true).unwrap();
//! let (unpacked, _sign) = unpack_decimal(&packed, 0).unwrap();
//! assert_eq!(value, unpacked);
//! ```

pub mod decimal;
pub mod ebcdic;
pub mod error;

// Re-export commonly used types at crate root
pub use decimal::{
    decode_binary, decode_hfp_long, decode_hfp_short, decode_ieee_long, decode_ieee_short,
    decode_native_binary, encode_binary, encode_hfp_long, encode_hfp_short, encode_ieee_long,
    encode_ieee_long_into, encode_ieee_short, encode_ieee_short_into, encode_native_binary,
    hfp_long_to_ieee, hfp_short_to_ieee, ieee_to_hfp_long, ieee_to_hfp_short, pack_decimal,
    unpack_decimal, unzone_decimal, validate_native_range, zone_decimal, BinaryInteger, HfpDouble,
    HfpFloat, IeeeDouble, IeeeFloat, NativeBinaryInteger, PackedDecimal, Sign, ZonedDecimal,
};
pub use ebcdic::{
    collation::{
        classify_ebcdic, compare_ebcdic_bytes, ebcdic_compare, is_ebcdic_sorted,
        native_collation_weight, sort_ebcdic, sort_ebcdic_refs, EbcdicCharClass,
    },
    CodePage, CodePageRegistry, CP037, CP1047, CP1140, CP1141, CP1142, CP1143, CP1144, CP1145,
    CP1146, CP1147, CP1148, CP1149, CP273, CP277, CP278, CP280, CP284, CP285, CP297, CP500,
    CP871,
};
pub use error::EncodingError;

/// Result type for encoding operations.
pub type Result<T> = std::result::Result<T, EncodingError>;
