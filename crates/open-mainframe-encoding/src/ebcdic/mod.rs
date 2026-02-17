//! EBCDIC encoding support for IBM mainframe code pages.
//!
//! This module provides EBCDIC-to-ASCII and ASCII-to-EBCDIC conversion
//! for IBM code pages used in mainframe environments worldwide.
//!
//! # Supported Code Pages
//!
//! - **Base pages:** CP037, CP500, CP1047
//! - **National pages:** CP273, CP277, CP278, CP280, CP284, CP285, CP297, CP871
//! - **Euro-enabled pages:** CP1140-CP1149
//!
//! Use [`CodePageRegistry`] or [`CodePage::from_ccsid`] for runtime lookup.

pub mod collation;
mod extended_tables;
mod registry;
mod tables;

pub use extended_tables::{
    CP1140, CP1141, CP1142, CP1143, CP1144, CP1145, CP1146, CP1147, CP1148, CP1149, CP273,
    CP277, CP278, CP280, CP284, CP285, CP297, CP871,
};
pub use registry::CodePageRegistry;
pub use tables::{CodePage, CP037, CP1047, CP500};

use crate::error::EncodingError;

/// Result type for encoding operations.
pub type Result<T> = std::result::Result<T, EncodingError>;

impl CodePage {
    /// Decode EBCDIC bytes to a UTF-8 string.
    ///
    /// Handles special characters (e.g., Euro sign) that map to Unicode
    /// code points outside the Latin-1 range (U+0000-U+00FF).
    ///
    /// # Arguments
    /// * `bytes` - EBCDIC-encoded bytes
    ///
    /// # Returns
    /// A `String` containing the UTF-8 representation.
    ///
    /// # Errors
    /// Returns `EncodingError::ConversionFailed` if conversion fails.
    pub fn decode(&self, bytes: &[u8]) -> Result<String> {
        let mut result = String::with_capacity(bytes.len());
        for &b in bytes {
            if let Some(&(_, ch)) = self.special_chars.iter().find(|&&(eb, _)| eb == b) {
                result.push(ch);
            } else {
                // Map EBCDIC byte -> Latin-1 byte -> Unicode char
                result.push(char::from(self.ebcdic_to_ascii[b as usize]));
            }
        }
        Ok(result)
    }

    /// Encode a UTF-8 string to EBCDIC bytes.
    ///
    /// Handles special characters (e.g., Euro sign) that map to specific
    /// EBCDIC byte positions in this code page.
    ///
    /// # Arguments
    /// * `s` - UTF-8 string to encode
    ///
    /// # Returns
    /// A `Vec<u8>` containing the EBCDIC-encoded bytes.
    ///
    /// # Errors
    /// Returns `EncodingError::ConversionFailed` if the string contains
    /// characters that cannot be represented in the target code page.
    pub fn encode(&self, s: &str) -> Result<Vec<u8>> {
        let mut result = Vec::with_capacity(s.len());

        for ch in s.chars() {
            // Check special characters first (e.g., Euro sign)
            if let Some(&(eb, _)) = self.special_chars.iter().find(|&&(_, c)| c == ch) {
                result.push(eb);
                continue;
            }

            if ch as u32 > 255 {
                return Err(EncodingError::ConversionFailed {
                    message: format!(
                        "Character '{}' (U+{:04X}) cannot be encoded in {}",
                        ch, ch as u32, self.name
                    ),
                });
            }
            let ascii_byte = ch as u8;
            result.push(self.ascii_to_ebcdic[ascii_byte as usize]);
        }

        Ok(result)
    }

    /// Convert a single EBCDIC byte to its Unicode character.
    ///
    /// This method handles special characters (e.g., Euro sign) correctly.
    /// For pure byte-to-byte conversion, use [`ebcdic_to_ascii_byte`](CodePage::ebcdic_to_ascii_byte).
    pub fn ebcdic_to_char(&self, ebcdic: u8) -> char {
        if let Some(&(_, ch)) = self.special_chars.iter().find(|&&(eb, _)| eb == ebcdic) {
            ch
        } else {
            self.ebcdic_to_ascii[ebcdic as usize] as char
        }
    }

    /// Convert a single EBCDIC byte to ASCII/Latin-1.
    ///
    /// Note: This does NOT handle special characters like Euro sign.
    /// For full Unicode support, use [`ebcdic_to_char`](CodePage::ebcdic_to_char).
    #[inline]
    pub fn ebcdic_to_ascii_byte(&self, ebcdic: u8) -> u8 {
        self.ebcdic_to_ascii[ebcdic as usize]
    }

    /// Convert a single ASCII/Latin-1 byte to EBCDIC.
    #[inline]
    pub fn ascii_to_ebcdic_byte(&self, ascii: u8) -> u8 {
        self.ascii_to_ebcdic[ascii as usize]
    }

    /// Look up a code page by CCSID number.
    ///
    /// # Errors
    /// Returns `EncodingError::InvalidCodePage` if the CCSID is not recognized.
    pub fn from_ccsid(ccsid: u16) -> std::result::Result<&'static CodePage, EncodingError> {
        CodePageRegistry::from_ccsid(ccsid)
    }

    /// Look up a code page by name (e.g., "CP037", "IBM-1047").
    ///
    /// Accepts names in the formats: "CP037", "IBM-037", "IBM037", "EBCDIC-037".
    ///
    /// # Errors
    /// Returns `EncodingError::InvalidCodePage` if the name is not recognized.
    pub fn by_name(name: &str) -> std::result::Result<&'static CodePage, EncodingError> {
        CodePageRegistry::by_name(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // =========================================================================
    // Base code page tests
    // =========================================================================

    #[test]
    fn test_cp037_roundtrip() {
        let original = "HELLO WORLD";
        let encoded = CP037.encode(original).unwrap();
        let decoded = CP037.decode(&encoded).unwrap();
        assert_eq!(decoded, original);
    }

    #[test]
    fn test_cp1047_roundtrip() {
        let original = "HELLO WORLD";
        let encoded = CP1047.encode(original).unwrap();
        let decoded = CP1047.decode(&encoded).unwrap();
        assert_eq!(decoded, original);
    }

    #[test]
    fn test_cp500_roundtrip() {
        let original = "HELLO WORLD";
        let encoded = CP500.encode(original).unwrap();
        let decoded = CP500.decode(&encoded).unwrap();
        assert_eq!(decoded, original);
    }

    #[test]
    fn test_hello_world_cp037() {
        let hello_ebcdic = CP037.encode("HELLO").unwrap();
        assert_eq!(hello_ebcdic, vec![0xC8, 0xC5, 0xD3, 0xD3, 0xD6]);
    }

    #[test]
    fn test_digits_cp037() {
        let digits = "0123456789";
        let encoded = CP037.encode(digits).unwrap();
        assert_eq!(
            encoded,
            vec![0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9]
        );
    }

    #[test]
    fn test_special_characters_cp037() {
        let space_encoded = CP037.encode(" ").unwrap();
        assert_eq!(space_encoded, vec![0x40]);
    }

    #[test]
    fn test_ccsid_field() {
        assert_eq!(CP037.ccsid, 37);
        assert_eq!(CP1047.ccsid, 1047);
        assert_eq!(CP500.ccsid, 500);
    }

    // =========================================================================
    // Euro code page tests (Story 700.1)
    // =========================================================================

    #[test]
    fn test_cp1140_euro_sign_decode() {
        // AC: Given data encoded in CP1140, When byte 0x9F is decoded,
        // Then the Euro sign (€, U+20AC) is produced
        let decoded = CP1140.decode(&[0x9F]).unwrap();
        assert_eq!(decoded, "€");
    }

    #[test]
    fn test_cp1140_euro_sign_encode() {
        // Euro sign should encode to 0x9F
        let encoded = CP1140.encode("€").unwrap();
        assert_eq!(encoded, vec![0x9F]);
    }

    #[test]
    fn test_cp1140_euro_sign_roundtrip() {
        let original = "Price: 100€";
        let encoded = CP1140.encode(original).unwrap();
        let decoded = CP1140.decode(&encoded).unwrap();
        assert_eq!(decoded, original);
    }

    #[test]
    fn test_cp1140_same_as_cp037_except_9f() {
        // CP1140 should be identical to CP037 except at position 0x9F
        for i in 0u8..=255 {
            if i == 0x9F {
                continue;
            }
            assert_eq!(
                CP1140.ebcdic_to_ascii_byte(i),
                CP037.ebcdic_to_ascii_byte(i),
                "CP1140 and CP037 differ at EBCDIC 0x{:02X}",
                i
            );
        }
    }

    #[test]
    fn test_all_euro_pages_decode_euro_at_9f() {
        // AC: Given all 10 Euro code pages (CP1140-1149),
        // When byte 0x9F is decoded, Then Euro sign is produced
        let euro_pages: &[&CodePage] = &[
            &CP1140, &CP1141, &CP1142, &CP1143, &CP1144, &CP1145, &CP1146, &CP1147, &CP1148,
            &CP1149,
        ];
        for cp in euro_pages {
            let decoded = cp.decode(&[0x9F]).unwrap();
            assert_eq!(decoded, "€", "CP{} failed to decode Euro sign at 0x9F", cp.name);
        }
    }

    #[test]
    fn test_all_euro_pages_encode_euro_to_9f() {
        let euro_pages: &[&CodePage] = &[
            &CP1140, &CP1141, &CP1142, &CP1143, &CP1144, &CP1145, &CP1146, &CP1147, &CP1148,
            &CP1149,
        ];
        for cp in euro_pages {
            let encoded = cp.encode("€").unwrap();
            assert_eq!(encoded, vec![0x9F], "CP{} failed to encode Euro sign to 0x9F", cp.name);
        }
    }

    #[test]
    fn test_euro_page_roundtrip_all_bytes() {
        // AC: Given all 10 Euro code pages, When roundtrip-tested for all 256 byte values,
        // Then encode(decode(byte)) == byte for all values
        let euro_pages: &[&CodePage] = &[
            &CP1140, &CP1141, &CP1142, &CP1143, &CP1144, &CP1145, &CP1146, &CP1147, &CP1148,
            &CP1149,
        ];
        for cp in euro_pages {
            for b in 0u8..=255 {
                let decoded = cp.decode(&[b]).unwrap();
                let re_encoded = cp.encode(&decoded).unwrap();
                assert_eq!(
                    re_encoded,
                    vec![b],
                    "{}: roundtrip failed for byte 0x{:02X} -> '{}' -> {:?}",
                    cp.name,
                    b,
                    decoded,
                    re_encoded
                );
            }
        }
    }

    #[test]
    fn test_cp1148_euro_same_as_cp500_except_9f() {
        // CP1148 is Euro variant of CP500
        for i in 0u8..=255 {
            if i == 0x9F {
                continue;
            }
            assert_eq!(
                CP1148.ebcdic_to_ascii_byte(i),
                CP500.ebcdic_to_ascii_byte(i),
                "CP1148 and CP500 differ at EBCDIC 0x{:02X}",
                i
            );
        }
    }

    // =========================================================================
    // National code page tests (Story 700.2)
    // =========================================================================

    #[test]
    fn test_cp273_german_characters() {
        // AC: Given German data encoded in CP273,
        // When EBCDIC bytes for ä, ö, ü, ß, Ä, Ö, Ü are decoded,
        // Then the correct Unicode characters are produced
        assert_eq!(CP273.ebcdic_to_char(0xC0), 'ä');
        assert_eq!(CP273.ebcdic_to_char(0x6A), 'ö');
        assert_eq!(CP273.ebcdic_to_char(0xD0), 'ü');
        assert_eq!(CP273.ebcdic_to_char(0xA1), 'ß');
        assert_eq!(CP273.ebcdic_to_char(0x4A), 'Ä');
        assert_eq!(CP273.ebcdic_to_char(0xE0), 'Ö');
        assert_eq!(CP273.ebcdic_to_char(0x5A), 'Ü');
    }

    #[test]
    fn test_cp273_roundtrip_ascii_subset() {
        // AC: Given any supported national code page,
        // When all printable ASCII characters are roundtrip-tested,
        // Then encode(decode(byte)) == byte for the ASCII subset
        // Note: roundtrip for ALL 256 bytes
        for b in 0u8..=255 {
            let decoded = CP273.decode(&[b]).unwrap();
            let re_encoded = CP273.encode(&decoded).unwrap();
            assert_eq!(
                re_encoded,
                vec![b],
                "CP273 roundtrip failed for byte 0x{:02X}",
                b
            );
        }
    }

    #[test]
    fn test_national_pages_roundtrip_all_bytes() {
        let national_pages: &[&CodePage] =
            &[&CP273, &CP277, &CP278, &CP280, &CP284, &CP285, &CP297, &CP871];
        for cp in national_pages {
            for b in 0u8..=255 {
                let decoded = cp.decode(&[b]).unwrap();
                let re_encoded = cp.encode(&decoded).unwrap();
                assert_eq!(
                    re_encoded,
                    vec![b],
                    "{}: roundtrip failed for byte 0x{:02X} -> '{}' -> {:?}",
                    cp.name,
                    b,
                    decoded,
                    re_encoded
                );
            }
        }
    }

    #[test]
    fn test_national_pages_letters_and_digits() {
        // All national pages should share the same letter and digit positions as CP037
        let national_pages: &[&CodePage] =
            &[&CP273, &CP277, &CP278, &CP280, &CP284, &CP285, &CP297, &CP871];
        for cp in national_pages {
            // Digits 0-9 are always at 0xF0-0xF9
            for (i, expected) in (b'0'..=b'9').enumerate() {
                let ebcdic = 0xF0 + i as u8;
                assert_eq!(
                    cp.ebcdic_to_ascii_byte(ebcdic),
                    expected,
                    "{}: digit {} at 0x{:02X}",
                    cp.name,
                    expected as char,
                    ebcdic
                );
            }
            // Uppercase A-I at 0xC1-0xC9
            for (i, expected) in (b'A'..=b'I').enumerate() {
                let ebcdic = 0xC1 + i as u8;
                assert_eq!(
                    cp.ebcdic_to_ascii_byte(ebcdic),
                    expected,
                    "{}: letter {} at 0x{:02X}",
                    cp.name,
                    expected as char,
                    ebcdic
                );
            }
            // Lowercase a-i at 0x81-0x89
            for (i, expected) in (b'a'..=b'i').enumerate() {
                let ebcdic = 0x81 + i as u8;
                assert_eq!(
                    cp.ebcdic_to_ascii_byte(ebcdic),
                    expected,
                    "{}: letter {} at 0x{:02X}",
                    cp.name,
                    expected as char,
                    ebcdic
                );
            }
        }
    }

    #[test]
    fn test_cp277_danish_characters() {
        // Danish/Norwegian: Æ, Ø, Å and lowercase
        assert_eq!(CP277.ebcdic_to_char(0xC0), 'æ');
        assert_eq!(CP277.ebcdic_to_char(0xD0), 'å');
        assert_eq!(CP277.ebcdic_to_char(0x6A), 'ø');
        assert_eq!(CP277.ebcdic_to_char(0x7B), 'Æ');
        assert_eq!(CP277.ebcdic_to_char(0x7C), 'Ø');
        assert_eq!(CP277.ebcdic_to_char(0x5B), 'Å');
    }

    #[test]
    fn test_cp285_uk_pound_sign() {
        // UK: £ at position 0x5B (where $ is in CP037)
        assert_eq!(CP285.ebcdic_to_char(0x5B), '£');
        assert_eq!(CP285.ebcdic_to_char(0x4A), '$');
    }

    #[test]
    fn test_cp297_french_characters() {
        // French: é, è, à, ù, ç
        assert_eq!(CP297.ebcdic_to_char(0xC0), 'é');
        assert_eq!(CP297.ebcdic_to_char(0xD0), 'è');
        assert_eq!(CP297.ebcdic_to_char(0x7C), 'à');
        assert_eq!(CP297.ebcdic_to_char(0x6A), 'ù');
        assert_eq!(CP297.ebcdic_to_char(0xE0), 'ç');
    }

    // =========================================================================
    // Registry tests (Story 700.3)
    // =========================================================================

    #[test]
    fn test_codepage_from_ccsid() {
        // AC: Given CodePage::from_ccsid(37), Then CP037 is returned
        let cp = CodePage::from_ccsid(37).unwrap();
        assert_eq!(cp.name, "CP037");
    }

    #[test]
    fn test_codepage_by_name() {
        // AC: Given CodePage::by_name("IBM-1047"), Then CP1047 is returned
        let cp = CodePage::by_name("IBM-1047").unwrap();
        assert_eq!(cp.ccsid, 1047);
    }

    #[test]
    fn test_codepage_from_ccsid_unknown() {
        // AC: Given an unknown CCSID (99999), Then InvalidCodePage error
        let err = CodePage::from_ccsid(65535).unwrap_err();
        assert!(matches!(err, EncodingError::InvalidCodePage(_)));
    }

    #[test]
    fn test_ebcdic_to_char_basic() {
        assert_eq!(CP037.ebcdic_to_char(0xC1), 'A');
        assert_eq!(CP037.ebcdic_to_char(0xF0), '0');
        assert_eq!(CP037.ebcdic_to_char(0x40), ' ');
    }

    #[test]
    fn test_ebcdic_to_char_euro() {
        assert_eq!(CP1140.ebcdic_to_char(0x9F), '€');
    }

    #[test]
    fn test_encode_unsupported_char() {
        // Non-Euro code pages should reject Euro sign
        let err = CP037.encode("€").unwrap_err();
        assert!(matches!(err, EncodingError::ConversionFailed { .. }));
    }
}
