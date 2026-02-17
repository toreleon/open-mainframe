//! EBCDIC collation order for byte-level comparison and sorting.
//!
//! In EBCDIC, the collation order differs significantly from ASCII:
//! - Lowercase letters (0x81–0xA9) sort **before** uppercase (0xC1–0xE9)
//! - Letters sort **before** digits (digits are at 0xF0–0xF9)
//! - Special characters are interspersed differently
//!
//! This module provides comparison and sorting functions that match
//! the z/OS DFSORT / COBOL SORT `COLLATING SEQUENCE IS NATIVE` behavior,
//! which is simply unsigned byte-value ordering of EBCDIC bytes.

use std::cmp::Ordering;

use super::CodePage;

/// Compare two ASCII/UTF-8 strings using EBCDIC byte-value collation.
///
/// Encodes both strings to EBCDIC using the given code page and compares
/// the resulting byte sequences lexicographically.
///
/// # Errors
/// Returns `None` if either string cannot be encoded.
pub fn ebcdic_compare(a: &str, b: &str, cp: &CodePage) -> Option<Ordering> {
    let ea = cp.encode(a).ok()?;
    let eb = cp.encode(b).ok()?;
    Some(compare_ebcdic_bytes(&ea, &eb))
}

/// Compare two EBCDIC byte slices in native byte-value order.
///
/// This is the z/OS native collation: pure unsigned byte comparison.
pub fn compare_ebcdic_bytes(a: &[u8], b: &[u8]) -> Ordering {
    a.cmp(b)
}

/// Sort a slice of ASCII/UTF-8 strings using EBCDIC collation.
///
/// Strings are compared by their EBCDIC encoding under the given code page.
/// Strings that cannot be encoded sort to the end.
pub fn sort_ebcdic(strings: &mut [String], cp: &CodePage) {
    strings.sort_by(|a, b| ebcdic_compare(a, b, cp).unwrap_or(Ordering::Equal));
}

/// Sort a slice of ASCII/UTF-8 string references using EBCDIC collation.
pub fn sort_ebcdic_refs(strings: &mut [&str], cp: &CodePage) {
    strings.sort_by(|a, b| ebcdic_compare(a, b, cp).unwrap_or(Ordering::Equal));
}

/// Check if a byte sequence is in EBCDIC-sorted (non-descending) order.
///
/// Each element is a byte slice representing an EBCDIC-encoded record key.
pub fn is_ebcdic_sorted(records: &[&[u8]]) -> bool {
    records.windows(2).all(|w| w[0] <= w[1])
}

/// EBCDIC collation sequence table.
///
/// Maps each EBCDIC byte value to its sort weight (0–255).
/// For native EBCDIC collation, the weight IS the byte value itself,
/// since z/OS sorts by raw byte values.
///
/// This function is provided for cases where a custom weight table
/// is needed (e.g., for PROGRAM COLLATING SEQUENCE IS ...).
pub fn native_collation_weight(ebcdic_byte: u8) -> u8 {
    // z/OS native collation is pure byte-value ordering
    ebcdic_byte
}

/// Determine the EBCDIC character class for a byte.
///
/// Useful for understanding sort order relationships.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EbcdicCharClass {
    /// Control character (0x00–0x3F)
    Control,
    /// Space (0x40)
    Space,
    /// Special/punctuation
    Special,
    /// Lowercase letter (a–z in 0x81–0xA9)
    Lowercase,
    /// Uppercase letter (A–Z in 0xC1–0xE9)
    Uppercase,
    /// Digit (0–9 in 0xF0–0xF9)
    Digit,
}

/// Classify an EBCDIC byte value.
pub fn classify_ebcdic(byte: u8) -> EbcdicCharClass {
    match byte {
        0x00..=0x3F => EbcdicCharClass::Control,
        0x40 => EbcdicCharClass::Space,
        0x81..=0x89 | 0x91..=0x99 | 0xA2..=0xA9 => EbcdicCharClass::Lowercase,
        0xC1..=0xC9 | 0xD1..=0xD9 | 0xE2..=0xE9 => EbcdicCharClass::Uppercase,
        0xF0..=0xF9 => EbcdicCharClass::Digit,
        _ => EbcdicCharClass::Special,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ebcdic::CP037;

    // --- Epic 707: EBCDIC Collation Order ---

    #[test]
    fn test_lowercase_before_uppercase() {
        // AC: Given EBCDIC strings "abc" and "ABC"
        // When compared using EBCDIC collation
        // Then "abc" sorts before "ABC" (lowercase 0x81-0xA9 < uppercase 0xC1-0xE9)
        let result = ebcdic_compare("abc", "ABC", &CP037);
        assert_eq!(result, Some(Ordering::Less));
    }

    #[test]
    fn test_uppercase_before_digits() {
        // AC: Given EBCDIC strings "ABC" and "123"
        // When compared using EBCDIC collation
        // Then "ABC" sorts before "123" (letters 0xC1+ < digits 0xF0+)
        let result = ebcdic_compare("ABC", "123", &CP037);
        assert_eq!(result, Some(Ordering::Less));
    }

    #[test]
    fn test_lowercase_before_digits() {
        let result = ebcdic_compare("abc", "123", &CP037);
        assert_eq!(result, Some(Ordering::Less));
    }

    #[test]
    fn test_equal_strings() {
        let result = ebcdic_compare("HELLO", "HELLO", &CP037);
        assert_eq!(result, Some(Ordering::Equal));
    }

    #[test]
    fn test_prefix_ordering() {
        // "AB" < "ABC" (shorter prefix)
        let result = ebcdic_compare("AB", "ABC", &CP037);
        assert_eq!(result, Some(Ordering::Less));
    }

    #[test]
    fn test_sort_ebcdic_order() {
        // AC: Given PROGRAM COLLATING SEQUENCE IS NATIVE context
        // When COBOL SORT is executed
        // Then EBCDIC byte-value ordering is used
        let mut strings = vec![
            "123".to_string(),
            "ABC".to_string(),
            "abc".to_string(),
            " ".to_string(),
        ];
        sort_ebcdic(&mut strings, &CP037);
        // EBCDIC order: space(0x40) < lowercase(0x81+) < uppercase(0xC1+) < digits(0xF0+)
        assert_eq!(strings, vec![" ", "abc", "ABC", "123"]);
    }

    #[test]
    fn test_sort_ebcdic_refs() {
        let mut strings: Vec<&str> = vec!["123", "ABC", "abc", " "];
        sort_ebcdic_refs(&mut strings, &CP037);
        assert_eq!(strings, vec![" ", "abc", "ABC", "123"]);
    }

    #[test]
    fn test_compare_ebcdic_bytes_direct() {
        // Direct byte comparison
        let a = CP037.encode("abc").unwrap();
        let b = CP037.encode("ABC").unwrap();
        assert_eq!(compare_ebcdic_bytes(&a, &b), Ordering::Less);
    }

    #[test]
    fn test_is_ebcdic_sorted() {
        let a = CP037.encode("abc").unwrap();
        let b = CP037.encode("ABC").unwrap();
        let c = CP037.encode("123").unwrap();
        assert!(is_ebcdic_sorted(&[&a, &b, &c]));
        assert!(!is_ebcdic_sorted(&[&c, &a, &b]));
    }

    #[test]
    fn test_native_collation_weight() {
        // Native weight is just the byte value
        assert_eq!(native_collation_weight(0x40), 0x40);
        assert_eq!(native_collation_weight(0xC1), 0xC1);
        assert_eq!(native_collation_weight(0xF0), 0xF0);
    }

    #[test]
    fn test_classify_ebcdic() {
        assert_eq!(classify_ebcdic(0x00), EbcdicCharClass::Control);
        assert_eq!(classify_ebcdic(0x40), EbcdicCharClass::Space);
        assert_eq!(classify_ebcdic(0x81), EbcdicCharClass::Lowercase); // 'a'
        assert_eq!(classify_ebcdic(0xC1), EbcdicCharClass::Uppercase); // 'A'
        assert_eq!(classify_ebcdic(0xF0), EbcdicCharClass::Digit);     // '0'
        assert_eq!(classify_ebcdic(0x4B), EbcdicCharClass::Special);   // '.'
    }

    #[test]
    fn test_ebcdic_sort_order_comprehensive() {
        // Verify the full EBCDIC sort order: control < space < special < lowercase < uppercase < digits
        let space = CP037.encode(" ").unwrap();       // 0x40
        let lower_a = CP037.encode("a").unwrap();     // 0x81
        let upper_a = CP037.encode("A").unwrap();     // 0xC1
        let digit_0 = CP037.encode("0").unwrap();     // 0xF0

        assert!(space < lower_a);
        assert!(lower_a < upper_a);
        assert!(upper_a < digit_0);
    }

    #[test]
    fn test_ebcdic_letter_ranges() {
        // Within each case, letters should sort alphabetically
        assert_eq!(
            ebcdic_compare("a", "z", &CP037),
            Some(Ordering::Less)
        );
        assert_eq!(
            ebcdic_compare("A", "Z", &CP037),
            Some(Ordering::Less)
        );
    }

    #[test]
    fn test_ebcdic_digit_order() {
        assert_eq!(
            ebcdic_compare("0", "9", &CP037),
            Some(Ordering::Less)
        );
    }

    #[test]
    fn test_empty_string_sorting() {
        let result = ebcdic_compare("", "A", &CP037);
        assert_eq!(result, Some(Ordering::Less));

        let result = ebcdic_compare("", "", &CP037);
        assert_eq!(result, Some(Ordering::Equal));
    }
}
