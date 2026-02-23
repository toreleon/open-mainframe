//! LE Bit Manipulation Services — CEESICLR/CEESISET/CEESISHF/CEESITST.
//!
//! Implements the z/OS Language Environment bit manipulation callable services
//! that operate on 32-bit integer values.

use crate::date_time::FeedbackCode;

/// CEESICLR — Clear (zero) a specific bit in a 32-bit integer.
///
/// Bit numbering: 0 = most significant bit, 31 = least significant bit
/// (IBM convention, big-endian bit ordering).
pub fn ceesiclr(target: u32, bit: u32) -> (u32, FeedbackCode) {
    if bit > 31 {
        return (target, FeedbackCode::error(2601));
    }
    let mask = !(1u32 << (31 - bit));
    (target & mask, FeedbackCode::success())
}

/// CEESISET — Set (to 1) a specific bit in a 32-bit integer.
///
/// Bit numbering: 0 = most significant bit, 31 = least significant bit.
pub fn ceesiset(target: u32, bit: u32) -> (u32, FeedbackCode) {
    if bit > 31 {
        return (target, FeedbackCode::error(2601));
    }
    let mask = 1u32 << (31 - bit);
    (target | mask, FeedbackCode::success())
}

/// CEESISHF — Shift bits left or right by a specified count.
///
/// A positive `count` shifts left; a negative `count` shifts right.
/// Vacated bits are filled with zeros.
pub fn ceesishf(target: u32, count: i32) -> (u32, FeedbackCode) {
    if count.unsigned_abs() > 31 {
        return (0, FeedbackCode::success());
    }
    let result = if count > 0 {
        target << count as u32
    } else if count < 0 {
        target >> count.unsigned_abs()
    } else {
        target
    };
    (result, FeedbackCode::success())
}

/// CEESITST — Test a specific bit in a 32-bit integer.
///
/// Returns 1 if the bit is set, 0 if the bit is clear.
/// Bit numbering: 0 = most significant bit, 31 = least significant bit.
pub fn ceesitst(target: u32, bit: u32) -> (u32, FeedbackCode) {
    if bit > 31 {
        return (0, FeedbackCode::error(2601));
    }
    let mask = 1u32 << (31 - bit);
    let result = if target & mask != 0 { 1 } else { 0 };
    (result, FeedbackCode::success())
}

#[cfg(test)]
mod tests {
    use super::*;

    // ─── LE110.1: Bit Manipulation Services ───

    #[test]
    fn test_ceesiset_set_bit_0() {
        let (result, fc) = ceesiset(0, 0);
        assert!(fc.is_success());
        assert_eq!(result, 0x8000_0000); // MSB set
    }

    #[test]
    fn test_ceesiset_set_bit_31() {
        let (result, fc) = ceesiset(0, 31);
        assert!(fc.is_success());
        assert_eq!(result, 1); // LSB set
    }

    #[test]
    fn test_ceesiclr_clear_bit() {
        let (result, fc) = ceesiclr(0xFFFF_FFFF, 0);
        assert!(fc.is_success());
        assert_eq!(result, 0x7FFF_FFFF); // MSB cleared
    }

    #[test]
    fn test_ceesiclr_clear_bit_31() {
        let (result, fc) = ceesiclr(0xFFFF_FFFF, 31);
        assert!(fc.is_success());
        assert_eq!(result, 0xFFFF_FFFE); // LSB cleared
    }

    #[test]
    fn test_ceesitst_bit_set() {
        let (result, fc) = ceesitst(0x8000_0000, 0);
        assert!(fc.is_success());
        assert_eq!(result, 1);
    }

    #[test]
    fn test_ceesitst_bit_clear() {
        let (result, fc) = ceesitst(0x0000_0000, 0);
        assert!(fc.is_success());
        assert_eq!(result, 0);
    }

    #[test]
    fn test_ceesishf_shift_left() {
        let (result, fc) = ceesishf(1, 4);
        assert!(fc.is_success());
        assert_eq!(result, 16);
    }

    #[test]
    fn test_ceesishf_shift_right() {
        let (result, fc) = ceesishf(16, -4);
        assert!(fc.is_success());
        assert_eq!(result, 1);
    }

    #[test]
    fn test_ceesishf_zero_shift() {
        let (result, fc) = ceesishf(0xAB, 0);
        assert!(fc.is_success());
        assert_eq!(result, 0xAB);
    }

    #[test]
    fn test_ceesishf_overflow_returns_zero() {
        let (result, fc) = ceesishf(0xFFFF_FFFF, 32);
        assert!(fc.is_success());
        assert_eq!(result, 0);
    }

    #[test]
    fn test_ceesiset_invalid_bit() {
        let (_, fc) = ceesiset(0, 32);
        assert!(!fc.is_success());
    }

    #[test]
    fn test_ceesiclr_invalid_bit() {
        let (_, fc) = ceesiclr(0, 32);
        assert!(!fc.is_success());
    }

    #[test]
    fn test_ceesitst_invalid_bit() {
        let (_, fc) = ceesitst(0, 32);
        assert!(!fc.is_success());
    }

    #[test]
    fn test_set_clear_roundtrip() {
        let (val, _) = ceesiset(0, 15);
        let (result, _) = ceesiclr(val, 15);
        assert_eq!(result, 0);
    }

    #[test]
    fn test_set_test_roundtrip() {
        let (val, _) = ceesiset(0, 10);
        let (bit_val, _) = ceesitst(val, 10);
        assert_eq!(bit_val, 1);
        let (bit_val2, _) = ceesitst(val, 11);
        assert_eq!(bit_val2, 0);
    }
}
