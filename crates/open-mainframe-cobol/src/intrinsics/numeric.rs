//! Numeric intrinsic functions.
//!
//! Implements COBOL-2014 mathematical and numeric functions.

use std::f64::consts::{E as EULER, PI};

/// FUNCTION E implementation.
///
/// Returns Euler's number (e ≈ 2.71828...).
pub fn e() -> f64 {
    EULER
}

/// FUNCTION PI implementation.
///
/// Returns Pi (π ≈ 3.14159...).
pub fn pi() -> f64 {
    PI
}

/// FUNCTION EXP implementation.
///
/// Returns e raised to the power of x.
pub fn exp(x: f64) -> f64 {
    x.exp()
}

/// FUNCTION EXP10 implementation.
///
/// Returns 10 raised to the power of x.
pub fn exp10(x: f64) -> f64 {
    (10.0_f64).powf(x)
}

/// FUNCTION LOG implementation.
///
/// Returns the natural logarithm (base e) of x.
pub fn log(x: f64) -> f64 {
    x.ln()
}

/// FUNCTION LOG10 implementation.
///
/// Returns the base-10 logarithm of x.
pub fn log10(x: f64) -> f64 {
    x.log10()
}

/// FUNCTION SQRT implementation.
///
/// Returns the square root of x.
pub fn sqrt(x: f64) -> f64 {
    x.sqrt()
}

/// FUNCTION ABS implementation.
///
/// Returns the absolute value of x.
pub fn abs(x: f64) -> f64 {
    x.abs()
}

/// FUNCTION SIGN implementation.
///
/// Returns -1, 0, or 1 based on the sign of x.
pub fn sign(x: f64) -> i32 {
    if x < 0.0 {
        -1
    } else if x > 0.0 {
        1
    } else {
        0
    }
}

/// FUNCTION SIN implementation.
///
/// Returns the sine of x (in radians).
pub fn sin(x: f64) -> f64 {
    x.sin()
}

/// FUNCTION COS implementation.
///
/// Returns the cosine of x (in radians).
pub fn cos(x: f64) -> f64 {
    x.cos()
}

/// FUNCTION TAN implementation.
///
/// Returns the tangent of x (in radians).
pub fn tan(x: f64) -> f64 {
    x.tan()
}

/// FUNCTION ASIN implementation.
///
/// Returns the arc sine of x (in radians).
pub fn asin(x: f64) -> f64 {
    x.asin()
}

/// FUNCTION ACOS implementation.
///
/// Returns the arc cosine of x (in radians).
pub fn acos(x: f64) -> f64 {
    x.acos()
}

/// FUNCTION ATAN implementation.
///
/// Returns the arc tangent of x (in radians).
pub fn atan(x: f64) -> f64 {
    x.atan()
}

/// FUNCTION MIN implementation.
///
/// Returns the minimum value from a list.
pub fn min(values: &[f64]) -> Option<f64> {
    values.iter().cloned().reduce(f64::min)
}

/// FUNCTION MAX implementation.
///
/// Returns the maximum value from a list.
pub fn max(values: &[f64]) -> Option<f64> {
    values.iter().cloned().reduce(f64::max)
}

/// FUNCTION SUM implementation.
///
/// Returns the sum of all values.
pub fn sum(values: &[f64]) -> f64 {
    values.iter().sum()
}

/// FUNCTION MEAN implementation.
///
/// Returns the arithmetic mean (average) of values.
pub fn mean(values: &[f64]) -> Option<f64> {
    if values.is_empty() {
        None
    } else {
        Some(sum(values) / values.len() as f64)
    }
}

/// FUNCTION MEDIAN implementation.
///
/// Returns the median value.
pub fn median(values: &[f64]) -> Option<f64> {
    if values.is_empty() {
        return None;
    }

    let mut sorted: Vec<f64> = values.to_vec();
    sorted.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

    let len = sorted.len();
    if len % 2 == 0 {
        Some((sorted[len / 2 - 1] + sorted[len / 2]) / 2.0)
    } else {
        Some(sorted[len / 2])
    }
}

/// FUNCTION VARIANCE implementation.
///
/// Returns the variance of values.
pub fn variance(values: &[f64]) -> Option<f64> {
    let m = mean(values)?;
    let sq_diff_sum: f64 = values.iter().map(|x| (x - m).powi(2)).sum();
    Some(sq_diff_sum / values.len() as f64)
}

/// FUNCTION STANDARD-DEVIATION implementation.
///
/// Returns the standard deviation of values.
pub fn standard_deviation(values: &[f64]) -> Option<f64> {
    variance(values).map(|v| v.sqrt())
}

/// FUNCTION MOD implementation.
///
/// Returns the modulo (remainder) of a / b with COBOL semantics.
/// Result has the same sign as b.
pub fn mod_func(a: i64, b: i64) -> i64 {
    if b == 0 {
        return 0;
    }
    let r = a % b;
    if (r > 0 && b < 0) || (r < 0 && b > 0) {
        r + b
    } else {
        r
    }
}

/// FUNCTION REM implementation.
///
/// Returns the remainder of a / b.
/// Result has the same sign as a.
pub fn rem(a: f64, b: f64) -> f64 {
    if b == 0.0 {
        return 0.0;
    }
    a % b
}

/// FUNCTION INTEGER implementation.
///
/// Returns the greatest integer not greater than x.
pub fn integer(x: f64) -> i64 {
    x.floor() as i64
}

/// FUNCTION INTEGER-PART implementation.
///
/// Returns the integer part of x (truncation toward zero).
pub fn integer_part(x: f64) -> i64 {
    x.trunc() as i64
}

/// FUNCTION FACTORIAL implementation.
///
/// Returns the factorial of n (n!).
pub fn factorial(n: u32) -> u64 {
    (1..=n as u64).product()
}

/// FUNCTION RANDOM implementation.
///
/// Returns a pseudo-random number between 0 and 1.
/// Note: Uses a simple LCG for reproducibility in tests.
pub fn random(seed: Option<u64>) -> f64 {
    // Linear congruential generator parameters
    const A: u64 = 1103515245;
    const C: u64 = 12345;
    const M: u64 = 1 << 31;

    static mut CURRENT_SEED: u64 = 1;

    unsafe {
        if let Some(s) = seed {
            CURRENT_SEED = s;
        }
        CURRENT_SEED = (A.wrapping_mul(CURRENT_SEED).wrapping_add(C)) % M;
        CURRENT_SEED as f64 / M as f64
    }
}

/// FUNCTION ORD-MIN implementation.
///
/// Returns the ordinal position of the minimum value.
pub fn ord_min(values: &[f64]) -> Option<usize> {
    values
        .iter()
        .enumerate()
        .min_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
        .map(|(i, _)| i + 1) // COBOL uses 1-based indexing
}

/// FUNCTION ORD-MAX implementation.
///
/// Returns the ordinal position of the maximum value.
pub fn ord_max(values: &[f64]) -> Option<usize> {
    values
        .iter()
        .enumerate()
        .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
        .map(|(i, _)| i + 1) // COBOL uses 1-based indexing
}

/// FUNCTION RANGE implementation.
///
/// Returns the difference between max and min values.
pub fn range(values: &[f64]) -> Option<f64> {
    let min_val = min(values)?;
    let max_val = max(values)?;
    Some(max_val - min_val)
}

/// FUNCTION PRESENT-VALUE implementation.
///
/// Calculates present value given discount rate and future amounts.
pub fn present_value(rate: f64, amounts: &[f64]) -> f64 {
    amounts
        .iter()
        .enumerate()
        .map(|(i, amount)| amount / (1.0 + rate).powi((i + 1) as i32))
        .sum()
}

/// FUNCTION MIDRANGE implementation.
///
/// Returns the mean of the minimum and maximum values.
pub fn midrange(values: &[f64]) -> Option<f64> {
    let min_val = min(values)?;
    let max_val = max(values)?;
    Some((min_val + max_val) / 2.0)
}

/// FUNCTION ANNUITY implementation.
///
/// Returns the ratio of an annuity paid for n periods at interest rate r.
pub fn annuity(rate: f64, periods: i32) -> f64 {
    if rate == 0.0 {
        return 1.0 / periods as f64;
    }
    rate / (1.0 - (1.0 + rate).powi(-periods))
}

#[cfg(test)]
mod tests {
    use super::*;

    const EPSILON: f64 = 1e-10;

    fn approx_eq(a: f64, b: f64) -> bool {
        (a - b).abs() < EPSILON
    }

    #[test]
    fn test_constants() {
        assert!(approx_eq(e(), std::f64::consts::E));
        assert!(approx_eq(pi(), std::f64::consts::PI));
    }

    #[test]
    fn test_exp_log() {
        assert!(approx_eq(exp(1.0), EULER));
        assert!(approx_eq(exp(0.0), 1.0));
        assert!(approx_eq(log(EULER), 1.0));
        assert!(approx_eq(log10(100.0), 2.0));
        assert!(approx_eq(exp10(2.0), 100.0));
    }

    #[test]
    fn test_sqrt_abs() {
        assert!(approx_eq(sqrt(4.0), 2.0));
        assert!(approx_eq(sqrt(9.0), 3.0));
        assert!(approx_eq(abs(-5.0), 5.0));
        assert!(approx_eq(abs(5.0), 5.0));
    }

    #[test]
    fn test_sign() {
        assert_eq!(sign(-10.0), -1);
        assert_eq!(sign(0.0), 0);
        assert_eq!(sign(10.0), 1);
    }

    #[test]
    fn test_trig() {
        assert!(approx_eq(sin(0.0), 0.0));
        assert!(approx_eq(cos(0.0), 1.0));
        assert!(approx_eq(sin(PI / 2.0), 1.0));
    }

    #[test]
    fn test_min_max() {
        assert_eq!(min(&[1.0, 2.0, 3.0]), Some(1.0));
        assert_eq!(max(&[1.0, 2.0, 3.0]), Some(3.0));
        assert_eq!(min(&[]), None);
    }

    #[test]
    fn test_sum_mean() {
        assert!(approx_eq(sum(&[1.0, 2.0, 3.0]), 6.0));
        assert_eq!(mean(&[1.0, 2.0, 3.0]), Some(2.0));
        assert_eq!(mean(&[]), None);
    }

    #[test]
    fn test_median() {
        assert_eq!(median(&[1.0, 2.0, 3.0]), Some(2.0));
        assert_eq!(median(&[1.0, 2.0, 3.0, 4.0]), Some(2.5));
        assert_eq!(median(&[3.0, 1.0, 2.0]), Some(2.0)); // Unsorted
    }

    #[test]
    fn test_variance_stddev() {
        let vals = [2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0];
        let v = variance(&vals).unwrap();
        assert!(approx_eq(v, 4.0));
        assert!(approx_eq(standard_deviation(&vals).unwrap(), 2.0));
    }

    #[test]
    fn test_mod_rem() {
        assert_eq!(mod_func(7, 3), 1);
        assert_eq!(mod_func(-7, 3), 2); // COBOL MOD semantics
        assert!(approx_eq(rem(7.0, 3.0), 1.0));
        assert!(approx_eq(rem(-7.0, 3.0), -1.0)); // REM preserves sign of dividend
    }

    #[test]
    fn test_integer_funcs() {
        assert_eq!(integer(3.7), 3);
        assert_eq!(integer(-3.7), -4); // Floor
        assert_eq!(integer_part(3.7), 3);
        assert_eq!(integer_part(-3.7), -3); // Truncate toward zero
    }

    #[test]
    fn test_factorial() {
        assert_eq!(factorial(0), 1);
        assert_eq!(factorial(1), 1);
        assert_eq!(factorial(5), 120);
        assert_eq!(factorial(10), 3628800);
    }

    #[test]
    fn test_ord_min_max() {
        assert_eq!(ord_min(&[3.0, 1.0, 2.0]), Some(2)); // 1-based: position of 1.0
        assert_eq!(ord_max(&[3.0, 1.0, 2.0]), Some(1)); // 1-based: position of 3.0
    }

    #[test]
    fn test_range() {
        assert_eq!(range(&[1.0, 5.0, 3.0]), Some(4.0));
    }

    #[test]
    fn test_midrange() {
        assert_eq!(midrange(&[1.0, 5.0, 3.0]), Some(3.0)); // (1+5)/2
        assert_eq!(midrange(&[10.0, 20.0]), Some(15.0));
        assert_eq!(midrange(&[]), None);
    }

    #[test]
    fn test_financial() {
        // Present value of $100 in 1 year at 10% discount
        let pv = present_value(0.10, &[100.0]);
        assert!((pv - 90.909).abs() < 0.01);

        // Annuity factor
        let ann = annuity(0.10, 10);
        assert!((ann - 0.1627).abs() < 0.001);
    }
}
