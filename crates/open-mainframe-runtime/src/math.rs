//! LE Callable Math Services — trigonometric, logarithmic, power, and random.
//!
//! Implements the Language Environment math callable services following the
//! `CEESx` naming convention where `x` encodes precision:
//! - `S` = single precision (f32 / FLOAT(4))
//! - `D` = double precision (f64 / FLOAT(8))
//! - `Q` = extended precision (f64 / FLOAT(16), mapped to f64)
//!
//! Each service follows the LE calling convention:
//! `CEESxFUNC(input(s), &result, &feedback_code)` —
//! returning a `(result, FeedbackCode)` tuple.

use crate::date_time::FeedbackCode;

// ---------------------------------------------------------------------------
//  Precision helpers
// ---------------------------------------------------------------------------

/// Precision variant for LE math services.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Precision {
    /// Single precision (FLOAT4 / f32).
    Single,
    /// Double precision (FLOAT8 / f64).
    Double,
    /// Extended precision (FLOAT16, emulated as f64).
    Extended,
}

fn round_to_single(v: f64) -> f64 {
    (v as f32) as f64
}

fn result_for(v: f64, prec: Precision) -> f64 {
    match prec {
        Precision::Single => round_to_single(v),
        Precision::Double | Precision::Extended => v,
    }
}

fn domain_error(_func: &str, _msg: &str) -> FeedbackCode {
    // LE math domain errors use message number 2104.
    FeedbackCode::error(2104)
}

// ---------------------------------------------------------------------------
//  Trigonometric functions
// ---------------------------------------------------------------------------

/// CEESxSIN — Sine.
pub fn cees_sin(x: f64, prec: Precision) -> (f64, FeedbackCode) {
    (result_for(x.sin(), prec), FeedbackCode::success())
}

/// CEESxCOS — Cosine.
pub fn cees_cos(x: f64, prec: Precision) -> (f64, FeedbackCode) {
    (result_for(x.cos(), prec), FeedbackCode::success())
}

/// CEESxTAN — Tangent.
pub fn cees_tan(x: f64, prec: Precision) -> (f64, FeedbackCode) {
    (result_for(x.tan(), prec), FeedbackCode::success())
}

/// CEESxCTN — Cotangent.
pub fn cees_ctn(x: f64, prec: Precision) -> (f64, FeedbackCode) {
    let s = x.sin();
    if s == 0.0 {
        return (f64::INFINITY, domain_error("CTN", "cotangent undefined at 0"));
    }
    (result_for(x.cos() / s, prec), FeedbackCode::success())
}

/// CEESxASN — Arcsine.
pub fn cees_asn(x: f64, prec: Precision) -> (f64, FeedbackCode) {
    if !(-1.0..=1.0).contains(&x) {
        return (0.0, domain_error("ASN", "argument must be in [-1, 1]"));
    }
    (result_for(x.asin(), prec), FeedbackCode::success())
}

/// CEESxACS — Arccosine.
pub fn cees_acs(x: f64, prec: Precision) -> (f64, FeedbackCode) {
    if !(-1.0..=1.0).contains(&x) {
        return (0.0, domain_error("ACS", "argument must be in [-1, 1]"));
    }
    (result_for(x.acos(), prec), FeedbackCode::success())
}

/// CEESxATN — Arctangent.
pub fn cees_atn(x: f64, prec: Precision) -> (f64, FeedbackCode) {
    (result_for(x.atan(), prec), FeedbackCode::success())
}

/// CEESxAT2 — Arctangent of y/x (atan2).
pub fn cees_at2(y: f64, x: f64, prec: Precision) -> (f64, FeedbackCode) {
    if x == 0.0 && y == 0.0 {
        return (0.0, domain_error("AT2", "both arguments are zero"));
    }
    (result_for(y.atan2(x), prec), FeedbackCode::success())
}

// ---------------------------------------------------------------------------
//  Hyperbolic functions
// ---------------------------------------------------------------------------

/// CEESxSNH — Hyperbolic sine.
pub fn cees_snh(x: f64, prec: Precision) -> (f64, FeedbackCode) {
    (result_for(x.sinh(), prec), FeedbackCode::success())
}

/// CEESxCSH — Hyperbolic cosine.
pub fn cees_csh(x: f64, prec: Precision) -> (f64, FeedbackCode) {
    (result_for(x.cosh(), prec), FeedbackCode::success())
}

/// CEESxTNH — Hyperbolic tangent.
pub fn cees_tnh(x: f64, prec: Precision) -> (f64, FeedbackCode) {
    (result_for(x.tanh(), prec), FeedbackCode::success())
}

/// CEESxATH — Hyperbolic arctangent.
pub fn cees_ath(x: f64, prec: Precision) -> (f64, FeedbackCode) {
    if !(-1.0..=1.0).contains(&x) {
        return (0.0, domain_error("ATH", "argument must be in (-1, 1)"));
    }
    (result_for(x.atanh(), prec), FeedbackCode::success())
}

// ---------------------------------------------------------------------------
//  Logarithmic / Exponential
// ---------------------------------------------------------------------------

/// CEESxLOG — Natural logarithm.
pub fn cees_log(x: f64, prec: Precision) -> (f64, FeedbackCode) {
    if x <= 0.0 {
        return (f64::NEG_INFINITY, domain_error("LOG", "argument must be positive"));
    }
    (result_for(x.ln(), prec), FeedbackCode::success())
}

/// CEESxLG1 — Log base 10.
pub fn cees_lg1(x: f64, prec: Precision) -> (f64, FeedbackCode) {
    if x <= 0.0 {
        return (f64::NEG_INFINITY, domain_error("LG1", "argument must be positive"));
    }
    (result_for(x.log10(), prec), FeedbackCode::success())
}

/// CEESxEXP — Exponential (e^x).
pub fn cees_exp(x: f64, prec: Precision) -> (f64, FeedbackCode) {
    let r = x.exp();
    if r.is_infinite() {
        return (r, domain_error("EXP", "result overflow"));
    }
    (result_for(r, prec), FeedbackCode::success())
}

/// CEESxSQT — Square root.
pub fn cees_sqt(x: f64, prec: Precision) -> (f64, FeedbackCode) {
    if x < 0.0 {
        return (0.0, domain_error("SQT", "argument must be non-negative"));
    }
    (result_for(x.sqrt(), prec), FeedbackCode::success())
}

// ---------------------------------------------------------------------------
//  Arithmetic functions
// ---------------------------------------------------------------------------

/// CEESxABS — Absolute value.
pub fn cees_abs(x: f64, prec: Precision) -> (f64, FeedbackCode) {
    (result_for(x.abs(), prec), FeedbackCode::success())
}

/// CEESxSGN — Signum function (returns -1, 0, or 1).
pub fn cees_sgn(x: f64, _prec: Precision) -> (f64, FeedbackCode) {
    let s = if x > 0.0 { 1.0 } else if x < 0.0 { -1.0 } else { 0.0 };
    (s, FeedbackCode::success())
}

/// CEESxMOD — Modular arithmetic.
pub fn cees_mod(x: f64, y: f64, prec: Precision) -> (f64, FeedbackCode) {
    if y == 0.0 {
        return (0.0, domain_error("MOD", "divisor is zero"));
    }
    (result_for(x % y, prec), FeedbackCode::success())
}

/// CEESxNIN — Nearest integer (round to nearest even).
pub fn cees_nin(x: f64, prec: Precision) -> (f64, FeedbackCode) {
    (result_for(x.round(), prec), FeedbackCode::success())
}

/// CEESxNWN — Nearest whole number (truncation toward zero).
pub fn cees_nwn(x: f64, prec: Precision) -> (f64, FeedbackCode) {
    (result_for(x.trunc(), prec), FeedbackCode::success())
}

// ---------------------------------------------------------------------------
//  Power / Exponentiation
// ---------------------------------------------------------------------------

/// CEESxXPx — Exponentiation (base^exponent).
pub fn cees_xpx(base: f64, exponent: f64, prec: Precision) -> (f64, FeedbackCode) {
    let r = base.powf(exponent);
    if r.is_nan() {
        return (0.0, domain_error("XPX", "result is NaN"));
    }
    if r.is_infinite() {
        return (r, domain_error("XPX", "result overflow"));
    }
    (result_for(r, prec), FeedbackCode::success())
}

// ---------------------------------------------------------------------------
//  Random number generator
// ---------------------------------------------------------------------------

/// State for the CEERAN0 linear congruential generator.
///
/// Uses the IBM LE constants: seed = (seed * 16807) mod (2^31 - 1).
/// The Lehmer/Park-Miller minimal standard generator.
#[derive(Debug, Clone)]
pub struct RandomState {
    seed: u64,
}

impl RandomState {
    /// Create a new generator with the given seed.
    ///
    /// A seed of 0 is replaced with 1 per LE convention.
    pub fn new(seed: u64) -> Self {
        Self {
            seed: if seed == 0 { 1 } else { seed },
        }
    }

    /// CEERAN0 — Generate a uniform random number in \[0.0, 1.0).
    ///
    /// Returns `(random_value, updated_seed, FeedbackCode)`.
    pub fn ceeran0(&mut self) -> (f64, u64, FeedbackCode) {
        const A: u64 = 16807;
        const M: u64 = 2_147_483_647; // 2^31 - 1
        self.seed = (A.wrapping_mul(self.seed)) % M;
        let result = self.seed as f64 / M as f64;
        (result, self.seed, FeedbackCode::success())
    }
}

// ---------------------------------------------------------------------------
//  Convenience dispatchers (string-based function name)
// ---------------------------------------------------------------------------

/// Call a math service by its LE name (e.g. "CEESDSIN").
///
/// Returns `None` if the name is not recognized.
pub fn call_le_math(name: &str, args: &[f64]) -> Option<(f64, FeedbackCode)> {
    // Name format: "CEESxFUNC" — prefix "CEES", precision char at index 4, suffix from index 5.
    if !name.starts_with("CEES") || name.len() < 6 {
        return None;
    }
    let prec = match name.as_bytes()[4] {
        b'S' | b's' => Precision::Single,
        b'D' | b'd' => Precision::Double,
        b'Q' | b'q' => Precision::Extended,
        _ => return None,
    };
    let suffix = &name[5..];

    match suffix {
        "SIN" => Some(cees_sin(*args.first()?, prec)),
        "COS" => Some(cees_cos(*args.first()?, prec)),
        "TAN" => Some(cees_tan(*args.first()?, prec)),
        "CTN" => Some(cees_ctn(*args.first()?, prec)),
        "ASN" => Some(cees_asn(*args.first()?, prec)),
        "ACS" => Some(cees_acs(*args.first()?, prec)),
        "ATN" => Some(cees_atn(*args.first()?, prec)),
        "AT2" => Some(cees_at2(*args.first()?, *args.get(1)?, prec)),
        "SNH" => Some(cees_snh(*args.first()?, prec)),
        "CSH" => Some(cees_csh(*args.first()?, prec)),
        "TNH" => Some(cees_tnh(*args.first()?, prec)),
        "ATH" => Some(cees_ath(*args.first()?, prec)),
        "LOG" => Some(cees_log(*args.first()?, prec)),
        "LG1" => Some(cees_lg1(*args.first()?, prec)),
        "EXP" => Some(cees_exp(*args.first()?, prec)),
        "SQT" => Some(cees_sqt(*args.first()?, prec)),
        "ABS" => Some(cees_abs(*args.first()?, prec)),
        "SGN" => Some(cees_sgn(*args.first()?, prec)),
        "MOD" => Some(cees_mod(*args.first()?, *args.get(1)?, prec)),
        "NIN" => Some(cees_nin(*args.first()?, prec)),
        "NWN" => Some(cees_nwn(*args.first()?, prec)),
        "XPX" => Some(cees_xpx(*args.first()?, *args.get(1)?, prec)),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::f64::consts::{E, FRAC_PI_2, PI};

    // ─── LE104.1: Trigonometric and Logarithmic ───

    #[test]
    fn test_sin_double() {
        let (r, fc) = cees_sin(FRAC_PI_2, Precision::Double);
        assert!(fc.is_success());
        assert!((r - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_sin_single() {
        let (r, fc) = cees_sin(FRAC_PI_2, Precision::Single);
        assert!(fc.is_success());
        assert!((r - 1.0).abs() < 1e-6);
    }

    #[test]
    fn test_cos_zero() {
        let (r, fc) = cees_cos(0.0, Precision::Double);
        assert!(fc.is_success());
        assert!((r - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_cos_pi() {
        let (r, fc) = cees_cos(PI, Precision::Double);
        assert!(fc.is_success());
        assert!((r + 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_tan_zero() {
        let (r, fc) = cees_tan(0.0, Precision::Double);
        assert!(fc.is_success());
        assert!(r.abs() < 1e-10);
    }

    #[test]
    fn test_ctn_at_pi_over_4() {
        let (r, fc) = cees_ctn(PI / 4.0, Precision::Double);
        assert!(fc.is_success());
        assert!((r - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_ctn_at_zero() {
        let (_, fc) = cees_ctn(0.0, Precision::Double);
        assert!(!fc.is_success());
    }

    #[test]
    fn test_asin() {
        let (r, fc) = cees_asn(1.0, Precision::Double);
        assert!(fc.is_success());
        assert!((r - FRAC_PI_2).abs() < 1e-10);
    }

    #[test]
    fn test_asin_domain() {
        let (_, fc) = cees_asn(2.0, Precision::Double);
        assert!(!fc.is_success());
    }

    #[test]
    fn test_acos() {
        let (r, fc) = cees_acs(0.0, Precision::Double);
        assert!(fc.is_success());
        assert!((r - FRAC_PI_2).abs() < 1e-10);
    }

    #[test]
    fn test_atan() {
        let (r, fc) = cees_atn(1.0, Precision::Double);
        assert!(fc.is_success());
        assert!((r - PI / 4.0).abs() < 1e-10);
    }

    #[test]
    fn test_atan2() {
        let (r, fc) = cees_at2(1.0, 1.0, Precision::Double);
        assert!(fc.is_success());
        assert!((r - PI / 4.0).abs() < 1e-10);
    }

    #[test]
    fn test_atan2_both_zero() {
        let (_, fc) = cees_at2(0.0, 0.0, Precision::Double);
        assert!(!fc.is_success());
    }

    #[test]
    fn test_sinh_cosh_tanh() {
        let (s, fc) = cees_snh(0.0, Precision::Double);
        assert!(fc.is_success());
        assert!(s.abs() < 1e-10);

        let (c, fc) = cees_csh(0.0, Precision::Double);
        assert!(fc.is_success());
        assert!((c - 1.0).abs() < 1e-10);

        let (t, fc) = cees_tnh(0.0, Precision::Double);
        assert!(fc.is_success());
        assert!(t.abs() < 1e-10);
    }

    #[test]
    fn test_atanh() {
        let (r, fc) = cees_ath(0.5, Precision::Double);
        assert!(fc.is_success());
        assert!((r - (0.5_f64).atanh()).abs() < 1e-10);
    }

    #[test]
    fn test_atanh_domain() {
        let (_, fc) = cees_ath(2.0, Precision::Double);
        assert!(!fc.is_success());
    }

    #[test]
    fn test_log_e() {
        let (r, fc) = cees_log(E, Precision::Double);
        assert!(fc.is_success());
        assert!((r - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_log_domain() {
        let (_, fc) = cees_log(-1.0, Precision::Double);
        assert!(!fc.is_success());
    }

    #[test]
    fn test_log10() {
        let (r, fc) = cees_lg1(100.0, Precision::Double);
        assert!(fc.is_success());
        assert!((r - 2.0).abs() < 1e-10);
    }

    #[test]
    fn test_exp_zero() {
        let (r, fc) = cees_exp(0.0, Precision::Double);
        assert!(fc.is_success());
        assert!((r - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_exp_overflow() {
        let (_, fc) = cees_exp(1000.0, Precision::Double);
        assert!(!fc.is_success());
    }

    #[test]
    fn test_sqrt() {
        let (r, fc) = cees_sqt(144.0, Precision::Double);
        assert!(fc.is_success());
        assert!((r - 12.0).abs() < 1e-10);
    }

    #[test]
    fn test_sqrt_domain() {
        let (_, fc) = cees_sqt(-1.0, Precision::Double);
        assert!(!fc.is_success());
    }

    // ─── LE104.2: Additional Math and Random ───

    #[test]
    fn test_abs() {
        let (r, fc) = cees_abs(-42.0, Precision::Double);
        assert!(fc.is_success());
        assert!((r - 42.0).abs() < 1e-10);
    }

    #[test]
    fn test_sgn() {
        let (r, fc) = cees_sgn(100.0, Precision::Double);
        assert!(fc.is_success());
        assert!((r - 1.0).abs() < 1e-10);

        let (r, _) = cees_sgn(-5.0, Precision::Double);
        assert!((r + 1.0).abs() < 1e-10);

        let (r, _) = cees_sgn(0.0, Precision::Double);
        assert!(r.abs() < 1e-10);
    }

    #[test]
    fn test_mod() {
        let (r, fc) = cees_mod(10.0, 3.0, Precision::Double);
        assert!(fc.is_success());
        assert!((r - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_mod_div_zero() {
        let (_, fc) = cees_mod(10.0, 0.0, Precision::Double);
        assert!(!fc.is_success());
    }

    #[test]
    fn test_nin() {
        let (r, fc) = cees_nin(3.7, Precision::Double);
        assert!(fc.is_success());
        assert!((r - 4.0).abs() < 1e-10);
    }

    #[test]
    fn test_nwn() {
        let (r, fc) = cees_nwn(3.7, Precision::Double);
        assert!(fc.is_success());
        assert!((r - 3.0).abs() < 1e-10);
    }

    #[test]
    fn test_power() {
        let (r, fc) = cees_xpx(2.0, 10.0, Precision::Double);
        assert!(fc.is_success());
        assert!((r - 1024.0).abs() < 1e-10);
    }

    #[test]
    fn test_power_overflow() {
        let (_, fc) = cees_xpx(10.0, 1000.0, Precision::Double);
        assert!(!fc.is_success());
    }

    #[test]
    fn test_ceeran0_range() {
        let mut rng = RandomState::new(12345);
        for _ in 0..100 {
            let (val, _, fc) = rng.ceeran0();
            assert!(fc.is_success());
            assert!((0.0..1.0).contains(&val), "random value {val} out of [0,1)");
        }
    }

    #[test]
    fn test_ceeran0_deterministic() {
        let mut rng1 = RandomState::new(42);
        let mut rng2 = RandomState::new(42);
        for _ in 0..10 {
            let (v1, _, _) = rng1.ceeran0();
            let (v2, _, _) = rng2.ceeran0();
            assert!((v1 - v2).abs() < 1e-15);
        }
    }

    #[test]
    fn test_ceeran0_seed_zero() {
        let mut rng = RandomState::new(0);
        let (val, _, fc) = rng.ceeran0();
        assert!(fc.is_success());
        assert!(val > 0.0);
    }

    // ─── Dispatcher tests ───

    #[test]
    fn test_call_le_math_double_sin() {
        let r = call_le_math("CEESDSIN", &[FRAC_PI_2]);
        let (val, fc) = r.unwrap();
        assert!(fc.is_success());
        assert!((val - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_call_le_math_single_cos() {
        let r = call_le_math("CEESSCOS", &[0.0]);
        let (val, fc) = r.unwrap();
        assert!(fc.is_success());
        assert!((val - 1.0).abs() < 1e-6);
    }

    #[test]
    fn test_call_le_math_power() {
        let r = call_le_math("CEESDXPX", &[2.0, 10.0]);
        let (val, fc) = r.unwrap();
        assert!(fc.is_success());
        assert!((val - 1024.0).abs() < 1e-10);
    }

    #[test]
    fn test_call_le_math_unknown() {
        assert!(call_le_math("CEESDZZZ", &[1.0]).is_none());
        assert!(call_le_math("NOTASERVICE", &[1.0]).is_none());
    }

    #[test]
    fn test_extended_precision_log() {
        let r = call_le_math("CEESQLOG", &[E]);
        let (val, fc) = r.unwrap();
        assert!(fc.is_success());
        assert!((val - 1.0).abs() < 1e-10);
    }
}
