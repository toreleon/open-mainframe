// SPDX-License-Identifier: Apache-2.0
//! NAT-108 (partial): Built-in Functions for Natural.
//!
//! Implements 25+ built-in functions across categories:
//! - Character: SUBSTR, LENGTH, EDIT, VAL, TRANSLATE, UPPER, LOWER, TRIM
//! - Date/time: EDIT-DATE, ADD-DURATION, SUBTRACT-DURATION
//! - Numeric: ABS, SIGN, FRAC, INT, MOD, RET, SQRT
//! - Conversion: MOVE EDITED, IS(NUMERIC), IS(ALPHA)

use crate::data_model::NaturalValue;

// ---------------------------------------------------------------------------
// Function evaluator
// ---------------------------------------------------------------------------

/// Evaluate a built-in Natural function by name.
pub fn eval_builtin(name: &str, args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    match name.to_uppercase().as_str() {
        // Character functions
        "SUBSTR" => fn_substr(args),
        "LENGTH" => fn_length(args),
        "EDIT" => fn_edit(args),
        "VAL" => fn_val(args),
        "TRANSLATE" => fn_translate(args),
        "UPPER" => fn_upper(args),
        "LOWER" => fn_lower(args),
        "TRIM" => fn_trim(args),

        // Date/time functions
        "EDIT-DATE" | "EDITDATE" => fn_edit_date(args),
        "ADD-DURATION" | "ADDDURATION" => fn_add_duration(args),
        "SUBTRACT-DURATION" | "SUBTRACTDURATION" => fn_subtract_duration(args),

        // Numeric functions
        "ABS" => fn_abs(args),
        "SIGN" => fn_sign(args),
        "FRAC" => fn_frac(args),
        "INT" => fn_int(args),
        "MOD" => fn_mod(args),
        "RET" => fn_ret(args),
        "SQRT" => fn_sqrt(args),

        // Conversion / test functions
        "IS" => fn_is(args),

        // Trigonometric functions
        "SIN" => fn_sin(args),
        "COS" => fn_cos(args),
        "TAN" => fn_tan(args),
        "ATN" | "ATAN" => fn_atn(args),
        "LOG" => fn_log(args),
        "EXP" => fn_exp(args),
        "SGN" => fn_sign(args),

        // Misc
        "MIN" => fn_min(args),
        "MAX" => fn_max(args),
        "SUM" => fn_sum(args),
        "OLD" => fn_old(args),
        "COUNT" => fn_count(args),
        "AVER" => fn_aver(args),

        _ => Err(FunctionError::UnknownFunction(name.to_string())),
    }
}

// ---------------------------------------------------------------------------
// Character functions
// ---------------------------------------------------------------------------

fn fn_substr(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("SUBSTR", args, 2, 3)?;
    let s = args[0].to_display_string();
    let start = args[1].to_i64() as usize;
    let len = if args.len() > 2 {
        args[2].to_i64() as usize
    } else {
        s.len().saturating_sub(start.saturating_sub(1))
    };
    let start_idx = start.saturating_sub(1); // 1-based
    let result: String = s.chars().skip(start_idx).take(len).collect();
    Ok(NaturalValue::Alpha(result))
}

fn fn_length(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("LENGTH", args, 1, 1)?;
    let s = args[0].to_display_string();
    Ok(NaturalValue::Integer(s.len() as i64))
}

fn fn_edit(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("EDIT", args, 2, 2)?;
    let value = &args[0];
    let mask = args[1].to_display_string();
    let result = crate::manipulation::move_edited(value, &mask);
    Ok(NaturalValue::Alpha(result))
}

fn fn_val(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("VAL", args, 1, 1)?;
    let s = args[0].to_display_string();
    match s.trim().parse::<f64>() {
        Ok(v) => Ok(NaturalValue::Float(v)),
        Err(_) => Ok(NaturalValue::Float(0.0)),
    }
}

fn fn_translate(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("TRANSLATE", args, 2, 2)?;
    let s = args[0].to_display_string();
    let mode = args[1].to_display_string();
    let result = match mode.to_uppercase().as_str() {
        "UPPER" => s.to_uppercase(),
        "LOWER" => s.to_lowercase(),
        _ => s,
    };
    Ok(NaturalValue::Alpha(result))
}

fn fn_upper(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("UPPER", args, 1, 1)?;
    Ok(NaturalValue::Alpha(args[0].to_display_string().to_uppercase()))
}

fn fn_lower(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("LOWER", args, 1, 1)?;
    Ok(NaturalValue::Alpha(args[0].to_display_string().to_lowercase()))
}

fn fn_trim(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("TRIM", args, 1, 2)?;
    let s = args[0].to_display_string();
    let result = if args.len() > 1 {
        let mode = args[1].to_display_string();
        match mode.to_uppercase().as_str() {
            "LEADING" => s.trim_start().to_string(),
            "TRAILING" => s.trim_end().to_string(),
            _ => s.trim().to_string(),
        }
    } else {
        s.trim().to_string()
    };
    Ok(NaturalValue::Alpha(result))
}

// ---------------------------------------------------------------------------
// Date/time functions
// ---------------------------------------------------------------------------

fn fn_edit_date(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("EDIT-DATE", args, 1, 2)?;
    let date_val = args[0].to_i64();
    let format = if args.len() > 1 {
        args[1].to_display_string()
    } else {
        "YYYY-MM-DD".to_string()
    };

    let year = date_val / 10000;
    let month = (date_val % 10000) / 100;
    let day = date_val % 100;

    let result = format
        .replace("YYYY", &format!("{year:04}"))
        .replace("MM", &format!("{month:02}"))
        .replace("DD", &format!("{day:02}"));

    Ok(NaturalValue::Alpha(result))
}

fn fn_add_duration(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("ADD-DURATION", args, 2, 2)?;
    let base = args[0].to_i64();
    let duration = args[1].to_i64();
    // Simplified: just add days to the YYYYMMDD integer
    Ok(NaturalValue::Integer(base + duration))
}

fn fn_subtract_duration(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("SUBTRACT-DURATION", args, 2, 2)?;
    let base = args[0].to_i64();
    let duration = args[1].to_i64();
    Ok(NaturalValue::Integer(base - duration))
}

// ---------------------------------------------------------------------------
// Numeric functions
// ---------------------------------------------------------------------------

fn fn_abs(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("ABS", args, 1, 1)?;
    Ok(NaturalValue::Float(args[0].to_f64().abs()))
}

fn fn_sign(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("SIGN", args, 1, 1)?;
    let v = args[0].to_f64();
    let s = if v > 0.0 { 1 } else if v < 0.0 { -1 } else { 0 };
    Ok(NaturalValue::Integer(s))
}

fn fn_frac(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("FRAC", args, 1, 1)?;
    let v = args[0].to_f64();
    Ok(NaturalValue::Float(v.fract()))
}

fn fn_int(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("INT", args, 1, 1)?;
    let v = args[0].to_f64();
    Ok(NaturalValue::Integer(v.trunc() as i64))
}

fn fn_mod(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("MOD", args, 2, 2)?;
    let a = args[0].to_i64();
    let b = args[1].to_i64();
    if b == 0 {
        return Err(FunctionError::DivisionByZero);
    }
    Ok(NaturalValue::Integer(a % b))
}

fn fn_ret(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("RET", args, 0, 1)?;
    // RET('program') returns the return code, simulated as 0
    Ok(NaturalValue::Integer(0))
}

fn fn_sqrt(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("SQRT", args, 1, 1)?;
    let v = args[0].to_f64();
    if v < 0.0 {
        return Err(FunctionError::InvalidArgument("SQRT of negative number".into()));
    }
    Ok(NaturalValue::Float(v.sqrt()))
}

// ---------------------------------------------------------------------------
// Trigonometric functions
// ---------------------------------------------------------------------------

fn fn_sin(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("SIN", args, 1, 1)?;
    Ok(NaturalValue::Float(args[0].to_f64().sin()))
}

fn fn_cos(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("COS", args, 1, 1)?;
    Ok(NaturalValue::Float(args[0].to_f64().cos()))
}

fn fn_tan(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("TAN", args, 1, 1)?;
    Ok(NaturalValue::Float(args[0].to_f64().tan()))
}

fn fn_atn(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("ATN", args, 1, 1)?;
    Ok(NaturalValue::Float(args[0].to_f64().atan()))
}

fn fn_log(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("LOG", args, 1, 1)?;
    let v = args[0].to_f64();
    if v <= 0.0 {
        return Err(FunctionError::InvalidArgument("LOG of non-positive number".into()));
    }
    Ok(NaturalValue::Float(v.ln()))
}

fn fn_exp(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("EXP", args, 1, 1)?;
    Ok(NaturalValue::Float(args[0].to_f64().exp()))
}

// ---------------------------------------------------------------------------
// Conversion / test functions
// ---------------------------------------------------------------------------

fn fn_is(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    check_args("IS", args, 2, 2)?;
    let value = &args[0];
    let test_type = args[1].to_display_string();

    let result = match test_type.to_uppercase().as_str() {
        "NUMERIC" => {
            let s = value.to_display_string();
            s.trim().parse::<f64>().is_ok() || s.trim().is_empty()
        }
        "ALPHA" => {
            let s = value.to_display_string();
            s.chars().all(|c| c.is_alphabetic() || c == ' ')
        }
        _ => false,
    };
    Ok(NaturalValue::Logical(result))
}

// ---------------------------------------------------------------------------
// Aggregate functions (for report processing)
// ---------------------------------------------------------------------------

fn fn_min(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    if args.is_empty() {
        return Err(FunctionError::WrongArgCount { func: "MIN".into(), expected_min: 1, expected_max: 255, got: 0 });
    }
    let mut min_val = args[0].to_f64();
    for arg in &args[1..] {
        let v = arg.to_f64();
        if v < min_val { min_val = v; }
    }
    Ok(NaturalValue::Float(min_val))
}

fn fn_max(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    if args.is_empty() {
        return Err(FunctionError::WrongArgCount { func: "MAX".into(), expected_min: 1, expected_max: 255, got: 0 });
    }
    let mut max_val = args[0].to_f64();
    for arg in &args[1..] {
        let v = arg.to_f64();
        if v > max_val { max_val = v; }
    }
    Ok(NaturalValue::Float(max_val))
}

fn fn_sum(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    let total: f64 = args.iter().map(|a| a.to_f64()).sum();
    Ok(NaturalValue::Float(total))
}

fn fn_old(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    // OLD() returns the previous value of a variable — stub: return current
    check_args("OLD", args, 1, 1)?;
    Ok(args[0].clone())
}

fn fn_count(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    // COUNT returns the number of items
    Ok(NaturalValue::Integer(args.len() as i64))
}

fn fn_aver(args: &[NaturalValue]) -> Result<NaturalValue, FunctionError> {
    if args.is_empty() {
        return Err(FunctionError::WrongArgCount { func: "AVER".into(), expected_min: 1, expected_max: 255, got: 0 });
    }
    let total: f64 = args.iter().map(|a| a.to_f64()).sum();
    Ok(NaturalValue::Float(total / args.len() as f64))
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn check_args(name: &str, args: &[NaturalValue], min: usize, max: usize) -> Result<(), FunctionError> {
    if args.len() < min || args.len() > max {
        return Err(FunctionError::WrongArgCount {
            func: name.to_string(),
            expected_min: min,
            expected_max: max,
            got: args.len(),
        });
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, thiserror::Error)]
pub enum FunctionError {
    #[error("unknown function: {0}")]
    UnknownFunction(String),
    #[error("{func}: expected {expected_min}..{expected_max} args, got {got}")]
    WrongArgCount { func: String, expected_min: usize, expected_max: usize, got: usize },
    #[error("division by zero")]
    DivisionByZero,
    #[error("invalid argument: {0}")]
    InvalidArgument(String),
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_substr() {
        let result = eval_builtin("SUBSTR", &[
            NaturalValue::Alpha("Hello World".into()),
            NaturalValue::Integer(7),
            NaturalValue::Integer(5),
        ]).unwrap();
        assert_eq!(result.to_display_string(), "World");
    }

    #[test]
    fn test_substr_no_length() {
        let result = eval_builtin("SUBSTR", &[
            NaturalValue::Alpha("Hello".into()),
            NaturalValue::Integer(2),
        ]).unwrap();
        assert_eq!(result.to_display_string(), "ello");
    }

    #[test]
    fn test_length() {
        let result = eval_builtin("LENGTH", &[
            NaturalValue::Alpha("Hello".into()),
        ]).unwrap();
        assert_eq!(result.to_i64(), 5);
    }

    #[test]
    fn test_val() {
        let result = eval_builtin("VAL", &[
            NaturalValue::Alpha("123.45".into()),
        ]).unwrap();
        assert_eq!(result.to_f64(), 123.45);
    }

    #[test]
    fn test_val_invalid() {
        let result = eval_builtin("VAL", &[
            NaturalValue::Alpha("abc".into()),
        ]).unwrap();
        assert_eq!(result.to_f64(), 0.0);
    }

    #[test]
    fn test_upper() {
        let result = eval_builtin("UPPER", &[
            NaturalValue::Alpha("hello".into()),
        ]).unwrap();
        assert_eq!(result.to_display_string(), "HELLO");
    }

    #[test]
    fn test_lower() {
        let result = eval_builtin("LOWER", &[
            NaturalValue::Alpha("HELLO".into()),
        ]).unwrap();
        assert_eq!(result.to_display_string(), "hello");
    }

    #[test]
    fn test_trim() {
        let result = eval_builtin("TRIM", &[
            NaturalValue::Alpha("  hello  ".into()),
        ]).unwrap();
        assert_eq!(result.to_display_string(), "hello");
    }

    #[test]
    fn test_trim_leading() {
        let result = eval_builtin("TRIM", &[
            NaturalValue::Alpha("  hello  ".into()),
            NaturalValue::Alpha("LEADING".into()),
        ]).unwrap();
        assert_eq!(result.to_display_string(), "hello  ");
    }

    #[test]
    fn test_trim_trailing() {
        let result = eval_builtin("TRIM", &[
            NaturalValue::Alpha("  hello  ".into()),
            NaturalValue::Alpha("TRAILING".into()),
        ]).unwrap();
        assert_eq!(result.to_display_string(), "  hello");
    }

    #[test]
    fn test_translate_upper() {
        let result = eval_builtin("TRANSLATE", &[
            NaturalValue::Alpha("hello".into()),
            NaturalValue::Alpha("UPPER".into()),
        ]).unwrap();
        assert_eq!(result.to_display_string(), "HELLO");
    }

    #[test]
    fn test_abs() {
        let result = eval_builtin("ABS", &[NaturalValue::Float(-5.5)]).unwrap();
        assert_eq!(result.to_f64(), 5.5);
    }

    #[test]
    fn test_abs_positive() {
        let result = eval_builtin("ABS", &[NaturalValue::Integer(7)]).unwrap();
        assert_eq!(result.to_f64(), 7.0);
    }

    #[test]
    fn test_sign_positive() {
        let result = eval_builtin("SIGN", &[NaturalValue::Integer(5)]).unwrap();
        assert_eq!(result.to_i64(), 1);
    }

    #[test]
    fn test_sign_negative() {
        let result = eval_builtin("SIGN", &[NaturalValue::Float(-3.0)]).unwrap();
        assert_eq!(result.to_i64(), -1);
    }

    #[test]
    fn test_sign_zero() {
        let result = eval_builtin("SIGN", &[NaturalValue::Integer(0)]).unwrap();
        assert_eq!(result.to_i64(), 0);
    }

    #[test]
    fn test_frac() {
        let result = eval_builtin("FRAC", &[NaturalValue::Float(3.14)]).unwrap();
        let frac = result.to_f64();
        assert!((frac - 0.14).abs() < 0.001);
    }

    #[test]
    fn test_int() {
        let result = eval_builtin("INT", &[NaturalValue::Float(3.99)]).unwrap();
        assert_eq!(result.to_i64(), 3);
    }

    #[test]
    fn test_mod() {
        let result = eval_builtin("MOD", &[
            NaturalValue::Integer(10),
            NaturalValue::Integer(3),
        ]).unwrap();
        assert_eq!(result.to_i64(), 1);
    }

    #[test]
    fn test_mod_zero() {
        let result = eval_builtin("MOD", &[
            NaturalValue::Integer(10),
            NaturalValue::Integer(0),
        ]);
        assert!(result.is_err());
    }

    #[test]
    fn test_sqrt() {
        let result = eval_builtin("SQRT", &[NaturalValue::Float(25.0)]).unwrap();
        assert_eq!(result.to_f64(), 5.0);
    }

    #[test]
    fn test_sqrt_negative() {
        let result = eval_builtin("SQRT", &[NaturalValue::Float(-1.0)]);
        assert!(result.is_err());
    }

    #[test]
    fn test_ret() {
        let result = eval_builtin("RET", &[NaturalValue::Alpha("prog".into())]).unwrap();
        assert_eq!(result.to_i64(), 0);
    }

    #[test]
    fn test_is_numeric() {
        let result = eval_builtin("IS", &[
            NaturalValue::Alpha("123".into()),
            NaturalValue::Alpha("NUMERIC".into()),
        ]).unwrap();
        assert_eq!(result, NaturalValue::Logical(true));
    }

    #[test]
    fn test_is_numeric_false() {
        let result = eval_builtin("IS", &[
            NaturalValue::Alpha("abc".into()),
            NaturalValue::Alpha("NUMERIC".into()),
        ]).unwrap();
        assert_eq!(result, NaturalValue::Logical(false));
    }

    #[test]
    fn test_is_alpha() {
        let result = eval_builtin("IS", &[
            NaturalValue::Alpha("Hello World".into()),
            NaturalValue::Alpha("ALPHA".into()),
        ]).unwrap();
        assert_eq!(result, NaturalValue::Logical(true));
    }

    #[test]
    fn test_is_alpha_false() {
        let result = eval_builtin("IS", &[
            NaturalValue::Alpha("Hello123".into()),
            NaturalValue::Alpha("ALPHA".into()),
        ]).unwrap();
        assert_eq!(result, NaturalValue::Logical(false));
    }

    #[test]
    fn test_min() {
        let result = eval_builtin("MIN", &[
            NaturalValue::Integer(5),
            NaturalValue::Integer(3),
            NaturalValue::Integer(7),
        ]).unwrap();
        assert_eq!(result.to_f64(), 3.0);
    }

    #[test]
    fn test_max() {
        let result = eval_builtin("MAX", &[
            NaturalValue::Integer(5),
            NaturalValue::Integer(3),
            NaturalValue::Integer(7),
        ]).unwrap();
        assert_eq!(result.to_f64(), 7.0);
    }

    #[test]
    fn test_sum() {
        let result = eval_builtin("SUM", &[
            NaturalValue::Integer(1),
            NaturalValue::Integer(2),
            NaturalValue::Integer(3),
        ]).unwrap();
        assert_eq!(result.to_f64(), 6.0);
    }

    #[test]
    fn test_aver() {
        let result = eval_builtin("AVER", &[
            NaturalValue::Integer(10),
            NaturalValue::Integer(20),
            NaturalValue::Integer(30),
        ]).unwrap();
        assert_eq!(result.to_f64(), 20.0);
    }

    #[test]
    fn test_count() {
        let result = eval_builtin("COUNT", &[
            NaturalValue::Integer(1),
            NaturalValue::Integer(2),
        ]).unwrap();
        assert_eq!(result.to_i64(), 2);
    }

    #[test]
    fn test_edit_date() {
        let result = eval_builtin("EDIT-DATE", &[
            NaturalValue::Integer(20260115),
            NaturalValue::Alpha("YYYY-MM-DD".into()),
        ]).unwrap();
        assert_eq!(result.to_display_string(), "2026-01-15");
    }

    #[test]
    fn test_add_duration() {
        let result = eval_builtin("ADD-DURATION", &[
            NaturalValue::Integer(20260101),
            NaturalValue::Integer(5),
        ]).unwrap();
        assert_eq!(result.to_i64(), 20260106);
    }

    #[test]
    fn test_subtract_duration() {
        let result = eval_builtin("SUBTRACT-DURATION", &[
            NaturalValue::Integer(20260110),
            NaturalValue::Integer(5),
        ]).unwrap();
        assert_eq!(result.to_i64(), 20260105);
    }

    #[test]
    fn test_unknown_function() {
        let result = eval_builtin("NONEXISTENT", &[]);
        assert!(result.is_err());
    }

    #[test]
    fn test_wrong_arg_count() {
        let result = eval_builtin("ABS", &[]);
        assert!(result.is_err());
    }

    #[test]
    fn test_edit() {
        let result = eval_builtin("EDIT", &[
            NaturalValue::Integer(12345),
            NaturalValue::Alpha("99,999".into()),
        ]).unwrap();
        assert_eq!(result.to_display_string(), "12,345");
    }

    // --- Trigonometric / math functions ---

    #[test]
    fn test_sin() {
        let result = eval_builtin("SIN", &[NaturalValue::Float(0.0)]).unwrap();
        assert!((result.to_f64() - 0.0).abs() < 1e-10);
    }

    #[test]
    fn test_cos() {
        let result = eval_builtin("COS", &[NaturalValue::Float(0.0)]).unwrap();
        assert!((result.to_f64() - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_tan() {
        let result = eval_builtin("TAN", &[NaturalValue::Float(0.0)]).unwrap();
        assert!((result.to_f64() - 0.0).abs() < 1e-10);
    }

    #[test]
    fn test_atn() {
        let result = eval_builtin("ATN", &[NaturalValue::Float(1.0)]).unwrap();
        assert!((result.to_f64() - std::f64::consts::FRAC_PI_4).abs() < 1e-10);
    }

    #[test]
    fn test_log() {
        let result = eval_builtin("LOG", &[NaturalValue::Float(std::f64::consts::E)]).unwrap();
        assert!((result.to_f64() - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_log_negative() {
        let result = eval_builtin("LOG", &[NaturalValue::Float(-1.0)]);
        assert!(result.is_err());
    }

    #[test]
    fn test_exp() {
        let result = eval_builtin("EXP", &[NaturalValue::Float(0.0)]).unwrap();
        assert!((result.to_f64() - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_sgn() {
        let result = eval_builtin("SGN", &[NaturalValue::Integer(-42)]).unwrap();
        assert_eq!(result.to_i64(), -1);
    }
}
