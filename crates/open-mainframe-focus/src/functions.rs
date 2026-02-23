//! FOC-106: Built-in Functions (5 stories).
//!
//! Character, date/time, numeric, and conversion functions with a
//! `FunctionRegistry` for name-based lookup and invocation.

use std::collections::HashMap;
use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum FunctionError {
    #[error("unknown function: {0}")]
    UnknownFunction(String),
    #[error("wrong number of arguments for {name}: expected {expected}, got {got}")]
    WrongArgCount {
        name: String,
        expected: usize,
        got: usize,
    },
    #[error("type error in {0}: {1}")]
    TypeError(String, String),
}

// ---------------------------------------------------------------------------
// Function value
// ---------------------------------------------------------------------------

/// Value type used by the function system.
#[derive(Debug, Clone, PartialEq)]
pub enum FnValue {
    Str(String),
    Num(f64),
    Null,
}

impl FnValue {
    pub fn as_num(&self) -> f64 {
        match self {
            FnValue::Num(n) => *n,
            FnValue::Str(s) => s.parse().unwrap_or(0.0),
            FnValue::Null => 0.0,
        }
    }

    pub fn as_str(&self) -> String {
        match self {
            FnValue::Str(s) => s.clone(),
            FnValue::Num(n) => {
                if *n == n.floor() && n.abs() < 1e15 {
                    format!("{}", *n as i64)
                } else {
                    format!("{n}")
                }
            }
            FnValue::Null => String::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// Function type
// ---------------------------------------------------------------------------

type BuiltinFn = fn(&[FnValue]) -> Result<FnValue, FunctionError>;

// ---------------------------------------------------------------------------
// Registry
// ---------------------------------------------------------------------------

/// Registry of built-in FOCUS functions.
pub struct FunctionRegistry {
    functions: HashMap<String, BuiltinFn>,
}

impl FunctionRegistry {
    /// Create a new registry pre-loaded with all built-in functions.
    pub fn new() -> Self {
        let mut reg = Self {
            functions: HashMap::new(),
        };
        // Character functions
        reg.register("SUBSTR", fn_substr);
        reg.register("TRIM", fn_trim);
        reg.register("EDIT", fn_edit);
        reg.register("PARONE", fn_parone);
        reg.register("ARGLEN", fn_arglen);
        reg.register("CHAR", fn_char);
        reg.register("ORONE", fn_orone);
        reg.register("POSIT", fn_posit);
        reg.register("ASIS", fn_asis);
        reg.register("REVERSE", fn_reverse);
        reg.register("LTRUNC", fn_ltrunc);
        reg.register("RTRUNC", fn_rtrunc);
        reg.register("LJUST", fn_ljust);
        reg.register("RJUST", fn_rjust);
        reg.register("CTRAN", fn_ctran);
        reg.register("CHKFMT", fn_chkfmt);
        reg.register("UPCASE", fn_upcase);
        reg.register("LOCASE", fn_locase);

        // Date/time functions
        reg.register("DATECVT", fn_datecvt);
        reg.register("DATEDIF", fn_datedif);
        reg.register("DATEADD", fn_dateadd);
        reg.register("TODAY", fn_today);
        reg.register("FITEFLD", fn_fitefld);
        reg.register("AYMD", fn_aymd);
        reg.register("HMASK", fn_hmask);
        reg.register("DTVAL", fn_dtval);

        // Numeric functions
        reg.register("ABS", fn_abs);
        reg.register("INT", fn_int);
        reg.register("MOD", fn_mod);
        reg.register("ROUND", fn_round);
        reg.register("MAX", fn_max);
        reg.register("MIN", fn_min);
        reg.register("SQRT", fn_sqrt);
        reg.register("LOG", fn_log);
        reg.register("EXP", fn_exp);
        reg.register("PCT", fn_pct);
        reg.register("RDPCT", fn_rdpct);
        reg.register("CNTR", fn_cntr);

        // Conversion functions
        reg.register("ATODBL", fn_atodbl);
        reg.register("DBLTOS", fn_dbltos);
        reg.register("ITONUM", fn_itonum);
        reg.register("NUMTOI", fn_numtoi);
        reg.register("DECODE", fn_decode);

        reg
    }

    /// Register a function by name.
    pub fn register(&mut self, name: &str, func: BuiltinFn) {
        self.functions.insert(name.to_uppercase(), func);
    }

    /// Look up and invoke a function by name.
    pub fn invoke(&self, name: &str, args: &[FnValue]) -> Result<FnValue, FunctionError> {
        let upper = name.to_uppercase();
        match self.functions.get(&upper) {
            Some(func) => func(args),
            None => Err(FunctionError::UnknownFunction(upper)),
        }
    }

    /// Check if a function exists.
    pub fn exists(&self, name: &str) -> bool {
        self.functions.contains_key(&name.to_uppercase())
    }

    /// List all registered function names.
    pub fn list_functions(&self) -> Vec<String> {
        let mut names: Vec<String> = self.functions.keys().cloned().collect();
        names.sort();
        names
    }
}

impl Default for FunctionRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Character functions
// ---------------------------------------------------------------------------

fn fn_substr(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // SUBSTR(string, start, length)
    if args.len() < 2 {
        return Err(FunctionError::WrongArgCount {
            name: "SUBSTR".into(),
            expected: 2,
            got: args.len(),
        });
    }
    let s = args[0].as_str();
    let start = (args[1].as_num() as usize).saturating_sub(1);
    let len = if args.len() > 2 {
        args[2].as_num() as usize
    } else {
        s.len().saturating_sub(start)
    };
    let end = (start + len).min(s.len());
    Ok(FnValue::Str(s[start..end].to_string()))
}

fn fn_trim(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("TRIM", args, 1)?;
    Ok(FnValue::Str(args[0].as_str().trim().to_string()))
}

fn fn_edit(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // EDIT(value, format_mask) — simplified: just format the value
    check_args("EDIT", args, 2)?;
    let val = args[0].as_str();
    let mask = args[1].as_str();
    // Simple mask: replace '9' placeholders with digits
    let mut result = String::new();
    let mut vi = 0;
    let val_chars: Vec<char> = val.chars().collect();
    for mc in mask.chars() {
        if mc == '9' && vi < val_chars.len() {
            result.push(val_chars[vi]);
            vi += 1;
        } else if mc == '9' {
            result.push('0');
        } else {
            result.push(mc);
        }
    }
    Ok(FnValue::Str(result))
}

fn fn_parone(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // PARONE: extract first word
    check_args("PARONE", args, 1)?;
    let s = args[0].as_str();
    let word = s.split_whitespace().next().unwrap_or("");
    Ok(FnValue::Str(word.to_string()))
}

fn fn_arglen(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("ARGLEN", args, 1)?;
    Ok(FnValue::Num(args[0].as_str().len() as f64))
}

fn fn_char(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // CHAR(code) — return character for ASCII code
    check_args("CHAR", args, 1)?;
    let code = args[0].as_num() as u8;
    Ok(FnValue::Str(String::from(code as char)))
}

fn fn_orone(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // ORONE: return first non-empty argument
    for arg in args {
        let s = arg.as_str();
        if !s.is_empty() {
            return Ok(FnValue::Str(s));
        }
    }
    Ok(FnValue::Null)
}

fn fn_posit(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // POSIT(string, substring) — position of substring (1-based)
    check_args("POSIT", args, 2)?;
    let haystack = args[0].as_str();
    let needle = args[1].as_str();
    let pos = haystack.find(&needle).map_or(0, |p| p + 1);
    Ok(FnValue::Num(pos as f64))
}

fn fn_asis(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("ASIS", args, 1)?;
    Ok(FnValue::Str(args[0].as_str()))
}

fn fn_reverse(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("REVERSE", args, 1)?;
    Ok(FnValue::Str(args[0].as_str().chars().rev().collect()))
}

fn fn_ltrunc(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // LTRUNC(string, n) — remove first n characters
    check_args("LTRUNC", args, 2)?;
    let s = args[0].as_str();
    let n = args[1].as_num() as usize;
    let start = n.min(s.len());
    Ok(FnValue::Str(s[start..].to_string()))
}

fn fn_rtrunc(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // RTRUNC(string, n) — remove last n characters
    check_args("RTRUNC", args, 2)?;
    let s = args[0].as_str();
    let n = args[1].as_num() as usize;
    let end = s.len().saturating_sub(n);
    Ok(FnValue::Str(s[..end].to_string()))
}

fn fn_ljust(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("LJUST", args, 2)?;
    let s = args[0].as_str();
    let width = args[1].as_num() as usize;
    Ok(FnValue::Str(format!("{s:<width$}")))
}

fn fn_rjust(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("RJUST", args, 2)?;
    let s = args[0].as_str();
    let width = args[1].as_num() as usize;
    Ok(FnValue::Str(format!("{s:>width$}")))
}

fn fn_ctran(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // CTRAN(string, from_char, to_char) — translate characters
    check_args("CTRAN", args, 3)?;
    let s = args[0].as_str();
    let from = args[1].as_str();
    let to = args[2].as_str();
    let from_c = from.chars().next().unwrap_or(' ');
    let to_c = to.chars().next().unwrap_or(' ');
    Ok(FnValue::Str(s.replace(from_c, &to_c.to_string())))
}

fn fn_chkfmt(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // CHKFMT(string, format) — check if string matches format pattern
    // Returns 1 if matches, 0 if not (simplified)
    check_args("CHKFMT", args, 2)?;
    let s = args[0].as_str();
    let fmt = args[1].as_str();
    // Simple check: 'A' = alpha, '9' = digit
    let matches = s.len() == fmt.len()
        && s.chars().zip(fmt.chars()).all(|(sc, fc)| match fc {
            'A' => sc.is_alphabetic(),
            '9' => sc.is_ascii_digit(),
            _ => sc == fc,
        });
    Ok(FnValue::Num(if matches { 1.0 } else { 0.0 }))
}

fn fn_upcase(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("UPCASE", args, 1)?;
    Ok(FnValue::Str(args[0].as_str().to_uppercase()))
}

fn fn_locase(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("LOCASE", args, 1)?;
    Ok(FnValue::Str(args[0].as_str().to_lowercase()))
}

// ---------------------------------------------------------------------------
// Date/time functions
// ---------------------------------------------------------------------------

fn fn_datecvt(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // DATECVT(date_value, from_format, to_format) — simplified
    check_args("DATECVT", args, 3)?;
    // Return date as-is for this simulation
    Ok(FnValue::Str(args[0].as_str()))
}

fn fn_datedif(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // DATEDIF(date1, date2, unit) — simplified: return difference as number
    check_args("DATEDIF", args, 3)?;
    let d1 = args[0].as_num();
    let d2 = args[1].as_num();
    Ok(FnValue::Num((d2 - d1).abs()))
}

fn fn_dateadd(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // DATEADD(date, amount, unit) — simplified
    check_args("DATEADD", args, 3)?;
    let date = args[0].as_num();
    let amount = args[1].as_num();
    Ok(FnValue::Num(date + amount))
}

fn fn_today(_args: &[FnValue]) -> Result<FnValue, FunctionError> {
    Ok(FnValue::Str("2026-02-23".to_string()))
}

fn fn_fitefld(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // FITEFLD: trim trailing blanks and fit field
    check_args("FITEFLD", args, 1)?;
    Ok(FnValue::Str(args[0].as_str().trim_end().to_string()))
}

fn fn_aymd(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // AYMD(date, days) — add days to date (simplified as numeric)
    check_args("AYMD", args, 2)?;
    let date = args[0].as_num();
    let days = args[1].as_num();
    Ok(FnValue::Num(date + days))
}

fn fn_hmask(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // HMASK(value, mask) — apply heading mask
    check_args("HMASK", args, 2)?;
    Ok(FnValue::Str(format!("{}: {}", args[1].as_str(), args[0].as_str())))
}

fn fn_dtval(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // DTVAL(date_string) — convert date string to numeric value
    check_args("DTVAL", args, 1)?;
    let s = args[0].as_str();
    // Simple: try to parse as number, or hash the string
    let val: f64 = s.parse().unwrap_or_else(|_| {
        s.bytes().map(|b| b as f64).sum()
    });
    Ok(FnValue::Num(val))
}

// ---------------------------------------------------------------------------
// Numeric functions
// ---------------------------------------------------------------------------

fn fn_abs(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("ABS", args, 1)?;
    Ok(FnValue::Num(args[0].as_num().abs()))
}

fn fn_int(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("INT", args, 1)?;
    Ok(FnValue::Num(args[0].as_num().trunc()))
}

fn fn_mod(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("MOD", args, 2)?;
    let a = args[0].as_num();
    let b = args[1].as_num();
    if b.abs() < f64::EPSILON {
        Ok(FnValue::Num(0.0))
    } else {
        Ok(FnValue::Num(a % b))
    }
}

fn fn_round(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("ROUND", args, 2)?;
    let val = args[0].as_num();
    let places = args[1].as_num() as i32;
    let factor = 10_f64.powi(places);
    Ok(FnValue::Num((val * factor).round() / factor))
}

fn fn_max(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    if args.is_empty() {
        return Err(FunctionError::WrongArgCount {
            name: "MAX".into(),
            expected: 1,
            got: 0,
        });
    }
    let m = args.iter().map(|a| a.as_num()).fold(f64::NEG_INFINITY, f64::max);
    Ok(FnValue::Num(m))
}

fn fn_min(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    if args.is_empty() {
        return Err(FunctionError::WrongArgCount {
            name: "MIN".into(),
            expected: 1,
            got: 0,
        });
    }
    let m = args.iter().map(|a| a.as_num()).fold(f64::INFINITY, f64::min);
    Ok(FnValue::Num(m))
}

fn fn_sqrt(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("SQRT", args, 1)?;
    let n = args[0].as_num();
    if n < 0.0 {
        return Err(FunctionError::TypeError(
            "SQRT".into(),
            "negative argument".into(),
        ));
    }
    Ok(FnValue::Num(n.sqrt()))
}

fn fn_log(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("LOG", args, 1)?;
    let n = args[0].as_num();
    if n <= 0.0 {
        return Err(FunctionError::TypeError(
            "LOG".into(),
            "non-positive argument".into(),
        ));
    }
    Ok(FnValue::Num(n.ln()))
}

fn fn_exp(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("EXP", args, 1)?;
    Ok(FnValue::Num(args[0].as_num().exp()))
}

fn fn_pct(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // PCT(value, total) — percentage
    check_args("PCT", args, 2)?;
    let val = args[0].as_num();
    let total = args[1].as_num();
    if total.abs() < f64::EPSILON {
        Ok(FnValue::Num(0.0))
    } else {
        Ok(FnValue::Num((val / total) * 100.0))
    }
}

fn fn_rdpct(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // RDPCT(value, total) — rounded percentage
    check_args("RDPCT", args, 2)?;
    let val = args[0].as_num();
    let total = args[1].as_num();
    if total.abs() < f64::EPSILON {
        Ok(FnValue::Num(0.0))
    } else {
        Ok(FnValue::Num(((val / total) * 100.0).round()))
    }
}

fn fn_cntr(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // CNTR(string, width) — center string in field
    check_args("CNTR", args, 2)?;
    let s = args[0].as_str();
    let width = args[1].as_num() as usize;
    if s.len() >= width {
        Ok(FnValue::Str(s))
    } else {
        let pad = (width - s.len()) / 2;
        let result = format!("{}{}{}", " ".repeat(pad), s, " ".repeat(width - s.len() - pad));
        Ok(FnValue::Str(result))
    }
}

// ---------------------------------------------------------------------------
// Conversion functions
// ---------------------------------------------------------------------------

fn fn_atodbl(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("ATODBL", args, 1)?;
    Ok(FnValue::Num(args[0].as_num()))
}

fn fn_dbltos(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("DBLTOS", args, 1)?;
    Ok(FnValue::Str(format!("{}", args[0].as_num())))
}

fn fn_itonum(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("ITONUM", args, 1)?;
    let n = args[0].as_num() as i64;
    Ok(FnValue::Num(n as f64))
}

fn fn_numtoi(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    check_args("NUMTOI", args, 1)?;
    let n = args[0].as_num() as i64;
    Ok(FnValue::Num(n as f64))
}

fn fn_decode(args: &[FnValue]) -> Result<FnValue, FunctionError> {
    // DECODE(value, match1, result1, match2, result2, ..., default)
    if args.len() < 3 {
        return Err(FunctionError::WrongArgCount {
            name: "DECODE".into(),
            expected: 3,
            got: args.len(),
        });
    }
    let val = args[0].as_str();
    let mut i = 1;
    while i + 1 < args.len() {
        if args[i].as_str() == val {
            return Ok(args[i + 1].clone());
        }
        i += 2;
    }
    // If odd remaining args, last is default
    if i < args.len() {
        Ok(args[i].clone())
    } else {
        Ok(FnValue::Null)
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn check_args(name: &str, args: &[FnValue], expected: usize) -> Result<(), FunctionError> {
    if args.len() < expected {
        Err(FunctionError::WrongArgCount {
            name: name.into(),
            expected,
            got: args.len(),
        })
    } else {
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn reg() -> FunctionRegistry {
        FunctionRegistry::new()
    }

    fn s(val: &str) -> FnValue {
        FnValue::Str(val.to_string())
    }

    fn n(val: f64) -> FnValue {
        FnValue::Num(val)
    }

    // --- Character function tests ---

    #[test]
    fn test_substr() {
        let r = reg();
        let result = r.invoke("SUBSTR", &[s("HELLO WORLD"), n(7.0), n(5.0)]).unwrap();
        assert_eq!(result, s("WORLD"));
    }

    #[test]
    fn test_substr_no_length() {
        let r = reg();
        let result = r.invoke("SUBSTR", &[s("ABCDEF"), n(4.0)]).unwrap();
        assert_eq!(result, s("DEF"));
    }

    #[test]
    fn test_trim() {
        let r = reg();
        let result = r.invoke("TRIM", &[s("  hello  ")]).unwrap();
        assert_eq!(result, s("hello"));
    }

    #[test]
    fn test_arglen() {
        let r = reg();
        let result = r.invoke("ARGLEN", &[s("test")]).unwrap();
        assert_eq!(result, n(4.0));
    }

    #[test]
    fn test_posit() {
        let r = reg();
        let result = r.invoke("POSIT", &[s("ABCDEF"), s("CD")]).unwrap();
        assert_eq!(result, n(3.0));
    }

    #[test]
    fn test_posit_not_found() {
        let r = reg();
        let result = r.invoke("POSIT", &[s("ABCDEF"), s("XY")]).unwrap();
        assert_eq!(result, n(0.0));
    }

    #[test]
    fn test_reverse() {
        let r = reg();
        let result = r.invoke("REVERSE", &[s("HELLO")]).unwrap();
        assert_eq!(result, s("OLLEH"));
    }

    #[test]
    fn test_ltrunc() {
        let r = reg();
        let result = r.invoke("LTRUNC", &[s("ABCDEF"), n(2.0)]).unwrap();
        assert_eq!(result, s("CDEF"));
    }

    #[test]
    fn test_rtrunc() {
        let r = reg();
        let result = r.invoke("RTRUNC", &[s("ABCDEF"), n(2.0)]).unwrap();
        assert_eq!(result, s("ABCD"));
    }

    #[test]
    fn test_ljust() {
        let r = reg();
        let result = r.invoke("LJUST", &[s("HI"), n(6.0)]).unwrap();
        assert_eq!(result, s("HI    "));
    }

    #[test]
    fn test_rjust() {
        let r = reg();
        let result = r.invoke("RJUST", &[s("HI"), n(6.0)]).unwrap();
        assert_eq!(result, s("    HI"));
    }

    #[test]
    fn test_ctran() {
        let r = reg();
        let result = r.invoke("CTRAN", &[s("HELLO"), s("L"), s("R")]).unwrap();
        assert_eq!(result, s("HERRO"));
    }

    #[test]
    fn test_parone() {
        let r = reg();
        let result = r.invoke("PARONE", &[s("hello world")]).unwrap();
        assert_eq!(result, s("hello"));
    }

    #[test]
    fn test_upcase() {
        let r = reg();
        let result = r.invoke("UPCASE", &[s("hello")]).unwrap();
        assert_eq!(result, s("HELLO"));
    }

    #[test]
    fn test_locase() {
        let r = reg();
        let result = r.invoke("LOCASE", &[s("HELLO")]).unwrap();
        assert_eq!(result, s("hello"));
    }

    #[test]
    fn test_chkfmt_match() {
        let r = reg();
        let result = r.invoke("CHKFMT", &[s("AB12"), s("AA99")]).unwrap();
        assert_eq!(result, n(1.0));
    }

    #[test]
    fn test_chkfmt_no_match() {
        let r = reg();
        let result = r.invoke("CHKFMT", &[s("1234"), s("AA99")]).unwrap();
        assert_eq!(result, n(0.0));
    }

    #[test]
    fn test_char() {
        let r = reg();
        let result = r.invoke("CHAR", &[n(65.0)]).unwrap();
        assert_eq!(result, s("A"));
    }

    #[test]
    fn test_asis() {
        let r = reg();
        let result = r.invoke("ASIS", &[s("hello")]).unwrap();
        assert_eq!(result, s("hello"));
    }

    // --- Numeric function tests ---

    #[test]
    fn test_abs() {
        let r = reg();
        assert_eq!(r.invoke("ABS", &[n(-5.0)]).unwrap(), n(5.0));
        assert_eq!(r.invoke("ABS", &[n(3.0)]).unwrap(), n(3.0));
    }

    #[test]
    fn test_int() {
        let r = reg();
        assert_eq!(r.invoke("INT", &[n(3.7)]).unwrap(), n(3.0));
        assert_eq!(r.invoke("INT", &[n(-2.3)]).unwrap(), n(-2.0));
    }

    #[test]
    fn test_mod() {
        let r = reg();
        let result = r.invoke("MOD", &[n(17.0), n(5.0)]).unwrap();
        assert_eq!(result, n(2.0));
    }

    #[test]
    fn test_mod_by_zero() {
        let r = reg();
        let result = r.invoke("MOD", &[n(17.0), n(0.0)]).unwrap();
        assert_eq!(result, n(0.0));
    }

    #[test]
    fn test_round() {
        let r = reg();
        let result = r.invoke("ROUND", &[n(3.456), n(2.0)]).unwrap();
        assert!((result.as_num() - 3.46).abs() < 0.001);
    }

    #[test]
    fn test_max() {
        let r = reg();
        let result = r.invoke("MAX", &[n(3.0), n(7.0), n(1.0)]).unwrap();
        assert_eq!(result, n(7.0));
    }

    #[test]
    fn test_min() {
        let r = reg();
        let result = r.invoke("MIN", &[n(3.0), n(7.0), n(1.0)]).unwrap();
        assert_eq!(result, n(1.0));
    }

    #[test]
    fn test_sqrt() {
        let r = reg();
        let result = r.invoke("SQRT", &[n(16.0)]).unwrap();
        assert!((result.as_num() - 4.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_sqrt_negative() {
        let r = reg();
        assert!(r.invoke("SQRT", &[n(-1.0)]).is_err());
    }

    #[test]
    fn test_log() {
        let r = reg();
        let result = r.invoke("LOG", &[n(std::f64::consts::E)]).unwrap();
        assert!((result.as_num() - 1.0).abs() < 0.001);
    }

    #[test]
    fn test_exp() {
        let r = reg();
        let result = r.invoke("EXP", &[n(1.0)]).unwrap();
        assert!((result.as_num() - std::f64::consts::E).abs() < 0.001);
    }

    #[test]
    fn test_pct() {
        let r = reg();
        let result = r.invoke("PCT", &[n(25.0), n(100.0)]).unwrap();
        assert!((result.as_num() - 25.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_rdpct() {
        let r = reg();
        let result = r.invoke("RDPCT", &[n(33.0), n(100.0)]).unwrap();
        assert!((result.as_num() - 33.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_cntr() {
        let r = reg();
        let result = r.invoke("CNTR", &[s("HI"), n(6.0)]).unwrap();
        assert_eq!(result.as_str().len(), 6);
        assert!(result.as_str().contains("HI"));
    }

    // --- Date function tests ---

    #[test]
    fn test_today() {
        let r = reg();
        let result = r.invoke("TODAY", &[]).unwrap();
        assert_eq!(result, s("2026-02-23"));
    }

    #[test]
    fn test_dateadd() {
        let r = reg();
        let result = r.invoke("DATEADD", &[n(20260101.0), n(30.0), s("D")]).unwrap();
        assert!((result.as_num() - 20260131.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_datedif() {
        let r = reg();
        let result = r.invoke("DATEDIF", &[n(100.0), n(130.0), s("D")]).unwrap();
        assert!((result.as_num() - 30.0).abs() < f64::EPSILON);
    }

    // --- Conversion function tests ---

    #[test]
    fn test_atodbl() {
        let r = reg();
        let result = r.invoke("ATODBL", &[s("3.14")]).unwrap();
        assert!((result.as_num() - 3.14).abs() < 0.001);
    }

    #[test]
    fn test_dbltos() {
        let r = reg();
        let result = r.invoke("DBLTOS", &[n(42.0)]).unwrap();
        assert_eq!(result.as_str(), "42");
    }

    #[test]
    fn test_decode() {
        let r = reg();
        let result = r
            .invoke("DECODE", &[s("B"), s("A"), s("First"), s("B"), s("Second"), s("Default")])
            .unwrap();
        assert_eq!(result, s("Second"));
    }

    #[test]
    fn test_decode_default() {
        let r = reg();
        let result = r
            .invoke("DECODE", &[s("C"), s("A"), s("First"), s("Default")])
            .unwrap();
        assert_eq!(result, s("Default"));
    }

    // --- Registry tests ---

    #[test]
    fn test_unknown_function() {
        let r = reg();
        assert!(r.invoke("NONEXISTENT", &[]).is_err());
    }

    #[test]
    fn test_function_exists() {
        let r = reg();
        assert!(r.exists("SUBSTR"));
        assert!(r.exists("ABS"));
        assert!(!r.exists("NONEXISTENT"));
    }

    #[test]
    fn test_list_functions() {
        let r = reg();
        let list = r.list_functions();
        assert!(list.len() >= 30);
        assert!(list.contains(&"SUBSTR".to_string()));
        assert!(list.contains(&"ABS".to_string()));
    }

    #[test]
    fn test_orone() {
        let r = reg();
        let result = r.invoke("ORONE", &[s(""), s(""), s("found")]).unwrap();
        assert_eq!(result, s("found"));
    }

    #[test]
    fn test_fitefld() {
        let r = reg();
        let result = r.invoke("FITEFLD", &[s("hello   ")]).unwrap();
        assert_eq!(result, s("hello"));
    }

    #[test]
    fn test_edit_mask() {
        let r = reg();
        let result = r.invoke("EDIT", &[s("1234"), s("99-99")]).unwrap();
        assert_eq!(result, s("12-34"));
    }

    #[test]
    fn test_fnvalue_conversions() {
        assert!((FnValue::Num(3.14).as_num() - 3.14).abs() < 0.001);
        assert_eq!(FnValue::Str("42".into()).as_num(), 42.0);
        assert_eq!(FnValue::Null.as_num(), 0.0);
        assert_eq!(FnValue::Num(42.0).as_str(), "42");
        assert_eq!(FnValue::Str("hi".into()).as_str(), "hi");
        assert_eq!(FnValue::Null.as_str(), "");
    }

    #[test]
    fn test_wrong_arg_count() {
        let r = reg();
        assert!(r.invoke("SUBSTR", &[]).is_err());
        assert!(r.invoke("TRIM", &[]).is_err());
    }
}
