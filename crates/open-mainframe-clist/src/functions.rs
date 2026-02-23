//! # CLIST Built-in Functions (CL-102)
//!
//! Provides ~15 built-in functions for CLIST scripts:
//! - &EVAL — arithmetic evaluation
//! - &SUBSTR, &LENGTH, &SYSINDEX — string operations
//! - &SYSCAPS, &SYSLC — case conversion
//! - &DATATYPE, &STR, &NRSTR, &SYSNSUB — type/substitution control
//! - &SYSDSN — dataset name checking

// ─────────────────────── Function Registry ───────────────────────

/// Built-in function identifier.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltinFunction {
    Eval,
    Substr,
    Length,
    SysIndex,
    SysCaps,
    SysLc,
    DataType,
    Str,
    NrStr,
    SysNsub,
    SysDsn,
}

impl BuiltinFunction {
    /// Parse function name.
    pub fn from_name(name: &str) -> Option<Self> {
        match name.to_uppercase().as_str() {
            "EVAL" => Some(Self::Eval),
            "SUBSTR" => Some(Self::Substr),
            "LENGTH" => Some(Self::Length),
            "SYSINDEX" => Some(Self::SysIndex),
            "SYSCAPS" => Some(Self::SysCaps),
            "SYSLC" => Some(Self::SysLc),
            "DATATYPE" => Some(Self::DataType),
            "STR" => Some(Self::Str),
            "NRSTR" => Some(Self::NrStr),
            "SYSNSUB" => Some(Self::SysNsub),
            "SYSDSN" => Some(Self::SysDsn),
            _ => None,
        }
    }
}

/// Function evaluation error.
#[derive(Debug, thiserror::Error)]
pub enum FunctionError {
    #[error("unknown function: &{0}")]
    Unknown(String),
    #[error("wrong number of arguments for &{func}: expected {expected}, got {got}")]
    ArgCount {
        func: String,
        expected: usize,
        got: usize,
    },
    #[error("invalid argument: {0}")]
    InvalidArg(String),
    #[error("arithmetic error: {0}")]
    Arithmetic(String),
}

// ─────────────────────── Evaluation ───────────────────────

/// Evaluate a built-in function.
pub fn evaluate_builtin(name: &str, args: &[String]) -> Result<String, FunctionError> {
    let func = BuiltinFunction::from_name(name)
        .ok_or_else(|| FunctionError::Unknown(name.to_string()))?;

    match func {
        BuiltinFunction::Eval => eval_func(args),
        BuiltinFunction::Substr => substr_func(args),
        BuiltinFunction::Length => length_func(args),
        BuiltinFunction::SysIndex => sysindex_func(args),
        BuiltinFunction::SysCaps => syscaps_func(args),
        BuiltinFunction::SysLc => syslc_func(args),
        BuiltinFunction::DataType => datatype_func(args),
        BuiltinFunction::Str => str_func(args),
        BuiltinFunction::NrStr => nrstr_func(args),
        BuiltinFunction::SysNsub => sysnsub_func(args),
        BuiltinFunction::SysDsn => sysdsn_func(args),
    }
}

// ─── CL-102.1: &EVAL ───

fn eval_func(args: &[String]) -> Result<String, FunctionError> {
    if args.is_empty() {
        return Err(FunctionError::ArgCount {
            func: "EVAL".to_string(),
            expected: 1,
            got: 0,
        });
    }

    let expr = &args[0];
    // Simple expression evaluator: handles +, -, *, /
    eval_arithmetic(expr)
}

fn eval_arithmetic(expr: &str) -> Result<String, FunctionError> {
    let trimmed = expr.trim();

    // Try direct number parse
    if let Ok(n) = trimmed.parse::<i64>() {
        return Ok(n.to_string());
    }

    // Look for +/- at top level (lowest precedence, right to left)
    let mut depth = 0;
    let chars: Vec<char> = trimmed.chars().collect();

    // Scan for + or - (not at start) from right
    for i in (1..chars.len()).rev() {
        match chars[i] {
            '(' => depth += 1,
            ')' => depth -= 1,
            '+' | '-' if depth == 0 => {
                let left = eval_arithmetic(&trimmed[..i])?;
                let right = eval_arithmetic(&trimmed[i + 1..])?;
                let l: i64 = left.parse().map_err(|_| FunctionError::Arithmetic(format!("not a number: {left}")))?;
                let r: i64 = right.parse().map_err(|_| FunctionError::Arithmetic(format!("not a number: {right}")))?;
                return Ok(if chars[i] == '+' { l + r } else { l - r }.to_string());
            }
            _ => {}
        }
    }

    // Scan for * or /
    depth = 0;
    for i in (1..chars.len()).rev() {
        match chars[i] {
            '(' => depth += 1,
            ')' => depth -= 1,
            '*' | '/' if depth == 0 => {
                let left = eval_arithmetic(&trimmed[..i])?;
                let right = eval_arithmetic(&trimmed[i + 1..])?;
                let l: i64 = left.parse().map_err(|_| FunctionError::Arithmetic(format!("not a number: {left}")))?;
                let r: i64 = right.parse().map_err(|_| FunctionError::Arithmetic(format!("not a number: {right}")))?;
                if chars[i] == '/' && r == 0 {
                    return Err(FunctionError::Arithmetic("division by zero".to_string()));
                }
                return Ok(if chars[i] == '*' { l * r } else { l / r }.to_string());
            }
            _ => {}
        }
    }

    // Handle parenthesized expression
    if chars.first() == Some(&'(') && chars.last() == Some(&')') {
        return eval_arithmetic(&trimmed[1..trimmed.len() - 1]);
    }

    // Try number again
    trimmed.parse::<i64>()
        .map(|n| n.to_string())
        .map_err(|_| FunctionError::Arithmetic(format!("cannot evaluate: {trimmed}")))
}

// ─── CL-102.2: String Functions ───

fn substr_func(args: &[String]) -> Result<String, FunctionError> {
    // &SUBSTR(start:end, string) or &SUBSTR(start, string)
    if args.len() < 2 {
        return Err(FunctionError::ArgCount {
            func: "SUBSTR".to_string(),
            expected: 2,
            got: args.len(),
        });
    }

    let pos_spec = &args[0];
    let string = &args[1];

    let (start, len) = if let Some(colon_pos) = pos_spec.find(':') {
        let s: usize = pos_spec[..colon_pos].trim().parse().unwrap_or(1);
        let e: usize = pos_spec[colon_pos + 1..].trim().parse().unwrap_or(string.len());
        (s, e - s + 1)
    } else {
        let s: usize = pos_spec.trim().parse().unwrap_or(1);
        (s, string.len() - s + 1)
    };

    let start_idx = start.saturating_sub(1);
    let end_idx = (start_idx + len).min(string.len());

    if start_idx >= string.len() {
        Ok(String::new())
    } else {
        Ok(string[start_idx..end_idx].to_string())
    }
}

fn length_func(args: &[String]) -> Result<String, FunctionError> {
    if args.is_empty() {
        return Err(FunctionError::ArgCount {
            func: "LENGTH".to_string(),
            expected: 1,
            got: 0,
        });
    }
    Ok(args[0].len().to_string())
}

fn sysindex_func(args: &[String]) -> Result<String, FunctionError> {
    // &SYSINDEX(needle, haystack [, start])
    if args.len() < 2 {
        return Err(FunctionError::ArgCount {
            func: "SYSINDEX".to_string(),
            expected: 2,
            got: args.len(),
        });
    }

    let needle = &args[0];
    let haystack = &args[1];
    let start = args.get(2)
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(1)
        .saturating_sub(1);

    if start >= haystack.len() {
        return Ok("0".to_string());
    }

    match haystack[start..].find(needle.as_str()) {
        Some(pos) => Ok((pos + start + 1).to_string()), // 1-based
        None => Ok("0".to_string()),
    }
}

fn syscaps_func(args: &[String]) -> Result<String, FunctionError> {
    if args.is_empty() {
        return Err(FunctionError::ArgCount {
            func: "SYSCAPS".to_string(),
            expected: 1,
            got: 0,
        });
    }
    Ok(args[0].to_uppercase())
}

fn syslc_func(args: &[String]) -> Result<String, FunctionError> {
    if args.is_empty() {
        return Err(FunctionError::ArgCount {
            func: "SYSLC".to_string(),
            expected: 1,
            got: 0,
        });
    }
    Ok(args[0].to_lowercase())
}

// ─── CL-102.3: Data Functions ───

fn datatype_func(args: &[String]) -> Result<String, FunctionError> {
    if args.is_empty() {
        return Err(FunctionError::ArgCount {
            func: "DATATYPE".to_string(),
            expected: 1,
            got: 0,
        });
    }
    let val = &args[0];
    if val.parse::<i64>().is_ok() {
        Ok("NUM".to_string())
    } else {
        Ok("CHAR".to_string())
    }
}

fn str_func(args: &[String]) -> Result<String, FunctionError> {
    // &STR() returns its argument without variable substitution
    // Since substitution already happened, just return as-is
    if args.is_empty() {
        Ok(String::new())
    } else {
        Ok(args[0].clone())
    }
}

fn nrstr_func(args: &[String]) -> Result<String, FunctionError> {
    // &NRSTR() returns its argument suppressing all substitution
    if args.is_empty() {
        Ok(String::new())
    } else {
        Ok(args[0].clone())
    }
}

fn sysnsub_func(args: &[String]) -> Result<String, FunctionError> {
    // &SYSNSUB() returns 0 if no substitution occurred, 1 otherwise
    if args.is_empty() {
        Ok("0".to_string())
    } else {
        // Check if the argument contains & (variable reference)
        if args[0].contains('&') {
            Ok("1".to_string())
        } else {
            Ok("0".to_string())
        }
    }
}

// ─── CL-102.4: System Functions ───

fn sysdsn_func(args: &[String]) -> Result<String, FunctionError> {
    // &SYSDSN('dsname') — checks if dataset exists
    // Since we can't really check, return a standard response
    if args.is_empty() {
        return Err(FunctionError::ArgCount {
            func: "SYSDSN".to_string(),
            expected: 1,
            got: 0,
        });
    }
    let dsname = &args[0];
    if dsname.is_empty() {
        Ok("MISSING DATASET NAME".to_string())
    } else {
        Ok("OK".to_string())
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ─── CL-102.1: &EVAL ───

    #[test]
    fn test_eval_simple() {
        let result = evaluate_builtin("EVAL", &["42".to_string()]).unwrap();
        assert_eq!(result, "42");
    }

    #[test]
    fn test_eval_addition() {
        let result = evaluate_builtin("EVAL", &["3+4".to_string()]).unwrap();
        assert_eq!(result, "7");
    }

    #[test]
    fn test_eval_multiplication() {
        let result = evaluate_builtin("EVAL", &["5*6".to_string()]).unwrap();
        assert_eq!(result, "30");
    }

    #[test]
    fn test_eval_precedence() {
        let result = evaluate_builtin("EVAL", &["2+3*4".to_string()]).unwrap();
        assert_eq!(result, "14");
    }

    #[test]
    fn test_eval_division() {
        let result = evaluate_builtin("EVAL", &["10/3".to_string()]).unwrap();
        assert_eq!(result, "3");
    }

    #[test]
    fn test_eval_division_by_zero() {
        let result = evaluate_builtin("EVAL", &["10/0".to_string()]);
        assert!(result.is_err());
    }

    // ─── CL-102.2: String Functions ───

    #[test]
    fn test_substr() {
        let result = evaluate_builtin("SUBSTR", &["2:4".to_string(), "HELLO".to_string()]).unwrap();
        assert_eq!(result, "ELL");
    }

    #[test]
    fn test_substr_start_only() {
        let result = evaluate_builtin("SUBSTR", &["3".to_string(), "HELLO".to_string()]).unwrap();
        assert_eq!(result, "LLO");
    }

    #[test]
    fn test_length() {
        let result = evaluate_builtin("LENGTH", &["HELLO".to_string()]).unwrap();
        assert_eq!(result, "5");
    }

    #[test]
    fn test_sysindex_found() {
        let result = evaluate_builtin("SYSINDEX", &["LL".to_string(), "HELLO".to_string()]).unwrap();
        assert_eq!(result, "3");
    }

    #[test]
    fn test_sysindex_not_found() {
        let result = evaluate_builtin("SYSINDEX", &["XY".to_string(), "HELLO".to_string()]).unwrap();
        assert_eq!(result, "0");
    }

    #[test]
    fn test_syscaps() {
        let result = evaluate_builtin("SYSCAPS", &["hello world".to_string()]).unwrap();
        assert_eq!(result, "HELLO WORLD");
    }

    #[test]
    fn test_syslc() {
        let result = evaluate_builtin("SYSLC", &["HELLO WORLD".to_string()]).unwrap();
        assert_eq!(result, "hello world");
    }

    // ─── CL-102.3: Data Functions ───

    #[test]
    fn test_datatype_num() {
        let result = evaluate_builtin("DATATYPE", &["123".to_string()]).unwrap();
        assert_eq!(result, "NUM");
    }

    #[test]
    fn test_datatype_char() {
        let result = evaluate_builtin("DATATYPE", &["ABC".to_string()]).unwrap();
        assert_eq!(result, "CHAR");
    }

    #[test]
    fn test_str() {
        let result = evaluate_builtin("STR", &["test".to_string()]).unwrap();
        assert_eq!(result, "test");
    }

    // ─── CL-102.4: System Functions ───

    #[test]
    fn test_sysdsn_ok() {
        let result = evaluate_builtin("SYSDSN", &["MY.DATA.SET".to_string()]).unwrap();
        assert_eq!(result, "OK");
    }

    #[test]
    fn test_sysdsn_missing() {
        let result = evaluate_builtin("SYSDSN", &["".to_string()]).unwrap();
        assert_eq!(result, "MISSING DATASET NAME");
    }

    #[test]
    fn test_unknown_function() {
        let result = evaluate_builtin("NOSUCHFUNC", &[]);
        assert!(result.is_err());
    }
}
