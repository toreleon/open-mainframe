//! REXX values — all values are strings, with decimal arithmetic.
//!
//! REXX has no typed variables; every value is a string.  Arithmetic is
//! performed by interpreting strings as decimal numbers with configurable
//! precision (NUMERIC DIGITS).

use std::cmp::Ordering;
use std::fmt;

/// A REXX value — always stored as a string representation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RexxValue(pub String);

impl RexxValue {
    /// Create a value from a string.
    pub fn from_string(s: impl Into<String>) -> Self {
        Self(s.into())
    }

    /// Create a zero value.
    pub fn zero() -> Self {
        Self("0".into())
    }

    /// Create a one value.
    pub fn one() -> Self {
        Self("1".into())
    }

    /// Return the string contents.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Check if this value is a valid number.
    pub fn is_number(&self) -> bool {
        parse_number(&self.0).is_some()
    }

    /// Interpret this value as a boolean (REXX: "0" or "1" only).
    pub fn as_bool(&self) -> Option<bool> {
        match self.0.trim() {
            "0" => Some(false),
            "1" => Some(true),
            _ => None,
        }
    }

    /// The length of the string value.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl fmt::Display for RexxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

// ---------------------------------------------------------------------------
//  Internal decimal representation — uses a flat digit array + exponent
// ---------------------------------------------------------------------------

/// A decimal number as: sign * coeff * 10^exp, where coeff is a digit array.
/// E.g. 12.34 = +[1,2,3,4] * 10^-2, with exp = -2.
#[derive(Debug, Clone)]
struct Decimal {
    negative: bool,
    /// Coefficient digits (most-significant first). Always non-empty.
    coeff: Vec<u8>,
    /// Power of 10 applied to the *last* digit.
    /// E.g. [1,2,3] with exp=-1 means 12.3, [1,2,3] with exp=0 means 123.
    exp: i64,
}

impl Decimal {
    fn zero() -> Self {
        Self {
            negative: false,
            coeff: vec![0],
            exp: 0,
        }
    }

    fn is_zero(&self) -> bool {
        self.coeff.iter().all(|&d| d == 0)
    }

    /// Adjusted exponent (exponent of most-significant digit).
    fn adjusted_exp(&self) -> i64 {
        self.exp + self.coeff.len() as i64 - 1
    }

    /// Normalize: strip leading zeros (keep one) and trailing zeros.
    fn normalize(&mut self) {
        // Strip leading zeros.
        while self.coeff.len() > 1 && self.coeff[0] == 0 {
            self.coeff.remove(0);
        }
        // Strip trailing zeros.
        while self.coeff.len() > 1 && self.coeff.last() == Some(&0) {
            self.coeff.pop();
            self.exp += 1;
        }
        if self.is_zero() {
            self.negative = false;
        }
    }

    /// Format as a REXX string with given NUMERIC DIGITS.
    fn to_rexx_string(&self, digits: usize) -> String {
        if self.is_zero() {
            return "0".to_string();
        }

        // Round to `digits` significant digits.
        let rounded = round_digits(&self.coeff, digits);
        let adj_exp = self.exp + (self.coeff.len() as i64) - 1;
        // After rounding, the adjusted exponent might shift if rounding added a digit.
        let new_adj = adj_exp + (rounded.len() as i64 - self.coeff.len().min(digits) as i64);
        let _ = new_adj;

        // Recalculate: the rounded digits represent a number whose adjusted exponent
        // is the same as the original (adj_exp).
        // So the exp for rounded is: adj_exp - (rounded.len() - 1).
        let result_exp = adj_exp - rounded.len() as i64 + 1;

        // Decide plain vs scientific notation.
        // REXX: use plain if adjusted exponent is >= 0 and < 2 * digits.
        // Also use plain for small negative exponents.
        let use_plain = adj_exp >= 0 && adj_exp < (2 * digits) as i64;
        let use_plain_neg = adj_exp < 0 && adj_exp >= -(digits as i64);

        let mut s = String::new();
        if self.negative {
            s.push('-');
        }

        if use_plain || use_plain_neg {
            // Plain notation: reconstruct the number.
            // result_exp is the position of the last coefficient digit.
            // If result_exp >= 0, all digits are integer with trailing zeros.
            if result_exp >= 0 {
                for &d in &rounded {
                    s.push((b'0' + d) as char);
                }
                for _ in 0..result_exp {
                    s.push('0');
                }
            } else {
                // Some digits are fractional.
                let frac_digits = (-result_exp) as usize;
                if rounded.len() > frac_digits {
                    let int_len = rounded.len() - frac_digits;
                    for &d in &rounded[..int_len] {
                        s.push((b'0' + d) as char);
                    }
                    s.push('.');
                    for &d in &rounded[int_len..] {
                        s.push((b'0' + d) as char);
                    }
                } else {
                    // All digits fractional, need leading "0."
                    s.push_str("0.");
                    let pad = frac_digits - rounded.len();
                    for _ in 0..pad {
                        s.push('0');
                    }
                    for &d in &rounded {
                        s.push((b'0' + d) as char);
                    }
                }
            }
        } else {
            // Scientific notation.
            s.push((b'0' + rounded[0]) as char);
            if rounded.len() > 1 {
                s.push('.');
                for &d in &rounded[1..] {
                    s.push((b'0' + d) as char);
                }
            }
            s.push('E');
            if adj_exp >= 0 {
                s.push('+');
            }
            s.push_str(&adj_exp.to_string());
        }
        s
    }
}

/// Round a digit vector to at most `n` significant digits.
fn round_digits(digits: &[u8], n: usize) -> Vec<u8> {
    if digits.len() <= n {
        return digits.to_vec();
    }
    let mut result: Vec<u8> = digits[..n].to_vec();
    if digits[n] >= 5 {
        let mut carry = true;
        for d in result.iter_mut().rev() {
            if carry {
                *d += 1;
                if *d >= 10 {
                    *d = 0;
                } else {
                    carry = false;
                }
            }
        }
        if carry {
            result.insert(0, 1);
        }
    }
    // Strip trailing zeros.
    while result.len() > 1 && result.last() == Some(&0) {
        result.pop();
    }
    result
}

/// Parse a REXX number string into a Decimal.
fn parse_number(s: &str) -> Option<Decimal> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }

    let mut chars = s.chars().peekable();
    let negative = match chars.peek() {
        Some('+') => { chars.next(); false }
        Some('-') => { chars.next(); true }
        _ => false,
    };

    let mut int_digits = Vec::new();
    while let Some(&c) = chars.peek() {
        if c.is_ascii_digit() {
            int_digits.push((c as u8) - b'0');
            chars.next();
        } else {
            break;
        }
    }

    let mut frac_digits = Vec::new();
    if chars.peek() == Some(&'.') {
        chars.next();
        while let Some(&c) = chars.peek() {
            if c.is_ascii_digit() {
                frac_digits.push((c as u8) - b'0');
                chars.next();
            } else {
                break;
            }
        }
    }

    if int_digits.is_empty() && frac_digits.is_empty() {
        return None;
    }

    // Optional exponent.
    let mut exponent: i64 = 0;
    if let Some(&c) = chars.peek() {
        if c == 'e' || c == 'E' {
            chars.next();
            let exp_neg = match chars.peek() {
                Some('+') => { chars.next(); false }
                Some('-') => { chars.next(); true }
                _ => false,
            };
            let mut exp_val: i64 = 0;
            let mut has_digit = false;
            for c in chars.by_ref() {
                if c.is_ascii_digit() {
                    exp_val = exp_val * 10 + (c as u8 - b'0') as i64;
                    has_digit = true;
                } else {
                    return None;
                }
            }
            if !has_digit {
                return None;
            }
            exponent = if exp_neg { -exp_val } else { exp_val };
        }
    }

    if chars.next().is_some() {
        return None;
    }

    // Build coefficient: int_digits + frac_digits.
    let mut coeff = int_digits;
    coeff.extend_from_slice(&frac_digits);
    // exp of the last coefficient digit.
    let exp = exponent - frac_digits.len() as i64;

    // Strip leading zeros (keep at least one).
    while coeff.len() > 1 && coeff[0] == 0 {
        coeff.remove(0);
    }

    let is_zero = coeff.iter().all(|&d| d == 0);

    Some(Decimal {
        negative: negative && !is_zero,
        coeff,
        exp,
    })
}

// ---------------------------------------------------------------------------
//  Arithmetic operations (public API)
// ---------------------------------------------------------------------------

/// Numeric settings for REXX arithmetic.
#[derive(Debug, Clone)]
pub struct NumericSettings {
    pub digits: usize,
    pub fuzz: usize,
    pub form: NumericForm,
}

impl Default for NumericSettings {
    fn default() -> Self {
        Self { digits: 9, fuzz: 0, form: NumericForm::Scientific }
    }
}

/// NUMERIC FORM setting.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericForm {
    Scientific,
    Engineering,
}

pub fn rexx_add(a: &str, b: &str, settings: &NumericSettings) -> Result<String, String> {
    let da = parse_number(a).ok_or_else(|| format!("Bad arithmetic conversion: '{a}'"))?;
    let db = parse_number(b).ok_or_else(|| format!("Bad arithmetic conversion: '{b}'"))?;
    let result = add_dec(&da, &db);
    Ok(result.to_rexx_string(settings.digits))
}

pub fn rexx_sub(a: &str, b: &str, settings: &NumericSettings) -> Result<String, String> {
    let da = parse_number(a).ok_or_else(|| format!("Bad arithmetic conversion: '{a}'"))?;
    let mut db = parse_number(b).ok_or_else(|| format!("Bad arithmetic conversion: '{b}'"))?;
    db.negative = !db.negative;
    if db.is_zero() { db.negative = false; }
    let result = add_dec(&da, &db);
    Ok(result.to_rexx_string(settings.digits))
}

pub fn rexx_mul(a: &str, b: &str, settings: &NumericSettings) -> Result<String, String> {
    let da = parse_number(a).ok_or_else(|| format!("Bad arithmetic conversion: '{a}'"))?;
    let db = parse_number(b).ok_or_else(|| format!("Bad arithmetic conversion: '{b}'"))?;
    let result = mul_dec(&da, &db);
    Ok(result.to_rexx_string(settings.digits))
}

pub fn rexx_div(a: &str, b: &str, settings: &NumericSettings) -> Result<String, String> {
    let da = parse_number(a).ok_or_else(|| format!("Bad arithmetic conversion: '{a}'"))?;
    let db = parse_number(b).ok_or_else(|| format!("Bad arithmetic conversion: '{b}'"))?;
    if db.is_zero() {
        return Err("Arithmetic overflow/underflow (division by zero)".into());
    }
    let result = div_dec(&da, &db, settings.digits);
    Ok(result.to_rexx_string(settings.digits))
}

pub fn rexx_idiv(a: &str, b: &str, settings: &NumericSettings) -> Result<String, String> {
    let da = parse_number(a).ok_or_else(|| format!("Bad arithmetic conversion: '{a}'"))?;
    let db = parse_number(b).ok_or_else(|| format!("Bad arithmetic conversion: '{b}'"))?;
    if db.is_zero() {
        return Err("Arithmetic overflow/underflow (division by zero)".into());
    }
    let quotient = div_dec(&da, &db, settings.digits + 1);
    // Truncate to integer: keep only digits with exp >= 0.
    let mut trunc = quotient;
    if trunc.exp < 0 {
        let remove = (-trunc.exp) as usize;
        if remove >= trunc.coeff.len() {
            return Ok("0".into());
        }
        trunc.coeff.truncate(trunc.coeff.len() - remove);
        trunc.exp = 0;
    }
    trunc.normalize();
    Ok(trunc.to_rexx_string(settings.digits))
}

pub fn rexx_rem(a: &str, b: &str, settings: &NumericSettings) -> Result<String, String> {
    let da = parse_number(a).ok_or_else(|| format!("Bad arithmetic conversion: '{a}'"))?;
    let db = parse_number(b).ok_or_else(|| format!("Bad arithmetic conversion: '{b}'"))?;
    if db.is_zero() {
        return Err("Arithmetic overflow/underflow (division by zero)".into());
    }
    // remainder = a - trunc(a/b)*b
    let quotient = div_dec(&da, &db, settings.digits + 1);
    let mut trunc_q = quotient;
    if trunc_q.exp < 0 {
        let remove = (-trunc_q.exp) as usize;
        if remove >= trunc_q.coeff.len() {
            trunc_q = Decimal::zero();
        } else {
            trunc_q.coeff.truncate(trunc_q.coeff.len() - remove);
            trunc_q.exp = 0;
        }
    }
    trunc_q.normalize();
    let prod = mul_dec(&trunc_q, &db);
    let mut neg_prod = prod;
    neg_prod.negative = !neg_prod.negative;
    if neg_prod.is_zero() { neg_prod.negative = false; }
    let result = add_dec(&da, &neg_prod);
    Ok(result.to_rexx_string(settings.digits))
}

pub fn rexx_pow(a: &str, b: &str, settings: &NumericSettings) -> Result<String, String> {
    let da = parse_number(a).ok_or_else(|| format!("Bad arithmetic conversion: '{a}'"))?;
    let db = parse_number(b).ok_or_else(|| format!("Bad arithmetic conversion: '{b}'"))?;
    if db.exp < 0 {
        return Err("Exponent must be a whole number".into());
    }
    // Convert exponent to i64.
    let mut exp_val: i64 = 0;
    for &d in &db.coeff {
        exp_val = exp_val * 10 + d as i64;
    }
    // Account for trailing zeros (exp > 0).
    for _ in 0..db.exp {
        exp_val *= 10;
    }
    if db.negative { exp_val = -exp_val; }

    if exp_val == 0 {
        return Ok("1".into());
    }

    let neg_exp = exp_val < 0;
    let abs_exp = exp_val.unsigned_abs();

    let mut result = Decimal { negative: false, coeff: vec![1], exp: 0 };
    let mut base = da;
    let mut e = abs_exp;
    while e > 0 {
        if e & 1 == 1 {
            result = mul_dec(&result, &base);
        }
        e >>= 1;
        if e > 0 {
            base = mul_dec(&base, &base);
        }
    }

    if neg_exp {
        let one = Decimal { negative: false, coeff: vec![1], exp: 0 };
        result = div_dec(&one, &result, settings.digits);
    }

    Ok(result.to_rexx_string(settings.digits))
}

/// Compare two REXX values numerically.
pub fn rexx_compare(a: &str, b: &str, _fuzz: usize) -> Result<Ordering, String> {
    let da = parse_number(a).ok_or_else(|| format!("Bad arithmetic conversion: '{a}'"))?;
    let db = parse_number(b).ok_or_else(|| format!("Bad arithmetic conversion: '{b}'"))?;
    Ok(cmp_dec(&da, &db))
}

// ---------------------------------------------------------------------------
//  Low-level decimal operations using the (coeff, exp) representation
// ---------------------------------------------------------------------------

/// Align two decimals to the same exponent, returning flat digit arrays
/// and the common exponent.
fn align(a: &Decimal, b: &Decimal) -> (Vec<u8>, Vec<u8>, i64) {
    let min_exp = a.exp.min(b.exp);
    let a_pad = (a.exp - min_exp) as usize;
    let b_pad = (b.exp - min_exp) as usize;

    let mut a_digits = a.coeff.clone();
    a_digits.resize(a_digits.len() + a_pad, 0);

    let mut b_digits = b.coeff.clone();
    b_digits.resize(b_digits.len() + b_pad, 0);

    // Now both have the same exp = min_exp.
    // Pad the shorter one with leading zeros.
    let max_len = a_digits.len().max(b_digits.len());
    while a_digits.len() < max_len {
        a_digits.insert(0, 0);
    }
    while b_digits.len() < max_len {
        b_digits.insert(0, 0);
    }

    (a_digits, b_digits, min_exp)
}

/// Add two aligned digit arrays (unsigned), return digits + carry.
fn add_aligned(a: &[u8], b: &[u8]) -> Vec<u8> {
    let n = a.len();
    let mut result = vec![0u8; n];
    let mut carry = 0u8;
    for i in (0..n).rev() {
        let sum = a[i] + b[i] + carry;
        result[i] = sum % 10;
        carry = sum / 10;
    }
    if carry > 0 {
        result.insert(0, carry);
    }
    result
}

/// Subtract aligned arrays: a - b (assumes a >= b), return digits.
fn sub_aligned(a: &[u8], b: &[u8]) -> Vec<u8> {
    let n = a.len();
    let mut result = vec![0u8; n];
    let mut borrow = 0i8;
    for i in (0..n).rev() {
        let mut diff = a[i] as i8 - b[i] as i8 - borrow;
        if diff < 0 {
            diff += 10;
            borrow = 1;
        } else {
            borrow = 0;
        }
        result[i] = diff as u8;
    }
    result
}

/// Compare magnitudes of aligned arrays.
fn cmp_aligned(a: &[u8], b: &[u8]) -> Ordering {
    for (da, db) in a.iter().zip(b.iter()) {
        if da != db {
            return da.cmp(db);
        }
    }
    Ordering::Equal
}

fn add_dec(a: &Decimal, b: &Decimal) -> Decimal {
    let (ad, bd, exp) = align(a, b);

    if a.negative == b.negative {
        let coeff = add_aligned(&ad, &bd);
        let mut d = Decimal { negative: a.negative, coeff, exp };
        d.normalize();
        d
    } else {
        match cmp_aligned(&ad, &bd) {
            Ordering::Greater | Ordering::Equal => {
                let coeff = sub_aligned(&ad, &bd);
                let mut d = Decimal { negative: a.negative, coeff, exp };
                d.normalize();
                d
            }
            Ordering::Less => {
                let coeff = sub_aligned(&bd, &ad);
                let mut d = Decimal { negative: b.negative, coeff, exp };
                d.normalize();
                d
            }
        }
    }
}

fn mul_dec(a: &Decimal, b: &Decimal) -> Decimal {
    let a_coeff = &a.coeff;
    let b_coeff = &b.coeff;
    let result_exp = a.exp + b.exp;

    let mut product = vec![0u32; a_coeff.len() + b_coeff.len()];
    for (i, &da) in a_coeff.iter().enumerate().rev() {
        for (j, &db) in b_coeff.iter().enumerate().rev() {
            let p = da as u32 * db as u32;
            let pos = i + j + 1;
            product[pos] += p;
        }
    }
    // Propagate carries.
    for i in (1..product.len()).rev() {
        product[i - 1] += product[i] / 10;
        product[i] %= 10;
    }

    let coeff: Vec<u8> = product.iter().map(|&d| d as u8).collect();
    let neg = a.negative != b.negative;

    let mut d = Decimal { negative: neg, coeff, exp: result_exp };
    d.normalize();
    d
}

fn div_dec(a: &Decimal, b: &Decimal, prec: usize) -> Decimal {
    let neg = a.negative != b.negative;

    if a.is_zero() {
        return Decimal::zero();
    }

    // Strategy: convert to integers, do integer long division, then fix up exponent.
    // a = a.coeff * 10^a.exp, b = b.coeff * 10^b.exp
    // a/b = (a.coeff / b.coeff) * 10^(a.exp - b.exp)
    // To get `prec` significant digits, we pad a.coeff with enough trailing zeros
    // so the integer division produces enough digits.

    let mut a_coeff = a.coeff.clone();
    while a_coeff.len() > 1 && a_coeff[0] == 0 { a_coeff.remove(0); }
    let mut b_coeff = b.coeff.clone();
    while b_coeff.len() > 1 && b_coeff[0] == 0 { b_coeff.remove(0); }

    // Pad a_coeff with zeros to ensure enough quotient digits.
    // We need at least prec + 2 quotient digits.
    // The quotient of two N-digit and M-digit numbers has roughly N-M+1 digits.
    let extra_zeros = prec + 2 + b_coeff.len();
    let mut padded_a = a_coeff.clone();
    padded_a.resize(padded_a.len() + extra_zeros, 0);

    // Now perform integer long division: padded_a / b_coeff.
    let mut quotient_digits: Vec<u8> = Vec::new();
    let mut remainder: Vec<u8> = vec![0];
    let mut started = false;

    for &digit in &padded_a {
        remainder.push(digit);
        while remainder.len() > 1 && remainder[0] == 0 {
            remainder.remove(0);
        }

        let mut q = 0u8;
        while remainder.len() > b_coeff.len()
            || (remainder.len() == b_coeff.len() && cmp_aligned(&remainder, &b_coeff) != Ordering::Less)
        {
            remainder = sub_aligned_unequal(&remainder, &b_coeff);
            q += 1;
        }

        if q > 0 || started {
            quotient_digits.push(q);
            started = true;
        }

        if started && quotient_digits.len() >= prec + 2 {
            break;
        }
    }

    if quotient_digits.is_empty() {
        return Decimal::zero();
    }

    // The exponent: padded_a was a_coeff * 10^extra_zeros as an integer.
    // So the integer quotient represents: (a_coeff * 10^extra_zeros) / b_coeff
    // We consumed some digits from padded_a. If we consumed `consumed` digits total,
    // the quotient's value is quotient_digits * 10^(padded_a.len() - consumed - quotient_digits.len() + 1)
    // ... but that's complex. Simpler approach:
    //
    // The integer quotient of padded_a / b_coeff is the quotient_digits followed by
    // (padded_a.len() - consumed) zeros. But we may have stopped early.
    //
    // Actually: the true result is a.coeff/b.coeff * 10^(a.exp - b.exp).
    // padded_a = a.coeff * 10^extra_zeros (as integer).
    // So quotient_digits * 10^(remaining_digits) ≈ a.coeff * 10^extra_zeros / b.coeff
    // where remaining_digits = padded_a.len() - consumed - quotient_digits.len() + 1
    //
    // Actually, the number of digits consumed from padded_a determines the "position":
    // After consuming `consumed` digits, quotient has `quotient_digits.len()` digits, and
    // represents: actual_quotient * 10^(padded_a.len() - consumed)
    // No... the standard long-division result after consuming all digits would give us:
    // quotient * 10^0 + remainder. We stopped after consuming some digits.
    //
    // Let's use: the quotient integer (all of padded_a consumed) would have been
    // quotient * 10^(padded_a.len() - consumed) (approximately, but enough for our needs).
    //
    // The final Decimal exponent:
    // The quotient represents a_coeff * 10^extra_zeros / b_coeff, which means
    // a/b = quotient * 10^(a.exp - b.exp - extra_zeros)
    //
    // But quotient = quotient_digits (as integer) * 10^remaining_zeros where
    // remaining_zeros might be 0 if we consumed enough digits.
    //
    // Simplest correct formula: the result digits have an exponent such that the
    // adjusted exponent of the result = adjusted_exp(a) - adjusted_exp(b) (roughly).
    // More precisely: result * 10^result_exp = a/b
    // result_digits represent an integer. The integer quotient of padded_a / b_coeff
    // = a.coeff * 10^extra_zeros / b.coeff
    // So result_digits * 10^unconsumed ≈ a.coeff * 10^extra_zeros / b.coeff
    // result = result_digits * 10^(unconsumed + a.exp - b.exp - extra_zeros)
    // where unconsumed = padded_a.len() - consumed (digits not brought down yet).

    // But we don't track consumed perfectly since we break early. Let's compute it:
    // We iterated through padded_a and consumed digits. The loop processes one digit
    // per iteration. Let's count consumed by checking how many digits we went through.
    // Actually the break condition was based on quotient_digits length, so let's just
    // count the number of iterations we went through. We can compute this from context:
    // We process digits sequentially. If we processed `n` digits from padded_a:
    // quotient_digits is the long-division result of the first n digits of padded_a / b_coeff.
    // The remaining digits would be padded_a.len() - n appended zeros to the quotient.
    // So the quotient integer ≈ quotient_digits * 10^(padded_a.len() - n).
    // But we actually tracked that in the loop. Since we used a for loop over padded_a,
    // we can't easily tell how many we consumed. Let me restructure:

    // --- Redo with explicit consumed count ---
    let mut quotient_digits2: Vec<u8> = Vec::new();
    let mut rem2: Vec<u8> = vec![0];
    let mut started2 = false;
    let mut consumed = 0usize;

    for &digit in &padded_a {
        consumed += 1;
        rem2.push(digit);
        while rem2.len() > 1 && rem2[0] == 0 {
            rem2.remove(0);
        }

        let mut q = 0u8;
        while rem2.len() > b_coeff.len()
            || (rem2.len() == b_coeff.len() && cmp_aligned(&rem2, &b_coeff) != Ordering::Less)
        {
            rem2 = sub_aligned_unequal(&rem2, &b_coeff);
            q += 1;
        }

        if q > 0 || started2 {
            quotient_digits2.push(q);
            started2 = true;
        }

        if started2 && quotient_digits2.len() >= prec + 2 {
            break;
        }
    }

    if quotient_digits2.is_empty() {
        return Decimal::zero();
    }

    let unconsumed = padded_a.len() as i64 - consumed as i64;
    // The effective exponent of the quotient coefficient.
    let coeff_exp = unconsumed + a.exp - b.exp - extra_zeros as i64;

    let rounded = round_digits(&quotient_digits2, prec);
    let final_exp = coeff_exp + (quotient_digits2.len() as i64 - rounded.len() as i64);

    let mut d = Decimal { negative: neg, coeff: rounded, exp: final_exp };
    d.normalize();
    d
}

/// Subtract b from a where arrays may differ in length (a >= b assumed).
fn sub_aligned_unequal(a: &[u8], b: &[u8]) -> Vec<u8> {
    let n = a.len();
    let mut result = a.to_vec();
    let mut borrow = 0i8;
    let offset = n - b.len();
    for i in (0..n).rev() {
        let bi = if i >= offset { b[i - offset] as i8 } else { 0 };
        let mut diff = result[i] as i8 - bi - borrow;
        if diff < 0 {
            diff += 10;
            borrow = 1;
        } else {
            borrow = 0;
        }
        result[i] = diff as u8;
    }
    // Strip leading zeros.
    while result.len() > 1 && result[0] == 0 {
        result.remove(0);
    }
    result
}

fn cmp_dec(a: &Decimal, b: &Decimal) -> Ordering {
    if a.is_zero() && b.is_zero() {
        return Ordering::Equal;
    }
    match (a.negative, b.negative) {
        (true, false) => return Ordering::Less,
        (false, true) => return Ordering::Greater,
        _ => {}
    }
    // Same sign: compare adjusted exponents first, then coefficients.
    let a_adj = a.adjusted_exp();
    let b_adj = b.adjusted_exp();
    let mag = if a_adj != b_adj {
        a_adj.cmp(&b_adj)
    } else {
        // Same order of magnitude: align and compare digit-by-digit.
        let (ad, bd, _) = align(a, b);
        cmp_aligned(&ad, &bd)
    };
    if a.negative { mag.reverse() } else { mag }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_integers() {
        let settings = NumericSettings::default();
        assert_eq!(rexx_add("3", "4", &settings).unwrap(), "7");
        assert_eq!(rexx_add("100", "200", &settings).unwrap(), "300");
        assert_eq!(rexx_add("-5", "3", &settings).unwrap(), "-2");
    }

    #[test]
    fn test_subtraction() {
        let settings = NumericSettings::default();
        assert_eq!(rexx_sub("10", "3", &settings).unwrap(), "7");
        assert_eq!(rexx_sub("3", "10", &settings).unwrap(), "-7");
        assert_eq!(rexx_sub("5", "5", &settings).unwrap(), "0");
    }

    #[test]
    fn test_multiplication() {
        let settings = NumericSettings::default();
        assert_eq!(rexx_mul("6", "7", &settings).unwrap(), "42");
        assert_eq!(rexx_mul("-3", "4", &settings).unwrap(), "-12");
        // 1.5 * 2 = 3.0 — REXX preserves trailing zero from multiplication.
        let r = rexx_mul("1.5", "2", &settings).unwrap();
        assert!(r == "3" || r == "3.0", "got {r}");
    }

    #[test]
    fn test_division() {
        let settings = NumericSettings::default();
        let r = rexx_div("10", "3", &settings).unwrap();
        assert!(r.starts_with("3.3333333"), "got {r}");
        assert_eq!(rexx_div("100", "4", &settings).unwrap(), "25");
        assert!(rexx_div("1", "0", &settings).is_err());
    }

    #[test]
    fn test_integer_division() {
        let settings = NumericSettings::default();
        assert_eq!(rexx_idiv("10", "3", &settings).unwrap(), "3");
        assert_eq!(rexx_idiv("17", "5", &settings).unwrap(), "3");
    }

    #[test]
    fn test_remainder() {
        let settings = NumericSettings::default();
        assert_eq!(rexx_rem("10", "3", &settings).unwrap(), "1");
        assert_eq!(rexx_rem("17", "5", &settings).unwrap(), "2");
    }

    #[test]
    fn test_power() {
        let settings = NumericSettings::default();
        assert_eq!(rexx_pow("2", "10", &settings).unwrap(), "1024");
        assert_eq!(rexx_pow("3", "0", &settings).unwrap(), "1");
        assert_eq!(rexx_pow("5", "2", &settings).unwrap(), "25");
    }

    #[test]
    fn test_decimal_arithmetic() {
        let settings = NumericSettings::default();
        assert_eq!(rexx_add("1.5", "2.3", &settings).unwrap(), "3.8");
        assert_eq!(rexx_mul("0.1", "0.2", &settings).unwrap(), "0.02");
    }

    #[test]
    fn test_numeric_digits() {
        let settings = NumericSettings { digits: 20, ..Default::default() };
        let result = rexx_div("1", "3", &settings).unwrap();
        assert!(result.starts_with("0.3333333333333333333"), "got {result}");
    }

    #[test]
    fn test_compare() {
        assert_eq!(rexx_compare("5", "3", 0).unwrap(), Ordering::Greater);
        assert_eq!(rexx_compare("3", "5", 0).unwrap(), Ordering::Less);
        assert_eq!(rexx_compare("5", "5", 0).unwrap(), Ordering::Equal);
        assert_eq!(rexx_compare("-1", "1", 0).unwrap(), Ordering::Less);
    }

    #[test]
    fn test_rexx_value_basics() {
        let v = RexxValue::from_string("Hello");
        assert_eq!(v.as_str(), "Hello");
        assert!(!v.is_number());
        assert_eq!(v.len(), 5);

        let n = RexxValue::from_string("42");
        assert!(n.is_number());

        let b = RexxValue::from_string("1");
        assert_eq!(b.as_bool(), Some(true));
        let b0 = RexxValue::from_string("0");
        assert_eq!(b0.as_bool(), Some(false));
    }

    #[test]
    fn test_exponential_notation() {
        let settings = NumericSettings::default();
        assert_eq!(rexx_add("1E2", "50", &settings).unwrap(), "150");
        assert_eq!(rexx_mul("1.5E1", "2", &settings).unwrap(), "30");
    }

    #[test]
    fn test_not_a_number() {
        let settings = NumericSettings::default();
        assert!(rexx_add("abc", "1", &settings).is_err());
    }
}
