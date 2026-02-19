//! REXX built-in functions — string manipulation, conversion, and bit operations.
//!
//! Implements 36+ standard REXX built-in functions:
//! - String: ABBREV, CENTER/CENTRE, CHANGESTR, COMPARE, COPIES, COUNTSTR,
//!   DELSTR, DELWORD, INSERT, LASTPOS, LEFT, LENGTH, OVERLAY, POS, REVERSE,
//!   RIGHT, SPACE, STRIP, SUBSTR, SUBWORD, TRANSLATE, VERIFY, WORD,
//!   WORDINDEX, WORDLENGTH, WORDPOS, WORDS
//! - Conversion: B2X, C2D, C2X, D2C, D2X, X2B, X2C, X2D
//! - Bit: BITAND, BITOR, BITXOR
//! - Misc: ABS, DATATYPE, MAX, MIN, SIGN, SYMBOL, TRUNC

/// Dispatch a built-in function call. Returns `None` if not a built-in.
pub fn call_builtin(name: &str, args: &[String]) -> Option<Result<String, String>> {
    match name {
        // -- String functions --
        "ABBREV" => Some(fn_abbrev(args)),
        "CENTER" | "CENTRE" => Some(fn_center(args)),
        "CHANGESTR" => Some(fn_changestr(args)),
        "COMPARE" => Some(fn_compare(args)),
        "COPIES" => Some(fn_copies(args)),
        "COUNTSTR" => Some(fn_countstr(args)),
        "DELSTR" => Some(fn_delstr(args)),
        "DELWORD" => Some(fn_delword(args)),
        "INSERT" => Some(fn_insert(args)),
        "LASTPOS" => Some(fn_lastpos(args)),
        "LEFT" => Some(fn_left(args)),
        "LENGTH" => Some(fn_length(args)),
        "OVERLAY" => Some(fn_overlay(args)),
        "POS" => Some(fn_pos(args)),
        "REVERSE" => Some(fn_reverse(args)),
        "RIGHT" => Some(fn_right(args)),
        "SPACE" => Some(fn_space(args)),
        "STRIP" => Some(fn_strip(args)),
        "SUBSTR" => Some(fn_substr(args)),
        "SUBWORD" => Some(fn_subword(args)),
        "TRANSLATE" => Some(fn_translate(args)),
        "VERIFY" => Some(fn_verify(args)),
        "WORD" => Some(fn_word(args)),
        "WORDINDEX" => Some(fn_wordindex(args)),
        "WORDLENGTH" => Some(fn_wordlength(args)),
        "WORDPOS" => Some(fn_wordpos(args)),
        "WORDS" => Some(fn_words(args)),

        // -- Conversion functions --
        "B2X" => Some(fn_b2x(args)),
        "C2D" => Some(fn_c2d(args)),
        "C2X" => Some(fn_c2x(args)),
        "D2C" => Some(fn_d2c(args)),
        "D2X" => Some(fn_d2x(args)),
        "X2B" => Some(fn_x2b(args)),
        "X2C" => Some(fn_x2c(args)),
        "X2D" => Some(fn_x2d(args)),

        // -- Bit functions --
        "BITAND" => Some(fn_bitand(args)),
        "BITOR" => Some(fn_bitor(args)),
        "BITXOR" => Some(fn_bitxor(args)),

        // -- Misc (non-context-dependent) --
        "SIGN" => Some(fn_sign(args)),

        _ => None,
    }
}

// ---------------------------------------------------------------------------
//  Helper
// ---------------------------------------------------------------------------

fn arg(args: &[String], i: usize) -> &str {
    args.get(i).map(|s| s.as_str()).unwrap_or("")
}

fn arg_usize(args: &[String], i: usize, default: usize) -> usize {
    args.get(i)
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(default)
}

// ---------------------------------------------------------------------------
//  String functions
// ---------------------------------------------------------------------------

fn fn_abbrev(args: &[String]) -> Result<String, String> {
    let info = arg(args, 0);
    let info_check = arg(args, 1);
    let len = arg_usize(args, 2, info_check.len());
    let ok = info_check.len() >= len && info.starts_with(&info_check[..len]);
    Ok(if ok { "1" } else { "0" }.into())
}

fn fn_center(args: &[String]) -> Result<String, String> {
    let s = arg(args, 0);
    let n = arg_usize(args, 1, 0);
    let pad = args.get(2).and_then(|a| a.chars().next()).unwrap_or(' ');
    if n == 0 {
        return Ok(String::new());
    }
    if s.len() >= n {
        let start = (s.len() - n) / 2;
        return Ok(s[start..start + n].to_string());
    }
    let total_pad = n - s.len();
    let left_pad = total_pad / 2;
    let right_pad = total_pad - left_pad;
    let mut result = String::new();
    for _ in 0..left_pad {
        result.push(pad);
    }
    result.push_str(s);
    for _ in 0..right_pad {
        result.push(pad);
    }
    Ok(result)
}

fn fn_changestr(args: &[String]) -> Result<String, String> {
    let needle = arg(args, 0);
    let haystack = arg(args, 1);
    let replacement = arg(args, 2);
    if needle.is_empty() {
        return Ok(haystack.to_string());
    }
    Ok(haystack.replace(needle, replacement))
}

fn fn_compare(args: &[String]) -> Result<String, String> {
    let s1 = arg(args, 0);
    let s2 = arg(args, 1);
    let pad = args.get(2).and_then(|a| a.chars().next()).unwrap_or(' ');
    let max_len = s1.len().max(s2.len());
    for i in 0..max_len {
        let c1 = s1.chars().nth(i).unwrap_or(pad);
        let c2 = s2.chars().nth(i).unwrap_or(pad);
        if c1 != c2 {
            return Ok((i + 1).to_string());
        }
    }
    Ok("0".into())
}

fn fn_copies(args: &[String]) -> Result<String, String> {
    let s = arg(args, 0);
    let n = arg_usize(args, 1, 0);
    Ok(s.repeat(n))
}

fn fn_countstr(args: &[String]) -> Result<String, String> {
    let needle = arg(args, 0);
    let haystack = arg(args, 1);
    if needle.is_empty() {
        return Ok("0".into());
    }
    Ok(haystack.matches(needle).count().to_string())
}

fn fn_delstr(args: &[String]) -> Result<String, String> {
    let s = arg(args, 0);
    let start = arg_usize(args, 1, 1);
    let len = args.get(2).and_then(|a| a.parse::<usize>().ok());
    let start_idx = if start > 0 { start - 1 } else { 0 };
    if start_idx >= s.len() {
        return Ok(s.to_string());
    }
    let mut result = s[..start_idx].to_string();
    if let Some(l) = len {
        let end = (start_idx + l).min(s.len());
        result.push_str(&s[end..]);
    }
    Ok(result)
}

fn fn_delword(args: &[String]) -> Result<String, String> {
    let s = arg(args, 0);
    let n = arg_usize(args, 1, 1);
    let count = args.get(2).and_then(|a| a.parse::<usize>().ok());
    let words: Vec<&str> = s.split_whitespace().collect();
    if n == 0 || n > words.len() {
        return Ok(s.to_string());
    }
    let start_word = n - 1;
    let end_word = if let Some(c) = count {
        (start_word + c).min(words.len())
    } else {
        words.len()
    };
    let mut result_words: Vec<&str> = Vec::new();
    for (i, w) in words.iter().enumerate() {
        if i < start_word || i >= end_word {
            result_words.push(w);
        }
    }
    Ok(result_words.join(" "))
}

fn fn_insert(args: &[String]) -> Result<String, String> {
    let new_str = arg(args, 0);
    let target = arg(args, 1);
    let n = arg_usize(args, 2, 0);
    let len = args.get(3).and_then(|a| a.parse::<usize>().ok());
    let pad = args.get(4).and_then(|a| a.chars().next()).unwrap_or(' ');
    let insert_at = n.min(target.len());

    let insert_str = if let Some(l) = len {
        if new_str.len() >= l {
            new_str[..l].to_string()
        } else {
            let mut s = new_str.to_string();
            while s.len() < l {
                s.push(pad);
            }
            s
        }
    } else {
        new_str.to_string()
    };

    let mut result = target[..insert_at].to_string();
    result.push_str(&insert_str);
    result.push_str(&target[insert_at..]);
    Ok(result)
}

fn fn_lastpos(args: &[String]) -> Result<String, String> {
    let needle = arg(args, 0);
    let haystack = arg(args, 1);
    let start = arg_usize(args, 2, haystack.len());
    let search_in = &haystack[..start.min(haystack.len())];
    match search_in.rfind(needle) {
        Some(pos) => Ok((pos + 1).to_string()),
        None => Ok("0".into()),
    }
}

fn fn_left(args: &[String]) -> Result<String, String> {
    let s = arg(args, 0);
    let n = arg_usize(args, 1, 0);
    let pad = args.get(2).and_then(|a| a.chars().next()).unwrap_or(' ');
    if s.len() >= n {
        Ok(s[..n].to_string())
    } else {
        let mut result = s.to_string();
        while result.len() < n {
            result.push(pad);
        }
        Ok(result)
    }
}

fn fn_length(args: &[String]) -> Result<String, String> {
    Ok(arg(args, 0).len().to_string())
}

fn fn_overlay(args: &[String]) -> Result<String, String> {
    let new_str = arg(args, 0);
    let target = arg(args, 1);
    let n = arg_usize(args, 2, 1);
    let len = args.get(3).and_then(|a| a.parse::<usize>().ok()).unwrap_or(new_str.len());
    let pad = args.get(4).and_then(|a| a.chars().next()).unwrap_or(' ');
    let start = if n > 0 { n - 1 } else { 0 };

    // Pad target if needed.
    let mut target_str = target.to_string();
    while target_str.len() < start + len {
        target_str.push(pad);
    }

    // Pad/truncate overlay.
    let overlay = if new_str.len() >= len {
        new_str[..len].to_string()
    } else {
        let mut s = new_str.to_string();
        while s.len() < len {
            s.push(pad);
        }
        s
    };

    let mut result = target_str[..start].to_string();
    result.push_str(&overlay);
    if start + len < target_str.len() {
        result.push_str(&target_str[start + len..]);
    }
    Ok(result)
}

fn fn_pos(args: &[String]) -> Result<String, String> {
    let needle = arg(args, 0);
    let haystack = arg(args, 1);
    let start = arg_usize(args, 2, 1);
    let start_idx = if start > 0 { start - 1 } else { 0 };
    if start_idx >= haystack.len() {
        return Ok("0".into());
    }
    match haystack[start_idx..].find(needle) {
        Some(pos) => Ok((pos + start_idx + 1).to_string()),
        None => Ok("0".into()),
    }
}

fn fn_reverse(args: &[String]) -> Result<String, String> {
    Ok(arg(args, 0).chars().rev().collect())
}

fn fn_right(args: &[String]) -> Result<String, String> {
    let s = arg(args, 0);
    let n = arg_usize(args, 1, 0);
    let pad = args.get(2).and_then(|a| a.chars().next()).unwrap_or(' ');
    if s.len() >= n {
        Ok(s[s.len() - n..].to_string())
    } else {
        let pad_count = n - s.len();
        let mut result = String::new();
        for _ in 0..pad_count {
            result.push(pad);
        }
        result.push_str(s);
        Ok(result)
    }
}

fn fn_space(args: &[String]) -> Result<String, String> {
    let s = arg(args, 0);
    let n = arg_usize(args, 1, 1);
    let pad = args.get(2).and_then(|a| a.chars().next()).unwrap_or(' ');
    let words: Vec<&str> = s.split_whitespace().collect();
    let sep: String = std::iter::repeat(pad).take(n).collect();
    Ok(words.join(&sep))
}

fn fn_strip(args: &[String]) -> Result<String, String> {
    let s = arg(args, 0);
    let option = args.get(1).map(|a| a.to_uppercase()).unwrap_or_else(|| "B".into());
    let ch = args.get(2).and_then(|a| a.chars().next()).unwrap_or(' ');
    let result = match option.as_str() {
        "L" => s.trim_start_matches(ch).to_string(),
        "T" => s.trim_end_matches(ch).to_string(),
        _ => s.trim_matches(ch).to_string(),
    };
    Ok(result)
}

fn fn_substr(args: &[String]) -> Result<String, String> {
    let s = arg(args, 0);
    let start = arg_usize(args, 1, 1);
    let len = args.get(2).and_then(|a| a.parse::<usize>().ok());
    let pad = args.get(3).and_then(|a| a.chars().next()).unwrap_or(' ');
    let start_idx = if start > 0 { start - 1 } else { 0 };
    let result = if let Some(l) = len {
        let end = (start_idx + l).min(s.len());
        let mut sub = if start_idx < s.len() {
            s[start_idx..end].to_string()
        } else {
            String::new()
        };
        while sub.len() < l {
            sub.push(pad);
        }
        sub
    } else if start_idx < s.len() {
        s[start_idx..].to_string()
    } else {
        String::new()
    };
    Ok(result)
}

fn fn_subword(args: &[String]) -> Result<String, String> {
    let s = arg(args, 0);
    let n = arg_usize(args, 1, 1);
    let count = args.get(2).and_then(|a| a.parse::<usize>().ok());
    let words: Vec<&str> = s.split_whitespace().collect();
    let start = if n > 0 { n - 1 } else { 0 };
    if start >= words.len() {
        return Ok(String::new());
    }
    let end = if let Some(c) = count {
        (start + c).min(words.len())
    } else {
        words.len()
    };
    Ok(words[start..end].join(" "))
}

fn fn_translate(args: &[String]) -> Result<String, String> {
    let s = arg(args, 0);
    let output = args.get(1).map(|a| a.as_str());
    let input = args.get(2).map(|a| a.as_str());

    match (output, input) {
        (None, None) => {
            // No tables — uppercase.
            Ok(s.to_uppercase())
        }
        (Some(out_table), Some(in_table)) => {
            let out_chars: Vec<char> = out_table.chars().collect();
            let in_chars: Vec<char> = in_table.chars().collect();
            let pad = args.get(3).and_then(|a| a.chars().next()).unwrap_or(' ');
            let result: String = s
                .chars()
                .map(|c| {
                    if let Some(pos) = in_chars.iter().position(|&ic| ic == c) {
                        out_chars.get(pos).copied().unwrap_or(pad)
                    } else {
                        c
                    }
                })
                .collect();
            Ok(result)
        }
        (Some(out_table), None) => {
            // Output table only — translate to output chars by position.
            let out_chars: Vec<char> = out_table.chars().collect();
            let result: String = s
                .chars()
                .enumerate()
                .map(|(i, c)| out_chars.get(i).copied().unwrap_or(c))
                .collect();
            Ok(result)
        }
        (None, Some(_)) => Ok(s.to_string()),
    }
}

fn fn_verify(args: &[String]) -> Result<String, String> {
    let s = arg(args, 0);
    let reference = arg(args, 1);
    let option = args.get(2).map(|a| a.to_uppercase()).unwrap_or_else(|| "N".into());
    let start = arg_usize(args, 3, 1);
    let start_idx = if start > 0 { start - 1 } else { 0 };

    let nomatch = option != "M"; // "N" or default = find first non-match; "M" = find first match.

    for (i, c) in s.chars().enumerate().skip(start_idx) {
        let in_ref = reference.contains(c);
        if nomatch && !in_ref {
            return Ok((i + 1).to_string());
        }
        if !nomatch && in_ref {
            return Ok((i + 1).to_string());
        }
    }
    Ok("0".into())
}

fn fn_word(args: &[String]) -> Result<String, String> {
    let s = arg(args, 0);
    let n = arg_usize(args, 1, 1);
    let words: Vec<&str> = s.split_whitespace().collect();
    Ok(words.get(n.wrapping_sub(1)).copied().unwrap_or("").to_string())
}

fn fn_wordindex(args: &[String]) -> Result<String, String> {
    let s = arg(args, 0);
    let n = arg_usize(args, 1, 1);
    let mut word_num = 0;
    let mut in_word = false;
    for (i, c) in s.char_indices() {
        if c.is_whitespace() {
            in_word = false;
        } else if !in_word {
            in_word = true;
            word_num += 1;
            if word_num == n {
                return Ok((i + 1).to_string());
            }
        }
    }
    Ok("0".into())
}

fn fn_wordlength(args: &[String]) -> Result<String, String> {
    let s = arg(args, 0);
    let n = arg_usize(args, 1, 1);
    let words: Vec<&str> = s.split_whitespace().collect();
    Ok(words
        .get(n.wrapping_sub(1))
        .map(|w| w.len())
        .unwrap_or(0)
        .to_string())
}

fn fn_wordpos(args: &[String]) -> Result<String, String> {
    let phrase = arg(args, 0);
    let s = arg(args, 1);
    let start = arg_usize(args, 2, 1);
    let s_words: Vec<&str> = s.split_whitespace().collect();
    let p_words: Vec<&str> = phrase.split_whitespace().collect();
    if p_words.is_empty() || start == 0 {
        return Ok("0".into());
    }
    let start_idx = start - 1;
    for i in start_idx..s_words.len() {
        if i + p_words.len() <= s_words.len() {
            let matched = p_words
                .iter()
                .zip(s_words[i..].iter())
                .all(|(a, b)| a.eq_ignore_ascii_case(b));
            if matched {
                return Ok((i + 1).to_string());
            }
        }
    }
    Ok("0".into())
}

fn fn_words(args: &[String]) -> Result<String, String> {
    Ok(arg(args, 0).split_whitespace().count().to_string())
}

// ---------------------------------------------------------------------------
//  Conversion functions
// ---------------------------------------------------------------------------

fn fn_b2x(args: &[String]) -> Result<String, String> {
    let bin = arg(args, 0).replace(' ', "");
    // Pad to multiple of 4.
    let padded = format!("{:0>width$}", bin, width = ((bin.len() + 3) / 4) * 4);
    let mut hex = String::new();
    for chunk in padded.as_bytes().chunks(4) {
        let val = chunk.iter().fold(0u8, |acc, &b| acc * 2 + if b == b'1' { 1 } else { 0 });
        hex.push(char::from_digit(val as u32, 16).unwrap_or('0').to_ascii_uppercase());
    }
    Ok(hex)
}

fn fn_c2d(args: &[String]) -> Result<String, String> {
    let s = arg(args, 0);
    let mut val: u64 = 0;
    for b in s.bytes() {
        val = val * 256 + b as u64;
    }
    Ok(val.to_string())
}

fn fn_c2x(args: &[String]) -> Result<String, String> {
    let s = arg(args, 0);
    use std::fmt::Write;
    let hex = s.bytes().fold(String::new(), |mut acc, b| {
        let _ = write!(acc, "{b:02X}");
        acc
    });
    Ok(hex)
}

fn fn_d2c(args: &[String]) -> Result<String, String> {
    let d = arg(args, 0).parse::<u64>().unwrap_or(0);
    if d <= 0xFF {
        Ok(String::from(d as u8 as char))
    } else {
        let mut bytes = Vec::new();
        let mut val = d;
        while val > 0 {
            bytes.push((val & 0xFF) as u8);
            val >>= 8;
        }
        bytes.reverse();
        Ok(bytes.iter().map(|&b| b as char).collect())
    }
}

fn fn_d2x(args: &[String]) -> Result<String, String> {
    let d = arg(args, 0).parse::<u64>().unwrap_or(0);
    let n = args.get(1).and_then(|a| a.parse::<usize>().ok());
    let hex = format!("{d:X}");
    if let Some(width) = n {
        Ok(format!("{hex:0>width$}"))
    } else {
        Ok(hex)
    }
}

fn fn_x2b(args: &[String]) -> Result<String, String> {
    let hex = arg(args, 0).replace(' ', "");
    let mut bin = String::new();
    for c in hex.chars() {
        let val = c.to_digit(16).unwrap_or(0);
        bin.push_str(&format!("{val:04b}"));
    }
    Ok(bin)
}

fn fn_x2c(args: &[String]) -> Result<String, String> {
    let hex = arg(args, 0).replace(' ', "");
    let padded = if hex.len() % 2 != 0 {
        format!("0{hex}")
    } else {
        hex
    };
    let mut result = String::new();
    let bytes: Vec<char> = padded.chars().collect();
    let mut i = 0;
    while i + 1 < bytes.len() {
        let hi = bytes[i].to_digit(16).unwrap_or(0);
        let lo = bytes[i + 1].to_digit(16).unwrap_or(0);
        result.push((hi * 16 + lo) as u8 as char);
        i += 2;
    }
    Ok(result)
}

fn fn_x2d(args: &[String]) -> Result<String, String> {
    let hex = arg(args, 0).replace(' ', "");
    let val = u64::from_str_radix(&hex, 16).unwrap_or(0);
    Ok(val.to_string())
}

// ---------------------------------------------------------------------------
//  Bit functions
// ---------------------------------------------------------------------------

fn fn_bitand(args: &[String]) -> Result<String, String> {
    bitop(args, |a, b| a & b)
}

fn fn_bitor(args: &[String]) -> Result<String, String> {
    bitop(args, |a, b| a | b)
}

fn fn_bitxor(args: &[String]) -> Result<String, String> {
    bitop(args, |a, b| a ^ b)
}

fn bitop(args: &[String], op: impl Fn(u8, u8) -> u8) -> Result<String, String> {
    let s1 = arg(args, 0);
    let s2 = arg(args, 1);
    let pad = args.get(2).and_then(|a| a.bytes().next()).unwrap_or(0xFF);
    let max_len = s1.len().max(s2.len());
    let mut result = String::new();
    for i in 0..max_len {
        let b1 = s1.as_bytes().get(i).copied().unwrap_or(pad);
        let b2 = s2.as_bytes().get(i).copied().unwrap_or(pad);
        result.push(op(b1, b2) as char);
    }
    Ok(result)
}

// ---------------------------------------------------------------------------
//  Misc functions
// ---------------------------------------------------------------------------

fn fn_sign(args: &[String]) -> Result<String, String> {
    let s = arg(args, 0);
    use crate::value::rexx_compare;
    match rexx_compare(s, "0", 0) {
        Ok(std::cmp::Ordering::Greater) => Ok("1".into()),
        Ok(std::cmp::Ordering::Less) => Ok("-1".into()),
        Ok(std::cmp::Ordering::Equal) => Ok("0".into()),
        Err(e) => Err(e),
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn call(name: &str, args: &[&str]) -> String {
        let a: Vec<String> = args.iter().map(|s| s.to_string()).collect();
        call_builtin(name, &a).unwrap().unwrap()
    }

    // -- String tests --

    #[test]
    fn test_abbrev() {
        assert_eq!(call("ABBREV", &["INFORMATION", "INFO"]), "1");
        assert_eq!(call("ABBREV", &["INFORMATION", "INF", "4"]), "0");
    }

    #[test]
    fn test_center() {
        assert_eq!(call("CENTER", &["abc", "7"]), "  abc  ");
        assert_eq!(call("CENTRE", &["abc", "7", "-"]), "--abc--");
    }

    #[test]
    fn test_changestr() {
        assert_eq!(call("CHANGESTR", &["a", "abcabc", "X"]), "XbcXbc");
    }

    #[test]
    fn test_compare() {
        assert_eq!(call("COMPARE", &["abc", "abc"]), "0");
        assert_eq!(call("COMPARE", &["abc", "axc"]), "2");
    }

    #[test]
    fn test_copies() {
        assert_eq!(call("COPIES", &["Ab", "3"]), "AbAbAb");
    }

    #[test]
    fn test_countstr() {
        assert_eq!(call("COUNTSTR", &["ab", "abcabcab"]), "3");
    }

    #[test]
    fn test_delstr() {
        assert_eq!(call("DELSTR", &["abcdef", "3", "2"]), "abef");
        assert_eq!(call("DELSTR", &["abcdef", "3"]), "ab");
    }

    #[test]
    fn test_delword() {
        assert_eq!(call("DELWORD", &["one two three four", "2", "2"]), "one four");
    }

    #[test]
    fn test_insert() {
        assert_eq!(call("INSERT", &["abc", "XYZ", "2"]), "XYabcZ");
    }

    #[test]
    fn test_lastpos() {
        assert_eq!(call("LASTPOS", &["a", "abcabc"]), "4");
    }

    #[test]
    fn test_left() {
        assert_eq!(call("LEFT", &["hello", "3"]), "hel");
        assert_eq!(call("LEFT", &["hi", "5"]), "hi   ");
    }

    #[test]
    fn test_length() {
        assert_eq!(call("LENGTH", &["hello"]), "5");
    }

    #[test]
    fn test_overlay() {
        // OVERLAY('new','old stuff',5) replaces positions 5-7 ('stu') with 'new'.
        assert_eq!(call("OVERLAY", &["new", "old stuff", "5"]), "old newff");
    }

    #[test]
    fn test_pos() {
        assert_eq!(call("POS", &["c", "abcdef"]), "3");
        assert_eq!(call("POS", &["z", "abcdef"]), "0");
    }

    #[test]
    fn test_reverse() {
        assert_eq!(call("REVERSE", &["Hello"]), "olleH");
    }

    #[test]
    fn test_right() {
        assert_eq!(call("RIGHT", &["hello", "3"]), "llo");
        assert_eq!(call("RIGHT", &["hi", "5"]), "   hi");
    }

    #[test]
    fn test_space() {
        assert_eq!(call("SPACE", &["  a  b  c  ", "1"]), "a b c");
        assert_eq!(call("SPACE", &["a b c", "0"]), "abc");
    }

    #[test]
    fn test_strip() {
        assert_eq!(call("STRIP", &["  hello  "]), "hello");
        assert_eq!(call("STRIP", &["  hello  ", "L"]), "hello  ");
    }

    #[test]
    fn test_substr() {
        assert_eq!(call("SUBSTR", &["Hello World", "7", "5"]), "World");
        assert_eq!(call("SUBSTR", &["Hi", "1", "5"]), "Hi   ");
    }

    #[test]
    fn test_subword() {
        assert_eq!(call("SUBWORD", &["one two three four", "2", "2"]), "two three");
    }

    #[test]
    fn test_translate() {
        assert_eq!(call("TRANSLATE", &["abc"]), "ABC");
        assert_eq!(call("TRANSLATE", &["abcdef", "12", "ac"]), "1b2def");
    }

    #[test]
    fn test_verify() {
        assert_eq!(call("VERIFY", &["123abc", "0123456789"]), "4");
        assert_eq!(call("VERIFY", &["12345", "0123456789"]), "0");
    }

    #[test]
    fn test_word() {
        assert_eq!(call("WORD", &["the quick brown fox", "3"]), "brown");
    }

    #[test]
    fn test_wordindex() {
        assert_eq!(call("WORDINDEX", &["the quick brown fox", "3"]), "11");
    }

    #[test]
    fn test_wordlength() {
        assert_eq!(call("WORDLENGTH", &["the quick brown fox", "2"]), "5");
    }

    #[test]
    fn test_wordpos() {
        assert_eq!(call("WORDPOS", &["fox", "the quick brown fox"]), "4");
        assert_eq!(call("WORDPOS", &["cat", "the quick brown fox"]), "0");
    }

    #[test]
    fn test_words() {
        assert_eq!(call("WORDS", &["one two three"]), "3");
    }

    // -- Conversion tests --

    #[test]
    fn test_c2x() {
        assert_eq!(call("C2X", &["AB"]), "4142");
    }

    #[test]
    fn test_x2d() {
        assert_eq!(call("X2D", &["FF"]), "255");
    }

    #[test]
    fn test_d2x() {
        assert_eq!(call("D2X", &["255"]), "FF");
        assert_eq!(call("D2X", &["255", "4"]), "00FF");
    }

    #[test]
    fn test_x2c() {
        assert_eq!(call("X2C", &["4142"]), "AB");
    }

    #[test]
    fn test_b2x() {
        assert_eq!(call("B2X", &["11110000"]), "F0");
    }

    #[test]
    fn test_x2b() {
        assert_eq!(call("X2B", &["F0"]), "11110000");
    }

    #[test]
    fn test_c2d() {
        assert_eq!(call("C2D", &["A"]), "65");
    }

    #[test]
    fn test_d2c() {
        assert_eq!(call("D2C", &["65"]), "A");
    }

    // -- Bit tests --

    #[test]
    fn test_bitand() {
        // 'a' (0x61) AND 'A' (0x41) = 'A' (0x41)
        assert_eq!(call("BITAND", &["a", "A"]), "A");
    }

    #[test]
    fn test_bitor() {
        // 'A' (0x41) OR ' ' (0x20) = 'a' (0x61)
        assert_eq!(call("BITOR", &["A", " "]), "a");
    }

    #[test]
    fn test_bitxor() {
        assert_eq!(call("BITXOR", &["A", " "]), "a");
    }

    // -- Misc --

    #[test]
    fn test_sign() {
        assert_eq!(call("SIGN", &["42"]), "1");
        assert_eq!(call("SIGN", &["-5"]), "-1");
        assert_eq!(call("SIGN", &["0"]), "0");
    }
}
