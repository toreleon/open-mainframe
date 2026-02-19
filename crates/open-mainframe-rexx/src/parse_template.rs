//! REXX PARSE instruction — template-based string parsing.
//!
//! REXX PARSE splits a source string into variables using a template.
//! Templates support:
//! - **Word parsing**: `PARSE VALUE str WITH a b c` (split on whitespace)
//! - **Literal patterns**: `PARSE VALUE str WITH a ',' b` (split on delimiter)
//! - **Absolute positions**: `PARSE VALUE str WITH a 5 b` (column-based)
//! - **Relative positions**: `PARSE VALUE str WITH a +3 b` (offset from current)
//! - **Variable patterns**: `PARSE VALUE str WITH a (delim) b`

use std::collections::HashMap;

/// A single element in a PARSE template.
#[derive(Debug, Clone, PartialEq)]
pub enum TemplateElement {
    /// Variable name to receive the parsed value.
    Variable(String),
    /// Dot placeholder — discard parsed value.
    Dot,
    /// Literal string delimiter.
    Literal(String),
    /// Absolute column position (1-based).
    AbsolutePos(usize),
    /// Relative positive offset.
    RelativePos(i64),
    /// Variable reference for pattern: `(varname)`.
    VarPattern(String),
}

/// Parse a template string into a list of elements.
///
/// Template syntax examples:
/// - `first last age` — three word variables
/// - `name '=' value` — literal delimiter
/// - `a 5 b 10 c` — absolute positions
/// - `a +3 b` — relative position
/// - `a (delim) b` — variable pattern
pub fn parse_template(template: &str) -> Vec<TemplateElement> {
    let mut elements = Vec::new();
    let mut chars = template.chars().peekable();

    while chars.peek().is_some() {
        // Skip whitespace.
        while chars.peek() == Some(&' ') {
            chars.next();
        }
        if chars.peek().is_none() {
            break;
        }

        match chars.peek() {
            // Quoted literal delimiter.
            Some(&'\'' | &'"') => {
                let quote = chars.next().unwrap();
                let mut lit = String::new();
                loop {
                    match chars.next() {
                        Some(c) if c == quote => {
                            // Check for doubled quote.
                            if chars.peek() == Some(&quote) {
                                chars.next();
                                lit.push(quote);
                            } else {
                                break;
                            }
                        }
                        Some(c) => lit.push(c),
                        None => break,
                    }
                }
                elements.push(TemplateElement::Literal(lit));
            }
            // Variable pattern: (varname).
            Some(&'(') => {
                chars.next(); // consume '('
                let mut name = String::new();
                while let Some(&c) = chars.peek() {
                    if c == ')' {
                        chars.next();
                        break;
                    }
                    name.push(c);
                    chars.next();
                }
                elements.push(TemplateElement::VarPattern(name.trim().to_uppercase()));
            }
            // Numeric position (absolute or relative).
            Some(&c) if c == '+' || c == '-' || c.is_ascii_digit() => {
                let mut num_str = String::new();
                if c == '+' || c == '-' {
                    num_str.push(c);
                    chars.next();
                }
                while let Some(&d) = chars.peek() {
                    if d.is_ascii_digit() {
                        num_str.push(d);
                        chars.next();
                    } else {
                        break;
                    }
                }
                if num_str.starts_with('+') || num_str.starts_with('-') {
                    if let Ok(offset) = num_str.parse::<i64>() {
                        elements.push(TemplateElement::RelativePos(offset));
                    }
                } else if let Ok(pos) = num_str.parse::<usize>() {
                    elements.push(TemplateElement::AbsolutePos(pos));
                }
            }
            // Dot placeholder.
            Some(&'.') => {
                chars.next();
                elements.push(TemplateElement::Dot);
            }
            // Variable name.
            Some(_) => {
                let mut name = String::new();
                while let Some(&c) = chars.peek() {
                    if c == ' ' || c == '\'' || c == '"' || c == '(' {
                        break;
                    }
                    name.push(c);
                    chars.next();
                }
                if !name.is_empty() {
                    elements.push(TemplateElement::Variable(name.to_uppercase()));
                }
            }
            None => break,
        }
    }

    elements
}

/// Execute a PARSE template against a source string.
///
/// Returns a map of variable name → value.
/// `var_resolver` is used to resolve `(varname)` patterns.
pub fn execute_parse(
    source: &str,
    template: &[TemplateElement],
    upper: bool,
    var_resolver: &dyn Fn(&str) -> String,
) -> HashMap<String, String> {
    let input = if upper {
        source.to_uppercase()
    } else {
        source.to_string()
    };

    let mut result = HashMap::new();
    let mut pos: usize = 0; // 0-based cursor position in input.

    // Group template elements into (target, delimiter) pairs.
    // Each variable/dot is followed by an optional delimiter (literal, position, var pattern).
    // The last variable gets the remainder.

    let mut i = 0;
    while i < template.len() {
        // Find the target (variable or dot).
        let target = match &template[i] {
            TemplateElement::Variable(name) => {
                i += 1;
                Some(name.clone())
            }
            TemplateElement::Dot => {
                i += 1;
                None // Discard.
            }
            // If we hit a delimiter without a preceding variable, it just moves the cursor.
            TemplateElement::Literal(_)
            | TemplateElement::AbsolutePos(_)
            | TemplateElement::RelativePos(_)
            | TemplateElement::VarPattern(_) => {
                // Apply the delimiter to move the cursor.
                apply_delimiter(&template[i], &input, &mut pos, var_resolver);
                i += 1;
                continue;
            }
        };

        // Check if there's a delimiter next.
        if i < template.len() {
            match &template[i] {
                TemplateElement::Literal(lit) => {
                    // Find the literal in the remaining string.
                    let remaining = &input[pos..];
                    if let Some(found) = remaining.find(lit.as_str()) {
                        let value = remaining[..found].to_string();
                        if let Some(name) = target {
                            result.insert(name, value);
                        }
                        pos += found + lit.len();
                    } else {
                        // Literal not found — variable gets remainder.
                        let value = remaining.to_string();
                        if let Some(name) = target {
                            result.insert(name, value);
                        }
                        pos = input.len();
                    }
                    i += 1;
                }
                TemplateElement::AbsolutePos(abs_pos) => {
                    let target_pos = if *abs_pos > 0 { abs_pos - 1 } else { 0 };
                    let end = target_pos.min(input.len());
                    let start = pos.min(end);
                    let value = if start <= end {
                        input[start..end].to_string()
                    } else {
                        // Backwards: REXX allows moving cursor backwards.
                        String::new()
                    };
                    if let Some(name) = target {
                        result.insert(name, value);
                    }
                    pos = target_pos.min(input.len());
                    i += 1;
                }
                TemplateElement::RelativePos(offset) => {
                    let new_pos = (pos as i64 + offset).max(0) as usize;
                    let end = new_pos.min(input.len());
                    let value = if pos < end {
                        input[pos..end].to_string()
                    } else {
                        String::new()
                    };
                    if let Some(name) = target {
                        result.insert(name, value);
                    }
                    pos = end;
                    i += 1;
                }
                TemplateElement::VarPattern(vname) => {
                    let pattern = var_resolver(vname);
                    let remaining = &input[pos..];
                    if let Some(found) = remaining.find(pattern.as_str()) {
                        let value = remaining[..found].to_string();
                        if let Some(name) = target {
                            result.insert(name, value);
                        }
                        pos += found + pattern.len();
                    } else {
                        let value = remaining.to_string();
                        if let Some(name) = target {
                            result.insert(name, value);
                        }
                        pos = input.len();
                    }
                    i += 1;
                }
                // Next element is another variable — word parse.
                TemplateElement::Variable(_) | TemplateElement::Dot => {
                    // Word parse: take next whitespace-delimited word.
                    let remaining = &input[pos..];
                    let trimmed = remaining.trim_start();
                    let skipped = remaining.len() - trimmed.len();
                    if let Some(space_pos) = trimmed.find(' ') {
                        let value = trimmed[..space_pos].to_string();
                        if let Some(name) = target {
                            result.insert(name, value);
                        }
                        pos += skipped + space_pos + 1;
                    } else {
                        // Last word — take everything.
                        let value = trimmed.to_string();
                        if let Some(name) = target {
                            result.insert(name, value);
                        }
                        pos = input.len();
                    }
                }
            }
        } else {
            // Last variable — gets remainder (stripped of leading spaces).
            let remaining = &input[pos..];
            let value = remaining.trim_start().to_string();
            if let Some(name) = target {
                result.insert(name, value);
            }
            pos = input.len();
        }
    }

    result
}

fn apply_delimiter(
    element: &TemplateElement,
    input: &str,
    pos: &mut usize,
    var_resolver: &dyn Fn(&str) -> String,
) {
    match element {
        TemplateElement::Literal(lit) => {
            let remaining = &input[*pos..];
            if let Some(found) = remaining.find(lit.as_str()) {
                *pos += found + lit.len();
            }
        }
        TemplateElement::AbsolutePos(abs_pos) => {
            *pos = if *abs_pos > 0 { abs_pos - 1 } else { 0 };
            *pos = (*pos).min(input.len());
        }
        TemplateElement::RelativePos(offset) => {
            let new_pos = (*pos as i64 + offset).max(0) as usize;
            *pos = new_pos.min(input.len());
        }
        TemplateElement::VarPattern(vname) => {
            let pattern = var_resolver(vname);
            let remaining = &input[*pos..];
            if let Some(found) = remaining.find(pattern.as_str()) {
                *pos += found + pattern.len();
            }
        }
        _ => {}
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn no_vars(_name: &str) -> String {
        _name.to_string()
    }

    #[test]
    fn test_word_parse() {
        let template = parse_template("first last age");
        let result = execute_parse("John Smith 42", &template, false, &no_vars);
        assert_eq!(result.get("FIRST").unwrap(), "John");
        assert_eq!(result.get("LAST").unwrap(), "Smith");
        assert_eq!(result.get("AGE").unwrap(), "42");
    }

    #[test]
    fn test_word_parse_remainder() {
        let template = parse_template("x y z");
        let result = execute_parse("a b c d e", &template, false, &no_vars);
        assert_eq!(result.get("X").unwrap(), "a");
        assert_eq!(result.get("Y").unwrap(), "b");
        assert_eq!(result.get("Z").unwrap(), "c d e");
    }

    #[test]
    fn test_literal_delimiter() {
        let template = parse_template("name '=' val");
        let result = execute_parse("key=value", &template, false, &no_vars);
        assert_eq!(result.get("NAME").unwrap(), "key");
        assert_eq!(result.get("VAL").unwrap(), "value");
    }

    #[test]
    fn test_literal_delimiter_dash() {
        let template = parse_template("year '-' month '-' day");
        let result = execute_parse("2025-12-31", &template, false, &no_vars);
        assert_eq!(result.get("YEAR").unwrap(), "2025");
        assert_eq!(result.get("MONTH").unwrap(), "12");
        assert_eq!(result.get("DAY").unwrap(), "31");
    }

    #[test]
    fn test_absolute_position() {
        let template = parse_template("a 5 b");
        let result = execute_parse("ABCDEFGH", &template, false, &no_vars);
        assert_eq!(result.get("A").unwrap(), "ABCD");
        assert_eq!(result.get("B").unwrap(), "EFGH");
    }

    #[test]
    fn test_relative_position() {
        let template = parse_template("a +3 b");
        let result = execute_parse("ABCDEFGH", &template, false, &no_vars);
        assert_eq!(result.get("A").unwrap(), "ABC");
        assert_eq!(result.get("B").unwrap(), "DEFGH");
    }

    #[test]
    fn test_dot_placeholder() {
        let template = parse_template(". name");
        let result = execute_parse("skip this keepme", &template, false, &no_vars);
        assert!(!result.contains_key("."));
        assert_eq!(result.get("NAME").unwrap(), "this keepme");
    }

    #[test]
    fn test_upper() {
        let template = parse_template("name");
        let result = execute_parse("hello", &template, true, &no_vars);
        assert_eq!(result.get("NAME").unwrap(), "HELLO");
    }

    #[test]
    fn test_var_pattern() {
        let resolver = |name: &str| -> String {
            if name == "DELIM" {
                ",".to_string()
            } else {
                name.to_string()
            }
        };
        let template = parse_template("a (DELIM) b");
        let result = execute_parse("hello,world", &template, false, &resolver);
        assert_eq!(result.get("A").unwrap(), "hello");
        assert_eq!(result.get("B").unwrap(), "world");
    }

    #[test]
    fn test_empty_source() {
        let template = parse_template("a b c");
        let result = execute_parse("", &template, false, &no_vars);
        assert_eq!(result.get("A").unwrap(), "");
        assert_eq!(result.get("B").unwrap(), "");
        assert_eq!(result.get("C").unwrap(), "");
    }

    #[test]
    fn test_mixed_template() {
        // Combination of literal and word parsing.
        let template = parse_template("cmd '/' path rest");
        let result = execute_parse("GET /api/users extra data", &template, false, &no_vars);
        assert_eq!(result.get("CMD").unwrap(), "GET ");
        // After '/' match, remaining is "api/users extra data".
        // Word parse takes "api/users" (up to next space).
        assert_eq!(result.get("PATH").unwrap(), "api/users");
        assert_eq!(result.get("REST").unwrap(), "extra data");
    }

    #[test]
    fn test_parse_template_parsing() {
        let elems = parse_template("a 5 b '-' c +3 d");
        assert_eq!(elems.len(), 7);
        assert_eq!(elems[0], TemplateElement::Variable("A".into()));
        assert_eq!(elems[1], TemplateElement::AbsolutePos(5));
        assert_eq!(elems[2], TemplateElement::Variable("B".into()));
        assert_eq!(elems[3], TemplateElement::Literal("-".into()));
        assert_eq!(elems[4], TemplateElement::Variable("C".into()));
        assert_eq!(elems[5], TemplateElement::RelativePos(3));
        assert_eq!(elems[6], TemplateElement::Variable("D".into()));
    }
}
