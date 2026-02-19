//! TSO command parser — keyword/positional parameter extraction.
//!
//! TSO commands follow the general form:
//! ```text
//! COMMAND positional('quoted') KEYWORD(value) FLAG
//! ```
//!
//! - Positional parameters appear first (before any keywords).
//! - Quoted strings preserve case; unquoted tokens are uppercased.
//! - Keywords are `NAME(value)` pairs.
//! - Flags are single tokens with no parenthesized value.

use std::collections::HashMap;

/// A parsed TSO command.
#[derive(Debug, Clone)]
pub struct ParsedCommand {
    /// Command name (uppercased).
    pub name: String,
    /// Positional parameters (in order).
    pub positional: Vec<String>,
    /// Keyword parameters (keyword → value).
    pub keywords: HashMap<String, String>,
    /// Boolean flags (set of uppercased flag names).
    pub flags: Vec<String>,
}

impl ParsedCommand {
    /// Check if a flag is present.
    pub fn has_flag(&self, flag: &str) -> bool {
        let upper = flag.to_ascii_uppercase();
        self.flags.iter().any(|f| f == &upper)
    }

    /// Get a keyword value by name (case-insensitive).
    pub fn keyword(&self, key: &str) -> Option<&str> {
        let upper = key.to_ascii_uppercase();
        self.keywords.get(&upper).map(|s| s.as_str())
    }

    /// Get the first positional parameter.
    pub fn first_positional(&self) -> Option<&str> {
        self.positional.first().map(|s| s.as_str())
    }
}

/// Parse a TSO command string into its components.
///
/// # Examples
///
/// ```
/// use open_mainframe_tso::parser::parse_command;
///
/// let cmd = parse_command("ALLOC DA('PROD.DATA') FILE(INFILE) SHR REUSE");
/// assert_eq!(cmd.name, "ALLOC");
/// assert_eq!(cmd.keyword("DA"), Some("PROD.DATA"));
/// assert_eq!(cmd.keyword("FILE"), Some("INFILE"));
/// assert!(cmd.has_flag("SHR"));
/// assert!(cmd.has_flag("REUSE"));
/// ```
pub fn parse_command(input: &str) -> ParsedCommand {
    let input = input.trim();
    let tokens = tokenize(input);

    let mut name = String::new();
    let mut positional = Vec::new();
    let mut keywords = HashMap::new();
    let mut flags = Vec::new();
    let mut past_name = false;

    for token in &tokens {
        if !past_name {
            name = token.to_ascii_uppercase();
            past_name = true;
            continue;
        }

        // Check if this token is a keyword (contains parentheses)
        if let Some(eq_pos) = token.find('(') {
            let key = token[..eq_pos].to_ascii_uppercase();
            let val_part = &token[eq_pos + 1..];
            let val = val_part.trim_end_matches(')');
            // Strip quotes from value
            let val = strip_quotes(val);
            keywords.insert(key, val);
        } else if token.starts_with('\'') || token.starts_with('"') {
            // Quoted positional parameter
            positional.push(strip_quotes(token));
        } else {
            // Unquoted token — could be positional or flag.
            let upper = token.to_ascii_uppercase();
            let all_alpha = upper.chars().all(|c| c.is_ascii_alphabetic());

            if all_alpha && is_common_flag(&upper) {
                // Known flag — always a flag
                flags.push(upper);
            } else if all_alpha && upper.starts_with("NO") && upper.len() > 2 {
                // NO-prefix toggle — always a flag
                flags.push(upper);
            } else if positional.is_empty() && keywords.is_empty() && flags.is_empty() {
                // First token after command name — treat as positional
                positional.push(if token.starts_with('\'') || token.starts_with('"') {
                    strip_quotes(token)
                } else {
                    upper
                });
            } else if all_alpha {
                // Subsequent all-alpha token — flag
                flags.push(upper);
            } else {
                // Non-alpha token — positional
                positional.push(strip_quotes(token));
            }
        }
    }

    ParsedCommand {
        name,
        positional,
        keywords,
        flags,
    }
}

/// Tokenize a TSO command line, respecting quotes and parentheses.
fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut current = String::new();
    let mut in_quote = false;
    let mut quote_char = ' ';
    let mut paren_depth = 0;

    for c in input.chars() {
        if in_quote {
            current.push(c);
            if c == quote_char {
                in_quote = false;
            }
        } else {
            match c {
                '\'' | '"' => {
                    in_quote = true;
                    quote_char = c;
                    current.push(c);
                }
                '(' => {
                    paren_depth += 1;
                    current.push(c);
                }
                ')' => {
                    paren_depth -= 1;
                    current.push(c);
                }
                ' ' | '\t' if paren_depth == 0 => {
                    if !current.is_empty() {
                        tokens.push(current.clone());
                        current.clear();
                    }
                }
                _ => {
                    current.push(c);
                }
            }
        }
    }
    if !current.is_empty() {
        tokens.push(current);
    }
    tokens
}

/// Check if a token is a common TSO flag (disposition, options, etc.).
fn is_common_flag(s: &str) -> bool {
    matches!(
        s,
        "SHR" | "OLD" | "NEW" | "MOD"
            | "REUSE" | "TRACKS" | "CYLINDERS"
            | "MEMBERS" | "STATUS" | "HISTORY"
            | "MSGID" | "WTPMSG" | "INTERCOM" | "RECOVER"
            | "ACTIVATE" | "DEACTIVATE" | "DISPLAY"
    )
}

/// Strip surrounding single or double quotes from a string.
fn strip_quotes(s: &str) -> String {
    let s = s.trim();
    if (s.starts_with('\'') && s.ends_with('\'')) || (s.starts_with('"') && s.ends_with('"')) {
        s[1..s.len() - 1].to_string()
    } else {
        s.to_string()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_alloc() {
        let cmd = parse_command("ALLOC DA('PROD.DATA') FILE(INFILE) SHR REUSE");
        assert_eq!(cmd.name, "ALLOC");
        assert_eq!(cmd.keyword("DA"), Some("PROD.DATA"));
        assert_eq!(cmd.keyword("FILE"), Some("INFILE"));
        assert!(cmd.has_flag("SHR"));
        assert!(cmd.has_flag("REUSE"));
    }

    #[test]
    fn test_parse_listds() {
        let cmd = parse_command("LISTDS 'SYS1.PARMLIB' MEMBERS STATUS");
        assert_eq!(cmd.name, "LISTDS");
        assert_eq!(cmd.first_positional(), Some("SYS1.PARMLIB"));
        assert!(cmd.has_flag("MEMBERS"));
        assert!(cmd.has_flag("STATUS"));
    }

    #[test]
    fn test_parse_profile() {
        let cmd = parse_command("PROFILE PREFIX(USER01) MSGID WTPMSG");
        assert_eq!(cmd.name, "PROFILE");
        assert_eq!(cmd.keyword("PREFIX"), Some("USER01"));
        assert!(cmd.has_flag("MSGID"));
        assert!(cmd.has_flag("WTPMSG"));
    }

    #[test]
    fn test_parse_noprefix() {
        let cmd = parse_command("PROFILE NOPREFIX NOMSGID");
        assert_eq!(cmd.name, "PROFILE");
        assert!(cmd.has_flag("NOPREFIX"));
        assert!(cmd.has_flag("NOMSGID"));
    }

    #[test]
    fn test_parse_delete() {
        let cmd = parse_command("DELETE 'USER01.TEMP.DATA'");
        assert_eq!(cmd.name, "DELETE");
        assert_eq!(cmd.first_positional(), Some("USER01.TEMP.DATA"));
    }

    #[test]
    fn test_parse_rename() {
        let cmd = parse_command("RENAME 'USER01.OLD.DATA' 'USER01.NEW.DATA'");
        assert_eq!(cmd.name, "RENAME");
        assert_eq!(cmd.positional.len(), 2);
        assert_eq!(cmd.positional[0], "USER01.OLD.DATA");
        assert_eq!(cmd.positional[1], "USER01.NEW.DATA");
    }

    #[test]
    fn test_parse_alloc_new() {
        let cmd = parse_command(
            "ALLOC DA('USER01.NEW.DATA') FILE(OUTFILE) NEW SPACE(5,5) TRACKS LRECL(80) RECFM(F,B) BLKSIZE(3120)",
        );
        assert_eq!(cmd.name, "ALLOC");
        assert_eq!(cmd.keyword("DA"), Some("USER01.NEW.DATA"));
        assert_eq!(cmd.keyword("FILE"), Some("OUTFILE"));
        assert!(cmd.has_flag("NEW"));
        assert!(cmd.has_flag("TRACKS"));
        assert_eq!(cmd.keyword("SPACE"), Some("5,5"));
        assert_eq!(cmd.keyword("LRECL"), Some("80"));
        assert_eq!(cmd.keyword("RECFM"), Some("F,B"));
        assert_eq!(cmd.keyword("BLKSIZE"), Some("3120"));
    }

    #[test]
    fn test_parse_free() {
        let cmd = parse_command("FREE FILE(INFILE)");
        assert_eq!(cmd.name, "FREE");
        assert_eq!(cmd.keyword("FILE"), Some("INFILE"));
    }

    #[test]
    fn test_parse_help() {
        let cmd = parse_command("HELP ALLOCATE");
        assert_eq!(cmd.name, "HELP");
        assert_eq!(cmd.first_positional(), Some("ALLOCATE"));
    }

    #[test]
    fn test_parse_altlib() {
        let cmd = parse_command("ALTLIB ACTIVATE APPLICATION(EXEC) DA('USER01.MY.EXEC')");
        assert_eq!(cmd.name, "ALTLIB");
        assert_eq!(cmd.keyword("APPLICATION"), Some("EXEC"));
        assert_eq!(cmd.keyword("DA"), Some("USER01.MY.EXEC"));
    }

    #[test]
    fn test_case_insensitive_keyword_lookup() {
        let cmd = parse_command("ALLOC da('test.data') file(dd1)");
        assert_eq!(cmd.keyword("DA"), Some("test.data"));
        assert_eq!(cmd.keyword("FILE"), Some("dd1"));
    }

    #[test]
    fn test_empty_input() {
        let cmd = parse_command("");
        assert_eq!(cmd.name, "");
    }

    #[test]
    fn test_tokenize_quotes_preserved() {
        let tokens = tokenize("LISTDS 'SYS1.PARMLIB' MEMBERS");
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0], "LISTDS");
        assert_eq!(tokens[1], "'SYS1.PARMLIB'");
        assert_eq!(tokens[2], "MEMBERS");
    }

    #[test]
    fn test_tokenize_nested_parens() {
        let tokens = tokenize("ALLOC SPACE(5,5) TRACKS");
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[1], "SPACE(5,5)");
    }
}
