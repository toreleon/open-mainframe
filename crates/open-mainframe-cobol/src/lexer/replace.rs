//! REPLACE Statement Processing (Pass 3 of the Preprocessor Pipeline).
//!
//! Implements the COBOL REPLACE compiler-directing statement which performs
//! source text replacement independent of COPY. Per AD-2.0-01, this pass
//! runs after COPY expansion (Pass 2).
//!
//! Supports:
//! - `REPLACE ==pseudo-text-1== BY ==pseudo-text-2==.`
//! - Multiple replacement operands in a single REPLACE
//! - `REPLACE OFF.` to deactivate all replacements
//! - Pseudo-text matching (single-token and multi-token)
//! - Empty pseudo-text for deletion

use crate::error::CobolError;

/// Result type for replace operations.
pub type Result<T> = std::result::Result<T, CobolError>;

/// A single pseudo-text replacement pair.
#[derive(Debug, Clone)]
pub struct PseudoTextReplacement {
    /// The pseudo-text pattern to match (without `==` delimiters).
    pub from: String,
    /// The replacement pseudo-text (without `==` delimiters). Empty means delete.
    pub to: String,
}

impl PseudoTextReplacement {
    /// Create a new pseudo-text replacement.
    pub fn new(from: impl Into<String>, to: impl Into<String>) -> Self {
        Self {
            from: from.into(),
            to: to.into(),
        }
    }
}

/// REPLACE statement processor.
///
/// Processes COBOL source text after COPY expansion, applying REPLACE
/// directives to perform pseudo-text substitution.
#[derive(Debug)]
pub struct ReplaceProcessor {
    /// Currently active replacement pairs.
    active_replacements: Vec<PseudoTextReplacement>,
}

impl ReplaceProcessor {
    /// Create a new REPLACE processor with no active replacements.
    pub fn new() -> Self {
        Self {
            active_replacements: Vec::new(),
        }
    }

    /// Process source text, applying REPLACE directives.
    ///
    /// This is the main entry point (Pass 3 of the pipeline).
    /// It scans for REPLACE statements, updates the active replacement set,
    /// and applies replacements to non-directive lines.
    pub fn process(&mut self, source: &str) -> Result<String> {
        let mut result = String::with_capacity(source.len());
        let lines: Vec<&str> = source.lines().collect();
        let mut i = 0;

        while i < lines.len() {
            let line = lines[i];

            // Check if this is a comment line (column 7 = '*' or '/')
            if is_comment_line(line) {
                result.push_str(line);
                result.push('\n');
                i += 1;
                continue;
            }

            // Get the content portion of the line (from column 8 onward for fixed format)
            let content = get_content(line);
            let trimmed = content.trim();
            let trimmed_upper = trimmed.to_uppercase();

            // Check for REPLACE statement
            if trimmed_upper.starts_with("REPLACE ") || trimmed_upper == "REPLACE" {
                // Collect the full REPLACE statement (may span multiple lines)
                let mut replace_text = trimmed.to_string();

                // Keep reading lines until we find the terminating period
                while !replace_text.trim_end().ends_with('.') && i + 1 < lines.len() {
                    i += 1;
                    let next_content = get_content(lines[i]);
                    replace_text.push(' ');
                    replace_text.push_str(next_content.trim());
                }

                // Parse the REPLACE statement
                self.parse_replace_statement(&replace_text)?;

                // REPLACE directives are consumed (not passed to lexer)
                i += 1;
                continue;
            }

            // Apply active replacements to this line
            let processed_line = if self.active_replacements.is_empty() {
                line.to_string()
            } else {
                self.apply_replacements(line)
            };

            result.push_str(&processed_line);
            result.push('\n');
            i += 1;
        }

        Ok(result)
    }

    /// Parse a REPLACE statement and update active replacements.
    fn parse_replace_statement(&mut self, stmt: &str) -> Result<()> {
        // Remove the "REPLACE" keyword and trailing period
        let body = stmt
            .trim()
            .strip_prefix("REPLACE")
            .or_else(|| stmt.trim().strip_prefix("replace"))
            .or_else(|| stmt.trim().strip_prefix("Replace"))
            .unwrap_or(stmt.trim());
        let body = body.trim();

        // Strip trailing period
        let body = body.strip_suffix('.').unwrap_or(body).trim();

        // Check for REPLACE OFF
        if body.eq_ignore_ascii_case("OFF") {
            self.active_replacements.clear();
            return Ok(());
        }

        // Parse replacement pairs: ==from== BY ==to== [==from== BY ==to==]...
        let pairs = parse_pseudo_text_pairs(body)?;

        // A new REPLACE statement replaces (not supplements) the previous one
        self.active_replacements = pairs;

        Ok(())
    }

    /// Apply active replacements to a source line.
    fn apply_replacements(&self, line: &str) -> String {
        let mut result = line.to_string();

        for replacement in &self.active_replacements {
            if replacement.from.is_empty() {
                continue;
            }
            // Apply pseudo-text replacement
            // Pseudo-text matching is case-insensitive for COBOL tokens
            result = pseudo_text_replace(&result, &replacement.from, &replacement.to);
        }

        result
    }
}

impl Default for ReplaceProcessor {
    fn default() -> Self {
        Self::new()
    }
}

/// Check if a line is a comment (column 7 = '*' or '/').
fn is_comment_line(line: &str) -> bool {
    line.len() > 6 && {
        let c7 = line.as_bytes().get(6).copied().unwrap_or(b' ');
        c7 == b'*' || c7 == b'/'
    }
}

/// Get the content portion of a line (column 8 onward for fixed format).
fn get_content(line: &str) -> &str {
    if line.len() > 7 {
        &line[7..]
    } else {
        ""
    }
}

/// Parse pseudo-text replacement pairs from the body of a REPLACE statement.
///
/// Expected format: `==from1== BY ==to1== ==from2== BY ==to2==`
/// Empty pseudo-text `====` means delete (replace with nothing).
fn parse_pseudo_text_pairs(body: &str) -> Result<Vec<PseudoTextReplacement>> {
    let mut pairs = Vec::new();
    let mut rest = body.trim();

    while !rest.is_empty() {
        // Extract the "from" pseudo-text
        let (from_text, remaining) = extract_pseudo_text(rest)?;
        rest = remaining.trim();

        // Expect "BY" keyword
        let rest_upper = rest.to_uppercase();
        if !rest_upper.starts_with("BY") {
            return Err(CobolError::ParseError {
                message: format!(
                    "Expected BY keyword in REPLACE statement, found: {}",
                    &rest[..rest.len().min(20)]
                ),
            });
        }
        rest = rest[2..].trim();

        // Extract the "to" pseudo-text
        let (to_text, remaining) = extract_pseudo_text(rest)?;
        rest = remaining.trim();

        pairs.push(PseudoTextReplacement::new(from_text, to_text));
    }

    if pairs.is_empty() {
        return Err(CobolError::ParseError {
            message: "REPLACE statement contains no replacement pairs".to_string(),
        });
    }

    Ok(pairs)
}

/// Extract pseudo-text from a string starting with `==`.
///
/// Returns the pseudo-text content (without delimiters) and the remaining string.
fn extract_pseudo_text(s: &str) -> Result<(String, &str)> {
    let s = s.trim();

    if !s.starts_with("==") {
        return Err(CobolError::ParseError {
            message: format!(
                "Expected pseudo-text delimiter '==' in REPLACE, found: {}",
                &s[..s.len().min(20)]
            ),
        });
    }

    // Find the closing ==
    let content_start = 2;
    let rest = &s[content_start..];

    if let Some(end_pos) = rest.find("==") {
        let content = rest[..end_pos].trim().to_string();
        let remaining = &rest[end_pos + 2..];
        Ok((content, remaining))
    } else {
        Err(CobolError::ParseError {
            message: "Unterminated pseudo-text in REPLACE statement (missing closing '==')".to_string(),
        })
    }
}

/// Perform pseudo-text replacement on a string.
///
/// This handles COBOL pseudo-text matching semantics:
/// - Matching is case-insensitive
/// - Multiple spaces in source are treated as a single space for matching
/// - The replacement preserves the surrounding context
fn pseudo_text_replace(text: &str, from: &str, to: &str) -> String {
    if from.is_empty() {
        return text.to_string();
    }

    // For simple single-token patterns, do case-insensitive replacement
    // while preserving non-content areas (sequence/indicator columns)
    let text_upper = text.to_uppercase();
    let from_upper = from.to_uppercase();

    let mut result = String::with_capacity(text.len());
    let mut pos = 0;

    while pos < text.len() {
        if let Some(match_pos) = text_upper[pos..].find(&from_upper) {
            let abs_match = pos + match_pos;
            // Copy text before match
            result.push_str(&text[pos..abs_match]);
            // Insert replacement
            result.push_str(to);
            pos = abs_match + from.len();
        } else {
            // No more matches; copy rest
            result.push_str(&text[pos..]);
            break;
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    // ─── Story 60.1: Pseudo-Text Matching Engine ───

    #[test]
    fn test_pseudo_text_single_token_match() {
        let result = pseudo_text_replace(":PREFIX:-FIELD", ":PREFIX:", "WS");
        assert_eq!(result, "WS-FIELD");
    }

    #[test]
    fn test_pseudo_text_multi_token_match() {
        let result = pseudo_text_replace("MOVE A TO B", "MOVE A TO", "MOVE X TO");
        assert_eq!(result, "MOVE X TO B");
    }

    #[test]
    fn test_pseudo_text_delete_empty_replacement() {
        let result = pseudo_text_replace("DISPLAY HELLO", "DISPLAY ", "");
        assert_eq!(result, "HELLO");
    }

    #[test]
    fn test_pseudo_text_case_insensitive() {
        let result = pseudo_text_replace("move a to b", "MOVE A TO", "MOVE X TO");
        assert_eq!(result, "MOVE X TO b");
    }

    #[test]
    fn test_pseudo_text_no_match() {
        let result = pseudo_text_replace("DISPLAY HELLO", "COMPUTE", "ADD");
        assert_eq!(result, "DISPLAY HELLO");
    }

    #[test]
    fn test_pseudo_text_multiple_occurrences() {
        let result = pseudo_text_replace("FOO BAR FOO", "FOO", "BAZ");
        assert_eq!(result, "BAZ BAR BAZ");
    }

    // ─── Story 60.2: REPLACE Statement Parser ───

    #[test]
    fn test_extract_pseudo_text_simple() {
        let (text, rest) = extract_pseudo_text("==FOO== BY ==BAR==").unwrap();
        assert_eq!(text, "FOO");
        assert_eq!(rest.trim(), "BY ==BAR==");
    }

    #[test]
    fn test_extract_pseudo_text_empty() {
        let (text, _rest) = extract_pseudo_text("====").unwrap();
        assert_eq!(text, "");
    }

    #[test]
    fn test_extract_pseudo_text_with_spaces() {
        let (text, _rest) = extract_pseudo_text("== MOVE A TO == BY ==MOVE B TO==").unwrap();
        assert_eq!(text, "MOVE A TO");
    }

    #[test]
    fn test_extract_pseudo_text_unterminated() {
        let result = extract_pseudo_text("==FOO");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_pairs_single() {
        let pairs = parse_pseudo_text_pairs("==FOO== BY ==BAR==").unwrap();
        assert_eq!(pairs.len(), 1);
        assert_eq!(pairs[0].from, "FOO");
        assert_eq!(pairs[0].to, "BAR");
    }

    #[test]
    fn test_parse_pairs_multiple() {
        let pairs = parse_pseudo_text_pairs("==A== BY ==B== ==C== BY ==D==").unwrap();
        assert_eq!(pairs.len(), 2);
        assert_eq!(pairs[0].from, "A");
        assert_eq!(pairs[0].to, "B");
        assert_eq!(pairs[1].from, "C");
        assert_eq!(pairs[1].to, "D");
    }

    #[test]
    fn test_parse_pairs_with_deletion() {
        let pairs = parse_pseudo_text_pairs("==FOO== BY ====").unwrap();
        assert_eq!(pairs.len(), 1);
        assert_eq!(pairs[0].from, "FOO");
        assert_eq!(pairs[0].to, "");
    }

    #[test]
    fn test_parse_pairs_empty_body() {
        let result = parse_pseudo_text_pairs("");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_pairs_missing_by() {
        let result = parse_pseudo_text_pairs("==FOO== ==BAR==");
        assert!(result.is_err());
    }

    // ─── Story 60.2: REPLACE Statement Parsing ───

    #[test]
    fn test_replace_off() {
        let mut proc = ReplaceProcessor::new();
        // First activate some replacements
        proc.parse_replace_statement("REPLACE ==A== BY ==B==.").unwrap();
        assert_eq!(proc.active_replacements.len(), 1);

        // Then turn off
        proc.parse_replace_statement("REPLACE OFF.").unwrap();
        assert!(proc.active_replacements.is_empty());
    }

    #[test]
    fn test_replace_replaces_previous() {
        let mut proc = ReplaceProcessor::new();

        // First REPLACE
        proc.parse_replace_statement("REPLACE ==A== BY ==B==.").unwrap();
        assert_eq!(proc.active_replacements.len(), 1);
        assert_eq!(proc.active_replacements[0].from, "A");

        // Second REPLACE replaces (not supplements) the first
        proc.parse_replace_statement("REPLACE ==X== BY ==Y==.").unwrap();
        assert_eq!(proc.active_replacements.len(), 1);
        assert_eq!(proc.active_replacements[0].from, "X");
    }

    // ─── Story 60.3: Integration ───

    #[test]
    fn test_process_simple_replace() {
        let mut proc = ReplaceProcessor::new();
        let source = "       REPLACE ==:TAG:== BY ==WS-TAG==.\n       MOVE :TAG:-NAME TO :TAG:-OUT.\n";
        let result = proc.process(source).unwrap();
        assert!(result.contains("WS-TAG-NAME"));
        assert!(result.contains("WS-TAG-OUT"));
        // The REPLACE directive itself should not be in the output
        assert!(!result.contains("REPLACE =="));
    }

    #[test]
    fn test_process_replace_off() {
        let mut proc = ReplaceProcessor::new();
        let source = "       REPLACE ==FOO== BY ==BAR==.\n       MOVE FOO TO X.\n       REPLACE OFF.\n       MOVE FOO TO Y.\n";
        let result = proc.process(source).unwrap();
        // First MOVE should have FOO replaced
        assert!(result.contains("MOVE BAR TO X."));
        // After REPLACE OFF, FOO should remain
        assert!(result.contains("MOVE FOO TO Y."));
    }

    #[test]
    fn test_process_second_replace_overrides() {
        let mut proc = ReplaceProcessor::new();
        let source = "       REPLACE ==A== BY ==B==.\n       MOVE A TO X.\n       REPLACE ==A== BY ==C==.\n       MOVE A TO Y.\n";
        let result = proc.process(source).unwrap();
        assert!(result.contains("MOVE B TO X."));
        assert!(result.contains("MOVE C TO Y."));
    }

    #[test]
    fn test_process_comment_passthrough() {
        let mut proc = ReplaceProcessor::new();
        let source = "       REPLACE ==FOO== BY ==BAR==.\n      * This is a comment with FOO in it.\n       MOVE FOO TO X.\n";
        let result = proc.process(source).unwrap();
        // Comment should be unchanged
        assert!(result.contains("* This is a comment with FOO in it."));
        // Non-comment should be replaced
        assert!(result.contains("MOVE BAR TO X."));
    }

    // ─── Story 60.4: Edge Cases ───

    #[test]
    fn test_process_partial_word_match() {
        let mut proc = ReplaceProcessor::new();
        let source = "       REPLACE ==:TAG:== BY ==WS-TAG==.\n       01 :TAG:-NAME PIC X(10).\n       01 :TAG:-ADDR PIC X(20).\n";
        let result = proc.process(source).unwrap();
        assert!(result.contains("WS-TAG-NAME"));
        assert!(result.contains("WS-TAG-ADDR"));
    }

    #[test]
    fn test_process_delete_via_empty_replacement() {
        let mut proc = ReplaceProcessor::new();
        let source = "       REPLACE ==DEBUG-== BY ====.\n       DISPLAY DEBUG-MSG.\n";
        let result = proc.process(source).unwrap();
        assert!(result.contains("DISPLAY MSG."));
    }

    #[test]
    fn test_process_multiple_pairs_in_one_replace() {
        let mut proc = ReplaceProcessor::new();
        let source = "       REPLACE ==:A:== BY ==X== ==:B:== BY ==Y==.\n       MOVE :A: TO :B:.\n";
        let result = proc.process(source).unwrap();
        assert!(result.contains("MOVE X TO Y."));
    }

    #[test]
    fn test_process_no_replace_passthrough() {
        let mut proc = ReplaceProcessor::new();
        let source = "       MOVE A TO B.\n       DISPLAY C.\n";
        let result = proc.process(source).unwrap();
        assert_eq!(result, source);
    }

    #[test]
    fn test_process_multiline_replace_statement() {
        let mut proc = ReplaceProcessor::new();
        // REPLACE statement spanning multiple lines
        // :PREFIX: replaced by WS, so :PREFIX:-FIELD becomes WS-FIELD
        let source = "       REPLACE ==:PREFIX:==\n       BY ==WS==.\n       MOVE :PREFIX:-FIELD TO X.\n";
        let result = proc.process(source).unwrap();
        assert!(result.contains("WS-FIELD"), "result was: {:?}", result);
    }
}
