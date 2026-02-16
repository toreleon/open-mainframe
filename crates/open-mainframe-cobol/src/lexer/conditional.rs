//! Conditional Compilation Directives (Pass 1 of the Preprocessor Pipeline).
//!
//! Implements COBOL conditional compilation directives which control source
//! inclusion/exclusion. Per AD-2.0-01, this pass runs before COPY expansion
//! (Pass 2) so that conditional blocks can exclude COPY statements entirely.
//!
//! Supports:
//! - `>>DEFINE var AS 'value'` / `>>DEFINE var AS PARAMETER`
//! - `>>IF var = 'value'` / `>>IF DEFINED var`
//! - `>>ELSE`
//! - `>>END-IF`
//! - `>>EVALUATE var >>WHEN 'value' ... >>WHEN OTHER ... >>END-EVALUATE`
//! - `>>SET SOURCEFORMAT"FREE"` / `>>SET SOURCEFORMAT"FIXED"`

use std::collections::HashMap;

use crate::error::CobolError;

/// Result type for conditional compilation operations.
pub type Result<T> = std::result::Result<T, CobolError>;

/// Conditional compilation processor.
///
/// Processes COBOL source text before COPY expansion, evaluating >>DEFINE,
/// >>IF, >>EVALUATE, and >>SET directives to include or exclude source lines.
#[derive(Debug)]
pub struct ConditionalProcessor {
    /// Compile-time variable store: variable name → optional value.
    /// A variable defined with AS PARAMETER has `Some("")` if no external
    /// value is provided, or `None` if not defined.
    variables: HashMap<String, String>,
    /// Source format changes requested by >>SET (consumed by caller).
    pub source_format_changes: Vec<SourceFormatChange>,
}

/// A source format change requested by >>SET SOURCEFORMAT.
#[derive(Debug, Clone, PartialEq)]
pub struct SourceFormatChange {
    /// The line number (0-indexed) where the change was requested.
    pub line: usize,
    /// The new format.
    pub format: SourceFormatKind,
}

/// Source format kind for >>SET SOURCEFORMAT.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SourceFormatKind {
    /// Fixed format (columns 1-6 sequence, 7 indicator, 8-72 code).
    Fixed,
    /// Free format (no column restrictions).
    Free,
}

/// State of a conditional block (>>IF or >>EVALUATE branch).
#[derive(Debug, Clone)]
enum ConditionalState {
    /// In an >>IF block.
    If {
        /// Whether the >>IF condition was true (this branch is included).
        condition_met: bool,
        /// Whether we've entered the >>ELSE branch.
        in_else: bool,
    },
    /// In an >>EVALUATE block.
    Evaluate {
        /// The value being evaluated.
        eval_value: String,
        /// Whether any >>WHEN branch has matched.
        any_matched: bool,
        /// Whether the current >>WHEN branch is active.
        current_active: bool,
    },
}

impl ConditionalProcessor {
    /// Create a new conditional compilation processor.
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            source_format_changes: Vec::new(),
        }
    }

    /// Create a processor with pre-defined compile-time variables.
    pub fn with_variables(variables: HashMap<String, String>) -> Self {
        Self {
            variables,
            source_format_changes: Vec::new(),
        }
    }

    /// Define a compile-time variable.
    pub fn define(&mut self, name: &str, value: &str) {
        self.variables
            .insert(name.to_uppercase(), value.to_string());
    }

    /// Check if a variable is defined.
    pub fn is_defined(&self, name: &str) -> bool {
        self.variables.contains_key(&name.to_uppercase())
    }

    /// Get the value of a variable.
    pub fn get_variable(&self, name: &str) -> Option<&str> {
        self.variables.get(&name.to_uppercase()).map(|s| s.as_str())
    }

    /// Process source text, evaluating conditional compilation directives.
    ///
    /// This is the main entry point (Pass 1 of the pipeline).
    /// Lines inside excluded conditional blocks are removed.
    /// Directive lines (>>DEFINE, >>IF, etc.) are always consumed.
    pub fn process(&mut self, source: &str) -> Result<String> {
        let mut result = String::with_capacity(source.len());
        let lines: Vec<&str> = source.lines().collect();
        let mut stack: Vec<ConditionalState> = Vec::new();

        for (line_idx, line) in lines.iter().enumerate() {
            // Check if this is a comment line
            if is_comment_line(line) {
                if is_active(&stack) {
                    result.push_str(line);
                    result.push('\n');
                }
                continue;
            }

            // Extract directive from the line
            let directive = extract_directive(line);

            match directive {
                Some(DirectiveKind::Define { name, value }) => {
                    if is_active(&stack) {
                        self.variables.insert(name.to_uppercase(), value);
                    }
                    // Directive consumed — not passed to output
                }
                Some(DirectiveKind::If { condition }) => {
                    let cond_met = if is_active(&stack) {
                        self.evaluate_condition(&condition)
                    } else {
                        false
                    };
                    stack.push(ConditionalState::If {
                        condition_met: cond_met && is_active_before_push(&stack),
                        in_else: false,
                    });
                }
                Some(DirectiveKind::Else) => {
                    let parent_active = is_parent_active(&stack);
                    if let Some(ConditionalState::If {
                        condition_met,
                        in_else,
                    }) = stack.last_mut()
                    {
                        if *in_else {
                            return Err(CobolError::ParseError {
                                message: "Duplicate >>ELSE without >>END-IF".to_string(),
                            });
                        }
                        *in_else = true;
                        // Flip the condition for the ELSE branch, but only if parent is active
                        *condition_met = !*condition_met && parent_active;
                    } else {
                        return Err(CobolError::ParseError {
                            message: ">>ELSE without matching >>IF".to_string(),
                        });
                    }
                }
                Some(DirectiveKind::EndIf) => {
                    match stack.last() {
                        Some(ConditionalState::If { .. }) => {
                            stack.pop();
                        }
                        _ => {
                            return Err(CobolError::ParseError {
                                message: ">>END-IF without matching >>IF".to_string(),
                            });
                        }
                    }
                }
                Some(DirectiveKind::Evaluate { variable }) => {
                    let eval_value = if is_active(&stack) {
                        self.variables
                            .get(&variable.to_uppercase())
                            .cloned()
                            .unwrap_or_default()
                    } else {
                        String::new()
                    };
                    stack.push(ConditionalState::Evaluate {
                        eval_value,
                        any_matched: false,
                        current_active: false,
                    });
                }
                Some(DirectiveKind::When { value }) => {
                    let parent_active = is_parent_active(&stack);
                    if let Some(ConditionalState::Evaluate {
                        eval_value,
                        any_matched,
                        current_active,
                    }) = stack.last_mut()
                    {
                        if *any_matched || !parent_active {
                            *current_active = false;
                        } else if value.eq_ignore_ascii_case("OTHER") {
                            // >>WHEN OTHER matches if no previous >>WHEN matched
                            *current_active = true;
                            *any_matched = true;
                        } else {
                            let matches = value.eq_ignore_ascii_case(eval_value);
                            *current_active = matches;
                            if matches {
                                *any_matched = true;
                            }
                        }
                    } else {
                        return Err(CobolError::ParseError {
                            message: ">>WHEN without matching >>EVALUATE".to_string(),
                        });
                    }
                }
                Some(DirectiveKind::EndEvaluate) => {
                    match stack.last() {
                        Some(ConditionalState::Evaluate { .. }) => {
                            stack.pop();
                        }
                        _ => {
                            return Err(CobolError::ParseError {
                                message: ">>END-EVALUATE without matching >>EVALUATE".to_string(),
                            });
                        }
                    }
                }
                Some(DirectiveKind::Set { option, value }) => {
                    if is_active(&stack) {
                        self.handle_set(line_idx, &option, &value)?;
                    }
                }
                None => {
                    // Regular line — include only if all conditions are active
                    if is_active(&stack) {
                        result.push_str(line);
                        result.push('\n');
                    }
                }
            }
        }

        // Check for unclosed blocks
        if !stack.is_empty() {
            let unclosed = match stack.last().unwrap() {
                ConditionalState::If { .. } => ">>IF",
                ConditionalState::Evaluate { .. } => ">>EVALUATE",
            };
            return Err(CobolError::ParseError {
                message: format!("Unclosed {} block at end of source", unclosed),
            });
        }

        Ok(result)
    }

    /// Evaluate a conditional expression for >>IF.
    fn evaluate_condition(&self, condition: &str) -> bool {
        let cond = condition.trim();

        // >>IF DEFINED var-name
        if let Some(rest) = cond
            .strip_prefix("DEFINED ")
            .or_else(|| cond.strip_prefix("defined "))
        {
            let var_name = rest.trim();
            return self.is_defined(var_name);
        }

        // >>IF var-name = 'value'
        if let Some((var_part, val_part)) = cond.split_once('=') {
            let var_name = var_part.trim();
            let val = val_part.trim();
            // Strip quotes from value
            let val = val
                .trim_matches('\'')
                .trim_matches('"');
            return match self.get_variable(var_name) {
                Some(v) => v.eq_ignore_ascii_case(val),
                None => false,
            };
        }

        // >>IF NOT DEFINED var-name
        if let Some(rest) = cond
            .strip_prefix("NOT DEFINED ")
            .or_else(|| cond.strip_prefix("not defined "))
        {
            let var_name = rest.trim();
            return !self.is_defined(var_name);
        }

        // Fallback: treat as boolean — defined and non-empty = true
        match self.get_variable(cond) {
            Some(v) => !v.is_empty(),
            None => false,
        }
    }

    /// Handle >>SET directive.
    fn handle_set(&mut self, line_idx: usize, option: &str, value: &str) -> Result<()> {
        if option.eq_ignore_ascii_case("SOURCEFORMAT") {
            let format = if value.eq_ignore_ascii_case("FREE") {
                SourceFormatKind::Free
            } else if value.eq_ignore_ascii_case("FIXED") {
                SourceFormatKind::Fixed
            } else {
                return Err(CobolError::ParseError {
                    message: format!(
                        "Invalid SOURCEFORMAT value '{}', expected FREE or FIXED",
                        value
                    ),
                });
            };
            self.source_format_changes.push(SourceFormatChange {
                line: line_idx,
                format,
            });
        }
        // Other >>SET options can be added here in the future
        Ok(())
    }
}

impl Default for ConditionalProcessor {
    fn default() -> Self {
        Self::new()
    }
}

/// Kinds of directives we recognize.
#[derive(Debug)]
enum DirectiveKind {
    Define { name: String, value: String },
    If { condition: String },
    Else,
    EndIf,
    Evaluate { variable: String },
    When { value: String },
    EndEvaluate,
    Set { option: String, value: String },
}

/// Check if a line is a comment (column 7 = '*' or '/').
fn is_comment_line(line: &str) -> bool {
    line.len() > 6 && {
        let c7 = line.as_bytes().get(6).copied().unwrap_or(b' ');
        c7 == b'*' || c7 == b'/'
    }
}

/// Check if all conditions in the stack are active (we should include lines).
fn is_active(stack: &[ConditionalState]) -> bool {
    for state in stack {
        match state {
            ConditionalState::If { condition_met, .. } => {
                if !condition_met {
                    return false;
                }
            }
            ConditionalState::Evaluate {
                current_active, ..
            } => {
                if !current_active {
                    return false;
                }
            }
        }
    }
    true
}

/// Check if all conditions in the stack BEFORE the last element are active.
fn is_parent_active(stack: &[ConditionalState]) -> bool {
    if stack.len() <= 1 {
        return true;
    }
    is_active(&stack[..stack.len() - 1])
}

/// Check if conditions are active before a push (i.e., current full stack).
fn is_active_before_push(stack: &[ConditionalState]) -> bool {
    is_active(stack)
}

/// Extract a compiler directive from a source line.
///
/// Directives start with `>>` and appear in the content area.
/// For fixed format, the content starts at column 8 (index 7).
/// We also check from column 1 for free-format or cases where >> appears early.
fn extract_directive(line: &str) -> Option<DirectiveKind> {
    // Find `>>` anywhere in the line (may be in area A or B)
    let trimmed = line.trim();
    let content = if line.len() > 7 {
        line[7..].trim()
    } else {
        trimmed
    };

    // Check both the full trimmed line and the content area
    let directive_text = if content.starts_with(">>") {
        content
    } else if trimmed.starts_with(">>") {
        trimmed
    } else {
        return None;
    };

    let rest = &directive_text[2..].trim();
    let upper = rest.to_uppercase();

    // >>DEFINE name AS 'value' or >>DEFINE name AS PARAMETER
    if let Some(after_define) = upper
        .strip_prefix("DEFINE ")
    {
        return parse_define(after_define, rest);
    }

    // >>END-IF
    if upper == "END-IF" || upper.starts_with("END-IF ") {
        return Some(DirectiveKind::EndIf);
    }

    // >>END-EVALUATE
    if upper == "END-EVALUATE" || upper.starts_with("END-EVALUATE ") {
        return Some(DirectiveKind::EndEvaluate);
    }

    // >>IF condition
    if upper.starts_with("IF ") {
        let condition = rest[3..].trim().to_string();
        if condition.is_empty() {
            return None;
        }
        return Some(DirectiveKind::If { condition });
    }

    // >>ELSE
    if upper == "ELSE" || upper.starts_with("ELSE ") {
        return Some(DirectiveKind::Else);
    }

    // >>EVALUATE variable
    if upper.starts_with("EVALUATE ") {
        let variable = rest[9..].trim().to_string();
        return Some(DirectiveKind::Evaluate { variable });
    }

    // >>WHEN value or >>WHEN OTHER
    if upper.starts_with("WHEN ") {
        let value = rest[5..].trim();
        // Strip quotes
        let value = value.trim_matches('\'').trim_matches('"').to_string();
        return Some(DirectiveKind::When { value });
    }

    // >>SET option"value" or >>SET option "value"
    if upper.starts_with("SET ") {
        let set_rest = rest[4..].trim();
        return parse_set(set_rest);
    }

    None
}

/// Parse a >>DEFINE directive.
fn parse_define(_upper_after: &str, original_rest: &str) -> Option<DirectiveKind> {
    // Format: name AS 'value' or name AS PARAMETER
    let define_text = &original_rest[7..]; // skip "DEFINE "
    let parts: Vec<&str> = define_text.splitn(3, char::is_whitespace).collect();

    if parts.len() < 1 {
        return None;
    }

    let name = parts[0].trim().to_string();

    if parts.len() < 3
        || !parts[1].eq_ignore_ascii_case("AS")
    {
        // >>DEFINE name (no value — treated as defined with empty value)
        return Some(DirectiveKind::Define {
            name,
            value: String::new(),
        });
    }

    let raw_value = parts[2].trim();
    if raw_value.eq_ignore_ascii_case("PARAMETER") {
        // >>DEFINE name AS PARAMETER — defined but with empty value
        return Some(DirectiveKind::Define {
            name,
            value: String::new(),
        });
    }

    // Strip quotes from value
    let value = raw_value
        .trim_matches('\'')
        .trim_matches('"')
        .to_string();

    Some(DirectiveKind::Define { name, value })
}

/// Parse a >>SET directive.
fn parse_set(set_rest: &str) -> Option<DirectiveKind> {
    // Format: SOURCEFORMAT"FREE" or SOURCEFORMAT "FREE" or SOURCEFORMAT 'FIXED'
    // Find the option name and value
    let upper = set_rest.to_uppercase();

    // Try SOURCEFORMAT"value" (no space)
    if upper.starts_with("SOURCEFORMAT") {
        let after_sf_orig = &set_rest["SOURCEFORMAT".len()..];
        let value_text = after_sf_orig
            .trim()
            .trim_matches('"')
            .trim_matches('\'')
            .trim();
        if !value_text.is_empty() {
            return Some(DirectiveKind::Set {
                option: "SOURCEFORMAT".to_string(),
                value: value_text.to_string(),
            });
        }
    }

    // Generic: option value
    let parts: Vec<&str> = set_rest.splitn(2, char::is_whitespace).collect();
    if parts.len() >= 2 {
        let option = parts[0].trim().to_string();
        let value = parts[1]
            .trim()
            .trim_matches('"')
            .trim_matches('\'')
            .to_string();
        return Some(DirectiveKind::Set { option, value });
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    // ─── Story 61.1: Compile-Time Variable Store ───

    #[test]
    fn test_define_variable() {
        let mut proc = ConditionalProcessor::new();
        proc.define("MY-VAR", "PROD");
        assert!(proc.is_defined("MY-VAR"));
        assert_eq!(proc.get_variable("MY-VAR"), Some("PROD"));
    }

    #[test]
    fn test_define_case_insensitive() {
        let mut proc = ConditionalProcessor::new();
        proc.define("my-var", "test");
        assert!(proc.is_defined("MY-VAR"));
        assert_eq!(proc.get_variable("my-var"), Some("test"));
    }

    #[test]
    fn test_define_redefine() {
        let mut proc = ConditionalProcessor::new();
        proc.define("MY-VAR", "PROD");
        proc.define("MY-VAR", "TEST");
        assert_eq!(proc.get_variable("MY-VAR"), Some("TEST"));
    }

    #[test]
    fn test_undefined_variable() {
        let mut proc = ConditionalProcessor::new();
        assert!(!proc.is_defined("NONEXISTENT"));
        assert_eq!(proc.get_variable("NONEXISTENT"), None);
    }

    #[test]
    fn test_process_define_directive() {
        let mut proc = ConditionalProcessor::new();
        let source = "       >>DEFINE MY-VAR AS 'PROD'\n       DISPLAY 'HELLO'.\n";
        let result = proc.process(source).unwrap();
        assert!(proc.is_defined("MY-VAR"));
        assert_eq!(proc.get_variable("MY-VAR"), Some("PROD"));
        // Directive consumed, DISPLAY line included
        assert!(result.contains("DISPLAY 'HELLO'"));
        assert!(!result.contains(">>DEFINE"));
    }

    #[test]
    fn test_process_define_parameter() {
        let mut proc = ConditionalProcessor::new();
        let source = "       >>DEFINE MY-FLAG AS PARAMETER\n";
        let _ = proc.process(source).unwrap();
        assert!(proc.is_defined("MY-FLAG"));
        assert_eq!(proc.get_variable("MY-FLAG"), Some(""));
    }

    // ─── Story 61.2: >>IF / >>ELSE / >>END-IF ───

    #[test]
    fn test_if_true_branch() {
        let mut proc = ConditionalProcessor::new();
        proc.define("ENV", "PROD");
        let source = "\
       >>IF ENV = 'PROD'
       DISPLAY 'PRODUCTION'.
       >>END-IF
";
        let result = proc.process(source).unwrap();
        assert!(result.contains("DISPLAY 'PRODUCTION'"));
    }

    #[test]
    fn test_if_false_branch() {
        let mut proc = ConditionalProcessor::new();
        proc.define("ENV", "TEST");
        let source = "\
       >>IF ENV = 'PROD'
       DISPLAY 'PRODUCTION'.
       >>END-IF
";
        let result = proc.process(source).unwrap();
        assert!(!result.contains("DISPLAY 'PRODUCTION'"));
    }

    #[test]
    fn test_if_else() {
        let mut proc = ConditionalProcessor::new();
        proc.define("ENV", "TEST");
        let source = "\
       >>IF ENV = 'PROD'
       DISPLAY 'PRODUCTION'.
       >>ELSE
       DISPLAY 'TESTING'.
       >>END-IF
";
        let result = proc.process(source).unwrap();
        assert!(!result.contains("DISPLAY 'PRODUCTION'"));
        assert!(result.contains("DISPLAY 'TESTING'"));
    }

    #[test]
    fn test_if_defined() {
        let mut proc = ConditionalProcessor::new();
        proc.define("DEBUG-MODE", "");
        let source = "\
       >>IF DEFINED DEBUG-MODE
       DISPLAY 'DEBUG ON'.
       >>END-IF
";
        let result = proc.process(source).unwrap();
        assert!(result.contains("DISPLAY 'DEBUG ON'"));
    }

    #[test]
    fn test_if_not_defined() {
        let mut proc = ConditionalProcessor::new();
        let source = "\
       >>IF DEFINED MY-FLAG
       DISPLAY 'FLAG SET'.
       >>END-IF
";
        let result = proc.process(source).unwrap();
        assert!(!result.contains("DISPLAY 'FLAG SET'"));
    }

    #[test]
    fn test_nested_if() {
        let mut proc = ConditionalProcessor::new();
        proc.define("ENV", "PROD");
        proc.define("DEBUG", "ON");
        let source = "\
       >>IF ENV = 'PROD'
       DISPLAY 'PROD'.
       >>IF DEFINED DEBUG
       DISPLAY 'PROD-DEBUG'.
       >>END-IF
       >>END-IF
";
        let result = proc.process(source).unwrap();
        assert!(result.contains("DISPLAY 'PROD'"));
        assert!(result.contains("DISPLAY 'PROD-DEBUG'"));
    }

    #[test]
    fn test_nested_if_outer_false() {
        let mut proc = ConditionalProcessor::new();
        proc.define("ENV", "TEST");
        proc.define("DEBUG", "ON");
        let source = "\
       >>IF ENV = 'PROD'
       DISPLAY 'PROD'.
       >>IF DEFINED DEBUG
       DISPLAY 'PROD-DEBUG'.
       >>END-IF
       >>END-IF
";
        let result = proc.process(source).unwrap();
        assert!(!result.contains("DISPLAY 'PROD'"));
        assert!(!result.contains("DISPLAY 'PROD-DEBUG'"));
    }

    // ─── Story 61.3: >>EVALUATE / >>WHEN / >>END-EVALUATE ───

    #[test]
    fn test_evaluate_first_when() {
        let mut proc = ConditionalProcessor::new();
        proc.define("ENV", "PROD");
        let source = "\
       >>EVALUATE ENV
       >>WHEN 'PROD'
       DISPLAY 'PRODUCTION'.
       >>WHEN 'TEST'
       DISPLAY 'TESTING'.
       >>END-EVALUATE
";
        let result = proc.process(source).unwrap();
        assert!(result.contains("DISPLAY 'PRODUCTION'"));
        assert!(!result.contains("DISPLAY 'TESTING'"));
    }

    #[test]
    fn test_evaluate_second_when() {
        let mut proc = ConditionalProcessor::new();
        proc.define("ENV", "TEST");
        let source = "\
       >>EVALUATE ENV
       >>WHEN 'PROD'
       DISPLAY 'PRODUCTION'.
       >>WHEN 'TEST'
       DISPLAY 'TESTING'.
       >>END-EVALUATE
";
        let result = proc.process(source).unwrap();
        assert!(!result.contains("DISPLAY 'PRODUCTION'"));
        assert!(result.contains("DISPLAY 'TESTING'"));
    }

    #[test]
    fn test_evaluate_when_other() {
        let mut proc = ConditionalProcessor::new();
        proc.define("ENV", "DEV");
        let source = "\
       >>EVALUATE ENV
       >>WHEN 'PROD'
       DISPLAY 'PRODUCTION'.
       >>WHEN 'TEST'
       DISPLAY 'TESTING'.
       >>WHEN OTHER
       DISPLAY 'OTHER'.
       >>END-EVALUATE
";
        let result = proc.process(source).unwrap();
        assert!(!result.contains("DISPLAY 'PRODUCTION'"));
        assert!(!result.contains("DISPLAY 'TESTING'"));
        assert!(result.contains("DISPLAY 'OTHER'"));
    }

    #[test]
    fn test_evaluate_no_match_no_other() {
        let mut proc = ConditionalProcessor::new();
        proc.define("ENV", "DEV");
        let source = "\
       >>EVALUATE ENV
       >>WHEN 'PROD'
       DISPLAY 'PRODUCTION'.
       >>WHEN 'TEST'
       DISPLAY 'TESTING'.
       >>END-EVALUATE
";
        let result = proc.process(source).unwrap();
        assert!(!result.contains("DISPLAY 'PRODUCTION'"));
        assert!(!result.contains("DISPLAY 'TESTING'"));
    }

    // ─── Story 61.4: >>SET Directive ───

    #[test]
    fn test_set_sourceformat_free() {
        let mut proc = ConditionalProcessor::new();
        let source = "       >>SET SOURCEFORMAT\"FREE\"\n       DISPLAY 'HELLO'.\n";
        let result = proc.process(source).unwrap();
        assert_eq!(proc.source_format_changes.len(), 1);
        assert_eq!(proc.source_format_changes[0].format, SourceFormatKind::Free);
        assert!(result.contains("DISPLAY 'HELLO'"));
        assert!(!result.contains(">>SET"));
    }

    #[test]
    fn test_set_sourceformat_fixed() {
        let mut proc = ConditionalProcessor::new();
        let source = "       >>SET SOURCEFORMAT\"FIXED\"\n";
        let _ = proc.process(source).unwrap();
        assert_eq!(proc.source_format_changes.len(), 1);
        assert_eq!(
            proc.source_format_changes[0].format,
            SourceFormatKind::Fixed
        );
    }

    #[test]
    fn test_set_sourceformat_with_spaces() {
        let mut proc = ConditionalProcessor::new();
        let source = "       >>SET SOURCEFORMAT 'FREE'\n";
        let _ = proc.process(source).unwrap();
        assert_eq!(proc.source_format_changes.len(), 1);
        assert_eq!(proc.source_format_changes[0].format, SourceFormatKind::Free);
    }

    // ─── Story 61.5: Integration ───

    #[test]
    fn test_define_inside_excluded_block() {
        let mut proc = ConditionalProcessor::new();
        proc.define("ENV", "TEST");
        let source = "\
       >>IF ENV = 'PROD'
       >>DEFINE SPECIAL-FLAG AS 'YES'
       >>END-IF
";
        let _ = proc.process(source).unwrap();
        // DEFINE inside excluded block should not take effect
        assert!(!proc.is_defined("SPECIAL-FLAG"));
    }

    #[test]
    fn test_if_excludes_copy() {
        let mut proc = ConditionalProcessor::new();
        proc.define("USE-COPY", "NO");
        let source = "\
       >>IF USE-COPY = 'YES'
       COPY MYBOOK.
       >>END-IF
       DISPLAY 'AFTER'.
";
        let result = proc.process(source).unwrap();
        assert!(!result.contains("COPY MYBOOK"));
        assert!(result.contains("DISPLAY 'AFTER'"));
    }

    #[test]
    fn test_comment_passthrough_in_active_block() {
        let mut proc = ConditionalProcessor::new();
        proc.define("ENV", "PROD");
        let source = "\
       >>IF ENV = 'PROD'
      * This is a comment
       DISPLAY 'PROD'.
       >>END-IF
";
        let result = proc.process(source).unwrap();
        assert!(result.contains("* This is a comment"));
        assert!(result.contains("DISPLAY 'PROD'"));
    }

    #[test]
    fn test_comment_excluded_in_inactive_block() {
        let mut proc = ConditionalProcessor::new();
        proc.define("ENV", "TEST");
        let source = "\
       >>IF ENV = 'PROD'
      * This is a comment
       DISPLAY 'PROD'.
       >>END-IF
";
        let result = proc.process(source).unwrap();
        assert!(!result.contains("* This is a comment"));
        assert!(!result.contains("DISPLAY 'PROD'"));
    }

    #[test]
    fn test_full_pipeline_order() {
        let mut proc = ConditionalProcessor::new();
        let source = "\
       >>DEFINE ENV AS 'PROD'
       >>IF ENV = 'PROD'
       DISPLAY 'PRODUCTION'.
       COPY PRODBOOK.
       >>ELSE
       DISPLAY 'TESTING'.
       COPY TESTBOOK.
       >>END-IF
       REPLACE ==:TAG:== BY ==WS-TAG==.
       MOVE :TAG:-NAME TO X.
";
        let result = proc.process(source).unwrap();
        // Conditional pass includes PROD branch
        assert!(result.contains("DISPLAY 'PRODUCTION'"));
        assert!(result.contains("COPY PRODBOOK"));
        // Excludes TEST branch
        assert!(!result.contains("DISPLAY 'TESTING'"));
        assert!(!result.contains("COPY TESTBOOK"));
        // REPLACE line passes through (processed by Pass 3)
        assert!(result.contains("REPLACE ==:TAG:== BY ==WS-TAG==."));
        assert!(result.contains("MOVE :TAG:-NAME TO X."));
    }

    #[test]
    fn test_no_directives_passthrough() {
        let mut proc = ConditionalProcessor::new();
        let source = "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.\n";
        let result = proc.process(source).unwrap();
        assert_eq!(result, source);
    }

    // ─── Error Cases ───

    #[test]
    fn test_unclosed_if() {
        let mut proc = ConditionalProcessor::new();
        let source = "       >>IF DEFINED MY-VAR\n       DISPLAY 'X'.\n";
        let result = proc.process(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_else_without_if() {
        let mut proc = ConditionalProcessor::new();
        let source = "       >>ELSE\n";
        let result = proc.process(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_endif_without_if() {
        let mut proc = ConditionalProcessor::new();
        let source = "       >>END-IF\n";
        let result = proc.process(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_unclosed_evaluate() {
        let mut proc = ConditionalProcessor::new();
        let source = "       >>EVALUATE MY-VAR\n       >>WHEN 'X'\n       DISPLAY 'X'.\n";
        let result = proc.process(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_when_without_evaluate() {
        let mut proc = ConditionalProcessor::new();
        let source = "       >>WHEN 'X'\n";
        let result = proc.process(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_with_predefined_variables() {
        let mut vars = HashMap::new();
        vars.insert("ENV".to_string(), "PROD".to_string());
        let mut proc = ConditionalProcessor::with_variables(vars);
        let source = "\
       >>IF ENV = 'PROD'
       DISPLAY 'YES'.
       >>END-IF
";
        let result = proc.process(source).unwrap();
        assert!(result.contains("DISPLAY 'YES'"));
    }
}
