//! COBOL Source Preprocessor.
//!
//! Expands COPY statements by inlining copybook content.
//! Handles:
//! - Basic COPY statement: COPY copybook-name.
//! - COPY with library: COPY copybook-name IN library.
//! - COPY with REPLACING: COPY copybook-name REPLACING ==old== BY ==new==.
//! - Nested copybook inclusion
//! - Circular inclusion detection

use crate::error::CobolError;
use crate::lexer::copybook::{apply_replacements, CopybookConfig, CopybookResolver, Replacement};
use crate::lexer::source::SourceFormat;

/// Result type for preprocessor operations.
pub type Result<T> = std::result::Result<T, CobolError>;

/// COBOL source preprocessor.
///
/// Expands COPY statements by resolving and inlining copybook content.
#[derive(Debug)]
pub struct Preprocessor {
    /// Copybook resolver.
    resolver: CopybookResolver,
    /// Maximum nesting depth for copybooks.
    max_depth: usize,
}

impl Preprocessor {
    /// Create a new preprocessor.
    pub fn new(config: CopybookConfig, format: SourceFormat) -> Self {
        Self {
            resolver: CopybookResolver::new(config, format),
            max_depth: 10,
        }
    }

    /// Set the maximum copybook nesting depth.
    pub fn with_max_depth(mut self, depth: usize) -> Self {
        self.max_depth = depth;
        self
    }

    /// Preprocess COBOL source, expanding all COPY statements.
    ///
    /// Returns the expanded source with all copybooks inlined.
    pub fn preprocess(&mut self, source: &str) -> Result<String> {
        self.preprocess_recursive(source, 0)
    }

    /// Recursively preprocess source, tracking depth.
    fn preprocess_recursive(&mut self, source: &str, depth: usize) -> Result<String> {
        if depth > self.max_depth {
            return Err(CobolError::ParseError {
                message: format!(
                    "Maximum copybook nesting depth ({}) exceeded",
                    self.max_depth
                ),
            });
        }

        let mut result = String::with_capacity(source.len());
        let mut copy_buffer = String::new();
        let mut in_copy = false;

        for line in source.lines() {
            // Check if this is a comment line (column 7 = '*' or '/')
            let is_comment = line.len() > 6 && {
                let c7 = line.chars().nth(6).unwrap_or(' ');
                c7 == '*' || c7 == '/'
            };

            if is_comment {
                result.push_str(line);
                result.push('\n');
                continue;
            }

            // Get content starting from column 8 (index 7)
            let content = if line.len() > 7 { &line[7..] } else { "" };

            // Check if continuing a multi-line COPY
            if in_copy {
                copy_buffer.push(' ');
                copy_buffer.push_str(content.trim());

                // Check if COPY statement ends with period
                if copy_buffer.trim().ends_with('.') {
                    let expanded = self.expand_copy(&copy_buffer, depth)?;
                    result.push_str(&expanded);
                    copy_buffer.clear();
                    in_copy = false;
                }
                continue;
            }

            // Check for COPY statement start
            let trimmed = content.trim();
            if trimmed.to_uppercase().starts_with("COPY ") {
                copy_buffer = trimmed.to_string();

                // Check if COPY statement ends on this line
                if trimmed.ends_with('.') {
                    let expanded = self.expand_copy(&copy_buffer, depth)?;
                    result.push_str(&expanded);
                    copy_buffer.clear();
                } else {
                    in_copy = true;
                }
                continue;
            }

            // Regular line - pass through
            result.push_str(line);
            result.push('\n');
        }

        // Handle unclosed COPY (error)
        if in_copy {
            return Err(CobolError::ParseError {
                message: format!("Unclosed COPY statement: {}", copy_buffer),
            });
        }

        Ok(result)
    }

    /// Expand a COPY statement into the copybook content.
    fn expand_copy(&mut self, stmt: &str, depth: usize) -> Result<String> {
        let parsed = self.parse_copy_statement(stmt)?;

        // Resolve and load the copybook
        let path = self.resolver.resolve(&parsed.copybook_name)?;
        let content = std::fs::read_to_string(&path).map_err(|e| CobolError::ParseError {
            message: format!("Failed to read copybook '{}': {}", parsed.copybook_name, e),
        })?;

        // Apply REPLACING if present
        let content = if parsed.replacements.is_empty() {
            content
        } else {
            apply_replacements(&content, &parsed.replacements)
        };

        // Track nesting for circular detection
        self.resolver.push_include(&parsed.copybook_name)?;

        // Recursively preprocess (handles nested COPY)
        let expanded = self.preprocess_recursive(&content, depth + 1)?;

        self.resolver.pop_include();

        // Add comment markers for debugging
        Ok(format!(
            "      *>>> COPY {} from {}\n{}\n      *<<< END COPY {}\n",
            parsed.copybook_name,
            path.display(),
            expanded.trim_end(),
            parsed.copybook_name
        ))
    }

    /// Parse a COPY statement to extract copybook name and REPLACING clause.
    fn parse_copy_statement(&self, stmt: &str) -> Result<CopyStatement> {
        // Expected format: COPY name [IN library] [REPLACING ...].
        let stmt = stmt.trim().trim_end_matches('.');
        let parts: Vec<&str> = stmt.split_whitespace().collect();

        if parts.is_empty() || !parts[0].eq_ignore_ascii_case("COPY") {
            return Err(CobolError::ParseError {
                message: format!("Invalid COPY statement: {}", stmt),
            });
        }

        if parts.len() < 2 {
            return Err(CobolError::ParseError {
                message: "COPY statement missing copybook name".to_string(),
            });
        }

        // Get copybook name (strip quotes if present)
        let copybook_name = parts[1]
            .trim_matches('\'')
            .trim_matches('"')
            .to_string();
        let mut idx = 2;

        // Check for IN/OF library
        if idx < parts.len()
            && (parts[idx].eq_ignore_ascii_case("IN") || parts[idx].eq_ignore_ascii_case("OF"))
        {
            idx += 2; // Skip "IN" and library name
        }

        // Check for REPLACING clause
        let mut replacements = Vec::new();
        if idx < parts.len() && parts[idx].eq_ignore_ascii_case("REPLACING") {
            idx += 1;
            // Collect the rest of the REPLACING clause
            let replacing_text: String = parts[idx..].join(" ");
            replacements = self.parse_replacing(&replacing_text);
        }

        Ok(CopyStatement {
            copybook_name,
            replacements,
        })
    }

    /// Parse REPLACING clause into replacements.
    fn parse_replacing(&self, text: &str) -> Vec<Replacement> {
        let mut replacements = Vec::new();
        let mut current_from = String::new();
        let mut current_to = String::new();
        let mut collecting_to = false;

        for part in text.split_whitespace() {
            if part.eq_ignore_ascii_case("BY") {
                collecting_to = true;
                continue;
            }

            if collecting_to {
                // Check if this starts a new pattern
                if part.starts_with("==") && !current_to.is_empty() {
                    // Finish current replacement
                    if !current_from.is_empty() {
                        replacements.push(Replacement::new(
                            Self::clean_delimiters(&current_from),
                            Self::clean_delimiters(&current_to),
                        ));
                    }
                    current_from = part.to_string();
                    current_to.clear();
                    collecting_to = false;
                } else {
                    if !current_to.is_empty() {
                        current_to.push(' ');
                    }
                    current_to.push_str(part);
                }
            } else {
                if !current_from.is_empty() {
                    current_from.push(' ');
                }
                current_from.push_str(part);
            }
        }

        // Handle last replacement
        if !current_from.is_empty() {
            replacements.push(Replacement::new(
                Self::clean_delimiters(&current_from),
                Self::clean_delimiters(&current_to),
            ));
        }

        replacements
    }

    /// Remove == delimiters from pattern.
    fn clean_delimiters(s: &str) -> String {
        s.trim_start_matches("==")
            .trim_end_matches("==")
            .to_string()
    }
}

/// Parsed COPY statement.
#[derive(Debug)]
struct CopyStatement {
    /// Name of the copybook.
    copybook_name: String,
    /// REPLACING substitutions.
    replacements: Vec<Replacement>,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_config() -> CopybookConfig {
        CopybookConfig::new()
    }

    #[test]
    fn test_parse_simple_copy() {
        let preprocessor = Preprocessor::new(test_config(), SourceFormat::Fixed);
        let stmt = "COPY MYBOOK.";
        let parsed = preprocessor.parse_copy_statement(stmt).unwrap();
        assert_eq!(parsed.copybook_name, "MYBOOK");
        assert!(parsed.replacements.is_empty());
    }

    #[test]
    fn test_parse_copy_with_in() {
        let preprocessor = Preprocessor::new(test_config(), SourceFormat::Fixed);
        let stmt = "COPY MYBOOK IN MYLIB.";
        let parsed = preprocessor.parse_copy_statement(stmt).unwrap();
        assert_eq!(parsed.copybook_name, "MYBOOK");
    }

    #[test]
    fn test_parse_copy_with_replacing() {
        let preprocessor = Preprocessor::new(test_config(), SourceFormat::Fixed);
        let stmt = "COPY MYBOOK REPLACING ==:PREFIX:== BY ==WS-==.";
        let parsed = preprocessor.parse_copy_statement(stmt).unwrap();
        assert_eq!(parsed.copybook_name, "MYBOOK");
        assert_eq!(parsed.replacements.len(), 1);
        assert_eq!(parsed.replacements[0].from, ":PREFIX:");
        assert_eq!(parsed.replacements[0].to, "WS-");
    }

    #[test]
    fn test_parse_replacing_multiple() {
        let preprocessor = Preprocessor::new(test_config(), SourceFormat::Fixed);
        let text = "==:A:== BY ==X== ==:B:== BY ==Y==";
        let replacements = preprocessor.parse_replacing(text);
        assert_eq!(replacements.len(), 2);
        assert_eq!(replacements[0].from, ":A:");
        assert_eq!(replacements[0].to, "X");
        assert_eq!(replacements[1].from, ":B:");
        assert_eq!(replacements[1].to, "Y");
    }

    #[test]
    fn test_preprocess_no_copy() {
        let mut preprocessor = Preprocessor::new(test_config(), SourceFormat::Fixed);
        let source = "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.\n";
        let result = preprocessor.preprocess(source).unwrap();
        assert_eq!(result, source);
    }

    #[test]
    fn test_comment_passthrough() {
        let mut preprocessor = Preprocessor::new(test_config(), SourceFormat::Fixed);
        let source = "      * This is a comment\n       DATA DIVISION.\n";
        let result = preprocessor.preprocess(source).unwrap();
        assert_eq!(result, source);
    }

    #[test]
    fn test_commented_copy_ignored() {
        let mut preprocessor = Preprocessor::new(test_config(), SourceFormat::Fixed);
        // Commented COPY statement should be passed through unchanged
        let source = "      *COPY DFHATTR.\n";
        let result = preprocessor.preprocess(source).unwrap();
        assert_eq!(result, source);
    }
}
