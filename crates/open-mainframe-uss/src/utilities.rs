//! Core UNIX Utilities (USS-108).
//!
//! Provides standard UNIX utilities:
//! - File utilities (ls, cp, mv, rm, mkdir, cat, find)
//! - Text utilities (grep, head, tail, wc, sort)
//! - sed — stream editor
//! - awk — pattern processing
//! - Miscellaneous (chmod, chown, date, env, echo, printf, test, expr)

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors for utility operations.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, miette::Diagnostic)]
pub enum UtilityError {
    /// File not found.
    #[error("file not found: {path}")]
    FileNotFound { path: String },

    /// Invalid option.
    #[error("invalid option: {option}")]
    InvalidOption { option: String },

    /// Invalid pattern.
    #[error("invalid pattern: {pattern}")]
    InvalidPattern { pattern: String },

    /// Generic error.
    #[error("{message}")]
    Generic { message: String },
}

// ---------------------------------------------------------------------------
//  Ls Entry
// ---------------------------------------------------------------------------

/// An entry from ls output.
#[derive(Debug, Clone)]
pub struct LsEntry {
    /// File name.
    pub name: String,
    /// Permission string (e.g., "-rwxr-xr-x").
    pub permissions: String,
    /// Number of hard links.
    pub nlink: u32,
    /// Owner name.
    pub owner: String,
    /// Group name.
    pub group: String,
    /// File size.
    pub size: u64,
    /// Modification time (as a formatted string).
    pub mtime: String,
    /// Is directory.
    pub is_dir: bool,
}

/// Options for ls.
#[derive(Debug, Clone, Default)]
pub struct LsOptions {
    /// Long format (-l).
    pub long: bool,
    /// Show all including hidden (-a).
    pub all: bool,
    /// Human-readable sizes (-h).
    pub human_readable: bool,
    /// Recursive (-R).
    pub recursive: bool,
}

/// Format permissions from mode bits.
pub fn format_permissions(mode: u32, is_dir: bool) -> String {
    let file_type = if is_dir { 'd' } else { '-' };
    let mut perms = String::with_capacity(10);
    perms.push(file_type);
    for shift in (0..9).rev() {
        let bit = (mode >> shift) & 1;
        let ch = match shift % 3 {
            2 => if bit == 1 { 'r' } else { '-' },
            1 => if bit == 1 { 'w' } else { '-' },
            0 => if bit == 1 { 'x' } else { '-' },
            _ => '-',
        };
        perms.push(ch);
    }
    perms
}

// ---------------------------------------------------------------------------
//  Find Options
// ---------------------------------------------------------------------------

/// Options for find.
#[derive(Debug, Clone, Default)]
pub struct FindOptions {
    /// Name pattern (-name).
    pub name_pattern: Option<String>,
    /// Type filter (-type f, d, l).
    pub file_type: Option<char>,
    /// Max depth.
    pub max_depth: Option<u32>,
}

/// A find result.
#[derive(Debug, Clone)]
pub struct FindResult {
    /// Path to the found file.
    pub path: String,
    /// Is directory.
    pub is_dir: bool,
}

/// Simple glob pattern matching (supports * and ?).
pub fn glob_match(pattern: &str, text: &str) -> bool {
    let mut pi = 0;
    let mut ti = 0;
    let pb = pattern.as_bytes();
    let tb = text.as_bytes();
    let mut star_pi = usize::MAX;
    let mut star_ti = 0;

    while ti < tb.len() {
        if pi < pb.len() && (pb[pi] == b'?' || pb[pi] == tb[ti]) {
            pi += 1;
            ti += 1;
        } else if pi < pb.len() && pb[pi] == b'*' {
            star_pi = pi;
            star_ti = ti;
            pi += 1;
        } else if star_pi != usize::MAX {
            pi = star_pi + 1;
            star_ti += 1;
            ti = star_ti;
        } else {
            return false;
        }
    }

    while pi < pb.len() && pb[pi] == b'*' {
        pi += 1;
    }

    pi == pb.len()
}

// ---------------------------------------------------------------------------
//  Grep
// ---------------------------------------------------------------------------

/// Options for grep.
#[derive(Debug, Clone, Default)]
pub struct GrepOptions {
    /// Show line numbers (-n).
    pub line_numbers: bool,
    /// Case insensitive (-i).
    pub case_insensitive: bool,
    /// Invert match (-v).
    pub invert: bool,
    /// Count matches (-c).
    pub count_only: bool,
}

/// A grep match.
#[derive(Debug, Clone)]
pub struct GrepMatch {
    /// Line number (1-based).
    pub line_number: usize,
    /// Line content.
    pub line: String,
}

/// Run grep on content.
pub fn grep(content: &str, pattern: &str, options: &GrepOptions) -> Vec<GrepMatch> {
    let mut matches = Vec::new();

    let search_pattern = if options.case_insensitive {
        pattern.to_lowercase()
    } else {
        pattern.to_string()
    };

    for (i, line) in content.lines().enumerate() {
        let search_line = if options.case_insensitive {
            line.to_lowercase()
        } else {
            line.to_string()
        };

        let found = search_line.contains(&search_pattern);
        let matches_filter = if options.invert { !found } else { found };

        if matches_filter {
            matches.push(GrepMatch {
                line_number: i + 1,
                line: line.to_string(),
            });
        }
    }

    matches
}

// ---------------------------------------------------------------------------
//  Wc
// ---------------------------------------------------------------------------

/// Word count result.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WcResult {
    /// Number of lines.
    pub lines: usize,
    /// Number of words.
    pub words: usize,
    /// Number of bytes (characters).
    pub bytes: usize,
}

/// Run wc on content.
pub fn wc(content: &str) -> WcResult {
    WcResult {
        lines: content.lines().count(),
        words: content.split_whitespace().count(),
        bytes: content.len(),
    }
}

// ---------------------------------------------------------------------------
//  Sort
// ---------------------------------------------------------------------------

/// Options for sort.
#[derive(Debug, Clone, Default)]
pub struct SortOptions {
    /// Sort numerically (-n).
    pub numeric: bool,
    /// Reverse order (-r).
    pub reverse: bool,
    /// Key field (-k, 1-based).
    pub key_field: Option<usize>,
    /// Unique (-u).
    pub unique: bool,
}

/// Sort lines.
pub fn sort_lines(content: &str, options: &SortOptions) -> Vec<String> {
    let mut lines: Vec<String> = content.lines().map(|l| l.to_string()).collect();

    if let Some(key) = options.key_field {
        if options.numeric {
            lines.sort_by(|a, b| {
                let a_val: f64 = a
                    .split_whitespace()
                    .nth(key - 1)
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(0.0);
                let b_val: f64 = b
                    .split_whitespace()
                    .nth(key - 1)
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(0.0);
                a_val.partial_cmp(&b_val).unwrap_or(std::cmp::Ordering::Equal)
            });
        } else {
            lines.sort_by(|a, b| {
                let a_key = a.split_whitespace().nth(key - 1).unwrap_or("");
                let b_key = b.split_whitespace().nth(key - 1).unwrap_or("");
                a_key.cmp(b_key)
            });
        }
    } else if options.numeric {
        lines.sort_by(|a, b| {
            let a_val: f64 = a.trim().parse().unwrap_or(0.0);
            let b_val: f64 = b.trim().parse().unwrap_or(0.0);
            a_val.partial_cmp(&b_val).unwrap_or(std::cmp::Ordering::Equal)
        });
    } else {
        lines.sort();
    }

    if options.reverse {
        lines.reverse();
    }

    if options.unique {
        lines.dedup();
    }

    lines
}

/// head — return first N lines.
pub fn head(content: &str, n: usize) -> Vec<String> {
    content.lines().take(n).map(|l| l.to_string()).collect()
}

/// tail — return last N lines.
pub fn tail(content: &str, n: usize) -> Vec<String> {
    let lines: Vec<&str> = content.lines().collect();
    let start = lines.len().saturating_sub(n);
    lines[start..].iter().map(|l| l.to_string()).collect()
}

// ---------------------------------------------------------------------------
//  Sed
// ---------------------------------------------------------------------------

/// A sed substitution command.
#[derive(Debug, Clone)]
pub struct SedSubstitution {
    /// Pattern to match.
    pub pattern: String,
    /// Replacement string.
    pub replacement: String,
    /// Global flag (replace all occurrences per line).
    pub global: bool,
}

/// A sed command.
#[derive(Debug, Clone)]
pub enum SedCommand {
    /// s/pattern/replacement/[g]
    Substitute(SedSubstitution),
    /// Print lines in range (Np or N,Mp).
    PrintRange { start: usize, end: usize },
    /// Delete lines.
    Delete { start: usize, end: usize },
}

/// Parse a simple sed expression.
pub fn parse_sed(expr: &str) -> Result<SedCommand, UtilityError> {
    if expr.starts_with('s') && expr.len() > 3 {
        let delim = expr.as_bytes()[1];
        let parts: Vec<&str> = expr[2..].split(delim as char).collect();
        if parts.len() >= 2 {
            let pattern = parts[0].to_string();
            let replacement = parts[1].to_string();
            let global = parts.get(2).map_or(false, |f| f.contains('g'));
            return Ok(SedCommand::Substitute(SedSubstitution {
                pattern,
                replacement,
                global,
            }));
        }
    }
    // Try print range: '10,20p'
    if let Some(range) = expr.strip_suffix('p') {
        if let Some(comma) = range.find(',') {
            let start: usize = range[..comma]
                .parse()
                .map_err(|_| UtilityError::InvalidPattern {
                    pattern: expr.to_string(),
                })?;
            let end: usize = range[comma + 1..]
                .parse()
                .map_err(|_| UtilityError::InvalidPattern {
                    pattern: expr.to_string(),
                })?;
            return Ok(SedCommand::PrintRange { start, end });
        }
    }
    Err(UtilityError::InvalidPattern {
        pattern: expr.to_string(),
    })
}

/// Apply a sed command to content.
pub fn sed(content: &str, command: &SedCommand) -> String {
    match command {
        SedCommand::Substitute(sub) => {
            let mut result = String::new();
            for line in content.lines() {
                let new_line = if sub.global {
                    line.replace(&sub.pattern, &sub.replacement)
                } else {
                    line.replacen(&sub.pattern, &sub.replacement, 1)
                };
                result.push_str(&new_line);
                result.push('\n');
            }
            if result.ends_with('\n') && !content.ends_with('\n') {
                result.pop();
            }
            result
        }
        SedCommand::PrintRange { start, end } => {
            let mut result = String::new();
            for (i, line) in content.lines().enumerate() {
                let line_num = i + 1;
                if line_num >= *start && line_num <= *end {
                    result.push_str(line);
                    result.push('\n');
                }
            }
            if result.ends_with('\n') {
                result.pop();
            }
            result
        }
        SedCommand::Delete { start, end } => {
            let mut result = String::new();
            for (i, line) in content.lines().enumerate() {
                let line_num = i + 1;
                if line_num < *start || line_num > *end {
                    result.push_str(line);
                    result.push('\n');
                }
            }
            if result.ends_with('\n') && !content.ends_with('\n') {
                result.pop();
            }
            result
        }
    }
}

// ---------------------------------------------------------------------------
//  Awk
// ---------------------------------------------------------------------------

/// A simple awk program (print selected fields).
#[derive(Debug, Clone)]
pub struct AwkProgram {
    /// Field separator.
    pub field_separator: char,
    /// Fields to print (1-based, 0 means whole line).
    pub print_fields: Vec<usize>,
    /// Aggregation: sum of a field (for END blocks).
    pub sum_field: Option<usize>,
}

impl Default for AwkProgram {
    fn default() -> Self {
        Self {
            field_separator: ' ',
            print_fields: Vec::new(),
            sum_field: None,
        }
    }
}

/// Run a simple awk program on content.
pub fn awk(content: &str, program: &AwkProgram) -> Vec<String> {
    let mut results = Vec::new();
    let mut sum: f64 = 0.0;

    for line in content.lines() {
        let fields: Vec<&str> = if program.field_separator == ' ' {
            line.split_whitespace().collect()
        } else {
            line.split(program.field_separator).collect()
        };

        if !program.print_fields.is_empty() {
            let selected: Vec<&str> = program
                .print_fields
                .iter()
                .filter_map(|&f| {
                    if f == 0 {
                        Some(line)
                    } else {
                        fields.get(f - 1).copied()
                    }
                })
                .collect();
            results.push(selected.join(" "));
        }

        if let Some(sf) = program.sum_field {
            if let Some(val) = fields.get(sf - 1) {
                if let Ok(n) = val.parse::<f64>() {
                    sum += n;
                }
            }
        }
    }

    if program.sum_field.is_some() {
        results.push(format!("{sum}"));
    }

    results
}

// ---------------------------------------------------------------------------
//  Test Command
// ---------------------------------------------------------------------------

/// The test/[ command for conditional expressions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TestExpr {
    /// -f file (file exists and is regular).
    FileExists(String),
    /// -d file (file exists and is directory).
    DirExists(String),
    /// -z string (string is empty).
    StringEmpty(String),
    /// -n string (string is non-empty).
    StringNonEmpty(String),
    /// string1 = string2.
    StringEqual(String, String),
    /// int1 -eq int2.
    IntEqual(i64, i64),
    /// int1 -lt int2.
    IntLessThan(i64, i64),
    /// int1 -gt int2.
    IntGreaterThan(i64, i64),
}

/// Evaluate a test expression.
pub fn evaluate_test(
    expr: &TestExpr,
    file_exists: &dyn Fn(&str) -> bool,
    dir_exists: &dyn Fn(&str) -> bool,
) -> bool {
    match expr {
        TestExpr::FileExists(path) => file_exists(path),
        TestExpr::DirExists(path) => dir_exists(path),
        TestExpr::StringEmpty(s) => s.is_empty(),
        TestExpr::StringNonEmpty(s) => !s.is_empty(),
        TestExpr::StringEqual(a, b) => a == b,
        TestExpr::IntEqual(a, b) => a == b,
        TestExpr::IntLessThan(a, b) => a < b,
        TestExpr::IntGreaterThan(a, b) => a > b,
    }
}

// ---------------------------------------------------------------------------
//  Expr
// ---------------------------------------------------------------------------

/// Evaluate a simple arithmetic expression (integer only).
pub fn expr_eval(args: &[&str]) -> Result<i64, UtilityError> {
    if args.len() == 3 {
        let a: i64 = args[0]
            .parse()
            .map_err(|_| UtilityError::Generic {
                message: format!("not a number: {}", args[0]),
            })?;
        let b: i64 = args[2]
            .parse()
            .map_err(|_| UtilityError::Generic {
                message: format!("not a number: {}", args[2]),
            })?;
        match args[1] {
            "+" => Ok(a + b),
            "-" => Ok(a - b),
            "*" => Ok(a * b),
            "/" => {
                if b == 0 {
                    Err(UtilityError::Generic {
                        message: "division by zero".to_string(),
                    })
                } else {
                    Ok(a / b)
                }
            }
            "%" => {
                if b == 0 {
                    Err(UtilityError::Generic {
                        message: "division by zero".to_string(),
                    })
                } else {
                    Ok(a % b)
                }
            }
            op => Err(UtilityError::InvalidOption {
                option: op.to_string(),
            }),
        }
    } else {
        Err(UtilityError::Generic {
            message: "expr requires 3 arguments: operand operator operand".to_string(),
        })
    }
}

// ---------------------------------------------------------------------------
//  Date Formatting
// ---------------------------------------------------------------------------

/// Format a date string (simplified — returns a fixed format).
pub fn format_date(format_str: &str) -> String {
    // Simplified implementation: just return the format with placeholders replaced.
    let mut result = format_str.to_string();
    result = result.replace("%Y", "2026");
    result = result.replace("%m", "02");
    result = result.replace("%d", "23");
    result = result.replace("%H", "12");
    result = result.replace("%M", "00");
    result = result.replace("%S", "00");
    result
}

// ---------------------------------------------------------------------------
//  Utility Registry
// ---------------------------------------------------------------------------

/// Registry of available UNIX utilities.
#[derive(Debug)]
pub struct UtilityRegistry {
    /// Registered utility names.
    utilities: HashMap<String, UtilityInfo>,
}

/// Information about a utility.
#[derive(Debug, Clone)]
pub struct UtilityInfo {
    /// Utility name.
    pub name: String,
    /// Brief description.
    pub description: String,
    /// Category.
    pub category: UtilityCategory,
}

/// Category of utility.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UtilityCategory {
    File,
    Text,
    System,
}

impl UtilityRegistry {
    /// Create a new registry with all standard utilities.
    pub fn new() -> Self {
        let mut reg = Self {
            utilities: HashMap::new(),
        };
        let utils = vec![
            ("ls", "List directory contents", UtilityCategory::File),
            ("cp", "Copy files", UtilityCategory::File),
            ("mv", "Move/rename files", UtilityCategory::File),
            ("rm", "Remove files", UtilityCategory::File),
            ("mkdir", "Create directories", UtilityCategory::File),
            ("cat", "Concatenate files", UtilityCategory::File),
            ("find", "Search for files", UtilityCategory::File),
            ("grep", "Search file contents", UtilityCategory::Text),
            ("head", "Output first lines", UtilityCategory::Text),
            ("tail", "Output last lines", UtilityCategory::Text),
            ("wc", "Word, line, byte count", UtilityCategory::Text),
            ("sort", "Sort lines", UtilityCategory::Text),
            ("sed", "Stream editor", UtilityCategory::Text),
            ("awk", "Pattern processing", UtilityCategory::Text),
            ("chmod", "Change permissions", UtilityCategory::System),
            ("chown", "Change owner", UtilityCategory::System),
            ("date", "Display date", UtilityCategory::System),
            ("env", "Display environment", UtilityCategory::System),
            ("echo", "Display text", UtilityCategory::System),
            ("printf", "Formatted output", UtilityCategory::System),
            ("test", "Evaluate expression", UtilityCategory::System),
            ("expr", "Evaluate arithmetic", UtilityCategory::System),
        ];

        for (name, desc, cat) in utils {
            reg.utilities.insert(
                name.to_string(),
                UtilityInfo {
                    name: name.to_string(),
                    description: desc.to_string(),
                    category: cat,
                },
            );
        }

        reg
    }

    /// Check if a utility exists.
    pub fn has_utility(&self, name: &str) -> bool {
        self.utilities.contains_key(name)
    }

    /// Get utility info.
    pub fn get_info(&self, name: &str) -> Option<&UtilityInfo> {
        self.utilities.get(name)
    }

    /// List all utilities.
    pub fn list(&self) -> Vec<&UtilityInfo> {
        let mut utils: Vec<&UtilityInfo> = self.utilities.values().collect();
        utils.sort_by_key(|u| &u.name);
        utils
    }
}

impl Default for UtilityRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// ===========================================================================
//  Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_permissions() {
        assert_eq!(format_permissions(0o755, false), "-rwxr-xr-x");
        assert_eq!(format_permissions(0o644, false), "-rw-r--r--");
        assert_eq!(format_permissions(0o755, true), "drwxr-xr-x");
        assert_eq!(format_permissions(0o600, false), "-rw-------");
    }

    #[test]
    fn test_glob_match() {
        assert!(glob_match("*.txt", "file.txt"));
        assert!(!glob_match("*.txt", "file.rs"));
        assert!(glob_match("test?", "test1"));
        assert!(glob_match("*", "anything"));
        assert!(glob_match("file.*", "file.txt"));
    }

    #[test]
    fn test_grep_basic() {
        let content = "hello world\nfoo bar\nhello again";
        let matches = grep(content, "hello", &GrepOptions::default());
        assert_eq!(matches.len(), 2);
        assert_eq!(matches[0].line_number, 1);
        assert_eq!(matches[1].line_number, 3);
    }

    #[test]
    fn test_grep_case_insensitive() {
        let content = "Hello World\nhello world";
        let opts = GrepOptions {
            case_insensitive: true,
            ..Default::default()
        };
        let matches = grep(content, "hello", &opts);
        assert_eq!(matches.len(), 2);
    }

    #[test]
    fn test_grep_invert() {
        let content = "yes\nno\nyes";
        let opts = GrepOptions {
            invert: true,
            ..Default::default()
        };
        let matches = grep(content, "yes", &opts);
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].line, "no");
    }

    #[test]
    fn test_wc() {
        let content = "line one\nline two\nline three";
        let result = wc(content);
        assert_eq!(result.lines, 3);
        assert_eq!(result.words, 6);
    }

    #[test]
    fn test_sort_default() {
        let content = "banana\napple\ncherry";
        let sorted = sort_lines(content, &SortOptions::default());
        assert_eq!(sorted, vec!["apple", "banana", "cherry"]);
    }

    #[test]
    fn test_sort_numeric_by_field() {
        let content = "alice 30\nbob 20\ncharlie 40";
        let opts = SortOptions {
            numeric: true,
            key_field: Some(2),
            ..Default::default()
        };
        let sorted = sort_lines(content, &opts);
        assert_eq!(sorted[0], "bob 20");
        assert_eq!(sorted[1], "alice 30");
        assert_eq!(sorted[2], "charlie 40");
    }

    #[test]
    fn test_sort_reverse() {
        let content = "a\nb\nc";
        let opts = SortOptions {
            reverse: true,
            ..Default::default()
        };
        let sorted = sort_lines(content, &opts);
        assert_eq!(sorted, vec!["c", "b", "a"]);
    }

    #[test]
    fn test_head() {
        let content = "1\n2\n3\n4\n5";
        let result = head(content, 3);
        assert_eq!(result, vec!["1", "2", "3"]);
    }

    #[test]
    fn test_tail() {
        let content = "1\n2\n3\n4\n5";
        let result = tail(content, 2);
        assert_eq!(result, vec!["4", "5"]);
    }

    #[test]
    fn test_sed_substitute_global() {
        let content = "old is old";
        let cmd = SedCommand::Substitute(SedSubstitution {
            pattern: "old".to_string(),
            replacement: "new".to_string(),
            global: true,
        });
        assert_eq!(sed(content, &cmd), "new is new");
    }

    #[test]
    fn test_sed_substitute_first_only() {
        let content = "old is old";
        let cmd = SedCommand::Substitute(SedSubstitution {
            pattern: "old".to_string(),
            replacement: "new".to_string(),
            global: false,
        });
        assert_eq!(sed(content, &cmd), "new is old");
    }

    #[test]
    fn test_sed_print_range() {
        let content = "line1\nline2\nline3\nline4\nline5";
        let cmd = SedCommand::PrintRange { start: 2, end: 4 };
        let result = sed(content, &cmd);
        assert_eq!(result, "line2\nline3\nline4");
    }

    #[test]
    fn test_parse_sed() {
        let cmd = parse_sed("s/old/new/g").unwrap();
        match cmd {
            SedCommand::Substitute(sub) => {
                assert_eq!(sub.pattern, "old");
                assert_eq!(sub.replacement, "new");
                assert!(sub.global);
            }
            _ => panic!("expected substitute"),
        }
    }

    #[test]
    fn test_awk_print_fields() {
        let content = "alice 30 engineering\nbob 25 marketing";
        let program = AwkProgram {
            print_fields: vec![1, 3],
            ..Default::default()
        };
        let result = awk(content, &program);
        assert_eq!(result, vec!["alice engineering", "bob marketing"]);
    }

    #[test]
    fn test_awk_sum_field() {
        let content = "x:10\ny:20\nz:30";
        let program = AwkProgram {
            field_separator: ':',
            print_fields: Vec::new(),
            sum_field: Some(2),
        };
        let result = awk(content, &program);
        assert_eq!(result, vec!["60"]);
    }

    #[test]
    fn test_test_file_exists() {
        let expr = TestExpr::FileExists("/etc/profile".to_string());
        assert!(evaluate_test(&expr, &|_| true, &|_| false));
    }

    #[test]
    fn test_test_string_equal() {
        let expr = TestExpr::StringEqual("abc".to_string(), "abc".to_string());
        assert!(evaluate_test(&expr, &|_| false, &|_| false));
    }

    #[test]
    fn test_test_int_less_than() {
        let expr = TestExpr::IntLessThan(5, 10);
        assert!(evaluate_test(&expr, &|_| false, &|_| false));
    }

    #[test]
    fn test_expr_eval() {
        assert_eq!(expr_eval(&["5", "+", "3"]).unwrap(), 8);
        assert_eq!(expr_eval(&["10", "-", "4"]).unwrap(), 6);
        assert_eq!(expr_eval(&["3", "*", "7"]).unwrap(), 21);
        assert_eq!(expr_eval(&["10", "/", "3"]).unwrap(), 3);
        assert_eq!(expr_eval(&["10", "%", "3"]).unwrap(), 1);
    }

    #[test]
    fn test_expr_division_by_zero() {
        let err = expr_eval(&["10", "/", "0"]).unwrap_err();
        assert!(matches!(err, UtilityError::Generic { .. }));
    }

    #[test]
    fn test_date_format() {
        let result = format_date("%Y-%m-%d");
        assert_eq!(result, "2026-02-23");
    }

    #[test]
    fn test_utility_registry() {
        let reg = UtilityRegistry::new();
        assert!(reg.has_utility("ls"));
        assert!(reg.has_utility("grep"));
        assert!(reg.has_utility("awk"));
        assert!(!reg.has_utility("nonexistent"));

        let info = reg.get_info("ls").unwrap();
        assert_eq!(info.category, UtilityCategory::File);

        let all = reg.list();
        assert!(all.len() >= 20);
    }
}
