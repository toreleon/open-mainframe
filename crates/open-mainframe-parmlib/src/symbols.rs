//! System symbol definition and substitution.
//!
//! Provides [`IeaSymConfig`] to parse IEASYMxx members, [`StaticSymbols`] for
//! built-in system symbols (`&SYSNAME.`, etc.), and [`SymbolEngine`] to resolve
//! `&symbol.` references in text strings.

use std::collections::HashMap;

use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

/// Errors from the symbol subsystem.
#[derive(Debug, Error)]
pub enum SymbolError {
    /// Too many symbol definitions (z/OS limit is 200).
    #[error("symbol table full: maximum 200 symbols reached")]
    TableFull,

    /// A parse error in the IEASYMxx member.
    #[error("IEASYMxx parse error: {0}")]
    ParseError(String),

    /// Recursive substitution exceeded the maximum depth.
    #[error("recursive symbol substitution exceeded depth {0}")]
    RecursionLimit(usize),
}

/// Convenience result alias.
pub type Result<T> = std::result::Result<T, SymbolError>;

/// Maximum number of user-defined symbols (z/OS limit).
const MAX_SYMBOLS: usize = 200;

/// Maximum recursion depth for nested symbol resolution.
const MAX_DEPTH: usize = 10;

// ---------------------------------------------------------------------------
// IeaSymConfig — SYS-118 Story 1
// ---------------------------------------------------------------------------

/// Parsed IEASYMxx member — symbol definitions.
///
/// Each line may contain `SYMDEF(&NAME.='VALUE')`.  Up to 200 symbols are
/// supported, matching the z/OS limit.
#[derive(Debug, Clone, Default)]
pub struct IeaSymConfig {
    /// Source member name.
    pub member_name: String,
    /// Symbol definitions in order of appearance.
    pub symbols: Vec<(String, String)>,
}

impl IeaSymConfig {
    /// Parse an IEASYMxx member.
    ///
    /// Expected format per line:
    /// ```text
    /// SYMDEF(&NAME.='VALUE')
    /// ```
    pub fn parse(name: &str, content: &str) -> Result<Self> {
        let mut cfg = IeaSymConfig {
            member_name: name.to_string(),
            ..Default::default()
        };
        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('*') {
                continue;
            }
            let upper = line.to_uppercase();
            if !upper.starts_with("SYMDEF") {
                continue;
            }
            // Extract the parenthesised content
            let open = line.find('(').ok_or_else(|| {
                SymbolError::ParseError(format!("missing '(' in SYMDEF: {line}"))
            })?;
            let close = line.rfind(')').ok_or_else(|| {
                SymbolError::ParseError(format!("missing ')' in SYMDEF: {line}"))
            })?;
            if close <= open {
                return Err(SymbolError::ParseError(format!(
                    "malformed SYMDEF: {line}"
                )));
            }
            let inner = &line[open + 1..close];
            // inner should be &NAME.='VALUE'
            let eq = inner.find("='").ok_or_else(|| {
                SymbolError::ParseError(format!("missing =\"'\" in SYMDEF: {line}"))
            })?;
            let sym_name = inner[..eq].trim();
            // Must start with & and end with .
            if !sym_name.starts_with('&') || !sym_name.ends_with('.') {
                return Err(SymbolError::ParseError(format!(
                    "symbol name must be &NAME.: got {sym_name}"
                )));
            }
            let sym_name_clean = &sym_name[1..sym_name.len() - 1]; // strip & and .
            let value_part = &inner[eq + 2..]; // skip ='
            let value = value_part.trim_end_matches('\'');

            if cfg.symbols.len() >= MAX_SYMBOLS {
                return Err(SymbolError::TableFull);
            }
            cfg.symbols
                .push((sym_name_clean.to_uppercase(), value.to_string()));
        }
        Ok(cfg)
    }
}

// ---------------------------------------------------------------------------
// StaticSymbols — SYS-118 Story 3
// ---------------------------------------------------------------------------

/// Static (built-in) system symbols set at IPL time.
///
/// These correspond to z/OS static system symbols such as `&SYSNAME.`,
/// `&SYSPLEX.`, `&SYSDATE.`, `&SYSTIME.`, and `&SYSCLONE.`.
#[derive(Debug, Clone)]
pub struct StaticSymbols {
    /// System name (`&SYSNAME.`).
    pub sysname: String,
    /// Sysplex name (`&SYSPLEX.`).
    pub sysplex: String,
    /// Current date (`&SYSDATE.`) in `MM/DD/YY` format.
    pub sysdate: String,
    /// Current time (`&SYSTIME.`) in `HH.MM.SS` format.
    pub systime: String,
    /// Two-character system clone abbreviation (`&SYSCLONE.`).
    pub sysclone: String,
}

impl Default for StaticSymbols {
    fn default() -> Self {
        Self {
            sysname: "SYS1".to_string(),
            sysplex: "LOCAL".to_string(),
            sysdate: "01/01/00".to_string(),
            systime: "00.00.00".to_string(),
            sysclone: "S1".to_string(),
        }
    }
}

impl StaticSymbols {
    /// Create static symbols with the given system and sysplex names.
    pub fn new(sysname: &str, sysplex: &str, sysclone: &str) -> Self {
        Self {
            sysname: sysname.to_string(),
            sysplex: sysplex.to_string(),
            sysclone: sysclone.to_string(),
            ..Default::default()
        }
    }

    /// Convert static symbols into a key-value map.
    pub fn as_map(&self) -> HashMap<String, String> {
        let mut map = HashMap::new();
        map.insert("SYSNAME".to_string(), self.sysname.clone());
        map.insert("SYSPLEX".to_string(), self.sysplex.clone());
        map.insert("SYSDATE".to_string(), self.sysdate.clone());
        map.insert("SYSTIME".to_string(), self.systime.clone());
        map.insert("SYSCLONE".to_string(), self.sysclone.clone());
        map
    }
}

// ---------------------------------------------------------------------------
// SymbolEngine — SYS-118 Story 2
// ---------------------------------------------------------------------------

/// Symbol substitution engine.
///
/// Resolves `&SYMBOL.` references in text, supporting nested/recursive
/// resolution up to a maximum depth of 10.  Undefined symbols are left
/// as-is.  Escaped `&&` is replaced with a literal `&`.
#[derive(Debug, Clone)]
pub struct SymbolEngine {
    /// Combined symbol table (static + user-defined).
    symbols: HashMap<String, String>,
}

impl Default for SymbolEngine {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolEngine {
    /// Create an empty symbol engine.
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    /// Create an engine pre-loaded with the given static symbols.
    pub fn with_static(statics: &StaticSymbols) -> Self {
        Self {
            symbols: statics.as_map(),
        }
    }

    /// Define (or redefine) a symbol.
    pub fn define(&mut self, name: &str, value: &str) {
        self.symbols
            .insert(name.to_uppercase(), value.to_string());
    }

    /// Load all symbols from a parsed [`IeaSymConfig`].
    pub fn load_config(&mut self, config: &IeaSymConfig) {
        for (name, value) in &config.symbols {
            self.define(name, value);
        }
    }

    /// Return the current symbol table.
    pub fn symbols(&self) -> &HashMap<String, String> {
        &self.symbols
    }

    /// Perform symbol substitution on the input text.
    ///
    /// - `&NAME.` references are resolved from the symbol table.
    /// - Undefined symbols are left as-is (including the `&` and `.`).
    /// - `&&` is treated as an escaped ampersand and produces a single `&`.
    /// - Nested references (a symbol value containing `&OTHER.`) are
    ///   resolved recursively up to depth 10.
    pub fn substitute(&self, text: &str) -> String {
        self.substitute_recursive(text, 0)
    }

    /// Recursive implementation with depth tracking.
    fn substitute_recursive(&self, text: &str, depth: usize) -> String {
        if depth >= MAX_DEPTH {
            return text.to_string();
        }

        let mut result = String::with_capacity(text.len());
        let chars: Vec<char> = text.chars().collect();
        let len = chars.len();
        let mut i = 0;

        while i < len {
            if chars[i] == '&' {
                // Check for escaped &&
                if i + 1 < len && chars[i + 1] == '&' {
                    result.push('&');
                    i += 2;
                    continue;
                }
                // Try to parse a symbol reference &NAME.
                if let Some((sym_name, end_pos)) = self.extract_symbol_ref(&chars, i) {
                    if let Some(value) = self.symbols.get(&sym_name.to_uppercase()) {
                        // Recursively resolve the value
                        let resolved = self.substitute_recursive(value, depth + 1);
                        result.push_str(&resolved);
                    } else {
                        // Undefined symbol: leave as-is
                        result.push_str(&text[i..end_pos]);
                    }
                    i = end_pos;
                } else {
                    result.push('&');
                    i += 1;
                }
            } else {
                result.push(chars[i]);
                i += 1;
            }
        }

        result
    }

    /// Extract a symbol reference starting at position `start` in `chars`.
    ///
    /// Returns `(name, end_position)` where end is past the closing `.`.
    fn extract_symbol_ref(&self, chars: &[char], start: usize) -> Option<(String, usize)> {
        // start should be '&'
        if chars[start] != '&' {
            return None;
        }
        let mut i = start + 1;
        let mut name = String::new();
        while i < chars.len() {
            if chars[i] == '.' {
                if name.is_empty() {
                    return None;
                }
                return Some((name, i + 1));
            }
            if chars[i].is_ascii_alphanumeric() || chars[i] == '_' {
                name.push(chars[i]);
                i += 1;
            } else {
                break;
            }
        }
        None // no closing dot found
    }
}

// ---------------------------------------------------------------------------
// Tests — SYS-118 Story 4
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -- IeaSymConfig tests -------------------------------------------------

    #[test]
    fn parse_ieasym_basic() {
        let content = "\
* Symbol definitions
SYMDEF(&SYSNAME.='PROD1')
SYMDEF(&ENV.='PRODUCTION')
";
        let cfg = IeaSymConfig::parse("IEASYM00", content).unwrap();
        assert_eq!(cfg.symbols.len(), 2);
        assert_eq!(cfg.symbols[0], ("SYSNAME".to_string(), "PROD1".to_string()));
        assert_eq!(
            cfg.symbols[1],
            ("ENV".to_string(), "PRODUCTION".to_string())
        );
    }

    #[test]
    fn parse_ieasym_max_exceeded() {
        let mut content = String::new();
        for i in 0..201 {
            content.push_str(&format!("SYMDEF(&SYM{i:03}.='VAL{i}')\n"));
        }
        let err = IeaSymConfig::parse("IEASYM00", &content).unwrap_err();
        assert!(matches!(err, SymbolError::TableFull));
    }

    #[test]
    fn parse_ieasym_malformed_no_ampersand() {
        let content = "SYMDEF(NAME.='VALUE')";
        let err = IeaSymConfig::parse("IEASYM00", content).unwrap_err();
        assert!(matches!(err, SymbolError::ParseError(_)));
    }

    // -- SymbolEngine substitution tests ------------------------------------

    #[test]
    fn substitute_basic_symbols() {
        let mut engine = SymbolEngine::new();
        engine.define("SYSNAME", "PROD1");
        engine.define("ENV", "PRODUCTION");

        assert_eq!(
            engine.substitute("System &SYSNAME. is &ENV."),
            "System PROD1 is PRODUCTION"
        );
    }

    #[test]
    fn substitute_undefined_left_as_is() {
        let engine = SymbolEngine::new();
        assert_eq!(
            engine.substitute("Value is &UNKNOWN."),
            "Value is &UNKNOWN."
        );
    }

    #[test]
    fn substitute_escaped_ampersand() {
        let mut engine = SymbolEngine::new();
        engine.define("X", "VAL");
        assert_eq!(engine.substitute("A && B"), "A & B");
        assert_eq!(engine.substitute("&&X. is not a symbol"), "&X. is not a symbol");
    }

    #[test]
    fn substitute_nested_recursive() {
        let mut engine = SymbolEngine::new();
        engine.define("OUTER", "&INNER.");
        engine.define("INNER", "RESOLVED");

        assert_eq!(engine.substitute("&OUTER."), "RESOLVED");
    }

    #[test]
    fn substitute_recursive_depth_limit() {
        let mut engine = SymbolEngine::new();
        // Create a cycle: A -> B -> A -> ...
        engine.define("A", "&B.");
        engine.define("B", "&A.");
        // Should stop after MAX_DEPTH without panic
        let result = engine.substitute("&A.");
        // The result is indeterminate (partially resolved), but we must not panic
        assert!(!result.is_empty());
    }

    #[test]
    fn substitute_no_closing_dot() {
        let engine = SymbolEngine::new();
        // &FOO without a trailing dot — leave the & as a literal character
        assert_eq!(engine.substitute("&FOO bar"), "&FOO bar");
    }

    #[test]
    fn static_symbols_map() {
        let statics = StaticSymbols::new("SYS1", "PLEX1", "S1");
        let engine = SymbolEngine::with_static(&statics);
        assert_eq!(
            engine.substitute("Name=&SYSNAME. Plex=&SYSPLEX. Clone=&SYSCLONE."),
            "Name=SYS1 Plex=PLEX1 Clone=S1"
        );
    }

    #[test]
    fn engine_load_config() {
        let content = "\
SYMDEF(&APP.='MYAPP')
SYMDEF(&VER.='V2')
";
        let cfg = IeaSymConfig::parse("IEASYM00", content).unwrap();
        let mut engine = SymbolEngine::new();
        engine.load_config(&cfg);
        assert_eq!(engine.substitute("&APP. &VER."), "MYAPP V2");
    }

    #[test]
    fn substitute_case_insensitive() {
        let mut engine = SymbolEngine::new();
        engine.define("name", "SYS1");
        // Symbols should match case-insensitively (define lowercases, ref uppercases)
        assert_eq!(engine.substitute("&NAME."), "SYS1");
    }

    #[test]
    fn substitute_empty_string() {
        let engine = SymbolEngine::new();
        assert_eq!(engine.substitute(""), "");
    }

    #[test]
    fn substitute_no_symbols() {
        let engine = SymbolEngine::new();
        assert_eq!(
            engine.substitute("plain text with no symbols"),
            "plain text with no symbols"
        );
    }
}
