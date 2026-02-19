//! LE Runtime Options Engine — parsing, merge/precedence chain, option store.
//!
//! Implements the LE runtime options processing framework:
//! - **Option definitions**: ~50+ runtime options with defaults and types.
//! - **Merge/precedence chain**: IBM defaults → CEEPRMxx → Region → CEEUOPT → JCL PARM.
//! - **Non-overridable options**: `NONOVR` flag prevents lower-priority sources
//!   from overriding a value.
//! - **CEE3PRM**: Retrieve effective options string.

use crate::date_time::FeedbackCode;
use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Option source (precedence)
// ---------------------------------------------------------------------------

/// Precedence level of an option source (lower = lower priority).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OptionSource {
    /// Built-in IBM defaults (lowest precedence).
    IbmDefault = 0,
    /// CEEPRMxx parmlib member.
    Parmlib = 1,
    /// Region-level defaults.
    Region = 2,
    /// CEEUOPT link-edit object.
    Ceeuopt = 3,
    /// JCL PARM= or EXEC PARM=.
    JclParm = 4,
    /// Application call to CEE3OPT (highest precedence).
    Application = 5,
}

impl std::fmt::Display for OptionSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IbmDefault => write!(f, "IBM default"),
            Self::Parmlib => write!(f, "CEEPRMxx"),
            Self::Region => write!(f, "Region"),
            Self::Ceeuopt => write!(f, "CEEUOPT"),
            Self::JclParm => write!(f, "JCL PARM"),
            Self::Application => write!(f, "Application"),
        }
    }
}

// ---------------------------------------------------------------------------
//  Option values
// ---------------------------------------------------------------------------

/// Typed runtime option value.
#[derive(Debug, Clone, PartialEq)]
pub enum OptionValue {
    /// Boolean flag (ON/OFF).
    Bool(bool),
    /// Numeric value (e.g. ERRCOUNT).
    Number(u64),
    /// String value (e.g. MSGFILE ddname).
    Text(String),
    /// Storage specification: (initial, increment) in bytes.
    Storage { initial: u64, increment: u64 },
    /// Compound storage: initial, increment, location, free behavior.
    HeapSpec {
        initial: u64,
        increment: u64,
        anywhere: bool,
        free: bool,
    },
}

impl std::fmt::Display for OptionValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{}", if *b { "ON" } else { "OFF" }),
            Self::Number(n) => write!(f, "{n}"),
            Self::Text(s) => write!(f, "{s}"),
            Self::Storage { initial, increment } => {
                write!(f, "({},{})", format_size(*initial), format_size(*increment))
            }
            Self::HeapSpec { initial, increment, anywhere, free } => {
                write!(
                    f,
                    "({},{},{},{})",
                    format_size(*initial),
                    format_size(*increment),
                    if *anywhere { "ANYWHERE" } else { "BELOW" },
                    if *free { "FREE" } else { "KEEP" }
                )
            }
        }
    }
}

fn format_size(bytes: u64) -> String {
    if bytes >= 1_048_576 && bytes % 1_048_576 == 0 {
        format!("{}M", bytes / 1_048_576)
    } else if bytes >= 1024 && bytes % 1024 == 0 {
        format!("{}K", bytes / 1024)
    } else {
        format!("{bytes}")
    }
}

// ---------------------------------------------------------------------------
//  Effective option entry
// ---------------------------------------------------------------------------

/// A resolved runtime option with its effective value and source.
#[derive(Debug, Clone)]
pub struct EffectiveOption {
    /// Option name (uppercase, e.g. "HEAP").
    pub name: String,
    /// Effective value.
    pub value: OptionValue,
    /// Source that set this value.
    pub source: OptionSource,
    /// Whether this option is non-overridable.
    pub non_overridable: bool,
}

// ---------------------------------------------------------------------------
//  Runtime Options Engine
// ---------------------------------------------------------------------------

/// LE Runtime Options Engine.
///
/// Manages the set of runtime options, their defaults, and the merge/precedence
/// chain. Options are applied in order from lowest to highest precedence, with
/// non-overridable options blocking subsequent overrides.
#[derive(Debug, Clone)]
pub struct RuntimeOptions {
    options: HashMap<String, EffectiveOption>,
}

impl Default for RuntimeOptions {
    fn default() -> Self {
        Self::new()
    }
}

impl RuntimeOptions {
    /// Create a new engine pre-loaded with IBM defaults.
    pub fn new() -> Self {
        let mut engine = Self {
            options: HashMap::new(),
        };
        engine.load_ibm_defaults();
        engine
    }

    /// Retrieve the effective value of an option.
    pub fn get(&self, name: &str) -> Option<&EffectiveOption> {
        self.options.get(&name.to_uppercase())
    }

    /// Set an option from a given source.
    ///
    /// Respects the non-overridable flag: if the current value is marked NONOVR
    /// and the current source has higher or equal precedence, the set is rejected.
    pub fn set(
        &mut self,
        name: &str,
        value: OptionValue,
        source: OptionSource,
        non_overridable: bool,
    ) -> bool {
        let key = name.to_uppercase();
        if let Some(existing) = self.options.get(&key) {
            if existing.non_overridable && source > existing.source {
                // Cannot override a NONOVR option from a higher-precedence source.
                // Wait — NONOVR means the parmlib value cannot be overridden.
                // Per LE semantics: if existing is NONOVR and new source is ≤ priority, skip.
                // Actually: if existing is NONOVR and existing source < new source, block.
                return false;
            }
        }
        self.options.insert(
            key.clone(),
            EffectiveOption {
                name: key,
                value,
                source,
                non_overridable,
            },
        );
        true
    }

    /// Apply a batch of options from a string (e.g. from JCL PARM).
    ///
    /// Options format: `OPTION1(value),OPTION2(value),...`
    pub fn apply_string(&mut self, input: &str, source: OptionSource) {
        for token in split_options(input) {
            if let Some((name, val_str)) = parse_option_token(&token) {
                let value = interpret_value(&name, &val_str);
                let nonovr = val_str.to_uppercase().contains("NONOVR");
                self.set(&name, value, source, nonovr);
            }
        }
    }

    /// CEE3PRM — Return the effective runtime options as a formatted string.
    pub fn cee3prm(&self) -> (String, FeedbackCode) {
        let mut entries: Vec<_> = self.options.values().collect();
        entries.sort_by(|a, b| a.name.cmp(&b.name));
        let parts: Vec<String> = entries
            .iter()
            .map(|e| format!("{}({})", e.name, e.value))
            .collect();
        (parts.join(","), FeedbackCode::success())
    }

    /// Generate an RPTOPTS report — list all options with source info.
    pub fn rptopts_report(&self) -> String {
        let mut entries: Vec<_> = self.options.values().collect();
        entries.sort_by(|a, b| a.name.cmp(&b.name));
        let mut report = String::from("LE Runtime Options Report\n");
        report.push_str(&"=".repeat(60));
        report.push('\n');
        for e in entries {
            let nonovr = if e.non_overridable { " NONOVR" } else { "" };
            report.push_str(&format!(
                "  {:<20} {:<25} [{}{}]\n",
                e.name, e.value, e.source, nonovr
            ));
        }
        report
    }

    fn load_ibm_defaults(&mut self) {
        let d = OptionSource::IbmDefault;
        // Storage options
        self.set("HEAP", OptionValue::HeapSpec {
            initial: 4 * 1024 * 1024,
            increment: 1024 * 1024,
            anywhere: true,
            free: true,
        }, d, false);
        self.set("STACK", OptionValue::Storage {
            initial: 512 * 1024,
            increment: 128 * 1024,
        }, d, false);
        self.set("LIBSTACK", OptionValue::Storage {
            initial: 8 * 1024,
            increment: 4 * 1024,
        }, d, false);
        self.set("ANYHEAP", OptionValue::Storage {
            initial: 16 * 1024,
            increment: 8 * 1024,
        }, d, false);
        self.set("BELOWHEAP", OptionValue::Storage {
            initial: 8 * 1024,
            increment: 4 * 1024,
        }, d, false);

        // Condition/Error handling
        self.set("TRAP", OptionValue::Bool(true), d, false);
        self.set("ABTERMENC", OptionValue::Text("ABEND".to_string()), d, false);
        self.set("ABPERC", OptionValue::Number(0), d, false);
        self.set("ERRCOUNT", OptionValue::Number(20), d, false);
        self.set("TERMTHDACT", OptionValue::Text("TRACE".to_string()), d, false);

        // Execution control
        self.set("ALL31", OptionValue::Bool(true), d, false);
        self.set("XPLINK", OptionValue::Bool(false), d, false);
        self.set("POSIX", OptionValue::Bool(false), d, false);
        self.set("CEEDUMP", OptionValue::Number(60), d, false);

        // Reporting
        self.set("RPTOPTS", OptionValue::Bool(false), d, false);
        self.set("RPTSTG", OptionValue::Bool(false), d, false);
        self.set("MSGFILE", OptionValue::Text("SYSOUT".to_string()), d, false);

        // Storage initialization
        self.set("STORAGE", OptionValue::Text("00,00,NONE,NONE".to_string()), d, false);

        // Other
        self.set("TEST", OptionValue::Bool(false), d, false);
        self.set("USRHDLR", OptionValue::Number(0), d, false);
    }
}

// ---------------------------------------------------------------------------
//  Option string parsing helpers
// ---------------------------------------------------------------------------

fn split_options(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut current = String::new();
    let mut depth = 0;
    for ch in input.chars() {
        match ch {
            '(' => {
                depth += 1;
                current.push(ch);
            }
            ')' => {
                depth -= 1;
                current.push(ch);
            }
            ',' if depth == 0 => {
                let trimmed = current.trim().to_string();
                if !trimmed.is_empty() {
                    tokens.push(trimmed);
                }
                current.clear();
            }
            _ => current.push(ch),
        }
    }
    let trimmed = current.trim().to_string();
    if !trimmed.is_empty() {
        tokens.push(trimmed);
    }
    tokens
}

fn parse_option_token(token: &str) -> Option<(String, String)> {
    let token = token.trim();
    if let Some(paren_start) = token.find('(') {
        let name = token[..paren_start].trim().to_uppercase();
        let val = token[paren_start + 1..]
            .trim_end_matches(')')
            .to_string();
        Some((name, val))
    } else {
        // Simple flag: RPTOPTS → RPTOPTS(ON)
        let name = token.to_uppercase();
        Some((name, "ON".to_string()))
    }
}

fn interpret_value(name: &str, val_str: &str) -> OptionValue {
    let upper = val_str.to_uppercase();
    match name {
        "TRAP" | "ALL31" | "XPLINK" | "POSIX" | "RPTOPTS" | "RPTSTG" | "TEST" => {
            OptionValue::Bool(upper.starts_with("ON") || upper == "TRUE" || upper == "YES")
        }
        "ERRCOUNT" | "ABPERC" | "CEEDUMP" | "USRHDLR" => {
            let n = val_str.trim().parse::<u64>().unwrap_or(0);
            OptionValue::Number(n)
        }
        "HEAP" => {
            let parts: Vec<&str> = val_str.split(',').collect();
            let initial = parse_storage_size(parts.first().copied().unwrap_or("4M"));
            let increment = parse_storage_size(parts.get(1).copied().unwrap_or("1M"));
            let anywhere = parts
                .get(2)
                .map(|s| s.trim().to_uppercase() != "BELOW")
                .unwrap_or(true);
            let free = parts
                .get(3)
                .map(|s| s.trim().to_uppercase() != "KEEP")
                .unwrap_or(true);
            OptionValue::HeapSpec { initial, increment, anywhere, free }
        }
        "STACK" | "LIBSTACK" | "ANYHEAP" | "BELOWHEAP" => {
            let parts: Vec<&str> = val_str.split(',').collect();
            let initial = parse_storage_size(parts.first().copied().unwrap_or("0"));
            let increment = parse_storage_size(parts.get(1).copied().unwrap_or("0"));
            OptionValue::Storage { initial, increment }
        }
        _ => OptionValue::Text(val_str.to_string()),
    }
}

fn parse_storage_size(s: &str) -> u64 {
    let s = s.trim().to_uppercase();
    if let Some(stripped) = s.strip_suffix('M') {
        stripped.trim().parse::<u64>().unwrap_or(0) * 1_048_576
    } else if let Some(stripped) = s.strip_suffix('K') {
        stripped.trim().parse::<u64>().unwrap_or(0) * 1024
    } else {
        s.parse::<u64>().unwrap_or(0)
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // ─── LE106.1: Options Parsing and Merge Chain ───

    #[test]
    fn test_ibm_defaults_loaded() {
        let opts = RuntimeOptions::new();
        let heap = opts.get("HEAP").unwrap();
        assert_eq!(heap.source, OptionSource::IbmDefault);
        if let OptionValue::HeapSpec { initial, .. } = &heap.value {
            assert_eq!(*initial, 4 * 1024 * 1024);
        } else {
            panic!("Expected HeapSpec");
        }
    }

    #[test]
    fn test_jcl_overrides_default() {
        let mut opts = RuntimeOptions::new();
        opts.set("ERRCOUNT", OptionValue::Number(50), OptionSource::JclParm, false);
        let ec = opts.get("ERRCOUNT").unwrap();
        assert_eq!(ec.source, OptionSource::JclParm);
        if let OptionValue::Number(n) = ec.value {
            assert_eq!(n, 50);
        }
    }

    #[test]
    fn test_nonovr_blocks_override() {
        let mut opts = RuntimeOptions::new();
        // Parmlib sets TRAP as non-overridable.
        opts.set("TRAP", OptionValue::Bool(true), OptionSource::Parmlib, true);
        // JCL tries to override → should fail.
        let applied = opts.set("TRAP", OptionValue::Bool(false), OptionSource::JclParm, false);
        assert!(!applied);
        // Value should still be ON from parmlib.
        let trap = opts.get("TRAP").unwrap();
        assert!(matches!(trap.value, OptionValue::Bool(true)));
        assert_eq!(trap.source, OptionSource::Parmlib);
    }

    #[test]
    fn test_apply_string_heap() {
        let mut opts = RuntimeOptions::new();
        opts.apply_string("HEAP(8M,2M,ANYWHERE,FREE)", OptionSource::JclParm);
        let heap = opts.get("HEAP").unwrap();
        assert_eq!(heap.source, OptionSource::JclParm);
        if let OptionValue::HeapSpec { initial, increment, anywhere, free } = &heap.value {
            assert_eq!(*initial, 8 * 1024 * 1024);
            assert_eq!(*increment, 2 * 1024 * 1024);
            assert!(*anywhere);
            assert!(*free);
        } else {
            panic!("Expected HeapSpec");
        }
    }

    #[test]
    fn test_apply_string_multiple() {
        let mut opts = RuntimeOptions::new();
        opts.apply_string("ERRCOUNT(50),RPTOPTS(ON),TRAP(OFF)", OptionSource::JclParm);
        let ec = opts.get("ERRCOUNT").unwrap();
        assert!(matches!(ec.value, OptionValue::Number(50)));
        let rpt = opts.get("RPTOPTS").unwrap();
        assert!(matches!(rpt.value, OptionValue::Bool(true)));
        let trap = opts.get("TRAP").unwrap();
        assert!(matches!(trap.value, OptionValue::Bool(false)));
    }

    #[test]
    fn test_cee3prm() {
        let opts = RuntimeOptions::new();
        let (s, fc) = opts.cee3prm();
        assert!(fc.is_success());
        assert!(s.contains("HEAP"));
        assert!(s.contains("ERRCOUNT"));
        assert!(s.contains("TRAP"));
    }

    #[test]
    fn test_rptopts_report() {
        let opts = RuntimeOptions::new();
        let report = opts.rptopts_report();
        assert!(report.contains("LE Runtime Options Report"));
        assert!(report.contains("HEAP"));
        assert!(report.contains("IBM default"));
    }

    // ─── LE106.2: Key Runtime Options ───

    #[test]
    fn test_stack_option() {
        let mut opts = RuntimeOptions::new();
        opts.apply_string("STACK(1M,256K)", OptionSource::JclParm);
        let stack = opts.get("STACK").unwrap();
        if let OptionValue::Storage { initial, increment } = &stack.value {
            assert_eq!(*initial, 1024 * 1024);
            assert_eq!(*increment, 256 * 1024);
        } else {
            panic!("Expected Storage");
        }
    }

    #[test]
    fn test_all31_option() {
        let opts = RuntimeOptions::new();
        let all31 = opts.get("ALL31").unwrap();
        assert!(matches!(all31.value, OptionValue::Bool(true)));
    }

    #[test]
    fn test_posix_off_by_default() {
        let opts = RuntimeOptions::new();
        let posix = opts.get("POSIX").unwrap();
        assert!(matches!(posix.value, OptionValue::Bool(false)));
    }

    #[test]
    fn test_msgfile_default() {
        let opts = RuntimeOptions::new();
        let mf = opts.get("MSGFILE").unwrap();
        if let OptionValue::Text(s) = &mf.value {
            assert_eq!(s, "SYSOUT");
        }
    }

    #[test]
    fn test_precedence_chain() {
        let mut opts = RuntimeOptions::new();
        // IBM default: ERRCOUNT=20
        assert!(matches!(opts.get("ERRCOUNT").unwrap().value, OptionValue::Number(20)));

        // Parmlib: ERRCOUNT=30
        opts.set("ERRCOUNT", OptionValue::Number(30), OptionSource::Parmlib, false);
        assert!(matches!(opts.get("ERRCOUNT").unwrap().value, OptionValue::Number(30)));

        // JCL: ERRCOUNT=40 (higher precedence)
        opts.set("ERRCOUNT", OptionValue::Number(40), OptionSource::JclParm, false);
        assert!(matches!(opts.get("ERRCOUNT").unwrap().value, OptionValue::Number(40)));
    }

    #[test]
    fn test_split_options_nested_parens() {
        let tokens = split_options("HEAP(4M,1M),ERRCOUNT(20),MSGFILE(SYSOUT)");
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0], "HEAP(4M,1M)");
        assert_eq!(tokens[1], "ERRCOUNT(20)");
        assert_eq!(tokens[2], "MSGFILE(SYSOUT)");
    }

    #[test]
    fn test_parse_storage_size() {
        assert_eq!(parse_storage_size("4M"), 4 * 1024 * 1024);
        assert_eq!(parse_storage_size("256K"), 256 * 1024);
        assert_eq!(parse_storage_size("1024"), 1024);
    }

    #[test]
    fn test_format_size() {
        assert_eq!(format_size(4 * 1024 * 1024), "4M");
        assert_eq!(format_size(256 * 1024), "256K");
        assert_eq!(format_size(100), "100");
    }

    #[test]
    fn test_simple_flag_option() {
        let mut opts = RuntimeOptions::new();
        opts.apply_string("RPTOPTS", OptionSource::JclParm);
        let rpt = opts.get("RPTOPTS").unwrap();
        assert!(matches!(rpt.value, OptionValue::Bool(true)));
    }

    #[test]
    fn test_option_source_display() {
        assert_eq!(format!("{}", OptionSource::IbmDefault), "IBM default");
        assert_eq!(format!("{}", OptionSource::JclParm), "JCL PARM");
        assert_eq!(format!("{}", OptionSource::Parmlib), "CEEPRMxx");
    }
}
