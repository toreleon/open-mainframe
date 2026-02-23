//! PARMLIB framework and core member parsers.
//!
//! Provides the [`ParmlibConcat`] search path, a [`ParserRegistry`] for
//! dispatching member parsing, and parsers for the fundamental system
//! initialisation members: IEASYSxx, LNKLSTxx, PROGxx, CONSOLxx, COMMNDxx.

use std::collections::HashMap;
use std::path::PathBuf;

use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

/// Errors produced by PARMLIB member operations.
#[derive(Debug, Error)]
pub enum ParmlibError {
    /// The requested member was not found in any concatenated directory.
    #[error("member '{0}' not found in PARMLIB concatenation")]
    MemberNotFound(String),

    /// A parse error in a PARMLIB member.
    #[error("parse error in member '{member}': {detail}")]
    ParseError {
        /// Member name.
        member: String,
        /// Human-readable detail.
        detail: String,
    },

    /// No parser registered for the given member prefix.
    #[error("no parser registered for prefix '{0}'")]
    NoParser(String),

    /// I/O error while reading a member.
    #[error("I/O error: {0}")]
    Io(String),
}

/// Convenience result alias.
pub type Result<T> = std::result::Result<T, ParmlibError>;

// ---------------------------------------------------------------------------
// ParmlibConcat — SYS-117 Story 1
// ---------------------------------------------------------------------------

/// Concatenated PARMLIB search path.
///
/// Searches an ordered list of directories for named members, analogous to
/// the z/OS PARMLIB concatenation (SYS1.PARMLIB, etc.).
#[derive(Debug, Clone)]
pub struct ParmlibConcat {
    directories: Vec<PathBuf>,
}

impl ParmlibConcat {
    /// Create a new PARMLIB concatenation from an ordered list of directories.
    pub fn new(directories: Vec<PathBuf>) -> Self {
        Self { directories }
    }

    /// Add a directory to the end of the concatenation.
    pub fn add_directory(&mut self, dir: PathBuf) {
        self.directories.push(dir);
    }

    /// Return the ordered list of directories.
    pub fn directories(&self) -> &[PathBuf] {
        &self.directories
    }

    /// Search the concatenation for a member by name.
    ///
    /// Returns the full path to the first matching file, or `None` if the
    /// member does not exist in any directory.
    pub fn find_member(&self, name: &str) -> Option<PathBuf> {
        for dir in &self.directories {
            let candidate = dir.join(name);
            if candidate.is_file() {
                return Some(candidate);
            }
        }
        None
    }

    /// List all unique member names across the concatenation.
    ///
    /// Earlier directories take precedence (matching z/OS behaviour).
    pub fn list_members(&self) -> Vec<String> {
        let mut seen = std::collections::HashSet::new();
        let mut result = Vec::new();
        for dir in &self.directories {
            if let Ok(entries) = std::fs::read_dir(dir) {
                for entry in entries.flatten() {
                    if entry.file_type().map(|ft| ft.is_file()).unwrap_or(false) {
                        let name = entry.file_name().to_string_lossy().to_string();
                        if seen.insert(name.clone()) {
                            result.push(name);
                        }
                    }
                }
            }
        }
        result.sort();
        result
    }
}

// ---------------------------------------------------------------------------
// ParserRegistry — SYS-117 Story 2
// ---------------------------------------------------------------------------

/// Result of parsing a PARMLIB member.
#[derive(Debug, Clone)]
pub enum ParsedMember {
    /// Parsed IEASYSxx member.
    IeaSys(IeaSysConfig),
    /// Parsed LNKLSTxx member.
    LnkLst(LnkLstConfig),
    /// Parsed PROGxx member.
    Prog(ProgConfig),
    /// Parsed CONSOLxx member.
    Consol(ConsolConfig),
    /// Parsed COMMNDxx member.
    Commnd(CommndConfig),
    /// A member parsed by an external (delegated) parser.
    External(String),
}

/// A boxed parser function that takes `(name, content)` and returns a parsed member.
type ParserFn = Box<dyn Fn(&str, &str) -> Result<ParsedMember> + Send + Sync>;

/// Registry that maps member-name prefixes to parser functions.
///
/// When `parse_member` is called the registry extracts the prefix (all
/// alphabetic characters before the two-character suffix), looks up the
/// registered parser, and invokes it.
pub struct ParserRegistry {
    parsers: HashMap<String, ParserFn>,
}

impl std::fmt::Debug for ParserRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ParserRegistry")
            .field("prefixes", &self.parsers.keys().collect::<Vec<_>>())
            .finish()
    }
}

impl Default for ParserRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl ParserRegistry {
    /// Create an empty parser registry.
    pub fn new() -> Self {
        Self {
            parsers: HashMap::new(),
        }
    }

    /// Create a registry pre-loaded with the built-in PARMLIB parsers.
    pub fn with_builtins() -> Self {
        let mut reg = Self::new();
        reg.register("IEASYS", |name, content| {
            Ok(ParsedMember::IeaSys(IeaSysConfig::parse(name, content)?))
        });
        reg.register("LNKLST", |name, content| {
            Ok(ParsedMember::LnkLst(LnkLstConfig::parse(name, content)?))
        });
        reg.register("PROG", |name, content| {
            Ok(ParsedMember::Prog(ProgConfig::parse(name, content)?))
        });
        reg.register("CONSOL", |name, content| {
            Ok(ParsedMember::Consol(ConsolConfig::parse(name, content)?))
        });
        reg.register("COMMND", |name, content| {
            Ok(ParsedMember::Commnd(CommndConfig::parse(name, content)?))
        });
        reg
    }

    /// Register a parser for the given member prefix.
    pub fn register<F>(&mut self, member_prefix: &str, parser_fn: F)
    where
        F: Fn(&str, &str) -> Result<ParsedMember> + Send + Sync + 'static,
    {
        self.parsers
            .insert(member_prefix.to_uppercase(), Box::new(parser_fn));
    }

    /// Extract the prefix from a member name (e.g. `IEASYS00` -> `IEASYS`).
    fn extract_prefix(name: &str) -> &str {
        let end = name
            .char_indices()
            .rev()
            .take_while(|(_, c)| c.is_ascii_alphanumeric() && !c.is_ascii_alphabetic())
            .last()
            .map(|(i, _)| i)
            .unwrap_or(name.len());
        &name[..end]
    }

    /// Parse a member by name and content, dispatching to the registered parser.
    pub fn parse_member(&self, name: &str, content: &str) -> Result<ParsedMember> {
        let upper = name.to_uppercase();
        let prefix = Self::extract_prefix(&upper);
        if let Some(parser) = self.parsers.get(prefix) {
            parser(name, content)
        } else {
            Err(ParmlibError::NoParser(prefix.to_string()))
        }
    }
}

// ---------------------------------------------------------------------------
// IeaSysConfig — SYS-117 Story 3
// ---------------------------------------------------------------------------

/// Parsed IEASYSxx system initialisation member.
///
/// Reads `KEY(VALUE)` and `KEY=VALUE` parameter pairs that control the
/// fundamental system initialisation options.
#[derive(Debug, Clone, Default)]
pub struct IeaSysConfig {
    /// The SYSPLEX name.
    pub sysplex: Option<String>,
    /// The LNKLST suffix (xx).
    pub lnklst_suffix: Option<String>,
    /// The SMF suffix (xx).
    pub smf_suffix: Option<String>,
    /// Maximum number of address spaces.
    pub maxuser: Option<u32>,
    /// CLOCK setting.
    pub clock: Option<String>,
    /// Raw key-value pairs for any other parameters.
    pub parameters: HashMap<String, String>,
    /// Source member name.
    pub member_name: String,
}

impl IeaSysConfig {
    /// Parse the content of an IEASYSxx member.
    pub fn parse(name: &str, content: &str) -> Result<Self> {
        let mut cfg = IeaSysConfig {
            member_name: name.to_string(),
            ..Default::default()
        };

        for line in content.lines() {
            let line = line.trim();
            // skip blank lines and comments (lines starting with *)
            if line.is_empty() || line.starts_with('*') {
                continue;
            }
            // split on commas to handle multiple parameters per line
            for token in line.split(',') {
                let token = token.trim();
                if token.is_empty() {
                    continue;
                }
                let (key, value) = Self::parse_param(token);
                let upper_key = key.to_uppercase();
                match upper_key.as_str() {
                    "SYSPLEX" => cfg.sysplex = Some(value.to_string()),
                    "LNKLST" => cfg.lnklst_suffix = Some(value.to_string()),
                    "SMF" => cfg.smf_suffix = Some(value.to_string()),
                    "MAXUSER" => {
                        cfg.maxuser = Some(value.parse::<u32>().map_err(|_| {
                            ParmlibError::ParseError {
                                member: name.to_string(),
                                detail: format!("invalid MAXUSER value: {value}"),
                            }
                        })?);
                    }
                    "CLOCK" => cfg.clock = Some(value.to_string()),
                    _ => {
                        cfg.parameters.insert(upper_key, value.to_string());
                    }
                }
            }
        }
        Ok(cfg)
    }

    /// Parse a single `KEY(VALUE)` or `KEY=VALUE` token.
    fn parse_param(token: &str) -> (&str, &str) {
        // KEY(VALUE) form
        if let Some(open) = token.find('(') {
            let key = &token[..open];
            let rest = &token[open + 1..];
            let value = rest.trim_end_matches(')');
            return (key.trim(), value.trim());
        }
        // KEY=VALUE form
        if let Some(eq) = token.find('=') {
            return (token[..eq].trim(), token[eq + 1..].trim());
        }
        // Bare keyword
        (token.trim(), "")
    }
}

// ---------------------------------------------------------------------------
// LnkLstConfig — SYS-117 Story 4
// ---------------------------------------------------------------------------

/// A single LNKLST library entry.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LnkLstEntry {
    /// Dataset name.
    pub dsname: String,
    /// Optional volume serial number.
    pub volume: Option<String>,
}

/// Parsed LNKLSTxx member — ordered list of linklist library entries.
#[derive(Debug, Clone, Default)]
pub struct LnkLstConfig {
    /// Source member name.
    pub member_name: String,
    /// Library entries in concatenation order.
    pub entries: Vec<LnkLstEntry>,
}

impl LnkLstConfig {
    /// Parse a LNKLSTxx member.
    ///
    /// Expected format per line:
    /// ```text
    /// dsname
    /// dsname,volser
    /// ```
    pub fn parse(name: &str, content: &str) -> Result<Self> {
        let mut cfg = LnkLstConfig {
            member_name: name.to_string(),
            ..Default::default()
        };
        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('*') {
                continue;
            }
            let parts: Vec<&str> = line.splitn(2, ',').collect();
            let dsname = parts[0].trim().to_string();
            let volume = parts.get(1).map(|v| v.trim().to_string()).filter(|v| !v.is_empty());
            cfg.entries.push(LnkLstEntry { dsname, volume });
        }
        Ok(cfg)
    }
}

// ---------------------------------------------------------------------------
// ProgConfig — SYS-117 Story 5
// ---------------------------------------------------------------------------

/// An APF-authorised library entry.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ApfEntry {
    /// Dataset name.
    pub dsname: String,
    /// Optional volume serial.
    pub volume: Option<String>,
}

/// A linklist update entry.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinklistUpdate {
    /// Dataset name.
    pub dsname: String,
    /// Optional volume serial.
    pub volume: Option<String>,
}

/// An LPA (Link Pack Area) addition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LpaEntry {
    /// Dataset name.
    pub dsname: String,
    /// Optional volume serial.
    pub volume: Option<String>,
}

/// Parsed PROGxx member — APF authorisations, linklist, and LPA changes.
#[derive(Debug, Clone, Default)]
pub struct ProgConfig {
    /// Source member name.
    pub member_name: String,
    /// APF-authorised libraries.
    pub apf_entries: Vec<ApfEntry>,
    /// Linklist additions.
    pub linklist_updates: Vec<LinklistUpdate>,
    /// LPA additions.
    pub lpa_entries: Vec<LpaEntry>,
}

impl ProgConfig {
    /// Parse a PROGxx member.
    ///
    /// Recognises:
    /// - `APF ADD DSNAME(x) VOLUME(y)`
    /// - `LNKLST ADD DSNAME(x) VOLUME(y)`
    /// - `LPA ADD DSNAME(x) VOLUME(y)`
    pub fn parse(name: &str, content: &str) -> Result<Self> {
        let mut cfg = ProgConfig {
            member_name: name.to_string(),
            ..Default::default()
        };
        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('*') {
                continue;
            }
            let upper = line.to_uppercase();
            let dsname = Self::extract_paren(&upper, "DSNAME");
            let volume = Self::extract_paren(&upper, "VOLUME");

            if let Some(ds) = dsname {
                if upper.starts_with("APF") {
                    cfg.apf_entries.push(ApfEntry {
                        dsname: ds,
                        volume,
                    });
                } else if upper.starts_with("LNKLST") {
                    cfg.linklist_updates.push(LinklistUpdate {
                        dsname: ds,
                        volume,
                    });
                } else if upper.starts_with("LPA") {
                    cfg.lpa_entries.push(LpaEntry {
                        dsname: ds,
                        volume,
                    });
                }
            }
        }
        Ok(cfg)
    }

    /// Extract a parenthesised value e.g. `DSNAME(SYS1.LINKLIB)` -> `SYS1.LINKLIB`.
    fn extract_paren(text: &str, keyword: &str) -> Option<String> {
        let kw_upper = keyword.to_uppercase();
        if let Some(pos) = text.find(&kw_upper) {
            let rest = &text[pos + kw_upper.len()..];
            if rest.starts_with('(') {
                let end = rest.find(')')?;
                return Some(rest[1..end].trim().to_string());
            }
        }
        None
    }
}

// ---------------------------------------------------------------------------
// ConsolConfig — SYS-117 Story 6
// ---------------------------------------------------------------------------

/// Console authority level.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConsoleAuthority {
    /// Master console authority.
    Master,
    /// All command authority.
    All,
    /// System command authority.
    Sys,
    /// I/O command authority.
    Io,
    /// Console (basic) command authority.
    Cons,
}

impl ConsoleAuthority {
    /// Parse an authority keyword.
    pub fn parse(s: &str) -> Result<Self> {
        match s.trim().to_uppercase().as_str() {
            "MASTER" => Ok(Self::Master),
            "ALL" => Ok(Self::All),
            "SYS" => Ok(Self::Sys),
            "IO" => Ok(Self::Io),
            "CONS" => Ok(Self::Cons),
            other => Err(ParmlibError::ParseError {
                member: String::new(),
                detail: format!("unknown console authority: {other}"),
            }),
        }
    }
}

/// A single console definition.
#[derive(Debug, Clone)]
pub struct ConsoleDefinition {
    /// Console name.
    pub name: String,
    /// Authority level.
    pub authority: ConsoleAuthority,
    /// Routing codes for this console.
    pub routcodes: Vec<u16>,
}

/// Parsed CONSOLxx member.
#[derive(Debug, Clone, Default)]
pub struct ConsolConfig {
    /// Source member name.
    pub member_name: String,
    /// Console definitions.
    pub consoles: Vec<ConsoleDefinition>,
}

impl ConsolConfig {
    /// Parse a CONSOLxx member.
    ///
    /// Expected format:
    /// ```text
    /// CONSOLE NAME(name) AUTH(level) ROUTCODE(1,2,3)
    /// ```
    pub fn parse(name: &str, content: &str) -> Result<Self> {
        let mut cfg = ConsolConfig {
            member_name: name.to_string(),
            ..Default::default()
        };
        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('*') {
                continue;
            }
            let upper = line.to_uppercase();
            if !upper.starts_with("CONSOLE") {
                continue;
            }
            let cname = Self::extract_paren(&upper, "NAME").ok_or_else(|| {
                ParmlibError::ParseError {
                    member: name.to_string(),
                    detail: "CONSOLE missing NAME".to_string(),
                }
            })?;
            let auth_str = Self::extract_paren(&upper, "AUTH").unwrap_or_else(|| "CONS".to_string());
            let authority = ConsoleAuthority::parse(&auth_str).map_err(|e| {
                ParmlibError::ParseError {
                    member: name.to_string(),
                    detail: format!("{e}"),
                }
            })?;
            let routcodes = Self::extract_paren(&upper, "ROUTCODE")
                .map(|rc| {
                    rc.split(',')
                        .filter_map(|s| s.trim().parse::<u16>().ok())
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();

            cfg.consoles.push(ConsoleDefinition {
                name: cname,
                authority,
                routcodes,
            });
        }
        Ok(cfg)
    }

    /// Extract a parenthesised value.
    fn extract_paren(text: &str, keyword: &str) -> Option<String> {
        let kw = keyword.to_uppercase();
        let pos = text.find(&kw)?;
        let rest = &text[pos + kw.len()..];
        if rest.starts_with('(') {
            let end = rest.find(')')?;
            return Some(rest[1..end].trim().to_string());
        }
        None
    }
}

// ---------------------------------------------------------------------------
// CommndConfig — SYS-117 Story 7
// ---------------------------------------------------------------------------

/// Parsed COMMNDxx member — list of IPL commands to execute during init.
#[derive(Debug, Clone, Default)]
pub struct CommndConfig {
    /// Source member name.
    pub member_name: String,
    /// Commands in the order they should be executed.
    pub commands: Vec<String>,
}

impl CommndConfig {
    /// Parse a COMMNDxx member.
    ///
    /// Each non-blank, non-comment line is an operator command.
    pub fn parse(name: &str, content: &str) -> Result<Self> {
        let mut cfg = CommndConfig {
            member_name: name.to_string(),
            ..Default::default()
        };
        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('*') {
                continue;
            }
            cfg.commands.push(line.to_string());
        }
        Ok(cfg)
    }
}

// ---------------------------------------------------------------------------
// Tests — SYS-117 Story 8
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    // -- ParmlibConcat tests ------------------------------------------------

    #[test]
    fn concat_find_member_first_dir_wins() {
        let tmp = std::env::temp_dir().join("parmlib_test_concat1");
        let dir1 = tmp.join("dir1");
        let dir2 = tmp.join("dir2");
        let _ = fs::create_dir_all(&dir1);
        let _ = fs::create_dir_all(&dir2);
        fs::write(dir1.join("IEASYS00"), "SYSPLEX(PLEX1)").unwrap();
        fs::write(dir2.join("IEASYS00"), "SYSPLEX(PLEX2)").unwrap();
        fs::write(dir2.join("PROG00"), "").unwrap();

        let concat = ParmlibConcat::new(vec![dir1.clone(), dir2.clone()]);
        let found = concat.find_member("IEASYS00").unwrap();
        assert_eq!(found, dir1.join("IEASYS00"));

        // Member only in dir2
        let found2 = concat.find_member("PROG00").unwrap();
        assert_eq!(found2, dir2.join("PROG00"));

        // Not found
        assert!(concat.find_member("NOSUCH").is_none());

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn concat_list_members_deduplicates() {
        let tmp = std::env::temp_dir().join("parmlib_test_concat2");
        let dir1 = tmp.join("d1");
        let dir2 = tmp.join("d2");
        let _ = fs::create_dir_all(&dir1);
        let _ = fs::create_dir_all(&dir2);
        fs::write(dir1.join("AAA"), "").unwrap();
        fs::write(dir1.join("BBB"), "").unwrap();
        fs::write(dir2.join("AAA"), "").unwrap();
        fs::write(dir2.join("CCC"), "").unwrap();

        let concat = ParmlibConcat::new(vec![dir1, dir2]);
        let members = concat.list_members();
        assert_eq!(members, vec!["AAA", "BBB", "CCC"]);

        let _ = fs::remove_dir_all(&tmp);
    }

    // -- IeaSysConfig tests -------------------------------------------------

    #[test]
    fn parse_ieasys_paren_format() {
        let content = "\
* System init parameters
SYSPLEX(PRODPLEX),LNKLST(01),SMF(00)
MAXUSER(500),CLOCK(00)
VATLST(00)
";
        let cfg = IeaSysConfig::parse("IEASYS00", content).unwrap();
        assert_eq!(cfg.sysplex.as_deref(), Some("PRODPLEX"));
        assert_eq!(cfg.lnklst_suffix.as_deref(), Some("01"));
        assert_eq!(cfg.smf_suffix.as_deref(), Some("00"));
        assert_eq!(cfg.maxuser, Some(500));
        assert_eq!(cfg.clock.as_deref(), Some("00"));
        assert_eq!(cfg.parameters.get("VATLST").map(|s| s.as_str()), Some("00"));
    }

    #[test]
    fn parse_ieasys_equal_format() {
        let content = "SYSPLEX=TESTPLEX\nMAXUSER=100";
        let cfg = IeaSysConfig::parse("IEASYS01", content).unwrap();
        assert_eq!(cfg.sysplex.as_deref(), Some("TESTPLEX"));
        assert_eq!(cfg.maxuser, Some(100));
    }

    #[test]
    fn parse_ieasys_bad_maxuser() {
        let content = "MAXUSER(ABC)";
        let err = IeaSysConfig::parse("IEASYS00", content).unwrap_err();
        assert!(matches!(err, ParmlibError::ParseError { .. }));
    }

    // -- LnkLstConfig tests -------------------------------------------------

    #[test]
    fn parse_lnklst_entries() {
        let content = "\
* Linklist
SYS1.LINKLIB
SYS1.MIGLIB,MVSRES
USER.LOADLIB
";
        let cfg = LnkLstConfig::parse("LNKLST00", content).unwrap();
        assert_eq!(cfg.entries.len(), 3);
        assert_eq!(
            cfg.entries[0],
            LnkLstEntry {
                dsname: "SYS1.LINKLIB".to_string(),
                volume: None,
            }
        );
        assert_eq!(
            cfg.entries[1],
            LnkLstEntry {
                dsname: "SYS1.MIGLIB".to_string(),
                volume: Some("MVSRES".to_string()),
            }
        );
    }

    // -- ProgConfig tests ---------------------------------------------------

    #[test]
    fn parse_prog_apf_and_lnklst() {
        let content = "\
* APF list
APF ADD DSNAME(SYS1.LINKLIB) VOLUME(MVSRES)
APF ADD DSNAME(USER.AUTH.LOAD)
LNKLST ADD DSNAME(USER.LNKLST) VOLUME(USR001)
LPA ADD DSNAME(USER.LPA)
";
        let cfg = ProgConfig::parse("PROG00", content).unwrap();
        assert_eq!(cfg.apf_entries.len(), 2);
        assert_eq!(cfg.apf_entries[0].dsname, "SYS1.LINKLIB");
        assert_eq!(cfg.apf_entries[0].volume.as_deref(), Some("MVSRES"));
        assert_eq!(cfg.apf_entries[1].dsname, "USER.AUTH.LOAD");
        assert!(cfg.apf_entries[1].volume.is_none());
        assert_eq!(cfg.linklist_updates.len(), 1);
        assert_eq!(cfg.lpa_entries.len(), 1);
    }

    // -- ConsolConfig tests -------------------------------------------------

    #[test]
    fn parse_consol_definitions() {
        let content = "\
* Consoles
CONSOLE NAME(SYSCON) AUTH(MASTER) ROUTCODE(1,2,3,11)
CONSOLE NAME(OPCON1) AUTH(ALL) ROUTCODE(1,2)
CONSOLE NAME(BASIC) AUTH(CONS)
";
        let cfg = ConsolConfig::parse("CONSOL00", content).unwrap();
        assert_eq!(cfg.consoles.len(), 3);
        assert_eq!(cfg.consoles[0].name, "SYSCON");
        assert_eq!(cfg.consoles[0].authority, ConsoleAuthority::Master);
        assert_eq!(cfg.consoles[0].routcodes, vec![1, 2, 3, 11]);
        assert_eq!(cfg.consoles[1].authority, ConsoleAuthority::All);
        assert_eq!(cfg.consoles[2].authority, ConsoleAuthority::Cons);
        assert!(cfg.consoles[2].routcodes.is_empty());
    }

    #[test]
    fn parse_consol_missing_name_errors() {
        let content = "CONSOLE AUTH(MASTER)";
        let err = ConsolConfig::parse("CONSOL00", content).unwrap_err();
        assert!(matches!(err, ParmlibError::ParseError { .. }));
    }

    // -- CommndConfig tests -------------------------------------------------

    #[test]
    fn parse_commnd_commands() {
        let content = "\
* IPL commands
START JES2
SET SMF=00
START VTAM
";
        let cfg = CommndConfig::parse("COMMND00", content).unwrap();
        assert_eq!(cfg.commands.len(), 3);
        assert_eq!(cfg.commands[0], "START JES2");
        assert_eq!(cfg.commands[1], "SET SMF=00");
    }

    // -- ParserRegistry tests -----------------------------------------------

    #[test]
    fn registry_dispatches_to_correct_parser() {
        let reg = ParserRegistry::with_builtins();
        let result = reg.parse_member("IEASYS00", "SYSPLEX(TEST)").unwrap();
        assert!(matches!(result, ParsedMember::IeaSys(_)));

        let result2 = reg
            .parse_member("COMMND01", "START JES2")
            .unwrap();
        assert!(matches!(result2, ParsedMember::Commnd(_)));
    }

    #[test]
    fn registry_no_parser_error() {
        let reg = ParserRegistry::new();
        let err = reg.parse_member("UNKNOWN00", "content").unwrap_err();
        assert!(matches!(err, ParmlibError::NoParser(_)));
    }

    #[test]
    fn registry_custom_parser() {
        let mut reg = ParserRegistry::new();
        reg.register("MYMBR", |_name, _content| {
            Ok(ParsedMember::External("custom".to_string()))
        });
        let result = reg.parse_member("MYMBR01", "data").unwrap();
        assert!(matches!(result, ParsedMember::External(_)));
    }
}
