//! Subsystem configuration members and delegation.
//!
//! Parsers for IKJTSOxx (TSO configuration) and ALLOCxx (default dataset
//! allocation), plus a [`SubsystemDelegate`] trait that allows external
//! crates (SMF, USS, JES2, RACF) to register their own PARMLIB member
//! parsers.

use std::collections::HashMap;

use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

/// Errors from subsystem configuration processing.
#[derive(Debug, Error)]
pub enum SubsystemError {
    /// Parse error in a subsystem member.
    #[error("parse error in {member}: {detail}")]
    ParseError {
        /// Member name.
        member: String,
        /// Detail message.
        detail: String,
    },

    /// Delegate already registered for this prefix.
    #[error("delegate already registered for prefix '{0}'")]
    DuplicateDelegate(String),

    /// No delegate registered for the given prefix.
    #[error("no delegate for prefix '{0}'")]
    NoDelegate(String),
}

/// Convenience result alias.
pub type Result<T> = std::result::Result<T, SubsystemError>;

// ---------------------------------------------------------------------------
// IkjTsoConfig — SYS-119 Story 1
// ---------------------------------------------------------------------------

/// Parsed IKJTSOxx member — TSO configuration.
///
/// Contains lists of authorised commands, authorised programs,
/// foreground-only commands, and authorised TSO service routines.
#[derive(Debug, Clone, Default)]
pub struct IkjTsoConfig {
    /// Source member name.
    pub member_name: String,
    /// Authorised commands (AUTHCMD section).
    pub authcmd: Vec<String>,
    /// Authorised programs (AUTHPGM section).
    pub authpgm: Vec<String>,
    /// Foreground-only commands (NOTBKGND section).
    pub notbkgnd: Vec<String>,
    /// Authorised TSO service routines (AUTHTSF section).
    pub authtsf: Vec<String>,
}

impl IkjTsoConfig {
    /// Parse an IKJTSOxx member.
    ///
    /// The format uses section headers followed by `NAMES(...)` blocks:
    /// ```text
    /// AUTHCMD NAMES(         +
    ///   RACDCERT             +
    ///   ALTLIB               +
    /// )
    /// ```
    pub fn parse(name: &str, content: &str) -> Result<Self> {
        let mut cfg = IkjTsoConfig {
            member_name: name.to_string(),
            ..Default::default()
        };

        let mut current_section: Option<&str> = None;
        let mut in_names_block = false;

        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('*') {
                continue;
            }

            let upper = line.to_uppercase();
            let trimmed_upper = upper.trim();

            // Check for section headers
            if trimmed_upper.starts_with("AUTHCMD") {
                current_section = Some("AUTHCMD");
                if trimmed_upper.contains("NAMES(") {
                    in_names_block = true;
                    // Extract names on same line
                    Self::extract_inline_names(line, &mut cfg, "AUTHCMD");
                    if trimmed_upper.contains(')') && !trimmed_upper.ends_with("NAMES(") {
                        in_names_block = false;
                    }
                }
                continue;
            }
            if trimmed_upper.starts_with("AUTHPGM") {
                current_section = Some("AUTHPGM");
                if trimmed_upper.contains("NAMES(") {
                    in_names_block = true;
                    Self::extract_inline_names(line, &mut cfg, "AUTHPGM");
                    if trimmed_upper.contains(')') && !trimmed_upper.ends_with("NAMES(") {
                        in_names_block = false;
                    }
                }
                continue;
            }
            if trimmed_upper.starts_with("NOTBKGND") {
                current_section = Some("NOTBKGND");
                if trimmed_upper.contains("NAMES(") {
                    in_names_block = true;
                    Self::extract_inline_names(line, &mut cfg, "NOTBKGND");
                    if trimmed_upper.contains(')') && !trimmed_upper.ends_with("NAMES(") {
                        in_names_block = false;
                    }
                }
                continue;
            }
            if trimmed_upper.starts_with("AUTHTSF") {
                current_section = Some("AUTHTSF");
                if trimmed_upper.contains("NAMES(") {
                    in_names_block = true;
                    Self::extract_inline_names(line, &mut cfg, "AUTHTSF");
                    if trimmed_upper.contains(')') && !trimmed_upper.ends_with("NAMES(") {
                        in_names_block = false;
                    }
                }
                continue;
            }

            // Inside a NAMES block — collect name tokens
            if in_names_block {
                if let Some(section) = current_section {
                    // Remove continuation character and closing paren
                    let cleaned = line
                        .trim_end_matches('+')
                        .trim_end_matches(')')
                        .trim();
                    // If line starts with NAMES(, strip that prefix
                    let cleaned = if cleaned.to_uppercase().starts_with("NAMES(") {
                        &cleaned[6..]
                    } else {
                        cleaned
                    };
                    for token in cleaned.split_whitespace() {
                        let token = token.trim_matches(|c: char| !c.is_ascii_alphanumeric() && c != '@' && c != '#' && c != '$');
                        if !token.is_empty() {
                            let target = match section {
                                "AUTHCMD" => &mut cfg.authcmd,
                                "AUTHPGM" => &mut cfg.authpgm,
                                "NOTBKGND" => &mut cfg.notbkgnd,
                                "AUTHTSF" => &mut cfg.authtsf,
                                _ => continue,
                            };
                            target.push(token.to_string());
                        }
                    }
                    if line.contains(')') {
                        in_names_block = false;
                    }
                }
            }
        }

        Ok(cfg)
    }

    /// Extract names that appear on the same line as the section header.
    fn extract_inline_names(line: &str, cfg: &mut IkjTsoConfig, section: &str) {
        let upper = line.to_uppercase();
        if let Some(start) = upper.find("NAMES(") {
            let rest = &line[start + 6..];
            let rest = rest.trim_end_matches('+').trim_end_matches(')').trim();
            for token in rest.split_whitespace() {
                let token = token.trim_matches(|c: char| !c.is_ascii_alphanumeric() && c != '@' && c != '#' && c != '$');
                if !token.is_empty() {
                    let target = match section {
                        "AUTHCMD" => &mut cfg.authcmd,
                        "AUTHPGM" => &mut cfg.authpgm,
                        "NOTBKGND" => &mut cfg.notbkgnd,
                        "AUTHTSF" => &mut cfg.authtsf,
                        _ => return,
                    };
                    target.push(token.to_string());
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// AllocConfig — SYS-119 Story 2
// ---------------------------------------------------------------------------

/// Parsed ALLOCxx member — default allocation parameters for new datasets.
#[derive(Debug, Clone, Default)]
pub struct AllocConfig {
    /// Source member name.
    pub member_name: String,
    /// Default UNIT (e.g. `SYSALLDA`).
    pub unit: Option<String>,
    /// Default VOLUME.
    pub volume: Option<String>,
    /// Default storage class.
    pub storclas: Option<String>,
    /// Default management class.
    pub mgmtclas: Option<String>,
    /// Default data class.
    pub dataclas: Option<String>,
}

impl AllocConfig {
    /// Parse an ALLOCxx member.
    ///
    /// Recognises `KEY(VALUE)` or `KEY=VALUE` format on each line.
    pub fn parse(name: &str, content: &str) -> Result<Self> {
        let mut cfg = AllocConfig {
            member_name: name.to_string(),
            ..Default::default()
        };
        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('*') {
                continue;
            }
            for token in line.split(',') {
                let token = token.trim();
                if token.is_empty() {
                    continue;
                }
                let (key, value) = Self::parse_param(token);
                match key.to_uppercase().as_str() {
                    "UNIT" => cfg.unit = Some(value.to_string()),
                    "VOLUME" | "VOL" => cfg.volume = Some(value.to_string()),
                    "STORCLAS" => cfg.storclas = Some(value.to_string()),
                    "MGMTCLAS" => cfg.mgmtclas = Some(value.to_string()),
                    "DATACLAS" => cfg.dataclas = Some(value.to_string()),
                    _ => {}
                }
            }
        }
        Ok(cfg)
    }

    /// Parse a single `KEY(VALUE)` or `KEY=VALUE` token.
    fn parse_param(token: &str) -> (&str, &str) {
        if let Some(open) = token.find('(') {
            let key = &token[..open];
            let rest = &token[open + 1..];
            let value = rest.trim_end_matches(')');
            return (key.trim(), value.trim());
        }
        if let Some(eq) = token.find('=') {
            return (token[..eq].trim(), token[eq + 1..].trim());
        }
        (token.trim(), "")
    }
}

// ---------------------------------------------------------------------------
// SubsystemDelegate — SYS-119 Story 3
// ---------------------------------------------------------------------------

/// Trait for crates that want to register a PARMLIB member parser.
///
/// External subsystems (SMF, USS, JES2, RACF, etc.) implement this trait so
/// their member parsers can be dynamically registered with the PARMLIB
/// framework.
pub trait SubsystemDelegate: Send + Sync + std::fmt::Debug {
    /// Return the member-name prefix this delegate handles (e.g. `SMFPRM`).
    fn member_prefix(&self) -> &str;

    /// Parse the content of a member matching this delegate's prefix.
    fn parse(&self, content: &str) -> std::result::Result<(), String>;
}

// ---------------------------------------------------------------------------
// DelegateRegistry — SYS-119 Stories 4-6
// ---------------------------------------------------------------------------

/// Registry of subsystem delegates keyed by member prefix.
///
/// Allows SMFPRMxx, BPXPRMxx, JES2PARMxx, ICHPRMxx, and any other
/// subsystem-specific members to be parsed through a common interface.
#[derive(Default)]
pub struct DelegateRegistry {
    delegates: HashMap<String, Box<dyn SubsystemDelegate>>,
}

impl std::fmt::Debug for DelegateRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DelegateRegistry")
            .field("prefixes", &self.delegates.keys().collect::<Vec<_>>())
            .finish()
    }
}

impl DelegateRegistry {
    /// Create an empty delegate registry.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a subsystem delegate.
    pub fn register(&mut self, delegate: Box<dyn SubsystemDelegate>) -> Result<()> {
        let prefix = delegate.member_prefix().to_uppercase();
        if self.delegates.contains_key(&prefix) {
            return Err(SubsystemError::DuplicateDelegate(prefix));
        }
        self.delegates.insert(prefix, delegate);
        Ok(())
    }

    /// Look up a delegate by member-name prefix.
    pub fn get(&self, prefix: &str) -> Option<&dyn SubsystemDelegate> {
        self.delegates.get(&prefix.to_uppercase()).map(|b| &**b)
    }

    /// Parse member content by prefix delegation.
    pub fn parse_member(&self, prefix: &str, content: &str) -> Result<()> {
        let upper = prefix.to_uppercase();
        let delegate = self
            .delegates
            .get(&upper)
            .ok_or(SubsystemError::NoDelegate(upper))?;
        delegate
            .parse(content)
            .map_err(|detail| SubsystemError::ParseError {
                member: prefix.to_string(),
                detail,
            })
    }

    /// List all registered prefixes.
    pub fn prefixes(&self) -> Vec<&str> {
        self.delegates.keys().map(|s| s.as_str()).collect()
    }
}

// ---------------------------------------------------------------------------
// Tests — SYS-119 Story 7
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -- IkjTsoConfig tests -------------------------------------------------

    #[test]
    fn parse_ikjtso_sections() {
        let content = "\
* TSO configuration
AUTHCMD NAMES(          +
  RACDCERT              +
  ALTLIB                +
  TRANSMIT              +
)
AUTHPGM NAMES(          +
  IKJEFT01              +
  IRXJCL                +
)
NOTBKGND NAMES(         +
  OPERATOR              +
)
AUTHTSF NAMES(          +
  IKJEFT01              +
)
";
        let cfg = IkjTsoConfig::parse("IKJTSO00", content).unwrap();
        assert_eq!(cfg.authcmd, vec!["RACDCERT", "ALTLIB", "TRANSMIT"]);
        assert_eq!(cfg.authpgm, vec!["IKJEFT01", "IRXJCL"]);
        assert_eq!(cfg.notbkgnd, vec!["OPERATOR"]);
        assert_eq!(cfg.authtsf, vec!["IKJEFT01"]);
    }

    #[test]
    fn parse_ikjtso_empty() {
        let content = "* empty member\n";
        let cfg = IkjTsoConfig::parse("IKJTSO00", content).unwrap();
        assert!(cfg.authcmd.is_empty());
        assert!(cfg.authpgm.is_empty());
    }

    // -- AllocConfig tests --------------------------------------------------

    #[test]
    fn parse_alloc_paren_format() {
        let content = "\
* Default allocations
UNIT(SYSALLDA),STORCLAS(SCBASE)
MGMTCLAS(MCSTD),DATACLAS(DCSTD)
VOLUME(USR001)
";
        let cfg = AllocConfig::parse("ALLOC00", content).unwrap();
        assert_eq!(cfg.unit.as_deref(), Some("SYSALLDA"));
        assert_eq!(cfg.storclas.as_deref(), Some("SCBASE"));
        assert_eq!(cfg.mgmtclas.as_deref(), Some("MCSTD"));
        assert_eq!(cfg.dataclas.as_deref(), Some("DCSTD"));
        assert_eq!(cfg.volume.as_deref(), Some("USR001"));
    }

    #[test]
    fn parse_alloc_equal_format() {
        let content = "UNIT=SYSDA\nSTORCLAS=SC1";
        let cfg = AllocConfig::parse("ALLOC01", content).unwrap();
        assert_eq!(cfg.unit.as_deref(), Some("SYSDA"));
        assert_eq!(cfg.storclas.as_deref(), Some("SC1"));
    }

    // -- SubsystemDelegate / DelegateRegistry tests -------------------------

    #[derive(Debug)]
    struct MockSmfDelegate;

    impl SubsystemDelegate for MockSmfDelegate {
        fn member_prefix(&self) -> &str {
            "SMFPRM"
        }
        fn parse(&self, content: &str) -> std::result::Result<(), String> {
            if content.contains("ERROR") {
                Err("simulated error".to_string())
            } else {
                Ok(())
            }
        }
    }

    #[derive(Debug)]
    struct MockBpxDelegate;

    impl SubsystemDelegate for MockBpxDelegate {
        fn member_prefix(&self) -> &str {
            "BPXPRM"
        }
        fn parse(&self, _content: &str) -> std::result::Result<(), String> {
            Ok(())
        }
    }

    #[test]
    fn delegate_registry_register_and_parse() {
        let mut reg = DelegateRegistry::new();
        reg.register(Box::new(MockSmfDelegate)).unwrap();
        reg.register(Box::new(MockBpxDelegate)).unwrap();

        assert!(reg.parse_member("SMFPRM", "valid content").is_ok());
        assert!(reg.parse_member("BPXPRM", "anything").is_ok());
    }

    #[test]
    fn delegate_registry_parse_error() {
        let mut reg = DelegateRegistry::new();
        reg.register(Box::new(MockSmfDelegate)).unwrap();

        let err = reg.parse_member("SMFPRM", "ERROR here").unwrap_err();
        assert!(matches!(err, SubsystemError::ParseError { .. }));
    }

    #[test]
    fn delegate_registry_no_delegate() {
        let reg = DelegateRegistry::new();
        let err = reg.parse_member("NOSUCH", "content").unwrap_err();
        assert!(matches!(err, SubsystemError::NoDelegate(_)));
    }

    #[test]
    fn delegate_registry_duplicate() {
        let mut reg = DelegateRegistry::new();
        reg.register(Box::new(MockSmfDelegate)).unwrap();

        #[derive(Debug)]
        struct SmfDup;
        impl SubsystemDelegate for SmfDup {
            fn member_prefix(&self) -> &str {
                "SMFPRM"
            }
            fn parse(&self, _: &str) -> std::result::Result<(), String> {
                Ok(())
            }
        }

        let err = reg.register(Box::new(SmfDup)).unwrap_err();
        assert!(matches!(err, SubsystemError::DuplicateDelegate(_)));
    }

    #[test]
    fn delegate_registry_prefixes() {
        let mut reg = DelegateRegistry::new();
        reg.register(Box::new(MockSmfDelegate)).unwrap();
        reg.register(Box::new(MockBpxDelegate)).unwrap();
        let mut prefixes = reg.prefixes();
        prefixes.sort();
        assert_eq!(prefixes, vec!["BPXPRM", "SMFPRM"]);
    }
}
