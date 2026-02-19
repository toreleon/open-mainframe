//! JES2PARM configuration parser and initialization parameters.
//!
//! Parses JES2 initialization statements that configure job classes, output
//! classes, spool definitions, checkpoint settings, and initiators.

use crate::checkpoint::CheckpointConfig;
use crate::commands::Initiator;
use crate::job::{JobClass, JobClassDef};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Configuration types
// ---------------------------------------------------------------------------

/// Output class definition (for SYSOUT routing).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OutClassDef {
    /// Class identifier (A-Z, 0-9).
    pub class: char,
    /// Default destination (printer name or HOLD).
    pub dest: String,
    /// Whether output is held until explicitly released.
    pub hold: bool,
}

/// Spool definition parameters.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpoolDef {
    /// Volume serial numbers for spool space.
    pub volumes: Vec<String>,
    /// Track group size (number of tracks per track group).
    pub tgsize: u32,
    /// Spool space warning threshold (percentage).
    pub warn_pct: u8,
}

impl Default for SpoolDef {
    fn default() -> Self {
        Self {
            volumes: vec!["SPOOL1".to_string()],
            tgsize: 100,
            warn_pct: 80,
        }
    }
}

/// Initiator definition from JES2PARM.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InitDef {
    /// Initiator number (1-based).
    pub id: u32,
    /// Classes this initiator should process.
    pub classes: Vec<char>,
    /// Start automatically on JES2 init.
    pub auto_start: bool,
}

/// Complete JES2 configuration, typically loaded from JES2PARM.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Jes2Config {
    /// Job class definitions.
    pub job_classes: HashMap<String, JobClassDef>,
    /// Output class definitions.
    pub out_classes: HashMap<char, OutClassDef>,
    /// Spool definition.
    pub spool_def: SpoolDef,
    /// Checkpoint configuration.
    pub checkpoint: CheckpointConfig,
    /// Initiator definitions.
    pub initiators: Vec<InitDef>,
}

impl Default for Jes2Config {
    fn default() -> Self {
        let mut job_classes = HashMap::new();
        for c in b'A'..=b'Z' {
            let cls = JobClass::Standard(c as char);
            job_classes.insert(cls.to_string(), JobClassDef::default_for(cls));
        }
        for c in b'0'..=b'9' {
            let cls = JobClass::Standard(c as char);
            job_classes.insert(cls.to_string(), JobClassDef::default_for(cls));
        }
        job_classes.insert("STC".to_string(), JobClassDef::default_for(JobClass::Stc));
        job_classes.insert("TSU".to_string(), JobClassDef::default_for(JobClass::Tsu));

        Self {
            job_classes,
            out_classes: HashMap::new(),
            spool_def: SpoolDef::default(),
            checkpoint: CheckpointConfig::default(),
            initiators: Vec::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// JES2PARM parser
// ---------------------------------------------------------------------------

/// A single parsed JES2PARM statement.
#[derive(Debug, Clone, PartialEq)]
pub enum Jes2Parm {
    /// `JOBCLASS(c) PROCLIB=...,MSGCLASS=...,MAXRC=...`
    JobClass {
        class: char,
        proclib: Option<String>,
        msgclass: Option<char>,
        max_rc: Option<u32>,
    },
    /// `OUTCLASS(c) DEST=...,HOLD=YES|NO`
    OutClass {
        class: char,
        dest: Option<String>,
        hold: Option<bool>,
    },
    /// `SPOOLDEF VOLUMES=(v1,v2,...),TGSIZE(n)`
    SpoolDef {
        volumes: Option<Vec<String>>,
        tgsize: Option<u32>,
    },
    /// `CKPTDEF DUAL=YES|NO`
    CkptDef {
        dual: Option<bool>,
    },
    /// `INIT(n) CLASS=ABCD,START=YES|NO`
    Init {
        id: u32,
        classes: Option<Vec<char>>,
        auto_start: Option<bool>,
    },
}

/// Parse JES2PARM text into a list of parameter statements.
///
/// Lines starting with `/*` or blank lines are treated as comments.
/// Continuation is handled by trailing commas (not yet implemented —
/// each statement must be on one line).
pub fn parse_jes2parm(input: &str) -> Vec<Jes2Parm> {
    let mut result = Vec::new();
    for line in input.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with("/*") {
            continue;
        }
        if let Some(parm) = parse_parm_line(line) {
            result.push(parm);
        }
    }
    result
}

fn parse_parm_line(line: &str) -> Option<Jes2Parm> {
    let upper = line.to_ascii_uppercase();

    if upper.starts_with("JOBCLASS(") {
        return parse_jobclass_parm(&upper);
    }
    if upper.starts_with("OUTCLASS(") {
        return parse_outclass_parm(&upper);
    }
    if upper.starts_with("SPOOLDEF") {
        return parse_spooldef_parm(&upper);
    }
    if upper.starts_with("CKPTDEF") {
        return parse_ckptdef_parm(&upper);
    }
    if upper.starts_with("INIT(") {
        return parse_init_parm(&upper);
    }
    None
}

/// Parse `JOBCLASS(A) PROCLIB=PROC00,MSGCLASS=X,MAXRC=4`
fn parse_jobclass_parm(s: &str) -> Option<Jes2Parm> {
    let class = s.chars().nth(9)?; // character after JOBCLASS(
    if !class.is_alphanumeric() {
        return None;
    }
    let rest = extract_after_paren(s, "JOBCLASS(")?;
    let kv = parse_key_values(&rest);

    Some(Jes2Parm::JobClass {
        class,
        proclib: kv.get("PROCLIB").cloned(),
        msgclass: kv.get("MSGCLASS").and_then(|v| v.chars().next()),
        max_rc: kv.get("MAXRC").and_then(|v| v.parse().ok()),
    })
}

/// Parse `OUTCLASS(A) DEST=LOCAL,HOLD=YES`
fn parse_outclass_parm(s: &str) -> Option<Jes2Parm> {
    let class = s.chars().nth(9)?;
    if !class.is_alphanumeric() {
        return None;
    }
    let rest = extract_after_paren(s, "OUTCLASS(")?;
    let kv = parse_key_values(&rest);

    Some(Jes2Parm::OutClass {
        class,
        dest: kv.get("DEST").cloned(),
        hold: kv.get("HOLD").map(|v| v == "YES"),
    })
}

/// Parse `SPOOLDEF VOLUMES=(SPOOL1,SPOOL2),TGSIZE(100)`
fn parse_spooldef_parm(s: &str) -> Option<Jes2Parm> {
    let rest = s.strip_prefix("SPOOLDEF")?.trim();
    let volumes = extract_paren_list(rest, "VOLUMES=");
    let tgsize = extract_paren_value(rest, "TGSIZE(").and_then(|v| v.parse().ok());

    Some(Jes2Parm::SpoolDef { volumes, tgsize })
}

/// Parse `CKPTDEF DUAL=YES`
fn parse_ckptdef_parm(s: &str) -> Option<Jes2Parm> {
    let rest = s.strip_prefix("CKPTDEF")?.trim();
    let kv = parse_key_values(rest);
    let dual = kv.get("DUAL").map(|v| v == "YES");

    Some(Jes2Parm::CkptDef { dual })
}

/// Parse `INIT(1) CLASS=ABCD,START=YES`
fn parse_init_parm(s: &str) -> Option<Jes2Parm> {
    let id_char_end = s.find(')')?;
    let id: u32 = s[5..id_char_end].parse().ok()?;
    let rest = &s[id_char_end + 1..].trim();
    let kv = parse_key_values(rest);

    let classes = kv
        .get("CLASS")
        .map(|v| v.chars().filter(|c| c.is_alphanumeric()).collect());
    let auto_start = kv.get("START").map(|v| v == "YES");

    Some(Jes2Parm::Init {
        id,
        classes,
        auto_start,
    })
}

// ---------------------------------------------------------------------------
// Helper parsers
// ---------------------------------------------------------------------------

/// Extract the remainder after closing paren of a `PREFIX(x)` token.
fn extract_after_paren(s: &str, prefix: &str) -> Option<String> {
    let after_prefix = s.strip_prefix(prefix)?;
    let close = after_prefix.find(')')?;
    Some(after_prefix[close + 1..].trim().to_string())
}

/// Parse `KEY=VALUE,KEY=VALUE,...` into a map.
fn parse_key_values(s: &str) -> HashMap<String, String> {
    let mut map = HashMap::new();
    for part in s.split(',') {
        let part = part.trim();
        if let Some((key, value)) = part.split_once('=') {
            let key = key.trim().to_string();
            let value = value.trim().to_string();
            map.insert(key, value);
        }
    }
    map
}

/// Extract a parenthesized list: `VOLUMES=(A,B,C)` → `Some(vec!["A","B","C"])`
fn extract_paren_list(s: &str, prefix: &str) -> Option<Vec<String>> {
    let start = s.find(prefix)? + prefix.len();
    let rest = &s[start..];
    if !rest.starts_with('(') {
        return None;
    }
    let close = rest.find(')')?;
    let inner = &rest[1..close];
    Some(
        inner
            .split(',')
            .map(|v| v.trim().to_string())
            .collect(),
    )
}

/// Extract a parenthesized value: `TGSIZE(100)` → `Some("100")`
fn extract_paren_value<'a>(s: &'a str, prefix: &str) -> Option<&'a str> {
    let start = s.find(prefix)? + prefix.len();
    let rest = &s[start..];
    let close = rest.find(')')?;
    Some(&rest[..close])
}

// ---------------------------------------------------------------------------
// Apply config
// ---------------------------------------------------------------------------

/// Apply parsed JES2PARM statements to a configuration.
pub fn apply_parms(config: &mut Jes2Config, parms: &[Jes2Parm]) {
    for parm in parms {
        match parm {
            Jes2Parm::JobClass {
                class,
                proclib,
                msgclass,
                max_rc,
            } => {
                let cls = JobClass::Standard(*class);
                let key = cls.to_string();
                let def = config
                    .job_classes
                    .entry(key)
                    .or_insert_with(|| JobClassDef::default_for(cls));
                if let Some(p) = proclib {
                    def.proclib = p.clone();
                }
                if let Some(m) = msgclass {
                    def.msgclass = *m;
                }
                if let Some(r) = max_rc {
                    def.max_rc = *r;
                }
            }
            Jes2Parm::OutClass { class, dest, hold } => {
                let def = config
                    .out_classes
                    .entry(*class)
                    .or_insert_with(|| OutClassDef {
                        class: *class,
                        dest: "LOCAL".to_string(),
                        hold: false,
                    });
                if let Some(d) = dest {
                    def.dest = d.clone();
                }
                if let Some(h) = hold {
                    def.hold = *h;
                }
            }
            Jes2Parm::SpoolDef { volumes, tgsize } => {
                if let Some(v) = volumes {
                    config.spool_def.volumes = v.clone();
                }
                if let Some(t) = tgsize {
                    config.spool_def.tgsize = *t;
                }
            }
            Jes2Parm::CkptDef { dual } => {
                if let Some(d) = dual {
                    config.checkpoint.dual = *d;
                }
            }
            Jes2Parm::Init {
                id,
                classes,
                auto_start,
            } => {
                // Find or create initiator def
                if let Some(existing) = config.initiators.iter_mut().find(|i| i.id == *id) {
                    if let Some(c) = classes {
                        existing.classes = c.clone();
                    }
                    if let Some(a) = auto_start {
                        existing.auto_start = *a;
                    }
                } else {
                    config.initiators.push(InitDef {
                        id: *id,
                        classes: classes.clone().unwrap_or_else(|| vec!['A']),
                        auto_start: auto_start.unwrap_or(true),
                    });
                }
            }
        }
    }
}

/// Build initiators from config definitions.
pub fn build_initiators(config: &Jes2Config) -> Vec<Initiator> {
    config
        .initiators
        .iter()
        .map(|def| {
            let classes: Vec<JobClass> = def
                .classes
                .iter()
                .filter_map(|&c| JobClass::standard(c))
                .collect();
            let mut init = Initiator::new(def.id, classes);
            init.active = def.auto_start;
            init
        })
        .collect()
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_jobclass() {
        let parms = parse_jes2parm("JOBCLASS(A) PROCLIB=PROC00,MSGCLASS=X,MAXRC=4");
        assert_eq!(parms.len(), 1);
        assert_eq!(
            parms[0],
            Jes2Parm::JobClass {
                class: 'A',
                proclib: Some("PROC00".to_string()),
                msgclass: Some('X'),
                max_rc: Some(4),
            }
        );
    }

    #[test]
    fn parse_outclass() {
        let parms = parse_jes2parm("OUTCLASS(A) DEST=LOCAL,HOLD=YES");
        assert_eq!(parms.len(), 1);
        assert_eq!(
            parms[0],
            Jes2Parm::OutClass {
                class: 'A',
                dest: Some("LOCAL".to_string()),
                hold: Some(true),
            }
        );
    }

    #[test]
    fn parse_spooldef() {
        let parms = parse_jes2parm("SPOOLDEF VOLUMES=(SPOOL1,SPOOL2),TGSIZE(100)");
        assert_eq!(parms.len(), 1);
        match &parms[0] {
            Jes2Parm::SpoolDef { volumes, tgsize } => {
                assert_eq!(
                    volumes.as_ref().unwrap(),
                    &["SPOOL1".to_string(), "SPOOL2".to_string()]
                );
                assert_eq!(*tgsize, Some(100));
            }
            _ => panic!("expected SpoolDef"),
        }
    }

    #[test]
    fn parse_ckptdef() {
        let parms = parse_jes2parm("CKPTDEF DUAL=YES");
        assert_eq!(
            parms[0],
            Jes2Parm::CkptDef {
                dual: Some(true),
            }
        );
    }

    #[test]
    fn parse_init() {
        let parms = parse_jes2parm("INIT(1) CLASS=ABCD,START=YES");
        assert_eq!(
            parms[0],
            Jes2Parm::Init {
                id: 1,
                classes: Some(vec!['A', 'B', 'C', 'D']),
                auto_start: Some(true),
            }
        );
    }

    #[test]
    fn parse_comments_and_blanks() {
        let input = r#"
/* JES2 Configuration
JOBCLASS(A) PROCLIB=PROC00,MSGCLASS=A,MAXRC=4

/* Another comment
JOBCLASS(B) PROCLIB=PROC01,MSGCLASS=B,MAXRC=8
"#;
        let parms = parse_jes2parm(input);
        assert_eq!(parms.len(), 2);
    }

    #[test]
    fn apply_jobclass_parm() {
        let mut config = Jes2Config::default();
        let parms = parse_jes2parm("JOBCLASS(A) PROCLIB=MYPROC,MSGCLASS=X,MAXRC=8");
        apply_parms(&mut config, &parms);

        let def = config.job_classes.get("A").unwrap();
        assert_eq!(def.proclib, "MYPROC");
        assert_eq!(def.msgclass, 'X');
        assert_eq!(def.max_rc, 8);
    }

    #[test]
    fn apply_spooldef_parm() {
        let mut config = Jes2Config::default();
        let parms = parse_jes2parm("SPOOLDEF VOLUMES=(SPOOL1,SPOOL2),TGSIZE(200)");
        apply_parms(&mut config, &parms);

        assert_eq!(config.spool_def.volumes, vec!["SPOOL1", "SPOOL2"]);
        assert_eq!(config.spool_def.tgsize, 200);
    }

    #[test]
    fn apply_checkpoint_parm() {
        let mut config = Jes2Config::default();
        let parms = parse_jes2parm("CKPTDEF DUAL=NO");
        apply_parms(&mut config, &parms);
        assert!(!config.checkpoint.dual);
    }

    #[test]
    fn apply_init_parm() {
        let mut config = Jes2Config::default();
        let parms = parse_jes2parm(
            "INIT(1) CLASS=AB,START=YES\nINIT(2) CLASS=CD,START=NO",
        );
        apply_parms(&mut config, &parms);

        assert_eq!(config.initiators.len(), 2);
        assert_eq!(config.initiators[0].id, 1);
        assert_eq!(config.initiators[0].classes, vec!['A', 'B']);
        assert!(config.initiators[0].auto_start);
        assert!(!config.initiators[1].auto_start);
    }

    #[test]
    fn build_initiators_from_config() {
        let mut config = Jes2Config::default();
        let parms = parse_jes2parm("INIT(1) CLASS=ABCD,START=YES\nINIT(2) CLASS=EF,START=NO");
        apply_parms(&mut config, &parms);

        let inits = build_initiators(&config);
        assert_eq!(inits.len(), 2);
        assert!(inits[0].active);
        assert_eq!(inits[0].classes.len(), 4);
        assert!(!inits[1].active);
        assert_eq!(inits[1].classes.len(), 2);
    }

    #[test]
    fn apply_outclass_parm() {
        let mut config = Jes2Config::default();
        let parms = parse_jes2parm("OUTCLASS(A) DEST=PRT001,HOLD=NO");
        apply_parms(&mut config, &parms);

        let def = config.out_classes.get(&'A').unwrap();
        assert_eq!(def.dest, "PRT001");
        assert!(!def.hold);
    }

    #[test]
    fn full_jes2parm_file() {
        let input = r#"
/* JES2 Initialization Parameters
JOBCLASS(A) PROCLIB=PROC00,MSGCLASS=A,MAXRC=4
JOBCLASS(B) PROCLIB=PROC01,MSGCLASS=B,MAXRC=8
OUTCLASS(A) DEST=LOCAL,HOLD=NO
OUTCLASS(H) DEST=HOLD,HOLD=YES
SPOOLDEF VOLUMES=(SPOOL1,SPOOL2),TGSIZE(100)
CKPTDEF DUAL=YES
INIT(1) CLASS=ABCD,START=YES
INIT(2) CLASS=ABCDEFGH,START=YES
INIT(3) CLASS=ST,START=NO
"#;
        let mut config = Jes2Config::default();
        let parms = parse_jes2parm(input);
        assert_eq!(parms.len(), 9);
        apply_parms(&mut config, &parms);

        // Verify everything applied
        assert_eq!(config.job_classes.get("A").unwrap().proclib, "PROC00");
        assert_eq!(config.job_classes.get("B").unwrap().max_rc, 8);
        assert_eq!(config.out_classes.len(), 2);
        assert_eq!(config.spool_def.volumes.len(), 2);
        assert!(config.checkpoint.dual);
        assert_eq!(config.initiators.len(), 3);

        let inits = build_initiators(&config);
        assert_eq!(inits.len(), 3);
    }

    #[test]
    fn config_serialization_roundtrip() {
        let mut config = Jes2Config::default();
        let parms = parse_jes2parm("JOBCLASS(A) PROCLIB=TEST,MSGCLASS=Z,MAXRC=12");
        apply_parms(&mut config, &parms);

        let json = serde_json::to_string(&config).unwrap();
        let restored: Jes2Config = serde_json::from_str(&json).unwrap();

        assert_eq!(restored.job_classes.get("A").unwrap().proclib, "TEST");
        assert_eq!(restored.job_classes.get("A").unwrap().msgclass, 'Z');
    }
}
