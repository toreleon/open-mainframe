//! JECL — JES2 Control Language statement parsing.
//!
//! Parses JES2 control language statements embedded in JCL:
//! - `/*JOBPARM` — job parameters (SYSAFF, LINECT, COPIES, TIME, ROOM, etc.)
//! - `/*ROUTE`   — output and execution routing (PRINT, PUNCH, XEQ)
//! - `/*OUTPUT`  — output processing parameters
//! - `/*PRIORITY` — job selection priority override
//! - `/*SETUP`   — volume/tape mount setup requests
//! - `/*NOTIFY`  — job completion notification
//! - `/*MESSAGE` — operator message at job entry
//! - `/*SIGNOFF` — end of RJE session
//! - `/*SIGNON`  — start of RJE session
//! - `/*XEQ`     — alias for `/*ROUTE XEQ`
//! - `/*XMIT`    — NJE job transmission

use crate::output::Destination;

// ---------------------------------------------------------------------------
//  JECL statement types
// ---------------------------------------------------------------------------

/// A parsed JECL statement.
#[derive(Debug, Clone, PartialEq)]
pub enum JeclStmt {
    /// `/*JOBPARM` — job-level processing parameters.
    Jobparm(JobparmParams),
    /// `/*ROUTE PRINT dest` — route print output.
    RoutePrint(Destination),
    /// `/*ROUTE PUNCH dest` — route punch output.
    RoutePunch(Destination),
    /// `/*ROUTE XEQ node` — route execution to a node.
    RouteXeq(String),
    /// `/*OUTPUT` — output processing parameters.
    Output(OutputParams),
    /// `/*PRIORITY n` — set job priority.
    Priority(u8),
    /// `/*SETUP VOL=(...) | `/*SETUP` — volume/device setup.
    Setup(SetupParams),
    /// `/*NOTIFY userid` — notify user on job completion.
    Notify(String),
    /// `/*MESSAGE text` — send message to operator.
    Message(String),
    /// `/*XMIT NODE dest` — transmit job to remote node.
    Xmit(String),
    /// `/*SIGNOFF` — end RJE session.
    Signoff,
    /// `/*SIGNON` — begin RJE session (with optional password).
    Signon(Option<String>),
}

// ---------------------------------------------------------------------------
//  JOBPARM parameters
// ---------------------------------------------------------------------------

/// Parameters from a `/*JOBPARM` statement.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct JobparmParams {
    /// System affinity — list of systems where the job can run.
    pub sysaff: Vec<String>,
    /// Line count per page (LINECT=nn).
    pub linect: Option<u32>,
    /// Number of copies (COPIES=nn).
    pub copies: Option<u16>,
    /// Maximum CPU time in minutes (TIME=nn).
    pub time: Option<u32>,
    /// Room identification (ROOM=xxxx).
    pub room: Option<String>,
    /// Forms name (FORMS=xxxx).
    pub forms: Option<String>,
    /// Number of lines for SYSOUT (LINES=nn).
    pub lines: Option<u32>,
    /// Number of pages for SYSOUT (PAGES=nn).
    pub pages: Option<u32>,
    /// Number of bytes for SYSOUT (BYTES=nn).
    pub bytes: Option<u64>,
    /// Number of cards for SYSOUT (CARDS=nn).
    pub cards: Option<u32>,
    /// NOLOG — suppress job log.
    pub nolog: bool,
}

// ---------------------------------------------------------------------------
//  OUTPUT parameters
// ---------------------------------------------------------------------------

/// Parameters from a `/*OUTPUT` statement.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct OutputParams {
    /// SYSOUT class.
    pub class: Option<char>,
    /// Destination.
    pub dest: Option<Destination>,
    /// Forms name.
    pub forms: Option<String>,
    /// FCB image.
    pub fcb: Option<String>,
    /// UCS image.
    pub ucs: Option<String>,
    /// Copies.
    pub copies: Option<u16>,
}

// ---------------------------------------------------------------------------
//  SETUP parameters
// ---------------------------------------------------------------------------

/// Parameters from a `/*SETUP` statement.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct SetupParams {
    /// Volume serial numbers.
    pub volumes: Vec<String>,
}

// ---------------------------------------------------------------------------
//  Parser
// ---------------------------------------------------------------------------

/// Parse a single JECL statement.
///
/// JECL statements begin with `/*` in columns 1-2 and a keyword.
/// Returns `None` if the line is not a JECL statement.
pub fn parse_jecl(line: &str) -> Option<JeclStmt> {
    let trimmed = line.trim();

    // JECL lines start with "/*"
    if !trimmed.starts_with("/*") {
        return None;
    }

    // Skip the "/*" prefix.
    let rest = trimmed[2..].trim();

    // Split into keyword and operand(s).
    let (keyword, operands) = match rest.split_once(|c: char| c.is_whitespace()) {
        Some((k, o)) => (k.to_uppercase(), o.trim().to_string()),
        None => (rest.to_uppercase(), String::new()),
    };

    match keyword.as_str() {
        "JOBPARM" => Some(JeclStmt::Jobparm(parse_jobparm(&operands))),
        "ROUTE" => parse_route(&operands),
        "OUTPUT" => Some(JeclStmt::Output(parse_output_params(&operands))),
        "PRIORITY" => parse_priority(&operands),
        "SETUP" => Some(JeclStmt::Setup(parse_setup(&operands))),
        "NOTIFY" => Some(JeclStmt::Notify(operands.to_uppercase())),
        "MESSAGE" => Some(JeclStmt::Message(operands)),
        "XMIT" => parse_xmit(&operands),
        "XEQ" => Some(JeclStmt::RouteXeq(operands.to_uppercase())),
        "SIGNOFF" => Some(JeclStmt::Signoff),
        "SIGNON" => {
            let pw = if operands.is_empty() {
                None
            } else {
                Some(operands)
            };
            Some(JeclStmt::Signon(pw))
        }
        _ => None,
    }
}

/// Parse `/*JOBPARM` operands.
fn parse_jobparm(operands: &str) -> JobparmParams {
    let mut params = JobparmParams::default();

    for token in split_params(operands) {
        if let Some((key, val)) = token.split_once('=') {
            let key = key.trim().to_uppercase();
            let val = val.trim();
            match key.as_str() {
                "SYSAFF" => {
                    // SYSAFF=(SY1,SY2) or SYSAFF=SY1
                    let inner = val.trim_start_matches('(').trim_end_matches(')');
                    params.sysaff = inner.split(',').map(|s| s.trim().to_uppercase()).collect();
                }
                "LINECT" => {
                    params.linect = val.parse().ok();
                }
                "COPIES" => {
                    params.copies = val.parse().ok();
                }
                "TIME" => {
                    params.time = val.parse().ok();
                }
                "ROOM" => {
                    params.room = Some(val.to_string());
                }
                "FORMS" => {
                    params.forms = Some(val.to_uppercase());
                }
                "LINES" => {
                    params.lines = val.parse().ok();
                }
                "PAGES" => {
                    params.pages = val.parse().ok();
                }
                "BYTES" => {
                    params.bytes = val.parse().ok();
                }
                "CARDS" => {
                    params.cards = val.parse().ok();
                }
                _ => {} // Ignore unknown keywords.
            }
        } else {
            let token_upper = token.trim().to_uppercase();
            if token_upper == "NOLOG" {
                params.nolog = true;
            }
        }
    }

    params
}

/// Parse `/*ROUTE` operands.
fn parse_route(operands: &str) -> Option<JeclStmt> {
    let (sub, dest_str) = operands.split_once(|c: char| c.is_whitespace())?;
    let sub = sub.trim().to_uppercase();
    let dest_str = dest_str.trim();

    match sub.as_str() {
        "PRINT" => Some(JeclStmt::RoutePrint(crate::output::parse_destination(dest_str))),
        "PUNCH" => Some(JeclStmt::RoutePunch(crate::output::parse_destination(dest_str))),
        "XEQ" => Some(JeclStmt::RouteXeq(dest_str.to_uppercase())),
        _ => None,
    }
}

/// Parse `/*PRIORITY` operands.
fn parse_priority(operands: &str) -> Option<JeclStmt> {
    let val: u8 = operands.trim().parse().ok()?;
    Some(JeclStmt::Priority(val.min(15)))
}

/// Parse `/*OUTPUT` operands.
fn parse_output_params(operands: &str) -> OutputParams {
    let mut params = OutputParams::default();

    for token in split_params(operands) {
        if let Some((key, val)) = token.split_once('=') {
            let key = key.trim().to_uppercase();
            let val = val.trim();
            match key.as_str() {
                "CLASS" => {
                    params.class = val.chars().next();
                }
                "DEST" => {
                    params.dest = Some(crate::output::parse_destination(val));
                }
                "FORMS" => {
                    params.forms = Some(val.to_uppercase());
                }
                "FCB" => {
                    params.fcb = Some(val.to_uppercase());
                }
                "UCS" => {
                    params.ucs = Some(val.to_uppercase());
                }
                "COPIES" => {
                    params.copies = val.parse().ok();
                }
                _ => {}
            }
        }
    }

    params
}

/// Parse `/*SETUP` operands.
fn parse_setup(operands: &str) -> SetupParams {
    let mut params = SetupParams::default();
    let trimmed = operands.trim();

    if trimmed.is_empty() {
        return params;
    }

    // VOL=(vol1,vol2) or just vol1,vol2
    let vol_str = if let Some(rest) = trimmed.strip_prefix("VOL=") {
        rest
    } else {
        trimmed
    };

    let inner = vol_str.trim_start_matches('(').trim_end_matches(')');
    params.volumes = inner.split(',').map(|s| s.trim().to_uppercase()).collect();

    params
}

/// Parse `/*XMIT` operands.
fn parse_xmit(operands: &str) -> Option<JeclStmt> {
    // /*XMIT NODE dest
    let rest = operands.trim();
    if rest.is_empty() {
        return None;
    }
    // Skip optional "NODE" keyword.
    let dest = if let Some(stripped) = rest
        .to_uppercase()
        .strip_prefix("NODE")
        .map(|s| s.to_string())
    {
        stripped.trim().to_string()
    } else {
        rest.to_uppercase()
    };
    if dest.is_empty() {
        return None;
    }
    Some(JeclStmt::Xmit(dest))
}

/// Split JECL parameter string on commas, respecting parenthesized groups.
fn split_params(s: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut current = String::new();
    let mut depth = 0u32;

    for ch in s.chars() {
        match ch {
            '(' => {
                depth += 1;
                current.push(ch);
            }
            ')' => {
                depth = depth.saturating_sub(1);
                current.push(ch);
            }
            ',' if depth == 0 => {
                let trimmed = current.trim().to_string();
                if !trimmed.is_empty() {
                    result.push(trimmed);
                }
                current.clear();
            }
            _ => current.push(ch),
        }
    }

    let trimmed = current.trim().to_string();
    if !trimmed.is_empty() {
        result.push(trimmed);
    }

    result
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::output::Destination;

    // ─── J105.1: JECL Parser ───

    #[test]
    fn test_jobparm_sysaff_linect_copies() {
        let stmt = parse_jecl("/*JOBPARM SYSAFF=(SY1,SY2),LINECT=60,COPIES=2").unwrap();
        match stmt {
            JeclStmt::Jobparm(p) => {
                assert_eq!(p.sysaff, vec!["SY1", "SY2"]);
                assert_eq!(p.linect, Some(60));
                assert_eq!(p.copies, Some(2));
            }
            other => panic!("expected Jobparm, got {other:?}"),
        }
    }

    #[test]
    fn test_jobparm_time_room() {
        let stmt = parse_jecl("/*JOBPARM TIME=30,ROOM=1234").unwrap();
        match stmt {
            JeclStmt::Jobparm(p) => {
                assert_eq!(p.time, Some(30));
                assert_eq!(p.room, Some("1234".to_string()));
            }
            other => panic!("expected Jobparm, got {other:?}"),
        }
    }

    #[test]
    fn test_jobparm_nolog() {
        let stmt = parse_jecl("/*JOBPARM NOLOG").unwrap();
        match stmt {
            JeclStmt::Jobparm(p) => {
                assert!(p.nolog);
            }
            other => panic!("expected Jobparm, got {other:?}"),
        }
    }

    #[test]
    fn test_jobparm_forms_lines_pages_bytes_cards() {
        let stmt =
            parse_jecl("/*JOBPARM FORMS=STD,LINES=1000,PAGES=50,BYTES=500000,CARDS=200")
                .unwrap();
        match stmt {
            JeclStmt::Jobparm(p) => {
                assert_eq!(p.forms, Some("STD".to_string()));
                assert_eq!(p.lines, Some(1000));
                assert_eq!(p.pages, Some(50));
                assert_eq!(p.bytes, Some(500_000));
                assert_eq!(p.cards, Some(200));
            }
            other => panic!("expected Jobparm, got {other:?}"),
        }
    }

    #[test]
    fn test_route_print() {
        let stmt = parse_jecl("/*ROUTE PRINT RMT001").unwrap();
        assert_eq!(
            stmt,
            JeclStmt::RoutePrint(Destination::Remote("RMT001".to_string()))
        );
    }

    #[test]
    fn test_route_punch() {
        let stmt = parse_jecl("/*ROUTE PUNCH NODE01.USER01").unwrap();
        assert_eq!(
            stmt,
            JeclStmt::RoutePunch(Destination::NodeUser(
                "NODE01".to_string(),
                "USER01".to_string()
            ))
        );
    }

    #[test]
    fn test_route_xeq() {
        let stmt = parse_jecl("/*ROUTE XEQ SYS2").unwrap();
        assert_eq!(stmt, JeclStmt::RouteXeq("SYS2".to_string()));
    }

    #[test]
    fn test_priority() {
        let stmt = parse_jecl("/*PRIORITY 15").unwrap();
        assert_eq!(stmt, JeclStmt::Priority(15));
    }

    #[test]
    fn test_priority_clamped() {
        let stmt = parse_jecl("/*PRIORITY 99").unwrap();
        assert_eq!(stmt, JeclStmt::Priority(15));
    }

    #[test]
    fn test_output_params() {
        let stmt = parse_jecl("/*OUTPUT CLASS=A,DEST=RMT001,COPIES=3").unwrap();
        match stmt {
            JeclStmt::Output(p) => {
                assert_eq!(p.class, Some('A'));
                assert_eq!(p.dest, Some(Destination::Remote("RMT001".to_string())));
                assert_eq!(p.copies, Some(3));
            }
            other => panic!("expected Output, got {other:?}"),
        }
    }

    #[test]
    fn test_output_forms_fcb_ucs() {
        let stmt = parse_jecl("/*OUTPUT FORMS=STD,FCB=STD1,UCS=PN").unwrap();
        match stmt {
            JeclStmt::Output(p) => {
                assert_eq!(p.forms, Some("STD".to_string()));
                assert_eq!(p.fcb, Some("STD1".to_string()));
                assert_eq!(p.ucs, Some("PN".to_string()));
            }
            other => panic!("expected Output, got {other:?}"),
        }
    }

    #[test]
    fn test_setup_volumes() {
        let stmt = parse_jecl("/*SETUP VOL=(VOL001,VOL002)").unwrap();
        match stmt {
            JeclStmt::Setup(p) => {
                assert_eq!(p.volumes, vec!["VOL001", "VOL002"]);
            }
            other => panic!("expected Setup, got {other:?}"),
        }
    }

    #[test]
    fn test_setup_bare_volumes() {
        let stmt = parse_jecl("/*SETUP VOL001,VOL002").unwrap();
        match stmt {
            JeclStmt::Setup(p) => {
                assert_eq!(p.volumes, vec!["VOL001", "VOL002"]);
            }
            other => panic!("expected Setup, got {other:?}"),
        }
    }

    #[test]
    fn test_notify() {
        let stmt = parse_jecl("/*NOTIFY USER01").unwrap();
        assert_eq!(stmt, JeclStmt::Notify("USER01".to_string()));
    }

    #[test]
    fn test_message() {
        let stmt = parse_jecl("/*MESSAGE MOUNT TAPE VOL001 ON UNIT 0A80").unwrap();
        assert_eq!(
            stmt,
            JeclStmt::Message("MOUNT TAPE VOL001 ON UNIT 0A80".to_string())
        );
    }

    #[test]
    fn test_xmit() {
        let stmt = parse_jecl("/*XMIT NODE SYS2").unwrap();
        assert_eq!(stmt, JeclStmt::Xmit("SYS2".to_string()));
    }

    #[test]
    fn test_xmit_without_node_keyword() {
        let stmt = parse_jecl("/*XMIT SYS2").unwrap();
        assert_eq!(stmt, JeclStmt::Xmit("SYS2".to_string()));
    }

    #[test]
    fn test_xeq() {
        let stmt = parse_jecl("/*XEQ SYS3").unwrap();
        assert_eq!(stmt, JeclStmt::RouteXeq("SYS3".to_string()));
    }

    #[test]
    fn test_signoff() {
        let stmt = parse_jecl("/*SIGNOFF").unwrap();
        assert_eq!(stmt, JeclStmt::Signoff);
    }

    #[test]
    fn test_signon_no_password() {
        let stmt = parse_jecl("/*SIGNON").unwrap();
        assert_eq!(stmt, JeclStmt::Signon(None));
    }

    #[test]
    fn test_signon_with_password() {
        let stmt = parse_jecl("/*SIGNON SECRET").unwrap();
        assert_eq!(stmt, JeclStmt::Signon(Some("SECRET".to_string())));
    }

    #[test]
    fn test_not_jecl() {
        assert!(parse_jecl("// EXEC PGM=IEFBR14").is_none());
        assert!(parse_jecl("HELLO WORLD").is_none());
    }

    #[test]
    fn test_unknown_jecl() {
        assert!(parse_jecl("/*FOOBAR SOMETHING").is_none());
    }

    #[test]
    fn test_case_insensitive() {
        let stmt = parse_jecl("/*jobparm SYSAFF=SY1").unwrap();
        match stmt {
            JeclStmt::Jobparm(p) => {
                assert_eq!(p.sysaff, vec!["SY1"]);
            }
            other => panic!("expected Jobparm, got {other:?}"),
        }
    }

    #[test]
    fn test_split_params_with_parens() {
        let result = split_params("SYSAFF=(SY1,SY2),LINECT=60");
        assert_eq!(result, vec!["SYSAFF=(SY1,SY2)", "LINECT=60"]);
    }

    #[test]
    fn test_single_sysaff() {
        let stmt = parse_jecl("/*JOBPARM SYSAFF=SY1").unwrap();
        match stmt {
            JeclStmt::Jobparm(p) => {
                assert_eq!(p.sysaff, vec!["SY1"]);
            }
            other => panic!("expected Jobparm, got {other:?}"),
        }
    }

    #[test]
    fn test_route_print_local() {
        let stmt = parse_jecl("/*ROUTE PRINT LOCAL").unwrap();
        assert_eq!(stmt, JeclStmt::RoutePrint(Destination::Local));
    }

    #[test]
    fn test_route_print_node() {
        let stmt = parse_jecl("/*ROUTE PRINT SYSA").unwrap();
        assert_eq!(
            stmt,
            JeclStmt::RoutePrint(Destination::Node("SYSA".to_string()))
        );
    }

    #[test]
    fn test_empty_setup() {
        let stmt = parse_jecl("/*SETUP").unwrap();
        match stmt {
            JeclStmt::Setup(p) => {
                assert!(p.volumes.is_empty());
            }
            other => panic!("expected Setup, got {other:?}"),
        }
    }
}
