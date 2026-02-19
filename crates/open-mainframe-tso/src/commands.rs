//! TSO command implementations — ALLOCATE, FREE, LISTDS, LISTALC, PROFILE,
//! DELETE, RENAME, ALTLIB, HELP.

use crate::error::TsoError;
use crate::parser::ParsedCommand;
use crate::session::*;

/// Result of executing a TSO command.
#[derive(Debug, Clone)]
pub struct CommandResult {
    /// Output lines for the terminal.
    pub output: Vec<String>,
    /// Return code (0 = success).
    pub rc: u32,
}

impl CommandResult {
    pub(crate) fn ok(lines: Vec<String>) -> Self {
        Self { output: lines, rc: 0 }
    }

    pub(crate) fn error(msg: &str) -> Self {
        Self {
            output: vec![msg.to_string()],
            rc: 12,
        }
    }
}

/// Execute a parsed TSO command against the session.
pub fn execute(session: &mut TsoSession, cmd: &ParsedCommand) -> CommandResult {
    match cmd.name.as_str() {
        "ALLOC" | "ALLOCATE" => cmd_allocate(session, cmd),
        "FREE" | "UNALLOC" => cmd_free(session, cmd),
        "LISTDS" => cmd_listds(session, cmd),
        "LISTALC" => cmd_listalc(session, cmd),
        "PROFILE" | "PROF" => cmd_profile(session, cmd),
        "DELETE" | "DEL" => cmd_delete(session, cmd),
        "RENAME" | "REN" => cmd_rename(session, cmd),
        "ALTLIB" => cmd_altlib(session, cmd),
        "HELP" => cmd_help(cmd),
        "TIME" => cmd_time(),
        _ => CommandResult::error(&format!("IKJ56500I COMMAND {} NOT FOUND", cmd.name)),
    }
}

// ---------------------------------------------------------------------------
// ALLOCATE (T100.2)
// ---------------------------------------------------------------------------

fn cmd_allocate(session: &mut TsoSession, cmd: &ParsedCommand) -> CommandResult {
    // Required: DA(dsn) or DATASET(dsn)
    let raw_dsn = match cmd.keyword("DA").or_else(|| cmd.keyword("DATASET")) {
        Some(d) => d.to_string(),
        None => return CommandResult::error("IKJ56702I MISSING DATASET OPERAND"),
    };

    let dsn = session.qualify_dsn(&raw_dsn);
    let path = session.resolve_dsn(&dsn);

    // DD name: FILE(dd) — default to SYSUT1 if not specified
    let dd_name = cmd
        .keyword("FILE")
        .or_else(|| cmd.keyword("FI"))
        .unwrap_or("SYSUT1")
        .to_ascii_uppercase();

    // Disposition
    let disp = if cmd.has_flag("NEW") {
        AllocDisp::New
    } else if cmd.has_flag("OLD") {
        AllocDisp::Old
    } else if cmd.has_flag("MOD") {
        AllocDisp::Mod
    } else {
        AllocDisp::Shr // default
    };

    // DCB attributes
    let dcb = DcbAttrs {
        lrecl: cmd.keyword("LRECL").and_then(|v| v.parse().ok()),
        blksize: cmd.keyword("BLKSIZE").and_then(|v| v.parse().ok()),
        recfm: cmd.keyword("RECFM").map(|v| v.to_ascii_uppercase()),
        dsorg: cmd.keyword("DSORG").map(|v| v.to_ascii_uppercase()),
    };

    let reuse = cmd.has_flag("REUSE");

    // If NEW, create parent directories
    if disp == AllocDisp::New {
        if let Some(parent) = path.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
    }

    let entry = AllocEntry {
        dd_name: dd_name.clone(),
        dsn: dsn.clone(),
        disp,
        dcb,
        path,
        reuse,
    };

    match session.allocate(entry) {
        Ok(()) => CommandResult::ok(vec![
            format!("IKJ56247I DATA SET {} ALLOCATED TO {}", dsn, dd_name),
        ]),
        Err(TsoError::DdAlreadyAllocated(dd)) => {
            CommandResult::error(&format!("IKJ56862I FILE {dd} ALREADY ALLOCATED"))
        }
        Err(e) => CommandResult::error(&e.to_string()),
    }
}

// ---------------------------------------------------------------------------
// FREE (T100.2)
// ---------------------------------------------------------------------------

fn cmd_free(session: &mut TsoSession, cmd: &ParsedCommand) -> CommandResult {
    // FREE FILE(dd) or FREE DA(dsn)
    if let Some(dd) = cmd.keyword("FILE").or_else(|| cmd.keyword("FI")) {
        match session.free(dd) {
            Ok(entry) => CommandResult::ok(vec![
                format!("IKJ56243I FILE {} FREED, DATA SET {} RELEASED", dd.to_ascii_uppercase(), entry.dsn),
            ]),
            Err(e) => CommandResult::error(&e.to_string()),
        }
    } else if let Some(raw_dsn) = cmd.keyword("DA").or_else(|| cmd.keyword("DATASET")) {
        let dsn = session.qualify_dsn(raw_dsn);
        // Find the DD name for this dataset
        let dd = session
            .list_allocs()
            .iter()
            .find(|a| a.dsn == dsn)
            .map(|a| a.dd_name.clone());
        if let Some(dd) = dd {
            match session.free(&dd) {
                Ok(entry) => CommandResult::ok(vec![
                    format!("IKJ56243I FILE {} FREED, DATA SET {} RELEASED", dd, entry.dsn),
                ]),
                Err(e) => CommandResult::error(&e.to_string()),
            }
        } else {
            CommandResult::error(&format!("IKJ56704I DATA SET {dsn} NOT ALLOCATED"))
        }
    } else {
        CommandResult::error("IKJ56702I MISSING FILE OR DATASET OPERAND")
    }
}

// ---------------------------------------------------------------------------
// LISTDS (T100.3)
// ---------------------------------------------------------------------------

fn cmd_listds(session: &mut TsoSession, cmd: &ParsedCommand) -> CommandResult {
    let raw_dsn = match cmd.first_positional().or_else(|| cmd.keyword("DA")) {
        Some(d) => d.to_string(),
        None => return CommandResult::error("IKJ56702I MISSING DATASET NAME"),
    };

    let dsn = session.qualify_dsn(&raw_dsn);
    let path = session.resolve_dsn(&dsn);

    let mut lines = vec![dsn.clone()];

    if !path.exists() {
        lines.push(format!("  DATA SET {dsn} NOT IN CATALOG"));
        return CommandResult { output: lines, rc: 4 };
    }

    // Show status info
    let meta = std::fs::metadata(&path);
    if cmd.has_flag("STATUS") {
        if path.is_dir() {
            lines.push("  DSORG=PO  RECFM=U".to_string());
        } else if let Ok(m) = &meta {
            lines.push(format!("  DSORG=PS  BYTES={}", m.len()));
        }
    }

    // Show members if PDS (directory)
    if cmd.has_flag("MEMBERS") && path.is_dir() {
        lines.push("--MEMBERS--".to_string());
        if let Ok(entries) = std::fs::read_dir(&path) {
            let mut member_names: Vec<String> = entries
                .filter_map(|e| e.ok())
                .map(|e| e.file_name().to_string_lossy().to_string())
                .collect();
            member_names.sort();
            for name in &member_names {
                lines.push(format!("  {name}"));
            }
        }
    }

    CommandResult::ok(lines)
}

// ---------------------------------------------------------------------------
// LISTALC (T100.3)
// ---------------------------------------------------------------------------

fn cmd_listalc(session: &mut TsoSession, cmd: &ParsedCommand) -> CommandResult {
    let allocs = session.list_allocs();
    if allocs.is_empty() {
        return CommandResult::ok(vec!["IKJ56246I NO DATA SETS CURRENTLY ALLOCATED".to_string()]);
    }

    let show_status = cmd.has_flag("STATUS");
    let mut lines = Vec::new();

    for alloc in &allocs {
        let mut line = format!("  {} {}", alloc.dd_name, alloc.dsn);
        if show_status {
            line.push_str(&format!("  DISP={}", alloc.disp));
            if let Some(lrecl) = alloc.dcb.lrecl {
                line.push_str(&format!("  LRECL={lrecl}"));
            }
        }
        lines.push(line);
    }

    CommandResult::ok(lines)
}

// ---------------------------------------------------------------------------
// PROFILE (T100.4)
// ---------------------------------------------------------------------------

fn cmd_profile(session: &mut TsoSession, cmd: &ParsedCommand) -> CommandResult {
    let mut changed = false;

    if let Some(prefix) = cmd.keyword("PREFIX") {
        session.profile.prefix = Some(prefix.to_ascii_uppercase());
        changed = true;
    }
    if cmd.has_flag("NOPREFIX") {
        session.profile.prefix = None;
        changed = true;
    }
    if cmd.has_flag("MSGID") {
        session.profile.msgid = true;
        changed = true;
    }
    if cmd.has_flag("NOMSGID") {
        session.profile.msgid = false;
        changed = true;
    }
    if cmd.has_flag("WTPMSG") {
        session.profile.wtpmsg = true;
        changed = true;
    }
    if cmd.has_flag("NOWTPMSG") {
        session.profile.wtpmsg = false;
        changed = true;
    }
    if cmd.has_flag("INTERCOM") {
        session.profile.intercom = true;
        changed = true;
    }
    if cmd.has_flag("NOINTERCOM") {
        session.profile.intercom = false;
        changed = true;
    }
    if cmd.has_flag("RECOVER") {
        session.profile.recover = true;
        changed = true;
    }
    if cmd.has_flag("NORECOVER") {
        session.profile.recover = false;
        changed = true;
    }
    if let Some(v) = cmd.keyword("LINE") {
        if let Ok(n) = v.parse::<u32>() {
            session.profile.line_size = n;
            changed = true;
        }
    }

    // Display current profile
    let mut lines = Vec::new();
    if !changed {
        lines.push("IKJ56688I CURRENT SETTINGS:".to_string());
    }
    let prefix_display = session
        .profile
        .prefix
        .as_deref()
        .unwrap_or("(NONE)");
    lines.push(format!(
        "  PREFIX({prefix_display}) {} {} LINE({}) RECOVER({})",
        if session.profile.msgid { "MSGID" } else { "NOMSGID" },
        if session.profile.wtpmsg { "WTPMSG" } else { "NOWTPMSG" },
        session.profile.line_size,
        if session.profile.recover { "ON" } else { "OFF" },
    ));

    CommandResult::ok(lines)
}

// ---------------------------------------------------------------------------
// DELETE (T100.5)
// ---------------------------------------------------------------------------

fn cmd_delete(session: &mut TsoSession, cmd: &ParsedCommand) -> CommandResult {
    let raw_dsn = match cmd.first_positional().or_else(|| cmd.keyword("DA")) {
        Some(d) => d.to_string(),
        None => return CommandResult::error("IKJ56702I MISSING DATASET NAME"),
    };

    let dsn = session.qualify_dsn(&raw_dsn);
    let path = session.resolve_dsn(&dsn);

    if !path.exists() {
        return CommandResult::error(&format!("IKJ56704I DATA SET {dsn} NOT FOUND"));
    }

    let result = if path.is_dir() {
        std::fs::remove_dir_all(&path)
    } else {
        std::fs::remove_file(&path)
    };

    match result {
        Ok(()) => CommandResult::ok(vec![format!("IKJ56253I DATA SET {dsn} DELETED")]),
        Err(e) => CommandResult::error(&format!("IKJ56870I DELETE FAILED - {e}")),
    }
}

// ---------------------------------------------------------------------------
// RENAME (T100.5)
// ---------------------------------------------------------------------------

fn cmd_rename(session: &mut TsoSession, cmd: &ParsedCommand) -> CommandResult {
    if cmd.positional.len() < 2 {
        return CommandResult::error("IKJ56702I MISSING OLD OR NEW DATASET NAME");
    }

    let old_dsn = session.qualify_dsn(&cmd.positional[0]);
    let new_dsn = session.qualify_dsn(&cmd.positional[1]);

    let old_path = session.resolve_dsn(&old_dsn);
    let new_path = session.resolve_dsn(&new_dsn);

    if !old_path.exists() {
        return CommandResult::error(&format!("IKJ56704I DATA SET {old_dsn} NOT FOUND"));
    }

    // Create parent dirs for new path
    if let Some(parent) = new_path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }

    match std::fs::rename(&old_path, &new_path) {
        Ok(()) => CommandResult::ok(vec![
            format!("IKJ56254I DATA SET {old_dsn} RENAMED TO {new_dsn}"),
        ]),
        Err(e) => CommandResult::error(&format!("IKJ56870I RENAME FAILED - {e}")),
    }
}

// ---------------------------------------------------------------------------
// ALTLIB (T100.5)
// ---------------------------------------------------------------------------

fn cmd_altlib(session: &mut TsoSession, cmd: &ParsedCommand) -> CommandResult {
    let action = cmd
        .first_positional()
        .or_else(|| {
            // ACTIVATE/DEACTIVATE/DISPLAY may be parsed as flags
            if cmd.has_flag("ACTIVATE") { Some("ACTIVATE") }
            else if cmd.has_flag("DEACTIVATE") { Some("DEACTIVATE") }
            else if cmd.has_flag("DISPLAY") { Some("DISPLAY") }
            else { None }
        })
        .unwrap_or("DISPLAY");

    match action.to_ascii_uppercase().as_str() {
        "ACTIVATE" => {
            let level_str = cmd.keyword("APPLICATION").or_else(|| cmd.keyword("USER")).or_else(|| cmd.keyword("SYSTEM"));
            let level = if cmd.keyword("APPLICATION").is_some() {
                AltlibLevel::Application
            } else if cmd.keyword("USER").is_some() {
                AltlibLevel::User
            } else if cmd.keyword("SYSTEM").is_some() {
                AltlibLevel::System
            } else {
                return CommandResult::error("IKJ56702I MISSING LIBRARY LEVEL");
            };

            let lib_type = match level_str {
                Some(t) => match t.to_ascii_uppercase().as_str() {
                    "EXEC" => AltlibType::Exec,
                    "CLIST" => AltlibType::Clist,
                    _ => AltlibType::Exec,
                },
                None => AltlibType::Exec,
            };

            let dsn = match cmd.keyword("DA").or_else(|| cmd.keyword("DATASET")) {
                Some(d) => session.qualify_dsn(d),
                None => return CommandResult::error("IKJ56702I MISSING DATASET OPERAND"),
            };

            session.altlib_activate(AltlibEntry {
                level,
                lib_type,
                dsn: dsn.clone(),
            });

            CommandResult::ok(vec![format!("IKJ56241I ALTLIB ACTIVATED - {dsn}")])
        }
        "DEACTIVATE" => {
            let level = if cmd.keyword("APPLICATION").is_some() {
                AltlibLevel::Application
            } else if cmd.keyword("USER").is_some() {
                AltlibLevel::User
            } else {
                return CommandResult::error("IKJ56702I MISSING LIBRARY LEVEL");
            };

            let lib_type = cmd
                .keyword("APPLICATION")
                .or_else(|| cmd.keyword("USER"))
                .map(|t| match t.to_ascii_uppercase().as_str() {
                    "CLIST" => AltlibType::Clist,
                    _ => AltlibType::Exec,
                })
                .unwrap_or(AltlibType::Exec);

            if session.altlib_deactivate(&level, &lib_type) {
                CommandResult::ok(vec!["IKJ56242I ALTLIB DEACTIVATED".to_string()])
            } else {
                CommandResult { output: vec!["IKJ56243I NO ALTLIB ACTIVE AT SPECIFIED LEVEL".to_string()], rc: 4 }
            }
        }
        _ => {
            let entries = session.altlib_list();
            if entries.is_empty() {
                CommandResult::ok(vec!["IKJ56244I NO ALTLIB ENTRIES ACTIVE".to_string()])
            } else {
                let lines: Vec<String> = entries
                    .iter()
                    .map(|e| format!("  {:?} {:?} {}", e.level, e.lib_type, e.dsn))
                    .collect();
                CommandResult::ok(lines)
            }
        }
    }
}

// ---------------------------------------------------------------------------
// HELP (T100.4)
// ---------------------------------------------------------------------------

fn cmd_help(cmd: &ParsedCommand) -> CommandResult {
    let topic = cmd.first_positional().unwrap_or("").to_ascii_uppercase();

    let text = match topic.as_str() {
        "ALLOCATE" | "ALLOC" => vec![
            "ALLOCATE - Allocate a data set to a DD name",
            "",
            "Syntax:",
            "  ALLOC DA('dsname') FILE(ddname) [SHR|OLD|NEW|MOD]",
            "        [SPACE(primary,secondary)] [TRACKS|CYLINDERS]",
            "        [LRECL(n)] [RECFM(fmt)] [BLKSIZE(n)]",
            "        [REUSE]",
            "",
            "Examples:",
            "  ALLOC DA('SYS1.MACLIB') FILE(MACLIB) SHR",
            "  ALLOC DA('USER01.NEW.DATA') FILE(OUT) NEW LRECL(80) RECFM(F,B)",
        ],
        "FREE" | "UNALLOC" => vec![
            "FREE - Deallocate a data set or DD name",
            "",
            "Syntax:",
            "  FREE FILE(ddname)",
            "  FREE DA('dsname')",
        ],
        "LISTDS" => vec![
            "LISTDS - Display data set information",
            "",
            "Syntax:",
            "  LISTDS 'dsname' [MEMBERS] [STATUS]",
        ],
        "LISTALC" => vec![
            "LISTALC - List current allocations",
            "",
            "Syntax:",
            "  LISTALC [STATUS]",
        ],
        "PROFILE" | "PROF" => vec![
            "PROFILE - Display or change session profile settings",
            "",
            "Syntax:",
            "  PROFILE [PREFIX(userid)] [NOPREFIX]",
            "          [MSGID|NOMSGID] [WTPMSG|NOWTPMSG]",
        ],
        "DELETE" | "DEL" => vec![
            "DELETE - Delete a data set",
            "",
            "Syntax:",
            "  DELETE 'dsname'",
        ],
        "RENAME" | "REN" => vec![
            "RENAME - Rename a data set",
            "",
            "Syntax:",
            "  RENAME 'old.dsname' 'new.dsname'",
        ],
        "ALTLIB" => vec![
            "ALTLIB - Manage alternative library search paths",
            "",
            "Syntax:",
            "  ALTLIB ACTIVATE APPLICATION(EXEC) DA('dsname')",
            "  ALTLIB DEACTIVATE APPLICATION(EXEC)",
            "  ALTLIB DISPLAY",
        ],
        "SUBMIT" | "SUB" => vec![
            "SUBMIT - Submit JCL for batch execution",
            "",
            "Syntax:",
            "  SUBMIT 'dsname'",
            "  SUBMIT dsname",
            "",
            "The JCL is read from the specified data set and submitted",
            "to JES2. The job name and ID are displayed upon submission.",
        ],
        "STATUS" | "ST" => vec![
            "STATUS - Display job status",
            "",
            "Syntax:",
            "  STATUS [jobname]",
            "",
            "Displays the status of jobs owned by the current user.",
            "If a job name is specified, only matching jobs are shown.",
        ],
        "CANCEL" | "CAN" => vec![
            "CANCEL - Cancel a batch job",
            "",
            "Syntax:",
            "  CANCEL jobname(JOBnnnnn)",
            "  CANCEL JOBnnnnn",
            "  CANCEL jobname",
        ],
        "OUTPUT" | "OUT" => vec![
            "OUTPUT - Retrieve job output from spool",
            "",
            "Syntax:",
            "  OUTPUT jobname(JOBnnnnn)",
            "  OUTPUT JOBnnnnn",
            "",
            "Displays SYSOUT datasets from the job's spool.",
        ],
        "EXEC" | "EX" => vec![
            "EXEC - Execute a REXX or CLIST exec",
            "",
            "Syntax:",
            "  EXEC 'dsname' ['arguments']",
            "  EXEC dsname ['arguments']",
            "",
            "Loads and executes a REXX or CLIST exec from the specified",
            "data set. REXX execs are identified by /* REXX */ header.",
        ],
        "CALL" => vec![
            "CALL - Invoke a program (load module)",
            "",
            "Syntax:",
            "  CALL 'dsname' ['parm']",
            "",
            "Loads and executes the named program.",
        ],
        "" => vec![
            "HELP - Display help for TSO commands",
            "",
            "Available commands:",
            "  ALLOCATE (ALLOC)    - Allocate a data set",
            "  FREE (UNALLOC)      - Free a DD allocation",
            "  LISTDS              - List data set information",
            "  LISTALC             - List current allocations",
            "  PROFILE (PROF)      - Session profile settings",
            "  DELETE (DEL)        - Delete a data set",
            "  RENAME (REN)        - Rename a data set",
            "  ALTLIB              - Manage library search paths",
            "  SUBMIT (SUB)        - Submit JCL for batch execution",
            "  STATUS (ST)         - Display job status",
            "  CANCEL (CAN)        - Cancel a batch job",
            "  OUTPUT (OUT)        - Retrieve job output",
            "  EXEC (EX)           - Execute a REXX/CLIST exec",
            "  CALL                - Invoke a program",
            "  HELP                - Display this help",
            "  TIME                - Display current time",
            "",
            "Use HELP commandname for details.",
        ],
        _ => {
            return CommandResult::ok(vec![
                format!("IKJ56700I HELP DATA UNAVAILABLE FOR {topic}"),
            ]);
        }
    };

    CommandResult::ok(text.iter().map(|s| s.to_string()).collect())
}

// ---------------------------------------------------------------------------
// TIME
// ---------------------------------------------------------------------------

fn cmd_time() -> CommandResult {
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    let secs = now.as_secs();
    let hours = (secs / 3600) % 24;
    let minutes = (secs / 60) % 60;
    let seconds = secs % 60;

    CommandResult::ok(vec![
        format!("IKJ56690I TIME-{hours:02}:{minutes:02}:{seconds:02}"),
    ])
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_command;

    fn test_session() -> TsoSession {
        let temp = std::env::temp_dir().join("tso_cmd_tests");
        let _ = std::fs::create_dir_all(&temp);
        TsoSession::new("USER01", temp)
    }

    #[test]
    fn test_allocate_shr() {
        let mut s = test_session();
        let cmd = parse_command("ALLOC DA('PROD.DATA') FILE(INFILE) SHR");
        let result = execute(&mut s, &cmd);
        assert_eq!(result.rc, 0);
        assert!(result.output[0].contains("ALLOCATED"));
        assert!(s.get_alloc("INFILE").is_some());
    }

    #[test]
    fn test_allocate_and_free() {
        let mut s = test_session();
        let cmd = parse_command("ALLOC DA('PROD.DATA') FILE(DD1) SHR");
        execute(&mut s, &cmd);
        assert_eq!(s.alloc_count(), 1);

        let cmd = parse_command("FREE FILE(DD1)");
        let result = execute(&mut s, &cmd);
        assert_eq!(result.rc, 0);
        assert!(result.output[0].contains("FREED"));
        assert_eq!(s.alloc_count(), 0);
    }

    #[test]
    fn test_allocate_new_with_dcb() {
        let mut s = test_session();
        let cmd = parse_command("ALLOC DA('USER01.NEW') FILE(OUT) NEW LRECL(80) RECFM(F,B) BLKSIZE(3120)");
        let result = execute(&mut s, &cmd);
        assert_eq!(result.rc, 0);

        let alloc = s.get_alloc("OUT").unwrap();
        assert_eq!(alloc.disp, AllocDisp::New);
        assert_eq!(alloc.dcb.lrecl, Some(80));
        assert_eq!(alloc.dcb.recfm.as_deref(), Some("F,B"));
        assert_eq!(alloc.dcb.blksize, Some(3120));
    }

    #[test]
    fn test_allocate_duplicate_error() {
        let mut s = test_session();
        let cmd = parse_command("ALLOC DA('A.B') FILE(DD1) SHR");
        execute(&mut s, &cmd);

        let cmd2 = parse_command("ALLOC DA('C.D') FILE(DD1) SHR");
        let result = execute(&mut s, &cmd2);
        assert!(result.rc > 0);
        assert!(result.output[0].contains("ALREADY ALLOCATED"));
    }

    #[test]
    fn test_allocate_reuse() {
        let mut s = test_session();
        let cmd = parse_command("ALLOC DA('A.B') FILE(DD1) SHR");
        execute(&mut s, &cmd);

        let cmd2 = parse_command("ALLOC DA('C.D') FILE(DD1) SHR REUSE");
        let result = execute(&mut s, &cmd2);
        assert_eq!(result.rc, 0);
        // Parser strips quotes from DA value, so qualify_dsn adds prefix
        assert_eq!(s.get_alloc("DD1").unwrap().dsn, "USER01.C.D");
    }

    #[test]
    fn test_free_not_allocated() {
        let mut s = test_session();
        let cmd = parse_command("FREE FILE(NODD)");
        let result = execute(&mut s, &cmd);
        assert!(result.rc > 0);
    }

    #[test]
    fn test_listalc() {
        let mut s = test_session();
        let cmd = parse_command("ALLOC DA('A.B') FILE(DD1) SHR");
        execute(&mut s, &cmd);
        let cmd = parse_command("ALLOC DA('C.D') FILE(DD2) OLD");
        execute(&mut s, &cmd);

        let cmd = parse_command("LISTALC STATUS");
        let result = execute(&mut s, &cmd);
        assert_eq!(result.rc, 0);
        assert!(result.output.len() >= 2);
    }

    #[test]
    fn test_listalc_empty() {
        let mut s = test_session();
        let cmd = parse_command("LISTALC");
        let result = execute(&mut s, &cmd);
        assert!(result.output[0].contains("NO DATA SETS"));
    }

    #[test]
    fn test_profile_display() {
        let mut s = test_session();
        let cmd = parse_command("PROFILE");
        let result = execute(&mut s, &cmd);
        assert_eq!(result.rc, 0);
        assert!(result.output.iter().any(|l| l.contains("PREFIX(USER01)")));
    }

    #[test]
    fn test_profile_change_prefix() {
        let mut s = test_session();
        let cmd = parse_command("PROFILE PREFIX(NEWPFX)");
        execute(&mut s, &cmd);
        assert_eq!(s.profile.prefix, Some("NEWPFX".to_string()));
    }

    #[test]
    fn test_profile_noprefix() {
        let mut s = test_session();
        let cmd = parse_command("PROFILE NOPREFIX");
        execute(&mut s, &cmd);
        assert_eq!(s.profile.prefix, None);
    }

    #[test]
    fn test_profile_nomsgid() {
        let mut s = test_session();
        let cmd = parse_command("PROFILE NOMSGID");
        execute(&mut s, &cmd);
        assert!(!s.profile.msgid);
    }

    #[test]
    fn test_delete() {
        let mut s = test_session();
        // qualify_dsn will add prefix USER01, so we need the full path
        let qualified = s.qualify_dsn("TEST.DELETE");
        let path = s.resolve_dsn(&qualified);
        if let Some(parent) = path.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        std::fs::write(&path, "data").unwrap();
        assert!(path.exists());

        // Unquoted positional → qualify adds prefix
        let cmd = parse_command("DELETE TEST.DELETE");
        let result = execute(&mut s, &cmd);
        assert_eq!(result.rc, 0, "output: {:?}", result.output);
        assert!(!path.exists());
    }

    #[test]
    fn test_delete_not_found() {
        let mut s = test_session();
        let cmd = parse_command("DELETE 'NO.SUCH.DATA'");
        let result = execute(&mut s, &cmd);
        assert!(result.rc > 0);
        assert!(result.output[0].contains("NOT FOUND"));
    }

    #[test]
    fn test_rename() {
        let mut s = test_session();
        let old_q = s.qualify_dsn("RENAME.OLD");
        let new_q = s.qualify_dsn("RENAME.NEW");
        let old_path = s.resolve_dsn(&old_q);
        let new_path = s.resolve_dsn(&new_q);
        if let Some(parent) = old_path.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        std::fs::write(&old_path, "data").unwrap();

        // Unquoted positionals → qualify adds prefix
        let cmd = parse_command("RENAME RENAME.OLD RENAME.NEW");
        let result = execute(&mut s, &cmd);
        assert_eq!(result.rc, 0, "output: {:?}", result.output);
        assert!(!old_path.exists());
        assert!(new_path.exists());

        let _ = std::fs::remove_file(&new_path);
    }

    #[test]
    fn test_altlib_activate() {
        let mut s = test_session();
        let cmd = parse_command("ALTLIB ACTIVATE APPLICATION(EXEC) DA('USER01.MY.EXEC')");
        let result = execute(&mut s, &cmd);
        assert_eq!(result.rc, 0);
        assert!(result.output[0].contains("ACTIVATED"));
        assert_eq!(s.altlib_list().len(), 1);
    }

    #[test]
    fn test_help() {
        let mut s = test_session();
        let cmd = parse_command("HELP ALLOCATE");
        let result = execute(&mut s, &cmd);
        assert_eq!(result.rc, 0);
        assert!(result.output.iter().any(|l| l.contains("Allocate")));
    }

    #[test]
    fn test_help_no_topic() {
        let mut s = test_session();
        let cmd = parse_command("HELP");
        let result = execute(&mut s, &cmd);
        assert_eq!(result.rc, 0);
        assert!(result.output.iter().any(|l| l.contains("Available commands")));
    }

    #[test]
    fn test_unknown_command() {
        let mut s = test_session();
        let cmd = parse_command("XYZZY");
        let result = execute(&mut s, &cmd);
        assert!(result.rc > 0);
        assert!(result.output[0].contains("NOT FOUND"));
    }

    #[test]
    fn test_time() {
        let mut s = test_session();
        let cmd = parse_command("TIME");
        let result = execute(&mut s, &cmd);
        assert_eq!(result.rc, 0);
        assert!(result.output[0].contains("TIME-"));
    }

    #[test]
    fn test_listds_not_in_catalog() {
        let mut s = test_session();
        let cmd = parse_command("LISTDS 'NO.SUCH.DATASET'");
        let result = execute(&mut s, &cmd);
        assert_eq!(result.rc, 4);
        assert!(result.output.iter().any(|l| l.contains("NOT IN CATALOG")));
    }

    #[test]
    fn test_listds_with_members() {
        let mut s = test_session();
        // qualify_dsn adds prefix, so use qualified path
        let qualified = s.qualify_dsn("TEST.PDS");
        let path = s.resolve_dsn(&qualified);
        std::fs::create_dir_all(&path).unwrap();
        std::fs::write(path.join("MEM1"), "data").unwrap();
        std::fs::write(path.join("MEM2"), "data").unwrap();

        // Unquoted positional → qualify adds prefix USER01
        let cmd = parse_command("LISTDS TEST.PDS MEMBERS STATUS");
        let result = execute(&mut s, &cmd);
        assert_eq!(result.rc, 0, "output: {:?}", result.output);
        assert!(result.output.iter().any(|l| l.contains("--MEMBERS--")));
        assert!(result.output.iter().any(|l| l.contains("MEM1")));
        assert!(result.output.iter().any(|l| l.contains("MEM2")));

        let _ = std::fs::remove_dir_all(&path);
    }
}
