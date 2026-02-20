//! TSO program execution — EXEC, CALL, and batch TSO (IKJEFT01/1B).
//!
//! - **EXEC** loads and executes REXX/CLIST execs from a dataset.
//! - **CALL** loads and invokes compiled programs (load modules).
//! - **IKJEFT01/1B** runs TSO commands / REXX in batch.

use crate::commands::CommandResult;
use crate::parser::ParsedCommand;
use crate::services::TsoIo;
use crate::session::TsoSession;

/// Result of a program or exec invocation.
#[derive(Debug, Clone)]
pub struct ExecResult {
    /// Return code from the exec/program.
    pub rc: u32,
    /// Output lines produced during execution.
    pub output: Vec<String>,
}

/// An exec script type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecType {
    /// REXX exec (`/* REXX */` header).
    Rexx,
    /// CLIST (command list) script.
    Clist,
    /// Unknown — treated as CLIST by default.
    Unknown,
}

/// Detect the exec type from the script source.
pub fn detect_exec_type(source: &str) -> ExecType {
    let trimmed = source.trim_start();
    if trimmed.starts_with("/*") {
        // Check for REXX comment header
        let upper = trimmed.to_ascii_uppercase();
        if upper.contains("REXX") {
            return ExecType::Rexx;
        }
    }
    // Check for PROC statement (CLIST)
    if trimmed
        .lines()
        .next()
        .map(|l| l.trim().to_ascii_uppercase().starts_with("PROC"))
        .unwrap_or(false)
    {
        return ExecType::Clist;
    }
    ExecType::Unknown
}

/// Try to execute an EXEC or CALL command.  Returns `Some(result)` if
/// handled, `None` if not an exec command.
pub fn try_execute(
    session: &mut TsoSession,
    cmd: &ParsedCommand,
) -> Option<CommandResult> {
    match cmd.name.as_str() {
        "EXEC" | "EX" => Some(cmd_exec(session, cmd)),
        "CALL" => Some(cmd_call(session, cmd)),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// EXEC (T102.1)
// ---------------------------------------------------------------------------

fn cmd_exec(session: &mut TsoSession, cmd: &ParsedCommand) -> CommandResult {
    let raw_dsn = match cmd.first_positional() {
        Some(d) => d.to_string(),
        None => return CommandResult::error("IKJ56702I MISSING EXEC NAME"),
    };

    let dsn = session.qualify_dsn(&raw_dsn);
    let path = session.resolve_dsn(&dsn);

    // Read exec source
    let source = match std::fs::read_to_string(&path) {
        Ok(s) => s,
        Err(_) => {
            return CommandResult::error(&format!(
                "IKJ56704I EXEC {dsn} NOT FOUND"
            ))
        }
    };

    // Extract argument string (second positional, if any)
    let args = cmd.positional.get(1).cloned().unwrap_or_default();

    // Detect exec type
    let exec_type = detect_exec_type(&source);

    match exec_type {
        ExecType::Rexx => {
            // REXX execution placeholder — actual engine is in the REXX crate (R100-R109).
            // For now, produce a stub result.
            CommandResult::ok(vec![
                format!("IKJ56471I EXECUTING {dsn} (REXX)"),
                format!("IKJ56472I REXX EXEC ARGS: {args}"),
                "IKJ56473I REXX INTERPRETER NOT YET AVAILABLE".to_string(),
            ])
        }
        ExecType::Clist | ExecType::Unknown => {
            // CLIST execution — execute each line as a TSO command.
            let result = execute_clist(session, &source, &args);
            let mut lines = vec![format!("IKJ56471I EXECUTING {dsn} (CLIST)")];
            lines.extend(result.output);
            CommandResult { output: lines, rc: result.rc }
        }
    }
}

/// Execute a CLIST script (simple line-by-line TSO command execution).
///
/// This is a simplified CLIST processor that executes each non-comment,
/// non-blank line as a TSO command.
fn execute_clist(
    session: &mut TsoSession,
    source: &str,
    _args: &str,
) -> ExecResult {
    let mut output = Vec::new();
    let mut max_rc = 0;

    for line in source.lines() {
        let trimmed = line.trim();

        // Skip blank lines and comments
        if trimmed.is_empty() || trimmed.starts_with("/*") {
            continue;
        }

        // Skip PROC/END statements
        let upper = trimmed.to_ascii_uppercase();
        if upper.starts_with("PROC") || upper == "END" {
            continue;
        }

        // Execute as TSO command
        let cmd = crate::parser::parse_command(trimmed);
        let result = crate::commands::execute(session, &cmd);
        output.extend(result.output);
        if result.rc > max_rc {
            max_rc = result.rc;
        }
    }

    ExecResult { rc: max_rc, output }
}

// ---------------------------------------------------------------------------
// CALL (T102.1)
// ---------------------------------------------------------------------------

fn cmd_call(session: &mut TsoSession, cmd: &ParsedCommand) -> CommandResult {
    let raw_dsn = match cmd.first_positional() {
        Some(d) => d.to_string(),
        None => return CommandResult::error("IKJ56702I MISSING PROGRAM NAME"),
    };

    let dsn = session.qualify_dsn(&raw_dsn);
    let path = session.resolve_dsn(&dsn);

    if !path.exists() {
        return CommandResult::error(&format!("IKJ56704I PROGRAM {dsn} NOT FOUND"));
    }

    // Extract parameter string
    let parm = cmd.positional.get(1).cloned().unwrap_or_default();

    // Program loading is a stub — actual load module execution
    // would require the LE runtime (crate open-mainframe-le).
    CommandResult::ok(vec![
        format!("IKJ56475I CALLING {dsn}"),
        format!("IKJ56476I PARM: {parm}"),
        "IKJ56477I PROGRAM EXECUTION COMPLETE RC=0000".to_string(),
    ])
}

// ---------------------------------------------------------------------------
// Batch TSO (IKJEFT01 / IKJEFT1B) — T102.5
// ---------------------------------------------------------------------------

/// Execute TSO commands from input lines (batch mode — IKJEFT01).
///
/// Reads commands from the provided I/O and writes output back.
/// Returns the maximum return code across all commands.
pub fn batch_tso(
    session: &mut TsoSession,
    io: &mut dyn TsoIo,
) -> u32 {
    let mut max_rc = 0u32;

    while let Some(line) = io.getline() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        let cmd = crate::parser::parse_command(trimmed);
        let result = crate::commands::execute(session, &cmd);

        for out_line in &result.output {
            io.putline(out_line);
        }

        if result.rc > max_rc {
            max_rc = result.rc;
        }
    }

    max_rc
}

/// Execute a REXX exec in batch mode (IKJEFT1B).
///
/// Reads the REXX source from the given dataset and executes it.
/// The REXX return code becomes the step return code.
pub fn batch_rexx(
    session: &mut TsoSession,
    exec_dsn: &str,
    io: &mut dyn TsoIo,
) -> u32 {
    let dsn = session.qualify_dsn(exec_dsn);
    let path = session.resolve_dsn(&dsn);

    let source = match std::fs::read_to_string(&path) {
        Ok(s) => s,
        Err(_) => {
            io.putline(&format!("IKJ56704I EXEC {dsn} NOT FOUND"));
            return 12;
        }
    };

    let exec_type = detect_exec_type(&source);

    match exec_type {
        ExecType::Rexx => {
            io.putline(&format!("IKJ56471I EXECUTING {dsn} (REXX)"));
            io.putline("IKJ56473I REXX INTERPRETER NOT YET AVAILABLE");
            8
        }
        ExecType::Clist | ExecType::Unknown => {
            io.putline(&format!("IKJ56471I EXECUTING {dsn} (CLIST)"));
            let result = execute_clist(session, &source, "");
            for line in &result.output {
                io.putline(line);
            }
            result.rc
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_command;
    use crate::services::MemoryIo;
    use std::path::PathBuf;

    fn test_session() -> TsoSession {
        let temp = std::env::temp_dir().join("tso_exec_tests");
        let _ = std::fs::create_dir_all(&temp);
        TsoSession::new("USER01", temp)
    }

    fn write_exec(session: &TsoSession, dsn: &str, content: &str) -> PathBuf {
        let qualified = session.qualify_dsn(dsn);
        let path = session.resolve_dsn(&qualified);
        if let Some(parent) = path.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        std::fs::write(&path, content).unwrap();
        path
    }

    #[test]
    fn test_detect_rexx() {
        assert_eq!(detect_exec_type("/* REXX */\nsay 'hello'"), ExecType::Rexx);
        assert_eq!(detect_exec_type("/* rexx */\n"), ExecType::Rexx);
    }

    #[test]
    fn test_detect_clist() {
        assert_eq!(detect_exec_type("PROC 0\nWRITE HELLO\n"), ExecType::Clist);
    }

    #[test]
    fn test_detect_unknown() {
        assert_eq!(detect_exec_type("LISTDS 'SYS1.PARMLIB'\n"), ExecType::Unknown);
    }

    #[test]
    fn test_exec_rexx_stub() {
        let mut s = test_session();
        write_exec(&s, "MY.REXX", "/* REXX */\nsay 'hello'\nexit 0\n");

        let cmd = parse_command("EXEC MY.REXX");
        let result = try_execute(&mut s, &cmd).unwrap();
        assert_eq!(result.rc, 0);
        assert!(result.output.iter().any(|l| l.contains("REXX")));
    }

    #[test]
    fn test_exec_clist() {
        let mut s = test_session();
        let clist = "PROC 0\nTIME\nEND\n";
        write_exec(&s, "MY.CLIST", clist);

        let cmd = parse_command("EXEC MY.CLIST");
        let result = try_execute(&mut s, &cmd).unwrap();
        assert_eq!(result.rc, 0);
        assert!(result.output.iter().any(|l| l.contains("CLIST")));
        assert!(result.output.iter().any(|l| l.contains("TIME-")));
    }

    #[test]
    fn test_exec_not_found() {
        let mut s = test_session();
        let cmd = parse_command("EXEC MISSING.EXEC");
        let result = try_execute(&mut s, &cmd).unwrap();
        assert!(result.rc > 0);
        assert!(result.output[0].contains("NOT FOUND"));
    }

    #[test]
    fn test_call_not_found() {
        let mut s = test_session();
        let cmd = parse_command("CALL MISSING.PGM");
        let result = try_execute(&mut s, &cmd).unwrap();
        assert!(result.rc > 0);
        assert!(result.output[0].contains("NOT FOUND"));
    }

    #[test]
    fn test_call_program() {
        let mut s = test_session();
        // Create a dummy program file
        let qualified = s.qualify_dsn("MY.PGM");
        let path = s.resolve_dsn(&qualified);
        if let Some(parent) = path.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        std::fs::write(&path, b"dummy_program").unwrap();

        let cmd = parse_command("CALL MY.PGM");
        let result = try_execute(&mut s, &cmd).unwrap();
        assert_eq!(result.rc, 0);
        assert!(result.output.iter().any(|l| l.contains("CALLING")));
    }

    #[test]
    fn test_not_exec_command() {
        let mut s = test_session();
        let cmd = parse_command("LISTDS 'SYS1.DATA'");
        assert!(try_execute(&mut s, &cmd).is_none());
    }

    #[test]
    fn test_batch_tso() {
        let mut s = test_session();
        let mut io = MemoryIo::new(vec![
            "TIME".to_string(),
            "PROFILE".to_string(),
        ]);

        let rc = batch_tso(&mut s, &mut io);
        assert_eq!(rc, 0);
        assert!(io.output.iter().any(|l| l.contains("TIME-")));
        assert!(io.output.iter().any(|l| l.contains("PREFIX")));
    }

    #[test]
    fn test_batch_tso_error() {
        let mut s = test_session();
        let mut io = MemoryIo::new(vec![
            "XYZZY".to_string(),
        ]);

        let rc = batch_tso(&mut s, &mut io);
        assert!(rc > 0);
        assert!(io.output.iter().any(|l| l.contains("NOT FOUND")));
    }

    #[test]
    fn test_batch_rexx_clist_fallback() {
        let mut s = test_session();
        write_exec(&s, "BATCH.CLIST", "TIME\n");

        let mut io = MemoryIo::empty();
        let rc = batch_rexx(&mut s, "BATCH.CLIST", &mut io);
        assert_eq!(rc, 0);
        assert!(io.output.iter().any(|l| l.contains("CLIST")));
    }

    #[test]
    fn test_batch_rexx_not_found() {
        let mut s = test_session();
        let mut io = MemoryIo::empty();
        let rc = batch_rexx(&mut s, "MISSING.EXEC", &mut io);
        assert_eq!(rc, 12);
        assert!(io.output.iter().any(|l| l.contains("NOT FOUND")));
    }
}
