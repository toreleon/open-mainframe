//! # TSO/REXX/USS Batch Processors
//!
//! Batch execution utilities for running TSO commands, REXX execs, and
//! USS (Unix System Services) commands from JCL.
//!
//! ## Programs
//!
//! - **IKJEFT01** — TSO command execution in batch (SYSTSIN/SYSTSPRT)
//! - **IKJEFT1A** — Authorized TSO batch (APF-authorized commands)
//! - **IKJEFT1B** — Non-authorized TSO batch
//! - **IRXJCL** — REXX exec execution in batch (SYSTSIN/SYSTSPRT)
//! - **BPXBATCH** — USS command/script execution (STDPARM/STDIN/STDOUT/STDERR)
//!
//! ## DD Conventions
//!
//! | Program  | Input DD  | Output DD | Error DD |
//! |----------|-----------|-----------|----------|
//! | IKJEFT01 | SYSTSIN   | SYSTSPRT  | —        |
//! | IKJEFT1A | SYSTSIN   | SYSTSPRT  | —        |
//! | IKJEFT1B | SYSTSIN   | SYSTSPRT  | —        |
//! | IRXJCL   | SYSTSIN   | SYSTSPRT  | —        |
//! | BPXBATCH | STDPARM   | STDOUT    | STDERR   |

use crate::{
    format_message_id, MessageSeverity, UtilityContext, UtilityMessage, UtilityProgram,
    UtilityResult,
};

// ─────────────────────── Authorization Level ───────────────────────

/// TSO batch authorization level.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TsoAuthLevel {
    /// Standard — IKJEFT01.
    Standard,
    /// Authorized — IKJEFT1A (APF-authorized commands allowed).
    Authorized,
    /// Non-authorized — IKJEFT1B (explicitly non-authorized).
    NonAuthorized,
}

// ─────────────────────── IKJEFT01 / TSO Batch ───────────────────────

/// IKJEFT01 — TSO Command Processor in batch mode.
///
/// Reads TSO commands from SYSTSIN DD, executes them, and writes
/// output to SYSTSPRT. In our simulation, commands are echoed with
/// their execution status.
pub struct Ikjeft01;

/// IKJEFT1A — Authorized TSO batch.
pub struct Ikjeft1a;

/// IKJEFT1B — Non-authorized TSO batch.
pub struct Ikjeft1b;

fn execute_tso_batch(
    context: &mut UtilityContext,
    program_name: &str,
    auth_level: TsoAuthLevel,
) -> UtilityResult {
    let auth_desc = match auth_level {
        TsoAuthLevel::Standard => "STANDARD",
        TsoAuthLevel::Authorized => "AUTHORIZED",
        TsoAuthLevel::NonAuthorized => "NON-AUTHORIZED",
    };

    // Read commands from SYSTSIN
    let commands = context
        .get_dd("SYSTSIN")
        .and_then(|dd| dd.inline_data.clone())
        .unwrap_or_default();

    if commands.is_empty() {
        let msg = UtilityMessage::error(
            &format_message_id("IKJ", 56, MessageSeverity::Error),
            &format!("{program_name} — SYSTSIN IS EMPTY"),
        );
        context.write_utility_message(&msg);
        return UtilityResult::new(12, vec![msg]);
    }

    let mut messages = Vec::new();
    let cc: u32 = 0;

    // Header
    let hdr = UtilityMessage::info(
        &format_message_id("IKJ", 1, MessageSeverity::Info),
        &format!("{program_name} TSO BATCH PROCESSOR — {auth_desc} MODE"),
    );
    context.write_utility_message(&hdr);
    messages.push(hdr);

    // Process each command
    let mut cmd_count = 0u32;
    for line in &commands {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('*') {
            continue;
        }

        // Parse the TSO command verb (first token)
        let verb = trimmed
            .split_whitespace()
            .next()
            .unwrap_or("")
            .to_uppercase();

        // Check for END command
        if verb == "END" {
            let msg = UtilityMessage::info(
                &format_message_id("IKJ", 10, MessageSeverity::Info),
                "END COMMAND — TSO SESSION TERMINATED",
            );
            context.write_utility_message(&msg);
            messages.push(msg);
            break;
        }

        cmd_count += 1;

        // Echo the command execution
        let msg = UtilityMessage::info(
            &format_message_id("IKJ", 2, MessageSeverity::Info),
            &format!("EXECUTING: {trimmed}"),
        );
        context.write_utility_message(&msg);
        messages.push(msg);

        // Write command output to SYSTSPRT
        if let Some(dd) = context.get_dd_mut("SYSTSPRT") {
            dd.output.push(format!("TSO CMD: {trimmed}"));
            dd.output.push("  RC=0 — COMMAND COMPLETE".to_string());
        }
    }

    // Summary
    let summary = UtilityMessage::info(
        &format_message_id("IKJ", 50, MessageSeverity::Info),
        &format!("{cmd_count} TSO COMMAND(S) PROCESSED — HIGHEST CC={cc}"),
    );
    context.write_utility_message(&summary);
    messages.push(summary);

    UtilityResult::new(cc, messages)
}

impl UtilityProgram for Ikjeft01 {
    fn name(&self) -> &str {
        "IKJEFT01"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        execute_tso_batch(context, "IKJEFT01", TsoAuthLevel::Standard)
    }
}

impl UtilityProgram for Ikjeft1a {
    fn name(&self) -> &str {
        "IKJEFT1A"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        execute_tso_batch(context, "IKJEFT1A", TsoAuthLevel::Authorized)
    }
}

impl UtilityProgram for Ikjeft1b {
    fn name(&self) -> &str {
        "IKJEFT1B"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        execute_tso_batch(context, "IKJEFT1B", TsoAuthLevel::NonAuthorized)
    }
}

// ─────────────────────── IRXJCL / REXX Batch ───────────────────────

/// IRXJCL — REXX Exec Execution in batch mode.
///
/// Reads REXX exec name/commands from SYSTSIN DD, simulates execution,
/// and writes output to SYSTSPRT.
pub struct Irxjcl;

impl UtilityProgram for Irxjcl {
    fn name(&self) -> &str {
        "IRXJCL"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        // Read exec name/commands from SYSTSIN
        let commands = context
            .get_dd("SYSTSIN")
            .and_then(|dd| dd.inline_data.clone())
            .unwrap_or_default();

        if commands.is_empty() {
            let msg = UtilityMessage::error(
                &format_message_id("IRX", 56, MessageSeverity::Error),
                "IRXJCL — SYSTSIN IS EMPTY",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(12, vec![msg]);
        }

        let mut messages = Vec::new();

        let hdr = UtilityMessage::info(
            &format_message_id("IRX", 1, MessageSeverity::Info),
            "IRXJCL REXX BATCH PROCESSOR INITIALIZED",
        );
        context.write_utility_message(&hdr);
        messages.push(hdr);

        let mut exec_count = 0u32;

        for line in &commands {
            let trimmed = line.trim();
            if trimmed.is_empty() || trimmed.starts_with('*') {
                continue;
            }

            exec_count += 1;

            // REXX exec name (may be prefixed with % per convention)
            let exec_name = trimmed.strip_prefix('%').unwrap_or(trimmed);
            let exec_upper = exec_name.split_whitespace().next().unwrap_or("").to_uppercase();

            let msg = UtilityMessage::info(
                &format_message_id("IRX", 2, MessageSeverity::Info),
                &format!("EXECUTING REXX EXEC: {exec_upper}"),
            );
            context.write_utility_message(&msg);
            messages.push(msg);

            // Write exec output to SYSTSPRT
            if let Some(dd) = context.get_dd_mut("SYSTSPRT") {
                dd.output.push(format!("REXX EXEC: {exec_upper}"));
                dd.output.push("  RC=0 — EXEC COMPLETE".to_string());
            }
        }

        let summary = UtilityMessage::info(
            &format_message_id("IRX", 50, MessageSeverity::Info),
            &format!("{exec_count} REXX EXEC(S) PROCESSED"),
        );
        context.write_utility_message(&summary);
        messages.push(summary);

        UtilityResult::success_with(messages)
    }
}

// ─────────────────────── BPXBATCH / USS Batch ───────────────────────

/// BPXBATCH — USS (Unix System Services) command execution in batch.
///
/// Reads USS commands from STDPARM DD (or STDIN as fallback),
/// writes output to STDOUT DD and errors to STDERR DD.
pub struct Bpxbatch;

impl UtilityProgram for Bpxbatch {
    fn name(&self) -> &str {
        "BPXBATCH"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        // Read from STDPARM (primary) or STDIN (fallback)
        let commands = context
            .get_dd("STDPARM")
            .and_then(|dd| dd.inline_data.clone())
            .or_else(|| {
                context
                    .get_dd("STDIN")
                    .and_then(|dd| dd.inline_data.clone())
            })
            .unwrap_or_default();

        if commands.is_empty() {
            let msg = UtilityMessage::error(
                &format_message_id("BPX", 56, MessageSeverity::Error),
                "BPXBATCH — STDPARM/STDIN IS EMPTY",
            );
            context.write_utility_message(&msg);
            return UtilityResult::new(12, vec![msg]);
        }

        let mut messages = Vec::new();

        let hdr = UtilityMessage::info(
            &format_message_id("BPX", 1, MessageSeverity::Info),
            "BPXBATCH USS BATCH PROCESSOR INITIALIZED",
        );
        context.write_utility_message(&hdr);
        messages.push(hdr);

        let mut cmd_count = 0u32;

        for line in &commands {
            let trimmed = line.trim();
            if trimmed.is_empty() || trimmed.starts_with('#') {
                continue;
            }

            cmd_count += 1;

            // Parse the SH or PGM keyword prefix
            let (mode, cmd) = if let Some(rest) = trimmed.strip_prefix("SH ").or_else(|| trimmed.strip_prefix("sh ")) {
                ("SH", rest.trim())
            } else if let Some(rest) = trimmed.strip_prefix("PGM ").or_else(|| trimmed.strip_prefix("pgm ")) {
                ("PGM", rest.trim())
            } else {
                ("SH", trimmed) // default to shell mode
            };

            let msg = UtilityMessage::info(
                &format_message_id("BPX", 2, MessageSeverity::Info),
                &format!("EXECUTING {mode}: {cmd}"),
            );
            context.write_utility_message(&msg);
            messages.push(msg);

            // Write output to STDOUT DD
            if let Some(dd) = context.get_dd_mut("STDOUT") {
                dd.output.push(format!("USS {mode}: {cmd}"));
                dd.output.push("  RC=0".to_string());
            }
        }

        let summary = UtilityMessage::info(
            &format_message_id("BPX", 50, MessageSeverity::Info),
            &format!("{cmd_count} USS COMMAND(S) PROCESSED"),
        );
        context.write_utility_message(&summary);
        messages.push(summary);

        UtilityResult::success_with(messages)
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{DdAllocation, UtilityContext};

    // ─── UTIL-109.1: IKJEFT01 — TSO Command Execution ───

    #[test]
    fn test_ikjeft01_basic() {
        let mut ctx = UtilityContext::new("STEP01", "IKJEFT01");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("SYSTSPRT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSTSIN",
            vec![
                " ALLOCATE DA('MY.DATA') FI(INFILE) SHR".to_string(),
                " LISTDS 'MY.DATA' STATUS".to_string(),
            ],
        ));

        let result = Ikjeft01.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages.iter().any(|m| m.text.contains("STANDARD MODE")));
        assert!(result.messages.iter().any(|m| m.text.contains("2 TSO COMMAND(S)")));
    }

    #[test]
    fn test_ikjeft01_empty_systsin() {
        let mut ctx = UtilityContext::new("STEP01", "IKJEFT01");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline("SYSTSIN", vec![]));

        let result = Ikjeft01.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    #[test]
    fn test_ikjeft01_end_command() {
        let mut ctx = UtilityContext::new("STEP01", "IKJEFT01");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("SYSTSPRT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSTSIN",
            vec![
                " LISTDS 'MY.DATA'".to_string(),
                " END".to_string(),
                " THIS SHOULD NOT EXECUTE".to_string(),
            ],
        ));

        let result = Ikjeft01.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages.iter().any(|m| m.text.contains("END COMMAND")));
        // Only 1 command should have been counted before END
        assert!(result.messages.iter().any(|m| m.text.contains("1 TSO COMMAND(S)")));
    }

    #[test]
    fn test_ikjeft01_systsprt_output() {
        let mut ctx = UtilityContext::new("STEP01", "IKJEFT01");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("SYSTSPRT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSTSIN",
            vec![" LISTDS 'MY.DATA'".to_string()],
        ));

        Ikjeft01.execute(&mut ctx);

        let systsprt = ctx.get_dd("SYSTSPRT").unwrap();
        assert!(!systsprt.output.is_empty());
        assert!(systsprt.output[0].contains("LISTDS"));
    }

    // ─── UTIL-109.2: IKJEFT1A/IKJEFT1B Variants ───

    #[test]
    fn test_ikjeft1a_authorized() {
        let mut ctx = UtilityContext::new("STEP01", "IKJEFT1A");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("SYSTSPRT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSTSIN",
            vec![" AUTHCMD".to_string()],
        ));

        let result = Ikjeft1a.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages.iter().any(|m| m.text.contains("AUTHORIZED MODE")));
    }

    #[test]
    fn test_ikjeft1b_non_authorized() {
        let mut ctx = UtilityContext::new("STEP01", "IKJEFT1B");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("SYSTSPRT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSTSIN",
            vec![" LISTDS 'MY.DATA'".to_string()],
        ));

        let result = Ikjeft1b.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result
            .messages
            .iter()
            .any(|m| m.text.contains("NON-AUTHORIZED MODE")));
    }

    // ─── UTIL-109.3: IRXJCL — REXX Batch ───

    #[test]
    fn test_irxjcl_basic() {
        let mut ctx = UtilityContext::new("STEP01", "IRXJCL");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("SYSTSPRT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSTSIN",
            vec!["%MYREXX".to_string()],
        ));

        let result = Irxjcl.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages.iter().any(|m| m.text.contains("MYREXX")));
        assert!(result.messages.iter().any(|m| m.text.contains("1 REXX EXEC(S)")));
    }

    #[test]
    fn test_irxjcl_empty_systsin() {
        let mut ctx = UtilityContext::new("STEP01", "IRXJCL");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline("SYSTSIN", vec![]));

        let result = Irxjcl.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    #[test]
    fn test_irxjcl_multiple_execs() {
        let mut ctx = UtilityContext::new("STEP01", "IRXJCL");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("SYSTSPRT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSTSIN",
            vec![
                "%EXEC1".to_string(),
                "EXEC2 ARG1 ARG2".to_string(),
            ],
        ));

        let result = Irxjcl.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages.iter().any(|m| m.text.contains("EXEC1")));
        assert!(result.messages.iter().any(|m| m.text.contains("EXEC2")));
        assert!(result.messages.iter().any(|m| m.text.contains("2 REXX EXEC(S)")));
    }

    #[test]
    fn test_irxjcl_systsprt_output() {
        let mut ctx = UtilityContext::new("STEP01", "IRXJCL");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("SYSTSPRT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSTSIN",
            vec!["%MYREXX".to_string()],
        ));

        Irxjcl.execute(&mut ctx);

        let systsprt = ctx.get_dd("SYSTSPRT").unwrap();
        assert!(!systsprt.output.is_empty());
        assert!(systsprt.output[0].contains("MYREXX"));
    }

    // ─── UTIL-109.4: BPXBATCH — USS Batch ───

    #[test]
    fn test_bpxbatch_sh_command() {
        let mut ctx = UtilityContext::new("STEP01", "BPXBATCH");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("STDOUT"));
        ctx.add_dd(DdAllocation::inline(
            "STDPARM",
            vec!["SH /bin/ls /home/user".to_string()],
        ));

        let result = Bpxbatch.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages.iter().any(|m| m.text.contains("SH:")));
        assert!(result.messages.iter().any(|m| m.text.contains("/bin/ls")));
    }

    #[test]
    fn test_bpxbatch_pgm_command() {
        let mut ctx = UtilityContext::new("STEP01", "BPXBATCH");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("STDOUT"));
        ctx.add_dd(DdAllocation::inline(
            "STDPARM",
            vec!["PGM /usr/bin/my_program".to_string()],
        ));

        let result = Bpxbatch.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages.iter().any(|m| m.text.contains("PGM:")));
    }

    #[test]
    fn test_bpxbatch_empty_stdparm() {
        let mut ctx = UtilityContext::new("STEP01", "BPXBATCH");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline("STDPARM", vec![]));

        let result = Bpxbatch.execute(&mut ctx);
        assert_eq!(result.condition_code, 12);
    }

    #[test]
    fn test_bpxbatch_stdin_fallback() {
        let mut ctx = UtilityContext::new("STEP01", "BPXBATCH");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("STDOUT"));
        // No STDPARM, but STDIN is available
        ctx.add_dd(DdAllocation::inline(
            "STDIN",
            vec!["SH echo hello".to_string()],
        ));

        let result = Bpxbatch.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages.iter().any(|m| m.text.contains("1 USS COMMAND(S)")));
    }

    #[test]
    fn test_bpxbatch_stdout_output() {
        let mut ctx = UtilityContext::new("STEP01", "BPXBATCH");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("STDOUT"));
        ctx.add_dd(DdAllocation::inline(
            "STDPARM",
            vec!["SH /bin/ls".to_string()],
        ));

        Bpxbatch.execute(&mut ctx);

        let stdout = ctx.get_dd("STDOUT").unwrap();
        assert!(!stdout.output.is_empty());
        assert!(stdout.output[0].contains("/bin/ls"));
    }

    #[test]
    fn test_bpxbatch_default_sh_mode() {
        let mut ctx = UtilityContext::new("STEP01", "BPXBATCH");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("STDOUT"));
        ctx.add_dd(DdAllocation::inline(
            "STDPARM",
            vec!["/bin/ls /tmp".to_string()], // No SH/PGM prefix
        ));

        let result = Bpxbatch.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        // Should default to SH mode
        assert!(result.messages.iter().any(|m| m.text.contains("SH:")));
    }

    // ─── UTIL-109.5: DD Conventions ───

    #[test]
    fn test_ikjeft01_comments_ignored() {
        let mut ctx = UtilityContext::new("STEP01", "IKJEFT01");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("SYSTSPRT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSTSIN",
            vec![
                "* THIS IS A COMMENT".to_string(),
                "".to_string(),
                " LISTDS 'MY.DATA'".to_string(),
            ],
        ));

        let result = Ikjeft01.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages.iter().any(|m| m.text.contains("1 TSO COMMAND(S)")));
    }

    #[test]
    fn test_bpxbatch_hash_comments_ignored() {
        let mut ctx = UtilityContext::new("STEP01", "BPXBATCH");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("STDOUT"));
        ctx.add_dd(DdAllocation::inline(
            "STDPARM",
            vec![
                "# This is a comment".to_string(),
                "SH /bin/echo hello".to_string(),
            ],
        ));

        let result = Bpxbatch.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages.iter().any(|m| m.text.contains("1 USS COMMAND(S)")));
    }

    // ─── UTIL-109.6: Registry Integration ───

    #[test]
    fn test_all_batch_programs_registered() {
        let reg = crate::UtilityRegistry::with_builtins();
        assert!(reg.is_registered("IKJEFT01"));
        assert!(reg.is_registered("IKJEFT1A"));
        assert!(reg.is_registered("IKJEFT1B"));
        assert!(reg.is_registered("IRXJCL"));
        assert!(reg.is_registered("BPXBATCH"));
    }

    #[test]
    fn test_dispatch_ikjeft01() {
        let reg = crate::UtilityRegistry::with_builtins();
        let mut ctx = UtilityContext::new("STEP01", "IKJEFT01");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("SYSTSPRT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSTSIN",
            vec![" LISTDS 'SYS1.MACLIB'".to_string()],
        ));

        let result = reg.dispatch("IKJEFT01", &mut ctx).unwrap();
        assert_eq!(result.condition_code, 0);
    }

    #[test]
    fn test_dispatch_bpxbatch() {
        let reg = crate::UtilityRegistry::with_builtins();
        let mut ctx = UtilityContext::new("STEP01", "BPXBATCH");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("STDOUT"));
        ctx.add_dd(DdAllocation::inline(
            "STDPARM",
            vec!["SH /bin/echo test".to_string()],
        ));

        let result = reg.dispatch("BPXBATCH", &mut ctx).unwrap();
        assert_eq!(result.condition_code, 0);
    }
}
