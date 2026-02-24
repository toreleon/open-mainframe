//! # z/OS Subsystem Utility Programs
//!
//! Simulated utility programs for IMS, CICS, DB2, and other z/OS subsystems.
//! These programs are registered in the `UtilityRegistry` and dispatched by the
//! JCL executor when encountered in `EXEC PGM=` statements.
//!
//! ## Programs
//!
//! - **DFSRRC00** — IMS Region Controller (BMP/DLI batch)
//! - **DFHCSDUP** — CICS CSD (System Definition) Utility
//! - **SDSF**     — System Display and Search Facility (operator commands)
//! - **FTP**      — Batch FTP file transfer
//! - **IGYCRCTL** — Enterprise COBOL Compiler
//! - **IEWL**     — Linkage Editor (Binder)
//! - **DFHECP1$** — CICS Command-Level Precompiler
//! - **DFHMAPS**  — CICS BMS Map Assembler
//! - **DSNHPC**   — DB2 Precompiler
//! - **DSNTIAD**  — DB2 DDL/DML Execution (via SPUFI-like batch)
//! - **DSNTIAUL** — DB2 Data Unload Utility
//! - **DSNTEP4**  — DB2 Batch SQL Processor

use crate::{
    format_message_id, MessageSeverity, UtilityContext, UtilityMessage, UtilityProgram,
    UtilityResult,
};

// ─────────────────────── DFSRRC00 / IMS Region Controller ───────────────────

/// DFSRRC00 — IMS Region Controller.
///
/// Dispatches IMS BMP (Batch Message Processing) and DLI (Data Language/I)
/// batch operations. Reads PARM to determine region type and application.
pub struct Dfsrrc00;

impl UtilityProgram for Dfsrrc00 {
    fn name(&self) -> &str {
        "DFSRRC00"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let mut messages = Vec::new();

        // Parse PARM from step - the PARM is passed via context program_name metadata
        // In IMS: PARM='BMP,pgmname,psbname' or PARM='DLI,pgmname,psbname,...'
        // The PARM is available from the step's PARM field, which we receive as inline data
        // on a synthetic DD or from the context's program_name.

        let hdr = UtilityMessage::info(
            &format_message_id("DFS", 1, MessageSeverity::Info),
            "DFSRRC00 IMS REGION CONTROLLER INITIALIZED",
        );
        context.write_utility_message(&hdr);
        messages.push(hdr);

        // Check for IMS control DDs
        let has_steplib = context.has_dd("STEPLIB");
        let has_dfsreslb = context.has_dd("DFSRESLB");

        if has_steplib || has_dfsreslb {
            let msg = UtilityMessage::info(
                &format_message_id("DFS", 10, MessageSeverity::Info),
                "IMS LIBRARIES ALLOCATED",
            );
            context.write_utility_message(&msg);
            messages.push(msg);
        }

        // Log allocated DDs for diagnostic purposes
        let dd_names = context.dd_names();
        let dd_count = dd_names.len();

        let msg = UtilityMessage::info(
            &format_message_id("DFS", 20, MessageSeverity::Info),
            &format!("{} DD(S) ALLOCATED FOR IMS REGION", dd_count),
        );
        context.write_utility_message(&msg);
        messages.push(msg);

        // Process input DDs if present (INFILE1, INFILE2, etc.)
        for dd_name in &dd_names {
            let upper = dd_name.to_uppercase();
            if upper.starts_with("INFILE") || upper == "IEFRDER" || upper == "SYSUDUMP" {
                if let Some(dd) = context.get_dd(dd_name) {
                    let line_count = dd.inline_data.as_ref().map_or(0, |d| d.len());
                    if line_count > 0 {
                        let msg = UtilityMessage::info(
                            &format_message_id("DFS", 30, MessageSeverity::Info),
                            &format!("{}: {} RECORDS READ", upper, line_count),
                        );
                        context.write_utility_message(&msg);
                        messages.push(msg);
                    }
                }
            }
        }

        // Write output marker to SYSPRINT if allocated
        if let Some(dd) = context.get_dd_mut("SYSPRINT") {
            dd.output.push("DFSRRC00 COMPLETED SUCCESSFULLY".to_string());
            dd.output.push("IMS REGION TERMINATED NORMALLY".to_string());
        }

        let summary = UtilityMessage::info(
            &format_message_id("DFS", 99, MessageSeverity::Info),
            "DFSRRC00 IMS REGION TERMINATED NORMALLY — RC=0",
        );
        context.write_utility_message(&summary);
        messages.push(summary);

        UtilityResult::success_with(messages)
    }
}

// ─────────────────────── DFHCSDUP / CICS CSD Utility ────────────────────────

/// DFHCSDUP — CICS System Definition (CSD) Utility.
///
/// Processes CICS resource definitions from SYSIN DD. Supports DEFINE, LIST,
/// DELETE, and ALTER commands for programs, mapsets, transactions, and files.
pub struct Dfhcsdup;

impl UtilityProgram for Dfhcsdup {
    fn name(&self) -> &str {
        "DFHCSDUP"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let mut messages = Vec::new();

        let hdr = UtilityMessage::info(
            &format_message_id("DFH", 1, MessageSeverity::Info),
            "DFHCSDUP CICS CSD UTILITY INITIALIZED",
        );
        context.write_utility_message(&hdr);
        messages.push(hdr);

        // Read SYSIN for CSD commands
        let commands = context
            .get_dd("SYSIN")
            .and_then(|dd| dd.inline_data.clone())
            .unwrap_or_default();

        let mut cmd_count = 0u32;
        let mut define_count = 0u32;
        let mut list_count = 0u32;

        for line in &commands {
            let trimmed = line.trim();
            if trimmed.is_empty() || trimmed.starts_with('*') {
                continue;
            }

            cmd_count += 1;
            let upper = trimmed.to_uppercase();

            if upper.starts_with("DEFINE") {
                define_count += 1;
                let msg = UtilityMessage::info(
                    &format_message_id("DFH", 10, MessageSeverity::Info),
                    &format!("DEFINE: {}", trimmed),
                );
                context.write_utility_message(&msg);
                messages.push(msg);
            } else if upper.starts_with("LIST") {
                list_count += 1;
                let msg = UtilityMessage::info(
                    &format_message_id("DFH", 11, MessageSeverity::Info),
                    &format!("LIST: {}", trimmed),
                );
                context.write_utility_message(&msg);
                messages.push(msg);
            } else {
                let msg = UtilityMessage::info(
                    &format_message_id("DFH", 12, MessageSeverity::Info),
                    &format!("CSD: {}", trimmed),
                );
                context.write_utility_message(&msg);
                messages.push(msg);
            }
        }

        // Write output to SYSPRINT/DFHCSD
        if let Some(dd) = context.get_dd_mut("SYSPRINT") {
            dd.output.push(format!("DFHCSDUP COMPLETED — {} COMMANDS PROCESSED", cmd_count));
            dd.output.push(format!("  DEFINES: {}", define_count));
            dd.output.push(format!("  LISTS:   {}", list_count));
        }

        let summary = UtilityMessage::info(
            &format_message_id("DFH", 99, MessageSeverity::Info),
            &format!("{} CSD COMMAND(S) PROCESSED — HIGHEST CC=0", cmd_count),
        );
        context.write_utility_message(&summary);
        messages.push(summary);

        UtilityResult::success_with(messages)
    }
}

// ─────────────────────── SDSF / Operator Commands ───────────────────────────

/// SDSF — System Display and Search Facility.
///
/// Processes operator commands from SYSIN DD. In CICS context, used to issue
/// CEMT commands for file management (OPEN/CLOSE/ENABLE/DISABLE).
pub struct Sdsf;

impl UtilityProgram for Sdsf {
    fn name(&self) -> &str {
        "SDSF"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let mut messages = Vec::new();

        let hdr = UtilityMessage::info(
            &format_message_id("ISF", 1, MessageSeverity::Info),
            "SDSF SYSTEM DISPLAY AND SEARCH FACILITY",
        );
        context.write_utility_message(&hdr);
        messages.push(hdr);

        // Read commands from SYSIN or ISFIN
        let commands = context
            .get_dd("SYSIN")
            .and_then(|dd| dd.inline_data.clone())
            .or_else(|| {
                context
                    .get_dd("ISFIN")
                    .and_then(|dd| dd.inline_data.clone())
            })
            .unwrap_or_default();

        let mut cmd_count = 0u32;

        for line in &commands {
            let trimmed = line.trim();
            if trimmed.is_empty() || trimmed.starts_with('*') {
                continue;
            }

            cmd_count += 1;

            let msg = UtilityMessage::info(
                &format_message_id("ISF", 10, MessageSeverity::Info),
                &format!("CMD: {}", trimmed),
            );
            context.write_utility_message(&msg);
            messages.push(msg);

            // Write command output
            if let Some(dd) = context.get_dd_mut("SYSPRINT") {
                dd.output.push(format!("SDSF CMD: {}", trimmed));
                dd.output.push("  RESPONSE: NORMAL".to_string());
            }
        }

        let summary = UtilityMessage::info(
            &format_message_id("ISF", 99, MessageSeverity::Info),
            &format!("{} SDSF COMMAND(S) PROCESSED — RC=0", cmd_count),
        );
        context.write_utility_message(&summary);
        messages.push(summary);

        UtilityResult::success_with(messages)
    }
}

// ─────────────────────── FTP / Batch File Transfer ──────────────────────────

/// FTP — Batch File Transfer Protocol.
///
/// Processes FTP commands from SYSIN/SYSUT1/INPUT DD. Commands include
/// PUT, GET, CD, LCD, BIN, ASCII, QUIT etc.
pub struct Ftp;

impl UtilityProgram for Ftp {
    fn name(&self) -> &str {
        "FTP"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let mut messages = Vec::new();

        let hdr = UtilityMessage::info(
            &format_message_id("FTP", 1, MessageSeverity::Info),
            "FTP BATCH FILE TRANSFER INITIALIZED",
        );
        context.write_utility_message(&hdr);
        messages.push(hdr);

        // Read FTP commands from SYSIN, SYSUT1, or INPUT
        let commands = context
            .get_dd("SYSIN")
            .and_then(|dd| dd.inline_data.clone())
            .or_else(|| {
                context
                    .get_dd("SYSUT1")
                    .and_then(|dd| dd.inline_data.clone())
            })
            .or_else(|| {
                context
                    .get_dd("INPUT")
                    .and_then(|dd| dd.inline_data.clone())
            })
            .unwrap_or_default();

        let mut cmd_count = 0u32;

        for line in &commands {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }

            cmd_count += 1;

            let msg = UtilityMessage::info(
                &format_message_id("FTP", 10, MessageSeverity::Info),
                &format!("FTP> {}", trimmed),
            );
            context.write_utility_message(&msg);
            messages.push(msg);

            // Write response to SYSPRINT
            if let Some(dd) = context.get_dd_mut("SYSPRINT") {
                dd.output.push(format!(">>> {}", trimmed));
                dd.output.push("200 Command successful".to_string());
            }
        }

        // Write summary to OUTPUT/SYSPRINT
        if let Some(dd) = context.get_dd_mut("SYSPRINT") {
            dd.output.push(format!("226 FTP TRANSFER COMPLETE — {} COMMANDS", cmd_count));
        }

        let summary = UtilityMessage::info(
            &format_message_id("FTP", 99, MessageSeverity::Info),
            &format!("{} FTP COMMAND(S) PROCESSED — RC=0", cmd_count),
        );
        context.write_utility_message(&summary);
        messages.push(summary);

        UtilityResult::success_with(messages)
    }
}

// ─────────────────────── IGYCRCTL / COBOL Compiler ──────────────────────────

/// IGYCRCTL — Enterprise COBOL Compiler.
///
/// Reads COBOL source from SYSIN DD, produces object code to SYSLIN DD,
/// and writes listings to SYSPRINT DD.
pub struct Igycrctl;

impl UtilityProgram for Igycrctl {
    fn name(&self) -> &str {
        "IGYCRCTL"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let mut messages = Vec::new();

        let hdr = UtilityMessage::info(
            &format_message_id("IGY", 1, MessageSeverity::Info),
            "IGYCRCTL ENTERPRISE COBOL COMPILER V6.4",
        );
        context.write_utility_message(&hdr);
        messages.push(hdr);

        // Read source from SYSIN
        let source = context
            .get_dd("SYSIN")
            .and_then(|dd| dd.inline_data.clone())
            .unwrap_or_default();

        let line_count = source.len();

        let msg = UtilityMessage::info(
            &format_message_id("IGY", 10, MessageSeverity::Info),
            &format!("{} SOURCE LINE(S) READ", line_count),
        );
        context.write_utility_message(&msg);
        messages.push(msg);

        // Write compiler listing to SYSPRINT
        if let Some(dd) = context.get_dd_mut("SYSPRINT") {
            dd.output.push("PP 5655-EC6 IBM Enterprise COBOL for z/OS 6.4.0".to_string());
            dd.output.push(format!("  {} SOURCE LINES READ", line_count));
            dd.output.push("  0 ERRORS FOUND".to_string());
            dd.output.push("  0 WARNINGS FOUND".to_string());
            dd.output.push("  COMPILATION SUCCESSFUL".to_string());
        }

        // Write object code to SYSLIN
        if let Some(dd) = context.get_dd_mut("SYSLIN") {
            dd.output.push(" ENTRY MAINPGM".to_string());
            dd.output.push(" OBJECT MODULE GENERATED".to_string());
        }

        let summary = UtilityMessage::info(
            &format_message_id("IGY", 99, MessageSeverity::Info),
            &format!("COMPILATION COMPLETE — {} LINES, 0 ERRORS — RC=0", line_count),
        );
        context.write_utility_message(&summary);
        messages.push(summary);

        UtilityResult::success_with(messages)
    }
}

// ─────────────────────── IEWL / Linkage Editor ──────────────────────────────

/// IEWL — Linkage Editor (Binder).
///
/// Reads object modules from SYSLIN DD, resolves external references using
/// SYSLIB DD, and produces a load module to SYSLMOD DD.
pub struct Iewl;

impl UtilityProgram for Iewl {
    fn name(&self) -> &str {
        "IEWL"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let mut messages = Vec::new();

        let hdr = UtilityMessage::info(
            &format_message_id("IEW", 1, MessageSeverity::Info),
            "IEWL LINKAGE EDITOR (BINDER) V1R14",
        );
        context.write_utility_message(&hdr);
        messages.push(hdr);

        // Read object modules from SYSLIN
        let object = context
            .get_dd("SYSLIN")
            .and_then(|dd| dd.inline_data.clone())
            .unwrap_or_default();

        let msg = UtilityMessage::info(
            &format_message_id("IEW", 10, MessageSeverity::Info),
            &format!("{} OBJECT RECORD(S) READ", object.len()),
        );
        context.write_utility_message(&msg);
        messages.push(msg);

        // Write load module to SYSLMOD
        if let Some(dd) = context.get_dd_mut("SYSLMOD") {
            dd.output.push("LOAD MODULE CREATED".to_string());
            dd.output.push("  ENTRY POINT: MAINPGM".to_string());
            dd.output.push("  AUTHORIZATION CODE: 0".to_string());
        }

        // Write listing to SYSPRINT
        if let Some(dd) = context.get_dd_mut("SYSPRINT") {
            dd.output.push("IEW2008I PROCESSING COMPLETED".to_string());
            dd.output.push("IEW2010I MODULE SAVED".to_string());
        }

        let summary = UtilityMessage::info(
            &format_message_id("IEW", 99, MessageSeverity::Info),
            "LINK-EDIT COMPLETE — MODULE SAVED — RC=0",
        );
        context.write_utility_message(&summary);
        messages.push(summary);

        UtilityResult::success_with(messages)
    }
}

// ─────────────────────── DFHECP1$ / CICS Precompiler ────────────────────────

/// DFHECP1$ — CICS Command-Level Precompiler.
///
/// Translates EXEC CICS commands in COBOL source to CICS API calls.
/// Reads from SYSIN, writes translated source to SYSPRINT/SYSLIN.
pub struct Dfhecp1;

impl UtilityProgram for Dfhecp1 {
    fn name(&self) -> &str {
        "DFHECP1$"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let mut messages = Vec::new();

        let hdr = UtilityMessage::info(
            &format_message_id("DFH", 1, MessageSeverity::Info),
            "DFHECP1$ CICS COMMAND-LEVEL PRECOMPILER",
        );
        context.write_utility_message(&hdr);
        messages.push(hdr);

        // Read source from SYSIN
        let source = context
            .get_dd("SYSIN")
            .and_then(|dd| dd.inline_data.clone())
            .unwrap_or_default();

        let line_count = source.len();

        let msg = UtilityMessage::info(
            &format_message_id("DFH", 10, MessageSeverity::Info),
            &format!("{} SOURCE LINE(S) READ", line_count),
        );
        context.write_utility_message(&msg);
        messages.push(msg);

        // Count EXEC CICS statements
        let exec_cics_count = source
            .iter()
            .filter(|l| l.to_uppercase().contains("EXEC CICS"))
            .count();

        let msg = UtilityMessage::info(
            &format_message_id("DFH", 20, MessageSeverity::Info),
            &format!("{} EXEC CICS STATEMENT(S) TRANSLATED", exec_cics_count),
        );
        context.write_utility_message(&msg);
        messages.push(msg);

        // Write translated source to SYSPRINT
        if let Some(dd) = context.get_dd_mut("SYSPRINT") {
            dd.output.push("DFHECP1$ TRANSLATION COMPLETE".to_string());
            dd.output.push(format!("  {} EXEC CICS COMMANDS TRANSLATED", exec_cics_count));
        }

        // Write translated source to SYSLIN (for next compiler step)
        if let Some(dd) = context.get_dd_mut("SYSLIN") {
            for line in &source {
                dd.output.push(line.clone());
            }
        }

        let summary = UtilityMessage::info(
            &format_message_id("DFH", 99, MessageSeverity::Info),
            &format!("PRECOMPILE COMPLETE — {} CICS COMMANDS — RC=0", exec_cics_count),
        );
        context.write_utility_message(&summary);
        messages.push(summary);

        UtilityResult::success_with(messages)
    }
}

// ─────────────────────── DFHMAPS / BMS Map Assembler ────────────────────────

/// DFHMAPS — CICS BMS Map Assembler.
///
/// Processes BMS macros (DFHMSD/DFHMDI/DFHMDF) to generate physical and
/// symbolic map sets. Reads from SYSIN, writes maps to SYSPUNCH/SYSLIN.
pub struct Dfhmaps;

impl UtilityProgram for Dfhmaps {
    fn name(&self) -> &str {
        "DFHMAPS"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let mut messages = Vec::new();

        let hdr = UtilityMessage::info(
            &format_message_id("DFH", 1, MessageSeverity::Info),
            "DFHMAPS CICS BMS MAP ASSEMBLER",
        );
        context.write_utility_message(&hdr);
        messages.push(hdr);

        // Read BMS source from SYSIN
        let source = context
            .get_dd("SYSIN")
            .and_then(|dd| dd.inline_data.clone())
            .unwrap_or_default();

        let line_count = source.len();

        // Count BMS macros
        let macro_count = source
            .iter()
            .filter(|l| {
                let upper = l.to_uppercase();
                upper.contains("DFHMSD") || upper.contains("DFHMDI") || upper.contains("DFHMDF")
            })
            .count();

        let msg = UtilityMessage::info(
            &format_message_id("DFH", 10, MessageSeverity::Info),
            &format!("{} BMS MACRO(S) IN {} LINES", macro_count, line_count),
        );
        context.write_utility_message(&msg);
        messages.push(msg);

        // Write physical map to SYSPUNCH/SYSLIN
        if let Some(dd) = context.get_dd_mut("SYSPUNCH") {
            dd.output.push("BMS PHYSICAL MAP GENERATED".to_string());
        }
        if let Some(dd) = context.get_dd_mut("SYSLIN") {
            dd.output.push(" ENTRY MAPSET".to_string());
            dd.output.push(" PHYSICAL MAP OBJECT".to_string());
        }

        // Write listing to SYSPRINT
        if let Some(dd) = context.get_dd_mut("SYSPRINT") {
            dd.output.push("DFHMAPS ASSEMBLY COMPLETE".to_string());
            dd.output.push(format!("  {} BMS MACROS PROCESSED", macro_count));
        }

        let summary = UtilityMessage::info(
            &format_message_id("DFH", 99, MessageSeverity::Info),
            &format!("BMS ASSEMBLY COMPLETE — {} MACROS — RC=0", macro_count),
        );
        context.write_utility_message(&summary);
        messages.push(summary);

        UtilityResult::success_with(messages)
    }
}

// ─────────────────────── DSNHPC / DB2 Precompiler ───────────────────────────

/// DSNHPC — DB2 SQL Precompiler.
///
/// Scans COBOL source for embedded SQL statements (EXEC SQL), replaces them
/// with DB2 API calls, and generates a DBRM (Database Request Module).
pub struct Dsnhpc;

impl UtilityProgram for Dsnhpc {
    fn name(&self) -> &str {
        "DSNHPC"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let mut messages = Vec::new();

        let hdr = UtilityMessage::info(
            &format_message_id("DSN", 1, MessageSeverity::Info),
            "DSNHPC DB2 SQL PRECOMPILER V12",
        );
        context.write_utility_message(&hdr);
        messages.push(hdr);

        // Read source from SYSIN
        let source = context
            .get_dd("SYSIN")
            .and_then(|dd| dd.inline_data.clone())
            .unwrap_or_default();

        let line_count = source.len();

        // Count EXEC SQL statements
        let sql_count = source
            .iter()
            .filter(|l| l.to_uppercase().contains("EXEC SQL"))
            .count();

        let msg = UtilityMessage::info(
            &format_message_id("DSN", 10, MessageSeverity::Info),
            &format!("{} SQL STATEMENT(S) IN {} LINES", sql_count, line_count),
        );
        context.write_utility_message(&msg);
        messages.push(msg);

        // Write translated source to SYSLIN
        if let Some(dd) = context.get_dd_mut("SYSLIN") {
            for line in &source {
                dd.output.push(line.clone());
            }
        }

        // Write DBRM to DBRMLIB
        if let Some(dd) = context.get_dd_mut("DBRMLIB") {
            dd.output.push("DBRM GENERATED".to_string());
            dd.output.push(format!("  {} SQL STATEMENTS", sql_count));
        }

        // Write listing to SYSPRINT
        if let Some(dd) = context.get_dd_mut("SYSPRINT") {
            dd.output.push("DSNHPC PRECOMPILE COMPLETE".to_string());
            dd.output.push(format!("  {} SQL STATEMENTS PROCESSED", sql_count));
        }

        let summary = UtilityMessage::info(
            &format_message_id("DSN", 99, MessageSeverity::Info),
            &format!("PRECOMPILE COMPLETE — {} SQL STMTS — RC=0", sql_count),
        );
        context.write_utility_message(&summary);
        messages.push(summary);

        UtilityResult::success_with(messages)
    }
}

// ─────────────────────── DSNTIAD / DB2 DDL Execution ────────────────────────

/// DSNTIAD — DB2 DDL/DML Batch Execution.
///
/// Reads SQL statements from SYSIN DD and executes them against DB2.
/// Used for DDL (CREATE/ALTER/DROP) and DML operations in batch.
pub struct Dsntiad;

impl UtilityProgram for Dsntiad {
    fn name(&self) -> &str {
        "DSNTIAD"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let mut messages = Vec::new();

        let hdr = UtilityMessage::info(
            &format_message_id("DSN", 1, MessageSeverity::Info),
            "DSNTIAD DB2 BATCH DDL/DML PROCESSOR",
        );
        context.write_utility_message(&hdr);
        messages.push(hdr);

        // Read SQL from SYSIN
        let sql_lines = context
            .get_dd("SYSIN")
            .and_then(|dd| dd.inline_data.clone())
            .unwrap_or_default();

        let mut stmt_count = 0u32;

        // Count SQL statements (delimited by ;)
        let full_sql = sql_lines.join("\n");
        for stmt in full_sql.split(';') {
            let trimmed = stmt.trim();
            if !trimmed.is_empty() {
                stmt_count += 1;
                let first_line = trimmed.lines().next().unwrap_or("").trim();
                let msg = UtilityMessage::info(
                    &format_message_id("DSN", 10, MessageSeverity::Info),
                    &format!("SQL: {}", first_line),
                );
                context.write_utility_message(&msg);
                messages.push(msg);
            }
        }

        // Write output to SYSPRINT
        if let Some(dd) = context.get_dd_mut("SYSPRINT") {
            dd.output.push(format!("DSNTIAD — {} SQL STATEMENT(S) EXECUTED", stmt_count));
            dd.output.push("  SQLCODE=0 FOR ALL STATEMENTS".to_string());
        }

        let summary = UtilityMessage::info(
            &format_message_id("DSN", 99, MessageSeverity::Info),
            &format!("{} SQL STATEMENT(S) EXECUTED — RC=0", stmt_count),
        );
        context.write_utility_message(&summary);
        messages.push(summary);

        UtilityResult::success_with(messages)
    }
}

// ─────────────────────── DSNTIAUL / DB2 Unload Utility ──────────────────────

/// DSNTIAUL — DB2 Data Unload Utility.
///
/// Extracts data from DB2 tables using SQL SELECT statements, writing
/// results to SYSREC00 or other output DDs in sequential format.
pub struct Dsntiaul;

impl UtilityProgram for Dsntiaul {
    fn name(&self) -> &str {
        "DSNTIAUL"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let mut messages = Vec::new();

        let hdr = UtilityMessage::info(
            &format_message_id("DSN", 1, MessageSeverity::Info),
            "DSNTIAUL DB2 DATA UNLOAD UTILITY",
        );
        context.write_utility_message(&hdr);
        messages.push(hdr);

        // Read SQL from SYSIN
        let sql_lines = context
            .get_dd("SYSIN")
            .and_then(|dd| dd.inline_data.clone())
            .unwrap_or_default();

        let full_sql = sql_lines.join("\n");
        let mut select_count = 0u32;
        let mut record_count = 0u32;

        for stmt in full_sql.split(';') {
            let trimmed = stmt.trim();
            if trimmed.to_uppercase().starts_with("SELECT") {
                select_count += 1;
                // Simulate extracting 100 records per query
                record_count += 100;
            }
        }

        // Write dummy output data to SYSREC00
        if let Some(dd) = context.get_dd_mut("SYSREC00") {
            dd.output.push(format!("{} RECORDS UNLOADED", record_count));
        }

        // Write output to SYSPRINT
        if let Some(dd) = context.get_dd_mut("SYSPRINT") {
            dd.output.push(format!("DSNTIAUL — {} SELECT(S), {} RECORDS UNLOADED",
                select_count, record_count));
        }

        let summary = UtilityMessage::info(
            &format_message_id("DSN", 99, MessageSeverity::Info),
            &format!("{} SELECT(S) — {} RECORDS UNLOADED — RC=0", select_count, record_count),
        );
        context.write_utility_message(&summary);
        messages.push(summary);

        UtilityResult::success_with(messages)
    }
}

// ─────────────────────── DSNTEP4 / DB2 Batch SQL ────────────────────────────

/// DSNTEP4 — DB2 Batch SQL Processor.
///
/// Dynamic SQL execution utility that runs arbitrary SQL from SYSIN.
/// Similar to DSNTIAD but with enhanced error handling and output formatting.
pub struct Dsntep4;

impl UtilityProgram for Dsntep4 {
    fn name(&self) -> &str {
        "DSNTEP4"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let mut messages = Vec::new();

        let hdr = UtilityMessage::info(
            &format_message_id("DSN", 1, MessageSeverity::Info),
            "DSNTEP4 DB2 BATCH SQL PROCESSOR",
        );
        context.write_utility_message(&hdr);
        messages.push(hdr);

        // Read SQL from SYSIN
        let sql_lines = context
            .get_dd("SYSIN")
            .and_then(|dd| dd.inline_data.clone())
            .unwrap_or_default();

        let full_sql = sql_lines.join("\n");
        let mut stmt_count = 0u32;

        for stmt in full_sql.split(';') {
            let trimmed = stmt.trim();
            if !trimmed.is_empty() {
                stmt_count += 1;
                let first_line = trimmed.lines().next().unwrap_or("").trim();
                let msg = UtilityMessage::info(
                    &format_message_id("DSN", 10, MessageSeverity::Info),
                    &format!("DSNTEP4: {}", first_line),
                );
                context.write_utility_message(&msg);
                messages.push(msg);
            }
        }

        if let Some(dd) = context.get_dd_mut("SYSPRINT") {
            dd.output.push(format!("DSNTEP4 — {} SQL STATEMENT(S) EXECUTED", stmt_count));
            dd.output.push("  SQLCODE=0 FOR ALL STATEMENTS".to_string());
        }

        let summary = UtilityMessage::info(
            &format_message_id("DSN", 99, MessageSeverity::Info),
            &format!("{} SQL STATEMENT(S) EXECUTED — RC=0", stmt_count),
        );
        context.write_utility_message(&summary);
        messages.push(summary);

        UtilityResult::success_with(messages)
    }
}

// ─────────────────────── DFSURGU0 / IMS Unload Utility ──────────────────────

/// DFSURGU0 — IMS Database Unload/Reload Utility.
///
/// Unloads IMS database segments to a sequential dataset for reorganization
/// or backup. Used as a sub-program invoked by DFSRRC00.
pub struct Dfsurgu0;

impl UtilityProgram for Dfsurgu0 {
    fn name(&self) -> &str {
        "DFSURGU0"
    }

    fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
        let mut messages = Vec::new();

        let hdr = UtilityMessage::info(
            &format_message_id("DFS", 1, MessageSeverity::Info),
            "DFSURGU0 IMS DATABASE UNLOAD/RELOAD UTILITY",
        );
        context.write_utility_message(&hdr);
        messages.push(hdr);

        // Write output records
        if let Some(dd) = context.get_dd_mut("SYSPRINT") {
            dd.output.push("DFSURGU0 UNLOAD COMPLETE".to_string());
        }

        let summary = UtilityMessage::info(
            &format_message_id("DFS", 99, MessageSeverity::Info),
            "DFSURGU0 COMPLETED — RC=0",
        );
        context.write_utility_message(&summary);
        messages.push(summary);

        UtilityResult::success_with(messages)
    }
}

// ─────────────────────── Tests ──────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{DdAllocation, UtilityContext};

    #[test]
    fn test_dfsrrc00_basic() {
        let mut ctx = UtilityContext::new("STEP01", "DFSRRC00");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("STEPLIB"));

        let result = Dfsrrc00.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages.iter().any(|m| m.text.contains("IMS REGION CONTROLLER")));
    }

    #[test]
    fn test_dfhcsdup_basic() {
        let mut ctx = UtilityContext::new("STEP01", "DFHCSDUP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![
                "DEFINE PROGRAM(COSGN00C) GROUP(CARDDEMO) LANGUAGE(COBOL)".to_string(),
                "LIST GROUP(CARDDEMO)".to_string(),
            ],
        ));

        let result = Dfhcsdup.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages.iter().any(|m| m.text.contains("2 CSD COMMAND(S)")));
    }

    #[test]
    fn test_sdsf_basic() {
        let mut ctx = UtilityContext::new("STEP01", "SDSF");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec!["CEMT SET FILE(ACCTDAT) OPEN".to_string()],
        ));

        let result = Sdsf.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages.iter().any(|m| m.text.contains("1 SDSF COMMAND(S)")));
    }

    #[test]
    fn test_ftp_basic() {
        let mut ctx = UtilityContext::new("STEP01", "FTP");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![
                "cd /home/user".to_string(),
                "PUT MYDATA".to_string(),
                "QUIT".to_string(),
            ],
        ));

        let result = Ftp.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages.iter().any(|m| m.text.contains("3 FTP COMMAND(S)")));
    }

    #[test]
    fn test_igycrctl_basic() {
        let mut ctx = UtilityContext::new("STEP01", "IGYCRCTL");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("SYSLIN"));
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![
                "       IDENTIFICATION DIVISION.".to_string(),
                "       PROGRAM-ID. HELLO.".to_string(),
                "       PROCEDURE DIVISION.".to_string(),
                "           DISPLAY 'HELLO'.".to_string(),
                "           STOP RUN.".to_string(),
            ],
        ));

        let result = Igycrctl.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages.iter().any(|m| m.text.contains("5 SOURCE LINE(S)")));
    }

    #[test]
    fn test_iewl_basic() {
        let mut ctx = UtilityContext::new("LKED", "IEWL");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::output("SYSLMOD"));
        ctx.add_dd(DdAllocation::inline(
            "SYSLIN",
            vec![" ENTRY MAINPGM".to_string()],
        ));

        let result = Iewl.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.messages.iter().any(|m| m.text.contains("LINK-EDIT COMPLETE")));
    }

    #[test]
    fn test_all_subsystem_programs_registered() {
        let reg = crate::UtilityRegistry::with_builtins();
        assert!(reg.is_registered("DFSRRC00"));
        assert!(reg.is_registered("DFHCSDUP"));
        assert!(reg.is_registered("SDSF"));
        assert!(reg.is_registered("FTP"));
        assert!(reg.is_registered("IGYCRCTL"));
        assert!(reg.is_registered("IEWL"));
        assert!(reg.is_registered("DFHECP1$"));
        assert!(reg.is_registered("DFHMAPS"));
        assert!(reg.is_registered("DSNHPC"));
        assert!(reg.is_registered("DSNTIAD"));
        assert!(reg.is_registered("DSNTIAUL"));
        assert!(reg.is_registered("DSNTEP4"));
        assert!(reg.is_registered("DFSURGU0"));
    }
}
