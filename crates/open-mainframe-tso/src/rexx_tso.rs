//! TSO/REXX Integration — ADDRESS TSO host command execution and TSO-specific
//! REXX external functions.
//!
//! Provides:
//! - **`RexxTsoHost`** — bridges REXX ADDRESS TSO to the real TSO command processor
//! - **`exec_address_tso`** — execute a TSO command from REXX and capture output
//! - **`exec_address_ispexec`** — execute ISPF dialog services from REXX
//! - **TSO external functions**: OUTTRAP, SYSDSN, LISTDSI, SYSVAR, MSG

use crate::commands::execute;
use crate::parser::parse_command;
use crate::session::{AllocEntry, TsoSession};

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  RexxTsoHost — bridge between REXX ADDRESS and TSO commands
// ---------------------------------------------------------------------------

/// Host-command execution context for REXX ↔ TSO integration.
///
/// Wraps a `TsoSession` and captures command output so that OUTTRAP-style
/// stem variable capture can be implemented.
#[derive(Debug)]
pub struct RexxTsoHost {
    /// The underlying TSO session.
    session: TsoSession,
    /// OUTTRAP state: when set, command output is captured into this stem.
    outtrap_stem: Option<String>,
    /// Captured lines (while OUTTRAP is active).
    trapped_lines: Vec<String>,
    /// Maximum number of lines to capture (0 = unlimited).
    trap_max: usize,
    /// TSO variables set by LISTDSI, SYSVAR, etc.
    tso_vars: HashMap<String, String>,
}

impl RexxTsoHost {
    /// Create a new host with the given TSO session.
    pub fn new(session: TsoSession) -> Self {
        Self {
            session,
            outtrap_stem: None,
            trapped_lines: Vec::new(),
            trap_max: 0,
            tso_vars: HashMap::new(),
        }
    }

    /// Access the underlying session.
    pub fn session(&self) -> &TsoSession {
        &self.session
    }

    /// Mutable access to the session.
    pub fn session_mut(&mut self) -> &mut TsoSession {
        &mut self.session
    }

    /// Access TSO variables (set by LISTDSI, SYSVAR, etc.).
    pub fn tso_vars(&self) -> &HashMap<String, String> {
        &self.tso_vars
    }

    // -------------------------------------------------------------------
    //  ADDRESS TSO command execution
    // -------------------------------------------------------------------

    /// Execute a TSO command (as if ADDRESS TSO "cmd").
    ///
    /// Returns (rc, output_lines). If OUTTRAP is active, output is also
    /// captured into the trap buffer.
    pub fn exec_tso(&mut self, cmd: &str) -> (u32, Vec<String>) {
        let parsed = parse_command(cmd);
        let result = execute(&mut self.session, &parsed);

        if self.outtrap_stem.is_some() {
            for line in &result.output {
                if self.trap_max == 0 || self.trapped_lines.len() < self.trap_max {
                    self.trapped_lines.push(line.clone());
                }
            }
        }

        (result.rc, result.output)
    }

    // -------------------------------------------------------------------
    //  OUTTRAP — capture command output into stem variables
    // -------------------------------------------------------------------

    /// Activate OUTTRAP: subsequent TSO command output is captured into stem.
    ///
    /// `stem` should be the stem name (e.g., "OUT.").
    /// `max_lines` is the capture limit (0 = unlimited).
    pub fn outtrap_on(&mut self, stem: &str, max_lines: usize) {
        self.outtrap_stem = Some(stem.to_uppercase());
        self.trapped_lines.clear();
        self.trap_max = max_lines;
    }

    /// Deactivate OUTTRAP and return captured lines as a stem variable map.
    ///
    /// Returns entries like `("OUT.0", "3"), ("OUT.1", "first line"), ...`.
    pub fn outtrap_off(&mut self) -> HashMap<String, String> {
        let stem = match self.outtrap_stem.take() {
            Some(s) => s,
            None => return HashMap::new(),
        };

        let mut vars = HashMap::new();
        let count = self.trapped_lines.len();
        let stem_base = if stem.ends_with('.') {
            stem.to_string()
        } else {
            format!("{stem}.")
        };

        vars.insert(format!("{stem_base}0"), count.to_string());
        for (i, line) in self.trapped_lines.drain(..).enumerate() {
            vars.insert(format!("{stem_base}{}", i + 1), line);
        }
        vars
    }

    /// Check if OUTTRAP is currently active.
    pub fn is_trapping(&self) -> bool {
        self.outtrap_stem.is_some()
    }

    // -------------------------------------------------------------------
    //  SYSDSN — check dataset existence
    // -------------------------------------------------------------------

    /// SYSDSN(dsname) — returns "OK" if the dataset exists, or a diagnostic.
    pub fn sysdsn(&self, dsname: &str) -> String {
        let clean = dsname.trim().trim_matches('\'');
        if clean.is_empty() {
            return "MISSING DATASET NAME".to_string();
        }
        let qualified = self.session.qualify_dsn(clean);
        let path = self.session.resolve_dsn(&qualified);

        if path.exists() {
            "OK".to_string()
        } else {
            "DATASET NOT FOUND".to_string()
        }
    }

    // -------------------------------------------------------------------
    //  LISTDSI — dataset information
    // -------------------------------------------------------------------

    /// LISTDSI(dsname) — populates TSO variables with dataset attributes.
    ///
    /// Returns 0 on success, 16 if dataset not found.
    pub fn listdsi(&mut self, dsname: &str) -> u32 {
        let clean = dsname.trim().trim_matches('\'');
        let qualified = self.session.qualify_dsn(clean);

        // Look up dataset in allocations.
        let alloc = find_alloc_by_dsn(&self.session, &qualified);

        if let Some(entry) = alloc {
            self.tso_vars.insert("SYSDSNAME".into(), qualified.clone());
            self.tso_vars.insert("SYSVOLUME".into(), "WORK01".into());
            self.tso_vars.insert("SYSUNIT".into(), "3390".into());
            self.tso_vars.insert(
                "SYSDSORG".into(),
                entry.dcb.dsorg.clone().unwrap_or_else(|| "PS".into()),
            );
            self.tso_vars.insert(
                "SYSRECFM".into(),
                entry.dcb.recfm.clone().unwrap_or_else(|| "FB".into()),
            );
            self.tso_vars.insert(
                "SYSLRECL".into(),
                entry.dcb.lrecl.unwrap_or(80).to_string(),
            );
            self.tso_vars.insert(
                "SYSBLKSIZE".into(),
                entry.dcb.blksize.unwrap_or(27920).to_string(),
            );
            self.tso_vars.insert("SYSALLOC".into(), "100".into());
            self.tso_vars.insert("SYSUSED".into(), "50".into());
            self.tso_vars.insert("SYSPRIMARY".into(), "100".into());
            self.tso_vars.insert("SYSSECONDS".into(), "50".into());
            self.tso_vars.insert("SYSEXTENTS".into(), "1".into());
            self.tso_vars.insert("SYSCREATE".into(), "2024/01/15".into());
            self.tso_vars.insert("SYSREFDATE".into(), "2024/06/01".into());
            self.tso_vars.insert("SYSEXDATE".into(), "***NONE***".into());
            self.tso_vars.insert("SYSPASSWORD".into(), "NONE".into());
            self.tso_vars.insert("SYSRACFA".into(), "NONE".into());
            self.tso_vars.insert("SYSUPDATED".into(), "YES".into());
            self.tso_vars.insert("SYSTRKSCYL".into(), "15".into());
            self.tso_vars.insert("SYSBLKSTRK".into(), "20".into());
            self.tso_vars.insert("SYSADIRBLK".into(), "0".into());
            self.tso_vars.insert("SYSUDIRBLK".into(), "0".into());
            self.tso_vars.insert("SYSMEMBERS".into(), "0".into());
            self.tso_vars.insert("SYSREASON".into(), "0".into());
            self.tso_vars.insert("SYSMSGLVL1".into(), String::new());
            self.tso_vars.insert("SYSMSGLVL2".into(), String::new());
            0
        } else {
            // Not allocated — check file system.
            let path = self.session.resolve_dsn(&qualified);
            if path.exists() {
                self.tso_vars.insert("SYSDSNAME".into(), qualified);
                self.tso_vars.insert("SYSVOLUME".into(), "WORK01".into());
                self.tso_vars.insert("SYSUNIT".into(), "3390".into());
                self.tso_vars.insert("SYSDSORG".into(), "PS".into());
                self.tso_vars.insert("SYSRECFM".into(), "FB".into());
                self.tso_vars.insert("SYSLRECL".into(), "80".into());
                self.tso_vars.insert("SYSBLKSIZE".into(), "27920".into());
                self.tso_vars.insert("SYSREASON".into(), "0".into());
                self.tso_vars.insert("SYSMSGLVL1".into(), String::new());
                self.tso_vars.insert("SYSMSGLVL2".into(), String::new());
                0
            } else {
                self.tso_vars.insert("SYSREASON".into(), "5".into());
                self.tso_vars.insert(
                    "SYSMSGLVL1".into(),
                    format!("DATASET {qualified} NOT FOUND"),
                );
                self.tso_vars.insert("SYSMSGLVL2".into(), String::new());
                16
            }
        }
    }

    // -------------------------------------------------------------------
    //  SYSVAR — TSO/ISPF system variables
    // -------------------------------------------------------------------

    /// SYSVAR(name) — return TSO session variable.
    pub fn sysvar(&self, name: &str) -> String {
        let upper = name.to_uppercase();
        match upper.as_str() {
            "SYSUID" | "SYSPREF" => {
                self.session.profile.prefix.clone().unwrap_or_default()
            }
            "SYSNAME" => "OPENMF".to_string(),
            "SYSLTERM" => "24".to_string(),
            "SYSWTERM" => "80".to_string(),
            "SYSENV" => "FORE".to_string(),
            "SYSICMD" => String::new(),
            "SYSISPF" => "ACTIVE".to_string(),
            "SYSNEST" => "NO".to_string(),
            "SYSPCMD" => String::new(),
            "SYSSCMD" => String::new(),
            "SYSSRV" => "0".to_string(),
            "SYSCPU" => "0".to_string(),
            "SYSLRACF" => "AVAILABLE".to_string(),
            "SYSTERMID" => "3270A001".to_string(),
            "SYSPROC" => "SYSPROC".to_string(),
            "SYSEXEC" => "SYSEXEC".to_string(),
            _ => {
                // Check tso_vars (set by LISTDSI etc.).
                self.tso_vars
                    .get(&upper)
                    .cloned()
                    .unwrap_or_default()
            }
        }
    }

    // -------------------------------------------------------------------
    //  MSG — toggle TSO message display
    // -------------------------------------------------------------------

    /// MSG(ON|OFF) — toggle message display. Returns previous setting.
    pub fn msg(&mut self, setting: &str) -> String {
        let upper = setting.to_uppercase();
        let prev = if self.session.profile.msgid { "ON" } else { "OFF" };
        let prev_str = prev.to_string();

        match upper.as_str() {
            "ON" => self.session.profile.msgid = true,
            "OFF" => self.session.profile.msgid = false,
            _ => {} // Return current without changing.
        }
        prev_str
    }
}

/// Find an allocation entry by dataset name in the session.
fn find_alloc_by_dsn<'a>(session: &'a TsoSession, dsn: &str) -> Option<&'a AllocEntry> {
    let upper = dsn.to_uppercase();
    session.list_allocs().into_iter().find(|a| a.dsn == upper)
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::session::TsoSession;

    fn test_host() -> RexxTsoHost {
        let temp = std::env::temp_dir().join("tso_rexx_tests");
        let _ = std::fs::create_dir_all(&temp);
        let session = TsoSession::new("USER01", temp);
        RexxTsoHost::new(session)
    }

    // -- ADDRESS TSO --

    #[test]
    fn test_exec_tso_time() {
        let mut host = test_host();
        let (rc, output) = host.exec_tso("TIME");
        assert_eq!(rc, 0);
        assert!(!output.is_empty());
        assert!(output[0].contains("TIME-"));
    }

    #[test]
    fn test_exec_tso_profile() {
        let mut host = test_host();
        let (rc, output) = host.exec_tso("PROFILE");
        assert_eq!(rc, 0);
        assert!(output.iter().any(|l| l.contains("PREFIX")));
    }

    #[test]
    fn test_exec_tso_unknown_command() {
        let mut host = test_host();
        let (rc, _) = host.exec_tso("XYZZY");
        assert!(rc > 0);
    }

    // -- OUTTRAP --

    #[test]
    fn test_outtrap_basic() {
        let mut host = test_host();
        host.outtrap_on("OUT.", 0);
        assert!(host.is_trapping());

        host.exec_tso("TIME");
        host.exec_tso("PROFILE");

        let vars = host.outtrap_off();
        assert!(!host.is_trapping());
        let count: usize = vars.get("OUT.0").unwrap().parse().unwrap();
        assert!(count > 0);
        assert!(vars.contains_key("OUT.1"));
    }

    #[test]
    fn test_outtrap_max_lines() {
        let mut host = test_host();
        host.outtrap_on("X.", 1);
        host.exec_tso("PROFILE"); // Produces multiple lines.

        let vars = host.outtrap_off();
        let count: usize = vars.get("X.0").unwrap().parse().unwrap();
        assert_eq!(count, 1);
    }

    #[test]
    fn test_outtrap_no_stem_dot() {
        let mut host = test_host();
        host.outtrap_on("STEM", 0);
        host.exec_tso("TIME");
        let vars = host.outtrap_off();
        assert!(vars.contains_key("STEM.0"));
    }

    // -- SYSDSN --

    #[test]
    fn test_sysdsn_not_found() {
        let host = test_host();
        let result = host.sysdsn("'NONEXISTENT.DATASET'");
        assert_eq!(result, "DATASET NOT FOUND");
    }

    #[test]
    fn test_sysdsn_missing_name() {
        let host = test_host();
        let result = host.sysdsn("");
        assert_eq!(result, "MISSING DATASET NAME");
    }

    #[test]
    fn test_sysdsn_existing() {
        let host = test_host();
        // Create a file at the resolved path.
        let dsn = host.session.qualify_dsn("TEST.DATA");
        let path = host.session.resolve_dsn(&dsn);
        if let Some(parent) = path.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        std::fs::write(&path, "test").unwrap();

        let result = host.sysdsn("TEST.DATA");
        assert_eq!(result, "OK");
    }

    // -- LISTDSI --

    #[test]
    fn test_listdsi_not_found() {
        let mut host = test_host();
        let rc = host.listdsi("'MISSING.DATASET'");
        assert_eq!(rc, 16);
        assert_eq!(host.tso_vars().get("SYSREASON").unwrap(), "5");
    }

    #[test]
    fn test_listdsi_from_alloc() {
        let mut host = test_host();
        // Allocate a dataset first (quoted = fully qualified).
        host.exec_tso("ALLOC FI(INDD) DA('SYS1.PARMLIB') SHR");

        let rc = host.listdsi("'SYS1.PARMLIB'");
        assert_eq!(rc, 0);
        // SYSDSNAME reflects the qualified name.
        let dsname = host.tso_vars().get("SYSDSNAME").unwrap();
        assert!(dsname.contains("SYS1.PARMLIB"));
        assert!(host.tso_vars().contains_key("SYSLRECL"));
        assert!(host.tso_vars().contains_key("SYSRECFM"));
    }

    // -- SYSVAR --

    #[test]
    fn test_sysvar_uid() {
        let host = test_host();
        let uid = host.sysvar("SYSUID");
        assert_eq!(uid, "USER01");
    }

    #[test]
    fn test_sysvar_name() {
        let host = test_host();
        assert_eq!(host.sysvar("SYSNAME"), "OPENMF");
    }

    #[test]
    fn test_sysvar_ispf() {
        let host = test_host();
        assert_eq!(host.sysvar("SYSISPF"), "ACTIVE");
    }

    #[test]
    fn test_sysvar_term_size() {
        let host = test_host();
        assert_eq!(host.sysvar("SYSLTERM"), "24");
        assert_eq!(host.sysvar("SYSWTERM"), "80");
    }

    #[test]
    fn test_sysvar_from_listdsi() {
        let mut host = test_host();
        // LISTDSI populates TSO vars, SYSVAR can read them.
        host.tso_vars.insert("SYSDSORG".into(), "PO".into());
        assert_eq!(host.sysvar("SYSDSORG"), "PO");
    }

    // -- MSG --

    #[test]
    fn test_msg_toggle() {
        let mut host = test_host();
        // Default msgid is true.
        let prev = host.msg("OFF");
        assert_eq!(prev, "ON");
        assert!(!host.session().profile.msgid);

        let prev2 = host.msg("ON");
        assert_eq!(prev2, "OFF");
        assert!(host.session().profile.msgid);
    }

    #[test]
    fn test_msg_query() {
        let mut host = test_host();
        // Querying without changing.
        let result = host.msg("");
        assert!(result == "ON" || result == "OFF");
    }
}
