//! # CLIST TSO/ISPF Integration (CL-104)
//!
//! TSO command dispatch, ISPEXEC, ISREDIT, LISTDSI, nested CLIST
//! execution, and PROC statement parameter handling.

// ─────────────────────── TSO Environment Trait ───────────────────────

/// TSO environment abstraction.
///
/// Implement this trait to provide a TSO command execution environment
/// for CLIST scripts. This allows testing without a real TSO session.
pub trait TsoEnvironment: Send {
    /// Execute a TSO command. Returns the return code.
    fn execute_command(&self, command: &str) -> i32;

    /// Execute an ISPF Dialog Manager service (ISPEXEC).
    fn ispexec(&self, service: &str) -> i32;

    /// Execute an ISPF Edit macro service (ISREDIT).
    fn isredit(&self, command: &str) -> i32;

    /// LISTDSI — retrieve dataset attributes.
    fn listdsi(&self, dsname: &str) -> Option<DatasetAttributes>;

    /// Get an ISPF shared variable.
    fn get_ispf_variable(&self, name: &str) -> Option<String>;

    /// Set an ISPF shared variable.
    fn set_ispf_variable(&self, name: &str, value: &str);
}

// ─────────────────────── Dataset Attributes ───────────────────────

/// Dataset attributes returned by LISTDSI.
#[derive(Debug, Clone, Default)]
pub struct DatasetAttributes {
    /// Data set organization (PS, PO, DA, VSAM).
    pub dsorg: String,
    /// Record format (FB, VB, FBA, etc.).
    pub recfm: String,
    /// Logical record length.
    pub lrecl: u32,
    /// Block size.
    pub blksize: u32,
    /// Volume serial.
    pub volume: String,
    /// Primary allocation.
    pub primary: u32,
    /// Secondary allocation.
    pub secondary: u32,
    /// Allocation units (CYLINDER, TRACK, BLOCK).
    pub units: String,
    /// Number of members (PDS only).
    pub members: u32,
    /// Creation date.
    pub creation_date: String,
    /// Expiration date.
    pub expiration_date: String,
}

/// LISTDSI result.
#[derive(Debug, Clone)]
pub struct ListdsiResult {
    /// Return code (0=OK, 4=warning, 16=error).
    pub return_code: i32,
    /// Reason code.
    pub reason_code: i32,
    /// Attributes if successful.
    pub attributes: Option<DatasetAttributes>,
}

// ─────────────────────── Mock TSO Environment ───────────────────────

/// A mock TSO environment for testing.
pub struct MockTsoEnvironment {
    /// Registered datasets.
    datasets: std::collections::HashMap<String, DatasetAttributes>,
    /// ISPF shared variable pool.
    ispf_vars: std::sync::Mutex<std::collections::HashMap<String, String>>,
    /// Command history.
    commands: std::sync::Mutex<Vec<String>>,
}

impl MockTsoEnvironment {
    /// Create a new mock environment.
    pub fn new() -> Self {
        Self {
            datasets: std::collections::HashMap::new(),
            ispf_vars: std::sync::Mutex::new(std::collections::HashMap::new()),
            commands: std::sync::Mutex::new(Vec::new()),
        }
    }

    /// Register a dataset for LISTDSI.
    pub fn add_dataset(&mut self, name: &str, attrs: DatasetAttributes) {
        self.datasets.insert(name.to_uppercase(), attrs);
    }

    /// Get command history.
    pub fn command_history(&self) -> Vec<String> {
        self.commands.lock().unwrap().clone()
    }
}

impl Default for MockTsoEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

impl TsoEnvironment for MockTsoEnvironment {
    fn execute_command(&self, command: &str) -> i32 {
        if let Ok(mut cmds) = self.commands.lock() {
            cmds.push(command.to_string());
        }
        0
    }

    fn ispexec(&self, service: &str) -> i32 {
        if let Ok(mut cmds) = self.commands.lock() {
            cmds.push(format!("ISPEXEC {service}"));
        }
        0
    }

    fn isredit(&self, command: &str) -> i32 {
        if let Ok(mut cmds) = self.commands.lock() {
            cmds.push(format!("ISREDIT {command}"));
        }
        0
    }

    fn listdsi(&self, dsname: &str) -> Option<DatasetAttributes> {
        self.datasets.get(&dsname.to_uppercase()).cloned()
    }

    fn get_ispf_variable(&self, name: &str) -> Option<String> {
        self.ispf_vars.lock().ok()?.get(&name.to_uppercase()).cloned()
    }

    fn set_ispf_variable(&self, name: &str, value: &str) {
        if let Ok(mut vars) = self.ispf_vars.lock() {
            vars.insert(name.to_uppercase(), value.to_string());
        }
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::ClistInterpreter;

    // ─── CL-104.1: TSO Command Dispatch ───

    #[test]
    fn test_tso_command_dispatch() {
        let mut interp = ClistInterpreter::new();
        let mock = MockTsoEnvironment::new();
        interp.set_tso(Box::new(mock));

        interp.execute("ALLOC DA('MY.DATA') SHR").unwrap();
        // TSO command should have been dispatched
    }

    // ─── CL-104.2: ISPEXEC ───

    #[test]
    fn test_ispexec() {
        let mut interp = ClistInterpreter::new();
        let mock = MockTsoEnvironment::new();
        interp.set_tso(Box::new(mock));

        interp.execute("ISPEXEC DISPLAY PANEL(MYPANEL)").unwrap();
    }

    // ─── CL-104.3: ISREDIT ───

    #[test]
    fn test_isredit() {
        let mut interp = ClistInterpreter::new();
        let mock = MockTsoEnvironment::new();
        interp.set_tso(Box::new(mock));

        interp.execute("ISREDIT FIND 'HELLO'").unwrap();
    }

    // ─── CL-104.4: LISTDSI ───

    #[test]
    fn test_listdsi() {
        let mut interp = ClistInterpreter::new();
        let mut mock = MockTsoEnvironment::new();
        mock.add_dataset("MY.DATA.SET", DatasetAttributes {
            dsorg: "PS".to_string(),
            recfm: "FB".to_string(),
            lrecl: 80,
            blksize: 27920,
            volume: "VOL001".to_string(),
            primary: 10,
            secondary: 5,
            units: "TRACK".to_string(),
            members: 0,
            ..Default::default()
        });
        interp.set_tso(Box::new(mock));

        interp.execute("LISTDSI 'MY.DATA.SET'").unwrap();
        assert_eq!(interp.last_cc(), 0);
    }

    #[test]
    fn test_listdsi_not_found() {
        let mut interp = ClistInterpreter::new();
        let mock = MockTsoEnvironment::new();
        interp.set_tso(Box::new(mock));

        interp.execute("LISTDSI 'NOSUCH.DATA'").unwrap();
        assert_eq!(interp.last_cc(), 16);
    }

    // ─── CL-104.5: Nested CLIST Execution ───

    #[test]
    fn test_nested_exec() {
        let mut interp = ClistInterpreter::new();
        interp.execute("EXEC 'MYCLIST'").unwrap();
        assert!(interp.output().iter().any(|o| o.contains("MYCLIST")));
    }

    // ─── CL-104.6: PROC Statement ───

    #[test]
    fn test_proc_statement() {
        let mut interp = ClistInterpreter::new();
        interp.execute("PROC 2").unwrap();
        // PROC is processed at invocation time; here it's a no-op
    }

    // ─── Integration Tests ───

    #[test]
    fn test_mock_tso_environment() {
        let mock = MockTsoEnvironment::new();
        assert_eq!(mock.execute_command("ALLOC DA('X') SHR"), 0);
        assert_eq!(mock.ispexec("DISPLAY PANEL(P1)"), 0);
        assert_eq!(mock.isredit("FIND 'HELLO'"), 0);
        assert!(mock.listdsi("NOSUCH").is_none());
    }

    #[test]
    fn test_ispf_variables() {
        let mock = MockTsoEnvironment::new();
        mock.set_ispf_variable("ZUSER", "IBMUSER");
        assert_eq!(mock.get_ispf_variable("ZUSER"), Some("IBMUSER".to_string()));
    }

    #[test]
    fn test_dataset_attributes() {
        let attrs = DatasetAttributes {
            dsorg: "PO".to_string(),
            recfm: "FB".to_string(),
            lrecl: 80,
            blksize: 27920,
            volume: "SYSA01".to_string(),
            primary: 50,
            secondary: 10,
            units: "CYLINDER".to_string(),
            members: 125,
            ..Default::default()
        };
        assert_eq!(attrs.dsorg, "PO");
        assert_eq!(attrs.lrecl, 80);
        assert_eq!(attrs.members, 125);
    }

    #[test]
    fn test_full_clist_with_tso() {
        let mut interp = ClistInterpreter::new();
        let mut mock = MockTsoEnvironment::new();
        mock.add_dataset("SYS1.MACLIB", DatasetAttributes {
            dsorg: "PO".to_string(),
            recfm: "FB".to_string(),
            lrecl: 80,
            blksize: 27920,
            volume: "SYSRES".to_string(),
            primary: 100,
            secondary: 20,
            units: "TRACK".to_string(),
            members: 500,
            ..Default::default()
        });
        interp.set_tso(Box::new(mock));

        let source = r#"
PROC 0
CONTROL NOLIST NOMSG
LISTDSI 'SYS1.MACLIB'
WRITE 'Dataset found'
EXIT 0
"#;
        let rc = interp.execute(source).unwrap();
        assert_eq!(rc, 0);
        assert!(interp.output().contains(&"Dataset found".to_string()));
    }
}
