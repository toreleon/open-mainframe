//! CICS command handler bridge.
//!
//! Connects the COBOL interpreter to the zos-cics runtime, dispatching
//! EXEC CICS commands (SEND MAP, RECEIVE MAP, RETURN, READ, etc.)
//! to the appropriate runtime methods.

use std::collections::HashMap;

use zos_cics::runtime::{CicsRuntime, FileMode, FileRecord};
use zos_cics::terminal::TerminalManager;
use zos_cics::CicsResponse;
use zos_runtime::interpreter::{CicsCommandHandler, Environment, InterpreterError};
use zos_runtime::value::CobolValue;

type Result<T> = std::result::Result<T, InterpreterError>;

/// Concrete CICS command handler that dispatches to zos-cics runtime.
pub struct CicsBridge {
    /// CICS runtime (EIB, program control, file manager).
    pub runtime: CicsRuntime,
    /// Terminal manager for SEND/RECEIVE MAP.
    pub terminals: TerminalManager,
    /// Active terminal ID for this transaction.
    terminal_id: String,
    /// Program return flag - when set, the interpreter should stop.
    pub returned: bool,
    /// TRANSID to return to (for conversational pseudo-conversations).
    pub return_transid: Option<String>,
    /// Program to XCTL to.
    pub xctl_program: Option<String>,
    /// COMMAREA variable name (the COBOL variable holding COMMAREA data).
    pub commarea_var: Option<String>,
}

impl CicsBridge {
    /// Create a new CICS bridge for a transaction.
    pub fn new(transaction_id: &str, terminal_id: &str) -> Self {
        let mut runtime = CicsRuntime::new(transaction_id);
        runtime.eib.set_terminal_id(terminal_id);

        let mut terminals = TerminalManager::new();
        // Pre-create the terminal
        terminals.get_or_create(terminal_id);

        Self {
            runtime,
            terminals,
            terminal_id: terminal_id.to_string(),
            returned: false,
            return_transid: None,
            xctl_program: None,
            commarea_var: None,
        }
    }

    /// Reset the bridge for a new program execution (after XCTL).
    pub fn reset_for_xctl(&mut self) {
        self.returned = false;
        self.return_transid = None;
        self.xctl_program = None;
        self.commarea_var = None;
        self.runtime.eib.reset_for_command();
    }

    /// Helper to extract a named string option from CICS command options.
    fn get_option_str<'a>(
        options: &'a [(String, Option<CobolValue>)],
        name: &str,
    ) -> Option<String> {
        options.iter().find_map(|(n, v)| {
            if n.eq_ignore_ascii_case(name) {
                v.as_ref().map(|val| val.to_display_string().trim().to_string())
            } else {
                None
            }
        })
    }

    /// Helper to check if a flag option is present (no value needed).
    fn has_option(options: &[(String, Option<CobolValue>)], name: &str) -> bool {
        options.iter().any(|(n, _)| n.eq_ignore_ascii_case(name))
    }

    /// Handle SEND MAP command.
    fn handle_send_map(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let map_name = Self::get_option_str(options, "MAP").unwrap_or_default();
        let mapset_name = Self::get_option_str(options, "MAPSET").unwrap_or_default();
        let from_var = Self::get_option_str(options, "FROM");
        let erase = Self::has_option(options, "ERASE");
        let eraseaup = Self::has_option(options, "ERASEAUP");
        let _cursor = Self::get_option_str(options, "CURSOR");
        let _freekb = Self::has_option(options, "FREEKB");
        let _alarm = Self::has_option(options, "ALARM");

        // Build field data from COBOL variables
        // In a real implementation, we'd read the BMS symbolic map structure
        // For now, collect any FROM data or map fields from the environment
        let mut data: HashMap<String, Vec<u8>> = HashMap::new();

        if let Some(ref from) = from_var {
            // FROM(data-area) - send the whole data area
            if let Some(val) = env.get(from) {
                data.insert("__FROM__".to_string(), val.to_display_string().into_bytes());
            }
        }

        // Log the send for debugging
        let opts_desc = if erase { " ERASE" } else if eraseaup { " ERASEAUP" } else { "" };
        env.display(
            &format!(
                "[CICS] SEND MAP({}) MAPSET({}){} - screen updated",
                map_name, mapset_name, opts_desc
            ),
            false,
        )?;

        self.runtime.eib.set_response(CicsResponse::Normal);
        Ok(())
    }

    /// Handle SEND TEXT command.
    fn handle_send_text(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let from_var = Self::get_option_str(options, "FROM");
        let _length = Self::get_option_str(options, "LENGTH");
        let erase = Self::has_option(options, "ERASE");
        let _freekb = Self::has_option(options, "FREEKB");
        let _alarm = Self::has_option(options, "ALARM");

        if let Some(ref from) = from_var {
            if let Some(val) = env.get(from) {
                let text = val.to_display_string();
                let opts_desc = if erase { " ERASE" } else { "" };
                env.display(
                    &format!("[CICS] SEND TEXT{}: {}", opts_desc, text.trim()),
                    false,
                )?;
            }
        }

        self.runtime.eib.set_response(CicsResponse::Normal);
        Ok(())
    }

    /// Handle RECEIVE MAP command.
    fn handle_receive_map(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let map_name = Self::get_option_str(options, "MAP").unwrap_or_default();
        let mapset_name = Self::get_option_str(options, "MAPSET").unwrap_or_default();
        let into_var = Self::get_option_str(options, "INTO");

        // In a real 3270 terminal, this would block waiting for user input.
        // For the interpreter, we simulate by prompting on stdin.
        env.display(
            &format!(
                "[CICS] RECEIVE MAP({}) MAPSET({}) - awaiting input",
                map_name, mapset_name
            ),
            false,
        )?;

        // If INTO is specified, read input into that variable
        if let Some(ref into) = into_var {
            let input = env.accept()?;
            env.set(into, CobolValue::Alphanumeric(input))?;
        }

        // Set EIB aid to ENTER by default
        self.runtime.eib.set_aid(zos_cics::runtime::eib::aid::ENTER);
        self.runtime.eib.set_response(CicsResponse::Normal);
        Ok(())
    }

    /// Handle RETURN command.
    fn handle_return(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let transid = Self::get_option_str(options, "TRANSID");
        let commarea = Self::get_option_str(options, "COMMAREA");

        if let Some(ref tid) = transid {
            if let Some(ref ca) = commarea {
                env.display(
                    &format!("[CICS] RETURN TRANSID({}) COMMAREA({})", tid, ca),
                    false,
                )?;
                self.commarea_var = Some(ca.clone());
            } else {
                env.display(
                    &format!("[CICS] RETURN TRANSID({})", tid),
                    false,
                )?;
            }
            self.return_transid = Some(tid.clone());
        } else {
            env.display("[CICS] RETURN", false)?;
        }

        self.returned = true;
        self.runtime.eib.set_response(CicsResponse::Normal);
        env.stop();
        Ok(())
    }

    /// Handle XCTL command.
    fn handle_xctl(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let program = Self::get_option_str(options, "PROGRAM").unwrap_or_default();
        let commarea = Self::get_option_str(options, "COMMAREA");

        if let Some(ref ca) = commarea {
            env.display(
                &format!("[CICS] XCTL PROGRAM({}) COMMAREA({})", program, ca),
                false,
            )?;
            self.commarea_var = Some(ca.clone());
        } else {
            env.display(
                &format!("[CICS] XCTL PROGRAM({})", program),
                false,
            )?;
        }

        self.xctl_program = Some(program);
        self.runtime.eib.set_response(CicsResponse::Normal);
        env.stop();
        Ok(())
    }

    /// Handle READ FILE command.
    fn handle_read(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let file_name = Self::get_option_str(options, "FILE")
            .or_else(|| Self::get_option_str(options, "DATASET"))
            .unwrap_or_default();
        let into_var = Self::get_option_str(options, "INTO");
        let ridfld_var = Self::get_option_str(options, "RIDFLD");
        let update = Self::has_option(options, "UPDATE");

        // Get the key from RIDFLD variable
        let key = if let Some(ref ridfld) = ridfld_var {
            env.get(ridfld)
                .map(|v| v.to_display_string().trim().to_string().into_bytes())
                .unwrap_or_default()
        } else {
            Vec::new()
        };

        let mode = if update {
            FileMode::Update
        } else {
            FileMode::Read
        };

        match self.runtime.files.read(&file_name, &key, mode) {
            Ok(record) => {
                // Store record data into the INTO variable
                if let Some(ref into) = into_var {
                    let data_str = String::from_utf8_lossy(&record.data).to_string();
                    env.set(into, CobolValue::Alphanumeric(data_str))?;
                }
                self.runtime.eib.set_response(CicsResponse::Normal);
            }
            Err(_) => {
                self.runtime.eib.set_response(CicsResponse::Notfnd);
            }
        }

        // Set RESP/RESP2 variables if specified
        self.update_resp_variables(options, env)?;

        Ok(())
    }

    /// Handle WRITE FILE command.
    fn handle_write(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let file_name = Self::get_option_str(options, "FILE")
            .or_else(|| Self::get_option_str(options, "DATASET"))
            .unwrap_or_default();
        let from_var = Self::get_option_str(options, "FROM");
        let ridfld_var = Self::get_option_str(options, "RIDFLD");

        let key = if let Some(ref ridfld) = ridfld_var {
            env.get(ridfld)
                .map(|v| v.to_display_string().trim().to_string().into_bytes())
                .unwrap_or_default()
        } else {
            Vec::new()
        };

        let data = if let Some(ref from) = from_var {
            env.get(from)
                .map(|v| v.to_display_string().into_bytes())
                .unwrap_or_default()
        } else {
            Vec::new()
        };

        let record = FileRecord {
            key: key.clone(),
            data,
        };

        match self.runtime.files.write(&file_name, record) {
            Ok(()) => {
                self.runtime.eib.set_response(CicsResponse::Normal);
            }
            Err(_) => {
                self.runtime.eib.set_response(CicsResponse::Duprec);
            }
        }

        self.update_resp_variables(options, env)?;
        Ok(())
    }

    /// Handle REWRITE FILE command.
    fn handle_rewrite(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let file_name = Self::get_option_str(options, "FILE")
            .or_else(|| Self::get_option_str(options, "DATASET"))
            .unwrap_or_default();
        let from_var = Self::get_option_str(options, "FROM");

        let data = if let Some(ref from) = from_var {
            env.get(from)
                .map(|v| v.to_display_string().into_bytes())
                .unwrap_or_default()
        } else {
            Vec::new()
        };

        match self.runtime.files.rewrite(&file_name, &data) {
            Ok(()) => {
                self.runtime.eib.set_response(CicsResponse::Normal);
            }
            Err(_) => {
                self.runtime.eib.set_response(CicsResponse::Error);
            }
        }

        self.update_resp_variables(options, env)?;
        Ok(())
    }

    /// Handle STARTBR (start browse) command.
    fn handle_startbr(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let file_name = Self::get_option_str(options, "FILE")
            .or_else(|| Self::get_option_str(options, "DATASET"))
            .unwrap_or_default();
        let ridfld_var = Self::get_option_str(options, "RIDFLD");

        let key = if let Some(ref ridfld) = ridfld_var {
            env.get(ridfld)
                .map(|v| v.to_display_string().trim().to_string().into_bytes())
                .unwrap_or_default()
        } else {
            Vec::new()
        };

        match self.runtime.files.startbr(&file_name, &key) {
            Ok(_token) => {
                self.runtime.eib.set_response(CicsResponse::Normal);
            }
            Err(_) => {
                self.runtime.eib.set_response(CicsResponse::Notfnd);
            }
        }

        self.update_resp_variables(options, env)?;
        Ok(())
    }

    /// Handle READNEXT command.
    fn handle_readnext(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let into_var = Self::get_option_str(options, "INTO");

        // Use token 0 as default browse token (simplified)
        match self.runtime.files.readnext(0) {
            Ok(record) => {
                if let Some(ref into) = into_var {
                    let data_str = String::from_utf8_lossy(&record.data).to_string();
                    env.set(into, CobolValue::Alphanumeric(data_str))?;
                }
                self.runtime.eib.set_response(CicsResponse::Normal);
            }
            Err(_) => {
                self.runtime.eib.set_response(CicsResponse::Endfile);
            }
        }

        self.update_resp_variables(options, env)?;
        Ok(())
    }

    /// Handle READPREV command.
    fn handle_readprev(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let into_var = Self::get_option_str(options, "INTO");

        match self.runtime.files.readprev(0) {
            Ok(record) => {
                if let Some(ref into) = into_var {
                    let data_str = String::from_utf8_lossy(&record.data).to_string();
                    env.set(into, CobolValue::Alphanumeric(data_str))?;
                }
                self.runtime.eib.set_response(CicsResponse::Normal);
            }
            Err(_) => {
                self.runtime.eib.set_response(CicsResponse::Endfile);
            }
        }

        self.update_resp_variables(options, env)?;
        Ok(())
    }

    /// Handle ENDBR (end browse) command.
    fn handle_endbr(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        // End all active browse sessions (simplified)
        let _ = self.runtime.files.endbr(0);
        self.runtime.eib.set_response(CicsResponse::Normal);
        self.update_resp_variables(options, env)?;
        Ok(())
    }

    /// Handle HANDLE ABEND command.
    fn handle_handle_abend(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        _env: &mut Environment,
    ) -> Result<()> {
        let label = Self::get_option_str(options, "LABEL")
            .or_else(|| Self::get_option_str(options, "PROGRAM"));

        if let Some(lbl) = label {
            self.runtime.handle_abend(&lbl);
        }

        self.runtime.eib.set_response(CicsResponse::Normal);
        Ok(())
    }

    /// Handle ABEND command.
    fn handle_abend(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let abcode = Self::get_option_str(options, "ABCODE").unwrap_or_else(|| "ASRA".to_string());

        env.display(&format!("[CICS] ABEND({})", abcode), false)?;
        env.stop();
        Ok(())
    }

    /// Handle ASSIGN command (retrieve system values).
    fn handle_assign(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        for (name, _value) in options {
            let upper = name.to_uppercase();
            match upper.as_str() {
                "APPLID" => {
                    // Application ID - provide a default
                    if let Some(val) = _value {
                        let var_name = val.to_display_string().trim().to_string();
                        env.set(&var_name, CobolValue::Alphanumeric("CARDDEMO".to_string()))?;
                    }
                }
                "SYSID" => {
                    if let Some(val) = _value {
                        let var_name = val.to_display_string().trim().to_string();
                        env.set(&var_name, CobolValue::Alphanumeric("ZOS1".to_string()))?;
                    }
                }
                _ => {
                    // Unknown ASSIGN option - ignore
                }
            }
        }

        self.runtime.eib.set_response(CicsResponse::Normal);
        Ok(())
    }

    /// Handle HANDLE CONDITION command.
    fn handle_handle_condition(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        _env: &mut Environment,
    ) -> Result<()> {
        for (condition, label_val) in options {
            if let Some(val) = label_val {
                let label = val.to_display_string().trim().to_string();
                self.runtime.handle_condition(condition, &label);
            }
        }

        self.runtime.eib.set_response(CicsResponse::Normal);
        Ok(())
    }

    /// Handle IGNORE CONDITION command.
    fn handle_ignore_condition(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        _env: &mut Environment,
    ) -> Result<()> {
        for (condition, _) in options {
            self.runtime.ignore_condition(condition);
        }

        self.runtime.eib.set_response(CicsResponse::Normal);
        Ok(())
    }

    /// Handle WRITEQ TD (Transient Data queue write).
    fn handle_writeq_td(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let queue = Self::get_option_str(options, "QUEUE").unwrap_or_default();
        let from_var = Self::get_option_str(options, "FROM");

        if let Some(ref from) = from_var {
            if let Some(val) = env.get(from) {
                env.display(
                    &format!("[CICS] WRITEQ TD QUEUE({}) FROM: {}", queue, val.to_display_string().trim()),
                    false,
                )?;
            }
        }

        self.runtime.eib.set_response(CicsResponse::Normal);
        Ok(())
    }

    /// Update RESP and RESP2 variables if specified in options.
    fn update_resp_variables(
        &self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        if let Some(resp_var) = Self::get_option_str(options, "RESP") {
            env.set(
                &resp_var,
                CobolValue::from_i64(self.runtime.eib.eibresp as i64),
            )?;
        }
        if let Some(resp2_var) = Self::get_option_str(options, "RESP2") {
            env.set(
                &resp2_var,
                CobolValue::from_i64(self.runtime.eib.eibresp2 as i64),
            )?;
        }
        Ok(())
    }
}

impl CicsCommandHandler for CicsBridge {
    fn execute(
        &mut self,
        command: &str,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let cmd = command.to_uppercase();

        // Reset EIB for new command
        self.runtime.eib.reset_for_command();

        match cmd.as_str() {
            "SEND" => {
                // Distinguish between SEND MAP and SEND TEXT
                if Self::has_option(options, "MAP") {
                    self.handle_send_map(options, env)
                } else if Self::has_option(options, "TEXT") || Self::has_option(options, "FROM") {
                    self.handle_send_text(options, env)
                } else {
                    self.handle_send_text(options, env)
                }
            }
            "RECEIVE" => {
                self.handle_receive_map(options, env)
            }
            "RETURN" => {
                self.handle_return(options, env)
            }
            "XCTL" => {
                self.handle_xctl(options, env)
            }
            "READ" => {
                self.handle_read(options, env)
            }
            "WRITE" => {
                self.handle_write(options, env)
            }
            "REWRITE" => {
                self.handle_rewrite(options, env)
            }
            "DELETE" => {
                let file_name = Self::get_option_str(options, "FILE")
                    .or_else(|| Self::get_option_str(options, "DATASET"))
                    .unwrap_or_default();
                let ridfld_var = Self::get_option_str(options, "RIDFLD");
                let key = ridfld_var.and_then(|r| {
                    env.get(&r).map(|v| v.to_display_string().trim().to_string().into_bytes())
                });

                match self.runtime.files.delete(&file_name, key.as_deref()) {
                    Ok(()) => self.runtime.eib.set_response(CicsResponse::Normal),
                    Err(_) => self.runtime.eib.set_response(CicsResponse::Notfnd),
                }
                self.update_resp_variables(options, env)
            }
            "STARTBR" => {
                self.handle_startbr(options, env)
            }
            "READNEXT" => {
                self.handle_readnext(options, env)
            }
            "READPREV" => {
                self.handle_readprev(options, env)
            }
            "ENDBR" => {
                self.handle_endbr(options, env)
            }
            "HANDLE" => {
                // HANDLE ABEND or HANDLE CONDITION
                if Self::has_option(options, "ABEND") {
                    self.handle_handle_abend(options, env)
                } else {
                    self.handle_handle_condition(options, env)
                }
            }
            "IGNORE" => {
                self.handle_ignore_condition(options, env)
            }
            "ABEND" => {
                self.handle_abend(options, env)
            }
            "ASSIGN" => {
                self.handle_assign(options, env)
            }
            "WRITEQ" => {
                self.handle_writeq_td(options, env)
            }
            "GETMAIN" => {
                let length = Self::get_option_str(options, "LENGTH")
                    .and_then(|s| s.parse::<usize>().ok())
                    .unwrap_or(0);
                match self.runtime.getmain(length) {
                    Ok(_) => self.runtime.eib.set_response(CicsResponse::Normal),
                    Err(_) => self.runtime.eib.set_response(CicsResponse::Lengerr),
                }
                self.update_resp_variables(options, env)
            }
            "FREEMAIN" => {
                let _ = self.runtime.freemain();
                self.update_resp_variables(options, env)
            }
            _ => {
                // Unknown command - log and continue
                env.display(
                    &format!("[CICS] Unhandled command: {} (skipped)", cmd),
                    false,
                )?;
                self.runtime.eib.set_response(CicsResponse::Normal);
                Ok(())
            }
        }
    }

    fn as_any_mut(&mut self) -> Option<&mut dyn std::any::Any> {
        Some(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::BufReader;

    fn create_test_env() -> Environment {
        let output = Vec::<u8>::new();
        let input = std::io::Cursor::new(b"testuser\n".to_vec());
        Environment::with_io(Box::new(output), Box::new(BufReader::new(input)))
    }

    #[test]
    fn test_cics_bridge_creation() {
        let bridge = CicsBridge::new("MENU", "T001");
        assert_eq!(bridge.terminal_id, "T001");
        assert_eq!(bridge.runtime.eib.transaction_id(), "MENU");
        assert!(!bridge.returned);
    }

    #[test]
    fn test_return_command() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        let options = vec![
            ("TRANSID".to_string(), Some(CobolValue::Alphanumeric("NEXT".to_string()))),
        ];

        bridge.execute("RETURN", &options, &mut env).unwrap();
        assert!(bridge.returned);
        assert_eq!(bridge.return_transid, Some("NEXT".to_string()));
        assert!(env.is_stopped());
    }

    #[test]
    fn test_xctl_command() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        let options = vec![
            ("PROGRAM".to_string(), Some(CobolValue::Alphanumeric("COSGN00C".to_string()))),
        ];

        bridge.execute("XCTL", &options, &mut env).unwrap();
        assert_eq!(bridge.xctl_program, Some("COSGN00C".to_string()));
        assert!(env.is_stopped());
    }

    #[test]
    fn test_send_map_command() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        let options = vec![
            ("MAP".to_string(), Some(CobolValue::Alphanumeric("COSGN0A".to_string()))),
            ("MAPSET".to_string(), Some(CobolValue::Alphanumeric("COSGN00".to_string()))),
            ("ERASE".to_string(), None),
        ];

        bridge.execute("SEND", &options, &mut env).unwrap();
        assert_eq!(bridge.runtime.eib.eibresp, CicsResponse::Normal as u32);
    }

    #[test]
    fn test_receive_map_command() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        env.set("WS-INPUT", CobolValue::Alphanumeric(String::new())).unwrap();

        let options = vec![
            ("MAP".to_string(), Some(CobolValue::Alphanumeric("COSGN0A".to_string()))),
            ("MAPSET".to_string(), Some(CobolValue::Alphanumeric("COSGN00".to_string()))),
            ("INTO".to_string(), Some(CobolValue::Alphanumeric("WS-INPUT".to_string()))),
        ];

        bridge.execute("RECEIVE", &options, &mut env).unwrap();
        assert_eq!(bridge.runtime.eib.eibresp, CicsResponse::Normal as u32);
    }

    #[test]
    fn test_read_file_not_found() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        let options = vec![
            ("FILE".to_string(), Some(CobolValue::Alphanumeric("CARDFILE".to_string()))),
            ("INTO".to_string(), Some(CobolValue::Alphanumeric("WS-RECORD".to_string()))),
            ("RIDFLD".to_string(), Some(CobolValue::Alphanumeric("WS-KEY".to_string()))),
            ("RESP".to_string(), Some(CobolValue::Alphanumeric("WS-RESP".to_string()))),
        ];

        bridge.execute("READ", &options, &mut env).unwrap();

        // File doesn't exist, so NOTFND
        let resp = env.get("WS-RESP").unwrap();
        assert_eq!(resp.to_display_string().trim(), "13"); // NOTFND = 13
    }

    #[test]
    fn test_assign_command() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        let options = vec![
            ("APPLID".to_string(), Some(CobolValue::Alphanumeric("WS-APPLID".to_string()))),
        ];

        bridge.execute("ASSIGN", &options, &mut env).unwrap();
        // ASSIGN sets the variable named in the value
        let applid = env.get("WS-APPLID").unwrap();
        assert_eq!(applid.to_display_string(), "CARDDEMO");
    }

    #[test]
    fn test_unknown_command() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        bridge.execute("UNKNOWN_CMD", &[], &mut env).unwrap();
        assert_eq!(bridge.runtime.eib.eibresp, CicsResponse::Normal as u32);
    }

    #[test]
    fn test_handle_abend_command() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        let options = vec![
            ("ABEND".to_string(), None),
            ("LABEL".to_string(), Some(CobolValue::Alphanumeric("ABEND-PARA".to_string()))),
        ];

        bridge.execute("HANDLE", &options, &mut env).unwrap();
        assert_eq!(bridge.runtime.eib.eibresp, CicsResponse::Normal as u32);
    }

    #[test]
    fn test_resp_variable_update() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        env.set("WS-RESP", CobolValue::from_i64(0)).unwrap();
        env.set("WS-RESP2", CobolValue::from_i64(0)).unwrap();

        let options = vec![
            ("FILE".to_string(), Some(CobolValue::Alphanumeric("NOFILE".to_string()))),
            ("INTO".to_string(), Some(CobolValue::Alphanumeric("WS-REC".to_string()))),
            ("RIDFLD".to_string(), Some(CobolValue::Alphanumeric("WS-KEY".to_string()))),
            ("RESP".to_string(), Some(CobolValue::Alphanumeric("WS-RESP".to_string()))),
            ("RESP2".to_string(), Some(CobolValue::Alphanumeric("WS-RESP2".to_string()))),
        ];

        bridge.execute("READ", &options, &mut env).unwrap();

        // RESP should be updated with NOTFND (13)
        let resp = env.get("WS-RESP").unwrap();
        assert_eq!(resp.to_display_string().trim(), "13");
    }
}
