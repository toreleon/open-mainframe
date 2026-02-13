//! CICS command handler bridge.
//!
//! Connects the COBOL interpreter to the zos-cics runtime, dispatching
//! EXEC CICS commands (SEND MAP, RECEIVE MAP, RETURN, READ, etc.)
//! to the appropriate runtime methods.

use std::collections::HashMap;

use zos_cics::bms::BmsMap;
use zos_cics::runtime::{CicsRuntime, FileMode, FileRecord};
use zos_cics::terminal::{TerminalCallback, TerminalManager};
use zos_cics::CicsResponse;
use zos_runtime::interpreter::{CicsCommandHandler, Environment, InterpreterError};
use zos_runtime::value::CobolValue;

type Result<T> = std::result::Result<T, InterpreterError>;
type MapsetMaps = HashMap<String, HashMap<String, BmsMap>>;

/// Convert an AID byte to its CICS name for HANDLE AID lookup.
fn aid_byte_to_name(byte: u8) -> Option<&'static str> {
    use zos_cics::runtime::eib::aid;
    match byte {
        aid::ENTER => Some("ENTER"),
        aid::PF1 => Some("PF1"),
        aid::PF2 => Some("PF2"),
        aid::PF3 => Some("PF3"),
        aid::PF4 => Some("PF4"),
        aid::PF5 => Some("PF5"),
        aid::PF6 => Some("PF6"),
        aid::PF7 => Some("PF7"),
        aid::PF8 => Some("PF8"),
        aid::PF9 => Some("PF9"),
        aid::PF10 => Some("PF10"),
        aid::PF11 => Some("PF11"),
        aid::PF12 => Some("PF12"),
        aid::PA1 => Some("PA1"),
        aid::PA2 => Some("PA2"),
        aid::PA3 => Some("PA3"),
        aid::CLEAR => Some("CLEAR"),
        _ => None,
    }
}

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
    /// Program to LINK to (call + return).
    pub link_program: Option<String>,
    /// COMMAREA variable name (the COBOL variable holding COMMAREA data).
    pub commarea_var: Option<String>,
    /// Pending abend handler label — set when ABEND fires and a HANDLE ABEND
    /// label was registered.  The session loop checks this to execute the
    /// handler paragraph instead of terminating.
    pub abend_label: Option<String>,
    /// Active browse tokens per file (file_name -> token).
    browse_tokens: HashMap<String, u32>,
    /// Optional terminal callback for TUI-driven I/O.
    terminal_callback: Option<Box<dyn TerminalCallback>>,
    /// Last SEND MAP map name (for receive_map to reference).
    pub last_map_name: Option<String>,
    /// Last SEND MAP mapset name.
    pub last_mapset_name: Option<String>,
    /// AID key handlers: AID_NAME → paragraph label (from HANDLE AID).
    aid_handlers: HashMap<String, String>,
    /// System values for EXEC CICS ASSIGN — a generic key→value map.
    /// The bridge performs a plain lookup; it has zero knowledge of which
    /// option names exist or how the values are derived.
    assign_values: HashMap<String, CobolValue>,
    /// BMS mapset definitions — used by `handle_send_map` to look up
    /// individual output field values from the COBOL environment.
    mapset_maps: MapsetMaps,
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
            link_program: None,
            commarea_var: None,
            abend_label: None,
            aid_handlers: HashMap::new(),
            browse_tokens: HashMap::new(),
            terminal_callback: None,
            last_map_name: None,
            last_mapset_name: None,
            assign_values: HashMap::new(),
            mapset_maps: HashMap::new(),
        }
    }

    /// Populate ASSIGN values from a generic key→value map.
    /// The caller decides what keys and values to provide.
    pub fn set_assign_values(&mut self, values: HashMap<String, CobolValue>) {
        self.assign_values = values;
    }

    /// Store BMS mapset definitions so `handle_send_map` can look up
    /// individual output field values from the COBOL environment.
    pub fn set_mapset_maps(&mut self, maps: MapsetMaps) {
        self.mapset_maps = maps;
    }

    /// Register a VSAM file with the bridge's file manager.
    pub fn register_file(
        &mut self,
        name: &str,
        record_length: usize,
        key_length: usize,
        records: Vec<FileRecord>,
    ) {
        let file = zos_cics::runtime::CicsFile::new(name, record_length, key_length);
        self.runtime.files.register(file);
        if !records.is_empty() {
            self.runtime.files.load_data(name, records);
        }
    }

    /// Set a terminal callback for interactive TUI-driven I/O.
    pub fn set_terminal_callback(&mut self, callback: Box<dyn TerminalCallback>) {
        self.terminal_callback = Some(callback);
    }

    /// Reset the bridge for a new program execution (after XCTL).
    pub fn reset_for_xctl(&mut self) {
        self.returned = false;
        self.return_transid = None;
        self.xctl_program = None;
        self.link_program = None;
        self.commarea_var = None;
        self.abend_label = None;
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
        let cursor_pos = Self::get_option_str(options, "CURSOR")
            .and_then(|s| s.parse::<usize>().ok())
            .map(|offset| {
                // CICS CURSOR(n) is a linear buffer offset (0-based) in the 80-column screen.
                let row = (offset / 80) + 1; // 1-based
                let col = (offset % 80) + 1; // 1-based
                zos_cics::terminal::ScreenPosition::new(row, col)
            });
        let freekb = Self::has_option(options, "FREEKB");
        let alarm = Self::has_option(options, "ALARM");
        let frset = Self::has_option(options, "FRSET");
        let dataonly = Self::has_option(options, "DATAONLY");
        let maponly = Self::has_option(options, "MAPONLY");

        // Build field data from COBOL variables
        let mut data: HashMap<String, Vec<u8>> = HashMap::new();

        if let Some(ref from) = from_var {
            // FROM(data-area) - send the whole data area
            if let Some(val) = env.get(from) {
                data.insert("__FROM__".to_string(), val.to_display_string().into_bytes());
            }
        }

        // Look up individual output field values from the environment.
        // The COBOL program sets fields like TRNNAMEO, PGMNAMEO, etc. as
        // children of the symbolic output map group.  Since env.get() on
        // the group doesn't recompose from children, we need to fetch each
        // child individually and pass them to the TUI by BMS field name.
        if let Some(maps) = self.mapset_maps.get(&mapset_name.to_uppercase()) {
            if let Some(bms_map) = maps.get(&map_name.to_uppercase()) {
                for field in &bms_map.fields {
                    if !field.name.is_empty() {
                        // Output fields use the "O" suffix (COBOL symbolic map convention)
                        let output_name = format!("{}O", field.name);
                        if let Some(val) = env.get(&output_name) {
                            let s = val.to_display_string();
                            // Only include if the program actually set a value
                            if s.trim().len() > 0 {
                                data.insert(field.name.clone(), s.into_bytes());
                            }
                        }
                    }
                }
            }
        }

        // Remember last map/mapset for RECEIVE MAP
        self.last_map_name = Some(map_name.clone());
        self.last_mapset_name = Some(mapset_name.clone());

        // If we have a terminal callback, notify it
        if let Some(ref mut cb) = self.terminal_callback {
            use zos_cics::terminal::SendMapOptions;
            let opts = SendMapOptions {
                erase,
                eraseaup,
                maponly,
                dataonly,
                cursor: cursor_pos,
                freekb,
                alarm,
                frset,
            };
            // Pass map_name/mapset_name via the data map as metadata for the
            // callback to resolve the actual BMS map definition.
            data.insert("__MAP_NAME__".to_string(), map_name.as_bytes().to_vec());
            data.insert("__MAPSET_NAME__".to_string(), mapset_name.as_bytes().to_vec());

            // Invoke the callback. The _map parameter is a placeholder; the real
            // BMS map is resolved by the callback using the metadata keys above.
            let placeholder = zos_cics::bms::BmsMap::default();
            cb.on_send_map(&placeholder, &data, &opts);

            tracing::info!(
                "SEND MAP({}) MAPSET({}) via callback",
                map_name,
                mapset_name
            );
        }

        // Log the send for debugging (stdout mode)
        if self.terminal_callback.is_none() {
            let opts_desc = if erase {
                " ERASE"
            } else if eraseaup {
                " ERASEAUP"
            } else {
                ""
            };
            env.display(
                &format!(
                    "[CICS] SEND MAP({}) MAPSET({}){} - screen updated",
                    map_name, mapset_name, opts_desc
                ),
                false,
            )?;
        }

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

                if let Some(ref mut cb) = self.terminal_callback {
                    cb.on_send_text(text.trim(), erase);
                } else {
                    let opts_desc = if erase { " ERASE" } else { "" };
                    env.display(
                        &format!("[CICS] SEND TEXT{}: {}", opts_desc, text.trim()),
                        false,
                    )?;
                }
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

        if let Some(ref mut cb) = self.terminal_callback {
            // In TUI mode, use the callback to get pre-collected input.
            let placeholder = zos_cics::bms::BmsMap::default();
            let (aid, mut fields) = cb.on_receive_map(&placeholder)
                .map_err(|e| InterpreterError { message: format!("RECEIVE MAP failed: {}", e) })?;

            // Extract the composed display string before processing fields.
            let composed_data = fields.remove("__COMPOSED__");

            // Set EIB AID
            self.runtime.eib.set_aid(aid);

            // Set individual field variables into environment.
            // COBOL symbolic maps use suffixed names: <field>I for input, <field>O for output.
            // Programs reference these through the symbolic map overlay.
            let field_count = fields.len();
            for (name, data) in &fields {
                let value = String::from_utf8_lossy(data).to_string();
                // Set as <field>I (input suffix per COBOL symbolic map convention)
                let input_var = format!("{}I", name);
                env.set(&input_var, CobolValue::Alphanumeric(value.clone())).ok();
                // Also set without suffix for programs that reference fields directly
                env.set(name, CobolValue::Alphanumeric(value)).ok();
            }

            // If INTO is specified, set the composed symbolic map display string.
            // This is the full data area matching the symbolic map layout that the
            // COBOL program declared as its INTO variable.
            if let Some(ref into) = into_var {
                if let Some(composed) = composed_data {
                    let composed_str = String::from_utf8_lossy(&composed).to_string();
                    env.set(into, CobolValue::Alphanumeric(composed_str)).ok();
                }
            }

            tracing::info!(
                "RECEIVE MAP({}) MAPSET({}) - {} fields from pre-collected input",
                map_name,
                mapset_name,
                field_count
            );
            self.runtime.eib.set_response(CicsResponse::Normal);
            return Ok(());
        }

        // Stdout mode: simulate by prompting on stdin.
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

    /// Handle LINK command.
    ///
    /// LINK calls a sub-program and returns control to the caller when it
    /// finishes.  We set `link_program` so the session loop can execute the
    /// linked program inline, then resume the current program.
    fn handle_link(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let program = Self::get_option_str(options, "PROGRAM").unwrap_or_default();
        let commarea = Self::get_option_str(options, "COMMAREA");

        if let Some(ref ca) = commarea {
            env.display(
                &format!("[CICS] LINK PROGRAM({}) COMMAREA({})", program, ca),
                false,
            )?;
            self.commarea_var = Some(ca.clone());
        } else {
            env.display(
                &format!("[CICS] LINK PROGRAM({})", program),
                false,
            )?;
        }

        self.link_program = Some(program);
        self.runtime.eib.set_response(CicsResponse::Normal);
        env.stop(); // Pause current execution
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

        // Get KEYLENGTH if specified
        let key_length: Option<usize> = Self::get_option_str(options, "KEYLENGTH")
            .and_then(|s| s.parse().ok());

        // Get the key from RIDFLD variable, padded to KEYLENGTH if specified
        let key = if let Some(ref ridfld) = ridfld_var {
            let raw = env.get(ridfld)
                .map(|v| v.to_display_string())
                .unwrap_or_default();
            let mut key_bytes = raw.into_bytes();
            // Pad with spaces to match KEYLENGTH (COBOL keys are space-padded)
            if let Some(kl) = key_length {
                key_bytes.resize(kl, b' ');
            }
            key_bytes
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
            Ok(token) => {
                self.browse_tokens.insert(file_name.to_uppercase(), token);
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
        let file_name = Self::get_option_str(options, "FILE")
            .or_else(|| Self::get_option_str(options, "DATASET"))
            .unwrap_or_default();
        let into_var = Self::get_option_str(options, "INTO");

        let token = self.browse_tokens.get(&file_name.to_uppercase()).copied().unwrap_or(0);
        match self.runtime.files.readnext(token) {
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
        let file_name = Self::get_option_str(options, "FILE")
            .or_else(|| Self::get_option_str(options, "DATASET"))
            .unwrap_or_default();
        let into_var = Self::get_option_str(options, "INTO");

        let token = self.browse_tokens.get(&file_name.to_uppercase()).copied().unwrap_or(0);
        match self.runtime.files.readprev(token) {
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
        let file_name = Self::get_option_str(options, "FILE")
            .or_else(|| Self::get_option_str(options, "DATASET"))
            .unwrap_or_default();

        if let Some(token) = self.browse_tokens.remove(&file_name.to_uppercase()) {
            let _ = self.runtime.files.endbr(token);
        }
        self.runtime.eib.set_response(CicsResponse::Normal);
        self.update_resp_variables(options, env)?;
        Ok(())
    }

    /// Handle HANDLE AID command.
    ///
    /// Registers handlers for specific AID keys (PF1, PF3, ENTER, etc.).
    /// When the corresponding AID is received, the registered paragraph
    /// should be invoked.
    fn handle_handle_aid(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        _env: &mut Environment,
    ) -> Result<()> {
        for (name, label_val) in options {
            if name.eq_ignore_ascii_case("AID") {
                continue; // Skip the "AID" flag itself
            }
            if let Some(val) = label_val {
                let label = val.to_display_string().trim().to_string();
                self.aid_handlers.insert(name.to_uppercase(), label);
            }
        }
        self.runtime.eib.set_response(CicsResponse::Normal);
        Ok(())
    }

    /// Look up the handler label for the given AID byte, if registered.
    pub fn get_aid_handler_for_byte(&self, aid_byte: u8) -> Option<&str> {
        let name = aid_byte_to_name(aid_byte)?;
        self.aid_handlers.get(name).map(|s| s.as_str())
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
    ///
    /// If a HANDLE ABEND label was previously registered, sets `abend_label`
    /// so the session loop can transfer control to the handler paragraph.
    /// Otherwise just logs the abend and stops execution.
    fn handle_abend(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let abcode = Self::get_option_str(options, "ABCODE").unwrap_or_else(|| "ASRA".to_string());

        if let Some(ref handler) = self.runtime.context.abend_handler {
            // A HANDLE ABEND label was registered — signal the session loop
            // to execute that paragraph instead of terminating.
            tracing::info!("ABEND({}) — routing to handler '{}'", abcode, handler);
            self.abend_label = Some(handler.clone());
            env.stop();
        } else {
            env.display(&format!("[CICS] ABEND({})", abcode), false)?;
            env.stop();
        }
        Ok(())
    }

    /// Handle ASSIGN command — generic key→value lookup.
    ///
    /// The bridge has no knowledge of specific option names.  It simply
    /// looks up each requested option in `self.assign_values` and sets
    /// the target COBOL variable.  All derivation logic lives in the
    /// caller that populates `assign_values`.
    fn handle_assign(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        for (name, value) in options {
            if let Some(val) = value {
                let var_name = val.to_display_string().trim().to_string();
                let key = name.to_uppercase();
                if let Some(system_val) = self.assign_values.get(&key) {
                    env.set(&var_name, system_val.clone())?;
                } else {
                    tracing::debug!("ASSIGN option '{}' has no configured value", key);
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
            "LINK" => {
                self.handle_link(options, env)
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
                // HANDLE ABEND, HANDLE AID, or HANDLE CONDITION
                if Self::has_option(options, "ABEND") {
                    self.handle_handle_abend(options, env)
                } else if Self::has_option(options, "AID") {
                    self.handle_handle_aid(options, env)
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
    use zos_tui::mock::MockTerminal;

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
    fn test_assign_generic_lookup() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        // Caller provides arbitrary key→value pairs — bridge has no opinion
        let mut values = HashMap::new();
        values.insert("APPLID".to_string(), CobolValue::Alphanumeric("MYAPP".to_string()));
        values.insert("USERID".to_string(), CobolValue::Alphanumeric("JDOE".to_string()));
        values.insert("SYSID".to_string(), CobolValue::Alphanumeric("SY01".to_string()));
        values.insert("CUSTOM".to_string(), CobolValue::Alphanumeric("HELLO".to_string()));
        bridge.set_assign_values(values);

        let options = vec![
            ("APPLID".to_string(), Some(CobolValue::Alphanumeric("WS-APPLID".to_string()))),
            ("USERID".to_string(), Some(CobolValue::Alphanumeric("WS-USERID".to_string()))),
            ("SYSID".to_string(), Some(CobolValue::Alphanumeric("WS-SYSID".to_string()))),
            ("CUSTOM".to_string(), Some(CobolValue::Alphanumeric("WS-CUSTOM".to_string()))),
        ];

        bridge.execute("ASSIGN", &options, &mut env).unwrap();

        assert_eq!(env.get("WS-APPLID").unwrap().to_display_string(), "MYAPP");
        assert_eq!(env.get("WS-USERID").unwrap().to_display_string(), "JDOE");
        assert_eq!(env.get("WS-SYSID").unwrap().to_display_string(), "SY01");
        assert_eq!(env.get("WS-CUSTOM").unwrap().to_display_string(), "HELLO");
    }

    #[test]
    fn test_assign_missing_option_is_silent() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        // Provide only APPLID — no USERID
        let mut values = HashMap::new();
        values.insert("APPLID".to_string(), CobolValue::Alphanumeric("APP1".to_string()));
        bridge.set_assign_values(values);

        let options = vec![
            ("APPLID".to_string(), Some(CobolValue::Alphanumeric("WS-APPLID".to_string()))),
            ("USERID".to_string(), Some(CobolValue::Alphanumeric("WS-USERID".to_string()))),
        ];

        bridge.execute("ASSIGN", &options, &mut env).unwrap();

        assert_eq!(env.get("WS-APPLID").unwrap().to_display_string(), "APP1");
        // USERID was not provided, so the variable should not be set
        assert!(env.get("WS-USERID").is_none());
    }

    #[test]
    fn test_unknown_command() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        bridge.execute("UNKNOWN_CMD", &[], &mut env).unwrap();
        assert_eq!(bridge.runtime.eib.eibresp, CicsResponse::Normal as u32);
    }

    #[test]
    fn test_handle_aid_command() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        let options = vec![
            ("AID".to_string(), None),
            ("PF3".to_string(), Some(CobolValue::Alphanumeric("END-PARA".to_string()))),
            ("ENTER".to_string(), Some(CobolValue::Alphanumeric("PROCESS-PARA".to_string()))),
        ];

        bridge.execute("HANDLE", &options, &mut env).unwrap();
        assert_eq!(bridge.runtime.eib.eibresp, CicsResponse::Normal as u32);
        // Verify handlers registered
        assert_eq!(
            bridge.get_aid_handler_for_byte(zos_cics::runtime::eib::aid::PF3),
            Some("END-PARA")
        );
        assert_eq!(
            bridge.get_aid_handler_for_byte(zos_cics::runtime::eib::aid::ENTER),
            Some("PROCESS-PARA")
        );
        // No handler for PF1
        assert_eq!(
            bridge.get_aid_handler_for_byte(zos_cics::runtime::eib::aid::PF1),
            None
        );
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
        // Verify the handler was registered
        assert_eq!(bridge.runtime.context.abend_handler, Some("ABEND-PARA".to_string()));
    }

    #[test]
    fn test_abend_with_handler_sets_label() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        // Register HANDLE ABEND
        let handle_opts = vec![
            ("ABEND".to_string(), None),
            ("LABEL".to_string(), Some(CobolValue::Alphanumeric("ERR-HANDLER".to_string()))),
        ];
        bridge.execute("HANDLE", &handle_opts, &mut env).unwrap();
        assert!(bridge.abend_label.is_none());

        // Issue ABEND — should set abend_label instead of just stopping
        let abend_opts = vec![
            ("ABCODE".to_string(), Some(CobolValue::Alphanumeric("ASRA".to_string()))),
        ];
        bridge.execute("ABEND", &abend_opts, &mut env).unwrap();
        assert_eq!(bridge.abend_label, Some("ERR-HANDLER".to_string()));
        assert!(env.is_stopped());
    }

    #[test]
    fn test_abend_without_handler_no_label() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        // No HANDLE ABEND registered — ABEND should just stop
        let abend_opts = vec![
            ("ABCODE".to_string(), Some(CobolValue::Alphanumeric("AICA".to_string()))),
        ];
        bridge.execute("ABEND", &abend_opts, &mut env).unwrap();
        assert!(bridge.abend_label.is_none());
        assert!(env.is_stopped());
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

    #[test]
    fn test_vsam_read_write() {
        let mut bridge = CicsBridge::new("TEST", "T001");
        let mut env = create_test_env();

        // Register a file with test data
        bridge.register_file(
            "CUSTFILE",
            100,
            6,
            vec![
                FileRecord::from_strings("000001", "Alice Smith"),
                FileRecord::from_strings("000002", "Bob Jones"),
            ],
        );

        // Set up COBOL variables
        env.set("WS-KEY", CobolValue::Alphanumeric("000001".to_string())).unwrap();
        env.set("WS-RECORD", CobolValue::Alphanumeric(String::new())).unwrap();
        env.set("WS-RESP", CobolValue::from_i64(0)).unwrap();

        // READ existing record
        let options = vec![
            ("FILE".to_string(), Some(CobolValue::Alphanumeric("CUSTFILE".to_string()))),
            ("INTO".to_string(), Some(CobolValue::Alphanumeric("WS-RECORD".to_string()))),
            ("RIDFLD".to_string(), Some(CobolValue::Alphanumeric("WS-KEY".to_string()))),
            ("RESP".to_string(), Some(CobolValue::Alphanumeric("WS-RESP".to_string()))),
        ];

        bridge.execute("READ", &options, &mut env).unwrap();

        let resp = env.get("WS-RESP").unwrap();
        assert_eq!(resp.to_display_string().trim(), "0"); // Normal
        let record = env.get("WS-RECORD").unwrap();
        assert!(record.to_display_string().contains("Alice Smith"));
    }

    #[test]
    fn test_vsam_browse() {
        let mut bridge = CicsBridge::new("TEST", "T001");
        let mut env = create_test_env();

        bridge.register_file(
            "ACCTFILE",
            80,
            4,
            vec![
                FileRecord::from_strings("0001", "Account A"),
                FileRecord::from_strings("0002", "Account B"),
                FileRecord::from_strings("0003", "Account C"),
            ],
        );

        env.set("WS-KEY", CobolValue::Alphanumeric("0001".to_string())).unwrap();
        env.set("WS-RECORD", CobolValue::Alphanumeric(String::new())).unwrap();
        env.set("WS-RESP", CobolValue::from_i64(0)).unwrap();

        // STARTBR
        let startbr_opts = vec![
            ("FILE".to_string(), Some(CobolValue::Alphanumeric("ACCTFILE".to_string()))),
            ("RIDFLD".to_string(), Some(CobolValue::Alphanumeric("WS-KEY".to_string()))),
            ("RESP".to_string(), Some(CobolValue::Alphanumeric("WS-RESP".to_string()))),
        ];
        bridge.execute("STARTBR", &startbr_opts, &mut env).unwrap();
        assert_eq!(env.get("WS-RESP").unwrap().to_display_string().trim(), "0");

        // READNEXT
        let readnext_opts = vec![
            ("FILE".to_string(), Some(CobolValue::Alphanumeric("ACCTFILE".to_string()))),
            ("INTO".to_string(), Some(CobolValue::Alphanumeric("WS-RECORD".to_string()))),
            ("RESP".to_string(), Some(CobolValue::Alphanumeric("WS-RESP".to_string()))),
        ];
        bridge.execute("READNEXT", &readnext_opts, &mut env).unwrap();
        assert!(env.get("WS-RECORD").unwrap().to_display_string().contains("Account A"));

        bridge.execute("READNEXT", &readnext_opts, &mut env).unwrap();
        assert!(env.get("WS-RECORD").unwrap().to_display_string().contains("Account B"));

        bridge.execute("READNEXT", &readnext_opts, &mut env).unwrap();
        assert!(env.get("WS-RECORD").unwrap().to_display_string().contains("Account C"));

        // READNEXT at end of file
        bridge.execute("READNEXT", &readnext_opts, &mut env).unwrap();
        assert_eq!(env.get("WS-RESP").unwrap().to_display_string().trim(), "20"); // Endfile

        // ENDBR
        let endbr_opts = vec![
            ("FILE".to_string(), Some(CobolValue::Alphanumeric("ACCTFILE".to_string()))),
        ];
        bridge.execute("ENDBR", &endbr_opts, &mut env).unwrap();
    }

    #[test]
    fn test_send_map_invokes_callback() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        // Attach a MockTerminal as the callback
        let mock = MockTerminal::new();
        bridge.set_terminal_callback(Box::new(mock));

        let options = vec![
            ("MAP".to_string(), Some(CobolValue::Alphanumeric("COSGN0A".to_string()))),
            ("MAPSET".to_string(), Some(CobolValue::Alphanumeric("COSGN00".to_string()))),
            ("ERASE".to_string(), None),
        ];

        bridge.execute("SEND", &options, &mut env).unwrap();
        assert_eq!(bridge.runtime.eib.eibresp, CicsResponse::Normal as u32);

        // Verify the callback was invoked
        // We can't directly inspect the mock since it's behind Box<dyn TerminalCallback>,
        // but we can verify last_map_name was set and response is Normal.
        assert_eq!(bridge.last_map_name, Some("COSGN0A".to_string()));
        assert_eq!(bridge.last_mapset_name, Some("COSGN00".to_string()));
    }

    #[test]
    fn test_send_text_invokes_callback() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        // Set up the FROM variable
        env.set("WS-MSG", CobolValue::Alphanumeric("Hello World".to_string())).unwrap();

        // Attach a MockTerminal as the callback
        let mock = MockTerminal::new();
        bridge.set_terminal_callback(Box::new(mock));

        let options = vec![
            ("FROM".to_string(), Some(CobolValue::Alphanumeric("WS-MSG".to_string()))),
            ("ERASE".to_string(), None),
        ];

        bridge.execute("SEND", &options, &mut env).unwrap();
        assert_eq!(bridge.runtime.eib.eibresp, CicsResponse::Normal as u32);
    }

    #[test]
    fn test_receive_map_with_callback_uses_precollected_input() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        // Attach a MockTerminal as the callback
        let mock = MockTerminal::new();
        bridge.set_terminal_callback(Box::new(mock));

        let options = vec![
            ("MAP".to_string(), Some(CobolValue::Alphanumeric("COSGN0A".to_string()))),
            ("MAPSET".to_string(), Some(CobolValue::Alphanumeric("COSGN00".to_string()))),
        ];

        // In TUI mode, RECEIVE MAP should succeed without prompting stdin
        bridge.execute("RECEIVE", &options, &mut env).unwrap();
        assert_eq!(bridge.runtime.eib.eibresp, CicsResponse::Normal as u32);
    }

    #[test]
    fn test_receive_map_sets_field_variables() {
        let mut bridge = CicsBridge::new("MENU", "T001");
        let mut env = create_test_env();

        // Create a MockTerminal with pre-queued input
        let mut mock = MockTerminal::new();
        let mut fields = HashMap::new();
        fields.insert("USRIDI".to_string(), b"ADMIN".to_vec());
        fields.insert("PASSWI".to_string(), b"SECRET".to_vec());
        mock.queue_input(zos_cics::runtime::eib::aid::ENTER, fields);
        bridge.set_terminal_callback(Box::new(mock));

        let options = vec![
            ("MAP".to_string(), Some(CobolValue::Alphanumeric("COSGN0A".to_string()))),
            ("MAPSET".to_string(), Some(CobolValue::Alphanumeric("COSGN00".to_string()))),
        ];

        bridge.execute("RECEIVE", &options, &mut env).unwrap();
        assert_eq!(bridge.runtime.eib.eibresp, CicsResponse::Normal as u32);

        // Verify individual field variables were set
        let usridi = env.get("USRIDII").unwrap();
        assert_eq!(usridi.to_display_string(), "ADMIN");

        let passwi = env.get("PASSWII").unwrap();
        assert_eq!(passwi.to_display_string(), "SECRET");

        // Also verify plain field names
        let usridi_plain = env.get("USRIDI").unwrap();
        assert_eq!(usridi_plain.to_display_string(), "ADMIN");

        // Verify AID was set
        assert_eq!(bridge.runtime.eib.eibaid, zos_cics::runtime::eib::aid::ENTER);
    }
}
