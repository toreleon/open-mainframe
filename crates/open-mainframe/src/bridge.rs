//! CICS-COBOL bridge.
//!
//! Connects the COBOL interpreter to the CICS runtime and TUI session.
//! When the interpreter encounters an `EXEC CICS` command (lowered to
//! `SimpleStatement::ExecCics`), it calls
//! `cics_handler.execute(command, options, env)` which dispatches into
//! this bridge.
//!
//! # Architecture
//!
//! ```text
//! COBOL Interpreter
//!       │  (ExecCics)
//!       ▼
//!   CicsBridge  ─────── CicsRuntime (EIB, FileManager, etc.)
//!       │
//!       ├──► SEND MAP   → BMS map → TUI Session → render to terminal
//!       ├──► RECEIVE MAP → TUI Session → wait_for_input → set COBOL vars
//!       ├──► READ/WRITE  → CicsDispatcher → FileManager
//!       ├──► XCTL        → set pending Xctl action, stop interpreter
//!       └──► RETURN      → set pending Return action, stop interpreter
//! ```

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use tracing::{debug, info, warn};

use open_mainframe_cics::bms::{BmsMap, BmsMapset, BmsParser};
use open_mainframe_cics::runtime::{
    CicsDispatcher, CicsFile, CicsRuntime, CommandParamBlock, DispatchResult,
    FileMode, FileRecord, ProgramRegistry,
};
use open_mainframe_cics::terminal::{ScreenPosition, SendMapOptions};
use open_mainframe_cics::CicsResponse;

use open_mainframe_runtime::interpreter::{CicsCommandHandler, Environment, InterpreterError};
use open_mainframe_runtime::value::CobolValue;

use open_mainframe_tui::session::Session;

/// Result type alias using the interpreter error.
type Result<T> = std::result::Result<T, InterpreterError>;

// ---------------------------------------------------------------------------
// BridgeAction — deferred actions that the outer session loop inspects after
// the interpreter returns.
// ---------------------------------------------------------------------------

/// Action recorded by the bridge that the outer session loop must handle
/// after the COBOL interpreter finishes its current program run.
#[derive(Debug, Clone)]
pub enum BridgeAction {
    /// Transfer control to another program (EXEC CICS XCTL).
    Xctl {
        program: String,
        commarea: Option<Vec<u8>>,
    },
    /// Return to the CICS session (EXEC CICS RETURN).
    Return {
        transid: Option<String>,
        commarea: Option<Vec<u8>>,
    },
    /// ABEND — abnormal end.
    Abend {
        code: String,
    },
}

// ---------------------------------------------------------------------------
// CicsBridge
// ---------------------------------------------------------------------------

/// The CICS-COBOL bridge.
///
/// Implements `CicsCommandHandler` so it can be installed in the COBOL
/// interpreter environment.  Each EXEC CICS command flows through
/// `execute()`, which dispatches to the appropriate handler method.
pub struct CicsBridge {
    /// CICS runtime (EIB, file manager, transaction context, etc.).
    runtime: CicsRuntime,
    /// Program registry for LINK/XCTL target lookup.
    registry: ProgramRegistry,
    /// BMS maps loaded from the BMS directory, keyed by uppercase map name.
    bms_maps: HashMap<String, BmsMap>,
    /// BMS mapsets loaded, keyed by uppercase mapset name.
    bms_mapsets: HashMap<String, BmsMapset>,
    /// Pending action set by XCTL/RETURN/ABEND — the outer session loop
    /// inspects this after the interpreter exits.
    pending_action: Option<BridgeAction>,
    /// TUI session (shared with the outer session loop via Rc<RefCell<>>).
    session: Rc<RefCell<Session>>,
    /// Active browse tokens: maps a CICS file name to its browse token so
    /// that READNEXT/ENDBR can reference the correct browse session.
    browse_tokens: HashMap<String, u32>,
    /// Pending terminal input from the pseudo-conversational wait.
    /// Set by the main loop after `wait_for_input()`, consumed by RECEIVE MAP.
    pending_input: Option<(u8, HashMap<String, Vec<u8>>)>,
}

impl CicsBridge {
    /// Create a new CICS bridge with a shared session handle.
    ///
    /// The `session` is `Rc<RefCell<..>>` because the outer session loop
    /// also needs access to it for rendering and cleanup.  Terminal I/O
    /// (wait_for_input) is handled by the main loop, not the bridge.
    pub fn new(
        session: Rc<RefCell<Session>>,
    ) -> Self {
        Self {
            runtime: CicsRuntime::new("DFLT"),
            registry: ProgramRegistry::new(),
            bms_maps: HashMap::new(),
            bms_mapsets: HashMap::new(),
            pending_action: None,
            session,
            browse_tokens: HashMap::new(),
            pending_input: None,
        }
    }

    // -- BMS map loading ----------------------------------------------------

    /// Scan `bms_dir` for `.bms` files and parse each one.
    ///
    /// Every map found within each mapset is stored in `self.bms_maps` keyed
    /// by its uppercase name.  Parsing errors are logged but do not abort
    /// loading — other maps are still loaded.
    pub fn load_bms_maps(&mut self, bms_dir: &Path, _include_paths: &[PathBuf]) {
        let entries = match std::fs::read_dir(bms_dir) {
            Ok(entries) => entries,
            Err(e) => {
                warn!("Cannot read BMS directory {}: {}", bms_dir.display(), e);
                return;
            }
        };

        for entry in entries.flatten() {
            let path = entry.path();
            let ext = path
                .extension()
                .and_then(|e| e.to_str())
                .unwrap_or("")
                .to_lowercase();
            if ext != "bms" {
                continue;
            }

            let source = match std::fs::read_to_string(&path) {
                Ok(s) => s,
                Err(e) => {
                    warn!("Cannot read BMS file {}: {}", path.display(), e);
                    continue;
                }
            };

            let mut parser = BmsParser::new();
            match parser.parse(&source) {
                Ok(mapset) => {
                    info!(
                        "Loaded BMS mapset '{}' with {} map(s) from {}",
                        mapset.name,
                        mapset.maps.len(),
                        path.display()
                    );
                    // Store each map, keyed by its own name.
                    for map in &mapset.maps {
                        self.bms_maps.insert(map.name.to_uppercase(), map.clone());
                    }
                    // Also store the mapset so we can look up maps by mapset name.
                    self.bms_mapsets
                        .insert(mapset.name.to_uppercase(), mapset);
                }
                Err(e) => {
                    warn!("Failed to parse BMS file {}: {}", path.display(), e);
                }
            }
        }

        info!("Loaded {} BMS map(s) total", self.bms_maps.len());
    }

    // -- Data file loading --------------------------------------------------

    /// Parse data-file specifications and register them with the FileManager.
    ///
    /// The format for each entry is:
    ///
    /// ```text
    /// DDNAME=path[:key_len[:rec_len]]
    /// ```
    ///
    /// For example:
    /// - `CUSTFILE=/data/cust.dat:16:80`
    /// - `ACCTFILE=/data/acct.dat:11:300`
    ///
    /// If `key_len` or `rec_len` are omitted, sensible defaults are used
    /// (key_len=0, rec_len=80).
    pub fn load_data_files(&mut self, data_files: &[String]) {
        for spec in data_files {
            // Split on '=' to get DDNAME and the rest
            let (ddname, rest) = match spec.split_once('=') {
                Some(parts) => parts,
                None => {
                    warn!("Invalid data file spec (no '='): {}", spec);
                    continue;
                }
            };

            let ddname = ddname.trim().to_uppercase();

            // The rest is path[:key_len[:rec_len]]
            let parts: Vec<&str> = rest.splitn(3, ':').collect();
            let file_path = parts[0].trim();
            let key_len: usize = parts
                .get(1)
                .and_then(|s| s.trim().parse().ok())
                .unwrap_or(0);
            let rec_len: usize = parts
                .get(2)
                .and_then(|s| s.trim().parse().ok())
                .unwrap_or(80);

            // Register the CicsFile definition
            let cics_file = CicsFile::new(&ddname, rec_len, key_len);
            self.runtime.files.register(cics_file);

            // If the file path exists on disk, load records from it
            let path = Path::new(file_path);
            if path.exists() {
                match self.load_records_from_file(path, key_len, rec_len) {
                    Ok(records) => {
                        info!(
                            "Loaded {} record(s) for {} from {}",
                            records.len(),
                            ddname,
                            file_path
                        );
                        self.runtime.files.load_data(&ddname, records);
                    }
                    Err(e) => {
                        warn!(
                            "Failed to load data for {} from {}: {}",
                            ddname, file_path, e
                        );
                    }
                }
            } else {
                debug!(
                    "Registered file {} (no data file at {})",
                    ddname, file_path
                );
            }
        }
    }

    /// Read records from a flat file and split into key + data.
    ///
    /// If the file contains newlines, records are split on line boundaries
    /// and each line is right-padded with spaces to `rec_len`.  Otherwise
    /// the file is split into fixed-length `rec_len` chunks.
    fn load_records_from_file(
        &self,
        path: &Path,
        key_len: usize,
        rec_len: usize,
    ) -> std::io::Result<Vec<FileRecord>> {
        let content = std::fs::read(path)?;
        let mut records = Vec::new();

        if rec_len == 0 {
            return Ok(records);
        }

        // Detect text files: if the content contains newlines and no line
        // is longer than rec_len, use line-based splitting.
        let has_newlines = content.contains(&b'\n');

        if has_newlines {
            // Line-based splitting for text data files.
            for line in content.split(|&b| b == b'\n') {
                if line.is_empty() {
                    continue;
                }
                // Strip trailing CR for Windows line endings
                let line = if line.last() == Some(&b'\r') {
                    &line[..line.len() - 1]
                } else {
                    line
                };
                if line.is_empty() {
                    continue;
                }
                // Pad to rec_len with spaces
                let mut data = line.to_vec();
                if data.len() < rec_len {
                    data.resize(rec_len, b' ');
                }
                let key = if key_len > 0 && data.len() >= key_len {
                    data[..key_len].to_vec()
                } else {
                    Vec::new()
                };
                records.push(FileRecord::new(key, data));
            }
        } else {
            // Fixed-length chunking for binary data files.
            for chunk in content.chunks(rec_len) {
                if chunk.is_empty() {
                    continue;
                }
                let key = if key_len > 0 && chunk.len() >= key_len {
                    chunk[..key_len].to_vec()
                } else {
                    Vec::new()
                };
                let data = chunk.to_vec();
                records.push(FileRecord::new(key, data));
            }
        }

        Ok(records)
    }

    // -- Pending action accessors ------------------------------------------

    /// Inspect the pending action without consuming it.
    pub fn pending_action(&self) -> Option<&BridgeAction> {
        self.pending_action.as_ref()
    }

    /// Take (consume) the pending action.
    pub fn take_pending_action(&mut self) -> Option<BridgeAction> {
        self.pending_action.take()
    }

    /// Store terminal input from the pseudo-conversational wait so that
    /// the next RECEIVE MAP can use it instead of waiting again.
    pub fn set_pending_input(&mut self, aid: u8, fields: HashMap<String, Vec<u8>>) {
        self.pending_input = Some((aid, fields));
    }

    // -- Runtime / registry accessors --------------------------------------

    /// Borrow the CICS runtime immutably.
    pub fn runtime(&self) -> &CicsRuntime {
        &self.runtime
    }

    /// Borrow the CICS runtime mutably.
    pub fn runtime_mut(&mut self) -> &mut CicsRuntime {
        &mut self.runtime
    }

    /// Borrow the program registry immutably.
    pub fn registry(&self) -> &ProgramRegistry {
        &self.registry
    }

    /// Borrow the program registry mutably.
    pub fn registry_mut(&mut self) -> &mut ProgramRegistry {
        &mut self.registry
    }

    // ======================================================================
    // Private handler implementations for each EXEC CICS command
    // ======================================================================

    // -- SEND MAP -----------------------------------------------------------

    fn handle_send_map(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        // Extract MAP name
        let map_name = self
            .find_option_value(options, "MAP")
            .unwrap_or_default()
            .to_uppercase();

        // Extract MAPSET name (falls back to map name if not provided)
        let mapset_name = self
            .find_option_value(options, "MAPSET")
            .unwrap_or_else(|| map_name.clone())
            .to_uppercase();

        // Resolve the BMS map: try by map name first, then by mapset lookup
        let bms_map = self
            .bms_maps
            .get(&map_name)
            .or_else(|| {
                self.bms_mapsets
                    .get(&mapset_name)
                    .and_then(|ms| ms.get_map(&map_name))
            })
            .cloned();

        let bms_map = match bms_map {
            Some(m) => m,
            None => {
                warn!(
                    "BMS map '{}' (mapset '{}') not found — sending empty screen",
                    map_name, mapset_name
                );
                // MAPFAIL condition — set EIBRESP and continue
                self.runtime.eib.set_response(
                    CicsResponse::Mapfail,
                );
                return Ok(());
            }
        };

        // Build field data from COBOL variables.
        // The COBOL program sets fields in its symbolic map copy; these appear
        // as option values.  We also look up field names in the COBOL env
        // using the BMS field naming convention (e.g., FIELD-NAME + "O").
        let field_data = self.collect_send_map_data(options, env, &bms_map);

        // Build SendMapOptions from the EXEC CICS options
        let send_opts = self.build_send_map_options(options);

        // Push the map to the TUI session
        self.session
            .borrow_mut()
            .on_send_map(&bms_map, &field_data, &send_opts);

        // Update EIB
        self.runtime
            .eib
            .set_response(CicsResponse::Normal);

        Ok(())
    }

    /// Collect field data for SEND MAP from COBOL environment.
    fn collect_send_map_data(
        &self,
        options: &[(String, Option<CobolValue>)],
        env: &Environment,
        bms_map: &BmsMap,
    ) -> HashMap<String, Vec<u8>> {
        let mut data = HashMap::new();

        // Check for FROM option — a single data area holding the entire
        // symbolic map output buffer.
        if let Some(from_value) = self.find_option_cobol_value(options, "FROM") {
            let from_str = from_value.to_display_string();
            // If FROM is specified, we distribute bytes to each field based on
            // their offsets.  For simplicity, also try to resolve named fields.
            data.insert("__FROM__".to_string(), from_str.into_bytes());
        }

        // For each field in the BMS map, try to find the corresponding COBOL
        // variable.  The naming convention is <FIELD>O for output data.
        for field in &bms_map.fields {
            if field.name.is_empty() {
                continue;
            }
            let field_upper = field.name.to_uppercase();

            // Try the output suffix convention: FIELDO
            let output_var = format!("{}O", field_upper);
            if let Some(val) = env.get(&output_var) {
                let s = val.to_display_string();
                data.insert(field_upper.clone(), s.into_bytes());
                continue;
            }

            // Try the field name directly (some programs use plain names)
            if let Some(val) = env.get(&field_upper) {
                let s = val.to_display_string();
                data.insert(field_upper.clone(), s.into_bytes());
                continue;
            }

            // Try hyphenated versions with O suffix (FIELD-NAMEO)
            let hyphen_output = format!("{}-O", field_upper);
            if let Some(val) = env.get(&hyphen_output) {
                let s = val.to_display_string();
                data.insert(field_upper.clone(), s.into_bytes());
            }
        }

        // Also pick up explicit option values that match field names
        for (name, value) in options {
            let name_upper = name.to_uppercase();
            // Skip control options
            if matches!(
                name_upper.as_str(),
                "MAP" | "MAPSET"
                    | "FROM"
                    | "ERASE"
                    | "ERASEAUP"
                    | "MAPONLY"
                    | "DATAONLY"
                    | "FREEKB"
                    | "ALARM"
                    | "FRSET"
                    | "CURSOR"
                    | "ACCUM"
            ) {
                continue;
            }
            if let Some(val) = value {
                let s = val.to_display_string();
                data.insert(name_upper, s.into_bytes());
            }
        }

        data
    }

    /// Build `SendMapOptions` from the EXEC CICS option list.
    fn build_send_map_options(&self, options: &[(String, Option<CobolValue>)]) -> SendMapOptions {
        let mut opts = SendMapOptions::default();

        for (name, value) in options {
            match name.to_uppercase().as_str() {
                "ERASE" => opts.erase = true,
                "ERASEAUP" => opts.eraseaup = true,
                "MAPONLY" => opts.maponly = true,
                "DATAONLY" => opts.dataonly = true,
                "FREEKB" => opts.freekb = true,
                "ALARM" => opts.alarm = true,
                "FRSET" => opts.frset = true,
                "ACCUM" => opts.accum = true,
                "CURSOR" => {
                    // CURSOR with no value means position at the IC field.
                    // CURSOR(nnn) means absolute buffer position.
                    if let Some(val) = value {
                        let pos_str = val.to_display_string();
                        if let Ok(pos) = pos_str.trim().parse::<usize>() {
                            let cols = 80usize; // default
                            let row = pos / cols + 1;
                            let col = pos % cols + 1;
                            opts.cursor = Some(
                                ScreenPosition::new(row, col),
                            );
                        }
                    }
                }
                _ => {}
            }
        }

        opts
    }

    // -- SEND TEXT -----------------------------------------------------------

    fn handle_send_text(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        _env: &mut Environment,
    ) -> Result<()> {
        let text = self
            .find_option_value(options, "TEXT")
            .or_else(|| self.find_option_value(options, "FROM"))
            .unwrap_or_default();

        let erase = self.has_option(options, "ERASE");

        self.session.borrow_mut().on_send_text(&text, erase);
        self.runtime
            .eib
            .set_response(CicsResponse::Normal);

        Ok(())
    }

    // -- RECEIVE MAP --------------------------------------------------------

    fn handle_receive_map(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        // Get the map name so we know what fields to expect
        let map_name = self
            .find_option_value(options, "MAP")
            .unwrap_or_default()
            .to_uppercase();

        // Use pending input from the pseudo-conversational wait.
        // The main loop is responsible for calling wait_for_input (TUI mode)
        // or read_headless_input (headless mode) and storing the result via
        // set_pending_input() before the program issues RECEIVE MAP.
        let (aid, fields) = match self.pending_input.take() {
            Some(input) => input,
            None => {
                return Err(InterpreterError {
                    message: "RECEIVE MAP: no pending input — pseudo-conversational wait required".to_string(),
                });
            }
        };

        // Set EIBAID in the runtime
        self.runtime.eib.set_aid(aid);

        // Also set EIBAID in the COBOL environment so the program can
        // inspect DFHEIBLK directly.
        let _ = env.set("EIBAID", CobolValue::Alphanumeric(
            String::from(aid as char),
        ));
        let _ = env.set(
            "EIBRESP",
            CobolValue::from_i64(
                CicsResponse::Normal as i64,
            ),
        );

        // Set field values back into COBOL variables.
        // The naming convention is <FIELD>I for input data.
        for (field_name, field_data) in &fields {
            let field_upper = field_name.to_uppercase();

            // Set the input suffix variable (FIELDI)
            let input_var = format!("{}I", field_upper);
            let value_str = String::from_utf8_lossy(field_data).to_string();
            let _ = env.set(&input_var, CobolValue::Alphanumeric(value_str.clone()));

            // Also set the plain field name
            let _ = env.set(&field_upper, CobolValue::Alphanumeric(value_str.clone()));

            // Set the flag byte variable (FIELDF) to indicate the field was modified
            let flag_var = format!("{}F", field_upper);
            // BMS convention: flag byte = 0x80 if data received
            if !field_data.is_empty() {
                let _ = env.set(
                    &flag_var,
                    CobolValue::Alphanumeric("\u{0080}".to_string()),
                );
            }

            // Set the length variable (FIELDL) to the actual data length
            let len_var = format!("{}L", field_upper);
            let _ = env.set(
                &len_var,
                CobolValue::from_i64(field_data.len() as i64),
            );
        }

        // If the BMS map is known, also look up fields that were NOT returned
        // (empty / unmodified) — for those, try to find the map definition
        // and set the input variables accordingly.
        if let Some(bms_map) = self.bms_maps.get(&map_name) {
            for field in &bms_map.fields {
                if field.name.is_empty() {
                    continue;
                }
                let field_upper = field.name.to_uppercase();
                if !fields.contains_key(&field_upper) {
                    // Field not in the modified set — clear the flag
                    let flag_var = format!("{}F", field_upper);
                    let _ = env.set(
                        &flag_var,
                        CobolValue::Alphanumeric("\u{0000}".to_string()),
                    );
                    let len_var = format!("{}L", field_upper);
                    let _ = env.set(&len_var, CobolValue::from_i64(0));
                }
            }
        }

        self.runtime
            .eib
            .set_response(CicsResponse::Normal);
        Ok(())
    }

    // -- XCTL ---------------------------------------------------------------

    fn handle_xctl(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let program = self
            .find_option_value(options, "PROGRAM")
            .unwrap_or_default()
            .to_uppercase();

        // COMMAREA option is a variable name (VAR_REF) — look up its value.
        let commarea = self
            .find_option_value(options, "COMMAREA")
            .and_then(|var_name| env.get(&var_name))
            .map(|val| val.to_display_string().into_bytes());

        info!("EXEC CICS XCTL PROGRAM({})", program);

        // Update the TUI session status
        self.session.borrow_mut().set_program(&program);

        self.pending_action = Some(BridgeAction::Xctl { program, commarea });

        // Return an error to stop the interpreter — the outer loop will
        // pick up the pending_action and transfer control.
        Err(InterpreterError {
            message: "XCTL".to_string(),
        })
    }

    // -- RETURN -------------------------------------------------------------

    fn handle_return(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let transid = self.find_option_value(options, "TRANSID");
        // COMMAREA option is a variable name (VAR_REF) — look up its value.
        let commarea = self
            .find_option_value(options, "COMMAREA")
            .and_then(|var_name| env.get(&var_name))
            .map(|val| val.to_display_string().into_bytes());

        if let Some(ref tid) = transid {
            info!("EXEC CICS RETURN TRANSID({})", tid);
            self.session.borrow_mut().set_transid(tid);
        } else {
            info!("EXEC CICS RETURN");
        }

        self.pending_action = Some(BridgeAction::Return { transid, commarea });

        // Stop the interpreter
        Err(InterpreterError {
            message: "RETURN".to_string(),
        })
    }

    // -- ABEND --------------------------------------------------------------

    fn handle_abend(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        _env: &mut Environment,
    ) -> Result<()> {
        let code = self
            .find_option_value(options, "ABCODE")
            .unwrap_or_else(|| "????".to_string());

        warn!("EXEC CICS ABEND ABCODE({})", code);

        self.pending_action = Some(BridgeAction::Abend {
            code: code.clone(),
        });

        Err(InterpreterError {
            message: format!("ABEND {}", code),
        })
    }

    // -- ASSIGN -------------------------------------------------------------

    fn handle_assign(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        // Each option name is a system field the program wants to retrieve.
        let field_names: Vec<&str> = options
            .iter()
            .map(|(name, _)| name.as_str())
            .collect();
        let values = self.runtime.assign(&field_names);

        // Store each retrieved value into the COBOL environment.
        // The option value (if present) tells us the target COBOL variable
        // name; if not present, use the field name itself.
        for (opt_name, opt_val) in options {
            let field_upper = opt_name.to_uppercase();
            if let Some(system_value) = values.get(&field_upper) {
                // Determine the COBOL variable to store into
                let target_var = if let Some(val) = opt_val {
                    val.to_display_string().trim().to_uppercase()
                } else {
                    field_upper.clone()
                };

                if !target_var.is_empty() {
                    let _ = env.set(
                        &target_var,
                        CobolValue::Alphanumeric(system_value.clone()),
                    );
                }

                // Also set the field name directly (for programs that check
                // WS-SYSID after ASSIGN SYSID(WS-SYSID))
                let _ = env.set(
                    &field_upper,
                    CobolValue::Alphanumeric(system_value.clone()),
                );
            }
        }

        self.runtime
            .eib
            .set_response(CicsResponse::Normal);
        Ok(())
    }

    // -- File READ ----------------------------------------------------------

    fn handle_file_read(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let file_name = self
            .find_option_value(options, "FILE")
            .or_else(|| self.find_option_value(options, "DATASET"))
            .unwrap_or_default()
            .trim()
            .to_uppercase();

        // RIDFLD is a variable reference — resolve to the variable's value
        let ridfld = self
            .resolve_var_ref(options, "RIDFLD", env)
            .unwrap_or_default();

        let update = self.has_option(options, "UPDATE");
        let mode = if update {
            FileMode::Update
        } else {
            FileMode::Read
        };

        // Set EIB file name
        self.runtime.eib.set_filename(&file_name);

        match self.runtime.files.read(&file_name, ridfld.as_bytes(), mode) {
            Ok(record) => {
                self.runtime
                    .eib
                    .set_response(CicsResponse::Normal);

                // If INTO is specified, set the record data into the COBOL var
                if let Some(into_var) = self.find_option_value(options, "INTO") {
                    let data_str =
                        String::from_utf8_lossy(&record.data).to_string();
                    let _ = env.set(
                        &into_var.to_uppercase(),
                        CobolValue::Alphanumeric(data_str),
                    );
                }

                // Set EIBRESP in environment
                let _ = env.set("EIBRESP", CobolValue::from_i64(0));
            }
            Err(_) => {
                // The FileManager already set EIBRESP on the runtime's EIB
                // via CicsResponse. Propagate it to the COBOL environment.
                let resp = self.runtime.eib.eibresp;
                let _ = env.set("EIBRESP", CobolValue::from_i64(resp as i64));
            }
        }

        Ok(())
    }

    // -- File WRITE ---------------------------------------------------------

    fn handle_file_write(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let file_name = self
            .find_option_value(options, "FILE")
            .or_else(|| self.find_option_value(options, "DATASET"))
            .unwrap_or_default()
            .trim()
            .to_uppercase();

        let ridfld = self
            .resolve_var_ref(options, "RIDFLD", env)
            .unwrap_or_default();

        let from_data = self
            .resolve_var_ref(options, "FROM", env)
            .unwrap_or_default();

        self.runtime.eib.set_filename(&file_name);

        let record = FileRecord::new(
            ridfld.into_bytes(),
            from_data.into_bytes(),
        );

        match self.runtime.files.write(&file_name, record) {
            Ok(()) => {
                self.runtime
                    .eib
                    .set_response(CicsResponse::Normal);
                let _ = env.set("EIBRESP", CobolValue::from_i64(0));
            }
            Err(_) => {
                let resp = self.runtime.eib.eibresp;
                let _ = env.set("EIBRESP", CobolValue::from_i64(resp as i64));
            }
        }

        Ok(())
    }

    // -- File REWRITE -------------------------------------------------------

    fn handle_file_rewrite(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let file_name = self
            .find_option_value(options, "FILE")
            .or_else(|| self.find_option_value(options, "DATASET"))
            .unwrap_or_default()
            .trim()
            .to_uppercase();

        let from_data = self
            .find_option_value(options, "FROM")
            .unwrap_or_default();

        self.runtime.eib.set_filename(&file_name);

        match self
            .runtime
            .files
            .rewrite(&file_name, from_data.as_bytes())
        {
            Ok(()) => {
                self.runtime
                    .eib
                    .set_response(CicsResponse::Normal);
                let _ = env.set("EIBRESP", CobolValue::from_i64(0));
            }
            Err(_) => {
                let resp = self.runtime.eib.eibresp;
                let _ = env.set("EIBRESP", CobolValue::from_i64(resp as i64));
            }
        }

        Ok(())
    }

    // -- File DELETE ---------------------------------------------------------

    fn handle_file_delete(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let file_name = self
            .find_option_value(options, "FILE")
            .or_else(|| self.find_option_value(options, "DATASET"))
            .unwrap_or_default()
            .trim()
            .to_uppercase();

        let ridfld = self.resolve_var_ref(options, "RIDFLD", env);

        self.runtime.eib.set_filename(&file_name);

        let key_bytes = ridfld.as_ref().map(|s| s.as_bytes());

        match self.runtime.files.delete(&file_name, key_bytes) {
            Ok(()) => {
                self.runtime
                    .eib
                    .set_response(CicsResponse::Normal);
                let _ = env.set("EIBRESP", CobolValue::from_i64(0));
            }
            Err(_) => {
                let resp = self.runtime.eib.eibresp;
                let _ = env.set("EIBRESP", CobolValue::from_i64(resp as i64));
            }
        }

        Ok(())
    }

    // -- STARTBR ------------------------------------------------------------

    fn handle_startbr(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let file_name = self
            .find_option_value(options, "FILE")
            .or_else(|| self.find_option_value(options, "DATASET"))
            .unwrap_or_default()
            .trim()
            .to_uppercase();

        let ridfld = self
            .resolve_var_ref(options, "RIDFLD", env)
            .unwrap_or_default();

        self.runtime.eib.set_filename(&file_name);

        match self
            .runtime
            .files
            .startbr(&file_name, ridfld.as_bytes())
        {
            Ok(token) => {
                self.browse_tokens.insert(file_name, token);
                self.runtime
                    .eib
                    .set_response(CicsResponse::Normal);
                let _ = env.set("EIBRESP", CobolValue::from_i64(0));
            }
            Err(_) => {
                let resp = self.runtime.eib.eibresp;
                let _ = env.set("EIBRESP", CobolValue::from_i64(resp as i64));
            }
        }

        Ok(())
    }

    // -- READNEXT -----------------------------------------------------------

    fn handle_readnext(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let file_name = self
            .find_option_value(options, "FILE")
            .or_else(|| self.find_option_value(options, "DATASET"))
            .unwrap_or_default()
            .trim()
            .to_uppercase();

        self.runtime.eib.set_filename(&file_name);

        let token = match self.browse_tokens.get(&file_name) {
            Some(&t) => t,
            None => {
                self.runtime
                    .eib
                    .set_response(CicsResponse::Invreq);
                let _ = env.set(
                    "EIBRESP",
                    CobolValue::from_i64(
                        CicsResponse::Invreq as i64,
                    ),
                );
                return Ok(());
            }
        };

        match self.runtime.files.readnext(token) {
            Ok(record) => {
                self.runtime
                    .eib
                    .set_response(CicsResponse::Normal);

                // Set INTO variable
                if let Some(into_var) = self.find_option_value(options, "INTO") {
                    let data_str =
                        String::from_utf8_lossy(&record.data).to_string();
                    let _ = env.set(
                        &into_var.to_uppercase(),
                        CobolValue::Alphanumeric(data_str),
                    );
                }

                // Set RIDFLD to the record key
                if let Some(ridfld_var) =
                    self.find_option_value(options, "RIDFLD")
                {
                    let key_str =
                        String::from_utf8_lossy(&record.key).to_string();
                    let _ = env.set(
                        &ridfld_var.to_uppercase(),
                        CobolValue::Alphanumeric(key_str),
                    );
                }

                let _ = env.set("EIBRESP", CobolValue::from_i64(0));
            }
            Err(_) => {
                let resp = self.runtime.eib.eibresp;
                let _ = env.set("EIBRESP", CobolValue::from_i64(resp as i64));
            }
        }

        Ok(())
    }

    // -- ENDBR --------------------------------------------------------------

    fn handle_endbr(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let file_name = self
            .find_option_value(options, "FILE")
            .or_else(|| self.find_option_value(options, "DATASET"))
            .unwrap_or_default()
            .trim()
            .to_uppercase();

        if let Some(token) = self.browse_tokens.remove(&file_name) {
            match self.runtime.files.endbr(token) {
                Ok(()) => {
                    self.runtime
                        .eib
                        .set_response(CicsResponse::Normal);
                    let _ = env.set("EIBRESP", CobolValue::from_i64(0));
                }
                Err(_) => {
                    let resp = self.runtime.eib.eibresp;
                    let _ = env.set("EIBRESP", CobolValue::from_i64(resp as i64));
                }
            }
        } else {
            self.runtime
                .eib
                .set_response(CicsResponse::Invreq);
            let _ = env.set(
                "EIBRESP",
                CobolValue::from_i64(
                    CicsResponse::Invreq as i64,
                ),
            );
        }

        Ok(())
    }

    // -- HANDLE CONDITION / HANDLE ABEND ------------------------------------

    fn handle_handle_condition(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        _env: &mut Environment,
    ) -> Result<()> {
        // Each option is CONDITION(LABEL)
        for (condition, label_val) in options {
            let label = label_val
                .as_ref()
                .map(|v| v.to_display_string())
                .unwrap_or_default();
            self.runtime
                .handle_condition(&condition.to_uppercase(), label.trim());
        }

        self.runtime
            .eib
            .set_response(CicsResponse::Normal);
        Ok(())
    }

    fn handle_handle_abend(
        &mut self,
        options: &[(String, Option<CobolValue>)],
        _env: &mut Environment,
    ) -> Result<()> {
        let label = self
            .find_option_value(options, "LABEL")
            .or_else(|| self.find_option_value(options, "PROGRAM"))
            .unwrap_or_default();

        self.runtime.handle_abend(label.trim());

        self.runtime
            .eib
            .set_response(CicsResponse::Normal);
        Ok(())
    }

    // -- Dispatcher fallback ------------------------------------------------

    /// Delegate any command to the CicsDispatcher by building a
    /// `CommandParamBlock` from the interpreter options.
    fn dispatch_via_cics(
        &mut self,
        command: &str,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let call_name = format!("CICS{}", &command[..std::cmp::min(4, command.len())]);

        let mut params = CommandParamBlock::new(&call_name);
        for (name, value) in options {
            let val_str = value
                .as_ref()
                .map(|v| v.to_display_string())
                .unwrap_or_default();
            params.set(&name.to_uppercase(), val_str.trim());
        }

        match CicsDispatcher::dispatch(
            &call_name,
            &params,
            &mut self.runtime,
            Some(&self.registry),
        ) {
            Ok(result) => {
                self.apply_dispatch_result(&result, options, env);
            }
            Err(e) => {
                warn!("CicsDispatcher error for {}: {}", command, e);
                let _ = env.set(
                    "EIBRESP",
                    CobolValue::from_i64(self.runtime.eib.eibresp as i64),
                );
            }
        }

        Ok(())
    }

    /// Apply the output of a `DispatchResult` back into the COBOL environment.
    fn apply_dispatch_result(
        &self,
        result: &DispatchResult,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) {
        // Set EIBRESP / EIBRESP2
        let _ = env.set(
            "EIBRESP",
            CobolValue::from_i64(result.eibresp as i64),
        );
        let _ = env.set(
            "EIBRESP2",
            CobolValue::from_i64(result.eibresp2 as i64),
        );

        // If there is output data and an INTO variable, set it
        if let Some(ref data) = result.output_data {
            if let Some(into_var) = self.find_option_value(options, "INTO") {
                let data_str = String::from_utf8_lossy(data).to_string();
                let _ = env.set(
                    &into_var.to_uppercase(),
                    CobolValue::Alphanumeric(data_str),
                );
            }
        }

        // Apply output fields
        for (key, value) in &result.output_fields {
            let _ = env.set(
                &key.to_uppercase(),
                CobolValue::Alphanumeric(value.clone()),
            );
        }
    }

    // -- Option helpers ------------------------------------------------------

    /// Find the string value of a named option.
    /// Returns the `to_display_string()` of the CobolValue if present,
    /// or `None` if the option is absent or flag-only.
    fn find_option_value(
        &self,
        options: &[(String, Option<CobolValue>)],
        name: &str,
    ) -> Option<String> {
        let name_upper = name.to_uppercase();
        options
            .iter()
            .find(|(n, _)| n.to_uppercase() == name_upper)
            .and_then(|(_, v)| v.as_ref())
            .map(|v| v.to_display_string())
    }

    /// Find the raw CobolValue for a named option.
    fn find_option_cobol_value<'a>(
        &self,
        options: &'a [(String, Option<CobolValue>)],
        name: &str,
    ) -> Option<&'a CobolValue> {
        let name_upper = name.to_uppercase();
        options
            .iter()
            .find(|(n, _)| n.to_uppercase() == name_upper)
            .and_then(|(_, v)| v.as_ref())
    }

    /// Check whether a flag option is present (value may be `None`).
    fn has_option(
        &self,
        options: &[(String, Option<CobolValue>)],
        name: &str,
    ) -> bool {
        let name_upper = name.to_uppercase();
        options.iter().any(|(n, _)| n.to_uppercase() == name_upper)
    }

    /// Resolve a variable-reference option (like RIDFLD, FROM).
    ///
    /// The interpreter passes the *variable name* for VAR_REF_OPTIONS.
    /// This helper looks up that variable in the COBOL environment and
    /// returns its current value.  Falls back to the raw string if the
    /// variable is not found (handles literal values).
    fn resolve_var_ref(
        &self,
        options: &[(String, Option<CobolValue>)],
        name: &str,
        env: &Environment,
    ) -> Option<String> {
        let var_name = self.find_option_value(options, name)?;
        // Try to look up the variable in the environment
        if let Some(val) = env.get(&var_name.to_uppercase()) {
            Some(val.to_display_string())
        } else if let Some(val) = env.get(&var_name) {
            Some(val.to_display_string())
        } else {
            // Not a variable — treat as a literal value
            Some(var_name)
        }
    }
}

// ---------------------------------------------------------------------------
// CicsCommandHandler implementation
// ---------------------------------------------------------------------------

impl CicsCommandHandler for CicsBridge {
    fn execute(
        &mut self,
        command: &str,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        let cmd = command.to_uppercase();
        debug!("EXEC CICS {} ({} option(s))", cmd, options.len());

        // Capture RESP/RESP2 variable names before dispatching.
        let resp_var = self.find_option_value(options, "RESP");
        let resp2_var = self.find_option_value(options, "RESP2");

        let result = self.execute_inner(&cmd, command, options, env);

        // Write RESP/RESP2 back to the COBOL variables.
        if let Some(ref var_name) = resp_var {
            let resp_code = self.runtime.eib.eibresp as i64;
            let _ = env.set(
                &var_name.to_uppercase(),
                CobolValue::from_i64(resp_code),
            );
        }
        if let Some(ref var_name) = resp2_var {
            let resp2_code = self.runtime.eib.eibresp2 as i64;
            let _ = env.set(
                &var_name.to_uppercase(),
                CobolValue::from_i64(resp2_code),
            );
        }

        result
    }

    fn as_any_mut(&mut self) -> Option<&mut dyn std::any::Any> {
        Some(self)
    }
}

impl CicsBridge {
    /// Inner dispatch — called by `execute()` which wraps RESP/RESP2 handling.
    fn execute_inner(
        &mut self,
        cmd: &str,
        command: &str,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()> {
        match cmd {
            // -- Terminal I/O ------------------------------------------------
            "SEND" => {
                if self.has_option(options, "MAP") {
                    self.handle_send_map(options, env)
                } else if self.has_option(options, "TEXT")
                    || self.has_option(options, "FROM")
                {
                    self.handle_send_text(options, env)
                } else {
                    // Plain SEND — delegate
                    self.dispatch_via_cics(command, options, env)
                }
            }

            "RECEIVE" => {
                if self.has_option(options, "MAP") {
                    self.handle_receive_map(options, env)
                } else {
                    self.dispatch_via_cics(command, options, env)
                }
            }

            // -- File operations ---------------------------------------------
            "READ" => self.handle_file_read(options, env),
            "WRITE" => self.handle_file_write(options, env),
            "REWRITE" => self.handle_file_rewrite(options, env),
            "DELETE" => self.handle_file_delete(options, env),

            // -- File browse -------------------------------------------------
            "STARTBR" => self.handle_startbr(options, env),
            "READNEXT" => self.handle_readnext(options, env),
            "READPREV" => {
                // Similar to READNEXT but in reverse — delegate for now
                self.dispatch_via_cics(command, options, env)
            }
            "ENDBR" => self.handle_endbr(options, env),
            "RESETBR" => {
                // Reset browse position — delegate to dispatcher
                self.dispatch_via_cics(command, options, env)
            }

            // -- Program control ---------------------------------------------
            "XCTL" => self.handle_xctl(options, env),
            "RETURN" => self.handle_return(options, env),
            "LINK" => self.dispatch_via_cics(command, options, env),
            "ABEND" => self.handle_abend(options, env),

            // -- Condition handling -------------------------------------------
            "HANDLE CONDITION" | "HANDLE" => {
                self.handle_handle_condition(options, env)
            }
            "HANDLE ABEND" => self.handle_handle_abend(options, env),
            "IGNORE CONDITION" | "IGNORE" => {
                // IGNORE CONDITION — remove handlers
                for (condition, _) in options {
                    self.runtime.ignore_condition(&condition.to_uppercase());
                }
                self.runtime
                    .eib
                    .set_response(CicsResponse::Normal);
                Ok(())
            }

            // -- System info -------------------------------------------------
            "ASSIGN" => self.handle_assign(options, env),

            // -- Queue operations (delegate) ---------------------------------
            "WRITEQ" | "READQ" | "DELETEQ" => {
                self.dispatch_via_cics(command, options, env)
            }

            // -- Storage management (delegate) -------------------------------
            "GETMAIN" | "FREEMAIN" => {
                self.dispatch_via_cics(command, options, env)
            }

            // -- ENQ/DEQ (delegate) ------------------------------------------
            "ENQ" | "DEQ" => {
                self.dispatch_via_cics(command, options, env)
            }

            // -- Container operations (delegate) -----------------------------
            "PUT" | "GET" | "DELETE CONTAINER" => {
                self.dispatch_via_cics(command, options, env)
            }

            // -- Miscellaneous -----------------------------------------------
            "ASKTIME" => {
                // Set current time in EIB using std::time.
                // The EIB stores time as 0HHMMSS and date as 0CYYDDD.
                // We use UNIX_EPOCH arithmetic to derive the current UTC
                // time — sufficient for CICS ASKTIME purposes.
                use std::time::{SystemTime, UNIX_EPOCH};
                let secs = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_secs();

                // Convert seconds-since-epoch to hours/minutes/seconds of day
                let time_of_day = secs % 86400;
                let hour = (time_of_day / 3600) as u8;
                let minute = ((time_of_day % 3600) / 60) as u8;
                let second = (time_of_day % 60) as u8;
                self.runtime.eib.set_time(hour, minute, second);

                // Approximate year/day-of-year from epoch seconds.
                // This is a simplified calculation; for CICS simulation it is
                // adequate.
                let days_since_epoch = (secs / 86400) as i64;
                let (year, day_of_year) =
                    epoch_days_to_year_doy(days_since_epoch);
                self.runtime
                    .eib
                    .set_date(year as u16, day_of_year as u16);

                self.runtime
                    .eib
                    .set_response(CicsResponse::Normal);
                Ok(())
            }

            "FORMATTIME" => {
                // Delegate to dispatcher or handle inline.
                // For now, set a formatted date/time in the target variable.
                self.dispatch_via_cics(command, options, env)
            }

            // -- Catch-all: delegate to CicsDispatcher -----------------------
            _ => {
                debug!("Delegating unknown CICS command '{}' to dispatcher", cmd);
                self.dispatch_via_cics(command, options, env)
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Utility: convert days since Unix epoch to (year, day_of_year)
// ---------------------------------------------------------------------------

/// Convert days since the Unix epoch (1970-01-01) to a (year, day-of-year)
/// pair.  Day-of-year is 1-based.  This uses a simple loop over years which
/// is perfectly adequate for the range of dates we care about (1970–2100).
fn epoch_days_to_year_doy(mut days: i64) -> (i64, i64) {
    let mut year: i64 = 1970;

    loop {
        let days_in_year = if is_leap(year) { 366 } else { 365 };
        if days < days_in_year {
            return (year, days + 1); // 1-based DOY
        }
        days -= days_in_year;
        year += 1;
    }
}

/// Determine whether `year` is a leap year.
fn is_leap(year: i64) -> bool {
    (year % 4 == 0 && year % 100 != 0) || year % 400 == 0
}
