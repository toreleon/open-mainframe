//! Interactive CICS terminal session command.
//!
//! Launches a full-screen TUI that renders 3270 screens from BMS maps,
//! allowing interactive execution of CICS-based COBOL programs.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use miette::Result;

use zos_cics::bms::{BmsMap, BmsParser};
use zos_cics::terminal::{SendMapOptions, TerminalCallback};
use zos_cics::CicsResult;
use zos_runtime::interpreter::Environment;
use zos_runtime::value::CobolValue;

use zos_tui::session::{Session, SessionConfig, TuiTerminal, setup_terminal, restore_terminal};

use super::cics_bridge::CicsBridge;
use super::interpret::{load_program as load_interpret_program, find_program_source, load_vsam_data};

// ---------- TUI Callback (connects CicsBridge → Session) ----------

/// An accumulated SEND MAP event captured by the TUI callback.
#[derive(Clone)]
struct PendingSendMap {
    map: BmsMap,
    data: HashMap<String, Vec<u8>>,
    options: SendMapOptions,
}

/// An accumulated SEND TEXT event captured by the TUI callback.
#[derive(Clone)]
struct PendingSendText {
    text: String,
    erase: bool,
}

/// Shared state between the TUI callback and the session loop.
struct TuiCallbackState {
    /// BMS mapset definitions for resolving map names.
    mapset_maps: HashMap<String, HashMap<String, BmsMap>>,
    /// Pending SEND MAP events (accumulated during program execution).
    pending_maps: Vec<PendingSendMap>,
    /// Pending SEND TEXT events.
    pending_texts: Vec<PendingSendText>,
}

/// TerminalCallback implementation that captures events for the TUI session.
///
/// During program execution, SEND MAP/SEND TEXT calls go through the CicsBridge
/// to this callback. It resolves map names to actual BmsMap objects and stores
/// the events. After execution, the session loop drains these events and applies
/// them to the Session, which then renders them to the TUI.
struct TuiCallback {
    state: Arc<Mutex<TuiCallbackState>>,
}

impl TuiCallback {
    fn new(mapset_maps: HashMap<String, HashMap<String, BmsMap>>) -> (Self, Arc<Mutex<TuiCallbackState>>) {
        let state = Arc::new(Mutex::new(TuiCallbackState {
            mapset_maps,
            pending_maps: Vec::new(),
            pending_texts: Vec::new(),
        }));
        (Self { state: state.clone() }, state)
    }
}

impl TerminalCallback for TuiCallback {
    fn on_send_map(
        &mut self,
        _map: &BmsMap,
        data: &HashMap<String, Vec<u8>>,
        options: &SendMapOptions,
    ) {
        let mut state = self.state.lock().unwrap();

        // Resolve the real BMS map from the metadata passed via the data map.
        let map_name = data.get("__MAP_NAME__")
            .map(|v| String::from_utf8_lossy(v).trim().to_uppercase())
            .unwrap_or_default();
        let mapset_name = data.get("__MAPSET_NAME__")
            .map(|v| String::from_utf8_lossy(v).trim().to_uppercase())
            .unwrap_or_default();

        // Look up the BMS map definition.
        let resolved_map = state.mapset_maps
            .get(&mapset_name)
            .and_then(|maps| maps.get(&map_name))
            .cloned();

        if let Some(bms_map) = resolved_map {
            // Filter out metadata keys from data.
            let clean_data: HashMap<String, Vec<u8>> = data.iter()
                .filter(|(k, _)| !k.starts_with("__"))
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();

            state.pending_maps.push(PendingSendMap {
                map: bms_map,
                data: clean_data,
                options: options.clone(),
            });
            tracing::info!("TUI callback: captured SEND MAP({}) MAPSET({})", map_name, mapset_name);
        } else {
            tracing::warn!(
                "TUI callback: BMS map not found: MAP({}) MAPSET({})",
                map_name,
                mapset_name
            );
        }
    }

    fn on_send_text(&mut self, text: &str, erase: bool) {
        let mut state = self.state.lock().unwrap();
        state.pending_texts.push(PendingSendText {
            text: text.to_string(),
            erase,
        });
        tracing::info!("TUI callback: captured SEND TEXT");
    }

    fn on_receive_map(
        &mut self,
        _map: &BmsMap,
    ) -> CicsResult<(u8, HashMap<String, Vec<u8>>)> {
        // In the TUI model, input is collected by the session loop before
        // re-invocation, so this should not be called during execution.
        // Return default ENTER with no fields.
        Ok((zos_cics::runtime::eib::aid::ENTER, HashMap::new()))
    }

    fn on_receive(&mut self, _max_length: usize) -> CicsResult<(Vec<u8>, u8)> {
        Ok((Vec::new(), zos_cics::runtime::eib::aid::ENTER))
    }
}

/// Run an interactive CICS session with TUI rendering.
pub fn run_session(
    input: PathBuf,
    include_paths: Vec<PathBuf>,
    data_files: Vec<String>,
    bms_dir: Option<PathBuf>,
    color_theme: String,
    transid_map: Vec<String>,
) -> Result<()> {
    tracing::info!("Starting interactive CICS session: {}", input.display());

    let search_dir = input
        .parent()
        .unwrap_or(std::path::Path::new("."))
        .to_path_buf();

    // Parse TRANSID=PROGRAM mappings
    let mut transid_programs: HashMap<String, String> = HashMap::new();
    for spec in &transid_map {
        if let Some((tid, prog)) = spec.split_once('=') {
            transid_programs.insert(tid.to_uppercase(), prog.to_uppercase());
        }
    }

    // Load all BMS map definitions from bms_dir (or search_dir)
    let bms_search_dir = bms_dir.unwrap_or_else(|| search_dir.clone());
    let mut mapset_maps: HashMap<String, HashMap<String, BmsMap>> = HashMap::new();
    load_bms_maps(&bms_search_dir, &mut mapset_maps);

    // Load initial program
    let simple = load_interpret_program(&input, &include_paths)?;
    let program_name = simple.name.to_uppercase();
    tracing::info!("Initial program: {}", program_name);

    // Program cache
    let mut program_cache: HashMap<String, zos_runtime::interpreter::SimpleProgram> = HashMap::new();
    program_cache.insert(program_name.clone(), simple);

    // Set up CICS bridge
    let mut bridge = CicsBridge::new("CICS", "T001");

    // Load VSAM data files
    for spec in &data_files {
        if let Some((ddname, rest)) = spec.split_once('=') {
            let parts: Vec<&str> = rest.split(':').collect();
            let file_path = parts[0];
            let key_len: usize = parts.get(1).and_then(|s| s.parse().ok()).unwrap_or(8);
            let rec_len: usize = parts.get(2).and_then(|s| s.parse().ok()).unwrap_or(80);

            match load_vsam_data(file_path, key_len, rec_len) {
                Ok(records) => {
                    tracing::info!(
                        "Loaded {} records for {}",
                        records.len(),
                        ddname
                    );
                    bridge.register_file(ddname, rec_len, key_len, records);
                }
                Err(e) => {
                    tracing::warn!("Failed to load VSAM data for {}: {}", ddname, e);
                }
            }
        }
    }

    // Create the TUI callback and wire it into the bridge.
    // The callback captures SEND MAP/TEXT events during execution,
    // which the session loop then applies to the TUI after each program run.
    let (tui_callback, callback_state) = TuiCallback::new(mapset_maps);
    bridge.set_terminal_callback(Box::new(tui_callback));

    // Initialize TUI
    let config = SessionConfig {
        initial_program: input.clone(),
        include_paths: include_paths.clone(),
        data_files: data_files.clone(),
        program_dir: Some(search_dir.clone()),
        transid_map: transid_programs.clone(),
        color_theme,
        userid: None,
        initial_transid: None,
    };

    let mut session = Session::new(config);
    let mut terminal = setup_terminal().map_err(|e| miette::miette!("TUI init failed: {}", e))?;

    // Create environment with CICS handler
    let mut env = Environment::new().with_cics_handler(Box::new(bridge));

    // Inject CICS EIB fields
    env.set("EIBCALEN", CobolValue::from_i64(0)).ok();
    env.set("EIBAID", CobolValue::alphanumeric(String::from('\x7D'))).ok();
    env.set("EIBRESP", CobolValue::from_i64(0)).ok();
    env.set("EIBRESP2", CobolValue::from_i64(0)).ok();
    env.set("EIBTRMID", CobolValue::alphanumeric("T001")).ok();
    env.set("EIBTRNID", CobolValue::alphanumeric("CICS")).ok();

    let mut current_program = program_name.clone();

    // Main session loop: execute program, show screen, wait for input, repeat
    let mut ctx = SessionContext {
        program_cache: &mut program_cache,
        current_program: &mut current_program,
        search_dir: &search_dir,
        include_paths: &include_paths,
        transid_programs: &transid_programs,
        callback_state: &callback_state,
    };
    let result = run_session_loop(
        &mut session,
        &mut terminal,
        &mut env,
        &mut ctx,
    );

    // Restore terminal before handling result
    let _ = restore_terminal();

    match result {
        Ok(()) => {
            tracing::info!("CICS session ended normally");
            Ok(())
        }
        Err(e) => {
            let msg = e.to_string();
            if msg.contains("interrupted") || msg.contains("Interrupted") {
                tracing::info!("CICS session interrupted by user");
                Ok(())
            } else {
                Err(miette::miette!("{}", e))
            }
        }
    }
}

/// Drain pending events from the TUI callback state and apply them to the session.
fn apply_pending_events(
    session: &mut Session,
    callback_state: &Arc<Mutex<TuiCallbackState>>,
) {
    let mut state = callback_state.lock().unwrap();

    // Apply pending SEND MAP events
    for event in state.pending_maps.drain(..) {
        session.on_send_map(&event.map, &event.data, &event.options);
    }

    // Apply pending SEND TEXT events
    for event in state.pending_texts.drain(..) {
        session.on_send_text(&event.text, event.erase);
    }
}

/// Runtime context for the session loop, grouping related parameters.
struct SessionContext<'a> {
    program_cache: &'a mut HashMap<String, zos_runtime::interpreter::SimpleProgram>,
    current_program: &'a mut String,
    search_dir: &'a std::path::Path,
    include_paths: &'a [PathBuf],
    transid_programs: &'a HashMap<String, String>,
    callback_state: &'a Arc<Mutex<TuiCallbackState>>,
}

/// Main session loop: run programs, display screens, get input.
fn run_session_loop(
    session: &mut Session,
    terminal: &mut TuiTerminal,
    env: &mut Environment,
    ctx: &mut SessionContext<'_>,
) -> std::result::Result<(), zos_tui::SessionError> {
    loop {
        // Ensure program is loaded
        if !ctx.program_cache.contains_key(ctx.current_program.as_str()) {
            match find_program_source(ctx.current_program, ctx.search_dir) {
                Some(path) => {
                    tracing::info!("Loading program {} from {}", ctx.current_program, path.display());
                    let p = load_interpret_program(&path, ctx.include_paths)
                        .map_err(|e| zos_tui::SessionError::Execution {
                            program: ctx.current_program.clone(),
                            message: e.to_string(),
                        })?;
                    ctx.program_cache.insert(ctx.current_program.clone(), p);
                }
                None => {
                    return Err(zos_tui::SessionError::ProgramNotFound {
                        name: ctx.current_program.clone(),
                    });
                }
            }
        }

        session.set_program(ctx.current_program);

        // Execute the program
        env.resume();
        let program = ctx.program_cache.get(ctx.current_program.as_str()).unwrap();

        let _rc = zos_runtime::interpreter::execute(program, env)
            .map_err(|e| zos_tui::SessionError::Execution {
                program: ctx.current_program.clone(),
                message: e.to_string(),
            })?;

        // After execution, apply any SEND MAP/TEXT events captured by the callback
        // to the session so the TUI renders the correct screen.
        apply_pending_events(session, ctx.callback_state);

        // Check bridge state
        if let Some(mut handler) = env.cics_handler.take() {
            let bridge = handler
                .as_any_mut()
                .and_then(|a| a.downcast_mut::<CicsBridge>());

            if let Some(bridge) = bridge {
                // Handle XCTL
                if let Some(ref xctl_target) = bridge.xctl_program.clone() {
                    let commarea_data = bridge.commarea_var.as_ref().and_then(|ca| {
                        env.get(ca).map(|v| v.to_display_string())
                    });

                    *ctx.current_program = xctl_target.to_uppercase();
                    bridge.reset_for_xctl();
                    env.cics_handler = Some(handler);

                    if let Some(data) = commarea_data {
                        let len = data.len();
                        env.set("DFHCOMMAREA", CobolValue::Alphanumeric(data)).ok();
                        env.set("EIBCALEN", CobolValue::from_i64(len as i64)).ok();
                    }
                    continue;
                }

                // Handle RETURN TRANSID (pseudo-conversational)
                if bridge.returned {
                    if let Some(ref transid) = bridge.return_transid.clone() {
                        // Show the screen and wait for user input
                        let (aid, fields) = session.wait_for_input(terminal)?;

                        // Set EIBAID for the next program iteration
                        env.set("EIBAID", CobolValue::alphanumeric(String::from(aid as char))).ok();

                        // Set input fields into environment
                        for (name, data) in &fields {
                            let value = String::from_utf8_lossy(data).to_string();
                            env.set(name, CobolValue::Alphanumeric(value)).ok();
                        }

                        // Look up which program to run for this TRANSID
                        let next_program = ctx.transid_programs
                            .get(&transid.to_uppercase())
                            .cloned()
                            .unwrap_or_else(|| ctx.current_program.clone());

                        *ctx.current_program = next_program;

                        // Update bridge for next iteration
                        bridge.returned = false;
                        bridge.return_transid = None;
                        bridge.xctl_program = None;
                        bridge.runtime.eib.reset_for_command();

                        // Pass COMMAREA
                        if let Some(ref ca_var) = bridge.commarea_var.clone() {
                            if let Some(val) = env.get(ca_var) {
                                let data = val.to_display_string();
                                let len = data.len();
                                env.set("DFHCOMMAREA", CobolValue::Alphanumeric(data)).ok();
                                env.set("EIBCALEN", CobolValue::from_i64(len as i64)).ok();
                            }
                        }

                        env.cics_handler = Some(handler);
                        session.set_transid(transid);
                        continue;
                    }

                    // RETURN without TRANSID - session ends
                    env.cics_handler = Some(handler);
                    break;
                }
            }

            env.cics_handler = Some(handler);
        }

        // Normal completion - show screen with last output and wait
        let (aid, _fields) = session.wait_for_input(terminal)?;
        if aid == zos_cics::runtime::eib::aid::PF3 {
            break; // PF3 to exit
        }

        // No more programs to run
        break;
    }

    Ok(())
}

/// Load all BMS map definitions from a directory.
fn load_bms_maps(
    dir: &std::path::Path,
    maps: &mut HashMap<String, HashMap<String, BmsMap>>,
) {
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().map(|e| e == "bms" || e == "BMS").unwrap_or(false) {
            if let Ok(source) = std::fs::read_to_string(&path) {
                let mut parser = BmsParser::new();
                if let Ok(mapset) = parser.parse(&source) {
                    let mut map_entries = HashMap::new();
                    for map in mapset.maps {
                        map_entries.insert(map.name.to_uppercase(), map);
                    }
                    maps.insert(mapset.name.to_uppercase(), map_entries);
                    tracing::info!("Loaded BMS mapset from {}", path.display());
                }
            }
        }
    }
}
