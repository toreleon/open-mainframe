//! Per-session CICS execution runner.
//!
//! Each CICS session spawns a dedicated OS thread that owns the
//! `CicsBridge`, `Session`, and all `Rc<RefCell<>>` state.  The async
//! Axum handlers communicate with the thread via channels.

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;
use std::thread;

use tokio::sync::{mpsc, oneshot};
use tracing::{error, info};

use open_mainframe_lib::bridge::{BridgeAction, CicsBridge};
use open_mainframe_lib::headless::{self, EndOutput, ScreenOutput};
use open_mainframe_lib::{compile_program, find_program_source, setup_env, BridgeHandler};
use open_mainframe_runtime::interpreter;
use open_mainframe_tui::session::{Session, SessionConfig, TerminalModel};

// ---------------------------------------------------------------------------
// Public types
// ---------------------------------------------------------------------------

/// Configuration for a CICS application session.
#[derive(Debug, Clone)]
pub struct CicsAppConfig {
    pub initial_program: PathBuf,
    pub include_paths: Vec<PathBuf>,
    pub bms_dir: Option<PathBuf>,
    pub data_files: Vec<String>,
    pub transid_map: HashMap<String, String>,
    pub program_dir: Option<PathBuf>,
    pub userid: String,
}

/// Commands sent from async HTTP handlers to the session thread.
pub enum SessionCommand {
    /// Send AID + field data, await the resulting screen.
    SendInput {
        aid: String,
        fields: HashMap<String, String>,
        reply: oneshot::Sender<SessionResponse>,
    },
    /// Get the current screen state without sending input.
    GetScreen {
        reply: oneshot::Sender<SessionResponse>,
    },
    /// Shut down the session thread.
    Shutdown,
}

/// Responses sent from the session thread to HTTP handlers.
pub enum SessionResponse {
    /// A screen to display (session continues).
    Screen(ScreenOutput),
    /// The session has ended.
    End(EndOutput),
    /// An error occurred.
    Error(String),
}

/// Handle for an active CICS execution session, stored in `AppState`.
pub struct CicsSessionRunner {
    /// Channel to send commands to the session thread.
    pub cmd_tx: mpsc::Sender<SessionCommand>,
    /// Join handle for the session thread.
    thread_handle: Option<thread::JoinHandle<()>>,
    /// Owner userid.
    pub userid: String,
}

impl Drop for CicsSessionRunner {
    fn drop(&mut self) {
        // Best-effort shutdown: send Shutdown and join the thread.
        let _ = self.cmd_tx.try_send(SessionCommand::Shutdown);
        if let Some(handle) = self.thread_handle.take() {
            let _ = handle.join();
        }
    }
}

impl CicsSessionRunner {
    /// Spawn a new CICS session on a dedicated OS thread.
    ///
    /// Returns a runner handle with the channel endpoint, or an error if the
    /// thread could not be spawned.
    pub fn spawn(config: CicsAppConfig) -> Result<Self, String> {
        let (cmd_tx, cmd_rx) = mpsc::channel::<SessionCommand>(4);
        let userid = config.userid.clone();

        let handle = thread::Builder::new()
            .name(format!("cics-session-{}", userid))
            .spawn(move || {
                // Create a single-threaded tokio runtime so we can use the
                // async mpsc receiver from a synchronous context.
                let rt = tokio::runtime::Builder::new_current_thread()
                    .enable_all()
                    .build()
                    .expect("Failed to build tokio runtime for CICS session");

                // Wrap the entire session in catch_unwind for panic safety.
                // If the COBOL interpreter panics, we report the error
                // through the channel instead of crashing the server.
                let result =
                    std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        rt.block_on(async {
                            session_thread_main(config, cmd_rx).await;
                        });
                    }));

                if let Err(panic_info) = result {
                    let msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
                        format!("CICS session thread panicked: {s}")
                    } else if let Some(s) = panic_info.downcast_ref::<String>() {
                        format!("CICS session thread panicked: {s}")
                    } else {
                        "CICS session thread panicked (unknown payload)".to_string()
                    };
                    error!("{msg}");
                    // The channel will be dropped, causing any pending
                    // oneshot receivers to get a RecvError.
                }
            })
            .map_err(|e| format!("Failed to spawn CICS session thread: {e}"))?;

        Ok(Self {
            cmd_tx,
            thread_handle: Some(handle),
            userid,
        })
    }
}

// ---------------------------------------------------------------------------
// Session thread internals
// ---------------------------------------------------------------------------

/// Build the list of directories to search for XCTL target programs.
fn build_search_dirs(config: &CicsAppConfig) -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    if let Some(ref pd) = config.program_dir {
        dirs.push(pd.clone());
    }
    if let Some(parent) = config.initial_program.parent() {
        dirs.push(parent.to_path_buf());
    }
    dirs
}

/// Compile a program and register it in the bridge's program registry.
fn compile_and_register(
    prog_name: &str,
    search_dirs: &[PathBuf],
    include_paths: &[PathBuf],
    bridge: &Rc<RefCell<CicsBridge>>,
) -> Result<(interpreter::SimpleProgram, String), String> {
    let source_path = find_program_source(prog_name, search_dirs)
        .ok_or_else(|| format!("Program {} not found", prog_name))?;

    let prog = compile_program(&source_path, include_paths)?;
    let name = prog.name.clone();

    if !bridge.borrow().registry().exists(prog_name) {
        bridge
            .borrow_mut()
            .registry_mut()
            .register(prog_name, |_| {
                Ok(open_mainframe_cics::runtime::ProgramResult::Return)
            });
    }

    Ok((prog, name))
}

/// Send a screen response via the channel and wait for the next `SendInput`.
///
/// Returns `Some((aid_byte, fields))` on success, or `None` if the channel
/// closed or a `Shutdown` was received.
async fn wait_for_input_via_channel(
    cmd_rx: &mut mpsc::Receiver<SessionCommand>,
    screen: ScreenOutput,
) -> Option<(u8, HashMap<String, Vec<u8>>)> {
    loop {
        match cmd_rx.recv().await {
            Some(SessionCommand::GetScreen { reply }) => {
                let _ = reply.send(SessionResponse::Screen(screen.clone()));
                // Keep waiting for a SendInput.
            }
            Some(SessionCommand::SendInput {
                aid,
                fields,
                reply,
            }) => {
                // Reply with the current screen first (the caller gets it
                // immediately) then we'll produce the *next* screen after
                // executing the program.  Actually, the PUT handler waits
                // for the *next* screen.  So we store the reply and return
                // the input — the caller of this function is responsible
                // for sending the next screen on this reply channel.
                //
                // We need to pass the reply channel back up.  Stash it in
                // a thread-local or pass by ref.  For simplicity, we send
                // the current screen on the reply and let the outer loop
                // handle the next screen via a fresh GetScreen.  But that
                // means PUT returns the *current* screen, not the result
                // of the input.  That's wrong.
                //
                // Better approach: don't reply yet.  Return the reply
                // channel along with the input so the outer loop can
                // reply after execution.
                let aid_byte = headless::aid_from_name(&aid).unwrap_or(0x7D);
                let field_data: HashMap<String, Vec<u8>> = fields
                    .into_iter()
                    .map(|(k, v)| (k.to_uppercase(), v.into_bytes()))
                    .collect();

                // Stash the reply in PENDING_REPLY so the outer loop can
                // use it after execution.
                PENDING_REPLY.with(|cell| {
                    *cell.borrow_mut() = Some(reply);
                });

                return Some((aid_byte, field_data));
            }
            Some(SessionCommand::Shutdown) | None => return None,
        }
    }
}

thread_local! {
    /// Stashed oneshot reply sender for the current `SendInput` command.
    /// The session loop uses this to reply after the next execution cycle.
    static PENDING_REPLY: RefCell<Option<oneshot::Sender<SessionResponse>>> = const { RefCell::new(None) };
}

/// Reply to the pending `SendInput` command with a response.
fn reply_pending(response: SessionResponse) {
    PENDING_REPLY.with(|cell| {
        if let Some(reply) = cell.borrow_mut().take() {
            let _ = reply.send(response);
        }
    });
}

/// Drain the command channel, replying with an end message to any pending
/// callers, then return.
async fn drain_with_end(cmd_rx: &mut mpsc::Receiver<SessionCommand>, end: EndOutput) {
    // Reply to any stashed pending reply first.
    reply_pending(SessionResponse::End(end.clone()));

    // Drain remaining commands.
    while let Ok(cmd) = cmd_rx.try_recv() {
        match cmd {
            SessionCommand::SendInput { reply, .. } | SessionCommand::GetScreen { reply } => {
                let _ = reply.send(SessionResponse::End(end.clone()));
            }
            SessionCommand::Shutdown => break,
        }
    }
}

/// Drain the command channel, replying with an error to any pending callers.
async fn drain_with_error(cmd_rx: &mut mpsc::Receiver<SessionCommand>, msg: &str) {
    reply_pending(SessionResponse::Error(msg.to_string()));

    while let Ok(cmd) = cmd_rx.try_recv() {
        match cmd {
            SessionCommand::SendInput { reply, .. } | SessionCommand::GetScreen { reply } => {
                let _ = reply.send(SessionResponse::Error(msg.to_string()));
            }
            SessionCommand::Shutdown => break,
        }
    }
}

/// The main loop running on the dedicated session thread.
///
/// Structurally mirrors `run_cics_headless()` in the CLI binary, but uses
/// channels instead of stdin/stdout for I/O.
async fn session_thread_main(config: CicsAppConfig, mut cmd_rx: mpsc::Receiver<SessionCommand>) {
    let search_dirs = build_search_dirs(&config);

    // -- Bootstrap: compile initial program --
    let initial_program = match compile_program(&config.initial_program, &config.include_paths) {
        Ok(p) => p,
        Err(e) => {
            let msg = format!("Compilation failed: {e}");
            error!("{msg}");
            drain_with_error(&mut cmd_rx, &msg).await;
            return;
        }
    };
    let initial_name = initial_program.name.clone();

    // -- Create Session + CicsBridge --
    let session_config = SessionConfig {
        initial_program: config.initial_program.clone(),
        include_paths: config.include_paths.clone(),
        data_files: config.data_files.clone(),
        program_dir: config.program_dir.clone(),
        transid_map: config.transid_map.clone(),
        color_theme: "classic".to_string(),
        userid: Some(config.userid.clone()),
        initial_transid: None,
        terminal_model: TerminalModel::Model2,
    };

    let session = Session::new(session_config);
    let session_rc = Rc::new(RefCell::new(session));
    let bridge = Rc::new(RefCell::new(CicsBridge::new(Rc::clone(&session_rc))));

    // Load BMS maps
    if let Some(ref bms_dir) = config.bms_dir {
        bridge
            .borrow_mut()
            .load_bms_maps(bms_dir, &config.include_paths);
    }

    // Load data files
    bridge.borrow_mut().load_data_files(&config.data_files);

    // Register initial program
    bridge
        .borrow_mut()
        .registry_mut()
        .register(&initial_name, |_| {
            Ok(open_mainframe_cics::runtime::ProgramResult::Return)
        });

    info!(
        program = %initial_name,
        userid = %config.userid,
        "CICS execution session started"
    );

    // -- Execution loop --
    let mut current_program = initial_program;
    let mut current_name = initial_name;
    let mut commarea: Option<Vec<u8>> = None;
    let mut pending_aid: Option<u8> = None;

    loop {
        let mut env = setup_env(&bridge, &commarea, &mut pending_aid);

        let handler = BridgeHandler {
            inner: Rc::clone(&bridge),
        };
        env.cics_handler = Some(Box::new(handler));

        session_rc.borrow_mut().set_program(&current_name);

        // Execute the COBOL program
        let exec_result = interpreter::execute(&current_program, &mut env);
        let action = bridge.borrow_mut().take_pending_action();

        match &exec_result {
            Ok(_) => {}
            Err(e) if action.is_some() => {
                // Stopped by a CICS command (XCTL, RETURN, etc.) — expected.
                let _ = e;
            }
            Err(e) => {
                let end = EndOutput {
                    kind: "error".to_string(),
                    reason: "error".to_string(),
                    program: current_name.clone(),
                    message: Some(format!("Error in {}: {}", current_name, e.message)),
                };
                error!(program = %current_name, error = %e.message, "COBOL execution error");
                drain_with_end(&mut cmd_rx, end).await;
                return;
            }
        }

        match action {
            Some(BridgeAction::Xctl {
                program,
                commarea: ca,
            }) => {
                commarea = ca;
                let prog_upper = program.to_uppercase();
                match compile_and_register(
                    &prog_upper,
                    &search_dirs,
                    &config.include_paths,
                    &bridge,
                ) {
                    Ok((prog, name)) => {
                        current_program = prog;
                        current_name = name;
                        continue;
                    }
                    Err(e) => {
                        let end = EndOutput {
                            kind: "error".to_string(),
                            reason: "pgmiderr".to_string(),
                            program: prog_upper,
                            message: Some(e),
                        };
                        drain_with_end(&mut cmd_rx, end).await;
                        return;
                    }
                }
            }
            Some(BridgeAction::Return {
                transid,
                commarea: ca,
            }) => {
                commarea = ca;

                if let Some(tid) = transid {
                    let prog_name = config.transid_map.get(&tid.to_uppercase()).cloned();
                    if let Some(prog_name) = prog_name {
                        // Capture the current screen and wait for input.
                        let screen = {
                            let sess = session_rc.borrow();
                            headless::capture_screen(&sess)
                        };

                        match wait_for_input_via_channel(&mut cmd_rx, screen).await {
                            Some((aid_byte, fields)) => {
                                pending_aid = Some(aid_byte);
                                bridge.borrow_mut().set_pending_input(aid_byte, fields);
                            }
                            None => return, // channel closed or Shutdown
                        }

                        session_rc.borrow_mut().set_transid(&tid);

                        match compile_and_register(
                            &prog_name,
                            &search_dirs,
                            &config.include_paths,
                            &bridge,
                        ) {
                            Ok((prog, name)) => {
                                current_program = prog;
                                current_name = name;
                                bridge
                                    .borrow_mut()
                                    .runtime_mut()
                                    .eib
                                    .set_transaction_id(&tid);
                                continue;
                            }
                            Err(e) => {
                                let end = EndOutput {
                                    kind: "error".to_string(),
                                    reason: "pgmiderr".to_string(),
                                    program: prog_name,
                                    message: Some(e),
                                };
                                drain_with_end(&mut cmd_rx, end).await;
                                return;
                            }
                        }
                    } else {
                        let end = EndOutput {
                            kind: "error".to_string(),
                            reason: "unknown_transid".to_string(),
                            program: current_name.clone(),
                            message: Some(format!("Unknown TRANSID: {tid}")),
                        };
                        drain_with_end(&mut cmd_rx, end).await;
                        return;
                    }
                } else {
                    // RETURN without TRANSID — session ends.
                    let end = EndOutput {
                        kind: "end".to_string(),
                        reason: "return".to_string(),
                        program: current_name.clone(),
                        message: None,
                    };
                    drain_with_end(&mut cmd_rx, end).await;
                    return;
                }
            }
            Some(BridgeAction::Abend { code }) => {
                let end = EndOutput {
                    kind: "error".to_string(),
                    reason: "abend".to_string(),
                    program: current_name.clone(),
                    message: Some(format!("ABEND: {code}")),
                };
                drain_with_end(&mut cmd_rx, end).await;
                return;
            }
            None => {
                // Program ended normally without a CICS action.
                let end = EndOutput {
                    kind: "end".to_string(),
                    reason: "normal".to_string(),
                    program: current_name.clone(),
                    message: None,
                };
                drain_with_end(&mut cmd_rx, end).await;
                return;
            }
        }
    }
}
