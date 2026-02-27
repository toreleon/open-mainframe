//! OpenMainframe CLI — COBOL interpreter with CICS TUI.
//!
//! Provides the `cics` subcommand that runs an interactive CICS terminal session
//! with a 3270 TUI, interpreting COBOL programs with EXEC CICS support.
//! Pass `--headless` to use a JSON protocol on stdin/stdout instead.

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

use clap::{Parser, Subcommand};
use tracing::{debug, info, error};

use open_mainframe_runtime::interpreter::{self, SimpleProgram};
use open_mainframe_tui::event::CrosstermEventSource;
use open_mainframe_tui::session::{Session, SessionConfig, TerminalModel};

use open_mainframe_lib::bridge::{BridgeAction, CicsBridge};
use open_mainframe_lib::{headless, compile_program, find_program_source, setup_env, BridgeHandler};

/// OpenMainframe CLI.
#[derive(Parser)]
#[command(name = "open-mainframe", about = "OpenMainframe — COBOL tools for z/OS")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run an interactive CICS terminal session (3270 TUI).
    Cics(CicsArgs),
}

#[derive(Parser)]
struct CicsArgs {
    /// Input COBOL source file (initial program).
    file: PathBuf,

    /// Additional copybook search paths.
    #[arg(short = 'I', long = "include")]
    include: Vec<PathBuf>,

    /// Enable verbose output.
    #[arg(short, long)]
    verbose: bool,

    /// VSAM data files to load (format: DDNAME=path[:key_len[:rec_len]]).
    #[arg(long = "data")]
    data: Vec<String>,

    /// Directory containing BMS map source files.
    #[arg(long = "bms-dir")]
    bms_dir: Option<PathBuf>,

    /// Color theme (classic, modern, mono).
    #[arg(long = "theme", default_value = "classic")]
    theme: String,

    /// Transaction-to-program mappings (format: TRANSID=PROGRAM).
    #[arg(long = "transid")]
    transid: Vec<String>,

    /// Directory to search for XCTL target programs.
    #[arg(long = "program-dir")]
    program_dir: Option<PathBuf>,

    /// Run in headless mode (JSON protocol on stdin/stdout, no TUI).
    #[arg(long)]
    headless: bool,
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Cics(args) => {
            let headless = args.headless;

            // Install panic hook that restores the terminal (TUI mode only).
            if !headless {
                let default_hook = std::panic::take_hook();
                std::panic::set_hook(Box::new(move |info| {
                    let _ = open_mainframe_tui::session::restore_terminal();
                    default_hook(info);
                }));
            }

            // Set up tracing: write to a log file so it doesn't corrupt the TUI.
            // Enabled by --verbose or RUST_LOG env var.
            let verbose = args.verbose;
            if verbose || std::env::var("RUST_LOG").is_ok() {
                let log_file = std::fs::File::create("open-mainframe.log")
                    .expect("failed to create log file");
                let filter = if std::env::var("RUST_LOG").is_ok() {
                    tracing_subscriber::EnvFilter::from_default_env()
                } else {
                    tracing_subscriber::EnvFilter::new("debug")
                };
                tracing_subscriber::fmt()
                    .with_env_filter(filter)
                    .with_writer(log_file)
                    .with_ansi(false)
                    .init();
            }

            if let Err(e) = run_cics(args) {
                if headless {
                    headless::print_json(&headless::EndOutput {
                        kind: "error".to_string(),
                        reason: "error".to_string(),
                        program: String::new(),
                        message: Some(e.clone()),
                    });
                }
                eprintln!("Error: {e}");
                if !verbose {
                    eprintln!("Run with --verbose for details in open-mainframe.log");
                }
                std::process::exit(1);
            }
        }
    }
}

/// Run the CICS session (TUI or headless, depending on args).
fn run_cics(args: CicsArgs) -> Result<(), String> {
    // Parse transid mappings
    let mut transid_map = HashMap::new();
    for mapping in &args.transid {
        if let Some((tid, prog)) = mapping.split_once('=') {
            transid_map.insert(tid.to_uppercase(), prog.to_uppercase());
        }
    }

    // Determine program search directories
    let mut search_dirs = Vec::new();
    if let Some(ref pd) = args.program_dir {
        search_dirs.push(pd.clone());
    }
    // Always include the initial program's parent directory
    if let Some(parent) = args.file.parent() {
        search_dirs.push(parent.to_path_buf());
    }

    // Compile initial program
    let initial_program = compile_program(&args.file, &args.include)?;
    let initial_name = initial_program.name.clone();

    // Create session config
    let config = SessionConfig {
        initial_program: args.file.clone(),
        include_paths: args.include.clone(),
        data_files: args.data.clone(),
        program_dir: args.program_dir.clone(),
        transid_map: transid_map.clone(),
        color_theme: args.theme.clone(),
        userid: None,
        initial_transid: None,
        terminal_model: TerminalModel::Model2,
    };

    let session = Session::new(config);
    let session_rc = Rc::new(RefCell::new(session));

    // Create bridge (no longer takes terminal/events — I/O is the main loop's job).
    let bridge = Rc::new(RefCell::new(CicsBridge::new(
        Rc::clone(&session_rc),
    )));

    // Load BMS maps
    if let Some(ref bms_dir) = args.bms_dir {
        bridge.borrow_mut().load_bms_maps(bms_dir, &args.include);
    }

    // Load data files
    bridge.borrow_mut().load_data_files(&args.data);

    // Register the initial program in the CICS ProgramRegistry
    bridge.borrow_mut().registry_mut().register(&initial_name, |_| {
        Ok(open_mainframe_cics::runtime::ProgramResult::Return)
    });

    if args.headless {
        run_cics_headless(
            &args, &transid_map, &search_dirs,
            initial_program, initial_name,
            session_rc, bridge,
        )
    } else {
        run_cics_tui(
            &args, &transid_map, &search_dirs,
            initial_program, initial_name,
            session_rc, bridge,
        )
    }
}

// ---------------------------------------------------------------------------
// TUI mode (interactive terminal)
// ---------------------------------------------------------------------------

fn run_cics_tui(
    args: &CicsArgs,
    transid_map: &HashMap<String, String>,
    search_dirs: &[PathBuf],
    initial_program: SimpleProgram,
    initial_name: String,
    session_rc: Rc<RefCell<Session>>,
    bridge: Rc<RefCell<CicsBridge>>,
) -> Result<(), String> {
    // Set up TUI terminal and event source
    let terminal = open_mainframe_tui::session::setup_terminal()
        .map_err(|e| format!("Failed to set up terminal: {e}"))?;
    let terminal_rc = Rc::new(RefCell::new(terminal));
    let events: Rc<RefCell<dyn open_mainframe_tui::event::EventSource>> =
        Rc::new(RefCell::new(CrosstermEventSource));

    // Cleanup guard for terminal restore
    struct TerminalGuard;
    impl Drop for TerminalGuard {
        fn drop(&mut self) {
            let _ = open_mainframe_tui::session::restore_terminal();
        }
    }
    let _guard = TerminalGuard;

    let mut current_program = initial_program;
    let mut current_name = initial_name;
    let mut commarea: Option<Vec<u8>> = None;
    let mut pending_aid: Option<u8> = None;

    loop {
        let mut env = setup_env(&bridge, &commarea, &mut pending_aid);

        // Install the bridge as the CICS command handler
        let handler = BridgeHandler {
            inner: Rc::clone(&bridge),
        };
        env.cics_handler = Some(Box::new(handler));

        // Update session state
        session_rc.borrow_mut().set_program(&current_name);

        // Execute the program
        info!(program = %current_name, "Executing program");
        let exec_result = interpreter::execute(&current_program, &mut env);
        let action = bridge.borrow_mut().take_pending_action();

        match &exec_result {
            Ok(_) => debug!(program = %current_name, "Program completed normally"),
            Err(e) if action.is_some() => {
                debug!(program = %current_name, via = %e.message, "Program stopped by CICS command");
            }
            Err(e) => {
                error!(program = %current_name, error = %e.message, "Program execution error");
                session_rc.borrow_mut().set_message(
                    &format!("Error in {}: {}", current_name, e.message),
                );
                break;
            }
        }

        debug!(?action, "Pending action");

        match action {
            Some(BridgeAction::Xctl { program, commarea: ca }) => {
                commarea = ca;
                let prog_upper = program.to_uppercase();

                if let Some(source_path) = find_program_source(&prog_upper, search_dirs) {
                    match compile_program(&source_path, &args.include) {
                        Ok(prog) => {
                            let name = prog.name.clone();
                            if !bridge.borrow().registry().exists(&prog_upper) {
                                bridge.borrow_mut().registry_mut().register(&name, |_| {
                                    Ok(open_mainframe_cics::runtime::ProgramResult::Return)
                                });
                            }
                            current_program = prog;
                            current_name = name;
                            continue;
                        }
                        Err(e) => {
                            session_rc.borrow_mut().set_message(&format!("PGMIDERR: {e}"));
                            break;
                        }
                    }
                } else {
                    session_rc.borrow_mut().set_message(
                        &format!("PGMIDERR: Program {} not found", prog_upper),
                    );
                    break;
                }
            }
            Some(BridgeAction::Return { transid, commarea: ca }) => {
                commarea = ca;

                if let Some(tid) = transid {
                    let prog_name = transid_map.get(&tid.to_uppercase()).cloned();
                    if let Some(prog_name) = prog_name {
                        // Wait for user input (pseudo-conversational)
                        {
                            let mut sess = session_rc.borrow_mut();
                            sess.set_transid(&tid);
                            let mut term = terminal_rc.borrow_mut();
                            let mut evts = events.borrow_mut();
                            match sess.wait_for_input(&mut *term, &mut *evts) {
                                Ok((aid, fields)) => {
                                    pending_aid = Some(aid);
                                    bridge.borrow_mut().set_pending_input(aid, fields);
                                }
                                Err(open_mainframe_tui::error::SessionError::Interrupted) => {
                                    break; // User pressed Ctrl+C
                                }
                                Err(e) => {
                                    error!(%e, "Input error");
                                    break;
                                }
                            }
                        }

                        if let Some(source_path) = find_program_source(&prog_name, search_dirs) {
                            match compile_program(&source_path, &args.include) {
                                Ok(prog) => {
                                    if !bridge.borrow().registry().exists(&prog_name) {
                                        bridge.borrow_mut().registry_mut().register(&prog_name, |_| {
                                            Ok(open_mainframe_cics::runtime::ProgramResult::Return)
                                        });
                                    }
                                    current_program = prog;
                                    current_name = prog_name;
                                    bridge.borrow_mut().runtime_mut().eib.set_transaction_id(&tid);
                                    continue;
                                }
                                Err(e) => {
                                    session_rc.borrow_mut().set_message(&format!("PGMIDERR: {e}"));
                                    break;
                                }
                            }
                        } else {
                            session_rc.borrow_mut().set_message(
                                &format!("PGMIDERR: Program {} not found", prog_name),
                            );
                            break;
                        }
                    } else {
                        session_rc.borrow_mut().set_message(
                            &format!("Unknown TRANSID: {tid}"),
                        );
                        break;
                    }
                } else {
                    // RETURN without TRANSID — session ends
                    break;
                }
            }
            Some(BridgeAction::Abend { code }) => {
                session_rc.borrow_mut().set_message(&format!("ABEND: {code}"));
                break;
            }
            None => {
                // Program ended normally (STOP RUN or fell through)
                break;
            }
        }
    }

    // Terminal is restored by the TerminalGuard drop
    Ok(())
}

// ---------------------------------------------------------------------------
// Headless mode (JSON on stdin/stdout)
// ---------------------------------------------------------------------------

fn run_cics_headless(
    args: &CicsArgs,
    transid_map: &HashMap<String, String>,
    search_dirs: &[PathBuf],
    initial_program: SimpleProgram,
    initial_name: String,
    session_rc: Rc<RefCell<Session>>,
    bridge: Rc<RefCell<CicsBridge>>,
) -> Result<(), String> {
    let mut current_program = initial_program;
    let mut current_name = initial_name;
    let mut commarea: Option<Vec<u8>> = None;
    let mut pending_aid: Option<u8> = None;

    loop {
        let mut env = setup_env(&bridge, &commarea, &mut pending_aid);

        // Install the bridge as the CICS command handler
        let handler = BridgeHandler {
            inner: Rc::clone(&bridge),
        };
        env.cics_handler = Some(Box::new(handler));

        // Update session state
        session_rc.borrow_mut().set_program(&current_name);

        // Execute the program
        info!(program = %current_name, "Executing program (headless)");
        let exec_result = interpreter::execute(&current_program, &mut env);
        let action = bridge.borrow_mut().take_pending_action();

        match &exec_result {
            Ok(_) => debug!(program = %current_name, "Program completed normally"),
            Err(e) if action.is_some() => {
                debug!(program = %current_name, via = %e.message, "Program stopped by CICS command");
            }
            Err(e) => {
                error!(program = %current_name, error = %e.message, "Program execution error");
                headless::print_json(&headless::EndOutput {
                    kind: "error".to_string(),
                    reason: "error".to_string(),
                    program: current_name.clone(),
                    message: Some(format!("Error in {}: {}", current_name, e.message)),
                });
                return Ok(());
            }
        }

        debug!(?action, "Pending action");

        match action {
            Some(BridgeAction::Xctl { program, commarea: ca }) => {
                commarea = ca;
                let prog_upper = program.to_uppercase();

                if let Some(source_path) = find_program_source(&prog_upper, search_dirs) {
                    match compile_program(&source_path, &args.include) {
                        Ok(prog) => {
                            let name = prog.name.clone();
                            if !bridge.borrow().registry().exists(&prog_upper) {
                                bridge.borrow_mut().registry_mut().register(&name, |_| {
                                    Ok(open_mainframe_cics::runtime::ProgramResult::Return)
                                });
                            }
                            current_program = prog;
                            current_name = name;
                            continue;
                        }
                        Err(e) => {
                            headless::print_json(&headless::EndOutput {
                                kind: "error".to_string(),
                                reason: "pgmiderr".to_string(),
                                program: prog_upper,
                                message: Some(e),
                            });
                            return Ok(());
                        }
                    }
                } else {
                    headless::print_json(&headless::EndOutput {
                        kind: "error".to_string(),
                        reason: "pgmiderr".to_string(),
                        program: prog_upper.clone(),
                        message: Some(format!("Program {} not found", prog_upper)),
                    });
                    return Ok(());
                }
            }
            Some(BridgeAction::Return { transid, commarea: ca }) => {
                commarea = ca;

                if let Some(tid) = transid {
                    let prog_name = transid_map.get(&tid.to_uppercase()).cloned();
                    if let Some(prog_name) = prog_name {
                        // Emit current screen as JSON
                        {
                            let sess = session_rc.borrow();
                            let screen = headless::capture_screen(&sess);
                            headless::print_json(&screen);
                        }

                        // Read input from stdin
                        let input = headless::read_headless_input()
                            .map_err(|e| format!("Headless input error: {e}"))?;
                        let (aid, fields) = headless::input_to_pending(&input)
                            .map_err(|e| format!("Invalid headless input: {e}"))?;

                        pending_aid = Some(aid);
                        bridge.borrow_mut().set_pending_input(aid, fields);

                        session_rc.borrow_mut().set_transid(&tid);

                        if let Some(source_path) = find_program_source(&prog_name, search_dirs) {
                            match compile_program(&source_path, &args.include) {
                                Ok(prog) => {
                                    if !bridge.borrow().registry().exists(&prog_name) {
                                        bridge.borrow_mut().registry_mut().register(&prog_name, |_| {
                                            Ok(open_mainframe_cics::runtime::ProgramResult::Return)
                                        });
                                    }
                                    current_program = prog;
                                    current_name = prog_name;
                                    bridge.borrow_mut().runtime_mut().eib.set_transaction_id(&tid);
                                    continue;
                                }
                                Err(e) => {
                                    headless::print_json(&headless::EndOutput {
                                        kind: "error".to_string(),
                                        reason: "pgmiderr".to_string(),
                                        program: prog_name,
                                        message: Some(e),
                                    });
                                    return Ok(());
                                }
                            }
                        } else {
                            headless::print_json(&headless::EndOutput {
                                kind: "error".to_string(),
                                reason: "pgmiderr".to_string(),
                                program: prog_name.clone(),
                                message: Some(format!("Program {} not found", prog_name)),
                            });
                            return Ok(());
                        }
                    } else {
                        headless::print_json(&headless::EndOutput {
                            kind: "error".to_string(),
                            reason: "unknown_transid".to_string(),
                            program: current_name.clone(),
                            message: Some(format!("Unknown TRANSID: {tid}")),
                        });
                        return Ok(());
                    }
                } else {
                    // RETURN without TRANSID — session ends
                    headless::print_json(&headless::EndOutput {
                        kind: "end".to_string(),
                        reason: "return".to_string(),
                        program: current_name.clone(),
                        message: None,
                    });
                    return Ok(());
                }
            }
            Some(BridgeAction::Abend { code }) => {
                headless::print_json(&headless::EndOutput {
                    kind: "error".to_string(),
                    reason: "abend".to_string(),
                    program: current_name.clone(),
                    message: Some(format!("ABEND: {code}")),
                });
                return Ok(());
            }
            None => {
                // Program ended normally
                headless::print_json(&headless::EndOutput {
                    kind: "end".to_string(),
                    reason: "normal".to_string(),
                    program: current_name.clone(),
                    message: None,
                });
                return Ok(());
            }
        }
    }
}

