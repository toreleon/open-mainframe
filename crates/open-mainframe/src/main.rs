//! CLI for the OpenMainframe mainframe emulator.
//!
//! This is the main entry point for OpenMainframe, providing commands to
//! compile COBOL programs and execute JCL jobs.
//!
//! # Examples
//!
//! ```bash
//! # Compile a COBOL program
//! open-mainframe compile program.cbl -o program
//!
//! # Run a JCL job
//! open-mainframe run job.jcl
//!
//! # Parse and check COBOL syntax
//! open-mainframe check program.cbl
//! ```

use std::path::PathBuf;

use clap::{CommandFactory, Parser, Subcommand};
use miette::Result;

mod commands;
pub mod config;
mod error;

pub use config::Config;
pub use error::CliError;

#[derive(Parser, Debug)]
#[command(name = "open-mainframe")]
#[command(author, version, about = "OpenMainframe mainframe emulator", long_about = None)]
struct Cli {
    /// Enable verbose output
    #[arg(short, long, global = true)]
    verbose: bool,

    #[command(subcommand)]
    command: Commands,
}

/// Configuration subcommands.
#[derive(Subcommand, Debug)]
enum ConfigAction {
    /// Show current configuration
    Show,
    /// Generate a default configuration file
    Init {
        /// Output path for config file
        #[arg(short, long, default_value = "open-mainframe.toml")]
        output: PathBuf,
    },
    /// Show configuration file paths
    Paths,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Compile a COBOL source file
    Compile {
        /// Input COBOL source file
        #[arg(value_name = "FILE")]
        input: PathBuf,

        /// Output file path
        #[arg(short, long, value_name = "FILE")]
        output: Option<PathBuf>,

        /// Optimization level (0-3)
        #[arg(short = 'O', long, default_value = "0", value_parser = clap::value_parser!(u8).range(0..=3))]
        optimize: u8,

        /// Emit assembly instead of object file
        #[arg(long)]
        emit_asm: bool,

        /// Emit LLVM IR instead of object file
        #[arg(long)]
        emit_llvm: bool,

        /// Additional copybook search paths
        #[arg(short = 'I', long = "include", value_name = "DIR")]
        include_paths: Vec<PathBuf>,
    },

    /// Run a JCL job
    Run {
        /// Input JCL file
        #[arg(value_name = "FILE")]
        input: PathBuf,

        /// Directory containing compiled programs
        #[arg(long, value_name = "DIR")]
        program_dir: Option<PathBuf>,

        /// Directory for dataset files
        #[arg(long, value_name = "DIR")]
        dataset_dir: Option<PathBuf>,

        /// Working directory for job execution
        #[arg(long, value_name = "DIR")]
        work_dir: Option<PathBuf>,
    },

    /// Check COBOL syntax without compiling
    Check {
        /// Input COBOL source file
        #[arg(value_name = "FILE")]
        input: PathBuf,

        /// Additional copybook search paths
        #[arg(short = 'I', long = "include", value_name = "DIR")]
        include_paths: Vec<PathBuf>,
    },

    /// Parse JCL and show the job structure
    ParseJcl {
        /// Input JCL file
        #[arg(value_name = "FILE")]
        input: PathBuf,
    },

    /// Show COBOL tokens (for debugging)
    Lex {
        /// Input COBOL source file
        #[arg(value_name = "FILE")]
        input: PathBuf,

        /// Source format (fixed, free, or auto)
        #[arg(long, default_value = "auto")]
        format: String,
    },

    /// Interpret a COBOL program (run without compiling)
    Interpret {
        /// Input COBOL source file
        #[arg(value_name = "FILE")]
        input: PathBuf,

        /// Additional copybook search paths
        #[arg(short = 'I', long = "include", value_name = "DIR")]
        include_paths: Vec<PathBuf>,

        /// VSAM data files to load (format: DDNAME=path[:key_len[:rec_len]])
        #[arg(long = "data", value_name = "DDNAME=FILE")]
        data_files: Vec<String>,

        /// Set CICS EIB COMMAREA length (simulates returning from a pseudo-conversation)
        #[arg(long)]
        eibcalen: Option<i64>,

        /// Set CICS EIB AID key (e.g., ENTER, PF3, PF7)
        #[arg(long)]
        eibaid: Option<String>,

        /// Set CICS variables before execution (format: NAME=VALUE)
        #[arg(long = "set", value_name = "NAME=VALUE")]
        set_vars: Vec<String>,
    },

    /// Manage Generation Data Groups (GDG)
    Gdg {
        #[command(subcommand)]
        action: commands::gdg::GdgAction,
    },

    /// Run IDCAMS commands for dataset management
    Idcams {
        #[command(subcommand)]
        action: commands::idcams::IdcamsAction,
    },

    /// Compile BMS map definitions into COBOL copybooks
    Bms {
        /// Input BMS source file (or directory with --all)
        #[arg(value_name = "FILE")]
        input: PathBuf,

        /// Output directory for generated copybooks
        #[arg(short, long, value_name = "DIR")]
        output: Option<PathBuf>,

        /// Compile all .bms files in the input directory
        #[arg(long)]
        all: bool,
    },

    /// Run an interactive CICS terminal session (3270 TUI)
    Cics {
        /// Input COBOL source file (initial program)
        #[arg(value_name = "FILE")]
        input: PathBuf,

        /// Additional copybook search paths
        #[arg(short = 'I', long = "include", value_name = "DIR")]
        include_paths: Vec<PathBuf>,

        /// VSAM data files to load (format: DDNAME=path[:key_len[:rec_len]])
        #[arg(long = "data", value_name = "DDNAME=FILE")]
        data_files: Vec<String>,

        /// Directory containing BMS map source files
        #[arg(long = "bms-dir", value_name = "DIR")]
        bms_dir: Option<PathBuf>,

        /// Color theme (classic, modern, mono)
        #[arg(long, default_value = "classic")]
        theme: String,

        /// Transaction-to-program mappings (format: TRANSID=PROGRAM)
        #[arg(long = "transid", value_name = "TRANSID=PROGRAM")]
        transid_map: Vec<String>,
    },

    /// DB2 SQL preprocessing and utilities
    Db2(commands::db2::Db2Args),

    /// Manage configuration
    Config {
        #[command(subcommand)]
        action: ConfigAction,
    },

    /// Generate shell completions
    Completions {
        /// Shell to generate completions for
        #[arg(value_enum)]
        shell: clap_complete::Shell,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize tracing — for CICS TUI mode, redirect logs to a file so they
    // don't corrupt the terminal display.
    let filter = if cli.verbose { "debug" } else { "info" };
    let env_filter = tracing_subscriber::EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new(filter));

    let is_tui = matches!(cli.command, Commands::Cics { .. });
    if is_tui {
        let log_path = std::env::temp_dir().join("open-mainframe-cics.log");
        if let Ok(log_file) = std::fs::File::create(&log_path) {
            tracing_subscriber::fmt()
                .with_env_filter(env_filter)
                .with_writer(std::sync::Mutex::new(log_file))
                .with_ansi(false)
                .init();
        } else {
            // Fall back to stderr if we can't create the log file
            tracing_subscriber::fmt()
                .with_env_filter(env_filter)
                .init();
        }
    } else {
        tracing_subscriber::fmt()
            .with_env_filter(env_filter)
            .init();
    }

    match cli.command {
        Commands::Compile {
            input,
            output,
            optimize,
            emit_asm,
            emit_llvm,
            include_paths,
        } => commands::compile::run(input, output, optimize, emit_asm, emit_llvm, include_paths),
        Commands::Run {
            input,
            program_dir,
            dataset_dir,
            work_dir,
        } => commands::run::run(input, program_dir, dataset_dir, work_dir),
        Commands::Check {
            input,
            include_paths,
        } => commands::check::run(input, include_paths),
        Commands::ParseJcl { input } => commands::parse_jcl::run(input),
        Commands::Lex { input, format } => commands::lex::run(input, format),
        Commands::Interpret { input, include_paths, data_files, eibcalen, eibaid, set_vars } => {
            commands::interpret::interpret(input, include_paths, data_files, eibcalen, eibaid, set_vars)
        }
        Commands::Cics { input, include_paths, data_files, bms_dir, theme, transid_map } => {
            commands::cics::run_session(input, include_paths, data_files, bms_dir, theme, transid_map)
        }
        Commands::Gdg { action } => commands::gdg::run(action),
        Commands::Idcams { action } => commands::idcams::run(action),
        Commands::Bms { input, output, all } => {
            if all {
                commands::bms::run_all(input, output)
            } else {
                commands::bms::run(input, output)
            }
        }
        Commands::Db2(args) => commands::db2::execute(args),
        Commands::Config { action } => match action {
            ConfigAction::Show => {
                let config = config::Config::load();
                let toml = toml::to_string_pretty(&config)
                    .map_err(|e| miette::miette!("Failed to serialize config: {}", e))?;
                println!("{}", toml);
                Ok(())
            }
            ConfigAction::Init { output } => {
                let content = config::Config::generate_default();
                std::fs::write(&output, content)
                    .map_err(|e| miette::miette!("Failed to write config: {}", e))?;
                println!("Created configuration file: {}", output.display());
                Ok(())
            }
            ConfigAction::Paths => {
                println!("Configuration file search paths:");
                println!("  1. ./open-mainframe.toml (project config)");
                if let Some(user_path) = config::Config::user_config_path() {
                    println!("  2. {} (user config)", user_path.display());
                }
                println!("\nEnvironment variables:");
                println!("  OPEN_MAINFRAME_SOURCE_FORMAT     - Source format (fixed/free/auto)");
                println!("  OPEN_MAINFRAME_COPYBOOK_PATH     - Colon-separated copybook paths");
                println!("  OPEN_MAINFRAME_OPTIMIZATION      - Optimization level (0-3)");
                println!("  OPEN_MAINFRAME_DATASET_DIR       - Base directory for datasets");
                println!("  OPEN_MAINFRAME_PROGRAM_DIR       - Directory for compiled programs");
                println!("  OPEN_MAINFRAME_WORK_DIR          - Working directory for jobs");
                Ok(())
            }
        },
        Commands::Completions { shell } => {
            let mut cmd = Cli::command();
            clap_complete::generate(shell, &mut cmd, "open-mainframe", &mut std::io::stdout());
            Ok(())
        }
    }
}
