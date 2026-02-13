//! CLI for the zOS-clone mainframe emulator.
//!
//! This is the main entry point for zOS-clone, providing commands to
//! compile COBOL programs and execute JCL jobs.
//!
//! # Examples
//!
//! ```bash
//! # Compile a COBOL program
//! zos-clone compile program.cbl -o program
//!
//! # Run a JCL job
//! zos-clone run job.jcl
//!
//! # Parse and check COBOL syntax
//! zos-clone check program.cbl
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
#[command(name = "zos-clone")]
#[command(author, version, about = "zOS-clone mainframe emulator", long_about = None)]
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
        #[arg(short, long, default_value = "zos-clone.toml")]
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
    },

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

    // Initialize tracing
    let filter = if cli.verbose { "debug" } else { "info" };
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new(filter)),
        )
        .init();

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
        Commands::Interpret { input } => commands::interpret::interpret(input),
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
                println!("  1. ./zos-clone.toml (project config)");
                if let Some(user_path) = config::Config::user_config_path() {
                    println!("  2. {} (user config)", user_path.display());
                }
                println!("\nEnvironment variables:");
                println!("  ZOS_CLONE_SOURCE_FORMAT     - Source format (fixed/free/auto)");
                println!("  ZOS_CLONE_COPYBOOK_PATH     - Colon-separated copybook paths");
                println!("  ZOS_CLONE_OPTIMIZATION      - Optimization level (0-3)");
                println!("  ZOS_CLONE_DATASET_DIR       - Base directory for datasets");
                println!("  ZOS_CLONE_PROGRAM_DIR       - Directory for compiled programs");
                println!("  ZOS_CLONE_WORK_DIR          - Working directory for jobs");
                Ok(())
            }
        },
        Commands::Completions { shell } => {
            let mut cmd = Cli::command();
            clap_complete::generate(shell, &mut cmd, "zos-clone", &mut std::io::stdout());
            Ok(())
        }
    }
}
