//! DB2 command implementations.

use clap::{Args, Subcommand};
use miette::{IntoDiagnostic, Result};
use std::path::{Path, PathBuf};
use zos_db2::preprocess::{Dbrm, SqlPreprocessor};

/// DB2 SQL preprocessing and utilities.
#[derive(Debug, Args)]
pub struct Db2Args {
    #[command(subcommand)]
    pub action: Db2Action,
}

/// DB2 subcommands.
#[derive(Debug, Subcommand)]
pub enum Db2Action {
    /// Preprocess COBOL source to extract EXEC SQL statements.
    Preprocess {
        /// Input COBOL source file.
        #[arg(value_name = "FILE")]
        input: PathBuf,

        /// Output COBOL file (default: input with .cob extension).
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Output DBRM file (default: input with .dbrm extension).
        #[arg(long)]
        dbrm: Option<PathBuf>,

        /// Show listing instead of writing files.
        #[arg(long)]
        listing: bool,
    },
}

/// Execute DB2 command.
pub fn execute(args: Db2Args) -> Result<()> {
    match args.action {
        Db2Action::Preprocess {
            input,
            output,
            dbrm,
            listing,
        } => preprocess(&input, output.as_deref(), dbrm.as_deref(), listing),
    }
}

/// Preprocess COBOL source.
fn preprocess(
    input: &Path,
    output: Option<&Path>,
    dbrm_path: Option<&Path>,
    listing: bool,
) -> Result<()> {
    // Read input file
    let source = std::fs::read_to_string(input).into_diagnostic()?;

    // Extract program name from filename
    let program_name = input
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("UNKNOWN")
        .to_uppercase();

    // Preprocess
    let mut preprocessor = SqlPreprocessor::new();
    let result = preprocessor.process(&source).into_diagnostic()?;

    if listing {
        // Display listing
        println!("DB2 SQL Preprocessor Listing");
        println!("============================");
        println!("Program: {}", program_name);
        println!("Input:   {}", input.display());
        println!();

        if result.sql_statements.is_empty() {
            println!("No EXEC SQL statements found.");
        } else {
            println!("Found {} SQL statement(s):", result.sql_statements.len());
            println!();

            for stmt in &result.sql_statements {
                println!(
                    "Statement {:03} (lines {}-{}) - {:?}",
                    stmt.number, stmt.start_line, stmt.end_line, stmt.stmt_type
                );
                println!("  {}", stmt.sql);
                println!();
            }

            if !result.host_variables.is_empty() {
                println!("Host Variables:");
                for var in &result.host_variables {
                    print!("  {} ({:?})", var.name, var.usage);
                    if let Some(ref ind) = var.indicator {
                        print!(" [indicator: {}]", ind);
                    }
                    println!(" in statement {}", var.statement_number);
                }
            }
        }
    } else {
        // Write output files
        let output_path = output.map(|p| p.to_path_buf()).unwrap_or_else(|| {
            let mut p = input.to_path_buf();
            p.set_extension("cob");
            p
        });

        let dbrm_output = dbrm_path.map(|p| p.to_path_buf()).unwrap_or_else(|| {
            let mut p = input.to_path_buf();
            p.set_extension("dbrm");
            p
        });

        // Write preprocessed COBOL
        std::fs::write(&output_path, &result.cobol_source).into_diagnostic()?;
        println!("Wrote preprocessed COBOL: {}", output_path.display());

        // Write DBRM
        if !result.sql_statements.is_empty() {
            let mut dbrm = Dbrm::new(&program_name);
            dbrm.add_statements(&result.sql_statements, &result.host_variables);
            dbrm.write_to_file(&dbrm_output).into_diagnostic()?;
            println!("Wrote DBRM: {}", dbrm_output.display());
        } else {
            println!("No SQL statements found, DBRM not created.");
        }
    }

    Ok(())
}
