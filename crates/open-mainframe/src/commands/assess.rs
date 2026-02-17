//! Assess command implementation — migration assessment for COBOL source files.
//!
//! Provides two subcommands:
//! - `scan`: Scan a directory for COBOL files and produce an assessment report
//! - `file`: Assess a single COBOL file

use std::path::PathBuf;

use clap::Subcommand;
use miette::Result;

use crate::output::{print_json, OutputFormat};

/// Assessment subcommands.
#[derive(Subcommand, Debug)]
pub enum AssessCommand {
    /// Scan a directory for COBOL files and produce an assessment report
    Scan {
        /// Directory to scan for COBOL source files
        #[arg(value_name = "DIR")]
        directory: PathBuf,

        /// Additional copybook search paths
        #[arg(short = 'I', long = "include", value_name = "DIR", action = clap::ArgAction::Append)]
        include_paths: Vec<PathBuf>,

        /// Only include files matching this glob pattern (e.g., "src/**/*.cbl")
        #[arg(long, value_name = "PATTERN")]
        pattern: Option<String>,

        /// Do not recurse into subdirectories
        #[arg(long)]
        no_recursive: bool,
    },

    /// Assess a single COBOL file
    File {
        /// Path to a COBOL source file
        #[arg(value_name = "FILE")]
        path: PathBuf,
    },
}

/// Run the assess command.
pub fn run(command: AssessCommand, format: OutputFormat) -> Result<()> {
    match command {
        AssessCommand::Scan {
            directory,
            include_paths,
            pattern,
            no_recursive,
        } => run_scan(directory, include_paths, pattern, no_recursive, format),
        AssessCommand::File { path } => run_file(path, format),
    }
}

/// Scan a directory for COBOL files and produce an assessment report.
fn run_scan(
    directory: PathBuf,
    include_paths: Vec<PathBuf>,
    pattern: Option<String>,
    no_recursive: bool,
    format: OutputFormat,
) -> Result<()> {
    if !directory.exists() {
        return Err(miette::miette!(
            "Directory not found: {}",
            directory.display()
        ));
    }

    if !directory.is_dir() {
        return Err(miette::miette!(
            "Path is not a directory: {}",
            directory.display()
        ));
    }

    tracing::info!("Scanning directory: {}", directory.display());

    let mut config = open_mainframe_assess::ScanConfig::new(&directory);

    // Add copybook search paths
    for path in &include_paths {
        config = config.with_copybook_path(path);
        tracing::debug!("Added copybook path: {}", path.display());
    }

    // Add glob pattern filter
    if let Some(ref pat) = pattern {
        config = config.with_pattern(pat);
    }

    // Set recursion
    if no_recursive {
        config = config.with_recursive(false);
    }

    let scanner = open_mainframe_assess::Scanner::new(config);
    let result = scanner.scan().map_err(|e| miette::miette!("Scan failed: {}", e))?;

    if format.is_json() {
        let json = result.report.generate(open_mainframe_assess::ReportFormat::Json);
        println!("{}", json);
    } else {
        // Print scan summary
        println!("Assessment Scan: {}", directory.display());
        println!();
        println!(
            "Files discovered: {}",
            result.files_discovered
        );
        println!(
            "Files analyzed:   {}",
            result.files_analyzed
        );
        println!(
            "Files failed:     {}",
            result.files_failed
        );

        if !result.warnings.is_empty() {
            println!();
            println!("Warnings:");
            for warning in &result.warnings {
                println!("  - {}", warning);
            }
        }

        println!();
        println!(
            "{}",
            result.report.generate(open_mainframe_assess::ReportFormat::Text)
        );
    }

    Ok(())
}

/// Assess a single COBOL source file.
fn run_file(path: PathBuf, format: OutputFormat) -> Result<()> {
    if !path.exists() {
        return Err(miette::miette!("File not found: {}", path.display()));
    }

    tracing::info!("Assessing file: {}", path.display());

    let source = std::fs::read_to_string(&path)
        .map_err(|e| miette::miette!("Failed to read file {}: {}", path.display(), e))?;

    let file_name = path
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_else(|| path.display().to_string());

    let analyzer = open_mainframe_assess::Analyzer::new();
    let result = analyzer
        .analyze(&source, &file_name)
        .map_err(|e| miette::miette!("Analysis failed: {}", e))?;

    if format.is_json() {
        print_json(&result);
    } else {
        println!("{}", result.summary());

        // Features
        if !result.features.is_empty() {
            println!("Features Detected:");
            for feature in &result.features {
                println!(
                    "  - {} ({:?}): {} occurrence(s)",
                    feature.name, feature.category, feature.count
                );
            }
            println!();
        }

        // Issues
        if !result.issues.is_empty() {
            println!("Compatibility Issues:");
            for issue in &result.issues {
                println!("  [{}] {} - {}", issue.severity.name(), issue.code, issue.description);
                if !issue.recommendation.is_empty() {
                    println!("    Recommendation: {}", issue.recommendation);
                }
            }
            println!();
        }

        // Recommendations
        if !result.recommendations.is_empty() {
            println!("Recommendations:");
            for rec in &result.recommendations {
                println!("  - {}", rec);
            }
        }
    }

    Ok(())
}
