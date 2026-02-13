//! Lex command - show COBOL tokens for debugging.

use std::path::PathBuf;

use miette::{IntoDiagnostic, Result, WrapErr};

/// Run the lex command.
pub fn run(input: PathBuf, format: String) -> Result<()> {
    // Read source file
    let source = std::fs::read_to_string(&input)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read source file: {}", input.display()))?;

    tracing::info!("Lexing {}", input.display());

    // Determine source format
    let source_format = match format.to_lowercase().as_str() {
        "fixed" => zos_cobol::SourceFormat::Fixed,
        "free" => zos_cobol::SourceFormat::Free,
        _ => {
            // Auto-detect: check if first line looks like free format
            if source
                .lines()
                .next()
                .map(|l| !l.starts_with(' '))
                .unwrap_or(false)
            {
                tracing::debug!("Auto-detected: Free format");
                zos_cobol::SourceFormat::Free
            } else {
                tracing::debug!("Auto-detected: Fixed format");
                zos_cobol::SourceFormat::Fixed
            }
        }
    };

    println!("Source format: {:?}", source_format);
    println!();

    // Create source file
    let source_file =
        zos_cobol::SourceFile::from_text(zos_cobol::FileId::MAIN, source, source_format);

    // Scan the source file
    let (tokens, errors) = zos_cobol::scan(&source_file);

    if !errors.is_empty() {
        println!("Lexer errors:");
        for err in &errors {
            println!("  {}", err);
        }
        println!();
    }

    // Print tokens
    println!("Tokens ({} total):", tokens.len());
    println!("───────────────────────────────────────────────────────────────");

    for (i, token) in tokens.iter().enumerate() {
        println!("{:4}: {:?}", i + 1, token.kind,);
    }

    println!("───────────────────────────────────────────────────────────────");
    println!("Total: {} tokens", tokens.len());

    if errors.is_empty() {
        Ok(())
    } else {
        Err(miette::miette!(
            "Lexing completed with {} error(s)",
            errors.len()
        ))
    }
}
