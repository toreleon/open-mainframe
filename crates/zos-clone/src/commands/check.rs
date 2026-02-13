//! Check command implementation - syntax and semantic analysis without code generation.

use std::path::PathBuf;

use miette::{IntoDiagnostic, Result, WrapErr};

/// Run the syntax check command.
pub fn run(input: PathBuf, _include_paths: Vec<PathBuf>) -> Result<()> {
    // Read source file
    let source = std::fs::read_to_string(&input)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read source file: {}", input.display()))?;

    tracing::info!("Checking {}", input.display());

    // Create source file
    let source_file = zos_cobol::SourceFile::from_text(
        zos_cobol::FileId::MAIN,
        source,
        zos_cobol::SourceFormat::Fixed,
    );

    // Scan the source file
    let (tokens, lex_errors) = zos_cobol::scan(&source_file);

    if !lex_errors.is_empty() {
        for err in &lex_errors {
            println!("Lexer error: {}", err);
        }
        return Err(miette::miette!(
            "Lexical analysis failed with {} error(s)",
            lex_errors.len()
        ));
    }

    println!("✓ Lexical analysis: {} tokens", tokens.len());

    // Parse the source
    let parser = zos_cobol::Parser::new(tokens);
    let (program_opt, parse_errors) = parser.parse_program();

    if !parse_errors.is_empty() {
        for err in &parse_errors {
            println!("Parse error: {}", err);
        }
        return Err(miette::miette!(
            "Parse failed with {} error(s)",
            parse_errors.len()
        ));
    }

    let program = program_opt.ok_or_else(|| miette::miette!("Failed to parse program"))?;

    println!("✓ Syntax analysis: parsed successfully");
    println!("  Program ID: {:?}", program.identification.program_id);

    // Semantic analysis
    let semantic_result = zos_cobol::analyze(&program);

    let errors: Vec<_> = semantic_result
        .diagnostics
        .iter()
        .filter(|d| matches!(d.severity, zos_cobol::Severity::Error))
        .collect();
    let warnings: Vec<_> = semantic_result
        .diagnostics
        .iter()
        .filter(|d| matches!(d.severity, zos_cobol::Severity::Warning))
        .collect();
    let infos: Vec<_> = semantic_result
        .diagnostics
        .iter()
        .filter(|d| matches!(d.severity, zos_cobol::Severity::Info))
        .collect();

    if semantic_result.has_errors {
        println!("✗ Semantic analysis failed:");
    } else {
        println!("✓ Semantic analysis: passed");
    }

    // Print diagnostics
    for diag in &errors {
        println!("  ERROR: {}", diag.message);
    }
    for diag in &warnings {
        println!("  WARNING: {}", diag.message);
    }
    for diag in &infos {
        println!("  INFO: {}", diag.message);
    }

    // Print summary
    println!();
    println!("Summary:");
    println!(
        "  {} error(s), {} warning(s), {} info(s)",
        errors.len(),
        warnings.len(),
        infos.len()
    );

    // Print symbol table info
    println!();
    println!("Symbol table:");
    println!("  {} symbols defined", semantic_result.symbol_table.len());

    if semantic_result.has_errors {
        Err(miette::miette!(
            "Check failed with {} error(s)",
            errors.len()
        ))
    } else {
        Ok(())
    }
}
