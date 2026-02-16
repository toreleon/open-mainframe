//! Check command implementation - syntax and semantic analysis without code generation.

use std::path::PathBuf;

use miette::{IntoDiagnostic, Result, WrapErr};

use crate::output::{
    CompileOutput, DiagnosticEntry, DiagnosticSeverity, DiagnosticSummary, OutputFormat, print_json,
};

/// Run the syntax check command.
pub fn run(input: PathBuf, include_paths: Vec<PathBuf>, format: OutputFormat) -> Result<()> {
    // Read source file
    let source = std::fs::read_to_string(&input)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read source file: {}", input.display()))?;

    tracing::info!("Checking {}", input.display());

    // Configure copybook paths
    let mut copybook_config = open_mainframe_cobol::CopybookConfig::new();

    // Add the directory containing the source file
    if let Some(parent) = input.parent() {
        copybook_config.add_path(parent);
    }

    // Add user-specified include paths
    for path in &include_paths {
        copybook_config.add_path(path);
        tracing::debug!("Added include path: {}", path.display());
    }

    // Preprocess - expand COPY statements
    let mut preprocessor =
        open_mainframe_cobol::Preprocessor::new(copybook_config, open_mainframe_cobol::SourceFormat::Fixed);
    let preprocessed = preprocessor.preprocess(&source).map_err(|e| {
        miette::miette!("Preprocessing failed: {}", e)
    })?;

    if !format.is_json() {
        println!("✓ Preprocessor: copybooks expanded");
    }

    // Create source file from preprocessed source
    let source_file = open_mainframe_cobol::SourceFile::from_text(
        open_mainframe_cobol::FileId::MAIN,
        preprocessed,
        open_mainframe_cobol::SourceFormat::Fixed,
    );

    // Scan the source file
    let (tokens, lex_errors) = open_mainframe_cobol::scan(&source_file);

    if !lex_errors.is_empty() {
        if format.is_json() {
            let diagnostics: Vec<DiagnosticEntry> = lex_errors
                .iter()
                .map(|err| DiagnosticEntry {
                    severity: DiagnosticSeverity::Error,
                    message: err.to_string(),
                    file: Some(input.display().to_string()),
                    line: None,
                    col: None,
                })
                .collect();
            let output = CompileOutput {
                status: "error".to_string(),
                program_id: None,
                diagnostics,
                summary: DiagnosticSummary {
                    errors: lex_errors.len(),
                    warnings: 0,
                    infos: 0,
                },
                symbols: None,
            };
            print_json(&output);
            return Err(miette::miette!(
                "Lexical analysis failed with {} error(s)",
                lex_errors.len()
            ));
        }
        for err in &lex_errors {
            println!("Lexer error: {}", err);
        }
        return Err(miette::miette!(
            "Lexical analysis failed with {} error(s)",
            lex_errors.len()
        ));
    }

    if !format.is_json() {
        println!("✓ Lexical analysis: {} tokens", tokens.len());
    }

    // Parse the source
    let parser = open_mainframe_cobol::Parser::new(tokens);
    let (program_opt, parse_errors) = parser.parse_program();

    if !parse_errors.is_empty() {
        if format.is_json() {
            let diagnostics: Vec<DiagnosticEntry> = parse_errors
                .iter()
                .map(|err| DiagnosticEntry {
                    severity: DiagnosticSeverity::Error,
                    message: err.to_string(),
                    file: Some(input.display().to_string()),
                    line: None,
                    col: None,
                })
                .collect();
            let output = CompileOutput {
                status: "error".to_string(),
                program_id: None,
                diagnostics,
                summary: DiagnosticSummary {
                    errors: parse_errors.len(),
                    warnings: 0,
                    infos: 0,
                },
                symbols: None,
            };
            print_json(&output);
            return Err(miette::miette!(
                "Parse failed with {} error(s)",
                parse_errors.len()
            ));
        }
        for err in &parse_errors {
            println!("Parse error: {}", err);
        }
        return Err(miette::miette!(
            "Parse failed with {} error(s)",
            parse_errors.len()
        ));
    }

    let program = program_opt.ok_or_else(|| miette::miette!("Failed to parse program"))?;

    if !format.is_json() {
        println!("✓ Syntax analysis: parsed successfully");
        println!("  Program ID: {:?}", program.identification.program_id);
    }

    // Semantic analysis
    let semantic_result = open_mainframe_cobol::analyze(&program);

    let errors: Vec<_> = semantic_result
        .diagnostics
        .iter()
        .filter(|d| matches!(d.severity, open_mainframe_cobol::Severity::Error))
        .collect();
    let warnings: Vec<_> = semantic_result
        .diagnostics
        .iter()
        .filter(|d| matches!(d.severity, open_mainframe_cobol::Severity::Warning))
        .collect();
    let infos: Vec<_> = semantic_result
        .diagnostics
        .iter()
        .filter(|d| matches!(d.severity, open_mainframe_cobol::Severity::Info))
        .collect();

    if format.is_json() {
        let diagnostics: Vec<DiagnosticEntry> = semantic_result
            .diagnostics
            .iter()
            .map(|diag| {
                let severity = match diag.severity {
                    open_mainframe_cobol::Severity::Error => DiagnosticSeverity::Error,
                    open_mainframe_cobol::Severity::Warning => DiagnosticSeverity::Warning,
                    open_mainframe_cobol::Severity::Info => DiagnosticSeverity::Info,
                };
                DiagnosticEntry {
                    severity,
                    message: diag.message.clone(),
                    file: Some(input.display().to_string()),
                    line: None,
                    col: None,
                }
            })
            .collect();

        let output = CompileOutput {
            status: if semantic_result.has_errors {
                "error".to_string()
            } else {
                "success".to_string()
            },
            program_id: Some(program.identification.program_id.name.clone()),
            diagnostics,
            summary: DiagnosticSummary {
                errors: errors.len(),
                warnings: warnings.len(),
                infos: infos.len(),
            },
            symbols: Some(semantic_result.symbol_table.len()),
        };
        print_json(&output);

        return if semantic_result.has_errors {
            Err(miette::miette!(
                "Check failed with {} error(s)",
                errors.len()
            ))
        } else {
            Ok(())
        };
    }

    // Text output (original behavior)
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
