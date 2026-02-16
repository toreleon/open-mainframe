//! Compile command implementation.

use std::path::PathBuf;

use miette::{IntoDiagnostic, Result, WrapErr};

use crate::output::{
    CompileOutput, DiagnosticEntry, DiagnosticSeverity, DiagnosticSummary, OutputFormat, print_json,
};

/// Run the compile command.
pub fn run(
    input: PathBuf,
    output: Option<PathBuf>,
    optimize: u8,
    emit_asm: bool,
    emit_llvm: bool,
    _include_paths: Vec<PathBuf>,
    format: OutputFormat,
) -> Result<()> {
    // Read source file
    let source = std::fs::read_to_string(&input)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read source file: {}", input.display()))?;

    tracing::info!("Compiling {}", input.display());

    // Determine output path
    let output_path = output.unwrap_or_else(|| {
        let mut out = input.clone();
        if emit_llvm {
            out.set_extension("ll");
        } else if emit_asm {
            out.set_extension("s");
        } else {
            out.set_extension("o");
        }
        out
    });

    // Create source file
    let source_file = open_mainframe_cobol::SourceFile::from_text(
        open_mainframe_cobol::FileId::MAIN,
        source,
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
            tracing::error!("Lexer error: {}", err);
        }
        return Err(miette::miette!(
            "Lexical analysis failed with {} error(s)",
            lex_errors.len()
        ));
    }

    tracing::debug!("Scanned {} tokens", tokens.len());

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
            tracing::error!("Parse error: {}", err);
        }
        return Err(miette::miette!(
            "Parse failed with {} error(s)",
            parse_errors.len()
        ));
    }

    let program = program_opt.ok_or_else(|| miette::miette!("Failed to parse program"))?;

    tracing::debug!("Parsed program: {:?}", program.identification.program_id);

    // Semantic analysis
    let semantic_result = open_mainframe_cobol::analyze(&program);

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

        let errors = diagnostics
            .iter()
            .filter(|d| matches!(d.severity, DiagnosticSeverity::Error))
            .count();
        let warnings = diagnostics
            .iter()
            .filter(|d| matches!(d.severity, DiagnosticSeverity::Warning))
            .count();
        let infos = diagnostics
            .iter()
            .filter(|d| matches!(d.severity, DiagnosticSeverity::Info))
            .count();

        let output = CompileOutput {
            status: if semantic_result.has_errors {
                "error".to_string()
            } else {
                "success".to_string()
            },
            program_id: Some(program.identification.program_id.name.clone()),
            diagnostics,
            summary: DiagnosticSummary {
                errors,
                warnings,
                infos,
            },
            symbols: Some(semantic_result.symbol_table.len()),
        };
        print_json(&output);

        if semantic_result.has_errors {
            return Err(miette::miette!("Semantic analysis failed with errors"));
        }

        return Ok(());
    }

    // Text output (original behavior)
    if !semantic_result.diagnostics.is_empty() {
        for diag in &semantic_result.diagnostics {
            match diag.severity {
                open_mainframe_cobol::Severity::Error => tracing::error!("{}", diag.message),
                open_mainframe_cobol::Severity::Warning => tracing::warn!("{}", diag.message),
                open_mainframe_cobol::Severity::Info => tracing::info!("{}", diag.message),
            }
        }
        if semantic_result.has_errors {
            return Err(miette::miette!("Semantic analysis failed with errors"));
        }
    }

    // Code generation (only with LLVM feature)
    #[cfg(feature = "llvm")]
    {
        let optimization = match optimize {
            0 => open_mainframe_cobol::codegen::OptimizationLevel::None,
            1 => open_mainframe_cobol::codegen::OptimizationLevel::Less,
            2 => open_mainframe_cobol::codegen::OptimizationLevel::Default,
            _ => open_mainframe_cobol::codegen::OptimizationLevel::Aggressive,
        };

        let context = inkwell::context::Context::create();
        let options = open_mainframe_cobol::codegen::CodegenOptions::new(
            input
                .file_stem()
                .unwrap_or_default()
                .to_string_lossy()
                .to_string(),
        )
        .with_optimization(optimization);

        let mut codegen = open_mainframe_cobol::codegen::CodeGenerator::new(&context, options)
            .map_err(|e| miette::miette!("Failed to create code generator: {}", e))?;

        codegen
            .compile(&program)
            .map_err(|e| miette::miette!("Code generation failed: {}", e))?;

        if emit_llvm {
            let ir = codegen.to_ir_string();
            std::fs::write(&output_path, ir)
                .into_diagnostic()
                .wrap_err("Failed to write LLVM IR")?;
            tracing::info!("Wrote LLVM IR to {}", output_path.display());
        } else if emit_asm {
            codegen
                .write_assembly(&output_path)
                .map_err(|e| miette::miette!("Failed to write assembly: {}", e))?;
            tracing::info!("Wrote assembly to {}", output_path.display());
        } else {
            codegen
                .write_object_file(&output_path)
                .map_err(|e| miette::miette!("Failed to write object file: {}", e))?;
            tracing::info!("Wrote object file to {}", output_path.display());
        }
    }

    #[cfg(not(feature = "llvm"))]
    {
        let _ = (optimize, emit_asm, emit_llvm, output_path); // Suppress unused warnings
        tracing::warn!("Code generation requires LLVM feature");
        tracing::info!("Syntax and semantic analysis completed successfully");
        tracing::info!("To generate object code, rebuild with --features llvm");

        // For now, just indicate success of parsing/analysis phase
        println!("✓ Parsed and analyzed: {}", input.display());
        println!("  Program ID: {:?}", program.identification.program_id);
        if program.data.is_some() {
            println!("  Has DATA DIVISION");
        }
        if program.procedure.is_some() {
            println!("  Has PROCEDURE DIVISION");
        }
    }

    Ok(())
}
