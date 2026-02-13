//! Compile command implementation.

use std::path::PathBuf;

use miette::{IntoDiagnostic, Result, WrapErr};

/// Run the compile command.
pub fn run(
    input: PathBuf,
    output: Option<PathBuf>,
    optimize: u8,
    emit_asm: bool,
    emit_llvm: bool,
    _include_paths: Vec<PathBuf>,
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
    let source_file = zos_cobol::SourceFile::from_text(
        zos_cobol::FileId::MAIN,
        source,
        zos_cobol::SourceFormat::Fixed,
    );

    // Scan the source file
    let (tokens, lex_errors) = zos_cobol::scan(&source_file);

    if !lex_errors.is_empty() {
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
    let parser = zos_cobol::Parser::new(tokens);
    let (program_opt, parse_errors) = parser.parse_program();

    if !parse_errors.is_empty() {
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
    let semantic_result = zos_cobol::analyze(&program);
    if !semantic_result.diagnostics.is_empty() {
        for diag in &semantic_result.diagnostics {
            match diag.severity {
                zos_cobol::Severity::Error => tracing::error!("{}", diag.message),
                zos_cobol::Severity::Warning => tracing::warn!("{}", diag.message),
                zos_cobol::Severity::Info => tracing::info!("{}", diag.message),
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
            0 => zos_cobol::codegen::OptimizationLevel::None,
            1 => zos_cobol::codegen::OptimizationLevel::Less,
            2 => zos_cobol::codegen::OptimizationLevel::Default,
            _ => zos_cobol::codegen::OptimizationLevel::Aggressive,
        };

        let context = inkwell::context::Context::create();
        let options = zos_cobol::codegen::CodegenOptions::new(
            input
                .file_stem()
                .unwrap_or_default()
                .to_string_lossy()
                .to_string(),
        )
        .with_optimization(optimization);

        let mut codegen = zos_cobol::codegen::CodeGenerator::new(&context, options)
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
