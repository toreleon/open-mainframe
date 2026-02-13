//! Run command implementation.

use std::path::PathBuf;

use miette::{IntoDiagnostic, Result, WrapErr};

/// Run the JCL job execution command.
pub fn run(
    input: PathBuf,
    program_dir: Option<PathBuf>,
    dataset_dir: Option<PathBuf>,
    work_dir: Option<PathBuf>,
) -> Result<()> {
    // Read JCL file
    let source = std::fs::read_to_string(&input)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read JCL file: {}", input.display()))?;

    tracing::info!("Executing JCL: {}", input.display());

    // Parse JCL
    let job = zos_jcl::parse(&source)
        .map_err(|e| miette::miette!("JCL parse error: {}", e))?;

    tracing::info!("Job: {}", job.name);
    tracing::info!("Steps: {}", job.steps.len());

    // Configure executor
    let mut config = zos_jcl::ExecutionConfig::default();
    if let Some(dir) = program_dir {
        config.program_dir = dir;
    }
    if let Some(dir) = dataset_dir {
        config.dataset_dir = dir;
    }
    if let Some(dir) = work_dir {
        config.work_dir = dir.clone();
        config.sysout_dir = dir.join("sysout");
    }

    // Execute job
    let result = zos_jcl::run_with_config(&source, config)
        .map_err(|e| miette::miette!("Job execution error: {}", e))?;

    // Print results
    println!();
    println!("═══════════════════════════════════════════════════════════════");
    println!("JOB {} - {}", result.name, if result.success { "COMPLETED" } else { "FAILED" });
    println!("═══════════════════════════════════════════════════════════════");
    println!();

    for (i, step) in result.steps.iter().enumerate() {
        let step_name = step.name.as_deref().unwrap_or("UNNAMED");
        let status = if step.success { "OK" } else { "FAILED" };
        let rc_indicator = match step.return_code {
            0 => "✓",
            1..=4 => "⚠",
            _ => "✗",
        };

        println!(
            "  Step {}: {} - RC={:04} [{}] {}",
            i + 1,
            step_name,
            step.return_code,
            status,
            rc_indicator
        );

        if !step.stdout.is_empty() {
            println!("    STDOUT:");
            for line in step.stdout.lines().take(10) {
                println!("      {}", line);
            }
            if step.stdout.lines().count() > 10 {
                println!("      ... ({} more lines)", step.stdout.lines().count() - 10);
            }
        }

        if !step.stderr.is_empty() {
            println!("    STDERR:");
            for line in step.stderr.lines().take(5) {
                println!("      {}", line);
            }
        }
    }

    println!();
    println!("───────────────────────────────────────────────────────────────");
    println!("Maximum Return Code: {:04}", result.return_code);
    println!("───────────────────────────────────────────────────────────────");

    if result.success {
        Ok(())
    } else {
        Err(miette::miette!("Job {} failed with RC={}", result.name, result.return_code))
    }
}
