//! Parse JCL command - shows job structure.

use std::path::PathBuf;

use miette::{IntoDiagnostic, Result, WrapErr};

/// Run the parse-jcl command.
pub fn run(input: PathBuf) -> Result<()> {
    // Read JCL file
    let source = std::fs::read_to_string(&input)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read JCL file: {}", input.display()))?;

    tracing::info!("Parsing JCL: {}", input.display());

    // Parse JCL
    let job = zos_jcl::parse(&source)
        .map_err(|e| miette::miette!("JCL parse error: {}", e))?;

    // Print job structure
    println!("JCL Analysis: {}", input.display());
    println!("═══════════════════════════════════════════════════════════════");
    println!();
    println!("JOB: {}", job.name);

    // Job parameters
    if let Some(ref acct) = job.params.accounting {
        println!("  Accounting: {}", acct);
    }
    if let Some(ref prog) = job.params.programmer {
        println!("  Programmer: {}", prog);
    }
    if let Some(class) = job.params.class {
        println!("  Class: {}", class);
    }
    if let Some(msgclass) = job.params.msgclass {
        println!("  MSGCLASS: {}", msgclass);
    }
    if let Some(ref notify) = job.params.notify {
        println!("  Notify: {}", notify);
    }
    if let Some(region) = job.params.region {
        println!("  Region: {}K", region);
    }

    println!();
    println!("STEPS: {} total", job.steps.len());
    println!("───────────────────────────────────────────────────────────────");

    for (i, step) in job.steps.iter().enumerate() {
        let step_name = step.name.as_deref().unwrap_or("(unnamed)");
        let exec_desc = match &step.exec {
            zos_jcl::ExecType::Program(pgm) => format!("PGM={}", pgm),
            zos_jcl::ExecType::Procedure(proc) => format!("PROC={}", proc),
        };

        println!();
        println!("Step {}: {} - {}", i + 1, step_name, exec_desc);

        if let Some(ref parm) = step.params.parm {
            println!("  PARM: {}", parm);
        }
        if let Some(region) = step.params.region {
            println!("  REGION: {}K", region);
        }

        if !step.dd_statements.is_empty() {
            println!("  DD Statements:");
            for dd in &step.dd_statements {
                let dd_desc = match &dd.definition {
                    zos_jcl::DdDefinition::Dataset(def) => {
                        let disp = def.disp.as_ref()
                            .map(|d| format!("{:?}", d.status))
                            .unwrap_or_else(|| "SHR".to_string());
                        format!("DSN={} DISP={}", def.dsn, disp)
                    }
                    zos_jcl::DdDefinition::Sysout(def) => {
                        format!("SYSOUT={}", def.class)
                    }
                    zos_jcl::DdDefinition::Inline(_) => "* (inline data)".to_string(),
                    zos_jcl::DdDefinition::Dummy => "DUMMY".to_string(),
                    zos_jcl::DdDefinition::Concatenation(defs) => {
                        format!("CONCAT ({} datasets)", defs.len())
                    }
                };
                println!("    {}: {}", dd.name, dd_desc);
            }
        }
    }

    println!();
    println!("═══════════════════════════════════════════════════════════════");
    println!("Parse completed successfully.");

    Ok(())
}
