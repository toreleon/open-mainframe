//! Parse JCL command - shows job structure.

use std::path::PathBuf;

use miette::{IntoDiagnostic, Result, WrapErr};

use crate::output::{JclDdOutput, JclStepParseOutput, OutputFormat, ParseJclOutput, print_json};

/// Run the parse-jcl command.
pub fn run(input: PathBuf, format: OutputFormat) -> Result<()> {
    // Read JCL file
    let source = std::fs::read_to_string(&input)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read JCL file: {}", input.display()))?;

    tracing::info!("Parsing JCL: {}", input.display());

    // Parse JCL
    let job = open_mainframe_jcl::parse(&source).map_err(|e| miette::miette!("JCL parse error: {}", e))?;

    if format.is_json() {
        let steps: Vec<JclStepParseOutput> = job
            .steps
            .iter()
            .map(|step| {
                let exec = match &step.exec {
                    open_mainframe_jcl::ExecType::Program(pgm) => format!("PGM={}", pgm),
                    open_mainframe_jcl::ExecType::Procedure(proc) => format!("PROC={}", proc),
                };
                let dd_statements: Vec<JclDdOutput> = step
                    .dd_statements
                    .iter()
                    .map(|dd| {
                        let definition = match &dd.definition {
                            open_mainframe_jcl::DdDefinition::Dataset(def) => {
                                let disp = def
                                    .disp
                                    .as_ref()
                                    .map(|d| format!("{:?}", d.status))
                                    .unwrap_or_else(|| "SHR".to_string());
                                format!("DSN={} DISP={}", def.dsn, disp)
                            }
                            open_mainframe_jcl::DdDefinition::Sysout(def) => {
                                format!("SYSOUT={}", def.class)
                            }
                            open_mainframe_jcl::DdDefinition::Inline(_) => "INLINE".to_string(),
                            open_mainframe_jcl::DdDefinition::Dummy => "DUMMY".to_string(),
                            open_mainframe_jcl::DdDefinition::Concatenation(defs) => {
                                format!("CONCAT({})", defs.len())
                            }
                        };
                        JclDdOutput {
                            name: dd.name.clone(),
                            definition,
                        }
                    })
                    .collect();

                JclStepParseOutput {
                    name: step.name.clone().unwrap_or_else(|| "UNNAMED".to_string()),
                    exec,
                    parm: step.params.parm.clone(),
                    dd_statements,
                }
            })
            .collect();

        let output = ParseJclOutput {
            status: "success".to_string(),
            job_name: job.name.clone(),
            accounting: job.params.accounting.clone(),
            programmer: job.params.programmer.clone(),
            class: job.params.class.map(|c| c.to_string()),
            msgclass: job.params.msgclass.map(|c| c.to_string()),
            steps,
        };
        print_json(&output);
        return Ok(());
    }

    // Text output (original behavior)
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
            open_mainframe_jcl::ExecType::Program(pgm) => format!("PGM={}", pgm),
            open_mainframe_jcl::ExecType::Procedure(proc) => format!("PROC={}", proc),
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
                    open_mainframe_jcl::DdDefinition::Dataset(def) => {
                        let disp = def
                            .disp
                            .as_ref()
                            .map(|d| format!("{:?}", d.status))
                            .unwrap_or_else(|| "SHR".to_string());
                        format!("DSN={} DISP={}", def.dsn, disp)
                    }
                    open_mainframe_jcl::DdDefinition::Sysout(def) => {
                        format!("SYSOUT={}", def.class)
                    }
                    open_mainframe_jcl::DdDefinition::Inline(_) => "* (inline data)".to_string(),
                    open_mainframe_jcl::DdDefinition::Dummy => "DUMMY".to_string(),
                    open_mainframe_jcl::DdDefinition::Concatenation(defs) => {
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
