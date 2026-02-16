//! Structured output types for machine-readable CLI responses.
//!
//! When `--format json` is specified, commands emit these types as JSON
//! instead of human-readable text.

use serde::Serialize;

/// Output format selection.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    Text,
    Json,
}

impl OutputFormat {
    pub fn from_str(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "json" => OutputFormat::Json,
            _ => OutputFormat::Text,
        }
    }

    pub fn is_json(self) -> bool {
        self == OutputFormat::Json
    }
}

/// Diagnostic severity level.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Info,
}

/// A single diagnostic message.
#[derive(Debug, Clone, Serialize)]
pub struct DiagnosticEntry {
    pub severity: DiagnosticSeverity,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub file: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub col: Option<usize>,
}

/// Summary of diagnostic counts.
#[derive(Debug, Clone, Serialize)]
pub struct DiagnosticSummary {
    pub errors: usize,
    pub warnings: usize,
    pub infos: usize,
}

/// Output from compile/check commands.
#[derive(Debug, Clone, Serialize)]
pub struct CompileOutput {
    pub status: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub program_id: Option<String>,
    pub diagnostics: Vec<DiagnosticEntry>,
    pub summary: DiagnosticSummary,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub symbols: Option<usize>,
}

/// Output from interpret/run-cobol commands.
#[derive(Debug, Clone, Serialize)]
pub struct InterpretOutput {
    pub status: String,
    pub output: Vec<String>,
    pub return_code: i32,
    pub diagnostics: Vec<DiagnosticEntry>,
}

/// A JCL step result for JSON output.
#[derive(Debug, Clone, Serialize)]
pub struct JclStepOutput {
    pub name: String,
    pub return_code: u32,
    pub success: bool,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub stdout: Vec<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub stderr: Vec<String>,
}

/// Output from run (JCL execution) command.
#[derive(Debug, Clone, Serialize)]
pub struct RunOutput {
    pub status: String,
    pub job_name: String,
    pub return_code: u32,
    pub steps: Vec<JclStepOutput>,
}

/// A JCL DD statement for JSON output.
#[derive(Debug, Clone, Serialize)]
pub struct JclDdOutput {
    pub name: String,
    pub definition: String,
}

/// A JCL step for parse output.
#[derive(Debug, Clone, Serialize)]
pub struct JclStepParseOutput {
    pub name: String,
    pub exec: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parm: Option<String>,
    pub dd_statements: Vec<JclDdOutput>,
}

/// Output from parse-jcl command.
#[derive(Debug, Clone, Serialize)]
pub struct ParseJclOutput {
    pub status: String,
    pub job_name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub accounting: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub programmer: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub class: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub msgclass: Option<String>,
    pub steps: Vec<JclStepParseOutput>,
}

/// Print a serializable value as JSON to stdout.
pub fn print_json<T: Serialize>(value: &T) {
    match serde_json::to_string_pretty(value) {
        Ok(json) => println!("{}", json),
        Err(e) => eprintln!("Failed to serialize JSON: {}", e),
    }
}
