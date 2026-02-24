//! Procedure expansion for JCL in-stream and cataloged procedures.
//!
//! The `ProcedureExpander` runs as a preprocessing pass after parsing,
//! resolving `ExecType::Procedure` references by expanding in-stream or
//! cataloged procedure bodies with symbolic parameter substitution.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use open_mainframe_lang_core::Span;

use crate::ast::*;
use crate::error::JclError;
use crate::lexer::{tokenize_operands, Token};

/// Maximum allowed procedure nesting depth (IBM standard).
const MAX_NESTING_DEPTH: usize = 15;

/// Trait for resolving procedure names to JCL text.
///
/// Implementations can load procedures from PDS directories on the filesystem,
/// from in-memory stores (for testing), or from any other backend.
pub trait ProcedureLibrary {
    /// Look up a procedure by name and return its JCL text.
    ///
    /// Returns `None` if the procedure is not found in this library.
    fn find_procedure(&self, name: &str) -> Option<String>;
}

/// Filesystem-based procedure library that searches PDS directories.
///
/// Resolves procedure names by searching directories in order,
/// looking for files named `<PROCNAME>` or `<PROCNAME>.jcl`.
pub struct FilesystemProcLib {
    /// Directories to search, in order.
    search_dirs: Vec<PathBuf>,
}

impl FilesystemProcLib {
    /// Create a new filesystem procedure library.
    ///
    /// `base_dir` is the root directory for datasets (e.g., `datasets/`).
    /// `jcllib_order` contains dataset names like `PROD.PROCLIB` which map
    /// to directories like `datasets/PROD/PROCLIB/`.
    pub fn new(base_dir: &Path, jcllib_order: &[String]) -> Self {
        let search_dirs = jcllib_order
            .iter()
            .map(|dsn| {
                let mut path = base_dir.to_path_buf();
                for part in dsn.split('.') {
                    path.push(part);
                }
                path
            })
            .collect();
        Self { search_dirs }
    }
}

impl ProcedureLibrary for FilesystemProcLib {
    fn find_procedure(&self, name: &str) -> Option<String> {
        for dir in &self.search_dirs {
            // Try exact name
            let path = dir.join(name);
            if path.exists() {
                return std::fs::read_to_string(&path).ok();
            }
            // Try with .jcl extension
            let path_jcl = dir.join(format!("{}.jcl", name));
            if path_jcl.exists() {
                return std::fs::read_to_string(&path_jcl).ok();
            }
            // Try lowercase
            let path_lower = dir.join(name.to_lowercase());
            if path_lower.exists() {
                return std::fs::read_to_string(&path_lower).ok();
            }
        }
        None
    }
}

/// In-memory procedure library for testing.
pub struct InMemoryProcLib {
    /// Map of procedure name → JCL text.
    procedures: HashMap<String, String>,
}

impl InMemoryProcLib {
    /// Create a new in-memory procedure library.
    pub fn new() -> Self {
        Self {
            procedures: HashMap::new(),
        }
    }

    /// Add a procedure to the library.
    pub fn add(&mut self, name: &str, jcl: &str) {
        self.procedures
            .insert(name.to_uppercase(), jcl.to_string());
    }
}

impl Default for InMemoryProcLib {
    fn default() -> Self {
        Self::new()
    }
}

impl ProcedureLibrary for InMemoryProcLib {
    fn find_procedure(&self, name: &str) -> Option<String> {
        self.procedures.get(&name.to_uppercase()).cloned()
    }
}

/// Expands procedure references in a parsed JCL job.
///
/// Resolves `ExecType::Procedure(name)` by looking up in-stream procedures
/// or cataloged procedures from a `ProcedureLibrary`, substituting symbolic
/// parameters, applying DD overrides, and replacing the procedure call with
/// the expanded steps. Supports nesting up to 15 levels.
pub struct ProcedureExpander {
    /// In-stream procedures available for expansion.
    in_stream_procs: HashMap<String, InStreamProc>,
    /// Global symbol table from SET statements.
    global_symbols: SymbolTable,
    /// Counter for generating temporary dataset names.
    temp_counter: u32,
    /// Cataloged procedure library (optional).
    proc_library: Option<Box<dyn ProcedureLibrary>>,
    /// DD overrides from the calling JCL.
    dd_overrides: Vec<DdOverride>,
}

impl ProcedureExpander {
    /// Create a new expander with the given procedures and symbols.
    pub fn new(
        in_stream_procs: HashMap<String, InStreamProc>,
        global_symbols: SymbolTable,
    ) -> Self {
        Self {
            in_stream_procs,
            global_symbols,
            temp_counter: 0,
            proc_library: None,
            dd_overrides: Vec::new(),
        }
    }

    /// Set the cataloged procedure library for resolving external procedures.
    pub fn with_library(mut self, library: Box<dyn ProcedureLibrary>) -> Self {
        self.proc_library = Some(library);
        self
    }

    /// Set DD overrides for procedure step expansion.
    pub fn with_dd_overrides(mut self, overrides: Vec<DdOverride>) -> Self {
        self.dd_overrides = overrides;
        self
    }

    /// Expand all procedure references in a job.
    ///
    /// Returns a new `Job` where all `ExecType::Procedure` steps have been
    /// replaced with the expanded procedure body steps.
    pub fn expand(&mut self, job: &Job) -> Result<Job, JclError> {
        let mut expanded_job = Job::new(job.name.clone());
        expanded_job.params = job.params.clone();
        expanded_job.span = job.span;
        expanded_job.symbols = job.symbols.clone();
        expanded_job.in_stream_procs = job.in_stream_procs.clone();
        expanded_job.jcllib_order = job.jcllib_order.clone();

        self.expand_entries(&job.entries, &mut expanded_job)?;

        Ok(expanded_job)
    }

    /// Recursively expand procedure references within job entries.
    fn expand_entries(
        &mut self,
        entries: &[JobEntry],
        expanded_job: &mut Job,
    ) -> Result<(), JclError> {
        for entry in entries {
            match entry {
                JobEntry::Step(step) => match &step.exec {
                    ExecType::Program(_) => {
                        expanded_job.add_step(step.as_ref().clone());
                    }
                    ExecType::Procedure(proc_name) => {
                        let expanded_steps = self.expand_procedure_recursive(
                            proc_name,
                            &step.params,
                            &step.name,
                            step.span,
                            0,
                        )?;
                        for expanded_step in expanded_steps {
                            expanded_job.span =
                                expanded_job.span.extend(expanded_step.span);
                            expanded_job.add_step(expanded_step);
                        }
                    }
                },
                JobEntry::If(if_construct) => {
                    let mut expanded_then = Job::new("_temp");
                    self.expand_entries(&if_construct.then_entries, &mut expanded_then)?;
                    let mut expanded_else = Job::new("_temp");
                    self.expand_entries(&if_construct.else_entries, &mut expanded_else)?;

                    expanded_job.add_entry(JobEntry::If(IfConstruct {
                        condition: if_construct.condition.clone(),
                        then_entries: expanded_then.entries,
                        else_entries: expanded_else.entries,
                        span: if_construct.span,
                    }));
                }
            }
        }
        Ok(())
    }

    /// Resolve a procedure by name — checks in-stream first, then cataloged library.
    fn resolve_procedure(&self, proc_name: &str) -> Result<InStreamProc, JclError> {
        // Check in-stream procedures first
        if let Some(proc) = self.in_stream_procs.get(proc_name) {
            return Ok(proc.clone());
        }

        // Check cataloged procedure library
        if let Some(ref library) = self.proc_library {
            if let Some(jcl_text) = library.find_procedure(proc_name) {
                return self.parse_cataloged_proc(proc_name, &jcl_text);
            }
        }

        Err(JclError::ParseError {
            message: format!("Procedure '{}' not found", proc_name),
        })
    }

    /// Parse a cataloged procedure JCL text into an InStreamProc.
    ///
    /// Cataloged procedures may or may not have PROC/PEND wrappers.
    /// Handles JCL continuation lines: when operands end with a trailing
    /// comma, the next `//` line (with a blank name field) is a continuation.
    fn parse_cataloged_proc(
        &self,
        name: &str,
        jcl_text: &str,
    ) -> Result<InStreamProc, JclError> {
        let mut defaults = SymbolTable::new();
        let mut statements = Vec::new();

        // First pass: collect logical lines by joining continuations.
        // A JCL line ending with ',' (after trimming trailing spaces) means
        // the next '//' line with a blank name field is a continuation.
        let mut logical_lines: Vec<(Option<String>, String, String)> = Vec::new(); // (name, operation, operands)

        let lines: Vec<&str> = jcl_text.lines().collect();
        let mut i = 0;
        while i < lines.len() {
            let trimmed = lines[i].trim();
            if trimmed.is_empty() || trimmed.starts_with("//*") || trimmed.starts_with("/*") {
                i += 1;
                continue;
            }
            if !trimmed.starts_with("//") {
                i += 1;
                continue;
            }
            let content = &trimmed[2..];
            if content.trim().is_empty() {
                i += 1;
                continue;
            }

            let (stmt_name, rest) = parse_name_field(content);
            let rest_trimmed = rest.trim_start();
            if rest_trimmed.is_empty() {
                i += 1;
                continue;
            }

            let (operation, operands) = split_operation(rest_trimmed);

            // Check if this is a continuation line (no name, no recognized operation)
            // rather than a new statement. If so, skip — it was already consumed.
            // We only process lines that start a new statement.
            if stmt_name.is_none() && !is_jcl_operation(operation) {
                i += 1;
                continue;
            }

            let mut full_operands = operands.trim_end().to_string();

            // Collect continuation lines while operands end with ','
            while full_operands.ends_with(',') && i + 1 < lines.len() {
                i += 1;
                let next_trimmed = lines[i].trim();
                if next_trimmed.is_empty()
                    || next_trimmed.starts_with("//*")
                    || next_trimmed.starts_with("/*")
                {
                    continue; // Skip comments between continuations
                }
                if !next_trimmed.starts_with("//") {
                    break;
                }
                let next_content = &next_trimmed[2..];
                let cont_text = next_content.trim();
                if cont_text.is_empty() {
                    continue;
                }
                // Continuation lines have blank name field (start with spaces)
                if !next_content.starts_with(' ') && !next_content.starts_with('\t') {
                    // This is a new named statement, not a continuation
                    // Back up so the outer loop processes it
                    i -= 1;
                    break;
                }
                full_operands.push_str(cont_text);
            }

            logical_lines.push((stmt_name, operation.to_uppercase(), full_operands));
            i += 1;
        }

        // Second pass: process logical lines
        for (stmt_name, operation_upper, operands) in logical_lines {
            match operation_upper.as_str() {
                "PROC" => {
                    // Parse defaults from PROC statement
                    for assignment in operands.split(',') {
                        let assignment = assignment.trim();
                        if assignment.is_empty() {
                            continue;
                        }
                        if let Some(eq_pos) = assignment.find('=') {
                            let k = assignment[..eq_pos].trim().to_uppercase();
                            let v = assignment[eq_pos + 1..].trim().to_string();
                            defaults.insert(k, v);
                        }
                    }
                }
                "PEND" => {
                    break;
                }
                _ => {
                    statements.push(ProcStatement {
                        name: stmt_name,
                        operation: operation_upper,
                        operands,
                    });
                }
            }
        }

        Ok(InStreamProc {
            name: name.to_string(),
            defaults,
            statements,
        })
    }

    /// Expand a procedure with nesting depth tracking.
    fn expand_procedure_recursive(
        &mut self,
        proc_name: &str,
        exec_params: &ExecParams,
        caller_name: &Option<String>,
        span: Span,
        depth: usize,
    ) -> Result<Vec<Step>, JclError> {
        if depth >= MAX_NESTING_DEPTH {
            return Err(JclError::ParseError {
                message: format!(
                    "Procedure nesting exceeds maximum depth of {}",
                    MAX_NESTING_DEPTH
                ),
            });
        }

        let proc = self.resolve_procedure(proc_name)?;

        // Build effective symbol table:
        // 1. Start with global symbols (SET statements)
        // 2. Apply PROC defaults
        // 3. Apply EXEC overrides (from exec_params.other)
        let mut symbols = self.global_symbols.clone();
        for (k, v) in &proc.defaults {
            symbols.insert(k.clone(), v.clone());
        }
        for (k, v) in &exec_params.other {
            symbols.insert(k.to_uppercase(), v.clone());
        }

        // Parse each statement in the procedure body with substituted symbols
        let mut steps = Vec::new();
        let mut current_step: Option<Step> = None;

        for proc_stmt in &proc.statements {
            let substituted_operands =
                substitute_symbols_with_table(&symbols, &proc_stmt.operands, &mut self.temp_counter);

            match proc_stmt.operation.as_str() {
                "EXEC" => {
                    // Finish previous step if any
                    if let Some(step) = current_step.take() {
                        steps.push(step);
                    }

                    // Parse the EXEC operands
                    let tokens = tokenize_operands(&substituted_operands)?;
                    let (exec_type, params) = parse_exec_tokens(&tokens, proc_name)?;

                    // Check if this is a nested procedure call
                    let step_name = proc_stmt.name.as_ref().map(|n| {
                        if let Some(ref caller) = caller_name {
                            format!("{}.{}", caller, n)
                        } else {
                            n.clone()
                        }
                    });

                    match &exec_type {
                        ExecType::Procedure(nested_proc) => {
                            // Recursively expand nested procedure
                            let nested_steps = self.expand_procedure_recursive(
                                nested_proc,
                                &params,
                                &step_name,
                                span,
                                depth + 1,
                            )?;
                            for ns in nested_steps {
                                steps.push(ns);
                            }
                            // Don't set current_step since nested expansion already produced steps
                        }
                        ExecType::Program(_) => {
                            current_step = Some(Step {
                                name: step_name,
                                exec: exec_type,
                                params,
                                dd_statements: Vec::new(),
                                span,
                            });
                        }
                    }
                }
                "DD" => {
                    if let Some(ref mut step) = current_step {
                        let dd = parse_dd_from_operands(
                            &proc_stmt.name.clone().unwrap_or_else(|| "UNNAMED".to_string()),
                            &substituted_operands,
                            span,
                        )?;
                        step.add_dd(dd);
                    }
                }
                _ => {
                    // Skip other statement types inside procedures
                }
            }
        }

        // Don't forget the last step
        if let Some(step) = current_step.take() {
            steps.push(step);
        }

        // Apply DD overrides
        self.apply_dd_overrides(&mut steps);

        Ok(steps)
    }

    /// Apply DD overrides from the calling JCL to expanded steps.
    fn apply_dd_overrides(&self, steps: &mut [Step]) {
        for dd_override in &self.dd_overrides {
            for step in steps.iter_mut() {
                // Match step name — the override step_name may match the raw step name
                // or the qualified name (caller.step)
                let matches = step.name.as_ref().map_or(false, |name| {
                    name == &dd_override.step_name
                        || name.ends_with(&format!(".{}", dd_override.step_name))
                });

                if matches {
                    // Look for existing DD with same name to replace
                    let dd_name = &dd_override.dd.name;
                    let mut found = false;
                    for dd in step.dd_statements.iter_mut() {
                        if dd.name == *dd_name {
                            dd.definition = dd_override.dd.definition.clone();
                            found = true;
                            break;
                        }
                    }
                    // If not found, add as new DD
                    if !found {
                        step.dd_statements.push(dd_override.dd.clone());
                    }
                }
            }
        }
    }
}

/// Parse EXEC tokens into ExecType and ExecParams.
fn parse_exec_tokens(tokens: &[Token], context: &str) -> Result<(ExecType, ExecParams), JclError> {
    let mut idx = 0;
    let mut exec_type = None;
    let mut params = ExecParams::default();

    while idx < tokens.len() {
        if let Token::Ident(key) = &tokens[idx] {
            idx += 1;
            if idx < tokens.len() && matches!(tokens[idx], Token::Equals) {
                idx += 1;
                if idx < tokens.len() {
                    match key.as_str() {
                        "PGM" => {
                            if let Token::Ident(v) = &tokens[idx] {
                                exec_type = Some(ExecType::Program(v.clone()));
                            }
                        }
                        "PROC" => {
                            if let Token::Ident(v) = &tokens[idx] {
                                exec_type = Some(ExecType::Procedure(v.clone()));
                            }
                        }
                        "PARM" => {
                            if let Token::String(v) = &tokens[idx] {
                                params.parm = Some(v.clone());
                            } else if let Token::Ident(v) = &tokens[idx] {
                                params.parm = Some(v.clone());
                            }
                        }
                        "REGION" => {
                            if let Token::Number(n) = &tokens[idx] {
                                params.region = Some(*n as u32);
                            }
                        }
                        _ => {
                            if let Token::Ident(v) | Token::String(v) = &tokens[idx] {
                                params.other.insert(key.clone(), v.clone());
                            }
                        }
                    }
                    idx += 1;
                }
            } else if exec_type.is_none() {
                // Bare name = procedure name
                exec_type = Some(ExecType::Procedure(key.clone()));
            }
        } else {
            idx += 1;
        }
        if idx < tokens.len() && matches!(tokens[idx], Token::Comma) {
            idx += 1;
        }
    }

    let exec_type = exec_type.ok_or_else(|| JclError::ParseError {
        message: format!("EXEC in procedure '{}' missing PGM or PROC", context),
    })?;

    Ok((exec_type, params))
}

/// Check if a string is a recognized JCL operation keyword.
fn is_jcl_operation(op: &str) -> bool {
    matches!(
        op.to_uppercase().as_str(),
        "DD" | "EXEC" | "PROC" | "PEND" | "SET" | "IF" | "THEN" | "ELSE" | "ENDIF"
            | "INCLUDE" | "JCLLIB" | "OUTPUT" | "CNTL" | "ENDCNTL"
    )
}

/// Parse name field from JCL content (used for cataloged procedure parsing).
fn parse_name_field(content: &str) -> (Option<String>, &str) {
    if content.is_empty() || content.starts_with(' ') {
        return (None, content.trim_start());
    }
    let name_end = content
        .find(|c: char| !c.is_ascii_alphanumeric() && c != '@' && c != '#' && c != '$')
        .unwrap_or(content.len())
        .min(8);
    let name = &content[..name_end];
    let rest = &content[name_end..];
    (Some(name.to_string()), rest)
}

/// Split operation from operands (used for cataloged procedure parsing).
fn split_operation(content: &str) -> (&str, &str) {
    let op_end = content
        .find(|c: char| c.is_whitespace())
        .unwrap_or(content.len());
    let operation = &content[..op_end];
    let operands = content[op_end..].trim_start();
    (operation, operands)
}

/// Perform symbolic substitution on text using a given symbol table.
fn substitute_symbols_with_table(
    symbols: &SymbolTable,
    text: &str,
    temp_counter: &mut u32,
) -> String {
    let mut result = String::with_capacity(text.len());
    let chars: Vec<char> = text.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        if chars[i] == '&' {
            if i + 1 < chars.len() && chars[i + 1] == '&' {
                // &&NAME → temp dataset name
                i += 2;
                let mut name = String::new();
                while i < chars.len()
                    && (chars[i].is_ascii_alphanumeric()
                        || chars[i] == '@'
                        || chars[i] == '#'
                        || chars[i] == '$')
                {
                    name.push(chars[i]);
                    i += 1;
                }
                *temp_counter += 1;
                let temp_name =
                    format!("SYS{:05}.T{:06}.TEMP", *temp_counter, *temp_counter);
                result.push_str(&temp_name);
                if i < chars.len() && chars[i] == '.' {
                    i += 1;
                    if i < chars.len() && chars[i] == '.' {
                        result.push('.');
                        i += 1;
                    }
                }
            } else {
                i += 1;
                let mut name = String::new();
                while i < chars.len()
                    && (chars[i].is_ascii_alphanumeric()
                        || chars[i] == '@'
                        || chars[i] == '#'
                        || chars[i] == '$')
                {
                    name.push(chars[i]);
                    i += 1;
                }
                if name.is_empty() {
                    result.push('&');
                    continue;
                }
                let upper_name = name.to_uppercase();
                if let Some(value) = symbols.get(&upper_name) {
                    result.push_str(value);
                    if i < chars.len() && chars[i] == '.' {
                        i += 1;
                        if i < chars.len() && chars[i] == '.' {
                            result.push('.');
                            i += 1;
                        }
                    }
                } else {
                    result.push('&');
                    result.push_str(&name);
                }
            }
        } else {
            result.push(chars[i]);
            i += 1;
        }
    }

    result
}

/// Parse a DD statement from operands string (used during procedure expansion).
fn parse_dd_from_operands(
    dd_name: &str,
    operands: &str,
    span: Span,
) -> Result<DdStatement, JclError> {
    let tokens = tokenize_operands(operands)?;

    if tokens.is_empty() {
        return Ok(DdStatement {
            name: dd_name.to_string(),
            definition: DdDefinition::Dummy,
            span,
        });
    }

    if matches!(&tokens[0], Token::Ident(s) if s == "DUMMY") {
        return Ok(DdStatement {
            name: dd_name.to_string(),
            definition: DdDefinition::Dummy,
            span,
        });
    }

    if matches!(&tokens[0], Token::Asterisk) {
        return Ok(DdStatement {
            name: dd_name.to_string(),
            definition: DdDefinition::Inline(InlineDef {
                delimiter: None,
                data: Vec::new(),
            }),
            span,
        });
    }

    if matches!(&tokens[0], Token::Ident(s) if s == "SYSOUT") {
        let mut class = '*';
        let mut idx = 0;
        while idx < tokens.len() {
            if let Token::Ident(key) = &tokens[idx] {
                if key == "SYSOUT" {
                    idx += 1;
                    if idx < tokens.len() && matches!(tokens[idx], Token::Equals) {
                        idx += 1;
                        if idx < tokens.len() {
                            match &tokens[idx] {
                                Token::Asterisk => class = '*',
                                Token::Ident(s) => class = s.chars().next().unwrap_or('A'),
                                _ => {}
                            }
                        }
                    }
                }
            }
            idx += 1;
        }
        return Ok(DdStatement {
            name: dd_name.to_string(),
            definition: DdDefinition::Sysout(SysoutDef {
                class,
                writer: None,
                form: None,
            }),
            span,
        });
    }

    // Parse as dataset DD
    let mut dsn = String::new();
    let mut disp = None;
    let mut idx = 0;

    while idx < tokens.len() {
        if let Token::Ident(key) = &tokens[idx] {
            idx += 1;
            if idx < tokens.len() && matches!(tokens[idx], Token::Equals) {
                idx += 1;
                if idx < tokens.len() {
                    match key.as_str() {
                        "DSN" | "DSNAME" => {
                            while idx < tokens.len() {
                                match &tokens[idx] {
                                    Token::Ident(s) => dsn.push_str(s),
                                    Token::Period => dsn.push('.'),
                                    Token::LParen => {
                                        dsn.push('(');
                                        idx += 1;
                                        while idx < tokens.len() {
                                            if matches!(tokens[idx], Token::RParen) {
                                                dsn.push(')');
                                                idx += 1;
                                                break;
                                            }
                                            if let Token::Ident(m) = &tokens[idx] {
                                                dsn.push_str(m);
                                            }
                                            idx += 1;
                                        }
                                        break;
                                    }
                                    Token::String(s) => {
                                        dsn = s.clone();
                                        idx += 1;
                                        break;
                                    }
                                    Token::Ampersand => dsn.push('&'),
                                    _ => break,
                                }
                                idx += 1;
                            }
                            continue;
                        }
                        "DISP" => {
                            let mut status = DispStatus::Shr;
                            let has_paren = matches!(tokens[idx], Token::LParen);
                            if has_paren {
                                idx += 1;
                            }
                            if idx < tokens.len() {
                                if let Token::Ident(s) = &tokens[idx] {
                                    status = match s.as_str() {
                                        "NEW" => DispStatus::New,
                                        "OLD" => DispStatus::Old,
                                        "SHR" => DispStatus::Shr,
                                        "MOD" => DispStatus::Mod,
                                        _ => DispStatus::Shr,
                                    };
                                    idx += 1;
                                }
                            }
                            // Only scan for closing paren if DISP had an opening paren
                            if has_paren {
                                while idx < tokens.len() && !matches!(tokens[idx], Token::RParen) {
                                    idx += 1;
                                }
                                if idx < tokens.len() && matches!(tokens[idx], Token::RParen) {
                                    idx += 1;
                                }
                            }
                            disp = Some(Disposition {
                                status,
                                normal: None,
                                abnormal: None,
                            });
                            continue;
                        }
                        _ => {
                            idx += 1;
                        }
                    }
                }
            }
        } else {
            idx += 1;
        }
        if idx < tokens.len() && matches!(tokens[idx], Token::Comma) {
            idx += 1;
        }
    }

    Ok(DdStatement {
        name: dd_name.to_string(),
        definition: DdDefinition::Dataset(Box::new(DatasetDef {
            dsn,
            disp,
            ..Default::default()
        })),
        span,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_substitute_symbols_basic() {
        let mut symbols = SymbolTable::new();
        symbols.insert("PROGRAM".to_string(), "MYAPP".to_string());
        let mut counter = 0;
        let result = substitute_symbols_with_table(&symbols, "PGM=&PROGRAM", &mut counter);
        assert_eq!(result, "PGM=MYAPP");
    }

    #[test]
    fn test_substitute_symbols_double_period() {
        let mut symbols = SymbolTable::new();
        symbols.insert("HLQ".to_string(), "PROD".to_string());
        symbols.insert("ENV".to_string(), "DAILY".to_string());
        let mut counter = 0;
        let result =
            substitute_symbols_with_table(&symbols, "DSN=&HLQ..DATA.&ENV", &mut counter);
        assert_eq!(result, "DSN=PROD.DATA.DAILY");
    }

    #[test]
    fn test_substitute_symbols_temp_dataset() {
        let symbols = SymbolTable::new();
        let mut counter = 0;
        let result = substitute_symbols_with_table(&symbols, "DSN=&&TEMP", &mut counter);
        assert!(result.starts_with("DSN=SYS"));
        assert!(result.ends_with(".TEMP"));
        assert_eq!(counter, 1);
    }

    #[test]
    fn test_substitute_symbols_unknown() {
        let symbols = SymbolTable::new();
        let mut counter = 0;
        let result = substitute_symbols_with_table(&symbols, "PGM=&UNKNOWN", &mut counter);
        assert_eq!(result, "PGM=&UNKNOWN");
    }

    // === Story 102.1: JCLLIB parsing (tested in parser module) ===

    // === Story 102.2: ProcedureLibrary trait ===

    #[test]
    fn test_in_memory_proc_lib() {
        let mut lib = InMemoryProcLib::new();
        lib.add(
            "MYPROC",
            "//STEP1  EXEC PGM=MYPROG\n//INPUT  DD DSN=DEFAULT.DATA,DISP=SHR\n",
        );

        assert!(lib.find_procedure("MYPROC").is_some());
        assert!(lib.find_procedure("myproc").is_some()); // case-insensitive
        assert!(lib.find_procedure("NOTFOUND").is_none());
    }

    #[test]
    fn test_filesystem_proc_lib() {
        use std::fs;

        let temp_dir = std::env::temp_dir().join("jcl_proc_lib_test");
        let _ = fs::remove_dir_all(&temp_dir);

        // Create PROD/PROCLIB directory
        let proclib_dir = temp_dir.join("PROD").join("PROCLIB");
        fs::create_dir_all(&proclib_dir).unwrap();

        // Create a procedure file
        fs::write(
            proclib_dir.join("MYPROC"),
            "//STEP1  EXEC PGM=MYPROG\n//INPUT  DD DSN=DEFAULT.DATA,DISP=SHR\n",
        )
        .unwrap();

        let lib = FilesystemProcLib::new(
            &temp_dir,
            &["PROD.PROCLIB".to_string()],
        );

        assert!(lib.find_procedure("MYPROC").is_some());
        assert!(lib.find_procedure("NOTFOUND").is_none());

        let _ = fs::remove_dir_all(&temp_dir);
    }

    #[test]
    fn test_filesystem_proc_lib_search_order() {
        use std::fs;

        let temp_dir = std::env::temp_dir().join("jcl_proc_lib_order_test");
        let _ = fs::remove_dir_all(&temp_dir);

        // Create two proc libraries
        let lib1 = temp_dir.join("PROD").join("PROCLIB");
        let lib2 = temp_dir.join("TEST").join("PROCLIB");
        fs::create_dir_all(&lib1).unwrap();
        fs::create_dir_all(&lib2).unwrap();

        // MYPROC only in TEST.PROCLIB
        fs::write(
            lib2.join("MYPROC"),
            "//STEP1  EXEC PGM=TESTPROG\n",
        )
        .unwrap();

        let lib = FilesystemProcLib::new(
            &temp_dir,
            &["PROD.PROCLIB".to_string(), "TEST.PROCLIB".to_string()],
        );

        // Should find it in the second library
        let jcl = lib.find_procedure("MYPROC");
        assert!(jcl.is_some());
        assert!(jcl.unwrap().contains("TESTPROG"));

        let _ = fs::remove_dir_all(&temp_dir);
    }

    // === Story 102.2: Cataloged procedure expansion ===

    #[test]
    fn test_expand_cataloged_procedure() {
        let mut lib = InMemoryProcLib::new();
        lib.add(
            "MYPROC",
            "//MYPROC PROC HLQ=TEST\n//STEP1  EXEC PGM=MYPROG\n//INPUT  DD DSN=&HLQ..DATA,DISP=SHR\n// PEND\n",
        );

        let in_stream_procs = HashMap::new();
        let global_symbols = SymbolTable::new();
        let mut expander = ProcedureExpander::new(in_stream_procs, global_symbols)
            .with_library(Box::new(lib));

        let mut job = Job::new("TESTJOB");
        let mut step = Step::procedure(Some("RUN".to_string()), "MYPROC");
        step.params.other.insert("HLQ".to_string(), "PROD".to_string());
        job.add_step(step);

        let expanded = expander.expand(&job).unwrap();

        let steps = expanded.steps();
        assert_eq!(steps.len(), 1);
        if let ExecType::Program(ref pgm) = steps[0].exec {
            assert_eq!(pgm, "MYPROG");
        } else {
            panic!("Expected Program after expansion");
        }

        let dd = &steps[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            assert_eq!(def.dsn, "PROD.DATA");
        } else {
            panic!("Expected Dataset");
        }
    }

    // === Story 102.3: DD overrides ===

    #[test]
    fn test_dd_override_replace() {
        let mut procs = HashMap::new();
        procs.insert(
            "MYPROC".to_string(),
            InStreamProc {
                name: "MYPROC".to_string(),
                defaults: SymbolTable::new(),
                statements: vec![
                    ProcStatement {
                        name: Some("STEP1".to_string()),
                        operation: "EXEC".to_string(),
                        operands: "PGM=MYPROG".to_string(),
                    },
                    ProcStatement {
                        name: Some("INPUT".to_string()),
                        operation: "DD".to_string(),
                        operands: "DSN=DEFAULT.DATA,DISP=SHR".to_string(),
                    },
                ],
            },
        );

        let override_dd = DdStatement::dataset("INPUT", "OVERRIDE.DATA");
        let overrides = vec![DdOverride {
            step_name: "STEP1".to_string(),
            dd: override_dd,
        }];

        let mut expander = ProcedureExpander::new(procs, SymbolTable::new())
            .with_dd_overrides(overrides);

        let mut job = Job::new("TESTJOB");
        job.add_step(Step::procedure(Some("RUN".to_string()), "MYPROC"));
        job.in_stream_procs = expander.in_stream_procs.clone();

        let expanded = expander.expand(&job).unwrap();

        let steps = expanded.steps();
        let dd = &steps[0].dd_statements[0];
        assert_eq!(dd.name, "INPUT");
        if let DdDefinition::Dataset(ref def) = dd.definition {
            assert_eq!(def.dsn, "OVERRIDE.DATA");
        } else {
            panic!("Expected Dataset");
        }
    }

    #[test]
    fn test_dd_override_add_new() {
        let mut procs = HashMap::new();
        procs.insert(
            "MYPROC".to_string(),
            InStreamProc {
                name: "MYPROC".to_string(),
                defaults: SymbolTable::new(),
                statements: vec![
                    ProcStatement {
                        name: Some("STEP1".to_string()),
                        operation: "EXEC".to_string(),
                        operands: "PGM=MYPROG".to_string(),
                    },
                    ProcStatement {
                        name: Some("INPUT".to_string()),
                        operation: "DD".to_string(),
                        operands: "DSN=DEFAULT.DATA,DISP=SHR".to_string(),
                    },
                ],
            },
        );

        let extra_dd = DdStatement::dataset("EXTRA", "NEW.DATA");
        let overrides = vec![DdOverride {
            step_name: "STEP1".to_string(),
            dd: extra_dd,
        }];

        let mut expander = ProcedureExpander::new(procs, SymbolTable::new())
            .with_dd_overrides(overrides);

        let mut job = Job::new("TESTJOB");
        job.add_step(Step::procedure(Some("RUN".to_string()), "MYPROC"));
        job.in_stream_procs = expander.in_stream_procs.clone();

        let expanded = expander.expand(&job).unwrap();

        // Should have original INPUT DD plus new EXTRA DD
        let steps = expanded.steps();
        assert_eq!(steps[0].dd_statements.len(), 2);
        assert_eq!(steps[0].dd_statements[0].name, "INPUT");
        assert_eq!(steps[0].dd_statements[1].name, "EXTRA");
        if let DdDefinition::Dataset(ref def) = steps[0].dd_statements[1].definition {
            assert_eq!(def.dsn, "NEW.DATA");
        } else {
            panic!("Expected Dataset");
        }
    }

    // === Story 102.4: Nested procedure expansion ===

    #[test]
    fn test_nested_procedure_expansion() {
        let mut procs = HashMap::new();

        // PROC-C is the innermost
        procs.insert(
            "PROCC".to_string(),
            InStreamProc {
                name: "PROCC".to_string(),
                defaults: {
                    let mut d = SymbolTable::new();
                    d.insert("MSG".to_string(), "HELLO".to_string());
                    d
                },
                statements: vec![
                    ProcStatement {
                        name: Some("STEP1".to_string()),
                        operation: "EXEC".to_string(),
                        operands: "PGM=PROGC".to_string(),
                    },
                ],
            },
        );

        // PROC-B calls PROC-C
        procs.insert(
            "PROCB".to_string(),
            InStreamProc {
                name: "PROCB".to_string(),
                defaults: SymbolTable::new(),
                statements: vec![
                    ProcStatement {
                        name: Some("STEP1".to_string()),
                        operation: "EXEC".to_string(),
                        operands: "PROCC".to_string(), // Bare name = procedure call
                    },
                ],
            },
        );

        // PROC-A calls PROC-B
        procs.insert(
            "PROCA".to_string(),
            InStreamProc {
                name: "PROCA".to_string(),
                defaults: SymbolTable::new(),
                statements: vec![
                    ProcStatement {
                        name: Some("STEP1".to_string()),
                        operation: "EXEC".to_string(),
                        operands: "PROCB".to_string(),
                    },
                ],
            },
        );

        let mut expander = ProcedureExpander::new(procs, SymbolTable::new());

        let mut job = Job::new("TESTJOB");
        job.add_step(Step::procedure(Some("RUN".to_string()), "PROCA"));
        job.in_stream_procs = expander.in_stream_procs.clone();

        let expanded = expander.expand(&job).unwrap();

        // Should resolve all the way down to PGM=PROGC
        let steps = expanded.steps();
        assert_eq!(steps.len(), 1);
        if let ExecType::Program(ref pgm) = steps[0].exec {
            assert_eq!(pgm, "PROGC");
        } else {
            panic!("Expected Program PROGC after nested expansion");
        }
    }

    #[test]
    fn test_nesting_depth_limit() {
        // Create a chain that exceeds 15 levels
        let mut procs = HashMap::new();
        for i in 0..=MAX_NESTING_DEPTH {
            let name = format!("PROC{}", i);
            let next = if i < MAX_NESTING_DEPTH {
                format!("PROC{}", i + 1)
            } else {
                "PGM=FINAL".to_string()
            };
            procs.insert(
                name.clone(),
                InStreamProc {
                    name: name.clone(),
                    defaults: SymbolTable::new(),
                    statements: vec![ProcStatement {
                        name: Some("STEP1".to_string()),
                        operation: "EXEC".to_string(),
                        operands: next,
                    }],
                },
            );
        }

        let mut expander = ProcedureExpander::new(procs, SymbolTable::new());

        let mut job = Job::new("TESTJOB");
        job.add_step(Step::procedure(Some("RUN".to_string()), "PROC0"));
        job.in_stream_procs = expander.in_stream_procs.clone();

        let result = expander.expand(&job);
        assert!(result.is_err());
        let err = format!("{}", result.unwrap_err());
        assert!(
            err.contains("maximum depth of 15"),
            "Error should mention depth limit: {}",
            err
        );
    }

    // === In-stream procedure expansion (from Batch 5) ===

    #[test]
    fn test_expand_in_stream_procedure() {
        let mut procs = HashMap::new();
        procs.insert(
            "MYPROC".to_string(),
            InStreamProc {
                name: "MYPROC".to_string(),
                defaults: {
                    let mut d = SymbolTable::new();
                    d.insert("HLQ".to_string(), "TEST".to_string());
                    d
                },
                statements: vec![
                    ProcStatement {
                        name: Some("STEP1".to_string()),
                        operation: "EXEC".to_string(),
                        operands: "PGM=MYPROG".to_string(),
                    },
                    ProcStatement {
                        name: Some("INPUT".to_string()),
                        operation: "DD".to_string(),
                        operands: "DSN=&HLQ..DATA,DISP=SHR".to_string(),
                    },
                ],
            },
        );

        let global_symbols = SymbolTable::new();
        let mut expander = ProcedureExpander::new(procs, global_symbols);

        let mut job = Job::new("TESTJOB");
        let mut step = Step::procedure(Some("RUN".to_string()), "MYPROC");
        step.params.other.insert("HLQ".to_string(), "PROD".to_string());
        job.add_step(step);
        job.in_stream_procs = expander.in_stream_procs.clone();

        let expanded = expander.expand(&job).unwrap();

        let steps = expanded.steps();
        assert_eq!(steps.len(), 1);
        if let ExecType::Program(ref pgm) = steps[0].exec {
            assert_eq!(pgm, "MYPROG");
        } else {
            panic!("Expected Program after expansion");
        }

        assert_eq!(steps[0].dd_statements.len(), 1);
        if let DdDefinition::Dataset(ref def) = steps[0].dd_statements[0].definition {
            assert_eq!(def.dsn, "PROD.DATA");
        } else {
            panic!("Expected Dataset definition");
        }
    }

    #[test]
    fn test_expand_procedure_with_defaults() {
        let mut procs = HashMap::new();
        procs.insert(
            "MYPROC".to_string(),
            InStreamProc {
                name: "MYPROC".to_string(),
                defaults: {
                    let mut d = SymbolTable::new();
                    d.insert("HLQ".to_string(), "TEST".to_string());
                    d
                },
                statements: vec![
                    ProcStatement {
                        name: Some("STEP1".to_string()),
                        operation: "EXEC".to_string(),
                        operands: "PGM=MYPROG".to_string(),
                    },
                    ProcStatement {
                        name: Some("INPUT".to_string()),
                        operation: "DD".to_string(),
                        operands: "DSN=&HLQ..DATA,DISP=SHR".to_string(),
                    },
                ],
            },
        );

        let global_symbols = SymbolTable::new();
        let mut expander = ProcedureExpander::new(procs, global_symbols);

        let mut job = Job::new("TESTJOB");
        let step = Step::procedure(Some("RUN".to_string()), "MYPROC");
        job.add_step(step);
        job.in_stream_procs = expander.in_stream_procs.clone();

        let expanded = expander.expand(&job).unwrap();

        let steps = expanded.steps();
        assert_eq!(steps.len(), 1);
        if let DdDefinition::Dataset(ref def) = steps[0].dd_statements[0].definition {
            assert_eq!(def.dsn, "TEST.DATA");
        } else {
            panic!("Expected Dataset definition");
        }
    }
}
