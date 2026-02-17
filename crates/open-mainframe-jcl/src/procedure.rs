//! Procedure expansion for JCL in-stream and cataloged procedures.
//!
//! The `ProcedureExpander` runs as a preprocessing pass after parsing,
//! resolving `ExecType::Procedure` references by expanding in-stream
//! procedure bodies with symbolic parameter substitution.

use std::collections::HashMap;

use open_mainframe_lang_core::Span;

use crate::ast::*;
use crate::error::JclError;
use crate::lexer::{tokenize_operands, Token};

/// Expands procedure references in a parsed JCL job.
///
/// Resolves `ExecType::Procedure(name)` by looking up in-stream procedures,
/// substituting symbolic parameters, and replacing the procedure call with
/// the expanded steps.
pub struct ProcedureExpander {
    /// In-stream procedures available for expansion.
    in_stream_procs: HashMap<String, InStreamProc>,
    /// Global symbol table from SET statements.
    global_symbols: SymbolTable,
    /// Counter for generating temporary dataset names.
    temp_counter: u32,
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
        }
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

        for step in &job.steps {
            match &step.exec {
                ExecType::Program(_) => {
                    expanded_job.add_step(step.clone());
                }
                ExecType::Procedure(proc_name) => {
                    let expanded_steps =
                        self.expand_procedure(proc_name, &step.params, &step.name, step.span)?;
                    for expanded_step in expanded_steps {
                        expanded_job.span = expanded_job.span.extend(expanded_step.span);
                        expanded_job.add_step(expanded_step);
                    }
                }
            }
        }

        Ok(expanded_job)
    }

    /// Expand a single procedure reference into its constituent steps.
    fn expand_procedure(
        &mut self,
        proc_name: &str,
        exec_params: &ExecParams,
        caller_name: &Option<String>,
        span: Span,
    ) -> Result<Vec<Step>, JclError> {
        let proc = self
            .in_stream_procs
            .get(proc_name)
            .ok_or_else(|| JclError::ParseError {
                message: format!("Procedure '{}' not found", proc_name),
            })?
            .clone();

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
            let substituted_operands = substitute_symbols_with_table(&symbols, &proc_stmt.operands, &mut self.temp_counter);

            match proc_stmt.operation.as_str() {
                "EXEC" => {
                    // Finish previous step if any
                    if let Some(step) = current_step.take() {
                        steps.push(step);
                    }

                    // Parse the EXEC operands
                    let tokens = tokenize_operands(&substituted_operands)?;
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
                                                exec_type =
                                                    Some(ExecType::Program(v.clone()));
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
                                            if let Token::Ident(v) | Token::String(v) =
                                                &tokens[idx]
                                            {
                                                params.other.insert(key.clone(), v.clone());
                                            }
                                        }
                                    }
                                    idx += 1;
                                }
                            } else if exec_type.is_none() {
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
                        message: format!(
                            "EXEC in procedure '{}' missing PGM",
                            proc_name
                        ),
                    })?;

                    // Prefix step name with caller name if available
                    let step_name = proc_stmt.name.as_ref().map(|n| {
                        if let Some(ref caller) = caller_name {
                            format!("{}.{}", caller, n)
                        } else {
                            n.clone()
                        }
                    });

                    current_step = Some(Step {
                        name: step_name,
                        exec: exec_type,
                        params,
                        dd_statements: Vec::new(),
                        span,
                    });
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

        Ok(steps)
    }
}

/// Perform symbolic substitution on text using a given symbol table.
///
/// This is a standalone function used by the procedure expander.
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
                let temp_name = format!("SYS{:05}.T{:06}.TEMP", *temp_counter, *temp_counter);
                result.push_str(&temp_name);
                // Handle period after temp name
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

    // Check for DUMMY
    if matches!(&tokens[0], Token::Ident(s) if s == "DUMMY") {
        return Ok(DdStatement {
            name: dd_name.to_string(),
            definition: DdDefinition::Dummy,
            span,
        });
    }

    // Check for inline data (*)
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

    // Check for SYSOUT
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

    // Parse as dataset DD (simplified for procedure expansion)
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
                            // Collect DSN value
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
                            // Simplified disposition parsing
                            let mut status = DispStatus::Shr;
                            if matches!(tokens[idx], Token::LParen) {
                                idx += 1;
                            }
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
                            // Skip remaining DISP params
                            while idx < tokens.len() && !matches!(tokens[idx], Token::RParen) {
                                idx += 1;
                            }
                            if idx < tokens.len() && matches!(tokens[idx], Token::RParen) {
                                idx += 1;
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
        definition: DdDefinition::Dataset(DatasetDef {
            dsn,
            disp,
            ..Default::default()
        }),
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

        // Build a job with a procedure call
        let mut job = Job::new("TESTJOB");
        let mut step = Step::procedure(Some("RUN".to_string()), "MYPROC");
        step.params.other.insert("HLQ".to_string(), "PROD".to_string());
        job.add_step(step);

        // Also give the job the in_stream_procs
        job.in_stream_procs = expander.in_stream_procs.clone();

        let expanded = expander.expand(&job).unwrap();

        assert_eq!(expanded.steps.len(), 1);
        if let ExecType::Program(ref pgm) = expanded.steps[0].exec {
            assert_eq!(pgm, "MYPROG");
        } else {
            panic!("Expected Program after expansion");
        }

        // Check DD has expanded DSN
        assert_eq!(expanded.steps[0].dd_statements.len(), 1);
        if let DdDefinition::Dataset(ref def) = expanded.steps[0].dd_statements[0].definition {
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

        // Build a job calling procedure with NO overrides
        let mut job = Job::new("TESTJOB");
        let step = Step::procedure(Some("RUN".to_string()), "MYPROC");
        job.add_step(step);
        job.in_stream_procs = expander.in_stream_procs.clone();

        let expanded = expander.expand(&job).unwrap();

        assert_eq!(expanded.steps.len(), 1);
        if let DdDefinition::Dataset(ref def) = expanded.steps[0].dd_statements[0].definition {
            assert_eq!(def.dsn, "TEST.DATA");
        } else {
            panic!("Expected Dataset definition");
        }
    }
}
