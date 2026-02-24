//! JCL parser.
//!
//! Parses JCL statements into an AST.

use std::collections::HashMap;

use open_mainframe_lang_core::Span;

use crate::ast::*;
use crate::error::JclError;
use crate::lexer::{tokenize_operands, JclStatement, Lexer, Token};

/// JCL parser.
pub struct Parser<'a> {
    /// Source JCL.
    source: &'a str,
    /// Parsed statements.
    statements: Vec<JclStatement>,
    /// Current statement index.
    current: usize,
    /// Inline data collected during parsing (step index -> data lines).
    #[allow(dead_code)]
    inline_data: HashMap<usize, Vec<String>>,
    /// Symbol table for symbolic parameter substitution.
    symbols: SymbolTable,
    /// In-stream procedures found during parsing.
    in_stream_procs: HashMap<String, InStreamProc>,
    /// Counter for generating temporary dataset names.
    temp_counter: u32,
    /// JCLLIB ORDER — procedure library search order.
    jcllib_order: Vec<String>,
    /// DD overrides for procedure steps (stepname.ddname).
    dd_overrides: Vec<DdOverride>,
}

impl<'a> Parser<'a> {
    /// Create a new parser for JCL source.
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            statements: Vec::new(),
            current: 0,
            inline_data: HashMap::new(),
            symbols: SymbolTable::new(),
            in_stream_procs: HashMap::new(),
            temp_counter: 0,
            jcllib_order: Vec::new(),
            dd_overrides: Vec::new(),
        }
    }

    /// Parse the JCL source into a Job.
    pub fn parse(&mut self) -> Result<Job, JclError> {
        // Lexical analysis
        let mut lexer = Lexer::new(self.source);
        self.statements = lexer.parse_statements()?;

        // Must start with JOB statement
        if self.statements.is_empty() {
            return Err(JclError::ParseError {
                message: "Empty JCL - no statements found".to_string(),
            });
        }

        let first = &self.statements[0];
        if first.operation != "JOB" {
            return Err(JclError::ParseError {
                message: format!(
                    "JCL must start with JOB statement, found {}",
                    first.operation
                ),
            });
        }

        // Parse JOB statement
        let mut job = self.parse_job_statement()?;
        self.current += 1;

        // First pass: collect SET statements and PROC/PEND blocks
        // We do this before parsing steps so symbols are available for substitution
        self.collect_symbols_and_procs()?;

        // Reset to parse steps
        self.current = 1; // After JOB statement

        // Parse entries (EXEC + DD statements, IF/THEN/ELSE/ENDIF)
        let entries = self.parse_entries(&mut job, 0)?;
        job.entries = entries;

        // Transfer collected symbols, procs, jcllib, overrides, and output stmts to the job
        job.symbols = self.symbols.clone();
        job.in_stream_procs = self.in_stream_procs.clone();
        job.jcllib_order = self.jcllib_order.clone();
        job.dd_overrides = self.dd_overrides.clone();
        // output_stmts are populated during parse_entries via parse_output_statement

        Ok(job)
    }

    /// First pass: collect SET statement values, JCLLIB ORDER, and PROC/PEND blocks.
    fn collect_symbols_and_procs(&mut self) -> Result<(), JclError> {
        let mut idx = 1; // Skip JOB statement
        while idx < self.statements.len() {
            let stmt = &self.statements[idx];
            match stmt.operation.as_str() {
                "SET" => {
                    let operands = stmt.operands.clone();
                    self.parse_set_operands(&operands)?;
                    idx += 1;
                }
                "JCLLIB" => {
                    let operands = stmt.operands.clone();
                    self.jcllib_order = Self::parse_jcllib_operands(&operands);
                    idx += 1;
                }
                "PROC" => {
                    let proc = self.parse_proc_block(idx)?;
                    // Skip past the PEND
                    idx += 1; // past PROC
                    while idx < self.statements.len() {
                        let s = &self.statements[idx];
                        idx += 1;
                        if s.operation == "PEND" {
                            break;
                        }
                    }
                    self.in_stream_procs.insert(proc.name.clone(), proc);
                }
                _ => {
                    idx += 1;
                }
            }
        }
        Ok(())
    }

    /// Parse SET statement operands and add to symbol table.
    ///
    /// Handles formats like: `HLQ=PROD,ENV=DAILY` or `HLQ=PROD`
    fn parse_set_operands(&mut self, operands: &str) -> Result<(), JclError> {
        // SET operands are in the form NAME=VALUE,NAME=VALUE,...
        for assignment in operands.split(',') {
            let assignment = assignment.trim();
            if assignment.is_empty() {
                continue;
            }
            if let Some(eq_pos) = assignment.find('=') {
                let name = assignment[..eq_pos].trim().to_uppercase();
                let value = assignment[eq_pos + 1..].trim().to_string();
                // Strip quotes if present
                let value = if value.starts_with('\'') && value.ends_with('\'') && value.len() >= 2
                {
                    value[1..value.len() - 1].to_string()
                } else {
                    value
                };
                self.symbols.insert(name, value);
            }
        }
        Ok(())
    }

    /// Parse a PROC...PEND block starting at the given index.
    fn parse_proc_block(&self, start_idx: usize) -> Result<InStreamProc, JclError> {
        let proc_stmt = &self.statements[start_idx];
        let proc_name = proc_stmt
            .name
            .clone()
            .ok_or_else(|| JclError::ParseError {
                message: format!("PROC statement must have a name at line {}", proc_stmt.line),
            })?;

        // Parse PROC defaults from operands (e.g., "HLQ=TEST,ENV=DEV")
        let mut defaults = SymbolTable::new();
        for assignment in proc_stmt.operands.split(',') {
            let assignment = assignment.trim();
            if assignment.is_empty() {
                continue;
            }
            if let Some(eq_pos) = assignment.find('=') {
                let name = assignment[..eq_pos].trim().to_uppercase();
                let value = assignment[eq_pos + 1..].trim().to_string();
                defaults.insert(name, value);
            }
        }

        // Collect statements between PROC and PEND
        let mut body_stmts = Vec::new();
        let mut idx = start_idx + 1;
        while idx < self.statements.len() {
            let stmt = &self.statements[idx];
            if stmt.operation == "PEND" {
                break;
            }
            body_stmts.push(ProcStatement {
                name: stmt.name.clone(),
                operation: stmt.operation.clone(),
                operands: stmt.operands.clone(),
            });
            idx += 1;
        }

        if idx >= self.statements.len() {
            return Err(JclError::ParseError {
                message: format!(
                    "PROC {} at line {} has no matching PEND",
                    proc_name, proc_stmt.line
                ),
            });
        }

        Ok(InStreamProc {
            name: proc_name,
            defaults,
            statements: body_stmts,
        })
    }

    /// Skip past a PROC/PEND block during the second pass.
    fn skip_proc_block(&mut self) -> Result<(), JclError> {
        self.current += 1; // Skip PROC
        while self.current < self.statements.len() {
            let stmt = &self.statements[self.current];
            self.current += 1;
            if stmt.operation == "PEND" {
                return Ok(());
            }
        }
        Err(JclError::ParseError {
            message: "PROC without matching PEND".to_string(),
        })
    }

    /// Parse JCLLIB ORDER operands into a list of library dataset names.
    ///
    /// Handles: `ORDER=(PROD.PROCLIB,TEST.PROCLIB)` or `ORDER=SINGLE.PROCLIB`
    fn parse_jcllib_operands(operands: &str) -> Vec<String> {
        let mut libs = Vec::new();
        // Look for ORDER= keyword
        let upper = operands.to_uppercase();
        if let Some(pos) = upper.find("ORDER=") {
            let value_start = pos + 6;
            let rest = &operands[value_start..].trim();
            if rest.starts_with('(') {
                // Multiple libraries in parens
                let end = rest.find(')').unwrap_or(rest.len());
                let inner = &rest[1..end];
                for lib in inner.split(',') {
                    let lib = lib.trim().trim_matches('\'');
                    if !lib.is_empty() {
                        libs.push(lib.to_string());
                    }
                }
            } else {
                // Single library
                let end = rest
                    .find(|c: char| c == ',' || c.is_whitespace())
                    .unwrap_or(rest.len());
                let lib = rest[..end].trim().trim_matches('\'');
                if !lib.is_empty() {
                    libs.push(lib.to_string());
                }
            }
        }
        libs
    }

    /// Parse a DD statement from pre-tokenized tokens (for DD overrides).
    fn parse_dd_from_tokens(
        &self,
        dd_name: &str,
        tokens: &[Token],
        span: Span,
    ) -> Result<DdStatement, JclError> {
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
            let class = self.parse_sysout_class(tokens)?;
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

        let def = self.parse_dataset_def(tokens)?;
        Ok(DdStatement {
            name: dd_name.to_string(),
            definition: DdDefinition::Dataset(Box::new(def)),
            span,
        })
    }

    /// Parse a JOB statement.
    fn parse_job_statement(&self) -> Result<Job, JclError> {
        let stmt = &self.statements[0];
        let name = stmt.name.clone().unwrap_or_else(|| "UNNAMED".to_string());
        let mut job = Job::new(name);
        job.span = Span::main(stmt.byte_offset, stmt.byte_end);

        // Parse operands
        let tokens = tokenize_operands(&stmt.operands)?;
        let mut params = JobParams::default();
        let mut idx = 0;

        // First positional: accounting info
        if idx < tokens.len() {
            if let Token::LParen = &tokens[idx] {
                // Accounting in parens
                idx += 1;
                let mut acct = String::new();
                while idx < tokens.len() {
                    if let Token::RParen = &tokens[idx] {
                        idx += 1;
                        break;
                    }
                    match &tokens[idx] {
                        Token::Ident(s) | Token::String(s) => acct.push_str(s),
                        Token::Number(n) => acct.push_str(&n.to_string()),
                        Token::Comma => acct.push(','),
                        _ => {}
                    }
                    idx += 1;
                }
                params.accounting = Some(acct);
                // Skip comma after accounting
                if idx < tokens.len() && matches!(tokens[idx], Token::Comma) {
                    idx += 1;
                }
            }
        }

        // Second positional: programmer name
        if idx < tokens.len() {
            if let Token::String(s) = &tokens[idx] {
                params.programmer = Some(s.clone());
                idx += 1;
                // Skip comma
                if idx < tokens.len() && matches!(tokens[idx], Token::Comma) {
                    idx += 1;
                }
            }
        }

        // Keyword parameters
        while idx < tokens.len() {
            if let Token::Ident(key) = &tokens[idx] {
                idx += 1;
                if idx < tokens.len() && matches!(tokens[idx], Token::Equals) {
                    idx += 1;
                    if idx < tokens.len() {
                        match key.as_str() {
                            "CLASS" => {
                                if let Token::Ident(v) = &tokens[idx] {
                                    params.class = v.chars().next();
                                }
                            }
                            "MSGCLASS" => {
                                if let Token::Ident(v) = &tokens[idx] {
                                    params.msgclass = v.chars().next();
                                }
                            }
                            "NOTIFY" => {
                                if let Token::Ident(v) | Token::String(v) = &tokens[idx] {
                                    params.notify = Some(v.clone());
                                }
                            }
                            "REGION" => {
                                if let Token::Number(n) = &tokens[idx] {
                                    params.region = Some(*n as u32);
                                } else if let Token::Ident(v) = &tokens[idx] {
                                    // Handle "0M", "4M", etc.
                                    let digits: String = v.chars().take_while(|c| c.is_ascii_digit()).collect();
                                    if let Ok(n) = digits.parse::<u32>() {
                                        params.region = Some(n * 1024); // Convert M to KB
                                    }
                                }
                            }
                            // Epic 105: Extended JOB parameters
                            "TIME" => {
                                if let Token::LParen = &tokens[idx] {
                                    idx += 1;
                                    let mut mins = 0u32;
                                    let mut secs = 0u32;
                                    if let Token::Number(n) = &tokens[idx] {
                                        mins = *n as u32;
                                        idx += 1;
                                    }
                                    if idx < tokens.len() && matches!(tokens[idx], Token::Comma) {
                                        idx += 1;
                                        if idx < tokens.len() {
                                            if let Token::Number(n) = &tokens[idx] {
                                                secs = *n as u32;
                                                idx += 1;
                                            }
                                        }
                                    }
                                    // Skip RParen
                                    if idx < tokens.len() && matches!(tokens[idx], Token::RParen) {
                                        idx += 1;
                                    }
                                    params.time = Some((mins, secs));
                                    // Skip comma and continue
                                    if idx < tokens.len() && matches!(tokens[idx], Token::Comma) {
                                        idx += 1;
                                    }
                                    continue;
                                } else if let Token::Number(n) = &tokens[idx] {
                                    params.time = Some((*n as u32, 0));
                                }
                            }
                            "MSGLEVEL" => {
                                if let Token::LParen = &tokens[idx] {
                                    idx += 1;
                                    let mut stmt_lvl = 0u8;
                                    let mut alloc_lvl = 0u8;
                                    if let Token::Number(n) = &tokens[idx] {
                                        stmt_lvl = *n as u8;
                                        idx += 1;
                                    }
                                    if idx < tokens.len() && matches!(tokens[idx], Token::Comma) {
                                        idx += 1;
                                        if idx < tokens.len() {
                                            if let Token::Number(n) = &tokens[idx] {
                                                alloc_lvl = *n as u8;
                                                idx += 1;
                                            }
                                        }
                                    }
                                    if idx < tokens.len() && matches!(tokens[idx], Token::RParen) {
                                        idx += 1;
                                    }
                                    params.msglevel = Some((stmt_lvl, alloc_lvl));
                                    if idx < tokens.len() && matches!(tokens[idx], Token::Comma) {
                                        idx += 1;
                                    }
                                    continue;
                                } else if let Token::Number(n) = &tokens[idx] {
                                    params.msglevel = Some((*n as u8, 0));
                                }
                            }
                            "TYPRUN" => {
                                if let Token::Ident(v) = &tokens[idx] {
                                    params.typrun = Some(match v.to_uppercase().as_str() {
                                        "HOLD" => crate::ast::TypeRun::Hold,
                                        "SCAN" => crate::ast::TypeRun::Scan,
                                        "COPY" => crate::ast::TypeRun::Copy,
                                        _ => crate::ast::TypeRun::Run,
                                    });
                                }
                            }
                            "MEMLIMIT" => {
                                // MEMLIMIT can be "2G", "512M", etc.
                                // Tokenizer may produce Number(2)+Ident("G") or Ident("2G")
                                match &tokens[idx] {
                                    Token::Ident(v) => {
                                        params.memlimit = Some(v.clone());
                                    }
                                    Token::Number(n) => {
                                        let mut val = n.to_string();
                                        // Check if next token is a suffix like G, M, K
                                        if idx + 1 < tokens.len() {
                                            if let Token::Ident(suffix) = &tokens[idx + 1] {
                                                val.push_str(suffix);
                                                idx += 1;
                                            }
                                        }
                                        params.memlimit = Some(val);
                                    }
                                    _ => {}
                                }
                            }
                            "JOBRC" => {
                                if let Token::Ident(v) = &tokens[idx] {
                                    params.jobrc = Some(v.clone());
                                }
                            }
                            "SCHENV" => {
                                if let Token::Ident(v) = &tokens[idx] {
                                    params.schenv = Some(v.clone());
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
                }
            } else {
                idx += 1;
            }
            // Skip comma
            if idx < tokens.len() && matches!(tokens[idx], Token::Comma) {
                idx += 1;
            }
        }

        job.params = params;
        Ok(job)
    }

    /// Perform text-level symbolic parameter substitution on an operand string.
    ///
    /// Handles:
    /// - `&NAME` → resolved value from symbol table
    /// - `&&NAME` → system-generated temporary dataset name
    /// - `&NAME..` → resolved value followed by a single period (double-period becomes single)
    /// - Unknown `&NAME` → left as-is
    fn substitute_symbols(&mut self, text: &str) -> String {
        let mut result = String::with_capacity(text.len());
        let chars: Vec<char> = text.chars().collect();
        let mut i = 0;

        while i < chars.len() {
            if chars[i] == '&' {
                // Check for double ampersand (temporary dataset name)
                if i + 1 < chars.len() && chars[i + 1] == '&' {
                    // &&NAME → system-generated temp name
                    i += 2; // skip &&
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
                    // Generate a temp dataset name
                    self.temp_counter += 1;
                    let temp_name = format!("SYS{:05}.T{:06}.TEMP", self.temp_counter, self.temp_counter);
                    result.push_str(&temp_name);
                    // Handle double-period after temp name
                    if i < chars.len() && chars[i] == '.' {
                        i += 1; // consume the period
                        // If next char is also a period, that's a double-period; first is consumed
                        // by the symbolic resolution, second remains
                        if i < chars.len() && chars[i] == '.' {
                            result.push('.');
                            i += 1;
                        }
                    }
                } else {
                    // Single ampersand: symbolic parameter
                    i += 1; // skip &
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
                        // Bare & with no name, keep as-is
                        result.push('&');
                        continue;
                    }
                    let upper_name = name.to_uppercase();
                    if let Some(value) = self.symbols.get(&upper_name) {
                        result.push_str(value);
                        // Handle double-period: &HLQ..DATA → value.DATA
                        // The first period is consumed as the symbolic terminator,
                        // the second period becomes a real period
                        if i < chars.len() && chars[i] == '.' {
                            i += 1; // consume first period (symbolic terminator)
                            if i < chars.len() && chars[i] == '.' {
                                result.push('.');
                                i += 1;
                            }
                            // If there's no second period, the single period is just a terminator
                            // and is consumed (standard IBM behavior)
                        }
                    } else {
                        // Unknown symbol — leave as-is
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

    /// Parse job entries until a terminator is reached.
    ///
    /// `depth` tracks IF nesting (max 15). Returns entries and stops at
    /// NULL, ELSE, ENDIF, or end of statements.
    fn parse_entries(
        &mut self,
        job: &mut Job,
        depth: usize,
    ) -> Result<Vec<JobEntry>, JclError> {
        let mut entries = Vec::new();

        while self.current < self.statements.len() {
            let stmt = &self.statements[self.current];
            match stmt.operation.as_str() {
                "EXEC" => {
                    let step = self.parse_step()?;
                    job.span = job.span.extend(step.span);
                    entries.push(JobEntry::Step(Box::new(step)));
                }
                "NULL" => {
                    job.span = job.span.extend(Span::main(stmt.byte_offset, stmt.byte_end));
                    break;
                }
                "SET" => {
                    job.span = job.span.extend(Span::main(stmt.byte_offset, stmt.byte_end));
                    self.current += 1;
                }
                "PROC" => {
                    job.span = job.span.extend(Span::main(stmt.byte_offset, stmt.byte_end));
                    self.skip_proc_block()?;
                }
                "JCLLIB" => {
                    job.span = job.span.extend(Span::main(stmt.byte_offset, stmt.byte_end));
                    self.current += 1;
                }
                "DD" => {
                    let dd_name = stmt.name.clone().unwrap_or_default();
                    if dd_name.contains('.') {
                        let parts: Vec<&str> = dd_name.splitn(2, '.').collect();
                        let step_name = parts[0].to_string();
                        let override_dd_name = parts[1].to_string();
                        let dd_span = Span::main(stmt.byte_offset, stmt.byte_end);
                        let raw_operands = stmt.operands.clone();
                        let substituted_operands = self.substitute_symbols(&raw_operands);
                        let tokens = tokenize_operands(&substituted_operands)?;

                        let dd = self.parse_dd_from_tokens(&override_dd_name, &tokens, dd_span)?;
                        self.dd_overrides.push(DdOverride {
                            step_name,
                            dd,
                        });
                        job.span = job.span.extend(dd_span);
                    } else {
                        job.span = job.span.extend(Span::main(stmt.byte_offset, stmt.byte_end));
                    }
                    self.current += 1;
                }
                "IF" => {
                    if depth >= 15 {
                        return Err(JclError::ParseError {
                            message: format!(
                                "IF nesting exceeds maximum depth of 15 at line {}",
                                stmt.line
                            ),
                        });
                    }
                    let if_construct = self.parse_if_construct(job, depth)?;
                    job.span = job.span.extend(if_construct.span);
                    entries.push(JobEntry::If(if_construct));
                }
                "ELSE" | "ENDIF" => {
                    // These terminate the current entry list — caller handles them
                    break;
                }
                "INCLUDE" => {
                    job.span = job.span.extend(Span::main(stmt.byte_offset, stmt.byte_end));
                    self.current += 1;
                }
                "OUTPUT" => {
                    let output_stmt = self.parse_output_statement()?;
                    job.output_stmts.push(output_stmt);
                    job.span = job.span.extend(Span::main(stmt.byte_offset, stmt.byte_end));
                    self.current += 1;
                }
                _ => {
                    return Err(JclError::ParseError {
                        message: format!(
                            "Unexpected {} statement at line {}",
                            stmt.operation, stmt.line
                        ),
                    });
                }
            }
        }

        Ok(entries)
    }

    /// Parse an IF/THEN/ELSE/ENDIF construct.
    fn parse_if_construct(
        &mut self,
        job: &mut Job,
        depth: usize,
    ) -> Result<IfConstruct, JclError> {
        let if_stmt = &self.statements[self.current];
        let if_span_start = if_stmt.byte_offset;
        let raw_operands = if_stmt.operands.clone();
        let if_line = if_stmt.line;
        self.current += 1;

        // Parse the condition expression from the IF operands
        let condition = Self::parse_condition_expr(&raw_operands).map_err(|_| {
            JclError::ParseError {
                message: format!("Invalid IF condition at line {}", if_line),
            }
        })?;

        // Parse THEN branch entries
        let then_entries = self.parse_entries(job, depth + 1)?;

        // Check for ELSE or ENDIF
        let mut else_entries = Vec::new();
        if self.current < self.statements.len() {
            let stmt = &self.statements[self.current];
            if stmt.operation == "ELSE" {
                self.current += 1;
                else_entries = self.parse_entries(job, depth + 1)?;
            }
        }

        // Expect ENDIF
        let span_end = if self.current < self.statements.len() {
            let stmt = &self.statements[self.current];
            if stmt.operation == "ENDIF" {
                let end = stmt.byte_end;
                self.current += 1;
                end
            } else {
                return Err(JclError::ParseError {
                    message: format!(
                        "Expected ENDIF for IF at line {}, found {} at line {}",
                        if_line, stmt.operation, stmt.line
                    ),
                });
            }
        } else {
            return Err(JclError::ParseError {
                message: format!("Missing ENDIF for IF at line {}", if_line),
            });
        };

        Ok(IfConstruct {
            condition,
            then_entries,
            else_entries,
            span: Span::main(if_span_start, span_end),
        })
    }

    /// Parse a condition expression string into a `ConditionExpr`.
    ///
    /// Handles formats like:
    /// - `(STEP1.RC = 0) THEN`
    /// - `(STEP1.RC <= 4 & STEP2.RC = 0) | STEP3.ABEND THEN`
    /// - `STEP1.ABEND THEN`
    /// - `NOT (STEP1.RC > 4) THEN`
    fn parse_condition_expr(operands: &str) -> Result<ConditionExpr, String> {
        // Strip trailing THEN and surrounding whitespace
        let text = operands.trim();
        let text = if text.to_uppercase().ends_with("THEN") {
            text[..text.len() - 4].trim()
        } else {
            text
        };

        Self::parse_or_expr(text)
    }

    /// Parse OR-level expression: `expr | expr | ...`
    fn parse_or_expr(text: &str) -> Result<ConditionExpr, String> {
        let parts = Self::split_logical(text, '|');
        if parts.len() == 1 {
            Self::parse_and_expr(parts[0].trim())
        } else {
            let mut exprs = Vec::new();
            for part in parts {
                exprs.push(Self::parse_and_expr(part.trim())?);
            }
            Ok(ConditionExpr::Or(exprs))
        }
    }

    /// Parse AND-level expression: `expr & expr & ...`
    fn parse_and_expr(text: &str) -> Result<ConditionExpr, String> {
        let parts = Self::split_logical(text, '&');
        if parts.len() == 1 {
            Self::parse_primary_expr(parts[0].trim())
        } else {
            let mut exprs = Vec::new();
            for part in parts {
                exprs.push(Self::parse_primary_expr(part.trim())?);
            }
            Ok(ConditionExpr::And(exprs))
        }
    }

    /// Split text on a logical operator, respecting parentheses.
    fn split_logical(text: &str, op: char) -> Vec<&str> {
        let mut parts = Vec::new();
        let mut depth = 0;
        let mut start = 0;
        let bytes = text.as_bytes();

        for (i, &b) in bytes.iter().enumerate() {
            match b {
                b'(' => depth += 1,
                b')' => depth -= 1,
                _ if b == op as u8 && depth == 0 => {
                    parts.push(&text[start..i]);
                    start = i + 1;
                }
                _ => {}
            }
        }
        parts.push(&text[start..]);
        parts
    }

    /// Parse a primary expression (possibly parenthesized or NOT).
    fn parse_primary_expr(text: &str) -> Result<ConditionExpr, String> {
        let text = text.trim();

        // Handle NOT
        if text.to_uppercase().starts_with("NOT ") || text.to_uppercase().starts_with("NOT(") {
            let inner = if text.to_uppercase().starts_with("NOT ") {
                text[4..].trim()
            } else {
                text[3..].trim()
            };
            let expr = Self::parse_primary_expr(inner)?;
            return Ok(ConditionExpr::Not(Box::new(expr)));
        }

        // Handle parenthesized expression
        if text.starts_with('(') {
            // Find matching close paren
            let mut depth = 0;
            let mut end = 0;
            for (i, c) in text.chars().enumerate() {
                match c {
                    '(' => depth += 1,
                    ')' => {
                        depth -= 1;
                        if depth == 0 {
                            end = i;
                            break;
                        }
                    }
                    _ => {}
                }
            }
            if end == 0 {
                return Err("Unmatched parenthesis".to_string());
            }
            let inner = &text[1..end];
            return Self::parse_or_expr(inner);
        }

        // Parse atomic condition: STEP1.RC op value, STEP1.ABEND, STEP1.RUN
        Self::parse_atomic_condition(text)
    }

    /// Parse an atomic condition like `STEP1.RC = 0` or `STEP1.ABEND`.
    fn parse_atomic_condition(text: &str) -> Result<ConditionExpr, String> {
        let text = text.trim();

        // Split on first '.' to get step name and property
        let dot_pos = text.find('.').ok_or_else(|| {
            format!("Invalid condition: expected STEP.property, got '{}'", text)
        })?;

        let step_name = text[..dot_pos].trim().to_string();
        let rest = text[dot_pos + 1..].trim();

        // Check what follows the dot
        let rest_upper = rest.to_uppercase();

        if rest_upper == "ABEND" {
            return Ok(ConditionExpr::Abend { step_name });
        }

        if rest_upper == "RUN" {
            return Ok(ConditionExpr::Run { step_name });
        }

        // Must be RC comparison or ABENDCC comparison
        // Find the comparison operator
        let (property, op_and_value) = if rest_upper.starts_with("RC") {
            ("RC", rest[2..].trim())
        } else if rest_upper.starts_with("ABENDCC") {
            ("ABENDCC", rest[7..].trim())
        } else {
            return Err(format!(
                "Unknown property in condition: '{}'",
                rest
            ));
        };

        // Parse operator
        let (operator, value_str) = if let Some(rest) = op_and_value.strip_prefix("<=") {
            (ConditionOperator::Le, rest.trim())
        } else if let Some(rest) = op_and_value.strip_prefix(">=") {
            (ConditionOperator::Ge, rest.trim())
        } else if let Some(rest) = op_and_value.strip_prefix("!=") {
            (ConditionOperator::Ne, rest.trim())
        } else if let Some(rest) = op_and_value.strip_prefix("<>") {
            (ConditionOperator::Ne, rest.trim())
        } else if let Some(rest) = op_and_value.strip_prefix('<') {
            (ConditionOperator::Lt, rest.trim())
        } else if let Some(rest) = op_and_value.strip_prefix('>') {
            (ConditionOperator::Gt, rest.trim())
        } else if let Some(rest) = op_and_value.strip_prefix('=') {
            (ConditionOperator::Eq, rest.trim())
        } else {
            return Err(format!(
                "Invalid operator in condition: '{}'",
                op_and_value
            ));
        };

        if property == "RC" {
            let value: u32 = value_str
                .parse()
                .map_err(|_| format!("Invalid RC value: '{}'", value_str))?;
            Ok(ConditionExpr::RcCompare {
                step_name,
                operator,
                value,
            })
        } else {
            // ABENDCC
            Ok(ConditionExpr::AbendCc {
                step_name,
                operator,
                value: value_str.to_string(),
            })
        }
    }

    /// Parse an EXEC statement and its associated DDs.
    fn parse_step(&mut self) -> Result<Step, JclError> {
        let stmt = &self.statements[self.current];
        let step_name = stmt.name.clone();
        let raw_operands = stmt.operands.clone();
        let stmt_line = stmt.line;

        // Apply symbolic substitution to operands before tokenizing
        let substituted_operands = self.substitute_symbols(&raw_operands);
        let tokens = tokenize_operands(&substituted_operands)?;
        let mut idx = 0;
        let mut exec_type = None;
        let mut params = ExecParams::default();

        // Parse keyword parameters
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
                                let parm = self.collect_parm_value(&tokens, &mut idx);
                                params.parm = Some(parm);
                                continue; // idx already advanced
                            }
                            "REGION" => {
                                if let Token::Number(n) = &tokens[idx] {
                                    params.region = Some(*n as u32);
                                }
                            }
                            "COND" => {
                                let cond_result = self.parse_exec_cond(&tokens, &mut idx);
                                params.cond = cond_result.0;
                                params.cond_special = cond_result.1;
                                continue;
                            }
                            "TIME" => {
                                if let Token::LParen = &tokens[idx] {
                                    idx += 1;
                                    let mut mins = 0u32;
                                    let mut secs = 0u32;
                                    if idx < tokens.len() {
                                        if let Token::Number(n) = &tokens[idx] {
                                            mins = *n as u32;
                                            idx += 1;
                                        }
                                    }
                                    if idx < tokens.len() && matches!(tokens[idx], Token::Comma) {
                                        idx += 1;
                                        if idx < tokens.len() {
                                            if let Token::Number(n) = &tokens[idx] {
                                                secs = *n as u32;
                                                idx += 1;
                                            }
                                        }
                                    }
                                    if idx < tokens.len() && matches!(tokens[idx], Token::RParen) {
                                        idx += 1;
                                    }
                                    params.time = Some((mins, secs));
                                    if idx < tokens.len() && matches!(tokens[idx], Token::Comma) {
                                        idx += 1;
                                    }
                                    continue;
                                } else if let Token::Number(n) = &tokens[idx] {
                                    params.time = Some((*n as u32, 0));
                                }
                            }
                            _ => {
                                // Collect dotted names (e.g., AWS.M2.CARDDEMO.CNTL)
                                // for symbolic parameter overrides in procedures.
                                if let Token::Ident(v) | Token::String(v) = &tokens[idx] {
                                    let mut full_name = v.clone();
                                    idx += 1;
                                    while idx + 1 < tokens.len() {
                                        if matches!(tokens[idx], Token::Period) {
                                            if let Token::Ident(ref next) = tokens[idx + 1] {
                                                full_name.push('.');
                                                full_name.push_str(next);
                                                idx += 2;
                                            } else {
                                                break;
                                            }
                                        } else {
                                            break;
                                        }
                                    }
                                    params.other.insert(key.clone(), full_name);
                                } else {
                                    idx += 1;
                                }
                            }
                        }
                        // Advance past the value token for standard keywords.
                        // The _ (default) case already advanced idx in its loop.
                        match key.as_str() {
                            "PGM" | "PROC" | "PARM" | "REGION" | "COND" | "TIME" => { idx += 1; }
                            _ => {} // already advanced
                        }
                    }
                } else if exec_type.is_none() {
                    // Bare name could be procedure name
                    exec_type = Some(ExecType::Procedure(key.clone()));
                }
            } else {
                idx += 1;
            }
            // Skip comma
            if idx < tokens.len() && matches!(tokens[idx], Token::Comma) {
                idx += 1;
            }
        }

        let exec_type = exec_type.ok_or_else(|| JclError::ParseError {
            message: format!("EXEC statement missing PGM or PROC at line {}", stmt_line),
        })?;

        let exec_span = Span::main(
            self.statements[self.current].byte_offset,
            self.statements[self.current].byte_end,
        );
        let mut step = Step {
            name: step_name,
            exec: exec_type,
            params,
            dd_statements: Vec::new(),
            span: exec_span,
        };

        self.current += 1;

        // Parse DD statements for this step (with concatenation support).
        // An unnamed DD (no name field) following a named DD is a concatenation.
        while self.current < self.statements.len() {
            let stmt = &self.statements[self.current];
            if stmt.operation != "DD" {
                break;
            }

            let is_unnamed = stmt.name.is_none();
            let dd = self.parse_dd_statement()?;

            if is_unnamed && !step.dd_statements.is_empty() {
                // This is a concatenation — add to the previous DD
                let prev = step.dd_statements.last_mut().unwrap();
                step.span = step.span.extend(dd.span);
                self.add_to_concatenation(prev, dd);
            } else {
                step.span = step.span.extend(dd.span);
                step.add_dd(dd);
            }
            self.current += 1;
        }

        Ok(step)
    }

    /// Collect PARM value (may be quoted string or in parens).
    fn collect_parm_value(&self, tokens: &[Token], idx: &mut usize) -> String {
        match &tokens[*idx] {
            Token::String(s) => {
                *idx += 1;
                s.clone()
            }
            Token::LParen => {
                *idx += 1;
                let mut parm = String::new();
                let mut depth = 1;
                while *idx < tokens.len() && depth > 0 {
                    match &tokens[*idx] {
                        Token::LParen => {
                            parm.push('(');
                            depth += 1;
                        }
                        Token::RParen => {
                            depth -= 1;
                            if depth > 0 {
                                parm.push(')');
                            }
                        }
                        Token::Ident(s) => parm.push_str(s),
                        Token::String(s) => {
                            parm.push('\'');
                            parm.push_str(s);
                            parm.push('\'');
                        }
                        Token::Number(n) => parm.push_str(&n.to_string()),
                        Token::Comma => parm.push(','),
                        Token::Equals => parm.push('='),
                        _ => {}
                    }
                    *idx += 1;
                }
                parm
            }
            Token::Ident(s) => {
                *idx += 1;
                s.clone()
            }
            _ => {
                *idx += 1;
                String::new()
            }
        }
    }

    /// Parse an OUTPUT JCL statement.
    fn parse_output_statement(&self) -> Result<OutputStatement, JclError> {
        let stmt = &self.statements[self.current];
        let name = stmt.name.clone().unwrap_or_else(|| "OUT".to_string());
        let raw_operands = stmt.operands.clone();
        let tokens = tokenize_operands(&raw_operands)?;

        let mut class = None;
        let mut dest = None;
        let mut copies = None;
        let mut forms = None;
        let mut writer = None;
        let mut other = HashMap::new();

        let mut idx = 0;
        while idx < tokens.len() {
            if let Token::Ident(key) = &tokens[idx] {
                idx += 1;
                if idx < tokens.len() && matches!(tokens[idx], Token::Equals) {
                    idx += 1;
                    if idx < tokens.len() {
                        match key.as_str() {
                            "CLASS" => {
                                if let Token::Ident(v) = &tokens[idx] {
                                    class = v.chars().next();
                                }
                            }
                            "DEST" => {
                                if let Token::Ident(v) | Token::String(v) = &tokens[idx] {
                                    dest = Some(v.clone());
                                }
                            }
                            "COPIES" => {
                                if let Token::Number(n) = &tokens[idx] {
                                    copies = Some(*n as u32);
                                }
                            }
                            "FORMS" => {
                                if let Token::Ident(v) | Token::String(v) = &tokens[idx] {
                                    forms = Some(v.clone());
                                }
                            }
                            "WRITER" => {
                                if let Token::Ident(v) | Token::String(v) = &tokens[idx] {
                                    writer = Some(v.clone());
                                }
                            }
                            _ => {
                                if let Token::Ident(v) | Token::String(v) = &tokens[idx] {
                                    other.insert(key.clone(), v.clone());
                                }
                            }
                        }
                        idx += 1;
                    }
                }
            } else {
                idx += 1;
            }
            if idx < tokens.len() && matches!(tokens[idx], Token::Comma) {
                idx += 1;
            }
        }

        Ok(OutputStatement {
            name,
            class,
            dest,
            copies,
            forms,
            writer,
            other,
        })
    }

    /// Convert a DD definition to a concatenation by adding another DD to it.
    fn add_to_concatenation(&self, prev: &mut DdStatement, new_dd: DdStatement) {
        // Extract the dataset def from the new DD
        let new_def = match new_dd.definition {
            DdDefinition::Dataset(def) => *def,
            _ => return, // Only datasets can be concatenated
        };

        match prev.definition {
            DdDefinition::Dataset(ref old_box) => {
                // Convert single dataset to concatenation
                let old_def = (**old_box).clone();
                prev.definition = DdDefinition::Concatenation(vec![old_def, new_def]);
            }
            DdDefinition::Concatenation(ref mut datasets) => {
                // Add to existing concatenation
                datasets.push(new_def);
            }
            _ => {} // Can't concatenate non-dataset DDs
        }
    }

    /// Parse a DD statement.
    fn parse_dd_statement(&mut self) -> Result<DdStatement, JclError> {
        let stmt = &self.statements[self.current];
        let dd_name = stmt.name.clone().unwrap_or_else(|| "UNNAMED".to_string());
        let dd_span = Span::main(stmt.byte_offset, stmt.byte_end);
        let raw_operands = stmt.operands.clone();

        // Apply symbolic substitution to DD operands
        let substituted_operands = self.substitute_symbols(&raw_operands);
        let tokens = tokenize_operands(&substituted_operands)?;

        // Check for special DD types first
        if tokens.is_empty() {
            let mut dd = DdStatement::dummy(dd_name);
            dd.span = dd_span;
            return Ok(dd);
        }

        // Check for DUMMY
        if matches!(&tokens[0], Token::Ident(s) if s == "DUMMY") {
            let mut dd = DdStatement::dummy(dd_name);
            dd.span = dd_span;
            return Ok(dd);
        }

        // Check for inline data (*)
        if matches!(&tokens[0], Token::Asterisk) {
            let inline_data = self.statements[self.current].inline_data.clone();
            let mut dd = DdStatement::inline(dd_name, inline_data);
            dd.span = dd_span;
            return Ok(dd);
        }

        // Check for SYSOUT
        if matches!(&tokens[0], Token::Ident(s) if s == "SYSOUT") {
            let class = self.parse_sysout_class(&tokens)?;
            let mut dd = DdStatement::sysout(dd_name, class);
            dd.span = dd_span;
            return Ok(dd);
        }

        // Check for PATH= (USS file)
        let has_path = tokens.iter().enumerate().any(|(i, t)| {
            if let Token::Ident(k) = t {
                k == "PATH"
                    && tokens.get(i + 1).map_or(false, |t2| matches!(t2, Token::Equals))
            } else {
                false
            }
        });

        if has_path {
            let def = self.parse_uss_file_def(&tokens)?;
            return Ok(DdStatement {
                name: dd_name,
                definition: DdDefinition::UssFile(def),
                span: dd_span,
            });
        }

        // Parse as dataset DD
        let def = self.parse_dataset_def(&tokens)?;
        Ok(DdStatement {
            name: dd_name,
            definition: DdDefinition::Dataset(Box::new(def)),
            span: dd_span,
        })
    }

    /// Parse SYSOUT class.
    fn parse_sysout_class(&self, tokens: &[Token]) -> Result<char, JclError> {
        let mut idx = 0;
        while idx < tokens.len() {
            if let Token::Ident(key) = &tokens[idx] {
                if key == "SYSOUT" {
                    idx += 1;
                    if idx < tokens.len() && matches!(tokens[idx], Token::Equals) {
                        idx += 1;
                        if idx < tokens.len() {
                            return match &tokens[idx] {
                                Token::Asterisk => Ok('*'),
                                Token::Ident(s) => Ok(s.chars().next().unwrap_or('A')),
                                _ => Ok('A'),
                            };
                        }
                    }
                }
            }
            idx += 1;
        }
        Ok('*')
    }

    /// Parse EXEC COND parameter.
    ///
    /// Handles:
    /// - `COND=(code,op)` — single condition
    /// - `COND=((code,op),(code,op,step))` — multiple conditions
    /// - `COND=((code,op),EVEN)` or `COND=((code,op),ONLY)`
    /// - `COND=EVEN` / `COND=ONLY` — special modes without conditions
    fn parse_exec_cond(
        &self,
        tokens: &[Token],
        idx: &mut usize,
    ) -> (Option<Vec<Condition>>, Option<CondSpecial>) {
        let mut conditions = Vec::new();
        let mut special = None;

        // Check for simple EVEN/ONLY
        if let Token::Ident(v) = &tokens[*idx] {
            match v.to_uppercase().as_str() {
                "EVEN" => {
                    *idx += 1;
                    return (None, Some(CondSpecial::Even));
                }
                "ONLY" => {
                    *idx += 1;
                    return (None, Some(CondSpecial::Only));
                }
                _ => {}
            }
        }

        if !matches!(tokens[*idx], Token::LParen) {
            *idx += 1;
            return (None, None);
        }
        *idx += 1; // skip outer LParen

        // Check if first token inside is another LParen (list of conditions)
        // or a Number (single condition): COND=(4,LT) vs COND=((4,LT),(8,LT,STEP1))
        let is_list = matches!(tokens.get(*idx), Some(Token::LParen));

        if is_list {
            // List of conditions and optional EVEN/ONLY
            while *idx < tokens.len() {
                if matches!(tokens[*idx], Token::RParen) {
                    *idx += 1;
                    break;
                }
                if matches!(tokens[*idx], Token::LParen) {
                    if let Some(cond) = self.parse_single_cond(tokens, idx) {
                        conditions.push(cond);
                    }
                } else if let Token::Ident(v) = &tokens[*idx] {
                    match v.to_uppercase().as_str() {
                        "EVEN" => special = Some(CondSpecial::Even),
                        "ONLY" => special = Some(CondSpecial::Only),
                        _ => {}
                    }
                    *idx += 1;
                } else {
                    *idx += 1;
                }
                // Skip comma
                if *idx < tokens.len() && matches!(tokens[*idx], Token::Comma) {
                    *idx += 1;
                }
            }
        } else {
            // Single condition: COND=(4,LT) or COND=(4,LT,STEP1)
            if let Some(cond) = self.parse_single_cond_inner(tokens, idx) {
                conditions.push(cond);
            }
            // Skip closing RParen
            if *idx < tokens.len() && matches!(tokens[*idx], Token::RParen) {
                *idx += 1;
            }
        }

        let conds = if conditions.is_empty() {
            None
        } else {
            Some(conditions)
        };
        (conds, special)
    }

    /// Parse a single parenthesized condition: (code,op) or (code,op,step) or (code,op,step.procstep).
    fn parse_single_cond(&self, tokens: &[Token], idx: &mut usize) -> Option<Condition> {
        if !matches!(tokens.get(*idx), Some(Token::LParen)) {
            return None;
        }
        *idx += 1; // skip LParen
        let result = self.parse_single_cond_inner(tokens, idx);
        // skip RParen
        if *idx < tokens.len() && matches!(tokens[*idx], Token::RParen) {
            *idx += 1;
        }
        result
    }

    /// Parse the interior of a single condition: code,op[,step[.procstep]].
    fn parse_single_cond_inner(&self, tokens: &[Token], idx: &mut usize) -> Option<Condition> {
        // code
        let code = if let Some(Token::Number(n)) = tokens.get(*idx) {
            *idx += 1;
            *n as u32
        } else {
            return None;
        };

        // comma
        if *idx < tokens.len() && matches!(tokens[*idx], Token::Comma) {
            *idx += 1;
        }

        // operator
        let operator = if let Some(Token::Ident(op)) = tokens.get(*idx) {
            let op = match op.to_uppercase().as_str() {
                "GT" => ConditionOperator::Gt,
                "GE" => ConditionOperator::Ge,
                "EQ" => ConditionOperator::Eq,
                "NE" => ConditionOperator::Ne,
                "LT" => ConditionOperator::Lt,
                "LE" => ConditionOperator::Le,
                _ => return None,
            };
            *idx += 1;
            op
        } else {
            return None;
        };

        let mut step = None;
        let mut procstep = None;

        // optional: comma + step name
        if *idx < tokens.len() && matches!(tokens[*idx], Token::Comma) {
            *idx += 1;
            if let Some(Token::Ident(s)) = tokens.get(*idx) {
                step = Some(s.clone());
                *idx += 1;
                // optional: period + procstep
                if *idx < tokens.len() && matches!(tokens[*idx], Token::Period) {
                    *idx += 1;
                    if let Some(Token::Ident(ps)) = tokens.get(*idx) {
                        procstep = Some(ps.clone());
                        *idx += 1;
                    }
                }
            }
        }

        Some(Condition {
            code,
            operator,
            step,
            procstep,
        })
    }

    /// Parse dataset definition.
    fn parse_dataset_def(&self, tokens: &[Token]) -> Result<DatasetDef, JclError> {
        let mut def = DatasetDef::default();
        let mut idx = 0;

        while idx < tokens.len() {
            if let Token::Ident(key) = &tokens[idx] {
                idx += 1;
                if idx < tokens.len() && matches!(tokens[idx], Token::Equals) {
                    idx += 1;
                    if idx < tokens.len() {
                        match key.as_str() {
                            "DSN" | "DSNAME" => {
                                let (dsn, gdg) = self.collect_dsn_with_gdg(tokens, &mut idx);
                                def.dsn = dsn;
                                def.gdg_generation = gdg;
                                continue;
                            }
                            "DISP" => {
                                def.disp = Some(self.parse_disposition(tokens, &mut idx)?);
                                continue;
                            }
                            "UNIT" => {
                                if let Token::Ident(v) = &tokens[idx] {
                                    def.unit = Some(v.clone());
                                }
                            }
                            "VOL" | "VOLUME" => {
                                // Parse VOL=SER=xxx
                                if let Token::Ident(v) = &tokens[idx] {
                                    if v == "SER" {
                                        idx += 1;
                                        if idx < tokens.len()
                                            && matches!(tokens[idx], Token::Equals)
                                        {
                                            idx += 1;
                                            if let Token::Ident(ser) = &tokens[idx] {
                                                def.vol_ser = Some(ser.clone());
                                            }
                                        }
                                    }
                                }
                            }
                            "SPACE" => {
                                def.space = Some(self.parse_space(tokens, &mut idx)?);
                                continue;
                            }
                            "DCB" => {
                                def.dcb = Some(self.parse_dcb(tokens, &mut idx)?);
                                continue;
                            }
                            "AMP" => {
                                def.amp = Some(self.parse_amp(tokens, &mut idx)?);
                                continue;
                            }
                            // Epic 104: Extended DD parameters
                            "STORCLAS" => {
                                if let Token::Ident(v) = &tokens[idx] {
                                    def.storclas = Some(v.clone());
                                }
                            }
                            "DATACLAS" => {
                                if let Token::Ident(v) = &tokens[idx] {
                                    def.dataclas = Some(v.clone());
                                }
                            }
                            "MGMTCLAS" => {
                                if let Token::Ident(v) = &tokens[idx] {
                                    def.mgmtclas = Some(v.clone());
                                }
                            }
                            "EXPDT" => {
                                match &tokens[idx] {
                                    Token::Ident(v) => {
                                        def.expdt = Some(v.clone());
                                    }
                                    Token::Number(n) => {
                                        def.expdt = Some(n.to_string());
                                    }
                                    _ => {}
                                }
                            }
                            "RETPD" => {
                                match &tokens[idx] {
                                    Token::Number(n) => {
                                        def.retpd = Some(*n as u32);
                                    }
                                    Token::Ident(v) => {
                                        if let Ok(n) = v.parse::<u32>() {
                                            def.retpd = Some(n);
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            "DSNTYPE" => {
                                if let Token::Ident(v) = &tokens[idx] {
                                    def.dsntype = Some(match v.to_uppercase().as_str() {
                                        "PDS" => crate::ast::DsType::Pds,
                                        "LIBRARY" => crate::ast::DsType::Library,
                                        "LARGE" => crate::ast::DsType::Large,
                                        "EXTREQ" => crate::ast::DsType::Extreq,
                                        "EXTPREF" => crate::ast::DsType::Extpref,
                                        "BASIC" => crate::ast::DsType::Basic,
                                        "HFS" => crate::ast::DsType::Hfs,
                                        "PIPE" => crate::ast::DsType::Pipe,
                                        _ => crate::ast::DsType::Basic,
                                    });
                                }
                            }
                            "LIKE" => {
                                def.like = Some(self.collect_dsn(tokens, &mut idx));
                                continue;
                            }
                            "REFDD" => {
                                if let Token::Ident(v) = &tokens[idx] {
                                    def.refdd = Some(v.clone());
                                }
                            }
                            "KEYLEN" => {
                                match &tokens[idx] {
                                    Token::Number(n) => {
                                        def.keylen = Some(*n as u32);
                                    }
                                    Token::Ident(v) => {
                                        if let Ok(n) = v.parse::<u32>() {
                                            def.keylen = Some(n);
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            "KEYOFF" => {
                                match &tokens[idx] {
                                    Token::Number(n) => {
                                        def.keyoff = Some(*n as u32);
                                    }
                                    Token::Ident(v) => {
                                        if let Ok(n) = v.parse::<u32>() {
                                            def.keyoff = Some(n);
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            "LABEL" => {
                                def.label = Some(self.parse_label(tokens, &mut idx));
                                continue;
                            }
                            "AVGREC" => {
                                if let Token::Ident(v) = &tokens[idx] {
                                    def.avgrec = Some(v.clone());
                                }
                            }
                            _ => {}
                        }
                        idx += 1;
                    }
                }
            } else {
                idx += 1;
            }
            // Skip comma
            if idx < tokens.len() && matches!(tokens[idx], Token::Comma) {
                idx += 1;
            }
        }

        Ok(def)
    }

    /// Collect dataset name (may contain qualifiers).
    ///
    /// Also returns a GDG relative generation number if present
    /// (e.g., `MY.GDG(+1)` returns `("MY.GDG", Some(1))`).
    fn collect_dsn_with_gdg(&self, tokens: &[Token], idx: &mut usize) -> (String, Option<i32>) {
        let mut dsn = String::new();
        let mut gdg_gen = None;

        while *idx < tokens.len() {
            match &tokens[*idx] {
                Token::Ident(s) => {
                    dsn.push_str(s);
                }
                Token::Period => {
                    dsn.push('.');
                }
                Token::LParen => {
                    // Check if this is a GDG reference like (+1), (0), (-1)
                    // or a member name like (MEMBER)
                    let saved_idx = *idx;
                    *idx += 1;

                    // Look for +N, -N, or just N (all numeric with optional sign)
                    let mut is_gdg = false;

                    if *idx < tokens.len() {
                        // Check for + or - followed by number, or just a number
                        match &tokens[*idx] {
                            Token::Ident(v) if v == "+" => {
                                *idx += 1;
                                if *idx < tokens.len() {
                                    if let Token::Number(n) = &tokens[*idx] {
                                        gdg_gen = Some(*n as i32);
                                        *idx += 1;
                                        is_gdg = true;
                                    }
                                }
                            }
                            Token::Ident(v) if v == "-" => {
                                *idx += 1;
                                if *idx < tokens.len() {
                                    if let Token::Number(n) = &tokens[*idx] {
                                        gdg_gen = Some(-(*n as i32));
                                        *idx += 1;
                                        is_gdg = true;
                                    }
                                }
                            }
                            Token::Number(n) => {
                                gdg_gen = Some(*n as i32);
                                *idx += 1;
                                is_gdg = true;
                            }
                            _ => {}
                        }
                    }

                    if is_gdg {
                        // Skip RParen
                        if *idx < tokens.len() && matches!(tokens[*idx], Token::RParen) {
                            *idx += 1;
                        }
                        return (dsn, gdg_gen);
                    } else {
                        // Not a GDG, this is a member name in parens
                        *idx = saved_idx;
                        dsn.push('(');
                        *idx += 1;
                        while *idx < tokens.len() {
                            if matches!(tokens[*idx], Token::RParen) {
                                dsn.push(')');
                                *idx += 1;
                                break;
                            }
                            if let Token::Ident(m) = &tokens[*idx] {
                                dsn.push_str(m);
                            }
                            *idx += 1;
                        }
                        return (dsn, None);
                    }
                }
                Token::String(s) => {
                    dsn = s.clone();
                    *idx += 1;
                    return (dsn, None);
                }
                Token::Ampersand => {
                    // Symbolic parameter
                    dsn.push('&');
                }
                _ => break,
            }
            *idx += 1;
        }

        (dsn, gdg_gen)
    }

    /// Collect dataset name (may contain qualifiers).
    fn collect_dsn(&self, tokens: &[Token], idx: &mut usize) -> String {
        self.collect_dsn_with_gdg(tokens, idx).0
    }

    /// Parse DISP parameter.
    fn parse_disposition(
        &self,
        tokens: &[Token],
        idx: &mut usize,
    ) -> Result<Disposition, JclError> {
        let mut status = DispStatus::Shr;
        let mut normal = None;
        let mut abnormal = None;

        // Check if we have parentheses (full DISP syntax)
        let has_parens = *idx < tokens.len() && matches!(tokens[*idx], Token::LParen);
        if has_parens {
            *idx += 1;
        }

        // Status
        if *idx < tokens.len() {
            if let Token::Ident(s) = &tokens[*idx] {
                status = match s.as_str() {
                    "NEW" => DispStatus::New,
                    "OLD" => DispStatus::Old,
                    "SHR" => DispStatus::Shr,
                    "MOD" => DispStatus::Mod,
                    _ => DispStatus::Shr,
                };
                *idx += 1;
            }
        }

        // Only parse normal/abnormal disposition if we had parens
        if has_parens {
            // Skip comma
            if *idx < tokens.len() && matches!(tokens[*idx], Token::Comma) {
                *idx += 1;

                // Normal disposition
                if *idx < tokens.len() {
                    if let Token::Ident(s) = &tokens[*idx] {
                        normal = self.parse_disp_action(s);
                        *idx += 1;
                    }
                }

                // Skip comma
                if *idx < tokens.len() && matches!(tokens[*idx], Token::Comma) {
                    *idx += 1;

                    // Abnormal disposition
                    if *idx < tokens.len() {
                        if let Token::Ident(s) = &tokens[*idx] {
                            abnormal = self.parse_disp_action(s);
                            *idx += 1;
                        }
                    }
                }
            }

            // Skip closing paren
            if *idx < tokens.len() && matches!(tokens[*idx], Token::RParen) {
                *idx += 1;
            }
        }

        Ok(Disposition {
            status,
            normal,
            abnormal,
        })
    }

    /// Parse disposition action.
    fn parse_disp_action(&self, s: &str) -> Option<DispAction> {
        match s {
            "KEEP" => Some(DispAction::Keep),
            "DELETE" => Some(DispAction::Delete),
            "CATLG" => Some(DispAction::Catlg),
            "UNCATLG" => Some(DispAction::Uncatlg),
            "PASS" => Some(DispAction::Pass),
            _ => None,
        }
    }

    /// Parse SPACE parameter.
    fn parse_space(&self, tokens: &[Token], idx: &mut usize) -> Result<Space, JclError> {
        let mut unit = SpaceUnit::Trk;
        let mut primary = 0;
        let mut secondary = None;
        let mut directory = None;

        if *idx < tokens.len() && matches!(tokens[*idx], Token::LParen) {
            *idx += 1;
        }

        // Unit (TRK, CYL, or block size)
        if *idx < tokens.len() {
            match &tokens[*idx] {
                Token::Ident(s) => {
                    unit = match s.as_str() {
                        "TRK" => SpaceUnit::Trk,
                        "CYL" => SpaceUnit::Cyl,
                        _ => SpaceUnit::Trk,
                    };
                    *idx += 1;
                }
                Token::Number(n) => {
                    unit = SpaceUnit::Blk(*n as u32);
                    *idx += 1;
                }
                _ => {}
            }
        }

        // Skip comma
        if *idx < tokens.len() && matches!(tokens[*idx], Token::Comma) {
            *idx += 1;
        }

        // Allocation quantities in parens
        if *idx < tokens.len() && matches!(tokens[*idx], Token::LParen) {
            *idx += 1;

            // Primary
            if *idx < tokens.len() {
                if let Token::Number(n) = &tokens[*idx] {
                    primary = *n as u32;
                    *idx += 1;
                }
            }

            // Skip comma
            if *idx < tokens.len() && matches!(tokens[*idx], Token::Comma) {
                *idx += 1;

                // Secondary
                if *idx < tokens.len() {
                    if let Token::Number(n) = &tokens[*idx] {
                        secondary = Some(*n as u32);
                        *idx += 1;
                    }
                }

                // Skip comma
                if *idx < tokens.len() && matches!(tokens[*idx], Token::Comma) {
                    *idx += 1;

                    // Directory
                    if *idx < tokens.len() {
                        if let Token::Number(n) = &tokens[*idx] {
                            directory = Some(*n as u32);
                            *idx += 1;
                        }
                    }
                }
            }

            // Skip closing paren
            if *idx < tokens.len() && matches!(tokens[*idx], Token::RParen) {
                *idx += 1;
            }
        }

        // Skip outer closing paren
        if *idx < tokens.len() && matches!(tokens[*idx], Token::RParen) {
            *idx += 1;
        }

        Ok(Space {
            unit,
            primary,
            secondary,
            directory,
        })
    }

    /// Parse DCB parameter.
    fn parse_dcb(&self, tokens: &[Token], idx: &mut usize) -> Result<DcbParams, JclError> {
        let mut dcb = DcbParams::default();

        if *idx < tokens.len() && matches!(tokens[*idx], Token::LParen) {
            *idx += 1;
        }

        while *idx < tokens.len() {
            if matches!(tokens[*idx], Token::RParen) {
                *idx += 1;
                break;
            }

            if let Token::Ident(key) = &tokens[*idx] {
                *idx += 1;
                if *idx < tokens.len() && matches!(tokens[*idx], Token::Equals) {
                    *idx += 1;
                    if *idx < tokens.len() {
                        match key.as_str() {
                            "RECFM" => {
                                if let Token::Ident(v) = &tokens[*idx] {
                                    dcb.recfm = match v.as_str() {
                                        "F" => Some(RecordFormat::Fixed),
                                        "FB" => Some(RecordFormat::FixedBlocked),
                                        "V" => Some(RecordFormat::Variable),
                                        "VB" => Some(RecordFormat::VariableBlocked),
                                        "U" => Some(RecordFormat::Undefined),
                                        _ => None,
                                    };
                                }
                            }
                            "LRECL" => {
                                if let Token::Number(n) = &tokens[*idx] {
                                    dcb.lrecl = Some(*n as u32);
                                }
                            }
                            "BLKSIZE" => {
                                if let Token::Number(n) = &tokens[*idx] {
                                    dcb.blksize = Some(*n as u32);
                                }
                            }
                            "DSORG" => {
                                if let Token::Ident(v) = &tokens[*idx] {
                                    dcb.dsorg = match v.as_str() {
                                        "PS" => Some(DatasetOrg::Ps),
                                        "PO" => Some(DatasetOrg::Po),
                                        "DA" => Some(DatasetOrg::Da),
                                        _ => None,
                                    };
                                }
                            }
                            _ => {}
                        }
                        *idx += 1;
                    }
                }
            } else {
                *idx += 1;
            }

            // Skip comma
            if *idx < tokens.len() && matches!(tokens[*idx], Token::Comma) {
                *idx += 1;
            }
        }

        Ok(dcb)
    }

    /// Parse LABEL parameter: `LABEL=(seq,type[,password[,IN|OUT[,expdt]]])`
    fn parse_label(&self, tokens: &[Token], idx: &mut usize) -> LabelDef {
        let mut seq = 1u32;
        let mut label_type = LabelType::Sl;
        let mut password = None;
        let mut in_out = None;
        let mut expdt = None;

        let has_parens = *idx < tokens.len() && matches!(tokens[*idx], Token::LParen);
        if has_parens {
            *idx += 1;
        }

        // Sequence number
        if *idx < tokens.len() {
            if let Token::Number(n) = &tokens[*idx] {
                seq = *n as u32;
                *idx += 1;
            }
        }

        if has_parens {
            // Comma + label type
            if *idx < tokens.len() && matches!(tokens[*idx], Token::Comma) {
                *idx += 1;
                if *idx < tokens.len() {
                    if let Token::Ident(v) = &tokens[*idx] {
                        label_type = match v.to_uppercase().as_str() {
                            "SL" => LabelType::Sl,
                            "NSL" => LabelType::Nsl,
                            "NL" => LabelType::Nl,
                            "SUL" => LabelType::Sul,
                            "BLP" => LabelType::Blp,
                            "AL" => LabelType::Al,
                            "AUL" => LabelType::Aul,
                            _ => LabelType::Sl,
                        };
                        *idx += 1;
                    }
                }
            }

            // Optional: comma + password
            if *idx < tokens.len() && matches!(tokens[*idx], Token::Comma) {
                *idx += 1;
                if *idx < tokens.len() {
                    if let Token::Ident(v) | Token::String(v) = &tokens[*idx] {
                        password = Some(v.clone());
                        *idx += 1;
                    }
                }
            }

            // Optional: comma + IN/OUT
            if *idx < tokens.len() && matches!(tokens[*idx], Token::Comma) {
                *idx += 1;
                if *idx < tokens.len() {
                    if let Token::Ident(v) = &tokens[*idx] {
                        in_out = Some(v.clone());
                        *idx += 1;
                    }
                }
            }

            // Optional: comma + expdt
            if *idx < tokens.len() && matches!(tokens[*idx], Token::Comma) {
                *idx += 1;
                if *idx < tokens.len() {
                    if let Token::Ident(v) = &tokens[*idx] {
                        expdt = Some(v.clone());
                        *idx += 1;
                    }
                }
            }

            // Skip RParen
            if *idx < tokens.len() && matches!(tokens[*idx], Token::RParen) {
                *idx += 1;
            }
        }

        LabelDef {
            sequence: seq,
            label_type,
            password,
            in_out,
            expdt,
        }
    }

    /// Parse a USS file DD statement (PATH=, PATHOPTS=, PATHMODE=, PATHDISP=).
    fn parse_uss_file_def(&self, tokens: &[Token]) -> Result<UssFileDef, JclError> {
        let mut path = String::new();
        let mut pathopts = Vec::new();
        let mut pathmode = Vec::new();
        let mut pathdisp = None;
        let mut idx = 0;

        while idx < tokens.len() {
            if let Token::Ident(key) = &tokens[idx] {
                idx += 1;
                if idx < tokens.len() && matches!(tokens[idx], Token::Equals) {
                    idx += 1;
                    if idx < tokens.len() {
                        match key.as_str() {
                            "PATH" => {
                                // PATH may be a quoted string or an identifier
                                match &tokens[idx] {
                                    Token::String(s) => {
                                        path = s.clone();
                                        idx += 1;
                                    }
                                    Token::Ident(s) => {
                                        // Collect path components joined by periods/slashes
                                        path = s.clone();
                                        idx += 1;
                                    }
                                    _ => { idx += 1; }
                                }
                                continue;
                            }
                            "PATHOPTS" => {
                                pathopts = self.parse_paren_list(tokens, &mut idx);
                                continue;
                            }
                            "PATHMODE" => {
                                pathmode = self.parse_paren_list(tokens, &mut idx);
                                continue;
                            }
                            "PATHDISP" => {
                                pathdisp = Some(self.parse_pathdisp(tokens, &mut idx));
                                continue;
                            }
                            _ => { idx += 1; }
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

        Ok(UssFileDef {
            path,
            pathopts,
            pathmode,
            pathdisp,
        })
    }

    /// Parse a parenthesized list of identifiers: `(A,B,C)` or a single identifier.
    fn parse_paren_list(&self, tokens: &[Token], idx: &mut usize) -> Vec<String> {
        let mut items = Vec::new();
        if *idx < tokens.len() && matches!(tokens[*idx], Token::LParen) {
            *idx += 1;
            while *idx < tokens.len() {
                if matches!(tokens[*idx], Token::RParen) {
                    *idx += 1;
                    break;
                }
                if let Token::Ident(v) = &tokens[*idx] {
                    items.push(v.clone());
                }
                *idx += 1;
                if *idx < tokens.len() && matches!(tokens[*idx], Token::Comma) {
                    *idx += 1;
                }
            }
        } else if let Some(Token::Ident(v)) = tokens.get(*idx) {
            items.push(v.clone());
            *idx += 1;
        }
        items
    }

    /// Parse PATHDISP: `(normal[,abnormal])` or `normal`.
    fn parse_pathdisp(&self, tokens: &[Token], idx: &mut usize) -> (String, Option<String>) {
        let mut normal = String::new();
        let mut abnormal = None;

        if *idx < tokens.len() && matches!(tokens[*idx], Token::LParen) {
            *idx += 1;
            if *idx < tokens.len() {
                if let Token::Ident(v) = &tokens[*idx] {
                    normal = v.clone();
                    *idx += 1;
                }
            }
            if *idx < tokens.len() && matches!(tokens[*idx], Token::Comma) {
                *idx += 1;
                if *idx < tokens.len() {
                    if let Token::Ident(v) = &tokens[*idx] {
                        abnormal = Some(v.clone());
                        *idx += 1;
                    }
                }
            }
            if *idx < tokens.len() && matches!(tokens[*idx], Token::RParen) {
                *idx += 1;
            }
        } else if let Some(Token::Ident(v)) = tokens.get(*idx) {
            normal = v.clone();
            *idx += 1;
        }

        (normal, abnormal)
    }

    /// Parse AMP (Access Method Parameters) for VSAM.
    fn parse_amp(&self, tokens: &[Token], idx: &mut usize) -> Result<AmpParams, JclError> {
        let mut amp = AmpParams::default();

        if *idx < tokens.len() && matches!(tokens[*idx], Token::LParen) {
            *idx += 1;
        }

        while *idx < tokens.len() {
            if matches!(tokens[*idx], Token::RParen) {
                *idx += 1;
                break;
            }

            if let Token::Ident(key) = &tokens[*idx] {
                *idx += 1;
                if *idx < tokens.len() && matches!(tokens[*idx], Token::Equals) {
                    *idx += 1;
                    if *idx < tokens.len() {
                        match key.as_str() {
                            "BUFND" => {
                                if let Token::Number(n) = &tokens[*idx] {
                                    amp.bufnd = Some(*n as u32);
                                }
                            }
                            "BUFNI" => {
                                if let Token::Number(n) = &tokens[*idx] {
                                    amp.bufni = Some(*n as u32);
                                }
                            }
                            "BUFSP" => {
                                if let Token::Number(n) = &tokens[*idx] {
                                    amp.bufsp = Some(*n as u32);
                                }
                            }
                            "STRNO" => {
                                if let Token::Number(n) = &tokens[*idx] {
                                    amp.strno = Some(*n as u32);
                                }
                            }
                            "MODE" => {
                                if let Token::Ident(v) = &tokens[*idx] {
                                    amp.mode = match v.as_str() {
                                        "SEQ" => Some(VsamAccessMode::Sequential),
                                        "DIR" => Some(VsamAccessMode::Direct),
                                        "SKP" => Some(VsamAccessMode::Skip),
                                        _ => None,
                                    };
                                }
                            }
                            _ => {}
                        }
                        *idx += 1;
                    }
                }
            } else {
                *idx += 1;
            }

            // Skip comma
            if *idx < tokens.len() && matches!(tokens[*idx], Token::Comma) {
                *idx += 1;
            }
        }

        Ok(amp)
    }
}

/// Parse JCL source into a Job.
pub fn parse(source: &str) -> Result<Job, JclError> {
    let mut parser = Parser::new(source);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_job() {
        let jcl = r#"//TESTJOB  JOB (ACCT),'PROGRAMMER',CLASS=A
//STEP1    EXEC PGM=IEFBR14
//"#;

        let job = parse(jcl).unwrap();
        assert_eq!(job.name, "TESTJOB");
        assert_eq!(job.params.class, Some('A'));
        let steps = job.steps();
        assert_eq!(steps.len(), 1);

        let step = steps[0];
        assert_eq!(step.name, Some("STEP1".to_string()));
        if let ExecType::Program(ref pgm) = step.exec {
            assert_eq!(pgm, "IEFBR14");
        } else {
            panic!("Expected Program");
        }
    }

    #[test]
    fn test_parse_job_with_dd() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=MYPROC
//INPUT    DD DSN=MY.DATA.SET,DISP=SHR
//OUTPUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=A
//"#;

        let job = parse(jcl).unwrap();
        let steps = job.steps();
        assert_eq!(steps.len(), 1);

        let step = steps[0];
        assert_eq!(step.dd_statements.len(), 3);

        // Check INPUT DD
        let input_dd = &step.dd_statements[0];
        assert_eq!(input_dd.name, "INPUT");
        if let DdDefinition::Dataset(ref def) = input_dd.definition {
            assert_eq!(def.dsn, "MY.DATA.SET");
            assert!(matches!(def.disp.as_ref().unwrap().status, DispStatus::Shr));
        } else {
            panic!("Expected Dataset definition");
        }

        // Check OUTPUT DD
        let output_dd = &step.dd_statements[1];
        assert_eq!(output_dd.name, "OUTPUT");
        if let DdDefinition::Sysout(ref def) = output_dd.definition {
            assert_eq!(def.class, '*');
        } else {
            panic!("Expected Sysout definition");
        }
    }

    #[test]
    fn test_parse_parm() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=MYPROC,PARM='OPTION1,OPTION2'
//"#;

        let job = parse(jcl).unwrap();
        let step = job.steps()[0];
        assert_eq!(step.params.parm, Some("OPTION1,OPTION2".to_string()));
    }

    #[test]
    fn test_parse_dataset_with_member() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=TEST
//INPUT    DD DSN=MY.PDS(MEMBER),DISP=SHR
//"#;

        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            assert_eq!(def.dsn, "MY.PDS(MEMBER)");
        } else {
            panic!("Expected Dataset definition");
        }
    }

    #[test]
    fn test_parse_new_dataset() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=TEST
//OUTPUT   DD DSN=NEW.DATA.SET,DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(TRK,(10,5)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//"#;

        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            assert_eq!(def.dsn, "NEW.DATA.SET");
            let disp = def.disp.as_ref().unwrap();
            assert!(matches!(disp.status, DispStatus::New));
            assert!(matches!(disp.normal, Some(DispAction::Catlg)));
            assert!(matches!(disp.abnormal, Some(DispAction::Delete)));
        } else {
            panic!("Expected Dataset definition");
        }
    }

    #[test]
    fn test_span_tracking() {
        //                       0         1         2
        //                       0123456789012345678901_
        let line1 = "//MYJOB    JOB CLASS=A";  // 22 chars
        let line2 = "//STEP1    EXEC PGM=TEST"; // 24 chars
        let line3 = "//INPUT    DD DSN=MY.DS,DISP=SHR"; // 32 chars
        let line4 = "//";
        let jcl = format!("{}\n{}\n{}\n{}", line1, line2, line3, line4);

        let line2_start = line1.len() as u32 + 1; // +1 for \n
        let line3_start = line2_start + line2.len() as u32 + 1;

        let job = parse(&jcl).unwrap();

        // Job span covers entire source (from JOB through NULL)
        assert_eq!(job.span.start, 0);
        assert!(job.span.end > 0, "Job span end should be non-zero");

        // Step span covers EXEC through last DD
        let steps = job.steps();
        let step = steps[0];
        assert_eq!(step.span.start, line2_start);
        assert!(step.span.end > step.span.start);

        // DD span covers the DD statement
        let dd = &step.dd_statements[0];
        assert_eq!(dd.span.start, line3_start);
        assert!(dd.span.end > dd.span.start);

        // Step span extends to cover the DD
        assert_eq!(step.span.end, dd.span.end);

        // All spans are in the main file
        assert_eq!(job.span.file, open_mainframe_lang_core::FileId::MAIN);
    }

    #[test]
    fn test_parse_vsam_with_amp() {
        let jcl = "//MYJOB    JOB CLASS=A\n//STEP1    EXEC PGM=VSAMPROC\n//VSAMDD   DD DSN=MY.KSDS.CLUSTER,DISP=SHR,\n//             AMP=(BUFND=10,BUFNI=5,MODE=DIR)\n//\n";

        let job = parse(jcl).unwrap();
        let steps = job.steps();
        let step = steps[0];
        assert_eq!(step.dd_statements.len(), 1);
        let dd = &step.dd_statements[0];
        assert_eq!(dd.name, "VSAMDD");
        if let DdDefinition::Dataset(ref def) = dd.definition {
            assert_eq!(def.dsn, "MY.KSDS.CLUSTER");
            assert!(def.disp.is_some());
            let amp = def.amp.as_ref().expect("AMP should be parsed");
            assert_eq!(amp.bufnd, Some(10));
            assert_eq!(amp.bufni, Some(5));
            assert_eq!(amp.mode, Some(VsamAccessMode::Direct));
        } else {
            panic!("Expected Dataset definition");
        }
    }

    // ======================================================================
    // Epic 100: Symbolic Parameter Substitution & SET Statement
    // ======================================================================

    /// Story 100.1: &PROGRAM substitution in EXEC operands.
    #[test]
    fn test_symbolic_substitution_in_exec() {
        let jcl = r#"//MYJOB    JOB CLASS=A
// SET PROGRAM=MYAPP
//STEP1    EXEC PGM=&PROGRAM
//"#;

        let job = parse(jcl).unwrap();
        let steps = job.steps();
        assert_eq!(steps.len(), 1);
        if let ExecType::Program(ref pgm) = steps[0].exec {
            assert_eq!(pgm, "MYAPP");
        } else {
            panic!("Expected Program after symbolic substitution");
        }
    }

    /// Story 100.1: Double-period handling (&HLQ..DATA.&ENV → PROD.DATA.DAILY).
    #[test]
    fn test_symbolic_substitution_double_period() {
        let jcl = r#"//MYJOB    JOB CLASS=A
// SET HLQ=PROD,ENV=DAILY
//STEP1    EXEC PGM=TEST
//INPUT    DD DSN=&HLQ..DATA.&ENV,DISP=SHR
//"#;

        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            assert_eq!(def.dsn, "PROD.DATA.DAILY");
        } else {
            panic!("Expected Dataset definition");
        }
    }

    /// Story 100.1: &&TEMP resolves to a system-generated temporary name.
    #[test]
    fn test_symbolic_substitution_temp_dataset() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=TEST
//WORK     DD DSN=&&TEMP,DISP=(NEW,PASS)
//"#;

        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            // Should be a system-generated temp name, not &&TEMP
            assert!(!def.dsn.contains("&&"), "&&TEMP should be resolved: {}", def.dsn);
            assert!(def.dsn.contains("SYS"), "Temp name should start with SYS: {}", def.dsn);
            assert!(def.dsn.contains("TEMP"), "Temp name should contain TEMP: {}", def.dsn);
        } else {
            panic!("Expected Dataset definition");
        }
    }

    /// Story 100.2: SET statement parsing — symbols added to symbol table.
    #[test]
    fn test_set_statement_parsing() {
        let jcl = r#"//MYJOB    JOB CLASS=A
// SET HLQ=PROD,ENV=DAILY
//STEP1    EXEC PGM=TEST
//"#;

        let job = parse(jcl).unwrap();
        assert_eq!(job.symbols.get("HLQ"), Some(&"PROD".to_string()));
        assert_eq!(job.symbols.get("ENV"), Some(&"DAILY".to_string()));
    }

    /// Story 100.2: Later SET overrides earlier SET.
    #[test]
    fn test_set_statement_override() {
        let jcl = r#"//MYJOB    JOB CLASS=A
// SET HLQ=TEST
// SET HLQ=PROD
//STEP1    EXEC PGM=TEST
//INPUT    DD DSN=&HLQ..DATA,DISP=SHR
//"#;

        let job = parse(jcl).unwrap();
        // Later SET should override
        assert_eq!(job.symbols.get("HLQ"), Some(&"PROD".to_string()));

        // The substituted DSN should use PROD
        let dd = &job.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            assert_eq!(def.dsn, "PROD.DATA");
        } else {
            panic!("Expected Dataset definition");
        }
    }

    /// Story 100.1: Multiple symbolic parameters in one operand.
    #[test]
    fn test_multiple_symbolics_in_operand() {
        let jcl = r#"//MYJOB    JOB CLASS=A
// SET HLQ=PROD,APP=BATCH,ENV=TEST
//STEP1    EXEC PGM=TEST
//INPUT    DD DSN=&HLQ..&APP..&ENV..DATA,DISP=SHR
//"#;

        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            assert_eq!(def.dsn, "PROD.BATCH.TEST.DATA");
        } else {
            panic!("Expected Dataset definition");
        }
    }

    // ======================================================================
    // Epic 101: In-Stream Procedure Support (PROC/PEND)
    // ======================================================================

    /// Story 101.1: In-stream procedure parsed with defaults.
    #[test]
    fn test_parse_in_stream_proc() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//MYPROC   PROC HLQ=TEST
//STEP1    EXEC PGM=MYPROG
//INPUT    DD DSN=&HLQ..DATA,DISP=SHR
//         PEND
//RUN      EXEC MYPROC
//"#;

        let job = parse(jcl).unwrap();

        // Procedure should be captured
        assert!(job.in_stream_procs.contains_key("MYPROC"));
        let proc = &job.in_stream_procs["MYPROC"];
        assert_eq!(proc.name, "MYPROC");
        assert_eq!(proc.defaults.get("HLQ"), Some(&"TEST".to_string()));
        assert_eq!(proc.statements.len(), 2); // EXEC and DD
    }

    /// Story 101.2: Procedure expansion with override (HLQ=PROD).
    #[test]
    fn test_proc_expansion_with_override() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//MYPROC   PROC HLQ=TEST
//STEP1    EXEC PGM=MYPROG
//INPUT    DD DSN=&HLQ..DATA,DISP=SHR
//         PEND
//RUN      EXEC MYPROC,HLQ=PROD
//"#;

        let job = parse(jcl).unwrap();

        // The job should have one step (the procedure call)
        assert_eq!(job.steps().len(), 1);

        // Expand the procedure
        let mut expander = crate::procedure::ProcedureExpander::new(
            job.in_stream_procs.clone(),
            job.symbols.clone(),
        );
        let expanded = expander.expand(&job).unwrap();

        // Should have one expanded step with PGM=MYPROG
        let exp_steps = expanded.steps();
        assert_eq!(exp_steps.len(), 1);
        if let ExecType::Program(ref pgm) = exp_steps[0].exec {
            assert_eq!(pgm, "MYPROG");
        } else {
            panic!("Expected Program after expansion");
        }

        // DD should have DSN=PROD.DATA (HLQ overridden to PROD)
        let dd = &exp_steps[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            assert_eq!(def.dsn, "PROD.DATA");
        } else {
            panic!("Expected Dataset definition");
        }
    }

    /// Story 101.2: Procedure expansion with defaults (no override).
    #[test]
    fn test_proc_expansion_with_defaults() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//MYPROC   PROC HLQ=TEST
//STEP1    EXEC PGM=MYPROG
//INPUT    DD DSN=&HLQ..DATA,DISP=SHR
//         PEND
//RUN      EXEC MYPROC
//"#;

        let job = parse(jcl).unwrap();

        let mut expander = crate::procedure::ProcedureExpander::new(
            job.in_stream_procs.clone(),
            job.symbols.clone(),
        );
        let expanded = expander.expand(&job).unwrap();

        // DD should have DSN=TEST.DATA (default HLQ=TEST from PROC)
        let dd = &expanded.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            assert_eq!(def.dsn, "TEST.DATA");
        } else {
            panic!("Expected Dataset definition");
        }
    }

    /// Story 101.1: PROC without matching PEND produces an error.
    #[test]
    fn test_proc_without_pend_error() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//MYPROC   PROC HLQ=TEST
//STEP1    EXEC PGM=MYPROG
//"#;

        let result = parse(jcl);
        assert!(result.is_err());
        let err = result.unwrap_err();
        let msg = format!("{}", err);
        assert!(msg.contains("PEND"), "Error should mention PEND: {}", msg);
    }

    /// Story 101.1: Multiple defaults on PROC statement.
    #[test]
    fn test_proc_multiple_defaults() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//MYPROC   PROC HLQ=TEST,ENV=DEV,APP=BATCH
//STEP1    EXEC PGM=MYPROG
//INPUT    DD DSN=&HLQ..&APP..&ENV,DISP=SHR
//         PEND
//RUN      EXEC MYPROC,HLQ=PROD
//"#;

        let job = parse(jcl).unwrap();
        let proc = &job.in_stream_procs["MYPROC"];
        assert_eq!(proc.defaults.get("HLQ"), Some(&"TEST".to_string()));
        assert_eq!(proc.defaults.get("ENV"), Some(&"DEV".to_string()));
        assert_eq!(proc.defaults.get("APP"), Some(&"BATCH".to_string()));

        // Expand with only HLQ overridden
        let mut expander = crate::procedure::ProcedureExpander::new(
            job.in_stream_procs.clone(),
            job.symbols.clone(),
        );
        let expanded = expander.expand(&job).unwrap();
        let dd = &expanded.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            // HLQ=PROD (overridden), APP=BATCH (default), ENV=DEV (default)
            assert_eq!(def.dsn, "PROD.BATCH.DEV");
        } else {
            panic!("Expected Dataset definition");
        }
    }

    /// SET + PROC integration: SET provides global symbol, PROC provides defaults.
    #[test]
    fn test_set_and_proc_integration() {
        let jcl = r#"//MYJOB    JOB CLASS=A
// SET GLOBAL=SHARED
//MYPROC   PROC HLQ=TEST
//STEP1    EXEC PGM=MYPROG
//INPUT    DD DSN=&GLOBAL..&HLQ..DATA,DISP=SHR
//         PEND
//RUN      EXEC MYPROC
//"#;

        let job = parse(jcl).unwrap();

        let mut expander = crate::procedure::ProcedureExpander::new(
            job.in_stream_procs.clone(),
            job.symbols.clone(),
        );
        let expanded = expander.expand(&job).unwrap();

        let dd = &expanded.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            assert_eq!(def.dsn, "SHARED.TEST.DATA");
        } else {
            panic!("Expected Dataset definition");
        }
    }

    /// Procedure with SYSOUT DD.
    #[test]
    fn test_proc_expansion_with_sysout() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//MYPROC   PROC
//STEP1    EXEC PGM=MYPROG
//SYSOUT   DD SYSOUT=*
//         PEND
//RUN      EXEC MYPROC
//"#;

        let job = parse(jcl).unwrap();

        let mut expander = crate::procedure::ProcedureExpander::new(
            job.in_stream_procs.clone(),
            job.symbols.clone(),
        );
        let expanded = expander.expand(&job).unwrap();

        let exp_steps = expanded.steps();
        assert_eq!(exp_steps.len(), 1);
        let dd = &exp_steps[0].dd_statements[0];
        assert_eq!(dd.name, "SYSOUT");
        if let DdDefinition::Sysout(ref def) = dd.definition {
            assert_eq!(def.class, '*');
        } else {
            panic!("Expected Sysout definition, got {:?}", dd.definition);
        }
    }

    // ======================================================================
    // Epic 103: IF/THEN/ELSE/ENDIF Conditional Processing
    // ======================================================================

    /// Story 103.1: Simple RC comparison condition expression.
    #[test]
    fn test_parse_condition_expr_rc_eq() {
        let expr =
            Parser::parse_condition_expr("(STEP1.RC = 0) THEN").unwrap();
        if let ConditionExpr::RcCompare {
            step_name,
            operator,
            value,
        } = &expr
        {
            assert_eq!(step_name, "STEP1");
            assert_eq!(*operator, ConditionOperator::Eq);
            assert_eq!(*value, 0);
        } else {
            panic!("Expected RcCompare, got {:?}", expr);
        }
    }

    /// Story 103.1: Complex condition with AND and OR.
    #[test]
    fn test_parse_condition_expr_complex() {
        let expr = Parser::parse_condition_expr(
            "(STEP1.RC <= 4 & STEP2.RC = 0) | STEP3.ABEND THEN",
        )
        .unwrap();
        if let ConditionExpr::Or(parts) = &expr {
            assert_eq!(parts.len(), 2);
            if let ConditionExpr::And(and_parts) = &parts[0] {
                assert_eq!(and_parts.len(), 2);
                // Check first AND operand: STEP1.RC <= 4
                if let ConditionExpr::RcCompare {
                    step_name,
                    operator,
                    value,
                } = &and_parts[0]
                {
                    assert_eq!(step_name, "STEP1");
                    assert_eq!(*operator, ConditionOperator::Le);
                    assert_eq!(*value, 4);
                } else {
                    panic!("Expected RcCompare for STEP1");
                }
                // Check second AND operand: STEP2.RC = 0
                if let ConditionExpr::RcCompare {
                    step_name,
                    operator,
                    value,
                } = &and_parts[1]
                {
                    assert_eq!(step_name, "STEP2");
                    assert_eq!(*operator, ConditionOperator::Eq);
                    assert_eq!(*value, 0);
                } else {
                    panic!("Expected RcCompare for STEP2");
                }
            } else {
                panic!("Expected And expression");
            }
            // Check OR operand: STEP3.ABEND
            if let ConditionExpr::Abend { step_name } = &parts[1] {
                assert_eq!(step_name, "STEP3");
            } else {
                panic!("Expected Abend for STEP3");
            }
        } else {
            panic!("Expected Or expression, got {:?}", expr);
        }
    }

    /// Story 103.2: Parse IF/THEN/ELSE/ENDIF into AST.
    #[test]
    fn test_parse_if_then_else_endif() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=PROG1
// IF (STEP1.RC = 0) THEN
//STEP2    EXEC PGM=PROG2
// ELSE
//STEP3    EXEC PGM=PROG3
// ENDIF
//"#;

        let job = parse(jcl).unwrap();

        // Should have 2 entries: STEP1 (Step) and IF construct
        assert_eq!(job.entries.len(), 2);

        // First entry is a regular step
        if let JobEntry::Step(ref step) = job.entries[0] {
            assert_eq!(step.name, Some("STEP1".to_string()));
        } else {
            panic!("Expected Step for first entry");
        }

        // Second entry is an IF construct
        if let JobEntry::If(ref if_construct) = job.entries[1] {
            assert_eq!(if_construct.then_entries.len(), 1);
            assert_eq!(if_construct.else_entries.len(), 1);

            // THEN branch has STEP2
            if let JobEntry::Step(ref step) = if_construct.then_entries[0] {
                assert_eq!(step.name, Some("STEP2".to_string()));
                if let ExecType::Program(ref pgm) = step.exec {
                    assert_eq!(pgm, "PROG2");
                }
            } else {
                panic!("Expected Step in THEN branch");
            }

            // ELSE branch has STEP3
            if let JobEntry::Step(ref step) = if_construct.else_entries[0] {
                assert_eq!(step.name, Some("STEP3".to_string()));
                if let ExecType::Program(ref pgm) = step.exec {
                    assert_eq!(pgm, "PROG3");
                }
            } else {
                panic!("Expected Step in ELSE branch");
            }
        } else {
            panic!("Expected If construct for second entry");
        }
    }

    /// Story 103.2: Parse IF/THEN/ENDIF without ELSE.
    #[test]
    fn test_parse_if_then_endif_no_else() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=PROG1
// IF (STEP1.RC = 0) THEN
//STEP2    EXEC PGM=PROG2
// ENDIF
//"#;

        let job = parse(jcl).unwrap();

        assert_eq!(job.entries.len(), 2);
        if let JobEntry::If(ref if_construct) = job.entries[1] {
            assert_eq!(if_construct.then_entries.len(), 1);
            assert!(if_construct.else_entries.is_empty());
        } else {
            panic!("Expected If construct");
        }
    }

    /// Story 103.2: Nested IF/THEN/ELSE/ENDIF.
    #[test]
    fn test_parse_nested_if() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=PROG1
// IF (STEP1.RC = 0) THEN
// IF (STEP1.RC <= 4) THEN
//STEP2    EXEC PGM=PROG2
// ENDIF
// ELSE
//STEP3    EXEC PGM=PROG3
// ENDIF
//"#;

        let job = parse(jcl).unwrap();

        assert_eq!(job.entries.len(), 2);
        if let JobEntry::If(ref outer) = job.entries[1] {
            // THEN branch contains nested IF
            assert_eq!(outer.then_entries.len(), 1);
            if let JobEntry::If(ref inner) = outer.then_entries[0] {
                assert_eq!(inner.then_entries.len(), 1);
                if let JobEntry::Step(ref step) = inner.then_entries[0] {
                    assert_eq!(step.name, Some("STEP2".to_string()));
                }
            } else {
                panic!("Expected nested If");
            }
            // ELSE has STEP3
            assert_eq!(outer.else_entries.len(), 1);
            if let JobEntry::Step(ref step) = outer.else_entries[0] {
                assert_eq!(step.name, Some("STEP3".to_string()));
            }
        } else {
            panic!("Expected If construct");
        }
    }

    /// Story 103.1: ABEND condition keyword.
    #[test]
    fn test_parse_condition_abend() {
        let expr = Parser::parse_condition_expr("STEP1.ABEND THEN").unwrap();
        if let ConditionExpr::Abend { step_name } = &expr {
            assert_eq!(step_name, "STEP1");
        } else {
            panic!("Expected Abend, got {:?}", expr);
        }
    }

    /// Story 103.1: RUN condition keyword.
    #[test]
    fn test_parse_condition_run() {
        let expr = Parser::parse_condition_expr("STEP1.RUN THEN").unwrap();
        if let ConditionExpr::Run { step_name } = &expr {
            assert_eq!(step_name, "STEP1");
        } else {
            panic!("Expected Run, got {:?}", expr);
        }
    }

    /// Story 103.1: NOT condition.
    #[test]
    fn test_parse_condition_not() {
        let expr =
            Parser::parse_condition_expr("NOT (STEP1.RC > 4) THEN").unwrap();
        if let ConditionExpr::Not(inner) = &expr {
            if let ConditionExpr::RcCompare { step_name, operator, value } = inner.as_ref() {
                assert_eq!(step_name, "STEP1");
                assert_eq!(*operator, ConditionOperator::Gt);
                assert_eq!(*value, 4);
            } else {
                panic!("Expected RcCompare inside NOT");
            }
        } else {
            panic!("Expected Not, got {:?}", expr);
        }
    }

    // ======================================================================
    // Epic 104: Extended DD Parameters
    // ======================================================================

    /// Story 104.1: SMS storage class (STORCLAS).
    #[test]
    fn test_parse_sms_classes() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=IEFBR14
//OUT      DD DSN=MY.DATA,DISP=(NEW,CATLG,DELETE),
//            STORCLAS=FAST,DATACLAS=BIGDATA,MGMTCLAS=ARCHIVE
//"#;

        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            assert_eq!(def.storclas, Some("FAST".to_string()));
            assert_eq!(def.dataclas, Some("BIGDATA".to_string()));
            assert_eq!(def.mgmtclas, Some("ARCHIVE".to_string()));
        } else {
            panic!("Expected Dataset definition");
        }
    }

    /// Story 104.2: LABEL parameter with sequence and type.
    #[test]
    fn test_parse_label() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=TAPEPROC
//TAPE     DD DSN=MY.TAPE.DATA,DISP=SHR,LABEL=(2,SL)
//"#;

        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            let label = def.label.as_ref().expect("LABEL should be parsed");
            assert_eq!(label.sequence, 2);
            assert_eq!(label.label_type, LabelType::Sl);
        } else {
            panic!("Expected Dataset definition");
        }
    }

    /// Story 104.2: EXPDT and RETPD parameters.
    #[test]
    fn test_parse_expdt_retpd() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=TEST
//OUT1     DD DSN=MY.EXP.DATA,DISP=(NEW,CATLG),EXPDT=2025365
//OUT2     DD DSN=MY.RET.DATA,DISP=(NEW,CATLG),RETPD=90
//"#;

        let job = parse(jcl).unwrap();
        let steps = job.steps();
        let dd1 = &steps[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd1.definition {
            assert_eq!(def.expdt, Some("2025365".to_string()));
        } else {
            panic!("Expected Dataset definition for OUT1");
        }
        let dd2 = &steps[0].dd_statements[1];
        if let DdDefinition::Dataset(ref def) = dd2.definition {
            assert_eq!(def.retpd, Some(90));
        } else {
            panic!("Expected Dataset definition for OUT2");
        }
    }

    /// Story 104.3: DSNTYPE, LIKE, REFDD parameters.
    #[test]
    fn test_parse_dsntype_like_refdd() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=TEST
//OUT1     DD DSN=MY.NEW.PDS,DISP=(NEW,CATLG),DSNTYPE=LIBRARY
//OUT2     DD DSN=MY.LIKE.DS,DISP=(NEW,CATLG),LIKE=MODEL.DS
//OUT3     DD DSN=MY.REF.DS,DISP=(NEW,CATLG),REFDD=OUT1
//"#;

        let job = parse(jcl).unwrap();
        let steps = job.steps();
        let dd1 = &steps[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd1.definition {
            assert_eq!(def.dsntype, Some(DsType::Library));
        } else {
            panic!("Expected Dataset for OUT1");
        }
        let dd2 = &steps[0].dd_statements[1];
        if let DdDefinition::Dataset(ref def) = dd2.definition {
            assert_eq!(def.like, Some("MODEL.DS".to_string()));
        } else {
            panic!("Expected Dataset for OUT2");
        }
        let dd3 = &steps[0].dd_statements[2];
        if let DdDefinition::Dataset(ref def) = dd3.definition {
            assert_eq!(def.refdd, Some("OUT1".to_string()));
        } else {
            panic!("Expected Dataset for OUT3");
        }
    }

    /// Story 104.3: KEYLEN and KEYOFF for VSAM KSDS.
    #[test]
    fn test_parse_keylen_keyoff() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=VSAMPROC
//KSDS     DD DSN=MY.KSDS.DATA,DISP=SHR,KEYLEN=20,KEYOFF=0
//"#;

        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            assert_eq!(def.keylen, Some(20));
            assert_eq!(def.keyoff, Some(0));
        } else {
            panic!("Expected Dataset definition");
        }
    }

    /// Story 104.4: USS file definition with PATH=.
    #[test]
    fn test_parse_uss_path() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=BPXBATCH
//STDOUT   DD PATH='/u/user/output.txt',PATHOPTS=(OWRONLY,OCREAT),
//            PATHMODE=(SIRUSR,SIWUSR),PATHDISP=(KEEP,DELETE)
//"#;

        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        if let DdDefinition::UssFile(ref def) = dd.definition {
            assert_eq!(def.path, "/u/user/output.txt");
            assert_eq!(def.pathopts, vec!["OWRONLY", "OCREAT"]);
            assert_eq!(def.pathmode, vec!["SIRUSR", "SIWUSR"]);
            let (norm, abn) = def.pathdisp.as_ref().expect("PATHDISP should be set");
            assert_eq!(norm, "KEEP");
            assert_eq!(abn.as_deref(), Some("DELETE"));
        } else {
            panic!("Expected UssFile definition, got {:?}", dd.definition);
        }
    }

    // ======================================================================
    // Epic 105: Extended JOB/EXEC Parameters
    // ======================================================================

    /// Story 105.1: JOB TIME parameter.
    #[test]
    fn test_parse_job_time() {
        let jcl = r#"//MYJOB    JOB CLASS=A,TIME=(5,30)
//STEP1    EXEC PGM=IEFBR14
//"#;

        let job = parse(jcl).unwrap();
        assert_eq!(job.params.time, Some((5, 30)));
    }

    /// Story 105.1: JOB MSGLEVEL parameter.
    #[test]
    fn test_parse_job_msglevel() {
        let jcl = r#"//MYJOB    JOB CLASS=A,MSGLEVEL=(1,1)
//STEP1    EXEC PGM=IEFBR14
//"#;

        let job = parse(jcl).unwrap();
        assert_eq!(job.params.msglevel, Some((1, 1)));
    }

    /// Story 105.1: JOB TYPRUN parameter.
    #[test]
    fn test_parse_job_typrun() {
        let jcl = r#"//MYJOB    JOB CLASS=A,TYPRUN=SCAN
//STEP1    EXEC PGM=IEFBR14
//"#;

        let job = parse(jcl).unwrap();
        assert_eq!(job.params.typrun, Some(TypeRun::Scan));
    }

    /// Story 105.1: JOB MEMLIMIT, JOBRC, SCHENV parameters.
    #[test]
    fn test_parse_job_extended_params() {
        let jcl = r#"//MYJOB    JOB CLASS=A,MEMLIMIT=2G,SCHENV=PRODENV
//STEP1    EXEC PGM=IEFBR14
//"#;

        let job = parse(jcl).unwrap();
        assert_eq!(job.params.memlimit, Some("2G".to_string()));
        assert_eq!(job.params.schenv, Some("PRODENV".to_string()));
    }

    /// Story 105.2: EXEC COND single condition.
    #[test]
    fn test_parse_exec_cond_single() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=PROG1
//STEP2    EXEC PGM=PROG2,COND=(4,LT)
//"#;

        let job = parse(jcl).unwrap();
        let steps = job.steps();
        let conds = steps[1].params.cond.as_ref().expect("COND should be parsed");
        assert_eq!(conds.len(), 1);
        assert_eq!(conds[0].code, 4);
        assert_eq!(conds[0].operator, ConditionOperator::Lt);
        assert!(conds[0].step.is_none());
    }

    /// Story 105.2: EXEC COND with step reference and multiple conditions.
    #[test]
    fn test_parse_exec_cond_multiple() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=PROG1
//STEP2    EXEC PGM=PROG2,COND=((4,LT,STEP1),(0,NE))
//"#;

        let job = parse(jcl).unwrap();
        let steps = job.steps();
        let conds = steps[1].params.cond.as_ref().expect("COND should be parsed");
        assert_eq!(conds.len(), 2);
        assert_eq!(conds[0].code, 4);
        assert_eq!(conds[0].operator, ConditionOperator::Lt);
        assert_eq!(conds[0].step, Some("STEP1".to_string()));
        assert_eq!(conds[1].code, 0);
        assert_eq!(conds[1].operator, ConditionOperator::Ne);
    }

    /// Story 105.2: EXEC COND=EVEN and COND=ONLY.
    #[test]
    fn test_parse_exec_cond_even_only() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=PROG1
//STEP2    EXEC PGM=PROG2,COND=EVEN
//"#;

        let job = parse(jcl).unwrap();
        let steps = job.steps();
        assert_eq!(steps[1].params.cond_special, Some(CondSpecial::Even));
    }

    /// Story 105.2: EXEC COND with EVEN alongside conditions.
    #[test]
    fn test_parse_exec_cond_with_even() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=PROG1
//STEP2    EXEC PGM=PROG2,COND=((4,LT),EVEN)
//"#;

        let job = parse(jcl).unwrap();
        let steps = job.steps();
        let conds = steps[1].params.cond.as_ref().expect("COND should be parsed");
        assert_eq!(conds.len(), 1);
        assert_eq!(conds[0].code, 4);
        assert_eq!(steps[1].params.cond_special, Some(CondSpecial::Even));
    }

    /// Story 105.2: EXEC COND with procstep reference.
    #[test]
    fn test_parse_exec_cond_procstep() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=PROG1
//STEP2    EXEC PGM=PROG2,COND=((4,LT,STEP1.PROCSTEP1))
//"#;

        let job = parse(jcl).unwrap();
        let steps = job.steps();
        let conds = steps[1].params.cond.as_ref().expect("COND should be parsed");
        assert_eq!(conds[0].step, Some("STEP1".to_string()));
        assert_eq!(conds[0].procstep, Some("PROCSTEP1".to_string()));
    }

    /// Story 105.1: EXEC TIME parameter.
    #[test]
    fn test_parse_exec_time() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=PROG1,TIME=(2,30)
//"#;

        let job = parse(jcl).unwrap();
        let steps = job.steps();
        assert_eq!(steps[0].params.time, Some((2, 30)));
    }

    /// Story 103.2: Missing ENDIF produces error.
    #[test]
    fn test_parse_missing_endif_error() {
        let jcl = r#"//MYJOB    JOB CLASS=A
// IF (STEP1.RC = 0) THEN
//STEP1    EXEC PGM=PROG1
//"#;

        let result = parse(jcl);
        assert!(result.is_err());
        let err = format!("{}", result.unwrap_err());
        assert!(
            err.contains("ENDIF"),
            "Error should mention ENDIF: {}",
            err
        );
    }

    // ======================================================================
    // Epic 106: INCLUDE Statement and DD Concatenation
    // ======================================================================

    /// Story 106.2: DD concatenation — unnamed DDs form a concatenation.
    #[test]
    fn test_parse_dd_concatenation() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=MYPROC
//INPUT    DD DSN=FIRST.DATA,DISP=SHR
//         DD DSN=SECOND.DATA,DISP=SHR
//         DD DSN=THIRD.DATA,DISP=SHR
//"#;

        let job = parse(jcl).unwrap();
        let steps = job.steps();
        assert_eq!(steps.len(), 1);
        // Should have 1 DD (concatenated), not 3
        assert_eq!(steps[0].dd_statements.len(), 1);

        let dd = &steps[0].dd_statements[0];
        assert_eq!(dd.name, "INPUT");
        if let DdDefinition::Concatenation(ref datasets) = dd.definition {
            assert_eq!(datasets.len(), 3);
            assert_eq!(datasets[0].dsn, "FIRST.DATA");
            assert_eq!(datasets[1].dsn, "SECOND.DATA");
            assert_eq!(datasets[2].dsn, "THIRD.DATA");
        } else {
            panic!("Expected Concatenation definition, got {:?}", dd.definition);
        }
    }

    /// Story 106.2: Mixed DDs — named DD after unnamed (not concatenated).
    #[test]
    fn test_parse_dd_concat_then_named() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=MYPROC
//INPUT    DD DSN=FIRST.DATA,DISP=SHR
//         DD DSN=SECOND.DATA,DISP=SHR
//OUTPUT   DD SYSOUT=*
//"#;

        let job = parse(jcl).unwrap();
        let steps = job.steps();
        // 2 DDs: INPUT (concatenated) and OUTPUT
        assert_eq!(steps[0].dd_statements.len(), 2);

        // INPUT is concatenated
        let dd = &steps[0].dd_statements[0];
        assert_eq!(dd.name, "INPUT");
        if let DdDefinition::Concatenation(ref datasets) = dd.definition {
            assert_eq!(datasets.len(), 2);
        } else {
            panic!("Expected Concatenation");
        }

        // OUTPUT is a separate DD
        let dd2 = &steps[0].dd_statements[1];
        assert_eq!(dd2.name, "OUTPUT");
    }

    // ======================================================================
    // Epic 107: GDG Support and OUTPUT Statement
    // ======================================================================

    /// Story 107.1: GDG relative reference parsing (+1).
    #[test]
    fn test_parse_gdg_positive() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=TEST
//OUT      DD DSN=MY.GDG(+1),DISP=(NEW,CATLG)
//"#;

        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            assert_eq!(def.dsn, "MY.GDG");
            assert_eq!(def.gdg_generation, Some(1));
        } else {
            panic!("Expected Dataset definition");
        }
    }

    /// Story 107.1: GDG relative reference parsing (0 = current).
    #[test]
    fn test_parse_gdg_current() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=TEST
//IN       DD DSN=MY.GDG(0),DISP=SHR
//"#;

        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            assert_eq!(def.dsn, "MY.GDG");
            assert_eq!(def.gdg_generation, Some(0));
        } else {
            panic!("Expected Dataset definition");
        }
    }

    /// Story 107.1: Regular member names are not confused with GDG.
    #[test]
    fn test_parse_pds_member_not_gdg() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=TEST
//IN       DD DSN=MY.PDS(MEMBER),DISP=SHR
//"#;

        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            assert_eq!(def.dsn, "MY.PDS(MEMBER)");
            assert_eq!(def.gdg_generation, None);
        } else {
            panic!("Expected Dataset definition");
        }
    }

    /// Story 107.3: OUTPUT statement parsing.
    #[test]
    fn test_parse_output_statement() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//OUT1     OUTPUT CLASS=A,DEST=REMOTE1,COPIES=3,FORMS=STD
//STEP1    EXEC PGM=TEST
//"#;

        let job = parse(jcl).unwrap();
        assert_eq!(job.output_stmts.len(), 1);

        let out = &job.output_stmts[0];
        assert_eq!(out.name, "OUT1");
        assert_eq!(out.class, Some('A'));
        assert_eq!(out.dest, Some("REMOTE1".to_string()));
        assert_eq!(out.copies, Some(3));
        assert_eq!(out.forms, Some("STD".to_string()));
    }

    /// Story 107.3: Multiple OUTPUT statements.
    #[test]
    fn test_parse_multiple_output_stmts() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//OUT1     OUTPUT CLASS=A,DEST=LOCAL
//OUT2     OUTPUT CLASS=B,COPIES=5
//STEP1    EXEC PGM=TEST
//"#;

        let job = parse(jcl).unwrap();
        assert_eq!(job.output_stmts.len(), 2);
        assert_eq!(job.output_stmts[0].name, "OUT1");
        assert_eq!(job.output_stmts[1].name, "OUT2");
        assert_eq!(job.output_stmts[1].copies, Some(5));
    }

    // ======================================================================
    // Epic 109: Parser Test Suite Expansion (Story 109.2)
    // ======================================================================

    /// Story 109.2: Multi-step jobs parse correctly.
    #[test]
    fn test_parse_multi_step_job() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=PROG1
//SYSOUT   DD SYSOUT=*
//STEP2    EXEC PGM=PROG2
//INPUT    DD DSN=MY.DATA,DISP=SHR
//STEP3    EXEC PGM=PROG3,PARM='ABC'
//"#;

        let job = parse(jcl).unwrap();
        let steps = job.steps();
        assert_eq!(steps.len(), 3);
        assert_eq!(steps[0].name, Some("STEP1".to_string()));
        assert_eq!(steps[1].name, Some("STEP2".to_string()));
        assert_eq!(steps[2].name, Some("STEP3".to_string()));
        assert_eq!(steps[2].params.parm, Some("ABC".to_string()));
    }

    /// Story 109.2: Continuation lines work for long DD parameters.
    #[test]
    fn test_parse_continuation_lines() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=TEST
//OUT      DD DSN=LONG.DATASET.NAME,DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(TRK,(100,50)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=27920)
//"#;

        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            assert_eq!(def.dsn, "LONG.DATASET.NAME");
            let disp = def.disp.as_ref().unwrap();
            assert!(matches!(disp.status, DispStatus::New));
            assert!(matches!(disp.normal, Some(DispAction::Catlg)));
            let dcb = def.dcb.as_ref().unwrap();
            assert_eq!(dcb.recfm, Some(RecordFormat::FixedBlocked));
            assert_eq!(dcb.lrecl, Some(80));
            assert_eq!(dcb.blksize, Some(27920));
        } else {
            panic!("Expected Dataset definition");
        }
    }

    /// Story 109.2: All DISP status combinations parse.
    #[test]
    fn test_parse_all_disp_combinations() {
        // Test each DISP status: NEW, OLD, SHR, MOD
        for (disp_str, expected) in [
            ("(NEW,CATLG)", DispStatus::New),
            ("(OLD,KEEP)", DispStatus::Old),
            ("SHR", DispStatus::Shr),
            ("(MOD,CATLG)", DispStatus::Mod),
        ] {
            let jcl = format!(
                "//MYJOB    JOB CLASS=A\n//STEP1    EXEC PGM=TEST\n//DD1      DD DSN=MY.DS,DISP={}\n//",
                disp_str
            );
            let job = parse(&jcl).unwrap();
            let dd = &job.steps()[0].dd_statements[0];
            if let DdDefinition::Dataset(ref def) = dd.definition {
                assert_eq!(def.disp.as_ref().unwrap().status, expected,
                    "DISP={} should produce {:?}", disp_str, expected);
            } else {
                panic!("Expected Dataset for DISP={}", disp_str);
            }
        }
    }

    /// Story 109.2: DCB variants (RECFM=F, V, VB, U).
    #[test]
    fn test_parse_dcb_recfm_variants() {
        for (recfm_str, expected) in [
            ("F", RecordFormat::Fixed),
            ("FB", RecordFormat::FixedBlocked),
            ("V", RecordFormat::Variable),
            ("VB", RecordFormat::VariableBlocked),
            ("U", RecordFormat::Undefined),
        ] {
            let jcl = format!(
                "//MYJOB    JOB CLASS=A\n//STEP1    EXEC PGM=T\n//DD1      DD DSN=MY.DS,DISP=SHR,DCB=(RECFM={},LRECL=80)\n//",
                recfm_str
            );
            let job = parse(&jcl).unwrap();
            let dd = &job.steps()[0].dd_statements[0];
            if let DdDefinition::Dataset(ref def) = dd.definition {
                let dcb = def.dcb.as_ref().expect("DCB should be parsed");
                assert_eq!(dcb.recfm, Some(expected), "RECFM={}", recfm_str);
            } else {
                panic!("Expected Dataset for RECFM={}", recfm_str);
            }
        }
    }

    /// Story 109.2: SPACE with various units (TRK, CYL, block size).
    #[test]
    fn test_parse_space_units() {
        // TRK
        let jcl = "//MYJOB    JOB CLASS=A\n//STEP1    EXEC PGM=T\n//DD1      DD DSN=MY.DS,DISP=(NEW,CATLG),SPACE=(TRK,(10,5))\n//";
        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            let space = def.space.as_ref().expect("SPACE should be parsed");
            assert_eq!(space.unit, SpaceUnit::Trk);
            assert_eq!(space.primary, 10);
            assert_eq!(space.secondary, Some(5));
        } else {
            panic!("Expected Dataset");
        }

        // CYL
        let jcl = "//MYJOB    JOB CLASS=A\n//STEP1    EXEC PGM=T\n//DD1      DD DSN=MY.DS,DISP=(NEW,CATLG),SPACE=(CYL,(5,2))\n//";
        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            let space = def.space.as_ref().unwrap();
            assert_eq!(space.unit, SpaceUnit::Cyl);
            assert_eq!(space.primary, 5);
        } else {
            panic!("Expected Dataset");
        }
    }

    /// Story 109.2: Inline data DD parsed as Inline definition with data lines.
    #[test]
    fn test_parse_inline_data_default_delim() {
        let jcl = "//MYJOB    JOB CLASS=A\n//STEP1    EXEC PGM=T\n//SYSIN    DD *\n  SORT FIELDS=(1,10,CH,A)\n/*\n//";
        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        assert_eq!(dd.name, "SYSIN");
        if let DdDefinition::Inline(ref def) = dd.definition {
            assert_eq!(def.data.len(), 1);
            assert!(def.data[0].contains("SORT FIELDS"));
        } else {
            panic!("Expected Inline definition, got {:?}", dd.definition);
        }
    }

    /// Story 109.2: Multiple inline data DDs — SYMNAMES terminated by next // statement.
    #[test]
    fn test_parse_multiple_inline_dds() {
        let jcl = "//MYJOB    JOB CLASS=A\n//STEP1    EXEC PGM=SORT\n//SYMNAMES DD *\nTRAN-ID,1,16,CH\n//SYSIN    DD *\n SORT FIELDS=(TRAN-ID,A)\n/*\n//SYSOUT   DD SYSOUT=*\n//";
        let job = parse(jcl).unwrap();
        let dds = &job.steps()[0].dd_statements;

        // SYMNAMES should have 1 line of inline data
        assert_eq!(dds[0].name, "SYMNAMES");
        if let DdDefinition::Inline(ref def) = dds[0].definition {
            assert_eq!(def.data.len(), 1);
            assert!(def.data[0].contains("TRAN-ID"), "Expected TRAN-ID in SYMNAMES data, got: {:?}", def.data);
        } else {
            panic!("Expected Inline definition for SYMNAMES");
        }

        // SYSIN should have 1 line of inline data
        assert_eq!(dds[1].name, "SYSIN");
        if let DdDefinition::Inline(ref def) = dds[1].definition {
            assert_eq!(def.data.len(), 1);
            assert!(def.data[0].contains("SORT FIELDS"), "Expected SORT FIELDS in SYSIN data, got: {:?}", def.data);
        } else {
            panic!("Expected Inline definition for SYSIN");
        }

        // SYSOUT should be parsed as Sysout
        assert_eq!(dds[2].name, "SYSOUT");
        assert!(matches!(dds[2].definition, DdDefinition::Sysout(_)));
    }

    /// Story 109.2: SYSOUT with writer and form.
    #[test]
    fn test_parse_sysout_star() {
        let jcl = "//MYJOB    JOB CLASS=A\n//STEP1    EXEC PGM=T\n//PRINT    DD SYSOUT=A\n//";
        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        if let DdDefinition::Sysout(ref def) = dd.definition {
            assert_eq!(def.class, 'A');
        } else {
            panic!("Expected Sysout");
        }
    }

    /// Story 109.2: DUMMY DD parsing.
    #[test]
    fn test_parse_dummy_dd() {
        let jcl = "//MYJOB    JOB CLASS=A\n//STEP1    EXEC PGM=T\n//NULL     DD DUMMY\n//";
        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        assert_eq!(dd.name, "NULL");
        assert!(matches!(dd.definition, DdDefinition::Dummy));
    }

    /// Story 109.2: Comment handling — comments are ignored.
    #[test]
    fn test_parse_with_comments() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//*  THIS IS A COMMENT
//STEP1    EXEC PGM=IEFBR14
//*  ANOTHER COMMENT
//"#;

        let job = parse(jcl).unwrap();
        let steps = job.steps();
        assert_eq!(steps.len(), 1);
        assert_eq!(steps[0].name, Some("STEP1".to_string()));
    }

    /// Story 109.2: Null statement (//) terminates job.
    #[test]
    fn test_parse_null_statement() {
        let jcl = "//MYJOB    JOB CLASS=A\n//STEP1    EXEC PGM=TEST\n//";
        let job = parse(jcl).unwrap();
        assert_eq!(job.steps().len(), 1);
    }

    /// Story 109.2: Error case — empty input.
    #[test]
    fn test_parse_empty_input_error() {
        let result = parse("");
        assert!(result.is_err());
    }

    /// Story 109.2: Error case — missing JOB statement.
    #[test]
    fn test_parse_missing_job_error() {
        let jcl = "//STEP1    EXEC PGM=IEFBR14\n//";
        let result = parse(jcl);
        assert!(result.is_err());
        let err = format!("{}", result.unwrap_err());
        assert!(err.contains("JOB"), "Error should mention JOB: {}", err);
    }

    /// Story 109.2: SPACE with directory blocks (for PDS).
    #[test]
    fn test_parse_space_with_directory() {
        let jcl = "//MYJOB    JOB CLASS=A\n//STEP1    EXEC PGM=T\n//DD1      DD DSN=MY.PDS,DISP=(NEW,CATLG),SPACE=(TRK,(10,5,2))\n//";
        let job = parse(jcl).unwrap();
        let dd = &job.steps()[0].dd_statements[0];
        if let DdDefinition::Dataset(ref def) = dd.definition {
            let space = def.space.as_ref().unwrap();
            assert_eq!(space.primary, 10);
            assert_eq!(space.secondary, Some(5));
            assert_eq!(space.directory, Some(2));
        } else {
            panic!("Expected Dataset");
        }
    }

    /// JCLLIB ORDER with quoted dataset names should strip single quotes.
    #[test]
    fn test_jcllib_strips_single_quotes() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//         JCLLIB ORDER=('AWS.M2.CARDDEMO.PROC','SYS1.PROCLIB')
//STEP1    EXEC PGM=IEFBR14
//"#;

        let job = parse(jcl).unwrap();
        assert_eq!(
            job.jcllib_order,
            vec![
                "AWS.M2.CARDDEMO.PROC".to_string(),
                "SYS1.PROCLIB".to_string(),
            ]
        );
    }

    /// JCLLIB ORDER with a single unquoted library name.
    #[test]
    fn test_jcllib_single_unquoted() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//         JCLLIB ORDER=MY.PROC.LIB
//STEP1    EXEC PGM=IEFBR14
//"#;

        let job = parse(jcl).unwrap();
        assert_eq!(job.jcllib_order, vec!["MY.PROC.LIB".to_string()]);
    }

    /// JCLLIB ORDER with a single quoted library name.
    #[test]
    fn test_jcllib_single_quoted() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//         JCLLIB ORDER='MY.PROC.LIB'
//STEP1    EXEC PGM=IEFBR14
//"#;

        let job = parse(jcl).unwrap();
        assert_eq!(job.jcllib_order, vec!["MY.PROC.LIB".to_string()]);
    }
}
