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

        // Parse steps (EXEC + DD statements)
        while self.current < self.statements.len() {
            let stmt = &self.statements[self.current];
            match stmt.operation.as_str() {
                "EXEC" => {
                    let step = self.parse_step()?;
                    // Extend job span to cover this step
                    job.span = job.span.extend(step.span);
                    job.add_step(step);
                }
                "NULL" => {
                    // Null statement marks end of JCL — extend span to cover it
                    job.span = job.span.extend(Span::main(stmt.byte_offset, stmt.byte_end));
                    break;
                }
                "SET" => {
                    // Already processed in first pass, skip
                    job.span = job.span.extend(Span::main(stmt.byte_offset, stmt.byte_end));
                    self.current += 1;
                }
                "PROC" => {
                    // Skip PROC/PEND blocks (already collected)
                    job.span = job.span.extend(Span::main(stmt.byte_offset, stmt.byte_end));
                    self.skip_proc_block()?;
                }
                "IF" | "ENDIF" | "INCLUDE" | "JCLLIB" => {
                    // Skip these for now but extend span
                    job.span = job.span.extend(Span::main(stmt.byte_offset, stmt.byte_end));
                    self.current += 1;
                }
                _ => {
                    // Unexpected statement
                    return Err(JclError::ParseError {
                        message: format!(
                            "Unexpected {} statement at line {}",
                            stmt.operation, stmt.line
                        ),
                    });
                }
            }
        }

        // Transfer collected symbols and procs to the job
        job.symbols = self.symbols.clone();
        job.in_stream_procs = self.in_stream_procs.clone();

        Ok(job)
    }

    /// First pass: collect SET statement values and PROC/PEND blocks.
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
                                // Parse condition (simplified)
                                if let Token::LParen = &tokens[idx] {
                                    // Skip condition parsing for now
                                    while idx < tokens.len()
                                        && !matches!(tokens[idx], Token::RParen)
                                    {
                                        idx += 1;
                                    }
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

        // Parse DD statements for this step
        while self.current < self.statements.len() {
            let stmt = &self.statements[self.current];
            if stmt.operation != "DD" {
                break;
            }
            let dd = self.parse_dd_statement()?;
            // Extend step span to cover this DD
            step.span = step.span.extend(dd.span);
            step.add_dd(dd);
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
            let mut dd = DdStatement::inline(dd_name, Vec::new());
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

        // Parse as dataset DD
        let def = self.parse_dataset_def(&tokens)?;
        Ok(DdStatement {
            name: dd_name,
            definition: DdDefinition::Dataset(def),
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
                                def.dsn = self.collect_dsn(tokens, &mut idx);
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
    fn collect_dsn(&self, tokens: &[Token], idx: &mut usize) -> String {
        let mut dsn = String::new();

        while *idx < tokens.len() {
            match &tokens[*idx] {
                Token::Ident(s) => {
                    dsn.push_str(s);
                }
                Token::Period => {
                    dsn.push('.');
                }
                Token::LParen => {
                    // Member name in parens
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
                    return dsn;
                }
                Token::String(s) => {
                    dsn = s.clone();
                    *idx += 1;
                    return dsn;
                }
                Token::Ampersand => {
                    // Symbolic parameter
                    dsn.push('&');
                }
                _ => break,
            }
            *idx += 1;
        }

        dsn
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
        assert_eq!(job.steps.len(), 1);

        let step = &job.steps[0];
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
        assert_eq!(job.steps.len(), 1);

        let step = &job.steps[0];
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
        let step = &job.steps[0];
        assert_eq!(step.params.parm, Some("OPTION1,OPTION2".to_string()));
    }

    #[test]
    fn test_parse_dataset_with_member() {
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=TEST
//INPUT    DD DSN=MY.PDS(MEMBER),DISP=SHR
//"#;

        let job = parse(jcl).unwrap();
        let dd = &job.steps[0].dd_statements[0];
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
        let dd = &job.steps[0].dd_statements[0];
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
        let step = &job.steps[0];
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
        let jcl = r#"//MYJOB    JOB CLASS=A
//STEP1    EXEC PGM=VSAMPROC
//VSAMDD   DD DSN=MY.KSDS.CLUSTER,DISP=SHR,AMP=(BUFND=10,BUFNI=5,MODE=DIR)
//"#;

        let job = parse(jcl).unwrap();
        let step = &job.steps[0];
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
        assert_eq!(job.steps.len(), 1);
        if let ExecType::Program(ref pgm) = job.steps[0].exec {
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
        let dd = &job.steps[0].dd_statements[0];
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
        let dd = &job.steps[0].dd_statements[0];
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
        let dd = &job.steps[0].dd_statements[0];
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
        let dd = &job.steps[0].dd_statements[0];
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
        assert_eq!(job.steps.len(), 1);

        // Expand the procedure
        let mut expander = crate::procedure::ProcedureExpander::new(
            job.in_stream_procs.clone(),
            job.symbols.clone(),
        );
        let expanded = expander.expand(&job).unwrap();

        // Should have one expanded step with PGM=MYPROG
        assert_eq!(expanded.steps.len(), 1);
        if let ExecType::Program(ref pgm) = expanded.steps[0].exec {
            assert_eq!(pgm, "MYPROG");
        } else {
            panic!("Expected Program after expansion");
        }

        // DD should have DSN=PROD.DATA (HLQ overridden to PROD)
        let dd = &expanded.steps[0].dd_statements[0];
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
        let dd = &expanded.steps[0].dd_statements[0];
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
        let dd = &expanded.steps[0].dd_statements[0];
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

        let dd = &expanded.steps[0].dd_statements[0];
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

        assert_eq!(expanded.steps.len(), 1);
        let dd = &expanded.steps[0].dd_statements[0];
        assert_eq!(dd.name, "SYSOUT");
        if let DdDefinition::Sysout(ref def) = dd.definition {
            assert_eq!(def.class, '*');
        } else {
            panic!("Expected Sysout definition, got {:?}", dd.definition);
        }
    }
}
