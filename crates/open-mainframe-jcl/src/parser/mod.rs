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
}

impl<'a> Parser<'a> {
    /// Create a new parser for JCL source.
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            statements: Vec::new(),
            current: 0,
            inline_data: HashMap::new(),
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
                "SET" | "IF" | "ENDIF" | "INCLUDE" | "JCLLIB" => {
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

        Ok(job)
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

    /// Parse an EXEC statement and its associated DDs.
    fn parse_step(&mut self) -> Result<Step, JclError> {
        let stmt = &self.statements[self.current];
        let step_name = stmt.name.clone();

        // Parse operands
        let tokens = tokenize_operands(&stmt.operands)?;
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
            message: format!("EXEC statement missing PGM or PROC at line {}", stmt.line),
        })?;

        let exec_span = Span::main(stmt.byte_offset, stmt.byte_end);
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
    fn parse_dd_statement(&self) -> Result<DdStatement, JclError> {
        let stmt = &self.statements[self.current];
        let dd_name = stmt.name.clone().unwrap_or_else(|| "UNNAMED".to_string());
        let dd_span = Span::main(stmt.byte_offset, stmt.byte_end);

        let tokens = tokenize_operands(&stmt.operands)?;

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
}
