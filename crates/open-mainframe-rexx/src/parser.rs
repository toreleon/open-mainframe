//! REXX parser — builds an AST from a token stream.
//!
//! REXX has no reserved words, so the parser must use context to decide
//! whether a symbol is a keyword, variable, or label.  The core heuristic:
//! if the first token of a clause is a symbol followed by `=`, it's an
//! assignment; if it matches a known instruction keyword and context fits,
//! it's an instruction; otherwise it's a host command.

use crate::ast::*;
use crate::token::{Token, TokenKind};

/// Parse error.
#[derive(Debug, Clone, thiserror::Error)]
pub enum ParseError {
    #[error("unexpected token {found:?} at line {line} (expected {expected})")]
    Unexpected {
        found: TokenKind,
        expected: String,
        line: u32,
    },
    #[error("unexpected end of input (expected {expected})")]
    UnexpectedEof { expected: String },
    #[error("syntax error at line {line}: {message}")]
    Syntax { line: u32, message: String },
}

/// Parse a token stream into a REXX program AST.
pub fn parse(tokens: &[Token]) -> Result<Program, ParseError> {
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}

struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, pos: 0 }
    }

    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut clauses = Vec::new();

        while !self.at_eof() {
            self.skip_separators();
            if self.at_eof() {
                break;
            }
            let clause = self.parse_clause()?;
            clauses.push(clause);
        }

        Ok(Program { clauses })
    }

    // -----------------------------------------------------------------------
    // Clause parsing
    // -----------------------------------------------------------------------

    fn parse_clause(&mut self) -> Result<Clause, ParseError> {
        let line = self.current_line();

        // Check for label: symbol followed by ':'
        if self.is_label() {
            let name = self.expect_symbol()?;
            self.expect(TokenKind::Colon)?;
            return Ok(Clause { line, body: ClauseBody::Label(name) });
        }

        // Check for assignment: symbol followed by '='
        if self.is_assignment() {
            let var = self.expect_symbol()?;
            self.expect(TokenKind::Eq)?;
            let expr = self.parse_expr()?;
            return Ok(Clause { line, body: ClauseBody::Assignment { var, expr } });
        }

        // Try instruction keywords
        if let Some(sym) = self.peek_symbol() {
            let upper = sym.to_ascii_uppercase();
            match upper.as_str() {
                "SAY" => return self.parse_say(line),
                "IF" => return self.parse_if(line),
                "DO" => return self.parse_do(line),
                "SELECT" => return self.parse_select(line),
                "CALL" => return self.parse_call_instr(line),
                "RETURN" => return self.parse_return(line),
                "EXIT" => return self.parse_exit(line),
                "PARSE" => return self.parse_parse(line),
                "ARG" => return self.parse_arg(line),
                "PULL" => return self.parse_pull(line),
                "PUSH" => return self.parse_push(line),
                "QUEUE" => return self.parse_queue(line),
                "DROP" => return self.parse_drop(line),
                "SIGNAL" => return self.parse_signal(line),
                "ITERATE" => return self.parse_iterate(line),
                "LEAVE" => return self.parse_leave(line),
                "NOP" => { self.advance(); return Ok(Clause { line, body: ClauseBody::Nop }); }
                "TRACE" => return self.parse_trace(line),
                "ADDRESS" => return self.parse_address(line),
                "PROCEDURE" => return self.parse_procedure(line),
                "NUMERIC" => return self.parse_numeric(line),
                _ => {}
            }
        }

        // Anything else is a host command
        let expr = self.parse_expr()?;
        Ok(Clause { line, body: ClauseBody::Command(expr) })
    }

    // -----------------------------------------------------------------------
    // Instruction parsers
    // -----------------------------------------------------------------------

    fn parse_say(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip SAY
        let expr = if self.at_clause_end() {
            Expr::StringLit(String::new())
        } else {
            self.parse_expr()?
        };
        Ok(Clause { line, body: ClauseBody::Say(expr) })
    }

    fn parse_if(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip IF
        let cond = self.parse_expr()?;
        self.expect_keyword("THEN")?;
        self.skip_separators();
        let then_clause = Box::new(self.parse_clause()?);
        self.skip_separators();

        let else_clause = if self.peek_keyword("ELSE") {
            self.advance(); // skip ELSE
            self.skip_separators();
            Some(Box::new(self.parse_clause()?))
        } else {
            None
        };

        Ok(Clause {
            line,
            body: ClauseBody::If { cond, then_clause, else_clause },
        })
    }

    fn parse_do(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip DO

        let control = self.parse_do_control()?;
        self.skip_separators();

        let mut body = Vec::new();
        while !self.peek_keyword("END") && !self.at_eof() {
            self.skip_separators();
            if self.peek_keyword("END") || self.at_eof() {
                break;
            }
            body.push(self.parse_clause()?);
        }

        if self.peek_keyword("END") {
            self.advance(); // skip END
            // Optionally skip the control variable name after END
            if let Some(_sym) = self.peek_symbol() {
                if !self.at_clause_end() && !self.peek_keyword("ELSE") {
                    // Could be END varname — skip it
                    let next = self.peek_kind();
                    if matches!(next, Some(TokenKind::Symbol(_))) {
                        self.advance();
                    }
                }
            }
        }

        Ok(Clause { line, body: ClauseBody::Do { control, body } })
    }

    fn parse_do_control(&mut self) -> Result<DoControl, ParseError> {
        // DO FOREVER
        if self.peek_keyword("FOREVER") {
            self.advance();
            return Ok(DoControl::Forever);
        }

        // DO WHILE cond
        if self.peek_keyword("WHILE") {
            self.advance();
            let cond = self.parse_expr()?;
            return Ok(DoControl::While(cond));
        }

        // DO UNTIL cond
        if self.peek_keyword("UNTIL") {
            self.advance();
            let cond = self.parse_expr()?;
            return Ok(DoControl::Until(cond));
        }

        // DO var = from TO to [BY step]
        if self.is_do_iterative() {
            let var = self.expect_symbol()?;
            self.expect(TokenKind::Eq)?;
            let from = self.parse_expr()?;
            self.expect_keyword("TO")?;
            let to = self.parse_expr()?;
            let by = if self.peek_keyword("BY") {
                self.advance();
                Some(self.parse_expr()?)
            } else {
                None
            };
            return Ok(DoControl::Iterative { var, from, to, by });
        }

        // DO expr (count) — or simple DO
        if self.at_clause_end() {
            return Ok(DoControl::Simple);
        }

        let expr = self.parse_expr()?;
        Ok(DoControl::Count(expr))
    }

    fn parse_select(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip SELECT
        self.skip_separators();

        let mut whens = Vec::new();
        let mut otherwise = None;

        loop {
            self.skip_separators();
            if self.peek_keyword("END") {
                self.advance();
                break;
            }
            if self.peek_keyword("WHEN") {
                self.advance();
                let cond = self.parse_expr()?;
                self.expect_keyword("THEN")?;
                self.skip_separators();
                let mut stmts = Vec::new();
                // Parse statements until next WHEN, OTHERWISE, or END
                while !self.peek_keyword("WHEN")
                    && !self.peek_keyword("OTHERWISE")
                    && !self.peek_keyword("END")
                    && !self.at_eof()
                {
                    self.skip_separators();
                    if self.peek_keyword("WHEN")
                        || self.peek_keyword("OTHERWISE")
                        || self.peek_keyword("END")
                    {
                        break;
                    }
                    stmts.push(self.parse_clause()?);
                }
                whens.push((cond, stmts));
            } else if self.peek_keyword("OTHERWISE") {
                self.advance();
                self.skip_separators();
                let mut stmts = Vec::new();
                while !self.peek_keyword("END") && !self.at_eof() {
                    self.skip_separators();
                    if self.peek_keyword("END") {
                        break;
                    }
                    stmts.push(self.parse_clause()?);
                }
                otherwise = Some(stmts);
            } else if self.at_eof() {
                break;
            } else {
                return Err(ParseError::Syntax {
                    line: self.current_line(),
                    message: "expected WHEN, OTHERWISE, or END in SELECT".to_string(),
                });
            }
        }

        Ok(Clause {
            line,
            body: ClauseBody::Select { whens, otherwise },
        })
    }

    fn parse_call_instr(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip CALL
        let name = self.expect_symbol()?;
        let mut args = Vec::new();
        while !self.at_clause_end() {
            if !args.is_empty() && self.peek_kind() == Some(&TokenKind::Comma) {
                self.advance();
            }
            if self.at_clause_end() {
                break;
            }
            args.push(self.parse_expr()?);
        }
        Ok(Clause { line, body: ClauseBody::Call { name, args } })
    }

    fn parse_return(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip RETURN
        let expr = if self.at_clause_end() {
            None
        } else {
            Some(self.parse_expr()?)
        };
        Ok(Clause { line, body: ClauseBody::Return(expr) })
    }

    fn parse_exit(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip EXIT
        let expr = if self.at_clause_end() {
            None
        } else {
            Some(self.parse_expr()?)
        };
        Ok(Clause { line, body: ClauseBody::Exit(expr) })
    }

    fn parse_parse(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip PARSE
        let upper = if self.peek_keyword("UPPER") {
            self.advance();
            true
        } else {
            false
        };

        let source = self.parse_parse_source()?;

        // Rest of clause is the template string
        let template = self.collect_remaining_text();

        Ok(Clause {
            line,
            body: ClauseBody::Parse { upper, source, template },
        })
    }

    fn parse_parse_source(&mut self) -> Result<ParseSource, ParseError> {
        if let Some(sym) = self.peek_symbol() {
            match sym.as_str() {
                "ARG" => { self.advance(); Ok(ParseSource::Arg) }
                "PULL" => { self.advance(); Ok(ParseSource::Pull) }
                "EXTERNAL" => { self.advance(); Ok(ParseSource::External) }
                "SOURCE" => { self.advance(); Ok(ParseSource::Source) }
                "VERSION" => { self.advance(); Ok(ParseSource::Version) }
                "LINEIN" => { self.advance(); Ok(ParseSource::Linein) }
                "VAR" => {
                    self.advance();
                    let name = self.expect_symbol()?;
                    Ok(ParseSource::Var(name))
                }
                "VALUE" => {
                    self.advance();
                    let expr = self.parse_expr()?;
                    self.expect_keyword("WITH")?;
                    Ok(ParseSource::Value(expr))
                }
                _ => Err(ParseError::Syntax {
                    line: self.current_line(),
                    message: format!("unexpected PARSE source '{sym}'"),
                }),
            }
        } else {
            Err(ParseError::Syntax {
                line: self.current_line(),
                message: "expected PARSE source keyword".to_string(),
            })
        }
    }

    fn parse_arg(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip ARG
        let template = self.collect_remaining_text();
        Ok(Clause { line, body: ClauseBody::Arg(template) })
    }

    fn parse_pull(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip PULL
        let template = self.collect_remaining_text();
        Ok(Clause { line, body: ClauseBody::Pull(template) })
    }

    fn parse_push(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip PUSH
        let expr = if self.at_clause_end() {
            Expr::StringLit(String::new())
        } else {
            self.parse_expr()?
        };
        Ok(Clause { line, body: ClauseBody::Push(expr) })
    }

    fn parse_queue(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip QUEUE
        let expr = if self.at_clause_end() {
            Expr::StringLit(String::new())
        } else {
            self.parse_expr()?
        };
        Ok(Clause { line, body: ClauseBody::Queue(expr) })
    }

    fn parse_drop(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip DROP
        let mut vars = Vec::new();
        while !self.at_clause_end() {
            vars.push(self.expect_symbol()?);
        }
        Ok(Clause { line, body: ClauseBody::Drop(vars) })
    }

    fn parse_signal(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip SIGNAL
        if self.peek_keyword("ON") {
            self.advance();
            let condition = self.expect_symbol()?;
            let name = if self.peek_keyword("NAME") {
                self.advance();
                Some(self.expect_symbol()?)
            } else {
                None
            };
            Ok(Clause {
                line,
                body: ClauseBody::Signal(SignalTarget::On { condition, name }),
            })
        } else if self.peek_keyword("OFF") {
            self.advance();
            let condition = self.expect_symbol()?;
            Ok(Clause {
                line,
                body: ClauseBody::Signal(SignalTarget::Off(condition)),
            })
        } else {
            let label = self.expect_symbol()?;
            Ok(Clause {
                line,
                body: ClauseBody::Signal(SignalTarget::Label(label)),
            })
        }
    }

    fn parse_iterate(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip ITERATE
        let var = if self.at_clause_end() {
            None
        } else {
            Some(self.expect_symbol()?)
        };
        Ok(Clause { line, body: ClauseBody::Iterate(var) })
    }

    fn parse_leave(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip LEAVE
        let var = if self.at_clause_end() {
            None
        } else {
            Some(self.expect_symbol()?)
        };
        Ok(Clause { line, body: ClauseBody::Leave(var) })
    }

    fn parse_trace(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip TRACE
        let setting = if self.at_clause_end() {
            "N".to_string()
        } else {
            self.expect_symbol()?
        };
        Ok(Clause { line, body: ClauseBody::Trace(setting) })
    }

    fn parse_address(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip ADDRESS
        if self.at_clause_end() {
            return Ok(Clause {
                line,
                body: ClauseBody::Address { environment: None, command: None },
            });
        }
        let env = self.expect_symbol()?;
        let command = if self.at_clause_end() {
            None
        } else {
            Some(self.parse_expr()?)
        };
        Ok(Clause {
            line,
            body: ClauseBody::Address { environment: Some(env), command },
        })
    }

    fn parse_procedure(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip PROCEDURE
        let mut expose = Vec::new();
        if self.peek_keyword("EXPOSE") {
            self.advance();
            while !self.at_clause_end() {
                expose.push(self.expect_symbol()?);
            }
        }
        Ok(Clause { line, body: ClauseBody::Procedure { expose } })
    }

    fn parse_numeric(&mut self, line: u32) -> Result<Clause, ParseError> {
        self.advance(); // skip NUMERIC
        let setting = self.expect_symbol()?; // DIGITS, FUZZ, or FORM
        let value = self.parse_expr()?;
        Ok(Clause { line, body: ClauseBody::Numeric { setting, value } })
    }

    // -----------------------------------------------------------------------
    // Expression parser (Pratt-style)
    // -----------------------------------------------------------------------

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_or_expr()
    }

    fn parse_or_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_and_expr()?;
        while let Some(op) = self.peek_binop_or() {
            self.advance();
            let right = self.parse_and_expr()?;
            left = Expr::BinOp { left: Box::new(left), op, right: Box::new(right) };
        }
        Ok(left)
    }

    fn parse_and_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_comparison()?;
        while let Some(op) = self.peek_binop_and() {
            self.advance();
            let right = self.parse_comparison()?;
            left = Expr::BinOp { left: Box::new(left), op, right: Box::new(right) };
        }
        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_concat()?;
        if let Some(op) = self.peek_binop_cmp() {
            self.advance();
            let right = self.parse_concat()?;
            left = Expr::BinOp { left: Box::new(left), op, right: Box::new(right) };
        }
        Ok(left)
    }

    fn parse_concat(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_additive()?;
        while self.peek_kind() == Some(&TokenKind::Concat) {
            self.advance();
            let right = self.parse_additive()?;
            left = Expr::BinOp {
                left: Box::new(left),
                op: BinOp::Concat,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_multiplicative()?;
        loop {
            match self.peek_kind() {
                Some(TokenKind::Plus) => {
                    self.advance();
                    let right = self.parse_multiplicative()?;
                    left = Expr::BinOp { left: Box::new(left), op: BinOp::Add, right: Box::new(right) };
                }
                Some(TokenKind::Minus) => {
                    self.advance();
                    let right = self.parse_multiplicative()?;
                    left = Expr::BinOp { left: Box::new(left), op: BinOp::Sub, right: Box::new(right) };
                }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_power()?;
        loop {
            match self.peek_kind() {
                Some(TokenKind::Star) => {
                    self.advance();
                    let right = self.parse_power()?;
                    left = Expr::BinOp { left: Box::new(left), op: BinOp::Mul, right: Box::new(right) };
                }
                Some(TokenKind::Slash) => {
                    self.advance();
                    let right = self.parse_power()?;
                    left = Expr::BinOp { left: Box::new(left), op: BinOp::Div, right: Box::new(right) };
                }
                Some(TokenKind::Percent) => {
                    self.advance();
                    let right = self.parse_power()?;
                    left = Expr::BinOp { left: Box::new(left), op: BinOp::IntDiv, right: Box::new(right) };
                }
                Some(TokenKind::SlashSlash) => {
                    self.advance();
                    let right = self.parse_power()?;
                    left = Expr::BinOp { left: Box::new(left), op: BinOp::Mod, right: Box::new(right) };
                }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_power(&mut self) -> Result<Expr, ParseError> {
        let base = self.parse_unary()?;
        if self.peek_kind() == Some(&TokenKind::StarStar) {
            self.advance();
            let exp = self.parse_unary()?;
            Ok(Expr::BinOp {
                left: Box::new(base),
                op: BinOp::Power,
                right: Box::new(exp),
            })
        } else {
            Ok(base)
        }
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Minus) => {
                self.advance();
                let operand = self.parse_primary()?;
                Ok(Expr::UnaryOp { op: UnaryOp::Neg, operand: Box::new(operand) })
            }
            Some(TokenKind::Plus) => {
                self.advance();
                let operand = self.parse_primary()?;
                Ok(Expr::UnaryOp { op: UnaryOp::Pos, operand: Box::new(operand) })
            }
            Some(TokenKind::Not) => {
                self.advance();
                let operand = self.parse_primary()?;
                Ok(Expr::UnaryOp { op: UnaryOp::Not, operand: Box::new(operand) })
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Number(n)) => {
                let n = n.clone();
                self.advance();
                Ok(Expr::Number(n))
            }
            Some(TokenKind::StringLit(s)) => {
                let s = s.clone();
                self.advance();
                Ok(Expr::StringLit(s))
            }
            Some(TokenKind::HexString(s)) => {
                let s = s.clone();
                self.advance();
                Ok(Expr::HexLit(s))
            }
            Some(TokenKind::BinString(s)) => {
                let s = s.clone();
                self.advance();
                Ok(Expr::BinLit(s))
            }
            Some(TokenKind::Symbol(s)) => {
                let s = s.clone();
                self.advance();
                // Check for function call: symbol followed by (
                if self.peek_kind() == Some(&TokenKind::LParen) {
                    self.advance(); // skip (
                    let mut args = Vec::new();
                    if self.peek_kind() != Some(&TokenKind::RParen) {
                        args.push(self.parse_expr()?);
                        while self.peek_kind() == Some(&TokenKind::Comma) {
                            self.advance();
                            args.push(self.parse_expr()?);
                        }
                    }
                    self.expect(TokenKind::RParen)?;
                    Ok(Expr::FunctionCall { name: s, args })
                } else {
                    Ok(Expr::Variable(s))
                }
            }
            Some(TokenKind::LParen) => {
                self.advance(); // skip (
                let expr = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                Ok(expr)
            }
            Some(other) => {
                Err(ParseError::Unexpected {
                    found: other.clone(),
                    expected: "expression".to_string(),
                    line: self.current_line(),
                })
            }
            None => {
                Err(ParseError::UnexpectedEof {
                    expected: "expression".to_string(),
                })
            }
        }
    }

    // -----------------------------------------------------------------------
    // Operator precedence helpers
    // -----------------------------------------------------------------------

    fn peek_binop_or(&self) -> Option<BinOp> {
        match self.peek_kind() {
            Some(TokenKind::Or) => Some(BinOp::Or),
            Some(TokenKind::Xor) => Some(BinOp::Xor),
            _ => None,
        }
    }

    fn peek_binop_and(&self) -> Option<BinOp> {
        match self.peek_kind() {
            Some(TokenKind::And) => Some(BinOp::And),
            _ => None,
        }
    }

    fn peek_binop_cmp(&self) -> Option<BinOp> {
        match self.peek_kind() {
            Some(TokenKind::Eq) => Some(BinOp::Eq),
            Some(TokenKind::Ne) => Some(BinOp::Ne),
            Some(TokenKind::Gt) => Some(BinOp::Gt),
            Some(TokenKind::Lt) => Some(BinOp::Lt),
            Some(TokenKind::Ge) => Some(BinOp::Ge),
            Some(TokenKind::Le) => Some(BinOp::Le),
            Some(TokenKind::StrictGt) => Some(BinOp::StrictGt),
            Some(TokenKind::StrictLt) => Some(BinOp::StrictLt),
            Some(TokenKind::StrictGe) => Some(BinOp::StrictGe),
            Some(TokenKind::StrictLe) => Some(BinOp::StrictLe),
            _ => None,
        }
    }

    // -----------------------------------------------------------------------
    // Token utilities
    // -----------------------------------------------------------------------

    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    fn peek_kind(&self) -> Option<&TokenKind> {
        self.tokens.get(self.pos).map(|t| &t.kind)
    }

    fn peek_symbol(&self) -> Option<String> {
        match self.peek_kind() {
            Some(TokenKind::Symbol(s)) => Some(s.clone()),
            _ => None,
        }
    }

    fn peek_keyword(&self, kw: &str) -> bool {
        match self.peek_kind() {
            Some(TokenKind::Symbol(s)) => s == kw,
            _ => false,
        }
    }

    fn current_line(&self) -> u32 {
        self.tokens.get(self.pos).map(|t| t.span.line).unwrap_or(0)
    }

    fn at_eof(&self) -> bool {
        matches!(self.peek_kind(), Some(TokenKind::Eof) | None)
    }

    fn at_clause_end(&self) -> bool {
        matches!(
            self.peek_kind(),
            Some(TokenKind::Eol)
                | Some(TokenKind::Semicolon)
                | Some(TokenKind::Eof)
                | None
        )
    }

    fn skip_separators(&mut self) {
        while matches!(
            self.peek_kind(),
            Some(TokenKind::Eol) | Some(TokenKind::Semicolon)
        ) {
            self.advance();
        }
    }

    fn expect_symbol(&mut self) -> Result<String, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Symbol(s)) => {
                let s = s.clone();
                self.advance();
                Ok(s)
            }
            Some(other) => Err(ParseError::Unexpected {
                found: other.clone(),
                expected: "symbol".to_string(),
                line: self.current_line(),
            }),
            None => Err(ParseError::UnexpectedEof {
                expected: "symbol".to_string(),
            }),
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<(), ParseError> {
        match self.peek_kind() {
            Some(k) if std::mem::discriminant(k) == std::mem::discriminant(&expected) => {
                self.advance();
                Ok(())
            }
            Some(other) => Err(ParseError::Unexpected {
                found: other.clone(),
                expected: format!("{expected:?}"),
                line: self.current_line(),
            }),
            None => Err(ParseError::UnexpectedEof {
                expected: format!("{expected:?}"),
            }),
        }
    }

    fn expect_keyword(&mut self, kw: &str) -> Result<(), ParseError> {
        if self.peek_keyword(kw) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::Unexpected {
                found: self.peek_kind().cloned().unwrap_or(TokenKind::Eof),
                expected: kw.to_string(),
                line: self.current_line(),
            })
        }
    }

    /// Is the current position a label? (symbol followed by ':')
    fn is_label(&self) -> bool {
        matches!(self.peek_kind(), Some(TokenKind::Symbol(_)))
            && matches!(
                self.tokens.get(self.pos + 1).map(|t| &t.kind),
                Some(TokenKind::Colon)
            )
    }

    /// Is the current position an assignment? (symbol followed by '=')
    fn is_assignment(&self) -> bool {
        matches!(self.peek_kind(), Some(TokenKind::Symbol(_)))
            && matches!(
                self.tokens.get(self.pos + 1).map(|t| &t.kind),
                Some(TokenKind::Eq)
            )
    }

    /// Is the current position a DO-iterative? (symbol = expr TO ...)
    fn is_do_iterative(&self) -> bool {
        if !matches!(self.peek_kind(), Some(TokenKind::Symbol(_))) {
            return false;
        }
        if !matches!(
            self.tokens.get(self.pos + 1).map(|t| &t.kind),
            Some(TokenKind::Eq)
        ) {
            return false;
        }
        // Look ahead for TO
        let mut i = self.pos + 2;
        while i < self.tokens.len() {
            match &self.tokens[i].kind {
                TokenKind::Symbol(s) if s == "TO" => return true,
                TokenKind::Eol | TokenKind::Semicolon | TokenKind::Eof => return false,
                _ => {}
            }
            i += 1;
        }
        false
    }

    /// Collect remaining text on the current clause as a raw string (for PARSE templates).
    fn collect_remaining_text(&mut self) -> String {
        let mut parts = Vec::new();
        while !self.at_clause_end() {
            match self.peek_kind() {
                Some(TokenKind::Symbol(s)) => {
                    parts.push(s.clone());
                    self.advance();
                }
                Some(TokenKind::StringLit(s)) => {
                    parts.push(format!("'{s}'"));
                    self.advance();
                }
                Some(TokenKind::Number(n)) => {
                    parts.push(n.clone());
                    self.advance();
                }
                Some(TokenKind::Dot) => {
                    parts.push(".".to_string());
                    self.advance();
                }
                Some(TokenKind::LParen) => {
                    parts.push("(".to_string());
                    self.advance();
                }
                Some(TokenKind::RParen) => {
                    parts.push(")".to_string());
                    self.advance();
                }
                Some(TokenKind::Comma) => {
                    parts.push(",".to_string());
                    self.advance();
                }
                Some(TokenKind::Plus) => {
                    parts.push("+".to_string());
                    self.advance();
                }
                Some(TokenKind::Minus) => {
                    parts.push("-".to_string());
                    self.advance();
                }
                _ => {
                    self.advance(); // skip unknown
                }
            }
        }
        parts.join(" ")
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;

    fn parse_src(src: &str) -> Program {
        let tokens = lex(src).unwrap();
        parse(&tokens).unwrap()
    }

    #[test]
    fn test_parse_say() {
        let prog = parse_src("SAY 'Hello World'");
        assert_eq!(prog.clauses.len(), 1);
        assert!(matches!(&prog.clauses[0].body, ClauseBody::Say(_)));
    }

    #[test]
    fn test_parse_assignment() {
        let prog = parse_src("x = 42");
        assert_eq!(prog.clauses.len(), 1);
        match &prog.clauses[0].body {
            ClauseBody::Assignment { var, .. } => assert_eq!(var, "X"),
            other => panic!("expected assignment, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_if_then_else() {
        let prog = parse_src("IF x > 5 THEN SAY 'big'\nELSE SAY 'small'");
        assert_eq!(prog.clauses.len(), 1);
        match &prog.clauses[0].body {
            ClauseBody::If { else_clause, .. } => {
                assert!(else_clause.is_some());
            }
            other => panic!("expected IF, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_do_simple() {
        let prog = parse_src("DO\n  SAY 'hello'\nEND");
        assert_eq!(prog.clauses.len(), 1);
        match &prog.clauses[0].body {
            ClauseBody::Do { control, body } => {
                assert!(matches!(control, DoControl::Simple));
                assert_eq!(body.len(), 1);
            }
            other => panic!("expected DO, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_do_iterative() {
        let prog = parse_src("DO i = 1 TO 10 BY 2\n  SAY i\nEND");
        match &prog.clauses[0].body {
            ClauseBody::Do { control, body } => {
                match control {
                    DoControl::Iterative { var, by, .. } => {
                        assert_eq!(var, "I");
                        assert!(by.is_some());
                    }
                    other => panic!("expected iterative, got {other:?}"),
                }
                assert_eq!(body.len(), 1);
            }
            other => panic!("expected DO, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_do_forever() {
        let prog = parse_src("DO FOREVER\n  SAY 'loop'\n  LEAVE\nEND");
        match &prog.clauses[0].body {
            ClauseBody::Do { control, body } => {
                assert!(matches!(control, DoControl::Forever));
                assert_eq!(body.len(), 2);
            }
            other => panic!("expected DO, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_select() {
        let prog = parse_src("SELECT\n  WHEN x = 1 THEN SAY 'one'\n  WHEN x = 2 THEN SAY 'two'\n  OTHERWISE SAY 'other'\nEND");
        match &prog.clauses[0].body {
            ClauseBody::Select { whens, otherwise } => {
                assert_eq!(whens.len(), 2);
                assert!(otherwise.is_some());
            }
            other => panic!("expected SELECT, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_call() {
        let prog = parse_src("CALL MYFUNC 'arg1', 42");
        match &prog.clauses[0].body {
            ClauseBody::Call { name, args } => {
                assert_eq!(name, "MYFUNC");
                assert_eq!(args.len(), 2);
            }
            other => panic!("expected CALL, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_return_exit() {
        let prog = parse_src("RETURN 0");
        assert!(matches!(&prog.clauses[0].body, ClauseBody::Return(Some(_))));

        let prog = parse_src("EXIT");
        assert!(matches!(&prog.clauses[0].body, ClauseBody::Exit(None)));
    }

    #[test]
    fn test_parse_label() {
        let prog = parse_src("myLabel:\nRETURN");
        assert_eq!(prog.clauses.len(), 2);
        match &prog.clauses[0].body {
            ClauseBody::Label(name) => assert_eq!(name, "MYLABEL"),
            other => panic!("expected Label, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_procedure() {
        let prog = parse_src("myFunc:\nPROCEDURE EXPOSE x y\nRETURN x + y");
        assert_eq!(prog.clauses.len(), 3);
        match &prog.clauses[1].body {
            ClauseBody::Procedure { expose } => {
                assert_eq!(expose, &["X".to_string(), "Y".to_string()]);
            }
            other => panic!("expected Procedure, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_signal() {
        let prog = parse_src("SIGNAL ON ERROR NAME myHandler");
        match &prog.clauses[0].body {
            ClauseBody::Signal(SignalTarget::On { condition, name }) => {
                assert_eq!(condition, "ERROR");
                assert_eq!(name.as_deref(), Some("MYHANDLER"));
            }
            other => panic!("expected Signal On, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_drop() {
        let prog = parse_src("DROP a b c");
        match &prog.clauses[0].body {
            ClauseBody::Drop(vars) => {
                assert_eq!(vars, &["A", "B", "C"]);
            }
            other => panic!("expected Drop, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_expr_arithmetic() {
        let prog = parse_src("x = 2 + 3 * 4");
        match &prog.clauses[0].body {
            ClauseBody::Assignment { expr, .. } => {
                // Should parse as 2 + (3 * 4) due to precedence
                match expr {
                    Expr::BinOp { op: BinOp::Add, .. } => {}
                    other => panic!("expected Add at top, got {other:?}"),
                }
            }
            other => panic!("expected assignment, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_function_call() {
        let prog = parse_src("x = LENGTH('hello')");
        match &prog.clauses[0].body {
            ClauseBody::Assignment { expr, .. } => {
                match expr {
                    Expr::FunctionCall { name, args } => {
                        assert_eq!(name, "LENGTH");
                        assert_eq!(args.len(), 1);
                    }
                    other => panic!("expected function call, got {other:?}"),
                }
            }
            other => panic!("expected assignment, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_full_program() {
        let src = r#"/* REXX */
SAY 'Hello World'
x = 10
IF x > 5 THEN DO
  SAY 'x is big'
  x = x - 1
END
ELSE SAY 'x is small'
EXIT 0
"#;
        let tokens = lex(src).unwrap();
        let prog = parse(&tokens).unwrap();
        // Should parse without errors
        assert!(prog.clauses.len() >= 3); // SAY, assignment, IF, EXIT
    }

    #[test]
    fn test_parse_command() {
        // Unrecognized instruction → host command
        let prog = parse_src("'ALLOC DA(TEST) SHR'");
        assert!(matches!(&prog.clauses[0].body, ClauseBody::Command(_)));
    }

    #[test]
    fn test_parse_parse_arg() {
        let prog = parse_src("PARSE ARG name age");
        match &prog.clauses[0].body {
            ClauseBody::Parse { upper, source, template } => {
                assert!(!upper);
                assert!(matches!(source, ParseSource::Arg));
                assert!(template.contains("NAME"));
                assert!(template.contains("AGE"));
            }
            other => panic!("expected Parse, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_parse_upper_arg() {
        let prog = parse_src("PARSE UPPER ARG input");
        match &prog.clauses[0].body {
            ClauseBody::Parse { upper, source, .. } => {
                assert!(upper);
                assert!(matches!(source, ParseSource::Arg));
            }
            other => panic!("expected Parse, got {other:?}"),
        }
    }
}
