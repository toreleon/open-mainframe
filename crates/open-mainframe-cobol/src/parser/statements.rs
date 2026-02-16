//! PROCEDURE DIVISION and statement parsers.
//!
//! This module contains the parser methods for the PROCEDURE DIVISION header,
//! procedure body structure (sections, paragraphs), statement dispatch, and
//! all individual statement parsers including:
//! - Basic statements (MOVE, DISPLAY, STOP, PERFORM, IF, EVALUATE, CONTINUE)
//! - Arithmetic statements (ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE)
//! - String statements (STRING, UNSTRING)
//! - Control flow statements (CALL, GO TO, EXIT, EXEC CICS/SQL)
//! - File I/O statements (OPEN, CLOSE, READ, WRITE, REWRITE, DELETE, START)
//! - Other statements (SET, INITIALIZE, ACCEPT, CANCEL, SORT, MERGE,
//!   RELEASE, RETURN, SEARCH, INSPECT)

use super::Result;
use crate::ast::*;
use crate::error::CobolError;
use crate::lexer::{Keyword, Span, TokenKind};

impl super::Parser {
    // ========================================================================
    // PROCEDURE DIVISION
    // ========================================================================

    pub(super) fn parse_procedure_division(&mut self) -> Result<ProcedureDivision> {
        let start = self.current_span();

        // PROCEDURE DIVISION
        self.expect_keyword(Keyword::Procedure)?;
        self.expect_keyword(Keyword::Division)?;

        let mut using = Vec::new();
        let mut returning = None;

        // USING clause
        if self.check_keyword(Keyword::Using) {
            self.advance();
            while self.check_identifier()
                || self.check_keyword(Keyword::Reference)
                || self.check_keyword(Keyword::Content)
                || self.check_keyword(Keyword::Value)
            {
                let mode = if self.check_keyword(Keyword::By) {
                    self.advance();
                    if self.check_keyword(Keyword::Reference) {
                        self.advance();
                        ParameterMode::Reference
                    } else if self.check_keyword(Keyword::Content) {
                        self.advance();
                        ParameterMode::Content
                    } else if self.check_keyword(Keyword::Value) {
                        self.advance();
                        ParameterMode::Value
                    } else {
                        ParameterMode::Reference
                    }
                } else if self.check_keyword(Keyword::Reference)
                    || self.check_keyword(Keyword::Content)
                    || self.check_keyword(Keyword::Value)
                {
                    if self.check_keyword(Keyword::Reference) {
                        self.advance();
                        ParameterMode::Reference
                    } else if self.check_keyword(Keyword::Content) {
                        self.advance();
                        ParameterMode::Content
                    } else {
                        self.advance();
                        ParameterMode::Value
                    }
                } else {
                    ParameterMode::Reference
                };

                if self.check_identifier() {
                    let name = self.parse_qualified_name()?;
                    using.push(UsingParameter {
                        name: name.clone(),
                        mode,
                        span: name.span,
                    });
                }
                // Skip commas between parameters
                if self.check(TokenKind::Comma) {
                    self.advance();
                }
            }
        }

        // RETURNING clause
        if self.check_keyword(Keyword::Return) {
            self.advance();
            returning = Some(self.parse_qualified_name()?);
        }

        self.expect(TokenKind::Period)?;

        // Parse procedure body
        let body = self.parse_procedure_body()?;

        let end = self.previous_span();

        Ok(ProcedureDivision {
            using,
            returning,
            body,
            span: start.extend(end),
        })
    }

    fn parse_procedure_body(&mut self) -> Result<ProcedureBody> {
        let mut sections = Vec::new();
        let mut paragraphs = Vec::new();
        let mut statements = Vec::new();

        while !self.is_at_end()
            && !self.is_at_end_program()
            && !self.check_keyword(Keyword::Identification)
        {
            // Check for section header
            if self.check_identifier() && self.peek_keyword(Keyword::Section) {
                let section = self.parse_section()?;
                sections.push(section);
            }
            // Check for paragraph header
            else if self.check_identifier() && self.peek(TokenKind::Period) {
                let para = self.parse_paragraph()?;
                paragraphs.push(para);
            }
            // Otherwise parse statement
            else if !self.check(TokenKind::Eof) {
                match self.parse_statement() {
                    Ok(stmt) => statements.push(stmt),
                    Err(e) => {
                        self.errors.push(e);
                        self.advance_to_next_sentence();
                    }
                }
            } else {
                break;
            }
        }

        if !sections.is_empty() {
            Ok(ProcedureBody::Sections(sections))
        } else if !paragraphs.is_empty() {
            Ok(ProcedureBody::Paragraphs(paragraphs))
        } else {
            Ok(ProcedureBody::Statements(statements))
        }
    }

    fn parse_section(&mut self) -> Result<Section> {
        let start = self.current_span();
        let name = self.expect_identifier()?;
        self.expect_keyword(Keyword::Section)?;
        self.expect(TokenKind::Period)?;

        let mut paragraphs = Vec::new();

        while !(self.is_at_end()
            || self.is_at_end_program()
            || self.check_keyword(Keyword::Identification)
            || (self.check_identifier() && self.peek_keyword(Keyword::Section)))
        {
            if self.check_identifier() && self.peek(TokenKind::Period) {
                paragraphs.push(self.parse_paragraph()?);
            } else if !self.check(TokenKind::Eof) {
                // Inline statement in section
                break;
            } else {
                break;
            }
        }

        let end = self.previous_span();

        Ok(Section {
            name,
            paragraphs,
            span: start.extend(end),
        })
    }

    fn parse_paragraph(&mut self) -> Result<Paragraph> {
        let start = self.current_span();
        let name = self.expect_identifier()?;
        self.expect(TokenKind::Period)?;

        let mut statements = Vec::new();

        while !(self.is_at_end()
            || self.is_at_end_program()
            || self.check_keyword(Keyword::Identification)
            || (self.check_identifier() && self.peek_keyword(Keyword::Section))
            || (self.check_identifier() && self.peek(TokenKind::Period)))
        {
            // Skip stray periods (sentence terminators) between statements
            if self.check(TokenKind::Period) {
                self.advance();
                continue;
            }
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    self.errors.push(e);
                    self.advance_to_next_sentence();
                }
            }
        }

        let end = self.previous_span();

        Ok(Paragraph {
            name,
            statements,
            span: start.extend(end),
        })
    }

    // ========================================================================
    // BASIC STATEMENTS
    // ========================================================================

    pub(super) fn parse_move_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // MOVE

        let corresponding =
            if self.check_keyword(Keyword::Corresponding) || self.check_keyword(Keyword::Corr) {
                self.advance();
                true
            } else {
                false
            };

        let from = self.parse_expression()?;

        self.expect_keyword(Keyword::To)?;

        let mut to = Vec::new();
        while !self.check(TokenKind::Period) && !self.is_at_end() && !self.is_statement_start() {
            to.push(self.parse_qualified_name()?);
        }

        // Optional period
        self.skip_if(TokenKind::Period);

        let end = self.previous_span();

        Ok(Statement::Move(MoveStatement {
            from,
            to,
            corresponding,
            span: start.extend(end),
        }))
    }

    pub(super) fn parse_display_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // DISPLAY

        let mut items = Vec::new();
        let mut upon = None;
        let mut no_advancing = false;

        while !self.check(TokenKind::Period)
            && !self.is_at_end()
            && !self.is_statement_start()
            && !self.check_keyword(Keyword::Upon)
            && !self.check_keyword(Keyword::With)
        {
            items.push(self.parse_expression()?);
        }

        if self.check_keyword(Keyword::Upon) {
            self.advance();
            upon = Some(self.expect_identifier()?);
        }

        if self.check_keyword(Keyword::With) {
            self.advance();
            if self.check_keyword(Keyword::Not) {
                self.advance();
            }
            // NO ADVANCING
            self.advance(); // Skip to next
            no_advancing = true;
        }

        // Optional period
        self.skip_if(TokenKind::Period);

        let end = self.previous_span();

        Ok(Statement::Display(DisplayStatement {
            items,
            upon,
            no_advancing,
            span: start.extend(end),
        }))
    }

    pub(super) fn parse_stop_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // STOP

        // STOP RUN [return-code] or STOP literal
        if self.check_keyword(Keyword::Run) {
            self.advance(); // RUN
            let return_code = if !self.check(TokenKind::Period) && !self.is_statement_start() {
                Some(self.parse_expression()?)
            } else {
                None
            };
            self.skip_if(TokenKind::Period);
            let end = self.previous_span();
            Ok(Statement::StopRun(StopRunStatement {
                return_code,
                is_literal: false,
                span: start.extend(end),
            }))
        } else {
            // STOP literal — display literal and pause
            let literal = self.parse_expression()?;
            self.skip_if(TokenKind::Period);
            let end = self.previous_span();
            Ok(Statement::StopRun(StopRunStatement {
                return_code: Some(literal),
                is_literal: true,
                span: start.extend(end),
            }))
        }
    }

    pub(super) fn parse_goback_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // GOBACK

        // Optional RETURNING expression
        let returning = if self.check_keyword(Keyword::Returning) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.skip_if(TokenKind::Period);

        let end = self.previous_span();

        Ok(Statement::GoBack(GoBackStatement {
            returning,
            span: start.extend(end),
        }))
    }

    pub(super) fn parse_perform_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // PERFORM

        // Check for inline PERFORM
        if self.is_statement_start()
            || self.check_keyword(Keyword::Until)
            || self.check_keyword(Keyword::Varying)
        {
            // Inline PERFORM
            let mut until = None;
            let mut varying = None;
            let test_before = true;

            if self.check_keyword(Keyword::Until) {
                self.advance();
                until = Some(self.parse_condition()?);
            } else if self.check_keyword(Keyword::Varying) {
                // PERFORM VARYING variable FROM expr BY expr UNTIL condition
                self.advance(); // VARYING
                let var_start = self.current_span();
                let variable = self.parse_qualified_name()?;

                self.expect_keyword(Keyword::From)?;
                let from = self.parse_expression()?;

                self.expect_keyword(Keyword::By)?;
                let by = self.parse_expression()?;

                self.expect_keyword(Keyword::Until)?;
                let vary_until = self.parse_condition()?;

                let var_end = self.previous_span();
                varying = Some(PerformVarying {
                    variable,
                    from,
                    by,
                    until: vary_until,
                    after: Vec::new(),
                    span: var_start.extend(var_end),
                });
            }

            // Parse inline statements until END-PERFORM
            let mut inline = Vec::new();
            while !self.check_keyword(Keyword::EndPerform) && !self.is_at_end() {
                match self.parse_statement() {
                    Ok(stmt) => inline.push(stmt),
                    Err(e) => {
                        self.errors.push(e);
                        self.advance_to_next_sentence();
                        break;
                    }
                }
            }

            if self.check_keyword(Keyword::EndPerform) {
                self.advance();
            }

            // Consume optional trailing period after END-PERFORM
            self.skip_if(TokenKind::Period);

            let end = self.previous_span();

            return Ok(Statement::Perform(PerformStatement {
                target: None,
                thru: None,
                inline: Some(inline),
                times: None,
                until,
                varying,
                test_before,
                span: start.extend(end),
            }));
        }

        // Out-of-line PERFORM
        let target_name = self.expect_identifier()?;
        let target = Some(PerformTarget {
            name: target_name,
            span: self.previous_span(),
        });

        let mut thru = None;
        if self.check_keyword(Keyword::Thru) || self.check_keyword(Keyword::Through) {
            self.advance();
            thru = Some(self.expect_identifier()?);
        }

        let mut times = None;
        let mut until = None;

        if !self.check(TokenKind::Period) && !self.is_statement_start() {
            if self.check_keyword(Keyword::Times) {
                // n TIMES - but the number should have been parsed
                self.advance();
            } else if self.check_keyword(Keyword::Until) {
                self.advance();
                until = Some(self.parse_condition()?);
            } else if let Ok(expr) = self.parse_expression() {
                // Could be TIMES
                if self.check_keyword(Keyword::Times) {
                    self.advance();
                    times = Some(expr);
                }
            }
        }

        self.skip_if(TokenKind::Period);

        let end = self.previous_span();

        Ok(Statement::Perform(PerformStatement {
            target,
            thru,
            inline: None,
            times,
            until,
            varying: None,
            test_before: true,
            span: start.extend(end),
        }))
    }

    pub(super) fn parse_if_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // IF

        let condition = self.parse_condition()?;

        // Optional THEN
        if self.check_keyword(Keyword::Then) {
            self.advance();
        }

        // Parse THEN branch
        let mut then_branch = Vec::new();
        while !self.check_keyword(Keyword::Else)
            && !self.check_keyword(Keyword::EndIf)
            && !self.check(TokenKind::Period)
            && !self.is_at_end()
        {
            match self.parse_statement() {
                Ok(stmt) => then_branch.push(stmt),
                Err(e) => {
                    self.errors.push(e);
                    break;
                }
            }
        }

        // Parse ELSE branch
        let else_branch = if self.check_keyword(Keyword::Else) {
            self.advance();
            let mut stmts = Vec::new();
            while !self.check_keyword(Keyword::EndIf)
                && !self.check(TokenKind::Period)
                && !self.is_at_end()
            {
                match self.parse_statement() {
                    Ok(stmt) => stmts.push(stmt),
                    Err(e) => {
                        self.errors.push(e);
                        break;
                    }
                }
            }
            Some(stmts)
        } else {
            None
        };

        // END-IF or period
        if self.check_keyword(Keyword::EndIf) {
            self.advance();
        }
        self.skip_if(TokenKind::Period);

        let end = self.previous_span();

        Ok(Statement::If(IfStatement {
            condition,
            then_branch,
            else_branch,
            span: start.extend(end),
        }))
    }

    pub(super) fn parse_continue_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // CONTINUE
        self.skip_if(TokenKind::Period);
        Ok(Statement::Continue(ContinueStatement { span: start }))
    }

    pub(super) fn parse_evaluate_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // EVALUATE

        // Parse subjects - could be TRUE, FALSE, identifier, or expression
        let mut subjects = Vec::new();

        // Helper closure-like parse of a subject
        let subj = self.parse_evaluate_subject()?;
        subjects.push(subj);

        // Parse ALSO subjects
        while self.check_keyword(Keyword::Also) {
            self.advance();
            let subj = self.parse_evaluate_subject()?;
            subjects.push(subj);
        }

        let mut when_clauses = Vec::new();
        let mut when_other = None;

        // Parse WHEN clauses
        while self.check_keyword(Keyword::When) {
            self.advance(); // WHEN

            // Check for WHEN OTHER
            if self.check_keyword(Keyword::Other) {
                self.advance();
                let mut stmts = Vec::new();
                while !self.check_keyword(Keyword::EndEvaluate)
                    && !self.check_keyword(Keyword::When)
                    && !self.check(TokenKind::Period)
                    && !self.is_at_end()
                {
                    match self.parse_statement() {
                        Ok(stmt) => stmts.push(stmt),
                        Err(e) => {
                            self.errors.push(e);
                            self.advance_to_next_sentence();
                            break;
                        }
                    }
                }
                when_other = Some(stmts);
                break;
            }

            // Parse WHEN conditions (possibly multiple via ALSO)
            let when_start = self.current_span();
            let mut conditions = Vec::new();

            // Parse first condition
            conditions.push(self.parse_when_condition(&subjects, 0)?);

            // Parse ALSO conditions
            let mut idx = 1;
            while self.check_keyword(Keyword::Also) {
                self.advance();
                conditions.push(self.parse_when_condition(&subjects, idx)?);
                idx += 1;
            }

            // Check for additional WHEN clauses that share the same statements
            // (multiple WHEN on consecutive lines before statements)
            let mut extra_whens: Vec<Vec<WhenCondition>> = Vec::new();
            while self.check_keyword(Keyword::When) {
                // Peek to see if this is WHEN OTHER
                let saved = self.current;
                self.advance(); // WHEN
                if self.check_keyword(Keyword::Other) {
                    self.current = saved;
                    break;
                }
                // Parse this WHEN condition group
                let mut extra_conditions = Vec::new();
                extra_conditions.push(self.parse_when_condition(&subjects, 0)?);
                let mut eidx = 1;
                while self.check_keyword(Keyword::Also) {
                    self.advance();
                    extra_conditions.push(self.parse_when_condition(&subjects, eidx)?);
                    eidx += 1;
                }
                extra_whens.push(extra_conditions);
            }

            // Parse statements for this WHEN clause
            let mut stmts = Vec::new();
            while !self.check_keyword(Keyword::When)
                && !self.check_keyword(Keyword::EndEvaluate)
                && !self.check(TokenKind::Period)
                && !self.is_at_end()
            {
                match self.parse_statement() {
                    Ok(stmt) => stmts.push(stmt),
                    Err(e) => {
                        self.errors.push(e);
                        self.advance_to_next_sentence();
                        break;
                    }
                }
            }

            let when_end = self.previous_span();

            when_clauses.push(WhenClause {
                conditions,
                statements: stmts.clone(),
                span: when_start.extend(when_end),
            });

            // Add extra WHEN clauses with the same statements
            for extra_conds in extra_whens {
                when_clauses.push(WhenClause {
                    conditions: extra_conds,
                    statements: stmts.clone(),
                    span: when_start.extend(when_end),
                });
            }
        }

        // END-EVALUATE or period
        if self.check_keyword(Keyword::EndEvaluate) {
            self.advance();
        }
        self.skip_if(TokenKind::Period);

        let end = self.previous_span();

        Ok(Statement::Evaluate(EvaluateStatement {
            subjects,
            when_clauses,
            when_other,
            span: start.extend(end),
        }))
    }

    fn parse_evaluate_subject(&mut self) -> Result<Expression> {
        if self.check_keyword(Keyword::True) {
            let span = self.current_span();
            self.advance();
            Ok(Expression::Variable(QualifiedName {
                name: "TRUE".to_string(),
                qualifiers: Vec::new(),
                subscripts: Vec::new(),
                refmod: None,
                span,
            }))
        } else if self.check_keyword(Keyword::False) {
            let span = self.current_span();
            self.advance();
            Ok(Expression::Variable(QualifiedName {
                name: "FALSE".to_string(),
                qualifiers: Vec::new(),
                subscripts: Vec::new(),
                refmod: None,
                span,
            }))
        } else {
            self.parse_expression()
        }
    }

    fn parse_when_condition(
        &mut self,
        subjects: &[Expression],
        _subject_idx: usize,
    ) -> Result<WhenCondition> {
        // ANY
        if self.check_keyword(Keyword::Any) {
            self.advance();
            return Ok(WhenCondition::Any);
        }

        // TRUE
        if self.check_keyword(Keyword::True) {
            self.advance();
            return Ok(WhenCondition::True);
        }

        // FALSE
        if self.check_keyword(Keyword::False) {
            self.advance();
            return Ok(WhenCondition::False);
        }

        // For EVALUATE TRUE, WHEN conditions are conditions (comparisons)
        // For EVALUATE variable, WHEN conditions are values/expressions
        let is_evaluate_true = !subjects.is_empty()
            && matches!(
                &subjects[0],
                Expression::Variable(ref name) if name.name == "TRUE"
            );

        if is_evaluate_true {
            // Parse as a condition
            let cond = self.parse_condition()?;
            return Ok(WhenCondition::Condition(cond));
        }

        // Parse as a value expression - could be a range (value THRU value)
        let expr = self.parse_expression()?;

        if self.check_keyword(Keyword::Thru) || self.check_keyword(Keyword::Through) {
            self.advance();
            let to = self.parse_expression()?;
            Ok(WhenCondition::Range { from: expr, to })
        } else {
            Ok(WhenCondition::Value(expr))
        }
    }

    // ========================================================================
    // ARITHMETIC STATEMENTS
    // ========================================================================

    /// Parse ADD statement.
    /// ADD operand... TO target... [GIVING target...]
    /// ADD operand... TO operand GIVING target...
    pub(super) fn parse_add_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // ADD

        // Parse operands (values to add)
        let mut operands = Vec::new();
        while !self.check_keyword(Keyword::To)
            && !self.check_keyword(Keyword::Giving)
            && !self.check(TokenKind::Period)
            && !self.is_at_end()
        {
            operands.push(self.parse_expression()?);
        }

        // TO clause
        // Note: In "ADD a TO b GIVING c", b can be an expression (including ZERO)
        // In "ADD a TO b", b must be an identifier (target)
        let mut to = Vec::new();
        let mut to_expressions = Vec::new();
        if self.check_keyword(Keyword::To) {
            self.advance();
            while !self.check_keyword(Keyword::Giving)
                && !self.check_keyword(Keyword::OnSizeError)
                && !self.check_keyword(Keyword::NotOnSizeError)
                && !self.check_keyword(Keyword::EndAdd)
                && !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.is_statement_start()
            {
                // Parse as expression to handle figurative constants like ZERO
                let expr = self.parse_expression()?;
                let rounded = if self.check_keyword(Keyword::Rounded) {
                    self.advance();
                    true
                } else {
                    false
                };
                to_expressions.push((expr, rounded));
            }

            // If GIVING follows, these are operands; otherwise convert to targets
            if !self.check_keyword(Keyword::Giving) {
                for (expr, rounded) in to_expressions {
                    // Convert expression to qualified name for target
                    if let Expression::Variable(name) = expr {
                        to.push(AddTarget { name, rounded });
                    } else {
                        // Non-identifier target - semantic error, but parse it
                        // Use a placeholder name
                        to.push(AddTarget {
                            name: QualifiedName::simple("_ERROR_", Span::dummy()),
                            rounded,
                        });
                    }
                }
            } else {
                // GIVING follows, so TO expressions are operands
                for (expr, _) in to_expressions {
                    operands.push(expr);
                }
            }
        }

        // GIVING clause
        let mut giving = Vec::new();
        if self.check_keyword(Keyword::Giving) {
            self.advance();
            giving = self.parse_compute_targets()?;
        }

        // ON SIZE ERROR / NOT ON SIZE ERROR
        let (on_size_error, not_on_size_error) = self.parse_size_error_handlers()?;

        // END-ADD
        if self.check_keyword(Keyword::EndAdd) {
            self.advance();
        }

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Add(AddStatement {
            operands,
            to,
            giving,
            on_size_error,
            not_on_size_error,
            span: start.extend(end),
        }))
    }

    /// Parse SUBTRACT statement.
    /// SUBTRACT operand... FROM target... [GIVING target...]
    pub(super) fn parse_subtract_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // SUBTRACT

        // Parse operands
        let mut operands = Vec::new();
        while !self.check_keyword(Keyword::From)
            && !self.check(TokenKind::Period)
            && !self.is_at_end()
        {
            operands.push(self.parse_expression()?);
        }

        // FROM clause
        let mut from = Vec::new();
        if self.check_keyword(Keyword::From) {
            self.advance();
            while !self.check_keyword(Keyword::Giving)
                && !self.check_keyword(Keyword::OnSizeError)
                && !self.check_keyword(Keyword::NotOnSizeError)
                && !self.check_keyword(Keyword::EndSubtract)
                && !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.is_statement_start()
            {
                let name = self.parse_qualified_name()?;
                let rounded = if self.check_keyword(Keyword::Rounded) {
                    self.advance();
                    true
                } else {
                    false
                };
                from.push(AddTarget { name, rounded });
            }
        }

        // GIVING clause
        let mut giving = Vec::new();
        if self.check_keyword(Keyword::Giving) {
            self.advance();
            giving = self.parse_compute_targets()?;
        }

        // ON SIZE ERROR / NOT ON SIZE ERROR
        let (on_size_error, not_on_size_error) = self.parse_size_error_handlers()?;

        // END-SUBTRACT
        if self.check_keyword(Keyword::EndSubtract) {
            self.advance();
        }

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Subtract(SubtractStatement {
            operands,
            from,
            giving,
            on_size_error,
            not_on_size_error,
            span: start.extend(end),
        }))
    }

    /// Parse MULTIPLY statement.
    /// MULTIPLY operand BY operand [GIVING target...]
    pub(super) fn parse_multiply_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // MULTIPLY

        let operand = self.parse_expression()?;

        self.expect_keyword(Keyword::By)?;

        let by = self.parse_expression()?;

        // GIVING clause
        let mut giving = Vec::new();
        if self.check_keyword(Keyword::Giving) {
            self.advance();
            giving = self.parse_compute_targets()?;
        }

        // ON SIZE ERROR / NOT ON SIZE ERROR
        let (on_size_error, not_on_size_error) = self.parse_size_error_handlers()?;

        // END-MULTIPLY
        if self.check_keyword(Keyword::EndMultiply) {
            self.advance();
        }

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Multiply(MultiplyStatement {
            operand,
            by,
            giving,
            on_size_error,
            not_on_size_error,
            span: start.extend(end),
        }))
    }

    /// Parse DIVIDE statement.
    /// DIVIDE operand INTO operand [GIVING target...] [REMAINDER target]
    /// DIVIDE operand BY operand GIVING target... [REMAINDER target]
    pub(super) fn parse_divide_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // DIVIDE

        let operand = self.parse_expression()?;

        let (into_or_by, is_into) = if self.check_keyword(Keyword::Into) {
            self.advance();
            (self.parse_expression()?, true)
        } else if self.check_keyword(Keyword::By) {
            self.advance();
            (self.parse_expression()?, false)
        } else {
            return Err(CobolError::ParseError {
                message: "Expected INTO or BY in DIVIDE statement".to_string(),
            });
        };

        // GIVING clause
        let mut giving = Vec::new();
        if self.check_keyword(Keyword::Giving) {
            self.advance();
            giving = self.parse_compute_targets()?;
        }

        // REMAINDER clause
        let remainder = if self.check_keyword(Keyword::Remainder) {
            self.advance();
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        // ON SIZE ERROR / NOT ON SIZE ERROR
        let (on_size_error, not_on_size_error) = self.parse_size_error_handlers()?;

        // END-DIVIDE
        if self.check_keyword(Keyword::EndDivide) {
            self.advance();
        }

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Divide(DivideStatement {
            operand,
            into_or_by,
            is_into,
            giving,
            remainder,
            on_size_error,
            not_on_size_error,
            span: start.extend(end),
        }))
    }

    /// Parse COMPUTE statement.
    /// COMPUTE target... = expression
    pub(super) fn parse_compute_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // COMPUTE

        let targets = self.parse_compute_targets()?;

        // Expect = sign
        self.expect(TokenKind::Equals)?;

        let expression = self.parse_expression()?;

        // ON SIZE ERROR / NOT ON SIZE ERROR
        let (on_size_error, not_on_size_error) = self.parse_size_error_handlers()?;

        // END-COMPUTE
        if self.check_keyword(Keyword::EndCompute) {
            self.advance();
        }

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Compute(ComputeStatement {
            targets,
            expression,
            on_size_error,
            not_on_size_error,
            span: start.extend(end),
        }))
    }

    /// Parse COMPUTE targets (variable names with optional ROUNDED).
    fn parse_compute_targets(&mut self) -> Result<Vec<ComputeTarget>> {
        let mut targets = Vec::new();

        while !self.check(TokenKind::Equals)
            && !self.check_keyword(Keyword::OnSizeError)
            && !self.check_keyword(Keyword::NotOnSizeError)
            && !self.check_keyword(Keyword::Remainder)
            && !self.check(TokenKind::Period)
            && !self.is_at_end()
            && !self.is_statement_start()
            && self.check_identifier()
        {
            let name = self.parse_qualified_name()?;
            let rounded = if self.check_keyword(Keyword::Rounded) {
                self.advance();
                true
            } else {
                false
            };
            targets.push(ComputeTarget { name, rounded });
        }

        Ok(targets)
    }

    /// Parse ON SIZE ERROR and NOT ON SIZE ERROR handlers.
    #[allow(clippy::type_complexity)]
    fn parse_size_error_handlers(
        &mut self,
    ) -> Result<(Option<Vec<Statement>>, Option<Vec<Statement>>)> {
        let mut on_size_error = None;
        let mut not_on_size_error = None;

        if self.check_keyword(Keyword::OnSizeError) {
            self.advance();
            on_size_error = Some(self.parse_imperative_statements()?);
        }

        if self.check_keyword(Keyword::NotOnSizeError) {
            self.advance();
            not_on_size_error = Some(self.parse_imperative_statements()?);
        }

        Ok((on_size_error, not_on_size_error))
    }

    /// Parse imperative statements for error handlers.
    fn parse_imperative_statements(&mut self) -> Result<Vec<Statement>> {
        let mut statements = Vec::new();

        while !self.check_keyword(Keyword::EndAdd)
            && !self.check_keyword(Keyword::EndSubtract)
            && !self.check_keyword(Keyword::EndMultiply)
            && !self.check_keyword(Keyword::EndDivide)
            && !self.check_keyword(Keyword::EndCompute)
            && !self.check_keyword(Keyword::EndString)
            && !self.check_keyword(Keyword::EndUnstring)
            && !self.check_keyword(Keyword::NotOnSizeError)
            && !self.check_keyword(Keyword::NotOnOverflow)
            && !self.check(TokenKind::Period)
            && !self.is_at_end()
        {
            if self.is_statement_start() {
                statements.push(self.parse_statement()?);
            } else {
                break;
            }
        }

        Ok(statements)
    }

    // ========================================================================
    // STRING STATEMENTS
    // ========================================================================

    /// Parse STRING statement.
    /// STRING source... INTO target [WITH POINTER pointer] [ON OVERFLOW...]
    pub(super) fn parse_string_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // STRING

        // Parse sources
        let mut sources = Vec::new();
        while !self.check_keyword(Keyword::Into) && !self.is_at_end() {
            // Skip commas between sources
            if self.check(TokenKind::Comma) {
                self.advance();
                continue;
            }
            let value = self.parse_expression()?;

            let delimited_by = if self.check_keyword(Keyword::Delimited) {
                self.advance();
                if self.check_keyword(Keyword::By) {
                    self.advance();
                }
                if self.check_keyword(Keyword::Size) {
                    self.advance();
                    StringDelimiter::Size
                } else {
                    StringDelimiter::Value(self.parse_expression()?)
                }
            } else {
                StringDelimiter::Size
            };

            sources.push(StringSource {
                value,
                delimited_by,
            });
        }

        // INTO clause
        self.expect_keyword(Keyword::Into)?;
        let into = self.parse_qualified_name()?;

        // WITH POINTER clause
        let pointer = if self.check_keyword(Keyword::With) {
            self.advance();
            if self.check_keyword(Keyword::Pointer) {
                self.advance();
            }
            Some(self.parse_qualified_name()?)
        } else if self.check_keyword(Keyword::Pointer) {
            self.advance();
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        // ON OVERFLOW / NOT ON OVERFLOW
        let (on_overflow, not_on_overflow) = self.parse_overflow_handlers()?;

        // END-STRING
        if self.check_keyword(Keyword::EndString) {
            self.advance();
        }

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::String(StringStatement {
            sources,
            into,
            pointer,
            on_overflow,
            not_on_overflow,
            span: start.extend(end),
        }))
    }

    /// Parse UNSTRING statement.
    /// UNSTRING source DELIMITED BY delimiter... INTO target... [WITH POINTER...]
    pub(super) fn parse_unstring_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // UNSTRING

        let source = self.parse_qualified_name()?;

        // DELIMITED BY clause
        let mut delimiters = Vec::new();
        if self.check_keyword(Keyword::Delimited) {
            self.advance();
            if self.check_keyword(Keyword::By) {
                self.advance();
            }

            loop {
                let all = if self.check_keyword(Keyword::All) {
                    self.advance();
                    true
                } else {
                    false
                };
                let value = self.parse_expression()?;
                delimiters.push(UnstringDelimiter { all, value });

                if self.check_keyword(Keyword::Or) {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        // INTO clause
        let mut into = Vec::new();
        if self.check_keyword(Keyword::Into) {
            self.advance();
            while !self.check_keyword(Keyword::With)
                && !self.check_keyword(Keyword::Pointer)
                && !self.check_keyword(Keyword::Tallying)
                && !self.check_keyword(Keyword::OnOverflow)
                && !self.check_keyword(Keyword::NotOnOverflow)
                && !self.check_keyword(Keyword::EndUnstring)
                && !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.is_statement_start()
                && self.check_identifier()
            {
                let name = self.parse_qualified_name()?;

                let delimiter_in = if self.check_keyword(Keyword::Delimiter) {
                    self.advance();
                    Some(self.parse_qualified_name()?)
                } else {
                    None
                };

                let count_in = if self.check_keyword(Keyword::Count) {
                    self.advance();
                    Some(self.parse_qualified_name()?)
                } else {
                    None
                };

                into.push(UnstringTarget {
                    name,
                    delimiter_in,
                    count_in,
                });
            }
        }

        // WITH POINTER clause
        let pointer = if self.check_keyword(Keyword::With) {
            self.advance();
            if self.check_keyword(Keyword::Pointer) {
                self.advance();
            }
            Some(self.parse_qualified_name()?)
        } else if self.check_keyword(Keyword::Pointer) {
            self.advance();
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        // TALLYING clause
        let tallying = if self.check_keyword(Keyword::Tallying) {
            self.advance();
            // Skip IN if present (IN is not a keyword, check identifier)
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        // ON OVERFLOW / NOT ON OVERFLOW
        let (on_overflow, not_on_overflow) = self.parse_overflow_handlers()?;

        // END-UNSTRING
        if self.check_keyword(Keyword::EndUnstring) {
            self.advance();
        }

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Unstring(UnstringStatement {
            source,
            delimiters,
            into,
            pointer,
            tallying,
            on_overflow,
            not_on_overflow,
            span: start.extend(end),
        }))
    }

    /// Parse ON OVERFLOW and NOT ON OVERFLOW handlers.
    #[allow(clippy::type_complexity)]
    fn parse_overflow_handlers(
        &mut self,
    ) -> Result<(Option<Vec<Statement>>, Option<Vec<Statement>>)> {
        let mut on_overflow = None;
        let mut not_on_overflow = None;

        if self.check_keyword(Keyword::OnOverflow) {
            self.advance();
            on_overflow = Some(self.parse_imperative_statements()?);
        }

        if self.check_keyword(Keyword::NotOnOverflow) {
            self.advance();
            not_on_overflow = Some(self.parse_imperative_statements()?);
        }

        Ok((on_overflow, not_on_overflow))
    }

    // ========================================================================
    // CALL, GOTO, EXIT STATEMENTS
    // ========================================================================

    /// Parse CALL statement.
    /// CALL program [USING parameter...] [RETURNING var] [ON EXCEPTION...]
    pub(super) fn parse_call_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // CALL

        // Program name (literal or identifier)
        let program = self.parse_expression()?;

        // USING clause
        let mut using = Vec::new();
        if self.check_keyword(Keyword::Using) {
            self.advance();
            while !self.check_keyword(Keyword::Returning)
                && !self.check_keyword(Keyword::OnException)
                && !self.check_keyword(Keyword::NotOnException)
                && !self.check_keyword(Keyword::EndCall)
                && !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.is_statement_start()
            {
                let param_start = self.current_span();

                // Check for BY REFERENCE/CONTENT/VALUE
                let mode = if self.check_keyword(Keyword::By) {
                    self.advance();
                    if self.check_keyword(Keyword::Reference) {
                        self.advance();
                        ParameterMode::Reference
                    } else if self.check_keyword(Keyword::Content) {
                        self.advance();
                        ParameterMode::Content
                    } else if self.check_keyword(Keyword::Value) {
                        self.advance();
                        ParameterMode::Value
                    } else {
                        ParameterMode::Reference
                    }
                } else if self.check_keyword(Keyword::Reference) {
                    self.advance();
                    ParameterMode::Reference
                } else if self.check_keyword(Keyword::Content) {
                    self.advance();
                    ParameterMode::Content
                } else if self.check_keyword(Keyword::Value) {
                    self.advance();
                    ParameterMode::Value
                } else {
                    ParameterMode::Reference
                };

                let value = self.parse_expression()?;
                let param_end = self.previous_span();

                using.push(CallParameter {
                    value,
                    mode,
                    span: param_start.extend(param_end),
                });

                // Skip optional comma between parameters
                if self.check(TokenKind::Comma) {
                    self.advance();
                }
            }
        }

        // RETURNING clause
        let returning = if self.check_keyword(Keyword::Returning) {
            self.advance();
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        // ON EXCEPTION / NOT ON EXCEPTION
        let (on_exception, not_on_exception) = self.parse_exception_handlers()?;

        // END-CALL
        if self.check_keyword(Keyword::EndCall) {
            self.advance();
        }

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Call(CallStatement {
            program,
            using,
            returning,
            on_exception,
            not_on_exception,
            span: start.extend(end),
        }))
    }

    /// Parse ON EXCEPTION and NOT ON EXCEPTION handlers.
    #[allow(clippy::type_complexity)]
    fn parse_exception_handlers(
        &mut self,
    ) -> Result<(Option<Vec<Statement>>, Option<Vec<Statement>>)> {
        let mut on_exception = None;
        let mut not_on_exception = None;

        if self.check_keyword(Keyword::OnException) {
            self.advance();
            on_exception = Some(self.parse_imperative_statements()?);
        }

        if self.check_keyword(Keyword::NotOnException) {
            self.advance();
            not_on_exception = Some(self.parse_imperative_statements()?);
        }

        Ok((on_exception, not_on_exception))
    }

    /// Parse GO TO statement.
    /// GO TO paragraph-name [DEPENDING ON identifier]
    pub(super) fn parse_goto_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();

        // Skip GO or GOTO
        if self.check_keyword(Keyword::Go) {
            self.advance();
            if self.check_keyword(Keyword::To) {
                self.advance();
            }
        } else {
            self.advance(); // GOTO
        }

        // Parse targets
        let mut targets = Vec::new();
        while self.check_identifier()
            && !self.check_keyword(Keyword::Depending)
            && !self.check(TokenKind::Period)
        {
            targets.push(self.expect_identifier()?);
        }

        // DEPENDING ON clause
        let depending = if self.check_keyword(Keyword::Depending) {
            self.advance();
            if self.check_keyword(Keyword::On) {
                self.advance();
            }
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::GoTo(GoToStatement {
            targets,
            depending,
            span: start.extend(end),
        }))
    }

    /// Parse EXIT statement.
    /// EXIT [PROGRAM | PERFORM]
    pub(super) fn parse_exit_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // EXIT

        let mut program = false;
        let mut perform_cycle = false;

        if self.check_keyword(Keyword::Program) {
            self.advance();
            program = true;
        } else if self.check_keyword(Keyword::Perform) {
            self.advance();
            perform_cycle = true;
        }
        // Default is just EXIT (exit paragraph)

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Exit(ExitStatement {
            program,
            perform_cycle,
            span: start.extend(end),
        }))
    }

    pub(super) fn parse_exec_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // EXEC

        if self.check_keyword(Keyword::Cics) {
            self.parse_exec_cics(start)
        } else if self.check_keyword(Keyword::Sql) {
            self.parse_exec_sql(start)
        } else {
            // Unknown EXEC type - skip to END-EXEC
            self.advance_to_end_exec();
            let end = self.previous_span();
            Ok(Statement::Continue(ContinueStatement {
                span: start.extend(end),
            }))
        }
    }

    fn parse_exec_cics(&mut self, start: Span) -> Result<Statement> {
        self.advance(); // CICS

        // Get the command (SEND, RECEIVE, RETURN, XCTL, READ, etc.)
        let command = if let TokenKind::Identifier(name) = &self.current().kind {
            let cmd = name.clone();
            self.advance();
            cmd
        } else if let TokenKind::Keyword(kw) = self.current().kind {
            // Some CICS commands are also COBOL keywords (READ, WRITE, RETURN, etc.)
            let cmd = kw.as_str().to_string();
            self.advance();
            cmd
        } else {
            return Err(CobolError::ParseError {
                message: "Expected CICS command".to_string(),
            });
        };

        // Parse options until END-EXEC
        let mut options = Vec::new();
        while !self.check_keyword(Keyword::EndExec) && !self.is_at_end() {
            if let Some(option) = self.parse_cics_option()? {
                options.push(option);
            }
        }

        // Consume END-EXEC
        if self.check_keyword(Keyword::EndExec) {
            self.advance();
        }

        // Optional period
        self.skip_if(TokenKind::Period);

        let end = self.previous_span();

        Ok(Statement::ExecCics(ExecCicsStatement {
            command,
            options,
            span: start.extend(end),
        }))
    }

    fn parse_cics_option(&mut self) -> Result<Option<CicsOption>> {
        // Get option name
        let name = match &self.current().kind {
            TokenKind::Identifier(name) => name.clone(),
            TokenKind::Keyword(kw) => kw.as_str().to_string(),
            _ => {
                // Skip unknown tokens
                self.advance();
                return Ok(None);
            }
        };
        self.advance();

        // Check for value in parentheses
        let value = if self.check(TokenKind::LeftParen) {
            self.advance(); // (
            let expr = if self.check(TokenKind::RightParen) {
                None
            } else {
                Some(self.parse_expression()?)
            };
            if self.check(TokenKind::RightParen) {
                self.advance(); // )
            }
            expr
        } else {
            None
        };

        Ok(Some(CicsOption { name, value }))
    }

    fn parse_exec_sql(&mut self, start: Span) -> Result<Statement> {
        self.advance(); // SQL

        // Collect all tokens until END-EXEC as raw SQL
        let mut sql = String::new();
        while !self.check_keyword(Keyword::EndExec) && !self.is_at_end() {
            match &self.current().kind {
                TokenKind::Identifier(s) => sql.push_str(s),
                TokenKind::Keyword(kw) => sql.push_str(kw.as_str()),
                TokenKind::IntegerLiteral(n) => sql.push_str(&n.to_string()),
                TokenKind::StringLiteral(s) => {
                    sql.push('\'');
                    sql.push_str(s);
                    sql.push('\'');
                }
                TokenKind::LeftParen => sql.push('('),
                TokenKind::RightParen => sql.push(')'),
                TokenKind::Comma => sql.push(','),
                TokenKind::Period => sql.push('.'),
                TokenKind::Colon => sql.push(':'),
                TokenKind::Equals => sql.push('='),
                TokenKind::Plus => sql.push('+'),
                TokenKind::Minus => sql.push('-'),
                TokenKind::Star => sql.push('*'),
                TokenKind::Slash => sql.push('/'),
                _ => {}
            }
            sql.push(' ');
            self.advance();
        }

        // Consume END-EXEC
        if self.check_keyword(Keyword::EndExec) {
            self.advance();
        }

        // Optional period
        self.skip_if(TokenKind::Period);

        let end = self.previous_span();

        Ok(Statement::ExecSql(ExecSqlStatement {
            sql: sql.trim().to_string(),
            span: start.extend(end),
        }))
    }

    fn advance_to_end_exec(&mut self) {
        while !self.check_keyword(Keyword::EndExec) && !self.is_at_end() {
            self.advance();
        }
        if self.check_keyword(Keyword::EndExec) {
            self.advance();
        }
        self.skip_if(TokenKind::Period);
    }

    // ========================================================================
    // STUB PARSERS FOR FILE I/O AND OTHER STATEMENTS
    // ========================================================================

    /// Parse SET statement (sets condition names, indexes, etc.)
    pub(super) fn parse_set_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // SET

        // SET ADDRESS OF target TO source
        if self.check_keyword(Keyword::Address) {
            self.advance(); // ADDRESS
            self.expect_keyword(Keyword::Of)?;
            let target = self.parse_qualified_name()?;
            self.expect_keyword(Keyword::To)?;
            let source = self.parse_qualified_name()?;
            let end = self.previous_span();
            return Ok(Statement::Set(SetStatement {
                mode: SetMode::AddressOf { target, source },
                span: start.extend(end),
            }));
        }

        // Collect target(s)
        let mut targets = vec![self.parse_qualified_name()?];
        while !self.check_keyword(Keyword::To)
            && !self.check_keyword(Keyword::UpBy)
            && !self.check_keyword(Keyword::DownBy)
            && !self.check(TokenKind::Period)
            && !self.is_at_end()
            && !self.is_statement_start()
        {
            targets.push(self.parse_qualified_name()?);
        }

        if self.check_keyword(Keyword::To) {
            self.advance(); // TO
            // SET condition-name TO TRUE / FALSE
            if self.check_keyword(Keyword::True) {
                self.advance();
                let end = self.previous_span();
                let target = targets.into_iter().next().unwrap();
                return Ok(Statement::Set(SetStatement {
                    mode: SetMode::ConditionTo {
                        target,
                        value: true,
                    },
                    span: start.extend(end),
                }));
            }
            if self.check_keyword(Keyword::False) {
                self.advance();
                let end = self.previous_span();
                let target = targets.into_iter().next().unwrap();
                return Ok(Statement::Set(SetStatement {
                    mode: SetMode::ConditionTo {
                        target,
                        value: false,
                    },
                    span: start.extend(end),
                }));
            }
            // SET targets TO value (index assignment)
            let value = self.parse_expression()?;
            let end = self.previous_span();
            return Ok(Statement::Set(SetStatement {
                mode: SetMode::IndexTo { targets, value },
                span: start.extend(end),
            }));
        }

        // SET targets UP BY / DOWN BY value
        let up = if self.check_keyword(Keyword::UpBy) {
            self.advance();
            true
        } else if self.check_keyword(Keyword::DownBy) {
            self.advance();
            false
        } else {
            // Fallback: skip remaining tokens
            while !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.is_statement_start()
                && !self.is_scope_terminator()
            {
                self.advance();
            }
            return Ok(Statement::Continue(ContinueStatement { span: start }));
        };

        let value = self.parse_expression()?;
        let end = self.previous_span();
        Ok(Statement::Set(SetStatement {
            mode: SetMode::IndexUpDown { targets, up, value },
            span: start.extend(end),
        }))
    }

    /// Parse INITIALIZE statement
    pub(super) fn parse_initialize_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // INITIALIZE

        // Collect target variable(s)
        let mut variables = vec![self.parse_qualified_name()?];
        while !self.check_keyword(Keyword::Replacing)
            && !self.check(TokenKind::Period)
            && !self.is_at_end()
            && !self.is_statement_start()
            && !self.is_scope_terminator()
        {
            variables.push(self.parse_qualified_name()?);
        }

        // Optional REPLACING clause(s)
        let mut replacing = Vec::new();
        if self.check_keyword(Keyword::Replacing) {
            self.advance(); // REPLACING
            while !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.is_statement_start()
                && !self.is_scope_terminator()
            {
                // Parse category: ALPHABETIC | ALPHANUMERIC | NUMERIC | ALPHANUMERIC-EDITED | NUMERIC-EDITED
                let category = if self.check_identifier_value("ALPHABETIC") {
                    self.advance();
                    InitializeCategory::Alphabetic
                } else if self.check_identifier_value("ALPHANUMERIC-EDITED") {
                    self.advance();
                    InitializeCategory::AlphanumericEdited
                } else if self.check_identifier_value("ALPHANUMERIC") {
                    self.advance();
                    InitializeCategory::Alphanumeric
                } else if self.check_identifier_value("NUMERIC-EDITED") {
                    self.advance();
                    InitializeCategory::NumericEdited
                } else if self.check_keyword(Keyword::Numeric) {
                    self.advance();
                    InitializeCategory::Numeric
                } else {
                    break;
                };

                // Optional DATA keyword
                if self.check_identifier_value("DATA") {
                    self.advance();
                }

                self.expect_keyword(Keyword::By)?;
                let value = self.parse_expression()?;
                replacing.push(InitializeReplacing { category, value });
            }
        }

        let end = self.previous_span();
        Ok(Statement::Initialize(InitializeStatement {
            variables,
            replacing,
            span: start.extend(end),
        }))
    }

    /// Parse ACCEPT statement
    pub(super) fn parse_accept_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // ACCEPT

        let target = self.parse_qualified_name()?;

        // Optional FROM clause
        let from = if self.check_keyword(Keyword::From) {
            self.advance(); // FROM
            if self.check_keyword(Keyword::Date) {
                self.advance();
                Some(AcceptFrom::Date)
            } else if self.check_keyword(Keyword::Day) {
                self.advance();
                Some(AcceptFrom::Day)
            } else if self.check_keyword(Keyword::DayOfWeek) {
                self.advance();
                Some(AcceptFrom::DayOfWeek)
            } else if self.check_keyword(Keyword::Time) {
                self.advance();
                Some(AcceptFrom::Time)
            } else if self.check_identifier_value("CONSOLE") {
                self.advance();
                Some(AcceptFrom::Console)
            } else {
                // Device name
                let name = self.expect_identifier()?;
                Some(AcceptFrom::Device(name))
            }
        } else if self.check_keyword(Keyword::Date) {
            // ACCEPT target DATE (without FROM)
            self.advance();
            Some(AcceptFrom::Date)
        } else if self.check_keyword(Keyword::Day) {
            self.advance();
            Some(AcceptFrom::Day)
        } else if self.check_keyword(Keyword::DayOfWeek) {
            self.advance();
            Some(AcceptFrom::DayOfWeek)
        } else if self.check_keyword(Keyword::Time) {
            self.advance();
            Some(AcceptFrom::Time)
        } else {
            None // ACCEPT from console (default)
        };

        let end = self.previous_span();
        Ok(Statement::Accept(AcceptStatement {
            target,
            from,
            span: start.extend(end),
        }))
    }

    /// Parse CANCEL statement
    pub(super) fn parse_cancel_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // CANCEL

        // Collect program names (identifiers or literals)
        let mut programs = Vec::new();
        while !self.check(TokenKind::Period)
            && !self.is_at_end()
            && !self.is_statement_start()
            && !self.is_scope_terminator()
        {
            programs.push(self.parse_expression()?);
        }

        let end = self.previous_span();
        Ok(Statement::Cancel(CancelStatement {
            programs,
            span: start.extend(end),
        }))
    }

    /// Parse SORT statement
    pub(super) fn parse_sort_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // SORT

        let file = self.expect_identifier()?;

        // Parse sort keys: ON ASCENDING/DESCENDING KEY field...
        let mut keys = Vec::new();
        while self.check_keyword(Keyword::On)
            || self.check_keyword(Keyword::Ascending)
            || self.check_keyword(Keyword::Descending)
        {
            if self.check_keyword(Keyword::On) {
                self.advance();
            }
            let ascending = if self.check_keyword(Keyword::Ascending) {
                self.advance();
                true
            } else if self.check_keyword(Keyword::Descending) {
                self.advance();
                false
            } else {
                true
            };
            if self.check_keyword(Keyword::Key) {
                self.advance();
            }
            // One or more key fields
            while !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.check_keyword(Keyword::On)
                && !self.check_keyword(Keyword::Ascending)
                && !self.check_keyword(Keyword::Descending)
                && !self.check_keyword(Keyword::Using)
                && !self.check_keyword(Keyword::Giving)
                && !self.check_identifier_value("INPUT")
                && !self.check_identifier_value("OUTPUT")
                && !self.check_identifier_value("DUPLICATES")
                && !self.check_identifier_value("COLLATING")
                && !self.check_identifier_value("WITH")
                && !self.is_statement_start()
            {
                let field = self.parse_qualified_name()?;
                keys.push(SortKey { field, ascending });
            }
        }

        // Skip optional WITH DUPLICATES IN ORDER
        if self.check_identifier_value("WITH") || self.check_identifier_value("DUPLICATES") {
            while !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.check_keyword(Keyword::Using)
                && !self.check_keyword(Keyword::Giving)
                && !self.check_identifier_value("INPUT")
                && !self.check_identifier_value("OUTPUT")
                && !self.is_statement_start()
            {
                self.advance();
            }
        }

        // Skip optional COLLATING SEQUENCE clause
        if self.check_identifier_value("COLLATING") {
            while !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.check_keyword(Keyword::Using)
                && !self.check_keyword(Keyword::Giving)
                && !self.check_identifier_value("INPUT")
                && !self.check_identifier_value("OUTPUT")
                && !self.is_statement_start()
            {
                self.advance();
            }
        }

        let mut using = Vec::new();
        let mut giving = Vec::new();
        let mut input_procedure = None;
        let mut output_procedure = None;

        // USING / GIVING / INPUT PROCEDURE / OUTPUT PROCEDURE
        loop {
            if self.check_keyword(Keyword::Using) {
                self.advance();
                while !self.check(TokenKind::Period)
                    && !self.is_at_end()
                    && !self.check_keyword(Keyword::Giving)
                    && !self.check_identifier_value("INPUT")
                    && !self.check_identifier_value("OUTPUT")
                    && !self.is_statement_start()
                {
                    using.push(self.expect_identifier()?);
                }
            } else if self.check_keyword(Keyword::Giving) {
                self.advance();
                while !self.check(TokenKind::Period)
                    && !self.is_at_end()
                    && !self.check_identifier_value("OUTPUT")
                    && !self.is_statement_start()
                {
                    giving.push(self.expect_identifier()?);
                }
            } else if self.check_identifier_value("INPUT") {
                self.advance(); // INPUT
                if self.check_keyword(Keyword::Procedure) {
                    self.advance();
                }
                if self.check_keyword(Keyword::Is) {
                    self.advance();
                }
                input_procedure = Some(self.expect_identifier()?);
                // Optional THRU/THROUGH
                if self.check_keyword(Keyword::Thru) || self.check_keyword(Keyword::Through) {
                    self.advance();
                    let _end_proc = self.expect_identifier()?;
                }
            } else if self.check_identifier_value("OUTPUT") {
                self.advance(); // OUTPUT
                if self.check_keyword(Keyword::Procedure) {
                    self.advance();
                }
                if self.check_keyword(Keyword::Is) {
                    self.advance();
                }
                output_procedure = Some(self.expect_identifier()?);
                if self.check_keyword(Keyword::Thru) || self.check_keyword(Keyword::Through) {
                    self.advance();
                    let _end_proc = self.expect_identifier()?;
                }
            } else {
                break;
            }
        }

        let end = self.previous_span();
        Ok(Statement::Sort(SortStatement {
            file,
            keys,
            input_procedure,
            output_procedure,
            using,
            giving,
            span: start.extend(end),
        }))
    }

    /// Parse MERGE statement
    pub(super) fn parse_merge_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // MERGE

        let file = self.expect_identifier()?;

        // Parse keys (same as SORT)
        let mut keys = Vec::new();
        while self.check_keyword(Keyword::On)
            || self.check_keyword(Keyword::Ascending)
            || self.check_keyword(Keyword::Descending)
        {
            if self.check_keyword(Keyword::On) {
                self.advance();
            }
            let ascending = if self.check_keyword(Keyword::Ascending) {
                self.advance();
                true
            } else if self.check_keyword(Keyword::Descending) {
                self.advance();
                false
            } else {
                true
            };
            if self.check_keyword(Keyword::Key) {
                self.advance();
            }
            while !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.check_keyword(Keyword::On)
                && !self.check_keyword(Keyword::Ascending)
                && !self.check_keyword(Keyword::Descending)
                && !self.check_keyword(Keyword::Using)
                && !self.check_keyword(Keyword::Giving)
                && !self.check_identifier_value("OUTPUT")
                && !self.is_statement_start()
            {
                let field = self.parse_qualified_name()?;
                keys.push(SortKey { field, ascending });
            }
        }

        // Skip optional COLLATING SEQUENCE
        if self.check_identifier_value("COLLATING") {
            while !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.check_keyword(Keyword::Using)
                && !self.check_keyword(Keyword::Giving)
                && !self.check_identifier_value("OUTPUT")
                && !self.is_statement_start()
            {
                self.advance();
            }
        }

        let mut using = Vec::new();
        let mut giving = Vec::new();
        let mut output_procedure = None;

        loop {
            if self.check_keyword(Keyword::Using) {
                self.advance();
                while !self.check(TokenKind::Period)
                    && !self.is_at_end()
                    && !self.check_keyword(Keyword::Giving)
                    && !self.check_identifier_value("OUTPUT")
                    && !self.is_statement_start()
                {
                    using.push(self.expect_identifier()?);
                }
            } else if self.check_keyword(Keyword::Giving) {
                self.advance();
                while !self.check(TokenKind::Period)
                    && !self.is_at_end()
                    && !self.check_identifier_value("OUTPUT")
                    && !self.is_statement_start()
                {
                    giving.push(self.expect_identifier()?);
                }
            } else if self.check_identifier_value("OUTPUT") {
                self.advance();
                if self.check_keyword(Keyword::Procedure) {
                    self.advance();
                }
                if self.check_keyword(Keyword::Is) {
                    self.advance();
                }
                output_procedure = Some(self.expect_identifier()?);
                if self.check_keyword(Keyword::Thru) || self.check_keyword(Keyword::Through) {
                    self.advance();
                    let _end_proc = self.expect_identifier()?;
                }
            } else {
                break;
            }
        }

        let end = self.previous_span();
        Ok(Statement::Merge(MergeStatement {
            file,
            keys,
            using,
            giving,
            output_procedure,
            span: start.extend(end),
        }))
    }

    /// Parse RELEASE statement
    pub(super) fn parse_release_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // RELEASE

        let record = self.parse_qualified_name()?;

        let from = if self.check_keyword(Keyword::From) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        let end = self.previous_span();
        Ok(Statement::Release(ReleaseStatement {
            record,
            from,
            span: start.extend(end),
        }))
    }

    /// Parse RETURN statement (file I/O, not procedure return)
    pub(super) fn parse_return_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // RETURN

        let file = self.expect_identifier()?;

        // Optional RECORD
        if self.check_keyword(Keyword::Record) {
            self.advance();
        }

        // Optional INTO target
        let into = if self.check_keyword(Keyword::Into) {
            self.advance();
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        // AT END / NOT AT END handlers
        let mut at_end = None;
        let mut not_at_end = None;

        loop {
            if self.check_keyword(Keyword::AtEnd) {
                self.advance();
                at_end = Some(self.parse_io_handler_statements(Keyword::EndReturn)?);
            } else if self.check_keyword(Keyword::Not) && self.peek_keyword(Keyword::AtEnd) {
                self.advance();
                self.advance();
                not_at_end = Some(self.parse_io_handler_statements(Keyword::EndReturn)?);
            } else {
                break;
            }
        }

        if self.check_keyword(Keyword::EndReturn) {
            self.advance();
        }

        let end = self.previous_span();
        Ok(Statement::ReturnStmt(ReturnStatement {
            file,
            into,
            at_end,
            not_at_end,
            span: start.extend(end),
        }))
    }

    /// Parse OPEN statement
    pub(super) fn parse_open_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // OPEN

        let mut files = Vec::new();
        while !self.check(TokenKind::Period)
            && !self.is_at_end()
            && !self.is_statement_start()
            && !self.is_scope_terminator()
        {
            let mode = if self.check_keyword(Keyword::Input) {
                self.advance();
                OpenMode::Input
            } else if self.check_keyword(Keyword::Output) {
                self.advance();
                OpenMode::Output
            } else if self.check_keyword(Keyword::Io) {
                self.advance();
                OpenMode::InputOutput
            } else if self.check_keyword(Keyword::Extend) {
                self.advance();
                OpenMode::Extend
            } else {
                break;
            };

            // One or more file names after the mode
            while !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.check_keyword(Keyword::Input)
                && !self.check_keyword(Keyword::Output)
                && !self.check_keyword(Keyword::Io)
                && !self.check_keyword(Keyword::Extend)
                && !self.is_statement_start()
                && !self.is_scope_terminator()
            {
                let name = self.expect_identifier()?;
                files.push(OpenFile { name, mode });
            }
        }

        let end = self.previous_span();
        Ok(Statement::Open(OpenStatement {
            files,
            span: start.extend(end),
        }))
    }

    /// Parse CLOSE statement
    pub(super) fn parse_close_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // CLOSE

        let mut files = Vec::new();
        while !self.check(TokenKind::Period)
            && !self.is_at_end()
            && !self.is_statement_start()
            && !self.is_scope_terminator()
        {
            let name = self.expect_identifier()?;
            files.push(name);
        }

        let end = self.previous_span();
        Ok(Statement::Close(CloseStatement {
            files,
            span: start.extend(end),
        }))
    }

    /// Parse READ statement
    pub(super) fn parse_read_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // READ

        let file = self.expect_identifier()?;

        // Optional NEXT RECORD
        let next = if self.check_keyword(Keyword::Next) {
            self.advance();
            if self.check_keyword(Keyword::Record) {
                self.advance();
            }
            true
        } else {
            if self.check_keyword(Keyword::Record) {
                self.advance();
            }
            false
        };

        // Optional INTO target
        let into = if self.check_keyword(Keyword::Into) {
            self.advance();
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        // Optional KEY IS clause (skip)
        if self.check_keyword(Keyword::Key) {
            self.advance();
            if self.check_keyword(Keyword::Is) {
                self.advance();
            }
            let _key = self.parse_qualified_name()?;
        }

        // AT END / NOT AT END / INVALID KEY / NOT INVALID KEY handlers
        let mut at_end = None;
        let mut not_at_end = None;
        let mut invalid_key = None;
        let mut not_invalid_key = None;

        loop {
            if self.check_keyword(Keyword::AtEnd) {
                self.advance();
                at_end = Some(self.parse_io_handler_statements(Keyword::EndRead)?);
            } else if self.check_keyword(Keyword::Not) && self.peek_keyword(Keyword::AtEnd) {
                self.advance(); // NOT
                self.advance(); // AT END
                not_at_end = Some(self.parse_io_handler_statements(Keyword::EndRead)?);
            } else if self.check_keyword(Keyword::InvalidKey) {
                self.advance(); // INVALID
                if self.check_keyword(Keyword::Key) {
                    self.advance();
                }
                invalid_key = Some(self.parse_io_handler_statements(Keyword::EndRead)?);
            } else if self.check_keyword(Keyword::Not) && self.peek_keyword(Keyword::InvalidKey) {
                self.advance(); // NOT
                self.advance(); // INVALID
                if self.check_keyword(Keyword::Key) {
                    self.advance();
                }
                not_invalid_key = Some(self.parse_io_handler_statements(Keyword::EndRead)?);
            } else {
                break;
            }
        }

        if self.check_keyword(Keyword::EndRead) {
            self.advance();
        }

        let end = self.previous_span();
        Ok(Statement::Read(ReadStatement {
            file,
            into,
            next,
            at_end,
            not_at_end,
            invalid_key,
            not_invalid_key,
            span: start.extend(end),
        }))
    }

    /// Parse WRITE statement
    pub(super) fn parse_write_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // WRITE

        let record = self.parse_qualified_name()?;

        // Optional FROM clause
        let from = if self.check_keyword(Keyword::From) {
            self.advance();
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        // Optional ADVANCING clause
        let advancing = if self.check_keyword(Keyword::Before) || self.check_keyword(Keyword::After) {
            let before = self.check_keyword(Keyword::Before);
            self.advance(); // BEFORE or AFTER
            if self.check_keyword(Keyword::Advancing) {
                self.advance();
            }
            if self.check_identifier_value("PAGE") {
                self.advance();
                Some(WriteAdvancing::Page { before })
            } else {
                let count = self.parse_expression()?;
                // Optional LINES/LINE keyword
                if self.check_identifier_value("LINE") || self.check_identifier_value("LINES") {
                    self.advance();
                }
                Some(WriteAdvancing::Lines { count, before })
            }
        } else if self.check_keyword(Keyword::Advancing) {
            self.advance();
            if self.check_identifier_value("PAGE") {
                self.advance();
                Some(WriteAdvancing::Page { before: false })
            } else {
                let count = self.parse_expression()?;
                if self.check_identifier_value("LINE") || self.check_identifier_value("LINES") {
                    self.advance();
                }
                Some(WriteAdvancing::Lines { count, before: false })
            }
        } else {
            None
        };

        // INVALID KEY / NOT INVALID KEY / AT EOP / NOT AT EOP handlers
        let mut invalid_key = None;
        let mut not_invalid_key = None;
        let at_eop = None;
        let not_at_eop = None;

        loop {
            if self.check_keyword(Keyword::InvalidKey) {
                self.advance();
                if self.check_keyword(Keyword::Key) {
                    self.advance();
                }
                invalid_key = Some(self.parse_io_handler_statements(Keyword::EndWrite)?);
            } else if self.check_keyword(Keyword::Not) && self.peek_keyword(Keyword::InvalidKey) {
                self.advance(); // NOT
                self.advance(); // INVALID
                if self.check_keyword(Keyword::Key) {
                    self.advance();
                }
                not_invalid_key = Some(self.parse_io_handler_statements(Keyword::EndWrite)?);
            } else {
                break;
            }
        }

        if self.check_keyword(Keyword::EndWrite) {
            self.advance();
        }

        let end = self.previous_span();
        Ok(Statement::Write(WriteStatement {
            record,
            from,
            advancing,
            invalid_key,
            not_invalid_key,
            at_eop,
            not_at_eop,
            span: start.extend(end),
        }))
    }

    /// Parse REWRITE statement
    pub(super) fn parse_rewrite_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // REWRITE

        let record = self.parse_qualified_name()?;

        let from = if self.check_keyword(Keyword::From) {
            self.advance();
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        let mut invalid_key = None;
        let mut not_invalid_key = None;

        loop {
            if self.check_keyword(Keyword::InvalidKey) {
                self.advance();
                if self.check_keyword(Keyword::Key) {
                    self.advance();
                }
                invalid_key = Some(self.parse_io_handler_statements(Keyword::EndRewrite)?);
            } else if self.check_keyword(Keyword::Not) && self.peek_keyword(Keyword::InvalidKey) {
                self.advance();
                self.advance();
                if self.check_keyword(Keyword::Key) {
                    self.advance();
                }
                not_invalid_key = Some(self.parse_io_handler_statements(Keyword::EndRewrite)?);
            } else {
                break;
            }
        }

        if self.check_keyword(Keyword::EndRewrite) {
            self.advance();
        }

        let end = self.previous_span();
        // Reuse WriteStatement for REWRITE (same structure)
        Ok(Statement::Write(WriteStatement {
            record,
            from,
            advancing: None,
            invalid_key,
            not_invalid_key,
            at_eop: None,
            not_at_eop: None,
            span: start.extend(end),
        }))
    }

    /// Parse DELETE statement
    pub(super) fn parse_delete_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // DELETE

        let file = self.expect_identifier()?;

        // Optional RECORD
        if self.check_keyword(Keyword::Record) {
            self.advance();
        }

        let mut invalid_key = None;
        let mut not_invalid_key = None;

        loop {
            if self.check_keyword(Keyword::InvalidKey) {
                self.advance();
                if self.check_keyword(Keyword::Key) {
                    self.advance();
                }
                invalid_key = Some(self.parse_io_handler_statements(Keyword::EndDelete)?);
            } else if self.check_keyword(Keyword::Not) && self.peek_keyword(Keyword::InvalidKey) {
                self.advance();
                self.advance();
                if self.check_keyword(Keyword::Key) {
                    self.advance();
                }
                not_invalid_key = Some(self.parse_io_handler_statements(Keyword::EndDelete)?);
            } else {
                break;
            }
        }

        if self.check_keyword(Keyword::EndDelete) {
            self.advance();
        }

        let end = self.previous_span();
        // Reuse Read structure for DELETE (file + invalid key handlers)
        Ok(Statement::Read(ReadStatement {
            file,
            into: None,
            next: false,
            at_end: None,
            not_at_end: None,
            invalid_key,
            not_invalid_key,
            span: start.extend(end),
        }))
    }

    /// Parse START statement
    pub(super) fn parse_start_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // START

        let file = self.expect_identifier()?;

        // Optional KEY IS/= clause (skip details)
        if self.check_keyword(Keyword::Key) {
            self.advance();
            if self.check_keyword(Keyword::Is) {
                self.advance();
            }
            // Skip comparison operator and key name
            while !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.check_keyword(Keyword::InvalidKey)
                && !self.check_keyword(Keyword::EndStart)
                && !(self.check_keyword(Keyword::Not) && self.peek_keyword(Keyword::InvalidKey))
                && !self.is_statement_start()
                && !self.is_scope_terminator()
            {
                self.advance();
            }
        }

        let mut invalid_key = None;
        let mut not_invalid_key = None;

        loop {
            if self.check_keyword(Keyword::InvalidKey) {
                self.advance();
                if self.check_keyword(Keyword::Key) {
                    self.advance();
                }
                invalid_key = Some(self.parse_io_handler_statements(Keyword::EndStart)?);
            } else if self.check_keyword(Keyword::Not) && self.peek_keyword(Keyword::InvalidKey) {
                self.advance();
                self.advance();
                if self.check_keyword(Keyword::Key) {
                    self.advance();
                }
                not_invalid_key = Some(self.parse_io_handler_statements(Keyword::EndStart)?);
            } else {
                break;
            }
        }

        if self.check_keyword(Keyword::EndStart) {
            self.advance();
        }

        let end = self.previous_span();
        Ok(Statement::Read(ReadStatement {
            file,
            into: None,
            next: false,
            at_end: None,
            not_at_end: None,
            invalid_key,
            not_invalid_key,
            span: start.extend(end),
        }))
    }

    /// Parse statements within an I/O handler clause (AT END, INVALID KEY, etc.)
    fn parse_io_handler_statements(&mut self, end_keyword: Keyword) -> Result<Vec<Statement>> {
        let mut stmts = Vec::new();
        while !self.check(TokenKind::Period)
            && !self.is_at_end()
            && !self.check_keyword(Keyword::InvalidKey)
            && !self.check_keyword(Keyword::NotInvalidKey)
            && !self.check_keyword(Keyword::AtEnd)
            && !self.check_keyword(Keyword::NotAtEnd)
            && !self.check_keyword(end_keyword)
            && !(self.check_keyword(Keyword::Not)
                && (self.peek_keyword(Keyword::InvalidKey) || self.peek_keyword(Keyword::AtEnd)))
        {
            match self.parse_statement() {
                Ok(stmt) => stmts.push(stmt),
                Err(e) => {
                    self.errors.push(e);
                    self.advance_to_next_sentence();
                    break;
                }
            }
        }
        Ok(stmts)
    }

    /// Parse SEARCH statement
    pub(super) fn parse_search_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // SEARCH

        // SEARCH ALL (binary search)?
        let all = if self.check_keyword(Keyword::All) {
            self.advance();
            true
        } else {
            false
        };

        // Table name
        let table = self.parse_qualified_name()?;

        // Optional VARYING clause
        let varying = if self.check_keyword(Keyword::Varying) {
            self.advance();
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        // Optional AT END handler
        let at_end = if self.check_keyword(Keyword::AtEnd) {
            self.advance();
            let mut stmts = Vec::new();
            while !self.check_keyword(Keyword::When)
                && !self.check_keyword(Keyword::EndSearch)
                && !self.check(TokenKind::Period)
                && !self.is_at_end()
            {
                match self.parse_statement() {
                    Ok(stmt) => stmts.push(stmt),
                    Err(e) => {
                        self.errors.push(e);
                        break;
                    }
                }
            }
            Some(stmts)
        } else {
            None
        };

        // WHEN clauses
        let mut when_clauses = Vec::new();
        while self.check_keyword(Keyword::When) {
            let when_start = self.current_span();
            self.advance(); // WHEN

            let condition = self.parse_condition()?;

            // Parse statements until next WHEN, END-SEARCH, or period
            let mut stmts = Vec::new();
            while !self.check_keyword(Keyword::When)
                && !self.check_keyword(Keyword::EndSearch)
                && !self.check(TokenKind::Period)
                && !self.is_at_end()
            {
                match self.parse_statement() {
                    Ok(stmt) => stmts.push(stmt),
                    Err(e) => {
                        self.errors.push(e);
                        break;
                    }
                }
            }

            let when_end = self.previous_span();
            when_clauses.push(SearchWhen {
                condition,
                statements: stmts,
                span: when_start.extend(when_end),
            });
        }

        if self.check_keyword(Keyword::EndSearch) {
            self.advance();
        }

        let end = self.previous_span();
        Ok(Statement::Search(SearchStatement {
            table,
            varying,
            all,
            at_end,
            when_clauses,
            span: start.extend(end),
        }))
    }

    /// Parse INSPECT statement
    pub(super) fn parse_inspect_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // INSPECT

        let target = self.parse_qualified_name()?;

        let mut tallying = None;
        let mut replacing = None;
        let mut converting = None;

        // INSPECT target TALLYING ...
        if self.check_keyword(Keyword::Tallying) {
            self.advance(); // TALLYING
            let counter = self.parse_qualified_name()?;
            self.expect_keyword(Keyword::For)?;

            let mut for_clauses = Vec::new();
            loop {
                let mode = self.parse_inspect_mode()?;
                let pattern = if mode != InspectMode::Characters {
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                let delimiters = self.parse_inspect_delimiters()?;
                for_clauses.push(InspectFor {
                    mode,
                    pattern,
                    delimiters,
                });
                // Continue if next token is another mode keyword
                if !self.is_inspect_mode() && !self.check_keyword(Keyword::For) {
                    break;
                }
                if self.check_keyword(Keyword::For) {
                    self.advance();
                }
            }

            tallying = Some(InspectTallying {
                counter,
                for_clauses,
            });
        }

        // INSPECT target REPLACING ...
        if self.check_keyword(Keyword::Replacing) {
            self.advance(); // REPLACING

            let mut rules = Vec::new();
            loop {
                let mode = self.parse_inspect_mode()?;
                let pattern = if mode != InspectMode::Characters {
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                self.expect_keyword(Keyword::By)?;
                let by = self.parse_expression()?;
                let delimiters = self.parse_inspect_delimiters()?;
                rules.push(InspectReplacingRule {
                    mode,
                    pattern,
                    by,
                    delimiters,
                });
                if !self.is_inspect_mode() {
                    break;
                }
            }

            replacing = Some(InspectReplacing { rules });
        }

        // INSPECT target CONVERTING ...
        if self.check_keyword(Keyword::Converting) {
            self.advance(); // CONVERTING
            let from = self.parse_expression()?;
            self.expect_keyword(Keyword::To)?;
            let to = self.parse_expression()?;
            let delimiters = self.parse_inspect_delimiters()?;
            converting = Some(InspectConverting {
                from,
                to,
                delimiters,
            });
        }

        let end = self.previous_span();
        Ok(Statement::Inspect(InspectStatement {
            target,
            tallying,
            replacing,
            converting,
            span: start.extend(end),
        }))
    }

    /// Check if current token is an INSPECT mode keyword.
    fn is_inspect_mode(&self) -> bool {
        self.check_keyword(Keyword::Characters)
            || self.check_keyword(Keyword::All)
            || self.check_keyword(Keyword::Leading)
            || self.check_identifier_value("FIRST")
    }

    /// Parse INSPECT mode: CHARACTERS | ALL | LEADING | FIRST.
    fn parse_inspect_mode(&mut self) -> Result<InspectMode> {
        if self.check_keyword(Keyword::Characters) {
            self.advance();
            Ok(InspectMode::Characters)
        } else if self.check_keyword(Keyword::All) {
            self.advance();
            Ok(InspectMode::All)
        } else if self.check_keyword(Keyword::Leading) {
            self.advance();
            Ok(InspectMode::Leading)
        } else if self.check_identifier_value("FIRST") {
            self.advance();
            Ok(InspectMode::First)
        } else {
            Err(CobolError::ParseError {
                message: format!("Expected CHARACTERS, ALL, LEADING, or FIRST, found {:?}", self.current().kind),
            })
        }
    }

    /// Parse BEFORE/AFTER INITIAL delimiters for INSPECT.
    fn parse_inspect_delimiters(&mut self) -> Result<Vec<InspectDelimiter>> {
        let mut delimiters = Vec::new();
        while self.check_keyword(Keyword::Before) || self.check_keyword(Keyword::After) {
            let before = self.check_keyword(Keyword::Before);
            self.advance(); // BEFORE or AFTER
            // Optional INITIAL keyword
            let initial = if self.check_keyword(Keyword::Initial) {
                self.advance();
                true
            } else {
                false
            };
            let value = self.parse_expression()?;
            delimiters.push(InspectDelimiter { before, initial, value });
        }
        Ok(delimiters)
    }

    // ========================================================================
    // JSON GENERATE / JSON PARSE
    // ========================================================================

    /// Parse a JSON GENERATE or JSON PARSE statement.
    ///
    /// The dispatch macro sends us here when the current token is `JSON`.
    pub(super) fn parse_json_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // JSON

        if self.check_keyword(Keyword::Generate) {
            self.parse_json_generate_statement(start)
        } else if self.check_keyword(Keyword::Parse) {
            self.parse_json_parse_statement(start)
        } else {
            Err(CobolError::ParseError {
                message: format!(
                    "Expected GENERATE or PARSE after JSON, found {:?}",
                    self.current().kind
                ),
            })
        }
    }

    /// Parse `JSON GENERATE receiver FROM source [COUNT IN count]
    ///  [NAME ...] [SUPPRESS ...] [ON EXCEPTION ...] [NOT ON EXCEPTION ...] [END-JSON]`
    fn parse_json_generate_statement(&mut self, start: Span) -> Result<Statement> {
        self.advance(); // GENERATE

        let receiver = self.parse_qualified_name()?;

        self.expect_keyword(Keyword::From)?;
        let source = self.parse_qualified_name()?;

        // Optional COUNT IN
        let count_in = if self.check_keyword(Keyword::Count) {
            self.advance(); // COUNT
            if self.check_keyword(Keyword::Into) || self.check_identifier_value("IN") {
                self.advance(); // IN
            }
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        // Optional NAME phrases
        let name_phrases = self.parse_json_name_phrases()?;

        // Optional SUPPRESS phrases
        let suppress_phrases = self.parse_json_suppress_phrases()?;

        // ON EXCEPTION / NOT ON EXCEPTION
        let (on_exception, not_on_exception) = self.parse_json_exception_handlers()?;

        // Optional END-JSON
        let end_json = if self.check_keyword(Keyword::EndJson) {
            self.advance();
            true
        } else {
            false
        };

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::JsonGenerate(JsonGenerateStatement {
            receiver,
            source,
            count_in,
            name_phrases,
            suppress_phrases,
            on_exception,
            not_on_exception,
            end_json,
            span: start.extend(end),
        }))
    }

    /// Parse `JSON PARSE source INTO target [WITH DETAIL]
    ///  [NAME ...] [SUPPRESS ...] [ON EXCEPTION ...] [NOT ON EXCEPTION ...] [END-JSON]`
    fn parse_json_parse_statement(&mut self, start: Span) -> Result<Statement> {
        self.advance(); // PARSE

        let source = self.parse_qualified_name()?;

        self.expect_keyword(Keyword::Into)?;
        let target = self.parse_qualified_name()?;

        // Optional WITH DETAIL
        let with_detail = if self.check_keyword(Keyword::With) {
            self.advance(); // WITH
            if self.check_keyword(Keyword::Detail) {
                self.advance(); // DETAIL
                true
            } else {
                false
            }
        } else {
            false
        };

        // Optional NAME phrases
        let name_phrases = self.parse_json_name_phrases()?;

        // Optional SUPPRESS phrases
        let suppress_phrases = self.parse_json_suppress_phrases()?;

        // ON EXCEPTION / NOT ON EXCEPTION
        let (on_exception, not_on_exception) = self.parse_json_exception_handlers()?;

        // Optional END-JSON
        let end_json = if self.check_keyword(Keyword::EndJson) {
            self.advance();
            true
        } else {
            false
        };

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::JsonParse(JsonParseStatement {
            source,
            target,
            with_detail,
            name_phrases,
            suppress_phrases,
            on_exception,
            not_on_exception,
            end_json,
            span: start.extend(end),
        }))
    }

    /// Parse NAME phrases: `NAME [OF] data-name IS 'literal' ...`
    fn parse_json_name_phrases(&mut self) -> Result<Vec<JsonNamePhrase>> {
        let mut phrases = Vec::new();
        while self.check_keyword(Keyword::Name) {
            self.advance(); // NAME
            // Optional OF keyword
            if self.check_keyword(Keyword::Of) {
                self.advance();
            }
            let data_name = self.parse_qualified_name()?;
            self.expect_keyword(Keyword::Is)?;
            // Expect a string literal for the JSON name
            let json_name = match &self.current().kind {
                TokenKind::StringLiteral(s) => {
                    let name = s.clone();
                    self.advance();
                    name
                }
                _ => {
                    return Err(CobolError::ParseError {
                        message: format!(
                            "Expected string literal after IS in NAME phrase, found {:?}",
                            self.current().kind
                        ),
                    });
                }
            };
            phrases.push(JsonNamePhrase { data_name, json_name });
        }
        Ok(phrases)
    }

    /// Parse SUPPRESS phrases: `SUPPRESS data-name ...`
    fn parse_json_suppress_phrases(&mut self) -> Result<Vec<QualifiedName>> {
        let mut names = Vec::new();
        if self.check_keyword(Keyword::Suppress) {
            self.advance(); // SUPPRESS
            // Parse one or more data names to suppress
            while !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.check_keyword(Keyword::OnException)
                && !self.check_keyword(Keyword::Not)
                && !self.check_keyword(Keyword::EndJson)
                && !self.check_keyword(Keyword::Name)
                && !self.is_statement_start()
            {
                names.push(self.parse_qualified_name()?);
            }
        }
        Ok(names)
    }

    /// Parse ON EXCEPTION / NOT ON EXCEPTION handlers for JSON statements.
    fn parse_json_exception_handlers(
        &mut self,
    ) -> Result<(Option<Vec<Statement>>, Option<Vec<Statement>>)> {
        let mut on_exception = None;
        let mut not_on_exception = None;

        // ON EXCEPTION
        if self.check_keyword(Keyword::OnException) {
            self.advance(); // EXCEPTION
            let mut stmts = Vec::new();
            while !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.check_keyword(Keyword::Not)
                && !self.check_keyword(Keyword::EndJson)
            {
                stmts.push(self.parse_statement()?);
            }
            on_exception = Some(stmts);
        }

        // NOT ON EXCEPTION
        if self.check_keyword(Keyword::Not) {
            self.advance(); // NOT
            if self.check_keyword(Keyword::OnException) {
                self.advance(); // EXCEPTION
                let mut stmts = Vec::new();
                while !self.check(TokenKind::Period)
                    && !self.is_at_end()
                    && !self.check_keyword(Keyword::EndJson)
                {
                    stmts.push(self.parse_statement()?);
                }
                not_on_exception = Some(stmts);
            }
        }

        Ok((on_exception, not_on_exception))
    }

    // ========================================================================
    // XML GENERATE / XML PARSE
    // ========================================================================

    /// Parse an XML GENERATE or XML PARSE statement.
    pub(super) fn parse_xml_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // XML

        if self.check_keyword(Keyword::Generate) {
            self.parse_xml_generate_statement(start)
        } else if self.check_keyword(Keyword::Parse) {
            self.parse_xml_parse_statement(start)
        } else {
            Err(CobolError::ParseError {
                message: format!(
                    "Expected GENERATE or PARSE after XML, found {:?}",
                    self.current().kind
                ),
            })
        }
    }

    /// Parse `XML GENERATE receiver FROM source [COUNT IN count]
    ///  [NAME ...] [TYPE ...] [NAMESPACE ...] [ENCODING ...] [ON EXCEPTION ...] [END-XML]`
    fn parse_xml_generate_statement(&mut self, start: Span) -> Result<Statement> {
        self.advance(); // GENERATE

        let receiver = self.parse_qualified_name()?;

        self.expect_keyword(Keyword::From)?;
        let source = self.parse_qualified_name()?;

        // Optional COUNT IN
        let count_in = if self.check_keyword(Keyword::Count) {
            self.advance(); // COUNT
            if self.check_keyword(Keyword::Into) || self.check_identifier_value("IN") {
                self.advance(); // IN
            }
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        // Optional NAME phrases
        let name_phrases = self.parse_xml_name_phrases()?;

        // Optional TYPE phrases
        let type_phrases = self.parse_xml_type_phrases()?;

        // Optional NAMESPACE
        let namespace = if self.check_keyword(Keyword::Namespace) {
            self.advance(); // NAMESPACE
            let uri = self.parse_expression()?;
            let prefix = if self.check_identifier_value("PREFIX") {
                self.advance(); // PREFIX
                Some(self.parse_expression()?)
            } else {
                None
            };
            Some(XmlNamespace { uri, prefix })
        } else {
            None
        };

        // Optional ENCODING
        let encoding = if self.check_keyword(Keyword::Encoding) {
            self.advance(); // ENCODING
            Some(self.parse_expression()?)
        } else {
            None
        };

        // ON EXCEPTION / NOT ON EXCEPTION
        let (on_exception, not_on_exception) = self.parse_xml_exception_handlers()?;

        // Optional END-XML
        let end_xml = if self.check_keyword(Keyword::EndXml) {
            self.advance();
            true
        } else {
            false
        };

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::XmlGenerate(XmlGenerateStatement {
            receiver,
            source,
            count_in,
            name_phrases,
            type_phrases,
            namespace,
            encoding,
            on_exception,
            not_on_exception,
            end_xml,
            span: start.extend(end),
        }))
    }

    /// Parse `XML PARSE source PROCESSING PROCEDURE procedure
    ///  [ENCODING ...] [VALIDATING ...] [ON EXCEPTION ...] [END-XML]`
    fn parse_xml_parse_statement(&mut self, start: Span) -> Result<Statement> {
        self.advance(); // PARSE

        let source = self.parse_qualified_name()?;

        self.expect_keyword(Keyword::Processing)?;
        if self.check_identifier_value("PROCEDURE") {
            self.advance(); // PROCEDURE
        }
        let processing_procedure = self.expect_identifier()?;

        // Optional ENCODING
        let encoding = if self.check_keyword(Keyword::Encoding) {
            self.advance(); // ENCODING
            Some(self.parse_expression()?)
        } else {
            None
        };

        // Optional VALIDATING
        let validating = if self.check_keyword(Keyword::Validating) {
            self.advance(); // VALIDATING
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        // ON EXCEPTION / NOT ON EXCEPTION
        let (on_exception, not_on_exception) = self.parse_xml_exception_handlers()?;

        // Optional END-XML
        let end_xml = if self.check_keyword(Keyword::EndXml) {
            self.advance();
            true
        } else {
            false
        };

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::XmlParse(XmlParseStatement {
            source,
            processing_procedure,
            encoding,
            validating,
            on_exception,
            not_on_exception,
            end_xml,
            span: start.extend(end),
        }))
    }

    /// Parse NAME phrases for XML GENERATE: `NAME [OF] data-name IS 'literal' ...`
    fn parse_xml_name_phrases(&mut self) -> Result<Vec<XmlNamePhrase>> {
        let mut phrases = Vec::new();
        while self.check_keyword(Keyword::Name) {
            self.advance(); // NAME
            if self.check_keyword(Keyword::Of) {
                self.advance();
            }
            let data_name = self.parse_qualified_name()?;
            self.expect_keyword(Keyword::Is)?;
            let xml_name = match &self.current().kind {
                TokenKind::StringLiteral(s) => {
                    let name = s.clone();
                    self.advance();
                    name
                }
                _ => {
                    return Err(CobolError::ParseError {
                        message: format!(
                            "Expected string literal after IS in NAME phrase, found {:?}",
                            self.current().kind
                        ),
                    });
                }
            };
            phrases.push(XmlNamePhrase { data_name, xml_name });
        }
        Ok(phrases)
    }

    /// Parse TYPE phrases for XML GENERATE: `TYPE [OF] data-name IS ATTRIBUTE|ELEMENT|CONTENT`
    fn parse_xml_type_phrases(&mut self) -> Result<Vec<XmlTypePhrase>> {
        let mut phrases = Vec::new();
        while self.check_keyword(Keyword::Type) {
            self.advance(); // TYPE
            if self.check_keyword(Keyword::Of) {
                self.advance();
            }
            let data_name = self.parse_qualified_name()?;
            self.expect_keyword(Keyword::Is)?;
            let xml_type = if self.check_keyword(Keyword::Attribute) {
                self.advance();
                XmlTypeKind::Attribute
            } else if self.check_identifier_value("ELEMENT") {
                self.advance();
                XmlTypeKind::Element
            } else if self.check_identifier_value("CONTENT") {
                self.advance();
                XmlTypeKind::Content
            } else {
                return Err(CobolError::ParseError {
                    message: format!(
                        "Expected ATTRIBUTE, ELEMENT, or CONTENT in TYPE phrase, found {:?}",
                        self.current().kind
                    ),
                });
            };
            phrases.push(XmlTypePhrase { data_name, xml_type });
        }
        Ok(phrases)
    }

    /// Parse ON EXCEPTION / NOT ON EXCEPTION handlers for XML statements.
    fn parse_xml_exception_handlers(
        &mut self,
    ) -> Result<(Option<Vec<Statement>>, Option<Vec<Statement>>)> {
        let mut on_exception = None;
        let mut not_on_exception = None;

        if self.check_keyword(Keyword::OnException) {
            self.advance(); // EXCEPTION
            let mut stmts = Vec::new();
            while !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.check_keyword(Keyword::Not)
                && !self.check_keyword(Keyword::EndXml)
            {
                stmts.push(self.parse_statement()?);
            }
            on_exception = Some(stmts);
        }

        if self.check_keyword(Keyword::Not) {
            self.advance(); // NOT
            if self.check_keyword(Keyword::OnException) {
                self.advance(); // EXCEPTION
                let mut stmts = Vec::new();
                while !self.check(TokenKind::Period)
                    && !self.is_at_end()
                    && !self.check_keyword(Keyword::EndXml)
                {
                    stmts.push(self.parse_statement()?);
                }
                not_on_exception = Some(stmts);
            }
        }

        Ok((on_exception, not_on_exception))
    }

    // ========================================================================
    // ALLOCATE / FREE
    // ========================================================================

    /// Parse `ALLOCATE data-name [CHARACTERS count] [RETURNING pointer] [INITIALIZED]`
    pub(super) fn parse_allocate_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // ALLOCATE

        let data_name = self.parse_qualified_name()?;

        // Optional CHARACTERS count
        let characters = if self.check_keyword(Keyword::Characters) {
            self.advance(); // CHARACTERS
            Some(self.parse_expression()?)
        } else {
            None
        };

        // Optional RETURNING
        let returning = if self.check_keyword(Keyword::Returning) {
            self.advance(); // RETURNING
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        // Optional INITIALIZED
        let initialized = if self.check_keyword(Keyword::Initialized) {
            self.advance();
            true
        } else {
            false
        };

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Allocate(AllocateStatement {
            data_name,
            characters,
            returning,
            initialized,
            span: start.extend(end),
        }))
    }

    /// Parse `FREE pointer-name ...`
    pub(super) fn parse_free_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // FREE

        let mut pointers = Vec::new();
        while !self.check(TokenKind::Period) && !self.is_at_end() && !self.is_statement_start() {
            pointers.push(self.parse_qualified_name()?);
        }

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Free(FreeStatement {
            pointers,
            span: start.extend(end),
        }))
    }

    // ========================================================================
    // ENTRY / ALTER / INVOKE
    // ========================================================================

    /// Parse `ENTRY 'literal' [USING parameters ...]`
    pub(super) fn parse_entry_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // ENTRY

        // Expect a string literal for the entry point name
        let literal = match &self.current().kind {
            TokenKind::StringLiteral(s) => {
                let name = s.clone();
                self.advance();
                name
            }
            _ => {
                return Err(CobolError::ParseError {
                    message: format!(
                        "Expected string literal after ENTRY, found {:?}",
                        self.current().kind
                    ),
                });
            }
        };

        // Optional USING parameters
        let mut using = Vec::new();
        if self.check_keyword(Keyword::Using) {
            self.advance(); // USING
            while !self.check(TokenKind::Period) && !self.is_at_end() && !self.is_statement_start() {
                // Skip BY REFERENCE/CONTENT/VALUE
                if self.check_keyword(Keyword::Reference) || self.check_keyword(Keyword::Content) {
                    self.advance();
                    continue;
                }
                if self.check_keyword(Keyword::By) {
                    self.advance();
                    continue;
                }
                using.push(self.parse_qualified_name()?);
            }
        }

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Entry(EntryStatement {
            literal,
            using,
            span: start.extend(end),
        }))
    }

    /// Parse `ALTER paragraph-1 TO [PROCEED TO] paragraph-2`
    pub(super) fn parse_alter_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // ALTER

        let source = self.expect_identifier()?;

        self.expect_keyword(Keyword::To)?;

        // Optional PROCEED TO
        if self.check_keyword(Keyword::Proceed) {
            self.advance(); // PROCEED
            self.expect_keyword(Keyword::To)?;
        }

        let target = self.expect_identifier()?;

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Alter(AlterStatement {
            source,
            target,
            span: start.extend(end),
        }))
    }

    /// Parse `INVOKE object method [USING args ...] [RETURNING result]`
    pub(super) fn parse_invoke_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // INVOKE

        let object = self.parse_qualified_name()?;

        // Method name — string literal or identifier
        let method = match &self.current().kind {
            TokenKind::StringLiteral(s) => {
                let m = s.clone();
                self.advance();
                m
            }
            TokenKind::Identifier(s) => {
                let m = s.clone();
                self.advance();
                m
            }
            _ => {
                return Err(CobolError::ParseError {
                    message: format!(
                        "Expected method name after INVOKE object, found {:?}",
                        self.current().kind
                    ),
                });
            }
        };

        // Optional USING
        let mut using = Vec::new();
        if self.check_keyword(Keyword::Using) {
            self.advance(); // USING
            while !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.is_statement_start()
                && !self.check_keyword(Keyword::Returning)
            {
                using.push(self.parse_expression()?);
            }
        }

        // Optional RETURNING
        let returning = if self.check_keyword(Keyword::Returning) {
            self.advance(); // RETURNING
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Invoke(InvokeStatement {
            object,
            method,
            using,
            returning,
            span: start.extend(end),
        }))
    }
}
