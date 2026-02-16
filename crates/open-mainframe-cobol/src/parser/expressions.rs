//! Expression and condition parsers.
//!
//! This module contains the parser methods for arithmetic expressions
//! (additive, multiplicative, power, unary, primary), function calls,
//! literals, figurative constants, qualified names with subscripts and
//! reference modification, and all condition types (comparison, class,
//! logical AND/OR/NOT, condition names).

use super::Result;
use crate::ast::*;
use crate::error::CobolError;
use crate::lexer::{Keyword, Span, TokenKind};

impl super::Parser {
    // ========================================================================
    // EXPRESSIONS
    // ========================================================================

    pub(super) fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_additive_expression()
    }

    fn parse_additive_expression(&mut self) -> Result<Expression> {
        let mut left = self.parse_multiplicative_expression()?;

        while self.check(TokenKind::Plus) || self.check(TokenKind::Minus) {
            let op = if self.check(TokenKind::Plus) {
                BinaryOp::Add
            } else {
                BinaryOp::Subtract
            };
            self.advance();
            let right = self.parse_multiplicative_expression()?;
            let span = left.span().extend(right.span());
            left = Expression::Binary(Box::new(BinaryExpr {
                left,
                op,
                right,
                span,
            }));
        }

        Ok(left)
    }

    fn parse_multiplicative_expression(&mut self) -> Result<Expression> {
        let mut left = self.parse_power_expression()?;

        while self.check(TokenKind::Star) || self.check(TokenKind::Slash) {
            let op = if self.check(TokenKind::Star) {
                BinaryOp::Multiply
            } else {
                BinaryOp::Divide
            };
            self.advance();
            let right = self.parse_power_expression()?;
            let span = left.span().extend(right.span());
            left = Expression::Binary(Box::new(BinaryExpr {
                left,
                op,
                right,
                span,
            }));
        }

        Ok(left)
    }

    fn parse_power_expression(&mut self) -> Result<Expression> {
        let mut left = self.parse_unary_expression()?;

        if self.check(TokenKind::DoubleStar) {
            self.advance();
            let right = self.parse_power_expression()?; // Right associative
            let span = left.span().extend(right.span());
            left = Expression::Binary(Box::new(BinaryExpr {
                left,
                op: BinaryOp::Power,
                right,
                span,
            }));
        }

        Ok(left)
    }

    fn parse_unary_expression(&mut self) -> Result<Expression> {
        if self.check(TokenKind::Plus) {
            let start = self.current_span();
            self.advance();
            let operand = self.parse_primary_expression()?;
            let span = start.extend(operand.span());
            Ok(Expression::Unary(Box::new(UnaryExpr {
                op: UnaryOp::Plus,
                operand,
                span,
            })))
        } else if self.check(TokenKind::Minus) {
            let start = self.current_span();
            self.advance();
            let operand = self.parse_primary_expression()?;
            let span = start.extend(operand.span());
            Ok(Expression::Unary(Box::new(UnaryExpr {
                op: UnaryOp::Minus,
                operand,
                span,
            })))
        } else {
            self.parse_primary_expression()
        }
    }

    fn parse_primary_expression(&mut self) -> Result<Expression> {
        if self.check(TokenKind::LeftParen) {
            self.advance();
            let expr = self.parse_expression()?;
            self.expect(TokenKind::RightParen)?;
            Ok(Expression::Paren(Box::new(expr)))
        } else if self.check_keyword(Keyword::Function) {
            self.parse_function_call()
        } else if self.check_keyword(Keyword::Length) {
            // LENGTH OF data-item
            self.parse_length_of()
        } else if self.check_keyword(Keyword::Address) {
            // ADDRESS OF data-item
            self.parse_address_of()
        } else if self.check_literal() {
            Ok(Expression::Literal(self.parse_literal()?))
        } else if self.check_figurative_constant() {
            Ok(Expression::Literal(self.parse_figurative_constant()?))
        } else if self.check_identifier() {
            let name = self.parse_qualified_name()?;
            // Check for reference modification
            if self.check(TokenKind::LeftParen) {
                // Could be subscript or refmod - for now treat as subscript
            }
            Ok(Expression::Variable(name))
        } else {
            Err(CobolError::ParseError {
                message: format!("Expected expression, found {:?}", self.current().kind),
            })
        }
    }

    fn parse_length_of(&mut self) -> Result<Expression> {
        let start = self.current_span();
        self.advance(); // LENGTH

        // OF is optional but commonly used
        if self.check_keyword(Keyword::Of) {
            self.advance();
        }

        let name = self.parse_qualified_name()?;
        let end = self.previous_span();

        Ok(Expression::LengthOf(LengthOf {
            item: name,
            span: start.extend(end),
        }))
    }

    fn parse_address_of(&mut self) -> Result<Expression> {
        let start = self.current_span();
        self.advance(); // ADDRESS

        // OF is required
        if self.check_keyword(Keyword::Of) {
            self.advance();
        }

        let name = self.parse_qualified_name()?;
        let end = self.previous_span();

        Ok(Expression::AddressOf(AddressOf {
            item: name,
            span: start.extend(end),
        }))
    }

    fn parse_function_call(&mut self) -> Result<Expression> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Function)?;
        let name = self.expect_identifier()?;

        let mut arguments = Vec::new();
        if self.check(TokenKind::LeftParen) {
            self.advance();

            // Check if this is reference modification (expr:expr) vs arguments
            let first_expr = self.parse_expression()?;

            if self.check(TokenKind::Colon) {
                // This is reference modification on the function result: FUNCTION NAME(start:length)
                self.advance(); // skip colon
                let length = if self.check(TokenKind::RightParen) {
                    None
                } else {
                    Some(Box::new(self.parse_expression()?))
                };
                self.expect(TokenKind::RightParen)?;
                // Return the function call - store refmod args as regular args for now
                // The refmod info is encoded in the arguments
                arguments.push(first_expr);
                if let Some(len) = length {
                    arguments.push(*len);
                }
            } else {
                // Regular arguments
                arguments.push(first_expr);
                while self.check(TokenKind::Comma) && !self.is_at_end() {
                    self.advance();
                    if !self.check(TokenKind::RightParen) {
                        arguments.push(self.parse_expression()?);
                    }
                }
                // Handle additional arguments without commas
                while !self.check(TokenKind::RightParen) && !self.is_at_end() {
                    arguments.push(self.parse_expression()?);
                    if self.check(TokenKind::Comma) {
                        self.advance();
                    }
                }
                self.expect(TokenKind::RightParen)?;
            }
        }

        let end = self.previous_span();

        Ok(Expression::Function(FunctionCall {
            name,
            arguments,
            span: start.extend(end),
        }))
    }

    pub(super) fn parse_literal(&mut self) -> Result<Literal> {
        let span = self.current_span();

        // Handle optional sign prefix for numeric literals
        let is_negative = if self.check(TokenKind::Plus) {
            self.advance();
            false
        } else if self.check(TokenKind::Minus) {
            self.advance();
            true
        } else {
            false
        };

        let kind = match &self.current().kind {
            TokenKind::IntegerLiteral(n) => {
                let n = if is_negative { -(*n as i64) } else { *n as i64 };
                self.advance();
                LiteralKind::Integer(n)
            }
            TokenKind::DecimalLiteral(s) => {
                let s = if is_negative {
                    format!("-{}", s)
                } else {
                    s.clone()
                };
                self.advance();
                LiteralKind::Decimal(s)
            }
            TokenKind::StringLiteral(s) => {
                if is_negative {
                    return Err(CobolError::ParseError {
                        message: "Sign not allowed with string literal".to_string(),
                    });
                }
                let s = s.clone();
                self.advance();
                LiteralKind::String(s)
            }
            TokenKind::HexLiteral(s) => {
                if is_negative {
                    return Err(CobolError::ParseError {
                        message: "Sign not allowed with hex literal".to_string(),
                    });
                }
                let s = s.clone();
                self.advance();
                LiteralKind::Hex(s)
            }
            // Handle figurative constants
            TokenKind::Keyword(Keyword::Zero)
            | TokenKind::Keyword(Keyword::Zeros)
            | TokenKind::Keyword(Keyword::Zeroes) => {
                if is_negative {
                    return Err(CobolError::ParseError {
                        message: "Sign not allowed with figurative constant".to_string(),
                    });
                }
                self.advance();
                LiteralKind::Figurative(FigurativeConstant::Zero)
            }
            TokenKind::Keyword(Keyword::Space) | TokenKind::Keyword(Keyword::Spaces) => {
                if is_negative {
                    return Err(CobolError::ParseError {
                        message: "Sign not allowed with figurative constant".to_string(),
                    });
                }
                self.advance();
                LiteralKind::Figurative(FigurativeConstant::Space)
            }
            TokenKind::Keyword(Keyword::HighValue) | TokenKind::Keyword(Keyword::HighValues) => {
                if is_negative {
                    return Err(CobolError::ParseError {
                        message: "Sign not allowed with figurative constant".to_string(),
                    });
                }
                self.advance();
                LiteralKind::Figurative(FigurativeConstant::HighValue)
            }
            TokenKind::Keyword(Keyword::LowValue) | TokenKind::Keyword(Keyword::LowValues) => {
                if is_negative {
                    return Err(CobolError::ParseError {
                        message: "Sign not allowed with figurative constant".to_string(),
                    });
                }
                self.advance();
                LiteralKind::Figurative(FigurativeConstant::LowValue)
            }
            TokenKind::Keyword(Keyword::Quote) | TokenKind::Keyword(Keyword::Quotes) => {
                if is_negative {
                    return Err(CobolError::ParseError {
                        message: "Sign not allowed with figurative constant".to_string(),
                    });
                }
                self.advance();
                LiteralKind::Figurative(FigurativeConstant::Quote)
            }
            // ALL followed by a literal
            TokenKind::Keyword(Keyword::All) => {
                if is_negative {
                    return Err(CobolError::ParseError {
                        message: "Sign not allowed with ALL".to_string(),
                    });
                }
                self.advance();
                // Parse the literal that follows ALL
                let inner_literal = self.parse_literal()?;
                LiteralKind::AllOf(Box::new(inner_literal))
            }
            _ => {
                return Err(CobolError::ParseError {
                    message: format!("Expected literal, found {:?}", self.current().kind),
                })
            }
        };

        Ok(Literal { kind, span })
    }

    fn parse_figurative_constant(&mut self) -> Result<Literal> {
        let span = self.current_span();
        let fc = if self.check_keyword(Keyword::Zero)
            || self.check_keyword(Keyword::Zeros)
            || self.check_keyword(Keyword::Zeroes)
        {
            self.advance();
            FigurativeConstant::Zero
        } else if self.check_keyword(Keyword::Space) || self.check_keyword(Keyword::Spaces) {
            self.advance();
            FigurativeConstant::Space
        } else if self.check_keyword(Keyword::HighValue) || self.check_keyword(Keyword::HighValues)
        {
            self.advance();
            FigurativeConstant::HighValue
        } else if self.check_keyword(Keyword::LowValue) || self.check_keyword(Keyword::LowValues) {
            self.advance();
            FigurativeConstant::LowValue
        } else if self.check_keyword(Keyword::Quote) || self.check_keyword(Keyword::Quotes) {
            self.advance();
            FigurativeConstant::Quote
        } else {
            return Err(CobolError::ParseError {
                message: "Expected figurative constant".to_string(),
            });
        };

        Ok(Literal {
            kind: LiteralKind::Figurative(fc),
            span,
        })
    }

    pub(super) fn parse_qualified_name(&mut self) -> Result<QualifiedName> {
        let start = self.current_span();
        let name = self.expect_identifier()?;

        let mut qualifiers = Vec::new();
        let mut subscripts = Vec::new();
        let mut refmod = None;

        // Parse OF/IN qualifications (OF and IN are synonymous in COBOL)
        while self.check_keyword(Keyword::Of) || self.check_keyword(Keyword::In) {
            self.advance(); // consume OF or IN
            qualifiers.push(self.expect_identifier()?);
        }

        // Parse subscripts and/or reference modification
        if self.check(TokenKind::LeftParen) {
            self.advance();

            // Parse first expression
            let first_expr = self.parse_expression()?;

            // Check if this is reference modification (has colon)
            if self.check(TokenKind::Colon) {
                self.advance();
                // Reference modification: (start:length) or (start:)
                let length = if self.check(TokenKind::RightParen) {
                    None
                } else {
                    Some(Box::new(self.parse_expression()?))
                };
                refmod = Some((Box::new(first_expr), length));
                self.expect(TokenKind::RightParen)?;
            } else {
                // Regular subscripts
                subscripts.push(first_expr);
                while self.check(TokenKind::Comma) && !self.is_at_end() {
                    self.advance();
                    subscripts.push(self.parse_expression()?);
                }
                self.expect(TokenKind::RightParen)?;

                // Check for reference modification after subscripts
                if self.check(TokenKind::LeftParen) {
                    self.advance();
                    let refmod_start = self.parse_expression()?;
                    self.expect(TokenKind::Colon)?;
                    let length = if self.check(TokenKind::RightParen) {
                        None
                    } else {
                        Some(Box::new(self.parse_expression()?))
                    };
                    refmod = Some((Box::new(refmod_start), length));
                    self.expect(TokenKind::RightParen)?;
                }
            }
        }

        let end = self.previous_span();

        Ok(QualifiedName {
            name,
            qualifiers,
            subscripts,
            refmod,
            span: start.extend(end),
        })
    }

    // ========================================================================
    // CONDITIONS
    // ========================================================================

    pub(super) fn parse_condition(&mut self) -> Result<Condition> {
        self.parse_or_condition()
    }

    fn parse_or_condition(&mut self) -> Result<Condition> {
        let mut left = self.parse_and_condition()?;

        while self.check_keyword(Keyword::Or) {
            self.advance();
            let right = self.parse_and_condition()?;
            left = Condition::Or(Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    fn parse_and_condition(&mut self) -> Result<Condition> {
        let mut left = self.parse_not_condition()?;

        while self.check_keyword(Keyword::And) {
            self.advance();
            let right = self.parse_not_condition()?;
            left = Condition::And(Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    fn parse_not_condition(&mut self) -> Result<Condition> {
        if self.check_keyword(Keyword::Not) {
            self.advance();
            let cond = self.parse_primary_condition()?;
            Ok(Condition::Not(Box::new(cond)))
        } else {
            self.parse_primary_condition()
        }
    }

    fn parse_primary_condition(&mut self) -> Result<Condition> {
        if self.check(TokenKind::LeftParen) {
            self.advance();
            let cond = self.parse_condition()?;
            self.expect(TokenKind::RightParen)?;
            return Ok(Condition::Paren(Box::new(cond)));
        }

        // Try to parse as comparison
        let left = self.parse_expression()?;

        // Check for class conditions first (with or without IS)
        // e.g., "X IS NUMERIC" or "X NOT NUMERIC" or "X NUMERIC"
        {
            let is_present = self.check_keyword(Keyword::Is);
            if is_present {
                self.advance();
            }

            let negated = if self.check_keyword(Keyword::Not) {
                // Check if this is a class condition (NOT NUMERIC) vs comparison (NOT =)
                let next_is_class = self.peek_keyword(Keyword::Numeric)
                    || self.peek_keyword(Keyword::Alphabetic)
                    || self.peek_keyword(Keyword::AlphabeticLower)
                    || self.peek_keyword(Keyword::AlphabeticUpper);
                if is_present || next_is_class {
                    self.advance();
                    true
                } else {
                    false
                }
            } else {
                false
            };

            if self.check_keyword(Keyword::Numeric) {
                self.advance();
                return Ok(Condition::Class(Box::new(ClassCondition {
                    operand: left,
                    class: ClassType::Numeric,
                    negated,
                    span: Span::dummy(),
                })));
            } else if self.check_keyword(Keyword::Alphabetic) {
                self.advance();
                return Ok(Condition::Class(Box::new(ClassCondition {
                    operand: left,
                    class: ClassType::Alphabetic,
                    negated,
                    span: Span::dummy(),
                })));
            } else if self.check_keyword(Keyword::AlphabeticLower) {
                self.advance();
                return Ok(Condition::Class(Box::new(ClassCondition {
                    operand: left,
                    class: ClassType::AlphabeticLower,
                    negated,
                    span: Span::dummy(),
                })));
            } else if self.check_keyword(Keyword::AlphabeticUpper) {
                self.advance();
                return Ok(Condition::Class(Box::new(ClassCondition {
                    operand: left,
                    class: ClassType::AlphabeticUpper,
                    negated,
                    span: Span::dummy(),
                })));
            }
            // If we consumed IS but didn't find a class keyword, that's an error
            // But if no IS was present, fall through to comparison parsing
        }

        // Check for comparison operators
        if self.check_comparison_op() {
            let op = self.parse_comparison_op()?;
            let right = self.parse_expression()?;
            let span = left.span().extend(right.span());
            return Ok(Condition::Comparison(Box::new(Comparison {
                left,
                op,
                right,
                span,
            })));
        }

        // Condition name (88-level)
        if let Expression::Variable(name) = &left {
            Ok(Condition::ConditionName(name.clone()))
        } else {
            // Treat any non-zero expression as a truthy condition
            // This handles abbreviated conditions and other edge cases
            let span = left.span();
            let qn = QualifiedName {
                name: "__EXPR__".to_string(),
                qualifiers: Vec::new(),
                subscripts: Vec::new(),
                refmod: None,
                span,
            };
            Ok(Condition::ConditionName(qn))
        }
    }

    fn check_comparison_op(&self) -> bool {
        matches!(
            self.current().kind,
            TokenKind::Equals
                | TokenKind::GreaterThan
                | TokenKind::LessThan
                | TokenKind::GreaterEquals
                | TokenKind::LessEquals
                | TokenKind::NotEquals
        ) || self.check_keyword(Keyword::Equal)
            || self.check_keyword(Keyword::Greater)
            || self.check_keyword(Keyword::Less)
            || self.check_keyword(Keyword::Not)
    }

    fn parse_comparison_op(&mut self) -> Result<ComparisonOp> {
        let op = match &self.current().kind {
            TokenKind::Equals => {
                self.advance();
                ComparisonOp::Equal
            }
            TokenKind::GreaterThan => {
                self.advance();
                ComparisonOp::GreaterThan
            }
            TokenKind::LessThan => {
                self.advance();
                ComparisonOp::LessThan
            }
            TokenKind::GreaterEquals => {
                self.advance();
                ComparisonOp::GreaterOrEqual
            }
            TokenKind::LessEquals => {
                self.advance();
                ComparisonOp::LessOrEqual
            }
            TokenKind::NotEquals => {
                self.advance();
                ComparisonOp::NotEqual
            }
            TokenKind::Keyword(Keyword::Equal) => {
                self.advance();
                // Skip TO if present
                if self.check_keyword(Keyword::To) {
                    self.advance();
                }
                ComparisonOp::Equal
            }
            TokenKind::Keyword(Keyword::Greater) => {
                self.advance();
                if self.check_keyword(Keyword::Than) {
                    self.advance();
                }
                if self.check_keyword(Keyword::Or) {
                    self.advance();
                    self.expect_keyword(Keyword::Equal)?;
                    if self.check_keyword(Keyword::To) {
                        self.advance();
                    }
                    ComparisonOp::GreaterOrEqual
                } else {
                    ComparisonOp::GreaterThan
                }
            }
            TokenKind::Keyword(Keyword::Less) => {
                self.advance();
                if self.check_keyword(Keyword::Than) {
                    self.advance();
                }
                if self.check_keyword(Keyword::Or) {
                    self.advance();
                    self.expect_keyword(Keyword::Equal)?;
                    if self.check_keyword(Keyword::To) {
                        self.advance();
                    }
                    ComparisonOp::LessOrEqual
                } else {
                    ComparisonOp::LessThan
                }
            }
            TokenKind::Keyword(Keyword::Not) => {
                self.advance();
                if self.check_keyword(Keyword::Equal) {
                    self.advance();
                    if self.check_keyword(Keyword::To) {
                        self.advance();
                    }
                    ComparisonOp::NotEqual
                } else if self.check(TokenKind::Equals) {
                    self.advance();
                    ComparisonOp::NotEqual
                } else if self.check_keyword(Keyword::Greater) {
                    self.advance();
                    if self.check_keyword(Keyword::Than) {
                        self.advance();
                    }
                    ComparisonOp::LessOrEqual // NOT GREATER = LESS OR EQUAL
                } else if self.check_keyword(Keyword::Less) {
                    self.advance();
                    if self.check_keyword(Keyword::Than) {
                        self.advance();
                    }
                    ComparisonOp::GreaterOrEqual // NOT LESS = GREATER OR EQUAL
                } else if self.check(TokenKind::GreaterThan) {
                    self.advance();
                    ComparisonOp::LessOrEqual
                } else if self.check(TokenKind::LessThan) {
                    self.advance();
                    ComparisonOp::GreaterOrEqual
                } else {
                    return Err(CobolError::ParseError {
                        message: format!("Expected comparison operator after NOT, found {:?}", self.current().kind),
                    });
                }
            }
            _ => {
                return Err(CobolError::ParseError {
                    message: "Expected comparison operator".to_string(),
                })
            }
        };
        Ok(op)
    }
}
