//! COBOL recursive descent parser.
//!
//! This module implements a hand-written recursive descent parser for COBOL.
//! It consumes tokens produced by the lexer and builds an AST.
//!
//! The parser supports error recovery to report multiple errors per compilation.

mod data;
mod divisions;
mod expressions;
mod statements;

use crate::ast::*;
use crate::error::CobolError;
use crate::lexer::{Keyword, Span, Token, TokenKind};

/// Result type for parser operations.
pub type Result<T> = std::result::Result<T, CobolError>;

/// The COBOL parser.
pub struct Parser {
    /// Token stream.
    tokens: Vec<Token>,
    /// Current position in the token stream.
    current: usize,
    /// Accumulated errors (for error recovery).
    errors: Vec<CobolError>,
}

impl Parser {
    /// Create a new parser from a token stream.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            errors: Vec::new(),
        }
    }

    /// Parse a complete COBOL program.
    pub fn parse_program(mut self) -> (Option<Program>, Vec<CobolError>) {
        match self.parse_program_inner() {
            Ok(program) => (Some(program), self.errors),
            Err(e) => {
                self.errors.push(e);
                (None, self.errors)
            }
        }
    }

    fn parse_program_inner(&mut self) -> Result<Program> {
        let start = self.current_span();

        // IDENTIFICATION DIVISION is required
        let identification = self.parse_identification_division()?;

        // ENVIRONMENT DIVISION is optional
        let environment = if self.check_keyword(Keyword::Environment) {
            Some(self.parse_environment_division()?)
        } else {
            None
        };

        // DATA DIVISION is optional
        let data = if self.check_keyword(Keyword::Data) {
            Some(self.parse_data_division()?)
        } else {
            None
        };

        // PROCEDURE DIVISION is optional (for copybooks)
        let procedure = if self.check_keyword(Keyword::Procedure) {
            Some(self.parse_procedure_division()?)
        } else {
            None
        };

        // Check for END PROGRAM
        if self.check_keyword(Keyword::EndProgram) {
            self.advance(); // END
            self.expect_keyword(Keyword::Program)?;
            // Optional program name
            if self.check_identifier() {
                self.advance();
            }
            self.expect(TokenKind::Period)?;
        }

        let end = self.previous_span();

        Ok(Program {
            identification,
            environment,
            data,
            procedure,
            span: start.extend(end),
        })
    }
}

// ── Macro-generated dispatch ───────────────────────────────────────────────
// Statement dispatch is generated from `for_parse_dispatch!` in `macros.rs`.
// To add a new statement, add one line there and write the `parse_xxx_statement()`
// method in the impl block below.

macro_rules! gen_parse_dispatch {
    ( $($kw:ident => $parse_fn:ident),* $(,)? ) => {
        impl Parser {
            fn parse_statement(&mut self) -> Result<Statement> {
                $(
                    if self.check_keyword(Keyword::$kw) {
                        return self.$parse_fn();
                    }
                )*
                // Skip unknown statement - but stop at statement boundaries
                let start = self.current_span();
                // Must advance at least once to avoid infinite loop
                self.advance();
                // Then skip tokens until we hit a statement start or scope terminator
                while !self.check(TokenKind::Period)
                    && !self.is_at_end()
                    && !self.is_statement_start()
                    && !self.is_scope_terminator()
                {
                    self.advance();
                }
                Ok(Statement::Continue(ContinueStatement { span: start }))
            }
        }
    };
}
for_parse_dispatch!(gen_parse_dispatch);

// ========================================================================
// UTILITY FUNCTIONS
// ========================================================================

impl Parser {
    fn current(&self) -> &Token {
        self.tokens.get(self.current).unwrap_or_else(|| {
            self.tokens
                .last()
                .expect("Token stream should not be empty")
        })
    }

    fn current_span(&self) -> Span {
        self.current().span
    }

    fn previous_span(&self) -> Span {
        if self.current > 0 {
            self.tokens[self.current - 1].span
        } else {
            self.current_span()
        }
    }

    fn is_at_end(&self) -> bool {
        self.current().kind == TokenKind::Eof
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        &self.tokens[self.current - 1]
    }

    fn check(&self, kind: TokenKind) -> bool {
        std::mem::discriminant(&self.current().kind) == std::mem::discriminant(&kind)
    }

    fn check_keyword(&self, kw: Keyword) -> bool {
        matches!(&self.current().kind, TokenKind::Keyword(k) if *k == kw)
    }

    fn check_identifier_value(&self, name: &str) -> bool {
        matches!(&self.current().kind, TokenKind::Identifier(s) if s.eq_ignore_ascii_case(name))
    }

    fn peek_keyword(&self, kw: Keyword) -> bool {
        if self.current + 1 < self.tokens.len() {
            matches!(&self.tokens[self.current + 1].kind, TokenKind::Keyword(k) if *k == kw)
        } else {
            false
        }
    }

    #[allow(dead_code)]
    fn peek2_keyword(&self, kw: Keyword) -> bool {
        if self.current + 2 < self.tokens.len() {
            matches!(&self.tokens[self.current + 2].kind, TokenKind::Keyword(k) if *k == kw)
        } else {
            false
        }
    }

    fn peek(&self, kind: TokenKind) -> bool {
        if self.current + 1 < self.tokens.len() {
            std::mem::discriminant(&self.tokens[self.current + 1].kind)
                == std::mem::discriminant(&kind)
        } else {
            false
        }
    }

    fn check_identifier(&self) -> bool {
        matches!(&self.current().kind, TokenKind::Identifier(_))
    }

    fn check_literal(&self) -> bool {
        matches!(
            &self.current().kind,
            TokenKind::IntegerLiteral(_)
                | TokenKind::DecimalLiteral(_)
                | TokenKind::StringLiteral(_)
                | TokenKind::HexLiteral(_)
        )
    }

    fn check_figurative_constant(&self) -> bool {
        self.check_keyword(Keyword::Zero)
            || self.check_keyword(Keyword::Zeros)
            || self.check_keyword(Keyword::Zeroes)
            || self.check_keyword(Keyword::Space)
            || self.check_keyword(Keyword::Spaces)
            || self.check_keyword(Keyword::HighValue)
            || self.check_keyword(Keyword::HighValues)
            || self.check_keyword(Keyword::LowValue)
            || self.check_keyword(Keyword::LowValues)
            || self.check_keyword(Keyword::Quote)
            || self.check_keyword(Keyword::Quotes)
    }

    fn check_level_number(&self) -> bool {
        if let TokenKind::IntegerLiteral(n) = &self.current().kind {
            let n = *n;
            (1..=49).contains(&n) || n == 66 || n == 77 || n == 88
        } else {
            false
        }
    }

    fn peek_level_number(&self) -> u8 {
        if let TokenKind::IntegerLiteral(n) = &self.current().kind {
            *n as u8
        } else {
            0
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<()> {
        if self.check(kind.clone()) {
            self.advance();
            Ok(())
        } else {
            Err(CobolError::ParseError {
                message: format!("Expected {:?}, found {:?}", kind, self.current().kind),
            })
        }
    }

    fn expect_keyword(&mut self, kw: Keyword) -> Result<()> {
        if self.check_keyword(kw) {
            self.advance();
            Ok(())
        } else {
            Err(CobolError::ParseError {
                message: format!("Expected keyword {:?}, found {:?}", kw, self.current().kind),
            })
        }
    }

    fn expect_identifier(&mut self) -> Result<String> {
        match &self.current().kind {
            TokenKind::Identifier(s) => {
                let s = s.clone();
                self.advance();
                Ok(s)
            }
            // In COBOL, many keywords can be used as identifiers in data names,
            // paragraph names, etc. Accept keywords as identifiers when expected.
            TokenKind::Keyword(kw) => {
                let s = kw.as_str().to_string();
                self.advance();
                Ok(s)
            }
            _ => Err(CobolError::ParseError {
                message: format!("Expected identifier, found {:?}", self.current().kind),
            }),
        }
    }

    fn expect_identifier_or_string(&mut self) -> Result<String> {
        match &self.current().kind {
            TokenKind::Identifier(s) => {
                let s = s.clone();
                self.advance();
                Ok(s)
            }
            TokenKind::StringLiteral(s) => {
                let s = s.clone();
                self.advance();
                Ok(s)
            }
            _ => Err(CobolError::ParseError {
                message: format!(
                    "Expected identifier or string, found {:?}",
                    self.current().kind
                ),
            }),
        }
    }

    fn expect_integer(&mut self) -> Result<i64> {
        if let TokenKind::IntegerLiteral(n) = &self.current().kind {
            let n = *n;
            self.advance();
            Ok(n)
        } else {
            Err(CobolError::ParseError {
                message: format!("Expected integer, found {:?}", self.current().kind),
            })
        }
    }

    fn expect_level_number(&mut self) -> Result<u8> {
        if let TokenKind::IntegerLiteral(n) = &self.current().kind {
            let n = *n;
            if (1..=49).contains(&n) || n == 66 || n == 77 || n == 88 {
                self.advance();
                Ok(n as u8)
            } else {
                Err(CobolError::ParseError {
                    message: format!("Invalid level number: {}", n),
                })
            }
        } else {
            Err(CobolError::ParseError {
                message: format!("Expected level number, found {:?}", self.current().kind),
            })
        }
    }

    fn skip_if(&mut self, kind: TokenKind) {
        if self.check(kind) {
            self.advance();
        }
    }

    fn is_at_division_start(&self) -> bool {
        self.check_keyword(Keyword::Identification)
            || self.check_keyword(Keyword::Environment)
            || self.check_keyword(Keyword::Data)
            || self.check_keyword(Keyword::Procedure)
    }

    fn is_at_section_start(&self) -> bool {
        (self.check_keyword(Keyword::Configuration)
            || self.check_keyword(Keyword::InputOutput)
            || self.check_keyword(Keyword::File)
            || self.check_keyword(Keyword::WorkingStorage)
            || self.check_keyword(Keyword::Working)
            || self.check_keyword(Keyword::LocalStorage)
            || self.check_keyword(Keyword::Linkage))
            && self.peek_keyword(Keyword::Section)
    }
}

// `is_statement_start` -- generated from `for_parse_dispatch!` in `macros.rs`.
macro_rules! gen_is_statement_start {
    ( $($kw:ident => $parse_fn:ident),* $(,)? ) => {
        impl Parser {
            fn is_statement_start(&self) -> bool {
                $(self.check_keyword(Keyword::$kw) ||)*
                // Also include scope terminators
                self.is_scope_terminator()
            }
        }
    };
}
for_parse_dispatch!(gen_is_statement_start);

impl Parser {
    fn is_scope_terminator(&self) -> bool {
        self.check_keyword(Keyword::Else)
            || self.check_keyword(Keyword::EndIf)
            || self.check_keyword(Keyword::EndEvaluate)
            || self.check_keyword(Keyword::EndPerform)
            || self.check_keyword(Keyword::EndRead)
            || self.check_keyword(Keyword::EndWrite)
            || self.check_keyword(Keyword::EndCompute)
            || self.check_keyword(Keyword::EndAdd)
            || self.check_keyword(Keyword::EndSubtract)
            || self.check_keyword(Keyword::EndMultiply)
            || self.check_keyword(Keyword::EndDivide)
            || self.check_keyword(Keyword::EndCall)
            || self.check_keyword(Keyword::EndString)
            || self.check_keyword(Keyword::EndUnstring)
            || self.check_keyword(Keyword::EndSearch)
            || self.check_keyword(Keyword::When)
            || self.check_keyword(Keyword::Other)
    }

    fn is_data_clause_start(&self) -> bool {
        self.check_keyword(Keyword::Pic)
            || self.check_keyword(Keyword::Picture)
            || self.check_keyword(Keyword::Usage)
            || self.check_keyword(Keyword::Value)
            || self.check_keyword(Keyword::Occurs)
            || self.check_keyword(Keyword::Redefines)
            || self.check_keyword(Keyword::Sign)
            || self.check_keyword(Keyword::Justified)
            || self.check_keyword(Keyword::Just)
            || self.check_keyword(Keyword::Blank)
            || self.is_usage_keyword()
    }

    fn is_usage_keyword(&self) -> bool {
        self.check_keyword(Keyword::Display)
            || self.check_keyword(Keyword::Binary)
            || self.check_keyword(Keyword::Comp)
            || self.check_keyword(Keyword::Comp1)
            || self.check_keyword(Keyword::Comp2)
            || self.check_keyword(Keyword::Comp3)
            || self.check_keyword(Keyword::Comp4)
            || self.check_keyword(Keyword::Comp5)
            || self.check_keyword(Keyword::Computational)
            || self.check_keyword(Keyword::Computational1)
            || self.check_keyword(Keyword::Computational2)
            || self.check_keyword(Keyword::Computational3)
            || self.check_keyword(Keyword::Computational4)
            || self.check_keyword(Keyword::Computational5)
            || self.check_keyword(Keyword::PackedDecimal)
            || self.check_keyword(Keyword::Pointer)
            || self.check_identifier_value("FUNCTION-POINTER")
            || self.check_identifier_value("PROCEDURE-POINTER")
            || self.check_identifier_value("NATIONAL")
    }

    fn consume_until_period(&mut self) -> String {
        let mut result = String::new();
        while !self.check(TokenKind::Period) && !self.is_at_end() {
            match &self.current().kind {
                TokenKind::Identifier(s) => result.push_str(s),
                TokenKind::StringLiteral(s) => result.push_str(s),
                TokenKind::IntegerLiteral(n) => result.push_str(&n.to_string()),
                _ => {}
            }
            result.push(' ');
            self.advance();
        }
        self.skip_if(TokenKind::Period);
        result.trim().to_string()
    }

    fn advance_to_next_sentence(&mut self) {
        while !self.check(TokenKind::Period) && !self.is_at_end() {
            self.advance();
        }
        self.skip_if(TokenKind::Period);
    }
}

/// Parse a COBOL program from tokens.
pub fn parse(tokens: Vec<Token>) -> (Option<Program>, Vec<CobolError>) {
    Parser::new(tokens).parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{scan, FileId, SourceFile, SourceFormat};

    fn parse_text(text: &str) -> (Option<Program>, Vec<CobolError>) {
        let source = SourceFile::from_text(FileId::MAIN, text.to_string(), SourceFormat::Free);
        let (tokens, _errors) = scan(&source);
        parse(tokens)
    }

    #[test]
    fn test_parse_minimal_program() {
        let text = r#"
            IDENTIFICATION DIVISION.
            PROGRAM-ID. HELLO.
            PROCEDURE DIVISION.
                DISPLAY "HELLO, WORLD!".
                STOP RUN.
        "#;

        let (program, errors) = parse_text(text);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
        let program = program.unwrap();
        assert_eq!(program.identification.program_id.name, "HELLO");
        assert!(program.procedure.is_some());
    }

    #[test]
    fn test_parse_with_working_storage() {
        let text = r#"
            IDENTIFICATION DIVISION.
            PROGRAM-ID. TEST.
            DATA DIVISION.
            WORKING-STORAGE SECTION.
            01 WS-NAME PIC X(20).
            01 WS-COUNT PIC 9(5).
            PROCEDURE DIVISION.
                STOP RUN.
        "#;

        let (program, errors) = parse_text(text);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
        let program = program.unwrap();
        assert!(program.data.is_some());
        let data = program.data.as_ref().unwrap();
        assert_eq!(data.working_storage.len(), 2);
    }

    #[test]
    fn test_analyze_picture() {
        let (cat, size, dec) = data::analyze_picture("X(10)");
        assert_eq!(cat, PictureCategory::Alphanumeric);
        assert_eq!(size, 10);
        assert_eq!(dec, 0);

        let (cat, size, dec) = data::analyze_picture("9(5)V99");
        assert_eq!(cat, PictureCategory::Numeric);
        assert_eq!(size, 7);
        assert_eq!(dec, 2);

        let (cat, size, dec) = data::analyze_picture("S9(7)V9(2)");
        assert_eq!(cat, PictureCategory::Numeric);
        assert_eq!(size, 9);
        assert_eq!(dec, 2);
    }
}
