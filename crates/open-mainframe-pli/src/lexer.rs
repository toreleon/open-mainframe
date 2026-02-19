//! PL/I Lexer — tokenizes free-form PL/I source.
//!
//! PL/I has **no reserved words**: any keyword (DECLARE, IF, DO, etc.) can also
//! be used as an identifier.  The lexer therefore produces `Ident` tokens for
//! all keyword-like names; the parser resolves context later.

use serde::{Deserialize, Serialize};

// ---------------------------------------------------------------------------
//  Token types
// ---------------------------------------------------------------------------

/// A PL/I token.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// Token kind.
    pub kind: TokenKind,
    /// Source text of the token.
    pub text: String,
    /// Line number (1-based).
    pub line: usize,
    /// Column number (1-based).
    pub col: usize,
}

/// Token kinds produced by the PL/I lexer.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TokenKind {
    /// Identifier or keyword-in-context (e.g. DECLARE, X, IF).
    Ident,
    /// Integer literal (e.g. 123).
    IntLit,
    /// Decimal literal with fractional part (e.g. 12.34).
    DecLit,
    /// Floating-point literal (e.g. 1.23E+02).
    FloatLit,
    /// String literal in single quotes (e.g. 'Hello').
    StringLit,
    /// Bit-string literal (e.g. '1010'B).
    BitLit,
    /// Hex-string literal (e.g. 'FF'X or X'FF').
    HexLit,
    /// Semicolon (;).
    Semi,
    /// Comma (,).
    Comma,
    /// Left parenthesis.
    LParen,
    /// Right parenthesis.
    RParen,
    /// Colon (:).
    Colon,
    /// Period (.).
    Dot,
    /// Assignment / equality (=).
    Eq,
    /// Not-equal (^= or \= or ¬=).
    Ne,
    /// Less than (<).
    Lt,
    /// Greater than (>).
    Gt,
    /// Less than or equal (<=).
    Le,
    /// Greater than or equal (>=).
    Ge,
    /// Not (^ or \ or ¬).
    Not,
    /// AND (&).
    And,
    /// OR (|).
    Or,
    /// Concatenation (||).
    Concat,
    /// Plus (+).
    Plus,
    /// Minus (-).
    Minus,
    /// Asterisk / multiply (*).
    Star,
    /// Slash / divide (/).
    Slash,
    /// Exponentiation (**).
    Power,
    /// Arrow / pointer qualification (->).
    Arrow,
    /// Percent sign (%) — preprocessor.
    Percent,
    /// End of file.
    Eof,
}

// ---------------------------------------------------------------------------
//  Lexer errors
// ---------------------------------------------------------------------------

/// Errors from the PL/I lexer.
#[derive(Debug, Clone, thiserror::Error)]
pub enum LexerError {
    /// Unterminated string literal.
    #[error("unterminated string literal at line {line}:{col}")]
    UnterminatedString { line: usize, col: usize },

    /// Unterminated comment.
    #[error("unterminated comment starting at line {line}:{col}")]
    UnterminatedComment { line: usize, col: usize },

    /// Unexpected character.
    #[error("unexpected character '{ch}' at line {line}:{col}")]
    UnexpectedChar { ch: char, line: usize, col: usize },
}

// ---------------------------------------------------------------------------
//  Lexer
// ---------------------------------------------------------------------------

/// PL/I Lexer — converts source text into a stream of tokens.
pub struct Lexer {
    chars: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
}

impl Lexer {
    /// Create a new lexer for the given source text.
    pub fn new(source: &str) -> Self {
        Self {
            chars: source.chars().collect(),
            pos: 0,
            line: 1,
            col: 1,
        }
    }

    /// Tokenize the entire source, returning all tokens (including EOF).
    pub fn tokenize(source: &str) -> Result<Vec<Token>, LexerError> {
        let mut lexer = Self::new(source);
        let mut tokens = Vec::new();
        loop {
            let tok = lexer.next_token()?;
            let is_eof = tok.kind == TokenKind::Eof;
            tokens.push(tok);
            if is_eof {
                break;
            }
        }
        Ok(tokens)
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn peek_at(&self, offset: usize) -> Option<char> {
        self.chars.get(self.pos + offset).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.chars.get(self.pos).copied()?;
        self.pos += 1;
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) -> Result<bool, LexerError> {
        if self.peek() == Some('/') && self.peek_at(1) == Some('*') {
            let start_line = self.line;
            let start_col = self.col;
            self.advance(); // /
            self.advance(); // *
            loop {
                match self.advance() {
                    Some('*') if self.peek() == Some('/') => {
                        self.advance(); // /
                        return Ok(true);
                    }
                    Some(_) => {}
                    None => {
                        return Err(LexerError::UnterminatedComment {
                            line: start_line,
                            col: start_col,
                        });
                    }
                }
            }
        }
        Ok(false)
    }

    /// Get the next token.
    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        // Skip whitespace and comments.
        loop {
            self.skip_whitespace();
            if !self.skip_comment()? {
                break;
            }
        }

        let line = self.line;
        let col = self.col;

        let Some(ch) = self.peek() else {
            return Ok(Token {
                kind: TokenKind::Eof,
                text: String::new(),
                line,
                col,
            });
        };

        // String literals.
        if ch == '\'' || ch == '"' {
            return self.lex_string(ch);
        }

        // Hex prefix: X'...'
        if (ch == 'X' || ch == 'x') && self.peek_at(1) == Some('\'') {
            return self.lex_hex_prefixed();
        }

        // Numeric literals.
        if ch.is_ascii_digit() {
            return self.lex_number();
        }

        // Identifiers and keywords.
        if ch.is_ascii_alphabetic() || ch == '_' || ch == '#' || ch == '@' || ch == '$' {
            return self.lex_ident();
        }

        // Preprocessor.
        if ch == '%' {
            self.advance();
            return Ok(Token {
                kind: TokenKind::Percent,
                text: "%".to_string(),
                line,
                col,
            });
        }

        // Operators and punctuation.
        self.lex_operator()
    }

    fn lex_string(&mut self, quote: char) -> Result<Token, LexerError> {
        let line = self.line;
        let col = self.col;
        self.advance(); // opening quote

        let mut text = String::new();
        text.push(quote);

        loop {
            match self.advance() {
                Some(c) if c == quote => {
                    // Doubled quote = escaped.
                    if self.peek() == Some(quote) {
                        text.push(quote);
                        text.push(quote);
                        self.advance();
                    } else {
                        text.push(quote);
                        break;
                    }
                }
                Some(c) => text.push(c),
                None => return Err(LexerError::UnterminatedString { line, col }),
            }
        }

        // Check for suffix: B (bit), X (hex).
        if let Some(suffix) = self.peek() {
            let upper = suffix.to_ascii_uppercase();
            if upper == 'B' {
                self.advance();
                text.push(suffix);
                return Ok(Token {
                    kind: TokenKind::BitLit,
                    text,
                    line,
                    col,
                });
            }
            if upper == 'X' {
                self.advance();
                text.push(suffix);
                return Ok(Token {
                    kind: TokenKind::HexLit,
                    text,
                    line,
                    col,
                });
            }
        }

        Ok(Token {
            kind: TokenKind::StringLit,
            text,
            line,
            col,
        })
    }

    fn lex_hex_prefixed(&mut self) -> Result<Token, LexerError> {
        let line = self.line;
        let col = self.col;
        let x = self.advance().unwrap(); // X or x
        self.advance(); // opening quote

        let mut text = String::new();
        text.push(x);
        text.push('\'');

        loop {
            match self.advance() {
                Some('\'') => {
                    if self.peek() == Some('\'') {
                        text.push('\'');
                        text.push('\'');
                        self.advance();
                    } else {
                        text.push('\'');
                        break;
                    }
                }
                Some(c) => text.push(c),
                None => return Err(LexerError::UnterminatedString { line, col }),
            }
        }

        Ok(Token {
            kind: TokenKind::HexLit,
            text,
            line,
            col,
        })
    }

    fn lex_number(&mut self) -> Result<Token, LexerError> {
        let line = self.line;
        let col = self.col;
        let mut text = String::new();

        // Integer part.
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() {
                text.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        // Check for suffix: B (binary literal).
        if let Some(suffix) = self.peek() {
            if suffix.to_ascii_uppercase() == 'B' && !text.chars().any(|c| c > '1') {
                self.advance();
                text.push(suffix);
                return Ok(Token {
                    kind: TokenKind::BitLit,
                    text,
                    line,
                    col,
                });
            }
        }

        // Fractional part.
        if self.peek() == Some('.') && self.peek_at(1).map_or(false, |c| c.is_ascii_digit()) {
            text.push('.');
            self.advance();
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() {
                    text.push(ch);
                    self.advance();
                } else {
                    break;
                }
            }
        }

        // Exponent part.
        if let Some(e) = self.peek() {
            if e == 'E' || e == 'e' {
                let mut exp_text = String::new();
                exp_text.push(e);
                let next = self.peek_at(1);
                if next == Some('+') || next == Some('-') {
                    exp_text.push(next.unwrap());
                    if self.peek_at(2).map_or(false, |c| c.is_ascii_digit()) {
                        text.push_str(&exp_text);
                        self.advance(); // E
                        self.advance(); // +/-
                        while let Some(ch) = self.peek() {
                            if ch.is_ascii_digit() {
                                text.push(ch);
                                self.advance();
                            } else {
                                break;
                            }
                        }
                        return Ok(Token {
                            kind: TokenKind::FloatLit,
                            text,
                            line,
                            col,
                        });
                    }
                } else if next.map_or(false, |c| c.is_ascii_digit()) {
                    text.push(e);
                    self.advance(); // E
                    while let Some(ch) = self.peek() {
                        if ch.is_ascii_digit() {
                            text.push(ch);
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    return Ok(Token {
                        kind: TokenKind::FloatLit,
                        text,
                        line,
                        col,
                    });
                }
            }
        }

        let kind = if text.contains('.') {
            TokenKind::DecLit
        } else {
            TokenKind::IntLit
        };

        Ok(Token {
            kind,
            text,
            line,
            col,
        })
    }

    fn lex_ident(&mut self) -> Result<Token, LexerError> {
        let line = self.line;
        let col = self.col;
        let mut text = String::new();

        while let Some(ch) = self.peek() {
            if ch.is_ascii_alphanumeric() || ch == '_' || ch == '#' || ch == '@' || ch == '$' {
                text.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        Ok(Token {
            kind: TokenKind::Ident,
            text,
            line,
            col,
        })
    }

    fn lex_operator(&mut self) -> Result<Token, LexerError> {
        let line = self.line;
        let col = self.col;
        let ch = self.advance().unwrap();

        let (kind, text) = match ch {
            ';' => (TokenKind::Semi, ";".to_string()),
            ',' => (TokenKind::Comma, ",".to_string()),
            '(' => (TokenKind::LParen, "(".to_string()),
            ')' => (TokenKind::RParen, ")".to_string()),
            ':' => (TokenKind::Colon, ":".to_string()),
            '.' => (TokenKind::Dot, ".".to_string()),
            '+' => (TokenKind::Plus, "+".to_string()),
            '-' => {
                if self.peek() == Some('>') {
                    self.advance();
                    (TokenKind::Arrow, "->".to_string())
                } else {
                    (TokenKind::Minus, "-".to_string())
                }
            }
            '*' => {
                if self.peek() == Some('*') {
                    self.advance();
                    (TokenKind::Power, "**".to_string())
                } else {
                    (TokenKind::Star, "*".to_string())
                }
            }
            '/' => (TokenKind::Slash, "/".to_string()),
            '=' => (TokenKind::Eq, "=".to_string()),
            '<' => {
                if self.peek() == Some('=') {
                    self.advance();
                    (TokenKind::Le, "<=".to_string())
                } else {
                    (TokenKind::Lt, "<".to_string())
                }
            }
            '>' => {
                if self.peek() == Some('=') {
                    self.advance();
                    (TokenKind::Ge, ">=".to_string())
                } else {
                    (TokenKind::Gt, ">".to_string())
                }
            }
            '^' | '\\' | '¬' => {
                if self.peek() == Some('=') {
                    self.advance();
                    (TokenKind::Ne, format!("{}=", ch))
                } else {
                    (TokenKind::Not, ch.to_string())
                }
            }
            '&' => (TokenKind::And, "&".to_string()),
            '|' => {
                if self.peek() == Some('|') {
                    self.advance();
                    (TokenKind::Concat, "||".to_string())
                } else {
                    (TokenKind::Or, "|".to_string())
                }
            }
            _ => {
                return Err(LexerError::UnexpectedChar { ch, line, col });
            }
        };

        Ok(Token { kind, text, line, col })
    }
}

// ---------------------------------------------------------------------------
//  Keyword helpers
// ---------------------------------------------------------------------------

/// Check if an identifier token matches a PL/I keyword (case-insensitive).
pub fn is_keyword(ident: &str, keyword: &str) -> bool {
    ident.eq_ignore_ascii_case(keyword)
}

/// Extract the string value from a string literal token (removing quotes, unescaping).
pub fn string_value(lit: &str) -> String {
    if lit.len() < 2 {
        return String::new();
    }
    let quote = lit.chars().next().unwrap();
    let inner = &lit[1..lit.len() - 1];
    let doubled = format!("{}{}", quote, quote);
    inner.replace(&doubled, &quote.to_string())
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn tok_kinds(src: &str) -> Vec<TokenKind> {
        Lexer::tokenize(src)
            .unwrap()
            .into_iter()
            .map(|t| t.kind)
            .collect()
    }

    fn tok_texts(src: &str) -> Vec<String> {
        Lexer::tokenize(src)
            .unwrap()
            .into_iter()
            .map(|t| t.text)
            .collect()
    }

    #[test]
    fn test_declare_statement() {
        let kinds = tok_kinds("DECLARE X FIXED DECIMAL(7,2);");
        assert_eq!(kinds[0], TokenKind::Ident); // DECLARE
        assert_eq!(kinds[1], TokenKind::Ident); // X
        assert_eq!(kinds[2], TokenKind::Ident); // FIXED
        assert_eq!(kinds[3], TokenKind::Ident); // DECIMAL
        assert_eq!(kinds[4], TokenKind::LParen);
        assert_eq!(kinds[5], TokenKind::IntLit); // 7
        assert_eq!(kinds[6], TokenKind::Comma);
        assert_eq!(kinds[7], TokenKind::IntLit); // 2
        assert_eq!(kinds[8], TokenKind::RParen);
        assert_eq!(kinds[9], TokenKind::Semi);
    }

    #[test]
    fn test_identifier_as_keyword() {
        // IF used as variable name.
        let kinds = tok_kinds("IF = 1;");
        assert_eq!(kinds[0], TokenKind::Ident); // IF — not a keyword token
        assert_eq!(kinds[1], TokenKind::Eq);
        assert_eq!(kinds[2], TokenKind::IntLit);
    }

    #[test]
    fn test_string_literal_with_embedded_quotes() {
        let tokens = Lexer::tokenize("'Hello''World'").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::StringLit);
        assert_eq!(tokens[0].text, "'Hello''World'");
        assert_eq!(string_value(&tokens[0].text), "Hello'World");
    }

    #[test]
    fn test_simple_string() {
        let tokens = Lexer::tokenize("'Hello'").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::StringLit);
        assert_eq!(string_value(&tokens[0].text), "Hello");
    }

    #[test]
    fn test_bit_string_literal() {
        let tokens = Lexer::tokenize("'10110000'B").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::BitLit);
        assert_eq!(tokens[0].text, "'10110000'B");
    }

    #[test]
    fn test_hex_string_suffix() {
        let tokens = Lexer::tokenize("'FF'X").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::HexLit);
    }

    #[test]
    fn test_hex_string_prefix() {
        let tokens = Lexer::tokenize("X'3F5A'").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::HexLit);
        assert_eq!(tokens[0].text, "X'3F5A'");
    }

    #[test]
    fn test_integer_literal() {
        let tokens = Lexer::tokenize("123").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::IntLit);
        assert_eq!(tokens[0].text, "123");
    }

    #[test]
    fn test_decimal_literal() {
        let tokens = Lexer::tokenize("12.34").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::DecLit);
        assert_eq!(tokens[0].text, "12.34");
    }

    #[test]
    fn test_float_literal() {
        let tokens = Lexer::tokenize("1.23E+02").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::FloatLit);
        assert_eq!(tokens[0].text, "1.23E+02");
    }

    #[test]
    fn test_float_no_sign() {
        let tokens = Lexer::tokenize("5E3").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::FloatLit);
        assert_eq!(tokens[0].text, "5E3");
    }

    #[test]
    fn test_comments_skipped() {
        let kinds = tok_kinds("X /* comment */ = 1;");
        assert_eq!(kinds[0], TokenKind::Ident);
        assert_eq!(kinds[1], TokenKind::Eq);
        assert_eq!(kinds[2], TokenKind::IntLit);
    }

    #[test]
    fn test_unterminated_comment() {
        let err = Lexer::tokenize("X /* unterminated").unwrap_err();
        assert!(matches!(err, LexerError::UnterminatedComment { .. }));
    }

    #[test]
    fn test_unterminated_string() {
        let err = Lexer::tokenize("'unterminated").unwrap_err();
        assert!(matches!(err, LexerError::UnterminatedString { .. }));
    }

    #[test]
    fn test_operators() {
        let kinds = tok_kinds("+ - * / ** = ^= < > <= >= & | || -> ;");
        assert_eq!(kinds[0], TokenKind::Plus);
        assert_eq!(kinds[1], TokenKind::Minus);
        assert_eq!(kinds[2], TokenKind::Star);
        assert_eq!(kinds[3], TokenKind::Slash);
        assert_eq!(kinds[4], TokenKind::Power);
        assert_eq!(kinds[5], TokenKind::Eq);
        assert_eq!(kinds[6], TokenKind::Ne);
        assert_eq!(kinds[7], TokenKind::Lt);
        assert_eq!(kinds[8], TokenKind::Gt);
        assert_eq!(kinds[9], TokenKind::Le);
        assert_eq!(kinds[10], TokenKind::Ge);
        assert_eq!(kinds[11], TokenKind::And);
        assert_eq!(kinds[12], TokenKind::Or);
        assert_eq!(kinds[13], TokenKind::Concat);
        assert_eq!(kinds[14], TokenKind::Arrow);
        assert_eq!(kinds[15], TokenKind::Semi);
    }

    #[test]
    fn test_punctuation() {
        let kinds = tok_kinds("( , ) : .");
        assert_eq!(kinds[0], TokenKind::LParen);
        assert_eq!(kinds[1], TokenKind::Comma);
        assert_eq!(kinds[2], TokenKind::RParen);
        assert_eq!(kinds[3], TokenKind::Colon);
        assert_eq!(kinds[4], TokenKind::Dot);
    }

    #[test]
    fn test_percent_preprocessor() {
        let kinds = tok_kinds("%IF DEBUG");
        assert_eq!(kinds[0], TokenKind::Percent);
        assert_eq!(kinds[1], TokenKind::Ident);
    }

    #[test]
    fn test_line_tracking() {
        let tokens = Lexer::tokenize("X\nY\nZ").unwrap();
        assert_eq!(tokens[0].line, 1);
        assert_eq!(tokens[1].line, 2);
        assert_eq!(tokens[2].line, 3);
    }

    #[test]
    fn test_is_keyword() {
        assert!(is_keyword("DECLARE", "DECLARE"));
        assert!(is_keyword("declare", "DECLARE"));
        assert!(is_keyword("Dcl", "DCL"));
        assert!(!is_keyword("X", "DECLARE"));
    }

    #[test]
    fn test_do_iterative() {
        let texts = tok_texts("DO I = 1 TO 100 BY 2;");
        assert_eq!(texts[0], "DO");
        assert_eq!(texts[1], "I");
        assert_eq!(texts[2], "=");
        assert_eq!(texts[3], "1");
        assert_eq!(texts[4], "TO");
        assert_eq!(texts[5], "100");
        assert_eq!(texts[6], "BY");
        assert_eq!(texts[7], "2");
        assert_eq!(texts[8], ";");
    }

    #[test]
    fn test_special_chars_in_idents() {
        let tokens = Lexer::tokenize("#VAR @FIELD $SYS").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Ident);
        assert_eq!(tokens[0].text, "#VAR");
        assert_eq!(tokens[1].text, "@FIELD");
        assert_eq!(tokens[2].text, "$SYS");
    }

    #[test]
    fn test_concatenation_operator() {
        let kinds = tok_kinds("A || B");
        assert_eq!(kinds[0], TokenKind::Ident);
        assert_eq!(kinds[1], TokenKind::Concat);
        assert_eq!(kinds[2], TokenKind::Ident);
    }

    #[test]
    fn test_not_equals_variants() {
        let kinds = tok_kinds("^= \\=");
        assert_eq!(kinds[0], TokenKind::Ne);
        assert_eq!(kinds[1], TokenKind::Ne);
    }

    #[test]
    fn test_eof() {
        let tokens = Lexer::tokenize("").unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Eof);
    }

    #[test]
    fn test_multiline_program() {
        let src = "HELLO: PROC OPTIONS(MAIN);\n  PUT LIST('Hello, World!');\nEND HELLO;";
        let tokens = Lexer::tokenize(src).unwrap();
        // Should tokenize without error.
        let idents: Vec<&str> = tokens
            .iter()
            .filter(|t| t.kind == TokenKind::Ident)
            .map(|t| t.text.as_str())
            .collect();
        assert!(idents.contains(&"HELLO"));
        assert!(idents.contains(&"PROC"));
        assert!(idents.contains(&"OPTIONS"));
        assert!(idents.contains(&"MAIN"));
        assert!(idents.contains(&"PUT"));
        assert!(idents.contains(&"LIST"));
        assert!(idents.contains(&"END"));
    }
}
