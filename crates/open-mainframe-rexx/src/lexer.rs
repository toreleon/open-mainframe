//! REXX lexer — tokenizes REXX source into a stream of tokens.
//!
//! Key REXX lexical rules:
//! - Comments: `/* ... */` (nestable)
//! - Strings: single-quoted `'...'` or double-quoted `"..."` (doubled quotes for escaping)
//! - Hex strings: `'FF00'x`, binary strings: `'1010'b`
//! - Continuation: comma at end of line continues to next line
//! - No reserved words — all identifiers are `Symbol` tokens
//! - Semicolons are optional clause terminators (EOL also terminates)

use crate::token::{Span, Token, TokenKind};

/// Lexer error.
#[derive(Debug, Clone, thiserror::Error)]
pub enum LexError {
    #[error("unterminated string at line {line}, column {col}")]
    UnterminatedString { line: u32, col: u32 },
    #[error("unterminated comment at line {line}, column {col}")]
    UnterminatedComment { line: u32, col: u32 },
    #[error("unexpected character '{ch}' at line {line}, column {col}")]
    UnexpectedChar { ch: char, line: u32, col: u32 },
}

/// Tokenize REXX source code.
pub fn lex(source: &str) -> Result<Vec<Token>, LexError> {
    let mut lexer = Lexer::new(source);
    lexer.tokenize()
}

/// Detect whether source is a REXX program (starts with `/* REXX */` comment).
pub fn is_rexx(source: &str) -> bool {
    let trimmed = source.trim_start();
    if trimmed.starts_with("/*") {
        let upper = trimmed.to_ascii_uppercase();
        upper.contains("REXX")
    } else {
        false
    }
}

struct Lexer<'a> {
    chars: Vec<char>,
    pos: usize,
    line: u32,
    col: u32,
    source: &'a str,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().collect(),
            pos: 0,
            line: 1,
            col: 1,
            source,
        }
    }

    fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        let _ = self.source; // keep for potential future use

        while self.pos < self.chars.len() {
            let c = self.chars[self.pos];

            match c {
                // Whitespace (not newline)
                ' ' | '\t' => {
                    self.advance();
                }
                // Newline → EOL token
                '\n' => {
                    tokens.push(self.make_token(TokenKind::Eol));
                    self.advance();
                    self.line += 1;
                    self.col = 1;
                }
                '\r' => {
                    self.advance();
                    if self.peek() == Some('\n') {
                        self.advance();
                    }
                    tokens.push(Token {
                        kind: TokenKind::Eol,
                        span: Span { line: self.line, col: self.col },
                    });
                    self.line += 1;
                    self.col = 1;
                }
                // Comments
                '/' if self.peek_at(1) == Some('*') => {
                    self.skip_comment()?;
                }
                // Strings
                '\'' | '"' => {
                    let tok = self.lex_string(c)?;
                    tokens.push(tok);
                }
                // Operators and delimiters
                '+' => { tokens.push(self.make_token(TokenKind::Plus)); self.advance(); }
                '-' => { tokens.push(self.make_token(TokenKind::Minus)); self.advance(); }
                '%' => { tokens.push(self.make_token(TokenKind::Percent)); self.advance(); }
                '(' => { tokens.push(self.make_token(TokenKind::LParen)); self.advance(); }
                ')' => { tokens.push(self.make_token(TokenKind::RParen)); self.advance(); }
                ',' => {
                    // Comma at end of line = continuation
                    let span = self.span();
                    self.advance();
                    // Check if rest of line is blank/comment → continuation
                    if self.is_line_end() {
                        // Skip to next line (continuation)
                        self.skip_to_next_line();
                    } else {
                        tokens.push(Token { kind: TokenKind::Comma, span });
                    }
                }
                ';' => { tokens.push(self.make_token(TokenKind::Semicolon)); self.advance(); }
                ':' => { tokens.push(self.make_token(TokenKind::Colon)); self.advance(); }
                '*' => {
                    let span = self.span();
                    self.advance();
                    if self.peek() == Some('*') {
                        self.advance();
                        tokens.push(Token { kind: TokenKind::StarStar, span });
                    } else {
                        tokens.push(Token { kind: TokenKind::Star, span });
                    }
                }
                '/' => {
                    let span = self.span();
                    self.advance();
                    if self.peek() == Some('/') {
                        self.advance();
                        tokens.push(Token { kind: TokenKind::SlashSlash, span });
                    } else {
                        tokens.push(Token { kind: TokenKind::Slash, span });
                    }
                }
                '|' => {
                    let span = self.span();
                    self.advance();
                    if self.peek() == Some('|') {
                        self.advance();
                        tokens.push(Token { kind: TokenKind::Concat, span });
                    } else {
                        tokens.push(Token { kind: TokenKind::Or, span });
                    }
                }
                '&' => {
                    let span = self.span();
                    self.advance();
                    if self.peek() == Some('&') {
                        self.advance();
                        tokens.push(Token { kind: TokenKind::Xor, span });
                    } else {
                        tokens.push(Token { kind: TokenKind::And, span });
                    }
                }
                '=' => {
                    let span = self.span();
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        // Strict equal (==) — same as Eq for REXX, but let's keep it as Eq
                        tokens.push(Token { kind: TokenKind::Eq, span });
                    } else {
                        tokens.push(Token { kind: TokenKind::Eq, span });
                    }
                }
                '>' => {
                    let span = self.span();
                    self.advance();
                    match self.peek() {
                        Some('=') => { self.advance(); tokens.push(Token { kind: TokenKind::Ge, span }); }
                        Some('>') => {
                            self.advance();
                            if self.peek() == Some('=') {
                                self.advance();
                                tokens.push(Token { kind: TokenKind::StrictGe, span });
                            } else {
                                tokens.push(Token { kind: TokenKind::StrictGt, span });
                            }
                        }
                        _ => tokens.push(Token { kind: TokenKind::Gt, span }),
                    }
                }
                '<' => {
                    let span = self.span();
                    self.advance();
                    match self.peek() {
                        Some('=') => { self.advance(); tokens.push(Token { kind: TokenKind::Le, span }); }
                        Some('<') => {
                            self.advance();
                            if self.peek() == Some('=') {
                                self.advance();
                                tokens.push(Token { kind: TokenKind::StrictLe, span });
                            } else {
                                tokens.push(Token { kind: TokenKind::StrictLt, span });
                            }
                        }
                        _ => tokens.push(Token { kind: TokenKind::Lt, span }),
                    }
                }
                '\\' | '¬' => {
                    let span = self.span();
                    self.advance();
                    match self.peek() {
                        Some('=') => { self.advance(); tokens.push(Token { kind: TokenKind::Ne, span }); }
                        Some('>') => {
                            self.advance();
                            if self.peek() == Some('>') {
                                self.advance();
                                tokens.push(Token { kind: TokenKind::StrictNgt, span });
                            } else {
                                // \> means "not greater than" — same as <=
                                tokens.push(Token { kind: TokenKind::Le, span });
                            }
                        }
                        Some('<') => {
                            self.advance();
                            if self.peek() == Some('<') {
                                self.advance();
                                tokens.push(Token { kind: TokenKind::StrictNlt, span });
                            } else {
                                // \< means "not less than" — same as >=
                                tokens.push(Token { kind: TokenKind::Ge, span });
                            }
                        }
                        _ => tokens.push(Token { kind: TokenKind::Not, span }),
                    }
                }
                // Numbers and symbols
                _ if c.is_ascii_digit() => {
                    let tok = self.lex_number();
                    tokens.push(tok);
                }
                _ if c.is_ascii_alphabetic() || c == '_' || c == '@' || c == '#' || c == '$' || c == '!' || c == '?' => {
                    let tok = self.lex_symbol();
                    tokens.push(tok);
                }
                '.' => {
                    // Could be start of a number (.5) or dot operator
                    if self.peek_at(1).map(|c| c.is_ascii_digit()).unwrap_or(false) {
                        let tok = self.lex_number();
                        tokens.push(tok);
                    } else {
                        tokens.push(self.make_token(TokenKind::Dot));
                        self.advance();
                    }
                }
                _ => {
                    return Err(LexError::UnexpectedChar {
                        ch: c,
                        line: self.line,
                        col: self.col,
                    });
                }
            }
        }

        tokens.push(Token {
            kind: TokenKind::Eof,
            span: Span { line: self.line, col: self.col },
        });

        Ok(tokens)
    }

    fn advance(&mut self) {
        self.pos += 1;
        self.col += 1;
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn peek_at(&self, offset: usize) -> Option<char> {
        self.chars.get(self.pos + offset).copied()
    }

    fn span(&self) -> Span {
        Span { line: self.line, col: self.col }
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        Token { kind, span: self.span() }
    }

    /// Skip a `/* ... */` comment (nestable).
    fn skip_comment(&mut self) -> Result<(), LexError> {
        let start_line = self.line;
        let start_col = self.col;
        self.advance(); // skip '/'
        self.advance(); // skip '*'
        let mut depth = 1u32;

        while self.pos < self.chars.len() && depth > 0 {
            let c = self.chars[self.pos];
            if c == '/' && self.peek_at(1) == Some('*') {
                depth += 1;
                self.advance();
                self.advance();
            } else if c == '*' && self.peek_at(1) == Some('/') {
                depth -= 1;
                self.advance();
                self.advance();
            } else if c == '\n' {
                self.pos += 1;
                self.line += 1;
                self.col = 1;
            } else {
                self.advance();
            }
        }

        if depth > 0 {
            return Err(LexError::UnterminatedComment {
                line: start_line,
                col: start_col,
            });
        }
        Ok(())
    }

    /// Lex a string literal.  Handles hex (`'FF'x`) and binary (`'01'b`).
    fn lex_string(&mut self, quote: char) -> Result<Token, LexError> {
        let span = self.span();
        self.advance(); // skip opening quote
        let mut value = String::new();

        loop {
            if self.pos >= self.chars.len() {
                return Err(LexError::UnterminatedString {
                    line: span.line,
                    col: span.col,
                });
            }
            let c = self.chars[self.pos];
            if c == quote {
                self.advance();
                // Doubled quote = escaped
                if self.peek() == Some(quote) {
                    value.push(quote);
                    self.advance();
                } else {
                    break;
                }
            } else if c == '\n' {
                return Err(LexError::UnterminatedString {
                    line: span.line,
                    col: span.col,
                });
            } else {
                value.push(c);
                self.advance();
            }
        }

        // Check for hex/binary suffix
        let kind = match self.peek() {
            Some('x') | Some('X') => {
                self.advance();
                TokenKind::HexString(value)
            }
            Some('b') | Some('B') => {
                self.advance();
                TokenKind::BinString(value)
            }
            _ => TokenKind::StringLit(value),
        };

        Ok(Token { kind, span })
    }

    /// Lex a number (integer, decimal, or exponential).
    fn lex_number(&mut self) -> Token {
        let span = self.span();
        let mut num = String::new();

        // Integer part
        while self.pos < self.chars.len() && self.chars[self.pos].is_ascii_digit() {
            num.push(self.chars[self.pos]);
            self.advance();
        }

        // Decimal part
        if self.peek() == Some('.') && self.peek_at(1).map(|c| c.is_ascii_digit()).unwrap_or(false) {
            num.push('.');
            self.advance();
            while self.pos < self.chars.len() && self.chars[self.pos].is_ascii_digit() {
                num.push(self.chars[self.pos]);
                self.advance();
            }
        }

        // Exponential part
        if self.peek() == Some('E') || self.peek() == Some('e') {
            num.push('E');
            self.advance();
            if self.peek() == Some('+') || self.peek() == Some('-') {
                num.push(self.chars[self.pos]);
                self.advance();
            }
            while self.pos < self.chars.len() && self.chars[self.pos].is_ascii_digit() {
                num.push(self.chars[self.pos]);
                self.advance();
            }
        }

        Token { kind: TokenKind::Number(num), span }
    }

    /// Lex a symbol (identifier/keyword).
    fn lex_symbol(&mut self) -> Token {
        let span = self.span();
        let mut sym = String::new();

        while self.pos < self.chars.len() {
            let c = self.chars[self.pos];
            if c.is_ascii_alphanumeric() || c == '_' || c == '@' || c == '#' || c == '$' || c == '!' || c == '?' || c == '.' {
                sym.push(c);
                self.advance();
            } else {
                break;
            }
        }

        // Uppercase for REXX (case-insensitive)
        let upper = sym.to_ascii_uppercase();
        Token { kind: TokenKind::Symbol(upper), span }
    }

    /// Check if the rest of the current line is blank or only whitespace/comments.
    fn is_line_end(&self) -> bool {
        let mut i = self.pos;
        while i < self.chars.len() {
            let c = self.chars[i];
            if c == '\n' || c == '\r' {
                return true;
            }
            if c == ' ' || c == '\t' {
                i += 1;
                continue;
            }
            if c == '/' && i + 1 < self.chars.len() && self.chars[i + 1] == '*' {
                // Skip inline comment
                i += 2;
                let mut depth = 1;
                while i < self.chars.len() && depth > 0 {
                    if self.chars[i] == '*' && i + 1 < self.chars.len() && self.chars[i + 1] == '/' {
                        depth -= 1;
                        i += 2;
                    } else if self.chars[i] == '/' && i + 1 < self.chars.len() && self.chars[i + 1] == '*' {
                        depth += 1;
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                continue;
            }
            return false;
        }
        true // end of source
    }

    /// Skip to the beginning of the next line.
    fn skip_to_next_line(&mut self) {
        while self.pos < self.chars.len() {
            let c = self.chars[self.pos];
            if c == '\n' {
                self.pos += 1;
                self.line += 1;
                self.col = 1;
                return;
            }
            self.advance();
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn tok_kinds(src: &str) -> Vec<TokenKind> {
        lex(src)
            .unwrap()
            .into_iter()
            .filter(|t| !matches!(t.kind, TokenKind::Eol | TokenKind::Eof))
            .map(|t| t.kind)
            .collect()
    }

    #[test]
    fn test_is_rexx() {
        assert!(is_rexx("/* REXX */\nsay 'hello'"));
        assert!(is_rexx("  /* rexx */ \n"));
        assert!(!is_rexx("PROC 0\nWRITE hello"));
    }

    #[test]
    fn test_say_string() {
        let tokens = tok_kinds("SAY 'Hello World'");
        assert_eq!(tokens, vec![
            TokenKind::Symbol("SAY".to_string()),
            TokenKind::StringLit("Hello World".to_string()),
        ]);
    }

    #[test]
    fn test_assignment() {
        let tokens = tok_kinds("x = 42");
        assert_eq!(tokens, vec![
            TokenKind::Symbol("X".to_string()),
            TokenKind::Eq,
            TokenKind::Number("42".to_string()),
        ]);
    }

    #[test]
    fn test_operators() {
        let tokens = tok_kinds("a + b - c * d / e % f // g ** h");
        let expected = vec![
            TokenKind::Symbol("A".to_string()), TokenKind::Plus,
            TokenKind::Symbol("B".to_string()), TokenKind::Minus,
            TokenKind::Symbol("C".to_string()), TokenKind::Star,
            TokenKind::Symbol("D".to_string()), TokenKind::Slash,
            TokenKind::Symbol("E".to_string()), TokenKind::Percent,
            TokenKind::Symbol("F".to_string()), TokenKind::SlashSlash,
            TokenKind::Symbol("G".to_string()), TokenKind::StarStar,
            TokenKind::Symbol("H".to_string()),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_comparison_operators() {
        let tokens = tok_kinds("a > b >= c < d <= e \\= f");
        let expected = vec![
            TokenKind::Symbol("A".to_string()), TokenKind::Gt,
            TokenKind::Symbol("B".to_string()), TokenKind::Ge,
            TokenKind::Symbol("C".to_string()), TokenKind::Lt,
            TokenKind::Symbol("D".to_string()), TokenKind::Le,
            TokenKind::Symbol("E".to_string()), TokenKind::Ne,
            TokenKind::Symbol("F".to_string()),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_strict_comparison() {
        let tokens = tok_kinds("a >> b << c >>= d <<= e");
        let expected = vec![
            TokenKind::Symbol("A".to_string()), TokenKind::StrictGt,
            TokenKind::Symbol("B".to_string()), TokenKind::StrictLt,
            TokenKind::Symbol("C".to_string()), TokenKind::StrictGe,
            TokenKind::Symbol("D".to_string()), TokenKind::StrictLe,
            TokenKind::Symbol("E".to_string()),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_logical_operators() {
        let tokens = tok_kinds("a & b | c && d");
        let expected = vec![
            TokenKind::Symbol("A".to_string()), TokenKind::And,
            TokenKind::Symbol("B".to_string()), TokenKind::Or,
            TokenKind::Symbol("C".to_string()), TokenKind::Xor,
            TokenKind::Symbol("D".to_string()),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_concat_operator() {
        let tokens = tok_kinds("a || b");
        assert_eq!(tokens, vec![
            TokenKind::Symbol("A".to_string()),
            TokenKind::Concat,
            TokenKind::Symbol("B".to_string()),
        ]);
    }

    #[test]
    fn test_comment_stripped() {
        let tokens = tok_kinds("x = 2 /* comment */ + 3");
        assert_eq!(tokens, vec![
            TokenKind::Symbol("X".to_string()),
            TokenKind::Eq,
            TokenKind::Number("2".to_string()),
            TokenKind::Plus,
            TokenKind::Number("3".to_string()),
        ]);
    }

    #[test]
    fn test_nested_comment() {
        let tokens = tok_kinds("x /* outer /* inner */ still */ + y");
        assert_eq!(tokens, vec![
            TokenKind::Symbol("X".to_string()),
            TokenKind::Plus,
            TokenKind::Symbol("Y".to_string()),
        ]);
    }

    #[test]
    fn test_hex_string() {
        let tokens = tok_kinds("x = 'FF00'x");
        assert_eq!(tokens[2], TokenKind::HexString("FF00".to_string()));
    }

    #[test]
    fn test_bin_string() {
        let tokens = tok_kinds("x = '10101010'b");
        assert_eq!(tokens[2], TokenKind::BinString("10101010".to_string()));
    }

    #[test]
    fn test_doubled_quote() {
        let tokens = tok_kinds("x = 'it''s'");
        assert_eq!(tokens[2], TokenKind::StringLit("it's".to_string()));
    }

    #[test]
    fn test_double_quote_string() {
        let tokens = tok_kinds(r#"x = "hello""#);
        assert_eq!(tokens[2], TokenKind::StringLit("hello".to_string()));
    }

    #[test]
    fn test_number_decimal() {
        let tokens = tok_kinds("x = 3.14");
        assert_eq!(tokens[2], TokenKind::Number("3.14".to_string()));
    }

    #[test]
    fn test_number_exponential() {
        let tokens = tok_kinds("x = 1E10");
        assert_eq!(tokens[2], TokenKind::Number("1E10".to_string()));
    }

    #[test]
    fn test_semicolons() {
        let tokens = tok_kinds("SAY 'a'; SAY 'b'");
        assert!(tokens.contains(&TokenKind::Semicolon));
    }

    #[test]
    fn test_label_colon() {
        let tokens = tok_kinds("myLabel:");
        assert_eq!(tokens, vec![
            TokenKind::Symbol("MYLABEL".to_string()),
            TokenKind::Colon,
        ]);
    }

    #[test]
    fn test_parens_function_call() {
        let tokens = tok_kinds("LENGTH(x)");
        assert_eq!(tokens, vec![
            TokenKind::Symbol("LENGTH".to_string()),
            TokenKind::LParen,
            TokenKind::Symbol("X".to_string()),
            TokenKind::RParen,
        ]);
    }

    #[test]
    fn test_continuation_comma() {
        let tokens = tok_kinds("SAY 'part1' ||,\n'part2'");
        // Comma at end of line is consumed (continuation), not emitted
        let has_comma = tokens.iter().any(|t| matches!(t, TokenKind::Comma));
        assert!(!has_comma);
        assert!(tokens.contains(&TokenKind::StringLit("part1".to_string())));
        assert!(tokens.contains(&TokenKind::StringLit("part2".to_string())));
    }

    #[test]
    fn test_multiline() {
        let src = "x = 1\ny = 2";
        let all = lex(src).unwrap();
        let eol_count = all.iter().filter(|t| matches!(t.kind, TokenKind::Eol)).count();
        assert_eq!(eol_count, 1); // one EOL between lines (second line has no trailing newline)
    }

    #[test]
    fn test_unterminated_string() {
        let result = lex("SAY 'oops");
        assert!(result.is_err());
    }

    #[test]
    fn test_unterminated_comment() {
        let result = lex("/* unclosed");
        assert!(result.is_err());
    }

    #[test]
    fn test_special_chars_in_symbol() {
        let tokens = tok_kinds("@var #name $sys !flag ?query");
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0], TokenKind::Symbol("@VAR".to_string()));
    }

    #[test]
    fn test_compound_variable() {
        let tokens = tok_kinds("stem.i.j");
        assert_eq!(tokens, vec![
            TokenKind::Symbol("STEM.I.J".to_string()),
        ]);
    }

    #[test]
    fn test_empty_source() {
        let tokens = lex("").unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Eof);
    }

    #[test]
    fn test_full_rexx_program() {
        let src = r#"/* REXX */
SAY 'Hello World'
x = 10
IF x > 5 THEN SAY 'big'
DO i = 1 TO 3
  SAY i
END
EXIT 0
"#;
        let tokens = lex(src).unwrap();
        // Should tokenize without errors
        assert!(tokens.len() > 20);
        // Check SAY is a symbol, not special
        let say_count = tokens.iter()
            .filter(|t| t.kind == TokenKind::Symbol("SAY".to_string()))
            .count();
        assert_eq!(say_count, 3);
    }
}
