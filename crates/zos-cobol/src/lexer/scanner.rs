//! COBOL token scanner (lexer).
//!
//! The scanner converts COBOL source text into a stream of tokens.
//! It handles both fixed and free format sources, string literals,
//! numeric literals, identifiers, and keywords.

use crate::error::CobolError;
use crate::lexer::keywords::lookup_keyword;
use crate::lexer::source::{SourceFile, SourceLine};
use crate::lexer::span::Span;
use crate::lexer::token::{Token, TokenKind};

/// Result type for scanner operations.
pub type Result<T> = std::result::Result<T, CobolError>;

/// The COBOL token scanner.
///
/// Converts processed source lines into a sequence of tokens.
pub struct Scanner<'a> {
    /// The source file being scanned.
    source: &'a SourceFile,
    /// Current line index.
    line_idx: usize,
    /// Current position within the line's content.
    pos: usize,
    /// Accumulated tokens.
    tokens: Vec<Token>,
    /// Accumulated errors.
    errors: Vec<CobolError>,
    /// Whether we're parsing a PICTURE clause (special handling).
    in_picture: bool,
}

impl<'a> Scanner<'a> {
    /// Create a new scanner for a source file.
    pub fn new(source: &'a SourceFile) -> Self {
        Self {
            source,
            line_idx: 0,
            pos: 0,
            tokens: Vec::new(),
            errors: Vec::new(),
            in_picture: false,
        }
    }

    /// Scan all tokens from the source.
    pub fn scan_all(mut self) -> (Vec<Token>, Vec<CobolError>) {
        while !self.is_at_end() {
            self.scan_line();
            self.line_idx += 1;
            self.pos = 0;
        }

        // Add EOF token
        let eof_span = Span::new(
            self.source.id,
            self.source.text.len() as u32,
            self.source.text.len() as u32,
        );
        self.tokens.push(Token::new(TokenKind::Eof, eof_span));

        (self.tokens, self.errors)
    }

    /// Check if we've scanned all lines.
    fn is_at_end(&self) -> bool {
        self.line_idx >= self.source.lines.len()
    }

    /// Get the current line.
    fn current_line(&self) -> Option<&SourceLine> {
        self.source.lines.get(self.line_idx)
    }

    /// Scan a single line.
    fn scan_line(&mut self) {
        let line = match self.current_line() {
            Some(l) => l.clone(),
            None => return,
        };

        // Skip comment lines
        if line.is_comment() {
            return;
        }

        // Handle continuation lines specially
        if line.is_continuation() {
            // Continuation is handled when we encounter it during string parsing
            // For now, just scan it as normal content
        }

        // Skip debug lines (unless in debug mode, which we don't support yet)
        if line.is_debug() {
            return;
        }

        let content = &line.content;
        let base_offset = line.content_offset;

        self.pos = 0;
        while self.pos < content.len() {
            self.scan_token(content, base_offset);
        }
    }

    /// Scan a single token from the content.
    fn scan_token(&mut self, content: &str, base_offset: u32) {
        // Skip whitespace
        self.skip_whitespace(content);
        if self.pos >= content.len() {
            return;
        }

        let start_pos = self.pos;
        let start_offset = base_offset + start_pos as u32;

        let chars: Vec<char> = content[self.pos..].chars().collect();
        if chars.is_empty() {
            return;
        }

        let ch = chars[0];

        // Special handling for PICTURE strings
        if self.in_picture {
            self.scan_picture_string(content, base_offset);
            return;
        }

        let token = match ch {
            // Punctuation
            '.' => {
                self.pos += 1;
                Some(TokenKind::Period)
            }
            ',' => {
                self.pos += 1;
                Some(TokenKind::Comma)
            }
            ';' => {
                self.pos += 1;
                Some(TokenKind::Semicolon)
            }
            '(' => {
                self.pos += 1;
                Some(TokenKind::LeftParen)
            }
            ')' => {
                self.pos += 1;
                Some(TokenKind::RightParen)
            }
            ':' => {
                self.pos += 1;
                if chars.get(1) == Some(&':') {
                    self.pos += 1;
                    Some(TokenKind::DoubleColon)
                } else {
                    Some(TokenKind::Colon)
                }
            }

            // Operators
            '+' => {
                self.pos += 1;
                Some(TokenKind::Plus)
            }
            '-' => {
                self.pos += 1;
                Some(TokenKind::Minus)
            }
            '*' => {
                self.pos += 1;
                if chars.get(1) == Some(&'*') {
                    self.pos += 1;
                    Some(TokenKind::DoubleStar)
                } else {
                    Some(TokenKind::Star)
                }
            }
            '/' => {
                self.pos += 1;
                Some(TokenKind::Slash)
            }
            '=' => {
                self.pos += 1;
                Some(TokenKind::Equals)
            }
            '>' => {
                self.pos += 1;
                if chars.get(1) == Some(&'=') {
                    self.pos += 1;
                    Some(TokenKind::GreaterEquals)
                } else {
                    Some(TokenKind::GreaterThan)
                }
            }
            '<' => {
                self.pos += 1;
                if chars.get(1) == Some(&'=') {
                    self.pos += 1;
                    Some(TokenKind::LessEquals)
                } else if chars.get(1) == Some(&'>') {
                    self.pos += 1;
                    Some(TokenKind::NotEquals)
                } else {
                    Some(TokenKind::LessThan)
                }
            }

            // String literals
            '"' | '\'' => self.scan_string_literal(content, ch),

            // Hex literals X"..." or X'...'
            'X' | 'x' if matches!(chars.get(1), Some('"') | Some('\'')) => {
                self.scan_hex_literal(content)
            }

            // National literals N"..." or N'...'
            'N' | 'n' if matches!(chars.get(1), Some('"') | Some('\'')) => {
                self.scan_national_literal(content)
            }

            // Numbers
            '0'..='9' => self.scan_number(content),

            // Identifiers and keywords
            'A'..='Z' | 'a'..='z' | '_' => self.scan_identifier(content),

            // Unknown character
            _ => {
                self.pos += 1;
                let line_info = self.current_line();
                let line_num = line_info.map(|l| l.line_number).unwrap_or(1);
                self.errors.push(CobolError::InvalidCharacter {
                    character: ch,
                    line: line_num,
                    column: start_pos as u32 + 1,
                });
                Some(TokenKind::Error(format!("Invalid character: {}", ch)))
            }
        };

        if let Some(kind) = token {
            let end_offset = base_offset + self.pos as u32;
            let span = Span::new(self.source.id, start_offset, end_offset);
            self.tokens.push(Token::new(kind, span));
        }
    }

    /// Skip whitespace characters.
    fn skip_whitespace(&mut self, content: &str) {
        while self.pos < content.len() {
            let ch = content[self.pos..].chars().next().unwrap();
            if ch.is_whitespace() {
                self.pos += ch.len_utf8();
            } else {
                break;
            }
        }
    }

    /// Scan a string literal (double or single quoted).
    fn scan_string_literal(&mut self, content: &str, quote: char) -> Option<TokenKind> {
        self.pos += 1; // Skip opening quote

        let mut value = String::new();
        let chars: Vec<char> = content[self.pos..].chars().collect();
        let mut i = 0;

        while i < chars.len() {
            let ch = chars[i];
            if ch == quote {
                // Check for escaped quote (doubled quote)
                if chars.get(i + 1) == Some(&quote) {
                    value.push(quote);
                    i += 2;
                    self.pos += 2;
                } else {
                    // End of string
                    self.pos += 1;
                    return Some(TokenKind::StringLiteral(value));
                }
            } else {
                value.push(ch);
                i += 1;
                self.pos += ch.len_utf8();
            }
        }

        // Unterminated string - report error but continue
        let line_num = self.current_line().map(|l| l.line_number).unwrap_or(1);
        self.errors
            .push(CobolError::UnterminatedString { line: line_num });
        Some(TokenKind::StringLiteral(value))
    }

    /// Scan a hex literal (X"..." or X'...').
    fn scan_hex_literal(&mut self, content: &str) -> Option<TokenKind> {
        self.pos += 1; // Skip 'X'
        let quote = content[self.pos..].chars().next()?;
        self.pos += 1; // Skip quote

        let mut value = String::new();
        let chars: Vec<char> = content[self.pos..].chars().collect();

        for (i, &ch) in chars.iter().enumerate() {
            if ch == quote {
                self.pos += i + 1;
                return Some(TokenKind::HexLiteral(value));
            }
            value.push(ch);
        }

        // Unterminated
        let line_num = self.current_line().map(|l| l.line_number).unwrap_or(1);
        self.errors
            .push(CobolError::UnterminatedString { line: line_num });
        self.pos = content.len();
        Some(TokenKind::HexLiteral(value))
    }

    /// Scan a national literal (N"..." or N'...').
    fn scan_national_literal(&mut self, content: &str) -> Option<TokenKind> {
        self.pos += 1; // Skip 'N'
        let quote = content[self.pos..].chars().next()?;
        self.pos += 1; // Skip quote

        let mut value = String::new();
        let chars: Vec<char> = content[self.pos..].chars().collect();

        for (i, &ch) in chars.iter().enumerate() {
            if ch == quote {
                self.pos += i + 1;
                return Some(TokenKind::NationalLiteral(value));
            }
            value.push(ch);
        }

        // Unterminated
        let line_num = self.current_line().map(|l| l.line_number).unwrap_or(1);
        self.errors
            .push(CobolError::UnterminatedString { line: line_num });
        self.pos = content.len();
        Some(TokenKind::NationalLiteral(value))
    }

    /// Scan a numeric literal.
    fn scan_number(&mut self, content: &str) -> Option<TokenKind> {
        let start = self.pos;
        let chars: Vec<char> = content.chars().collect();

        // Collect integer part
        while self.pos < chars.len() && chars[self.pos].is_ascii_digit() {
            self.pos += 1;
        }

        // Check if this is actually a numeric-prefixed paragraph name (e.g., 2000-OUTFILE-OPEN)
        // In COBOL, paragraph names can start with digits if they contain letters
        if self.pos < chars.len() && chars[self.pos] == '-' {
            let peek_pos = self.pos + 1;
            if peek_pos < chars.len() && chars[peek_pos].is_ascii_alphabetic() {
                // This is a paragraph name, scan as identifier
                while self.pos < chars.len() {
                    let ch = chars[self.pos];
                    if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                // Trim trailing hyphens
                while self.pos > start && chars[self.pos - 1] == '-' {
                    self.pos -= 1;
                }
                let text: String = chars[start..self.pos].iter().collect();
                return Some(TokenKind::Identifier(text.to_uppercase()));
            }
        }

        // Check for decimal point
        let has_decimal =
            self.pos < chars.len() && chars[self.pos] == '.' && self.pos + 1 < chars.len() && {
                // Make sure it's not a period terminator (followed by space or end)
                chars
                    .get(self.pos + 1)
                    .map(|c| c.is_ascii_digit())
                    .unwrap_or(false)
            };

        if has_decimal {
            self.pos += 1; // Skip '.'
            while self.pos < chars.len() && chars[self.pos].is_ascii_digit() {
                self.pos += 1;
            }
        }

        // Check for exponent
        let has_exponent =
            self.pos < chars.len() && (chars[self.pos] == 'E' || chars[self.pos] == 'e');
        if has_exponent {
            self.pos += 1;
            if self.pos < chars.len() && (chars[self.pos] == '+' || chars[self.pos] == '-') {
                self.pos += 1;
            }
            while self.pos < chars.len() && chars[self.pos].is_ascii_digit() {
                self.pos += 1;
            }
        }

        let text: String = chars[start..self.pos].iter().collect();

        if has_decimal || has_exponent {
            Some(TokenKind::DecimalLiteral(text))
        } else {
            // Try to parse as i64, fall back to decimal literal
            match text.parse::<i64>() {
                Ok(n) => Some(TokenKind::IntegerLiteral(n)),
                Err(_) => Some(TokenKind::DecimalLiteral(text)),
            }
        }
    }

    /// Scan an identifier or keyword.
    fn scan_identifier(&mut self, content: &str) -> Option<TokenKind> {
        let start = self.pos;
        let chars: Vec<char> = content.chars().collect();

        // COBOL identifiers can contain letters, digits, and hyphens
        // Must start with a letter
        while self.pos < chars.len() {
            let ch = chars[self.pos];
            if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
                self.pos += 1;
            } else {
                break;
            }
        }

        // Trim trailing hyphens (not valid)
        while self.pos > start && chars[self.pos - 1] == '-' {
            self.pos -= 1;
        }

        let text: String = chars[start..self.pos].iter().collect();

        // Check for PIC/PICTURE keyword to enter picture mode
        let upper = text.to_uppercase();
        if upper == "PIC" || upper == "PICTURE" {
            self.in_picture = true;
            if let Some(kw) = lookup_keyword(&text) {
                return Some(TokenKind::Keyword(kw));
            }
        }

        // Check if it's a keyword
        if let Some(kw) = lookup_keyword(&text) {
            Some(TokenKind::Keyword(kw))
        } else {
            Some(TokenKind::Identifier(text))
        }
    }

    /// Scan a PICTURE string.
    ///
    /// PICTURE strings have special syntax: they contain characters like
    /// X, 9, A, S, V, P and can include parenthesized repeat counts.
    fn scan_picture_string(&mut self, content: &str, base_offset: u32) {
        self.skip_whitespace(content);
        if self.pos >= content.len() {
            self.in_picture = false;
            return;
        }

        // Handle optional IS keyword
        let remaining = &content[self.pos..];
        if remaining.to_uppercase().starts_with("IS") {
            let chars: Vec<char> = remaining.chars().collect();
            if chars.len() > 2 && !chars[2].is_ascii_alphanumeric() && chars[2] != '-' {
                // It's the keyword IS
                let start_offset = base_offset + self.pos as u32;
                self.pos += 2;
                let span = Span::new(self.source.id, start_offset, base_offset + self.pos as u32);
                self.tokens.push(Token::new(
                    TokenKind::Keyword(crate::lexer::token::Keyword::Is),
                    span,
                ));
                self.skip_whitespace(content);
            }
        }

        if self.pos >= content.len() {
            self.in_picture = false;
            return;
        }

        let start = self.pos;
        let start_offset = base_offset + start as u32;
        let chars: Vec<char> = content.chars().collect();

        // PICTURE characters: A B E G N P S V X Z 9 0 1 + - , . * / $ CR DB ( )
        // Also supports currency symbols and editing characters
        let mut paren_depth = 0;

        while self.pos < chars.len() {
            let ch = chars[self.pos];

            if ch == '(' {
                paren_depth += 1;
                self.pos += 1;
            } else if ch == ')' {
                if paren_depth > 0 {
                    paren_depth -= 1;
                    self.pos += 1;
                } else {
                    break;
                }
            } else if ch == '.' {
                // Period is special: it's part of the picture only if followed by a picture char
                // (like 9(3).99), otherwise it's the statement terminator
                let next_char = chars.get(self.pos + 1);
                if let Some(&next) = next_char {
                    if is_picture_char_no_period(next) || next == '(' {
                        self.pos += 1;
                    } else {
                        // Trailing period - statement terminator
                        break;
                    }
                } else {
                    // Period at end of input - statement terminator
                    break;
                }
            } else if is_picture_char_no_period(ch) || (paren_depth > 0 && ch.is_ascii_digit()) {
                self.pos += 1;
            } else if ch.is_whitespace() && paren_depth > 0 {
                // Allow spaces inside parentheses (unusual but sometimes seen)
                self.pos += 1;
            } else {
                break;
            }
        }

        let pic_string: String = chars[start..self.pos].iter().collect();
        let end_offset = base_offset + self.pos as u32;
        let span = Span::new(self.source.id, start_offset, end_offset);
        self.tokens
            .push(Token::new(TokenKind::PictureString(pic_string), span));

        self.in_picture = false;
    }
}

/// Check if a character is valid in a PICTURE string (excluding period).
/// Period is handled specially to distinguish decimal point from statement terminator.
fn is_picture_char_no_period(ch: char) -> bool {
    matches!(
        ch.to_ascii_uppercase(),
        'A' | 'B'
            | 'E'
            | 'G'
            | 'N'
            | 'P'
            | 'S'
            | 'V'
            | 'X'
            | 'Z'
            | '9'
            | '0'
            | '1'
            | '+'
            | '-'
            | ','
            | '*'
            | '/'
            | '$'
            | 'C'
            | 'R'
            | 'D'
    )
}

/// Scan a source file and return tokens.
pub fn scan(source: &SourceFile) -> (Vec<Token>, Vec<CobolError>) {
    Scanner::new(source).scan_all()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::source::SourceFormat;
    use crate::lexer::span::FileId;
    use crate::lexer::token::Keyword;

    fn scan_text(text: &str) -> (Vec<Token>, Vec<CobolError>) {
        let source = SourceFile::from_text(FileId::MAIN, text.to_string(), SourceFormat::Free);
        scan(&source)
    }

    #[test]
    fn test_scan_keywords() {
        let (tokens, errors) = scan_text("IDENTIFICATION DIVISION");
        assert!(errors.is_empty());
        assert_eq!(tokens.len(), 3); // 2 keywords + EOF
        assert!(tokens[0].is_keyword(Keyword::Identification));
        assert!(tokens[1].is_keyword(Keyword::Division));
    }

    #[test]
    fn test_scan_identifier() {
        let (tokens, errors) = scan_text("MY-VARIABLE");
        assert!(errors.is_empty());
        assert_eq!(tokens.len(), 2);
        assert!(matches!(&tokens[0].kind, TokenKind::Identifier(s) if s == "MY-VARIABLE"));
    }

    #[test]
    fn test_scan_string_literal() {
        let (tokens, errors) = scan_text("\"HELLO WORLD\"");
        assert!(errors.is_empty());
        assert_eq!(tokens.len(), 2);
        assert!(matches!(&tokens[0].kind, TokenKind::StringLiteral(s) if s == "HELLO WORLD"));
    }

    #[test]
    fn test_scan_integer() {
        let (tokens, errors) = scan_text("12345");
        assert!(errors.is_empty());
        assert!(matches!(tokens[0].kind, TokenKind::IntegerLiteral(12345)));
    }

    #[test]
    fn test_scan_decimal() {
        let (tokens, errors) = scan_text("123.45");
        assert!(errors.is_empty());
        assert!(matches!(&tokens[0].kind, TokenKind::DecimalLiteral(s) if s == "123.45"));
    }

    #[test]
    fn test_scan_operators() {
        let (tokens, errors) = scan_text("+ - * / ** = > < >= <= <>");
        assert!(errors.is_empty());
        assert_eq!(tokens[0].kind, TokenKind::Plus);
        assert_eq!(tokens[1].kind, TokenKind::Minus);
        assert_eq!(tokens[2].kind, TokenKind::Star);
        assert_eq!(tokens[3].kind, TokenKind::Slash);
        assert_eq!(tokens[4].kind, TokenKind::DoubleStar);
        assert_eq!(tokens[5].kind, TokenKind::Equals);
        assert_eq!(tokens[6].kind, TokenKind::GreaterThan);
        assert_eq!(tokens[7].kind, TokenKind::LessThan);
        assert_eq!(tokens[8].kind, TokenKind::GreaterEquals);
        assert_eq!(tokens[9].kind, TokenKind::LessEquals);
        assert_eq!(tokens[10].kind, TokenKind::NotEquals);
    }

    #[test]
    fn test_scan_punctuation() {
        let (tokens, errors) = scan_text(". , ; : ( )");
        assert!(errors.is_empty());
        assert_eq!(tokens[0].kind, TokenKind::Period);
        assert_eq!(tokens[1].kind, TokenKind::Comma);
        assert_eq!(tokens[2].kind, TokenKind::Semicolon);
        assert_eq!(tokens[3].kind, TokenKind::Colon);
        assert_eq!(tokens[4].kind, TokenKind::LeftParen);
        assert_eq!(tokens[5].kind, TokenKind::RightParen);
    }

    #[test]
    fn test_scan_picture() {
        let (tokens, errors) = scan_text("PIC X(10)");
        assert!(errors.is_empty());
        assert!(tokens[0].is_keyword(Keyword::Pic));
        assert!(matches!(&tokens[1].kind, TokenKind::PictureString(s) if s == "X(10)"));
    }

    #[test]
    fn test_scan_picture_is() {
        let (tokens, errors) = scan_text("PICTURE IS 9(5)V99");
        assert!(errors.is_empty());
        assert!(tokens[0].is_keyword(Keyword::Picture));
        assert!(tokens[1].is_keyword(Keyword::Is));
        assert!(matches!(&tokens[2].kind, TokenKind::PictureString(s) if s == "9(5)V99"));
    }

    #[test]
    fn test_scan_hex_literal() {
        let (tokens, errors) = scan_text("X\"FF00\"");
        assert!(errors.is_empty());
        assert!(matches!(&tokens[0].kind, TokenKind::HexLiteral(s) if s == "FF00"));
    }

    #[test]
    fn test_scan_escaped_quote() {
        let (tokens, errors) = scan_text("\"He said \"\"Hello\"\"\"");
        assert!(errors.is_empty());
        assert!(matches!(&tokens[0].kind, TokenKind::StringLiteral(s) if s == "He said \"Hello\""));
    }

    #[test]
    fn test_scan_move_statement() {
        let (tokens, errors) = scan_text("MOVE 'A' TO WS-FIELD.");
        assert!(errors.is_empty());
        assert!(tokens[0].is_keyword(Keyword::Move));
        assert!(matches!(&tokens[1].kind, TokenKind::StringLiteral(s) if s == "A"));
        assert!(tokens[2].is_keyword(Keyword::To));
        assert!(matches!(&tokens[3].kind, TokenKind::Identifier(s) if s == "WS-FIELD"));
        assert_eq!(tokens[4].kind, TokenKind::Period);
    }
}
