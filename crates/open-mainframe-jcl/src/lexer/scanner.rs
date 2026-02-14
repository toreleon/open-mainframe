//! Character-level scanning for JCL operand tokenization.

use super::token::Token;
use crate::error::JclError;

/// Tokenize operands string for further parsing.
pub fn tokenize_operands(operands: &str) -> Result<Vec<Token>, JclError> {
    let mut tokens = Vec::new();
    let mut chars = operands.chars().peekable();

    while let Some(&c) = chars.peek() {
        match c {
            ' ' | '\t' => {
                chars.next();
            }
            '=' => {
                chars.next();
                tokens.push(Token::Equals);
            }
            ',' => {
                chars.next();
                tokens.push(Token::Comma);
            }
            '(' => {
                chars.next();
                tokens.push(Token::LParen);
            }
            ')' => {
                chars.next();
                tokens.push(Token::RParen);
            }
            '.' => {
                chars.next();
                tokens.push(Token::Period);
            }
            '*' => {
                chars.next();
                tokens.push(Token::Asterisk);
            }
            '&' => {
                chars.next();
                // Collect symbolic parameter name
                let mut name = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphanumeric() || c == '@' || c == '#' || c == '$' {
                        name.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Ampersand);
                if !name.is_empty() {
                    tokens.push(Token::Ident(name));
                }
            }
            '\'' => {
                chars.next();
                let mut s = String::new();
                loop {
                    match chars.next() {
                        Some('\'') => {
                            if chars.peek() == Some(&'\'') {
                                // Escaped quote
                                s.push('\'');
                                chars.next();
                            } else {
                                break;
                            }
                        }
                        Some(c) => s.push(c),
                        None => {
                            return Err(JclError::ParseError {
                                message: "Unterminated string".to_string(),
                            });
                        }
                    }
                }
                tokens.push(Token::String(s));
            }
            '0'..='9' => {
                let mut num = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() {
                        num.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Number(num.parse().unwrap_or(0)));
            }
            _ if c.is_ascii_alphabetic() || c == '@' || c == '#' || c == '$' => {
                let mut ident = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphanumeric() || c == '@' || c == '#' || c == '$' || c == '-' {
                        ident.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Ident(ident.to_uppercase()));
            }
            _ => {
                chars.next();
            }
        }
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_operands() {
        let tokens = tokenize_operands("PGM=MYPROG,PARM='TEST'").unwrap();

        assert!(matches!(tokens[0], Token::Ident(ref s) if s == "PGM"));
        assert!(matches!(tokens[1], Token::Equals));
        assert!(matches!(tokens[2], Token::Ident(ref s) if s == "MYPROG"));
        assert!(matches!(tokens[3], Token::Comma));
        assert!(matches!(tokens[4], Token::Ident(ref s) if s == "PARM"));
        assert!(matches!(tokens[5], Token::Equals));
        assert!(matches!(tokens[6], Token::String(ref s) if s == "TEST"));
    }
}
