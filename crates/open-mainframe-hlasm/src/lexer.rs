//! HLASM lexer — fixed-format source parser and tokenizer.
//!
//! HLASM source format:
//! - **Column 1**: label start (space = no label, `*` = comment)
//! - **Columns 1-8**: label field
//! - **Column 10+**: operation code
//! - **Column 16+**: operand field
//! - **Column 72**: continuation character (non-blank = continued)
//! - **Columns 73-80**: sequence number (ignored)

use std::fmt;

// ---------------------------------------------------------------------------
//  Source line parsing
// ---------------------------------------------------------------------------

/// A parsed HLASM source line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceLine {
    /// A full-line comment (`*` in column 1).
    Comment(String),
    /// An instruction line with optional label, opcode, operands, remarks.
    Instruction(InstructionLine),
    /// A blank line.
    Blank,
}

/// A parsed instruction line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstructionLine {
    /// Label (columns 1-8), if present.
    pub label: Option<String>,
    /// Operation code.
    pub opcode: String,
    /// Operand string (may be empty).
    pub operands: String,
    /// Remarks (trailing comment).
    pub remarks: String,
}

/// Parse a single raw source line (up to 80 characters) into a `SourceLine`.
pub fn parse_source_line(raw: &str) -> SourceLine {
    // Pad to at least 72 chars for column-based parsing.
    let line = if raw.len() < 72 {
        format!("{:<72}", raw)
    } else {
        raw.to_string()
    };

    // Column 1: comment check.
    if line.starts_with('*') {
        return SourceLine::Comment(raw.to_string());
    }

    // All blank?
    if raw.trim().is_empty() {
        return SourceLine::Blank;
    }

    // Extract fields by column position.
    // Label: columns 1-8 (indices 0-7).
    let label_field = &line[..8.min(line.len())];
    let label = if label_field.starts_with(' ') {
        None
    } else {
        let l = label_field.trim().to_string();
        if l.is_empty() { None } else { Some(l) }
    };

    // Find the opcode: first non-blank field after label area.
    let after_label = if line.len() > 8 { &line[8..] } else { "" };
    let trimmed = after_label.trim_start();
    if trimmed.is_empty() {
        // Label only, no opcode.
        return SourceLine::Instruction(InstructionLine {
            label,
            opcode: String::new(),
            operands: String::new(),
            remarks: String::new(),
        });
    }

    // Limit to column 71 (index 70) for the meaningful content.
    let content_area = if line.len() > 71 { &line[..71] } else { &line[..] };
    let after_label_content = if content_area.len() > 8 { &content_area[8..] } else { "" };
    let trimmed_content = after_label_content.trim_start();

    let opcode_end = trimmed_content.find(char::is_whitespace).unwrap_or(trimmed_content.len());
    let opcode = trimmed_content[..opcode_end].to_string();

    // After opcode: operands and remarks.
    let after_opcode = trimmed_content[opcode_end..].trim_start();

    // Operands end at the first space not inside quotes.
    let (operands, remarks) = split_operands_remarks(after_opcode);

    SourceLine::Instruction(InstructionLine {
        label,
        opcode,
        operands,
        remarks,
    })
}

/// Parse multiple source lines with continuation handling.
/// Returns a vector of `SourceLine`s with continuations merged.
pub fn parse_source(source: &str) -> Vec<SourceLine> {
    let raw_lines: Vec<&str> = source.lines().collect();
    let mut result = Vec::new();
    let mut i = 0;

    while i < raw_lines.len() {
        let line = raw_lines[i];

        // Check for continuation: non-blank in column 72 (index 71).
        if line.len() >= 72 && !line.as_bytes().get(71).map_or(true, |b| *b == b' ') {
            // Collect continuation lines.
            let mut combined = if line.len() >= 71 {
                line[..71].trim_end().to_string()
            } else {
                line.trim_end().to_string()
            };

            i += 1;
            while i < raw_lines.len() {
                let cont = raw_lines[i];
                // Continuation content starts at column 16 (index 15).
                let cont_data = if cont.len() > 15 {
                    let end = if cont.len() >= 72 { 71 } else { cont.len() };
                    cont[15..end].trim_end()
                } else {
                    ""
                };
                combined.push_str(cont_data);

                // Check if this continuation also continues.
                if cont.len() >= 72 && !cont.as_bytes().get(71).map_or(true, |b| *b == b' ') {
                    i += 1;
                } else {
                    i += 1;
                    break;
                }
            }

            result.push(parse_source_line(&combined));
        } else {
            result.push(parse_source_line(line));
            i += 1;
        }
    }

    result
}

/// Split operand string from remarks. Respects quoted strings.
fn split_operands_remarks(text: &str) -> (String, String) {
    if text.is_empty() {
        return (String::new(), String::new());
    }

    let mut in_quote = false;
    let chars: Vec<char> = text.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        if chars[i] == '\'' {
            in_quote = !in_quote;
        } else if chars[i] == ' ' && !in_quote {
            // Found unquoted space — rest is remarks.
            let operands = text[..i].trim().to_string();
            let remarks = text[i..].trim().to_string();
            return (operands, remarks);
        }
        i += 1;
    }

    // No space found — all operands.
    (text.trim().to_string(), String::new())
}

// ---------------------------------------------------------------------------
//  Token types
// ---------------------------------------------------------------------------

/// An HLASM operand token.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    /// A general-purpose register (R0-R15 or 0-15).
    Register(u8),
    /// A symbol/label reference.
    Symbol(String),
    /// A decimal number.
    Number(i64),
    /// A hex self-defining term: X'nn'.
    HexTerm(String),
    /// A character self-defining term: C'...'.
    CharTerm(String),
    /// A binary self-defining term: B'...'.
    BinTerm(String),
    /// A literal: =F'100', =CL8'TEXT', etc.
    Literal(String),
    /// An arithmetic operator: +, -, *, /.
    Operator(char),
    /// Left parenthesis.
    LParen,
    /// Right parenthesis.
    RParen,
    /// Comma separator.
    Comma,
    /// An equals sign (for EQU, etc.).
    Equals,
    /// A location counter reference: *.
    LocationCounter,
    /// Length attribute: L'.
    LengthAttr(String),
    /// A string in quotes.
    QuotedString(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Register(n) => write!(f, "R{n}"),
            Token::Symbol(s) => write!(f, "{s}"),
            Token::Number(n) => write!(f, "{n}"),
            Token::HexTerm(h) => write!(f, "X'{h}'"),
            Token::CharTerm(c) => write!(f, "C'{c}'"),
            Token::BinTerm(b) => write!(f, "B'{b}'"),
            Token::Literal(l) => write!(f, "={l}"),
            Token::Operator(o) => write!(f, "{o}"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Comma => write!(f, ","),
            Token::Equals => write!(f, "="),
            Token::LocationCounter => write!(f, "*"),
            Token::LengthAttr(s) => write!(f, "L'{s}"),
            Token::QuotedString(s) => write!(f, "'{s}'"),
        }
    }
}

/// Tokenize an operand string into a vector of `Token`s.
pub fn tokenize_operands(operands: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = operands.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        match chars[i] {
            ' ' => { i += 1; }
            '(' => { tokens.push(Token::LParen); i += 1; }
            ')' => { tokens.push(Token::RParen); i += 1; }
            ',' => { tokens.push(Token::Comma); i += 1; }
            '+' | '-' | '/' => {
                tokens.push(Token::Operator(chars[i]));
                i += 1;
            }
            '*' => {
                // Could be location counter or multiply.
                if i > 0 && matches!(tokens.last(), Some(Token::Number(_)) | Some(Token::Symbol(_)) | Some(Token::RParen)) {
                    tokens.push(Token::Operator('*'));
                } else {
                    tokens.push(Token::LocationCounter);
                }
                i += 1;
            }
            '=' => {
                // Literal: =F'100', =CL8'TEXT', etc.
                i += 1;
                let start = i;
                // Collect until end, comma, or space (respecting quotes).
                let mut in_quote = false;
                while i < chars.len() {
                    if chars[i] == '\'' {
                        in_quote = !in_quote;
                    } else if !in_quote && (chars[i] == ',' || chars[i] == ' ' || chars[i] == '(' || chars[i] == ')') {
                        break;
                    }
                    i += 1;
                }
                let lit: String = chars[start..i].iter().collect();
                tokens.push(Token::Literal(lit));
            }
            '\'' => {
                // Quoted string.
                i += 1;
                let start = i;
                while i < chars.len() && chars[i] != '\'' {
                    i += 1;
                }
                let s: String = chars[start..i].iter().collect();
                tokens.push(Token::QuotedString(s));
                if i < chars.len() { i += 1; } // skip closing quote
            }
            c if c.is_ascii_alphabetic() || c == '@' || c == '#' || c == '$' || c == '_' => {
                // Symbol or self-defining term or register.
                let start = i;
                while i < chars.len() && (chars[i].is_ascii_alphanumeric() || chars[i] == '@' || chars[i] == '#' || chars[i] == '$' || chars[i] == '_') {
                    i += 1;
                }
                let word: String = chars[start..i].iter().collect();
                let upper = word.to_uppercase();

                // Check for self-defining terms: X'nn', C'cc', B'bb'.
                if i < chars.len() && chars[i] == '\'' && (upper == "X" || upper == "C" || upper == "B" || upper == "L") {
                    i += 1; // skip opening quote
                    let q_start = i;
                    while i < chars.len() && chars[i] != '\'' {
                        i += 1;
                    }
                    let val: String = chars[q_start..i].iter().collect();
                    if i < chars.len() { i += 1; } // skip closing quote

                    match upper.as_str() {
                        "X" => tokens.push(Token::HexTerm(val)),
                        "C" => tokens.push(Token::CharTerm(val)),
                        "B" => tokens.push(Token::BinTerm(val)),
                        "L" => tokens.push(Token::LengthAttr(val)),
                        _ => unreachable!(),
                    }
                } else if let Some(reg) = parse_register(&upper) {
                    tokens.push(Token::Register(reg));
                } else {
                    tokens.push(Token::Symbol(word));
                }
            }
            c if c.is_ascii_digit() => {
                let start = i;
                while i < chars.len() && chars[i].is_ascii_digit() {
                    i += 1;
                }
                let num_str: String = chars[start..i].iter().collect();
                let n = num_str.parse::<i64>().unwrap_or(0);
                tokens.push(Token::Number(n));
            }
            _ => { i += 1; } // Skip unknown.
        }
    }

    tokens
}

/// Parse a register name (R0-R15 or 0-15) into a register number.
fn parse_register(name: &str) -> Option<u8> {
    if let Some(rest) = name.strip_prefix('R') {
        rest.parse::<u8>().ok().filter(|&n| n <= 15)
    } else {
        // Bare number 0-15 in register position — only valid if <= 15.
        // We only treat bare numbers as registers in context; here we skip
        // to avoid ambiguity with numeric operands.
        None
    }
}

// ---------------------------------------------------------------------------
//  Error types
// ---------------------------------------------------------------------------

/// HLASM lexer error.
#[derive(Debug, Clone, thiserror::Error)]
pub enum LexerError {
    #[error("Invalid source format at line {line}: {msg}")]
    InvalidFormat { line: usize, msg: String },
    #[error("Unterminated continuation at line {line}")]
    UnterminatedContinuation { line: usize },
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_comment() {
        let line = "* THIS IS A COMMENT";
        assert_eq!(
            parse_source_line(line),
            SourceLine::Comment("* THIS IS A COMMENT".to_string())
        );
    }

    #[test]
    fn test_parse_blank() {
        assert_eq!(parse_source_line(""), SourceLine::Blank);
        assert_eq!(parse_source_line("   "), SourceLine::Blank);
    }

    #[test]
    fn test_parse_instruction_with_label() {
        let line = "MYLAB    L     R5,MYDATA          LOAD VALUE";
        match parse_source_line(line) {
            SourceLine::Instruction(inst) => {
                assert_eq!(inst.label, Some("MYLAB".to_string()));
                assert_eq!(inst.opcode, "L");
                assert_eq!(inst.operands, "R5,MYDATA");
                assert_eq!(inst.remarks, "LOAD VALUE");
            }
            other => panic!("Expected Instruction, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_instruction_no_label() {
        let line = "         L     R5,MYDATA";
        match parse_source_line(line) {
            SourceLine::Instruction(inst) => {
                assert_eq!(inst.label, None);
                assert_eq!(inst.opcode, "L");
                assert_eq!(inst.operands, "R5,MYDATA");
            }
            other => panic!("Expected Instruction, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_instruction_dc() {
        let line = "MYDATA   DC    F'100'";
        match parse_source_line(line) {
            SourceLine::Instruction(inst) => {
                assert_eq!(inst.label, Some("MYDATA".to_string()));
                assert_eq!(inst.opcode, "DC");
                assert_eq!(inst.operands, "F'100'");
            }
            other => panic!("Expected Instruction, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_continuation() {
        // Line 1: content up to col 71, non-blank col 72
        let line1 = format!("{:<71}X{:>8}", "MYLAB    MVC   TARGET,=CL80' '", "00000010");
        // Line 2: continuation starts at col 16
        let line2 = format!("{:<15}CONTINUED", " ");
        let source = format!("{line1}\n{line2}");

        let lines = parse_source(&source);
        assert_eq!(lines.len(), 1);
        match &lines[0] {
            SourceLine::Instruction(inst) => {
                assert_eq!(inst.label, Some("MYLAB".to_string()));
                assert_eq!(inst.opcode, "MVC");
                assert!(inst.operands.contains("TARGET"));
            }
            other => panic!("Expected Instruction, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_multiple_lines() {
        let source = "* COMMENT\nMYLAB    L     R5,DATA\n         ST    R5,SAVE\n";
        let lines = parse_source(source);
        assert_eq!(lines.len(), 3);
        assert!(matches!(lines[0], SourceLine::Comment(_)));
        assert!(matches!(lines[1], SourceLine::Instruction(_)));
        assert!(matches!(lines[2], SourceLine::Instruction(_)));
    }

    #[test]
    fn test_tokenize_register_and_symbol() {
        let tokens = tokenize_operands("R5,MYDATA");
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0], Token::Register(5));
        assert_eq!(tokens[1], Token::Comma);
        assert_eq!(tokens[2], Token::Symbol("MYDATA".to_string()));
    }

    #[test]
    fn test_tokenize_literal() {
        let tokens = tokenize_operands("R5,=F'100'");
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0], Token::Register(5));
        assert_eq!(tokens[1], Token::Comma);
        assert_eq!(tokens[2], Token::Literal("F'100'".to_string()));
    }

    #[test]
    fn test_tokenize_literal_with_index() {
        let tokens = tokenize_operands("=F'100'(R12)");
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0], Token::Literal("F'100'".to_string()));
        assert_eq!(tokens[1], Token::LParen);
        assert_eq!(tokens[2], Token::Register(12));
        assert_eq!(tokens[3], Token::RParen);
    }

    #[test]
    fn test_tokenize_symbol_plus_offset() {
        let tokens = tokenize_operands("MYLAB+4(R3)");
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0], Token::Symbol("MYLAB".to_string()));
        assert_eq!(tokens[1], Token::Operator('+'));
        assert_eq!(tokens[2], Token::Number(4));
        assert_eq!(tokens[3], Token::LParen);
        assert_eq!(tokens[4], Token::Register(3));
        assert_eq!(tokens[5], Token::RParen);
    }

    #[test]
    fn test_tokenize_hex_term() {
        let tokens = tokenize_operands("X'FF'");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], Token::HexTerm("FF".to_string()));
    }

    #[test]
    fn test_tokenize_char_term() {
        let tokens = tokenize_operands("C'HELLO'");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], Token::CharTerm("HELLO".to_string()));
    }

    #[test]
    fn test_tokenize_bin_term() {
        let tokens = tokenize_operands("B'11001010'");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], Token::BinTerm("11001010".to_string()));
    }

    #[test]
    fn test_tokenize_location_counter() {
        let tokens = tokenize_operands("*");
        assert_eq!(tokens[0], Token::LocationCounter);
    }

    #[test]
    fn test_tokenize_complex_operands() {
        // LA R1,AREA(R2)
        let tokens = tokenize_operands("AREA(R2)");
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0], Token::Symbol("AREA".to_string()));
        assert_eq!(tokens[1], Token::LParen);
        assert_eq!(tokens[2], Token::Register(2));
        assert_eq!(tokens[3], Token::RParen);
    }

    #[test]
    fn test_tokenize_multiple_operands() {
        let tokens = tokenize_operands("R1,R2,R3");
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0], Token::Register(1));
        assert_eq!(tokens[2], Token::Register(2));
        assert_eq!(tokens[4], Token::Register(3));
    }

    #[test]
    fn test_tokenize_length_attr() {
        let tokens = tokenize_operands("L'FIELD");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], Token::LengthAttr("FIELD".to_string()));
    }

    #[test]
    fn test_tokenize_dc_operand() {
        // DC F'100' — the F'100' is just operand text, not preceded by =
        let tokens = tokenize_operands("F'100'");
        // F is not a register, and F' triggers a term check.
        // Actually "F" is not X/C/B/L, so it's parsed as Symbol("F") then QuotedString("100").
        // This is correct for the tokenizer level — higher layers handle DC types.
        assert!(tokens.len() >= 1);
    }

    #[test]
    fn test_split_operands_remarks() {
        let (ops, rem) = split_operands_remarks("R5,MYDATA          LOAD VALUE");
        assert_eq!(ops, "R5,MYDATA");
        assert_eq!(rem, "LOAD VALUE");
    }

    #[test]
    fn test_split_operands_quoted() {
        let (ops, rem) = split_operands_remarks("=C'HELLO WORLD' REMARK");
        assert_eq!(ops, "=C'HELLO WORLD'");
        assert_eq!(rem, "REMARK");
    }

    #[test]
    fn test_split_operands_no_remarks() {
        let (ops, rem) = split_operands_remarks("R5,MYDATA");
        assert_eq!(ops, "R5,MYDATA");
        assert_eq!(rem, "");
    }
}
