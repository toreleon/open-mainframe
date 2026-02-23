//! EZ-100: Lexer & Parser for Easytrieve Plus.
//!
//! Handles fixed-column format source (positions 1-4 line number, 5-72 statement,
//! 73-80 sequence) and produces a structured AST of Easytrieve statements.

use std::fmt;

use miette::Diagnostic;
use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

/// Errors produced during lexing or parsing.
#[derive(Debug, Error, Diagnostic)]
pub enum ParseError {
    /// Unexpected character encountered during lexing.
    #[error("unexpected character '{ch}' at line {line}, column {col}")]
    UnexpectedChar {
        /// The offending character.
        ch: char,
        /// Source line number.
        line: usize,
        /// Column within the statement area.
        col: usize,
    },
    /// Unterminated string literal.
    #[error("unterminated string literal at line {line}")]
    UnterminatedString {
        /// Source line number.
        line: usize,
    },
    /// Unexpected token during parsing.
    #[error("unexpected token {token} at line {line}")]
    UnexpectedToken {
        /// The token text.
        token: String,
        /// Source line number.
        line: usize,
    },
    /// Missing required operand.
    #[error("missing operand for {statement} at line {line}")]
    MissingOperand {
        /// The statement that requires an operand.
        statement: String,
        /// Source line number.
        line: usize,
    },
}

// ---------------------------------------------------------------------------
// Tokens
// ---------------------------------------------------------------------------

/// A lexical token from Easytrieve source.
#[derive(Debug, Clone, PartialEq)]
pub enum EzToken {
    /// A reserved keyword (FILE, DEFINE, JOB, etc.).
    Keyword(String),
    /// A user-defined identifier.
    Identifier(String),
    /// An integer or decimal number literal.
    Number(String),
    /// A quoted string literal.
    StringLiteral(String),
    /// An operator (+, -, *, /, =, <, >, etc.).
    Operator(String),
    /// Left parenthesis.
    LeftParen,
    /// Right parenthesis.
    RightParen,
    /// Comma separator.
    Comma,
    /// Period / statement terminator.
    Period,
    /// End of a source line.
    EndOfLine,
}

impl fmt::Display for EzToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Keyword(k) => write!(f, "Keyword({k})"),
            Self::Identifier(id) => write!(f, "Identifier({id})"),
            Self::Number(n) => write!(f, "Number({n})"),
            Self::StringLiteral(s) => write!(f, "String(\"{s}\")"),
            Self::Operator(op) => write!(f, "Operator({op})"),
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::Comma => write!(f, ","),
            Self::Period => write!(f, "."),
            Self::EndOfLine => write!(f, "EOL"),
        }
    }
}

/// Known Easytrieve keywords.
const KEYWORDS: &[&str] = &[
    "FILE", "DEFINE", "JOB", "SORT", "PUT", "GET", "PRINT", "DISPLAY",
    "HEADING", "LINE", "TITLE", "IF", "ELSE", "END-IF", "DO", "END-DO",
    "GOTO", "GO", "PERFORM", "STOP", "MACRO", "END-MACRO", "COPY", "SQL",
    "END-SQL", "END", "INPUT", "OUTPUT", "REPORT", "SEQUENCE", "CONTROL",
    "SUM", "COUNT", "AVG", "MIN", "MAX", "AND", "OR", "NOT", "EQ", "NE",
    "GT", "GE", "LT", "LE", "TO", "THRU", "BY", "WHILE", "UNTIL",
    "BEFORE", "AFTER", "MOVE", "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE",
    "W", "N", "P", "B", "A", "MATCHED", "SYNCHRONIZED", "CALL",
];

/// Check whether an identifier is a keyword.
fn is_keyword(s: &str) -> bool {
    KEYWORDS.contains(&s.to_uppercase().as_str())
}

// ---------------------------------------------------------------------------
// Lexer
// ---------------------------------------------------------------------------

/// Tokenize a single statement text (columns 5-72 content).
fn tokenize_statement(text: &str, line: usize) -> Result<Vec<EzToken>, ParseError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = text.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let ch = chars[i];

        // Skip whitespace
        if ch.is_ascii_whitespace() {
            i += 1;
            continue;
        }

        // Comment: asterisk in column 1 of the statement area (position 5 of the line = index 0 here)
        if ch == '*' && i == 0 {
            // Entire line is a comment, skip
            break;
        }

        // String literals
        if ch == '\'' {
            let start = i;
            i += 1;
            let mut s = String::new();
            loop {
                if i >= chars.len() {
                    return Err(ParseError::UnterminatedString { line });
                }
                if chars[i] == '\'' {
                    // Check for doubled quote (escape)
                    if i + 1 < chars.len() && chars[i + 1] == '\'' {
                        s.push('\'');
                        i += 2;
                    } else {
                        i += 1;
                        break;
                    }
                } else {
                    s.push(chars[i]);
                    i += 1;
                }
            }
            let _ = start; // suppress unused warning
            tokens.push(EzToken::StringLiteral(s));
            continue;
        }

        // Numbers
        if ch.is_ascii_digit() {
            let mut num = String::new();
            while i < chars.len() && (chars[i].is_ascii_digit() || chars[i] == '.') {
                num.push(chars[i]);
                i += 1;
            }
            tokens.push(EzToken::Number(num));
            continue;
        }

        // Identifiers / keywords
        if ch.is_ascii_alphabetic() || ch == '#' || ch == '@' || ch == '$' || ch == '_' {
            let mut ident = String::new();
            while i < chars.len()
                && (chars[i].is_ascii_alphanumeric()
                    || chars[i] == '-'
                    || chars[i] == '_'
                    || chars[i] == '#'
                    || chars[i] == '@'
                    || chars[i] == '$')
            {
                ident.push(chars[i]);
                i += 1;
            }
            let upper = ident.to_uppercase();
            if is_keyword(&upper) {
                tokens.push(EzToken::Keyword(upper));
            } else {
                tokens.push(EzToken::Identifier(ident));
            }
            continue;
        }

        // Parentheses, comma, period
        match ch {
            '(' => {
                tokens.push(EzToken::LeftParen);
                i += 1;
                continue;
            }
            ')' => {
                tokens.push(EzToken::RightParen);
                i += 1;
                continue;
            }
            ',' => {
                tokens.push(EzToken::Comma);
                i += 1;
                continue;
            }
            '.' => {
                tokens.push(EzToken::Period);
                i += 1;
                continue;
            }
            _ => {}
        }

        // Operators
        if matches!(ch, '+' | '-' | '*' | '/' | '=' | '<' | '>' | ':') {
            let mut op = String::new();
            op.push(ch);
            i += 1;
            // Two-char operators
            if i < chars.len() {
                let next = chars[i];
                if matches!(
                    (ch, next),
                    ('<', '=') | ('>', '=') | ('<', '>') | (':', '=')
                ) {
                    op.push(next);
                    i += 1;
                }
            }
            tokens.push(EzToken::Operator(op));
            continue;
        }

        return Err(ParseError::UnexpectedChar {
            ch,
            line,
            col: i + 5,
        });
    }

    tokens.push(EzToken::EndOfLine);
    Ok(tokens)
}

// ---------------------------------------------------------------------------
// AST types
// ---------------------------------------------------------------------------

/// Parsed Easytrieve statement kinds.
#[derive(Debug, Clone, PartialEq)]
pub enum EzStatement {
    /// FILE declaration with optional attributes.
    File {
        /// File name identifier.
        name: String,
        /// Keyword-value attributes (e.g., LRECL, RECFM).
        attrs: Vec<(String, String)>,
    },
    /// DEFINE field within a file or working storage.
    Define {
        /// Field name.
        name: String,
        /// Owning file (None = working storage).
        file: Option<String>,
        /// Start position (1-based).
        position: Option<u32>,
        /// Field length.
        length: Option<u32>,
        /// Data type (A=alpha, N=numeric, P=packed, B=binary, W=working).
        data_type: Option<String>,
        /// Optional initial value.
        value: Option<String>,
    },
    /// JOB statement with optional INPUT file.
    Job {
        /// Job name (optional).
        name: Option<String>,
        /// INPUT file name.
        input: Option<String>,
    },
    /// SORT statement.
    Sort {
        /// File to sort.
        file: String,
        /// Sort keys with optional direction.
        keys: Vec<(String, SortDirection)>,
    },
    /// PUT record to output file.
    Put {
        /// Target file.
        file: Option<String>,
        /// Optional MATCHED keyword.
        matched: bool,
    },
    /// GET record from input file.
    Get {
        /// Source file.
        file: String,
    },
    /// PRINT report output.
    Print {
        /// Report name.
        report: Option<String>,
    },
    /// DISPLAY expression list.
    Display {
        /// Items to display.
        items: Vec<String>,
    },
    /// HEADING report heading definition.
    Heading {
        /// Heading number.
        number: Option<u32>,
        /// Content items.
        items: Vec<String>,
    },
    /// LINE detail line definition.
    Line {
        /// Line number.
        number: Option<u32>,
        /// Content items.
        items: Vec<String>,
    },
    /// TITLE page title definition.
    Title {
        /// Title number.
        number: Option<u32>,
        /// Content items.
        items: Vec<String>,
    },
    /// IF conditional.
    If {
        /// Condition expression tokens.
        condition: Vec<String>,
    },
    /// ELSE branch.
    Else,
    /// END-IF terminator.
    EndIf,
    /// DO loop start.
    Do {
        /// Optional WHILE/UNTIL condition.
        condition: Option<Vec<String>>,
    },
    /// END-DO loop end.
    EndDo,
    /// GOTO label transfer.
    GoTo {
        /// Target label.
        label: String,
    },
    /// PERFORM paragraph.
    Perform {
        /// Target procedure name.
        procedure: String,
    },
    /// STOP execution.
    Stop,
    /// MACRO definition start.
    Macro {
        /// Macro name.
        name: String,
    },
    /// COPY external member.
    Copy {
        /// Member name.
        member: String,
    },
    /// SQL block.
    Sql {
        /// SQL statement text.
        text: String,
    },
    /// END statement.
    End,
    /// Assignment (MOVE, direct assignment).
    Assignment {
        /// Target variable.
        target: String,
        /// Source expression tokens.
        source: Vec<String>,
    },
    /// Arithmetic operations (ADD, SUBTRACT, MULTIPLY, DIVIDE).
    Arithmetic {
        /// Operation verb.
        op: String,
        /// Operand 1.
        operand1: String,
        /// TO/FROM keyword + operand 2.
        operand2: String,
    },
    /// REPORT declaration.
    Report {
        /// Report name.
        name: String,
        /// Attributes.
        attrs: Vec<(String, String)>,
    },
    /// SEQUENCE declaration.
    Sequence {
        /// File name.
        file: String,
        /// Sequence fields.
        fields: Vec<String>,
    },
    /// CONTROL declaration.
    Control {
        /// File or report name.
        target: String,
        /// Control break fields.
        fields: Vec<String>,
    },
    /// SUM declaration.
    Sum {
        /// Fields to accumulate.
        fields: Vec<String>,
    },
    /// CALL external program.
    Call {
        /// Program name.
        program: String,
        /// Parameters.
        params: Vec<String>,
    },
    /// Label definition (identifier followed by a period at statement start).
    Label {
        /// Label name.
        name: String,
    },
    /// Comment line.
    Comment {
        /// Comment text.
        text: String,
    },
}

/// Sort direction for SORT keys.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SortDirection {
    /// Ascending order.
    Ascending,
    /// Descending order.
    Descending,
}

/// A fully parsed Easytrieve program.
#[derive(Debug, Clone)]
pub struct EzProgram {
    /// File declarations.
    pub files: Vec<EzStatement>,
    /// Field definitions.
    pub defines: Vec<EzStatement>,
    /// Job activities (all non-file/define statements under JOB).
    pub activities: Vec<EzStatement>,
    /// All statements in order.
    pub statements: Vec<EzStatement>,
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

/// Easytrieve source parser.
///
/// Parses fixed-column format Easytrieve source into [`EzProgram`].
pub struct EzParser;

impl EzParser {
    /// Create a new parser instance.
    pub fn new() -> Self {
        Self
    }

    /// Extract the statement area from a source line (columns 5-72).
    ///
    /// Lines shorter than 5 characters are treated as blank.
    fn extract_statement(line: &str) -> &str {
        if line.len() <= 4 {
            return "";
        }
        let end = std::cmp::min(line.len(), 72);
        line[4..end].trim_end()
    }

    /// Tokenize entire source into line-grouped token lists.
    pub fn tokenize(source: &str) -> Result<Vec<Vec<EzToken>>, ParseError> {
        let mut result = Vec::new();
        for (line_num, line) in source.lines().enumerate() {
            let stmt = Self::extract_statement(line);
            if stmt.is_empty() {
                continue;
            }
            let tokens = tokenize_statement(stmt, line_num + 1)?;
            if tokens.len() > 1 {
                // More than just EndOfLine
                result.push(tokens);
            }
        }
        Ok(result)
    }

    /// Parse Easytrieve source text into an [`EzProgram`].
    pub fn parse(source: &str) -> Result<EzProgram, ParseError> {
        let token_lines = Self::tokenize(source)?;
        let mut files = Vec::new();
        let mut defines = Vec::new();
        let mut activities = Vec::new();
        let mut statements = Vec::new();

        for (line_idx, tokens) in token_lines.iter().enumerate() {
            let line_num = line_idx + 1;
            let stmt = Self::parse_statement(tokens, line_num)?;
            match &stmt {
                EzStatement::File { .. } => files.push(stmt.clone()),
                EzStatement::Define { .. } => defines.push(stmt.clone()),
                EzStatement::Comment { .. } => { /* skip comments in categorized lists */ }
                _ => activities.push(stmt.clone()),
            }
            statements.push(stmt);
        }

        Ok(EzProgram {
            files,
            defines,
            activities,
            statements,
        })
    }

    /// Parse a single token line into a statement.
    fn parse_statement(tokens: &[EzToken], line: usize) -> Result<EzStatement, ParseError> {
        if tokens.is_empty() {
            return Ok(EzStatement::Comment {
                text: String::new(),
            });
        }

        match &tokens[0] {
            EzToken::Keyword(kw) => Self::parse_keyword_statement(kw, tokens, line),
            EzToken::Identifier(id) => {
                // Could be a label or assignment
                if tokens.len() >= 2 {
                    match &tokens[1] {
                        EzToken::Period => Ok(EzStatement::Label {
                            name: id.clone(),
                        }),
                        EzToken::Operator(op) if op == "=" || op == ":=" => {
                            let source = Self::collect_remaining_text(&tokens[2..]);
                            Ok(EzStatement::Assignment {
                                target: id.clone(),
                                source,
                            })
                        }
                        _ => {
                            // Treat as assignment if next token is identifier or number
                            let source = Self::collect_remaining_text(&tokens[1..]);
                            Ok(EzStatement::Assignment {
                                target: id.clone(),
                                source,
                            })
                        }
                    }
                } else {
                    Ok(EzStatement::Label {
                        name: id.clone(),
                    })
                }
            }
            _ => Ok(EzStatement::Comment {
                text: format!("unrecognized line {line}"),
            }),
        }
    }

    /// Parse a statement that begins with a keyword.
    fn parse_keyword_statement(
        kw: &str,
        tokens: &[EzToken],
        line: usize,
    ) -> Result<EzStatement, ParseError> {
        match kw {
            "FILE" => Self::parse_file(tokens, line),
            "DEFINE" => Self::parse_define(tokens, line),
            "JOB" => Self::parse_job(tokens),
            "SORT" => Self::parse_sort(tokens, line),
            "PUT" => Self::parse_put(tokens),
            "GET" => Self::parse_get(tokens, line),
            "PRINT" => Self::parse_print(tokens),
            "DISPLAY" => Ok(EzStatement::Display {
                items: Self::collect_remaining_text(&tokens[1..]),
            }),
            "HEADING" => Self::parse_heading_line_title(tokens, "HEADING"),
            "LINE" => Self::parse_heading_line_title(tokens, "LINE"),
            "TITLE" => Self::parse_heading_line_title(tokens, "TITLE"),
            "IF" => Ok(EzStatement::If {
                condition: Self::collect_remaining_text(&tokens[1..]),
            }),
            "ELSE" => Ok(EzStatement::Else),
            "END-IF" => Ok(EzStatement::EndIf),
            "DO" => Ok(EzStatement::Do {
                condition: if tokens.len() > 2 {
                    Some(Self::collect_remaining_text(&tokens[1..]))
                } else {
                    None
                },
            }),
            "END-DO" => Ok(EzStatement::EndDo),
            "GOTO" | "GO" => {
                let label = Self::get_identifier_at(tokens, 1).unwrap_or_default();
                Ok(EzStatement::GoTo { label })
            }
            "PERFORM" => {
                let procedure = Self::get_identifier_at(tokens, 1).unwrap_or_default();
                Ok(EzStatement::Perform { procedure })
            }
            "STOP" => Ok(EzStatement::Stop),
            "MACRO" => {
                let name = Self::get_identifier_at(tokens, 1).unwrap_or_default();
                Ok(EzStatement::Macro { name })
            }
            "END-MACRO" | "END" => Ok(EzStatement::End),
            "COPY" => {
                let member = Self::get_identifier_at(tokens, 1).unwrap_or_else(|| {
                    Self::get_keyword_at(tokens, 1).unwrap_or_default()
                });
                Ok(EzStatement::Copy { member })
            }
            "SQL" => {
                let text_parts = Self::collect_remaining_text(&tokens[1..]);
                Ok(EzStatement::Sql {
                    text: text_parts.join(" "),
                })
            }
            "END-SQL" => Ok(EzStatement::End),
            "MOVE" => {
                let parts = Self::collect_remaining_text(&tokens[1..]);
                // MOVE source TO target
                let to_idx = parts.iter().position(|s| s.eq_ignore_ascii_case("TO"));
                if let Some(idx) = to_idx {
                    let source = parts[..idx].to_vec();
                    let target = parts.get(idx + 1).cloned().unwrap_or_default();
                    Ok(EzStatement::Assignment { target, source })
                } else if parts.len() >= 2 {
                    Ok(EzStatement::Assignment {
                        target: parts.last().cloned().unwrap_or_default(),
                        source: parts[..parts.len() - 1].to_vec(),
                    })
                } else {
                    Err(ParseError::MissingOperand {
                        statement: "MOVE".into(),
                        line,
                    })
                }
            }
            "ADD" | "SUBTRACT" | "MULTIPLY" | "DIVIDE" => {
                let parts = Self::collect_remaining_text(&tokens[1..]);
                let op = kw.to_string();
                let operand1 = parts.first().cloned().unwrap_or_default();
                let to_idx = parts
                    .iter()
                    .position(|s| s.eq_ignore_ascii_case("TO") || s.eq_ignore_ascii_case("FROM"));
                let operand2 = if let Some(idx) = to_idx {
                    parts.get(idx + 1).cloned().unwrap_or_default()
                } else {
                    parts.get(1).cloned().unwrap_or_default()
                };
                Ok(EzStatement::Arithmetic {
                    op,
                    operand1,
                    operand2,
                })
            }
            "REPORT" => {
                let name = Self::get_identifier_at(tokens, 1).unwrap_or_default();
                let attrs = Self::parse_keyword_value_pairs(&tokens[2..]);
                Ok(EzStatement::Report { name, attrs })
            }
            "SEQUENCE" => {
                let file = Self::get_identifier_at(tokens, 1).unwrap_or_default();
                let fields = Self::collect_remaining_text(&tokens[2..]);
                Ok(EzStatement::Sequence { file, fields })
            }
            "CONTROL" => {
                let target = Self::get_identifier_at(tokens, 1).unwrap_or_default();
                let fields = Self::collect_remaining_text(&tokens[2..]);
                Ok(EzStatement::Control { target, fields })
            }
            "SUM" => Ok(EzStatement::Sum {
                fields: Self::collect_remaining_text(&tokens[1..]),
            }),
            "CALL" => {
                let program = Self::get_identifier_at(tokens, 1).unwrap_or_default();
                let params = Self::collect_remaining_text(&tokens[2..]);
                Ok(EzStatement::Call { program, params })
            }
            "INPUT" => {
                let file = Self::get_identifier_at(tokens, 1).unwrap_or_default();
                Ok(EzStatement::Get { file })
            }
            "BEFORE" | "AFTER" => {
                // BEFORE/AFTER are control break triggers — store as labels
                let text = Self::collect_remaining_text(tokens);
                Ok(EzStatement::Label {
                    name: text.join(" "),
                })
            }
            _ => {
                // Unknown keyword — treat as identifier-like assignment
                let kw_str = kw.to_string();
                let source = Self::collect_remaining_text(&tokens[1..]);
                Ok(EzStatement::Assignment {
                    target: kw_str,
                    source,
                })
            }
        }
    }

    /// Parse FILE statement.
    fn parse_file(tokens: &[EzToken], _line: usize) -> Result<EzStatement, ParseError> {
        let name = Self::get_identifier_at(tokens, 1).unwrap_or_default();
        let attrs = Self::parse_keyword_value_pairs(&tokens[2..]);
        Ok(EzStatement::File { name, attrs })
    }

    /// Parse DEFINE statement.
    fn parse_define(tokens: &[EzToken], _line: usize) -> Result<EzStatement, ParseError> {
        let name = Self::get_identifier_at(tokens, 1).unwrap_or_default();
        let mut file = None;
        let mut position = None;
        let mut length = None;
        let mut data_type = None;
        let mut value = None;

        let remaining = Self::collect_remaining_text(&tokens[2..]);
        let mut i = 0;
        while i < remaining.len() {
            let upper = remaining[i].to_uppercase();
            match upper.as_str() {
                "W" | "A" | "N" | "P" | "B" => {
                    data_type = Some(upper.clone());
                    // Next might be length
                    if i + 1 < remaining.len() {
                        if let Ok(len) = remaining[i + 1].parse::<u32>() {
                            length = Some(len);
                            i += 1;
                        }
                    }
                }
                "VALUE" => {
                    if i + 1 < remaining.len() {
                        value = Some(remaining[i + 1].clone());
                        i += 1;
                    }
                }
                _ => {
                    // Could be position or file reference
                    if let Ok(pos) = remaining[i].parse::<u32>() {
                        if position.is_none() {
                            position = Some(pos);
                        } else if length.is_none() {
                            length = Some(pos);
                        }
                    } else if file.is_none()
                        && !remaining[i].starts_with('(')
                        && !remaining[i].ends_with(')')
                    {
                        // Might be a file name if no file set yet and before position
                        if position.is_none() {
                            file = Some(remaining[i].clone());
                        }
                    }
                }
            }
            i += 1;
        }

        Ok(EzStatement::Define {
            name,
            file,
            position,
            length,
            data_type,
            value,
        })
    }

    /// Parse JOB statement.
    fn parse_job(tokens: &[EzToken]) -> Result<EzStatement, ParseError> {
        let mut name = None;
        let mut input = None;
        let remaining = Self::collect_remaining_text(&tokens[1..]);
        let mut i = 0;
        while i < remaining.len() {
            let upper = remaining[i].to_uppercase();
            if upper == "INPUT" {
                if i + 1 < remaining.len() {
                    input = Some(remaining[i + 1].clone());
                    i += 1;
                }
            } else if name.is_none() {
                name = Some(remaining[i].clone());
            }
            i += 1;
        }
        Ok(EzStatement::Job { name, input })
    }

    /// Parse SORT statement.
    fn parse_sort(tokens: &[EzToken], _line: usize) -> Result<EzStatement, ParseError> {
        let file = Self::get_identifier_at(tokens, 1).unwrap_or_default();
        let remaining = Self::collect_remaining_text(&tokens[2..]);
        let mut keys = Vec::new();
        let mut i = 0;
        while i < remaining.len() {
            let upper = remaining[i].to_uppercase();
            if upper == "BY" {
                i += 1;
                continue;
            }
            let field = remaining[i].clone();
            let mut dir = SortDirection::Ascending;
            if i + 1 < remaining.len() {
                let next_upper = remaining[i + 1].to_uppercase();
                if next_upper == "D" || next_upper == "DESC" || next_upper == "DESCENDING" {
                    dir = SortDirection::Descending;
                    i += 1;
                } else if next_upper == "A" || next_upper == "ASC" || next_upper == "ASCENDING" {
                    i += 1;
                }
            }
            keys.push((field, dir));
            i += 1;
        }
        Ok(EzStatement::Sort { file, keys })
    }

    /// Parse PUT statement.
    fn parse_put(tokens: &[EzToken]) -> Result<EzStatement, ParseError> {
        let mut file = None;
        let mut matched = false;
        for tok in tokens.iter().skip(1) {
            match tok {
                EzToken::Identifier(id) => file = Some(id.clone()),
                EzToken::Keyword(k) if k == "MATCHED" => matched = true,
                _ => {}
            }
        }
        Ok(EzStatement::Put { file, matched })
    }

    /// Parse GET statement.
    fn parse_get(tokens: &[EzToken], _line: usize) -> Result<EzStatement, ParseError> {
        let file = Self::get_identifier_at(tokens, 1).unwrap_or_default();
        Ok(EzStatement::Get { file })
    }

    /// Parse PRINT statement.
    fn parse_print(tokens: &[EzToken]) -> Result<EzStatement, ParseError> {
        let report = Self::get_identifier_at(tokens, 1);
        Ok(EzStatement::Print { report })
    }

    /// Parse HEADING/LINE/TITLE statement.
    fn parse_heading_line_title(
        tokens: &[EzToken],
        kind: &str,
    ) -> Result<EzStatement, ParseError> {
        let mut number = None;
        let mut start_idx = 1;

        // Check for optional number
        if let Some(EzToken::Number(n)) = tokens.get(1) {
            number = n.parse().ok();
            start_idx = 2;
        }

        let items = Self::collect_remaining_text(&tokens[start_idx..]);

        match kind {
            "HEADING" => Ok(EzStatement::Heading { number, items }),
            "LINE" => Ok(EzStatement::Line { number, items }),
            "TITLE" => Ok(EzStatement::Title { number, items }),
            _ => unreachable!(),
        }
    }

    /// Extract an identifier or keyword token at a given index.
    ///
    /// In Easytrieve, single-letter data type codes (A, B, N, P, W) are
    /// also keywords. When they appear in name positions we still want
    /// to accept them.
    fn get_identifier_at(tokens: &[EzToken], idx: usize) -> Option<String> {
        match tokens.get(idx) {
            Some(EzToken::Identifier(id)) => Some(id.clone()),
            Some(EzToken::Keyword(k)) => Some(k.clone()),
            _ => None,
        }
    }

    /// Extract a keyword token value at a given index.
    fn get_keyword_at(tokens: &[EzToken], idx: usize) -> Option<String> {
        match tokens.get(idx) {
            Some(EzToken::Keyword(k)) => Some(k.clone()),
            _ => None,
        }
    }

    /// Collect all remaining tokens as text strings, skipping EndOfLine.
    fn collect_remaining_text(tokens: &[EzToken]) -> Vec<String> {
        tokens
            .iter()
            .filter_map(|t| match t {
                EzToken::Keyword(k) => Some(k.clone()),
                EzToken::Identifier(id) => Some(id.clone()),
                EzToken::Number(n) => Some(n.clone()),
                EzToken::StringLiteral(s) => Some(format!("'{s}'")),
                EzToken::Operator(op) => Some(op.clone()),
                EzToken::LeftParen => Some("(".into()),
                EzToken::RightParen => Some(")".into()),
                EzToken::Comma => Some(",".into()),
                EzToken::Period => Some(".".into()),
                EzToken::EndOfLine => None,
            })
            .collect()
    }

    /// Parse keyword-value pairs from token slice (e.g., LRECL 80 RECFM FB).
    fn parse_keyword_value_pairs(tokens: &[EzToken]) -> Vec<(String, String)> {
        let items = Self::collect_remaining_text(tokens);
        let mut pairs = Vec::new();
        let mut i = 0;
        while i < items.len() {
            let key = items[i].clone();
            if i + 1 < items.len() {
                pairs.push((key, items[i + 1].clone()));
                i += 2;
            } else {
                pairs.push((key, String::new()));
                i += 1;
            }
        }
        pairs
    }
}

impl Default for EzParser {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_simple() {
        let tokens = tokenize_statement("FILE INFILE", 1).unwrap();
        assert_eq!(tokens[0], EzToken::Keyword("FILE".into()));
        assert_eq!(tokens[1], EzToken::Identifier("INFILE".into()));
        assert_eq!(tokens[2], EzToken::EndOfLine);
    }

    #[test]
    fn test_tokenize_string_literal() {
        let tokens = tokenize_statement("DISPLAY 'HELLO WORLD'", 1).unwrap();
        assert_eq!(tokens[0], EzToken::Keyword("DISPLAY".into()));
        assert_eq!(tokens[1], EzToken::StringLiteral("HELLO WORLD".into()));
    }

    #[test]
    fn test_tokenize_numbers() {
        let tokens = tokenize_statement("DEFINE AMOUNT W 8 N", 1).unwrap();
        assert_eq!(tokens[0], EzToken::Keyword("DEFINE".into()));
        assert_eq!(tokens[1], EzToken::Identifier("AMOUNT".into()));
        assert_eq!(tokens[2], EzToken::Keyword("W".into()));
        assert_eq!(tokens[3], EzToken::Number("8".into()));
        assert_eq!(tokens[4], EzToken::Keyword("N".into()));
    }

    #[test]
    fn test_tokenize_operators() {
        let tokens = tokenize_statement("IF AMOUNT > 100", 1).unwrap();
        assert_eq!(tokens[0], EzToken::Keyword("IF".into()));
        assert_eq!(tokens[1], EzToken::Identifier("AMOUNT".into()));
        assert_eq!(tokens[2], EzToken::Operator(">".into()));
        assert_eq!(tokens[3], EzToken::Number("100".into()));
    }

    #[test]
    fn test_tokenize_parentheses() {
        let tokens = tokenize_statement("CALL MYPROG (A B C)", 1).unwrap();
        assert!(matches!(tokens[2], EzToken::LeftParen));
        assert!(matches!(tokens[6], EzToken::RightParen));
    }

    #[test]
    fn test_comment_line() {
        let tokens = tokenize_statement("* THIS IS A COMMENT", 1).unwrap();
        // Comment line produces just EndOfLine
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], EzToken::EndOfLine);
    }

    #[test]
    fn test_extract_statement_fixed_column() {
        let line = "0010FILE INFILE                                                            SEQ00001";
        let stmt = EzParser::extract_statement(line);
        assert!(stmt.starts_with("FILE"));
    }

    #[test]
    fn test_parse_file_statement() {
        let source = "    FILE INFILE LRECL 80 RECFM FB\n";
        let prog = EzParser::parse(source).unwrap();
        assert_eq!(prog.files.len(), 1);
        match &prog.files[0] {
            EzStatement::File { name, attrs } => {
                assert_eq!(name, "INFILE");
                assert_eq!(attrs.len(), 2);
                assert_eq!(attrs[0], ("LRECL".into(), "80".into()));
            }
            _ => panic!("expected File statement"),
        }
    }

    #[test]
    fn test_parse_define_statement() {
        let source = "    DEFINE AMOUNT W 8 N\n";
        let prog = EzParser::parse(source).unwrap();
        assert_eq!(prog.defines.len(), 1);
        match &prog.defines[0] {
            EzStatement::Define {
                name, data_type, ..
            } => {
                assert_eq!(name, "AMOUNT");
                assert_eq!(data_type.as_deref(), Some("N"));
            }
            _ => panic!("expected Define statement"),
        }
    }

    #[test]
    fn test_parse_job_statement() {
        let source = "    JOB INPUT INFILE\n";
        let prog = EzParser::parse(source).unwrap();
        assert!(!prog.activities.is_empty());
        match &prog.activities[0] {
            EzStatement::Job { input, .. } => {
                assert_eq!(input.as_deref(), Some("INFILE"));
            }
            _ => panic!("expected Job statement"),
        }
    }

    #[test]
    fn test_parse_if_else_endif() {
        let source = "    IF AMOUNT > 100\n    DISPLAY 'BIG'\n    ELSE\n    DISPLAY 'SMALL'\n    END-IF\n";
        let prog = EzParser::parse(source).unwrap();
        let kinds: Vec<&str> = prog
            .activities
            .iter()
            .map(|s| match s {
                EzStatement::If { .. } => "IF",
                EzStatement::Else => "ELSE",
                EzStatement::EndIf => "END-IF",
                EzStatement::Display { .. } => "DISPLAY",
                _ => "OTHER",
            })
            .collect();
        assert_eq!(kinds, vec!["IF", "DISPLAY", "ELSE", "DISPLAY", "END-IF"]);
    }

    #[test]
    fn test_parse_sort_statement() {
        let source = "    SORT INFILE BY AMOUNT D\n";
        let prog = EzParser::parse(source).unwrap();
        match &prog.activities[0] {
            EzStatement::Sort { file, keys } => {
                assert_eq!(file, "INFILE");
                assert_eq!(keys.len(), 1);
                assert_eq!(keys[0].1, SortDirection::Descending);
            }
            _ => panic!("expected Sort statement"),
        }
    }

    #[test]
    fn test_parse_complete_program() {
        let source = "    FILE INFILE\n    DEFINE NAME INFILE 1 20 A\n    DEFINE SALARY INFILE 21 8 N\n    JOB INPUT INFILE\n    PRINT\n    HEADING 1 'EMPLOYEE REPORT'\n    LINE 1 NAME SALARY\n    END\n";
        let prog = EzParser::parse(source).unwrap();
        assert_eq!(prog.files.len(), 1);
        assert_eq!(prog.defines.len(), 2);
        assert!(prog.activities.len() >= 4);
    }

    #[test]
    fn test_unterminated_string() {
        let result = tokenize_statement("DISPLAY 'UNTERMINATED", 1);
        assert!(result.is_err());
    }
}
