//! # CLIST Lexer & Parser (CL-100)
//!
//! Parses CLIST source scripts into structured AST representations.
//! Handles continuation characters (+/-), labels, ~25 statement types,
//! expressions with operator precedence.

use std::fmt;

// ─────────────────────── Errors ───────────────────────

/// Parse error.
#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("line {line}: {message}")]
    Syntax { line: usize, message: String },
    #[error("unexpected end of input at line {0}")]
    UnexpectedEnd(usize),
    #[error("unknown statement: {0}")]
    UnknownStatement(String),
}

// ─────────────────────── Tokens ───────────────────────

/// Token kind.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClistTokenKind {
    /// A CLIST keyword (SET, IF, DO, WRITE, etc.)
    Keyword(String),
    /// A &-prefixed symbolic variable
    Variable(String),
    /// A string literal (quoted)
    StringLiteral(String),
    /// A numeric literal
    Number(i64),
    /// An operator (+, -, *, /, =, <, >, etc.)
    Operator(String),
    /// Left paren
    LeftParen,
    /// Right paren
    RightParen,
    /// Comma
    Comma,
    /// A label reference
    Label(String),
    /// A bare identifier (e.g., dataset name, command name)
    Identifier(String),
    /// End of line
    EndOfLine,
}

/// A token with source position.
#[derive(Debug, Clone)]
pub struct ClistToken {
    pub kind: ClistTokenKind,
    pub line: usize,
    pub col: usize,
}

// ─────────────────────── CL-100.1: Source Line Processing ───────────────────────

/// Process CLIST source: handle continuation chars (+/-) and join logical lines.
/// `+` at end of line: continuation, strip leading blanks of next line.
/// `-` at end of line: continuation, preserve leading blanks of next line.
pub fn process_source_lines(source: &str) -> Vec<(usize, String)> {
    let raw_lines: Vec<&str> = source.lines().collect();
    let mut logical_lines: Vec<(usize, String)> = Vec::new();
    let mut current = String::new();
    let mut start_line = 1;
    let mut in_continuation = false;
    let mut strip_leading = false;

    for (idx, raw) in raw_lines.iter().enumerate() {
        let line_num = idx + 1;
        // Skip blank lines and comment lines (starting with /* outside strings)
        let trimmed = raw.trim();
        if trimmed.is_empty() {
            if in_continuation {
                continue;
            }
            continue;
        }

        let mut line_content = if in_continuation && strip_leading {
            trimmed.to_string()
        } else if in_continuation {
            raw.to_string()
        } else {
            start_line = line_num;
            current.clear();
            trimmed.to_string()
        };

        // Check for continuation at end
        in_continuation = false;
        strip_leading = false;
        if line_content.ends_with('+') {
            line_content.pop();
            in_continuation = true;
            strip_leading = true;
        } else if line_content.ends_with('-') {
            line_content.pop();
            in_continuation = true;
            strip_leading = false;
        }

        current.push_str(&line_content);

        if !in_continuation {
            logical_lines.push((start_line, current.clone()));
            current.clear();
        }
    }

    // Flush any remaining
    if !current.is_empty() {
        logical_lines.push((start_line, current));
    }

    logical_lines
}

// ─────────────────────── CL-100.2: Tokenizer ───────────────────────

/// Tokenize a single logical CLIST line.
pub fn tokenize_line(line: &str, line_num: usize) -> Vec<ClistToken> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = line.chars().collect();
    let len = chars.len();
    let mut i = 0;

    while i < len {
        let ch = chars[i];

        // Skip whitespace
        if ch.is_whitespace() {
            i += 1;
            continue;
        }

        let col = i + 1;

        // Comment: /* ... */
        if ch == '/' && i + 1 < len && chars[i + 1] == '*' {
            // Skip to end of comment
            i += 2;
            while i < len {
                if chars[i] == '*' && i + 1 < len && chars[i + 1] == '/' {
                    i += 2;
                    break;
                }
                i += 1;
            }
            continue;
        }

        // String literal: 'quoted'
        if ch == '\'' {
            let mut s = String::new();
            i += 1;
            while i < len {
                if chars[i] == '\'' {
                    if i + 1 < len && chars[i + 1] == '\'' {
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
            tokens.push(ClistToken {
                kind: ClistTokenKind::StringLiteral(s),
                line: line_num,
                col,
            });
            continue;
        }

        // Variable: &name or &&name
        if ch == '&' {
            let mut name = String::new();
            i += 1;
            // Handle &&
            if i < len && chars[i] == '&' {
                name.push('&');
                i += 1;
            }
            while i < len && (chars[i].is_alphanumeric() || chars[i] == '_') {
                name.push(chars[i]);
                i += 1;
            }
            if !name.is_empty() {
                tokens.push(ClistToken {
                    kind: ClistTokenKind::Variable(name.to_uppercase()),
                    line: line_num,
                    col,
                });
            }
            continue;
        }

        // Operators
        if "+-*/=".contains(ch) {
            // Check for multi-char operators
            let mut op = String::new();
            op.push(ch);
            tokens.push(ClistToken {
                kind: ClistTokenKind::Operator(op),
                line: line_num,
                col,
            });
            i += 1;
            continue;
        }

        // Comparison operators
        if ch == '<' || ch == '>' {
            let mut op = String::new();
            op.push(ch);
            i += 1;
            if i < len && chars[i] == '=' {
                op.push('=');
                i += 1;
            } else if ch == '<' && i < len && chars[i] == '>' {
                op.push('>');
                i += 1;
            }
            tokens.push(ClistToken {
                kind: ClistTokenKind::Operator(op),
                line: line_num,
                col,
            });
            continue;
        }

        // Parentheses
        if ch == '(' {
            tokens.push(ClistToken {
                kind: ClistTokenKind::LeftParen,
                line: line_num,
                col,
            });
            i += 1;
            continue;
        }
        if ch == ')' {
            tokens.push(ClistToken {
                kind: ClistTokenKind::RightParen,
                line: line_num,
                col,
            });
            i += 1;
            continue;
        }

        // Comma
        if ch == ',' {
            tokens.push(ClistToken {
                kind: ClistTokenKind::Comma,
                line: line_num,
                col,
            });
            i += 1;
            continue;
        }

        // Colon (label separator)
        if ch == ':' {
            i += 1;
            continue;
        }

        // Number
        if ch.is_ascii_digit() {
            let mut num = String::new();
            while i < len && chars[i].is_ascii_digit() {
                num.push(chars[i]);
                i += 1;
            }
            if let Ok(n) = num.parse::<i64>() {
                tokens.push(ClistToken {
                    kind: ClistTokenKind::Number(n),
                    line: line_num,
                    col,
                });
            }
            continue;
        }

        // Identifier or keyword
        if ch.is_alphabetic() || ch == '_' {
            let mut word = String::new();
            while i < len && (chars[i].is_alphanumeric() || chars[i] == '_' || chars[i] == '.') {
                word.push(chars[i]);
                i += 1;
            }
            let upper = word.to_uppercase();
            // Check if this is a CLIST keyword
            if is_keyword(&upper) {
                tokens.push(ClistToken {
                    kind: ClistTokenKind::Keyword(upper),
                    line: line_num,
                    col,
                });
            } else {
                tokens.push(ClistToken {
                    kind: ClistTokenKind::Identifier(word),
                    line: line_num,
                    col,
                });
            }
            continue;
        }

        // Skip unknown characters
        i += 1;
    }

    tokens
}

fn is_keyword(word: &str) -> bool {
    matches!(
        word,
        "SET" | "IF" | "THEN" | "ELSE" | "DO" | "WHILE" | "UNTIL" | "END"
            | "SELECT" | "WHEN" | "OTHERWISE" | "GOTO" | "EXIT"
            | "WRITE" | "WRITENR" | "READ" | "READDVAL" | "TERMIN"
            | "OPENFILE" | "GETFILE" | "PUTFILE" | "CLOSFILE"
            | "ERROR" | "ATTN" | "CONTROL" | "PROC" | "RETURN"
            | "SYSCALL" | "SYSREF" | "GLOBAL" | "NGLOBAL" | "EXEC"
            | "LISTDSI" | "ISPEXEC" | "ISREDIT" | "DATA" | "ENDDATA"
            | "EQ" | "NE" | "GT" | "GE" | "LT" | "LE" | "AND" | "OR" | "NOT"
            | "OFF" | "ON" | "MAIN" | "NOFLUSH" | "FLUSH"
            | "LIST" | "NOLIST" | "CONLIST" | "NOCONLIST"
            | "SYMLIST" | "NOSYMLIST" | "MSG" | "NOMSG" | "PROMPT" | "NOPROMPT"
            | "ASIS" | "CAPS"
    )
}

/// Tokenize an entire CLIST source.
pub fn tokenize_clist(source: &str) -> Vec<Vec<ClistToken>> {
    let lines = process_source_lines(source);
    lines
        .iter()
        .map(|(line_num, content)| tokenize_line(content, *line_num))
        .collect()
}

// ─────────────────────── CL-100.3/4: AST & Expression ───────────────────────

/// Expression operator.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessOrEqual,
    GreaterOrEqual,
    And,
    Or,
    Not,
    Concat,
}

impl fmt::Display for ExprOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Subtract => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::Modulo => write!(f, "//"),
            Self::Equal => write!(f, "="),
            Self::NotEqual => write!(f, "<>"),
            Self::LessThan => write!(f, "<"),
            Self::GreaterThan => write!(f, ">"),
            Self::LessOrEqual => write!(f, "<="),
            Self::GreaterOrEqual => write!(f, ">="),
            Self::And => write!(f, "AND"),
            Self::Or => write!(f, "OR"),
            Self::Not => write!(f, "NOT"),
            Self::Concat => write!(f, "||"),
        }
    }
}

/// Expression node.
#[derive(Debug, Clone)]
pub enum Expression {
    /// A numeric literal.
    Number(i64),
    /// A string literal.
    StringLit(String),
    /// A variable reference.
    Variable(String),
    /// A binary operation.
    BinaryOp {
        left: Box<Expression>,
        op: ExprOp,
        right: Box<Expression>,
    },
    /// A unary operation (NOT).
    UnaryOp {
        op: ExprOp,
        operand: Box<Expression>,
    },
    /// A function call.
    FunctionCall {
        name: String,
        args: Vec<Expression>,
    },
}

/// A CLIST statement.
#[derive(Debug, Clone)]
pub enum ClistStatement {
    /// A label definition.
    Label(String),
    /// SET &var = expr
    Set {
        variable: String,
        value: Expression,
    },
    /// IF expr THEN
    If {
        condition: Expression,
        then_body: Vec<ClistStatement>,
        else_body: Vec<ClistStatement>,
    },
    /// DO [WHILE expr | UNTIL expr]
    Do {
        condition: Option<(DoCondition, Expression)>,
        body: Vec<ClistStatement>,
    },
    /// SELECT
    Select {
        cases: Vec<(Expression, Vec<ClistStatement>)>,
        otherwise: Vec<ClistStatement>,
    },
    /// GOTO label
    Goto(String),
    /// EXIT [code]
    Exit(Option<Expression>),
    /// WRITE text
    Write(Expression),
    /// WRITENR text
    WriteNr(Expression),
    /// READ &var
    Read(String),
    /// READDVAL &var1 &var2 ...
    ReadDval(Vec<String>),
    /// OPENFILE &file [INPUT|OUTPUT|UPDATE]
    OpenFile {
        variable: String,
        mode: String,
    },
    /// GETFILE &file
    GetFile(String),
    /// PUTFILE &file
    PutFile(String),
    /// CLOSFILE &file
    CloseFile(String),
    /// ERROR DO ... END or ERROR OFF
    Error(ErrorAction),
    /// ATTN DO ... END or ATTN OFF
    Attn(ErrorAction),
    /// CONTROL options
    Control(Vec<String>),
    /// PROC positional keyword(defaults)
    Proc {
        positional: u32,
        keywords: Vec<(String, Option<String>)>,
    },
    /// SYSCALL label (args)
    SysCall {
        label: String,
        args: Vec<Expression>,
    },
    /// GLOBAL &vars
    Global(Vec<String>),
    /// NGLOBAL &vars
    NLocal(Vec<String>),
    /// EXEC 'clist-name' args
    Exec {
        name: String,
        args: Vec<Expression>,
    },
    /// LISTDSI 'dsname'
    Listdsi(Expression),
    /// ISPEXEC service args
    IspExec(String),
    /// ISREDIT command
    IsrEdit(String),
    /// TSO command (unrecognized statement)
    TsoCommand(String),
    /// RETURN [code]
    Return(Option<Expression>),
    /// DATA ... ENDDATA block
    Data(String),
    /// TERMIN
    Termin,
}

/// DO loop condition type.
#[derive(Debug, Clone)]
pub enum DoCondition {
    While,
    Until,
}

/// ERROR/ATTN action.
#[derive(Debug, Clone)]
pub enum ErrorAction {
    Off,
    Body(Vec<ClistStatement>),
    Return,
}

/// A parsed CLIST AST.
#[derive(Debug, Clone)]
pub struct ClistAst {
    pub statements: Vec<ClistStatement>,
    pub line_map: Vec<usize>,
}

// ─────────────────────── Parser ───────────────────────

/// Parse CLIST source into AST.
pub fn parse_clist(source: &str) -> Result<ClistAst, ParseError> {
    let logical_lines = process_source_lines(source);
    let mut statements = Vec::new();
    let mut line_map = Vec::new();
    let mut i = 0;

    while i < logical_lines.len() {
        let (line_num, ref content) = logical_lines[i];
        let trimmed = content.trim();

        // Skip empty lines and pure comments
        if trimmed.is_empty() || trimmed.starts_with("/*") {
            i += 1;
            continue;
        }

        // Check for label: "LABEL: ..."
        if let Some(colon_pos) = trimmed.find(':') {
            let potential_label = trimmed[..colon_pos].trim();
            if !potential_label.is_empty()
                && potential_label.chars().all(|c| c.is_alphanumeric() || c == '_')
                && !potential_label.starts_with('&')
            {
                statements.push(ClistStatement::Label(potential_label.to_uppercase()));
                line_map.push(line_num);
                let rest = trimmed[colon_pos + 1..].trim();
                if !rest.is_empty() {
                    if let Some(stmt) = parse_statement_text(rest, line_num, &logical_lines, &mut i)? {
                        statements.push(stmt);
                        line_map.push(line_num);
                    }
                    i += 1;
                    continue;
                }
                i += 1;
                continue;
            }
        }

        if let Some(stmt) = parse_statement_text(trimmed, line_num, &logical_lines, &mut i)? {
            statements.push(stmt);
            line_map.push(line_num);
        }

        i += 1;
    }

    Ok(ClistAst {
        statements,
        line_map,
    })
}

fn parse_statement_text(
    text: &str,
    line_num: usize,
    lines: &[(usize, String)],
    idx: &mut usize,
) -> Result<Option<ClistStatement>, ParseError> {
    let tokens = tokenize_line(text, line_num);
    if tokens.is_empty() {
        return Ok(None);
    }

    let first = &tokens[0];
    match &first.kind {
        ClistTokenKind::Keyword(kw) => match kw.as_str() {
            "SET" => parse_set(&tokens, line_num),
            "IF" => parse_if_inline(&tokens, line_num, lines, idx),
            "DO" => parse_do(&tokens, line_num, lines, idx),
            "SELECT" => parse_select(line_num, lines, idx),
            "GOTO" => parse_goto(&tokens, line_num),
            "EXIT" => parse_exit(&tokens),
            "RETURN" => parse_return(&tokens),
            "WRITE" => parse_write(&tokens, false),
            "WRITENR" => parse_write(&tokens, true),
            "READ" => parse_read(&tokens),
            "READDVAL" => parse_readdval(&tokens),
            "OPENFILE" => parse_openfile(&tokens),
            "GETFILE" => parse_getputfile(&tokens, true),
            "PUTFILE" => parse_getputfile(&tokens, false),
            "CLOSFILE" => parse_closfile(&tokens),
            "ERROR" => parse_error_attn(&tokens, true, lines, idx),
            "ATTN" => parse_error_attn(&tokens, false, lines, idx),
            "CONTROL" => parse_control(&tokens),
            "PROC" => parse_proc(&tokens),
            "SYSCALL" => parse_syscall(&tokens),
            "GLOBAL" => parse_global(&tokens, true),
            "NGLOBAL" => parse_global(&tokens, false),
            "EXEC" => parse_exec(&tokens),
            "LISTDSI" => parse_listdsi(&tokens),
            "ISPEXEC" => Ok(Some(ClistStatement::IspExec(
                text.strip_prefix("ISPEXEC").or_else(|| text.strip_prefix("ispexec"))
                    .unwrap_or("")
                    .trim()
                    .to_string(),
            ))),
            "ISREDIT" => Ok(Some(ClistStatement::IsrEdit(
                text.strip_prefix("ISREDIT").or_else(|| text.strip_prefix("isredit"))
                    .unwrap_or("")
                    .trim()
                    .to_string(),
            ))),
            "TERMIN" => Ok(Some(ClistStatement::Termin)),
            "DATA" => parse_data(lines, idx),
            "END" | "THEN" | "ELSE" | "WHEN" | "OTHERWISE" | "ENDDATA" => Ok(None),
            _ => Ok(Some(ClistStatement::TsoCommand(text.to_string()))),
        },
        _ => {
            // Treat as TSO command
            Ok(Some(ClistStatement::TsoCommand(text.to_string())))
        }
    }
}

fn parse_set(tokens: &[ClistToken], line_num: usize) -> Result<Option<ClistStatement>, ParseError> {
    // SET &var = expr
    if tokens.len() < 4 {
        return Err(ParseError::Syntax {
            line: line_num,
            message: "SET requires &variable = expression".to_string(),
        });
    }
    let var_name = match &tokens[1].kind {
        ClistTokenKind::Variable(v) => v.clone(),
        _ => {
            return Err(ParseError::Syntax {
                line: line_num,
                message: "SET requires a &variable".to_string(),
            });
        }
    };
    // Skip '=' at tokens[2]
    let expr = parse_expression_from_tokens(&tokens[3..]);
    Ok(Some(ClistStatement::Set {
        variable: var_name,
        value: expr,
    }))
}

fn parse_if_inline(
    tokens: &[ClistToken],
    line_num: usize,
    lines: &[(usize, String)],
    idx: &mut usize,
) -> Result<Option<ClistStatement>, ParseError> {
    // Find THEN
    let then_pos = tokens.iter().position(|t| matches!(&t.kind, ClistTokenKind::Keyword(k) if k == "THEN"));

    let condition_tokens = if let Some(pos) = then_pos {
        &tokens[1..pos]
    } else {
        &tokens[1..]
    };

    let condition = parse_expression_from_tokens(condition_tokens);

    // Parse THEN body — could be inline or block
    let mut then_body = Vec::new();
    let mut else_body = Vec::new();

    if let Some(pos) = then_pos {
        let after_then = &tokens[pos + 1..];
        if after_then.is_empty() {
            // Block IF — collect until ELSE or END
            collect_if_body(lines, idx, &mut then_body, &mut else_body)?;
        } else if matches!(&after_then[0].kind, ClistTokenKind::Keyword(k) if k == "DO") {
            collect_if_body(lines, idx, &mut then_body, &mut else_body)?;
        } else {
            let rest_text = reconstruct_tokens(after_then);
            if let Some(stmt) = parse_statement_text(&rest_text, line_num, lines, idx)? {
                then_body.push(stmt);
            }
            // Check next line for ELSE
            if *idx + 1 < lines.len() {
                let next = lines[*idx + 1].1.trim().to_uppercase();
                if next.starts_with("ELSE") {
                    *idx += 1;
                    let else_text = lines[*idx].1.trim();
                    let else_rest = else_text.strip_prefix("ELSE").or_else(|| else_text.strip_prefix("else"))
                        .unwrap_or("").trim();
                    if !else_rest.is_empty() {
                        if let Some(stmt) = parse_statement_text(else_rest, lines[*idx].0, lines, idx)? {
                            else_body.push(stmt);
                        }
                    }
                }
            }
        }
    }

    Ok(Some(ClistStatement::If {
        condition,
        then_body,
        else_body,
    }))
}

fn collect_if_body(
    lines: &[(usize, String)],
    idx: &mut usize,
    then_body: &mut Vec<ClistStatement>,
    else_body: &mut Vec<ClistStatement>,
) -> Result<(), ParseError> {
    let mut in_else = false;
    let mut depth = 1i32;

    while *idx + 1 < lines.len() {
        *idx += 1;
        let (ln, ref content) = lines[*idx];
        let trimmed = content.trim();
        let upper = trimmed.to_uppercase();

        if upper.starts_with("DO") {
            depth += 1;
        }
        if upper == "END" {
            depth -= 1;
            if depth <= 0 {
                break;
            }
        }
        if upper.starts_with("ELSE") && depth == 1 {
            in_else = true;
            let rest = trimmed.strip_prefix("ELSE").or_else(|| trimmed.strip_prefix("else"))
                .unwrap_or("").trim();
            if !rest.is_empty() {
                if let Some(stmt) = parse_statement_text(rest, ln, lines, idx)? {
                    else_body.push(stmt);
                }
            }
            continue;
        }

        let target = if in_else { &mut *else_body } else { &mut *then_body };
        if let Some(stmt) = parse_statement_text(trimmed, ln, lines, idx)? {
            target.push(stmt);
        }
    }
    Ok(())
}

fn parse_do(
    tokens: &[ClistToken],
    _line_num: usize,
    lines: &[(usize, String)],
    idx: &mut usize,
) -> Result<Option<ClistStatement>, ParseError> {
    // DO [WHILE|UNTIL expr]
    let condition = if tokens.len() > 1 {
        match &tokens[1].kind {
            ClistTokenKind::Keyword(k) if k == "WHILE" => {
                let expr = parse_expression_from_tokens(&tokens[2..]);
                Some((DoCondition::While, expr))
            }
            ClistTokenKind::Keyword(k) if k == "UNTIL" => {
                let expr = parse_expression_from_tokens(&tokens[2..]);
                Some((DoCondition::Until, expr))
            }
            _ => None,
        }
    } else {
        None
    };

    let mut body = Vec::new();
    // Collect until END
    while *idx + 1 < lines.len() {
        *idx += 1;
        let (ln, ref content) = lines[*idx];
        let trimmed = content.trim();
        if trimmed.eq_ignore_ascii_case("END") {
            break;
        }
        if let Some(stmt) = parse_statement_text(trimmed, ln, lines, idx)? {
            body.push(stmt);
        }
    }

    Ok(Some(ClistStatement::Do { condition, body }))
}

fn parse_select(
    _line_num: usize,
    lines: &[(usize, String)],
    idx: &mut usize,
) -> Result<Option<ClistStatement>, ParseError> {
    let mut cases = Vec::new();
    let mut otherwise = Vec::new();
    let mut in_otherwise = false;

    while *idx + 1 < lines.len() {
        *idx += 1;
        let (ln, ref content) = lines[*idx];
        let trimmed = content.trim();
        let upper = trimmed.to_uppercase();

        if upper == "END" {
            break;
        }
        if upper.starts_with("WHEN") {
            let when_tokens = tokenize_line(trimmed, ln);
            // WHEN expr THEN stmt...
            let then_pos = when_tokens.iter().position(|t| matches!(&t.kind, ClistTokenKind::Keyword(k) if k == "THEN"));
            let end = then_pos.unwrap_or(when_tokens.len());
            let expr = parse_expression_from_tokens(&when_tokens[1..end]);
            let mut body = Vec::new();
            if let Some(pos) = then_pos {
                let rest = reconstruct_tokens(&when_tokens[pos + 1..]);
                if !rest.is_empty() {
                    if let Some(stmt) = parse_statement_text(&rest, ln, lines, idx)? {
                        body.push(stmt);
                    }
                }
            }
            cases.push((expr, body));
            continue;
        }
        if upper.starts_with("OTHERWISE") {
            in_otherwise = true;
            let rest = trimmed.strip_prefix("OTHERWISE").or_else(|| trimmed.strip_prefix("otherwise"))
                .unwrap_or("").trim();
            if !rest.is_empty() {
                if let Some(stmt) = parse_statement_text(rest, ln, lines, idx)? {
                    otherwise.push(stmt);
                }
            }
            continue;
        }

        if in_otherwise {
            if let Some(stmt) = parse_statement_text(trimmed, ln, lines, idx)? {
                otherwise.push(stmt);
            }
        } else if let Some(last_case) = cases.last_mut() {
            if let Some(stmt) = parse_statement_text(trimmed, ln, lines, idx)? {
                last_case.1.push(stmt);
            }
        }
    }

    Ok(Some(ClistStatement::Select { cases, otherwise }))
}

fn parse_goto(tokens: &[ClistToken], line_num: usize) -> Result<Option<ClistStatement>, ParseError> {
    if tokens.len() < 2 {
        return Err(ParseError::Syntax {
            line: line_num,
            message: "GOTO requires a label".to_string(),
        });
    }
    let label = match &tokens[1].kind {
        ClistTokenKind::Identifier(s) | ClistTokenKind::Keyword(s) => s.to_uppercase(),
        ClistTokenKind::Variable(v) => v.clone(),
        _ => "UNKNOWN".to_string(),
    };
    Ok(Some(ClistStatement::Goto(label)))
}

fn parse_exit(tokens: &[ClistToken]) -> Result<Option<ClistStatement>, ParseError> {
    let code = if tokens.len() > 1 {
        Some(parse_expression_from_tokens(&tokens[1..]))
    } else {
        None
    };
    Ok(Some(ClistStatement::Exit(code)))
}

fn parse_return(tokens: &[ClistToken]) -> Result<Option<ClistStatement>, ParseError> {
    let code = if tokens.len() > 1 {
        Some(parse_expression_from_tokens(&tokens[1..]))
    } else {
        None
    };
    Ok(Some(ClistStatement::Return(code)))
}

fn parse_write(tokens: &[ClistToken], is_nr: bool) -> Result<Option<ClistStatement>, ParseError> {
    let expr = if tokens.len() > 1 {
        parse_expression_from_tokens(&tokens[1..])
    } else {
        Expression::StringLit(String::new())
    };
    if is_nr {
        Ok(Some(ClistStatement::WriteNr(expr)))
    } else {
        Ok(Some(ClistStatement::Write(expr)))
    }
}

fn parse_read(tokens: &[ClistToken]) -> Result<Option<ClistStatement>, ParseError> {
    let var = if tokens.len() > 1 {
        match &tokens[1].kind {
            ClistTokenKind::Variable(v) => v.clone(),
            _ => "SYSDVAL".to_string(),
        }
    } else {
        "SYSDVAL".to_string()
    };
    Ok(Some(ClistStatement::Read(var)))
}

fn parse_readdval(tokens: &[ClistToken]) -> Result<Option<ClistStatement>, ParseError> {
    let vars: Vec<String> = tokens[1..]
        .iter()
        .filter_map(|t| match &t.kind {
            ClistTokenKind::Variable(v) => Some(v.clone()),
            _ => None,
        })
        .collect();
    Ok(Some(ClistStatement::ReadDval(vars)))
}

fn parse_openfile(tokens: &[ClistToken]) -> Result<Option<ClistStatement>, ParseError> {
    let var = tokens.get(1)
        .and_then(|t| match &t.kind {
            ClistTokenKind::Variable(v) => Some(v.clone()),
            _ => None,
        })
        .unwrap_or_default();
    let mode = tokens.get(2)
        .and_then(|t| match &t.kind {
            ClistTokenKind::Keyword(k) | ClistTokenKind::Identifier(k) => Some(k.to_uppercase()),
            _ => None,
        })
        .unwrap_or_else(|| "INPUT".to_string());
    Ok(Some(ClistStatement::OpenFile { variable: var, mode }))
}

fn parse_getputfile(tokens: &[ClistToken], is_get: bool) -> Result<Option<ClistStatement>, ParseError> {
    let var = tokens.get(1)
        .and_then(|t| match &t.kind {
            ClistTokenKind::Variable(v) => Some(v.clone()),
            _ => None,
        })
        .unwrap_or_default();
    if is_get {
        Ok(Some(ClistStatement::GetFile(var)))
    } else {
        Ok(Some(ClistStatement::PutFile(var)))
    }
}

fn parse_closfile(tokens: &[ClistToken]) -> Result<Option<ClistStatement>, ParseError> {
    let var = tokens.get(1)
        .and_then(|t| match &t.kind {
            ClistTokenKind::Variable(v) => Some(v.clone()),
            _ => None,
        })
        .unwrap_or_default();
    Ok(Some(ClistStatement::CloseFile(var)))
}

fn parse_error_attn(
    tokens: &[ClistToken],
    is_error: bool,
    lines: &[(usize, String)],
    idx: &mut usize,
) -> Result<Option<ClistStatement>, ParseError> {
    // ERROR OFF | ERROR DO ... END | ERROR RETURN
    let action = if tokens.len() > 1 {
        match &tokens[1].kind {
            ClistTokenKind::Keyword(k) if k == "OFF" => ErrorAction::Off,
            ClistTokenKind::Keyword(k) if k == "RETURN" => ErrorAction::Return,
            ClistTokenKind::Keyword(k) if k == "DO" => {
                let mut body = Vec::new();
                while *idx + 1 < lines.len() {
                    *idx += 1;
                    let (ln, ref content) = lines[*idx];
                    let trimmed = content.trim();
                    if trimmed.eq_ignore_ascii_case("END") {
                        break;
                    }
                    if let Some(stmt) = parse_statement_text(trimmed, ln, lines, idx)? {
                        body.push(stmt);
                    }
                }
                ErrorAction::Body(body)
            }
            _ => ErrorAction::Off,
        }
    } else {
        ErrorAction::Off
    };

    if is_error {
        Ok(Some(ClistStatement::Error(action)))
    } else {
        Ok(Some(ClistStatement::Attn(action)))
    }
}

fn parse_control(tokens: &[ClistToken]) -> Result<Option<ClistStatement>, ParseError> {
    let options: Vec<String> = tokens[1..]
        .iter()
        .filter_map(|t| match &t.kind {
            ClistTokenKind::Keyword(k) | ClistTokenKind::Identifier(k) => Some(k.to_uppercase()),
            _ => None,
        })
        .collect();
    Ok(Some(ClistStatement::Control(options)))
}

fn parse_proc(tokens: &[ClistToken]) -> Result<Option<ClistStatement>, ParseError> {
    let positional = tokens.get(1)
        .and_then(|t| match &t.kind {
            ClistTokenKind::Number(n) => Some(*n as u32),
            _ => None,
        })
        .unwrap_or(0);

    let mut keywords = Vec::new();
    let mut i = 2;
    while i < tokens.len() {
        if let ClistTokenKind::Variable(v) | ClistTokenKind::Identifier(v) = &tokens[i].kind {
            let name = v.to_uppercase();
            let default = if i + 1 < tokens.len() {
                if let ClistTokenKind::LeftParen = &tokens[i + 1].kind {
                    i += 2;
                    let mut val = String::new();
                    while i < tokens.len() {
                        if let ClistTokenKind::RightParen = &tokens[i].kind {
                            break;
                        }
                        match &tokens[i].kind {
                            ClistTokenKind::StringLiteral(s) => val.push_str(s),
                            ClistTokenKind::Number(n) => val.push_str(&n.to_string()),
                            ClistTokenKind::Identifier(s) => val.push_str(s),
                            _ => {}
                        }
                        i += 1;
                    }
                    Some(val)
                } else {
                    None
                }
            } else {
                None
            };
            keywords.push((name, default));
        }
        i += 1;
    }

    Ok(Some(ClistStatement::Proc {
        positional,
        keywords,
    }))
}

fn parse_syscall(tokens: &[ClistToken]) -> Result<Option<ClistStatement>, ParseError> {
    let label = tokens.get(1)
        .map(|t| match &t.kind {
            ClistTokenKind::Identifier(s) | ClistTokenKind::Keyword(s) => s.to_uppercase(),
            ClistTokenKind::Variable(v) => v.clone(),
            _ => "UNKNOWN".to_string(),
        })
        .unwrap_or_default();

    let args: Vec<Expression> = tokens[2..]
        .iter()
        .filter_map(|t| match &t.kind {
            ClistTokenKind::Variable(v) => Some(Expression::Variable(v.clone())),
            ClistTokenKind::StringLiteral(s) => Some(Expression::StringLit(s.clone())),
            ClistTokenKind::Number(n) => Some(Expression::Number(*n)),
            _ => None,
        })
        .collect();

    Ok(Some(ClistStatement::SysCall { label, args }))
}

fn parse_global(tokens: &[ClistToken], is_global: bool) -> Result<Option<ClistStatement>, ParseError> {
    let vars: Vec<String> = tokens[1..]
        .iter()
        .filter_map(|t| match &t.kind {
            ClistTokenKind::Variable(v) => Some(v.clone()),
            _ => None,
        })
        .collect();
    if is_global {
        Ok(Some(ClistStatement::Global(vars)))
    } else {
        Ok(Some(ClistStatement::NLocal(vars)))
    }
}

fn parse_exec(tokens: &[ClistToken]) -> Result<Option<ClistStatement>, ParseError> {
    let name = tokens.get(1)
        .map(|t| match &t.kind {
            ClistTokenKind::StringLiteral(s) | ClistTokenKind::Identifier(s) => s.clone(),
            _ => String::new(),
        })
        .unwrap_or_default();

    let args: Vec<Expression> = tokens[2..]
        .iter()
        .filter_map(|t| match &t.kind {
            ClistTokenKind::StringLiteral(s) => Some(Expression::StringLit(s.clone())),
            ClistTokenKind::Variable(v) => Some(Expression::Variable(v.clone())),
            ClistTokenKind::Number(n) => Some(Expression::Number(*n)),
            _ => None,
        })
        .collect();

    Ok(Some(ClistStatement::Exec { name, args }))
}

fn parse_listdsi(tokens: &[ClistToken]) -> Result<Option<ClistStatement>, ParseError> {
    let expr = if tokens.len() > 1 {
        parse_expression_from_tokens(&tokens[1..])
    } else {
        Expression::StringLit(String::new())
    };
    Ok(Some(ClistStatement::Listdsi(expr)))
}

fn parse_data(
    lines: &[(usize, String)],
    idx: &mut usize,
) -> Result<Option<ClistStatement>, ParseError> {
    let mut data = String::new();
    while *idx + 1 < lines.len() {
        *idx += 1;
        let (_, ref content) = lines[*idx];
        if content.trim().eq_ignore_ascii_case("ENDDATA") {
            break;
        }
        if !data.is_empty() {
            data.push('\n');
        }
        data.push_str(content);
    }
    Ok(Some(ClistStatement::Data(data)))
}

// ─────────────────────── Expression Parsing ───────────────────────

fn parse_expression_from_tokens(tokens: &[ClistToken]) -> Expression {
    if tokens.is_empty() {
        return Expression::StringLit(String::new());
    }

    // Single token fast path
    if tokens.len() == 1 {
        return token_to_expr(&tokens[0]);
    }

    // Check for function call: &FUNC(...)
    if let ClistTokenKind::Variable(name) = &tokens[0].kind {
        if tokens.len() > 1 && matches!(&tokens[1].kind, ClistTokenKind::LeftParen) {
            // This is a function call
            let args = parse_function_args(&tokens[2..]);
            return Expression::FunctionCall {
                name: name.clone(),
                args,
            };
        }
    }

    // Look for comparison/logical operators (lowest precedence)
    for (i, tok) in tokens.iter().enumerate().rev() {
        if let Some(op) = token_to_comparison_op(tok) {
            if i > 0 && i < tokens.len() - 1 {
                let left = parse_expression_from_tokens(&tokens[..i]);
                let right = parse_expression_from_tokens(&tokens[i + 1..]);
                return Expression::BinaryOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                };
            }
        }
    }

    // Look for +/- (low precedence additive)
    for (i, tok) in tokens.iter().enumerate().rev() {
        match &tok.kind {
            ClistTokenKind::Operator(op) if (op == "+" || op == "-") && i > 0 => {
                let left = parse_expression_from_tokens(&tokens[..i]);
                let right = parse_expression_from_tokens(&tokens[i + 1..]);
                let expr_op = if op == "+" { ExprOp::Add } else { ExprOp::Subtract };
                return Expression::BinaryOp {
                    left: Box::new(left),
                    op: expr_op,
                    right: Box::new(right),
                };
            }
            _ => {}
        }
    }

    // Look for * / (high precedence multiplicative)
    for (i, tok) in tokens.iter().enumerate().rev() {
        match &tok.kind {
            ClistTokenKind::Operator(op) if (op == "*" || op == "/") && i > 0 => {
                let left = parse_expression_from_tokens(&tokens[..i]);
                let right = parse_expression_from_tokens(&tokens[i + 1..]);
                let expr_op = if op == "*" { ExprOp::Multiply } else { ExprOp::Divide };
                return Expression::BinaryOp {
                    left: Box::new(left),
                    op: expr_op,
                    right: Box::new(right),
                };
            }
            _ => {}
        }
    }

    // Handle parenthesized expressions
    if matches!(&tokens[0].kind, ClistTokenKind::LeftParen) {
        let depth_end = find_matching_paren(tokens, 0);
        if depth_end == tokens.len() - 1 {
            return parse_expression_from_tokens(&tokens[1..depth_end]);
        }
    }

    // Fallback: concatenate as string
    let mut parts = Vec::new();
    for tok in tokens {
        match &tok.kind {
            ClistTokenKind::StringLiteral(s) => parts.push(s.clone()),
            ClistTokenKind::Variable(v) => parts.push(format!("&{v}")),
            ClistTokenKind::Identifier(s) => parts.push(s.clone()),
            ClistTokenKind::Number(n) => parts.push(n.to_string()),
            _ => {}
        }
    }

    if parts.len() == 1 {
        // Try to see if the single token was a variable
        if let ClistTokenKind::Variable(v) = &tokens[0].kind {
            return Expression::Variable(v.clone());
        }
    }

    Expression::StringLit(parts.join(" "))
}

fn token_to_expr(token: &ClistToken) -> Expression {
    match &token.kind {
        ClistTokenKind::Number(n) => Expression::Number(*n),
        ClistTokenKind::StringLiteral(s) => Expression::StringLit(s.clone()),
        ClistTokenKind::Variable(v) => Expression::Variable(v.clone()),
        ClistTokenKind::Identifier(s) => Expression::StringLit(s.clone()),
        _ => Expression::StringLit(String::new()),
    }
}

fn token_to_comparison_op(token: &ClistToken) -> Option<ExprOp> {
    match &token.kind {
        ClistTokenKind::Operator(op) => match op.as_str() {
            "=" => Some(ExprOp::Equal),
            "<>" => Some(ExprOp::NotEqual),
            "<" => Some(ExprOp::LessThan),
            ">" => Some(ExprOp::GreaterThan),
            "<=" => Some(ExprOp::LessOrEqual),
            ">=" => Some(ExprOp::GreaterOrEqual),
            _ => None,
        },
        ClistTokenKind::Keyword(kw) => match kw.as_str() {
            "EQ" => Some(ExprOp::Equal),
            "NE" => Some(ExprOp::NotEqual),
            "LT" => Some(ExprOp::LessThan),
            "GT" => Some(ExprOp::GreaterThan),
            "LE" => Some(ExprOp::LessOrEqual),
            "GE" => Some(ExprOp::GreaterOrEqual),
            "AND" => Some(ExprOp::And),
            "OR" => Some(ExprOp::Or),
            _ => None,
        },
        _ => None,
    }
}

fn parse_function_args(tokens: &[ClistToken]) -> Vec<Expression> {
    let mut args = Vec::new();
    let mut current = Vec::new();
    let mut depth = 0;

    for tok in tokens {
        match &tok.kind {
            ClistTokenKind::LeftParen => {
                depth += 1;
                current.push(tok.clone());
            }
            ClistTokenKind::RightParen => {
                if depth == 0 {
                    // End of function args
                    if !current.is_empty() {
                        args.push(parse_expression_from_tokens(&current));
                    }
                    break;
                }
                depth -= 1;
                current.push(tok.clone());
            }
            ClistTokenKind::Comma if depth == 0 => {
                if !current.is_empty() {
                    args.push(parse_expression_from_tokens(&current));
                    current.clear();
                }
            }
            _ => {
                current.push(tok.clone());
            }
        }
    }

    args
}

fn find_matching_paren(tokens: &[ClistToken], start: usize) -> usize {
    let mut depth = 0;
    for (i, tok) in tokens.iter().enumerate().skip(start) {
        match &tok.kind {
            ClistTokenKind::LeftParen => depth += 1,
            ClistTokenKind::RightParen => {
                depth -= 1;
                if depth == 0 {
                    return i;
                }
            }
            _ => {}
        }
    }
    tokens.len() - 1
}

fn reconstruct_tokens(tokens: &[ClistToken]) -> String {
    let mut parts = Vec::new();
    for tok in tokens {
        match &tok.kind {
            ClistTokenKind::Keyword(k) => parts.push(k.clone()),
            ClistTokenKind::Variable(v) => parts.push(format!("&{v}")),
            ClistTokenKind::StringLiteral(s) => parts.push(format!("'{s}'")),
            ClistTokenKind::Number(n) => parts.push(n.to_string()),
            ClistTokenKind::Operator(o) => parts.push(o.clone()),
            ClistTokenKind::LeftParen => parts.push("(".to_string()),
            ClistTokenKind::RightParen => parts.push(")".to_string()),
            ClistTokenKind::Comma => parts.push(",".to_string()),
            ClistTokenKind::Identifier(s) => parts.push(s.clone()),
            ClistTokenKind::Label(l) => parts.push(format!("{l}:")),
            ClistTokenKind::EndOfLine => {}
        }
    }
    parts.join(" ")
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ─── CL-100.1: Source Line Processing ───

    #[test]
    fn test_simple_lines() {
        let source = "SET &A = 1\nSET &B = 2\n";
        let lines = process_source_lines(source);
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0].1, "SET &A = 1");
        assert_eq!(lines[1].1, "SET &B = 2");
    }

    #[test]
    fn test_continuation_plus() {
        let source = "SET &A = +\n  HELLO\n";
        let lines = process_source_lines(source);
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].1, "SET &A = HELLO");
    }

    #[test]
    fn test_continuation_minus() {
        let source = "SET &A = -\n  HELLO\n";
        let lines = process_source_lines(source);
        assert_eq!(lines.len(), 1);
        assert!(lines[0].1.contains("HELLO"));
    }

    // ─── CL-100.2: Tokenizer ───

    #[test]
    fn test_tokenize_set() {
        let tokens = tokenize_line("SET &VAR = 42", 1);
        assert_eq!(tokens.len(), 4);
        assert!(matches!(&tokens[0].kind, ClistTokenKind::Keyword(k) if k == "SET"));
        assert!(matches!(&tokens[1].kind, ClistTokenKind::Variable(v) if v == "VAR"));
        assert!(matches!(&tokens[2].kind, ClistTokenKind::Operator(o) if o == "="));
        assert!(matches!(&tokens[3].kind, ClistTokenKind::Number(42)));
    }

    #[test]
    fn test_tokenize_string_literal() {
        let tokens = tokenize_line("WRITE 'Hello World'", 1);
        assert_eq!(tokens.len(), 2);
        assert!(matches!(&tokens[1].kind, ClistTokenKind::StringLiteral(s) if s == "Hello World"));
    }

    #[test]
    fn test_tokenize_comparison() {
        let tokens = tokenize_line("IF &A EQ &B THEN", 1);
        assert!(tokens.len() >= 4);
        assert!(matches!(&tokens[2].kind, ClistTokenKind::Keyword(k) if k == "EQ"));
    }

    #[test]
    fn test_tokenize_comment() {
        let tokens = tokenize_line("SET &A = 1 /* comment */", 1);
        assert_eq!(tokens.len(), 4);
    }

    // ─── CL-100.3: Statement Parser ───

    #[test]
    fn test_parse_set_statement() {
        let ast = parse_clist("SET &X = 10").unwrap();
        assert_eq!(ast.statements.len(), 1);
        assert!(matches!(&ast.statements[0], ClistStatement::Set { variable, .. } if variable == "X"));
    }

    #[test]
    fn test_parse_write_statement() {
        let ast = parse_clist("WRITE 'Hello'").unwrap();
        assert_eq!(ast.statements.len(), 1);
        assert!(matches!(&ast.statements[0], ClistStatement::Write(_)));
    }

    #[test]
    fn test_parse_goto() {
        let ast = parse_clist("GOTO LOOP").unwrap();
        assert_eq!(ast.statements.len(), 1);
        assert!(matches!(&ast.statements[0], ClistStatement::Goto(l) if l == "LOOP"));
    }

    #[test]
    fn test_parse_label() {
        let ast = parse_clist("LOOP: SET &I = 0").unwrap();
        assert_eq!(ast.statements.len(), 2);
        assert!(matches!(&ast.statements[0], ClistStatement::Label(l) if l == "LOOP"));
    }

    #[test]
    fn test_parse_exit() {
        let ast = parse_clist("EXIT 0").unwrap();
        assert!(matches!(&ast.statements[0], ClistStatement::Exit(Some(_))));
    }

    #[test]
    fn test_parse_exit_no_code() {
        let ast = parse_clist("EXIT").unwrap();
        assert!(matches!(&ast.statements[0], ClistStatement::Exit(None)));
    }

    #[test]
    fn test_parse_control() {
        let ast = parse_clist("CONTROL NOLIST NOMSG").unwrap();
        if let ClistStatement::Control(opts) = &ast.statements[0] {
            assert!(opts.contains(&"NOLIST".to_string()));
            assert!(opts.contains(&"NOMSG".to_string()));
        } else {
            panic!("expected Control statement");
        }
    }

    #[test]
    fn test_parse_proc() {
        let ast = parse_clist("PROC 2").unwrap();
        if let ClistStatement::Proc { positional, .. } = &ast.statements[0] {
            assert_eq!(*positional, 2);
        } else {
            panic!("expected Proc statement");
        }
    }

    #[test]
    fn test_parse_do_end() {
        let source = "DO\nSET &A = 1\nEND";
        let ast = parse_clist(source).unwrap();
        assert_eq!(ast.statements.len(), 1);
        if let ClistStatement::Do { body, .. } = &ast.statements[0] {
            assert_eq!(body.len(), 1);
        } else {
            panic!("expected Do statement");
        }
    }

    #[test]
    fn test_parse_if_then() {
        let source = "IF &A EQ 1 THEN SET &B = 2";
        let ast = parse_clist(source).unwrap();
        assert!(matches!(&ast.statements[0], ClistStatement::If { .. }));
    }

    #[test]
    fn test_parse_select() {
        let source = "SELECT\nWHEN 1 THEN SET &A = 1\nOTHERWISE SET &A = 0\nEND";
        let ast = parse_clist(source).unwrap();
        if let ClistStatement::Select { cases, otherwise } = &ast.statements[0] {
            assert_eq!(cases.len(), 1);
            assert_eq!(otherwise.len(), 1);
        } else {
            panic!("expected Select statement");
        }
    }

    // ─── CL-100.4: Expression Parser ───

    #[test]
    fn test_expr_number() {
        let tokens = tokenize_line("42", 1);
        let expr = parse_expression_from_tokens(&tokens);
        assert!(matches!(expr, Expression::Number(42)));
    }

    #[test]
    fn test_expr_variable() {
        let tokens = tokenize_line("&VAR", 1);
        let expr = parse_expression_from_tokens(&tokens);
        assert!(matches!(expr, Expression::Variable(v) if v == "VAR"));
    }

    #[test]
    fn test_expr_addition() {
        let tokens = tokenize_line("&A + &B", 1);
        let expr = parse_expression_from_tokens(&tokens);
        assert!(matches!(expr, Expression::BinaryOp { op: ExprOp::Add, .. }));
    }

    #[test]
    fn test_expr_comparison() {
        let tokens = tokenize_line("&A EQ &B", 1);
        let expr = parse_expression_from_tokens(&tokens);
        assert!(matches!(expr, Expression::BinaryOp { op: ExprOp::Equal, .. }));
    }

    // ─── CL-100.5: Parser Tests ───

    #[test]
    fn test_full_clist_parse() {
        let source = r#"
PROC 0
CONTROL NOLIST NOMSG
SET &RESULT = 0
SET &I = 1
LOOP: IF &I GT 10 THEN GOTO DONE
  SET &RESULT = &RESULT + &I
  SET &I = &I + 1
  GOTO LOOP
DONE: WRITE &RESULT
EXIT 0
"#;
        let ast = parse_clist(source).unwrap();
        assert!(ast.statements.len() >= 8);
    }

    #[test]
    fn test_error_handler() {
        let source = "ERROR DO\nWRITE 'Error occurred'\nRETURN\nEND";
        let ast = parse_clist(source).unwrap();
        assert!(matches!(&ast.statements[0], ClistStatement::Error(ErrorAction::Body(_))));
    }

    #[test]
    fn test_openfile_getfile() {
        let source = "OPENFILE &INFILE INPUT\nGETFILE &INFILE\nCLOSFILE &INFILE";
        let ast = parse_clist(source).unwrap();
        assert_eq!(ast.statements.len(), 3);
        assert!(matches!(&ast.statements[0], ClistStatement::OpenFile { .. }));
        assert!(matches!(&ast.statements[1], ClistStatement::GetFile(_)));
        assert!(matches!(&ast.statements[2], ClistStatement::CloseFile(_)));
    }

    #[test]
    fn test_tso_command() {
        let source = "ALLOC DA('MY.DATA') SHR";
        let ast = parse_clist(source).unwrap();
        assert!(matches!(&ast.statements[0], ClistStatement::TsoCommand(_)));
    }

    #[test]
    fn test_data_enddata() {
        let source = "DATA\nsome raw text\nmore text\nENDDATA";
        let ast = parse_clist(source).unwrap();
        if let ClistStatement::Data(content) = &ast.statements[0] {
            assert!(content.contains("some raw text"));
            assert!(content.contains("more text"));
        } else {
            panic!("expected Data statement");
        }
    }
}
