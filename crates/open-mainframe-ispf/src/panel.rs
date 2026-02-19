//! ISPF panel definition parser and runtime.
//!
//! Implements the ISPF panel definition language:
//! - Section parsing: )ATTR, )BODY, )INIT, )REINIT, )PROC, )AREA, )MODEL, )END
//! - Attribute character mapping: TYPE(TEXT|INPUT|OUTPUT), INTENS(HIGH|LOW|NON)
//! - Body layout with text fields, input fields, and dynamic areas
//! - Executable statements: assignment, IF/ELSE, VER, VGET, VPUT, TRANS, TRUNC

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Panel AST
// ---------------------------------------------------------------------------

/// A parsed ISPF panel.
#[derive(Debug, Clone)]
pub struct Panel {
    /// Panel name (derived from member name or source identifier).
    pub name: String,
    /// Attribute definitions: character → display attributes.
    pub attrs: HashMap<char, FieldAttr>,
    /// Default attribute characters: (type_text, type_input, type_output).
    pub defaults: (char, char, char),
    /// Body lines (raw text with attribute characters).
    pub body: Vec<String>,
    /// )INIT section executable statements.
    pub init: Vec<PanelStmt>,
    /// )REINIT section executable statements.
    pub reinit: Vec<PanelStmt>,
    /// )PROC section executable statements.
    pub proc_section: Vec<PanelStmt>,
    /// )MODEL lines for TBDISPL (table display model rows).
    pub model: Vec<String>,
    /// )AREA definitions: name → (row, col, height, width).
    pub areas: HashMap<String, AreaDef>,
}

/// Display attributes for a field.
#[derive(Debug, Clone)]
pub struct FieldAttr {
    /// Field type.
    pub field_type: FieldType,
    /// Intensity.
    pub intensity: Intensity,
    /// CAPS mode (auto-uppercase input).
    pub caps: bool,
    /// JUST (justification): LEFT, RIGHT, ASIS.
    pub just: Justification,
    /// PAD character.
    pub pad: Option<char>,
}

impl Default for FieldAttr {
    fn default() -> Self {
        Self {
            field_type: FieldType::Text,
            intensity: Intensity::Low,
            caps: false,
            just: Justification::Left,
            pad: None,
        }
    }
}

/// ISPF field types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldType {
    Text,
    Input,
    Output,
    DataIn,
    DataOut,
}

/// Display intensity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Intensity {
    High,
    Low,
    Non,
}

/// Field justification.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Justification {
    Left,
    Right,
    Asis,
}

/// Area definition from )AREA section.
#[derive(Debug, Clone)]
pub struct AreaDef {
    /// Area name.
    pub name: String,
    /// Number of lines in the area.
    pub depth: usize,
    /// Number of columns in the area.
    pub width: usize,
}

/// Executable statements in )INIT, )REINIT, )PROC sections.
#[derive(Debug, Clone)]
pub enum PanelStmt {
    /// Variable assignment: `&var = value` or `&var = TRANS(&x, a,b, c,d, *,e)`
    Assign {
        var: String,
        value: PanelExpr,
    },
    /// IF condition / ELSE block.
    If {
        cond: PanelCond,
        then_stmts: Vec<PanelStmt>,
        else_stmts: Vec<PanelStmt>,
    },
    /// VER — field verification: `VER (&field,NB)` or `VER (&field,NB,MSG=xxx)`
    Ver {
        field: String,
        checks: Vec<VerCheck>,
        msg: Option<String>,
    },
    /// VGET — get variables from pool.
    VGet {
        vars: Vec<String>,
        pool: VarPool,
    },
    /// VPUT — put variables to pool.
    VPut {
        vars: Vec<String>,
        pool: VarPool,
    },
    /// GOTO label.
    Goto(String),
    /// Label definition.
    Label(String),
    /// REFRESH — refresh field display.
    Refresh(Vec<String>),
}

/// Panel expression (right-hand side of assignment).
#[derive(Debug, Clone)]
pub enum PanelExpr {
    /// Literal string or variable reference.
    Literal(String),
    /// TRANS function: translate using a table.
    Trans {
        var: String,
        pairs: Vec<(String, String)>,
        default: Option<String>,
    },
    /// TRUNC function: truncate at a delimiter.
    Trunc {
        var: String,
        delim: char,
    },
}

/// Panel condition for IF statements.
#[derive(Debug, Clone)]
pub enum PanelCond {
    /// Simple comparison: `&var = value` or `&var NE value`.
    Compare {
        var: String,
        op: CmpOp,
        value: String,
    },
    /// Negation.
    Not(Box<PanelCond>),
    /// AND combination.
    And(Box<PanelCond>, Box<PanelCond>),
    /// OR combination.
    Or(Box<PanelCond>, Box<PanelCond>),
}

/// Comparison operators for panel conditions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CmpOp {
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
}

/// Verification checks for VER statement.
#[derive(Debug, Clone)]
pub enum VerCheck {
    /// NB — not blank.
    NonBlank,
    /// NUM — numeric.
    Numeric,
    /// ALPHA — alphabetic.
    Alpha,
    /// DSNAME — valid dataset name.
    Dsname,
    /// LIST — value must be in list.
    List(Vec<String>),
    /// RANGE — value must be in range.
    Range(String, String),
}

/// ISPF variable pool type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarPool {
    Shared,
    Profile,
    Asis,
}

// ---------------------------------------------------------------------------
//  Panel parsing
// ---------------------------------------------------------------------------

/// Parse an ISPF panel from source text.
pub fn parse_panel(name: &str, source: &str) -> Result<Panel, PanelError> {
    let mut panel = Panel {
        name: name.to_string(),
        attrs: default_attrs(),
        defaults: ('%', '+', '_'),
        body: Vec::new(),
        init: Vec::new(),
        reinit: Vec::new(),
        proc_section: Vec::new(),
        model: Vec::new(),
        areas: HashMap::new(),
    };

    let lines: Vec<&str> = source.lines().collect();
    let mut i = 0;
    let mut current_section = Section::None;

    while i < lines.len() {
        let line = lines[i].trim();

        // Section headers.
        if line.starts_with(')') {
            let upper = line.to_uppercase();
            if upper.starts_with(")ATTR") {
                current_section = Section::Attr;
                // Parse DEFAULT if present.
                if let Some(defaults) = parse_attr_defaults(line) {
                    panel.defaults = defaults;
                }
                i += 1;
                continue;
            } else if upper.starts_with(")BODY") {
                current_section = Section::Body;
                // Parse expansion/width hints (ignored for now).
                i += 1;
                continue;
            } else if upper.starts_with(")INIT") {
                current_section = Section::Init;
                i += 1;
                continue;
            } else if upper.starts_with(")REINIT") {
                current_section = Section::Reinit;
                i += 1;
                continue;
            } else if upper.starts_with(")PROC") {
                current_section = Section::Proc;
                i += 1;
                continue;
            } else if upper.starts_with(")MODEL") {
                current_section = Section::Model;
                i += 1;
                continue;
            } else if upper.starts_with(")AREA") {
                current_section = Section::Area;
                i += 1;
                continue;
            } else if upper.starts_with(")END") {
                break;
            }
        }

        match current_section {
            Section::Attr => {
                if !line.is_empty() {
                    parse_attr_line(line, &mut panel.attrs);
                }
            }
            Section::Body => {
                panel.body.push(lines[i].to_string());
            }
            Section::Init => {
                let (stmts, consumed) = parse_stmts(&lines[i..]);
                panel.init.extend(stmts);
                i += consumed.max(1);
                continue;
            }
            Section::Reinit => {
                let (stmts, consumed) = parse_stmts(&lines[i..]);
                panel.reinit.extend(stmts);
                i += consumed.max(1);
                continue;
            }
            Section::Proc => {
                let (stmts, consumed) = parse_stmts(&lines[i..]);
                panel.proc_section.extend(stmts);
                i += consumed.max(1);
                continue;
            }
            Section::Model => {
                panel.model.push(lines[i].to_string());
            }
            Section::Area => {
                // Parse area definitions: AREA_NAME DEPTH(n) WIDTH(m)
                if !line.is_empty() {
                    parse_area_line(line, &mut panel.areas);
                }
            }
            Section::None => {
                // Lines before any section are ignored.
            }
        }

        i += 1;
    }

    Ok(panel)
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Section {
    None,
    Attr,
    Body,
    Init,
    Reinit,
    Proc,
    Model,
    Area,
}

// ---------------------------------------------------------------------------
//  Panel body field extraction
// ---------------------------------------------------------------------------

/// A field extracted from a panel body.
#[derive(Debug, Clone)]
pub struct PanelField {
    /// Field variable name (empty for text-only fields).
    pub name: String,
    /// Row (0-indexed).
    pub row: usize,
    /// Column (0-indexed).
    pub col: usize,
    /// Field width.
    pub width: usize,
    /// Attribute applied to this field.
    pub attr: FieldAttr,
}

/// Extract fields from a panel body using attribute character mappings.
pub fn extract_fields(panel: &Panel) -> Vec<PanelField> {
    let mut fields = Vec::new();
    let attr_chars: Vec<char> = panel.attrs.keys().copied().collect();

    for (row, line) in panel.body.iter().enumerate() {
        let chars: Vec<char> = line.chars().collect();
        let mut col = 0;

        while col < chars.len() {
            let ch = chars[col];
            if attr_chars.contains(&ch) {
                // Found an attribute character — the field follows.
                let attr = panel.attrs.get(&ch).cloned().unwrap_or_default();
                let field_start = col + 1;
                let mut field_end = field_start;

                // Find field extent: until next attribute character or end of line.
                while field_end < chars.len() && !attr_chars.contains(&chars[field_end]) {
                    field_end += 1;
                }

                let field_text: String = chars[field_start..field_end].iter().collect();
                let field_name = if attr.field_type == FieldType::Input
                    || attr.field_type == FieldType::DataIn
                {
                    // Input fields: the text is the variable name.
                    field_text.trim().to_uppercase()
                } else {
                    String::new()
                };

                fields.push(PanelField {
                    name: field_name,
                    row,
                    col: field_start,
                    width: field_end - field_start,
                    attr,
                });

                col = field_end;
            } else {
                col += 1;
            }
        }
    }

    fields
}

// ---------------------------------------------------------------------------
//  Internal parsing helpers
// ---------------------------------------------------------------------------

fn default_attrs() -> HashMap<char, FieldAttr> {
    let mut attrs = HashMap::new();
    attrs.insert(
        '%',
        FieldAttr {
            field_type: FieldType::Text,
            intensity: Intensity::High,
            ..Default::default()
        },
    );
    attrs.insert(
        '+',
        FieldAttr {
            field_type: FieldType::Text,
            intensity: Intensity::Low,
            ..Default::default()
        },
    );
    attrs.insert(
        '_',
        FieldAttr {
            field_type: FieldType::Input,
            intensity: Intensity::Low,
            caps: true,
            ..Default::default()
        },
    );
    attrs
}

/// Parse )ATTR DEFAULT(%+_) to extract the three default characters.
fn parse_attr_defaults(line: &str) -> Option<(char, char, char)> {
    let upper = line.to_uppercase();
    if let Some(start) = upper.find("DEFAULT(") {
        let after = &line[start + 8..];
        if let Some(end) = after.find(')') {
            let chars: Vec<char> = after[..end].chars().collect();
            if chars.len() >= 3 {
                return Some((chars[0], chars[1], chars[2]));
            }
        }
    }
    None
}

/// Parse an attribute definition line: `% TYPE(TEXT) INTENS(HIGH) CAPS(ON)`
fn parse_attr_line(line: &str, attrs: &mut HashMap<char, FieldAttr>) {
    let trimmed = line.trim();
    if trimmed.is_empty() {
        return;
    }

    let chars: Vec<char> = trimmed.chars().collect();
    if chars.is_empty() {
        return;
    }

    let attr_char = chars[0];
    let rest = &trimmed[1..].trim().to_uppercase();

    let mut field_attr = FieldAttr::default();

    // Parse TYPE(xxx)
    if let Some(val) = extract_paren(rest, "TYPE") {
        field_attr.field_type = match val.as_str() {
            "TEXT" => FieldType::Text,
            "INPUT" => FieldType::Input,
            "OUTPUT" => FieldType::Output,
            "DATAIN" => FieldType::DataIn,
            "DATAOUT" => FieldType::DataOut,
            _ => FieldType::Text,
        };
    }

    // Parse INTENS(xxx)
    if let Some(val) = extract_paren(rest, "INTENS") {
        field_attr.intensity = match val.as_str() {
            "HIGH" => Intensity::High,
            "LOW" => Intensity::Low,
            "NON" => Intensity::Non,
            _ => Intensity::Low,
        };
    }

    // Parse CAPS(ON/OFF)
    if let Some(val) = extract_paren(rest, "CAPS") {
        field_attr.caps = val == "ON";
    }

    // Parse JUST(LEFT/RIGHT/ASIS)
    if let Some(val) = extract_paren(rest, "JUST") {
        field_attr.just = match val.as_str() {
            "RIGHT" => Justification::Right,
            "ASIS" => Justification::Asis,
            _ => Justification::Left,
        };
    }

    // Parse PAD(x)
    if let Some(val) = extract_paren(rest, "PAD") {
        field_attr.pad = val.chars().next();
    }

    attrs.insert(attr_char, field_attr);
}

/// Extract parenthesized value: `KEY(VALUE)` → `"VALUE"`.
fn extract_paren(text: &str, key: &str) -> Option<String> {
    let pat = format!("{key}(");
    if let Some(start) = text.find(&pat) {
        let after = &text[start + pat.len()..];
        if let Some(end) = after.find(')') {
            return Some(after[..end].to_string());
        }
    }
    None
}

/// Parse area definition lines.
fn parse_area_line(line: &str, areas: &mut HashMap<String, AreaDef>) {
    let upper = line.trim().to_uppercase();
    let words: Vec<&str> = upper.split_whitespace().collect();
    if words.is_empty() {
        return;
    }
    let name = words[0].to_string();
    let depth = extract_paren(&upper, "DEPTH")
        .and_then(|v| v.parse::<usize>().ok())
        .unwrap_or(1);
    let width = extract_paren(&upper, "WIDTH")
        .and_then(|v| v.parse::<usize>().ok())
        .unwrap_or(80);

    areas.insert(
        name.clone(),
        AreaDef { name, depth, width },
    );
}

/// Parse executable statements from lines. Returns (statements, lines_consumed).
fn parse_stmts(lines: &[&str]) -> (Vec<PanelStmt>, usize) {
    let mut stmts = Vec::new();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i].trim();

        // Stop at next section header.
        if line.starts_with(')') {
            break;
        }

        if line.is_empty() {
            i += 1;
            continue;
        }

        let upper = line.to_uppercase();

        // VER statement.
        if upper.starts_with("VER ") || upper.starts_with("VER(") {
            if let Some(stmt) = parse_ver(line) {
                stmts.push(stmt);
            }
            i += 1;
            continue;
        }

        // VGET statement.
        if upper.starts_with("VGET ") {
            if let Some(stmt) = parse_vget_vput(line, true) {
                stmts.push(stmt);
            }
            i += 1;
            continue;
        }

        // VPUT statement.
        if upper.starts_with("VPUT ") {
            if let Some(stmt) = parse_vget_vput(line, false) {
                stmts.push(stmt);
            }
            i += 1;
            continue;
        }

        // REFRESH statement.
        if upper.starts_with("REFRESH(") || upper.starts_with("REFRESH ") {
            let vars_str = if upper.starts_with("REFRESH(") {
                extract_paren(&upper, "REFRESH").unwrap_or_default()
            } else {
                upper["REFRESH ".len()..].trim().to_string()
            };
            let vars: Vec<String> = vars_str
                .split(|c: char| c == ',' || c.is_whitespace())
                .filter(|s| !s.is_empty())
                .map(|s| s.trim_start_matches('&').to_string())
                .collect();
            stmts.push(PanelStmt::Refresh(vars));
            i += 1;
            continue;
        }

        // IF statement.
        if upper.starts_with("IF ") {
            let (stmt, consumed) = parse_if(&lines[i..]);
            if let Some(s) = stmt {
                stmts.push(s);
            }
            i += consumed;
            continue;
        }

        // Assignment: `&var = value` or `.ATTR(..) = ...`
        if line.starts_with('&') || line.starts_with('.') {
            if let Some(stmt) = parse_assignment(line) {
                stmts.push(stmt);
            }
            i += 1;
            continue;
        }

        // Label: `LABEL:`
        if let Some(stripped) = line.strip_suffix(':') {
            let label = stripped.trim().to_uppercase();
            stmts.push(PanelStmt::Label(label));
            i += 1;
            continue;
        }

        // GOTO.
        if let Some(stripped) = upper.strip_prefix("GOTO ") {
            let label = stripped.trim().to_string();
            stmts.push(PanelStmt::Goto(label));
            i += 1;
            continue;
        }

        // Skip unrecognized lines.
        i += 1;
    }

    (stmts, i)
}

/// Parse a VER statement.
fn parse_ver(line: &str) -> Option<PanelStmt> {
    // VER (&field,NB,MSG=ISRZ002) or VER(&field,NB,DSNAME)
    let content = if let Some(start) = line.find('(') {
        let end = line.rfind(')')?;
        &line[start + 1..end]
    } else {
        return None;
    };

    let parts: Vec<&str> = content.split(',').map(|s| s.trim()).collect();
    if parts.is_empty() {
        return None;
    }

    let field = parts[0].trim_start_matches('&').to_uppercase();
    let mut checks = Vec::new();
    let mut msg = None;

    for part in &parts[1..] {
        let upper = part.to_uppercase();
        if let Some(stripped) = upper.strip_prefix("MSG=") {
            msg = Some(stripped.to_string());
        } else {
            match upper.as_str() {
                "NB" | "NONBLANK" => checks.push(VerCheck::NonBlank),
                "NUM" | "NUMERIC" => checks.push(VerCheck::Numeric),
                "ALPHA" | "ALPHABETIC" => checks.push(VerCheck::Alpha),
                "DSNAME" | "DSNAM" => checks.push(VerCheck::Dsname),
                _ => {}
            }
        }
    }

    Some(PanelStmt::Ver { field, checks, msg })
}

/// Parse VGET or VPUT statement.
fn parse_vget_vput(line: &str, is_get: bool) -> Option<PanelStmt> {
    let upper = line.to_uppercase();
    let after_keyword = if is_get {
        upper.strip_prefix("VGET ")?
    } else {
        upper.strip_prefix("VPUT ")?
    };

    // Parse (var1 var2 ...) POOL
    let vars_str = if let Some(start) = after_keyword.find('(') {
        let end = after_keyword.find(')')?;
        &after_keyword[start + 1..end]
    } else {
        after_keyword.split_whitespace().next()?
    };

    let vars: Vec<String> = vars_str
        .split_whitespace()
        .map(|s| s.trim_start_matches('&').to_string())
        .collect();

    let remainder = if let Some(end) = after_keyword.find(')') {
        after_keyword[end + 1..].trim().to_uppercase()
    } else {
        String::new()
    };

    let pool = if remainder.contains("PROFILE") {
        VarPool::Profile
    } else if remainder.contains("ASIS") {
        VarPool::Asis
    } else {
        VarPool::Shared
    };

    if is_get {
        Some(PanelStmt::VGet { vars, pool })
    } else {
        Some(PanelStmt::VPut { vars, pool })
    }
}

/// Parse an IF / ELSE construct. Returns (statement, lines_consumed).
fn parse_if(lines: &[&str]) -> (Option<PanelStmt>, usize) {
    if lines.is_empty() {
        return (None, 0);
    }

    let first = lines[0].trim();
    let upper = first.to_uppercase();

    // Parse condition from `IF (...)`
    let cond = if let Some(start) = upper.find('(') {
        // Find matching close paren.
        let end = upper.rfind(')').unwrap_or(upper.len() - 1);
        let cond_str = &first[start + 1..end];
        parse_condition(cond_str)
    } else {
        // Condition without parens: IF &var = value
        let rest = &first[3..].trim();
        parse_condition(rest)
    };

    let cond = match cond {
        Some(c) => c,
        None => return (None, 1),
    };

    let mut then_stmts = Vec::new();
    let mut else_stmts = Vec::new();
    let mut i = 1;
    let mut in_else = false;

    while i < lines.len() {
        let line = lines[i].trim();
        let line_upper = line.to_uppercase();

        if line.starts_with(')') {
            break;
        }

        if line_upper == "ELSE" || line_upper.starts_with("ELSE ") {
            in_else = true;
            // If there's content on the ELSE line itself...
            let after_else = line[4..].trim();
            if !after_else.is_empty() {
                if let Some(stmt) = parse_assignment(after_else) {
                    else_stmts.push(stmt);
                }
            }
            i += 1;
            continue;
        }

        if line.is_empty() {
            i += 1;
            continue;
        }

        // Simple single-line statement inside IF/ELSE.
        let stmt = parse_assignment(line);
        if let Some(s) = stmt {
            if in_else {
                else_stmts.push(s);
            } else {
                then_stmts.push(s);
            }
        }
        i += 1;
    }

    (
        Some(PanelStmt::If {
            cond,
            then_stmts,
            else_stmts,
        }),
        i,
    )
}

/// Parse a simple condition: `&var = value` or `&var NE value`.
fn parse_condition(text: &str) -> Option<PanelCond> {
    let trimmed = text.trim();

    // Try operators: =, NE, EQ, GT, LT, GE, LE, !=, ¬=
    let ops = [
        ("!=", CmpOp::Ne),
        ("NE", CmpOp::Ne),
        ("EQ", CmpOp::Eq),
        ("GE", CmpOp::Ge),
        ("LE", CmpOp::Le),
        ("GT", CmpOp::Gt),
        ("LT", CmpOp::Lt),
        ("=", CmpOp::Eq),
        (">", CmpOp::Gt),
        ("<", CmpOp::Lt),
    ];

    for (op_str, op) in &ops {
        // For word operators (NE, EQ, etc.), require whitespace boundaries.
        let find_pos = if op_str.chars().all(|c| c.is_alphabetic()) {
            trimmed.to_uppercase().find(&format!(" {op_str} "))
                .map(|p| p + 1) // Skip the leading space.
        } else {
            trimmed.find(op_str)
        };

        if let Some(pos) = find_pos {
            let var = trimmed[..pos].trim().trim_start_matches('&').to_uppercase();
            let value = trimmed[pos + op_str.len()..].trim().trim_matches('\'').to_string();
            return Some(PanelCond::Compare {
                var,
                op: *op,
                value,
            });
        }
    }

    None
}

/// Parse an assignment statement: `&var = value`.
fn parse_assignment(line: &str) -> Option<PanelStmt> {
    let trimmed = line.trim();
    let eq_pos = trimmed.find('=')?;
    let var = trimmed[..eq_pos]
        .trim()
        .trim_start_matches('&')
        .to_uppercase();
    let rhs = trimmed[eq_pos + 1..].trim();

    let value = if rhs.to_uppercase().starts_with("TRANS(") {
        parse_trans_expr(rhs)
    } else if rhs.to_uppercase().starts_with("TRUNC(") {
        parse_trunc_expr(rhs)
    } else {
        PanelExpr::Literal(rhs.to_string())
    };

    Some(PanelStmt::Assign { var, value })
}

/// Parse TRANS(&var, a,b, c,d, *,e).
fn parse_trans_expr(text: &str) -> PanelExpr {
    let upper = text.to_uppercase();
    let content = if let (Some(start), Some(end)) = (upper.find('('), upper.rfind(')')) {
        &text[start + 1..end]
    } else {
        return PanelExpr::Literal(text.to_string());
    };

    let parts: Vec<&str> = content.split(',').map(|s| s.trim()).collect();
    if parts.is_empty() {
        return PanelExpr::Literal(text.to_string());
    }

    let var = parts[0].trim_start_matches('&').to_uppercase();
    let mut pairs = Vec::new();
    let mut default = None;

    let mut i = 1;
    while i + 1 < parts.len() {
        let key = parts[i].trim_matches('\'').to_string();
        let val = parts[i + 1].trim_matches('\'').to_string();
        if key == "*" {
            default = Some(val);
        } else {
            pairs.push((key, val));
        }
        i += 2;
    }

    PanelExpr::Trans {
        var,
        pairs,
        default,
    }
}

/// Parse TRUNC(&var, delim).
fn parse_trunc_expr(text: &str) -> PanelExpr {
    let content = if let (Some(start), Some(end)) = (text.find('('), text.rfind(')')) {
        &text[start + 1..end]
    } else {
        return PanelExpr::Literal(text.to_string());
    };

    let parts: Vec<&str> = content.split(',').map(|s| s.trim()).collect();
    if parts.len() < 2 {
        return PanelExpr::Literal(text.to_string());
    }

    let var = parts[0].trim_start_matches('&').to_uppercase();
    let delim = parts[1].trim_matches('\'').chars().next().unwrap_or(' ');

    PanelExpr::Trunc { var, delim }
}

// ---------------------------------------------------------------------------
//  Error
// ---------------------------------------------------------------------------

/// Panel parsing/processing error.
#[derive(Debug, Clone, thiserror::Error)]
pub enum PanelError {
    #[error("Panel error: {0}")]
    General(String),
    #[error("Verification failed on field {field}: {message}")]
    Verification { field: String, message: String },
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_panel() {
        let source = r#")ATTR DEFAULT(%+_)
% TYPE(TEXT)  INTENS(HIGH)
+ TYPE(TEXT)  INTENS(LOW)
_ TYPE(INPUT) INTENS(LOW) CAPS(ON)
)BODY
%COMMAND ===>_ZCMD
+
+DATASET NAME%===>_DSN
)INIT
  &ZCMD = ''
  VGET (ZPREFIX) PROFILE
)PROC
  VER (&DSN,NB,MSG=ISRZ002)
)END
"#;
        let panel = parse_panel("TEST", source).unwrap();
        assert_eq!(panel.name, "TEST");
        assert_eq!(panel.body.len(), 3);
        assert_eq!(panel.init.len(), 2);
        assert_eq!(panel.proc_section.len(), 1);
    }

    #[test]
    fn test_parse_attr_defaults() {
        let result = parse_attr_defaults(")ATTR DEFAULT(%+_)");
        assert_eq!(result, Some(('%', '+', '_')));
    }

    #[test]
    fn test_parse_attr_line() {
        let mut attrs = HashMap::new();
        parse_attr_line("# TYPE(INPUT) INTENS(HIGH) CAPS(ON)", &mut attrs);
        let attr = attrs.get(&'#').unwrap();
        assert_eq!(attr.field_type, FieldType::Input);
        assert_eq!(attr.intensity, Intensity::High);
        assert!(attr.caps);
    }

    #[test]
    fn test_extract_fields() {
        let source = r#")ATTR DEFAULT(%+_)
% TYPE(TEXT)  INTENS(HIGH)
_ TYPE(INPUT) INTENS(LOW)
)BODY
%COMMAND ===>_ZCMD
)END
"#;
        let panel = parse_panel("TEST", source).unwrap();
        let fields = extract_fields(&panel);
        // Should find: text field "COMMAND ===>", input field "ZCMD"
        assert!(fields.len() >= 2);
        let input_field = fields.iter().find(|f| f.name == "ZCMD");
        assert!(input_field.is_some());
    }

    #[test]
    fn test_parse_ver_statement() {
        let stmt = parse_ver("VER (&DSN,NB,MSG=ISRZ002)").unwrap();
        if let PanelStmt::Ver { field, checks, msg } = stmt {
            assert_eq!(field, "DSN");
            assert_eq!(checks.len(), 1);
            assert_eq!(msg, Some("ISRZ002".to_string()));
        } else {
            panic!("Expected VER statement");
        }
    }

    #[test]
    fn test_parse_vget() {
        let stmt = parse_vget_vput("VGET (ZPREFIX ZUSER) PROFILE", true).unwrap();
        if let PanelStmt::VGet { vars, pool } = stmt {
            assert_eq!(vars, vec!["ZPREFIX", "ZUSER"]);
            assert_eq!(pool, VarPool::Profile);
        } else {
            panic!("Expected VGET statement");
        }
    }

    #[test]
    fn test_parse_vput() {
        let stmt = parse_vget_vput("VPUT (DSN) SHARED", false).unwrap();
        if let PanelStmt::VPut { vars, pool } = stmt {
            assert_eq!(vars, vec!["DSN"]);
            assert_eq!(pool, VarPool::Shared);
        } else {
            panic!("Expected VPUT statement");
        }
    }

    #[test]
    fn test_parse_assignment() {
        let stmt = parse_assignment("&DSN = 'MY.DATASET'").unwrap();
        if let PanelStmt::Assign { var, value } = stmt {
            assert_eq!(var, "DSN");
            if let PanelExpr::Literal(v) = value {
                assert_eq!(v, "'MY.DATASET'");
            } else {
                panic!("Expected literal");
            }
        } else {
            panic!("Expected assignment");
        }
    }

    #[test]
    fn test_parse_trans_expr() {
        let expr = parse_trans_expr("TRANS(&OPT, 1,'PANEL(MAIN1)', 2,'PANEL(MAIN2)', *,'?')");
        if let PanelExpr::Trans { var, pairs, default } = expr {
            assert_eq!(var, "OPT");
            assert_eq!(pairs.len(), 2);
            assert_eq!(pairs[0], ("1".to_string(), "PANEL(MAIN1)".to_string()));
            assert_eq!(default, Some("?".to_string()));
        } else {
            panic!("Expected TRANS expression");
        }
    }

    #[test]
    fn test_parse_condition() {
        let cond = parse_condition("&OPT = 1").unwrap();
        if let PanelCond::Compare { var, op, value } = cond {
            assert_eq!(var, "OPT");
            assert_eq!(op, CmpOp::Eq);
            assert_eq!(value, "1");
        } else {
            panic!("Expected Compare condition");
        }
    }

    #[test]
    fn test_parse_condition_ne() {
        let cond = parse_condition("&STATUS NE 'ACTIVE'").unwrap();
        if let PanelCond::Compare { var, op, value } = cond {
            assert_eq!(var, "STATUS");
            assert_eq!(op, CmpOp::Ne);
            assert_eq!(value, "ACTIVE");
        } else {
            panic!("Expected Compare condition");
        }
    }

    #[test]
    fn test_parse_if_then_else() {
        let lines = [
            "IF (&OPT = 1)",
            "  &SEL = 'PANEL(MAIN1)'",
            "ELSE",
            "  &SEL = 'PANEL(MAIN2)'",
        ];
        let (stmt, consumed) = parse_if(&lines);
        assert_eq!(consumed, 4);
        let stmt = stmt.unwrap();
        if let PanelStmt::If { then_stmts, else_stmts, .. } = stmt {
            assert_eq!(then_stmts.len(), 1);
            assert_eq!(else_stmts.len(), 1);
        } else {
            panic!("Expected IF statement");
        }
    }

    #[test]
    fn test_full_panel_with_proc() {
        let source = r#")ATTR DEFAULT(%+_)
% TYPE(TEXT)  INTENS(HIGH)
+ TYPE(TEXT)  INTENS(LOW)
_ TYPE(INPUT) INTENS(LOW) CAPS(ON)
)BODY
%OPTION ===>_ZCMD
+
+ 1 +Browse     +Browse a dataset
+ 2 +Edit       +Edit a dataset
)INIT
  VGET (ZPREFIX) PROFILE
  &ZCMD = ''
)PROC
  VER (&ZCMD,NB)
  IF (&ZCMD = 1)
    &SEL = 'PGM(ISRBRO)'
  ELSE
    &SEL = 'PGM(ISREDIT)'
)END
"#;
        let panel = parse_panel("MENU", source).unwrap();
        assert_eq!(panel.name, "MENU");
        assert_eq!(panel.body.len(), 4);
        assert_eq!(panel.init.len(), 2);
        // proc: VER + IF = 2 statements
        assert_eq!(panel.proc_section.len(), 2);
    }
}
