//! Interpret command implementation.
//!
//! Runs COBOL programs directly using the tree-walking interpreter.

use std::collections::HashMap;
use std::path::PathBuf;

use miette::{IntoDiagnostic, Result, WrapErr};

use zos_cobol::ast::{
    DataItem, DataItemName, Expression, LiteralKind, ProcedureBody, Program, Statement,
};
use zos_cobol::{scan, FileId, SourceFile, SourceFormat};
use zos_runtime::interpreter::{
    DataItemMeta, Environment, SimpleBinaryOp, SimpleCompareOp, SimpleCondition, SimpleExpr,
    SimpleProgram, SimpleStatement,
};

use super::cics_bridge::CicsBridge;

/// Load and parse a single COBOL source file into a SimpleProgram.
fn load_program(path: &std::path::Path) -> Result<SimpleProgram> {
    let source_text = std::fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read: {}", path.display()))?;

    let source_file = SourceFile::from_text(FileId(0), source_text, SourceFormat::Fixed);
    let (tokens, lex_errors) = scan(&source_file);

    if !lex_errors.is_empty() {
        for err in &lex_errors {
            tracing::error!("Lex error: {:?}", err);
        }
        return Err(miette::miette!(
            "Lexing failed with {} errors in {}",
            lex_errors.len(),
            path.display()
        ));
    }

    let (program, parse_errors) = zos_cobol::parser::parse(tokens);

    if !parse_errors.is_empty() {
        for err in &parse_errors {
            tracing::error!("Parse error: {:?}", err);
        }
        return Err(miette::miette!(
            "Parse failed with {} errors in {}",
            parse_errors.len(),
            path.display()
        ));
    }

    let program = program.ok_or_else(|| miette::miette!("Failed to parse program"))?;
    convert_program(&program)
}

/// Try to find a COBOL source file for a program name in a directory.
fn find_program_source(program_name: &str, search_dir: &std::path::Path) -> Option<PathBuf> {
    let name_upper = program_name.to_uppercase();
    // Try common naming patterns: NAME.cbl, NAME.CBL, NAME.cob
    for ext in &["cbl", "CBL", "cob", "COB"] {
        let candidate = search_dir.join(format!("{}.{}", name_upper, ext));
        if candidate.exists() {
            return Some(candidate);
        }
    }
    None
}

/// Interpret a COBOL program directly.
pub fn interpret(input: PathBuf) -> Result<()> {
    tracing::info!("Interpreting: {}", input.display());

    let simple = load_program(&input)?;
    let program_name = simple.name.clone();
    tracing::info!("Program: {}", program_name);

    // Check if program uses CICS (has any ExecCics statements)
    let uses_cics = has_cics_statements(&simple);

    if !uses_cics {
        // Simple non-CICS program - execute directly
        let mut env = Environment::new();
        let rc = zos_runtime::interpreter::execute(&simple, &mut env)
            .map_err(|e| miette::miette!("Runtime error: {}", e))?;
        tracing::info!("Program completed with RC={}", rc);
        return if rc == 0 {
            Ok(())
        } else {
            Err(miette::miette!("Program {} exited with RC={}", program_name, rc))
        };
    }

    // CICS program - set up bridge with XCTL dispatch support
    tracing::info!("CICS program detected, installing CICS bridge");
    let search_dir = input.parent().unwrap_or(std::path::Path::new(".")).to_path_buf();

    // Program registry: cache loaded programs by name
    let mut program_cache: HashMap<String, SimpleProgram> = HashMap::new();
    program_cache.insert(program_name.to_uppercase(), simple);

    let bridge = CicsBridge::new("CARD", "T001");
    let mut env = Environment::new().with_cics_handler(Box::new(bridge));

    let mut current_program = program_name.to_uppercase();
    let max_xctl_chain = 50; // Safety limit to prevent infinite XCTL loops

    for xctl_depth in 0..max_xctl_chain {
        let program = program_cache.get(&current_program).cloned();
        let program = match program {
            Some(p) => p,
            None => {
                // Try to load the program from the search directory
                match find_program_source(&current_program, &search_dir) {
                    Some(path) => {
                        tracing::info!("Loading program {} from {}", current_program, path.display());
                        let p = load_program(&path)?;
                        program_cache.insert(current_program.clone(), p.clone());
                        p
                    }
                    None => {
                        return Err(miette::miette!(
                            "XCTL target program not found: {} (searched in {})",
                            current_program,
                            search_dir.display()
                        ));
                    }
                }
            }
        };

        tracing::info!(
            "Executing program {} (XCTL depth {})",
            current_program,
            xctl_depth
        );

        // Resume environment for new program (clear stopped state)
        env.resume();

        // Execute the program
        let rc = zos_runtime::interpreter::execute(&program, &mut env)
            .map_err(|e| miette::miette!("Runtime error in {}: {}", current_program, e))?;

        // Check if XCTL was issued - extract handler to inspect state
        if let Some(mut handler) = env.cics_handler.take() {
            let bridge = handler
                .as_any_mut()
                .and_then(|a| a.downcast_mut::<CicsBridge>());

            if let Some(bridge) = bridge {
                if let Some(ref xctl_target) = bridge.xctl_program.clone() {
                    tracing::info!(
                        "XCTL from {} to {} (RC={})",
                        current_program,
                        xctl_target,
                        rc
                    );
                    current_program = xctl_target.to_uppercase();
                    bridge.reset_for_xctl();
                    env.cics_handler = Some(handler);
                    continue;
                }

                if bridge.returned {
                    if let Some(ref transid) = bridge.return_transid.clone() {
                        tracing::info!(
                            "RETURN TRANSID({}) from {} (RC={})",
                            transid,
                            current_program,
                            rc
                        );
                    } else {
                        tracing::info!("RETURN from {} (RC={})", current_program, rc);
                    }
                    env.cics_handler = Some(handler);
                    break;
                }
            }

            env.cics_handler = Some(handler);
        }

        // Normal completion (no XCTL, no RETURN) - done
        tracing::info!("Program {} completed with RC={}", current_program, rc);
        break;
    }

    Ok(())
}

/// Convert a COBOL AST Program to SimpleProgram.
fn convert_program(program: &Program) -> Result<SimpleProgram> {
    let name = program.identification.program_id.name.clone();

    // Collect data items and initial values
    let mut data_items = Vec::new();
    let mut initial_values = Vec::new();
    if let Some(ref data) = program.data {
        collect_data_items(&data.working_storage, &mut data_items, &mut initial_values);
    }

    // Convert statements
    let mut statements = Vec::new();
    let mut paragraphs = HashMap::new();

    // Add initial value assignments at the start
    for (name, value) in initial_values {
        statements.push(SimpleStatement::Move {
            from: value,
            to: vec![name],
        });
    }

    if let Some(ref procedure) = program.procedure {
        match &procedure.body {
            ProcedureBody::Statements(stmts) => {
                // Append statements after initial values
                for stmt in stmts {
                    if let Some(s) = convert_statement(stmt)? {
                        statements.push(s);
                    }
                }
            }
            ProcedureBody::Paragraphs(paras) => {
                let mut is_first = true;
                for para in paras {
                    let mut para_stmts = Vec::new();
                    for stmt in &para.statements {
                        if let Some(s) = convert_statement(stmt)? {
                            para_stmts.push(s);
                        }
                    }
                    // First paragraph becomes main (append to initial value statements)
                    if is_first {
                        statements.extend(para_stmts);
                        is_first = false;
                    } else {
                        paragraphs.insert(para.name.to_uppercase(), para_stmts);
                    }
                }
            }
            ProcedureBody::Sections(sections) => {
                let mut is_first = true;
                for section in sections {
                    for para in &section.paragraphs {
                        let mut para_stmts = Vec::new();
                        for stmt in &para.statements {
                            if let Some(s) = convert_statement(stmt)? {
                                para_stmts.push(s);
                            }
                        }
                        // First section's first paragraph becomes main (append to initial values)
                        if is_first {
                            statements.extend(para_stmts);
                            is_first = false;
                        } else {
                            paragraphs.insert(para.name.to_uppercase(), para_stmts);
                        }
                    }
                }
            }
        }
    }

    Ok(SimpleProgram {
        name,
        data_items,
        statements,
        paragraphs,
    })
}

/// Collect data items from DATA DIVISION.
fn collect_data_items(
    items: &[DataItem],
    out: &mut Vec<(String, DataItemMeta)>,
    inits: &mut Vec<(String, SimpleExpr)>,
) {
    for item in items {
        if let DataItemName::Named(ref name) = item.name {
            let meta = DataItemMeta {
                size: item.picture.as_ref().map(|p| p.size as usize).unwrap_or(80),
                decimals: item
                    .picture
                    .as_ref()
                    .map(|p| p.decimal_positions)
                    .unwrap_or(0),
                is_numeric: item
                    .picture
                    .as_ref()
                    .map(|p| {
                        matches!(
                            p.category,
                            zos_cobol::ast::PictureCategory::Numeric
                                | zos_cobol::ast::PictureCategory::NumericEdited
                        )
                    })
                    .unwrap_or(false),
                picture: item.picture.as_ref().map(|p| p.picture.clone()),
            };
            out.push((name.clone(), meta));

            // Handle VALUE clause
            if let Some(ref value) = item.value {
                if let Ok(expr) = convert_literal(value) {
                    inits.push((name.clone(), expr));
                }
            }
        }
        // Recurse into children
        collect_data_items(&item.children, out, inits);
    }
}

/// Convert a Literal to SimpleExpr.
fn convert_literal(lit: &zos_cobol::ast::Literal) -> Result<SimpleExpr> {
    match &lit.kind {
        LiteralKind::Integer(n) => Ok(SimpleExpr::Integer(*n)),
        LiteralKind::Decimal(s) => {
            let n: i64 = s.parse().unwrap_or(0);
            Ok(SimpleExpr::Integer(n))
        }
        LiteralKind::String(s) => Ok(SimpleExpr::String(s.clone())),
        LiteralKind::Hex(s) => Ok(SimpleExpr::String(s.clone())),
        LiteralKind::Figurative(f) => {
            use zos_cobol::ast::FigurativeConstant;
            match f {
                FigurativeConstant::Zero => Ok(SimpleExpr::Integer(0)),
                FigurativeConstant::Space => Ok(SimpleExpr::String(" ".to_string())),
                FigurativeConstant::HighValue => Ok(SimpleExpr::String(
                    String::from_utf8_lossy(&[0xFF]).to_string(),
                )),
                FigurativeConstant::LowValue => Ok(SimpleExpr::String(
                    String::from_utf8_lossy(&[0x00]).to_string(),
                )),
                FigurativeConstant::Quote => Ok(SimpleExpr::String("\"".to_string())),
                FigurativeConstant::All => Ok(SimpleExpr::String("".to_string())),
            }
        }
        LiteralKind::AllOf(inner) => convert_literal(inner),
    }
}

/// Convert a COBOL Statement to SimpleStatement.
fn convert_statement(stmt: &Statement) -> Result<Option<SimpleStatement>> {
    match stmt {
        Statement::Display(d) => {
            let items = d
                .items
                .iter()
                .filter_map(|e| convert_expr(e).ok())
                .collect();
            Ok(Some(SimpleStatement::Display {
                items,
                no_advancing: d.no_advancing,
            }))
        }

        Statement::Accept(a) => Ok(Some(SimpleStatement::Accept {
            target: a.target.name.clone(),
        })),

        Statement::Move(m) => {
            let from = convert_expr(&m.from)?;
            let to = m.to.iter().map(|q| q.name.clone()).collect();
            Ok(Some(SimpleStatement::Move { from, to }))
        }

        Statement::Compute(c) => {
            if let Some(first) = c.targets.first() {
                let expr = convert_expr(&c.expression)?;
                Ok(Some(SimpleStatement::Compute {
                    target: first.name.name.clone(),
                    expr,
                }))
            } else {
                Ok(None)
            }
        }

        Statement::Add(a) => {
            let values = a
                .operands
                .iter()
                .filter_map(|e| convert_expr(e).ok())
                .collect();
            let to = a.to.iter().map(|t| t.name.name.clone()).collect();
            Ok(Some(SimpleStatement::Add { values, to }))
        }

        Statement::Subtract(s) => {
            let values = s
                .operands
                .iter()
                .filter_map(|e| convert_expr(e).ok())
                .collect();
            let from = s.from.iter().map(|t| t.name.name.clone()).collect();
            Ok(Some(SimpleStatement::Subtract { values, from }))
        }

        Statement::Multiply(m) => {
            let value = convert_expr(&m.operand)?;
            let by = convert_expr(&m.by)?;
            let giving = m.giving.first().map(|t| t.name.name.clone());
            Ok(Some(SimpleStatement::Multiply { value, by, giving }))
        }

        Statement::Divide(d) => {
            let value = convert_expr(&d.operand)?;
            let into = convert_expr(&d.into_or_by)?;
            let giving = d.giving.first().map(|t| t.name.name.clone());
            Ok(Some(SimpleStatement::Divide {
                value,
                into,
                giving,
            }))
        }

        Statement::If(i) => {
            let condition = convert_condition(&i.condition)?;
            let then_branch = i
                .then_branch
                .iter()
                .filter_map(|s| convert_statement(s).ok().flatten())
                .collect();
            let else_branch = i.else_branch.as_ref().map(|stmts| {
                stmts
                    .iter()
                    .filter_map(|s| convert_statement(s).ok().flatten())
                    .collect()
            });
            Ok(Some(SimpleStatement::If {
                condition,
                then_branch,
                else_branch,
            }))
        }

        Statement::Perform(p) => {
            // Inline PERFORM with statements
            if let Some(ref inline_stmts) = p.inline {
                let stmts: Vec<SimpleStatement> = inline_stmts
                    .iter()
                    .filter_map(|s| convert_statement(s).ok().flatten())
                    .collect();
                let until = p.until.as_ref().and_then(|c| convert_condition(c).ok());
                return Ok(Some(SimpleStatement::PerformInline {
                    until,
                    statements: stmts,
                }));
            }

            let target = p
                .target
                .as_ref()
                .map(|t| t.name.clone())
                .unwrap_or_default();
            let times = p.times.as_ref().and_then(|e| {
                if let Expression::Literal(l) = e {
                    if let LiteralKind::Integer(n) = l.kind {
                        return Some(n as u32);
                    }
                }
                None
            });
            Ok(Some(SimpleStatement::Perform { target, times }))
        }

        Statement::StopRun(s) => {
            let return_code = s.return_code.as_ref().and_then(|e| {
                if let Expression::Literal(l) = e {
                    if let LiteralKind::Integer(n) = l.kind {
                        return Some(n as i32);
                    }
                }
                None
            });
            Ok(Some(SimpleStatement::StopRun { return_code }))
        }

        Statement::Continue(_) => Ok(None),
        Statement::Exit(_) => Ok(None),

        Statement::ExecCics(e) => {
            let options = e
                .options
                .iter()
                .map(|opt| {
                    let value = opt.value.as_ref().map(|v| convert_expr(v)).transpose()?;
                    Ok((opt.name.clone(), value))
                })
                .collect::<Result<Vec<_>>>()?;
            Ok(Some(SimpleStatement::ExecCics {
                command: e.command.clone(),
                options,
            }))
        }

        Statement::Evaluate(e) => {
            let subjects = e
                .subjects
                .iter()
                .filter_map(|s| convert_expr(s).ok())
                .collect();

            let mut when_clauses: Vec<zos_runtime::interpreter::SimpleWhenClause> = e
                .when_clauses
                .iter()
                .filter_map(|w| {
                    // Use first condition (simplified - ignore ALSO for now)
                    let condition = if let Some(cond) = w.conditions.first() {
                        convert_when_condition(cond).ok()?
                    } else {
                        return None;
                    };

                    let statements: Vec<SimpleStatement> = w
                        .statements
                        .iter()
                        .filter_map(|s| convert_statement(s).ok().flatten())
                        .collect();

                    Some(zos_runtime::interpreter::SimpleWhenClause {
                        condition,
                        statements,
                    })
                })
                .collect();

            // Add WHEN OTHER as a catch-all clause
            if let Some(other_stmts) = &e.when_other {
                let statements: Vec<SimpleStatement> = other_stmts
                    .iter()
                    .filter_map(|s| convert_statement(s).ok().flatten())
                    .collect();
                when_clauses.push(zos_runtime::interpreter::SimpleWhenClause {
                    condition: SimpleCondition::Compare {
                        left: SimpleExpr::Integer(1),
                        op: SimpleCompareOp::Equal,
                        right: SimpleExpr::Integer(1),
                    },
                    statements,
                });
            }

            Ok(Some(SimpleStatement::Evaluate {
                subjects,
                when_clauses,
            }))
        }

        Statement::GoTo(g) => {
            let target = g.targets.first().cloned().unwrap_or_default();
            Ok(Some(SimpleStatement::GoTo { target }))
        }

        Statement::Initialize(i) => {
            let targets = i.variables.iter().map(|v| v.name.clone()).collect();
            Ok(Some(SimpleStatement::Initialize { targets }))
        }

        Statement::Set(s) => {
            use zos_cobol::ast::SetMode;
            match &s.mode {
                SetMode::ConditionTo { target, value } => {
                    // SET condition-name TO TRUE/FALSE
                    // Map as setting the condition variable to 1 (TRUE) or 0 (FALSE)
                    let val = if *value {
                        SimpleExpr::Integer(1)
                    } else {
                        SimpleExpr::Integer(0)
                    };
                    Ok(Some(SimpleStatement::Set {
                        target: target.name.clone(),
                        value: val,
                    }))
                }
                SetMode::IndexTo { targets, value } => {
                    let val = convert_expr(value)?;
                    if let Some(first) = targets.first() {
                        Ok(Some(SimpleStatement::Set {
                            target: first.name.clone(),
                            value: val,
                        }))
                    } else {
                        Ok(None)
                    }
                }
                SetMode::IndexUpDown { targets, up, value } => {
                    let val = convert_expr(value)?;
                    if let Some(first) = targets.first() {
                        let op = if *up { SimpleBinaryOp::Add } else { SimpleBinaryOp::Subtract };
                        Ok(Some(SimpleStatement::Compute {
                            target: first.name.clone(),
                            expr: SimpleExpr::Binary {
                                left: Box::new(SimpleExpr::Variable(first.name.clone())),
                                op,
                                right: Box::new(val),
                            },
                        }))
                    } else {
                        Ok(None)
                    }
                }
                SetMode::AddressOf { .. } => {
                    // Pointer operations not meaningful in interpreter
                    Ok(None)
                }
            }
        }

        Statement::Call(c) => {
            let program = convert_expr(&c.program)?;
            let using = c.using.iter().filter_map(|a| {
                if let Expression::Variable(q) = &a.value {
                    Some(q.name.clone())
                } else {
                    None
                }
            }).collect();
            Ok(Some(SimpleStatement::Call { program, using }))
        }

        Statement::String(s) => {
            let sources = s.sources.iter()
                .filter_map(|src| convert_expr(&src.value).ok())
                .collect();
            Ok(Some(SimpleStatement::StringConcat {
                sources,
                into: s.into.name.clone(),
            }))
        }

        // Other statements not yet implemented (Open, Close, Read, Write, etc.)
        _ => Ok(None),
    }
}

/// Convert an Expression to SimpleExpr.
fn convert_expr(expr: &Expression) -> Result<SimpleExpr> {
    match expr {
        Expression::Literal(l) => match &l.kind {
            LiteralKind::Integer(n) => Ok(SimpleExpr::Integer(*n)),
            LiteralKind::Decimal(s) => {
                // Parse as integer for simplicity
                let n: i64 = s.parse().unwrap_or(0);
                Ok(SimpleExpr::Integer(n))
            }
            LiteralKind::String(s) => Ok(SimpleExpr::String(s.clone())),
            LiteralKind::Hex(s) => Ok(SimpleExpr::String(s.clone())),
            LiteralKind::Figurative(f) => {
                use zos_cobol::ast::FigurativeConstant;
                match f {
                    FigurativeConstant::Zero => Ok(SimpleExpr::Integer(0)),
                    FigurativeConstant::Space => Ok(SimpleExpr::String(" ".to_string())),
                    FigurativeConstant::HighValue => Ok(SimpleExpr::String(
                        String::from_utf8_lossy(&[0xFF]).to_string(),
                    )),
                    FigurativeConstant::LowValue => Ok(SimpleExpr::String(
                        String::from_utf8_lossy(&[0x00]).to_string(),
                    )),
                    FigurativeConstant::Quote => Ok(SimpleExpr::String("\"".to_string())),
                    FigurativeConstant::All => Ok(SimpleExpr::String("".to_string())),
                }
            }
            LiteralKind::AllOf(inner) => convert_literal(inner),
        },

        Expression::Variable(q) => Ok(SimpleExpr::Variable(q.name.clone())),

        Expression::Binary(b) => {
            let left = Box::new(convert_expr(&b.left)?);
            let right = Box::new(convert_expr(&b.right)?);
            let op = match b.op {
                zos_cobol::ast::BinaryOp::Add => SimpleBinaryOp::Add,
                zos_cobol::ast::BinaryOp::Subtract => SimpleBinaryOp::Subtract,
                zos_cobol::ast::BinaryOp::Multiply => SimpleBinaryOp::Multiply,
                zos_cobol::ast::BinaryOp::Divide => SimpleBinaryOp::Divide,
                zos_cobol::ast::BinaryOp::Power => SimpleBinaryOp::Multiply, // Approximate
            };
            Ok(SimpleExpr::Binary { left, op, right })
        }

        Expression::Unary(u) => {
            let inner = convert_expr(&u.operand)?;
            match u.op {
                zos_cobol::ast::UnaryOp::Minus => {
                    // Negate by multiplying by -1
                    Ok(SimpleExpr::Binary {
                        left: Box::new(SimpleExpr::Integer(-1)),
                        op: SimpleBinaryOp::Multiply,
                        right: Box::new(inner),
                    })
                }
                zos_cobol::ast::UnaryOp::Plus => Ok(inner),
            }
        }

        Expression::Paren(inner) => convert_expr(inner),

        Expression::RefMod(r) => {
            // Reference modification - just use the variable
            Ok(SimpleExpr::Variable(r.variable.name.clone()))
        }

        Expression::Function(f) => {
            // Functions not fully supported, return 0
            tracing::warn!("Function {} not implemented, returning 0", f.name);
            Ok(SimpleExpr::Integer(0))
        }

        Expression::LengthOf(l) => {
            // LENGTH OF returns the length of a data item
            // In interpreter mode, we just return a placeholder
            tracing::debug!("LENGTH OF {} evaluated as placeholder", l.item.name);
            Ok(SimpleExpr::Integer(0))
        }

        Expression::AddressOf(a) => {
            // ADDRESS OF returns a pointer
            // Not meaningful in interpreter mode
            tracing::debug!("ADDRESS OF {} evaluated as placeholder", a.item.name);
            Ok(SimpleExpr::Integer(0))
        }
    }
}

/// Convert a WhenCondition to SimpleCondition.
fn convert_when_condition(cond: &zos_cobol::ast::WhenCondition) -> Result<SimpleCondition> {
    use zos_cobol::ast::WhenCondition;
    match cond {
        WhenCondition::Any => Ok(SimpleCondition::Compare {
            left: SimpleExpr::Integer(1),
            op: SimpleCompareOp::Equal,
            right: SimpleExpr::Integer(1),
        }),
        WhenCondition::True => Ok(SimpleCondition::Compare {
            left: SimpleExpr::Integer(1),
            op: SimpleCompareOp::Equal,
            right: SimpleExpr::Integer(1),
        }),
        WhenCondition::False => Ok(SimpleCondition::Compare {
            left: SimpleExpr::Integer(0),
            op: SimpleCompareOp::Equal,
            right: SimpleExpr::Integer(1),
        }),
        WhenCondition::Value(expr) => {
            // For EVALUATE variable: the runtime compares subject == value
            let val = convert_expr(expr)?;
            Ok(SimpleCondition::Compare {
                left: SimpleExpr::Variable("__EVAL_SUBJECT__".to_string()),
                op: SimpleCompareOp::Equal,
                right: val,
            })
        }
        WhenCondition::Condition(c) => convert_condition(c),
        WhenCondition::Range { .. } => {
            // Approximate range as always-true
            Ok(SimpleCondition::Compare {
                left: SimpleExpr::Integer(1),
                op: SimpleCompareOp::Equal,
                right: SimpleExpr::Integer(1),
            })
        }
    }
}

/// Check if a SimpleProgram contains any EXEC CICS statements.
fn has_cics_statements(program: &SimpleProgram) -> bool {
    fn stmts_have_cics(stmts: &[SimpleStatement]) -> bool {
        stmts.iter().any(|s| match s {
            SimpleStatement::ExecCics { .. } => true,
            SimpleStatement::If {
                then_branch,
                else_branch,
                ..
            } => {
                stmts_have_cics(then_branch)
                    || else_branch.as_ref().is_some_and(|eb| stmts_have_cics(eb))
            }
            SimpleStatement::Evaluate { when_clauses, .. } => {
                when_clauses.iter().any(|w| stmts_have_cics(&w.statements))
            }
            _ => false,
        })
    }

    stmts_have_cics(&program.statements)
        || program.paragraphs.values().any(|stmts| stmts_have_cics(stmts))
}

/// Convert a Condition to SimpleCondition.
fn convert_condition(cond: &zos_cobol::ast::Condition) -> Result<SimpleCondition> {
    match cond {
        zos_cobol::ast::Condition::Comparison(c) => {
            let left = convert_expr(&c.left)?;
            let right = convert_expr(&c.right)?;
            let op = match c.op {
                zos_cobol::ast::ComparisonOp::Equal => SimpleCompareOp::Equal,
                zos_cobol::ast::ComparisonOp::NotEqual => SimpleCompareOp::NotEqual,
                zos_cobol::ast::ComparisonOp::LessThan => SimpleCompareOp::LessThan,
                zos_cobol::ast::ComparisonOp::LessOrEqual => SimpleCompareOp::LessOrEqual,
                zos_cobol::ast::ComparisonOp::GreaterThan => SimpleCompareOp::GreaterThan,
                zos_cobol::ast::ComparisonOp::GreaterOrEqual => SimpleCompareOp::GreaterOrEqual,
            };
            Ok(SimpleCondition::Compare { left, op, right })
        }

        zos_cobol::ast::Condition::Not(inner) => {
            Ok(SimpleCondition::Not(Box::new(convert_condition(inner)?)))
        }

        zos_cobol::ast::Condition::And(left, right) => Ok(SimpleCondition::And(
            Box::new(convert_condition(left)?),
            Box::new(convert_condition(right)?),
        )),

        zos_cobol::ast::Condition::Or(left, right) => Ok(SimpleCondition::Or(
            Box::new(convert_condition(left)?),
            Box::new(convert_condition(right)?),
        )),

        zos_cobol::ast::Condition::Paren(inner) => convert_condition(inner),

        // Class and sign conditions - evaluate to true for now
        zos_cobol::ast::Condition::Class(_) => Ok(SimpleCondition::Compare {
            left: SimpleExpr::Integer(1),
            op: SimpleCompareOp::Equal,
            right: SimpleExpr::Integer(1),
        }),

        zos_cobol::ast::Condition::Sign(_) => Ok(SimpleCondition::Compare {
            left: SimpleExpr::Integer(1),
            op: SimpleCompareOp::Equal,
            right: SimpleExpr::Integer(1),
        }),

        // Condition name - evaluate to true for now
        zos_cobol::ast::Condition::ConditionName(_) => Ok(SimpleCondition::Compare {
            left: SimpleExpr::Integer(1),
            op: SimpleCompareOp::Equal,
            right: SimpleExpr::Integer(1),
        }),
    }
}
