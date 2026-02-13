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

/// Interpret a COBOL program directly.
pub fn interpret(input: PathBuf) -> Result<()> {
    // Read source file
    let source_text = std::fs::read_to_string(&input)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read: {}", input.display()))?;

    tracing::info!("Interpreting: {}", input.display());

    // Create source file and lex
    let source_file = SourceFile::from_text(FileId(0), source_text, SourceFormat::Fixed);
    let (tokens, lex_errors) = scan(&source_file);

    if !lex_errors.is_empty() {
        for err in &lex_errors {
            tracing::error!("Lex error: {:?}", err);
        }
        return Err(miette::miette!(
            "Lexing failed with {} errors",
            lex_errors.len()
        ));
    }

    // Parse
    let (program, parse_errors) = zos_cobol::parser::parse(tokens);

    if !parse_errors.is_empty() {
        for err in &parse_errors {
            tracing::error!("Parse error: {:?}", err);
        }
        return Err(miette::miette!(
            "Parse failed with {} errors",
            parse_errors.len()
        ));
    }

    let program = program.ok_or_else(|| miette::miette!("Failed to parse program"))?;

    let program_name = program.identification.program_id.name.clone();
    tracing::info!("Program: {}", program_name);

    // Convert to SimpleProgram
    let simple = convert_program(&program)?;

    // Execute
    let mut env = Environment::new();
    let rc = zos_runtime::interpreter::execute(&simple, &mut env)
        .map_err(|e| miette::miette!("Runtime error: {}", e))?;

    tracing::info!("Program completed with RC={}", rc);

    if rc == 0 {
        Ok(())
    } else {
        Err(miette::miette!(
            "Program {} exited with RC={}",
            program_name,
            rc
        ))
    }
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

        // Other statements not yet implemented
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
    }
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
