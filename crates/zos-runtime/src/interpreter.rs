//! COBOL interpreter for running programs without compilation.
//!
//! A tree-walking interpreter that executes COBOL AST directly.

use std::collections::HashMap;
use std::io::{BufRead, Write};

use crate::value::{CobolValue, NumericValue};

/// Interpreter error.
#[derive(Debug, Clone)]
pub struct InterpreterError {
    pub message: String,
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for InterpreterError {}

type Result<T> = std::result::Result<T, InterpreterError>;

/// Data item metadata.
#[derive(Debug, Clone)]
pub struct DataItemMeta {
    /// Size in bytes.
    pub size: usize,
    /// Decimal positions.
    pub decimals: u32,
    /// Whether numeric.
    pub is_numeric: bool,
    /// Picture string.
    pub picture: Option<String>,
}

impl Default for DataItemMeta {
    fn default() -> Self {
        Self {
            size: 80,
            decimals: 0,
            is_numeric: false,
            picture: None,
        }
    }
}

/// Runtime environment for interpreter.
pub struct Environment {
    /// Data item values.
    variables: HashMap<String, CobolValue>,
    /// Data item metadata.
    metadata: HashMap<String, DataItemMeta>,
    /// Standard output.
    stdout: Box<dyn Write>,
    /// Standard input.
    stdin: Box<dyn BufRead>,
    /// Return code.
    return_code: i32,
    /// Stop flag.
    stopped: bool,
}

impl Environment {
    /// Create a new environment with default I/O.
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            metadata: HashMap::new(),
            stdout: Box::new(std::io::stdout()),
            stdin: Box::new(std::io::BufReader::new(std::io::stdin())),
            return_code: 0,
            stopped: false,
        }
    }

    /// Create environment with custom I/O.
    pub fn with_io(
        stdout: Box<dyn Write>,
        stdin: Box<dyn BufRead>,
    ) -> Self {
        Self {
            variables: HashMap::new(),
            metadata: HashMap::new(),
            stdout,
            stdin,
            return_code: 0,
            stopped: false,
        }
    }

    /// Define a data item.
    pub fn define(&mut self, name: &str, meta: DataItemMeta) {
        let initial = if meta.is_numeric {
            CobolValue::from_i64(0)
        } else {
            CobolValue::Alphanumeric(" ".repeat(meta.size))
        };
        self.variables.insert(name.to_uppercase(), initial);
        self.metadata.insert(name.to_uppercase(), meta);
    }

    /// Get a variable value.
    pub fn get(&self, name: &str) -> Option<&CobolValue> {
        self.variables.get(&name.to_uppercase())
    }

    /// Set a variable value.
    pub fn set(&mut self, name: &str, value: CobolValue) -> Result<()> {
        let name_upper = name.to_uppercase();

        // If variable doesn't exist, create it with default metadata
        if !self.variables.contains_key(&name_upper) {
            let meta = DataItemMeta::default();
            self.metadata.insert(name_upper.clone(), meta);
        }

        self.variables.insert(name_upper, value);
        Ok(())
    }

    /// Get metadata for a variable.
    pub fn meta(&self, name: &str) -> Option<&DataItemMeta> {
        self.metadata.get(&name.to_uppercase())
    }

    /// Write to stdout.
    pub fn display(&mut self, text: &str, no_advancing: bool) -> Result<()> {
        if no_advancing {
            write!(self.stdout, "{}", text)
        } else {
            writeln!(self.stdout, "{}", text)
        }
        .map_err(|e| InterpreterError {
            message: format!("Display error: {}", e),
        })
    }

    /// Read from stdin.
    pub fn accept(&mut self) -> Result<String> {
        let mut line = String::new();
        self.stdin.read_line(&mut line).map_err(|e| InterpreterError {
            message: format!("Accept error: {}", e),
        })?;
        Ok(line.trim_end().to_string())
    }

    /// Set return code.
    pub fn set_return_code(&mut self, code: i32) {
        self.return_code = code;
    }

    /// Get return code.
    pub fn return_code(&self) -> i32 {
        self.return_code
    }

    /// Stop execution.
    pub fn stop(&mut self) {
        self.stopped = true;
    }

    /// Check if stopped.
    pub fn is_stopped(&self) -> bool {
        self.stopped
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

/// Simple statement for interpreter (parsed from AST).
#[derive(Debug, Clone)]
pub enum SimpleStatement {
    /// DISPLAY items
    Display {
        items: Vec<SimpleExpr>,
        no_advancing: bool,
    },
    /// ACCEPT variable
    Accept { target: String },
    /// MOVE value TO targets
    Move {
        from: SimpleExpr,
        to: Vec<String>,
    },
    /// COMPUTE target = expr
    Compute {
        target: String,
        expr: SimpleExpr,
    },
    /// ADD values TO targets
    Add {
        values: Vec<SimpleExpr>,
        to: Vec<String>,
    },
    /// SUBTRACT values FROM targets
    Subtract {
        values: Vec<SimpleExpr>,
        from: Vec<String>,
    },
    /// MULTIPLY value BY target
    Multiply {
        value: SimpleExpr,
        by: SimpleExpr,
        giving: Option<String>,
    },
    /// DIVIDE value INTO target
    Divide {
        value: SimpleExpr,
        into: SimpleExpr,
        giving: Option<String>,
    },
    /// IF condition THEN statements ELSE statements
    If {
        condition: SimpleCondition,
        then_branch: Vec<SimpleStatement>,
        else_branch: Option<Vec<SimpleStatement>>,
    },
    /// PERFORM paragraph
    Perform { target: String, times: Option<u32> },
    /// STOP RUN
    StopRun { return_code: Option<i32> },
}

/// Simple expression for interpreter.
#[derive(Debug, Clone)]
pub enum SimpleExpr {
    /// Integer literal
    Integer(i64),
    /// String literal
    String(String),
    /// Variable reference
    Variable(String),
    /// Binary operation
    Binary {
        left: Box<SimpleExpr>,
        op: SimpleBinaryOp,
        right: Box<SimpleExpr>,
    },
}

/// Simple binary operator.
#[derive(Debug, Clone, Copy)]
pub enum SimpleBinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

/// Simple condition for interpreter.
#[derive(Debug, Clone)]
pub enum SimpleCondition {
    /// Comparison
    Compare {
        left: SimpleExpr,
        op: SimpleCompareOp,
        right: SimpleExpr,
    },
    /// NOT condition
    Not(Box<SimpleCondition>),
    /// AND
    And(Box<SimpleCondition>, Box<SimpleCondition>),
    /// OR
    Or(Box<SimpleCondition>, Box<SimpleCondition>),
}

/// Simple comparison operator.
#[derive(Debug, Clone, Copy)]
pub enum SimpleCompareOp {
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

/// Simple program representation.
#[derive(Debug, Clone)]
pub struct SimpleProgram {
    /// Program name.
    pub name: String,
    /// Data items (name -> metadata).
    pub data_items: Vec<(String, DataItemMeta)>,
    /// Main procedure statements.
    pub statements: Vec<SimpleStatement>,
    /// Named paragraphs.
    pub paragraphs: HashMap<String, Vec<SimpleStatement>>,
}

/// Execute a simple program.
pub fn execute(program: &SimpleProgram, env: &mut Environment) -> Result<i32> {
    // Initialize data items
    for (name, meta) in &program.data_items {
        env.define(name, meta.clone());
    }

    // Execute main statements
    execute_statements(&program.statements, program, env)?;

    Ok(env.return_code())
}

/// Execute a list of statements.
fn execute_statements(
    statements: &[SimpleStatement],
    program: &SimpleProgram,
    env: &mut Environment,
) -> Result<()> {
    for stmt in statements {
        if env.is_stopped() {
            break;
        }
        execute_statement(stmt, program, env)?;
    }
    Ok(())
}

/// Execute a single statement.
fn execute_statement(
    stmt: &SimpleStatement,
    program: &SimpleProgram,
    env: &mut Environment,
) -> Result<()> {
    match stmt {
        SimpleStatement::Display { items, no_advancing } => {
            let mut output = String::new();
            for item in items {
                let value = eval_expr(item, env)?;
                output.push_str(&value.to_display_string());
            }
            env.display(&output, *no_advancing)?;
        }

        SimpleStatement::Accept { target } => {
            let input = env.accept()?;
            env.set(target, CobolValue::Alphanumeric(input))?;
        }

        SimpleStatement::Move { from, to } => {
            let value = eval_expr(from, env)?;
            for target in to {
                env.set(target, value.clone())?;
            }
        }

        SimpleStatement::Compute { target, expr } => {
            let value = eval_expr(expr, env)?;
            env.set(target, value)?;
        }

        SimpleStatement::Add { values, to } => {
            let mut sum = NumericValue::from_i64(0);
            for val in values {
                let v = eval_expr(val, env)?;
                sum = sum.add(&to_numeric(&v));
            }
            for target in to {
                if let Some(current) = env.get(target) {
                    let new_val = to_numeric(current).add(&sum);
                    env.set(target, CobolValue::Numeric(new_val))?;
                }
            }
        }

        SimpleStatement::Subtract { values, from } => {
            let mut total = NumericValue::from_i64(0);
            for val in values {
                let v = eval_expr(val, env)?;
                total = total.add(&to_numeric(&v));
            }
            for target in from {
                if let Some(current) = env.get(target) {
                    let new_val = to_numeric(current).subtract(&total);
                    env.set(target, CobolValue::Numeric(new_val))?;
                }
            }
        }

        SimpleStatement::Multiply { value, by, giving } => {
            let v1 = eval_expr(value, env)?;
            let v2 = eval_expr(by, env)?;
            let result = to_numeric(&v1).multiply(&to_numeric(&v2));
            if let Some(target) = giving {
                env.set(target, CobolValue::Numeric(result))?;
            }
        }

        SimpleStatement::Divide { value, into, giving } => {
            let v1 = eval_expr(value, env)?;
            let v2 = eval_expr(into, env)?;
            let result = to_numeric(&v2)
                .divide(&to_numeric(&v1))
                .ok_or_else(|| InterpreterError {
                    message: "Division by zero".to_string(),
                })?;
            if let Some(target) = giving {
                env.set(target, CobolValue::Numeric(result))?;
            }
        }

        SimpleStatement::If {
            condition,
            then_branch,
            else_branch,
        } => {
            if eval_condition(condition, env)? {
                execute_statements(then_branch, program, env)?;
            } else if let Some(else_stmts) = else_branch {
                execute_statements(else_stmts, program, env)?;
            }
        }

        SimpleStatement::Perform { target, times } => {
            let iterations = times.unwrap_or(1);
            for _ in 0..iterations {
                if env.is_stopped() {
                    break;
                }
                if let Some(para) = program.paragraphs.get(&target.to_uppercase()) {
                    execute_statements(para, program, env)?;
                }
            }
        }

        SimpleStatement::StopRun { return_code } => {
            if let Some(rc) = return_code {
                env.set_return_code(*rc);
            }
            env.stop();
        }
    }

    Ok(())
}

/// Convert CobolValue to NumericValue.
fn to_numeric(value: &CobolValue) -> NumericValue {
    match value {
        CobolValue::Numeric(n) => n.clone(),
        CobolValue::Alphanumeric(s) => {
            // Try to parse as number
            if let Ok(n) = s.trim().parse::<i64>() {
                NumericValue::from_i64(n)
            } else {
                NumericValue::from_i64(0)
            }
        }
        CobolValue::Group(_) => NumericValue::from_i64(0),
    }
}

/// Evaluate an expression.
fn eval_expr(expr: &SimpleExpr, env: &Environment) -> Result<CobolValue> {
    match expr {
        SimpleExpr::Integer(n) => Ok(CobolValue::from_i64(*n)),

        SimpleExpr::String(s) => Ok(CobolValue::Alphanumeric(s.clone())),

        SimpleExpr::Variable(name) => {
            env.get(name)
                .cloned()
                .ok_or_else(|| InterpreterError {
                    message: format!("Undefined variable: {}", name),
                })
        }

        SimpleExpr::Binary { left, op, right } => {
            let l = to_numeric(&eval_expr(left, env)?);
            let r = to_numeric(&eval_expr(right, env)?);
            let result = match op {
                SimpleBinaryOp::Add => l.add(&r),
                SimpleBinaryOp::Subtract => l.subtract(&r),
                SimpleBinaryOp::Multiply => l.multiply(&r),
                SimpleBinaryOp::Divide => l.divide(&r).ok_or_else(|| InterpreterError {
                    message: "Division by zero".to_string(),
                })?,
            };
            Ok(CobolValue::Numeric(result))
        }
    }
}

/// Compare two CobolValues.
fn compare_values(left: &CobolValue, right: &CobolValue) -> std::cmp::Ordering {
    match (left, right) {
        (CobolValue::Numeric(l), CobolValue::Numeric(r)) => l.value.cmp(&r.value),
        (CobolValue::Alphanumeric(l), CobolValue::Alphanumeric(r)) => l.cmp(r),
        (CobolValue::Numeric(l), CobolValue::Alphanumeric(r)) => {
            l.to_display_string().cmp(r)
        }
        (CobolValue::Alphanumeric(l), CobolValue::Numeric(r)) => {
            l.cmp(&r.to_display_string())
        }
        _ => std::cmp::Ordering::Equal,
    }
}

/// Evaluate a condition.
fn eval_condition(cond: &SimpleCondition, env: &Environment) -> Result<bool> {
    match cond {
        SimpleCondition::Compare { left, op, right } => {
            let l = eval_expr(left, env)?;
            let r = eval_expr(right, env)?;
            Ok(match op {
                SimpleCompareOp::Equal => compare_values(&l, &r) == std::cmp::Ordering::Equal,
                SimpleCompareOp::NotEqual => compare_values(&l, &r) != std::cmp::Ordering::Equal,
                SimpleCompareOp::LessThan => compare_values(&l, &r) == std::cmp::Ordering::Less,
                SimpleCompareOp::LessOrEqual => compare_values(&l, &r) != std::cmp::Ordering::Greater,
                SimpleCompareOp::GreaterThan => compare_values(&l, &r) == std::cmp::Ordering::Greater,
                SimpleCompareOp::GreaterOrEqual => compare_values(&l, &r) != std::cmp::Ordering::Less,
            })
        }

        SimpleCondition::Not(inner) => Ok(!eval_condition(inner, env)?),

        SimpleCondition::And(left, right) => {
            Ok(eval_condition(left, env)? && eval_condition(right, env)?)
        }

        SimpleCondition::Or(left, right) => {
            Ok(eval_condition(left, env)? || eval_condition(right, env)?)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_env() -> Environment {
        let output = Vec::<u8>::new();
        let input = std::io::Cursor::new(Vec::<u8>::new());
        Environment::with_io(
            Box::new(output),
            Box::new(std::io::BufReader::new(input)),
        )
    }

    #[test]
    fn test_move_and_compute() {
        let mut env = create_test_env();

        env.define("X", DataItemMeta {
            size: 5,
            decimals: 0,
            is_numeric: true,
            picture: Some("9(5)".to_string()),
        });

        env.define("Y", DataItemMeta {
            size: 5,
            decimals: 0,
            is_numeric: true,
            picture: Some("9(5)".to_string()),
        });

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            statements: vec![
                SimpleStatement::Move {
                    from: SimpleExpr::Integer(10),
                    to: vec!["X".to_string()],
                },
                SimpleStatement::Compute {
                    target: "Y".to_string(),
                    expr: SimpleExpr::Binary {
                        left: Box::new(SimpleExpr::Variable("X".to_string())),
                        op: SimpleBinaryOp::Multiply,
                        right: Box::new(SimpleExpr::Integer(2)),
                    },
                },
            ],
            paragraphs: HashMap::new(),
        };

        execute(&program, &mut env).unwrap();

        let y = env.get("Y").unwrap();
        assert_eq!(to_numeric(y).integer_part(), 20);
    }

    #[test]
    fn test_if_statement() {
        let mut env = create_test_env();

        env.define("X", DataItemMeta {
            size: 5,
            decimals: 0,
            is_numeric: true,
            picture: Some("9(5)".to_string()),
        });

        env.define("RESULT", DataItemMeta {
            size: 10,
            decimals: 0,
            is_numeric: false,
            picture: Some("X(10)".to_string()),
        });

        env.set("X", CobolValue::from_i64(5)).unwrap();

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            statements: vec![SimpleStatement::If {
                condition: SimpleCondition::Compare {
                    left: SimpleExpr::Variable("X".to_string()),
                    op: SimpleCompareOp::GreaterThan,
                    right: SimpleExpr::Integer(3),
                },
                then_branch: vec![SimpleStatement::Move {
                    from: SimpleExpr::String("BIG".to_string()),
                    to: vec!["RESULT".to_string()],
                }],
                else_branch: Some(vec![SimpleStatement::Move {
                    from: SimpleExpr::String("SMALL".to_string()),
                    to: vec!["RESULT".to_string()],
                }]),
            }],
            paragraphs: HashMap::new(),
        };

        execute(&program, &mut env).unwrap();

        let result = env.get("RESULT").unwrap();
        assert_eq!(result.to_display_string(), "BIG");
    }

    #[test]
    fn test_add_statement() {
        let mut env = create_test_env();

        env.define("X", DataItemMeta {
            size: 5,
            decimals: 0,
            is_numeric: true,
            picture: Some("9(5)".to_string()),
        });

        env.set("X", CobolValue::from_i64(10)).unwrap();

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            statements: vec![SimpleStatement::Add {
                values: vec![SimpleExpr::Integer(5), SimpleExpr::Integer(3)],
                to: vec!["X".to_string()],
            }],
            paragraphs: HashMap::new(),
        };

        execute(&program, &mut env).unwrap();

        let x = env.get("X").unwrap();
        assert_eq!(to_numeric(x).integer_part(), 18);
    }

    #[test]
    fn test_stop_run() {
        let mut env = create_test_env();

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            statements: vec![
                SimpleStatement::StopRun { return_code: Some(4) },
                // This should not execute
                SimpleStatement::Move {
                    from: SimpleExpr::Integer(999),
                    to: vec!["SHOULD_NOT_EXIST".to_string()],
                },
            ],
            paragraphs: HashMap::new(),
        };

        let rc = execute(&program, &mut env).unwrap();
        assert_eq!(rc, 4);
        assert!(env.is_stopped());
    }
}
