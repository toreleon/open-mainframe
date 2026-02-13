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

/// Trait for handling EXEC CICS commands.
pub trait CicsCommandHandler {
    /// Execute a CICS command.
    fn execute(
        &mut self,
        command: &str,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()>;

    /// Downcast to concrete type (for inspecting handler state after execution).
    fn as_any_mut(&mut self) -> Option<&mut dyn std::any::Any> {
        None
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
    /// CICS command handler (optional).
    pub cics_handler: Option<Box<dyn CicsCommandHandler>>,
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
            cics_handler: None,
        }
    }

    /// Create environment with custom I/O.
    pub fn with_io(stdout: Box<dyn Write>, stdin: Box<dyn BufRead>) -> Self {
        Self {
            variables: HashMap::new(),
            metadata: HashMap::new(),
            stdout,
            stdin,
            return_code: 0,
            stopped: false,
            cics_handler: None,
        }
    }

    /// Install a CICS command handler.
    pub fn with_cics_handler(mut self, handler: Box<dyn CicsCommandHandler>) -> Self {
        self.cics_handler = Some(handler);
        self
    }

    /// Define a data item.
    /// If the variable already has a value (e.g., pre-set by CICS EIB or --set),
    /// the existing value is preserved and only metadata is updated.
    pub fn define(&mut self, name: &str, meta: DataItemMeta) {
        let name_upper = name.to_uppercase();
        if !self.variables.contains_key(&name_upper) {
            let initial = if meta.is_numeric {
                CobolValue::from_i64(0)
            } else {
                CobolValue::Alphanumeric(" ".repeat(meta.size))
            };
            self.variables.insert(name_upper.clone(), initial);
        }
        self.metadata.insert(name_upper, meta);
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
        self.stdin
            .read_line(&mut line)
            .map_err(|e| InterpreterError {
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

    /// Reset stopped flag (for XCTL dispatch to new program).
    pub fn resume(&mut self) {
        self.stopped = false;
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
    Move { from: SimpleExpr, to: Vec<String> },
    /// COMPUTE target = expr
    Compute { target: String, expr: SimpleExpr },
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
    /// EVALUATE statement
    Evaluate {
        subjects: Vec<SimpleExpr>,
        when_clauses: Vec<SimpleWhenClause>,
    },
    /// EXEC CICS command
    ExecCics {
        command: String,
        options: Vec<(String, Option<SimpleExpr>)>,
    },
    /// GO TO paragraph
    GoTo { target: String },
    /// INITIALIZE variable
    Initialize { targets: Vec<String> },
    /// STRING concatenation
    StringConcat {
        sources: Vec<SimpleExpr>,
        into: String,
    },
    /// SET variable TO value (index or general)
    Set {
        target: String,
        value: SimpleExpr,
    },
    /// SET condition-name TO TRUE: resolves parent field from condition_names
    SetCondition {
        condition_name: String,
        value: bool,
    },
    /// CALL subprogram
    Call {
        program: SimpleExpr,
        using: Vec<String>,
    },
    /// PERFORM inline (with statements)
    PerformInline {
        until: Option<SimpleCondition>,
        statements: Vec<SimpleStatement>,
        /// VARYING clause: (variable, FROM, BY)
        varying: Option<(String, SimpleExpr, SimpleExpr)>,
    },
}

/// EVALUATE WHEN clause.
#[derive(Debug, Clone)]
pub struct SimpleWhenClause {
    /// Condition to match (for EVALUATE TRUE: condition; for EVALUATE var: value)
    pub condition: SimpleCondition,
    /// Statements to execute
    pub statements: Vec<SimpleStatement>,
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
    /// Intrinsic function call (e.g., UPPER-CASE, CURRENT-DATE)
    FunctionCall {
        name: String,
        args: Vec<SimpleExpr>,
    },
    /// Reference modification: variable(start:length)
    /// start is 1-based. If length is None, takes from start to end.
    RefMod {
        variable: Box<SimpleExpr>,
        start: Box<SimpleExpr>,
        length: Option<Box<SimpleExpr>>,
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
    /// Level-88 condition name (resolved at runtime from program.condition_names)
    ConditionName(String),
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
    /// Level-88 condition names: condition_name -> (parent_field, value).
    pub condition_names: HashMap<String, (String, String)>,
    /// Group item layouts: group_name -> list of sub-fields with offsets.
    pub group_layouts: HashMap<String, Vec<GroupField>>,
}

/// A sub-field within a group item, with its offset and size.
#[derive(Debug, Clone)]
pub struct GroupField {
    /// Field name (uppercase). Empty string for FILLER fields.
    pub name: String,
    /// Byte offset within the group.
    pub offset: usize,
    /// Size in bytes.
    pub size: usize,
    /// Whether numeric.
    pub is_numeric: bool,
    /// Default value for FILLER fields with VALUE clause (e.g., "/", ":").
    pub default_value: Option<String>,
}

/// When a group item is set, decompose its value into sub-fields based on the group layout.
fn decompose_group(group_name: &str, data: &str, program: &SimpleProgram, env: &mut Environment) -> Result<()> {
    if let Some(fields) = program.group_layouts.get(&group_name.to_uppercase()) {
        let data_bytes = data.as_bytes();
        for field in fields {
            let end = (field.offset + field.size).min(data_bytes.len());
            if field.offset < data_bytes.len() {
                let slice = &data_bytes[field.offset..end];
                let val_str = String::from_utf8_lossy(slice).to_string();
                if field.is_numeric {
                    let trimmed = val_str.trim();
                    let n = trimmed.parse::<i64>().unwrap_or(0);
                    env.set(&field.name, CobolValue::from_i64(n)).ok();
                } else {
                    env.set(&field.name, CobolValue::Alphanumeric(val_str)).ok();
                }
            }
        }
    }
    Ok(())
}

/// Recompose a group variable's value from its children's current values.
///
/// This is the inverse of `decompose_group`. When a group variable is read
/// (e.g., used as the source of a MOVE), its stored value may be stale if
/// children were updated independently. This function rebuilds the group
/// string by slotting each child's current display value into its offset.
fn compose_group(group_name: &str, program: &SimpleProgram, env: &Environment) -> Option<CobolValue> {
    let fields = program.group_layouts.get(&group_name.to_uppercase())?;
    if fields.is_empty() {
        return None;
    }

    // Determine total group size from the last field's offset + size
    let total_size = fields.iter().map(|f| f.offset + f.size).max().unwrap_or(0);
    if total_size == 0 {
        return None;
    }

    let mut buffer = vec![b' '; total_size];

    for field in fields {
        // For FILLER fields with default values (e.g., "/" or ":"), use the default
        if field.name.is_empty() {
            if let Some(ref default) = field.default_value {
                let bytes = default.as_bytes();
                for i in 0..field.size {
                    if field.offset + i < buffer.len() {
                        buffer[field.offset + i] = if i < bytes.len() { bytes[i] } else { b' ' };
                    }
                }
            }
            continue;
        }

        if let Some(val) = env.get(&field.name) {
            let display = if field.is_numeric {
                // Numeric fields: zero-pad on the left to field size
                let raw = val.to_display_string();
                let trimmed = raw.trim();
                format!("{:0>width$}", trimmed, width = field.size)
            } else {
                val.to_display_string()
            };
            let bytes = display.as_bytes();
            // Slot value into buffer at field offset
            for i in 0..field.size {
                if field.offset + i < buffer.len() {
                    buffer[field.offset + i] = if i < bytes.len() { bytes[i] } else { b' ' };
                }
            }
        }
    }

    Some(CobolValue::Alphanumeric(String::from_utf8_lossy(&buffer).to_string()))
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
pub fn execute_statements(
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
    // Track call depth to detect infinite recursion
    thread_local! {
        static DEPTH: std::cell::Cell<u32> = const { std::cell::Cell::new(0) };
    }
    let current_depth = DEPTH.with(|d| {
        let v = d.get();
        d.set(v + 1);
        v
    });
    if current_depth > 500 {
        DEPTH.with(|d| d.set(d.get().saturating_sub(1)));
        return Err(InterpreterError {
            message: format!("Recursion depth exceeded ({})", current_depth),
        });
    }
    let result = execute_statement_impl(stmt, program, env);
    DEPTH.with(|d| d.set(d.get().saturating_sub(1)));
    result
}

fn execute_statement_impl(
    stmt: &SimpleStatement,
    program: &SimpleProgram,
    env: &mut Environment,
) -> Result<()> {
    match stmt {
        SimpleStatement::Display {
            items,
            no_advancing,
        } => {
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
            // If the source is a group variable, recompose from children first
            if let SimpleExpr::Variable(ref var_name) = from {
                if program.group_layouts.contains_key(&var_name.to_uppercase()) {
                    if let Some(composed) = compose_group(var_name, program, env) {
                        env.set(var_name, composed)?;
                    }
                }
            }
            let value = eval_expr(from, env)?;
            for target in to {
                env.set(target, value.clone())?;
                // If target is a group item, decompose into sub-fields
                if program.group_layouts.contains_key(&target.to_uppercase()) {
                    let data = value.to_display_string();
                    if std::env::var("ZOS_DEBUG_GROUPS").is_ok() {
                        eprintln!("[MOVE→DECOMPOSE] target={} data_len={} data={:?}", target, data.len(), &data[..data.len().min(40)]);
                    }
                    decompose_group(target, &data, program, env)?;
                }
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

        SimpleStatement::Divide {
            value,
            into,
            giving,
        } => {
            let v1 = eval_expr(value, env)?;
            let v2 = eval_expr(into, env)?;
            let result =
                to_numeric(&v2)
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
            if eval_condition(condition, env, program)? {
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

        SimpleStatement::Evaluate {
            subjects,
            when_clauses,
        } => {
            // Evaluate subject(s)
            let subject_values: Vec<CobolValue> = subjects
                .iter()
                .map(|s| eval_expr(s, env))
                .collect::<Result<_>>()?;

            // Set __EVAL_SUBJECT__ so WHEN value comparisons can reference it
            if let Some(first_subject) = subject_values.first() {
                env.set("__EVAL_SUBJECT__", first_subject.clone())?;
            }

            // Find first matching WHEN clause
            for when_clause in when_clauses {
                let matched = if subject_values.is_empty() {
                    // EVALUATE with no subjects - check condition directly
                    eval_condition(&when_clause.condition, env, program)?
                } else {
                    // EVALUATE var / EVALUATE TRUE: condition compares subject to value
                    eval_condition(&when_clause.condition, env, program)?
                };

                if matched {
                    execute_statements(&when_clause.statements, program, env)?;
                    break;
                }
            }
        }

        SimpleStatement::ExecCics { command, options } => {
            // Options that reference variables by name (not by value)
            const VAR_REF_OPTIONS: &[&str] = &[
                "INTO", "FROM", "RIDFLD", "RESP", "RESP2", "COMMAREA",
            ];
            // Commands where ALL option values are paragraph/label names, not expressions
            let is_label_command = command == "HANDLE" || command == "IGNORE";
            // Commands where ALL option values are variable names (target for writing)
            let is_assign_command = command == "ASSIGN";

            // Evaluate option expressions, but for variable-reference options,
            // pass the variable NAME as a string instead of evaluating the value.
            // For HANDLE/IGNORE CONDITION, all values are paragraph names.
            let eval_options: Vec<(String, Option<CobolValue>)> = options
                .iter()
                .map(|(name, expr)| {
                    let value = if let Some(e) = expr {
                        if is_assign_command {
                            // ASSIGN: all values are variable names (targets for writing)
                            if let SimpleExpr::Variable(var_name) = e {
                                Some(CobolValue::Alphanumeric(var_name.clone()))
                            } else if let SimpleExpr::String(s) = e {
                                Some(CobolValue::Alphanumeric(s.clone()))
                            } else {
                                Some(eval_expr(e, env)?)
                            }
                        } else if is_label_command {
                            // HANDLE CONDITION / IGNORE CONDITION: values are paragraph names
                            if let SimpleExpr::Variable(var_name) = e {
                                Some(CobolValue::Alphanumeric(var_name.clone()))
                            } else if let SimpleExpr::String(s) = e {
                                Some(CobolValue::Alphanumeric(s.clone()))
                            } else {
                                Some(eval_expr(e, env)?)
                            }
                        } else if VAR_REF_OPTIONS.iter().any(|o| name.eq_ignore_ascii_case(o)) {
                            // Pass variable name as string
                            if let SimpleExpr::Variable(var_name) = e {
                                Some(CobolValue::Alphanumeric(var_name.clone()))
                            } else {
                                Some(eval_expr(e, env)?)
                            }
                        } else {
                            Some(eval_expr(e, env)?)
                        }
                    } else {
                        None
                    };
                    Ok((name.clone(), value))
                })
                .collect::<Result<_>>()?;

            // Take the handler out temporarily to avoid borrow conflict
            if let Some(mut handler) = env.cics_handler.take() {
                let result = handler.execute(command, &eval_options, env);
                env.cics_handler = Some(handler);
                result?;
            }

            // After CICS READ/RECEIVE INTO: decompose group items into sub-fields
            if command == "READ" || command == "RECEIVE" {
                if let Some(into_expr) = options.iter()
                    .find(|(name, _)| name == "INTO")
                    .and_then(|(_, expr)| expr.as_ref())
                {
                    if let SimpleExpr::Variable(var_name) = into_expr {
                        if let Some(val) = env.get(var_name) {
                            let data = val.to_display_string();
                            if std::env::var("ZOS_DEBUG_CMP").is_ok() {
                                eprintln!("[DECOMPOSE] {} INTO {} len={}", command, var_name, data.len());
                            }
                            decompose_group(var_name, &data, program, env)?;
                        }
                    }
                }
            }
        }

        SimpleStatement::GoTo { target } => {
            // GO TO requires special flow control - for now, try PERFORM-like semantics
            if let Some(para) = program.paragraphs.get(&target.to_uppercase()) {
                execute_statements(para, program, env)?;
                env.stop(); // GO TO means transfer, stop current flow
            }
        }

        SimpleStatement::Initialize { targets } => {
            for target in targets {
                if let Some(meta) = env.meta(target).cloned() {
                    if meta.is_numeric {
                        env.set(target, CobolValue::from_i64(0))?;
                    } else {
                        env.set(target, CobolValue::Alphanumeric(" ".repeat(meta.size)))?;
                    }
                }
            }
        }

        SimpleStatement::StringConcat { sources, into } => {
            let mut result = String::new();
            for src in sources {
                let val = eval_expr(src, env)?;
                result.push_str(&val.to_display_string());
            }
            env.set(into, CobolValue::Alphanumeric(result))?;
        }

        SimpleStatement::Set { target, value } => {
            let val = eval_expr(value, env)?;
            env.set(target, val)?;
        }

        SimpleStatement::SetCondition { condition_name, value } => {
            // SET condition-name TO TRUE: set the parent field to the condition's value
            // SET condition-name TO FALSE: not standard COBOL, but we handle it
            if *value {
                if let Some((parent, expected_val)) = program.condition_names.get(&condition_name.to_uppercase()) {
                    env.set(parent, CobolValue::Alphanumeric(expected_val.clone()))?;
                }
            }
            // SET condition TO FALSE not supported in standard COBOL
        }

        SimpleStatement::Call { program, using: _ } => {
            let prog_name = eval_expr(program, env)?;
            // CALLed programs are not yet supported - log and continue
            env.display(
                &format!("[CALL] {} - not yet supported", prog_name.to_display_string().trim()),
                false,
            )?;
        }

        SimpleStatement::PerformInline { until, statements: stmts, varying } => {
            // Initialize VARYING variable if present
            if let Some((ref var_name, ref from_expr, _)) = varying {
                let from_val = eval_expr(from_expr, env)?;
                env.set(var_name, from_val)?;
            }

            // Determine the UNTIL condition: prefer varying's until (embedded in `until` field)
            let effective_until = if let Some(ref v) = varying {
                // For PERFORM VARYING, the UNTIL is stored in the `until` field
                until.as_ref()
            } else {
                until.as_ref()
            };

            let max_iterations = 10_000; // Safety limit
            let mut count = 0;

            loop {
                if env.is_stopped() || count >= max_iterations {
                    break;
                }
                // Check UNTIL condition before (test before)
                if let Some(cond) = effective_until {
                    if eval_condition(cond, env, program)? {
                        break;
                    }
                }
                execute_statements(stmts, program, env)?;
                count += 1;

                // Increment VARYING variable
                if let Some((ref var_name, _, ref by_expr)) = varying {
                    let by_val = to_numeric(&eval_expr(by_expr, env)?);
                    if let Some(current) = env.get(var_name).cloned() {
                        let new_val = to_numeric(&current).add(&by_val);
                        env.set(var_name, CobolValue::Numeric(new_val))?;
                    }
                }

                // If no UNTIL and no VARYING, execute once
                if effective_until.is_none() && varying.is_none() {
                    break;
                }
            }
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

        SimpleExpr::Variable(name) => env.get(name).cloned().ok_or_else(|| InterpreterError {
            message: format!("Undefined variable: {}", name),
        }),

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

        SimpleExpr::FunctionCall { name, args } => {
            match name.as_str() {
                "UPPER-CASE" => {
                    let val = eval_expr(args.first().ok_or_else(|| InterpreterError {
                        message: "UPPER-CASE requires an argument".to_string(),
                    })?, env)?;
                    Ok(CobolValue::Alphanumeric(val.to_display_string().to_uppercase()))
                }
                "LOWER-CASE" => {
                    let val = eval_expr(args.first().ok_or_else(|| InterpreterError {
                        message: "LOWER-CASE requires an argument".to_string(),
                    })?, env)?;
                    Ok(CobolValue::Alphanumeric(val.to_display_string().to_lowercase()))
                }
                "CURRENT-DATE" => {
                    // Returns YYYYMMDDHHMMSSCC+HHMM (21 chars)
                    let now = std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap_or_default()
                        .as_secs();

                    let days_since_epoch = (now / 86400) as i64;
                    let time_of_day = now % 86400;
                    let hh = time_of_day / 3600;
                    let mm = (time_of_day % 3600) / 60;
                    let ss = time_of_day % 60;

                    // Calculate year, month, day from days since 1970-01-01
                    let mut year = 1970i64;
                    let mut remaining = days_since_epoch;
                    loop {
                        let days_in_year = if year % 4 == 0 && (year % 100 != 0 || year % 400 == 0) {
                            366
                        } else {
                            365
                        };
                        if remaining < days_in_year {
                            break;
                        }
                        remaining -= days_in_year;
                        year += 1;
                    }
                    let is_leap = year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
                    let month_days: [i64; 12] = if is_leap {
                        [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
                    } else {
                        [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
                    };
                    let mut month = 1i64;
                    for &d in &month_days {
                        if remaining < d {
                            break;
                        }
                        remaining -= d;
                        month += 1;
                    }
                    let day = remaining + 1;

                    let date_str = format!(
                        "{:04}{:02}{:02}{:02}{:02}{:02}00+0000",
                        year, month, day, hh, mm, ss
                    );
                    Ok(CobolValue::Alphanumeric(date_str))
                }
                "LENGTH" => {
                    let arg = args.first().ok_or_else(|| InterpreterError {
                        message: "LENGTH requires an argument".to_string(),
                    })?;
                    // For LENGTH OF variable, use metadata size if available
                    let len = if let SimpleExpr::Variable(var_name) = arg {
                        env.meta(var_name)
                            .map(|m| m.size)
                            .unwrap_or_else(|| {
                                // Fallback to actual string length
                                env.get(var_name)
                                    .map(|v| v.to_display_string().len())
                                    .unwrap_or(0)
                            })
                    } else {
                        let val = eval_expr(arg, env)?;
                        val.to_display_string().len()
                    };
                    Ok(CobolValue::from_i64(len as i64))
                }
                _ => {
                    // Unknown function - return 0
                    Ok(CobolValue::from_i64(0))
                }
            }
        }

        SimpleExpr::RefMod { variable, start, length } => {
            let val = eval_expr(variable, env)?;
            let s = val.to_display_string();
            let start_pos = to_numeric(&eval_expr(start, env)?).integer_part() as usize;
            // COBOL reference modification is 1-based
            let zero_start = if start_pos > 0 { start_pos - 1 } else { 0 };
            let end_pos = if let Some(len_expr) = length {
                let len = to_numeric(&eval_expr(len_expr, env)?).integer_part() as usize;
                (zero_start + len).min(s.len())
            } else {
                s.len()
            };
            let zero_start = zero_start.min(s.len());
            let substring = &s[zero_start..end_pos];
            Ok(CobolValue::Alphanumeric(substring.to_string()))
        }
    }
}

/// Compare two alphanumeric strings using COBOL rules (pad shorter with spaces).
fn compare_alpha(l: &str, r: &str) -> std::cmp::Ordering {
    let max_len = l.len().max(r.len());
    let l_padded: String = format!("{:<width$}", l, width = max_len);
    let r_padded: String = format!("{:<width$}", r, width = max_len);
    l_padded.cmp(&r_padded)
}

/// Compare two CobolValues.
fn compare_values(left: &CobolValue, right: &CobolValue) -> std::cmp::Ordering {
    match (left, right) {
        (CobolValue::Numeric(l), CobolValue::Numeric(r)) => l.value.cmp(&r.value),
        (CobolValue::Alphanumeric(l), CobolValue::Alphanumeric(r)) => compare_alpha(l, r),
        (CobolValue::Numeric(l), CobolValue::Alphanumeric(r)) => {
            compare_alpha(&l.to_display_string(), r)
        }
        (CobolValue::Alphanumeric(l), CobolValue::Numeric(r)) => {
            compare_alpha(l, &r.to_display_string())
        }
        _ => std::cmp::Ordering::Equal,
    }
}

/// Evaluate a condition.
fn eval_condition(cond: &SimpleCondition, env: &Environment, program: &SimpleProgram) -> Result<bool> {
    match cond {
        SimpleCondition::Compare { left, op, right } => {
            let l = eval_expr(left, env)?;
            let r = eval_expr(right, env)?;
            if std::env::var("ZOS_DEBUG_CMP").is_ok() {
                eprintln!("[CMP] {:?} {:?} {:?} => {:?}", l, op, r, compare_values(&l, &r));
            }
            Ok(match op {
                SimpleCompareOp::Equal => compare_values(&l, &r) == std::cmp::Ordering::Equal,
                SimpleCompareOp::NotEqual => compare_values(&l, &r) != std::cmp::Ordering::Equal,
                SimpleCompareOp::LessThan => compare_values(&l, &r) == std::cmp::Ordering::Less,
                SimpleCompareOp::LessOrEqual => {
                    compare_values(&l, &r) != std::cmp::Ordering::Greater
                }
                SimpleCompareOp::GreaterThan => {
                    compare_values(&l, &r) == std::cmp::Ordering::Greater
                }
                SimpleCompareOp::GreaterOrEqual => {
                    compare_values(&l, &r) != std::cmp::Ordering::Less
                }
            })
        }

        SimpleCondition::Not(inner) => Ok(!eval_condition(inner, env, program)?),

        SimpleCondition::And(left, right) => {
            Ok(eval_condition(left, env, program)? && eval_condition(right, env, program)?)
        }

        SimpleCondition::Or(left, right) => {
            Ok(eval_condition(left, env, program)? || eval_condition(right, env, program)?)
        }

        SimpleCondition::ConditionName(name) => {
            // Look up the level-88 condition: condition_name -> (parent_field, expected_value)
            if let Some((parent, expected_val)) = program.condition_names.get(&name.to_uppercase()) {
                let actual = env.get(parent)
                    .map(|v| v.to_display_string().trim().to_string())
                    .unwrap_or_default();
                let result = actual == *expected_val;
                if std::env::var("ZOS_DEBUG_CMP").is_ok() {
                    eprintln!("[COND88] {} => {}={:?} expected={:?} => {}",
                        name, parent, actual, expected_val, result);
                }
                Ok(result)
            } else {
                if std::env::var("ZOS_DEBUG_CMP").is_ok() {
                    eprintln!("[COND88] {} => NOT FOUND in condition_names", name);
                }
                Ok(false)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_env() -> Environment {
        let output = Vec::<u8>::new();
        let input = std::io::Cursor::new(Vec::<u8>::new());
        Environment::with_io(Box::new(output), Box::new(std::io::BufReader::new(input)))
    }

    #[test]
    fn test_move_and_compute() {
        let mut env = create_test_env();

        env.define(
            "X",
            DataItemMeta {
                size: 5,
                decimals: 0,
                is_numeric: true,
                picture: Some("9(5)".to_string()),
            },
        );

        env.define(
            "Y",
            DataItemMeta {
                size: 5,
                decimals: 0,
                is_numeric: true,
                picture: Some("9(5)".to_string()),
            },
        );

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
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

        env.define(
            "X",
            DataItemMeta {
                size: 5,
                decimals: 0,
                is_numeric: true,
                picture: Some("9(5)".to_string()),
            },
        );

        env.define(
            "RESULT",
            DataItemMeta {
                size: 10,
                decimals: 0,
                is_numeric: false,
                picture: Some("X(10)".to_string()),
            },
        );

        env.set("X", CobolValue::from_i64(5)).unwrap();

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
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

        env.define(
            "X",
            DataItemMeta {
                size: 5,
                decimals: 0,
                is_numeric: true,
                picture: Some("9(5)".to_string()),
            },
        );

        env.set("X", CobolValue::from_i64(10)).unwrap();

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
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
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            statements: vec![
                SimpleStatement::StopRun {
                    return_code: Some(4),
                },
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
