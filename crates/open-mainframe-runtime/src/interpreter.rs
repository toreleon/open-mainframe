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

/// State for an open file in the interpreter.
#[derive(Debug)]
pub struct FileState {
    /// Open mode.
    pub mode: SimpleOpenMode,
    /// Records (in-memory simulation).
    pub records: Vec<String>,
    /// Current record position (for sequential read).
    pub position: usize,
    /// File status code.
    pub status: String,
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
    /// Open files (in-memory simulation).
    pub files: HashMap<String, FileState>,
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
            files: HashMap::new(),
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
            files: HashMap::new(),
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

    /// Get a variable value as a string (returns empty string if not found).
    pub fn get_string(&self, name: &str) -> String {
        match self.get(name) {
            Some(CobolValue::Alphanumeric(s)) => s.clone(),
            Some(CobolValue::Numeric(n)) => n.to_string(),
            Some(CobolValue::Group(bytes)) => {
                String::from_utf8_lossy(bytes).to_string()
            }
            None => String::new(),
        }
    }

    /// Get all fields belonging to a group item (variables with the group prefix).
    pub fn get_group_fields(&self, group: &str) -> Vec<(String, CobolValue)> {
        let prefix = format!("{}-", group);
        let dot_prefix = format!("{}.", group);
        let mut fields: Vec<(String, CobolValue)> = self
            .variables
            .iter()
            .filter(|(k, _)| k.starts_with(&prefix) || k.starts_with(&dot_prefix))
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        fields.sort_by(|a, b| a.0.cmp(&b.0));
        fields
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
    /// MOVE value TO targets (each target is an expression that resolves to a variable name)
    Move { from: SimpleExpr, to: Vec<SimpleExpr> },
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
    /// INSPECT TALLYING
    InspectTallying {
        target: String,
        counter: String,
        for_clauses: Vec<SimpleInspectFor>,
    },
    /// INSPECT REPLACING
    InspectReplacing {
        target: String,
        rules: Vec<SimpleInspectRule>,
    },
    /// INSPECT CONVERTING
    InspectConverting {
        target: String,
        from: String,
        to: String,
        before: Option<String>,
        after: Option<String>,
    },
    /// UNSTRING
    Unstring {
        source: String,
        delimiters: Vec<String>,
        into: Vec<String>,
        tallying: Option<String>,
    },
    /// OPEN file
    FileOpen {
        files: Vec<(String, SimpleOpenMode)>,
    },
    /// CLOSE file
    FileClose {
        files: Vec<String>,
    },
    /// READ file
    FileRead {
        file: String,
        into: Option<String>,
        at_end: Option<Vec<SimpleStatement>>,
        not_at_end: Option<Vec<SimpleStatement>>,
    },
    /// WRITE record
    FileWrite {
        record: String,
        from: Option<String>,
        advancing: Option<SimpleAdvancing>,
    },
    /// SEARCH table
    Search {
        table: String,
        all: bool,
        at_end: Option<Vec<SimpleStatement>>,
        when_clauses: Vec<SimpleSearchWhen>,
    },
    /// JSON GENERATE: convert COBOL group item to JSON text.
    JsonGenerate {
        /// Receiver data item name (stores JSON text).
        receiver: String,
        /// Source data item name (COBOL group to convert).
        source: String,
        /// Optional COUNT IN data item (stores character count).
        count_in: Option<String>,
    },
    /// JSON PARSE: populate COBOL data items from JSON text.
    JsonParse {
        /// Source data item name (contains JSON text).
        source: String,
        /// Target data item name (COBOL group to populate).
        target: String,
    },
    /// XML GENERATE: convert COBOL group item to XML text.
    XmlGenerate {
        /// Receiver data item name (stores XML text).
        receiver: String,
        /// Source data item name (COBOL group to convert).
        source: String,
        /// Optional COUNT IN data item.
        count_in: Option<String>,
    },
    /// XML PARSE: invoke processing procedure for XML events.
    XmlParse {
        /// Source data item name (contains XML text).
        source: String,
        /// Processing procedure paragraph name.
        processing_procedure: String,
    },
    /// ALLOCATE: obtain dynamic storage.
    Allocate {
        /// Data item to allocate.
        data_name: String,
        /// Number of characters to allocate (if specified).
        characters: Option<i64>,
        /// Pointer to receive the address.
        returning: Option<String>,
    },
    /// FREE: release dynamic storage.
    Free {
        /// Pointer data item names to free.
        pointers: Vec<String>,
    },
    /// ENTRY: register alternate entry point (no-op at runtime dispatch level).
    Entry {
        /// Entry point literal name.
        literal: String,
    },
    /// ALTER: change GO TO target at runtime.
    Alter {
        /// Source paragraph.
        source: String,
        /// New target paragraph.
        target: String,
    },
}

/// INSPECT FOR clause (tallying).
#[derive(Debug, Clone)]
pub struct SimpleInspectFor {
    /// Mode: Characters, All, Leading, First.
    pub mode: SimpleInspectMode,
    /// Pattern to match (None for CHARACTERS mode).
    pub pattern: Option<String>,
    /// BEFORE INITIAL delimiter.
    pub before: Option<String>,
    /// AFTER INITIAL delimiter.
    pub after: Option<String>,
}

/// INSPECT REPLACING rule.
#[derive(Debug, Clone)]
pub struct SimpleInspectRule {
    /// Mode: Characters, All, Leading, First.
    pub mode: SimpleInspectMode,
    /// Pattern to match (None for CHARACTERS mode).
    pub pattern: Option<String>,
    /// Replacement string.
    pub by: String,
    /// BEFORE INITIAL delimiter.
    pub before: Option<String>,
    /// AFTER INITIAL delimiter.
    pub after: Option<String>,
}

/// INSPECT mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimpleInspectMode {
    /// CHARACTERS (count or replace all characters).
    Characters,
    /// ALL occurrences.
    All,
    /// LEADING occurrences only.
    Leading,
    /// FIRST occurrence only.
    First,
}

/// File open mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimpleOpenMode {
    Input,
    Output,
    InputOutput,
    Extend,
}

/// WRITE ADVANCING clause.
#[derive(Debug, Clone)]
pub enum SimpleAdvancing {
    Lines { count: i64, before: bool },
    Page { before: bool },
}

/// SEARCH WHEN clause.
#[derive(Debug, Clone)]
pub struct SimpleSearchWhen {
    /// Condition to check.
    pub condition: SimpleCondition,
    /// Statements to execute on match.
    pub statements: Vec<SimpleStatement>,
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
    /// Subscripted variable reference: ARRAY(INDEX)
    Subscript {
        variable: String,
        index: Box<SimpleExpr>,
    },
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
    /// Contained (nested) programs.
    pub contained_programs: Vec<SimpleProgram>,
    /// Whether this program has the INITIAL attribute (re-init on each CALL).
    pub is_initial: bool,
    /// Whether this program has the COMMON attribute (callable by siblings).
    pub is_common: bool,
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
pub fn compose_group(group_name: &str, program: &SimpleProgram, env: &Environment) -> Option<CobolValue> {
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
            for target_expr in to {
                let target = resolve_target_name(target_expr, env)?;
                env.set(&target, value.clone())?;
                // If target is a group item, decompose into sub-fields
                if program.group_layouts.contains_key(&target.to_uppercase()) {
                    let data = value.to_display_string();
                    if std::env::var("OPEN_MAINFRAME_DEBUG_GROUPS").is_ok() {
                        eprintln!("[MOVE→DECOMPOSE] target={} data_len={} data={:?}", target, data.len(), &data[..data.len().min(40)]);
                    }
                    decompose_group(&target, &data, program, env)?;
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
                            if std::env::var("OPEN_MAINFRAME_DEBUG_CMP").is_ok() {
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

        SimpleStatement::Call { program: call_target, using: _ } => {
            let prog_name = eval_expr(call_target, env)?;
            let target_name = prog_name.to_display_string().trim().to_uppercase();

            // Try to resolve the CALL to a contained program
            let found = program.contained_programs.iter().find(|p| {
                p.name.eq_ignore_ascii_case(&target_name)
            });

            if let Some(contained) = found {
                // Execute the contained program with its own environment
                let mut sub_env = Environment::new();
                // Initialize data items for the contained program
                for (name, meta) in &contained.data_items {
                    let val = if meta.is_numeric {
                        CobolValue::from_i64(0)
                    } else {
                        CobolValue::Alphanumeric(" ".repeat(meta.size))
                    };
                    sub_env.set(name, val).ok();
                }
                // Copy GLOBAL data from outer env to sub-env
                for (name, _meta) in &program.data_items {
                    if let Some(val) = env.get(name) {
                        sub_env.set(name, val.clone()).ok();
                    }
                }
                // Execute the contained program's initialization + main statements
                execute_statements(&contained.statements, contained, &mut sub_env)?;
            } else {
                // CALLed programs not resolved - log and continue
                env.display(
                    &format!("[CALL] {} - external program not resolved", target_name),
                    false,
                )?;
            }
        }

        SimpleStatement::PerformInline { until, statements: stmts, varying } => {
            // Initialize VARYING variable if present
            if let Some((ref var_name, ref from_expr, _)) = varying {
                let from_val = eval_expr(from_expr, env)?;
                env.set(var_name, from_val)?;
            }

            // Determine the UNTIL condition: prefer varying's until (embedded in `until` field)
            let effective_until = if let Some(ref _v) = varying {
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

        SimpleStatement::InspectTallying { target, counter, for_clauses } => {
            let target_val = env.get(target).cloned().unwrap_or(CobolValue::Alphanumeric(String::new()));
            let data = target_val.to_display_string();
            let mut tally: i64 = 0;

            for clause in for_clauses {
                let (start, end) = inspect_delimited_range(&data, &clause.before, &clause.after);
                let region = &data[start..end];

                match clause.mode {
                    SimpleInspectMode::Characters => {
                        tally += region.len() as i64;
                    }
                    SimpleInspectMode::All => {
                        if let Some(ref pat) = clause.pattern {
                            if !pat.is_empty() {
                                tally += region.matches(pat.as_str()).count() as i64;
                            }
                        }
                    }
                    SimpleInspectMode::Leading => {
                        if let Some(ref pat) = clause.pattern {
                            if !pat.is_empty() {
                                let mut pos = 0;
                                while pos + pat.len() <= region.len() && &region[pos..pos + pat.len()] == pat.as_str() {
                                    tally += 1;
                                    pos += pat.len();
                                }
                            }
                        }
                    }
                    SimpleInspectMode::First => {
                        if let Some(ref pat) = clause.pattern {
                            if region.contains(pat.as_str()) {
                                tally += 1;
                            }
                        }
                    }
                }
            }

            // Add tally to the counter variable (INSPECT adds to existing value)
            let current = env.get(counter).cloned().unwrap_or(CobolValue::from_i64(0));
            let new_val = to_numeric(&current).add(&NumericValue::from_i64(tally));
            env.set(counter, CobolValue::Numeric(new_val))?;
        }

        SimpleStatement::InspectReplacing { target, rules } => {
            let target_val = env.get(target).cloned().unwrap_or(CobolValue::Alphanumeric(String::new()));
            let mut data = target_val.to_display_string();

            for rule in rules {
                let (start, end) = inspect_delimited_range(&data, &rule.before, &rule.after);

                match rule.mode {
                    SimpleInspectMode::Characters => {
                        // Replace every character in range with first char of `by`
                        if let Some(by_ch) = rule.by.chars().next() {
                            let mut chars: Vec<char> = data.chars().collect();
                            for i in start..end.min(chars.len()) {
                                chars[i] = by_ch;
                            }
                            data = chars.into_iter().collect();
                        }
                    }
                    SimpleInspectMode::All => {
                        if let Some(ref pat) = rule.pattern {
                            if !pat.is_empty() {
                                let prefix = &data[..start];
                                let region = &data[start..end];
                                let suffix = &data[end..];
                                let replaced = region.replace(pat.as_str(), &rule.by);
                                data = format!("{}{}{}", prefix, replaced, suffix);
                            }
                        }
                    }
                    SimpleInspectMode::Leading => {
                        if let Some(ref pat) = rule.pattern {
                            if !pat.is_empty() {
                                let prefix = data[..start].to_string();
                                let region = data[start..end].to_string();
                                let suffix = data[end..].to_string();
                                let mut result = String::new();
                                let mut pos = 0;
                                // Replace leading occurrences only
                                while pos + pat.len() <= region.len() && &region[pos..pos + pat.len()] == pat.as_str() {
                                    result.push_str(&rule.by);
                                    pos += pat.len();
                                }
                                result.push_str(&region[pos..]);
                                data = format!("{}{}{}", prefix, result, suffix);
                            }
                        }
                    }
                    SimpleInspectMode::First => {
                        if let Some(ref pat) = rule.pattern {
                            if !pat.is_empty() {
                                let prefix = data[..start].to_string();
                                let region = data[start..end].to_string();
                                let suffix = data[end..].to_string();
                                let replaced = region.replacen(pat.as_str(), &rule.by, 1);
                                data = format!("{}{}{}", prefix, replaced, suffix);
                            }
                        }
                    }
                }
            }

            env.set(target, CobolValue::Alphanumeric(data))?;
        }

        SimpleStatement::InspectConverting { target, from, to, before, after } => {
            let target_val = env.get(target).cloned().unwrap_or(CobolValue::Alphanumeric(String::new()));
            let mut data = target_val.to_display_string();
            let (start, end) = inspect_delimited_range(&data, before, after);

            let from_chars: Vec<char> = from.chars().collect();
            let to_chars: Vec<char> = to.chars().collect();
            let mut chars: Vec<char> = data.chars().collect();

            for i in start..end.min(chars.len()) {
                if let Some(pos) = from_chars.iter().position(|&c| c == chars[i]) {
                    if pos < to_chars.len() {
                        chars[i] = to_chars[pos];
                    }
                }
            }
            data = chars.into_iter().collect();
            env.set(target, CobolValue::Alphanumeric(data))?;
        }

        SimpleStatement::Unstring { source, delimiters, into, tallying } => {
            let source_val = env.get(source).cloned().unwrap_or(CobolValue::Alphanumeric(String::new()));
            let data = source_val.to_display_string();

            let parts: Vec<&str> = if delimiters.is_empty() {
                vec![data.as_str()]
            } else {
                // Split by the first delimiter (simplified: multiple delimiters act as alternatives)
                let mut result = vec![data.as_str()];
                for delim in delimiters {
                    let mut new_result = Vec::new();
                    for part in &result {
                        let splits: Vec<&str> = part.split(delim.as_str()).collect();
                        new_result.extend(splits);
                    }
                    result = new_result;
                }
                result
            };

            let mut field_count: i64 = 0;
            for (i, target) in into.iter().enumerate() {
                if i < parts.len() {
                    env.set(target, CobolValue::Alphanumeric(parts[i].to_string()))?;
                    field_count += 1;
                }
            }

            if let Some(ref tally_var) = tallying {
                env.set(tally_var, CobolValue::from_i64(field_count))?;
            }
        }

        SimpleStatement::FileOpen { files } => {
            for (file_name, mode) in files {
                let file_upper = file_name.to_uppercase();
                let records = if *mode == SimpleOpenMode::Input || *mode == SimpleOpenMode::InputOutput {
                    // Try to load records from a pre-set variable "{FILE}-DATA"
                    let data_var = format!("{}-DATA", file_upper);
                    if let Some(val) = env.get(&data_var) {
                        val.to_display_string().lines().map(|l| l.to_string()).collect()
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                };
                env.files.insert(file_upper.clone(), FileState {
                    mode: *mode,
                    records,
                    position: 0,
                    status: "00".to_string(),
                });
                // Set FILE STATUS variable if it exists
                let status_var = format!("{}-STATUS", file_upper);
                let _ = env.set(&status_var, CobolValue::Alphanumeric("00".to_string()));
            }
        }

        SimpleStatement::FileClose { files } => {
            for file_name in files {
                let file_upper = file_name.to_uppercase();
                if env.files.remove(&file_upper).is_none() {
                    // File wasn't open — status 42
                    let status_var = format!("{}-STATUS", file_upper);
                    let _ = env.set(&status_var, CobolValue::Alphanumeric("42".to_string()));
                } else {
                    let status_var = format!("{}-STATUS", file_upper);
                    let _ = env.set(&status_var, CobolValue::Alphanumeric("00".to_string()));
                }
            }
        }

        SimpleStatement::FileRead { file, into, at_end, not_at_end } => {
            let file_upper = file.to_uppercase();
            let (record, at_eof) = if let Some(state) = env.files.get_mut(&file_upper) {
                if state.position < state.records.len() {
                    let rec = state.records[state.position].clone();
                    state.position += 1;
                    state.status = "00".to_string();
                    (Some(rec), false)
                } else {
                    state.status = "10".to_string();
                    (None, true)
                }
            } else {
                (None, true)
            };

            // Set FILE STATUS
            let status = if at_eof { "10" } else { "00" };
            let status_var = format!("{}-STATUS", file_upper);
            let _ = env.set(&status_var, CobolValue::Alphanumeric(status.to_string()));

            if let Some(rec) = record {
                // Set INTO target or the file record variable
                if let Some(ref into_var) = into {
                    env.set(into_var, CobolValue::Alphanumeric(rec))?;
                } else {
                    // Set the record variable (conventionally: {FILE}-RECORD)
                    let rec_var = format!("{}-RECORD", file_upper);
                    env.set(&rec_var, CobolValue::Alphanumeric(rec))?;
                }
                // Execute NOT AT END
                if let Some(ref stmts) = not_at_end {
                    execute_statements(stmts, program, env)?;
                }
            } else {
                // Execute AT END
                if let Some(ref stmts) = at_end {
                    execute_statements(stmts, program, env)?;
                }
            }
        }

        SimpleStatement::FileWrite { record, from, advancing } => {
            // Get the record data
            let data = if let Some(ref from_var) = from {
                env.get(from_var).map(|v| v.to_display_string()).unwrap_or_default()
            } else {
                env.get(record).map(|v| v.to_display_string()).unwrap_or_default()
            };

            // Find which file this record belongs to (try {RECORD}-FILE or any open output file)
            let file_key = {
                let record_upper = record.to_uppercase();
                // Convention: look for an output file whose name is a prefix of the record name
                let mut found = None;
                for (name, state) in &env.files {
                    if state.mode == SimpleOpenMode::Output || state.mode == SimpleOpenMode::Extend
                        || state.mode == SimpleOpenMode::InputOutput
                    {
                        if found.is_none() {
                            found = Some(name.clone());
                        }
                        // Check if record name starts with file name
                        if record_upper.starts_with(name.as_str()) {
                            found = Some(name.clone());
                            break;
                        }
                    }
                }
                found
            };

            if let Some(ref fk) = file_key {
                if let Some(state) = env.files.get_mut(fk) {
                    // Handle ADVANCING
                    if let Some(ref adv) = advancing {
                        match adv {
                            SimpleAdvancing::Lines { count, before } => {
                                if *before {
                                    for _ in 0..*count { state.records.push(String::new()); }
                                    state.records.push(data.clone());
                                } else {
                                    state.records.push(data.clone());
                                    for _ in 0..*count { state.records.push(String::new()); }
                                }
                            }
                            SimpleAdvancing::Page { before } => {
                                if *before {
                                    state.records.push("\x0C".to_string()); // form feed
                                }
                                state.records.push(data.clone());
                                if !before {
                                    state.records.push("\x0C".to_string());
                                }
                            }
                        }
                    } else {
                        state.records.push(data);
                    }
                    state.status = "00".to_string();
                }
            }
        }

        SimpleStatement::Search { table, all, at_end, when_clauses } => {
            // Get table size by looking for subscripted variables
            let table_upper = table.to_uppercase();
            let max_entries = 100; // Safety limit
            let mut found = false;

            if *all {
                // Binary search: simplified — just check each entry (proper binary search
                // would require knowing the sorted key, which we approximate here)
                let mut lo = 1i64;
                let mut hi = max_entries;
                while lo <= hi && !found && !env.is_stopped() {
                    let mid = (lo + hi) / 2;
                    // Set the index variable
                    let idx_var = format!("{}-IDX", table_upper);
                    env.set(&idx_var, CobolValue::from_i64(mid))?;

                    // Check if entry exists
                    let entry_key = format!("{}({})", table_upper, mid);
                    if env.get(&entry_key).is_none() {
                        hi = mid - 1;
                        continue;
                    }

                    // Check WHEN conditions
                    for when in when_clauses {
                        if eval_condition(&when.condition, env, program)? {
                            execute_statements(&when.statements, program, env)?;
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        // Can't determine search direction without key info; just try next
                        lo = mid + 1;
                    }
                }
            } else {
                // Sequential search
                for i in 1..=max_entries {
                    if env.is_stopped() { break; }

                    // Set index variable
                    let idx_var = format!("{}-IDX", table_upper);
                    env.set(&idx_var, CobolValue::from_i64(i))?;

                    // Check if entry exists
                    let entry_key = format!("{}({})", table_upper, i);
                    if env.get(&entry_key).is_none() {
                        break; // Past end of table
                    }

                    // Check WHEN conditions
                    for when in when_clauses {
                        if eval_condition(&when.condition, env, program)? {
                            execute_statements(&when.statements, program, env)?;
                            found = true;
                            break;
                        }
                    }
                    if found { break; }
                }
            }

            if !found {
                if let Some(ref stmts) = at_end {
                    execute_statements(stmts, program, env)?;
                }
            }
        }

        SimpleStatement::JsonGenerate { receiver, source, count_in } => {
            // Collect fields from the source group item into a JSON object
            let source_upper = source.to_uppercase();
            let mut json_obj = String::from("{");
            let mut first = true;

            // Iterate through environment variables that belong to the source group
            let fields = env.get_group_fields(&source_upper);
            for (field_name, field_value) in &fields {
                if !first {
                    json_obj.push(',');
                }
                first = false;
                // Strip the group prefix to get the field name
                let short_name = field_name
                    .strip_prefix(&format!("{}-", source_upper))
                    .or_else(|| field_name.strip_prefix(&format!("{}.", source_upper)))
                    .unwrap_or(field_name);
                json_obj.push('"');
                json_obj.push_str(short_name);
                json_obj.push_str("\":");
                match field_value {
                    CobolValue::Numeric(n) => {
                        json_obj.push_str(&n.to_string());
                    }
                    CobolValue::Alphanumeric(s) => {
                        json_obj.push('"');
                        // Escape special characters
                        for ch in s.trim_end().chars() {
                            match ch {
                                '"' => json_obj.push_str("\\\""),
                                '\\' => json_obj.push_str("\\\\"),
                                '\n' => json_obj.push_str("\\n"),
                                '\r' => json_obj.push_str("\\r"),
                                '\t' => json_obj.push_str("\\t"),
                                c => json_obj.push(c),
                            }
                        }
                        json_obj.push('"');
                    }
                    CobolValue::Group(_) => {
                        json_obj.push_str("{}");
                    }
                }
            }
            json_obj.push('}');

            let json_len = json_obj.len() as i64;
            env.set(receiver, CobolValue::Alphanumeric(json_obj))?;

            if let Some(ref count_var) = count_in {
                env.set(count_var, CobolValue::from_i64(json_len))?;
            }

            // Set JSON-CODE to 0 (success)
            env.set("JSON-CODE", CobolValue::from_i64(0))?;
        }

        SimpleStatement::JsonParse { source, target } => {
            // Get the JSON text from the source variable
            let json_text = env.get_string(source);
            let json_text = json_text.trim();

            if json_text.starts_with('{') {
                // Simple JSON object parsing — extract key-value pairs
                let target_upper = target.to_uppercase();
                // Strip outer braces and split on commas (simplified parser)
                let inner = &json_text[1..json_text.len().saturating_sub(1)];
                for pair in split_json_pairs(inner) {
                    let pair = pair.trim();
                    if let Some(colon_pos) = pair.find(':') {
                        let key = pair[..colon_pos].trim().trim_matches('"');
                        let value = pair[colon_pos + 1..].trim();
                        let field_name = format!("{}-{}", target_upper, key.to_uppercase());

                        if value.starts_with('"') && value.ends_with('"') {
                            // String value
                            let s = &value[1..value.len() - 1];
                            env.set(&field_name, CobolValue::Alphanumeric(s.to_string()))?;
                        } else if let Ok(n) = value.parse::<i64>() {
                            env.set(&field_name, CobolValue::from_i64(n))?;
                        } else {
                            env.set(&field_name, CobolValue::Alphanumeric(value.to_string()))?;
                        }
                    }
                }

                env.set("JSON-CODE", CobolValue::from_i64(0))?;
            } else {
                // Invalid JSON
                env.set("JSON-CODE", CobolValue::from_i64(1))?;
            }
        }

        SimpleStatement::XmlGenerate { receiver, source, count_in } => {
            let source_upper = source.to_uppercase();
            let mut xml = String::from("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
            xml.push('<');
            xml.push_str(&source_upper);
            xml.push('>');

            let fields = env.get_group_fields(&source_upper);
            for (field_name, field_value) in &fields {
                let short_name = field_name
                    .strip_prefix(&format!("{}-", source_upper))
                    .or_else(|| field_name.strip_prefix(&format!("{}.", source_upper)))
                    .unwrap_or(field_name);
                xml.push('<');
                xml.push_str(short_name);
                xml.push('>');
                match field_value {
                    CobolValue::Numeric(n) => xml.push_str(&n.to_string()),
                    CobolValue::Alphanumeric(s) => {
                        for ch in s.trim_end().chars() {
                            match ch {
                                '<' => xml.push_str("&lt;"),
                                '>' => xml.push_str("&gt;"),
                                '&' => xml.push_str("&amp;"),
                                '"' => xml.push_str("&quot;"),
                                '\'' => xml.push_str("&apos;"),
                                c => xml.push(c),
                            }
                        }
                    }
                    CobolValue::Group(_) => {}
                }
                xml.push_str("</");
                xml.push_str(short_name);
                xml.push('>');
            }

            xml.push_str("</");
            xml.push_str(&source_upper);
            xml.push('>');

            let xml_len = xml.len() as i64;
            env.set(receiver, CobolValue::Alphanumeric(xml))?;
            if let Some(ref count_var) = count_in {
                env.set(count_var, CobolValue::from_i64(xml_len))?;
            }
            env.set("XML-CODE", CobolValue::from_i64(0))?;
        }

        SimpleStatement::XmlParse { source, processing_procedure } => {
            let xml_text = env.get_string(source);
            let xml_text = xml_text.trim();
            let proc_upper = processing_procedure.to_uppercase();

            env.set("XML-EVENT", CobolValue::Alphanumeric("START-OF-DOCUMENT".to_string()))?;
            env.set("XML-TEXT", CobolValue::Alphanumeric(String::new()))?;
            env.set("XML-CODE", CobolValue::from_i64(0))?;
            if let Some(para) = program.paragraphs.get(&proc_upper) {
                execute_statements(&para.clone(), program, env)?;
            }

            let mut pos = 0;
            let bytes = xml_text.as_bytes();
            if xml_text.starts_with("<?") {
                if let Some(end) = xml_text.find("?>") {
                    pos = end + 2;
                }
            }

            while pos < bytes.len() && !env.is_stopped() {
                if let Some(CobolValue::Numeric(n)) = env.get("XML-CODE") {
                    if !n.is_zero() { break; }
                }
                if bytes[pos] == b'<' {
                    if pos + 1 < bytes.len() && bytes[pos + 1] == b'/' {
                        let tag_start = pos + 2;
                        if let Some(end) = xml_text[tag_start..].find('>') {
                            let tag_name = &xml_text[tag_start..tag_start + end];
                            env.set("XML-EVENT", CobolValue::Alphanumeric("END-OF-ELEMENT".to_string()))?;
                            env.set("XML-TEXT", CobolValue::Alphanumeric(tag_name.to_string()))?;
                            if let Some(para) = program.paragraphs.get(&proc_upper) {
                                execute_statements(&para.clone(), program, env)?;
                            }
                            pos = tag_start + end + 1;
                        } else { break; }
                    } else {
                        let tag_start = pos + 1;
                        if let Some(end) = xml_text[tag_start..].find('>') {
                            let tag_name = &xml_text[tag_start..tag_start + end];
                            let elem_name = tag_name.split_whitespace().next().unwrap_or(tag_name);
                            env.set("XML-EVENT", CobolValue::Alphanumeric("START-OF-ELEMENT".to_string()))?;
                            env.set("XML-TEXT", CobolValue::Alphanumeric(elem_name.to_string()))?;
                            if let Some(para) = program.paragraphs.get(&proc_upper) {
                                execute_statements(&para.clone(), program, env)?;
                            }
                            pos = tag_start + end + 1;
                        } else { break; }
                    }
                } else {
                    let text_start = pos;
                    while pos < bytes.len() && bytes[pos] != b'<' { pos += 1; }
                    let text = xml_text[text_start..pos].trim();
                    if !text.is_empty() {
                        env.set("XML-EVENT", CobolValue::Alphanumeric("CONTENT-CHARACTERS".to_string()))?;
                        env.set("XML-TEXT", CobolValue::Alphanumeric(text.to_string()))?;
                        if let Some(para) = program.paragraphs.get(&proc_upper) {
                            execute_statements(&para.clone(), program, env)?;
                        }
                    }
                }
            }

            env.set("XML-EVENT", CobolValue::Alphanumeric("END-OF-DOCUMENT".to_string()))?;
            env.set("XML-TEXT", CobolValue::Alphanumeric(String::new()))?;
            if let Some(para) = program.paragraphs.get(&proc_upper) {
                execute_statements(&para.clone(), program, env)?;
            }
        }

        SimpleStatement::Allocate { data_name, characters, returning } => {
            let size = characters.unwrap_or(256) as usize;
            let buffer = " ".repeat(size);
            let data_upper = data_name.to_uppercase();
            env.set(&data_upper, CobolValue::Alphanumeric(buffer))?;

            if let Some(ref ptr_name) = returning {
                // Set the pointer to the data name (symbolic reference)
                env.set(ptr_name, CobolValue::Alphanumeric(data_upper))?;
            }
        }

        SimpleStatement::Free { pointers } => {
            for ptr in pointers {
                let ptr_upper = ptr.to_uppercase();
                // Get the target name if it's a pointer
                let target = env.get_string(&ptr_upper);
                if !target.is_empty() {
                    // Clear the allocated storage
                    env.set(&target, CobolValue::Alphanumeric(String::new()))?;
                }
                // Set pointer to NULL (empty)
                env.set(&ptr_upper, CobolValue::Alphanumeric(String::new()))?;
            }
        }

        SimpleStatement::Entry { literal: _ } => {
            // ENTRY registers an alternate entry point — this is handled at
            // the program/call dispatch level, not during statement execution.
            // At runtime, encountering ENTRY inline is a no-op (fall-through).
        }

        SimpleStatement::Alter { source, target } => {
            // ALTER modifies the GO TO target of a paragraph at runtime.
            // Store the altered target in a special runtime variable.
            let alter_key = format!("__ALTER__{}", source.to_uppercase());
            env.set(&alter_key, CobolValue::Alphanumeric(target.to_uppercase()))?;
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

/// Split a JSON object's inner text into key-value pairs, respecting nested strings.
fn split_json_pairs(inner: &str) -> Vec<&str> {
    let mut pairs = Vec::new();
    let mut start = 0;
    let mut in_string = false;
    let mut depth = 0;

    for (i, ch) in inner.char_indices() {
        match ch {
            '"' if !in_string => in_string = true,
            '"' if in_string => in_string = false,
            '{' | '[' if !in_string => depth += 1,
            '}' | ']' if !in_string => depth -= 1,
            ',' if !in_string && depth == 0 => {
                pairs.push(&inner[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }
    if start < inner.len() {
        pairs.push(&inner[start..]);
    }
    pairs
}

/// Compute the effective byte range for INSPECT BEFORE/AFTER INITIAL delimiters.
fn inspect_delimited_range(data: &str, before: &Option<String>, after: &Option<String>) -> (usize, usize) {
    let mut start = 0;
    let mut end = data.len();

    if let Some(ref after_delim) = after {
        if let Some(pos) = data.find(after_delim.as_str()) {
            start = pos + after_delim.len();
        }
    }
    if let Some(ref before_delim) = before {
        if let Some(pos) = data[start..].find(before_delim.as_str()) {
            end = start + pos;
        }
    }

    (start, end)
}

/// Resolve a target expression to a variable name string.
fn resolve_target_name(expr: &SimpleExpr, env: &Environment) -> Result<String> {
    match expr {
        SimpleExpr::Variable(name) => Ok(name.clone()),
        SimpleExpr::Subscript { variable, index } => {
            let idx = to_numeric(&eval_expr(index, env)?).integer_part();
            Ok(format!("{}({})", variable, idx))
        }
        _ => Err(InterpreterError {
            message: format!("Invalid MOVE target expression: {:?}", expr),
        }),
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

        SimpleExpr::Subscript { variable, index } => {
            let idx = to_numeric(&eval_expr(index, env)?).integer_part();
            let indexed_name = format!("{}({})", variable, idx);
            env.get(&indexed_name)
                .cloned()
                .or_else(|| env.get(variable).cloned())
                .ok_or_else(|| InterpreterError {
                    message: format!("Undefined subscripted variable: {}", indexed_name),
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
            if std::env::var("OPEN_MAINFRAME_DEBUG_CMP").is_ok() {
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
                if std::env::var("OPEN_MAINFRAME_DEBUG_CMP").is_ok() {
                    eprintln!("[COND88] {} => {}={:?} expected={:?} => {}",
                        name, parent, actual, expected_val, result);
                }
                Ok(result)
            } else {
                if std::env::var("OPEN_MAINFRAME_DEBUG_CMP").is_ok() {
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
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            statements: vec![
                SimpleStatement::Move {
                    from: SimpleExpr::Integer(10),
                    to: vec![SimpleExpr::Variable("X".to_string())],
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
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            statements: vec![SimpleStatement::If {
                condition: SimpleCondition::Compare {
                    left: SimpleExpr::Variable("X".to_string()),
                    op: SimpleCompareOp::GreaterThan,
                    right: SimpleExpr::Integer(3),
                },
                then_branch: vec![SimpleStatement::Move {
                    from: SimpleExpr::String("BIG".to_string()),
                    to: vec![SimpleExpr::Variable("RESULT".to_string())],
                }],
                else_branch: Some(vec![SimpleStatement::Move {
                    from: SimpleExpr::String("SMALL".to_string()),
                    to: vec![SimpleExpr::Variable("RESULT".to_string())],
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
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
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
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            statements: vec![
                SimpleStatement::StopRun {
                    return_code: Some(4),
                },
                // This should not execute
                SimpleStatement::Move {
                    from: SimpleExpr::Integer(999),
                    to: vec![SimpleExpr::Variable("SHOULD_NOT_EXIST".to_string())],
                },
            ],
            paragraphs: HashMap::new(),
        };

        let rc = execute(&program, &mut env).unwrap();
        assert_eq!(rc, 4);
        assert!(env.is_stopped());
    }

    #[test]
    fn test_occurs_redefines_filler_initialization() {
        // Simulates the COADM02Y.cpy pattern:
        //   01 MENU-OPTIONS.
        //     05 OPT-COUNT  PIC 9(02) VALUE 6.
        //     05 OPTIONS-DATA.
        //       10 FILLER PIC 9(02) VALUE 1.
        //       10 FILLER PIC X(05) VALUE 'Alpha'.
        //       10 FILLER PIC 9(02) VALUE 2.
        //       10 FILLER PIC X(05) VALUE 'Beta '.
        //     05 OPTIONS-ARRAY REDEFINES OPTIONS-DATA.
        //       10 OPT-ENTRY OCCURS 2 TIMES.
        //         15 OPT-NUM  PIC 9(02).
        //         15 OPT-NAME PIC X(05).
        //
        // Group layout for MENU-OPTIONS should have:
        //   OPT-COUNT at offset 0, size 2
        //   FILLER defaults at offsets 2,4,9,11
        //   OPT-NUM(1) at offset 2, OPT-NAME(1) at offset 4
        //   OPT-NUM(2) at offset 9, OPT-NAME(2) at offset 11

        let mut env = create_test_env();

        // Define data items
        env.define("OPT-COUNT", DataItemMeta {
            size: 2, decimals: 0, is_numeric: true,
            picture: Some("9(02)".to_string()),
        });

        // Build group layout for MENU-OPTIONS
        let group_fields = vec![
            // Named field: OPT-COUNT
            GroupField { name: "OPT-COUNT".to_string(), offset: 0, size: 2,
                         is_numeric: true, default_value: None },
            // FILLER: VALUE 1 (from OPTIONS-DATA)
            GroupField { name: String::new(), offset: 2, size: 2,
                         is_numeric: false, default_value: Some("1".to_string()) },
            // FILLER: VALUE 'Alpha' (from OPTIONS-DATA)
            GroupField { name: String::new(), offset: 4, size: 5,
                         is_numeric: false, default_value: Some("Alpha".to_string()) },
            // FILLER: VALUE 2 (from OPTIONS-DATA)
            GroupField { name: String::new(), offset: 9, size: 2,
                         is_numeric: false, default_value: Some("2".to_string()) },
            // FILLER: VALUE 'Beta ' (from OPTIONS-DATA)
            GroupField { name: String::new(), offset: 11, size: 5,
                         is_numeric: false, default_value: Some("Beta ".to_string()) },
            // REDEFINES overlay: OPT-NUM(1)
            GroupField { name: "OPT-NUM(1)".to_string(), offset: 2, size: 2,
                         is_numeric: true, default_value: None },
            // REDEFINES overlay: OPT-NAME(1)
            GroupField { name: "OPT-NAME(1)".to_string(), offset: 4, size: 5,
                         is_numeric: false, default_value: None },
            // REDEFINES overlay: OPT-NUM(2)
            GroupField { name: "OPT-NUM(2)".to_string(), offset: 9, size: 2,
                         is_numeric: true, default_value: None },
            // REDEFINES overlay: OPT-NAME(2)
            GroupField { name: "OPT-NAME(2)".to_string(), offset: 11, size: 5,
                         is_numeric: false, default_value: None },
        ];

        let mut group_layouts = HashMap::new();
        group_layouts.insert("MENU-OPTIONS".to_string(), group_fields);

        // Build the initial composed string from FILLER defaults (what load_program does)
        let fields = group_layouts.get("MENU-OPTIONS").unwrap();
        let total_size = fields.iter().map(|f| f.offset + f.size).max().unwrap_or(0);
        let mut buffer = vec![b' '; total_size];
        for field in fields {
            if let Some(ref default) = field.default_value {
                let bytes = default.as_bytes();
                for i in 0..field.size.min(total_size - field.offset) {
                    buffer[field.offset + i] = if i < bytes.len() { bytes[i] } else { b' ' };
                }
            }
        }
        let init_str = String::from_utf8_lossy(&buffer).to_string();

        // Statements: 1) FILLER group init, 2) VALUE init for OPT-COUNT
        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            condition_names: HashMap::new(),
            group_layouts,
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            statements: vec![
                // Step 1: Initialize group from FILLER defaults
                SimpleStatement::Move {
                    from: SimpleExpr::String(init_str.clone()),
                    to: vec![SimpleExpr::Variable("MENU-OPTIONS".to_string())],
                },
                // Step 2: Initialize named field VALUE
                SimpleStatement::Move {
                    from: SimpleExpr::Integer(6),
                    to: vec![SimpleExpr::Variable("OPT-COUNT".to_string())],
                },
            ],
            paragraphs: HashMap::new(),
        };

        execute(&program, &mut env).unwrap();

        // Verify: OPT-COUNT should be 6 (from VALUE clause, applied after group init)
        let count = env.get("OPT-COUNT").expect("OPT-COUNT missing");
        assert_eq!(to_numeric(count).integer_part(), 6,
            "OPT-COUNT should be 6, got {:?}", count);

        // Verify: OPT-NUM(1) should be 1 (from FILLER VALUE 1, through REDEFINES)
        let num1 = env.get("OPT-NUM(1)").expect("OPT-NUM(1) missing");
        assert_eq!(to_numeric(num1).integer_part(), 1,
            "OPT-NUM(1) should be 1, got {:?}", num1);

        // Verify: OPT-NAME(1) should be "Alpha"
        let name1 = env.get("OPT-NAME(1)").expect("OPT-NAME(1) missing");
        assert_eq!(name1.to_display_string(), "Alpha",
            "OPT-NAME(1) should be 'Alpha', got {:?}", name1);

        // Verify: OPT-NUM(2) should be 2
        let num2 = env.get("OPT-NUM(2)").expect("OPT-NUM(2) missing");
        assert_eq!(to_numeric(num2).integer_part(), 2,
            "OPT-NUM(2) should be 2, got {:?}", num2);

        // Verify: OPT-NAME(2) should be "Beta "
        let name2 = env.get("OPT-NAME(2)").expect("OPT-NAME(2) missing");
        assert_eq!(name2.to_display_string(), "Beta ",
            "OPT-NAME(2) should be 'Beta ', got {:?}", name2);

        // Verify: subscript evaluation works
        let subscript_expr = SimpleExpr::Subscript {
            variable: "OPT-NUM".to_string(),
            index: Box::new(SimpleExpr::Integer(2)),
        };
        let val = eval_expr(&subscript_expr, &env).unwrap();
        assert_eq!(to_numeric(&val).integer_part(), 2,
            "OPT-NUM(2) via subscript should be 2, got {:?}", val);
    }
}
