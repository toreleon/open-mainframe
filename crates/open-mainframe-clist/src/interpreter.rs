//! # CLIST Interpreter Core (CL-101)
//!
//! Executes CLIST scripts with variable substitution, control flow,
//! system control variables, and subprocedure support.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use crate::functions::evaluate_builtin;
use crate::io::{ControlOptions, IoManager};
use crate::parser::*;
use crate::tso_bridge::TsoEnvironment;

// ─────────────────────── Errors ───────────────────────

/// Interpreter error.
#[derive(Debug, thiserror::Error)]
pub enum InterpreterError {
    #[error("undefined variable: &{0}")]
    UndefinedVariable(String),
    #[error("type error: {0}")]
    TypeError(String),
    #[error("label not found: {0}")]
    LabelNotFound(String),
    #[error("division by zero")]
    DivisionByZero,
    #[error("parse error: {0}")]
    Parse(#[from] ParseError),
    #[error("I/O error: {0}")]
    Io(#[from] crate::io::IoError),
    #[error("exit with code {0}")]
    Exit(i32),
    #[error("max iterations exceeded")]
    MaxIterations,
    #[error("{0}")]
    General(String),
}

/// Execution result.
pub type ExecResult<T> = Result<T, InterpreterError>;

// ─────────────────────── CL-101.1: Variable Pool ───────────────────────

/// Variable pool for CLIST execution.
#[derive(Debug, Clone)]
pub struct VariablePool {
    /// Local variables.
    locals: HashMap<String, String>,
    /// Global (shared) variables.
    globals: Arc<Mutex<HashMap<String, String>>>,
    /// Names declared GLOBAL.
    global_names: Vec<String>,
}

impl Default for VariablePool {
    fn default() -> Self {
        Self::new()
    }
}

impl VariablePool {
    pub fn new() -> Self {
        Self {
            locals: HashMap::new(),
            globals: Arc::new(Mutex::new(HashMap::new())),
            global_names: Vec::new(),
        }
    }

    /// Create with shared global pool.
    pub fn with_globals(globals: Arc<Mutex<HashMap<String, String>>>) -> Self {
        Self {
            locals: HashMap::new(),
            globals,
            global_names: Vec::new(),
        }
    }

    /// Set a variable value.
    pub fn set(&mut self, name: &str, value: String) {
        let upper = name.to_uppercase();
        if self.global_names.contains(&upper) {
            if let Ok(mut g) = self.globals.lock() {
                g.insert(upper, value);
            }
        } else {
            self.locals.insert(upper, value);
        }
    }

    /// Get a variable value.
    pub fn get(&self, name: &str) -> Option<String> {
        let upper = name.to_uppercase();
        if self.global_names.contains(&upper) {
            if let Ok(g) = self.globals.lock() {
                return g.get(&upper).cloned();
            }
        }
        self.locals.get(&upper).cloned()
    }

    /// Declare names as global.
    pub fn declare_global(&mut self, names: &[String]) {
        for name in names {
            let upper = name.to_uppercase();
            if !self.global_names.contains(&upper) {
                self.global_names.push(upper.clone());
                // Move local to global if exists
                if let Some(val) = self.locals.remove(&upper) {
                    if let Ok(mut g) = self.globals.lock() {
                        g.insert(upper, val);
                    }
                }
            }
        }
    }

    /// Declare names as non-global (local).
    pub fn declare_nglobal(&mut self, names: &[String]) {
        for name in names {
            let upper = name.to_uppercase();
            self.global_names.retain(|n| n != &upper);
        }
    }

    /// Get the shared globals reference.
    pub fn globals_ref(&self) -> Arc<Mutex<HashMap<String, String>>> {
        self.globals.clone()
    }
}

// ─────────────────────── CL-101.2: System Control Variables ───────────────────────

/// A system control variable.
#[derive(Debug, Clone)]
pub struct ControlVariable {
    pub name: String,
    pub value: String,
}

fn get_system_variables() -> HashMap<String, String> {
    let mut vars = HashMap::new();
    vars.insert("LASTCC".to_string(), "0".to_string());
    vars.insert("MAXCC".to_string(), "0".to_string());
    vars.insert("SYSDATE".to_string(), "02/23/26".to_string());
    vars.insert("SYSSDATE".to_string(), "26/02/23".to_string());
    vars.insert("SYSJDATE".to_string(), "26.054".to_string());
    vars.insert("SYSTIME".to_string(), "12:00:00".to_string());
    vars.insert("SYSSTIME".to_string(), "12:00".to_string());
    vars.insert("SYSUID".to_string(), "USER01".to_string());
    vars.insert("SYSPREF".to_string(), "USER01".to_string());
    vars.insert("SYSENV".to_string(), "FORE".to_string());
    vars.insert("SYSICMD".to_string(), String::new());
    vars.insert("SYSPCMD".to_string(), String::new());
    vars.insert("SYSSCMD".to_string(), String::new());
    vars.insert("SYSNEST".to_string(), "0".to_string());
    vars.insert("SYSSCAN".to_string(), "1".to_string());
    vars.insert("SYSDLM".to_string(), String::new());
    vars.insert("SYSDVAL".to_string(), String::new());
    vars.insert("SYSISPF".to_string(), "NOT ACTIVE".to_string());
    vars.insert("SYSCPU".to_string(), "0".to_string());
    vars.insert("SYSSRV".to_string(), "0".to_string());
    vars.insert("SYSRACF".to_string(), "AVAILABLE".to_string());
    vars.insert("SYSTSOE".to_string(), "0".to_string());
    vars.insert("SYSLTERM".to_string(), "24".to_string());
    vars.insert("SYSWTERM".to_string(), "80".to_string());
    vars.insert("SYSNODE".to_string(), "LOCAL".to_string());
    vars.insert("SYSNAME".to_string(), "LOCAL".to_string());
    vars.insert("SYSPROC".to_string(), String::new());
    vars.insert("SYSPROMPT".to_string(), "ON".to_string());
    vars.insert("SYSHSM".to_string(), "AVAILABLE".to_string());
    vars.insert("SYSJES".to_string(), "JES2".to_string());
    vars.insert("SYSLRACF".to_string(), "AVAILABLE".to_string());
    vars.insert("SYSAPPCLU".to_string(), String::new());
    vars.insert("SYSMVS".to_string(), "SP7.1".to_string());
    vars.insert("SYSTERMID".to_string(), "T001".to_string());
    vars.insert("SYSJOBID".to_string(), "JOB00001".to_string());
    vars.insert("SYSDSORG".to_string(), String::new());
    vars.insert("SYSRECFM".to_string(), String::new());
    vars.insert("SYSLRECL".to_string(), "0".to_string());
    vars.insert("SYSBLKSIZE".to_string(), "0".to_string());
    vars.insert("SYSUNITS".to_string(), String::new());
    vars.insert("SYSPRIMARY".to_string(), "0".to_string());
    vars.insert("SYSSECONDS".to_string(), "0".to_string());
    vars.insert("SYSVOLUME".to_string(), String::new());
    vars.insert("SYSMEMBERS".to_string(), "0".to_string());
    vars
}

// ─────────────────────── CL-101.5/6: Subprocedure ───────────────────────

/// A subprocedure definition (internal to a CLIST).
#[derive(Debug, Clone)]
pub struct SubprocDef {
    pub label: String,
    pub body: Vec<ClistStatement>,
}

// ─────────────────────── Interpreter ───────────────────────

/// CLIST interpreter.
pub struct ClistInterpreter {
    /// Variable pool.
    pub variables: VariablePool,
    /// System control variables.
    system_vars: HashMap<String, String>,
    /// I/O manager.
    pub io: IoManager,
    /// Control options.
    pub control: ControlOptions,
    /// TSO environment (optional).
    tso: Option<Box<dyn TsoEnvironment>>,
    /// Label positions (label -> statement index).
    labels: HashMap<String, usize>,
    /// Error handler.
    error_handler: Option<Vec<ClistStatement>>,
    /// Attention handler.
    attn_handler: Option<Vec<ClistStatement>>,
    /// Nesting depth (for nested EXEC).
    nest_depth: u32,
    /// Maximum loop iterations (safety).
    max_iterations: u64,
    /// Output buffer (for testing).
    output: Vec<String>,
    /// Last return code.
    last_cc: i32,
    /// Max return code.
    max_cc: i32,
}

impl ClistInterpreter {
    /// Create a new interpreter.
    pub fn new() -> Self {
        Self {
            variables: VariablePool::new(),
            system_vars: get_system_variables(),
            io: IoManager::new(),
            control: ControlOptions::default(),
            tso: None,
            labels: HashMap::new(),
            error_handler: None,
            attn_handler: None,
            nest_depth: 0,
            max_iterations: 100_000,
            output: Vec::new(),
            last_cc: 0,
            max_cc: 0,
        }
    }

    /// Create with shared globals.
    pub fn with_globals(globals: Arc<Mutex<HashMap<String, String>>>) -> Self {
        let mut interp = Self::new();
        interp.variables = VariablePool::with_globals(globals);
        interp
    }

    /// Set TSO environment.
    pub fn set_tso(&mut self, tso: Box<dyn TsoEnvironment>) {
        self.tso = Some(tso);
    }

    /// Get output lines.
    pub fn output(&self) -> &[String] {
        &self.output
    }

    /// Get last return code.
    pub fn last_cc(&self) -> i32 {
        self.last_cc
    }

    /// Provide simulated input for READ.
    pub fn provide_input(&mut self, input: &str) {
        self.io.push_input(input);
    }

    /// Execute a CLIST source.
    pub fn execute(&mut self, source: &str) -> ExecResult<i32> {
        let ast = parse_clist(source)?;
        self.execute_ast(&ast)
    }

    /// Execute a parsed AST.
    pub fn execute_ast(&mut self, ast: &ClistAst) -> ExecResult<i32> {
        // Build label map
        self.labels.clear();
        for (i, stmt) in ast.statements.iter().enumerate() {
            if let ClistStatement::Label(name) = stmt {
                self.labels.insert(name.clone(), i);
            }
        }

        self.execute_top_level(&ast.statements)
    }

    /// Top-level executor that handles GOTO by label lookup.
    fn execute_top_level(&mut self, stmts: &[ClistStatement]) -> ExecResult<i32> {
        let mut pc = 0;
        let mut iterations: u64 = 0;

        while pc < stmts.len() {
            iterations += 1;
            if iterations > self.max_iterations {
                return Err(InterpreterError::MaxIterations);
            }

            let result = self.execute_statement(&stmts[pc]);
            match result {
                Ok(StmtResult::Continue) => {
                    pc += 1;
                }
                Ok(StmtResult::Goto(label)) => {
                    if let Some(&target) = self.labels.get(&label) {
                        pc = target + 1; // Skip the label itself
                    } else {
                        return Err(InterpreterError::LabelNotFound(label));
                    }
                }
                Ok(StmtResult::Exit(code)) => {
                    return Ok(code);
                }
                Ok(StmtResult::Return(code)) => {
                    return Ok(code);
                }
                Err(InterpreterError::Exit(code)) => {
                    return Ok(code);
                }
                Err(e) => {
                    // Check error handler
                    self.last_cc = 12;
                    self.update_cc(12);
                    if let Some(handler) = self.error_handler.clone() {
                        self.execute_block(&handler)?;
                        pc += 1;
                    } else {
                        return Err(e);
                    }
                }
            }
        }

        Ok(self.last_cc)
    }

    /// Execute a block of statements (IF body, DO body, etc.)
    /// Propagates Goto/Exit/Return upward.
    fn execute_block(&mut self, stmts: &[ClistStatement]) -> ExecResult<StmtResult> {
        for stmt in stmts {
            let result = self.execute_statement(stmt)?;
            match result {
                StmtResult::Continue => continue,
                other => return Ok(other),
            }
        }
        Ok(StmtResult::Continue)
    }

    fn execute_statement(&mut self, stmt: &ClistStatement) -> ExecResult<StmtResult> {
        match stmt {
            ClistStatement::Label(_) => Ok(StmtResult::Continue),

            ClistStatement::Set { variable, value } => {
                let val = self.eval_expression(value)?;
                self.variables.set(variable, val);
                Ok(StmtResult::Continue)
            }

            ClistStatement::If { condition, then_body, else_body } => {
                let cond = self.eval_condition(condition)?;
                let body = if cond { then_body } else { else_body };
                if !body.is_empty() {
                    return self.execute_block(body);
                }
                Ok(StmtResult::Continue)
            }

            ClistStatement::Do { condition, body } => {
                self.execute_do(condition, body)
            }

            ClistStatement::Select { cases, otherwise } => {
                for (expr, body) in cases {
                    if self.eval_condition(expr)? {
                        return self.execute_block(body);
                    }
                }
                if !otherwise.is_empty() {
                    return self.execute_block(otherwise);
                }
                Ok(StmtResult::Continue)
            }

            ClistStatement::Goto(label) => {
                Ok(StmtResult::Goto(label.clone()))
            }

            ClistStatement::Exit(code) => {
                let exit_code = if let Some(expr) = code {
                    self.eval_expression(expr)?
                        .parse::<i32>()
                        .unwrap_or(0)
                } else {
                    0
                };
                Ok(StmtResult::Exit(exit_code))
            }

            ClistStatement::Return(code) => {
                let rc = if let Some(expr) = code {
                    self.eval_expression(expr)?
                        .parse::<i32>()
                        .unwrap_or(0)
                } else {
                    0
                };
                Ok(StmtResult::Return(rc))
            }

            ClistStatement::Write(expr) => {
                let text = self.eval_expression(expr)?;
                self.output.push(text);
                Ok(StmtResult::Continue)
            }

            ClistStatement::WriteNr(expr) => {
                let text = self.eval_expression(expr)?;
                self.output.push(text);
                Ok(StmtResult::Continue)
            }

            ClistStatement::Read(var) => {
                let input = self.io.read_line()?;
                self.variables.set(var, input);
                Ok(StmtResult::Continue)
            }

            ClistStatement::ReadDval(vars) => {
                let input = self.io.read_line()?;
                let parts: Vec<&str> = input.split_whitespace().collect();
                for (i, var) in vars.iter().enumerate() {
                    let val = parts.get(i).unwrap_or(&"").to_string();
                    self.variables.set(var, val);
                }
                Ok(StmtResult::Continue)
            }

            ClistStatement::Termin => {
                let _input = self.io.read_line()?;
                Ok(StmtResult::Continue)
            }

            ClistStatement::OpenFile { variable, mode } => {
                let filename = self.resolve_variable(variable);
                self.io.open_file(&filename, mode)?;
                Ok(StmtResult::Continue)
            }

            ClistStatement::GetFile(var) => {
                let filename = self.resolve_variable(var);
                let line = self.io.get_file(&filename)?;
                self.variables.set(var, line);
                Ok(StmtResult::Continue)
            }

            ClistStatement::PutFile(var) => {
                let filename = self.resolve_variable(var);
                let data = self.resolve_variable(var);
                self.io.put_file(&filename, &data)?;
                Ok(StmtResult::Continue)
            }

            ClistStatement::CloseFile(var) => {
                let filename = self.resolve_variable(var);
                self.io.close_file(&filename)?;
                Ok(StmtResult::Continue)
            }

            ClistStatement::Error(action) => {
                match action {
                    ErrorAction::Off => { self.error_handler = None; }
                    ErrorAction::Body(body) => { self.error_handler = Some(body.clone()); }
                    ErrorAction::Return => { self.error_handler = Some(vec![ClistStatement::Return(None)]); }
                }
                Ok(StmtResult::Continue)
            }

            ClistStatement::Attn(action) => {
                match action {
                    ErrorAction::Off => { self.attn_handler = None; }
                    ErrorAction::Body(body) => { self.attn_handler = Some(body.clone()); }
                    ErrorAction::Return => { self.attn_handler = Some(vec![ClistStatement::Return(None)]); }
                }
                Ok(StmtResult::Continue)
            }

            ClistStatement::Control(options) => {
                for opt in options {
                    match opt.as_str() {
                        "LIST" => self.control.list = true,
                        "NOLIST" => self.control.list = false,
                        "CONLIST" => self.control.conlist = true,
                        "NOCONLIST" => self.control.conlist = false,
                        "SYMLIST" => self.control.symlist = true,
                        "NOSYMLIST" => self.control.symlist = false,
                        "MSG" => self.control.msg = true,
                        "NOMSG" => self.control.msg = false,
                        "PROMPT" => self.control.prompt = true,
                        "NOPROMPT" => self.control.prompt = false,
                        "ASIS" => self.control.asis = true,
                        "CAPS" => self.control.asis = false,
                        "MAIN" => self.control.main = true,
                        "NOFLUSH" => self.control.flush = false,
                        "FLUSH" => self.control.flush = true,
                        "NOEND" => self.control.noend = true,
                        "END" => self.control.noend = false,
                        _ => {}
                    }
                }
                Ok(StmtResult::Continue)
            }

            ClistStatement::Proc { positional: _, keywords: _ } => {
                // PROC is handled at invocation time
                Ok(StmtResult::Continue)
            }

            ClistStatement::SysCall { label, args } => {
                // Find the label and execute the subprocedure
                let arg_values: Vec<String> = args
                    .iter()
                    .map(|a| self.eval_expression(a))
                    .collect::<ExecResult<Vec<_>>>()?;

                // Set SYSREASON variables
                for (i, val) in arg_values.iter().enumerate() {
                    self.variables.set(&format!("SYSREASON{}", i + 1), val.clone());
                }

                if self.labels.contains_key(label) {
                    // Execute from label to RETURN
                    self.output.push(format!("SYSCALL {label}"));
                }
                Ok(StmtResult::Continue)
            }

            ClistStatement::Global(vars) => {
                self.variables.declare_global(vars);
                Ok(StmtResult::Continue)
            }

            ClistStatement::NLocal(vars) => {
                self.variables.declare_nglobal(vars);
                Ok(StmtResult::Continue)
            }

            ClistStatement::Exec { name, args: _ } => {
                // Nested CLIST execution
                self.nest_depth += 1;
                self.system_vars.insert("SYSNEST".to_string(), self.nest_depth.to_string());
                // In a real implementation, we'd load and execute the named CLIST
                self.output.push(format!("EXEC {name}"));
                self.nest_depth -= 1;
                self.system_vars.insert("SYSNEST".to_string(), self.nest_depth.to_string());
                Ok(StmtResult::Continue)
            }

            ClistStatement::Listdsi(expr) => {
                let dsname = self.eval_expression(expr)?;
                if let Some(ref tso) = self.tso {
                    let result = tso.listdsi(&dsname);
                    if let Some(attrs) = result {
                        self.system_vars.insert("SYSDSORG".to_string(), attrs.dsorg.clone());
                        self.system_vars.insert("SYSRECFM".to_string(), attrs.recfm.clone());
                        self.system_vars.insert("SYSLRECL".to_string(), attrs.lrecl.to_string());
                        self.system_vars.insert("SYSBLKSIZE".to_string(), attrs.blksize.to_string());
                        self.system_vars.insert("SYSVOLUME".to_string(), attrs.volume.clone());
                        self.system_vars.insert("SYSPRIMARY".to_string(), attrs.primary.to_string());
                        self.system_vars.insert("SYSSECONDS".to_string(), attrs.secondary.to_string());
                        self.system_vars.insert("SYSUNITS".to_string(), attrs.units.clone());
                        self.system_vars.insert("SYSMEMBERS".to_string(), attrs.members.to_string());
                        self.last_cc = 0;
                    } else {
                        self.last_cc = 16;
                    }
                } else {
                    self.last_cc = 16;
                }
                self.update_cc(self.last_cc);
                Ok(StmtResult::Continue)
            }

            ClistStatement::IspExec(cmd) => {
                let resolved = self.substitute_variables(cmd);
                if let Some(ref tso) = self.tso {
                    let rc = tso.ispexec(&resolved);
                    self.last_cc = rc;
                    self.update_cc(rc);
                }
                Ok(StmtResult::Continue)
            }

            ClistStatement::IsrEdit(cmd) => {
                let resolved = self.substitute_variables(cmd);
                if let Some(ref tso) = self.tso {
                    let rc = tso.isredit(&resolved);
                    self.last_cc = rc;
                    self.update_cc(rc);
                }
                Ok(StmtResult::Continue)
            }

            ClistStatement::TsoCommand(cmd) => {
                let resolved = self.substitute_variables(cmd);
                if let Some(ref tso) = self.tso {
                    let rc = tso.execute_command(&resolved);
                    self.last_cc = rc;
                    self.update_cc(rc);
                }
                Ok(StmtResult::Continue)
            }

            ClistStatement::SysRef(_vars) => {
                // SYSREF declares that variables are passed by reference
                // in a subprocedure. This is a declaration-only statement;
                // actual reference semantics are handled by SYSCALL.
                Ok(StmtResult::Continue)
            }

            ClistStatement::Data(_) => {
                Ok(StmtResult::Continue)
            }
        }
    }

    fn execute_do(
        &mut self,
        condition: &Option<(DoCondition, Expression)>,
        body: &[ClistStatement],
    ) -> ExecResult<StmtResult> {
        let mut iterations: u64 = 0;

        match condition {
            None => {
                // Simple DO ... END (execute once)
                let result = self.execute_block(body)?;
                if !matches!(result, StmtResult::Continue) {
                    return Ok(result);
                }
            }
            Some((DoCondition::While, expr)) => {
                while self.eval_condition(expr)? {
                    iterations += 1;
                    if iterations > self.max_iterations {
                        return Err(InterpreterError::MaxIterations);
                    }
                    let result = self.execute_block(body)?;
                    if !matches!(result, StmtResult::Continue) {
                        return Ok(result);
                    }
                }
            }
            Some((DoCondition::Until, expr)) => {
                loop {
                    iterations += 1;
                    if iterations > self.max_iterations {
                        return Err(InterpreterError::MaxIterations);
                    }
                    let result = self.execute_block(body)?;
                    if !matches!(result, StmtResult::Continue) {
                        return Ok(result);
                    }
                    if self.eval_condition(expr)? {
                        break;
                    }
                }
            }
        }

        Ok(StmtResult::Continue)
    }

    // ─── Expression Evaluation ───

    /// Evaluate an expression to a string value.
    pub fn eval_expression(&self, expr: &Expression) -> ExecResult<String> {
        match expr {
            Expression::Number(n) => Ok(n.to_string()),
            Expression::StringLit(s) => Ok(self.substitute_variables(s)),
            Expression::Variable(name) => {
                self.resolve_variable_result(name)
            }
            Expression::BinaryOp { left, op, right } => {
                let l = self.eval_expression(left)?;
                let r = self.eval_expression(right)?;
                self.eval_binary_op(&l, op, &r)
            }
            Expression::UnaryOp { op: ExprOp::Not, operand } => {
                let val = self.eval_expression(operand)?;
                let b = val != "0" && !val.is_empty();
                Ok(if b { "0" } else { "1" }.to_string())
            }
            Expression::UnaryOp { op: _, operand } => {
                self.eval_expression(operand)
            }
            Expression::FunctionCall { name, args } => {
                let evaluated_args: Vec<String> = args
                    .iter()
                    .map(|a| self.eval_expression(a))
                    .collect::<ExecResult<Vec<_>>>()?;
                evaluate_builtin(name, &evaluated_args)
                    .map_err(|e| InterpreterError::General(e.to_string()))
            }
        }
    }

    /// Evaluate a condition expression to bool.
    fn eval_condition(&self, expr: &Expression) -> ExecResult<bool> {
        let val = self.eval_expression(expr)?;
        // "1", "TRUE", non-zero => true
        Ok(match val.as_str() {
            "0" | "" | "FALSE" => false,
            _ => {
                if let Ok(n) = val.parse::<i64>() {
                    n != 0
                } else {
                    true // Non-empty string is truthy
                }
            }
        })
    }

    fn eval_binary_op(&self, left: &str, op: &ExprOp, right: &str) -> ExecResult<String> {
        match op {
            ExprOp::Add | ExprOp::Subtract | ExprOp::Multiply | ExprOp::Divide | ExprOp::Modulo => {
                let l = left.trim().parse::<i64>().unwrap_or(0);
                let r = right.trim().parse::<i64>().unwrap_or(0);
                let result = match op {
                    ExprOp::Add => l + r,
                    ExprOp::Subtract => l - r,
                    ExprOp::Multiply => l * r,
                    ExprOp::Divide => {
                        if r == 0 {
                            return Err(InterpreterError::DivisionByZero);
                        }
                        l / r
                    }
                    ExprOp::Modulo => {
                        if r == 0 {
                            return Err(InterpreterError::DivisionByZero);
                        }
                        l % r
                    }
                    _ => unreachable!(),
                };
                Ok(result.to_string())
            }
            ExprOp::Equal => Ok(if left == right { "1" } else { "0" }.to_string()),
            ExprOp::NotEqual => Ok(if left != right { "1" } else { "0" }.to_string()),
            ExprOp::LessThan => {
                let cmp = numeric_or_string_cmp(left, right);
                Ok(if cmp < 0 { "1" } else { "0" }.to_string())
            }
            ExprOp::GreaterThan => {
                let cmp = numeric_or_string_cmp(left, right);
                Ok(if cmp > 0 { "1" } else { "0" }.to_string())
            }
            ExprOp::LessOrEqual => {
                let cmp = numeric_or_string_cmp(left, right);
                Ok(if cmp <= 0 { "1" } else { "0" }.to_string())
            }
            ExprOp::GreaterOrEqual => {
                let cmp = numeric_or_string_cmp(left, right);
                Ok(if cmp >= 0 { "1" } else { "0" }.to_string())
            }
            ExprOp::And => {
                let l = left != "0" && !left.is_empty();
                let r = right != "0" && !right.is_empty();
                Ok(if l && r { "1" } else { "0" }.to_string())
            }
            ExprOp::Or => {
                let l = left != "0" && !left.is_empty();
                let r = right != "0" && !right.is_empty();
                Ok(if l || r { "1" } else { "0" }.to_string())
            }
            ExprOp::Concat => Ok(format!("{left}{right}")),
            ExprOp::Not => Ok(if left == "0" || left.is_empty() { "1" } else { "0" }.to_string()),
        }
    }

    fn resolve_variable(&self, name: &str) -> String {
        self.resolve_variable_result(name).unwrap_or_default()
    }

    fn resolve_variable_result(&self, name: &str) -> ExecResult<String> {
        let upper = name.to_uppercase();
        // Check user variables first
        if let Some(val) = self.variables.get(&upper) {
            return Ok(val);
        }
        // Check system variables
        if let Some(val) = self.system_vars.get(&upper) {
            return Ok(val.clone());
        }
        // Return empty string for undefined (CLIST convention)
        Ok(String::new())
    }

    fn substitute_variables(&self, text: &str) -> String {
        let mut result = String::new();
        let chars: Vec<char> = text.chars().collect();
        let mut i = 0;

        while i < chars.len() {
            if chars[i] == '&' {
                i += 1;
                let mut name = String::new();
                while i < chars.len() && (chars[i].is_alphanumeric() || chars[i] == '_') {
                    name.push(chars[i]);
                    i += 1;
                }
                if !name.is_empty() {
                    let val = self.resolve_variable(&name);
                    result.push_str(&val);
                } else {
                    result.push('&');
                }
            } else {
                result.push(chars[i]);
                i += 1;
            }
        }

        result
    }

    fn update_cc(&mut self, cc: i32) {
        self.last_cc = cc;
        if cc > self.max_cc {
            self.max_cc = cc;
        }
        self.system_vars.insert("LASTCC".to_string(), cc.to_string());
        self.system_vars.insert("MAXCC".to_string(), self.max_cc.to_string());
    }
}

impl Default for ClistInterpreter {
    fn default() -> Self {
        Self::new()
    }
}

/// Internal statement execution result.
enum StmtResult {
    Continue,
    Goto(String),
    Exit(i32),
    Return(i32),
}

fn numeric_or_string_cmp(left: &str, right: &str) -> i32 {
    if let (Ok(l), Ok(r)) = (left.parse::<i64>(), right.parse::<i64>()) {
        l.cmp(&r) as i32
    } else {
        left.cmp(right) as i32
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ─── CL-101.1: Variable Pool ───

    #[test]
    fn test_variable_set_get() {
        let mut pool = VariablePool::new();
        pool.set("VAR1", "Hello".to_string());
        assert_eq!(pool.get("VAR1"), Some("Hello".to_string()));
    }

    #[test]
    fn test_variable_case_insensitive() {
        let mut pool = VariablePool::new();
        pool.set("myvar", "test".to_string());
        assert_eq!(pool.get("MYVAR"), Some("test".to_string()));
    }

    #[test]
    fn test_global_variables() {
        let globals = Arc::new(Mutex::new(HashMap::new()));
        let mut pool1 = VariablePool::with_globals(globals.clone());
        let mut pool2 = VariablePool::with_globals(globals);

        pool1.declare_global(&["SHARED".to_string()]);
        pool1.set("SHARED", "value1".to_string());

        pool2.declare_global(&["SHARED".to_string()]);
        assert_eq!(pool2.get("SHARED"), Some("value1".to_string()));
    }

    // ─── CL-101.2: System Variables ───

    #[test]
    fn test_system_variables() {
        let interp = ClistInterpreter::new();
        let val = interp.resolve_variable("SYSUID");
        assert!(!val.is_empty());
    }

    // ─── CL-101.3: SET Statement ───

    #[test]
    fn test_set_numeric() {
        let mut interp = ClistInterpreter::new();
        interp.execute("SET &A = 42").unwrap();
        assert_eq!(interp.variables.get("A"), Some("42".to_string()));
    }

    #[test]
    fn test_set_string() {
        let mut interp = ClistInterpreter::new();
        interp.execute("SET &NAME = 'John'").unwrap();
        assert_eq!(interp.variables.get("NAME"), Some("John".to_string()));
    }

    // ─── CL-101.4: Control Flow ───

    #[test]
    fn test_if_true() {
        let mut interp = ClistInterpreter::new();
        interp.execute("SET &A = 1\nIF &A EQ 1 THEN WRITE 'YES'").unwrap();
        assert!(interp.output().contains(&"YES".to_string()));
    }

    #[test]
    fn test_if_false() {
        let mut interp = ClistInterpreter::new();
        interp.execute("SET &A = 2\nIF &A EQ 1 THEN WRITE 'YES'").unwrap();
        assert!(!interp.output().contains(&"YES".to_string()));
    }

    #[test]
    fn test_do_while() {
        let mut interp = ClistInterpreter::new();
        let source = r#"
SET &I = 0
DO WHILE &I LT 3
  SET &I = &I + 1
END
"#;
        interp.execute(source).unwrap();
        assert_eq!(interp.variables.get("I"), Some("3".to_string()));
    }

    #[test]
    fn test_do_until() {
        let mut interp = ClistInterpreter::new();
        let source = r#"
SET &I = 0
DO UNTIL &I EQ 5
  SET &I = &I + 1
END
"#;
        interp.execute(source).unwrap();
        assert_eq!(interp.variables.get("I"), Some("5".to_string()));
    }

    #[test]
    fn test_goto() {
        let mut interp = ClistInterpreter::new();
        let source = r#"
SET &X = 0
GOTO SKIP
SET &X = 99
SKIP: SET &X = 1
"#;
        interp.execute(source).unwrap();
        assert_eq!(interp.variables.get("X"), Some("1".to_string()));
    }

    // ─── CL-101.5: EXIT ───

    #[test]
    fn test_exit_code() {
        let mut interp = ClistInterpreter::new();
        let rc = interp.execute("EXIT 4").unwrap();
        assert_eq!(rc, 4);
    }

    #[test]
    fn test_exit_default() {
        let mut interp = ClistInterpreter::new();
        let rc = interp.execute("EXIT").unwrap();
        assert_eq!(rc, 0);
    }

    // ─── CL-101.6: Subprocedures / Nesting ───

    #[test]
    fn test_global_declaration() {
        let mut interp = ClistInterpreter::new();
        interp.execute("GLOBAL &SHARED\nSET &SHARED = 'test'").unwrap();
        // The variable should be in the global pool
        let globals = interp.variables.globals_ref();
        let g = globals.lock().unwrap();
        assert_eq!(g.get("SHARED"), Some(&"test".to_string()));
    }

    // ─── CL-101: Integration ───

    #[test]
    fn test_arithmetic_loop() {
        let mut interp = ClistInterpreter::new();
        let source = r#"
SET &SUM = 0
SET &I = 1
LOOP: IF &I GT 5 THEN GOTO DONE
  SET &SUM = &SUM + &I
  SET &I = &I + 1
  GOTO LOOP
DONE: WRITE &SUM
EXIT 0
"#;
        let rc = interp.execute(source).unwrap();
        assert_eq!(rc, 0);
        assert_eq!(interp.variables.get("SUM"), Some("15".to_string()));
    }

    #[test]
    fn test_select_when() {
        let mut interp = ClistInterpreter::new();
        let source = r#"
SET &X = 2
SELECT
WHEN &X EQ 1 THEN WRITE 'ONE'
WHEN &X EQ 2 THEN WRITE 'TWO'
OTHERWISE WRITE 'OTHER'
END
"#;
        interp.execute(source).unwrap();
        assert!(interp.output().contains(&"TWO".to_string()));
    }

    #[test]
    fn test_control_statement() {
        let mut interp = ClistInterpreter::new();
        interp.execute("CONTROL NOLIST NOMSG").unwrap();
        assert!(!interp.control.list);
        assert!(!interp.control.msg);
    }

    #[test]
    fn test_write_output() {
        let mut interp = ClistInterpreter::new();
        interp.execute("WRITE 'Hello World'").unwrap();
        assert_eq!(interp.output(), &["Hello World"]);
    }
}
