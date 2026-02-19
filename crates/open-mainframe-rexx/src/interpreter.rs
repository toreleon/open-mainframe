//! REXX interpreter — executes a parsed REXX AST.
//!
//! The interpreter maintains variable pools, evaluates expressions with
//! configurable-precision decimal arithmetic, and implements all REXX
//! control flow including DO loops, IF/THEN/ELSE, SELECT/WHEN/OTHERWISE,
//! and CALL/RETURN with PROCEDURE EXPOSE scoping.

use std::collections::{HashMap, HashSet, VecDeque};

use crate::ast::{
    BinOp, Clause, ClauseBody, DoControl, Expr, ParseSource, Program, SignalTarget, UnaryOp,
};
use crate::builtins;
use crate::parse_template::{execute_parse, parse_template};
use crate::value::{
    rexx_add, rexx_compare, rexx_div, rexx_idiv, rexx_mul, rexx_pow, rexx_rem, rexx_sub,
    NumericForm, NumericSettings, RexxValue,
};

// ---------------------------------------------------------------------------
//  Public API
// ---------------------------------------------------------------------------

/// Run a parsed REXX program and return its exit code and captured output.
pub fn interpret(program: &Program) -> Result<ExecResult, InterpError> {
    let mut interp = Interpreter::new();
    interp.run(program)?;
    Ok(ExecResult {
        rc: interp.rc,
        output: interp.output,
    })
}

/// Result of executing a REXX program.
#[derive(Debug, Clone)]
pub struct ExecResult {
    /// Return code (set by EXIT or RETURN).
    pub rc: i32,
    /// Lines produced by SAY.
    pub output: Vec<String>,
}

/// Interpreter error.
#[derive(Debug, Clone, thiserror::Error)]
pub enum InterpError {
    #[error("REXX error at line {line}: {message}")]
    Runtime { line: u32, message: String },
    #[error("REXX error: {0}")]
    General(String),
}

// ---------------------------------------------------------------------------
//  Control flow signals (not user-visible errors)
// ---------------------------------------------------------------------------

/// Internal control flow — not exposed to callers.
#[derive(Debug)]
enum Flow {
    /// Normal execution — continue to next clause.
    Normal,
    /// RETURN [value] from a subroutine.
    Return(Option<String>),
    /// EXIT [value] from the program.
    Exit(Option<String>),
    /// ITERATE [name] in a DO loop.
    Iterate(Option<String>),
    /// LEAVE [name] from a DO loop.
    Leave(Option<String>),
    /// SIGNAL to a label.
    Signal(String),
}

// ---------------------------------------------------------------------------
//  Variable pool
// ---------------------------------------------------------------------------

/// A variable pool — simple variables and stems.
#[derive(Debug, Clone, Default)]
struct VarPool {
    /// Simple variables: name → value (names are always uppercased).
    simple: HashMap<String, String>,
    /// Stem defaults: `STEM.` → default value.
    stem_defaults: HashMap<String, String>,
    /// Compound variables: `STEM.tail` → value.
    compounds: HashMap<String, String>,
    /// Variables exposed from parent scope (for PROCEDURE EXPOSE).
    exposed: HashSet<String>,
}

impl VarPool {
    fn get(&self, name: &str) -> String {
        let upper = name.to_uppercase();
        if let Some(v) = self.simple.get(&upper) {
            return v.clone();
        }
        // Check compound: STEM.TAIL
        if let Some(dot) = upper.find('.') {
            let stem = &upper[..=dot]; // includes the dot
            let tail = &upper[dot + 1..];
            let key = format!("{stem}{tail}");
            if let Some(v) = self.compounds.get(&key) {
                return v.clone();
            }
            // Check stem default.
            if let Some(def) = self.stem_defaults.get(stem) {
                return def.clone();
            }
        }
        // Uninitialized: return its own name uppercased.
        upper
    }

    fn set(&mut self, name: &str, value: String) {
        let upper = name.to_uppercase();
        if let Some(dot) = upper.find('.') {
            if dot == upper.len() - 1 {
                // Setting stem default: `stem. = value`
                self.stem_defaults.insert(upper, value);
            } else {
                // Setting compound variable: `stem.tail = value`
                self.compounds.insert(upper, value);
            }
        } else {
            self.simple.insert(upper, value);
        }
    }

    fn drop_var(&mut self, name: &str) {
        let upper = name.to_uppercase();
        self.simple.remove(&upper);
        self.compounds.remove(&upper);
        // If it's a stem, drop all compounds with that stem.
        if upper.ends_with('.') {
            self.stem_defaults.remove(&upper);
            let prefix = &upper;
            self.compounds.retain(|k, _| !k.starts_with(prefix.as_str()));
        }
    }

    /// Check the SYMBOL state of a variable.
    fn symbol_state(&self, name: &str) -> &'static str {
        let upper = name.to_uppercase();
        if self.simple.contains_key(&upper) {
            return "VAR";
        }
        if let Some(dot) = upper.find('.') {
            let stem = &upper[..=dot];
            let tail = &upper[dot + 1..];
            let key = format!("{stem}{tail}");
            if self.compounds.contains_key(&key) || self.stem_defaults.contains_key(stem) {
                return "VAR";
            }
        }
        "LIT"
    }
}

// ---------------------------------------------------------------------------
//  Interpreter state
// ---------------------------------------------------------------------------

/// Label table: label name → index into clause list.
type LabelTable = HashMap<String, usize>;

// ---------------------------------------------------------------------------
//  Data stack
// ---------------------------------------------------------------------------

/// REXX data stack — supports LIFO (PUSH), FIFO (QUEUE), and buffer management.
#[derive(Debug, Clone, Default)]
struct DataStack {
    /// The stack elements (front = top).
    items: VecDeque<String>,
    /// Buffer boundaries — each entry is the stack depth when MAKEBUF was called.
    buffers: Vec<usize>,
}

impl DataStack {
    /// PUSH — add item to top (LIFO).
    fn push(&mut self, value: String) {
        self.items.push_front(value);
    }

    /// QUEUE — add item to bottom (FIFO).
    fn queue(&mut self, value: String) {
        self.items.push_back(value);
    }

    /// PULL — remove and return from top. Returns empty string if empty.
    fn pull(&mut self) -> String {
        self.items.pop_front().unwrap_or_default()
    }

    /// QUEUED — number of items on the stack.
    fn queued(&self) -> usize {
        self.items.len()
    }

    /// MAKEBUF — create a new buffer boundary, returns the buffer number.
    fn makebuf(&mut self) -> usize {
        self.buffers.push(self.items.len());
        self.buffers.len()
    }

    /// DROPBUF — drop the most recent buffer and all items added since it was created.
    /// If `n` is 0, drop all buffers. Otherwise drop buffer `n`.
    fn dropbuf(&mut self, n: usize) {
        if n == 0 {
            // Drop all buffers and their contents.
            if let Some(&first_boundary) = self.buffers.first() {
                while self.items.len() > first_boundary {
                    self.items.pop_front();
                }
            }
            self.buffers.clear();
        } else if !self.buffers.is_empty() {
            let boundary = self.buffers.pop().unwrap();
            // Remove items added since this buffer was created.
            while self.items.len() > boundary {
                self.items.pop_front();
            }
        }
    }

    /// NEWSTACK — save the current stack and start a fresh one.
    /// Returns the saved stack.
    fn newstack(&mut self) -> DataStack {
        std::mem::take(self)
    }
}

// ---------------------------------------------------------------------------
//  DD file simulation for EXECIO
// ---------------------------------------------------------------------------

/// A simulated DD allocation for EXECIO.
#[derive(Debug, Clone, Default)]
struct DdAllocation {
    /// Records in the DD.
    records: Vec<String>,
    /// Current read position.
    read_pos: usize,
}

struct Interpreter {
    /// Stack of variable pools (for PROCEDURE scoping).
    var_stack: Vec<VarPool>,
    /// Numeric settings.
    numeric: NumericSettings,
    /// RESULT special variable (set after CALL).
    result: Option<String>,
    /// RC special variable.
    rc: i32,
    /// Captured SAY output.
    output: Vec<String>,
    /// Current line (for error reporting).
    current_line: u32,
    /// REXX data stack.
    data_stack: DataStack,
    /// Saved stacks from NEWSTACK.
    saved_stacks: Vec<DataStack>,
    /// Simulated DD allocations for EXECIO.
    dd_files: HashMap<String, DdAllocation>,
    /// Current ADDRESS environment (default: TSO).
    address_env: String,
    /// Previous ADDRESS environment (for ADDRESS VALUE toggling).
    prev_address_env: String,
}

impl Interpreter {
    fn new() -> Self {
        Self {
            var_stack: vec![VarPool::default()],
            numeric: NumericSettings::default(),
            result: None,
            rc: 0,
            output: Vec::new(),
            current_line: 0,
            data_stack: DataStack::default(),
            saved_stacks: Vec::new(),
            dd_files: HashMap::new(),
            address_env: "TSO".to_string(),
            prev_address_env: "TSO".to_string(),
        }
    }

    /// Get the current (top) variable pool.
    fn vars(&self) -> &VarPool {
        self.var_stack.last().expect("empty var stack")
    }

    /// Get the current (top) variable pool mutably.
    fn vars_mut(&mut self) -> &mut VarPool {
        self.var_stack.last_mut().expect("empty var stack")
    }

    // -----------------------------------------------------------------------
    //  Top-level execution
    // -----------------------------------------------------------------------

    fn run(&mut self, program: &Program) -> Result<(), InterpError> {
        // Build label table from all clauses (including inside DO/IF etc. is
        // handled separately, but top-level labels need to be found first).
        let labels = build_label_table(&program.clauses);
        match self.exec_clauses_toplevel(&program.clauses, &labels)? {
            Flow::Normal | Flow::Return(_) => {}
            Flow::Exit(Some(v)) => {
                self.rc = v.parse::<i32>().unwrap_or(0);
            }
            _ => {}
        }
        Ok(())
    }

    /// Execute top-level clauses (with SIGNAL support).
    fn exec_clauses_toplevel(&mut self, clauses: &[Clause], labels: &LabelTable) -> Result<Flow, InterpError> {
        let mut i = 0;
        while i < clauses.len() {
            self.current_line = clauses[i].line;
            match self.exec_clause(&clauses[i], clauses, labels)? {
                Flow::Normal => {}
                Flow::Signal(label) => {
                    let target = label.to_uppercase();
                    if let Some(&idx) = labels.get(&target) {
                        i = idx + 1;
                        continue;
                    } else {
                        return Err(self.error(&format!("Label not found: {target}")));
                    }
                }
                flow => return Ok(flow),
            }
            i += 1;
        }
        Ok(Flow::Normal)
    }

    fn exec_clauses(&mut self, clauses: &[Clause], labels: &LabelTable) -> Result<Flow, InterpError> {
        for clause in clauses {
            self.current_line = clause.line;
            match self.exec_clause(clause, clauses, labels)? {
                Flow::Normal => {}
                flow => return Ok(flow),
            }
        }
        Ok(Flow::Normal)
    }

    fn exec_clause(
        &mut self,
        clause: &Clause,
        all_clauses: &[Clause],
        labels: &LabelTable,
    ) -> Result<Flow, InterpError> {
        match &clause.body {
            ClauseBody::Label(_) => Ok(Flow::Normal), // Labels are no-ops during execution.
            ClauseBody::Assignment { var, expr } => {
                let val = self.eval_expr(expr)?;
                // Resolve compound tail variables.
                let resolved = self.resolve_var_name(var);
                self.vars_mut().set(&resolved, val);
                Ok(Flow::Normal)
            }
            ClauseBody::Say(expr) => {
                let val = self.eval_expr(expr)?;
                self.output.push(val);
                Ok(Flow::Normal)
            }
            ClauseBody::If {
                cond,
                then_clause,
                else_clause,
            } => {
                let val = self.eval_expr(cond)?;
                let b = self.to_bool(&val)?;
                if b {
                    self.exec_clause(then_clause, all_clauses, labels)
                } else if let Some(ec) = else_clause {
                    self.exec_clause(ec, all_clauses, labels)
                } else {
                    Ok(Flow::Normal)
                }
            }
            ClauseBody::Do { control, body } => self.exec_do(control, body, labels),
            ClauseBody::Select { whens, otherwise } => {
                for (cond, body) in whens {
                    let val = self.eval_expr(cond)?;
                    let b = self.to_bool(&val)?;
                    if b {
                        let sub_labels = build_label_table(body);
                        return self.exec_clauses(body, &sub_labels);
                    }
                }
                if let Some(ow) = otherwise {
                    let sub_labels = build_label_table(ow);
                    return self.exec_clauses(ow, &sub_labels);
                }
                Err(self.error("No WHEN matched and no OTHERWISE in SELECT"))
            }
            ClauseBody::Call { name, args } => {
                self.exec_call(name, args, all_clauses, labels)
            }
            ClauseBody::Return(expr) => {
                let val = if let Some(e) = expr {
                    Some(self.eval_expr(e)?)
                } else {
                    None
                };
                Ok(Flow::Return(val))
            }
            ClauseBody::Exit(expr) => {
                let val = if let Some(e) = expr {
                    Some(self.eval_expr(e)?)
                } else {
                    None
                };
                Ok(Flow::Exit(val))
            }
            ClauseBody::Drop(vars) => {
                for v in vars {
                    self.vars_mut().drop_var(v);
                }
                Ok(Flow::Normal)
            }
            ClauseBody::Signal(target) => match target {
                SignalTarget::Label(lbl) => Ok(Flow::Signal(lbl.clone())),
                SignalTarget::On { .. } | SignalTarget::Off(_) => {
                    // Condition traps are stub — just record, don't execute.
                    Ok(Flow::Normal)
                }
            },
            ClauseBody::Iterate(name) => Ok(Flow::Iterate(name.clone())),
            ClauseBody::Leave(name) => Ok(Flow::Leave(name.clone())),
            ClauseBody::Nop => Ok(Flow::Normal),
            ClauseBody::Trace(_) => Ok(Flow::Normal), // Stub.
            ClauseBody::Address { environment, command } => {
                match (environment, command) {
                    (Some(env), Some(cmd_expr)) => {
                        // ADDRESS env command — execute command in specified environment.
                        let cmd = self.eval_expr(cmd_expr)?;
                        let env_upper = env.to_uppercase();
                        self.exec_in_env(&env_upper, &cmd)?;
                    }
                    (Some(env), None) => {
                        // ADDRESS env — change default environment.
                        let old = std::mem::replace(
                            &mut self.address_env,
                            env.to_uppercase(),
                        );
                        self.prev_address_env = old;
                    }
                    (None, None) => {
                        // ADDRESS (no args) — toggle between current and previous.
                        std::mem::swap(&mut self.address_env, &mut self.prev_address_env);
                    }
                    (None, Some(cmd_expr)) => {
                        // ADDRESS VALUE expr — set env from expression.
                        let env = self.eval_expr(cmd_expr)?;
                        let old = std::mem::replace(
                            &mut self.address_env,
                            env.to_uppercase(),
                        );
                        self.prev_address_env = old;
                    }
                }
                Ok(Flow::Normal)
            }
            ClauseBody::Procedure { expose } => {
                // Push a new variable pool, exposing listed variables.
                let parent = self.vars().clone();
                let mut new_pool = VarPool::default();
                let mut exposed_set = std::collections::HashSet::new();
                for var in expose {
                    let upper = var.to_uppercase();
                    exposed_set.insert(upper.clone());
                    if let Some(v) = parent.simple.get(&upper) {
                        new_pool.simple.insert(upper, v.clone());
                    }
                }
                // Store the exposed set for write-back on RETURN.
                new_pool.exposed = exposed_set;
                self.var_stack.push(new_pool);
                Ok(Flow::Normal)
            }
            ClauseBody::Numeric { setting, value } => {
                let val_str = self.eval_expr(value)?;
                let upper_setting = setting.to_uppercase();
                match upper_setting.as_str() {
                    "DIGITS" => {
                        let d = val_str
                            .parse::<usize>()
                            .map_err(|_| self.error(&format!("Invalid NUMERIC DIGITS: {val_str}")))?;
                        if d == 0 {
                            return Err(self.error("NUMERIC DIGITS must be positive"));
                        }
                        self.numeric.digits = d;
                    }
                    "FUZZ" => {
                        let f = val_str
                            .parse::<usize>()
                            .map_err(|_| self.error(&format!("Invalid NUMERIC FUZZ: {val_str}")))?;
                        self.numeric.fuzz = f;
                    }
                    "FORM" => match val_str.to_uppercase().as_str() {
                        "SCIENTIFIC" => self.numeric.form = NumericForm::Scientific,
                        "ENGINEERING" => self.numeric.form = NumericForm::Engineering,
                        _ => return Err(self.error(&format!("Invalid NUMERIC FORM: {val_str}"))),
                    },
                    _ => return Err(self.error(&format!("Unknown NUMERIC setting: {setting}"))),
                }
                Ok(Flow::Normal)
            }
            ClauseBody::Parse {
                upper,
                source,
                template,
            } => {
                let source_str = self.get_parse_source(source)?;
                self.apply_parse(&source_str, template, *upper);
                Ok(Flow::Normal)
            }
            ClauseBody::Arg(template) => {
                // ARG is shorthand for PARSE UPPER ARG template.
                let source_str = self.vars().get("ARG");
                self.apply_parse(&source_str, template, true);
                Ok(Flow::Normal)
            }
            ClauseBody::Pull(template) => {
                // PULL is shorthand for PARSE UPPER PULL template.
                // Read from data stack (stub: use empty string).
                let source_str = self.pull_from_stack();
                self.apply_parse(&source_str, template, true);
                Ok(Flow::Normal)
            }
            ClauseBody::Push(expr) => {
                let val = self.eval_expr(expr)?;
                self.data_stack.push(val);
                Ok(Flow::Normal)
            }
            ClauseBody::Queue(expr) => {
                let val = self.eval_expr(expr)?;
                self.data_stack.queue(val);
                Ok(Flow::Normal)
            }
            ClauseBody::Command(expr) => {
                let cmd = self.eval_expr(expr)?;
                let env = self.address_env.clone();
                self.exec_in_env(&env, &cmd)?;
                Ok(Flow::Normal)
            }
        }
    }

    // -----------------------------------------------------------------------
    //  DO loops
    // -----------------------------------------------------------------------

    fn exec_do(
        &mut self,
        control: &DoControl,
        body: &[Clause],
        _labels: &LabelTable,
    ) -> Result<Flow, InterpError> {
        let body_labels = build_label_table(body);

        match control {
            DoControl::Simple => self.exec_clauses(body, &body_labels),

            DoControl::Forever => loop {
                match self.exec_clauses(body, &body_labels)? {
                    Flow::Normal => {}
                    Flow::Iterate(None) => continue,
                    Flow::Iterate(Some(_)) => continue, // Named iterate — simplified.
                    Flow::Leave(None) => break Ok(Flow::Normal),
                    Flow::Leave(Some(_)) => break Ok(Flow::Normal),
                    flow => return Ok(flow),
                }
            },

            DoControl::Count(expr) => {
                let count_str = self.eval_expr(expr)?;
                let count = count_str
                    .parse::<i64>()
                    .map_err(|_| self.error(&format!("Bad repetition count: {count_str}")))?;
                for _ in 0..count {
                    match self.exec_clauses(body, &body_labels)? {
                        Flow::Normal => {}
                        Flow::Iterate(None) => continue,
                        Flow::Iterate(Some(_)) => continue,
                        Flow::Leave(None) => break,
                        Flow::Leave(Some(_)) => break,
                        flow => return Ok(flow),
                    }
                }
                Ok(Flow::Normal)
            }

            DoControl::Iterative { var, from, to, by } => {
                let from_val = self.eval_expr(from)?;
                let to_val = self.eval_expr(to)?;
                let by_val = if let Some(b) = by {
                    self.eval_expr(b)?
                } else {
                    "1".to_string()
                };

                let var_upper = var.to_uppercase();
                self.vars_mut().set(&var_upper, from_val.clone());

                loop {
                    // Check termination condition.
                    let current = self.vars().get(&var_upper);
                    let cmp = rexx_compare(&current, &to_val, 0)
                        .map_err(|e| self.error(&e))?;

                    let by_negative = by_val.starts_with('-');
                    let done = if by_negative {
                        cmp == std::cmp::Ordering::Less
                    } else {
                        cmp == std::cmp::Ordering::Greater
                    };
                    if done {
                        break;
                    }

                    match self.exec_clauses(body, &body_labels)? {
                        Flow::Normal => {}
                        Flow::Iterate(ref name) if name.is_none() || name.as_deref() == Some(&var_upper) => {}
                        Flow::Leave(ref name) if name.is_none() || name.as_deref() == Some(&var_upper) => {
                            break;
                        }
                        flow @ (Flow::Iterate(_) | Flow::Leave(_)) => return Ok(flow),
                        flow => return Ok(flow),
                    }

                    // Increment.
                    let current = self.vars().get(&var_upper);
                    let next = rexx_add(&current, &by_val, &self.numeric)
                        .map_err(|e| self.error(&e))?;
                    self.vars_mut().set(&var_upper, next);
                }
                Ok(Flow::Normal)
            }

            DoControl::While(cond) => loop {
                let val = self.eval_expr(cond)?;
                if !self.to_bool(&val)? {
                    break Ok(Flow::Normal);
                }
                match self.exec_clauses(body, &body_labels)? {
                    Flow::Normal => {}
                    Flow::Iterate(None) => continue,
                    Flow::Iterate(Some(_)) => continue,
                    Flow::Leave(None) => break Ok(Flow::Normal),
                    Flow::Leave(Some(_)) => break Ok(Flow::Normal),
                    flow => return Ok(flow),
                }
            },

            DoControl::Until(cond) => loop {
                match self.exec_clauses(body, &body_labels)? {
                    Flow::Normal => {}
                    Flow::Iterate(None) => {
                        let val = self.eval_expr(cond)?;
                        if self.to_bool(&val)? {
                            break Ok(Flow::Normal);
                        }
                        continue;
                    }
                    Flow::Iterate(Some(_)) => {
                        let val = self.eval_expr(cond)?;
                        if self.to_bool(&val)? {
                            break Ok(Flow::Normal);
                        }
                        continue;
                    }
                    Flow::Leave(None) => break Ok(Flow::Normal),
                    Flow::Leave(Some(_)) => break Ok(Flow::Normal),
                    flow => return Ok(flow),
                }
                let val = self.eval_expr(cond)?;
                if self.to_bool(&val)? {
                    break Ok(Flow::Normal);
                }
            },
        }
    }

    // -----------------------------------------------------------------------
    //  CALL / RETURN
    // -----------------------------------------------------------------------

    fn exec_call(
        &mut self,
        name: &str,
        args: &[Expr],
        all_clauses: &[Clause],
        labels: &LabelTable,
    ) -> Result<Flow, InterpError> {
        let upper = name.to_uppercase();

        // Evaluate arguments.
        let mut arg_vals = Vec::new();
        for a in args {
            arg_vals.push(self.eval_expr(a)?);
        }

        // Check for internal label.
        if let Some(&idx) = labels.get(&upper) {
            // Internal subroutine call.
            // Record stack depth before call.
            let stack_depth = self.var_stack.len();

            // Set ARG (via variable pool).
            let arg_string = arg_vals.join(" ");
            self.vars_mut().set("ARG", arg_string);

            // Execute from label + 1 (skip the label clause).
            let sub_clauses = &all_clauses[idx + 1..];

            match self.exec_clauses(sub_clauses, labels)? {
                Flow::Return(val) => {
                    self.result = val.clone();
                    // Pop PROCEDURE scope(s) if pushed during this call.
                    while self.var_stack.len() > stack_depth {
                        let returned = self.var_stack.pop().unwrap();
                        // Copy back only exposed variables to parent scope.
                        if !returned.exposed.is_empty() {
                            let parent = self.vars_mut();
                            for k in &returned.exposed {
                                if let Some(v) = returned.simple.get(k) {
                                    parent.simple.insert(k.clone(), v.clone());
                                }
                            }
                        }
                    }
                    if let Some(v) = val {
                        self.vars_mut().set("RESULT", v);
                    }
                    Ok(Flow::Normal)
                }
                Flow::Exit(val) => Ok(Flow::Exit(val)),
                Flow::Normal => {
                    // Subroutine fell through without RETURN.
                    self.result = None;
                    while self.var_stack.len() > stack_depth {
                        self.var_stack.pop();
                    }
                    Ok(Flow::Normal)
                }
                flow => Ok(flow),
            }
        } else {
            // Built-in function check (some can be called via CALL too).
            let result = self.try_builtin_function(&upper, &arg_vals);
            if let Some(val) = result {
                let v = val.map_err(|e| self.error(&e))?;
                self.result = Some(v.clone());
                self.vars_mut().set("RESULT", v);
                Ok(Flow::Normal)
            } else {
                Err(self.error(&format!("Routine not found: {name}")))
            }
        }
    }

    // -----------------------------------------------------------------------
    //  Expression evaluation
    // -----------------------------------------------------------------------

    fn eval_expr(&mut self, expr: &Expr) -> Result<String, InterpError> {
        match expr {
            Expr::StringLit(s) => Ok(s.clone()),
            Expr::Number(s) => Ok(s.clone()),
            Expr::Variable(name) => {
                let resolved = self.resolve_var_name(name);
                Ok(self.vars().get(&resolved))
            }
            Expr::HexLit(hex) => {
                // Convert hex pairs to ASCII string.
                let clean: String = hex.chars().filter(|c| !c.is_whitespace()).collect();
                let mut result = String::new();
                let mut i = 0;
                let bytes: Vec<char> = clean.chars().collect();
                while i + 1 < bytes.len() {
                    let hi = bytes[i].to_digit(16).unwrap_or(0);
                    let lo = bytes[i + 1].to_digit(16).unwrap_or(0);
                    result.push((hi * 16 + lo) as u8 as char);
                    i += 2;
                }
                Ok(result)
            }
            Expr::BinLit(bin) => {
                // Convert binary groups to characters.
                let clean: String = bin.chars().filter(|c| !c.is_whitespace()).collect();
                let mut result = String::new();
                // Pad to multiple of 8.
                let padded = format!("{:0>width$}", clean, width = ((clean.len() + 7) / 8) * 8);
                let mut i = 0;
                let bytes: Vec<char> = padded.chars().collect();
                while i + 7 < bytes.len() {
                    let mut val = 0u8;
                    for j in 0..8 {
                        val = val * 2 + if bytes[i + j] == '1' { 1 } else { 0 };
                    }
                    result.push(val as char);
                    i += 8;
                }
                Ok(result)
            }
            Expr::BinOp { left, op, right } => {
                let lval = self.eval_expr(left)?;
                let rval = self.eval_expr(right)?;
                self.eval_binop(&lval, *op, &rval)
            }
            Expr::UnaryOp { op, operand } => {
                let val = self.eval_expr(operand)?;
                self.eval_unaryop(*op, &val)
            }
            Expr::FunctionCall { name, args } => {
                let mut arg_vals = Vec::new();
                for a in args {
                    arg_vals.push(self.eval_expr(a)?);
                }
                let upper = name.to_uppercase();
                if let Some(result) = self.try_builtin_function(&upper, &arg_vals) {
                    result.map_err(|e| self.error(&e))
                } else {
                    Err(self.error(&format!("Unknown function: {name}")))
                }
            }
            Expr::Abuttal(parts) => {
                let mut result = String::new();
                for (i, part) in parts.iter().enumerate() {
                    if i > 0 {
                        result.push(' ');
                    }
                    result.push_str(&self.eval_expr(part)?);
                }
                Ok(result)
            }
        }
    }

    fn eval_binop(&mut self, left: &str, op: BinOp, right: &str) -> Result<String, InterpError> {
        match op {
            BinOp::Add => rexx_add(left, right, &self.numeric).map_err(|e| self.error(&e)),
            BinOp::Sub => rexx_sub(left, right, &self.numeric).map_err(|e| self.error(&e)),
            BinOp::Mul => rexx_mul(left, right, &self.numeric).map_err(|e| self.error(&e)),
            BinOp::Div => rexx_div(left, right, &self.numeric).map_err(|e| self.error(&e)),
            BinOp::IntDiv => rexx_idiv(left, right, &self.numeric).map_err(|e| self.error(&e)),
            BinOp::Mod => rexx_rem(left, right, &self.numeric).map_err(|e| self.error(&e)),
            BinOp::Power => rexx_pow(left, right, &self.numeric).map_err(|e| self.error(&e)),
            BinOp::Concat => Ok(format!("{left}{right}")),
            BinOp::Eq => {
                let r = self.compare_values(left, right)?;
                Ok(if r == std::cmp::Ordering::Equal { "1" } else { "0" }.into())
            }
            BinOp::Ne => {
                let r = self.compare_values(left, right)?;
                Ok(if r != std::cmp::Ordering::Equal { "1" } else { "0" }.into())
            }
            BinOp::Gt => {
                let r = self.compare_values(left, right)?;
                Ok(if r == std::cmp::Ordering::Greater { "1" } else { "0" }.into())
            }
            BinOp::Lt => {
                let r = self.compare_values(left, right)?;
                Ok(if r == std::cmp::Ordering::Less { "1" } else { "0" }.into())
            }
            BinOp::Ge => {
                let r = self.compare_values(left, right)?;
                Ok(if r != std::cmp::Ordering::Less { "1" } else { "0" }.into())
            }
            BinOp::Le => {
                let r = self.compare_values(left, right)?;
                Ok(if r != std::cmp::Ordering::Greater { "1" } else { "0" }.into())
            }
            BinOp::StrictEq => Ok(if left == right { "1" } else { "0" }.into()),
            BinOp::StrictNe => Ok(if left != right { "1" } else { "0" }.into()),
            BinOp::StrictGt => Ok(if left > right { "1" } else { "0" }.into()),
            BinOp::StrictLt => Ok(if left < right { "1" } else { "0" }.into()),
            BinOp::StrictGe => Ok(if left >= right { "1" } else { "0" }.into()),
            BinOp::StrictLe => Ok(if left <= right { "1" } else { "0" }.into()),
            BinOp::And => {
                let a = self.to_bool(left)?;
                let b = self.to_bool(right)?;
                Ok(if a && b { "1" } else { "0" }.into())
            }
            BinOp::Or => {
                let a = self.to_bool(left)?;
                let b = self.to_bool(right)?;
                Ok(if a || b { "1" } else { "0" }.into())
            }
            BinOp::Xor => {
                let a = self.to_bool(left)?;
                let b = self.to_bool(right)?;
                Ok(if a ^ b { "1" } else { "0" }.into())
            }
        }
    }

    fn eval_unaryop(&mut self, op: UnaryOp, val: &str) -> Result<String, InterpError> {
        match op {
            UnaryOp::Neg => rexx_sub("0", val, &self.numeric).map_err(|e| self.error(&e)),
            UnaryOp::Pos => rexx_add("0", val, &self.numeric).map_err(|e| self.error(&e)),
            UnaryOp::Not => {
                let b = self.to_bool(val)?;
                Ok(if b { "0" } else { "1" }.into())
            }
        }
    }

    // -----------------------------------------------------------------------
    //  Built-in functions
    // -----------------------------------------------------------------------

    fn try_builtin_function(
        &mut self,
        name: &str,
        args: &[String],
    ) -> Option<Result<String, String>> {
        // Context-dependent functions that need interpreter state.
        match name {
            "DATATYPE" => {
                let s = args.first().map(|a| a.as_str()).unwrap_or("");
                let typ = args
                    .get(1)
                    .map(|a| a.to_uppercase())
                    .unwrap_or_default();
                let result = if typ.is_empty() {
                    if RexxValue::from_string(s).is_number() {
                        "NUM"
                    } else {
                        "CHAR"
                    }
                    .to_string()
                } else {
                    let ok = match typ.as_str() {
                        "N" => RexxValue::from_string(s).is_number(),
                        "A" => s.chars().all(|c| c.is_ascii_alphabetic()),
                        "U" => s.chars().all(|c| c.is_ascii_uppercase()),
                        "L" => s.chars().all(|c| c.is_ascii_lowercase()),
                        "W" => s.split_whitespace().all(|w| RexxValue::from_string(w).is_number()),
                        _ => false,
                    };
                    if ok { "1" } else { "0" }.to_string()
                };
                return Some(Ok(result));
            }
            "SYMBOL" => {
                let name_arg = args.first().map(|a| a.as_str()).unwrap_or("");
                let state = self.vars().symbol_state(name_arg);
                return Some(Ok(state.to_string()));
            }
            "QUEUED" => {
                return Some(Ok(self.data_stack.queued().to_string()));
            }
            "ABS" => {
                let s = args.first().map(|a| a.as_str()).unwrap_or("0");
                return match rexx_compare(s, "0", 0) {
                    Ok(std::cmp::Ordering::Less) => Some(rexx_sub("0", s, &self.numeric)),
                    Ok(_) => Some(rexx_add(s, "0", &self.numeric)),
                    Err(e) => Some(Err(e)),
                };
            }
            "MAX" => {
                if args.is_empty() {
                    return Some(Err("MAX requires at least one argument".into()));
                }
                let mut best = args[0].clone();
                for a in &args[1..] {
                    if rexx_compare(a, &best, 0) == Ok(std::cmp::Ordering::Greater) {
                        best = a.clone();
                    }
                }
                return Some(Ok(best));
            }
            "MIN" => {
                if args.is_empty() {
                    return Some(Err("MIN requires at least one argument".into()));
                }
                let mut best = args[0].clone();
                for a in &args[1..] {
                    if rexx_compare(a, &best, 0) == Ok(std::cmp::Ordering::Less) {
                        best = a.clone();
                    }
                }
                return Some(Ok(best));
            }
            "TRUNC" => {
                let s = args.first().map(|a| a.as_str()).unwrap_or("0");
                let n = args
                    .get(1)
                    .and_then(|a| a.parse::<usize>().ok())
                    .unwrap_or(0);
                return if let Some(dot) = s.find('.') {
                    if n == 0 {
                        Some(Ok(s[..dot].to_string()))
                    } else {
                        let frac = &s[dot + 1..];
                        let truncated: String = frac.chars().take(n).collect();
                        let padded = format!("{truncated:0<width$}", width = n);
                        Some(Ok(format!("{}.{padded}", &s[..dot])))
                    }
                } else if n > 0 {
                    Some(Ok(format!("{s}.{zeros}", zeros = "0".repeat(n))))
                } else {
                    Some(Ok(s.to_string()))
                };
            }
            _ => {}
        }

        // Delegate to builtins module for all other functions.
        builtins::call_builtin(name, args)
    }

    // -----------------------------------------------------------------------
    //  Helpers
    // -----------------------------------------------------------------------

    /// Resolve compound variable names — substitute tail variables.
    fn resolve_var_name(&self, name: &str) -> String {
        let upper = name.to_uppercase();
        if let Some(dot) = upper.find('.') {
            if dot < upper.len() - 1 {
                let stem = &upper[..=dot];
                let tail = &upper[dot + 1..];
                // Resolve each tail component (split by .).
                let resolved_tail: Vec<String> = tail
                    .split('.')
                    .map(|part| {
                        if part.is_empty() {
                            String::new()
                        } else if part.chars().all(|c| c.is_ascii_digit()) {
                            part.to_string() // Numeric tail — keep as-is.
                        } else {
                            self.vars().get(part)
                        }
                    })
                    .collect();
                format!("{stem}{}", resolved_tail.join("."))
            } else {
                upper
            }
        } else {
            upper
        }
    }

    /// Compare two REXX values — numeric if both are numbers, otherwise string comparison.
    fn compare_values(&self, a: &str, b: &str) -> Result<std::cmp::Ordering, InterpError> {
        // Try numeric comparison first.
        if let Ok(ord) = rexx_compare(a, b, self.numeric.fuzz) {
            return Ok(ord);
        }
        // Fall back to string comparison (padded with spaces).
        let a_trimmed = a.trim();
        let b_trimmed = b.trim();
        Ok(a_trimmed.cmp(b_trimmed))
    }

    /// Convert a string to a REXX boolean (must be "0" or "1").
    fn to_bool(&self, val: &str) -> Result<bool, InterpError> {
        match val.trim() {
            "0" => Ok(false),
            "1" => Ok(true),
            _ => Err(self.error(&format!(
                "Value is not boolean (0 or 1): '{val}'"
            ))),
        }
    }

    /// Create an error at the current line.
    fn error(&self, msg: &str) -> InterpError {
        InterpError::Runtime {
            line: self.current_line,
            message: msg.to_string(),
        }
    }

    // -----------------------------------------------------------------------
    //  PARSE helpers
    // -----------------------------------------------------------------------

    /// Get the source string for a PARSE instruction.
    fn get_parse_source(&mut self, source: &ParseSource) -> Result<String, InterpError> {
        match source {
            ParseSource::Arg => Ok(self.vars().get("ARG")),
            ParseSource::Pull => Ok(self.pull_from_stack()),
            ParseSource::Var(name) => {
                let resolved = self.resolve_var_name(name);
                Ok(self.vars().get(&resolved))
            }
            ParseSource::Value(expr) => self.eval_expr(expr),
            ParseSource::External => Ok(String::new()), // Stub.
            ParseSource::Source => {
                // PARSE SOURCE returns: system invocation_type name
                Ok("TSO COMMAND EXEC".to_string())
            }
            ParseSource::Version => {
                // PARSE VERSION returns: language level date
                Ok("REXX-OpenMainframe 4.00 01 Jan 2025".to_string())
            }
            ParseSource::Linein => Ok(String::new()), // Stub.
        }
    }

    /// Apply a PARSE template, setting variables in the current pool.
    fn apply_parse(&mut self, source: &str, template_str: &str, upper: bool) {
        let template = parse_template(template_str);
        let var_resolver = |name: &str| -> String {
            // We can't borrow self here, so return the name as-is.
            // Variable pattern resolution is limited in this context.
            name.to_string()
        };
        let bindings = execute_parse(source, &template, upper, &var_resolver);
        for (name, value) in bindings {
            self.vars_mut().set(&name, value);
        }
    }

    /// Pull a value from the data stack.
    fn pull_from_stack(&mut self) -> String {
        self.data_stack.pull()
    }

    // -----------------------------------------------------------------------
    //  ADDRESS environment command dispatch
    // -----------------------------------------------------------------------

    /// Execute a command in a specific ADDRESS environment.
    fn exec_in_env(&mut self, env: &str, cmd: &str) -> Result<(), InterpError> {
        match env {
            "TSO" => self.exec_tso_command(cmd),
            "MVS" | "LINK" | "LINKPGM" | "LINKMVS" | "ATTACH" | "ATTCHPGM" | "ATTCHMVS" => {
                self.exec_mvs_command(cmd)
            }
            "ISPEXEC" => self.exec_ispexec_command(cmd),
            "ISREDIT" => self.exec_isredit_command(cmd),
            _ => {
                // Unknown environment — treat as TSO.
                self.exec_tso_command(cmd)
            }
        }
    }

    /// Execute a TSO command.
    fn exec_tso_command(&mut self, cmd: &str) -> Result<(), InterpError> {
        let trimmed = cmd.trim();
        let upper = trimmed.to_uppercase();
        let words: Vec<&str> = upper.split_whitespace().collect();

        match words.first().copied() {
            Some("MAKEBUF") => {
                let buf_num = self.data_stack.makebuf();
                self.rc = buf_num as i32;
                Ok(())
            }
            Some("DROPBUF") => {
                let n = words.get(1).and_then(|s| s.parse::<usize>().ok()).unwrap_or(1);
                self.data_stack.dropbuf(n);
                self.rc = 0;
                Ok(())
            }
            Some("NEWSTACK") => {
                let old = self.data_stack.newstack();
                self.saved_stacks.push(old);
                self.rc = 0;
                Ok(())
            }
            Some("DELSTACK") => {
                if let Some(restored) = self.saved_stacks.pop() {
                    self.data_stack = restored;
                } else {
                    self.data_stack = DataStack::default();
                }
                self.rc = 0;
                Ok(())
            }
            Some("EXECIO") => self.exec_execio(trimmed),
            Some("ALLOCATE" | "ALLOC") => {
                // Simulated ALLOCATE: parse DD name and dataset.
                self.exec_allocate(trimmed)
            }
            Some("FREE") => {
                // Simulated FREE: remove a DD allocation.
                if let Some(dd) = words.iter().position(|&w| w == "FI" || w == "FILE" || w == "F")
                    .and_then(|i| words.get(i + 1))
                {
                    let ddname = dd.trim_matches(|c| c == '(' || c == ')');
                    self.dd_files.remove(ddname);
                }
                self.rc = 0;
                Ok(())
            }
            Some("LISTDS" | "LISTDSI") => {
                // Stub: just set RC=0.
                self.rc = 0;
                Ok(())
            }
            _ => {
                // Unknown TSO command — set RC=0 (no error in simulation).
                self.rc = 0;
                Ok(())
            }
        }
    }

    /// Execute an MVS command.
    fn exec_mvs_command(&mut self, cmd: &str) -> Result<(), InterpError> {
        let trimmed = cmd.trim();
        let upper = trimmed.to_uppercase();
        let words: Vec<&str> = upper.split_whitespace().collect();

        match words.first().copied() {
            Some("EXECIO") => self.exec_execio(trimmed),
            _ => {
                // Unknown MVS command — set RC=0.
                self.rc = 0;
                Ok(())
            }
        }
    }

    /// Execute an ISPEXEC command (stub for ISPF dialog services).
    fn exec_ispexec_command(&mut self, _cmd: &str) -> Result<(), InterpError> {
        // ISPF dialog services will be implemented in T104.
        self.rc = 0;
        Ok(())
    }

    /// Execute an ISREDIT command (stub for ISPF editor macros).
    fn exec_isredit_command(&mut self, _cmd: &str) -> Result<(), InterpError> {
        // ISPF edit macros will be implemented in T109.
        self.rc = 0;
        Ok(())
    }

    /// Simulated ALLOCATE command: parse FI(ddname) and create a DD entry.
    fn exec_allocate(&mut self, cmd: &str) -> Result<(), InterpError> {
        let upper = cmd.to_uppercase();
        // Find FI(ddname) or FILE(ddname) or F(ddname).
        let ddname = extract_paren_value(&upper, "FI")
            .or_else(|| extract_paren_value(&upper, "FILE"))
            .or_else(|| extract_paren_value(&upper, "F"));

        if let Some(dd) = ddname {
            self.dd_files.entry(dd).or_default();
        }
        self.rc = 0;
        Ok(())
    }

    // -----------------------------------------------------------------------
    //  EXECIO
    // -----------------------------------------------------------------------

    /// Execute an EXECIO command: `EXECIO count DISKR|DISKW ddname (options`
    fn exec_execio(&mut self, cmd: &str) -> Result<(), InterpError> {
        // Parse: EXECIO count DISKR|DISKW ddname [(options)]
        // The actual command string has mixed case from the user; parse case-insensitively.
        let parts: Vec<&str> = cmd.split_whitespace().collect();
        if parts.len() < 4 {
            return Err(self.error("EXECIO requires: EXECIO count DISKR|DISKW ddname"));
        }

        let count_str = parts[1].to_uppercase();
        let op = parts[2].to_uppercase();
        let ddname = parts[3].to_uppercase();

        // Parse options after '('.
        let opts_str = cmd.find('(').map(|i| &cmd[i + 1..]).unwrap_or("");
        let opts_upper = opts_str.to_uppercase();
        let opt_words: Vec<&str> = opts_upper.split_whitespace().collect();

        let stem = opt_words.iter()
            .position(|&w| w == "STEM")
            .and_then(|i| opt_words.get(i + 1))
            .map(|s| {
                let mut st = s.to_string();
                if !st.ends_with('.') {
                    st.push('.');
                }
                st
            });
        let finis = opt_words.contains(&"FINIS");

        match op.as_str() {
            "DISKR" => {
                let count = if count_str == "*" {
                    usize::MAX
                } else {
                    count_str.parse::<usize>().unwrap_or(0)
                };
                self.execio_read(&ddname, count, stem.as_deref(), finis)
            }
            "DISKW" => {
                let count = count_str.parse::<usize>().unwrap_or(0);
                self.execio_write(&ddname, count, stem.as_deref(), finis)
            }
            _ => Err(self.error(&format!("EXECIO: unknown operation '{op}'"))),
        }
    }

    /// EXECIO DISKR — read records from a DD.
    fn execio_read(
        &mut self,
        ddname: &str,
        count: usize,
        stem: Option<&str>,
        _finis: bool,
    ) -> Result<(), InterpError> {
        // Clone records out to avoid borrow conflicts.
        let dd = self.dd_files.entry(ddname.to_string()).or_default();
        let read_pos = dd.read_pos;
        let available = dd.records.len().saturating_sub(read_pos);
        let to_read = count.min(available);
        let records: Vec<String> = dd.records[read_pos..read_pos + to_read].to_vec();
        dd.read_pos += to_read;

        let records_read = records.len();

        if let Some(stem_name) = stem {
            for (i, rec) in records.into_iter().enumerate() {
                let var_name = format!("{stem_name}{}", i + 1);
                self.vars_mut().set(&var_name, rec);
            }
            let count_var = format!("{stem_name}0");
            self.vars_mut().set(&count_var, records_read.to_string());
        } else {
            for rec in records {
                self.data_stack.queue(rec);
            }
        }

        // RC: 0 = success, 2 = EOF reached before count satisfied.
        self.rc = if records_read < count && count != usize::MAX { 2 } else { 0 };
        Ok(())
    }

    /// EXECIO DISKW — write records to a DD.
    fn execio_write(
        &mut self,
        ddname: &str,
        count: usize,
        stem: Option<&str>,
        _finis: bool,
    ) -> Result<(), InterpError> {
        if count == 0 {
            self.dd_files.entry(ddname.to_string()).or_default();
            self.rc = 0;
            return Ok(());
        }

        // Collect values first to avoid borrow conflicts.
        let values: Vec<String> = if let Some(stem_name) = stem {
            (1..=count)
                .map(|i| {
                    let var_name = format!("{stem_name}{i}");
                    self.vars().get(&var_name)
                })
                .collect()
        } else {
            (0..count).map(|_| self.data_stack.pull()).collect()
        };

        let dd = self.dd_files.entry(ddname.to_string()).or_default();
        dd.records.extend(values);
        self.rc = 0;
        Ok(())
    }

    /// Allocate a DD with initial records (for testing).
    #[cfg(test)]
    fn allocate_dd(&mut self, ddname: &str, records: Vec<String>) {
        self.dd_files.insert(
            ddname.to_uppercase(),
            DdAllocation {
                records,
                read_pos: 0,
            },
        );
    }
}

// ---------------------------------------------------------------------------
//  Helpers
// ---------------------------------------------------------------------------

/// Extract a parenthesized value after a keyword, e.g. `FI(INDD)` → `"INDD"`.
fn extract_paren_value(text: &str, keyword: &str) -> Option<String> {
    let pat = format!("{keyword}(");
    if let Some(start) = text.find(&pat) {
        let after = &text[start + pat.len()..];
        if let Some(end) = after.find(')') {
            return Some(after[..end].to_string());
        }
    }
    None
}

/// Build a label table from a clause list.
fn build_label_table(clauses: &[Clause]) -> LabelTable {
    let mut labels = LabelTable::new();
    for (i, clause) in clauses.iter().enumerate() {
        if let ClauseBody::Label(name) = &clause.body {
            labels.insert(name.to_uppercase(), i);
        }
    }
    labels
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    use crate::lexer::lex;

    fn run(source: &str) -> ExecResult {
        let tokens = lex(source).expect("lex failed");
        let program = parse(&tokens).expect("parse failed");
        interpret(&program).expect("interpret failed")
    }

    #[test]
    fn test_say_string() {
        let result = run("SAY 'Hello World'");
        assert_eq!(result.output, vec!["Hello World"]);
    }

    #[test]
    fn test_say_number() {
        let result = run("SAY 42");
        assert_eq!(result.output, vec!["42"]);
    }

    #[test]
    fn test_assignment_and_say() {
        let result = run("x = 10\nSAY x");
        assert_eq!(result.output, vec!["10"]);
    }

    #[test]
    fn test_arithmetic() {
        let result = run("SAY 3 + 4 * 2");
        // Parser uses precedence: 3 + (4*2) = 3 + 8 = 11
        assert_eq!(result.output, vec!["11"]);
    }

    #[test]
    fn test_string_concat() {
        let result = run("SAY 'AB' || 'CD'");
        assert_eq!(result.output, vec!["ABCD"]);
    }

    #[test]
    fn test_blank_concat() {
        // The parser doesn't yet support implicit blank concatenation (abuttal);
        // use the || operator for explicit concatenation.
        let result = run("SAY 'Hello' || ' ' || 'World'");
        assert_eq!(result.output, vec!["Hello World"]);
    }

    #[test]
    fn test_if_then() {
        let result = run("x = 1\nIF x = 1 THEN SAY 'yes'");
        assert_eq!(result.output, vec!["yes"]);
    }

    #[test]
    fn test_if_then_else() {
        let result = run("x = 0\nIF x = 1 THEN SAY 'yes'\nELSE SAY 'no'");
        assert_eq!(result.output, vec!["no"]);
    }

    #[test]
    fn test_do_count() {
        let result = run("DO 3\n  SAY 'hi'\nEND");
        assert_eq!(result.output, vec!["hi", "hi", "hi"]);
    }

    #[test]
    fn test_do_iterative() {
        let result = run("DO i = 1 TO 3\n  SAY i\nEND");
        assert_eq!(result.output, vec!["1", "2", "3"]);
    }

    #[test]
    fn test_do_iterative_by() {
        let result = run("DO i = 2 TO 10 BY 3\n  SAY i\nEND");
        assert_eq!(result.output, vec!["2", "5", "8"]);
    }

    #[test]
    fn test_do_while() {
        let result = run("x = 0\nDO WHILE x < 3\n  x = x + 1\n  SAY x\nEND");
        assert_eq!(result.output, vec!["1", "2", "3"]);
    }

    #[test]
    fn test_do_until() {
        let result = run("x = 0\nDO UNTIL x = 3\n  x = x + 1\n  SAY x\nEND");
        assert_eq!(result.output, vec!["1", "2", "3"]);
    }

    #[test]
    fn test_iterate() {
        let result = run("DO i = 1 TO 5\n  IF i = 3 THEN ITERATE\n  SAY i\nEND");
        assert_eq!(result.output, vec!["1", "2", "4", "5"]);
    }

    #[test]
    fn test_leave() {
        let result = run("DO i = 1 TO 10\n  IF i = 4 THEN LEAVE\n  SAY i\nEND");
        assert_eq!(result.output, vec!["1", "2", "3"]);
    }

    #[test]
    fn test_select() {
        let result = run(
            "x = 2\nSELECT\n  WHEN x = 1 THEN SAY 'one'\n  WHEN x = 2 THEN SAY 'two'\n  OTHERWISE SAY 'other'\nEND",
        );
        assert_eq!(result.output, vec!["two"]);
    }

    #[test]
    fn test_select_otherwise() {
        let result = run(
            "x = 99\nSELECT\n  WHEN x = 1 THEN SAY 'one'\n  OTHERWISE SAY 'other'\nEND",
        );
        assert_eq!(result.output, vec!["other"]);
    }

    #[test]
    fn test_call_return() {
        let result = run(
            "CALL greet\nSAY RESULT\nEXIT\ngreet:\n  RETURN 'Hello!'",
        );
        assert_eq!(result.output, vec!["Hello!"]);
    }

    #[test]
    fn test_procedure_expose() {
        let result = run(
            "x = 10\nCALL inc\nSAY x\nEXIT\ninc: PROCEDURE EXPOSE x\n  x = x + 1\n  RETURN",
        );
        assert_eq!(result.output, vec!["11"]);
    }

    #[test]
    fn test_procedure_isolation() {
        // Without EXPOSE, the subroutine gets its own scope.
        let result = run(
            "x = 10\nCALL sub\nSAY x\nEXIT\nsub: PROCEDURE\n  x = 99\n  RETURN",
        );
        assert_eq!(result.output, vec!["10"]);
    }

    #[test]
    fn test_compound_variable() {
        let result = run("stem. = 0\nstem.1 = 'first'\nSAY stem.1\nSAY stem.99");
        assert_eq!(result.output, vec!["first", "0"]);
    }

    #[test]
    fn test_drop() {
        let result = run("x = 'hello'\nDROP x\nSAY x");
        // After DROP, x returns its own name uppercased.
        assert_eq!(result.output, vec!["X"]);
    }

    #[test]
    fn test_symbol_function() {
        let result = run("x = 5\nSAY SYMBOL('x')\nSAY SYMBOL('y')");
        assert_eq!(result.output, vec!["VAR", "LIT"]);
    }

    #[test]
    fn test_numeric_digits() {
        let result = run("NUMERIC DIGITS 20\nSAY 1 / 3");
        let output = &result.output[0];
        assert!(output.starts_with("0.3333333333333333333"), "got {output}");
    }

    #[test]
    fn test_comparison_operators() {
        let result = run("SAY (5 > 3)\nSAY (3 > 5)\nSAY (5 = 5)");
        assert_eq!(result.output, vec!["1", "0", "1"]);
    }

    #[test]
    fn test_logical_operators() {
        let result = run("SAY (1 & 1)\nSAY (1 & 0)\nSAY (0 | 1)\nSAY \\1");
        assert_eq!(result.output, vec!["1", "0", "1", "0"]);
    }

    #[test]
    fn test_unary_minus() {
        let result = run("x = 5\nSAY -x");
        assert_eq!(result.output, vec!["-5"]);
    }

    #[test]
    fn test_exit_rc() {
        let result = run("EXIT 4");
        assert_eq!(result.rc, 4);
    }

    #[test]
    fn test_nop() {
        let result = run("NOP\nSAY 'ok'");
        assert_eq!(result.output, vec!["ok"]);
    }

    #[test]
    fn test_do_forever_leave() {
        let result = run("i = 0\nDO FOREVER\n  i = i + 1\n  IF i = 3 THEN LEAVE\nEND\nSAY i");
        assert_eq!(result.output, vec!["3"]);
    }

    #[test]
    fn test_function_call_length() {
        let result = run("SAY LENGTH('Hello')");
        assert_eq!(result.output, vec!["5"]);
    }

    #[test]
    fn test_function_copies() {
        let result = run("SAY COPIES('Ab', 3)");
        assert_eq!(result.output, vec!["AbAbAb"]);
    }

    #[test]
    fn test_signal_label() {
        let result = run("SIGNAL skip\nSAY 'not printed'\nskip:\nSAY 'jumped'");
        assert_eq!(result.output, vec!["jumped"]);
    }

    #[test]
    fn test_nested_do() {
        let result = run("DO i = 1 TO 2\n  DO j = 1 TO 2\n    SAY i || j\n  END\nEND");
        assert_eq!(result.output, vec!["11", "12", "21", "22"]);
    }

    #[test]
    fn test_abs_function() {
        let result = run("SAY ABS(-42)\nSAY ABS(7)");
        assert_eq!(result.output, vec!["42", "7"]);
    }

    #[test]
    fn test_max_min_functions() {
        let result = run("SAY MAX(3, 7, 1)\nSAY MIN(3, 7, 1)");
        assert_eq!(result.output, vec!["7", "1"]);
    }

    // -- Data stack tests --

    #[test]
    fn test_push_pull_lifo() {
        let result = run("PUSH 'last'\nPUSH 'first'\nPULL x\nSAY x");
        assert_eq!(result.output, vec!["FIRST"]);
    }

    #[test]
    fn test_queue_pull_fifo() {
        let result = run("QUEUE 'first'\nQUEUE 'last'\nPULL x\nSAY x");
        assert_eq!(result.output, vec!["FIRST"]);
    }

    #[test]
    fn test_queued_function() {
        let result = run("PUSH 'a'\nPUSH 'b'\nPUSH 'c'\nSAY QUEUED()");
        assert_eq!(result.output, vec!["3"]);
    }

    #[test]
    fn test_push_pull_multiple() {
        let result = run(
            "PUSH 'c'\nPUSH 'b'\nPUSH 'a'\nPULL x\nPULL y\nPULL z\nSAY x\nSAY y\nSAY z",
        );
        assert_eq!(result.output, vec!["A", "B", "C"]);
    }

    #[test]
    fn test_queue_fifo_order() {
        let result = run(
            "QUEUE 'a'\nQUEUE 'b'\nQUEUE 'c'\nPULL x\nPULL y\nPULL z\nSAY x\nSAY y\nSAY z",
        );
        assert_eq!(result.output, vec!["A", "B", "C"]);
    }

    #[test]
    fn test_pull_empty_stack() {
        let result = run("PULL x\nSAY x");
        // Empty pull returns empty string, PARSE UPPER gives empty.
        assert_eq!(result.output, vec![""]);
    }

    #[test]
    fn test_queued_after_pull() {
        let result = run(
            "PUSH 'a'\nPUSH 'b'\nPULL x\nSAY QUEUED()",
        );
        assert_eq!(result.output, vec!["1"]);
    }

    // -- EXECIO tests (require direct interpreter access) --

    fn run_with_dd(source: &str, ddname: &str, records: Vec<&str>) -> (ExecResult, Interpreter) {
        let tokens = lex(source).expect("lex failed");
        let program = parse(&tokens).expect("parse failed");
        let mut interp = Interpreter::new();
        let recs: Vec<String> = records.iter().map(|s| s.to_string()).collect();
        interp.allocate_dd(ddname, recs);
        interp.run(&program).expect("interpret failed");
        let result = ExecResult {
            rc: interp.rc,
            output: interp.output.clone(),
        };
        (result, interp)
    }

    #[test]
    fn test_execio_read_stem() {
        let (result, interp) = run_with_dd(
            "\"EXECIO * DISKR INDD (STEM LINE. FINIS\"",
            "INDD",
            vec!["rec1", "rec2", "rec3"],
        );
        assert_eq!(result.rc, 0);
        assert_eq!(interp.vars().get("LINE.0"), "3");
        assert_eq!(interp.vars().get("LINE.1"), "rec1");
        assert_eq!(interp.vars().get("LINE.2"), "rec2");
        assert_eq!(interp.vars().get("LINE.3"), "rec3");
    }

    #[test]
    fn test_execio_read_stack() {
        let (result, interp) = run_with_dd(
            "\"EXECIO * DISKR INDD (FINIS\"",
            "INDD",
            vec!["line1", "line2"],
        );
        assert_eq!(result.rc, 0);
        assert_eq!(interp.data_stack.queued(), 2);
    }

    #[test]
    fn test_execio_write_stem() {
        let source = "rec.1 = 'hello'\nrec.2 = 'world'\n\"EXECIO 2 DISKW OUTDD (STEM REC.\"";
        let (result, interp) = run_with_dd(source, "DUMMY", vec![]);
        assert_eq!(result.rc, 0);
        let dd = interp.dd_files.get("OUTDD").unwrap();
        assert_eq!(dd.records, vec!["hello", "world"]);
    }

    #[test]
    fn test_execio_eof_rc() {
        // Request 10 records from a DD with only 2 — should get RC=2.
        let (result, _) = run_with_dd(
            "\"EXECIO 10 DISKR INDD (STEM LINE. FINIS\"",
            "INDD",
            vec!["a", "b"],
        );
        assert_eq!(result.rc, 2);
    }

    // -- Data stack unit tests --

    #[test]
    fn test_data_stack_makebuf_dropbuf() {
        let mut stack = DataStack::default();
        stack.push("a".into());
        stack.makebuf();
        stack.push("b".into());
        stack.push("c".into());
        // Stack now: c, b, a (with buffer boundary at depth 1).
        assert_eq!(stack.queued(), 3);
        stack.dropbuf(1);
        // After dropbuf, items added since makebuf are removed.
        assert_eq!(stack.queued(), 1);
        assert_eq!(stack.pull(), "a");
    }

    #[test]
    fn test_data_stack_newstack() {
        let mut stack = DataStack::default();
        stack.push("old".into());
        let saved = stack.newstack();
        assert_eq!(stack.queued(), 0); // New stack is empty.
        assert_eq!(saved.queued(), 1); // Old stack has the item.
        stack.push("new".into());
        // Restore.
        stack = saved;
        assert_eq!(stack.pull(), "old");
    }

    // -- ADDRESS environment tests --

    #[test]
    fn test_address_tso_command() {
        // ADDRESS TSO 'cmd' should route to TSO environment.
        let result = run("ADDRESS TSO 'LISTDS DUMMY'");
        assert_eq!(result.rc, 0);
    }

    #[test]
    fn test_address_change_default() {
        // ADDRESS env (no command) changes the default environment.
        let result = run("ADDRESS ISPEXEC\nSAY 'ok'");
        assert_eq!(result.output, vec!["ok"]);
    }

    #[test]
    fn test_address_toggle() {
        // ADDRESS with no args toggles between current and previous.
        let result = run("ADDRESS MVS\nADDRESS\nSAY 'ok'");
        assert_eq!(result.output, vec!["ok"]);
    }

    #[test]
    fn test_address_mvs_execio() {
        // EXECIO via ADDRESS MVS environment.
        let (result, interp) = run_with_dd(
            "ADDRESS MVS \"EXECIO * DISKR INDD (STEM LINE. FINIS\"",
            "INDD",
            vec!["data1", "data2"],
        );
        assert_eq!(result.rc, 0);
        assert_eq!(interp.vars().get("LINE.0"), "2");
        assert_eq!(interp.vars().get("LINE.1"), "data1");
        assert_eq!(interp.vars().get("LINE.2"), "data2");
    }

    #[test]
    fn test_address_tso_allocate() {
        // ADDRESS TSO ALLOCATE should create a DD entry.
        let result = run("ADDRESS TSO \"ALLOCATE FI(MYDD) DA('DUMMY') SHR\"");
        assert_eq!(result.rc, 0);
    }

    #[test]
    fn test_host_command_default_tso() {
        // Unquoted host commands default to TSO environment.
        let result = run("\"LISTDS DUMMY\"");
        assert_eq!(result.rc, 0);
    }
}
