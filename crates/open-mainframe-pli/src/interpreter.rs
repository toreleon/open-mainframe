//! PL/I Interpreter — expression evaluator, control flow, and procedure calling.
//!
//! Implements the core runtime for PL/I programs:
//! - Expression evaluation with automatic type coercion
//! - Control flow: IF/THEN/ELSE, DO (simple, WHILE, UNTIL, iterative), SELECT
//! - Procedures: CALL, RETURN, recursion, scoped variables
//! - Assignment with type conversion
//! - I/O: PUT LIST (console output)

use std::collections::HashMap;

use crate::parser::*;
use crate::types::*;

// ---------------------------------------------------------------------------
//  Interpreter errors
// ---------------------------------------------------------------------------

/// Errors from the PL/I interpreter.
#[derive(Debug, Clone, thiserror::Error)]
pub enum InterpreterError {
    #[error("undefined variable: {0}")]
    UndefinedVariable(String),

    #[error("undefined procedure: {0}")]
    UndefinedProcedure(String),

    #[error("type error: {0}")]
    TypeError(String),

    #[error("conversion error: {0}")]
    Conversion(#[from] ConversionError),

    #[error("division by zero")]
    DivisionByZero,

    #[error("label not found: {0}")]
    LabelNotFound(String),

    #[error("STOP statement executed")]
    Stop,

    #[error("EXIT statement executed")]
    Exit,

    #[error("stack overflow: recursion depth exceeded {0}")]
    StackOverflow(usize),
}

// ---------------------------------------------------------------------------
//  Scope / Environment
// ---------------------------------------------------------------------------

/// A scope frame holding variable bindings and their types.
#[derive(Debug, Clone)]
struct Scope {
    /// Variable bindings: name → (type, value).
    vars: HashMap<String, (PliType, PliValue)>,
}

impl Scope {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }
}

// ---------------------------------------------------------------------------
//  Interpreter
// ---------------------------------------------------------------------------

/// PL/I interpreter — executes a parsed AST.
pub struct Interpreter {
    /// Scope stack (last = current scope).
    scopes: Vec<Scope>,
    /// Procedure definitions: name → ProcedureStmt.
    procedures: HashMap<String, ProcedureStmt>,
    /// Output buffer (PUT LIST writes here).
    output: Vec<String>,
    /// Maximum recursion depth.
    max_depth: usize,
    /// Current recursion depth.
    depth: usize,
}

/// Result of executing a statement — used internally for control flow.
enum StmtResult {
    /// Normal completion.
    Ok,
    /// RETURN with optional value.
    Return(Option<PliValue>),
    /// LEAVE loop (with optional label).
    #[allow(dead_code)]
    Leave(Option<String>),
    /// ITERATE loop (with optional label).
    #[allow(dead_code)]
    Iterate(Option<String>),
    /// GOTO label.
    #[allow(dead_code)]
    GoTo(String),
}

impl Interpreter {
    /// Create a new interpreter.
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
            procedures: HashMap::new(),
            output: Vec::new(),
            max_depth: 1000,
            depth: 0,
        }
    }

    /// Run a program.
    pub fn run(&mut self, program: &Program) -> Result<(), InterpreterError> {
        // First pass: collect procedure definitions.
        for stmt in &program.statements {
            if let Statement::Procedure(proc) = stmt {
                self.procedures
                    .insert(proc.name.to_uppercase(), proc.clone());
            }
        }

        // Check for a MAIN procedure.
        let main_proc = program.statements.iter().find_map(|stmt| {
            if let Statement::Procedure(proc) = stmt {
                if proc.options.iter().any(|o| o.eq_ignore_ascii_case("MAIN")) {
                    return Some(proc.name.clone());
                }
            }
            None
        });

        if let Some(main_name) = main_proc {
            // Execute MAIN procedure.
            self.exec_call(&main_name, &[])?;
        } else {
            // Execute top-level statements.
            for stmt in &program.statements {
                match self.exec_stmt(stmt)? {
                    StmtResult::Ok => {}
                    StmtResult::Return(_) => break,
                    StmtResult::Leave(_) | StmtResult::Iterate(_) => break,
                    StmtResult::GoTo(_) => break,
                }
            }
        }

        Ok(())
    }

    /// Get captured output.
    pub fn output(&self) -> &[String] {
        &self.output
    }

    /// Get a variable's value by name (case-insensitive).
    pub fn get_var(&self, name: &str) -> Option<&PliValue> {
        let upper = name.to_uppercase();
        for scope in self.scopes.iter().rev() {
            if let Some((_, val)) = scope.vars.get(&upper) {
                return Some(val);
            }
        }
        None
    }

    /// Set a variable's value.
    pub fn set_var(&mut self, name: &str, pli_type: PliType, value: PliValue) {
        let upper = name.to_uppercase();
        // Look for existing binding in current or parent scopes.
        for scope in self.scopes.iter_mut().rev() {
            if let std::collections::hash_map::Entry::Occupied(mut e) = scope.vars.entry(upper.clone()) {
                e.insert((pli_type, value));
                return;
            }
        }
        // Create in current scope.
        if let Some(scope) = self.scopes.last_mut() {
            scope.vars.insert(upper, (pli_type, value));
        }
    }

    fn get_var_type(&self, name: &str) -> Option<&PliType> {
        let upper = name.to_uppercase();
        for scope in self.scopes.iter().rev() {
            if let Some((t, _)) = scope.vars.get(&upper) {
                return Some(t);
            }
        }
        None
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    // ─────── Statement execution ───────

    fn exec_stmt(&mut self, stmt: &Statement) -> Result<StmtResult, InterpreterError> {
        match stmt {
            Statement::Procedure(proc) => {
                // Already collected; skip execution at top level.
                self.procedures
                    .insert(proc.name.to_uppercase(), proc.clone());
                Ok(StmtResult::Ok)
            }
            Statement::Declare(dcl) => {
                self.exec_declare(dcl)?;
                Ok(StmtResult::Ok)
            }
            Statement::Assignment(assign) => {
                self.exec_assignment(assign)?;
                Ok(StmtResult::Ok)
            }
            Statement::If(if_stmt) => self.exec_if(if_stmt),
            Statement::Do(do_stmt) => self.exec_do(do_stmt),
            Statement::Select(sel) => self.exec_select(sel),
            Statement::Call(call) => {
                self.exec_call(&call.name, &call.args)?;
                Ok(StmtResult::Ok)
            }
            Statement::Return(ret) => {
                let val = if let Some(expr) = &ret.value {
                    Some(self.eval_expr(expr)?)
                } else {
                    None
                };
                Ok(StmtResult::Return(val))
            }
            Statement::GoTo(goto) => Ok(StmtResult::GoTo(goto.label.to_uppercase())),
            Statement::Leave(leave) => {
                Ok(StmtResult::Leave(leave.label.as_ref().map(|l| l.to_uppercase())))
            }
            Statement::Iterate(iter) => {
                Ok(StmtResult::Iterate(iter.label.as_ref().map(|l| l.to_uppercase())))
            }
            Statement::Put(put) => {
                self.exec_put(put)?;
                Ok(StmtResult::Ok)
            }
            Statement::Display(disp) => {
                let val = self.eval_expr(&disp.expr)?;
                self.output.push(val.to_string_value());
                Ok(StmtResult::Ok)
            }
            Statement::Stop => Err(InterpreterError::Stop),
            Statement::Exit => Err(InterpreterError::Exit),
            Statement::Null | Statement::End(_) => Ok(StmtResult::Ok),
            Statement::Labeled(label, inner) => {
                // Store the label for GOTO (simplified — we don't support full GOTO yet).
                let _ = label;
                self.exec_stmt(inner)
            }
            Statement::BeginBlock(block) => {
                self.push_scope();
                let result = self.exec_block(&block.body)?;
                self.pop_scope();
                Ok(result)
            }
            Statement::Allocate(alloc) => {
                // Simplified: just declare the variable if not already.
                let upper = alloc.name.to_uppercase();
                if self.get_var(&upper).is_none() {
                    self.set_var(&upper, PliType::Pointer, PliValue::Pointer(0));
                }
                Ok(StmtResult::Ok)
            }
            Statement::Free(_) => Ok(StmtResult::Ok),
            Statement::On(_) | Statement::Signal(_) | Statement::Revert(_) => {
                // Exception handling — deferred to P105.
                Ok(StmtResult::Ok)
            }
            Statement::Get(_)
            | Statement::Open(_)
            | Statement::Close(_)
            | Statement::Read(_)
            | Statement::Write(_) => {
                // I/O — simplified stubs.
                Ok(StmtResult::Ok)
            }
            Statement::Preprocessor(_) => Ok(StmtResult::Ok),
        }
    }

    fn exec_block(&mut self, stmts: &[Statement]) -> Result<StmtResult, InterpreterError> {
        for stmt in stmts {
            match self.exec_stmt(stmt)? {
                StmtResult::Ok => {}
                other => return Ok(other),
            }
        }
        Ok(StmtResult::Ok)
    }

    fn exec_declare(&mut self, dcl: &DeclareStmt) -> Result<(), InterpreterError> {
        for item in &dcl.items {
            let pli_type = self.resolve_data_type(&item.data_type);
            let upper = item.name.to_uppercase();

            if let Some(init) = &item.initial {
                // Explicit INIT always sets the value.
                let val = self.eval_expr(init)?;
                let converted = convert_value(&val, &pli_type).unwrap_or(val);
                self.set_var(&upper, pli_type, converted);
            } else if self.get_var(&upper).is_some() {
                // Variable already exists (e.g. bound as parameter) — just update type.
                // Don't overwrite the existing value.
                if let Some(scope) = self.scopes.last_mut() {
                    if let Some(entry) = scope.vars.get_mut(&upper) {
                        // Convert existing value to declared type if possible.
                        if let Ok(converted) = convert_value(&entry.1, &pli_type) {
                            *entry = (pli_type, converted);
                        } else {
                            entry.0 = pli_type;
                        }
                    }
                }
            } else {
                // New variable with default value.
                let value = self.default_value(&pli_type);
                self.set_var(&upper, pli_type, value);
            }
        }
        Ok(())
    }

    fn resolve_data_type(&self, ast_type: &Option<DataType>) -> PliType {
        match ast_type {
            Some(DataType::FixedDecimal(p, s)) => PliType::FixedDecimal(*p, *s),
            Some(DataType::FixedBinary(p, s)) => PliType::FixedBinary(*p, *s),
            Some(DataType::FloatDecimal(p)) => PliType::FloatDecimal(*p),
            Some(DataType::FloatBinary(p)) => PliType::FloatBinary(*p),
            Some(DataType::Character(n)) => PliType::Character(*n),
            Some(DataType::CharacterVarying(n)) => PliType::CharacterVarying(*n),
            Some(DataType::CharacterStar) => PliType::CharacterVarying(32767),
            Some(DataType::Bit(n)) => PliType::Bit(*n),
            Some(DataType::BitVarying(n)) => PliType::BitVarying(*n),
            Some(DataType::Graphic(n)) => PliType::Graphic(*n),
            Some(DataType::Widechar(n)) => PliType::Widechar(*n),
            Some(DataType::Pointer) => PliType::Pointer,
            Some(DataType::Offset) => PliType::Offset,
            Some(DataType::Handle(s)) => PliType::Handle(s.clone()),
            Some(DataType::Area(n)) => PliType::Area(*n),
            Some(DataType::Label) => PliType::Label,
            Some(DataType::Entry) => PliType::Entry,
            Some(DataType::File) => PliType::File,
            Some(DataType::Format) => PliType::Format,
            Some(DataType::Picture(s)) => PliType::Picture(s.clone()),
            Some(DataType::Task) | Some(DataType::Event) => PliType::FixedBinary(31, 0),
            Some(DataType::Ordinal(_)) => PliType::FixedBinary(31, 0),
            Some(DataType::Union) => PliType::FixedBinary(31, 0),
            Some(DataType::Structure) => PliType::FixedBinary(31, 0),
            None => PliType::FixedDecimal(15, 0), // PL/I default
        }
    }

    fn default_value(&self, pli_type: &PliType) -> PliValue {
        match pli_type {
            PliType::FixedDecimal(p, s) => PliValue::FixedDecimal {
                raw: 0,
                precision: *p,
                scale: *s,
            },
            PliType::FixedBinary(..) => PliValue::FixedBinary(0),
            PliType::FloatDecimal(..) => PliValue::FloatDecimal(0.0),
            PliType::FloatBinary(..) => PliValue::FloatBinary(0.0),
            PliType::Character(n) => PliValue::Character(" ".repeat(*n as usize)),
            PliType::CharacterVarying(..) => PliValue::Character(String::new()),
            PliType::Bit(n) => PliValue::Bit(vec![false; *n as usize]),
            PliType::BitVarying(..) => PliValue::Bit(Vec::new()),
            PliType::Graphic(n) | PliType::Widechar(n) => PliValue::Graphic(vec![0x0020; *n as usize]),
            PliType::Pointer | PliType::Offset | PliType::Handle(..) => PliValue::Null,
            PliType::Area(..) => PliValue::Null,
            PliType::Label => PliValue::Label(String::new()),
            PliType::Entry => PliValue::Entry(String::new()),
            PliType::File => PliValue::File(String::new()),
            PliType::Format => PliValue::Null,
            PliType::Picture(..) => PliValue::Character(String::new()),
            PliType::Structure(..) | PliType::Union(..) => PliValue::Null,
        }
    }

    fn exec_assignment(&mut self, assign: &AssignmentStmt) -> Result<(), InterpreterError> {
        let value = self.eval_expr(&assign.value)?;
        let name = self.expr_target_name(&assign.target)?;

        // Convert to target type if declared.
        let converted = if let Some(target_type) = self.get_var_type(&name).cloned() {
            convert_value(&value, &target_type).unwrap_or(value)
        } else {
            value.clone()
        };

        let pli_type = converted.pli_type();
        self.set_var(&name, pli_type, converted);
        Ok(())
    }

    fn expr_target_name(&self, expr: &Expr) -> Result<String, InterpreterError> {
        match expr {
            Expr::Ident(name) => Ok(name.to_uppercase()),
            Expr::Subscript(base, _) | Expr::PtrQual(base, _) => {
                // Walk through to find base identifier.
                let mut current = base.as_ref();
                loop {
                    match current {
                        Expr::Ident(name) => return Ok(name.to_uppercase()),
                        Expr::Subscript(inner, _) | Expr::PtrQual(inner, _) => {
                            current = inner.as_ref();
                        }
                        _ => {
                            return Err(InterpreterError::TypeError(
                                "invalid assignment target".to_string(),
                            ))
                        }
                    }
                }
            }
            _ => Err(InterpreterError::TypeError(
                "invalid assignment target".to_string(),
            )),
        }
    }

    // ─────── Control flow ───────

    fn exec_if(&mut self, if_stmt: &IfStmt) -> Result<StmtResult, InterpreterError> {
        let cond = self.eval_expr(&if_stmt.condition)?;
        if self.is_truthy(&cond) {
            self.exec_stmt(&if_stmt.then_stmt)
        } else if let Some(else_stmt) = &if_stmt.else_stmt {
            self.exec_stmt(else_stmt)
        } else {
            Ok(StmtResult::Ok)
        }
    }

    fn exec_do(&mut self, do_stmt: &DoStmt) -> Result<StmtResult, InterpreterError> {
        let loop_label = do_stmt.label.as_ref().map(|l| l.to_uppercase());

        match &do_stmt.control {
            None => {
                // Simple DO; END — execute body once.
                self.exec_do_body(&do_stmt.body, &loop_label)
            }
            Some(DoControl::While(cond_expr)) => {
                loop {
                    let cond = self.eval_expr(cond_expr)?;
                    if !self.is_truthy(&cond) {
                        break;
                    }
                    match self.exec_do_body(&do_stmt.body, &loop_label)? {
                        StmtResult::Ok | StmtResult::Iterate(_) => {}
                        StmtResult::Leave(_) => break,
                        other => return Ok(other),
                    }
                }
                Ok(StmtResult::Ok)
            }
            Some(DoControl::Until(cond_expr)) => {
                loop {
                    match self.exec_do_body(&do_stmt.body, &loop_label)? {
                        StmtResult::Ok | StmtResult::Iterate(_) => {}
                        StmtResult::Leave(_) => break,
                        other => return Ok(other),
                    }
                    let cond = self.eval_expr(cond_expr)?;
                    if self.is_truthy(&cond) {
                        break;
                    }
                }
                Ok(StmtResult::Ok)
            }
            Some(DoControl::Iterative {
                var,
                from,
                to,
                by,
                while_cond,
            }) => {
                let from_val = self.eval_expr(from)?;
                let to_val = self.eval_expr(to)?;
                let by_val = if let Some(by_expr) = by {
                    self.eval_expr(by_expr)?
                } else {
                    PliValue::FixedBinary(1)
                };

                let mut current = from_val.to_f64().unwrap_or(0.0);
                let to_f = to_val.to_f64().unwrap_or(0.0);
                let by_f = by_val.to_f64().unwrap_or(1.0);

                let var_upper = var.to_uppercase();

                loop {
                    // Check bounds.
                    if by_f > 0.0 && current > to_f {
                        break;
                    }
                    if by_f < 0.0 && current < to_f {
                        break;
                    }
                    if by_f == 0.0 {
                        break;
                    }

                    // Check WHILE condition if present.
                    if let Some(while_expr) = while_cond {
                        let w = self.eval_expr(while_expr)?;
                        if !self.is_truthy(&w) {
                            break;
                        }
                    }

                    // Set loop variable.
                    self.set_var(
                        &var_upper,
                        PliType::FixedBinary(31, 0),
                        PliValue::FixedBinary(current as i64),
                    );

                    match self.exec_do_body(&do_stmt.body, &loop_label)? {
                        StmtResult::Ok | StmtResult::Iterate(_) => {}
                        StmtResult::Leave(_) => break,
                        other => return Ok(other),
                    }

                    current += by_f;
                }
                Ok(StmtResult::Ok)
            }
            Some(DoControl::Repeat(expr)) => {
                loop {
                    match self.exec_do_body(&do_stmt.body, &loop_label)? {
                        StmtResult::Ok | StmtResult::Iterate(_) => {}
                        StmtResult::Leave(_) => break,
                        other => return Ok(other),
                    }
                    let val = self.eval_expr(expr)?;
                    if !self.is_truthy(&val) {
                        break;
                    }
                }
                Ok(StmtResult::Ok)
            }
        }
    }

    fn exec_do_body(
        &mut self,
        body: &[Statement],
        _loop_label: &Option<String>,
    ) -> Result<StmtResult, InterpreterError> {
        self.exec_block(body)
    }

    fn exec_select(&mut self, sel: &SelectStmt) -> Result<StmtResult, InterpreterError> {
        let sel_val = if let Some(expr) = &sel.expr {
            Some(self.eval_expr(expr)?)
        } else {
            None
        };

        for when in &sel.when_clauses {
            let matched = if let Some(ref sv) = sel_val {
                // SELECT(expr); WHEN(val1, val2) ...
                let mut found = false;
                for wv in &when.values {
                    let val = self.eval_expr(wv)?;
                    if self.values_equal(sv, &val) {
                        found = true;
                        break;
                    }
                }
                found
            } else {
                // SELECT; WHEN(condition) ...
                let cond = self.eval_expr(&when.values[0])?;
                self.is_truthy(&cond)
            };

            if matched {
                return self.exec_block(&when.body);
            }
        }

        // OTHERWISE.
        if let Some(ref otherwise) = sel.otherwise {
            return self.exec_block(otherwise);
        }

        Ok(StmtResult::Ok)
    }

    fn exec_put(&mut self, put: &PutStmt) -> Result<(), InterpreterError> {
        let mut parts = Vec::new();
        for item in &put.items {
            let val = self.eval_expr(item)?;
            parts.push(val.to_string_value());
        }

        if put.page {
            self.output.push("\n--- PAGE ---\n".to_string());
        }

        let line = parts.join(" ");
        self.output.push(line);
        Ok(())
    }

    // ─────── Procedure calls ───────

    fn exec_call(
        &mut self,
        name: &str,
        args: &[Expr],
    ) -> Result<Option<PliValue>, InterpreterError> {
        let upper = name.to_uppercase();

        // Check recursion depth.
        self.depth += 1;
        if self.depth > self.max_depth {
            self.depth -= 1;
            return Err(InterpreterError::StackOverflow(self.max_depth));
        }

        let proc = self
            .procedures
            .get(&upper)
            .cloned()
            .ok_or_else(|| InterpreterError::UndefinedProcedure(upper.clone()))?;

        // Evaluate arguments.
        let mut arg_vals = Vec::new();
        for arg in args {
            arg_vals.push(self.eval_expr(arg)?);
        }

        // Push scope and bind parameters directly in the new scope.
        self.push_scope();
        for (i, param_name) in proc.params.iter().enumerate() {
            let val = arg_vals.get(i).cloned().unwrap_or(PliValue::Null);
            let pli_type = val.pli_type();
            let upper = param_name.to_uppercase();
            // Always bind in current (innermost) scope for parameter isolation.
            if let Some(scope) = self.scopes.last_mut() {
                scope.vars.insert(upper, (pli_type, val));
            }
        }

        // Execute procedure body.
        let mut return_val = None;
        for stmt in &proc.body {
            match self.exec_stmt(stmt)? {
                StmtResult::Ok => {}
                StmtResult::Return(val) => {
                    return_val = val;
                    break;
                }
                StmtResult::Leave(_) | StmtResult::Iterate(_) => break,
                StmtResult::GoTo(_) => break,
            }
        }

        self.pop_scope();
        self.depth -= 1;

        Ok(return_val)
    }

    // ─────── Expression evaluation ───────

    pub fn eval_expr(&mut self, expr: &Expr) -> Result<PliValue, InterpreterError> {
        match expr {
            Expr::IntLit(n) => Ok(PliValue::FixedBinary(*n)),
            Expr::DecLit(s) => {
                // Parse decimal literal into FixedDecimal.
                let (raw, scale) = parse_decimal_lit(s);
                Ok(PliValue::FixedDecimal {
                    raw,
                    precision: 15,
                    scale,
                })
            }
            Expr::FloatLit(s) => {
                let f: f64 = s.parse().unwrap_or(0.0);
                Ok(PliValue::FloatDecimal(f))
            }
            Expr::StringLit(s) => {
                // Strip surrounding quotes.
                let inner = crate::lexer::string_value(s);
                Ok(PliValue::Character(inner))
            }
            Expr::BitLit(s) => {
                let bits = parse_bit_lit(s);
                Ok(PliValue::Bit(bits))
            }
            Expr::HexLit(s) => {
                let val = parse_hex_lit(s);
                Ok(PliValue::FixedBinary(val))
            }
            Expr::Ident(name) => {
                let upper = name.to_uppercase();
                self.get_var(&upper)
                    .cloned()
                    .ok_or(InterpreterError::UndefinedVariable(upper))
            }
            Expr::BinOp(left, op, right) => {
                let lval = self.eval_expr(left)?;
                let rval = self.eval_expr(right)?;
                self.eval_binop(&lval, *op, &rval)
            }
            Expr::UnaryOp(op, operand) => {
                let val = self.eval_expr(operand)?;
                self.eval_unaryop(*op, &val)
            }
            Expr::FuncCall(name, args) => {
                // Try built-in functions first, then user procedures.
                let upper = name.to_uppercase();
                if let Some(result) = self.try_builtin(&upper, args)? {
                    Ok(result)
                } else {
                    let result = self.exec_call(name, args)?;
                    result.ok_or_else(|| {
                        InterpreterError::TypeError(format!("function {name} returned no value"))
                    })
                }
            }
            Expr::Subscript(base, indices) => {
                // Simplified: treat as function call if base is ident.
                if let Expr::Ident(name) = base.as_ref() {
                    let upper = name.to_uppercase();
                    if let Some(result) = self.try_builtin(&upper, indices)? {
                        return Ok(result);
                    }
                    if self.procedures.contains_key(&upper) {
                        let result = self.exec_call(name, indices)?;
                        return result.ok_or_else(|| {
                            InterpreterError::TypeError(format!(
                                "function {name} returned no value"
                            ))
                        });
                    }
                }
                // Otherwise evaluate base.
                self.eval_expr(base)
            }
            Expr::PtrQual(base, _field) => {
                // Simplified: evaluate base.
                self.eval_expr(base)
            }
            Expr::Paren(inner) => self.eval_expr(inner),
        }
    }

    fn eval_binop(
        &self,
        left: &PliValue,
        op: BinOp,
        right: &PliValue,
    ) -> Result<PliValue, InterpreterError> {
        match op {
            // Concatenation.
            BinOp::Concat => {
                let ls = left.to_string_value();
                let rs = right.to_string_value();
                Ok(PliValue::Character(format!("{ls}{rs}")))
            }
            // Comparison operators.
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => {
                let result = self.compare_values(left, right, op)?;
                Ok(PliValue::Bit(vec![result]))
            }
            // Logical operators.
            BinOp::And => {
                let result = self.is_truthy(left) && self.is_truthy(right);
                Ok(PliValue::Bit(vec![result]))
            }
            BinOp::Or => {
                let result = self.is_truthy(left) || self.is_truthy(right);
                Ok(PliValue::Bit(vec![result]))
            }
            // Arithmetic.
            _ => {
                let lf = left.to_f64().ok_or_else(|| {
                    InterpreterError::TypeError("non-numeric left operand".to_string())
                })?;
                let rf = right.to_f64().ok_or_else(|| {
                    InterpreterError::TypeError("non-numeric right operand".to_string())
                })?;

                let result = match op {
                    BinOp::Add => lf + rf,
                    BinOp::Sub => lf - rf,
                    BinOp::Mul => lf * rf,
                    BinOp::Div => {
                        if rf == 0.0 {
                            return Err(InterpreterError::DivisionByZero);
                        }
                        lf / rf
                    }
                    BinOp::Power => lf.powf(rf),
                    BinOp::Mod => {
                        if rf == 0.0 {
                            return Err(InterpreterError::DivisionByZero);
                        }
                        lf % rf
                    }
                    _ => unreachable!(),
                };

                // Determine result type based on PL/I rules.
                let lt = left.pli_type();
                let rt = right.pli_type();
                let result_type = arithmetic_result_type(&lt, &rt);

                match result_type {
                    PliType::FixedBinary(..) => Ok(PliValue::FixedBinary(result as i64)),
                    PliType::FixedDecimal(p, s) => {
                        let scale_factor = 10_f64.powi(s as i32);
                        Ok(PliValue::FixedDecimal {
                            raw: (result * scale_factor).round() as i128,
                            precision: p,
                            scale: s,
                        })
                    }
                    PliType::FloatBinary(..) => Ok(PliValue::FloatBinary(result)),
                    _ => Ok(PliValue::FloatDecimal(result)),
                }
            }
        }
    }

    fn eval_unaryop(
        &self,
        op: UnaryOp,
        val: &PliValue,
    ) -> Result<PliValue, InterpreterError> {
        match op {
            UnaryOp::Neg => {
                let f = val.to_f64().ok_or_else(|| {
                    InterpreterError::TypeError("non-numeric operand for negation".to_string())
                })?;
                match val {
                    PliValue::FixedBinary(n) => Ok(PliValue::FixedBinary(-n)),
                    PliValue::FixedDecimal {
                        raw,
                        precision,
                        scale,
                    } => Ok(PliValue::FixedDecimal {
                        raw: -raw,
                        precision: *precision,
                        scale: *scale,
                    }),
                    _ => Ok(PliValue::FloatDecimal(-f)),
                }
            }
            UnaryOp::Pos => Ok(val.clone()),
            UnaryOp::Not => {
                let result = !self.is_truthy(val);
                Ok(PliValue::Bit(vec![result]))
            }
        }
    }

    fn compare_values(
        &self,
        left: &PliValue,
        right: &PliValue,
        op: BinOp,
    ) -> Result<bool, InterpreterError> {
        // Try numeric comparison first.
        if let (Some(lf), Some(rf)) = (left.to_f64(), right.to_f64()) {
            return Ok(match op {
                BinOp::Eq => (lf - rf).abs() < f64::EPSILON,
                BinOp::Ne => (lf - rf).abs() >= f64::EPSILON,
                BinOp::Lt => lf < rf,
                BinOp::Gt => lf > rf,
                BinOp::Le => lf <= rf,
                BinOp::Ge => lf >= rf,
                _ => false,
            });
        }

        // String comparison.
        let ls = left.to_string_value();
        let rs = right.to_string_value();
        Ok(match op {
            BinOp::Eq => ls == rs,
            BinOp::Ne => ls != rs,
            BinOp::Lt => ls < rs,
            BinOp::Gt => ls > rs,
            BinOp::Le => ls <= rs,
            BinOp::Ge => ls >= rs,
            _ => false,
        })
    }

    fn values_equal(&self, a: &PliValue, b: &PliValue) -> bool {
        self.compare_values(a, b, BinOp::Eq).unwrap_or(false)
    }

    fn is_truthy(&self, val: &PliValue) -> bool {
        match val {
            PliValue::FixedBinary(n) => *n != 0,
            PliValue::FixedDecimal { raw, .. } => *raw != 0,
            PliValue::FloatDecimal(f) | PliValue::FloatBinary(f) => *f != 0.0,
            PliValue::Character(s) => !s.is_empty() && s.trim() != "0" && !s.trim().is_empty(),
            PliValue::Bit(bits) => bits.iter().any(|b| *b),
            PliValue::Null => false,
            _ => true,
        }
    }

    // ─────── Built-in functions (minimal set for P102) ───────

    fn try_builtin(
        &mut self,
        name: &str,
        args: &[Expr],
    ) -> Result<Option<PliValue>, InterpreterError> {
        match name {
            "MOD" if args.len() == 2 => {
                let a = self.eval_expr(&args[0])?;
                let b = self.eval_expr(&args[1])?;
                let af = a.to_f64().unwrap_or(0.0);
                let bf = b.to_f64().unwrap_or(0.0);
                if bf == 0.0 {
                    return Err(InterpreterError::DivisionByZero);
                }
                Ok(Some(PliValue::FixedBinary((af % bf) as i64)))
            }
            "ABS" if args.len() == 1 => {
                let a = self.eval_expr(&args[0])?;
                match &a {
                    PliValue::FixedBinary(n) => Ok(Some(PliValue::FixedBinary(n.abs()))),
                    PliValue::FixedDecimal {
                        raw,
                        precision,
                        scale,
                    } => Ok(Some(PliValue::FixedDecimal {
                        raw: raw.abs(),
                        precision: *precision,
                        scale: *scale,
                    })),
                    _ => {
                        let f = a.to_f64().unwrap_or(0.0);
                        Ok(Some(PliValue::FloatDecimal(f.abs())))
                    }
                }
            }
            "MAX" if args.len() >= 2 => {
                let mut max = self.eval_expr(&args[0])?.to_f64().unwrap_or(f64::MIN);
                for arg in &args[1..] {
                    let v = self.eval_expr(arg)?.to_f64().unwrap_or(f64::MIN);
                    if v > max {
                        max = v;
                    }
                }
                Ok(Some(PliValue::FloatDecimal(max)))
            }
            "MIN" if args.len() >= 2 => {
                let mut min = self.eval_expr(&args[0])?.to_f64().unwrap_or(f64::MAX);
                for arg in &args[1..] {
                    let v = self.eval_expr(arg)?.to_f64().unwrap_or(f64::MAX);
                    if v < min {
                        min = v;
                    }
                }
                Ok(Some(PliValue::FloatDecimal(min)))
            }
            "LENGTH" if args.len() == 1 => {
                let a = self.eval_expr(&args[0])?;
                let len = match &a {
                    PliValue::Character(s) => s.len() as i64,
                    PliValue::Bit(bits) => bits.len() as i64,
                    _ => a.to_string_value().len() as i64,
                };
                Ok(Some(PliValue::FixedBinary(len)))
            }
            "SUBSTR" if args.len() >= 2 => {
                let a = self.eval_expr(&args[0])?;
                let start = self.eval_expr(&args[1])?.to_i64().unwrap_or(1) as usize;
                let s = a.to_string_value();
                let len = if args.len() >= 3 {
                    self.eval_expr(&args[2])?.to_i64().unwrap_or(s.len() as i64) as usize
                } else {
                    s.len().saturating_sub(start.saturating_sub(1))
                };
                let start_idx = start.saturating_sub(1); // PL/I is 1-based
                let end_idx = (start_idx + len).min(s.len());
                let result = if start_idx < s.len() {
                    s[start_idx..end_idx].to_string()
                } else {
                    String::new()
                };
                Ok(Some(PliValue::Character(result)))
            }
            "INDEX" if args.len() == 2 => {
                let haystack = self.eval_expr(&args[0])?.to_string_value();
                let needle = self.eval_expr(&args[1])?.to_string_value();
                let pos = haystack
                    .find(&needle)
                    .map(|i| i as i64 + 1) // PL/I is 1-based
                    .unwrap_or(0);
                Ok(Some(PliValue::FixedBinary(pos)))
            }
            "TRIM" if args.len() == 1 => {
                let a = self.eval_expr(&args[0])?;
                let s = a.to_string_value();
                Ok(Some(PliValue::Character(s.trim().to_string())))
            }
            _ => Ok(None),
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
//  Literal parsing helpers
// ---------------------------------------------------------------------------

fn parse_decimal_lit(s: &str) -> (i128, u8) {
    if let Some(dot_pos) = s.find('.') {
        let scale = (s.len() - dot_pos - 1) as u8;
        let digits: String = s.chars().filter(|c| *c != '.').collect();
        let raw: i128 = digits.parse().unwrap_or(0);
        (raw, scale)
    } else {
        let raw: i128 = s.parse().unwrap_or(0);
        (raw, 0)
    }
}

fn parse_bit_lit(s: &str) -> Vec<bool> {
    // Remove quotes and B suffix: '10110000'B → 10110000
    let inner = s
        .trim_end_matches('B')
        .trim_end_matches('b')
        .trim_matches('\'');
    inner.chars().map(|c| c == '1').collect()
}

fn parse_hex_lit(s: &str) -> i64 {
    // Remove X prefix/suffix and quotes.
    let inner = s
        .trim_start_matches('X')
        .trim_start_matches('x')
        .trim_end_matches('X')
        .trim_end_matches('x')
        .trim_matches('\'');
    i64::from_str_radix(inner, 16).unwrap_or(0)
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn run(src: &str) -> Interpreter {
        let tokens = Lexer::tokenize(src).unwrap();
        let program = Parser::parse(tokens).unwrap();
        let mut interp = Interpreter::new();
        let _ = interp.run(&program);
        interp
    }

    fn run_get(src: &str, var: &str) -> PliValue {
        let interp = run(src);
        interp.get_var(var).cloned().unwrap()
    }

    // ─── P102.1: Expression evaluator with type coercion ───

    #[test]
    fn test_simple_assignment() {
        let val = run_get("DCL X FIXED BIN(31); X = 42;", "X");
        assert_eq!(val, PliValue::FixedBinary(42));
    }

    #[test]
    fn test_arithmetic_add() {
        let val = run_get("DCL X FIXED BIN(31); X = 10 + 20;", "X");
        assert_eq!(val, PliValue::FixedBinary(30));
    }

    #[test]
    fn test_arithmetic_precedence() {
        // X = 2 + 3 * 4 should be 14, not 20.
        let val = run_get("DCL X FIXED BIN(31); X = 2 + 3 * 4;", "X");
        assert_eq!(val, PliValue::FixedBinary(14));
    }

    #[test]
    fn test_arithmetic_power() {
        let val = run_get("DCL X FIXED BIN(31); X = 2 ** 10;", "X");
        assert_eq!(val, PliValue::FixedBinary(1024));
    }

    #[test]
    fn test_mixed_type_arithmetic() {
        // FIXED DEC + FIXED BIN → promoted to FIXED BIN (binary wins).
        let val = run_get(
            "DCL A FIXED DEC(5) INIT(10); DCL B FIXED BIN(15) INIT(20); DCL X FIXED BIN(31); X = A + B;",
            "X",
        );
        assert_eq!(val, PliValue::FixedBinary(30));
    }

    #[test]
    fn test_string_concatenation() {
        let val = run_get(
            "DCL X CHAR(20) VARYING; X = 'Hello' || ' ' || 'World';",
            "X",
        );
        if let PliValue::Character(s) = val {
            assert_eq!(s, "Hello World");
        } else {
            panic!("Expected Character");
        }
    }

    #[test]
    fn test_string_to_numeric_conversion() {
        let val = run_get(
            "DCL X FIXED BIN(31); X = '  123  ';",
            "X",
        );
        // Character '  123  ' should convert to 123.
        assert_eq!(val, PliValue::FixedBinary(123));
    }

    #[test]
    fn test_numeric_to_string_conversion() {
        let val = run_get(
            "DCL X CHAR(10); X = 42;",
            "X",
        );
        if let PliValue::Character(s) = val {
            assert_eq!(s.len(), 10);
            assert!(s.starts_with("42"));
        } else {
            panic!("Expected Character");
        }
    }

    #[test]
    fn test_unary_negation() {
        let val = run_get("DCL X FIXED BIN(31); X = -5;", "X");
        assert_eq!(val, PliValue::FixedBinary(-5));
    }

    #[test]
    fn test_comparison_ops() {
        let val = run_get("DCL X FIXED BIN(31); IF 5 > 3 THEN X = 1; ELSE X = 0;", "X");
        assert_eq!(val, PliValue::FixedBinary(1));
    }

    // ─── P102.2: Control flow ───

    #[test]
    fn test_if_then_else_true() {
        let val = run_get(
            "DCL X FIXED BIN(31) INIT(10); DCL Y FIXED BIN(31); IF X > 5 THEN Y = 1; ELSE Y = 0;",
            "Y",
        );
        assert_eq!(val, PliValue::FixedBinary(1));
    }

    #[test]
    fn test_if_then_else_false() {
        let val = run_get(
            "DCL X FIXED BIN(31) INIT(3); DCL Y FIXED BIN(31); IF X > 5 THEN Y = 1; ELSE Y = 0;",
            "Y",
        );
        assert_eq!(val, PliValue::FixedBinary(0));
    }

    #[test]
    fn test_do_iterative() {
        let val = run_get(
            "DCL SUM FIXED BIN(31) INIT(0); DO I = 1 TO 10; SUM = SUM + I; END;",
            "SUM",
        );
        assert_eq!(val, PliValue::FixedBinary(55)); // 1+2+...+10 = 55
    }

    #[test]
    fn test_do_iterative_with_by() {
        let val = run_get(
            "DCL SUM FIXED BIN(31) INIT(0); DO I = 1 TO 10 BY 2; SUM = SUM + I; END;",
            "SUM",
        );
        assert_eq!(val, PliValue::FixedBinary(25)); // 1+3+5+7+9 = 25
    }

    #[test]
    fn test_do_while() {
        let val = run_get(
            "DCL X FIXED BIN(31) INIT(10); DCL COUNT FIXED BIN(31) INIT(0); DO WHILE (X > 0); X = X - 1; COUNT = COUNT + 1; END;",
            "COUNT",
        );
        assert_eq!(val, PliValue::FixedBinary(10));
    }

    #[test]
    fn test_do_until() {
        let val = run_get(
            "DCL X FIXED BIN(31) INIT(0); DO UNTIL (X >= 5); X = X + 1; END;",
            "X",
        );
        assert_eq!(val, PliValue::FixedBinary(5));
    }

    #[test]
    fn test_select_with_expression() {
        let val = run_get(
            "DCL STATUS CHAR(1) INIT('B'); DCL RESULT FIXED BIN(31) INIT(0); SELECT (STATUS); WHEN ('A') RESULT = 1; WHEN ('B') RESULT = 2; WHEN ('C') RESULT = 3; END;",
            "RESULT",
        );
        assert_eq!(val, PliValue::FixedBinary(2));
    }

    #[test]
    fn test_select_otherwise() {
        let val = run_get(
            "DCL STATUS CHAR(1) INIT('Z'); DCL RESULT FIXED BIN(31) INIT(0); SELECT (STATUS); WHEN ('A') RESULT = 1; OTHERWISE RESULT = 99; END;",
            "RESULT",
        );
        assert_eq!(val, PliValue::FixedBinary(99));
    }

    #[test]
    fn test_nested_if_do() {
        let val = run_get(
            "DCL SUM FIXED BIN(31) INIT(0); DO I = 1 TO 20; IF MOD(I, 2) = 0 THEN SUM = SUM + I; END;",
            "SUM",
        );
        assert_eq!(val, PliValue::FixedBinary(110)); // 2+4+6+...+20 = 110
    }

    // ─── P102.3: Procedures ───

    #[test]
    fn test_procedure_call() {
        let val = run_get(
            "DCL RESULT FIXED BIN(31); ADD_NUMS: PROC(A, B) RETURNS(FIXED BIN(31)); DCL A FIXED BIN(31); DCL B FIXED BIN(31); RETURN(A + B); END ADD_NUMS; RESULT = ADD_NUMS(10, 20);",
            "RESULT",
        );
        assert_eq!(val, PliValue::FixedBinary(30));
    }

    #[test]
    fn test_recursive_factorial() {
        let val = run_get(
            "DCL RESULT FIXED BIN(31); FACTORIAL: PROC(N) RETURNS(FIXED BIN(31)); DCL N FIXED BIN(31); IF N <= 1 THEN RETURN(1); RETURN(N * FACTORIAL(N - 1)); END FACTORIAL; RESULT = FACTORIAL(10);",
            "RESULT",
        );
        assert_eq!(val, PliValue::FixedBinary(3628800)); // 10! = 3628800
    }

    #[test]
    fn test_recursive_fibonacci() {
        let val = run_get(
            "DCL RESULT FIXED BIN(31); FIB: PROC(N) RETURNS(FIXED BIN(31)); DCL N FIXED BIN(31); IF N <= 1 THEN RETURN(N); RETURN(FIB(N - 1) + FIB(N - 2)); END FIB; RESULT = FIB(10);",
            "RESULT",
        );
        assert_eq!(val, PliValue::FixedBinary(55)); // fib(10) = 55
    }

    #[test]
    fn test_call_statement() {
        let interp = run(
            "DCL X FIXED BIN(31) INIT(0); SETX: PROC(VAL); DCL VAL FIXED BIN(31); X = VAL; END SETX; CALL SETX(42);",
        );
        // Note: X is set within procedure but since it uses set_var which looks up scopes,
        // it should set the outer X.
        let val = interp.get_var("X").cloned().unwrap();
        assert_eq!(val, PliValue::FixedBinary(42));
    }

    // ─── PUT LIST output ───

    #[test]
    fn test_put_list() {
        let interp = run("PUT LIST('Hello, World!');");
        assert_eq!(interp.output().len(), 1);
        assert_eq!(interp.output()[0], "Hello, World!");
    }

    #[test]
    fn test_put_list_multiple() {
        let interp = run("DCL X FIXED BIN(31) INIT(42); PUT LIST('Value is', X);");
        assert_eq!(interp.output().len(), 1);
        assert!(interp.output()[0].contains("42"));
    }

    // ─── Built-in functions ───

    #[test]
    fn test_builtin_abs() {
        let val = run_get("DCL X FIXED BIN(31); X = ABS(-42);", "X");
        assert_eq!(val, PliValue::FixedBinary(42));
    }

    #[test]
    fn test_builtin_mod() {
        let val = run_get("DCL X FIXED BIN(31); X = MOD(17, 5);", "X");
        assert_eq!(val, PliValue::FixedBinary(2));
    }

    #[test]
    fn test_builtin_length() {
        let val = run_get("DCL X FIXED BIN(31); X = LENGTH('Hello');", "X");
        assert_eq!(val, PliValue::FixedBinary(5));
    }

    #[test]
    fn test_builtin_substr() {
        let val = run_get("DCL X CHAR(10) VARYING; X = SUBSTR('Hello World', 7, 5);", "X");
        if let PliValue::Character(s) = val {
            assert_eq!(s, "World");
        } else {
            panic!("Expected Character");
        }
    }

    #[test]
    fn test_builtin_index() {
        let val = run_get("DCL X FIXED BIN(31); X = INDEX('Hello World', 'World');", "X");
        assert_eq!(val, PliValue::FixedBinary(7));
    }

    #[test]
    fn test_builtin_max() {
        let val = run_get("DCL X FIXED BIN(31); X = MAX(5, 10, 3);", "X");
        // MAX returns FloatDecimal, assigned to FIXED BIN → converted
        assert_eq!(val, PliValue::FixedBinary(10));
    }

    #[test]
    fn test_hello_world_program() {
        let src = "HELLO: PROC OPTIONS(MAIN); PUT LIST('Hello, World!'); END HELLO;";
        let interp = run(src);
        assert_eq!(interp.output().len(), 1);
        assert_eq!(interp.output()[0], "Hello, World!");
    }

    #[test]
    fn test_display_statement() {
        let interp = run("DISPLAY('Test message');");
        assert_eq!(interp.output().len(), 1);
        assert_eq!(interp.output()[0], "Test message");
    }

    #[test]
    fn test_begin_block_scoping() {
        let val = run_get(
            "DCL X FIXED BIN(31) INIT(1); BEGIN; DCL Y FIXED BIN(31) INIT(99); X = Y; END;",
            "X",
        );
        assert_eq!(val, PliValue::FixedBinary(99));
    }
}
