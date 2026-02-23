// SPDX-License-Identifier: Apache-2.0
//! NAT-102: Interpreter Core for Natural programs.
//!
//! Provides the execution engine with variable pool, program counter,
//! call stack, and support for all control-flow constructs including
//! PERFORM, CALLNAT, FETCH, and STACK operations.

use std::collections::HashMap;

use crate::data_model::{NaturalValue, VariablePool};
use crate::parser::{BinOp, EscapeKind, Expr, Program, StackPosition, Statement};

// ---------------------------------------------------------------------------
// Program objects
// ---------------------------------------------------------------------------

/// Type of Natural object.
#[derive(Debug, Clone, PartialEq)]
pub enum ObjectType {
    Program,
    Subprogram,
    Subroutine,
    Helproutine,
    Map,
    Copycode,
    Gda,
    Lda,
    Pda,
}

/// A loaded Natural program object.
#[derive(Debug, Clone)]
pub struct NaturalObject {
    pub name: String,
    pub object_type: ObjectType,
    pub program: Program,
}

// ---------------------------------------------------------------------------
// Call stack frame
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
struct CallFrame {
    #[allow(dead_code)]
    program_name: String,
    #[allow(dead_code)]
    return_pc: usize,
    local_vars: VariablePool,
}

// ---------------------------------------------------------------------------
// Interpreter
// ---------------------------------------------------------------------------

/// Error during interpretation.
#[derive(Debug, thiserror::Error)]
pub enum InterpreterError {
    #[error("undefined variable: {0}")]
    UndefinedVariable(String),
    #[error("subroutine not found: {0}")]
    SubroutineNotFound(String),
    #[error("program not found: {0}")]
    ProgramNotFound(String),
    #[error("ESCAPE ROUTINE with no active subroutine")]
    EscapeRoutineNoSub,
    #[error("division by zero")]
    DivisionByZero,
    #[error("type mismatch: {0}")]
    TypeMismatch(String),
    #[error("runtime error {code} at line {line}: {message}")]
    RuntimeError { code: u32, line: usize, message: String },
    #[error("data model error: {0}")]
    DataModel(#[from] crate::data_model::DataModelError),
}

/// Signals for interpreter control flow.
#[derive(Debug)]
enum ControlSignal {
    None,
    EscapeTop,
    EscapeBottom,
    EscapeRoutine,
    Return,
}

/// The Natural interpreter.
pub struct NaturalInterpreter {
    pub variables: VariablePool,
    pub output_lines: Vec<String>,
    pub data_stack: Vec<NaturalValue>,
    call_stack: Vec<CallFrame>,
    subroutines: HashMap<String, Vec<Statement>>,
    programs: HashMap<String, NaturalObject>,
    level: u32,
    pub error_nr: u32,
    pub error_line: usize,
}

impl NaturalInterpreter {
    pub fn new() -> Self {
        Self {
            variables: VariablePool::new(),
            output_lines: Vec::new(),
            data_stack: Vec::new(),
            call_stack: Vec::new(),
            subroutines: HashMap::new(),
            programs: HashMap::new(),
            level: 1,
            error_nr: 0,
            error_line: 0,
        }
    }

    /// Register a program that can be called via CALLNAT or FETCH.
    pub fn register_program(&mut self, obj: NaturalObject) {
        self.programs.insert(obj.name.clone(), obj);
    }

    /// Execute a program.
    pub fn execute(&mut self, program: &Program) -> Result<(), InterpreterError> {
        // Extract inline subroutines
        self.extract_subroutines(&program.statements);

        // Build variable pool from DEFINE DATA
        if let Some(dd) = &program.define_data {
            let pool = crate::data_model::build_variable_pool(&dd.sections)
                .map_err(InterpreterError::DataModel)?;
            // Merge into existing pool
            for name in pool.names() {
                if let Some(v) = pool.get(&name) {
                    self.variables.define(v.clone());
                }
            }
        }

        let signal = self.exec_statements(&program.statements)?;
        match signal {
            ControlSignal::EscapeRoutine => Err(InterpreterError::EscapeRoutineNoSub),
            _ => Ok(()),
        }
    }

    fn extract_subroutines(&mut self, statements: &[Statement]) {
        for stmt in statements {
            if let Statement::InlineSubroutine { name, body } = stmt {
                self.subroutines.insert(name.clone(), body.clone());
            }
        }
    }

    fn exec_statements(&mut self, statements: &[Statement]) -> Result<ControlSignal, InterpreterError> {
        for stmt in statements {
            if let Statement::InlineSubroutine { .. } = stmt {
                continue; // Skip subroutine definitions
            }
            let signal = self.exec_statement(stmt)?;
            match signal {
                ControlSignal::None => {}
                other => return Ok(other),
            }
        }
        Ok(ControlSignal::None)
    }

    fn exec_statement(&mut self, stmt: &Statement) -> Result<ControlSignal, InterpreterError> {
        match stmt {
            Statement::ComputeStmt { target, expr } => {
                let val = self.eval_expr(expr)?;
                self.variables.set_value(target, val);
            }
            Statement::MoveStmt { source, target } => {
                let val = self.eval_expr(source)?;
                self.variables.set_value(target, val);
            }
            Statement::MoveByNameStmt { source, target } => {
                self.variables.move_by_name(source, target);
            }
            Statement::DisplayStmt { items } | Statement::WriteStmt { items } | Statement::PrintStmt { items } => {
                let mut parts = Vec::new();
                for item in items {
                    let val = self.eval_expr(item)?;
                    parts.push(val.to_display_string());
                }
                self.output_lines.push(parts.join(" "));
            }
            Statement::IfStmt { condition, then_body, else_body } => {
                let cond = self.eval_bool(condition)?;
                let body = if cond { then_body } else { else_body };
                let signal = self.exec_statements(body)?;
                if !matches!(signal, ControlSignal::None) {
                    return Ok(signal);
                }
            }
            Statement::ForLoop { var, from, to, step, body } => {
                let from_val = self.eval_expr(from)?.to_i64();
                let to_val = self.eval_expr(to)?.to_i64();
                let step_val = step.as_ref().map(|s| self.eval_expr(s).map(|v| v.to_i64())).transpose()?.unwrap_or(1);
                let mut i = from_val;
                while (step_val > 0 && i <= to_val) || (step_val < 0 && i >= to_val) {
                    self.variables.set_value(var, NaturalValue::Integer(i));
                    let signal = self.exec_statements(body)?;
                    match signal {
                        ControlSignal::EscapeTop => continue,
                        ControlSignal::EscapeBottom => break,
                        ControlSignal::EscapeRoutine | ControlSignal::Return => return Ok(signal),
                        ControlSignal::None => {}
                    }
                    i += step_val;
                }
            }
            Statement::RepeatLoop { body, until } => {
                loop {
                    let signal = self.exec_statements(body)?;
                    match signal {
                        ControlSignal::EscapeTop => {}
                        ControlSignal::EscapeBottom => break,
                        ControlSignal::EscapeRoutine | ControlSignal::Return => return Ok(signal),
                        ControlSignal::None => {}
                    }
                    if let Some(cond) = until {
                        if self.eval_bool(cond)? {
                            break;
                        }
                    }
                }
            }
            Statement::DecideFor { conditions, none_body } => {
                let mut any_matched = false;
                for (cond, body) in conditions {
                    if self.eval_bool(cond)? {
                        any_matched = true;
                        let signal = self.exec_statements(body)?;
                        if !matches!(signal, ControlSignal::None) {
                            return Ok(signal);
                        }
                        break; // DECIDE FOR FIRST
                    }
                }
                if !any_matched && !none_body.is_empty() {
                    let signal = self.exec_statements(none_body)?;
                    if !matches!(signal, ControlSignal::None) {
                        return Ok(signal);
                    }
                }
            }
            Statement::DecideOn { expr, values, none_body } => {
                let val = self.eval_expr(expr)?;
                let mut any_matched = false;
                for (match_vals, body) in values {
                    for mv in match_vals {
                        let mv_val = self.eval_expr(mv)?;
                        if self.values_equal(&val, &mv_val) {
                            any_matched = true;
                            let signal = self.exec_statements(body)?;
                            if !matches!(signal, ControlSignal::None) {
                                return Ok(signal);
                            }
                            break;
                        }
                    }
                    if any_matched { break; }
                }
                if !any_matched && !none_body.is_empty() {
                    let signal = self.exec_statements(none_body)?;
                    if !matches!(signal, ControlSignal::None) {
                        return Ok(signal);
                    }
                }
            }
            Statement::EscapeStmt(kind) => {
                return Ok(match kind {
                    EscapeKind::Top => ControlSignal::EscapeTop,
                    EscapeKind::Bottom => ControlSignal::EscapeBottom,
                    EscapeKind::Routine => ControlSignal::EscapeRoutine,
                });
            }
            Statement::PerformStmt { name } => {
                if let Some(body) = self.subroutines.get(name).cloned() {
                    let signal = self.exec_statements(&body)?;
                    if matches!(signal, ControlSignal::EscapeRoutine) {
                        // Normal return from subroutine
                    } else if !matches!(signal, ControlSignal::None) {
                        return Ok(signal);
                    }
                } else {
                    return Err(InterpreterError::SubroutineNotFound(name.clone()));
                }
            }
            Statement::CallnatStmt { subprogram, args } => {
                let mut arg_vals = Vec::new();
                for arg in args {
                    arg_vals.push(self.eval_expr(arg)?);
                }
                self.exec_callnat(subprogram, &arg_vals)?;
            }
            Statement::FetchStmt { program } => {
                self.exec_fetch(program)?;
            }
            Statement::ReturnStmt => {
                return Ok(ControlSignal::Return);
            }
            Statement::StackStmt { position, data } => {
                let mut vals = Vec::new();
                for d in data {
                    vals.push(self.eval_expr(d)?);
                }
                match position {
                    StackPosition::Top => {
                        for v in vals.into_iter().rev() {
                            self.data_stack.insert(0, v);
                        }
                    }
                    StackPosition::Bottom => {
                        self.data_stack.extend(vals);
                    }
                }
            }
            Statement::OnErrorBlock { body } => {
                // Error handler registration — store for potential later use
                let _ = body;
            }
            Statement::CompressStmt { sources, into, leaving_space } => {
                let mut parts = Vec::new();
                for src in sources {
                    parts.push(self.eval_expr(src)?.to_display_string());
                }
                let sep = if *leaving_space { " " } else { "" };
                let result = parts.join(sep);
                self.variables.set_value(into, NaturalValue::Alpha(result));
            }
            Statement::SeparateStmt { source, into, delimiter } => {
                let src = self.eval_expr(source)?.to_display_string();
                let delim = delimiter.as_deref().unwrap_or(" ");
                let parts: Vec<&str> = src.split(delim).collect();
                for (i, var) in into.iter().enumerate() {
                    let val = parts.get(i).unwrap_or(&"").to_string();
                    self.variables.set_value(var, NaturalValue::Alpha(val));
                }
            }
            Statement::ExamineStmt { target, pattern, replace_with, giving_number, .. } => {
                let pat = self.eval_expr(pattern)?.to_display_string();
                let current = self.variables.get_value(target).to_display_string();
                let count = current.matches(&pat).count() as i64;

                if let Some(ref gn) = giving_number {
                    self.variables.set_value(gn, NaturalValue::Integer(count));
                }

                if let Some(repl_expr) = replace_with {
                    let repl = self.eval_expr(repl_expr)?.to_display_string();
                    let new_val = current.replace(&pat, &repl);
                    self.variables.set_value(target, NaturalValue::Alpha(new_val));
                }
            }
            Statement::Newpage => {
                self.output_lines.push("\x0C".to_string()); // form feed
            }
            Statement::EndTransaction | Statement::BackoutTransaction => {
                // Handled by adabas_access module
            }
            Statement::CommitStmt | Statement::RollbackStmt => {
                // Handled by sql_access module
            }

            // Database/report/IO statements are no-ops in the core interpreter
            // They are handled by the respective modules
            Statement::ReadStmt { body, .. }
            | Statement::FindStmt { body, .. }
            | Statement::HistogramStmt { body, .. }
            | Statement::SortStmt { body, .. }
            | Statement::AtBreak { body, .. }
            | Statement::AtTopOfPage { body, .. }
            | Statement::AtEndOfPage { body, .. } => {
                let signal = self.exec_statements(body)?;
                if !matches!(signal, ControlSignal::None) {
                    return Ok(signal);
                }
            }

            Statement::GetStmt { .. }
            | Statement::StoreStmt { .. }
            | Statement::UpdateStmt
            | Statement::DeleteStmt
            | Statement::InputStmt { .. }
            | Statement::ReinputStmt { .. }
            | Statement::SelectStmt { .. }
            | Statement::InsertStmt { .. }
            | Statement::SqlUpdate { .. }
            | Statement::SqlDelete { .. }
            | Statement::ReadWorkFile { .. }
            | Statement::WriteWorkFile { .. } => {
                // Handled by specialized modules
            }

            Statement::DefineDataStmt(_) | Statement::InlineSubroutine { .. } => {
                // Already processed
            }
        }
        Ok(ControlSignal::None)
    }

    fn exec_callnat(&mut self, name: &str, args: &[NaturalValue]) -> Result<(), InterpreterError> {
        let obj = self.programs.get(name).cloned()
            .ok_or_else(|| InterpreterError::ProgramNotFound(name.to_string()))?;

        // Save frame
        let frame = CallFrame {
            program_name: name.to_string(),
            return_pc: 0,
            local_vars: self.variables.clone(),
        };
        self.call_stack.push(frame);
        self.level += 1;

        // Set parameter variables
        if let Some(dd) = &obj.program.define_data {
            for section in &dd.sections {
                if section.kind == crate::parser::DataSectionKind::Parameter {
                    for (i, decl) in section.variables.iter().enumerate() {
                        if let Some(val) = args.get(i) {
                            self.variables.set_value(&decl.name, val.clone());
                        }
                    }
                }
            }
        }

        let result = self.exec_statements(&obj.program.statements);

        // Restore frame
        self.level -= 1;
        if let Some(frame) = self.call_stack.pop() {
            self.variables = frame.local_vars;
        }

        match result {
            Ok(ControlSignal::Return | ControlSignal::EscapeRoutine | ControlSignal::None) => Ok(()),
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        }
    }

    fn exec_fetch(&mut self, name: &str) -> Result<(), InterpreterError> {
        let obj = self.programs.get(name).cloned()
            .ok_or_else(|| InterpreterError::ProgramNotFound(name.to_string()))?;

        // Process any stacked data
        self.variables = VariablePool::new();
        self.level = 1;
        self.call_stack.clear();

        self.execute(&obj.program)?;
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Expression evaluation
    // -----------------------------------------------------------------------

    pub fn eval_expr(&self, expr: &Expr) -> Result<NaturalValue, InterpreterError> {
        match expr {
            Expr::IntLit(n) => Ok(NaturalValue::Integer(*n)),
            Expr::DecLit(s) => Ok(NaturalValue::Packed(s.clone())),
            Expr::StrLit(s) => Ok(NaturalValue::Alpha(s.clone())),
            Expr::BoolLit(b) => Ok(NaturalValue::Logical(*b)),
            Expr::Var(name) => Ok(self.variables.get_value(name)),
            Expr::SysVar(name) => Ok(self.get_system_variable(name)),
            Expr::UnaryMinus(e) => {
                let v = self.eval_expr(e)?;
                Ok(NaturalValue::Float(-v.to_f64()))
            }
            Expr::Not(e) => {
                let v = self.eval_bool(e)?;
                Ok(NaturalValue::Logical(!v))
            }
            Expr::BinOp { op, left, right } => {
                let lv = self.eval_expr(left)?;
                let rv = self.eval_expr(right)?;
                self.eval_binop(op, &lv, &rv)
            }
            Expr::FuncCall { name, args } => {
                let mut arg_vals = Vec::new();
                for a in args {
                    arg_vals.push(self.eval_expr(a)?);
                }
                self.eval_function(name, &arg_vals)
            }
        }
    }

    fn eval_binop(&self, op: &BinOp, lv: &NaturalValue, rv: &NaturalValue) -> Result<NaturalValue, InterpreterError> {
        match op {
            BinOp::Add => Ok(NaturalValue::Float(lv.to_f64() + rv.to_f64())),
            BinOp::Sub => Ok(NaturalValue::Float(lv.to_f64() - rv.to_f64())),
            BinOp::Mul => Ok(NaturalValue::Float(lv.to_f64() * rv.to_f64())),
            BinOp::Div => {
                let r = rv.to_f64();
                if r == 0.0 { return Err(InterpreterError::DivisionByZero); }
                Ok(NaturalValue::Float(lv.to_f64() / r))
            }
            BinOp::Mod => {
                let r = rv.to_i64();
                if r == 0 { return Err(InterpreterError::DivisionByZero); }
                Ok(NaturalValue::Integer(lv.to_i64() % r))
            }
            BinOp::Eq => Ok(NaturalValue::Logical(self.values_equal(lv, rv))),
            BinOp::Ne => Ok(NaturalValue::Logical(!self.values_equal(lv, rv))),
            BinOp::Lt => Ok(NaturalValue::Logical(lv.to_f64() < rv.to_f64())),
            BinOp::Le => Ok(NaturalValue::Logical(lv.to_f64() <= rv.to_f64())),
            BinOp::Gt => Ok(NaturalValue::Logical(lv.to_f64() > rv.to_f64())),
            BinOp::Ge => Ok(NaturalValue::Logical(lv.to_f64() >= rv.to_f64())),
            BinOp::And => Ok(NaturalValue::Logical(self.to_bool(lv) && self.to_bool(rv))),
            BinOp::Or => Ok(NaturalValue::Logical(self.to_bool(lv) || self.to_bool(rv))),
        }
    }

    fn eval_function(&self, name: &str, args: &[NaturalValue]) -> Result<NaturalValue, InterpreterError> {
        // Delegate to functions module — basic fallback here
        crate::functions::eval_builtin(name, args)
            .map_err(|e| InterpreterError::TypeMismatch(e.to_string()))
    }

    fn eval_bool(&self, expr: &Expr) -> Result<bool, InterpreterError> {
        let v = self.eval_expr(expr)?;
        Ok(self.to_bool(&v))
    }

    fn to_bool(&self, v: &NaturalValue) -> bool {
        match v {
            NaturalValue::Logical(b) => *b,
            NaturalValue::Integer(i) => *i != 0,
            NaturalValue::Float(f) => *f != 0.0,
            NaturalValue::Alpha(s) | NaturalValue::Unicode(s) => !s.is_empty(),
            NaturalValue::Null => false,
            _ => true,
        }
    }

    fn values_equal(&self, a: &NaturalValue, b: &NaturalValue) -> bool {
        match (a, b) {
            (NaturalValue::Alpha(s1), NaturalValue::Alpha(s2)) => s1 == s2,
            (NaturalValue::Unicode(s1), NaturalValue::Unicode(s2)) => s1 == s2,
            (NaturalValue::Alpha(s1), NaturalValue::Unicode(s2)) => s1 == s2,
            (NaturalValue::Unicode(s1), NaturalValue::Alpha(s2)) => s1 == s2,
            (NaturalValue::Logical(b1), NaturalValue::Logical(b2)) => b1 == b2,
            _ => a.to_f64() == b.to_f64(),
        }
    }

    fn get_system_variable(&self, name: &str) -> NaturalValue {
        crate::sysvars::get_system_variable(name, self)
    }
}

impl Default for NaturalInterpreter {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_natural;

    fn run(src: &str) -> NaturalInterpreter {
        let prog = parse_natural(src, "TEST").unwrap();
        let mut interp = NaturalInterpreter::new();
        interp.execute(&prog).unwrap();
        interp
    }

    #[test]
    fn test_compute_integer() {
        let interp = run("COMPUTE #X = 5 + 3");
        let val = interp.variables.get_value("#X");
        assert_eq!(val.to_f64(), 8.0);
    }

    #[test]
    fn test_compute_multiply() {
        let interp = run("COMPUTE #X = 4 * 3");
        assert_eq!(interp.variables.get_value("#X").to_f64(), 12.0);
    }

    #[test]
    fn test_move_string() {
        let interp = run("MOVE 'Hello' TO #GREETING");
        assert_eq!(interp.variables.get_value("#GREETING").to_display_string(), "Hello");
    }

    #[test]
    fn test_if_true_branch() {
        let interp = run("COMPUTE #X = 1\nIF #X = 1\n  DISPLAY 'yes'\nEND-IF");
        assert_eq!(interp.output_lines.len(), 1);
        assert_eq!(interp.output_lines[0], "yes");
    }

    #[test]
    fn test_if_false_branch() {
        let interp = run("COMPUTE #X = 2\nIF #X = 1\n  DISPLAY 'yes'\nELSE\n  DISPLAY 'no'\nEND-IF");
        assert_eq!(interp.output_lines[0], "no");
    }

    #[test]
    fn test_for_loop() {
        let interp = run("FOR #I = 1 TO 3\n  DISPLAY #I\nEND-FOR");
        assert_eq!(interp.output_lines.len(), 3);
    }

    #[test]
    fn test_repeat_until() {
        let interp = run("COMPUTE #X = 0\nREPEAT\n  COMPUTE #X = #X + 1\n  UNTIL #X = 3\nEND-REPEAT");
        assert_eq!(interp.variables.get_value("#X").to_i64(), 3);
    }

    #[test]
    fn test_escape_bottom() {
        let interp = run("FOR #I = 1 TO 10\n  IF #I = 3\n    ESCAPE BOTTOM\n  END-IF\nEND-FOR");
        assert_eq!(interp.variables.get_value("#I").to_i64(), 3);
    }

    #[test]
    fn test_escape_top() {
        let interp = run(
            "COMPUTE #SUM = 0\nFOR #I = 1 TO 5\n  IF #I = 3\n    ESCAPE TOP\n  END-IF\n  COMPUTE #SUM = #SUM + #I\nEND-FOR"
        );
        // 1 + 2 + 4 + 5 = 12 (skip 3)
        assert_eq!(interp.variables.get_value("#SUM").to_f64(), 12.0);
    }

    #[test]
    fn test_decide_for_first() {
        let src = "COMPUTE #X = 2\nDECIDE FOR FIRST CONDITION\n  WHEN #X = 1\n    DISPLAY 'one'\n  WHEN #X = 2\n    DISPLAY 'two'\n  WHEN NONE\n    DISPLAY 'other'\nEND-DECIDE";
        let interp = run(src);
        assert_eq!(interp.output_lines[0], "two");
    }

    #[test]
    fn test_decide_for_none() {
        let src = "COMPUTE #X = 99\nDECIDE FOR FIRST CONDITION\n  WHEN #X = 1\n    DISPLAY 'one'\n  WHEN NONE\n    DISPLAY 'none'\nEND-DECIDE";
        let interp = run(src);
        assert_eq!(interp.output_lines[0], "none");
    }

    #[test]
    fn test_decide_on() {
        let src = "MOVE 'B' TO #STATUS\nDECIDE ON FIRST VALUE OF #STATUS\n  WHEN 'A'\n    DISPLAY 'active'\n  WHEN 'B'\n    DISPLAY 'blocked'\n  WHEN NONE\n    DISPLAY 'unknown'\nEND-DECIDE";
        let interp = run(src);
        assert_eq!(interp.output_lines[0], "blocked");
    }

    #[test]
    fn test_compress() {
        let src = "MOVE 'John' TO #FIRST\nMOVE 'Doe' TO #LAST\nCOMPRESS #FIRST #LAST INTO #FULL LEAVING SPACE";
        let interp = run(src);
        assert_eq!(interp.variables.get_value("#FULL").to_display_string(), "John Doe");
    }

    #[test]
    fn test_compress_no_space() {
        let src = "MOVE 'AB' TO #A\nMOVE 'CD' TO #B\nCOMPRESS #A #B INTO #C";
        let interp = run(src);
        assert_eq!(interp.variables.get_value("#C").to_display_string(), "ABCD");
    }

    #[test]
    fn test_separate() {
        let src = "MOVE 'John,Doe' TO #FULL\nSEPARATE #FULL INTO #FIRST #LAST WITH DELIMITER ','";
        let interp = run(src);
        assert_eq!(interp.variables.get_value("#FIRST").to_display_string(), "John");
        assert_eq!(interp.variables.get_value("#LAST").to_display_string(), "Doe");
    }

    #[test]
    fn test_examine_count() {
        let src = "MOVE 'ABCABC' TO #S\nEXAMINE #S FOR 'A' GIVING NUMBER #CNT";
        let interp = run(src);
        assert_eq!(interp.variables.get_value("#CNT").to_i64(), 2);
    }

    #[test]
    fn test_examine_replace() {
        let src = "MOVE 'hello world' TO #S\nEXAMINE #S FOR 'world' REPLACE WITH 'earth'";
        let interp = run(src);
        assert_eq!(interp.variables.get_value("#S").to_display_string(), "hello earth");
    }

    #[test]
    fn test_stack_top() {
        let src = "STACK TOP DATA 'first' 'second'";
        let interp = run(src);
        assert_eq!(interp.data_stack.len(), 2);
        assert_eq!(interp.data_stack[0].to_display_string(), "first");
        assert_eq!(interp.data_stack[1].to_display_string(), "second");
    }

    #[test]
    fn test_stack_bottom() {
        let src = "STACK TOP DATA 'first'\nSTACK BOTTOM DATA 'last'";
        let interp = run(src);
        assert_eq!(interp.data_stack[0].to_display_string(), "first");
        assert_eq!(interp.data_stack[1].to_display_string(), "last");
    }

    #[test]
    fn test_display_multiple() {
        let interp = run("DISPLAY 'A' 'B' 'C'");
        assert_eq!(interp.output_lines[0], "A B C");
    }

    #[test]
    fn test_nested_for() {
        let src = "COMPUTE #SUM = 0\nFOR #I = 1 TO 2\n  FOR #J = 1 TO 2\n    COMPUTE #SUM = #SUM + 1\n  END-FOR\nEND-FOR";
        let interp = run(src);
        assert_eq!(interp.variables.get_value("#SUM").to_f64(), 4.0);
    }

    #[test]
    fn test_callnat() {
        let sub_src = "COMPUTE #OUT = #IN * 2";
        let sub_prog = parse_natural(sub_src, "DOUBLE").unwrap();
        let obj = NaturalObject {
            name: "DOUBLE".to_string(),
            object_type: ObjectType::Subprogram,
            program: sub_prog,
        };

        let main_src = "COMPUTE #IN = 5\nCALLNAT 'DOUBLE' #IN";
        let main_prog = parse_natural(main_src, "MAIN").unwrap();
        let mut interp = NaturalInterpreter::new();
        interp.register_program(obj);
        interp.execute(&main_prog).unwrap();
        // After CALLNAT, local state is restored
    }

    #[test]
    fn test_division_by_zero() {
        let src = "COMPUTE #X = 10 / 0";
        let prog = parse_natural(src, "TEST").unwrap();
        let mut interp = NaturalInterpreter::new();
        let result = interp.execute(&prog);
        assert!(result.is_err());
    }

    #[test]
    fn test_return_stmt() {
        let src = "DISPLAY 'before'\nRETURN\nDISPLAY 'after'";
        let interp = run(src);
        // Return should stop execution but not error in main (it returns ControlSignal::Return)
        assert_eq!(interp.output_lines.len(), 1);
        assert_eq!(interp.output_lines[0], "before");
    }

    #[test]
    fn test_newpage() {
        let interp = run("NEWPAGE");
        assert_eq!(interp.output_lines[0], "\x0C");
    }

    #[test]
    fn test_expression_precedence() {
        let interp = run("COMPUTE #R = 2 + 3 * 4");
        assert_eq!(interp.variables.get_value("#R").to_f64(), 14.0);
    }
}
