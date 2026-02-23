//! FOC-105: Dialogue Manager (5 stories).
//!
//! Amper variables (local, global, system), control flow (-IF, -GOTO, -REPEAT),
//! procedure execution (-RUN, -INCLUDE), user interaction (-TYPE, -READ),
//! and the DialogueInterpreter for executing Dialogue Manager scripts.

use std::collections::HashMap;
use thiserror::Error;

use crate::parser::{CompOp, DialogueCmd, Expr, FocusLexer, FocusParser, FocusStatement};

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum DialogueError {
    #[error("undefined variable: {0}")]
    UndefinedVariable(String),
    #[error("label not found: {0}")]
    LabelNotFound(String),
    #[error("procedure not found: {0}")]
    ProcedureNotFound(String),
    #[error("parse error: {0}")]
    ParseError(String),
    #[error("execution limit reached (possible infinite loop)")]
    ExecutionLimit,
    #[error("type mismatch in expression")]
    TypeMismatch,
}

// ---------------------------------------------------------------------------
// Amper Variables
// ---------------------------------------------------------------------------

/// An amper variable in the Dialogue Manager.
#[derive(Debug, Clone, PartialEq)]
pub enum AmperVariable {
    /// Local variable `&var`.
    Local(String),
    /// Global variable `&&var`.
    Global(String),
    /// System variable `&DATE`, `&TIME`, `&USER`, etc.
    System(String),
}

/// Value of a Dialogue Manager variable.
#[derive(Debug, Clone, PartialEq)]
pub enum DmValue {
    Str(String),
    Num(f64),
    Null,
}

impl DmValue {
    pub fn as_num(&self) -> f64 {
        match self {
            DmValue::Num(n) => *n,
            DmValue::Str(s) => s.parse().unwrap_or(0.0),
            DmValue::Null => 0.0,
        }
    }

    pub fn as_str(&self) -> String {
        match self {
            DmValue::Str(s) => s.clone(),
            DmValue::Num(n) => format!("{n}"),
            DmValue::Null => String::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// Dialogue Command (runtime version with labels)
// ---------------------------------------------------------------------------

/// A labelled Dialogue Manager command for execution.
#[derive(Debug, Clone)]
pub struct LabelledCommand {
    pub label: Option<String>,
    pub command: DialogueCmd,
}

/// Result of executing a Dialogue Manager script.
#[derive(Debug, Clone)]
pub struct DialogueResult {
    pub output_messages: Vec<String>,
    pub variables: HashMap<String, DmValue>,
    pub procedures_run: Vec<String>,
    pub includes_processed: Vec<String>,
}

// ---------------------------------------------------------------------------
// Dialogue Interpreter
// ---------------------------------------------------------------------------

/// Interprets and executes Dialogue Manager commands.
pub struct DialogueInterpreter {
    local_vars: HashMap<String, DmValue>,
    global_vars: HashMap<String, DmValue>,
    output: Vec<String>,
    procedures_run: Vec<String>,
    includes_processed: Vec<String>,
    /// Maximum number of steps before aborting (loop guard).
    pub max_steps: usize,
    /// Procedure registry: name -> source text.
    procedures: HashMap<String, String>,
    /// Input queue for -READ commands.
    input_queue: Vec<String>,
}

impl DialogueInterpreter {
    pub fn new() -> Self {
        Self {
            local_vars: HashMap::new(),
            global_vars: HashMap::new(),
            output: Vec::new(),
            procedures_run: Vec::new(),
            includes_processed: Vec::new(),
            max_steps: 10_000,
            procedures: HashMap::new(),
            input_queue: Vec::new(),
        }
    }

    /// Set a local variable.
    pub fn set_local(&mut self, name: &str, value: DmValue) {
        self.local_vars.insert(name.to_uppercase(), value);
    }

    /// Set a global variable.
    pub fn set_global(&mut self, name: &str, value: DmValue) {
        self.global_vars.insert(name.to_uppercase(), value);
    }

    /// Get a variable value (local first, then global, then system).
    pub fn get_var(&self, name: &str) -> Option<&DmValue> {
        let upper = name.to_uppercase();
        self.local_vars
            .get(&upper)
            .or_else(|| self.global_vars.get(&upper))
    }

    /// Get a system variable value.
    pub fn get_system_var(&self, name: &str) -> DmValue {
        match name.to_uppercase().as_str() {
            "DATE" => DmValue::Str("2026-02-23".to_string()),
            "TIME" => DmValue::Str("12:00:00".to_string()),
            "USER" => DmValue::Str("SYSUSER".to_string()),
            "FOCFOC" => DmValue::Str("FOCUS".to_string()),
            "FOESSION" => DmValue::Str("001".to_string()),
            _ => DmValue::Null,
        }
    }

    /// Register a procedure (FOCUS .fex file content).
    pub fn register_procedure(&mut self, name: &str, source: &str) {
        self.procedures.insert(name.to_string(), source.to_string());
    }

    /// Queue input values for -READ commands.
    pub fn queue_input(&mut self, values: Vec<String>) {
        self.input_queue.extend(values);
    }

    /// Execute a sequence of Dialogue Manager commands from source text.
    pub fn execute(&mut self, source: &str) -> Result<DialogueResult, DialogueError> {
        let commands = self.parse_commands(source)?;
        self.execute_commands(&commands)
    }

    /// Parse source text into labelled commands.
    fn parse_commands(&self, source: &str) -> Result<Vec<LabelledCommand>, DialogueError> {
        let mut commands = Vec::new();

        // First pass: extract labels (lines ending with ':' or starting with label)
        for line in source.lines() {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }

            // Check for label: "LABEL_NAME. -command" or standalone label
            let (label, cmd_text) = if let Some(dot_pos) = trimmed.find('.') {
                let potential_label = trimmed[..dot_pos].trim();
                if !potential_label.is_empty()
                    && potential_label.chars().all(|c| c.is_alphanumeric() || c == '_')
                    && trimmed[dot_pos + 1..].trim().starts_with('-')
                {
                    (
                        Some(potential_label.to_string()),
                        trimmed[dot_pos + 1..].trim().to_string(),
                    )
                } else {
                    (None, trimmed.to_string())
                }
            } else {
                (None, trimmed.to_string())
            };

            if !cmd_text.starts_with('-') {
                // Might be a standalone label
                let label_candidate = cmd_text.trim_end_matches(':').trim();
                if !label_candidate.is_empty()
                    && label_candidate
                        .chars()
                        .all(|c| c.is_alphanumeric() || c == '_')
                {
                    // Label-only line: treated as a NOP with label
                    commands.push(LabelledCommand {
                        label: Some(label_candidate.to_string()),
                        command: DialogueCmd::TypeMsg {
                            message: String::new(),
                        },
                    });
                }
                continue;
            }

            // Parse the DM command
            let tokens = FocusLexer::new(&cmd_text)
                .tokenize()
                .map_err(|e| DialogueError::ParseError(format!("{e}")))?;
            let mut parser = FocusParser::new(tokens);
            match parser.parse_all() {
                Ok(stmts) => {
                    for stmt in stmts {
                        if let FocusStatement::Dialogue(cmd) = stmt {
                            commands.push(LabelledCommand {
                                label: label.clone(),
                                command: cmd,
                            });
                        }
                    }
                }
                Err(e) => return Err(DialogueError::ParseError(format!("{e}"))),
            }
        }

        Ok(commands)
    }

    /// Execute a list of labelled commands.
    fn execute_commands(
        &mut self,
        commands: &[LabelledCommand],
    ) -> Result<DialogueResult, DialogueError> {
        let mut pc = 0; // program counter
        let mut steps = 0;

        while pc < commands.len() {
            steps += 1;
            if steps > self.max_steps {
                return Err(DialogueError::ExecutionLimit);
            }

            let cmd = &commands[pc];
            match &cmd.command {
                DialogueCmd::Set { var, value } => {
                    let val = self.eval_dm_expr(value)?;
                    self.local_vars.insert(var.to_uppercase(), val);
                    pc += 1;
                }
                DialogueCmd::If {
                    var,
                    op,
                    value,
                    label,
                } => {
                    let var_val = self
                        .get_var(var)
                        .cloned()
                        .unwrap_or(DmValue::Null);
                    let cmp_val = self.eval_dm_expr(value)?;
                    if self.dm_compare(&var_val, op, &cmp_val) {
                        // GOTO label
                        pc = self.find_label(commands, label)?;
                    } else {
                        pc += 1;
                    }
                }
                DialogueCmd::Goto { label } => {
                    pc = self.find_label(commands, label)?;
                }
                DialogueCmd::Run { procedure } => {
                    self.procedures_run.push(procedure.clone());
                    if let Some(src) = self.procedures.get(procedure).cloned() {
                        let sub_commands = self.parse_commands(&src)?;
                        self.execute_commands(&sub_commands)?;
                    }
                    pc += 1;
                }
                DialogueCmd::Include { file } => {
                    self.includes_processed.push(file.clone());
                    if let Some(src) = self.procedures.get(file).cloned() {
                        let sub_commands = self.parse_commands(&src)?;
                        self.execute_commands(&sub_commands)?;
                    }
                    pc += 1;
                }
                DialogueCmd::Repeat => {
                    // Mark repeat point — handled by -UNTIL scanning back
                    pc += 1;
                }
                DialogueCmd::Until { var, op, value } => {
                    let var_val = self
                        .get_var(var)
                        .cloned()
                        .unwrap_or(DmValue::Null);
                    let cmp_val = self.eval_dm_expr(value)?;
                    if !self.dm_compare(&var_val, op, &cmp_val) {
                        // Condition not met, loop back to -REPEAT
                        if let Some(repeat_pos) = self.find_repeat_before(commands, pc) {
                            pc = repeat_pos + 1;
                        } else {
                            pc += 1;
                        }
                    } else {
                        pc += 1;
                    }
                }
                DialogueCmd::TypeMsg { message } => {
                    if !message.is_empty() {
                        // Substitute variables in message
                        let resolved = self.substitute_vars(message);
                        self.output.push(resolved);
                    }
                    pc += 1;
                }
                DialogueCmd::Read { var } => {
                    let val = if let Some(input) = self.input_queue.first().cloned() {
                        self.input_queue.remove(0);
                        if let Ok(n) = input.parse::<f64>() {
                            DmValue::Num(n)
                        } else {
                            DmValue::Str(input)
                        }
                    } else {
                        DmValue::Null
                    };
                    self.local_vars.insert(var.to_uppercase(), val);
                    pc += 1;
                }
            }
        }

        Ok(DialogueResult {
            output_messages: self.output.clone(),
            variables: self.local_vars.clone(),
            procedures_run: self.procedures_run.clone(),
            includes_processed: self.includes_processed.clone(),
        })
    }

    fn eval_dm_expr(&self, expr: &Expr) -> Result<DmValue, DialogueError> {
        match expr {
            Expr::Literal(n) => Ok(DmValue::Num(*n)),
            Expr::StringLiteral(s) => Ok(DmValue::Str(s.clone())),
            Expr::Var(name) => Ok(self
                .get_var(name)
                .cloned()
                .unwrap_or(self.get_system_var(name))),
            Expr::Field(name) => Ok(self
                .get_var(name)
                .cloned()
                .unwrap_or(DmValue::Str(name.clone()))),
            Expr::BinOp { left, op, right } => {
                let l = self.eval_dm_expr(left)?.as_num();
                let r = self.eval_dm_expr(right)?.as_num();
                let result = match op {
                    '+' => l + r,
                    '-' => l - r,
                    '*' => l * r,
                    '/' => {
                        if r.abs() < f64::EPSILON {
                            0.0
                        } else {
                            l / r
                        }
                    }
                    _ => return Err(DialogueError::TypeMismatch),
                };
                Ok(DmValue::Num(result))
            }
            Expr::FunctionCall { .. } => Ok(DmValue::Null),
        }
    }

    fn dm_compare(&self, left: &DmValue, op: &CompOp, right: &DmValue) -> bool {
        let ln = left.as_num();
        let rn = right.as_num();
        // Use numeric comparison if either side is numeric
        let use_num = matches!(left, DmValue::Num(_)) || matches!(right, DmValue::Num(_));

        if use_num {
            match op {
                CompOp::Eq => (ln - rn).abs() < f64::EPSILON,
                CompOp::Ne => (ln - rn).abs() >= f64::EPSILON,
                CompOp::Gt => ln > rn,
                CompOp::Lt => ln < rn,
                CompOp::Ge => ln >= rn,
                CompOp::Le => ln <= rn,
            }
        } else {
            let ls = left.as_str();
            let rs = right.as_str();
            match op {
                CompOp::Eq => ls == rs,
                CompOp::Ne => ls != rs,
                CompOp::Gt => ls > rs,
                CompOp::Lt => ls < rs,
                CompOp::Ge => ls >= rs,
                CompOp::Le => ls <= rs,
            }
        }
    }

    fn find_label(
        &self,
        commands: &[LabelledCommand],
        label: &str,
    ) -> Result<usize, DialogueError> {
        for (i, cmd) in commands.iter().enumerate() {
            if let Some(ref lbl) = cmd.label {
                if lbl.eq_ignore_ascii_case(label) {
                    return Ok(i);
                }
            }
        }
        Err(DialogueError::LabelNotFound(label.to_string()))
    }

    fn find_repeat_before(&self, commands: &[LabelledCommand], before: usize) -> Option<usize> {
        (0..before)
            .rev()
            .find(|&i| matches!(commands[i].command, DialogueCmd::Repeat))
    }

    fn substitute_vars(&self, text: &str) -> String {
        let mut result = text.to_string();
        for (k, v) in &self.local_vars {
            let pattern = format!("&{k}");
            result = result.replace(&pattern, &v.as_str());
        }
        for (k, v) in &self.global_vars {
            let pattern = format!("&&{k}");
            result = result.replace(&pattern, &v.as_str());
        }
        result
    }
}

impl Default for DialogueInterpreter {
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

    #[test]
    fn test_set_local_variable() {
        let mut interp = DialogueInterpreter::new();
        let result = interp.execute("-SET &X = 42").unwrap();
        assert_eq!(
            result.variables.get("X"),
            Some(&DmValue::Num(42.0))
        );
    }

    #[test]
    fn test_set_string_variable() {
        let mut interp = DialogueInterpreter::new();
        let result = interp.execute("-SET &NAME = 'Alice'").unwrap();
        assert_eq!(
            result.variables.get("NAME"),
            Some(&DmValue::Str("Alice".to_string()))
        );
    }

    #[test]
    fn test_set_expression_variable() {
        let mut interp = DialogueInterpreter::new();
        let result = interp.execute("-SET &X = 10 + 5").unwrap();
        let val = result.variables.get("X").unwrap();
        assert!((val.as_num() - 15.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_global_variable() {
        let mut interp = DialogueInterpreter::new();
        interp.set_global("CONFIG", DmValue::Str("PROD".into()));
        let val = interp.get_var("CONFIG").unwrap();
        assert_eq!(*val, DmValue::Str("PROD".into()));
    }

    #[test]
    fn test_system_variable_date() {
        let interp = DialogueInterpreter::new();
        let val = interp.get_system_var("DATE");
        assert_eq!(val, DmValue::Str("2026-02-23".to_string()));
    }

    #[test]
    fn test_system_variable_user() {
        let interp = DialogueInterpreter::new();
        let val = interp.get_system_var("USER");
        assert_eq!(val, DmValue::Str("SYSUSER".to_string()));
    }

    #[test]
    fn test_type_message() {
        let mut interp = DialogueInterpreter::new();
        let result = interp.execute("-TYPE 'Hello World'").unwrap();
        assert_eq!(result.output_messages, vec!["Hello World"]);
    }

    #[test]
    fn test_type_with_variable_substitution() {
        let mut interp = DialogueInterpreter::new();
        interp.set_local("NAME", DmValue::Str("Alice".into()));
        let result = interp.execute("-TYPE '&NAME is here'").unwrap();
        assert_eq!(result.output_messages, vec!["Alice is here"]);
    }

    #[test]
    fn test_if_goto_true() {
        let mut interp = DialogueInterpreter::new();
        let src = "\
-SET &X = 5
-IF &X EQ 5 -GOTO DONE
-TYPE 'not reached'
DONE:
-TYPE 'done'";
        let result = interp.execute(src).unwrap();
        assert_eq!(result.output_messages, vec!["done"]);
    }

    #[test]
    fn test_if_goto_false() {
        let mut interp = DialogueInterpreter::new();
        let src = "\
-SET &X = 3
-IF &X EQ 5 -GOTO SKIP
-TYPE 'reached'
SKIP:
-TYPE 'end'";
        let result = interp.execute(src).unwrap();
        assert_eq!(result.output_messages, vec!["reached", "end"]);
    }

    #[test]
    fn test_goto() {
        let mut interp = DialogueInterpreter::new();
        let src = "\
-GOTO FINISH
-TYPE 'skipped'
FINISH:
-TYPE 'final'";
        let result = interp.execute(src).unwrap();
        assert_eq!(result.output_messages, vec!["final"]);
    }

    #[test]
    fn test_repeat_until() {
        let mut interp = DialogueInterpreter::new();
        let src = "\
-SET &I = 0
-REPEAT
-SET &I = &I + 1
-UNTIL &I GE 3";
        let result = interp.execute(src).unwrap();
        let i_val = result.variables.get("I").unwrap();
        assert!((i_val.as_num() - 3.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_run_procedure() {
        let mut interp = DialogueInterpreter::new();
        interp.register_procedure("sub.fex", "-SET &RESULT = 99");
        let result = interp.execute("-RUN sub.fex").unwrap();
        assert!(result.procedures_run.contains(&"sub.fex".to_string()));
        assert_eq!(
            result.variables.get("RESULT"),
            Some(&DmValue::Num(99.0))
        );
    }

    #[test]
    fn test_include_file() {
        let mut interp = DialogueInterpreter::new();
        interp.register_procedure("header.fex", "-TYPE 'Header loaded'");
        let result = interp.execute("-INCLUDE header.fex").unwrap();
        assert!(result.includes_processed.contains(&"header.fex".to_string()));
        assert!(result.output_messages.contains(&"Header loaded".to_string()));
    }

    #[test]
    fn test_read_from_queue() {
        let mut interp = DialogueInterpreter::new();
        interp.queue_input(vec!["hello".into()]);
        let result = interp.execute("-READ &INPUT").unwrap();
        assert_eq!(
            result.variables.get("INPUT"),
            Some(&DmValue::Str("hello".to_string()))
        );
    }

    #[test]
    fn test_read_numeric() {
        let mut interp = DialogueInterpreter::new();
        interp.queue_input(vec!["42".into()]);
        let result = interp.execute("-READ &NUM").unwrap();
        assert_eq!(result.variables.get("NUM"), Some(&DmValue::Num(42.0)));
    }

    #[test]
    fn test_execution_limit() {
        let mut interp = DialogueInterpreter::new();
        interp.max_steps = 50;
        let src = "\
-SET &I = 0
-REPEAT
-SET &I = &I + 1
-UNTIL &I GE 999999";
        let result = interp.execute(src);
        assert!(result.is_err());
    }

    #[test]
    fn test_dm_value_conversions() {
        let n = DmValue::Num(3.14);
        assert!((n.as_num() - 3.14).abs() < 0.001);
        assert_eq!(n.as_str(), "3.14");

        let s = DmValue::Str("42".into());
        assert!((s.as_num() - 42.0).abs() < f64::EPSILON);
        assert_eq!(s.as_str(), "42");

        let null = DmValue::Null;
        assert!((null.as_num()).abs() < f64::EPSILON);
        assert_eq!(null.as_str(), "");
    }

    #[test]
    fn test_amper_variable_types() {
        let local = AmperVariable::Local("X".into());
        let global = AmperVariable::Global("CONFIG".into());
        let system = AmperVariable::System("DATE".into());
        assert_eq!(local, AmperVariable::Local("X".into()));
        assert_eq!(global, AmperVariable::Global("CONFIG".into()));
        assert_eq!(system, AmperVariable::System("DATE".into()));
    }

    #[test]
    fn test_multiple_type_messages() {
        let mut interp = DialogueInterpreter::new();
        let src = "\
-TYPE 'line 1'
-TYPE 'line 2'
-TYPE 'line 3'";
        let result = interp.execute(src).unwrap();
        assert_eq!(result.output_messages.len(), 3);
    }

    #[test]
    fn test_nested_procedure_call() {
        let mut interp = DialogueInterpreter::new();
        interp.register_procedure("inner.fex", "-SET &INNER = 1");
        interp.register_procedure("outer.fex", "-RUN inner.fex\n-SET &OUTER = 2");
        let result = interp.execute("-RUN outer.fex").unwrap();
        assert_eq!(result.variables.get("INNER"), Some(&DmValue::Num(1.0)));
        assert_eq!(result.variables.get("OUTER"), Some(&DmValue::Num(2.0)));
    }

    #[test]
    fn test_if_ne_condition() {
        let mut interp = DialogueInterpreter::new();
        let src = "\
-SET &X = 10
-IF &X NE 5 -GOTO YES
-TYPE 'no'
YES:
-TYPE 'yes'";
        let result = interp.execute(src).unwrap();
        assert_eq!(result.output_messages, vec!["yes"]);
    }

    #[test]
    fn test_comparison_operators() {
        let mut interp = DialogueInterpreter::new();

        interp.set_local("A", DmValue::Num(10.0));
        // GT
        let src = "-IF &A GT 5 -GOTO OK\n-TYPE 'fail'\nOK:\n-TYPE 'pass'";
        let result = interp.execute(src).unwrap();
        assert!(result.output_messages.contains(&"pass".to_string()));
    }
}
