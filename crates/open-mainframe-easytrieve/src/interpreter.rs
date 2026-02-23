//! EZ-101: Interpreter Core for Easytrieve Plus.
//!
//! Executes parsed Easytrieve programs with variable storage,
//! arithmetic, control flow (IF/ELSE/END-IF, DO/END-DO, PERFORM, GOTO).

use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use crate::parser::{EzProgram, EzStatement};

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

/// Errors during Easytrieve program execution.
#[derive(Debug, Error, Diagnostic)]
pub enum InterpretError {
    /// Undefined variable referenced.
    #[error("undefined variable '{name}'")]
    UndefinedVariable {
        /// Variable name.
        name: String,
    },
    /// Type mismatch in operation.
    #[error("type mismatch: expected {expected}, got {got}")]
    TypeMismatch {
        /// Expected type description.
        expected: String,
        /// Actual type description.
        got: String,
    },
    /// Division by zero.
    #[error("division by zero")]
    DivisionByZero,
    /// Label not found for GOTO/PERFORM.
    #[error("label '{label}' not found")]
    LabelNotFound {
        /// The missing label.
        label: String,
    },
    /// Execution limit exceeded (runaway loop protection).
    #[error("execution limit exceeded ({limit} statements)")]
    ExecutionLimitExceeded {
        /// The statement limit.
        limit: usize,
    },
}

// ---------------------------------------------------------------------------
// Value types
// ---------------------------------------------------------------------------

/// Runtime value in Easytrieve.
#[derive(Debug, Clone, PartialEq)]
pub enum EzValue {
    /// Integer numeric value.
    Numeric(i64),
    /// Decimal numeric value.
    Decimal(f64),
    /// Alphanumeric string value.
    Alpha(String),
}

impl EzValue {
    /// Convert value to a numeric i64, if possible.
    pub fn as_numeric(&self) -> Option<i64> {
        match self {
            Self::Numeric(n) => Some(*n),
            Self::Decimal(d) => Some(*d as i64),
            Self::Alpha(s) => s.trim().parse().ok(),
        }
    }

    /// Convert value to f64.
    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Self::Numeric(n) => Some(*n as f64),
            Self::Decimal(d) => Some(*d),
            Self::Alpha(s) => s.trim().parse().ok(),
        }
    }

    /// Convert value to string representation.
    pub fn as_string(&self) -> String {
        match self {
            Self::Numeric(n) => n.to_string(),
            Self::Decimal(d) => format!("{d:.2}"),
            Self::Alpha(s) => s.clone(),
        }
    }

    /// Check if value is truthy (non-zero numeric or non-empty string).
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Numeric(n) => *n != 0,
            Self::Decimal(d) => *d != 0.0,
            Self::Alpha(s) => !s.is_empty(),
        }
    }
}

impl std::fmt::Display for EzValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

// ---------------------------------------------------------------------------
// Variable
// ---------------------------------------------------------------------------

/// Working storage variable with PIC-like type information.
#[derive(Debug, Clone)]
pub struct EzVariable {
    /// Variable name.
    pub name: String,
    /// Current value.
    pub value: EzValue,
    /// Data type code (A=alpha, N=numeric, P=packed, B=binary, W=working).
    pub data_type: String,
    /// Field length.
    pub length: u32,
}

impl EzVariable {
    /// Create a new working storage variable.
    pub fn new(name: &str, data_type: &str, length: u32) -> Self {
        let value = match data_type {
            "N" | "P" | "B" | "U" | "I" => EzValue::Numeric(0),
            _ => EzValue::Alpha(String::new()),
        };
        Self {
            name: name.to_string(),
            value,
            data_type: data_type.to_string(),
            length,
        }
    }

    /// Create a variable with an initial value.
    pub fn with_value(name: &str, data_type: &str, length: u32, value: EzValue) -> Self {
        Self {
            name: name.to_string(),
            value,
            data_type: data_type.to_string(),
            length,
        }
    }
}

// ---------------------------------------------------------------------------
// Interpreter
// ---------------------------------------------------------------------------

/// Easytrieve program interpreter.
///
/// Executes an [`EzProgram`] with variable storage, control flow,
/// and output capture.
pub struct EzInterpreter {
    /// Variable storage keyed by name.
    pub variables: HashMap<String, EzVariable>,
    /// Output lines produced by DISPLAY/PRINT.
    pub output: Vec<String>,
    /// Label positions (label name -> statement index in activities).
    labels: HashMap<String, usize>,
    /// Maximum number of statements to execute (runaway protection).
    pub max_statements: usize,
    /// Whether the program has been stopped.
    stopped: bool,
}

impl EzInterpreter {
    /// Create a new interpreter.
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            output: Vec::new(),
            labels: HashMap::new(),
            max_statements: 100_000,
            stopped: false,
        }
    }

    /// Execute a parsed Easytrieve program.
    pub fn execute(&mut self, program: &EzProgram) -> Result<(), InterpretError> {
        // Initialize defines as variables
        for stmt in &program.defines {
            if let EzStatement::Define {
                name,
                data_type,
                length,
                value,
                ..
            } = stmt
            {
                let dt = data_type.as_deref().unwrap_or("A");
                let len = length.unwrap_or(10);
                let var = if let Some(val) = value {
                    let ez_val = Self::parse_literal(val, dt);
                    EzVariable::with_value(name, dt, len, ez_val)
                } else {
                    EzVariable::new(name, dt, len)
                };
                self.variables.insert(name.clone(), var);
            }
        }

        // Build label index from activities
        for (i, stmt) in program.activities.iter().enumerate() {
            if let EzStatement::Label { name } = stmt {
                self.labels.insert(name.clone(), i);
            }
        }

        // Execute activities
        let mut pc = 0;
        let mut executed = 0;

        while pc < program.activities.len() && !self.stopped {
            if executed >= self.max_statements {
                return Err(InterpretError::ExecutionLimitExceeded {
                    limit: self.max_statements,
                });
            }

            let stmt = &program.activities[pc];
            match self.execute_statement(stmt, &program.activities, &mut pc)? {
                Flow::Next => pc += 1,
                Flow::Jump(target) => pc = target,
                Flow::Stop => {
                    self.stopped = true;
                    break;
                }
            }
            executed += 1;
        }

        Ok(())
    }

    /// Execute a single statement, returning flow control action.
    fn execute_statement(
        &mut self,
        stmt: &EzStatement,
        activities: &[EzStatement],
        pc: &mut usize,
    ) -> Result<Flow, InterpretError> {
        match stmt {
            EzStatement::Display { items } => {
                let mut parts = Vec::new();
                for item in items {
                    let trimmed = item.trim_matches('\'');
                    if item.starts_with('\'') && item.ends_with('\'') {
                        parts.push(trimmed.to_string());
                    } else if let Some(var) = self.variables.get(trimmed) {
                        parts.push(var.value.as_string());
                    } else {
                        parts.push(item.clone());
                    }
                }
                self.output.push(parts.join(" "));
                Ok(Flow::Next)
            }

            EzStatement::Assignment { target, source } => {
                let value = self.evaluate_expression(source)?;
                self.set_variable(target, value);
                Ok(Flow::Next)
            }

            EzStatement::Arithmetic {
                op,
                operand1,
                operand2,
            } => {
                let val1 = self.resolve_value(operand1)?;
                let val2 = self.resolve_value(operand2)?;

                let n1 = val1
                    .as_f64()
                    .ok_or_else(|| InterpretError::TypeMismatch {
                        expected: "numeric".into(),
                        got: "alpha".into(),
                    })?;
                let n2 = val2
                    .as_f64()
                    .ok_or_else(|| InterpretError::TypeMismatch {
                        expected: "numeric".into(),
                        got: "alpha".into(),
                    })?;

                let result = match op.as_str() {
                    "ADD" => n2 + n1,
                    "SUBTRACT" => n2 - n1,
                    "MULTIPLY" => n2 * n1,
                    "DIVIDE" => {
                        if n1 == 0.0 {
                            return Err(InterpretError::DivisionByZero);
                        }
                        n2 / n1
                    }
                    _ => n1,
                };

                let result_val = if result.fract() == 0.0 {
                    EzValue::Numeric(result as i64)
                } else {
                    EzValue::Decimal(result)
                };

                self.set_variable(operand2, result_val);
                Ok(Flow::Next)
            }

            EzStatement::If { condition } => {
                let cond_result = self.evaluate_condition(condition)?;
                if cond_result {
                    Ok(Flow::Next)
                } else {
                    // Skip to ELSE or END-IF
                    let target = Self::find_else_or_endif(activities, *pc);
                    Ok(Flow::Jump(target))
                }
            }

            EzStatement::Else => {
                // If we reach ELSE during normal flow, skip to END-IF
                let target = Self::find_endif(activities, *pc);
                Ok(Flow::Jump(target))
            }

            EzStatement::EndIf | EzStatement::EndDo => Ok(Flow::Next),

            EzStatement::Do { condition } => {
                if let Some(cond) = condition {
                    let cond_result = self.evaluate_condition(cond)?;
                    if !cond_result {
                        let target = Self::find_enddo(activities, *pc);
                        return Ok(Flow::Jump(target));
                    }
                }
                Ok(Flow::Next)
            }

            EzStatement::GoTo { label } => {
                let target = self
                    .labels
                    .get(label)
                    .copied()
                    .ok_or_else(|| InterpretError::LabelNotFound {
                        label: label.clone(),
                    })?;
                Ok(Flow::Jump(target))
            }

            EzStatement::Perform { procedure } => {
                let target = self.labels.get(procedure).copied().ok_or_else(|| {
                    InterpretError::LabelNotFound {
                        label: procedure.clone(),
                    }
                })?;
                // Execute from target until END or next label
                let mut sub_pc = target;
                while sub_pc < activities.len() {
                    match &activities[sub_pc] {
                        EzStatement::End => break,
                        EzStatement::Label { name } if sub_pc != target => {
                            let _ = name;
                            break;
                        }
                        other => {
                            match self.execute_statement(other, activities, &mut sub_pc)? {
                                Flow::Next => sub_pc += 1,
                                Flow::Jump(t) => sub_pc = t,
                                Flow::Stop => return Ok(Flow::Stop),
                            }
                        }
                    }
                }
                Ok(Flow::Next)
            }

            EzStatement::Stop => Ok(Flow::Stop),

            EzStatement::End => Ok(Flow::Stop),

            EzStatement::ElseIf { condition } => {
                // If we reach ELSE-IF during normal flow, skip to END-IF
                // (we already executed the IF-true branch)
                let target = Self::find_endif(activities, *pc);
                let _ = condition;
                Ok(Flow::Jump(target))
            }

            EzStatement::Case { .. } => Ok(Flow::Next),
            EzStatement::When { values } => {
                // WHEN within CASE — skip (CASE evaluation not fully wired)
                let _ = values;
                Ok(Flow::Next)
            }
            EzStatement::EndCase => Ok(Flow::Next),
            EzStatement::EndProc => Ok(Flow::Next),

            // Statements that are no-ops in the interpreter (handled elsewhere)
            EzStatement::Job { .. }
            | EzStatement::Print { .. }
            | EzStatement::Heading { .. }
            | EzStatement::Line { .. }
            | EzStatement::Title { .. }
            | EzStatement::File { .. }
            | EzStatement::Define { .. }
            | EzStatement::Sort { .. }
            | EzStatement::Put { .. }
            | EzStatement::Get { .. }
            | EzStatement::Macro { .. }
            | EzStatement::Copy { .. }
            | EzStatement::Sql { .. }
            | EzStatement::Report { .. }
            | EzStatement::Sequence { .. }
            | EzStatement::Control { .. }
            | EzStatement::Sum { .. }
            | EzStatement::Call { .. }
            | EzStatement::Read { .. }
            | EzStatement::Write { .. }
            | EzStatement::Point { .. }
            | EzStatement::Close { .. }
            | EzStatement::Search { .. }
            | EzStatement::Mask { .. }
            | EzStatement::Link { .. }
            | EzStatement::Transfer { .. }
            | EzStatement::Proc { .. }
            | EzStatement::NewPage
            | EzStatement::Skip { .. }
            | EzStatement::Release { .. }
            | EzStatement::Parm { .. }
            | EzStatement::Dli { .. }
            | EzStatement::Record { .. }
            | EzStatement::Comment { .. }
            | EzStatement::Label { .. } => Ok(Flow::Next),
        }
    }

    /// Find the matching ELSE or END-IF for an IF at position `start`.
    fn find_else_or_endif(activities: &[EzStatement], start: usize) -> usize {
        let mut depth = 0;
        for (i, stmt) in activities.iter().enumerate().skip(start + 1) {
            match stmt {
                EzStatement::If { .. } => depth += 1,
                EzStatement::Else if depth == 0 => return i + 1,
                EzStatement::EndIf if depth == 0 => return i + 1,
                EzStatement::EndIf => depth -= 1,
                _ => {}
            }
        }
        activities.len()
    }

    /// Find the matching END-IF for an ELSE at position `start`.
    fn find_endif(activities: &[EzStatement], start: usize) -> usize {
        let mut depth = 0;
        for (i, stmt) in activities.iter().enumerate().skip(start + 1) {
            match stmt {
                EzStatement::If { .. } => depth += 1,
                EzStatement::EndIf if depth == 0 => return i + 1,
                EzStatement::EndIf => depth -= 1,
                _ => {}
            }
        }
        activities.len()
    }

    /// Find the matching END-DO for a DO at position `start`.
    fn find_enddo(activities: &[EzStatement], start: usize) -> usize {
        let mut depth = 0;
        for (i, stmt) in activities.iter().enumerate().skip(start + 1) {
            match stmt {
                EzStatement::Do { .. } => depth += 1,
                EzStatement::EndDo if depth == 0 => return i + 1,
                EzStatement::EndDo => depth -= 1,
                _ => {}
            }
        }
        activities.len()
    }

    /// Evaluate a condition expression (simple comparisons).
    fn evaluate_condition(&self, tokens: &[String]) -> Result<bool, InterpretError> {
        if tokens.is_empty() {
            return Ok(false);
        }

        // Simple two-operand comparison: field OP value
        if tokens.len() >= 3 {
            let left = self.resolve_value(&tokens[0])?;
            let op = &tokens[1];
            let right = self.resolve_value(&tokens[2])?;
            return Ok(Self::compare_values(&left, op, &right));
        }

        // Single value truthiness
        if tokens.len() == 1 {
            let val = self.resolve_value(&tokens[0])?;
            return Ok(val.is_truthy());
        }

        Ok(false)
    }

    /// Compare two values with an operator.
    fn compare_values(left: &EzValue, op: &str, right: &EzValue) -> bool {
        // Try numeric comparison first
        if let (Some(l), Some(r)) = (left.as_f64(), right.as_f64()) {
            return match op.to_uppercase().as_str() {
                "=" | "EQ" => (l - r).abs() < f64::EPSILON,
                "<>" | "NE" => (l - r).abs() >= f64::EPSILON,
                ">" | "GT" => l > r,
                ">=" | "GE" => l >= r,
                "<" | "LT" => l < r,
                "<=" | "LE" => l <= r,
                _ => false,
            };
        }

        // Fall back to string comparison
        let ls = left.as_string();
        let rs = right.as_string();
        match op.to_uppercase().as_str() {
            "=" | "EQ" => ls == rs,
            "<>" | "NE" => ls != rs,
            ">" | "GT" => ls > rs,
            ">=" | "GE" => ls >= rs,
            "<" | "LT" => ls < rs,
            "<=" | "LE" => ls <= rs,
            _ => false,
        }
    }

    /// Evaluate a simple expression (single value or literal).
    fn evaluate_expression(&self, tokens: &[String]) -> Result<EzValue, InterpretError> {
        if tokens.is_empty() {
            return Ok(EzValue::Alpha(String::new()));
        }
        if tokens.len() == 1 {
            return self.resolve_value(&tokens[0]);
        }

        // Try simple arithmetic: val OP val
        if tokens.len() == 3 {
            let left = self.resolve_value(&tokens[0])?;
            let op = &tokens[1];
            let right = self.resolve_value(&tokens[2])?;

            if let (Some(l), Some(r)) = (left.as_f64(), right.as_f64()) {
                let result = match op.as_str() {
                    "+" => l + r,
                    "-" => l - r,
                    "*" => l * r,
                    "/" => {
                        if r == 0.0 {
                            return Err(InterpretError::DivisionByZero);
                        }
                        l / r
                    }
                    _ => l,
                };
                return if result.fract() == 0.0 {
                    Ok(EzValue::Numeric(result as i64))
                } else {
                    Ok(EzValue::Decimal(result))
                };
            }
        }

        // Concatenate as string
        let mut result = String::new();
        for token in tokens {
            let val = self.resolve_value(token)?;
            result.push_str(&val.as_string());
        }
        Ok(EzValue::Alpha(result))
    }

    /// Resolve a token to a value (variable lookup or literal).
    fn resolve_value(&self, token: &str) -> Result<EzValue, InterpretError> {
        // String literal
        if token.starts_with('\'') && token.ends_with('\'') && token.len() >= 2 {
            let inner = &token[1..token.len() - 1];
            return Ok(EzValue::Alpha(inner.to_string()));
        }

        // Number literal
        if let Ok(n) = token.parse::<i64>() {
            return Ok(EzValue::Numeric(n));
        }
        if let Ok(d) = token.parse::<f64>() {
            return Ok(EzValue::Decimal(d));
        }

        // Variable lookup
        if let Some(var) = self.variables.get(token) {
            return Ok(var.value.clone());
        }

        // Return as alpha string (could be a keyword used as value)
        Ok(EzValue::Alpha(token.to_string()))
    }

    /// Set a variable value, creating it if it doesn't exist.
    fn set_variable(&mut self, name: &str, value: EzValue) {
        if let Some(var) = self.variables.get_mut(name) {
            var.value = value;
        } else {
            let dt = match &value {
                EzValue::Numeric(_) | EzValue::Decimal(_) => "N",
                EzValue::Alpha(_) => "A",
            };
            self.variables
                .insert(name.to_string(), EzVariable::with_value(name, dt, 10, value));
        }
    }

    /// Parse a literal value string into an EzValue.
    fn parse_literal(s: &str, data_type: &str) -> EzValue {
        let trimmed = s.trim().trim_matches('\'');
        match data_type {
            "N" | "P" | "B" | "U" | "I" => {
                if let Ok(n) = trimmed.parse::<i64>() {
                    EzValue::Numeric(n)
                } else if let Ok(d) = trimmed.parse::<f64>() {
                    EzValue::Decimal(d)
                } else {
                    EzValue::Numeric(0)
                }
            }
            _ => EzValue::Alpha(trimmed.to_string()),
        }
    }
}

impl Default for EzInterpreter {
    fn default() -> Self {
        Self::new()
    }
}

/// Internal flow control for statement execution.
enum Flow {
    /// Proceed to next statement.
    Next,
    /// Jump to a specific statement index.
    Jump(usize),
    /// Stop execution.
    Stop,
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::EzParser;

    fn run_program(source: &str) -> Result<EzInterpreter, InterpretError> {
        let prog = EzParser::parse(source).expect("parse error");
        let mut interp = EzInterpreter::new();
        interp.execute(&prog)?;
        Ok(interp)
    }

    #[test]
    fn test_display_literal() {
        let source = "    DISPLAY 'HELLO WORLD'\n";
        let interp = run_program(source).unwrap();
        assert_eq!(interp.output, vec!["HELLO WORLD"]);
    }

    #[test]
    fn test_define_and_display() {
        let source = "    DEFINE GREETING W 20 A VALUE 'HI THERE'\n    DISPLAY GREETING\n";
        let interp = run_program(source).unwrap();
        assert_eq!(interp.output, vec!["HI THERE"]);
    }

    #[test]
    fn test_assignment() {
        let source = "    DEFINE X W 8 N\n    X = 42\n    DISPLAY X\n";
        let interp = run_program(source).unwrap();
        assert_eq!(interp.output, vec!["42"]);
    }

    #[test]
    fn test_arithmetic_add() {
        let source = "    DEFINE TOTAL W 8 N VALUE 100\n    ADD 50 TO TOTAL\n    DISPLAY TOTAL\n";
        let interp = run_program(source).unwrap();
        assert_eq!(interp.output, vec!["150"]);
    }

    #[test]
    fn test_arithmetic_subtract() {
        let source = "    DEFINE TOTAL W 8 N VALUE 100\n    SUBTRACT 30 TO TOTAL\n    DISPLAY TOTAL\n";
        let interp = run_program(source).unwrap();
        assert_eq!(interp.output, vec!["70"]);
    }

    #[test]
    fn test_if_true_branch() {
        let source = "    DEFINE X W 8 N VALUE 200\n    IF X > 100\n    DISPLAY 'BIG'\n    END-IF\n";
        let interp = run_program(source).unwrap();
        assert_eq!(interp.output, vec!["BIG"]);
    }

    #[test]
    fn test_if_false_branch() {
        let source = "    DEFINE X W 8 N VALUE 50\n    IF X > 100\n    DISPLAY 'BIG'\n    ELSE\n    DISPLAY 'SMALL'\n    END-IF\n";
        let interp = run_program(source).unwrap();
        assert_eq!(interp.output, vec!["SMALL"]);
    }

    #[test]
    fn test_expression_arithmetic() {
        let source = "    DEFINE A W 8 N VALUE 10\n    DEFINE B W 8 N VALUE 20\n    DEFINE C W 8 N\n    C = A + B\n    DISPLAY C\n";
        let interp = run_program(source).unwrap();
        assert_eq!(interp.output, vec!["30"]);
    }

    #[test]
    fn test_stop_execution() {
        let source = "    DISPLAY 'BEFORE'\n    STOP\n    DISPLAY 'AFTER'\n";
        let interp = run_program(source).unwrap();
        assert_eq!(interp.output, vec!["BEFORE"]);
    }

    #[test]
    fn test_value_display() {
        let val = EzValue::Numeric(42);
        assert_eq!(val.as_string(), "42");
        assert!(val.is_truthy());

        let zero = EzValue::Numeric(0);
        assert!(!zero.is_truthy());

        let alpha = EzValue::Alpha("hello".into());
        assert!(alpha.is_truthy());

        let empty = EzValue::Alpha(String::new());
        assert!(!empty.is_truthy());
    }

    #[test]
    fn test_variable_creation() {
        let var = EzVariable::new("TEST", "N", 8);
        assert_eq!(var.name, "TEST");
        assert_eq!(var.value, EzValue::Numeric(0));
    }
}
