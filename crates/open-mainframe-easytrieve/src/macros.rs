//! EZ-106: Macros, External Calls & Copy for Easytrieve Plus.
//!
//! Provides MACRO/END-MACRO definition and expansion, CALL to external
//! programs, and COPY statement for including external member definitions.

use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use crate::parser::EzStatement;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

/// Errors during macro processing and external calls.
#[derive(Debug, Error, Diagnostic)]
pub enum MacroError {
    /// Macro not found.
    #[error("macro '{name}' not defined")]
    MacroNotDefined {
        /// Macro name.
        name: String,
    },
    /// Copy member not found.
    #[error("copy member '{name}' not found")]
    CopyMemberNotFound {
        /// Member name.
        name: String,
    },
    /// External program call failed.
    #[error("external call to '{program}' failed: {reason}")]
    ExternalCallFailed {
        /// Program name.
        program: String,
        /// Failure reason.
        reason: String,
    },
    /// Recursive macro expansion detected.
    #[error("recursive macro expansion detected for '{name}'")]
    RecursiveMacro {
        /// Macro name.
        name: String,
    },
    /// Maximum expansion depth exceeded.
    #[error("maximum macro expansion depth ({depth}) exceeded")]
    MaxDepthExceeded {
        /// Depth limit.
        depth: usize,
    },
}

// ---------------------------------------------------------------------------
// Macro definition
// ---------------------------------------------------------------------------

/// An Easytrieve MACRO definition (MACRO name ... END-MACRO).
///
/// Stores the body statements and parameter definitions for expansion.
#[derive(Debug, Clone)]
pub struct EzMacro {
    /// Macro name.
    pub name: String,
    /// Parameter names (positional).
    pub parameters: Vec<String>,
    /// Body statements of the macro.
    pub body: Vec<EzStatement>,
}

impl EzMacro {
    /// Create a new macro definition.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            parameters: Vec::new(),
            body: Vec::new(),
        }
    }

    /// Add a parameter to the macro.
    pub fn add_parameter(&mut self, name: &str) {
        self.parameters.push(name.to_string());
    }

    /// Add a body statement to the macro.
    pub fn add_statement(&mut self, stmt: EzStatement) {
        self.body.push(stmt);
    }

    /// Expand the macro with the given argument values.
    ///
    /// Substitutes parameter references in the body with actual values.
    pub fn expand(&self, args: &[String]) -> Result<Vec<EzStatement>, MacroError> {
        let mut param_map = HashMap::new();
        for (i, param) in self.parameters.iter().enumerate() {
            if let Some(arg) = args.get(i) {
                param_map.insert(param.clone(), arg.clone());
            }
        }

        let mut expanded = Vec::new();
        for stmt in &self.body {
            expanded.push(Self::substitute_statement(stmt, &param_map));
        }
        Ok(expanded)
    }

    /// Substitute parameter references in a statement.
    fn substitute_statement(
        stmt: &EzStatement,
        params: &HashMap<String, String>,
    ) -> EzStatement {
        match stmt {
            EzStatement::Display { items } => EzStatement::Display {
                items: items
                    .iter()
                    .map(|i| Self::substitute_token(i, params))
                    .collect(),
            },
            EzStatement::Assignment { target, source } => EzStatement::Assignment {
                target: Self::substitute_token(target, params),
                source: source
                    .iter()
                    .map(|s| Self::substitute_token(s, params))
                    .collect(),
            },
            EzStatement::If { condition } => EzStatement::If {
                condition: condition
                    .iter()
                    .map(|c| Self::substitute_token(c, params))
                    .collect(),
            },
            EzStatement::Perform { procedure } => EzStatement::Perform {
                procedure: Self::substitute_token(procedure, params),
            },
            EzStatement::GoTo { label } => EzStatement::GoTo {
                label: Self::substitute_token(label, params),
            },
            // For statements that don't need substitution, return as-is
            other => other.clone(),
        }
    }

    /// Substitute a single token if it matches a parameter name.
    fn substitute_token(token: &str, params: &HashMap<String, String>) -> String {
        // Check for &PARAM style references
        if let Some(param_name) = token.strip_prefix('&') {
            if let Some(value) = params.get(param_name) {
                return value.clone();
            }
        }
        // Direct parameter name match
        if let Some(value) = params.get(token) {
            return value.clone();
        }
        token.to_string()
    }
}

// ---------------------------------------------------------------------------
// Macro library
// ---------------------------------------------------------------------------

/// Library of macro definitions for expansion.
#[derive(Debug, Default)]
pub struct MacroLibrary {
    /// Macros keyed by name.
    macros: HashMap<String, EzMacro>,
    /// Maximum expansion depth.
    pub max_depth: usize,
}

impl MacroLibrary {
    /// Create a new macro library.
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
            max_depth: 10,
        }
    }

    /// Register a macro definition.
    pub fn register(&mut self, mac: EzMacro) {
        self.macros.insert(mac.name.clone(), mac);
    }

    /// Look up a macro by name.
    pub fn get(&self, name: &str) -> Option<&EzMacro> {
        self.macros.get(name)
    }

    /// Expand a macro by name with arguments.
    pub fn expand(&self, name: &str, args: &[String]) -> Result<Vec<EzStatement>, MacroError> {
        let mac = self.macros.get(name).ok_or_else(|| MacroError::MacroNotDefined {
            name: name.to_string(),
        })?;
        mac.expand(args)
    }

    /// Get the number of registered macros.
    pub fn count(&self) -> usize {
        self.macros.len()
    }
}

// ---------------------------------------------------------------------------
// External call
// ---------------------------------------------------------------------------

/// Represents a CALL to an external program.
///
/// Easytrieve's CALL statement invokes an external load module,
/// passing parameters via a parameter list.
#[derive(Debug, Clone)]
pub struct EzExternalCall {
    /// External program name.
    pub program: String,
    /// Parameter list (field names or literals).
    pub parameters: Vec<String>,
    /// Return code from the last call.
    pub return_code: i32,
}

impl EzExternalCall {
    /// Create a new external call definition.
    pub fn new(program: &str) -> Self {
        Self {
            program: program.to_string(),
            parameters: Vec::new(),
            return_code: 0,
        }
    }

    /// Add a parameter to the call.
    pub fn add_parameter(&mut self, param: &str) {
        self.parameters.push(param.to_string());
    }

    /// Create from program name and parameter list.
    pub fn with_params(program: &str, params: Vec<String>) -> Self {
        Self {
            program: program.to_string(),
            parameters: params,
            return_code: 0,
        }
    }

    /// Simulate the external call (for testing).
    ///
    /// In a real implementation, this would use OS linkage conventions
    /// to invoke the external program.
    pub fn simulate(&mut self) -> Result<i32, MacroError> {
        // Simulated call — always returns 0
        self.return_code = 0;
        Ok(self.return_code)
    }
}

// ---------------------------------------------------------------------------
// Copy statement
// ---------------------------------------------------------------------------

/// Represents a COPY statement that includes external definitions.
///
/// COPY brings in field definitions or macro definitions from
/// a library member, similar to COBOL's COPY statement.
#[derive(Debug, Clone)]
pub struct EzCopy {
    /// Member name to copy.
    pub member: String,
    /// Resolved statements from the copy member.
    pub statements: Vec<EzStatement>,
}

impl EzCopy {
    /// Create a new copy reference.
    pub fn new(member: &str) -> Self {
        Self {
            member: member.to_string(),
            statements: Vec::new(),
        }
    }

    /// Set the resolved copy member content.
    pub fn set_content(&mut self, statements: Vec<EzStatement>) {
        self.statements = statements;
    }

    /// Check if the copy member has been resolved.
    pub fn is_resolved(&self) -> bool {
        !self.statements.is_empty()
    }

    /// Get the resolved statements.
    pub fn statements(&self) -> &[EzStatement] {
        &self.statements
    }
}

/// Copy library that maps member names to their statement content.
#[derive(Debug, Default)]
pub struct CopyLibrary {
    /// Members keyed by name.
    members: HashMap<String, Vec<EzStatement>>,
}

impl CopyLibrary {
    /// Create a new copy library.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a copy member.
    pub fn register(&mut self, name: &str, statements: Vec<EzStatement>) {
        self.members.insert(name.to_string(), statements);
    }

    /// Resolve a COPY statement by looking up the member.
    pub fn resolve(&self, copy: &mut EzCopy) -> Result<(), MacroError> {
        if let Some(stmts) = self.members.get(&copy.member) {
            copy.set_content(stmts.clone());
            Ok(())
        } else {
            Err(MacroError::CopyMemberNotFound {
                name: copy.member.clone(),
            })
        }
    }

    /// Get the number of registered members.
    pub fn count(&self) -> usize {
        self.members.len()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_macro_creation() {
        let mut mac = EzMacro::new("PRINT-HEADER");
        mac.add_parameter("TITLE");
        mac.add_statement(EzStatement::Display {
            items: vec!["&TITLE".into()],
        });

        assert_eq!(mac.name, "PRINT-HEADER");
        assert_eq!(mac.parameters, vec!["TITLE"]);
        assert_eq!(mac.body.len(), 1);
    }

    #[test]
    fn test_macro_expansion() {
        let mut mac = EzMacro::new("SHOW");
        mac.add_parameter("MSG");
        mac.add_statement(EzStatement::Display {
            items: vec!["&MSG".into()],
        });

        let expanded = mac.expand(&["'HELLO'".into()]).unwrap();
        assert_eq!(expanded.len(), 1);
        match &expanded[0] {
            EzStatement::Display { items } => {
                assert_eq!(items[0], "'HELLO'");
            }
            _ => panic!("expected Display statement"),
        }
    }

    #[test]
    fn test_macro_library() {
        let mut lib = MacroLibrary::new();

        let mut mac = EzMacro::new("TEST");
        mac.add_statement(EzStatement::Display {
            items: vec!["'test'".into()],
        });
        lib.register(mac);

        assert_eq!(lib.count(), 1);
        assert!(lib.get("TEST").is_some());
        assert!(lib.get("MISSING").is_none());
    }

    #[test]
    fn test_macro_library_expand() {
        let mut lib = MacroLibrary::new();

        let mut mac = EzMacro::new("GREET");
        mac.add_parameter("NAME");
        mac.add_statement(EzStatement::Display {
            items: vec!["'Hello'".into(), "&NAME".into()],
        });
        lib.register(mac);

        let expanded = lib.expand("GREET", &["World".into()]).unwrap();
        assert_eq!(expanded.len(), 1);
    }

    #[test]
    fn test_macro_library_expand_not_found() {
        let lib = MacroLibrary::new();
        let result = lib.expand("MISSING", &[]);
        assert!(result.is_err());
    }

    #[test]
    fn test_external_call() {
        let mut call = EzExternalCall::new("DATECONV");
        call.add_parameter("INPUT-DATE");
        call.add_parameter("OUTPUT-DATE");

        assert_eq!(call.program, "DATECONV");
        assert_eq!(call.parameters.len(), 2);
    }

    #[test]
    fn test_external_call_simulate() {
        let mut call = EzExternalCall::new("MYPROG");
        let rc = call.simulate().unwrap();
        assert_eq!(rc, 0);
        assert_eq!(call.return_code, 0);
    }

    #[test]
    fn test_external_call_with_params() {
        let call = EzExternalCall::with_params(
            "MYPROG",
            vec!["PARAM1".into(), "PARAM2".into()],
        );
        assert_eq!(call.program, "MYPROG");
        assert_eq!(call.parameters.len(), 2);
    }

    #[test]
    fn test_copy_creation() {
        let copy = EzCopy::new("EMPREC");
        assert_eq!(copy.member, "EMPREC");
        assert!(!copy.is_resolved());
    }

    #[test]
    fn test_copy_resolution() {
        let mut lib = CopyLibrary::new();
        lib.register(
            "EMPREC",
            vec![EzStatement::Define {
                name: "EMP-NAME".into(),
                file: None,
                position: Some(1),
                length: Some(20),
                data_type: Some("A".into()),
                value: None,
            }],
        );

        let mut copy = EzCopy::new("EMPREC");
        lib.resolve(&mut copy).unwrap();

        assert!(copy.is_resolved());
        assert_eq!(copy.statements().len(), 1);
    }

    #[test]
    fn test_copy_not_found() {
        let lib = CopyLibrary::new();
        let mut copy = EzCopy::new("MISSING");
        assert!(lib.resolve(&mut copy).is_err());
    }

    #[test]
    fn test_copy_library_count() {
        let mut lib = CopyLibrary::new();
        assert_eq!(lib.count(), 0);
        lib.register("A", vec![]);
        lib.register("B", vec![]);
        assert_eq!(lib.count(), 2);
    }

    #[test]
    fn test_macro_substitute_if() {
        let mut mac = EzMacro::new("CHECK");
        mac.add_parameter("FIELD");
        mac.add_parameter("VAL");
        mac.add_statement(EzStatement::If {
            condition: vec!["&FIELD".into(), ">".into(), "&VAL".into()],
        });

        let expanded = mac.expand(&["AMOUNT".into(), "100".into()]).unwrap();
        match &expanded[0] {
            EzStatement::If { condition } => {
                assert_eq!(condition[0], "AMOUNT");
                assert_eq!(condition[2], "100");
            }
            _ => panic!("expected If statement"),
        }
    }

    #[test]
    fn test_macro_substitute_goto() {
        let mut mac = EzMacro::new("JUMP");
        mac.add_parameter("LBL");
        mac.add_statement(EzStatement::GoTo {
            label: "&LBL".into(),
        });

        let expanded = mac.expand(&["EXIT-POINT".into()]).unwrap();
        match &expanded[0] {
            EzStatement::GoTo { label } => assert_eq!(label, "EXIT-POINT"),
            _ => panic!("expected GoTo"),
        }
    }
}
