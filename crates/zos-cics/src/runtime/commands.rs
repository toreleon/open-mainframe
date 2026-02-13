//! CICS command execution.
//!
//! Implements LINK, XCTL, RETURN, and other program control commands.

use super::{Commarea, Eib, FileManager, TransactionContext};
use crate::{CicsError, CicsResponse, CicsResult};
use std::collections::HashMap;

/// Result of executing a CICS program.
#[derive(Debug)]
pub enum ProgramResult {
    /// Normal return
    Return,
    /// Return with TRANSID for next transaction
    ReturnTransid(String),
    /// Return with COMMAREA
    ReturnCommarea(Commarea),
    /// XCTL to another program
    Xctl { program: String, commarea: Option<Commarea> },
    /// ABEND
    Abend(String),
}

/// Program entry point type.
pub type ProgramEntry = Box<dyn Fn(&mut CicsRuntime) -> CicsResult<ProgramResult>>;

/// Registry of available programs.
pub struct ProgramRegistry {
    programs: HashMap<String, ProgramEntry>,
}

impl ProgramRegistry {
    /// Create a new registry.
    pub fn new() -> Self {
        Self {
            programs: HashMap::new(),
        }
    }

    /// Register a program.
    pub fn register<F>(&mut self, name: &str, entry: F)
    where
        F: Fn(&mut CicsRuntime) -> CicsResult<ProgramResult> + 'static,
    {
        self.programs.insert(name.to_uppercase(), Box::new(entry));
    }

    /// Check if program exists.
    pub fn exists(&self, name: &str) -> bool {
        self.programs.contains_key(&name.to_uppercase())
    }

    /// Get program entry point.
    pub fn get(&self, name: &str) -> Option<&ProgramEntry> {
        self.programs.get(&name.to_uppercase())
    }
}

impl Default for ProgramRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// CICS runtime environment.
pub struct CicsRuntime {
    /// Execute Interface Block
    pub eib: Eib,
    /// Transaction context
    pub context: TransactionContext,
    /// File manager
    pub files: FileManager,
    /// Current COMMAREA
    commarea: Option<Commarea>,
    /// Program call stack
    call_stack: Vec<String>,
    /// Mock mode
    mock_mode: bool,
}

impl CicsRuntime {
    /// Create a new runtime.
    pub fn new(transaction_id: &str) -> Self {
        let mut eib = Eib::new();
        eib.set_transaction_id(transaction_id);

        Self {
            eib,
            context: TransactionContext::new(transaction_id),
            files: FileManager::new(),
            commarea: None,
            call_stack: Vec::new(),
            mock_mode: true,
        }
    }

    /// Get current COMMAREA.
    pub fn commarea(&self) -> Option<&Commarea> {
        self.commarea.as_ref()
    }

    /// Get mutable COMMAREA.
    pub fn commarea_mut(&mut self) -> Option<&mut Commarea> {
        self.commarea.as_mut()
    }

    /// Set COMMAREA.
    pub fn set_commarea(&mut self, commarea: Commarea) {
        self.eib.set_commarea_length(commarea.len() as u16);
        self.commarea = Some(commarea);
    }

    /// Execute LINK command.
    pub fn link(
        &mut self,
        program: &str,
        commarea: Option<Commarea>,
        registry: &ProgramRegistry,
    ) -> CicsResult<()> {
        self.eib.reset_for_command();

        // Check if program exists
        if !registry.exists(program) {
            self.eib.set_response(CicsResponse::Pgmiderr);
            if let Some(handler) = self.context.get_handler("PGMIDERR") {
                return Err(CicsError::ProgramNotFound(format!("{} -> {}", program, handler)));
            }
            return Err(CicsError::ProgramNotFound(program.to_string()));
        }

        // Save current state
        if let Some(ca) = commarea {
            self.set_commarea(ca);
        }
        self.call_stack.push(program.to_uppercase());

        // In mock mode, just simulate success
        if self.mock_mode {
            self.eib.set_response(CicsResponse::Normal);
            self.call_stack.pop();
            return Ok(());
        }

        // Execute program
        if let Some(entry) = registry.get(program) {
            let result = entry(self)?;
            self.call_stack.pop();

            match result {
                ProgramResult::Return | ProgramResult::ReturnCommarea(_) | ProgramResult::ReturnTransid(_) => {
                    self.eib.set_response(CicsResponse::Normal);
                }
                ProgramResult::Abend(code) => {
                    return Err(CicsError::InvalidRequest(format!("ABEND {}", code)));
                }
                ProgramResult::Xctl { .. } => {
                    // XCTL from linked program returns normally to caller
                    self.eib.set_response(CicsResponse::Normal);
                }
            }
        }

        Ok(())
    }

    /// Execute XCTL command.
    pub fn xctl(
        &mut self,
        program: &str,
        commarea: Option<Commarea>,
        registry: &ProgramRegistry,
    ) -> CicsResult<ProgramResult> {
        self.eib.reset_for_command();

        // Check if program exists
        if !registry.exists(program) {
            self.eib.set_response(CicsResponse::Pgmiderr);
            return Err(CicsError::ProgramNotFound(program.to_string()));
        }

        // Set COMMAREA
        if let Some(ca) = commarea.clone() {
            self.set_commarea(ca);
        }

        // Return XCTL result for caller to handle
        Ok(ProgramResult::Xctl {
            program: program.to_string(),
            commarea,
        })
    }

    /// Execute RETURN command.
    pub fn return_(&mut self, transid: Option<&str>, commarea: Option<Commarea>) -> CicsResult<ProgramResult> {
        self.eib.reset_for_command();
        self.eib.set_response(CicsResponse::Normal);

        if let Some(trans) = transid {
            Ok(ProgramResult::ReturnTransid(trans.to_string()))
        } else if let Some(ca) = commarea {
            Ok(ProgramResult::ReturnCommarea(ca))
        } else {
            Ok(ProgramResult::Return)
        }
    }

    /// Execute ABEND command.
    pub fn abend(&mut self, code: &str) -> CicsResult<ProgramResult> {
        // Check for abend handler
        if let Some(handler) = &self.context.abend_handler {
            return Err(CicsError::InvalidRequest(format!(
                "ABEND {} handled by {}",
                code, handler
            )));
        }

        Ok(ProgramResult::Abend(code.to_string()))
    }

    /// Execute GETMAIN command.
    pub fn getmain(&mut self, length: usize) -> CicsResult<Vec<u8>> {
        self.eib.reset_for_command();

        if length == 0 {
            self.eib.set_response(CicsResponse::Lengerr);
            return Err(CicsError::InvalidRequest("GETMAIN length must be > 0".to_string()));
        }

        self.eib.set_response(CicsResponse::Normal);
        Ok(vec![0; length])
    }

    /// Execute FREEMAIN command.
    pub fn freemain(&mut self) -> CicsResult<()> {
        self.eib.reset_for_command();
        self.eib.set_response(CicsResponse::Normal);
        Ok(())
    }

    /// Execute HANDLE CONDITION command.
    pub fn handle_condition(&mut self, condition: &str, label: &str) {
        self.context.handle_condition(condition, label);
    }

    /// Execute IGNORE CONDITION command.
    pub fn ignore_condition(&mut self, condition: &str) {
        self.context.ignore_condition(condition);
    }

    /// Execute HANDLE ABEND command.
    pub fn handle_abend(&mut self, label: &str) {
        self.context.handle_abend(label);
    }

    /// Get call stack depth.
    pub fn call_depth(&self) -> usize {
        self.call_stack.len()
    }
}

impl Default for CicsRuntime {
    fn default() -> Self {
        Self::new("DFLT")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_runtime_creation() {
        let runtime = CicsRuntime::new("MENU");
        assert_eq!(runtime.eib.transaction_id(), "MENU");
        assert_eq!(runtime.call_depth(), 0);
    }

    #[test]
    fn test_set_commarea() {
        let mut runtime = CicsRuntime::new("TEST");
        let ca = Commarea::new(100);
        runtime.set_commarea(ca);

        assert!(runtime.commarea().is_some());
        assert_eq!(runtime.eib.eibcalen, 100);
    }

    #[test]
    fn test_link_not_found() {
        let mut runtime = CicsRuntime::new("TEST");
        let registry = ProgramRegistry::new();

        // Mock mode - simulates program not found
        runtime.mock_mode = false;
        let result = runtime.link("NONEXIST", None, &registry);

        assert!(result.is_err());
        assert_eq!(runtime.eib.eibresp, CicsResponse::Pgmiderr as u32);
    }

    #[test]
    fn test_link_success_mock() {
        let mut runtime = CicsRuntime::new("TEST");
        let mut registry = ProgramRegistry::new();
        registry.register("SUBPROG", |_rt| Ok(ProgramResult::Return));

        let result = runtime.link("SUBPROG", None, &registry);

        assert!(result.is_ok());
        assert_eq!(runtime.eib.eibresp, CicsResponse::Normal as u32);
    }

    #[test]
    fn test_return() {
        let mut runtime = CicsRuntime::new("TEST");

        let result = runtime.return_(None, None).unwrap();
        assert!(matches!(result, ProgramResult::Return));
    }

    #[test]
    fn test_return_with_transid() {
        let mut runtime = CicsRuntime::new("TEST");

        let result = runtime.return_(Some("MENU"), None).unwrap();
        assert!(matches!(result, ProgramResult::ReturnTransid(ref t) if t == "MENU"));
    }

    #[test]
    fn test_xctl() {
        let mut runtime = CicsRuntime::new("TEST");
        let mut registry = ProgramRegistry::new();
        registry.register("NEXT", |_rt| Ok(ProgramResult::Return));

        let result = runtime.xctl("NEXT", None, &registry).unwrap();
        assert!(matches!(result, ProgramResult::Xctl { ref program, .. } if program == "NEXT"));
    }

    #[test]
    fn test_getmain() {
        let mut runtime = CicsRuntime::new("TEST");

        let data = runtime.getmain(1000).unwrap();
        assert_eq!(data.len(), 1000);
        assert!(data.iter().all(|&b| b == 0));
    }

    #[test]
    fn test_getmain_zero_length() {
        let mut runtime = CicsRuntime::new("TEST");

        let result = runtime.getmain(0);
        assert!(result.is_err());
        assert_eq!(runtime.eib.eibresp, CicsResponse::Lengerr as u32);
    }

    #[test]
    fn test_handle_condition() {
        let mut runtime = CicsRuntime::new("TEST");
        runtime.handle_condition("NOTFND", "NOT-FOUND-PARA");

        assert_eq!(runtime.context.get_handler("NOTFND"), Some("NOT-FOUND-PARA"));
    }

    #[test]
    fn test_ignore_condition() {
        let mut runtime = CicsRuntime::new("TEST");
        runtime.handle_condition("NOTFND", "LABEL1");
        runtime.ignore_condition("NOTFND");

        assert_eq!(runtime.context.get_handler("NOTFND"), None);
    }

    #[test]
    fn test_abend() {
        let mut runtime = CicsRuntime::new("TEST");

        let result = runtime.abend("ASRA").unwrap();
        assert!(matches!(result, ProgramResult::Abend(ref code) if code == "ASRA"));
    }

    #[test]
    fn test_program_registry() {
        let mut registry = ProgramRegistry::new();
        registry.register("TEST1", |_| Ok(ProgramResult::Return));
        registry.register("test2", |_| Ok(ProgramResult::Return)); // lowercase

        assert!(registry.exists("TEST1"));
        assert!(registry.exists("test1")); // case insensitive
        assert!(registry.exists("TEST2"));
        assert!(!registry.exists("NONEXIST"));
    }
}
