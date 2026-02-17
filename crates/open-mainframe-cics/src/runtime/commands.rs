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
    /// Return with channel
    ReturnChannel(String),
    /// XCTL to another program
    Xctl { program: String, commarea: Option<Commarea>, channel: Option<String> },
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
        self.link_with_channel(program, commarea, None, registry)
    }

    /// Execute LINK command with an optional channel.
    ///
    /// When a channel name is provided, it becomes the current channel
    /// for the called program. Modifications to containers are visible
    /// to the caller when the called program returns.
    pub fn link_with_channel(
        &mut self,
        program: &str,
        commarea: Option<Commarea>,
        channel: Option<&str>,
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

        // Set current channel if provided
        let prev_channel = self.context.channels.current_channel_name().map(|s| s.to_string());
        if let Some(ch_name) = channel {
            // Ensure the channel exists
            self.context.channels.get_or_create(ch_name);
            self.context.channels.set_current_channel(Some(ch_name.to_string()));
        }

        self.call_stack.push(program.to_uppercase());

        // In mock mode, just simulate success
        if self.mock_mode {
            self.eib.set_response(CicsResponse::Normal);
            self.call_stack.pop();
            // Restore previous channel
            self.context.channels.set_current_channel(prev_channel);
            return Ok(());
        }

        // Execute program
        if let Some(entry) = registry.get(program) {
            let result = entry(self)?;
            self.call_stack.pop();

            match result {
                ProgramResult::Return
                | ProgramResult::ReturnCommarea(_)
                | ProgramResult::ReturnTransid(_)
                | ProgramResult::ReturnChannel(_) => {
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

        // Restore previous channel (modifications to containers persist)
        self.context.channels.set_current_channel(prev_channel);

        Ok(())
    }

    /// Execute XCTL command.
    pub fn xctl(
        &mut self,
        program: &str,
        commarea: Option<Commarea>,
        registry: &ProgramRegistry,
    ) -> CicsResult<ProgramResult> {
        self.xctl_with_channel(program, commarea, None, registry)
    }

    /// Execute XCTL command with an optional channel.
    pub fn xctl_with_channel(
        &mut self,
        program: &str,
        commarea: Option<Commarea>,
        channel: Option<&str>,
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

        // Set channel if provided
        if let Some(ch_name) = channel {
            self.context.channels.get_or_create(ch_name);
            self.context.channels.set_current_channel(Some(ch_name.to_string()));
        }

        // Return XCTL result for caller to handle
        Ok(ProgramResult::Xctl {
            program: program.to_string(),
            commarea,
            channel: channel.map(|s| s.to_string()),
        })
    }

    /// Execute RETURN command.
    pub fn return_(&mut self, transid: Option<&str>, commarea: Option<Commarea>) -> CicsResult<ProgramResult> {
        self.return_with_channel(transid, commarea, None)
    }

    /// Execute RETURN command with optional channel.
    pub fn return_with_channel(
        &mut self,
        transid: Option<&str>,
        commarea: Option<Commarea>,
        channel: Option<&str>,
    ) -> CicsResult<ProgramResult> {
        self.eib.reset_for_command();
        self.eib.set_response(CicsResponse::Normal);

        if let Some(trans) = transid {
            Ok(ProgramResult::ReturnTransid(trans.to_string()))
        } else if let Some(ch_name) = channel {
            Ok(ProgramResult::ReturnChannel(ch_name.to_string()))
        } else if let Some(ca) = commarea {
            Ok(ProgramResult::ReturnCommarea(ca))
        } else {
            Ok(ProgramResult::Return)
        }
    }

    /// PUT CONTAINER — stores data in a container within a channel.
    pub fn put_container(
        &mut self,
        container_name: &str,
        channel_name: Option<&str>,
        data: &[u8],
    ) -> CicsResult<()> {
        self.eib.reset_for_command();
        let ch_name = self.resolve_channel_name(channel_name)?;
        self.context.channels.put_container(&ch_name, container_name, data)?;
        self.eib.set_response(CicsResponse::Normal);
        Ok(())
    }

    /// GET CONTAINER — retrieves data from a container.
    pub fn get_container(
        &self,
        container_name: &str,
        channel_name: Option<&str>,
    ) -> CicsResult<Vec<u8>> {
        let ch_name = self.resolve_channel_name(channel_name)?;
        let data = self.context.channels.get_container(&ch_name, container_name)?;
        Ok(data.to_vec())
    }

    /// DELETE CONTAINER — removes a container from a channel.
    pub fn delete_container(
        &mut self,
        container_name: &str,
        channel_name: Option<&str>,
    ) -> CicsResult<()> {
        self.eib.reset_for_command();
        let ch_name = self.resolve_channel_name(channel_name)?;
        self.context.channels.delete_container(&ch_name, container_name)?;
        self.eib.set_response(CicsResponse::Normal);
        Ok(())
    }

    /// Resolve channel name: use explicit name or fall back to current.
    fn resolve_channel_name(&self, explicit: Option<&str>) -> CicsResult<String> {
        if let Some(name) = explicit {
            Ok(name.to_uppercase())
        } else if let Some(name) = self.context.channels.current_channel_name() {
            Ok(name.to_string())
        } else {
            Err(CicsError::InvalidRequest(
                "No channel specified and no current channel set".to_string(),
            ))
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

    // === Story 200.2: Channel passing on LINK/XCTL/RETURN ===

    #[test]
    fn test_link_with_channel() {
        let mut runtime = CicsRuntime::new("TEST");
        let mut registry = ProgramRegistry::new();
        registry.register("SUBPROG", |_rt| Ok(ProgramResult::Return));

        // Set up channel with container data
        runtime.context.channels.put_container("MY-CHANNEL", "DATA1", b"payload").unwrap();

        // LINK with CHANNEL
        let result = runtime.link_with_channel("SUBPROG", None, Some("MY-CHANNEL"), &registry);
        assert!(result.is_ok());

        // Modified containers visible after return
        let data = runtime.context.channels.get_container("MY-CHANNEL", "DATA1").unwrap();
        assert_eq!(data, b"payload");
    }

    #[test]
    fn test_link_with_channel_callee_sees_channel() {
        let mut runtime = CicsRuntime::new("TEST");
        let mut registry = ProgramRegistry::new();

        // The subprogram reads from the channel
        registry.register("SUB", |rt| {
            let data = rt.get_container("DATA1", None)?;
            assert_eq!(&data, b"from caller");
            // Modify a container
            rt.put_container("RESULT", None, b"from callee")?;
            Ok(ProgramResult::Return)
        });

        // Caller sets up channel
        runtime.context.channels.put_container("MY-CHANNEL", "DATA1", b"from caller").unwrap();

        // LINK with channel — not in mock mode
        runtime.mock_mode = false;
        let result = runtime.link_with_channel("SUB", None, Some("MY-CHANNEL"), &registry);
        assert!(result.is_ok());

        // Caller can see the container added by callee
        let result_data = runtime.context.channels.get_container("MY-CHANNEL", "RESULT").unwrap();
        assert_eq!(result_data, b"from callee");
    }

    #[test]
    fn test_xctl_with_channel() {
        let mut runtime = CicsRuntime::new("TEST");
        let mut registry = ProgramRegistry::new();
        registry.register("NEXT", |_rt| Ok(ProgramResult::Return));

        runtime.context.channels.put_container("CH1", "D1", b"data").unwrap();

        let result = runtime.xctl_with_channel("NEXT", None, Some("CH1"), &registry).unwrap();
        assert!(matches!(result, ProgramResult::Xctl { ref channel, .. } if channel == &Some("CH1".to_string())));

        // Channel should now be current
        assert_eq!(runtime.context.channels.current_channel_name(), Some("CH1"));
    }

    #[test]
    fn test_return_with_channel() {
        let mut runtime = CicsRuntime::new("TEST");

        let result = runtime.return_with_channel(None, None, Some("MY-CHANNEL")).unwrap();
        assert!(matches!(result, ProgramResult::ReturnChannel(ref ch) if ch == "MY-CHANNEL"));
    }

    #[test]
    fn test_put_get_delete_container() {
        let mut runtime = CicsRuntime::new("TEST");

        // Set current channel
        runtime.context.channels.get_or_create("TEST-CH");
        runtime.context.channels.set_current_channel(Some("TEST-CH".to_string()));

        // PUT CONTAINER
        runtime.put_container("DATA1", None, b"hello").unwrap();

        // GET CONTAINER
        let data = runtime.get_container("DATA1", None).unwrap();
        assert_eq!(&data, b"hello");

        // DELETE CONTAINER
        runtime.delete_container("DATA1", None).unwrap();
        assert!(runtime.get_container("DATA1", None).is_err());
    }

    #[test]
    fn test_container_with_explicit_channel() {
        let mut runtime = CicsRuntime::new("TEST");

        // PUT with explicit channel name (auto-creates channel)
        runtime.put_container("ITEM", Some("MY-CH"), b"value").unwrap();

        // GET with explicit channel name
        let data = runtime.get_container("ITEM", Some("MY-CH")).unwrap();
        assert_eq!(&data, b"value");
    }

    #[test]
    fn test_container_no_channel_error() {
        let runtime = CicsRuntime::new("TEST");
        // No current channel set and no explicit channel
        let result = runtime.get_container("DATA1", None);
        assert!(result.is_err());
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
