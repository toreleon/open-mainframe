//! CICS command dispatcher — bridges preprocessor-generated CALL statements
//! to runtime command execution.
//!
//! When the CICS preprocessor transforms `EXEC CICS ...` into
//! `CALL "CICSxxxx" USING CICS-CMD-nnn DFHEIBLK`, the COBOL runtime
//! invokes this dispatcher. The dispatcher:
//!
//! 1. Identifies the command from the CALL target name (e.g., "CICSLINK")
//! 2. Deserializes the command parameter block
//! 3. Invokes the appropriate `CicsRuntime` method
//! 4. Sets EIBRESP/EIBRESP2 in the EIB for the COBOL program
//!
//! # Parameter Block Format
//!
//! Each command uses a `CommandParamBlock` containing key-value pairs
//! matching EXEC CICS option names. The preprocessor generates these as
//! COBOL data areas; the dispatcher deserializes them before invoking
//! the runtime.

use crate::CicsResponse;
use std::collections::HashMap;

/// A command parameter block deserialized from the COBOL data area.
///
/// Parameters are stored as key-value string pairs matching the
/// EXEC CICS option syntax (e.g., QUEUE='CSSL', FROM=LOG-REC).
#[derive(Debug, Clone, Default)]
pub struct CommandParamBlock {
    /// The command type identifier (e.g., "CICSLINK", "CICSREAD").
    pub command_id: String,
    /// Key-value parameters from the EXEC CICS statement.
    pub params: HashMap<String, String>,
}

impl CommandParamBlock {
    /// Create a new parameter block for a given command.
    pub fn new(command_id: &str) -> Self {
        Self {
            command_id: command_id.to_string(),
            params: HashMap::new(),
        }
    }

    /// Set a parameter value.
    pub fn set(&mut self, key: &str, value: &str) -> &mut Self {
        self.params.insert(key.to_uppercase(), value.to_string());
        self
    }

    /// Get a parameter value.
    pub fn get(&self, key: &str) -> Option<&str> {
        self.params.get(&key.to_uppercase()).map(|s| s.as_str())
    }

    /// Get a parameter, returning an error if missing.
    pub fn require(&self, key: &str) -> Result<&str, DispatchError> {
        self.get(key).ok_or_else(|| DispatchError::MissingParam {
            command: self.command_id.clone(),
            param: key.to_string(),
        })
    }

    /// Serialize to a text format: "KEY1=VALUE1;KEY2=VALUE2;..."
    pub fn to_text(&self) -> String {
        let mut parts: Vec<String> = self.params
            .iter()
            .map(|(k, v)| format!("{}={}", k, v))
            .collect();
        parts.sort();
        parts.join(";")
    }

    /// Deserialize from text format.
    pub fn from_text(command_id: &str, text: &str) -> Self {
        let mut block = Self::new(command_id);
        for part in text.split(';') {
            let part = part.trim();
            if part.is_empty() {
                continue;
            }
            if let Some((key, value)) = part.split_once('=') {
                block.params.insert(key.trim().to_uppercase(), value.trim().to_string());
            }
        }
        block
    }
}

/// Result of dispatching a CICS command.
#[derive(Debug, Clone)]
pub struct DispatchResult {
    /// EIBRESP value
    pub eibresp: u32,
    /// EIBRESP2 value
    pub eibresp2: u32,
    /// Output data (for READ-type commands)
    pub output_data: Option<Vec<u8>>,
    /// Output fields: named results (e.g., INTO data, PROGRAM name)
    pub output_fields: HashMap<String, String>,
}

impl DispatchResult {
    /// Create a success result.
    pub fn ok() -> Self {
        Self {
            eibresp: CicsResponse::Normal as u32,
            eibresp2: 0,
            output_data: None,
            output_fields: HashMap::new(),
        }
    }

    /// Create a success result with output data.
    pub fn ok_with_data(data: Vec<u8>) -> Self {
        Self {
            eibresp: CicsResponse::Normal as u32,
            eibresp2: 0,
            output_data: Some(data),
            output_fields: HashMap::new(),
        }
    }

    /// Create an error result with the current EIB values.
    fn from_eib(eib: &super::Eib) -> Self {
        Self {
            eibresp: eib.eibresp,
            eibresp2: eib.eibresp2,
            output_data: None,
            output_fields: HashMap::new(),
        }
    }
}

/// Errors from the dispatch layer.
#[derive(Debug, Clone)]
pub enum DispatchError {
    /// Unknown command name.
    UnknownCommand(String),
    /// Required parameter missing.
    MissingParam {
        command: String,
        param: String,
    },
    /// Runtime error from CicsRuntime.
    RuntimeError(String),
}

impl std::fmt::Display for DispatchError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DispatchError::UnknownCommand(cmd) => {
                write!(f, "Unknown CICS command: {}", cmd)
            }
            DispatchError::MissingParam { command, param } => {
                write!(f, "Missing parameter '{}' for command '{}'", param, command)
            }
            DispatchError::RuntimeError(msg) => {
                write!(f, "CICS runtime error: {}", msg)
            }
        }
    }
}

impl std::error::Error for DispatchError {}

/// CICS command dispatcher.
///
/// Routes preprocessor-generated CALL "CICSxxxx" statements to the
/// appropriate `CicsRuntime` methods. After each dispatch, the EIB
/// (DFHEIBLK) contains the EIBRESP and EIBRESP2 codes that the
/// COBOL program can inspect.
pub struct CicsDispatcher;

impl CicsDispatcher {
    /// Dispatch a command to the CICS runtime.
    ///
    /// The `call_name` is the CALL target (e.g., "CICSLINK", "CICSREAD").
    /// The `params` is the deserialized command parameter block.
    /// The `runtime` is the CICS runtime context.
    /// The `registry` is the program registry for LINK/XCTL (pass `None`
    /// if program control commands are not needed).
    ///
    /// Returns a `DispatchResult` with EIBRESP/EIBRESP2 and any output data.
    pub fn dispatch(
        call_name: &str,
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
        registry: Option<&super::ProgramRegistry>,
    ) -> Result<DispatchResult, DispatchError> {
        let call_upper = call_name.to_uppercase();
        match call_upper.as_str() {
            // Program control
            "CICSLINK" => Self::dispatch_link(params, runtime, registry),
            "CICSXCTL" => Self::dispatch_xctl(params, runtime, registry),
            "CICSRETN" => Self::dispatch_return(params, runtime),
            // File operations
            "CICSREAD" => Self::dispatch_read(params, runtime),
            "CICSWRIT" => Self::dispatch_write(params, runtime),
            "CICSRWRT" => Self::dispatch_rewrite(params, runtime),
            "CICSDELT" => Self::dispatch_delete(params, runtime),
            // TD queue operations
            "CICSRDTD" => Self::dispatch_readq_td(params, runtime),
            "CICSWRTD" => Self::dispatch_writeq_td(params, runtime),
            "CICSDLTD" => Self::dispatch_deleteq_td(params, runtime),
            // Container operations
            "CICSPUTC" => Self::dispatch_put_container(params, runtime),
            "CICSGETC" => Self::dispatch_get_container(params, runtime),
            "CICSDLCN" => Self::dispatch_delete_container(params, runtime),
            // Terminal I/O
            "CICSSEND" => Self::dispatch_send(params, runtime),
            "CICSRECV" => Self::dispatch_receive(params, runtime),
            "CICSCNVS" => Self::dispatch_converse(params, runtime),
            // System info
            "CICSASGN" => Self::dispatch_assign(params, runtime),
            // Browse operations
            "CICSSTBR" => Self::dispatch_startbr(params, runtime),
            "CICSRDNX" => Self::dispatch_readnext(params, runtime),
            "CICSRDPV" => Self::dispatch_readprev(params, runtime),
            "CICSRSBR" => Self::dispatch_resetbr(params, runtime),
            "CICSENDB" => Self::dispatch_endbr(params, runtime),
            // ENQ/DEQ
            "CICSENQ" => Self::dispatch_enq(params, runtime),
            "CICSDEQ" => Self::dispatch_deq(params, runtime),
            // Storage management
            "CICSGMN"  => Self::dispatch_getmain(params, runtime),
            // Condition handling
            "CICSHCND" => Self::dispatch_handle_condition(params, runtime),
            "CICSHABN" => Self::dispatch_handle_abend(params, runtime),
            "CICSABND" => Self::dispatch_abend(params, runtime),
            _ => Err(DispatchError::UnknownCommand(call_upper)),
        }
    }

    // --- Program Control ---

    fn dispatch_link(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
        registry: Option<&super::ProgramRegistry>,
    ) -> Result<DispatchResult, DispatchError> {
        let program = params.require("PROGRAM")?;
        let channel = params.get("CHANNEL");

        runtime.eib.reset_for_command();

        let reg = match registry {
            Some(r) => r,
            None => {
                runtime.eib.set_response(CicsResponse::Pgmiderr);
                return Ok(DispatchResult::from_eib(&runtime.eib));
            }
        };

        let result = if let Some(ch) = channel {
            runtime.link_with_channel(program, None, Some(ch), reg)
        } else {
            runtime.link(program, None, reg)
        };

        match result {
            Ok(()) => Ok(DispatchResult::ok()),
            Err(_) => Ok(DispatchResult::from_eib(&runtime.eib)),
        }
    }

    fn dispatch_xctl(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
        registry: Option<&super::ProgramRegistry>,
    ) -> Result<DispatchResult, DispatchError> {
        let program = params.require("PROGRAM")?;

        runtime.eib.reset_for_command();

        let reg = match registry {
            Some(r) => r,
            None => {
                runtime.eib.set_response(CicsResponse::Pgmiderr);
                return Ok(DispatchResult::from_eib(&runtime.eib));
            }
        };

        match runtime.xctl(program, None, reg) {
            Ok(_) => {
                let mut result = DispatchResult::ok();
                result.output_fields.insert("XCTL_PROGRAM".to_string(), program.to_string());
                Ok(result)
            }
            Err(_) => Ok(DispatchResult::from_eib(&runtime.eib)),
        }
    }

    fn dispatch_return(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        runtime.eib.reset_for_command();
        runtime.eib.set_response(CicsResponse::Normal);

        let mut result = DispatchResult::ok();
        if let Some(transid) = params.get("TRANSID") {
            result.output_fields.insert("TRANSID".to_string(), transid.to_string());
        }
        if let Some(channel) = params.get("CHANNEL") {
            result.output_fields.insert("CHANNEL".to_string(), channel.to_string());
        }
        Ok(result)
    }

    // --- File Operations ---

    fn dispatch_read(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let file = params.require("FILE")?;
        let ridfld = params.get("RIDFLD").unwrap_or("");

        match runtime.files.read(file, ridfld.as_bytes(), super::FileMode::Read) {
            Ok(record) => {
                runtime.eib.set_response(CicsResponse::Normal);
                Ok(DispatchResult::ok_with_data(record.data))
            }
            Err(_) => {
                Ok(DispatchResult::from_eib(&runtime.eib))
            }
        }
    }

    fn dispatch_write(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let file = params.require("FILE")?;
        let ridfld = params.get("RIDFLD").unwrap_or("");
        let from = params.get("FROM").unwrap_or("");

        let record = super::FileRecord {
            key: ridfld.as_bytes().to_vec(),
            data: from.as_bytes().to_vec(),
        };

        match runtime.files.write(file, record) {
            Ok(()) => {
                runtime.eib.set_response(CicsResponse::Normal);
                Ok(DispatchResult::ok())
            }
            Err(_) => {
                Ok(DispatchResult::from_eib(&runtime.eib))
            }
        }
    }

    fn dispatch_rewrite(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let file = params.require("FILE")?;
        let from = params.get("FROM").unwrap_or("");

        match runtime.files.rewrite(file, from.as_bytes()) {
            Ok(()) => {
                runtime.eib.set_response(CicsResponse::Normal);
                Ok(DispatchResult::ok())
            }
            Err(_) => {
                Ok(DispatchResult::from_eib(&runtime.eib))
            }
        }
    }

    fn dispatch_delete(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let file = params.require("FILE")?;
        let ridfld = params.get("RIDFLD");

        match runtime.files.delete(file, ridfld.map(|r| r.as_bytes())) {
            Ok(()) => {
                runtime.eib.set_response(CicsResponse::Normal);
                Ok(DispatchResult::ok())
            }
            Err(_) => {
                Ok(DispatchResult::from_eib(&runtime.eib))
            }
        }
    }

    // --- Browse Operations ---

    fn dispatch_startbr(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let file = params.require("FILE")
            .or_else(|_| params.require("DATASET"))?;
        let ridfld = params.get("RIDFLD").unwrap_or("");

        runtime.eib.reset_for_command();

        match runtime.files.startbr(file, ridfld.as_bytes()) {
            Ok(_token) => {
                runtime.eib.set_response(CicsResponse::Normal);
                Ok(DispatchResult::ok())
            }
            Err(_) => {
                Ok(DispatchResult::from_eib(&runtime.eib))
            }
        }
    }

    fn dispatch_readnext(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let file = params.require("FILE")
            .or_else(|_| params.require("DATASET"))?;

        runtime.eib.reset_for_command();

        // Look up the browse token by file name
        let token = match runtime.files.browse_token_for_file(file) {
            Ok(t) => t,
            Err(_) => {
                runtime.eib.set_response(CicsResponse::Invreq);
                return Ok(DispatchResult::from_eib(&runtime.eib));
            }
        };

        match runtime.files.readnext(token) {
            Ok(record) => {
                runtime.eib.set_response(CicsResponse::Normal);
                Ok(DispatchResult::ok_with_data(record.data))
            }
            Err(_) => {
                Ok(DispatchResult::from_eib(&runtime.eib))
            }
        }
    }

    fn dispatch_readprev(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let file = params.require("FILE")
            .or_else(|_| params.require("DATASET"))?;

        runtime.eib.reset_for_command();

        let token = match runtime.files.browse_token_for_file(file) {
            Ok(t) => t,
            Err(_) => {
                runtime.eib.set_response(CicsResponse::Invreq);
                return Ok(DispatchResult::from_eib(&runtime.eib));
            }
        };

        match runtime.files.readprev(token) {
            Ok(record) => {
                runtime.eib.set_response(CicsResponse::Normal);
                Ok(DispatchResult::ok_with_data(record.data))
            }
            Err(_) => {
                Ok(DispatchResult::from_eib(&runtime.eib))
            }
        }
    }

    fn dispatch_resetbr(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let file = params.require("FILE")
            .or_else(|_| params.require("DATASET"))?;
        let ridfld = params.get("RIDFLD").unwrap_or("");

        runtime.eib.reset_for_command();

        let token = match runtime.files.browse_token_for_file(file) {
            Ok(t) => t,
            Err(_) => {
                runtime.eib.set_response(CicsResponse::Invreq);
                return Ok(DispatchResult::from_eib(&runtime.eib));
            }
        };

        match runtime.files.resetbr(token, ridfld.as_bytes()) {
            Ok(()) => {
                runtime.eib.set_response(CicsResponse::Normal);
                Ok(DispatchResult::ok())
            }
            Err(_) => {
                Ok(DispatchResult::from_eib(&runtime.eib))
            }
        }
    }

    fn dispatch_endbr(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let file = params.require("FILE")
            .or_else(|_| params.require("DATASET"))?;

        runtime.eib.reset_for_command();

        let token = match runtime.files.browse_token_for_file(file) {
            Ok(t) => t,
            Err(_) => {
                runtime.eib.set_response(CicsResponse::Invreq);
                return Ok(DispatchResult::from_eib(&runtime.eib));
            }
        };

        match runtime.files.endbr(token) {
            Ok(()) => {
                runtime.eib.set_response(CicsResponse::Normal);
                Ok(DispatchResult::ok())
            }
            Err(_) => {
                Ok(DispatchResult::from_eib(&runtime.eib))
            }
        }
    }

    // --- TD Queue Operations ---

    fn dispatch_readq_td(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let queue = params.require("QUEUE")?;

        match runtime.readq_td(queue) {
            Ok(data) => Ok(DispatchResult::ok_with_data(data)),
            Err(_) => Ok(DispatchResult::from_eib(&runtime.eib)),
        }
    }

    fn dispatch_writeq_td(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let queue = params.require("QUEUE")?;
        let from = params.get("FROM").unwrap_or("");

        match runtime.writeq_td(queue, from.as_bytes()) {
            Ok(()) => Ok(DispatchResult::ok()),
            Err(_) => Ok(DispatchResult::from_eib(&runtime.eib)),
        }
    }

    fn dispatch_deleteq_td(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let queue = params.require("QUEUE")?;

        match runtime.deleteq_td(queue) {
            Ok(()) => Ok(DispatchResult::ok()),
            Err(_) => Ok(DispatchResult::from_eib(&runtime.eib)),
        }
    }

    // --- Container Operations ---

    fn dispatch_put_container(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let container = params.require("CONTAINER")?;
        let channel = params.get("CHANNEL");
        let from = params.get("FROM").unwrap_or("");

        match runtime.put_container(container, channel, from.as_bytes()) {
            Ok(()) => Ok(DispatchResult::ok()),
            Err(_) => Ok(DispatchResult::from_eib(&runtime.eib)),
        }
    }

    fn dispatch_get_container(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let container = params.require("CONTAINER")?;
        let channel = params.get("CHANNEL");

        match runtime.get_container(container, channel) {
            Ok(data) => Ok(DispatchResult::ok_with_data(data)),
            Err(_) => Ok(DispatchResult::from_eib(&runtime.eib)),
        }
    }

    fn dispatch_delete_container(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let container = params.require("CONTAINER")?;
        let channel = params.get("CHANNEL");

        match runtime.delete_container(container, channel) {
            Ok(()) => Ok(DispatchResult::ok()),
            Err(_) => Ok(DispatchResult::from_eib(&runtime.eib)),
        }
    }

    // --- Terminal I/O ---

    fn dispatch_send(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let from = params.get("FROM").unwrap_or("");
        let erase = params.get("ERASE").is_some();

        match runtime.send_data(from.as_bytes(), erase) {
            Ok(()) => Ok(DispatchResult::ok()),
            Err(_) => Ok(DispatchResult::from_eib(&runtime.eib)),
        }
    }

    fn dispatch_receive(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let max_length: usize = params.get("MAXLENGTH")
            .and_then(|s| s.parse().ok())
            .unwrap_or(32767);

        match runtime.receive_data(max_length) {
            Ok(data) => Ok(DispatchResult::ok_with_data(data)),
            Err(_) => Ok(DispatchResult::from_eib(&runtime.eib)),
        }
    }

    fn dispatch_converse(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let from = params.get("FROM").unwrap_or("");
        let max_length: usize = params.get("MAXLENGTH")
            .and_then(|s| s.parse().ok())
            .unwrap_or(32767);
        let erase = params.get("ERASE").is_some();

        match runtime.converse(from.as_bytes(), max_length, erase) {
            Ok(data) => Ok(DispatchResult::ok_with_data(data)),
            Err(_) => Ok(DispatchResult::from_eib(&runtime.eib)),
        }
    }

    // --- ENQ/DEQ ---

    fn dispatch_enq(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let resource = params.require("RESOURCE")?;
        let nosuspend = params.get("NOSUSPEND").is_some();
        let lock_type = if params.get("LENGTH").is_some() {
            // LENGTH implies shared lock semantics for partial resource
            crate::sync::LockType::Shared
        } else {
            crate::sync::LockType::Exclusive
        };

        runtime.eib.reset_for_command();
        let task_id = runtime.eib.eibtaskn;

        match runtime.enqueue.enq(resource, lock_type, task_id, nosuspend) {
            Ok(()) => {
                runtime.eib.set_response(CicsResponse::Normal);
                Ok(DispatchResult::ok())
            }
            Err(crate::sync::SyncError::ResourceBusy) => {
                runtime.eib.set_response(CicsResponse::Enqbusy);
                Ok(DispatchResult::from_eib(&runtime.eib))
            }
            Err(crate::sync::SyncError::Deadlock { .. }) => {
                // AEYD abend — deadlock detected
                runtime.eib.set_response(CicsResponse::Error);
                runtime.eib.set_response2(2); // Deadlock indicator
                Ok(DispatchResult::from_eib(&runtime.eib))
            }
            Err(_) => {
                runtime.eib.set_response(CicsResponse::Error);
                Ok(DispatchResult::from_eib(&runtime.eib))
            }
        }
    }

    fn dispatch_deq(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let resource = params.require("RESOURCE")?;

        runtime.eib.reset_for_command();
        let task_id = runtime.eib.eibtaskn;

        match runtime.enqueue.deq(resource, task_id) {
            Ok(()) => {
                runtime.eib.set_response(CicsResponse::Normal);
                Ok(DispatchResult::ok())
            }
            Err(_) => {
                runtime.eib.set_response(CicsResponse::Error);
                Ok(DispatchResult::from_eib(&runtime.eib))
            }
        }
    }

    // --- System Info ---

    fn dispatch_assign(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        // The params contain the field names the program wants to retrieve.
        // Each key in the params is a field name (e.g., SYSID, USERID, etc.)
        // and the value is the COBOL variable to store the result.
        let field_names: Vec<&str> = params.params.keys().map(|k| k.as_str()).collect();
        let values = runtime.assign(&field_names);

        let mut result = DispatchResult::ok();
        for (key, value) in values {
            result.output_fields.insert(key, value);
        }
        Ok(result)
    }

    // --- Storage Management ---

    fn dispatch_getmain(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let length_str = params.get("LENGTH").unwrap_or("0");
        let length: usize = length_str.parse().unwrap_or(0);

        match runtime.getmain(length) {
            Ok(data) => Ok(DispatchResult::ok_with_data(data)),
            Err(e) => Err(DispatchError::RuntimeError(e.to_string())),
        }
    }

    // --- Condition Handling ---

    fn dispatch_handle_condition(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        for (key, value) in &params.params {
            runtime.handle_condition(key, value);
        }
        Ok(DispatchResult::ok())
    }

    fn dispatch_handle_abend(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let label = params.get("LABEL")
            .or_else(|| params.get("PROGRAM"))
            .unwrap_or("");
        runtime.handle_abend(label);
        Ok(DispatchResult::ok())
    }

    fn dispatch_abend(
        params: &CommandParamBlock,
        runtime: &mut super::CicsRuntime,
    ) -> Result<DispatchResult, DispatchError> {
        let code = params.get("ABCODE").unwrap_or("????");
        match runtime.abend(code) {
            Ok(_) => Ok(DispatchResult::ok()),
            Err(e) => Err(DispatchError::RuntimeError(e.to_string())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::CicsRuntime;
    use crate::queues::td::{TdQueue, TdDestType};

    // === Story 206.1: Parameter Block Format ===

    #[test]
    fn test_param_block_new() {
        let block = CommandParamBlock::new("CICSREAD");
        assert_eq!(block.command_id, "CICSREAD");
        assert!(block.params.is_empty());
    }

    #[test]
    fn test_param_block_set_get() {
        let mut block = CommandParamBlock::new("CICSREAD");
        block.set("FILE", "CUSTFILE");
        block.set("RIDFLD", "12345");
        block.set("KEYLENGTH", "5");

        assert_eq!(block.get("FILE"), Some("CUSTFILE"));
        assert_eq!(block.get("RIDFLD"), Some("12345"));
        assert_eq!(block.get("KEYLENGTH"), Some("5"));
        assert_eq!(block.get("MISSING"), None);
    }

    #[test]
    fn test_param_block_require() {
        let mut block = CommandParamBlock::new("CICSREAD");
        block.set("FILE", "CUSTFILE");

        assert!(block.require("FILE").is_ok());
        assert!(block.require("MISSING").is_err());
    }

    #[test]
    fn test_param_block_serialization_roundtrip() {
        let mut block = CommandParamBlock::new("CICSREAD");
        block.set("FILE", "CUSTFILE");
        block.set("RIDFLD", "12345");

        let text = block.to_text();
        assert!(text.contains("FILE=CUSTFILE"));
        assert!(text.contains("RIDFLD=12345"));

        let restored = CommandParamBlock::from_text("CICSREAD", &text);
        assert_eq!(restored.get("FILE"), Some("CUSTFILE"));
        assert_eq!(restored.get("RIDFLD"), Some("12345"));
    }

    #[test]
    fn test_param_block_case_insensitive() {
        let mut block = CommandParamBlock::new("TEST");
        block.set("queue", "TDQ1");
        assert_eq!(block.get("QUEUE"), Some("TDQ1"));
        assert_eq!(block.get("queue"), Some("TDQ1"));
    }

    // === Story 206.2: Dispatcher Integration ===

    #[test]
    fn test_dispatch_unknown_command() {
        let mut runtime = CicsRuntime::new("TEST");
        let params = CommandParamBlock::new("UNKNOWN");
        let result = CicsDispatcher::dispatch("UNKNOWN", &params, &mut runtime, None);
        assert!(matches!(result, Err(DispatchError::UnknownCommand(_))));
    }

    #[test]
    fn test_dispatch_missing_required_param() {
        let mut runtime = CicsRuntime::new("TEST");
        let params = CommandParamBlock::new("CICSLINK");
        // Missing PROGRAM parameter
        let result = CicsDispatcher::dispatch("CICSLINK", &params, &mut runtime, None);
        assert!(matches!(result, Err(DispatchError::MissingParam { .. })));
    }

    #[test]
    fn test_dispatch_link_pgmiderr() {
        // Story 206.2 AC: When runtime sets EIBRESP=27 (PGMIDERR),
        // DFHEIBLK contains EIBRESP=27
        let mut runtime = CicsRuntime::new("TEST");
        let registry = super::super::ProgramRegistry::new();
        let mut params = CommandParamBlock::new("CICSLINK");
        params.set("PROGRAM", "NOTEXIST");

        let result = CicsDispatcher::dispatch("CICSLINK", &params, &mut runtime, Some(&registry)).unwrap();
        assert_eq!(result.eibresp, CicsResponse::Pgmiderr as u32);
        assert_eq!(runtime.eib.eibresp, CicsResponse::Pgmiderr as u32,
            "DFHEIBLK EIBRESP should be set to PGMIDERR");
    }

    #[test]
    fn test_dispatch_writeq_td() {
        let mut runtime = CicsRuntime::new("TEST");
        runtime.td_queues.define_queue(TdQueue::new("CSSL", TdDestType::Intrapartition));

        let mut params = CommandParamBlock::new("CICSWRTD");
        params.set("QUEUE", "CSSL");
        params.set("FROM", "Log entry");

        let result = CicsDispatcher::dispatch("CICSWRTD", &params, &mut runtime, None).unwrap();
        assert_eq!(result.eibresp, CicsResponse::Normal as u32);
    }

    #[test]
    fn test_dispatch_readq_td() {
        let mut runtime = CicsRuntime::new("TEST");
        runtime.td_queues.define_queue(TdQueue::new("CSSL", TdDestType::Intrapartition));
        runtime.writeq_td("CSSL", b"Test record").unwrap();

        let mut params = CommandParamBlock::new("CICSRDTD");
        params.set("QUEUE", "CSSL");

        let result = CicsDispatcher::dispatch("CICSRDTD", &params, &mut runtime, None).unwrap();
        assert_eq!(result.eibresp, CicsResponse::Normal as u32);
        assert_eq!(result.output_data, Some(b"Test record".to_vec()));
    }

    #[test]
    fn test_dispatch_readq_td_sets_eibresp_qzero() {
        let mut runtime = CicsRuntime::new("TEST");
        runtime.td_queues.define_queue(TdQueue::new("CSSL", TdDestType::Intrapartition));

        let mut params = CommandParamBlock::new("CICSRDTD");
        params.set("QUEUE", "CSSL");

        let result = CicsDispatcher::dispatch("CICSRDTD", &params, &mut runtime, None).unwrap();
        assert_eq!(result.eibresp, CicsResponse::Qzero as u32,
            "EIBRESP should be QZERO for empty queue");
    }

    #[test]
    fn test_dispatch_eibresp_accessible_after_call() {
        // Story 206.2 AC: When the CALL returns, DFHEIBLK contains
        // EIBRESP accessible by the COBOL program
        let mut runtime = CicsRuntime::new("TEST");
        let registry = super::super::ProgramRegistry::new();

        let mut params = CommandParamBlock::new("CICSLINK");
        params.set("PROGRAM", "NOTEXIST");

        let result = CicsDispatcher::dispatch("CICSLINK", &params, &mut runtime, Some(&registry)).unwrap();
        // After dispatch, the EIB contains the result
        assert_eq!(runtime.eib.eibresp, result.eibresp,
            "EIB EIBRESP should match dispatch result");
    }

    #[test]
    fn test_dispatch_handle_condition() {
        let mut runtime = CicsRuntime::new("TEST");
        let mut params = CommandParamBlock::new("CICSHCND");
        params.set("NOTFND", "NOT-FOUND-PARA");

        CicsDispatcher::dispatch("CICSHCND", &params, &mut runtime, None).unwrap();
        assert_eq!(runtime.context.get_handler("NOTFND"), Some("NOT-FOUND-PARA"));
    }

    #[test]
    fn test_dispatch_put_get_container() {
        let mut runtime = CicsRuntime::new("TEST");

        // PUT CONTAINER
        let mut params = CommandParamBlock::new("CICSPUTC");
        params.set("CONTAINER", "DATA1");
        params.set("CHANNEL", "MYCHAN");
        params.set("FROM", "Hello");

        let result = CicsDispatcher::dispatch("CICSPUTC", &params, &mut runtime, None).unwrap();
        assert_eq!(result.eibresp, CicsResponse::Normal as u32);

        // GET CONTAINER
        let mut params = CommandParamBlock::new("CICSGETC");
        params.set("CONTAINER", "DATA1");
        params.set("CHANNEL", "MYCHAN");

        let result = CicsDispatcher::dispatch("CICSGETC", &params, &mut runtime, None).unwrap();
        assert_eq!(result.eibresp, CicsResponse::Normal as u32);
        assert_eq!(result.output_data, Some(b"Hello".to_vec()));
    }

    // === Story 203.1: CONVERSE dispatch ===

    #[test]
    fn test_dispatch_converse() {
        let mut runtime = CicsRuntime::new("TEST");
        runtime.set_converse_response(b"RESPONSE");

        let mut params = CommandParamBlock::new("CICSCNVS");
        params.set("FROM", "ENTER DATA:");
        params.set("MAXLENGTH", "80");

        let result = CicsDispatcher::dispatch("CICSCNVS", &params, &mut runtime, None).unwrap();
        assert_eq!(result.eibresp, CicsResponse::Normal as u32);
        assert_eq!(result.output_data, Some(b"RESPONSE".to_vec()));
    }

    #[test]
    fn test_dispatch_send() {
        let mut runtime = CicsRuntime::new("TEST");

        let mut params = CommandParamBlock::new("CICSSEND");
        params.set("FROM", "HELLO");

        let result = CicsDispatcher::dispatch("CICSSEND", &params, &mut runtime, None).unwrap();
        assert_eq!(result.eibresp, CicsResponse::Normal as u32);
    }

    #[test]
    fn test_dispatch_receive() {
        let mut runtime = CicsRuntime::new("TEST");
        runtime.set_terminal_input(b"INPUT DATA");

        let mut params = CommandParamBlock::new("CICSRECV");
        params.set("MAXLENGTH", "80");

        let result = CicsDispatcher::dispatch("CICSRECV", &params, &mut runtime, None).unwrap();
        assert_eq!(result.eibresp, CicsResponse::Normal as u32);
        assert_eq!(result.output_data, Some(b"INPUT DATA".to_vec()));
    }

    // === Story 205.1: ASSIGN dispatch ===

    #[test]
    fn test_dispatch_assign() {
        let mut runtime = CicsRuntime::new("MENU");
        runtime.context.user_id = Some("ADMIN".to_string());

        let mut params = CommandParamBlock::new("CICSASGN");
        params.set("SYSID", "WS-SYS");
        params.set("USERID", "WS-USER");

        let result = CicsDispatcher::dispatch("CICSASGN", &params, &mut runtime, None).unwrap();
        assert_eq!(result.eibresp, CicsResponse::Normal as u32);
        assert_eq!(result.output_fields.get("SYSID"), Some(&"CICS".to_string()));
        assert_eq!(result.output_fields.get("USERID"), Some(&"ADMIN".to_string()));
    }

    // === Story 207.1/207.2: ENQ/DEQ dispatch ===

    #[test]
    fn test_dispatch_enq_deq() {
        let mut runtime = CicsRuntime::new("TEST");
        runtime.eib.eibtaskn = 1;

        let mut params = CommandParamBlock::new("CICSENQ");
        params.set("RESOURCE", "MY-RES");

        let result = CicsDispatcher::dispatch("CICSENQ", &params, &mut runtime, None).unwrap();
        assert_eq!(result.eibresp, CicsResponse::Normal as u32);

        // DEQ
        let mut params = CommandParamBlock::new("CICSDEQ");
        params.set("RESOURCE", "MY-RES");

        let result = CicsDispatcher::dispatch("CICSDEQ", &params, &mut runtime, None).unwrap();
        assert_eq!(result.eibresp, CicsResponse::Normal as u32);
    }

    #[test]
    fn test_dispatch_enq_nosuspend_busy() {
        let mut runtime = CicsRuntime::new("TEST");
        runtime.eib.eibtaskn = 1;

        // Task 1 locks the resource
        runtime.enqueue.enq("MY-RES", crate::sync::LockType::Exclusive, 1, false).unwrap();

        // Task 2 tries with NOSUSPEND
        runtime.eib.eibtaskn = 2;
        let mut params = CommandParamBlock::new("CICSENQ");
        params.set("RESOURCE", "MY-RES");
        params.set("NOSUSPEND", "");

        let result = CicsDispatcher::dispatch("CICSENQ", &params, &mut runtime, None).unwrap();
        assert_eq!(result.eibresp, CicsResponse::Enqbusy as u32);
    }

    #[test]
    fn test_dispatch_return_with_transid() {
        let mut runtime = CicsRuntime::new("TEST");
        let mut params = CommandParamBlock::new("CICSRETN");
        params.set("TRANSID", "MENU");

        let result = CicsDispatcher::dispatch("CICSRETN", &params, &mut runtime, None).unwrap();
        assert_eq!(result.eibresp, CicsResponse::Normal as u32);
        assert_eq!(result.output_fields.get("TRANSID"), Some(&"MENU".to_string()));
    }
}
