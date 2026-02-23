//! # z/OS Utility Programs
//!
//! Implementation of standard z/OS utility programs for the OpenMainframe project.
//!
//! ## Features
//!
//! - **UtilityProgram trait** — common interface for all utility programs
//! - **UtilityRegistry** — dispatch `EXEC PGM=` to registered implementations
//! - **DD handling** — standard SYSUT1/SYSUT2/SYSIN/SYSPRINT allocation
//! - **Condition codes** — 0, 4, 8, 12, 16 per IBM convention
//! - **IBM message format** — `IEBnnnnS` severity-coded messages
//!
//! ## Example
//!
//! ```rust
//! use open_mainframe_utilities::{UtilityRegistry, UtilityContext, Iefbr14};
//!
//! let mut registry = UtilityRegistry::new();
//! registry.register(Box::new(Iefbr14));
//!
//! let mut ctx = UtilityContext::new("STEP01", "IEFBR14");
//! let result = registry.dispatch("IEFBR14", &mut ctx).unwrap();
//! assert_eq!(result.condition_code, 0);
//! ```

pub mod error;
pub mod iebcompr;
pub mod iebcopy;
pub mod iebdg;
pub mod iebgener;
pub mod iebptpch;
pub mod iebupdte;

use std::collections::{BTreeMap, HashMap};
use std::fmt;

use tracing::info;

pub use error::UtilityError;

/// Convenience result type for utility operations.
pub type Result<T> = std::result::Result<T, UtilityError>;

// ─────────────────────── Utility Message ───────────────────────

/// Severity level for utility messages, following IBM conventions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MessageSeverity {
    /// Informational message.
    Info,
    /// Warning — processing continues with possible issues.
    Warning,
    /// Error — step may have incorrect results.
    Error,
    /// Severe — processing cannot continue.
    Severe,
}

impl MessageSeverity {
    /// Returns the single-character severity code used in IBM message IDs.
    pub fn code(&self) -> char {
        match self {
            Self::Info => 'I',
            Self::Warning => 'W',
            Self::Error => 'E',
            Self::Severe => 'S',
        }
    }

    /// Create from the IBM severity character.
    pub fn from_code(c: char) -> Option<Self> {
        match c.to_ascii_uppercase() {
            'I' => Some(Self::Info),
            'W' => Some(Self::Warning),
            'E' => Some(Self::Error),
            'S' => Some(Self::Severe),
            _ => None,
        }
    }
}

impl fmt::Display for MessageSeverity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code())
    }
}

/// A utility message following IBM message ID conventions.
///
/// Format: `IEB1013I COPY OPERATION SUCCESSFUL`
///         ^^^^^^^^ ^^^^^^^^^^^^^^^^^^^^^^^^^^
///          msg_id         text
#[derive(Debug, Clone)]
pub struct UtilityMessage {
    /// Message ID (e.g., "IEB1013I").
    pub id: String,
    /// Severity level.
    pub severity: MessageSeverity,
    /// Message text.
    pub text: String,
}

impl UtilityMessage {
    /// Create a new utility message.
    pub fn new(id: &str, severity: MessageSeverity, text: &str) -> Self {
        Self {
            id: id.to_string(),
            severity,
            text: text.to_string(),
        }
    }

    /// Create an informational message.
    pub fn info(id: &str, text: &str) -> Self {
        Self::new(id, MessageSeverity::Info, text)
    }

    /// Create a warning message.
    pub fn warning(id: &str, text: &str) -> Self {
        Self::new(id, MessageSeverity::Warning, text)
    }

    /// Create an error message.
    pub fn error(id: &str, text: &str) -> Self {
        Self::new(id, MessageSeverity::Error, text)
    }

    /// Create a severe error message.
    pub fn severe(id: &str, text: &str) -> Self {
        Self::new(id, MessageSeverity::Severe, text)
    }
}

impl fmt::Display for UtilityMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.id, self.text)
    }
}

/// Format a message ID following IBM conventions.
///
/// `format_message_id("IEB", 1013, MessageSeverity::Info)` → `"IEB1013I"`
pub fn format_message_id(prefix: &str, number: u32, severity: MessageSeverity) -> String {
    format!("{}{:04}{}", prefix, number, severity.code())
}

// ─────────────────────── Utility Result ───────────────────────

/// Result of executing a utility program.
#[derive(Debug, Clone)]
pub struct UtilityResult {
    /// Condition code (0, 4, 8, 12, or 16).
    pub condition_code: u32,
    /// Messages generated during execution.
    pub messages: Vec<UtilityMessage>,
}

impl UtilityResult {
    /// Create a successful result (CC=0).
    pub fn success() -> Self {
        Self {
            condition_code: 0,
            messages: Vec::new(),
        }
    }

    /// Create a successful result with messages.
    pub fn success_with(messages: Vec<UtilityMessage>) -> Self {
        Self {
            condition_code: 0,
            messages,
        }
    }

    /// Create a result with the given condition code.
    pub fn with_cc(condition_code: u32) -> Self {
        Self {
            condition_code,
            messages: Vec::new(),
        }
    }

    /// Create a result with condition code and messages.
    pub fn new(condition_code: u32, messages: Vec<UtilityMessage>) -> Self {
        Self {
            condition_code,
            messages,
        }
    }

    /// Whether the condition code indicates success (CC=0).
    pub fn is_success(&self) -> bool {
        self.condition_code == 0
    }

    /// Whether the condition code indicates a warning (CC=4).
    pub fn is_warning(&self) -> bool {
        self.condition_code == 4
    }

    /// Whether the condition code indicates an error (CC >= 8).
    pub fn is_error(&self) -> bool {
        self.condition_code >= 8
    }
}

// ─────────────────────── DD Allocation ───────────────────────

// ─────────────────────── PDS In-Memory Data ───────────────────────

/// In-memory representation of a PDS member for utility processing.
#[derive(Debug, Clone)]
pub struct PdsMemberData {
    /// Member name (uppercase, up to 8 characters).
    pub name: String,
    /// Member content (records/lines).
    pub content: Vec<String>,
    /// Whether this member has been "deleted" (for compress tracking).
    pub deleted: bool,
}

impl PdsMemberData {
    /// Create a new PDS member with content.
    pub fn new(name: &str, content: Vec<String>) -> Self {
        Self {
            name: name.to_uppercase(),
            content,
            deleted: false,
        }
    }
}

/// In-memory PDS dataset for utility processing.
///
/// Simulates a Partitioned Data Set with members accessible by name.
#[derive(Debug, Clone, Default)]
pub struct PdsData {
    /// Members indexed by uppercase name, stored in order.
    pub members: BTreeMap<String, PdsMemberData>,
}

impl PdsData {
    /// Create a new empty PDS.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a member to the PDS.
    pub fn add_member(&mut self, name: &str, content: Vec<String>) {
        let key = name.to_uppercase();
        self.members
            .insert(key.clone(), PdsMemberData::new(&key, content));
    }

    /// Check if a member exists (and is not deleted).
    pub fn has_member(&self, name: &str) -> bool {
        self.members
            .get(&name.to_uppercase())
            .map_or(false, |m| !m.deleted)
    }

    /// Get a member's content.
    pub fn get_member(&self, name: &str) -> Option<&PdsMemberData> {
        self.members
            .get(&name.to_uppercase())
            .filter(|m| !m.deleted)
    }

    /// Delete a member (marks as deleted for compress tracking).
    pub fn delete_member(&mut self, name: &str) -> bool {
        if let Some(m) = self.members.get_mut(&name.to_uppercase()) {
            m.deleted = true;
            true
        } else {
            false
        }
    }

    /// List active (non-deleted) member names in sorted order.
    pub fn member_names(&self) -> Vec<String> {
        self.members
            .values()
            .filter(|m| !m.deleted)
            .map(|m| m.name.clone())
            .collect()
    }

    /// Number of active members.
    pub fn member_count(&self) -> usize {
        self.members.values().filter(|m| !m.deleted).count()
    }

    /// Number of deleted members (for compress tracking).
    pub fn deleted_count(&self) -> usize {
        self.members.values().filter(|m| m.deleted).count()
    }

    /// Compress: remove all deleted entries and repack.
    pub fn compress(&mut self) {
        self.members.retain(|_, m| !m.deleted);
    }
}

/// Sequential (flat file) unload format for PDS transport.
#[derive(Debug, Clone, Default)]
pub struct UnloadData {
    /// Records in unload format: header lines + member content.
    pub records: Vec<String>,
}

impl UnloadData {
    /// Create new empty unload data.
    pub fn new() -> Self {
        Self::default()
    }
}

// ─────────────────────── DD Allocation ───────────────────────

/// A simulated DD (Data Definition) allocation for utility processing.
///
/// Represents the connection between a DDname and its backing data,
/// whether it's a dataset, inline data, or output buffer.
#[derive(Debug, Clone)]
pub struct DdAllocation {
    /// DDname (e.g., "SYSUT1", "SYSIN", "SYSPRINT").
    pub ddname: String,
    /// Dataset name (if allocated to a dataset).
    pub dsname: Option<String>,
    /// Inline data (from `DD *` or `DD DATA`).
    pub inline_data: Option<Vec<String>>,
    /// Output buffer (for SYSPRINT and similar).
    pub output: Vec<String>,
    /// Disposition (NEW, OLD, SHR, MOD).
    pub disposition: String,
    /// PDS data (for partitioned datasets).
    pub pds_data: Option<PdsData>,
    /// Sequential unload data (for load/unload operations).
    pub unload_data: Option<UnloadData>,
    /// Dataset organization (PS=sequential, PO=partitioned).
    pub dsorg: Option<String>,
}

impl DdAllocation {
    /// Create a DD allocation backed by a dataset.
    pub fn dataset(ddname: &str, dsname: &str, disp: &str) -> Self {
        Self {
            ddname: ddname.to_string(),
            dsname: Some(dsname.to_string()),
            inline_data: None,
            output: Vec::new(),
            disposition: disp.to_string(),
            pds_data: None,
            unload_data: None,
            dsorg: None,
        }
    }

    /// Create a DD allocation with inline data (DD * or DD DATA).
    pub fn inline(ddname: &str, data: Vec<String>) -> Self {
        Self {
            ddname: ddname.to_string(),
            dsname: None,
            inline_data: Some(data),
            output: Vec::new(),
            disposition: String::new(),
            pds_data: None,
            unload_data: None,
            dsorg: None,
        }
    }

    /// Create a DD allocation for output (SYSPRINT, etc.).
    pub fn output(ddname: &str) -> Self {
        Self {
            ddname: ddname.to_string(),
            dsname: None,
            inline_data: None,
            output: Vec::new(),
            disposition: "NEW".to_string(),
            pds_data: None,
            unload_data: None,
            dsorg: None,
        }
    }

    /// Create a DUMMY DD allocation.
    pub fn dummy(ddname: &str) -> Self {
        Self {
            ddname: ddname.to_string(),
            dsname: None,
            inline_data: None,
            output: Vec::new(),
            disposition: "DUMMY".to_string(),
            pds_data: None,
            unload_data: None,
            dsorg: None,
        }
    }

    /// Create a DD allocation backed by a PDS.
    pub fn pds(ddname: &str, dsname: &str, disp: &str, pds_data: PdsData) -> Self {
        Self {
            ddname: ddname.to_string(),
            dsname: Some(dsname.to_string()),
            inline_data: None,
            output: Vec::new(),
            disposition: disp.to_string(),
            pds_data: Some(pds_data),
            unload_data: None,
            dsorg: Some("PO".to_string()),
        }
    }

    /// Create a DD allocation for sequential output (for unload).
    pub fn sequential(ddname: &str, dsname: &str, disp: &str) -> Self {
        Self {
            ddname: ddname.to_string(),
            dsname: Some(dsname.to_string()),
            inline_data: None,
            output: Vec::new(),
            disposition: disp.to_string(),
            pds_data: None,
            unload_data: None,
            dsorg: Some("PS".to_string()),
        }
    }

    /// Whether this is a DUMMY allocation.
    pub fn is_dummy(&self) -> bool {
        self.disposition == "DUMMY"
    }

    /// Whether this DD is allocated to a PDS.
    pub fn is_pds(&self) -> bool {
        self.pds_data.is_some()
    }
}

// ─────────────────────── Utility Context ───────────────────────

/// Context passed to a utility program during execution.
///
/// Provides access to DD allocations, control statements (SYSIN),
/// and output facilities (SYSPRINT).
pub struct UtilityContext {
    /// JCL step name.
    pub step_name: String,
    /// Program name being executed.
    pub program_name: String,
    /// DD allocations indexed by DDname (uppercase).
    dd_table: HashMap<String, DdAllocation>,
}

impl UtilityContext {
    /// Create a new utility context.
    pub fn new(step_name: &str, program_name: &str) -> Self {
        Self {
            step_name: step_name.to_string(),
            program_name: program_name.to_string(),
            dd_table: HashMap::new(),
        }
    }

    /// Add a DD allocation to the context.
    pub fn add_dd(&mut self, dd: DdAllocation) {
        self.dd_table.insert(dd.ddname.to_uppercase(), dd);
    }

    /// Check if a DDname is allocated.
    pub fn has_dd(&self, ddname: &str) -> bool {
        self.dd_table.contains_key(&ddname.to_uppercase())
    }

    /// Get a DD allocation by name.
    pub fn get_dd(&self, ddname: &str) -> Option<&DdAllocation> {
        self.dd_table.get(&ddname.to_uppercase())
    }

    /// Get a mutable DD allocation by name.
    pub fn get_dd_mut(&mut self, ddname: &str) -> Option<&mut DdAllocation> {
        self.dd_table.get_mut(&ddname.to_uppercase())
    }

    /// Read input from a DD allocation.
    ///
    /// Returns the inline data lines or an error if the DD is not allocated.
    pub fn open_input(&self, ddname: &str) -> Result<Vec<String>> {
        let dd = self
            .dd_table
            .get(&ddname.to_uppercase())
            .ok_or(UtilityError::DdNotFound {
                ddname: ddname.to_string(),
            })?;

        if dd.is_dummy() {
            return Ok(Vec::new());
        }

        Ok(dd.inline_data.clone().unwrap_or_default())
    }

    /// Read SYSIN control statements.
    ///
    /// Returns control statements if SYSIN DD is allocated, or an empty vec if not.
    pub fn read_sysin(&self) -> Vec<String> {
        self.dd_table
            .get("SYSIN")
            .and_then(|dd| dd.inline_data.clone())
            .unwrap_or_default()
    }

    /// Write a message line to SYSPRINT.
    ///
    /// If SYSPRINT is not allocated, the message is silently discarded.
    pub fn write_message(&mut self, msg: &str) {
        if let Some(dd) = self.dd_table.get_mut("SYSPRINT") {
            dd.output.push(msg.to_string());
        }
    }

    /// Write a utility message to SYSPRINT in IBM format.
    pub fn write_utility_message(&mut self, msg: &UtilityMessage) {
        self.write_message(&msg.to_string());
    }

    /// Get all output written to SYSPRINT.
    pub fn sysprint_output(&self) -> Vec<String> {
        self.dd_table
            .get("SYSPRINT")
            .map(|dd| dd.output.clone())
            .unwrap_or_default()
    }

    /// List all allocated DDnames.
    pub fn dd_names(&self) -> Vec<String> {
        let mut names: Vec<String> = self.dd_table.keys().cloned().collect();
        names.sort();
        names
    }
}

impl fmt::Debug for UtilityContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("UtilityContext")
            .field("step_name", &self.step_name)
            .field("program_name", &self.program_name)
            .field("dd_count", &self.dd_table.len())
            .finish()
    }
}

// ─────────────────────── UtilityProgram Trait ───────────────────────

/// Trait that all utility programs implement.
///
/// Each z/OS utility (IEBCOPY, IEBGENER, etc.) implements this trait.
/// The JCL executor dispatches to utilities via `UtilityRegistry::dispatch()`.
pub trait UtilityProgram: Send + Sync {
    /// The program name (e.g., "IEBCOPY", "IEBGENER", "IEFBR14").
    fn name(&self) -> &str;

    /// Execute the utility with the given context.
    ///
    /// The context provides access to DD allocations and control statements.
    /// Returns a `UtilityResult` with condition code and messages.
    fn execute(&self, context: &mut UtilityContext) -> UtilityResult;
}

// ─────────────────────── Utility Registry ───────────────────────

/// Registry of available utility programs.
///
/// The JCL executor uses this registry to dispatch `EXEC PGM=name` to the
/// corresponding utility implementation.
pub struct UtilityRegistry {
    programs: HashMap<String, Box<dyn UtilityProgram>>,
}

impl fmt::Debug for UtilityRegistry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let names: Vec<&str> = self.programs.keys().map(|s| s.as_str()).collect();
        f.debug_struct("UtilityRegistry")
            .field("programs", &names)
            .finish()
    }
}

impl Default for UtilityRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl UtilityRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self {
            programs: HashMap::new(),
        }
    }

    /// Create a registry pre-loaded with all built-in utility programs.
    pub fn with_builtins() -> Self {
        let mut reg = Self::new();
        reg.register(Box::new(Iefbr14));
        reg.register(Box::new(iebcopy::Iebcopy));
        reg.register(Box::new(iebgener::Iebgener));
        reg.register(Box::new(iebcompr::Iebcompr));
        reg.register(Box::new(iebupdte::Iebupdte));
        reg.register(Box::new(iebptpch::Iebptpch));
        reg.register(Box::new(iebdg::Iebdg));
        reg
    }

    /// Register a utility program.
    ///
    /// If a program with the same name is already registered, it is replaced.
    pub fn register(&mut self, program: Box<dyn UtilityProgram>) {
        let name = program.name().to_uppercase();
        info!("Registering utility program: {}", name);
        self.programs.insert(name, program);
    }

    /// Dispatch execution to a registered utility program.
    ///
    /// Returns `Err(UtilityError::ProgramNotFound)` if the program name
    /// is not registered (S806 ABEND equivalent).
    pub fn dispatch(&self, program_name: &str, context: &mut UtilityContext) -> Result<UtilityResult> {
        let name = program_name.to_uppercase();
        let program = self.programs.get(&name).ok_or(UtilityError::ProgramNotFound {
            name: name.clone(),
        })?;

        info!(
            "Dispatching utility: PGM={}, STEP={}",
            name, context.step_name
        );

        let result = program.execute(context);

        info!(
            "Utility {} completed: CC={}",
            name, result.condition_code
        );

        Ok(result)
    }

    /// Check if a program is registered.
    pub fn is_registered(&self, program_name: &str) -> bool {
        self.programs.contains_key(&program_name.to_uppercase())
    }

    /// List all registered program names.
    pub fn list_programs(&self) -> Vec<String> {
        let mut names: Vec<String> = self.programs.keys().cloned().collect();
        names.sort();
        names
    }

    /// Get the number of registered programs.
    pub fn program_count(&self) -> usize {
        self.programs.len()
    }
}

// ─────────────────────── IEFBR14 ───────────────────────

/// IEFBR14 — the "do nothing" utility program.
///
/// Always returns CC=0 with no processing. Used in JCL for
/// DD allocation/deallocation side effects.
pub struct Iefbr14;

impl UtilityProgram for Iefbr14 {
    fn name(&self) -> &str {
        "IEFBR14"
    }

    fn execute(&self, _context: &mut UtilityContext) -> UtilityResult {
        UtilityResult::success()
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ─── Test utility implementations ───

    /// Test utility that counts SYSIN statements and returns CC=0.
    struct CountingUtility;

    impl UtilityProgram for CountingUtility {
        fn name(&self) -> &str {
            "TESTCOUNT"
        }

        fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
            let stmts = context.read_sysin();
            let count = stmts.len();
            let msg = UtilityMessage::info(
                &format_message_id("TST", 1, MessageSeverity::Info),
                &format!("{count} CONTROL STATEMENTS PROCESSED"),
            );
            context.write_utility_message(&msg);
            UtilityResult::success_with(vec![msg])
        }
    }

    /// Test utility that returns CC=8 on error conditions.
    struct ErrorUtility;

    impl UtilityProgram for ErrorUtility {
        fn name(&self) -> &str {
            "TESTERROR"
        }

        fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
            if !context.has_dd("SYSUT1") {
                let msg = UtilityMessage::severe("TST0002S", "SYSUT1 DD REQUIRED");
                context.write_utility_message(&msg);
                return UtilityResult::new(12, vec![msg]);
            }
            UtilityResult::success()
        }
    }

    /// Test utility that copies data from SYSUT1 to SYSUT2.
    struct CopyUtility;

    impl UtilityProgram for CopyUtility {
        fn name(&self) -> &str {
            "TESTCOPY"
        }

        fn execute(&self, context: &mut UtilityContext) -> UtilityResult {
            let input = match context.open_input("SYSUT1") {
                Ok(data) => data,
                Err(_) => {
                    return UtilityResult::new(
                        12,
                        vec![UtilityMessage::severe("TST0010S", "SYSUT1 NOT ALLOCATED")],
                    );
                }
            };

            if let Some(out_dd) = context.get_dd_mut("SYSUT2") {
                for line in &input {
                    out_dd.output.push(line.clone());
                }
            }

            let msg = UtilityMessage::info(
                "TST0011I",
                &format!("{} RECORDS COPIED", input.len()),
            );
            context.write_utility_message(&msg);
            UtilityResult::success_with(vec![msg])
        }
    }

    // ─────── UTIL-110.1: UtilityProgram Trait and Registry ───────

    #[test]
    fn test_registry_new_is_empty() {
        let reg = UtilityRegistry::new();
        assert_eq!(reg.program_count(), 0);
    }

    #[test]
    fn test_register_and_dispatch() {
        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(Iefbr14));

        let mut ctx = UtilityContext::new("STEP01", "IEFBR14");
        let result = reg.dispatch("IEFBR14", &mut ctx).unwrap();
        assert_eq!(result.condition_code, 0);
    }

    #[test]
    fn test_dispatch_case_insensitive() {
        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(Iefbr14));

        let mut ctx = UtilityContext::new("STEP01", "IEFBR14");
        assert!(reg.dispatch("iefbr14", &mut ctx).is_ok());
        assert!(reg.dispatch("IeFbR14", &mut ctx).is_ok());
    }

    #[test]
    fn test_dispatch_unregistered_program() {
        let reg = UtilityRegistry::new();
        let mut ctx = UtilityContext::new("STEP01", "NOSUCH");
        let err = reg.dispatch("NOSUCH", &mut ctx).unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("S806"));
        assert!(msg.contains("NOSUCH"));
    }

    #[test]
    fn test_registry_is_registered() {
        let mut reg = UtilityRegistry::new();
        assert!(!reg.is_registered("IEFBR14"));
        reg.register(Box::new(Iefbr14));
        assert!(reg.is_registered("IEFBR14"));
        assert!(reg.is_registered("iefbr14"));
    }

    #[test]
    fn test_registry_list_programs() {
        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(Iefbr14));
        reg.register(Box::new(CountingUtility));

        let programs = reg.list_programs();
        assert_eq!(programs.len(), 2);
        assert!(programs.contains(&"IEFBR14".to_string()));
        assert!(programs.contains(&"TESTCOUNT".to_string()));
    }

    #[test]
    fn test_registry_with_builtins() {
        let reg = UtilityRegistry::with_builtins();
        assert!(reg.is_registered("IEFBR14"));
    }

    #[test]
    fn test_registry_replace_program() {
        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(Iefbr14));

        struct CustomIefbr14;
        impl UtilityProgram for CustomIefbr14 {
            fn name(&self) -> &str { "IEFBR14" }
            fn execute(&self, _ctx: &mut UtilityContext) -> UtilityResult {
                UtilityResult::with_cc(4)
            }
        }

        reg.register(Box::new(CustomIefbr14));
        let mut ctx = UtilityContext::new("STEP01", "IEFBR14");
        let result = reg.dispatch("IEFBR14", &mut ctx).unwrap();
        assert_eq!(result.condition_code, 4);
        assert_eq!(reg.program_count(), 1);
    }

    #[test]
    fn test_sysin_control_statements_accessible() {
        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(CountingUtility));

        let mut ctx = UtilityContext::new("STEP01", "TESTCOUNT");
        ctx.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![
                " COPY OUTDD=SYSUT2,INDD=SYSUT1".to_string(),
                " SELECT MEMBER=MOD1".to_string(),
            ],
        ));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = reg.dispatch("TESTCOUNT", &mut ctx).unwrap();
        assert_eq!(result.condition_code, 0);
        assert_eq!(result.messages.len(), 1);
        assert!(result.messages[0].text.contains("2 CONTROL STATEMENTS"));
    }

    #[test]
    fn test_condition_code_propagation() {
        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(ErrorUtility));

        // Without required DD → CC=12.
        let mut ctx = UtilityContext::new("STEP01", "TESTERROR");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        let result = reg.dispatch("TESTERROR", &mut ctx).unwrap();
        assert_eq!(result.condition_code, 12);
        assert!(result.is_error());

        // With required DD → CC=0.
        let mut ctx2 = UtilityContext::new("STEP01", "TESTERROR");
        ctx2.add_dd(DdAllocation::inline("SYSUT1", vec![]));
        let result2 = reg.dispatch("TESTERROR", &mut ctx2).unwrap();
        assert_eq!(result2.condition_code, 0);
        assert!(result2.is_success());
    }

    // ─────── UTIL-110.2: Common DD Handling and Message Formatting ───────

    #[test]
    fn test_open_input_returns_data() {
        let mut ctx = UtilityContext::new("STEP01", "TEST");
        ctx.add_dd(DdAllocation::inline(
            "SYSUT1",
            vec!["LINE1".to_string(), "LINE2".to_string()],
        ));

        let data = ctx.open_input("SYSUT1").unwrap();
        assert_eq!(data, vec!["LINE1", "LINE2"]);
    }

    #[test]
    fn test_open_input_dd_not_found() {
        let ctx = UtilityContext::new("STEP01", "TEST");
        let err = ctx.open_input("SYSUT1").unwrap_err();
        assert!(err.to_string().contains("SYSUT1"));
    }

    #[test]
    fn test_open_input_dummy_returns_empty() {
        let mut ctx = UtilityContext::new("STEP01", "TEST");
        ctx.add_dd(DdAllocation::dummy("SYSUT1"));

        let data = ctx.open_input("SYSUT1").unwrap();
        assert!(data.is_empty());
    }

    #[test]
    fn test_write_message_to_sysprint() {
        let mut ctx = UtilityContext::new("STEP01", "TEST");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        ctx.write_message("IEB1013I COPY OPERATION SUCCESSFUL");
        let output = ctx.sysprint_output();
        assert_eq!(output.len(), 1);
        assert_eq!(output[0], "IEB1013I COPY OPERATION SUCCESSFUL");
    }

    #[test]
    fn test_write_message_no_sysprint_is_silent() {
        let mut ctx = UtilityContext::new("STEP01", "TEST");
        // No SYSPRINT allocated — should not panic.
        ctx.write_message("IEB1013I COPY OPERATION SUCCESSFUL");
        assert!(ctx.sysprint_output().is_empty());
    }

    #[test]
    fn test_message_formatting() {
        let msg_id = format_message_id("IEB", 1013, MessageSeverity::Info);
        assert_eq!(msg_id, "IEB1013I");

        let msg_id = format_message_id("IEB", 2, MessageSeverity::Error);
        assert_eq!(msg_id, "IEB0002E");

        let msg_id = format_message_id("IEB", 999, MessageSeverity::Severe);
        assert_eq!(msg_id, "IEB0999S");
    }

    #[test]
    fn test_utility_message_display() {
        let msg = UtilityMessage::info("IEB1013I", "COPY OPERATION SUCCESSFUL");
        assert_eq!(format!("{msg}"), "IEB1013I COPY OPERATION SUCCESSFUL");
    }

    #[test]
    fn test_message_severity_from_code() {
        assert_eq!(MessageSeverity::from_code('I'), Some(MessageSeverity::Info));
        assert_eq!(MessageSeverity::from_code('W'), Some(MessageSeverity::Warning));
        assert_eq!(MessageSeverity::from_code('E'), Some(MessageSeverity::Error));
        assert_eq!(MessageSeverity::from_code('S'), Some(MessageSeverity::Severe));
        assert_eq!(MessageSeverity::from_code('i'), Some(MessageSeverity::Info));
        assert_eq!(MessageSeverity::from_code('X'), None);
    }

    #[test]
    fn test_dd_allocation_dataset() {
        let dd = DdAllocation::dataset("SYSUT1", "MY.DATASET", "SHR");
        assert_eq!(dd.ddname, "SYSUT1");
        assert_eq!(dd.dsname.as_deref(), Some("MY.DATASET"));
        assert_eq!(dd.disposition, "SHR");
        assert!(!dd.is_dummy());
    }

    #[test]
    fn test_dd_allocation_inline() {
        let dd = DdAllocation::inline("SYSIN", vec!["STMT1".to_string()]);
        assert!(dd.inline_data.is_some());
        assert!(dd.dsname.is_none());
    }

    #[test]
    fn test_dd_allocation_dummy() {
        let dd = DdAllocation::dummy("SYSUT1");
        assert!(dd.is_dummy());
    }

    #[test]
    fn test_context_dd_names() {
        let mut ctx = UtilityContext::new("STEP01", "TEST");
        ctx.add_dd(DdAllocation::output("SYSPRINT"));
        ctx.add_dd(DdAllocation::inline("SYSIN", vec![]));
        ctx.add_dd(DdAllocation::dummy("SYSUT1"));

        let names = ctx.dd_names();
        assert_eq!(names, vec!["SYSIN", "SYSPRINT", "SYSUT1"]);
    }

    #[test]
    fn test_context_case_insensitive_dd() {
        let mut ctx = UtilityContext::new("STEP01", "TEST");
        ctx.add_dd(DdAllocation::output("sysprint"));
        assert!(ctx.has_dd("SYSPRINT"));
        assert!(ctx.has_dd("sysprint"));
    }

    #[test]
    fn test_copy_utility_with_data_flow() {
        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(CopyUtility));

        let mut ctx = UtilityContext::new("STEP01", "TESTCOPY");
        ctx.add_dd(DdAllocation::inline(
            "SYSUT1",
            vec!["REC1".to_string(), "REC2".to_string(), "REC3".to_string()],
        ));
        ctx.add_dd(DdAllocation::output("SYSUT2"));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        let result = reg.dispatch("TESTCOPY", &mut ctx).unwrap();
        assert_eq!(result.condition_code, 0);

        let copied = ctx.get_dd("SYSUT2").unwrap();
        assert_eq!(copied.output.len(), 3);
        assert_eq!(copied.output[0], "REC1");
    }

    // ─────── UTIL-110.3: Condition Code Protocol and IEFBR14 ───────

    #[test]
    fn test_iefbr14_returns_cc0() {
        let mut ctx = UtilityContext::new("STEP01", "IEFBR14");
        let result = Iefbr14.execute(&mut ctx);
        assert_eq!(result.condition_code, 0);
        assert!(result.is_success());
        assert!(result.messages.is_empty());
    }

    #[test]
    fn test_iefbr14_via_registry() {
        let reg = UtilityRegistry::with_builtins();
        let mut ctx = UtilityContext::new("STEP01", "IEFBR14");
        let result = reg.dispatch("IEFBR14", &mut ctx).unwrap();
        assert_eq!(result.condition_code, 0);
    }

    #[test]
    fn test_condition_code_success() {
        let r = UtilityResult::success();
        assert!(r.is_success());
        assert!(!r.is_warning());
        assert!(!r.is_error());
    }

    #[test]
    fn test_condition_code_warning() {
        let r = UtilityResult::with_cc(4);
        assert!(!r.is_success());
        assert!(r.is_warning());
        assert!(!r.is_error());
    }

    #[test]
    fn test_condition_code_error() {
        let r = UtilityResult::with_cc(8);
        assert!(!r.is_success());
        assert!(!r.is_warning());
        assert!(r.is_error());
    }

    #[test]
    fn test_condition_code_severe() {
        let r = UtilityResult::with_cc(12);
        assert!(r.is_error());
    }

    #[test]
    fn test_condition_code_16() {
        let r = UtilityResult::with_cc(16);
        assert!(r.is_error());
    }

    // ─────── UTIL-110.4: Framework Integration Tests ───────

    #[test]
    fn test_multi_step_job_execution() {
        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(Iefbr14));
        reg.register(Box::new(CountingUtility));
        reg.register(Box::new(CopyUtility));

        // Step 1: IEFBR14 (allocate).
        let mut ctx1 = UtilityContext::new("ALLOC", "IEFBR14");
        let r1 = reg.dispatch("IEFBR14", &mut ctx1).unwrap();
        assert_eq!(r1.condition_code, 0);

        // Step 2: TESTCOUNT (process SYSIN).
        let mut ctx2 = UtilityContext::new("COUNT", "TESTCOUNT");
        ctx2.add_dd(DdAllocation::inline(
            "SYSIN",
            vec![
                " COPY OUTDD=SYSUT2".to_string(),
                " SELECT MEMBER=MOD1".to_string(),
                " SELECT MEMBER=MOD2".to_string(),
            ],
        ));
        ctx2.add_dd(DdAllocation::output("SYSPRINT"));
        let r2 = reg.dispatch("TESTCOUNT", &mut ctx2).unwrap();
        assert_eq!(r2.condition_code, 0);
        assert!(r2.messages[0].text.contains("3 CONTROL STATEMENTS"));

        // Step 3: TESTCOPY (copy data).
        let mut ctx3 = UtilityContext::new("COPY", "TESTCOPY");
        ctx3.add_dd(DdAllocation::inline(
            "SYSUT1",
            vec!["DATA1".to_string(), "DATA2".to_string()],
        ));
        ctx3.add_dd(DdAllocation::output("SYSUT2"));
        ctx3.add_dd(DdAllocation::output("SYSPRINT"));
        let r3 = reg.dispatch("TESTCOPY", &mut ctx3).unwrap();
        assert_eq!(r3.condition_code, 0);

        // All 3 steps completed with CC=0.
        assert!(r1.is_success() && r2.is_success() && r3.is_success());
    }

    #[test]
    fn test_unregistered_program_abend_s806() {
        let reg = UtilityRegistry::with_builtins();
        let mut ctx = UtilityContext::new("STEP01", "NOSUCHPGM");

        let err = reg.dispatch("NOSUCHPGM", &mut ctx).unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("S806"), "Expected S806 ABEND: {msg}");
        assert!(msg.contains("NOSUCHPGM"));
    }

    #[test]
    fn test_multi_step_with_error_step() {
        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(Iefbr14));
        reg.register(Box::new(ErrorUtility));

        // Step 1: CC=0.
        let mut ctx1 = UtilityContext::new("STEP01", "IEFBR14");
        let r1 = reg.dispatch("IEFBR14", &mut ctx1).unwrap();
        assert_eq!(r1.condition_code, 0);

        // Step 2: CC=12 (missing SYSUT1).
        let mut ctx2 = UtilityContext::new("STEP02", "TESTERROR");
        ctx2.add_dd(DdAllocation::output("SYSPRINT"));
        let r2 = reg.dispatch("TESTERROR", &mut ctx2).unwrap();
        assert_eq!(r2.condition_code, 12);

        // Step 3: Would normally run but check CC.
        // Simulate COND=(8,LT) — skip if any prior step CC >= 8.
        let max_cc = r1.condition_code.max(r2.condition_code);
        let skip_step3 = max_cc >= 8;
        assert!(skip_step3, "Step 3 should be skipped due to CC=12 in step 2");
    }

    #[test]
    fn test_sysprint_messages_accumulated() {
        let mut reg = UtilityRegistry::new();
        reg.register(Box::new(CountingUtility));

        let mut ctx = UtilityContext::new("STEP01", "TESTCOUNT");
        ctx.add_dd(DdAllocation::inline("SYSIN", vec!["STMT1".to_string()]));
        ctx.add_dd(DdAllocation::output("SYSPRINT"));

        reg.dispatch("TESTCOUNT", &mut ctx).unwrap();

        let output = ctx.sysprint_output();
        assert_eq!(output.len(), 1);
        assert!(output[0].contains("TST0001I"));
    }
}
