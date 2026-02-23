//! # Program Management — LOAD/LINK/XCTL/ATTACH & Search Order
//!
//! Implements the MVS program search hierarchy (STEPLIB→JOBLIB→LPA→LNKLST),
//! the LOAD/DELETE/LINK/XCTL/ATTACH macro equivalents, and APF authorization.

use std::collections::HashMap;

use crate::binder::LoadModule;

// ─────────────────────── Addressing Mode ───────────────────────

/// AMODE — Addressing mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Amode {
    /// 24-bit addressing.
    Amode24,
    /// 31-bit addressing.
    Amode31,
    /// 64-bit addressing.
    Amode64,
    /// Any mode (callee adapts).
    Any,
}

/// RMODE — Residency mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Rmode {
    /// Must reside below 16 MB.
    Rmode24,
    /// Can reside anywhere below 2 GB.
    RmodeAny,
}

// ─────────────────────── Loaded Program ───────────────────────

/// A program loaded into virtual storage.
#[derive(Debug, Clone)]
pub struct LoadedProgram {
    /// Program name.
    pub name: String,
    /// Entry point offset.
    pub entry_point: u32,
    /// AMODE.
    pub amode: Amode,
    /// RMODE.
    pub rmode: Rmode,
    /// Whether loaded from an APF-authorized library.
    pub apf_authorized: bool,
    /// Use count (number of active LOAD references).
    pub use_count: u32,
    /// Program text.
    pub text: Vec<u8>,
    /// Aliases.
    pub aliases: Vec<String>,
}

// ─────────────────────── Search Path ───────────────────────

/// Library search path type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SearchPathType {
    /// STEPLIB DD.
    Steplib,
    /// JOBLIB DD.
    Joblib,
    /// Link Pack Area.
    Lpa,
    /// Linklist.
    Lnklst,
}

/// A library in the search path.
#[derive(Debug, Clone)]
pub struct ProgramLibrary {
    /// Library name.
    pub name: String,
    /// Path type.
    pub path_type: SearchPathType,
    /// APF authorized.
    pub apf_authorized: bool,
    /// Programs in this library (name → LoadModule).
    programs: HashMap<String, LoadModule>,
}

impl ProgramLibrary {
    /// Create a new library.
    pub fn new(name: &str, path_type: SearchPathType, apf_authorized: bool) -> Self {
        Self {
            name: name.into(),
            path_type,
            apf_authorized,
            programs: HashMap::new(),
        }
    }

    /// Add a program to the library.
    pub fn add_program(&mut self, module: LoadModule) {
        // Also register aliases.
        let aliases = module.aliases.clone();
        let name = module.name.clone();
        self.programs.insert(name, module);
        for alias in aliases {
            if let Some(original) = self.programs.values().last().cloned() {
                let mut aliased = original;
                aliased.name = alias.clone();
                self.programs.insert(alias, aliased);
            }
        }
    }

    /// Find a program.
    pub fn find(&self, name: &str) -> Option<&LoadModule> {
        self.programs.get(name)
    }
}

// ─────────────────────── Program Manager Error ───────────────────────

/// Program management error.
#[derive(Debug, Clone, thiserror::Error)]
pub enum ProgramError {
    /// S806 — Program not found.
    #[error("S806 ABEND — PROGRAM {name} NOT FOUND")]
    ProgramNotFound { name: String },
    /// S047 — Unauthorized program.
    #[error("S047 ABEND — PROGRAM {name} NOT APF AUTHORIZED")]
    NotAuthorized { name: String },
    /// Program already loaded (for duplicate LOAD).
    #[error("PROGRAM {name} ALREADY LOADED — USE COUNT INCREMENTED")]
    AlreadyLoaded { name: String },
    /// Program not loaded (for DELETE).
    #[error("PROGRAM {name} NOT LOADED")]
    NotLoaded { name: String },
}

// ─────────────────────── Execution Result ───────────────────────

/// Return code from program execution.
#[derive(Debug, Clone)]
pub struct ExecutionResult {
    /// Program name.
    pub program: String,
    /// Return code (R15).
    pub return_code: u32,
}

/// TCB (Task Control Block) — represents a subtask.
#[derive(Debug, Clone)]
pub struct Tcb {
    /// Task ID.
    pub task_id: u32,
    /// Program name.
    pub program: String,
    /// Completion code.
    pub completion_code: Option<u32>,
    /// ECB (Event Control Block) address — posted on completion.
    pub ecb_posted: bool,
}

// ─────────────────────── APF Authorization ───────────────────────

/// APF authorized library list.
#[derive(Debug, Clone, Default)]
pub struct ApfList {
    /// Authorized library names.
    libraries: Vec<String>,
}

impl ApfList {
    /// Create a new APF list.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a library.
    pub fn add(&mut self, library: &str) {
        self.libraries.push(library.to_uppercase());
    }

    /// Check if a library is authorized.
    pub fn is_authorized(&self, library: &str) -> bool {
        self.libraries.contains(&library.to_uppercase())
    }

    /// List authorized libraries.
    pub fn list(&self) -> &[String] {
        &self.libraries
    }
}

// ─────────────────────── Program Manager ───────────────────────

/// The Program Manager — handles LOAD/DELETE/LINK/XCTL/ATTACH.
#[derive(Debug)]
pub struct ProgramManager {
    /// Search path (in priority order).
    search_path: Vec<ProgramLibrary>,
    /// Currently loaded programs.
    loaded: HashMap<String, LoadedProgram>,
    /// APF list.
    apf_list: ApfList,
    /// Next task ID for ATTACH.
    next_task_id: u32,
    /// Active subtasks.
    subtasks: Vec<Tcb>,
    /// Execution chain (for XCTL tracking).
    execution_stack: Vec<String>,
}

impl ProgramManager {
    /// Create a new program manager.
    pub fn new() -> Self {
        Self {
            search_path: Vec::new(),
            loaded: HashMap::new(),
            apf_list: ApfList::new(),
            next_task_id: 1,
            subtasks: Vec::new(),
            execution_stack: Vec::new(),
        }
    }

    /// Add a library to the search path.
    pub fn add_library(&mut self, library: ProgramLibrary) {
        self.search_path.push(library);
    }

    /// Get the APF list.
    pub fn apf_list_mut(&mut self) -> &mut ApfList {
        &mut self.apf_list
    }

    /// Search for a program using the STEPLIB→JOBLIB→LPA→LNKLST hierarchy.
    fn search_program(&self, name: &str) -> Option<(&LoadModule, bool)> {
        let order = [
            SearchPathType::Steplib,
            SearchPathType::Joblib,
            SearchPathType::Lpa,
            SearchPathType::Lnklst,
        ];

        for path_type in &order {
            for lib in &self.search_path {
                if lib.path_type == *path_type {
                    if let Some(module) = lib.find(name) {
                        let apf = lib.apf_authorized
                            || self.apf_list.is_authorized(&lib.name);
                        return Some((module, apf));
                    }
                }
            }
        }

        None
    }

    /// LOAD macro (SVC 8) — bring a module into virtual storage.
    ///
    /// Returns entry point address. Increments use count if already loaded.
    pub fn load(&mut self, name: &str) -> Result<u32, ProgramError> {
        // Check if already loaded.
        if let Some(prog) = self.loaded.get_mut(name) {
            prog.use_count += 1;
            return Ok(prog.entry_point);
        }

        // Search for the program.
        let (module, apf) = self
            .search_program(name)
            .ok_or_else(|| ProgramError::ProgramNotFound {
                name: name.to_string(),
            })?;

        let loaded = LoadedProgram {
            name: name.to_string(),
            entry_point: module.entry_point,
            amode: Amode::Amode31,
            rmode: Rmode::RmodeAny,
            apf_authorized: apf,
            use_count: 1,
            text: module.text.clone(),
            aliases: module.aliases.clone(),
        };

        let ep = loaded.entry_point;
        self.loaded.insert(name.to_string(), loaded);
        Ok(ep)
    }

    /// DELETE macro (SVC 9) — release a loaded module.
    ///
    /// Decrements use count. Module removed when count reaches 0.
    pub fn delete(&mut self, name: &str) -> Result<(), ProgramError> {
        let prog = self
            .loaded
            .get_mut(name)
            .ok_or_else(|| ProgramError::NotLoaded {
                name: name.to_string(),
            })?;

        prog.use_count -= 1;
        if prog.use_count == 0 {
            self.loaded.remove(name);
        }
        Ok(())
    }

    /// LINK macro (SVC 6) — load, branch to entry, return.
    ///
    /// Simulates execution and returns R15 (return code).
    pub fn link(
        &mut self,
        name: &str,
        _parm: Option<&str>,
        require_apf: bool,
    ) -> Result<ExecutionResult, ProgramError> {
        // Load the program.
        let ep = self.load(name)?;

        // Check APF if required.
        if require_apf {
            if let Some(prog) = self.loaded.get(name) {
                if !prog.apf_authorized {
                    self.delete(name).ok();
                    return Err(ProgramError::NotAuthorized {
                        name: name.to_string(),
                    });
                }
            }
        }

        self.execution_stack.push(name.to_string());

        // Simulate execution (return code based on entry point for testing).
        let rc = if ep == 0 { 0 } else { ep };

        // Unload after return.
        self.delete(name).ok();
        self.execution_stack.pop();

        Ok(ExecutionResult {
            program: name.to_string(),
            return_code: rc,
        })
    }

    /// XCTL macro (SVC 7) — transfer control without return.
    ///
    /// Caller is removed from execution stack.
    pub fn xctl(&mut self, name: &str) -> Result<ExecutionResult, ProgramError> {
        // Remove current program from stack if any.
        if let Some(current) = self.execution_stack.last().cloned() {
            self.delete(&current).ok();
            self.execution_stack.pop();
        }

        // Load and execute the new program.
        let ep = self.load(name)?;
        self.execution_stack.push(name.to_string());

        let rc = if ep == 0 { 0 } else { ep };
        self.delete(name).ok();
        self.execution_stack.pop();

        Ok(ExecutionResult {
            program: name.to_string(),
            return_code: rc,
        })
    }

    /// ATTACH macro (SVC 42) — create a subtask.
    ///
    /// Returns the TCB for synchronization.
    pub fn attach(&mut self, name: &str) -> Result<Tcb, ProgramError> {
        // Search for the program.
        if self.search_program(name).is_none() {
            return Err(ProgramError::ProgramNotFound {
                name: name.to_string(),
            });
        }

        let task_id = self.next_task_id;
        self.next_task_id += 1;

        let tcb = Tcb {
            task_id,
            program: name.to_string(),
            completion_code: Some(0), // Simulate immediate completion.
            ecb_posted: true,
        };

        self.subtasks.push(tcb.clone());
        Ok(tcb)
    }

    /// Get a loaded program.
    pub fn get_loaded(&self, name: &str) -> Option<&LoadedProgram> {
        self.loaded.get(name)
    }

    /// Get number of loaded programs.
    pub fn loaded_count(&self) -> usize {
        self.loaded.len()
    }

    /// Get current execution stack.
    pub fn execution_stack(&self) -> &[String] {
        &self.execution_stack
    }

    /// Get subtasks.
    pub fn subtasks(&self) -> &[Tcb] {
        &self.subtasks
    }
}

impl Default for ProgramManager {
    fn default() -> Self {
        Self::new()
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binder::{Binder, EsdEntry, EsdType, ObjectModule, TextRecord};

    fn make_load_module(name: &str, text: &[u8], entry: u32) -> LoadModule {
        LoadModule {
            name: name.into(),
            entry_point: entry,
            text: text.to_vec(),
            symbols: HashMap::new(),
            aliases: Vec::new(),
        }
    }

    fn setup_manager() -> ProgramManager {
        let mut mgr = ProgramManager::new();

        // STEPLIB with MYPROG
        let mut steplib = ProgramLibrary::new("MY.STEPLIB", SearchPathType::Steplib, false);
        steplib.add_program(make_load_module("MYPROG", &[0x07, 0xFE, 0x00, 0x00], 0));
        mgr.add_library(steplib);

        // JOBLIB with JOBPROG
        let mut joblib = ProgramLibrary::new("MY.JOBLIB", SearchPathType::Joblib, false);
        joblib.add_program(make_load_module("JOBPROG", &[0x47, 0xF0, 0x00, 0x08], 0));
        mgr.add_library(joblib);

        // LPA with IEFBR14
        let mut lpa = ProgramLibrary::new("SYS1.LPALIB", SearchPathType::Lpa, true);
        lpa.add_program(make_load_module("IEFBR14", &[0x07, 0xFE], 0));
        mgr.add_library(lpa);

        // LNKLST with IKJEFT01
        let mut lnklst = ProgramLibrary::new("SYS1.LINKLIB", SearchPathType::Lnklst, true);
        lnklst.add_program(make_load_module("IKJEFT01", &[0x00; 32], 0));
        mgr.add_library(lnklst);

        mgr
    }

    // ─── SYS-104.1: Program Search Order ───

    #[test]
    fn test_search_steplib_first() {
        let mgr = setup_manager();
        let (module, _) = mgr.search_program("MYPROG").unwrap();
        assert_eq!(module.name, "MYPROG");
    }

    #[test]
    fn test_search_joblib_second() {
        let mgr = setup_manager();
        let (module, _) = mgr.search_program("JOBPROG").unwrap();
        assert_eq!(module.name, "JOBPROG");
    }

    #[test]
    fn test_search_lpa() {
        let mgr = setup_manager();
        let (module, apf) = mgr.search_program("IEFBR14").unwrap();
        assert_eq!(module.name, "IEFBR14");
        assert!(apf);
    }

    #[test]
    fn test_search_lnklst() {
        let mgr = setup_manager();
        let (module, _) = mgr.search_program("IKJEFT01").unwrap();
        assert_eq!(module.name, "IKJEFT01");
    }

    #[test]
    fn test_search_not_found_s806() {
        let mut mgr = setup_manager();
        let result = mgr.load("NOTEXIST");
        assert!(matches!(result, Err(ProgramError::ProgramNotFound { .. })));
    }

    // ─── SYS-104.2: LOAD Macro ───

    #[test]
    fn test_load_returns_entry_point() {
        let mut mgr = setup_manager();
        let ep = mgr.load("MYPROG").unwrap();
        assert_eq!(ep, 0);
        assert_eq!(mgr.loaded_count(), 1);
    }

    #[test]
    fn test_load_increments_use_count() {
        let mut mgr = setup_manager();
        mgr.load("MYPROG").unwrap();
        mgr.load("MYPROG").unwrap();
        assert_eq!(mgr.get_loaded("MYPROG").unwrap().use_count, 2);
    }

    // ─── SYS-104.3: DELETE Macro ───

    #[test]
    fn test_delete_decrements_use_count() {
        let mut mgr = setup_manager();
        mgr.load("MYPROG").unwrap();
        mgr.load("MYPROG").unwrap(); // use_count = 2
        mgr.delete("MYPROG").unwrap();
        assert_eq!(mgr.get_loaded("MYPROG").unwrap().use_count, 1);
    }

    #[test]
    fn test_delete_removes_at_zero() {
        let mut mgr = setup_manager();
        mgr.load("MYPROG").unwrap();
        mgr.delete("MYPROG").unwrap();
        assert!(mgr.get_loaded("MYPROG").is_none());
        assert_eq!(mgr.loaded_count(), 0);
    }

    #[test]
    fn test_delete_not_loaded() {
        let mut mgr = setup_manager();
        let result = mgr.delete("NOTLOADED");
        assert!(matches!(result, Err(ProgramError::NotLoaded { .. })));
    }

    // ─── SYS-104.4: LINK Macro ───

    #[test]
    fn test_link_executes_and_returns() {
        let mut mgr = setup_manager();
        let result = mgr.link("MYPROG", None, false).unwrap();
        assert_eq!(result.program, "MYPROG");
        assert_eq!(result.return_code, 0);
        // Module should be unloaded after LINK returns.
        assert_eq!(mgr.loaded_count(), 0);
    }

    #[test]
    fn test_link_with_parm() {
        let mut mgr = setup_manager();
        let result = mgr.link("MYPROG", Some("PARM1"), false).unwrap();
        assert_eq!(result.return_code, 0);
    }

    // ─── SYS-104.5: XCTL Macro ───

    #[test]
    fn test_xctl_transfers_control() {
        let mut mgr = setup_manager();
        // First LINK to establish current
        mgr.load("MYPROG").unwrap();
        mgr.execution_stack.push("MYPROG".to_string());

        let result = mgr.xctl("JOBPROG").unwrap();
        assert_eq!(result.program, "JOBPROG");
        // MYPROG should be freed
        assert!(mgr.execution_stack.is_empty());
    }

    // ─── SYS-104.6: ATTACH Macro ───

    #[test]
    fn test_attach_creates_subtask() {
        let mut mgr = setup_manager();
        let tcb = mgr.attach("MYPROG").unwrap();
        assert_eq!(tcb.task_id, 1);
        assert_eq!(tcb.program, "MYPROG");
        assert!(tcb.ecb_posted);
        assert_eq!(mgr.subtasks().len(), 1);
    }

    #[test]
    fn test_attach_not_found() {
        let mut mgr = setup_manager();
        let result = mgr.attach("MISSING");
        assert!(matches!(result, Err(ProgramError::ProgramNotFound { .. })));
    }

    #[test]
    fn test_attach_multiple_subtasks() {
        let mut mgr = setup_manager();
        let tcb1 = mgr.attach("MYPROG").unwrap();
        let tcb2 = mgr.attach("JOBPROG").unwrap();
        assert_eq!(tcb1.task_id, 1);
        assert_eq!(tcb2.task_id, 2);
        assert_eq!(mgr.subtasks().len(), 2);
    }

    // ─── SYS-104.9: APF Authorization ───

    #[test]
    fn test_apf_list() {
        let mut apf = ApfList::new();
        apf.add("SYS1.LINKLIB");
        assert!(apf.is_authorized("SYS1.LINKLIB"));
        assert!(!apf.is_authorized("USER.LOAD"));
    }

    #[test]
    fn test_link_requires_apf() {
        let mut mgr = setup_manager();
        // MYPROG is in non-APF STEPLIB.
        let result = mgr.link("MYPROG", None, true);
        assert!(matches!(result, Err(ProgramError::NotAuthorized { .. })));
    }

    #[test]
    fn test_link_apf_authorized_succeeds() {
        let mut mgr = setup_manager();
        // IEFBR14 is in APF-authorized LPA.
        let result = mgr.link("IEFBR14", None, true).unwrap();
        assert_eq!(result.return_code, 0);
    }

    // ─── SYS-104.10: Full Lifecycle ───

    #[test]
    fn test_full_lifecycle_compile_bind_load_link() {
        let mut mgr = setup_manager();

        // Create an object module.
        let module = ObjectModule {
            name: "HELLO".into(),
            esd_entries: vec![EsdEntry {
                name: "HELLO".into(),
                esd_type: EsdType::SectionDef,
                esdid: 1,
                offset: 0,
                length: 4,
            }],
            text_records: vec![TextRecord {
                esdid: 1,
                offset: 0,
                data: vec![0x07, 0xFE, 0x00, 0x00],
            }],
            rld_entries: Vec::new(),
        };

        // Bind it.
        let mut binder = Binder::new("HELLO");
        binder.add_module(module);
        binder.set_entry("HELLO");
        let load_mod = binder.bind().unwrap();

        // Add to a library.
        let mut lib = ProgramLibrary::new("USER.LOAD", SearchPathType::Steplib, false);
        lib.add_program(load_mod);
        mgr.add_library(lib);

        // LOAD.
        let ep = mgr.load("HELLO").unwrap();
        assert_eq!(ep, 0);

        // LINK.
        mgr.delete("HELLO").unwrap();
        let result = mgr.link("HELLO", None, false).unwrap();
        assert_eq!(result.return_code, 0);
    }
}
