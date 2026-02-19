//! ISPF dialog services — DISPLAY, SELECT, TBDISPL, SETMSG, CONTROL, ADDPOP/REMPOP.
//!
//! Provides the ISPF dialog manager that processes ISPEXEC commands:
//! - DISPLAY PANEL(name) — display a panel and collect input
//! - SELECT PGM(name)/PANEL(name)/CMD(cmd) — invoke a dialog function
//! - TBDISPL table PANEL(name) — display table data with scrolling
//! - SETMSG MSG(id) — set a message for the next panel display
//! - GETMSG MSG(id) — retrieve a message definition
//! - CONTROL ERRORS RETURN — control error handling mode
//! - ADDPOP/REMPOP — manage pop-up windows

use std::collections::HashMap;

use crate::panel::{Panel, PanelStmt, PanelExpr, PanelCond, CmpOp, VerCheck, VarPool as PanelVarPool};
use crate::skeleton::FileTailor;
use crate::table::TableManager;

// ---------------------------------------------------------------------------
//  Dialog manager
// ---------------------------------------------------------------------------

/// The ISPF dialog manager — manages panels, variables, messages, and services.
#[derive(Debug)]
pub struct DialogManager {
    /// Panel library: name → parsed panel.
    panels: HashMap<String, Panel>,
    /// Variable pools.
    pub vars: IspfVarPools,
    /// Table manager.
    pub tables: TableManager,
    /// File tailoring engine.
    pub file_tailor: FileTailor,
    /// Message library: id → message definition.
    messages: HashMap<String, MessageDef>,
    /// Pending message (set by SETMSG, displayed on next DISPLAY).
    pending_msg: Option<String>,
    /// Control mode: whether errors return to dialog (vs terminate).
    errors_return: bool,
    /// Pop-up stack: (row, col) positions.
    popup_stack: Vec<(u16, u16)>,
    /// Captured output for testing (simulates screen display).
    pub display_log: Vec<DisplayEvent>,
    /// Last return code from a service call.
    pub last_rc: i32,
}

/// A display event captured for testing/logging.
#[derive(Debug, Clone)]
pub enum DisplayEvent {
    /// Panel was displayed.
    PanelDisplay { name: String, fields: HashMap<String, String> },
    /// Message was shown.
    Message { id: String, short: String },
    /// SELECT was invoked.
    Select { target: String },
    /// Pop-up was added.
    PopupAdd { row: u16, col: u16 },
    /// Pop-up was removed.
    PopupRemove,
}

/// ISPF message definition.
#[derive(Debug, Clone)]
pub struct MessageDef {
    /// Message ID (e.g., "ISRZ001").
    pub id: String,
    /// Short message text (shown in message area).
    pub short_msg: String,
    /// Long message text (shown on Help).
    pub long_msg: String,
    /// Alarm: whether to sound the terminal bell.
    pub alarm: bool,
}

// ---------------------------------------------------------------------------
//  ISPF variable pools
// ---------------------------------------------------------------------------

/// Variable type hint for VDEFINE.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarType {
    Char,
    Fixed,
    Bit,
    Hex,
}

impl VarType {
    fn as_str(self) -> &'static str {
        match self {
            VarType::Char => "CHAR",
            VarType::Fixed => "FIXED",
            VarType::Bit => "BIT",
            VarType::Hex => "HEX",
        }
    }

    fn from_str(s: &str) -> Self {
        match s.to_uppercase().as_str() {
            "FIXED" => VarType::Fixed,
            "BIT" => VarType::Bit,
            "HEX" => VarType::Hex,
            _ => VarType::Char,
        }
    }
}

/// The four-pool ISPF variable model.
#[derive(Debug)]
pub struct IspfVarPools {
    /// Function pool stack (one per SELECT level).
    function_stack: Vec<HashMap<String, String>>,
    /// Shared pool (visible across split screens within a session).
    shared: HashMap<String, String>,
    /// Profile pool (persisted across sessions).
    profile: HashMap<String, String>,
    /// System variables (read-only).
    system: HashMap<String, String>,
}

impl IspfVarPools {
    fn new() -> Self {
        let mut sys = HashMap::new();
        // User / session variables.
        sys.insert("ZUSER".to_string(), "USER01".to_string());
        sys.insert("ZPREFIX".to_string(), "USER01".to_string());
        sys.insert("ZLOGON".to_string(), "TSO".to_string());
        sys.insert("ZAPPLID".to_string(), "ISR".to_string());
        // Date / time variables.
        sys.insert("ZDATE".to_string(), "2025/01/01".to_string());
        sys.insert("ZJDATE".to_string(), "25.001".to_string());
        sys.insert("ZTIME".to_string(), "12:00".to_string());
        sys.insert("ZDAY".to_string(), "01".to_string());
        sys.insert("ZMONTH".to_string(), "01".to_string());
        sys.insert("ZYEAR".to_string(), "2025".to_string());
        sys.insert("ZJ4DATE".to_string(), "2025.001".to_string());
        sys.insert("ZDAYOFWK".to_string(), "Wednesday".to_string());
        // Screen variables.
        sys.insert("ZSCREEN".to_string(), "1".to_string());
        sys.insert("ZSCREENI".to_string(), "1".to_string());
        sys.insert("ZSCRMAXD".to_string(), "24".to_string());
        sys.insert("ZSCRMAXW".to_string(), "80".to_string());
        sys.insert("ZSCRDEPTH".to_string(), "24".to_string());
        sys.insert("ZSCRWIDTH".to_string(), "80".to_string());
        // Environment variables.
        sys.insert("ZENVIR".to_string(), "ISPF 7.6 OpenMainframe".to_string());
        sys.insert("ZSYSID".to_string(), "SYS1".to_string());
        sys.insert("ZNODE".to_string(), "NODE1".to_string());
        sys.insert("ZTEMPF".to_string(), "/tmp/ispf".to_string());
        sys.insert("ZTERM".to_string(), "3278-2".to_string());
        sys.insert("ZKEYS".to_string(), "24".to_string());
        // PF key defaults.
        for i in 1..=24 {
            sys.insert(format!("ZPF{i:02}"), String::new());
        }

        Self {
            function_stack: vec![HashMap::new()],
            shared: HashMap::new(),
            profile: HashMap::new(),
            system: sys,
        }
    }

    /// Get a variable value, searching pools in ISPF order:
    /// function → shared → profile → system.
    pub fn get(&self, name: &str) -> Option<String> {
        let upper = name.to_uppercase();
        // Function pool (current level).
        if let Some(func) = self.function_stack.last() {
            if let Some(v) = func.get(&upper) {
                return Some(v.clone());
            }
        }
        // Shared pool.
        if let Some(v) = self.shared.get(&upper) {
            return Some(v.clone());
        }
        // Profile pool.
        if let Some(v) = self.profile.get(&upper) {
            return Some(v.clone());
        }
        // System pool.
        if let Some(v) = self.system.get(&upper) {
            return Some(v.clone());
        }
        None
    }

    /// Set a variable in the function pool.
    pub fn set(&mut self, name: &str, value: String) {
        let upper = name.to_uppercase();
        if let Some(func) = self.function_stack.last_mut() {
            func.insert(upper, value);
        }
    }

    /// VGET — copy variables from the specified pool to the function pool.
    pub fn vget(&mut self, vars: &[String], pool: PanelVarPool) {
        for var in vars {
            let upper = var.to_uppercase();
            let value = match pool {
                PanelVarPool::Shared => self.shared.get(&upper).cloned(),
                PanelVarPool::Profile => self.profile.get(&upper).cloned(),
                PanelVarPool::Asis => self.shared.get(&upper).cloned()
                    .or_else(|| self.profile.get(&upper).cloned()),
            };
            // Also check system variables.
            let value = value.or_else(|| self.system.get(&upper).cloned());
            if let Some(v) = value {
                self.set(&upper, v);
            }
        }
    }

    /// VPUT — copy variables from the function pool to the specified pool.
    pub fn vput(&mut self, vars: &[String], pool: PanelVarPool) {
        for var in vars {
            let upper = var.to_uppercase();
            let value = self.function_stack.last()
                .and_then(|f| f.get(&upper).cloned());
            if let Some(v) = value {
                match pool {
                    PanelVarPool::Shared => { self.shared.insert(upper, v); }
                    PanelVarPool::Profile => { self.profile.insert(upper, v); }
                    PanelVarPool::Asis => { self.shared.insert(upper, v); }
                }
            }
        }
    }

    /// VERASE — remove a variable from the specified pool.
    pub fn verase(&mut self, vars: &[String], pool: PanelVarPool) {
        for var in vars {
            let upper = var.to_uppercase();
            match pool {
                PanelVarPool::Shared => { self.shared.remove(&upper); }
                PanelVarPool::Profile => { self.profile.remove(&upper); }
                PanelVarPool::Asis => {
                    self.shared.remove(&upper);
                    self.profile.remove(&upper);
                }
            }
        }
    }

    /// VCOPY — copy a variable's value and length into the function pool.
    /// Sets `<name>_VALUE` and `<name>_LENGTH` in the function pool.
    /// Returns 0 on success, 8 if variable not found.
    pub fn vcopy(&mut self, name: &str) -> i32 {
        let upper = name.to_uppercase();
        if let Some(val) = self.get(&upper) {
            let len = val.len();
            self.set(&upper, val.clone());
            self.set(&format!("{upper}_LENGTH"), len.to_string());
            0
        } else {
            8
        }
    }

    /// VDEFINE — declare a variable with a type hint and optional initial value.
    /// In real ISPF this creates an application-level binding; here we store
    /// a type marker and optionally initialise the value.
    pub fn vdefine(&mut self, name: &str, var_type: VarType, initial: Option<String>) {
        let upper = name.to_uppercase();
        // Store the type marker as a system-level metadata variable.
        self.set(&format!("{upper}_TYPE"), var_type.as_str().to_string());
        if let Some(val) = initial {
            self.set(&upper, val);
        }
    }

    /// VRESET — remove all VDEFINE bindings for the current function.
    pub fn vreset(&mut self) {
        if let Some(func) = self.function_stack.last_mut() {
            let type_keys: Vec<String> = func.keys()
                .filter(|k| k.ends_with("_TYPE"))
                .cloned()
                .collect();
            for k in type_keys {
                func.remove(&k);
            }
        }
    }

    /// Push a new function pool (for SELECT).
    pub fn push_function(&mut self) {
        self.function_stack.push(HashMap::new());
    }

    /// Pop the current function pool (returning from SELECT).
    pub fn pop_function(&mut self) {
        if self.function_stack.len() > 1 {
            self.function_stack.pop();
        }
    }

    /// Save profile pool to a map (simulates writing ISPPROF dataset).
    pub fn save_profile(&self) -> HashMap<String, String> {
        self.profile.clone()
    }

    /// Load profile pool from a map (simulates reading ISPPROF dataset).
    pub fn load_profile(&mut self, data: HashMap<String, String>) {
        self.profile = data;
    }

    /// Number of function pool levels (SELECT depth).
    pub fn function_depth(&self) -> usize {
        self.function_stack.len()
    }

    /// Get all variables from the current function pool.
    pub fn current_function_vars(&self) -> HashMap<String, String> {
        self.function_stack.last().cloned().unwrap_or_default()
    }
}

// ---------------------------------------------------------------------------
//  Dialog manager implementation
// ---------------------------------------------------------------------------

impl DialogManager {
    /// Create a new dialog manager.
    pub fn new() -> Self {
        Self {
            panels: HashMap::new(),
            vars: IspfVarPools::new(),
            tables: TableManager::new(),
            file_tailor: FileTailor::new(),
            messages: HashMap::new(),
            pending_msg: None,
            errors_return: false,
            popup_stack: Vec::new(),
            display_log: Vec::new(),
            last_rc: 0,
        }
    }

    /// Load a panel into the panel library.
    pub fn load_panel(&mut self, panel: Panel) {
        self.panels.insert(panel.name.to_uppercase(), panel);
    }

    /// Register a message definition.
    pub fn register_message(&mut self, msg: MessageDef) {
        self.messages.insert(msg.id.to_uppercase(), msg);
    }

    /// Execute an ISPEXEC command string.
    pub fn exec(&mut self, cmd: &str) -> i32 {
        let trimmed = cmd.trim();
        let upper = trimmed.to_uppercase();
        let words: Vec<&str> = upper.split_whitespace().collect();

        match words.first().copied() {
            Some("DISPLAY") => self.exec_display(&upper),
            Some("SELECT") => self.exec_select(&upper),
            Some("TBDISPL") => self.exec_tbdispl(&upper),
            Some("SETMSG") => self.exec_setmsg(&upper),
            Some("GETMSG") => self.exec_getmsg(&upper),
            Some("CONTROL") => self.exec_control(&upper),
            Some("ADDPOP") => self.exec_addpop(&upper),
            Some("REMPOP") => self.exec_rempop(),
            Some("VGET") => self.exec_vget(&upper),
            Some("VPUT") => self.exec_vput(&upper),
            Some("VERASE") => self.exec_verase(&upper),
            Some("VCOPY") => self.exec_vcopy(&upper),
            Some("VDEFINE") => self.exec_vdefine(&upper),
            Some("VRESET") => self.exec_vreset(),
            Some("TBCREATE") => self.exec_tbcreate(&upper),
            Some("TBOPEN") => self.exec_tbopen(&upper),
            Some("TBCLOSE") => self.exec_tbclose(&upper),
            Some("TBEND") => self.exec_tbend(&upper),
            Some("TBSAVE") => self.exec_tbsave(&upper),
            Some("TBADD") => self.exec_tbadd(&upper),
            Some("TBPUT") => self.exec_tbput(&upper),
            Some("TBMOD") => self.exec_tbmod(&upper),
            Some("TBDELETE") => self.exec_tbdelete(&upper),
            Some("TBTOP") => self.exec_tbtop(&upper),
            Some("TBBOT") => self.exec_tbbot(&upper),
            Some("TBSKIP") => self.exec_tbskip(&upper),
            Some("TBSORT") => self.exec_tbsort(&upper),
            Some("TBSCAN") => self.exec_tbscan(&upper),
            Some("TBSARG") => self.exec_tbsarg(&upper),
            Some("FTOPEN") => self.exec_ftopen(),
            Some("FTINCL") => self.exec_ftincl(&upper),
            Some("FTCLOSE") => self.exec_ftclose(&upper),
            _ => {
                self.last_rc = 12;
                12
            }
        }
    }

    // -----------------------------------------------------------------------
    //  DISPLAY
    // -----------------------------------------------------------------------

    fn exec_display(&mut self, cmd: &str) -> i32 {
        let panel_name = extract_paren(cmd, "PANEL").unwrap_or_default();

        if let Some(panel) = self.panels.get(&panel_name).cloned() {
            // Execute )INIT section.
            self.exec_panel_stmts(&panel.init);

            // Show pending message if any.
            if let Some(msg_id) = self.pending_msg.take() {
                if let Some(msg) = self.messages.get(&msg_id) {
                    self.display_log.push(DisplayEvent::Message {
                        id: msg_id,
                        short: msg.short_msg.clone(),
                    });
                }
            }

            // Collect field values from variables.
            let fields: HashMap<String, String> = crate::extract_fields(&panel)
                .iter()
                .filter(|f| !f.name.is_empty())
                .map(|f| {
                    let val = self.vars.get(&f.name).unwrap_or_default();
                    (f.name.clone(), val)
                })
                .collect();

            self.display_log.push(DisplayEvent::PanelDisplay {
                name: panel_name,
                fields,
            });

            // Execute )PROC section.
            let proc_rc = self.exec_panel_stmts(&panel.proc_section);
            self.last_rc = proc_rc;
            proc_rc
        } else {
            self.last_rc = 12;
            12
        }
    }

    // -----------------------------------------------------------------------
    //  SELECT
    // -----------------------------------------------------------------------

    fn exec_select(&mut self, cmd: &str) -> i32 {
        let target = if let Some(pgm) = extract_paren(cmd, "PGM") {
            format!("PGM({pgm})")
        } else if let Some(panel) = extract_paren(cmd, "PANEL") {
            format!("PANEL({panel})")
        } else if let Some(c) = extract_paren(cmd, "CMD") {
            format!("CMD({c})")
        } else {
            "UNKNOWN".to_string()
        };

        self.vars.push_function();
        self.display_log.push(DisplayEvent::Select { target: target.clone() });

        // If it's a PANEL select, display the panel.
        if let Some(panel_name) = extract_paren(cmd, "PANEL") {
            self.exec_display(&format!("DISPLAY PANEL({panel_name})"));
        }

        self.vars.pop_function();
        self.last_rc = 0;
        0
    }

    // -----------------------------------------------------------------------
    //  TBDISPL
    // -----------------------------------------------------------------------

    fn exec_tbdispl(&mut self, cmd: &str) -> i32 {
        let _table_name = cmd.split_whitespace().nth(1).unwrap_or("");
        let panel_name = extract_paren(cmd, "PANEL").unwrap_or_default();

        if self.panels.contains_key(&panel_name) || panel_name.is_empty() {
            self.last_rc = 0;
            0
        } else {
            self.last_rc = 12;
            12
        }
    }

    // -----------------------------------------------------------------------
    //  SETMSG / GETMSG
    // -----------------------------------------------------------------------

    fn exec_setmsg(&mut self, cmd: &str) -> i32 {
        if let Some(msg_id) = extract_paren(cmd, "MSG") {
            self.pending_msg = Some(msg_id);
            self.last_rc = 0;
            0
        } else {
            self.last_rc = 12;
            12
        }
    }

    fn exec_getmsg(&mut self, cmd: &str) -> i32 {
        if let Some(msg_id) = extract_paren(cmd, "MSG") {
            if let Some(msg) = self.messages.get(&msg_id) {
                self.vars.set("ZERRSM", msg.short_msg.clone());
                self.vars.set("ZERRLM", msg.long_msg.clone());
                self.last_rc = 0;
                0
            } else {
                self.last_rc = 12;
                12
            }
        } else {
            self.last_rc = 12;
            12
        }
    }

    // -----------------------------------------------------------------------
    //  CONTROL
    // -----------------------------------------------------------------------

    fn exec_control(&mut self, cmd: &str) -> i32 {
        if cmd.contains("ERRORS") && cmd.contains("RETURN") {
            self.errors_return = true;
        } else if cmd.contains("ERRORS") && cmd.contains("CANCEL") {
            self.errors_return = false;
        }
        self.last_rc = 0;
        0
    }

    // -----------------------------------------------------------------------
    //  ADDPOP / REMPOP
    // -----------------------------------------------------------------------

    fn exec_addpop(&mut self, cmd: &str) -> i32 {
        let row = extract_paren(cmd, "ROW")
            .and_then(|v| v.parse::<u16>().ok())
            .unwrap_or(1);
        let col = extract_paren(cmd, "COLUMN")
            .and_then(|v| v.parse::<u16>().ok())
            .unwrap_or(1);
        self.popup_stack.push((row, col));
        self.display_log.push(DisplayEvent::PopupAdd { row, col });
        self.last_rc = 0;
        0
    }

    fn exec_rempop(&mut self) -> i32 {
        if self.popup_stack.pop().is_some() {
            self.display_log.push(DisplayEvent::PopupRemove);
            self.last_rc = 0;
            0
        } else {
            self.last_rc = 8;
            8
        }
    }

    // -----------------------------------------------------------------------
    //  VGET / VPUT / VERASE passthrough
    // -----------------------------------------------------------------------

    fn exec_vget(&mut self, cmd: &str) -> i32 {
        let (vars, pool) = parse_var_list_and_pool(cmd, "VGET");
        self.vars.vget(&vars, pool);
        self.last_rc = 0;
        0
    }

    fn exec_vput(&mut self, cmd: &str) -> i32 {
        let (vars, pool) = parse_var_list_and_pool(cmd, "VPUT");
        self.vars.vput(&vars, pool);
        self.last_rc = 0;
        0
    }

    fn exec_verase(&mut self, cmd: &str) -> i32 {
        let (vars, pool) = parse_var_list_and_pool(cmd, "VERASE");
        self.vars.verase(&vars, pool);
        self.last_rc = 0;
        0
    }

    // -----------------------------------------------------------------------
    //  VCOPY / VDEFINE / VRESET
    // -----------------------------------------------------------------------

    fn exec_vcopy(&mut self, cmd: &str) -> i32 {
        // VCOPY name MYLENGTH MYVALUE MOVE|LOCATE
        // Simplified: VCOPY NAME — copies value + length into function pool.
        let name = cmd.split_whitespace().nth(1).unwrap_or("");
        if name.is_empty() {
            self.last_rc = 20;
            return 20;
        }
        let rc = self.vars.vcopy(name);
        self.last_rc = rc;
        rc
    }

    fn exec_vdefine(&mut self, cmd: &str) -> i32 {
        // VDEFINE (name) TYPE(CHAR|FIXED|BIT|HEX)
        let (vars, _pool) = parse_var_list_and_pool(cmd, "VDEFINE");
        let var_type = extract_paren(cmd, "TYPE")
            .map(|s| VarType::from_str(&s))
            .unwrap_or(VarType::Char);
        for var in &vars {
            self.vars.vdefine(var, var_type, None);
        }
        self.last_rc = 0;
        0
    }

    fn exec_vreset(&mut self) -> i32 {
        self.vars.vreset();
        self.last_rc = 0;
        0
    }

    // -----------------------------------------------------------------------
    //  Table services (delegated to TableManager)
    // -----------------------------------------------------------------------

    fn exec_tbcreate(&mut self, cmd: &str) -> i32 {
        let table_name = cmd.split_whitespace().nth(1).unwrap_or("");
        let keys = extract_paren(cmd, "KEYS")
            .map(|s| s.split_whitespace().map(|w| w.to_uppercase()).collect::<Vec<_>>())
            .unwrap_or_default();
        let names = extract_paren(cmd, "NAMES")
            .map(|s| s.split_whitespace().map(|w| w.to_uppercase()).collect::<Vec<_>>())
            .unwrap_or_default();
        let writable = !cmd.contains("NOWRITE");
        let rc = self.tables.tbcreate(table_name, &keys, &names, writable);
        self.last_rc = rc;
        rc
    }

    fn exec_tbopen(&mut self, cmd: &str) -> i32 {
        let table_name = cmd.split_whitespace().nth(1).unwrap_or("");
        let writable = cmd.contains("WRITE");
        let rc = self.tables.tbopen(table_name, writable);
        self.last_rc = rc;
        rc
    }

    fn exec_tbclose(&mut self, cmd: &str) -> i32 {
        let table_name = cmd.split_whitespace().nth(1).unwrap_or("");
        let rc = self.tables.tbclose(table_name);
        self.last_rc = rc;
        rc
    }

    fn exec_tbend(&mut self, cmd: &str) -> i32 {
        let table_name = cmd.split_whitespace().nth(1).unwrap_or("");
        let rc = self.tables.tbend(table_name);
        self.last_rc = rc;
        rc
    }

    fn exec_tbsave(&mut self, cmd: &str) -> i32 {
        let table_name = cmd.split_whitespace().nth(1).unwrap_or("");
        let rc = self.tables.tbsave(table_name);
        self.last_rc = rc;
        rc
    }

    fn exec_tbadd(&mut self, cmd: &str) -> i32 {
        let table_name = cmd.split_whitespace().nth(1).unwrap_or("");
        let vars = self.collect_func_vars();
        let rc = self.tables.tbadd(table_name, &vars);
        self.last_rc = rc;
        rc
    }

    fn exec_tbput(&mut self, cmd: &str) -> i32 {
        let table_name = cmd.split_whitespace().nth(1).unwrap_or("");
        let vars = self.collect_func_vars();
        let rc = self.tables.tbput(table_name, &vars);
        self.last_rc = rc;
        rc
    }

    fn exec_tbmod(&mut self, cmd: &str) -> i32 {
        let table_name = cmd.split_whitespace().nth(1).unwrap_or("");
        let vars = self.collect_func_vars();
        let rc = self.tables.tbmod(table_name, &vars);
        self.last_rc = rc;
        rc
    }

    fn exec_tbdelete(&mut self, cmd: &str) -> i32 {
        let table_name = cmd.split_whitespace().nth(1).unwrap_or("");
        let rc = self.tables.tbdelete(table_name);
        self.last_rc = rc;
        rc
    }

    fn exec_tbtop(&mut self, cmd: &str) -> i32 {
        let table_name = cmd.split_whitespace().nth(1).unwrap_or("");
        let rc = self.tables.tbtop(table_name);
        self.last_rc = rc;
        rc
    }

    fn exec_tbbot(&mut self, cmd: &str) -> i32 {
        let table_name = cmd.split_whitespace().nth(1).unwrap_or("");
        let rc = self.tables.tbbot(table_name);
        self.last_rc = rc;
        rc
    }

    fn exec_tbskip(&mut self, cmd: &str) -> i32 {
        let table_name = cmd.split_whitespace().nth(1).unwrap_or("");
        let count = extract_paren(cmd, "NUMBER")
            .and_then(|s| s.parse::<i32>().ok())
            .unwrap_or(1);
        let (rc, row) = self.tables.tbskip(table_name, count);
        if let Some(row) = row {
            for (k, v) in &row {
                self.vars.set(k, v.clone());
            }
        }
        self.last_rc = rc;
        rc
    }

    fn exec_tbsort(&mut self, cmd: &str) -> i32 {
        let table_name = cmd.split_whitespace().nth(1).unwrap_or("");
        let fields = extract_paren(cmd, "FIELDS").unwrap_or_default();
        let rc = self.tables.tbsort(table_name, &fields);
        self.last_rc = rc;
        rc
    }

    fn exec_tbscan(&mut self, cmd: &str) -> i32 {
        let table_name = cmd.split_whitespace().nth(1).unwrap_or("");
        let arglist = extract_paren(cmd, "ARGLIST")
            .map(|s| s.split_whitespace().map(|w| w.to_string()).collect::<Vec<_>>());
        let condlist = extract_paren(cmd, "CONDLIST")
            .map(|s| s.split_whitespace().map(|w| w.to_string()).collect::<Vec<_>>());
        let vars = self.collect_func_vars();

        let (rc, row) = if let Some(args) = &arglist {
            let conds = condlist.as_deref().unwrap_or(&[]);
            self.tables.tbscan(table_name, Some((args, conds)), &vars)
        } else {
            self.tables.tbscan(table_name, None, &vars)
        };

        if let Some(row) = row {
            for (k, v) in &row {
                self.vars.set(k, v.clone());
            }
        }
        self.last_rc = rc;
        rc
    }

    fn exec_tbsarg(&mut self, cmd: &str) -> i32 {
        let table_name = cmd.split_whitespace().nth(1).unwrap_or("");
        let arglist = extract_paren(cmd, "ARGLIST")
            .map(|s| s.split_whitespace().map(|w| w.to_string()).collect::<Vec<_>>())
            .unwrap_or_default();
        let condlist = extract_paren(cmd, "CONDLIST")
            .map(|s| s.split_whitespace().map(|w| w.to_string()).collect::<Vec<_>>())
            .unwrap_or_default();
        let vars = self.collect_func_vars();
        let rc = self.tables.tbsarg(table_name, &arglist, &condlist, &vars);
        self.last_rc = rc;
        rc
    }

    /// Collect all variables from the current function pool as a map.
    fn collect_func_vars(&self) -> HashMap<String, String> {
        self.vars.current_function_vars()
    }

    // -----------------------------------------------------------------------
    //  File tailoring services
    // -----------------------------------------------------------------------

    fn exec_ftopen(&mut self) -> i32 {
        let rc = self.file_tailor.ftopen();
        self.last_rc = rc;
        rc
    }

    fn exec_ftincl(&mut self, cmd: &str) -> i32 {
        let skel_name = cmd.split_whitespace().nth(1).unwrap_or("");
        let rc = self.file_tailor.ftincl(skel_name, &self.vars, &mut self.tables);
        self.last_rc = rc;
        rc
    }

    fn exec_ftclose(&mut self, cmd: &str) -> i32 {
        let _out_dd = extract_paren(cmd, "NAME");
        let (rc, output) = self.file_tailor.ftclose();
        // Store the output in the ZFTOUT variable for retrieval.
        self.vars.set("ZFTOUT", output.join("\n"));
        self.last_rc = rc;
        rc
    }

    // -----------------------------------------------------------------------
    //  Panel statement execution
    // -----------------------------------------------------------------------

    fn exec_panel_stmts(&mut self, stmts: &[PanelStmt]) -> i32 {
        for stmt in stmts {
            match stmt {
                PanelStmt::Assign { var, value } => {
                    let val = self.eval_panel_expr(value);
                    self.vars.set(var, val);
                }
                PanelStmt::If { cond, then_stmts, else_stmts } => {
                    if self.eval_panel_cond(cond) {
                        self.exec_panel_stmts(then_stmts);
                    } else {
                        self.exec_panel_stmts(else_stmts);
                    }
                }
                PanelStmt::Ver { field, checks, msg } => {
                    let val = self.vars.get(field).unwrap_or_default();
                    for check in checks {
                        let ok = match check {
                            VerCheck::NonBlank => !val.trim().is_empty(),
                            VerCheck::Numeric => val.chars().all(|c| c.is_ascii_digit() || c == '.' || c == '-'),
                            VerCheck::Alpha => val.chars().all(|c| c.is_ascii_alphabetic()),
                            VerCheck::Dsname => !val.is_empty() && val.len() <= 44,
                            VerCheck::List(items) => items.contains(&val),
                            VerCheck::Range(lo, hi) => val >= *lo && val <= *hi,
                        };
                        if !ok {
                            if let Some(msg_id) = msg {
                                self.pending_msg = Some(msg_id.clone());
                            }
                            return 8; // Verification failed.
                        }
                    }
                }
                PanelStmt::VGet { vars, pool } => {
                    self.vars.vget(vars, *pool);
                }
                PanelStmt::VPut { vars, pool } => {
                    self.vars.vput(vars, *pool);
                }
                PanelStmt::Refresh(_) | PanelStmt::Label(_) | PanelStmt::Goto(_) => {
                    // Stubs for now.
                }
            }
        }
        0
    }

    fn eval_panel_expr(&self, expr: &PanelExpr) -> String {
        match expr {
            PanelExpr::Literal(s) => {
                // Substitute &var references.
                substitute_vars(s, &self.vars)
            }
            PanelExpr::Trans { var, pairs, default } => {
                let val = self.vars.get(var).unwrap_or_default();
                for (key, result) in pairs {
                    if val == *key {
                        return result.clone();
                    }
                }
                default.clone().unwrap_or(val)
            }
            PanelExpr::Trunc { var, delim } => {
                let val = self.vars.get(var).unwrap_or_default();
                if let Some(pos) = val.find(*delim) {
                    val[..pos].to_string()
                } else {
                    val
                }
            }
        }
    }

    fn eval_panel_cond(&self, cond: &PanelCond) -> bool {
        match cond {
            PanelCond::Compare { var, op, value } => {
                let val = self.vars.get(var).unwrap_or_default();
                let rhs = substitute_vars(value, &self.vars);
                match op {
                    CmpOp::Eq => val == rhs,
                    CmpOp::Ne => val != rhs,
                    CmpOp::Gt => val > rhs,
                    CmpOp::Lt => val < rhs,
                    CmpOp::Ge => val >= rhs,
                    CmpOp::Le => val <= rhs,
                }
            }
            PanelCond::Not(inner) => !self.eval_panel_cond(inner),
            PanelCond::And(a, b) => self.eval_panel_cond(a) && self.eval_panel_cond(b),
            PanelCond::Or(a, b) => self.eval_panel_cond(a) || self.eval_panel_cond(b),
        }
    }
}

impl Default for DialogManager {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for IspfVarPools {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
//  Helpers
// ---------------------------------------------------------------------------

/// Extract parenthesized value: `KEY(VALUE)` → `"VALUE"`.
fn extract_paren(text: &str, key: &str) -> Option<String> {
    let pat = format!("{key}(");
    if let Some(start) = text.find(&pat) {
        let after = &text[start + pat.len()..];
        if let Some(end) = after.find(')') {
            return Some(after[..end].to_string());
        }
    }
    None
}

/// Parse variable list and pool from VGET/VPUT/VERASE command.
fn parse_var_list_and_pool(cmd: &str, keyword: &str) -> (Vec<String>, PanelVarPool) {
    let after = cmd.find(keyword)
        .map(|i| &cmd[i + keyword.len()..])
        .unwrap_or("");

    let vars_str = if let (Some(start), Some(end)) = (after.find('('), after.find(')')) {
        &after[start + 1..end]
    } else {
        ""
    };

    let vars: Vec<String> = vars_str
        .split_whitespace()
        .map(|s| s.trim_start_matches('&').to_uppercase())
        .filter(|s| !s.is_empty())
        .collect();

    let remainder = after.rfind(')').map(|i| &after[i + 1..]).unwrap_or("");
    let pool = if remainder.contains("PROFILE") {
        PanelVarPool::Profile
    } else if remainder.contains("ASIS") {
        PanelVarPool::Asis
    } else {
        PanelVarPool::Shared
    };

    (vars, pool)
}

/// Substitute `&var` references in a string with variable values.
fn substitute_vars(text: &str, vars: &IspfVarPools) -> String {
    let mut result = String::new();
    let chars: Vec<char> = text.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        if chars[i] == '&' {
            let start = i + 1;
            let mut end = start;
            while end < chars.len() && (chars[end].is_ascii_alphanumeric() || chars[end] == '_') {
                end += 1;
            }
            if end > start {
                let var_name: String = chars[start..end].iter().collect();
                let val = vars.get(&var_name).unwrap_or_default();
                result.push_str(&val);
                // Skip trailing period if present (ISPF concatenation).
                if end < chars.len() && chars[end] == '.' {
                    end += 1;
                }
                i = end;
            } else {
                result.push('&');
                i += 1;
            }
        } else if chars[i] == '\'' {
            // Skip quotes.
            i += 1;
        } else {
            result.push(chars[i]);
            i += 1;
        }
    }

    result
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::panel::parse_panel;

    #[test]
    fn test_dialog_manager_display() {
        let source = r#")ATTR DEFAULT(%+_)
% TYPE(TEXT) INTENS(HIGH)
_ TYPE(INPUT) INTENS(LOW)
)BODY
%COMMAND ===>_ZCMD
)INIT
  &ZCMD = ''
)PROC
)END
"#;
        let panel = parse_panel("MYPANEL", source).unwrap();
        let mut dm = DialogManager::new();
        dm.load_panel(panel);
        let rc = dm.exec("DISPLAY PANEL(MYPANEL)");
        assert_eq!(rc, 0);
        assert!(!dm.display_log.is_empty());
    }

    #[test]
    fn test_dialog_manager_setmsg() {
        let mut dm = DialogManager::new();
        dm.register_message(MessageDef {
            id: "ISRZ001".to_string(),
            short_msg: "Error".to_string(),
            long_msg: "An error occurred".to_string(),
            alarm: false,
        });

        let source = r#")ATTR DEFAULT(%+_)
)BODY
%Test Panel
)END
"#;
        let panel = parse_panel("TEST", source).unwrap();
        dm.load_panel(panel);

        dm.exec("SETMSG MSG(ISRZ001)");
        dm.exec("DISPLAY PANEL(TEST)");

        let has_msg = dm.display_log.iter().any(|e| matches!(e, DisplayEvent::Message { id, .. } if id == "ISRZ001"));
        assert!(has_msg);
    }

    #[test]
    fn test_dialog_manager_select() {
        let mut dm = DialogManager::new();
        let rc = dm.exec("SELECT PGM(ISRBRO)");
        assert_eq!(rc, 0);
        let has_select = dm.display_log.iter().any(|e| matches!(e, DisplayEvent::Select { target } if target.contains("ISRBRO")));
        assert!(has_select);
    }

    #[test]
    fn test_dialog_manager_control() {
        let mut dm = DialogManager::new();
        dm.exec("CONTROL ERRORS RETURN");
        assert!(dm.errors_return);
        dm.exec("CONTROL ERRORS CANCEL");
        assert!(!dm.errors_return);
    }

    #[test]
    fn test_dialog_manager_addpop_rempop() {
        let mut dm = DialogManager::new();
        dm.exec("ADDPOP ROW(5) COLUMN(10)");
        assert_eq!(dm.popup_stack.len(), 1);
        assert_eq!(dm.popup_stack[0], (5, 10));
        dm.exec("REMPOP");
        assert_eq!(dm.popup_stack.len(), 0);
    }

    #[test]
    fn test_var_pools_function_scope() {
        let mut vars = IspfVarPools::new();
        vars.set("MYVAR", "hello".into());
        assert_eq!(vars.get("MYVAR"), Some("hello".to_string()));

        vars.push_function();
        // New function pool doesn't see parent.
        assert_eq!(vars.get("MYVAR"), None);
        vars.set("MYVAR", "world".into());
        assert_eq!(vars.get("MYVAR"), Some("world".to_string()));

        vars.pop_function();
        // Back to original.
        assert_eq!(vars.get("MYVAR"), Some("hello".to_string()));
    }

    #[test]
    fn test_var_pools_vget_vput() {
        let mut vars = IspfVarPools::new();
        vars.set("DSN", "MY.DATA".into());
        vars.vput(&["DSN".into()], PanelVarPool::Shared);

        vars.push_function();
        assert_eq!(vars.get("DSN"), Some("MY.DATA".to_string())); // Found in shared.
        vars.vget(&["DSN".into()], PanelVarPool::Shared);
        // Now it's also in the function pool.
        assert_eq!(vars.function_stack.last().unwrap().get("DSN"), Some(&"MY.DATA".to_string()));

        vars.pop_function();
    }

    #[test]
    fn test_var_pools_system_vars() {
        let vars = IspfVarPools::new();
        assert_eq!(vars.get("ZUSER"), Some("USER01".to_string()));
        assert_eq!(vars.get("ZPREFIX"), Some("USER01".to_string()));
    }

    #[test]
    fn test_var_pools_verase() {
        let mut vars = IspfVarPools::new();
        vars.shared.insert("TEMPVAR".into(), "temp".into());
        assert_eq!(vars.get("TEMPVAR"), Some("temp".to_string()));
        vars.verase(&["TEMPVAR".into()], PanelVarPool::Shared);
        assert_eq!(vars.get("TEMPVAR"), None);
    }

    #[test]
    fn test_substitute_vars() {
        let mut vars = IspfVarPools::new();
        vars.set("PREFIX", "USER01".into());
        let result = substitute_vars("&PREFIX..DATA", &vars);
        assert_eq!(result, "USER01.DATA");
    }

    #[test]
    fn test_panel_init_vget() {
        let source = r#")ATTR DEFAULT(%+_)
)BODY
%Test
)INIT
  VGET (ZUSER) SHARED
  &WHO = &ZUSER
)PROC
)END
"#;
        let panel = parse_panel("TEST", source).unwrap();
        let mut dm = DialogManager::new();
        dm.load_panel(panel);
        dm.exec("DISPLAY PANEL(TEST)");
        assert_eq!(dm.vars.get("WHO"), Some("USER01".to_string()));
    }

    #[test]
    fn test_getmsg() {
        let mut dm = DialogManager::new();
        dm.register_message(MessageDef {
            id: "TST001".to_string(),
            short_msg: "Short".to_string(),
            long_msg: "Long message".to_string(),
            alarm: false,
        });

        let rc = dm.exec("GETMSG MSG(TST001)");
        assert_eq!(rc, 0);
        assert_eq!(dm.vars.get("ZERRSM"), Some("Short".to_string()));
        assert_eq!(dm.vars.get("ZERRLM"), Some("Long message".to_string()));
    }

    #[test]
    fn test_panel_ver_nonblank() {
        let source = r#")ATTR DEFAULT(%+_)
_ TYPE(INPUT) INTENS(LOW)
)BODY
_DSN
)PROC
  VER (&DSN,NB,MSG=ISRZ002)
)END
"#;
        let panel = parse_panel("TEST", source).unwrap();
        let mut dm = DialogManager::new();
        dm.load_panel(panel);
        // DSN is empty by default — VER should fail.
        let rc = dm.exec("DISPLAY PANEL(TEST)");
        assert_eq!(rc, 8);
    }

    // -------------------------------------------------------------------
    //  T105 — Variable Services
    // -------------------------------------------------------------------

    #[test]
    fn test_vcopy() {
        let mut dm = DialogManager::new();
        dm.vars.set("MYDSN", "SYS1.MACLIB".into());
        let rc = dm.exec("VCOPY MYDSN");
        assert_eq!(rc, 0);
        // VCOPY places value + length into function pool.
        assert_eq!(dm.vars.get("MYDSN"), Some("SYS1.MACLIB".to_string()));
        assert_eq!(dm.vars.get("MYDSN_LENGTH"), Some("11".to_string()));
    }

    #[test]
    fn test_vcopy_not_found() {
        let mut dm = DialogManager::new();
        let rc = dm.exec("VCOPY NOSUCHVAR");
        assert_eq!(rc, 8);
    }

    #[test]
    fn test_vdefine_char() {
        let mut dm = DialogManager::new();
        let rc = dm.exec("VDEFINE (MYFIELD) TYPE(CHAR)");
        assert_eq!(rc, 0);
        assert_eq!(dm.vars.get("MYFIELD_TYPE"), Some("CHAR".to_string()));
    }

    #[test]
    fn test_vdefine_fixed() {
        let mut dm = DialogManager::new();
        let rc = dm.exec("VDEFINE (COUNT) TYPE(FIXED)");
        assert_eq!(rc, 0);
        assert_eq!(dm.vars.get("COUNT_TYPE"), Some("FIXED".to_string()));
    }

    #[test]
    fn test_vreset() {
        let mut dm = DialogManager::new();
        dm.exec("VDEFINE (A) TYPE(CHAR)");
        dm.exec("VDEFINE (B) TYPE(FIXED)");
        assert!(dm.vars.get("A_TYPE").is_some());
        assert!(dm.vars.get("B_TYPE").is_some());

        dm.exec("VRESET");
        assert!(dm.vars.get("A_TYPE").is_none());
        assert!(dm.vars.get("B_TYPE").is_none());
    }

    #[test]
    fn test_system_variables_extended() {
        let vars = IspfVarPools::new();
        // Session.
        assert_eq!(vars.get("ZLOGON"), Some("TSO".to_string()));
        assert_eq!(vars.get("ZAPPLID"), Some("ISR".to_string()));
        // Date/time.
        assert!(vars.get("ZJDATE").is_some());
        assert!(vars.get("ZDAY").is_some());
        assert!(vars.get("ZMONTH").is_some());
        assert!(vars.get("ZYEAR").is_some());
        assert!(vars.get("ZJ4DATE").is_some());
        assert!(vars.get("ZDAYOFWK").is_some());
        // Screen.
        assert_eq!(vars.get("ZSCRDEPTH"), Some("24".to_string()));
        assert_eq!(vars.get("ZSCRWIDTH"), Some("80".to_string()));
        // Environment.
        assert!(vars.get("ZENVIR").is_some());
        assert!(vars.get("ZSYSID").is_some());
        assert_eq!(vars.get("ZTERM"), Some("3278-2".to_string()));
        assert_eq!(vars.get("ZKEYS"), Some("24".to_string()));
    }

    #[test]
    fn test_profile_pool_persistence() {
        let mut vars = IspfVarPools::new();
        vars.set("PREF", "OLDVAL".into());
        vars.vput(&["PREF".into()], PanelVarPool::Profile);

        // Save profile.
        let saved = vars.save_profile();
        assert_eq!(saved.get("PREF"), Some(&"OLDVAL".to_string()));

        // Create a fresh pool and load the profile.
        let mut vars2 = IspfVarPools::new();
        vars2.load_profile(saved);
        // Profile pool is searchable.
        assert_eq!(vars2.get("PREF"), Some("OLDVAL".to_string()));
    }

    #[test]
    fn test_function_pool_destroyed_on_return() {
        let mut vars = IspfVarPools::new();
        vars.set("OUTER", "1".into());
        assert_eq!(vars.function_depth(), 1);

        vars.push_function(); // SELECT level 2.
        assert_eq!(vars.function_depth(), 2);
        vars.set("INNER", "2".into());
        assert_eq!(vars.get("INNER"), Some("2".to_string()));
        assert_eq!(vars.get("OUTER"), None); // Not visible in inner function.

        vars.pop_function();
        assert_eq!(vars.function_depth(), 1);
        assert_eq!(vars.get("INNER"), None); // Destroyed.
        assert_eq!(vars.get("OUTER"), Some("1".to_string())); // Restored.
    }

    #[test]
    fn test_vget_shared_across_select_levels() {
        let mut dm = DialogManager::new();
        dm.vars.set("DSN", "MY.DATASET".into());
        dm.exec("VPUT (DSN) SHARED");

        // Simulate SELECT by pushing a new function pool.
        dm.vars.push_function();
        dm.exec("VGET (DSN) SHARED");
        assert_eq!(dm.vars.get("DSN"), Some("MY.DATASET".to_string()));
        dm.vars.pop_function();
    }

    #[test]
    fn test_vput_profile_survives_function_pop() {
        let mut vars = IspfVarPools::new();
        vars.set("SETTING", "YES".into());
        vars.vput(&["SETTING".into()], PanelVarPool::Profile);

        // Pop and push doesn't lose profile.
        vars.push_function();
        vars.pop_function();

        // Profile pool still has it.
        assert_eq!(vars.profile.get("SETTING"), Some(&"YES".to_string()));
    }

    #[test]
    fn test_verase_asis_removes_both() {
        let mut vars = IspfVarPools::new();
        vars.shared.insert("BOTH".into(), "S".into());
        vars.profile.insert("BOTH".into(), "P".into());
        vars.verase(&["BOTH".into()], PanelVarPool::Asis);
        assert!(vars.shared.get("BOTH").is_none());
        assert!(vars.profile.get("BOTH").is_none());
    }

    #[test]
    fn test_pf_key_system_vars() {
        let vars = IspfVarPools::new();
        // PF keys 01–24 should be present.
        for i in 1..=24 {
            let key = format!("ZPF{i:02}");
            assert!(vars.get(&key).is_some(), "Missing {key}");
        }
    }
}
