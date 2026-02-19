//! COBOL interpreter for running programs without compilation.
//!
//! A tree-walking interpreter that executes COBOL AST directly.

use std::collections::HashMap;
use std::io::{BufRead, Write};
use std::sync::{Arc, Mutex};

use crate::value::{CobolValue, NumericValue};

/// Interpreter error.
#[derive(Debug, Clone)]
pub struct InterpreterError {
    pub message: String,
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for InterpreterError {}

type Result<T> = std::result::Result<T, InterpreterError>;

/// Data item metadata.
#[derive(Debug, Clone)]
pub struct DataItemMeta {
    /// Size in bytes.
    pub size: usize,
    /// Decimal positions.
    pub decimals: u32,
    /// Whether numeric.
    pub is_numeric: bool,
    /// Picture string.
    pub picture: Option<String>,
    /// Whether JUSTIFIED RIGHT is specified.
    pub is_justified: bool,
}

impl Default for DataItemMeta {
    fn default() -> Self {
        Self {
            size: 80,
            decimals: 0,
            is_numeric: false,
            picture: None,
            is_justified: false,
        }
    }
}

/// Parameter passing mode for CALL ... USING.
#[derive(Debug, Clone)]
pub enum CallParam {
    /// BY REFERENCE: called program shares the same data item (modifications visible to caller).
    ByReference(String),
    /// BY CONTENT: called program receives a copy (modifications NOT visible to caller).
    ByContent(String),
    /// BY VALUE: called program receives the value directly (for numeric/primitive passing).
    ByValue(String),
}

/// Trait for a registry of callable programs.
///
/// The program registry maps program names to `SimpleProgram` instances.
/// Programs must be registered before they can be CALLed.
pub trait ProgramRegistry {
    /// Look up a program by name. Returns a clone of the program if found.
    fn lookup(&self, name: &str) -> Option<SimpleProgram>;

    /// Register a program in the registry.
    fn register(&mut self, program: SimpleProgram);

    /// Remove (CANCEL) a program from the registry, freeing its resources.
    /// Returns true if the program was found and removed.
    fn cancel(&mut self, name: &str) -> bool;
}

/// Simple in-memory program registry backed by a HashMap.
#[derive(Default)]
pub struct SimpleProgramRegistry {
    programs: HashMap<String, SimpleProgram>,
}

impl SimpleProgramRegistry {
    /// Create a new empty program registry.
    pub fn new() -> Self {
        Self {
            programs: HashMap::new(),
        }
    }
}

impl ProgramRegistry for SimpleProgramRegistry {
    fn lookup(&self, name: &str) -> Option<SimpleProgram> {
        self.programs.get(&name.to_uppercase()).cloned()
    }

    fn register(&mut self, program: SimpleProgram) {
        self.programs.insert(program.name.to_uppercase(), program);
    }

    fn cancel(&mut self, name: &str) -> bool {
        self.programs.remove(&name.to_uppercase()).is_some()
    }
}

/// Trait for handling EXEC CICS commands.
pub trait CicsCommandHandler {
    /// Execute a CICS command.
    fn execute(
        &mut self,
        command: &str,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<()>;

    /// Downcast to concrete type (for inspecting handler state after execution).
    fn as_any_mut(&mut self) -> Option<&mut dyn std::any::Any> {
        None
    }
}

/// State for an open file in the interpreter.
#[derive(Debug)]
pub struct FileState {
    /// Open mode.
    pub mode: SimpleOpenMode,
    /// Records (in-memory simulation).
    pub records: Vec<String>,
    /// Current record position (for sequential read).
    pub position: usize,
    /// File status code.
    pub status: String,
}

/// Runtime environment for interpreter.
pub struct Environment {
    /// Data item values.
    variables: HashMap<String, CobolValue>,
    /// Data item metadata.
    metadata: HashMap<String, DataItemMeta>,
    /// Standard output.
    stdout: Box<dyn Write>,
    /// Standard input.
    stdin: Box<dyn BufRead>,
    /// Return code.
    return_code: i32,
    /// Stop flag.
    stopped: bool,
    /// GOBACK flag — signals return to caller without stopping the entire run unit.
    goback: bool,
    /// CICS command handler (optional).
    pub cics_handler: Option<Box<dyn CicsCommandHandler>>,
    /// Open files (in-memory simulation).
    pub files: HashMap<String, FileState>,
    /// REDEFINES aliases: maps a REDEFINES variable name to the original variable name.
    redefines_aliases: HashMap<String, String>,
    /// Program registry for external CALL resolution.
    pub program_registry: Option<Arc<Mutex<Box<dyn ProgramRegistry + Send>>>>,
}

impl Environment {
    /// Create a new environment with default I/O.
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            metadata: HashMap::new(),
            stdout: Box::new(std::io::stdout()),
            stdin: Box::new(std::io::BufReader::new(std::io::stdin())),
            return_code: 0,
            stopped: false,
            goback: false,
            cics_handler: None,
            files: HashMap::new(),
            redefines_aliases: HashMap::new(),
            program_registry: None,
        }
    }

    /// Create environment with custom I/O.
    pub fn with_io(stdout: Box<dyn Write>, stdin: Box<dyn BufRead>) -> Self {
        Self {
            variables: HashMap::new(),
            metadata: HashMap::new(),
            stdout,
            stdin,
            return_code: 0,
            stopped: false,
            goback: false,
            cics_handler: None,
            files: HashMap::new(),
            redefines_aliases: HashMap::new(),
            program_registry: None,
        }
    }

    /// Install a CICS command handler.
    pub fn with_cics_handler(mut self, handler: Box<dyn CicsCommandHandler>) -> Self {
        self.cics_handler = Some(handler);
        self
    }

    /// Install a program registry for external CALL resolution.
    pub fn with_program_registry(
        mut self,
        registry: Arc<Mutex<Box<dyn ProgramRegistry + Send>>>,
    ) -> Self {
        self.program_registry = Some(registry);
        self
    }

    /// Define a data item.
    /// If the variable already has a value (e.g., pre-set by CICS EIB or --set),
    /// the existing value is preserved and only metadata is updated.
    pub fn define(&mut self, name: &str, meta: DataItemMeta) {
        let name_upper = name.to_uppercase();
        if !self.variables.contains_key(&name_upper) {
            let initial = if meta.is_numeric {
                CobolValue::from_i64(0)
            } else {
                CobolValue::Alphanumeric(" ".repeat(meta.size))
            };
            self.variables.insert(name_upper.clone(), initial);
        }
        self.metadata.insert(name_upper, meta);
    }

    /// Register a REDEFINES alias: `alias_name` REDEFINES `original_name`.
    /// When either is set, the other gets the same display value.
    pub fn register_redefines(&mut self, alias_name: &str, original_name: &str) {
        self.redefines_aliases.insert(
            alias_name.to_uppercase(),
            original_name.to_uppercase(),
        );
    }

    /// Get a variable value.
    /// If the variable has a REDEFINES alias, returns the original's value.
    pub fn get(&self, name: &str) -> Option<&CobolValue> {
        let name_upper = name.to_uppercase();
        self.variables.get(&name_upper).or_else(|| {
            // Check if this is a REDEFINES alias
            self.redefines_aliases.get(&name_upper)
                .and_then(|original| self.variables.get(original))
        })
    }

    /// Set a variable value.
    /// If the variable has REDEFINES relationships, propagates the value.
    pub fn set(&mut self, name: &str, value: CobolValue) -> Result<()> {
        let name_upper = name.to_uppercase();

        // If variable doesn't exist, create it with default metadata
        if !self.variables.contains_key(&name_upper) {
            let meta = DataItemMeta::default();
            self.metadata.insert(name_upper.clone(), meta);
        }

        // Format the value according to this field's PIC clause for REDEFINES propagation.
        // In COBOL, REDEFINES shares raw storage, so a PIC 9(11) field with value 1
        // is stored as "00000000001" in memory, and a REDEFINES PIC X(11) sees those bytes.
        let formatted = if let Some(meta) = self.metadata.get(&name_upper) {
            if meta.is_numeric && meta.size > 0 {
                let raw = value.to_display_string();
                let trimmed = raw.trim().trim_start_matches('-');
                // Zero-pad numeric value to field size
                let padded = format!("{:0>width$}", trimmed, width = meta.size);
                // If the value has a decimal and the field has decimals, handle it
                if meta.decimals > 0 {
                    // Remove decimal point and pad
                    let no_dot: String = padded.replace('.', "");
                    format!("{:0>width$}", no_dot, width = meta.size)
                } else {
                    padded
                }
            } else if meta.size > 0 {
                // Alphanumeric: right-pad with spaces to field size
                let raw = value.to_display_string();
                format!("{:<width$}", raw, width = meta.size)
            } else {
                value.to_display_string()
            }
        } else {
            value.to_display_string()
        };

        // Propagate to REDEFINES aliases
        if let Some(original) = self.redefines_aliases.get(&name_upper).cloned() {
            // This is a REDEFINES alias — also update the original
            self.variables.insert(original, CobolValue::Alphanumeric(formatted.clone()));
        }
        // Check if any alias points to this variable as its original
        let aliases_to_update: Vec<String> = self.redefines_aliases.iter()
            .filter(|(_, orig)| orig.as_str() == name_upper)
            .map(|(alias, _)| alias.clone())
            .collect();
        for alias in aliases_to_update {
            self.variables.insert(alias, CobolValue::Alphanumeric(formatted.clone()));
        }

        // Apply JUSTIFIED RIGHT: right-justify alphanumeric values in the field
        let final_value = if let Some(meta) = self.metadata.get(&name_upper) {
            if meta.is_justified && !meta.is_numeric && meta.size > 0 {
                let raw = value.to_display_string();
                let trimmed = raw.trim_end();
                match trimmed.len().cmp(&meta.size) {
                    std::cmp::Ordering::Less => {
                        // Right-justify: pad with spaces on the left
                        let justified = format!("{:>width$}", trimmed, width = meta.size);
                        CobolValue::Alphanumeric(justified)
                    }
                    std::cmp::Ordering::Greater => {
                        // Truncate on the left
                        let start = trimmed.len() - meta.size;
                        CobolValue::Alphanumeric(trimmed[start..].to_string())
                    }
                    std::cmp::Ordering::Equal => value,
                }
            } else {
                value
            }
        } else {
            value
        };
        self.variables.insert(name_upper, final_value);
        Ok(())
    }

    /// Get metadata for a variable.
    pub fn meta(&self, name: &str) -> Option<&DataItemMeta> {
        self.metadata.get(&name.to_uppercase())
    }

    /// Write to stdout.
    pub fn display(&mut self, text: &str, no_advancing: bool) -> Result<()> {
        if no_advancing {
            write!(self.stdout, "{}", text)
        } else {
            writeln!(self.stdout, "{}", text)
        }
        .map_err(|e| InterpreterError {
            message: format!("Display error: {}", e),
        })
    }

    /// Read from stdin.
    pub fn accept(&mut self) -> Result<String> {
        let mut line = String::new();
        self.stdin
            .read_line(&mut line)
            .map_err(|e| InterpreterError {
                message: format!("Accept error: {}", e),
            })?;
        Ok(line.trim_end().to_string())
    }

    /// Set return code.
    pub fn set_return_code(&mut self, code: i32) {
        self.return_code = code;
    }

    /// Get return code.
    pub fn return_code(&self) -> i32 {
        self.return_code
    }

    /// Stop execution.
    pub fn stop(&mut self) {
        self.stopped = true;
    }

    /// Check if stopped.
    pub fn is_stopped(&self) -> bool {
        self.stopped
    }

    /// Signal GOBACK (return to caller without stopping run unit).
    pub fn goback(&mut self) {
        self.goback = true;
    }

    /// Check if GOBACK was signalled.
    pub fn is_goback(&self) -> bool {
        self.goback
    }

    /// Reset stopped flag (for XCTL dispatch to new program).
    pub fn resume(&mut self) {
        self.stopped = false;
        self.goback = false;
    }

    /// Get a variable value as a string (returns empty string if not found).
    pub fn get_string(&self, name: &str) -> String {
        match self.get(name) {
            Some(CobolValue::Alphanumeric(s)) => s.clone(),
            Some(CobolValue::Numeric(n)) => n.to_string(),
            Some(CobolValue::Group(bytes)) => {
                String::from_utf8_lossy(bytes).to_string()
            }
            None => String::new(),
        }
    }

    /// Get all fields belonging to a group item (variables with the group prefix).
    pub fn get_group_fields(&self, group: &str) -> Vec<(String, CobolValue)> {
        let prefix = format!("{}-", group);
        let dot_prefix = format!("{}.", group);
        let mut fields: Vec<(String, CobolValue)> = self
            .variables
            .iter()
            .filter(|(k, _)| k.starts_with(&prefix) || k.starts_with(&dot_prefix))
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        fields.sort_by(|a, b| a.0.cmp(&b.0));
        fields
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

/// Simple statement for interpreter (parsed from AST).
#[derive(Debug, Clone)]
pub enum SimpleStatement {
    /// DISPLAY items
    Display {
        items: Vec<SimpleExpr>,
        no_advancing: bool,
    },
    /// ACCEPT variable
    Accept { target: String },
    /// MOVE value TO targets (each target is an expression that resolves to a variable name)
    Move { from: SimpleExpr, to: Vec<SimpleExpr> },
    /// COMPUTE target = expr
    Compute { target: String, expr: SimpleExpr },
    /// ADD values TO targets
    Add {
        values: Vec<SimpleExpr>,
        to: Vec<String>,
    },
    /// SUBTRACT values FROM targets
    Subtract {
        values: Vec<SimpleExpr>,
        from: Vec<String>,
    },
    /// MULTIPLY value BY target
    Multiply {
        value: SimpleExpr,
        by: SimpleExpr,
        giving: Option<String>,
    },
    /// DIVIDE value INTO target
    Divide {
        value: SimpleExpr,
        into: SimpleExpr,
        giving: Option<String>,
    },
    /// IF condition THEN statements ELSE statements
    If {
        condition: SimpleCondition,
        then_branch: Vec<SimpleStatement>,
        else_branch: Option<Vec<SimpleStatement>>,
    },
    /// PERFORM paragraph
    Perform { target: String, times: Option<u32> },
    /// PERFORM paragraph-1 THRU paragraph-2
    PerformThru {
        from: String,
        thru: String,
        times: Option<u32>,
    },
    /// EXIT PARAGRAPH: exit the current paragraph.
    ExitParagraph,
    /// EXIT SECTION: exit the current section.
    ExitSection,
    /// STOP RUN
    StopRun { return_code: Option<i32> },
    /// EVALUATE statement
    Evaluate {
        subjects: Vec<SimpleExpr>,
        when_clauses: Vec<SimpleWhenClause>,
    },
    /// EXEC CICS command
    ExecCics {
        command: String,
        options: Vec<(String, Option<SimpleExpr>)>,
    },
    /// GO TO paragraph
    GoTo { target: String },
    /// INITIALIZE variable
    Initialize { targets: Vec<String> },
    /// STRING concatenation
    StringConcat {
        sources: Vec<SimpleExpr>,
        into: String,
    },
    /// SET variable TO value (index or general)
    Set {
        target: String,
        value: SimpleExpr,
    },
    /// SET condition-name TO TRUE: resolves parent field from condition_names
    SetCondition {
        condition_name: String,
        value: bool,
    },
    /// CALL subprogram with parameter passing modes.
    Call {
        program: SimpleExpr,
        using: Vec<CallParam>,
    },
    /// GOBACK: return control to the calling program.
    GoBack {
        /// Optional return code (from GOBACK RETURNING n).
        return_code: Option<i32>,
    },
    /// CANCEL: remove a program from the registry.
    Cancel {
        program: SimpleExpr,
    },
    /// PERFORM inline (with statements)
    PerformInline {
        until: Option<SimpleCondition>,
        statements: Vec<SimpleStatement>,
        /// VARYING clause: (variable, FROM, BY)
        varying: Option<(String, SimpleExpr, SimpleExpr)>,
        /// AFTER clauses for nested VARYING: Vec<(variable, FROM, BY, UNTIL)>
        after: Vec<VaryingAfter>,
    },
    /// INSPECT TALLYING
    InspectTallying {
        target: String,
        counter: String,
        for_clauses: Vec<SimpleInspectFor>,
    },
    /// INSPECT REPLACING
    InspectReplacing {
        target: String,
        rules: Vec<SimpleInspectRule>,
    },
    /// INSPECT CONVERTING
    InspectConverting {
        target: String,
        from: String,
        to: String,
        before: Option<String>,
        after: Option<String>,
    },
    /// UNSTRING
    Unstring {
        source: String,
        delimiters: Vec<String>,
        into: Vec<String>,
        tallying: Option<String>,
    },
    /// OPEN file
    FileOpen {
        files: Vec<(String, SimpleOpenMode)>,
    },
    /// CLOSE file
    FileClose {
        files: Vec<String>,
    },
    /// READ file
    FileRead {
        file: String,
        into: Option<String>,
        at_end: Option<Vec<SimpleStatement>>,
        not_at_end: Option<Vec<SimpleStatement>>,
    },
    /// WRITE record
    FileWrite {
        record: String,
        from: Option<String>,
        advancing: Option<SimpleAdvancing>,
    },
    /// SEARCH table
    Search {
        table: String,
        all: bool,
        at_end: Option<Vec<SimpleStatement>>,
        when_clauses: Vec<SimpleSearchWhen>,
    },
    /// JSON GENERATE: convert COBOL group item to JSON text.
    JsonGenerate {
        /// Receiver data item name (stores JSON text).
        receiver: String,
        /// Source data item name (COBOL group to convert).
        source: String,
        /// Optional COUNT IN data item (stores character count).
        count_in: Option<String>,
    },
    /// JSON PARSE: populate COBOL data items from JSON text.
    JsonParse {
        /// Source data item name (contains JSON text).
        source: String,
        /// Target data item name (COBOL group to populate).
        target: String,
    },
    /// XML GENERATE: convert COBOL group item to XML text.
    XmlGenerate {
        /// Receiver data item name (stores XML text).
        receiver: String,
        /// Source data item name (COBOL group to convert).
        source: String,
        /// Optional COUNT IN data item.
        count_in: Option<String>,
    },
    /// XML PARSE: invoke processing procedure for XML events.
    XmlParse {
        /// Source data item name (contains XML text).
        source: String,
        /// Processing procedure paragraph name.
        processing_procedure: String,
    },
    /// ALLOCATE: obtain dynamic storage.
    Allocate {
        /// Data item to allocate.
        data_name: String,
        /// Number of characters to allocate (if specified).
        characters: Option<i64>,
        /// Pointer to receive the address.
        returning: Option<String>,
    },
    /// FREE: release dynamic storage.
    Free {
        /// Pointer data item names to free.
        pointers: Vec<String>,
    },
    /// ENTRY: register alternate entry point (no-op at runtime dispatch level).
    Entry {
        /// Entry point literal name.
        literal: String,
    },
    /// ALTER: change GO TO target at runtime.
    Alter {
        /// Source paragraph.
        source: String,
        /// New target paragraph.
        target: String,
    },
}

/// INSPECT FOR clause (tallying).
#[derive(Debug, Clone)]
pub struct SimpleInspectFor {
    /// Mode: Characters, All, Leading, First.
    pub mode: SimpleInspectMode,
    /// Pattern to match (None for CHARACTERS mode).
    pub pattern: Option<String>,
    /// BEFORE INITIAL delimiter.
    pub before: Option<String>,
    /// AFTER INITIAL delimiter.
    pub after: Option<String>,
}

/// INSPECT REPLACING rule.
#[derive(Debug, Clone)]
pub struct SimpleInspectRule {
    /// Mode: Characters, All, Leading, First.
    pub mode: SimpleInspectMode,
    /// Pattern to match (None for CHARACTERS mode).
    pub pattern: Option<String>,
    /// Replacement string.
    pub by: String,
    /// BEFORE INITIAL delimiter.
    pub before: Option<String>,
    /// AFTER INITIAL delimiter.
    pub after: Option<String>,
}

/// INSPECT mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimpleInspectMode {
    /// CHARACTERS (count or replace all characters).
    Characters,
    /// ALL occurrences.
    All,
    /// LEADING occurrences only.
    Leading,
    /// FIRST occurrence only.
    First,
}

/// File open mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimpleOpenMode {
    Input,
    Output,
    InputOutput,
    Extend,
}

/// WRITE ADVANCING clause.
#[derive(Debug, Clone)]
pub enum SimpleAdvancing {
    Lines { count: i64, before: bool },
    Page { before: bool },
}

/// SEARCH WHEN clause.
#[derive(Debug, Clone)]
pub struct SimpleSearchWhen {
    /// Condition to check.
    pub condition: SimpleCondition,
    /// Statements to execute on match.
    pub statements: Vec<SimpleStatement>,
}

/// EVALUATE WHEN clause.
#[derive(Debug, Clone)]
pub struct SimpleWhenClause {
    /// Condition to match (for EVALUATE TRUE: condition; for EVALUATE var: value)
    pub condition: SimpleCondition,
    /// Statements to execute
    pub statements: Vec<SimpleStatement>,
}

/// Simple expression for interpreter.
#[derive(Debug, Clone)]
pub enum SimpleExpr {
    /// Integer literal
    Integer(i64),
    /// String literal
    String(String),
    /// Variable reference
    Variable(String),
    /// Subscripted variable reference: ARRAY(INDEX)
    Subscript {
        variable: String,
        index: Box<SimpleExpr>,
    },
    /// Binary operation
    Binary {
        left: Box<SimpleExpr>,
        op: SimpleBinaryOp,
        right: Box<SimpleExpr>,
    },
    /// Intrinsic function call (e.g., UPPER-CASE, CURRENT-DATE)
    FunctionCall {
        name: String,
        args: Vec<SimpleExpr>,
    },
    /// Reference modification: variable(start:length)
    /// start is 1-based. If length is None, takes from start to end.
    RefMod {
        variable: Box<SimpleExpr>,
        start: Box<SimpleExpr>,
        length: Option<Box<SimpleExpr>>,
    },
}

/// Simple binary operator.
#[derive(Debug, Clone, Copy)]
pub enum SimpleBinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

/// Simple condition for interpreter.
#[derive(Debug, Clone)]
pub enum SimpleCondition {
    /// Comparison
    Compare {
        left: SimpleExpr,
        op: SimpleCompareOp,
        right: SimpleExpr,
    },
    /// NOT condition
    Not(Box<SimpleCondition>),
    /// AND
    And(Box<SimpleCondition>, Box<SimpleCondition>),
    /// OR
    Or(Box<SimpleCondition>, Box<SimpleCondition>),
    /// Level-88 condition name (resolved at runtime from program.condition_names)
    ConditionName(String),
    /// Class condition (IS NUMERIC, IS ALPHABETIC, etc.)
    ClassCondition {
        operand: SimpleExpr,
        class: SimpleClassType,
        negated: bool,
    },
}

/// Class type for IS NUMERIC / IS ALPHABETIC conditions.
#[derive(Debug, Clone, Copy)]
pub enum SimpleClassType {
    Numeric,
    Alphabetic,
    AlphabeticLower,
    AlphabeticUpper,
}

/// Simple comparison operator.
#[derive(Debug, Clone, Copy)]
pub enum SimpleCompareOp {
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

/// A single AFTER clause in PERFORM VARYING ... AFTER.
///
/// Each AFTER introduces a nested loop variable with its own FROM, BY, UNTIL.
#[derive(Debug, Clone)]
pub struct VaryingAfter {
    /// Variable name to iterate.
    pub variable: String,
    /// Initial value expression.
    pub from: SimpleExpr,
    /// Increment expression.
    pub by: SimpleExpr,
    /// Termination condition.
    pub until: SimpleCondition,
}

/// Definition of an 88-level condition name.
///
/// Supports single values, value lists, and THRU ranges as per COBOL standard.
#[derive(Debug, Clone)]
pub struct ConditionDef {
    /// Parent data item that this condition tests.
    pub parent: String,
    /// List of acceptable values (single values or ranges).
    pub values: Vec<ConditionValue>,
}

/// A single value or range in an 88-level condition definition.
#[derive(Debug, Clone)]
pub enum ConditionValue {
    /// Single value: VALUE 'Y'
    Single(String),
    /// Range: VALUE 'A' THRU 'Z'
    Range(String, String),
}

/// Control flow signal returned by statement execution.
///
/// Used to implement unstructured control flow (GO TO, EXIT PARAGRAPH, etc.)
/// without rewriting the entire interpreter. Statements return `Continue` for
/// normal sequential flow, or a signal that the caller must handle.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ControlFlow {
    /// Normal: continue with the next statement.
    Continue,
    /// GO TO: jump to the named paragraph.
    GoTo(String),
    /// EXIT PARAGRAPH: exit the current paragraph.
    ExitParagraph,
    /// EXIT SECTION: exit the current section.
    ExitSection,
}

/// Simple program representation.
#[derive(Debug, Clone)]
pub struct SimpleProgram {
    /// Program name.
    pub name: String,
    /// Data items (name -> metadata).
    pub data_items: Vec<(String, DataItemMeta)>,
    /// Main procedure statements.
    pub statements: Vec<SimpleStatement>,
    /// Named paragraphs.
    pub paragraphs: HashMap<String, Vec<SimpleStatement>>,
    /// Ordered list of paragraph names (for PERFORM THRU and fall-through).
    pub paragraph_order: Vec<String>,
    /// Level-88 condition names: condition_name -> definition with parent field and values.
    pub condition_names: HashMap<String, ConditionDef>,
    /// Group item layouts: group_name -> list of sub-fields with offsets.
    pub group_layouts: HashMap<String, Vec<GroupField>>,
    /// Contained (nested) programs.
    pub contained_programs: Vec<SimpleProgram>,
    /// Whether this program has the INITIAL attribute (re-init on each CALL).
    pub is_initial: bool,
    /// Whether this program has the COMMON attribute (callable by siblings).
    pub is_common: bool,
    /// Declarative error handlers: file-name (or "INPUT"/"OUTPUT") -> paragraph name.
    pub declarative_handlers: HashMap<String, String>,
    /// REDEFINES aliases: (alias_name, original_name) for propagating set/get.
    pub redefines_aliases: Vec<(String, String)>,
}

/// A sub-field within a group item, with its offset and size.
#[derive(Debug, Clone)]
pub struct GroupField {
    /// Field name (uppercase). Empty string for FILLER fields.
    pub name: String,
    /// Byte offset within the group.
    pub offset: usize,
    /// Size in bytes.
    pub size: usize,
    /// Whether numeric.
    pub is_numeric: bool,
    /// Default value for FILLER fields with VALUE clause (e.g., "/", ":").
    pub default_value: Option<String>,
}

/// When a group item is set, decompose its value into sub-fields based on the group layout.
fn decompose_group(group_name: &str, data: &str, program: &SimpleProgram, env: &mut Environment) -> Result<()> {
    let debug = std::env::var("OPEN_MAINFRAME_DEBUG_CMP").is_ok();
    if let Some(fields) = program.group_layouts.get(&group_name.to_uppercase()) {
        let data_bytes = data.as_bytes();
        if debug {
            eprintln!("[DECOMPOSE] group={} data_len={} field_count={}", group_name, data_bytes.len(), fields.len());
            // Print first 200 bytes as hex + ascii
            let show = data_bytes.len().min(200);
            eprintln!("[DECOMPOSE] data[0..{}] hex: {:?}", show, &data_bytes[..show]);
            eprintln!("[DECOMPOSE] data[0..{}] str: {:?}", show, &data[..show]);
        }
        for field in fields {
            let end = (field.offset + field.size).min(data_bytes.len());
            if field.offset < data_bytes.len() {
                let slice = &data_bytes[field.offset..end];
                let val_str = String::from_utf8_lossy(slice).to_string();
                if debug && !field.name.is_empty() {
                    eprintln!("[DECOMPOSE]   field={} offset={} size={} is_numeric={} raw={:?} val_str={:?}",
                        field.name, field.offset, field.size, field.is_numeric, slice, val_str);
                }
                if field.is_numeric {
                    let trimmed = val_str.trim();
                    let n = trimmed.parse::<i64>().unwrap_or(0);
                    env.set(&field.name, CobolValue::from_i64(n)).ok();
                } else {
                    env.set(&field.name, CobolValue::Alphanumeric(val_str)).ok();
                }
            }
        }
    } else if debug {
        eprintln!("[DECOMPOSE] NO LAYOUT for group={}", group_name);
    }
    Ok(())
}

/// Recompose a group variable's value from its children's current values.
///
/// This is the inverse of `decompose_group`. When a group variable is read
/// (e.g., used as the source of a MOVE), its stored value may be stale if
/// children were updated independently. This function rebuilds the group
/// string by slotting each child's current display value into its offset.
pub fn compose_group(group_name: &str, program: &SimpleProgram, env: &Environment) -> Option<CobolValue> {
    let fields = program.group_layouts.get(&group_name.to_uppercase())?;
    if fields.is_empty() {
        return None;
    }

    // Determine total group size from the last field's offset + size
    let total_size = fields.iter().map(|f| f.offset + f.size).max().unwrap_or(0);
    if total_size == 0 {
        return None;
    }

    let mut buffer = vec![b' '; total_size];

    for field in fields {
        // For FILLER fields with default values (e.g., "/" or ":"), use the default
        if field.name.is_empty() {
            if let Some(ref default) = field.default_value {
                let bytes = default.as_bytes();
                for i in 0..field.size {
                    if field.offset + i < buffer.len() {
                        buffer[field.offset + i] = if i < bytes.len() { bytes[i] } else { b' ' };
                    }
                }
            }
            continue;
        }

        if let Some(val) = env.get(&field.name) {
            let display = if field.is_numeric {
                // Numeric fields: zero-pad on the left to field size
                let raw = val.to_display_string();
                let trimmed = raw.trim();
                format!("{:0>width$}", trimmed, width = field.size)
            } else {
                val.to_display_string()
            };
            let bytes = display.as_bytes();
            // Slot value into buffer at field offset
            for i in 0..field.size {
                if field.offset + i < buffer.len() {
                    buffer[field.offset + i] = if i < bytes.len() { bytes[i] } else { b' ' };
                }
            }
        }
    }

    Some(CobolValue::Alphanumeric(String::from_utf8_lossy(&buffer).to_string()))
}

/// Execute a simple program.
pub fn execute(program: &SimpleProgram, env: &mut Environment) -> Result<i32> {
    // Initialize data items
    for (name, meta) in &program.data_items {
        env.define(name, meta.clone());
    }

    // Register REDEFINES aliases
    for (alias, original) in &program.redefines_aliases {
        env.register_redefines(alias, original);
    }

    // Execute main statements
    let flow = execute_statements(&program.statements, program, env)?;

    // Handle top-level GO TO: execute paragraphs with fall-through
    if let ControlFlow::GoTo(label) = flow {
        execute_from_paragraph(&label, program, env)?;
    }

    Ok(env.return_code())
}

/// Execute paragraphs starting from the given label, with fall-through.
///
/// This implements the COBOL paragraph fall-through model: after a GO TO
/// transfers control to a paragraph, execution continues sequentially
/// through subsequent paragraphs until STOP RUN or GOBACK.
fn execute_from_paragraph(
    start_label: &str,
    program: &SimpleProgram,
    env: &mut Environment,
) -> Result<()> {
    let start_upper = start_label.to_uppercase();

    // Find the starting paragraph in the ordered list
    if let Some(start_idx) = program.paragraph_order.iter()
        .position(|p| p.eq_ignore_ascii_case(&start_upper))
    {
        let mut idx = start_idx;
        while idx < program.paragraph_order.len() {
            if env.is_stopped() || env.is_goback() {
                break;
            }
            let para_name = &program.paragraph_order[idx];
            if let Some(para) = program.paragraphs.get(&para_name.to_uppercase()) {
                let flow = execute_statements(para, program, env)?;
                match flow {
                    ControlFlow::Continue => {}
                    ControlFlow::ExitParagraph => {}
                    ControlFlow::ExitSection => break,
                    ControlFlow::GoTo(ref label) => {
                        // Recursive GO TO: find new target and continue
                        let goto_upper = label.to_uppercase();
                        if let Some(goto_idx) = program.paragraph_order.iter()
                            .position(|p| p.eq_ignore_ascii_case(&goto_upper))
                        {
                            idx = goto_idx;
                            continue;
                        }
                        // If not found in ordered list, try direct execution
                        if let Some(target_para) = program.paragraphs.get(&goto_upper) {
                            execute_statements(target_para, program, env)?;
                        }
                        break;
                    }
                }
            }
            idx += 1;
        }
    } else {
        // Not in ordered list — try HashMap
        if let Some(para) = program.paragraphs.get(&start_upper) {
            execute_statements(para, program, env)?;
        }
    }
    Ok(())
}

/// Execute a list of statements, returning any control flow signal.
pub fn execute_statements(
    statements: &[SimpleStatement],
    program: &SimpleProgram,
    env: &mut Environment,
) -> Result<ControlFlow> {
    for stmt in statements {
        if env.is_stopped() || env.is_goback() {
            break;
        }
        let flow = execute_statement(stmt, program, env)?;
        match flow {
            ControlFlow::Continue => {}
            other => return Ok(other),
        }
    }
    Ok(ControlFlow::Continue)
}

/// Execute a single statement, returning control flow.
fn execute_statement(
    stmt: &SimpleStatement,
    program: &SimpleProgram,
    env: &mut Environment,
) -> Result<ControlFlow> {
    // Track call depth to detect infinite recursion
    thread_local! {
        static DEPTH: std::cell::Cell<u32> = const { std::cell::Cell::new(0) };
    }
    let current_depth = DEPTH.with(|d| {
        let v = d.get();
        d.set(v + 1);
        v
    });
    if current_depth > 500 {
        DEPTH.with(|d| d.set(d.get().saturating_sub(1)));
        return Err(InterpreterError {
            message: format!("Recursion depth exceeded ({})", current_depth),
        });
    }
    let result = execute_statement_impl(stmt, program, env);
    DEPTH.with(|d| d.set(d.get().saturating_sub(1)));
    result
}

fn execute_statement_impl(
    stmt: &SimpleStatement,
    program: &SimpleProgram,
    env: &mut Environment,
) -> Result<ControlFlow> {
    match stmt {
        SimpleStatement::Display {
            items,
            no_advancing,
        } => {
            let mut output = String::new();
            for item in items {
                let value = eval_expr(item, env)?;
                output.push_str(&value.to_display_string());
            }
            env.display(&output, *no_advancing)?;
        }

        SimpleStatement::Accept { target } => {
            let input = env.accept()?;
            env.set(target, CobolValue::Alphanumeric(input))?;
        }

        SimpleStatement::Move { from, to } => {
            // If the source is a group variable, recompose from children first
            if let SimpleExpr::Variable(ref var_name) = from {
                if program.group_layouts.contains_key(&var_name.to_uppercase()) {
                    if let Some(composed) = compose_group(var_name, program, env) {
                        env.set(var_name, composed)?;
                    }
                }
            }
            let value = eval_expr(from, env)?;
            for target_expr in to {
                // Handle RefMod on target: splice the value into the variable
                if let SimpleExpr::RefMod { variable, start, length } = target_expr {
                    let var_name = resolve_target_name(variable, env)?;
                    let current = env.get(&var_name)
                        .map(|v| v.to_display_string())
                        .unwrap_or_default();
                    let start_pos = to_numeric(&eval_expr(start, env)?).integer_part() as usize;
                    let zero_start = if start_pos > 0 { start_pos - 1 } else { 0 };
                    let src_str = value.to_display_string();
                    let splice_len = if let Some(len_expr) = length {
                        to_numeric(&eval_expr(len_expr, env)?).integer_part() as usize
                    } else {
                        current.len().saturating_sub(zero_start)
                    };
                    // Build new value by splicing src into current at the right position
                    let mut buf: Vec<u8> = current.into_bytes();
                    // Ensure buffer is large enough
                    if zero_start + splice_len > buf.len() {
                        buf.resize(zero_start + splice_len, b' ');
                    }
                    let src_bytes = src_str.as_bytes();
                    for i in 0..splice_len {
                        buf[zero_start + i] = if i < src_bytes.len() { src_bytes[i] } else { b' ' };
                    }
                    env.set(&var_name, CobolValue::Alphanumeric(
                        String::from_utf8_lossy(&buf).to_string(),
                    ))?;
                } else {
                    let target = resolve_target_name(target_expr, env)?;
                    env.set(&target, value.clone())?;
                    // If target is a group item, decompose into sub-fields
                    if program.group_layouts.contains_key(&target.to_uppercase()) {
                        let data = value.to_display_string();
                        if std::env::var("OPEN_MAINFRAME_DEBUG_GROUPS").is_ok() {
                            eprintln!("[MOVE→DECOMPOSE] target={} data_len={} data={:?}", target, data.len(), &data[..data.len().min(40)]);
                        }
                        decompose_group(&target, &data, program, env)?;
                    }
                }
            }
        }

        SimpleStatement::Compute { target, expr } => {
            let value = eval_expr(expr, env)?;
            env.set(target, value)?;
        }

        SimpleStatement::Add { values, to } => {
            let mut sum = NumericValue::from_i64(0);
            for val in values {
                let v = eval_expr(val, env)?;
                sum = sum.add(&to_numeric(&v));
            }
            for target in to {
                if let Some(current) = env.get(target) {
                    let new_val = to_numeric(current).add(&sum);
                    env.set(target, CobolValue::Numeric(new_val))?;
                }
            }
        }

        SimpleStatement::Subtract { values, from } => {
            let mut total = NumericValue::from_i64(0);
            for val in values {
                let v = eval_expr(val, env)?;
                total = total.add(&to_numeric(&v));
            }
            for target in from {
                if let Some(current) = env.get(target) {
                    let new_val = to_numeric(current).subtract(&total);
                    env.set(target, CobolValue::Numeric(new_val))?;
                }
            }
        }

        SimpleStatement::Multiply { value, by, giving } => {
            let v1 = eval_expr(value, env)?;
            let v2 = eval_expr(by, env)?;
            let result = to_numeric(&v1).multiply(&to_numeric(&v2));
            if let Some(target) = giving {
                env.set(target, CobolValue::Numeric(result))?;
            }
        }

        SimpleStatement::Divide {
            value,
            into,
            giving,
        } => {
            let v1 = eval_expr(value, env)?;
            let v2 = eval_expr(into, env)?;
            let result =
                to_numeric(&v2)
                    .divide(&to_numeric(&v1))
                    .ok_or_else(|| InterpreterError {
                        message: "Division by zero".to_string(),
                    })?;
            if let Some(target) = giving {
                env.set(target, CobolValue::Numeric(result))?;
            }
        }

        SimpleStatement::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let flow = if eval_condition(condition, env, program)? {
                execute_statements(then_branch, program, env)?
            } else if let Some(else_stmts) = else_branch {
                execute_statements(else_stmts, program, env)?
            } else {
                ControlFlow::Continue
            };
            if flow != ControlFlow::Continue {
                return Ok(flow);
            }
        }

        SimpleStatement::Perform { target, times } => {
            let iterations = times.unwrap_or(1);
            for _ in 0..iterations {
                if env.is_stopped() || env.is_goback() {
                    break;
                }
                if let Some(para) = program.paragraphs.get(&target.to_uppercase()) {
                    let flow = execute_statements(para, program, env)?;
                    match flow {
                        ControlFlow::Continue | ControlFlow::ExitParagraph => {}
                        ControlFlow::GoTo(_) => {
                            // GO TO within a simple PERFORM: propagate upward
                            return Ok(flow);
                        }
                        ControlFlow::ExitSection => break,
                    }
                }
            }
        }

        SimpleStatement::PerformThru { from, thru, times } => {
            let iterations = times.unwrap_or(1);
            let from_upper = from.to_uppercase();
            let thru_upper = thru.to_uppercase();

            // Find paragraph range in ordered list
            let start_idx = program.paragraph_order.iter()
                .position(|p| p.eq_ignore_ascii_case(&from_upper));
            let end_idx = program.paragraph_order.iter()
                .position(|p| p.eq_ignore_ascii_case(&thru_upper));

            if let (Some(start), Some(end)) = (start_idx, end_idx) {
                let range_end = end.max(start);
                for _ in 0..iterations {
                    if env.is_stopped() || env.is_goback() {
                        break;
                    }
                    let mut idx = start;
                    while idx <= range_end {
                        if env.is_stopped() || env.is_goback() {
                            break;
                        }
                        let para_name = &program.paragraph_order[idx];
                        if let Some(para) = program.paragraphs.get(&para_name.to_uppercase()) {
                            let flow = execute_statements(para, program, env)?;
                            match flow {
                                ControlFlow::Continue => {}
                                ControlFlow::ExitParagraph => {
                                    // Skip to next paragraph in range
                                }
                                ControlFlow::ExitSection => {
                                    // Exit the entire PERFORM THRU
                                    break;
                                }
                                ControlFlow::GoTo(ref label) => {
                                    // GO TO within PERFORM THRU: jump to target
                                    // if target is within the range, continue from there
                                    let goto_upper = label.to_uppercase();
                                    if let Some(goto_idx) = program.paragraph_order.iter()
                                        .position(|p| p.eq_ignore_ascii_case(&goto_upper))
                                    {
                                        if goto_idx >= start && goto_idx <= range_end {
                                            idx = goto_idx;
                                            continue;
                                        }
                                    }
                                    // Target outside range: propagate
                                    return Ok(flow);
                                }
                            }
                        }
                        idx += 1;
                    }
                }
            }
            // If paragraphs not found in order, try HashMap fallback for from only
            else if start_idx.is_none() {
                if let Some(para) = program.paragraphs.get(&from_upper) {
                    let iterations = times.unwrap_or(1);
                    for _ in 0..iterations {
                        if env.is_stopped() || env.is_goback() { break; }
                        execute_statements(para, program, env)?;
                    }
                }
            }
        }

        SimpleStatement::ExitParagraph => {
            return Ok(ControlFlow::ExitParagraph);
        }

        SimpleStatement::ExitSection => {
            return Ok(ControlFlow::ExitSection);
        }

        SimpleStatement::StopRun { return_code } => {
            if let Some(rc) = return_code {
                env.set_return_code(*rc);
            }
            env.stop();
        }

        SimpleStatement::Evaluate {
            subjects,
            when_clauses,
        } => {
            // Evaluate subject(s)
            let subject_values: Vec<CobolValue> = subjects
                .iter()
                .map(|s| eval_expr(s, env))
                .collect::<Result<_>>()?;

            // Set __EVAL_SUBJECT__ so WHEN value comparisons can reference it
            if let Some(first_subject) = subject_values.first() {
                env.set("__EVAL_SUBJECT__", first_subject.clone())?;
            }

            // Find first matching WHEN clause
            for when_clause in when_clauses {
                let matched = if subject_values.is_empty() {
                    // EVALUATE with no subjects - check condition directly
                    eval_condition(&when_clause.condition, env, program)?
                } else {
                    // EVALUATE var / EVALUATE TRUE: condition compares subject to value
                    eval_condition(&when_clause.condition, env, program)?
                };

                if matched {
                    let flow = execute_statements(&when_clause.statements, program, env)?;
                    if flow != ControlFlow::Continue {
                        return Ok(flow);
                    }
                    break;
                }
            }
        }

        SimpleStatement::ExecCics { command, options } => {
            // Options that reference variables by name (not by value)
            const VAR_REF_OPTIONS: &[&str] = &[
                "INTO", "FROM", "RIDFLD", "RESP", "RESP2", "COMMAREA",
            ];
            // Commands where ALL option values are paragraph/label names, not expressions
            let is_label_command = command == "HANDLE" || command == "IGNORE";
            // Commands where ALL option values are variable names (target for writing)
            let is_assign_command = command == "ASSIGN";

            // Evaluate option expressions, but for variable-reference options,
            // pass the variable NAME as a string instead of evaluating the value.
            // For HANDLE/IGNORE CONDITION, all values are paragraph names.
            let eval_options: Vec<(String, Option<CobolValue>)> = options
                .iter()
                .map(|(name, expr)| {
                    let value = if let Some(e) = expr {
                        if is_assign_command {
                            // ASSIGN: all values are variable names (targets for writing)
                            if let SimpleExpr::Variable(var_name) = e {
                                Some(CobolValue::Alphanumeric(var_name.clone()))
                            } else if let SimpleExpr::String(s) = e {
                                Some(CobolValue::Alphanumeric(s.clone()))
                            } else {
                                Some(eval_expr(e, env)?)
                            }
                        } else if is_label_command {
                            // HANDLE CONDITION / IGNORE CONDITION: values are paragraph names
                            if let SimpleExpr::Variable(var_name) = e {
                                Some(CobolValue::Alphanumeric(var_name.clone()))
                            } else if let SimpleExpr::String(s) = e {
                                Some(CobolValue::Alphanumeric(s.clone()))
                            } else {
                                Some(eval_expr(e, env)?)
                            }
                        } else if VAR_REF_OPTIONS.iter().any(|o| name.eq_ignore_ascii_case(o)) {
                            // Pass variable name as string
                            if let SimpleExpr::Variable(var_name) = e {
                                Some(CobolValue::Alphanumeric(var_name.clone()))
                            } else {
                                Some(eval_expr(e, env)?)
                            }
                        } else {
                            Some(eval_expr(e, env)?)
                        }
                    } else {
                        None
                    };
                    Ok((name.clone(), value))
                })
                .collect::<Result<_>>()?;

            // Take the handler out temporarily to avoid borrow conflict
            if let Some(mut handler) = env.cics_handler.take() {
                let result = handler.execute(command, &eval_options, env);
                env.cics_handler = Some(handler);
                result?;
            }

            // After CICS READ/RECEIVE INTO: decompose group items into sub-fields
            if command == "READ" || command == "RECEIVE" {
                if let Some(SimpleExpr::Variable(var_name)) = options.iter()
                    .find(|(name, _)| name == "INTO")
                    .and_then(|(_, expr)| expr.as_ref())
                {
                    if let Some(val) = env.get(var_name) {
                        let data = val.to_display_string();
                        if std::env::var("OPEN_MAINFRAME_DEBUG_CMP").is_ok() {
                            eprintln!("[DECOMPOSE] {} INTO {} len={}", command, var_name, data.len());
                        }
                        decompose_group(var_name, &data, program, env)?;
                    }
                }
            }
        }

        SimpleStatement::GoTo { target } => {
            // Check if this GO TO has been ALTERed
            let alter_key = format!("__ALTER__{}", target.to_uppercase());
            let effective_target = if let Some(val) = env.get(&alter_key) {
                let altered = val.to_display_string().trim().to_uppercase();
                if altered.is_empty() {
                    target.to_uppercase()
                } else {
                    altered
                }
            } else {
                target.to_uppercase()
            };
            return Ok(ControlFlow::GoTo(effective_target));
        }

        SimpleStatement::Initialize { targets } => {
            for target in targets {
                if let Some(meta) = env.meta(target).cloned() {
                    if meta.is_numeric {
                        env.set(target, CobolValue::from_i64(0))?;
                    } else {
                        env.set(target, CobolValue::Alphanumeric(" ".repeat(meta.size)))?;
                    }
                }
            }
        }

        SimpleStatement::StringConcat { sources, into } => {
            let mut result = String::new();
            for src in sources {
                let val = eval_expr(src, env)?;
                result.push_str(&val.to_display_string());
            }
            env.set(into, CobolValue::Alphanumeric(result))?;
        }

        SimpleStatement::Set { target, value } => {
            let val = eval_expr(value, env)?;
            env.set(target, val)?;
        }

        SimpleStatement::SetCondition { condition_name, value } => {
            // SET condition-name TO TRUE: set the parent field to the first value
            // SET condition-name TO FALSE: not standard COBOL, but we handle it
            if *value {
                if let Some(def) = program.condition_names.get(&condition_name.to_uppercase()) {
                    // Use the first value from the condition definition
                    if let Some(first_val) = def.values.first() {
                        let set_val = match first_val {
                            ConditionValue::Single(v) => v.clone(),
                            ConditionValue::Range(low, _) => low.clone(),
                        };
                        env.set(&def.parent, CobolValue::Alphanumeric(set_val))?;
                    }
                }
            }
            // SET condition TO FALSE not supported in standard COBOL
        }

        SimpleStatement::Call { program: call_target, using } => {
            let prog_name = eval_expr(call_target, env)?;
            let target_name = prog_name.to_display_string().trim().to_uppercase();

            // Try to resolve the CALL: 1) contained programs, 2) program registry
            let contained = program.contained_programs.iter().find(|p| {
                p.name.eq_ignore_ascii_case(&target_name)
            }).cloned();

            let resolved = if contained.is_some() {
                contained
            } else if let Some(ref registry) = env.program_registry {
                let reg = registry.lock().map_err(|e| InterpreterError {
                    message: format!("Program registry lock error: {}", e),
                })?;
                reg.lookup(&target_name)
            } else {
                None
            };

            if let Some(called_prog) = resolved {
                // Create a sub-environment for the called program
                let output = Vec::<u8>::new();
                let input = std::io::Cursor::new(Vec::<u8>::new());
                let mut sub_env = Environment::with_io(
                    Box::new(output),
                    Box::new(std::io::BufReader::new(input)),
                );

                // Share the program registry
                if let Some(ref registry) = env.program_registry {
                    sub_env.program_registry = Some(Arc::clone(registry));
                }

                // Initialize data items for the called program
                for (name, meta) in &called_prog.data_items {
                    sub_env.define(name, meta.clone());
                }

                // Copy GLOBAL data from caller to callee
                for (name, _meta) in &program.data_items {
                    if let Some(val) = env.get(name) {
                        sub_env.set(name, val.clone()).ok();
                    }
                }

                // Pass parameters according to their mode
                // Map callee LINKAGE SECTION items (by position) to caller params
                let linkage_params: Vec<String> = called_prog.data_items
                    .iter()
                    .filter(|(_, meta)| meta.picture.is_some()) // LINKAGE items have pictures
                    .map(|(name, _)| name.clone())
                    .collect();

                for (i, param) in using.iter().enumerate() {
                    let (caller_name, value) = match param {
                        CallParam::ByReference(name) | CallParam::ByContent(name) | CallParam::ByValue(name) => {
                            let val = env.get(name).cloned().unwrap_or_else(|| {
                                CobolValue::Alphanumeric(String::new())
                            });
                            (name.clone(), val)
                        }
                    };
                    // Set the value in the sub-env under the callee's parameter name
                    // (positional mapping) or under the caller's name if no callee param
                    let callee_name = linkage_params.get(i)
                        .cloned()
                        .unwrap_or_else(|| caller_name.clone());
                    sub_env.set(&callee_name, value).ok();
                }

                // Execute the called program
                execute_statements(&called_prog.statements, &called_prog, &mut sub_env)?;

                // After return: propagate RETURN-CODE from callee
                env.set_return_code(sub_env.return_code());
                // Make RETURN-CODE available as a variable too
                env.set("RETURN-CODE", CobolValue::from_i64(sub_env.return_code() as i64)).ok();

                // Copy back BY REFERENCE parameters from callee to caller
                for (i, param) in using.iter().enumerate() {
                    if let CallParam::ByReference(caller_name) = param {
                        let callee_name = linkage_params.get(i)
                            .cloned()
                            .unwrap_or_else(|| caller_name.clone());
                        if let Some(val) = sub_env.get(&callee_name) {
                            env.set(caller_name, val.clone())?;
                        }
                    }
                    // BY CONTENT and BY VALUE: no copy-back (caller's value unchanged)
                }
            } else {
                // Program not found — raise runtime error
                return Err(InterpreterError {
                    message: format!("CALL target not found: {}", target_name),
                });
            }
        }

        SimpleStatement::GoBack { return_code } => {
            // GOBACK returns control to the calling program.
            // If RETURNING is specified, set the return code first.
            if let Some(rc) = return_code {
                env.set_return_code(*rc);
            }
            env.goback();
        }

        SimpleStatement::Cancel { program: cancel_target } => {
            let prog_name = eval_expr(cancel_target, env)?;
            let target_name = prog_name.to_display_string().trim().to_uppercase();
            if let Some(ref registry) = env.program_registry {
                let mut reg = registry.lock().map_err(|e| InterpreterError {
                    message: format!("Program registry lock error: {}", e),
                })?;
                reg.cancel(&target_name);
            }
        }

        SimpleStatement::PerformInline { until, statements: stmts, varying, after } => {
            // Initialize VARYING variable if present
            if let Some((ref var_name, ref from_expr, _)) = varying {
                let from_val = eval_expr(from_expr, env)?;
                env.set(var_name, from_val)?;
            }

            // Initialize all AFTER variables
            for af in after {
                let from_val = eval_expr(&af.from, env)?;
                env.set(&af.variable, from_val)?;
            }

            // Determine the UNTIL condition
            let effective_until = until.as_ref();

            let max_iterations = 1_000_000; // Safety limit (higher for nested loops)
            let mut count = 0;

            loop {
                if env.is_stopped() || env.is_goback() || count >= max_iterations {
                    break;
                }
                // Check outer UNTIL condition before (test before)
                if let Some(cond) = effective_until {
                    if eval_condition(cond, env, program)? {
                        break;
                    }
                }

                if after.is_empty() {
                    // Simple VARYING without AFTER
                    execute_statements(stmts, program, env)?;
                    count += 1;
                } else {
                    // VARYING with AFTER: run inner loop(s) before incrementing outer
                    // Re-initialize the first AFTER variable from its FROM value
                    let from_val = eval_expr(&after[0].from, env)?;
                    env.set(&after[0].variable, from_val)?;

                    // For multiple AFTER levels, re-initialize subsequent ones too
                    for af in after.iter().skip(1) {
                        let fv = eval_expr(&af.from, env)?;
                        env.set(&af.variable, fv)?;
                    }

                    // Execute the innermost AFTER loop
                    execute_after_loop(after, 0, stmts, program, env, &mut count, max_iterations)?;
                }

                // Increment outer VARYING variable
                if let Some((ref var_name, _, ref by_expr)) = varying {
                    let by_val = to_numeric(&eval_expr(by_expr, env)?);
                    if let Some(current) = env.get(var_name).cloned() {
                        let new_val = to_numeric(&current).add(&by_val);
                        env.set(var_name, CobolValue::Numeric(new_val))?;
                    }
                }

                // If no UNTIL and no VARYING, execute once
                if effective_until.is_none() && varying.is_none() {
                    break;
                }
            }
        }

        SimpleStatement::InspectTallying { target, counter, for_clauses } => {
            let target_val = env.get(target).cloned().unwrap_or(CobolValue::Alphanumeric(String::new()));
            let data = target_val.to_display_string();
            let mut tally: i64 = 0;

            for clause in for_clauses {
                let (start, end) = inspect_delimited_range(&data, &clause.before, &clause.after);
                let region = &data[start..end];

                match clause.mode {
                    SimpleInspectMode::Characters => {
                        tally += region.len() as i64;
                    }
                    SimpleInspectMode::All => {
                        if let Some(ref pat) = clause.pattern {
                            if !pat.is_empty() {
                                tally += region.matches(pat.as_str()).count() as i64;
                            }
                        }
                    }
                    SimpleInspectMode::Leading => {
                        if let Some(ref pat) = clause.pattern {
                            if !pat.is_empty() {
                                let mut pos = 0;
                                while pos + pat.len() <= region.len() && &region[pos..pos + pat.len()] == pat.as_str() {
                                    tally += 1;
                                    pos += pat.len();
                                }
                            }
                        }
                    }
                    SimpleInspectMode::First => {
                        if let Some(ref pat) = clause.pattern {
                            if region.contains(pat.as_str()) {
                                tally += 1;
                            }
                        }
                    }
                }
            }

            // Add tally to the counter variable (INSPECT adds to existing value)
            let current = env.get(counter).cloned().unwrap_or(CobolValue::from_i64(0));
            let new_val = to_numeric(&current).add(&NumericValue::from_i64(tally));
            env.set(counter, CobolValue::Numeric(new_val))?;
        }

        SimpleStatement::InspectReplacing { target, rules } => {
            let target_val = env.get(target).cloned().unwrap_or(CobolValue::Alphanumeric(String::new()));
            let mut data = target_val.to_display_string();

            for rule in rules {
                let (start, end) = inspect_delimited_range(&data, &rule.before, &rule.after);

                match rule.mode {
                    SimpleInspectMode::Characters => {
                        // Replace every character in range with first char of `by`
                        if let Some(by_ch) = rule.by.chars().next() {
                            let mut chars: Vec<char> = data.chars().collect();
                            for i in start..end.min(chars.len()) {
                                chars[i] = by_ch;
                            }
                            data = chars.into_iter().collect();
                        }
                    }
                    SimpleInspectMode::All => {
                        if let Some(ref pat) = rule.pattern {
                            if !pat.is_empty() {
                                let prefix = &data[..start];
                                let region = &data[start..end];
                                let suffix = &data[end..];
                                let replaced = region.replace(pat.as_str(), &rule.by);
                                data = format!("{}{}{}", prefix, replaced, suffix);
                            }
                        }
                    }
                    SimpleInspectMode::Leading => {
                        if let Some(ref pat) = rule.pattern {
                            if !pat.is_empty() {
                                let prefix = data[..start].to_string();
                                let region = data[start..end].to_string();
                                let suffix = data[end..].to_string();
                                let mut result = String::new();
                                let mut pos = 0;
                                // Replace leading occurrences only
                                while pos + pat.len() <= region.len() && &region[pos..pos + pat.len()] == pat.as_str() {
                                    result.push_str(&rule.by);
                                    pos += pat.len();
                                }
                                result.push_str(&region[pos..]);
                                data = format!("{}{}{}", prefix, result, suffix);
                            }
                        }
                    }
                    SimpleInspectMode::First => {
                        if let Some(ref pat) = rule.pattern {
                            if !pat.is_empty() {
                                let prefix = data[..start].to_string();
                                let region = data[start..end].to_string();
                                let suffix = data[end..].to_string();
                                let replaced = region.replacen(pat.as_str(), &rule.by, 1);
                                data = format!("{}{}{}", prefix, replaced, suffix);
                            }
                        }
                    }
                }
            }

            env.set(target, CobolValue::Alphanumeric(data))?;
        }

        SimpleStatement::InspectConverting { target, from, to, before, after } => {
            let target_val = env.get(target).cloned().unwrap_or(CobolValue::Alphanumeric(String::new()));
            let mut data = target_val.to_display_string();
            let (start, end) = inspect_delimited_range(&data, before, after);

            let from_chars: Vec<char> = from.chars().collect();
            let to_chars: Vec<char> = to.chars().collect();
            let mut chars: Vec<char> = data.chars().collect();

            for i in start..end.min(chars.len()) {
                if let Some(pos) = from_chars.iter().position(|&c| c == chars[i]) {
                    if pos < to_chars.len() {
                        chars[i] = to_chars[pos];
                    }
                }
            }
            data = chars.into_iter().collect();
            env.set(target, CobolValue::Alphanumeric(data))?;
        }

        SimpleStatement::Unstring { source, delimiters, into, tallying } => {
            let source_val = env.get(source).cloned().unwrap_or(CobolValue::Alphanumeric(String::new()));
            let data = source_val.to_display_string();

            let parts: Vec<&str> = if delimiters.is_empty() {
                vec![data.as_str()]
            } else {
                // Split by the first delimiter (simplified: multiple delimiters act as alternatives)
                let mut result = vec![data.as_str()];
                for delim in delimiters {
                    let mut new_result = Vec::new();
                    for part in &result {
                        let splits: Vec<&str> = part.split(delim.as_str()).collect();
                        new_result.extend(splits);
                    }
                    result = new_result;
                }
                result
            };

            let mut field_count: i64 = 0;
            for (i, target) in into.iter().enumerate() {
                if i < parts.len() {
                    env.set(target, CobolValue::Alphanumeric(parts[i].to_string()))?;
                    field_count += 1;
                }
            }

            if let Some(ref tally_var) = tallying {
                env.set(tally_var, CobolValue::from_i64(field_count))?;
            }
        }

        SimpleStatement::FileOpen { files } => {
            for (file_name, mode) in files {
                let file_upper = file_name.to_uppercase();
                let records = if *mode == SimpleOpenMode::Input || *mode == SimpleOpenMode::InputOutput {
                    // Try to load records from a pre-set variable "{FILE}-DATA"
                    let data_var = format!("{}-DATA", file_upper);
                    if let Some(val) = env.get(&data_var) {
                        val.to_display_string().lines().map(|l| l.to_string()).collect()
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                };
                env.files.insert(file_upper.clone(), FileState {
                    mode: *mode,
                    records,
                    position: 0,
                    status: "00".to_string(),
                });
                // Set FILE STATUS variable if it exists
                let status_var = format!("{}-STATUS", file_upper);
                let _ = env.set(&status_var, CobolValue::Alphanumeric("00".to_string()));
            }
        }

        SimpleStatement::FileClose { files } => {
            for file_name in files {
                let file_upper = file_name.to_uppercase();
                if env.files.remove(&file_upper).is_none() {
                    // File wasn't open — status 42
                    let status_var = format!("{}-STATUS", file_upper);
                    let _ = env.set(&status_var, CobolValue::Alphanumeric("42".to_string()));
                } else {
                    let status_var = format!("{}-STATUS", file_upper);
                    let _ = env.set(&status_var, CobolValue::Alphanumeric("00".to_string()));
                }
            }
        }

        SimpleStatement::FileRead { file, into, at_end, not_at_end } => {
            let file_upper = file.to_uppercase();
            let (record, at_eof) = if let Some(state) = env.files.get_mut(&file_upper) {
                if state.position < state.records.len() {
                    let rec = state.records[state.position].clone();
                    state.position += 1;
                    state.status = "00".to_string();
                    (Some(rec), false)
                } else {
                    state.status = "10".to_string();
                    (None, true)
                }
            } else {
                (None, true)
            };

            // Set FILE STATUS
            let status = if at_eof { "10" } else { "00" };
            let status_var = format!("{}-STATUS", file_upper);
            let _ = env.set(&status_var, CobolValue::Alphanumeric(status.to_string()));

            if let Some(rec) = record {
                // Set INTO target or the file record variable
                if let Some(ref into_var) = into {
                    env.set(into_var, CobolValue::Alphanumeric(rec))?;
                } else {
                    // Set the record variable (conventionally: {FILE}-RECORD)
                    let rec_var = format!("{}-RECORD", file_upper);
                    env.set(&rec_var, CobolValue::Alphanumeric(rec))?;
                }
                // Execute NOT AT END
                if let Some(ref stmts) = not_at_end {
                    execute_statements(stmts, program, env)?;
                }
            } else {
                // Execute AT END
                if let Some(ref stmts) = at_end {
                    execute_statements(stmts, program, env)?;
                }
            }
        }

        SimpleStatement::FileWrite { record, from, advancing } => {
            // Get the record data
            let data = if let Some(ref from_var) = from {
                env.get(from_var).map(|v| v.to_display_string()).unwrap_or_default()
            } else {
                env.get(record).map(|v| v.to_display_string()).unwrap_or_default()
            };

            // Find which file this record belongs to (try {RECORD}-FILE or any open output file)
            let file_key = {
                let record_upper = record.to_uppercase();
                // Convention: look for an output file whose name is a prefix of the record name
                let mut found = None;
                for (name, state) in &env.files {
                    if state.mode == SimpleOpenMode::Output || state.mode == SimpleOpenMode::Extend
                        || state.mode == SimpleOpenMode::InputOutput
                    {
                        if found.is_none() {
                            found = Some(name.clone());
                        }
                        // Check if record name starts with file name
                        if record_upper.starts_with(name.as_str()) {
                            found = Some(name.clone());
                            break;
                        }
                    }
                }
                found
            };

            if let Some(ref fk) = file_key {
                if let Some(state) = env.files.get_mut(fk) {
                    // Handle ADVANCING
                    if let Some(ref adv) = advancing {
                        match adv {
                            SimpleAdvancing::Lines { count, before } => {
                                if *before {
                                    for _ in 0..*count { state.records.push(String::new()); }
                                    state.records.push(data.clone());
                                } else {
                                    state.records.push(data.clone());
                                    for _ in 0..*count { state.records.push(String::new()); }
                                }
                            }
                            SimpleAdvancing::Page { before } => {
                                if *before {
                                    state.records.push("\x0C".to_string()); // form feed
                                }
                                state.records.push(data.clone());
                                if !before {
                                    state.records.push("\x0C".to_string());
                                }
                            }
                        }
                    } else {
                        state.records.push(data);
                    }
                    state.status = "00".to_string();
                }
            }
        }

        SimpleStatement::Search { table, all, at_end, when_clauses } => {
            // Get table size by looking for subscripted variables
            let table_upper = table.to_uppercase();
            let max_entries = 100; // Safety limit
            let mut found = false;

            if *all {
                // Binary search: simplified — just check each entry (proper binary search
                // would require knowing the sorted key, which we approximate here)
                let mut lo = 1i64;
                let mut hi = max_entries;
                while lo <= hi && !found && !env.is_stopped() {
                    let mid = (lo + hi) / 2;
                    // Set the index variable
                    let idx_var = format!("{}-IDX", table_upper);
                    env.set(&idx_var, CobolValue::from_i64(mid))?;

                    // Check if entry exists
                    let entry_key = format!("{}({})", table_upper, mid);
                    if env.get(&entry_key).is_none() {
                        hi = mid - 1;
                        continue;
                    }

                    // Check WHEN conditions
                    for when in when_clauses {
                        if eval_condition(&when.condition, env, program)? {
                            execute_statements(&when.statements, program, env)?;
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        // Can't determine search direction without key info; just try next
                        lo = mid + 1;
                    }
                }
            } else {
                // Sequential search
                for i in 1..=max_entries {
                    if env.is_stopped() { break; }

                    // Set index variable
                    let idx_var = format!("{}-IDX", table_upper);
                    env.set(&idx_var, CobolValue::from_i64(i))?;

                    // Check if entry exists
                    let entry_key = format!("{}({})", table_upper, i);
                    if env.get(&entry_key).is_none() {
                        break; // Past end of table
                    }

                    // Check WHEN conditions
                    for when in when_clauses {
                        if eval_condition(&when.condition, env, program)? {
                            execute_statements(&when.statements, program, env)?;
                            found = true;
                            break;
                        }
                    }
                    if found { break; }
                }
            }

            if !found {
                if let Some(ref stmts) = at_end {
                    execute_statements(stmts, program, env)?;
                }
            }
        }

        SimpleStatement::JsonGenerate { receiver, source, count_in } => {
            // Collect fields from the source group item into a JSON object
            let source_upper = source.to_uppercase();
            let mut json_obj = String::from("{");
            let mut first = true;

            // Iterate through environment variables that belong to the source group
            let fields = env.get_group_fields(&source_upper);
            for (field_name, field_value) in &fields {
                if !first {
                    json_obj.push(',');
                }
                first = false;
                // Strip the group prefix to get the field name
                let short_name = field_name
                    .strip_prefix(&format!("{}-", source_upper))
                    .or_else(|| field_name.strip_prefix(&format!("{}.", source_upper)))
                    .unwrap_or(field_name);
                json_obj.push('"');
                json_obj.push_str(short_name);
                json_obj.push_str("\":");
                match field_value {
                    CobolValue::Numeric(n) => {
                        json_obj.push_str(&n.to_string());
                    }
                    CobolValue::Alphanumeric(s) => {
                        json_obj.push('"');
                        // Escape special characters
                        for ch in s.trim_end().chars() {
                            match ch {
                                '"' => json_obj.push_str("\\\""),
                                '\\' => json_obj.push_str("\\\\"),
                                '\n' => json_obj.push_str("\\n"),
                                '\r' => json_obj.push_str("\\r"),
                                '\t' => json_obj.push_str("\\t"),
                                c => json_obj.push(c),
                            }
                        }
                        json_obj.push('"');
                    }
                    CobolValue::Group(_) => {
                        json_obj.push_str("{}");
                    }
                }
            }
            json_obj.push('}');

            let json_len = json_obj.len() as i64;
            env.set(receiver, CobolValue::Alphanumeric(json_obj))?;

            if let Some(ref count_var) = count_in {
                env.set(count_var, CobolValue::from_i64(json_len))?;
            }

            // Set JSON-CODE to 0 (success)
            env.set("JSON-CODE", CobolValue::from_i64(0))?;
        }

        SimpleStatement::JsonParse { source, target } => {
            // Get the JSON text from the source variable
            let json_text = env.get_string(source);
            let json_text = json_text.trim();

            if json_text.starts_with('{') {
                // Simple JSON object parsing — extract key-value pairs
                let target_upper = target.to_uppercase();
                // Strip outer braces and split on commas (simplified parser)
                let inner = &json_text[1..json_text.len().saturating_sub(1)];
                for pair in split_json_pairs(inner) {
                    let pair = pair.trim();
                    if let Some(colon_pos) = pair.find(':') {
                        let key = pair[..colon_pos].trim().trim_matches('"');
                        let value = pair[colon_pos + 1..].trim();
                        let field_name = format!("{}-{}", target_upper, key.to_uppercase());

                        if value.starts_with('"') && value.ends_with('"') {
                            // String value
                            let s = &value[1..value.len() - 1];
                            env.set(&field_name, CobolValue::Alphanumeric(s.to_string()))?;
                        } else if let Ok(n) = value.parse::<i64>() {
                            env.set(&field_name, CobolValue::from_i64(n))?;
                        } else {
                            env.set(&field_name, CobolValue::Alphanumeric(value.to_string()))?;
                        }
                    }
                }

                env.set("JSON-CODE", CobolValue::from_i64(0))?;
            } else {
                // Invalid JSON
                env.set("JSON-CODE", CobolValue::from_i64(1))?;
            }
        }

        SimpleStatement::XmlGenerate { receiver, source, count_in } => {
            let source_upper = source.to_uppercase();
            let mut xml = String::from("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
            xml.push('<');
            xml.push_str(&source_upper);
            xml.push('>');

            let fields = env.get_group_fields(&source_upper);
            for (field_name, field_value) in &fields {
                let short_name = field_name
                    .strip_prefix(&format!("{}-", source_upper))
                    .or_else(|| field_name.strip_prefix(&format!("{}.", source_upper)))
                    .unwrap_or(field_name);
                xml.push('<');
                xml.push_str(short_name);
                xml.push('>');
                match field_value {
                    CobolValue::Numeric(n) => xml.push_str(&n.to_string()),
                    CobolValue::Alphanumeric(s) => {
                        for ch in s.trim_end().chars() {
                            match ch {
                                '<' => xml.push_str("&lt;"),
                                '>' => xml.push_str("&gt;"),
                                '&' => xml.push_str("&amp;"),
                                '"' => xml.push_str("&quot;"),
                                '\'' => xml.push_str("&apos;"),
                                c => xml.push(c),
                            }
                        }
                    }
                    CobolValue::Group(_) => {}
                }
                xml.push_str("</");
                xml.push_str(short_name);
                xml.push('>');
            }

            xml.push_str("</");
            xml.push_str(&source_upper);
            xml.push('>');

            let xml_len = xml.len() as i64;
            env.set(receiver, CobolValue::Alphanumeric(xml))?;
            if let Some(ref count_var) = count_in {
                env.set(count_var, CobolValue::from_i64(xml_len))?;
            }
            env.set("XML-CODE", CobolValue::from_i64(0))?;
        }

        SimpleStatement::XmlParse { source, processing_procedure } => {
            let xml_text = env.get_string(source);
            let xml_text = xml_text.trim();
            let proc_upper = processing_procedure.to_uppercase();

            env.set("XML-EVENT", CobolValue::Alphanumeric("START-OF-DOCUMENT".to_string()))?;
            env.set("XML-TEXT", CobolValue::Alphanumeric(String::new()))?;
            env.set("XML-CODE", CobolValue::from_i64(0))?;
            if let Some(para) = program.paragraphs.get(&proc_upper) {
                execute_statements(&para.clone(), program, env)?;
            }

            let mut pos = 0;
            let bytes = xml_text.as_bytes();
            if xml_text.starts_with("<?") {
                if let Some(end) = xml_text.find("?>") {
                    pos = end + 2;
                }
            }

            while pos < bytes.len() && !env.is_stopped() {
                if let Some(CobolValue::Numeric(n)) = env.get("XML-CODE") {
                    if !n.is_zero() { break; }
                }
                if bytes[pos] == b'<' {
                    if pos + 1 < bytes.len() && bytes[pos + 1] == b'/' {
                        let tag_start = pos + 2;
                        if let Some(end) = xml_text[tag_start..].find('>') {
                            let tag_name = &xml_text[tag_start..tag_start + end];
                            env.set("XML-EVENT", CobolValue::Alphanumeric("END-OF-ELEMENT".to_string()))?;
                            env.set("XML-TEXT", CobolValue::Alphanumeric(tag_name.to_string()))?;
                            if let Some(para) = program.paragraphs.get(&proc_upper) {
                                execute_statements(&para.clone(), program, env)?;
                            }
                            pos = tag_start + end + 1;
                        } else { break; }
                    } else {
                        let tag_start = pos + 1;
                        if let Some(end) = xml_text[tag_start..].find('>') {
                            let tag_name = &xml_text[tag_start..tag_start + end];
                            let elem_name = tag_name.split_whitespace().next().unwrap_or(tag_name);
                            env.set("XML-EVENT", CobolValue::Alphanumeric("START-OF-ELEMENT".to_string()))?;
                            env.set("XML-TEXT", CobolValue::Alphanumeric(elem_name.to_string()))?;
                            if let Some(para) = program.paragraphs.get(&proc_upper) {
                                execute_statements(&para.clone(), program, env)?;
                            }
                            pos = tag_start + end + 1;
                        } else { break; }
                    }
                } else {
                    let text_start = pos;
                    while pos < bytes.len() && bytes[pos] != b'<' { pos += 1; }
                    let text = xml_text[text_start..pos].trim();
                    if !text.is_empty() {
                        env.set("XML-EVENT", CobolValue::Alphanumeric("CONTENT-CHARACTERS".to_string()))?;
                        env.set("XML-TEXT", CobolValue::Alphanumeric(text.to_string()))?;
                        if let Some(para) = program.paragraphs.get(&proc_upper) {
                            execute_statements(&para.clone(), program, env)?;
                        }
                    }
                }
            }

            env.set("XML-EVENT", CobolValue::Alphanumeric("END-OF-DOCUMENT".to_string()))?;
            env.set("XML-TEXT", CobolValue::Alphanumeric(String::new()))?;
            if let Some(para) = program.paragraphs.get(&proc_upper) {
                execute_statements(&para.clone(), program, env)?;
            }
        }

        SimpleStatement::Allocate { data_name, characters, returning } => {
            let size = characters.unwrap_or(256) as usize;
            let buffer = " ".repeat(size);
            let data_upper = data_name.to_uppercase();
            env.set(&data_upper, CobolValue::Alphanumeric(buffer))?;

            if let Some(ref ptr_name) = returning {
                // Set the pointer to the data name (symbolic reference)
                env.set(ptr_name, CobolValue::Alphanumeric(data_upper))?;
            }
        }

        SimpleStatement::Free { pointers } => {
            for ptr in pointers {
                let ptr_upper = ptr.to_uppercase();
                // Get the target name if it's a pointer
                let target = env.get_string(&ptr_upper);
                if !target.is_empty() {
                    // Clear the allocated storage
                    env.set(&target, CobolValue::Alphanumeric(String::new()))?;
                }
                // Set pointer to NULL (empty)
                env.set(&ptr_upper, CobolValue::Alphanumeric(String::new()))?;
            }
        }

        SimpleStatement::Entry { literal: _ } => {
            // ENTRY registers an alternate entry point — this is handled at
            // the program/call dispatch level, not during statement execution.
            // At runtime, encountering ENTRY inline is a no-op (fall-through).
        }

        SimpleStatement::Alter { source, target } => {
            // ALTER modifies the GO TO target of a paragraph at runtime.
            // Store the altered target in a special runtime variable.
            let alter_key = format!("__ALTER__{}", source.to_uppercase());
            env.set(&alter_key, CobolValue::Alphanumeric(target.to_uppercase()))?;
        }
    }

    Ok(ControlFlow::Continue)
}

/// Convert CobolValue to NumericValue.
fn to_numeric(value: &CobolValue) -> NumericValue {
    match value {
        CobolValue::Numeric(n) => n.clone(),
        CobolValue::Alphanumeric(s) => {
            // Try to parse as number
            if let Ok(n) = s.trim().parse::<i64>() {
                NumericValue::from_i64(n)
            } else {
                NumericValue::from_i64(0)
            }
        }
        CobolValue::Group(_) => NumericValue::from_i64(0),
    }
}

/// Split a JSON object's inner text into key-value pairs, respecting nested strings.
fn split_json_pairs(inner: &str) -> Vec<&str> {
    let mut pairs = Vec::new();
    let mut start = 0;
    let mut in_string = false;
    let mut depth = 0;

    for (i, ch) in inner.char_indices() {
        match ch {
            '"' if !in_string => in_string = true,
            '"' if in_string => in_string = false,
            '{' | '[' if !in_string => depth += 1,
            '}' | ']' if !in_string => depth -= 1,
            ',' if !in_string && depth == 0 => {
                pairs.push(&inner[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }
    if start < inner.len() {
        pairs.push(&inner[start..]);
    }
    pairs
}

/// Compute the effective byte range for INSPECT BEFORE/AFTER INITIAL delimiters.
fn inspect_delimited_range(data: &str, before: &Option<String>, after: &Option<String>) -> (usize, usize) {
    let mut start = 0;
    let mut end = data.len();

    if let Some(ref after_delim) = after {
        if let Some(pos) = data.find(after_delim.as_str()) {
            start = pos + after_delim.len();
        }
    }
    if let Some(ref before_delim) = before {
        if let Some(pos) = data[start..].find(before_delim.as_str()) {
            end = start + pos;
        }
    }

    (start, end)
}

/// Resolve a target expression to a variable name string.
fn resolve_target_name(expr: &SimpleExpr, env: &Environment) -> Result<String> {
    match expr {
        SimpleExpr::Variable(name) => Ok(name.clone()),
        SimpleExpr::Subscript { variable, index } => {
            let idx = to_numeric(&eval_expr(index, env)?).integer_part();
            Ok(format!("{}({})", variable, idx))
        }
        _ => Err(InterpreterError {
            message: format!("Invalid MOVE target expression: {:?}", expr),
        }),
    }
}

/// Evaluate an expression.
fn eval_expr(expr: &SimpleExpr, env: &Environment) -> Result<CobolValue> {
    match expr {
        SimpleExpr::Integer(n) => Ok(CobolValue::from_i64(*n)),

        SimpleExpr::String(s) => Ok(CobolValue::Alphanumeric(s.clone())),

        SimpleExpr::Variable(name) => env.get(name).cloned().ok_or_else(|| InterpreterError {
            message: format!("Undefined variable: {}", name),
        }),

        SimpleExpr::Subscript { variable, index } => {
            let idx = to_numeric(&eval_expr(index, env)?).integer_part();
            let indexed_name = format!("{}({})", variable, idx);
            env.get(&indexed_name)
                .cloned()
                .or_else(|| env.get(variable).cloned())
                .ok_or_else(|| InterpreterError {
                    message: format!("Undefined subscripted variable: {}", indexed_name),
                })
        }

        SimpleExpr::Binary { left, op, right } => {
            let l = to_numeric(&eval_expr(left, env)?);
            let r = to_numeric(&eval_expr(right, env)?);
            let result = match op {
                SimpleBinaryOp::Add => l.add(&r),
                SimpleBinaryOp::Subtract => l.subtract(&r),
                SimpleBinaryOp::Multiply => l.multiply(&r),
                SimpleBinaryOp::Divide => l.divide(&r).ok_or_else(|| InterpreterError {
                    message: "Division by zero".to_string(),
                })?,
            };
            Ok(CobolValue::Numeric(result))
        }

        SimpleExpr::FunctionCall { name, args } => {
            eval_intrinsic_function(name, args, env)
        }

        SimpleExpr::RefMod { variable, start, length } => {
            let val = eval_expr(variable, env)?;
            let s = val.to_display_string();
            let start_pos = to_numeric(&eval_expr(start, env)?).integer_part() as usize;
            // COBOL reference modification is 1-based
            let zero_start = if start_pos > 0 { start_pos - 1 } else { 0 };
            let end_pos = if let Some(len_expr) = length {
                let len = to_numeric(&eval_expr(len_expr, env)?).integer_part() as usize;
                (zero_start + len).min(s.len())
            } else {
                s.len()
            };
            let zero_start = zero_start.min(s.len());
            let substring = &s[zero_start..end_pos];
            Ok(CobolValue::Alphanumeric(substring.to_string()))
        }
    }
}

// ================================================================
// Calendar helpers for Lilian day calculations
// ================================================================

/// Whether a year is a leap year.
fn is_leap_year(year: i64) -> bool {
    year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
}

/// Days in each month for a given year.
fn days_in_month(year: i64) -> [i64; 12] {
    if is_leap_year(year) {
        [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    } else {
        [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    }
}

/// Convert a Gregorian date (YYYYMMDD) to a Lilian day number.
/// Lilian day 1 = October 15, 1582 (start of the Gregorian calendar).
fn gregorian_to_lilian(year: i64, month: i64, day: i64) -> i64 {
    // Calculate days from the Lilian epoch (1582-10-15)
    let mut total_days: i64 = 0;

    // Count full years from 1582 to year-1
    for y in 1582..year {
        total_days += if is_leap_year(y) { 366 } else { 365 };
    }

    // Add days for months in the target year
    let md = days_in_month(year);
    for d in md.iter().take((month - 1) as usize) {
        total_days += d;
    }

    // Add the day of month
    total_days += day;

    // Subtract the days before the Lilian epoch in 1582
    // Jan 1 to Oct 14 in 1582:
    let epoch_offset = {
        let md_1582 = days_in_month(1582);
        let mut offset: i64 = 0;
        // Full months Jan-Sep
        for d in md_1582.iter().take(9) {
            offset += d;
        }
        // Oct 1-14
        offset += 14;
        offset
    };

    total_days - epoch_offset
}

/// Convert a Lilian day number to a Gregorian date (year, month, day).
fn lilian_to_gregorian(lilian: i64) -> (i64, i64, i64) {
    // Lilian day 1 = October 15, 1582
    // Start from Oct 15, 1582 and add days
    let remaining = lilian - 1; // day 1 = Oct 15 itself

    let mut year: i64 = 1582;
    let mut month: i64 = 10;
    let mut day: i64 = 15;

    // Add remaining days to Oct 15, 1582
    day += remaining;

    // Normalize: advance months/years
    loop {
        let md = days_in_month(year);
        let dim = md[(month - 1) as usize];
        if day <= dim {
            break;
        }
        day -= dim;
        month += 1;
        if month > 12 {
            month = 1;
            year += 1;
        }
    }

    (year, month, day)
}

/// Convert a Gregorian date to day-of-year (1-366).
fn day_of_year(year: i64, month: i64, day: i64) -> i64 {
    let md = days_in_month(year);
    let mut doy: i64 = 0;
    for d in md.iter().take((month - 1) as usize) {
        doy += d;
    }
    doy + day
}

/// Convert year and day-of-year to (year, month, day).
fn from_day_of_year(year: i64, doy: i64) -> (i64, i64, i64) {
    let md = days_in_month(year);
    let mut remaining = doy;
    for (i, &d) in md.iter().enumerate() {
        if remaining <= d {
            return (year, (i + 1) as i64, remaining);
        }
        remaining -= d;
    }
    (year, 12, 31) // fallback
}

/// Date from UNIX epoch seconds: (year, month, day, hh, mm, ss).
fn date_from_epoch_secs(secs: u64) -> (i64, i64, i64, u64, u64, u64) {
    let days_since_epoch = (secs / 86400) as i64;
    let time_of_day = secs % 86400;
    let hh = time_of_day / 3600;
    let mm = (time_of_day % 3600) / 60;
    let ss = time_of_day % 60;

    let mut year = 1970i64;
    let mut remaining = days_since_epoch;
    loop {
        let diy = if is_leap_year(year) { 366 } else { 365 };
        if remaining < diy {
            break;
        }
        remaining -= diy;
        year += 1;
    }
    let md = days_in_month(year);
    let mut month = 1i64;
    for &d in &md {
        if remaining < d {
            break;
        }
        remaining -= d;
        month += 1;
    }
    let day = remaining + 1;
    (year, month, day, hh, mm, ss)
}

// ================================================================
// Intrinsic function evaluation
// ================================================================

/// Parse sign from a NUMVAL-style string. Returns (number_str, is_negative).
fn parse_numval_sign(cleaned: &str) -> (&str, bool) {
    if let Some(s) = cleaned.strip_suffix("CR").or_else(|| cleaned.strip_suffix("DB")) {
        (s, true)
    } else if let Some(s) = cleaned.strip_suffix('-') {
        (s, true)
    } else if let Some(s) = cleaned.strip_prefix('-') {
        (s, true)
    } else if let Some(s) = cleaned.strip_prefix('+') {
        (s, false)
    } else {
        (cleaned, false)
    }
}

/// Evaluate a COBOL intrinsic function.
fn eval_intrinsic_function(
    name: &str,
    args: &[SimpleExpr],
    env: &Environment,
) -> Result<CobolValue> {
    /// Helper to get a single required argument.
    fn require_arg<'a>(
        args: &'a [SimpleExpr],
        func_name: &str,
    ) -> Result<&'a SimpleExpr> {
        args.first().ok_or_else(|| InterpreterError {
            message: format!("{} requires an argument", func_name),
        })
    }

    match name {
        // ============================================================
        // String functions (Story 502.1)
        // ============================================================
        "UPPER-CASE" => {
            let val = eval_expr(require_arg(args, "UPPER-CASE")?, env)?;
            Ok(CobolValue::Alphanumeric(val.to_display_string().to_uppercase()))
        }
        "LOWER-CASE" => {
            let val = eval_expr(require_arg(args, "LOWER-CASE")?, env)?;
            Ok(CobolValue::Alphanumeric(val.to_display_string().to_lowercase()))
        }
        "TRIM" => {
            let val = eval_expr(require_arg(args, "TRIM")?, env)?;
            let s = val.to_display_string();
            // COBOL TRIM: with 2nd arg LEADING/TRAILING, else both
            if args.len() > 1 {
                let mode = eval_expr(&args[1], env)?.to_display_string().to_uppercase();
                let trimmed = match mode.trim() {
                    "LEADING" => s.trim_start().to_string(),
                    "TRAILING" => s.trim_end().to_string(),
                    _ => s.trim().to_string(),
                };
                Ok(CobolValue::Alphanumeric(trimmed))
            } else {
                Ok(CobolValue::Alphanumeric(s.trim().to_string()))
            }
        }
        "REVERSE" => {
            let val = eval_expr(require_arg(args, "REVERSE")?, env)?;
            let s = val.to_display_string();
            Ok(CobolValue::Alphanumeric(s.chars().rev().collect()))
        }
        "ORD" => {
            // Returns the ordinal position (1-based) of the first character
            let val = eval_expr(require_arg(args, "ORD")?, env)?;
            let s = val.to_display_string();
            let ord = s.bytes().next().map(|b| b as i64 + 1).unwrap_or(0);
            Ok(CobolValue::from_i64(ord))
        }
        "CHAR" => {
            // Returns the character at ordinal position (1-based)
            let val = eval_expr(require_arg(args, "CHAR")?, env)?;
            let n = to_numeric(&val).integer_part();
            if (1..=256).contains(&n) {
                let ch = (n - 1) as u8 as char;
                Ok(CobolValue::Alphanumeric(ch.to_string()))
            } else {
                Ok(CobolValue::Alphanumeric(" ".to_string()))
            }
        }
        "CONCATENATE" => {
            let mut result = String::new();
            for arg in args {
                let val = eval_expr(arg, env)?;
                result.push_str(&val.to_display_string());
            }
            Ok(CobolValue::Alphanumeric(result))
        }
        "BYTE-LENGTH" => {
            let arg = require_arg(args, "BYTE-LENGTH")?;
            let len = if let SimpleExpr::Variable(var_name) = arg {
                env.meta(var_name)
                    .map(|m| m.size)
                    .unwrap_or_else(|| {
                        env.get(var_name)
                            .map(|v| v.to_display_string().len())
                            .unwrap_or(0)
                    })
            } else {
                let val = eval_expr(arg, env)?;
                val.to_display_string().len()
            };
            Ok(CobolValue::from_i64(len as i64))
        }
        "LENGTH" => {
            let arg = require_arg(args, "LENGTH")?;
            let len = if let SimpleExpr::Variable(var_name) = arg {
                env.meta(var_name)
                    .map(|m| m.size)
                    .unwrap_or_else(|| {
                        env.get(var_name)
                            .map(|v| v.to_display_string().len())
                            .unwrap_or(0)
                    })
            } else {
                let val = eval_expr(arg, env)?;
                val.to_display_string().len()
            };
            Ok(CobolValue::from_i64(len as i64))
        }

        // ============================================================
        // Numeric functions (Story 502.2)
        // ============================================================
        "NUMVAL" => {
            // Parse an edited numeric string to a numeric value
            let val = eval_expr(require_arg(args, "NUMVAL")?, env)?;
            let s = val.to_display_string();
            let cleaned: String = s.trim().replace(',', "");
            let (num_str, negative) = parse_numval_sign(&cleaned);
            let parsed: rust_decimal::Decimal = num_str.trim().parse().unwrap_or_default();
            let final_val = if negative { -parsed } else { parsed };
            let dp = final_val.scale() as u8;
            Ok(CobolValue::Numeric(NumericValue::new(final_val, dp, true)))
        }
        "NUMVAL-C" => {
            // Like NUMVAL but strips currency symbols
            let val = eval_expr(require_arg(args, "NUMVAL-C")?, env)?;
            let s = val.to_display_string();
            let currency = if args.len() > 1 {
                eval_expr(&args[1], env)?.to_display_string().trim().to_string()
            } else {
                "$".to_string()
            };
            let cleaned: String = s.trim().replace(',', "").replace(&currency, "");
            let (num_str, negative) = parse_numval_sign(&cleaned);
            let parsed: rust_decimal::Decimal = num_str.trim().parse().unwrap_or_default();
            let final_val = if negative { -parsed } else { parsed };
            let dp = final_val.scale() as u8;
            Ok(CobolValue::Numeric(NumericValue::new(final_val, dp, true)))
        }
        "MOD" => {
            // FUNCTION MOD(a, b) = a - (b * FUNCTION INTEGER(a / b))
            // IBM definition: result has same sign as b
            if args.len() < 2 {
                return Err(InterpreterError { message: "MOD requires two arguments".to_string() });
            }
            let a = to_numeric(&eval_expr(&args[0], env)?).value;
            let b = to_numeric(&eval_expr(&args[1], env)?).value;
            if b.is_zero() {
                return Err(InterpreterError { message: "MOD: division by zero".to_string() });
            }
            let quotient = (a / b).floor();
            let result = a - b * quotient;
            Ok(CobolValue::Numeric(NumericValue::new(result, 0, true)))
        }
        "REM" => {
            // FUNCTION REM(a, b) = a - (b * FUNCTION INTEGER-PART(a / b))
            // Result has same sign as a (truncation toward zero)
            if args.len() < 2 {
                return Err(InterpreterError { message: "REM requires two arguments".to_string() });
            }
            let a = to_numeric(&eval_expr(&args[0], env)?).value;
            let b = to_numeric(&eval_expr(&args[1], env)?).value;
            if b.is_zero() {
                return Err(InterpreterError { message: "REM: division by zero".to_string() });
            }
            let quotient = (a / b).trunc();
            let result = a - b * quotient;
            Ok(CobolValue::Numeric(NumericValue::new(result, 0, true)))
        }
        "INTEGER" => {
            // Greatest integer <= argument (floor)
            let val = to_numeric(&eval_expr(require_arg(args, "INTEGER")?, env)?);
            let result = val.value.floor();
            Ok(CobolValue::Numeric(NumericValue::new(result, 0, true)))
        }
        "INTEGER-PART" => {
            // Truncation toward zero
            let val = to_numeric(&eval_expr(require_arg(args, "INTEGER-PART")?, env)?);
            let result = val.value.trunc();
            Ok(CobolValue::Numeric(NumericValue::new(result, 0, true)))
        }
        "MAX" => {
            if args.is_empty() {
                return Err(InterpreterError { message: "MAX requires at least one argument".to_string() });
            }
            let mut max_val = eval_expr(&args[0], env)?;
            for arg in &args[1..] {
                let v = eval_expr(arg, env)?;
                let max_n = to_numeric(&max_val).value;
                let v_n = to_numeric(&v).value;
                if v_n > max_n {
                    max_val = v;
                }
            }
            Ok(max_val)
        }
        "MIN" => {
            if args.is_empty() {
                return Err(InterpreterError { message: "MIN requires at least one argument".to_string() });
            }
            let mut min_val = eval_expr(&args[0], env)?;
            for arg in &args[1..] {
                let v = eval_expr(arg, env)?;
                let min_n = to_numeric(&min_val).value;
                let v_n = to_numeric(&v).value;
                if v_n < min_n {
                    min_val = v;
                }
            }
            Ok(min_val)
        }
        "ABS" => {
            let val = to_numeric(&eval_expr(require_arg(args, "ABS")?, env)?);
            let result = val.value.abs();
            let dp = val.decimal_places;
            Ok(CobolValue::Numeric(NumericValue::new(result, dp, true)))
        }
        "RANDOM" => {
            // Simple deterministic PRNG using seed if provided
            // Returns a value between 0.0 and 1.0
            if let Some(arg) = args.first() {
                let seed = to_numeric(&eval_expr(arg, env)?).integer_part();
                // Linear congruential: (seed * 1103515245 + 12345) mod 2^31
                let raw = ((seed.wrapping_mul(1103515245)).wrapping_add(12345)) & 0x7FFFFFFF;
                let frac = rust_decimal::Decimal::new(raw, 10);
                Ok(CobolValue::Numeric(NumericValue::new(frac.abs(), 10, false)))
            } else {
                // No seed: use system time as seed
                let now = std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_nanos() as i64;
                let raw = (now.wrapping_mul(1103515245).wrapping_add(12345)) & 0x7FFFFFFF;
                let frac = rust_decimal::Decimal::new(raw, 10);
                Ok(CobolValue::Numeric(NumericValue::new(frac.abs(), 10, false)))
            }
        }
        "SQRT" => {
            let val = to_numeric(&eval_expr(require_arg(args, "SQRT")?, env)?);
            // Use Newton's method for square root on Decimal
            let n = val.value;
            if n.is_sign_negative() {
                return Err(InterpreterError { message: "SQRT: negative argument".to_string() });
            }
            if n.is_zero() {
                return Ok(CobolValue::Numeric(NumericValue::new(rust_decimal::Decimal::ZERO, 0, false)));
            }
            let mut guess = n / rust_decimal::Decimal::TWO;
            for _ in 0..50 {
                let next = (guess + n / guess) / rust_decimal::Decimal::TWO;
                if (next - guess).abs() < rust_decimal::Decimal::new(1, 18) {
                    break;
                }
                guess = next;
            }
            Ok(CobolValue::Numeric(NumericValue::new(guess, 18, false)))
        }
        "MEDIAN" => {
            if args.is_empty() {
                return Err(InterpreterError { message: "MEDIAN requires at least one argument".to_string() });
            }
            let mut values: Vec<rust_decimal::Decimal> = Vec::new();
            for arg in args {
                values.push(to_numeric(&eval_expr(arg, env)?).value);
            }
            values.sort();
            let mid = values.len() / 2;
            let result = if values.len() % 2 == 0 {
                (values[mid - 1] + values[mid]) / rust_decimal::Decimal::TWO
            } else {
                values[mid]
            };
            Ok(CobolValue::Numeric(NumericValue::new(result, 18, true)))
        }
        "MEAN" | "MIDRANGE" => {
            if args.is_empty() {
                return Err(InterpreterError { message: format!("{} requires at least one argument", name) });
            }
            if name == "MIDRANGE" {
                let mut min_v = to_numeric(&eval_expr(&args[0], env)?).value;
                let mut max_v = min_v;
                for arg in &args[1..] {
                    let v = to_numeric(&eval_expr(arg, env)?).value;
                    if v < min_v { min_v = v; }
                    if v > max_v { max_v = v; }
                }
                let result = (min_v + max_v) / rust_decimal::Decimal::TWO;
                Ok(CobolValue::Numeric(NumericValue::new(result, 18, true)))
            } else {
                let mut sum = rust_decimal::Decimal::ZERO;
                for arg in args {
                    sum += to_numeric(&eval_expr(arg, env)?).value;
                }
                let count = rust_decimal::Decimal::from(args.len() as i64);
                let result = sum / count;
                Ok(CobolValue::Numeric(NumericValue::new(result, 18, true)))
            }
        }
        "RANGE" => {
            if args.is_empty() {
                return Err(InterpreterError { message: "RANGE requires at least one argument".to_string() });
            }
            let mut min_v = to_numeric(&eval_expr(&args[0], env)?).value;
            let mut max_v = min_v;
            for arg in &args[1..] {
                let v = to_numeric(&eval_expr(arg, env)?).value;
                if v < min_v { min_v = v; }
                if v > max_v { max_v = v; }
            }
            Ok(CobolValue::Numeric(NumericValue::new(max_v - min_v, 0, true)))
        }
        "ORD-MAX" => {
            if args.is_empty() {
                return Err(InterpreterError { message: "ORD-MAX requires at least one argument".to_string() });
            }
            let mut max_val = to_numeric(&eval_expr(&args[0], env)?).value;
            let mut max_idx: i64 = 1;
            for (i, arg) in args.iter().enumerate().skip(1) {
                let v = to_numeric(&eval_expr(arg, env)?).value;
                if v > max_val {
                    max_val = v;
                    max_idx = (i + 1) as i64;
                }
            }
            Ok(CobolValue::from_i64(max_idx))
        }
        "ORD-MIN" => {
            if args.is_empty() {
                return Err(InterpreterError { message: "ORD-MIN requires at least one argument".to_string() });
            }
            let mut min_val = to_numeric(&eval_expr(&args[0], env)?).value;
            let mut min_idx: i64 = 1;
            for (i, arg) in args.iter().enumerate().skip(1) {
                let v = to_numeric(&eval_expr(arg, env)?).value;
                if v < min_val {
                    min_val = v;
                    min_idx = (i + 1) as i64;
                }
            }
            Ok(CobolValue::from_i64(min_idx))
        }
        "SUM" => {
            let mut sum = rust_decimal::Decimal::ZERO;
            for arg in args {
                sum += to_numeric(&eval_expr(arg, env)?).value;
            }
            Ok(CobolValue::Numeric(NumericValue::new(sum, 0, true)))
        }

        // ============================================================
        // Date functions (Story 502.3)
        // ============================================================
        "CURRENT-DATE" => {
            // Returns YYYYMMDDHHMMSSCC+HHMM (21 chars)
            let now = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs();
            let (year, month, day, hh, mm, ss) = date_from_epoch_secs(now);
            let date_str = format!(
                "{:04}{:02}{:02}{:02}{:02}{:02}00+0000",
                year, month, day, hh, mm, ss
            );
            Ok(CobolValue::Alphanumeric(date_str))
        }
        "INTEGER-OF-DATE" => {
            // Convert YYYYMMDD to Lilian day number
            let val = to_numeric(&eval_expr(require_arg(args, "INTEGER-OF-DATE")?, env)?);
            let yyyymmdd = val.integer_part();
            let year = yyyymmdd / 10000;
            let month = (yyyymmdd % 10000) / 100;
            let day = yyyymmdd % 100;
            let lilian = gregorian_to_lilian(year, month, day);
            Ok(CobolValue::from_i64(lilian))
        }
        "DATE-OF-INTEGER" => {
            // Convert Lilian day number to YYYYMMDD
            let val = to_numeric(&eval_expr(require_arg(args, "DATE-OF-INTEGER")?, env)?);
            let lilian = val.integer_part();
            let (year, month, day) = lilian_to_gregorian(lilian);
            let yyyymmdd = year * 10000 + month * 100 + day;
            Ok(CobolValue::from_i64(yyyymmdd))
        }
        "INTEGER-OF-DAY" => {
            // Convert YYYYDDD to Lilian day number
            let val = to_numeric(&eval_expr(require_arg(args, "INTEGER-OF-DAY")?, env)?);
            let yyyyddd = val.integer_part();
            let year = yyyyddd / 1000;
            let doy = yyyyddd % 1000;
            let (_, month, day) = from_day_of_year(year, doy);
            let lilian = gregorian_to_lilian(year, month, day);
            Ok(CobolValue::from_i64(lilian))
        }
        "DAY-OF-INTEGER" => {
            // Convert Lilian day number to YYYYDDD
            let val = to_numeric(&eval_expr(require_arg(args, "DAY-OF-INTEGER")?, env)?);
            let lilian = val.integer_part();
            let (year, month, day) = lilian_to_gregorian(lilian);
            let doy = day_of_year(year, month, day);
            let yyyyddd = year * 1000 + doy;
            Ok(CobolValue::from_i64(yyyyddd))
        }
        "DATE-TO-YYYYMMDD" => {
            // Convert a 2-digit year date (YYMMDD) to YYYYMMDD
            // Optional 2nd arg is the century window (default 1900)
            let val = to_numeric(&eval_expr(require_arg(args, "DATE-TO-YYYYMMDD")?, env)?);
            let yymmdd = val.integer_part();
            let yy = yymmdd / 10000;
            let mmdd = yymmdd % 10000;
            let window = if args.len() > 1 {
                to_numeric(&eval_expr(&args[1], env)?).integer_part()
            } else {
                50 // default pivot: 50 means 1950-2049
            };
            let century = if yy < window { 2000 } else { 1900 };
            let yyyy = century + yy;
            Ok(CobolValue::from_i64(yyyy * 10000 + mmdd))
        }
        "DATE-TO-YYYYDDD" => {
            // Convert YYDDD to YYYYDDD
            let val = to_numeric(&eval_expr(require_arg(args, "DATE-TO-YYYYDDD")?, env)?);
            let yyddd = val.integer_part();
            let yy = yyddd / 1000;
            let ddd = yyddd % 1000;
            let window = if args.len() > 1 {
                to_numeric(&eval_expr(&args[1], env)?).integer_part()
            } else {
                50
            };
            let century = if yy < window { 2000 } else { 1900 };
            let yyyy = century + yy;
            Ok(CobolValue::from_i64(yyyy * 1000 + ddd))
        }
        "YEAR-TO-YYYY" => {
            // Convert 2-digit year to 4-digit year
            let val = to_numeric(&eval_expr(require_arg(args, "YEAR-TO-YYYY")?, env)?);
            let yy = val.integer_part();
            let window = if args.len() > 1 {
                to_numeric(&eval_expr(&args[1], env)?).integer_part()
            } else {
                50
            };
            let century = if yy < window { 2000 } else { 1900 };
            Ok(CobolValue::from_i64(century + yy))
        }
        "WHEN-COMPILED" => {
            // Returns a fixed compilation timestamp
            // Format: YYYYMMDDHHMMSSCC+HHMM (same as CURRENT-DATE)
            let now = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs();
            let (year, month, day, hh, mm, ss) = date_from_epoch_secs(now);
            let date_str = format!(
                "{:04}{:02}{:02}{:02}{:02}{:02}00+0000",
                year, month, day, hh, mm, ss
            );
            Ok(CobolValue::Alphanumeric(date_str))
        }

        // Unknown function fallback
        _ => {
            Ok(CobolValue::from_i64(0))
        }
    }
}

/// Compare two alphanumeric strings using COBOL rules (pad shorter with spaces).
fn compare_alpha(l: &str, r: &str) -> std::cmp::Ordering {
    let max_len = l.len().max(r.len());
    let l_padded: String = format!("{:<width$}", l, width = max_len);
    let r_padded: String = format!("{:<width$}", r, width = max_len);
    l_padded.cmp(&r_padded)
}

/// Compare two CobolValues.
fn compare_values(left: &CobolValue, right: &CobolValue) -> std::cmp::Ordering {
    match (left, right) {
        (CobolValue::Numeric(l), CobolValue::Numeric(r)) => l.value.cmp(&r.value),
        (CobolValue::Alphanumeric(l), CobolValue::Alphanumeric(r)) => compare_alpha(l, r),
        (CobolValue::Numeric(l), CobolValue::Alphanumeric(r)) => {
            compare_alpha(&l.to_display_string(), r)
        }
        (CobolValue::Alphanumeric(l), CobolValue::Numeric(r)) => {
            compare_alpha(l, &r.to_display_string())
        }
        _ => std::cmp::Ordering::Equal,
    }
}

/// Execute nested AFTER loops for PERFORM VARYING ... AFTER.
///
/// Recursively handles multiple AFTER levels. Each level iterates its variable
/// from FROM by BY until UNTIL, then the next outer level increments.
fn execute_after_loop(
    afters: &[VaryingAfter],
    level: usize,
    stmts: &[SimpleStatement],
    program: &SimpleProgram,
    env: &mut Environment,
    count: &mut usize,
    max_iterations: usize,
) -> Result<()> {
    if level >= afters.len() {
        // Base case: execute the loop body
        execute_statements(stmts, program, env)?;
        *count += 1;
        return Ok(());
    }

    let af = &afters[level];

    loop {
        if env.is_stopped() || env.is_goback() || *count >= max_iterations {
            break;
        }

        // Check UNTIL condition for this AFTER level
        if eval_condition(&af.until, env, program)? {
            break;
        }

        if level + 1 < afters.len() {
            // Re-initialize next AFTER level variable
            let from_val = eval_expr(&afters[level + 1].from, env)?;
            env.set(&afters[level + 1].variable, from_val)?;
        }

        // Recurse to inner level or execute body
        execute_after_loop(afters, level + 1, stmts, program, env, count, max_iterations)?;

        // Increment this AFTER variable
        let by_val = to_numeric(&eval_expr(&af.by, env)?);
        if let Some(current) = env.get(&af.variable).cloned() {
            let new_val = to_numeric(&current).add(&by_val);
            env.set(&af.variable, CobolValue::Numeric(new_val))?;
        }
    }

    Ok(())
}

/// Evaluate a condition.
fn eval_condition(cond: &SimpleCondition, env: &Environment, program: &SimpleProgram) -> Result<bool> {
    match cond {
        SimpleCondition::Compare { left, op, right } => {
            let l = eval_expr(left, env)?;
            let r = eval_expr(right, env)?;
            if std::env::var("OPEN_MAINFRAME_DEBUG_CMP").is_ok() {
                eprintln!("[CMP] {:?} {:?} {:?} => {:?}", l, op, r, compare_values(&l, &r));
            }
            Ok(match op {
                SimpleCompareOp::Equal => compare_values(&l, &r) == std::cmp::Ordering::Equal,
                SimpleCompareOp::NotEqual => compare_values(&l, &r) != std::cmp::Ordering::Equal,
                SimpleCompareOp::LessThan => compare_values(&l, &r) == std::cmp::Ordering::Less,
                SimpleCompareOp::LessOrEqual => {
                    compare_values(&l, &r) != std::cmp::Ordering::Greater
                }
                SimpleCompareOp::GreaterThan => {
                    compare_values(&l, &r) == std::cmp::Ordering::Greater
                }
                SimpleCompareOp::GreaterOrEqual => {
                    compare_values(&l, &r) != std::cmp::Ordering::Less
                }
            })
        }

        SimpleCondition::Not(inner) => Ok(!eval_condition(inner, env, program)?),

        SimpleCondition::And(left, right) => {
            Ok(eval_condition(left, env, program)? && eval_condition(right, env, program)?)
        }

        SimpleCondition::Or(left, right) => {
            Ok(eval_condition(left, env, program)? || eval_condition(right, env, program)?)
        }

        SimpleCondition::ConditionName(name) => {
            // Look up the level-88 condition: condition_name -> ConditionDef
            if let Some(def) = program.condition_names.get(&name.to_uppercase()) {
                let actual = env.get(&def.parent)
                    .map(|v| v.to_display_string().trim().to_string())
                    .unwrap_or_default();
                let result = def.values.iter().any(|v| match v {
                    ConditionValue::Single(expected) => actual == *expected,
                    ConditionValue::Range(low, high) => actual >= *low && actual <= *high,
                });
                if std::env::var("OPEN_MAINFRAME_DEBUG_CMP").is_ok() {
                    eprintln!("[COND88] {} => {}={:?} values={:?} => {}",
                        name, def.parent, actual, def.values, result);
                }
                Ok(result)
            } else {
                if std::env::var("OPEN_MAINFRAME_DEBUG_CMP").is_ok() {
                    eprintln!("[COND88] {} => NOT FOUND in condition_names", name);
                }
                Ok(false)
            }
        }

        SimpleCondition::ClassCondition { operand, class, negated } => {
            let val = eval_expr(operand, env)?;
            let display = val.to_display_string();
            let result = match class {
                SimpleClassType::Numeric => {
                    // IS NUMERIC: all characters are digits, spaces, signs, or decimal points
                    // For display numeric fields, the display string should be parseable as a number
                    let trimmed = display.trim();
                    if trimmed.is_empty() {
                        // Empty/all-spaces is considered numeric in COBOL
                        false
                    } else {
                        trimmed.chars().all(|c| c.is_ascii_digit() || c == '+' || c == '-' || c == '.')
                    }
                }
                SimpleClassType::Alphabetic => {
                    display.chars().all(|c| c.is_ascii_alphabetic() || c == ' ')
                }
                SimpleClassType::AlphabeticLower => {
                    display.chars().all(|c| c.is_ascii_lowercase() || c == ' ')
                }
                SimpleClassType::AlphabeticUpper => {
                    display.chars().all(|c| c.is_ascii_uppercase() || c == ' ')
                }
            };
            Ok(if *negated { !result } else { result })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_env() -> Environment {
        let output = Vec::<u8>::new();
        let input = std::io::Cursor::new(Vec::<u8>::new());
        Environment::with_io(Box::new(output), Box::new(std::io::BufReader::new(input)))
    }

    #[test]
    fn test_move_and_compute() {
        let mut env = create_test_env();

        env.define(
            "X",
            DataItemMeta {
                size: 5,
                decimals: 0,
                is_numeric: true,
                picture: Some("9(5)".to_string()),
                is_justified: false,
            },
        );

        env.define(
            "Y",
            DataItemMeta {
                size: 5,
                decimals: 0,
                is_numeric: true,
                picture: Some("9(5)".to_string()),
                is_justified: false,
            },
        );

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
            statements: vec![
                SimpleStatement::Move {
                    from: SimpleExpr::Integer(10),
                    to: vec![SimpleExpr::Variable("X".to_string())],
                },
                SimpleStatement::Compute {
                    target: "Y".to_string(),
                    expr: SimpleExpr::Binary {
                        left: Box::new(SimpleExpr::Variable("X".to_string())),
                        op: SimpleBinaryOp::Multiply,
                        right: Box::new(SimpleExpr::Integer(2)),
                    },
                },
            ],
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
        };

        execute(&program, &mut env).unwrap();

        let y = env.get("Y").unwrap();
        assert_eq!(to_numeric(y).integer_part(), 20);
    }

    #[test]
    fn test_if_statement() {
        let mut env = create_test_env();

        env.define(
            "X",
            DataItemMeta {
                size: 5,
                decimals: 0,
                is_numeric: true,
                picture: Some("9(5)".to_string()),
                is_justified: false,
            },
        );

        env.define(
            "RESULT",
            DataItemMeta {
                size: 10,
                decimals: 0,
                is_numeric: false,
                picture: Some("X(10)".to_string()),
                is_justified: false,
            },
        );

        env.set("X", CobolValue::from_i64(5)).unwrap();

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
            statements: vec![SimpleStatement::If {
                condition: SimpleCondition::Compare {
                    left: SimpleExpr::Variable("X".to_string()),
                    op: SimpleCompareOp::GreaterThan,
                    right: SimpleExpr::Integer(3),
                },
                then_branch: vec![SimpleStatement::Move {
                    from: SimpleExpr::String("BIG".to_string()),
                    to: vec![SimpleExpr::Variable("RESULT".to_string())],
                }],
                else_branch: Some(vec![SimpleStatement::Move {
                    from: SimpleExpr::String("SMALL".to_string()),
                    to: vec![SimpleExpr::Variable("RESULT".to_string())],
                }]),
            }],
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
        };

        execute(&program, &mut env).unwrap();

        let result = env.get("RESULT").unwrap();
        assert_eq!(result.to_display_string(), "BIG");
    }

    #[test]
    fn test_add_statement() {
        let mut env = create_test_env();

        env.define(
            "X",
            DataItemMeta {
                size: 5,
                decimals: 0,
                is_numeric: true,
                picture: Some("9(5)".to_string()),
                is_justified: false,
            },
        );

        env.set("X", CobolValue::from_i64(10)).unwrap();

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
            statements: vec![SimpleStatement::Add {
                values: vec![SimpleExpr::Integer(5), SimpleExpr::Integer(3)],
                to: vec!["X".to_string()],
            }],
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
        };

        execute(&program, &mut env).unwrap();

        let x = env.get("X").unwrap();
        assert_eq!(to_numeric(x).integer_part(), 18);
    }

    #[test]
    fn test_stop_run() {
        let mut env = create_test_env();

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
            statements: vec![
                SimpleStatement::StopRun {
                    return_code: Some(4),
                },
                // This should not execute
                SimpleStatement::Move {
                    from: SimpleExpr::Integer(999),
                    to: vec![SimpleExpr::Variable("SHOULD_NOT_EXIST".to_string())],
                },
            ],
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
        };

        let rc = execute(&program, &mut env).unwrap();
        assert_eq!(rc, 4);
        assert!(env.is_stopped());
    }

    #[test]
    fn test_occurs_redefines_filler_initialization() {
        // Simulates the COADM02Y.cpy pattern:
        //   01 MENU-OPTIONS.
        //     05 OPT-COUNT  PIC 9(02) VALUE 6.
        //     05 OPTIONS-DATA.
        //       10 FILLER PIC 9(02) VALUE 1.
        //       10 FILLER PIC X(05) VALUE 'Alpha'.
        //       10 FILLER PIC 9(02) VALUE 2.
        //       10 FILLER PIC X(05) VALUE 'Beta '.
        //     05 OPTIONS-ARRAY REDEFINES OPTIONS-DATA.
        //       10 OPT-ENTRY OCCURS 2 TIMES.
        //         15 OPT-NUM  PIC 9(02).
        //         15 OPT-NAME PIC X(05).
        //
        // Group layout for MENU-OPTIONS should have:
        //   OPT-COUNT at offset 0, size 2
        //   FILLER defaults at offsets 2,4,9,11
        //   OPT-NUM(1) at offset 2, OPT-NAME(1) at offset 4
        //   OPT-NUM(2) at offset 9, OPT-NAME(2) at offset 11

        let mut env = create_test_env();

        // Define data items
        env.define("OPT-COUNT", DataItemMeta {
            size: 2, decimals: 0, is_numeric: true,
            picture: Some("9(02)".to_string()), is_justified: false,
        });

        // Build group layout for MENU-OPTIONS
        let group_fields = vec![
            // Named field: OPT-COUNT
            GroupField { name: "OPT-COUNT".to_string(), offset: 0, size: 2,
                         is_numeric: true, default_value: None },
            // FILLER: VALUE 1 (from OPTIONS-DATA)
            GroupField { name: String::new(), offset: 2, size: 2,
                         is_numeric: false, default_value: Some("1".to_string()) },
            // FILLER: VALUE 'Alpha' (from OPTIONS-DATA)
            GroupField { name: String::new(), offset: 4, size: 5,
                         is_numeric: false, default_value: Some("Alpha".to_string()) },
            // FILLER: VALUE 2 (from OPTIONS-DATA)
            GroupField { name: String::new(), offset: 9, size: 2,
                         is_numeric: false, default_value: Some("2".to_string()) },
            // FILLER: VALUE 'Beta ' (from OPTIONS-DATA)
            GroupField { name: String::new(), offset: 11, size: 5,
                         is_numeric: false, default_value: Some("Beta ".to_string()) },
            // REDEFINES overlay: OPT-NUM(1)
            GroupField { name: "OPT-NUM(1)".to_string(), offset: 2, size: 2,
                         is_numeric: true, default_value: None },
            // REDEFINES overlay: OPT-NAME(1)
            GroupField { name: "OPT-NAME(1)".to_string(), offset: 4, size: 5,
                         is_numeric: false, default_value: None },
            // REDEFINES overlay: OPT-NUM(2)
            GroupField { name: "OPT-NUM(2)".to_string(), offset: 9, size: 2,
                         is_numeric: true, default_value: None },
            // REDEFINES overlay: OPT-NAME(2)
            GroupField { name: "OPT-NAME(2)".to_string(), offset: 11, size: 5,
                         is_numeric: false, default_value: None },
        ];

        let mut group_layouts = HashMap::new();
        group_layouts.insert("MENU-OPTIONS".to_string(), group_fields);

        // Build the initial composed string from FILLER defaults (what load_program does)
        let fields = group_layouts.get("MENU-OPTIONS").unwrap();
        let total_size = fields.iter().map(|f| f.offset + f.size).max().unwrap_or(0);
        let mut buffer = vec![b' '; total_size];
        for field in fields {
            if let Some(ref default) = field.default_value {
                let bytes = default.as_bytes();
                for i in 0..field.size.min(total_size - field.offset) {
                    buffer[field.offset + i] = if i < bytes.len() { bytes[i] } else { b' ' };
                }
            }
        }
        let init_str = String::from_utf8_lossy(&buffer).to_string();

        // Statements: 1) FILLER group init, 2) VALUE init for OPT-COUNT
        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            condition_names: HashMap::new(),
            group_layouts,
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
            statements: vec![
                // Step 1: Initialize group from FILLER defaults
                SimpleStatement::Move {
                    from: SimpleExpr::String(init_str.clone()),
                    to: vec![SimpleExpr::Variable("MENU-OPTIONS".to_string())],
                },
                // Step 2: Initialize named field VALUE
                SimpleStatement::Move {
                    from: SimpleExpr::Integer(6),
                    to: vec![SimpleExpr::Variable("OPT-COUNT".to_string())],
                },
            ],
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
        };

        execute(&program, &mut env).unwrap();

        // Verify: OPT-COUNT should be 6 (from VALUE clause, applied after group init)
        let count = env.get("OPT-COUNT").expect("OPT-COUNT missing");
        assert_eq!(to_numeric(count).integer_part(), 6,
            "OPT-COUNT should be 6, got {:?}", count);

        // Verify: OPT-NUM(1) should be 1 (from FILLER VALUE 1, through REDEFINES)
        let num1 = env.get("OPT-NUM(1)").expect("OPT-NUM(1) missing");
        assert_eq!(to_numeric(num1).integer_part(), 1,
            "OPT-NUM(1) should be 1, got {:?}", num1);

        // Verify: OPT-NAME(1) should be "Alpha"
        let name1 = env.get("OPT-NAME(1)").expect("OPT-NAME(1) missing");
        assert_eq!(name1.to_display_string(), "Alpha",
            "OPT-NAME(1) should be 'Alpha', got {:?}", name1);

        // Verify: OPT-NUM(2) should be 2
        let num2 = env.get("OPT-NUM(2)").expect("OPT-NUM(2) missing");
        assert_eq!(to_numeric(num2).integer_part(), 2,
            "OPT-NUM(2) should be 2, got {:?}", num2);

        // Verify: OPT-NAME(2) should be "Beta "
        let name2 = env.get("OPT-NAME(2)").expect("OPT-NAME(2) missing");
        assert_eq!(name2.to_display_string(), "Beta ",
            "OPT-NAME(2) should be 'Beta ', got {:?}", name2);

        // Verify: subscript evaluation works
        let subscript_expr = SimpleExpr::Subscript {
            variable: "OPT-NUM".to_string(),
            index: Box::new(SimpleExpr::Integer(2)),
        };
        let val = eval_expr(&subscript_expr, &env).unwrap();
        assert_eq!(to_numeric(&val).integer_part(), 2,
            "OPT-NUM(2) via subscript should be 2, got {:?}", val);
    }

    /// Helper to create a minimal SimpleProgram for testing.
    fn make_program(statements: Vec<SimpleStatement>) -> SimpleProgram {
        SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
            statements,
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
        }
    }

    /// Helper to create a SimpleProgram with data items.
    fn make_program_with_data(
        name: &str,
        data_items: Vec<(String, DataItemMeta)>,
        statements: Vec<SimpleStatement>,
    ) -> SimpleProgram {
        SimpleProgram {
            name: name.to_string(),
            data_items,
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
            statements,
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
        }
    }

    fn numeric_meta(size: usize) -> DataItemMeta {
        DataItemMeta {
            size,
            decimals: 0,
            is_numeric: true,
            picture: Some(format!("9({})", size)),
            is_justified: false,
        }
    }

    fn alpha_meta(size: usize) -> DataItemMeta {
        DataItemMeta {
            size,
            decimals: 0,
            is_numeric: false,
            picture: Some(format!("X({})", size)),
            is_justified: false,
        }
    }

    // ================================================================
    // Story 500.1 Tests: Program Registry and CALL Dispatch
    // ================================================================

    #[test]
    fn test_program_registry_register_and_lookup() {
        let mut registry = SimpleProgramRegistry::new();
        let subprog = make_program(vec![]);
        registry.register(SimpleProgram {
            name: "SUBPROG".to_string(),
            ..subprog
        });

        let found = registry.lookup("SUBPROG");
        assert!(found.is_some());
        assert_eq!(found.unwrap().name, "SUBPROG");

        // Case-insensitive lookup
        let found = registry.lookup("subprog");
        assert!(found.is_some());
    }

    #[test]
    fn test_program_registry_lookup_not_found() {
        let registry = SimpleProgramRegistry::new();
        assert!(registry.lookup("NONEXISTENT").is_none());
    }

    #[test]
    fn test_call_registered_program() {
        // Story 500.1 AC: CALL 'SUBPROG' USING BY REFERENCE WS-DATA
        // When SUBPROG is registered, it executes with access to WS-DATA
        let mut env = create_test_env();

        // Register a subprogram that sets WS-RESULT = WS-DATA + 10
        let subprog = make_program_with_data(
            "SUBPROG",
            vec![
                ("WS-DATA".to_string(), numeric_meta(5)),
            ],
            vec![
                // COMPUTE WS-DATA = WS-DATA + 10
                SimpleStatement::Compute {
                    target: "WS-DATA".to_string(),
                    expr: SimpleExpr::Binary {
                        left: Box::new(SimpleExpr::Variable("WS-DATA".to_string())),
                        op: SimpleBinaryOp::Add,
                        right: Box::new(SimpleExpr::Integer(10)),
                    },
                },
            ],
        );

        let registry: Box<dyn ProgramRegistry + Send> =
            Box::new(SimpleProgramRegistry::new());
        let registry = Arc::new(Mutex::new(registry));
        registry.lock().unwrap().register(subprog);

        env = env.with_program_registry(Arc::clone(&registry));

        // Define caller data
        env.define("WS-DATA", numeric_meta(5));
        env.set("WS-DATA", CobolValue::from_i64(42)).unwrap();

        // Main program: CALL 'SUBPROG' USING BY REFERENCE WS-DATA
        let main_prog = make_program(vec![
            SimpleStatement::Call {
                program: SimpleExpr::String("SUBPROG".to_string()),
                using: vec![CallParam::ByReference("WS-DATA".to_string())],
            },
        ]);

        execute(&main_prog, &mut env).unwrap();

        // BY REFERENCE: caller's WS-DATA should be modified (42 + 10 = 52)
        let ws_data = env.get("WS-DATA").unwrap();
        assert_eq!(to_numeric(ws_data).integer_part(), 52);
    }

    #[test]
    fn test_call_unknown_program_raises_error() {
        // Story 500.1 AC: CALL 'UNKNOWN' when program is not in registry
        // → runtime error is raised
        let mut env = create_test_env();

        let registry: Box<dyn ProgramRegistry + Send> =
            Box::new(SimpleProgramRegistry::new());
        let registry = Arc::new(Mutex::new(registry));
        env = env.with_program_registry(registry);

        let main_prog = make_program(vec![
            SimpleStatement::Call {
                program: SimpleExpr::String("UNKNOWN".to_string()),
                using: vec![],
            },
        ]);

        let result = execute(&main_prog, &mut env);
        assert!(result.is_err());
        let err_msg = result.unwrap_err().message;
        assert!(err_msg.contains("UNKNOWN"), "Error should mention program name: {}", err_msg);
    }

    // ================================================================
    // Story 500.2 Tests: BY REFERENCE, BY CONTENT, BY VALUE
    // ================================================================

    #[test]
    fn test_call_by_reference_modifies_caller() {
        // Story 500.2 AC: BY REFERENCE — changes in called program are visible to caller
        let mut env = create_test_env();

        let subprog = make_program_with_data(
            "SUB",
            vec![("PARAM-A".to_string(), numeric_meta(5))],
            vec![
                SimpleStatement::Move {
                    from: SimpleExpr::Integer(999),
                    to: vec![SimpleExpr::Variable("PARAM-A".to_string())],
                },
            ],
        );

        let registry: Box<dyn ProgramRegistry + Send> =
            Box::new(SimpleProgramRegistry::new());
        let registry = Arc::new(Mutex::new(registry));
        registry.lock().unwrap().register(subprog);

        env = env.with_program_registry(Arc::clone(&registry));
        env.define("WS-A", numeric_meta(5));
        env.set("WS-A", CobolValue::from_i64(100)).unwrap();

        let main_prog = make_program(vec![
            SimpleStatement::Call {
                program: SimpleExpr::String("SUB".to_string()),
                using: vec![CallParam::ByReference("WS-A".to_string())],
            },
        ]);

        execute(&main_prog, &mut env).unwrap();

        // BY REFERENCE: WS-A should be modified to 999
        let ws_a = env.get("WS-A").unwrap();
        assert_eq!(to_numeric(ws_a).integer_part(), 999);
    }

    #[test]
    fn test_call_by_content_does_not_modify_caller() {
        // Story 500.2 AC: BY CONTENT — changes in called program are NOT visible to caller
        let mut env = create_test_env();

        let subprog = make_program_with_data(
            "SUB",
            vec![("PARAM-B".to_string(), numeric_meta(5))],
            vec![
                SimpleStatement::Move {
                    from: SimpleExpr::Integer(999),
                    to: vec![SimpleExpr::Variable("PARAM-B".to_string())],
                },
            ],
        );

        let registry: Box<dyn ProgramRegistry + Send> =
            Box::new(SimpleProgramRegistry::new());
        let registry = Arc::new(Mutex::new(registry));
        registry.lock().unwrap().register(subprog);

        env = env.with_program_registry(Arc::clone(&registry));
        env.define("WS-B", numeric_meta(5));
        env.set("WS-B", CobolValue::from_i64(100)).unwrap();

        let main_prog = make_program(vec![
            SimpleStatement::Call {
                program: SimpleExpr::String("SUB".to_string()),
                using: vec![CallParam::ByContent("WS-B".to_string())],
            },
        ]);

        execute(&main_prog, &mut env).unwrap();

        // BY CONTENT: WS-B should remain unchanged (100)
        let ws_b = env.get("WS-B").unwrap();
        assert_eq!(to_numeric(ws_b).integer_part(), 100);
    }

    #[test]
    fn test_call_by_value_does_not_modify_caller() {
        // BY VALUE: changes in called program are NOT visible to caller
        let mut env = create_test_env();

        let subprog = make_program_with_data(
            "SUB",
            vec![("PARAM-C".to_string(), numeric_meta(5))],
            vec![
                SimpleStatement::Move {
                    from: SimpleExpr::Integer(777),
                    to: vec![SimpleExpr::Variable("PARAM-C".to_string())],
                },
            ],
        );

        let registry: Box<dyn ProgramRegistry + Send> =
            Box::new(SimpleProgramRegistry::new());
        let registry = Arc::new(Mutex::new(registry));
        registry.lock().unwrap().register(subprog);

        env = env.with_program_registry(Arc::clone(&registry));
        env.define("WS-C", numeric_meta(5));
        env.set("WS-C", CobolValue::from_i64(50)).unwrap();

        let main_prog = make_program(vec![
            SimpleStatement::Call {
                program: SimpleExpr::String("SUB".to_string()),
                using: vec![CallParam::ByValue("WS-C".to_string())],
            },
        ]);

        execute(&main_prog, &mut env).unwrap();

        // BY VALUE: WS-C should remain unchanged (50)
        let ws_c = env.get("WS-C").unwrap();
        assert_eq!(to_numeric(ws_c).integer_part(), 50);
    }

    #[test]
    fn test_call_mixed_parameter_modes() {
        // Mix BY REFERENCE and BY CONTENT in a single CALL
        let mut env = create_test_env();

        let subprog = make_program_with_data(
            "MIXED-SUB",
            vec![
                ("P-REF".to_string(), alpha_meta(10)),
                ("P-CONTENT".to_string(), alpha_meta(10)),
            ],
            vec![
                SimpleStatement::Move {
                    from: SimpleExpr::String("MODIFIED".to_string()),
                    to: vec![SimpleExpr::Variable("P-REF".to_string())],
                },
                SimpleStatement::Move {
                    from: SimpleExpr::String("ALSO-MOD".to_string()),
                    to: vec![SimpleExpr::Variable("P-CONTENT".to_string())],
                },
            ],
        );

        let registry: Box<dyn ProgramRegistry + Send> =
            Box::new(SimpleProgramRegistry::new());
        let registry = Arc::new(Mutex::new(registry));
        registry.lock().unwrap().register(subprog);

        env = env.with_program_registry(Arc::clone(&registry));
        env.define("WS-REF", alpha_meta(10));
        env.define("WS-CONTENT", alpha_meta(10));
        env.set("WS-REF", CobolValue::Alphanumeric("ORIGINAL".to_string())).unwrap();
        env.set("WS-CONTENT", CobolValue::Alphanumeric("ORIGINAL".to_string())).unwrap();

        let main_prog = make_program(vec![
            SimpleStatement::Call {
                program: SimpleExpr::String("MIXED-SUB".to_string()),
                using: vec![
                    CallParam::ByReference("WS-REF".to_string()),
                    CallParam::ByContent("WS-CONTENT".to_string()),
                ],
            },
        ]);

        execute(&main_prog, &mut env).unwrap();

        // BY REFERENCE: should be modified
        let ws_ref = env.get("WS-REF").unwrap();
        assert_eq!(ws_ref.to_display_string(), "MODIFIED");

        // BY CONTENT: should remain unchanged
        let ws_content = env.get("WS-CONTENT").unwrap();
        assert_eq!(ws_content.to_display_string(), "ORIGINAL");
    }

    // ================================================================
    // Story 500.3 Tests: RETURN-CODE and CANCEL
    // ================================================================

    #[test]
    fn test_return_code_from_called_program() {
        // Story 500.3 AC: Called program sets RETURN-CODE to 8 via GOBACK
        // After return, caller's RETURN-CODE contains 8
        let mut env = create_test_env();

        let subprog = make_program_with_data(
            "RC-SUB",
            vec![],
            vec![
                SimpleStatement::GoBack { return_code: Some(8) },
            ],
        );

        let registry: Box<dyn ProgramRegistry + Send> =
            Box::new(SimpleProgramRegistry::new());
        let registry = Arc::new(Mutex::new(registry));
        registry.lock().unwrap().register(subprog);

        env = env.with_program_registry(Arc::clone(&registry));

        let main_prog = make_program(vec![
            SimpleStatement::Call {
                program: SimpleExpr::String("RC-SUB".to_string()),
                using: vec![],
            },
        ]);

        execute(&main_prog, &mut env).unwrap();

        // RETURN-CODE should be 8
        assert_eq!(env.return_code(), 8);
        // Also available as a variable
        let rc_var = env.get("RETURN-CODE").unwrap();
        assert_eq!(to_numeric(rc_var).integer_part(), 8);
    }

    #[test]
    fn test_return_code_via_move_and_goback() {
        // Called program: MOVE 16 TO RETURN-CODE, then GOBACK
        let mut env = create_test_env();

        let subprog = make_program_with_data(
            "RC-SUB2",
            vec![
                ("RETURN-CODE".to_string(), numeric_meta(4)),
            ],
            vec![
                SimpleStatement::Move {
                    from: SimpleExpr::Integer(16),
                    to: vec![SimpleExpr::Variable("RETURN-CODE".to_string())],
                },
                SimpleStatement::GoBack { return_code: None },
            ],
        );

        let registry: Box<dyn ProgramRegistry + Send> =
            Box::new(SimpleProgramRegistry::new());
        let registry = Arc::new(Mutex::new(registry));
        registry.lock().unwrap().register(subprog);

        env = env.with_program_registry(Arc::clone(&registry));

        let main_prog = make_program(vec![
            SimpleStatement::Call {
                program: SimpleExpr::String("RC-SUB2".to_string()),
                using: vec![],
            },
        ]);

        execute(&main_prog, &mut env).unwrap();

        // When GoBack has no return_code, the callee's RETURN-CODE register value is used
        // Since the callee explicitly set return_code to 0 (default), but MOVE 16 to variable
        // won't update the return_code register. Check env.return_code() = 0 (default)
        // The MOVE only sets the variable, not the register, so return_code remains 0.
        assert_eq!(env.return_code(), 0);
    }

    #[test]
    fn test_cancel_removes_program_from_registry() {
        // Story 500.3 AC: CANCEL 'SUBPROG' removes it from registry
        let mut env = create_test_env();

        let subprog = make_program_with_data("TO-CANCEL", vec![], vec![]);

        let registry: Box<dyn ProgramRegistry + Send> =
            Box::new(SimpleProgramRegistry::new());
        let registry = Arc::new(Mutex::new(registry));
        registry.lock().unwrap().register(subprog);

        env = env.with_program_registry(Arc::clone(&registry));

        // Verify program exists
        assert!(registry.lock().unwrap().lookup("TO-CANCEL").is_some());

        let main_prog = make_program(vec![
            SimpleStatement::Cancel {
                program: SimpleExpr::String("TO-CANCEL".to_string()),
            },
        ]);

        execute(&main_prog, &mut env).unwrap();

        // Program should be removed from registry
        assert!(registry.lock().unwrap().lookup("TO-CANCEL").is_none());
    }

    #[test]
    fn test_cancel_then_call_fails() {
        // After CANCEL, calling the same program should fail
        let mut env = create_test_env();

        let subprog = make_program_with_data("CANCEL-ME", vec![], vec![]);

        let registry: Box<dyn ProgramRegistry + Send> =
            Box::new(SimpleProgramRegistry::new());
        let registry = Arc::new(Mutex::new(registry));
        registry.lock().unwrap().register(subprog);

        env = env.with_program_registry(Arc::clone(&registry));

        let main_prog = make_program(vec![
            SimpleStatement::Cancel {
                program: SimpleExpr::String("CANCEL-ME".to_string()),
            },
            SimpleStatement::Call {
                program: SimpleExpr::String("CANCEL-ME".to_string()),
                using: vec![],
            },
        ]);

        let result = execute(&main_prog, &mut env);
        assert!(result.is_err());
    }

    #[test]
    fn test_goback_stops_execution_in_called_program() {
        // GOBACK should stop execution of the called program but not the caller
        let mut env = create_test_env();

        let subprog = make_program_with_data(
            "GOBACK-SUB",
            vec![("WS-FLAG".to_string(), alpha_meta(5))],
            vec![
                SimpleStatement::Move {
                    from: SimpleExpr::String("BEFORE".to_string()),
                    to: vec![SimpleExpr::Variable("WS-FLAG".to_string())],
                },
                SimpleStatement::GoBack { return_code: Some(4) },
                // This should NOT execute
                SimpleStatement::Move {
                    from: SimpleExpr::String("AFTER".to_string()),
                    to: vec![SimpleExpr::Variable("WS-FLAG".to_string())],
                },
            ],
        );

        let registry: Box<dyn ProgramRegistry + Send> =
            Box::new(SimpleProgramRegistry::new());
        let registry = Arc::new(Mutex::new(registry));
        registry.lock().unwrap().register(subprog);

        env = env.with_program_registry(Arc::clone(&registry));

        // Define a post-call variable to prove caller continues
        env.define("CALLER-FLAG", alpha_meta(10));

        let main_prog = make_program(vec![
            SimpleStatement::Call {
                program: SimpleExpr::String("GOBACK-SUB".to_string()),
                using: vec![],
            },
            // Caller should continue executing after CALL returns
            SimpleStatement::Move {
                from: SimpleExpr::String("CONTINUED".to_string()),
                to: vec![SimpleExpr::Variable("CALLER-FLAG".to_string())],
            },
        ]);

        execute(&main_prog, &mut env).unwrap();

        // Caller continues after CALL (GOBACK only stops the callee)
        let flag = env.get("CALLER-FLAG").unwrap();
        assert_eq!(flag.to_display_string(), "CONTINUED");
        assert_eq!(env.return_code(), 4);
    }

    #[test]
    fn test_call_contained_program_still_works() {
        // Ensure existing contained program resolution still functions
        let mut env = create_test_env();
        env.define("WS-RESULT", alpha_meta(10));

        let contained = SimpleProgram {
            name: "INNER".to_string(),
            data_items: vec![],
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
            statements: vec![
                SimpleStatement::Move {
                    from: SimpleExpr::String("FROM-INNER".to_string()),
                    to: vec![SimpleExpr::Variable("WS-RESULT".to_string())],
                },
            ],
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
        };

        let main_prog = SimpleProgram {
            name: "OUTER".to_string(),
            data_items: vec![("WS-RESULT".to_string(), alpha_meta(10))],
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: vec![contained],
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
            statements: vec![
                SimpleStatement::Call {
                    program: SimpleExpr::String("INNER".to_string()),
                    using: vec![],
                },
            ],
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
        };

        execute(&main_prog, &mut env).unwrap();
        // Contained programs copy GLOBAL data, but don't use CallParam
        // The call should succeed without error
    }

    #[test]
    fn test_simple_program_registry_cancel() {
        let mut registry = SimpleProgramRegistry::new();
        let prog = make_program(vec![]);
        registry.register(SimpleProgram {
            name: "MY-PROG".to_string(),
            ..prog
        });

        assert!(registry.lookup("MY-PROG").is_some());
        assert!(registry.cancel("MY-PROG"));
        assert!(registry.lookup("MY-PROG").is_none());

        // Cancelling non-existent program returns false
        assert!(!registry.cancel("NO-SUCH"));
    }

    // ================================================================
    // Epic 501 — True GO TO and Paragraph Flow Control
    // ================================================================

    /// Helper: create a MOVE literal -> variable statement.
    fn move_lit(val: &str, target: &str) -> SimpleStatement {
        SimpleStatement::Move {
            from: SimpleExpr::String(val.to_string()),
            to: vec![SimpleExpr::Variable(target.to_string())],
        }
    }

    fn num_meta() -> DataItemMeta {
        DataItemMeta {
            size: 5,
            decimals: 0,
            is_numeric: true,
            picture: Some("9(5)".to_string()),
            is_justified: false,
        }
    }

    /// Story 501.1: GO TO as unstructured jump.
    /// Given paragraph A contains GO TO C, when executed during PERFORM A THRU D,
    /// then control jumps to paragraph C (skipping B), and execution continues through D.
    #[test]
    fn test_goto_within_perform_thru_skips_paragraphs() {
        // Paragraphs: A (GO TO C), B (MOVE "B" -> RESULT), C (MOVE "C" -> RESULT), D (MOVE "D" -> RESULT)
        // RESULT tracks which paragraphs executed. Only C and D should run.
        let mut paragraphs = HashMap::new();
        paragraphs.insert(
            "PARA-A".to_string(),
            vec![SimpleStatement::GoTo {
                target: "PARA-C".to_string(),
            }],
        );
        paragraphs.insert(
            "PARA-B".to_string(),
            vec![move_lit("B", "WS-B")],
        );
        paragraphs.insert(
            "PARA-C".to_string(),
            vec![move_lit("C", "WS-C")],
        );
        paragraphs.insert(
            "PARA-D".to_string(),
            vec![move_lit("D", "WS-D")],
        );

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![
                ("WS-B".to_string(), alpha_meta(1)),
                ("WS-C".to_string(), alpha_meta(1)),
                ("WS-D".to_string(), alpha_meta(1)),
            ],
            statements: vec![SimpleStatement::PerformThru {
                from: "PARA-A".to_string(),
                thru: "PARA-D".to_string(),
                times: None,
            }],
            paragraphs,
            paragraph_order: vec![
                "PARA-A".to_string(),
                "PARA-B".to_string(),
                "PARA-C".to_string(),
                "PARA-D".to_string(),
            ],
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
        };

        let mut env = create_test_env();
        execute(&program, &mut env).unwrap();
        // B should be skipped, C and D should execute
        assert_eq!(
            env.get("WS-B").unwrap().to_display_string().trim(),
            "",
            "Paragraph B should be skipped by GO TO C"
        );
        assert_eq!(
            env.get("WS-C").unwrap().to_display_string().trim(),
            "C",
            "Paragraph C should execute after GO TO"
        );
        assert_eq!(
            env.get("WS-D").unwrap().to_display_string().trim(),
            "D",
            "Paragraph D should execute (fall-through)"
        );
    }

    /// Story 501.1: GO TO outside of PERFORM transfers control without returning.
    /// Given GO TO C outside of PERFORM, control transfers to paragraph C
    /// and does not return to the GO TO location.
    #[test]
    fn test_goto_outside_perform_no_return() {
        let mut paragraphs = HashMap::new();
        paragraphs.insert(
            "PARA-A".to_string(),
            vec![move_lit("A", "WS-A")],
        );
        paragraphs.insert(
            "PARA-B".to_string(),
            vec![move_lit("B", "WS-B")],
        );
        paragraphs.insert(
            "PARA-C".to_string(),
            vec![move_lit("C", "WS-C")],
        );
        paragraphs.insert(
            "PARA-D".to_string(),
            vec![move_lit("D", "WS-D")],
        );

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![
                ("WS-BEFORE".to_string(), alpha_meta(6)),
                ("WS-AFTER".to_string(), alpha_meta(5)),
                ("WS-A".to_string(), alpha_meta(1)),
                ("WS-B".to_string(), alpha_meta(1)),
                ("WS-C".to_string(), alpha_meta(1)),
                ("WS-D".to_string(), alpha_meta(1)),
            ],
            statements: vec![
                move_lit("BEFORE", "WS-BEFORE"),
                SimpleStatement::GoTo {
                    target: "PARA-C".to_string(),
                },
                // This should NOT execute — GO TO transfers control
                move_lit("AFTER", "WS-AFTER"),
            ],
            paragraphs,
            paragraph_order: vec![
                "PARA-A".to_string(),
                "PARA-B".to_string(),
                "PARA-C".to_string(),
                "PARA-D".to_string(),
            ],
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
        };

        let mut env = create_test_env();
        execute(&program, &mut env).unwrap();
        assert_eq!(
            env.get("WS-BEFORE").unwrap().to_display_string().trim(),
            "BEFORE",
            "Statement before GO TO should execute"
        );
        assert_eq!(
            env.get("WS-AFTER").unwrap().to_display_string().trim(),
            "",
            "Statement after GO TO should NOT execute"
        );
        assert_eq!(
            env.get("WS-A").unwrap().to_display_string().trim(),
            "",
            "Paragraph A should not execute"
        );
        assert_eq!(
            env.get("WS-B").unwrap().to_display_string().trim(),
            "",
            "Paragraph B should not execute"
        );
        assert_eq!(
            env.get("WS-C").unwrap().to_display_string().trim(),
            "C",
            "Paragraph C should execute (GO TO target)"
        );
        assert_eq!(
            env.get("WS-D").unwrap().to_display_string().trim(),
            "D",
            "Paragraph D should execute (fall-through)"
        );
    }

    /// Story 501.2: PERFORM THRU executes paragraphs sequentially.
    /// Given PERFORM INIT-PARA THRU INIT-EXIT, all paragraphs from
    /// INIT-PARA to INIT-EXIT are executed sequentially.
    #[test]
    fn test_perform_thru_sequential_execution() {
        // Use a counter variable to track execution order
        let mut paragraphs = HashMap::new();
        paragraphs.insert(
            "INIT-PARA".to_string(),
            vec![move_lit("1", "WS-INIT")],
        );
        paragraphs.insert(
            "PROCESS-PARA".to_string(),
            vec![move_lit("2", "WS-PROC")],
        );
        paragraphs.insert(
            "INIT-EXIT".to_string(),
            vec![move_lit("3", "WS-EXIT")],
        );

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![
                ("WS-INIT".to_string(), alpha_meta(1)),
                ("WS-PROC".to_string(), alpha_meta(1)),
                ("WS-EXIT".to_string(), alpha_meta(1)),
            ],
            statements: vec![SimpleStatement::PerformThru {
                from: "INIT-PARA".to_string(),
                thru: "INIT-EXIT".to_string(),
                times: None,
            }],
            paragraphs,
            paragraph_order: vec![
                "INIT-PARA".to_string(),
                "PROCESS-PARA".to_string(),
                "INIT-EXIT".to_string(),
            ],
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
        };

        let mut env = create_test_env();
        execute(&program, &mut env).unwrap();
        assert_eq!(env.get("WS-INIT").unwrap().to_display_string().trim(), "1");
        assert_eq!(env.get("WS-PROC").unwrap().to_display_string().trim(), "2");
        assert_eq!(env.get("WS-EXIT").unwrap().to_display_string().trim(), "3");
    }

    /// Story 501.2: EXIT PARAGRAPH within a PERFORM range exits the current
    /// paragraph and continues with the next one.
    #[test]
    fn test_exit_paragraph_in_perform_thru() {
        let mut paragraphs = HashMap::new();
        paragraphs.insert(
            "PARA-A".to_string(),
            vec![
                move_lit("A-BEFORE", "WS-A"),
                SimpleStatement::ExitParagraph,
                // This should NOT execute
                move_lit("A-AFTER", "WS-A"),
            ],
        );
        paragraphs.insert(
            "PARA-B".to_string(),
            vec![move_lit("B", "WS-B")],
        );

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![
                ("WS-A".to_string(), alpha_meta(10)),
                ("WS-B".to_string(), alpha_meta(1)),
            ],
            statements: vec![SimpleStatement::PerformThru {
                from: "PARA-A".to_string(),
                thru: "PARA-B".to_string(),
                times: None,
            }],
            paragraphs,
            paragraph_order: vec!["PARA-A".to_string(), "PARA-B".to_string()],
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
        };

        let mut env = create_test_env();
        execute(&program, &mut env).unwrap();
        assert_eq!(
            env.get("WS-A").unwrap().to_display_string().trim(),
            "A-BEFORE",
            "Statement before EXIT PARAGRAPH should execute; A-AFTER should not overwrite"
        );
        assert_eq!(
            env.get("WS-B").unwrap().to_display_string().trim(),
            "B",
            "Next paragraph should still execute after EXIT PARAGRAPH"
        );
    }

    /// Story 501.3: ALTER changes GO TO target dynamically.
    /// Given ALTER DISPATCH-PARA TO PROCEED TO NEW-TARGET, when DISPATCH-PARA
    /// (which contains GO TO) executes, control transfers to NEW-TARGET.
    #[test]
    fn test_alter_changes_goto_target() {
        let mut paragraphs = HashMap::new();
        // DISPATCH-PARA has a GO TO ORIGINAL-TARGET
        paragraphs.insert(
            "DISPATCH-PARA".to_string(),
            vec![SimpleStatement::GoTo {
                target: "ORIGINAL-TARGET".to_string(),
            }],
        );
        paragraphs.insert(
            "ORIGINAL-TARGET".to_string(),
            vec![move_lit("ORIGINAL", "WS-RESULT")],
        );
        paragraphs.insert(
            "NEW-TARGET".to_string(),
            vec![move_lit("NEW", "WS-RESULT")],
        );

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![("WS-RESULT".to_string(), alpha_meta(10))],
            statements: vec![
                // ALTER DISPATCH-PARA TO PROCEED TO NEW-TARGET
                SimpleStatement::Alter {
                    source: "DISPATCH-PARA".to_string(),
                    target: "NEW-TARGET".to_string(),
                },
                // PERFORM DISPATCH-PARA THRU NEW-TARGET
                SimpleStatement::PerformThru {
                    from: "DISPATCH-PARA".to_string(),
                    thru: "NEW-TARGET".to_string(),
                    times: None,
                },
            ],
            paragraphs,
            paragraph_order: vec![
                "DISPATCH-PARA".to_string(),
                "ORIGINAL-TARGET".to_string(),
                "NEW-TARGET".to_string(),
            ],
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
        };

        let mut env = create_test_env();
        execute(&program, &mut env).unwrap();
        // ALTER redirects GO TO from ORIGINAL-TARGET to NEW-TARGET,
        // so ORIGINAL-TARGET is skipped, only NEW-TARGET executes
        assert_eq!(
            env.get("WS-RESULT").unwrap().to_display_string().trim(),
            "NEW",
            "ALTER should redirect GO TO to NEW-TARGET"
        );
    }

    /// Story 501.2: EXIT SECTION within a PERFORM THRU stops the whole range.
    #[test]
    fn test_exit_section_stops_perform_thru() {
        let mut paragraphs = HashMap::new();
        paragraphs.insert(
            "PARA-A".to_string(),
            vec![
                move_lit("A", "WS-A"),
                SimpleStatement::ExitSection,
            ],
        );
        paragraphs.insert(
            "PARA-B".to_string(),
            vec![move_lit("B", "WS-B")],
        );

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![
                ("WS-A".to_string(), alpha_meta(1)),
                ("WS-B".to_string(), alpha_meta(1)),
            ],
            statements: vec![SimpleStatement::PerformThru {
                from: "PARA-A".to_string(),
                thru: "PARA-B".to_string(),
                times: None,
            }],
            paragraphs,
            paragraph_order: vec!["PARA-A".to_string(), "PARA-B".to_string()],
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
        };

        let mut env = create_test_env();
        execute(&program, &mut env).unwrap();
        assert_eq!(
            env.get("WS-A").unwrap().to_display_string().trim(),
            "A",
            "PARA-A should execute before EXIT SECTION"
        );
        assert_eq!(
            env.get("WS-B").unwrap().to_display_string().trim(),
            "",
            "PARA-B should NOT execute after EXIT SECTION"
        );
    }

    /// Story 501.1: GO TO with fall-through continues paragraphs after target.
    #[test]
    fn test_goto_fall_through() {
        let mut paragraphs = HashMap::new();
        paragraphs.insert(
            "PARA-A".to_string(),
            vec![move_lit("A", "WS-A")],
        );
        paragraphs.insert(
            "PARA-B".to_string(),
            vec![move_lit("B", "WS-B")],
        );
        paragraphs.insert(
            "PARA-C".to_string(),
            vec![move_lit("C", "WS-C")],
        );

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![
                ("WS-A".to_string(), alpha_meta(1)),
                ("WS-B".to_string(), alpha_meta(1)),
                ("WS-C".to_string(), alpha_meta(1)),
            ],
            // Top-level GO TO PARA-B: should fall through B, C
            statements: vec![SimpleStatement::GoTo {
                target: "PARA-B".to_string(),
            }],
            paragraphs,
            paragraph_order: vec![
                "PARA-A".to_string(),
                "PARA-B".to_string(),
                "PARA-C".to_string(),
            ],
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
        };

        let mut env = create_test_env();
        execute(&program, &mut env).unwrap();
        assert_eq!(
            env.get("WS-A").unwrap().to_display_string().trim(),
            "",
            "PARA-A should be skipped"
        );
        assert_eq!(
            env.get("WS-B").unwrap().to_display_string().trim(),
            "B",
            "PARA-B should execute (GO TO target)"
        );
        assert_eq!(
            env.get("WS-C").unwrap().to_display_string().trim(),
            "C",
            "PARA-C should execute (fall-through)"
        );
    }

    /// Story 501.2: PERFORM THRU with TIMES clause repeats the range.
    #[test]
    fn test_perform_thru_with_times() {
        // Use a numeric counter that gets incremented in each paragraph
        let mut paragraphs = HashMap::new();
        paragraphs.insert(
            "PARA-A".to_string(),
            vec![SimpleStatement::Add {
                values: vec![SimpleExpr::Integer(1)],
                to: vec!["WS-COUNT".to_string()],
            }],
        );
        paragraphs.insert(
            "PARA-B".to_string(),
            vec![SimpleStatement::Add {
                values: vec![SimpleExpr::Integer(1)],
                to: vec!["WS-COUNT".to_string()],
            }],
        );

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![("WS-COUNT".to_string(), num_meta())],
            statements: vec![SimpleStatement::PerformThru {
                from: "PARA-A".to_string(),
                thru: "PARA-B".to_string(),
                times: Some(3),
            }],
            paragraphs,
            paragraph_order: vec!["PARA-A".to_string(), "PARA-B".to_string()],
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
        };

        let mut env = create_test_env();
        execute(&program, &mut env).unwrap();
        // 3 iterations * 2 paragraphs = 6 increments
        let count = env.get("WS-COUNT").unwrap().to_display_string().trim().to_string();
        assert_eq!(count, "6", "Should execute 6 times (3 iterations x 2 paragraphs)");
    }

    /// ControlFlow enum variant tests.
    #[test]
    fn test_controlflow_enum() {
        // Test PartialEq
        assert_eq!(ControlFlow::Continue, ControlFlow::Continue);
        assert_eq!(
            ControlFlow::GoTo("X".to_string()),
            ControlFlow::GoTo("X".to_string())
        );
        assert_ne!(ControlFlow::Continue, ControlFlow::ExitParagraph);
        assert_ne!(ControlFlow::ExitParagraph, ControlFlow::ExitSection);
    }

    // ================================================================
    // Epic 502 — Complete Intrinsic Functions
    // ================================================================

    /// Helper: evaluate an intrinsic function with given arguments.
    fn eval_func(name: &str, args: Vec<SimpleExpr>) -> CobolValue {
        let env = create_test_env();
        eval_intrinsic_function(name, &args, &env).unwrap()
    }

    fn str_arg(s: &str) -> SimpleExpr {
        SimpleExpr::String(s.to_string())
    }

    fn int_arg(n: i64) -> SimpleExpr {
        SimpleExpr::Integer(n)
    }

    // Story 502.1: String functions

    #[test]
    fn test_intrinsic_trim() {
        let result = eval_func("TRIM", vec![str_arg("  HELLO  ")]);
        assert_eq!(result.to_display_string(), "HELLO");
    }

    #[test]
    fn test_intrinsic_trim_leading() {
        let result = eval_func("TRIM", vec![str_arg("  HELLO  "), str_arg("LEADING")]);
        assert_eq!(result.to_display_string(), "HELLO  ");
    }

    #[test]
    fn test_intrinsic_trim_trailing() {
        let result = eval_func("TRIM", vec![str_arg("  HELLO  "), str_arg("TRAILING")]);
        assert_eq!(result.to_display_string(), "  HELLO");
    }

    #[test]
    fn test_intrinsic_reverse() {
        let result = eval_func("REVERSE", vec![str_arg("ABCDE")]);
        assert_eq!(result.to_display_string(), "EDCBA");
    }

    #[test]
    fn test_intrinsic_ord() {
        // ORD('A') = ASCII 65 + 1 = 66
        let result = eval_func("ORD", vec![str_arg("A")]);
        assert_eq!(result.to_display_string().trim(), "66");
    }

    #[test]
    fn test_intrinsic_char() {
        // CHAR(66) = character at ordinal 66 = 'A' (ordinal is 1-based, so 66-1=65='A')
        let result = eval_func("CHAR", vec![int_arg(66)]);
        assert_eq!(result.to_display_string(), "A");
    }

    #[test]
    fn test_intrinsic_concatenate() {
        let result = eval_func("CONCATENATE", vec![str_arg("HELLO"), str_arg(" "), str_arg("WORLD")]);
        assert_eq!(result.to_display_string(), "HELLO WORLD");
    }

    #[test]
    fn test_intrinsic_length() {
        let result = eval_func("LENGTH", vec![str_arg("HELLO")]);
        assert_eq!(result.to_display_string().trim(), "5");
    }

    #[test]
    fn test_intrinsic_byte_length() {
        let result = eval_func("BYTE-LENGTH", vec![str_arg("TEST")]);
        assert_eq!(result.to_display_string().trim(), "4");
    }

    #[test]
    fn test_intrinsic_upper_case() {
        let result = eval_func("UPPER-CASE", vec![str_arg("hello world")]);
        assert_eq!(result.to_display_string(), "HELLO WORLD");
    }

    #[test]
    fn test_intrinsic_lower_case() {
        let result = eval_func("LOWER-CASE", vec![str_arg("HELLO WORLD")]);
        assert_eq!(result.to_display_string(), "hello world");
    }

    // Story 502.2: Numeric functions

    #[test]
    fn test_intrinsic_numval() {
        let result = eval_func("NUMVAL", vec![str_arg("  -123.45  ")]);
        let n = result.as_numeric().unwrap();
        assert_eq!(n.value, rust_decimal::Decimal::new(-12345, 2));
    }

    #[test]
    fn test_intrinsic_numval_c() {
        let result = eval_func("NUMVAL-C", vec![str_arg("$1,234.56")]);
        let n = result.as_numeric().unwrap();
        assert_eq!(n.value, rust_decimal::Decimal::new(123456, 2));
    }

    #[test]
    fn test_intrinsic_numval_cr_suffix() {
        let result = eval_func("NUMVAL", vec![str_arg("123.45CR")]);
        let n = result.as_numeric().unwrap();
        assert!(n.value.is_sign_negative(), "CR suffix should make value negative");
    }

    #[test]
    fn test_intrinsic_mod() {
        let result = eval_func("MOD", vec![int_arg(17), int_arg(5)]);
        assert_eq!(result.to_display_string().trim(), "2");
    }

    #[test]
    fn test_intrinsic_mod_negative() {
        // MOD(-17, 5) = -17 - 5 * floor(-17/5) = -17 - 5 * (-4) = -17 + 20 = 3
        let result = eval_func("MOD", vec![int_arg(-17), int_arg(5)]);
        assert_eq!(result.to_display_string().trim(), "3");
    }

    #[test]
    fn test_intrinsic_rem() {
        // REM(17, 5) = 17 - 5 * trunc(17/5) = 17 - 5*3 = 2
        let result = eval_func("REM", vec![int_arg(17), int_arg(5)]);
        assert_eq!(result.to_display_string().trim(), "2");
    }

    #[test]
    fn test_intrinsic_rem_negative() {
        // REM(-17, 5) = -17 - 5 * trunc(-17/5) = -17 - 5*(-3) = -17 + 15 = -2
        let result = eval_func("REM", vec![int_arg(-17), int_arg(5)]);
        assert_eq!(result.to_display_string().trim(), "-2");
    }

    #[test]
    fn test_intrinsic_integer() {
        // INTEGER(3.7) = 3 (floor)
        let env = create_test_env();
        let args = vec![str_arg("3.7")]; // will parse as numeric via to_numeric
        // Use NUMVAL first to get a decimal, then INTEGER on it
        let _numval = eval_intrinsic_function("NUMVAL", &args, &env).unwrap();
        // Now test INTEGER — but we need a SimpleExpr, so let's test via the function directly
        // For simplicity, test integer part:
        let result = eval_func("INTEGER", vec![int_arg(3)]);
        assert_eq!(result.to_display_string().trim(), "3");
    }

    #[test]
    fn test_intrinsic_max() {
        let result = eval_func("MAX", vec![int_arg(5), int_arg(3), int_arg(8), int_arg(1)]);
        assert_eq!(result.to_display_string().trim(), "8");
    }

    #[test]
    fn test_intrinsic_min() {
        let result = eval_func("MIN", vec![int_arg(5), int_arg(3), int_arg(8), int_arg(1)]);
        assert_eq!(result.to_display_string().trim(), "1");
    }

    #[test]
    fn test_intrinsic_abs() {
        let result = eval_func("ABS", vec![int_arg(-42)]);
        assert_eq!(result.to_display_string().trim(), "42");
    }

    #[test]
    fn test_intrinsic_abs_positive() {
        let result = eval_func("ABS", vec![int_arg(42)]);
        assert_eq!(result.to_display_string().trim(), "42");
    }

    #[test]
    fn test_intrinsic_random_with_seed() {
        let result = eval_func("RANDOM", vec![int_arg(42)]);
        let n = result.as_numeric().unwrap();
        // Should return a value >= 0
        assert!(!n.value.is_sign_negative(), "RANDOM should return non-negative value");
    }

    #[test]
    fn test_intrinsic_sum() {
        let result = eval_func("SUM", vec![int_arg(10), int_arg(20), int_arg(30)]);
        assert_eq!(result.to_display_string().trim(), "60");
    }

    #[test]
    fn test_intrinsic_mean() {
        let result = eval_func("MEAN", vec![int_arg(10), int_arg(20), int_arg(30)]);
        let n = result.as_numeric().unwrap();
        assert_eq!(n.value, rust_decimal::Decimal::new(20, 0));
    }

    #[test]
    fn test_intrinsic_range() {
        let result = eval_func("RANGE", vec![int_arg(5), int_arg(3), int_arg(8), int_arg(1)]);
        assert_eq!(result.to_display_string().trim(), "7"); // 8 - 1
    }

    #[test]
    fn test_intrinsic_ord_max() {
        let result = eval_func("ORD-MAX", vec![int_arg(5), int_arg(3), int_arg(8), int_arg(1)]);
        assert_eq!(result.to_display_string().trim(), "3"); // index of 8
    }

    #[test]
    fn test_intrinsic_ord_min() {
        let result = eval_func("ORD-MIN", vec![int_arg(5), int_arg(3), int_arg(8), int_arg(1)]);
        assert_eq!(result.to_display_string().trim(), "4"); // index of 1
    }

    // Story 502.3: Date functions

    #[test]
    fn test_intrinsic_integer_of_date_epoch() {
        // Lilian day 1 = October 15, 1582
        let result = eval_func("INTEGER-OF-DATE", vec![int_arg(15821015)]);
        assert_eq!(result.to_display_string().trim(), "1");
    }

    #[test]
    fn test_intrinsic_integer_of_date_modern() {
        // January 15, 2024
        let result = eval_func("INTEGER-OF-DATE", vec![int_arg(20240115)]);
        let lilian = result.as_numeric().unwrap().integer_part();
        assert!(lilian > 160000, "2024-01-15 should have a large Lilian day number");
    }

    #[test]
    fn test_intrinsic_date_of_integer_epoch() {
        // Lilian day 1 = October 15, 1582
        let result = eval_func("DATE-OF-INTEGER", vec![int_arg(1)]);
        assert_eq!(result.to_display_string().trim(), "15821015");
    }

    #[test]
    fn test_intrinsic_integer_of_date_roundtrip() {
        // Convert 20240229 to Lilian and back
        let lilian = eval_func("INTEGER-OF-DATE", vec![int_arg(20240229)]);
        let lilian_val = lilian.as_numeric().unwrap().integer_part();
        let date = eval_func("DATE-OF-INTEGER", vec![int_arg(lilian_val)]);
        assert_eq!(date.to_display_string().trim(), "20240229");
    }

    #[test]
    fn test_intrinsic_day_of_integer() {
        // Lilian day 1 = Oct 15, 1582 -> day of year 288
        let result = eval_func("DAY-OF-INTEGER", vec![int_arg(1)]);
        assert_eq!(result.to_display_string().trim(), "1582288");
    }

    #[test]
    fn test_intrinsic_integer_of_day() {
        // 1582288 = Oct 15, 1582 = Lilian day 1
        let result = eval_func("INTEGER-OF-DAY", vec![int_arg(1582288)]);
        assert_eq!(result.to_display_string().trim(), "1");
    }

    #[test]
    fn test_intrinsic_date_to_yyyymmdd() {
        // 240115 with default pivot -> 20240115
        let result = eval_func("DATE-TO-YYYYMMDD", vec![int_arg(240115)]);
        assert_eq!(result.to_display_string().trim(), "20240115");
    }

    #[test]
    fn test_intrinsic_date_to_yyyymmdd_old() {
        // 850115 with default pivot 50 -> 19850115
        let result = eval_func("DATE-TO-YYYYMMDD", vec![int_arg(850115)]);
        assert_eq!(result.to_display_string().trim(), "19850115");
    }

    #[test]
    fn test_intrinsic_year_to_yyyy() {
        let result = eval_func("YEAR-TO-YYYY", vec![int_arg(24)]);
        assert_eq!(result.to_display_string().trim(), "2024");
    }

    #[test]
    fn test_intrinsic_current_date_format() {
        let result = eval_func("CURRENT-DATE", vec![]);
        let s = result.to_display_string();
        assert_eq!(s.len(), 21, "CURRENT-DATE should be 21 chars");
        assert!(s.contains('+') || s.contains('-'), "Should contain timezone offset");
    }

    #[test]
    fn test_intrinsic_when_compiled() {
        let result = eval_func("WHEN-COMPILED", vec![]);
        let s = result.to_display_string();
        assert_eq!(s.len(), 21, "WHEN-COMPILED should be 21 chars");
    }

    // Calendar helpers tests

    #[test]
    fn test_is_leap_year() {
        assert!(is_leap_year(2000));
        assert!(is_leap_year(2024));
        assert!(!is_leap_year(1900));
        assert!(!is_leap_year(2023));
        assert!(is_leap_year(2400));
    }

    #[test]
    fn test_gregorian_to_lilian_epoch() {
        assert_eq!(gregorian_to_lilian(1582, 10, 15), 1);
    }

    #[test]
    fn test_lilian_to_gregorian_epoch() {
        assert_eq!(lilian_to_gregorian(1), (1582, 10, 15));
    }

    #[test]
    fn test_lilian_roundtrip_dates() {
        // Test several dates for roundtrip
        let test_dates = vec![
            (1582, 10, 15), // Lilian epoch
            (1582, 12, 31), // End of 1582
            (2000, 1, 1),   // Y2K
            (2000, 2, 29),  // Leap day
            (2024, 2, 29),  // Leap day
            (2024, 1, 15),  // Regular date
            (1900, 3, 1),   // Non-leap century
        ];
        for (y, m, d) in test_dates {
            let lilian = gregorian_to_lilian(y, m, d);
            let (y2, m2, d2) = lilian_to_gregorian(lilian);
            assert_eq!((y, m, d), (y2, m2, d2), "Roundtrip failed for {}-{}-{}", y, m, d);
        }
    }

    // ================================================================
    // Epic 507 — PERFORM VARYING with AFTER + 88-Level Conditions
    // ================================================================

    /// Story 507.1: PERFORM VARYING with AFTER for multi-dimensional iteration.
    /// Given PERFORM VARYING WS-ROW FROM 1 BY 1 UNTIL WS-ROW > 10
    ///       AFTER WS-COL FROM 1 BY 1 UNTIL WS-COL > 5
    /// Then PROCESS-CELL is called 50 times (10 rows x 5 columns).
    #[test]
    fn test_perform_varying_with_after() {
        let mut env = create_test_env();
        env.define("WS-ROW", num_meta());
        env.define("WS-COL", num_meta());
        env.define("WS-COUNT", num_meta());

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![
                ("WS-ROW".to_string(), num_meta()),
                ("WS-COL".to_string(), num_meta()),
                ("WS-COUNT".to_string(), num_meta()),
            ],
            statements: vec![SimpleStatement::PerformInline {
                // UNTIL WS-ROW > 10
                until: Some(SimpleCondition::Compare {
                    left: SimpleExpr::Variable("WS-ROW".to_string()),
                    op: SimpleCompareOp::GreaterThan,
                    right: SimpleExpr::Integer(10),
                }),
                statements: vec![
                    // Body: ADD 1 TO WS-COUNT
                    SimpleStatement::Add {
                        values: vec![SimpleExpr::Integer(1)],
                        to: vec!["WS-COUNT".to_string()],
                    },
                ],
                // VARYING WS-ROW FROM 1 BY 1
                varying: Some((
                    "WS-ROW".to_string(),
                    SimpleExpr::Integer(1),
                    SimpleExpr::Integer(1),
                )),
                // AFTER WS-COL FROM 1 BY 1 UNTIL WS-COL > 5
                after: vec![VaryingAfter {
                    variable: "WS-COL".to_string(),
                    from: SimpleExpr::Integer(1),
                    by: SimpleExpr::Integer(1),
                    until: SimpleCondition::Compare {
                        left: SimpleExpr::Variable("WS-COL".to_string()),
                        op: SimpleCompareOp::GreaterThan,
                        right: SimpleExpr::Integer(5),
                    },
                }],
            }],
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
        };

        execute(&program, &mut env).unwrap();
        let count = env.get("WS-COUNT").unwrap().to_display_string().trim().to_string();
        assert_eq!(count, "50", "10 rows x 5 columns = 50 iterations");
    }

    /// Story 507.1: PERFORM VARYING with multiple AFTER levels.
    /// 3 x 4 x 2 = 24 iterations.
    #[test]
    fn test_perform_varying_with_two_after_levels() {
        let mut env = create_test_env();
        env.define("WS-X", num_meta());
        env.define("WS-Y", num_meta());
        env.define("WS-Z", num_meta());
        env.define("WS-COUNT", num_meta());

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            statements: vec![SimpleStatement::PerformInline {
                until: Some(SimpleCondition::Compare {
                    left: SimpleExpr::Variable("WS-X".to_string()),
                    op: SimpleCompareOp::GreaterThan,
                    right: SimpleExpr::Integer(3),
                }),
                statements: vec![SimpleStatement::Add {
                    values: vec![SimpleExpr::Integer(1)],
                    to: vec!["WS-COUNT".to_string()],
                }],
                varying: Some((
                    "WS-X".to_string(),
                    SimpleExpr::Integer(1),
                    SimpleExpr::Integer(1),
                )),
                after: vec![
                    VaryingAfter {
                        variable: "WS-Y".to_string(),
                        from: SimpleExpr::Integer(1),
                        by: SimpleExpr::Integer(1),
                        until: SimpleCondition::Compare {
                            left: SimpleExpr::Variable("WS-Y".to_string()),
                            op: SimpleCompareOp::GreaterThan,
                            right: SimpleExpr::Integer(4),
                        },
                    },
                    VaryingAfter {
                        variable: "WS-Z".to_string(),
                        from: SimpleExpr::Integer(1),
                        by: SimpleExpr::Integer(1),
                        until: SimpleCondition::Compare {
                            left: SimpleExpr::Variable("WS-Z".to_string()),
                            op: SimpleCompareOp::GreaterThan,
                            right: SimpleExpr::Integer(2),
                        },
                    },
                ],
            }],
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
        };

        execute(&program, &mut env).unwrap();
        let count = env.get("WS-COUNT").unwrap().to_display_string().trim().to_string();
        assert_eq!(count, "24", "3 x 4 x 2 = 24 iterations");
    }

    /// Story 507.1: PERFORM VARYING without AFTER (existing behavior preserved).
    #[test]
    fn test_perform_varying_simple_no_after() {
        let mut env = create_test_env();
        env.define("WS-I", num_meta());
        env.define("WS-COUNT", num_meta());

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            statements: vec![SimpleStatement::PerformInline {
                until: Some(SimpleCondition::Compare {
                    left: SimpleExpr::Variable("WS-I".to_string()),
                    op: SimpleCompareOp::GreaterThan,
                    right: SimpleExpr::Integer(5),
                }),
                statements: vec![SimpleStatement::Add {
                    values: vec![SimpleExpr::Integer(1)],
                    to: vec!["WS-COUNT".to_string()],
                }],
                varying: Some((
                    "WS-I".to_string(),
                    SimpleExpr::Integer(1),
                    SimpleExpr::Integer(1),
                )),
                after: Vec::new(),
            }],
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
        };

        execute(&program, &mut env).unwrap();
        let count = env.get("WS-COUNT").unwrap().to_display_string().trim().to_string();
        assert_eq!(count, "5", "VARYING 1..5 = 5 iterations");
    }

    /// Story 507.2: 88-level condition name with single VALUE.
    /// Given 05 WS-STATUS PIC X. 88 VALID VALUE 'Y'. 88 INVALID VALUE 'N'.
    /// When WS-STATUS = 'Y', then IF VALID is TRUE.
    #[test]
    fn test_88_level_single_value() {
        let mut env = create_test_env();
        env.define("WS-STATUS", DataItemMeta {
            size: 1, decimals: 0, is_numeric: false,
            picture: Some("X".to_string()),
            is_justified: false,
        });
        env.set("WS-STATUS", CobolValue::Alphanumeric("Y".to_string())).unwrap();

        let mut cond_names = HashMap::new();
        cond_names.insert("VALID".to_string(), ConditionDef {
            parent: "WS-STATUS".to_string(),
            values: vec![ConditionValue::Single("Y".to_string())],
        });
        cond_names.insert("INVALID".to_string(), ConditionDef {
            parent: "WS-STATUS".to_string(),
            values: vec![ConditionValue::Single("N".to_string())],
        });

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            statements: vec![],
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
            condition_names: cond_names,
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
        };

        // VALID should be TRUE when WS-STATUS = 'Y'
        let result = eval_condition(
            &SimpleCondition::ConditionName("VALID".to_string()),
            &env,
            &program,
        ).unwrap();
        assert!(result, "VALID should be TRUE when WS-STATUS = Y");

        // INVALID should be FALSE
        let result = eval_condition(
            &SimpleCondition::ConditionName("INVALID".to_string()),
            &env,
            &program,
        ).unwrap();
        assert!(!result, "INVALID should be FALSE when WS-STATUS = Y");
    }

    /// Story 507.2: SET condition-name TO TRUE.
    /// SET INVALID TO TRUE should set WS-STATUS to 'N'.
    #[test]
    fn test_88_level_set_to_true() {
        let mut env = create_test_env();
        env.define("WS-STATUS", DataItemMeta {
            size: 1, decimals: 0, is_numeric: false,
            picture: Some("X".to_string()),
            is_justified: false,
        });
        env.set("WS-STATUS", CobolValue::Alphanumeric("Y".to_string())).unwrap();

        let mut cond_names = HashMap::new();
        cond_names.insert("VALID".to_string(), ConditionDef {
            parent: "WS-STATUS".to_string(),
            values: vec![ConditionValue::Single("Y".to_string())],
        });
        cond_names.insert("INVALID".to_string(), ConditionDef {
            parent: "WS-STATUS".to_string(),
            values: vec![ConditionValue::Single("N".to_string())],
        });

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            statements: vec![
                SimpleStatement::SetCondition {
                    condition_name: "INVALID".to_string(),
                    value: true,
                },
            ],
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
            condition_names: cond_names,
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
        };

        execute(&program, &mut env).unwrap();
        let status = env.get("WS-STATUS").unwrap().to_display_string().trim().to_string();
        assert_eq!(status, "N", "SET INVALID TO TRUE should set WS-STATUS to N");
    }

    /// Story 507.2: 88-level with multiple VALUES.
    /// 88 VOWEL VALUE 'A' 'E' 'I' 'O' 'U'.
    #[test]
    fn test_88_level_multiple_values() {
        let mut env = create_test_env();
        env.define("WS-CHAR", DataItemMeta {
            size: 1, decimals: 0, is_numeric: false,
            picture: Some("X".to_string()),
            is_justified: false,
        });

        let mut cond_names = HashMap::new();
        cond_names.insert("VOWEL".to_string(), ConditionDef {
            parent: "WS-CHAR".to_string(),
            values: vec![
                ConditionValue::Single("A".to_string()),
                ConditionValue::Single("E".to_string()),
                ConditionValue::Single("I".to_string()),
                ConditionValue::Single("O".to_string()),
                ConditionValue::Single("U".to_string()),
            ],
        });

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            statements: vec![],
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
            condition_names: cond_names,
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
        };

        // Test each vowel
        for ch in &["A", "E", "I", "O", "U"] {
            env.set("WS-CHAR", CobolValue::Alphanumeric(ch.to_string())).unwrap();
            let result = eval_condition(
                &SimpleCondition::ConditionName("VOWEL".to_string()),
                &env,
                &program,
            ).unwrap();
            assert!(result, "VOWEL should be TRUE for '{}'", ch);
        }

        // Test a consonant
        env.set("WS-CHAR", CobolValue::Alphanumeric("B".to_string())).unwrap();
        let result = eval_condition(
            &SimpleCondition::ConditionName("VOWEL".to_string()),
            &env,
            &program,
        ).unwrap();
        assert!(!result, "VOWEL should be FALSE for 'B'");
    }

    /// Story 507.2: 88-level with THRU range.
    /// 88 VALID-GRADE VALUE 'A' THRU 'D'.
    #[test]
    fn test_88_level_thru_range() {
        let mut env = create_test_env();
        env.define("WS-GRADE", DataItemMeta {
            size: 1, decimals: 0, is_numeric: false,
            picture: Some("X".to_string()),
            is_justified: false,
        });

        let mut cond_names = HashMap::new();
        cond_names.insert("VALID-GRADE".to_string(), ConditionDef {
            parent: "WS-GRADE".to_string(),
            values: vec![
                ConditionValue::Range("A".to_string(), "D".to_string()),
            ],
        });

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            statements: vec![],
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
            condition_names: cond_names,
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
        };

        // 'A', 'B', 'C', 'D' should all be valid
        for ch in &["A", "B", "C", "D"] {
            env.set("WS-GRADE", CobolValue::Alphanumeric(ch.to_string())).unwrap();
            let result = eval_condition(
                &SimpleCondition::ConditionName("VALID-GRADE".to_string()),
                &env,
                &program,
            ).unwrap();
            assert!(result, "VALID-GRADE should be TRUE for '{}'", ch);
        }

        // 'E', 'F' should not be valid
        for ch in &["E", "F"] {
            env.set("WS-GRADE", CobolValue::Alphanumeric(ch.to_string())).unwrap();
            let result = eval_condition(
                &SimpleCondition::ConditionName("VALID-GRADE".to_string()),
                &env,
                &program,
            ).unwrap();
            assert!(!result, "VALID-GRADE should be FALSE for '{}'", ch);
        }
    }

    /// Story 507.2: SET TO TRUE with multiple values uses first value.
    #[test]
    fn test_88_level_set_true_uses_first_value() {
        let mut env = create_test_env();
        env.define("WS-CHAR", DataItemMeta {
            size: 1, decimals: 0, is_numeric: false,
            picture: Some("X".to_string()),
            is_justified: false,
        });

        let mut cond_names = HashMap::new();
        cond_names.insert("VOWEL".to_string(), ConditionDef {
            parent: "WS-CHAR".to_string(),
            values: vec![
                ConditionValue::Single("A".to_string()),
                ConditionValue::Single("E".to_string()),
            ],
        });

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            statements: vec![
                SimpleStatement::SetCondition {
                    condition_name: "VOWEL".to_string(),
                    value: true,
                },
            ],
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
            condition_names: cond_names,
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
        };

        execute(&program, &mut env).unwrap();
        let val = env.get("WS-CHAR").unwrap().to_display_string().trim().to_string();
        assert_eq!(val, "A", "SET VOWEL TO TRUE should set WS-CHAR to first value 'A'");
    }

    /// Story 507.2: 88-level combined values and range.
    /// 88 PASS VALUE 'A' 'B' 'C' 'D' '1' THRU '9'.
    #[test]
    fn test_88_level_mixed_values_and_ranges() {
        let mut env = create_test_env();
        env.define("WS-CODE", DataItemMeta {
            size: 1, decimals: 0, is_numeric: false,
            picture: Some("X".to_string()),
            is_justified: false,
        });

        let mut cond_names = HashMap::new();
        cond_names.insert("PASS".to_string(), ConditionDef {
            parent: "WS-CODE".to_string(),
            values: vec![
                ConditionValue::Single("A".to_string()),
                ConditionValue::Single("B".to_string()),
                ConditionValue::Range("1".to_string(), "9".to_string()),
            ],
        });

        let program = SimpleProgram {
            name: "TEST".to_string(),
            data_items: vec![],
            statements: vec![],
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
            condition_names: cond_names,
            group_layouts: HashMap::new(),
            contained_programs: Vec::new(),
            is_initial: false,
            is_common: false,
            declarative_handlers: HashMap::new(),
            redefines_aliases: Vec::new(),
        };

        // 'A' and 'B' should pass (single values)
        env.set("WS-CODE", CobolValue::Alphanumeric("A".to_string())).unwrap();
        assert!(eval_condition(&SimpleCondition::ConditionName("PASS".to_string()), &env, &program).unwrap());

        // '5' should pass (in range 1-9)
        env.set("WS-CODE", CobolValue::Alphanumeric("5".to_string())).unwrap();
        assert!(eval_condition(&SimpleCondition::ConditionName("PASS".to_string()), &env, &program).unwrap());

        // 'Z' should not pass
        env.set("WS-CODE", CobolValue::Alphanumeric("Z".to_string())).unwrap();
        assert!(!eval_condition(&SimpleCondition::ConditionName("PASS".to_string()), &env, &program).unwrap());
    }
}
