//! AST types for JCL (Job Control Language).
//!
//! This module defines the abstract syntax tree for JCL statements.
//! JCL is used to describe jobs, steps, and data definitions.

use std::collections::HashMap;

use open_mainframe_lang_core::{AstNode, Span};

/// Symbol table for JCL symbolic parameter resolution.
///
/// Maps symbolic parameter names (without the leading `&`) to their values.
/// Populated in priority order: PROC defaults → SET statements → EXEC overrides.
pub type SymbolTable = HashMap<String, String>;

/// An in-stream procedure defined between PROC and PEND statements.
#[derive(Debug, Clone)]
pub struct InStreamProc {
    /// Procedure name (from the PROC statement name field).
    pub name: String,
    /// Default symbolic parameter values declared on the PROC statement.
    pub defaults: SymbolTable,
    /// The raw JCL statements within the procedure body (between PROC and PEND).
    pub statements: Vec<ProcStatement>,
}

/// A statement within an in-stream procedure body.
///
/// Stores the raw statement data so it can be re-parsed after symbolic substitution.
#[derive(Debug, Clone)]
pub struct ProcStatement {
    /// Step name (if any).
    pub name: Option<String>,
    /// Operation (EXEC, DD, etc.).
    pub operation: String,
    /// Raw operands string (with symbolic parameters still present).
    pub operands: String,
}

/// An entry in a job — either a step or an IF/THEN/ELSE/ENDIF construct.
#[derive(Debug, Clone)]
pub enum JobEntry {
    /// A regular job step (EXEC + DDs).
    Step(Box<Step>),
    /// An IF/THEN/ELSE/ENDIF conditional construct.
    If(IfConstruct),
}

/// IF/THEN/ELSE/ENDIF conditional construct.
///
/// Controls which steps execute based on return codes and abend conditions
/// from previous steps. Supports nesting up to 15 levels.
#[derive(Debug, Clone)]
pub struct IfConstruct {
    /// The condition expression to evaluate.
    pub condition: ConditionExpr,
    /// Steps to execute if the condition is true (THEN branch).
    pub then_entries: Vec<JobEntry>,
    /// Steps to execute if the condition is false (ELSE branch, optional).
    pub else_entries: Vec<JobEntry>,
    /// Source span of the entire IF/THEN/ELSE/ENDIF block.
    pub span: Span,
}

/// A condition expression for IF statement evaluation.
#[derive(Debug, Clone)]
pub enum ConditionExpr {
    /// Compare a step's return code to a value: `STEP1.RC = 0`
    RcCompare {
        /// Step name to compare.
        step_name: String,
        /// Comparison operator.
        operator: ConditionOperator,
        /// Return code value to compare against.
        value: u32,
    },
    /// Check if a step abended: `STEP1.ABEND`
    Abend {
        /// Step name to check.
        step_name: String,
    },
    /// Check a step's abend completion code: `STEP1.ABENDCC = Sxxx`
    AbendCc {
        /// Step name to check.
        step_name: String,
        /// Comparison operator.
        operator: ConditionOperator,
        /// Abend code value.
        value: String,
    },
    /// Check if a step ran (was not bypassed): `STEP1.RUN`
    Run {
        /// Step name to check.
        step_name: String,
    },
    /// Logical NOT of an expression: `NOT (expr)`
    Not(Box<ConditionExpr>),
    /// Logical AND of expressions: `expr & expr`
    And(Vec<ConditionExpr>),
    /// Logical OR of expressions: `expr | expr`
    Or(Vec<ConditionExpr>),
}

/// A complete JCL job.
#[derive(Debug, Clone)]
pub struct Job {
    /// Job name (1-8 characters).
    pub name: String,
    /// Job parameters.
    pub params: JobParams,
    /// Entries in the job (steps and IF constructs).
    pub entries: Vec<JobEntry>,
    /// Source span covering the entire job.
    pub span: Span,
    /// Symbol table populated from SET statements.
    pub symbols: SymbolTable,
    /// In-stream procedures defined in this job (PROC...PEND blocks).
    pub in_stream_procs: HashMap<String, InStreamProc>,
    /// JCLLIB ORDER — procedure library search order (dataset names).
    pub jcllib_order: Vec<String>,
    /// DD overrides for procedure steps (stepname.ddname → DdStatement).
    pub dd_overrides: Vec<DdOverride>,
    /// OUTPUT statements defined in this job.
    pub output_stmts: Vec<OutputStatement>,
}

/// A DD override for a procedure step.
///
/// When a calling JCL specifies `//STEP1.INPUT DD DSN=OVERRIDE.DATA`,
/// this overrides or adds the INPUT DD on STEP1 within the procedure.
#[derive(Debug, Clone)]
pub struct DdOverride {
    /// The procedure step name to apply the override to.
    pub step_name: String,
    /// The DD statement (name is the DD name, definition is the override).
    pub dd: DdStatement,
}

impl AstNode for Job {
    fn span(&self) -> Span {
        self.span
    }
}

/// Job-level parameters (from JOB statement).
#[derive(Debug, Clone, Default)]
pub struct JobParams {
    /// Accounting information.
    pub accounting: Option<String>,
    /// Programmer name.
    pub programmer: Option<String>,
    /// Job class.
    pub class: Option<char>,
    /// Message class for output.
    pub msgclass: Option<char>,
    /// Message level (statements, allocation messages).
    pub msglevel: Option<(u8, u8)>,
    /// Notify user on completion.
    pub notify: Option<String>,
    /// Region size in KB.
    pub region: Option<u32>,
    /// Time limit (minutes, seconds).
    pub time: Option<(u32, u32)>,
    /// TYPRUN — Job type (SCAN, HOLD, etc.).
    pub typrun: Option<TypeRun>,
    /// MEMLIMIT — Memory limit (e.g., "2G", "512M").
    pub memlimit: Option<String>,
    /// JOBRC — Job return code behavior.
    pub jobrc: Option<String>,
    /// SCHENV — Scheduling environment name.
    pub schenv: Option<String>,
    /// Additional parameters.
    pub other: HashMap<String, String>,
}

/// TYPRUN parameter values.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeRun {
    /// Normal execution (default).
    Run,
    /// Hold the job without executing.
    Hold,
    /// Syntax check only (no execution).
    Scan,
    /// Copy job to internal reader.
    Copy,
}

/// A job step (EXEC statement with associated DDs).
#[derive(Debug, Clone)]
pub struct Step {
    /// Step name (optional, 1-8 characters).
    pub name: Option<String>,
    /// Program or procedure to execute.
    pub exec: ExecType,
    /// EXEC parameters.
    pub params: ExecParams,
    /// DD statements for this step.
    pub dd_statements: Vec<DdStatement>,
    /// Source span covering from EXEC through last DD.
    pub span: Span,
}

/// Type of EXEC statement.
#[derive(Debug, Clone)]
pub enum ExecType {
    /// Execute a program (PGM=name).
    Program(String),
    /// Execute a cataloged procedure (PROC=name).
    Procedure(String),
}

/// Parameters for EXEC statement.
#[derive(Debug, Clone, Default)]
pub struct ExecParams {
    /// Region size for this step.
    pub region: Option<u32>,
    /// Time limit for this step.
    pub time: Option<(u32, u32)>,
    /// Condition codes for step execution.
    pub cond: Option<Vec<Condition>>,
    /// Special COND mode (EVEN or ONLY).
    pub cond_special: Option<CondSpecial>,
    /// PARM value passed to program.
    pub parm: Option<String>,
    /// Additional parameters.
    pub other: HashMap<String, String>,
}

/// Condition for conditional step execution.
#[derive(Debug, Clone)]
pub struct Condition {
    /// Return code to compare.
    pub code: u32,
    /// Comparison operator.
    pub operator: ConditionOperator,
    /// Step name (if comparing to specific step).
    pub step: Option<String>,
    /// Procedure step name (for step.procstep qualification).
    pub procstep: Option<String>,
}

/// Special COND execution modes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CondSpecial {
    /// Execute even if a previous step abended.
    Even,
    /// Execute only if a previous step abended.
    Only,
}

/// Operators for COND parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConditionOperator {
    /// Greater than.
    Gt,
    /// Greater than or equal.
    Ge,
    /// Equal.
    Eq,
    /// Not equal.
    Ne,
    /// Less than.
    Lt,
    /// Less than or equal.
    Le,
}

/// DD (Data Definition) statement.
#[derive(Debug, Clone)]
pub struct DdStatement {
    /// DD name (1-8 characters).
    pub name: String,
    /// Data definition type.
    pub definition: DdDefinition,
    /// Source span of the DD statement.
    pub span: Span,
}

/// Types of data definitions.
#[derive(Debug, Clone)]
pub enum DdDefinition {
    /// Reference to a dataset (DSN=).
    Dataset(Box<DatasetDef>),
    /// Inline data (DD *).
    Inline(InlineDef),
    /// Concatenation of datasets.
    Concatenation(Vec<DatasetDef>),
    /// Dummy dataset (no actual I/O).
    Dummy,
    /// Sysout (output class).
    Sysout(SysoutDef),
    /// USS file (PATH=).
    UssFile(UssFileDef),
}

/// Dataset definition parameters.
#[derive(Debug, Clone, Default)]
pub struct DatasetDef {
    /// Dataset name.
    pub dsn: String,
    /// Disposition (status, normal, abnormal).
    pub disp: Option<Disposition>,
    /// Unit type (SYSDA, TAPE, etc.).
    pub unit: Option<String>,
    /// Volume serial number.
    pub vol_ser: Option<String>,
    /// Space allocation.
    pub space: Option<Space>,
    /// Data Control Block parameters.
    pub dcb: Option<DcbParams>,
    /// Access Method Parameters (for VSAM).
    pub amp: Option<AmpParams>,
    // -- Epic 104: Extended DD parameters --
    /// SMS storage class (STORCLAS=).
    pub storclas: Option<String>,
    /// SMS data class (DATACLAS=).
    pub dataclas: Option<String>,
    /// SMS management class (MGMTCLAS=).
    pub mgmtclas: Option<String>,
    /// Tape label specification.
    pub label: Option<LabelDef>,
    /// Expiration date (EXPDT=yyyy/ddd or EXPDT=yyddd).
    pub expdt: Option<String>,
    /// Retention period in days (RETPD=nnn).
    pub retpd: Option<u32>,
    /// Dataset type (DSNTYPE=LIBRARY, PDS, etc.).
    pub dsntype: Option<DsType>,
    /// Model dataset name (LIKE=model.dsn).
    pub like: Option<String>,
    /// Reference DD name (REFDD=stepname.ddname).
    pub refdd: Option<String>,
    /// Key length for VSAM KSDS (KEYLEN=n).
    pub keylen: Option<u32>,
    /// Key offset for VSAM KSDS (KEYOFF=n).
    pub keyoff: Option<u32>,
    /// Average record count unit (AVGREC=U|K|M).
    pub avgrec: Option<String>,
    /// GDG relative generation number (+1, 0, -1, etc.).
    pub gdg_generation: Option<i32>,
}

/// Tape label specification.
#[derive(Debug, Clone)]
pub struct LabelDef {
    /// Data file sequence number on tape (1-based).
    pub sequence: u32,
    /// Label type.
    pub label_type: LabelType,
    /// Password protection indicator.
    pub password: Option<String>,
    /// IN/OUT indicator.
    pub in_out: Option<String>,
    /// Expiration date (from LABEL parameter).
    pub expdt: Option<String>,
}

/// Tape label types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LabelType {
    /// Standard labels (IBM default).
    Sl,
    /// Non-standard labels.
    Nsl,
    /// No labels.
    Nl,
    /// Standard user labels.
    Sul,
    /// Bypass label processing.
    Blp,
    /// ANSI labels.
    Al,
    /// ANSI user labels.
    Aul,
}

/// Dataset type values for DSNTYPE parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DsType {
    /// PDS (Partitioned Data Set).
    Pds,
    /// PDSE / LIBRARY.
    Library,
    /// Large format sequential.
    Large,
    /// Extended format sequential.
    Extreq,
    /// Extended preferred sequential.
    Extpref,
    /// Basic format.
    Basic,
    /// HFS (Hierarchical File System).
    Hfs,
    /// Pipe.
    Pipe,
}

/// USS (Unix System Services) file definition.
///
/// Used when DD specifies PATH= instead of DSN=.
#[derive(Debug, Clone)]
pub struct UssFileDef {
    /// USS file path (e.g., /u/user/file.txt).
    pub path: String,
    /// Path options (ORDONLY, ORDWR, OWRONLY, OCREAT, OEXCL, OTRUNC, OAPPEND).
    pub pathopts: Vec<String>,
    /// Path mode (SIRWXU, SIRUSR, SIWUSR, etc.).
    pub pathmode: Vec<String>,
    /// Path disposition: what to do on normal/abnormal termination.
    pub pathdisp: Option<(String, Option<String>)>,
}

/// OUTPUT JCL statement for SYSOUT processing control.
#[derive(Debug, Clone)]
pub struct OutputStatement {
    /// Output statement name (1-8 characters).
    pub name: String,
    /// Output class.
    pub class: Option<char>,
    /// Destination (printer, node, etc.).
    pub dest: Option<String>,
    /// Number of copies.
    pub copies: Option<u32>,
    /// Form name.
    pub forms: Option<String>,
    /// Writer name.
    pub writer: Option<String>,
    /// Additional parameters.
    pub other: HashMap<String, String>,
}

/// Dataset catalog trait for GDG resolution.
///
/// Resolves GDG relative generation references to absolute dataset names.
pub trait DatasetCatalog {
    /// Resolve a GDG relative reference to an absolute dataset name.
    ///
    /// `base` is the GDG base name (e.g., "MY.GDG") and `generation` is the
    /// relative generation number (+1 for new, 0 for current, -1 for previous, etc.).
    fn resolve_gdg(&self, base: &str, generation: i32) -> Result<String, String>;
}

/// VSAM Access Method Parameters.
#[derive(Debug, Clone, Default)]
pub struct AmpParams {
    /// Data buffer count.
    pub bufnd: Option<u32>,
    /// Index buffer count.
    pub bufni: Option<u32>,
    /// Buffer space in bytes.
    pub bufsp: Option<u32>,
    /// String count (concurrent requests).
    pub strno: Option<u32>,
    /// Access mode (SEQ, DIR, SKP).
    pub mode: Option<VsamAccessMode>,
}

/// VSAM access modes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VsamAccessMode {
    /// Sequential access.
    Sequential,
    /// Direct (keyed) access.
    Direct,
    /// Skip sequential access.
    Skip,
}

/// Dataset disposition.
#[derive(Debug, Clone)]
pub struct Disposition {
    /// Status (NEW, OLD, SHR, MOD).
    pub status: DispStatus,
    /// Normal disposition (KEEP, DELETE, CATLG, UNCATLG, PASS).
    pub normal: Option<DispAction>,
    /// Abnormal disposition.
    pub abnormal: Option<DispAction>,
}

/// Dataset status values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum DispStatus {
    /// New dataset being created.
    New,
    /// Existing dataset, exclusive access.
    Old,
    /// Existing dataset, shared access.
    #[default]
    Shr,
    /// Existing dataset, append mode.
    Mod,
}

/// Disposition actions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DispAction {
    /// Keep the dataset.
    Keep,
    /// Delete the dataset.
    Delete,
    /// Catalog the dataset.
    Catlg,
    /// Uncatalog the dataset.
    Uncatlg,
    /// Pass to next step.
    Pass,
}

/// Space allocation.
#[derive(Debug, Clone)]
pub struct Space {
    /// Allocation unit (TRK, CYL, or block size).
    pub unit: SpaceUnit,
    /// Primary allocation.
    pub primary: u32,
    /// Secondary allocation.
    pub secondary: Option<u32>,
    /// Directory blocks (for PDS).
    pub directory: Option<u32>,
}

/// Space allocation unit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpaceUnit {
    /// Tracks.
    Trk,
    /// Cylinders.
    Cyl,
    /// Block size in bytes.
    Blk(u32),
}

/// DCB (Data Control Block) parameters.
#[derive(Debug, Clone, Default)]
pub struct DcbParams {
    /// Record format (F, FB, V, VB, U).
    pub recfm: Option<RecordFormat>,
    /// Logical record length.
    pub lrecl: Option<u32>,
    /// Block size.
    pub blksize: Option<u32>,
    /// Dataset organization.
    pub dsorg: Option<DatasetOrg>,
}

/// Record format.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecordFormat {
    /// Fixed length.
    Fixed,
    /// Fixed blocked.
    FixedBlocked,
    /// Variable length.
    Variable,
    /// Variable blocked.
    VariableBlocked,
    /// Undefined.
    Undefined,
}

/// Dataset organization.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DatasetOrg {
    /// Physical sequential.
    Ps,
    /// Partitioned (PDS).
    Po,
    /// Direct access.
    Da,
}

/// Inline data definition.
#[derive(Debug, Clone)]
pub struct InlineDef {
    /// Delimiter (default is /*).
    pub delimiter: Option<String>,
    /// The inline data lines.
    pub data: Vec<String>,
}

/// Sysout definition.
#[derive(Debug, Clone)]
pub struct SysoutDef {
    /// Output class (A-Z, 0-9, *).
    pub class: char,
    /// Writer name (optional).
    pub writer: Option<String>,
    /// Form name (optional).
    pub form: Option<String>,
}

impl Job {
    /// Create a new job with the given name.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            params: JobParams::default(),
            entries: Vec::new(),
            span: Span::dummy(),
            symbols: SymbolTable::new(),
            in_stream_procs: HashMap::new(),
            jcllib_order: Vec::new(),
            dd_overrides: Vec::new(),
            output_stmts: Vec::new(),
        }
    }

    /// Add a step to the job.
    pub fn add_step(&mut self, step: Step) {
        self.entries.push(JobEntry::Step(Box::new(step)));
    }

    /// Add an entry (step or IF construct) to the job.
    pub fn add_entry(&mut self, entry: JobEntry) {
        self.entries.push(entry);
    }

    /// Return a flat list of all steps (for backwards compatibility).
    ///
    /// Flattens IF/THEN/ELSE constructs into a single list containing
    /// all steps from all branches. Useful for procedure expansion.
    pub fn steps(&self) -> Vec<&Step> {
        fn collect_steps<'a>(entries: &'a [JobEntry], out: &mut Vec<&'a Step>) {
            for entry in entries {
                match entry {
                    JobEntry::Step(step) => out.push(step.as_ref()),
                    JobEntry::If(if_construct) => {
                        collect_steps(&if_construct.then_entries, out);
                        collect_steps(&if_construct.else_entries, out);
                    }
                }
            }
        }
        let mut steps = Vec::new();
        collect_steps(&self.entries, &mut steps);
        steps
    }
}

impl Step {
    /// Create a new step executing a program.
    pub fn program(name: Option<String>, pgm: impl Into<String>) -> Self {
        Self {
            name,
            exec: ExecType::Program(pgm.into()),
            params: ExecParams::default(),
            dd_statements: Vec::new(),
            span: Span::dummy(),
        }
    }

    /// Create a new step executing a procedure.
    pub fn procedure(name: Option<String>, proc: impl Into<String>) -> Self {
        Self {
            name,
            exec: ExecType::Procedure(proc.into()),
            params: ExecParams::default(),
            dd_statements: Vec::new(),
            span: Span::dummy(),
        }
    }

    /// Add a DD statement to this step.
    pub fn add_dd(&mut self, dd: DdStatement) {
        self.dd_statements.push(dd);
    }
}

impl DdStatement {
    /// Create a dataset DD.
    pub fn dataset(name: impl Into<String>, dsn: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            definition: DdDefinition::Dataset(Box::new(DatasetDef {
                dsn: dsn.into(),
                ..Default::default()
            })),
            span: Span::dummy(),
        }
    }

    /// Create an inline data DD.
    pub fn inline(name: impl Into<String>, data: Vec<String>) -> Self {
        Self {
            name: name.into(),
            definition: DdDefinition::Inline(InlineDef {
                delimiter: None,
                data,
            }),
            span: Span::dummy(),
        }
    }

    /// Create a SYSOUT DD.
    pub fn sysout(name: impl Into<String>, class: char) -> Self {
        Self {
            name: name.into(),
            definition: DdDefinition::Sysout(SysoutDef {
                class,
                writer: None,
                form: None,
            }),
            span: Span::dummy(),
        }
    }

    /// Create a DUMMY DD.
    pub fn dummy(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            definition: DdDefinition::Dummy,
            span: Span::dummy(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_job() {
        let mut job = Job::new("TESTJOB");
        assert_eq!(job.name, "TESTJOB");
        assert!(job.entries.is_empty());

        let mut step = Step::program(Some("STEP1".to_string()), "HELLO");
        step.add_dd(DdStatement::sysout("SYSOUT", '*'));
        job.add_step(step);

        assert_eq!(job.entries.len(), 1);
        assert_eq!(job.steps().len(), 1);
    }

    #[test]
    fn test_create_dd_statements() {
        let ds_dd = DdStatement::dataset("INPUT", "MY.DATA.SET");
        if let DdDefinition::Dataset(ref def) = ds_dd.definition {
            assert_eq!(def.dsn, "MY.DATA.SET");
        } else {
            panic!("Expected Dataset definition");
        }

        let inline_dd = DdStatement::inline("SYSIN", vec!["DATA LINE 1".to_string()]);
        if let DdDefinition::Inline(ref def) = inline_dd.definition {
            assert_eq!(def.data.len(), 1);
        } else {
            panic!("Expected Inline definition");
        }

        let sysout_dd = DdStatement::sysout("SYSPRINT", 'A');
        if let DdDefinition::Sysout(ref def) = sysout_dd.definition {
            assert_eq!(def.class, 'A');
        } else {
            panic!("Expected Sysout definition");
        }

        let dummy_dd = DdStatement::dummy("NULLDD");
        assert!(matches!(dummy_dd.definition, DdDefinition::Dummy));
    }
}
