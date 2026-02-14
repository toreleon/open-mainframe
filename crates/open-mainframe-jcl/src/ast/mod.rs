//! AST types for JCL (Job Control Language).
//!
//! This module defines the abstract syntax tree for JCL statements.
//! JCL is used to describe jobs, steps, and data definitions.

use std::collections::HashMap;

use open_mainframe_lang_core::{AstNode, Span};

/// A complete JCL job.
#[derive(Debug, Clone)]
pub struct Job {
    /// Job name (1-8 characters).
    pub name: String,
    /// Job parameters.
    pub params: JobParams,
    /// Steps in the job.
    pub steps: Vec<Step>,
    /// Source span covering the entire job.
    pub span: Span,
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
    /// Additional parameters.
    pub other: HashMap<String, String>,
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
    Dataset(DatasetDef),
    /// Inline data (DD *).
    Inline(InlineDef),
    /// Concatenation of datasets.
    Concatenation(Vec<DatasetDef>),
    /// Dummy dataset (no actual I/O).
    Dummy,
    /// Sysout (output class).
    Sysout(SysoutDef),
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
            steps: Vec::new(),
            span: Span::dummy(),
        }
    }

    /// Add a step to the job.
    pub fn add_step(&mut self, step: Step) {
        self.steps.push(step);
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
            definition: DdDefinition::Dataset(DatasetDef {
                dsn: dsn.into(),
                ..Default::default()
            }),
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
        assert!(job.steps.is_empty());

        let mut step = Step::program(Some("STEP1".to_string()), "HELLO");
        step.add_dd(DdStatement::sysout("SYSOUT", '*'));
        job.add_step(step);

        assert_eq!(job.steps.len(), 1);
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
