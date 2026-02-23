//! # JES2 Job Entry Subsystem
//!
//! Implementation of the z/OS JES2 (Job Entry Subsystem 2) for the
//! OpenMainframe project.
//!
//! ## Features
//!
//! - **Job queue** — priority-based scheduling with class selection
//! - **Job state machine** — Input → Conversion → Ready → Running → Output → Purge
//! - **Job classes** — configurable A-Z, 0-9 plus STC/TSU pseudo-classes
//! - **Spool management** — SYSOUT storage and retrieval
//! - **Operator commands** — $S, $P, $A, $C, $D for job/system management
//!
//! ## Example
//!
//! ```rust
//! use open_mainframe_jes2::Jes2;
//!
//! let mut jes = Jes2::new();
//! let job_id = jes.submit("JOB1", 'A', 15, false);
//! assert_eq!(jes.job_count(), 1);
//! ```

pub mod checkpoint;
pub mod commands;
pub mod config;
pub mod error;
pub mod exit;
pub mod initiator;
pub mod intrdr;
pub mod jecl;
pub mod job;
pub mod output;
pub mod queue;
pub mod spool;

pub use checkpoint::{CheckpointConfig, CheckpointData, CheckpointManager, StartMode};
pub use commands::{execute_command, parse_command, CommandResponse, Initiator, Jes2Command};
pub use config::{apply_parms, build_initiators, parse_jes2parm, Jes2Config, Jes2Parm};
pub use error::Jes2Error;
pub use exit::{
    ExitAction, ExitConfig, ExitContext, ExitDispatcher, ExitDisplayResult, ExitNumber, ExitParm,
    Jes2Exit, exit_name, parse_exit_parm, EXIT1, EXIT2, EXIT5, EXIT6, EXIT7, EXIT15, EXIT44,
};
pub use output::{
    parse_destination, Destination, JesDatasets, OutputDescriptor, OutputDisposition,
    OutputGroup, OutputProcessor,
};
pub use initiator::{InitiatorManager, InitiatorState, ManagedInitiator};
pub use intrdr::{
    sdsf_browse_output, sdsf_display_active, sdsf_header, sdsf_input_queue, sdsf_line_action,
    sdsf_output_queue, sdsf_status, InternalReader, SdsfLineCommand, SdsfPanel, SdsfRow,
};
pub use jecl::{parse_jecl, JeclStmt, JobparmParams, OutputParams, SetupParams};
pub use job::{Job, JobClass, JobId, JobState, TypeRun};
pub use queue::Jes2;
pub use spool::SpoolManager;

/// Convenience result type for JES2 operations.
pub type Result<T> = std::result::Result<T, Jes2Error>;
