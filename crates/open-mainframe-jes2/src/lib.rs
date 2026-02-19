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
pub mod job;
pub mod queue;
pub mod spool;

pub use checkpoint::{CheckpointConfig, CheckpointData, CheckpointManager, StartMode};
pub use commands::{execute_command, parse_command, CommandResponse, Initiator, Jes2Command};
pub use config::{apply_parms, build_initiators, parse_jes2parm, Jes2Config, Jes2Parm};
pub use error::Jes2Error;
pub use job::{Job, JobClass, JobId, JobState};
pub use queue::Jes2;
pub use spool::SpoolManager;

/// Convenience result type for JES2 operations.
pub type Result<T> = std::result::Result<T, Jes2Error>;
