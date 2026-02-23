//! # z/OS System Commands & Console
//!
//! Implementation of MVS system commands for the OpenMainframe project.
//!
//! ## Features
//!
//! - **Command Dispatcher** — parse and route system commands
//! - **DISPLAY commands** — D A,L / D J / D T / D M
//! - **START/STOP/MODIFY** — address space lifecycle
//! - **CANCEL/FORCE** — job cancellation
//! - **REPLY** — WTOR reply handling
//! - **JES2 routing** — $D, $S, $P, $C, $A, $T commands

pub mod commands;
pub mod sdsf;

pub use commands::{
    AddressSpace, AsidStatus, CancelOptions, CommandDispatcher, CommandOutput, CommandRegistry,
    DisplayResult, JobInfo, MemoryInfo, ModifyParams, ReplyResult, StartParams, StorageArea,
    SystemCommand, SystemTime, WtorEntry,
};

pub use sdsf::{
    JobStatus, JobType, LineCommand, LineCommandResult, LogEntry, PanelRow, PanelType,
    RenderedPanel, SdsfEngine, SdsfJob, SortField, SysoutDataset,
};
