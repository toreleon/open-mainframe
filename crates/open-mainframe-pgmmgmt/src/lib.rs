//! # z/OS Program Management
//!
//! Implementation of the MVS program management subsystem for the OpenMainframe project.
//!
//! ## Features
//!
//! - **Binder** — symbol resolution, relocation, load module production
//! - **Program Search** — STEPLIB → JOBLIB → LPA → LNKLST hierarchy
//! - **LOAD/DELETE** — bring modules into/release from virtual storage
//! - **LINK/XCTL** — program invocation with/without return
//! - **ATTACH** — subtask creation
//! - **APF Authorization** — authorized program facility

pub mod binder;
pub mod program;

pub use binder::{
    AdconType, Binder, BinderError, EsdEntry, EsdType, LoadModule, ObjectModule, ResolvedSymbol,
    RldEntry, TextRecord,
};

pub use program::{
    Amode, ApfList, ExecutionResult, LoadedProgram, ProgramError, ProgramLibrary, ProgramManager,
    Rmode, SearchPathType, Tcb,
};
