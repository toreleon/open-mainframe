//! # MVS System Services
//!
//! Foundational z/OS system services (SVCs) for the OpenMainframe project.
//!
//! ## Services
//!
//! - **DYNALLOC** — Dynamic allocation (SVC 99) for dataset DD mapping
//! - **Console** — WTO/WTOR operator messaging
//! - **Sync** — ECB wait/post and ENQ/DEQ resource serialization
//! - **Storage** — GETMAIN/FREEMAIN subpool storage management
//! - **Task** — TCB representation and ABEND processing
//! - **Timer** — TIME macro for z/OS time formats
//! - **Recovery** — ESTAE/ESPIE recovery exit management
//! - **Program** — LINK/XCTL/LOAD/DELETE/ATTACH/DETACH

#![forbid(unsafe_code)]

pub mod console;
pub mod dynalloc;
pub mod error;
pub mod program;
pub mod recovery;
pub mod storage;
pub mod sync;
pub mod task;
pub mod timer;

pub use error::{MvsError, Result};
