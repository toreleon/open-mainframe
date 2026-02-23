//! Program management — LINK/XCTL/LOAD/DELETE and search order.
//!
//! Full implementation in SYS-101 epic.

pub mod link;
pub mod load;
pub mod search;

pub use link::{link, xctl};
pub use load::{LoadedModule, ModuleManager};
pub use search::ProgramSearchOrder;
