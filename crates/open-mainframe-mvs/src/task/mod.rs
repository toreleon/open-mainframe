//! Task management — TCB representation and ABEND processing.

pub mod abend;
pub mod tcb;

pub use abend::{AbendCode, abend, abend_with_reason};
pub use tcb::{TaskState, Tcb};
