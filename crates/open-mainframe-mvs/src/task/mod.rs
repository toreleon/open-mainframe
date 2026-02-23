//! Task management — TCB representation, ABEND processing, and subtask management.

pub mod abend;
pub mod attach;
pub mod tcb;

pub use abend::{AbendCode, abend, abend_with_reason};
pub use attach::{AttachToken, attach, complete_subtask, detach};
pub use tcb::{TaskState, Tcb};
