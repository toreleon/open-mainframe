//! Task Control Block — represents a dispatchable unit of work.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use serde::{Deserialize, Serialize};
use tokio::sync::RwLock;

use super::AbendCode;
use crate::dynalloc::DdTable;
use crate::storage::SubpoolManager;

/// Global TCB ID counter.
static NEXT_TCB_ID: AtomicU64 = AtomicU64::new(1);

/// Task state enumeration.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TaskState {
    /// Task is ready to be dispatched.
    Ready,
    /// Task is currently executing.
    Running,
    /// Task is suspended waiting on an ECB or timer.
    Waiting,
    /// Task has terminated (normal or ABEND).
    Terminated,
}

/// Task Control Block — the fundamental z/OS task representation.
#[derive(Debug)]
pub struct Tcb {
    /// Unique task identifier.
    pub id: u64,
    /// Parent task ID (None for job-step task).
    pub parent: Option<u64>,
    /// Current task state.
    pub state: TaskState,
    /// Name of the currently executing program.
    pub program_name: String,
    /// ABEND code if the task terminated abnormally.
    pub abend_code: Option<AbendCode>,
    /// Reason code associated with ABEND.
    pub reason_code: u32,
    /// DD table shared within the address space.
    pub dd_table: Arc<RwLock<DdTable>>,
    /// Storage subpool manager for this task.
    pub storage: SubpoolManager,
    /// Child task IDs.
    pub children: Vec<u64>,
}

impl Tcb {
    /// Create a new TCB for a program.
    pub fn new(program_name: &str, dd_table: Arc<RwLock<DdTable>>) -> Self {
        Self {
            id: NEXT_TCB_ID.fetch_add(1, Ordering::Relaxed),
            parent: None,
            state: TaskState::Ready,
            program_name: program_name.to_string(),
            abend_code: None,
            reason_code: 0,
            dd_table,
            storage: SubpoolManager::new(),
            children: Vec::new(),
        }
    }

    /// Create a child TCB inheriting the parent's DD table.
    pub fn new_child(
        parent_id: u64,
        program_name: &str,
        dd_table: Arc<RwLock<DdTable>>,
    ) -> Self {
        Self {
            id: NEXT_TCB_ID.fetch_add(1, Ordering::Relaxed),
            parent: Some(parent_id),
            state: TaskState::Ready,
            program_name: program_name.to_string(),
            abend_code: None,
            reason_code: 0,
            dd_table,
            storage: SubpoolManager::new(),
            children: Vec::new(),
        }
    }

    /// Terminate this task with an ABEND code.
    pub fn terminate_abend(&mut self, code: AbendCode, reason: u32) {
        self.abend_code = Some(code);
        self.reason_code = reason;
        self.state = TaskState::Terminated;
    }

    /// Terminate this task normally.
    pub fn terminate_normal(&mut self) {
        self.state = TaskState::Terminated;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_tcb_has_ready_state() {
        let dd = Arc::new(RwLock::new(DdTable::new()));
        let tcb = Tcb::new("IEFBR14", dd);
        assert_eq!(tcb.state, TaskState::Ready);
        assert!(tcb.parent.is_none());
        assert!(tcb.abend_code.is_none());
    }

    #[test]
    fn child_tcb_inherits_dd_table() {
        let dd = Arc::new(RwLock::new(DdTable::new()));
        let parent = Tcb::new("PARENT", dd.clone());
        let child = Tcb::new_child(parent.id, "CHILD", dd.clone());
        assert_eq!(child.parent, Some(parent.id));
        assert!(Arc::ptr_eq(&child.dd_table, &parent.dd_table));
    }

    #[test]
    fn terminate_abend_sets_code() {
        let dd = Arc::new(RwLock::new(DdTable::new()));
        let mut tcb = Tcb::new("TEST", dd);
        tcb.terminate_abend(AbendCode::User(100), 42);
        assert_eq!(tcb.state, TaskState::Terminated);
        assert_eq!(tcb.abend_code, Some(AbendCode::User(100)));
        assert_eq!(tcb.reason_code, 42);
    }

    #[test]
    fn terminate_normal_sets_terminated() {
        let dd = Arc::new(RwLock::new(DdTable::new()));
        let mut tcb = Tcb::new("TEST", dd);
        tcb.terminate_normal();
        assert_eq!(tcb.state, TaskState::Terminated);
        assert!(tcb.abend_code.is_none());
    }

    #[test]
    fn unique_tcb_ids() {
        let dd = Arc::new(RwLock::new(DdTable::new()));
        let tcb1 = Tcb::new("A", dd.clone());
        let tcb2 = Tcb::new("B", dd);
        assert_ne!(tcb1.id, tcb2.id);
    }

    #[test]
    fn task_state_serialization() {
        let state = TaskState::Running;
        let json = serde_json::to_string(&state).unwrap();
        let deserialized: TaskState = serde_json::from_str(&json).unwrap();
        assert_eq!(state, deserialized);
    }
}
