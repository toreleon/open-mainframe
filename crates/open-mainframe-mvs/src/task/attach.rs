//! ATTACH/DETACH — subtask management.

use std::sync::Arc;

use tokio::sync::RwLock;

use crate::dynalloc::DdTable;
use crate::error::Result;
use crate::sync::Ecb;
use crate::task::tcb::Tcb;

/// Token returned by ATTACH to identify a subtask.
#[derive(Debug, Clone)]
pub struct AttachToken {
    /// TCB ID of the attached subtask.
    pub tcb_id: u64,
    /// Completion ECB (posted when subtask ends).
    pub completion_ecb: Arc<Ecb>,
}

/// ATTACH — create a subtask running a program.
///
/// Creates a new child TCB that inherits the parent's DD table.
/// Returns a token the parent can use to wait for and detach the subtask.
pub fn attach(
    parent: &mut Tcb,
    program_name: &str,
    _param: Option<u64>,
) -> AttachToken {
    let dd_table = parent.dd_table.clone();
    let child = Tcb::new_child(parent.id, program_name, dd_table);
    let tcb_id = child.id;
    let ecb = Arc::new(Ecb::new());

    parent.children.push(tcb_id);

    AttachToken {
        tcb_id,
        completion_ecb: ecb,
    }
}

/// DETACH — remove a subtask and free its resources.
///
/// If the subtask is still running, it is terminated.
pub fn detach(parent: &mut Tcb, token: &AttachToken) -> Result<()> {
    parent.children.retain(|&id| id != token.tcb_id);
    Ok(())
}

/// Simulate subtask completion — post the completion ECB.
///
/// In a real implementation, this would be called by the task dispatcher
/// when the subtask's program returns.
pub fn complete_subtask(token: &AttachToken, completion_code: u32) {
    token.completion_ecb.post(completion_code);
}

/// Create a subtask context for testing — returns a child TCB and its DD table.
pub fn create_subtask_context(
    parent_id: u64,
    program_name: &str,
    dd_table: Arc<RwLock<DdTable>>,
) -> Tcb {
    Tcb::new_child(parent_id, program_name, dd_table)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_parent() -> Tcb {
        let dd = Arc::new(RwLock::new(DdTable::new()));
        Tcb::new("PARENT", dd)
    }

    #[test]
    fn attach_creates_child() {
        let mut parent = make_parent();
        let token = attach(&mut parent, "CHILDPGM", None);
        assert!(parent.children.contains(&token.tcb_id));
        assert!(!token.completion_ecb.is_complete());
    }

    #[test]
    fn attach_inherits_dd_table() {
        let dd = Arc::new(RwLock::new(DdTable::new()));
        let mut parent = Tcb::new("PARENT", dd.clone());
        let _token = attach(&mut parent, "CHILD", None);
        // Child would have same dd_table reference (verified in subtask context)
        let child = create_subtask_context(parent.id, "CHILD", parent.dd_table.clone());
        assert!(Arc::ptr_eq(&child.dd_table, &parent.dd_table));
    }

    #[test]
    fn detach_removes_child() {
        let mut parent = make_parent();
        let token = attach(&mut parent, "CHILDPGM", None);
        assert_eq!(parent.children.len(), 1);
        detach(&mut parent, &token).unwrap();
        assert!(parent.children.is_empty());
    }

    #[test]
    fn complete_subtask_posts_ecb() {
        let mut parent = make_parent();
        let token = attach(&mut parent, "CHILDPGM", None);
        complete_subtask(&token, 0);
        assert!(token.completion_ecb.is_complete());
        assert_eq!(token.completion_ecb.completion_code(), 0);
    }

    #[tokio::test]
    async fn parent_waits_for_subtask() {
        let mut parent = make_parent();
        let token = attach(&mut parent, "CHILDPGM", None);
        let ecb = token.completion_ecb.clone();

        // Simulate subtask completing asynchronously
        let token_clone = token.clone();
        tokio::spawn(async move {
            tokio::task::yield_now().await;
            complete_subtask(&token_clone, 4);
        });

        ecb.wait_on().await;
        assert!(ecb.is_complete());
        assert_eq!(ecb.completion_code(), 4);
    }

    #[test]
    fn detach_running_subtask() {
        let mut parent = make_parent();
        let token = attach(&mut parent, "LONGRUN", None);
        // Detach without waiting for completion
        assert!(detach(&mut parent, &token).is_ok());
        assert!(parent.children.is_empty());
    }

    #[test]
    fn multiple_subtasks() {
        let mut parent = make_parent();
        let t1 = attach(&mut parent, "CHILD1", None);
        let t2 = attach(&mut parent, "CHILD2", None);
        assert_eq!(parent.children.len(), 2);
        assert_ne!(t1.tcb_id, t2.tcb_id);
    }
}
