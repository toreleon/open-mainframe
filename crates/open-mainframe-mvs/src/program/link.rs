//! LINK/XCTL — program call and transfer control.

use crate::error::{MvsError, Result};
use crate::program::load::ModuleManager;

/// LINK — call a program and wait for its return code.
///
/// Locates the program via search order, loads it (incrementing use count),
/// and simulates execution. Returns the return code (R15).
pub fn link(mgr: &mut ModuleManager, name: &str, _param: Option<u64>) -> Result<i32> {
    let _module = mgr.load(name)?;

    // In simulation, the program "executes" and returns 0.
    // Real implementation would invoke the program's entry point.
    // The module remains loaded after LINK returns.
    Ok(0)
}

/// XCTL — transfer control to another program without return.
///
/// The current program's context is replaced by the target program.
/// Unlike LINK, there is no return to the caller.
pub fn xctl(mgr: &mut ModuleManager, name: &str, _param: Option<u64>) -> Result<()> {
    // Check that the target exists
    if mgr.search_order().search(name).is_none() {
        // S806 ABEND — program not found
        return Err(MvsError::Abend {
            code: crate::task::AbendCode::System(0x806),
            reason: 0,
        });
    }

    let _module = mgr.load(name)?;

    // In simulation, we mark that control has transferred.
    // Real implementation would free the current program and jump to target.
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::program::search::ProgramSearchOrder;

    fn make_manager() -> ModuleManager {
        let mut search = ProgramSearchOrder::new();
        search.register_program("IEFBR14", "SYS1.LINKLIB");
        search.register_program("IKJEFT01", "SYS1.LINKLIB");
        ModuleManager::new(search)
    }

    #[test]
    fn link_returns_return_code() {
        let mut mgr = make_manager();
        let rc = link(&mut mgr, "IEFBR14", None).unwrap();
        assert_eq!(rc, 0);
    }

    #[test]
    fn link_not_found_returns_error() {
        let mut mgr = make_manager();
        let err = link(&mut mgr, "BOGUS", None).unwrap_err();
        assert!(matches!(err, MvsError::ProgramNotFound { .. }));
    }

    #[test]
    fn link_with_param() {
        let mut mgr = make_manager();
        let rc = link(&mut mgr, "IEFBR14", Some(0x1234)).unwrap();
        assert_eq!(rc, 0);
    }

    #[test]
    fn xctl_transfers_control() {
        let mut mgr = make_manager();
        assert!(xctl(&mut mgr, "IKJEFT01", None).is_ok());
    }

    #[test]
    fn xctl_not_found_abend_s806() {
        let mut mgr = make_manager();
        let err = xctl(&mut mgr, "BOGUS", None).unwrap_err();
        match err {
            MvsError::Abend { code, .. } => {
                assert_eq!(code, crate::task::AbendCode::System(0x806));
            }
            _ => panic!("expected ABEND S806"),
        }
    }
}
