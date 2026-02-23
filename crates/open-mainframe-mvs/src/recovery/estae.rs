//! ESTAE — recovery exit chain management.
//!
//! ESTAE exits are invoked in LIFO order when an ABEND occurs.

use std::sync::Arc;

use super::sdwa::Sdwa;

/// Action returned by an ESTAE recovery exit.
#[derive(Debug, Clone, Copy)]
pub enum EstaeAction {
    /// Retry execution at the specified address.
    Retry { address: u64 },
    /// Percolate to the next ESTAE in the chain.
    Percolate,
}

/// ESTAE recovery chain entry.
#[derive(Clone)]
pub struct EstaeEntry {
    /// Recovery handler callback.
    pub handler: Arc<dyn Fn(&Sdwa) -> EstaeAction + Send + Sync>,
    /// User parameter passed to the handler.
    pub parameter: u64,
}

impl std::fmt::Debug for EstaeEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EstaeEntry")
            .field("parameter", &self.parameter)
            .finish_non_exhaustive()
    }
}

/// Manages the ESTAE recovery exit chain for a task.
#[derive(Debug, Default)]
pub struct EstaeManager {
    chain: Vec<EstaeEntry>,
}

impl EstaeManager {
    /// Create a new empty ESTAE manager.
    pub fn new() -> Self {
        Self { chain: Vec::new() }
    }

    /// Establish (push) an ESTAE exit onto the chain.
    pub fn establish(&mut self, entry: EstaeEntry) {
        self.chain.push(entry);
    }

    /// Remove the most recently established ESTAE exit.
    pub fn remove_latest(&mut self) -> Option<EstaeEntry> {
        self.chain.pop()
    }

    /// Process the ESTAE chain for an ABEND.
    ///
    /// Invokes exits in LIFO order. Returns the first non-Percolate action,
    /// or `None` if all exits percolated.
    pub fn process(&self, sdwa: &Sdwa) -> Option<EstaeAction> {
        for entry in self.chain.iter().rev() {
            let action = (entry.handler)(sdwa);
            match action {
                EstaeAction::Retry { .. } => return Some(action),
                EstaeAction::Percolate => continue,
            }
        }
        None
    }

    /// Number of ESTAE exits in the chain.
    pub fn len(&self) -> usize {
        self.chain.len()
    }

    /// Whether the chain is empty.
    pub fn is_empty(&self) -> bool {
        self.chain.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::task::AbendCode;

    #[test]
    fn establish_and_remove() {
        let mut mgr = EstaeManager::new();
        let entry = EstaeEntry {
            handler: Arc::new(|_| EstaeAction::Percolate),
            parameter: 0,
        };
        mgr.establish(entry);
        assert_eq!(mgr.len(), 1);
        mgr.remove_latest();
        assert!(mgr.is_empty());
    }

    #[test]
    fn lifo_order_processing() {
        let mut mgr = EstaeManager::new();

        // First exit: percolate
        mgr.establish(EstaeEntry {
            handler: Arc::new(|_| EstaeAction::Percolate),
            parameter: 1,
        });

        // Second exit: retry
        mgr.establish(EstaeEntry {
            handler: Arc::new(|_| EstaeAction::Retry { address: 0x1000 }),
            parameter: 2,
        });

        let sdwa = Sdwa::new(AbendCode::System(0x0C7), 0);
        let action = mgr.process(&sdwa);
        // Second (last-established) exit should be invoked first and returns Retry
        assert!(matches!(action, Some(EstaeAction::Retry { address: 0x1000 })));
    }

    #[test]
    fn all_percolate_returns_none() {
        let mut mgr = EstaeManager::new();
        mgr.establish(EstaeEntry {
            handler: Arc::new(|_| EstaeAction::Percolate),
            parameter: 0,
        });
        mgr.establish(EstaeEntry {
            handler: Arc::new(|_| EstaeAction::Percolate),
            parameter: 0,
        });

        let sdwa = Sdwa::new(AbendCode::User(100), 0);
        assert!(mgr.process(&sdwa).is_none());
    }

    #[test]
    fn empty_chain_returns_none() {
        let mgr = EstaeManager::new();
        let sdwa = Sdwa::new(AbendCode::User(100), 0);
        assert!(mgr.process(&sdwa).is_none());
    }

    #[test]
    fn estae_receives_sdwa_data() {
        let mut mgr = EstaeManager::new();
        mgr.establish(EstaeEntry {
            handler: Arc::new(|sdwa| {
                if sdwa.abend_code == AbendCode::System(0x0C7) {
                    EstaeAction::Retry { address: 0x2000 }
                } else {
                    EstaeAction::Percolate
                }
            }),
            parameter: 0,
        });

        let sdwa = Sdwa::new(AbendCode::System(0x0C7), 42);
        let action = mgr.process(&sdwa);
        assert!(matches!(action, Some(EstaeAction::Retry { address: 0x2000 })));
    }
}
