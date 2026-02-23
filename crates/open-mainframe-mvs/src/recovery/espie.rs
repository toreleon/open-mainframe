//! ESPIE — program exception handling (intercept before ABEND).

use std::collections::HashMap;
use std::sync::Arc;

use serde::{Deserialize, Serialize};

/// Program Interruption Element — passed to ESPIE handlers.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Pie {
    /// Program interrupt code (e.g., 0x01=operation, 0x04=protection, 0x07=data).
    pub interrupt_code: u8,
    /// Address of the failing instruction.
    pub failing_instruction: u64,
    /// Program Status Word at time of interrupt.
    pub psw: u64,
}

/// Action returned by an ESPIE handler.
#[derive(Debug, Clone, Copy)]
pub enum EspieAction {
    /// Resume execution at the specified address.
    Resume { address: u64 },
    /// Proceed with normal ABEND processing.
    Abend,
}

/// Type alias for ESPIE handler function.
type EspieHandler = Arc<dyn Fn(&Pie) -> EspieAction + Send + Sync>;

/// ESPIE table — maps interrupt codes to handlers.
#[derive(Default)]
pub struct EspieTable {
    handlers: HashMap<u8, EspieHandler>,
}

impl std::fmt::Debug for EspieTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EspieTable")
            .field("codes", &self.handlers.keys().collect::<Vec<_>>())
            .finish()
    }
}

impl EspieTable {
    /// Create an empty ESPIE table.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register an ESPIE handler for the given interrupt codes.
    pub fn establish(
        &mut self,
        codes: &[u8],
        handler: Arc<dyn Fn(&Pie) -> EspieAction + Send + Sync>,
    ) {
        for &code in codes {
            self.handlers.insert(code, handler.clone());
        }
    }

    /// Remove ESPIE handling for specific codes.
    pub fn remove(&mut self, codes: &[u8]) {
        for code in codes {
            self.handlers.remove(code);
        }
    }

    /// Try to handle a program interrupt.
    ///
    /// Returns `Some(action)` if a handler exists, `None` otherwise.
    pub fn handle(&self, pie: &Pie) -> Option<EspieAction> {
        self.handlers.get(&pie.interrupt_code).map(|h| h(pie))
    }

    /// Check if a handler is registered for the given interrupt code.
    pub fn has_handler(&self, code: u8) -> bool {
        self.handlers.contains_key(&code)
    }

    /// Number of registered handlers.
    pub fn len(&self) -> usize {
        self.handlers.len()
    }

    /// Whether no handlers are registered.
    pub fn is_empty(&self) -> bool {
        self.handlers.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn establish_and_handle() {
        let mut table = EspieTable::new();
        table.establish(
            &[0x07],
            Arc::new(|_| EspieAction::Resume { address: 0x3000 }),
        );

        let pie = Pie {
            interrupt_code: 0x07,
            failing_instruction: 0x1000,
            psw: 0,
        };
        let action = table.handle(&pie);
        assert!(matches!(action, Some(EspieAction::Resume { address: 0x3000 })));
    }

    #[test]
    fn no_handler_returns_none() {
        let table = EspieTable::new();
        let pie = Pie {
            interrupt_code: 0x07,
            failing_instruction: 0x1000,
            psw: 0,
        };
        assert!(table.handle(&pie).is_none());
    }

    #[test]
    fn multiple_codes_same_handler() {
        let mut table = EspieTable::new();
        table.establish(
            &[0x01, 0x04, 0x07],
            Arc::new(|_| EspieAction::Abend),
        );
        assert!(table.has_handler(0x01));
        assert!(table.has_handler(0x04));
        assert!(table.has_handler(0x07));
        assert!(!table.has_handler(0x02));
    }

    #[test]
    fn remove_handler() {
        let mut table = EspieTable::new();
        table.establish(
            &[0x07],
            Arc::new(|_| EspieAction::Abend),
        );
        assert!(table.has_handler(0x07));
        table.remove(&[0x07]);
        assert!(!table.has_handler(0x07));
    }

    #[test]
    fn espie_before_estae() {
        // ESPIE intercepts before ESTAE — this is a semantic test
        let mut table = EspieTable::new();
        table.establish(
            &[0x07],
            Arc::new(|pie| {
                if pie.interrupt_code == 0x07 {
                    EspieAction::Resume { address: pie.failing_instruction + 4 }
                } else {
                    EspieAction::Abend
                }
            }),
        );

        let pie = Pie {
            interrupt_code: 0x07,
            failing_instruction: 0x2000,
            psw: 0,
        };
        let action = table.handle(&pie).unwrap();
        assert!(matches!(action, EspieAction::Resume { address: 0x2004 }));
    }
}
