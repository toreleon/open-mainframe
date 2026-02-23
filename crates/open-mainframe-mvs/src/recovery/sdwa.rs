//! System Diagnostic Work Area — passed to ESTAE recovery exits.

use serde::{Deserialize, Serialize};

use crate::task::AbendCode;

/// System Diagnostic Work Area — contains diagnostic info for recovery exits.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Sdwa {
    /// ABEND code that triggered recovery.
    pub abend_code: AbendCode,
    /// Reason code associated with the ABEND.
    pub reason_code: u32,
    /// Address of the failing instruction.
    pub failing_instruction: u64,
    /// General-purpose register contents at time of failure.
    pub registers: [u64; 16],
    /// Program Status Word at time of failure.
    pub psw: u64,
}

impl Sdwa {
    /// Create a new SDWA for an ABEND.
    pub fn new(abend_code: AbendCode, reason_code: u32) -> Self {
        Self {
            abend_code,
            reason_code,
            failing_instruction: 0,
            registers: [0; 16],
            psw: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sdwa_creation() {
        let sdwa = Sdwa::new(AbendCode::System(0x0C7), 4);
        assert_eq!(sdwa.abend_code, AbendCode::System(0x0C7));
        assert_eq!(sdwa.reason_code, 4);
        assert_eq!(sdwa.registers.len(), 16);
    }
}
