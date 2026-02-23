//! ABEND processing — abnormal termination with system and user codes.

use std::fmt;

use serde::{Deserialize, Serialize};

/// ABEND completion code — either a system code or a user code.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum AbendCode {
    /// System ABEND code (e.g., 0C7 = data exception).
    System(u16),
    /// User ABEND code (0-4095).
    User(u16),
}

impl fmt::Display for AbendCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AbendCode::System(code) => write!(f, "S{code:03X}"),
            AbendCode::User(code) => write!(f, "U{code:04}"),
        }
    }
}

/// Trigger an abnormal termination with the given code.
///
/// Returns an `MvsError::Abend` that callers can propagate.
pub fn abend(code: AbendCode) -> crate::MvsError {
    crate::MvsError::Abend { code, reason: 0 }
}

/// Trigger an abnormal termination with code and reason.
pub fn abend_with_reason(code: AbendCode, reason: u32) -> crate::MvsError {
    crate::MvsError::Abend { code, reason }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn system_abend_display() {
        let code = AbendCode::System(0x0C7);
        assert_eq!(format!("{code}"), "S0C7");
    }

    #[test]
    fn user_abend_display() {
        let code = AbendCode::User(100);
        assert_eq!(format!("{code}"), "U0100");
    }

    #[test]
    fn user_abend_max() {
        let code = AbendCode::User(4095);
        assert_eq!(format!("{code}"), "U4095");
    }

    #[test]
    fn abend_creates_error() {
        let err = abend(AbendCode::User(999));
        match err {
            crate::MvsError::Abend { code, reason } => {
                assert_eq!(code, AbendCode::User(999));
                assert_eq!(reason, 0);
            }
            _ => panic!("expected Abend error"),
        }
    }

    #[test]
    fn abend_with_reason_creates_error() {
        let err = abend_with_reason(AbendCode::System(0x0C4), 42);
        match err {
            crate::MvsError::Abend { code, reason } => {
                assert_eq!(code, AbendCode::System(0x0C4));
                assert_eq!(reason, 42);
            }
            _ => panic!("expected Abend error"),
        }
    }

    #[test]
    fn abend_code_serialization() {
        let code = AbendCode::System(0x0C7);
        let json = serde_json::to_string(&code).unwrap();
        let deserialized: AbendCode = serde_json::from_str(&json).unwrap();
        assert_eq!(code, deserialized);
    }
}
