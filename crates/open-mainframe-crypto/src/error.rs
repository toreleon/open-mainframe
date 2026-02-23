//! Crypto crate error types.

use miette::Diagnostic;
use thiserror::Error;

/// Errors produced by the cryptographic subsystem.
#[derive(Debug, Error, Diagnostic)]
pub enum CryptoError {
    /// Invalid key length for the requested algorithm.
    #[error("invalid key length {actual} for {algorithm} (expected {expected})")]
    #[diagnostic(code(crypto::invalid_key_length))]
    InvalidKeyLength {
        /// Algorithm name.
        algorithm: String,
        /// Expected length in bytes.
        expected: usize,
        /// Actual length in bytes.
        actual: usize,
    },

    /// Key label not found in the key store.
    #[error("key label '{label}' not found in {store}")]
    #[diagnostic(code(crypto::key_not_found))]
    KeyNotFound {
        /// The missing key label.
        label: String,
        /// The store name (CKDS/PKDS/TKDS).
        store: String,
    },

    /// Key label already exists in the key store.
    #[error("key label '{label}' already exists in {store}")]
    #[diagnostic(code(crypto::key_exists))]
    KeyExists {
        /// The duplicate key label.
        label: String,
        /// The store name.
        store: String,
    },

    /// ICSF service returned an error.
    #[error("ICSF service error: return_code={return_code}, reason_code={reason_code}")]
    #[diagnostic(code(crypto::icsf_error))]
    IcsfError {
        /// ICSF return code.
        return_code: i32,
        /// ICSF reason code.
        reason_code: i32,
    },

    /// Empty data provided where non-empty is required.
    #[error("empty data provided for {operation}")]
    #[diagnostic(code(crypto::empty_data))]
    EmptyData {
        /// The operation name.
        operation: String,
    },

    /// Invalid RSA key size.
    #[error("invalid RSA key size {bits}: must be 2048 or 4096")]
    #[diagnostic(code(crypto::invalid_rsa_size))]
    InvalidRsaKeySize {
        /// The requested bit size.
        bits: u32,
    },

    /// Invalid EC curve.
    #[error("unsupported EC curve for operation")]
    #[diagnostic(code(crypto::unsupported_curve))]
    UnsupportedCurve,

    /// Signature verification failed.
    #[error("signature verification failed")]
    #[diagnostic(code(crypto::verification_failed))]
    VerificationFailed,

    /// Access denied by RACF profile.
    #[error("access denied: user '{user}' lacks {required} access to '{resource}'")]
    #[diagnostic(code(crypto::access_denied))]
    AccessDenied {
        /// The user ID.
        user: String,
        /// The required access level.
        required: String,
        /// The resource name.
        resource: String,
    },

    /// Exit already registered for the given point.
    #[error("exit already registered for point {point}")]
    #[diagnostic(code(crypto::exit_exists))]
    ExitAlreadyRegistered {
        /// The exit point name.
        point: String,
    },

    /// RACF database integrity error.
    #[error("RACF database integrity error: {message}")]
    #[diagnostic(code(crypto::integrity_error))]
    IntegrityError {
        /// Description of the integrity issue.
        message: String,
    },

    /// Invalid configuration.
    #[error("invalid configuration: {message}")]
    #[diagnostic(code(crypto::invalid_config))]
    InvalidConfig {
        /// Description.
        message: String,
    },
}
