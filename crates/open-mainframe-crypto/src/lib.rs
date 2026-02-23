#![forbid(unsafe_code)]
//! # z/OS ICSF Cryptographic Services
//!
//! Implementation of the IBM Integrated Cryptographic Service Facility (ICSF) and
//! related RACF security extensions for the OpenMainframe project.
//!
//! ## Epics
//!
//! - **CRYPTO-100** — ICSF Symmetric & Hashing (`symmetric`)
//! - **CRYPTO-101** — ICSF Asymmetric Key Operations (`asymmetric`)
//! - **CRYPTO-102** — Key Management & Key Stores (`keystore`)
//! - **CRYPTO-103** — CSFKEYS/CSFSERV RACF Integration (`racf_crypto`)
//! - **SEC-107** — Security Labels / MAC (`seclabel`)
//! - **SEC-108** — RACF Audit & SMF Integration (`audit`)
//! - **SEC-109** — RACF Exits, Utilities & Config (`exits`)

pub mod asymmetric;
pub mod audit;
pub mod exits;
pub mod keystore;
pub mod racf_crypto;
pub mod seclabel;
pub mod symmetric;

mod error;

pub use asymmetric::{
    EcCurve, EcKeyPair, RsaKeyPair, SignScheme, ecdh_agree, ecdsa_sign, ecdsa_verify,
    generate_rsa_keypair, rsa_sign, rsa_verify,
};
pub use audit::{AuditTrail, Irrdbu00, SmfType80ProfileChange, SmfType80Record, SmfType81Record, SmfType83Record};
pub use error::CryptoError;
pub use exits::{
    ExitPoint, ExitRegistry, RacfSearchUtil, RacfSplitMergeUtil, RacfVerifyUtil,
};
pub use keystore::{Ckds, KeyLifecycle, MasterKey, Pkds, Tkds};
pub use racf_crypto::{
    CsfKeysProfile, CsfServProfile, GcsfKeysProfile, check_key_access, check_service_access,
};
pub use seclabel::{
    MlsMode, SeclabelProfile, SecurityCategory, SecurityLabel, SecurityLevel, dominance_check,
};
pub use symmetric::{
    CipherMode, HashAlgorithm, IcsfConfig, IcsfResult, IcsfSymmetricKey, SymmetricAlgorithm,
    generate_random, hmac_generate, hmac_verify, one_way_hash,
};

/// Convenience result type for crypto operations.
pub type Result<T> = std::result::Result<T, CryptoError>;
