//! MVS error types.

use miette::Diagnostic;
use thiserror::Error;

use crate::task::AbendCode;

/// Errors returned by MVS system services.
#[derive(Debug, Error, Diagnostic)]
pub enum MvsError {
    /// DYNALLOC (SVC 99) request failed.
    #[error("DYNALLOC failed: verb={verb}, error={error_code:#06x}, info={info_code:#06x}")]
    #[diagnostic(code(mvs::dynalloc_failed))]
    DynallocFailed {
        verb: u8,
        error_code: u16,
        info_code: u16,
    },

    /// ENQ contention timeout.
    #[error("ENQ contention timeout on resource {qname}/{rname}")]
    #[diagnostic(code(mvs::enq_timeout))]
    EnqTimeout { qname: String, rname: String },

    /// ENQ validation error.
    #[error("ENQ invalid resource: {reason}")]
    #[diagnostic(code(mvs::enq_invalid))]
    EnqInvalid { reason: String },

    /// Task ABEND.
    #[error("ABEND {code}")]
    #[diagnostic(code(mvs::abend))]
    Abend { code: AbendCode, reason: u32 },

    /// No ESTAE recovery exit established.
    #[error("ESTAE recovery failed: no recovery routine established")]
    #[diagnostic(code(mvs::no_estae))]
    NoEstaeExit,

    /// Program not found in search order.
    #[error("program '{name}' not found in search order")]
    #[diagnostic(code(mvs::program_not_found))]
    ProgramNotFound { name: String },

    /// Invalid text unit key.
    #[error("invalid text unit key {key:#06x}")]
    #[diagnostic(code(mvs::invalid_text_unit))]
    InvalidTextUnit { key: u16 },

    /// Invalid DDname.
    #[error("invalid DDname '{name}': {reason}")]
    #[diagnostic(code(mvs::invalid_ddname))]
    InvalidDdname { name: String, reason: String },

    /// DDname not found.
    #[error("DDname '{name}' not found in DD table")]
    #[diagnostic(code(mvs::ddname_not_found))]
    DdnameNotFound { name: String },

    /// Invalid dataset name.
    #[error("invalid dataset name '{name}': {reason}")]
    #[diagnostic(code(mvs::invalid_dsname))]
    InvalidDsname { name: String, reason: String },

    /// Storage allocation failure.
    #[error("GETMAIN failed: subpool={subpool}, length={length}")]
    #[diagnostic(code(mvs::getmain_failed))]
    GetmainFailed { subpool: u8, length: u32 },

    /// Storage free failure.
    #[error("FREEMAIN failed: address={address:#x}")]
    #[diagnostic(code(mvs::freemain_failed))]
    FreemainFailed { address: u64 },

    /// Console message error.
    #[error("WTO failed: {reason}")]
    #[diagnostic(code(mvs::wto_failed))]
    WtoFailed { reason: String },
}

/// MVS result type alias.
pub type Result<T> = std::result::Result<T, MvsError>;
