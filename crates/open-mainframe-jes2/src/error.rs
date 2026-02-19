//! JES2 error types.

use thiserror::Error;

/// Errors produced by JES2 operations.
#[derive(Debug, Error)]
pub enum Jes2Error {
    /// Job not found in the queue.
    #[error("job {0} not found")]
    JobNotFound(String),

    /// Invalid job state transition.
    #[error("invalid state transition for job {job}: {from:?} -> {to:?}")]
    InvalidTransition {
        job: String,
        from: super::job::JobState,
        to: super::job::JobState,
    },

    /// Job is already held.
    #[error("job {0} is already held")]
    AlreadyHeld(String),

    /// Job is not held.
    #[error("job {0} is not held")]
    NotHeld(String),

    /// Job is not cancellable in its current state.
    #[error("job {0} cannot be cancelled in state {1:?}")]
    NotCancellable(String, super::job::JobState),

    /// Job class is not defined.
    #[error("job class '{0}' is not defined")]
    UndefinedClass(String),

    /// Spool dataset not found.
    #[error("spool dataset {0} not found")]
    SpoolDatasetNotFound(u64),

    /// Duplicate job name (when duplicate checking is enabled).
    #[error("duplicate job name: {0}")]
    DuplicateJobName(String),
}
