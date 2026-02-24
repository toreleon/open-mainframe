//! Job representation, states, classes, and identifiers.

use serde::{Deserialize, Serialize};
use std::fmt;

// ---------------------------------------------------------------------------
// Job identifier
// ---------------------------------------------------------------------------

/// Unique job identifier assigned by JES2 (e.g. JOB00001).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct JobId(pub u32);

impl fmt::Display for JobId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "JOB{:05}", self.0)
    }
}

// ---------------------------------------------------------------------------
// Job state machine
// ---------------------------------------------------------------------------

/// States a job transitions through during its lifecycle.
///
/// Normal flow: `Input → Conversion → Ready → Running → Output → Purge`
///
/// A job can be held (`Held`) at any point before `Running`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum JobState {
    /// Job has been submitted and is on the input queue.
    Input,
    /// JCL is being converted / validated.
    Conversion,
    /// Job is ready to be selected by an initiator.
    Ready,
    /// Job is currently executing.
    Running,
    /// Job has completed; output is available on the spool.
    Output,
    /// Job is being purged from the system.
    Purge,
    /// Job is held and will not progress until released.
    Held {
        /// The state the job was in before being held.
        previous: HeldFrom,
    },
    /// Job was cancelled by the operator.
    Cancelled,
}

/// Captures which state a job was in when it was held, so we can restore it
/// on release.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum HeldFrom {
    Input,
    Conversion,
    Ready,
}

impl JobState {
    /// Returns `true` if the job can be held in this state.
    pub fn can_hold(&self) -> bool {
        matches!(self, JobState::Input | JobState::Conversion | JobState::Ready)
    }

    /// Returns `true` if the job is in the `Held` state.
    pub fn is_held(&self) -> bool {
        matches!(self, JobState::Held { .. })
    }

    /// Returns `true` if the job can be cancelled in this state.
    pub fn can_cancel(&self) -> bool {
        !matches!(self, JobState::Purge | JobState::Cancelled)
    }

    /// Returns the next state in the normal lifecycle, or `None` if the job
    /// has reached a terminal state.
    pub fn next(self) -> Option<JobState> {
        match self {
            JobState::Input => Some(JobState::Conversion),
            JobState::Conversion => Some(JobState::Ready),
            JobState::Ready => Some(JobState::Running),
            JobState::Running => Some(JobState::Output),
            JobState::Output => Some(JobState::Purge),
            _ => None,
        }
    }
}

// ---------------------------------------------------------------------------
// Job class
// ---------------------------------------------------------------------------

/// JES2 job class.
///
/// Standard classes: A-Z, 0-9.
/// Pseudo-classes: STC (started task), TSU (time-sharing user).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum JobClass {
    /// Standard single-character class (A-Z, 0-9).
    Standard(char),
    /// Started task pseudo-class.
    Stc,
    /// Time-sharing user pseudo-class.
    Tsu,
}

impl fmt::Display for JobClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JobClass::Standard(c) => write!(f, "{c}"),
            JobClass::Stc => write!(f, "STC"),
            JobClass::Tsu => write!(f, "TSU"),
        }
    }
}

impl JobClass {
    /// Create a standard class from a character. Returns `None` if the
    /// character is not a valid class identifier (A-Z, 0-9).
    pub fn standard(c: char) -> Option<Self> {
        let c = c.to_ascii_uppercase();
        if c.is_ascii_uppercase() || c.is_ascii_digit() {
            Some(JobClass::Standard(c))
        } else {
            None
        }
    }
}

// ---------------------------------------------------------------------------
// Job class definition (configurable attributes)
// ---------------------------------------------------------------------------

/// Configuration for a job class, as defined in JES2PARM.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobClassDef {
    /// The class this definition applies to.
    pub class: JobClass,
    /// Name of the procedure library (e.g. "PROC00").
    pub proclib: String,
    /// Message class for job-log output routing.
    pub msgclass: char,
    /// Maximum acceptable return code; higher causes job failure.
    pub max_rc: u32,
    /// Description text (for display purposes).
    pub description: String,
}

impl JobClassDef {
    /// Create a default definition for a standard class.
    pub fn default_for(class: JobClass) -> Self {
        let msgclass = match &class {
            JobClass::Standard(c) => *c,
            JobClass::Stc => 'A',
            JobClass::Tsu => 'A',
        };
        Self {
            class,
            proclib: "PROC00".to_string(),
            msgclass,
            max_rc: 4,
            description: String::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// TYPRUN
// ---------------------------------------------------------------------------

/// JCL `TYPRUN=` parameter — controls special job processing modes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TypeRun {
    /// Normal execution (no TYPRUN specified).
    Run,
    /// `TYPRUN=HOLD` — job enters Held state on submission.
    Hold,
    /// `TYPRUN=SCAN` — syntax-check JCL only; no execution.
    Scan,
    /// `TYPRUN=COPY` — copy JCL to internal reader without execution.
    Copy,
}

impl Default for TypeRun {
    fn default() -> Self {
        Self::Run
    }
}

impl fmt::Display for TypeRun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Run => write!(f, "RUN"),
            Self::Hold => write!(f, "HOLD"),
            Self::Scan => write!(f, "SCAN"),
            Self::Copy => write!(f, "COPY"),
        }
    }
}

// ---------------------------------------------------------------------------
// Job
// ---------------------------------------------------------------------------

/// A JES2 job.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Job {
    /// JES2-assigned job identifier.
    pub id: JobId,
    /// Job name (from JCL JOB card, e.g. "PAYROLL").
    pub name: String,
    /// Execution class.
    pub class: JobClass,
    /// Priority (0-15, higher = more urgent).
    pub priority: u8,
    /// Current state.
    pub state: JobState,
    /// Whether the job was submitted with `TYPRUN=HOLD`.
    pub hold_on_submit: bool,
    /// TYPRUN processing mode.
    pub typrun: TypeRun,
    /// Owner (submitting user).
    pub owner: String,
    /// Maximum return code observed across steps.
    pub max_rc: u32,
    /// Spool dataset keys for SYSOUT owned by this job.
    pub spool_keys: Vec<u64>,
    /// Target system name (from `/*ROUTE XEQ` or API). None = local system.
    pub system: Option<String>,
}

impl Job {
    /// Create a new job. If `hold_on_submit` is true, the initial state is
    /// `Held { previous: HeldFrom::Input }` instead of `Input`.
    pub fn new(
        id: JobId,
        name: String,
        class: JobClass,
        priority: u8,
        hold_on_submit: bool,
    ) -> Self {
        let priority = priority.min(15);
        let state = if hold_on_submit {
            JobState::Held {
                previous: HeldFrom::Input,
            }
        } else {
            JobState::Input
        };
        Self {
            id,
            name,
            class,
            priority,
            state,
            hold_on_submit,
            typrun: if hold_on_submit { TypeRun::Hold } else { TypeRun::Run },
            owner: String::new(),
            max_rc: 0,
            spool_keys: Vec::new(),
            system: None,
        }
    }

    /// Create a new job with a specific TYPRUN mode.
    ///
    /// - `TypeRun::Hold` — enters Held state on submission
    /// - `TypeRun::Scan` — JCL syntax check only; transitions Input -> Conversion -> Purge
    /// - `TypeRun::Copy` — copies JCL to internal reader; transitions Input -> Purge
    /// - `TypeRun::Run` — normal execution
    pub fn new_with_typrun(
        id: JobId,
        name: String,
        class: JobClass,
        priority: u8,
        typrun: TypeRun,
    ) -> Self {
        let priority = priority.min(15);
        let hold_on_submit = typrun == TypeRun::Hold;
        let state = if hold_on_submit {
            JobState::Held {
                previous: HeldFrom::Input,
            }
        } else {
            JobState::Input
        };
        Self {
            id,
            name,
            class,
            priority,
            state,
            hold_on_submit,
            typrun,
            owner: String::new(),
            max_rc: 0,
            spool_keys: Vec::new(),
            system: None,
        }
    }

    /// Returns true if this job is a TYPRUN=SCAN job (syntax check only).
    pub fn is_scan_only(&self) -> bool {
        self.typrun == TypeRun::Scan
    }

    /// Returns true if this job is a TYPRUN=COPY job (copy to internal reader).
    pub fn is_copy_only(&self) -> bool {
        self.typrun == TypeRun::Copy
    }

    /// Advance the job to the next state in its lifecycle.
    pub fn advance(&mut self) -> Result<JobState, super::Jes2Error> {
        if let Some(next) = self.state.next() {
            self.state = next;
            Ok(next)
        } else {
            Err(super::Jes2Error::InvalidTransition {
                job: self.id.to_string(),
                from: self.state,
                to: self.state, // no valid next
            })
        }
    }

    /// Hold the job. Only valid in Input, Conversion, or Ready states.
    pub fn hold(&mut self) -> Result<(), super::Jes2Error> {
        if self.state.is_held() {
            return Err(super::Jes2Error::AlreadyHeld(self.id.to_string()));
        }
        let previous = match self.state {
            JobState::Input => HeldFrom::Input,
            JobState::Conversion => HeldFrom::Conversion,
            JobState::Ready => HeldFrom::Ready,
            _ => {
                return Err(super::Jes2Error::InvalidTransition {
                    job: self.id.to_string(),
                    from: self.state,
                    to: JobState::Held {
                        previous: HeldFrom::Input,
                    },
                })
            }
        };
        self.state = JobState::Held { previous };
        Ok(())
    }

    /// Release a held job, restoring its previous state.
    pub fn release(&mut self) -> Result<(), super::Jes2Error> {
        match self.state {
            JobState::Held { previous } => {
                self.state = match previous {
                    HeldFrom::Input => JobState::Input,
                    HeldFrom::Conversion => JobState::Conversion,
                    HeldFrom::Ready => JobState::Ready,
                };
                Ok(())
            }
            _ => Err(super::Jes2Error::NotHeld(self.id.to_string())),
        }
    }

    /// Cancel the job.
    pub fn cancel(&mut self) -> Result<(), super::Jes2Error> {
        if !self.state.can_cancel() {
            return Err(super::Jes2Error::NotCancellable(
                self.id.to_string(),
                self.state,
            ));
        }
        self.state = JobState::Cancelled;
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn job_id_display() {
        assert_eq!(JobId(1).to_string(), "JOB00001");
        assert_eq!(JobId(12345).to_string(), "JOB12345");
    }

    #[test]
    fn standard_class_validation() {
        assert!(JobClass::standard('A').is_some());
        assert!(JobClass::standard('z').is_some()); // lowercased to Z
        assert!(JobClass::standard('5').is_some());
        assert!(JobClass::standard('!').is_none());
    }

    #[test]
    fn class_display() {
        assert_eq!(JobClass::Standard('A').to_string(), "A");
        assert_eq!(JobClass::Stc.to_string(), "STC");
        assert_eq!(JobClass::Tsu.to_string(), "TSU");
    }

    #[test]
    fn job_normal_lifecycle() {
        let mut job = Job::new(
            JobId(1),
            "TEST".to_string(),
            JobClass::Standard('A'),
            10,
            false,
        );
        assert_eq!(job.state, JobState::Input);

        assert_eq!(job.advance().unwrap(), JobState::Conversion);
        assert_eq!(job.advance().unwrap(), JobState::Ready);
        assert_eq!(job.advance().unwrap(), JobState::Running);
        assert_eq!(job.advance().unwrap(), JobState::Output);
        assert_eq!(job.advance().unwrap(), JobState::Purge);
        assert!(job.advance().is_err()); // terminal
    }

    #[test]
    fn job_hold_on_submit() {
        let job = Job::new(
            JobId(2),
            "HELD".to_string(),
            JobClass::Standard('B'),
            5,
            true,
        );
        assert!(job.state.is_held());
        assert_eq!(
            job.state,
            JobState::Held {
                previous: HeldFrom::Input
            }
        );
    }

    #[test]
    fn hold_and_release() {
        let mut job = Job::new(
            JobId(3),
            "HOLDREL".to_string(),
            JobClass::Standard('A'),
            8,
            false,
        );
        // Advance to Ready
        job.advance().unwrap(); // -> Conversion
        job.advance().unwrap(); // -> Ready

        // Hold
        job.hold().unwrap();
        assert_eq!(
            job.state,
            JobState::Held {
                previous: HeldFrom::Ready,
            }
        );

        // Release restores Ready
        job.release().unwrap();
        assert_eq!(job.state, JobState::Ready);
    }

    #[test]
    fn hold_running_fails() {
        let mut job = Job::new(
            JobId(4),
            "RUNNING".to_string(),
            JobClass::Standard('A'),
            1,
            false,
        );
        job.advance().unwrap(); // Conversion
        job.advance().unwrap(); // Ready
        job.advance().unwrap(); // Running
        assert!(job.hold().is_err());
    }

    #[test]
    fn double_hold_fails() {
        let mut job = Job::new(
            JobId(5),
            "DBLHOLD".to_string(),
            JobClass::Standard('A'),
            1,
            false,
        );
        job.hold().unwrap();
        assert!(job.hold().is_err());
    }

    #[test]
    fn release_non_held_fails() {
        let mut job = Job::new(
            JobId(6),
            "NOHOLD".to_string(),
            JobClass::Standard('A'),
            1,
            false,
        );
        assert!(job.release().is_err());
    }

    #[test]
    fn cancel_job() {
        let mut job = Job::new(
            JobId(7),
            "CANCEL".to_string(),
            JobClass::Standard('C'),
            3,
            false,
        );
        job.advance().unwrap(); // Conversion
        job.advance().unwrap(); // Ready
        job.advance().unwrap(); // Running
        job.cancel().unwrap();
        assert_eq!(job.state, JobState::Cancelled);
    }

    #[test]
    fn cancel_purge_fails() {
        let mut job = Job::new(
            JobId(8),
            "PURGED".to_string(),
            JobClass::Standard('A'),
            1,
            false,
        );
        // Drive to Purge
        job.advance().unwrap();
        job.advance().unwrap();
        job.advance().unwrap();
        job.advance().unwrap();
        job.advance().unwrap();
        assert!(job.cancel().is_err());
    }

    #[test]
    fn priority_clamped_to_15() {
        let job = Job::new(
            JobId(9),
            "HIPRTY".to_string(),
            JobClass::Standard('A'),
            99,
            false,
        );
        assert_eq!(job.priority, 15);
    }

    #[test]
    fn class_def_defaults() {
        let def = JobClassDef::default_for(JobClass::Standard('X'));
        assert_eq!(def.msgclass, 'X');
        assert_eq!(def.proclib, "PROC00");
        assert_eq!(def.max_rc, 4);

        let stc_def = JobClassDef::default_for(JobClass::Stc);
        assert_eq!(stc_def.msgclass, 'A');
    }

    #[test]
    fn job_state_next() {
        assert_eq!(JobState::Input.next(), Some(JobState::Conversion));
        assert_eq!(JobState::Purge.next(), None);
        assert_eq!(JobState::Cancelled.next(), None);
        assert_eq!(
            (JobState::Held {
                previous: HeldFrom::Input
            })
            .next(),
            None
        );
    }

    #[test]
    fn typrun_default_is_run() {
        assert_eq!(TypeRun::default(), TypeRun::Run);
    }

    #[test]
    fn typrun_display() {
        assert_eq!(TypeRun::Run.to_string(), "RUN");
        assert_eq!(TypeRun::Hold.to_string(), "HOLD");
        assert_eq!(TypeRun::Scan.to_string(), "SCAN");
        assert_eq!(TypeRun::Copy.to_string(), "COPY");
    }

    #[test]
    fn new_with_typrun_scan() {
        let job = Job::new_with_typrun(
            JobId(10),
            "SCANME".to_string(),
            JobClass::Standard('A'),
            5,
            TypeRun::Scan,
        );
        assert_eq!(job.typrun, TypeRun::Scan);
        assert!(job.is_scan_only());
        assert!(!job.is_copy_only());
        assert!(!job.hold_on_submit);
        assert_eq!(job.state, JobState::Input);
    }

    #[test]
    fn new_with_typrun_copy() {
        let job = Job::new_with_typrun(
            JobId(11),
            "COPYME".to_string(),
            JobClass::Standard('B'),
            3,
            TypeRun::Copy,
        );
        assert_eq!(job.typrun, TypeRun::Copy);
        assert!(job.is_copy_only());
        assert!(!job.is_scan_only());
        assert_eq!(job.state, JobState::Input);
    }

    #[test]
    fn new_with_typrun_hold() {
        let job = Job::new_with_typrun(
            JobId(12),
            "HOLDME".to_string(),
            JobClass::Standard('A'),
            7,
            TypeRun::Hold,
        );
        assert_eq!(job.typrun, TypeRun::Hold);
        assert!(job.hold_on_submit);
        assert!(job.state.is_held());
    }

    #[test]
    fn new_sets_typrun_from_hold() {
        let hold_job = Job::new(
            JobId(13),
            "HELD".to_string(),
            JobClass::Standard('A'),
            5,
            true,
        );
        assert_eq!(hold_job.typrun, TypeRun::Hold);

        let normal_job = Job::new(
            JobId(14),
            "NORM".to_string(),
            JobClass::Standard('A'),
            5,
            false,
        );
        assert_eq!(normal_job.typrun, TypeRun::Run);
    }
}
