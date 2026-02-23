//! z/OS Workload Manager (WLM).
//!
//! This crate provides:
//!
//! - **Service Classes** — define performance goals for groups of work
//! - **Service Goals** — response time, execution velocity, discretionary
//! - **Classification Rules** — route work to service classes based on attributes
//! - **Service Policy** — named collections of service class/goal definitions

pub mod capping;
pub mod classify;
pub mod enclave;
pub mod goals;
pub mod iwm;
pub mod policy;
pub mod scheduling;
pub mod service;

pub use capping::{CappingEngine, EnforcementAction, GroupUtilization, RuntimeEnvironment, ThrottleAction};
pub use enclave::{Enclave, EnclaveManager, EnclaveState};
pub use iwm::{ClassifyResult, IwmServices, RegisteredServer, ServiceClassInfo};
pub use classify::{ClassificationRule, Classifier, SubsystemType, WorkAttribute, WorkRequest};
pub use goals::{
    PerformanceIndex, ResourceDemand, ServiceUnits, SlidingWindow, WorkUnitTracker,
};
pub use policy::{
    CpuCapType, ReportClass, ResourceGroup, ServiceDefinition, Workload,
};
pub use scheduling::{
    InitiatorScheduler, InitiatorState, ManagedInitiator, QueuedJob, SchedulingDecision,
    SchedulingEnvironment,
};
pub use service::{
    GoalType, Importance, ServiceClass, ServiceGoal, ServicePolicy, WlmError,
};
