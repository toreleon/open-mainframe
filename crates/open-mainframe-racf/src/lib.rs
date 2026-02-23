//! # RACF Security Subsystem
//!
//! Implementation of the IBM RACF (Resource Access Control Facility) security
//! subsystem for the OpenMainframe project.
//!
//! ## Features
//!
//! - **User profiles** — ADDUSER/ALTUSER/LISTUSER/DELUSER with attributes
//!   (SPECIAL, OPERATIONS, AUDITOR, REVOKE, etc.)
//! - **Group profiles** — ADDGROUP/ALTGROUP/LISTGRP/DELGROUP with hierarchical
//!   group structure under SYS1
//! - **Connect/Remove** — group membership management with authority levels
//!   (USE, CREATE, CONNECT, JOIN)
//! - **Persistent storage** — JSON-backed database with load/save
//! - **Search** — wildcard mask matching across user and group profiles
//!
//! ## Example
//!
//! ```rust
//! use open_mainframe_racf::RacfDatabase;
//! use open_mainframe_racf::types::ConnectAuthority;
//!
//! let mut db = RacfDatabase::new();
//!
//! // Create a group under SYS1
//! db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();
//!
//! // Create a user with DEPT01 as default group
//! db.add_user("JSMITH", "DEPT01", "John Smith", "ADMIN1").unwrap();
//!
//! // Connect user to another group
//! db.add_group("PAYROLL", "SYS1", "ADMIN1").unwrap();
//! db.connect("JSMITH", "PAYROLL", ConnectAuthority::Use).unwrap();
//!
//! // Search for users
//! let results = db.search("USER", "J*");
//! assert_eq!(results.entries.len(), 1);
//! ```

pub mod auth;
pub mod certificate;
pub mod database;
pub mod dataset;
pub mod error;
pub mod exits;
pub mod profile;
pub mod resource;
pub mod saf;
pub mod seclabel;
pub mod setropts;
pub mod types;
pub mod utilities;

pub use auth::{
    AuthResult, AuthService, PasswordChangeResult, PasswordPolicy, PassTicket,
    PassTicketKeyType, PassTicketListResult, PassTicketProfile, PassTicketValidationResult,
};
pub use certificate::{
    CertMapping, CertRc, CertType, CertUsage, Certificate, CertificateManager,
    ChainValidationResult, GencertParams, KeyAlgorithm, Keyring, KeyringConnection, SubjectDN,
    TrustStatus,
};
pub use database::{AuthCheckResult, ListDatasetResult, ListGroupResult, ListUserResult, RacfDatabase, SearchResult};
pub use dataset::DatasetProfile;
pub use error::RacfError;
pub use profile::{GroupProfile, UserProfile};
pub use resource::{
    AuthReason, ClassDescriptor, ConditionalAccess, IbmClasses, ListResourceResult,
    ProgramMember, ResourceAuthResult, ResourceManager, ResourceProfile, StartedData,
    WhenCondition,
};
pub use saf::{SafRouter, SafRc, RacfRc, RacrouteResult, ExtractResult};
pub use setropts::{Setropts, SetroptsListResult, SystemOptions, LogOption, MacSettings, PasswordRules};
pub use exits::{
    AuthExitContext, EventContext, EventType, ExitAction, ExitManager, PasswordExitContext,
    PasswordExitResult, VerifyExitContext,
};
pub use seclabel::{
    MacCheckResult, MacReason, SeclabelManager, SecurityCategory, SecurityLabel, SecurityLevel,
};
pub use utilities::{
    SearchUtilEntry, SearchUtilResult, UnloadRecord, UnloadResult, VerifyResult,
    irrdbu00, irrut100, irrut200,
};

/// Convenience result type for RACF operations.
pub type Result<T> = std::result::Result<T, RacfError>;
