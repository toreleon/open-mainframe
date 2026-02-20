//! Shared application state for all z/OSMF handlers.

use std::sync::{Arc, RwLock};

use dashmap::DashMap;
use open_mainframe_dataset::Catalog;
use open_mainframe_jes2::Jes2;
use open_mainframe_racf::{AuthService, RacfDatabase, SafRouter};

use crate::config::ZosmfConfig;
use crate::types::auth::AuthenticatedUser;

/// Shared state available to all Axum handlers via `State<Arc<AppState>>`.
pub struct AppState {
    /// RACF security database.
    pub racf: RacfDatabase,
    /// SAF router for authorization checks.
    pub saf: SafRouter,
    /// RACF authentication service.
    pub auth_service: AuthService,
    /// Dataset catalog (RwLock for mutable list/create/delete).
    pub catalog: RwLock<Catalog>,
    /// JES2 job entry subsystem (RwLock for submit/cancel/purge).
    pub jes2: RwLock<Jes2>,
    /// TSO sessions: servlet key → session handle.
    pub tso_sessions: DashMap<String, TsoSessionHandle>,
    /// Token store: JWT token → authenticated user.
    pub token_store: DashMap<String, AuthenticatedUser>,
    /// Server configuration.
    pub config: Arc<ZosmfConfig>,
}

/// Handle to a TSO session stored in the session map.
#[derive(Debug)]
pub struct TsoSessionHandle {
    /// The TSO session.
    pub session: open_mainframe_tso::TsoSession,
    /// Output buffer for async responses.
    pub output_buffer: Vec<String>,
    /// Owner userid.
    pub userid: String,
}

impl AppState {
    /// Create a new AppState with default subsystem instances.
    pub fn new(config: ZosmfConfig) -> Self {
        Self {
            racf: RacfDatabase::new(),
            saf: SafRouter::new(),
            auth_service: AuthService::new(),
            catalog: RwLock::new(Catalog::default()),
            jes2: RwLock::new(Jes2::new()),
            tso_sessions: DashMap::new(),
            token_store: DashMap::new(),
            config: Arc::new(config),
        }
    }
}
