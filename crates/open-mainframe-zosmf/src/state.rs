//! Shared application state for all z/OSMF handlers.

use std::sync::{Arc, RwLock};

use dashmap::DashMap;
use open_mainframe_dataset::Catalog;
use open_mainframe_jes2::Jes2;
use open_mainframe_parmlib::{ParmlibConcat, ParserRegistry};
use open_mainframe_racf::{AuthService, RacfDatabase, SafRouter};
use open_mainframe_wlm::{DisplayWlmResponse, PolicyStore, WlmMode};

use open_mainframe_cics::bms::BmsMapset;
use open_mainframe_cics::terminal::TerminalManager;

use crate::config::ZosmfConfig;
use crate::types::auth::AuthenticatedUser;

/// A workflow instance tracked by the z/OSMF workflow service.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WorkflowInstance {
    pub workflow_key: String,
    pub workflow_name: String,
    pub workflow_description: String,
    pub owner: String,
    pub status: String,
    pub created: String,
    pub system: String,
}

/// A published software service template.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct ProvisioningTemplate {
    pub name: String,
    pub description: String,
    pub version: String,
    pub domain_name: String,
    pub state: String,
}

/// A provisioned software service instance.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct ProvisionedInstance {
    pub object_id: String,
    pub name: String,
    pub template_name: String,
    pub state: String,
    pub owner: String,
    pub created: String,
    pub system: String,
}

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
    /// Console command responses: response-key → response text.
    pub console_responses: DashMap<String, String>,
    /// WLM display response (current state).
    pub wlm: RwLock<DisplayWlmResponse>,
    /// WLM policy store (install/activate policies).
    pub wlm_policy_store: RwLock<PolicyStore>,
    /// Parmlib concatenation.
    pub parmlib: RwLock<ParmlibConcat>,
    /// Parmlib parser registry.
    pub parmlib_registry: RwLock<ParserRegistry>,
    /// Workflow instances.
    pub workflows: DashMap<String, WorkflowInstance>,
    /// Provisioning templates (published software service catalog).
    pub provisioning_templates: DashMap<String, ProvisioningTemplate>,
    /// Provisioned instances (software service instances).
    pub provisioning_instances: DashMap<String, ProvisionedInstance>,
    /// CICS terminal sessions: session key → session handle.
    pub cics_sessions: DashMap<String, CicsSessionHandle>,
}

/// Handle to a CICS terminal session.
pub struct CicsSessionHandle {
    /// Terminal manager managing this session's terminal.
    pub terminal_manager: TerminalManager,
    /// The terminal ID for this session.
    pub terminal_id: String,
    /// Loaded BMS mapsets (name → parsed mapset).
    pub mapsets: std::collections::HashMap<String, BmsMapset>,
    /// Currently displayed mapset name.
    pub current_mapset: Option<String>,
    /// Currently displayed map name.
    pub current_map: Option<String>,
    /// Owner userid.
    pub userid: String,
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
            console_responses: DashMap::new(),
            wlm: RwLock::new(DisplayWlmResponse {
                active_policy: Some("DEFAULT".to_string()),
                mode: WlmMode::Goal,
                classes: Vec::new(),
                resource_groups: Vec::new(),
                initiator_count: 0,
                enclave_count: 0,
            }),
            wlm_policy_store: RwLock::new(PolicyStore::new()),
            parmlib: RwLock::new(ParmlibConcat::new(Vec::new())),
            parmlib_registry: RwLock::new(ParserRegistry::with_builtins()),
            workflows: DashMap::new(),
            provisioning_templates: DashMap::new(),
            provisioning_instances: DashMap::new(),
            cics_sessions: DashMap::new(),
        }
    }
}
