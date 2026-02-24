//! # z/OSMF REST API Server
//!
//! z/OSMF-compatible REST API server for OpenMainframe, enabling Zowe CLI,
//! Zowe Explorer, and Zowe API Mediation Layer to connect to OpenMainframe
//! as if it were a real z/OS instance.
//!
//! ## Features
//!
//! - **HTTPS server** with TLS 1.2+ via rustls
//! - **Authentication** — Basic Auth + JWT tokens backed by RACF
//! - **Dataset REST API** — list, read, write, create, delete datasets and PDS members
//! - **Jobs REST API** — submit JCL, monitor jobs, view spool output
//! - **TSO REST API** — stateless commands and session lifecycle
//! - **Console REST API** — MVS operator commands
//! - **USS File REST API** — host filesystem mapping
//! - **API ML integration** — Eureka dynamic registration
//!
//! ## Example
//!
//! ```rust,no_run
//! use open_mainframe_zosmf::{build_router, config::ZosmfConfig};
//!
//! # async fn example() {
//! let config = ZosmfConfig::default();
//! let router = build_router(config);
//! // router can be served via axum::serve()
//! # }
//! ```

#![forbid(unsafe_code)]

pub mod config;
pub mod eureka;
pub mod handlers;
pub mod jwt;
pub mod middleware;
pub mod mounts;
pub mod state;
pub mod sysplex;
pub mod tn3270;
pub mod types;

pub use config::ZosmfConfig;
pub use state::AppState;

use std::sync::Arc;

/// Build the Axum router with all z/OSMF REST API routes.
pub fn build_router(config: ZosmfConfig) -> axum::Router {
    let state = Arc::new(AppState::new(config));
    handlers::build_router(state)
}

/// Convenience result type for z/OSMF operations.
pub type Result<T> = std::result::Result<T, types::error::ZosmfErrorResponse>;
