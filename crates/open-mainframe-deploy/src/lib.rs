//! Deployment, health checks, and observability for OpenMainframe.
//!
//! This crate provides:
//! - Health check endpoints for Kubernetes
//! - Prometheus metrics
//! - OpenTelemetry tracing
//! - Structured logging
//!
//! # Example
//!
//! ```ignore
//! use open_mainframe_deploy::{HealthChecker, Metrics, init_tracing};
//!
//! // Initialize observability
//! init_tracing()?;
//!
//! // Create health checker
//! let health = HealthChecker::new();
//! health.set_database_ready(true);
//!
//! // Record metrics
//! let metrics = Metrics::new();
//! metrics.record_request("cobol", 150.0);
//! ```

mod config;
mod health;
pub mod instrumentation;
pub mod k8s_manifest;
mod metrics;
pub mod secrets;
pub mod server;
pub mod trace_context;
mod tracing_setup;

pub use config::{Config, DatabaseConfig, ObservabilityConfig, ServerConfig};
pub use health::{HealthChecker, HealthStatus, ReadinessStatus};
pub use instrumentation::{
    CicsInstrumentation, CobolInstrumentation, ImsInstrumentation, InstrumentedRuntime,
};
pub use metrics::{
    CicsMetrics, CobolMetrics, DatabaseMetrics, ImsMetrics, Metrics, MetricsRegistry,
};
pub use k8s_manifest::{generate_manifests, GeneratedManifests, ManifestOverrides};
pub use secrets::{
    CredentialSource, DatabaseCredentials, ResolvedCredentials, SecretsResolver,
    DEFAULT_SECRET_MOUNT_PATH,
};
pub use server::{start_servers, ServerHandle};
pub use trace_context::{
    SpanId, SpanKind, TraceId, TraceSpan, TransactionTrace,
};
pub use tracing_setup::{init_tracing, LogFormat, TracingConfig};

/// Re-export prometheus for custom metrics
pub use prometheus;

/// Re-export tracing for instrumentation
pub use tracing;
