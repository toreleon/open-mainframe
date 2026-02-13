//! Health check endpoints for Kubernetes.

use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

/// Health check status.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct HealthStatus {
    /// Overall health status
    pub status: String,
    /// Version information
    pub version: String,
    /// Uptime in seconds
    pub uptime_seconds: u64,
}

impl HealthStatus {
    /// Create a healthy status.
    pub fn healthy(uptime: Duration) -> Self {
        Self {
            status: "healthy".to_string(),
            version: env!("CARGO_PKG_VERSION").to_string(),
            uptime_seconds: uptime.as_secs(),
        }
    }

    /// Create an unhealthy status.
    pub fn unhealthy(reason: &str, uptime: Duration) -> Self {
        Self {
            status: format!("unhealthy: {}", reason),
            version: env!("CARGO_PKG_VERSION").to_string(),
            uptime_seconds: uptime.as_secs(),
        }
    }
}

/// Readiness check status.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ReadinessStatus {
    /// Overall readiness
    pub ready: bool,
    /// Database connection status
    pub database: ComponentStatus,
    /// CICS region status
    pub cics: ComponentStatus,
    /// IMS region status
    pub ims: ComponentStatus,
}

/// Component status.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ComponentStatus {
    /// Component name
    pub name: String,
    /// Is component ready
    pub ready: bool,
    /// Optional status message
    pub message: Option<String>,
}

impl ComponentStatus {
    /// Create a ready component status.
    pub fn ready(name: &str) -> Self {
        Self {
            name: name.to_string(),
            ready: true,
            message: None,
        }
    }

    /// Create a not-ready component status.
    pub fn not_ready(name: &str, message: &str) -> Self {
        Self {
            name: name.to_string(),
            ready: false,
            message: Some(message.to_string()),
        }
    }
}

/// Health checker for Kubernetes probes.
#[derive(Debug)]
pub struct HealthChecker {
    /// Start time
    start_time: Instant,
    /// Database ready flag
    database_ready: Arc<AtomicBool>,
    /// CICS ready flag
    cics_ready: Arc<AtomicBool>,
    /// IMS ready flag
    ims_ready: Arc<AtomicBool>,
}

impl Default for HealthChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl HealthChecker {
    /// Create a new health checker.
    pub fn new() -> Self {
        Self {
            start_time: Instant::now(),
            database_ready: Arc::new(AtomicBool::new(false)),
            cics_ready: Arc::new(AtomicBool::new(true)),
            ims_ready: Arc::new(AtomicBool::new(true)),
        }
    }

    /// Get uptime duration.
    pub fn uptime(&self) -> Duration {
        self.start_time.elapsed()
    }

    /// Set database ready status.
    pub fn set_database_ready(&self, ready: bool) {
        self.database_ready.store(ready, Ordering::SeqCst);
    }

    /// Set CICS ready status.
    pub fn set_cics_ready(&self, ready: bool) {
        self.cics_ready.store(ready, Ordering::SeqCst);
    }

    /// Set IMS ready status.
    pub fn set_ims_ready(&self, ready: bool) {
        self.ims_ready.store(ready, Ordering::SeqCst);
    }

    /// Check liveness (is the process alive and responding).
    pub fn check_liveness(&self) -> HealthStatus {
        HealthStatus::healthy(self.uptime())
    }

    /// Check readiness (is the service ready to accept traffic).
    pub fn check_readiness(&self) -> ReadinessStatus {
        let db_ready = self.database_ready.load(Ordering::SeqCst);
        let cics_ready = self.cics_ready.load(Ordering::SeqCst);
        let ims_ready = self.ims_ready.load(Ordering::SeqCst);

        let database = if db_ready {
            ComponentStatus::ready("database")
        } else {
            ComponentStatus::not_ready("database", "Database connection not established")
        };

        let cics = if cics_ready {
            ComponentStatus::ready("cics")
        } else {
            ComponentStatus::not_ready("cics", "CICS region not initialized")
        };

        let ims = if ims_ready {
            ComponentStatus::ready("ims")
        } else {
            ComponentStatus::not_ready("ims", "IMS region not initialized")
        };

        ReadinessStatus {
            ready: db_ready && cics_ready && ims_ready,
            database,
            cics,
            ims,
        }
    }

    /// Get the database_ready flag for sharing with other components.
    pub fn database_ready_flag(&self) -> Arc<AtomicBool> {
        Arc::clone(&self.database_ready)
    }

    /// Get the CICS ready flag for sharing.
    pub fn cics_ready_flag(&self) -> Arc<AtomicBool> {
        Arc::clone(&self.cics_ready)
    }

    /// Get the IMS ready flag for sharing.
    pub fn ims_ready_flag(&self) -> Arc<AtomicBool> {
        Arc::clone(&self.ims_ready)
    }
}

impl Clone for HealthChecker {
    fn clone(&self) -> Self {
        Self {
            start_time: self.start_time,
            database_ready: Arc::clone(&self.database_ready),
            cics_ready: Arc::clone(&self.cics_ready),
            ims_ready: Arc::clone(&self.ims_ready),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_health_status_healthy() {
        let status = HealthStatus::healthy(Duration::from_secs(100));
        assert_eq!(status.status, "healthy");
        assert_eq!(status.uptime_seconds, 100);
    }

    #[test]
    fn test_health_status_unhealthy() {
        let status = HealthStatus::unhealthy("test error", Duration::from_secs(50));
        assert!(status.status.contains("unhealthy"));
        assert!(status.status.contains("test error"));
    }

    #[test]
    fn test_health_checker_liveness() {
        let checker = HealthChecker::new();
        let status = checker.check_liveness();
        assert_eq!(status.status, "healthy");
    }

    #[test]
    fn test_health_checker_readiness_not_ready() {
        let checker = HealthChecker::new();
        let status = checker.check_readiness();
        assert!(!status.ready);
        assert!(!status.database.ready);
    }

    #[test]
    fn test_health_checker_readiness_ready() {
        let checker = HealthChecker::new();
        checker.set_database_ready(true);
        checker.set_cics_ready(true);
        checker.set_ims_ready(true);

        let status = checker.check_readiness();
        assert!(status.ready);
        assert!(status.database.ready);
        assert!(status.cics.ready);
        assert!(status.ims.ready);
    }

    #[test]
    fn test_component_status() {
        let ready = ComponentStatus::ready("test");
        assert!(ready.ready);
        assert!(ready.message.is_none());

        let not_ready = ComponentStatus::not_ready("test", "error");
        assert!(!not_ready.ready);
        assert_eq!(not_ready.message, Some("error".to_string()));
    }

    #[test]
    fn test_health_checker_clone() {
        let checker = HealthChecker::new();
        checker.set_database_ready(true);

        let cloned = checker.clone();
        assert!(cloned.database_ready.load(Ordering::SeqCst));

        // Changes to one should affect the other
        cloned.set_database_ready(false);
        assert!(!checker.database_ready.load(Ordering::SeqCst));
    }
}
