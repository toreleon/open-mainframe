//! Configuration management for zOS-clone.

use serde::{Deserialize, Serialize};
use std::env;
use std::path::Path;

/// Main configuration structure.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    /// Server configuration
    #[serde(default)]
    pub server: ServerConfig,

    /// Database configuration
    #[serde(default)]
    pub database: DatabaseConfig,

    /// COBOL runtime configuration
    #[serde(default)]
    pub cobol: CobolConfig,

    /// CICS configuration
    #[serde(default)]
    pub cics: CicsConfig,

    /// IMS configuration
    #[serde(default)]
    pub ims: ImsConfig,

    /// Observability configuration
    #[serde(default)]
    pub observability: ObservabilityConfig,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            server: ServerConfig::default(),
            database: DatabaseConfig::default(),
            cobol: CobolConfig::default(),
            cics: CicsConfig::default(),
            ims: ImsConfig::default(),
            observability: ObservabilityConfig::default(),
        }
    }
}

impl Config {
    /// Load configuration from environment and file.
    pub fn load() -> Result<Self, ConfigError> {
        let mut config = Self::default();

        // Load from config file if exists
        if let Ok(path) = env::var("ZOS_CONFIG_PATH") {
            config = Self::load_from_file(&path)?;
        } else if Path::new("/etc/zos-clone/config.yaml").exists() {
            config = Self::load_from_file("/etc/zos-clone/config.yaml")?;
        }

        // Override with environment variables
        config.apply_env_overrides();

        Ok(config)
    }

    /// Load configuration from a YAML file.
    pub fn load_from_file(path: &str) -> Result<Self, ConfigError> {
        let content = std::fs::read_to_string(path).map_err(ConfigError::IoError)?;
        serde_yaml::from_str(&content).map_err(ConfigError::ParseError)
    }

    /// Apply environment variable overrides.
    fn apply_env_overrides(&mut self) {
        if let Ok(host) = env::var("ZOS_SERVER_HOST") {
            self.server.host = host;
        }
        if let Ok(port) = env::var("ZOS_SERVER_PORT") {
            if let Ok(p) = port.parse() {
                self.server.port = p;
            }
        }
        if let Ok(port) = env::var("ZOS_METRICS_PORT") {
            if let Ok(p) = port.parse() {
                self.server.metrics_port = p;
            }
        }
        if let Ok(url) = env::var("ZOS_DB_URL") {
            self.database.url = url;
        }
        if let Ok(level) = env::var("ZOS_LOG_LEVEL") {
            self.observability.log_level = level;
        }
        if let Ok(format) = env::var("ZOS_LOG_FORMAT") {
            self.observability.log_format = format;
        }
        if let Ok(endpoint) = env::var("OTEL_EXPORTER_OTLP_ENDPOINT") {
            self.observability.otel_endpoint = Some(endpoint);
        }
    }
}

/// Server configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServerConfig {
    /// Host to bind to
    pub host: String,
    /// Port for HTTP
    pub port: u16,
    /// Port for metrics
    pub metrics_port: u16,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            host: "0.0.0.0".to_string(),
            port: 8080,
            metrics_port: 9090,
        }
    }
}

/// Database configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DatabaseConfig {
    /// Database connection URL
    pub url: String,
    /// Connection pool size
    pub pool_size: u32,
    /// Idle timeout in seconds
    pub idle_timeout: u64,
    /// Maximum connection lifetime in seconds
    pub max_lifetime: u64,
}

impl Default for DatabaseConfig {
    fn default() -> Self {
        Self {
            url: "postgresql://localhost:5432/zos".to_string(),
            pool_size: 10,
            idle_timeout: 300,
            max_lifetime: 3600,
        }
    }
}

/// COBOL runtime configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CobolConfig {
    /// Default character encoding
    pub default_encoding: String,
    /// Collating sequence
    pub collating_sequence: String,
}

impl Default for CobolConfig {
    fn default() -> Self {
        Self {
            default_encoding: "EBCDIC".to_string(),
            collating_sequence: "NATIVE".to_string(),
        }
    }
}

/// CICS configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CicsConfig {
    /// Maximum concurrent tasks
    pub max_tasks: u32,
    /// Transaction timeout in seconds
    pub timeout: u32,
}

impl Default for CicsConfig {
    fn default() -> Self {
        Self {
            max_tasks: 100,
            timeout: 30,
        }
    }
}

/// IMS configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImsConfig {
    /// Checkpoint interval (number of calls)
    pub checkpoint_interval: u32,
    /// Maximum segments per call
    pub max_segments: u32,
}

impl Default for ImsConfig {
    fn default() -> Self {
        Self {
            checkpoint_interval: 100,
            max_segments: 1000,
        }
    }
}

/// Observability configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObservabilityConfig {
    /// Log level (trace, debug, info, warn, error)
    pub log_level: String,
    /// Log format (text, json)
    pub log_format: String,
    /// OpenTelemetry collector endpoint
    pub otel_endpoint: Option<String>,
    /// Metrics prefix
    pub metrics_prefix: String,
    /// Tracing sample rate (0.0 - 1.0)
    pub sample_rate: f64,
}

impl Default for ObservabilityConfig {
    fn default() -> Self {
        Self {
            log_level: "info".to_string(),
            log_format: "json".to_string(),
            otel_endpoint: None,
            metrics_prefix: "zos_clone".to_string(),
            sample_rate: 0.1,
        }
    }
}

/// Configuration errors.
#[derive(Debug)]
pub enum ConfigError {
    IoError(std::io::Error),
    ParseError(serde_yaml::Error),
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IoError(e) => write!(f, "IO error: {}", e),
            Self::ParseError(e) => write!(f, "Parse error: {}", e),
        }
    }
}

impl std::error::Error for ConfigError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = Config::default();
        assert_eq!(config.server.port, 8080);
        assert_eq!(config.database.pool_size, 10);
    }

    #[test]
    fn test_server_config_default() {
        let config = ServerConfig::default();
        assert_eq!(config.host, "0.0.0.0");
        assert_eq!(config.port, 8080);
        assert_eq!(config.metrics_port, 9090);
    }

    #[test]
    fn test_observability_config_default() {
        let config = ObservabilityConfig::default();
        assert_eq!(config.log_level, "info");
        assert_eq!(config.log_format, "json");
        assert!(config.otel_endpoint.is_none());
    }
}
