//! z/OSMF server configuration — port, TLS, auth, USS, CORS, and system info settings.

use serde::{Deserialize, Serialize};

/// Top-level z/OSMF server configuration.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ZosmfConfig {
    /// HTTP/HTTPS server settings.
    #[serde(default)]
    pub server: ServerConfig,
    /// TLS certificate settings.
    #[serde(default)]
    pub tls: Option<TlsConfig>,
    /// Authentication and token settings.
    #[serde(default)]
    pub auth: AuthConfig,
    /// USS filesystem mapping settings.
    #[serde(default)]
    pub uss: UssConfig,
    /// CORS settings.
    #[serde(default)]
    pub cors: CorsConfig,
    /// z/OSMF info endpoint values.
    #[serde(default)]
    pub zosmf_info: ZosmfInfoConfig,
    /// Sysplex configuration (multi-system support).
    #[serde(default)]
    pub sysplex: Option<crate::sysplex::SysplexConfig>,
    /// External filesystem mounts.
    #[serde(default)]
    pub mounts: Vec<MountConfig>,
    /// CICS application configuration.
    #[serde(default)]
    pub cics: CicsConfig,
}

/// Configuration for a single mount entry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MountConfig {
    /// Mount type: "dataset-pds", "dataset-seq", or "uss".
    #[serde(rename = "type")]
    pub mount_type: String,
    /// Host filesystem path.
    pub host_path: String,
    /// Virtual path in OpenMainframe (DSN or USS path).
    pub virtual_path: String,
    /// Whether the mount is read-only.
    #[serde(default)]
    pub read_only: bool,
    /// Glob pattern to filter files (e.g., "*.cbl").
    #[serde(default)]
    pub file_filter: Option<String>,
}

/// Server binding configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServerConfig {
    /// Bind address.
    #[serde(default = "default_host")]
    pub host: String,
    /// Listen port.
    #[serde(default = "default_port")]
    pub port: u16,
}

/// TLS configuration with PEM certificate and key paths.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TlsConfig {
    /// Path to PEM certificate file.
    pub cert_file: String,
    /// Path to PEM private key file.
    pub key_file: String,
}

/// Authentication token configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuthConfig {
    /// JWT token time-to-live in seconds.
    #[serde(default = "default_token_ttl")]
    pub token_ttl_seconds: u64,
    /// JWT signing algorithm (HS256 or RS256).
    #[serde(default = "default_token_algorithm")]
    pub token_algorithm: String,
    /// Secret key for HS256 signing.
    #[serde(default = "default_token_secret")]
    pub token_secret: String,
}

/// USS filesystem mapping configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UssConfig {
    /// Root directory on host that maps to USS `/u/`.
    #[serde(default = "default_uss_root")]
    pub root_directory: String,
}

/// CORS configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CorsConfig {
    /// Allowed origins (use `["*"]` for permissive).
    #[serde(default = "default_cors_origins")]
    pub allowed_origins: Vec<String>,
}

/// Values returned by the `/zosmf/info` endpoint.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ZosmfInfoConfig {
    /// Hostname reported by z/OSMF info.
    #[serde(default = "default_hostname")]
    pub hostname: String,
    /// SAF realm name.
    #[serde(default = "default_saf_realm")]
    pub saf_realm: String,
}

impl ZosmfConfig {
    /// Load configuration from a TOML file.
    pub fn from_file(path: &str) -> std::result::Result<Self, Box<dyn std::error::Error>> {
        let content = std::fs::read_to_string(path)?;
        let config: Self = toml::from_str(&content)?;
        Ok(config)
    }
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            host: default_host(),
            port: default_port(),
        }
    }
}

impl Default for AuthConfig {
    fn default() -> Self {
        Self {
            token_ttl_seconds: default_token_ttl(),
            token_algorithm: default_token_algorithm(),
            token_secret: default_token_secret(),
        }
    }
}

impl Default for UssConfig {
    fn default() -> Self {
        Self {
            root_directory: default_uss_root(),
        }
    }
}

impl Default for CorsConfig {
    fn default() -> Self {
        Self {
            allowed_origins: default_cors_origins(),
        }
    }
}

impl Default for ZosmfInfoConfig {
    fn default() -> Self {
        Self {
            hostname: default_hostname(),
            saf_realm: default_saf_realm(),
        }
    }
}

fn default_host() -> String {
    "0.0.0.0".to_string()
}

fn default_port() -> u16 {
    10443
}

fn default_token_ttl() -> u64 {
    28800 // 8 hours
}

fn default_token_algorithm() -> String {
    "HS256".to_string()
}

fn default_token_secret() -> String {
    "openmainframe-default-secret-change-me".to_string()
}

fn default_uss_root() -> String {
    "/opt/openmainframe/uss".to_string()
}

fn default_cors_origins() -> Vec<String> {
    vec!["*".to_string()]
}

fn default_hostname() -> String {
    "openmainframe-host".to_string()
}

fn default_saf_realm() -> String {
    "SAFRealm".to_string()
}

fn default_cics_timeout() -> u64 {
    1800 // 30 minutes
}

/// CICS application server configuration.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CicsConfig {
    /// Named CICS application profiles (e.g., "CARDDEMO").
    #[serde(default)]
    pub apps: std::collections::HashMap<String, CicsAppProfile>,
    /// Default application name when POST body omits `appName`.
    #[serde(default)]
    pub default_app: Option<String>,
    /// Session idle timeout in seconds (default: 1800 = 30 min).
    #[serde(default = "default_cics_timeout")]
    pub session_timeout_seconds: u64,
    /// System-level CICS copybook paths (DFHAID, DFHBMSCA, DFHEIBLK).
    /// Automatically appended to every app's include_paths.
    #[serde(default)]
    pub system_copybooks: Vec<String>,
}

/// A named CICS application profile loaded from `zosmf.toml`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CicsAppProfile {
    /// Path to the initial COBOL program.
    pub program: String,
    /// Copybook include paths.
    #[serde(default)]
    pub include_paths: Vec<String>,
    /// BMS map directory.
    #[serde(default)]
    pub bms_dir: Option<String>,
    /// VSAM data files (format: `DDNAME=path[:key_len[:rec_len]]`).
    #[serde(default)]
    pub data_files: Vec<String>,
    /// TRANSID → program name mappings.
    #[serde(default)]
    pub transids: std::collections::HashMap<String, String>,
    /// Directory to search for XCTL target programs.
    #[serde(default)]
    pub program_dir: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = ZosmfConfig::default();
        assert_eq!(config.server.port, 10443);
        assert_eq!(config.auth.token_ttl_seconds, 28800);
        assert_eq!(config.auth.token_algorithm, "HS256");
        assert!(config.tls.is_none());
    }

    #[test]
    fn test_config_from_toml() {
        let toml_str = r#"
[server]
host = "127.0.0.1"
port = 8443

[auth]
token_ttl_seconds = 3600
token_algorithm = "HS256"
token_secret = "mysecret"

[uss]
root_directory = "/tmp/uss"

[cors]
allowed_origins = ["https://example.com"]

[zosmf_info]
hostname = "testhost"
saf_realm = "TestRealm"
"#;
        let config: ZosmfConfig = toml::from_str(toml_str).unwrap();
        assert_eq!(config.server.host, "127.0.0.1");
        assert_eq!(config.server.port, 8443);
        assert_eq!(config.auth.token_ttl_seconds, 3600);
        assert_eq!(config.uss.root_directory, "/tmp/uss");
        assert_eq!(config.zosmf_info.hostname, "testhost");
    }
}
