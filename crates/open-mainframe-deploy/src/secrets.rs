//! Secrets management for database credentials and TLS certificates.
//!
//! Loads credentials from Kubernetes Secret volume mounts, with fallback
//! to environment variables and config-file values.

use std::env;
use std::path::{Path, PathBuf};

/// Default mount path for Kubernetes secrets.
pub const DEFAULT_SECRET_MOUNT_PATH: &str = "/etc/open-mainframe/secrets";

/// Database credentials resolved from secrets, env vars, or config.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DatabaseCredentials {
    /// Database connection URL.
    pub url: String,
    /// Database username (if separate from URL).
    pub username: Option<String>,
    /// Database password (if separate from URL).
    pub password: Option<String>,
}

/// Source from which credentials were resolved.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CredentialSource {
    /// Loaded from Kubernetes Secret volume mount.
    SecretMount,
    /// Loaded from environment variables.
    Environment,
    /// Loaded from configuration file / defaults.
    Config,
}

/// Result of credential resolution.
#[derive(Debug, Clone)]
pub struct ResolvedCredentials {
    /// The resolved credentials.
    pub credentials: DatabaseCredentials,
    /// Where the credentials came from.
    pub source: CredentialSource,
}

/// Secrets resolver that checks mounted secrets, then env vars, then config.
pub struct SecretsResolver {
    /// Path to the mounted secrets directory.
    mount_path: PathBuf,
}

impl Default for SecretsResolver {
    fn default() -> Self {
        Self::new()
    }
}

impl SecretsResolver {
    /// Create a new resolver with the default mount path.
    pub fn new() -> Self {
        Self {
            mount_path: PathBuf::from(DEFAULT_SECRET_MOUNT_PATH),
        }
    }

    /// Create a resolver with a custom mount path.
    pub fn with_mount_path(path: impl Into<PathBuf>) -> Self {
        Self {
            mount_path: path.into(),
        }
    }

    /// Get the configured mount path.
    pub fn mount_path(&self) -> &Path {
        &self.mount_path
    }

    /// Resolve database credentials using the priority chain:
    /// 1. Kubernetes Secret mount files (url, username, password)
    /// 2. Environment variables (OPEN_MAINFRAME_DB_URL, etc.)
    /// 3. Provided fallback config values
    pub fn resolve_database_credentials(
        &self,
        fallback_url: &str,
    ) -> ResolvedCredentials {
        // Priority 1: Kubernetes Secret volume mount
        if let Some(creds) = self.try_secret_mount() {
            return ResolvedCredentials {
                credentials: creds,
                source: CredentialSource::SecretMount,
            };
        }

        // Priority 2: Environment variables
        if let Some(creds) = Self::try_environment() {
            return ResolvedCredentials {
                credentials: creds,
                source: CredentialSource::Environment,
            };
        }

        // Priority 3: Config fallback
        ResolvedCredentials {
            credentials: DatabaseCredentials {
                url: fallback_url.to_string(),
                username: None,
                password: None,
            },
            source: CredentialSource::Config,
        }
    }

    /// Try to load credentials from Kubernetes Secret volume mount.
    fn try_secret_mount(&self) -> Option<DatabaseCredentials> {
        let url_path = self.mount_path.join("url");
        if !url_path.exists() {
            return None;
        }

        let url = read_secret_file(&url_path)?;
        let username = read_secret_file(&self.mount_path.join("username"));
        let password = read_secret_file(&self.mount_path.join("password"));

        Some(DatabaseCredentials {
            url,
            username,
            password,
        })
    }

    /// Try to load credentials from environment variables.
    fn try_environment() -> Option<DatabaseCredentials> {
        let url = env::var("OPEN_MAINFRAME_DB_URL").ok()?;
        let username = env::var("OPEN_MAINFRAME_DB_USERNAME").ok();
        let password = env::var("OPEN_MAINFRAME_DB_PASSWORD").ok();

        Some(DatabaseCredentials {
            url,
            username,
            password,
        })
    }

    /// Check if secret files are present at the mount path.
    pub fn has_secret_mount(&self) -> bool {
        self.mount_path.join("url").exists()
    }
}

/// Read a secret file, trimming whitespace/newlines.
fn read_secret_file(path: &Path) -> Option<String> {
    std::fs::read_to_string(path)
        .ok()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    /// Helper to create a temp directory with secret files.
    fn create_secret_dir(url: &str, username: Option<&str>, password: Option<&str>) -> tempfile::TempDir {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("url"), url).unwrap();
        if let Some(u) = username {
            fs::write(dir.path().join("username"), u).unwrap();
        }
        if let Some(p) = password {
            fs::write(dir.path().join("password"), p).unwrap();
        }
        dir
    }

    #[test]
    fn test_resolve_from_secret_mount() {
        let dir = create_secret_dir(
            "postgresql://db:5432/prod",
            Some("admin"),
            Some("s3cret"),
        );
        let resolver = SecretsResolver::with_mount_path(dir.path());
        let result = resolver.resolve_database_credentials("fallback://url");

        assert_eq!(result.source, CredentialSource::SecretMount);
        assert_eq!(result.credentials.url, "postgresql://db:5432/prod");
        assert_eq!(result.credentials.username.as_deref(), Some("admin"));
        assert_eq!(result.credentials.password.as_deref(), Some("s3cret"));
    }

    #[test]
    fn test_resolve_from_secret_mount_url_only() {
        let dir = create_secret_dir("postgresql://db:5432/prod", None, None);
        let resolver = SecretsResolver::with_mount_path(dir.path());
        let result = resolver.resolve_database_credentials("fallback://url");

        assert_eq!(result.source, CredentialSource::SecretMount);
        assert_eq!(result.credentials.url, "postgresql://db:5432/prod");
        assert!(result.credentials.username.is_none());
        assert!(result.credentials.password.is_none());
    }

    #[test]
    fn test_resolve_fallback_to_config() {
        // No mount, no env vars (env vars might be set by other tests, use a unique resolver)
        let dir = tempfile::tempdir().unwrap();
        // Don't create any files — simulates no secret mount
        let resolver = SecretsResolver::with_mount_path(dir.path());

        // Clear env vars for this test (save/restore handled by test isolation)
        let saved_url = env::var("OPEN_MAINFRAME_DB_URL").ok();
        env::remove_var("OPEN_MAINFRAME_DB_URL");

        let result = resolver.resolve_database_credentials("postgresql://localhost/test");

        // Restore
        if let Some(v) = saved_url {
            env::set_var("OPEN_MAINFRAME_DB_URL", v);
        }

        assert_eq!(result.source, CredentialSource::Config);
        assert_eq!(result.credentials.url, "postgresql://localhost/test");
    }

    #[test]
    fn test_has_secret_mount() {
        let dir = create_secret_dir("postgresql://db:5432/prod", None, None);
        let resolver = SecretsResolver::with_mount_path(dir.path());
        assert!(resolver.has_secret_mount());

        let empty_dir = tempfile::tempdir().unwrap();
        let resolver2 = SecretsResolver::with_mount_path(empty_dir.path());
        assert!(!resolver2.has_secret_mount());
    }

    #[test]
    fn test_secret_file_trimming() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("url"), "  postgresql://db:5432/prod  \n").unwrap();
        fs::write(dir.path().join("username"), "admin\n").unwrap();

        let resolver = SecretsResolver::with_mount_path(dir.path());
        let result = resolver.resolve_database_credentials("fallback");

        assert_eq!(result.credentials.url, "postgresql://db:5432/prod");
        assert_eq!(result.credentials.username.as_deref(), Some("admin"));
    }

    #[test]
    fn test_empty_secret_file_ignored() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("url"), "postgresql://db/prod").unwrap();
        fs::write(dir.path().join("username"), "").unwrap(); // empty
        fs::write(dir.path().join("password"), "  \n").unwrap(); // whitespace only

        let resolver = SecretsResolver::with_mount_path(dir.path());
        let result = resolver.resolve_database_credentials("fallback");

        assert_eq!(result.source, CredentialSource::SecretMount);
        assert!(result.credentials.username.is_none());
        assert!(result.credentials.password.is_none());
    }

    #[test]
    fn test_default_mount_path() {
        let resolver = SecretsResolver::new();
        assert_eq!(
            resolver.mount_path().to_str().unwrap(),
            "/etc/open-mainframe/secrets"
        );
    }

    #[test]
    fn test_credential_source_equality() {
        assert_eq!(CredentialSource::SecretMount, CredentialSource::SecretMount);
        assert_ne!(CredentialSource::SecretMount, CredentialSource::Environment);
        assert_ne!(CredentialSource::Environment, CredentialSource::Config);
    }

    #[test]
    fn test_database_credentials_equality() {
        let a = DatabaseCredentials {
            url: "pg://localhost".to_string(),
            username: Some("user".to_string()),
            password: None,
        };
        let b = a.clone();
        assert_eq!(a, b);
    }
}
