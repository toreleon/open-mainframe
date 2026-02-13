//! Configuration system for zOS-clone.
//!
//! Supports loading configuration from:
//! - `zos-clone.toml` in current directory
//! - `~/.config/zos-clone/config.toml` for user defaults
//! - Environment variables (ZOS_CLONE_*)
//! - Command-line arguments (highest priority)

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

/// Complete configuration for zOS-clone.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct Config {
    /// Compiler settings.
    pub compiler: CompilerConfig,
    /// Runtime settings.
    pub runtime: RuntimeConfig,
    /// Dataset settings.
    pub dataset: DatasetConfig,
    /// JCL execution settings.
    pub jcl: JclConfig,
}

/// Compiler configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct CompilerConfig {
    /// Source format (fixed, free, auto).
    pub source_format: String,
    /// Copybook search paths.
    pub copybook_paths: Vec<PathBuf>,
    /// Default optimization level (0-3).
    pub optimization: u8,
    /// Enable all warnings.
    pub all_warnings: bool,
    /// Treat warnings as errors.
    pub warnings_as_errors: bool,
    /// Generate debug information.
    pub debug_info: bool,
    /// Target architecture.
    pub target: Option<String>,
    /// COBOL dialect (ibm, gnucobol, mf).
    pub dialect: String,
    /// Enable specific extensions.
    pub extensions: Vec<String>,
}

impl Default for CompilerConfig {
    fn default() -> Self {
        Self {
            source_format: "fixed".to_string(),
            copybook_paths: vec![
                PathBuf::from("."),
                PathBuf::from("./copybooks"),
                PathBuf::from("./copy"),
            ],
            optimization: 0,
            all_warnings: false,
            warnings_as_errors: false,
            debug_info: true,
            target: None,
            dialect: "ibm".to_string(),
            extensions: vec![],
        }
    }
}

/// Runtime configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct RuntimeConfig {
    /// Maximum decimal precision.
    pub decimal_precision: u8,
    /// Enable runtime bounds checking.
    pub bounds_checking: bool,
    /// Enable numeric overflow detection.
    pub overflow_detection: bool,
    /// Default DISPLAY output target.
    pub display_target: String,
    /// Default ACCEPT input source.
    pub accept_source: String,
    /// Environment variables to pass to programs.
    pub environment: HashMap<String, String>,
}

impl Default for RuntimeConfig {
    fn default() -> Self {
        Self {
            decimal_precision: 18,
            bounds_checking: true,
            overflow_detection: true,
            display_target: "stdout".to_string(),
            accept_source: "stdin".to_string(),
            environment: HashMap::new(),
        }
    }
}

/// Dataset configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct DatasetConfig {
    /// Base directory for datasets.
    pub base_dir: PathBuf,
    /// File extensions to search for datasets.
    pub extensions: Vec<String>,
    /// Default record format.
    pub default_recfm: String,
    /// Default logical record length.
    pub default_lrecl: u32,
    /// Default block size.
    pub default_blksize: u32,
}

impl Default for DatasetConfig {
    fn default() -> Self {
        Self {
            base_dir: PathBuf::from("./datasets"),
            extensions: vec![
                String::new(),
                ".dat".to_string(),
                ".txt".to_string(),
                ".cbl".to_string(),
                ".cob".to_string(),
                ".cpy".to_string(),
            ],
            default_recfm: "FB".to_string(),
            default_lrecl: 80,
            default_blksize: 800,
        }
    }
}

/// JCL execution configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct JclConfig {
    /// Directory containing compiled programs.
    pub program_dir: PathBuf,
    /// Working directory for job execution.
    pub work_dir: PathBuf,
    /// Sysout directory.
    pub sysout_dir: PathBuf,
    /// Maximum job execution time (seconds).
    pub max_time: u64,
    /// Default job class.
    pub default_class: String,
    /// Default message class.
    pub default_msgclass: String,
}

impl Default for JclConfig {
    fn default() -> Self {
        Self {
            program_dir: PathBuf::from("./bin"),
            work_dir: PathBuf::from("./work"),
            sysout_dir: PathBuf::from("./sysout"),
            max_time: 300,
            default_class: "A".to_string(),
            default_msgclass: "X".to_string(),
        }
    }
}

impl Config {
    /// Load configuration from default locations.
    pub fn load() -> Self {
        let mut config = Config::default();

        // Try user config first
        if let Some(user_config) = Self::user_config_path() {
            if let Ok(c) = Self::load_from_file(&user_config) {
                config = config.merge(c);
            }
        }

        // Then project config (higher priority)
        if let Ok(c) = Self::load_from_file(Path::new("zos-clone.toml")) {
            config = config.merge(c);
        }

        // Then environment variables (highest priority)
        config.apply_env_vars();

        config
    }

    /// Load configuration from a specific file.
    pub fn load_from_file(path: &Path) -> Result<Self, ConfigError> {
        let content = std::fs::read_to_string(path).map_err(|e| ConfigError::IoError {
            path: path.to_path_buf(),
            message: e.to_string(),
        })?;

        toml::from_str(&content).map_err(|e| ConfigError::ParseError {
            path: path.to_path_buf(),
            message: e.to_string(),
        })
    }

    /// Get the user configuration file path.
    pub fn user_config_path() -> Option<PathBuf> {
        dirs::config_dir().map(|d| d.join("zos-clone").join("config.toml"))
    }

    /// Merge another config into this one (other takes priority).
    pub fn merge(mut self, other: Config) -> Self {
        // Merge compiler config
        if !other.compiler.copybook_paths.is_empty() {
            self.compiler.copybook_paths = other.compiler.copybook_paths;
        }
        if other.compiler.source_format != "fixed" {
            self.compiler.source_format = other.compiler.source_format;
        }
        if other.compiler.optimization != 0 {
            self.compiler.optimization = other.compiler.optimization;
        }
        self.compiler.all_warnings = other.compiler.all_warnings;
        self.compiler.warnings_as_errors = other.compiler.warnings_as_errors;
        self.compiler.debug_info = other.compiler.debug_info;
        if other.compiler.target.is_some() {
            self.compiler.target = other.compiler.target;
        }
        if other.compiler.dialect != "ibm" {
            self.compiler.dialect = other.compiler.dialect;
        }
        if !other.compiler.extensions.is_empty() {
            self.compiler.extensions = other.compiler.extensions;
        }

        // Merge runtime config
        if other.runtime.decimal_precision != 18 {
            self.runtime.decimal_precision = other.runtime.decimal_precision;
        }
        self.runtime.bounds_checking = other.runtime.bounds_checking;
        self.runtime.overflow_detection = other.runtime.overflow_detection;
        if other.runtime.display_target != "stdout" {
            self.runtime.display_target = other.runtime.display_target;
        }
        if other.runtime.accept_source != "stdin" {
            self.runtime.accept_source = other.runtime.accept_source;
        }
        self.runtime.environment.extend(other.runtime.environment);

        // Merge dataset config
        if other.dataset.base_dir != PathBuf::from("./datasets") {
            self.dataset.base_dir = other.dataset.base_dir;
        }
        if !other.dataset.extensions.is_empty()
            && other.dataset.extensions != DatasetConfig::default().extensions
        {
            self.dataset.extensions = other.dataset.extensions;
        }
        if other.dataset.default_recfm != "FB" {
            self.dataset.default_recfm = other.dataset.default_recfm;
        }
        if other.dataset.default_lrecl != 80 {
            self.dataset.default_lrecl = other.dataset.default_lrecl;
        }
        if other.dataset.default_blksize != 800 {
            self.dataset.default_blksize = other.dataset.default_blksize;
        }

        // Merge JCL config
        if other.jcl.program_dir != PathBuf::from("./bin") {
            self.jcl.program_dir = other.jcl.program_dir;
        }
        if other.jcl.work_dir != PathBuf::from("./work") {
            self.jcl.work_dir = other.jcl.work_dir;
        }
        if other.jcl.sysout_dir != PathBuf::from("./sysout") {
            self.jcl.sysout_dir = other.jcl.sysout_dir;
        }
        if other.jcl.max_time != 300 {
            self.jcl.max_time = other.jcl.max_time;
        }

        self
    }

    /// Apply environment variables to configuration.
    fn apply_env_vars(&mut self) {
        if let Ok(val) = std::env::var("ZOS_CLONE_SOURCE_FORMAT") {
            self.compiler.source_format = val;
        }
        if let Ok(val) = std::env::var("ZOS_CLONE_COPYBOOK_PATH") {
            self.compiler.copybook_paths = val
                .split(':')
                .map(PathBuf::from)
                .collect();
        }
        if let Ok(val) = std::env::var("ZOS_CLONE_OPTIMIZATION") {
            if let Ok(level) = val.parse() {
                self.compiler.optimization = level;
            }
        }
        if let Ok(val) = std::env::var("ZOS_CLONE_DATASET_DIR") {
            self.dataset.base_dir = PathBuf::from(val);
        }
        if let Ok(val) = std::env::var("ZOS_CLONE_PROGRAM_DIR") {
            self.jcl.program_dir = PathBuf::from(val);
        }
        if let Ok(val) = std::env::var("ZOS_CLONE_WORK_DIR") {
            self.jcl.work_dir = PathBuf::from(val);
        }
    }

    /// Generate a default configuration file.
    pub fn generate_default() -> String {
        let config = Config::default();
        toml::to_string_pretty(&config).unwrap_or_default()
    }

    /// Write configuration to a file.
    pub fn write_to_file(&self, path: &Path) -> Result<(), ConfigError> {
        let content = toml::to_string_pretty(self).map_err(|e| ConfigError::SerializeError {
            message: e.to_string(),
        })?;

        std::fs::write(path, content).map_err(|e| ConfigError::IoError {
            path: path.to_path_buf(),
            message: e.to_string(),
        })
    }
}

/// Configuration error.
#[derive(Debug)]
pub enum ConfigError {
    /// I/O error reading/writing config.
    IoError { path: PathBuf, message: String },
    /// Parse error in config file.
    ParseError { path: PathBuf, message: String },
    /// Serialization error.
    SerializeError { message: String },
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigError::IoError { path, message } => {
                write!(f, "I/O error for {}: {}", path.display(), message)
            }
            ConfigError::ParseError { path, message } => {
                write!(f, "Parse error in {}: {}", path.display(), message)
            }
            ConfigError::SerializeError { message } => {
                write!(f, "Serialization error: {}", message)
            }
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
        assert_eq!(config.compiler.source_format, "fixed");
        assert_eq!(config.compiler.optimization, 0);
        assert!(config.compiler.debug_info);
        assert_eq!(config.runtime.decimal_precision, 18);
        assert_eq!(config.dataset.default_lrecl, 80);
    }

    #[test]
    fn test_generate_default() {
        let toml = Config::generate_default();
        assert!(toml.contains("[compiler]"));
        assert!(toml.contains("[runtime]"));
        assert!(toml.contains("[dataset]"));
        assert!(toml.contains("[jcl]"));
    }

    #[test]
    fn test_parse_config() {
        let toml = r#"
[compiler]
source_format = "free"
optimization = 2
copybook_paths = ["./copy", "./includes"]

[runtime]
decimal_precision = 31

[dataset]
base_dir = "/data/datasets"
default_lrecl = 133
"#;
        let config: Config = toml::from_str(toml).unwrap();
        assert_eq!(config.compiler.source_format, "free");
        assert_eq!(config.compiler.optimization, 2);
        assert_eq!(config.compiler.copybook_paths.len(), 2);
        assert_eq!(config.runtime.decimal_precision, 31);
        assert_eq!(config.dataset.base_dir, PathBuf::from("/data/datasets"));
        assert_eq!(config.dataset.default_lrecl, 133);
    }

    #[test]
    fn test_merge_config() {
        let base = Config::default();
        let override_toml = r#"
[compiler]
optimization = 3
"#;
        let override_config: Config = toml::from_str(override_toml).unwrap();
        let merged = base.merge(override_config);
        assert_eq!(merged.compiler.optimization, 3);
        // Other values should remain default
        assert_eq!(merged.compiler.source_format, "fixed");
    }
}
