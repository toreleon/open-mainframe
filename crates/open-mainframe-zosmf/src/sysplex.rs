//! Sysplex manager for multi-system support.
//!
//! Manages multiple virtual z/OS system instances within a single OpenMainframe
//! server. Each system has its own identity (SYSNAME, SYSCLONE), and can have
//! its own or shared dataset directories and USS roots.

use serde::{Deserialize, Serialize};

use crate::config::ZosmfConfig;

/// A single z/OS system instance in the sysplex.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemInstance {
    /// System name (e.g., "SYS1").
    pub sysname: String,
    /// Two-character system clone (e.g., "S1").
    pub sysclone: String,
    /// Dataset directory for this system.
    pub dataset_dir: Option<String>,
    /// USS root directory for this system.
    pub uss_root: Option<String>,
    /// z/OS version/release (e.g., "V2R5").
    pub zos_vr: String,
    /// JES type (e.g., "JES2").
    pub jes_type: String,
    /// System status ("active" or "inactive").
    pub status: String,
}

impl SystemInstance {
    /// Create a new system instance with the given name.
    pub fn new(sysname: &str, sysclone: &str) -> Self {
        Self {
            sysname: sysname.to_string(),
            sysclone: sysclone.to_string(),
            dataset_dir: None,
            uss_root: None,
            zos_vr: "V2R5".to_string(),
            jes_type: "JES2".to_string(),
            status: "active".to_string(),
        }
    }
}

/// Sysplex configuration for the TOML config file.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SysplexConfig {
    /// Sysplex name (e.g., "TESTPLEX").
    #[serde(default = "default_sysplex_name")]
    pub name: String,
    /// System definitions.
    #[serde(default)]
    pub systems: Vec<SystemConfig>,
}

/// Per-system configuration entry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemConfig {
    /// System name.
    pub sysname: String,
    /// Two-character clone abbreviation.
    #[serde(default)]
    pub sysclone: Option<String>,
    /// Dataset directory override for this system.
    #[serde(default)]
    pub dataset_dir: Option<String>,
    /// USS root override for this system.
    #[serde(default)]
    pub uss_root: Option<String>,
    /// z/OS version (default: "V2R5").
    #[serde(default = "default_zos_vr")]
    pub zos_vr: String,
    /// JES type (default: "JES2").
    #[serde(default = "default_jes_type")]
    pub jes_type: String,
}

fn default_sysplex_name() -> String {
    "LOCAL".to_string()
}

fn default_zos_vr() -> String {
    "V2R5".to_string()
}

fn default_jes_type() -> String {
    "JES2".to_string()
}

/// Manages multiple z/OS system instances.
#[derive(Debug)]
pub struct SysplexManager {
    /// Sysplex name.
    pub name: String,
    /// Registered system instances.
    pub systems: Vec<SystemInstance>,
}

impl SysplexManager {
    /// Create a new sysplex manager from the server configuration.
    ///
    /// If the config has a `[sysplex]` section with systems, those are used.
    /// Otherwise, a default single-system sysplex is created.
    pub fn from_config(config: &ZosmfConfig) -> Self {
        if let Some(ref sysplex_cfg) = config.sysplex {
            let systems: Vec<SystemInstance> = sysplex_cfg
                .systems
                .iter()
                .map(|sc| {
                    let sysclone = sc
                        .sysclone
                        .clone()
                        .unwrap_or_else(|| {
                            // Default: first two chars of sysname
                            sc.sysname.chars().take(2).collect::<String>().to_uppercase()
                        });
                    SystemInstance {
                        sysname: sc.sysname.to_uppercase(),
                        sysclone,
                        dataset_dir: sc.dataset_dir.clone(),
                        uss_root: sc.uss_root.clone(),
                        zos_vr: sc.zos_vr.clone(),
                        jes_type: sc.jes_type.clone(),
                        status: "active".to_string(),
                    }
                })
                .collect();

            Self {
                name: sysplex_cfg.name.clone(),
                systems,
            }
        } else {
            // Default: single system
            Self {
                name: "LOCAL".to_string(),
                systems: vec![SystemInstance::new("SYS1", "S1")],
            }
        }
    }

    /// Get all systems.
    pub fn list_systems(&self) -> &[SystemInstance] {
        &self.systems
    }

    /// Find a system by name (case-insensitive).
    pub fn get_system(&self, sysname: &str) -> Option<&SystemInstance> {
        let upper = sysname.to_uppercase();
        self.systems.iter().find(|s| s.sysname == upper)
    }

    /// Add a new system to the sysplex.
    pub fn add_system(&mut self, system: SystemInstance) {
        self.systems.push(system);
    }

    /// Remove a system by name. Returns true if found and removed.
    pub fn remove_system(&mut self, sysname: &str) -> bool {
        let upper = sysname.to_uppercase();
        let len_before = self.systems.len();
        self.systems.retain(|s| s.sysname != upper);
        self.systems.len() < len_before
    }

    /// Get the number of systems.
    pub fn system_count(&self) -> usize {
        self.systems.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_sysplex() {
        let config = ZosmfConfig::default();
        let mgr = SysplexManager::from_config(&config);
        assert_eq!(mgr.name, "LOCAL");
        assert_eq!(mgr.system_count(), 1);
        assert_eq!(mgr.systems[0].sysname, "SYS1");
        assert_eq!(mgr.systems[0].sysclone, "S1");
    }

    #[test]
    fn test_multi_system_config() {
        let mut config = ZosmfConfig::default();
        config.sysplex = Some(SysplexConfig {
            name: "TESTPLEX".to_string(),
            systems: vec![
                SystemConfig {
                    sysname: "SYSA".to_string(),
                    sysclone: Some("SA".to_string()),
                    dataset_dir: Some("/data/sysa".to_string()),
                    uss_root: Some("/uss/sysa".to_string()),
                    zos_vr: "V2R5".to_string(),
                    jes_type: "JES2".to_string(),
                },
                SystemConfig {
                    sysname: "SYSB".to_string(),
                    sysclone: Some("SB".to_string()),
                    dataset_dir: None,
                    uss_root: None,
                    zos_vr: "V2R5".to_string(),
                    jes_type: "JES2".to_string(),
                },
            ],
        });

        let mgr = SysplexManager::from_config(&config);
        assert_eq!(mgr.name, "TESTPLEX");
        assert_eq!(mgr.system_count(), 2);
        assert_eq!(mgr.systems[0].sysname, "SYSA");
        assert_eq!(mgr.systems[1].sysname, "SYSB");
    }

    #[test]
    fn test_get_system_case_insensitive() {
        let config = ZosmfConfig::default();
        let mgr = SysplexManager::from_config(&config);
        assert!(mgr.get_system("SYS1").is_some());
        assert!(mgr.get_system("sys1").is_some());
        assert!(mgr.get_system("UNKNOWN").is_none());
    }

    #[test]
    fn test_add_remove_system() {
        let config = ZosmfConfig::default();
        let mut mgr = SysplexManager::from_config(&config);
        assert_eq!(mgr.system_count(), 1);

        mgr.add_system(SystemInstance::new("SYS2", "S2"));
        assert_eq!(mgr.system_count(), 2);

        assert!(mgr.remove_system("SYS2"));
        assert_eq!(mgr.system_count(), 1);

        assert!(!mgr.remove_system("NONEXISTENT"));
    }
}
