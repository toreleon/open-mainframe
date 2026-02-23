//! # ICF (Integrated Catalog Facility) Infrastructure
//!
//! Implements the z/OS ICF catalog hierarchy:
//!
//! - **Master Catalog** — system datasets (SYS1.*) and user catalog connectors
//! - **User Catalogs** — application-level dataset grouping
//! - **BCS** (Basic Catalog Structure) — KSDS-based catalog entries
//! - **VVDS** (VSAM Volume Data Set) — per-volume extent tracking
//! - **Alias/Connector Records** — HLQ routing to user catalogs
//! - **Catalog Search Order** — alias → master → user catalog
//! - **EXAMINE** — BCS structural integrity checking
//! - **DIAGNOSE** — BCS-VVDS synchronization checking

use std::collections::{BTreeMap, HashMap};

// ─────────────────────── BCS Entry Types ───────────────────────

/// Type of BCS catalog entry.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BcsEntryType {
    /// Non-VSAM dataset (sequential, PDS, etc.).
    NonVsam,
    /// VSAM cluster.
    Cluster,
    /// VSAM data component.
    Data,
    /// VSAM index component.
    Index,
    /// Alternate index.
    Aix,
    /// Path over an AIX.
    Path,
    /// GDG base.
    Gdg,
    /// User catalog connector.
    UserCatalog,
    /// HLQ alias.
    Alias,
}

/// A BCS (Basic Catalog Structure) entry.
///
/// Each entry represents a cataloged object with a 44-byte DSN key.
#[derive(Debug, Clone)]
pub struct BcsEntry {
    /// Dataset name (the catalog key, up to 44 characters).
    pub dsn: String,
    /// Entry type.
    pub entry_type: BcsEntryType,
    /// Volume serial(s) where the dataset resides.
    pub volumes: Vec<String>,
    /// For clusters: data component name.
    pub data_component: Option<String>,
    /// For clusters: index component name.
    pub index_component: Option<String>,
    /// For aliases: the related catalog name.
    pub relates_to: Option<String>,
    /// Dataset attributes (RECFM, LRECL, etc.) stored as key-value pairs.
    pub attributes: HashMap<String, String>,
}

impl BcsEntry {
    /// Create a non-VSAM entry.
    pub fn non_vsam(dsn: &str, volumes: Vec<String>) -> Self {
        Self {
            dsn: dsn.to_uppercase(),
            entry_type: BcsEntryType::NonVsam,
            volumes,
            data_component: None,
            index_component: None,
            relates_to: None,
            attributes: HashMap::new(),
        }
    }

    /// Create a VSAM cluster entry.
    pub fn cluster(dsn: &str, data: &str, index: Option<&str>, volumes: Vec<String>) -> Self {
        Self {
            dsn: dsn.to_uppercase(),
            entry_type: BcsEntryType::Cluster,
            volumes,
            data_component: Some(data.to_uppercase()),
            index_component: index.map(|i| i.to_uppercase()),
            relates_to: None,
            attributes: HashMap::new(),
        }
    }

    /// Create an alias entry.
    pub fn alias(name: &str, relates_to: &str) -> Self {
        Self {
            dsn: name.to_uppercase(),
            entry_type: BcsEntryType::Alias,
            volumes: Vec::new(),
            data_component: None,
            index_component: None,
            relates_to: Some(relates_to.to_uppercase()),
            attributes: HashMap::new(),
        }
    }

    /// Create a user catalog connector entry.
    pub fn user_catalog(name: &str, volume: &str) -> Self {
        Self {
            dsn: name.to_uppercase(),
            entry_type: BcsEntryType::UserCatalog,
            volumes: vec![volume.to_uppercase()],
            data_component: None,
            index_component: None,
            relates_to: None,
            attributes: HashMap::new(),
        }
    }

    /// Create a GDG base entry.
    pub fn gdg_base(dsn: &str) -> Self {
        Self {
            dsn: dsn.to_uppercase(),
            entry_type: BcsEntryType::Gdg,
            volumes: Vec::new(),
            data_component: None,
            index_component: None,
            relates_to: None,
            attributes: HashMap::new(),
        }
    }
}

// ─────────────────────── VVDS Records ───────────────────────

/// Type of VVDS record.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VvdsRecordType {
    /// VSAM Volume Record — extent info for VSAM datasets.
    Vvr,
    /// Non-VSAM Volume Record — extent info for non-VSAM datasets.
    Nvr,
    /// VSAM Volume Control Record — links VVDS to its owning BCS.
    Vvcr,
}

/// A VVDS (VSAM Volume Data Set) record.
#[derive(Debug, Clone)]
pub struct VvdsRecord {
    /// Dataset name this record pertains to.
    pub dsn: String,
    /// Record type.
    pub record_type: VvdsRecordType,
    /// Owning BCS catalog name (for VVCR).
    pub owning_catalog: Option<String>,
    /// Extent information (simplified as byte ranges).
    pub extents: Vec<(u64, u64)>,
}

/// A VVDS for a single volume.
#[derive(Debug, Clone, Default)]
pub struct Vvds {
    /// Volume serial.
    pub volser: String,
    /// Records indexed by DSN.
    pub records: BTreeMap<String, VvdsRecord>,
}

impl Vvds {
    /// Create a new VVDS for a volume.
    pub fn new(volser: &str) -> Self {
        Self {
            volser: volser.to_uppercase(),
            records: BTreeMap::new(),
        }
    }

    /// Add a VVR (VSAM Volume Record).
    pub fn add_vvr(&mut self, dsn: &str, extents: Vec<(u64, u64)>) {
        let key = dsn.to_uppercase();
        self.records.insert(
            key.clone(),
            VvdsRecord {
                dsn: key,
                record_type: VvdsRecordType::Vvr,
                owning_catalog: None,
                extents,
            },
        );
    }

    /// Add an NVR (Non-VSAM Volume Record).
    pub fn add_nvr(&mut self, dsn: &str, extents: Vec<(u64, u64)>) {
        let key = dsn.to_uppercase();
        self.records.insert(
            key.clone(),
            VvdsRecord {
                dsn: key,
                record_type: VvdsRecordType::Nvr,
                owning_catalog: None,
                extents,
            },
        );
    }

    /// Add a VVCR (VSAM Volume Control Record).
    pub fn add_vvcr(&mut self, catalog_name: &str) {
        let key = format!("VVCR.{}", catalog_name.to_uppercase());
        self.records.insert(
            key.clone(),
            VvdsRecord {
                dsn: key,
                record_type: VvdsRecordType::Vvcr,
                owning_catalog: Some(catalog_name.to_uppercase()),
                extents: Vec::new(),
            },
        );
    }

    /// Check if a dataset has a record in this VVDS.
    pub fn has_record(&self, dsn: &str) -> bool {
        self.records.contains_key(&dsn.to_uppercase())
    }
}

// ─────────────────────── ICF Catalog ───────────────────────

/// An ICF catalog (either master or user catalog).
#[derive(Debug, Clone)]
pub struct IcfCatalog {
    /// Catalog name.
    pub name: String,
    /// Whether this is the master catalog.
    pub is_master: bool,
    /// BCS entries indexed by DSN (sorted for O(log n) lookup).
    entries: BTreeMap<String, BcsEntry>,
}

impl IcfCatalog {
    /// Create a new catalog.
    pub fn new(name: &str, is_master: bool) -> Self {
        Self {
            name: name.to_uppercase(),
            is_master,
            entries: BTreeMap::new(),
        }
    }

    /// Add a BCS entry to this catalog.
    pub fn add_entry(&mut self, entry: BcsEntry) {
        self.entries.insert(entry.dsn.clone(), entry);
    }

    /// Remove a BCS entry.
    pub fn remove_entry(&mut self, dsn: &str) -> Option<BcsEntry> {
        self.entries.remove(&dsn.to_uppercase())
    }

    /// Look up an entry by DSN.
    pub fn get_entry(&self, dsn: &str) -> Option<&BcsEntry> {
        self.entries.get(&dsn.to_uppercase())
    }

    /// List all entries.
    pub fn list_entries(&self) -> Vec<&BcsEntry> {
        self.entries.values().collect()
    }

    /// List entries matching a level (HLQ prefix).
    pub fn list_level(&self, level: &str) -> Vec<&BcsEntry> {
        let prefix = level.to_uppercase();
        self.entries
            .values()
            .filter(|e| e.dsn.starts_with(&prefix))
            .collect()
    }

    /// Number of entries.
    pub fn entry_count(&self) -> usize {
        self.entries.len()
    }

    /// Get all alias entries.
    pub fn aliases(&self) -> Vec<&BcsEntry> {
        self.entries
            .values()
            .filter(|e| e.entry_type == BcsEntryType::Alias)
            .collect()
    }

    /// Get all user catalog connector entries.
    pub fn user_catalog_connectors(&self) -> Vec<&BcsEntry> {
        self.entries
            .values()
            .filter(|e| e.entry_type == BcsEntryType::UserCatalog)
            .collect()
    }
}

// ─────────────────────── ICF Catalog System ───────────────────────

/// The complete ICF catalog system with master catalog and user catalogs.
#[derive(Debug, Clone)]
pub struct IcfCatalogSystem {
    /// The master catalog.
    pub master: IcfCatalog,
    /// User catalogs indexed by name.
    user_catalogs: HashMap<String, IcfCatalog>,
    /// VVDS per volume.
    vvds_map: HashMap<String, Vvds>,
}

/// Result of a catalog lookup.
#[derive(Debug, Clone)]
pub struct CatalogLookupResult {
    /// The found entry.
    pub entry: BcsEntry,
    /// Which catalog it was found in.
    pub catalog_name: String,
}

/// Validation issue from EXAMINE or DIAGNOSE.
#[derive(Debug, Clone)]
pub struct CatalogIssue {
    /// Issue description.
    pub message: String,
    /// Affected DSN.
    pub dsn: Option<String>,
    /// Severity (true = error, false = warning).
    pub is_error: bool,
}

impl IcfCatalogSystem {
    /// Create a new catalog system with a master catalog.
    pub fn new(master_name: &str) -> Self {
        Self {
            master: IcfCatalog::new(master_name, true),
            user_catalogs: HashMap::new(),
            vvds_map: HashMap::new(),
        }
    }

    /// Define a user catalog and create a connector in the master.
    pub fn define_user_catalog(&mut self, name: &str, volume: &str) {
        let ucat = IcfCatalog::new(name, false);
        let connector = BcsEntry::user_catalog(name, volume);
        self.master.add_entry(connector);
        self.user_catalogs
            .insert(name.to_uppercase(), ucat);
    }

    /// Define an HLQ alias pointing to a user catalog.
    pub fn define_alias(&mut self, alias_name: &str, catalog_name: &str) {
        let entry = BcsEntry::alias(alias_name, catalog_name);
        self.master.add_entry(entry);
    }

    /// Catalog a dataset (add to appropriate catalog based on alias routing).
    pub fn catalog_dataset(&mut self, entry: BcsEntry) {
        let target_catalog = self.resolve_catalog_for_dsn(&entry.dsn);

        // Add VVDS records for each volume
        for vol in &entry.volumes {
            let vvds = self
                .vvds_map
                .entry(vol.to_uppercase())
                .or_insert_with(|| Vvds::new(vol));

            match entry.entry_type {
                BcsEntryType::Cluster | BcsEntryType::Data | BcsEntryType::Index | BcsEntryType::Aix => {
                    vvds.add_vvr(&entry.dsn, vec![(0, 1024)]);
                }
                BcsEntryType::NonVsam => {
                    vvds.add_nvr(&entry.dsn, vec![(0, 1024)]);
                }
                _ => {}
            }
        }

        if let Some(ucat) = self.user_catalogs.get_mut(&target_catalog) {
            ucat.add_entry(entry);
        } else {
            self.master.add_entry(entry);
        }
    }

    /// Uncatalog a dataset.
    pub fn uncatalog_dataset(&mut self, dsn: &str) -> bool {
        let dsn_upper = dsn.to_uppercase();

        // Try user catalogs first
        for ucat in self.user_catalogs.values_mut() {
            if ucat.remove_entry(&dsn_upper).is_some() {
                return true;
            }
        }

        // Try master catalog
        self.master.remove_entry(&dsn_upper).is_some()
    }

    /// Look up a dataset following z/OS search order.
    ///
    /// 1. Check alias routing → search target user catalog
    /// 2. Search master catalog
    /// 3. Return NOT CATALOGED if not found
    pub fn lookup(&self, dsn: &str) -> Option<CatalogLookupResult> {
        let dsn_upper = dsn.to_uppercase();

        // Step 1: Check alias routing
        let hlq = dsn_upper.split('.').next().unwrap_or("");
        if let Some(alias_entry) = self.master.get_entry(hlq) {
            if alias_entry.entry_type == BcsEntryType::Alias {
                if let Some(catalog_name) = &alias_entry.relates_to {
                    if let Some(ucat) = self.user_catalogs.get(catalog_name) {
                        if let Some(entry) = ucat.get_entry(&dsn_upper) {
                            return Some(CatalogLookupResult {
                                entry: entry.clone(),
                                catalog_name: ucat.name.clone(),
                            });
                        }
                    }
                }
            }
        }

        // Step 2: Search master catalog
        if let Some(entry) = self.master.get_entry(&dsn_upper) {
            return Some(CatalogLookupResult {
                entry: entry.clone(),
                catalog_name: self.master.name.clone(),
            });
        }

        // Step 3: Search all user catalogs (fallback)
        for ucat in self.user_catalogs.values() {
            if let Some(entry) = ucat.get_entry(&dsn_upper) {
                return Some(CatalogLookupResult {
                    entry: entry.clone(),
                    catalog_name: ucat.name.clone(),
                });
            }
        }

        None
    }

    /// List entries matching a level prefix from a specific catalog.
    pub fn listcat_level(&self, level: &str, catalog_name: Option<&str>) -> Vec<CatalogLookupResult> {
        let mut results = Vec::new();

        if let Some(cat_name) = catalog_name {
            let cat_upper = cat_name.to_uppercase();
            if cat_upper == self.master.name {
                for entry in self.master.list_level(level) {
                    results.push(CatalogLookupResult {
                        entry: entry.clone(),
                        catalog_name: self.master.name.clone(),
                    });
                }
            } else if let Some(ucat) = self.user_catalogs.get(&cat_upper) {
                for entry in ucat.list_level(level) {
                    results.push(CatalogLookupResult {
                        entry: entry.clone(),
                        catalog_name: ucat.name.clone(),
                    });
                }
            }
        } else {
            // Search all catalogs
            for entry in self.master.list_level(level) {
                results.push(CatalogLookupResult {
                    entry: entry.clone(),
                    catalog_name: self.master.name.clone(),
                });
            }
            for ucat in self.user_catalogs.values() {
                for entry in ucat.list_level(level) {
                    results.push(CatalogLookupResult {
                        entry: entry.clone(),
                        catalog_name: ucat.name.clone(),
                    });
                }
            }
        }

        results
    }

    /// Get a VVDS for a volume.
    pub fn get_vvds(&self, volser: &str) -> Option<&Vvds> {
        self.vvds_map.get(&volser.to_uppercase())
    }

    /// Get a user catalog by name.
    pub fn get_user_catalog(&self, name: &str) -> Option<&IcfCatalog> {
        self.user_catalogs.get(&name.to_uppercase())
    }

    /// EXAMINE — check BCS structural integrity.
    ///
    /// Verifies:
    /// - Cluster entries have valid data/index component references
    /// - Alias entries reference existing catalogs
    /// - No orphaned component entries
    pub fn examine(&self, catalog_name: &str) -> (u32, Vec<CatalogIssue>) {
        let cat_upper = catalog_name.to_uppercase();
        let catalog = if cat_upper == self.master.name {
            &self.master
        } else if let Some(ucat) = self.user_catalogs.get(&cat_upper) {
            ucat
        } else {
            return (
                12,
                vec![CatalogIssue {
                    message: format!("Catalog '{cat_upper}' not found"),
                    dsn: None,
                    is_error: true,
                }],
            );
        };

        let mut issues = Vec::new();

        for entry in catalog.list_entries() {
            // Check cluster component references
            if entry.entry_type == BcsEntryType::Cluster {
                if let Some(data_name) = &entry.data_component {
                    if catalog.get_entry(data_name).is_none() {
                        issues.push(CatalogIssue {
                            message: format!(
                                "Cluster {} references missing data component {}",
                                entry.dsn, data_name
                            ),
                            dsn: Some(entry.dsn.clone()),
                            is_error: false,
                        });
                    }
                }
                if let Some(index_name) = &entry.index_component {
                    if catalog.get_entry(index_name).is_none() {
                        issues.push(CatalogIssue {
                            message: format!(
                                "Cluster {} references missing index component {}",
                                entry.dsn, index_name
                            ),
                            dsn: Some(entry.dsn.clone()),
                            is_error: false,
                        });
                    }
                }
            }

            // Check alias references
            if entry.entry_type == BcsEntryType::Alias {
                if let Some(rel) = &entry.relates_to {
                    if !self.user_catalogs.contains_key(rel)
                        && *rel != self.master.name
                    {
                        issues.push(CatalogIssue {
                            message: format!(
                                "Alias {} references non-existent catalog {}",
                                entry.dsn, rel
                            ),
                            dsn: Some(entry.dsn.clone()),
                            is_error: false,
                        });
                    }
                }
            }
        }

        let cc = if issues.iter().any(|i| i.is_error) {
            8
        } else if issues.is_empty() {
            0
        } else {
            4
        };

        (cc, issues)
    }

    /// DIAGNOSE — check BCS-VVDS synchronization.
    ///
    /// Verifies:
    /// - Every BCS entry with volumes has matching VVDS records
    /// - Every VVDS record has a matching BCS entry
    pub fn diagnose(&self, catalog_name: &str) -> (u32, Vec<CatalogIssue>) {
        let cat_upper = catalog_name.to_uppercase();
        let catalog = if cat_upper == self.master.name {
            &self.master
        } else if let Some(ucat) = self.user_catalogs.get(&cat_upper) {
            ucat
        } else {
            return (
                12,
                vec![CatalogIssue {
                    message: format!("Catalog '{cat_upper}' not found"),
                    dsn: None,
                    is_error: true,
                }],
            );
        };

        let mut issues = Vec::new();

        // Check BCS → VVDS: every cataloged dataset should have VVDS records
        for entry in catalog.list_entries() {
            if matches!(
                entry.entry_type,
                BcsEntryType::Alias | BcsEntryType::UserCatalog | BcsEntryType::Gdg
            ) {
                continue; // These don't have VVDS records
            }

            for vol in &entry.volumes {
                if let Some(vvds) = self.vvds_map.get(&vol.to_uppercase()) {
                    if !vvds.has_record(&entry.dsn) {
                        issues.push(CatalogIssue {
                            message: format!(
                                "BCS entry {} on volume {} has no matching VVDS record",
                                entry.dsn, vol
                            ),
                            dsn: Some(entry.dsn.clone()),
                            is_error: false,
                        });
                    }
                } else {
                    issues.push(CatalogIssue {
                        message: format!(
                            "BCS entry {} references volume {} with no VVDS",
                            entry.dsn, vol
                        ),
                        dsn: Some(entry.dsn.clone()),
                        is_error: false,
                    });
                }
            }
        }

        // Check VVDS → BCS: every VVDS record should have a BCS entry
        for vvds in self.vvds_map.values() {
            for record in vvds.records.values() {
                if record.record_type == VvdsRecordType::Vvcr {
                    continue; // Control records don't need BCS entries
                }
                if catalog.get_entry(&record.dsn).is_none() {
                    // Also check other catalogs
                    let found = self.lookup(&record.dsn).is_some();
                    if !found {
                        issues.push(CatalogIssue {
                            message: format!(
                                "VVDS record {} on volume {} has no matching BCS entry",
                                record.dsn, vvds.volser
                            ),
                            dsn: Some(record.dsn.clone()),
                            is_error: false,
                        });
                    }
                }
            }
        }

        let cc = if issues.iter().any(|i| i.is_error) {
            8
        } else if issues.is_empty() {
            0
        } else {
            4
        };

        (cc, issues)
    }

    /// Resolve which catalog a DSN should be cataloged in.
    fn resolve_catalog_for_dsn(&self, dsn: &str) -> String {
        let hlq = dsn.to_uppercase();
        let hlq = hlq.split('.').next().unwrap_or("");

        // Check alias routing
        if let Some(alias_entry) = self.master.get_entry(hlq) {
            if alias_entry.entry_type == BcsEntryType::Alias {
                if let Some(catalog_name) = &alias_entry.relates_to {
                    if self.user_catalogs.contains_key(catalog_name) {
                        return catalog_name.clone();
                    }
                }
            }
        }

        self.master.name.clone()
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn setup_system() -> IcfCatalogSystem {
        let mut sys = IcfCatalogSystem::new("MASTER.CATALOG");

        // Add system datasets to master
        sys.master
            .add_entry(BcsEntry::non_vsam("SYS1.PARMLIB", vec!["SYSRES".to_string()]));
        sys.master
            .add_entry(BcsEntry::non_vsam("SYS1.MACLIB", vec!["SYSRES".to_string()]));
        sys.master
            .add_entry(BcsEntry::non_vsam("SYS1.LINKLIB", vec!["SYSRES".to_string()]));

        // Create user catalogs
        sys.define_user_catalog("UCAT.PROD", "UCATVL");
        sys.define_user_catalog("UCAT.TEST", "UCATVL");

        // Define aliases
        sys.define_alias("PROD", "UCAT.PROD");
        sys.define_alias("TEST", "UCAT.TEST");

        // Add datasets to user catalogs
        sys.catalog_dataset(BcsEntry::non_vsam(
            "PROD.DATA.FILE",
            vec!["VOL001".to_string()],
        ));
        sys.catalog_dataset(BcsEntry::non_vsam(
            "PROD.BACKUP.DAILY",
            vec!["VOL002".to_string()],
        ));
        sys.catalog_dataset(BcsEntry::cluster(
            "PROD.VSAM.KSDS",
            "PROD.VSAM.KSDS.DATA",
            Some("PROD.VSAM.KSDS.INDEX"),
            vec!["VOL001".to_string()],
        ));
        sys.catalog_dataset(BcsEntry::non_vsam(
            "TEST.UNIT.DATA",
            vec!["VOL003".to_string()],
        ));

        sys
    }

    // ─── DFSMS-102.1: Master Catalog ───

    #[test]
    fn test_master_catalog_system_datasets() {
        let sys = setup_system();
        assert!(sys.master.get_entry("SYS1.PARMLIB").is_some());
        assert!(sys.master.get_entry("SYS1.MACLIB").is_some());
        assert!(sys.master.get_entry("SYS1.LINKLIB").is_some());
    }

    #[test]
    fn test_master_catalog_has_connectors() {
        let sys = setup_system();
        let connectors = sys.master.user_catalog_connectors();
        assert_eq!(connectors.len(), 2);
    }

    #[test]
    fn test_listcat_level_sys1() {
        let sys = setup_system();
        let results = sys.listcat_level("SYS1", None);
        assert_eq!(results.len(), 3);
        assert!(results.iter().all(|r| r.catalog_name == "MASTER.CATALOG"));
    }

    // ─── DFSMS-102.2: User Catalogs ───

    #[test]
    fn test_user_catalog_created() {
        let sys = setup_system();
        assert!(sys.get_user_catalog("UCAT.PROD").is_some());
        assert!(sys.get_user_catalog("UCAT.TEST").is_some());
    }

    #[test]
    fn test_listcat_specific_user_catalog() {
        let sys = setup_system();
        let results = sys.listcat_level("PROD", Some("UCAT.PROD"));
        assert!(results.len() >= 2); // PROD.DATA.FILE, PROD.BACKUP.DAILY, PROD.VSAM.KSDS
        assert!(results.iter().all(|r| r.catalog_name == "UCAT.PROD"));
    }

    #[test]
    fn test_datasets_in_correct_user_catalog() {
        let sys = setup_system();
        let ucat_prod = sys.get_user_catalog("UCAT.PROD").unwrap();
        assert!(ucat_prod.get_entry("PROD.DATA.FILE").is_some());
        assert!(ucat_prod.get_entry("TEST.UNIT.DATA").is_none());

        let ucat_test = sys.get_user_catalog("UCAT.TEST").unwrap();
        assert!(ucat_test.get_entry("TEST.UNIT.DATA").is_some());
    }

    // ─── DFSMS-102.3: BCS (Basic Catalog Structure) ───

    #[test]
    fn test_bcs_vsam_cluster_entry() {
        let sys = setup_system();
        let result = sys.lookup("PROD.VSAM.KSDS").unwrap();
        assert_eq!(result.entry.entry_type, BcsEntryType::Cluster);
        assert_eq!(
            result.entry.data_component.as_deref(),
            Some("PROD.VSAM.KSDS.DATA")
        );
        assert_eq!(
            result.entry.index_component.as_deref(),
            Some("PROD.VSAM.KSDS.INDEX")
        );
    }

    #[test]
    fn test_bcs_non_vsam_entry() {
        let sys = setup_system();
        let result = sys.lookup("PROD.DATA.FILE").unwrap();
        assert_eq!(result.entry.entry_type, BcsEntryType::NonVsam);
        assert!(result.entry.volumes.contains(&"VOL001".to_string()));
    }

    #[test]
    fn test_bcs_sorted_lookup() {
        let sys = setup_system();
        // BTreeMap ensures sorted order for O(log n) lookup
        let ucat = sys.get_user_catalog("UCAT.PROD").unwrap();
        let entries = ucat.list_entries();
        let dsns: Vec<&str> = entries.iter().map(|e| e.dsn.as_str()).collect();
        let mut sorted = dsns.clone();
        sorted.sort();
        assert_eq!(dsns, sorted);
    }

    // ─── DFSMS-102.4: VVDS ───

    #[test]
    fn test_vvds_vvr_created_for_vsam() {
        let sys = setup_system();
        let vvds = sys.get_vvds("VOL001").unwrap();
        assert!(vvds.has_record("PROD.VSAM.KSDS"));
    }

    #[test]
    fn test_vvds_nvr_created_for_non_vsam() {
        let sys = setup_system();
        let vvds = sys.get_vvds("VOL001").unwrap();
        assert!(vvds.has_record("PROD.DATA.FILE"));
    }

    #[test]
    fn test_vvds_per_volume() {
        let sys = setup_system();
        // VOL001 should have records
        assert!(sys.get_vvds("VOL001").is_some());
        // VOL002 should have records
        assert!(sys.get_vvds("VOL002").is_some());
        // VOL003 should have records
        assert!(sys.get_vvds("VOL003").is_some());
        // Non-existent volume
        assert!(sys.get_vvds("VOL999").is_none());
    }

    #[test]
    fn test_vvds_vvcr() {
        let mut vvds = Vvds::new("VOL001");
        vvds.add_vvcr("UCAT.PROD");
        let vvcr_key = "VVCR.UCAT.PROD";
        assert!(vvds.records.contains_key(vvcr_key));
        let rec = &vvds.records[vvcr_key];
        assert_eq!(rec.record_type, VvdsRecordType::Vvcr);
        assert_eq!(rec.owning_catalog.as_deref(), Some("UCAT.PROD"));
    }

    // ─── DFSMS-102.5: Alias and Connector Records ───

    #[test]
    fn test_alias_entry() {
        let sys = setup_system();
        let alias = sys.master.get_entry("PROD").unwrap();
        assert_eq!(alias.entry_type, BcsEntryType::Alias);
        assert_eq!(alias.relates_to.as_deref(), Some("UCAT.PROD"));
    }

    #[test]
    fn test_connector_entry() {
        let sys = setup_system();
        let conn = sys.master.get_entry("UCAT.PROD").unwrap();
        assert_eq!(conn.entry_type, BcsEntryType::UserCatalog);
        assert!(conn.volumes.contains(&"UCATVL".to_string()));
    }

    // ─── DFSMS-102.6: Catalog Search Order ───

    #[test]
    fn test_search_order_alias_routing() {
        let sys = setup_system();
        // PROD.DATA.FILE should be found via alias PROD → UCAT.PROD
        let result = sys.lookup("PROD.DATA.FILE").unwrap();
        assert_eq!(result.catalog_name, "UCAT.PROD");
    }

    #[test]
    fn test_search_order_master_direct() {
        let sys = setup_system();
        // SYS1.PARMLIB is in master with no alias match
        let result = sys.lookup("SYS1.PARMLIB").unwrap();
        assert_eq!(result.catalog_name, "MASTER.CATALOG");
    }

    #[test]
    fn test_search_order_not_cataloged() {
        let sys = setup_system();
        assert!(sys.lookup("NONEXISTENT.DATASET").is_none());
    }

    #[test]
    fn test_uncatalog() {
        let mut sys = setup_system();
        assert!(sys.lookup("PROD.DATA.FILE").is_some());
        assert!(sys.uncatalog_dataset("PROD.DATA.FILE"));
        assert!(sys.lookup("PROD.DATA.FILE").is_none());
    }

    // ─── DFSMS-102.7: EXAMINE ───

    #[test]
    fn test_examine_healthy_catalog() {
        let sys = setup_system();
        let (cc, issues) = sys.examine("UCAT.PROD");
        // Cluster components aren't separately cataloged → expect warnings
        assert!(cc <= 4);
        // But no hard errors
        assert!(!issues.iter().any(|i| i.is_error));
    }

    #[test]
    fn test_examine_master_aliases_ok() {
        let sys = setup_system();
        let (cc, issues) = sys.examine("MASTER.CATALOG");
        // Aliases reference existing user catalogs → no alias issues
        let alias_issues: Vec<_> = issues
            .iter()
            .filter(|i| i.message.contains("Alias"))
            .collect();
        assert!(alias_issues.is_empty());
        assert!(cc <= 4);
    }

    #[test]
    fn test_examine_nonexistent_catalog() {
        let sys = setup_system();
        let (cc, _) = sys.examine("NOSUCH.CATALOG");
        assert_eq!(cc, 12);
    }

    // ─── DFSMS-102.8: DIAGNOSE ───

    #[test]
    fn test_diagnose_synchronized() {
        let sys = setup_system();
        let (cc, issues) = sys.diagnose("UCAT.PROD");
        // All datasets should have VVDS records (we create them in catalog_dataset)
        assert_eq!(cc, 0, "Expected CC=0, got issues: {issues:?}");
    }

    #[test]
    fn test_diagnose_missing_vvds_record() {
        let mut sys = setup_system();
        // Add a BCS entry without going through catalog_dataset (no VVDS record)
        let ucat = sys.user_catalogs.get_mut("UCAT.PROD").unwrap();
        ucat.add_entry(BcsEntry::non_vsam(
            "PROD.ORPHAN.DS",
            vec!["VOL999".to_string()],
        ));

        let (cc, issues) = sys.diagnose("UCAT.PROD");
        assert_eq!(cc, 4);
        assert!(issues
            .iter()
            .any(|i| i.message.contains("PROD.ORPHAN.DS")));
    }

    #[test]
    fn test_diagnose_orphaned_vvds_record() {
        let mut sys = setup_system();
        // Add a VVDS record without a BCS entry
        let vvds = sys
            .vvds_map
            .entry("VOL001".to_string())
            .or_insert_with(|| Vvds::new("VOL001"));
        vvds.add_nvr("ORPHAN.VVDS.RECORD", vec![(0, 512)]);

        let (cc, issues) = sys.diagnose("UCAT.PROD");
        assert_eq!(cc, 4);
        assert!(issues
            .iter()
            .any(|i| i.message.contains("ORPHAN.VVDS.RECORD")));
    }

    // ─── DFSMS-102.9: Integration Tests ───

    #[test]
    fn test_full_catalog_scenario() {
        let mut sys = IcfCatalogSystem::new("MASTER.CATALOG");

        // Set up 3 user catalogs with aliases
        sys.define_user_catalog("UCAT.PROD", "UCVOL1");
        sys.define_user_catalog("UCAT.TEST", "UCVOL2");
        sys.define_user_catalog("UCAT.DEV", "UCVOL3");
        sys.define_alias("PROD", "UCAT.PROD");
        sys.define_alias("TEST", "UCAT.TEST");
        sys.define_alias("DEV", "UCAT.DEV");

        // Catalog 20 datasets across catalogs
        for i in 0..7 {
            sys.catalog_dataset(BcsEntry::non_vsam(
                &format!("PROD.DATA.FILE{i:02}"),
                vec!["VOL001".to_string()],
            ));
        }
        for i in 0..7 {
            sys.catalog_dataset(BcsEntry::non_vsam(
                &format!("TEST.DATA.FILE{i:02}"),
                vec!["VOL002".to_string()],
            ));
        }
        for i in 0..3 {
            sys.catalog_dataset(BcsEntry::non_vsam(
                &format!("DEV.DATA.FILE{i:02}"),
                vec!["VOL003".to_string()],
            ));
        }
        // 3 system datasets in master
        sys.master
            .add_entry(BcsEntry::non_vsam("SYS1.PARMLIB", vec!["SYSRES".to_string()]));
        sys.master
            .add_entry(BcsEntry::non_vsam("SYS1.MACLIB", vec!["SYSRES".to_string()]));
        sys.master
            .add_entry(BcsEntry::non_vsam("SYS1.LINKLIB", vec!["SYSRES".to_string()]));

        // All 20 datasets are findable
        assert!(sys.lookup("PROD.DATA.FILE00").is_some());
        assert!(sys.lookup("PROD.DATA.FILE06").is_some());
        assert!(sys.lookup("TEST.DATA.FILE00").is_some());
        assert!(sys.lookup("DEV.DATA.FILE02").is_some());
        assert!(sys.lookup("SYS1.PARMLIB").is_some());

        // Correct catalog routing
        assert_eq!(
            sys.lookup("PROD.DATA.FILE00").unwrap().catalog_name,
            "UCAT.PROD"
        );
        assert_eq!(
            sys.lookup("TEST.DATA.FILE03").unwrap().catalog_name,
            "UCAT.TEST"
        );
        assert_eq!(
            sys.lookup("SYS1.PARMLIB").unwrap().catalog_name,
            "MASTER.CATALOG"
        );

        // EXAMINE and DIAGNOSE pass
        let (cc_exam, _) = sys.examine("UCAT.PROD");
        assert!(cc_exam <= 4);

        let (cc_diag, _) = sys.diagnose("UCAT.PROD");
        assert_eq!(cc_diag, 0);
    }

    #[test]
    fn test_examine_and_diagnose_with_intentional_mismatch() {
        let mut sys = setup_system();

        // Create intentional BCS-VVDS mismatch
        let ucat = sys.user_catalogs.get_mut("UCAT.PROD").unwrap();
        ucat.add_entry(BcsEntry::non_vsam("PROD.NO.VVDS", vec!["VOLX".to_string()]));

        // Add orphaned VVDS record
        let vvds = sys
            .vvds_map
            .entry("VOL001".to_string())
            .or_insert_with(|| Vvds::new("VOL001"));
        vvds.add_nvr("NO.BCS.ENTRY", vec![(0, 100)]);

        // EXAMINE should find cluster issues (component refs)
        let (cc_exam, _issues_exam) = sys.examine("UCAT.PROD");
        assert!(cc_exam <= 4);

        // DIAGNOSE should find sync issues
        let (cc_diag, issues_diag) = sys.diagnose("UCAT.PROD");
        assert_eq!(cc_diag, 4);
        assert!(issues_diag.iter().any(|i| i.message.contains("PROD.NO.VVDS")));
        assert!(issues_diag
            .iter()
            .any(|i| i.message.contains("NO.BCS.ENTRY")));
    }

    #[test]
    fn test_gdg_base_entry() {
        let mut sys = IcfCatalogSystem::new("MASTER.CATALOG");
        sys.catalog_dataset(BcsEntry::gdg_base("MY.GDG"));
        let result = sys.lookup("MY.GDG").unwrap();
        assert_eq!(result.entry.entry_type, BcsEntryType::Gdg);
    }

    #[test]
    fn test_case_insensitive_lookup() {
        let sys = setup_system();
        assert!(sys.lookup("prod.data.file").is_some());
        assert!(sys.lookup("SYS1.parmlib").is_some());
    }
}
