//! SYS-114: DB2 BIND & Package Management.
//!
//! Provides a full BIND/REBIND/FREE lifecycle with a simulated DB2 catalog.
//!
//! # Overview
//!
//! - [`BindPackage`] models the BIND PACKAGE command.
//! - [`BindPlan`] models the BIND PLAN command.
//! - [`RebindCommand`] models REBIND PACKAGE / REBIND PLAN.
//! - [`FreeCommand`] models FREE PACKAGE / FREE PLAN.
//! - [`PackageBindOptions`] captures common bind-time options.
//! - [`Db2Catalog`] is an in-memory HashMap-based catalog that tracks
//!   packages, plans, and package-lists.
//! - [`Dbrm`] represents a Database Request Module input to BIND.

use std::collections::HashMap;
use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

/// Errors specific to BIND / package management operations.
#[derive(Error, Debug, PartialEq, Eq)]
pub enum BindError {
    /// Package already exists when ACTION(ADD) was specified.
    #[error("Package {collection}.{name}.{version} already exists")]
    PackageAlreadyExists {
        /// Collection ID.
        collection: String,
        /// Package name.
        name: String,
        /// Version.
        version: String,
    },

    /// Package not found in catalog.
    #[error("Package {collection}.{name}.{version} not found")]
    PackageNotFound {
        /// Collection ID.
        collection: String,
        /// Package name.
        name: String,
        /// Version.
        version: String,
    },

    /// Plan already exists when ACTION(ADD) was specified.
    #[error("Plan '{name}' already exists")]
    PlanAlreadyExists {
        /// Plan name.
        name: String,
    },

    /// Plan not found in catalog.
    #[error("Plan '{name}' not found")]
    PlanNotFound {
        /// Plan name.
        name: String,
    },

    /// DBRM has no SQL statements.
    #[error("DBRM '{program}' contains no SQL statements")]
    EmptyDbrm {
        /// Program name.
        program: String,
    },

    /// Package list references a package that is not in the catalog.
    #[error("Referenced package not found: {0}")]
    PackageReferenceNotFound(String),

    /// Invalid option combination.
    #[error("Invalid option: {0}")]
    InvalidOption(String),
}

/// Result alias for BIND operations.
pub type BindResult<T> = Result<T, BindError>;

// ---------------------------------------------------------------------------
// Enums — common option values
// ---------------------------------------------------------------------------

/// SQL isolation level for BIND options.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CatalogIsolation {
    /// Cursor Stability (CS) — read committed.
    CS,
    /// Repeatable Read (RR) — serializable.
    RR,
    /// Uncommitted Read (UR) — read uncommitted.
    UR,
    /// Read Stability (RS) — repeatable read.
    RS,
}

impl std::fmt::Display for CatalogIsolation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CS => write!(f, "CS"),
            Self::RR => write!(f, "RR"),
            Self::UR => write!(f, "UR"),
            Self::RS => write!(f, "RS"),
        }
    }
}

/// VALIDATE option — when to verify authorization and object existence.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CatalogValidate {
    /// Validate at run time.
    Run,
    /// Validate at bind time.
    Bind,
}

impl std::fmt::Display for CatalogValidate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Run => write!(f, "RUN"),
            Self::Bind => write!(f, "BIND"),
        }
    }
}

/// EXPLAIN option — whether to capture EXPLAIN output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CatalogExplain {
    /// Capture EXPLAIN output.
    Yes,
    /// Do not capture EXPLAIN output.
    No,
}

impl std::fmt::Display for CatalogExplain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Yes => write!(f, "YES"),
            Self::No => write!(f, "NO"),
        }
    }
}

/// ACTION option — what to do with an existing package / plan.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CatalogAction {
    /// Replace existing object.
    Replace,
    /// Add new (fail if exists).
    Add,
}

impl std::fmt::Display for CatalogAction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Replace => write!(f, "REPLACE"),
            Self::Add => write!(f, "ADD"),
        }
    }
}

// ---------------------------------------------------------------------------
// PackageBindOptions — SYS-114 story 5
// ---------------------------------------------------------------------------

/// Common BIND options shared by BIND PACKAGE, BIND PLAN, and REBIND.
#[derive(Debug, Clone)]
pub struct PackageBindOptions {
    /// Isolation level (default CS).
    pub isolation: CatalogIsolation,
    /// Validate timing (default BIND).
    pub validate: CatalogValidate,
    /// EXPLAIN output (default NO).
    pub explain: CatalogExplain,
    /// Action on existing object (default REPLACE).
    pub action: CatalogAction,
    /// Package / plan owner.
    pub owner: String,
    /// Default qualifier for unqualified objects.
    pub qualifier: String,
}

impl Default for PackageBindOptions {
    fn default() -> Self {
        Self {
            isolation: CatalogIsolation::CS,
            validate: CatalogValidate::Bind,
            explain: CatalogExplain::No,
            action: CatalogAction::Replace,
            owner: String::new(),
            qualifier: String::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// Dbrm — SYS-114 story 7
// ---------------------------------------------------------------------------

/// A Database Request Module (DBRM) — the input to BIND PACKAGE.
///
/// Contains extracted SQL statements and host variable metadata from a
/// pre-compiled application program.
#[derive(Debug, Clone)]
pub struct Dbrm {
    /// Program (module) name.
    pub program_name: String,
    /// SQL statement texts.
    pub sql_statements: Vec<String>,
    /// Host variable names referenced across all statements.
    pub host_variables: Vec<String>,
    /// Timestamp of DBRM creation.
    pub timestamp: String,
}

impl Dbrm {
    /// Create a new DBRM with the given program name.
    pub fn new(program_name: &str) -> Self {
        Self {
            program_name: program_name.to_string(),
            sql_statements: Vec::new(),
            host_variables: Vec::new(),
            timestamp: "2026-01-01T00:00:00Z".to_string(),
        }
    }

    /// Add an SQL statement to this DBRM.
    pub fn add_sql(&mut self, sql: &str) {
        self.sql_statements.push(sql.to_string());
    }

    /// Add a host variable reference.
    pub fn add_host_variable(&mut self, name: &str) {
        self.host_variables.push(name.to_string());
    }
}

// ---------------------------------------------------------------------------
// BindResult payload — catalog rows
// ---------------------------------------------------------------------------

/// A package entry stored in the catalog (SYSPACKAGE row).
#[derive(Debug, Clone)]
pub struct CatalogPackage {
    /// Collection ID.
    pub collection_id: String,
    /// Package name.
    pub name: String,
    /// Package version (empty string = default version).
    pub version: String,
    /// Owner of the package.
    pub owner: String,
    /// Default qualifier.
    pub qualifier: String,
    /// Isolation level.
    pub isolation: CatalogIsolation,
    /// DBRM program name that was bound.
    pub dbrm_name: String,
    /// Number of SQL statements.
    pub statement_count: usize,
    /// Validate option.
    pub validate: CatalogValidate,
    /// Explain option.
    pub explain: CatalogExplain,
    /// Bind timestamp.
    pub bind_timestamp: String,
}

/// A plan entry stored in the catalog (SYSPLAN row).
#[derive(Debug, Clone)]
pub struct CatalogPlan {
    /// Plan name.
    pub name: String,
    /// Owner.
    pub owner: String,
    /// Isolation level.
    pub isolation: CatalogIsolation,
    /// Validate option.
    pub validate: CatalogValidate,
    /// Bind timestamp.
    pub bind_timestamp: String,
}

/// An entry in the SYSPACKLIST table linking plans to packages.
#[derive(Debug, Clone)]
pub struct CatalogPackListEntry {
    /// Plan name.
    pub plan_name: String,
    /// Collection ID of included package.
    pub collection_id: String,
    /// Package name.
    pub package_name: String,
    /// Package version.
    pub version: String,
}

// ---------------------------------------------------------------------------
// Db2Catalog — SYS-114 story 6
// ---------------------------------------------------------------------------

/// Simulated DB2 catalog tables (SYSPACKAGE, SYSPLAN, SYSPACKLIST).
///
/// Uses `HashMap`-based storage with insert / query / delete operations.
#[derive(Debug, Default)]
pub struct Db2Catalog {
    /// SYSPACKAGE: keyed by `(collection_id, name, version)`.
    packages: HashMap<(String, String, String), CatalogPackage>,
    /// SYSPLAN: keyed by plan name.
    plans: HashMap<String, CatalogPlan>,
    /// SYSPACKLIST: list of plan-to-package associations.
    packlist: Vec<CatalogPackListEntry>,
}

impl Db2Catalog {
    /// Create a new empty catalog.
    pub fn new() -> Self {
        Self::default()
    }

    // -- package operations ------------------------------------------------

    /// Insert a package into the catalog.
    pub fn insert_package(&mut self, pkg: CatalogPackage) {
        let key = (
            pkg.collection_id.clone(),
            pkg.name.clone(),
            pkg.version.clone(),
        );
        self.packages.insert(key, pkg);
    }

    /// Look up a package by (collection, name, version).
    pub fn get_package(
        &self,
        collection: &str,
        name: &str,
        version: &str,
    ) -> Option<&CatalogPackage> {
        self.packages.get(&(
            collection.to_string(),
            name.to_string(),
            version.to_string(),
        ))
    }

    /// Check whether a package exists.
    pub fn package_exists(&self, collection: &str, name: &str, version: &str) -> bool {
        self.get_package(collection, name, version).is_some()
    }

    /// Remove a package from the catalog.  Returns `true` if it existed.
    pub fn remove_package(&mut self, collection: &str, name: &str, version: &str) -> bool {
        self.packages
            .remove(&(
                collection.to_string(),
                name.to_string(),
                version.to_string(),
            ))
            .is_some()
    }

    /// Return all packages in the catalog.
    pub fn list_packages(&self) -> Vec<&CatalogPackage> {
        self.packages.values().collect()
    }

    /// Query packages by collection ID.
    pub fn query_packages_by_collection(&self, collection: &str) -> Vec<&CatalogPackage> {
        self.packages
            .values()
            .filter(|p| p.collection_id == collection)
            .collect()
    }

    // -- plan operations ---------------------------------------------------

    /// Insert a plan into the catalog.
    pub fn insert_plan(&mut self, plan: CatalogPlan) {
        self.plans.insert(plan.name.clone(), plan);
    }

    /// Look up a plan by name.
    pub fn get_plan(&self, name: &str) -> Option<&CatalogPlan> {
        self.plans.get(name)
    }

    /// Check whether a plan exists.
    pub fn plan_exists(&self, name: &str) -> bool {
        self.get_plan(name).is_some()
    }

    /// Remove a plan and its packlist entries.  Returns `true` if it existed.
    pub fn remove_plan(&mut self, name: &str) -> bool {
        self.packlist.retain(|e| e.plan_name != name);
        self.plans.remove(name).is_some()
    }

    /// Return all plans in the catalog.
    pub fn list_plans(&self) -> Vec<&CatalogPlan> {
        self.plans.values().collect()
    }

    // -- packlist operations -----------------------------------------------

    /// Add a packlist entry (plan -> package association).
    pub fn add_packlist_entry(&mut self, entry: CatalogPackListEntry) {
        self.packlist.push(entry);
    }

    /// Get all packlist entries for a given plan.
    pub fn get_packlist_for_plan(&self, plan_name: &str) -> Vec<&CatalogPackListEntry> {
        self.packlist
            .iter()
            .filter(|e| e.plan_name == plan_name)
            .collect()
    }

    /// Remove all packlist entries for a given plan.
    pub fn remove_packlist_for_plan(&mut self, plan_name: &str) {
        self.packlist.retain(|e| e.plan_name != plan_name);
    }
}

// ---------------------------------------------------------------------------
// BindPackage — SYS-114 story 1
// ---------------------------------------------------------------------------

/// BIND PACKAGE command — binds a DBRM into a package in the catalog.
#[derive(Debug, Clone)]
pub struct BindPackage {
    /// Collection ID for the resulting package.
    pub collection_id: String,
    /// Package name (if empty, derived from DBRM program name).
    pub package_name: String,
    /// Package version (empty = default).
    pub version: String,
    /// Owner of the resulting package.
    pub owner: String,
    /// Default qualifier for unqualified SQL objects.
    pub qualifier: String,
    /// Isolation level.
    pub isolation: CatalogIsolation,
    /// DBRM name (program name).
    pub dbrm_name: String,
    /// Bind options.
    pub options: PackageBindOptions,
}

impl BindPackage {
    /// Create a new BIND PACKAGE command with minimal required fields.
    pub fn new(collection_id: &str, dbrm_name: &str) -> Self {
        Self {
            collection_id: collection_id.to_string(),
            package_name: String::new(),
            version: String::new(),
            owner: String::new(),
            qualifier: String::new(),
            isolation: CatalogIsolation::CS,
            dbrm_name: dbrm_name.to_string(),
            options: PackageBindOptions::default(),
        }
    }

    /// Execute the BIND PACKAGE against the catalog using the given DBRM.
    pub fn bind(&self, catalog: &mut Db2Catalog, dbrm: &Dbrm) -> BindResult<CatalogPackage> {
        if dbrm.sql_statements.is_empty() {
            return Err(BindError::EmptyDbrm {
                program: dbrm.program_name.clone(),
            });
        }

        let pkg_name = if self.package_name.is_empty() {
            dbrm.program_name.clone()
        } else {
            self.package_name.clone()
        };

        let version = if self.version.is_empty() {
            String::new()
        } else {
            self.version.clone()
        };

        // Check ACTION
        if self.options.action == CatalogAction::Add
            && catalog.package_exists(&self.collection_id, &pkg_name, &version)
        {
            return Err(BindError::PackageAlreadyExists {
                collection: self.collection_id.clone(),
                name: pkg_name,
                version,
            });
        }

        let owner = if self.owner.is_empty() {
            self.options.owner.clone()
        } else {
            self.owner.clone()
        };

        let qualifier = if self.qualifier.is_empty() {
            self.options.qualifier.clone()
        } else {
            self.qualifier.clone()
        };

        let pkg = CatalogPackage {
            collection_id: self.collection_id.clone(),
            name: pkg_name,
            version,
            owner,
            qualifier,
            isolation: self.isolation,
            dbrm_name: dbrm.program_name.clone(),
            statement_count: dbrm.sql_statements.len(),
            validate: self.options.validate,
            explain: self.options.explain,
            bind_timestamp: "2026-01-01T00:00:00Z".to_string(),
        };

        catalog.insert_package(pkg.clone());
        Ok(pkg)
    }
}

// ---------------------------------------------------------------------------
// BindPlan — SYS-114 story 2
// ---------------------------------------------------------------------------

/// A reference to a package to include in a plan.
#[derive(Debug, Clone)]
pub struct PackageRef {
    /// Collection ID.
    pub collection_id: String,
    /// Package name.
    pub package_name: String,
    /// Package version.
    pub version: String,
}

/// BIND PLAN command — binds a plan that references one or more packages.
#[derive(Debug, Clone)]
pub struct BindPlan {
    /// Plan name.
    pub plan_name: String,
    /// List of packages to include.
    pub packages: Vec<PackageRef>,
    /// Bind options.
    pub options: PackageBindOptions,
}

impl BindPlan {
    /// Create a new BIND PLAN command.
    pub fn new(plan_name: &str) -> Self {
        Self {
            plan_name: plan_name.to_string(),
            packages: Vec::new(),
            options: PackageBindOptions::default(),
        }
    }

    /// Add a package reference.
    pub fn add_package(&mut self, collection: &str, name: &str, version: &str) {
        self.packages.push(PackageRef {
            collection_id: collection.to_string(),
            package_name: name.to_string(),
            version: version.to_string(),
        });
    }

    /// Execute the BIND PLAN against the catalog.
    ///
    /// All referenced packages must exist in the catalog unless
    /// VALIDATE(RUN) is specified.
    pub fn bind(&self, catalog: &mut Db2Catalog) -> BindResult<CatalogPlan> {
        // Check ACTION
        if self.options.action == CatalogAction::Add && catalog.plan_exists(&self.plan_name) {
            return Err(BindError::PlanAlreadyExists {
                name: self.plan_name.clone(),
            });
        }

        // Validate package references (unless VALIDATE(RUN))
        if self.options.validate == CatalogValidate::Bind {
            for pkg_ref in &self.packages {
                if !catalog.package_exists(
                    &pkg_ref.collection_id,
                    &pkg_ref.package_name,
                    &pkg_ref.version,
                ) {
                    return Err(BindError::PackageReferenceNotFound(format!(
                        "{}.{}.{}",
                        pkg_ref.collection_id, pkg_ref.package_name, pkg_ref.version
                    )));
                }
            }
        }

        let plan = CatalogPlan {
            name: self.plan_name.clone(),
            owner: self.options.owner.clone(),
            isolation: self.options.isolation,
            validate: self.options.validate,
            bind_timestamp: "2026-01-01T00:00:00Z".to_string(),
        };

        catalog.insert_plan(plan.clone());

        // Remove old packlist entries, insert fresh ones
        catalog.remove_packlist_for_plan(&self.plan_name);
        for pkg_ref in &self.packages {
            catalog.add_packlist_entry(CatalogPackListEntry {
                plan_name: self.plan_name.clone(),
                collection_id: pkg_ref.collection_id.clone(),
                package_name: pkg_ref.package_name.clone(),
                version: pkg_ref.version.clone(),
            });
        }

        Ok(plan)
    }
}

// ---------------------------------------------------------------------------
// RebindCommand — SYS-114 story 3
// ---------------------------------------------------------------------------

/// Target of a REBIND command.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RebindTarget {
    /// REBIND PACKAGE(collection.name.version).
    Package {
        /// Collection ID.
        collection: String,
        /// Package name.
        name: String,
        /// Version.
        version: String,
    },
    /// REBIND PLAN(name).
    Plan {
        /// Plan name.
        name: String,
    },
}

/// REBIND command — re-binds an existing package or plan with updated options.
#[derive(Debug, Clone)]
pub struct RebindCommand {
    /// What to rebind.
    pub target: RebindTarget,
    /// Optional new isolation level.
    pub isolation: Option<CatalogIsolation>,
    /// Optional new validate option.
    pub validate: Option<CatalogValidate>,
    /// Optional new explain option.
    pub explain: Option<CatalogExplain>,
}

impl RebindCommand {
    /// Create a REBIND PACKAGE command.
    pub fn package(collection: &str, name: &str, version: &str) -> Self {
        Self {
            target: RebindTarget::Package {
                collection: collection.to_string(),
                name: name.to_string(),
                version: version.to_string(),
            },
            isolation: None,
            validate: None,
            explain: None,
        }
    }

    /// Create a REBIND PLAN command.
    pub fn plan(name: &str) -> Self {
        Self {
            target: RebindTarget::Plan {
                name: name.to_string(),
            },
            isolation: None,
            validate: None,
            explain: None,
        }
    }

    /// Execute the REBIND against the catalog, updating stored options.
    pub fn execute(&self, catalog: &mut Db2Catalog) -> BindResult<()> {
        match &self.target {
            RebindTarget::Package {
                collection,
                name,
                version,
            } => {
                let key = (collection.clone(), name.clone(), version.clone());
                let pkg = catalog.packages.get_mut(&key).ok_or_else(|| {
                    BindError::PackageNotFound {
                        collection: collection.clone(),
                        name: name.clone(),
                        version: version.clone(),
                    }
                })?;
                if let Some(iso) = self.isolation {
                    pkg.isolation = iso;
                }
                if let Some(val) = self.validate {
                    pkg.validate = val;
                }
                if let Some(exp) = self.explain {
                    pkg.explain = exp;
                }
                pkg.bind_timestamp = "2026-01-01T00:00:01Z".to_string();
                Ok(())
            }
            RebindTarget::Plan { name } => {
                let plan =
                    catalog
                        .plans
                        .get_mut(name)
                        .ok_or_else(|| BindError::PlanNotFound {
                            name: name.clone(),
                        })?;
                if let Some(iso) = self.isolation {
                    plan.isolation = iso;
                }
                if let Some(val) = self.validate {
                    plan.validate = val;
                }
                plan.bind_timestamp = "2026-01-01T00:00:01Z".to_string();
                Ok(())
            }
        }
    }
}

// ---------------------------------------------------------------------------
// FreeCommand — SYS-114 story 4
// ---------------------------------------------------------------------------

/// Target of a FREE command.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FreeTarget {
    /// FREE PACKAGE(collection.name.version).
    Package {
        /// Collection ID.
        collection: String,
        /// Package name.
        name: String,
        /// Version.
        version: String,
    },
    /// FREE PLAN(name).
    Plan {
        /// Plan name.
        name: String,
    },
}

/// FREE command — removes a package or plan from the catalog.
#[derive(Debug, Clone)]
pub struct FreeCommand {
    /// What to free.
    pub target: FreeTarget,
}

impl FreeCommand {
    /// Create a FREE PACKAGE command.
    pub fn package(collection: &str, name: &str, version: &str) -> Self {
        Self {
            target: FreeTarget::Package {
                collection: collection.to_string(),
                name: name.to_string(),
                version: version.to_string(),
            },
        }
    }

    /// Create a FREE PLAN command.
    pub fn plan(name: &str) -> Self {
        Self {
            target: FreeTarget::Plan {
                name: name.to_string(),
            },
        }
    }

    /// Execute the FREE against the catalog.
    pub fn execute(&self, catalog: &mut Db2Catalog) -> BindResult<()> {
        match &self.target {
            FreeTarget::Package {
                collection,
                name,
                version,
            } => {
                if !catalog.remove_package(collection, name, version) {
                    return Err(BindError::PackageNotFound {
                        collection: collection.clone(),
                        name: name.clone(),
                        version: version.clone(),
                    });
                }
                Ok(())
            }
            FreeTarget::Plan { name } => {
                if !catalog.remove_plan(name) {
                    return Err(BindError::PlanNotFound { name: name.clone() });
                }
                Ok(())
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Tests — SYS-114 story 8
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -- helpers -----------------------------------------------------------

    fn sample_dbrm() -> Dbrm {
        let mut dbrm = Dbrm::new("CUSTPROG");
        dbrm.add_sql("SELECT NAME FROM CUSTOMER WHERE ID = ?");
        dbrm.add_sql("INSERT INTO ORDERS (CUST_ID, AMOUNT) VALUES (?, ?)");
        dbrm.add_host_variable("WS-ID");
        dbrm.add_host_variable("WS-NAME");
        dbrm
    }

    fn bind_sample_package(catalog: &mut Db2Catalog) -> CatalogPackage {
        let dbrm = sample_dbrm();
        let cmd = BindPackage::new("PROD", "CUSTPROG");
        cmd.bind(catalog, &dbrm).unwrap()
    }

    // -- BindPackage -------------------------------------------------------

    #[test]
    fn test_bind_package_basic() {
        let mut catalog = Db2Catalog::new();
        let pkg = bind_sample_package(&mut catalog);

        assert_eq!(pkg.collection_id, "PROD");
        assert_eq!(pkg.name, "CUSTPROG");
        assert_eq!(pkg.statement_count, 2);
        assert_eq!(pkg.isolation, CatalogIsolation::CS);
        assert!(catalog.package_exists("PROD", "CUSTPROG", ""));
    }

    #[test]
    fn test_bind_package_custom_name_and_version() {
        let mut catalog = Db2Catalog::new();
        let dbrm = sample_dbrm();
        let mut cmd = BindPackage::new("PROD", "CUSTPROG");
        cmd.package_name = "MYPKG".to_string();
        cmd.version = "V1".to_string();
        cmd.owner = "ADMIN".to_string();
        cmd.qualifier = "SCHEMA1".to_string();
        cmd.isolation = CatalogIsolation::RR;

        let pkg = cmd.bind(&mut catalog, &dbrm).unwrap();
        assert_eq!(pkg.name, "MYPKG");
        assert_eq!(pkg.version, "V1");
        assert_eq!(pkg.owner, "ADMIN");
        assert_eq!(pkg.qualifier, "SCHEMA1");
        assert_eq!(pkg.isolation, CatalogIsolation::RR);
    }

    #[test]
    fn test_bind_package_action_add_fails_on_duplicate() {
        let mut catalog = Db2Catalog::new();
        let dbrm = sample_dbrm();
        let mut cmd = BindPackage::new("PROD", "CUSTPROG");
        cmd.options.action = CatalogAction::Add;

        cmd.bind(&mut catalog, &dbrm).unwrap();
        let err = cmd.bind(&mut catalog, &dbrm).unwrap_err();
        assert!(matches!(err, BindError::PackageAlreadyExists { .. }));
    }

    #[test]
    fn test_bind_package_action_replace_succeeds_on_duplicate() {
        let mut catalog = Db2Catalog::new();
        let dbrm = sample_dbrm();
        let cmd = BindPackage::new("PROD", "CUSTPROG");

        cmd.bind(&mut catalog, &dbrm).unwrap();
        let pkg = cmd.bind(&mut catalog, &dbrm).unwrap();
        assert_eq!(pkg.name, "CUSTPROG");
    }

    #[test]
    fn test_bind_package_empty_dbrm_fails() {
        let mut catalog = Db2Catalog::new();
        let dbrm = Dbrm::new("EMPTYPROG");
        let cmd = BindPackage::new("PROD", "EMPTYPROG");

        let err = cmd.bind(&mut catalog, &dbrm).unwrap_err();
        assert!(matches!(err, BindError::EmptyDbrm { .. }));
    }

    #[test]
    fn test_bind_package_options_explain_yes() {
        let mut catalog = Db2Catalog::new();
        let dbrm = sample_dbrm();
        let mut cmd = BindPackage::new("PROD", "CUSTPROG");
        cmd.options.explain = CatalogExplain::Yes;

        let pkg = cmd.bind(&mut catalog, &dbrm).unwrap();
        assert_eq!(pkg.explain, CatalogExplain::Yes);
    }

    // -- BindPlan ----------------------------------------------------------

    #[test]
    fn test_bind_plan_basic() {
        let mut catalog = Db2Catalog::new();
        bind_sample_package(&mut catalog);

        let mut plan_cmd = BindPlan::new("MYPLAN");
        plan_cmd.add_package("PROD", "CUSTPROG", "");

        let plan = plan_cmd.bind(&mut catalog).unwrap();
        assert_eq!(plan.name, "MYPLAN");

        let entries = catalog.get_packlist_for_plan("MYPLAN");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].package_name, "CUSTPROG");
    }

    #[test]
    fn test_bind_plan_missing_package_fails() {
        let mut catalog = Db2Catalog::new();
        let mut plan_cmd = BindPlan::new("BADPLAN");
        plan_cmd.add_package("PROD", "NONEXIST", "");

        let err = plan_cmd.bind(&mut catalog).unwrap_err();
        assert!(matches!(err, BindError::PackageReferenceNotFound(_)));
    }

    #[test]
    fn test_bind_plan_validate_run_skips_check() {
        let mut catalog = Db2Catalog::new();
        let mut plan_cmd = BindPlan::new("DEFERPLAN");
        plan_cmd.add_package("PROD", "NONEXIST", "");
        plan_cmd.options.validate = CatalogValidate::Run;

        let plan = plan_cmd.bind(&mut catalog).unwrap();
        assert_eq!(plan.name, "DEFERPLAN");
    }

    #[test]
    fn test_bind_plan_action_add_fails_on_duplicate() {
        let mut catalog = Db2Catalog::new();
        bind_sample_package(&mut catalog);

        let mut plan_cmd = BindPlan::new("DUP");
        plan_cmd.add_package("PROD", "CUSTPROG", "");
        plan_cmd.options.action = CatalogAction::Add;

        plan_cmd.bind(&mut catalog).unwrap();
        let err = plan_cmd.bind(&mut catalog).unwrap_err();
        assert!(matches!(err, BindError::PlanAlreadyExists { .. }));
    }

    #[test]
    fn test_bind_plan_multiple_packages() {
        let mut catalog = Db2Catalog::new();
        // Bind two packages
        let dbrm1 = sample_dbrm();
        BindPackage::new("PROD", "CUSTPROG")
            .bind(&mut catalog, &dbrm1)
            .unwrap();

        let mut dbrm2 = Dbrm::new("ORDERPROG");
        dbrm2.add_sql("SELECT * FROM ORDERS");
        let mut cmd2 = BindPackage::new("PROD", "ORDERPROG");
        cmd2.package_name = "ORDERPROG".to_string();
        cmd2.bind(&mut catalog, &dbrm2).unwrap();

        let mut plan_cmd = BindPlan::new("BIGPLAN");
        plan_cmd.add_package("PROD", "CUSTPROG", "");
        plan_cmd.add_package("PROD", "ORDERPROG", "");

        plan_cmd.bind(&mut catalog).unwrap();
        let entries = catalog.get_packlist_for_plan("BIGPLAN");
        assert_eq!(entries.len(), 2);
    }

    // -- RebindCommand -----------------------------------------------------

    #[test]
    fn test_rebind_package() {
        let mut catalog = Db2Catalog::new();
        bind_sample_package(&mut catalog);

        let mut rebind = RebindCommand::package("PROD", "CUSTPROG", "");
        rebind.isolation = Some(CatalogIsolation::RR);
        rebind.explain = Some(CatalogExplain::Yes);

        rebind.execute(&mut catalog).unwrap();

        let pkg = catalog.get_package("PROD", "CUSTPROG", "").unwrap();
        assert_eq!(pkg.isolation, CatalogIsolation::RR);
        assert_eq!(pkg.explain, CatalogExplain::Yes);
    }

    #[test]
    fn test_rebind_package_not_found() {
        let mut catalog = Db2Catalog::new();
        let rebind = RebindCommand::package("X", "Y", "Z");
        let err = rebind.execute(&mut catalog).unwrap_err();
        assert!(matches!(err, BindError::PackageNotFound { .. }));
    }

    #[test]
    fn test_rebind_plan() {
        let mut catalog = Db2Catalog::new();
        bind_sample_package(&mut catalog);

        let mut plan_cmd = BindPlan::new("MYPLAN");
        plan_cmd.add_package("PROD", "CUSTPROG", "");
        plan_cmd.bind(&mut catalog).unwrap();

        let mut rebind = RebindCommand::plan("MYPLAN");
        rebind.isolation = Some(CatalogIsolation::UR);
        rebind.validate = Some(CatalogValidate::Run);

        rebind.execute(&mut catalog).unwrap();

        let plan = catalog.get_plan("MYPLAN").unwrap();
        assert_eq!(plan.isolation, CatalogIsolation::UR);
        assert_eq!(plan.validate, CatalogValidate::Run);
    }

    #[test]
    fn test_rebind_plan_not_found() {
        let mut catalog = Db2Catalog::new();
        let rebind = RebindCommand::plan("NOPLAN");
        let err = rebind.execute(&mut catalog).unwrap_err();
        assert!(matches!(err, BindError::PlanNotFound { .. }));
    }

    // -- FreeCommand -------------------------------------------------------

    #[test]
    fn test_free_package() {
        let mut catalog = Db2Catalog::new();
        bind_sample_package(&mut catalog);

        assert!(catalog.package_exists("PROD", "CUSTPROG", ""));

        FreeCommand::package("PROD", "CUSTPROG", "")
            .execute(&mut catalog)
            .unwrap();

        assert!(!catalog.package_exists("PROD", "CUSTPROG", ""));
    }

    #[test]
    fn test_free_package_not_found() {
        let mut catalog = Db2Catalog::new();
        let err = FreeCommand::package("X", "Y", "Z")
            .execute(&mut catalog)
            .unwrap_err();
        assert!(matches!(err, BindError::PackageNotFound { .. }));
    }

    #[test]
    fn test_free_plan() {
        let mut catalog = Db2Catalog::new();
        bind_sample_package(&mut catalog);

        let mut plan_cmd = BindPlan::new("MYPLAN");
        plan_cmd.add_package("PROD", "CUSTPROG", "");
        plan_cmd.bind(&mut catalog).unwrap();

        FreeCommand::plan("MYPLAN")
            .execute(&mut catalog)
            .unwrap();

        assert!(!catalog.plan_exists("MYPLAN"));
        assert!(catalog.get_packlist_for_plan("MYPLAN").is_empty());
    }

    #[test]
    fn test_free_plan_not_found() {
        let mut catalog = Db2Catalog::new();
        let err = FreeCommand::plan("NOPLAN")
            .execute(&mut catalog)
            .unwrap_err();
        assert!(matches!(err, BindError::PlanNotFound { .. }));
    }

    // -- Full lifecycle ----------------------------------------------------

    #[test]
    fn test_full_bind_rebind_free_lifecycle() {
        let mut catalog = Db2Catalog::new();

        // 1. BIND PACKAGE
        let dbrm = sample_dbrm();
        let bind_pkg = BindPackage::new("PROD", "CUSTPROG");
        bind_pkg.bind(&mut catalog, &dbrm).unwrap();

        // 2. BIND PLAN
        let mut bind_plan = BindPlan::new("MAINPLAN");
        bind_plan.add_package("PROD", "CUSTPROG", "");
        bind_plan.bind(&mut catalog).unwrap();

        assert_eq!(catalog.list_packages().len(), 1);
        assert_eq!(catalog.list_plans().len(), 1);

        // 3. REBIND PACKAGE with new isolation
        let mut rebind = RebindCommand::package("PROD", "CUSTPROG", "");
        rebind.isolation = Some(CatalogIsolation::RS);
        rebind.execute(&mut catalog).unwrap();

        let pkg = catalog.get_package("PROD", "CUSTPROG", "").unwrap();
        assert_eq!(pkg.isolation, CatalogIsolation::RS);

        // 4. FREE PLAN
        FreeCommand::plan("MAINPLAN")
            .execute(&mut catalog)
            .unwrap();
        assert!(!catalog.plan_exists("MAINPLAN"));

        // 5. FREE PACKAGE
        FreeCommand::package("PROD", "CUSTPROG", "")
            .execute(&mut catalog)
            .unwrap();
        assert!(!catalog.package_exists("PROD", "CUSTPROG", ""));
        assert!(catalog.list_packages().is_empty());
    }

    // -- Catalog queries ---------------------------------------------------

    #[test]
    fn test_catalog_query_by_collection() {
        let mut catalog = Db2Catalog::new();
        let dbrm = sample_dbrm();

        let cmd1 = BindPackage::new("PROD", "CUSTPROG");
        cmd1.bind(&mut catalog, &dbrm).unwrap();

        let mut cmd2 = BindPackage::new("TEST", "CUSTPROG");
        cmd2.package_name = "TESTPKG".to_string();
        cmd2.bind(&mut catalog, &dbrm).unwrap();

        let prod = catalog.query_packages_by_collection("PROD");
        assert_eq!(prod.len(), 1);
        assert_eq!(prod[0].collection_id, "PROD");

        let test = catalog.query_packages_by_collection("TEST");
        assert_eq!(test.len(), 1);
        assert_eq!(test[0].collection_id, "TEST");
    }

    // -- Option validation -------------------------------------------------

    #[test]
    fn test_option_display_formats() {
        assert_eq!(CatalogIsolation::CS.to_string(), "CS");
        assert_eq!(CatalogIsolation::RR.to_string(), "RR");
        assert_eq!(CatalogIsolation::UR.to_string(), "UR");
        assert_eq!(CatalogIsolation::RS.to_string(), "RS");

        assert_eq!(CatalogValidate::Run.to_string(), "RUN");
        assert_eq!(CatalogValidate::Bind.to_string(), "BIND");

        assert_eq!(CatalogExplain::Yes.to_string(), "YES");
        assert_eq!(CatalogExplain::No.to_string(), "NO");

        assert_eq!(CatalogAction::Replace.to_string(), "REPLACE");
        assert_eq!(CatalogAction::Add.to_string(), "ADD");
    }

    #[test]
    fn test_default_options() {
        let opts = PackageBindOptions::default();
        assert_eq!(opts.isolation, CatalogIsolation::CS);
        assert_eq!(opts.validate, CatalogValidate::Bind);
        assert_eq!(opts.explain, CatalogExplain::No);
        assert_eq!(opts.action, CatalogAction::Replace);
        assert!(opts.owner.is_empty());
        assert!(opts.qualifier.is_empty());
    }

    // -- Dbrm struct -------------------------------------------------------

    #[test]
    fn test_dbrm_creation_and_mutation() {
        let mut dbrm = Dbrm::new("MYPROG");
        assert_eq!(dbrm.program_name, "MYPROG");
        assert!(dbrm.sql_statements.is_empty());
        assert!(dbrm.host_variables.is_empty());

        dbrm.add_sql("SELECT 1");
        dbrm.add_sql("SELECT 2");
        dbrm.add_host_variable("WS-A");
        dbrm.add_host_variable("WS-B");

        assert_eq!(dbrm.sql_statements.len(), 2);
        assert_eq!(dbrm.host_variables.len(), 2);
        assert!(!dbrm.timestamp.is_empty());
    }

    // -- Catalog list operations -------------------------------------------

    #[test]
    fn test_catalog_list_packages_and_plans() {
        let mut catalog = Db2Catalog::new();
        assert!(catalog.list_packages().is_empty());
        assert!(catalog.list_plans().is_empty());

        let dbrm = sample_dbrm();
        BindPackage::new("C1", "P1")
            .bind(&mut catalog, &dbrm)
            .unwrap();

        let mut dbrm2 = Dbrm::new("P2");
        dbrm2.add_sql("SELECT 1");
        let mut cmd = BindPackage::new("C1", "P2");
        cmd.package_name = "P2".to_string();
        cmd.bind(&mut catalog, &dbrm2).unwrap();

        assert_eq!(catalog.list_packages().len(), 2);

        let mut plan_cmd = BindPlan::new("PLAN1");
        plan_cmd.add_package("C1", "P1", "");
        plan_cmd.options.validate = CatalogValidate::Run;
        plan_cmd.bind(&mut catalog).unwrap();
        assert_eq!(catalog.list_plans().len(), 1);
    }
}
