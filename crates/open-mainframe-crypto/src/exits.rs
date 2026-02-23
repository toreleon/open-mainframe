//! SEC-109: RACF Exits, Utilities & Configuration.
//!
//! Provides RACF exit points (ICHRTX00, ICHPWX01, IRREVX01), an exit
//! registry for registering and invoking callback functions, and simulated
//! RACF utility programs (IRRUT100, IRRUT200, IRRUT400).

use std::collections::HashMap;
use std::fmt;

// ---------------------------------------------------------------------------
// Story 1: Exit Points
// ---------------------------------------------------------------------------

/// RACF exit points.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExitPoint {
    /// ICHRTX00 — Pre-authorization exit (invoked before RACROUTE REQUEST=AUTH).
    PreAuth,
    /// ICHPWX01 — Password quality/validation exit.
    PasswordQuality,
    /// IRREVX01 — Event notification exit (invoked after security events).
    EventNotification,
}

impl fmt::Display for ExitPoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::PreAuth => write!(f, "ICHRTX00"),
            Self::PasswordQuality => write!(f, "ICHPWX01"),
            Self::EventNotification => write!(f, "IRREVX01"),
        }
    }
}

// ---------------------------------------------------------------------------
// Story 2: Exit Registry
// ---------------------------------------------------------------------------

/// Result of an exit invocation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExitAction {
    /// Allow the operation to proceed.
    Allow,
    /// Deny the operation.
    Deny,
    /// Let RACF handle normally (exit takes no action).
    PassThrough,
}

impl fmt::Display for ExitAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Allow => write!(f, "ALLOW"),
            Self::Deny => write!(f, "DENY"),
            Self::PassThrough => write!(f, "PASS_THROUGH"),
        }
    }
}

/// Context passed to an exit function.
#[derive(Debug, Clone)]
pub struct ExitContext {
    /// User ID involved in the operation.
    pub user: String,
    /// Resource or operation name.
    pub resource: String,
    /// Additional data (e.g., proposed password for ICHPWX01).
    pub data: String,
}

impl ExitContext {
    /// Create a new exit context.
    pub fn new(user: &str, resource: &str, data: &str) -> Self {
        Self {
            user: user.to_string(),
            resource: resource.to_string(),
            data: data.to_string(),
        }
    }
}

/// An exit handler function type.
///
/// Takes an `ExitContext` and returns an `ExitAction`.
type ExitHandler = Box<dyn Fn(&ExitContext) -> ExitAction + Send + Sync>;

/// Registry for RACF exit handlers.
///
/// Allows registering callback functions at specific exit points and
/// invoking them during security processing.
pub struct ExitRegistry {
    /// Registered handlers indexed by exit point.
    handlers: HashMap<ExitPoint, ExitHandler>,
}

impl ExitRegistry {
    /// Create a new empty exit registry.
    pub fn new() -> Self {
        Self {
            handlers: HashMap::new(),
        }
    }

    /// Register an exit handler for a given exit point.
    ///
    /// Returns an error if a handler is already registered for that point.
    pub fn register<F>(&mut self, point: ExitPoint, handler: F) -> crate::Result<()>
    where
        F: Fn(&ExitContext) -> ExitAction + Send + Sync + 'static,
    {
        if self.handlers.contains_key(&point) {
            return Err(crate::error::CryptoError::ExitAlreadyRegistered {
                point: point.to_string(),
            });
        }
        self.handlers.insert(point, Box::new(handler));
        Ok(())
    }

    /// Deregister an exit handler for a given exit point.
    ///
    /// Returns `true` if a handler was removed.
    pub fn deregister(&mut self, point: ExitPoint) -> bool {
        self.handlers.remove(&point).is_some()
    }

    /// Invoke the exit handler for a given exit point.
    ///
    /// Returns `ExitAction::PassThrough` if no handler is registered.
    pub fn invoke(&self, point: ExitPoint, context: &ExitContext) -> ExitAction {
        self.handlers
            .get(&point)
            .map(|h| h(context))
            .unwrap_or(ExitAction::PassThrough)
    }

    /// Returns `true` if a handler is registered for the given exit point.
    pub fn is_registered(&self, point: ExitPoint) -> bool {
        self.handlers.contains_key(&point)
    }

    /// Returns the number of registered handlers.
    pub fn len(&self) -> usize {
        self.handlers.len()
    }

    /// Returns `true` if no handlers are registered.
    pub fn is_empty(&self) -> bool {
        self.handlers.is_empty()
    }
}

impl Default for ExitRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// We implement Debug manually since closures don't implement Debug.
impl fmt::Debug for ExitRegistry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExitRegistry")
            .field("handler_count", &self.handlers.len())
            .field(
                "registered_points",
                &self.handlers.keys().collect::<Vec<_>>(),
            )
            .finish()
    }
}

// ---------------------------------------------------------------------------
// Story 3: RACF Search Utility (IRRUT100)
// ---------------------------------------------------------------------------

/// A search criterion for IRRUT100.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SearchCriterion {
    /// Field to search (e.g., "NAME", "DFLTGRP", "CLASS").
    pub field: String,
    /// Pattern to match (supports `*` wildcard).
    pub pattern: String,
}

impl SearchCriterion {
    /// Create a new search criterion.
    pub fn new(field: &str, pattern: &str) -> Self {
        Self {
            field: field.to_uppercase(),
            pattern: pattern.to_string(),
        }
    }

    /// Check if a value matches this criterion's pattern.
    ///
    /// Supports `*` as a trailing wildcard.
    pub fn matches(&self, value: &str) -> bool {
        if self.pattern.ends_with('*') {
            let prefix = &self.pattern[..self.pattern.len() - 1];
            value.to_uppercase().starts_with(&prefix.to_uppercase())
        } else {
            value.eq_ignore_ascii_case(&self.pattern)
        }
    }
}

/// A search result record.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SearchResultEntry {
    /// The profile type (USER, GROUP, DATASET, GENERAL).
    pub profile_type: String,
    /// The profile name.
    pub name: String,
}

/// IRRUT100 — RACF database search utility.
///
/// Searches the RACF database by field-based criteria.
#[derive(Debug, Default)]
pub struct RacfSearchUtil {
    /// Known entries (profile_type, name, fields).
    entries: Vec<(String, String, HashMap<String, String>)>,
}

impl RacfSearchUtil {
    /// Create a new search utility instance.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add an entry to the searchable database.
    pub fn add_entry(
        &mut self,
        profile_type: &str,
        name: &str,
        fields: HashMap<String, String>,
    ) {
        self.entries.push((
            profile_type.to_uppercase(),
            name.to_string(),
            fields,
        ));
    }

    /// Search for entries matching the given criteria.
    ///
    /// All criteria must match for an entry to be included in results.
    pub fn search(&self, criteria: &[SearchCriterion]) -> Vec<SearchResultEntry> {
        self.entries
            .iter()
            .filter(|(_, _, fields)| {
                criteria.iter().all(|c| {
                    fields
                        .get(&c.field)
                        .is_some_and(|v| c.matches(v))
                })
            })
            .map(|(pt, name, _)| SearchResultEntry {
                profile_type: pt.clone(),
                name: name.clone(),
            })
            .collect()
    }
}

// ---------------------------------------------------------------------------
// Story 4: RACF Verify Utility (IRRUT200)
// ---------------------------------------------------------------------------

/// Integrity check result.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntegrityIssue {
    /// Description of the issue.
    pub description: String,
    /// Severity: "ERROR" or "WARNING".
    pub severity: String,
}

/// IRRUT200 — RACF database integrity verification utility.
///
/// Checks the RACF database for structural integrity issues such as
/// orphaned connections, missing superior groups, and duplicate entries.
#[derive(Debug, Default)]
pub struct RacfVerifyUtil {
    /// User entries.
    users: Vec<String>,
    /// Group entries.
    groups: Vec<String>,
    /// Connections (user, group).
    connections: Vec<(String, String)>,
}

impl RacfVerifyUtil {
    /// Create a new verify utility instance.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a user entry.
    pub fn add_user(&mut self, userid: &str) {
        self.users.push(userid.to_uppercase());
    }

    /// Add a group entry.
    pub fn add_group(&mut self, group: &str) {
        self.groups.push(group.to_uppercase());
    }

    /// Add a connection entry.
    pub fn add_connection(&mut self, userid: &str, group: &str) {
        self.connections
            .push((userid.to_uppercase(), group.to_uppercase()));
    }

    /// Run integrity checks and return any issues found.
    pub fn verify(&self) -> Vec<IntegrityIssue> {
        let mut issues = Vec::new();

        // Check for orphaned connections (user not found).
        for (user, group) in &self.connections {
            if !self.users.contains(user) {
                issues.push(IntegrityIssue {
                    description: format!(
                        "Orphaned connection: user '{}' in group '{}' not found",
                        user, group
                    ),
                    severity: "ERROR".into(),
                });
            }
            if !self.groups.contains(group) {
                issues.push(IntegrityIssue {
                    description: format!(
                        "Orphaned connection: group '{}' for user '{}' not found",
                        group, user
                    ),
                    severity: "ERROR".into(),
                });
            }
        }

        // Check for duplicate users.
        let mut seen = std::collections::HashSet::new();
        for user in &self.users {
            if !seen.insert(user) {
                issues.push(IntegrityIssue {
                    description: format!("Duplicate user entry: '{}'", user),
                    severity: "WARNING".into(),
                });
            }
        }

        // Check for duplicate groups.
        seen.clear();
        for group in &self.groups {
            if !seen.insert(group) {
                issues.push(IntegrityIssue {
                    description: format!("Duplicate group entry: '{}'", group),
                    severity: "WARNING".into(),
                });
            }
        }

        issues
    }
}

// ---------------------------------------------------------------------------
// Story 5: RACF Split/Merge Utility (IRRUT400)
// ---------------------------------------------------------------------------

/// A partition of the RACF database for split/merge.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DatabasePartition {
    /// Partition name/identifier.
    pub name: String,
    /// Users in this partition.
    pub users: Vec<String>,
    /// Groups in this partition.
    pub groups: Vec<String>,
}

impl DatabasePartition {
    /// Create a new empty partition.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            users: Vec::new(),
            groups: Vec::new(),
        }
    }

    /// Returns the total number of entries.
    pub fn entry_count(&self) -> usize {
        self.users.len() + self.groups.len()
    }
}

/// IRRUT400 — RACF database split/merge utility.
///
/// Splits a RACF database into partitions or merges partitions back together.
#[derive(Debug, Default)]
pub struct RacfSplitMergeUtil {
    /// Partitions managed by this utility.
    partitions: Vec<DatabasePartition>,
}

impl RacfSplitMergeUtil {
    /// Create a new split/merge utility instance.
    pub fn new() -> Self {
        Self::default()
    }

    /// Split a list of users and groups into `n` partitions (round-robin).
    pub fn split(
        &mut self,
        users: &[String],
        groups: &[String],
        partition_count: usize,
    ) -> &[DatabasePartition] {
        self.partitions.clear();
        let count = partition_count.max(1);
        for i in 0..count {
            self.partitions
                .push(DatabasePartition::new(&format!("PART{:02}", i + 1)));
        }

        for (i, user) in users.iter().enumerate() {
            self.partitions[i % count]
                .users
                .push(user.clone());
        }
        for (i, group) in groups.iter().enumerate() {
            self.partitions[i % count]
                .groups
                .push(group.clone());
        }

        &self.partitions
    }

    /// Merge all partitions into combined user and group lists.
    pub fn merge(&self) -> (Vec<String>, Vec<String>) {
        let mut users = Vec::new();
        let mut groups = Vec::new();
        for part in &self.partitions {
            users.extend(part.users.clone());
            groups.extend(part.groups.clone());
        }
        (users, groups)
    }

    /// Returns the current partitions.
    pub fn partitions(&self) -> &[DatabasePartition] {
        &self.partitions
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // --- Exit Points ---

    #[test]
    fn exit_point_display() {
        assert_eq!(format!("{}", ExitPoint::PreAuth), "ICHRTX00");
        assert_eq!(format!("{}", ExitPoint::PasswordQuality), "ICHPWX01");
        assert_eq!(format!("{}", ExitPoint::EventNotification), "IRREVX01");
    }

    // --- Exit Registry ---

    #[test]
    fn exit_registry_register_invoke() {
        let mut registry = ExitRegistry::new();
        assert!(registry.is_empty());

        registry
            .register(ExitPoint::PreAuth, |ctx| {
                if ctx.user == "BLOCKED" {
                    ExitAction::Deny
                } else {
                    ExitAction::Allow
                }
            })
            .unwrap();

        assert!(registry.is_registered(ExitPoint::PreAuth));
        assert_eq!(registry.len(), 1);

        let ctx_ok = ExitContext::new("JSMITH", "SYS1.PARMLIB", "");
        assert_eq!(registry.invoke(ExitPoint::PreAuth, &ctx_ok), ExitAction::Allow);

        let ctx_blocked = ExitContext::new("BLOCKED", "SYS1.PARMLIB", "");
        assert_eq!(
            registry.invoke(ExitPoint::PreAuth, &ctx_blocked),
            ExitAction::Deny
        );
    }

    #[test]
    fn exit_registry_unregistered_passes_through() {
        let registry = ExitRegistry::new();
        let ctx = ExitContext::new("ANYONE", "ANYTHING", "");
        assert_eq!(
            registry.invoke(ExitPoint::EventNotification, &ctx),
            ExitAction::PassThrough
        );
    }

    #[test]
    fn exit_registry_duplicate_register_fails() {
        let mut registry = ExitRegistry::new();
        registry.register(ExitPoint::PreAuth, |_| ExitAction::Allow).unwrap();
        assert!(registry
            .register(ExitPoint::PreAuth, |_| ExitAction::Deny)
            .is_err());
    }

    #[test]
    fn exit_registry_deregister() {
        let mut registry = ExitRegistry::new();
        registry.register(ExitPoint::PreAuth, |_| ExitAction::Allow).unwrap();
        assert!(registry.deregister(ExitPoint::PreAuth));
        assert!(!registry.is_registered(ExitPoint::PreAuth));
        assert!(!registry.deregister(ExitPoint::PreAuth)); // already removed
    }

    #[test]
    fn exit_registry_password_quality() {
        let mut registry = ExitRegistry::new();
        registry
            .register(ExitPoint::PasswordQuality, |ctx| {
                if ctx.data.len() >= 8 {
                    ExitAction::Allow
                } else {
                    ExitAction::Deny
                }
            })
            .unwrap();

        let short = ExitContext::new("USER1", "", "abc");
        assert_eq!(
            registry.invoke(ExitPoint::PasswordQuality, &short),
            ExitAction::Deny
        );

        let good = ExitContext::new("USER1", "", "longpassword");
        assert_eq!(
            registry.invoke(ExitPoint::PasswordQuality, &good),
            ExitAction::Allow
        );
    }

    // --- Exit Action ---

    #[test]
    fn exit_action_display() {
        assert_eq!(format!("{}", ExitAction::Allow), "ALLOW");
        assert_eq!(format!("{}", ExitAction::Deny), "DENY");
        assert_eq!(format!("{}", ExitAction::PassThrough), "PASS_THROUGH");
    }

    // --- IRRUT100 Search ---

    #[test]
    fn search_exact_match() {
        let mut util = RacfSearchUtil::new();
        let mut fields = HashMap::new();
        fields.insert("NAME".into(), "JOHN SMITH".into());
        fields.insert("DFLTGRP".into(), "SYS1".into());
        util.add_entry("USER", "JSMITH", fields);

        let results = util.search(&[SearchCriterion::new("DFLTGRP", "SYS1")]);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "JSMITH");
    }

    #[test]
    fn search_wildcard() {
        let mut util = RacfSearchUtil::new();
        let mut f1 = HashMap::new();
        f1.insert("NAME".into(), "JOHN SMITH".into());
        util.add_entry("USER", "JSMITH", f1);

        let mut f2 = HashMap::new();
        f2.insert("NAME".into(), "JANE DOE".into());
        util.add_entry("USER", "JDOE", f2);

        let results = util.search(&[SearchCriterion::new("NAME", "J*")]);
        assert_eq!(results.len(), 2);
    }

    #[test]
    fn search_no_match() {
        let mut util = RacfSearchUtil::new();
        let mut fields = HashMap::new();
        fields.insert("NAME".into(), "JOHN".into());
        util.add_entry("USER", "JSMITH", fields);

        let results = util.search(&[SearchCriterion::new("NAME", "ALICE")]);
        assert!(results.is_empty());
    }

    #[test]
    fn search_multiple_criteria() {
        let mut util = RacfSearchUtil::new();
        let mut f1 = HashMap::new();
        f1.insert("DFLTGRP".into(), "SYS1".into());
        f1.insert("NAME".into(), "JOHN".into());
        util.add_entry("USER", "JSMITH", f1);

        let mut f2 = HashMap::new();
        f2.insert("DFLTGRP".into(), "DEPT01".into());
        f2.insert("NAME".into(), "JANE".into());
        util.add_entry("USER", "JDOE", f2);

        let results = util.search(&[
            SearchCriterion::new("DFLTGRP", "SYS1"),
            SearchCriterion::new("NAME", "JOHN"),
        ]);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "JSMITH");
    }

    // --- IRRUT200 Verify ---

    #[test]
    fn verify_clean_database() {
        let mut util = RacfVerifyUtil::new();
        util.add_user("JSMITH");
        util.add_group("SYS1");
        util.add_connection("JSMITH", "SYS1");
        let issues = util.verify();
        assert!(issues.is_empty());
    }

    #[test]
    fn verify_orphaned_user() {
        let mut util = RacfVerifyUtil::new();
        util.add_group("SYS1");
        util.add_connection("GHOST", "SYS1");
        let issues = util.verify();
        assert_eq!(issues.len(), 1);
        assert!(issues[0].description.contains("GHOST"));
        assert_eq!(issues[0].severity, "ERROR");
    }

    #[test]
    fn verify_orphaned_group() {
        let mut util = RacfVerifyUtil::new();
        util.add_user("JSMITH");
        util.add_connection("JSMITH", "BOGUS");
        let issues = util.verify();
        assert_eq!(issues.len(), 1);
        assert!(issues[0].description.contains("BOGUS"));
    }

    #[test]
    fn verify_duplicate_user_warning() {
        let mut util = RacfVerifyUtil::new();
        util.add_user("JSMITH");
        util.add_user("JSMITH");
        let issues = util.verify();
        assert_eq!(issues.len(), 1);
        assert_eq!(issues[0].severity, "WARNING");
    }

    // --- IRRUT400 Split/Merge ---

    #[test]
    fn split_round_robin() {
        let mut util = RacfSplitMergeUtil::new();
        let users: Vec<String> = vec!["U1", "U2", "U3", "U4"]
            .into_iter()
            .map(String::from)
            .collect();
        let groups: Vec<String> = vec!["G1", "G2"].into_iter().map(String::from).collect();

        let parts = util.split(&users, &groups, 2);
        assert_eq!(parts.len(), 2);
        assert_eq!(parts[0].users, vec!["U1", "U3"]);
        assert_eq!(parts[1].users, vec!["U2", "U4"]);
        assert_eq!(parts[0].groups, vec!["G1"]);
        assert_eq!(parts[1].groups, vec!["G2"]);
    }

    #[test]
    fn merge_combines_all() {
        let mut util = RacfSplitMergeUtil::new();
        let users: Vec<String> = vec!["U1", "U2", "U3"]
            .into_iter()
            .map(String::from)
            .collect();
        let groups: Vec<String> = vec!["G1", "G2"].into_iter().map(String::from).collect();

        util.split(&users, &groups, 2);
        let (merged_users, merged_groups) = util.merge();
        assert_eq!(merged_users.len(), 3);
        assert_eq!(merged_groups.len(), 2);
    }

    #[test]
    fn split_single_partition() {
        let mut util = RacfSplitMergeUtil::new();
        let users: Vec<String> = vec!["U1", "U2"].into_iter().map(String::from).collect();
        let groups: Vec<String> = Vec::new();

        let parts = util.split(&users, &groups, 1);
        assert_eq!(parts.len(), 1);
        assert_eq!(parts[0].users, vec!["U1", "U2"]);
    }

    #[test]
    fn partition_entry_count() {
        let mut p = DatabasePartition::new("TEST");
        assert_eq!(p.entry_count(), 0);
        p.users.push("U1".into());
        p.groups.push("G1".into());
        assert_eq!(p.entry_count(), 2);
    }

    // --- Search Criterion ---

    #[test]
    fn search_criterion_matches_exact() {
        let c = SearchCriterion::new("NAME", "JOHN");
        assert!(c.matches("JOHN"));
        assert!(c.matches("john"));
        assert!(!c.matches("JANE"));
    }

    #[test]
    fn search_criterion_matches_wildcard() {
        let c = SearchCriterion::new("NAME", "J*");
        assert!(c.matches("JOHN"));
        assert!(c.matches("JANE"));
        assert!(!c.matches("ALICE"));
    }

    // --- Exit Registry Debug ---

    #[test]
    fn exit_registry_debug() {
        let registry = ExitRegistry::new();
        let debug = format!("{:?}", registry);
        assert!(debug.contains("ExitRegistry"));
        assert!(debug.contains("handler_count"));
    }
}
