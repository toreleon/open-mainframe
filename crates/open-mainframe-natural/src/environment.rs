// SPDX-License-Identifier: Apache-2.0
//! NAT-110: Environment & Security for Natural.
//!
//! Provides library management (LOGON, CATALOG, STOW), directory-based
//! object storage (`NaturalLibrary`), profile-based access control
//! (`NaturalSecurity`), and simulated EntireX RPC support.

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Natural library
// ---------------------------------------------------------------------------

/// A stored Natural object within a library.
#[derive(Debug, Clone)]
pub struct StoredObject {
    pub name: String,
    pub object_type: StoredObjectType,
    pub source: Option<String>,
    pub cataloged: bool,
    pub timestamp: u64,
}

/// Type of stored object.
#[derive(Debug, Clone, PartialEq)]
pub enum StoredObjectType {
    Program,
    Subprogram,
    Subroutine,
    Helproutine,
    Map,
    Gda,
    Lda,
    Pda,
    Copycode,
    Text,
}

impl StoredObject {
    pub fn new(name: &str, object_type: StoredObjectType) -> Self {
        Self {
            name: name.to_string(),
            object_type,
            source: None,
            cataloged: false,
            timestamp: 0,
        }
    }

    pub fn with_source(mut self, source: &str) -> Self {
        self.source = Some(source.to_string());
        self
    }
}

/// Directory-based Natural library for object storage.
#[derive(Debug, Clone)]
pub struct NaturalLibrary {
    pub name: String,
    objects: HashMap<String, StoredObject>,
    pub steplib: Option<String>,
}

impl NaturalLibrary {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            objects: HashMap::new(),
            steplib: None,
        }
    }

    /// STOW: save source to library.
    pub fn stow(&mut self, name: &str, source: &str, object_type: StoredObjectType) {
        let obj = StoredObject {
            name: name.to_string(),
            object_type,
            source: Some(source.to_string()),
            cataloged: false,
            timestamp: current_timestamp(),
        };
        self.objects.insert(name.to_string(), obj);
    }

    /// CATALOG: compile and save a Natural object.
    pub fn catalog(&mut self, name: &str, source: &str, object_type: StoredObjectType) -> Result<(), EnvironmentError> {
        // In a real system, this would compile the source
        // Here we simulate success if the source is non-empty
        if source.is_empty() {
            return Err(EnvironmentError::CatalogFailed(name.to_string(), "empty source".into()));
        }
        let obj = StoredObject {
            name: name.to_string(),
            object_type,
            source: Some(source.to_string()),
            cataloged: true,
            timestamp: current_timestamp(),
        };
        self.objects.insert(name.to_string(), obj);
        Ok(())
    }

    /// Retrieve an object by name.
    pub fn get(&self, name: &str) -> Option<&StoredObject> {
        self.objects.get(name)
    }

    /// List all objects in the library.
    pub fn list(&self) -> Vec<&StoredObject> {
        self.objects.values().collect()
    }

    /// List objects by type.
    pub fn list_by_type(&self, obj_type: StoredObjectType) -> Vec<&StoredObject> {
        self.objects.values().filter(|o| o.object_type == obj_type).collect()
    }

    /// Delete an object.
    pub fn delete(&mut self, name: &str) -> Result<(), EnvironmentError> {
        self.objects.remove(name)
            .map(|_| ())
            .ok_or(EnvironmentError::ObjectNotFound(name.to_string()))
    }

    /// Check if an object exists.
    pub fn exists(&self, name: &str) -> bool {
        self.objects.contains_key(name)
    }

    /// Set steplib (library chain).
    pub fn set_steplib(&mut self, steplib: &str) {
        self.steplib = Some(steplib.to_string());
    }
}

// ---------------------------------------------------------------------------
// Library manager
// ---------------------------------------------------------------------------

/// Manages multiple libraries and the current session context.
#[derive(Debug, Clone)]
pub struct LibraryManager {
    libraries: HashMap<String, NaturalLibrary>,
    pub current_library: String,
}

impl LibraryManager {
    pub fn new() -> Self {
        let mut mgr = Self {
            libraries: HashMap::new(),
            current_library: "SYSTEM".to_string(),
        };
        mgr.libraries.insert("SYSTEM".to_string(), NaturalLibrary::new("SYSTEM"));
        mgr
    }

    /// LOGON: set the current library context.
    pub fn logon(&mut self, library: &str) -> Result<(), EnvironmentError> {
        if !self.libraries.contains_key(library) {
            // Auto-create library if it doesn't exist
            self.libraries.insert(library.to_string(), NaturalLibrary::new(library));
        }
        self.current_library = library.to_string();
        Ok(())
    }

    /// Get the current library.
    pub fn current(&self) -> &NaturalLibrary {
        self.libraries.get(&self.current_library).unwrap()
    }

    /// Get a mutable reference to the current library.
    pub fn current_mut(&mut self) -> &mut NaturalLibrary {
        let name = self.current_library.clone();
        self.libraries.get_mut(&name).unwrap()
    }

    /// Get a library by name.
    pub fn get_library(&self, name: &str) -> Option<&NaturalLibrary> {
        self.libraries.get(name)
    }

    /// Create a new library.
    pub fn create_library(&mut self, name: &str) -> Result<(), EnvironmentError> {
        if self.libraries.contains_key(name) {
            return Err(EnvironmentError::LibraryAlreadyExists(name.to_string()));
        }
        self.libraries.insert(name.to_string(), NaturalLibrary::new(name));
        Ok(())
    }

    /// Delete a library.
    pub fn delete_library(&mut self, name: &str) -> Result<(), EnvironmentError> {
        if name == "SYSTEM" {
            return Err(EnvironmentError::CannotDeleteSystem);
        }
        self.libraries.remove(name)
            .map(|_| ())
            .ok_or(EnvironmentError::LibraryNotFound(name.to_string()))
    }

    /// List all library names.
    pub fn list_libraries(&self) -> Vec<String> {
        self.libraries.keys().cloned().collect()
    }
}

impl Default for LibraryManager {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Natural Security
// ---------------------------------------------------------------------------

/// Access level for security profiles.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum AccessLevel {
    None,
    Read,
    Execute,
    Update,
    Full,
}

/// Security profile for a library or program.
#[derive(Debug, Clone)]
pub struct SecurityProfile {
    pub resource: String,
    pub resource_type: SecurityResourceType,
    pub allowed_users: Vec<String>,
    pub access_level: AccessLevel,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SecurityResourceType {
    Library,
    Program,
    Subprogram,
    Map,
    File,
}

impl SecurityProfile {
    pub fn new(resource: &str, resource_type: SecurityResourceType, access_level: AccessLevel) -> Self {
        Self {
            resource: resource.to_string(),
            resource_type,
            allowed_users: Vec::new(),
            access_level,
        }
    }

    pub fn add_user(&mut self, user: &str) {
        if !self.allowed_users.contains(&user.to_string()) {
            self.allowed_users.push(user.to_string());
        }
    }

    pub fn is_allowed(&self, user: &str) -> bool {
        self.allowed_users.is_empty() || self.allowed_users.iter().any(|u| u == user || u == "*")
    }
}

/// Natural Security service.
#[derive(Debug, Clone, Default)]
pub struct NaturalSecurity {
    profiles: Vec<SecurityProfile>,
    pub active: bool,
}

impl NaturalSecurity {
    pub fn new() -> Self {
        Self { profiles: Vec::new(), active: false }
    }

    /// Activate security checking.
    pub fn activate(&mut self) {
        self.active = true;
    }

    /// Add a security profile.
    pub fn add_profile(&mut self, profile: SecurityProfile) {
        self.profiles.push(profile);
    }

    /// Check if a user has access to a resource.
    pub fn check_access(
        &self,
        user: &str,
        resource: &str,
        resource_type: SecurityResourceType,
        required_level: AccessLevel,
    ) -> bool {
        if !self.active {
            return true; // Security not active — allow all
        }
        for profile in &self.profiles {
            if profile.resource == resource && profile.resource_type == resource_type {
                return profile.is_allowed(user) && profile.access_level >= required_level;
            }
        }
        // No matching profile — allow by default
        true
    }

    /// Get profiles for a resource.
    pub fn get_profiles(&self, resource: &str) -> Vec<&SecurityProfile> {
        self.profiles.iter().filter(|p| p.resource == resource).collect()
    }
}

// ---------------------------------------------------------------------------
// EntireX RPC (simulated)
// ---------------------------------------------------------------------------

/// Simulated EntireX RPC call result.
#[derive(Debug, Clone)]
pub struct RpcResult {
    pub return_code: i32,
    pub output_values: Vec<String>,
    pub error_message: Option<String>,
}

/// Simulate a CALLNAT ... REMOTELY invocation.
pub fn callnat_remotely(
    _server: &str,
    _subprogram: &str,
    parameters: &[String],
) -> RpcResult {
    // Simulated — always return success with echo
    RpcResult {
        return_code: 0,
        output_values: parameters.to_vec(),
        error_message: None,
    }
}

/// EntireX broker connection (simulated).
#[derive(Debug, Clone)]
pub struct EntireXBroker {
    pub server_name: String,
    pub server_class: String,
    pub service: String,
    pub connected: bool,
}

impl EntireXBroker {
    pub fn new(server: &str, class: &str, service: &str) -> Self {
        Self {
            server_name: server.to_string(),
            server_class: class.to_string(),
            service: service.to_string(),
            connected: false,
        }
    }

    pub fn connect(&mut self) -> Result<(), EnvironmentError> {
        self.connected = true;
        Ok(())
    }

    pub fn disconnect(&mut self) {
        self.connected = false;
    }

    pub fn call(&self, subprogram: &str, params: &[String]) -> Result<RpcResult, EnvironmentError> {
        if !self.connected {
            return Err(EnvironmentError::NotConnected);
        }
        Ok(callnat_remotely(&self.server_name, subprogram, params))
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn current_timestamp() -> u64 {
    // Simulated timestamp
    20260223120000
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, thiserror::Error)]
pub enum EnvironmentError {
    #[error("library not found: {0}")]
    LibraryNotFound(String),
    #[error("library already exists: {0}")]
    LibraryAlreadyExists(String),
    #[error("object not found: {0}")]
    ObjectNotFound(String),
    #[error("cannot delete SYSTEM library")]
    CannotDeleteSystem,
    #[error("catalog failed for {0}: {1}")]
    CatalogFailed(String, String),
    #[error("access denied for user {user} to {resource}")]
    AccessDenied { user: String, resource: String },
    #[error("not connected to EntireX broker")]
    NotConnected,
    #[error("RPC error: {0}")]
    RpcError(String),
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_library_stow() {
        let mut lib = NaturalLibrary::new("TESTLIB");
        lib.stow("PROG1", "DISPLAY 'hello'", StoredObjectType::Program);
        assert!(lib.exists("PROG1"));
        let obj = lib.get("PROG1").unwrap();
        assert!(!obj.cataloged);
        assert_eq!(obj.source.as_deref(), Some("DISPLAY 'hello'"));
    }

    #[test]
    fn test_library_catalog() {
        let mut lib = NaturalLibrary::new("TESTLIB");
        lib.catalog("PROG1", "DISPLAY 'hello'", StoredObjectType::Program).unwrap();
        let obj = lib.get("PROG1").unwrap();
        assert!(obj.cataloged);
    }

    #[test]
    fn test_library_catalog_empty_source() {
        let mut lib = NaturalLibrary::new("TESTLIB");
        assert!(lib.catalog("PROG1", "", StoredObjectType::Program).is_err());
    }

    #[test]
    fn test_library_list() {
        let mut lib = NaturalLibrary::new("TESTLIB");
        lib.stow("P1", "src1", StoredObjectType::Program);
        lib.stow("S1", "src2", StoredObjectType::Subprogram);
        assert_eq!(lib.list().len(), 2);
    }

    #[test]
    fn test_library_list_by_type() {
        let mut lib = NaturalLibrary::new("TESTLIB");
        lib.stow("P1", "src1", StoredObjectType::Program);
        lib.stow("P2", "src2", StoredObjectType::Program);
        lib.stow("S1", "src3", StoredObjectType::Subprogram);
        assert_eq!(lib.list_by_type(StoredObjectType::Program).len(), 2);
        assert_eq!(lib.list_by_type(StoredObjectType::Subprogram).len(), 1);
    }

    #[test]
    fn test_library_delete() {
        let mut lib = NaturalLibrary::new("TESTLIB");
        lib.stow("P1", "src", StoredObjectType::Program);
        lib.delete("P1").unwrap();
        assert!(!lib.exists("P1"));
    }

    #[test]
    fn test_library_delete_not_found() {
        let mut lib = NaturalLibrary::new("TESTLIB");
        assert!(lib.delete("NONEXIST").is_err());
    }

    #[test]
    fn test_library_steplib() {
        let mut lib = NaturalLibrary::new("TESTLIB");
        lib.set_steplib("SYSLIB");
        assert_eq!(lib.steplib.as_deref(), Some("SYSLIB"));
    }

    #[test]
    fn test_library_manager_logon() {
        let mut mgr = LibraryManager::new();
        mgr.logon("DEVLIB").unwrap();
        assert_eq!(mgr.current_library, "DEVLIB");
    }

    #[test]
    fn test_library_manager_current() {
        let mut mgr = LibraryManager::new();
        mgr.current_mut().stow("P1", "src", StoredObjectType::Program);
        assert!(mgr.current().exists("P1"));
    }

    #[test]
    fn test_library_manager_create() {
        let mut mgr = LibraryManager::new();
        mgr.create_library("NEWLIB").unwrap();
        assert!(mgr.get_library("NEWLIB").is_some());
    }

    #[test]
    fn test_library_manager_create_duplicate() {
        let mut mgr = LibraryManager::new();
        mgr.create_library("NEWLIB").unwrap();
        assert!(mgr.create_library("NEWLIB").is_err());
    }

    #[test]
    fn test_library_manager_delete() {
        let mut mgr = LibraryManager::new();
        mgr.create_library("TEMPLIB").unwrap();
        mgr.delete_library("TEMPLIB").unwrap();
        assert!(mgr.get_library("TEMPLIB").is_none());
    }

    #[test]
    fn test_library_manager_delete_system() {
        let mut mgr = LibraryManager::new();
        assert!(mgr.delete_library("SYSTEM").is_err());
    }

    #[test]
    fn test_library_manager_list() {
        let mut mgr = LibraryManager::new();
        mgr.create_library("LIB1").unwrap();
        mgr.create_library("LIB2").unwrap();
        assert!(mgr.list_libraries().len() >= 3); // SYSTEM + LIB1 + LIB2
    }

    #[test]
    fn test_security_inactive() {
        let sec = NaturalSecurity::new();
        assert!(sec.check_access("JOHN", "MYLIB", SecurityResourceType::Library, AccessLevel::Full));
    }

    #[test]
    fn test_security_active_allowed() {
        let mut sec = NaturalSecurity::new();
        sec.activate();
        let mut profile = SecurityProfile::new("MYLIB", SecurityResourceType::Library, AccessLevel::Full);
        profile.add_user("JOHN");
        sec.add_profile(profile);
        assert!(sec.check_access("JOHN", "MYLIB", SecurityResourceType::Library, AccessLevel::Read));
    }

    #[test]
    fn test_security_active_denied() {
        let mut sec = NaturalSecurity::new();
        sec.activate();
        let mut profile = SecurityProfile::new("SECLIB", SecurityResourceType::Library, AccessLevel::Read);
        profile.add_user("ADMIN");
        sec.add_profile(profile);
        assert!(!sec.check_access("JOHN", "SECLIB", SecurityResourceType::Library, AccessLevel::Read));
    }

    #[test]
    fn test_security_level_insufficient() {
        let mut sec = NaturalSecurity::new();
        sec.activate();
        let mut profile = SecurityProfile::new("LIB1", SecurityResourceType::Library, AccessLevel::Read);
        profile.add_user("USER1");
        sec.add_profile(profile);
        // User has Read, requests Full — denied
        assert!(!sec.check_access("USER1", "LIB1", SecurityResourceType::Library, AccessLevel::Full));
    }

    #[test]
    fn test_security_wildcard_user() {
        let mut sec = NaturalSecurity::new();
        sec.activate();
        let mut profile = SecurityProfile::new("PUBLIB", SecurityResourceType::Library, AccessLevel::Execute);
        profile.add_user("*");
        sec.add_profile(profile);
        assert!(sec.check_access("ANYONE", "PUBLIB", SecurityResourceType::Library, AccessLevel::Execute));
    }

    #[test]
    fn test_security_no_profile() {
        let mut sec = NaturalSecurity::new();
        sec.activate();
        // No profile defined — allow by default
        assert!(sec.check_access("JOHN", "UNKNOWN", SecurityResourceType::Library, AccessLevel::Full));
    }

    #[test]
    fn test_security_get_profiles() {
        let mut sec = NaturalSecurity::new();
        sec.add_profile(SecurityProfile::new("LIB1", SecurityResourceType::Library, AccessLevel::Read));
        sec.add_profile(SecurityProfile::new("LIB1", SecurityResourceType::Program, AccessLevel::Execute));
        assert_eq!(sec.get_profiles("LIB1").len(), 2);
    }

    #[test]
    fn test_callnat_remotely() {
        let result = callnat_remotely("SERVER1", "CALC-TAX", &["100".into(), "10".into()]);
        assert_eq!(result.return_code, 0);
        assert_eq!(result.output_values.len(), 2);
        assert!(result.error_message.is_none());
    }

    #[test]
    fn test_entirex_broker_connect() {
        let mut broker = EntireXBroker::new("SERVER1", "RPC", "NATSERV");
        assert!(!broker.connected);
        broker.connect().unwrap();
        assert!(broker.connected);
    }

    #[test]
    fn test_entirex_broker_call() {
        let mut broker = EntireXBroker::new("SERVER1", "RPC", "NATSERV");
        broker.connect().unwrap();
        let result = broker.call("CALC", &["10".into()]).unwrap();
        assert_eq!(result.return_code, 0);
    }

    #[test]
    fn test_entirex_broker_not_connected() {
        let broker = EntireXBroker::new("SERVER1", "RPC", "NATSERV");
        assert!(broker.call("CALC", &[]).is_err());
    }

    #[test]
    fn test_entirex_broker_disconnect() {
        let mut broker = EntireXBroker::new("SERVER1", "RPC", "NATSERV");
        broker.connect().unwrap();
        broker.disconnect();
        assert!(!broker.connected);
    }

    #[test]
    fn test_stored_object_with_source() {
        let obj = StoredObject::new("TEST", StoredObjectType::Program)
            .with_source("DISPLAY 'test'");
        assert_eq!(obj.source.as_deref(), Some("DISPLAY 'test'"));
    }
}
