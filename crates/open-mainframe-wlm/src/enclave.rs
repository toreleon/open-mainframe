//! # Enclave Framework
//!
//! Enclaves track work that spans multiple address spaces (e.g., CICS → DB2).
//! CPU time and service units are attributed to the enclave's service class
//! regardless of which address space consumes them.

use std::collections::HashMap;

use crate::goals::ServiceUnits;
use crate::service::Importance;

// ─────────────────────── Enclave ───────────────────────

/// Enclave state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EnclaveState {
    /// Enclave is active.
    Active,
    /// Enclave is complete.
    Complete,
    /// Enclave was abended.
    Abended,
}

/// A participant in an enclave (an address space that joined).
#[derive(Debug, Clone)]
pub struct EnclaveParticipant {
    /// Address space identifier (subsystem name or ASID).
    pub address_space: String,
    /// Subsystem type (e.g., "CICS", "DB2").
    pub subsystem_type: String,
    /// CPU time consumed by this participant (seconds).
    pub cpu_seconds: f64,
    /// Service units consumed by this participant.
    pub service_units: ServiceUnits,
}

/// An enclave — a unit of work spanning multiple address spaces.
#[derive(Debug, Clone)]
pub struct Enclave {
    /// Unique enclave token.
    pub token: u64,
    /// Correlator for propagation across address spaces.
    pub correlator: String,
    /// Service class assigned to this enclave.
    pub service_class: String,
    /// Report class (if any).
    pub report_class: Option<String>,
    /// Importance.
    pub importance: Importance,
    /// Creating subsystem type.
    pub creating_subsystem: String,
    /// Current state.
    pub state: EnclaveState,
    /// Participants.
    pub participants: Vec<EnclaveParticipant>,
    /// Creation timestamp.
    pub created_at: f64,
    /// Completion timestamp (if complete).
    pub completed_at: Option<f64>,
}

impl Enclave {
    /// Total CPU seconds across all participants.
    pub fn total_cpu_seconds(&self) -> f64 {
        self.participants.iter().map(|p| p.cpu_seconds).sum()
    }

    /// Total service units across all participants.
    pub fn total_service_units(&self) -> ServiceUnits {
        let mut total = ServiceUnits::default();
        for p in &self.participants {
            total.add(&p.service_units);
        }
        total
    }

    /// Number of address spaces participating.
    pub fn participant_count(&self) -> usize {
        self.participants.len()
    }

    /// Find a participant by address space.
    pub fn find_participant(&self, address_space: &str) -> Option<&EnclaveParticipant> {
        self.participants
            .iter()
            .find(|p| p.address_space == address_space)
    }
}

// ─────────────────────── Enclave Manager ───────────────────────

/// Manages enclave lifecycle.
#[derive(Debug)]
pub struct EnclaveManager {
    /// Active enclaves by token.
    enclaves: HashMap<u64, Enclave>,
    /// Correlator → token index.
    correlator_index: HashMap<String, u64>,
    /// Next token.
    next_token: u64,
}

impl EnclaveManager {
    /// Create a new enclave manager.
    pub fn new() -> Self {
        Self {
            enclaves: HashMap::new(),
            correlator_index: HashMap::new(),
            next_token: 1,
        }
    }

    /// Create an enclave.
    pub fn create(
        &mut self,
        creating_subsystem: &str,
        address_space: &str,
        service_class: &str,
        importance: Importance,
        timestamp: f64,
    ) -> u64 {
        let token = self.next_token;
        self.next_token += 1;

        let correlator = format!("ENC{token:08X}");

        let participant = EnclaveParticipant {
            address_space: address_space.to_string(),
            subsystem_type: creating_subsystem.to_string(),
            cpu_seconds: 0.0,
            service_units: ServiceUnits::default(),
        };

        let enclave = Enclave {
            token,
            correlator: correlator.clone(),
            service_class: service_class.to_uppercase(),
            report_class: None,
            importance,
            creating_subsystem: creating_subsystem.to_string(),
            state: EnclaveState::Active,
            participants: vec![participant],
            created_at: timestamp,
            completed_at: None,
        };

        self.correlator_index.insert(correlator, token);
        self.enclaves.insert(token, enclave);
        token
    }

    /// Join an existing enclave by correlator.
    pub fn join(
        &mut self,
        correlator: &str,
        address_space: &str,
        subsystem_type: &str,
    ) -> Option<u64> {
        let token = *self.correlator_index.get(correlator)?;
        let enclave = self.enclaves.get_mut(&token)?;

        if enclave.state != EnclaveState::Active {
            return None;
        }

        // Don't add duplicate participants.
        if enclave
            .participants
            .iter()
            .any(|p| p.address_space == address_space)
        {
            return Some(token);
        }

        enclave.participants.push(EnclaveParticipant {
            address_space: address_space.to_string(),
            subsystem_type: subsystem_type.to_string(),
            cpu_seconds: 0.0,
            service_units: ServiceUnits::default(),
        });

        Some(token)
    }

    /// Record CPU time for a participant.
    pub fn record_cpu(
        &mut self,
        token: u64,
        address_space: &str,
        cpu_seconds: f64,
        service_units: &ServiceUnits,
    ) {
        if let Some(enclave) = self.enclaves.get_mut(&token) {
            if let Some(participant) = enclave
                .participants
                .iter_mut()
                .find(|p| p.address_space == address_space)
            {
                participant.cpu_seconds += cpu_seconds;
                participant.service_units.add(service_units);
            }
        }
    }

    /// Complete an enclave.
    pub fn complete(&mut self, token: u64, timestamp: f64) {
        if let Some(enclave) = self.enclaves.get_mut(&token) {
            enclave.state = EnclaveState::Complete;
            enclave.completed_at = Some(timestamp);
        }
    }

    /// Get an enclave by token.
    pub fn get(&self, token: u64) -> Option<&Enclave> {
        self.enclaves.get(&token)
    }

    /// Get an enclave by correlator.
    pub fn get_by_correlator(&self, correlator: &str) -> Option<&Enclave> {
        let token = self.correlator_index.get(correlator)?;
        self.enclaves.get(token)
    }

    /// Count active enclaves.
    pub fn active_count(&self) -> usize {
        self.enclaves
            .values()
            .filter(|e| e.state == EnclaveState::Active)
            .count()
    }

    /// Count active enclaves by creating subsystem type.
    pub fn active_by_subsystem(&self) -> HashMap<String, usize> {
        let mut counts = HashMap::new();
        for e in self.enclaves.values() {
            if e.state == EnclaveState::Active {
                *counts.entry(e.creating_subsystem.clone()).or_insert(0) += 1;
            }
        }
        counts
    }
}

impl Default for EnclaveManager {
    fn default() -> Self {
        Self::new()
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ─── WLM-105.1: Enclave Lifecycle ───

    #[test]
    fn test_create_enclave() {
        let mut mgr = EnclaveManager::new();
        let token = mgr.create("CICS", "CICSPROD", "CICSHIGH", Importance(1), 100.0);

        let enc = mgr.get(token).unwrap();
        assert_eq!(enc.state, EnclaveState::Active);
        assert_eq!(enc.creating_subsystem, "CICS");
        assert_eq!(enc.service_class, "CICSHIGH");
        assert_eq!(enc.participant_count(), 1);
    }

    #[test]
    fn test_complete_enclave() {
        let mut mgr = EnclaveManager::new();
        let token = mgr.create("CICS", "CICSPROD", "CICSHIGH", Importance(1), 100.0);
        mgr.complete(token, 200.0);

        let enc = mgr.get(token).unwrap();
        assert_eq!(enc.state, EnclaveState::Complete);
        assert_eq!(enc.completed_at, Some(200.0));
    }

    #[test]
    fn test_join_enclave() {
        let mut mgr = EnclaveManager::new();
        let token = mgr.create("CICS", "CICSPROD", "CICSHIGH", Importance(1), 100.0);

        let correlator = mgr.get(token).unwrap().correlator.clone();
        let joined = mgr.join(&correlator, "DB2PROD", "DB2").unwrap();
        assert_eq!(joined, token);

        let enc = mgr.get(token).unwrap();
        assert_eq!(enc.participant_count(), 2);
    }

    // ─── WLM-105.2: CPU Tracking Across Address Spaces ───

    #[test]
    fn test_cpu_tracking() {
        let mut mgr = EnclaveManager::new();
        let token = mgr.create("CICS", "CICSPROD", "CICSHIGH", Importance(1), 100.0);

        let correlator = mgr.get(token).unwrap().correlator.clone();
        mgr.join(&correlator, "DB2PROD", "DB2");

        // Record CPU for CICS.
        mgr.record_cpu(token, "CICSPROD", 0.5, &ServiceUnits::new(100, 50, 30, 20));
        // Record CPU for DB2.
        mgr.record_cpu(token, "DB2PROD", 0.3, &ServiceUnits::new(80, 40, 20, 10));

        let enc = mgr.get(token).unwrap();
        assert!((enc.total_cpu_seconds() - 0.8).abs() < 0.001);

        let total_su = enc.total_service_units();
        assert_eq!(total_su.cpu, 180);
        assert_eq!(total_su.total(), 350);
    }

    #[test]
    fn test_participant_cpu_attribution() {
        let mut mgr = EnclaveManager::new();
        let token = mgr.create("CICS", "CICSPROD", "CICSHIGH", Importance(1), 100.0);

        let correlator = mgr.get(token).unwrap().correlator.clone();
        mgr.join(&correlator, "DB2PROD", "DB2");

        mgr.record_cpu(token, "CICSPROD", 1.0, &ServiceUnits::new(200, 0, 0, 0));
        mgr.record_cpu(token, "DB2PROD", 0.5, &ServiceUnits::new(100, 0, 0, 0));

        let enc = mgr.get(token).unwrap();
        let cics = enc.find_participant("CICSPROD").unwrap();
        assert!((cics.cpu_seconds - 1.0).abs() < 0.001);
        let db2 = enc.find_participant("DB2PROD").unwrap();
        assert!((db2.cpu_seconds - 0.5).abs() < 0.001);
    }

    // ─── WLM-105.3: Correlator Propagation ───

    #[test]
    fn test_correlator_lookup() {
        let mut mgr = EnclaveManager::new();
        let token = mgr.create("CICS", "CICSPROD", "CICSHIGH", Importance(1), 100.0);

        let correlator = mgr.get(token).unwrap().correlator.clone();
        let found = mgr.get_by_correlator(&correlator).unwrap();
        assert_eq!(found.token, token);
    }

    #[test]
    fn test_correlator_join_same_enclave() {
        let mut mgr = EnclaveManager::new();
        let token = mgr.create("CICS", "CICSPROD", "HIGH", Importance(1), 100.0);

        let correlator = mgr.get(token).unwrap().correlator.clone();

        // DB2 joins using the correlator.
        let joined = mgr.join(&correlator, "DB2PROD", "DB2").unwrap();
        assert_eq!(joined, token);

        // MQ also joins.
        let joined2 = mgr.join(&correlator, "MQPROD", "MQ").unwrap();
        assert_eq!(joined2, token);

        let enc = mgr.get(token).unwrap();
        assert_eq!(enc.participant_count(), 3);
    }

    // ─── WLM-105.4: Enclave Classification ───

    #[test]
    fn test_enclave_uses_creator_classification() {
        let mut mgr = EnclaveManager::new();

        // CICS creates enclave with its classification result.
        let token = mgr.create("CICS", "CICSPROD", "CICSHIGH", Importance(1), 100.0);

        // DB2 joins — should use the CICS-assigned class.
        let correlator = mgr.get(token).unwrap().correlator.clone();
        mgr.join(&correlator, "DB2PROD", "DB2");

        let enc = mgr.get(token).unwrap();
        // All participants share the creator's service class.
        assert_eq!(enc.service_class, "CICSHIGH");
        assert_eq!(enc.creating_subsystem, "CICS");
    }

    // ─── WLM-105.5: Enclave Tests ───

    #[test]
    fn test_active_count() {
        let mut mgr = EnclaveManager::new();
        let t1 = mgr.create("CICS", "C1", "HIGH", Importance(1), 100.0);
        mgr.create("CICS", "C2", "HIGH", Importance(1), 100.0);
        mgr.create("IMS", "I1", "MED", Importance(2), 100.0);

        assert_eq!(mgr.active_count(), 3);

        mgr.complete(t1, 200.0);
        assert_eq!(mgr.active_count(), 2);
    }

    #[test]
    fn test_active_by_subsystem() {
        let mut mgr = EnclaveManager::new();
        mgr.create("CICS", "C1", "HIGH", Importance(1), 100.0);
        mgr.create("CICS", "C2", "HIGH", Importance(1), 100.0);
        mgr.create("IMS", "I1", "MED", Importance(2), 100.0);

        let counts = mgr.active_by_subsystem();
        assert_eq!(*counts.get("CICS").unwrap(), 2);
        assert_eq!(*counts.get("IMS").unwrap(), 1);
    }

    #[test]
    fn test_cannot_join_completed_enclave() {
        let mut mgr = EnclaveManager::new();
        let token = mgr.create("CICS", "C1", "HIGH", Importance(1), 100.0);
        let correlator = mgr.get(token).unwrap().correlator.clone();

        mgr.complete(token, 200.0);
        let result = mgr.join(&correlator, "DB2", "DB2");
        assert!(result.is_none());
    }

    #[test]
    fn test_duplicate_join_no_extra_participant() {
        let mut mgr = EnclaveManager::new();
        let token = mgr.create("CICS", "C1", "HIGH", Importance(1), 100.0);
        let correlator = mgr.get(token).unwrap().correlator.clone();

        mgr.join(&correlator, "DB2", "DB2");
        mgr.join(&correlator, "DB2", "DB2"); // Duplicate.

        let enc = mgr.get(token).unwrap();
        assert_eq!(enc.participant_count(), 2); // Not 3.
    }
}
