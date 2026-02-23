//! Sysplex Networking — VIPA/Distributor (NET-108).
//!
//! Provides Static VIPA, Dynamic VIPA with failover,
//! Sysplex Distributor for TCP connection routing,
//! and WLM-based routing decisions.

use std::collections::HashMap;
use std::net::IpAddr;

use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum SysplexNetError {
    #[error("VIPA {0} already defined")]
    VipaDuplicate(IpAddr),
    #[error("VIPA {0} not found")]
    VipaNotFound(IpAddr),
    #[error("target stack '{0}' not found")]
    TargetStackNotFound(String),
    #[error("no healthy targets for port {0}")]
    NoHealthyTargets(u16),
    #[error("distributor not configured for port {0}")]
    DistributorNotConfigured(u16),
}

// ---------------------------------------------------------------------------
// NET-108.1 — Static VIPA
// ---------------------------------------------------------------------------

/// State of a VIPA address.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VipaState {
    Active,
    Backup,
    Failed,
}

/// A static VIPA (Virtual IP Address) definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StaticVipa {
    pub address: IpAddr,
    pub stack_name: String,
    pub state: VipaState,
}

// ---------------------------------------------------------------------------
// NET-108.2 — Dynamic VIPA
// ---------------------------------------------------------------------------

/// A Dynamic VIPA that can move between stacks on failure.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DynamicVipa {
    pub address: IpAddr,
    pub owning_stack: String,
    pub backup_stack: Option<String>,
    pub state: VipaState,
}

impl DynamicVipa {
    /// Simulate failover to the backup stack.
    pub fn failover(&mut self) -> Result<(), SysplexNetError> {
        match &self.backup_stack {
            Some(backup) => {
                self.owning_stack = backup.clone();
                self.state = VipaState::Active;
                Ok(())
            }
            None => {
                self.state = VipaState::Failed;
                Err(SysplexNetError::TargetStackNotFound(
                    "no backup configured".to_string(),
                ))
            }
        }
    }
}

// ---------------------------------------------------------------------------
// NET-108.3 — Sysplex Distributor
// ---------------------------------------------------------------------------

/// A target server instance for the Sysplex Distributor.
#[derive(Debug, Clone, PartialEq)]
pub struct DistributorTarget {
    pub stack_name: String,
    pub address: IpAddr,
    pub port: u16,
    pub healthy: bool,
    /// WLM weight (higher = more capacity).
    pub wlm_weight: u32,
    /// Current connection count.
    pub connections: u32,
}

/// Distributor configuration for a port.
#[derive(Debug)]
pub struct DistributorPort {
    pub port: u16,
    pub targets: Vec<DistributorTarget>,
}

/// The Sysplex Distributor routes connections to target stacks.
#[derive(Debug)]
pub struct SysplexDistributor {
    ports: HashMap<u16, DistributorPort>,
}

impl Default for SysplexDistributor {
    fn default() -> Self {
        Self::new()
    }
}

impl SysplexDistributor {
    pub fn new() -> Self {
        Self {
            ports: HashMap::new(),
        }
    }

    /// Configure distribution for a port.
    pub fn configure_port(&mut self, port: u16, targets: Vec<DistributorTarget>) {
        self.ports.insert(port, DistributorPort { port, targets });
    }

    /// Route a connection to the best target for a port.
    pub fn route_connection(&mut self, port: u16) -> Result<&DistributorTarget, SysplexNetError> {
        let dp = self
            .ports
            .get_mut(&port)
            .ok_or(SysplexNetError::DistributorNotConfigured(port))?;

        // Find the best healthy target using WLM weight
        let best_idx = dp
            .targets
            .iter()
            .enumerate()
            .filter(|(_, t)| t.healthy)
            .max_by_key(|(_, t)| {
                // Weighted score: higher weight, fewer connections = better
                if t.connections == 0 {
                    t.wlm_weight * 1000
                } else {
                    t.wlm_weight * 1000 / t.connections
                }
            })
            .map(|(i, _)| i)
            .ok_or(SysplexNetError::NoHealthyTargets(port))?;

        dp.targets[best_idx].connections += 1;
        Ok(&dp.targets[best_idx])
    }

    /// Get targets for a port.
    pub fn targets(&self, port: u16) -> Option<&[DistributorTarget]> {
        self.ports.get(&port).map(|dp| dp.targets.as_slice())
    }
}

// ---------------------------------------------------------------------------
// NET-108.4 — WLM-Based Routing
// (Integrated into SysplexDistributor via wlm_weight)
// ---------------------------------------------------------------------------

/// VIPA manager handling both static and dynamic VIPAs.
#[derive(Debug)]
pub struct VipaManager {
    static_vipas: Vec<StaticVipa>,
    dynamic_vipas: Vec<DynamicVipa>,
    pub distributor: SysplexDistributor,
}

impl Default for VipaManager {
    fn default() -> Self {
        Self::new()
    }
}

impl VipaManager {
    pub fn new() -> Self {
        Self {
            static_vipas: Vec::new(),
            dynamic_vipas: Vec::new(),
            distributor: SysplexDistributor::new(),
        }
    }

    /// Define a static VIPA.
    pub fn define_static_vipa(
        &mut self,
        address: IpAddr,
        stack_name: impl Into<String>,
    ) -> Result<(), SysplexNetError> {
        if self.static_vipas.iter().any(|v| v.address == address) {
            return Err(SysplexNetError::VipaDuplicate(address));
        }
        self.static_vipas.push(StaticVipa {
            address,
            stack_name: stack_name.into(),
            state: VipaState::Active,
        });
        Ok(())
    }

    /// Define a dynamic VIPA.
    pub fn define_dynamic_vipa(
        &mut self,
        address: IpAddr,
        owning_stack: impl Into<String>,
        backup_stack: Option<String>,
    ) -> Result<(), SysplexNetError> {
        if self.dynamic_vipas.iter().any(|v| v.address == address) {
            return Err(SysplexNetError::VipaDuplicate(address));
        }
        self.dynamic_vipas.push(DynamicVipa {
            address,
            owning_stack: owning_stack.into(),
            backup_stack,
            state: VipaState::Active,
        });
        Ok(())
    }

    /// Get a static VIPA by address.
    pub fn static_vipa(&self, address: IpAddr) -> Option<&StaticVipa> {
        self.static_vipas.iter().find(|v| v.address == address)
    }

    /// Get a dynamic VIPA by address (mutable for failover).
    pub fn dynamic_vipa_mut(&mut self, address: IpAddr) -> Option<&mut DynamicVipa> {
        self.dynamic_vipas.iter_mut().find(|v| v.address == address)
    }

    /// Trigger failover for a dynamic VIPA.
    pub fn failover(&mut self, address: IpAddr) -> Result<(), SysplexNetError> {
        let vipa = self
            .dynamic_vipas
            .iter_mut()
            .find(|v| v.address == address)
            .ok_or(SysplexNetError::VipaNotFound(address))?;
        vipa.failover()
    }

    /// Total number of VIPAs.
    pub fn vipa_count(&self) -> usize {
        self.static_vipas.len() + self.dynamic_vipas.len()
    }
}

// ---------------------------------------------------------------------------
// Tests — NET-108.5
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::net::Ipv4Addr;

    fn addr(a: u8, b: u8, c: u8, d: u8) -> IpAddr {
        IpAddr::V4(Ipv4Addr::new(a, b, c, d))
    }

    #[test]
    fn define_static_vipa() {
        let mut mgr = VipaManager::new();
        mgr.define_static_vipa(addr(10, 1, 1, 100), "STACK1")
            .unwrap();
        let vipa = mgr.static_vipa(addr(10, 1, 1, 100)).unwrap();
        assert_eq!(vipa.stack_name, "STACK1");
        assert_eq!(vipa.state, VipaState::Active);
    }

    #[test]
    fn duplicate_static_vipa_errors() {
        let mut mgr = VipaManager::new();
        mgr.define_static_vipa(addr(10, 1, 1, 100), "STACK1")
            .unwrap();
        assert!(mgr
            .define_static_vipa(addr(10, 1, 1, 100), "STACK2")
            .is_err());
    }

    #[test]
    fn dynamic_vipa_failover() {
        let mut mgr = VipaManager::new();
        mgr.define_dynamic_vipa(
            addr(10, 1, 1, 200),
            "STACK1",
            Some("STACK2".to_string()),
        )
        .unwrap();
        mgr.failover(addr(10, 1, 1, 200)).unwrap();
        let vipa = mgr.dynamic_vipa_mut(addr(10, 1, 1, 200)).unwrap();
        assert_eq!(vipa.owning_stack, "STACK2");
        assert_eq!(vipa.state, VipaState::Active);
    }

    #[test]
    fn dynamic_vipa_no_backup_failover_errors() {
        let mut mgr = VipaManager::new();
        mgr.define_dynamic_vipa(addr(10, 1, 1, 200), "STACK1", None)
            .unwrap();
        assert!(mgr.failover(addr(10, 1, 1, 200)).is_err());
    }

    #[test]
    fn distributor_routes_to_best_target() {
        let mut dist = SysplexDistributor::new();
        dist.configure_port(
            8080,
            vec![
                DistributorTarget {
                    stack_name: "STACK1".to_string(),
                    address: addr(10, 1, 1, 1),
                    port: 8080,
                    healthy: true,
                    wlm_weight: 50,
                    connections: 0,
                },
                DistributorTarget {
                    stack_name: "STACK2".to_string(),
                    address: addr(10, 1, 2, 1),
                    port: 8080,
                    healthy: true,
                    wlm_weight: 100,
                    connections: 0,
                },
            ],
        );

        // Higher weight target should be chosen first
        let target = dist.route_connection(8080).unwrap();
        assert_eq!(target.stack_name, "STACK2");
    }

    #[test]
    fn distributor_skips_unhealthy_targets() {
        let mut dist = SysplexDistributor::new();
        dist.configure_port(
            8080,
            vec![
                DistributorTarget {
                    stack_name: "STACK1".to_string(),
                    address: addr(10, 1, 1, 1),
                    port: 8080,
                    healthy: false,
                    wlm_weight: 100,
                    connections: 0,
                },
                DistributorTarget {
                    stack_name: "STACK2".to_string(),
                    address: addr(10, 1, 2, 1),
                    port: 8080,
                    healthy: true,
                    wlm_weight: 50,
                    connections: 0,
                },
            ],
        );

        let target = dist.route_connection(8080).unwrap();
        assert_eq!(target.stack_name, "STACK2");
    }

    #[test]
    fn distributor_no_healthy_targets_errors() {
        let mut dist = SysplexDistributor::new();
        dist.configure_port(
            8080,
            vec![DistributorTarget {
                stack_name: "STACK1".to_string(),
                address: addr(10, 1, 1, 1),
                port: 8080,
                healthy: false,
                wlm_weight: 100,
                connections: 0,
            }],
        );
        assert!(dist.route_connection(8080).is_err());
    }

    #[test]
    fn distributor_not_configured_errors() {
        let mut dist = SysplexDistributor::new();
        assert!(dist.route_connection(9999).is_err());
    }

    #[test]
    fn wlm_weighted_routing_balances_load() {
        let mut dist = SysplexDistributor::new();
        dist.configure_port(
            8080,
            vec![
                DistributorTarget {
                    stack_name: "STACK1".to_string(),
                    address: addr(10, 1, 1, 1),
                    port: 8080,
                    healthy: true,
                    wlm_weight: 100,
                    connections: 0,
                },
                DistributorTarget {
                    stack_name: "STACK2".to_string(),
                    address: addr(10, 1, 2, 1),
                    port: 8080,
                    healthy: true,
                    wlm_weight: 50,
                    connections: 0,
                },
            ],
        );

        // Route multiple connections and check distribution
        for _ in 0..3 {
            dist.route_connection(8080).unwrap();
        }
        let targets = dist.targets(8080).unwrap();
        // STACK1 (weight 100) should have more connections than STACK2 (weight 50)
        let s1_conns = targets.iter().find(|t| t.stack_name == "STACK1").unwrap().connections;
        let s2_conns = targets.iter().find(|t| t.stack_name == "STACK2").unwrap().connections;
        assert!(s1_conns >= s2_conns);
    }

    #[test]
    fn vipa_count() {
        let mut mgr = VipaManager::new();
        mgr.define_static_vipa(addr(10, 1, 1, 100), "STACK1")
            .unwrap();
        mgr.define_dynamic_vipa(addr(10, 1, 1, 200), "STACK1", None)
            .unwrap();
        assert_eq!(mgr.vipa_count(), 2);
    }

    #[test]
    fn failover_nonexistent_vipa_errors() {
        let mut mgr = VipaManager::new();
        assert!(mgr.failover(addr(10, 1, 1, 200)).is_err());
    }
}
