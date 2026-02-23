//! IP Filtering & IPSec (NET-109).
//!
//! Provides IP filter rules (permit/deny/IPSec actions),
//! IPSec Security Associations, and a Defense Manager Daemon model
//! for dynamic rule management.

use std::net::IpAddr;

use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum SecurityError {
    #[error("rule '{0}' not found")]
    RuleNotFound(String),
    #[error("invalid CIDR prefix length: /{0}")]
    InvalidPrefix(u8),
    #[error("IPSec SA negotiation failed: {0}")]
    IpsecNegotiationFailed(String),
    #[error("duplicate rule name: {0}")]
    DuplicateRule(String),
    #[error("rule set is empty")]
    EmptyRuleSet,
}

// ---------------------------------------------------------------------------
// NET-109.1 — IP Filter Rules
// ---------------------------------------------------------------------------

/// IP address with prefix length (CIDR notation).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IpNetwork {
    pub address: IpAddr,
    pub prefix_len: u8,
}

impl IpNetwork {
    pub fn new(address: IpAddr, prefix_len: u8) -> Result<Self, SecurityError> {
        let max = match address {
            IpAddr::V4(_) => 32,
            IpAddr::V6(_) => 128,
        };
        if prefix_len > max {
            return Err(SecurityError::InvalidPrefix(prefix_len));
        }
        Ok(Self {
            address,
            prefix_len,
        })
    }

    /// Check if a given IP address is within this network.
    pub fn contains(&self, addr: IpAddr) -> bool {
        match (&self.address, &addr) {
            (IpAddr::V4(net), IpAddr::V4(host)) => {
                if self.prefix_len == 0 {
                    return true;
                }
                let mask = !0u32 << (32 - self.prefix_len);
                (u32::from(*net) & mask) == (u32::from(*host) & mask)
            }
            (IpAddr::V6(net), IpAddr::V6(host)) => {
                if self.prefix_len == 0 {
                    return true;
                }
                let net_bits = u128::from(*net);
                let host_bits = u128::from(*host);
                let mask = !0u128 << (128 - self.prefix_len);
                (net_bits & mask) == (host_bits & mask)
            }
            _ => false, // v4/v6 mismatch
        }
    }

    /// Wildcard — matches any address.
    pub fn any_v4() -> Self {
        Self {
            address: IpAddr::V4(std::net::Ipv4Addr::UNSPECIFIED),
            prefix_len: 0,
        }
    }
}

/// Protocol match for filter rules.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProtocolMatch {
    Any,
    Tcp,
    Udp,
    Icmp,
}

// ---------------------------------------------------------------------------
// NET-109.2 — IP Filter Actions
// ---------------------------------------------------------------------------

/// Action to take when a filter rule matches.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FilterAction {
    Permit,
    Deny,
    Ipsec,
}

/// A single IP filter rule.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IpFilterRule {
    pub name: String,
    pub source: IpNetwork,
    pub destination: IpNetwork,
    pub source_port: Option<u16>,
    pub dest_port: Option<u16>,
    pub protocol: ProtocolMatch,
    pub action: FilterAction,
    pub priority: u32,
    pub enabled: bool,
}

impl IpFilterRule {
    /// Check if this rule matches a given packet.
    pub fn matches(
        &self,
        src_addr: IpAddr,
        dst_addr: IpAddr,
        src_port: u16,
        dst_port: u16,
        protocol: ProtocolMatch,
    ) -> bool {
        if !self.enabled {
            return false;
        }
        if !self.source.contains(src_addr) {
            return false;
        }
        if !self.destination.contains(dst_addr) {
            return false;
        }
        if let Some(sp) = self.source_port {
            if sp != src_port {
                return false;
            }
        }
        if let Some(dp) = self.dest_port {
            if dp != dst_port {
                return false;
            }
        }
        if self.protocol != ProtocolMatch::Any && self.protocol != protocol {
            return false;
        }
        true
    }
}

// ---------------------------------------------------------------------------
// NET-109.3 — IPSec Security Associations
// ---------------------------------------------------------------------------

/// IKE authentication method.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IkeAuthMethod {
    PreSharedKey(String),
    Certificate(String),
}

/// IPSec encryption algorithm.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IpsecAlgorithm {
    Aes128,
    Aes256,
    TripleDes,
}

/// IPSec protocol mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IpsecMode {
    Transport,
    Tunnel,
}

/// An IPSec Security Association.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SecurityAssociation {
    pub name: String,
    pub local_addr: IpAddr,
    pub remote_addr: IpAddr,
    pub algorithm: IpsecAlgorithm,
    pub mode: IpsecMode,
    pub auth_method: IkeAuthMethod,
    pub active: bool,
    pub spi: u32,
}

impl SecurityAssociation {
    /// Negotiate (simulate) an IPSec SA.
    pub fn negotiate(
        name: impl Into<String>,
        local: IpAddr,
        remote: IpAddr,
        algorithm: IpsecAlgorithm,
        mode: IpsecMode,
        auth: IkeAuthMethod,
    ) -> Result<Self, SecurityError> {
        // Simulate IKE negotiation
        Ok(Self {
            name: name.into(),
            local_addr: local,
            remote_addr: remote,
            algorithm,
            mode,
            auth_method: auth,
            active: true,
            spi: rand_spi(),
        })
    }
}

/// Generate a pseudo-random SPI value (non-cryptographic, for simulation).
fn rand_spi() -> u32 {
    // Use a simple counter-based approach for deterministic tests
    static COUNTER: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(256);
    COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

// ---------------------------------------------------------------------------
// NET-109.4 — Defense Manager Daemon (DMD) Model
// ---------------------------------------------------------------------------

/// The Defense Manager Daemon manages IP filter rules and IPSec SAs.
#[derive(Debug)]
pub struct DefenseManager {
    rules: Vec<IpFilterRule>,
    associations: Vec<SecurityAssociation>,
    generation: u64,
}

impl Default for DefenseManager {
    fn default() -> Self {
        Self::new()
    }
}

impl DefenseManager {
    pub fn new() -> Self {
        Self {
            rules: Vec::new(),
            associations: Vec::new(),
            generation: 1,
        }
    }

    /// Load initial filter rules.
    pub fn load_rules(&mut self, rules: Vec<IpFilterRule>) -> Result<(), SecurityError> {
        // Check for duplicate names
        for (i, r) in rules.iter().enumerate() {
            for other in &rules[i + 1..] {
                if r.name == other.name {
                    return Err(SecurityError::DuplicateRule(r.name.clone()));
                }
            }
        }
        self.rules = rules;
        self.rules.sort_by(|a, b| b.priority.cmp(&a.priority));
        self.generation += 1;
        Ok(())
    }

    /// Dynamically add or update a rule.
    pub fn update_rule(&mut self, rule: IpFilterRule) {
        if let Some(existing) = self.rules.iter_mut().find(|r| r.name == rule.name) {
            *existing = rule;
        } else {
            self.rules.push(rule);
        }
        self.rules.sort_by(|a, b| b.priority.cmp(&a.priority));
        self.generation += 1;
    }

    /// Remove a rule by name.
    pub fn remove_rule(&mut self, name: &str) -> Result<(), SecurityError> {
        let idx = self
            .rules
            .iter()
            .position(|r| r.name == name)
            .ok_or_else(|| SecurityError::RuleNotFound(name.to_string()))?;
        self.rules.remove(idx);
        self.generation += 1;
        Ok(())
    }

    /// Evaluate a packet against all rules, returning the first matching action.
    pub fn evaluate(
        &self,
        src_addr: IpAddr,
        dst_addr: IpAddr,
        src_port: u16,
        dst_port: u16,
        protocol: ProtocolMatch,
    ) -> FilterAction {
        for rule in &self.rules {
            if rule.matches(src_addr, dst_addr, src_port, dst_port, protocol) {
                return rule.action;
            }
        }
        // Default: permit (no matching rule)
        FilterAction::Permit
    }

    /// Add a Security Association.
    pub fn add_sa(&mut self, sa: SecurityAssociation) {
        self.associations.push(sa);
    }

    /// Find active SAs for a given remote address.
    pub fn find_sa(&self, remote: IpAddr) -> Option<&SecurityAssociation> {
        self.associations
            .iter()
            .find(|sa| sa.remote_addr == remote && sa.active)
    }

    /// Current rule generation (incremented on each change).
    pub fn generation(&self) -> u64 {
        self.generation
    }

    /// Number of rules.
    pub fn rule_count(&self) -> usize {
        self.rules.len()
    }
}

// ---------------------------------------------------------------------------
// Tests — NET-109.5
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::net::Ipv4Addr;

    fn addr(a: u8, b: u8, c: u8, d: u8) -> IpAddr {
        IpAddr::V4(Ipv4Addr::new(a, b, c, d))
    }

    fn deny_rule(name: &str, src_prefix: u8, dest_port: u16) -> IpFilterRule {
        IpFilterRule {
            name: name.to_string(),
            source: IpNetwork::new(addr(10, 0, 0, 0), src_prefix).unwrap(),
            destination: IpNetwork::any_v4(),
            source_port: None,
            dest_port: Some(dest_port),
            protocol: ProtocolMatch::Tcp,
            action: FilterAction::Deny,
            priority: 100,
            enabled: true,
        }
    }

    #[test]
    fn ip_network_contains() {
        let net = IpNetwork::new(addr(10, 0, 0, 0), 8).unwrap();
        assert!(net.contains(addr(10, 1, 2, 3)));
        assert!(net.contains(addr(10, 255, 255, 255)));
        assert!(!net.contains(addr(192, 168, 1, 1)));
    }

    #[test]
    fn ip_network_any() {
        let net = IpNetwork::any_v4();
        assert!(net.contains(addr(10, 1, 1, 1)));
        assert!(net.contains(addr(192, 168, 1, 1)));
    }

    #[test]
    fn invalid_prefix_errors() {
        assert!(IpNetwork::new(addr(10, 0, 0, 0), 33).is_err());
    }

    #[test]
    fn filter_rule_deny_telnet() {
        let rule = deny_rule("deny-telnet", 8, 23);
        assert!(rule.matches(addr(10, 1, 1, 1), addr(192, 168, 1, 1), 5000, 23, ProtocolMatch::Tcp));
        assert!(!rule.matches(addr(192, 168, 1, 1), addr(10, 1, 1, 1), 5000, 23, ProtocolMatch::Tcp));
    }

    #[test]
    fn filter_rule_disabled_no_match() {
        let mut rule = deny_rule("deny-telnet", 8, 23);
        rule.enabled = false;
        assert!(!rule.matches(addr(10, 1, 1, 1), addr(192, 168, 1, 1), 5000, 23, ProtocolMatch::Tcp));
    }

    #[test]
    fn filter_action_ipsec() {
        let rule = IpFilterRule {
            name: "ipsec-required".to_string(),
            source: IpNetwork::any_v4(),
            destination: IpNetwork::new(addr(10, 2, 0, 0), 16).unwrap(),
            source_port: None,
            dest_port: None,
            protocol: ProtocolMatch::Any,
            action: FilterAction::Ipsec,
            priority: 200,
            enabled: true,
        };
        assert_eq!(rule.action, FilterAction::Ipsec);
        assert!(rule.matches(addr(192, 168, 1, 1), addr(10, 2, 3, 4), 0, 0, ProtocolMatch::Any));
    }

    #[test]
    fn ipsec_sa_negotiation() {
        let sa = SecurityAssociation::negotiate(
            "sa-1",
            addr(10, 1, 1, 1),
            addr(10, 2, 2, 2),
            IpsecAlgorithm::Aes256,
            IpsecMode::Tunnel,
            IkeAuthMethod::PreSharedKey("secret".to_string()),
        )
        .unwrap();
        assert!(sa.active);
        assert!(sa.spi >= 256);
    }

    #[test]
    fn defense_manager_evaluate() {
        let mut dm = DefenseManager::new();
        dm.load_rules(vec![deny_rule("deny-telnet", 8, 23)])
            .unwrap();

        let action = dm.evaluate(addr(10, 1, 1, 1), addr(192, 168, 1, 1), 5000, 23, ProtocolMatch::Tcp);
        assert_eq!(action, FilterAction::Deny);

        // Non-matching returns default Permit
        let action = dm.evaluate(addr(192, 168, 1, 1), addr(10, 1, 1, 1), 5000, 80, ProtocolMatch::Tcp);
        assert_eq!(action, FilterAction::Permit);
    }

    #[test]
    fn defense_manager_dynamic_update() {
        let mut dm = DefenseManager::new();
        dm.load_rules(vec![deny_rule("deny-telnet", 8, 23)])
            .unwrap();
        let gen1 = dm.generation();

        dm.update_rule(IpFilterRule {
            name: "deny-ftp".to_string(),
            source: IpNetwork::any_v4(),
            destination: IpNetwork::any_v4(),
            source_port: None,
            dest_port: Some(21),
            protocol: ProtocolMatch::Tcp,
            action: FilterAction::Deny,
            priority: 50,
            enabled: true,
        });
        assert_eq!(dm.rule_count(), 2);
        assert!(dm.generation() > gen1);
    }

    #[test]
    fn defense_manager_remove_rule() {
        let mut dm = DefenseManager::new();
        dm.load_rules(vec![deny_rule("deny-telnet", 8, 23)])
            .unwrap();
        dm.remove_rule("deny-telnet").unwrap();
        assert_eq!(dm.rule_count(), 0);
    }

    #[test]
    fn defense_manager_remove_nonexistent_errors() {
        let mut dm = DefenseManager::new();
        assert!(dm.remove_rule("missing").is_err());
    }

    #[test]
    fn defense_manager_duplicate_rule_names_error() {
        let mut dm = DefenseManager::new();
        assert!(dm
            .load_rules(vec![
                deny_rule("same-name", 8, 23),
                deny_rule("same-name", 16, 80),
            ])
            .is_err());
    }

    #[test]
    fn defense_manager_sa_lookup() {
        let mut dm = DefenseManager::new();
        let sa = SecurityAssociation::negotiate(
            "sa-1",
            addr(10, 1, 1, 1),
            addr(10, 2, 2, 2),
            IpsecAlgorithm::Aes256,
            IpsecMode::Tunnel,
            IkeAuthMethod::PreSharedKey("key".to_string()),
        )
        .unwrap();
        dm.add_sa(sa);
        assert!(dm.find_sa(addr(10, 2, 2, 2)).is_some());
        assert!(dm.find_sa(addr(10, 3, 3, 3)).is_none());
    }

    #[test]
    fn priority_ordering() {
        let mut dm = DefenseManager::new();
        dm.load_rules(vec![
            IpFilterRule {
                name: "low".to_string(),
                source: IpNetwork::any_v4(),
                destination: IpNetwork::any_v4(),
                source_port: None,
                dest_port: Some(80),
                protocol: ProtocolMatch::Tcp,
                action: FilterAction::Permit,
                priority: 10,
                enabled: true,
            },
            IpFilterRule {
                name: "high".to_string(),
                source: IpNetwork::any_v4(),
                destination: IpNetwork::any_v4(),
                source_port: None,
                dest_port: Some(80),
                protocol: ProtocolMatch::Tcp,
                action: FilterAction::Deny,
                priority: 100,
                enabled: true,
            },
        ])
        .unwrap();
        // Higher priority (Deny) should match first
        let action = dm.evaluate(addr(1, 2, 3, 4), addr(5, 6, 7, 8), 1000, 80, ProtocolMatch::Tcp);
        assert_eq!(action, FilterAction::Deny);
    }
}
