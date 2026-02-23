//! AT-TLS & Security (NET-105).
//!
//! Provides AT-TLS policy agent configuration, TLS handshake simulation,
//! RACF keyring integration, and client certificate authentication.

use std::net::IpAddr;

use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum TlsError {
    #[error("no matching AT-TLS rule for {addr}:{port}")]
    NoMatchingRule { addr: IpAddr, port: u16 },
    #[error("keyring '{0}' not found")]
    KeyringNotFound(String),
    #[error("certificate not found: {0}")]
    CertificateNotFound(String),
    #[error("TLS handshake failed: {0}")]
    HandshakeFailed(String),
    #[error("client certificate required but not provided")]
    ClientCertRequired,
    #[error("client certificate validation failed: {0}")]
    ClientCertInvalid(String),
    #[error("unsupported TLS version: {0:?}")]
    UnsupportedVersion(TlsVersion),
}

// ---------------------------------------------------------------------------
// TLS Version
// ---------------------------------------------------------------------------

/// Supported TLS versions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TlsVersion {
    Tls10,
    Tls11,
    Tls12,
    Tls13,
}

impl std::fmt::Display for TlsVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tls10 => write!(f, "TLS 1.0"),
            Self::Tls11 => write!(f, "TLS 1.1"),
            Self::Tls12 => write!(f, "TLS 1.2"),
            Self::Tls13 => write!(f, "TLS 1.3"),
        }
    }
}

// ---------------------------------------------------------------------------
// NET-105.1 — AT-TLS Policy Agent
// ---------------------------------------------------------------------------

/// Direction for AT-TLS rules.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Inbound,
    Outbound,
    Both,
}

/// Handshake role.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HandshakeRole {
    Server,
    Client,
    ServerWithClientAuth,
}

/// A TLS cipher suite.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CipherSuite {
    pub name: String,
    pub strength: CipherStrength,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CipherStrength {
    High,
    Medium,
    Low,
}

/// A single AT-TLS rule matching connections to TLS parameters.
#[derive(Debug, Clone)]
pub struct TlsRule {
    pub name: String,
    pub local_port: Option<u16>,
    pub remote_port: Option<u16>,
    pub local_addr: Option<IpAddr>,
    pub remote_addr: Option<IpAddr>,
    pub direction: Direction,
    pub min_tls_version: TlsVersion,
    pub max_tls_version: TlsVersion,
    pub handshake_role: HandshakeRole,
    pub keyring: String,
    pub cipher_suites: Vec<CipherSuite>,
    pub enabled: bool,
    pub priority: u32,
}

impl TlsRule {
    /// Check if this rule matches a given connection.
    pub fn matches(&self, local_addr: IpAddr, local_port: u16, direction: Direction) -> bool {
        if !self.enabled {
            return false;
        }
        if let Some(rp) = self.local_port {
            if rp != local_port {
                return false;
            }
        }
        if let Some(ra) = self.local_addr {
            if ra != local_addr {
                return false;
            }
        }
        if self.direction != Direction::Both && self.direction != direction {
            return false;
        }
        true
    }
}

/// The AT-TLS Policy Agent managing rules.
#[derive(Debug)]
pub struct PolicyAgent {
    rules: Vec<TlsRule>,
}

impl Default for PolicyAgent {
    fn default() -> Self {
        Self::new()
    }
}

impl PolicyAgent {
    pub fn new() -> Self {
        Self { rules: Vec::new() }
    }

    /// Add a TLS rule to the policy agent.
    pub fn add_rule(&mut self, rule: TlsRule) {
        self.rules.push(rule);
        self.rules.sort_by(|a, b| b.priority.cmp(&a.priority));
    }

    /// Find the first matching rule for a connection.
    pub fn find_rule(
        &self,
        local_addr: IpAddr,
        local_port: u16,
        direction: Direction,
    ) -> Option<&TlsRule> {
        self.rules
            .iter()
            .find(|r| r.matches(local_addr, local_port, direction))
    }

    /// Number of rules.
    pub fn rule_count(&self) -> usize {
        self.rules.len()
    }
}

// ---------------------------------------------------------------------------
// NET-105.2 — TLS Handshake (simulated)
// ---------------------------------------------------------------------------

/// Result of a simulated TLS handshake.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HandshakeResult {
    pub negotiated_version: TlsVersion,
    pub cipher_suite: String,
    pub client_cert_subject: Option<String>,
}

/// Simulate a TLS handshake according to the matched rule.
pub fn perform_handshake(
    rule: &TlsRule,
    client_version: TlsVersion,
    client_cert: Option<&Certificate>,
) -> Result<HandshakeResult, TlsError> {
    // Version negotiation
    if client_version < rule.min_tls_version {
        return Err(TlsError::HandshakeFailed(format!(
            "client offers {}, server requires at least {}",
            client_version, rule.min_tls_version
        )));
    }
    let negotiated = client_version.min(rule.max_tls_version);

    // Cipher suite selection
    let cipher = rule
        .cipher_suites
        .first()
        .map(|c| c.name.clone())
        .unwrap_or_else(|| "TLS_AES_256_GCM_SHA384".to_string());

    // Client certificate check
    let client_cert_subject = if rule.handshake_role == HandshakeRole::ServerWithClientAuth {
        match client_cert {
            Some(cert) => {
                if !cert.valid {
                    return Err(TlsError::ClientCertInvalid(cert.subject.clone()));
                }
                Some(cert.subject.clone())
            }
            None => return Err(TlsError::ClientCertRequired),
        }
    } else {
        client_cert.map(|c| c.subject.clone())
    };

    Ok(HandshakeResult {
        negotiated_version: negotiated,
        cipher_suite: cipher,
        client_cert_subject,
    })
}

// ---------------------------------------------------------------------------
// NET-105.3 — RACF Keyring Integration
// ---------------------------------------------------------------------------

/// A certificate entry in a keyring.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Certificate {
    pub label: String,
    pub subject: String,
    pub issuer: String,
    pub serial: String,
    pub valid: bool,
    pub is_ca: bool,
}

/// A RACF keyring containing certificates.
#[derive(Debug, Clone)]
pub struct Keyring {
    pub name: String,
    pub owner: String,
    pub certificates: Vec<Certificate>,
}

impl Keyring {
    pub fn new(name: impl Into<String>, owner: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            owner: owner.into(),
            certificates: Vec::new(),
        }
    }

    /// Add a certificate to the keyring.
    pub fn add_certificate(&mut self, cert: Certificate) {
        self.certificates.push(cert);
    }

    /// Find a certificate by label.
    pub fn find_certificate(&self, label: &str) -> Option<&Certificate> {
        self.certificates.iter().find(|c| c.label == label)
    }

    /// Get all CA certificates (trust anchors).
    pub fn ca_certificates(&self) -> Vec<&Certificate> {
        self.certificates.iter().filter(|c| c.is_ca).collect()
    }
}

/// Keyring store managing multiple keyrings.
#[derive(Debug, Default)]
pub struct KeyringStore {
    keyrings: Vec<Keyring>,
}

impl KeyringStore {
    pub fn new() -> Self {
        Self {
            keyrings: Vec::new(),
        }
    }

    /// Add a keyring to the store.
    pub fn add_keyring(&mut self, keyring: Keyring) {
        self.keyrings.push(keyring);
    }

    /// Find a keyring by name.
    pub fn find_keyring(&self, name: &str) -> Option<&Keyring> {
        self.keyrings.iter().find(|k| k.name == name)
    }

    /// Load certificates for TLS from a named keyring.
    pub fn load_for_tls(&self, keyring_name: &str) -> Result<&Keyring, TlsError> {
        self.find_keyring(keyring_name)
            .ok_or_else(|| TlsError::KeyringNotFound(keyring_name.to_string()))
    }
}

// ---------------------------------------------------------------------------
// NET-105.4 — Client Certificate Authentication
// (covered by perform_handshake with ServerWithClientAuth)
// ---------------------------------------------------------------------------

/// Validate a client certificate against trust store CA certs.
pub fn validate_client_cert(cert: &Certificate, trust_store: &Keyring) -> Result<(), TlsError> {
    if !cert.valid {
        return Err(TlsError::ClientCertInvalid(cert.subject.clone()));
    }
    let ca_certs = trust_store.ca_certificates();
    let trusted = ca_certs.iter().any(|ca| ca.subject == cert.issuer);
    if !trusted {
        return Err(TlsError::ClientCertInvalid(format!(
            "issuer '{}' not in trust store",
            cert.issuer
        )));
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Tests — NET-105.5
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn make_rule(port: u16, role: HandshakeRole) -> TlsRule {
        TlsRule {
            name: format!("rule-{}", port),
            local_port: Some(port),
            remote_port: None,
            local_addr: None,
            remote_addr: None,
            direction: Direction::Both,
            min_tls_version: TlsVersion::Tls12,
            max_tls_version: TlsVersion::Tls13,
            handshake_role: role,
            keyring: "ring01".to_string(),
            cipher_suites: vec![CipherSuite {
                name: "TLS_AES_256_GCM_SHA384".to_string(),
                strength: CipherStrength::High,
            }],
            enabled: true,
            priority: 100,
        }
    }

    fn make_cert(valid: bool) -> Certificate {
        Certificate {
            label: "server-cert".to_string(),
            subject: "CN=server.example.com".to_string(),
            issuer: "CN=Example CA".to_string(),
            serial: "001".to_string(),
            valid,
            is_ca: false,
        }
    }

    #[test]
    fn policy_agent_rule_matching() {
        let mut agent = PolicyAgent::new();
        agent.add_rule(make_rule(443, HandshakeRole::Server));
        agent.add_rule(make_rule(8443, HandshakeRole::ServerWithClientAuth));

        let addr: IpAddr = "10.1.1.100".parse().unwrap();
        let rule = agent.find_rule(addr, 443, Direction::Inbound).unwrap();
        assert_eq!(rule.name, "rule-443");

        let rule = agent.find_rule(addr, 8443, Direction::Inbound).unwrap();
        assert_eq!(rule.handshake_role, HandshakeRole::ServerWithClientAuth);

        assert!(agent.find_rule(addr, 80, Direction::Inbound).is_none());
    }

    #[test]
    fn handshake_tls13() {
        let rule = make_rule(443, HandshakeRole::Server);
        let result = perform_handshake(&rule, TlsVersion::Tls13, None).unwrap();
        assert_eq!(result.negotiated_version, TlsVersion::Tls13);
    }

    #[test]
    fn handshake_version_negotiation() {
        let rule = make_rule(443, HandshakeRole::Server);
        let result = perform_handshake(&rule, TlsVersion::Tls12, None).unwrap();
        assert_eq!(result.negotiated_version, TlsVersion::Tls12);
    }

    #[test]
    fn handshake_version_too_low() {
        let rule = make_rule(443, HandshakeRole::Server);
        assert!(perform_handshake(&rule, TlsVersion::Tls10, None).is_err());
    }

    #[test]
    fn handshake_client_cert_required() {
        let rule = make_rule(8443, HandshakeRole::ServerWithClientAuth);
        assert!(perform_handshake(&rule, TlsVersion::Tls13, None).is_err());
    }

    #[test]
    fn handshake_client_cert_valid() {
        let rule = make_rule(8443, HandshakeRole::ServerWithClientAuth);
        let cert = make_cert(true);
        let result = perform_handshake(&rule, TlsVersion::Tls13, Some(&cert)).unwrap();
        assert_eq!(
            result.client_cert_subject,
            Some("CN=server.example.com".to_string())
        );
    }

    #[test]
    fn handshake_client_cert_invalid() {
        let rule = make_rule(8443, HandshakeRole::ServerWithClientAuth);
        let cert = make_cert(false);
        assert!(perform_handshake(&rule, TlsVersion::Tls13, Some(&cert)).is_err());
    }

    #[test]
    fn keyring_operations() {
        let mut keyring = Keyring::new("ring01", "IBMUSER");
        keyring.add_certificate(make_cert(true));
        keyring.add_certificate(Certificate {
            label: "ca-cert".to_string(),
            subject: "CN=Example CA".to_string(),
            issuer: "CN=Example CA".to_string(),
            serial: "000".to_string(),
            valid: true,
            is_ca: true,
        });

        assert!(keyring.find_certificate("server-cert").is_some());
        assert_eq!(keyring.ca_certificates().len(), 1);
    }

    #[test]
    fn keyring_store() {
        let mut store = KeyringStore::new();
        let keyring = Keyring::new("ring01", "IBMUSER");
        store.add_keyring(keyring);
        assert!(store.load_for_tls("ring01").is_ok());
        assert!(store.load_for_tls("missing").is_err());
    }

    #[test]
    fn validate_client_cert_trusted() {
        let mut trust_store = Keyring::new("trust", "IBMUSER");
        trust_store.add_certificate(Certificate {
            label: "ca".to_string(),
            subject: "CN=Example CA".to_string(),
            issuer: "CN=Example CA".to_string(),
            serial: "000".to_string(),
            valid: true,
            is_ca: true,
        });
        let cert = make_cert(true);
        validate_client_cert(&cert, &trust_store).unwrap();
    }

    #[test]
    fn validate_client_cert_untrusted_issuer() {
        let trust_store = Keyring::new("trust", "IBMUSER");
        let cert = make_cert(true);
        assert!(validate_client_cert(&cert, &trust_store).is_err());
    }

    #[test]
    fn disabled_rule_no_match() {
        let mut rule = make_rule(443, HandshakeRole::Server);
        rule.enabled = false;
        let addr: IpAddr = "10.1.1.100".parse().unwrap();
        assert!(!rule.matches(addr, 443, Direction::Inbound));
    }

    #[test]
    fn rule_priority_ordering() {
        let mut agent = PolicyAgent::new();
        let mut r1 = make_rule(443, HandshakeRole::Server);
        r1.priority = 50;
        r1.name = "low-priority".to_string();
        let mut r2 = make_rule(443, HandshakeRole::ServerWithClientAuth);
        r2.priority = 200;
        r2.name = "high-priority".to_string();
        agent.add_rule(r1);
        agent.add_rule(r2);

        let addr: IpAddr = "10.1.1.100".parse().unwrap();
        let matched = agent.find_rule(addr, 443, Direction::Inbound).unwrap();
        assert_eq!(matched.name, "high-priority");
    }
}
