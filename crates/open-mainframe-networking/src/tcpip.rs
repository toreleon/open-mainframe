//! TCP/IP Stack Configuration (NET-103).
//!
//! Provides TCPIP.PROFILE parsing for port reservations, interface config,
//! DNS resolver settings, AUTOLOG entries, and protocol options.

use std::collections::HashMap;
use std::net::IpAddr;

use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum TcpIpError {
    #[error("invalid profile directive: {0}")]
    InvalidDirective(String),
    #[error("port {0} already reserved")]
    PortAlreadyReserved(u16),
    #[error("invalid IP address: {0}")]
    InvalidIpAddress(String),
    #[error("parse error at line {line}: {message}")]
    ParseError { line: usize, message: String },
    #[error("unknown protocol: {0}")]
    UnknownProtocol(String),
}

// ---------------------------------------------------------------------------
// NET-103.1 — TCPIP.PROFILE Parser
// ---------------------------------------------------------------------------

/// Transport protocol for port reservations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TransportProtocol {
    Tcp,
    Udp,
}

/// A port reservation from the TCPIP.PROFILE.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PortReservation {
    pub port: u16,
    pub protocol: TransportProtocol,
    pub daemon_name: String,
}

/// TCP configuration options from TCPCONFIG.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TcpConfig {
    pub ttls_enabled: bool,
    pub keepalive_interval: u32,
    pub timestamp: bool,
    pub ecn: bool,
}

impl Default for TcpConfig {
    fn default() -> Self {
        Self {
            ttls_enabled: false,
            keepalive_interval: 120,
            timestamp: false,
            ecn: false,
        }
    }
}

/// UDP configuration options.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UdpConfig {
    pub checksum: bool,
}

impl Default for UdpConfig {
    fn default() -> Self {
        Self { checksum: true }
    }
}

// ---------------------------------------------------------------------------
// NET-103.2 — Port Reservation and AUTOLOG
// ---------------------------------------------------------------------------

/// An AUTOLOG entry that starts daemon instances at stack startup.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AutologEntry {
    pub instance_count: u32,
    pub daemon_name: String,
}

// ---------------------------------------------------------------------------
// NET-103.3 — Interface and Routing Configuration
// ---------------------------------------------------------------------------

/// Device type for network interfaces.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeviceType {
    Osa,
    Vswitch,
    Loopback,
}

/// A network device definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeviceDefinition {
    pub name: String,
    pub device_type: DeviceType,
}

/// A link definition tying a device to a link name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkDefinition {
    pub link_name: String,
    pub device_name: String,
}

/// A HOME address binding.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HomeAddress {
    pub address: IpAddr,
    pub link_name: String,
}

/// A static route entry.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RouteEntry {
    pub destination: String,
    pub gateway: IpAddr,
    pub link_name: String,
}

// ---------------------------------------------------------------------------
// NET-103.4 — DNS Resolver Configuration
// ---------------------------------------------------------------------------

/// DNS resolver configuration from TCPIP.PROFILE.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolverConfig {
    pub nameservers: Vec<IpAddr>,
    pub timeout_seconds: u32,
    pub retry_count: u32,
    pub domain_name: String,
}

impl Default for ResolverConfig {
    fn default() -> Self {
        Self {
            nameservers: Vec::new(),
            timeout_seconds: 5,
            retry_count: 2,
            domain_name: String::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// TCPIP.PROFILE — Combined Profile
// ---------------------------------------------------------------------------

/// Complete TCP/IP stack profile parsed from TCPIP.PROFILE.
#[derive(Debug, Clone)]
pub struct TcpIpProfile {
    pub hostname: String,
    pub ports: Vec<PortReservation>,
    pub tcp_config: TcpConfig,
    pub udp_config: UdpConfig,
    pub autolog: Vec<AutologEntry>,
    pub devices: Vec<DeviceDefinition>,
    pub links: Vec<LinkDefinition>,
    pub home_addresses: Vec<HomeAddress>,
    pub routes: Vec<RouteEntry>,
    pub resolver: ResolverConfig,
}

impl Default for TcpIpProfile {
    fn default() -> Self {
        Self::new()
    }
}

impl TcpIpProfile {
    pub fn new() -> Self {
        Self {
            hostname: String::new(),
            ports: Vec::new(),
            tcp_config: TcpConfig::default(),
            udp_config: UdpConfig::default(),
            autolog: Vec::new(),
            devices: Vec::new(),
            links: Vec::new(),
            home_addresses: Vec::new(),
            routes: Vec::new(),
            resolver: ResolverConfig::default(),
        }
    }

    /// Parse a TCPIP.PROFILE text into a profile structure.
    pub fn parse(input: &str) -> Result<Self, TcpIpError> {
        let mut profile = Self::new();
        let mut reserved_ports: HashMap<u16, String> = HashMap::new();

        for (line_num, line) in input.lines().enumerate() {
            let line = line.trim();
            if line.is_empty() || line.starts_with(';') || line.starts_with('#') {
                continue;
            }

            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.is_empty() {
                continue;
            }

            match parts[0].to_uppercase().as_str() {
                "HOSTNAME" => {
                    if parts.len() >= 2 {
                        profile.hostname = parts[1].to_string();
                    }
                }
                "PORT" => {
                    if parts.len() >= 4 {
                        let port: u16 = parts[1].parse().map_err(|_| TcpIpError::ParseError {
                            line: line_num + 1,
                            message: format!("invalid port number: {}", parts[1]),
                        })?;
                        let protocol = match parts[2].to_uppercase().as_str() {
                            "TCP" => TransportProtocol::Tcp,
                            "UDP" => TransportProtocol::Udp,
                            other => return Err(TcpIpError::UnknownProtocol(other.to_string())),
                        };
                        let daemon = parts[3].to_string();
                        if let Some(existing) = reserved_ports.get(&port) {
                            if *existing != daemon {
                                return Err(TcpIpError::PortAlreadyReserved(port));
                            }
                        }
                        reserved_ports.insert(port, daemon.clone());
                        profile.ports.push(PortReservation {
                            port,
                            protocol,
                            daemon_name: daemon,
                        });
                    }
                }
                "TCPCONFIG" => {
                    for flag in &parts[1..] {
                        match flag.to_uppercase().as_str() {
                            "TTLS" => profile.tcp_config.ttls_enabled = true,
                            "TIMESTAMP" => profile.tcp_config.timestamp = true,
                            "ECN" => profile.tcp_config.ecn = true,
                            _ => {}
                        }
                    }
                }
                "AUTOLOG" => {
                    if parts.len() >= 3 {
                        let count: u32 =
                            parts[1].parse().map_err(|_| TcpIpError::ParseError {
                                line: line_num + 1,
                                message: format!("invalid instance count: {}", parts[1]),
                            })?;
                        profile.autolog.push(AutologEntry {
                            instance_count: count,
                            daemon_name: parts[2].to_string(),
                        });
                    }
                }
                "DEVICE" => {
                    if parts.len() >= 3 {
                        let device_type = match parts[2].to_uppercase().as_str() {
                            "OSA" => DeviceType::Osa,
                            "VSWITCH" => DeviceType::Vswitch,
                            "LOOPBACK" => DeviceType::Loopback,
                            _ => DeviceType::Osa,
                        };
                        profile.devices.push(DeviceDefinition {
                            name: parts[1].to_string(),
                            device_type,
                        });
                    }
                }
                "LINK" => {
                    if parts.len() >= 3 {
                        profile.links.push(LinkDefinition {
                            link_name: parts[1].to_string(),
                            device_name: parts[2].to_string(),
                        });
                    }
                }
                "HOME" => {
                    if parts.len() >= 3 {
                        let addr: IpAddr = parts[1].parse().map_err(|_| {
                            TcpIpError::InvalidIpAddress(parts[1].to_string())
                        })?;
                        profile.home_addresses.push(HomeAddress {
                            address: addr,
                            link_name: parts[2].to_string(),
                        });
                    }
                }
                "NSINTERADDR" => {
                    if parts.len() >= 2 {
                        let addr: IpAddr = parts[1].parse().map_err(|_| {
                            TcpIpError::InvalidIpAddress(parts[1].to_string())
                        })?;
                        profile.resolver.nameservers.push(addr);
                    }
                }
                "RESOLVERTIMEOUT" => {
                    if parts.len() >= 2 {
                        let timeout: u32 =
                            parts[1].parse().map_err(|_| TcpIpError::ParseError {
                                line: line_num + 1,
                                message: format!("invalid timeout: {}", parts[1]),
                            })?;
                        profile.resolver.timeout_seconds = timeout;
                    }
                }
                "DOMAINNAME" => {
                    if parts.len() >= 2 {
                        profile.resolver.domain_name = parts[1].to_string();
                    }
                }
                _ => {
                    // Unknown directives are ignored for forward compatibility
                }
            }
        }

        Ok(profile)
    }

    /// Look up which daemon owns a given port.
    pub fn port_owner(&self, port: u16) -> Option<&str> {
        self.ports
            .iter()
            .find(|p| p.port == port)
            .map(|p| p.daemon_name.as_str())
    }
}

// ---------------------------------------------------------------------------
// Tests — NET-103.5
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE_PROFILE: &str = r#"
; Sample TCPIP.PROFILE
HOSTNAME LPAR1
PORT 21 TCP FTPD
PORT 22 TCP SSHD
PORT 23 TCP TELNETD
PORT 80 TCP HTTPD
PORT 443 TCP HTTPD
TCPCONFIG TTLS TIMESTAMP
AUTOLOG 5 FTPD
AUTOLOG 2 SSHD
DEVICE ETH0 OSA
DEVICE VSWITCH0 VSWITCH
LINK ETHLINK ETH0
HOME 10.1.1.100 ETHLINK
NSINTERADDR 10.1.1.1
NSINTERADDR 10.1.1.2
RESOLVERTIMEOUT 10
DOMAINNAME example.com
"#;

    #[test]
    fn parse_hostname() {
        let profile = TcpIpProfile::parse(SAMPLE_PROFILE).unwrap();
        assert_eq!(profile.hostname, "LPAR1");
    }

    #[test]
    fn parse_port_reservations() {
        let profile = TcpIpProfile::parse(SAMPLE_PROFILE).unwrap();
        assert_eq!(profile.ports.len(), 5);
        assert_eq!(profile.port_owner(21), Some("FTPD"));
        assert_eq!(profile.port_owner(22), Some("SSHD"));
        assert_eq!(profile.port_owner(443), Some("HTTPD"));
    }

    #[test]
    fn parse_tcpconfig_ttls() {
        let profile = TcpIpProfile::parse(SAMPLE_PROFILE).unwrap();
        assert!(profile.tcp_config.ttls_enabled);
        assert!(profile.tcp_config.timestamp);
        assert!(!profile.tcp_config.ecn);
    }

    #[test]
    fn parse_autolog() {
        let profile = TcpIpProfile::parse(SAMPLE_PROFILE).unwrap();
        assert_eq!(profile.autolog.len(), 2);
        assert_eq!(profile.autolog[0].instance_count, 5);
        assert_eq!(profile.autolog[0].daemon_name, "FTPD");
    }

    #[test]
    fn parse_device_and_link() {
        let profile = TcpIpProfile::parse(SAMPLE_PROFILE).unwrap();
        assert_eq!(profile.devices.len(), 2);
        assert_eq!(profile.devices[0].device_type, DeviceType::Osa);
        assert_eq!(profile.links.len(), 1);
        assert_eq!(profile.links[0].link_name, "ETHLINK");
    }

    #[test]
    fn parse_home_address() {
        let profile = TcpIpProfile::parse(SAMPLE_PROFILE).unwrap();
        assert_eq!(profile.home_addresses.len(), 1);
        let home = &profile.home_addresses[0];
        assert_eq!(home.address, "10.1.1.100".parse::<IpAddr>().unwrap());
        assert_eq!(home.link_name, "ETHLINK");
    }

    #[test]
    fn parse_dns_resolver() {
        let profile = TcpIpProfile::parse(SAMPLE_PROFILE).unwrap();
        assert_eq!(profile.resolver.nameservers.len(), 2);
        assert_eq!(
            profile.resolver.nameservers[0],
            "10.1.1.1".parse::<IpAddr>().unwrap()
        );
        assert_eq!(profile.resolver.timeout_seconds, 10);
        assert_eq!(profile.resolver.domain_name, "example.com");
    }

    #[test]
    fn empty_profile() {
        let profile = TcpIpProfile::parse("").unwrap();
        assert!(profile.hostname.is_empty());
        assert!(profile.ports.is_empty());
    }

    #[test]
    fn comment_lines_ignored() {
        let profile = TcpIpProfile::parse("; comment only\n# another comment").unwrap();
        assert!(profile.hostname.is_empty());
    }

    #[test]
    fn duplicate_port_different_daemon_errors() {
        let input = "PORT 21 TCP FTPD\nPORT 21 TCP SSHD\n";
        assert!(TcpIpProfile::parse(input).is_err());
    }

    #[test]
    fn invalid_ip_address_errors() {
        let input = "HOME notanip ETHLINK\n";
        assert!(TcpIpProfile::parse(input).is_err());
    }

    #[test]
    fn port_owner_not_found() {
        let profile = TcpIpProfile::parse(SAMPLE_PROFILE).unwrap();
        assert_eq!(profile.port_owner(9999), None);
    }
}
