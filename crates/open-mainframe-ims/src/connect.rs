//! IMS-TM107: IMS Connect Gateway.
//!
//! IMS Connect is a TCP/IP gateway that enables non-z/OS clients to
//! communicate with IMS. This module models the configuration, connection
//! pool, IRM/RSM message formats, and an XML/JSON adapter.

use crate::{ImsError, ImsResult};
use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

/// TLS settings for IMS Connect.
#[derive(Debug, Clone)]
pub struct TlsSettings {
    /// Whether TLS is enabled.
    pub enabled: bool,
    /// Certificate label (key ring entry).
    pub cert_label: String,
}

impl TlsSettings {
    /// Create TLS settings with TLS disabled.
    pub fn disabled() -> Self {
        Self {
            enabled: false,
            cert_label: String::new(),
        }
    }

    /// Create TLS settings with TLS enabled.
    pub fn enabled(cert_label: &str) -> Self {
        Self {
            enabled: true,
            cert_label: cert_label.to_string(),
        }
    }
}

/// IMS Connect configuration.
#[derive(Debug, Clone)]
pub struct ImsConnectConfig {
    /// Listening port.
    pub port: u16,
    /// Host name or IP address.
    pub host: String,
    /// Maximum number of concurrent connections.
    pub max_connections: usize,
    /// TLS configuration.
    pub tls: TlsSettings,
    /// IMS data store name (the IMS system to connect to).
    pub datastore: String,
}

impl ImsConnectConfig {
    /// Create a new configuration.
    pub fn new(host: &str, port: u16, datastore: &str) -> Self {
        Self {
            port,
            host: host.to_string(),
            max_connections: 100,
            tls: TlsSettings::disabled(),
            datastore: datastore.to_string(),
        }
    }

    /// Builder: set maximum connections.
    pub fn with_max_connections(mut self, max: usize) -> Self {
        self.max_connections = max;
        self
    }

    /// Builder: enable TLS.
    pub fn with_tls(mut self, cert_label: &str) -> Self {
        self.tls = TlsSettings::enabled(cert_label);
        self
    }
}

// ---------------------------------------------------------------------------
// Connection pool
// ---------------------------------------------------------------------------

/// State of a pooled connection.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConnectionState {
    /// Available for use.
    Idle,
    /// Currently in use.
    InUse,
    /// Connection is closed / invalid.
    Closed,
}

/// A pooled connection entry.
#[derive(Debug, Clone)]
pub struct PooledConnection {
    /// Unique connection ID.
    pub id: u64,
    /// Current state.
    state: ConnectionState,
    /// Client identifier.
    pub client_id: String,
}

impl PooledConnection {
    /// Return the current state.
    pub fn state(&self) -> ConnectionState {
        self.state
    }
}

/// Manages a pool of client connections.
#[derive(Debug)]
pub struct ConnectionPool {
    /// Maximum pool size.
    pub max_connections: usize,
    /// Pool of connections.
    connections: HashMap<u64, PooledConnection>,
    /// Next connection ID.
    next_id: u64,
}

impl ConnectionPool {
    /// Create a new connection pool.
    pub fn new(max_connections: usize) -> Self {
        Self {
            max_connections,
            connections: HashMap::new(),
            next_id: 1,
        }
    }

    /// Acquire a connection from the pool.
    ///
    /// If an idle connection exists it is reused; otherwise a new one is
    /// created (up to the maximum).
    pub fn acquire(&mut self, client_id: &str) -> ImsResult<u64> {
        // Try to reuse an idle connection
        for conn in self.connections.values_mut() {
            if conn.state == ConnectionState::Idle {
                conn.state = ConnectionState::InUse;
                conn.client_id = client_id.to_string();
                return Ok(conn.id);
            }
        }

        // Create a new connection
        let active = self
            .connections
            .values()
            .filter(|c| c.state != ConnectionState::Closed)
            .count();
        if active >= self.max_connections {
            return Err(ImsError::ConnectionError(
                "Connection pool exhausted".to_string(),
            ));
        }

        let id = self.next_id;
        self.next_id += 1;
        self.connections.insert(
            id,
            PooledConnection {
                id,
                state: ConnectionState::InUse,
                client_id: client_id.to_string(),
            },
        );
        Ok(id)
    }

    /// Release a connection back to the pool.
    pub fn release(&mut self, id: u64) -> ImsResult<()> {
        if let Some(conn) = self.connections.get_mut(&id) {
            conn.state = ConnectionState::Idle;
            conn.client_id.clear();
            Ok(())
        } else {
            Err(ImsError::ConnectionError(format!(
                "Connection {} not found",
                id
            )))
        }
    }

    /// Close and remove a connection.
    pub fn close(&mut self, id: u64) -> ImsResult<()> {
        if let Some(conn) = self.connections.get_mut(&id) {
            conn.state = ConnectionState::Closed;
            Ok(())
        } else {
            Err(ImsError::ConnectionError(format!(
                "Connection {} not found",
                id
            )))
        }
    }

    /// Return the number of active (non-closed) connections.
    pub fn active_count(&self) -> usize {
        self.connections
            .values()
            .filter(|c| c.state != ConnectionState::Closed)
            .count()
    }

    /// Return the number of idle connections.
    pub fn idle_count(&self) -> usize {
        self.connections
            .values()
            .filter(|c| c.state == ConnectionState::Idle)
            .count()
    }
}

// ---------------------------------------------------------------------------
// IRM / RSM messages
// ---------------------------------------------------------------------------

/// IMS Request Message (IRM) -- sent by the client to IMS Connect.
#[derive(Debug, Clone)]
pub struct IrmMessage {
    /// Transaction code.
    pub trancode: String,
    /// Data store name.
    pub datastore: String,
    /// Client ID.
    pub client_id: String,
    /// Message body (application data).
    pub body: Vec<u8>,
}

impl IrmMessage {
    /// Create a new IRM.
    pub fn new(trancode: &str, datastore: &str, client_id: &str, body: Vec<u8>) -> Self {
        Self {
            trancode: trancode.to_string(),
            datastore: datastore.to_string(),
            client_id: client_id.to_string(),
            body,
        }
    }

    /// Serialize the IRM to a byte vector (simplified wire format).
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut buf = Vec::new();
        // Length prefix (4 bytes, big-endian, placeholder)
        buf.extend_from_slice(&[0u8; 4]);
        buf.extend_from_slice(self.trancode.as_bytes());
        buf.push(b' ');
        buf.extend_from_slice(self.datastore.as_bytes());
        buf.push(b' ');
        buf.extend_from_slice(&self.body);
        // Fill in length
        let len = buf.len() as u32;
        buf[..4].copy_from_slice(&len.to_be_bytes());
        buf
    }
}

/// IMS Response Message (RSM) -- returned by IMS Connect to the client.
#[derive(Debug, Clone)]
pub struct RsmMessage {
    /// Return code.
    pub return_code: u32,
    /// Reason code.
    pub reason_code: u32,
    /// Response body (application data).
    pub body: Vec<u8>,
}

impl RsmMessage {
    /// Create a successful RSM.
    pub fn ok(body: Vec<u8>) -> Self {
        Self {
            return_code: 0,
            reason_code: 0,
            body,
        }
    }

    /// Create an error RSM.
    pub fn error(return_code: u32, reason_code: u32) -> Self {
        Self {
            return_code,
            reason_code,
            body: Vec::new(),
        }
    }

    /// Check if the response indicates success.
    pub fn is_ok(&self) -> bool {
        self.return_code == 0
    }
}

// ---------------------------------------------------------------------------
// XML / JSON Adapter
// ---------------------------------------------------------------------------

/// Converts between XML/JSON text and IMS segment format.
#[derive(Debug, Default)]
pub struct XmlJsonAdapter;

impl XmlJsonAdapter {
    /// Create a new adapter.
    pub fn new() -> Self {
        Self
    }

    /// Convert a JSON object to a flat IMS segment byte representation.
    ///
    /// Each key becomes a fixed-length field (padded to 16 bytes), followed
    /// by the value (padded to 64 bytes).
    pub fn json_to_segment(&self, json: &str) -> ImsResult<Vec<u8>> {
        let parsed: serde_json::Value =
            serde_json::from_str(json).map_err(|e| ImsError::IoError(e.to_string()))?;

        let obj = parsed
            .as_object()
            .ok_or_else(|| ImsError::IoError("Expected JSON object".to_string()))?;

        let mut segment = Vec::new();
        for (key, value) in obj {
            let val_str = match value {
                serde_json::Value::String(s) => s.clone(),
                other => other.to_string(),
            };
            // 16-byte key field
            let mut key_field = [b' '; 16];
            let key_bytes = key.as_bytes();
            let copy_len = key_bytes.len().min(16);
            key_field[..copy_len].copy_from_slice(&key_bytes[..copy_len]);
            segment.extend_from_slice(&key_field);

            // 64-byte value field
            let mut val_field = [b' '; 64];
            let val_bytes = val_str.as_bytes();
            let copy_len = val_bytes.len().min(64);
            val_field[..copy_len].copy_from_slice(&val_bytes[..copy_len]);
            segment.extend_from_slice(&val_field);
        }
        Ok(segment)
    }

    /// Convert IMS segment bytes to a JSON string.
    ///
    /// Reverses the encoding from [`json_to_segment`](Self::json_to_segment).
    pub fn segment_to_json(&self, segment: &[u8]) -> ImsResult<String> {
        let record_len = 80; // 16 + 64
        if segment.len() % record_len != 0 {
            return Err(ImsError::IoError(
                "Segment length not a multiple of 80".to_string(),
            ));
        }

        let mut map = serde_json::Map::new();
        for chunk in segment.chunks(record_len) {
            let key = std::str::from_utf8(&chunk[..16])
                .unwrap_or("")
                .trim()
                .to_string();
            let val = std::str::from_utf8(&chunk[16..80])
                .unwrap_or("")
                .trim()
                .to_string();
            if !key.is_empty() {
                map.insert(key, serde_json::Value::String(val));
            }
        }

        serde_json::to_string(&map).map_err(|e| ImsError::IoError(e.to_string()))
    }

    /// Convert a simple XML string to a flat segment.
    ///
    /// Expects `<root><key>value</key>...</root>` structure.
    pub fn xml_to_segment(&self, xml: &str) -> ImsResult<Vec<u8>> {
        // Minimal XML parser: extract <tag>value</tag> pairs
        let mut segment = Vec::new();
        let mut rest = xml;
        // Skip root open tag
        if let Some(pos) = rest.find('>') {
            rest = &rest[pos + 1..];
        }
        while let Some(open_start) = rest.find('<') {
            rest = &rest[open_start + 1..];
            // Check for closing root tag
            if rest.starts_with('/') {
                break;
            }
            let tag_end = match rest.find('>') {
                Some(p) => p,
                None => break,
            };
            let tag = &rest[..tag_end];
            rest = &rest[tag_end + 1..];

            let close_tag = format!("</{}>", tag);
            let val_end = match rest.find(&close_tag) {
                Some(p) => p,
                None => break,
            };
            let value = &rest[..val_end];
            rest = &rest[val_end + close_tag.len()..];

            // Encode as 16+64
            let mut key_field = [b' '; 16];
            let kb = tag.as_bytes();
            let cl = kb.len().min(16);
            key_field[..cl].copy_from_slice(&kb[..cl]);
            segment.extend_from_slice(&key_field);

            let mut val_field = [b' '; 64];
            let vb = value.as_bytes();
            let cl = vb.len().min(64);
            val_field[..cl].copy_from_slice(&vb[..cl]);
            segment.extend_from_slice(&val_field);
        }

        Ok(segment)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // --- Config ---

    #[test]
    fn test_config_new() {
        let cfg = ImsConnectConfig::new("localhost", 9999, "IMS1");
        assert_eq!(cfg.host, "localhost");
        assert_eq!(cfg.port, 9999);
        assert_eq!(cfg.datastore, "IMS1");
        assert!(!cfg.tls.enabled);
    }

    #[test]
    fn test_config_builder() {
        let cfg = ImsConnectConfig::new("host", 8080, "DS1")
            .with_max_connections(50)
            .with_tls("MyCert");
        assert_eq!(cfg.max_connections, 50);
        assert!(cfg.tls.enabled);
        assert_eq!(cfg.tls.cert_label, "MyCert");
    }

    #[test]
    fn test_tls_disabled() {
        let tls = TlsSettings::disabled();
        assert!(!tls.enabled);
    }

    #[test]
    fn test_tls_enabled() {
        let tls = TlsSettings::enabled("CERT1");
        assert!(tls.enabled);
        assert_eq!(tls.cert_label, "CERT1");
    }

    // --- Connection pool ---

    #[test]
    fn test_pool_acquire_release() {
        let mut pool = ConnectionPool::new(10);
        let id = pool.acquire("CLIENT1").unwrap();
        assert_eq!(pool.active_count(), 1);
        assert_eq!(pool.idle_count(), 0);

        pool.release(id).unwrap();
        assert_eq!(pool.idle_count(), 1);
    }

    #[test]
    fn test_pool_reuse_idle() {
        let mut pool = ConnectionPool::new(10);
        let id1 = pool.acquire("C1").unwrap();
        pool.release(id1).unwrap();
        let id2 = pool.acquire("C2").unwrap();
        assert_eq!(id1, id2); // Reused
        assert_eq!(pool.active_count(), 1);
    }

    #[test]
    fn test_pool_exhausted() {
        let mut pool = ConnectionPool::new(2);
        pool.acquire("C1").unwrap();
        pool.acquire("C2").unwrap();
        assert!(pool.acquire("C3").is_err());
    }

    #[test]
    fn test_pool_close() {
        let mut pool = ConnectionPool::new(10);
        let id = pool.acquire("C1").unwrap();
        pool.close(id).unwrap();
        // Closed connections don't count as active
        assert_eq!(pool.active_count(), 0);
    }

    #[test]
    fn test_pool_release_nonexistent() {
        let mut pool = ConnectionPool::new(10);
        assert!(pool.release(999).is_err());
    }

    #[test]
    fn test_pool_close_nonexistent() {
        let mut pool = ConnectionPool::new(10);
        assert!(pool.close(999).is_err());
    }

    // --- IRM / RSM ---

    #[test]
    fn test_irm_message() {
        let irm = IrmMessage::new("TRAN1", "IMS1", "CLIENT1", b"Hello".to_vec());
        assert_eq!(irm.trancode, "TRAN1");
        assert_eq!(irm.body, b"Hello");
    }

    #[test]
    fn test_irm_to_bytes() {
        let irm = IrmMessage::new("T1", "DS1", "C1", b"data".to_vec());
        let bytes = irm.to_bytes();
        // First 4 bytes are the length
        let len = u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
        assert_eq!(len as usize, bytes.len());
    }

    #[test]
    fn test_rsm_ok() {
        let rsm = RsmMessage::ok(b"response".to_vec());
        assert!(rsm.is_ok());
        assert_eq!(rsm.body, b"response");
    }

    #[test]
    fn test_rsm_error() {
        let rsm = RsmMessage::error(8, 4);
        assert!(!rsm.is_ok());
        assert_eq!(rsm.return_code, 8);
        assert_eq!(rsm.reason_code, 4);
    }

    // --- XML/JSON adapter ---

    #[test]
    fn test_json_to_segment_and_back() {
        let adapter = XmlJsonAdapter::new();
        let json = r#"{"NAME":"SMITH","CITY":"NYC"}"#;
        let segment = adapter.json_to_segment(json).unwrap();
        assert_eq!(segment.len(), 160); // 2 fields * 80 bytes

        let result = adapter.segment_to_json(&segment).unwrap();
        assert!(result.contains("SMITH"));
        assert!(result.contains("NYC"));
    }

    #[test]
    fn test_json_to_segment_invalid() {
        let adapter = XmlJsonAdapter::new();
        assert!(adapter.json_to_segment("not json").is_err());
    }

    #[test]
    fn test_segment_to_json_bad_length() {
        let adapter = XmlJsonAdapter::new();
        assert!(adapter.segment_to_json(&[0u8; 17]).is_err());
    }

    #[test]
    fn test_xml_to_segment() {
        let adapter = XmlJsonAdapter::new();
        let xml = "<root><NAME>JONES</NAME><AGE>30</AGE></root>";
        let segment = adapter.xml_to_segment(xml).unwrap();
        assert_eq!(segment.len(), 160);
    }

    #[test]
    fn test_xml_to_segment_empty() {
        let adapter = XmlJsonAdapter::new();
        let segment = adapter.xml_to_segment("<root></root>").unwrap();
        assert!(segment.is_empty());
    }

    #[test]
    fn test_connection_state_values() {
        assert_ne!(ConnectionState::Idle, ConnectionState::InUse);
        assert_ne!(ConnectionState::InUse, ConnectionState::Closed);
    }
}
