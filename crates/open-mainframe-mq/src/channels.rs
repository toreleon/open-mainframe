//! MQ Channel definitions and management.
//!
//! Implements:
//! - **Channel types** — SDR, RCVR, SVR, RQSTR, SVRCONN, CLNTCONN, CLUSSDR, CLUSRCVR, AMQP
//! - **Channel status** — BINDING, STARTING, RUNNING, STOPPING, STOPPED, RETRYING, INACTIVE
//! - **Channel authentication records (CHLAUTH)**
//! - **Channel attributes** — heartbeat, batch size, compression, SSL/TLS

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Channel types
// ---------------------------------------------------------------------------

/// MQ channel type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ChannelType {
    /// Sender — initiates connection, sends from XMITQ.
    Sender,
    /// Receiver — accepts connection, puts to destination queue.
    Receiver,
    /// Server — sends messages (can be triggered or started by requester).
    Server,
    /// Requester — requests server to start sending.
    Requester,
    /// Server-connection — queue manager end of client connection.
    ServerConnection,
    /// Client-connection — client end of client connection.
    ClientConnection,
    /// Cluster-sender — sends cluster info and messages to full repository.
    ClusterSender,
    /// Cluster-receiver — receives from cluster members.
    ClusterReceiver,
    /// AMQP — accepts AMQP 1.0 client connections.
    Amqp,
}

/// Channel status.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ChannelStatus {
    /// Channel is inactive (not started).
    Inactive,
    /// Channel is binding (establishing connection).
    Binding,
    /// Channel is starting.
    Starting,
    /// Channel is running (transferring messages).
    Running,
    /// Channel is stopping.
    Stopping,
    /// Channel is stopped.
    Stopped,
    /// Channel is retrying after failure.
    Retrying,
}

impl Default for ChannelStatus {
    fn default() -> Self {
        Self::Inactive
    }
}

/// Transmission protocol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TransportType {
    /// TCP/IP.
    Tcp,
    /// LU 6.2 (SNA).
    Lu62,
}

impl Default for TransportType {
    fn default() -> Self {
        Self::Tcp
    }
}

/// Compression method for channel data.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Compression {
    /// No compression.
    None,
    /// Fast zlib compression.
    ZlibFast,
    /// High zlib compression.
    ZlibHigh,
}

impl Default for Compression {
    fn default() -> Self {
        Self::None
    }
}

// ---------------------------------------------------------------------------
//  Channel definition
// ---------------------------------------------------------------------------

/// An MQ channel definition.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChannelDefinition {
    /// Channel name (max 20 chars).
    pub name: String,
    /// Channel type.
    pub channel_type: ChannelType,
    /// Description.
    pub description: String,
    /// Connection name (host/port for TCP).
    pub connection_name: String,
    /// Transmission queue name (for sender/server channels).
    pub xmit_queue: String,
    /// Target queue manager name.
    pub target_qmgr: String,
    /// Transport type.
    pub transport_type: TransportType,
    /// Heartbeat interval in seconds (0 = disabled).
    pub heartbeat_interval: u32,
    /// Batch size (number of messages per batch).
    pub batch_size: u32,
    /// Maximum message length.
    pub max_msg_length: u32,
    /// Short retry count.
    pub short_retry_count: u32,
    /// Short retry interval in seconds.
    pub short_retry_interval: u32,
    /// Long retry count.
    pub long_retry_count: u32,
    /// Long retry interval in seconds.
    pub long_retry_interval: u32,
    /// Data compression.
    pub compression: Compression,
    /// SSL/TLS cipher spec.
    pub ssl_cipher_spec: String,
    /// SSL/TLS peer name for DN matching.
    pub ssl_peer_name: String,
    /// Current status.
    pub status: ChannelStatus,
    /// Message sequence number.
    pub msg_sequence_number: u64,
}

impl Default for ChannelDefinition {
    fn default() -> Self {
        Self {
            name: String::new(),
            channel_type: ChannelType::Sender,
            description: String::new(),
            connection_name: String::new(),
            xmit_queue: String::new(),
            target_qmgr: String::new(),
            transport_type: TransportType::Tcp,
            heartbeat_interval: 300,
            batch_size: 50,
            max_msg_length: 4_194_304,
            short_retry_count: 10,
            short_retry_interval: 60,
            long_retry_count: 999_999_999,
            long_retry_interval: 1200,
            compression: Compression::None,
            ssl_cipher_spec: String::new(),
            ssl_peer_name: String::new(),
            status: ChannelStatus::Inactive,
            msg_sequence_number: 1,
        }
    }
}

// ---------------------------------------------------------------------------
//  Channel Authentication Record (CHLAUTH)
// ---------------------------------------------------------------------------

/// CHLAUTH rule type.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ChlauthRuleType {
    /// Block access by IP address.
    BlockIp { address: String },
    /// Block access by user ID.
    BlockUser { user_id: String },
    /// Map SSL DN to MCAUSER.
    SslPeerMap { ssl_peer: String, mca_user: String },
    /// Map IP address to MCAUSER.
    AddressMap { address: String, mca_user: String },
    /// Map queue manager name to MCAUSER.
    QmgrMap { qmgr_name: String, mca_user: String },
}

/// A channel authentication record.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChlauthRecord {
    /// Channel name pattern.
    pub channel_name: String,
    /// Rule type and parameters.
    pub rule: ChlauthRuleType,
    /// Whether this rule is enabled.
    pub enabled: bool,
    /// Description.
    pub description: String,
}

// ---------------------------------------------------------------------------
//  Channel Manager
// ---------------------------------------------------------------------------

/// Manages channel definitions and CHLAUTH records.
#[derive(Debug, Default)]
pub struct ChannelManager {
    channels: HashMap<String, ChannelDefinition>,
    chlauth_records: Vec<ChlauthRecord>,
}

impl ChannelManager {
    pub fn new() -> Self {
        Self::default()
    }

    /// Define a new channel.
    pub fn define(&mut self, def: ChannelDefinition) -> Result<(), String> {
        let name = def.name.to_uppercase();
        if self.channels.contains_key(&name) {
            return Err(format!("Channel already exists: {name}"));
        }
        self.channels.insert(name, def);
        Ok(())
    }

    /// Alter an existing channel's attributes.
    pub fn alter(
        &mut self,
        name: &str,
        updates: &HashMap<String, String>,
    ) -> Result<(), String> {
        let upper = name.to_uppercase();
        let ch = self
            .channels
            .get_mut(&upper)
            .ok_or_else(|| format!("Channel not found: {upper}"))?;

        if let Some(desc) = updates.get("DESCR") {
            ch.description = desc.clone();
        }
        if let Some(conn) = updates.get("CONNAME") {
            ch.connection_name = conn.clone();
        }
        if let Some(xmitq) = updates.get("XMITQ") {
            ch.xmit_queue = xmitq.clone();
        }
        if let Some(hb) = updates.get("HBINT") {
            if let Ok(v) = hb.parse::<u32>() {
                ch.heartbeat_interval = v;
            }
        }
        if let Some(bs) = updates.get("BATCHSZ") {
            if let Ok(v) = bs.parse::<u32>() {
                ch.batch_size = v;
            }
        }
        Ok(())
    }

    /// Delete a channel.
    pub fn delete(&mut self, name: &str) -> Result<(), String> {
        let upper = name.to_uppercase();
        self.channels
            .remove(&upper)
            .map(|_| ())
            .ok_or_else(|| format!("Channel not found: {upper}"))
    }

    /// Get a channel definition.
    pub fn get(&self, name: &str) -> Option<&ChannelDefinition> {
        self.channels.get(&name.to_uppercase())
    }

    /// List all channel names.
    pub fn list(&self) -> Vec<String> {
        self.channels.keys().cloned().collect()
    }

    /// Start a channel (set status to Running).
    pub fn start(&mut self, name: &str) -> Result<(), String> {
        let upper = name.to_uppercase();
        let ch = self
            .channels
            .get_mut(&upper)
            .ok_or_else(|| format!("Channel not found: {upper}"))?;
        ch.status = ChannelStatus::Running;
        Ok(())
    }

    /// Stop a channel.
    pub fn stop(&mut self, name: &str) -> Result<(), String> {
        let upper = name.to_uppercase();
        let ch = self
            .channels
            .get_mut(&upper)
            .ok_or_else(|| format!("Channel not found: {upper}"))?;
        ch.status = ChannelStatus::Stopped;
        Ok(())
    }

    /// Ping a channel (returns true if channel definition exists).
    pub fn ping(&self, name: &str) -> bool {
        self.channels.contains_key(&name.to_uppercase())
    }

    /// Reset channel sequence number.
    pub fn reset_sequence(&mut self, name: &str, seq: u64) -> Result<(), String> {
        let upper = name.to_uppercase();
        let ch = self
            .channels
            .get_mut(&upper)
            .ok_or_else(|| format!("Channel not found: {upper}"))?;
        ch.msg_sequence_number = seq;
        Ok(())
    }

    /// Add a CHLAUTH record.
    pub fn add_chlauth(&mut self, record: ChlauthRecord) {
        self.chlauth_records.push(record);
    }

    /// List CHLAUTH records for a channel.
    pub fn get_chlauth(&self, channel: &str) -> Vec<&ChlauthRecord> {
        let upper = channel.to_uppercase();
        self.chlauth_records
            .iter()
            .filter(|r| r.channel_name.to_uppercase() == upper || r.channel_name == "*")
            .collect()
    }

    /// Check if a connection is allowed by CHLAUTH rules.
    pub fn check_chlauth(&self, channel: &str, ip: &str, user: &str) -> bool {
        let records = self.get_chlauth(channel);
        for record in &records {
            if !record.enabled {
                continue;
            }
            match &record.rule {
                ChlauthRuleType::BlockIp { address } if address == ip => return false,
                ChlauthRuleType::BlockUser { user_id } if user_id == user => return false,
                _ => {}
            }
        }
        true
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn make_sender(name: &str) -> ChannelDefinition {
        ChannelDefinition {
            name: name.to_string(),
            channel_type: ChannelType::Sender,
            connection_name: "remotehost(1414)".to_string(),
            xmit_queue: "XMIT.Q".to_string(),
            ..Default::default()
        }
    }

    #[test]
    fn test_define_channel() {
        let mut mgr = ChannelManager::new();
        mgr.define(make_sender("TO.REMOTE")).unwrap();
        assert!(mgr.get("TO.REMOTE").is_some());
    }

    #[test]
    fn test_define_duplicate() {
        let mut mgr = ChannelManager::new();
        mgr.define(make_sender("CH1")).unwrap();
        assert!(mgr.define(make_sender("CH1")).is_err());
    }

    #[test]
    fn test_delete_channel() {
        let mut mgr = ChannelManager::new();
        mgr.define(make_sender("CH1")).unwrap();
        mgr.delete("CH1").unwrap();
        assert!(mgr.get("CH1").is_none());
    }

    #[test]
    fn test_start_stop() {
        let mut mgr = ChannelManager::new();
        mgr.define(make_sender("CH1")).unwrap();

        mgr.start("CH1").unwrap();
        assert_eq!(mgr.get("CH1").unwrap().status, ChannelStatus::Running);

        mgr.stop("CH1").unwrap();
        assert_eq!(mgr.get("CH1").unwrap().status, ChannelStatus::Stopped);
    }

    #[test]
    fn test_alter_channel() {
        let mut mgr = ChannelManager::new();
        mgr.define(make_sender("CH1")).unwrap();

        let mut updates = HashMap::new();
        updates.insert("HBINT".to_string(), "60".to_string());
        updates.insert("DESCR".to_string(), "Updated channel".to_string());
        mgr.alter("CH1", &updates).unwrap();

        let ch = mgr.get("CH1").unwrap();
        assert_eq!(ch.heartbeat_interval, 60);
        assert_eq!(ch.description, "Updated channel");
    }

    #[test]
    fn test_ping_channel() {
        let mut mgr = ChannelManager::new();
        mgr.define(make_sender("CH1")).unwrap();
        assert!(mgr.ping("CH1"));
        assert!(!mgr.ping("NONEXIST"));
    }

    #[test]
    fn test_reset_sequence() {
        let mut mgr = ChannelManager::new();
        mgr.define(make_sender("CH1")).unwrap();
        mgr.reset_sequence("CH1", 100).unwrap();
        assert_eq!(mgr.get("CH1").unwrap().msg_sequence_number, 100);
    }

    #[test]
    fn test_channel_types() {
        let mut mgr = ChannelManager::new();

        let svrconn = ChannelDefinition {
            name: "SYSTEM.DEF.SVRCONN".to_string(),
            channel_type: ChannelType::ServerConnection,
            ..Default::default()
        };
        mgr.define(svrconn).unwrap();

        let clusrcvr = ChannelDefinition {
            name: "TO.CLUSTER".to_string(),
            channel_type: ChannelType::ClusterReceiver,
            ..Default::default()
        };
        mgr.define(clusrcvr).unwrap();

        assert_eq!(mgr.list().len(), 2);
    }

    #[test]
    fn test_chlauth_block_ip() {
        let mut mgr = ChannelManager::new();
        mgr.define(make_sender("CH1")).unwrap();

        mgr.add_chlauth(ChlauthRecord {
            channel_name: "CH1".to_string(),
            rule: ChlauthRuleType::BlockIp {
                address: "192.168.1.100".to_string(),
            },
            enabled: true,
            description: "Block bad IP".to_string(),
        });

        assert!(!mgr.check_chlauth("CH1", "192.168.1.100", "user1"));
        assert!(mgr.check_chlauth("CH1", "10.0.0.1", "user1"));
    }

    #[test]
    fn test_chlauth_block_user() {
        let mut mgr = ChannelManager::new();
        mgr.define(make_sender("CH1")).unwrap();

        mgr.add_chlauth(ChlauthRecord {
            channel_name: "CH1".to_string(),
            rule: ChlauthRuleType::BlockUser {
                user_id: "baduser".to_string(),
            },
            enabled: true,
            description: "Block user".to_string(),
        });

        assert!(!mgr.check_chlauth("CH1", "10.0.0.1", "baduser"));
        assert!(mgr.check_chlauth("CH1", "10.0.0.1", "gooduser"));
    }

    #[test]
    fn test_chlauth_disabled_rule() {
        let mut mgr = ChannelManager::new();
        mgr.define(make_sender("CH1")).unwrap();

        mgr.add_chlauth(ChlauthRecord {
            channel_name: "CH1".to_string(),
            rule: ChlauthRuleType::BlockIp {
                address: "192.168.1.100".to_string(),
            },
            enabled: false,
            description: "Disabled rule".to_string(),
        });

        // Rule is disabled, so connection should be allowed.
        assert!(mgr.check_chlauth("CH1", "192.168.1.100", "user1"));
    }
}
