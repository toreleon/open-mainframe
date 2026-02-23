//! IMS-TM109: Multiple Systems Coupling (MSC) & Shared Queues.
//!
//! MSC enables communication between multiple IMS systems, allowing
//! transactions to be routed to remote IMS instances. Shared queues
//! use a coupling facility to share the IMS message queue across
//! multiple IMS systems in a sysplex.

use crate::{ImsError, ImsResult};
use std::collections::HashMap;

// ---------------------------------------------------------------------------
// MSC Link
// ---------------------------------------------------------------------------

/// Type of MSC link.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LinkType {
    /// CTC (Channel-to-Channel) link.
    Ctc,
    /// VTAM link (SNA).
    Vtam,
    /// TCP/IP link.
    TcpIp,
}

/// State of an MSC link.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LinkState {
    /// Link is inactive.
    Inactive,
    /// Link is active.
    Active,
    /// Link is in error state.
    Error,
}

/// An MSC link between two IMS systems.
#[derive(Debug, Clone)]
pub struct MscLink {
    /// Link name.
    pub name: String,
    /// Link number (1-999).
    pub link_number: u16,
    /// Type of physical link.
    pub link_type: LinkType,
    /// Remote IMS system ID.
    pub remote_sysid: String,
    /// Current state.
    state: LinkState,
    /// Number of messages sent.
    messages_sent: u64,
    /// Number of messages received.
    messages_received: u64,
}

impl MscLink {
    /// Create a new MSC link.
    pub fn new(name: &str, link_number: u16, link_type: LinkType, remote_sysid: &str) -> Self {
        Self {
            name: name.to_string(),
            link_number,
            link_type,
            remote_sysid: remote_sysid.to_string(),
            state: LinkState::Inactive,
            messages_sent: 0,
            messages_received: 0,
        }
    }

    /// Activate the link.
    pub fn activate(&mut self) {
        self.state = LinkState::Active;
    }

    /// Deactivate the link.
    pub fn deactivate(&mut self) {
        self.state = LinkState::Inactive;
    }

    /// Mark the link as in error.
    pub fn set_error(&mut self) {
        self.state = LinkState::Error;
    }

    /// Record a sent message.
    pub fn record_send(&mut self) {
        self.messages_sent += 1;
    }

    /// Record a received message.
    pub fn record_receive(&mut self) {
        self.messages_received += 1;
    }

    /// Return the current state.
    pub fn state(&self) -> LinkState {
        self.state
    }

    /// Return messages-sent count.
    pub fn messages_sent(&self) -> u64 {
        self.messages_sent
    }

    /// Return messages-received count.
    pub fn messages_received(&self) -> u64 {
        self.messages_received
    }
}

// ---------------------------------------------------------------------------
// Remote Routing
// ---------------------------------------------------------------------------

/// Routing entry mapping a transaction to a remote IMS system.
#[derive(Debug, Clone)]
pub struct RoutingEntry {
    /// Transaction code.
    pub trancode: String,
    /// Remote IMS system ID.
    pub remote_sysid: String,
    /// Link number to use.
    pub link_number: u16,
}

/// Routes transactions to remote IMS systems via MSC links.
#[derive(Debug, Default)]
pub struct RemoteRouting {
    /// Routing table: trancode -> entry.
    routes: HashMap<String, RoutingEntry>,
}

impl RemoteRouting {
    /// Create a new empty routing table.
    pub fn new() -> Self {
        Self {
            routes: HashMap::new(),
        }
    }

    /// Add a routing entry.
    pub fn add_route(&mut self, trancode: &str, remote_sysid: &str, link_number: u16) {
        self.routes.insert(
            trancode.to_string(),
            RoutingEntry {
                trancode: trancode.to_string(),
                remote_sysid: remote_sysid.to_string(),
                link_number,
            },
        );
    }

    /// Remove a routing entry.
    pub fn remove_route(&mut self, trancode: &str) -> bool {
        self.routes.remove(trancode).is_some()
    }

    /// Look up the routing entry for a transaction.
    pub fn resolve(&self, trancode: &str) -> Option<&RoutingEntry> {
        self.routes.get(trancode)
    }

    /// Check if a transaction is routed remotely.
    pub fn is_remote(&self, trancode: &str) -> bool {
        self.routes.contains_key(trancode)
    }

    /// Return the number of routing entries.
    pub fn route_count(&self) -> usize {
        self.routes.len()
    }
}

// ---------------------------------------------------------------------------
// Shared Queue
// ---------------------------------------------------------------------------

/// State of a shared queue structure.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SharedQueueState {
    /// Disconnected from coupling facility.
    Disconnected,
    /// Connected and operational.
    Connected,
    /// Connected but in a failed-over state.
    FailedOver,
}

/// A coupling-facility-based shared message queue.
///
/// Shared queues allow multiple IMS systems in a sysplex to share a
/// single message queue, enabling workload balancing and high availability.
#[derive(Debug)]
pub struct SharedQueue {
    /// Structure name in the coupling facility.
    pub structure_name: String,
    /// Current state.
    state: SharedQueueState,
    /// Messages currently in the shared queue.
    messages: Vec<SharedQueueMessage>,
}

/// A message in the shared queue.
#[derive(Debug, Clone)]
pub struct SharedQueueMessage {
    /// Transaction code.
    pub trancode: String,
    /// Message data.
    pub data: Vec<u8>,
    /// Originating IMS system.
    pub origin_sysid: String,
}

impl SharedQueue {
    /// Create a new shared queue.
    pub fn new(structure_name: &str) -> Self {
        Self {
            structure_name: structure_name.to_string(),
            state: SharedQueueState::Disconnected,
            messages: Vec::new(),
        }
    }

    /// Connect to the coupling facility.
    pub fn connect(&mut self) {
        self.state = SharedQueueState::Connected;
    }

    /// Disconnect from the coupling facility.
    pub fn disconnect(&mut self) {
        self.state = SharedQueueState::Disconnected;
    }

    /// Put a message onto the shared queue.
    pub fn put(&mut self, trancode: &str, data: Vec<u8>, origin_sysid: &str) -> ImsResult<()> {
        if self.state != SharedQueueState::Connected {
            return Err(ImsError::ConnectionError(
                "Shared queue not connected".to_string(),
            ));
        }
        self.messages.push(SharedQueueMessage {
            trancode: trancode.to_string(),
            data,
            origin_sysid: origin_sysid.to_string(),
        });
        Ok(())
    }

    /// Get the next message from the shared queue for a given transaction.
    pub fn get(&mut self, trancode: &str) -> ImsResult<Option<SharedQueueMessage>> {
        if self.state != SharedQueueState::Connected {
            return Err(ImsError::ConnectionError(
                "Shared queue not connected".to_string(),
            ));
        }
        if let Some(pos) = self.messages.iter().position(|m| m.trancode == trancode) {
            Ok(Some(self.messages.remove(pos)))
        } else {
            Ok(None)
        }
    }

    /// Return the number of messages in the queue.
    pub fn depth(&self) -> usize {
        self.messages.len()
    }

    /// Return the current state.
    pub fn state(&self) -> SharedQueueState {
        self.state
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // --- MSC Link ---

    #[test]
    fn test_msc_link_new() {
        let link = MscLink::new("LINK1", 1, LinkType::TcpIp, "IMS2");
        assert_eq!(link.name, "LINK1");
        assert_eq!(link.link_number, 1);
        assert_eq!(link.remote_sysid, "IMS2");
        assert_eq!(link.state(), LinkState::Inactive);
    }

    #[test]
    fn test_msc_link_activate() {
        let mut link = MscLink::new("L1", 1, LinkType::Ctc, "REM1");
        link.activate();
        assert_eq!(link.state(), LinkState::Active);
    }

    #[test]
    fn test_msc_link_deactivate() {
        let mut link = MscLink::new("L1", 1, LinkType::Vtam, "REM1");
        link.activate();
        link.deactivate();
        assert_eq!(link.state(), LinkState::Inactive);
    }

    #[test]
    fn test_msc_link_error() {
        let mut link = MscLink::new("L1", 1, LinkType::TcpIp, "REM1");
        link.activate();
        link.set_error();
        assert_eq!(link.state(), LinkState::Error);
    }

    #[test]
    fn test_msc_link_message_counts() {
        let mut link = MscLink::new("L1", 1, LinkType::TcpIp, "REM1");
        link.record_send();
        link.record_send();
        link.record_receive();
        assert_eq!(link.messages_sent(), 2);
        assert_eq!(link.messages_received(), 1);
    }

    // --- Remote Routing ---

    #[test]
    fn test_remote_routing_new() {
        let rr = RemoteRouting::new();
        assert_eq!(rr.route_count(), 0);
    }

    #[test]
    fn test_remote_routing_add_resolve() {
        let mut rr = RemoteRouting::new();
        rr.add_route("TRAN1", "IMS2", 1);
        assert!(rr.is_remote("TRAN1"));
        assert!(!rr.is_remote("TRAN2"));

        let entry = rr.resolve("TRAN1").unwrap();
        assert_eq!(entry.remote_sysid, "IMS2");
        assert_eq!(entry.link_number, 1);
    }

    #[test]
    fn test_remote_routing_remove() {
        let mut rr = RemoteRouting::new();
        rr.add_route("T1", "IMS2", 1);
        assert!(rr.remove_route("T1"));
        assert!(!rr.is_remote("T1"));
        assert!(!rr.remove_route("T1"));
    }

    #[test]
    fn test_remote_routing_resolve_none() {
        let rr = RemoteRouting::new();
        assert!(rr.resolve("NOEXIST").is_none());
    }

    // --- Shared Queue ---

    #[test]
    fn test_shared_queue_new() {
        let sq = SharedQueue::new("STRUCT1");
        assert_eq!(sq.structure_name, "STRUCT1");
        assert_eq!(sq.state(), SharedQueueState::Disconnected);
        assert_eq!(sq.depth(), 0);
    }

    #[test]
    fn test_shared_queue_connect_disconnect() {
        let mut sq = SharedQueue::new("S1");
        sq.connect();
        assert_eq!(sq.state(), SharedQueueState::Connected);
        sq.disconnect();
        assert_eq!(sq.state(), SharedQueueState::Disconnected);
    }

    #[test]
    fn test_shared_queue_put_get() {
        let mut sq = SharedQueue::new("S1");
        sq.connect();
        sq.put("T1", b"msg1".to_vec(), "IMS1").unwrap();
        sq.put("T2", b"msg2".to_vec(), "IMS1").unwrap();
        assert_eq!(sq.depth(), 2);

        let msg = sq.get("T1").unwrap().unwrap();
        assert_eq!(msg.trancode, "T1");
        assert_eq!(msg.data, b"msg1");
        assert_eq!(sq.depth(), 1);
    }

    #[test]
    fn test_shared_queue_get_not_found() {
        let mut sq = SharedQueue::new("S1");
        sq.connect();
        sq.put("T1", b"d".to_vec(), "IMS1").unwrap();
        assert!(sq.get("T2").unwrap().is_none());
    }

    #[test]
    fn test_shared_queue_put_disconnected() {
        let mut sq = SharedQueue::new("S1");
        assert!(sq.put("T1", b"d".to_vec(), "IMS1").is_err());
    }

    #[test]
    fn test_shared_queue_get_disconnected() {
        let mut sq = SharedQueue::new("S1");
        assert!(sq.get("T1").is_err());
    }
}
