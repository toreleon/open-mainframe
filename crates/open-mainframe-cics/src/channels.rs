//! CICS Channel and Container support.
//!
//! Channels are the modern replacement for COMMAREA, supporting data > 32KB.
//! A channel is a named collection of containers; a container holds named
//! byte data. Channels flow with LINK/XCTL/RETURN.
//!
//! # Example
//!
//! ```ignore
//! use open_mainframe_cics::channels::Channel;
//!
//! let mut channel = Channel::new("MY-CHANNEL");
//! channel.put_container("DATA1", b"large payload...")?;
//! let data = channel.get_container("DATA1")?;
//! ```

use std::collections::HashMap;

use crate::{CicsError, CicsResult};

/// A CICS container holding named byte data.
///
/// Containers are the individual data items within a channel.
/// Each container has a name (up to 16 characters) and holds
/// arbitrary byte data with no size limit.
#[derive(Debug, Clone)]
pub struct Container {
    /// Container name.
    pub name: String,
    /// Container data.
    pub data: Vec<u8>,
}

impl Container {
    /// Creates a new container with the given name and data.
    pub fn new(name: impl Into<String>, data: Vec<u8>) -> Self {
        Self {
            name: name.into().to_uppercase(),
            data,
        }
    }

    /// Returns the data length.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns whether the container is empty.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

/// A CICS channel — a named collection of containers.
///
/// Channels are the modern mechanism for passing data between CICS
/// programs, replacing COMMAREA for new applications. A channel can
/// hold multiple containers, each identified by name, with no
/// practical size limit (vs COMMAREA's 32KB).
#[derive(Debug, Clone)]
pub struct Channel {
    /// Channel name (up to 16 characters).
    name: String,
    /// Containers indexed by name.
    containers: HashMap<String, Container>,
}

impl Channel {
    /// Creates a new empty channel.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into().to_uppercase(),
            containers: HashMap::new(),
        }
    }

    /// Returns the channel name.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns the number of containers.
    pub fn container_count(&self) -> usize {
        self.containers.len()
    }

    /// Checks whether a container exists.
    pub fn has_container(&self, name: &str) -> bool {
        self.containers.contains_key(&name.to_uppercase())
    }

    /// PUT CONTAINER — stores data in a named container.
    ///
    /// If the container already exists, its data is replaced.
    pub fn put_container(
        &mut self,
        name: &str,
        data: &[u8],
    ) -> CicsResult<()> {
        let upper = name.to_uppercase();
        let container = Container::new(&upper, data.to_vec());
        self.containers.insert(upper, container);
        Ok(())
    }

    /// GET CONTAINER — retrieves data from a named container.
    ///
    /// Returns CONTAINERERR (CicsResponse::Invreq) if the container
    /// does not exist.
    pub fn get_container(&self, name: &str) -> CicsResult<&[u8]> {
        let upper = name.to_uppercase();
        match self.containers.get(&upper) {
            Some(container) => Ok(&container.data),
            None => Err(CicsError::InvalidRequest(format!(
                "Container '{}' not found in channel '{}'",
                upper, self.name
            ))),
        }
    }

    /// GET CONTAINER — retrieves the container object.
    pub fn get_container_ref(&self, name: &str) -> Option<&Container> {
        self.containers.get(&name.to_uppercase())
    }

    /// DELETE CONTAINER — removes a named container.
    ///
    /// Returns CONTAINERERR if the container does not exist.
    pub fn delete_container(&mut self, name: &str) -> CicsResult<()> {
        let upper = name.to_uppercase();
        if self.containers.remove(&upper).is_some() {
            Ok(())
        } else {
            Err(CicsError::InvalidRequest(format!(
                "Container '{}' not found in channel '{}'",
                upper, self.name
            )))
        }
    }

    /// MOVE CONTAINER — moves a container from this channel to another.
    ///
    /// The container is removed from this channel and added to the
    /// target channel. Returns CONTAINERERR if not found.
    pub fn move_container_to(
        &mut self,
        container_name: &str,
        target: &mut Channel,
    ) -> CicsResult<()> {
        let upper = container_name.to_uppercase();
        match self.containers.remove(&upper) {
            Some(container) => {
                target.containers.insert(upper, container);
                Ok(())
            }
            None => Err(CicsError::InvalidRequest(format!(
                "Container '{}' not found in channel '{}'",
                upper, self.name
            ))),
        }
    }

    /// Returns an iterator over container names.
    pub fn container_names(&self) -> Vec<String> {
        let mut names: Vec<String> = self.containers.keys().cloned().collect();
        names.sort();
        names
    }
}

/// Manages channels for a CICS runtime.
///
/// Provides channel lookup, creation, and deletion across the
/// transaction lifetime.
#[derive(Debug, Default)]
pub struct ChannelManager {
    /// Channels indexed by name.
    channels: HashMap<String, Channel>,
    /// Name of the current channel (set by LINK/XCTL with CHANNEL option).
    current_channel: Option<String>,
}

impl ChannelManager {
    /// Creates a new empty channel manager.
    pub fn new() -> Self {
        Self {
            channels: HashMap::new(),
            current_channel: None,
        }
    }

    /// Returns the current channel name.
    pub fn current_channel_name(&self) -> Option<&str> {
        self.current_channel.as_deref()
    }

    /// Sets the current channel.
    pub fn set_current_channel(&mut self, name: Option<String>) {
        self.current_channel = name.map(|n| n.to_uppercase());
    }

    /// Gets or creates a channel by name.
    pub fn get_or_create(&mut self, name: &str) -> &mut Channel {
        let upper = name.to_uppercase();
        self.channels
            .entry(upper.clone())
            .or_insert_with(|| Channel::new(&upper))
    }

    /// Gets a channel by name.
    pub fn get(&self, name: &str) -> Option<&Channel> {
        self.channels.get(&name.to_uppercase())
    }

    /// Gets a mutable channel by name.
    pub fn get_mut(&mut self, name: &str) -> Option<&mut Channel> {
        self.channels.get_mut(&name.to_uppercase())
    }

    /// Gets the current channel (if set).
    pub fn current_channel(&self) -> Option<&Channel> {
        self.current_channel
            .as_ref()
            .and_then(|name| self.channels.get(name))
    }

    /// Gets the current channel mutably (if set).
    pub fn current_channel_mut(&mut self) -> Option<&mut Channel> {
        // Need to clone the name to avoid borrow issues
        let name = self.current_channel.clone();
        name.and_then(move |n| self.channels.get_mut(&n))
    }

    /// Deletes a channel and all its containers.
    pub fn delete_channel(&mut self, name: &str) -> bool {
        self.channels.remove(&name.to_uppercase()).is_some()
    }

    /// Returns the number of channels.
    pub fn channel_count(&self) -> usize {
        self.channels.len()
    }

    /// PUT CONTAINER — convenience that auto-creates the channel.
    pub fn put_container(
        &mut self,
        channel_name: &str,
        container_name: &str,
        data: &[u8],
    ) -> CicsResult<()> {
        let channel = self.get_or_create(channel_name);
        channel.put_container(container_name, data)
    }

    /// GET CONTAINER — convenience that looks up channel first.
    pub fn get_container(
        &self,
        channel_name: &str,
        container_name: &str,
    ) -> CicsResult<&[u8]> {
        match self.get(channel_name) {
            Some(channel) => channel.get_container(container_name),
            None => Err(CicsError::InvalidRequest(format!(
                "Channel '{}' not found",
                channel_name
            ))),
        }
    }

    /// DELETE CONTAINER — convenience.
    pub fn delete_container(
        &mut self,
        channel_name: &str,
        container_name: &str,
    ) -> CicsResult<()> {
        match self.get_mut(channel_name) {
            Some(channel) => channel.delete_container(container_name),
            None => Err(CicsError::InvalidRequest(format!(
                "Channel '{}' not found",
                channel_name
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // === Story 200.1: Channel and Container Data Structures ===

    #[test]
    fn test_container_creation() {
        let container = Container::new("DATA1", b"hello world".to_vec());
        assert_eq!(container.name, "DATA1");
        assert_eq!(container.data, b"hello world");
        assert_eq!(container.len(), 11);
        assert!(!container.is_empty());
    }

    #[test]
    fn test_channel_put_and_get() {
        let mut channel = Channel::new("MY-CHANNEL");

        // PUT CONTAINER with 100KB of data
        let large_data = vec![0xAA; 100 * 1024];
        channel.put_container("DATA1", &large_data).unwrap();

        // Container is stored and retrievable by name
        let result = channel.get_container("DATA1").unwrap();
        assert_eq!(result.len(), 100 * 1024);
        assert_eq!(result[0], 0xAA);
    }

    #[test]
    fn test_channel_multiple_containers() {
        let mut channel = Channel::new("MY-CHANNEL");
        channel.put_container("DATA1", b"first").unwrap();
        channel.put_container("DATA2", b"second").unwrap();
        channel.put_container("DATA3", b"third").unwrap();

        assert_eq!(channel.container_count(), 3);
        assert!(channel.has_container("DATA1"));
        assert!(channel.has_container("DATA2"));
        assert!(channel.has_container("DATA3"));
    }

    #[test]
    fn test_channel_delete_container() {
        let mut channel = Channel::new("MY-CHANNEL");
        channel.put_container("DATA1", b"first").unwrap();
        channel.put_container("DATA2", b"second").unwrap();

        // DELETE CONTAINER "DATA1"
        channel.delete_container("DATA1").unwrap();

        // Only DATA1 is removed; DATA2 remains
        assert!(!channel.has_container("DATA1"));
        assert!(channel.has_container("DATA2"));
        assert_eq!(channel.container_count(), 1);
    }

    #[test]
    fn test_channel_get_nonexistent_container() {
        let channel = Channel::new("MY-CHANNEL");
        let result = channel.get_container("NOPE");
        assert!(result.is_err());
    }

    #[test]
    fn test_channel_delete_nonexistent_container() {
        let mut channel = Channel::new("MY-CHANNEL");
        let result = channel.delete_container("NOPE");
        assert!(result.is_err());
    }

    #[test]
    fn test_channel_put_replaces_existing() {
        let mut channel = Channel::new("MY-CHANNEL");
        channel.put_container("DATA1", b"version 1").unwrap();
        channel.put_container("DATA1", b"version 2").unwrap();

        let result = channel.get_container("DATA1").unwrap();
        assert_eq!(result, b"version 2");
        assert_eq!(channel.container_count(), 1);
    }

    #[test]
    fn test_channel_case_insensitive() {
        let mut channel = Channel::new("my-channel");
        channel.put_container("data1", b"value").unwrap();

        assert!(channel.has_container("DATA1"));
        assert_eq!(channel.name(), "MY-CHANNEL");
    }

    #[test]
    fn test_channel_move_container() {
        let mut source = Channel::new("SOURCE");
        let mut target = Channel::new("TARGET");

        source.put_container("DATA1", b"payload").unwrap();
        source.move_container_to("DATA1", &mut target).unwrap();

        assert!(!source.has_container("DATA1"));
        assert!(target.has_container("DATA1"));
        assert_eq!(target.get_container("DATA1").unwrap(), b"payload");
    }

    #[test]
    fn test_channel_container_names() {
        let mut channel = Channel::new("CH");
        channel.put_container("BETA", b"b").unwrap();
        channel.put_container("ALPHA", b"a").unwrap();
        channel.put_container("GAMMA", b"g").unwrap();

        let names = channel.container_names();
        assert_eq!(names, vec!["ALPHA", "BETA", "GAMMA"]);
    }

    // === Channel Manager Tests ===

    #[test]
    fn test_channel_manager_put_get() {
        let mut mgr = ChannelManager::new();

        mgr.put_container("MY-CHANNEL", "DATA1", b"hello").unwrap();

        let data = mgr.get_container("MY-CHANNEL", "DATA1").unwrap();
        assert_eq!(data, b"hello");
    }

    #[test]
    fn test_channel_manager_current_channel() {
        let mut mgr = ChannelManager::new();
        mgr.put_container("CH1", "D1", b"data").unwrap();
        mgr.set_current_channel(Some("CH1".to_string()));

        assert_eq!(mgr.current_channel_name(), Some("CH1"));
        let ch = mgr.current_channel().unwrap();
        assert_eq!(ch.name(), "CH1");
    }

    #[test]
    fn test_channel_manager_delete_channel() {
        let mut mgr = ChannelManager::new();
        mgr.put_container("CH1", "D1", b"data").unwrap();

        assert!(mgr.delete_channel("CH1"));
        assert_eq!(mgr.channel_count(), 0);
    }

    #[test]
    fn test_channel_manager_get_nonexistent() {
        let mgr = ChannelManager::new();
        let result = mgr.get_container("NOPE", "DATA");
        assert!(result.is_err());
    }

    #[test]
    fn test_channel_large_data() {
        let mut channel = Channel::new("BIG");

        // Test with data > 32KB (COMMAREA limit)
        let big_data = vec![0x42; 1024 * 1024]; // 1MB
        channel.put_container("BIGDATA", &big_data).unwrap();

        let result = channel.get_container("BIGDATA").unwrap();
        assert_eq!(result.len(), 1024 * 1024);
    }
}
