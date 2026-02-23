//! Sockets API Compatibility Layer (NET-104).
//!
//! Provides a POSIX-style sockets API abstraction over the internal
//! networking stack. In production this would wrap Tokio; here we
//! simulate the API semantics for correctness testing.

use std::collections::HashMap;
use std::net::{IpAddr, Ipv4Addr, SocketAddr};

use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum SocketError {
    #[error("invalid file descriptor: {0}")]
    InvalidFd(i32),
    #[error("socket not bound")]
    NotBound,
    #[error("socket not listening")]
    NotListening,
    #[error("socket not connected")]
    NotConnected,
    #[error("socket already bound")]
    AlreadyBound,
    #[error("socket already connected")]
    AlreadyConnected,
    #[error("address already in use: {0}")]
    AddressInUse(SocketAddr),
    #[error("connection refused")]
    ConnectionRefused,
    #[error("unsupported address family: {0}")]
    UnsupportedAddressFamily(i32),
    #[error("unsupported socket type: {0}")]
    UnsupportedSocketType(i32),
    #[error("no pending connections")]
    NoPendingConnections,
    #[error("would block")]
    WouldBlock,
    #[error("invalid socket option")]
    InvalidOption,
}

// ---------------------------------------------------------------------------
// NET-104.1 — Socket Creation and Address Binding
// ---------------------------------------------------------------------------

/// Address families.
pub const AF_INET: i32 = 2;
pub const AF_INET6: i32 = 10;

/// Socket types.
pub const SOCK_STREAM: i32 = 1;
pub const SOCK_DGRAM: i32 = 2;

/// Socket option levels.
pub const SOL_SOCKET: i32 = 1;

/// Socket options.
pub const SO_REUSEADDR: i32 = 2;
pub const SO_KEEPALIVE: i32 = 9;
pub const SO_RCVBUF: i32 = 8;
pub const SO_SNDBUF: i32 = 7;

/// Internal socket state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SocketState {
    Created,
    Bound,
    Listening,
    Connected,
    Closed,
}

/// A simulated socket descriptor.
#[derive(Debug)]
struct SocketDesc {
    family: i32,
    sock_type: i32,
    state: SocketState,
    local_addr: Option<SocketAddr>,
    remote_addr: Option<SocketAddr>,
    recv_buffer: Vec<u8>,
    send_buffer: Vec<u8>,
    backlog: u32,
    pending_connections: Vec<i32>,
    options: HashMap<i32, i32>,
    /// For UDP: queued datagrams with source address.
    udp_queue: Vec<(Vec<u8>, SocketAddr)>,
}

/// The socket runtime managing all file descriptors.
#[derive(Debug)]
pub struct SocketRuntime {
    sockets: HashMap<i32, SocketDesc>,
    next_fd: i32,
    bound_addresses: HashMap<SocketAddr, i32>,
}

/// Readiness state for a file descriptor.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Readiness {
    Readable,
    Writable,
    Both,
    None,
}

impl Default for SocketRuntime {
    fn default() -> Self {
        Self::new()
    }
}

impl SocketRuntime {
    pub fn new() -> Self {
        Self {
            sockets: HashMap::new(),
            next_fd: 3, // 0, 1, 2 reserved for stdin/stdout/stderr
            bound_addresses: HashMap::new(),
        }
    }

    /// `socket()` — create a new socket.
    pub fn socket(&mut self, family: i32, sock_type: i32, _protocol: i32) -> Result<i32, SocketError> {
        if family != AF_INET && family != AF_INET6 {
            return Err(SocketError::UnsupportedAddressFamily(family));
        }
        if sock_type != SOCK_STREAM && sock_type != SOCK_DGRAM {
            return Err(SocketError::UnsupportedSocketType(sock_type));
        }
        let fd = self.next_fd;
        self.next_fd += 1;
        self.sockets.insert(
            fd,
            SocketDesc {
                family,
                sock_type,
                state: SocketState::Created,
                local_addr: None,
                remote_addr: None,
                recv_buffer: Vec::new(),
                send_buffer: Vec::new(),
                backlog: 0,
                pending_connections: Vec::new(),
                options: HashMap::new(),
                udp_queue: Vec::new(),
            },
        );
        Ok(fd)
    }

    /// `bind()` — bind a socket to a local address.
    pub fn bind(&mut self, fd: i32, addr: SocketAddr) -> Result<(), SocketError> {
        // Check reuse_addr option
        let reuse = {
            let sock = self.sockets.get(&fd).ok_or(SocketError::InvalidFd(fd))?;
            if sock.state != SocketState::Created {
                return Err(SocketError::AlreadyBound);
            }
            sock.options.get(&SO_REUSEADDR).copied().unwrap_or(0) != 0
        };

        if self.bound_addresses.contains_key(&addr) && !reuse {
            return Err(SocketError::AddressInUse(addr));
        }

        self.bound_addresses.insert(addr, fd);
        let sock = self.sockets.get_mut(&fd).unwrap();
        sock.local_addr = Some(addr);
        sock.state = SocketState::Bound;
        Ok(())
    }

    // NET-104.2 — TCP Server Operations ----------------------------------

    /// `listen()` — mark socket as passive (server).
    pub fn listen(&mut self, fd: i32, backlog: u32) -> Result<(), SocketError> {
        let sock = self.sockets.get_mut(&fd).ok_or(SocketError::InvalidFd(fd))?;
        if sock.state != SocketState::Bound {
            return Err(SocketError::NotBound);
        }
        sock.backlog = backlog;
        sock.state = SocketState::Listening;
        Ok(())
    }

    /// `accept()` — accept a pending connection, returning a new fd.
    pub fn accept(&mut self, fd: i32) -> Result<(i32, SocketAddr), SocketError> {
        let (listen_addr, client_fd) = {
            let sock = self.sockets.get_mut(&fd).ok_or(SocketError::InvalidFd(fd))?;
            if sock.state != SocketState::Listening {
                return Err(SocketError::NotListening);
            }
            let client_fd = sock
                .pending_connections
                .pop()
                .ok_or(SocketError::NoPendingConnections)?;
            (sock.local_addr.unwrap(), client_fd)
        };

        // The client_fd references a connected socket in our simulation
        let remote_addr = self
            .sockets
            .get(&client_fd)
            .and_then(|s| s.local_addr)
            .unwrap_or_else(|| SocketAddr::new(IpAddr::V4(Ipv4Addr::LOCALHOST), 0));

        // Create a new accepted socket
        let new_fd = self.next_fd;
        self.next_fd += 1;
        self.sockets.insert(
            new_fd,
            SocketDesc {
                family: AF_INET,
                sock_type: SOCK_STREAM,
                state: SocketState::Connected,
                local_addr: Some(listen_addr),
                remote_addr: Some(remote_addr),
                recv_buffer: Vec::new(),
                send_buffer: Vec::new(),
                backlog: 0,
                pending_connections: Vec::new(),
                options: HashMap::new(),
                udp_queue: Vec::new(),
            },
        );

        Ok((new_fd, remote_addr))
    }

    // NET-104.3 — TCP Client Operations ----------------------------------

    /// `connect()` — connect to a remote address.
    ///
    /// In simulation, this finds a listening socket on the address and
    /// adds this fd to its pending connection list.
    pub fn connect(&mut self, fd: i32, addr: SocketAddr) -> Result<(), SocketError> {
        {
            let sock = self.sockets.get(&fd).ok_or(SocketError::InvalidFd(fd))?;
            if sock.state == SocketState::Connected {
                return Err(SocketError::AlreadyConnected);
            }
        }

        // Find the listening socket at the target address
        let listener_fd = self
            .bound_addresses
            .get(&addr)
            .copied()
            .ok_or(SocketError::ConnectionRefused)?;

        {
            let listener = self
                .sockets
                .get_mut(&listener_fd)
                .ok_or(SocketError::ConnectionRefused)?;
            if listener.state != SocketState::Listening {
                return Err(SocketError::ConnectionRefused);
            }
            listener.pending_connections.push(fd);
        }

        let sock = self.sockets.get_mut(&fd).unwrap();
        sock.remote_addr = Some(addr);
        sock.state = SocketState::Connected;
        Ok(())
    }

    // NET-104.4 — Data Transfer ------------------------------------------

    /// `send()` — send data on a connected socket.
    pub fn send(&mut self, fd: i32, data: &[u8]) -> Result<usize, SocketError> {
        let sock = self.sockets.get_mut(&fd).ok_or(SocketError::InvalidFd(fd))?;
        if sock.state != SocketState::Connected {
            return Err(SocketError::NotConnected);
        }
        sock.send_buffer.extend_from_slice(data);
        Ok(data.len())
    }

    /// `recv()` — receive data from a connected socket.
    pub fn recv(&mut self, fd: i32, buf_size: usize) -> Result<Vec<u8>, SocketError> {
        let sock = self.sockets.get_mut(&fd).ok_or(SocketError::InvalidFd(fd))?;
        if sock.state != SocketState::Connected {
            return Err(SocketError::NotConnected);
        }
        if sock.recv_buffer.is_empty() {
            return Err(SocketError::WouldBlock);
        }
        let len = buf_size.min(sock.recv_buffer.len());
        let data: Vec<u8> = sock.recv_buffer.drain(..len).collect();
        Ok(data)
    }

    /// `sendto()` — send a datagram to a specific address (UDP).
    pub fn sendto(
        &mut self,
        fd: i32,
        data: &[u8],
        addr: SocketAddr,
    ) -> Result<usize, SocketError> {
        {
            let sock = self.sockets.get(&fd).ok_or(SocketError::InvalidFd(fd))?;
            if sock.sock_type != SOCK_DGRAM {
                return Err(SocketError::NotConnected);
            }
        }

        // Find destination socket and queue the datagram
        if let Some(&dest_fd) = self.bound_addresses.get(&addr) {
            let src_addr = self
                .sockets
                .get(&fd)
                .and_then(|s| s.local_addr)
                .unwrap_or_else(|| SocketAddr::new(IpAddr::V4(Ipv4Addr::LOCALHOST), 0));
            if let Some(dest) = self.sockets.get_mut(&dest_fd) {
                dest.udp_queue.push((data.to_vec(), src_addr));
            }
        }
        Ok(data.len())
    }

    /// `recvfrom()` — receive a datagram with sender address (UDP).
    pub fn recvfrom(&mut self, fd: i32) -> Result<(Vec<u8>, SocketAddr), SocketError> {
        let sock = self.sockets.get_mut(&fd).ok_or(SocketError::InvalidFd(fd))?;
        if sock.sock_type != SOCK_DGRAM {
            return Err(SocketError::NotConnected);
        }
        sock.udp_queue.pop().ok_or(SocketError::WouldBlock)
    }

    /// Inject data into a socket's receive buffer (for simulation).
    pub fn inject_recv_data(&mut self, fd: i32, data: &[u8]) -> Result<(), SocketError> {
        let sock = self.sockets.get_mut(&fd).ok_or(SocketError::InvalidFd(fd))?;
        sock.recv_buffer.extend_from_slice(data);
        Ok(())
    }

    // NET-104.5 — I/O Multiplexing (select/poll) -------------------------

    /// `select()` — check readiness of multiple fds.
    pub fn select(&self, fds: &[i32]) -> Vec<(i32, Readiness)> {
        let mut result = Vec::new();
        for &fd in fds {
            if let Some(sock) = self.sockets.get(&fd) {
                let readable = !sock.recv_buffer.is_empty()
                    || !sock.udp_queue.is_empty()
                    || !sock.pending_connections.is_empty();
                let writable =
                    sock.state == SocketState::Connected || sock.sock_type == SOCK_DGRAM;
                let readiness = match (readable, writable) {
                    (true, true) => Readiness::Both,
                    (true, false) => Readiness::Readable,
                    (false, true) => Readiness::Writable,
                    (false, false) => Readiness::None,
                };
                if readiness != Readiness::None {
                    result.push((fd, readiness));
                }
            }
        }
        result
    }

    /// `poll()` — poll readiness of multiple fds.
    pub fn poll(&self, fds: &[i32]) -> Vec<(i32, Readiness)> {
        self.select(fds) // Same implementation in simulation
    }

    // NET-104.6 — Socket Options and IPv6 ---------------------------------

    /// `setsockopt()` — set a socket option.
    pub fn setsockopt(
        &mut self,
        fd: i32,
        _level: i32,
        optname: i32,
        value: i32,
    ) -> Result<(), SocketError> {
        let sock = self.sockets.get_mut(&fd).ok_or(SocketError::InvalidFd(fd))?;
        sock.options.insert(optname, value);
        Ok(())
    }

    /// `getsockopt()` — get a socket option.
    pub fn getsockopt(&self, fd: i32, _level: i32, optname: i32) -> Result<i32, SocketError> {
        let sock = self.sockets.get(&fd).ok_or(SocketError::InvalidFd(fd))?;
        sock.options
            .get(&optname)
            .copied()
            .ok_or(SocketError::InvalidOption)
    }

    /// Close a socket.
    pub fn close(&mut self, fd: i32) -> Result<(), SocketError> {
        let sock = self.sockets.get_mut(&fd).ok_or(SocketError::InvalidFd(fd))?;
        if let Some(addr) = sock.local_addr {
            self.bound_addresses.remove(&addr);
        }
        sock.state = SocketState::Closed;
        self.sockets.remove(&fd);
        Ok(())
    }

    /// Get the address family of a socket.
    pub fn socket_family(&self, fd: i32) -> Result<i32, SocketError> {
        self.sockets
            .get(&fd)
            .map(|s| s.family)
            .ok_or(SocketError::InvalidFd(fd))
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn localhost_v4(port: u16) -> SocketAddr {
        SocketAddr::new(IpAddr::V4(Ipv4Addr::LOCALHOST), port)
    }

    #[test]
    fn socket_creation_af_inet() {
        let mut rt = SocketRuntime::new();
        let fd = rt.socket(AF_INET, SOCK_STREAM, 0).unwrap();
        assert!(fd >= 3);
    }

    #[test]
    fn socket_creation_af_inet6() {
        let mut rt = SocketRuntime::new();
        let fd = rt.socket(AF_INET6, SOCK_STREAM, 0).unwrap();
        assert_eq!(rt.socket_family(fd).unwrap(), AF_INET6);
    }

    #[test]
    fn socket_unsupported_family() {
        let mut rt = SocketRuntime::new();
        assert!(rt.socket(99, SOCK_STREAM, 0).is_err());
    }

    #[test]
    fn bind_socket() {
        let mut rt = SocketRuntime::new();
        let fd = rt.socket(AF_INET, SOCK_STREAM, 0).unwrap();
        rt.bind(fd, localhost_v4(8080)).unwrap();
    }

    #[test]
    fn bind_duplicate_address_errors() {
        let mut rt = SocketRuntime::new();
        let fd1 = rt.socket(AF_INET, SOCK_STREAM, 0).unwrap();
        let fd2 = rt.socket(AF_INET, SOCK_STREAM, 0).unwrap();
        rt.bind(fd1, localhost_v4(8080)).unwrap();
        assert!(rt.bind(fd2, localhost_v4(8080)).is_err());
    }

    #[test]
    fn listen_and_accept() {
        let mut rt = SocketRuntime::new();
        let server = rt.socket(AF_INET, SOCK_STREAM, 0).unwrap();
        rt.bind(server, localhost_v4(8080)).unwrap();
        rt.listen(server, 128).unwrap();

        let client = rt.socket(AF_INET, SOCK_STREAM, 0).unwrap();
        rt.bind(client, localhost_v4(9090)).unwrap();
        rt.connect(client, localhost_v4(8080)).unwrap();

        let (accepted_fd, _remote) = rt.accept(server).unwrap();
        assert!(accepted_fd > 0);
    }

    #[test]
    fn connect_to_nonexistent_errors() {
        let mut rt = SocketRuntime::new();
        let fd = rt.socket(AF_INET, SOCK_STREAM, 0).unwrap();
        assert!(rt.connect(fd, localhost_v4(9999)).is_err());
    }

    #[test]
    fn send_recv_tcp() {
        let mut rt = SocketRuntime::new();
        let server = rt.socket(AF_INET, SOCK_STREAM, 0).unwrap();
        rt.bind(server, localhost_v4(8080)).unwrap();
        rt.listen(server, 128).unwrap();

        let client = rt.socket(AF_INET, SOCK_STREAM, 0).unwrap();
        rt.bind(client, localhost_v4(9090)).unwrap();
        rt.connect(client, localhost_v4(8080)).unwrap();
        let (accepted, _) = rt.accept(server).unwrap();

        // Send from client
        rt.send(client, b"Hello server").unwrap();
        // Inject into accepted socket's recv buffer (simulating data transfer)
        rt.inject_recv_data(accepted, b"Hello server").unwrap();
        let data = rt.recv(accepted, 1024).unwrap();
        assert_eq!(data, b"Hello server");
    }

    #[test]
    fn recv_empty_would_block() {
        let mut rt = SocketRuntime::new();
        let server = rt.socket(AF_INET, SOCK_STREAM, 0).unwrap();
        rt.bind(server, localhost_v4(8080)).unwrap();
        rt.listen(server, 128).unwrap();

        let client = rt.socket(AF_INET, SOCK_STREAM, 0).unwrap();
        rt.bind(client, localhost_v4(9090)).unwrap();
        rt.connect(client, localhost_v4(8080)).unwrap();
        let (accepted, _) = rt.accept(server).unwrap();
        assert!(rt.recv(accepted, 1024).is_err());
    }

    #[test]
    fn sendto_recvfrom_udp() {
        let mut rt = SocketRuntime::new();
        let s1 = rt.socket(AF_INET, SOCK_DGRAM, 0).unwrap();
        let s2 = rt.socket(AF_INET, SOCK_DGRAM, 0).unwrap();
        rt.bind(s1, localhost_v4(5000)).unwrap();
        rt.bind(s2, localhost_v4(5001)).unwrap();

        rt.sendto(s1, b"UDP datagram", localhost_v4(5001)).unwrap();
        let (data, from) = rt.recvfrom(s2).unwrap();
        assert_eq!(data, b"UDP datagram");
        assert_eq!(from, localhost_v4(5000));
    }

    #[test]
    fn select_reports_readiness() {
        let mut rt = SocketRuntime::new();
        let server = rt.socket(AF_INET, SOCK_STREAM, 0).unwrap();
        rt.bind(server, localhost_v4(8080)).unwrap();
        rt.listen(server, 128).unwrap();

        // Nothing ready yet
        let ready = rt.select(&[server]);
        assert!(ready.is_empty());

        // Add a pending connection
        let client = rt.socket(AF_INET, SOCK_STREAM, 0).unwrap();
        rt.bind(client, localhost_v4(9090)).unwrap();
        rt.connect(client, localhost_v4(8080)).unwrap();

        let ready = rt.select(&[server]);
        assert!(!ready.is_empty());
    }

    #[test]
    fn poll_multiple_fds() {
        let mut rt = SocketRuntime::new();
        let s1 = rt.socket(AF_INET, SOCK_DGRAM, 0).unwrap();
        let s2 = rt.socket(AF_INET, SOCK_DGRAM, 0).unwrap();
        rt.bind(s1, localhost_v4(6000)).unwrap();
        rt.bind(s2, localhost_v4(6001)).unwrap();

        // UDP sockets are writable (SOCK_DGRAM)
        let ready = rt.poll(&[s1, s2]);
        assert_eq!(ready.len(), 2);
    }

    #[test]
    fn setsockopt_getsockopt() {
        let mut rt = SocketRuntime::new();
        let fd = rt.socket(AF_INET, SOCK_STREAM, 0).unwrap();
        rt.setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, 1).unwrap();
        assert_eq!(rt.getsockopt(fd, SOL_SOCKET, SO_REUSEADDR).unwrap(), 1);
    }

    #[test]
    fn so_reuseaddr_allows_rebind() {
        let mut rt = SocketRuntime::new();
        let fd1 = rt.socket(AF_INET, SOCK_STREAM, 0).unwrap();
        rt.bind(fd1, localhost_v4(8080)).unwrap();
        rt.close(fd1).unwrap();

        let fd2 = rt.socket(AF_INET, SOCK_STREAM, 0).unwrap();
        rt.bind(fd2, localhost_v4(8080)).unwrap(); // Should succeed after close
    }

    #[test]
    fn ipv6_socket() {
        let mut rt = SocketRuntime::new();
        let fd = rt.socket(AF_INET6, SOCK_STREAM, 0).unwrap();
        let addr = SocketAddr::new(IpAddr::V6(std::net::Ipv6Addr::LOCALHOST), 8080);
        rt.bind(fd, addr).unwrap();
        assert_eq!(rt.socket_family(fd).unwrap(), AF_INET6);
    }

    #[test]
    fn close_socket() {
        let mut rt = SocketRuntime::new();
        let fd = rt.socket(AF_INET, SOCK_STREAM, 0).unwrap();
        rt.close(fd).unwrap();
        assert!(rt.close(fd).is_err()); // Already closed
    }

    #[test]
    fn getsockopt_unset_errors() {
        let mut rt = SocketRuntime::new();
        let fd = rt.socket(AF_INET, SOCK_STREAM, 0).unwrap();
        assert!(rt.getsockopt(fd, SOL_SOCKET, SO_REUSEADDR).is_err());
    }
}
