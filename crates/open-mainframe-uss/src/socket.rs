//! POSIX Sockets (USS-106).
//!
//! Provides the POSIX socket API:
//! - Socket creation and address binding
//! - TCP server (listen/accept)
//! - TCP client (connect)
//! - send/recv and sendto/recvfrom
//! - select/poll multiplexing
//! - IPv6 and socket options

use std::collections::{HashMap, VecDeque};

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors for socket operations.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, miette::Diagnostic)]
pub enum SocketError {
    /// Bad file descriptor.
    #[error("bad socket descriptor: {fd}")]
    BadDescriptor { fd: u32 },

    /// Address already in use (EADDRINUSE).
    #[error("address already in use: {addr}")]
    AddressInUse { addr: String },

    /// Connection refused (ECONNREFUSED).
    #[error("connection refused: {addr}")]
    ConnectionRefused { addr: String },

    /// Not connected.
    #[error("socket is not connected")]
    NotConnected,

    /// Already connected.
    #[error("socket is already connected")]
    AlreadyConnected,

    /// Invalid argument.
    #[error("invalid argument: {detail}")]
    InvalidArgument { detail: String },

    /// Operation would block.
    #[error("operation would block (EAGAIN)")]
    WouldBlock,

    /// Socket not bound.
    #[error("socket not bound to address")]
    NotBound,

    /// Not listening.
    #[error("socket is not in listening state")]
    NotListening,
}

// ---------------------------------------------------------------------------
//  Address Family
// ---------------------------------------------------------------------------

/// Socket address family.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressFamily {
    /// IPv4 (AF_INET).
    Inet,
    /// IPv6 (AF_INET6).
    Inet6,
    /// UNIX domain (AF_UNIX).
    Unix,
}

impl std::fmt::Display for AddressFamily {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Inet => write!(f, "AF_INET"),
            Self::Inet6 => write!(f, "AF_INET6"),
            Self::Unix => write!(f, "AF_UNIX"),
        }
    }
}

// ---------------------------------------------------------------------------
//  Socket Type
// ---------------------------------------------------------------------------

/// Socket type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SocketType {
    /// Stream (TCP) — SOCK_STREAM.
    Stream,
    /// Datagram (UDP) — SOCK_DGRAM.
    Datagram,
}

// ---------------------------------------------------------------------------
//  Socket Address
// ---------------------------------------------------------------------------

/// A socket address.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SocketAddress {
    /// Address family.
    pub family: AddressFamily,
    /// IP address (as string, e.g. "127.0.0.1" or "::1").
    pub addr: String,
    /// Port number.
    pub port: u16,
}

impl SocketAddress {
    /// Create an IPv4 address.
    pub fn ipv4(addr: &str, port: u16) -> Self {
        Self {
            family: AddressFamily::Inet,
            addr: addr.to_string(),
            port,
        }
    }

    /// Create an IPv6 address.
    pub fn ipv6(addr: &str, port: u16) -> Self {
        Self {
            family: AddressFamily::Inet6,
            addr: addr.to_string(),
            port,
        }
    }
}

impl std::fmt::Display for SocketAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.family {
            AddressFamily::Inet => write!(f, "{}:{}", self.addr, self.port),
            AddressFamily::Inet6 => write!(f, "[{}]:{}", self.addr, self.port),
            AddressFamily::Unix => write!(f, "unix:{}", self.addr),
        }
    }
}

// ---------------------------------------------------------------------------
//  Socket Options
// ---------------------------------------------------------------------------

/// Socket option.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SocketOption {
    /// SO_REUSEADDR.
    ReuseAddr(bool),
    /// SO_KEEPALIVE.
    KeepAlive(bool),
    /// SO_RCVBUF.
    RecvBufSize(u32),
    /// SO_SNDBUF.
    SendBufSize(u32),
    /// TCP_NODELAY.
    TcpNoDelay(bool),
    /// IPV6_V6ONLY.
    Ipv6Only(bool),
}

// ---------------------------------------------------------------------------
//  Socket State
// ---------------------------------------------------------------------------

/// State of a socket.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SocketState {
    /// Just created, not bound.
    Created,
    /// Bound to address.
    Bound,
    /// Listening for connections.
    Listening,
    /// Connecting.
    Connecting,
    /// Connected.
    Connected,
    /// Closed.
    Closed,
}

// ---------------------------------------------------------------------------
//  Poll Event
// ---------------------------------------------------------------------------

/// Events for poll().
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PollEvent {
    /// File descriptor.
    pub fd: u32,
    /// Requested events.
    pub events: PollFlags,
    /// Returned events.
    pub revents: PollFlags,
}

/// Poll flags.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PollFlags {
    /// Data is ready for reading (POLLIN).
    pub pollin: bool,
    /// Writing won't block (POLLOUT).
    pub pollout: bool,
    /// Error condition (POLLERR).
    pub pollerr: bool,
    /// Hung up (POLLHUP).
    pub pollhup: bool,
}

impl PollFlags {
    /// No events.
    pub fn none() -> Self {
        Self {
            pollin: false,
            pollout: false,
            pollerr: false,
            pollhup: false,
        }
    }

    /// Check if any event is set.
    pub fn any(&self) -> bool {
        self.pollin || self.pollout || self.pollerr || self.pollhup
    }
}

// ---------------------------------------------------------------------------
//  Socket
// ---------------------------------------------------------------------------

/// A POSIX socket.
#[derive(Debug)]
pub struct Socket {
    /// Socket file descriptor.
    pub fd: u32,
    /// Address family.
    pub family: AddressFamily,
    /// Socket type.
    pub socket_type: SocketType,
    /// Socket state.
    pub state: SocketState,
    /// Bound address.
    pub local_addr: Option<SocketAddress>,
    /// Peer address (for connected sockets).
    pub peer_addr: Option<SocketAddress>,
    /// Listen backlog.
    pub backlog: u32,
    /// Pending connections (for listening sockets).
    pub pending_connections: VecDeque<u32>,
    /// Receive buffer.
    pub recv_buffer: VecDeque<u8>,
    /// Send buffer.
    pub send_buffer: VecDeque<u8>,
    /// Socket options.
    pub options: Vec<SocketOption>,
    /// Datagram receive queue (for UDP).
    pub datagram_queue: VecDeque<(Vec<u8>, SocketAddress)>,
}

impl Socket {
    /// Create a new socket.
    pub fn new(fd: u32, family: AddressFamily, socket_type: SocketType) -> Self {
        Self {
            fd,
            family,
            socket_type,
            state: SocketState::Created,
            local_addr: None,
            peer_addr: None,
            backlog: 0,
            pending_connections: VecDeque::new(),
            recv_buffer: VecDeque::new(),
            send_buffer: VecDeque::new(),
            options: Vec::new(),
            datagram_queue: VecDeque::new(),
        }
    }
}

// ---------------------------------------------------------------------------
//  Socket Manager
// ---------------------------------------------------------------------------

/// Manages POSIX sockets.
#[derive(Debug)]
pub struct SocketManager {
    /// Socket table.
    sockets: HashMap<u32, Socket>,
    /// Next socket descriptor.
    next_fd: u32,
    /// Bound addresses (for duplicate detection).
    bound_addresses: HashMap<String, u32>,
}

impl SocketManager {
    /// Create a new socket manager.
    pub fn new() -> Self {
        Self {
            sockets: HashMap::new(),
            next_fd: 100, // Start socket fds above normal fds.
            bound_addresses: HashMap::new(),
        }
    }

    /// socket() — create a socket.
    pub fn socket(
        &mut self,
        family: AddressFamily,
        socket_type: SocketType,
    ) -> u32 {
        let fd = self.next_fd;
        self.next_fd += 1;
        let sock = Socket::new(fd, family, socket_type);
        self.sockets.insert(fd, sock);
        fd
    }

    /// bind() — bind a socket to an address.
    pub fn bind(&mut self, fd: u32, addr: SocketAddress) -> Result<(), SocketError> {
        let addr_key = format!("{}:{}", addr.addr, addr.port);

        // Check reuse addr.
        let sock = self
            .sockets
            .get(&fd)
            .ok_or(SocketError::BadDescriptor { fd })?;
        let reuse = sock
            .options
            .iter()
            .any(|o| matches!(o, SocketOption::ReuseAddr(true)));

        if !reuse && self.bound_addresses.contains_key(&addr_key) {
            return Err(SocketError::AddressInUse { addr: addr_key });
        }

        let sock = self
            .sockets
            .get_mut(&fd)
            .ok_or(SocketError::BadDescriptor { fd })?;
        sock.local_addr = Some(addr);
        sock.state = SocketState::Bound;
        self.bound_addresses.insert(addr_key, fd);
        Ok(())
    }

    /// listen() — mark socket as passive (server).
    pub fn listen(&mut self, fd: u32, backlog: u32) -> Result<(), SocketError> {
        let sock = self
            .sockets
            .get_mut(&fd)
            .ok_or(SocketError::BadDescriptor { fd })?;
        if sock.local_addr.is_none() {
            return Err(SocketError::NotBound);
        }
        sock.state = SocketState::Listening;
        sock.backlog = backlog;
        Ok(())
    }

    /// accept() — accept a pending connection.
    pub fn accept(&mut self, fd: u32) -> Result<u32, SocketError> {
        let sock = self
            .sockets
            .get_mut(&fd)
            .ok_or(SocketError::BadDescriptor { fd })?;
        if sock.state != SocketState::Listening {
            return Err(SocketError::NotListening);
        }

        match sock.pending_connections.pop_front() {
            Some(client_fd) => Ok(client_fd),
            None => Err(SocketError::WouldBlock),
        }
    }

    /// Simulate a client connecting to a listening socket.
    pub fn simulate_connection(
        &mut self,
        server_fd: u32,
        client_addr: SocketAddress,
    ) -> Result<u32, SocketError> {
        let server_sock = self
            .sockets
            .get(&server_fd)
            .ok_or(SocketError::BadDescriptor { fd: server_fd })?;
        if server_sock.state != SocketState::Listening {
            return Err(SocketError::NotListening);
        }
        let server_family = server_sock.family;
        let server_type = server_sock.socket_type;

        // Create a new connected socket for this connection.
        let conn_fd = self.socket(server_family, server_type);
        let conn_sock = self.sockets.get_mut(&conn_fd).unwrap();
        conn_sock.state = SocketState::Connected;
        conn_sock.peer_addr = Some(client_addr);

        // Add to server's pending connections.
        let server_sock = self.sockets.get_mut(&server_fd).unwrap();
        server_sock.pending_connections.push_back(conn_fd);
        Ok(conn_fd)
    }

    /// connect() — connect to a remote address.
    pub fn connect(&mut self, fd: u32, addr: SocketAddress) -> Result<(), SocketError> {
        let sock = self
            .sockets
            .get_mut(&fd)
            .ok_or(SocketError::BadDescriptor { fd })?;
        if sock.state == SocketState::Connected {
            return Err(SocketError::AlreadyConnected);
        }
        sock.peer_addr = Some(addr);
        sock.state = SocketState::Connected;
        Ok(())
    }

    /// send() — send data on a connected socket.
    pub fn send(&mut self, fd: u32, data: &[u8]) -> Result<usize, SocketError> {
        let sock = self
            .sockets
            .get_mut(&fd)
            .ok_or(SocketError::BadDescriptor { fd })?;
        if sock.state != SocketState::Connected {
            return Err(SocketError::NotConnected);
        }
        sock.send_buffer.extend(data);
        Ok(data.len())
    }

    /// recv() — receive data from a connected socket.
    pub fn recv(&mut self, fd: u32, count: usize) -> Result<Vec<u8>, SocketError> {
        let sock = self
            .sockets
            .get_mut(&fd)
            .ok_or(SocketError::BadDescriptor { fd })?;
        if sock.state != SocketState::Connected {
            return Err(SocketError::NotConnected);
        }
        if sock.recv_buffer.is_empty() {
            return Err(SocketError::WouldBlock);
        }
        let to_read = std::cmp::min(count, sock.recv_buffer.len());
        let data: Vec<u8> = sock.recv_buffer.drain(..to_read).collect();
        Ok(data)
    }

    /// Deliver data to a socket's receive buffer (for testing).
    pub fn deliver_data(&mut self, fd: u32, data: &[u8]) -> Result<(), SocketError> {
        let sock = self
            .sockets
            .get_mut(&fd)
            .ok_or(SocketError::BadDescriptor { fd })?;
        sock.recv_buffer.extend(data);
        Ok(())
    }

    /// sendto() — send a datagram to a specific address.
    pub fn sendto(
        &mut self,
        fd: u32,
        data: &[u8],
        addr: &SocketAddress,
    ) -> Result<usize, SocketError> {
        let sock = self
            .sockets
            .get(&fd)
            .ok_or(SocketError::BadDescriptor { fd })?;
        if sock.socket_type != SocketType::Datagram {
            return Err(SocketError::InvalidArgument {
                detail: "sendto on non-datagram socket".to_string(),
            });
        }
        let _ = addr;
        Ok(data.len())
    }

    /// recvfrom() — receive a datagram with source address.
    pub fn recvfrom(
        &mut self,
        fd: u32,
    ) -> Result<(Vec<u8>, SocketAddress), SocketError> {
        let sock = self
            .sockets
            .get_mut(&fd)
            .ok_or(SocketError::BadDescriptor { fd })?;
        if sock.socket_type != SocketType::Datagram {
            return Err(SocketError::InvalidArgument {
                detail: "recvfrom on non-datagram socket".to_string(),
            });
        }
        sock.datagram_queue
            .pop_front()
            .ok_or(SocketError::WouldBlock)
    }

    /// Deliver a datagram to a UDP socket (for testing).
    pub fn deliver_datagram(
        &mut self,
        fd: u32,
        data: &[u8],
        from: SocketAddress,
    ) -> Result<(), SocketError> {
        let sock = self
            .sockets
            .get_mut(&fd)
            .ok_or(SocketError::BadDescriptor { fd })?;
        sock.datagram_queue.push_back((data.to_vec(), from));
        Ok(())
    }

    /// setsockopt() — set a socket option.
    pub fn setsockopt(
        &mut self,
        fd: u32,
        option: SocketOption,
    ) -> Result<(), SocketError> {
        let sock = self
            .sockets
            .get_mut(&fd)
            .ok_or(SocketError::BadDescriptor { fd })?;
        // Remove any existing option of the same kind.
        sock.options.retain(|o| {
            std::mem::discriminant(o) != std::mem::discriminant(&option)
        });
        sock.options.push(option);
        Ok(())
    }

    /// poll() — check multiple sockets for I/O readiness.
    pub fn poll(&self, events: &mut [PollEvent]) -> u32 {
        let mut ready = 0;
        for pe in events.iter_mut() {
            pe.revents = PollFlags::none();
            if let Some(sock) = self.sockets.get(&pe.fd) {
                if pe.events.pollin && !sock.recv_buffer.is_empty() {
                    pe.revents.pollin = true;
                }
                if pe.events.pollin && !sock.datagram_queue.is_empty() {
                    pe.revents.pollin = true;
                }
                if pe.events.pollin && !sock.pending_connections.is_empty() {
                    pe.revents.pollin = true;
                }
                if pe.events.pollout {
                    // Writing generally doesn't block.
                    pe.revents.pollout = true;
                }
                if sock.state == SocketState::Closed {
                    pe.revents.pollhup = true;
                }
                if pe.revents.any() {
                    ready += 1;
                }
            }
        }
        ready
    }

    /// select() — simplified wrapper around poll semantics.
    /// Returns the number of ready descriptors.
    pub fn select(
        &self,
        read_fds: &[u32],
        write_fds: &[u32],
    ) -> (Vec<u32>, Vec<u32>) {
        let mut ready_read = Vec::new();
        let mut ready_write = Vec::new();

        for &fd in read_fds {
            if let Some(sock) = self.sockets.get(&fd) {
                if !sock.recv_buffer.is_empty()
                    || !sock.datagram_queue.is_empty()
                    || !sock.pending_connections.is_empty()
                {
                    ready_read.push(fd);
                }
            }
        }

        for &fd in write_fds {
            if self.sockets.contains_key(&fd) {
                ready_write.push(fd);
            }
        }

        (ready_read, ready_write)
    }

    /// Get a socket by fd.
    pub fn get_socket(&self, fd: u32) -> Option<&Socket> {
        self.sockets.get(&fd)
    }

    /// Close a socket.
    pub fn close(&mut self, fd: u32) -> Result<(), SocketError> {
        self.sockets
            .remove(&fd)
            .ok_or(SocketError::BadDescriptor { fd })?;
        Ok(())
    }
}

impl Default for SocketManager {
    fn default() -> Self {
        Self::new()
    }
}

// ===========================================================================
//  Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_socket_creation() {
        let mut mgr = SocketManager::new();
        let fd = mgr.socket(AddressFamily::Inet, SocketType::Stream);
        let sock = mgr.get_socket(fd).unwrap();
        assert_eq!(sock.family, AddressFamily::Inet);
        assert_eq!(sock.socket_type, SocketType::Stream);
        assert_eq!(sock.state, SocketState::Created);
    }

    #[test]
    fn test_bind() {
        let mut mgr = SocketManager::new();
        let fd = mgr.socket(AddressFamily::Inet, SocketType::Stream);
        mgr.bind(fd, SocketAddress::ipv4("0.0.0.0", 8080))
            .unwrap();
        let sock = mgr.get_socket(fd).unwrap();
        assert_eq!(sock.state, SocketState::Bound);
        assert_eq!(sock.local_addr.as_ref().unwrap().port, 8080);
    }

    #[test]
    fn test_bind_address_in_use() {
        let mut mgr = SocketManager::new();
        let fd1 = mgr.socket(AddressFamily::Inet, SocketType::Stream);
        let fd2 = mgr.socket(AddressFamily::Inet, SocketType::Stream);
        mgr.bind(fd1, SocketAddress::ipv4("0.0.0.0", 8080))
            .unwrap();
        let err = mgr
            .bind(fd2, SocketAddress::ipv4("0.0.0.0", 8080))
            .unwrap_err();
        assert!(matches!(err, SocketError::AddressInUse { .. }));
    }

    #[test]
    fn test_listen_and_accept() {
        let mut mgr = SocketManager::new();
        let server = mgr.socket(AddressFamily::Inet, SocketType::Stream);
        mgr.bind(server, SocketAddress::ipv4("0.0.0.0", 8080))
            .unwrap();
        mgr.listen(server, 5).unwrap();

        // Simulate a client connection.
        let _conn = mgr
            .simulate_connection(server, SocketAddress::ipv4("192.168.1.1", 50000))
            .unwrap();

        let accepted = mgr.accept(server).unwrap();
        let conn_sock = mgr.get_socket(accepted).unwrap();
        assert_eq!(conn_sock.state, SocketState::Connected);
        assert_eq!(
            conn_sock.peer_addr.as_ref().unwrap().addr,
            "192.168.1.1"
        );
    }

    #[test]
    fn test_connect() {
        let mut mgr = SocketManager::new();
        let fd = mgr.socket(AddressFamily::Inet, SocketType::Stream);
        mgr.connect(fd, SocketAddress::ipv4("10.0.0.1", 80))
            .unwrap();
        let sock = mgr.get_socket(fd).unwrap();
        assert_eq!(sock.state, SocketState::Connected);
    }

    #[test]
    fn test_send_recv() {
        let mut mgr = SocketManager::new();
        let fd = mgr.socket(AddressFamily::Inet, SocketType::Stream);
        mgr.connect(fd, SocketAddress::ipv4("10.0.0.1", 80))
            .unwrap();

        mgr.send(fd, b"GET / HTTP/1.1").unwrap();
        mgr.deliver_data(fd, b"HTTP/1.1 200 OK").unwrap();

        let data = mgr.recv(fd, 1024).unwrap();
        assert_eq!(&data, b"HTTP/1.1 200 OK");
    }

    #[test]
    fn test_udp_sendto_recvfrom() {
        let mut mgr = SocketManager::new();
        let fd = mgr.socket(AddressFamily::Inet, SocketType::Datagram);
        mgr.bind(fd, SocketAddress::ipv4("0.0.0.0", 5353))
            .unwrap();

        mgr.sendto(fd, b"query", &SocketAddress::ipv4("8.8.8.8", 53))
            .unwrap();

        mgr.deliver_datagram(
            fd,
            b"response",
            SocketAddress::ipv4("8.8.8.8", 53),
        )
        .unwrap();

        let (data, from) = mgr.recvfrom(fd).unwrap();
        assert_eq!(&data, b"response");
        assert_eq!(from.addr, "8.8.8.8");
    }

    #[test]
    fn test_select() {
        let mut mgr = SocketManager::new();
        let fd = mgr.socket(AddressFamily::Inet, SocketType::Stream);
        mgr.connect(fd, SocketAddress::ipv4("10.0.0.1", 80))
            .unwrap();

        // No data yet.
        let (read_ready, _) = mgr.select(&[fd], &[fd]);
        assert!(read_ready.is_empty());

        // Deliver data.
        mgr.deliver_data(fd, b"data").unwrap();
        let (read_ready, write_ready) = mgr.select(&[fd], &[fd]);
        assert_eq!(read_ready, vec![fd]);
        assert_eq!(write_ready, vec![fd]);
    }

    #[test]
    fn test_poll() {
        let mut mgr = SocketManager::new();
        let fd = mgr.socket(AddressFamily::Inet, SocketType::Stream);
        mgr.connect(fd, SocketAddress::ipv4("10.0.0.1", 80))
            .unwrap();
        mgr.deliver_data(fd, b"data").unwrap();

        let mut events = [PollEvent {
            fd,
            events: PollFlags {
                pollin: true,
                pollout: false,
                pollerr: false,
                pollhup: false,
            },
            revents: PollFlags::none(),
        }];
        let ready = mgr.poll(&mut events);
        assert_eq!(ready, 1);
        assert!(events[0].revents.pollin);
    }

    #[test]
    fn test_ipv6_socket() {
        let mut mgr = SocketManager::new();
        let fd = mgr.socket(AddressFamily::Inet6, SocketType::Stream);
        mgr.connect(fd, SocketAddress::ipv6("::1", 443)).unwrap();
        let sock = mgr.get_socket(fd).unwrap();
        assert_eq!(sock.family, AddressFamily::Inet6);
        assert_eq!(sock.state, SocketState::Connected);
    }

    #[test]
    fn test_setsockopt_reuse_addr() {
        let mut mgr = SocketManager::new();
        let fd1 = mgr.socket(AddressFamily::Inet, SocketType::Stream);
        let fd2 = mgr.socket(AddressFamily::Inet, SocketType::Stream);

        mgr.setsockopt(fd1, SocketOption::ReuseAddr(true)).unwrap();
        mgr.setsockopt(fd2, SocketOption::ReuseAddr(true)).unwrap();

        mgr.bind(fd1, SocketAddress::ipv4("0.0.0.0", 9090))
            .unwrap();
        // With SO_REUSEADDR, second bind should succeed.
        mgr.bind(fd2, SocketAddress::ipv4("0.0.0.0", 9090))
            .unwrap();
    }

    #[test]
    fn test_address_display() {
        let addr = SocketAddress::ipv4("127.0.0.1", 8080);
        assert_eq!(addr.to_string(), "127.0.0.1:8080");

        let addr6 = SocketAddress::ipv6("::1", 443);
        assert_eq!(addr6.to_string(), "[::1]:443");
    }

    #[test]
    fn test_accept_no_pending() {
        let mut mgr = SocketManager::new();
        let server = mgr.socket(AddressFamily::Inet, SocketType::Stream);
        mgr.bind(server, SocketAddress::ipv4("0.0.0.0", 7070))
            .unwrap();
        mgr.listen(server, 5).unwrap();

        let err = mgr.accept(server).unwrap_err();
        assert!(matches!(err, SocketError::WouldBlock));
    }

    #[test]
    fn test_send_not_connected() {
        let mut mgr = SocketManager::new();
        let fd = mgr.socket(AddressFamily::Inet, SocketType::Stream);
        let err = mgr.send(fd, b"data").unwrap_err();
        assert!(matches!(err, SocketError::NotConnected));
    }
}
