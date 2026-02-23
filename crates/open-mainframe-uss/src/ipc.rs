//! IPC Mechanisms (USS-105).
//!
//! Provides UNIX inter-process communication:
//! - pipe() for unidirectional data flow
//! - FIFO (named pipes) via mkfifo
//! - POSIX message queues
//! - Shared memory
//! - POSIX semaphores

use std::collections::{HashMap, VecDeque};

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors for IPC operations.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, miette::Diagnostic)]
pub enum IpcError {
    /// Pipe is closed / broken pipe (EPIPE).
    #[error("broken pipe")]
    BrokenPipe,

    /// Would block (EAGAIN).
    #[error("operation would block (EAGAIN)")]
    WouldBlock,

    /// Name already exists.
    #[error("IPC name already exists: {name}")]
    AlreadyExists { name: String },

    /// Name not found.
    #[error("IPC name not found: {name}")]
    NotFound { name: String },

    /// Message too large.
    #[error("message too large: {size} > max {max}")]
    MessageTooLarge { size: usize, max: usize },

    /// Queue is full.
    #[error("message queue is full")]
    QueueFull,

    /// Invalid semaphore value.
    #[error("semaphore would block (value is 0)")]
    SemaphoreWouldBlock,

    /// Bad file descriptor.
    #[error("bad file descriptor: {fd}")]
    BadFileDescriptor { fd: u32 },
}

// ---------------------------------------------------------------------------
//  Pipe
// ---------------------------------------------------------------------------

/// A UNIX pipe for unidirectional data flow.
#[derive(Debug)]
pub struct Pipe {
    /// Pipe ID.
    pub id: u32,
    /// Internal buffer.
    buffer: VecDeque<u8>,
    /// Maximum buffer size.
    capacity: usize,
    /// Whether the read end is open.
    pub read_open: bool,
    /// Whether the write end is open.
    pub write_open: bool,
}

/// File descriptor pair from pipe().
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PipeFds {
    /// Read end file descriptor.
    pub read_fd: u32,
    /// Write end file descriptor.
    pub write_fd: u32,
}

impl Pipe {
    /// Create a new pipe with the given capacity.
    pub fn new(id: u32, capacity: usize) -> Self {
        Self {
            id,
            buffer: VecDeque::new(),
            capacity,
            read_open: true,
            write_open: true,
        }
    }

    /// Write data to the pipe.
    pub fn write(&mut self, data: &[u8]) -> Result<usize, IpcError> {
        if !self.read_open {
            return Err(IpcError::BrokenPipe);
        }
        let available = self.capacity - self.buffer.len();
        let to_write = std::cmp::min(data.len(), available);
        if to_write == 0 && !data.is_empty() {
            return Err(IpcError::WouldBlock);
        }
        self.buffer.extend(&data[..to_write]);
        Ok(to_write)
    }

    /// Read data from the pipe.
    pub fn read(&mut self, count: usize) -> Result<Vec<u8>, IpcError> {
        if self.buffer.is_empty() {
            if !self.write_open {
                return Ok(Vec::new()); // EOF
            }
            return Err(IpcError::WouldBlock);
        }
        let to_read = std::cmp::min(count, self.buffer.len());
        let data: Vec<u8> = self.buffer.drain(..to_read).collect();
        Ok(data)
    }

    /// Close the write end.
    pub fn close_write(&mut self) {
        self.write_open = false;
    }

    /// Close the read end.
    pub fn close_read(&mut self) {
        self.read_open = false;
    }

    /// Check if the pipe has data.
    pub fn has_data(&self) -> bool {
        !self.buffer.is_empty()
    }

    /// Check if EOF (write closed and no data).
    pub fn is_eof(&self) -> bool {
        !self.write_open && self.buffer.is_empty()
    }
}

// ---------------------------------------------------------------------------
//  FIFO (Named Pipe)
// ---------------------------------------------------------------------------

/// A FIFO (named pipe) in the file system.
#[derive(Debug)]
pub struct Fifo {
    /// Name in the filesystem.
    pub name: String,
    /// Permission mode.
    pub mode: u32,
    /// Internal pipe.
    pub pipe: Pipe,
    /// Number of readers.
    pub readers: u32,
    /// Number of writers.
    pub writers: u32,
}

impl Fifo {
    /// Create a new FIFO.
    pub fn new(name: &str, mode: u32, pipe_id: u32) -> Self {
        Self {
            name: name.to_string(),
            mode,
            pipe: Pipe::new(pipe_id, 65536),
            readers: 0,
            writers: 0,
        }
    }

    /// Open for reading.
    pub fn open_read(&mut self) {
        self.readers += 1;
        self.pipe.read_open = true;
    }

    /// Open for writing.
    pub fn open_write(&mut self) {
        self.writers += 1;
        self.pipe.write_open = true;
    }

    /// Close a reader.
    pub fn close_read(&mut self) {
        self.readers = self.readers.saturating_sub(1);
        if self.readers == 0 {
            self.pipe.read_open = false;
        }
    }

    /// Close a writer.
    pub fn close_write(&mut self) {
        self.writers = self.writers.saturating_sub(1);
        if self.writers == 0 {
            self.pipe.write_open = false;
        }
    }
}

// ---------------------------------------------------------------------------
//  Message Queue
// ---------------------------------------------------------------------------

/// A message in a POSIX message queue.
#[derive(Debug, Clone)]
pub struct Message {
    /// Message data.
    pub data: Vec<u8>,
    /// Priority (higher = delivered first).
    pub priority: u32,
}

/// Attributes for a message queue.
#[derive(Debug, Clone)]
pub struct MqAttr {
    /// Max number of messages.
    pub max_messages: usize,
    /// Max message size.
    pub max_msg_size: usize,
}

impl Default for MqAttr {
    fn default() -> Self {
        Self {
            max_messages: 10,
            max_msg_size: 8192,
        }
    }
}

/// A POSIX message queue.
#[derive(Debug)]
pub struct MessageQueue {
    /// Queue name.
    pub name: String,
    /// Queue attributes.
    pub attrs: MqAttr,
    /// Messages (sorted by priority on receive).
    messages: Vec<Message>,
    /// Permission mode.
    pub mode: u32,
}

impl MessageQueue {
    /// Create a new message queue.
    pub fn new(name: &str, mode: u32, attrs: MqAttr) -> Self {
        Self {
            name: name.to_string(),
            attrs,
            messages: Vec::new(),
            mode,
        }
    }

    /// mq_send — send a message.
    pub fn send(&mut self, data: &[u8], priority: u32) -> Result<(), IpcError> {
        if data.len() > self.attrs.max_msg_size {
            return Err(IpcError::MessageTooLarge {
                size: data.len(),
                max: self.attrs.max_msg_size,
            });
        }
        if self.messages.len() >= self.attrs.max_messages {
            return Err(IpcError::QueueFull);
        }
        self.messages.push(Message {
            data: data.to_vec(),
            priority,
        });
        Ok(())
    }

    /// mq_receive — receive the highest priority message.
    pub fn receive(&mut self) -> Result<Message, IpcError> {
        if self.messages.is_empty() {
            return Err(IpcError::WouldBlock);
        }
        // Find highest priority message.
        let max_idx = self
            .messages
            .iter()
            .enumerate()
            .max_by_key(|(_, m)| m.priority)
            .map(|(i, _)| i)
            .unwrap();
        Ok(self.messages.remove(max_idx))
    }

    /// Current queue length.
    pub fn len(&self) -> usize {
        self.messages.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.messages.is_empty()
    }
}

// ---------------------------------------------------------------------------
//  Shared Memory
// ---------------------------------------------------------------------------

/// A POSIX shared memory segment.
#[derive(Debug)]
pub struct SharedMemory {
    /// Name.
    pub name: String,
    /// Permission mode.
    pub mode: u32,
    /// Size in bytes.
    pub size: usize,
    /// Data.
    pub data: Vec<u8>,
    /// Number of active mappings.
    pub map_count: u32,
}

impl SharedMemory {
    /// Create a new shared memory segment.
    pub fn new(name: &str, mode: u32) -> Self {
        Self {
            name: name.to_string(),
            mode,
            size: 0,
            data: Vec::new(),
            map_count: 0,
        }
    }

    /// ftruncate — set the size of the shared memory.
    pub fn truncate(&mut self, size: usize) {
        self.size = size;
        self.data.resize(size, 0);
    }

    /// Write to the shared memory at an offset.
    pub fn write(&mut self, offset: usize, data: &[u8]) -> Result<usize, IpcError> {
        let end = offset + data.len();
        if end > self.size {
            return Err(IpcError::WouldBlock);
        }
        self.data[offset..end].copy_from_slice(data);
        Ok(data.len())
    }

    /// Read from the shared memory at an offset.
    pub fn read(&self, offset: usize, count: usize) -> Result<Vec<u8>, IpcError> {
        let end = std::cmp::min(offset + count, self.size);
        if offset >= self.size {
            return Ok(Vec::new());
        }
        Ok(self.data[offset..end].to_vec())
    }

    /// Increment map count (mmap).
    pub fn map(&mut self) {
        self.map_count += 1;
    }

    /// Decrement map count (munmap).
    pub fn unmap(&mut self) {
        self.map_count = self.map_count.saturating_sub(1);
    }
}

// ---------------------------------------------------------------------------
//  Semaphore
// ---------------------------------------------------------------------------

/// A POSIX named semaphore.
#[derive(Debug)]
pub struct Semaphore {
    /// Name.
    pub name: String,
    /// Current value.
    pub value: u32,
    /// Permission mode.
    pub mode: u32,
    /// Waiters.
    pub waiters: Vec<u32>,
}

impl Semaphore {
    /// Create a new semaphore.
    pub fn new(name: &str, mode: u32, initial_value: u32) -> Self {
        Self {
            name: name.to_string(),
            value: initial_value,
            mode,
            waiters: Vec::new(),
        }
    }

    /// sem_post — increment the semaphore.
    pub fn post(&mut self) -> Option<u32> {
        self.value += 1;
        if !self.waiters.is_empty() {
            self.value -= 1;
            Some(self.waiters.remove(0))
        } else {
            None
        }
    }

    /// sem_wait — decrement the semaphore (blocks if 0).
    pub fn wait(&mut self, pid: u32) -> Result<(), IpcError> {
        if self.value > 0 {
            self.value -= 1;
            Ok(())
        } else {
            self.waiters.push(pid);
            Err(IpcError::SemaphoreWouldBlock)
        }
    }

    /// sem_trywait — non-blocking decrement.
    pub fn try_wait(&mut self) -> Result<(), IpcError> {
        if self.value > 0 {
            self.value -= 1;
            Ok(())
        } else {
            Err(IpcError::SemaphoreWouldBlock)
        }
    }

    /// Get current value.
    pub fn get_value(&self) -> u32 {
        self.value
    }
}

// ---------------------------------------------------------------------------
//  IPC Registry
// ---------------------------------------------------------------------------

/// Central registry for all IPC objects.
#[derive(Debug)]
pub struct IpcRegistry {
    /// Pipes by ID.
    pipes: HashMap<u32, Pipe>,
    /// Next pipe ID.
    next_pipe_id: u32,
    /// FIFOs by name.
    fifos: HashMap<String, Fifo>,
    /// Message queues by name.
    message_queues: HashMap<String, MessageQueue>,
    /// Shared memory segments by name.
    shared_memory: HashMap<String, SharedMemory>,
    /// Semaphores by name.
    semaphores: HashMap<String, Semaphore>,
}

impl IpcRegistry {
    /// Create a new IPC registry.
    pub fn new() -> Self {
        Self {
            pipes: HashMap::new(),
            next_pipe_id: 1,
            fifos: HashMap::new(),
            message_queues: HashMap::new(),
            shared_memory: HashMap::new(),
            semaphores: HashMap::new(),
        }
    }

    // --- Pipe operations ---

    /// pipe() — create a pipe and return fd pair.
    pub fn pipe(&mut self) -> PipeFds {
        let id = self.next_pipe_id;
        self.next_pipe_id += 1;
        let pipe = Pipe::new(id, 65536);
        self.pipes.insert(id, pipe);
        PipeFds {
            read_fd: id * 2,
            write_fd: id * 2 + 1,
        }
    }

    /// Write to a pipe.
    pub fn pipe_write(&mut self, pipe_id: u32, data: &[u8]) -> Result<usize, IpcError> {
        let pipe = self
            .pipes
            .get_mut(&pipe_id)
            .ok_or(IpcError::BadFileDescriptor { fd: pipe_id })?;
        pipe.write(data)
    }

    /// Read from a pipe.
    pub fn pipe_read(&mut self, pipe_id: u32, count: usize) -> Result<Vec<u8>, IpcError> {
        let pipe = self
            .pipes
            .get_mut(&pipe_id)
            .ok_or(IpcError::BadFileDescriptor { fd: pipe_id })?;
        pipe.read(count)
    }

    /// Close write end of a pipe.
    pub fn pipe_close_write(&mut self, pipe_id: u32) {
        if let Some(pipe) = self.pipes.get_mut(&pipe_id) {
            pipe.close_write();
        }
    }

    /// Close read end of a pipe.
    pub fn pipe_close_read(&mut self, pipe_id: u32) {
        if let Some(pipe) = self.pipes.get_mut(&pipe_id) {
            pipe.close_read();
        }
    }

    // --- FIFO operations ---

    /// mkfifo — create a named pipe.
    pub fn mkfifo(&mut self, name: &str, mode: u32) -> Result<(), IpcError> {
        if self.fifos.contains_key(name) {
            return Err(IpcError::AlreadyExists {
                name: name.to_string(),
            });
        }
        let id = self.next_pipe_id;
        self.next_pipe_id += 1;
        self.fifos
            .insert(name.to_string(), Fifo::new(name, mode, id));
        Ok(())
    }

    /// Get a FIFO by name.
    pub fn get_fifo_mut(&mut self, name: &str) -> Result<&mut Fifo, IpcError> {
        self.fifos.get_mut(name).ok_or(IpcError::NotFound {
            name: name.to_string(),
        })
    }

    // --- Message Queue operations ---

    /// mq_open — create or open a message queue.
    pub fn mq_open(
        &mut self,
        name: &str,
        create: bool,
        mode: u32,
        attrs: MqAttr,
    ) -> Result<(), IpcError> {
        if self.message_queues.contains_key(name) {
            if create {
                return Ok(()); // Already exists, just open.
            }
            return Ok(());
        }
        if !create {
            return Err(IpcError::NotFound {
                name: name.to_string(),
            });
        }
        self.message_queues
            .insert(name.to_string(), MessageQueue::new(name, mode, attrs));
        Ok(())
    }

    /// mq_send — send a message.
    pub fn mq_send(
        &mut self,
        name: &str,
        data: &[u8],
        priority: u32,
    ) -> Result<(), IpcError> {
        let mq = self.message_queues.get_mut(name).ok_or(IpcError::NotFound {
            name: name.to_string(),
        })?;
        mq.send(data, priority)
    }

    /// mq_receive — receive highest priority message.
    pub fn mq_receive(&mut self, name: &str) -> Result<Message, IpcError> {
        let mq = self.message_queues.get_mut(name).ok_or(IpcError::NotFound {
            name: name.to_string(),
        })?;
        mq.receive()
    }

    // --- Shared Memory operations ---

    /// shm_open — create or open shared memory.
    pub fn shm_open(
        &mut self,
        name: &str,
        create: bool,
        mode: u32,
    ) -> Result<(), IpcError> {
        if self.shared_memory.contains_key(name) {
            return Ok(());
        }
        if !create {
            return Err(IpcError::NotFound {
                name: name.to_string(),
            });
        }
        self.shared_memory
            .insert(name.to_string(), SharedMemory::new(name, mode));
        Ok(())
    }

    /// Get shared memory mutably.
    pub fn shm_get_mut(&mut self, name: &str) -> Result<&mut SharedMemory, IpcError> {
        self.shared_memory.get_mut(name).ok_or(IpcError::NotFound {
            name: name.to_string(),
        })
    }

    // --- Semaphore operations ---

    /// sem_open — create or open a semaphore.
    pub fn sem_open(
        &mut self,
        name: &str,
        create: bool,
        mode: u32,
        initial: u32,
    ) -> Result<(), IpcError> {
        if self.semaphores.contains_key(name) {
            return Ok(());
        }
        if !create {
            return Err(IpcError::NotFound {
                name: name.to_string(),
            });
        }
        self.semaphores
            .insert(name.to_string(), Semaphore::new(name, mode, initial));
        Ok(())
    }

    /// sem_post — increment a semaphore.
    pub fn sem_post(&mut self, name: &str) -> Result<Option<u32>, IpcError> {
        let sem = self.semaphores.get_mut(name).ok_or(IpcError::NotFound {
            name: name.to_string(),
        })?;
        Ok(sem.post())
    }

    /// sem_wait — decrement a semaphore.
    pub fn sem_wait(&mut self, name: &str, pid: u32) -> Result<(), IpcError> {
        let sem = self.semaphores.get_mut(name).ok_or(IpcError::NotFound {
            name: name.to_string(),
        })?;
        sem.wait(pid)
    }
}

impl Default for IpcRegistry {
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
    fn test_pipe_write_read() {
        let mut reg = IpcRegistry::new();
        let fds = reg.pipe();
        let pipe_id = (fds.read_fd) / 2;

        reg.pipe_write(pipe_id, b"hello").unwrap();
        let data = reg.pipe_read(pipe_id, 100).unwrap();
        assert_eq!(&data, b"hello");
    }

    #[test]
    fn test_pipe_eof_on_close_write() {
        let mut reg = IpcRegistry::new();
        let fds = reg.pipe();
        let pipe_id = fds.read_fd / 2;

        reg.pipe_write(pipe_id, b"data").unwrap();
        reg.pipe_close_write(pipe_id);

        let data = reg.pipe_read(pipe_id, 100).unwrap();
        assert_eq!(&data, b"data");

        // After all data read, should get empty (EOF).
        let eof = reg.pipe_read(pipe_id, 100).unwrap();
        assert!(eof.is_empty());
    }

    #[test]
    fn test_fifo_create_and_use() {
        let mut reg = IpcRegistry::new();
        reg.mkfifo("/tmp/myfifo", 0o644).unwrap();

        let fifo = reg.get_fifo_mut("/tmp/myfifo").unwrap();
        fifo.open_read();
        fifo.open_write();

        fifo.pipe.write(b"fifo data").unwrap();
        let data = fifo.pipe.read(100).unwrap();
        assert_eq!(&data, b"fifo data");
    }

    #[test]
    fn test_fifo_duplicate_fails() {
        let mut reg = IpcRegistry::new();
        reg.mkfifo("/tmp/myfifo", 0o644).unwrap();
        let err = reg.mkfifo("/tmp/myfifo", 0o644).unwrap_err();
        assert!(matches!(err, IpcError::AlreadyExists { .. }));
    }

    #[test]
    fn test_message_queue_send_receive() {
        let mut reg = IpcRegistry::new();
        reg.mq_open("/myqueue", true, 0o644, MqAttr::default())
            .unwrap();

        reg.mq_send("/myqueue", b"low priority", 1).unwrap();
        reg.mq_send("/myqueue", b"high priority", 10).unwrap();

        // Should receive highest priority first.
        let msg = reg.mq_receive("/myqueue").unwrap();
        assert_eq!(&msg.data, b"high priority");
        assert_eq!(msg.priority, 10);

        let msg2 = reg.mq_receive("/myqueue").unwrap();
        assert_eq!(&msg2.data, b"low priority");
    }

    #[test]
    fn test_message_queue_full() {
        let mut reg = IpcRegistry::new();
        let attrs = MqAttr {
            max_messages: 2,
            max_msg_size: 1024,
        };
        reg.mq_open("/small", true, 0o644, attrs).unwrap();
        reg.mq_send("/small", b"msg1", 0).unwrap();
        reg.mq_send("/small", b"msg2", 0).unwrap();
        let err = reg.mq_send("/small", b"msg3", 0).unwrap_err();
        assert!(matches!(err, IpcError::QueueFull));
    }

    #[test]
    fn test_shared_memory() {
        let mut reg = IpcRegistry::new();
        reg.shm_open("/myshm", true, 0o644).unwrap();
        let shm = reg.shm_get_mut("/myshm").unwrap();
        shm.truncate(4096);
        shm.map();

        shm.write(0, b"shared data").unwrap();
        let data = shm.read(0, 11).unwrap();
        assert_eq!(&data, b"shared data");
    }

    #[test]
    fn test_shared_memory_write_visible() {
        let mut reg = IpcRegistry::new();
        reg.shm_open("/vis", true, 0o644).unwrap();
        let shm = reg.shm_get_mut("/vis").unwrap();
        shm.truncate(100);
        shm.write(0, b"AAA").unwrap();
        shm.write(0, b"BBB").unwrap();
        let data = shm.read(0, 3).unwrap();
        assert_eq!(&data, b"BBB");
    }

    #[test]
    fn test_semaphore_basic() {
        let mut reg = IpcRegistry::new();
        reg.sem_open("/mysem", true, 0o644, 1).unwrap();

        // First wait succeeds.
        reg.sem_wait("/mysem", 1).unwrap();

        // Second wait blocks.
        let err = reg.sem_wait("/mysem", 2).unwrap_err();
        assert!(matches!(err, IpcError::SemaphoreWouldBlock));

        // Post wakes waiter.
        let woken = reg.sem_post("/mysem").unwrap();
        assert_eq!(woken, Some(2));
    }

    #[test]
    fn test_semaphore_post_no_waiters() {
        let mut reg = IpcRegistry::new();
        reg.sem_open("/sem2", true, 0o644, 0).unwrap();
        let woken = reg.sem_post("/sem2").unwrap();
        assert_eq!(woken, None);
    }

    #[test]
    fn test_pipe_broken_on_read_close() {
        let mut pipe = Pipe::new(1, 1024);
        pipe.close_read();
        let err = pipe.write(b"data").unwrap_err();
        assert!(matches!(err, IpcError::BrokenPipe));
    }

    #[test]
    fn test_message_too_large() {
        let mut mq = MessageQueue::new("/test", 0o644, MqAttr {
            max_messages: 10,
            max_msg_size: 4,
        });
        let err = mq.send(b"too large message", 0).unwrap_err();
        assert!(matches!(err, IpcError::MessageTooLarge { .. }));
    }
}
