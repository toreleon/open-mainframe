//! Pthreads (USS-104).
//!
//! Provides POSIX threading abstractions:
//! - pthread_create/pthread_join
//! - pthread_mutex_* (including recursive)
//! - pthread_cond_*
//! - pthread_rwlock_*
//! - Thread-specific data and cancellation

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Errors
// ---------------------------------------------------------------------------

/// Errors for thread operations.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error, miette::Diagnostic)]
pub enum ThreadError {
    /// Thread not found.
    #[error("no such thread: {tid}")]
    NoSuchThread { tid: u64 },

    /// Mutex already locked (EBUSY for trylock).
    #[error("mutex is locked (EBUSY)")]
    MutexLocked,

    /// Mutex not owned by caller (for unlock).
    #[error("mutex not owned by caller")]
    MutexNotOwned,

    /// Deadlock detected (EDEADLK).
    #[error("deadlock would occur (EDEADLK)")]
    Deadlock,

    /// RWLock writer active.
    #[error("read-write lock is held by a writer")]
    RwlockWriterActive,

    /// RWLock readers active.
    #[error("read-write lock has active readers")]
    RwlockReadersActive,

    /// Thread was cancelled.
    #[error("thread was cancelled")]
    Cancelled,

    /// Invalid key.
    #[error("invalid thread-specific key: {key}")]
    InvalidKey { key: u32 },
}

// ---------------------------------------------------------------------------
//  Thread State
// ---------------------------------------------------------------------------

/// State of a POSIX thread.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThreadState {
    /// Thread is running.
    Running,
    /// Thread is waiting (on mutex, condvar, etc.).
    Blocked,
    /// Thread has completed.
    Terminated,
    /// Thread was cancelled.
    Cancelled,
}

// ---------------------------------------------------------------------------
//  Thread
// ---------------------------------------------------------------------------

/// A POSIX thread representation.
#[derive(Debug, Clone)]
pub struct PThread {
    /// Thread ID.
    pub tid: u64,
    /// Thread state.
    pub state: ThreadState,
    /// Thread function name (simulated).
    pub function: String,
    /// Return value (set on exit).
    pub return_value: Option<i64>,
    /// Whether the thread is detached.
    pub detached: bool,
    /// Whether cancellation is enabled.
    pub cancel_enabled: bool,
    /// Thread-specific data.
    pub specific_data: HashMap<u32, i64>,
    /// z/OS extension: thread-level security identity (pthread_security_np).
    pub security_identity: Option<ThreadSecurity>,
    /// z/OS extension: user-defined tag (pthread_tag_np).
    pub tag: Option<String>,
}

/// z/OS extension: thread-level security identity for pthread_security_np.
///
/// Allows individual threads to run under different RACF identities.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ThreadSecurity {
    /// RACF user ID for this thread.
    pub userid: String,
    /// Whether the security environment is active.
    pub active: bool,
}

// ---------------------------------------------------------------------------
//  Mutex Type
// ---------------------------------------------------------------------------

/// Type of mutex.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MutexType {
    /// Normal mutex (undefined behavior on re-lock by same thread).
    Normal,
    /// Recursive mutex (same thread can lock multiple times).
    Recursive,
    /// Error-checking mutex (returns error on re-lock by same thread).
    ErrorCheck,
}

// ---------------------------------------------------------------------------
//  Mutex
// ---------------------------------------------------------------------------

/// A POSIX mutex.
#[derive(Debug)]
pub struct PthreadMutex {
    /// Mutex type.
    pub mutex_type: MutexType,
    /// Owning thread (None if unlocked).
    pub owner: Option<u64>,
    /// Lock count (for recursive mutexes).
    pub lock_count: u32,
    /// Threads waiting for this mutex.
    pub waiters: Vec<u64>,
}

impl PthreadMutex {
    /// Create a new mutex.
    pub fn new(mutex_type: MutexType) -> Self {
        Self {
            mutex_type,
            owner: None,
            lock_count: 0,
            waiters: Vec::new(),
        }
    }

    /// Try to lock the mutex.
    pub fn lock(&mut self, tid: u64) -> Result<(), ThreadError> {
        match self.owner {
            None => {
                self.owner = Some(tid);
                self.lock_count = 1;
                Ok(())
            }
            Some(owner_tid) if owner_tid == tid => match self.mutex_type {
                MutexType::Recursive => {
                    self.lock_count += 1;
                    Ok(())
                }
                MutexType::ErrorCheck => Err(ThreadError::Deadlock),
                MutexType::Normal => Err(ThreadError::Deadlock),
            },
            Some(_) => {
                self.waiters.push(tid);
                Err(ThreadError::MutexLocked)
            }
        }
    }

    /// Unlock the mutex.
    pub fn unlock(&mut self, tid: u64) -> Result<Option<u64>, ThreadError> {
        match self.owner {
            Some(owner_tid) if owner_tid == tid => {
                self.lock_count -= 1;
                if self.lock_count == 0 {
                    self.owner = None;
                    // Wake first waiter.
                    let next = if self.waiters.is_empty() {
                        None
                    } else {
                        Some(self.waiters.remove(0))
                    };
                    if let Some(next_tid) = next {
                        self.owner = Some(next_tid);
                        self.lock_count = 1;
                    }
                    Ok(next)
                } else {
                    Ok(None)
                }
            }
            _ => Err(ThreadError::MutexNotOwned),
        }
    }

    /// Try to lock without blocking.
    pub fn try_lock(&mut self, tid: u64) -> Result<(), ThreadError> {
        match self.owner {
            None => {
                self.owner = Some(tid);
                self.lock_count = 1;
                Ok(())
            }
            Some(owner_tid) if owner_tid == tid && self.mutex_type == MutexType::Recursive => {
                self.lock_count += 1;
                Ok(())
            }
            _ => Err(ThreadError::MutexLocked),
        }
    }

    /// Check if locked.
    pub fn is_locked(&self) -> bool {
        self.owner.is_some()
    }
}

// ---------------------------------------------------------------------------
//  Condition Variable
// ---------------------------------------------------------------------------

/// A POSIX condition variable.
#[derive(Debug)]
pub struct PthreadCond {
    /// Threads waiting on this condition.
    pub waiters: Vec<u64>,
}

impl PthreadCond {
    /// Create a new condition variable.
    pub fn new() -> Self {
        Self {
            waiters: Vec::new(),
        }
    }

    /// Wait on the condition variable (caller must hold mutex).
    pub fn wait(&mut self, tid: u64) {
        self.waiters.push(tid);
    }

    /// Signal one waiter.
    pub fn signal(&mut self) -> Option<u64> {
        if self.waiters.is_empty() {
            None
        } else {
            Some(self.waiters.remove(0))
        }
    }

    /// Broadcast to all waiters.
    pub fn broadcast(&mut self) -> Vec<u64> {
        std::mem::take(&mut self.waiters)
    }
}

impl Default for PthreadCond {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
//  Read-Write Lock
// ---------------------------------------------------------------------------

/// A POSIX read-write lock.
#[derive(Debug)]
pub struct PthreadRwLock {
    /// Active readers (thread IDs).
    pub readers: Vec<u64>,
    /// Active writer (thread ID), if any.
    pub writer: Option<u64>,
    /// Threads waiting for read access.
    pub read_waiters: Vec<u64>,
    /// Threads waiting for write access.
    pub write_waiters: Vec<u64>,
}

impl PthreadRwLock {
    /// Create a new read-write lock.
    pub fn new() -> Self {
        Self {
            readers: Vec::new(),
            writer: None,
            read_waiters: Vec::new(),
            write_waiters: Vec::new(),
        }
    }

    /// Acquire a read lock.
    pub fn rdlock(&mut self, tid: u64) -> Result<(), ThreadError> {
        if self.writer.is_some() {
            self.read_waiters.push(tid);
            return Err(ThreadError::RwlockWriterActive);
        }
        self.readers.push(tid);
        Ok(())
    }

    /// Acquire a write lock.
    pub fn wrlock(&mut self, tid: u64) -> Result<(), ThreadError> {
        if self.writer.is_some() || !self.readers.is_empty() {
            self.write_waiters.push(tid);
            if self.writer.is_some() {
                return Err(ThreadError::RwlockWriterActive);
            }
            return Err(ThreadError::RwlockReadersActive);
        }
        self.writer = Some(tid);
        Ok(())
    }

    /// Release the lock.
    pub fn unlock(&mut self, tid: u64) -> Result<(), ThreadError> {
        if self.writer == Some(tid) {
            self.writer = None;
            // Wake write waiters first, then read waiters.
            if let Some(next_writer) = self.write_waiters.first().copied() {
                self.write_waiters.remove(0);
                self.writer = Some(next_writer);
            } else {
                // Wake all read waiters.
                let waiters = std::mem::take(&mut self.read_waiters);
                self.readers.extend(waiters);
            }
            return Ok(());
        }

        if let Some(pos) = self.readers.iter().position(|&r| r == tid) {
            self.readers.remove(pos);
            // If no more readers and write waiters exist, wake a writer.
            if self.readers.is_empty() {
                if let Some(next_writer) = self.write_waiters.first().copied() {
                    self.write_waiters.remove(0);
                    self.writer = Some(next_writer);
                }
            }
            return Ok(());
        }

        Err(ThreadError::MutexNotOwned)
    }
}

impl Default for PthreadRwLock {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
//  Thread-Specific Key
// ---------------------------------------------------------------------------

/// A thread-specific data key.
#[derive(Debug, Clone)]
pub struct ThreadKey {
    /// Key ID.
    pub key: u32,
    /// Whether a destructor is registered.
    pub has_destructor: bool,
}

// ---------------------------------------------------------------------------
//  Thread Manager
// ---------------------------------------------------------------------------

/// Manages POSIX threads.
#[derive(Debug)]
pub struct ThreadManager {
    /// Thread table.
    threads: HashMap<u64, PThread>,
    /// Next thread ID.
    next_tid: u64,
    /// Thread-specific data keys.
    keys: HashMap<u32, ThreadKey>,
    /// Next key ID.
    next_key: u32,
    /// Max threads (MAXTHREADS from BPXPRMxx).
    max_threads: u32,
}

impl ThreadManager {
    /// Create a new thread manager.
    pub fn new(max_threads: u32) -> Self {
        Self {
            threads: HashMap::new(),
            next_tid: 1,
            keys: HashMap::new(),
            next_key: 1,
            max_threads,
        }
    }

    /// pthread_create — create a new thread.
    pub fn create_thread(&mut self, function: &str) -> Result<u64, ThreadError> {
        if self.threads.len() as u32 >= self.max_threads {
            return Err(ThreadError::NoSuchThread {
                tid: 0,
            });
        }

        let tid = self.next_tid;
        self.next_tid += 1;

        let thread = PThread {
            tid,
            state: ThreadState::Running,
            function: function.to_string(),
            return_value: None,
            detached: false,
            cancel_enabled: true,
            specific_data: HashMap::new(),
            security_identity: None,
            tag: None,
        };
        self.threads.insert(tid, thread);
        Ok(tid)
    }

    /// pthread_join — wait for thread completion.
    pub fn join_thread(&mut self, tid: u64) -> Result<Option<i64>, ThreadError> {
        let thread = self
            .threads
            .get(&tid)
            .ok_or(ThreadError::NoSuchThread { tid })?;

        if thread.detached {
            return Err(ThreadError::NoSuchThread { tid });
        }

        match thread.state {
            ThreadState::Terminated | ThreadState::Cancelled => {
                let ret = thread.return_value;
                self.threads.remove(&tid);
                Ok(ret)
            }
            _ => {
                // In real implementation this would block.
                Ok(None)
            }
        }
    }

    /// Mark a thread as terminated with a return value.
    pub fn exit_thread(&mut self, tid: u64, return_value: i64) -> Result<(), ThreadError> {
        let thread = self
            .threads
            .get_mut(&tid)
            .ok_or(ThreadError::NoSuchThread { tid })?;
        thread.state = ThreadState::Terminated;
        thread.return_value = Some(return_value);

        // Call destructors for thread-specific data.
        let keys_with_destructors: Vec<u32> = self
            .keys
            .values()
            .filter(|k| k.has_destructor)
            .map(|k| k.key)
            .collect();

        let thread = self.threads.get_mut(&tid).unwrap();
        for key in keys_with_destructors {
            thread.specific_data.remove(&key);
        }

        // Remove if detached.
        if self.threads.get(&tid).map_or(false, |t| t.detached) {
            self.threads.remove(&tid);
        }

        Ok(())
    }

    /// pthread_cancel — request thread cancellation.
    pub fn cancel_thread(&mut self, tid: u64) -> Result<(), ThreadError> {
        let thread = self
            .threads
            .get_mut(&tid)
            .ok_or(ThreadError::NoSuchThread { tid })?;

        if !thread.cancel_enabled {
            return Ok(()); // Cancellation disabled, request ignored.
        }

        thread.state = ThreadState::Cancelled;
        thread.return_value = Some(-1);
        Ok(())
    }

    /// pthread_key_create — create a thread-specific data key.
    pub fn key_create(&mut self, has_destructor: bool) -> u32 {
        let key = self.next_key;
        self.next_key += 1;
        self.keys.insert(
            key,
            ThreadKey {
                key,
                has_destructor,
            },
        );
        key
    }

    /// pthread_setspecific — set thread-specific data.
    pub fn set_specific(
        &mut self,
        tid: u64,
        key: u32,
        value: i64,
    ) -> Result<(), ThreadError> {
        if !self.keys.contains_key(&key) {
            return Err(ThreadError::InvalidKey { key });
        }
        let thread = self
            .threads
            .get_mut(&tid)
            .ok_or(ThreadError::NoSuchThread { tid })?;
        thread.specific_data.insert(key, value);
        Ok(())
    }

    /// pthread_getspecific — get thread-specific data.
    pub fn get_specific(&self, tid: u64, key: u32) -> Result<Option<i64>, ThreadError> {
        if !self.keys.contains_key(&key) {
            return Err(ThreadError::InvalidKey { key });
        }
        let thread = self
            .threads
            .get(&tid)
            .ok_or(ThreadError::NoSuchThread { tid })?;
        Ok(thread.specific_data.get(&key).copied())
    }

    /// Get thread by ID.
    pub fn get_thread(&self, tid: u64) -> Option<&PThread> {
        self.threads.get(&tid)
    }

    /// Thread count.
    pub fn thread_count(&self) -> usize {
        self.threads.len()
    }

    /// pthread_security_np — create thread-level security (z/OS extension).
    ///
    /// Allows a thread to run under a different RACF identity.
    pub fn set_thread_security(
        &mut self,
        tid: u64,
        userid: &str,
    ) -> Result<(), ThreadError> {
        let thread = self
            .threads
            .get_mut(&tid)
            .ok_or(ThreadError::NoSuchThread { tid })?;
        thread.security_identity = Some(ThreadSecurity {
            userid: userid.to_string(),
            active: true,
        });
        Ok(())
    }

    /// pthread_security_np — delete thread-level security (z/OS extension).
    pub fn delete_thread_security(&mut self, tid: u64) -> Result<(), ThreadError> {
        let thread = self
            .threads
            .get_mut(&tid)
            .ok_or(ThreadError::NoSuchThread { tid })?;
        thread.security_identity = None;
        Ok(())
    }

    /// pthread_tag_np — tag a thread with user data (z/OS extension).
    pub fn tag_thread(
        &mut self,
        tid: u64,
        tag: &str,
    ) -> Result<(), ThreadError> {
        let thread = self
            .threads
            .get_mut(&tid)
            .ok_or(ThreadError::NoSuchThread { tid })?;
        thread.tag = Some(tag.to_string());
        Ok(())
    }

    /// Get thread tag.
    pub fn get_thread_tag(&self, tid: u64) -> Result<Option<&str>, ThreadError> {
        let thread = self
            .threads
            .get(&tid)
            .ok_or(ThreadError::NoSuchThread { tid })?;
        Ok(thread.tag.as_deref())
    }

    /// pthread_detach — detach a thread.
    pub fn detach_thread(&mut self, tid: u64) -> Result<(), ThreadError> {
        let thread = self
            .threads
            .get_mut(&tid)
            .ok_or(ThreadError::NoSuchThread { tid })?;
        thread.detached = true;
        Ok(())
    }
}

// ===========================================================================
//  Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_and_join_thread() {
        let mut mgr = ThreadManager::new(100);
        let tid = mgr.create_thread("worker_func").unwrap();
        assert_eq!(tid, 1);
        assert_eq!(mgr.thread_count(), 1);

        mgr.exit_thread(tid, 42).unwrap();
        let result = mgr.join_thread(tid).unwrap();
        assert_eq!(result, Some(42));
        assert_eq!(mgr.thread_count(), 0);
    }

    #[test]
    fn test_mutex_lock_unlock() {
        let mut mutex = PthreadMutex::new(MutexType::Normal);
        assert!(!mutex.is_locked());

        mutex.lock(1).unwrap();
        assert!(mutex.is_locked());
        assert_eq!(mutex.owner, Some(1));

        mutex.unlock(1).unwrap();
        assert!(!mutex.is_locked());
    }

    #[test]
    fn test_mutex_contention() {
        let mut mutex = PthreadMutex::new(MutexType::Normal);
        mutex.lock(1).unwrap();

        // Thread 2 tries to lock — should fail.
        let err = mutex.lock(2).unwrap_err();
        assert!(matches!(err, ThreadError::MutexLocked));
        assert_eq!(mutex.waiters, vec![2]);
    }

    #[test]
    fn test_recursive_mutex() {
        let mut mutex = PthreadMutex::new(MutexType::Recursive);
        mutex.lock(1).unwrap();
        mutex.lock(1).unwrap(); // Second lock succeeds.
        assert_eq!(mutex.lock_count, 2);

        mutex.unlock(1).unwrap();
        assert_eq!(mutex.lock_count, 1);
        assert!(mutex.is_locked());

        mutex.unlock(1).unwrap();
        assert!(!mutex.is_locked());
    }

    #[test]
    fn test_error_check_mutex_deadlock() {
        let mut mutex = PthreadMutex::new(MutexType::ErrorCheck);
        mutex.lock(1).unwrap();
        let err = mutex.lock(1).unwrap_err();
        assert!(matches!(err, ThreadError::Deadlock));
    }

    #[test]
    fn test_cond_signal() {
        let mut cond = PthreadCond::new();
        cond.wait(1);
        cond.wait(2);

        let woken = cond.signal();
        assert_eq!(woken, Some(1));
        assert_eq!(cond.waiters.len(), 1);
    }

    #[test]
    fn test_cond_broadcast() {
        let mut cond = PthreadCond::new();
        cond.wait(1);
        cond.wait(2);
        cond.wait(3);

        let woken = cond.broadcast();
        assert_eq!(woken, vec![1, 2, 3]);
        assert!(cond.waiters.is_empty());
    }

    #[test]
    fn test_rwlock_multiple_readers() {
        let mut rw = PthreadRwLock::new();
        rw.rdlock(1).unwrap();
        rw.rdlock(2).unwrap();
        rw.rdlock(3).unwrap();
        assert_eq!(rw.readers.len(), 3);
    }

    #[test]
    fn test_rwlock_writer_blocks_reader() {
        let mut rw = PthreadRwLock::new();
        rw.wrlock(1).unwrap();

        let err = rw.rdlock(2).unwrap_err();
        assert!(matches!(err, ThreadError::RwlockWriterActive));
    }

    #[test]
    fn test_rwlock_reader_blocks_writer() {
        let mut rw = PthreadRwLock::new();
        rw.rdlock(1).unwrap();

        let err = rw.wrlock(2).unwrap_err();
        assert!(matches!(err, ThreadError::RwlockReadersActive));
    }

    #[test]
    fn test_rwlock_unlock_writer_wakes_readers() {
        let mut rw = PthreadRwLock::new();
        rw.wrlock(1).unwrap();
        // Queue read waiter.
        let _ = rw.rdlock(2);
        let _ = rw.rdlock(3);

        rw.unlock(1).unwrap();
        // Readers should be woken.
        assert!(rw.writer.is_none());
        assert_eq!(rw.readers.len(), 2);
    }

    #[test]
    fn test_thread_specific_data() {
        let mut mgr = ThreadManager::new(100);
        let tid = mgr.create_thread("worker").unwrap();
        let key = mgr.key_create(true);

        mgr.set_specific(tid, key, 999).unwrap();
        let val = mgr.get_specific(tid, key).unwrap();
        assert_eq!(val, Some(999));
    }

    #[test]
    fn test_thread_cancellation() {
        let mut mgr = ThreadManager::new(100);
        let tid = mgr.create_thread("worker").unwrap();
        mgr.cancel_thread(tid).unwrap();
        let thread = mgr.get_thread(tid).unwrap();
        assert_eq!(thread.state, ThreadState::Cancelled);
    }

    #[test]
    fn test_thread_specific_key_destructor() {
        let mut mgr = ThreadManager::new(100);
        let tid = mgr.create_thread("worker").unwrap();
        let key = mgr.key_create(true);
        mgr.set_specific(tid, key, 42).unwrap();

        // When thread exits, destructor keys should be cleaned up.
        mgr.exit_thread(tid, 0).unwrap();
        // Thread should be terminated.
    }

    #[test]
    fn test_mutex_try_lock() {
        let mut mutex = PthreadMutex::new(MutexType::Normal);
        mutex.try_lock(1).unwrap();
        let err = mutex.try_lock(2).unwrap_err();
        assert!(matches!(err, ThreadError::MutexLocked));
    }

    #[test]
    fn test_set_thread_security() {
        let mut mgr = ThreadManager::new(100);
        let tid = mgr.create_thread("worker").unwrap();

        mgr.set_thread_security(tid, "IBMUSER").unwrap();
        let thread = mgr.get_thread(tid).unwrap();
        let sec = thread.security_identity.as_ref().unwrap();
        assert_eq!(sec.userid, "IBMUSER");
        assert!(sec.active);
    }

    #[test]
    fn test_delete_thread_security() {
        let mut mgr = ThreadManager::new(100);
        let tid = mgr.create_thread("worker").unwrap();

        mgr.set_thread_security(tid, "IBMUSER").unwrap();
        mgr.delete_thread_security(tid).unwrap();
        let thread = mgr.get_thread(tid).unwrap();
        assert!(thread.security_identity.is_none());
    }

    #[test]
    fn test_tag_thread() {
        let mut mgr = ThreadManager::new(100);
        let tid = mgr.create_thread("worker").unwrap();

        mgr.tag_thread(tid, "http-handler").unwrap();
        let tag = mgr.get_thread_tag(tid).unwrap();
        assert_eq!(tag, Some("http-handler"));
    }

    #[test]
    fn test_detach_thread() {
        let mut mgr = ThreadManager::new(100);
        let tid = mgr.create_thread("worker").unwrap();
        assert!(!mgr.get_thread(tid).unwrap().detached);

        mgr.detach_thread(tid).unwrap();
        assert!(mgr.get_thread(tid).unwrap().detached);

        // Detached thread should be removed on exit.
        mgr.exit_thread(tid, 0).unwrap();
        assert!(mgr.get_thread(tid).is_none());
    }

    #[test]
    fn test_detached_thread_cannot_join() {
        let mut mgr = ThreadManager::new(100);
        let tid = mgr.create_thread("worker").unwrap();
        mgr.detach_thread(tid).unwrap();

        let err = mgr.join_thread(tid).unwrap_err();
        assert!(matches!(err, ThreadError::NoSuchThread { .. }));
    }
}
