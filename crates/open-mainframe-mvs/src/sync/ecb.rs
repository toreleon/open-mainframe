//! Event Control Block — the fundamental z/OS synchronization primitive.

use std::sync::atomic::{AtomicU32, Ordering};

use tokio::sync::Notify;

/// Event Control Block (ECB).
///
/// Bit layout (matching IBM z/OS):
/// - Bit 0: wait bit (task is waiting)
/// - Bit 1: complete bit (event has occurred)
/// - Bits 2-31: completion code
#[derive(Debug)]
pub struct Ecb {
    value: AtomicU32,
    notify: Notify,
}

/// Bit mask for the wait bit (bit 0).
const WAIT_BIT: u32 = 0x8000_0000;
/// Bit mask for the complete bit (bit 1).
const COMPLETE_BIT: u32 = 0x4000_0000;
/// Bit mask for the completion code (bits 2-31).
const CODE_MASK: u32 = 0x3FFF_FFFF;

impl Ecb {
    /// Create a new ECB in the initial state (wait=0, complete=0, code=0).
    pub fn new() -> Self {
        Self {
            value: AtomicU32::new(0),
            notify: Notify::new(),
        }
    }

    /// Read the raw ECB value.
    pub fn value(&self) -> u32 {
        self.value.load(Ordering::Acquire)
    }

    /// Check if the wait bit is set.
    pub fn is_waiting(&self) -> bool {
        self.value() & WAIT_BIT != 0
    }

    /// Check if the complete bit is set.
    pub fn is_complete(&self) -> bool {
        self.value() & COMPLETE_BIT != 0
    }

    /// Extract the completion code (bits 2-31).
    pub fn completion_code(&self) -> u32 {
        self.value() & CODE_MASK
    }

    /// Set the wait bit (called before suspending).
    pub fn set_wait(&self) {
        self.value.fetch_or(WAIT_BIT, Ordering::Release);
    }

    /// Clear the wait bit.
    pub fn clear_wait(&self) {
        self.value.fetch_and(!WAIT_BIT, Ordering::Release);
    }

    /// Post the ECB — set the complete bit, store the completion code,
    /// and wake any waiting task.
    pub fn post(&self, completion_code: u32) {
        let code = completion_code & CODE_MASK;
        let new_val = COMPLETE_BIT | code;
        // Clear wait bit, set complete bit and code
        self.value.store(new_val, Ordering::Release);
        self.notify.notify_waiters();
    }

    /// Reset the ECB to its initial state.
    pub fn reset(&self) {
        self.value.store(0, Ordering::Release);
    }

    /// Wait asynchronously until this ECB is posted.
    pub async fn wait_on(&self) {
        // If already complete, return immediately
        if self.is_complete() {
            return;
        }
        self.set_wait();
        // Check again after setting wait bit to avoid race
        if self.is_complete() {
            self.clear_wait();
            return;
        }
        self.notify.notified().await;
        self.clear_wait();
    }
}

impl Default for Ecb {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_ecb_initial_state() {
        let ecb = Ecb::new();
        assert_eq!(ecb.value(), 0);
        assert!(!ecb.is_waiting());
        assert!(!ecb.is_complete());
        assert_eq!(ecb.completion_code(), 0);
    }

    #[test]
    fn post_sets_complete_bit_and_code() {
        let ecb = Ecb::new();
        ecb.post(42);
        assert!(ecb.is_complete());
        assert!(!ecb.is_waiting());
        assert_eq!(ecb.completion_code(), 42);
    }

    #[test]
    fn wait_bit_operations() {
        let ecb = Ecb::new();
        ecb.set_wait();
        assert!(ecb.is_waiting());
        ecb.clear_wait();
        assert!(!ecb.is_waiting());
    }

    #[test]
    fn reset_clears_all() {
        let ecb = Ecb::new();
        ecb.post(100);
        assert!(ecb.is_complete());
        ecb.reset();
        assert!(!ecb.is_complete());
        assert_eq!(ecb.completion_code(), 0);
    }

    #[test]
    fn completion_code_uses_bits_2_through_31() {
        let ecb = Ecb::new();
        // Max code is 30 bits = 0x3FFF_FFFF
        ecb.post(0x3FFF_FFFF);
        assert_eq!(ecb.completion_code(), 0x3FFF_FFFF);
        assert!(ecb.is_complete());
    }

    #[test]
    fn post_code_truncated_to_30_bits() {
        let ecb = Ecb::new();
        ecb.post(0xFFFF_FFFF);
        // Only bits 2-31 should be stored
        assert_eq!(ecb.completion_code(), 0x3FFF_FFFF);
    }

    #[tokio::test]
    async fn wait_on_already_posted_returns_immediately() {
        let ecb = Ecb::new();
        ecb.post(0);
        ecb.wait_on().await; // Should not block
        assert!(ecb.is_complete());
    }

    #[tokio::test]
    async fn wait_on_unposted_ecb_suspends_until_post() {
        use std::sync::Arc;

        let ecb = Arc::new(Ecb::new());
        let ecb2 = ecb.clone();

        let handle = tokio::spawn(async move {
            ecb2.wait_on().await;
            ecb2.completion_code()
        });

        // Give the waiter time to start
        tokio::task::yield_now().await;
        ecb.post(7);

        let code = handle.await.unwrap();
        assert_eq!(code, 7);
    }
}
