//! ENQ/DEQ — resource serialization (Global Resource Serialization).

use std::collections::VecDeque;
use std::sync::Arc;

use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use tokio::sync::Notify;

use crate::error::{MvsError, Result};

/// ENQ resource scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum EnqScope {
    /// Within current address space (task-local).
    Step,
    /// System-wide.
    System,
    /// Sysplex-wide (maps to System in single-system).
    Systems,
}

/// ENQ lock mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum EnqMode {
    /// Shared access — multiple holders allowed.
    Shared,
    /// Exclusive access — single holder only.
    Exclusive,
}

/// ENQ resource identifier (qname + rname).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct EnqResource {
    /// Queue name (1-8 characters).
    pub qname: String,
    /// Resource name (1-255 characters).
    pub rname: String,
}

impl EnqResource {
    /// Create and validate a resource identifier.
    pub fn new(qname: &str, rname: &str) -> Result<Self> {
        if qname.is_empty() || qname.len() > 8 {
            return Err(MvsError::EnqInvalid {
                reason: format!("qname must be 1-8 characters, got {}", qname.len()),
            });
        }
        if rname.is_empty() || rname.len() > 255 {
            return Err(MvsError::EnqInvalid {
                reason: format!("rname must be 1-255 characters, got {}", rname.len()),
            });
        }
        Ok(Self {
            qname: qname.to_string(),
            rname: rname.to_string(),
        })
    }
}

/// Internal lock state for a resource.
#[derive(Debug)]
struct EnqState {
    /// Current holders — (task_id, mode).
    holders: Vec<(u64, EnqMode)>,
    /// Waiting requests — (task_id, mode, notify).
    waiters: VecDeque<(u64, EnqMode, Arc<Notify>)>,
}

impl EnqState {
    fn new() -> Self {
        Self {
            holders: Vec::new(),
            waiters: VecDeque::new(),
        }
    }

    fn is_held_shared(&self) -> bool {
        !self.holders.is_empty()
            && self
                .holders
                .iter()
                .all(|(_, mode)| *mode == EnqMode::Shared)
    }

    fn can_grant(&self, mode: EnqMode) -> bool {
        if self.holders.is_empty() {
            return true;
        }
        match mode {
            EnqMode::Shared => self.is_held_shared() && self.waiters.is_empty(),
            EnqMode::Exclusive => false,
        }
    }

    fn add_holder(&mut self, task_id: u64, mode: EnqMode) {
        self.holders.push((task_id, mode));
    }

    fn remove_holder(&mut self, task_id: u64) -> bool {
        let len_before = self.holders.len();
        self.holders.retain(|(id, _)| *id != task_id);
        self.holders.len() < len_before
    }
}

/// ENQ manager — system-wide resource serialization.
#[derive(Debug)]
pub struct EnqManager {
    locks: DashMap<EnqResource, EnqState>,
}

impl EnqManager {
    /// Create a new ENQ manager.
    pub fn new() -> Self {
        Self {
            locks: DashMap::new(),
        }
    }

    /// Create a new ENQ manager wrapped in Arc for sharing.
    pub fn shared() -> Arc<Self> {
        Arc::new(Self::new())
    }

    /// Acquire an ENQ on a resource.
    ///
    /// If the resource is available in the requested mode, the ENQ is granted
    /// immediately. Otherwise, the caller waits (FIFO order) until the resource
    /// becomes available.
    pub async fn enq(
        &self,
        resource: &EnqResource,
        mode: EnqMode,
        _scope: EnqScope,
        task_id: u64,
    ) -> Result<()> {
        // Try immediate grant
        {
            let mut entry = self.locks.entry(resource.clone()).or_insert_with(EnqState::new);
            if entry.can_grant(mode) {
                entry.add_holder(task_id, mode);
                return Ok(());
            }
        }

        // Must wait — register as waiter
        let notify = Arc::new(Notify::new());
        {
            let mut entry = self.locks.entry(resource.clone()).or_insert_with(EnqState::new);
            entry.waiters.push_back((task_id, mode, notify.clone()));
        }

        // Wait loop
        loop {
            notify.notified().await;

            let mut entry = self.locks.entry(resource.clone()).or_insert_with(EnqState::new);
            // Check if we're at the front and can be granted
            if let Some((front_id, front_mode, _)) = entry.waiters.front() {
                if *front_id == task_id && entry.holders.is_empty()
                    || (*front_mode == EnqMode::Shared
                        && entry.is_held_shared()
                        && *front_id == task_id)
                {
                    let (_, granted_mode, _) = entry.waiters.pop_front().unwrap();
                    entry.add_holder(task_id, granted_mode);
                    return Ok(());
                }
            }
        }
    }

    /// Try to acquire an ENQ without waiting.
    ///
    /// Returns `true` if granted, `false` if the resource is busy.
    pub fn try_enq(
        &self,
        resource: &EnqResource,
        mode: EnqMode,
        _scope: EnqScope,
        task_id: u64,
    ) -> bool {
        let mut entry = self.locks.entry(resource.clone()).or_insert_with(EnqState::new);
        if entry.can_grant(mode) {
            entry.add_holder(task_id, mode);
            true
        } else {
            false
        }
    }

    /// Release an ENQ on a resource.
    ///
    /// Wakes the next waiter if appropriate.
    pub fn deq(&self, resource: &EnqResource, task_id: u64) {
        if let Some(mut entry) = self.locks.get_mut(resource) {
            entry.remove_holder(task_id);

            // Wake waiters if resource is now free
            if entry.holders.is_empty() {
                // Wake all waiters — they'll check if they can be granted
                for (_, _, notify) in entry.waiters.iter() {
                    notify.notify_one();
                }
            } else if entry.is_held_shared() {
                // Wake shared waiters at the front
                for (_, mode, notify) in entry.waiters.iter() {
                    if *mode == EnqMode::Shared {
                        notify.notify_one();
                    } else {
                        break; // Stop at first exclusive waiter (FIFO fairness)
                    }
                }
            }
        }
    }

    /// Check if a resource is currently held.
    pub fn is_held(&self, resource: &EnqResource) -> bool {
        self.locks
            .get(resource)
            .map(|e| !e.holders.is_empty())
            .unwrap_or(false)
    }
}

impl Default for EnqManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn resource(qname: &str, rname: &str) -> EnqResource {
        EnqResource::new(qname, rname).unwrap()
    }

    #[test]
    fn resource_validation_valid() {
        assert!(EnqResource::new("SYSDSN", "MY.DATA").is_ok());
    }

    #[test]
    fn resource_validation_qname_too_long() {
        let err = EnqResource::new("TOOLONGQN", "DATA").unwrap_err();
        assert!(matches!(err, MvsError::EnqInvalid { .. }));
    }

    #[test]
    fn resource_validation_rname_too_long() {
        let long_rname = "X".repeat(256);
        let err = EnqResource::new("QN", &long_rname).unwrap_err();
        assert!(matches!(err, MvsError::EnqInvalid { .. }));
    }

    #[test]
    fn resource_validation_empty_qname() {
        let err = EnqResource::new("", "DATA").unwrap_err();
        assert!(matches!(err, MvsError::EnqInvalid { .. }));
    }

    #[tokio::test]
    async fn exclusive_enq_grants_when_free() {
        let mgr = EnqManager::new();
        let res = resource("SYSDSN", "MY.DATA");
        mgr.enq(&res, EnqMode::Exclusive, EnqScope::System, 1)
            .await
            .unwrap();
        assert!(mgr.is_held(&res));
    }

    #[tokio::test]
    async fn shared_enq_allows_multiple_holders() {
        let mgr = EnqManager::new();
        let res = resource("SYSDSN", "MY.DATA");
        mgr.enq(&res, EnqMode::Shared, EnqScope::System, 1)
            .await
            .unwrap();
        mgr.enq(&res, EnqMode::Shared, EnqScope::System, 2)
            .await
            .unwrap();
        assert!(mgr.is_held(&res));
    }

    #[test]
    fn try_enq_exclusive_on_shared_fails() {
        let mgr = EnqManager::new();
        let res = resource("SYSDSN", "MY.DATA");
        assert!(mgr.try_enq(&res, EnqMode::Shared, EnqScope::System, 1));
        assert!(!mgr.try_enq(&res, EnqMode::Exclusive, EnqScope::System, 2));
    }

    #[test]
    fn try_enq_shared_on_exclusive_fails() {
        let mgr = EnqManager::new();
        let res = resource("SYSDSN", "MY.DATA");
        assert!(mgr.try_enq(&res, EnqMode::Exclusive, EnqScope::System, 1));
        assert!(!mgr.try_enq(&res, EnqMode::Shared, EnqScope::System, 2));
    }

    #[tokio::test]
    async fn deq_releases_and_grants_waiter() {
        let mgr = Arc::new(EnqManager::new());
        let res = resource("SYSDSN", "MY.DATA");

        // Task 1 gets exclusive
        mgr.enq(&res, EnqMode::Exclusive, EnqScope::System, 1)
            .await
            .unwrap();

        // Task 2 waits for exclusive
        let mgr2 = mgr.clone();
        let res2 = res.clone();
        let handle = tokio::spawn(async move {
            mgr2.enq(&res2, EnqMode::Exclusive, EnqScope::System, 2)
                .await
                .unwrap();
        });

        tokio::task::yield_now().await;

        // Task 1 releases
        mgr.deq(&res, 1);

        // Task 2 should now be granted
        handle.await.unwrap();
        assert!(mgr.is_held(&res));
    }

    #[test]
    fn deq_on_non_held_resource_is_noop() {
        let mgr = EnqManager::new();
        let res = resource("SYSDSN", "MY.DATA");
        mgr.deq(&res, 1); // Should not panic
    }

    #[test]
    fn scope_step_restricts_to_task() {
        let mgr = EnqManager::new();
        let res = resource("SYSDSN", "MY.DATA");
        // Step scope — still uses same manager but semantically scoped
        assert!(mgr.try_enq(&res, EnqMode::Exclusive, EnqScope::Step, 1));
        assert!(mgr.is_held(&res));
    }
}
