//! IDMS-110: Lock Management (5 stories).
//!
//! Provides record-level and area-level locking with deadlock detection.
//! The lock manager tracks locks held by run-units (transactions) and
//! uses a wait-for graph to detect deadlocks.

use std::collections::{HashMap, HashSet};

// ---------------------------------------------------------------------------
//  Lock mode
// ---------------------------------------------------------------------------

/// Lock mode for a database resource.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum LockMode {
    /// Shared lock -- multiple readers allowed.
    Share,
    /// Update lock -- intend to modify (upgradeable to exclusive).
    Update,
    /// Exclusive lock -- single writer, no readers.
    Exclusive,
}

impl LockMode {
    /// Check if this lock mode is compatible with another.
    pub fn compatible_with(self, other: LockMode) -> bool {
        matches!(
            (self, other),
            (LockMode::Share, LockMode::Share) | (LockMode::Share, LockMode::Update)
                | (LockMode::Update, LockMode::Share)
        )
    }
}

// ---------------------------------------------------------------------------
//  Lock target
// ---------------------------------------------------------------------------

/// What is being locked -- a record or an area.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LockTarget {
    /// Lock a specific record by dbkey.
    Record(u64),
    /// Lock an entire area by name.
    Area(String),
}

// ---------------------------------------------------------------------------
//  Lock entry
// ---------------------------------------------------------------------------

/// A held lock.
#[derive(Debug, Clone)]
struct LockEntry {
    /// Who holds this lock (run-unit / transaction ID).
    holder: u64,
    /// Lock mode.
    mode: LockMode,
}

// ---------------------------------------------------------------------------
//  Wait-for entry
// ---------------------------------------------------------------------------

/// A waiter blocked on a lock.
#[derive(Debug, Clone)]
struct WaitEntry {
    /// Who is waiting.
    waiter: u64,
    /// What they are waiting for.
    target: LockTarget,
    /// Requested mode.
    mode: LockMode,
}

// ---------------------------------------------------------------------------
//  Lock manager
// ---------------------------------------------------------------------------

/// Manages record-level and area-level locks.
///
/// Supports Share, Update, and Exclusive lock modes with compatibility
/// checking and a queue of waiters.
#[derive(Debug, Default)]
pub struct LockManager {
    /// Currently held locks: target -> list of holders.
    locks: HashMap<LockTarget, Vec<LockEntry>>,
    /// Waiters blocked on locks.
    waiters: Vec<WaitEntry>,
}

/// Result of a lock request.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LockResult {
    /// Lock was granted immediately.
    Granted,
    /// Lock request is queued (would block).
    Queued,
    /// Deadlock detected -- request denied.
    Deadlock,
}

impl LockManager {
    /// Create a new lock manager.
    pub fn new() -> Self {
        Self::default()
    }

    /// Request a lock on a target.
    pub fn lock(
        &mut self,
        holder: u64,
        target: LockTarget,
        mode: LockMode,
    ) -> LockResult {
        // Check existing locks.
        if let Some(entries) = self.locks.get(&target) {
            // Check if holder already has this lock.
            for entry in entries {
                if entry.holder == holder {
                    // Already held -- upgrade if needed.
                    if mode == LockMode::Exclusive && entry.mode != LockMode::Exclusive {
                        // Need to upgrade, check compatibility with others.
                        let others_compatible = entries
                            .iter()
                            .all(|e| e.holder == holder || mode.compatible_with(e.mode));
                        if others_compatible {
                            // Remove old and re-add as exclusive.
                            let target_clone = target.clone();
                            let entries = self.locks.get_mut(&target_clone).unwrap();
                            entries.retain(|e| e.holder != holder);
                            entries.push(LockEntry { holder, mode });
                            return LockResult::Granted;
                        }
                        // Cannot upgrade now -- queue.
                        self.waiters.push(WaitEntry {
                            waiter: holder,
                            target,
                            mode,
                        });
                        return LockResult::Queued;
                    }
                    return LockResult::Granted;
                }
            }

            // Check compatibility with all existing holders.
            let compatible = entries.iter().all(|e| mode.compatible_with(e.mode));
            if !compatible {
                // Check for deadlock before queuing.
                let detector = DeadlockDetector::new(self);
                if detector.would_deadlock(holder, &target) {
                    return LockResult::Deadlock;
                }
                self.waiters.push(WaitEntry {
                    waiter: holder,
                    target,
                    mode,
                });
                return LockResult::Queued;
            }
        }

        // Grant the lock.
        self.locks
            .entry(target)
            .or_default()
            .push(LockEntry { holder, mode });
        LockResult::Granted
    }

    /// Release all locks held by a holder on a specific target.
    pub fn unlock(&mut self, holder: u64, target: &LockTarget) -> bool {
        let released = if let Some(entries) = self.locks.get_mut(target) {
            let before = entries.len();
            entries.retain(|e| e.holder != holder);
            let released = before != entries.len();
            if entries.is_empty() {
                self.locks.remove(target);
            }
            released
        } else {
            false
        };
        if released {
            self.process_waiters();
        }
        released
    }

    /// Release all locks held by a holder (e.g., at end of transaction).
    pub fn unlock_all(&mut self, holder: u64) {
        for entries in self.locks.values_mut() {
            entries.retain(|e| e.holder != holder);
        }
        self.locks.retain(|_, entries| !entries.is_empty());
        self.waiters.retain(|w| w.waiter != holder);
        self.process_waiters();
    }

    /// Return the number of locks currently held.
    pub fn lock_count(&self) -> usize {
        self.locks.values().map(Vec::len).sum()
    }

    /// Return the number of waiters.
    pub fn waiter_count(&self) -> usize {
        self.waiters.len()
    }

    /// Check if a holder has a lock on a target.
    pub fn is_locked_by(&self, holder: u64, target: &LockTarget) -> bool {
        self.locks
            .get(target)
            .map_or(false, |entries| entries.iter().any(|e| e.holder == holder))
    }

    /// Return all holders of locks on a target.
    pub fn holders_of(&self, target: &LockTarget) -> Vec<u64> {
        self.locks
            .get(target)
            .map_or(Vec::new(), |entries| entries.iter().map(|e| e.holder).collect())
    }

    /// Try to grant locks to waiters.
    fn process_waiters(&mut self) {
        let mut granted = Vec::new();
        for (i, waiter) in self.waiters.iter().enumerate() {
            let compatible = self
                .locks
                .get(&waiter.target)
                .map_or(true, |entries| {
                    entries
                        .iter()
                        .all(|e| waiter.mode.compatible_with(e.mode))
                });
            if compatible {
                granted.push(i);
            }
        }

        // Grant in reverse order to maintain indices.
        for &i in granted.iter().rev() {
            let waiter = self.waiters.remove(i);
            self.locks
                .entry(waiter.target)
                .or_default()
                .push(LockEntry {
                    holder: waiter.waiter,
                    mode: waiter.mode,
                });
        }
    }
}

// ---------------------------------------------------------------------------
//  Deadlock detector
// ---------------------------------------------------------------------------

/// Detects deadlocks using a wait-for graph.
///
/// Builds a directed graph where an edge from A to B means "A is waiting
/// for B to release a lock".  A cycle in this graph indicates a deadlock.
#[derive(Debug)]
pub struct DeadlockDetector<'a> {
    lock_manager: &'a LockManager,
}

impl<'a> DeadlockDetector<'a> {
    /// Create a new deadlock detector.
    pub fn new(lock_manager: &'a LockManager) -> Self {
        Self { lock_manager }
    }

    /// Check if granting a lock to `holder` on `target` would create a deadlock.
    pub fn would_deadlock(&self, holder: u64, target: &LockTarget) -> bool {
        // Build wait-for graph.
        let mut waits_for: HashMap<u64, HashSet<u64>> = HashMap::new();

        // Existing waiters.
        for waiter in &self.lock_manager.waiters {
            if let Some(entries) = self.lock_manager.locks.get(&waiter.target) {
                for entry in entries {
                    if entry.holder != waiter.waiter {
                        waits_for
                            .entry(waiter.waiter)
                            .or_default()
                            .insert(entry.holder);
                    }
                }
            }
        }

        // Add the hypothetical new wait.
        if let Some(entries) = self.lock_manager.locks.get(target) {
            for entry in entries {
                if entry.holder != holder {
                    waits_for.entry(holder).or_default().insert(entry.holder);
                }
            }
        }

        // Detect cycle using DFS.
        has_cycle(&waits_for, holder)
    }
}

// ---------------------------------------------------------------------------
//  Free-standing cycle detection helpers
// ---------------------------------------------------------------------------

fn has_cycle(graph: &HashMap<u64, HashSet<u64>>, start: u64) -> bool {
    let mut visited = HashSet::new();
    let mut stack = HashSet::new();
    dfs(graph, start, &mut visited, &mut stack)
}

fn dfs(
    graph: &HashMap<u64, HashSet<u64>>,
    node: u64,
    visited: &mut HashSet<u64>,
    stack: &mut HashSet<u64>,
) -> bool {
    if stack.contains(&node) {
        return true; // Cycle detected.
    }
    if visited.contains(&node) {
        return false;
    }
    visited.insert(node);
    stack.insert(node);

    if let Some(neighbors) = graph.get(&node) {
        for &neighbor in neighbors {
            if dfs(graph, neighbor, visited, stack) {
                return true;
            }
        }
    }

    stack.remove(&node);
    false
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn share_locks_compatible() {
        let mut lm = LockManager::new();
        let target = LockTarget::Record(100);
        assert_eq!(lm.lock(1, target.clone(), LockMode::Share), LockResult::Granted);
        assert_eq!(lm.lock(2, target.clone(), LockMode::Share), LockResult::Granted);
        assert_eq!(lm.lock_count(), 2);
    }

    #[test]
    fn exclusive_blocks_share() {
        let mut lm = LockManager::new();
        let target = LockTarget::Record(100);
        assert_eq!(
            lm.lock(1, target.clone(), LockMode::Exclusive),
            LockResult::Granted
        );
        assert_eq!(
            lm.lock(2, target.clone(), LockMode::Share),
            LockResult::Queued
        );
    }

    #[test]
    fn share_blocks_exclusive() {
        let mut lm = LockManager::new();
        let target = LockTarget::Record(100);
        assert_eq!(lm.lock(1, target.clone(), LockMode::Share), LockResult::Granted);
        assert_eq!(
            lm.lock(2, target.clone(), LockMode::Exclusive),
            LockResult::Queued
        );
    }

    #[test]
    fn unlock_releases() {
        let mut lm = LockManager::new();
        let target = LockTarget::Record(100);
        lm.lock(1, target.clone(), LockMode::Exclusive);
        assert!(lm.is_locked_by(1, &target));

        lm.unlock(1, &target);
        assert!(!lm.is_locked_by(1, &target));
        assert_eq!(lm.lock_count(), 0);
    }

    #[test]
    fn unlock_all_releases_everything() {
        let mut lm = LockManager::new();
        lm.lock(1, LockTarget::Record(100), LockMode::Share);
        lm.lock(1, LockTarget::Record(200), LockMode::Exclusive);
        lm.lock(1, LockTarget::Area("EMP-AREA".into()), LockMode::Share);
        assert_eq!(lm.lock_count(), 3);

        lm.unlock_all(1);
        assert_eq!(lm.lock_count(), 0);
    }

    #[test]
    fn unlock_grants_waiters() {
        let mut lm = LockManager::new();
        let target = LockTarget::Record(100);
        lm.lock(1, target.clone(), LockMode::Exclusive);
        lm.lock(2, target.clone(), LockMode::Share); // queued

        assert_eq!(lm.waiter_count(), 1);
        lm.unlock(1, &target);
        // Waiter should now be granted.
        assert_eq!(lm.waiter_count(), 0);
        assert!(lm.is_locked_by(2, &target));
    }

    #[test]
    fn area_locking() {
        let mut lm = LockManager::new();
        let target = LockTarget::Area("DEPT-AREA".into());
        assert_eq!(lm.lock(1, target.clone(), LockMode::Share), LockResult::Granted);
        assert_eq!(lm.lock(2, target.clone(), LockMode::Share), LockResult::Granted);
        assert_eq!(
            lm.lock(3, target.clone(), LockMode::Exclusive),
            LockResult::Queued
        );
    }

    #[test]
    fn lock_mode_compatibility() {
        assert!(LockMode::Share.compatible_with(LockMode::Share));
        assert!(LockMode::Share.compatible_with(LockMode::Update));
        assert!(!LockMode::Exclusive.compatible_with(LockMode::Share));
        assert!(!LockMode::Exclusive.compatible_with(LockMode::Exclusive));
        assert!(!LockMode::Update.compatible_with(LockMode::Update));
    }

    #[test]
    fn deadlock_detection() {
        let mut lm = LockManager::new();
        let r1 = LockTarget::Record(100);
        let r2 = LockTarget::Record(200);

        // Tx 1 holds r1, Tx 2 holds r2.
        lm.lock(1, r1.clone(), LockMode::Exclusive);
        lm.lock(2, r2.clone(), LockMode::Exclusive);

        // Tx 1 wants r2 (queued).
        lm.lock(1, r2.clone(), LockMode::Exclusive);

        // Tx 2 wants r1 -- should detect deadlock.
        let result = lm.lock(2, r1.clone(), LockMode::Exclusive);
        assert_eq!(result, LockResult::Deadlock);
    }

    #[test]
    fn holders_of() {
        let mut lm = LockManager::new();
        let target = LockTarget::Record(100);
        lm.lock(1, target.clone(), LockMode::Share);
        lm.lock(2, target.clone(), LockMode::Share);
        let holders = lm.holders_of(&target);
        assert_eq!(holders.len(), 2);
        assert!(holders.contains(&1));
        assert!(holders.contains(&2));
    }

    #[test]
    fn reentrant_lock() {
        let mut lm = LockManager::new();
        let target = LockTarget::Record(100);
        assert_eq!(lm.lock(1, target.clone(), LockMode::Share), LockResult::Granted);
        // Same holder, same target -- should still be Granted.
        assert_eq!(lm.lock(1, target.clone(), LockMode::Share), LockResult::Granted);
    }
}
