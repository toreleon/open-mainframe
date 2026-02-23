//! IDMS-106: IDMS-DC Transaction Processing (6 stories).
//!
//! IDMS-DC is the teleprocessing monitor component of IDMS.  It manages
//! online tasks, pseudo-converse terminal sessions, scratch/queue areas,
//! and map-based I/O for DC application programs.

use std::collections::{HashMap, VecDeque};

// ---------------------------------------------------------------------------
//  Task priority
// ---------------------------------------------------------------------------

/// Task dispatch priority (0 = highest, 255 = lowest).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
pub struct TaskPriority(pub u8);

impl Default for TaskPriority {
    fn default() -> Self {
        Self(100)
    }
}

// ---------------------------------------------------------------------------
//  DC task
// ---------------------------------------------------------------------------

/// An IDMS-DC task -- a unit of work associated with a task code.
///
/// Each task has a priority, a task code that maps to a program, and
/// working storage for the duration of the task.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct IdmsDcTask {
    /// Unique task ID within the DC system.
    pub task_id: u32,
    /// Task code (e.g., `EMPMAIN`).
    pub task_code: String,
    /// Program name to invoke.
    pub program_name: String,
    /// Task priority.
    pub priority: TaskPriority,
    /// Whether this task is in pseudo-converse state.
    pub pseudo_converse: bool,
    /// Task-local storage (key-value).
    pub storage: HashMap<String, String>,
}

impl IdmsDcTask {
    /// Create a new DC task.
    pub fn new(task_id: u32, task_code: &str, program_name: &str) -> Self {
        Self {
            task_id,
            task_code: task_code.to_uppercase(),
            program_name: program_name.to_uppercase(),
            priority: TaskPriority::default(),
            pseudo_converse: false,
            storage: HashMap::new(),
        }
    }

    /// Set the priority of this task.
    pub fn with_priority(mut self, priority: u8) -> Self {
        self.priority = TaskPriority(priority);
        self
    }
}

// ---------------------------------------------------------------------------
//  Task scheduler
// ---------------------------------------------------------------------------

/// Dispatches and manages DC tasks.
///
/// Tasks are dispatched in priority order (lower number = higher priority).
#[derive(Debug, Default)]
pub struct TaskScheduler {
    /// Pending tasks, dispatched in priority order.
    pending: Vec<IdmsDcTask>,
    /// Currently running task (if any).
    current: Option<IdmsDcTask>,
    /// Completed task IDs.
    completed: Vec<u32>,
    /// Next task ID.
    next_id: u32,
}

impl TaskScheduler {
    /// Create a new task scheduler.
    pub fn new() -> Self {
        Self {
            pending: Vec::new(),
            current: None,
            completed: Vec::new(),
            next_id: 1,
        }
    }

    /// Submit a new task and return its assigned task ID.
    pub fn submit(&mut self, task_code: &str, program_name: &str) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        let task = IdmsDcTask::new(id, task_code, program_name);
        self.pending.push(task);
        self.pending.sort_by_key(|t| t.priority);
        id
    }

    /// Submit a task with a specific priority.
    pub fn submit_with_priority(
        &mut self,
        task_code: &str,
        program_name: &str,
        priority: u8,
    ) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        let task = IdmsDcTask::new(id, task_code, program_name).with_priority(priority);
        self.pending.push(task);
        self.pending.sort_by_key(|t| t.priority);
        id
    }

    /// Dispatch the next highest-priority task.
    pub fn dispatch(&mut self) -> Option<&IdmsDcTask> {
        if self.current.is_some() {
            return self.current.as_ref();
        }
        if let Some(task) = self.pending.first().cloned() {
            self.pending.remove(0);
            self.current = Some(task);
            self.current.as_ref()
        } else {
            None
        }
    }

    /// Complete the currently running task.
    pub fn complete_current(&mut self) -> Option<u32> {
        if let Some(task) = self.current.take() {
            self.completed.push(task.task_id);
            Some(task.task_id)
        } else {
            None
        }
    }

    /// Enter pseudo-converse for the current task (suspend and re-queue).
    pub fn pseudo_converse(&mut self) -> bool {
        if let Some(mut task) = self.current.take() {
            task.pseudo_converse = true;
            self.pending.push(task);
            true
        } else {
            false
        }
    }

    /// Return the number of pending tasks.
    pub fn pending_count(&self) -> usize {
        self.pending.len()
    }

    /// Return the number of completed tasks.
    pub fn completed_count(&self) -> usize {
        self.completed.len()
    }

    /// Check if a task is currently running.
    pub fn is_running(&self) -> bool {
        self.current.is_some()
    }
}

// ---------------------------------------------------------------------------
//  Scratch area
// ---------------------------------------------------------------------------

/// Temporary storage between pseudo-converse interactions.
///
/// Scratch records are keyed by a scratch-area ID and can survive
/// across pseudo-converse boundaries.
#[derive(Debug, Default)]
pub struct ScratchArea {
    /// Scratch records: area-id -> ordered records.
    areas: HashMap<String, Vec<String>>,
}

impl ScratchArea {
    /// Create a new scratch area.
    pub fn new() -> Self {
        Self::default()
    }

    /// Write a record to a scratch area.
    pub fn put(&mut self, area_id: &str, data: &str) {
        self.areas
            .entry(area_id.to_uppercase())
            .or_default()
            .push(data.to_string());
    }

    /// Read and remove the first record from a scratch area.
    pub fn get(&mut self, area_id: &str) -> Option<String> {
        let upper = area_id.to_uppercase();
        let records = self.areas.get_mut(&upper)?;
        if records.is_empty() {
            None
        } else {
            Some(records.remove(0))
        }
    }

    /// Return the number of records in a scratch area.
    pub fn count(&self, area_id: &str) -> usize {
        self.areas
            .get(&area_id.to_uppercase())
            .map_or(0, Vec::len)
    }

    /// Delete all records in a scratch area.
    pub fn delete(&mut self, area_id: &str) {
        self.areas.remove(&area_id.to_uppercase());
    }
}

// ---------------------------------------------------------------------------
//  Queue area
// ---------------------------------------------------------------------------

/// IDMS queue management for inter-task communication.
///
/// Named queues hold ordered messages (FIFO).
#[derive(Debug, Default)]
pub struct QueueArea {
    /// Queues keyed by queue name.
    queues: HashMap<String, VecDeque<String>>,
}

impl QueueArea {
    /// Create a new queue area.
    pub fn new() -> Self {
        Self::default()
    }

    /// Enqueue a message.
    pub fn put(&mut self, queue_name: &str, message: &str) {
        self.queues
            .entry(queue_name.to_uppercase())
            .or_default()
            .push_back(message.to_string());
    }

    /// Dequeue a message (FIFO).
    pub fn get(&mut self, queue_name: &str) -> Option<String> {
        self.queues
            .get_mut(&queue_name.to_uppercase())?
            .pop_front()
    }

    /// Return the number of messages in a queue.
    pub fn count(&self, queue_name: &str) -> usize {
        self.queues
            .get(&queue_name.to_uppercase())
            .map_or(0, VecDeque::len)
    }

    /// Delete a queue and all its messages.
    pub fn delete(&mut self, queue_name: &str) {
        self.queues.remove(&queue_name.to_uppercase());
    }
}

// ---------------------------------------------------------------------------
//  Map support
// ---------------------------------------------------------------------------

/// Basic map I/O for DC programs.
///
/// A map defines named fields that correspond to terminal screen positions.
/// `MapSupport` provides MAP IN (read) and MAP OUT (write) operations.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct MapSupport {
    /// Map name.
    pub map_name: String,
    /// Field values keyed by field name.
    pub fields: HashMap<String, String>,
}

impl MapSupport {
    /// Create a new map.
    pub fn new(name: &str) -> Self {
        Self {
            map_name: name.to_uppercase(),
            fields: HashMap::new(),
        }
    }

    /// Set a field value (MAP OUT).
    pub fn set_field(&mut self, name: &str, value: &str) {
        self.fields
            .insert(name.to_uppercase(), value.to_string());
    }

    /// Get a field value (MAP IN).
    pub fn get_field(&self, name: &str) -> Option<&str> {
        self.fields.get(&name.to_uppercase()).map(String::as_str)
    }

    /// Clear all field values.
    pub fn clear(&mut self) {
        self.fields.clear();
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn task_creation() {
        let task = IdmsDcTask::new(1, "EMPMAIN", "EMPPROG");
        assert_eq!(task.task_code, "EMPMAIN");
        assert_eq!(task.program_name, "EMPPROG");
        assert_eq!(task.priority, TaskPriority(100));
        assert!(!task.pseudo_converse);
    }

    #[test]
    fn task_priority_ordering() {
        let mut scheduler = TaskScheduler::new();
        scheduler.submit_with_priority("LOW", "LOWPROG", 200);
        scheduler.submit_with_priority("HIGH", "HIGHPROG", 10);
        scheduler.submit_with_priority("MED", "MEDPROG", 100);

        let task = scheduler.dispatch().unwrap();
        assert_eq!(task.task_code, "HIGH");
    }

    #[test]
    fn scheduler_dispatch_complete() {
        let mut scheduler = TaskScheduler::new();
        let id1 = scheduler.submit("TASK1", "PROG1");
        let _id2 = scheduler.submit("TASK2", "PROG2");

        scheduler.dispatch();
        assert!(scheduler.is_running());

        let completed = scheduler.complete_current().unwrap();
        assert_eq!(completed, id1);
        assert!(!scheduler.is_running());

        scheduler.dispatch();
        assert!(scheduler.is_running());
    }

    #[test]
    fn scheduler_pseudo_converse() {
        let mut scheduler = TaskScheduler::new();
        scheduler.submit("TASK1", "PROG1");
        scheduler.dispatch();
        assert!(scheduler.is_running());

        assert!(scheduler.pseudo_converse());
        assert!(!scheduler.is_running());
        assert_eq!(scheduler.pending_count(), 1);
    }

    #[test]
    fn scratch_area_operations() {
        let mut sa = ScratchArea::new();
        sa.put("AREA1", "record-1");
        sa.put("AREA1", "record-2");
        assert_eq!(sa.count("AREA1"), 2);

        let r = sa.get("AREA1").unwrap();
        assert_eq!(r, "record-1");
        assert_eq!(sa.count("AREA1"), 1);

        sa.delete("AREA1");
        assert_eq!(sa.count("AREA1"), 0);
    }

    #[test]
    fn queue_area_fifo() {
        let mut qa = QueueArea::new();
        qa.put("MYQUEUE", "msg-1");
        qa.put("MYQUEUE", "msg-2");
        assert_eq!(qa.count("MYQUEUE"), 2);

        assert_eq!(qa.get("MYQUEUE").unwrap(), "msg-1");
        assert_eq!(qa.get("MYQUEUE").unwrap(), "msg-2");
        assert!(qa.get("MYQUEUE").is_none());
    }

    #[test]
    fn queue_delete() {
        let mut qa = QueueArea::new();
        qa.put("Q1", "data");
        qa.delete("Q1");
        assert_eq!(qa.count("Q1"), 0);
    }

    #[test]
    fn map_support_fields() {
        let mut map = MapSupport::new("EMPMAP");
        map.set_field("EMP-ID", "12345");
        map.set_field("EMP-NAME", "SMITH");

        assert_eq!(map.get_field("emp-id"), Some("12345"));
        assert_eq!(map.get_field("EMP-NAME"), Some("SMITH"));
        assert!(map.get_field("NONEXISTENT").is_none());

        map.clear();
        assert!(map.get_field("EMP-ID").is_none());
    }

    #[test]
    fn scheduler_empty_dispatch() {
        let mut scheduler = TaskScheduler::new();
        assert!(scheduler.dispatch().is_none());
    }

    #[test]
    fn completed_count() {
        let mut scheduler = TaskScheduler::new();
        scheduler.submit("T1", "P1");
        scheduler.dispatch();
        scheduler.complete_current();
        assert_eq!(scheduler.completed_count(), 1);
    }
}
