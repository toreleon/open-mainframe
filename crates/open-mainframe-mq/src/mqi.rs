//! MQI (Message Queue Interface) — MQCONN, MQDISC, MQOPEN, MQCLOSE, MQPUT, MQGET.
//!
//! Provides the MQI programming interface for applications.

use crate::core::{MqError, QueueManager};
use crate::structures::{Mqgmo, Mqmd, Mqod, MqPmo};

// ---------------------------------------------------------------------------
//  Open options
// ---------------------------------------------------------------------------

/// Open options for MQOPEN.
#[derive(Debug, Clone, Default)]
pub struct OpenOptions {
    /// Open for output (MQPUT).
    pub output: bool,
    /// Open for input (shared).
    pub input_shared: bool,
    /// Open for input (exclusive).
    pub input_exclusive: bool,
    /// Open for browse.
    pub browse: bool,
    /// Open for inquire.
    pub inquire: bool,
    /// Open for set.
    pub set: bool,
}

// ---------------------------------------------------------------------------
//  Connection / Handle
// ---------------------------------------------------------------------------

/// An MQI connection handle.
#[derive(Debug)]
pub struct Connection {
    /// Connection handle.
    pub handle: u32,
    /// Queue manager name.
    pub qmgr_name: String,
    /// Whether connected.
    pub connected: bool,
    /// Open objects.
    open_objects: Vec<OpenObject>,
    /// Next object handle.
    next_obj_handle: u32,
}

/// An open MQ object.
#[derive(Debug)]
struct OpenObject {
    handle: u32,
    queue_name: String,
    options: OpenOptions,
}

/// An MQI handle (wraps connection and object operations).
#[derive(Debug)]
pub struct MqiHandle {
    handle: u32,
}

impl MqiHandle {
    /// Get the raw handle value.
    pub fn value(&self) -> u32 {
        self.handle
    }
}

impl Connection {
    /// MQCONN — connect to queue manager.
    pub fn connect(qm: &mut QueueManager) -> Result<Self, MqError> {
        if !qm.running {
            return Err(MqError::Other("Queue manager not running".to_string()));
        }
        let handle = qm.next_handle();
        Ok(Self {
            handle,
            qmgr_name: qm.name.clone(),
            connected: true,
            open_objects: Vec::new(),
            next_obj_handle: 1,
        })
    }

    /// MQDISC — disconnect from queue manager.
    pub fn disconnect(&mut self) -> Result<(), MqError> {
        if !self.connected {
            return Err(MqError::NotConnected);
        }
        self.open_objects.clear();
        self.connected = false;
        Ok(())
    }

    /// MQOPEN — open a queue.
    pub fn open(
        &mut self,
        qm: &QueueManager,
        od: &mut Mqod,
        options: OpenOptions,
    ) -> Result<MqiHandle, MqError> {
        if !self.connected {
            return Err(MqError::NotConnected);
        }

        // Resolve the queue name.
        let resolved = qm.resolve_alias(&od.object_name)?;
        od.resolved_q_name = resolved.clone();
        od.resolved_qmgr_name = qm.name.clone();

        let handle = self.next_obj_handle;
        self.next_obj_handle += 1;

        self.open_objects.push(OpenObject {
            handle,
            queue_name: resolved,
            options,
        });

        Ok(MqiHandle { handle })
    }

    /// MQCLOSE — close a queue.
    pub fn close(&mut self, obj_handle: &MqiHandle) -> Result<(), MqError> {
        if !self.connected {
            return Err(MqError::NotConnected);
        }
        let pos = self
            .open_objects
            .iter()
            .position(|o| o.handle == obj_handle.handle)
            .ok_or(MqError::InvalidHandle)?;
        self.open_objects.remove(pos);
        Ok(())
    }

    /// MQPUT — put a message to an open queue.
    pub fn put(
        &self,
        qm: &mut QueueManager,
        obj_handle: &MqiHandle,
        md: &mut Mqmd,
        pmo: &mut MqPmo,
        data: &[u8],
    ) -> Result<(), MqError> {
        if !self.connected {
            return Err(MqError::NotConnected);
        }
        let obj = self
            .open_objects
            .iter()
            .find(|o| o.handle == obj_handle.handle)
            .ok_or(MqError::InvalidHandle)?;

        if !obj.options.output {
            return Err(MqError::Other("Queue not open for output".to_string()));
        }

        pmo.resolved_q_name = obj.queue_name.clone();
        pmo.resolved_qmgr_name = qm.name.clone();

        // Generate new message ID if requested.
        if pmo.new_msg_id {
            let handle_bytes = obj_handle.handle.to_be_bytes();
            let counter_bytes = qm.next_handle().to_be_bytes();
            md.msg_id[..4].copy_from_slice(&handle_bytes);
            md.msg_id[4..8].copy_from_slice(&counter_bytes);
        }

        let queue = qm.get_queue_mut(&obj.queue_name)?;
        queue.put(md.clone(), data.to_vec())
    }

    /// MQPUT1 — put a single message (open, put, close in one call).
    pub fn put1(
        &mut self,
        qm: &mut QueueManager,
        od: &mut Mqod,
        md: &mut Mqmd,
        pmo: &mut MqPmo,
        data: &[u8],
    ) -> Result<(), MqError> {
        let options = OpenOptions {
            output: true,
            ..Default::default()
        };
        let handle = self.open(qm, od, options)?;
        let result = self.put(qm, &handle, md, pmo, data);
        let _ = self.close(&handle);
        result
    }

    /// MQGET — get a message from an open queue.
    pub fn get(
        &self,
        qm: &mut QueueManager,
        obj_handle: &MqiHandle,
        md: &mut Mqmd,
        gmo: &Mqgmo,
        buffer: &mut Vec<u8>,
    ) -> Result<usize, MqError> {
        if !self.connected {
            return Err(MqError::NotConnected);
        }
        let obj = self
            .open_objects
            .iter()
            .find(|o| o.handle == obj_handle.handle)
            .ok_or(MqError::InvalidHandle)?;

        if !obj.options.input_shared && !obj.options.input_exclusive && !obj.options.browse {
            return Err(MqError::Other("Queue not open for input".to_string()));
        }

        let queue = qm.get_queue_mut(&obj.queue_name)?;

        if gmo.browse {
            // Browse mode.
            if let Some(msg) = queue.browse(gmo.browse_cursor) {
                *md = msg.mqmd.clone();
                buffer.clear();
                buffer.extend_from_slice(&msg.data);
                Ok(msg.data.len())
            } else {
                Err(MqError::NoMessage(obj.queue_name.clone()))
            }
        } else if gmo.match_correl_id {
            // Get by correlation ID.
            let msg = queue.get_by_correl_id(&md.correl_id)?;
            let len = msg.data.len();
            *md = msg.mqmd;
            buffer.clear();
            buffer.extend_from_slice(&msg.data);
            Ok(len)
        } else {
            // Normal destructive get.
            let msg = queue.get()?;
            let len = msg.data.len();
            *md = msg.mqmd;
            buffer.clear();
            buffer.extend_from_slice(&msg.data);
            Ok(len)
        }
    }

    /// Number of currently open objects.
    pub fn open_count(&self) -> usize {
        self.open_objects.len()
    }

    /// MQINQ — inquire about queue attributes.
    pub fn inquire(
        &self,
        qm: &QueueManager,
        obj_handle: &MqiHandle,
    ) -> Result<QueueAttributes, MqError> {
        if !self.connected {
            return Err(MqError::NotConnected);
        }
        let obj = self
            .open_objects
            .iter()
            .find(|o| o.handle == obj_handle.handle)
            .ok_or(MqError::InvalidHandle)?;

        if !obj.options.inquire {
            return Err(MqError::Other("Queue not open for inquire".to_string()));
        }

        let queue = qm.get_queue(&obj.queue_name)?;
        Ok(QueueAttributes {
            queue_name: queue.name.clone(),
            queue_type: queue.queue_type,
            current_depth: queue.depth(),
            max_depth: queue.max_depth,
            put_inhibited: queue.put_inhibited,
            get_inhibited: queue.get_inhibited,
            description: queue.description.clone(),
        })
    }

    /// MQSET — set queue attributes dynamically.
    pub fn set(
        &self,
        qm: &mut QueueManager,
        obj_handle: &MqiHandle,
        attrs: &SetQueueAttributes,
    ) -> Result<(), MqError> {
        if !self.connected {
            return Err(MqError::NotConnected);
        }
        let obj = self
            .open_objects
            .iter()
            .find(|o| o.handle == obj_handle.handle)
            .ok_or(MqError::InvalidHandle)?;

        if !obj.options.set {
            return Err(MqError::Other("Queue not open for set".to_string()));
        }

        let queue = qm.get_queue_mut(&obj.queue_name)?;
        if let Some(inhibit) = attrs.put_inhibited {
            queue.put_inhibited = inhibit;
        }
        if let Some(inhibit) = attrs.get_inhibited {
            queue.get_inhibited = inhibit;
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
//  MQINQ / MQSET attribute structs
// ---------------------------------------------------------------------------

/// Attributes returned by MQINQ.
#[derive(Debug, Clone)]
pub struct QueueAttributes {
    pub queue_name: String,
    pub queue_type: crate::core::QueueType,
    pub current_depth: u32,
    pub max_depth: u32,
    pub put_inhibited: bool,
    pub get_inhibited: bool,
    pub description: String,
}

/// Attributes that can be set by MQSET.
#[derive(Debug, Clone, Default)]
pub struct SetQueueAttributes {
    pub put_inhibited: Option<bool>,
    pub get_inhibited: Option<bool>,
}

// ---------------------------------------------------------------------------
//  Transaction Coordinator (MQCMIT / MQBACK / MQBEGIN)
// ---------------------------------------------------------------------------

/// A pending operation within a unit of work.
#[derive(Debug, Clone)]
enum UowOperation {
    /// A put that should be committed or rolled back.
    Put {
        queue_name: String,
        mqmd: Mqmd,
        data: Vec<u8>,
    },
    /// A get that should be committed (msg stays removed) or rolled back (msg restored).
    Get {
        queue_name: String,
        message: crate::core::Message,
    },
}

/// A unit of work for syncpoint coordination.
#[derive(Debug, Default)]
pub struct UnitOfWork {
    operations: Vec<UowOperation>,
    active: bool,
}

/// Transaction coordinator providing MQCMIT/MQBACK/MQBEGIN semantics.
#[derive(Debug, Default)]
pub struct TransactionCoordinator {
    uow: UnitOfWork,
}

impl TransactionCoordinator {
    pub fn new() -> Self {
        Self::default()
    }

    /// MQBEGIN — begin a new unit of work.
    pub fn begin(&mut self) -> Result<(), MqError> {
        if self.uow.active {
            return Err(MqError::Other("Unit of work already active".to_string()));
        }
        self.uow.active = true;
        self.uow.operations.clear();
        Ok(())
    }

    /// Record a put operation under syncpoint.
    pub fn record_put(&mut self, queue_name: &str, mqmd: Mqmd, data: Vec<u8>) {
        if self.uow.active {
            self.uow.operations.push(UowOperation::Put {
                queue_name: queue_name.to_string(),
                mqmd,
                data,
            });
        }
    }

    /// Record a get operation under syncpoint (the message has been removed
    /// from the queue, and will be restored on rollback).
    pub fn record_get(&mut self, queue_name: &str, message: crate::core::Message) {
        if self.uow.active {
            self.uow.operations.push(UowOperation::Get {
                queue_name: queue_name.to_string(),
                message,
            });
        }
    }

    /// MQCMIT — commit the current unit of work.
    ///
    /// Put operations become permanent (already on queue).
    /// Get operations become permanent (messages stay removed).
    pub fn commit(&mut self) -> Result<(), MqError> {
        if !self.uow.active {
            return Err(MqError::Other("No active unit of work".to_string()));
        }
        // Commit is a no-op for in-memory queues since puts are already
        // on the queues. We simply finalize.
        self.uow.operations.clear();
        self.uow.active = false;
        Ok(())
    }

    /// MQBACK — back out (rollback) the current unit of work.
    ///
    /// Put operations are undone (messages removed from queues).
    /// Get operations are undone (messages restored to queues).
    pub fn backout(&mut self, qm: &mut QueueManager) -> Result<(), MqError> {
        if !self.uow.active {
            return Err(MqError::Other("No active unit of work".to_string()));
        }

        // Process operations in reverse order.
        for op in self.uow.operations.drain(..).rev() {
            match op {
                UowOperation::Put { queue_name, .. } => {
                    // Remove the put message (it was the last one added).
                    if let Ok(queue) = qm.get_queue_mut(&queue_name) {
                        // Best effort: remove the last message we put.
                        let _ = queue.get();
                    }
                }
                UowOperation::Get {
                    queue_name,
                    message,
                } => {
                    // Restore the message that was removed.
                    if let Ok(queue) = qm.get_queue_mut(&queue_name) {
                        let _ = queue.put(message.mqmd, message.data);
                    }
                }
            }
        }

        self.uow.active = false;
        Ok(())
    }

    /// Whether a unit of work is currently active.
    pub fn is_active(&self) -> bool {
        self.uow.active
    }

    /// Number of pending operations.
    pub fn pending_count(&self) -> usize {
        self.uow.operations.len()
    }
}

// ---------------------------------------------------------------------------
//  Message Properties API (MQCRTMH / MQDLTMH / MQSETMP / MQINQMP / MQDLTMP)
// ---------------------------------------------------------------------------

/// Manager for message handles and their properties.
#[derive(Debug, Default)]
pub struct MessageHandleManager {
    handles: std::collections::HashMap<u64, crate::structures::MqMessageHandle>,
    next_handle: u64,
}

impl MessageHandleManager {
    pub fn new() -> Self {
        Self {
            handles: std::collections::HashMap::new(),
            next_handle: 1,
        }
    }

    /// MQCRTMH — create a message handle.
    pub fn create_handle(&mut self) -> u64 {
        let id = self.next_handle;
        self.next_handle += 1;
        self.handles.insert(
            id,
            crate::structures::MqMessageHandle {
                handle: id,
                properties: std::collections::HashMap::new(),
            },
        );
        id
    }

    /// MQDLTMH — delete a message handle.
    pub fn delete_handle(&mut self, handle: u64) -> Result<(), MqError> {
        self.handles
            .remove(&handle)
            .map(|_| ())
            .ok_or(MqError::InvalidHandle)
    }

    /// MQSETMP — set a property on a message handle.
    pub fn set_property(
        &mut self,
        handle: u64,
        name: &str,
        value: crate::structures::MqPropertyValue,
    ) -> Result<(), MqError> {
        let mh = self
            .handles
            .get_mut(&handle)
            .ok_or(MqError::InvalidHandle)?;
        mh.properties.insert(name.to_string(), value);
        Ok(())
    }

    /// MQINQMP — inquire about a property on a message handle.
    pub fn inquire_property(
        &self,
        handle: u64,
        name: &str,
    ) -> Result<&crate::structures::MqPropertyValue, MqError> {
        let mh = self.handles.get(&handle).ok_or(MqError::InvalidHandle)?;
        mh.properties
            .get(name)
            .ok_or_else(|| MqError::Other(format!("Property not found: {name}")))
    }

    /// MQDLTMP — delete a property from a message handle.
    pub fn delete_property(&mut self, handle: u64, name: &str) -> Result<(), MqError> {
        let mh = self
            .handles
            .get_mut(&handle)
            .ok_or(MqError::InvalidHandle)?;
        mh.properties
            .remove(name)
            .map(|_| ())
            .ok_or_else(|| MqError::Other(format!("Property not found: {name}")))
    }

    /// List all property names on a handle.
    pub fn list_properties(&self, handle: u64) -> Result<Vec<String>, MqError> {
        let mh = self.handles.get(&handle).ok_or(MqError::InvalidHandle)?;
        Ok(mh.properties.keys().cloned().collect())
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn setup() -> (QueueManager, Connection) {
        let mut qm = QueueManager::new("TESTQM");
        qm.define_queue("TEST.Q", crate::core::QueueType::Local).unwrap();
        let conn = Connection::connect(&mut qm).unwrap();
        (qm, conn)
    }

    #[test]
    fn test_connect_disconnect() {
        let (mut qm, mut conn) = setup();
        assert!(conn.connected);
        conn.disconnect().unwrap();
        assert!(!conn.connected);
        // Double disconnect should fail.
        assert!(conn.disconnect().is_err());
        // Reconnect.
        let conn2 = Connection::connect(&mut qm).unwrap();
        assert!(conn2.connected);
    }

    #[test]
    fn test_open_close() {
        let (qm, mut conn) = setup();
        let mut od = Mqod {
            object_name: "TEST.Q".to_string(),
            ..Default::default()
        };
        let options = OpenOptions {
            output: true,
            ..Default::default()
        };
        let handle = conn.open(&qm, &mut od, options).unwrap();
        assert_eq!(conn.open_count(), 1);
        assert_eq!(od.resolved_q_name, "TEST.Q");

        conn.close(&handle).unwrap();
        assert_eq!(conn.open_count(), 0);
    }

    #[test]
    fn test_put_get() {
        let (mut qm, mut conn) = setup();

        // Open for output.
        let mut od = Mqod {
            object_name: "TEST.Q".to_string(),
            ..Default::default()
        };
        let out_handle = conn
            .open(&qm, &mut od, OpenOptions { output: true, ..Default::default() })
            .unwrap();

        // Put a message.
        let mut md = Mqmd::default();
        let mut pmo = MqPmo::default();
        conn.put(&mut qm, &out_handle, &mut md, &mut pmo, b"Hello MQ")
            .unwrap();

        conn.close(&out_handle).unwrap();

        // Open for input.
        let mut od2 = Mqod {
            object_name: "TEST.Q".to_string(),
            ..Default::default()
        };
        let in_handle = conn
            .open(&qm, &mut od2, OpenOptions { input_shared: true, ..Default::default() })
            .unwrap();

        // Get the message.
        let mut md2 = Mqmd::default();
        let gmo = Mqgmo::default();
        let mut buffer = Vec::new();
        let len = conn
            .get(&mut qm, &in_handle, &mut md2, &gmo, &mut buffer)
            .unwrap();

        assert_eq!(len, 8);
        assert_eq!(&buffer, b"Hello MQ");

        conn.close(&in_handle).unwrap();
    }

    #[test]
    fn test_put1() {
        let (mut qm, mut conn) = setup();
        let mut od = Mqod {
            object_name: "TEST.Q".to_string(),
            ..Default::default()
        };
        let mut md = Mqmd::default();
        let mut pmo = MqPmo::default();
        conn.put1(&mut qm, &mut od, &mut md, &mut pmo, b"One-shot")
            .unwrap();

        let queue = qm.get_queue("TEST.Q").unwrap();
        assert_eq!(queue.depth(), 1);
    }

    #[test]
    fn test_browse() {
        let (mut qm, mut conn) = setup();

        // Put messages.
        let mut od = Mqod {
            object_name: "TEST.Q".to_string(),
            ..Default::default()
        };
        let out_handle = conn
            .open(&qm, &mut od, OpenOptions { output: true, ..Default::default() })
            .unwrap();
        let mut md = Mqmd::default();
        let mut pmo = MqPmo::default();
        conn.put(&mut qm, &out_handle, &mut md, &mut pmo, b"msg1").unwrap();
        conn.put(&mut qm, &out_handle, &mut md, &mut pmo, b"msg2").unwrap();
        conn.close(&out_handle).unwrap();

        // Open for browse.
        let mut od2 = Mqod {
            object_name: "TEST.Q".to_string(),
            ..Default::default()
        };
        let brw_handle = conn
            .open(&qm, &mut od2, OpenOptions { browse: true, ..Default::default() })
            .unwrap();

        let mut md2 = Mqmd::default();
        let gmo = Mqgmo { browse: true, browse_cursor: 0, ..Default::default() };
        let mut buffer = Vec::new();
        conn.get(&mut qm, &brw_handle, &mut md2, &gmo, &mut buffer).unwrap();
        assert_eq!(&buffer, b"msg1");

        // Queue still has 2 messages.
        let queue = qm.get_queue("TEST.Q").unwrap();
        assert_eq!(queue.depth(), 2);
    }

    #[test]
    fn test_not_open_for_output() {
        let (mut qm, mut conn) = setup();
        let mut od = Mqod {
            object_name: "TEST.Q".to_string(),
            ..Default::default()
        };
        let handle = conn
            .open(&qm, &mut od, OpenOptions { input_shared: true, ..Default::default() })
            .unwrap();

        let mut md = Mqmd::default();
        let mut pmo = MqPmo::default();
        assert!(conn.put(&mut qm, &handle, &mut md, &mut pmo, b"fail").is_err());
    }

    #[test]
    fn test_mqi_handle_value() {
        let handle = MqiHandle { handle: 42 };
        assert_eq!(handle.value(), 42);
    }

    #[test]
    fn test_inquire() {
        let (qm, mut conn) = setup();
        let mut od = Mqod {
            object_name: "TEST.Q".to_string(),
            ..Default::default()
        };
        let handle = conn
            .open(&qm, &mut od, OpenOptions { inquire: true, ..Default::default() })
            .unwrap();

        let attrs = conn.inquire(&qm, &handle).unwrap();
        assert_eq!(attrs.queue_name, "TEST.Q");
        assert_eq!(attrs.current_depth, 0);
        assert_eq!(attrs.max_depth, 5000);
    }

    #[test]
    fn test_set() {
        let (mut qm, mut conn) = setup();
        let mut od = Mqod {
            object_name: "TEST.Q".to_string(),
            ..Default::default()
        };
        let handle = conn
            .open(&qm, &mut od, OpenOptions { set: true, ..Default::default() })
            .unwrap();

        let attrs = SetQueueAttributes {
            put_inhibited: Some(true),
            ..Default::default()
        };
        conn.set(&mut qm, &handle, &attrs).unwrap();

        let queue = qm.get_queue("TEST.Q").unwrap();
        assert!(queue.put_inhibited);
    }

    #[test]
    fn test_transaction_begin_commit() {
        let mut tc = TransactionCoordinator::new();
        assert!(!tc.is_active());

        tc.begin().unwrap();
        assert!(tc.is_active());

        tc.record_put("Q1", Mqmd::default(), b"data".to_vec());
        assert_eq!(tc.pending_count(), 1);

        tc.commit().unwrap();
        assert!(!tc.is_active());
        assert_eq!(tc.pending_count(), 0);
    }

    #[test]
    fn test_transaction_backout() {
        let mut qm = QueueManager::new("TESTQM");
        qm.define_queue("Q1", crate::core::QueueType::Local).unwrap();

        // Put a message on the queue.
        let queue = qm.get_queue_mut("Q1").unwrap();
        queue.put(Mqmd::default(), b"msg".to_vec()).unwrap();
        assert_eq!(queue.depth(), 1);

        // Begin UOW and record a get.
        let mut tc = TransactionCoordinator::new();
        tc.begin().unwrap();

        let msg = qm.get_queue_mut("Q1").unwrap().get().unwrap();
        tc.record_get("Q1", msg);

        // Backout should restore the message.
        tc.backout(&mut qm).unwrap();
        assert_eq!(qm.get_queue("Q1").unwrap().depth(), 1);
    }

    #[test]
    fn test_double_begin_fails() {
        let mut tc = TransactionCoordinator::new();
        tc.begin().unwrap();
        assert!(tc.begin().is_err());
    }

    #[test]
    fn test_commit_no_uow_fails() {
        let mut tc = TransactionCoordinator::new();
        assert!(tc.commit().is_err());
    }

    #[test]
    fn test_message_handle_manager() {
        use crate::structures::MqPropertyValue;

        let mut mgr = MessageHandleManager::new();

        // MQCRTMH
        let h = mgr.create_handle();
        assert!(h > 0);

        // MQSETMP
        mgr.set_property(h, "Color", MqPropertyValue::String("Blue".to_string()))
            .unwrap();
        mgr.set_property(h, "Weight", MqPropertyValue::Int32(42))
            .unwrap();

        // MQINQMP
        assert_eq!(
            mgr.inquire_property(h, "Color").unwrap(),
            &MqPropertyValue::String("Blue".to_string())
        );

        // List properties
        let props = mgr.list_properties(h).unwrap();
        assert_eq!(props.len(), 2);

        // MQDLTMP
        mgr.delete_property(h, "Color").unwrap();
        assert!(mgr.inquire_property(h, "Color").is_err());

        // MQDLTMH
        mgr.delete_handle(h).unwrap();
        assert!(mgr.inquire_property(h, "Weight").is_err());
    }
}
