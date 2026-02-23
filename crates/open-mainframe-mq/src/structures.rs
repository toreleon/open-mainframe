//! MQ Data Structures — MQMD, MQOD, MQGMO, MQPMO.
//!
//! These are the core MQ data structures used for message operations.

// ---------------------------------------------------------------------------
//  MQMD — Message Descriptor
// ---------------------------------------------------------------------------

/// Message persistence.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum MqPersistence {
    /// Message is not persistent (lost on queue manager restart).
    NotPersistent,
    /// Message is persistent (survives queue manager restart).
    Persistent,
    /// Use the queue's default persistence.
    AsQueueDef,
}

impl Default for MqPersistence {
    fn default() -> Self {
        Self::AsQueueDef
    }
}

/// Message priority.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum MqPriority {
    /// Use queue default priority.
    AsQueueDef,
    /// Explicit priority (0-9, higher = more important).
    Priority(u8),
}

impl Default for MqPriority {
    fn default() -> Self {
        Self::AsQueueDef
    }
}

/// Message type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum MqMsgType {
    /// Normal datagram message.
    Datagram,
    /// Request message (expects a reply).
    Request,
    /// Reply message.
    Reply,
    /// Report message.
    Report,
}

impl Default for MqMsgType {
    fn default() -> Self {
        Self::Datagram
    }
}

/// MQMD — Message Descriptor.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Mqmd {
    /// Structure version.
    pub version: u32,
    /// Message type.
    pub msg_type: MqMsgType,
    /// Message persistence.
    pub persistence: MqPersistence,
    /// Message priority.
    pub priority: MqPriority,
    /// Expiry (tenths of a second, -1 = unlimited).
    pub expiry: i32,
    /// Feedback code (for report messages).
    pub feedback: u32,
    /// Message ID (24 bytes).
    pub msg_id: [u8; 24],
    /// Correlation ID (24 bytes).
    pub correl_id: [u8; 24],
    /// Reply-to queue name.
    pub reply_to_queue: String,
    /// Reply-to queue manager.
    pub reply_to_qmgr: String,
    /// Format name (e.g., "MQSTR", "MQHRF2").
    pub format: String,
    /// Character set ID (CCSID).
    pub ccsid: u32,
    /// Encoding (numeric encoding of data).
    pub encoding: u32,
    /// Put date (YYYYMMDD).
    pub put_date: String,
    /// Put time (HHMMSSTH).
    pub put_time: String,
    /// Application identity data.
    pub appl_identity: String,
    /// Application origin data.
    pub appl_origin: String,
    /// Put application name.
    pub put_appl_name: String,
    /// Put application type.
    pub put_appl_type: u32,
    /// Group ID (24 bytes).
    pub group_id: [u8; 24],
    /// Sequence number in group.
    pub msg_seq_number: u32,
    /// Offset in group.
    pub offset: u32,
    /// Message flags.
    pub msg_flags: u32,
    /// Original length (-1 if not truncated).
    pub original_length: i32,
}

impl Default for Mqmd {
    fn default() -> Self {
        Self {
            version: 2,
            msg_type: MqMsgType::Datagram,
            persistence: MqPersistence::AsQueueDef,
            priority: MqPriority::AsQueueDef,
            expiry: -1,
            feedback: 0,
            msg_id: [0u8; 24],
            correl_id: [0u8; 24],
            reply_to_queue: String::new(),
            reply_to_qmgr: String::new(),
            format: "MQSTR".to_string(),
            ccsid: 819,
            encoding: 546,
            put_date: String::new(),
            put_time: String::new(),
            appl_identity: String::new(),
            appl_origin: String::new(),
            put_appl_name: String::new(),
            put_appl_type: 0,
            group_id: [0u8; 24],
            msg_seq_number: 1,
            offset: 0,
            msg_flags: 0,
            original_length: -1,
        }
    }
}

// ---------------------------------------------------------------------------
//  MQOD — Object Descriptor
// ---------------------------------------------------------------------------

/// MQOD — Object Descriptor (identifies a queue for open).
#[derive(Debug, Clone, Default)]
pub struct Mqod {
    /// Object name (queue name).
    pub object_name: String,
    /// Queue manager name (blank = local).
    pub object_qmgr_name: String,
    /// Dynamic queue name template (for model queues).
    pub dynamic_q_name: String,
    /// Alternate user ID.
    pub alternate_user_id: String,
    /// Resolved queue name (filled by MQOPEN).
    pub resolved_q_name: String,
    /// Resolved queue manager name.
    pub resolved_qmgr_name: String,
}

// ---------------------------------------------------------------------------
//  MQGMO — Get Message Options
// ---------------------------------------------------------------------------

/// MQGMO — Get Message Options.
#[derive(Debug, Clone)]
pub struct Mqgmo {
    /// Wait interval in milliseconds (-1 = unlimited, 0 = no wait).
    pub wait_interval: i32,
    /// Match on message ID.
    pub match_msg_id: bool,
    /// Match on correlation ID.
    pub match_correl_id: bool,
    /// Browse (non-destructive read).
    pub browse: bool,
    /// Browse cursor position.
    pub browse_cursor: usize,
    /// Accept truncated messages.
    pub accept_truncated: bool,
    /// Convert message data.
    pub convert: bool,
    /// Syncpoint control (message under syncpoint).
    pub syncpoint: bool,
    /// No syncpoint (immediate delivery to application).
    pub no_syncpoint: bool,
}

impl Default for Mqgmo {
    fn default() -> Self {
        Self {
            wait_interval: 0,
            match_msg_id: false,
            match_correl_id: false,
            browse: false,
            browse_cursor: 0,
            accept_truncated: false,
            convert: false,
            syncpoint: false,
            no_syncpoint: true,
        }
    }
}

// ---------------------------------------------------------------------------
//  MQPMO — Put Message Options
// ---------------------------------------------------------------------------

/// MQPMO — Put Message Options.
#[derive(Debug, Clone)]
pub struct MqPmo {
    /// Generate a new message ID.
    pub new_msg_id: bool,
    /// Generate a new correlation ID.
    pub new_correl_id: bool,
    /// Syncpoint control.
    pub syncpoint: bool,
    /// No syncpoint.
    pub no_syncpoint: bool,
    /// Resolved queue name (filled by MQPUT).
    pub resolved_q_name: String,
    /// Resolved queue manager name.
    pub resolved_qmgr_name: String,
}

impl Default for MqPmo {
    fn default() -> Self {
        Self {
            new_msg_id: true,
            new_correl_id: false,
            syncpoint: false,
            no_syncpoint: true,
            resolved_q_name: String::new(),
            resolved_qmgr_name: String::new(),
        }
    }
}

// ---------------------------------------------------------------------------
//  Report options (MQRO_*)
// ---------------------------------------------------------------------------

/// Report options for MQMD.Report field.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, serde::Serialize, serde::Deserialize)]
pub struct MqReportOptions {
    /// Confirm on arrival (COA).
    pub coa: bool,
    /// Confirm on delivery (COD).
    pub cod: bool,
    /// Exception report.
    pub exception: bool,
    /// Expiry report.
    pub expiry: bool,
}

// ---------------------------------------------------------------------------
//  MQPMO context options
// ---------------------------------------------------------------------------

/// Context options for MQPMO.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, serde::Serialize, serde::Deserialize)]
pub enum MqPmoContext {
    /// Default context (queue manager sets identity context).
    #[default]
    Default,
    /// Set identity context fields.
    SetIdentity,
    /// Set all context fields.
    SetAll,
    /// Pass identity context from source message.
    PassIdentity,
    /// Pass all context from source message.
    PassAll,
}

// ---------------------------------------------------------------------------
//  MQDLH — Dead-Letter Header
// ---------------------------------------------------------------------------

/// MQDLH — Dead-Letter Header prepended to undeliverable messages.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Mqdlh {
    /// Reason message was put on DLQ.
    pub reason: u32,
    /// Original destination queue name.
    pub dest_q_name: String,
    /// Original destination queue manager name.
    pub dest_qmgr_name: String,
    /// Format of the original message data.
    pub format: String,
    /// CCSID of original data.
    pub ccsid: u32,
    /// Encoding of original data.
    pub encoding: u32,
    /// Put application name.
    pub put_appl_name: String,
    /// Put application type.
    pub put_appl_type: u32,
    /// Date message was put on DLQ (YYYYMMDD).
    pub put_date: String,
    /// Time message was put on DLQ (HHMMSSTH).
    pub put_time: String,
}

impl Default for Mqdlh {
    fn default() -> Self {
        Self {
            reason: 0,
            dest_q_name: String::new(),
            dest_qmgr_name: String::new(),
            format: String::new(),
            ccsid: 819,
            encoding: 546,
            put_appl_name: String::new(),
            put_appl_type: 0,
            put_date: String::new(),
            put_time: String::new(),
        }
    }
}

// ---------------------------------------------------------------------------
//  MQTM — Trigger Message
// ---------------------------------------------------------------------------

/// Trigger types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum TriggerType {
    /// No triggering.
    None,
    /// Trigger on first message arriving on empty queue.
    First,
    /// Trigger on every message arrival.
    Every,
    /// Trigger when queue depth reaches threshold.
    Depth,
}

impl Default for TriggerType {
    fn default() -> Self {
        Self::None
    }
}

/// MQTM — Trigger Message (sent to initiation queue when trigger fires).
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct Mqtm {
    /// Queue name that triggered.
    pub queue_name: String,
    /// Process name.
    pub process_name: String,
    /// Trigger data.
    pub trigger_data: String,
    /// Application type.
    pub appl_type: u32,
    /// Application ID (program name).
    pub appl_id: String,
    /// Environment data.
    pub env_data: String,
    /// User data.
    pub user_data: String,
}

// ---------------------------------------------------------------------------
//  MQXQH — Transmission Queue Header
// ---------------------------------------------------------------------------

/// MQXQH — header prepended to messages on transmission queues.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct Mqxqh {
    /// Remote queue name.
    pub remote_q_name: String,
    /// Remote queue manager name.
    pub remote_qmgr_name: String,
    /// Embedded message descriptor.
    pub msg_desc: Mqmd,
}

// ---------------------------------------------------------------------------
//  MQRFH2 — Rules and Formatting Header v2
// ---------------------------------------------------------------------------

/// MQRFH2 — used for JMS headers, message properties, etc.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct Mqrfh2 {
    /// Format of data following this header.
    pub format: String,
    /// CCSID of the name/value data.
    pub name_value_ccsid: u32,
    /// Name/value folders (XML-like).
    pub folders: Vec<Mqrfh2Folder>,
}

/// An RFH2 folder (XML-like named property group).
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct Mqrfh2Folder {
    /// Folder name (e.g., "jms", "usr", "mcd").
    pub name: String,
    /// Properties within this folder.
    pub properties: std::collections::HashMap<String, String>,
}

// ---------------------------------------------------------------------------
//  Message Handle & Properties (MQ v7+ API)
// ---------------------------------------------------------------------------

/// A message handle for managing message properties.
#[derive(Debug, Clone)]
pub struct MqMessageHandle {
    /// Handle identifier.
    pub handle: u64,
    /// Properties stored on this handle.
    pub properties: std::collections::HashMap<String, MqPropertyValue>,
}

/// A message property value.
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum MqPropertyValue {
    /// Boolean property.
    Bool(bool),
    /// 8-bit integer.
    Int8(i8),
    /// 16-bit integer.
    Int16(i16),
    /// 32-bit integer.
    Int32(i32),
    /// 64-bit integer.
    Int64(i64),
    /// 32-bit float.
    Float32(f32),
    /// 64-bit float.
    Float64(f64),
    /// String property.
    String(String),
    /// Byte array property.
    ByteString(Vec<u8>),
    /// Null (property exists but has no value).
    Null,
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mqmd_default() {
        let md = Mqmd::default();
        assert_eq!(md.version, 2);
        assert_eq!(md.msg_type, MqMsgType::Datagram);
        assert_eq!(md.format, "MQSTR");
        assert_eq!(md.ccsid, 819);
        assert_eq!(md.expiry, -1);
    }

    #[test]
    fn test_mqod_default() {
        let od = Mqod::default();
        assert!(od.object_name.is_empty());
    }

    #[test]
    fn test_mqgmo_default() {
        let gmo = Mqgmo::default();
        assert_eq!(gmo.wait_interval, 0);
        assert!(!gmo.browse);
        assert!(gmo.no_syncpoint);
    }

    #[test]
    fn test_mqpmo_default() {
        let pmo = MqPmo::default();
        assert!(pmo.new_msg_id);
        assert!(pmo.no_syncpoint);
    }

    #[test]
    fn test_persistence_default() {
        assert_eq!(MqPersistence::default(), MqPersistence::AsQueueDef);
    }

    #[test]
    fn test_priority_default() {
        assert_eq!(MqPriority::default(), MqPriority::AsQueueDef);
    }

    #[test]
    fn test_msg_type_default() {
        assert_eq!(MqMsgType::default(), MqMsgType::Datagram);
    }

    #[test]
    fn test_mqdlh_default() {
        let dlh = Mqdlh::default();
        assert_eq!(dlh.reason, 0);
        assert!(dlh.dest_q_name.is_empty());
        assert_eq!(dlh.ccsid, 819);
    }

    #[test]
    fn test_mqtm_default() {
        let tm = Mqtm::default();
        assert!(tm.queue_name.is_empty());
        assert!(tm.process_name.is_empty());
    }

    #[test]
    fn test_trigger_type_default() {
        assert_eq!(TriggerType::default(), TriggerType::None);
    }

    #[test]
    fn test_mqxqh_default() {
        let xqh = Mqxqh::default();
        assert!(xqh.remote_q_name.is_empty());
        assert!(xqh.remote_qmgr_name.is_empty());
    }

    #[test]
    fn test_mqrfh2_default() {
        let rfh2 = Mqrfh2::default();
        assert!(rfh2.folders.is_empty());
    }

    #[test]
    fn test_message_handle_properties() {
        let mut handle = MqMessageHandle {
            handle: 1,
            properties: std::collections::HashMap::new(),
        };
        handle.properties.insert("Color".to_string(), MqPropertyValue::String("Red".to_string()));
        handle.properties.insert("Weight".to_string(), MqPropertyValue::Int32(150));
        assert_eq!(handle.properties.len(), 2);
        assert_eq!(
            handle.properties.get("Color"),
            Some(&MqPropertyValue::String("Red".to_string()))
        );
    }

    #[test]
    fn test_report_options_default() {
        let ro = MqReportOptions::default();
        assert!(!ro.coa);
        assert!(!ro.cod);
        assert!(!ro.exception);
        assert!(!ro.expiry);
    }

    #[test]
    fn test_pmo_context_default() {
        assert_eq!(MqPmoContext::default(), MqPmoContext::Default);
    }
}
