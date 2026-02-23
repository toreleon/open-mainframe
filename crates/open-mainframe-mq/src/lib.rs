//! IBM MQ — Message Queuing.
//!
//! This crate provides:
//!
//! - **Queue Manager** — local queue manager with queue lifecycle management
//! - **MQI Operations** — MQCONN, MQDISC, MQOPEN, MQCLOSE, MQPUT, MQGET, MQPUT1,
//!   MQINQ, MQSET, MQCMIT, MQBACK, MQBEGIN
//! - **Data Structures** — MQMD, MQOD, MQGMO, MQPMO, MQDLH, MQTM, MQXQH, MQRFH2
//! - **Message Properties** — MQCRTMH, MQDLTMH, MQSETMP, MQINQMP, MQDLTMP
//! - **MQSC Command Engine** — DEFINE/ALTER/DELETE/DISPLAY/CLEAR queue commands
//! - **Publish/Subscribe** — topic tree, subscriptions, retained publications, selection strings
//! - **Channels** — SDR/RCVR/SVR/RQSTR/SVRCONN/CLNTCONN/CLUSSDR/CLUSRCVR/AMQP,
//!   CHLAUTH records, SSL/TLS configuration
//! - **Triggering** — FIRST/EVERY/DEPTH trigger types, process definitions, trigger monitor
//! - **Dead-Letter Handling** — MQDLH header, DLQ handler rules

pub mod channels;
pub mod core;
pub mod mqi;
pub mod mqsc;
pub mod pubsub;
pub mod structures;
pub mod triggering;

pub use self::core::{MqError, Queue, QueueManager, QueueType};
pub use channels::{
    ChannelDefinition, ChannelManager, ChannelStatus, ChannelType, ChlauthRecord,
    ChlauthRuleType, Compression, TransportType,
};
pub use mqi::{
    Connection, MessageHandleManager, MqiHandle, QueueAttributes, SetQueueAttributes,
    TransactionCoordinator,
};
pub use mqsc::{MqscCommand, MqscEngine, MqscResult};
pub use pubsub::{
    PropValue, PubOptions, PubSubEngine, Publication, SubDurability, SubManaged, SubOptions,
    SubResult, Subscription,
};
pub use structures::{
    Mqdlh, Mqgmo, Mqmd, MqMessageHandle, Mqod, MqPersistence, MqPmo, MqPriority,
    MqPropertyValue, MqReportOptions, Mqrfh2, Mqtm, Mqxqh, TriggerType,
};
pub use triggering::{
    DlqAction, DlqHandler, DlqRule, ProcessDefinition, TriggerConfig, TriggerMonitor,
};
