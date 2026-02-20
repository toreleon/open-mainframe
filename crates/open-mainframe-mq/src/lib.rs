//! IBM MQ — Message Queuing.
//!
//! This crate provides:
//!
//! - **Queue Manager** — local queue manager with queue lifecycle management
//! - **MQI Operations** — MQPUT, MQGET, MQOPEN, MQCLOSE, MQCONN, MQDISC
//! - **Data Structures** — MQMD, MQOD, MQGMO, MQPMO message descriptors
//! - **MQSC Command Engine** — DEFINE/ALTER/DELETE/DISPLAY queue commands

pub mod core;
pub mod mqi;
pub mod structures;
pub mod mqsc;
pub mod pubsub;

pub use self::core::{MqError, Queue, QueueManager, QueueType};
pub use mqi::{Connection, MqiHandle};
pub use structures::{Mqgmo, Mqmd, Mqod, MqPersistence, MqPmo, MqPriority};
pub use mqsc::{MqscCommand, MqscEngine, MqscResult};
pub use pubsub::{
    PropValue, PubOptions, PubSubEngine, Publication, SubDurability, SubManaged, SubOptions,
    SubResult, Subscription,
};
