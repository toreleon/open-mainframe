//! Timer services — TIME macro for z/OS time formats.

pub mod time;

pub use time::{MvsTime, time_now};
