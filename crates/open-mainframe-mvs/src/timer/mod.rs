//! Timer services — TIME macro and STIMER interval timers.

pub mod stimer;
pub mod time;

pub use stimer::{StimerHandle, StimerInterval, stimer_exit, stimer_wait, stimerm_set};
pub use time::{MvsTime, time_now};
