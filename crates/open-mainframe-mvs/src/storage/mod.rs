//! Storage management — GETMAIN/FREEMAIN subpool allocation.

pub mod getmain;
pub mod subpool;

pub use getmain::{freemain, getmain};
pub use subpool::SubpoolManager;
