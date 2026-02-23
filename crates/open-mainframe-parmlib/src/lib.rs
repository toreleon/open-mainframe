//! # PARMLIB — System Initialization Configuration
//!
//! z/OS PARMLIB system initialization, symbol substitution, subsystem
//! configuration, and operator command processing.
//!
//! ## Modules
//!
//! - **members** — PARMLIB framework: concatenation, parser registry, and core
//!   member parsers (IEASYSxx, LNKLSTxx, PROGxx, CONSOLxx, COMMNDxx)
//! - **symbols** — System symbol definition (IEASYMxx) and `&symbol.`
//!   substitution engine with static system symbols
//! - **subsystem** — Subsystem configuration members (IKJTSOxx, ALLOCxx) and
//!   delegation interface for SMF, USS, JES2, RACF parsers
//! - **operator** — Initialization sequence orchestration and operator commands
//!   (SET, SETPROG, DISPLAY PARMLIB)

#![forbid(unsafe_code)]

pub mod members;
pub mod operator;
pub mod subsystem;
pub mod symbols;

pub use members::{
    CommndConfig, ConsolConfig, IeaSysConfig, LnkLstConfig, ParmlibConcat, ParserRegistry,
    ProgConfig,
};
pub use operator::{DisplayParmlibCommand, InitPhase, InitSequence, SetCommand, SetProgCommand};
pub use subsystem::{AllocConfig, IkjTsoConfig, SubsystemDelegate};
pub use symbols::{IeaSymConfig, StaticSymbols, SymbolEngine};
