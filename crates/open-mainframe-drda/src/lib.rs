//! DRDA (Distributed Relational Database Architecture) wire protocol server.
//!
//! Implements the DRDA protocol used by DB2 clients (including `ibm_db` ODBC
//! driver used by Zowe CLI) to execute SQL over TCP. This enables
//! `zowe db2 execute sql` to connect to the OpenMainframe emulation.
//!
//! # Architecture
//!
//! ```text
//! ibm_db (ODBC) ──TCP:50000──> DrdaServer
//!                                 ├─ DSS frame parsing (dss.rs)
//!                                 ├─ DDM object handling (ddm.rs)
//!                                 ├─ Connection handshake (connection.rs)
//!                                 ├─ SQL routing (sql_handler.rs)
//!                                 └─ Response building (response.rs)
//! ```

pub mod code_points;
pub mod connection;
pub mod ddm;
pub mod dss;
pub mod error;
pub mod handler;
pub mod response;
pub mod secmec9;
pub mod server;
pub mod sql_handler;

pub use error::{DrdaError, DrdaResult};
pub use server::{start_server, AuthFn, DrdaServerConfig};
