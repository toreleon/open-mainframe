//! Runtime library for zOS-clone COBOL execution.
//!
//! This crate provides the runtime library that compiled COBOL programs
//! link against. It includes:
//!
//! - Console I/O (DISPLAY, ACCEPT)
//! - Decimal arithmetic with IBM-compatible precision
//! - String manipulation (STRING, UNSTRING, INSPECT)
//! - Value representation for COBOL data types
//!
//! # Example
//!
//! ```
//! use zos_runtime::{display_to_writer, CobolValue};
//!
//! let mut output = Vec::new();
//! let values = vec![CobolValue::alphanumeric("HELLO WORLD")];
//! display_to_writer(&values, &mut output, false).unwrap();
//! assert_eq!(String::from_utf8(output).unwrap(), "HELLO WORLD\n");
//! ```

pub mod decimal;
pub mod error;
pub mod interpreter;
pub mod io;
pub mod string;
pub mod value;

pub use decimal::{add, add_to, compute, divide, multiply, power, subtract_from};
pub use decimal::{ArithmeticResult, RoundingMode};
pub use error::RuntimeError;
pub use io::{accept, accept_from_reader, display, display_to_writer};
pub use io::{AcceptSource, DisplayOptions, DisplayTarget};
pub use string::{
    inspect_converting, inspect_replacing, inspect_tallying, string_concat, unstring,
};
pub use string::{
    InspectMode, ReplacingClause, StringDelimiter, StringSource, TallyingClause, UnstringDelimiter,
    UnstringFieldResult, UnstringTarget,
};
pub use value::{CobolValue, NumericValue};
